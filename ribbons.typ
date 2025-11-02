#import "@preview/cetz:0.4.2"
#import "./utils.typ": *

#let assign-layers = (
	nodes,
	layer-override: (:)
) => {
	let layer-dict = (:)

	let queue = ()
	let visited = (:)
	let max-layer = 0
	for (node-id, properties) in nodes {
		// BFS to assign layers for each component
		if (visited.at(node-id, default: false) == false) {
			queue.push((node-id: node-id, layer: 0))
		}
		let tmp-layer = (:)
		let min-layer = 0
		while (queue.len() > 0) {
			let (node-id, layer) = queue.remove(0)
			if (visited.at(node-id, default: false) == true) {
				continue
			}
			tmp-layer.insert(node-id, layer)
			min-layer = calc.min(min-layer, layer)
			visited.insert(node-id, true)
			for (to-node-id, size) in nodes.at(node-id).edges {
				if (visited.at(to-node-id, default: false) == false) {
					queue.push((node-id: to-node-id, layer: layer + 1))
				}
			}
			for (from-node-id, size) in nodes.at(node-id).from-edges {
				if (visited.at(from-node-id, default: false) == false) {
					queue.push((node-id: from-node-id, layer: layer - 1))
				}
			}
		}
		// Normalize layers to start from 0
		for (node-id, layer) in tmp-layer {
			let normalized-layer = layer - min-layer
			max-layer = calc.max(max-layer, normalized-layer)
			layer-dict.insert(node-id, normalized-layer)
		}
	}
	// Apply layer override
	for (node-id, layer) in layer-override {
		layer-dict.insert(node-id, layer)
	}

	// Collect nodes in layers	
	let layers = ()
	for i in range(0, max-layer + 1) {
		layers.push(())
	}
	for node-id in nodes.keys() { // keep original order
		let layer = layer-dict.at(node-id)
		layers.at(layer).push(node-id)		
	}

	return layers
}


#let preprocess-data = (
	data,
	aliases
) => {
	// Add nodes that defined implicitly by being a target of an edge
	assert(type(data) == dictionary, message: "Expected a dictionary")

	for (node-id, edges) in data {
		assert(type(edges) == dictionary, message: "Expected a dictionary of dictionaries")
		for target in edges.keys() {
			if (data.at(target, default: none) == none) {
				data.insert(target, (:))
			}
		}
	}

	// Make edges dictionaries in another dictionary for other attributes
	for (node-id, edges) in data {
		data.insert(node-id, (
			edges: edges,
			from-edges: (:)
		))
	}
	// Add from-edges
	for (node-id, properties) in data {
		for (to, size) in properties.edges {
			data.at(to).from-edges.insert(node-id, size)
		}
	}
	// Add id (same as key) and name (alias | id)
	for (node-id, properties) in data {
		data.at(node-id).insert("id", node-id)
		if (aliases.at(node-id, default: none) != none) {
			data.at(node-id).insert("name", aliases.at(node-id))
		} else {
			data.at(node-id).insert("name", node-id)
		}
	}
	// add #id
	let counter = 0
	for (node-id, properties) in data {
		data.at(node-id).insert("number-id", counter)
		counter += 1
	}


	// Add other necessary attributes to nodes
	// in-size and out-size: sum of incoming and outgoing edge sizes
	let in-size = (:)
	for (node-id, properties) in data {
		for (to, size) in properties.edges {
			in-size.insert(to, in-size.at(to, default: 0) + size)
		}
	}
	for (node-id, properties) in data {
		let out-size = properties.edges.values().sum(default: 0)
		data.at(node-id).insert("in-size", in-size.at(node-id, default: 0))
		data.at(node-id).insert("out-size", out-size)
	}

	// TODO: Add more attributes
	data
}


/*
A Palette is an array of colours
A Tinter returns a function, Nodes -> Palette -> Nodes
*/
#let tint-override = (overrides) => {

}
// #let default-palette = (red, green, blue, orange, purple, gray, yellow)
#let color-brewer-palette = (rgb("#66C2A5"), rgb("#FC8D62"), rgb("#8DA0CB"), rgb("#E78AC3"), rgb("#A6D854"), rgb("#FFD92F"), rgb("#E5C494"), rgb("#B3B3B3"))
#let default-palette = color-brewer-palette

#let layer-tinter = (
	palette: default-palette
) => {
	(nodes) => {
		for (node-id, properties) in nodes {
			assert(properties.at("layer", default: none) != none, message: "Node " + node-id + " has no layer attribute. Are you using a layered layout?")
			let layer = properties.layer
			let color = palette.at(calc.rem(layer, palette.len()))
			nodes.at(node-id).insert("color", color)
		}
		nodes
	}
}

#let categorical-tinter = (
	palette: default-palette
) => {
	
}

#let node-tinter = (
	palette: default-palette
) => {
	(nodes) => {
		for (node-id, properties) in nodes {
			let number-id = properties.number-id
			let color = palette.at(calc.rem(number-id, palette.len()))
			nodes.at(node-id).insert("color", color)
		}
		return nodes
	}
}
#let dict-tinter = (
	color-map
) => {
	assert(type(color-map) == dictionary, message: "Expected a dictionary for color-map")
	(nodes) => {
		for (node-id, properties) in nodes {
			let color = color-map.at(node-id, default: default-palette.at(0))
			nodes.at(node-id).insert("color", color)
		}
		return nodes
	}
}



#let default-tinter = (
	palette: default-palette
) => {
	// choose layer tinter if layers exist, otherwise node tinter
	(nodes) => {
		if nodes.keys().len() == 0 {
			return layer-tinter(palette: palette)(nodes)
		}
		if (nodes.at(nodes.keys().first()).at("layer", default: none) != none) {
			return layer-tinter(palette: palette)(nodes)
		} else {
			return node-tinter(palette: palette)(nodes)
		}
	}
}



#let auto-linear-layout = (
	layer-gap: 1.5,
	node-gap: 0.75,
	node-width: 0.5,
	basenode-height: 3,
	layers: (:)
) => {
	let layer-override = layers
	( layouter: (nodes) => {
		// Calculate layers for each node
		let layers = assign-layers(nodes, layer-override: layer-override)

		for (layer-index, layer) in layers.enumerate() {
			for node-id in layer {
				nodes.at(node-id).insert("layer", layer-index)
			}
		}
		// node size=max(in-size, out-size)
		for (node-id, properties) in nodes {
			let node-size = calc.max(properties.in-size, properties.out-size)
			nodes.at(node-id).insert("size", node-size)
		}
		// Give widths
		for (node-id, properties) in nodes {
			nodes.at(node-id).insert("width", node-width)
		}
		// Give heights
		let max-node-size = 0
		for (node-id, properties) in nodes {
			max-node-size = calc.max(max-node-size, properties.size)
		}
		for (node-id, properties) in nodes {
			let node-size = properties.size
			let node-height = node-size / max-node-size * basenode-height
			nodes.at(node-id).insert("height", node-height)
		}

		// Give initial x positions
		for (node-id, properties) in nodes {
			let layer = properties.layer
			let x = layer * (node-width + layer-gap) + node-width / 2
			nodes.at(node-id).insert("x", x)
		}
		// Assign initial y positions based on node-gap
		let layer-assign-y-positions = (nodes, layer) => {
			let offset = 0.0
			for node-id in layer {
				let height = nodes.at(node-id).height
				nodes.at(node-id).insert("y", offset - height / 2)
				offset -= height + node-gap
			}
			nodes
		}
		for layer in layers {
			nodes = layer-assign-y-positions(nodes, layer)
		}

		/*
			A node receives forces from:
				- attraction to every connected nodes
			Nodes have rigid constraints:
				- nodes in the same layer must have at least node-gap distance
		*/
		let calculate-attraction-forces = (nodes) => {
			let forces = (:)
			for (node-id, properties) in nodes {
				forces.insert(node-id, 0.0)
			}

			for (node-id, properties) in nodes {
				let y1 = properties.y
				
				for to in properties.edges.keys() {
					let y2 = nodes.at(to).y
					let dy = y2 - y1
					let dx = layer-gap
					let dist = calc.sqrt(dx * dx + dy * dy)
					let theta = calc.atan2(dy, dx)
					let force = dist * calc.cos(theta) * 0.5
					forces.insert(node-id, forces.at(node-id) + force)
					forces.insert(to, forces.at(to) - force)
				}
			}
			
			return forces
		}
		
		let enforce-rigid-min-gap = (nodes, layers) => {
			let max-iterations = 15
			let nodes-new = nodes
			
			for iter in range(0, max-iterations) {
				let violations = false
				
				for layer in layers {
					for i in range(0, layer.len() - 1) {
						let node-id1 = layer.at(i)
						let node-id2 = layer.at(i + 1)
						let y1 = nodes-new.at(node-id1).y
						let h1 = nodes-new.at(node-id1).height
						let y2 = nodes-new.at(node-id2).y
						let h2 = nodes-new.at(node-id2).height
						
						let bottom1 = y1 - h1 / 2
						let top2 = y2 + h2 / 2
						let current-gap = bottom1 - top2
						
						if current-gap < node-gap {
							violations = true
							let violation = node-gap - current-gap
							// Push them apart equally
							nodes-new.at(node-id1).insert("y", nodes-new.at(node-id1).y + violation / 2)
							nodes-new.at(node-id2).insert("y", nodes-new.at(node-id2).y - violation / 2)
						}
					}
				}
				
				if not violations { break }
			}
			
			return nodes-new
		}

		// Resolve physical system
		let iterations = 30
		let alpha = 0.1
		
		for i in range(1, iterations) {
			let nodes-new = nodes
			
			let forces = calculate-attraction-forces(nodes)
			
			// Apply forces
			for (node-id, properties) in nodes {
				let y = properties.y
				let force = forces.at(node-id)
				y += force * alpha
				nodes-new.at(node-id).insert("y", y)
				nodes-new.at(node-id).insert("force", force)
			}
			
			nodes = enforce-rigid-min-gap(nodes-new, layers)
		}

		return nodes
	}, drawer: (nodes, ribbon-colorizer) => {
		cetz.canvas({
			import cetz.draw: *
			
			let acc-out-size = (:)
			let acc-in-size = (:)
			for (node-id, properties) in nodes {
				let (x, y, width, height) = properties
				rect((x - width/2, y - height/2), (x + width/2, y + height/2), fill: properties.color, stroke: none)
				
				content((x, y), text(properties.name, size: 8pt))

				// sort ribbons first by slope to prevent crossing
				let edges = ()
				for (to-node-id, edge-size) in properties.edges {
					edges.push((
						to-node-id: to-node-id,
						edge-size: edge-size,
						slope: calc.atan2((nodes.at(to-node-id).y - y), (nodes.at(to-node-id).x - x))
					))
				}
				edges = edges.sorted(key: it => it.slope)

				// ribbons
				for (to-node-id, edge-size) in edges {
					let to-properties = nodes.at(to-node-id)
					let top-left = (x + width / 2, y + height / 2 - acc-out-size.at(node-id, default: 0) / properties.size * height)
					let bottom-left = (top-left.at(0), top-left.at(1) - edge-size / properties.size * height)
					let top-right = (to-properties.x - to-properties.width / 2, to-properties.y + to-properties.height / 2 - acc-in-size.at(to-node-id, default: 0) / to-properties.size * to-properties.height)
					let bottom-right = (top-right.at(0), top-right.at(1) - edge-size / to-properties.size * to-properties.height)
					acc-out-size.insert(node-id, acc-out-size.at(node-id, default: 0) + edge-size)
					acc-in-size.insert(to-node-id, acc-in-size.at(to-node-id, default: 0) + edge-size)

					let ribbon-width = calc.min(top-left.at(1) - bottom-left.at(1), top-right.at(1) - bottom-right.at(1))

					let curve-factor = 0.3
					let bezier-top-control-1 = point-translate(top-left, (curve-factor * (top-right.at(0) - top-left.at(0)), 0))
					let bezier-top-control-2 = point-translate(top-right, (-curve-factor * (top-right.at(0) - top-left.at(0)), 0))
					let bezier-bottom-control-1 = point-translate(bottom-left, (curve-factor * (bottom-right.at(0) - bottom-left.at(0)), 0))
					let bezier-bottom-control-2 = point-translate(bottom-right, (-curve-factor * (bottom-right.at(0) - bottom-left.at(0)), 0))
					merge-path(
						fill: ribbon-colorizer(properties.color, to-properties.color, node-id, to-node-id),
						stroke: none,
						{
							bezier(top-left, top-right, bezier-top-control-1, bezier-top-control-2)
							line(top-right, bottom-right)
							bezier(bottom-right, bottom-left, bezier-bottom-control-2, bezier-bottom-control-1)
							line(bottom-left, top-left)
						}
					)
				}

				// forces
				let force = properties.at("force", default: 0)
				if (force != 0) {
					let sign = if (force > 0) { 1 } else { -1 }
					let len = calc.min(calc.abs(force), 1)
					// line((x, y), (x, y + len * sign), stroke: red, stroke-width: 0.05, mark: (end: ">"))
				}
			}
		})

	})
}

#let circular-layout = (
	radius: 4,
	node-width: 0.5,
	node-gap: 1deg,
	angle-offset: 0deg,
	directed: false,
) => {
	(layouter: (nodes) => {
		// node size=in-size+out-size by default
		for (node-id, properties) in nodes {
			let node-size = if (directed) { properties.in-size + properties.out-size } else { properties.out-size }
			nodes.at(node-id).insert("size", node-size)
		}
		// Place all node on a ring (only 1 layer)
		let sum = 0
		for (node-id, properties) in nodes {
			sum += properties.size
		}
		sum /= 1 - (node-gap * nodes.keys().len() / 360deg)

		let angle = angle-offset + 90deg
		for (node-id, properties) in nodes {
			let node-size = properties.size
			let node-arc = (node-size / sum) * 360deg
			nodes.at(node-id).insert("angle", angle + node-arc / 2)
			nodes.at(node-id).insert("arc", node-arc)
			angle += node-arc + node-gap
		}
		
		return nodes
	}, drawer: (nodes, ribbon-colorizer) => {
		cetz.canvas({
			import cetz.draw: *
			
			// out size acculates from 0, in size acculates from total size
			let in-acc-size = (:)
			let out-acc-size = (:) // if undirected, only use out-acc-size
			let drawn = (:) // drawn[from][to] = bool, for drawing undirected edges only once
			for (node-id, properties) in nodes {
				let angle = properties.angle
				let node-arc = properties.arc
				let width = node-width

				let inner-left = (radius * calc.cos(angle - node-arc / 2), radius * calc.sin(angle - node-arc / 2))
				let inner-center = (radius * calc.cos(angle), radius * calc.sin(angle))
				let inner-right = (radius * calc.cos(angle + node-arc / 2), radius * calc.sin(angle + node-arc / 2))
				let outer-left = ((radius + width) * calc.cos(angle - node-arc / 2), (radius + width) * calc.sin(angle - node-arc / 2))
				let outer-center = ((radius + width) * calc.cos(angle), (radius + width) * calc.sin(angle))
				let outer-right = ((radius + width) * calc.cos(angle + node-arc / 2), (radius + width) * calc.sin(angle + node-arc / 2))

				merge-path(
					fill: properties.color,
					stroke: none,
					{
						arc-through(inner-left, inner-center, inner-right)
						line(inner-right, outer-right)
						arc-through(outer-right, outer-center, outer-left)
						line(outer-left, inner-left)
					}
				)

				// content((x, y), text(properties.name, size: 8pt))

				// ribbons
				for (to-node-id, out-edge-size) in properties.edges {
					if (not directed and (
						drawn.at(node-id, default: (:)).at(to-node-id, default: false) or
						drawn.at(to-node-id, default: (:)).at(node-id, default: false)
					)) {
						continue
					}


					let to-properties = nodes.at(to-node-id)
					
					let in-edge-size = if (directed) { out-edge-size } else {
						to-properties.edges.at(node-id, default: 0)
					}

					let from-acc-size = out-acc-size.at(node-id, default: 0)
					let to-acc-size = if (directed) {in-acc-size.at(to-node-id, default: to-properties.size) }
										else { out-acc-size.at(to-node-id, default: 0) }
					let from-start-angle = angle - node-arc / 2 + (from-acc-size / properties.size * node-arc)
					let from-end-angle = angle - node-arc / 2 + ((from-acc-size + out-edge-size) / properties.size * node-arc)
					let to-start-angle = to-properties.angle - to-properties.arc / 2 + ((to-acc-size - in-edge-size) / to-properties.size * to-properties.arc)
					let to-end-angle = to-properties.angle - to-properties.arc / 2 + (to-acc-size / to-properties.size * to-properties.arc)
					if (not directed) {
						to-start-angle = to-properties.angle - to-properties.arc / 2 + (to-acc-size / to-properties.size * to-properties.arc)
						to-end-angle = to-properties.angle - to-properties.arc / 2 + ((to-acc-size + in-edge-size) / to-properties.size * to-properties.arc)
						if (drawn.at(node-id, default: none) == none) { drawn.insert(node-id, (:)) }
						drawn.at(node-id).insert(to-node-id, true)
						if (drawn.at(to-node-id, default: none) == none) { drawn.insert(to-node-id, (:)) }
						drawn.at(to-node-id).insert(node-id, true)
					}

					let from-left = (radius * calc.cos(from-start-angle), radius * calc.sin(from-start-angle))
					let from-center = (radius * calc.cos((from-start-angle + from-end-angle) / 2), radius * calc.sin((from-start-angle + from-end-angle) / 2))
					let from-right = (radius * calc.cos(from-end-angle), radius * calc.sin(from-end-angle))
					let to-left = (radius * calc.cos(to-start-angle), radius * calc.sin(to-start-angle))
					let to-center = (radius * calc.cos((to-start-angle + to-end-angle) / 2), radius * calc.sin((to-start-angle + to-end-angle) / 2))
					let to-right = (radius * calc.cos(to-end-angle), radius * calc.sin(to-end-angle))

					out-acc-size.insert(node-id, from-acc-size + out-edge-size)
					if (directed) {
						in-acc-size.insert(to-node-id, to-acc-size - in-edge-size)
					} else if (node-id != to-node-id) {
						out-acc-size.insert(to-node-id, to-acc-size + in-edge-size)
					}

					
					merge-path(
						fill: ribbon-colorizer(
							properties.color, to-properties.color, node-id, to-node-id,
							angle: -calc.atan2(to-center.at(0) - from-center.at(0), to-center.at(1) - from-center.at(1))
						),
						stroke: 0.5pt + white,
						{
							arc-through(from-left, from-center, from-right)
							bezier(from-right, to-left, (0, 0))
							arc-through(to-left, to-center, to-right)
							bezier(to-right, from-left, (0, 0))
						}
					)
					
				}

			}
		})
	})
}

/*
Ribbon colorizers
*/

#let ribbon-from-color = (
	transparency: 75%,
) => {
	(from-color, to-color, from-node, to-node, ..) => from-color.transparentize(transparency)
}
#let ribbon-to-color = (
	transparency: 75%,
) => {
	(from-color, to-color, from-node, to-node, ..) => to-color.transparentize(transparency)
}
#let ribbon-gradient-from-to = (
	transparency: 75%,
) => {
	(from-color, to-color, from-node, to-node, angle: 0deg, ..) => {
		gradient.linear(from-color.transparentize(transparency), to-color.transparentize(transparency), angle: angle)
	}
}
#let ribbon-solid-color = (
	color: black,
	transparency: 90%,
) => {
	(from-color, to-color, from-node, to-node, ..) => color.transparentize(transparency)
}


#let sankey = (
	data,
	aliases: (:),
	layout: auto-linear-layout(),
	tinter: default-tinter(),
	ribbon-color: ribbon-from-color()
) => {
	let nodes = preprocess-data(data, aliases)
	//repr(assign-layers(nodes))
	let (layouter, drawer) = layout
	nodes = layouter(nodes)
	nodes = tinter(nodes)
	// repr(nodes)
	drawer(nodes, ribbon-color)
}


#sankey(
	(
		"A": ("B": 5, "C": 3),
		"B": ("D": 2, "E": 4),
		"C": ("D": 3, "E": 4),
		"E": ("F": 2),
	)
)
#sankey(
	// (
	// 	"iPhone": (
	// 		"Products": 44582
	// 	),
	// 	"Wearables, Home, Accessories": (
	// 		"Products": 7404
	// 	),
	// 	"Mac": (
	// 		"Products": 8046
	// 	),
	// 	"iPad": (
	// 		"Products": 6581
	// 	),
	// 	"Products": (
	// 		"Apple Net Sales Quarter": 66613
	// 	),
	// 	"Services": (
	// 		"Apple Net Sales Quarter": 27423
	// 	),
	// 	"Apple Net Sales Quarter": (
	// 		"Cost of Sales": 50318,
	// 		"Gross Margin": 43718
	// 	),
	// 	"Gross Margin": (
	// 		"Research & Development": 8866,
	// 		"Selling, General, Administrative": 6650,
	// 		"Operating Income": 28202
	// 	),
	// 	"Operating Income": (
	// 		"Other Expense": 171,
	// 		"Income before Taxes": 28031
	// 	),
	// 	"Income before Taxes": (
	// 		"Taxes": 4597,
	// 		"Net Income": 23434
	// 	)
	// ),
	(
		"iPhone": (
			"Products": 44582
		),
		"Wearables, Home, Accessories": (
			"Products": 7404
		),
		"Mac": (
			"Products": 8046
		),
		"iPad": (
			"Products": 6581
		),
		"Products": (
			"Apple Net Sales Quarter": 66613
		),
		"Services": (
			"Apple Net Sales Quarter": 27423
		),
		"Apple Net Sales Quarter": (
			"Gross Margin": 43718,
			"Cost of Sales": 50318,
		),
		"Gross Margin": (
			"Operating Income": 28202,
			"Research & Development": 8866,
			"Selling, General, Administrative": 6650,
		),
		"Operating Income": (
			"Income before Taxes": 28031,
			"Other Expense": 171,
		),
		"Income before Taxes": (
			"Taxes": 4597,
			"Net Income": 23434
		)
	),
	layout: auto-linear-layout(
		layers: (
			"Services": 1,
		)
	)
)


#sankey(
	(
		"A": ("B": 4, "C": 9, "D": 4),
		"B": ("E": 2, "F": 2),
		"E": ("G": 1, "H": 1)
	),
	aliases: (
		"A": "meow"
	)
)
#sankey(
	(
		"Nuclear": (
			"Thermal generation": 839
		),
		"Agricultural 'waste'": (
			"Bio-conversion": 124
		),
		"UK land based bioenergy": (
			"Bio-conversion": 182
		),
		"Marine algae": (
			"Bio-conversion": 4
		),
		"Other waste": (
			"Bio-conversion": 77,
			"Solid": 56
		),
		"Tidal": (
			"Electricity grid": 9
		),
		"Wave": (
			"Electricity grid": 19
		),
		"Solar": (
			"Solar PV": 59,
			"Solar Thermal": 19
		),
		"Solar PV": (
			"Electricity grid": 59
		),
		"Geothermal": (
			"Electricity grid": 7
		),
		"Hydro": (
			"Electricity grid": 6
		),
		"Wind": (
			"Electricity grid": 289
		),
		"District heating": (
			"Industry": 10,
			"Heating and cooling - commercial": 22,
			"Heating and cooling - homes": 46
		),
		"Solar Thermal": (
			"Heating and cooling - homes": 19
		),
		"Pumped heat": (
			"Heating and cooling - homes": 193,
			"Heating and cooling - commercial": 70
		),
		"Bio-conversion": (
			"Losses": 26,
			"Solid": 280,
			"Gas": 81,
			"Liquid": 0
		),
		"Biomass imports": (
			"Solid": 35
		),
		"Coal imports": (
			"Coal": 11
		),
		"Coal reserves": (
			"Coal": 63
		),
		"Coal": (
			"Solid": 75
		),
		"Gas": (
			"Losses": 1,
			"Thermal generation": 151,
			"Heating and cooling - commercial": 0,
			"Industry": 48,
			"Agriculture": 2
		),
		"Gas imports": (
			"Ngas": 40
		),
		"Gas reserves": (
			"Ngas": 82
		),
		"Ngas": (
			"Gas": 122
		),
		"H2 conversion": (
			"H2": 20,
			"Losses": 6
		),
		"Solid": (
			"Agriculture": 0,
			"Thermal generation": 400,
			"Industry": 46
		),
		"Electricity grid": (
			"Losses": 56,
			"Industry": 342,
			"Over generation / exports": 104,
			"Lighting & appliances - commercial": 90,
			"Lighting & appliances - homes": 93,
			"Heating and cooling - homes": 113,
			"H2 conversion": 27,
			"Rail transport": 7,
			"Road transport": 37,
			"Agriculture": 4,
			"Heating and cooling - commercial": 40
		),
		"H2": (
			"Road transport": 20
		),
		"Liquid": (
			"Industry": 121,
			"Road transport": 135,
			"International aviation": 206,
			"International shipping": 128,
			"Agriculture": 3,
			"National navigation": 33,
			"Rail transport": 4,
			"Domestic aviation": 14
		),
		"Oil imports": (
			"Oil": 504
		),
		"Oil reserves": (
			"Oil": 107
		),
		"Oil": (
			"Liquid": 611
		),
		"Biofuel imports": (
			"Liquid": 35
		),
		"Thermal generation": (
			"Electricity grid": 525,
			"Losses": 787,
			"District heating": 79
		)
	),
	ribbon-color: ribbon-gradient-from-to()
)

#sankey(
	(
		"a": ("a": 1000, "b": 1000), 
		"b": ("a": 1000, "b": 1000), 
	),
	layout: circular-layout(),
	// tinter: node-tinter()
)

#sankey(
	(
		"a": ("a": 1000, "b": 1000), 
		"b": ("a": 500, "b": 1000), 
	),
	layout: circular-layout(directed: true),
	// tinter: node-tinter()
	ribbon-color: ribbon-gradient-from-to()
)


#sankey(
	(
		"black": ("black": 11975, "blond": 5871, "brown": 8916, "red": 2868), 
		"blond": ("black": 1951, "blond": 10048, "brown": 2060, "red": 6171), 
		"brown": ("black": 8010, "blond": 16145, "brown": 8090, "red": 8045), 
		"red": ("black": 1013, "blond": 990, "brown": 940, "red": 6907)  
	),
	layout: circular-layout(),
	ribbon-color: ribbon-gradient-from-to(transparency: 70%),
	tinter: dict-tinter((
		"black": rgb("#000000"),
		"blond": rgb("#ffdd89"),
		"brown": rgb("#957244"),
		"red": rgb("#f26223"),
	))
)

#sankey(
	(
		"A": ("B": 10),
		"B": ("C": 10),
		"C": ("D": 10, "Y": 10),
		"D": ("E": 10),
		"X": ("C": 10),
	)
)

// TODO: size overrides
