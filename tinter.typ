#import "./palette.typ"

#let layer-tinter = (
	palette: palette.default-palette
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
	palette: palette.default-palette
) => {
	(nodes) => {
		let categories = ()
		// collect categories
		for (node-id, properties) in nodes {
			categories.push(properties.at("category", default: "default"))
		}
		categories = categories.dedup()
		let category-index = (:)
		for (index, category) in categories.enumerate() {
			category-index.insert(category, index)
		}
		// assign colors
		for (node-id, properties) in nodes {
			let index = category-index.at(properties.at("category", default: "default"))
			let color = palette.at(calc.rem(index, palette.len()))
			nodes.at(node-id).insert("color", color)
		}
		return nodes
	}
}

#let node-tinter = (
	palette: palette.default-palette
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
	color-map,
	override: none
) => {
	assert(type(color-map) == dictionary, message: "Expected a dictionary for color-map")
	(nodes) => {
		if (override != none) {
			nodes = override(nodes)
		}
		for (node-id, properties) in nodes {
			let color = color-map.at(node-id, default: if (override == none) { palette.default-palette.at(0) } else { nodes.at(node-id).color })
			nodes.at(node-id).insert("color", color)
		}
		return nodes
	}
}

#let default-tinter = (
	palette: palette.default-palette
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
