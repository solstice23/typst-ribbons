#let match-from = (
    transparency: 75%,
    stroke-width: 0pt,
    stroke-color: auto,
) => {
    (from-color, to-color, from-node, to-node, ..) => (
        fill: from-color.transparentize(transparency),
        stroke: stroke-width + if (stroke-color == auto) {
            from-color
        } else {
            stroke-color
        }
    )
}
#let match-to = (
    transparency: 75%,
    stroke-width: 0pt,
    stroke-color: auto,
) => {
    (from-color, to-color, from-node, to-node, ..) => (
    	fill: to-color.transparentize(transparency),
        stroke: stroke-width + if (stroke-color == auto) {
        	to-color
        } else {
        	stroke-color
        }
    )
}
#let gradient-from-to = (
    transparency: 75%,
    stroke-width: 0pt,
    stroke-color: auto,
) => {
    (from-color, to-color, from-node, to-node, angle: 0deg, ..) => (
        fill: gradient.linear(from-color.transparentize(transparency), to-color.transparentize(transparency), angle: angle),
        stroke: stroke-width + if (stroke-color == auto) {
        	gradient.linear(from-color, to-color, angle: angle)
        } else {
        	stroke-color
        }
    )
}
#let solid-color = (
    color: black,
    transparency: 90%,
    stroke-width: 0pt,
    stroke-color: auto,
) => {
    (from-color, to-color, from-node, to-node, ..) => (
    	fill: color.transparentize(transparency),
        stroke: stroke-width + if (stroke-color == auto) {
        	from-color
        } else {
        	stroke-color
        }
    )
}

#let default = () => {
    (from-color, to-color, from-node, to-node, angle: none, ..args) => (
        if (angle != none) {
            // chord chart
            gradient-from-to(
                stroke-color: white,
                stroke-width: 0.5pt,
            )(
                from-color, to-color, from-node, to-node, angle: angle, ..args
            )
        } else {
            // everything else
            match-from()(
                from-color, to-color, from-node, to-node, ..args
            )
        }
    )
}