# Passthrough SVG

Penrose provides an 'escape hatch' for advanced users to specify SVG properties Penrose does not currently support.

Within any Style program, you may specify SVG properties directly as shape properties. The Style language does not allow dashes in property names. The current convention is to remove any dashes and make the next character after the dash uppercase. For example, SVG property `color-interpolation` is specified as `colorInterpolation` in Style. Likewise, SVG property `color-interpolation-filters` is specified as `colorInterpolationFilters` in Style.

You can find all SVG properties on [W3C's SVG website](https://www.w3.org/Graphics/SVG/).

## CAUTION

- Passthrough values are passed through as-is: Penrose does not optimize or consider their values. This means your passthrough property could interact unfavorably with (or violate) the constraints in your Style program. If you have problems with a rendered diagram, commenting out any SVG passthrough properties is a good troubleshooting step.
- If Penrose is already writing to an SVG property, it wins: it will override the passthrough value.
- You are responsible for ensuring the SVG property is allowed for the given shape type and that the property value is a valid SVG value.
