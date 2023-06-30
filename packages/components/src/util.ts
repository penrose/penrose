// crop the SVG if necessary, i.e. if the
// cropped view box is smaller than the current view box
export const cropSVG = (svg: string) => {
  const parser = new DOMParser();
  const serializer = new XMLSerializer();
  const svgDoc = parser.parseFromString(svg, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg")!;
  const viewBox = svgNode.getAttribute("viewBox");
  if (cropped && viewBox) {
    const viewBoxNums = svgNode.viewBox.baseVal;
    const croppedNums = cropped.split(/\s+|,/);

    const croppedWidth = parseFloat(croppedNums[3]);
    const croppedHeight = parseFloat(croppedNums[4]);
    const viewBoxWidth = viewBoxNums.width;
    const viewBoxHeight = viewBoxNums.height;

    // if area of cropped view box is leq area of current view box
    // then set the view box to the cropped view box
    if (
      Math.abs(croppedWidth * croppedHeight) <
      Math.abs(viewBoxWidth * viewBoxHeight)
    ) {
      svgNode.setAttribute("viewBox", cropped);
    }
  }
  return serializer.serializeToString(svgNode);
};
