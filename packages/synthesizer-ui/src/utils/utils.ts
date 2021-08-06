/**
 * (browser-only) Downloads any given exported SVG to the user's computer
 * @param svg
 * @param title the filename
 */
export const DownloadSVG = (svgStr: string, title = "illustration"): void => {
  const blob = new Blob([svgStr], {
    type: "image/svg+xml;charset=utf-8",
  });
  const url = URL.createObjectURL(blob);
  const downloadLink = document.createElement("a");
  downloadLink.href = url;
  downloadLink.download = `${title}.svg`;
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
};
