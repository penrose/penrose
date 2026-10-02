"""Import the supplied scan for the local reader, without checking it into Git.

Run with Python containing pypdf and Poppler's pdftoppm on PATH:
    python scripts/elementary-topology/import-source.py /path/to/book.pdf
The audited source checksum and page count are validated before any rendering.
"""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess

from pypdf import PdfReader


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("pdf", type=Path)
    parser.add_argument("--dpi", type=int, default=144)
    args = parser.parse_args()
    if args.dpi < 72 or args.dpi > 300:
        parser.error("--dpi must be between 72 and 300")

    root = Path(__file__).resolve().parents[2]
    audit = json.loads((root / "docs/elementary-topology/source-audit.json").read_text())
    digest = hashlib.file_digest(args.pdf.open("rb"), "sha256").hexdigest()
    if digest != audit["source"]["sha256"]:
        parser.error("The PDF does not match the audited source. Audit this edition before importing it.")
    pdf = PdfReader(args.pdf)
    if len(pdf.pages) != audit["source"]["pdfPageCount"]:
        parser.error("The PDF page count does not match the source audit")

    output = root / "packages/docs-site/public/elementary-topology/source"
    output.mkdir(parents=True, exist_ok=True)
    # pdftoppm renders mathematical text directly; OCR never substitutes for it.
    subprocess.run(
        ["pdftoppm", "-png", "-r", str(args.dpi), str(args.pdf), str(output / "page")],
        check=True,
    )
    labels = {page: label for label, page in audit["printedPageToPdfPage"].items()}
    pages = [
        {
            "pdfPage": i,
            "printedPage": labels.get(i),
            "width": float(page.mediabox.width),
            "height": float(page.mediabox.height),
            "image": f"page-{i:03}.png",
        }
        for i, page in enumerate(pdf.pages, 1)
    ]
    manifest = {
        "source": audit["source"],
        "missingPrintedPages": audit["pagination"]["missingPrintedPages"],
        "pages": pages,
    }
    (output / "book.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"Imported {len(pages)} source pages to {output}")


if __name__ == "__main__":
    main()
