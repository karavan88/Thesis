#!/usr/bin/env python3
import shutil
import sys
import tempfile
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": W_NS}


def qn(tag: str) -> str:
    prefix, name = tag.split(":")
    if prefix != "w":
        raise ValueError(tag)
    return f"{{{W_NS}}}{name}"


def paragraph_text(paragraph: ET.Element) -> str:
    return "".join(node.text or "" for node in paragraph.findall(".//w:t", NS)).strip()


def is_empty_heading_one(paragraph: ET.Element) -> bool:
    if paragraph.tag != qn("w:p"):
        return False
    text = paragraph_text(paragraph)
    style = paragraph.find("./w:pPr/w:pStyle", NS)
    return text == "1." and style is not None and style.attrib.get(qn("w:val")) == "Heading1"


def patch_document_xml(xml_bytes: bytes) -> bytes:
    root = ET.fromstring(xml_bytes)
    body = root.find("w:body", NS)
    if body is None:
        return xml_bytes

    children = list(body)
    removed_bookmark_ids = set()

    for idx, child in enumerate(children):
        if is_empty_heading_one(child):
            prev_child = children[idx - 1] if idx > 0 else None
            if prev_child is not None and prev_child.tag == qn("w:bookmarkStart"):
                bookmark_id = prev_child.attrib.get(qn("w:id"))
                if bookmark_id:
                    removed_bookmark_ids.add(bookmark_id)
                body.remove(prev_child)
            body.remove(child)
            break

    if removed_bookmark_ids:
        for parent in root.iter():
            for child in list(parent):
                if child.tag in {qn("w:bookmarkStart"), qn("w:bookmarkEnd")}:
                    if child.attrib.get(qn("w:id")) in removed_bookmark_ids:
                        parent.remove(child)

    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def patch_docx(docx_path: Path) -> None:
    if not docx_path.exists():
        return

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir_path = Path(tmpdir)
        with zipfile.ZipFile(docx_path, "r") as zin:
            zin.extractall(tmpdir_path)

        document_xml_path = tmpdir_path / "word" / "document.xml"
        document_xml_path.write_bytes(patch_document_xml(document_xml_path.read_bytes()))

        rebuilt = tmpdir_path / "rebuilt.docx"
        with zipfile.ZipFile(rebuilt, "w", compression=zipfile.ZIP_DEFLATED) as zout:
            for file_path in sorted(tmpdir_path.rglob("*")):
                if file_path == rebuilt or file_path.is_dir():
                    continue
                zout.write(file_path, file_path.relative_to(tmpdir_path))

        shutil.move(rebuilt, docx_path)


def main() -> int:
    targets = [Path(arg) for arg in sys.argv[1:]] or [Path("docs/Thesis.docx")]
    for target in targets:
        patch_docx(target)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
