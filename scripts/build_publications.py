#!/usr/bin/env python3
"""Convert the site BibTeX file into Hugo data."""

from __future__ import annotations

import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BIB_PATH = ROOT / "assets/source/lelkes_publications.bib"
OUT_PATH = ROOT / "data/publications.json"


FIELD_MAP = {
    "journaltitle": "journal",
}


def strip_latex(value: str) -> str:
    replacements = {
        r"\&": "&",
        r"\%": "%",
        r"\$": "$",
        r"\#": "#",
        r"\_": "_",
        r"``": '"',
        r"''": '"',
        "---": "—",
        "--": "–",
    }
    for old, new in replacements.items():
        value = value.replace(old, new)

    accents = {
        r"\~a": "ã",
        r"\~{a}": "ã",
        r"\~A": "Ã",
        r"\~{A}": "Ã",
        r"\"a": "ä",
        r"\"{a}": "ä",
        r"\"o": "ö",
        r"\"{o}": "ö",
        r"\"u": "ü",
        r"\"{u}": "ü",
        r"\"A": "Ä",
        r"\"{A}": "Ä",
        r"\"O": "Ö",
        r"\"{O}": "Ö",
        r"\"U": "Ü",
        r"\"{U}": "Ü",
        r"\'a": "á",
        r"\'e": "é",
        r"\'i": "í",
        r"\'o": "ó",
        r"\'u": "ú",
        r"\`a": "à",
        r"\`e": "è",
        r"\~n": "ñ",
        r"\c{c}": "ç",
    }
    for old, new in accents.items():
        value = value.replace(old, new)

    value = re.sub(r"\\href\{([^}]*)\}\{([^}]*)\}", r"\2", value)
    value = re.sub(r"\\~\{?([A-Za-z])\}?", r"~\1", value)
    value = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?", "", value)
    value = value.replace("{", "").replace("}", "")
    value = re.sub(r"\s+", " ", value)
    return value.strip()


def initials(value: str) -> str:
    parts = re.findall(r"[A-Za-zÀ-ÖØ-öø-ÿ]+", strip_latex(value))
    return " ".join(f"{part[0].upper()}." for part in parts)


def format_author_name(name: str) -> str:
    name = strip_latex(name)
    if "," in name:
        last, first = [part.strip() for part in name.split(",", 1)]
        first_initials = initials(first)
        return f"{first_initials} {last}".strip()

    parts = name.split()
    if len(parts) <= 1:
        return name
    last = parts[-1]
    first_initials = initials(" ".join(parts[:-1]))
    return f"{first_initials} {last}".strip()


def split_authors(value: str) -> list[str]:
    return [format_author_name(part) for part in re.split(r"\s+and\s+", value) if part.strip()]


def author_text(authors: list[str]) -> str:
    if not authors:
        return ""
    if len(authors) == 1:
        return authors[0]
    if len(authors) == 2:
        return f"{authors[0]} & {authors[1]}"
    return f"{', '.join(authors[:-1])}, & {authors[-1]}"


def parse_entries(text: str) -> list[dict[str, str]]:
    entries = []
    i = 0
    while True:
        at = text.find("@", i)
        if at == -1:
            break
        open_brace = text.find("{", at)
        if open_brace == -1:
            break
        entry_type = text[at + 1 : open_brace].strip().lower()
        depth = 1
        j = open_brace + 1
        while j < len(text) and depth:
            if text[j] == "{":
                depth += 1
            elif text[j] == "}":
                depth -= 1
            j += 1
        raw = text[open_brace + 1 : j - 1]
        key, _, body = raw.partition(",")
        fields = {"id": key.strip(), "type": entry_type}
        k = 0
        while k < len(body):
            match = re.search(r"([A-Za-z][A-Za-z0-9_-]*)\s*=", body[k:])
            if not match:
                break
            name = FIELD_MAP.get(match.group(1).lower(), match.group(1).lower())
            k += match.end()
            while k < len(body) and body[k].isspace():
                k += 1
            if k >= len(body):
                break
            if body[k] == "{":
                depth = 1
                start = k + 1
                k += 1
                while k < len(body) and depth:
                    if body[k] == "{":
                        depth += 1
                    elif body[k] == "}":
                        depth -= 1
                    k += 1
                value = body[start : k - 1]
            elif body[k] == '"':
                start = k + 1
                k += 1
                while k < len(body) and body[k] != '"':
                    k += 1
                value = body[start:k]
                k += 1
            else:
                start = k
                while k < len(body) and body[k] not in ",\n":
                    k += 1
                value = body[start:k]
            fields[name] = strip_latex(value)
            while k < len(body) and body[k] != ",":
                k += 1
            if k < len(body) and body[k] == ",":
                k += 1
        entries.append(fields)
        i = j
    return entries


def publication_line(entry: dict[str, str]) -> str:
    authors = author_text(split_authors(entry.get("author", "")))
    year = entry.get("year", "n.d.")
    title = entry.get("title", "Untitled")
    venue = entry.get("journal") or entry.get("booktitle") or entry.get("publisher", "")
    volume = entry.get("volume", "")
    number = entry.get("number", "")
    pages = entry.get("pages", "")

    details = venue
    if volume:
        details += f", {volume}"
        if number:
            details += f"({number})"
    elif number:
        details += f", ({number})" if details else f"({number})"
    if pages:
        details += f", {pages}" if details else pages

    pieces = [p for p in [authors, f"({year}).", title + ".", details] if p]
    return " ".join(pieces)


def main() -> None:
    entries = parse_entries(BIB_PATH.read_text())
    groups = {
        "peer": {"title": "Peer-Reviewed Publications", "items": []},
        "nonpeer": {"title": "Non-Peer-Reviewed Contributions", "items": []},
        "review": {"title": "Book Reviews", "items": []},
        "oped": {"title": "Op-Eds", "items": []},
    }
    fallback = {"title": "Other Publications", "items": []}

    for index, entry in enumerate(entries):
        keyword = entry.get("keywords", entry.get("keyword", "")).lower().strip()
        target = groups.get(keyword, fallback)
        item = {
            "id": entry.get("id", f"pub-{index}"),
            "year": entry.get("year", ""),
            "title": entry.get("title", ""),
            "authors": split_authors(entry.get("author", "")),
            "venue": entry.get("journal") or entry.get("booktitle") or entry.get("publisher", ""),
            "doi": entry.get("doi", ""),
            "url": entry.get("url", ""),
            "citation": publication_line(entry),
        }
        target["items"].append(item)

    output = {"groups": [group for group in groups.values() if group["items"]]}
    if fallback["items"]:
        output["groups"].append(fallback)

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    OUT_PATH.write_text(json.dumps(output, indent=2, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
