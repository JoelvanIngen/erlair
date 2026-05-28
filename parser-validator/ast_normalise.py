import re
import sys

from pathlib import Path


RULES = {
    # Normalise 'anno(x, y, etc)' to literal 'anno(...)'
    r'\banno\([^)]*\)': 'anno(...)',

    # Normalise paths inside 'fileAttr' to literal 'FILENAME'
    r'("/.*/)([^/]+\.erl")': 'FILENAME',
}


def validate_args() -> None:
    if len(sys.argv) < 2:
        print("Usage: python diff_ast.py <file1> [<file2> <file3> ... <fileN>]")
        sys.exit(1)


def normalise(text: str) -> str:
    """Applies all regex normalisation rules"""
    for pattern, replacement in RULES.items():
        text = re.sub(pattern, replacement, text)
    return text


def normalise_file(file: str) -> None:
    new_filename = f"{Path(file).stem}_norm.txt"

    with open(file, "r", encoding="utf-8") as f_src:
        with open(new_filename, "w", encoding="utf-8") as f_dest:
            f_dest.write(normalise(f_src.read()))


def normalise_files(files: list[str]) -> None:
    for file in files:
        normalise_file(file)


if __name__ == "__main__":
    validate_args()
    normalise_files(sys.argv[1:])
