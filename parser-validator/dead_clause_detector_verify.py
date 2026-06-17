import re
import argparse
import sys


def get_code_line(code: str, line_number: int) -> str:
    return code[line_number - 1].strip()


def evaluate(erlang_file: str, output_file: str):
    # Read the Erlang source code
    with open(erlang_file, 'r', encoding='utf-8') as f:
        erlang_lines = f.read().splitlines()
        
    # Read the detector output
    with open(output_file, 'r', encoding='utf-8') as f:
        output_content = f.read()

    # Find all lines with "unreachable" clause
    expected_lines = {
        i for i, line in enumerate(erlang_lines, start=1)
        if not line.strip().startswith("%") and re.search(r'->\s+unreachable', line)
    }

    # Extract line numbers from the detector output
    reported_lines = {int(m) for m in re.findall(r'anno\((\d+),\s*\d+\)', output_content)}

    true_positives = expected_lines & reported_lines
    false_positives = reported_lines - expected_lines
    false_negatives = expected_lines - reported_lines

    print(f"N defined dead clauses:   {len(expected_lines)}")
    print(f"N reported dead clauses:  {len(reported_lines)}")
    print(f"True Positives:           {len(true_positives)}")
    print(f"False Positives:          {len(false_positives)}")
    print(f"False Negatives:          {len(false_negatives)}")

    if false_positives:
        print("\nFalse Positives:")
        for ln in sorted(false_positives):
            code_snippet = get_code_line(erlang_lines, ln)
            print(f"Line {ln}: {code_snippet}")

    if false_negatives:
        print("\nFalse Negatives:")
        for ln in sorted(false_negatives):
            code_snippet = get_code_line(erlang_lines, ln)
            print(f"Line {ln}: {code_snippet}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Evaluate Dead Clause Detector Output")
    parser.add_argument("erlang_file", help="Path to the Erlang source file")
    parser.add_argument("output_file", help="Path to the detector output file")
    
    args = parser.parse_args()
    evaluate(args.erlang_file, args.output_file)