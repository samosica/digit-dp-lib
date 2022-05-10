#!/usr/bin/env python3

import argparse
import os
import sys

def pack(out_file):
    in_files = ["listExt", "math", "monoid", "nat", "automaton", "automatonDP", "digitDP"]
    tab_width = 2
    lines = []

    lines.append("module DigitDPLib = struct\n")

    for file in in_files:
        module_name = file[0].upper() + file[1:]
        file_path = os.path.join("src", f"{file}.ml")

        lines.append(" " * tab_width)
        lines.append(f"module {module_name} = struct\n")

        with open(file_path, mode="r", encoding="UTF-8") as fp:
            for line in fp:
                lines.append(" " * (tab_width * 2))
                lines.append(line)

        lines.append(" " * tab_width)
        lines.append("end\n\n")    

    lines.append("end\n")

    if out_file is None:
        out_fp = sys.stdout
    else:
        out_fp = open(out_file, mode="w", encoding="UTF-8")

    for line in lines:
        out_fp.write(line)

parser = argparse.ArgumentParser(
    description="Pack DigitDPLib into a single file"
)
parser.add_argument(
    "--output", "-o",
    help="file to write all the modules"
)

args = parser.parse_args()
pack(args.output)

