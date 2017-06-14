import fileinput
import json
import sys
from argparse import ArgumentParser
import os.path

file_name = "penrose_counter.json"

def main():
    # Parsing argument
    parser = ArgumentParser()
    parser.add_argument('--view-count', '-v', action="store_true", dest='view', default=False)
    parse_result = parser.parse_args()
    view = parse_result.view

    exits = os.path.exists(file_name)
    if exits:
        fp = open(file_name, "r+")
        data = json.load(fp)
        fp.close()
    else:
        data = { 'line_count' : 0 }
    line_count = data['line_count']

    if(view):
        print("You have written " + str(line_count) + " lines of code in Penrose!")
    else:
        fp = open(file_name, "w+")
        new_lines = 0
        for line in sys.stdin:
            new_lines += 1
        data["line_count"] = new_lines + line_count
        print("Added " + str(new_lines) + " lines of code in Penrose!")
        json.dump(data, fp)
        fp.close()

if(__name__ == "__main__"):
    main()
