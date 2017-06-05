#! /usr/bin/env nix-shell
#! nix-shell -i python -p pythonPackages.pyyaml
"""yaml2json"""

import json
import sys

import yaml


def do_file(in_fh, out_fh=sys.stdout):
    json.dump(yaml.safe_load(in_fh), out_fh, indent=2, sort_keys=True)


def main():
    args = sys.argv[1:] or ["-"]
    for arg in args:
        if arg == '-':
            do_file(sys.stdin)
        else:
            with open(arg, 'r') as ifh:
                do_file(ifh)

if __name__ == '__main__':
    main()
