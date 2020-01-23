"""Penrose Registry Generator.

Usage:
  generateRegistry.py <substance-path> <n> <style-path> <style-label> <domain-path> <domain-label> 
  generateRegistry.py (-h | --help)
  generateRegistry.py --version

Options:
  -h --help     Show this screen.
  --version     Show version.
"""

import json
from docopt import docopt
import os
import shutil


def genSub(path):
    sub = {
        "substanceURI": path,
        "element": 0,
        "style": 0,
        "previewURI": "",
        "name": os.path.basename(path)
    }
    return sub


if __name__ == '__main__':
    args = docopt(__doc__, version='Penrose registry generator 0.1')
    subPrefix = args["<substance-path>"]
    styPath = args["<style-path>"]
    domainPath = args["<domain-path>"]

    # generate Style registry
    styFile = os.path.basename(styPath)
    styleReg = [{
        "value": 0,
        "label": args['<style-label>'],
        "icon": "",
        "element": 0,
        "uri": styFile
    }]
    with open(subPrefix + '/style.json', 'w') as outfile:
        json.dump(styleReg, outfile)
    shutil.copyfile(styPath, subPrefix + '/' + styFile)

    # generate Domain registry
    domainFile = os.path.basename(domainPath)
    domainReg = [{
        "value": 0,
        "label": args['<domain-label>'],
        "icon": "",
        "element": 0,
        "uri": domainFile
    }]
    with open(subPrefix + '/domain.json', 'w') as outfile:
        json.dump(domainReg, outfile)
    shutil.copyfile(domainPath, subPrefix + '/' + domainFile)

    # generate Substance registry
    n = int(args["<n>"])
    subPaths = ['prog-' +
                str(i) + '.sub' for i in range(1, n + 1)]
    subReg = list(map(genSub, subPaths))
    with open(subPrefix + '/substance.json', 'w') as outfile:
        json.dump(subReg, outfile)
