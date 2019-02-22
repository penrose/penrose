import json
import random
from pprint import pprint
from xeger import Xeger   # string generator from regex

pathForms  = {}
numSamples = 0
scene = ""

def valueOf(json, s):
    res = [v['value'] for v in json['values'] if v['name'] == s]
    return res[0]

def genSubstance():
    pass

def process(json, gen):
    print json

    # Go through objects
    for obj in json['objects']:
        name = obj['objname']

    # Go through predicates
    for pred in json['constraints']['predicates']:
        name = pred['pname']
        if name == 'HasForm':
            pathName, form = pred['pargNames']
            pathForms[pathName] = valueOf(json, form)


if __name__ == '__main__':
    inputFile  = 'Sub_enduser.json'
    outputFile = 'Sub_instantiated.sub'
    limit = 20

    gen = Xeger(limit=limit)

    # load json data from input file
    with open(inputFile) as f:
        data = json.load(f)
    pprint(data)

    # process data
    process(data, gen)

    # emit Substance code

    print("generated an instance of a sampled path: ")
    print(gen.xeger("LSD*SE"))

    # Write results to the output file
    with open(outputFile, 'w') as the_file:
        the_file.write('PathVertex S_u1\n')
