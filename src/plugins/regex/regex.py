'''
- Assumes only one scene
- Assumes only one path sample
'''

import json
import random
from pprint import pprint
from xeger import Xeger   # string generator from regex


paths = {}
ids = {
    'PathVertex': ['v', 0]
}
numSamples = 0
scene = ""

def valueOf(json, s):
    res = [v['value'] for v in json['values'] if v['name'] == s]
    return res[0]

def genSubstance():
    res = ''
    for id, path in paths.iteritems():
        vertices = []
        pathString = gen.xeger(path['form'])
        print "generated an instance of a sampled path: ", pathString

        # declare verts
        for v in pathString:
            id, decl = vertexDecl(v)
            res += decl
            vertices.append(id)
        path['vertices'] = vertices

        # declare edges
        print vertices
        for v0, v1 in zip(vertices, vertices[1:]):
            res += edgeDecl(v0, v1)

        # declare scene objs

    return res

def nextVert(type):
    id   = ids[type][0] + str(ids[type][1])
    ids[type][1] = ids[type][1] + 1
    line = type + ' ' + id + '\n'
    return line, id

def edgeDecl(v0, v1):
    res = ''
    id = 'e_' + v0 + '_' + v1
    res += 'PathEdge {0}\n'.format(id)
    res += 'In({0})\n'.format(v0)
    res += 'In({0})\n'.format(v1)
    return res

def vertexDecl(v):
    line, id = nextVert('PathVertex')
    res = line
    if v == 'L':
        res += 'OnLight(' + id + ')\n'
    elif v == 'S':
        res += 'IsSpecular(' + id + ')\n'
    elif v == 'D':
        res += 'IsDiffuse(' + id + ')\n'
    elif v == 'E':
        res += 'OnEye(' + id + ')\n'
    else:
        exit('unrecognized path vertex string: ' + v)
    return id, res


def process(json, gen):
    # Go through objects
    for obj in json['objects']:
        type = obj['objType']
        name = obj['objName']
        if type == 'Path':
            paths[name] = {}

    # Go through predicates
    for pred in json['constraints']['predicates']:
        name = pred['pname']
        if name == 'HasForm':
            pathName, form = pred['pargNames']
            paths[pathName]['form'] = valueOf(json, form)
        elif name == 'SceneSatisfies':
            pathId, sceneId = pred['pargNames']
            paths[pathId]['scene'] = sceneId



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
    print 'received JSON: ', data
    process(data, gen)

    # emit Substance code
    subOut = genSubstance()
    print 'Substance code generated: \n', subOut

    # Write results to the output file
    with open(outputFile, 'w') as the_file:
        the_file.write('PathVertex S_u1\n')
