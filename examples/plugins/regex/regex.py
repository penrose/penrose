'''
- Assumes only one DiffuseObject
- Assumes only one path sample
- Generates exactly as many objects as vertices
- One-to-one relationship between specular objects and vertices
- Takes two constants as parameters: CANVAS_WIDTH, CANVAS_HEIGHT of type `AnnoFloat`, specially `Fix` instances. Otherwise the plugin throws an error
'''

import json
import re
import random
from pprint import pprint
from xeger import Xeger   # string generator from regex


paths = {}

CANVAS_WIDTH = 0
CANVAS_HEIGHT = 0


class Context:
    def __init__(self):
        self._substanceOut = ""
        # Type: [prefix, count]
        self._ids = {
            'PathVertex': ['v', 0],
            'DiffuseObject': ['d', 0],
            'SpecularObject': ['s', 0],
            'LightSource': ['l', 0],
            'Camera': ['c', 0]
        }

    def nextDecl(self, type):
        prefix, count = self._ids[type]
        id = prefix + str(count)
        self.incrementCounter(type)
        self._substanceOut += type + ' ' + id + '\n'
        return id

    def incrementCounter(self, type):
        self._ids[type][1] = self._ids[type][1] + 1

    def edgeDecl(self, v0, v1):
        id = 'e_' + v0 + '_' + v1
        self.addLine('PathEdge {0}'.format(id))
        self.addLine('{0} := CreateEdge({1}, {2})'.format(id, v0, v1))

    def vertexDecl(self, v, path):
        id = self.nextDecl('PathVertex')
        if v == 'L':
            self.addLine('OnLight(' + id + ')')
            type = 'Light'
        elif v == 'S':
            self.addLine('IsSpecular(' + id + ')')
            type = 'Specular'
        elif v == 'D':
            self.addLine('IsDiffuse(' + id + ')')
            type = 'Diffuse'
        elif v == 'E':
            self.addLine('OnEye(' + id + ')')
            type = 'Eye'
        else:
            exit('unrecognized path vertex string: ' + v)
        self.addLine('InVP({0}, {1})'.format(id, path))
        return type, id

    def addLine(self, s):
        self._substanceOut += s + '\n'


def genSubstance(ctx):
    '''Generate a Substance program from the context'''

    diffuseObject = ctx.nextDecl("DiffuseObject")
    eye = ctx.nextDecl("Camera")
    light = ctx.nextDecl("LightSource")

    for path_id, path in paths.iteritems():

        # for each path form, generate a path string
        pathString = genPathString(path['form'])
        print "generated an instance of a path: ", pathString

        # NOTE: we are not generating different strings for each sample yet
        for sample in path['samples']:
            vertices = []
            # declare verts
            for v in pathString:
                type, id = ctx.vertexDecl(v, path_id)
                vertices.append((type, id))

            # declare edges
            vertexIds = [v for typ, v in vertices]
            for v0, v1 in zip(vertexIds, vertexIds[1:]):
                ctx.edgeDecl(v0, v1)

            sample['vertices'] = vertices

        if len(path['samples']) > 0:
            # if there are path samples, declare scene objs
            # NOTE: using just the first sample to declare objects
            path['objects'] = []
            for type, v in path['samples'][0]['vertices']:
                if type == 'Diffuse':
                    id = diffuseObject
                elif type == 'Specular':
                    id = ctx.nextDecl('SpecularObject')
                elif type == 'Eye':
                    id = eye
                elif type == 'Light':
                    id = light
                else:
                    exit(
                        'unrecognized vertex type {0} when generating scene obj'.format(type))
                path['objects'].append((type, id))

        # Generate hits
        # HACK: find first obj of the same type everytime
        # COMBAK: support multiple specular object??
        for sample in path['samples']:
            specularCount = 0
            for vType, vId in sample['vertices']:
                if(vType == 'Specular'):
                    oType, oId = findObj(vType, path['objects'], specularCount)
                    specularCount = specularCount + 1
                else:
                    oType, oId = findObj(vType, path['objects'])
                ctx.addLine('Hits({0}, {1})'.format(vId, oId))


def valueOf(json, s):
    res = [v['value'] for v in json['values'] if v['name'] == s]
    return res[0]


def findObj(vType, objList, idx=0):
    return filter(lambda (t, i): t == vType, objList)[idx]


def process(json):
    '''Process JSON input from the backend and populate context'''
    substance = json['substance']
    params = json['params']

    # Go through objects
    for obj in substance['objects']:
        type = obj['objType']
        name = obj['objName']
        if type == 'PathType':
            paths[name] = {'samples': [], 'form': ""}

    # Go through predicates
    for pred in substance['constraints']['predicates']:
        name = pred['pname']
        if name == 'HasForm':
            pathName, form = fromLeft(pred['pargs'])
            # HasForm predicate is not nested
            paths[pathName]['form'] = form

    # Go through functions
    for func in substance['constraints']['functions']:
        name = func['fname']
        if name == 'Sample':
            pname = func['fargNames'][0]
            sample = func['varName']
            paths[pname]['samples'].append({'name': sample, 'vertices': []})
    print 'loaded paths: ', paths

    # Retrieve parameters
    global CANVAS_WIDTH, CANVAS_HEIGHT
    CANVAS_WIDTH = params[0]['contents']['contents']
    CANVAS_HEIGHT = params[1]['contents']['contents']


def genPathString(form):
    '''Generate a path string given canvas dimensions (globals) and a path form'''
    # determine the number of objects in the scene by some heuristics
    # 2 (L and E) + whatever that fits
    maxObjects = int(CANVAS_HEIGHT * CANVAS_WIDTH / (100 * 100))

    # Stats about the path form
    numIndefinites = len(re.findall(r"(?=(\+|\*))", form))

    if numIndefinites == 0:
        limit = 1
    else:
        limit = int(maxObjects / numIndefinites)

    print 'Limit per expansion computed via canvas dimensions: \n', limit
    gen = Xeger(limit=limit)
    return gen.xeger(form)


def fromLeft(maybes):
    return map(lambda m: m['Left'], maybes)


if __name__ == '__main__':
    inputFile = 'Sub_enduser.json'
    outputFile = 'Sub_instantiated.sub'

    # load json data from input file
    with open(inputFile) as f:
        data = json.load(f)
    pprint(data)

    # process data
    print 'received JSON: ', data
    process(data)

    # emit Substance code
    ctx = Context()
    genSubstance(ctx)
    print 'Substance code generated: \n', ctx._substanceOut

    # Write results to the output file
    with open(outputFile, 'w') as the_file:
        the_file.write(ctx._substanceOut)
