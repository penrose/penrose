'''
- Assumes only one scene
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
# Type: [prefix, count]
ids = {
    'PathVertex': ['v', 0],
    'DiffuseObject': ['d', 0],
    'SpecularObject': ['s', 0],
    'LightSource': ['l', 0],
    'Camera': ['c', 0]
}
CANVAS_WIDTH = 0
CANVAS_HEIGHT = 0

def valueOf(json, s):
    res = [v['value'] for v in json['values'] if v['name'] == s]
    return res[0]

def genSubstance():
    '''Generate a Substance program from the context'''
    res = ''
    for path_id, path in paths.iteritems():
        vertices = []
        pathString = genPathString(path['form'])
        print "generated an instance of a sampled path: ", pathString

        # declare verts
        for v in pathString:
            type, id, decl = vertexDecl(v, path_id)
            res += decl
            vertices.append((type, id))
        path['vertices'] = vertices

        # declare edges
        vertexIds = [v for typ, v in vertices]
        for v0, v1 in zip(vertexIds, vertexIds[1:]):
            res += edgeDecl(v0, v1)

        # declare scene objs
        diffuseExists = False
        path['objects'] = []
        for type, v in path['vertices']:
            if type == 'Diffuse':
                if not diffuseExists:
                    diffuseExists = True
                    line, id = nextDecl('DiffuseObject')
                else: continue
            elif type == 'Specular':
                line, id = nextDecl('SpecularObject')
            elif type == 'Eye':
                line, id = nextDecl('Camera')
            elif type == 'Light':
                line, id = nextDecl('LightSource')
            else:
                exit('unrecognized vertex type {0} when generating scene obj'.format(type))
            res += line
            res += 'InOS({0}, {1})\n'.format(id, path['scene'])
            path['objects'].append((type, id))

        # HACK: find first obj of the same type everytime
        # COMBAK: support multiple specular object??
        specularCount = 0
        for vType, vId in path['vertices']:
            if(vType == 'Specular'): 
                oType, oId = findObj(vType, path['objects'], specularCount) 
                specularCount = specularCount + 1
            else:
                oType, oId = findObj(vType, path['objects']) 
            res += 'Hits({0}, {1})\n'.format(vId, oId)

    return res

def findObj(vType, objList, idx=0):
    return filter(lambda (t, i): t == vType, objList)[idx]

def nextDecl(type):
    id   = ids[type][0] + str(ids[type][1])
    ids[type][1] = ids[type][1] + 1
    line = type + ' ' + id + '\n'
    return line, id

def edgeDecl(v0, v1):
    res = ''
    id = 'e_' + v0 + '_' + v1
    res += 'PathEdge {0}\n'.format(id)
    res += '{0} := CreateEdge({1}, {2})\n'.format(id, v0, v1)
    return res

def vertexDecl(v, path):
    line, id = nextDecl('PathVertex')
    res = line
    if v == 'L':
        res += 'OnLight(' + id + ')\n'
        type = 'Light'
    elif v == 'S':
        res += 'IsSpecular(' + id + ')\n'
        type = 'Specular'
    elif v == 'D':
        res += 'IsDiffuse(' + id + ')\n'
        type = 'Diffuse'
    elif v == 'E':
        res += 'OnEye(' + id + ')\n'
        type = 'Eye'
    else:
        exit('unrecognized path vertex string: ' + v)
    res += 'InVP({0}, {1})\n'.format(id, path)
    return type, id, res


def process(json):
    '''Process JSON input from the backend and populate context'''
    substance = json['substance']
    params = json['params']

    # Go through objects
    for obj in substance['objects']:
        type = obj['objType']
        name = obj['objName']
        if type == 'Path':
            paths[name] = {}

    # Go through predicates
    for pred in substance['constraints']['predicates']:
        name = pred['pname']
        if name == 'HasForm':
            pathName, form = pred['pargNames']
            paths[pathName]['form'] = valueOf(substance, form)
        elif name == 'SceneSatisfies':
            sceneId, pathId = pred['pargNames']
            paths[pathId]['scene'] = sceneId

    # Retrieve parameters 
    global CANVAS_WIDTH, CANVAS_HEIGHT
    CANVAS_WIDTH = params[0]['contents']['contents']
    CANVAS_HEIGHT = params[1]['contents']['contents']

def genPathString(form):
    '''Generate a path string given canvas dimensions (globals) and a path form'''
    # determine the number of objects in the scene by some heuristics
    # 2 (L and E) + whatever that fits
    maxObjects = int(CANVAS_HEIGHT * CANVAS_WIDTH / (200 * 100))
    
    # Stats about the path form
    numPatterns = len(filter(lambda c: c.isalpha(), form))
    numIndefinites = len(re.findall(r"\w(?=(\+|\*))", form))

    shortestForm = re.sub(r"\w(\?|\*)", '', form)
    minObjects = len(shortestForm)
    limit = int(maxObjects / numIndefinites)

    if maxObjects < minObjects:
        form = shortestForm
        limit = 1
        print "The scene is too small to fit more objects. Generating the minimal string: ", shortestForm

    print 'Limit per expansion computed via canvas dimensions: \n', limit
    gen = Xeger(limit=limit)
    return gen.xeger(form)

if __name__ == '__main__':
    inputFile  = 'Sub_enduser.json'
    outputFile = 'Sub_instantiated.sub'

    # load json data from input file
    with open(inputFile) as f:
        data = json.load(f)
    pprint(data)

    # process data
    print 'received JSON: ', data
    process(data)

    # emit Substance code
    subOut = genSubstance()
    print 'Substance code generated: \n', subOut

    # Write results to the output file
    with open(outputFile, 'w') as the_file:
        the_file.write(subOut)
