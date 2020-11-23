import {API, ASTPath, BinaryExpression, CallExpression, Collection, FileInfo, Identifier, Node,
    TSTypeReference, UnaryExpression, MemberExpression, ConditionalExpression, NumericLiteral, BooleanLiteral} from 'jscodeshift'
import {CommentKind, IdentifierKind} from "./node_modules/ast-types/gen/kinds" // todo update PATH!!  

interface Transform{
    newName: string;
    transformMethod: any;
    matchTargetFn: any; // function that will transform the node to a string that we check against in matchMethod. See use cases.
    matchMethod?: any;   // determines how we match the node string to a key in the config dict. Default is equality or you can use a custom match function
    // custom match function must have form : matchMethod(nodestring: string) : boolean
    // see hasMatch() for how it will be used
}

// jscodeshift mathtest.ts -t toCustomAD.ts -p -d

module.exports = function(fileInfo: FileInfo, api: API) {

    const j = api.jscodeshift   // DO NOT REMOVE

    ////////////////////////////////// Section 1: CONFIGURATION //////////////////////////////////
    // The section below contains what is essentially the configuration of the program.
    // It determines which AST nodes will be transformed and how they will be transformed.
    // Change what you need to.


    ////////////////////////////////// Subsection 1: type keywords //////////////////////////////////
    const KWTYPS: string[] = [ // Define all the simple keyword types.
        "TSAnyKeyword",
        "TSBigIntKeyword",
        "TSBooleanKeyword",
        "TSNeverKeyword",
        "TSNullKeyword",
        "TSNumberKeyword",
        "TSObjectKeyword",
        "TSStringKeyword",
        "TSSymbolKeyword",
        "TSUndefinedKeyword",
        "TSUnknownKeyword",
        "TSVoidKeyword",
        "TSThisType",
      ] // from https://github.com/benjamn/ast-types/blob/90a8e63d77fc6134bfd5bfcca793146259219246/def/typescript.ts
    // IMPORTANT: the types module for jscodeshift doesn't include the bigint typing, so I excluded it in the above array. If someone
    // tries to transform it it will break, but I don't think this is likely at all.

    ////////////////////////////////// Subsection 2: Transformation Methods //////////////////////////////////

    // transforms binary op to call expression - essentially deconstructs and reconstructs the node
    const BO2CE = (target: string, node: any) : CallExpression => {
        const newCallee = j.identifier(target); // get new name of function
        const newArgs = [node.left, node.right];    // reorder arguments
        node.callee = newCallee;
        node.arguments = newArgs;
        node.type = "CallExpression";
        return node;
    };
    // unary op to call expression
    const UO2CE = (target: string, node: any) : CallExpression => {
        const newCallee = j.identifier(target); // get new name of function
        const newArgs = [node.argument];    // reorder arguments
        node.callee = newCallee;
        node.arguments = newArgs;
        node.type = "CallExpression";
        return node;
    };
    // type preset keyword (number, etc.) to type reference i.e. custom type OR custom type to custom type
    // TS parser treats these differently, so it would have been prettier to separate the two, but for ease of future use I allowed them to be treated the same
    const TYPSUB = (target: string, node: any) : TSTypeReference => {
        const newTName = j.identifier(target);
        node.type = "TSTypeReference";
        node.typeName = newTName;
        return node;
    }
    // member expression to id
    const ME2ID = (target: string, node:any) : Identifier => {
        node.type = "Identifier";
        node.name = target;
        return node;
    }
    // identifier to identifier
    const ID2ID = (target: string, node:any) : Identifier => {
        node.name = target;
        return node;
    }
    // ternary to call expression
    const TERN2CE = (target: string, node:any) : CallExpression => {
        const ternNode = (node as ConditionalExpression);
        const test = ternNode.test;
        const consequent = ternNode.consequent;
        const alternate = ternNode.alternate;
        const newCallee = j.identifier(target);
        const newArgs = [test, consequent, alternate];
        node.type = "CallExpression";
        node.callee = newCallee;
        node.arguments = newArgs;
        return node;
    }
    // condenses a call expression made out of a member expression and arguments into a single identifier while preserving the arguments
    // e.g. Math.pow(x, y) => pow(x, y)
    // you can accomplish the same thing by transforming the member expression itself, but if you want to access
    // the arguments of the call expression for matching purposes, you have to modify at the call expression level
    const CONDENSECALLEXP = (target: string, node: CallExpression) => {
        if (node.callee.type === "MemberExpression") node.callee = ME2ID(target, node.callee);
        return node;
    }
    // custom transform method for transforming Math.pow(x, 2) into squared(x)
    const squaredTransform = (target: string, node: CallExpression) => {
        node.callee = ME2ID(target, node.callee);
        node.arguments = [node.arguments[0]];    // remove 2 and just leave first argument
        return node;
    }

    ////////////////////////////////// Subsection 3: match target functions //////////////////////////////////

    // extracts operator property from BinExp/UnExp
    const getOperator = (node: BinaryExpression | UnaryExpression) => node.operator;
    // extracts type name from TSTypeReference
    const getTypeRefName = (node: TSTypeReference) => (node.typeName as IdentifierKind).name;
    // converts member expressions to strings, but with no chaining! e.g. a.b.c() will not work, but a.b() will
    // NOTE - ONLY WORKS FOR BINARY MEMBER EXPRESSIONS E.G. A.B;
    // WILL NOT WORK FOR CHAINED MEMBER EXPRESSIONS E.G. A.B.C;
    // todo fix
    const ME2STR = (n: MemberExpression) => (n.object as IdentifierKind).name + "." + (n.property as IdentifierKind).name; // e.g. Math.pow
    // transforms args a, b, c, etc. in a call exp to the string "(a, b, c, ...)"
    const CALLARGS2STR = (n: CallExpression) => {
        if (n.arguments.length == 1) return "()";
        let ret = "(";
        for (const arg of n.arguments) {
            // ASSUMES ARGS ARE IDENTIFIERS! IF YOU HAVE NESTED ARGUMENTS THIS WILL NOT WORK
            if (arg.type === "Identifier") ret += (arg as IdentifierKind).name + ",";
            else if (arg.type === "NumericLiteral") ret += (arg as NumericLiteral).value?.toString() + ",";
            else if (arg.type === "BooleanLiteral") ret += (arg as BooleanLiteral).value?.toString() + ","; 
        }
        return ret.slice(0, -1) + ")"    // remove trailing comma
    }
    const CE2STR = (n: CallExpression) => {
        // only works if the callee is a member expression or identifier
        // will prob crash if n is different
        // todo fix
        if (n.callee.type === "MemberExpression") return ME2STR(n.callee) + CALLARGS2STR(n)
        else return (n.callee as IdentifierKind).name + CALLARGS2STR(n)
    }

    ////////////////////////////////// Subsection 4: transform maps //////////////////////////////////

    // binary operations map
    // typing may need to be changed
    const BOPS : {[k: string]: Transform} = {
        "+": {
            newName: "add",
            transformMethod: BO2CE, // supports custom structure transformations - e.g. not all bops get transformed to call expressions
            matchTargetFn: getOperator      // do we want to check that the operator matches exactly to "+"?
        },
        "-": {
            newName: "sub",
            transformMethod: BO2CE, // if you don't want to transform a specific operation at all just write a basic function instead of bo2ce that returns the node
            matchTargetFn: getOperator
        },
        "*": {
            newName: "mul",
            transformMethod: BO2CE,
            matchTargetFn: getOperator
        },
        "/": {
            newName: "div",
            transformMethod: BO2CE,
            matchTargetFn: getOperator
        },
    }
    const UOPS : {[k: string]: Transform}= {
        "-": {
            newName: "neg",
            transformMethod: UO2CE,
            matchTargetFn: getOperator
        }
    }
    const TYPS : {[k: string]: Transform} = {
        "TSNumberKeyword": {
            newName: "VarAD",
            transformMethod: TYPSUB, // TYPSUB - method to perform type substitution. This will practically always be the method used to transform any type.
            matchTargetFn: getTypeRefName
        }
        // "OldType": {
        //     newName: "NewType",
        //     transformMethod: TYPSUB,
        //     hasMatch: true,
        //     matchTargetFn: ...type extracting fn here
        // }
    }
    // member expressions
    const MEMEXP : {[k: string]: Transform} = {
        "a.b": {
            newName: "ab",
            transformMethod: ME2ID, // TYPSUB - method to perform type substitution. This will practically always be the method used to transform any type.
            matchTargetFn: ME2STR
        }
    }
    // call expressions
    const CALLEXP : {[k: string]: Transform} = {
        "Math.pow(x, y)": { // it doesn't matter what the name is because we provide a custom match method, this is just for clarity
            newName: "pow",
            transformMethod: CONDENSECALLEXP,
            matchTargetFn: CE2STR,
            matchMethod: (nstr: string) => /^Math\.pow\(\w+,([a-zA-z013-9]|\w{2,})\)$/.test(nstr) // should match anything of the form 
            // Math.pow(x, y) where x, y are var or num as long as y != 2
            // I did test it but you never know with regex :/
        },
        "Math.pow(x, 2)": {
            newName: "squared",
            transformMethod: squaredTransform,
            matchTargetFn: CE2STR,
            matchMethod: (nstr: string) => {
                return /^Math\.pow\(\w+,2\)$/.test(nstr)
            }    // should match anything of form Math.pow(x, 2) where x is var or num
        }
    }
    // identifiers
    const IDENT : {[k: string]: Transform} = {
        // "norm": {
        //     newName: "ADNorm",
        //     transformMethod: ID2ID,
        //     matchTargetFn: () => (node as IdentifierKind).name;
        // }
    }
    // ternary expressions
    const TERN : {[k: string]: Transform} = {
        "all": {  // it doesn't matter what the key is b/c we set it to match every ternary expression - although you could change this
            newName: "ifCond",
            transformMethod: TERN2CE,
            matchTargetFn: (n: ConditionalExpression) => "all"
        }
    }

    ////////////////////////////////// Subsection 5: marktag //////////////////////////////////
    const MARKTAG = "autodiff"; // stores what the leading comment will be to indicate that a node should be transformed

    ////////////////////////////////// Subsection 5: Preset Calls //////////////////////////////////

    const translatees : [any, string, any][]= [
        [j.BinaryExpression, "BinaryExpression", BOPS],
        [j.UnaryExpression, "UnaryExpression", UOPS],
        [j.MemberExpression, "MemberExpression", MEMEXP],
        [j.ConditionalExpression, "ConditionalExpression", TERN],
        [j.Identifier, "Identifier", IDENT],
        [j.CallExpression, "CallExpression", CALLEXP]
        // types needs to be set manually
    ]


    ////////////////////////////////// SECTION 2: PROGRAM FLOW //////////////////////////////////
    // Below here is all the generic stuff that makes the program work with JSCodeshift. 
        
    const hasMatch = (n: Node, map: {[k: string]: Transform}) => {
        for (const [k, v] of Object.entries(map)) {
            // if the config provides a custom match method use it, otherwise test that the matchTarget equals the key
            const matchTest = ('matchMethod' in v) ? v.matchMethod(v.matchTargetFn(n)) : (v.matchTargetFn(n) === k);
            if (matchTest) return k;
        }
        return "";
    }

    // find out how a node should be translated and return the translated node
    const transNodes = (nodes: Collection) => {
        nodes.replaceWith((nodePath: ASTPath<Node>) => {
            const {node} = nodePath;
            const transformData = getTransformData(node);
            if (typeof transformData === "boolean") return node; // we will return a boolean only when getTransformData fails to retrieve a key
            return transformData.transformMethod(transformData.newName, node);
        })
    };

    const findAndTranslate = (tnodes: Collection, nodeType: any) => {
        transNodes(tnodes.find(nodeType));
    }

    // https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API
    // The AST explorer (https://astexplorer.net/) with typescript-eslint parser is also helpful for figuring out parses.
    const isMarked = (comment: CommentKind) : boolean => comment.value.trim() === MARKTAG;
    // resists typing
    const needsTransform = (nodePath: any) : boolean => {
        return ((nodePath.value.leadingComments)) && (nodePath.value.leadingComments.some(isMarked)); // if it is marked it needs transform
    }

    // which operation to transform it to, and how to transform it - e.g. binary op to call expression? unary op to call expression? etc.
    const getTransformData = (node: Node): Transform | boolean => {
        const presets = translatees.map(x => x[1]);  // type name
        const presetInd = presets.indexOf(node.type);
        if (presetInd !== -1) {
            const matchKey = hasMatch(node, translatees[presetInd][2])
            return matchKey ? translatees[presetInd][2][matchKey] : false
        }
        // if (node.type === "BinaryExpression") {
        //     const matchKey = hasMatch(node, BOPS)
        //     return matchKey ? BOPS[matchKey] : false
        // }
        // else if (node.type === "UnaryExpression") {
        //     const matchKey = hasMatch((node as UnaryExpression), UOPS)
        //     return matchKey ? UOPS[matchKey] : false
        // }
        // else if (node.type === "MemberExpression") {
        //     const matchKey = hasMatch((node as MemberExpression), MEMEXP);
        //     return matchKey ? MEMEXP[matchKey] : false
        // }
        // else if (node.type === "Identifier") {
        //     const matchKey = hasMatch((node as IdentifierKind), IDENT);
        //     return matchKey ? IDENT[matchKey] : false
        // }
        // else if (node.type === "ConditionalExpression") {
        //     const matchKey = hasMatch((node as ConditionalExpression), TERN)
        //     return matchKey ? TERN[matchKey] : false
        // }
        // else if (node.type === "CallExpression") {
        //     const matchKey = hasMatch((node as CallExpression), CALLEXP);
        //     return matchKey ? CALLEXP[matchKey] : false
        // }
        // todo the next line excludes qualified types e.g. Foo.Bar as a type. fix this
        else if (node.type === "TSTypeReference") { // transform custom types
            const matchKey = hasMatch((node as TSTypeReference), TYPS);
            return matchKey? TYPS[matchKey] : false
        }
        else if (node.type in TYPS) return TYPS[node.type]  // keyword types //todo doublecheck
        else throw new Error("Error: Following node does not have transformable property: \n" + node);
    };

    const root = j(fileInfo.source);
    const tNodes = root.find(j.Node).filter((nodePath: ASTPath<Node>) => needsTransform(nodePath));
    // translate all bops
    findAndTranslate(tNodes, j.BinaryExpression);
    // uops
    findAndTranslate(tNodes, j.UnaryExpression);
    // member expressions
    findAndTranslate(tNodes, j.MemberExpression);
    // identifiers
    findAndTranslate(tNodes, j.Identifier);
    // ternary
    findAndTranslate(tNodes, j.ConditionalExpression);
    // call exp
    findAndTranslate(tNodes, j.CallExpression);

    // types
    for (const typ in TYPS) {
        let typNodes: Collection;
        if (KWTYPS.includes(typ)) {
            typNodes = tNodes.find(j[typ]);
        }
        else typNodes = tNodes.find(j.TSTypeReference).filter((nodePath:ASTPath<TSTypeReference>) => (nodePath.value.typeName as IdentifierKind).name === typ)
        transNodes(typNodes);
    }
    return root.toSource();
  };
module.exports.parser = 'ts';
