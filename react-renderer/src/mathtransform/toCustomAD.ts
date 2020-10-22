
module.exports = function(fileInfo: any, api: any) {
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
    // transforms binary op to call expression - essentially deconstructs and reconstructs the node
    const BO2CE = (target: string, node: any) => {
        const newCallee = j.identifier(target); // get new name of function
        const newArgs = [node.left, node.right];    // reorder arguments
        node.callee = newCallee;
        node.arguments = newArgs;
        node.type = "CallExpression";
        return node;
    };
    // unary op to call expression
    const UO2CE = (target: string, node: any) => {
        const newCallee = j.identifier(target); // get new name of function
        const newArgs = [node.argument];    // reorder arguments
        node.callee = newCallee;
        node.arguments = newArgs;
        node.type = "CallExpression";
        return node;
    };
    // type preset keyword (number, etc.) to type reference i.e. custom type OR custom type to custom type
    // TS parser treats these differently, so it would have been prettier to separate the two, but for ease of future use I allowed them to be treated the same
    const TYPSUB = (target: string, node: any) => {
        const newTName = j.identifier(target);
        node.type = "TSTypeReference";
        node.typeName = newTName;
        return node;
    }

    const BOPS : {[key:string]:any[]} = {
        "+": ["add", BO2CE],    // [new name, structure to convert to]
        "-": ["sub", BO2CE],    // supports custom structure transformations - e.g. not all bops get transformed to call expressions
        "*": ["mul", BO2CE],    // if you don't want to transform a specific operation at all just write a basic function instead of bo2ce that returns the node
        "/": ["div", BO2CE]
    }; 
    const UOPS : {[key:string]:any[]} = {
        "-": ["neg", UO2CE]
    }
    const TYPS : {[key:string]:any[]} = {
        // "Tensor": ["VarAD", TYPSUB],   // TYPSUB - method to perform type substitution. This will practically always be the method used to transform any type. 
        "TSNumberKeyword": ["VarAD", TYPSUB],
        // "OLDTYPE": ["NEWTYPE", TYPSUB]
        
    }
    // find how a node should be translated and return the translated node
    const transNodes = (nodes: any) => {
        nodes.replaceWith((nodePath: any) => {
            const {node} = nodePath;
            const transformData = getTransformData(node);

            // the below line is theoretically bad practice per https://stackoverflow.com/questions/1098040/checking-if-a-key-exists-in-a-javascript-object
            // however, given that our opmaps are manually generated it is low-risk. 
            return transformData ? transformData[1](transformData[0], node) : node;
        })
    };
    const MARKTAG = "autodiff";
    const j = api.jscodeshift;  
    // https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API
    // The AST explorer (https://astexplorer.net/) with typescript-eslint parser is also extremely helpful for figuring out parses.
    
    const isMarked = (comment: any) => comment.value.trim() === MARKTAG;
    const needsTransform = (nodePath: any) : boolean => {
        return (nodePath.value.leadingComments) && (nodePath.value.leadingComments.some(isMarked)); // if it is marked it needs transform
    }
    // which operation to transform it to, and how to transform it - e.g. binary op to call expression? unary op to call expression? etc.
    const getTransformData = (node: any): any[] => {
        if (node.type === "BinaryExpression") return BOPS[node.operator]
        else if (node.type === "UnaryExpression") return UOPS[node.operator]
        else if (node.type === "TSTypeReference") return TYPS[node.typeName.name]
        else if (node.type in TYPS) return TYPS[node.type]
        else throw new Error("Error: Following node does not have transformable property: \n" + node);
    };

    const root = j(fileInfo.source);
    const tNodes = root.find(j.Node).filter((nodePath: any) => needsTransform(nodePath));
    
    // translate all bops
    const bopNodes = tNodes.find(j.BinaryExpression);
    transNodes(bopNodes);
    // uops
    const uopNodes = tNodes.find(j.UnaryExpression);
    transNodes(uopNodes);
    // types
    for (const typ in TYPS) {
        let typNodes;
        if (KWTYPS.includes(typ)) {
            typNodes = tNodes.find(j[typ])
        }
        else typNodes = tNodes.find(j.TSTypeReference).filter((nodePath:any) => nodePath.value.typeName.name === typ)
        transNodes(typNodes);
    }
    return root.toSource();
  };
module.exports.parser = 'ts';
