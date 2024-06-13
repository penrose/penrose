import {useCallback} from 'react';
import {CompletionContext} from '@codemirror/autocomplete';
import {syntaxTree} from '@codemirror/language';
import { SyntaxNode } from '@lezer/common';

const keywordOptions = ["Let", "AutoLabel", "Label", "NoLabel"]
    .map(kw => ({label: kw, type: "keyword"}))

function InsideStatement(parentNode: SyntaxNode | null) {
    return parentNode !== null && (
        parentNode.name === "TypeApp" ||
        parentNode.name === "PredicateApp" ||
        parentNode.name === "Fn_ConsApp" ||
        parentNode.name === "Labeling")
}

const SubstanceAutocomplete = () => {
    return useCallback(async (context: CompletionContext) => {
        let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1)
        let parentNode = nodeBefore.parent
        let leftSib = nodeBefore.prevSibling
        // In erorr state, error node wraps the current node
        if (parentNode != null && parentNode.type.isError) {
            leftSib = parentNode.prevSibling
            parentNode = parentNode.parent
        }

        let word = context.matchBefore(/\w*/)
        let wholeTree = syntaxTree(context.state).topNode
 
        console.log(parentNode, leftSib, wholeTree.toString())

        // not sure what this does, stolen from autocomplete example 
        if (word == null || (word.from === word.to && !context.explicit)) {
            return null
        }

        /*
        Autocomplete keyword only if there's nothing to the left of it

        */
        if (leftSib === null && InsideStatement(parentNode)) {
            return {
                from: word.from, 
                options: keywordOptions 
            }
        }
        else if (leftSib !== null && leftSib.name === "AutoLabel") {
            return {
                from: word.from,
                options: [{label: "All", type: "keyword"}]
            }
        }
        else if (leftSib !== null && leftSib.name === "Range") {
            return {
                from: word.from,
                options: [{label: "where", type: "keyword"}]
            }
        }
        // for suggestor, will suggest in correct spaces in all but labeling statements
        else if (InsideStatement(parentNode) && leftSib !== null 
            // To suggest after TypeApp
            && (((leftSib.name === "Identifier") && (leftSib.prevSibling != null && leftSib.prevSibling.type.name === "Identifier"))
            // To suggest after PredicateApp and Fn_ConsApp
            || leftSib.name === "ArgList" )) {
                return {
                    from: word.from,
                    options: [{label: "for", type: "keyword"}]
                }
            }

        return null

    }, [])
}

export default SubstanceAutocomplete;