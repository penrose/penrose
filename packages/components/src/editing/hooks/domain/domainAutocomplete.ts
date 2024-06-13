import {useCallback} from 'react';
import {CompletionContext} from '@codemirror/autocomplete';
import {syntaxTree} from '@codemirror/language';

const keywordOptions = ["symmetric", "predicate", "type", "function", 
    "constructor"].map(kw => ({label: kw, type: "keyword"}))

const DomainAutocomplete = () => {
    return useCallback(async (context: CompletionContext) => {
        let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1)
        let leftSib = nodeBefore.prevSibling
        let word = context.matchBefore(/\w*/)
        let wholeTree = syntaxTree(context.state).topNode

        // console.log(nodeBefore.toString(), leftSib, word, wholeTree.toString(), nodeBefore)

        // not sure what this does, stolen from autocomplete example 
        if (word == null || (word.from === word.to && !context.explicit)) {
            return null
        }

        /*
        Autocomplete keyword only if there's nothing to the left of it
        Trying to case on if the characters are inside of some declaration
        doesn't work because by default, characters are viewed as part
        of subtype since subtype starts with Identifier 
        */
        if (leftSib === null) {
            return {
                from: word.from, 
                options: keywordOptions 
            }
        }
        // Exception: Autocomplete predicate if it's following symmetric 
        else if (leftSib.name === "symmetric") {
            return {
                from: word.from,
                options: [{label: "predicate", type: "keyword"}]
            }
        }

        return null

    }, [])
}

export default DomainAutocomplete;