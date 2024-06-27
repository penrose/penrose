import { CompletionContext, CompletionResult } from "@codemirror/autocomplete";
import { EditorState, Extension } from "@codemirror/state";
import { LRParser } from "@lezer/lr";
import { assert } from "vitest";
import { createDomainAutocomplete } from "../hooks/domain/domainAutocomplete";
import { getDomainCache } from "../hooks/domain/getDomainCache";
import { getShapeDefs } from "../hooks/hooksUtils";
import { createStyleAutocomplete } from "../hooks/style/styleAutocomplete";
import {
  getNamespaceDict,
  getNamespaceProps,
} from "../hooks/style/styleAutocompleteUtils";
import { getSubstanceCache } from "../hooks/substance/getSubstanceCache";
import { createSubstanceAutocomplete } from "../hooks/substance/substanceAutocomplete";
import { domainLanguageSupport } from "../parser/domain/domainLanguage";
import { parser } from "../parser/style/style";
import { styleLanguageSupport } from "../parser/style/styleLanguage";
import { substanceLanguageSupport } from "../parser/substance/substanceLanguage";
import { DomainCache } from "../types";

export function hasNoErrors(parser: LRParser, input: string) {
  let tree = parser.parse(input);
  let hasNoErrors = true;
  tree.iterate({
    enter: (node) => {
      if (node.type.isError)
        assert.fail(`Unexpected error node in ${node}. Input: ${input}`);
    },
  });
  return hasNoErrors;
}

export function constructSubstanceCacheObj(vars: string[]) {
  return { varNames: vars };
}

export function constructDomainCacheObj(
  typeNames: string[],
  predNames: string[],
  fnNames: string[],
  consNames: string[],
): DomainCache {
  return { typeNames, predNames, fnNames, consNames };
}

// Assumes each value is an array. Not sensitive to array element order.
export function compareDicts(dict1: any, dict2: any) {
  return (
    Object.keys(dict1).length === Object.keys(dict2).length &&
    Object.keys(dict1).every(
      (key) =>
        dict2.hasOwnProperty(key) &&
        new Set(dict1[key]).size === new Set(dict2[key]).size &&
        dict1[key].every((item: any) => dict2[key].includes(item)),
    )
  );
}

// Checks all elements in array 1 are in array 2
function arr1Subsetarr2(arr1: string[], arr2: string[]) {
  return arr1.every((key) => arr2.includes(key));
}

function sameItems(arr1: string[], arr2: string[]) {
  return arr1Subsetarr2(arr1, arr2) && arr1.length === arr2.length;
}

/*
 * Takes an input program, gets the namespace header names and compares it to
 * an expected array of namespaces
 */
export function testNamespaces(input: string, expected: string[]) {
  const parsedTree = parser.parse(input);
  const namespaceCache = getNamespaceDict(parsedTree.topNode, input);
  // Convert to array of namespaces
  const foundNamespaces: string[] = Object.keys(namespaceCache);

  if (!sameItems(expected, foundNamespaces)) {
    assert.fail(
      `"Failed namespaces test. 
      Expected: ${expected}
      Recieved: ${foundNamespaces}
      Program: ${input}`,
    );
  }
}

/*
 * Takes an input program and namespace name, then gets all the properties
 * of that namespaces. Then compares it to an expected array of properties
 */
export function testNamespaceProps(
  input: string,
  namespace: string,
  expected: string[],
) {
  const parsedTree = parser.parse(input);
  const namespaceCompletions = getNamespaceProps(
    parsedTree.topNode,
    input,
    namespace,
  );
  const namespaceProps = namespaceCompletions.map((cmpl) => cmpl.label);

  if (!sameItems(expected, namespaceProps)) {
    assert.fail(
      `"Failed namespace props test. 
      Expected: ${expected}
      Recieved: ${namespaceProps}
      Program: ${input}`,
    );
  }
}

export function CompletionsToLabels(
  completions: CompletionResult | null,
): string[] {
  if (completions == null) return [];
  return completions.options.map((cmp) => cmp.label.trim());
}

function constructContext(
  input: string,
  cursorOffset: number,
  extensions: Extension[],
): CompletionContext {
  const state = EditorState.create({
    doc: input,
    extensions: extensions,
  });

  return new CompletionContext(state, input.length - cursorOffset, true);
}

export async function testDomainAutocomplete(
  input: string,
  expected: string[],
  cursorOffset = 0,
) {
  const context = constructContext(input, cursorOffset, [
    domainLanguageSupport(),
  ]);
  const domainCache = getDomainCache(input);
  const autocompleteFn = createDomainAutocomplete(domainCache);
  const results = await autocompleteFn(context);
  const completionLabels = CompletionsToLabels(results);

  if (!sameItems(completionLabels, expected)) {
    assert.fail(
      `"Failed domain autocomplete test. 
      Expected: ${expected}
      Recieved: ${completionLabels}
      Program: ${input}`,
    );
  }
}

export async function testSubstanceAutocomplete(
  input: string,
  domainProg: string,
  expected: string[],
  cursorOffset = 0,
) {
  const context = constructContext(input, cursorOffset, [
    substanceLanguageSupport(),
  ]);
  const domainCache = getDomainCache(domainProg);
  const substanceCache = getSubstanceCache(input);
  const autocompleteFn = createSubstanceAutocomplete(
    domainCache,
    substanceCache,
  );
  const results = await autocompleteFn(context);
  const completionLabels = CompletionsToLabels(results);

  if (!sameItems(completionLabels, expected)) {
    assert.fail(
      `"Failed substance autocomplete test. 
      Expected: ${expected}
      Recieved: ${completionLabels}
      Program: ${input}`,
    );
  }
}

/*
 * cursorOffset: Cursor initialized to the last character. cursorOffset sets
 * cursor to last character - cursorOffset
 * semi: To check for expected is subset of completions rather than strict
 * equality
 */
export async function testStyleAutocomplete(
  input: string,
  domainProg: string,
  expected: string[],
  cursorOffset = 0,
  semi = false,
) {
  const context = constructContext(input, cursorOffset, [
    styleLanguageSupport(),
  ]);
  const domainCache = getDomainCache(domainProg);
  const autocompleteFn = createStyleAutocomplete(domainCache, getShapeDefs());
  const results = await autocompleteFn(context);
  const completionLabels = CompletionsToLabels(results);

  if (semi) {
    if (!arr1Subsetarr2(expected, completionLabels)) {
      assert.fail(
        `"Failed subset style autocomplete test. 
        Expected (subset): ${expected}
        Recieved: ${completionLabels}
        Program: ${input}`,
      );
    }
    return;
  }

  if (!sameItems(expected, completionLabels)) {
    assert.fail(
      `"Failed style autocomplete test. 
      Expected: ${expected}
      Recieved: ${completionLabels}
      Program: ${input}`,
    );
  }
}
