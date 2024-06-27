import { CompletionContext, CompletionResult } from "@codemirror/autocomplete";
import { EditorState } from "@codemirror/state";
import { LRParser } from "@lezer/lr";
import { createDomainAutocomplete } from "../hooks/domain/domainAutocomplete";
import { getDomainCache } from "../hooks/domain/getDomainCache";
import {
  getNamespaceDict,
  getNamespaceProps,
} from "../hooks/style/styleAutocompleteUtils";
import { domainLanguageSupport } from "../parser/domain/domainLanguage";
import { parser } from "../parser/style/style";
import { DomainCache } from "../types";

export function hasNoErrors(parser: LRParser, input: string) {
  let tree = parser.parse(input);
  let hasNoErrors = true;
  tree.iterate({
    enter: (node) => {
      return true;
    },
    leave: (node) => {
      if (node.type.isError) hasNoErrors = false;
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

function sameItems(arr1: string[], arr2: string[]) {
  return arr1.every((key) => arr2.includes(key)) && arr1.length === arr2.length;
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

  return sameItems(expected, foundNamespaces);
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
  return sameItems(namespaceProps, expected);
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
): CompletionContext {
  const state = EditorState.create({
    doc: input,
    extensions: [domainLanguageSupport()],
  });

  return new CompletionContext(state, input.length - cursorOffset, true);
}

export async function testDomainAutocomplete(
  input: string,
  expected: string[],
  cursorOffset = 0,
) {
  const context = constructContext(input, cursorOffset);
  const domainCache = getDomainCache(input);
  const autocompleteFn = createDomainAutocomplete(domainCache);
  const results = await autocompleteFn(context);
  // console.log(results);
  const completionLabels = CompletionsToLabels(results);
  console.log(completionLabels);
  console.log(expected);
  // console.log(sameItems(completionLabels, expected));

  return sameItems(completionLabels, expected);
}
