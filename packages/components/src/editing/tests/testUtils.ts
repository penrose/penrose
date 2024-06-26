import { LRParser } from "@lezer/lr";
import {
  getNamespaceDict,
  getNamespaceProps,
} from "../hooks/style/styleAutocompleteUtils";
import { parser } from "../parser/style/style";

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
) {
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

/*
 * Takes an input program, gets the namespace header names and compares it to
 * an expected array of namespaces
 */
export function testNamespaces(input: string, expected: string[]) {
  const parsedTree = parser.parse(input);
  const namespaceCache = getNamespaceDict(parsedTree.topNode, input);
  // Convert to array of namespaces
  const foundNamespaces: string[] = Object.keys(namespaceCache);

  return (
    expected.every((key) => foundNamespaces.includes(key)) &&
    foundNamespaces.every((key) => expected.includes(key))
  );
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
  return (
    namespaceProps.every((prop) => expected.includes(prop)) &&
    expected.every((prop) => namespaceProps.includes(prop))
  );
}
