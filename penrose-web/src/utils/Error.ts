import { Result } from "true-myth";
const { or, and, ok, err, andThen } = Result;

// TODO: fix template formatting
export const showDomainErr = (error: DomainError): string => {
  switch (error.tag) {
    case "TypeDeclared": {
      return `Type ${error.typeName.value} already exists`;
    }
    case "TypeNotFound": {
      return `Type ${error.typeName.value} (at ${loc(
        error.typeName
      )}) does not exist`;
    }
    case "TypeVarNotFound": {
      return `Type variable ${error.typeVar.name.value} (at ${loc(
        error.typeVar
      )}) does not exist`;
    }
    case "DuplicateName": {
      const { firstDefined, name, location } = error;
      return `Name ${name.value} (at ${loc(
        location
      )}) already exists, first declared at ${loc(firstDefined)}`;
    }
  }
};

// action constructors for error
export const duplicateName = (
  name: Identifier,
  location: ASTNode,
  firstDefined: ASTNode
): DuplicateName => ({
  tag: "DuplicateName",
  name,
  location,
  firstDefined,
});

// const loc = (node: ASTNode) => `${node.start.line}:${node.start.col}`;
const loc = (node: ASTNode) => `line ${node.start.line}`;

export const all = <Ok, Error>(
  ...results: Result<Ok, Error>[]
): Result<Ok, Error> =>
  results.reduce(
    (currentResult: Result<Ok, Error>, nextResult: Result<Ok, Error>) =>
      and(nextResult, currentResult),
    results[0] // TODO: separate out this element in argument
  );

export const safeChain = <Item, Ok, Error>(
  itemList: Item[],
  func: (nextItem: Item, currentResult: Ok) => Result<Ok, Error>,
  initialResult: Result<Ok, Error>
): Result<Ok, Error> =>
  itemList.reduce(
    (currentResult: Result<Ok, Error>, nextItem: Item) =>
      andThen((res: Ok) => func(nextItem, res), currentResult),
    initialResult
  );

// NOTE: re-export all true-myth types to reduce boilerplate
export { Result, and, or, ok, err, andThen };
