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
    case "NotTypeConsInSubtype": {
      if (error.type.tag === "Prop")
        return `Prop (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed in subtyping relations.`;
      else {
        return `${error.type.name.value} (at ${loc(
          error.type
        )}) is not a type constructor. Only type constructors are allowed in subtyping relations.`;
      }
    }
    case "CyclicSubtypes": {
      return `Subtyping relations in this program form a cycle. Cycles of types are:\n${showCycles(
        error.cycles
      )}`;
    }
  }
};

const showCycles = (cycles: string[][]) => {
  const pathString = (path: string[]) => path.join(" -> ");
  return cycles.map(pathString).join("\n");
};

export const cyclicSubtypes = (cycles: string[][]): CyclicSubtypes => ({
  tag: "CyclicSubtypes",
  cycles,
});

export const notTypeConsInSubtype = (
  type: Prop | TypeVar
): NotTypeConsInSubtype => ({
  tag: "NotTypeConsInSubtype",
  type,
});

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
