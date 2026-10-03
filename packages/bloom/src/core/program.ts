/**
 * Reusable Domain, Substance, and Style programs for Bloom.
 *
 * Domains contain declarations, substances contain immutable mathematical facts,
 * and styles contain synchronous callbacks. Every `diagram` assembly creates a
 * fresh builder and fresh style views; a substance can safely be drawn repeatedly
 * with different styles, including while another diagram is building.
 */
import type { Canvas } from "@penrose/core";
import {
  DiagramBuilder,
  getActiveBuilder,
  setActiveBuilder,
  withBuilder,
  type InteractiveLayoutOptions,
} from "./builder.js";
import type { Diagram } from "./diagram.js";
import { canvas as makeCanvas } from "./utils.js";

const entityBrand: unique symbol = Symbol("Bloom program entity");
const propositionBrand: unique symbol = Symbol("Bloom program proposition");

export interface ProgramEntity {
  readonly label: string;
  readonly [entityBrand]: {
    readonly domain: string;
    readonly types: Readonly<Record<string, true>>;
  };
}

/** A typed, builder-independent declaration of a mathematical object. */
export interface TypeDeclaration<E extends ProgramEntity = ProgramEntity> {
  readonly kind: "type";
  readonly name: string;
  readonly domainId: symbol;
  readonly supertypes: readonly TypeDeclaration[];
  /** Type information only; never read at runtime. */
  readonly entityType?: E;
}

/** Add typed mathematical data while retaining inferred subtype information. */
export interface DataTypeDeclaration<E extends ProgramEntity = ProgramEntity>
  extends TypeDeclaration<E> {
  withData<F extends object>(): DataTypeDeclaration<EntityWithData<E, F>>;
}

/** Named so generated domain declarations need not expose the private brand. */
export type EntityWithData<E extends ProgramEntity, F extends object> = E &
  DeepReadonly<F>;

export type EntityOf<T extends TypeDeclaration> = NonNullable<T["entityType"]>;

/** Use this marker in a predicate signature to accept another proposition. */
export const proposition = Object.freeze({ kind: "proposition" as const });
export type ArgumentDeclaration = TypeDeclaration | typeof proposition;
export type ProgramArgument = ProgramEntity | Proposition;
export type PredicateArguments<S extends readonly ArgumentDeclaration[]> = {
  -readonly [K in keyof S]: S[K] extends TypeDeclaration<infer E>
    ? E
    : Proposition;
};

export interface PredicateDeclaration<
  A extends readonly ProgramArgument[] = readonly ProgramArgument[],
> {
  readonly kind: "predicate";
  readonly name: string;
  readonly domainId: symbol;
  readonly signature: readonly ArgumentDeclaration[];
  /** Type information only; never read at runtime. */
  readonly argumentTypes?: A;
}

export type ArgumentsOf<P extends PredicateDeclaration> = NonNullable<
  P["argumentTypes"]
>;

/** An asserted proposition; arguments may themselves be propositions. */
export interface Proposition {
  readonly predicate: PredicateDeclaration;
  readonly args: readonly ProgramArgument[];
  readonly [propositionBrand]: true;
}

type Declaration = TypeDeclaration | PredicateDeclaration;
type Definitions = Record<string, Declaration>;
type TypeNames<T extends TypeDeclaration> = T extends TypeDeclaration<infer E>
  ? keyof E[typeof entityBrand]["types"]
  : never;
type ParentNames<P extends readonly TypeDeclaration[]> = TypeNames<P[number]>;
type ParentFields<P extends readonly TypeDeclaration[]> = P extends readonly [
  infer H extends TypeDeclaration,
  ...infer T extends TypeDeclaration[],
]
  ? Omit<EntityOf<H>, typeof entityBrand> & ParentFields<T>
  : object;
// Semantic references already are immutable nominal objects. Preserve their
// named types and identities instead of expanding their private brands.
type DeepReadonly<T> = T extends ProgramEntity | Proposition
  ? T
  : T extends object
  ? { readonly [K in keyof T]: DeepReadonly<T[K]> }
  : T;
export type DeclaredEntity<
  D extends string,
  N extends string,
  F extends object,
  P extends readonly TypeDeclaration[],
> = DeepReadonly<F & ParentFields<P>> & {
  readonly label: string;
  readonly [entityBrand]: {
    readonly domain: D;
    readonly types: Readonly<Record<N | ParentNames<P>, true>>;
  };
};
type EntityInit<E extends ProgramEntity> = Omit<
  E,
  typeof entityBrand | "label"
> & {
  label?: string;
};
type EntityConstructor<E extends ProgramEntity> = object extends EntityInit<E>
  ? (fields?: EntityInit<E>) => E
  : (fields: EntityInit<E>) => E;
export type PredicateAssertion<A extends readonly ProgramArgument[]> = ((
  ...args: A
) => Proposition) & {
  test: (...args: A) => boolean;
  /** Construct a nested proposition without asserting it as a top-level fact. */
  expression: (...args: A) => Proposition;
};
type SubstanceBindings<D extends Definitions> = {
  readonly [K in keyof D]: D[K] extends TypeDeclaration<infer E>
    ? EntityConstructor<E>
    : D[K] extends PredicateDeclaration<infer A>
    ? PredicateAssertion<A>
    : never;
};
export type SubstanceProgramBuilder<D extends Definitions> =
  SubstanceBindings<D> & { make(): SubstanceProgram<D> };
type Assignment<V extends Record<string, TypeDeclaration>> = {
  [K in keyof V]: EntityOf<V[K]>;
};

type EntityInfo = { owner: symbol; type: TypeDeclaration };
const entityInfo = new WeakMap<ProgramEntity, EntityInfo>();
const propositionOwners = new WeakMap<Proposition, symbol>();
const substanceOwners = new WeakMap<object, symbol>();

const isSubtype = (
  actual: TypeDeclaration,
  expected: TypeDeclaration,
): boolean =>
  actual === expected || actual.supertypes.some((p) => isSubtype(p, expected));

/** Copy and freeze factual metadata, so caller mutations cannot change a program. */
const copyFact = (
  value: unknown,
  owner: symbol,
  ancestors = new Set<object>(),
): unknown => {
  if (value === null || typeof value !== "object") {
    if (["function", "symbol", "undefined"].includes(typeof value)) {
      throw new Error("Substance metadata must contain plain data values");
    }
    return value;
  }
  if (ancestors.has(value))
    throw new Error("Substance metadata must be acyclic");
  const semanticOwner =
    entityInfo.get(value as ProgramEntity)?.owner ??
    propositionOwners.get(value as Proposition);
  if (semanticOwner !== undefined) {
    if (semanticOwner !== owner) {
      throw new Error(
        "Substance metadata cannot reference another substance's objects",
      );
    }
    return value;
  }
  if (
    !Array.isArray(value) &&
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error("Substance metadata must contain plain objects and arrays");
  }
  const next = new Set(ancestors).add(value);
  return Object.freeze(
    Array.isArray(value)
      ? value.map((item) => copyFact(item, owner, next))
      : Object.fromEntries(
          Object.entries(value).map(([key, item]) => [
            key,
            copyFact(item, owner, next),
          ]),
        ),
  );
};

const sameArguments = (
  a: readonly ProgramArgument[],
  b: readonly ProgramArgument[],
) => a.length === b.length && a.every((arg, i) => arg === b[i]);

const assertSynchronousCallback = (callback: (...args: never[]) => unknown) => {
  if (
    ["[object AsyncFunction]", "[object AsyncGeneratorFunction]"].includes(
      Object.prototype.toString.call(callback),
    )
  ) {
    throw new Error("Style callbacks must be synchronous");
  }
};

export interface SubstanceProgram<D extends Definitions = Definitions> {
  readonly domain: DomainProgram<D>;
  readonly entities: readonly ProgramEntity[];
  readonly propositions: readonly Proposition[];
}

export interface StyleOptions {
  readonly canvas?: Canvas;
}

export interface StyleProgram<D extends Definitions = Definitions> {
  readonly domain: DomainProgram<D>;
  readonly apply: (context: ProgramStyleContext<D>) => void;
  readonly canvas?: Canvas;
}

/** A sealed domain, usually exported from a small reusable TypeScript module. */
export interface DomainProgram<D extends Definitions = Definitions> {
  readonly name: string;
  readonly id: symbol;
  readonly definitions: Readonly<D>;
  substance(): SubstanceProgramBuilder<D>;
  style(
    apply: (context: ProgramStyleContext<D>) => void,
    options?: StyleOptions,
  ): StyleProgram<D>;
}

export class DomainProgramBuilder<D extends string> {
  private readonly id = Symbol();
  private readonly declarations = new Map<string, Declaration>();
  private sealed = false;

  constructor(private readonly name: D) {}

  private register = <T extends Declaration>(declaration: T): T => {
    if (this.sealed)
      throw new Error("Cannot declare objects after domain.make()");
    if (!declaration.name || this.declarations.has(declaration.name)) {
      throw new Error(
        `Duplicate or empty declaration name: ${declaration.name}`,
      );
    }
    if (
      [
        "name",
        "id",
        "definitions",
        "substance",
        "style",
        "make",
        "__proto__",
        "constructor",
        "prototype",
      ].includes(declaration.name)
    ) {
      throw new Error(`Reserved declaration name: ${declaration.name}`);
    }
    this.declarations.set(declaration.name, declaration);
    return Object.freeze(declaration);
  };

  /** Names are explicit, and become both TypeScript brands and exported bindings. */
  type = <
    N extends string,
    F extends object = object,
    const P extends readonly TypeDeclaration[] = readonly [],
  >(
    name: N,
    ...supertypes: P
  ): DataTypeDeclaration<DeclaredEntity<D, N, F, P>> => {
    for (const parent of supertypes) this.checkDeclaration(parent);
    const declaration = this.register({
      kind: "type",
      name,
      domainId: this.id,
      supertypes: Object.freeze([...supertypes]),
      // This method refines only the TypeScript type; the declaration identity
      // and its existing domain/subtype relationships remain the same.
      withData: (): TypeDeclaration => declaration,
    });
    return declaration as unknown as DataTypeDeclaration<
      DeclaredEntity<D, N, F, P>
    >;
  };

  predicate = <const S extends readonly ArgumentDeclaration[]>(
    name: string,
    signature: S,
  ): PredicateDeclaration<PredicateArguments<S>> => {
    for (const arg of signature) {
      if (arg !== proposition) this.checkDeclaration(arg as TypeDeclaration);
    }
    return this.register({
      kind: "predicate",
      name,
      domainId: this.id,
      signature: Object.freeze([...signature]),
    }) as PredicateDeclaration<PredicateArguments<S>>;
  };

  private checkDeclaration = (declaration: Declaration) => {
    if (
      declaration.domainId !== this.id ||
      this.declarations.get(declaration.name) !== declaration
    ) {
      throw new Error("Declarations must belong to the same domain");
    }
  };

  make = <T extends Definitions>(
    definitions: T,
  ): DomainProgram<T> & Readonly<T> => {
    if (this.sealed) throw new Error("domain.make() may only be called once");
    for (const [name, declaration] of Object.entries(definitions)) {
      this.checkDeclaration(declaration);
      if (name !== declaration.name) {
        throw new Error(
          `Export name ${name} must match declaration ${declaration.name}`,
        );
      }
    }
    if (Object.keys(definitions).length !== this.declarations.size) {
      throw new Error("domain.make() must export every declaration");
    }
    this.sealed = true;
    const domain: DomainProgram<T> & Readonly<T> = Object.freeze({
      ...definitions,
      name: this.name,
      id: this.id,
      definitions: Object.freeze({ ...definitions }),
      substance: () => makeSubstance(domain),
      style: (
        apply: (context: ProgramStyleContext<T>) => void,
        options: StyleOptions = {},
      ) => Object.freeze({ domain, apply, canvas: options.canvas }),
    });
    return domain;
  };
}

export const domain = <const D extends string>(
  name: D,
): DomainProgramBuilder<D> => new DomainProgramBuilder(name);

const checkArgument = (
  expected: ArgumentDeclaration,
  argument: ProgramArgument,
  owner: symbol,
) => {
  if (expected === proposition) {
    if (propositionOwners.get(argument as Proposition) !== owner) {
      throw new Error("Proposition arguments must belong to this substance");
    }
  } else {
    const info = entityInfo.get(argument as ProgramEntity);
    if (!info || info.owner !== owner) {
      throw new Error("Entity arguments must belong to this substance");
    }
    if (!isSubtype(info.type, expected as TypeDeclaration)) {
      throw new Error(
        `Expected ${(expected as TypeDeclaration).name}, got ${info.type.name}`,
      );
    }
  }
};

const makeSubstance = <D extends Definitions>(
  domain: DomainProgram<D>,
): SubstanceProgramBuilder<D> => {
  const owner = Symbol();
  const entities: ProgramEntity[] = [];
  const propositions: Proposition[] = [];
  const expressions: Proposition[] = [];
  let sealed = false;
  const checkOpen = () => {
    if (sealed) throw new Error("Cannot add facts after substance.make()");
  };
  const bindings: Record<string, unknown> = {};
  for (const [name, declaration] of Object.entries(domain.definitions)) {
    if (declaration.kind === "type") {
      bindings[name] = (fields: Record<string, unknown> = {}) => {
        checkOpen();
        if (fields.label !== undefined && typeof fields.label !== "string") {
          throw new Error("Entity labels must be strings");
        }
        const types: Record<string, true> = {};
        const visit = (type: TypeDeclaration) => {
          types[type.name] = true;
          type.supertypes.forEach(visit);
        };
        visit(declaration);
        const entity = Object.freeze({
          ...(copyFact(fields, owner) as object),
          label: fields.label ?? `${name}${entities.length + 1}`,
          [entityBrand]: Object.freeze({
            domain: domain.name,
            types: Object.freeze(types),
          }),
        }) as ProgramEntity;
        entityInfo.set(entity, { owner, type: declaration });
        entities.push(entity);
        return entity;
      };
    } else {
      const validate = (args: readonly ProgramArgument[]) => {
        if (args.length !== declaration.signature.length) {
          throw new Error(
            `${name} expects ${declaration.signature.length} arguments`,
          );
        }
        args.forEach((arg, i) =>
          checkArgument(declaration.signature[i], arg, owner),
        );
      };
      const find = (args: readonly ProgramArgument[]) =>
        propositions.find(
          (p) => p.predicate === declaration && sameArguments(p.args, args),
        );
      const expression = (...args: readonly ProgramArgument[]): Proposition => {
        checkOpen();
        validate(args);
        const existing = expressions.find(
          (p) => p.predicate === declaration && sameArguments(p.args, args),
        );
        if (existing) return existing;
        const fact: Proposition = Object.freeze({
          predicate: declaration,
          args: Object.freeze([...args]),
          [propositionBrand]: true as const,
        });
        propositionOwners.set(fact, owner);
        expressions.push(fact);
        return fact;
      };
      const assertion = ((...args: readonly ProgramArgument[]) => {
        const fact = expression(...args);
        if (!find(args)) propositions.push(fact);
        return fact;
      }) as PredicateAssertion<readonly ProgramArgument[]>;
      assertion.expression = expression;
      assertion.test = (...args) => {
        validate(args);
        return find(args) !== undefined;
      };
      bindings[name] = Object.freeze(assertion);
    }
  }
  bindings.make = () => {
    checkOpen();
    sealed = true;
    const program: SubstanceProgram<D> = Object.freeze({
      domain,
      entities: Object.freeze([...entities]),
      propositions: Object.freeze([...propositions]),
    });
    substanceOwners.set(program, owner);
    return program;
  };
  return Object.freeze(bindings) as SubstanceProgramBuilder<D>;
};

/** Visual data associated with entities in this particular style assembly. */
export interface StyleView<E extends ProgramEntity, V> {
  get(entity: E): V;
  has(entity: E): boolean;
}

export class ProgramStyleContext<D extends Definitions> {
  readonly ensure: DiagramBuilder["ensure"];
  readonly encourage: DiagramBuilder["encourage"];
  readonly layer: DiagramBuilder["layer"];
  readonly input: DiagramBuilder["input"];
  readonly circle: DiagramBuilder["circle"];
  readonly ellipse: DiagramBuilder["ellipse"];
  readonly rectangle: DiagramBuilder["rectangle"];
  readonly line: DiagramBuilder["line"];
  readonly path: DiagramBuilder["path"];
  readonly polygon: DiagramBuilder["polygon"];
  readonly polyline: DiagramBuilder["polyline"];
  readonly text: DiagramBuilder["text"];
  readonly equation: DiagramBuilder["equation"];
  readonly group: DiagramBuilder["group"];
  readonly image: DiagramBuilder["image"];

  constructor(
    readonly builder: DiagramBuilder,
    readonly substance: SubstanceProgram<D>,
  ) {
    this.ensure = builder.ensure;
    this.encourage = builder.encourage;
    this.layer = builder.layer;
    this.input = builder.input;
    this.circle = builder.circle;
    this.ellipse = builder.ellipse;
    this.rectangle = builder.rectangle;
    this.line = builder.line;
    this.path = builder.path;
    this.polygon = builder.polygon;
    this.polyline = builder.polyline;
    this.text = builder.text;
    this.equation = builder.equation;
    this.group = builder.group;
    this.image = builder.image;
  }

  private checkDeclaration = (declaration: Declaration) => {
    if (
      declaration.domainId !== this.substance.domain.id ||
      this.substance.domain.definitions[declaration.name] !== declaration
    ) {
      throw new Error("Style declarations must belong to the substance domain");
    }
  };

  entities = <T extends TypeDeclaration>(type: T): readonly EntityOf<T>[] => {
    this.checkDeclaration(type);
    return Object.freeze(
      this.substance.entities.filter((e) =>
        isSubtype(entityInfo.get(e)!.type, type),
      ),
    ) as readonly EntityOf<T>[];
  };

  /** Iterates recorded tuples directly, including reflexive relationships. */
  facts = <P extends PredicateDeclaration>(
    predicate: P,
  ): readonly ArgumentsOf<P>[] => {
    this.checkDeclaration(predicate);
    return Object.freeze(
      this.substance.propositions
        .filter((p) => p.predicate === predicate)
        .map((p) => p.args),
    ) as readonly ArgumentsOf<P>[];
  };

  test = <P extends PredicateDeclaration>(
    predicate: P,
    ...args: ArgumentsOf<P>
  ): boolean => {
    this.checkDeclaration(predicate);
    if (args.length !== predicate.signature.length) {
      throw new Error(
        `${predicate.name} expects ${predicate.signature.length} arguments`,
      );
    }
    const owner = substanceOwners.get(this.substance)!;
    args.forEach((arg, i) => checkArgument(predicate.signature[i], arg, owner));
    return this.substance.propositions.some(
      (p) => p.predicate === predicate && sameArguments(p.args, args),
    );
  };

  /** Ordered, distinct bindings. Use `facts` when a relation can be reflexive. */
  forall = <V extends Record<string, TypeDeclaration>>(
    vars: V,
    apply: (assignment: Assignment<V>, matchId: number) => void,
  ): void => this.forallWhere(vars, () => true, apply);

  forallWhere = <V extends Record<string, TypeDeclaration>>(
    vars: V,
    where: (assignment: Assignment<V>) => boolean,
    apply: (assignment: Assignment<V>, matchId: number) => void,
  ): void => {
    assertSynchronousCallback(where);
    assertSynchronousCallback(apply);
    const entries = Object.entries(vars).map(
      ([name, type]) => [name, this.entities(type)] as const,
    );
    let matchId = 0;
    const visit = (index: number, assigned: Record<string, ProgramEntity>) => {
      if (index === entries.length) {
        const assignment = Object.freeze({ ...assigned }) as Assignment<V>;
        if (withBuilder(this.builder, () => where(assignment))) {
          withBuilder(this.builder, () => apply(assignment, matchId++));
        }
        return;
      }
      const [name, entities] = entries[index];
      for (const entity of entities) {
        if (!Object.values(assigned).includes(entity)) {
          visit(index + 1, { ...assigned, [name]: entity });
        }
      }
    };
    visit(0, {});
  };

  view = <T extends TypeDeclaration, V>(
    type: T,
    create: (entity: EntityOf<T>) => V,
  ): StyleView<EntityOf<T>, V> => {
    assertSynchronousCallback(create);
    const values = new Map(
      this.entities(type).map((entity) => [
        entity,
        withBuilder<unknown>(this.builder, () => create(entity)) as V,
      ]),
    );
    return Object.freeze({
      get: (entity: EntityOf<T>) => {
        if (!values.has(entity))
          throw new Error(`Entity has no ${type.name} style view`);
        return values.get(entity)!;
      },
      has: (entity: EntityOf<T>) => values.has(entity),
    });
  };
}

export interface FigureRenderOptions {
  readonly variation?: string;
  readonly interactive?: boolean | InteractiveLayoutOptions;
}

export interface DiagramProgramOptions<D extends Definitions>
  extends FigureRenderOptions {
  readonly sub: SubstanceProgram<D>;
  readonly sty: StyleProgram<D> | readonly StyleProgram<D>[];
  readonly canvas?: Canvas;
}

/** Compose synchronous styles in order, and compile through Bloom's optimizer. */
export const diagram = async <D extends Definitions>(
  options: DiagramProgramOptions<D>,
): Promise<Diagram> => {
  if (!substanceOwners.has(options.sub)) {
    throw new Error(
      "diagram requires a substance created by domain.substance().make()",
    );
  }
  const styles: readonly StyleProgram<D>[] = Array.isArray(options.sty)
    ? options.sty
    : [options.sty as StyleProgram<D>];
  for (const style of styles) {
    if (style.domain !== options.sub.domain) {
      throw new Error("Substance and styles must belong to the same domain");
    }
  }
  const declaredCanvases = styles
    .map((s) => s.canvas)
    .filter((c): c is Canvas => c !== undefined);
  if (
    !options.canvas &&
    declaredCanvases.some(
      (c) =>
        c.width !== declaredCanvases[0].width ||
        c.height !== declaredCanvases[0].height,
    )
  ) {
    throw new Error(
      "Composed style canvases disagree; pass an explicit diagram canvas",
    );
  }
  const previous = getActiveBuilder();
  let builder: DiagramBuilder;
  try {
    builder = new DiagramBuilder(
      options.canvas ?? declaredCanvases[0] ?? makeCanvas(800, 600),
      options.variation ?? "",
      options.interactive ? 1000 : 0,
    );
    const context = new ProgramStyleContext(builder, options.sub);
    for (const style of styles) {
      assertSynchronousCallback(style.apply);
      withBuilder(builder, () => style.apply(context));
    }
    if (options.interactive)
      builder.interactiveLabels(
        typeof options.interactive === "object" ? options.interactive : {},
      );
  } finally {
    setActiveBuilder(previous);
  }
  return builder.build();
};
