import { JSXElement } from "solid-js";

export type PathResolver = (path: string) => Promise<string | undefined>;

export const join = (dir: string, path: string): string => {
  if (path.startsWith("/")) return path;
  const parts = dir.split("/");
  for (const part of path.split("/")) {
    if (part === "..") parts.pop();
    else if (part !== ".") parts.push(part);
  }
  return parts.join("/");
};

export interface Style {
  contents: string;
  resolver: PathResolver;
}

export interface Trio {
  substance: string;
  style: Style[];
  domain: string;
  variation: string;
}

export interface BaseMeta {
  name?: string;
}

export interface TrioMeta extends BaseMeta {
  kind: "trio";
  get: () => Promise<Trio>;
  gallery?: boolean;
}

export interface SolidMeta extends BaseMeta {
  kind: "solid";
  f: () => JSXElement;
}

export type Meta = TrioMeta | SolidMeta;
