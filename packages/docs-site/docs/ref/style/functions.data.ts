import {
  CompFunc,
  compDict,
  constrDict,
  describeType,
  objDict,
} from "@penrose/core";
import { ConstrFunc, ObjFunc } from "@penrose/core/dist/types/functions";
import Markdown from "markdown-it";
import markdownItKatex from "markdown-it-katex";
const md = new Markdown();
md.use(markdownItKatex);

const renderDict = (dict) => {
  const renderFn = (f: CompFunc | ObjFunc | ConstrFunc) => ({
    name: f.name,
    description: md.render(f.description ?? ""),
    returns: "returns" in f ? describeType(f.returns) : undefined,
    params: f.params.map((p: any) => ({
      ...p,
      type: describeType(p.type),
      description: p.description ? md.render(p.description) : "",
    })),
  });
  return Object.fromEntries(
    Object.entries(dict).map(([k, v]) => [k, renderFn(v)]),
  );
};

export default {
  load() {
    return {
      constrDict: renderDict(constrDict),
      objDict: renderDict(objDict),
      compDict: renderDict(compDict),
    };
  },
};
