/** @jsxImportSource @penrose/bloom */

import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Normal neighborhoods of a proposed split of a directed closed-family intersection. */
export function directedIntersectionStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const family = ctx.entities(topology.ClosedDirectedFamily)[0];
    const assumption = ctx
      .facts(topology.Hypothesis)
      .map(([p]) => p)
      .find((p) => p.predicate === topology.SplitBetween);
    const b =
      assumption &&
      ctx
        .entities(topology.ClosedSubspace)
        .find((s) => s === assumption.args[0]);
    const u =
      assumption &&
      ctx
        .entities(topology.ClopenSubspace)
        .find((s) => s === assumption.args[3]);
    const v =
      assumption &&
      ctx
        .entities(topology.ClopenSubspace)
        .find((s) => s === assumption.args[4]);
    const separation = ctx
      .facts(topology.SetSeparation)
      .find(([left, right]) => left === u && right === v);
    const ai =
      family &&
      ctx
        .entities(topology.ClosedSubspace)
        .find((s) => s !== b && ctx.test(topology.SetInFamily, s, family));
    const selection = ctx
      .entities(topology.Point)
      .find(
        (p) =>
          ai &&
          ctx.test(topology.Member, p, ai) &&
          ctx.facts(topology.Outside).some(([point]) => point === p),
      );
    if (
      !family ||
      !assumption ||
      !b ||
      !u ||
      !v ||
      !separation ||
      !ai ||
      !selection ||
      !ctx.test(topology.IntersectionOfFamily, b, family) ||
      !ctx.test(topology.Compact, separation[4]) ||
      !ctx.test(topology.T2, separation[4])
    )
      throw new Error(
        "Keep the directed-family intersection and its proposed split inside compact Hausdorff hypotheses",
      );
    const [, , g, h] = separation;
    if (
      !ctx.test(topology.Disjoint, g, h) ||
      !ctx.test(topology.Subset, u, g) ||
      !ctx.test(topology.Subset, v, h)
    )
      throw new Error(
        "The normal neighborhoods must separate the two relatively clopen parts",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [x - 342, 1092 - y];
    const tr = (cmds: [string, ...number[]][]) =>
      cmds.map(([c, ...p]): [string, ...number[]] => [
        c,
        ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
      ]);
    draw.outline(
      "intersection.family-member",
      tr([
        ["M", 181, 1050],
        ["C", 205, 1008, 255, 986, 314, 980],
        ["C", 369, 974, 425, 984, 462, 1008],
        ["C", 502, 1035, 517, 1079, 508, 1118],
        ["C", 502, 1160, 460, 1182, 409, 1196],
        ["C", 359, 1208, 279, 1205, 239, 1190],
        ["C", 195, 1173, 164, 1140, 175, 1093],
        ["C", 172, 1077, 175, 1065, 181, 1050],
        ["Z"],
      ]),
    );
    draw.outline(
      "intersection.normal-g",
      tr([
        ["M", 206, 1042],
        ["C", 225, 1014, 277, 1007, 310, 1018],
        ["C", 340, 1028, 344, 1062, 352, 1089],
        ["C", 363, 1124, 343, 1154, 316, 1167],
        ["C", 282, 1180, 239, 1166, 218, 1143],
        ["C", 195, 1118, 183, 1073, 206, 1042],
        ["Z"],
      ]),
    );
    draw.outline(
      "intersection.normal-h",
      tr([
        ["M", 378, 1035],
        ["C", 401, 1015, 450, 1026, 468, 1046],
        ["C", 486, 1067, 491, 1112, 481, 1142],
        ["C", 469, 1170, 420, 1188, 397, 1186],
        ["C", 370, 1187, 363, 1157, 363, 1125],
        ["C", 357, 1088, 361, 1058, 378, 1035],
        ["Z"],
      ]),
    );
    draw.outline(
      "intersection.relative-u",
      tr([
        ["M", 243, 1057],
        ["C", 259, 1040, 276, 1038, 289, 1049],
        ["C", 300, 1061, 313, 1050, 329, 1057],
        ["C", 350, 1067, 348, 1100, 335, 1119],
        ["C", 320, 1140, 283, 1148, 259, 1133],
        ["C", 234, 1118, 216, 1079, 243, 1057],
        ["Z"],
      ]),
    );
    draw.outline(
      "intersection.relative-v",
      tr([
        ["M", 384, 1069],
        ["C", 403, 1047, 438, 1044, 456, 1054],
        ["C", 472, 1066, 475, 1105, 466, 1129],
        ["C", 456, 1154, 415, 1159, 391, 1143],
        ["C", 369, 1129, 365, 1088, 384, 1069],
        ["Z"],
      ]),
    );
    draw.dot("intersection.outside-selection", xy(326, 1177));
    for (const [label, x, y] of [
      [ai.label, 359, 1006],
      [u.label, 285, 1090],
      [v.label, 429, 1090],
      [g.label, 258, 1149],
      [h.label, 411, 1168],
      [b.label, 321, 1119],
      [b.label, 387, 1119],
      [selection.label, 341, 1177],
    ] as const)
      draw.label(label, xy(x, y));
  });
}
