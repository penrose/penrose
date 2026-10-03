# Constraint topology experiments

These new Style programs draw circle/L-shaped set regions, point membership and nested sets, and directed relation/function edges with native Penrose shapes. Every center, circle radius, L width/height and label center is an optimized input. Initial coordinates and weak objectives are Style choices; the immutable Substance contains only mathematical objects and facts. The existing reviewed book reproductions were not changed.

The two `book-*` controls are source-inspired abstract separation/nesting schematics, not full reproductions of a numbered figure or the source's metric/topological hypotheses. The four additional compositions are a triangle across three disjoint neighborhoods inside an ambient set, a many-to-one bipartite map, a relation in an L-shaped set, and intersecting/disjoint sets with a relation using the Style's general defaults without per-instance coordinate hints.

The experiment ran **288 trials**: three revisions, each with seven programs × three seeds × perturbations of 0, 5, 90 and 160 pixels, plus twelve trials with absolute geometry and size priors disabled. Each revision therefore has 84 consistent trials and 12 deliberately contradictory trials. Label objectives relative to live geometry remain when geometry priors are off.

| Revision                                                     | Consistent trials stopped at convergence | Consistent trials feasible at 0.001 | Largest label distance among feasible trials |
| ------------------------------------------------------------ | ---------------------------------------: | ----------------------------------: | -------------------------------------------: |
| before: weak relative label objectives, no association bound |                                    84/84 |                               83/84 |                                   399.040 px |
| after: 24 px Euclidean association bound                     |                                    83/84 |                               77/84 |                                 24.000029 px |
| final: same bound expressed with normalized squared distance |                                    83/84 |                               79/84 |                                 24.000033 px |

All 36 contradictory trials report optimization finished while remaining infeasible. All 36 canonical/5-pixel consistent trials in the final revision are feasible. Final weak-prior cases pass 69/72; cases with geometry priors disabled pass 10/12. The final formulation avoids the square root in the label bound, but it does not eliminate nonconvex local minima or divergence.

| Program                   | Geometry + label inputs | Registered active constraints | Final weak-prior feasible | Native optimization calls | Median build / optimize time |
| ------------------------- | ----------------------: | ----------------------------: | ------------------------: | ------------------------: | ---------------------------: |
| separation control        |                  10 + 8 |                            32 |                     12/12 |                      6–43 |              10.68 / 3.12 ms |
| nesting control           |                  11 + 8 |                            28 |                     12/12 |                      6–91 |               9.62 / 2.87 ms |
| neighborhood triangle     |                 18 + 14 |                           109 |                     11/12 |                    10–121 |             37.23 / 21.33 ms |
| bipartite map             |                 18 + 16 |                           176 |                     12/12 |                      6–97 |             56.80 / 19.78 ms |
| L membership/relation     |                 16 + 12 |                            80 |                     10/12 |                     6–127 |             29.28 / 12.45 ms |
| composition without hints |                 15 + 12 |                            80 |                     12/12 |                    18–172 |             28.15 / 24.63 ms |

Times are local Node v23.11.0 wall times, including ordinary runtime variability; they are not a hardware-independent speed claim. Full data also record native exterior-point rounds, final-round iterations, total/optimized solver input counts, geometry and label displacement from initialization and preferred landmarks, every named residual, independent SVG clearance, exact edge incidence, and crossings. `geometryDriftFromBook` in the raw records means drift from preferred Style landmarks; for the unhinted program those landmarks are generic defaults, not book coordinates. Default sampled shape fields contribute unused or pinned inputs to total state counts; the explicitly optimized geometry/label counts above are more informative.

The Style registers Circle containment, simple-polygon/disk containment, disjointness and circular overlap, along with dot/label/edge clearance, positive region dimensions, relative label association and on-canvas constraints. Native line endpoints share the exact optimized point coordinates: independent incidence deviation is zero throughout finite successful runs. Independent numeric checks read the rendered SVG circles/polygon, use ray casting and segment distances for the L region, and do not reuse the core containment function.

Membership/containment checks include stylistic 8-pixel clearance and 4-pixel point-marker radii. A failed margin does not necessarily mean the abstract point membership is false. These are checks of the intended graphical representation, not a theorem prover. Region shape, notch proportions, minimum size, fill, arrow size, label anchors, visual padding and weak priors are aesthetic choices. Unasserted memberships are unknown; `MapBetween`/`RelationOn` are not general inference rules in this prototype. The examples explicitly record endpoint memberships. Self edges, empty-set self-disjointness and polygon-in-polygon subsets are intentionally rejected rather than approximated silently.

The final failures are retained:

- Neighborhood triangle, cedar/160, converges with 4.748967 px membership-clearance violation and 4.745252 px label/point overlap; its largest label association is 27.799231 px.
- L membership, cedar/90 and cedar/160, converges with 0.703800 and 0.722927 px maximum violation, including label/point clearance and membership margins.
- L membership, cedar/90 with geometry priors disabled, converges with 0.685536 px maximum violation.
- Neighborhood triangle, violet/90 with geometry priors disabled, stops before convergence after 68 calls with finite but divergent geometry (maximum constraint value 2.1260194×10³⁷). The earlier Euclidean-bound revision has a different large-perturbation failure with NaN inputs.

Bipartite arrows still cross: 14 crossings across the twelve final weak-prior trials. Crossings are not prohibited by the mathematical function facts; no planarity or edge-in-region invariant is claimed. Arrows use the shared node view from this bundle. Two independently instantiated bundles do not compose their region/relation views and fail with an explicit message. Same-bundle reuse assembles independent diagrams and is regression-tested. Initialization is deterministic per assembly. Instrumentation arrays are intended for a fresh bundle per measured diagram.

Full records: [before](./topology-before.json), [Euclidean bound](./topology-after.json), [final squared bound](./topology-final.json), and [compact summary](./topology-summary.json). Nonfinite values are preserved as `NaN`/`Infinity` strings instead of silently becoming JSON null. Source hashes in all three matrices match the released leaves.

Representative native SVGs:

- [Composition without hints](./topology-unhinted-canonical.svg)
- [Canonical neighborhood triangle](./topology-triangle-canonical.svg)
- [L layout before label bounds](./topology-l-labels-before.svg) and [the same initialization with final bounds](./topology-l-labels-final.svg)
- [Finite triangle local minimum](./topology-triangle-local-minimum.svg)
- [Bipartite crossing](./topology-bipartite-crossing.svg)
- [Contradictory subset/disjoint facts](./topology-contradiction.svg)

Reproduce after installing workspace dependencies:

```sh
node_modules/.bin/tsc -p packages/core/tsconfig.json
node_modules/.bin/tsc -p packages/bloom/tsconfig.json
node scripts/elementary-topology/experiment-constraint-topology.mjs tmp/elementary-topology/constraint-topology/revisions all
```

The script uses native Penrose optimization with its default settings and writes each raw SVG and measurement JSON to the private output directory. Select `before`, `after` or `final` as the last argument for one 96-trial matrix. Focused checks: `cd packages/bloom && ../../node_modules/.bin/vitest run src/styles/constraint-topology.test.ts` (13 tests).
