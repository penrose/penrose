export interface GroupData {
  id: string;
  name: string;
  phrase: string;
  order: number;
  elements: string[];
  multTable: number[][];
  generators: number[];
}

// Cyclic group Z_n: elements 0..n-1, product = (i+j) % n
const cyclic = (
  n: number,
  id: string,
  name: string,
  phrase: string,
  labels?: string[],
): GroupData => {
  const elements =
    labels ??
    Array.from({ length: n }, (_, i) =>
      i === 0 ? "e" : i === 1 ? "a" : `a^{${i}}`,
    );
  const multTable = Array.from({ length: n }, (_, i) =>
    Array.from({ length: n }, (_, j) => (i + j) % n),
  );
  return { id, name, phrase, order: n, elements, multTable, generators: [1] };
};

export const GROUPS: GroupData[] = [
  // ── Z_2 ────────────────────────────────────────────────────────────────
  cyclic(2, "Z_2", "ℤ₂", "Cyclic group of order 2"),

  // ── Z_3 ────────────────────────────────────────────────────────────────
  cyclic(3, "Z_3", "ℤ₃", "Cyclic group of order 3"),

  // ── Z_4 ────────────────────────────────────────────────────────────────
  cyclic(4, "Z_4", "ℤ₄", "Cyclic group of order 4"),

  // ── V_4 (Klein four-group) ─────────────────────────────────────────────
  {
    id: "V_4",
    name: "V₄",
    phrase: "Klein four-group",
    order: 4,
    elements: ["e", "a", "b", "ab"],
    multTable: [
      [0, 1, 2, 3],
      [1, 0, 3, 2],
      [2, 3, 0, 1],
      [3, 2, 1, 0],
    ],
    generators: [1, 2],
  },

  // ── Z_5 ────────────────────────────────────────────────────────────────
  cyclic(5, "Z_5", "ℤ₅", "Cyclic group of order 5"),

  // ── Z_6 ────────────────────────────────────────────────────────────────
  cyclic(6, "Z_6", "ℤ₆", "Cyclic group of order 6"),

  // ── S_3 (symmetric group on 3 letters, order 6) ────────────────────────
  // Elements as permutations of {0,1,2}:
  //   0=(), 1=(01), 2=(02), 3=(12), 4=(012), 5=(021)
  // Product: compose(a,b)[x] = perm[a][perm[b][x]]
  {
    id: "S_3",
    name: "S₃",
    phrase: "Symmetric group on 3 letters",
    order: 6,
    elements: ["e", "(12)", "(13)", "(23)", "(123)", "(132)"],
    multTable: [
      [0, 1, 2, 3, 4, 5],
      [1, 0, 5, 4, 3, 2],
      [2, 4, 0, 5, 1, 3],
      [3, 5, 4, 0, 2, 1],
      [4, 2, 3, 1, 5, 0],
      [5, 3, 1, 2, 0, 4],
    ],
    generators: [1, 4],
  },

  // ── Z_8 ────────────────────────────────────────────────────────────────
  cyclic(8, "Z_8", "ℤ₈", "Cyclic group of order 8"),

  // ── D_4 (dihedral group on 4 vertices, order 8) ────────────────────────
  // From nathancarter/group-explorer D_4.group
  // Elements: 0=e, 1=r, 2=r², 3=r³, 4=f, 5=fr, 6=r²f, 7=rf
  {
    id: "D_4",
    name: "D₄",
    phrase: "Dihedral group on four vertices",
    order: 8,
    elements: ["e", "r", "r^{2}", "r^{3}", "f", "fr", "r^{2}f", "rf"],
    multTable: [
      [0, 1, 2, 3, 4, 5, 6, 7],
      [1, 2, 3, 0, 7, 4, 5, 6],
      [2, 3, 0, 1, 6, 7, 4, 5],
      [3, 0, 1, 2, 5, 6, 7, 4],
      [4, 5, 6, 7, 0, 1, 2, 3],
      [5, 6, 7, 4, 3, 0, 1, 2],
      [6, 7, 4, 5, 2, 3, 0, 1],
      [7, 4, 5, 6, 1, 2, 3, 0],
    ],
    generators: [1, 4],
  },

  // ── Q_4 (quaternion group, order 8) ────────────────────────────────────
  // From nathancarter/group-explorer Q_4.group
  // Elements: 0=1, 1=i, 2=-1, 3=-i, 4=j, 5=k, 6=-j, 7=-k
  {
    id: "Q_4",
    name: "Q₄",
    phrase: "Quaternion group",
    order: 8,
    elements: ["1", "i", "-1", "-i", "j", "k", "-j", "-k"],
    multTable: [
      [0, 1, 2, 3, 4, 5, 6, 7],
      [1, 2, 3, 0, 7, 4, 5, 6],
      [2, 3, 0, 1, 6, 7, 4, 5],
      [3, 0, 1, 2, 5, 6, 7, 4],
      [4, 5, 6, 7, 2, 3, 0, 1],
      [5, 6, 7, 4, 1, 2, 3, 0],
      [6, 7, 4, 5, 0, 1, 2, 3],
      [7, 4, 5, 6, 3, 0, 1, 2],
    ],
    generators: [1, 4],
  },

  // ── Z_12 ───────────────────────────────────────────────────────────────
  cyclic(12, "Z_12", "ℤ₁₂", "Cyclic group of order 12"),
];
