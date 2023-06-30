import{s as e,r as n,d as a}from"./timeline.style-d4c12eb8.js";import"./resolver-9b16ebb5.js";import"./iframe-7b8a5a1d.js";const t=`Task e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13

Year y2023, y2024, y2025

Quarter q1_2023, q2_2023, q3_2023, q4_2023, q1_2024, q2_2024, q3_2024, q4_2024, q1_2025, q2_2025

q1_2023 := MkQuarter(y2023)
q2_2023 := MkQuarter(y2023)
q3_2023 := MkQuarter(y2023)
q4_2023 := MkQuarter(y2023)

q1_2024 := MkQuarter(y2024)
q2_2024 := MkQuarter(y2024)
q3_2024 := MkQuarter(y2024)
q4_2024 := MkQuarter(y2024)

q1_2025 := MkQuarter(y2025)
q2_2025 := MkQuarter(y2025)

Category outcome, access, milestone

Before(outcome, access)
Before(access, milestone)

e1 := MkTask(q1_2023, q2_2023)
In(e1, outcome)
Label e1 "Web IDE"
e2 := MkTask(q1_2023, q2_2025)
In(e2, milestone)
Label e2 "Enhanced Graphical Features"
e3 := MkTask(q4_2023, q3_2024)
In(e3, access)
Label e3 "Color Accuity Optimization"
e4 := MkTask(q3_2023, q1_2025)
In(e4, outcome)
Label e4 "Integrations: Powerpoint, Canvas, LaTeX, Google Docs, etc."
e5 := MkTask(q4_2023, q4_2024)
In(e5, outcome)
Label e5 "Animation and Progressively-built diagrams"
e6 := MkTask(q1_2023, q1_2024)
In(e6, outcome)
Label e6 "Standard Library that spans DOE Math Curriculum"

e7 := MkTask(q2_2023, q3_2023)
In(e7, access)
Label e7 "Language localization"
e8 := MkTask(q3_2023, q4_2023)
In(e8, access)
Label e8 "Spatially localize alt text"
e9 := MkTask(q1_2024, q2_2025)
In(e9, access)
Label e9 "Semantics-preserving, layout-optimized zoom"

e10 := MkTask(q1_2023, q3_2023)
In(e10, milestone)
Label e10 "Improve Compilation"
e11 := MkTask(q4_2023, q2_2025)
In(e11, milestone)
Label e11 "Interactivity"
e12 := MkTask(q3_2023, q1_2024)
In(e12, milestone)
Label e12 "Temporal specification"
e13 := MkTask(q4_2023, q4_2024)
In(e13, milestone)
Label e13 "Staged optimization"

Label q1_2023 "Q1"
Label q2_2023 "Q2"
Label q3_2023 "Q3"
Label q4_2023 "Q4"

Label q1_2024 "Q1"
Label q2_2024 "Q2"
Label q3_2024 "Q3"
Label q4_2024 "Q4"

Label q1_2025 "Q1"
Label q2_2025 "Q2"

Label y2023 "2023"
Label y2024 "2024"
Label y2025 "2025"`,l={substance:t,style:[{contents:e,resolver:n}],domain:a,variation:"ThumperMandrill547",excludeWarnings:[]};export{l as default};
//# sourceMappingURL=penrose.trio-4d48b36e.js.map
