import {syntaxTree} from "@codemirror/language"
import {linter, Diagnostic} from "@codemirror/lint"
// import {useCallback} from "react"


export const useLint = linter(view => {
  let diagnostics: Diagnostic[] = []
  syntaxTree(view.state).cursor().iterate(node => {
    // console.log(`${node.name}, From: ${node.from}, To: ${node.to}`)
    if (node.name === "Increment") diagnostics.push({
      from: node.from,
      to: node.to,
      severity: "warning",
      message: "Increment is FORBIDDEN",
      actions: [{
        name: "Remove",
        apply(view, from, to) { view.dispatch({changes: {from, to}}) }
      }]
    })
  })
  return diagnostics
})