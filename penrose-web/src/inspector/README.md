# Inspector

The inspector is a resizable pane in the "vanilla" Penrose renderer that provides *views* on the `State` of a Penrose translation.

You can think of each inspector view as a variation on the original Canvas svg renderer; both have access to the same data but present it in different ways.

Each view in the inspector is indexed in `views/viewMap.tsx`. The key is the name that appears as a tab in the pane and the value is the component class of that view. Every view has access to the same data and events defined in `views/IViewProps.tsx`.

## Data

The inspector lets you see the `history` of each `frame` of state during optimization so that you can see the optimization dynamics play out over time.

The `../App.tsx` is the source of truth for the history. Every new state is appended to the history  immutably.

The inspector has an internal state to track which `frame` has been "selected". This is so you can go back in time and see prior `frame`s in `history` if you want. This is tracked by index; but there is a second possible value for the current `frame`: `-1`. The value `-1` corresponds to "most recent `frame` in history", so you can see the *current* frame all the time, dynamically updating. In the timeline, clicking a frame and clicking it again corresponds to whether you're tracking the most recent frame or clamping to the selected frame (blue outline in the thumbnail).

This interaction model is handy but annoying to deal with when you're implementing a new view. Thus the prop `frame` is provided which always gives the frame that should be inspected in the view, and `null` if none such exist (if the history is empty). You should handle both states with separate empty and populated views, as seen in existing views.


## Adding a view

Make a new component that accepts `IViewProps`, and then put it in `views/viewMap.tsx`.

A useful library component is `import { ObjectInspector } from "react-inspector";`, which gives a chrome-style JSON tree you can inspect.

See `ShapeView`, `Timeline`, and `Frames` for useful examples.

## Unimplemented behavior

- [ ] Make scrolling not terrible (the bottom is currently cut off unless your inspector is tall enough, due to padding and box model and overflow: hidden).
- [ ] Sync current selected frame and the frame being shown on the canvas
- [ ] Provide an absolute-positioned overlay interface on top of canvas that highlights shape being inspected (needs to pass along pointer-events so interactions are still possible beneath). Google chrome inspector style