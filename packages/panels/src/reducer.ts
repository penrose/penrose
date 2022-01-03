import { PenroseError, PenroseState } from "@penrose/core";
import { cloneDeep, debounce } from "lodash";
import React from "react";
import { tryDomainHighlight } from "./Util";

export interface PaneState {
  sub: boolean;
  sty: boolean;
  dsl: boolean;
  preview: boolean;
}

export interface AuthorshipInfo {
  name: string;
  madeBy: string | null;
  gistID: string | null;
  avatar: string | null;
}

export interface CurrentInstance {
  authorship: AuthorshipInfo;
  sub: string;
  sty: string;
  dsl: string;
  state: PenroseState | null;
  /* For syntax highlighting */
  domainCache: any | null;
  err: PenroseError | null;
}
export interface GithubUser {
  username: string;
  access_token: string;
  avatar: string;
}
export interface ISettings {
  githubUser: GithubUser | null;
  vimMode: boolean;
}

export interface State {
  openPanes: PaneState;
  settings: ISettings;
  currentInstance: CurrentInstance;
}

export const initialState = (): State => {
  const fromStorage = window.localStorage.getItem("state");
  if (fromStorage !== null) {
    const saved = JSON.parse(fromStorage);
    const domainCache = tryDomainHighlight(saved.currentInstance.dsl);
    return {
      ...saved,
      currentInstance: { ...saved.currentInstance, domainCache },
    };
  }
  return {
    openPanes: { sub: true, sty: false, dsl: false, preview: true },
    currentInstance: {
      sub: "",
      sty: "",
      dsl: "",
      state: null,
      err: null,
      domainCache: null,
      authorship: {
        name: "untitled",
        madeBy: null,
        avatar: null,
        gistID: null,
      },
    },
    settings: { githubUser: null, vimMode: false },
  };
};

export const debouncedSave = debounce((state: State) => {
  if (state.currentInstance.authorship.gistID !== null) {
    // don't save if already gist
    return;
  }
  const modifiedState = cloneDeep(state);
  modifiedState.currentInstance.state = null;
  modifiedState.currentInstance.domainCache = null;
  modifiedState.currentInstance.err = null;

  window.localStorage.setItem("state", JSON.stringify(modifiedState));
}, 250);

export type Action =
  | { kind: "TOGGLE_SUB_PANE" }
  | { kind: "TOGGLE_STY_PANE" }
  | { kind: "TOGGLE_DSL_PANE" }
  | { kind: "TOGGLE_PREVIEW_PANE" }
  | { kind: "CHANGE_CODE"; lang: "sub" | "sty" | "dsl"; content: string }
  | { kind: "SET_TRIO"; sub: string; sty: string; dsl: string }
  | { kind: "SET_DOMAIN_CACHE"; domainCache: any | null }
  | { kind: "SET_AUTHORSHIP"; authorship: AuthorshipInfo }
  | { kind: "CHANGE_CANVAS_STATE"; content: PenroseState | null }
  | { kind: "CHANGE_ERROR"; content: PenroseError | null }
  | { kind: "CHANGE_TITLE"; name: string }
  | { kind: "CHANGE_GH_USER"; user: GithubUser };

export type Dispatcher = React.Dispatch<Action>;

const reducer = (state: State, action: Action): State => {
  switch (action.kind) {
    case "TOGGLE_DSL_PANE":
      return {
        ...state,
        openPanes: { ...state.openPanes, dsl: !state.openPanes.dsl },
      };
    case "TOGGLE_PREVIEW_PANE":
      return {
        ...state,
        openPanes: { ...state.openPanes, preview: !state.openPanes.preview },
      };
    case "TOGGLE_STY_PANE":
      return {
        ...state,
        openPanes: { ...state.openPanes, sty: !state.openPanes.sty },
      };
    case "TOGGLE_SUB_PANE":
      return {
        ...state,
        openPanes: { ...state.openPanes, sub: !state.openPanes.sub },
      };
    case "CHANGE_CODE":
      return {
        ...state,
        currentInstance: {
          ...state.currentInstance,
          [action.lang]: action.content,
          authorship: {
            ...state.currentInstance.authorship,
            madeBy: state.settings.githubUser
              ? state.settings.githubUser.username
              : null,
            // null out so we can save another
            gistID: null,
          },
        },
      };
    case "SET_TRIO":
      return {
        ...state,
        currentInstance: {
          ...state.currentInstance,
          dsl: action.dsl,
          sub: action.sub,
          sty: action.sty,
        },
      };
    case "SET_AUTHORSHIP":
      return {
        ...state,
        currentInstance: {
          ...state.currentInstance,
          authorship: action.authorship,
        },
      };
    case "CHANGE_CANVAS_STATE":
      return {
        ...state,
        currentInstance: { ...state.currentInstance, state: action.content },
      };
    case "CHANGE_ERROR":
      return {
        ...state,
        currentInstance: { ...state.currentInstance, err: action.content },
      };
    case "SET_DOMAIN_CACHE":
      return {
        ...state,
        currentInstance: {
          ...state.currentInstance,
          domainCache: action.domainCache,
        },
      };
    case "CHANGE_TITLE":
      return {
        ...state,
        currentInstance: {
          ...state.currentInstance,
          authorship: {
            ...state.currentInstance.authorship,
            name: action.name,
          },
        },
      };
    case "CHANGE_GH_USER":
      return {
        ...state,
        settings: { ...state.settings, githubUser: action.user },
      };
  }
};

export default reducer;
