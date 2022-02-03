import { PenroseError, PenroseState } from "@penrose/core";
import React from "react";
import { tryDomainHighlight } from "../Util";

export interface AuthorshipInfo {
  name: string;
  madeBy: string | null;
  gistID: string | null;
  avatar: string | null;
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
  settings: ISettings;
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
    settings: { githubUser: null, vimMode: false },
  };
};

export type Action = { kind: "CHANGE_SETTINGS"; settings: any };

export type Dispatcher = React.Dispatch<Action>;

const reducer = (state: State, action: Action): State => {
  switch (action.kind) {
    case "CHANGE_SETTINGS":
      return {
        ...state,
        settings: { ...state.settings, ...action.settings },
      };
  }
};

export default reducer;
