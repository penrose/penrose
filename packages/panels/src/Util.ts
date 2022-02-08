import { Octokit } from "@octokit/rest";
import { compileDomain } from "@penrose/core";
import { editor } from "monaco-editor";
import { useCallback } from "react";
import { toast } from "react-toastify";
import dummyRegistry from "./dummy-registry.json";
import { Action, Dispatcher, State } from "./reducer";

/**
 * (browser-only) Downloads any given exported SVG to the user's computer
 * @param svg
 * @param title the filename
 */
export const DownloadSVG = (
  svg: SVGSVGElement,
  title = "illustration"
): void => {
  const blob = new Blob([svg.outerHTML], {
    type: "image/svg+xml;charset=utf-8",
  });
  const url = URL.createObjectURL(blob);
  const downloadLink = document.createElement("a");
  downloadLink.href = url;
  downloadLink.download = `${title}.svg`;
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
};

export const fetchTrioPreset = async (dispatch: React.Dispatch<Action>) => {
  const domainReq = await fetch(
    `${dummyRegistry.root}${dummyRegistry.domains["set-theory"].URI}`
  );
  const domain = await domainReq.text();
  dispatch({ kind: "CHANGE_CODE", lang: "dsl", content: domain });
  const styReq = await fetch(
    `${dummyRegistry.root}${dummyRegistry.styles["venn"].URI}`
  );
  const sty = await styReq.text();
  dispatch({ kind: "CHANGE_CODE", lang: "sty", content: sty });
  const subReq = await fetch(
    `${dummyRegistry.root}${dummyRegistry.substances["nested"].URI}`
  );
  const sub = await subReq.text();
  dispatch({ kind: "CHANGE_CODE", lang: "sub", content: sub });
};

export const usePublishGist = (
  state: State,
  dispatch: React.Dispatch<Action>
) => {
  return useCallback(() => {
    const { githubUser } = state.settings;
    if (githubUser) {
      const octokit = new Octokit({
        auth: githubUser.access_token,
      });
      const gistData = {
        public: true,
        description: `${state.currentInstance.authorship.name}: a trio from https://penrose.ink`,
        files: {
          ".sub": { content: state.currentInstance.sub },
          ".sty": { content: state.currentInstance.sty },
          ".dsl": { content: state.currentInstance.dsl },
          "metadata.json": {
            content: JSON.stringify({
              authorship: {
                madeBy: githubUser.username,
                name: state.currentInstance.authorship.name,
                avatar: githubUser.avatar,
              },
            }),
          },
        },
      };
      (async () => {
        try {
          toast.info("Publishing gist...");
          const res = await octokit.gists.create(gistData);
          if (res.status === 201) {
            toast.success("Gist published, redirecting...");
            window.location.replace(`/gist/${res.data.id}`);
          }
        } catch (err) {
          toast.error((err as string).toString());
        }
      })();
    }
  }, [state]);
};

export const retrieveGist = async (gistId: string, dispatch: Dispatcher) => {
  const octokit = new Octokit();
  try {
    const { data } = await octokit.gists.get({ gist_id: gistId });
    const { files } = data;
    if (files !== undefined) {
      const sub = files[".sub"]!.content as string;
      const sty = files[".sty"]!.content as string;
      const dsl = files[".dsl"]!.content as string;
      const metadata = JSON.parse(files["metadata.json"]!.content as string);
      dispatch({ kind: "SET_TRIO", sub, sty, dsl });
      dispatch({
        kind: "SET_AUTHORSHIP",
        authorship: { ...metadata.authorship, gistID: gistId },
      });
      tryDomainHighlight(dsl, dispatch);
    } else {
      toast.error("No files in gist");
    }
  } catch (err) {
    toast.error(`Could not retrieve gist ID: ${(err as string).toString()}`);
  }
};

export const tryDomainHighlight = (
  dsl: string,
  dispatch?: Dispatcher
): any | undefined => {
  const domainComp = compileDomain(dsl);
  if (domainComp.isOk()) {
    dispatch &&
      dispatch({ kind: "SET_DOMAIN_CACHE", domainCache: domainComp.value });
    return domainComp.value || undefined;
  }
};

export const monacoOptions: editor.IStandaloneEditorConstructionOptions = {
  automaticLayout: true,
  minimap: { enabled: false },
  wordWrap: "on",
  fontSize: 16,
};
