import { Action, Dispatcher, State } from "./reducer";
import dummyRegistry from "./dummy-registry.json";
import { useCallback } from "react";
import { toast } from "react-toastify";
import { compileDomain } from "@penrose/core";
import { useEffect } from "react";

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
          const res = await fetch("https://api.github.com/gists", {
            method: "POST",
            headers: {
              accept: "application/vnd.github.v3+json",
              "X-OAuth-Scopes": "gist",
              "X-Accepted-OAuth-Scopes": "gist",
              Authorization: `token ${githubUser.access_token}`,
            },
            body: JSON.stringify(gistData),
          });
          const json = await res.json();
          if (res.status === 201) {
            toast.success("Gist published, redirecting...");
            window.location.replace(`/gist/${json.data.id}`);
          }
        } catch (err: any) {
          toast.error(err.toString());
        }
      })();
    }
  }, [state]);
};

export const useRoutingHandlers = (location: any, dispatch: Dispatcher) => {
  return useEffect(() => {
    if (location.state && location.state.authed) {
      const params = new URLSearchParams(location.state.params);
      const username = params.get("profile[login]");
      const access_token = params.get("access_token");
      const avatar = params.get("profile[avatar_url]");
      if (username !== null && access_token !== null && avatar !== null) {
        dispatch({
          kind: "CHANGE_GH_USER",
          user: {
            username,
            access_token,
            avatar,
          },
        });
      } else {
        toast.error(
          `Authentication failed: username=${username}, access_token=${access_token}, avatar=${avatar}`
        );
      }
    } else if (location.pathname === "/repo" && location.search) {
      const params = new URLSearchParams(location.search);
      const prefix = params.get("prefix");
      const sub = params.get("sub");
      const sty = params.get("sty");
      const dsl = params.get("dsl");
      if (prefix && sub && sty && dsl) {
        (async () => {
          try {
            const baseURL = `https://raw.githubusercontent.com/${prefix}/`;
            const subContent = await (await fetch(baseURL + sub)).text();
            const styContent = await (await fetch(baseURL + sty)).text();
            const dslContent = await (await fetch(baseURL + dsl)).text();
            dispatch({
              kind: "SET_TRIO",
              sub: subContent,
              sty: styContent,
              dsl: dslContent,
            });
            dispatch({
              kind: "SET_AUTHORSHIP",
              authorship: {
                name: sub,
                madeBy: "github repo",
                gistID: null,
                avatar: null,
              },
            });
            tryDomainHighlight(dslContent, dispatch);
          } catch (err) {
            toast.error(`Couldn't retrieve files: ${err}`);
          }
        })();
      } else {
        toast.error(
          `Invalid params: prefix=${prefix},
          sub=${sub}, sty=${sty}, dsl=${dsl}`
        );
      }
    }
  }, [location, dispatch]);
};

export const retrieveGist = async (gistId: string, dispatch: Dispatcher) => {
  try {
    const res = await fetch(`https://api.github.com/gists/${gistId}`, {
      headers: {
        accept: "application/vnd.github.v3+json",
      },
    });
    const json = await res.json();
    const { files } = json;
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
  } catch (err: any) {
    toast.error(`Could not retrieve gist ID: ${err.toString()}`);
  }
};

export const tryDomainHighlight = (
  dsl: string,
  dispatch?: Dispatcher
): any | null => {
  const domainComp = compileDomain(dsl);
  if (domainComp.isOk()) {
    dispatch &&
      dispatch({ kind: "SET_DOMAIN_CACHE", domainCache: domainComp.value });
    return domainComp.value || null;
  }
};
