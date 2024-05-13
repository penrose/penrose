import { useRecoilCallback } from "recoil";
import { currentStyleResourcesState } from "../state/atoms";
import { loadFromSnapshot } from "../utils/Utils";

/**
 * Fetch url, but try local storage first using a name.
 * Update local storage if the file is fetched via url
 *
 * @param name The short name of the file to fetch
 * @param url The url to fetch, if not found locally
 * @returns Promise that resolves to the fetched string or undefined if the fetch failed
 */
const fetchResource = async (url?: string): Promise<string | undefined> => {
  try {
    if (!url) return undefined;

    // Try to fetch it by url
    const httpResource = await fetch(url);
    // Update local storage and return the resource
    if (httpResource.ok) {
      const httpBody = httpResource.text();
      return httpBody;
    } else {
      console.log(`HTTP status ${httpResource.status} for ${url}`);
      return undefined;
    }
  } catch (e) {
    console.log(`Error fetching resource with url: ${url}`);
    return undefined;
  }
};

export const usePathResolver = () =>
  useRecoilCallback(
    ({ snapshot }) =>
      async (path: string): Promise<string | undefined> => {
        // Handle absolute URLs
        if (/^(http|https):\/\/[^ "]+$/.test(path)) {
          const url = new URL(path).href;
          return fetchResource(url);
        }

        const resources = loadFromSnapshot(
          snapshot,
          currentStyleResourcesState,
        );

        const ind = resources.findIndex(([name]) => name === path);
        if (ind === -1) {
          return undefined;
        } else {
          const [, resource] = resources.get(ind)!;
          return resource.contents;
        }
      },
  );
