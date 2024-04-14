import { useRecoilCallback } from "recoil";
import { StyleResources, currentStyleResourcesState } from "../state/atoms";

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

        const resources = snapshot.getLoadable(currentStyleResourcesState)
          .contents as StyleResources;

        const resource = resources.get(path);
        if (resource === undefined) {
          return undefined;
        } else {
          return resource.contents;
        }
      },
  );
