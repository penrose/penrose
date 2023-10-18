// Adapted from https://github.dev/vitest-dev/vitest/blob/991ff33ab717caee85ef6cbe1c16dc514186b4cc/scripts/update-contributors.ts#L6

interface Contributor {
  login: string;
}

async function fetchContributors(): Promise<string[]> {
  const collaborators: string[] = [];
  try {
    let page = 1;
    let data: Contributor[] = [];
    do {
      const response = await fetch(
        `https://api.github.com/repos/penrose/penrose/contributors?per_page=100&page=${page}`,
        {
          method: "GET",
          headers: {
            "content-type": "application/json",
          },
        },
      );
      data = await response.json();
      collaborators.push(...data.map((i) => i.login));
      console.log(`Fetched page ${page}`);
      page++;
    } while (data.length === 100);
  } catch (e) {
    /* contributors fetching failure must not hinder docs development */
  }
  return collaborators.filter((name) => !name.includes("[bot]"));
}

export default {
  async load() {
    return await fetchContributors();
  },
};
