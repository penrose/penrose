export default async function fetchResolver(
  path: string
): Promise<string | undefined> {
  const response = await fetch(path);
  if (!response.ok) {
    console.error(`could not fetch ${path}`);
    return undefined;
  }
  return await response.text();
}
