export default async function fetchResolver(
  path: string
): Promise<string | null> {
  const response = await fetch(path);
  if (!response.ok) {
    console.error(`could not fetch ${path}`);
    return null;
  }
  return await response.text();
}
