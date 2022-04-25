import { examples } from "@penrose/examples";

export const exampleFromURI = (uri: string): string => {
  let x = examples;
  for (const part of uri.split("/")) {
    x = x[part];
  }
  return (x as unknown) as string;
};
