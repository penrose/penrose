// keep in sync with `./transform.ts`

declare module "*.domain" {
  const contents: string;
  export default contents;
}

declare module "*.style" {
  const contents: string;
  export default contents;
}

declare module "*.substance" {
  const contents: string;
  export default contents;
}

declare module "*.svg" {
  const contents: string;
  export default contents;
}
