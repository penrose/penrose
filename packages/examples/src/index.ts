export type PathResolver = (path: string) => Promise<string | undefined>;

export interface Style {
  contents: string;
  resolver: PathResolver;
}

export interface Trio {
  substance: string;
  styles: Style[];
  domain: string;
  variation: string;
}

export interface Meta {
  get: () => Promise<Trio>;
  name?: string;
  gallery?: boolean;
}
