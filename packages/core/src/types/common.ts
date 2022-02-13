export interface Left<A> {
  tag: "Left";
  contents: A;
}

export interface Right<B> {
  tag: "Right";
  contents: B;
}

export type Either<A, B> = Left<A> | Right<B>;
