import styled from "styled-components";
import React from "react";
const LoaderStyle = styled.section`
  position: absolute,
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  zIndex: 10;
  background: rgba(255,255,255,.5);
  backfaceVisibility: hidden;
`;

const SpinnerStyle = styled.section`
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
  width: 100;
  height: 100;
`;

export interface LoaderProps {
  promiseTracker: any;
  background?: string;
  color?: string;
}
export const Loader = (props: LoaderProps): JSX.Element => {
  const { promiseInProgress } = props.promiseTracker
    ? props.promiseTracker()
    : false;
  return promiseInProgress ? (
    <div>
      <LoaderStyle>
        <SpinnerStyle>{"loading..."}</SpinnerStyle>
      </LoaderStyle>
    </div>
  ) : (
    <div>{""}</div>
  );
};
