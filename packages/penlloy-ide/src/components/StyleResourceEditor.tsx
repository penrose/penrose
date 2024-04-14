import { useRecoilState, useRecoilValue } from "recoil";
import { currentStyleResourcesState } from "../state/atoms";
import BlueButton from "./BlueButton";

const StyleResource = ({ resourceName }: { resourceName: string }) => {
  const [styleResources, setStyleResources] = useRecoilState(
    currentStyleResourcesState,
  );
  const resource = styleResources.get(resourceName);
  if (resource === undefined) {
    throw new Error("cannot locate resource with key = " + resourceName);
  }

  return (
    <div>
      <span>{resourceName}</span>
      <BlueButton
        onClick={() => {
          styleResources.delete(resourceName);
          setStyleResources(new Map(styleResources));
        }}
      >
        delete
      </BlueButton>
    </div>
  );
};

export const StyleResourceEditor = () => {
  const styleResources = useRecoilValue(currentStyleResourcesState);
  return (
    <div>
      {Array.from(styleResources.keys()).map((key) => (
        <StyleResource resourceName={key} key={key} />
      ))}
    </div>
  );
};
