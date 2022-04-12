import * as React from "react";
import { useCallback, useState } from "react";
import { AuthorshipInfo, GithubUser } from "../reducer";
import BlueButton from "./BlueButton";

const AuthorshipTitle = ({
  authorship,
  onChangeTitle,
  myUser,
  onPublish,
}: {
  authorship: AuthorshipInfo;
  onChangeTitle(name: string): void;
  myUser: GithubUser | undefined;
  onPublish(): void;
}) => {
  const [editingTitle, setEditingTitle] = useState(false);
  const [titleDraft, setTitleDraft] = useState(authorship.name);
  const changeTitle = useCallback((e) => {
    setTitleDraft(e.target.value);
  }, []);
  const onKey = useCallback(
    (e) => {
      if (e.key === "Enter" && titleDraft.length > 0) {
        onChangeTitle(titleDraft);
        setEditingTitle(false);
      }
    },
    [onChangeTitle, titleDraft]
  );
  const onTitleClick = useCallback(() => {
    setEditingTitle(true);
  }, []);
  const onSignIn = useCallback(() => {
    window.location.replace(
      "https://penrose-gh-auth.vercel.app/connect/github"
    );
  }, []);

  return (
    <div>
      {editingTitle ? (
        <input
          type="text"
          maxLength={50}
          style={{ fontSize: "16px", width: "150px" }}
          value={titleDraft}
          onChange={changeTitle}
          onKeyPress={onKey}
        />
      ) : (
        <span style={{ cursor: "pointer" }} onClick={onTitleClick}>
          {authorship.name}
        </span>
      )}
      {" - "}
      <span style={{ color: "#7d7d7d" }}>
        {authorship.madeBy ?? "anonymous"}
        {authorship.gistID === undefined &&
          (myUser === undefined ? (
            <BlueButton onClick={onSignIn}>sign in</BlueButton>
          ) : (
            <BlueButton onClick={onPublish}>publish</BlueButton>
          ))}
      </span>
    </div>
  );
};

export default AuthorshipTitle;
