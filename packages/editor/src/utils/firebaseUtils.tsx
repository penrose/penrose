import { initializeApp } from "firebase/app";
import {
  GithubAuthProvider,
  getAuth,
  signInWithPopup,
  signOut,
} from "firebase/auth";
import {
  collection,
  doc,
  getDoc,
  getDocs,
  getFirestore,
} from "firebase/firestore";
import toast from "react-hot-toast";
import { useRecoilCallback } from "recoil";
import {
  SavedWorkspaces,
  Workspace,
  currentWorkspaceState,
  defaultWorkspaceState,
  diagramState,
  settingsState,
} from "../state/atoms.js";
import { useClearAutosave } from "../state/callbacks.js";

const firebaseConfig = {
  apiKey: "AIzaSyBhmlpWCnREhtaeVR9fpAllSDSu9UtCXkc",
  authDomain: "penrose-32c8a.firebaseapp.com",
  projectId: "penrose-32c8a",
  storageBucket: "penrose-32c8a.appspot.com",
  messagingSenderId: "898391995102",
  appId: "1:898391995102:web:af47663b5e5e28b2eac84f",
  measurementId: "G-W7X5S06DH1",
};

const firebaseApp = initializeApp(firebaseConfig);
export const authObject = getAuth(firebaseApp);
export const db = getFirestore(firebaseApp, "diagram-store");
export const oauthProvider = new GithubAuthProvider();
oauthProvider.addScope("gist");

// Auth utils
export const logInWrapper = () =>
  useRecoilCallback(({ set }) => async () => {
    signInWithPopup(authObject, oauthProvider)
      .then((result) => {
        const credential = GithubAuthProvider.credentialFromResult(result);
        if (credential !== null) {
          /**
           * Save access token to local state
           * settingsEffect propogates to local storage
           */
          set(settingsState, (prevState) => ({
            ...prevState,
            githubAccessToken:
              credential.accessToken != undefined // For type safety, assign null if undefined
                ? credential.accessToken
                : null,
          }));

          toast.success(`Logged in as ${result.user.displayName}`);
        }
      })
      .catch((error) => {
        console.log(
          `Error code: ${error.code}, Error message: ${error.message}`,
        );
        toast.error("Error logging in");
      });
  });

export const signOutWrapper = () =>
  useRecoilCallback(({ set, reset, snapshot }) => async () => {
    // clear autosave timer to avoid race issue
    useClearAutosave(set, snapshot);

    signOut(authObject)
      .then(() => {
        set(settingsState, (prevState) => ({
          ...prevState,
          githubAccessToken: null,
        }));
        // set rather than reset to generate new id to avoid id conflicts
        set(currentWorkspaceState, () => defaultWorkspaceState());
        reset(diagramState);

        toast.success("Logged out");
      })
      .catch((error) => {
        console.log(
          `Error code: ${error.code}, Error message: ${error.message}`,
        );
        toast.error("Error logging out");
      });
  });

// Firestore database utils
export function createWorkspaceObject(
  name: string,
  lastModified: number,
  id: string,
  editorVersion: number,
  saved: boolean,
  substance: string,
  style: string,
  domain: string,
): Workspace {
  return {
    metadata: {
      name,
      lastModified,
      id,
      editorVersion,
      forkedFromGist: null,
      location: { kind: "stored", saved: saved },
    },
    files: {
      substance: {
        name: ".substance",
        contents: substance,
      },
      style: {
        name: ".style",
        contents: style,
      },
      domain: {
        name: ".domain",
        contents: domain,
      },
    },
  };
}

/*
 * Creates hashmap of workspace ids to workspace objects from user's cloud
 * storage. Used to populate savedFilesState
 */
export async function createSavedWorkspaceObject(userid: string) {
  var loadedWorkspaces = {} as SavedWorkspaces;
  const querySnapshot = await getDocs(collection(db, userid));

  querySnapshot.forEach((doc) => {
    var docData = doc.data();
    loadedWorkspaces[doc.id] = createWorkspaceObject(
      docData.name,
      docData.lastModified,
      docData.diagramId,
      docData.editorVersion,
      true,
      docData.substance,
      docData.style,
      docData.domain,
    );
  });

  return loadedWorkspaces;
}

export async function getDiagram(diagramId: string) {
  if (authObject.currentUser != null) {
    const fetchedDoc = await getDoc(
      doc(db, authObject.currentUser.uid, diagramId),
    );
    if (fetchedDoc.exists()) {
      const docData = fetchedDoc.data();
      return createWorkspaceObject(
        docData.name,
        docData.lastModified,
        docData.diagramId,
        docData.editorVersion,
        true,
        docData.substance,
        docData.style,
        docData.domain,
      );
    } else {
      toast.error(
        "Could not fetch diagram from cloud storage because it doesn't exist.",
      );
    }
  } else {
    toast.error("User not logged in");
  }
  return null;
}
