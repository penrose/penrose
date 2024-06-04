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
import { SavedWorkspaces, Workspace, settingsState } from "../state/atoms.js";

const firebaseConfig = {
  apiKey: "AIzaSyA_WZrM0sWpOt3oKmPp_D77rS7TsqaTC-w",
  authDomain: "penrose-test.firebaseapp.com",
  projectId: "penrose-test",
  storageBucket: "penrose-test.appspot.com",
  messagingSenderId: "997650567502",
  appId: "1:997650567502:web:f46626596ca78b065d8e1f",
  measurementId: "G-GMD2T1MJ24",
};

const firebaseApp = initializeApp(firebaseConfig);
export const authObject = getAuth(firebaseApp);
export const db = getFirestore(firebaseApp);
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
        toast.error("Error logging in");
      });
  });

export const signOutWrapper = () =>
  useRecoilCallback(({ set }) => async () => {
    signOut(authObject)
      .then(() => {
        set(settingsState, (prevState) => ({
          ...prevState,
          githubAccessToken: null,
        }));
        toast.success("Logged out");
      })
      .catch((error) => {
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
      name: name,
      lastModified: lastModified,
      id: id,
      editorVersion: editorVersion,
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
      toast.error("Could not fetch diagram");
    }
  } else {
    toast.error("User not logged in");
  }
  return null;
}
