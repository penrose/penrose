import { initializeApp } from "firebase/app";
import { getAuth, signOut } from "firebase/auth";
import {
  collection,
  doc,
  getDocs,
  getFirestore,
  setDoc,
} from "firebase/firestore";
import toast from "react-hot-toast";
import { SavedWorkspaces, Workspace } from "../state/atoms.js";

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

export const signOutWrapper = () => {
  signOut(authObject)
    .then(() => {
      toast.success("logged out");
    })
    .catch((error) => {
      toast.error(error.message);
    });
};

// untested no idea if this is valid
export async function createSavedWorkspaceObject(userid: string) {
  var loadedWorkspaces = {} as SavedWorkspaces;
  const querySnapshot = await getDocs(collection(db, userid));

  querySnapshot.forEach((doc) => {
    var docData = doc.data();
    loadedWorkspaces[doc.id] = {
      metadata: {
        name: docData.name,
        lastModified: docData.lastModified,
        // docData.diagramId vs doc.id
        id: docData.diagramId,
        editorVersion: docData.editorVersion,
        // maybe edit later idk what this does or if this will break things lol
        forkedFromGist: null,
        location: { kind: "local", saved: false },
      },
      files: {
        substance: {
          name: ".substance",
          contents: docData.substance,
        },
        style: {
          name: ".style",
          contents: docData.style,
        },
        domain: {
          name: ".domain",
          contents: docData.domain,
        },
      },
    };
  });

  return loadedWorkspaces;
}

export async function saveNewDiagram(
  userid: string,
  currentWorkspaceState: Workspace,
) {
  // console.log(userid);
  // console.log(currentWorkspaceState);
  await setDoc(doc(db, userid, currentWorkspaceState.metadata.id), {
    diagramId: currentWorkspaceState.metadata.id,
    name: currentWorkspaceState.metadata.name,
    lastModified: currentWorkspaceState.metadata.lastModified,
    editorVersion: currentWorkspaceState.metadata.editorVersion,
    substance: currentWorkspaceState.files.substance.contents,
    style: currentWorkspaceState.files.style.contents,
    domain: currentWorkspaceState.files.domain.contents,
  }).catch((error) => console.log(error));

  // Add to local
}

// export async function saveNewDiagram(
//   userid: string,
//   currentWorkspaceState: Workspace,
// ) {
//   console.log("hit ");
//   await addDoc(collection(db, "cities"), {
//     name: "Los Angeles",
//     state: "CA",
//     country: "USA",
//   })
//     .catch((error) => console.log(error))
//     .then(() => console.log("yeah"));
//   console.log("end");
// }
