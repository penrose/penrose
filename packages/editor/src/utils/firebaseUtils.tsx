import { initializeApp } from "firebase/app";
import { getAuth, signOut } from "firebase/auth";
import { getFirestore } from "firebase/firestore";
import toast from "react-hot-toast";
import { useResetRecoilState } from "recoil";
import { diagramState } from "../state/atoms.js";

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
      toast.error("Error: Could not log out");
    });

  const resetDiagramState = useResetRecoilState(diagramState);
  resetDiagramState();
};
