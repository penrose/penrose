import { initializeApp } from "firebase/app";
import {
  createUserWithEmailAndPassword,
  getAuth,
  sendEmailVerification,
  sendPasswordResetEmail,
  signInWithEmailAndPassword,
} from "firebase/auth";
import toast from "react-hot-toast";

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
const auth = getAuth(firebaseApp);

export const registerUser = () => {
  var email = document.getElementById("reg_email") as HTMLInputElement;
  var password = document.getElementById("reg_password") as HTMLInputElement;
  var password_confirmation = document.getElementById(
    "confirmed_password",
  ) as HTMLInputElement;

  if (email.value && password.value && password_confirmation.value) {
    if (password.value != password_confirmation.value) {
      toast.error("Password and confirmation must match!");
    }
    createUserWithEmailAndPassword(auth, email.value, password.value)
      .then((userCredential) => {
        // Signed up
        sendEmailVerification(userCredential.user);
        toast.success("Verification email sent, please check your email");
        return true;
      })
      .catch((error) => {
        const errorCode = error.code;
        const errorMessage = error.message;
        toast.error(errorMessage);
      });
  } else {
    toast.error("Cannot register, field is blank");
  }
  return false;
};

export const logInUser = () => {
  var email = document.getElementById("login_email") as HTMLInputElement;
  var password = document.getElementById("login_password") as HTMLInputElement;

  if (email.value && password.value) {
    signInWithEmailAndPassword(auth, email.value, password.value)
      .then((userCredential) => {
        // Signed up
        toast.success("Logged in successfully!");
        return true;
      })
      .catch((error) => {
        const errorCode = error.code;
        const errorMessage = error.message;
        toast.error(errorMessage);
        return false;
      });
  } else {
    toast.error("Cannot log in, field is blank");
    return false;
  }
};

export const forgotPassword = () => {
  var email = document.getElementById("login_email") as HTMLInputElement;
  if (email.value) {
    sendPasswordResetEmail(auth, email.value)
      .then(() => {
        toast.success(`Sent password reset link to ${email.value}`);
        return true;
      })
      .catch((error) => {
        const errorCode = error.code;
        const errorMessage = error.message;
        toast.error(errorMessage);
      });
  } else {
    toast.error("Please enter the account email above");
  }
  return false;
};

export const loginWithGithub = () => {
  toast.error("Github OAuth not yet implemented");
};
