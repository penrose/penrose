import {
  createUserWithEmailAndPassword,
  sendEmailVerification,
  sendPasswordResetEmail,
  signInWithEmailAndPassword,
} from "firebase/auth";
import { useState } from "react";
import toast from "react-hot-toast";
import { useRecoilState } from "recoil";
import styled from "styled-components";
import { AuthModalState, currentAuthModalState } from "../state/atoms.js";
import { authObject } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";

const MenuShadow = styled.div<{}>`
  position: absolute;
  z-index: 1;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  alignitems: center;
  justifycontent: center;
`;

const Menu = styled.div<{}>`
  height: 420px;
  width: 300px;
  background-color: white;
  position: relative;
  transform: translate(-50%, -50%);
  top: 50%;
  left: 50%;
  border-radius: 10px;
  border: 2px solid rgba(255, 255, 255, 0.7);
  box-shadow: 0 0 40px rgba(8, 7, 16, 0.8);
  padding: 50px 35px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;

  h3 {
    font-size: 32px;
    font-weight: 500;
    line-height: 20px;
    text-align: center;
  }

  input {
    display: block;
    height: 50px;
    width: 100%;
    background-color: rgba(255, 255, 255, 0.07);
    border-radius: 3px;
    margin-top: 10px;
    font-size: 14px;
    font-weight: 300;
    padding: 0px 5px;
  }
`;

const CloseButton = styled.button<{}>`
  background: none;
  border: none;
  position: absolute;
  top: 0px;
  right: 0px;
  width: 32px;
  height: 32px;
  opacity: 0.6;
  &:hover {
    opacity: 1;
  }
`;

const FormButton = styled.button<{}>`
  margin-top: 10px;
  width: 80%;
  background-color: #ffffff;
  color: #080710;
  padding: 15px;
  font-size: 18px;
  font-weight: 600;
  border-radius: 5px;
  cursor: pointer;
`;

const AdditionalOptions = styled.div<{}>`
  margin-top: 15px;
  opacity: 0.7;
  &:hover {
    opacity: 1;
  }
  cursor: pointer;
`;

export const AuthMenuModal = () => {
  const [authModalState, setAuthModalState] = useRecoilState<AuthModalState>(
    currentAuthModalState,
  );

  const toggleMode = () => {
    setAuthModalState({
      loginIsOpen: !authModalState.loginIsOpen,
      registerIsOpen: !authModalState.registerIsOpen,
    });
  };

  const closeModal = () => {
    setAuthModalState({
      loginIsOpen: false,
      registerIsOpen: false,
    });
  };

  const [showForgotPass, setShowForgotPass] = useState(true);

  const registerUser = () => {
    var email = document.getElementById("reg_email") as HTMLInputElement;
    var password = document.getElementById("reg_password") as HTMLInputElement;
    var password_confirmation = document.getElementById(
      "confirmed_password",
    ) as HTMLInputElement;

    if (email.value && password.value && password_confirmation.value) {
      if (password.value != password_confirmation.value) {
        toast.error("Password and confirmation must match!");
        return;
      }
      createUserWithEmailAndPassword(authObject, email.value, password.value)
        .then((userCredential) => {
          // Signed up
          sendEmailVerification(userCredential.user)
            .catch((error) => {
              toast.error(`Error: could not send verification email`);
              console.log(error.message);
            })
            .then(() => {
              toast.success("Verification email sent, please check your email");
            });
          closeModal();
        })
        .catch((error) => {
          const errorCode = error.code;
          const errorMessage = error.message;
          console.log(errorMessage);
          toast.error("Error registering");
        });
    } else {
      toast.error("Cannot register, field is blank");
    }
  };

  const logInUser = () => {
    var email = document.getElementById("login_email") as HTMLInputElement;
    var password = document.getElementById(
      "login_password",
    ) as HTMLInputElement;

    if (email.value && password.value) {
      signInWithEmailAndPassword(authObject, email.value, password.value)
        .then((userCredential) => {
          // Signed up
          toast.success("Logged in successfully!");
          closeModal();
        })
        .catch((error) => {
          const errorCode = error.code;
          const errorMessage = error.message;
          console.log(errorMessage);
          if (errorCode == "auth/invalid-credential") {
            toast.error("Invalid credentials");
          } else {
            toast.error("Error logging in");
          }
        });
    } else {
      toast.error("Cannot log in, field is blank");
    }
  };

  const forgotPassword = () => {
    var email = document.getElementById("login_email") as HTMLInputElement;
    if (email.value) {
      sendPasswordResetEmail(authObject, email.value)
        .then(() => {
          toast.success(`Sent password reset link to ${email.value}`);
          setShowForgotPass(false);
          // Password reset email sent!
          // ..
        })
        .catch((error) => {
          const errorCode = error.code;
          const errorMessage = error.message;
          console.log(errorMessage);
          toast.error("Error resetting password");
        });
    } else {
      toast.error("Please enter the account email above");
    }
  };

  const loginWithGithub = () => {
    toast.error("Github OAuth not yet implemented");
  };

  return (
    <>
      {(authModalState.loginIsOpen || authModalState.registerIsOpen) && (
        <MenuShadow>
          {authModalState.loginIsOpen ? (
            // Login menu
            <Menu>
              <CloseButton onClick={closeModal}>x</CloseButton>
              <h3>Sign in Here</h3>
              <input type="text" placeholder="Email" id="login_email" />
              <input
                type="password"
                placeholder="Password"
                id="login_password"
              />
              <FormButton onClick={logInUser}>Log In</FormButton>
              <AdditionalOptions onClick={toggleMode}>
                New user? Register
              </AdditionalOptions>
              {showForgotPass ? (
                <AdditionalOptions onClick={forgotPassword}>
                  Forgot Password?
                </AdditionalOptions>
              ) : null}

              <AdditionalOptions onClick={loginWithGithub}>
                Login with GitHub
              </AdditionalOptions>
            </Menu>
          ) : (
            // Registration menu
            <Menu>
              <CloseButton onClick={closeModal}>x</CloseButton>
              <h3>Register Here</h3>
              <input type="text" placeholder="Email" id="reg_email" />
              <input type="password" placeholder="Password" id="reg_password" />
              <input
                type="password"
                placeholder="Confirm Password"
                id="confirmed_password"
              />
              <FormButton onClick={registerUser}>Register</FormButton>
              <AdditionalOptions onClick={toggleMode}>
                Return to Login
              </AdditionalOptions>
              <AdditionalOptions onClick={loginWithGithub}>
                Login with GitHub
              </AdditionalOptions>
            </Menu>
          )}
        </MenuShadow>
      )}
    </>
  );
};

export const OpenModalButton = () => {
  const [authModalState, setAuthModalState] = useRecoilState<AuthModalState>(
    currentAuthModalState,
  );

  const toggleLoginModal = () => {
    setAuthModalState({
      loginIsOpen: !authModalState.loginIsOpen,
      registerIsOpen: authModalState.registerIsOpen,
    });
  };

  return <BlueButton onClick={toggleLoginModal}>sign in</BlueButton>;
};
