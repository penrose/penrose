import { useState } from "react";
import { useRecoilState } from "recoil";
import styled from "styled-components";
import { AuthModalState, currentAuthModalState } from "../state/atoms.js";
import {
  logInUser,
  loginWithGithub,
  registerUser,
} from "../utils/authUtils.js";

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

  const loginWrapper = () => {
    console.log("hit 1");
    console.log(logInUser());
    if (logInUser()) {
      console.log("hit 2");
      closeModal();
    }
  };

  const registerWrapper = () => {
    if (registerUser()) {
      closeModal();
    }
  };

  const forgotPassWrapper = () => {
    if (registerUser()) {
      setShowForgotPass(false);
    }
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
              <FormButton onClick={loginWrapper}>Log In</FormButton>
              <AdditionalOptions onClick={toggleMode}>
                New user? Register
              </AdditionalOptions>
              {showForgotPass ? (
                <AdditionalOptions onClick={forgotPassWrapper}>
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
              <FormButton onClick={registerWrapper}>Register</FormButton>
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
