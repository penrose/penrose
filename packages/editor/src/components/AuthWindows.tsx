import styled from "styled-components";
import { LoginModalState } from "../state/atoms.js";

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

const MenuBackground = styled.div<{}>`
  background: white;
  height: 150;
  width: 240;
  margin: auto;
  padding: 2%;
  border: 2px solid #e2e2e2;
  border-radius: 15px;
  boxshadow: 2px solid black;
  position: relative;
  width: 430px;
  height: 520px;
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

interface loginMenuModalProps {
  loginModalState: LoginModalState;
  toggleLoginModal: () => void;
}

export const LoginMenuModal = ({
  loginModalState,
  toggleLoginModal,
}: loginMenuModalProps) => {
  return (
    <>
      {loginModalState.isOpen && (
        <MenuShadow>
          <MenuBackground>
            <h1>aaaaaaaa</h1>
            <CloseButton onClick={toggleLoginModal}>x</CloseButton>
          </MenuBackground>
        </MenuShadow>
      )}
    </>
  );
};
