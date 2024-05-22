import styled from "styled-components";

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
  border: 2px solid #000;
  borderradius: 10px;
  boxshadow: 2px solid black;
  position: relative;
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

export const LoginMenuModal = ({ loginModalState, toggleLoginModal }) => {
  //   const toggleModal = () => {
  //     setLoginModalState(false);
  //   };

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

// export const Modal = ({ isOpen, children }) => {
//   if (!isOpen) return null;

//   return (
//     <div
//       //   onClick={onClose}
//       style={{
//         position: "fixed",
//         top: 0,
//         left: 0,
//         width: "100%",
//         height: "100%",
//         background: "rgba(0, 0, 0, 0.5)",
//         display: "flex",
//         alignItems: "center",
//         justifyContent: "center",
//       }}
//     >
//       <div
//         style={{
//           background: "white",
//           height: 150,
//           width: 240,
//           margin: "auto",
//           padding: "2%",
//           border: "2px solid #000",
//           borderRadius: "10px",
//           boxShadow: "2px solid black",
//         }}
//       >
//         {children}
//       </div>
//     </div>
//   );
// };

// export default function LoginButton() {
//   const [open, setOpen] = React.useState(false);

//   const handleClose = () => {
//     setOpen(false);
//   };

//   const handleOpen = () => {
//     setOpen(true);
//   };

//   return (
//     <div
//       style={{
//         textAlign: "center",
//         display: "block",
//         padding: 30,
//         margin: "auto",
//       }}
//     >
//       <h1 style={{ color: "green" }}>GeeksforGeeks</h1>
//       <h4>Modal Component in ReactJS?</h4>
//       <button type="button" onClick={handleOpen}>
//         Click Me to Open Modal
//       </button>
//       <Modal isOpen={open} onClose={handleClose}>
//         <>
//           <h1>GFG</h1>
//           <h3>A computer science portal!</h3>
//         </>
//       </Modal>
//     </div>
//   );
// }
