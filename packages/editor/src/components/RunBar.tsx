import BlueButton from "./BlueButton";

export default function RunBar({ compile }: { compile(): void }) {
  return (
    <nav
      style={{
        display: "flex",
        width: "100%",
        backgroundColor: "#F4F4F4",
        justifyContent: "space-between",
        alignItems: "center",
        padding: "10px",
        boxSizing: "border-box",
      }}
    >
      {/* TODO: sign in button/avatar */}
      <div>Penrose</div>
      <div>
        <BlueButton onClick={compile}>{"compile"}</BlueButton>
      </div>
    </nav>
  );
}
