import { useState } from "react";
import CayleyGraph from "./CayleyGraph.js";
import MultiplicationTable from "./MultiplicationTable.js";
import { GROUPS } from "./groups.js";
import type { GroupData } from "./groups.js";

const styles: Record<string, React.CSSProperties> = {
  app: {
    display: "flex",
    flexDirection: "column",
    height: "100vh",
    fontFamily: "system-ui, sans-serif",
  },
  header: {
    padding: "16px 24px",
    background: "#1a1a2e",
    color: "white",
    display: "flex",
    alignItems: "center",
    gap: "20px",
    boxShadow: "0 2px 8px rgba(0,0,0,0.3)",
  },
  title: {
    fontSize: "1.4rem",
    fontWeight: 600,
    letterSpacing: "0.02em",
  },
  subtitle: {
    fontSize: "0.85rem",
    opacity: 0.7,
    marginTop: 2,
  },
  select: {
    marginLeft: "auto",
    padding: "8px 14px",
    fontSize: "1rem",
    borderRadius: 6,
    border: "none",
    background: "rgba(255,255,255,0.15)",
    color: "white",
    cursor: "pointer",
    outline: "none",
    minWidth: 220,
  },
  main: {
    flex: 1,
    display: "grid",
    gridTemplateColumns: "1fr 1fr",
    gap: 0,
    overflow: "hidden",
  },
  panel: {
    display: "flex",
    flexDirection: "column",
    overflow: "hidden",
    borderRight: "1px solid #e0e0e0",
  },
  panelHeader: {
    padding: "10px 20px",
    background: "#f0f0f5",
    borderBottom: "1px solid #e0e0e0",
    fontSize: "0.9rem",
    fontWeight: 600,
    color: "#444",
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
  },
  panelBadge: {
    fontSize: "0.75rem",
    background: "#dde",
    color: "#446",
    padding: "2px 8px",
    borderRadius: 10,
    fontWeight: 400,
  },
  panelBody: {
    flex: 1,
    overflow: "hidden",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    background: "white",
  },
  info: {
    padding: "10px 20px",
    background: "#f8f8ff",
    borderTop: "1px solid #e0e0e0",
    fontSize: "0.78rem",
    color: "#666",
    display: "flex",
    gap: 16,
  },
};

function GroupInfo({ group }: { group: GroupData }) {
  const identity = group.elements[0];
  const generatorLabels = group.generators.map((i) => group.elements[i]);
  return (
    <div style={styles.info}>
      <span>
        <strong>Order:</strong> {group.order}
      </span>
      <span>
        <strong>Identity:</strong> {identity}
      </span>
      <span>
        <strong>Generators:</strong> {generatorLabels.join(", ")}
      </span>
    </div>
  );
}

export default function App() {
  const [selectedId, setSelectedId] = useState<string>("Q_4");

  const group: GroupData =
    GROUPS.find((g) => g.id === selectedId) ?? GROUPS[0];

  return (
    <div style={styles.app}>
      <header style={styles.header}>
        <div>
          <div style={styles.title}>Group Explorer</div>
          <div style={styles.subtitle}>{group.phrase}</div>
        </div>
        <select
          style={styles.select}
          value={selectedId}
          onChange={(e) => setSelectedId(e.target.value)}
        >
          {GROUPS.map((g) => (
            <option key={g.id} value={g.id}>
              {g.name} — {g.phrase}
            </option>
          ))}
        </select>
      </header>

      <main style={styles.main}>
        <div style={styles.panel}>
          <div style={styles.panelHeader}>
            Multiplication Table
            <span style={styles.panelBadge}>{group.name}</span>
          </div>
          <div style={styles.panelBody}>
            <MultiplicationTable key={group.id} group={group} />
          </div>
          <GroupInfo group={group} />
        </div>

        <div style={{ ...styles.panel, borderRight: "none" }}>
          <div style={styles.panelHeader}>
            Cayley Graph
            <span style={styles.panelBadge}>{group.name}</span>
          </div>
          <div style={styles.panelBody}>
            <CayleyGraph key={group.id} group={group} />
          </div>
          <GroupInfo group={group} />
        </div>
      </main>
    </div>
  );
}
