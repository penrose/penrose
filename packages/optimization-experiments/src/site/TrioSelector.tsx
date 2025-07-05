import { PathResolver } from "@penrose/core";
import { Trio } from "@penrose/examples/dist/index.js";
import examples from "@penrose/examples/dist/registry.js";
import { makeResolver } from "@penrose/examples/dist/resolver.js";
import { ChangeEvent, useEffect, useState } from "react";

export interface TrioInfo {
  name: string;
  trio: Trio;
  resolver: PathResolver;
}

export const TrioSelector = ({
  setTrioInfo,
}: {
  setTrioInfo: (info: TrioInfo | null) => void;
}) => {
  const [options, setOptions] = useState<TrioInfo[]>([]);
  const [selected, setSelected] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    (async () => {
      const keys = Array.from(examples.keys());

      const optionPromises = keys.map(async (key) => {
        const meta = examples.get(key);
        if (meta && meta.trio) {
          const dir = key.split("/")[0];
          const pathResolver = makeResolver(dir);
          return {
            name: key,
            trio: await meta.get(),
            resolver: pathResolver,
          };
        } else {
          return null;
        }
      });

      const options = await Promise.all(optionPromises);
      const nonNullOptions = options.filter((o) => o !== null) as TrioInfo[];

      setOptions(nonNullOptions);
    })();
  }, []);

  const handleChange = (e: ChangeEvent<HTMLSelectElement>) => {
    const selectedName = e.target.value;
    setSelected(selectedName);
    if (selectedName) {
      const selectedTrio = options.find((o) => o.name === selectedName);
      if (selectedTrio) {
        setTrioInfo(selectedTrio);
        setError(null);
      } else {
        setError("Trio not found");
      }
    } else {
      setTrioInfo(null);
    }
  };

  return (
    <div>
      <select value={selected ?? ""} onChange={handleChange}>
        <option value="">Select a Trio</option>
        {options.sort((a, b) => a.name.localeCompare(b.name)).map((o) => (
          <option key={o.name} value={o.name}>
            {o.name}
          </option>
        ))}
      </select>
      {error && <p style={{ color: "red" }}>{error}</p>}
    </div>
  );
};
