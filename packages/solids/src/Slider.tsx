export default ({
  curr,
  min,
  max,
  label,
  onChange,
}: {
  curr: number;
  min: number;
  max: number;
  label: string;
  onChange: (n: number) => void;
}) => (
  <div>
    {label}
    <input
      type="range"
      min={min}
      max={max}
      step={(max - min) / 100}
      value={curr}
      class="slider"
      onInput={(n) => onChange(+n.target.value)}
      onChange={(n) => onChange(+n.target.value)}
    />
    {curr}
  </div>
);
