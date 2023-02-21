const Checkbox = ({
  className,
  checked,
  onChange,
  ...props
}: {
  className?: string;
  checked: boolean;
  onChange: (event: any) => void;
  props?: any[];
}) => <input type="checkbox" checked={checked} onChange={onChange} />;

export default Checkbox;
