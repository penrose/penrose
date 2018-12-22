export interface IGPIProps {
  shape: any;
  canvasSize: [number, number];
  onShapeUpdate?(shape: any): void;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface IGPIPropsDraggable extends IGPIProps {
  dx: number;
  dy: number;
  onClick(e: React.MouseEvent<any>): void;
}
