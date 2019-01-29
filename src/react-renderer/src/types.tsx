export interface IGPIProps {
  shape: any;
  canvasSize: [number, number];
  ctm: DOMMatrix;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface IGPIPropsDraggable extends IGPIProps {
  onClick(e: React.MouseEvent<any>): void;
}
