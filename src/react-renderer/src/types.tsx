export interface IGPIProps {
  shape: any;
  canvasSize: [number, number];
  ctm: DOMMatrix;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface IGPIPropsDraggable extends IGPIProps {
  onClick(e: React.MouseEvent<any>): void;
}

export interface ILayerProps {
  shapes: Array<[string, any]>;
  ctm: DOMMatrix;
  canvasSize: [number, number];
}

export interface ILayer {
  layer: string;
  enabled: boolean;
}