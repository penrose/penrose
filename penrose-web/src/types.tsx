// TODO: move into the `types` folder?

export interface IGPIProps {
  shape: any;
  canvasSize: [number, number];
}

export interface IGPIPropsDraggable extends IGPIProps {
  ctm: DOMMatrix;
  onClick(e: React.MouseEvent<any>): void;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface ILayerProps {
  shapes: Shape[];
  debugData: any[];
  ctm: DOMMatrix;
  canvasSize: [number, number];
}

export interface ILayer {
  layer: string;
  enabled: boolean;
}
