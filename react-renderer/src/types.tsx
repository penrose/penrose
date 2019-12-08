export interface IGPIProps {
  shape: any;
  canvasSize: [number, number];
  // COMBAK: why are ctm and dragEvent property here and not in the next interface?? We should separate static and interactive rendering in the near future
  // ctm: DOMMatrix;
  // dragEvent?(id: string, dy: number, dx: number): void;
}

export interface IGPIPropsDraggable extends IGPIProps {
  onClick(e: React.MouseEvent<any>): void;
  ctm: DOMMatrix;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface ILayerProps {
  shapes: Array<[string, any]>;
  debugData: any[];
  ctm: DOMMatrix;
  canvasSize: [number, number];
}

export interface ILayer {
  layer: string;
  enabled: boolean;
}
