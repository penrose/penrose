import { add, cos, mul, sin } from "@penrose/core";
import { DiagramBuilder } from "bloom/lib/core/builder.js";
import { canvas } from "bloom/lib/core/utils.js";

export const tire = async () => {
  const { circle, rectangle, build, layer, substance, time } =
    new DiagramBuilder(canvas(800, 700), "abcd");

  const angVel = 1;
  const numSpokePairs = 5;

  const tire = substance();

  tire.angle = mul(angVel, time());

  tire.rim = circle({
    r: 150,
    strokeColor: [0.87, 0.64, 0.07, 1],
    strokeWidth: 10,
    fillColor: [0, 0, 0, 0],
    center: [0, 0],
  });

  tire.axle = circle({
    r: 10,
    fillColor: [0, 0, 0, 0.5],
    center: [0, 0],
  });

  for (let i = 0; i < numSpokePairs; ++i) {
    const spokeAngle = add(((2 * Math.PI) / numSpokePairs) * i, tire.angle);
    tire[`spoke_${i}.1`] = rectangle({
      width: 300,
      height: 2,
      fillColor: [0, 0, 0, 1],
      rotation: mul(spokeAngle, 180 / Math.PI),
      center: [mul(10, sin(spokeAngle)), mul(10, cos(spokeAngle))],
    });
    tire[`spoke_${i}.2`] = rectangle({
      width: 300,
      height: 2,
      fillColor: [0, 0, 0, 1],
      rotation: mul(spokeAngle, 180 / Math.PI),
      center: [mul(-10, sin(spokeAngle)), mul(-10, cos(spokeAngle))],
    });

    layer(tire[`spoke_${i}.1`], tire.rim);
    layer(tire[`spoke_${i}.2`], tire.rim);
  }

  rectangle({
    width: 400,
    height: 10,
    fillColor: [0, 0, 0, 0.5],
    center: [0, -160],
  });

  return await build();
};
