These functions are provided by the JavaScript Math Object.  Here's their status in Penrose.

| JavaScript  | Autodiff.ts | Functions.ts | Notes
|-------------|-------------|--------------|-------
| `abs(x)`    | `absVal(x)` | `abs`        |
| `acos(x)`   | `acos(x)`   | TODO         |
| `acosh(x)`  | TODO        | TODO         |
| `asin(x)`   | `asin(x)`   | TODO         |
| `asinh(x)`  | TODO        | TODO         |
| `atan(x)`   | TODO        | TODO         |
| `atan2(y,x)`| `atan2(y,x)`| TODO         |
| `atanh(x)`  | TODO        | TODO         |
| `cbrt(x)`   | TODO        | TODO         |
| `ceil(x)`   | TODO        | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `cos(x)`    | `cos(x)`    | `cos(x)`     |
| `cosh(x)`   | TODO        | TODO         |
| `exp(x)`    | TODO        | TODO         |
| `expm1(x)`  | TODO        | TODO         |
| `floor(x)`  | TODO        | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `fround(x)` | TODO        | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `hypot(x)`  | TODO        | TODO         |
| `log(x)`    | TODO        | TODO         |
| `log2(x)`   | TODO        | TODO         |
| `log10(x)`  | TODO        | TODO         |
| `log1p(x)`  | TODO        | TODO         |
| `max(x,...)`| `max(x,y)`  | `max(x,y)`   |
| `min(x,...)`| `min(x,y)`  | `min(x,y)`   |
| `pow(x, y)` | TODO        | TODO         |
| `random()`  | TODO        | TODO         |
| `round(x)`  | TODO        | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `sign(x)`   | TODO        | `sgn(x)`     | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `sin(x)`    | `sin(x)`    | `sin(x)`     |
| `sinh(x)`   | TODO        | TODO         |
| `sqrt(x)`   | `sqrt(x)`   | `sqrt(x)`    |
| `tan(x)`    | TODO        | TODO         |
| `tanh(x)`   | TODO        | TODO         |
| `trunc(x)`  | TODO        | TODO         |

The JavaScript Math object also provides some standard mathematical constants, as static properties.  In Style we provide these constants as static functions that return a constant value.  (These are all listed as NA in `Autodiff.ts`, since there is no reason to differentiate a constant.)

| JavaScript  | Autodiff.ts | Functions.ts | Notes
|-------------|-------------|--------------|-------
| `E`         | NA          | TODO         |
| `LN2`       | NA          | TODO         |
| `LN10`      | NA          | TODO         |
| `LOG2E`     | NA          | TODO         |
| `LOG10E`    | NA          | TODO         |
| `PI`        | NA          | TODO         |
| `SQRT1_2`   | NA          | TODO         |
| `SQRT2`     | NA          | TODO         |
