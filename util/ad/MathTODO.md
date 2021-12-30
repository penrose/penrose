These functions are provided by the JavaScript Math Object.  Here's their status in Penrose.

| JavaScript  | Autodiff.ts | Functions.ts | Notes
|-------------|-------------|--------------|-------
| `abs(x)`    | `absVal(x)` | `abs`        |
| `acos(x)`   | `acos(x)`   | `acos(x)`    |
| `acosh(x)`  | `acosh(x)`  | `acosh(x)`   |
| `asin(x)`   | `asin(x)`   | `asin(x)`    |
| `asinh(x)`  | `asinh(x)`  | `asinh(x)`   |
| `atan(x)`   | `atan(x)`   | `atan(x)`    |
| `atan2(y,x)`| `atan2(y,x)`| `atan2(y,x)` |
| `atanh(x)`  | `atanh(x)`  | `atanh(x)`   |
| `cbrt(x)`   | `cbrt(x)`   | `cbrt(x)`    |
| `ceil(x)`   | NA          | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `cos(x)`    | `cos(x)`    | `cos(x)`     |
| `cosh(x)`   | `cosh(x)`   | `cosh(x)`    |
| `exp(x)`    | `exp(x)`    | `exp(x)`     |
| `expm1(x)`  | `expm1(x)`  | `expm1(x)`   |
| `floor(x)`  | NA          | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `fround(x)` | NA          | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `hypot(x)`  | TODO        | TODO         |
| `log(x)`    | `ln(x)`     | `log(x)`     | The name `log` conflicts with the error log in `Autodiff.ts`, but is still exposed as `log` in `Functions.ts`.
| `log2(x)`   | `log2(x)`   | `log2(x)`    |
| `log10(x)`  | `log10(x)`  | `log10(x)`   |
| `log1p(x)`  | `log1p(x)`  | `log1p(x)`   |
| `max(x,...)`| `max(x,y)`  | `max(x,y)`   |
| `min(x,...)`| `min(x,y)`  | `min(x,y)`   |
| `pow(x, y)` | TODO        | TODO         |
| `random()`  | NA          | NA           | Can't differentiate in, and semantics are a bit unclear in the context of a Style program.
| `round(x)`  | NA          | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `sign(x)`   | NA          | `sgn(x)`     | Not provided as a differentiable function, since the derivative is zero almost everywhere.
| `sin(x)`    | `sin(x)`    | `sin(x)`     |
| `sinh(x)`   | `sinh(x)`   | `sinh(x)`    |
| `sqrt(x)`   | `sqrt(x)`   | `sqrt(x)`    |
| `tan(x)`    | `tan(x)`    | `tan(x)`     |
| `tanh(x)`   | `tanh(x)`   | `tanh(x)`    |
| `trunc(x)`  | NA          | TODO         | Not provided as a differentiable function, since the derivative is zero almost everywhere.

The JavaScript Math object also provides some standard mathematical constants, as static properties.  In Style we provide these constants as static functions that return a constant value.  (These are all listed as NA in `Autodiff.ts`, since there is no reason to differentiate a constant.)

| JavaScript  | Autodiff.ts | Functions.ts | Notes
|-------------|-------------|--------------|-------
| `E`         | NA          | `MathE()`    |
| `LN2`       | NA          | NA           | Can be computed in Style via `log(2.)`.
| `LN10`      | NA          | NA           | Can be computed in Style via `log(10.)`.
| `LOG2E`     | NA          | NA           | Can be computed in Style via `log2(MathE())`.
| `LOG10E`    | NA          | NA           | Can be computed in Style via `log10(MathE())`.
| `PI`        | NA          | `MathPI()`   |
| `SQRT1_2`   | NA          | NA           | Can be computed in Style via `sqrt(.5)`.
| `SQRT2`     | NA          | NA           | Can be computed in Style via `sqrt(2.)`.

