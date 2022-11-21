export const TextDecoder =
  typeof TextDecoder === "undefined"
    ? module.require("util").TextDecoder
    : TextDecoder;

export const TextEncoder =
  typeof TextEncoder === "undefined"
    ? module.require("util").TextEncoder
    : TextEncoder;
