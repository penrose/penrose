import { TextDecoder, TextEncoder } from "util";

// https://stackoverflow.com/a/57943686
global.TextDecoder = TextDecoder;
global.TextEncoder = TextEncoder;
