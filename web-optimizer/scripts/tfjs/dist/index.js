"use strict";
/**
 * @license
 * Copyright 2018 Google LLC. All Rights Reserved.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * =============================================================================
 */
function __export(m) {
    for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
}
Object.defineProperty(exports, "__esModule", { value: true });
__export(require("@tensorflow/tfjs-core"));
__export(require("@tensorflow/tfjs-layers"));
__export(require("@tensorflow/tfjs-converter"));
// Export data api as tf.data
var data = require("@tensorflow/tfjs-data");
exports.data = data;
// Import versions of all sub-packages.
var tfjs_core_1 = require("@tensorflow/tfjs-core");
var tfjs_data_1 = require("@tensorflow/tfjs-data");
var tfjs_layers_1 = require("@tensorflow/tfjs-layers");
var tfjs_converter_1 = require("@tensorflow/tfjs-converter");
var version_1 = require("./version");
exports.version = {
    'tfjs-core': tfjs_core_1.version_core,
    'tfjs-data': tfjs_data_1.version_data,
    'tfjs-layers': tfjs_layers_1.version_layers,
    'tfjs-converter': tfjs_converter_1.version_converter,
    'tfjs': version_1.version
};
//# sourceMappingURL=index.js.map