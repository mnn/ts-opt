"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Opt_1 = require("./Opt");
var jestSnapshotSerializer = {
    serialize: Opt_1.toString,
    test: Opt_1.isOpt,
};
exports.default = jestSnapshotSerializer;
//# sourceMappingURL=jest-snapshot-serializer.js.map