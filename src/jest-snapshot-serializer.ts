import {isOpt, toString} from './Opt';

interface JestSnapshotSerializer {
  serialize(val: unknown): string;
  test(val: unknown): boolean;
}

const jestSnapshotSerializer: JestSnapshotSerializer = {
  serialize: toString,
  test: isOpt,
};

export default jestSnapshotSerializer;
module.exports = {
  ...jestSnapshotSerializer,
  default: jestSnapshotSerializer
}
