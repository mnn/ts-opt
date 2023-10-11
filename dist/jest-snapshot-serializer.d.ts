interface JestSnapshotSerializer {
    serialize(val: unknown): string;
    test(val: unknown): boolean;
}
declare const jestSnapshotSerializer: JestSnapshotSerializer;
export default jestSnapshotSerializer;
