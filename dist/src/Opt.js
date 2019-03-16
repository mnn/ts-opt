class Opt {
    get length() { return this.isEmpty ? 0 : 1; }
    chain(f) { return this.flatMap(f); }
}
class Some extends Opt {
    constructor(value) {
        super();
        this.value = value;
    }
    toArray() { return [this.value]; }
    get isEmpty() { return false; }
    flatMap(f) {
        return f(this.value);
    }
    map(f) {
        return new Some(f(this.value));
    }
    orCrash() { return this.value; }
    orNull() { return this.value; }
    orUndef() { return this.value; }
}
class None extends Opt {
    toArray() { return []; }
    get isEmpty() { return true; }
    flatMap() { return none; }
    map() { return none; }
    orCrash(msg) { throw new Error(msg); }
    orNull() { return null; }
    orUndef() { return undefined; }
}
const isNoneValue = (x) => {
    return x === undefined || x === null || Number.isNaN(x);
};
export const none = new None();
export const some = (x) => new Some(x);
export const opt = (x) => isNoneValue(x) ? none : new Some(x);
export const fromArray = (x) => opt(x[0]);
//# sourceMappingURL=Opt.js.map