/**
 * @typeparam T Wrapped value type.
 */
export class Opt {
    /**
     * Create Opt instance from an array of one or zero items.
     *
     * ```ts
     * Opt.fromArray([]) // None
     * Opt.fromArray([1]) // Some(1)
     * ```
     *
     * @param x
     */
    static fromArray(x) { return opt(x[0]); }
    /**
     * `1` for [[Some]], `0` for [[None]].
     */
    get length() { return this.isEmpty ? 0 : 1; }
    /**
     * Alias of [[flatMap]]
     * @param f
     */
    chain(f) { return this.flatMap(f); }
    /**
     * Combination of [[flatMap]] and [[opt]] functions.
     *
     * ```ts
     * some(1).chainToOpt(x => x === 1 ? null : x + 1) // None
     * some(2).chainToOpt(x => x === 1 ? null : x + 1) // Some(3)
     * ```
     *
     * @param f
     */
    chainToOpt(f) { return this.flatMap(x => opt(f(x))); }
    /**
     * Applies passed function to this instance and returns function result.
     * Also known as a function application, `|>` or pipe operator.
     *
     * ```ts
     * some(1).pipe(x => x.isEmpty) // false
     * none.pipe(x => x.isEmpty) // true
     * ```
     *
     * @param f
     */
    pipe(f) { return f(this); }
}
class None extends Opt {
    toArray() { return []; }
    get isEmpty() { return true; }
    flatMap(_f) { return none; }
    map() { return none; }
    orCrash(msg) { throw new Error(msg); }
    orNull() { return null; }
    orUndef() { return undefined; }
    caseOf(_onSome, onNone) {
        return onNone();
    }
    onNone(f) { f(); }
    onSome(_f) { }
    contains(_x) { return false; }
    exists(_p) { return false; }
    forAll(_p) { return true; }
    orElse(def) { return def; }
    orElseOpt(def) { return def; }
    bimap(_someF, noneF) { return opt(noneF()); }
    flatBimap(_someF, noneF) { return noneF(); }
    toString() { return 'None'; }
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
    orCrash(_msg) { return this.value; }
    orNull() { return this.value; }
    orUndef() { return this.value; }
    caseOf(onSome, _onNone) { return onSome(this.value); }
    contains(x) { return this.value === x; }
    exists(p) { return p(this.value); }
    forAll(p) { return p(this.value); }
    onNone(_f) { }
    onSome(f) { f(this.value); }
    orElse(_def) { return this.value; }
    orElseOpt(_def) { return this; }
    bimap(someF, _noneF) { return opt(someF(this.value)); }
    flatBimap(someF, _noneF) { return someF(this.value); }
    toString() { return `Some(${JSON.stringify(this.value)})`; }
}
const isNoneValue = (x) => {
    return x === undefined || x === null || Number.isNaN(x);
};
/**
 * Single global instance of [[None]].
 */
export const none = Object.freeze(new None());
/**
 * Constructs [[Some]].
 * Usually it is [[opt]] you are looking for (only in rare cases you want to have for example `Some(undefined)`).
 * @param x
 */
export const some = (x) => new Some(x);
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
export const opt = (x) => isNoneValue(x) ? none : new Some(x);
/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
export const isOpt = (x) => x instanceof Opt;
//# sourceMappingURL=Opt.js.map