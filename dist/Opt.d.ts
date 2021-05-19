import { ActFn, ActInClassFn, MapFlowInClassFn, MapFlowFn, PipeInClassFn, PipeFn, ActToOptInClassFn, ActToOptFn, FlowFn, ComposeFn } from './FlowLike';
export declare type EqualityFunction = <T>(a: T, b: T) => boolean;
declare type NotObject<T> = T extends object ? never : T;
declare type SuperUnionOf<T, U> = Exclude<U, T> extends never ? NotObject<T> : never;
declare type WithoutOptValues<T> = NonNullable<T>;
/**
 * @typeparam T Wrapped value type.
 */
export declare abstract class Opt<T> {
    /**
     * `false` for [[Some]], `true` for [[None]].
     */
    abstract get isEmpty(): boolean;
    /**
     * `false` for [[Some]], `true` for [[None]].
     */
    get nonEmpty(): boolean;
    /**
     * Is this instance of [[Some]]?
     */
    isSome(): this is Some<T>;
    /**
     * Is this instance of [[None]]?
     */
    isNone(): this is None<T>;
    /**
     * `1` for [[Some]], `0` for [[None]].
     */
    get length(): number;
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
    static fromArray<T>(x: [] | [T]): Opt<T>;
    /**
     * Converts `Opt` to an array.
     *
     * ```ts
     * some(1).toArray() // [1]
     * none.toArray() // []
     * ```
     */
    abstract toArray(): [] | [T];
    /**
     * Applies function to the wrapped value and returns a new instance of [[Some]].
     *
     * ```
     * some(1).map(x => x + 1) // Some(2)
     * none.map(x => x + 1) // None
     * ```
     *
     * @param f
     */
    abstract map<U>(f: (_: T) => U): Opt<U>;
    /**
     * Similar to [[map]], but supports more functions which are called in succession, each on a result of a previous one.
     *
     * ```
     * const sq = (x: number) => x * x;
     * const dec = (x: number) => x - 1;
     * opt(4).mapFlow(sq, dec) // Some(15)
     * opt(null).mapFlow(sq, dec) // None
     * ```
     * @param fs
     */
    mapFlow: MapFlowInClassFn<T>;
    /**
     * Similar to [[map]], but function is expected to return [[Opt]] which will be returned.
     * Useful for including steps which may fail or return no value.
     *
     * ```
     * some(1).flatMap(x => x === 1 ? none : some(x + 1)) // None
     * none.flatMap(x => some(1)) // None
     * ```
     *
     * @param f
     */
    abstract flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    /**
     * Alias of [[flatMap]]
     * @param f
     */
    chain<U>(f: (_: T) => Opt<U>): Opt<U>;
    /**
     * Similar to [[chain]] (in other languages called `bind` or `>>=`), but supports more functions passed at once (resembles `do` notation in Haskell).
     * It is used to model a sequence of operations where each operation can fail (can return [[None]]).
     *
     * ```ts
     * // does addition when first argument is number
     * const f1 =
     *   (x: string | number) => (y: number) => opt(x).narrow(isNumber).map(z => z + y);
     * // passes only even numbers
     * const f2 = (x: number): Opt<number> => x % 2 === 0 ? opt(x) : none;
     *
     * opt(0).act( // Some(0)
     *   f1(-2), // Some(-2)
     *   f2, // Some(-2)
     *   optNegative, // None
     * ); // None
     *
     * opt(0).act( // Some(0)
     *   f1(1), // Some(1)
     *   f2, // None
     *   optNegative, // won't get called, still None
     * ); // None
     *
     * opt(3).act( // Some(3)
     *   f1(1), // Some(4)
     *   f2, // Some(4)
     *   optNegative, // Some(4)
     * ); // Some(4)
     * ```
     *
     * @param fs
     */
    act: ActInClassFn<T>;
    /**
     * Alias of [[act]]
     * @param args
     */
    chainFlow: ActInClassFn<T>;
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
    chainToOpt<U>(f: (_: T) => U | undefined | null): Opt<U>;
    /**
     * Similar to [[act]], but functions return empty values instead of [[Opt]].
     * It is useful for typical JavaScript functions (e.g. lodash), properly handles `undefined`/`null`/`NaN` at any point of the chain.
     *
     * ```ts
     * const data = [{}, {f: true, a: [{b: 7, c: 1}]}, {a: [{}]}];
     * opt(data).actToOpt(
     *   find(x => Boolean(x?.f)), // {f: true, a: [{b: 7, c: 1}]}
     *   x => x?.a, // [{b: 7, c: 1}]
     *   find(x => x.b === 8) // undefined
     * ); // None
     * ```
     *
     * @param fs
     */
    actToOpt: ActToOptInClassFn<T>;
    /**
     * Alias of [[actToOpt]].
     * @param args
     */
    chainToOptFlow: ActToOptInClassFn<T>;
    /**
     * Returns value when [[Some]], throws error with `msg` otherwise.
     * @param msg Error message.
     */
    abstract orCrash(msg: string): T;
    /**
     * Crash when called on [[None]], pass [[Opt]] instance on [[Some]].
     *
     * ```ts
     * some(1).optOrCrash('fail') // Some(1)
     * none.optOrCrash('fail') // throws
     * ```
     *
     * @param msg
     * @deprecated Please use [[someOrCrash]] instead
     */
    abstract optOrCrash(msg: string): Opt<T>;
    /**
     * Crash when called on [[None]], pass [[Opt]] instance on [[Some]].
     *
     * ```ts
     * some(1).someOrCrash('fail') // Some(1)
     * none.someOrCrash('fail') // throws
     * ```
     *
     * @param msg
     */
    abstract someOrCrash(msg: string): Some<T>;
    /**
     * Returns value for [[Some]] or `undefined` for [[None]].
     *
     * ```ts
     * some(1).orUndef() // 1
     * none.orUndef() // undefined
     * ```
     */
    abstract orUndef(): T | undefined;
    /**
     * Returns value for [[Some]] or `null` for [[None]].
     *
     * ```ts
     * some(1).orNull() // 1
     * none.orNull() // null
     * ```
     */
    abstract orNull(): T | null;
    /**
     * Returns inner value for [[Some]], `false` for [[None]].
     *
     * ```ts
     * some(1).orFalse() // 1
     * none.orFalse() // false
     * ```
     */
    abstract orFalse(): T | false;
    /**
     * Returns inner value for [[Some]], `true` for [[None]].
     *
     * ```ts
     * some(1).orTrue() // 1
     * none.orTrue() // true
     * ```
     */
    abstract orTrue(): T | true;
    /**
     * Returns inner value for [[Some]], `NaN` for [[None]].
     *
     * ```ts
     * some(1).orNaN() // 1
     * none.orNaN() // NaN
     * ```
     */
    abstract orNaN(): T | number;
    /**
     * Applies appropriate function and returns result from the function.
     *
     * ```
     * some(1).caseOf(x => x + 1, () => 0) // 2
     * none.caseOf(x => x + 1, () => 0) // 0
     * ```
     *
     * @param onSome Processing function for [[Some]].
     * @param onNone Processing function for [[None]].
     */
    abstract caseOf<R>(onSome: (x: T) => R, onNone: () => R): R;
    /**
     * Calls `f` on [[Some]] with its value, does nothing for [[None]].
     * @param f
     */
    abstract onSome(f: (x: T) => void): Opt<T>;
    /**
     * Calls `f` on [[None]], does nothing for [[Some]].
     * @param f
     */
    abstract onNone(f: () => void): Opt<T>;
    /**
     * Applies passed function to this instance and returns function result.
     * Also known as a reverse function application, `|>` (Reason/ReScript, F#, OCaml), `&` (Haskell), `#` (PureScript) or a pipe operator.
     *
     * ```ts
     * some(1).pipe(x => x.isEmpty) // false
     * none.pipe(x => x.isEmpty) // true
     * ```
     *
     * Supports multiple functions.
     *
     * ```ts
     * opt(1).pipe( // Some(1)
     *   x => x.isEmpty, // false
     *   x => !x, // true
     * ) // true
     * ```
     *
     * @param fs Functions in call chain
     */
    pipe: PipeInClassFn<T>;
    /**
     * Compares inner value with given value using `===`. Always `false` for [[None]].
     *
     * ```ts
     * some(1).contains(1) // true
     * some(0).contains(1) // false
     * none.contains(undefined) // false
     * ```
     *
     * @param x
     */
    abstract contains(x: T): boolean;
    /**
     * Applies `p` to inner value and passes result. Always `false` for [[None]].
     *
     * ```ts
     * some(0).exists(x => x > 0) // false
     * some(1).exists(x => x > 0) // true
     * none.exists(x => x > 0) // false
     * ```
     *
     * @param p Predicate.
     */
    abstract exists(p: (x: T) => boolean): boolean;
    /**
     * Applies `p` to inner value and passes result. Always `true` for [[None]].
     *
     * ```
     * some(0).forAll(x => x > 0) // false
     * some(1).forAll(x => x > 0) // true
     * none.forAll(x => x > 0) // true
     * ```
     * @param p Predicate.
     */
    abstract forAll(p: (x: T) => boolean): boolean;
    /**
     * Get inner value if [[Some]], or use passed `def` value for [[None]].
     *
     * ```ts
     * some(1).orElse(2) // 1
     * none.orElse(2) // 2
     * ```
     *
     * @param def Default value.
     */
    abstract orElse(def: T): T;
    /**
     * Return `this` for [[Some]], `def` for [[None]].
     *
     * ```ts
     * some(1).orElseOpt(some(2)) // Some(1)
     * none.orElseOpt(some(2)) // Some(2)
     * none.orElseOpt(none) // None
     * ```
     * @param def
     */
    abstract orElseOpt(def: Opt<T>): Opt<T>;
    /**
     * Similar to [[caseOf]] but doesn't unwrap value.
     *
     * ```
     * some(1).bimap(x => x + 1, () => 0) // Some(2)
     * none.bimap(x => x + 1, () => 0) // Some(0)
     * ```
     * @param someF
     * @param noneF
     */
    abstract bimap<U>(someF: (_: T) => U, noneF: () => U): Opt<U>;
    /**
     * Similar to [[bimap]], but accepts functions returning [[Opt]].
     *
     * ```
     * some(1).flatBimap(x => some(x+1), () => some(0)) // Some(2)
     * none.flatBimap(x => some(x+1), () => some(0)) // Some(0)
     * some(5).flatBimap(x => none, () => some(0)) // None
     * ```
     * @param someF
     * @param noneF
     */
    abstract flatBimap<U>(someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
    /**
     * Formats [[Opt]] to string. In case of [[Some]] inner value is converted using `JSON.stringify`.
     *
     * ```ts
     * some(1).toString() // 'Some(1)'
     * none.toString() // 'None'
     * ```
     */
    abstract toString(): string;
    /**
     * Joins two optional values to a pair. If either of them is [[None]] then the result is [[None]].
     *
     * ```ts
     * some(1).zip(some(true)) // Some([1, true])
     * some(1).zip(none) // None
     * none.zip(some(1)) // None
     * none.zip(none) // None
     * ```
     * @param other
     */
    abstract zip<U>(other: Opt<U>): Opt<[T, U]>;
    /**
     * Same as [[zip]], but with one more optional.
     * ```ts
     * some(1).zip3(some('a'), some(false)) // Some([1, 'a', false])
     * none.zip3(some(1), some(2)) // None
     * ```
     * @param x
     * @param y
     */
    abstract zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]>;
    /**
     * Same as [[zip3]], but with one more optional.
     * @param x
     * @param y
     * @param z
     */
    abstract zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]>;
    /**
     * Same as [[zip4]], but with one more optional.
     * @param x
     * @param y
     * @param z
     * @param zz
     */
    abstract zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    /**
     * Returns [[Some]] with same value if predicate holds, [[None]] otherwise.
     * ```ts
     * opt(1).filter(x => x > 0); // Some(1)
     * opt(-1).filter(x => x > 0); // None
     * ```
     * @see [[noneIf]]
     * @param predicate
     */
    abstract filter(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Returns [[None]] if predicate holds, otherwise passes same instance of [[Opt]].
     * ```ts
     * opt(1).noneIf(x => x > 0); // None
     * opt(-1).noneIf(x => x > 0); // Some(-1)
     * ```
     * @see [[filter]]
     * @param predicate
     */
    noneIf(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Narrows type inside [[Opt]] using given type guard.
     * ```ts
     * some('1' as string | number).narrow(isString) // Some('1'): Opt<string>
     * some(1 as string | number).narrow(isString) // None: Opt<string>
     * ```
     * @param guard
     */
    abstract narrow<U>(guard: (value: any) => value is U): Opt<U>;
    /**
     * Print value to console.
     * ```ts
     * opt(1).print() // logs 'Some:', '1'; returns Some(1)
     * opt(1).print('test') // logs '[test]', 'Some:', '1'; returns Some(1)
     * none.print('x') // logs '[x]', 'None'; returns None
     * ```
     * @param tag
     */
    abstract print(tag?: string): Opt<T>;
    /**
     * Is a value of this instance and given `other` instance the same?
     * Default comparator function is `===` (referential equality).
     *
     * ```ts
     * none.equals(none) // true
     * some(1).equals(none) // false
     * some(1).equals(some(1)) // true
     * some(1).equals(some(2)) // false
     * some({a: 1}).equals(some({a: 1})) // false (different objects)
     * some(1).equals(some(2), (x, y) => true) // true (custom comparator function)
     *
     * const jsonCmp = <T>(a: T, b: T): boolean => JSON.stringify(a) === JSON.stringify(b);
     * some({a: 1}).equals(some({a: 1}), jsonCmp) // true (comparing values converted to JSON)
     * ```
     * @param other
     * @param comparator
     */
    abstract equals(other: Opt<T>, comparator?: EqualityFunction): boolean;
    /**
     * Widen union (typically union of strings to string).
     * Experimental. May be removed if it is later found out it's unsafe and unfixable.
     * ```ts
     * opt(someValueOfUnionType).widen<SuperOfThatUnion>() // :Opt<SuperOfThatUnion>
     * ```
     */
    widen<U, R extends SuperUnionOf<U, T> = SuperUnionOf<U, T>>(): Opt<R>;
    /**
     * Maps property of a wrapped object.
     *
     * ```ts
     * const a = {x: 1};
     * const xValue = opt(a).prop('x').orCrash('missing prop x'); // 1
     * ```
     * @param key
     */
    abstract prop<K extends (T extends object ? keyof T : never)>(key: K): Opt<WithoutOptValues<T[K]>>;
}
/**
 * Empty [[Opt]].
 * @notExported
 * @see [[Opt]]
 */
declare class None<T> extends Opt<T> {
    readonly '@@type': symbol;
    get isEmpty(): boolean;
    toArray(): [] | [T];
    flatMap<U>(_f: (_: T) => Opt<U>): Opt<U>;
    map<U>(): Opt<U>;
    orCrash(msg: string): T;
    /**
     * @deprecated Please use [[someOrCrash]] instead
     */
    optOrCrash(msg: string): Opt<T>;
    someOrCrash(msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(_onSome: (x: T) => R, onNone: () => R): R;
    onNone(f: () => void): Opt<T>;
    onSome(_f: (x: T) => void): Opt<T>;
    contains(_x: T): boolean;
    exists(_p: (x: T) => boolean): boolean;
    forAll(_p: (x: T) => boolean): boolean;
    orElse(def: T): T;
    orElseOpt(def: Opt<T>): Opt<T>;
    bimap<U>(_someF: (_: T) => U, noneF: () => U): Opt<U>;
    flatBimap<U>(_someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
    toString(): string;
    zip<U>(_other: Opt<U>): Opt<[T, U]>;
    zip3<X, Y>(_x: Opt<X>, _y: Opt<Y>): Opt<[T, X, Y]>;
    zip4<X, Y, Z>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>): Opt<[T, X, Y, Z]>;
    zip5<X, Y, Z, ZZ>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>, _zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    filter(_predicate: (_: T) => boolean): Opt<T>;
    narrow<U>(_guard: (value: any) => value is U): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, _comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(_key: K): Opt<WithoutOptValues<T[K]>>;
}
/**
 * [[Opt]] with a value inside.
 * @notExported
 * @see [[Opt]]
 */
declare class Some<T> extends Opt<T> {
    private _value;
    readonly '@@type': symbol;
    constructor(_value: T);
    get isEmpty(): boolean;
    get value(): T;
    toArray(): [] | [T];
    flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    map<U>(f: (_: T) => U): Opt<U>;
    orCrash(_msg: string): T;
    /**
     * @deprecated Please use [[someOrCrash]] instead
     */
    optOrCrash(_msg: string): Opt<T>;
    someOrCrash(_msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R;
    contains(x: T): boolean;
    exists(p: (x: T) => boolean): boolean;
    forAll(p: (x: T) => boolean): boolean;
    onNone(_f: () => void): Opt<T>;
    onSome(f: (x: T) => void): Opt<T>;
    orElse(_def: T): T;
    orElseOpt(_def: Opt<T>): Opt<T>;
    bimap<U>(someF: (_: T) => U, _noneF: () => U): Opt<U>;
    flatBimap<U>(someF: (_: T) => Opt<U>, _noneF: () => Opt<U>): Opt<U>;
    toString(): string;
    zip<U>(other: Opt<U>): Opt<[T, U]>;
    zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]>;
    zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]>;
    zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    filter(predicate: (_: T) => boolean): Opt<T>;
    narrow<U>(guard: (value: any) => value is U): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(key: K): Opt<WithoutOptValues<T[K]>>;
}
declare const someSerializedType = "Opt/Some";
declare const noneSerializedType = "Opt/None";
declare type OptSerialized = {
    type: typeof noneSerializedType;
} | {
    type: typeof someSerializedType;
    value: any;
};
export declare class ReduxDevtoolsCompatibilityHelper {
    static replacer(_key: unknown, value: any): any | OptSerialized;
    static reviver(_key: unknown, value: any): any;
}
/**
 * Single global instance of [[None]].
 */
export declare const none: None<any>;
/**
 * Constructs [[Some]].
 * Usually it is [[opt]] you are looking for (only in rare cases you want to have for example `Some(undefined)`).
 * @param x
 */
export declare const some: <T>(x: T) => Readonly<Some<T>>;
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
export declare const opt: <T>(x: T | null | undefined) => Opt<T>;
/**
 * For falsy values returns [[None]], otherwise acts same as [[opt]].
 * ```ts
 * optFalsy(''); // None
 * optFalsy(0); // None
 * optFalsy(false); // None
 * optFalsy(NaN); // None
 * ```
 * @param x
 */
export declare const optFalsy: <T>(x: false | "" | 0 | T | null | undefined) => Opt<T>;
/**
 * For empty array (`[]`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyArray: <T>(x: T[] | null | undefined) => Opt<T[]>;
/**
 * For empty object (`{}`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyObject: <T extends object>(x: T | null | undefined) => Opt<T>;
/**
 * For empty string (`''`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyString: <T>(x: "" | T | null | undefined) => Opt<T>;
/**
 * For a number `0` returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optZero: <T>(x: 0 | T | null | undefined) => Opt<T>;
/**
 * For numbers lesser than `0` returns [[None]], otherwise acts same as [[opt]].
 * Useful for strange functions which return `-1` or other negative numbers on failure.
 * ```ts
 * optNegative(undefined) // None
 * optNegative(1) // Some(1)
 * optNegative(0) // Some(0)
 * optNegative(-1) // None
 * ```
 * @param x
 */
export declare const optNegative: (x: number | undefined | null) => Opt<number>;
/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
export declare const isOpt: (x: unknown) => x is Opt<unknown>;
/**
 * ```ts
 * <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>): Opt<B>
 * ```
 * Apply `oa` to function `of`. If any argument is [[None]] then result is [[None]].
 * ```ts
 * ap(opt(x => x > 0))(opt(1)) // Opt(true)
 * ap(opt(x => x > 0))(none) // None
 * ap(none)(opt(1)) // None
 * ap(none)(none) // None
 * ```
 * @typeparam A input of function inside `of`
 * @typeparam B output of function inside `of`
 */
export declare const ap: <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>) => Opt<B>;
/**
 * ```ts
 * <A, B>(f: (_: A) => B) => (oa: Opt<A>): Opt<B>
 * ```
 * Apply `oa` to function `f`. If argument is [[None]] then result is [[None]].
 * ```ts
 * apFn(x => x > 0)(opt(1)) // Opt(true)
 * apFn(x => x > 0)(none) // None
 * ```
 * @typeparam A input of function `f`
 * @typeparam B output of function `f`
 */
export declare const apFn: <A, B>(f: (_: A) => B) => (oa: Opt<A>) => Opt<B>;
/**
 * Transforms array of opts into an array where [[None]]s are omitted and [[Some]]s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
export declare const catOpts: <A>(xs: Opt<A>[]) => A[];
/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
export declare const mapOpt: <A, B>(f: (_: A) => Opt<B>) => (xs: A[]) => B[];
/**
 * Unwraps one level of nested [[Opt]]s. Similar to `flatten` in other libraries or languages.
 * ```ts
 * joinOpt(some(none)) // None
 * joinOpt(some(some(1))) // Some(1)
 * ```
 * @param x
 */
export declare const joinOpt: <T>(x: Opt<Opt<T>>) => Opt<T>;
/**
 * @see [[Opt.fromArray]]
 */
export declare const fromArray: typeof Opt.fromArray;
/**
 * @see [[Opt.toArray]]
 */
export declare const toArray: <T>(x: Opt<T>) => [] | [T];
declare type MapFn = <T, U>(f: (_: T) => U) => <I extends (Opt<T> | T[]), O extends (I extends Opt<T> ? Opt<U> : U[])>(x: I) => O;
/**
 * Same as [[Opt.map]], but also supports arrays.
 * @see [[Opt.map]]
 */
export declare const map: MapFn;
/** @see [[Opt.mapFlow]] */
export declare const mapFlow: MapFlowFn;
interface FlatMapFn {
    <T, U>(f: (_: T) => U[]): (x: T[]) => U[];
    <T, U>(f: (_: T) => Opt<U>): (x: Opt<T>) => Opt<U>;
}
/**
 * Same as [[Opt.flatMap]], but also supports arrays.
 * @see [[Opt.flatMap]]
 */
export declare const flatMap: FlatMapFn;
/** @see [[Opt.flatMap]] */
export declare const chain: FlatMapFn;
/** @see [[Opt.act]] */
export declare const act: ActFn;
/** @see [[Opt.chainFlow]] */
export declare const chainFlow: ActFn;
/** @see [[Opt.chainToOpt]] */
export declare const chainToOpt: <T, U>(f: (_: T) => U | null | undefined) => (x: Opt<T>) => Opt<U>;
/** @see [[Opt.actToOpt]] */
export declare const actToOpt: ActToOptFn;
/** @see [[Opt.chainToOptFlow]] */
export declare const chainToOptFlow: ActToOptFn;
/** @see [[Opt.someOrCrash]] */
export declare const someOrCrash: <T>(msg: string) => (x: Opt<T>) => Some<T>;
/** @see [[Opt.orCrash]] */
export declare const orCrash: <T>(msg: string) => (x: Opt<T>) => T;
/** @see [[Opt.orUndef]] */
export declare const orUndef: <T>(x: Opt<T>) => T | undefined;
/** @see [[Opt.orNull]] */
export declare const orNull: <T>(x: Opt<T>) => T | null;
/** @see [[Opt.orFalse]] */
export declare const orFalse: <T>(x: Opt<T>) => false | T;
/** @see [[Opt.orTrue]] */
export declare const orTrue: <T>(x: Opt<T>) => true | T;
/** @see [[Opt.orNaN]] */
export declare const orNaN: <T>(x: Opt<T>) => number | T;
/** @see [[Opt.caseOf]] */
export declare const caseOf: <T, R>(onSome: (x: T) => R) => (onNone: () => R) => (x: Opt<T>) => R;
/**
 * Similar to [[Opt.pipe]], but the first argument is the input.
 * Supports arbitrary input type, not just [[Opt]].
 * @see [[Opt.pipe]]
 */
export declare const pipe: PipeFn;
/** @see [[Opt.contains]] */
export declare const contains: <T>(y: T) => (x: Opt<T>) => boolean;
/** @see [[Opt.exists]] */
export declare const exists: <T>(y: (_: T) => boolean) => (x: Opt<T>) => boolean;
/** @see [[Opt.forAll]] */
export declare const forAll: <T>(p: (_: T) => boolean) => (x: Opt<T>) => boolean;
/** @see [[Opt.orElse]] */
export declare const orElse: <T>(e: T) => (x: Opt<T>) => T;
/** @see [[Opt.orElseOpt]] */
export declare const orElseOpt: <T>(def: Opt<T>) => (x: Opt<T>) => Opt<T>;
/** @see [[Opt.bimap]] */
export declare const bimap: <T, U>(someF: (_: T) => U) => (noneF: () => U) => (x: Opt<T>) => Opt<U>;
interface ZipFn {
    <U>(other: Opt<U>): <T>(x: Opt<T>) => Opt<[T, U]>;
    <U>(other: U[]): <T>(x: T[]) => [T, U][];
}
/**
 * Same as [[Opt.zip]], but also supports arrays.
 * @see [[Opt.zip]]
 */
export declare const zip: ZipFn;
/** @see [[Opt.zip3]] */
export declare const zip3: <A>(a: Opt<A>) => <B>(b: Opt<B>) => <T>(x: Opt<T>) => Opt<[T, A, B]>;
/** @see [[Opt.zip4]] */
export declare const zip4: <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <T>(x: Opt<T>) => Opt<[T, A, B, C]>;
/** @see [[Opt.zip5]] */
export declare const zip5: <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>) => <T>(x: Opt<T>) => Opt<[T, A, B, C, D]>;
declare type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];
/**
 * Same as [[Opt.filter]], but also supports arrays.
 * @see [[Opt.filter]]
 */
export declare const filter: FilterFn;
/** @see [[Opt.narrow]] */
export declare const narrow: <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>) => Opt<U>;
/** @see [[Opt.print]] */
export declare const print: (tag?: string | undefined) => <T>(x: Opt<T>) => Opt<T>;
/** @see [[Opt.equals]] */
export declare const equals: <T>(other: Opt<T>, comparator?: EqualityFunction) => (x: Opt<T>) => boolean;
/** @see [[Opt.prop]] */
export declare const prop: <T extends object, K extends T extends object ? keyof T : never = T extends object ? keyof T : never>(key: K) => (x: Opt<T>) => Opt<NonNullable<T[K]>>;
/**
 * Takes functions and builds a function which consecutively calls each given function with a result from a previous one.
 * Similar to [[Opt.pipe]], but doesn't take input directly, instead returns a function which can be called repeatedly with different inputs.
 *
 * ```ts
 * flow( // 1. 63
 *   add1, // 2. 64
 *   Math.sqrt, // 3. 8
 * )(63), // 4. 8
 *
 * // gives same result as
 * Math.sqrt(add1(63)) // 8
 * ```
 *
 * ```ts
 * const f = flow(add1, Math.sqrt); // (_: number) => number
 * f(63); // 8
 * f(3);  // 2
 * ```
 *
 * @param fs
 */
export declare const flow: FlowFn;
/**
 * Composes given functions (in the mathematical sense).
 *
 * Unlike [[flow]] and [[pipe]], functions passed to [[compose]] are applied (called) from last to first.
 *
 * ```ts
 * const f = (x: number): number => x * x;
 * const g = (x: number): string => x.toFixed();
 * const h = (x: string): boolean => x === '4';
 *
 * compose(
 *   h, // 3. true
 *   g, // 2. 4
 *   f, // 1. 2
 * )(2) // 4. true
 *
 * // gives same result as
 * h(g(f(2))) // true
 * ```
 *
 * @param fs
 */
export declare const compose: ComposeFn;
export {};
