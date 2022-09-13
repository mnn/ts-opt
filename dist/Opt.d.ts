import { ActFn, ActInClassFn, ActToOptFn, ActToOptInClassFn, ComposeFn, FlowFn, MapFlowFn, MapFlowInClassFn, PipeFn, PipeInClassFn } from './FlowLike';
export declare type EqualityFunction = <T>(a: T, b: T) => boolean;
declare type NotObject<T> = T extends object ? never : T;
declare type SuperUnionOf<T, U> = Exclude<U, T> extends never ? NotObject<T> : never;
declare type WithoutOptValues<T> = NonNullable<T>;
declare type EmptyValue = null | undefined;
declare type AnyFunc = (...args: any) => any;
declare type OptSafe<T> = Opt<WithoutOptValues<T>>;
declare class OperationNotAvailable<TypeGot, TypeExpected> {
    readonly '@@type': symbol;
    _notUsed1?: TypeGot;
    _notUsed2?: TypeExpected;
}
interface ConstInClassFn<T> {
    (): () => T | null;
    <E>(emptyValue: E): () => T | E;
}
export declare const isString: (x: any) => x is string;
export declare const toString: (x: {
    toString(): string;
}) => string;
export declare const isArray: (x: any) => x is unknown[];
export declare const isFunction: (x: any) => x is Function;
/**
 * Generic container class. It either holds exactly one value - [[Some]], or no value - [[None]] (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent
 * API" in OOP or [[pipe|chain of reverse applications]] in FP).
 *
 * @typeparam T Wrapped value type.
 */
export declare abstract class Opt<T> {
    /**
     * `false` for [[Some]], `true` for [[None]].
     *
     * @see [[ts-opt.isEmpty]]
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
    get length(): 0 | 1;
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
     * @example
     * ```ts
     * some(1).toArray() // [1]
     * none.toArray() // []
     * ```
     * @see [[ts-opt.toArray]]
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
     * @see [[onSome]] for imperative variant (for use with callback)
     * @see [[ts-opt.map]]
     *
     * @param f
     */
    abstract map<U>(f: (_: T) => U): Opt<U>;
    /**
     * Similar to [[map]], but supports more functions which are called in succession, each on a result of a previous one.
     *
     * @example
     * ```ts
     * const sq = (x: number) => x * x;
     * const dec = (x: number) => x - 1;
     * opt(4).mapFlow(sq, dec) // Some(15)
     * opt(null).mapFlow(sq, dec) // None
     * ```
     * @see [[ts-opt.mapFlow]]
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
     * @see [[ts-opt.flatMap]]
     * @param f
     */
    abstract flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    /**
     * @alias [[flatMap]]
     * @see [[ts-opt.chain]]
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
     * @see [[ts-opt.act]]
     * @param fs
     */
    act: ActInClassFn<T>;
    /**
     * @alias [[act]]
     * @see [[ts-opt.chainFlow]]
     * @param args
     */
    chainFlow: ActInClassFn<T>;
    /**
     * Combination of [[flatMap]] and [[opt]] functions.
     *
     * @example
     * ```ts
     * some(1).chainToOpt(x => x === 1 ? null : x + 1) // None
     * some(2).chainToOpt(x => x === 1 ? null : x + 1) // Some(3)
     * ```
     *
     * @see [[ts-opt.chainToOpt]]
     *
     * @param f
     */
    chainToOpt<U>(f: (_: T) => U | undefined | null): OptSafe<U>;
    /**
     * Similar to [[act]], but functions return empty values instead of [[Opt]].
     * It is useful for typical JavaScript functions (e.g. lodash), properly handles `undefined`/`null`/`NaN` at any point of the chain.
     *
     * ```ts
     * import {find} from 'lodash/fp';
     *
     * const data = [{}, {f: true, a: [{b: 7, c: 1}]}, {a: [{}]}];
     * opt(data).actToOpt(
     *   find(x => Boolean(x?.f)), // {f: true, a: [{b: 7, c: 1}]}
     *   x => x?.a, // [{b: 7, c: 1}]
     *   find(x => x.b === 8) // undefined
     * ); // None
     * ```
     *
     * @see [[ts-opt.actToOpt]]
     *
     * @param fs
     */
    actToOpt: ActToOptInClassFn<T>;
    /**
     * @alias [[actToOpt]]
     * @see [[ts-opt.chainToOptFlow]]
     * @param args
     */
    chainToOptFlow: ActToOptInClassFn<T>;
    /**
     * Returns value when [[Some]], throws error with `msg` otherwise.
     * @see [[ts-opt.orCrash]]
     * @param msg Error message.
     */
    abstract orCrash(msg: string): T;
    /**
     * Crash when called on [[None]], pass [[Opt]] instance on [[Some]].
     *
     * @example
     * ```ts
     * some(1).someOrCrash('fail') // Some(1)
     * none.someOrCrash('fail') // throws
     * ```
     *
     * @see [[ts-opt.someOrCrash]]
     *
     * @param msg
     */
    abstract someOrCrash(msg: string): Some<T>;
    /**
     * Returns value for [[Some]] or `undefined` for [[None]].
     *
     * @example
     * ```ts
     * some(1).orUndef() // 1
     * none.orUndef() // undefined
     * ```
     * @see [[ts-opt.orUndef]]
     */
    abstract orUndef(): T | undefined;
    /**
     * Returns value for [[Some]] or `null` for [[None]].
     *
     * @example
     * ```ts
     * some(1).orNull() // 1
     * none.orNull() // null
     * ```
     * @see [[ts-opt.orNull]]
     */
    abstract orNull(): T | null;
    /**
     * Returns inner value for [[Some]], `false` for [[None]].
     *
     * @example
     * ```ts
     * some(1).orFalse() // 1
     * none.orFalse() // false
     * ```
     * @see [[ts-opt.orFalse]]
     */
    abstract orFalse(): T | false;
    /**
     * Returns inner value for [[Some]], `true` for [[None]].
     *
     * @example
     * ```ts
     * some(1).orTrue() // 1
     * none.orTrue() // true
     * ```
     * @see [[ts-opt.orTrue]]
     */
    abstract orTrue(): T | true;
    /**
     * Returns inner value for [[Some]], `NaN` for [[None]].
     *
     * @example
     * ```ts
     * some(1).orNaN() // 1
     * none.orNaN() // NaN
     * ```
     * @see [[ts-opt.orNaN]]
     */
    abstract orNaN(): T | number;
    /**
     * Applies appropriate function and returns result from the function.
     *
     * ```ts
     * some(1).caseOf(x => x + 1, () => 0) // 2
     * none.caseOf(x => x + 1, () => 0) // 0
     * ```
     *
     * @see [[onBoth]] for imperative version
     * @see [[ts-opt.caseOf]]
     *
     * @param onSome Processing function for [[Some]].
     * @param onNone Processing function for [[None]].
     */
    abstract caseOf<R>(onSome: (x: T) => R, onNone: () => R): R;
    /**
     * Calls appropriate callback and returns without change current instance of [[Opt]].
     *
     * ```ts
     * // prints 1, returns some(1)
     * some(1).onBoth(x => console.log(x), () => console.log('none'))
     *
     * // prints "none", returns none
     * none.onBoth(x => console.log(x), () => console.log('none'))
     * ```
     *
     * @see [[caseOf]] for functional version
     * @see [[ts-opt.onBoth]]
     * @imperative
     *
     * @param onSome
     * @param onNone
     */
    abstract onBoth(onSome: (x: T) => void, onNone: () => void): Opt<T>;
    /**
     * Calls `f` on [[Some]] with its value, does nothing for [[None]].
     * @imperative
     * @param f
     */
    abstract onSome(f: (x: T) => void): Opt<T>;
    /**
     * Calls `f` on [[None]], does nothing for [[Some]].
     * @imperative
     * @param f
     */
    abstract onNone(f: () => void): Opt<T>;
    /**
     * Applies passed function to this instance and returns function result.
     * Also known as a reverse function application, `|>` (Reason/ReScript, F#, OCaml), `&` (Haskell), `#` (PureScript) or a pipe operator.
     *
     * @example
     * ```ts
     * some(1).pipe(x => x.isEmpty) // false
     * none.pipe(x => x.isEmpty) // true
     * ```
     *
     * Supports multiple functions.
     *
     * @example
     * ```ts
     * opt(1).pipe( // Some(1)
     *   x => x.isEmpty, // false
     *   x => !x, // true
     * ) // true
     * ```
     *
     * @see [[ts-opt.pipe]]
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
     * @see [[ts-opt.contains]]
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
     * @see [[ts-opt.exists]]
     *
     * @param p Predicate.
     */
    abstract exists(p: (x: T) => boolean): boolean;
    /**
     * Applies `p` to inner value and passes result. Always `true` for [[None]].
     *
     * @example
     * ```ts
     * some(0).forAll(x => x > 0) // false
     * some(1).forAll(x => x > 0) // true
     * none.forAll(x => x > 0) // true
     * ```
     * @see [[ts-opt.forAll]]
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
     * @see [[ts-opt.orElse]]
     *
     * @param def Default value.
     */
    abstract orElse(def: T): T;
    /**
     * Less strict version of [[orElse]].
     *
     * @example
     * ```ts
     * opt(1).orElse(true); // TS2345: Argument of type 'boolean' is not assignable to parameter of type 'number'.
     * opt(1).orElseAny(true) // 1 :: number | boolean
     * ```
     *
     * @param def
     */
    abstract orElseAny<U>(def: U): T | U;
    /**
     * Return `this` for [[Some]], `def` for [[None]].
     *
     * @example
     * ```ts
     * none.alt(some(0)) // Some(0)
     * opt(1).alt(some(0)) // Some(1)
     * none.alt(none) // None
     * some(1).alt(none) // Some(1)
     * ```
     *
     * @example It can be used to pick first from given possibly missing alternatives.
     * ```ts
     * type Handler = (_: number) => void;
     * const userHandler: Handler | null = a => console.log('user handling', a);
     * const systemHandler: Handler | null = a => console.log('system handling', a);
     * const backupHandler: Handler | null = a => console.log('backup handling', a);
     * const panicHandler: Handler = a => console.log('PANIC handling', a);
     * const handler =
     *   opt(userHandler)
     *     .alt(opt(systemHandler))
     *     .alt(opt(backupHandler))
     *     .orElse(panicHandler);
     * handler(250 + 64); // prints "user handling 314"
     * ```
     *
     * @param def
     */
    abstract alt(def: Opt<T>): Opt<T>;
    /**
     * Return `this` for [[Some]], `def` wrapped in `opt` for [[None]].
     *
     * @example
     * ```ts
     * const inputNull: number | null = null as number | null;
     * opt(inputNull).altOpt(null) // None
     * opt(inputNull).altOpt(1) // Some(1)
     * opt(2).altOpt(1) // Some(2)
     * ```
     *
     * @param def
     */
    abstract altOpt(def: T | EmptyValue): OptSafe<T>;
    /**
     * Similar to [[caseOf]] but doesn't unwrap value.
     *
     * @example
     * ```ts
     * some(1).bimap(x => x + 1, () => 0) // Some(2)
     * none.bimap(x => x + 1, () => 0) // Some(0)
     * ```
     *
     * @see [[ts-opt.bimap]]
     *
     * @param someF
     * @param noneF
     */
    abstract bimap<U>(someF: (_: T) => U, noneF: () => U): Opt<U>;
    /**
     * Similar to [[bimap]], but accepts functions returning [[Opt]].
     *
     * @example
     * ```ts
     * some(1).flatBimap(x => some(x+1), () => some(0)) // Some(2)
     * none.flatBimap(x => some(x+1), () => some(0)) // Some(0)
     * some(5).flatBimap(x => none, () => some(0)) // None
     * ```
     *
     * @see [[ts-opt.flatBimap]]
     *
     * @param someF
     * @param noneF
     */
    abstract flatBimap<U>(someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
    /**
     * Formats [[Opt]] to string. In case of [[Some]] inner value is converted using `JSON.stringify`.
     *
     * @example
     * ```ts
     * some(1).toString() // 'Some(1)'
     * none.toString() // 'None'
     * ```
     *
     * @see [[ts-opt.toString]]
     */
    abstract toString(): string;
    /**
     * Joins two optional values to a pair. If either of them is [[None]] then the result is [[None]].
     *
     * @example
     * ```ts
     * some(1).zip(some(true)) // Some([1, true])
     * some(1).zip(none) // None
     * none.zip(some(1)) // None
     * none.zip(none) // None
     * ```
     *
     * @example
     * ```ts
     * const formatAddress =
     *   (streetName?: string, streetNumber?: number): string =>
     *     opt(streetName).zip(opt(streetNumber)).map(join(' ')).orElse('');
     * formatAddress('Strawberry', '12') // 'Strawberry 12'
     * formatAddress('Strawberry', undefined) // ''
     * formatAddress(undefined, '12') // ''
     * formatAddress(undefined, undefined) // ''
     * ```
     *
     * @see [[ts-opt.zip]]
     *
     * @param other
     */
    abstract zip<U>(other: Opt<U>): Opt<[T, U]>;
    /**
     * Same as [[zip]], but with one more optional.
     *
     * @example
     * ```ts
     * some(1).zip3(some('a'), some(false)) // Some([1, 'a', false])
     * none.zip3(some(1), some(2)) // None
     * ```
     * @see [[ts-opt.zip3]]
     *
     * @param x
     * @param y
     */
    abstract zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]>;
    /**
     * Same as [[zip3]], but with one more optional.
     * @see [[ts-opt.zip4]]
     * @param x
     * @param y
     * @param z
     */
    abstract zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]>;
    /**
     * Same as [[zip4]], but with one more optional.
     * @see [[ts-opt.zip5]]
     * @param x
     * @param y
     * @param z
     * @param zz
     */
    abstract zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    /**
     * Returns [[Some]] with same value if predicate holds, [[None]] otherwise.
     *
     * @example
     * ```ts
     * opt(1).filter(x => x > 0); // Some(1)
     * opt(-1).filter(x => x > 0); // None
     * ```
     * @see [[noneIf]]
     * @see [[ts-opt.filter]]
     * @param predicate
     */
    abstract filter(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Returns [[None]] if predicate holds, otherwise passes same instance of [[Opt]].
     *
     * @example
     * ```ts
     * opt(1).noneIf(x => x > 0); // None
     * opt(-1).noneIf(x => x > 0); // Some(-1)
     * ```
     * @see [[filter]]
     * @param predicate
     */
    noneIf(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Returns [[None]] when given `true`, otherwise passes opt unchanged.
     *
     * @example
     * ```ts
     * opt(1).noneWhen(false) // Some(1)
     * opt(1).noneWhen(true) // None
     * ```
     *
     * @param returnNone
     */
    noneWhen(returnNone: boolean): Opt<T>;
    /**
     * Returns `0` or `1` for [[Some]] depending on whether the predicate holds.
     * Returns `0` for [[None]].
     *
     * It is a combination of [[Opt.filter]] and [[Opt.length]].
     *
     * @example
     * ```ts
     * opt('Mu').count(x => x.length > 3) // 0
     * opt('Ichi').count(x => x.length > 3) // 1
     * ```
     *
     * @see [[ts-opt.count]]
     * @param predicate
     */
    count(predicate: (_: T) => boolean): 0 | 1;
    /**
     * Narrows type inside [[Opt]] using given type guard.
     *
     * @example
     * ```ts
     * some('1' as string | number).narrow(isString) // Some('1'): Opt<string>
     * some(1 as string | number).narrow(isString) // None: Opt<string>
     * ```
     *
     * @see [[ts-opt.narrow]]
     *
     * @param guard
     */
    abstract narrow<U>(guard: (value: any) => value is U): Opt<U>;
    /**
     * Similar to [[Opt.narrow]], but crashes on a narrowing failure.
     *
     * @see [[Opt.narrow]]
     *
     * @param guard
     * @param crashMessage
     */
    abstract narrowOrCrash<U>(guard: (value: any) => value is U, crashMessage?: string): Opt<U>;
    /**
     * Print value to console.
     *
     * @example
     * ```ts
     * opt(1).print() // logs 'Some:', '1'; returns Some(1)
     * opt(1).print('test') // logs '[test]', 'Some:', '1'; returns Some(1)
     * none.print('x') // logs '[x]', 'None'; returns None
     * ```
     *
     * @see [[ts-opt.print]]
     *
     * @param tag
     */
    abstract print(tag?: string): Opt<T>;
    /**
     * Is a value of this instance and given `other` instance the same?
     * Default comparator function is `===` (referential equality).
     *
     * @example
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
     *
     * @see [[ts-opt.equals]]
     *
     * @param other
     * @param comparator
     */
    abstract equals(other: Opt<T>, comparator?: EqualityFunction): boolean;
    /**
     * Widen union (typically union of strings to string).
     *
     * @experimental May be removed if it is later found out it's unsafe and unfixable.
     *
     * @example
     * ```ts
     * type EnumAB = 'a' | 'b';
     * type EnumABC = 'a' | 'b' | 'c';
     * const ab = 'a' as EnumAB;
     * const abc = 'c' as EnumABC;
     * const correctWiden: Opt<EnumABC> = opt(ab).widen<EnumABC>(); // AB -> ABC: Ok
     * const wrongWiden: Opt<never> = opt(abc).widen<EnumAB>(); // ABC -> AB: Not Ok, C is not in AB
     * ```
     */
    widen<U, R extends SuperUnionOf<U, T> = SuperUnionOf<U, T>>(): Opt<R>;
    /**
     * Maps property of a wrapped object.
     *
     * @example
     * ```ts
     * const a = {x: 1};
     * const xValue = opt(a).prop('x').orCrash('missing prop x'); // 1
     * ```
     *
     * @see [[ts-opt.prop]]
     *
     * @param key
     */
    abstract prop<K extends (T extends object ? keyof T : never)>(key: K): OptSafe<T[K]>;
    /**
     * Constructs a function which returns a value for [[Some]] or an empty value for [[None]] (default is `null`).
     * Optionally takes an empty value as a parameter.
     *
     * @example
     * ```ts
     * opt(1).const()() // 1
     * opt(undefined).const()() // null
     *
     * // custom empty value
     * opt(NaN).const(undefined)() // undefined
     * ```
     */
    const: ConstInClassFn<T>;
    /**
     * Swaps value inside (for [[None]] it's noop).
     *
     * @example
     * ```ts
     * opt(1).swap('a') // Some('a')
     * none.swap('a') // None
     * ```
     *
     * Same as `map(const(newValue))`.
     *
     * @see [[ts-opt.swap]]
     *
     * @param newValue
     */
    abstract swap<U>(newValue: U): Opt<U>;
    /**
     * Get an item at given index of an array/string wrapped in [[Opt]].
     * Resulting value is wrapped in [[Opt]].
     * Non-existent index results in [[None]].
     * Negative index is interpreted as an index from the end of the array (e.g. a last item of an array lies on an `index` equal to `-1`).
     *
     * @example
     * ```ts
     * opt([1]).at(0) // Some(1)
     * opt([]).at(0) // None
     * none.at(0) // None
     * opt([null]).at(0) // None
     * opt([1, 2, 3]).at(-1) // Some(3)
     * opt('Palico').at(0) // Some('P')
     * ```
     *
     * @see [[ts-opt.at]]
     *
     * @param index
     */
    abstract at<R extends (T extends (infer A)[] ? A : (T extends string ? string : never))>(index: number): OptSafe<R>;
    /**
     * Get a first item of an array or a first character of a string.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).head() // Some(1)
     * opt([]).head() // None
     * opt(null).head() // None
     * opt('Palico').head() // Some('P')
     * ```
     *
     * @see [[ts-opt.head]]
     */
    head<R extends (T extends (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R>;
    /**
     * Get minimum from an array.
     *
     * @example
     * ```ts
     * opt([5, 1, 3]).min() // Some(1)
     * none.min() // None
     * opt([]).min() // None
     * ```
     */
    abstract min<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
    /**
     * Get maximum from an array.
     *
     * @example
     * ```ts
     * opt([3, 7]).max() // Some(7)
     * none.max() // None
     * opt([]).max() // None
     * ```
     */
    abstract max<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
    /**
     * Get a last item of an array or a last character of a string.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).last() // Some(3)
     * opt([]).last() // None
     * opt(null).last() // None
     * opt('Palico').last() // Some('o')
     * ```
     *
     * @see [[ts-opt.last]]
     */
    last<R extends (T extends (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R>;
    /**
     * A convenience function to test this (`Opt<string>`) against a given regular expression.
     *
     * @example
     * ```ts
     * opt('a').testReOrFalse(/a/) // true
     * opt('b').testReOrFalse(/a/) // false
     * ```
     *
     * @see [[ts-opt.testReOrFalse]]
     *
     * @param re Regular expression
     */
    testReOrFalse<R extends (T extends string ? boolean : OperationNotAvailable<T, string>)>(re: RegExp): R;
    /**
     * No-op terminator used to end imperative chains.
     *
     * @imperative
     *
     * @example
     * ```ts
     * const f = (x: unknown): void => opt(x).onBoth(noop, noop).end;
     * // same as
     * const g = (x: unknown): void => { opt(x).onBoth(noop, noop); };
     * ```
     */
    get end(): void;
    /**
     * Apply (call) a function inside [[Some]]. Does nothing for [[None]].
     *
     * @example
     * ```ts
     * const add = (a: number, b: number) => a + b;
     * opt(add).apply(2, 3) // Some(5)
     * none.apply(0).orNull() // None
     * ```
     *
     * @example It can also be used with curried functions.
     * ```ts
     * const sub = (a: number) => (b: number) => a - b;
     * opt(sub).apply(10).apply(3) // Some(7)
     * ```
     *
     * @note [[apply]] is only available for functions, otherwise an exception will be thrown when called on [[Some]].
     *
     * @see [[onFunc]] for imperative version
     *
     * @param args Parameters passed to wrapped function.
     * @return `opt`-wrapped result from the function
     */
    apply<//
    R extends (T extends AnyFunc ? ReturnType<T> : OperationNotAvailable<T, AnyFunc>), A extends (T extends AnyFunc ? Parameters<T> : never)>(...args: A): Opt<R>;
    /**
     * Apply (call) a function inside [[Some]]. Does nothing for [[None]].
     *
     * @example Both lines do the same thing
     * ```ts
     * opt(f).onSome(x => x())
     * opt(f).onFunc()
     * ```
     *
     * @example
     * ```ts
     * const g = (a: number, b: number): void => console.log(a, b);
     * opt(g).onFunc(1, 2) // calls `g` (prints 1 and 2), returns Some(g)
     * none.onFunc(79) // None
     * ```
     *
     * @note [[onFunc]] is only available for functions, otherwise an exception will be thrown when called on [[Some]].
     * @imperative
     *
     * @see [[apply]] for functional version
     *
     * @param args
     * @return Unchanged [[Opt]] instance
     */
    onFunc<A extends (T extends AnyFunc ? Parameters<T> : never)>(...args: A): Opt<T>;
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
    someOrCrash(msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(_onSome: (x: T) => R, onNone: () => R): R;
    onBoth(_onSome: (x: T) => void, onNone: () => void): Opt<T>;
    onNone(f: () => void): Opt<T>;
    onSome(_f: (x: T) => void): Opt<T>;
    contains(_x: T): boolean;
    exists(_p: (x: T) => boolean): boolean;
    forAll(_p: (x: T) => boolean): boolean;
    orElse(def: T): T;
    alt(def: Opt<T>): Opt<T>;
    altOpt(def: T | EmptyValue): OptSafe<T>;
    orElseAny<U>(def: U): U;
    bimap<U>(_someF: (_: T) => U, noneF: () => U): Opt<U>;
    flatBimap<U>(_someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
    toString(): string;
    zip<U>(_other: Opt<U>): Opt<[T, U]>;
    zip3<X, Y>(_x: Opt<X>, _y: Opt<Y>): Opt<[T, X, Y]>;
    zip4<X, Y, Z>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>): Opt<[T, X, Y, Z]>;
    zip5<X, Y, Z, ZZ>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>, _zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    filter(_predicate: (_: T) => boolean): Opt<T>;
    narrow<U>(_guard: (value: any) => value is U): Opt<U>;
    narrowOrCrash<U>(guard: (value: any) => value is U, _crashMessage?: string): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, _comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(_key: K): OptSafe<T[K]>;
    swap<U>(_newVal: U): Opt<U>;
    at<R extends (T extends (infer A)[] ? A : (T extends string ? string : never))>(_index: number): OptSafe<R>;
    max<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
    min<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
}
/**
 * [[Opt]] with a value inside.
 * @notExported
 * @see [[Opt]]
 */
declare class Some<T> extends Opt<T> {
    private _value;
    readonly '@@type': symbol;
    get isEmpty(): boolean;
    get value(): T;
    toArray(): [] | [T];
    flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    map<U>(f: (_: T) => U): Opt<U>;
    orCrash(_msg: string): T;
    someOrCrash(_msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R;
    onBoth(onSome: (x: T) => void, _onNone: () => void): Opt<T>;
    contains(x: T): boolean;
    exists(p: (x: T) => boolean): boolean;
    forAll(p: (x: T) => boolean): boolean;
    onNone(_f: () => void): Opt<T>;
    onSome(f: (x: T) => void): Opt<T>;
    orElse(_def: T): T;
    alt(_def: Opt<T>): Opt<T>;
    altOpt(_def: T | EmptyValue): OptSafe<T>;
    orElseAny<U>(_def: U): T;
    bimap<U>(someF: (_: T) => U, _noneF: () => U): Opt<U>;
    flatBimap<U>(someF: (_: T) => Opt<U>, _noneF: () => Opt<U>): Opt<U>;
    toString(): string;
    zip<U>(other: Opt<U>): Opt<[T, U]>;
    zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]>;
    zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]>;
    zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    filter(predicate: (_: T) => boolean): Opt<T>;
    narrow<U>(guard: (value: any) => value is U): Opt<U>;
    narrowOrCrash<U>(guard: (value: any) => value is U, crashMessage?: string): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(key: K): OptSafe<T[K]>;
    swap<U>(newVal: U): Opt<U>;
    at<R extends (T extends (infer A)[] ? A : (T extends string ? string : never))>(index: number): OptSafe<R>;
    min<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
    max<R extends (T extends (infer A)[] ? A : never)>(): OptSafe<R>;
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
 *
 * Warning: Usually it is [[opt]] you are looking for.
 * Only in rare cases you want to have for example `Some(undefined)`.
 * @param x
 */
export declare const some: <T>(x: T) => Readonly<Some<T>>;
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
export declare const opt: <T>(x: T | null | undefined) => OptSafe<T>;
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
export declare const optFalsy: <T>(x: false | "" | 0 | T | null | undefined) => OptSafe<T>;
/**
 * For empty array (`[]`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyArray: <T>(x: T[] | null | undefined) => OptSafe<T[]>;
/**
 * For empty object (`{}`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyObject: <T extends object>(x: T | null | undefined) => OptSafe<T>;
/**
 * For empty string (`''`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optEmptyString: <T>(x: "" | T | null | undefined) => OptSafe<T>;
/**
 * For a number `0` returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export declare const optZero: <T>(x: 0 | T | null | undefined) => OptSafe<T>;
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
export declare const optNegative: (x: number | undefined | null) => OptSafe<number>;
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
export declare const chainToOpt: <T, U>(f: (_: T) => U | null | undefined) => (x: Opt<T>) => OptSafe<U>;
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
/** @see [[Opt.onBoth]] */
export declare const onBoth: <T>(onSome: (x: T) => void) => (onNone: () => void) => (x: Opt<T>) => Opt<T>;
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
/** @see [[Opt.orElseAny]] */
export declare const orElseAny: <U>(e: U) => <T>(x: Opt<T>) => U | T;
/** @see [[Opt.alt]] */
export declare const alt: <T>(def: Opt<T>) => (x: Opt<T>) => Opt<T>;
/** @see [[Opt.altOpt]] */
export declare const altOpt: <T>(def: T) => (x: Opt<T>) => OptSafe<T>;
/** @see [[Opt.bimap]] */
export declare const bimap: <T, U>(someF: (_: T) => U) => (noneF: () => U) => (x: Opt<T>) => Opt<U>;
/** @see [[Opt.flatBimap]] */
export declare const flatBimap: <T, U>(someF: (_: T) => Opt<U>) => (noneF: () => Opt<U>) => (x: Opt<T>) => Opt<U>;
interface ZipFn {
    <T>(other: Opt<T>): <U>(x: Opt<U>) => Opt<[T, U]>;
    <T>(other: T[]): <U>(x: U[]) => [T, U][];
}
/**
 * Same as [[Opt.zip]], but also supports arrays.
 *
 * @example
 * ```ts
 * const formatAddress =
 *   (streetName?: string, streetNumber?: string): string =>
 *     zip(opt(streetName))(opt(streetNumber)).map(join(' ')).orElse('');
 * formatAddress('Strawberry', '12') // 'Strawberry 12'
 * formatAddress('Strawberry', undefined) // ''
 * formatAddress(undefined, '12') // ''
 * formatAddress(undefined, undefined) // ''
 * ```
 *
 * @see [[Opt.zip]]
 */
export declare const zip: ZipFn;
/** @see [[Opt.zip3]] */
export declare const zip3: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => Opt<[T, A, B]>;
/** @see [[Opt.zip4]] */
export declare const zip4: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => Opt<[T, A, B, C]>;
/** @see [[Opt.zip5]] */
export declare const zip5: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>) => Opt<[T, A, B, C, D]>;
declare type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];
/**
 * Same as [[Opt.filter]], but also supports arrays.
 * @see [[Opt.filter]]
 */
export declare const filter: FilterFn;
/** @see [[Opt.noneIf]] */
export declare const noneIf: <T>(predicate: (_: T) => boolean) => (x: Opt<T>) => Opt<T>;
/** @see [[Opt.noneWhen]] */
export declare const noneWhen: <T>(returnNone: boolean) => (x: Opt<T>) => Opt<T>;
declare type CountFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? 0 | 1 : number;
/**
 * Same as [[Opt.count]], but also supports arrays.
 *
 * @example
 * ```ts
 * const greaterThanZero = (x: number) => x > 0;
 *
 * count(greaterThanZero)([-3, 0, 5, 10]) // 2
 * ```
 *
 * @see [[Opt.count]]
 */
export declare const count: CountFn;
/**
 * Find a first item which holds true for a given predicate and return it wrapped in [[Some]].
 * Return [[None]] when no match is found.
 *
 * @example
 * ```ts
 * find((x: number) => x > 0)([-1, 0, 1]) // Some(1)
 * find((x: number) => x > 5)([0, 3, 5]) // None
 * ```
 *
 * @param predicate
 */
export declare const find: <T>(predicate: (_: T) => boolean) => (xs: T[]) => Opt<T>;
/** @see [[Opt.narrow]] */
export declare const narrow: <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>) => Opt<U>;
/** @see [[Opt.narrowOrCrash]] */
export declare const narrowOrCrash: <T, U>(guard: (value: any) => value is U, crashMessage?: string | undefined) => (x: Opt<T>) => Opt<U>;
/**
 * Same as [[Opt.print]], but supports arbitrary argument types.
 * @see [[Opt.print]]
 */
export declare const print: (tag?: string | undefined) => <T>(x: T) => T;
/** @see [[Opt.equals]] */
export declare const equals: <T>(other: Opt<T>, comparator?: EqualityFunction) => (x: Opt<T>) => boolean;
/** @see [[Opt.prop]] */
export declare const prop: <T extends object, K extends T extends object ? keyof T : never = T extends object ? keyof T : never>(key: K) => (x: Opt<T>) => OptSafe<T[K]>;
/** @see [[Opt.swap]] */
export declare const swap: <U>(newValue: U) => <T>(x: Opt<T>) => Opt<U>;
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
declare type CurryTupleFn = <A, B, C>(_: (_: [A, B]) => C) => (_: A) => (_: B) => C;
/**
 * Transforms the given function of two arguments from "tuple curried" format to curried one.
 *
 * ```ts
 * const addPair = ([a, b]: [number, number]) => a + b;
 * opt(1) // Some(1)
 *   .map(
 *     curryTuple(addPair)(4) // same as `(a => b => a + b)(4)`
 *   ) // Some(5)
 * ```
 *
 * @see [[uncurryTuple]]
 * @param f
 */
export declare const curryTuple: CurryTupleFn;
declare type CurryTuple3Fn = <A, B, C, D>(_: (_: [A, B, C]) => D) => (_: A) => (_: B) => (_: C) => D;
/**
 * Transforms the given function of three arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export declare const curryTuple3: CurryTuple3Fn;
declare type CurryTuple4Fn = <A, B, C, D, E>(_: (_: [A, B, C, D]) => E) => (_: A) => (_: B) => (_: C) => (_: D) => E;
/**
 * Transforms the given function of four arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export declare const curryTuple4: CurryTuple4Fn;
declare type CurryTuple5Fn = <A, B, C, D, E, F>(_: (_: [A, B, C, D, E]) => F) => (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F;
/**
 * Transforms the given function of five arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export declare const curryTuple5: CurryTuple5Fn;
declare type UncurryTupleFn = <A, B, C>(_: (_: A) => (_: B) => C) => (_: [A, B]) => C;
/**
 * Transforms the given function of two arguments from curried format to "tuple curried" which can be used with [[Opt.zip]].
 *
 * ```ts
 * const sub = (x: number) => (y: number) => x - y;
 * opt(4) // Some(4)
 *   .zip(opt(1)) // Some([4, 1])
 *   .map(uncurryTuple(sub)) // Some(3)
 * ```
 *
 * @see [[curryTuple]]
 * @param f
 */
export declare const uncurryTuple: UncurryTupleFn;
declare type UncurryTuple3Fn = <A, B, C, D>(_: (_: A) => (_: B) => (_: C) => D) => (_: [A, B, C]) => D;
/**
 * Transforms the given function of three arguments from curried format to "tuple curried" which can be used with [[Opt.zip3]].
 * @see [[uncurryTuple]]
 * @param f
 */
export declare const uncurryTuple3: UncurryTuple3Fn;
declare type UncurryTuple4Fn = <A, B, C, D, E>(_: (_: A) => (_: B) => (_: C) => (_: D) => E) => (_: [A, B, C, D]) => E;
/**
 * Transforms the given function of four arguments from curried format to "tuple curried" which can be used with [[Opt.zip4]].
 * @see [[uncurryTuple]]
 * @param f
 */
export declare const uncurryTuple4: UncurryTuple4Fn;
declare type UncurryTuple5Fn = <A, B, C, D, E, F>(_: (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F) => (_: [A, B, C, D, E]) => F;
/**
 * Transforms the given function of five arguments from curried format to "tuple curried" which can be used with [[Opt.zip5]].
 * @see [[uncurryTuple]]
 * @param f
 */
export declare const uncurryTuple5: UncurryTuple5Fn;
/**
 * Similar to `isEmpty` from lodash, but also supports [[Opt]]s.
 * Returns `true` for [[None]], `[]`, `null`, `undefined`, empty map, empty set, empty object, `''` and `NaN`.
 * Otherwise returns `false`.
 *
 * @example
 * ```ts
 * isEmpty(opt(1)) // false
 * isEmpty(opt(null)) // true
 * isEmpty([]) // true
 * isEmpty([1]) // false
 * isEmpty(null) // true
 * isEmpty('') // true
 * ```
 *
 * @param x
 */
export declare const isEmpty: (x: Opt<unknown> | unknown[] | null | undefined | Map<unknown, unknown> | Set<unknown> | object | string | number) => boolean;
/**
 * Negated version of [[isEmpty]].
 * `nonEmpty(x)` is the same as `!isEmpty(x)`. It can be useful when composing functions (e.g. via [[pipe]]).
 *
 * @example
 * ```ts
 * nonEmpty(opt(2)) // true
 * nonEmpty([]) // false
 *
 * const inc = (x: number) => x + 1;
 * pipe(
 *   a, // Some(4)
 *   map(inc), // Some(5)
 *   nonEmpty, // true
 * ) // true
 * ```
 *
 * @see [[isEmpty]]
 * @param x
 */
export declare const nonEmpty: (x: Opt<unknown> | unknown[] | null | undefined | Map<unknown, unknown> | Set<unknown> | object | string | number) => boolean;
/**
 * Identity function.
 *
 * ```ts
 * id(1) // 1
 * id(null) // null
 * ```
 *
 * @param x
 */
export declare const id: <T>(x: T) => T;
declare type AtFn = <T, R = T extends (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/**
 * Same as [[Opt.at]], but also supports unwrapped arrays.
 * @see [[Opt.at]]
 * @param index
 */
export declare const at: (index: number) => AtFn;
declare type HeadFn = <T, R = T extends (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/**
 * Same as [[Opt.head]], but also supports unwrapped arrays.
 * @see [[Opt.head]]
 * @param x
 */
export declare const head: HeadFn;
declare type LastFn = <T, R = T extends (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/**
 * Same as [[Opt.last]], but also supports unwrapped arrays.
 * @see [[Opt.last]]
 * @param x
 */
export declare const last: LastFn;
interface ZipToOptArrayFn {
    <A, B>(xs: [A, B]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>]>;
    <A, B, C>(xs: [A, B, C]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>]>;
    <A, B, C, D>(xs: [A, B, C, D]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>]>;
    <A, B, C, D, E>(xs: [A, B, C, D, E]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>, WithoutOptValues<E>]>;
}
/**
 * Takes a tuple, wraps each element in [[Opt]] and applies appropriate [[Opt.zip]] function.
 *
 * @example
 * ```ts
 * zipToOptArray([1, null, '', 7, false]) // None: Opt<[number, boolean, string, number, boolean]>
 * zipToOptArray([1, true, '', 7, false]) // Some<[1, true, '', 7, false]>: Opt<[number, boolean, string, number, boolean]>
 * ```
 *
 * Useful as a replacement to `zip*` functions when construction of [[Opt]]s happens in parameters of the function.
 * ```ts
 * zipToOptArray([1, null, '', 7, false])
 * // is same as
 * zip5(opt(1), opt(null), opt(''), opt(7), opt(false))
 * ```
 *
 * @param xs
 */
export declare const zipToOptArray: ZipToOptArrayFn;
/**
 * Test string against regular expression.
 *
 * @example
 * ```ts
 * testRe(/a/)('bac') // true
 * testRe(/a/)('xxx') // false
 *
 * opt('abc').map(testRe(/b/)).orFalse() // false
 * ```
 *
 * @param re
 */
export declare const testRe: (re: RegExp) => (x: string) => boolean;
/** @see [[Opt.testReOrFalse]] */
export declare const testReOrFalse: (re: RegExp) => (x: Opt<string>) => boolean;
/**
 * Runs a given function. Result is wrapped by [[opt]]. Returns [[None]] when the function throws.
 *
 * @example
 * ```ts
 * tryRun(() => 1) // Some(1)
 * tryRun(() => { throw new Error(); }) // None
 * ```
 *
 * @param f
 */
export declare const tryRun: <T>(f: () => T) => Opt<T>;
/**
 * Parses JSON. The result is passed to [[opt]], any error results in [[None]].
 *
 * @example
 * ```ts
 * parseJson('{"a": 1}') // Some({a: 1})
 * parseJson('Ryoka') // None
 * parseJson('null') // None - valid JSON (according to the new standard), but opt(null) is None
 * ```
 *
 * Typical use is to call [[Opt.narrow]] afterwards to validate parsed data and get proper type.
 *
 * @param x
 */
export declare const parseJson: (x: string) => Opt<unknown>;
/**
 * Parses integer (same semantics as `Number.parseInt`).
 * The result is wrapped into [[opt]] (so `NaN` will become [[None]]).
 *
 * @example
 * ```ts
 * parseInt('0') // Some(0)
 * parseInt('gin') // None
 * parseInt('1.1') // Some(1)
 * ```
 *
 * @param x
 */
export declare const parseInt: (x: string) => Opt<number>;
/**
 * Parses float (same semantics as `Number.parseFloat`).
 * The result is wrapped into [[opt]] (so `NaN` will become [[None]]).
 *
 * @example
 * ```ts
 * parseFloat('0') // Some(0)
 * parseFloat('-1.2') // Some(-1.2)
 * parseFloat('xFF') // None
 * ```
 *
 * @param x
 */
export declare const parseFloat: (x: string) => Opt<number>;
/** @see [[Opt.apply]] */
export declare const apply: <T extends AnyFunc, R extends ReturnType<T>, A extends Parameters<T>>(...args: A) => (x: Opt<T>) => Opt<R>;
/** @see [[Opt.onFunc]] */
export declare const onFunc: <T extends AnyFunc, A extends Parameters<T>>(...args: A) => (x: Opt<T>) => Opt<T>;
/**
 * Verify the given value passes the guard. If not, throw an exception.
 *
 * @example
 * ```ts
 * const a = isOrCrash(isNumber)(4 as unknown); // a is of type number, doesn't throw
 * const b: number = a; // ok
 * ```
 *
 * @param guard
 * @param msg
 */
export declare const isOrCrash: <T>(guard: (x: unknown) => x is T, msg?: string) => (x: unknown) => T;
declare type AssertTypeFunc = <T>(x: unknown, guard: (x: unknown) => x is T, msg?: string) => asserts x is T;
/**
 * Asserts a type via a given guard.
 *
 * @example
 * ```ts
 * const a: unknown = 1 as unknown;
 * assertType(a, isNumber);
 * const b: number = a; // ok
 * ```
 *
 * @param x
 * @param guard
 * @param msg
 */
export declare const assertType: AssertTypeFunc;
/** @see [[Opt.min]] */
export declare const min: <R>(x: Opt<R[]>) => OptSafe<R>;
/** @see [[Opt.max]] */
export declare const max: <R>(x: Opt<R[]>) => OptSafe<R>;
export {};
