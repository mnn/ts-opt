import { ActFn, ActInClassFn, ActToOptFn, ActToOptInClassFn, ComposeFn, FlowFn, MapFlowFn, MapFlowInClassFn, PipeFn, PipeInClassFn } from './FlowLike';
export type EqualityFunction = <T>(a: T, b: T) => boolean;
type NotObject<T> = T extends object ? never : T;
type SuperUnionOf<T, U> = Exclude<U, T> extends never ? NotObject<T> : never;
type WithoutOptValues<T> = NonNullable<T>;
type EmptyValue = null | undefined;
type AnyFunc = (...args: any) => any;
declare class OperationNotAvailable<TypeGot, TypeExpected> {
    readonly '@@type': symbol;
    _notUsed1?: TypeGot;
    _notUsed2?: TypeExpected;
}
interface ConstInClassFn<T> {
    (): () => T | null;
    <E>(emptyValue: E): () => T | E;
}
interface FromObjectFn {
    <O extends {
        value: T;
    }, T>(x: O): Opt<O['value']>;
    <K extends keyof O & string, O extends object>(x: O, k: K): Opt<O[K]>;
}
interface ToObjectFn<T> {
    (): ToObjectRes<T>;
    <K extends string>(k: string): Record<K, T | null>;
}
interface ToObjectRes<T> {
    value: T | null;
}
export declare const isString: (x: any) => x is string;
export declare const toString: (x: {
    toString(): string;
}) => string;
export declare const isArray: (x: any) => x is unknown[];
export declare const isReadonlyArray: (x: any) => x is readonly unknown[];
export declare const isFunction: (x: any) => x is Function;
export declare const isObject: (value: any) => value is object;
export declare const isNumber: (x: any) => x is number;
export declare const isUnknown: (_: unknown) => _ is unknown;
/**
 * Generic container class. It either holds exactly one value - {@link Some}, or no value - {@link None} (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent
 * API" in OOP or {@link Opt.pipe|chain of reverse applications} in FP).
 *
 * @typeparam T Wrapped value type.
 */
export declare abstract class Opt<T> {
    /**
     * `false` for {@link Some}, `true` for {@link None}.
     *
     * @see {@link isEmpty}
     */
    abstract get isEmpty(): boolean;
    /**
     * `false` for {@link Some}, `true` for {@link None}.
     *
     * If you need to narrow a type to {@link Some}, use {@link Opt.isSome}.
     */
    get nonEmpty(): boolean;
    /**
     * @alias {@link Opt.nonEmpty}
     */
    get isFull(): boolean;
    /**
     * Is this instance of {@link Some}?
     */
    isSome(): this is Some<T>;
    /**
     * Is this instance of {@link None}?
     */
    isNone(): this is None<T>;
    /**
     * `1` for {@link Some}, `0` for {@link None}.
     *
     * Important: This is not the wrapped value's length.
     * E.g., `opt([1,2,3]).length === 1`.
     * Use {@link Opt.lengthIn} for array/string length of the wrapped value.
     */
    get length(): 0 | 1;
    /**
     * Returns the length of a string or array wrapped in an `Opt`.
     *
     * @example
     * ```ts
     * opt('hello').lengthIn() // Some(5)
     * opt([1, 2, 3]).lengthIn() // Some(3)
     * opt('').lengthIn() // Some(0)
     * opt([]).lengthIn() // Some(0)
     * none.lengthIn() // None
     * ```
     */
    abstract lengthIn<R extends (T extends string | readonly unknown[] ? Opt<number> : never)>(this: Opt<string | readonly unknown[]>): R;
    /**
     * Create `Opt` instance from an array of one or zero items.
     *
     * ```ts
     * Opt.fromArray([]) // None
     * Opt.fromArray([1]) // Some(1)
     * ```
     *
     * @param x
     */
    static fromArray<T>(x: readonly [] | readonly [T]): Opt<T>;
    /**
     * Converts `Opt` to an array.
     *
     * @example
     * ```ts
     * some(1).toArray() // [1]
     * none.toArray() // []
     * ```
     * @see {@link toArray}
     */
    abstract toArray(): [] | [T];
    /**
     * Converts an object to {@link Opt}.
     *
     * @example
     * ```ts
     * Opt.fromObject({value: 4}) // Some(4)
     * Opt.fromObject({id: 4, something: '?'}, 'id') // Some(4)
     * Opt.fromObject({value: null}) // None
     * ```
     *
     * @param x
     * @param k
     */
    static fromObject: FromObjectFn;
    /**
     * Converts {@link Opt} to an object.
     *
     * @example
     * ```ts
     * opt(1).toObject() // {value: 1}
     * opt(undefined).toObject() // {value: null}
     * opt(undefined).toObject('id') // {id: null}
     * ```
     *
     * @param k
     */
    toObject: ToObjectFn<T>;
    /**
     * Serializes {@link Opt} to a plain JavaScript object.
     * @see {@link serialize}
     */
    serialize(): OptSerialized<T>;
    /**
     * Deserializes {@link Opt} from a plain JavaScript object.
     * @see {@link deserialize}
     */
    static deserialize<T>(x: unknown, guard: (x: unknown) => x is T): DeserializationResult<T>;
    /**
     * Applies function to the wrapped value and returns a new instance of {@link Some}.
     *
     * ```
     * some(1).map(x => x + 1) // Some(2)
     * none.map(x => x + 1) // None
     * ```
     *
     * @see {@link onSome} for imperative variant (for use with callback)
     * @see {@link map}
     *
     * @param f
     */
    abstract map<U>(f: (_: T) => U): Opt<U>;
    /**
     * Applies a map function to an array inside.
     * @example
     * ```ts
     * opt([1, 2, 3]).mapIn(x => x + 1) // Some([2, 3, 4])
     * ```
     */
    abstract mapIn<U, R>(this: Opt<U[]>, f: (x: U) => R): Opt<R[]>;
    /**
     * Similar to {@link Opt.map}, but supports more functions which are called in succession, each on a result of a previous one.
     *
     * @example
     * ```ts
     * const sq = (x: number) => x * x;
     * const dec = (x: number) => x - 1;
     * opt(4).mapFlow(sq, dec) // Some(15)
     * opt(null).mapFlow(sq, dec) // None
     * ```
     * @see {@link mapFlow}
     * @param fs
     */
    mapFlow: MapFlowInClassFn<T>;
    /**
     * Maps over characters in a string wrapped in an Opt.
     * @example
     * ```ts
     * opt('hello').mapStr(c => c.toUpperCase()) // Some('HELLO')
     * opt('').mapStr(c => c.toUpperCase()) // Some('')
     * none.mapStr(c => c.toUpperCase()) // None
     * ```
     * @see {@link mapStr}
     */
    abstract mapStr(this: Opt<string>, f: (c: string) => string): Opt<string>;
    /**
     * Similar to {@link Opt.map}, but function is expected to return {@link Opt} which will be returned.
     * Useful for including steps which may fail or return no value.
     *
     * ```
     * some(1).flatMap(x => x === 1 ? none : some(x + 1)) // None
     * none.flatMap(x => some(1)) // None
     * ```
     *
     * @see {@link flatMap}
     * @param f
     */
    abstract flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    /**
     * @alias {@link Opt.flatMap}
     * @see {@link chain}
     * @param f
     */
    chain<U>(f: (_: T) => Opt<U>): Opt<U>;
    /**
     * Applies a `flatMap` function to an array inside.
     * @example
     * ```ts
     * opt([1, 2]).flatMapIn(x => [x, x * 2]) // Some([1, 2, 2, 4])
     * ```
     */
    abstract flatMapIn<U, R>(this: Opt<U[]>, f: (x: U) => R[]): Opt<R[]>;
    /**
     * @alias {@link Opt.flatMapIn}
     */
    chainIn<U, R>(this: Opt<U[]>, f: (x: U) => R[]): Opt<R[]>;
    /**
     * Similar to {@link Opt.chain} (in other languages called `bind` or `>>=`), but supports more functions passed at once (resembles `do` notation in Haskell).
     * It is used to model a sequence of operations where each operation can fail (can return {@link None}).
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
     * @see {@link act}
     * @param fs
     */
    act: ActInClassFn<T>;
    /**
     * @alias {@link Opt.act}
     * @see {@link chainFlow}
     * @param args
     */
    chainFlow: ActInClassFn<T>;
    /**
     * Combination of {@link Opt.flatMap} and {@link opt} functions.
     *
     * @example
     * ```ts
     * some(1).chainToOpt(x => x === 1 ? null : x + 1) // None
     * some(2).chainToOpt(x => x === 1 ? null : x + 1) // Some(3)
     * ```
     *
     * @see {@link chainToOpt}
     *
     * @param f
     */
    chainToOpt<U>(f: (_: T) => U | undefined | null): OptSafe<U>;
    /**
     * Similar to {@link act}, but functions return empty values instead of {@link Opt}.
     * It is useful for typical JavaScript functions (e.g. lodash), properly handles `undefined`/`null`/`NaN` at any point of the chain.
     *
     * @example
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
     * @see {@link actToOpt}
     *
     * @param fs
     */
    actToOpt: ActToOptInClassFn<T>;
    /**
     * @alias {@link Opt.actToOpt}
     * @see {@link chainToOptFlow}
     * @param args
     */
    chainToOptFlow: ActToOptInClassFn<T>;
    /**
     * Joins (flattens) nested Opt instance, turning an `Opt<Opt<T>>` into an `Opt<T>`.
     * This is equivalent to calling `flatMap` with the identity function: `.join()` ~ `.flatMap(id)`.
     *
     * @example
     * ```ts
     * const nestedOpt: Opt<Opt<number>> = opt(opt(42)); // Some(Some(42))
     * const flattenedOpt: Opt<number> = nestedOpt.join(); // Some(42)
     * ```
     */
    join<U>(this: Opt<Opt<U>>): Opt<U>;
    /**
     * Returns value when {@link Some}, throws error with `msg` otherwise.
     *
     * @example
     * ```ts
     * opt(null).orCrash('unexpected empty value') // crashes with Error('unexpected empty value')
     * opt(1).orCrash('unexpected empty value') // 1
     * ```
     *
     * @see {@link orCrash}
     * @param msg Error message.
     */
    abstract orCrash(msg: string): T | never;
    /**
     * Crash when called on {@link None}, pass {@link Opt} instance on {@link Some}.
     *
     * @example
     * ```ts
     * some(1).someOrCrash('fail') // Some(1)
     * none.someOrCrash('fail') // throws
     * ```
     *
     * @see {@link someOrCrash}
     *
     * @param msg
     */
    abstract someOrCrash(msg: string): Some<T>;
    /**
     * Returns value for {@link Some} or `undefined` for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orUndef() // 1
     * none.orUndef() // undefined
     * ```
     * @see {@link orUndef}
     */
    abstract orUndef(): T | undefined;
    /**
     * Returns value for {@link Some} or `null` for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orNull() // 1
     * none.orNull() // null
     * ```
     * @see {@link orNull}
     */
    abstract orNull(): T | null;
    /**
     * Returns inner value for {@link Some}, `false` for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orFalse() // 1
     * none.orFalse() // false
     * ```
     * @see {@link Opt.exists}
     * @see {@link orFalse}
     */
    abstract orFalse(): T | false;
    /**
     * Returns inner value for {@link Some}, `true` for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orTrue() // 1
     * none.orTrue() // true
     * ```
     * @see {@link Opt.forAll}
     * @see {@link orTrue}
     */
    abstract orTrue(): T | true;
    /**
     * Returns inner value for {@link Some}, `NaN` for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orNaN() // 1
     * none.orNaN() // NaN
     * ```
     * @see {@link orNaN}
     */
    abstract orNaN(): T | number;
    /**
     * Applies appropriate function and returns result from the function.
     *
     * @example
     * ```ts
     * some(1).caseOf(x => x + 1, () => 0) // 2
     * none.caseOf(x => x + 1, () => 0) // 0
     * ```
     *
     * @see {@link Opt.onBoth} for imperative version
     * @see {@link caseOf}
     *
     * @param someCase Processing function for {@link Some}.
     * @param noneCase Processing function for {@link None}.
     */
    abstract caseOf<R>(someCase: (x: T) => R, noneCase: () => R): R;
    /**
     * Reduces the `Opt` instance to a single value by applying a function to the value inside {@link Some}
     * or returning a default value for {@link None}.
     *
     * @example
     * ```ts
     * opt(1).fold(x => x + 1, 0) // 2
     * none.fold(x => x + 1, 0) // 0
     * ```
     *
     * @param someCase Function to apply to the value inside `Some`.
     * @param noneCase Default value to return for `None`.
     */
    abstract fold<R>(someCase: (x: T) => R, noneCase: R): R;
    /**
     * Applies a reducer function to an array within an {@link Opt} instance,
     * combining its elements into a single value using the array's `reduce` method.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).foldIn((acc, x) => acc + x, 0) // Some(6)
     * none.foldIn((acc, x) => acc + x, 0) // None
     * ```
     */
    foldIn<U, R>(this: Opt<U[]>, f: (acc: R, x: U) => R, initial: R): Opt<R>;
    /**
     * Calls appropriate callback and returns without change current instance of {@link Opt}.
     *
     * @example
     * ```ts
     * // prints 1, returns some(1)
     * some(1).onBoth(x => console.log(x), () => console.log('none'))
     *
     * // prints "none", returns none
     * none.onBoth(x => console.log(x), () => console.log('none'))
     * ```
     *
     * @see {@link Opt.caseOf} for functional version
     * @see {@link onBoth}
     * @imperative
     *
     * @param onSome
     * @param onNone
     */
    abstract onBoth(onSome: (x: T) => void, onNone: () => void): Opt<T>;
    /**
     * Calls `f` on {@link Some} with its value, does nothing for {@link None}.
     * @imperative
     * @param f
     */
    abstract onSome(f: (x: T) => void): Opt<T>;
    /**
     * Calls `f` on {@link None}, does nothing for {@link Some}.
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
     * @see {@link pipe}
     *
     * @param fs Functions in call chain
     */
    pipe: PipeInClassFn<T>;
    /**
     * Compares inner value with given value using `===`. Always `false` for {@link None}.
     *
     * ```ts
     * some(1).contains(1) // true
     * some(0).contains(1) // false
     * none.contains(undefined) // false
     * ```
     *
     * Similar to JavaScript `Array#includes` method.
     *
     * @see {@link contains}
     *
     * @param x
     */
    abstract contains(x: T): boolean;
    /** @alias {@link Opt.contains} */
    has(x: T): boolean;
    /**
     * Checks if an array inside the Opt contains the given element.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).hasIn(2) // true
     * opt([1, 2, 3]).hasIn(4) // false
     * none.hasIn(1) // false
     * ```
     *
     * @param x Element to search for.
     */
    abstract hasIn<U>(this: Opt<readonly U[]>, x: U): boolean;
    /** @alias {@link Opt.hasIn} */
    containsIn<U>(this: Opt<readonly U[]>, x: U): boolean;
    /**
     * Applies `p` to inner value and passes result. Always `false` for {@link None}.
     *
     * ```ts
     * some(0).exists(x => x > 0) // false
     * some(1).exists(x => x > 0) // true
     * none.exists(x => x > 0) // false
     * ```
     *
     * @see {@link exists}
     *
     * @param p Predicate.
     */
    abstract exists(p: (x: T) => boolean): boolean;
    /**
     * Checks if any element in the array inside the Opt satisfies the predicate.
     * Similar to `Array.some` from JavaScript.
     *
     * @example
     * ```ts
     * opt([1]).existsIn(x => x > 0) // Some(true)
     * opt([-1]).existsIn(x => x > 0) // Some(false)
     * opt([]).existsIn(x => x > 0) // Some(false)
     * none.existsIn(x => x > 0) // None
     * ```
     */
    abstract existsIn<U>(this: Opt<U[]>, p: (x: U) => boolean): Opt<boolean>;
    /**
     * Applies `p` to inner value and passes result. Always `true` for {@link None}.
     *
     * @example
     * ```ts
     * some(0).forAll(x => x > 0) // false
     * some(1).forAll(x => x > 0) // true
     * none.forAll(x => x > 0) // true
     * ```
     * @see {@link forAll}
     * @param p Predicate.
     */
    abstract forAll(p: (x: T) => boolean): boolean;
    /**
     * Checks if all elements in the array inside the Opt satisfy the predicate.
     * Similar to `Array.every` from JavaScript.
     *
     * @example
     * ```ts
     * opt([1]).forAllIn(x => x > 0) // Some(true)
     * opt([-1]).forAllIn(x => x > 0) // Some(false)
     * opt([]).forAllIn(x => x > 0) // Some(true)
     * none.forAllIn(x => x > 0) // None
     * ```
     */
    abstract forAllIn<U>(this: Opt<U[]>, p: (x: U) => boolean): Opt<boolean>;
    /**
     * Get inner value of {@link Some}, or use passed `def` value for {@link None}.
     *
     * @example
     * ```ts
     * some(1).orElse(2) // 1
     * none.orElse(2) // 2
     * ```
     * @see {@link orElse}
     *
     * @param def Default value.
     */
    abstract orElse(def: T): T;
    /**
     * Get inner value of {@link Some}, or lazily use passed `def` value for {@link None}.
     *
     * If a computation of a default value is not expensive (or has been already computed),
     * use {@link orElse} instead.
     *
     * @example
     * ```ts
     * some(1).orElseLazy(() => 2) // 1
     * none.orElseLazy(() => 2) // 2
     * ```
     *
     * @see {@link orElseLazy}
     *
     * @param def
     */
    abstract orElseLazy(def: () => T): T;
    /**
     * Less strict version of {@link orElse}.
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
     * Return `this` for {@link Some}, `def` for {@link None}.
     * It represents an alternative on {@link Opt}s.
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
     * Return `this` for {@link Some}, `def` wrapped in `opt` for {@link None}.
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
     * Similar to {@link caseOf} but doesn't unwrap value.
     *
     * @example
     * ```ts
     * some(1).bimap(x => x + 1, () => 0) // Some(2)
     * none.bimap(x => x + 1, () => 0) // Some(0)
     * ```
     *
     * @see {@link bimap}
     *
     * @param someF
     * @param noneF
     */
    abstract bimap<U>(someF: (_: T) => U, noneF: () => U): Opt<U>;
    /**
     * Similar to {@link Opt.bimap}, but accepts functions returning {@link Opt}.
     *
     * @example
     * ```ts
     * some(1).flatBimap(x => some(x+1), () => some(0)) // Some(2)
     * none.flatBimap(x => some(x+1), () => some(0)) // Some(0)
     * some(5).flatBimap(x => none, () => some(0)) // None
     * ```
     *
     * @see {@link flatBimap}
     *
     * @param someF
     * @param noneF
     */
    abstract flatBimap<U>(someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
    /**
     * Formats {@link Opt} to string. In case of {@link Some} inner value is converted using `JSON.stringify`.
     *
     * @example
     * ```ts
     * some(1).toString() // 'Some(1)'
     * none.toString() // 'None'
     * ```
     *
     * @see {@link toString}
     */
    abstract toString(): string;
    /**
     * Joins two optional values to a pair. If either of them is {@link None} then the result is {@link None}.
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
     * @see {@link zip}
     *
     * @param other
     */
    abstract zip<U>(other: Opt<U>): Opt<[T, U]>;
    /**
     * Same as {@link Opt.zip}, but with one more optional.
     *
     * @example
     * ```ts
     * some(1).zip3(some('a'), some(false)) // Some([1, 'a', false])
     * none.zip3(some(1), some(2)) // None
     * ```
     * @see {@link zip3}
     *
     * @param x
     * @param y
     */
    abstract zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]>;
    /**
     * Same as {@link Opt.zip3}, but with one more optional.
     * @see {@link zip4}
     * @param x
     * @param y
     * @param z
     */
    abstract zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]>;
    /**
     * Same as {@link Opt.zip4}, but with one more optional.
     * @see {@link zip5}
     * @param x
     * @param y
     * @param z
     * @param zz
     */
    abstract zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]>;
    /**
     * Zips each element of an array inside the {@link Opt} with the corresponding element from another array.
     * If the arrays are of different lengths, the resulting array will have the length of the shorter one.
     * @example
     * ```ts
     * opt([1, 2]).zipIn([3, 4]) // Some([[1, 3], [2, 4]])
     * opt([1, 2]).zipIn(null) // None
     * none.zipIn([1, 2]) // None
     * ```
     */
    abstract zipIn<U, V>(this: Opt<readonly U[]>, other: V[] | EmptyValue): Opt<readonly [U, V][]>;
    /**
     * Returns {@link Some} with same value if predicate holds, {@link None} otherwise.
     *
     * @example
     * ```ts
     * opt(1).filter(x => x > 0); // Some(1)
     * opt(-1).filter(x => x > 0); // None
     * ```
     * @see {@link noneIf}
     * @see {@link filter}
     * @param predicate
     */
    abstract filter(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Filters each element of an array inside the {@link Opt}, returning an {@link Opt} of the array with elements that pass the test implemented by the provided function.
     * @example
     * ```ts
     * opt([1, 2, 3]).filterIn(x => x > 1) // Some([2, 3])
     * ```
     */
    abstract filterIn<U>(this: Opt<U[]>, f: (x: U) => boolean): Opt<U[]>;
    /**
     * Filter by regular expression.
     * It is a shortcut function for {@link Opt.filter} + {@link testRe}.
     *
     * @example
     * ```ts
     * opt('Luffy').filterByRe(/f+/) // Some('Luffy')
     * opt('Robin').filterByRe(/f+/) // None
     * ```
     *
     * @param regex
     */
    filterByRe<R extends (T extends string ? Opt<string> : never)>(regex: RegExp): R;
    /**
     * Searches for an element within an array inside the Opt instance that matches the given predicate.
     *
     * This method will return a new Opt containing the first element that satisfies the predicate function.
     * If the Opt instance is None or the value inside is not an array, it will return None.
     *
     * @template U - The type of elements in the array.
     * @param {Opt<U[]>} this - The Opt instance containing an array.
     * @param {(x: U) => boolean} f - A predicate function to test each element.
     * @returns {Opt<U>} A new Opt containing the found element or None if no element matches the predicate or if the Opt is None.
     *
     * @example
     * ```typescript
     * opt([1, 2, 3, 4]).findIn(x => x > 2) // Some(3)
     * opt([1, 2, 3, 4]).findIn(x => x > 5) // None
     * none.findIn(x => x > 2) // None
     * ```
     */
    findIn<U>(this: Opt<U[]>, f: (x: U) => boolean): Opt<U>;
    /**
     * Returns {@link None} if predicate holds, otherwise passes same instance of {@link Opt}.
     *
     * @example
     * ```ts
     * opt(1).noneIf(x => x > 0); // None
     * opt(-1).noneIf(x => x > 0); // Some(-1)
     * ```
     * @see {@link filter}
     * @param predicate
     */
    noneIf(predicate: (_: T) => boolean): Opt<T>;
    /**
     * Returns {@link None} if opt holds a value for which {@link isEmpty} returns `true`, otherwise passes opt unchanged.
     *
     * @example
     * ```ts
     * opt('x').noneIfEmpty() // Some('x')
     * opt('').noneIfEmpty() // None
     * opt([]).noneIfEmpty() // None
     * opt({}).noneIfEmpty() // None
     * ```
     */
    noneIfEmpty(this: Opt<PossiblyEmpty>): Opt<WithoutPossiblyEmptyEmptyValues<T>>;
    /**
     * Returns {@link None} when given `true`, otherwise passes opt unchanged.
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
     * Returns `0` or `1` for {@link Some} depending on whether the predicate holds.
     * Returns `0` for {@link None}.
     *
     * It is a combination of {@link Opt.filter} and {@link Opt.length}.
     *
     * @example
     * ```ts
     * opt('Mu').count(x => x.length > 3) // 0
     * opt('Ichi').count(x => x.length > 3) // 1
     * ```
     *
     * @see {@link count}
     * @param predicate
     */
    count(predicate: (_: T) => boolean): 0 | 1;
    /**
     * Counts the number of elements in the array inside the Opt that satisfy the predicate.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).countIn(x => x > 1) // Some(2)
     * opt([]).countIn(x => x > 1) // Some(0)
     * none.countIn(x => x > 1) // None
     * ```
     *
     * @param f The predicate function to test each element.
     * @returns An Opt containing the count of elements that satisfy the predicate, or None if the Opt is None.
     */
    abstract countIn<U>(this: Opt<U[]>, f: (x: U) => boolean): Opt<number>;
    /**
     * Narrows type inside {@link Opt} using given type guard.
     *
     * @example
     * ```ts
     * some('1' as string | number).narrow(isString) // Some('1'): Opt<string>
     * some(1 as string | number).narrow(isString) // None: Opt<string>
     * ```
     *
     * @see {@link narrow}
     *
     * @param guard
     */
    abstract narrow<U>(guard: (value: any) => value is U): Opt<U>;
    /**
     * Similar to {@link Opt.narrow}, but crashes on a narrowing failure.
     *
     * @see {@link Opt.narrow}
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
     * @see {@link print}
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
     * @see {@link equals}
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
     * @see {@link prop}
     * @see {@link Opt.propOrCrash}
     *
     * @param key
     */
    abstract prop<K extends (T extends object ? keyof T : never)>(key: K): OptSafe<T[K]>;
    /**
     * Get a field from a wrapped object. Crash if the field is missing or empty, or opt instance is {@link None}.
     * Shortcut of {@link Opt.prop} + {@link Opt.orCrash}.
     *
     * @param key
     */
    propOrCrash<//
    K extends (T extends object ? keyof T : never), //
    R extends (T extends object ? WithoutOptValues<T[K]> | never : never)>(key: K): R;
    /**
     * Constructs a function which returns a value for {@link Some} or an empty value for {@link None} (default is `null`).
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
     * Swaps value inside (for {@link None} it's noop).
     *
     * @example
     * ```ts
     * opt(1).swap('a') // Some('a')
     * none.swap('a') // None
     * ```
     *
     * Same as `map(const(newValue))`.
     *
     * @see {@link swap}
     *
     * @param newValue
     */
    abstract swap<U>(newValue: U): Opt<U>;
    /**
     * Get an item at given index of an array/string wrapped in {@link Opt}.
     * Resulting value is wrapped in {@link Opt}.
     * Non-existent index results in {@link None}.
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
     * @see {@link at}
     *
     * @param index
     */
    abstract at<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(index: number): OptSafe<R>;
    /**
     * Get a first item of an array or a first character of a string wrapped in {@link Opt}.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).headIn() // Some(1)
     * opt([]).headIn() // None
     * opt(null).headIn() // None
     * opt('Palico').headIn() // Some('P')
     * ```
     *
     * @see {@link head}
     */
    headIn<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R>;
    /**
     * Get minimum from an array.
     *
     * @example
     * ```ts
     * opt([5, 1, 3]).minIn() // Some(1)
     * none.minIn() // None
     * opt([]).minIn() // None
     * ```
     */
    abstract minIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
    /**
     * Get maximum from an array.
     *
     * @example
     * ```ts
     * opt([3, 7]).maxIn() // Some(7)
     * none.maxIn() // None
     * opt([]).maxIn() // None
     * ```
     */
    abstract maxIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
    /**
     * Get a last item of an array or a last character of a string.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).lastIn() // Some(3)
     * opt([]).lastIn() // None
     * opt(null).lastIn() // None
     * opt('Palico').lastIn() // Some('o')
     * ```
     *
     * @see {@link last}
     */
    lastIn<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R>;
    /**
     * A convenience function to test this (`Opt<string>`) against a given regular expression.
     *
     * @example
     * ```ts
     * opt('a').testReOrFalse(/a/) // true
     * opt('b').testReOrFalse(/a/) // false
     * ```
     *
     * @see {@link testReOrFalse}
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
     * Apply (call) a function inside {@link Some}. Does nothing for {@link None}.
     *
     * @example
     * ```ts
     * const add = (a: number, b: number) => a + b;
     * opt(add).apply(2, 3) // Some(5)
     * none.apply(0) // None
     * ```
     *
     * @example It can also be used with curried functions.
     * ```ts
     * const sub = (a: number) => (b: number) => a - b;
     * opt(sub).apply(10).apply(3) // Some(7)
     * ```
     *
     * @note {@link Opt.apply} is only available for functions, otherwise an exception will be thrown when called on {@link Some}.
     *
     * @see {@link Opt.onFunc} for imperative version
     *
     * @param args Parameters passed to wrapped function.
     * @return `opt`-wrapped result from the function
     */
    apply<//
    R extends (T extends AnyFunc ? ReturnType<T> : OperationNotAvailable<T, AnyFunc>), A extends (T extends AnyFunc ? Parameters<T> : never)>(...args: A): Opt<R>;
    /**
     * Apply (call) a function inside {@link Some}. Does nothing for {@link None}.
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
     * @note {@link onFunc} is only available for functions, otherwise an exception will be thrown when called on {@link Some}.
     * @imperative
     *
     * @see {@link Opt.apply} for functional version
     *
     * @param args
     * @return Unchanged {@link Opt} instance
     */
    onFunc<A extends (T extends AnyFunc ? Parameters<T> : never)>(...args: A): Opt<T>;
}
/**
 * Empty {@link Opt}.
 * @notExported
 * @see {@link opt}
 * @see {@link none}
 */
declare class None<T> extends Opt<T> {
    readonly '@@type': symbol;
    get isEmpty(): boolean;
    toArray(): [] | [T];
    lengthIn<R extends (T extends string | readonly unknown[] ? Opt<number> : never)>(): R;
    flatMap<U>(_f: (_: T) => Opt<U>): Opt<U>;
    flatMapIn<U, R>(this: None<U[]>, _f: (x: U) => R[]): Opt<R[]>;
    map<U>(): Opt<U>;
    mapIn<R, U>(this: None<U[]>, _f: (x: U) => R): Opt<R[]>;
    mapStr(this: None<string>, _f: (c: string) => string): None<string>;
    orCrash(msg: string): T;
    someOrCrash(msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(_onSome: (x: T) => R, onNone: () => R): R;
    fold<R>(_someCase: (x: T) => R, noneCase: R): R;
    onBoth(_onSome: (x: T) => void, onNone: () => void): Opt<T>;
    onNone(f: () => void): Opt<T>;
    onSome(_f: (x: T) => void): Opt<T>;
    contains(_x: T): boolean;
    hasIn<U>(_: U): boolean;
    exists(_p: (x: T) => boolean): boolean;
    existsIn<U>(_p: (x: U) => boolean): Opt<boolean>;
    forAll(_p: (x: T) => boolean): boolean;
    forAllIn<U>(_p: (x: U) => boolean): Opt<boolean>;
    orElse(def: T): T;
    orElseLazy(def: () => T): T;
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
    filterIn<U>(this: None<U[]>, _f: (x: U) => boolean): Opt<U[]>;
    zipIn<U, V>(this: Opt<readonly U[]>, _other: V[] | EmptyValue): Opt<readonly [U, V][]>;
    countIn<U>(this: None<U[]>, _f: (x: U) => boolean): Opt<number>;
    narrow<U>(_guard: (value: any) => value is U): Opt<U>;
    narrowOrCrash<U>(guard: (value: any) => value is U, _crashMessage?: string): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, _comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(_key: K): OptSafe<T[K]>;
    swap<U>(_newVal: U): Opt<U>;
    at<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(_index: number): OptSafe<R>;
    maxIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
    minIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
}
/**
 * {@link Opt} with a value inside.
 * @notExported
 * @see {@link Opt}
 */
declare class Some<T> extends Opt<T> {
    private _value;
    readonly '@@type': symbol;
    get isEmpty(): boolean;
    get value(): T;
    lengthIn<R extends (T extends string | readonly unknown[] ? Opt<number> : never)>(): R;
    toArray(): [] | [T];
    flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    flatMapIn<U, R>(this: Some<U[]>, f: (x: U) => R[]): Opt<R[]>;
    map<U>(f: (_: T) => U): Opt<U>;
    mapIn<R, U>(this: Some<U[]>, f: (x: U) => R): Opt<R[]>;
    mapStr(this: Some<string>, f: (c: string) => string): Some<string>;
    orCrash(_msg: string): T;
    someOrCrash(_msg: string): Some<T>;
    orNull(): T | null;
    orUndef(): T | undefined;
    orFalse(): false | T;
    orTrue(): true | T;
    orNaN(): number | T;
    caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R;
    fold<R>(someCase: (x: T) => R, _noneCase: R): R;
    onBoth(onSome: (x: T) => void, _onNone: () => void): Opt<T>;
    contains(x: T): boolean;
    hasIn<U>(this: Opt<readonly U[]>, x: U): boolean;
    exists(p: (x: T) => boolean): boolean;
    existsIn<U>(this: Some<U[]>, p: (x: U) => boolean): Opt<boolean>;
    forAll(p: (x: T) => boolean): boolean;
    forAllIn<U>(this: Some<U[]>, p: (x: U) => boolean): Opt<boolean>;
    onNone(_f: () => void): Opt<T>;
    onSome(f: (x: T) => void): Opt<T>;
    orElse(_def: T): T;
    orElseLazy(_def: () => T): T;
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
    filterIn<U>(this: Some<U[]>, f: (x: U) => boolean): Opt<U[]>;
    zipIn<U, V>(this: Opt<readonly U[]>, other: V[] | EmptyValue): Opt<readonly [U, V][]>;
    countIn<U>(this: Some<U[]>, f: (x: U) => boolean): Opt<number>;
    narrow<U>(guard: (value: any) => value is U): Opt<U>;
    narrowOrCrash<U>(guard: (value: any) => value is U, crashMessage?: string): Opt<U>;
    print(tag?: string): Opt<T>;
    equals(other: Opt<T>, comparator?: EqualityFunction): boolean;
    prop<K extends (T extends object ? keyof T : never)>(key: K): OptSafe<T[K]>;
    swap<U>(newVal: U): Opt<U>;
    at<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(index: number): OptSafe<R>;
    minIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
    maxIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R>;
}
declare const someSerializedType = "Opt/Some";
declare const noneSerializedType = "Opt/None";
export type NoneSerialized = {
    type: typeof noneSerializedType;
};
export type SomeSerialized<T> = {
    type: typeof someSerializedType;
    value: T;
};
/**
 * Represents a serialized {@link Opt} type, which can be either {@link NoneSerialized} or {@link SomeSerialized}.
 */
export type OptSerialized<T> = NoneSerialized | SomeSerialized<T>;
export declare const isOptSerialized: (x: unknown) => x is OptSerialized<unknown>;
/**
 * A helper class for providing compatibility with Redux DevTools.
 */
export declare class ReduxDevtoolsCompatibilityHelper {
    static replacer(_key: unknown, value: any): any | OptSerialized<unknown>;
    static reviver(_key: unknown, value: any): any;
}
/**
 * Serializes an Opt instance to a plain JavaScript object.
 *
 * @example
 * ```ts
 * serialize(none) // { type: 'Opt/None' }
 * serialize(opt(1)) // { type: 'Opt/Some', value: 1 }
 * ```
 *
 * @param x {@link Opt} instance to serialize
 * @returns serialized Opt instance as an {@link OptSerialized} object
 */
export declare const serialize: <T>(x: Opt<T>) => OptSerialized<T>;
export type DeserializationSuccess<T> = {
    tag: 'success';
    value: Opt<T>;
};
export type DeserializationFailure = {
    tag: 'failure';
    reason: string;
};
export type DeserializationResult<T> = DeserializationSuccess<T> | DeserializationFailure;
/**
 * Deserializes a plain JavaScript object to an Opt instance.
 *
 * @example
 * ```ts
 * deserialize({ type: 'Opt/None' }, isNumber) // { tag: 'success', value: none }
 * deserialize({ type: 'Opt/Some', value: 0 }, isNumber) // { tag: 'success', value: some(0) }
 * deserialize({ type: 'Opt/Some', value: 'not a number' }, isNumber) // { tag: 'failure', reason: 'failed to validate inner type' }
 * ```
 *
 * @see {@link serialize}
 * @param x serialized Opt object (expected shape is {@link OptSerialized})
 * @param guard function to validate the inner type
 * @returns deserialization result as a {@link DeserializationResult} object
 */
export declare const deserialize: <T>(x: unknown, guard: (x: unknown) => x is T) => DeserializationResult<T>;
/**
 * Deserializes the input value or throws an error if deserialization fails.
 *
 * @example
 * ```ts
 * deserializeOrCrash({ type: 'Opt/None' }, isNumber) // none
 * deserializeOrCrash({ type: 'Opt/Some', value: 0 }, isNumber) // some(0)
 * deserializeOrCrash({ type: 'Opt/Some', value: 'not a number' }, isNumber) // exception thrown
 * deserializeOrCrash(4, isNumber) // exception thrown
 * ```
 *
 * @param x input value to be deserialized
 * @param guard guard function to validate the inner type
 * @return deserialized value as an {@link Opt} instance
 */
export declare const deserializeOrCrash: <T>(x: unknown, guard: (x: unknown) => x is T) => Opt<T>;
/**
 * Unsafe version of {@link deserializeOrCrash}.
 * Deserialization failure is indistinguishable from deserialized {@link None}.
 * It is usually better to use {@link deserialize} or {@link deserializeOrCrash}.
 *
 * @example
 * ```ts
 * deserializeUnsafe({ type: 'Opt/None' }) // none
 * deserializeUnsafe({ type: 'Opt/Some', value: 0 }) // some(0)
 * deserializeUnsafe(9) // none
 * ```
 *
 * @param x input value to be deserialized
 * @returns deserialized value as an {@link Opt} instance
 */
export declare const deserializeUnsafe: (x: unknown) => Opt<unknown>;
/**
 * Single global instance of {@link None}.
 */
export declare const none: None<any>;
/**
 * Constructs {@link Some}.
 *
 * Warning: Usually it is {@link opt} you are looking for.
 * Only in rare cases you want to have for example `Some(undefined)`.
 * @param x
 */
export declare const some: <T>(x: T) => Readonly<Some<T>>;
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns {@link None}.
 * Anything else is wrapped into {@link Some}.
 * @example
 * ```ts
 * opt(0) // Some(0)
 * opt(1) // Some(1)
 * opt(true) // Some(true)
 * opt('Kagome') // Some('Kagome')
 * opt([]) // Some([])
 * opt([1]) // Some([1])
 * opt({}) // Some({})
 * opt({a: 0}) // Some({a: 0})
 * opt(undefined) // None
 * opt(null) // None
 * opt(NaN) // None
 * ```
 * @param x
 */
export declare const opt: <T>(x: T | undefined | null) => OptSafe<T>;
/**
 * For falsy values returns {@link None}, otherwise acts same as {@link opt}.
 * ```ts
 * optFalsy(''); // None
 * optFalsy(0); // None
 * optFalsy(false); // None
 * optFalsy(NaN); // None
 * ```
 * @param x
 */
export declare const optFalsy: <T>(x: T | undefined | null | "" | false | 0) => OptSafe<T>;
/**
 * For empty array (`[]`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export declare const optEmptyArray: <T, A extends readonly T[] | T[]>(x: A | undefined | null) => OptSafe<A>;
/**
 * For empty object (`{}`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export declare const optEmptyObject: <T extends object>(x: T | undefined | null) => OptSafe<T>;
/**
 * For empty string (`''`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export declare const optEmptyString: <T>(x: T | undefined | null | "") => OptSafe<T>;
/**
 * For a number `0` returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export declare const optZero: <T>(x: T | undefined | null | 0) => OptSafe<T>;
/**
 * For numbers lesser than `0` returns {@link None}, otherwise acts same as {@link opt}.
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
 * For numbers equal to `Infinity` or `-Infinity` returns {@link None}, otherwise acts same as {@link opt}.
 * ```ts
 * optInfinity(undefined) // None
 * optInfinity(1) // Some(1)
 * optInfinity(0) // Some(0)
 * optInfinity(Infinity) // None
 * optInfinity(-Infinity) // None
 * ```
 * @param x
 */
export declare const optInfinity: (x: number | undefined | null) => OptSafe<number>;
/**
 * Converts optional array of optional values to opt-wrapped array with empty values discarded.
 *
 * @example
 * ```ts
 * optArrayOpt(undefined) // None
 * optArrayOpt([]) // Some([])
 * optArrayOpt([1]) // Some([1])
 * optArrayOpt([0, null, undefined, 1]) // Some([0, 1])
 * ```
 *
 * @param xs
 */
export declare const optArrayOpt: <T>(xs: (T | EmptyValue)[] | EmptyValue) => OptSafe<T[]>;
/**
 * Is given value an instance of {@link Opt}?
 * @param x
 */
export declare const isOpt: (x: unknown) => x is Opt<unknown>;
/**
 * ```ts
 * <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>): Opt<B>
 * ```
 * Apply `oa` to function `of`. If any argument is {@link None} then result is {@link None}.
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
 * Apply `oa` to function `f`. If argument is {@link None} then result is {@link None}.
 * ```ts
 * apFn(x => x > 0)(opt(1)) // Opt(true)
 * apFn(x => x > 0)(none) // None
 * ```
 * @typeparam A input of function `f`
 * @typeparam B output of function `f`
 */
export declare const apFn: <A, B>(f: (_: A) => B) => (oa: Opt<A>) => Opt<B>;
/**
 * Transforms array of opts into an array where {@link None}s are omitted and {@link Some}s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
export declare const catOpts: <A>(xs: readonly Opt<A>[]) => A[];
/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
export declare const mapOpt: <A, B>(f: (_: A) => Opt<B>) => (xs: readonly A[]) => B[];
/**
 * Unwraps one level of nested {@link Opt}s. Similar to `flatten` in other libraries or languages.
 * ```ts
 * joinOpt(some(none)) // None
 * joinOpt(some(some(1))) // Some(1)
 * ```
 * @param x
 * @see {@link Opt.join}
 */
export declare const joinOpt: <T>(x: Opt<Opt<T>>) => Opt<T>;
/** @see {@link Opt.lengthIn} */
export declare const lengthIn: <T>(x: Opt<T[]>) => Opt<number>;
/** @see {@link Opt.fromArray} */
export declare const fromArray: typeof Opt.fromArray;
/** @see {@link Opt.toArray} */
export declare const toArray: <T>(x: Opt<T>) => [] | [T];
/** @see {@link Opt.fromObject} */
export declare const fromObject: FromObjectFn;
/** @see {@link Opt.toObject} */
export declare const toObject: <K extends string = "value">(k?: K) => <T>(x: Opt<T>) => Record<K, T | null>;
type MapFn = <T, U>(f: (_: T) => U) => <I extends (Opt<T> | readonly T[]), O extends (I extends Opt<T> ? Opt<U> : U[])>(x: I) => O;
/**
 * Same as {@link Opt.map}, but also supports arrays.
 * @see {@link Opt.map}
 */
export declare const map: MapFn;
/** @see {@link Opt.mapIn} */
export declare const mapIn: <T, U>(f: (x: T) => U) => (x: Opt<T[]>) => Opt<U[]>;
/** @see {@link Opt.mapFlow} */
export declare const mapFlow: MapFlowFn;
/**
 * Maps over each character in a string using the provided function.
 *
 * @example
 * ```ts
 * const toUpperCase = (c: string) => c.toUpperCase();
 * mapStr(toUpperCase)('hello') // 'HELLO'
 * mapStr(c => c === 'o' ? '0' : c)('hello world') // 'hell0 w0rld'
  * mapStr(c => c === 'r' ? 'ru' : c)('kirara') // 'kiruarua'
 * mapStr(c => c === 'a' ? '' : c)('sango') // 'sngo'
 * ```
 *
 * @param f Function to apply to each character
 * @returns A function that takes a string and returns the mapped string
 */
export declare const mapStr: <T extends string>(f: (c: string) => string) => (s: T) => string;
interface FlatMapFn {
    <T, U>(f: (_: T) => readonly U[]): (x: readonly T[]) => U[];
    <T, U>(f: (_: T) => Opt<U>): (x: Opt<T>) => Opt<U>;
}
/**
 * Same as {@link Opt.flatMap}, but also supports arrays.
 * @see {@link Opt.flatMap}
 */
export declare const flatMap: FlatMapFn;
/** @see {@link Opt.flatMapIn} */
export declare const flatMapIn: <T, U>(f: (a: T) => U[]) => (x: Opt<T[]>) => Opt<U[]>;
/** @see {@link Opt.flatMap} */
export declare const chain: FlatMapFn;
/** @see {@link Opt.chainIn} */
export declare const chainIn: <T, U>(f: (a: T) => U[]) => (x: Opt<T[]>) => Opt<U[]>;
/** @see {@link Opt.act} */
export declare const act: ActFn;
/** @see {@link Opt.chainFlow} */
export declare const chainFlow: ActFn;
/** @see {@link Opt.chainToOpt} */
export declare const chainToOpt: <T, U>(f: (_: T) => U | undefined | null) => (x: Opt<T>) => OptSafe<U>;
/** @see {@link Opt.actToOpt} */
export declare const actToOpt: ActToOptFn;
/** @see {@link Opt.chainToOptFlow} */
export declare const chainToOptFlow: ActToOptFn;
/** @see {@link Opt.someOrCrash} */
export declare const someOrCrash: <T>(msg: string) => (x: Opt<T>) => Some<T>;
/** @see {@link Opt.orCrash} */
export declare const orCrash: <T>(msg: string) => (x: Opt<T>) => T;
/** @see {@link Opt.orUndef} */
export declare const orUndef: <T>(x: Opt<T>) => T | undefined;
/** @see {@link Opt.orNull} */
export declare const orNull: <T>(x: Opt<T>) => T | null;
/** @see {@link Opt.orFalse} */
export declare const orFalse: <T>(x: Opt<T>) => T | false;
/** @see {@link Opt.orTrue} */
export declare const orTrue: <T>(x: Opt<T>) => T | true;
/** @see {@link Opt.orNaN} */
export declare const orNaN: <T>(x: Opt<T>) => T | number;
/** @see {@link Opt.caseOf} */
export declare const caseOf: <T, R>(onSome: (x: T) => R) => (onNone: () => R) => (x: Opt<T>) => R;
/** @see {@link Opt.fold} */
export declare const fold: <T, R>(someCase: (x: T) => R) => (noneCase: R) => (x: Opt<T>) => R;
/** @see {@link Opt.foldIn} */
export declare const foldIn: <T, R>(f: (acc: R, x: T) => R) => (initial: R) => (x: Opt<T[]>) => Opt<R>;
/** @see {@link Opt.onBoth} */
export declare const onBoth: <T>(onSome: (x: T) => void) => (onNone: () => void) => (x: Opt<T>) => Opt<T>;
/**
 * Similar to {@link Opt.pipe}, but the first argument is the input.
 * Supports arbitrary input type, not just {@link Opt}.
 * @see {@link Opt.pipe}
 */
export declare const pipe: PipeFn;
/** @see {@link Opt.contains} */
export declare const contains: <T>(y: T) => (x: Opt<T>) => boolean;
/** @see {@link Opt.has} */
export declare const has: <T>(x: T) => (opt: Opt<T>) => boolean;
/** @see {@link Opt.hasIn} */
export declare const hasIn: <U>(x: U) => (opt: Opt<readonly U[]>) => boolean;
/** @see {@link Opt.exists} */
export declare const exists: <T>(y: (_: T) => boolean) => (x: Opt<T>) => boolean;
/** @see {@link Opt.existsIn} */
export declare const existsIn: <T>(p: (x: T) => boolean) => (x: Opt<T[]>) => Opt<boolean>;
/** @see {@link Opt.forAll} */
export declare const forAll: <T>(p: (_: T) => boolean) => (x: Opt<T>) => boolean;
/** @see {@link Opt.forAllIn} */
export declare const forAllIn: <T>(p: (x: T) => boolean) => (x: Opt<T[]>) => Opt<boolean>;
/** @see {@link Opt.orElse} */
export declare const orElse: <T>(e: T) => (x: Opt<T>) => T;
/** @see {@link Opt.orElseLazy} */
export declare const orElseLazy: <T>(e: () => T) => (x: Opt<T>) => T;
/** @see {@link Opt.orElseAny} */
export declare const orElseAny: <U>(e: U) => <T>(x: Opt<T>) => T | U;
/** @see {@link Opt.alt} */
export declare const alt: <T>(def: Opt<T>) => (x: Opt<T>) => Opt<T>;
/** @see {@link Opt.altOpt} */
export declare const altOpt: <T>(def: T) => (x: Opt<T>) => OptSafe<T>;
/** @see {@link Opt.bimap} */
export declare const bimap: <T, U>(someF: (_: T) => U) => (noneF: () => U) => (x: Opt<T>) => Opt<U>;
/** @see {@link Opt.flatBimap} */
export declare const flatBimap: <T, U>(someF: (_: T) => Opt<U>) => (noneF: () => Opt<U>) => (x: Opt<T>) => Opt<U>;
export declare const zipArray: <T>(a: readonly T[]) => <U>(b: readonly U[]) => [T, U][];
export declare const zipOpt: <T>(x: Opt<T>) => <U>(other: Opt<U>) => Opt<[T, U]>;
/** @see {@link Opt.zip3} */
export declare const zip3Opt: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => Opt<[T, A, B]>;
/** @see {@link Opt.zip4} */
export declare const zip4Opt: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => Opt<[T, A, B, C]>;
/** @see {@link Opt.zip5} */
export declare const zip5Opt: <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>) => Opt<[T, A, B, C, D]>;
/** @see {@link Opt.zipIn} */
export declare const zipIn: <U, V>(x: Opt<readonly U[]>) => ((other: V[] | EmptyValue) => Opt<readonly [U, V][]>);
type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | readonly T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];
/**
 * Same as {@link Opt.filter}, but also supports arrays.
 * @see {@link Opt.filter}
 */
export declare const filter: FilterFn;
/** @see {@link Opt.filterIn} */
export declare const filterIn: <T>(p: (x: T) => boolean) => (opt: Opt<T[]>) => Opt<T[]>;
/** @see {@link Opt.findIn} */
export declare const findIn: <U>(f: (x: U) => boolean) => (opt: Opt<U[]>) => Opt<U>;
/** @see {@link Opt.noneIf} */
export declare const noneIf: <T>(predicate: (_: T) => boolean) => (x: Opt<T>) => Opt<T>;
/** @see {@link Opt.noneIfEmpty} */
export declare const noneIfEmpty: <T extends PossiblyEmpty>(x: Opt<T>) => Opt<WithoutPossiblyEmptyEmptyValues<T>>;
/** @see {@link Opt.noneWhen} */
export declare const noneWhen: <T>(returnNone: boolean) => (x: Opt<T>) => Opt<T>;
type CountFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | readonly T[]>(x: U) => U extends Opt<T> ? 0 | 1 : number;
/**
 * Same as {@link Opt.count}, but also supports arrays.
 *
 * @example
 * ```ts
 * const greaterThanZero = (x: number) => x > 0;
 *
 * count(greaterThanZero)([-3, 0, 5, 10]) // 2
 * ```
 *
 * @see {@link Opt.count}
 */
export declare const count: CountFn;
/** @see {@link Opt.countIn} */
export declare const countIn: <T>(p: (x: T) => boolean) => (x: Opt<T[]>) => Opt<number>;
/**
 * Find a first item which holds true for a given predicate and return it wrapped in {@link Some}.
 * Return {@link None} when no match is found.
 *
 * @example
 * ```ts
 * find((x: number) => x > 0)([-1, 0, 1]) // Some(1)
 * find((x: number) => x > 5)([0, 3, 5]) // None
 * ```
 *
 * @param predicate
 */
export declare const find: <T>(predicate: (_: T) => boolean) => (xs: readonly T[]) => Opt<T>;
/** @see {@link Opt.narrow} */
export declare const narrow: <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>) => Opt<U>;
/** @see {@link Opt.narrowOrCrash} */
export declare const narrowOrCrash: <T, U>(guard: (value: any) => value is U, crashMessage?: string) => (x: Opt<T>) => Opt<U>;
/**
 * Same as {@link Opt.print}, but supports arbitrary argument types.
 * @see {@link Opt.print}
 */
export declare const print: (tag?: string) => <T>(x: T) => T;
/** @see {@link Opt.equals} */
export declare const equals: <T>(other: Opt<T>, comparator?: EqualityFunction) => (x: Opt<T>) => boolean;
/** @see {@link Opt.prop} */
export declare const prop: <T extends object, K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never>(key: K) => (x: Opt<T>) => OptSafe<T[K]>;
/**
 * Similar to {@link Opt.prop}, but it is designed for naked objects (not wrapped in opt).
 *
 * @example
 * ```ts
 * type ObjA = {a: boolean};
 * propNaked<ObjA>('a')(null) // None
 * propNaked<ObjA>('a')({a: true}) // Some(true)
 *
 * type ObjC = {c: string | null};
 * propNaked<ObjC>('c')({c: null}) // None
 * ```
 */
export declare const propNaked: <T extends object | EmptyValue, K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never>(key: K) => (x: T | EmptyValue) => OptSafe<T[K]>;
/**
 * Similar to {@link Opt.propOrCrash}, but also supports naked objects.
 *
 * @example
 * ```ts
 * interface Animal {
 *   name?: string;
 * }
 * const a: Animal = {name: 'Spot'};
 * propOrCrash<Animal>('name')(a) // 'Spot'
 * ```
 */
export declare const propOrCrash: <T extends object, P extends Opt<T> | T = T | Opt<T>, K extends (P extends Opt<T> ? (T extends object ? keyof T : never) : (P extends object ? keyof P : never)) = P extends Opt<T> ? T extends object ? keyof T : never : P extends object ? keyof P : never>(key: K) => (x: P) => WithoutOptValues<T[K]>;
/**
 * Utility function for generating property getter for one specific object.
 * Functionally similar to {@link propOrCrash}, but it has swapped arguments and only supports naked objects.
 *
 * @example
 * ```ts
 * interface Animal {
 *   id: number;
 *   name?: string;
 * }
 *
 * const spot: Animal = {id: 36, name: 'Spot'};
 * const getSpotProp = genNakedPropOrCrash(spot);
 * getSpotProp('name') // 'Spot'
 * getSpotProp('id') // 36
 *
 * const cow: Animal = {id: 36};
 * const getCowProp = genNakedPropOrCrash(cow);
 * getCowProp('name') // crashes with 'missing name'
 * ```
 *
 * ---
 *
 * It is a shorter alternative of
 * ```ts
 * const o = opt(obj);
 * o.propOrCrash('fieldA')
 * o.propOrCrash('fieldB')
 * ```
 * ->
 * ```ts
 * const g = genNakedPropOrCrash(obj);
 * g('fieldA')
 * g('fieldB')
 * ```
 *
 * Performance characterics are expected to be similar.
 *
 * @param obj
 */
export declare const genNakedPropOrCrash: <T extends object>(obj: T) => <K extends keyof T>(k: K) => T extends object ? WithoutOptValues<T[any]> : never;
/** @see {@link Opt.swap} */
export declare const swap: <U>(newValue: U) => <T>(x: Opt<T>) => Opt<U>;
/**
 * Takes functions and builds a function which consecutively calls each given function with a result from a previous one.
 * Similar to {@link Opt.pipe}, but doesn't take input directly, instead returns a function which can be called repeatedly with different inputs.
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
 * Unlike {@link flow} and {@link pipe}, functions passed to {@link compose} are applied (called) from last to first.
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
type CurryTupleFn = <A, B, C>(_: (_: [A, B]) => C) => (_: A) => (_: B) => C;
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
 * @see {@link uncurryTuple}
 * @param f
 */
export declare const curryTuple: CurryTupleFn;
type CurryTuple3Fn = <A, B, C, D>(_: (_: [A, B, C]) => D) => (_: A) => (_: B) => (_: C) => D;
/**
 * Transforms the given function of three arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export declare const curryTuple3: CurryTuple3Fn;
type CurryTuple4Fn = <A, B, C, D, E>(_: (_: [A, B, C, D]) => E) => (_: A) => (_: B) => (_: C) => (_: D) => E;
/**
 * Transforms the given function of four arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export declare const curryTuple4: CurryTuple4Fn;
type CurryTuple5Fn = <A, B, C, D, E, F>(_: (_: [A, B, C, D, E]) => F) => (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F;
/**
 * Transforms the given function of five arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export declare const curryTuple5: CurryTuple5Fn;
type UncurryTupleFn = <A, B, C>(_: (_: A) => (_: B) => C) => (_: [A, B]) => C;
/**
 * Transforms the given function of two arguments from curried format to "tuple curried" which can be used with {@link Opt.zip}.
 *
 * ```ts
 * const sub = (x: number) => (y: number) => x - y;
 * opt(4) // Some(4)
 *   .zip(opt(1)) // Some([4, 1])
 *   .map(uncurryTuple(sub)) // Some(3)
 * ```
 *
 * @see {@link curryTuple}
 * @param f
 */
export declare const uncurryTuple: UncurryTupleFn;
type UncurryTuple3Fn = <A, B, C, D>(_: (_: A) => (_: B) => (_: C) => D) => (_: [A, B, C]) => D;
/**
 * Transforms the given function of three arguments from curried format to "tuple curried" which can be used with {@link Opt.zip3}.
 * @see {@link uncurryTuple}
 * @param f
 */
export declare const uncurryTuple3: UncurryTuple3Fn;
type UncurryTuple4Fn = <A, B, C, D, E>(_: (_: A) => (_: B) => (_: C) => (_: D) => E) => (_: [A, B, C, D]) => E;
/**
 * Transforms the given function of four arguments from curried format to "tuple curried" which can be used with {@link Opt.zip4}.
 * @see {@link uncurryTuple}
 * @param f
 */
export declare const uncurryTuple4: UncurryTuple4Fn;
type UncurryTuple5Fn = <A, B, C, D, E, F>(_: (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F) => (_: [A, B, C, D, E]) => F;
/**
 * Transforms the given function of five arguments from curried format to "tuple curried" which can be used with {@link Opt.zip5}.
 * @see {@link uncurryTuple}
 * @param f
 */
export declare const uncurryTuple5: UncurryTuple5Fn;
type PossiblyEmpty = Opt<unknown> | unknown[] | null | undefined | Map<unknown, unknown> | Set<unknown> | object | string | number;
type WithoutPossiblyEmptyEmptyValues<T> = Exclude<T, '' | [] | typeof none | EmptyValue>;
/**
 * Similar to `isEmpty` from lodash, but also supports {@link Opt}s.
 * Returns `true` for {@link None}, `[]`, `null`, `undefined`, empty map, empty set, empty object, `''` and `NaN`.
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
export declare const isEmpty: (x: PossiblyEmpty) => boolean;
/**
 * Negated version of {@link isEmpty}.
 * `nonEmpty(x)` is the same as `!isEmpty(x)`. It can be useful when composing functions (e.g. via {@link pipe}).
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
 * @see {@link isEmpty}
 * @param x
 */
export declare const nonEmpty: (x: PossiblyEmpty) => boolean;
/** @alias {@link nonEmpty} */
export declare const isFull: (x: PossiblyEmpty) => boolean;
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
type AtFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/**
 * Same as {@link Opt.at}, but also supports unwrapped arrays.
 * @see {@link Opt.at}
 * @param index
 */
export declare const at: (index: number) => AtFn;
type HeadFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/**
 * Returns the first element of an array or fist character of a string.
 * @param xs
 */
export declare const head: HeadFn;
type HeadInFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : (T extends string ? Opt<string> : never)>(x: EmptyValue | Opt<T>) => R;
/**
 * Same as {@link Opt.headIn}.
 * @see {@link Opt.headIn}
 * @param x
 */
export declare const headIn: HeadInFn;
type LastFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;
/** Returns the last element of an array or last character of a string. */
export declare const last: LastFn;
type LastInFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : (T extends string ? Opt<string> : never)>(x: EmptyValue | Opt<T>) => R;
/**
 * Same as {@link Opt.lastIn}.
 * @see {@link Opt.lastIn}
 * @param x
 */
export declare const lastIn: LastInFn;
interface ZipToOptArrayFn {
    <A, B>(xs: readonly [A, B]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>]>;
    <A, B, C>(xs: readonly [A, B, C]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>]>;
    <A, B, C, D>(xs: readonly [A, B, C, D]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>]>;
    <A, B, C, D, E>(xs: readonly [A, B, C, D, E]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>, WithoutOptValues<E>]>;
}
/**
 * Takes a tuple, wraps each element in {@link Opt} and applies appropriate {@link Opt.zip} function.
 *
 * @example
 * ```ts
 * zipToOptArray([1, null, '', 7, false]) // None: Opt<[number, boolean, string, number, boolean]>
 * zipToOptArray([1, true, '', 7, false]) // Some<[1, true, '', 7, false]>: Opt<[number, boolean, string, number, boolean]>
 * ```
 *
 * Useful as a replacement to `zip*` functions when construction of {@link Opt}s happens in parameters of the function.
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
/** @see {@link Opt.testReOrFalse} */
export declare const testReOrFalse: (re: RegExp) => (x: Opt<string>) => boolean;
/**
 * Runs a given function. Result is wrapped by {@link opt}. Returns {@link None} when the function throws.
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
 * Parses JSON. The result is passed to {@link opt}, any error results in {@link None}.
 *
 * @example
 * ```ts
 * parseJson('{"a": 1}') // Some({a: 1})
 * parseJson('Ryoka') // None
 * parseJson('null') // None - valid JSON (according to the new standard), but opt(null) is None
 * ```
 *
 * Typical use is to call {@link Opt.narrow} afterwards to validate parsed data and get proper type.
 *
 * @param x
 */
export declare const parseJson: (x: string) => Opt<unknown>;
/**
 * Parses integer (same semantics as `Number.parseInt`).
 * The result is wrapped into {@link opt} (so `NaN` will become {@link None}).
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
 * The result is wrapped into {@link opt} (so `NaN` will become {@link None}).
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
/** @see {@link Opt.apply} */
export declare const apply: <T extends AnyFunc, R extends ReturnType<T>, A extends Parameters<T>>(...args: A) => (x: Opt<T>) => Opt<R>;
/** @see {@link Opt.onFunc} */
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
type AssertTypeFunc = <T>(x: unknown, guard: (x: unknown) => x is T, msg?: string) => asserts x is T;
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
/** Returns the minimum value in an array. */
export declare const min: <R>(x: readonly R[]) => OptSafe<R>;
/** @see {@link Opt.minIn} */
export declare const minIn: <R>(x: Opt<readonly R[]>) => OptSafe<R>;
/** Returns the maximum value in an array. */
export declare const max: <R>(x: readonly R[]) => OptSafe<R>;
/** @see {@link Opt.maxIn} */
export declare const maxIn: <R>(x: Opt<readonly R[]>) => OptSafe<R>;
/**
 * Get a lesser number from two given numbers.
 *
 * @example
 * ```ts
 * min2Num(1)(2) // 1
 * ```
 *
 * @param a
 */
export declare const min2Num: (a: number) => (b: number) => number;
/**
 * Get a lesser number from two possibly empty numbers.
 * Returns `None` when any operand is `None`.
 *
 * @example
 * ```ts
 * min2All(1)(2) // Some(1)
 * min2All(1)(null) // None
 * min2All(null)(null) // None
 * ```
 */
export declare const min2All: (a: number | EmptyValue) => (b: number | EmptyValue) => Opt<number>;
/**
 * Get a lesser number from two possibly empty numbers.
 * Returns `None` when both operands are `None`, otherwise returns the other (nonempty) operand.
 *
 * @example
 * ```ts
 * min2Any(1)(2) // Some(1)
 * min2Any(1)(null) // Some(1)
 * min2Any(null)(undefined) // None
 * ```
 */
export declare const min2Any: (a: number | EmptyValue) => (b: number | EmptyValue) => Opt<number>;
/**
 * Get a larger number from two given numbers.
 *
 * @example
 * ```ts
 * max2Num(1)(2) // 2
 * ```
 *
 * @param a
 */
export declare const max2Num: (a: number) => (b: number) => number;
/**
 * Get a larger number from two possibly empty numbers.
 * Returns `None` when any operand is `None`.
 *
 * @example
 * ```ts
 * max2All(1)(2) // Some(2)
 * max2All(1)(null) // None
 * max2All(null)(null) // None
 * ```
 */
export declare const max2All: (a: number | EmptyValue) => (b: number | EmptyValue) => Opt<number>;
/**
 * Get a larger number from two possibly empty numbers.
 * Returns `None` when both operands are `None`, otherwise returns the other (nonempty) operand.
 *
 * @example
 * ```ts
 * max2Any(1)(2) // Some(2)
 * max2Any(1)(null) // Some(1)
 * max2Any(null)(undefined) // None
 * ```
 */
export declare const max2Any: (a: number | EmptyValue) => (b: number | EmptyValue) => Opt<number>;
/**
 * Given range (where each part may be empty), clamp a given possibly empty number to the given range.
 *
 * @example
 * ```ts
 * clamp(0)(10)(5) // Some(5)
 * clamp(0)(10)(-4) // Some(0)
 * clamp(0)(10)(12) // Some(10)
 *
 * clamp(0)(undefined)(5) // Some(5)
 * clamp(0)(null)(-1) // Some(0)
 *
 * clamp(NaN)(10)(5) // Some(5)
 * clamp(undefined)(10)(12) // Some(10)
 *
 * clamp(undefined)(undefined)(5) // Some(5)
 *
 * clamp(0)(1)(null) // None
 * ```
 *
 * @param minValue
 */
export declare const clamp: (minValue: number | EmptyValue) => (maxValue: number | EmptyValue) => (x: number | EmptyValue) => Opt<number>;
/**
 * Logical negation of a boolean value.
 *
 * @example
 * ```ts
 * not(true) // false
 * not(false) // true
 * ```
 *
 * @param x
 * @returns The negated boolean value
 */
export declare const not: (x: boolean) => boolean;
/**
 * Logical AND of two boolean values.
 *
 * @example
 * ```ts
 * and(true)(false) // false
 * and(true)(true) // true
 * ```
 *
 * @param x
 * @returns
 */
export declare const and: (x: boolean) => (y: boolean) => boolean;
/**
 * Logical OR of two boolean values.
 *
 * @example
 * ```ts
 * or(true)(false) // true
 * or(false)(false) // false
 * ```
 *
 * @param x
 * @returns
 */
export declare const or: (x: boolean) => (y: boolean) => boolean;
/**
 * Logical XOR (exclusive OR) of two boolean values.
 *
 * @example
 * ```ts
 * xor(true)(false) // true
 * xor(true)(true) // false
 * ```
 *
 * @param x
 * @returns
 */
export declare const xor: (x: boolean) => (y: boolean) => boolean;
/**
 * Returns one of two values based on a boolean condition.
 *
 * @example
 * ```ts
 * bool(0)(1)(true) // 1
 * bool('no')('yes')(false) // 'no'
 * ```
 *
 * @param falseValue
 * @returns
 */
export declare const bool: <T>(falseValue: T) => (trueValue: T) => (cond: boolean) => T;
/**
 * Increments a number by 1.
 *
 * @example
 * ```ts
 * inc(5) // 6
 * ```
 *
 * @param x - The number to increment
 * @returns The number incremented by 1
 */
export declare const inc: (x: number) => number;
/**
 * Decrements a number by 1.
 *
 * @example
 * ```ts
 * dec(5) // 4
 * ```
 *
 * @param x - The number to decrement
 * @returns The number decremented by 1
 */
export declare const dec: (x: number) => number;
/**
 * Prepends a string to a given string.
 *
 * @example
 * ```ts
 * prependStr('foo')('bar') // 'foobar'
 * opt('bar').map(prependStr('foo')) // Some('foobar')
 * ```
 */
export declare const prependStr: <P extends string>(prefix: P) => <X extends string>(str: X) => `${P}${X}`;
/**
 * Appends a string to a given string.
 *
 * @example
 * ```ts
 * appendStr('bar')('foo') // 'foobar'
 * opt('foo').map(appendStr('bar')) // Some('foobar')
 * opt('foo' as const).map(appendStr('bar')) // Some('foobar'), type is Opt<'foobar'>
 * ```
 */
export declare const appendStr: <S extends string>(suffix: S) => <X extends string>(str: X) => `${X}${S}`;
/**
 * Throws an error with a given message.
 *
 * @example
 * ```ts
 * crash('Zeref?') // throws Error
 * ```
 *
 * @param msg - The error message
 * @returns Nothing (function never returns due to exception)
 */
export declare const crash: (msg: string) => never;
/**
 * Checks equality between two values of type T using strict equality.
 *
 * @example
 * ```ts
 * eq(5)(5) // true
 * eq(5)(6) // false
 * eq({})({}) // false
 * ```
 *
 * @param a - The first value to compare
 * @returns A function that takes another value and returns whether it is strictly equal to the first value
 */
export declare const eq: <T>(a: T) => (b: T) => boolean;
/**
 * Checks equality between two values of any type using strict equality.
 *
 * @example
 * ```ts
 * eqAny('hello')('hello') // true
 * eqAny('hello')('Hi') // false
 * eqAny('')(2) // false
 * ```
 *
 * @param a - The first value to compare
 * @returns A function that takes another value and returns whether it is strictly equal to the first value
 */
export declare const eqAny: (a: unknown) => (b: unknown) => boolean;
/**
 * A no-operation function that simply returns `undefined`.
 * Can be used as a placeholder callback.
 *
 * @returns undefined
 */
export declare const noop: (..._args: unknown[]) => void;
export {};
