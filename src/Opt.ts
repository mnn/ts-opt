// Do NOT split to multiple modules - it's not possible, since there would be cyclic dependencies..
import {
  ActFn,
  ActInClassFn,
  ActToOptFn,
  ActToOptInClassFn,
  ComposeFn,
  FlowFn,
  MapFlowFn,
  MapFlowInClassFn,
  PipeFn,
  PipeInClassFn,
} from './FlowLike';

const someSymbol = Symbol('Some');
const noneSymbol = Symbol('None');
const errorSymbol = Symbol('Error');
export type EqualityFunction = <T>(a: T, b: T) => boolean;
const refCmp: EqualityFunction = <T>(a: T, b: T): boolean => a === b;
type NotObject<T> = T extends object ? never : T;
type SuperUnionOf<T, U> = Exclude<U, T> extends never ? NotObject<T> : never;

// NaN doesn't exist in TypeScript type system
// https://github.com/Microsoft/TypeScript/issues/28682
type WithoutOptValues<T> = NonNullable<T>;
type EmptyValue = null | undefined;

type AnyFunc = (...args: any) => any;
/**
 * @internal
 */
export type OptSafe<T> = Opt<WithoutOptValues<T>>;

/* istanbul ignore next */
class OperationNotAvailable<TypeGot, TypeExpected> {
  readonly '@@type' = errorSymbol;
  _notUsed1?: TypeGot;
  _notUsed2?: TypeExpected;
}

interface ConstInClassFn<T> {
  (): () => T | null;

  <E>(emptyValue: E): () => T | E;
}

interface FromObjectFn {
  <O extends { value: T }, T>(x: O): Opt<O['value']>;

  <K extends keyof O & string, O extends object>(x: O, k: K): Opt<O[K]>;
}

interface ToObjectFn<T> {
  (): ToObjectRes<T>;

  <K extends string>(k: string): Record<K, T | null>;
}

interface ToObjectRes<T> {
  value: T | null;
}

export const isString = (x: any): x is string => typeof x === 'string';
export const toString = (x: { toString(): string }): string => x.toString();
export const isArray = (x: any): x is unknown[] => Array.isArray(x);
export const isReadonlyArray = (x: any): x is readonly unknown[] => Array.isArray(x);
// eslint-disable-next-line @typescript-eslint/ban-types
export const isFunction = (x: any): x is Function => typeof x === 'function';
export const isObject = (value: any): value is object => value !== null && typeof value === 'object';
export const isNumber = (x: any): x is number => typeof x === 'number';
export const isUnknown = (_: unknown): _ is unknown => true;

const debugPrint = (tag?: string, ...xs: unknown[]) => {
  console.log(...[...opt(tag).map(x => [`[${x}]`]).orElse([]), ...xs]);
};

/**
 * Generic container class. It either holds exactly one value - {@link Some}, or no value - {@link None} (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent
 * API" in OOP or {@link Opt.pipe|chain of reverse applications} in FP).
 *
 * @typeparam T Wrapped value type.
 */
export abstract class Opt<T> {
  /** @internal */
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  constructor() { }

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
  get nonEmpty(): boolean { return !this.isEmpty; }

  /**
   * @alias {@link Opt.nonEmpty}
   */
  get isFull(): boolean { return this.nonEmpty; }

  /**
   * Is this instance of {@link Some}?
   */
  isSome(): this is Some<T> { return this.nonEmpty; }

  /**
   * Is this instance of {@link None}?
   */
  isNone(): this is None<T> { return this.isEmpty; }

  /**
   * `1` for {@link Some}, `0` for {@link None}.
   */
  get length(): 0 | 1 { return this.isEmpty ? 0 : 1; }

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
  static fromArray<T>(x: readonly [] | readonly [T]): Opt<T> { return opt(x[0]); }

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
  static fromObject: FromObjectFn = (x: object, k = 'value') => {
    return opt((x as any)[k]);
  };

  /**
   * Convets {@link Opt} to an object.
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
  toObject: ToObjectFn<T> = (k = 'value') => {
    return {[k]: this.orNull()} as any;
  };

  /**
   * Serializes {@link Opt} to a plain JavaScript object.
   * @see {@link serialize}
   */
  serialize(): OptSerialized<T> {
    return serialize(this);
  }

  /**
   * Deserializes {@link Opt} from a plain JavaScript object.
   * @see {@link deserialize}
   */
  static deserialize<T>(x: unknown, guard: (x: unknown) => x is T): DeserializationResult<T> {
    return deserialize(x, guard);
  }

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
  mapFlow: MapFlowInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.map(x), this);

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
   * @alias {@link flatMap}
   * @see {@link chain}
   * @param f
   */
  chain<U>(f: (_: T) => Opt<U>): Opt<U> { return this.flatMap(f); }

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
  act: ActInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.chain(x), this);

  /**
   * @alias {@link Opt.act}
   * @see {@link chainFlow}
   * @param args
   */
  chainFlow: ActInClassFn<T> = (...args: any[]) => (this.act as any)(...args);

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
  chainToOpt<U>(f: (_: T) => U | undefined | null): OptSafe<U> { return this.flatMap(x => opt(f(x))); }

  /**
   * Similar to {@link act}, but functions return empty values instead of {@link Opt}.
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
   * @see {@link actToOpt}
   *
   * @param fs
   */
  actToOpt: ActToOptInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.chainToOpt(x), this);

  /**
   * @alias {@link Opt.actToOpt}
   * @see {@link chainToOptFlow}
   * @param args
   */
  chainToOptFlow: ActToOptInClassFn<T> = (...args: any[]) => (this.act as any)(...args);

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
  join<U>(this: Opt<Opt<U>>): Opt<U> {
    return this.flatMap(id);
  }

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
   * Calls appropriate callback and returns without change current instance of {@link Opt}.
   *
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
  pipe: PipeInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => x(acc), this);

  /**
   * Compares inner value with given value using `===`. Always `false` for {@link None}.
   *
   * ```ts
   * some(1).contains(1) // true
   * some(0).contains(1) // false
   * none.contains(undefined) // false
   * ```
   *
   * @see {@link contains}
   *
   * @param x
   */
  abstract contains(x: T): boolean;

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
  filterByRe<R extends (T extends string ? Opt<string> : never)>(regex: RegExp): R {
    if (this.isSome() && !isString(this.value)) {
      throw new Error(`Expected string, got ${JSON.stringify(this.value)}.`);
    }
    return (this as unknown as Opt<string>).filter(testRe(regex)) as R;
  }

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
  noneIf(predicate: (_: T) => boolean): Opt<T> {
    return this.filter(x => !predicate(x));
  }

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
  noneIfEmpty(this: Opt<PossiblyEmpty>): Opt<WithoutPossiblyEmptyEmptyValues<T>> {
    return this.noneIf(isEmpty) as unknown as Opt<WithoutPossiblyEmptyEmptyValues<T>>;
  }

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
  noneWhen(returnNone: boolean): Opt<T> {
    return this.noneIf(_ => returnNone);
  }

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
  count(predicate: (_: T) => boolean): 0 | 1 {
    return this.filter(predicate).length;
  }

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
  widen<U, R extends SuperUnionOf<U, T> = SuperUnionOf<U, T>>(): Opt<R> {
    return this as unknown as Opt<R>;
  }

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
  propOrCrash< //
    K extends (T extends object ? keyof T : never), //
    R extends (T extends object ? WithoutOptValues<T[K]> | never : never) //
    >(key: K): R {
    return this.prop(key).orCrash(`missing ${String(key)}`) as R;
  }

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
  const: ConstInClassFn<T> = function (this: any) {
    if (arguments.length === 1) {
      // eslint-disable-next-line prefer-rest-params
      const e = arguments[0];
      return () => this.isSome() ? this.value : e;
    }
    return () => this.orNull();
  };

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
   * Get a first item of an array or a first character of a string.
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
  headIn<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R> { return this.at(0); }

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
  lastIn<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(): OptSafe<R> { return this.at(-1); }

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
  testReOrFalse<R extends (T extends string ? boolean : OperationNotAvailable<T, string>)>(re: RegExp): R {
    if (this.isEmpty) return false as R;
    return this.narrow(isString).someOrCrash(`testReOrFalse only works on Opt<string>`).map(testRe(re)).orFalse() as R;
  }

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
  get end(): void { return undefined; }

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
   * @note {@link apply} is only available for functions, otherwise an exception will be thrown when called on {@link Some}.
   *
   * @see {@link Opt.onFunc} for imperative version
   *
   * @param args Parameters passed to wrapped function.
   * @return `opt`-wrapped result from the function
   */
  apply< //
    R extends (T extends AnyFunc ? ReturnType<T> : OperationNotAvailable<T, AnyFunc>),
    A extends (T extends AnyFunc ? Parameters<T> : never) //
    >(...args: A): Opt<R> {
    if (this.isSome()) {
      const val = this.value;
      if (isFunction(val)) { return opt(val(...args)); }
      throw new Error(`Invalid input - expected function, got ${typeof val}.`);
    }
    return none;
  }

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
  onFunc<A extends (T extends AnyFunc ? Parameters<T> : never)>(...args: A): Opt<T> {
    if (this.isSome()) {
      const val = this.value;
      if (isFunction(val)) {
        val(...args);
      } else {
        throw new Error(`Invalid input - expected function, got ${typeof val}.`);
      }
    }
    return this;
  }
}

/**
 * Empty {@link Opt}.
 * @notExported
 * @see {@link opt}
 * @see {@link none}
 */
class None<T> extends Opt<T> {
  readonly '@@type' = noneSymbol;

  /** @internal */
  constructor() { super(); }

  get isEmpty(): boolean { return true; }

  toArray(): [] | [T] { return []; }

  flatMap<U>(_f: (_: T) => Opt<U>): Opt<U> { return none as unknown as Opt<U>; }

  map<U>(): Opt<U> { return none as unknown as Opt<U>; }

  orCrash(msg: string): T { throw new Error(msg); }

  someOrCrash(msg: string): Some<T> { throw new Error(msg); }

  orNull(): T | null { return null; }

  orUndef(): T | undefined { return undefined; }

  orFalse(): false | T { return false; }

  orTrue(): true | T { return true; }

  orNaN(): number | T { return NaN; }

  caseOf<R>(_onSome: (x: T) => R, onNone: () => R): R {
    return onNone();
  }

  onBoth(_onSome: (x: T) => void, onNone: () => void): Opt<T> {
    onNone();
    return this;
  }

  onNone(f: () => void): Opt<T> {
    f();
    return this;
  }

  onSome(_f: (x: T) => void): Opt<T> { return this; }

  contains(_x: T): boolean { return false; }

  exists(_p: (x: T) => boolean): boolean { return false; }

  forAll(_p: (x: T) => boolean): boolean { return true; }

  orElse(def: T): T { return def; }

  orElseLazy(def: () => T): T { return def(); }

  alt(def: Opt<T>): Opt<T> { return def; }

  altOpt(def: T | EmptyValue): OptSafe<T> { return opt(def); }

  orElseAny<U>(def: U): U { return def; }

  bimap<U>(_someF: (_: T) => U, noneF: () => U): Opt<U> { return opt(noneF()); }

  flatBimap<U>(_someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U> { return noneF(); }

  toString(): string { return 'None'; }

  zip<U>(_other: Opt<U>): Opt<[T, U]> { return none; }

  zip3<X, Y>(_x: Opt<X>, _y: Opt<Y>): Opt<[T, X, Y]> { return none; }

  zip4<X, Y, Z>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>): Opt<[T, X, Y, Z]> { return none; }

  zip5<X, Y, Z, ZZ>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>, _zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]> { return none; }

  filter(_predicate: (_: T) => boolean): Opt<T> { return none; }

  narrow<U>(_guard: (value: any) => value is U): Opt<U> { return this as unknown as Opt<U>; }

  narrowOrCrash<U>(guard: (value: any) => value is U, _crashMessage?: string): Opt<U> {
    // don't crash on previous none
    return this.narrow(guard);
  }

  print(tag?: string): Opt<T> {
    debugPrint(tag, 'None');
    return this;
  }

  equals(other: Opt<T>, _comparator: EqualityFunction = refCmp): boolean {
    return other.isEmpty;
  }

  prop<K extends (T extends object ? keyof T : never)>(_key: K): OptSafe<T[K]> { return none; }

  swap<U>(_newVal: U): Opt<U> {
    return none;
  }

  at<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(_index: number): OptSafe<R> {
    return none;
  }

  maxIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R> {
    return none;
  }

  minIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R> {
    return none;
  }
}

/**
 * {@link Opt} with a value inside.
 * @notExported
 * @see {@link Opt}
 */
class Some<T> extends Opt<T> {
  readonly '@@type' = someSymbol;

  /** @internal */
  constructor(private _value: T) { super(); }

  get isEmpty(): boolean { return false; }

  get value(): T { return this._value; }

  toArray(): [] | [T] { return [this._value]; }

  flatMap<U>(f: (_: T) => Opt<U>): Opt<U> {
    return f(this._value);
  }

  map<U>(f: (_: T) => U): Opt<U> {
    return some(f(this._value));
  }

  orCrash(_msg: string): T { return this._value; }

  someOrCrash(_msg: string): Some<T> { return this; }

  orNull(): T | null { return this._value; }

  orUndef(): T | undefined { return this._value; }

  orFalse(): false | T { return this._value; }

  orTrue(): true | T { return this._value; }

  orNaN(): number | T { return this._value; }

  caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R { return onSome(this._value); }

  onBoth(onSome: (x: T) => void, _onNone: () => void): Opt<T> {
    onSome(this._value);
    return this;
  }

  contains(x: T): boolean { return this._value === x; }

  exists(p: (x: T) => boolean): boolean { return p(this._value); }

  forAll(p: (x: T) => boolean): boolean { return p(this._value); }

  onNone(_f: () => void): Opt<T> { return this; }

  onSome(f: (x: T) => void): Opt<T> {
    f(this._value);
    return this;
  }

  orElse(_def: T): T { return this._value; }

  orElseLazy(_def: () => T): T { return this._value; }

  alt(_def: Opt<T>): Opt<T> { return this; }

  altOpt(_def: T | EmptyValue): OptSafe<T> { return this as unknown as OptSafe<T>; }

  orElseAny<U>(_def: U): T { return this._value; }

  bimap<U>(someF: (_: T) => U, _noneF: () => U): Opt<U> { return opt(someF(this._value)); }

  flatBimap<U>(someF: (_: T) => Opt<U>, _noneF: () => Opt<U>): Opt<U> { return someF(this._value); }

  toString(): string { return `Some(${JSON.stringify(this._value)})`; }

  zip<U>(other: Opt<U>): Opt<[T, U]> {
    if (other.isEmpty) { return none; }
    return opt([this._value, other.orCrash('bug in isEmpty or orCrash')] as [T, U]);
  }

  zip3<X, Y>(x: Opt<X>, y: Opt<Y>): Opt<[T, X, Y]> {
    if (x.isEmpty || y.isEmpty) { return none; }
    const [xVal, yVal] = [x.orCrash('bug in isEmpty or orCrash'), y.orCrash('bug in isEmpty or orCrash')];
    return opt([this._value, xVal, yVal] as [T, X, Y]);
  }

  zip4<X, Y, Z>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>): Opt<[T, X, Y, Z]> {
    const args = [x, y, z];
    if (args.some(a => a.isEmpty)) { return none; }
    const [xVal, yVal, zVal] = args.map(a => a.orCrash('bug in isEmpty or orCrash'));
    return opt([this._value, xVal, yVal, zVal] as [T, X, Y, Z]);
  }

  zip5<X, Y, Z, ZZ>(x: Opt<X>, y: Opt<Y>, z: Opt<Z>, zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]> {
    const args = [x, y, z, zz];
    if (args.some(a => a.isEmpty)) { return none; }
    const [xVal, yVal, zVal, zzVal] = args.map(a => a.orCrash('bug in isEmpty or orCrash'));
    return opt([this._value, xVal, yVal, zVal, zzVal] as [T, X, Y, Z, ZZ]);
  }

  filter(predicate: (_: T) => boolean): Opt<T> { return predicate(this._value) ? this : none; }

  narrow<U>(guard: (value: any) => value is U): Opt<U> {
    return guard(this._value) ? this as unknown as Opt<U> : none;
  }

  narrowOrCrash<U>(guard: (value: any) => value is U, crashMessage?: string): Opt<U> {
    return this.narrow(guard).someOrCrash(crashMessage ?? 'Unexpected type in opt.');
  }

  print(tag?: string): Opt<T> {
    debugPrint(tag, 'Some:', this._value);
    return this;
  }

  equals(other: Opt<T>, comparator: EqualityFunction = refCmp): boolean {
    if (other.isEmpty) { return false; }
    return comparator(this._value, other.orCrash('Some expected'));
  }

  prop<K extends (T extends object ? keyof T : never)>(key: K): OptSafe<T[K]> { return opt(this._value[key]) as Opt<WithoutOptValues<T[K]>>; }

  swap<U>(newVal: U): Opt<U> {
    return some(newVal);
  }

  at<R extends (T extends readonly (infer A)[] ? A : (T extends string ? string : never))>(index: number): OptSafe<R> {
    const val = this._value;
    if (Array.isArray(val) || isString(val)) {
      const processedIndex = (index < 0 ? val.length : 0) + index;
      return opt(val[processedIndex]);
    } else {
      throw new Error(`\`Opt#at\` can only be used on arrays and strings`);
    }
  }

  minIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R> {
    const val = this._value;
    if (!isArray(val)) { throw new Error('Expected array.'); }
    if (val.length === 0) return none;
    return some(val.reduce((acc: R, x: any) => x < acc ? x : acc, val[0] as R)) as OptSafe<R>;
  }

  maxIn<R extends (T extends readonly (infer A)[] ? A : never)>(): OptSafe<R> {
    const val = this._value;
    if (!isArray(val)) { throw new Error('Expected array.'); }
    if (val.length === 0) return none;
    return some(val.reduce((acc: R, x: any) => x > acc ? x : acc, val[0] as R)) as OptSafe<R>;
  }
}

const someSerializedType = 'Opt/Some';
const noneSerializedType = 'Opt/None';

export type NoneSerialized = {
  type: typeof noneSerializedType;
};

export type SomeSerialized<T> = {
  type: typeof someSerializedType,
  value: T,
};

/**
 * Represents a serialized {@link Opt} type, which can be either {@link NoneSerialized} or {@link SomeSerialized}.
 */
export type OptSerialized<T> = NoneSerialized | SomeSerialized<T>;

export const isOptSerialized = (x: unknown): x is OptSerialized<unknown> => {
  if (typeof x !== 'object' || x === null) { return false; }
  if ('type' in x) {
    return x.type === someSerializedType || x.type === noneSerializedType;
  } else {
    return false;
  }
}

/**
 * A helper class for providing compatibility with Redux DevTools.
 */
export class ReduxDevtoolsCompatibilityHelper {
  static replacer(_key: unknown, value: any): any | OptSerialized<unknown> {
    return isOpt(value) ? serialize(value) : value;
  }

  static reviver(_key: unknown, value: any): any {
    if (!value || typeof value !== 'object') { return value; }
    const deser = deserialize(value, isUnknown);
    if (deser.tag === 'failure') return value;
    return deser.value;
  }
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
export const serialize = <T>(x: Opt<T>): OptSerialized<T> => {
  if (x.isEmpty) {
    return {type: noneSerializedType};
  } else {
    return {
      type: someSerializedType,
      value: x.orCrash('failed to extract value from Some'),
    };
  }
};

export type DeserializationSuccess<T> = {
  tag: 'success',
  value: Opt<T>,
}

export type DeserializationFailure = {
  tag: 'failure',
  reason: string,
}

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
export const deserialize = <T>(x: unknown, guard: (x: unknown) => x is T): DeserializationResult<T> => {
  if (!isOptSerialized(x)) return {tag: 'failure', reason: 'not OptSerialized'};
  switch (x.type) {
    case noneSerializedType:
      return { tag: 'success', value: none };
    case someSerializedType:
      if (!guard(x.value)) return { tag: 'failure', reason: 'failed to validate inner type' };
      return { tag: 'success', value: some(x.value) };
  }
}

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
export const deserializeOrCrash = <T>(x: unknown, guard: (x: unknown) => x is T): Opt<T> => {
  const deser = deserialize(x, guard);
  if (deser.tag === 'failure') { throw new Error(deser.reason); }
  return deser.value;
}

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
export const deserializeUnsafe = (x: unknown): Opt<unknown> => {
  return tryRun(() => deserializeOrCrash(x, isUnknown)).join();
}

const isNoneValue = (x: any): x is EmptyValue => {
  return x === undefined || x === null || Number.isNaN(x);
};

/**
 * Single global instance of {@link None}.
 */
export const none: None<any> = Object.freeze(new None());

/**
 * Constructs {@link Some}.
 *
 * Warning: Usually it is {@link opt} you are looking for.
 * Only in rare cases you want to have for example `Some(undefined)`.
 * @param x
 */
export const some = <T>(x: T) => Object.freeze(new Some(x));

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
export const opt = <T>(x: T | undefined | null): OptSafe<T> =>
  isNoneValue(x) ? none : some(x as WithoutOptValues<T>);

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
export const optFalsy = <T>(x: T | undefined | null | '' | false | 0): OptSafe<T> => x ? some(x as WithoutOptValues<T>) : none;

/**
 * For empty array (`[]`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export const optEmptyArray = <T, A extends readonly T[] | T[]>(x: A | undefined | null): OptSafe<A> => opt(x).filter(y => y.length > 0);

/**
 * For empty object (`{}`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export const optEmptyObject = <T extends object>(x: T | undefined | null): OptSafe<T> =>
  opt(x).filter(y => Object.keys(y).length !== 0);

/**
 * For empty string (`''`) returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export const optEmptyString = <T>(x: T | undefined | null | ''): OptSafe<T> => x === '' ? none : opt(x);

/**
 * For a number `0` returns {@link None}, otherwise acts same as {@link opt}.
 * @param x
 */
export const optZero = <T>(x: T | undefined | null | 0): OptSafe<T> => x === 0 ? none : opt(x);

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
export const optNegative = (x: number | undefined | null): OptSafe<number> => typeof x === 'number' && x < 0 ? none : opt(x);

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
export const optArrayOpt = <T>(xs: (T | EmptyValue)[] | EmptyValue): OptSafe<T[]> =>
  opt(xs).mapFlow(ys => ys.map(opt), catOpts)

/**
 * Is given value an instance of {@link Opt}?
 * @param x
 */
export const isOpt = (x: unknown): x is Opt<unknown> => x instanceof Opt;

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
export const ap = <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>): Opt<B> =>
  oa.caseOf(a => of.map(f => f(a)), () => none as unknown as Opt<B>);

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
export const apFn = <A, B>(f: (_: A) => B) => (oa: Opt<A>): Opt<B> => ap(opt(f))(oa);

/**
 * Transforms array of opts into an array where {@link None}s are omitted and {@link Some}s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
export const catOpts = <A>(xs: readonly Opt<A>[]): A[] =>
  xs.reduce((acc, x) => x.caseOf(y => [...acc, y], () => acc), [] as A[]);

/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
export const mapOpt = <A, B>(f: (_: A) => Opt<B>) => (xs: readonly A[]): B[] => catOpts(xs.map(f));

/**
 * Unwraps one level of nested {@link Opt}s. Similar to `flatten` in other libraries or languages.
 * ```ts
 * joinOpt(some(none)) // None
 * joinOpt(some(some(1))) // Some(1)
 * ```
 * @param x
 * @see {@link Opt.join}
 */
export const joinOpt = <T>(x: Opt<Opt<T>>): Opt<T> => x.caseOf<Opt<T>>(y => y, () => none);

/**
 * @see {@link Opt.fromArray}
 */
export const fromArray = Opt.fromArray;

/**
 * @see {@link Opt.toArray}
 */
export const toArray = <T>(x: Opt<T>): [] | [T] => x.toArray();

/**
 * @see {@link Opt.fromObject}
 */
export const fromObject = Opt.fromObject;

/**
 * @see {@link Opt.toObject}
 */
export const toObject =
  <K extends string = 'value'>(k?: K) =>
    <T>(x: Opt<T>): Record<K, T | null> =>
      x.toObject(k as any);

type MapFn = <T, U>(f: (_: T) => U) => <I extends (Opt<T> | readonly T[]), O extends (I extends Opt<T> ? Opt<U> : U[])>(x: I) => O;

/**
 * Same as {@link Opt.map}, but also supports arrays.
 * @see {@link Opt.map}
 */
export const map: MapFn = (f: any) => (x: any) => x.map(f);

/** @see {@link Opt.mapFlow} */
export const mapFlow: MapFlowFn = (...fs: any[]) => <T>(x: Opt<T>) => fs.reduce((acc, x) => acc.map(x), x);

// type FlatMapFn = <T, U, O extends (Opt<U> | U[])>(f: (_: T) => O) => <I extends (O extends Opt<U> ? Opt<T> : T[])>(x: I) => O;
interface FlatMapFn {
  <T, U>(f: (_: T) => readonly U[]): (x: readonly T[]) => U[];

  <T, U>(f: (_: T) => Opt<U>): (x: Opt<T>) => Opt<U>;
}

/**
 * Same as {@link Opt.flatMap}, but also supports arrays.
 * @see {@link Opt.flatMap}
 */
export const flatMap: FlatMapFn = (f: any) => (x: any) => isOpt(x) ? x.flatMap(f) : x.map(f).flat();

/** @see {@link Opt.flatMap} */
export const chain = flatMap;

/** @see {@link Opt.act} */
export const act: ActFn = <I>(...fs: any[]) => (x: Opt<I>) => fs.reduce((acc, x) => acc.chain(x), x);

/** @see {@link Opt.chainFlow} */
export const chainFlow: ActFn = act;

/** @see {@link Opt.chainToOpt} */
export const chainToOpt = <T, U>(f: (_: T) => U | undefined | null) => (x: Opt<T>): OptSafe<U> => x.chainToOpt(f);

/** @see {@link Opt.actToOpt} */
export const actToOpt: ActToOptFn = <I>(...fs: any[]) => (x: Opt<I>) => fs.reduce((acc, x) => acc.chainToOpt(x), x);

/** @see {@link Opt.chainToOptFlow} */
export const chainToOptFlow: ActToOptFn = actToOpt;

/** @see {@link Opt.someOrCrash} */
export const someOrCrash = <T>(msg: string) => (x: Opt<T>): Some<T> => x.someOrCrash(msg);

/** @see {@link Opt.orCrash} */
export const orCrash = <T>(msg: string) => (x: Opt<T>): T => x.orCrash(msg);

/** @see {@link Opt.orUndef} */
export const orUndef = <T>(x: Opt<T>): T | undefined => x.orUndef();

/** @see {@link Opt.orNull} */
export const orNull = <T>(x: Opt<T>): T | null => x.orNull();

/** @see {@link Opt.orFalse} */
export const orFalse = <T>(x: Opt<T>): T | false => x.orFalse();

/** @see {@link Opt.orTrue} */
export const orTrue = <T>(x: Opt<T>): T | true => x.orTrue();

/** @see {@link Opt.orNaN} */
export const orNaN = <T>(x: Opt<T>): T | number => x.orNaN();

/** @see {@link Opt.caseOf} */
export const caseOf = <T, R>(onSome: (x: T) => R) => (onNone: () => R) => (x: Opt<T>): R => x.caseOf(onSome, onNone);

/** @see {@link Opt.onBoth} */
export const onBoth = <T>(onSome: (x: T) => void) => (onNone: () => void) => (x: Opt<T>): Opt<T> => x.onBoth(onSome, onNone);

/**
 * Similar to {@link Opt.pipe}, but the first argument is the input.
 * Supports arbitrary input type, not just {@link Opt}.
 * @see {@link Opt.pipe}
 */
export const pipe: PipeFn = <I>(x: I, ...fs: any[]) => fs.reduce((acc, y) => y(acc), x);

/** @see {@link Opt.contains} */
export const contains = <T>(y: T) => (x: Opt<T>): boolean => x.contains(y);

/** @see {@link Opt.exists} */
export const exists = <T>(y: (_: T) => boolean) => (x: Opt<T>): boolean => x.exists(y);

/** @see {@link Opt.forAll} */
export const forAll = <T>(p: (_: T) => boolean) => (x: Opt<T>): boolean => x.forAll(p);

/** @see {@link Opt.orElse} */
export const orElse = <T>(e: T) => (x: Opt<T>): T => x.orElse(e);

/** @see {@link Opt.orElseLazy} */
export const orElseLazy = <T>(e: () => T) => (x: Opt<T>): T => x.orElseLazy(e);

/** @see {@link Opt.orElseAny} */
export const orElseAny = <U>(e: U) => <T>(x: Opt<T>): T | U => x.orElseAny(e);

/** @see {@link Opt.alt} */
export const alt = <T>(def: Opt<T>) => (x: Opt<T>): Opt<T> => x.alt(def);

/** @see {@link Opt.altOpt} */
export const altOpt = <T>(def: T) => (x: Opt<T>): OptSafe<T> => x.altOpt(def);

/** @see {@link Opt.bimap} */
export const bimap = <T, U>(someF: (_: T) => U) => (noneF: () => U) => (x: Opt<T>): Opt<U> => x.bimap(someF, noneF);

/** @see {@link Opt.flatBimap} */
export const flatBimap = <T, U>(someF: (_: T) => Opt<U>) => (noneF: () => Opt<U>) => (x: Opt<T>): Opt<U> => x.flatBimap(someF, noneF);

interface ZipFn {
  <T>(other: Opt<T>): <U>(x: Opt<U>) => Opt<[T, U]>;

  <T>(other: readonly T[]): <U>(x: readonly U[]) => [T, U][];
}

const zipArray = <T, U>(a: readonly T[], b: readonly U[]): [T, U][] => [...Array(Math.min(b.length, a.length))].map((_, i) => [a[i], b[i]]);

/**
 * Same as {@link Opt.zip}, but also supports arrays.
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
 * @see {@link Opt.zip}
 */
export const zip: ZipFn = (x: any) => (other: any): any => isOpt(x) ? x.zip(other) : zipArray(x, other);

/** @see {@link Opt.zip3} */
export const zip3 = <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>): Opt<[T, A, B]> => x.zip3(a, b);

/** @see {@link Opt.zip4} */
export const zip4 =
  <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>): Opt<[T, A, B, C]> => x.zip4(a, b, c);

/** @see {@link Opt.zip5} */
export const zip5 =
  <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>): Opt<[T, A, B, C, D]> =>
    x.zip5(a, b, c, d);

type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | readonly T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];

/**
 * Same as {@link Opt.filter}, but also supports arrays.
 * @see {@link Opt.filter}
 */
export const filter: FilterFn = (p: any) => (x: any) => x.filter(p);

/** @see {@link Opt.noneIf} */
export const noneIf = <T>(predicate: (_: T) => boolean) => (x: Opt<T>): Opt<T> => x.noneIf(predicate);

/** @see {@link Opt.noneIfEmpty} */
export const noneIfEmpty = <T extends PossiblyEmpty>(x: Opt<T>): Opt<WithoutPossiblyEmptyEmptyValues<T>> =>
  x.noneIfEmpty();

/** @see {@link Opt.noneWhen} */
export const noneWhen = <T>(returnNone: boolean) => (x: Opt<T>): Opt<T> => x.noneWhen(returnNone);

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
export const count: CountFn = (p: any) => (x: any): any => {
  if (isOpt(x)) { return x.count(p); }
  if (isArray(x)) { return x.filter(p).length; }
  throw new Error(`Invalid input to count, only Opt and Array are supported: ${JSON.stringify(x)}`);
};

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
export const find = <T>(predicate: (_: T) => boolean) => (xs: readonly T[]): Opt<T> => opt(xs.find(x => predicate(x)));

/** @see {@link Opt.narrow} */
export const narrow = <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>): Opt<U> => x.narrow(guard);

/** @see {@link Opt.narrowOrCrash} */
export const narrowOrCrash = <T, U>(guard: (value: any) => value is U, crashMessage?: string) => (x: Opt<T>): Opt<U> => x.narrowOrCrash(guard, crashMessage);

/**
 * Same as {@link Opt.print}, but supports arbitrary argument types.
 * @see {@link Opt.print}
 */
export const print = (tag?: string) => <T>(x: T): T => {
  if (isOpt(x)) {
    x.print(tag);
  } else {
    debugPrint(tag, x);
  }
  return x;
};

/** @see {@link Opt.equals} */
export const equals = <T>(other: Opt<T>, comparator: EqualityFunction = refCmp) => (x: Opt<T>): boolean =>
  x.equals(other, comparator);

/** @see {@link Opt.prop} */
export const prop = < //
  T extends object, //
  K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never //
  >(key: K) => (x: Opt<T>): OptSafe<T[K]> =>
  x.prop(key);

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
export const propNaked = < //
  T extends object | EmptyValue, //
  K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never //
>(key: K) => (x: T | EmptyValue): OptSafe<T[K]> => opt(x).prop(key);

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
export const propOrCrash = < //
  T extends object, //
  P extends Opt<T> | T = Opt<T> | T, //
  K extends (P extends Opt<T> ? (T extends object ? keyof T : never) : (P extends object ? keyof P : never)) //
    = P extends Opt<T> ? (T extends object ? keyof T : never) : (P extends object ? keyof P : never) //
  >(key: K) => (x: P): WithoutOptValues<T[K]> =>
  ((isOpt(x) ? x : opt(x)) as Opt<T>).propOrCrash(key as any) as unknown as WithoutOptValues<T[K]>;

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
export const genNakedPropOrCrash = <T extends object>(obj: T) => {
  const o = opt(obj);
  return <K extends keyof T>(k: K) => o.propOrCrash(k as any);
};

/** @see {@link Opt.swap} */
export const swap = <U>(newValue: U) => <T>(x: Opt<T>): Opt<U> => x.swap(newValue);

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
export const flow: FlowFn = (...fs: any[]) => (x: any) => fs.reduce((acc, x) => x(acc), x);

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
export const compose: ComposeFn = (...fs: any[]) => (x: any) => fs.reduceRight((acc, x) => x(acc), x);

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
export const curryTuple: CurryTupleFn = f => a => b => f([a, b]);

type CurryTuple3Fn = <A, B, C, D>(_: (_: [A, B, C]) => D) => (_: A) => (_: B) => (_: C) => D;
/**
 * Transforms the given function of three arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export const curryTuple3: CurryTuple3Fn = f => a => b => c => f([a, b, c]);

type CurryTuple4Fn = <A, B, C, D, E>(_: (_: [A, B, C, D]) => E) => (_: A) => (_: B) => (_: C) => (_: D) => E;
/**
 * Transforms the given function of four arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export const curryTuple4: CurryTuple4Fn = f => a => b => c => d => f([a, b, c, d]);

type CurryTuple5Fn = <A, B, C, D, E, F>(_: (_: [A, B, C, D, E]) => F) => (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F;
/**
 * Transforms the given function of five arguments from "tuple curried" format to curried one.
 * @see {@link curryTuple}
 * @param f
 */
export const curryTuple5: CurryTuple5Fn = f => a => b => c => d => e => f([a, b, c, d, e]);


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
export const uncurryTuple: UncurryTupleFn = f => ([a, b]) => f(a)(b);

type UncurryTuple3Fn = <A, B, C, D>(_: (_: A) => (_: B) => (_: C) => D) => (_: [A, B, C]) => D;
/**
 * Transforms the given function of three arguments from curried format to "tuple curried" which can be used with {@link Opt.zip3}.
 * @see {@link uncurryTuple}
 * @param f
 */
export const uncurryTuple3: UncurryTuple3Fn = f => ([a, b, c]) => f(a)(b)(c);

type UncurryTuple4Fn = <A, B, C, D, E>(_: (_: A) => (_: B) => (_: C) => (_: D) => E) => (_: [A, B, C, D]) => E;
/**
 * Transforms the given function of four arguments from curried format to "tuple curried" which can be used with {@link Opt.zip4}.
 * @see {@link uncurryTuple}
 * @param f
 */
export const uncurryTuple4: UncurryTuple4Fn = f => ([a, b, c, d]) => f(a)(b)(c)(d);

type UncurryTuple5Fn = <A, B, C, D, E, F>(_: (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F) => (_: [A, B, C, D, E]) => F;
/**
 * Transforms the given function of five arguments from curried format to "tuple curried" which can be used with {@link Opt.zip5}.
 * @see {@link uncurryTuple}
 * @param f
 */
export const uncurryTuple5: UncurryTuple5Fn = f => ([a, b, c, d, e]) => f(a)(b)(c)(d)(e);

type PossiblyEmpty =
  Opt<unknown> |
  unknown[] |
  null |
  undefined |
  Map<unknown, unknown> |
  Set<unknown> |
  object |
  string |
  number;

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
export const isEmpty = (x: PossiblyEmpty): boolean => {
  if (isOpt(x)) { return x.isEmpty; }
  if (Array.isArray(x)) { return x.length === 0; }
  if (x === null || x === undefined) { return true; }
  if (x instanceof Map || x instanceof Set) { return x.size === 0; }
  if (typeof x === 'object') { return Object.getOwnPropertyNames(x).length === 0; }
  if (typeof x === 'string') { return x === ''; }
  if (typeof x === 'number') { return Number.isNaN(x); }
  throw new Error(`Unexpected input type: ${typeof x}`);
};

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
export const nonEmpty = (x: PossiblyEmpty): boolean => !isEmpty(x);

/** @alias {@link nonEmpty} */
export const isFull = (x: PossiblyEmpty): boolean => nonEmpty(x);

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
export const id = <T>(x: T): T => x;

type AtFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;

/**
 * Same as {@link Opt.at}, but also supports unwrapped arrays.
 * @see {@link Opt.at}
 * @param index
 */
export const at: (index: number) => AtFn = (index: number) => (x: any): any =>
  (isOpt(x) ? x : opt(x)).at(index);

type HeadFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;

/**
 * Returns the first element of an array or fist character of a string.
 * @param xs
 */
export const head: HeadFn = (xs: any): any => opt(xs).headIn();

type HeadInFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : (T extends string ? Opt<string> : never)>(x: EmptyValue | Opt<T>) => R;

/**
 * Same as {@link Opt.headIn}.
 * @see {@link Opt.headIn}
 * @param x
 */
export const headIn: HeadInFn = (x: any): any => x.headIn();

type LastFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : Opt<string>>(x: EmptyValue | T) => R;

/** Returns the last element of an array or last character of a string. */
export const last: LastFn = (xs: any): any => opt(xs).lastIn();

type LastInFn = <T, R = T extends readonly (infer A)[] ? OptSafe<A> : (T extends string ? Opt<string> : never)>(x: EmptyValue | Opt<T>) => R;

/**
 * Same as {@link Opt.lastIn}.
 * @see {@link Opt.lastIn}
 * @param x
 */
export const lastIn: LastInFn = (x: any): any => x.lastIn();

interface ZipToOptArrayFn {
  <A, B>(xs: readonly [A, B]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>]>;

  <A, B, C>(xs: readonly [A, B, C]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>]>;

  <A, B, C, D>(xs: readonly [A, B, C, D]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>]>;

  <A, B, C, D, E>(xs: readonly [A, B, C, D, E]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>, WithoutOptValues<E>]>;
}

const lenToZipFn = {
  2: uncurryTuple(zip),
  3: uncurryTuple3(zip3),
  4: uncurryTuple4(zip4),
  5: uncurryTuple5(zip5),
};
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
export const zipToOptArray: ZipToOptArrayFn = (xs: readonly unknown[]): Opt<any> =>
  opt((lenToZipFn as any)[xs.length]).orCrash(`Invalid input array length ${xs.length}`)(xs.map(opt));

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
export const testRe = (re: RegExp) => (x: string): boolean => re.test(x);

/** @see {@link Opt.testReOrFalse} */
export const testReOrFalse = (re: RegExp) => (x: Opt<string>): boolean => x.testReOrFalse(re);

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
export const tryRun = <T>(f: () => T): Opt<T> => {
  try {
    return opt(f());
  } catch (e) {
    return none;
  }
};

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
export const parseJson = (x: string): Opt<unknown> => tryRun(() => JSON.parse(x));

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
export const parseInt = (x: string): Opt<number> => opt(Number.parseInt(x, 10));

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
export const parseFloat = (x: string): Opt<number> => opt(Number.parseFloat(x));

/** @see {@link Opt.apply} */
export const apply = < //
  T extends AnyFunc,
  R extends ReturnType<T>,
  A extends Parameters<T> //
  >(...args: A) => (x: Opt<T>): Opt<R> => x.apply(...args);

/** @see {@link Opt.onFunc} */
export const onFunc = < //
  T extends AnyFunc,
  A extends Parameters<T> //
  >(...args: A) => (x: Opt<T>): Opt<T> => x.onFunc(...args);

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
export const isOrCrash = <T>(guard: (x: unknown) => x is T, msg = 'invalid value') => (x: unknown): T =>
  some(x).narrow(guard).orCrash(msg);

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
export const assertType: AssertTypeFunc = (x, guard, msg = 'invalid value') => {
  isOrCrash(guard, msg)(x);
};

/** Returns the minimum value in an array. */
export const min = <R>(x: readonly R[]): OptSafe<R> => opt(x).minIn();

/** @see {@link Opt.minIn} */
export const minIn = <R>(x: Opt<readonly R[]>): OptSafe<R> => x.minIn();

/** Returns the maximum value in an array. */
export const max = <R>(x: readonly R[]): OptSafe<R> => opt(x).maxIn();

/** @see {@link Opt.maxIn} */
export const maxIn = <R>(x: Opt<readonly R[]>): OptSafe<R> => x.maxIn();

// generate a function of two curried optional arguments
// common - two some values lead to call of op, two nones returns none
// all mode - all operands must be some, otherwise return none
// any mode - if one operand is none, return the other (non-none) one
const gen2Op = <T>(mode: 'all' | 'any', op: (a: T) => (b: T) => T): ((a: T | EmptyValue) => (b: T | EmptyValue) => Opt<T>) =>
  (x: T | EmptyValue) => (y: T | EmptyValue): Opt<T> => {
    const ox = opt(x);
    const oy = opt(y);
    const allRes = ox.zip(oy).map(uncurryTuple(op));
    const anyResGen = () => allRes.alt(ox).alt(oy);
    return mode === 'all' ? allRes : anyResGen();
  };

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
export const min2Num = (a: number) => (b: number): number => a < b ? a : b;

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
export const min2All = gen2Op('all', min2Num);

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
export const min2Any = gen2Op('any', min2Num);

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
export const max2Num = (a: number) => (b: number): number => a > b ? a : b;

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
export const max2All = gen2Op('all', max2Num);

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
export const max2Any = gen2Op('any', max2Num);

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
export const clamp = (minValue: number | EmptyValue) => (maxValue: number | EmptyValue) => (x: number | EmptyValue): Opt<number> =>
  opt(x).act(max2Any(minValue), min2Any(maxValue));

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
export const not = (x: boolean): boolean => !x;

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
export const and = (x: boolean) => (y: boolean): boolean => x && y;

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
export const or = (x: boolean) => (y: boolean): boolean => x || y;

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
export const xor = (x: boolean) => (y: boolean): boolean => x !== y;

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
export const bool = <T>(falseValue: T) => (trueValue: T) => (cond: boolean) => cond ? trueValue : falseValue;

// TODO: boolLazy

// TODO: imperative variant of bool?

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
export const inc = (x: number): number => x + 1;

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
export const dec = (x: number): number => x - 1;

/**
 * Prepends a string to a given string.
 *
 * @example
 * ```ts
 * prependStr('foo')('bar') // 'foobar'
 * opt('bar').map(prependStr('foo')) // Some('foobar')
 * ```
 */
export const prependStr = <P extends string>(prefix: P) => <X extends string>(str: X): `${P}${X}` =>
  `${prefix}${str}` as `${P}${X}`;

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
export const appendStr = <S extends string>(suffix: S) => <X extends string>(str: X): `${X}${S}` =>
  `${str}${suffix}` as `${X}${S}`;

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
export const crash = (msg: string): never => { throw new Error(msg); };

// TODO: customizable comparator
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
export const eq = <T>(a: T) => (b: T) => a === b;

// TODO: customizable comparator
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
export const eqAny = (a: unknown) => (b: unknown) => a === b;

/**
 * A no-operation function that simply returns `undefined`.
 * Can be used as a placeholder callback.
 *
 * @returns undefined
 */
// eslint-disable-next-line @typescript-eslint/no-empty-function
export const noop = (..._args: unknown[]): void => {};
