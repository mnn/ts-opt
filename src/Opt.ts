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
type WithoutOptValues<T> = NonNullable<T>;

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

export const isString = (x: any): x is string => typeof x === 'string';
export const toString = (x: { toString(): string }): string => x.toString();
export const isArray = (x: any): x is unknown[] => Array.isArray(x);

const debugPrint = (tag?: string, ...xs: unknown[]) => {
  // tslint:disable-next-line:no-console
  console.log(...[...opt(tag).map(x => [`[${x}]`]).orElse([]), ...xs]);
};

/**
 * Generic container class. It either holds exactly one value - [[Some]], or no value - [[None]] (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent API" in OOP or [[pipe|chain of reverse applications]] in FP).
 *
 * @typeparam T Wrapped value type.
 */
export abstract class Opt<T> {
  /**
   * `false` for [[Some]], `true` for [[None]].
   */
  abstract get isEmpty(): boolean;

  /**
   * `false` for [[Some]], `true` for [[None]].
   */
  get nonEmpty(): boolean { return !this.isEmpty; }

  /**
   * Is this instance of [[Some]]?
   */
  isSome(): this is Some<T> { return this.nonEmpty; }

  /**
   * Is this instance of [[None]]?
   */
  isNone(): this is None<T> { return this.isEmpty; }

  /**
   * `1` for [[Some]], `0` for [[None]].
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
  static fromArray<T>(x: [] | [T]): Opt<T> { return opt(x[0]); }

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
   * @see [[onSome]] for imperative variant (for use with callback)
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
  mapFlow: MapFlowInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.map(x), this);

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
  chain<U>(f: (_: T) => Opt<U>): Opt<U> { return this.flatMap(f); }

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
  act: ActInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.chain(x), this);
  /**
   * Alias of [[act]]
   * @param args
   */
  chainFlow: ActInClassFn<T> = (...args: any[]) => (this.act as any)(...args);

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
  chainToOpt<U>(f: (_: T) => U | undefined | null): Opt<U> { return this.flatMap(x => opt(f(x))); }

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
  actToOpt: ActToOptInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => acc.chainToOpt(x), this);

  /**
   * Alias of [[actToOpt]].
   * @param args
   */
  chainToOptFlow: ActToOptInClassFn<T> = (...args: any[]) => (this.act as any)(...args);

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
   * ```ts
   * some(1).caseOf(x => x + 1, () => 0) // 2
   * none.caseOf(x => x + 1, () => 0) // 0
   * ```
   *
   * @see [[onBoth]] for imperative version
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
   *
   * @param onSome
   * @param onNone
   */
  abstract onBoth(onSome: (x: T) => void, onNone: () => void): Opt<T>;

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
  pipe: PipeInClassFn<T> = (...fs: any[]) => fs.reduce((acc, x) => x(acc), this);

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
   *
   * @example
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
   *
   * @example
   * ```ts
   * opt(1).noneIf(x => x > 0); // None
   * opt(-1).noneIf(x => x > 0); // Some(-1)
   * ```
   * @see [[filter]]
   * @param predicate
   */
  noneIf(predicate: (_: T) => boolean): Opt<T> {
    return this.filter(x => !predicate(x));
  }

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
   * @see [[count]]
   * @param predicate
   */
  count(predicate: (_: T) => boolean): 0 | 1 {
    return this.filter(predicate).length;
  }

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
  widen<U, R extends SuperUnionOf<U, T> = SuperUnionOf<U, T>>(): Opt<R> {
    return this as unknown as Opt<R>;
  }

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

  /**
   * Constructs a function which returns a value for [[Some]] or an empty value for [[None]] (default is `null`).
   * Optionally takes an empty value as a parameter.
   *
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
      const e = arguments[0];
      return () => this.isSome() ? this.value : e;
    }
    return () => this.orNull();
  };

  /**
   * Swaps value inside (for [[None]] it's noop).
   *
   * ```ts
   * opt(1).swap('a') // Some('a')
   * none.swap('a') // None
   * ```
   *
   * Same as `map(const(newValue))`.
   * @param newValue
   */
  abstract swap<U>(newValue: U): Opt<U>;

  /**
   * Get an item at given index of an array wrapped in [[Opt]].
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
   * ```
   *
   * @param index
   */
  abstract at<R extends (T extends (infer A)[] ? A : never)>(index: number): Opt<R>;

  /**
   * Get a first item of an array.
   *
   * @example
   * ```ts
   * opt([1, 2, 3]).head() // Some(1)
   * opt([]).head() // None
   * opt(null).head() // None
   * ```
   */
  head<R extends (T extends (infer A)[] ? A : never)>(): Opt<R> { return this.at(0); }

  /**
   * Get a last item of an array.
   *
   * @example
   * ```ts
   * opt([1, 2, 3]).last() // Some(3)
   * opt([]).last() // None
   * opt(null).last() // None
   * ```
   */
  last<R extends (T extends (infer A)[] ? A : never)>(): Opt<R> { return this.at(-1); }

  /**
   * A convenience function to test this (`Opt<string>`) against a given regular expression.
   *
   * @example
   * ```ts
   * opt('a').testReOrFalse(/a/) // true
   * opt('b').testReOrFalse(/a/)) // false;
   * ```
   *
   * @param re Regular expression
   */
  testReOrFalse<R extends (T extends string ? boolean : OperationNotAvailable<T, string>)>(re: RegExp): R {
    return this.narrow(isString).someOrCrash(`testReOrFalse only works on Opt<string>`).map(testRe(re)).orFalse() as R;
  }

  /**
   * No-op terminator used to end imperative chains.
   *
   * @example
   * ```ts
   * const f = (x: unknown): void => opt(x).onBoth(noop, noop).end;
   * // same as
   * const g = (x: unknown): void => { opt(x).onBoth(noop, noop); };
   * ```
   */
  get end(): void { return undefined; }
}

/**
 * Empty [[Opt]].
 * @notExported
 * @see [[Opt]]
 */
class None<T> extends Opt<T> {
  readonly '@@type' = noneSymbol;

  get isEmpty(): boolean { return true; }

  toArray(): [] | [T] { return []; }

  flatMap<U>(_f: (_: T) => Opt<U>): Opt<U> { return none as unknown as Opt<U>; }

  map<U>(): Opt<U> { return none as unknown as Opt<U>; }

  orCrash(msg: string): T { throw new Error(msg); }

  /**
   * @deprecated Please use [[someOrCrash]] instead
   */
  optOrCrash(msg: string): Opt<T> { throw new Error(msg); }

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

  orElseOpt(def: Opt<T>): Opt<T> { return def; }

  bimap<U>(_someF: (_: T) => U, noneF: () => U): Opt<U> { return opt(noneF()); }

  flatBimap<U>(_someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U> { return noneF(); }

  toString(): string { return 'None'; }

  zip<U>(_other: Opt<U>): Opt<[T, U]> { return none; }

  zip3<X, Y>(_x: Opt<X>, _y: Opt<Y>): Opt<[T, X, Y]> { return none; }

  zip4<X, Y, Z>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>): Opt<[T, X, Y, Z]> { return none; }

  zip5<X, Y, Z, ZZ>(_x: Opt<X>, _y: Opt<Y>, _z: Opt<Z>, _zz: Opt<ZZ>): Opt<[T, X, Y, Z, ZZ]> { return none; }

  filter(_predicate: (_: T) => boolean): Opt<T> { return none; }

  narrow<U>(_guard: (value: any) => value is U): Opt<U> { return this as unknown as Opt<U>; }

  print(tag?: string): Opt<T> {
    debugPrint(tag, 'None');
    return this;
  }

  equals(other: Opt<T>, _comparator: EqualityFunction = refCmp): boolean {
    return other.isEmpty;
  }

  prop<K extends (T extends object ? keyof T : never)>(_key: K): Opt<WithoutOptValues<T[K]>> { return none; }

  swap<U>(_newVal: U): Opt<U> {
    return none;
  }

  at<R extends (T extends (infer A)[] ? A : never)>(_index: number): Opt<R> {
    return none;
  }
}

/**
 * [[Opt]] with a value inside.
 * @notExported
 * @see [[Opt]]
 */
class Some<T> extends Opt<T> {
  readonly '@@type' = someSymbol;

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

  /**
   * @deprecated Please use [[someOrCrash]] instead
   */
  optOrCrash(_msg: string): Opt<T> { return this; }

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

  orElseOpt(_def: Opt<T>): Opt<T> { return this; }

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

  print(tag?: string): Opt<T> {
    debugPrint(tag, 'Some:', this._value);
    return this;
  }

  equals(other: Opt<T>, comparator: EqualityFunction = refCmp): boolean {
    if (other.isEmpty) { return false; }
    return comparator(this._value, other.orCrash('Some expected'));
  }

  prop<K extends (T extends object ? keyof T : never)>(key: K): Opt<WithoutOptValues<T[K]>> { return opt(this._value[key]) as Opt<WithoutOptValues<T[K]>>; }

  swap<U>(newVal: U): Opt<U> {
    return some(newVal);
  }

  at<R extends (T extends (infer A)[] ? A : never)>(index: number): Opt<R> {
    const val = this._value;
    if (Array.isArray(val)) {
      const processedIndex = (index < 0 ? val.length : 0) + index;
      return opt(val[processedIndex]);
    } else {
      throw new Error(`\`Opt#at\` can only be used on arrays`);
    }
  }
}

const someSerializedType = 'Opt/Some';
const noneSerializedType = 'Opt/None';
type OptSerialized =
  {
    type: typeof noneSerializedType;
  } | {
    type: typeof someSerializedType,
    value: any,
  };

export class ReduxDevtoolsCompatibilityHelper {
  static replacer(_key: unknown, value: any): any | OptSerialized {
    if (isOpt(value)) {
      const res: OptSerialized =
        value.isEmpty ? {type: noneSerializedType} as const : {
          type: someSerializedType,
          value: value.orCrash('failed to extract value from Some'),
        } as const;
      return res;
    } else {
      return value;
    }
  }

  static reviver(_key: unknown, value: any): any {
    if (!value || typeof value !== 'object') { return value; }
    switch (value.type) {
      case noneSerializedType:
        return none;
      case someSerializedType:
        return some(value.value);
      default:
        return value;
    }
  }
}

const isNoneValue = (x: any): boolean => {
  return x === undefined || x === null || Number.isNaN(x);
};

/**
 * Single global instance of [[None]].
 */
export const none: None<any> = Object.freeze(new None());

/**
 * Constructs [[Some]].
 * Usually it is [[opt]] you are looking for (only in rare cases you want to have for example `Some(undefined)`).
 * @param x
 */
export const some = <T>(x: T) => Object.freeze(new Some(x));

/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
export const opt = <T>(x: T | undefined | null): Opt<T> => isNoneValue(x) ? none : some(x as T);

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
export const optFalsy = <T>(x: T | undefined | null | '' | false | 0): Opt<T> => x ? some(x as T) : none;

/**
 * For empty array (`[]`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export const optEmptyArray = <T>(x: T[] | undefined | null): Opt<T[]> => opt(x).filter(y => y.length > 0);

/**
 * For empty object (`{}`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export const optEmptyObject = <T extends object>(x: T | undefined | null): Opt<T> =>
  opt(x).filter(y => Object.keys(y).length !== 0);

/**
 * For empty string (`''`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export const optEmptyString = <T>(x: T | undefined | null | ''): Opt<T> => x === '' ? none : opt(x);

/**
 * For a number `0` returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
export const optZero = <T>(x: T | undefined | null | 0): Opt<T> => x === 0 ? none : opt(x);

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
export const optNegative = (x: number | undefined | null): Opt<number> => typeof x === 'number' && x < 0 ? none : opt(x);

/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
export const isOpt = (x: unknown): x is Opt<unknown> => x instanceof Opt;

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
export const ap = <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>): Opt<B> =>
  oa.caseOf(a => of.map(f => f(a)), () => none as unknown as Opt<B>);

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
export const apFn = <A, B>(f: (_: A) => B) => (oa: Opt<A>): Opt<B> => ap(opt(f))(oa);

/**
 * Transforms array of opts into an array where [[None]]s are omitted and [[Some]]s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
export const catOpts = <A>(xs: Opt<A>[]): A[] =>
  xs.reduce((acc, x) => x.caseOf(y => [...acc, y], () => acc), [] as A[]);

/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
export const mapOpt = <A, B>(f: (_: A) => Opt<B>) => (xs: A[]): B[] => catOpts(xs.map(f));

/**
 * Unwraps one level of nested [[Opt]]s. Similar to `flatten` in other libraries or languages.
 * ```ts
 * joinOpt(some(none)) // None
 * joinOpt(some(some(1))) // Some(1)
 * ```
 * @param x
 */
export const joinOpt = <T>(x: Opt<Opt<T>>): Opt<T> => x.caseOf<Opt<T>>(y => y, () => none);

/**
 * @see [[Opt.fromArray]]
 */
export const fromArray = Opt.fromArray;

/**
 * @see [[Opt.toArray]]
 */
export const toArray = <T>(x: Opt<T>): [] | [T] => x.toArray();

type MapFn = <T, U>(f: (_: T) => U) => <I extends (Opt<T> | T[]), O extends (I extends Opt<T> ? Opt<U> : U[])>(x: I) => O;

/**
 * Same as [[Opt.map]], but also supports arrays.
 * @see [[Opt.map]]
 */
export const map: MapFn = (f: any) => (x: any) => x.map(f);

/** @see [[Opt.mapFlow]] */
export const mapFlow: MapFlowFn = (...fs: any[]) => <T>(x: Opt<T>) => fs.reduce((acc, x) => acc.map(x), x);

// type FlatMapFn = <T, U, O extends (Opt<U> | U[])>(f: (_: T) => O) => <I extends (O extends Opt<U> ? Opt<T> : T[])>(x: I) => O;
interface FlatMapFn {
  <T, U>(f: (_: T) => U[]): (x: T[]) => U[];

  <T, U>(f: (_: T) => Opt<U>): (x: Opt<T>) => Opt<U>;
}

/**
 * Same as [[Opt.flatMap]], but also supports arrays.
 * @see [[Opt.flatMap]]
 */
export const flatMap: FlatMapFn = (f: any) => (x: any) => isOpt(x) ? x.flatMap(f) : x.map(f).flat();

/** @see [[Opt.flatMap]] */
export const chain = flatMap;

/** @see [[Opt.act]] */
export const act: ActFn = <I>(...fs: any[]) => (x: Opt<I>) => fs.reduce((acc, x) => acc.chain(x), x);

/** @see [[Opt.chainFlow]] */
export const chainFlow: ActFn = act;

/** @see [[Opt.chainToOpt]] */
export const chainToOpt = <T, U>(f: (_: T) => U | undefined | null) => (x: Opt<T>): Opt<U> => x.chainToOpt(f);

/** @see [[Opt.actToOpt]] */
export const actToOpt: ActToOptFn = <I>(...fs: any[]) => (x: Opt<I>) => fs.reduce((acc, x) => acc.chainToOpt(x), x);

/** @see [[Opt.chainToOptFlow]] */
export const chainToOptFlow: ActToOptFn = actToOpt;

/** @see [[Opt.someOrCrash]] */
export const someOrCrash = <T>(msg: string) => (x: Opt<T>): Some<T> => x.someOrCrash(msg);

/** @see [[Opt.orCrash]] */
export const orCrash = <T>(msg: string) => (x: Opt<T>): T => x.orCrash(msg);

/** @see [[Opt.orUndef]] */
export const orUndef = <T>(x: Opt<T>): T | undefined => x.orUndef();

/** @see [[Opt.orNull]] */
export const orNull = <T>(x: Opt<T>): T | null => x.orNull();

/** @see [[Opt.orFalse]] */
export const orFalse = <T>(x: Opt<T>): T | false => x.orFalse();

/** @see [[Opt.orTrue]] */
export const orTrue = <T>(x: Opt<T>): T | true => x.orTrue();

/** @see [[Opt.orNaN]] */
export const orNaN = <T>(x: Opt<T>): T | number => x.orNaN();

/** @see [[Opt.caseOf]] */
export const caseOf = <T, R>(onSome: (x: T) => R) => (onNone: () => R) => (x: Opt<T>): R => x.caseOf(onSome, onNone);

/** @see [[Opt.onBoth]] */
export const onBoth = <T>(onSome: (x: T) => void) => (onNone: () => void) => (x: Opt<T>): Opt<T> => x.onBoth(onSome, onNone);

/**
 * Similar to [[Opt.pipe]], but the first argument is the input.
 * Supports arbitrary input type, not just [[Opt]].
 * @see [[Opt.pipe]]
 */
export const pipe: PipeFn = <I>(x: I, ...fs: any[]) => fs.reduce((acc, y) => y(acc), x);

/** @see [[Opt.contains]] */
export const contains = <T>(y: T) => (x: Opt<T>): boolean => x.contains(y);

/** @see [[Opt.exists]] */
export const exists = <T>(y: (_: T) => boolean) => (x: Opt<T>): boolean => x.exists(y);

/** @see [[Opt.forAll]] */
export const forAll = <T>(p: (_: T) => boolean) => (x: Opt<T>): boolean => x.forAll(p);

/** @see [[Opt.orElse]] */
export const orElse = <T>(e: T) => (x: Opt<T>): T => x.orElse(e);

/** @see [[Opt.orElseOpt]] */
export const orElseOpt = <T>(def: Opt<T>) => (x: Opt<T>): Opt<T> => x.orElseOpt(def);

/** @see [[Opt.bimap]] */
export const bimap = <T, U>(someF: (_: T) => U) => (noneF: () => U) => (x: Opt<T>): Opt<U> => x.bimap(someF, noneF);

interface ZipFn {
  <T>(other: Opt<T>): <U>(x: Opt<U>) => Opt<[T, U]>;

  <T>(other: T[]): <U>(x: U[]) => [T, U][];
}

const zipArray = <T, U>(a: T[], b: U[]): [T, U][] => [...Array(Math.min(b.length, a.length))].map((_, i) => [a[i], b[i]]);

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
export const zip: ZipFn = (x: any) => (other: any): any => isOpt(x) ? x.zip(other) : zipArray(x, other);

/** @see [[Opt.zip3]] */
export const zip3 = <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>): Opt<[T, A, B]> => x.zip3(a, b);

/** @see [[Opt.zip4]] */
export const zip4 =
  <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>): Opt<[T, A, B, C]> => x.zip4(a, b, c);

/** @see [[Opt.zip5]] */
export const zip5 =
  <T>(x: Opt<T>) => <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>): Opt<[T, A, B, C, D]> =>
    x.zip5(a, b, c, d);

type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];

/**
 * Same as [[Opt.filter]], but also supports arrays.
 * @see [[Opt.filter]]
 */
export const filter: FilterFn = (p: any) => (x: any) => x.filter(p);

type CountFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? 0 | 1 : number;

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
export const count: CountFn = (p: any) => (x: any): any => {
  if (isOpt(x)) { return x.count(p); }
  if (isArray(x)) { return x.filter(p).length; }
  throw new Error(`Invalid input to count, only Opt and Array are supported: ${JSON.stringify(x)}`);
};

/** @see [[Opt.narrow]] */
export const narrow = <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>): Opt<U> => x.narrow(guard);

/**
 * Same as [[Opt.print]], but supports arbitrary argument types.
 * @see [[Opt.print]]
 */
export const print = (tag?: string) => <T>(x: T): T => {
  if (isOpt(x)) {
    x.print(tag);
  } else {
    debugPrint(tag, x);
  }
  return x;
};

/** @see [[Opt.equals]] */
export const equals = <T>(other: Opt<T>, comparator: EqualityFunction = refCmp) => (x: Opt<T>): boolean =>
  x.equals(other, comparator);

/** @see [[Opt.prop]] */
export const prop = <T extends object,
  K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never>(key: K) => (x: Opt<T>): Opt<WithoutOptValues<T[K]>> =>
  x.prop(key);

/** @see [[Opt.swap]] */
export const swap = <U>(newValue: U) => <T>(x: Opt<T>): Opt<U> => x.swap(newValue);

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
export const flow: FlowFn = (...fs: any[]) => (x: any) => fs.reduce((acc, x) => x(acc), x);

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
 * @see [[uncurryTuple]]
 * @param f
 */
export const curryTuple: CurryTupleFn = f => a => b => f([a, b]);

type CurryTuple3Fn = <A, B, C, D>(_: (_: [A, B, C]) => D) => (_: A) => (_: B) => (_: C) => D;
/**
 * Transforms the given function of three arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export const curryTuple3: CurryTuple3Fn = f => a => b => c => f([a, b, c]);

type CurryTuple4Fn = <A, B, C, D, E>(_: (_: [A, B, C, D]) => E) => (_: A) => (_: B) => (_: C) => (_: D) => E;
/**
 * Transforms the given function of four arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export const curryTuple4: CurryTuple4Fn = f => a => b => c => d => f([a, b, c, d]);

type CurryTuple5Fn = <A, B, C, D, E, F>(_: (_: [A, B, C, D, E]) => F) => (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F;
/**
 * Transforms the given function of five arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
export const curryTuple5: CurryTuple5Fn = f => a => b => c => d => e => f([a, b, c, d, e]);


type UncurryTupleFn = <A, B, C>(_: (_: A) => (_: B) => C) => (_: [A, B]) => C;
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
export const uncurryTuple: UncurryTupleFn = f => ([a, b]) => f(a)(b);

type UncurryTuple3Fn = <A, B, C, D>(_: (_: A) => (_: B) => (_: C) => D) => (_: [A, B, C]) => D;
/**
 * Transforms the given function of three arguments from curried format to "tuple curried" which can be used with [[Opt.zip3]].
 * @see [[uncurryTuple]]
 * @param f
 */
export const uncurryTuple3: UncurryTuple3Fn = f => ([a, b, c]) => f(a)(b)(c);

type UncurryTuple4Fn = <A, B, C, D, E>(_: (_: A) => (_: B) => (_: C) => (_: D) => E) => (_: [A, B, C, D]) => E;
/**
 * Transforms the given function of four arguments from curried format to "tuple curried" which can be used with [[Opt.zip4]].
 * @see [[uncurryTuple]]
 * @param f
 */
export const uncurryTuple4: UncurryTuple4Fn = f => ([a, b, c, d]) => f(a)(b)(c)(d);

type UncurryTuple5Fn = <A, B, C, D, E, F>(_: (_: A) => (_: B) => (_: C) => (_: D) => (_: E) => F) => (_: [A, B, C, D, E]) => F;
/**
 * Transforms the given function of five arguments from curried format to "tuple curried" which can be used with [[Opt.zip5]].
 * @see [[uncurryTuple]]
 * @param f
 */
export const uncurryTuple5: UncurryTuple5Fn = f => ([a, b, c, d, e]) => f(a)(b)(c)(d)(e);

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
export const isEmpty = (
  x: Opt<unknown> | unknown[] | null | undefined | Map<unknown, unknown> | Set<unknown> | object | string | number,
): boolean => {
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
export const nonEmpty = (
  x: Opt<unknown> | unknown[] | null | undefined | Map<unknown, unknown> | Set<unknown> | object | string | number,
): boolean => !isEmpty(x);

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

/**
 * Same as [[Opt.at]], but also supports unwrapped arrays.
 * @see [[Opt.at]]
 * @param index
 */
export const at = (index: number) => <T>(x: T[] | Opt<T[]>): Opt<T> => (isOpt(x) ? x : opt(x)).at(index);

/**
 * Same as [[Opt.head]], but also supports unwrapped arrays.
 * @see [[Opt.head]]
 * @param x
 */
export const head = <T>(x: T[] | Opt<T[]>): Opt<T> => (isOpt(x) ? x : opt(x)).head();

/**
 * Same as [[Opt.last]], but also supports unwrapped arrays.
 * @see [[Opt.last]]
 * @param x
 */
export const last = <T>(x: T[] | Opt<T[]>): Opt<T> => (isOpt(x) ? x : opt(x)).last();

interface ZipToOptArrayFn {
  <A, B>(xs: [A, B]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>]>;

  <A, B, C>(xs: [A, B, C]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>]>;

  <A, B, C, D>(xs: [A, B, C, D]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>]>;

  <A, B, C, D, E>(xs: [A, B, C, D, E]): Opt<[WithoutOptValues<A>, WithoutOptValues<B>, WithoutOptValues<C>, WithoutOptValues<D>, WithoutOptValues<E>]>;
}

const lenToZipFn = {
  2: uncurryTuple(zip),
  3: uncurryTuple3(zip3),
  4: uncurryTuple4(zip4),
  5: uncurryTuple5(zip5),
};
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
export const zipToOptArray: ZipToOptArrayFn = (xs: unknown[]): Opt<any> =>
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

/** @see [[Opt.testReOrFalse]] */
export const testReOrFalse = (re: RegExp) => (x: Opt<string>): boolean => x.testReOrFalse(re);

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
export const tryRun = <T>(f: () => T): Opt<T> => {
  try {
    return opt(f());
  } catch (e) {
    return none;
  }
};

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
export const parseJson = (x: string): Opt<unknown> => tryRun(() => JSON.parse(x));

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
export const parseInt = (x: string): Opt<number> => opt(Number.parseInt(x, 10));
