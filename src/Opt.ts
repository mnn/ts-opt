const someSymbol = Symbol('Some');
const noneSymbol = Symbol('None');

/**
 * @typeparam T Wrapped value type.
 */
export abstract class Opt<T> {
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
   * `false` for [[Some]], `true` for [[None]].
   */
  abstract get isEmpty(): boolean;

  /**
   * `false` for [[Some]], `true` for [[None]].
   */
  get nonEmpty(): boolean { return !this.isEmpty; }

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
   * `1` for [[Some]], `0` for [[None]].
   */
  get length(): number { return this.isEmpty ? 0 : 1; }

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
   * Returns value when [[Some]], throws error with `msg` otherwise.
   * @param msg Error message.
   */
  abstract orCrash(msg: string): T;

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
  abstract onSome(f: (x: T) => void): void;

  /**
   * Calls `f` on [[None]], does nothing for [[Some]].
   * @param f
   */
  abstract onNone(f: () => void): void;

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
  pipe<R>(f: (x: Opt<T>) => R): R { return f(this); }

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
   * some(1).orElse(some(2)) // Some(1)
   * none.orElse(some(2)) // Some(2)
   * none.orElse(none) // None
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
}

class None<T> extends Opt<T> {
  readonly '@@type' = noneSymbol;

  toArray(): [] | [T] { return []; }

  get isEmpty(): boolean { return true; }

  flatMap<U>(_f: (_: T) => Opt<U>): Opt<U> { return none as unknown as Opt<U>; }

  map<U>(): Opt<U> { return none as unknown as Opt<U>; }

  orCrash(msg: string): T { throw new Error(msg); }

  orNull(): T | null { return null; }

  orUndef(): T | undefined { return undefined; }

  orFalse(): false | T { return false; }

  orTrue(): true | T { return true; }

  orNaN(): number | T { return NaN; }

  caseOf<R>(_onSome: (x: T) => R, onNone: () => R): R {
    return onNone();
  }

  onNone(f: () => void): void { f(); }

  onSome(_f: (x: T) => void): void { }

  contains(_x: T): boolean { return false; }

  exists(_p: (x: T) => boolean): boolean { return false; }

  forAll(_p: (x: T) => boolean): boolean { return true; }

  orElse(def: T): T { return def; }

  orElseOpt(def: Opt<T>): Opt<T> { return def; }

  bimap<U>(_someF: (_: T) => U, noneF: () => U): Opt<U> { return opt(noneF()); }

  flatBimap<U>(_someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U> { return noneF(); }

  toString(): string { return 'None'; }
}

class Some<T> extends Opt<T> {
  readonly '@@type' = someSymbol;

  constructor(private _value: T) { super(); }

  toArray(): [] | [T] { return [this._value]; }

  get isEmpty(): boolean { return false; }

  flatMap<U>(f: (_: T) => Opt<U>): Opt<U> {
    return f(this._value);
  }

  map<U>(f: (_: T) => U): Opt<U> {
    return new Some(f(this._value));
  }

  orCrash(_msg: string): T { return this._value; }

  orNull(): T | null { return this._value; }

  orUndef(): T | undefined { return this._value; }

  orFalse(): false | T { return this._value; }

  orTrue(): true | T { return this._value; }

  orNaN(): number | T { return this._value; }

  caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R { return onSome(this._value); }

  contains(x: T): boolean { return this._value === x; }

  exists(p: (x: T) => boolean): boolean { return p(this._value); }

  forAll(p: (x: T) => boolean): boolean { return p(this._value); }

  onNone(_f: () => void): void { }

  onSome(f: (x: T) => void): void { f(this._value); }

  orElse(_def: T): T { return this._value; }

  orElseOpt(_def: Opt<T>): Opt<T> { return this; }

  bimap<U>(someF: (_: T) => U, _noneF: () => U): Opt<U> { return opt(someF(this._value)); }

  flatBimap<U>(someF: (_: T) => Opt<U>, _noneF: () => Opt<U>): Opt<U> { return someF(this._value); }

  toString(): string { return `Some(${JSON.stringify(this._value)})`; }
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
export const opt = <T>(x: T | undefined | null): Opt<T> => isNoneValue(x) ? none : new Some(x as T);

/**
 * For falsy values returns [[None]].
 * @param x
 */
export const optFalsy = <T>(x: T | undefined | null | ''): Opt<T> => x ? new Some(x as T) : none;

/**
 * For empty array (`[]`) returns [[None]].
 * @param x
 */
export const optEmptyArray = <T>(x: T[]): Opt<T[]> => x.length ? new Some(x) : none;

/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
export const isOpt = (x: unknown): x is Opt<unknown> => x instanceof Opt;
