// Do NOT split to multiple modules - it's not possible, since there would be cyclic dependencies..

const someSymbol = Symbol('Some');
const noneSymbol = Symbol('None');

export type EqualityFunction = <T>(a: T, b: T) => boolean;
const refCmp: EqualityFunction = <T>(a: T, b: T): boolean => a === b;

type NotObject<T> = T extends object ? never : T;
type SuperUnionOf<T, U> = Exclude<U, T> extends never ? NotObject<T> : never;

type WithoutOptValues<T> = NonNullable<T>;

// Don't modify manually, generated via utils/genFlowyThings
interface PipeInClassFn<T> {
  <R>(f1: (_: Opt<T>) => R): R;

  <A1, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => R): R;

  <A1, A2, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): R;

  <A1, A2, A3, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => R): R;

  <A1, A2, A3, A4, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => R): R;

  <A1, A2, A3, A4, A5, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => R): R;

  <A1, A2, A3, A4, A5, A6, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => R): R;

  <A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => R): R;

  <A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => R): R;

  <A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => A9, f10: (_: A9) => R): R;
}

/**
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
  get length(): number { return this.isEmpty ? 0 : 1; }

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
   * Also known as a function application, `|>` or pipe operator.
   *
   * ```ts
   * some(1).pipe(x => x.isEmpty) // false
   * none.pipe(x => x.isEmpty) // true
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
  noneIf(predicate: (_: T) => boolean): Opt<T> {
    return this.filter(x => !predicate(x));
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
    // tslint:disable-next-line:no-console
    console.log(...[...opt(tag).map(x => [`[${x}]`]).orElse([]), 'None']);
    return this;
  }

  equals(other: Opt<T>, _comparator: EqualityFunction = refCmp): boolean {
    return other.isEmpty;
  }

  prop<K extends (T extends object ? keyof T : never)>(_key: K): Opt<WithoutOptValues<T[K]>> { return none; }
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
    return new Some(f(this._value));
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
    // tslint:disable-next-line:no-console
    console.log(...[...opt(tag).map(x => [`[${x}]`]).orElse([]), 'Some:', this._value]);
    return this;
  }

  equals(other: Opt<T>, comparator: EqualityFunction = refCmp): boolean {
    if (other.isEmpty) { return false; }
    return comparator(this._value, other.orCrash('Some expected'));
  }

  prop<K extends (T extends object ? keyof T : never)>(key: K): Opt<WithoutOptValues<T[K]>> { return opt(this._value[key]) as Opt<WithoutOptValues<T[K]>>; }
}

const someSerializedType = 'Opt/Some';
const noneSerializedType = 'Opt/None';
type OptSerialized = {
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
export const opt = <T>(x: T | undefined | null): Opt<T> => isNoneValue(x) ? none : new Some(x as T);

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
export const optFalsy = <T>(x: T | undefined | null | '' | false | 0): Opt<T> => x ? new Some(x as T) : none;

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

/** @see [[Opt.chainToOpt]] */
export const chainToOpt = <T, U>(f: (_: T) => U | undefined | null) => (x: Opt<T>): Opt<U> => x.chainToOpt(f);

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

// Don't modify manually, generated via utils/genFlowyThings
interface PipeFn {
  <I, R>(x: I, f1: (_: I) => R): R;

  <I, A1, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => R): R;

  <I, A1, A2, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): R;

  <I, A1, A2, A3, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => R): R;

  <I, A1, A2, A3, A4, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => R): R;

  <I, A1, A2, A3, A4, A5, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => R): R;

  <I, A1, A2, A3, A4, A5, A6, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => R): R;

  <I, A1, A2, A3, A4, A5, A6, A7, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => R): R;

  <I, A1, A2, A3, A4, A5, A6, A7, A8, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => R): R;

  <I, A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => A9, f10: (_: A9) => R): R;
}

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
  <U>(other: Opt<U>): <T>(x: Opt<T>) => Opt<[T, U]>;

  <U>(other: U[]): <T>(x: T[]) => [T, U][];
}

const zipArray = <T, U>(a: T[], b: U[]): [T, U][] => [...Array(Math.min(b.length, a.length))].map((_, i) => [a[i], b[i]]);

/**
 * Same as [[Opt.zip]], but also supports arrays.
 * @see [[Opt.zip]]
 */
export const zip: ZipFn = (other: any) => (x: any): any => isOpt(x) ? x.zip(other) : zipArray(x, other);

/** @see [[Opt.zip3]] */
export const zip3 = <A>(a: Opt<A>) => <B>(b: Opt<B>) => <T>(x: Opt<T>): Opt<[T, A, B]> => x.zip3(a, b);

/** @see [[Opt.zip4]] */
export const zip4 =
  <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <T>(x: Opt<T>): Opt<[T, A, B, C]> => x.zip4(a, b, c);

/** @see [[Opt.zip5]] */
export const zip5 =
  <A>(a: Opt<A>) => <B>(b: Opt<B>) => <C>(c: Opt<C>) => <D>(d: Opt<D>) => <T>(x: Opt<T>): Opt<[T, A, B, C, D]> =>
    x.zip5(a, b, c, d);

type FilterFn = <T>(p: (_: T) => boolean) => <U extends Opt<T> | T[]>(x: U) => U extends Opt<T> ? Opt<T> : T[];

/**
 * Same as [[Opt.filter]], but also supports arrays.
 * @see [[Opt.filter]]
 */
export const filter: FilterFn = (p: any) => (x: any) => x.filter(p);

/** @see [[Opt.narrow]] */
export const narrow = <U>(guard: (value: any) => value is U) => <T>(x: Opt<T>): Opt<U> => x.narrow(guard);

/** @see [[Opt.print]] */
export const print = (tag?: string) => <T>(x: Opt<T>): Opt<T> => x.print(tag);

/** @see [[Opt.equals]] */
export const equals = <T>(other: Opt<T>, comparator: EqualityFunction = refCmp) => (x: Opt<T>): boolean =>
  x.equals(other, comparator);

/** @see [[Opt.prop]] */
export const prop = <T extends object,
  K extends (T extends object ? keyof T : never) = T extends object ? keyof T : never>(key: K) => (x: Opt<T>): Opt<WithoutOptValues<T[K]>> =>
  x.prop(key);
