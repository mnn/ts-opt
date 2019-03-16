export abstract class Opt<T> {
  static fromArray<T>(x: [] | [T]): Opt<T> { return opt(x[0]); }

  abstract get isEmpty(): boolean;

  abstract toArray(): [] | [T];

  get length(): number { return this.isEmpty ? 0 : 1; }

  abstract map<U>(f: (_: T) => U): Opt<U>;

  abstract flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;

  chain<U>(f: (_: T) => Opt<U>): Opt<U> { return this.flatMap(f); }

  abstract orCrash(msg: string): T;

  abstract orUndef(): T | undefined;

  abstract orNull(): T | null;

  abstract caseOf<R>(onSome: (x: T) => R, onNone: () => R): R;

  abstract onSome(f: (x: T) => void): void;

  abstract onNone(f: () => void): void;

  pipe<R>(f: (x: Opt<T>) => R): R { return f(this); }

  abstract contains(x: T): boolean;

  abstract exists(p: (x: T) => boolean): boolean;

  abstract forAll(p: (x: T) => boolean): boolean;

  abstract orElse(def: T): T;

  abstract orElseOpt(def: Opt<T>): Opt<T>;

  abstract bimap<U>(someF: (_: T) => U, noneF: () => U): Opt<U>;

  abstract flatBimap<U>(someF: (_: T) => Opt<U>, noneF: () => Opt<U>): Opt<U>;
}

class None<T> extends Opt<T> {
  toArray(): [] | [T] { return []; }

  get isEmpty(): boolean { return true; }

  flatMap<U>(_f: (_: T) => Opt<U>): Opt<U> { return none as unknown as Opt<U>; }

  map<U>(): Opt<U> { return none as unknown as Opt<U>; }

  orCrash(msg: string): T { throw new Error(msg); }

  orNull(): T | null { return null; }

  orUndef(): T | undefined { return undefined; }

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
}

class Some<T> extends Opt<T> {
  constructor(private value: T) { super(); }

  toArray(): [] | [T] { return [this.value]; }

  get isEmpty(): boolean { return false; }

  flatMap<U>(f: (_: T) => Opt<U>): Opt<U> {
    return f(this.value);
  }

  map<U>(f: (_: T) => U): Opt<U> {
    return new Some(f(this.value));
  }

  orCrash(_msg: string): T { return this.value; }

  orNull(): T | null { return this.value; }

  orUndef(): T | undefined { return this.value; }

  caseOf<R>(onSome: (x: T) => R, _onNone: () => R): R { return onSome(this.value); }

  contains(x: T): boolean { return this.value === x; }

  exists(p: (x: T) => boolean): boolean { return p(this.value); }

  forAll(p: (x: T) => boolean): boolean { return p(this.value); }

  onNone(_f: () => void): void { }

  onSome(f: (x: T) => void): void { f(this.value); }

  orElse(_def: T): T { return this.value; }

  orElseOpt(_def: Opt<T>): Opt<T> { return this; }

  bimap<U>(someF: (_: T) => U, _noneF: () => U): Opt<U> { return opt(someF(this.value)); }

  flatBimap<U>(someF: (_: T) => Opt<U>, _noneF: () => Opt<U>): Opt<U> { return someF(this.value); }
}

const isNoneValue = (x: any): boolean => {
  return x === undefined || x === null || Number.isNaN(x);
};

export const none: None<any> = Object.freeze(new None());

export const some = <T>(x: T) => new Some(x);

export const opt = <T>(x: T | undefined | null): Opt<T> => isNoneValue(x) ? none : new Some(x as T);

export const isOpt = (x: unknown): x is Opt<unknown> => x instanceof Opt;
