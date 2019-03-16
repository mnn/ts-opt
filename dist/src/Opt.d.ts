declare abstract class Opt<T> {
    abstract readonly isEmpty: boolean;
    abstract toArray(): [] | [T];
    readonly length: number;
    abstract map<U>(f: (_: T) => U): Opt<U>;
    abstract flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    chain<U>(f: (_: T) => Opt<U>): Opt<U>;
    abstract orCrash(msg: string): T;
    abstract orUndef(): T | undefined;
    abstract orNull(): T | null;
}
declare class Some<T> extends Opt<T> {
    private value;
    constructor(value: T);
    toArray(): [] | [T];
    readonly isEmpty: boolean;
    flatMap<U>(f: (_: T) => Opt<U>): Opt<U>;
    map<U>(f: (_: T) => U): Opt<U>;
    orCrash(): T;
    orNull(): T | null;
    orUndef(): T | undefined;
}
declare class None<T> extends Opt<T> {
    toArray(): [] | [T];
    readonly isEmpty: boolean;
    flatMap<U>(): Opt<U>;
    map<U>(): Opt<U>;
    orCrash(msg: string): T;
    orNull(): T | null;
    orUndef(): T | undefined;
}
export declare const none: None<any>;
export declare const some: <T>(x: T) => Some<T>;
export declare const opt: <T>(x: T | null | undefined) => Opt<T>;
export declare const fromArray: <T>(x: [] | [T]) => Opt<T>;
export {};
