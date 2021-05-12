import { Opt } from './Opt';
export interface PipeInClassFn<T> {
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
export interface PipeFn {
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
export interface MapFlowInClassFn<T> {
    <R>(f1: (_: T) => R): Opt<R>;
    <A1, R>(f1: (_: T) => A1, f2: (_: A1) => R): Opt<R>;
    <A1, A2, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): Opt<R>;
    <A1, A2, A3, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => R): Opt<R>;
    <A1, A2, A3, A4, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => R): Opt<R>;
    <A1, A2, A3, A4, A5, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => R): Opt<R>;
    <A1, A2, A3, A4, A5, A6, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => R): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => R): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => R): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => A9, f10: (_: A9) => R): Opt<R>;
}
export interface MapFlowFn {
    <I, R>(f1: (_: I) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, R>(f1: (_: I) => A1, f2: (_: A1) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => R): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => A3, f4: (_: A3) => A4, f5: (_: A4) => A5, f6: (_: A5) => A6, f7: (_: A6) => A7, f8: (_: A7) => A8, f9: (_: A8) => A9, f10: (_: A9) => R): (x: Opt<I>) => Opt<R>;
}
export interface ActInClassFn<T> {
    <R>(f1: (_: T) => Opt<R>): Opt<R>;
    <A1, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<R>): Opt<R>;
    <A1, A2, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<R>): Opt<R>;
    <A1, A2, A3, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, A5, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, A5, A6, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<A8>, f9: (_: A8) => Opt<R>): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<A8>, f9: (_: A8) => Opt<A9>, f10: (_: A9) => Opt<R>): Opt<R>;
}
export interface ActFn {
    <I, R>(f1: (_: I) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<A8>, f9: (_: A8) => Opt<R>): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<A3>, f4: (_: A3) => Opt<A4>, f5: (_: A4) => Opt<A5>, f6: (_: A5) => Opt<A6>, f7: (_: A6) => Opt<A7>, f8: (_: A7) => Opt<A8>, f9: (_: A8) => Opt<A9>, f10: (_: A9) => Opt<R>): (x: Opt<I>) => Opt<R>;
}
export interface ActToOptInClassFn<T> {
    <R>(f1: (_: T) => R | undefined | null): Opt<R>;
    <A1, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => R | undefined | null): Opt<R>;
    <A1, A2, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => R | undefined | null): Opt<R>;
    <A1, A2, A3, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, A5, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, A5, A6, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => A8 | undefined | null, f9: (_: A8) => R | undefined | null): Opt<R>;
    <A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => A8 | undefined | null, f9: (_: A8) => A9 | undefined | null, f10: (_: A9) => R | undefined | null): Opt<R>;
}
export interface ActToOptFn {
    <I, R>(f1: (_: I) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => A8 | undefined | null, f9: (_: A8) => R | undefined | null): (x: Opt<I>) => Opt<R>;
    <I, A1, A2, A3, A4, A5, A6, A7, A8, A9, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => A3 | undefined | null, f4: (_: A3) => A4 | undefined | null, f5: (_: A4) => A5 | undefined | null, f6: (_: A5) => A6 | undefined | null, f7: (_: A6) => A7 | undefined | null, f8: (_: A7) => A8 | undefined | null, f9: (_: A8) => A9 | undefined | null, f10: (_: A9) => R | undefined | null): (x: Opt<I>) => Opt<R>;
}
