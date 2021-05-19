{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module LibSpec
  ( htf_thisModulesTests,
  )
where

import Lib
import Test.Framework
import Text.InterpolatedString.Perl6 (q, qq)

test_htf :: IO ()
test_htf = assertBool True

test_renderFunction :: IO ()
test_renderFunction = do
  let f = renderFunction
  assertEqual "(): T" $ f [] [] "T"
  assertEqual "<A>(): T" $ f ["A"] [] "T"

test_genPipe :: IO ()
test_genPipe = do
  let f = genPipe
  let exp3 =
        [q|export interface PipeInClassFn<T> {
  <R>(f1: (_: Opt<T>) => R): R;
  <A1, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => R): R;
  <A1, A2, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): R;
}|]
  assertEqual exp3 $ f InterfaceIsInClass 3
  let exp3' =
        [q|export interface PipeFn {
  <I, R>(x: I, f1: (_: I) => R): R;
  <I, A1, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => R): R;
  <I, A1, A2, R>(x: I, f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): R;
}|]
  assertEqual exp3' $ f InterfaceIsNotInClass 3

test_genMapFlow :: IO ()
test_genMapFlow = do
  let f = genMapFlow
  let exp3 =
        [q|export interface MapFlowInClassFn<T> {
  <R>(f1: (_: T) => R): Opt<R>;
  <A1, R>(f1: (_: T) => A1, f2: (_: A1) => R): Opt<R>;
  <A1, A2, R>(f1: (_: T) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): Opt<R>;
}|]
  assertEqual exp3 $ f InterfaceIsInClass 3
  ---
  let exp3' =
        [q|export interface MapFlowFn {
  <I, R>(f1: (_: I) => R): (x: Opt<I>) => Opt<R>;
  <I, A1, R>(f1: (_: I) => A1, f2: (_: A1) => R): (x: Opt<I>) => Opt<R>;
  <I, A1, A2, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): (x: Opt<I>) => Opt<R>;
}|]
  let r3' = f InterfaceIsNotInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3'{nl}got:$nl$r3'|] exp3' r3'

test_genAct :: IO ()
test_genAct = do
  let f = genAct
  let exp3 =
        [q|export interface ActInClassFn<T> {
  <R>(f1: (_: T) => Opt<R>): Opt<R>;
  <A1, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<R>): Opt<R>;
  <A1, A2, R>(f1: (_: T) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<R>): Opt<R>;
}|]
  assertEqual exp3 $ f InterfaceIsInClass 3
  let exp3' =
        [q|export interface ActFn {
  <I, R>(f1: (_: I) => Opt<R>): (x: Opt<I>) => Opt<R>;
  <I, A1, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<R>): (x: Opt<I>) => Opt<R>;
  <I, A1, A2, R>(f1: (_: I) => Opt<A1>, f2: (_: A1) => Opt<A2>, f3: (_: A2) => Opt<R>): (x: Opt<I>) => Opt<R>;
}|]
  let r3' = f InterfaceIsNotInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3'{nl}got:$nl$r3'|] exp3' r3'

test_genActToOpt :: IO ()
test_genActToOpt = do
  let f = genActToOpt
  let exp3 =
        [q|export interface ActToOptInClassFn<T> {
  <R>(f1: (_: T) => R | undefined | null): Opt<R>;
  <A1, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => R | undefined | null): Opt<R>;
  <A1, A2, R>(f1: (_: T) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => R | undefined | null): Opt<R>;
}|]
  let r3 = f InterfaceIsInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3{nl}got:$nl$r3|] exp3 r3
  ---
  let exp3' =
        [q|export interface ActToOptFn {
  <I, R>(f1: (_: I) => R | undefined | null): (x: Opt<I>) => Opt<R>;
  <I, A1, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => R | undefined | null): (x: Opt<I>) => Opt<R>;
  <I, A1, A2, R>(f1: (_: I) => A1 | undefined | null, f2: (_: A1) => A2 | undefined | null, f3: (_: A2) => R | undefined | null): (x: Opt<I>) => Opt<R>;
}|]
  let r3' = f InterfaceIsNotInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3'{nl}got:$nl$r3'|] exp3' r3'

test_genFlow :: IO ()
test_genFlow = do
  let f = genFlow
  let exp3' =
        [q|export interface FlowFn {
  <I, R>(f1: (_: I) => R): (x: I) => R;
  <I, A1, R>(f1: (_: I) => A1, f2: (_: A1) => R): (x: I) => R;
  <I, A1, A2, R>(f1: (_: I) => A1, f2: (_: A1) => A2, f3: (_: A2) => R): (x: I) => R;
}|]
  let r3' = f InterfaceIsNotInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3'{nl}got:$nl$r3'|] exp3' r3'

test_genCompose :: IO ()
test_genCompose = do
  let f = genCompose
  let exp3' =
        [q|export interface ComposeFn {
  <I, R>(f1: (_: I) => R): (x: I) => R;
  <I, A1, R>(f1: (_: A1) => R, f2: (_: I) => A1): (x: I) => R;
  <I, A1, A2, R>(f1: (_: A2) => R, f2: (_: A1) => A2, f3: (_: I) => A1): (x: I) => R;
}|]
  let r3' = f InterfaceIsNotInClass 3
  assertEqualVerbose [qq|exp:$nl$exp3'{nl}got:$nl$r3'|] exp3' r3'
