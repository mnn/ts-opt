/* tslint:disable:no-unused-expression no-console */
import * as chai from 'chai';
import * as spies from 'chai-spies';
import * as React from 'react';
import * as chaiJestSnapshot from 'chai-jest-snapshot';

import {
  act,
  actToOpt,
  alt,
  altOpt,
  and,
  ap,
  apFn,
  appendStr,
  apply,
  assertType,
  at,
  bimap,
  bool,
  caseOf,
  catOpts,
  chainFlow,
  chainIn,
  chainToOpt,
  chainToOptFlow,
  clamp,
  compose,
  contains,
  count,
  countIn,
  crash,
  curryTuple,
  curryTuple3,
  curryTuple4,
  curryTuple5,
  dec,
  DeserializationResult,
  DeserializationSuccess,
  deserialize,
  deserializeOrCrash,
  deserializeUnsafe,
  elemOf,
  elemOfStr,
  elemOfStrIn,
  eq,
  eqAny,
  equals,
  exists,
  existsIn,
  filter,
  filterIn,
  find,
  findIn,
  flatBimap,
  flatMap,
  flatMapIn,
  flow,
  fold,
  foldIn,
  forAll,
  forAllIn,
  fromArray,
  fromObject,
  genNakedPropGetters,
  genNakedPropOrCrash,
  has,
  hasIn,
  head,
  headIn,
  id,
  inc,
  isArray,
  isEmpty,
  isFull,
  isFunction,
  isNumber,
  isObject,
  isOpt,
  isOrCrash,
  isReadonlyArray,
  isString,
  joinOpt,
  last,
  lastIn,
  lengthIn,
  map,
  mapFlow,
  mapIn,
  mapOpt,
  mapStr,
  mapWithIndex,
  mapWithIndexIn,
  max,
  max2All,
  max2Any,
  max2Num,
  maxIn,
  min,
  min2All,
  min2Any,
  min2Num,
  minIn,
  narrow,
  narrowOrCrash,
  none,
  noneIf,
  noneIfEmpty,
  nonEmpty,
  noneWhen,
  noop,
  not,
  onBoth,
  onFunc,
  opt,
  Opt,
  optArrayOpt,
  optEmptyArray,
  optEmptyObject,
  optEmptyString,
  optFalsy,
  optInfinity,
  optNegative,
  optZero,
  or,
  orCrash,
  orElse,
  orElseAny,
  orElseLazy,
  orFalse,
  orNaN,
  orNull,
  orTrue,
  orUndef,
  parseFloat,
  parseInt,
  parseJson,
  pipe,
  prependStr,
  print,
  prop,
  propNaked,
  propOrCrash,
  propOrNull,
  propOrNullNaked,
  propOrUndef,
  propOrUndefNaked,
  propOrZero,
  propOrZeroNaked,
  ReduxDevtoolsCompatibilityHelper,
  serialize,
  some,
  someOrCrash,
  swap,
  testRe,
  testReOrFalse,
  toArray,
  toObject,
  toString,
  tryRun,
  uncurryTuple,
  uncurryTuple3,
  uncurryTuple4,
  uncurryTuple5,
  xor,
  zip3Opt,
  zip4Opt,
  zip5Opt,
  zipArray,
  zipIn,
  zipOpt,
  zipToOptArray,
} from '../src/Opt';
import jestSnapshotSerializer from '../src/jest-snapshot-serializer';

chai.use(spies);
const {expect} = chai;
chai.use(chaiJestSnapshot);
chaiJestSnapshot.addSerializer(jestSnapshotSerializer);
chai.should();
const sandbox = chai.spy.sandbox();

const gt0 = (x: number): boolean => x > 0;
const lt0 = (x: number): boolean => x < 0;

const randomNumOpt = (): Opt<number> => Math.random() > .5 ? none : some(Math.random());

const join = (delim: string) => (xs: string[]): string => xs.join(delim);

type EnumAB = 'a' | 'b';
type Enum12 = 1 | 2;
type EnumABC = 'a' | 'b' | 'c';
type ObjA = { a: boolean };
type ObjB = { b: number };
type ObjC = { c: string | null; cc?: string; };
type ObjAB = ObjA & ObjB;

interface WidenPBase<T> {
  labelSize?: 'small' | 'big',
  otherStuff: T
}

type WidenPProps<T> = WidenPBase<T> & { moreStuff: T };

class WidenFakeComponent<P extends WidenPProps<T>, T> {
  constructor(public props: P) { }
}

export interface FilterPart {
  values?: string[];
}

const suppressUnused = (...xs: unknown[]) => expect(xs).to.be.eq(xs);

const flow2: <A, B = unknown, C = unknown>(f: (_: A) => B, g: (_: B) => C) => ((_: A) => C) = (f, g) => x => g(f(x));

describe('opt', () => {
  beforeEach(() => {
    sandbox.on(console, ['log'], noop);
  });

  afterEach(() => {
    sandbox.restore();
  });

  it('construction', () => {
    expect(opt(undefined).isEmpty).to.be.true;
    expect(opt(NaN).isEmpty).to.be.true;
    expect(opt(null).isEmpty).to.be.true;
    expect(opt('').isEmpty).to.be.false;
    expect(opt('a').isEmpty).to.be.false;
    expect(opt(0).isEmpty).to.be.false;
    expect(opt(1).isEmpty).to.be.false;
    expect(opt({}).isEmpty).to.be.false;
    expect(opt([]).isEmpty).to.be.false;
  });

  it('isEmpty', () => {
    expect(opt(null).isEmpty).to.be.true;
    expect(opt(0).isEmpty).to.be.false;
  });

  it('nonEmpty', () => {
    expect(opt(null).nonEmpty).to.be.false;
    expect(opt(0).nonEmpty).to.be.true;
  });

  it('isFull', () => {
    expect(opt(null).isFull).to.be.false;
    expect(opt(0).isFull).to.be.true;
  });

  it('isSome', () => {
    const f = (a: Opt<number>) => {
      if (a.isSome()) {
        expect(a.value).to.eq(4);
      } else {
        // @ts-expect-error no value on none
        expect(a.value).to.be.undefined;
      }
    };
    f(none);
    f(opt(4));
  });

  it('isNone', () => {
    const f = (a: Opt<number>) => {
      if (a.isNone()) {
        // @ts-expect-error no value on none
        expect(a.value).to.be.undefined;
      } else {
        // this doesn't work - Opt is not a union (there are several usability issues preventing conversion)
        // expect(a.value).to.eq(4);
      }
    };
    f(none);
    f(opt(4));
  });

  it('length', () => {
    expect(opt(null).length).to.eq(0);
    expect(opt(0).length).to.eq(1);
  });

  describe('lengthIn', () => {
    it('returns length of string wrapped in Some', () => {
      expect(opt('hello').lengthIn().orNull()).to.equal(5);
      expect(opt('').lengthIn().orNull()).to.equal(0);
    });
  
    it('returns length of array wrapped in Some', () => {
      expect(opt([1, 2, 3]).lengthIn().orNull()).to.equal(3);
      expect(opt([]).lengthIn().orNull()).to.equal(0);
    });
  
    it('returns None when called on None', () => {
      expect(none.lengthIn().orNull()).to.be.null;
    });
  
    it('throws when called on non-string and non-array', () => {
      expect(() => opt(123 as any).lengthIn()).to.throw(Error, '`Opt#lengthIn` can only be used on strings and arrays');
    });
  
    it('should fail type checking when called with incorrect types', () => {
      expect(() => {
        // @ts-expect-error Testing for type safety: lengthIn should only work on Opt<string> or Opt<T[]>
        const result: Opt<number> = opt(123).lengthIn();
        suppressUnused(result);
      }).to.throw(Error, '`Opt#lengthIn` can only be used on strings and arrays');
    });
  });

  it('orUndef', () => {
    expect(opt(null).orUndef()).to.eq(undefined);
    expect(opt(0).orUndef()).to.eq(0);
  });

  it('orNull', () => {
    expect(opt(null).orNull()).to.eq(null);
    expect(opt(0).orNull()).to.eq(0);
  });

  it('orFalse', () => {
    expect(opt(null).orFalse()).to.be.false;
    expect(opt(0).orFalse()).to.eq(0);
  });

  it('orTrue', () => {
    expect(opt(null).orTrue()).to.be.true;
    expect(opt(0).orTrue()).to.eq(0);
  });

  it('orNaN', () => {
    expect(opt(null).orNaN()).to.be.NaN;
    expect(opt(0).orNaN()).to.eq(0);
  });

  it('orCrash', () => {
    expect(() => opt(null).orCrash('')).to.throw();
    expect(opt(0).orCrash('')).to.eq(0);
    expect(opt([]).orCrash('')).to.eql([]);

    expect(opt(0).orCrash(() => new Error(''))).to.eq(0);
    expect(opt([]).orCrash(() => new Error(''))).to.eql([]);
    expect(() => opt(null).orCrash(() => new Error('')))
      .to.throw(Error)
      .with.property('message', '');
  });

  it('someOrCrash', () => {
    expect(() => opt(null).someOrCrash('')).to.throw();
    expect(opt(0).someOrCrash('').orNull()).to.be.eq(0);
    expect(opt([]).someOrCrash('').orNull()).to.be.eql([]);
    expect(() => opt(null).someOrCrash(() => new Error(''))).to.throw(Error).with.property('message', '');
    const some0 = opt(0).someOrCrash('');
    expect(some0.value).to.be.eql(0);
  });

  it('map', () => {
    expect(opt(1).map(inc).orNull()).to.eq(2);
    expect(opt(null as unknown as number).map(inc).orUndef()).to.eq(undefined);
  });

  describe('mapIn', () => {
    it('maps a function over an array in Some', () => {
      const result: Opt<number[]> = opt([1, 2, 3]).mapIn(x => x * 2);
      expect(result.orNull()).to.eql([2, 4, 6]);
    });

    it('returns None when called on None', () => {
      const result: Opt<number[]> = opt(null).mapIn((x: number) => x * 2);
      expect(result.orNull()).to.be.null;
    });

    it('throws when called on non array', () => {
      expect(() => opt(123 as any).mapIn(x => x)).to.throw(Error, 'mapIn called on non array: 123');
    });

    it('should fail type checking when called with incorrect argument types', () => {
      // @ts-expect-error Testing for type safety: mapIn expects a function that operates on array elements.
      const result: Opt<number[]> = opt([1, 2, 3]).mapIn((x: string) => +x);
      expect(result.orNull()).to.eql([1, 2, 3]);
    });
  });

  describe('mapWithIndexIn', () => {
    it('maps over an array inside Opt with index', () => {
      expect(none.mapWithIndexIn((x: string, i: number) => x + i).orNull()).to.be.null;
      expect(opt([]).mapWithIndexIn((x: string, i: number) => x + i).orNull()).to.be.eql([]);
      expect(mapWithIndexIn((x: string, i: number) => x + i)(opt(['a', 'b'])).orNull()).to.be.eql(['a0', 'b1']);
    });

    it('should fail type checking when called with incorrect argument types', () => {
      // @ts-expect-error Testing for type safety: mapWithIndexIn expects a function that operates on array elements.
      const result: Opt<number[]> = opt([1, 2, 3]).mapWithIndexIn((x: string, i: number) => +x + i);
      suppressUnused(result);
    });

    it('should throw when called on non array', () => {
      expect(() => opt(123 as any).mapWithIndexIn((x: string, i: number) => x + i)).to.throw(Error, 'mapWithIndexIn called on non array: 123');
    });

    it('should work with readonly arrays', () => {
      const a: readonly string[] = ['a', 'b'];
      expect(mapWithIndexIn((x: string, i: number) => x + i)(opt(a)).orNull()).to.be.eql(['a0', 'b1']);
    });

    it('works with example in the issue', () => {
      interface ContentWrapper {
        content: string[];
      }

      const dataContainer: ContentWrapper = {
        content: ['cow', 'horse', 'pig']
      };

      const itemTransformer = (item: string, index: number): string => `${index}: ${item.toUpperCase()}`;
      const indexedTransformedData = opt(dataContainer)
        .prop('content')
        .mapWithIndexIn(itemTransformer)
        .orElse([]);

      expect(indexedTransformedData).to.be.eql(['0: COW', '1: HORSE', '2: PIG']);
    });
  });

  describe('mapPropNakedIn', () => {
    it('maps over a property of objects in an array inside Opt', () => {
      type Data = {data?: number};
      const input: null | (Data | null)[] = [{data: 1}, {}, {data: 2}, null];
      const data: Opt<Data[]> = optArrayOpt(input);
      const result: Opt<number[]> = data.mapPropNakedIn('data');
      expect(result.orNull()).to.eql([1, 2]);
    });

    it('example', () => {
      expect(opt([{data: 1}, {}, {data: 2}, null]).mapPropNakedIn('data').orNull()).to.eql([1, 2]);
    });
  });

  describe('mapStr', () => {
    it('should fail type checking when called with incorrect types', () => {
      expect(() => {
        // @ts-expect-error Testing for type safety: mapStr should only work on Opt<string>
        const result = opt(123).mapStr(c => c);
        suppressUnused(result);
      }).to.throw(Error, '`Opt#mapStr` can only be used on strings');
    });
  
    it('should pass type checking when called with correct types', () => {
      const result: Opt<string> = opt('hello').mapStr(c => c.toUpperCase());
      expect(result.orNull()).to.equal('HELLO');
    });
  
    it('should work with none', () => {
      expect(none.mapStr(c => c.toUpperCase()).orNull()).to.be.null;
    });
  });

  it('mapFlow', () => {
    expect(opt(1).mapFlow(id).orNull()).to.be.eq(1);
    expect(opt(null).mapFlow(id).orNull()).to.be.null;
    expect(opt(1).mapFlow(id, id, id, inc).orNull()).to.be.eq(2);
    expect(opt<number>(null).mapFlow(id, id, id, inc).orNull()).to.be.null;
    const sq = (x: number) => x * x;
    const dec = (x: number) => x - 1;
    expect(opt(4).mapFlow(sq, dec).orNull()).to.be.eq(15);
  });

  it('flatMap', () => {
    expect(opt(1).flatMap(() => some(2)).orNull()).to.eq(2);
    expect(opt(1).flatMap(() => none).orNull()).to.eq(null);
    expect(opt(null).flatMap(() => none).orUndef()).to.eq(undefined);
    expect(opt(null).chain(() => none).orUndef()).to.eq(undefined);
  });

  describe('flatMapIn', () => {
    it('flatMaps a function over an array in Some', () => {
      const result: Opt<number[]> = opt([1, 2]).flatMapIn(x => [x, x * 2]);
      expect(result.orNull()).to.eql([1, 2, 2, 4]);
    });
  
    it('returns None when called on None', () => {
      const result: Opt<number[]> = opt(null).flatMapIn((x: number) => [x, x * 2]);
      expect(result.orNull()).to.be.null;  
    });
  
    it('throws when called on non array', () => {
      expect(() => opt(123 as any).flatMapIn(x => [x])).to.throw(Error, 'flatMapIn called on non array: 123');
    });
  
    it('should fail type checking when called with incorrect argument types', () => {
      // @ts-expect-error Testing for type safety: flatMapIn expects a function that returns an array  
      const result: Opt<number[]> = opt([1, 2]).flatMapIn((x: number) => x + 1);
      expect(result.orNull()).to.eql([2, 3]);
    });

    it('chainIn', () => {
      expect(opt([1, 2]).chainIn(x => [x, x * 2]).orNull()).to.eql([1, 2, 2, 4]);
    });
  });

  it('act', () => {
    const f1 = (x: string | number) => (y: number) => opt(x).narrow(isNumber).map(z => z + y);
    const f2 = (x: number): Opt<number> => x % 2 === 0 ? opt(x) : none;
    const f = (x: number, y: number | string) => opt(x).act(
      f1(y),
      f2,
      optNegative,
    ).orNull();
    expect(f(0, 2)).to.be.eq(2);
    expect(f(0, -2)).to.be.eq(null);
    expect(f(1, 2)).to.be.eq(null);
    expect(f(0, '2')).to.be.eq(null);
    expect(opt(1).chainFlow(opt).orNull()).to.be.eq(1);
    // examples
    const r = opt(0).act( // Some(0)
      f1(-2), // Some(-2)
      f2, // Some(-2)
      optNegative, // None
    ); // None
    expect(r.orNull()).to.be.null;
    const r2 = opt(0).act( // Some(0)
      f1(1), // Some(1)
      f2, // None
      optNegative, // won't get called, still None
    ); // None
    expect(r2.orNull()).to.be.null;
    const r3 = opt(3).act( // Some(3)
      f1(1), // Some(4)
      f2, // Some(4)
      optNegative, // Some(4)
    ); // Some(4)
    expect(r3.orNull()).to.be.eq(4);
  });

  it('toArray', () => {
    expect(opt(1).toArray()).to.eql([1]);
    expect(opt(null).toArray()).to.eql([]);
  });

  it('toObject', () => {
    expect(
      opt(1).toObject(), // {value: 1}
    ).to.be.eql({value: 1});
    expect(
      opt(undefined).toObject(), // {value: null}
    ).to.be.eql({value: null});
    expect(
      opt(undefined).toObject('id'), // {id: null}
    ).to.be.eql({id: null});

    const a: { value: string | null } = opt('Madara').toObject();
    // @ts-expect-error wrong type of value field
    const b: { value: string | null } = opt(57).toObject();
    const c: { c: number | null } = opt(57).toObject('c');
    expect(c).to.be.eql({c: 57});
    suppressUnused(a, b);
  });

  it('serialize', () => {
    expect(opt(1).serialize()).to.be.eql({type:'Opt/Some', value: 1});
    expect(opt(null).serialize()).to.be.eql({type:'Opt/None'});
  });

  it('deserialize', () => {
    const res = Opt.deserialize({type: 'Opt/Some', value: 1}, isNumber);
    expectSuccessDeserializationResult(res);
    expect(res.value.orNull()).to.be.eql(1);
  });

  it('caseOf', () => {
    expect(some(1).caseOf(x => 'x' + x, () => 'y')).to.eql('x1');
    expect(none.caseOf(x => 'x' + x, () => 'y')).to.eql('y');
    expect(randomNumOpt().caseOf(() => 'x', () => 'y')).to.be.oneOf(['x', 'y']);
  });

  it('fold', () => {
    expect(some(1).fold(x => x + 1, 0)).to.be.eq(2);
    expect(none.fold(x => x + 1, 0)).to.be.eq(0);
  });

  describe('onBoth', () => {
    it('calls correct callback', () => {
      const noneCb = chai.spy();
      const someCb = chai.spy();
      noneCb.should.have.not.been.called();
      someCb.should.have.not.been.called();
      expect(none.onBoth(someCb, noneCb).orNull()).to.be.null;
      someCb.should.have.not.been.called();
      noneCb.should.have.been.called.once;
      expect(some(1).onBoth(someCb, noneCb).orNull()).to.eql(1);
      someCb.should.have.been.called.once;
      someCb.should.have.been.called.always.with(1);
      noneCb.should.have.been.called.once;
    });
    it('example', () => {
      expect(
        // prints 1, returns some(1)
        some(1).onBoth(x => console.log(x), () => console.log('none'))
               .orNull(),
      ).to.eql(1);
      expect(
        // prints "none", returns none
        none.onBoth(x => console.log(x), () => console.log('none'))
            .orNull(),
      ).to.be.null;
    });
  });

  it('pipe', () => {
    expect(some(1).pipe(isOpt)).to.be.true;
    expect(none.pipe(isOpt)).to.be.true;
    expect(some(1).pipe(x => x.map(inc), x => x.orNull())).to.be.eq(2);
    expect(some(1).pipe(orElse(7), inc, inc)).to.be.eq(3);
    expect(some(1).pipe(orElse(7), inc, inc, inc)).to.be.eq(4);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc)).to.be.eq(5);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc, inc)).to.be.eq(6);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc, inc, inc)).to.be.eq(7);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc, inc, inc, inc)).to.be.eq(8);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc, inc, inc, inc, inc)).to.be.eq(9);
    expect(some(1).pipe(orElse(7), inc, inc, inc, inc, inc, inc, inc, inc, inc)).to.be.eq(10);
    // example
    expect(opt(1).pipe(
        x => x.isEmpty,
        x => !x,
      ),
    ).to.be.true;
  });

  it('onNone', () => {
    const cb = chai.spy();
    cb.should.have.not.been.called();
    const resSome = some(1).onNone(cb);
    expect(resSome.orNull()).to.be.eq(1);
    cb.should.have.not.been.called();
    const resNone = none.onNone(cb);
    expect(resNone.orNull()).to.be.null;
    cb.should.have.been.called.once;
  });

  it('onSome', () => {
    const cb = chai.spy();
    cb.should.have.not.been.called();
    const resNone = none.onSome(cb);
    expect(resNone.orNull()).to.be.null;
    cb.should.have.not.been.called();
    const resSome = some(1).onSome(cb);
    expect(resSome.orNull()).to.eq(1);
    cb.should.have.been.called.once;
    cb.should.have.been.called.with.exactly(1);
  });

  it('contains', () => {
    expect(none.contains(0)).to.be.false;
    expect(some(1).contains(0)).to.be.false;
    expect(some(0).contains(0)).to.be.true;
  });

  it('containsIn', () => {
    expect(none.containsIn(0)).to.be.false;
    expect(opt([1]).containsIn(0)).to.be.false;
    expect(opt([0]).containsIn(0)).to.be.true;
  });

  describe('elemOfIn', () => {
    it('returns true when value is in array', () => {
      expect(opt(2).elemOfIn([1, 2, 3])).to.be.true;
      expect(opt('b').elemOfIn(['a', 'b', 'c'])).to.be.true;
    });

    it('returns false when value is not in array', () => {
      expect(opt(4).elemOfIn([1, 2, 3])).to.be.false;
      expect(opt('d').elemOfIn(['a', 'b', 'c'])).to.be.false;
    });

    it('returns false for none', () => {
      expect(none.elemOfIn([1, 2, 3])).to.be.false;
    });
  });

  describe('elemOfStrIn', () => {
    it('returns true when substring in Opt is in string', () => {
      expect(opt('a').elemOfStrIn('abc')).to.be.true;
      expect(opt('ab').elemOfStrIn('abc')).to.be.true;
      expect(opt('abc').elemOfStrIn('abc')).to.be.true;
    });
  
    it('returns false when substring in Opt is not in string', () => {
      expect(opt('d').elemOfStrIn('abc')).to.be.false;
      expect(opt('ac').elemOfStrIn('abc')).to.be.false;
      expect(opt('abcd').elemOfStrIn('abc')).to.be.false;
    });
  
    it('returns false for none', () => {
      expect(none.elemOfStrIn('abc')).to.be.false;
    });
  });

  describe('isEmptyIn', () => {
    it('returns true for empty array in Some', () => {
      expect(opt([]).isEmptyIn()).to.be.true;
    });

    it('returns false for non-empty array in Some', () => {
      expect(opt([1, 2, 3]).isEmptyIn()).to.be.false;
    });

    it('returns true for None', () => {
      expect(none.isEmptyIn()).to.be.true;
    });

    it('throws when called on non-array value in Some', () => {
      const numOpt = opt(123);
      // @ts-expect-error wrong type
      expect(() => numOpt.isEmptyIn()).to.throw(Error, 'isEmptyIn called on non array: 123');
    });
  });

  it('has', () => {
    expect(none.has(0)).to.be.false;
    expect(some(1).has(0)).to.be.false;
    expect(some(0).has(0)).to.be.true;
  });

  it('hasIn', () => {
    expect(none.hasIn(0)).to.be.false;
    expect(opt([1]).hasIn(0)).to.be.false;
    expect(opt([0]).hasIn(0)).to.be.true;
  });

  it('exists', () => {
    expect(none.exists(() => true)).to.be.false;
    expect(none.exists(() => false)).to.be.false;
    expect(some(0).exists(() => false)).to.be.false;
    expect(some(0).exists(() => true)).to.be.true;
    const cb = chai.spy(() => true);
    none.exists(cb);
    cb.should.have.not.been.called();
    some(0).exists(cb);
    cb.should.have.been.called.with.exactly(0);
  });

  describe('existsIn', () => {
    it('checks existence of element satisfying predicate in Some', () => {
      expect(opt([1, 2, 3, 4]).existsIn(x => x % 2 === 0).orNull()).to.be.true;
      expect(opt([1, 3, 5]).existsIn(x => x %2 === 0).orNull()).to.be.false;
    });
  
    it('returns false when called on None', () => {
      expect(opt(null).existsIn((x: number) => x > 0).orNull()).to.be.null;
    });
  
    it('throws when called on non array', () => {
      expect(() => opt(123 as any).existsIn((x: any) => x > 0)).to.throw(Error, 'existsIn called on non array: 123');
    });
  });

  it('forAll', () => {
    expect(none.forAll(() => true)).to.be.true;
    expect(none.forAll(() => false)).to.be.true;
    expect(some(1).forAll(() => true)).to.be.true;
    expect(some(1).forAll(() => false)).to.be.false;
    const cb = chai.spy(() => true);
    none.forAll(cb);
    cb.should.have.not.been.called();
    some(0).forAll(cb);
    cb.should.have.been.called.with.exactly(0);
  });

  describe('forAllIn', () => {
    it('checks if all elements satisfy predicate in Some', () => {
      expect(opt([2, 4, 6, 8]).forAllIn(x => x % 2 === 0).orNull()).to.be.true;
      expect(opt([2, 3, 4, 6]).forAllIn(x => x % 2 === 0).orNull()).to.be.false;
    });
  
    it('returns true when called on None', () => {
      expect(opt(null).forAllIn((x: number) => x > 0).orNull()).to.be.null;
    });
  
    it('throws when called on non array', () => {
      expect(() => opt(123 as any).forAllIn((x: any) => x > 0)).to.throw(Error, 'forAllIn called on non array: 123');
    });
  });

  it('orElse', () => {
    expect(none.orElse(0)).to.be.eq(0);
    expect(some(1).orElse(0)).to.be.eq(1);
  });

  it('orElseLazy', () => {
    const noneF = chai.spy(() => 0);
    const someF = chai.spy(() => 0);
    expect(none.orElseLazy(noneF)).to.be.eq(0);
    noneF.should.have.been.called();
    expect(some(1).orElseLazy(someF)).to.be.eq(1);
    someF.should.have.not.been.called();

    expect(
      some(1).orElseLazy(() => 2), // 1
    ).to.be.eq(1);
    expect(
      none.orElseLazy(() => 2), // 2
    ).to.be.eq(2);
  });

  it('orElseAny', () => {
    // @ts-expect-error TS2345: Argument of type 'boolean' is not assignable to parameter of type 'number'.
    opt(1).orElse(true);
    const r1: number | boolean = opt(1).orElseAny(true);
    expect(r1).to.be.eq(1);
    const r2: number | boolean = opt<number>(null).orElseAny(true);
    expect(r2).to.be.true;
  });

  it('alt', () => {
    expect(
      none.alt(some(0)) // Some(0)
          .orNull(),
    ).to.be.eq(0);
    expect(
      opt(1).alt(some(0)) // Some(1)
            .orNull(),
    ).to.be.eq(1);
    expect(
      none.alt(none) // None
          .orNull(),
    ).to.be.eq(null);
    expect(
      some(1).alt(none) // Some(1)
             .orNull(),
    ).to.be.eq(1);
    type Handler = (_: number) => void;
    const userHandler: Opt<Handler> = opt(a => console.log('user handling', a));
    const systemHandler: Opt<Handler> = opt(a => console.log('system handling', a));
    const backupHandler: Opt<Handler> = opt(a => console.log('backup handling', a));
    const panicHandler: Handler = a => console.log('PANIC handling', a);
    const handler =
      userHandler
        .alt(systemHandler)
        .alt(backupHandler)
        .orElse(panicHandler);
    handler(250 + 64); // prints "user handling 314"
    expect(console.log).to.have.been.called.exactly(1);
    expect(console.log).to.have.been.called.with('user handling', 314);
  });

  it('altOpt', () => {
    const inputNull: number | null = null as number | null;
    expect(
      opt(inputNull).altOpt(null) // None
                    .orFalse(),
    ).to.be.false;
    expect(
      opt(inputNull).altOpt(1) // Some(1)
                    .orFalse(),
    ).to.be.eq(1);
    expect(
      opt(2).altOpt(1) // Some(2)
            .orFalse(),
    ).to.be.eq(2);
  });

  it('bimap', () => {
    expect(none.bimap(x => x, () => 'none').orNull()).to.be.eq('none');
    expect(some('some').bimap(x => x, () => 'none').orNull()).to.be.eq('some');
  });

  it('flatBimap', () => {
    expect(none.flatBimap<string>(x => some(x), () => some('none')).orNull()).to.be.eq('none');
    expect((none as Opt<string>).flatBimap(x => some(x), () => some('none')).orNull()).to.be.eq('none');
    expect(some('some').flatBimap(x => some(x), () => some('none')).orNull()).to.be.eq('some');
  });

  it('toString', () => {
    expect(some(1).toString()).to.eq('Some(1)');
    expect(some('a').toString()).to.eq('Some("a")');
    expect(some(null).toString()).to.eq('Some(null)');
    expect(none.toString()).to.eq('None');
  });

  it('chainToOpt', () => {
    expect(some(1).chainToOpt(x => x === 1 ? null : x + 1).orUndef()).to.be.eq(undefined);
    expect(some(2).chainToOpt(x => x === 1 ? null : x + 1).orUndef()).to.be.eq(3);
  });

  it('foldIn', () => {
    expect(opt([1, 2, 3]).foldIn((acc, x) => acc + x, 0).orNull()).to.be.eq(6);
    expect(none.foldIn((acc, x) => acc + x, 0).orNull()).to.be.null;
  });

  it('actToOpt', () => {
    expect(some(1).actToOpt(id).orNull()).to.be.eq(1);
    expect(none.chainToOptFlow(id, id).orNull()).to.be.null;
    expect(opt(1).actToOpt(id, inc, inc).orNull()).to.be.eq(3);
  });

  it('join', () => {
    expect(opt(opt(1)).join().orNull()).to.be.eq(1);
    expect(opt(none).join().orNull()).to.be.null;
    expect(none.join().orNull()).to.be.null;
    const a: Opt<number> = opt(2);
    (() => {
      // @ts-expect-error shouldn't work on non-nested opt
      a.join();
    });
  });

  it('zip', () => {
    expect(some(1).zip(some(true)).orNull()).to.be.eql([1, true]);
    expect(some(1).zip(none).orNull()).to.be.null;
    expect(none.zip(some(true)).orNull()).to.be.null;
    expect(none.zip(some(true)).orNull()).to.be.null;
      const formatAddress =
        (streetName?: string, streetNumber?: string): string =>
        opt(streetName).zip(opt(streetNumber)).map(join(' ')).orElse('');
      expect(formatAddress('Strawberry', '12')).to.be.eq('Strawberry 12');
      expect(formatAddress('Strawberry', undefined)).to.be.eq('');
      expect(formatAddress(undefined, '12')).to.be.eq('');
      expect(formatAddress(undefined, undefined)).to.be.eq('');
    });

  it('zip3', () => {
    expect(some(1).zip3(some(true), some('a')).orNull()).to.be.eql([1, true, 'a']);
    expect(some(1).zip3(some(true), none).orNull()).to.be.null;

    expect(some(1).zip3(none, some('a')).orNull()).to.be.null;
    expect(some(1).zip3(none, none).orNull()).to.be.null;

    expect(none.zip3(some(true), some('a')).orNull()).to.be.null;
    expect(none.zip3(some(true), none).orNull()).to.be.null;

    expect(none.zip3(some(true), some('a')).orNull()).to.be.null;
    expect(none.zip3(some(true), none).orNull()).to.be.null;
  });

  it('zip4', () => {
    expect(some(1).zip4(some(true), some('a'), some('b' as EnumAB)).orNull()).to.be.eql([1, true, 'a', 'b']);
    expect(some(1).zip4(some(true), none, some('b' as EnumAB)).orNull()).to.be.null;
    expect(some(1).zip4(none, some('a'), some('b' as EnumAB)).orNull()).to.be.null;
    expect(none.zip4(some(true), some('a'), some('b' as EnumAB)).orNull()).to.be.null;
  });

  it('zip5', () => {
    expect(some(1).zip5(some(true), some('a'), some('b' as EnumAB), some(2 as Enum12)).orNull())
    .to.be.eql([1, true, 'a', 'b', 2]);
    expect(some(1).zip5(some(true), some('a'), some('b' as EnumAB), none).orNull()).to.be.null;
    expect(some(1).zip5(some(true), some('a'), none, some(2 as Enum12)).orNull()).to.be.null;
    expect(some(1).zip5(some(true), none, some('b' as EnumAB), some(2 as Enum12)).orNull()).to.be.null;
    expect(some(1).zip5(none, some('a'), some('b' as EnumAB), some(2 as Enum12)).orNull()).to.be.null;
    expect(none.zip5(some(true), some('a'), some('b' as EnumAB), some(2 as Enum12)).orNull()).to.be.null;
  });

  it('filter', () => {
    expect(some(1).filter(gt0).orNull()).to.be.eq(1);
    expect(some(1).filter(lt0).orNull()).to.be.null;
    expect(none.filter(lt0).orNull()).to.be.null;
  });

  it('filterIn', () => {
    expect(opt([-1, 0, 1]).filterIn(gt0).orNull()).to.be.eql([1]);
    expect(none.filterIn(gt0).orNull()).to.be.null;
    expect(() => opt(0 as any).filterIn(gt0)).to.throw();
  });

  it('filterByRe', () => {
    const re1 = /b/;
    expect(opt('').filterByRe(re1).orNull()).to.be.null;
    expect(opt('abc').filterByRe(re1).orNull()).to.be.eq('abc');
    expect(some('ac').filterByRe(re1).orNull()).to.be.null;
    expect(none.filterByRe(re1).orNull()).to.be.null;
    const x: Opt<string> = opt('b').filterByRe(re1);
    const y: Opt<string> = opt('').filterByRe(re1);
    expect(() => {
      const z: never = opt(4).filterByRe(re1);
      suppressUnused(z);
    }).to.throw();
    suppressUnused(x, y);
    expect(
      opt('Luffy').filterByRe(/f+/)
                  .orNull(),
    ).to.be.eq('Luffy');
    expect(
      opt('Robin').filterByRe(/f+/)
                  .orNull(),
    ).to.be.null;
  });

  it('findIn', () => {
    expect(opt([1, 2, 3, 4]).findIn(x => x > 2).orNull()).to.be.eq(3);
    expect(opt([1, 2, 3, 4]).findIn(x => x > 5).orNull()).to.be.null;
    expect(none.findIn(x => x > 2).orNull()).to.be.null;
    expect(() => opt('' as any as number[]).findIn(x => x > 2)).to.throw();
  });

  it('zipIn', () => {
    expect(opt([1, 2]).zipIn([3, 4]).orNull()).to.be.eql([[1, 3], [2, 4]]);
    expect(opt([1]).zipIn([3, 4]).orNull()).to.be.eql([[1, 3]]);
    expect(opt([1, 2]).zipIn(null).orNull()).to.be.null;
    expect((none as Opt<number[]>).zipIn([1, 2]).orNull()).to.be.null;
  })

  it('noneIf', () => {
    expect(some(1).noneIf(lt0).orNull()).to.be.eq(1);
    expect(some(1).noneIf(gt0).orNull()).to.be.null;
    expect(none.noneIf(lt0).orNull()).to.be.null;
  });

  it('noneIfEmpty', () => {
      expect(opt(1).noneIfEmpty().orNull()).to.be.eq(1);
      expect(opt([1]).noneIfEmpty().orNull()).to.be.eql([1]);
      expect(opt({a:1}).noneIfEmpty().orNull()).to.be.eql({a:1});
      expect(opt(NaN).noneIfEmpty().orNull()).to.be.null;
      expect(opt([]).noneIfEmpty().orNull()).to.be.null;
      expect(opt({}).noneIfEmpty().orNull()).to.be.null;
      const arr: number[] = [];
      const arrRes: Opt<number[]> = opt(arr).noneIfEmpty();
      // eslint-disable-next-line @typescript-eslint/no-inferrable-types
      const str: string = 'x';
      const strRes: Opt<string> = opt(str).noneIfEmpty();
      const maybeEmptyStr: 'a' | '' = '' as 'a' | '';
      const maybeEmptyStrRes: Opt<'a'> = opt(maybeEmptyStr).noneIfEmpty();
      const maybeEmptyArr: [] | [number] = [] as [] | [number];
      const maybeEmptyArrRes: Opt<[number]> = opt(maybeEmptyArr).noneIfEmpty();
      // @ts-expect-error [] is not assignable to [number]
      const maybeEmptyArrResFail: Opt<[]> = opt(maybeEmptyArr).noneIfEmpty();
      suppressUnused(arrRes, strRes, maybeEmptyStrRes,maybeEmptyArrRes, maybeEmptyArrResFail);
  })

  it('noneWhen', () => {
    expect(
      opt(1).noneWhen(false) // Some(1)
            .orNull(),
    ).to.be.eq(1);
    expect(
      opt(1).noneWhen(true) // None
            .orNull(),
    ).to.be.null;
    expect(none.noneWhen(true).orNull()).to.be.null;
  });

  it('count', () => {
    expect(opt(1).count(gt0)).to.be.eq(1);
    expect(opt(-1).count(gt0)).to.be.eq(0);
    expect(opt(NaN).count(gt0)).to.be.eq(0);
  });

  describe('countIn', () => {
    it('counts the number of elements in an array wrapped in Opt that satisfy the given predicate', () => {
      const result: Opt<number> = opt([1, 2, 3, 4]).countIn(x => x % 2 === 0);
      expect(result.orNull()).to.equal(2);
    });
  
    it('returns None when called on None', () => {
      const result: Opt<number> = opt(null).countIn((x: number) => x > 0);
      expect(result.orNull()).to.be.null;
    });
  
    it('throws when called on non array opt', () => {
      expect(() => opt(123 as any).countIn((x: any) => x > 0)).to.throw(Error, 'countIn called on non array: 123');
    });
  });

  it('narrow', () => {
    type SN = string | number;
    const sn1: SN = 1 as SN;
    const a: Opt<string> = some(sn1).narrow(isString);
    expect(a.orNull()).to.eq(null);
    const b: Opt<number> = some(sn1).narrow(isNumber);
    expect(b.orNull()).to.eq(1);
    const noneSN: Opt<SN> = none;
    const n: Opt<number> = noneSN.narrow(isNumber);
    expect(n.orNull()).to.be.null;

    type SNAO = string | number | unknown[] | object;
    const snaoA = [] as SNAO;
    const fO = (x: object): object => x;
    const fA = (x: unknown[]): unknown[] => x;
    expect(opt(snaoA).narrow(isObject).map(fO).narrow(isArray).map(fA).orNull()).to.eql([]);

    // examples from docs
    const an1: Opt<string> = some('1' as string | number).narrow(isString); // Some('1'): Opt<string>
    expect(an1.orNull()).to.be.eq('1');
    const an2: Opt<string> = some(1 as string | number).narrow(isString); // None: Opt<string>
    expect(an2.orNull()).to.be.null;
  });

  describe('narrowOrCrash', () => {
    it('crashes on narrowing failure', () => {
      expect(() => opt(1).narrowOrCrash(isString)).to.throw();
    });
    it('doesn\'t crash on none', () => {
      expect(none.narrowOrCrash(isString).orNull()).to.be.null;
    });
    it('passes custom crash message', () => {
      const crashMessage = 'not a string';
      expect(() => opt(1).narrowOrCrash(isString, crashMessage)).to.throw(crashMessage);
    });
    it('throws custom error from factory', () => {
      expect(() => opt(1).narrowOrCrash(isString, () => new Error('not a string error'))).to.throw(Error, 'not a string error');
    });
    it('narrows type', () => {
      const a: Opt<string> = opt('y').narrowOrCrash(isString);
      suppressUnused(a);
    });
  });

  it('print', () => {
    opt(1).print();
    expect(console.log).to.have.been.called.exactly(1);
    expect(console.log).to.have.been.called.with('Some:', 1);

    opt(null).print();
    expect(console.log).to.have.been.called.exactly(2);
    expect(console.log).to.have.been.called.with('None');

    opt(2).print('>>');
    expect(console.log).to.have.been.called.exactly(3);
    expect(console.log).to.have.been.called.with('[>>]', 'Some:', 2);

    opt(null).print('<<');
    expect(console.log).to.have.been.called.exactly(4);
    expect(console.log).to.have.been.called.with('[<<]', 'None');
  });

  it('equals', () => {
    expect(none.equals(none)).to.be.true;
    expect(opt(1).equals(opt(1))).to.be.true;
    expect(opt(1).equals(none)).to.be.false;
    expect(none.equals(opt(1))).to.be.false;
    expect(opt(1).equals(opt(2), () => true)).to.be.true;
    expect(opt({a: 1}).equals(opt({a: 1}))).to.be.false;
    const jsonCmp = <T>(a: T, b: T): boolean => JSON.stringify(a) === JSON.stringify(b);
    expect(opt({a: 1}).equals(opt({a: 1}), jsonCmp)).to.be.true;
    expect(none.equals(none, jsonCmp)).to.be.true;
  });

  it('widen', () => {
    const ab = 'a' as EnumAB;
    const abc = 'c' as EnumABC;
    const correctWiden: Opt<EnumABC> = opt(ab).widen<EnumABC>(); // AB -> ABC: Ok
    const wrongWiden: Opt<never> = opt(abc).widen<EnumAB>(); // ABC -> AB: Not Ok, C is not in AB
    // @ts-expect-error incorrect use
    const wrongWiden2 = opt(abc).widen<EnumAB, EnumAB>();
    // @ts-expect-error incorrect use
    opt(abc).widen<EnumAB, EnumABC>();
    // disallowed use with objects (unsafe)
    const oa: ObjA = {a: true};
    const oab: ObjAB = {a: false, b: -1};
    const wrongWiden3: Opt<never> = opt(oa).widen<ObjAB>();
    const wrongWiden4: Opt<never> = opt(oab).widen<ObjA>();
    suppressUnused(correctWiden, wrongWiden, wrongWiden2, wrongWiden3, wrongWiden4);
    // real-world-ish (most likely used to be an issue with type-inference of TypeScript, currently seems to be fixed)
    const comp = new WidenFakeComponent<WidenPProps<ObjA>, ObjA>({otherStuff: {a: true}, moreStuff: {a: false}, labelSize: 'small'});
    const {labelSize} = comp.props;
    const capitalize = (x: string): string => x.replace(/^\w/, c => c.toUpperCase());
    const styles: Record<string, string | undefined> = {labelSizeSmall: 'S', labelSizeBig: 'B'};
    const className = opt(labelSize).widen<string>().chainToOpt(x => styles[`labelSize${capitalize(x)}`]).orUndef();
    expect(className).to.be.eq('S');
  });

  it('prop', () => {
    const oab: ObjAB = {a: false, b: -1};
    expect(opt(oab).prop('a').orNull()).to.be.false;
    expect((none as Opt<ObjAB>).prop('a').orNull()).to.be.null;
    // @ts-expect-error invalid prop
    opt(oab).prop('X');
    const a = {x: 1};
    const xValue = opt(a).prop('x').orCrash('missing prop x'); // 1
    expect(xValue).to.be.eq(1);
    // removing of "opty" values from type parameter of result
    const getValue2 = (filter?: FilterPart): boolean | null => opt(filter).prop('values').map(xs => xs[0] === 'true').orNull();
    const getValue3 = (filter?: FilterPart): boolean | null => opt(filter).prop('values').map(head).map(eqAny('true')).orNull();
    suppressUnused(getValue2, getValue3);
  });

  describe('propOrCrash', () => {
    it('crashes on missing prop', () => {
      interface A { x?: number; }

      const aFull: A = { x: 4 };
      const aEmpty: A = {};
      const xFromA: number = opt(aFull).propOrCrash('x');
      expect(xFromA).to.be.eq(4);
      expect(() => opt(aEmpty).propOrCrash('x')).to.throw('missing x');
      expect(() => {
        // @ts-expect-error invalid field
        const x = opt(aFull).propOrCrash('foo');
        suppressUnused(x);
      }).to.throw();
    });

    describe('with custom error', () => {
      interface Data { fieldA?: string; }
      const o: Data = {};
    
      it('supports string message', () => {
        expect(() => opt(o).propOrCrash('fieldA', 'Custom error: fieldA is missing'))
          .to.throw('Custom error: fieldA is missing');
      });

      it('supports string error message factory', () => {
        expect(() => opt(o).propOrCrash('fieldA', key => `Custom error: ${key} is missing`))
          .to.throw('Custom error: fieldA is missing');
      });
    
      it('supports Error object factory', () => {
        expect(() => opt(o).propOrCrash('fieldA', key => new Error(`Custom error: ${key} is missing`)))
          .to.throw('Custom error: fieldA is missing');
      });
    
      it('works with function references', () => {
        const makeErrorMsg = (key: string) => `Custom error: ${key} is missing`;
        const makeErrorObj = (key: string) => new Error(`Custom error: ${key} is missing`);
    
        expect(() => opt(o).propOrCrash('fieldA', makeErrorMsg))
          .to.throw('Custom error: fieldA is missing');
        expect(() => opt(o).propOrCrash('fieldA', makeErrorObj))
          .to.throw('Custom error: fieldA is missing');
      });
    });
  });

  describe('propOrNull', () => {
    interface Obj {x?: number;}

    it('returns value when present', () => {
      const obj: Obj = {x: 5};
      expect(opt(obj).propOrNull('x')).to.equal(5);
    });

    it('returns null when property is missing', () => {
      const obj: Obj = {};
      expect(opt(obj).propOrNull('x')).to.be.null;
    });

    it('returns null for none', () => {
      expect(none.propOrNull('x')).to.be.null;
    });
  });

  describe('propOrUndef', () => {
    interface Obj {x?: number;}

    it('returns value when present', () => {
      const obj: Obj = {x: 5};
      expect(opt(obj).propOrUndef('x')).to.equal(5);
    });

    it('returns undefined when property is missing', () => {
      const obj: Obj = {};
      expect(opt(obj).propOrUndef('x')).to.be.undefined;
    });

    it('returns undefined for none', () => {
      expect(none.propOrUndef('x')).to.be.undefined;
    });
  });

  describe('propOrZero', () => {
    interface Obj {x?: number;}

    it('returns value when present', () => {
      const obj: Obj = {x: 5};
      expect(opt(obj).propOrZero('x')).to.equal(5);
    });

    it('returns 0 when property is missing', () => {
      const obj: Obj = {};
      expect(opt(obj).propOrZero('x')).to.equal(0);
    });

    it('returns 0 for none', () => {
      expect(none.propOrZero('x')).to.equal(0);
    });
  });

  it('genPropGetters', () => {
    interface Obj { x: number; y: string; z?: number; }
    const obj = opt<Obj>({ x: 1, y: 'hello' });
    const getters = obj.genPropGetters();
    expect(getters.orCrash('x')).to.be.eq(1);
    expect(getters.orNull('y')).to.be.eq('hello');
    expect(getters.orUndef('z')).to.be.undefined;
    expect(() => getters.orCrash('z')).to.throw();
    expect(getters.orZero('x')).to.be.eq(1);
    expect(getters.prop('x').orCrash('missing x')).to.be.eq(1);
    // @ts-expect-error invalid prop
    expect(() => getters.orCrash('xxx')).to.throw();
    const yOrNull: string | null = getters.orNull('y');
    const zOrZero: number | 0 = getters.orZero('z');
    // @ts-expect-error y is string not a number
    const uOrUndef: number | undefined = getters.orUndef('y');
    suppressUnused(yOrNull, zOrZero, uOrUndef);

    const gettersWithCustomError = obj.genPropGetters(key => `Custom error: ${key} is missing`);
    expect(() => gettersWithCustomError.orCrash('z')).to.throw('Custom error: z is missing');
    expect(() => gettersWithCustomError.orCrash('z', 'nope, no z')).to.throw('nope, no z');
    expect(() => gettersWithCustomError.orCrash('z', key => `nope, no ${key}`)).to.throw('nope, no z');
    expect(() => gettersWithCustomError.orCrash('z', key => new Error(`nope, no ${key}`))).to.throw('nope, no z');
  });

  it('const', () => {
    expect(
      opt(1).const()(), // 1
    ).to.be.eq(1);
    const f: () => string | null = opt<string>(null).const();
    expect(f()).to.be.null;
    expect(
      opt(undefined).const()(), // null
    ).to.be.null;
    expect(
      opt(NaN).const(undefined)(), // undefined
    ).to.be.undefined;
    expect(opt(2).const(undefined)()).to.be.eq(2);
    const g: () => number | undefined = opt<number>(null).const(undefined);
    suppressUnused(g);
  });

  it('swap', () => {
    expect(opt(1).swap(true).orNull()).to.be.eq(true);
    expect(none.swap(true).orNull()).to.be.null;
    const a: Opt<string> = opt(1).swap('');
    const b: Opt<string> = none.swap('');
    suppressUnused(a, b);
  });

  it('at', () => {
    expect(opt([1]).at(0).orFalse()).to.be.eq(1);
    expect(opt([]).at(0).orFalse()).to.be.false;
    expect(opt(null).at(0).orFalse()).to.be.false;
    expect(none.at(0).orFalse()).to.be.false;
    expect(() => {
      const x: Opt<unknown> = opt(1).at(0);
      suppressUnused(x);
    }).to.throw();
    expect(opt([1]).at(-1).orFalse()).to.be.eq(1);
    expect(opt([1, 2, 3]).at(-1).orFalse()).to.be.eq(3);
    expect(opt([null]).at(0).orFalse()).to.be.false;
    expect(opt([1, 2, 3]).at(-3).orFalse()).to.be.eq(1);
    expect(opt([1, 2, 3]).at(-4).orFalse()).to.be.false;
    expect(opt('Palico').at(0).orNull()).to.be.eq('P');
    expect(opt('Palico').at(-1).orNull()).to.be.eq('o');
    expect(opt('Palico').at(-4).orNull()).to.be.eq('l');
    expect(opt(undefined as string | undefined).at(0).orNull()).to.be.null;
    const x: Opt<number> = opt([1]).at(0);
    suppressUnused(x);
    expect(opt([4] as readonly number[]).at(0).orFalse()).to.be.eq(4);
  });

  it('headIn', () => {
    expect(opt([1, 2, 3]).headIn().orFalse()).to.be.eq(1);
    expect(opt([]).headIn().orFalse()).to.be.false;
    expect(opt(null).headIn().orFalse()).to.be.false;
    expect(opt('Palico').headIn().orFalse()).to.be.eq('P');
    expect(opt([1, 2, 3] as readonly number[]).headIn().orFalse()).to.be.eq(1);
  });

  it('lastIn', () => {
    expect(opt([1, 2, 3]).lastIn().orFalse()).to.be.eq(3);
    expect(opt([]).lastIn().orFalse()).to.be.false;
    expect(opt(null).lastIn().orFalse()).to.be.false;
    expect(opt('Palico').lastIn().orFalse()).to.be.eq('o');
    expect(opt([1, 2, 3] as readonly number[]).lastIn().orFalse()).to.be.eq(3);
  });

  describe('testReOrFalse', () => {
    it('returns expected result', () => {
      expect(opt('a').testReOrFalse(/a/)).to.be.true;
      expect(opt('b').testReOrFalse(/a/)).to.be.false;
      const res1: boolean = opt(null as null | string).testReOrFalse(/a/);
      expect(res1).to.be.false;
    });
    it('rejects invalid wrapper type', () => {
      expect(() => {
        // @ts-expect-error invalid input type
        const x: boolean = opt(7).testReOrFalse(/a/);
        suppressUnused(x);
      }).to.throw('testReOrFalse only works on Opt<string>');
    });
  });

  it('end', () => {
    expect(opt(null).end).to.be.undefined;
    const f = (x: unknown): void => opt(x).onBoth(noop, noop).end;
    // same as
    const g = (x: unknown): void => { opt(x).onBoth(noop, noop); };
    expect(f(0)).to.be.undefined;
    expect(f(null)).to.be.undefined;
    suppressUnused(g);
  });

  it('apply', () => {
    const r1: number | null = opt(inc).apply(7).orNull();
    expect(r1).to.be.eql(8);
    const f = (a: number, b: string): boolean => (a + 1).toFixed() === b;
    const r2: boolean = opt(f).apply(7, '8').orFalse();
    expect(r2).to.be.true;
    const f2 = (a: number) => (b: string): boolean => (a + 1).toFixed() === b;
    const r3: boolean = opt(f2).apply(1).apply('2').orFalse();
    expect(r3).to.be.true;
    expect(
      none.apply(0).orNull(), // None
    ).to.be.null;
    // @ts-expect-error calling number function with string
    opt(inc).apply('wrong number');
    // @ts-expect-error calling number
    expect(() => opt(1).apply(7)).to.throw('Invalid input - expected function');
    const add = (a: number, b: number) => a + b;
    expect(
      opt(add).apply(2, 3) // Some(5)
              .orNull(),
    ).to.be.eq(5);
    const g = chai.spy();
    expect(opt(g).apply().orNull()).to.be.null;
    expect(g).to.have.been.called.once;
    const sub = (a: number) => (b: number) => a - b;
    expect(
      opt(sub).apply(10).apply(3) // Some(7)
              .orNull(),
    ).to.be.eq(7);
  });

  it('onFunc', () => {
    const f = chai.spy();
    expect(
      opt(f).onFunc() // calls `f` and returns Some(f)
            .orNull(),
    ).to.be.eq(f);
    expect(f).to.have.been.called.once;
    const g = (a: number, b: number): void => console.log(a, b);
    const gg = chai.spy(g);
    expect(
      opt(gg).onFunc(1, 2) // calls `g` (prints 1 and 2), returns Some(g)
             .orNull(),
    ).to.be.eq(gg);
    expect(gg).to.have.been.called.once;
    expect(gg).to.have.been.called.with(1, 2);
    // @ts-expect-error wrong argument type
    opt(inc).onFunc('not a number');
    // @ts-expect-error content is not a function
    expect(() => opt(true).onFunc(1)).to.throw();
    expect(
      none.onFunc(79) // None
          .orNull()).to.be.null;
  });

  it('minIn', () => {
    const res1: Opt<number> = opt([] as number[]).minIn();
    expect(res1.orNull()).to.be.null;
    expect(opt([]).minIn().orNull()).to.be.null;
    expect(none.minIn().orNull()).to.be.null;
    expect(opt([1]).minIn().orNull()).to.be.eq(1);
    expect(opt([1, 3]).minIn().orNull()).to.be.eq(1);
    expect(opt([3, 1]).minIn().orNull()).to.be.eq(1);
    expect(opt([5, 1, 3]).minIn().orNull()).to.be.eq(1);
    expect(opt([1, 1, 1]).minIn().orNull()).to.be.eq(1);
    expect(() => opt(0).minIn()).to.throw();
  });

  it('maxIn', () => {
    const res1: Opt<number> = opt([] as number[]).maxIn();
    expect(res1.orNull()).to.be.null;
    expect(opt([]).maxIn().orNull()).to.be.null;
    expect(none.maxIn().orNull()).to.be.null;
    expect(opt([7]).maxIn().orNull()).to.be.eq(7);
    expect(opt([7, 3]).maxIn().orNull()).to.be.eq(7);
    expect(opt([3, 7]).maxIn().orNull()).to.be.eq(7);
    expect(opt([5, 7, 3]).maxIn().orNull()).to.be.eq(7);
    expect(opt([7, 7, 7]).maxIn().orNull()).to.be.eq(7);
    expect(() => opt(0).maxIn()).to.throw();
  });
});

describe('helper functions', () => {
  describe('some', () => {
    it('create', () => {
      expect(some(0).orNull()).to.eq(0);
      expect(some('').orNull()).to.eq('');
      expect(some(NaN).orNull()).to.eql(NaN);
      expect(some(null).orUndef()).to.eq(null);
    });

    it('falsy', () => {
      expect(some(null).orUndef()).to.eq(null);
      expect(some(NaN).orUndef()).to.eql(NaN);
      expect(some(undefined).orNull()).to.eq(undefined);
    });
  });

  it('fromArray', () => {
    expect(Opt.fromArray([]).orNull()).to.eq(null);
    expect(Opt.fromArray([1]).orNull()).to.eq(1);
    // expect(fromArray([1, 2]).orNull()).to.eq(1);
  });

  describe('fromObject', () => {
    interface Obj {
      value: number;
      otherValue: string;
    }

    const genObj = (): Obj => ({
      value: 4,
      otherValue: '7',
    });

    it('constructs opt from object with default field name', () => {
      const a: Opt<number> = Opt.fromObject(genObj());
      expect(a.orNull()).to.be.eq(4);
    });

    it('constructs opt from object with custom field name', () => {
      const a: Opt<string> = Opt.fromObject(genObj(), 'otherValue');
      expect(a.orNull()).to.be.eq('7');
    });

    it('checks field name and type', () => {
      // @ts-expect-error invalid type of field
      const a: Opt<boolean> = Opt.fromObject(genObj());
      // @ts-expect-error invalid type of field
      const b: Opt<boolean> = Opt.fromObject(genObj(), 'value');
      // @ts-expect-error missing field
      const c: Opt<boolean> = Opt.fromObject(genObj(), 'x');
      suppressUnused(a, b, c);
    });

    it('example', () => {
      expect(
        Opt.fromObject({value: 4}) // Some(4)
           .orNull(),
      ).to.be.eq(4);

      expect(
        Opt.fromObject({id: 4, something: '?'}, 'id') // Some(4)
           .orNull(),
      ).to.be.eq(4);

      expect(
        Opt.fromObject({value: null}) // None
           .orElseAny(''),
      ).to.be.eq('');
    });
  });

  it('isOpt', () => {
    expect(isOpt(0)).to.be.false;
    expect(isOpt(undefined)).to.be.false;
    expect(isOpt({value: 0})).to.be.false;
    expect(isOpt(some(1))).to.be.true;
    expect(isOpt(none)).to.be.true;
  });
});

describe('optFalsy', () => {
  it('construction', () => {
    expect(optFalsy(undefined).isEmpty).to.be.true;
    expect(optFalsy(NaN).isEmpty).to.be.true;
    expect(optFalsy(null).isEmpty).to.be.true;
    expect(optFalsy('').isEmpty).to.be.true;
    expect(optFalsy('a').isEmpty).to.be.false;
    expect(optFalsy(0).isEmpty).to.be.true;
    expect(optFalsy(1).isEmpty).to.be.false;
    expect(optFalsy({}).isEmpty).to.be.false;
    expect(optFalsy([]).isEmpty).to.be.false;
  });
  it('false not in T', () => {
    const x: number | null = optFalsy(1 as number | false).orNull();
    expect(x).to.be.eq(1);
  });
});

describe('optEmptyArray', () => {
  it('construction', () => {
    expect(optEmptyArray(null).isEmpty).to.be.true;
    expect(optEmptyArray(undefined).isEmpty).to.be.true;
    expect(optEmptyArray([]).isEmpty).to.be.true;
    expect(optEmptyArray([0]).isEmpty).to.be.false;
    expect(optEmptyArray([0]).orNull()).to.eql([0]);
  });
});

describe('optEmptyObject', () => {
  it('construction', () => {
    expect(optEmptyObject(null).isEmpty).to.be.true;
    expect(optEmptyObject(undefined).isEmpty).to.be.true;
    expect(optEmptyObject({}).isEmpty).to.be.true;
    expect(optEmptyObject({a: 1}).isEmpty).to.be.false;
    expect(optEmptyObject({a: 1}).map(x => x.a).orNull()).to.be.eq(1);
  });
});

describe('optEmptyString', () => {
  it('construction', () => {
    expect(optEmptyString(undefined).isEmpty).to.be.true;
    expect(optEmptyString(NaN).isEmpty).to.be.true;
    expect(optEmptyString(null).isEmpty).to.be.true;
    expect(optEmptyString('').isEmpty).to.be.true;
    expect(optEmptyString('a').isEmpty).to.be.false;
    expect(optEmptyString(0).isEmpty).to.be.false;
    expect(optEmptyString(1).isEmpty).to.be.false;
    expect(optEmptyString({}).isEmpty).to.be.false;
    expect(optEmptyString([]).isEmpty).to.be.false;
  });
});

describe('optZero', () => {
  it('construction', () => {
    expect(optZero(undefined).isEmpty).to.be.true;
    expect(optZero(NaN).isEmpty).to.be.true;
    expect(optZero(null).isEmpty).to.be.true;
    expect(optZero(0).isEmpty).to.be.true;
    expect(optZero(1).isEmpty).to.be.false;
    expect(optZero('').isEmpty).to.be.false;
    expect(optZero('a').isEmpty).to.be.false;
    expect(optZero({}).isEmpty).to.be.false;
    expect(optZero([]).isEmpty).to.be.false;
  });
});

describe('optNegative', () => {
  it('construction', () => {
    expect(optNegative(undefined).isEmpty).to.be.true;
    expect(optNegative(NaN).isEmpty).to.be.true;
    expect(optNegative(null).isEmpty).to.be.true;
    expect(optNegative(-1).isEmpty).to.be.true;
    expect(optNegative(-0.0001).isEmpty).to.be.true;
    expect(optNegative(-10000).isEmpty).to.be.true;
    expect(optNegative(0).isEmpty).to.be.false;
    expect(optNegative(0).orNull()).to.be.eq(0);
    expect(optNegative(1).isEmpty).to.be.false;
    expect(optNegative(1).orNull()).to.be.eq(1);
    expect(optNegative(10000).isEmpty).to.be.false;
  });
});

describe('optInfinity', () => {
  it('construction', () => {
    expect(optInfinity(undefined).isEmpty).to.be.true;
    expect(optInfinity(NaN).isEmpty).to.be.true;
    expect(optInfinity(null).isEmpty).to.be.true;
    expect(optInfinity(Infinity).isEmpty).to.be.true;
    expect(optInfinity(-Infinity).isEmpty).to.be.true;
    expect(optInfinity(0).isSome()).to.be.true;
    expect(optInfinity(1).isSome()).to.be.true;
    expect(optInfinity(-1).isSome()).to.be.true;
    expect(optInfinity(0.0001).isSome()).to.be.true;
    expect(optInfinity(10000).isSome()).to.be.true;
  });
});

describe('optArrayOpt', () => {
  it('construction', () => {
    expect(optArrayOpt(undefined).isEmpty).to.be.true;
    expect(optArrayOpt(null).isEmpty).to.be.true;
    expect(optArrayOpt([]).isEmpty).to.be.false;
    expect(optArrayOpt([]).orNull()).to.be.eql([]);
    expect(optArrayOpt([0]).isEmpty).to.be.false;
    expect(optArrayOpt([0]).orNull()).to.eql([0]);
    expect(optArrayOpt([0, null, undefined, 1]).isEmpty).to.be.false;
    expect(optArrayOpt([0, null, undefined, 1]).orNull()).to.eql([0, 1]);
  });
});

describe('application', () => {
  it('ap', () => {
    expect(ap(opt(gt0))(opt(1)).orNull()).to.be.true;
    expect(ap(none)(opt(1)).orNull()).to.be.null;
    expect(ap(opt(gt0))(none).orNull()).to.be.null;
    expect(ap(none)(none).orNull()).to.be.null;
  });
  it('apFn', () => {
    expect(apFn(gt0)(opt(1)).orNull()).to.be.true;
    expect(apFn(gt0)(none).orNull()).to.be.null;
  });
});

describe('catOpts', () => {
  it('converts to array', () => {
    expect(catOpts([])).to.eql([]);
    expect(catOpts([some(1)])).to.eql([1]);
    expect(catOpts([none])).to.eql([]);
    expect(catOpts([opt(1), opt(null)])).to.eql([1]);
  });
  it('supports read-only array', () => {
    expect(catOpts([some(1)] as readonly Opt<number>[])).to.eql([1]);
  });
});

describe('mapOpt', () => {
  it('maps', () => {
    expect(mapOpt(opt)([1, 2, 3])).to.eql([1, 2, 3]);
  });
  it('omits', () => {
    expect(mapOpt(_x => none)([1])).to.eql([]);
  });
  it('maps and omits', () => {
    expect(mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1])).to.eql([1]);
  });
  it('works with readonly array', () => {
    expect(mapOpt(opt)([1, 2, 3] as readonly number[])).to.eql([1, 2, 3]);
  });
});

describe('isEmpty', () => {
  it('opt', () => {
    expect(isEmpty(opt(1))).to.be.false;
    expect(isEmpty(opt(null))).to.be.true;
    expect(isEmpty(opt([]))).to.be.false;
  });
  it('array', () => {
    expect(isEmpty([])).to.be.true;
    expect(isEmpty([1])).to.be.false;
    expect(isEmpty([null])).to.be.false;
  });
  it('null', () => {
    expect(isEmpty(null)).to.be.true;
  });
  it('undefined', () => {
    expect(isEmpty(undefined)).to.be.true;
  });
  it('Map', () => {
    expect(isEmpty(new Map())).to.be.true;
    expect(isEmpty(new Map().set(1, true))).to.be.false;
  });
  it('Set', () => {
    expect(isEmpty(new Set())).to.be.true;
    expect(isEmpty(new Set().add(2))).to.be.false;
  });
  it('object', () => {
    expect(isEmpty({})).to.be.true;
    expect(isEmpty({a: 1})).to.be.false;

    class A {
      a = 4;
    }

    class B extends A {}

    expect(isEmpty(new B())).to.be.false;
  });
  it('string', () => {
    expect(isEmpty('')).to.be.true;
    expect(isEmpty(' ')).to.be.false;
    // noinspection SpellCheckingInspection
    expect(isEmpty('asddgflhjglůerhlt')).to.be.false;
  });
  it('number', () => {
    expect(isEmpty(NaN)).to.be.true;
    expect(isEmpty(0)).to.be.false;
    expect(isEmpty(-5)).to.be.false;
  });
  it('throws on invalid type', () => {
    expect(() => isEmpty(true as any)).to.throw();
  });
  it('works with unions', () => {
    type OptNumMay = Opt<number> | undefined;
    const a: OptNumMay = undefined as OptNumMay;
    expect(isEmpty(a)).to.be.true;
    const b: OptNumMay = opt(null as null | number) as OptNumMay;
    expect(isEmpty(b)).to.be.true;
    const c: OptNumMay = opt(3) as OptNumMay;
    expect(isEmpty(c)).to.be.false;
  });
});

// uses isEmpty internally, so more tests shouldn't be necessary
describe('nonEmpty', () => {
  it('opt', () => {
    expect(nonEmpty(opt(2))).to.be.true;
    expect(nonEmpty(none)).to.be.false;
  });
  it('array', () => {
    expect(nonEmpty([])).to.be.false;
    expect(nonEmpty([1])).to.be.true;
  });
  it('works well with pipe', () => {
    const a: Opt<number> = opt(4) as Opt<number>;
    const inc = (x: number) => x + 1;
    expect(
      pipe(
        a,
        map(inc),
        nonEmpty,
      ),
    ).to.be.true;
  });
});

describe('isFull', () => {
  it('opt', () => {
    expect(isFull(opt(1))).to.be.true;
    expect(isFull(none)).to.be.false;
  });
  it('array', () => {
    expect(isFull([1])).to.be.true;
    expect(isFull([])).to.be.false;
  });
});

interface Person {
  name: string;
  surname: string | null;
}

interface Db {
  [_: string]: Person;
}

describe('examples', () => {
  it('basic', () => {
    // without
    const f = (name: string | undefined) => {
      if (!name || name === '') { throw new Error('Missing name.'); }
      return name[0];
    };

    // with
    const g = (name: string | undefined) => opt(name).orCrash('Missing name.')[0];

    f('Riker'); // 'R'
    g('Riker'); // 'R'

    // f(undefined); // exception thrown
    // g(undefined); // exception thrown

    expect(f('Riker')).to.eq('R');
    expect(g('Riker')).to.eq('R');

    expect(() => f(undefined)).to.throw();
    expect(() => g(undefined)).to.throw();
  });

  it('caseOf', () => {
    const console = {
      logHistory: [] as string[],
      log(x: string) { this.logHistory.push(x); },
    };

    // tslint:disable-next-line:no-console
    const fireMissiles = () => { console.log('FIRING!'); };
    // tslint:disable-next-line:no-console
    const printSuccess = (x: string) => { console.log(x); };

    const handleMoveVanilla = (usersMove?: string): void => { if (usersMove) printSuccess(usersMove); else fireMissiles(); };
    const handleMove = (usersMove?: string): void => opt(usersMove).onBoth(printSuccess, fireMissiles).end;

    handleMoveVanilla(); // prints FIRING!
    handleMove(); // prints FIRING!
    handleMoveVanilla('Build a pylon.'); // prints Build a pylon.
    handleMove('Build a pylon.'); // prints Build a pylon.

    // tslint:disable-next-line:no-console
    expect(console.logHistory).to.eql(['FIRING!', 'FIRING!', 'Build a pylon.', 'Build a pylon.']);
  });

  it('more advanced', () => {
    const db: Db = {
      0: {name: 'John', surname: null},
      1: {name: 'Worf', surname: 'Mercer'},
    };

    // without
    const f = (id: number | undefined): string | null => {
      if (id === undefined) { return null; }
      const item = db[id];
      if (!item) { return null; }
      const surname = item.surname ? item.surname.toUpperCase() : '<missing>';
      return item.name + ' ' + surname;
    };

    // with
    const g = (id: number | undefined): string | null => opt(id)
    .chainToOpt(x => db[x])
    .map(item => item.name + ' ' + opt(item.surname).map(x => x.toUpperCase()).orElse('<missing>'))
    .orNull();

    f(0); // 'John <missing>'
    g(0); // 'John <missing>'

    f(1); // 'Worf MERCER'
    g(1); // 'Worf MERCER'

    f(2); // null
    g(2); // null

    expect(f(0)).to.be.eq('John <missing>');
    expect(g(0)).to.be.eq('John <missing>');
    expect(f(1)).to.be.eq('Worf MERCER');
    expect(g(1)).to.be.eq('Worf MERCER');
    expect(f(2)).to.be.eq(null);
    expect(g(2)).to.be.eq(null);
  });
});

describe('ReduxDevtoolsCompatibilityHelper', () => {
  it('replacer', () => {
    const f = ReduxDevtoolsCompatibilityHelper.replacer;
    expect(f('a', 0)).to.eq(0);
    expect(f('a', null)).to.eq(null);
    expect(f('a', undefined)).to.eq(undefined);
    expect(f('a', NaN)).to.eql(NaN);
    expect(f('a', false)).to.eq(false);
    expect(f('a', true)).to.eq(true);
    expect(f('a', [])).to.eql([]);
    expect(f('b', {})).to.eql({});
    expect(f('a', {type: 'x'})).to.be.eql({type: 'x'});
    expect(f('a', none)).to.be.eql({type: 'Opt/None'});
    expect(f('a', some(1))).to.be.eql({type: 'Opt/Some', value: 1});
    expect(f('a', opt('Kirok'))).to.be.eql({type: 'Opt/Some', value: 'Kirok'});
    expect(f('a', opt({name: 'rigel'}))).to.be.eql({type: 'Opt/Some', value: {name: 'rigel'}});
  });

  it('reviver', () => {
    const f = ReduxDevtoolsCompatibilityHelper.reviver;
    expect(f('b', 0)).to.eq(0);
    expect(f('b', null)).to.eq(null);
    expect(f('b', undefined)).to.eq(undefined);
    expect(f('b', NaN)).to.eql(NaN);
    expect(f('b', false)).to.eq(false);
    expect(f('b', true)).to.eq(true);
    expect(f('b', [])).to.eql([]);
    expect(f('b', {})).to.eql({});
    expect(f('b', {type: null})).to.be.eql({type: null});
    expect(f('b', {type: 'x'})).to.be.eql({type: 'x'});
    const noneRevived = f('a', {type: 'Opt/None'});
    expect(noneRevived.orNull()).to.be.null;
    const someRevived = f('a', {type: 'Opt/Some', value: 248});
    expect(someRevived.orNull()).to.be.eq(248);
    const someUndefRevived = f('a', {type: 'Opt/Some'});
    expect(someUndefRevived.orNull()).to.be.eq(undefined);
  });
});

describe('serialize', () => {
  it('serializes', () => {
    const f = serialize;
    expect(f(none)).to.be.eql({type: 'Opt/None'});
    expect(f(some(1))).to.be.eql({type: 'Opt/Some', value: 1});
    expect(f(opt('Cid'))).to.be.eql({type: 'Opt/Some', value: 'Cid'});
    expect(f(opt({name: 'Cid'}))).to.be.eql({type: 'Opt/Some', value: {name: 'Cid'}});
  });
});

const expectSuccessDeserializationResult: (x: DeserializationResult<any>) => asserts x is DeserializationSuccess<any> = x => {
  expect(x.tag).to.be.eq('success');
}

describe('deserialize', () => {
  it('deserializes None', () => {
    const input = { type: 'Opt/None' };
    const result = deserialize(input, (x: unknown): x is number => typeof x === 'number');
    expectSuccessDeserializationResult(result);
    expect(result.value.isEmpty).to.be.true;
  });

  it('deserializes Some with valid inner type', () => {
    const input = { type: 'Opt/Some', value: 0 };
    const result = deserialize(input, (x: unknown): x is number => typeof x === 'number');
    expectSuccessDeserializationResult(result);
    expect(result.value.orNull()).to.be.eq(0);
  });

  it('fails to deserialize Some with invalid inner type', () => {
    const input = { type: 'Opt/Some', value: 'not a number' };
    const result = deserialize(input, (x: unknown): x is number => typeof x === 'number');
    expect(result).to.deep.equal({ tag: 'failure', reason: 'failed to validate inner type' });
  });

  it('fails to deserialize invalid type in type field', () => {
    const input = { type: 'InvalidType' };
    const result = deserialize(input, (x: unknown): x is number => typeof x === 'number');
    expect(result).to.deep.equal({ tag: 'failure', reason: 'not OptSerialized' });
  });

  it('fails to deserialize invalid input type', () => {
    const result = deserialize(4, (x: unknown): x is number => typeof x === 'number');
    expect(result).to.deep.equal({ tag: 'failure', reason: 'not OptSerialized' });
  });
});

describe('deserializeOrCrash', () => {
  it('deserializes without crashing', () => {
    const input = { type: 'Opt/Some', value: 4 };
    const result = deserializeOrCrash(input, isNumber);
    expect(result.orNull()).to.be.eq(4);
  });

  it('throws an error on invalid inner type', () => {
    const input = { type: 'Opt/Some', value: 'not a number' };
    expect(() => deserializeOrCrash(input, isNumber)).to.throw(Error, 'failed to validate inner type');
  });

  it('throws an error on invalid input type', () => {
    expect(() => deserializeOrCrash(4, isNumber)).to.throw(Error, 'not OptSerialized');
  });
});

describe('deserializeUnsafe', () => {
  it('deserializes without crashing', () => {
    const input = { type: 'Opt/Some', value: 2 };
    const result = deserializeUnsafe(input);
    expect(result.orNull()).to.be.eq(2);
  });

  it('returns None on invalid input type (not object)', () => {
    const result = deserializeUnsafe(9);
    expect(result.orNull()).to.be.null;
  })

  it('returns None on invalid input field type', () => {
    const input = { type: 'Zenos' };
    const result = deserializeUnsafe(input);
    expect(result.orNull()).to.be.null;
  });
});

describe('joinOpt', () => {
  it('joins', () => {
    expect(joinOpt(some(none)).orNull()).to.eql(null);
    expect(joinOpt(some(some(1))).orNull()).to.eql(1);
    expect(joinOpt(none).orNull()).to.eql(null);
  });
});

describe('lengthIn', () => {
  it('returns length of array inside opt', () => {
    expect(lengthIn(opt([1, 2, 3])).orNull()).to.be.eq(3);
    expect(lengthIn(none).orNull()).to.be.null;
  });
});

describe('fromArray', () => {
  it('creates opt from array', () => {
    expect(fromArray([7]).orNull()).to.be.eq(7);
    expect(fromArray([]).orNull()).to.be.null;
  });
  it('works with read-only array', () => {
    expect(fromArray([7] as readonly [7]).orNull()).to.be.eq(7);
    expect(fromArray([] as readonly []).orNull()).to.be.null;
  });
});

describe('toArray', () => {
  it('converts opt to array', () => {
    expect(toArray(opt(1))).to.be.eql([1]);
    expect(toArray(none)).to.be.eql([]);
  });
});

describe('fromObject', () => {
  it('converts object to opt', () => {
    expect(fromObject({value: 11}).orNull()).to.be.eq(11);
    expect(fromObject({value: NaN}).orNull()).to.be.null;
    // @ts-expect-error invalid field name
    expect(fromObject({id: 7}, 'idx').orNull()).to.be.null;
    expect(fromObject({id: 7}, 'id').orNull()).to.be.eq(7);
  });
});

describe('toObject', () => {
  it('converts opt to object', () => {
    const b: { value: number | null } = toObject()(opt(57));
    expect(b).to.be.eql({value: 57});
    const c: { c: number | null } = toObject('c')(opt(57));
    expect(c).to.be.eql({c: 57});
  });
});

describe('orCrash', () => {
  const f = flow2(opt, orCrash('msg'));
  it('crashes on none', () => {
    expect(() => f(null)).to.throw;
    expect(() => opt(null).orCrash('unexpected empty value')).to.throw;
  });
  it('returns value on some', () => {
    expect(f(1)).to.eq(1);
    expect(opt(1).orCrash('unexpected empty value')).to.eq(1);
  });
});

describe('map', () => {
  it('is correctly typed', () => {
    expect(map((x: number) => x)([])).to.eql([]);
    // ---
    const r1: number[] = map((x: number) => x + 1)([1]);
    // @ts-expect-error result should not be opt
    const r1Fail: Opt<number> = map((x: number) => x + 1)([1]);
    suppressUnused(r1Fail);
    expect(r1).to.eql([2]);
    expect(map((_x: number) => false)(none).orNull()).to.be.null;
    expect(map((x: number) => x + 1)(some(1)).orNull()).to.eq(2);
    // ---
    const r2: Opt<number> = map((x: number) => x + 1)(opt(1));
    // @ts-expect-error result should not be array
    const r2Fail: number[] = map((x: number) => x + 1)(opt(1));
    suppressUnused(r2Fail);
    expect(r2.orNull()).to.eq(2);
  });

  it('works with flow', () => {
    const r1 = flow2(id, id)([]);
    expect(r1).to.be.eql([]);
    // ---
    const r2 = flow2(map(inc), id)([1, 2]);
    expect(r2).to.be.eql([2, 3]);
    // ---
    const r3 = flow2(map(inc), id)(opt(1));
    expect(r3.orNull()).to.be.eq(2);
    // ---
    const r4 = flow2(id, id)(opt(1));
    expect(r4.orNull()).to.be.eq(1);
    // ---
    const r5 = flow2(map(inc), id)(opt(1));
    expect(r5.orNull()).to.be.eq(2);
    // ---
    const r6 = flow2(map(inc), map(gt0))(opt(1));
    expect(r6.orNull()).to.be.true;
  });
});

describe('mapWithIndex', () => {
  it('maps over array with index', () => {
    expect(mapWithIndex((x: string, i: number) => x + i)([])).to.be.eql([]);
    expect(mapWithIndex((x: string, i: number) => x + i)(['a', 'b'])).to.be.eql(['a0', 'b1']);
  });
});

describe('mapIn', () => {
  it('maps over array inside Opt', () => {
    expect(mapIn((x: number) => x + 1)(opt([1, 2, 3])).orNull()).to.be.eql([2, 3, 4]);
  });
});

describe('mapWithIndexIn', () => {
  it('maps over array inside Opt with index', () => {
    expect(mapWithIndexIn((x: string, i: number) => x + i)(opt(['a', 'b'])).orNull()).to.be.eql(['a0', 'b1']);
  });
});

describe('mapFlow', () => {
  expect(mapFlow(id)(opt(1)).orNull()).to.be.eq(1);
  expect(mapFlow(id)(opt(null)).orNull()).to.be.null;
  expect(mapFlow(inc, id, id, id, id)(opt(1)).orNull()).to.be.eq(2);
  expect(mapFlow(inc, id, id, id)(opt<number>(null)).orNull()).to.be.null;
});

describe('mapStr', () => {
  it('should map over characters in a string', () => {
    const toUpperCase = (c: string) => c.toUpperCase();
    expect(mapStr(toUpperCase)('hello')).to.equal('HELLO');
    expect(mapStr(c => c === 'o' ? '0' : c)('hello world')).to.equal('hell0 w0rld');
  });

  it('should work with empty strings', () => {
    expect(mapStr(c => c.toUpperCase())('')).to.equal('');
  });

  it('should work with single character strings', () => {
    expect(mapStr(c => c.toUpperCase())('a')).to.equal('A');
  });

  it('should work with Opt', () => {
    const toUpperCase = (c: string) => c.toUpperCase();
    expect(opt('hello').map(mapStr(toUpperCase)).orNull()).to.equal('HELLO');
    expect(opt<string>(null).map(mapStr(toUpperCase)).orNull()).to.be.null;
  });

  it('should replace characters with multiple characters', () => {
    expect(mapStr(c => c === 'r' ? 'ru' : c)('kirara')).to.equal('kiruarua');
  });

  it('should remove characters', () => {
    expect(mapStr(c => c === 'a' ? '' : c)('sango')).to.equal('sngo');
  });
});

describe('flatMap', () => {
  it('is correctly typed', () => {
    expect(flatMap((x: number) => [x])([])).to.eql([]);
    // ---
    const r1: number[] = flatMap((x: number) => [x + 1])([1]);
    // @ts-expect-error result should not be opt
    const r1Fail: Opt<number> = flatMap((x: number) => [x + 1])([1]);
    suppressUnused(r1Fail);
    expect(r1).to.eql([2]);
    expect(flatMap((_x: number) => none)(none).orNull()).to.be.null;
    expect(flatMap((x: number) => opt(x + 1))(some(1)).orNull()).to.eq(2);
    // ---
    const r2: Opt<number> = flatMap((x: number) => opt(x + 1))(opt(1));
    // @ts-expect-error result should not be array
    const r2Fail: number[] = flatMap((x: number) => opt(x + 1))(opt(1));
    suppressUnused(r2Fail);
    expect(r2.orNull()).to.eq(2);
  });

  it('works with flow', () => {
    const r1 = flow2(flatMap((x: number) => [x, x * 10]), id)([1, 2]);
    expect(r1).to.eql([1, 10, 2, 20]);
    // ---
    const r2 = flow2((x: number[]) => x, flatMap(x => [x, x * 10]))([1, 2, 3]);
    expect(r2).to.eql([1, 10, 2, 20, 3, 30]);
  });

  describe('works with arrays', () => {
    it('longer than one item', () => {
      const r1: number[] = flatMap((x: number) => [x, x * 2])([1, 2, 3]);
      expect(r1).to.eql([1, 2, 2, 4, 3, 6]);
    });
    it('removing and adding', () => {
      const r1: number[] = flatMap((x: number) => x > 2 ? [] : [x, x + 10])([1, 2, 3]);
      expect(r1).to.eql([1, 11, 2, 12]);
    });
  });

  it('works with readonly arrays', () => {
    expect(flatMap((x: number) => [x + 1])([6] as readonly number[])).to.be.eql([7]);
  });
});

describe('flatMapIn', () => {
  it('flatMaps over array inside Opt', () => {
    expect(flatMapIn((x: number) => [x, x * 10])(opt([1, 2])).orNull()).to.eql([1, 10, 2, 20]);
  });
});

describe('chainIn', () => {
  it('chains a function over an array in Some', () => {
    const result = chainIn((x: number) => [x, x * 2])(opt([1, 2]));
    expect(result.orNull()).to.eql([1, 2, 2, 4]);
  });
});

describe('act', () => {
  it('acts', () => {
    expect(act(opt)(opt(1)).orNull()).to.be.eq(1);
    expect(chainFlow(opt)(opt(1)).orNull()).to.be.eq(1);
    expect(act(opt, opt, opt)(opt(1)).orNull()).to.be.eq(1);
    expect(act(opt, opt, () => none)(opt(1)).orNull()).to.be.null;
  });
});

describe('chainToOpt', () => {
  it('chains to opt', () => {
    expect(chainToOpt(_x => undefined)(opt(1)).orNull()).to.be.null;
    expect(chainToOpt(_x => 'x')(opt(1)).orNull()).to.be.eq('x');
  });
});

interface ActToOptTest {
  f?: boolean;
  a?: { b?: number, c?: number }[];
}

describe('actToOpt', () => {
  it('acts and wraps in opt', () => {
    expect(actToOpt(id)(some(1)).orNull()).to.be.eq(1);
    expect(chainToOptFlow(id, id)(none).orNull()).to.be.null;
    expect(actToOpt((x: number) => x, inc, inc)(opt(1)).orNull()).to.be.eq(3);
    const find = <T>(pred: (_: T) => boolean) => (xs: T[]): T | undefined => xs.find(pred);

    const data: ActToOptTest[] = [{}, {f: true, a: [{b: 7, c: 1}]}, {a: [{}]}];
    const r = opt(data).actToOpt(
      find(x => Boolean(x?.f)), // {f: true, a: [{b: 7, c: 1}]}
      x => x?.a, // [{b: 7, c: 1}]
      find(x => x.b === 8), // undefined
    ); // None
    expect(r.isEmpty).to.be.eq(true, r.toString());
  });
});

describe('someOrCrash', () => {
  it('crashes on none', () => {
    expect(() => someOrCrash('x')(none)).to.throw('x');
  });
  it('returns some on some', () => {
    expect(someOrCrash('x')(opt(0)).orNull()).to.be.eq(0);
  });
});

describe('orUndef', () => {
  it('returns value', () => {
    expect(orUndef(opt(1))).to.be.eq(1);
  });
  it('returns undefined on none', () => {
    expect(orUndef(none)).to.be.undefined;
  });
});

describe('orNull', () => {
  it('returns value', () => {
    expect(orNull(opt(1))).to.be.eq(1);
  });
  it('returns null on none', () => {
    expect(orNull(none)).to.be.null;
  });
});

describe('orFalse', () => {
  it('returns value', () => {
    expect(orFalse(opt(1))).to.be.eq(1);
  });
  it('returns false on none', () => {
    expect(orFalse(none)).to.be.false;
  });
});

describe('orTrue', () => {
  it('returns value', () => {
    expect(orTrue(opt(1))).to.be.eq(1);
  });
  it('returns false on none', () => {
    expect(orTrue(none)).to.be.true;
  });
});

describe('orNaN', () => {
  it('returns value', () => {
    expect(orNaN(opt(1))).to.be.eq(1);
  });
  it('returns false on none', () => {
    expect(orNaN(none)).to.be.NaN;
  });
});

describe('caseOf', () => {
  it('uses onSome with some', () => {
    expect(caseOf(inc)(() => 7)(opt(1))).to.be.eq(2);
  });
  it('uses onNone with none', () => {
    expect(caseOf(inc)(() => 7)(none)).to.be.eq(7);
  });
});

describe('fold', () => {
  it('returns the initial value when none', () => {
      expect(fold((x: number) => x)(0)(none)).to.be.eq(0);
  });

  it('applies the function when some', () => {
      expect(fold((x: number) => x * 2)(0)(some(5))).to.be.eq(10);
  });
});

describe('foldIn', () => {
  it('returns none when none', () => {
      expect(foldIn((acc: number, x: number) => acc + x)(0)(none).orNull()).to.be.null;
  });

  it('folds over array inside opt when some', () => {
      expect(foldIn((acc: number, x: number) => acc + x)(0)(some([1, 2, 3])).orNull()).to.be.eq(6);
  });
});


describe('onBoth', () => {
  let onSome: (_: number) => void;
  let onNone: () => void;
  beforeEach(() => {
    onSome = chai.spy((x: number) => x * 10);
    onNone = chai.spy(() => 99);
  });

  it('calls onSome with some', () => {
    onSome.should.have.not.been.called();
    onNone.should.have.not.been.called();
    expect(onBoth(onSome)(onNone)(opt(7)).orNull()).to.eql(7);
    onSome.should.have.been.called.exactly(1);
    onSome.should.have.been.called.with(7);
    onNone.should.have.not.been.called();
  });
  it('calls onNone with none', () => {
    onSome.should.have.not.been.called();
    onNone.should.have.not.been.called();
    expect(onBoth(onSome)(onNone)(none).orNull()).to.be.null;
    onSome.should.have.not.been.called();
    onNone.should.have.been.called.exactly(1);
    onNone.should.have.been.called.with.exactly();
  });
});

describe('pipe', () => {
  it('pipes', () => {
    expect(pipe(0, inc)).to.be.eq(1);
    expect(pipe(0, inc, inc)).to.be.eq(2);
    expect(pipe(0, inc, inc, inc)).to.be.eq(3);
    expect(pipe(0, inc, inc, inc, inc)).to.be.eq(4);
    expect(pipe(0, inc, inc, inc, inc, inc)).to.be.eq(5);
    expect(pipe(0, inc, inc, inc, inc, inc, inc)).to.be.eq(6);
    expect(pipe(0, inc, inc, inc, inc, inc, inc, inc)).to.be.eq(7);
    expect(pipe(0, inc, inc, inc, inc, inc, inc, inc, inc)).to.be.eq(8);
    expect(pipe(0, inc, inc, inc, inc, inc, inc, inc, inc, inc)).to.be.eq(9);
    expect(pipe(0, inc, x => x.toString(10), x => x.length)).to.be.eq(1);
  });
});

describe('contains', () => {
  it('positive', () => {
    expect(contains(1)(opt(1))).to.be.true;
  });
  it('negative', () => {
    expect(contains(2)(opt(1))).to.be.false;
    expect(contains(2)(none)).to.be.false;
  });
});

describe('has', () => {
  it('positive', () => {
    expect(has(1)(opt(1))).to.be.true;
  });
  it('negative', () => {
    expect(has(2)(opt(1))).to.be.false;
    expect(has(2)(none)).to.be.false;
  });
});

describe('hasIn', () => {
  it('positive', () => {
    expect(hasIn(1)(opt([1]))).to.be.true;
  });
  it('negative', () => {
    expect(hasIn(1)(none)).to.be.false;
    expect(hasIn(1)(opt([2]))).to.be.false;
  });
});

describe('elemOf', () => {
  it('returns true when element is in array', () => {
    expect(elemOf([1, 2, 3])(2)).to.be.true;
    expect(elemOf(['a', 'b', 'c'])('b')).to.be.true;
  });

  it('returns false when element is not in array', () => {
    expect(elemOf([1, 2, 3])(4)).to.be.false;
    expect(elemOf(['a', 'b', 'c'])('d')).to.be.false;
  });

  it('works with opt.exists', () => {
    expect(opt(1).exists(elemOf([1, 2, 3]))).to.be.true;
    expect(opt(4).exists(elemOf([1, 2, 3]))).to.be.false;
  });
});

describe('elemOfStr', () => {
  it('returns true when substring is in string', () => {
    expect(elemOfStr('abc')('a')).to.be.true;
    expect(elemOfStr('abc')('ab')).to.be.true;
    expect(elemOfStr('abc')('abc')).to.be.true;
  });

  it('returns false when substring is not in string', () => {
    expect(elemOfStr('abc')('x')).to.be.false;
    expect(elemOfStr('abc')('ac')).to.be.false;
    expect(elemOfStr('abc')('abcd')).to.be.false;
  });

  it('works with opt.exists', () => {
    expect(opt('a').exists(elemOfStr('abc'))).to.be.true;
    expect(opt('ab').exists(elemOfStr('abc'))).to.be.true;
    expect(opt('x').exists(elemOfStr('abc'))).to.be.false;
  });

  it('works with pipe', () => {
    expect(pipe('b', elemOfStr('abc'))).to.be.true;
    expect(pipe('x', elemOfStr('abc'))).to.be.false;
  });
});

describe('elemOfStrIn', () => {
  it('returns Some(true) when substring is in the string', () => {
    expect(elemOfStrIn('abc')(opt('a')).orNull()).to.be.true;
    expect(elemOfStrIn('abc')(opt('ab')).orNull()).to.be.true;
    expect(elemOfStrIn('abc')(opt('abc')).orNull()).to.be.true;
  });

  it('returns Some(false) when substring is not in the string', () => {
    expect(elemOfStrIn('abc')(opt('d')).orNull()).to.be.false;
    expect(elemOfStrIn('abc')(opt('ac')).orNull()).to.be.false;
    expect(elemOfStrIn('abc')(opt('abcd')).orNull()).to.be.false;
  });

  it('returns None when given None', () => {
    expect(elemOfStrIn('abc')(none).orNull()).to.be.null;
  });

  it('works with empty strings', () => {
    expect(elemOfStrIn('')(opt('')).orNull()).to.be.true;
    expect(elemOfStrIn('abc')(opt('')).orNull()).to.be.true;
    expect(elemOfStrIn('')(opt('a')).orNull()).to.be.false;
  });

  it('can be used with pipe', () => {
    const result = pipe(
      'a',
      opt,
      elemOfStrIn('abc'),
      orNull
    );
    expect(result).to.be.true;
  });
});

describe('exists', () => {
  it('positive', () => {
    expect(exists(eqAny(1))(opt(1))).to.be.true;
  });
  it('negative', () => {
    expect(exists(eqAny(2))(opt(1))).to.be.false;
    expect(exists(eqAny(2))(none)).to.be.false;
  });
});

describe('existsIn', () => {
  it('returns true if any element in wrapped array satisfies predicate', () => {
    expect(existsIn((x: number) => x > 2)(opt([1, 2, 3, 4])).orNull()).to.be.true;
  });

  it('returns false if no element in wrapped array satisfies predicate', () => {
    expect(existsIn((x: number) => x > 5)(opt([1, 2, 3, 4])).orNull()).to.be.false;
  });
});

describe('forAll', () => {
  it('positive', () => {
    expect(forAll(eqAny(1))(opt(1))).to.be.true;
    expect(forAll(eqAny(2))(none)).to.be.true;
  });
  it('negative', () => {
    expect(forAll(eqAny(2))(opt(1))).to.be.false;
  });
});

describe('forAllIn', () => {
  it('returns true if all elements in wrapped array satisfy predicate', () => {
    expect(forAllIn((x: number) => x > 0)(opt([1, 2, 3, 4])).orNull()).to.be.true;
  });
});

describe('orElse', () => {
  it('returns value on some', () => {
    expect(orElse(0)(opt(1))).to.be.eq(1);
  });
  it('returns default value on none', () => {
    expect(orElse(0)(none)).to.be.eq(0);
  });
});

describe('orElseLazy', () => {
  it('calls function and returns its value for none', () => {
    const noneF = chai.spy(() => 0);
    expect(orElseLazy(noneF)(none)).to.be.eq(0);
    noneF.should.have.been.called();
  });

  it('uses inner value for some and doesn\'t call the function', () => {
    const someF = chai.spy(() => 0);
    expect(orElseLazy(someF)(some(1))).to.be.eq(1);
    someF.should.have.not.been.called();
  });
});

describe('orElseAny', () => {
  it('returns value on some', () => {
    expect(orElseAny(true)(opt(1))).to.be.eq(1);
  });
  it('returns default value on none', () => {
    expect(orElseAny(true)(none)).to.be.true;
  });
});

describe('alt', () => {
  it('returns value on some', () => {
    expect(alt(opt(0))(opt(1)).orNull()).to.be.eq(1);
  });
  it('returns default value on none', () => {
    expect(alt(opt(0))(none).orNull()).to.be.eq(0);
  });
});

describe('altOpt', () => {
  it('returns value on some', () => {
    expect(altOpt(0)(opt(1)).orNull()).to.be.eq(1);
  });
  it('returns default value on none', () => {
    expect(altOpt(0)(none).orNull()).to.be.eq(0);
  });
  it('returns none on none and empty value', () => {
    expect(altOpt(undefined as number | undefined)(opt(NaN)).orNull()).to.be.null;
  });
});

describe('bimap', () => {
  it('uses onSome with some', () => {
    expect(bimap(inc)(() => 7)(opt(1)).orNull()).to.be.eq(2);
  });
  it('uses onNone with none', () => {
    expect(bimap(inc)(() => 7)(none).orNull()).to.be.eq(7);
  });
});

describe('flatBimap', () => {
  it('uses onSome with some', () => {
    expect(flatBimap((x: number) => opt(inc(x)))(() => opt(7))(opt(1)).orNull()).to.be.eq(2);
  });
  it('uses onNone with none', () => {
    expect(flatBimap((x: number) => opt(inc(x)))(() => opt(7))(none).orNull()).to.be.eq(7);
  });
});

describe('zipArray', () => {
  it('zips', () => {
    expect(zipArray([1, 2])([true, false])).to.be.eql([[1, true], [2, false]]);
  });
  it('different lengths', () => {
    expect(zipArray([1, 2, 3])([true, false])).to.be.eql([[1, true], [2, false]]);
    expect(zipArray([1])([true, false, null])).to.be.eql([[1, true]]);
    expect(zipArray([])([true, false])).to.be.eql([]);
    expect(zipArray([1, 2, 3])([])).to.be.eql([]);
  });
});

describe('zipOpt', () => {
  describe('checks types', () => {
    it('opt', () => {
      const a: Opt<[number, boolean]> = zipOpt(opt(1))(opt(true));
      // @ts-expect-error wrong result type
      const aFail: Opt<[boolean, number]> = zipOpt(opt(1))(opt(true));
      suppressUnused(a, aFail);
    });
    it('mixing', () => {
      // @ts-expect-error wrong result type
      const aFail: [number, boolean][] = zipOpt(opt(1))(opt(true));
      suppressUnused(aFail);
    });
  });

  it('opt', () => {
    expect(zipOpt(opt(1))(opt(2)).orNull()).to.be.eql([1, 2]);
    expect(zipOpt(opt(1))(none).orNull()).to.be.eql(null);
    expect(zipOpt(none)(opt(2)).orNull()).to.be.eql(null);
    expect(zipOpt(none)(none).orNull()).to.be.eql(null);
  });

  it('works with flow', () => {
    const a: Opt<[string, number]> = flow2((x: Opt<number>) => x, zipOpt(opt('x')))(opt(2));
    expect(a.orNull()).to.be.eql(['x', 2]);
    const b: Opt<[string, number]> = flow2(zipOpt(opt('x')), id)(opt(2));
    expect(b.orNull()).to.be.eql(['x', 2]);
  });
});

describe('zip3Opt', () => {
  it('zips', () => {
    expect(zip3Opt(opt(1))(opt(2))(opt(3)).orNull()).to.be.eql([1, 2, 3]);
  });
});

describe('zip4Opt', () => {
  it('zips', () => {
    expect(zip4Opt(opt(1))(opt(2))(opt(3))(opt(4)).orNull()).to.be.eql([1, 2, 3, 4]);
  });
});

describe('zip5Opt', () => {
  it('zips', () => {
    expect(zip5Opt(opt(1))(opt(2))(opt(3))(opt(4))(opt(5)).orNull()).to.be.eql([1, 2, 3, 4, 5]);
  });
});

describe('zipIn', () => {
  it('zips array inside opt with another array', () => {
    expect(zipIn(opt([1, 2, 3]))([4, 5, 6]).orNull()).to.be.eql([[1, 4], [2, 5], [3, 6]]);
  });
});

describe('filter', () => {
  it('checks types', () => {
    const a: Opt<number> = filter(gt0)(opt(1));
    // @ts-expect-error wrong result type
    const aFail: number[] = filter(gt0)(opt(1));
    suppressUnused(a, aFail);
    // ---
    const b: number[] = filter(gt0)([7]);
    // @ts-expect-error wrong result type
    const bFail: Opt<number> = filter(gt0)([]);
    suppressUnused(b, bFail);
    // ---
  });
  describe('filters', () => {
    it('opt', () => {
      expect(filter((x: number) => x > 1)(opt(1)).orNull()).to.be.null;
      expect(filter(gt0)(opt(1)).orNull()).to.be.eq(1);
    });
    it('array', () => {
      expect(filter((x: number) => x > 1)([1])).to.be.eql([]);
      expect(filter(gt0)([-1, 0, 1])).to.be.eql([1]);
    });
    it('read-only array', () => {
      expect(filter(gt0)([-1, 0, 1] as readonly number[])).to.be.eql([1]);
    });
  });
});

describe('filterIn', () => {
  it('filters array inside opt', () => {
    expect(filterIn((x: number) => x > 1)(opt([1, 2, 3])).orNull()).to.be.eql([2, 3]);
  });
});

describe('findIn', () => {
  it('finds first element satisfying predicate in array inside opt', () => {
    expect(findIn((x: number) => x > 2)(opt([1, 2, 3, 4])).orNull()).to.be.eq(3);
  });
});

describe('noneIf', () => {
  it('passes opt when predicate doesn\'t hold', () => {
    expect(noneIf(gt0)(opt(-5)).orNull()).to.be.eq(-5);
  });
  it('returns none when predicate holds', () => {
    expect(noneIf(gt0)(opt(5)).orNull()).to.be.null;
  });
});

describe('noneIfEmpty', () => {
  it('passes opt when not empty', () => {
    expect(noneIfEmpty(opt(1)).orNull()).to.be.eq(1);
    expect(noneIfEmpty(opt([1])).orNull()).to.be.eql([1]);
    expect(noneIfEmpty(opt({a:1})).orNull()).to.be.eql({a:1});
  });
  it('returns none when empty', () => {
    expect(noneIfEmpty(opt(NaN)).orNull()).to.be.null;
    expect(noneIfEmpty(opt([])).orNull()).to.be.null;
    expect(noneIfEmpty(opt({})).orNull()).to.be.null;
  });
})

describe('noneWhen', () => {
  it('passes opt on false', () => {
    expect(noneWhen(false)(opt(1)).orNull()).to.be.eq(1);
  });
  it('returns none on true', () => {
    expect(noneWhen(true)(opt(1)).orNull()).to.be.null;
  });
});

describe('count', () => {
  it('checks types', () => {
    const a: 0 | 1 = count(gt0)(opt(1));
    const a2: number = count(gt0)(opt(1)); // 0 | 1 is assignable to number
    suppressUnused(a, a2);
    // ---
    const b: number = count(gt0)([0]);
    // @ts-expect-error but not the other way around
    const bFail: 0 | 1 = count(gt0)([0]);
    suppressUnused(b, bFail);
  });

  describe('counts', () => {
    it('opt', () => {
      expect(count(gt0)(opt(1))).to.be.eq(1);
      expect(count(gt0)(opt(-1))).to.be.eq(0);
      expect(count(gt0)(opt(NaN))).to.be.eq(0);
    });
    it('array', () => {
      expect(count(gt0)([1])).to.be.eq(1);
      expect(count(gt0)([-1])).to.be.eq(0);
      expect(count(gt0)([])).to.be.eq(0);
      expect(count(gt0)([-1, 1, 11])).to.be.eq(2);
    });
    it('read-only array', () => {
      expect(count(gt0)([] as readonly number[])).to.be.eq(0);
    });
  });

  it('crashes when given invalid input', () => {
    expect(() => count(gt0)('' as any)).to.throw('Invalid input to count, only Opt and Array are supported: ""');
    expect(() => count(gt0)(true as any)).to.throw('Invalid input to count, only Opt and Array are supported: true');
    expect(() => count(gt0)(new Date(0) as any))
    .to.throw('Invalid input to count, only Opt and Array are supported: "1970-01-01T00:00:00.000Z"');
  });

  it('example', () => {
    expect(
      opt('Mu').count(x => x.length > 3), // 0
    ).to.be.eq(0);
    expect(
      opt('Ichi').count(x => x.length > 3), // 1
    ).to.be.eq(1);
    const greaterThanZero = (x: number) => x > 0;
    expect(
      count(greaterThanZero)([-3, 0, 5, 10]), // 2
    ).to.be.eq(2);
  });
});

describe('countIn', () => {
  it('counts elements satisfying predicate in array inside opt', () => {
    const result = countIn((x: number) => x % 2 === 0)(opt([1, 2, 3, 4]));
    expect(result.orNull()).to.equal(2);
  });
});

describe('find', () => {
  interface Data {
    id: number;
    value: number;
  }

  const itemA: Data = {id: 6, value: 9};
  const itemB: Data = {id: 8, value: 2};
  const itemC: Data = {id: 6, value: 0};

  it('returns first match in Some', () => {
    expect(find(eq(4))([4]).orNull()).to.be.eq(4);
    expect(isOpt(find(eq(4))([4]))).to.be.true;
    expect(find(eq(4))([1, 4]).orNull()).to.be.eq(4);
    expect(find(eq(4))([1, 4, 5]).orNull()).to.be.eq(4);
    expect(find((x: Data) => x.id === 6)([itemA, itemB]).orNull()).to.be.eq(itemA);
    expect(find((x: Data) => x.id === 6)([itemA, itemB, itemC]).orNull()).to.be.eq(itemA);
  });

  it('returns None when nothing matches', () => {
    expect(find(eq(4))([]).orNull()).to.be.null;
    expect(isOpt(find(eq(4))([]))).to.be.true;
    expect(find(eq(4))([1]).orNull()).to.be.null;
    expect(find(eq(4))([1, 2, 3]).orNull()).to.be.null;
    expect(find((x: Data) => x.id === 0)([itemA, itemB, itemC]).orNull()).to.be.null;
  });

  it('works with read-only array', () => {
    expect(find(eq(4))([] as readonly number[]).orNull()).to.be.null;
  });

  it('passes examples', () => {
    expect(
      find((x: number) => x > 0)([-1, 0, 1]) // Some(1)
      .orNull(),
    ).to.be.eq(1);
    expect(
      find((x: number) => x > 5)([0, 3, 5]) // None
      .orNull(),
    ).to.be.null;
  });
});

describe('narrow', () => {
  it('narrows', () => {
    const a: Opt<string> = narrow(isString)(opt(1));
    expect(a.orNull()).to.eq(null);
    const b: Opt<number> = narrow(isNumber)(opt(1));
    expect(b.orNull()).to.eq(1);
  });
});

describe('narrowOrCrash', () => {
  it('crashes on narrowing failure', () => {
    expect(() => narrowOrCrash(isString)(opt(1))).to.throw();
  });
  it('crashes with custom error', () => {
    expect(() => narrowOrCrash(isString, () => new Error('not a string error'))(opt(1))).to.throw(Error, 'not a string error');
    expect(() => narrowOrCrash(isString, 'not a string error')(opt(1))).to.throw(Error, 'not a string error');
  });
});

describe('print', () => {
  beforeEach(() => {
    sandbox.on(console, ['log'], noop);
  });

  afterEach(() => {
    sandbox.restore();
  });

  it('prints', () => {
    expect(console.log).to.have.been.called.exactly(0);
    print()(opt(1));
    expect(console.log).to.have.been.called.exactly(1);
    expect(console.log).to.have.been.called.with('Some:', 1);
    print('a')(7);
    expect(console.log).to.have.been.called.exactly(2);
    expect(console.log).to.have.been.called.with('[a]', 7);
  });
});

describe('equals', () => {
  it('pos', () => {
    expect(equals(opt(1))(opt(1))).to.be.true;
    expect(equals(none)(none)).to.be.true;
    expect(equals(opt(1), (_a, _b) => true)(opt(2))).to.be.true;
  });
  it('neg', () => {
    expect(equals(opt(1))(none)).to.be.false;
    expect(equals(none)(opt(1))).to.be.false;
    expect(equals(opt(0))(opt(1))).to.be.false;
  });
});

describe('prop', () => {
  it('pos', () => {
    const a: ObjA = {a: true};
    expect(prop<ObjA>('a')(opt(a)).orNull()).to.be.true;
    flow2((x: Opt<ObjA>) => x, prop('a'))(opt(a));
  });

  it('neg', () => {
    expect(prop<ObjA>('a')(none).orNull()).to.be.null;
    // @ts-expect-error invalid field name
    prop<ObjA>('b');
  });
});

describe('propNaked', () => {
  it('works directly', () => {
    expect(propNaked<ObjA>('a')({a: true}).orNull()).to.be.true;
    expect(propNaked<ObjC>('c')({c: null}).orNull()).to.be.null;
    expect(propNaked<ObjC>('cc')({c: null}).orNull()).to.be.null;
    expect(propNaked<ObjC>('cc')({c: null, cc: 'cc'}).orNull()).to.be.eq('cc');
    const nul: ObjA | null = null as unknown as ObjA | null;
    expect(propNaked<ObjA>('a')(nul).orNull()).to.be.null;
  });
  it('works with array', () => {
    const xs: Opt<boolean>[] = [{a: true}].map(propNaked('a'));
    expect(xs.map(orNull)).to.eql([true]);
  });
  it('works with opt', () => {
     const x: Opt<ObjA> = opt({a: true});
     expect(x.chain(propNaked('a')).orNull()).to.be.true;
     const aOpt: Opt<boolean> = x.chain(propNaked('a'));
     expect(aOpt.orNull()).to.be.true;
  });
  it(`doesn't allow getting non-existing field`, () => {
    const x: Opt<ObjA> = opt({a: true});
    // @ts-expect-error invalid field
    propNaked<ObjA>('b')(x);
    // @ts-expect-error invalid field
    propNaked('b')(x);
    // @ts-expect-error invalid field
    propNaked<ObjA | null>('b')(null);
    // @ts-expect-error invalid field (null doesn't have any fields)
    propNaked<null>('b')(null);
  });
})

describe('propOrCrash', () => {
  interface A {x?: number;}

  const aFull: A = {x: 4};
  const aEmpty: A = {};
  Object.freeze(aFull);
  Object.freeze(aEmpty);

  describe('opt', () => {
    it('pos', () => {
      const xFromA: number = propOrCrash<A>('x')(opt(aFull));
      expect(xFromA).to.be.eq(4);
      expect(pipe(opt(aFull), propOrCrash<A>('x'))).to.be.eq(4);
    });

    it('neg', () => {
      expect(() => propOrCrash<A>('x')(opt(aEmpty))).to.throw('missing x');
      expect(() => {
        // @ts-expect-error missing field
        const x = propOrCrash<A>('foo')(opt(aFull));
        suppressUnused(x);
      }).to.throw();
    });
  });

  describe('with custom error', () => {
    it('supports string message', () => {
      expect(() => propOrCrash<A>('x', 'Custom error: x is missing')(aEmpty)).to.throw('Custom error: x is missing');
    });
    it('supports function', () => {
      expect(() => propOrCrash<A>('x', key => `Custom error: ${key} is missing`)(aEmpty)).to.throw('Custom error: x is missing');
    });
    it('supports Error', () => {
      expect(() => propOrCrash<A>('x', key => new Error(`Custom error: ${key} is missing`))(aEmpty)).to.throw('Custom error: x is missing');
    });
  });

  it('works with pipe', () => {
    const getX = propOrCrash<A>('x');
    expect(pipe(opt(aFull), getX)).to.be.eq(4);
    expect(() => pipe(none, getX)).to.throw('missing x');
  });

  describe('naked object', () => {
    it('pos', () => {
      const xFromA: number = propOrCrash<A>('x')(aFull);
      expect(xFromA).to.be.eq(4);
      expect(pipe(aFull, propOrCrash<A>('x'))).to.be.eq(4);

      interface Animal {
        name?: string;
      }

      const a: Animal = {name: 'Spot'};
      expect(
        propOrCrash<Animal>('name')(a), // 'Spot'
      ).to.be.eq('Spot');
    });

    it('neg', () => {
      expect(() => propOrCrash<A>('x')(aEmpty)).to.throw('missing x');
      expect(() => {
        // @ts-expect-error missing field
        const x = propOrCrash<A>('foo')(aFull);
        suppressUnused(x);
      }).to.throw();
    });
  });
});

describe('genNakedPropOrCrash', () => {
  interface Animal {
    id: number;
    name?: string;
  }

  it('pos', () => {
    const spot: Animal = {id: 36, name: 'Spot'};
    Object.freeze(spot);
    const getSpotProp = genNakedPropOrCrash(spot);
    expect(
      getSpotProp('name'), // 'Spot'
    ).to.be.eq('Spot');
    expect(
      getSpotProp('id'), // 36
    ).to.be.eq(36);
  });

  it('neg', () => {
    const cow: Animal = {id: 36};
    Object.freeze(cow);
    const getCowProp = genNakedPropOrCrash(cow);
    expect(() => {
      getCowProp('name'); // crashes with 'missing name'
    }).to.throw('missing name');
    expect(() => {
      // @ts-expect-error invalid field
      getCowProp('nope');
    }).to.throw('missing nope');
  });

  describe('with custom error in creation', () => {
    it('supports string message', () => {
      const cow: Animal = {id: 36};
      const getCowProp = genNakedPropOrCrash(cow, key => `Custom error: ${key} is missing`);
      expect(() => getCowProp('name')).to.throw('Custom error: name is missing');
    });
  });

  describe('with custom error in usage', () => {
    it('supports string message', () => {
      const cow: Animal = {id: 36};
      const getCowProp = genNakedPropOrCrash(cow);
      expect(() => getCowProp('name', 'Custom error: name is missing')).to.throw('Custom error: name is missing');
    });
  });

  describe('with custom error in both creation and usage - prefers error from usage', () => {
    it('supports string message', () => {
      const cow: Animal = {id: 36};
      const getCowProp = genNakedPropOrCrash(cow, key => `Custom error creation: ${key} is missing`);
      expect(() => getCowProp('name', 'Custom error usage: name is missing')).to.throw('Custom error usage: name is missing');
    });
  });

  it('type inference', () => {
    const cow: Animal = {id: 36, name: 'Iris'};
    Object.freeze(cow);
    const getCowProp = genNakedPropOrCrash(cow);
    const name: string = getCowProp('name');
    // @ts-expect-error name is not number
    const shouldFailOnString: number = getCowProp('name');
    // @ts-expect-error id is not string
    const shouldFailOnNumber: string = getCowProp('id');
    suppressUnused(name, shouldFailOnString, shouldFailOnNumber);
  });
});

describe('propOrNullNaked', () => {
  interface Obj {x?: number;}

  it('returns value when present', () => {
    const obj: Obj = {x: 5};
    expect(propOrNullNaked<Obj>()('x')(obj)).to.equal(5);
  });

  it('returns null when property is missing', () => {
    const obj: Obj = {};
    expect(propOrNullNaked<Obj>()('x')(obj)).to.be.null;
  });

  it('returns null for null input', () => {
    expect(propOrNullNaked<Obj>()('x')(null)).to.be.null;
  });

  it('correctly infers types', () => {
    interface VirtualItem {
      name: string;
      durability?: number;
    }

    const elucidator: VirtualItem = { name: 'Elucidator', durability: 1000 };
    const darkRepulser: VirtualItem = { name: 'Dark Repulser' };

    const getDurability = propOrNullNaked<VirtualItem>()('durability');

    const elucidatorDurability: number | null = getDurability(elucidator);
    expect(elucidatorDurability).to.equal(1000);

    const darkRepulserDurability: number | null = getDurability(darkRepulser);
    expect(darkRepulserDurability).to.be.null;

    // @ts-expect-error 'weight' is not a property of VirtualItem
    propOrNullNaked<VirtualItem>()('weight');

    // @ts-expect-error result should be string | null, not number | null
    const itemName: number | null = propOrNullNaked<VirtualItem>()('name')(elucidator);
    suppressUnused(itemName);
  });
});

describe('propOrNull', () => {
  interface Obj {x?: number;}
  it('supports opt', () => {
    expect(propOrNull<Obj>()('x')(opt({x: 1}))).to.be.eq(1);
    expect(propOrNull<Obj>()('x')(opt(null))).to.be.null;
  });

  it('type inference', () => {
    const getX: (x: Opt<Obj>) => number | null = propOrNull<Obj>()('x');
    const x: number | null = getX(opt({x: 1}));
    // @ts-expect-error x is number | null
    const shouldFailOnString: string = getX(opt({x: 1}));
    suppressUnused(x, shouldFailOnString);
  });

  it('works with pipe', () => {
    const getX = propOrNull<Obj>()('x');
    expect(pipe(opt({x: 1}), getX)).to.be.eq(1);
    expect(pipe(none, getX)).to.be.null;
  });
});

describe('propOrUndefNaked', () => {
  interface Obj {x?: number;}

  it('returns value when present', () => {
    const obj: Obj = {x: 5};
    expect(propOrUndefNaked<Obj>()('x')(obj)).to.equal(5);
  });

  it('returns undefined when property is missing', () => {
    const obj: Obj = {};
    expect(propOrUndefNaked<Obj>()('x')(obj)).to.be.undefined;
  });

  it('returns undefined for null input', () => {
    expect(propOrUndefNaked<Obj>()('x')(null)).to.be.undefined;
  });

  it('correctly infers types', () => {
    interface Player {
      username: string;
      level?: number;
    }

    const beater: Player = { username: 'Beater', level: 96 };
    const flashy: Player = { username: 'Flashy' };

    const getLevel = propOrUndefNaked<Player>()('level');

    const beaterLevel: number | undefined = getLevel(beater);
    expect(beaterLevel).to.equal(96);

    const flashyLevel: number | undefined = getLevel(flashy);
    expect(flashyLevel).to.be.undefined;

    // @ts-expect-error 'guild' is not a property of Player
    propOrUndefNaked<Player>()('guild');

    // @ts-expect-error result should be string | undefined, not number | undefined
    const playerName: number | undefined = propOrUndefNaked<Player>()('username')(beater);
    suppressUnused(playerName);
  });
});

describe('propOrUndef', () => {
  interface Obj {x?: number;}
  it('supports opt', () => {
    expect(propOrUndef<Obj>()('x')(opt({x: 1}))).to.be.eq(1);
    expect(propOrUndef<Obj>()('x')(opt(null))).to.be.undefined;
  });

  it('type inference', () => {
    const getX: (x: Opt<Obj>) => number | undefined = propOrUndef<Obj>()('x');
    const x: number | undefined = getX(opt({x: 1}));
    // @ts-expect-error x is number | undefined
    const shouldFailOnString: string = getX(opt({x: 1}));
    suppressUnused(x, shouldFailOnString);
  });
});

describe('propOrZeroNaked', () => {
  interface Obj {x?: number;}

  it('returns value when present', () => {
    const obj: Obj = {x: 5};
    expect(propOrZeroNaked<Obj>()('x')(obj)).to.equal(5);
  });

  it('returns 0 when property is missing', () => {
    const obj: Obj = {};
    expect(propOrZeroNaked<Obj>()('x')(obj)).to.equal(0);
  });

  it('returns 0 for null input', () => {
    expect(propOrZeroNaked<Obj>()('x')(null)).to.equal(0);
  });

  it('correctly infers types', () => {
    interface Skill {
      name: string;
      cooldown?: number;
    }

    const vorpalStrike: Skill = { name: 'Vorpal Strike', cooldown: 5 };
    const horizontalSquare: Skill = { name: 'Horizontal Square' };

    const getCooldown = propOrZeroNaked<Skill>()('cooldown');

    const vorpalStrikeCooldown: number = getCooldown(vorpalStrike);
    expect(vorpalStrikeCooldown).to.equal(5);

    const horizontalSquareCooldown: number = getCooldown(horizontalSquare);
    expect(horizontalSquareCooldown).to.equal(0);

    // @ts-expect-error 'damage' is not a property of Skill
    propOrZeroNaked<Skill>()('damage');

    // @ts-expect-error result should be number, not string
    const skillName: string = propOrZeroNaked<Skill>('name')(vorpalStrike);
    suppressUnused(skillName);
  });
});

describe('propOrZero', () => {
  interface Obj {x?: number;}
  it('supports opt', () => {
    expect(propOrZero<Obj>()('x')(opt({x: 1}))).to.be.eq(1);
    expect(propOrZero<Obj>()('x')(none)).to.be.eq(0);
  });

  it('type inference', () => {
    const getX: (x: Opt<Obj>) => number = propOrZero<Obj>()('x');
    const x: number = getX(opt({x: 1}));
    // @ts-expect-error x is number
    const shouldFailOnString: string = getX(opt({x: 1}));
    suppressUnused(x, shouldFailOnString);
  });
});

describe('genNakedPropGetters', () => {
  interface TestObj {
    id: number;
    name?: string;
    age?: number;
  }

  const obj: TestObj = { id: 1, name: 'Test', age: 30 };
  const emptyObj: TestObj = { id: 1 };

  it('generates correct getters', () => {
    const get = genNakedPropGetters(obj);

    expect(get.orCrash('id')).to.equal(1);
    expect(get.orNull('name')).to.equal('Test');
    expect(get.orUndef('age')).to.equal(30);
    expect(get.orZero('age')).to.equal(30);
    expect(get.prop('id').orCrash('missing id')).to.be.eq(1);
  });

  it('handles missing properties correctly', () => {
    const get = genNakedPropGetters(emptyObj);

    expect(get.orCrash('id')).to.equal(1);
    expect(get.orNull('name')).to.be.null;
    expect(get.orUndef('age')).to.be.undefined;
    expect(get.orZero('age')).to.equal(0);
  });

  it('crashes when using orCrash on missing property', () => {
    const get = genNakedPropGetters(emptyObj);
    expect(() => get.orCrash('name')).to.throw('missing name');
  });

  it('orCrash uses custom error factory when provided', () => {
    const get = genNakedPropGetters(emptyObj, key => `Custom error: ${key} is missing`);
    expect(() => get.orCrash('name')).to.throw('Custom error: name is missing');
    expect(() => get.orCrash('name', 'nope, no name')).to.throw('nope, no name');
    expect(() => get.orCrash('name', key => `nope, no ${key}`)).to.throw('nope, no name');
    expect(() => get.orCrash('name', key => new Error(`nope, no ${key}`))).to.throw(Error, 'nope, no name');
  });

  it('type inference', () => {
    interface Animal {
      id?: number;
      name?: string;
    }

    const cow: Animal = {id: 36, name: 'Iris'};
    Object.freeze(cow);
    const getters = genNakedPropGetters(cow);
    const nameOrCrashViaGetters: string = getters.orCrash('name');
    const nameOrNullViaGetters: string | null = getters.orNull('name');
    const nameOrUndefViaGetters: string | undefined = getters.orUndef('name');
    const nameOrZeroViaGetters: string | 0 = getters.orZero('name');
    const idOrZeroViaGetters: number | 0 = getters.orZero('id');
    const idPropViaGetters: Opt<number> = getters.prop('id');
    suppressUnused(nameOrCrashViaGetters, nameOrNullViaGetters, nameOrUndefViaGetters, nameOrZeroViaGetters, idOrZeroViaGetters, idPropViaGetters);

    const {orCrash, orNull, orUndef, orZero, prop: rawProp} = genNakedPropGetters(cow);
    const nameOrCrash: string = orCrash('name');
    const nameOrNull: string | null = orNull('name');
    const nameOrUndef: string | undefined = orUndef('name');
    const nameOrZero: string | 0 = orZero('name');
    const idOrZero: number | 0 = orZero('id');
    const idProp: Opt<number> = rawProp('id');
    suppressUnused(nameOrCrash, nameOrNull, nameOrUndef, nameOrZero, idOrZero, idProp);

    // @ts-expect-error name is not number
    const shouldFailOnStringWithOrCrash: number = orCrash('name');
    // @ts-expect-error name is not number
    const shouldFailOnStringWithOrNull: number | null = orNull('name');
    // @ts-expect-error name is not number
    const shouldFailOnStringWithOrUndef: number | undefined = orUndef('name');
    // @ts-expect-error name is not number
    const shouldFailOnStringWithOrZero: number | 0 = orZero('name');
    // @ts-expect-error name is not number
    const shouldFailOnStringWithProp: Opt<number> = rawProp('name');
    suppressUnused(shouldFailOnStringWithOrCrash, shouldFailOnStringWithOrNull, shouldFailOnStringWithOrUndef, shouldFailOnStringWithOrZero, shouldFailOnStringWithProp);

    // @ts-expect-error id is not string
    const shouldFailOnNumberWithOrCrash: string = orCrash('id');
    // @ts-expect-error id is not string
    const shouldFailOnNumberWithOrNull: string | null = orNull('id');
    // @ts-expect-error id is not string
    const shouldFailOnNumberWithOrUndef: string | undefined = orUndef('id');
    // @ts-expect-error id is not string
    const shouldFailOnNumberWithOrZero: string | 0 = orZero('id');
    // @ts-expect-error id is not string
    const shouldFailOnNumberWithProp: Opt<string> = rawProp('id');
    suppressUnused(shouldFailOnNumberWithOrCrash, shouldFailOnNumberWithOrNull, shouldFailOnNumberWithOrUndef, shouldFailOnNumberWithOrZero, shouldFailOnNumberWithProp);

    // @ts-expect-error null is not undefined
    const shouldFailOnStringOrUndefinedWithOrNull: string | undefined = orNull('name');
    suppressUnused(shouldFailOnStringOrUndefinedWithOrNull);
  });
});

describe('swap', () => {
  it('swaps', () => {
    expect(swap(true)(opt(1)).orNull()).to.be.eq(true);
    expect(swap(true)(none).orNull()).to.be.null;
    const a: Opt<string> = swap('')(opt(1));
    const b: Opt<string> = swap('')(none);
    suppressUnused(a, b);
  });
});

describe('flow', () => {
  it('one function', () => {
    expect(flow(id)(1)).to.be.eq(1);
  });
  it('more complex', () => {
    expect(flow((x: number) => x, x => -x, id)(1)).to.be.eq(-1);
    expect(flow(id, x => -x, id)(1)).to.be.eq(-1);
    expect(flow(inc, id, x => x * x)(1)).to.be.eq(4);
  });
  it('examples', () => {
    expect(
      flow( // 63
        inc, // 64
        Math.sqrt, // 8
      )(63), // 8
    ).to.be.eq(8);
    expect(
      Math.sqrt(inc(63)),
    ).to.be.eq(8);
    const f = flow(inc, Math.sqrt); // (_: number) => number
    expect(
      f(63), // 8
    ).to.be.eq(8);
    expect(
      f(3),  // 2
    ).to.be.eq(2);
  });
});

describe('compose', () => {
  it('one function', () => {
    expect(compose(id)(1)).to.be.eq(1);
  });
  it('more complex', () => {
    expect(compose((x: number) => x * 7, inc)(2)).to.be.eq(21);
    expect(compose((x: number) => x * 7, id, inc)(2)).to.be.eq(21);
    expect(compose(x => x.length, (x: number) => x.toFixed(0))(678)).to.be.eq(3);
    expect(compose(id, x => x.length, (x: number) => x.toFixed(0))(678)).to.be.eq(3);
  });
  it('example', () => {
    const f = (x: number): number => x * x;
    const g = (x: number): string => x.toFixed();
    const h = (x: string): boolean => x === '4';
    expect(
      compose(
        h, // true
        g, // 4
        f, // 2
      )(2), // true
    ).to.be.true;
    expect(h(g(f(2)))).to.be.true;
  });
});

describe('(un)curry', () => {
  it('curryTuple', () => {
    const addPair = ([a, b]: [number, number]) => a + b;
    const f: (_: number) => (_: number) => number = curryTuple(addPair);
    expect(f(80)(8)).to.be.eq(88);
    expect(
      opt(1) // Some(1)
      .map(
        curryTuple(addPair)(4), // same as `(a => b => a + b)(4)`
      ) // Some(5)
      .orNull(),
    ).to.be.eq(5);
  });

  it('curryTuple3', () => {
    const f: (_: number) => (_: number) => (_: boolean) => number =
      curryTuple3(([a, b, c]: [number, number, boolean]) => c ? a - b : 0);
    expect(f(1)(2)(true)).to.be.eq(-1);
    expect(f(1)(2)(false)).to.be.eq(0);
  });

  it('curryTuple4', () => {
    const f: (_: number) => (_: number) => (_: boolean) => (_: number) => number =
      curryTuple4(([a, b, c, d]: [number, number, boolean, number]) => c ? a - b : d);
    expect(f(1)(2)(true)(7)).to.be.eq(-1);
    expect(f(1)(2)(false)(7)).to.be.eq(7);
  });

  it('curryTuple5', () => {
    const f: (_: number) => (_: number) => (_: boolean) => (_: number) => (_: number) => number =
      curryTuple5(([a, b, c, d, e]: [number, number, boolean, number, number]) => c ? a - b : d * e);
    expect(f(1)(2)(true)(7)(2)).to.be.eq(-1);
    expect(f(1)(2)(false)(7)(2)).to.be.eq(14);
  });

  it('uncurryTuple', () => {
    const f: (_: [number, number]) => number = uncurryTuple((a: number) => (b: number) => a - b);
    expect(f([4, 1])).to.be.eq(3);
    const sub = (x: number) => (y: number) => x - y;
    expect(
      opt(4) // Some(4)
      .zip(opt(1)) // Some([4, 1])
      .map(uncurryTuple(sub)) // Some(3)
      .orNull(),
    ).to.be.eq(3);
  });

  it('uncurryTuple3', () => {
    const f: (_: [number, number, boolean]) => number =
      uncurryTuple3(a => b => c => c ? a - b : 0);
    expect(opt(1).zip3(opt(2), opt(true)).map(f).orNull()).to.be.eq(-1);
    expect(opt(1).zip3(opt(2), opt(false)).map(f).orNull()).to.be.eq(0);
  });

  it('uncurryTuple4', () => {
    const f: (_: [number, number, boolean, number]) => number =
      uncurryTuple4(a => b => c => d => c ? a - b : d);
    expect(opt(1).zip4(opt(2), opt(true), opt(7)).map(f).orNull()).to.be.eq(-1);
    expect(opt(1).zip4(opt(2), opt(false), opt(7)).map(f).orNull()).to.be.eq(7);
  });

  it('uncurryTuple5', () => {
    const f: (_: [number, number, boolean, number, number]) => number =
      uncurryTuple5(a => b => c => d => e => c ? a - b : d * e);
    expect(opt(1).zip5(opt(2), opt(true), opt(7), opt(2)).map(f).orNull()).to.be.eq(-1);
    expect(opt(1).zip5(opt(2), opt(false), opt(7), opt(2)).map(f).orNull()).to.be.eq(14);
  });
});

describe('id', () => {
  it('returns same value', () => {
    expect(id(1)).to.be.eq(1);
    expect(id(null)).to.be.eq(null);
  });
});

describe('at', () => {
  it('gets item from array', () => {
    expect(at(0)([5]).orFalse()).to.be.eq(5);
    expect(at(1)([5]).orFalse()).to.be.false;
    expect(at(0)(opt([5])).orFalse()).to.be.eq(5);
    expect(at(1)(opt([5])).orFalse()).to.be.false;
    const x: number | false = at(0)([1] as (number | null)[]).orElseAny(false);
    const y: Opt<number> = at(0)([1] as (number | null)[]);
    suppressUnused(x, y);
  });

  it('supports empty input values', () => {
    const input1: number[] | null = [] as number[] | null;
    const res1: Opt<number> = at(0)(input1);
    expect(res1.orFalse()).to.be.false;
  });

  it('works with strings', () => {
    const res1: Opt<string> = at(0)('Blender');
    expect(res1.orNull()).to.be.eq('B');
    const res2: Opt<string> = at(0)('');
    expect(res2.orNull()).to.be.null;
    const res3: Opt<string> = at(0)(undefined);
    expect(res3.orNull()).to.be.null;
  });

  it('works with readonly array', () => {
    const xs = [4] as readonly number[];
    expect(at(0)(xs).orNull()).to.be.eq(4);
  });

  it('works well with pipe/flow', () => {
    const res1: Opt<number> = pipe([20, 15], id, at(1), id);
    expect(res1.orNull()).to.be.eq(15);
    const f = flow(id, at(1), id, orNull);
    expect(f([20, 15])).to.be.eq(15);
    expect(f('2015')).to.be.eq('0');
    expect(f(undefined)).to.be.null;
  });
});

describe('head', () => {
  it('returns first element of array', () => {
    expect(head([1, 2, 3]).orFalse()).to.be.eq(1);
    expect(head([]).orFalse()).to.be.false;
    expect(head(null).orFalse()).to.be.false;
  });

  it('works with read-only arrays', () => {
    expect(head([1, 2, 3] as readonly number[]).orFalse()).to.be.eq(1);
  });

  it('supports empty input values', () => {
    const input1: number[] | null = [] as number[] | null;
    const res1: Opt<number> = head(input1);
    expect(res1.orFalse()).to.be.false;
    const res2: Opt<number> = head([1] as number[] | null);
    expect(res2.orFalse()).to.be.eq(1);
  });

  it('returns first character of string', () => {
    const res1: Opt<string> = head('Palico');
    expect(res1.orNull()).to.be.eq('P');
    const res2: Opt<string> = head('');
    expect(res2.orNull()).to.be.null;
    const res3: Opt<string> = head(null as string | null);
    expect(res3.orNull()).to.be.null;
  });

  it('works well with pipe/flow', () => {
    const res1: Opt<number> = pipe([1, 2, 3], id, head, id);
    expect(res1.orNull()).to.be.eq(1);
    const res2: Opt<string> = pipe('Yami', id, head, id);
    expect(res2.orNull()).to.be.eq('Y');
    const f: (_: number[] | null) => Opt<number> = flow(id, head, id);
    const res3: Opt<number> = f([1]);
    expect(res3.orNull()).to.be.eq(1);
    expect(f([]).orNull()).to.be.null;
    const g: (_: string | null) => Opt<string> = flow(id, head, id);
    expect(g('Noelle').orNull()).to.be.eq('N');
    expect(g('').orNull()).to.be.null;
    expect(g(null).orFalse()).to.be.false;
    const h = flow(id, head, id);
    expect(h([0]).orNull()).to.be.eq(0);
    expect(h('Yuno').orNull()).to.be.eq('Y');
  });
});

describe('headIn', () => {
  it('returns first element of array', () => {
    expect(headIn(opt([1, 2, 3])).orFalse()).to.be.eq(1);
    expect(headIn(opt([])).orFalse()).to.be.false;
    expect(headIn(opt(null as null | number[])).orFalse()).to.be.false;
  });

  it('works with read-only arrays', () => {
    expect(headIn(opt([1, 2, 3] as readonly number[])).orFalse()).to.be.eq(1);
  });

  it('returns first character of string', () => {
    const res1: Opt<string> = headIn(opt('Palico'));
    expect(res1.orNull()).to.be.eq('P');
    const res2: Opt<string> = headIn(opt(''));
    expect(res2.orNull()).to.be.null;
    const res3: Opt<string> = headIn(opt(null as string | null));
    expect(res3.orNull()).to.be.null;
  });

  it('works well with pipe/flow', () => {
    const res1: Opt<number> = pipe([1, 2, 3], opt, headIn, id);
    expect(res1.orNull()).to.be.eq(1);
    const res2: Opt<string> = pipe('Yami', opt, headIn, id);
    expect(res2.orNull()).to.be.eq('Y');
    const f: (_: number[] | null) => Opt<number> = flow(opt, headIn, id);
    const res3: Opt<number> = f([1]);
    expect(res3.orNull()).to.be.eq(1);
    expect(f([]).orNull()).to.be.null;
    const g: (_: string | null) => Opt<string> = flow(opt, headIn, id);
    expect(g('Noelle').orNull()).to.be.eq('N');
    expect(g('').orNull()).to.be.null;
    expect(g(null).orFalse()).to.be.false;
    const h = flow(opt, headIn, id);
    expect(h([0]).orNull()).to.be.eq(0);
    expect(h('Yuno').orNull()).to.be.eq('Y');
  });
});

describe('last', () => {
  it('returns last element of array', () => {
    expect(last([1, 2, 3]).orFalse()).to.be.eq(3);
    expect(last([]).orFalse()).to.be.false;
    expect(last(undefined).orFalse()).to.be.false;
  });

  it('works with read-only arrays', () => {
    expect(last([1, 2, 3] as readonly number[]).orFalse()).to.be.eq(3);
  });

  it('supports empty input values', () => {
    const input1: number[] | null = [] as number[] | null;
    const res1: Opt<number> = last(input1);
    expect(res1.orFalse()).to.be.false;
  });

  it('returns last element of string', () => {
    const res1: Opt<string> = last('Palico');
    expect(res1.orFalse()).to.be.eq('o');
    expect(last('').orFalse()).to.be.false;
  });

  it('works well with pipe/flow', () => {
    const res1: number | null = pipe([20, 15], last, orNull);
    expect(res1).to.be.eq(15);
    expect(pipe([], last, orNull)).to.be.null;
    expect(pipe('2015', last, orNull)).to.be.eq('5');
    expect(pipe('', last, orNull)).to.be.null;
  });
});

describe('lastIn', () => {
  it('returns last element of array', () => {
    expect(lastIn(opt([1, 2, 3])).orFalse()).to.be.eq(3);
    expect(lastIn(opt([])).orFalse()).to.be.false;
    expect(lastIn(opt(null as null | number[])).orFalse()).to.be.false;
  });

  it('works with read-only arrays', () => {
    expect(lastIn(opt([1, 2, 3] as readonly number[])).orFalse()).to.be.eq(3);
  });

  it('returns last element of string', () => {
    const res1: Opt<string> = lastIn(opt('Palico'));
    expect(res1.orFalse()).to.be.eq('o');
    expect(lastIn(opt('')).orFalse()).to.be.false;
  });

  it('works well with pipe/flow', () => {
    const res1: number | null = pipe([20, 15], opt, lastIn, orNull);
    expect(res1).to.be.eq(15);
    expect(pipe([], opt, lastIn, orNull)).to.be.null;
    expect(pipe('2015', opt, lastIn, orNull)).to.be.eq('5');
    expect(pipe('', opt, lastIn, orNull)).to.be.null;
  });
});

describe('zipToOptArray', () => {
  it('type checks', () => {
    const a: Opt<[1, 2]> = zipToOptArray([1 as 1 | null, 2 as 2 | undefined]);
    const b: Opt<[1, 2, 3]> = zipToOptArray([1 as 1 | null, 2 as 2 | undefined, 3 as 3 | null]);
    const c: Opt<[1, 2, 3, 4]> = zipToOptArray([1 as 1 | null, 2 as 2 | undefined, 3 as 3 | null, 4 as 4 | undefined]);
    const d: Opt<[1, 2, 3, 4, 5]> = zipToOptArray([1 as 1 | null, 2 as 2 | undefined, 3 as 3 | null, 4 as 4 | undefined, 5 as 5 | null]);
    suppressUnused(a, b, c, d);
  });

  it('returns correct result', () => {
    const a1: Opt<[1, number]> = zipToOptArray([1 as const, null as number | null]);
    expect(a1.orNull()).to.be.null;
    const a2: [1, number] | null = zipToOptArray([1 as const, 2]).orNull();
    expect(a2).to.be.eql([1, 2]);
    expect(zipToOptArray([1, null, '']).orNull()).to.be.null;
    expect(zipToOptArray([1, true, '']).orNull()).to.be.eql([1, true, '']);
    expect(zipToOptArray([1, null, '', 7]).orNull()).to.be.null;
    expect(zipToOptArray([1, true, '', 7]).orNull()).to.be.eql([1, true, '', 7]);
    expect(zipToOptArray([1, null, '', 7, false]).orNull()).to.be.null;
    expect(zipToOptArray([1, true, '', 7, false]).orNull()).to.be.eql([1, true, '', 7, false]);
  });

  it('works with readonly arrays', () => {
    expect(zipToOptArray([1, null] as readonly [number | undefined, string | null]).orNull()).to.be.null;
  });
});

describe('testRe', () => {
  it('pos', () => {
    expect(testRe(/a/)('a')).to.be.true;
    expect(testRe(/a/)('bac')).to.be.true;
  });
  it('neg', () => {
    expect(testRe(/a/)('')).to.be.false;
    expect(testRe(/a/)('xxx')).to.be.false;
    expect(testRe(/a/)('A')).to.be.false;
  });
  it('use', () => {
    expect(opt('abc').map(testRe(/b/)).orFalse()).to.be.true;
  });
});

describe('testReOrFalse', () => {
  it('pos', () => {
    expect(testReOrFalse(/\d/)(opt('7'))).to.be.true;
  });
  it('neg', () => {
    expect(testReOrFalse(/\d/)(opt('x'))).to.be.false;
    expect(testReOrFalse(/\d/)(opt(null))).to.be.false;
    expect(testReOrFalse(/\d/)(opt(undefined))).to.be.false;
    expect(testReOrFalse(/\d/)(opt(''))).to.be.false;
  });
  it('use', () => {
    expect(pipe('7', opt, testReOrFalse(/\d/))).to.be.true;
    expect(pipe(opt(4), map(toString), testReOrFalse(/\d/))).to.be.true;
    expect(pipe('x', opt, testReOrFalse(/\d/))).to.be.false;
  });
});

describe('toString', () => {
  it('calls toString method', () => {
    expect(toString(4)).to.be.eq('4');
    expect(isString(toString(new Date(0)))).to.be.true;
    expect(toString({})).to.be.eq('[object Object]');
    expect(toString({toString(): string { return '!'; }})).to.be.eq('!');
  });
});

describe('isString', () => {
  it('pos', () => {
    expect(isString('')).to.be.true;
    expect(isString('x')).to.be.true;
    expect(isString('Yoruichi')).to.be.true;
  });
  it('neg', () => {
    expect(isString(undefined)).to.be.false;
    expect(isString(null)).to.be.false;
    expect(isString(0)).to.be.false;
    expect(isString(false)).to.be.false;
    expect(isString(NaN)).to.be.false;
  });
});

describe('isArray', () => {
  it('pos', () => {
    expect(isArray([])).to.be.true;
    expect(isArray(['x'])).to.be.true;
    expect(isArray([0])).to.be.true;
    expect(isArray(['Kon'])).to.be.true;
  });
  it('neg', () => {
    expect(isArray(undefined)).to.be.false;
    expect(isArray({})).to.be.false;
    expect(isArray(0)).to.be.false;
    expect(isArray(NaN)).to.be.false;
    expect(isArray(false)).to.be.false;
    expect(isArray('')).to.be.false;
  });
});

describe('isReadonlyArray', () => {
  it('pos', () => {
    expect(isReadonlyArray([])).to.be.true;
    expect(isReadonlyArray([null])).to.be.true;
    expect(isReadonlyArray([1])).to.be.true;
    expect(isReadonlyArray(['x'])).to.be.true;
  });
  it('narrows', () => {
    const a: unknown = [] as unknown;
    expect(isReadonlyArray(a)).to.be.true;
    if (isReadonlyArray(a)) {
      const b: readonly unknown[] = a;
      suppressUnused(b);
    }
  });
  it('neg', () => {
    expect(isReadonlyArray('')).to.be.false;
    expect(isReadonlyArray(undefined)).to.be.false;
    expect(isReadonlyArray(null)).to.be.false;
    expect(isReadonlyArray(0)).to.be.false;
    expect(isReadonlyArray(NaN)).to.be.false;
    expect(isReadonlyArray(false)).to.be.false;
  });
});

describe('isFunction', () => {
  it('pos', () => {
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    expect(isFunction(() => {})).to.be.true;
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    expect(isFunction(function () {})).to.be.true;
  });
  it('neg', () => {
    expect(isFunction(undefined)).to.be.false;
    expect(isFunction(null)).to.be.false;
    expect(isFunction(0)).to.be.false;
    expect(isFunction(NaN)).to.be.false;
    expect(isFunction(false)).to.be.false;
  })
});

describe('isObject', () => {
  it('pos', () => {
    expect(isObject({})).to.be.true;
    expect(isObject({ a: 1 })).to.be.true;
    class A {}
    expect(isObject(new A())).to.be.true;
  });
  it('neg', () => {
    expect(isObject(undefined)).to.be.false;
    expect(isObject(null)).to.be.false;
    expect(isObject(0)).to.be.false;
    expect(isObject(NaN)).to.be.false;
    expect(isObject(false)).to.be.false;
  });
});

describe('isNumber', () => {
  it('pos', () => {
    expect(isNumber(0)).to.be.true;
    expect(isNumber(1)).to.be.true;
    expect(isNumber(1.1)).to.be.true;
    expect(isNumber(NaN)).to.be.true;
  });
  it('neg', () => {
    expect(isNumber(undefined)).to.be.false;
    expect(isNumber(null)).to.be.false;
    expect(isNumber({})).to.be.false;
    expect(isNumber([])).to.be.false;
    expect(isNumber(false)).to.be.false;
  })
})

describe('tryRun', () => {
  it('runs ok', () => {
    expect(tryRun(() => 1).orNull()).to.eql(1);
    expect(tryRun(() => null).orNull()).to.be.null;
  });
  it('catches throw', () => {
    expect(tryRun(() => { throw new Error(''); }).orNull()).to.be.null;
    expect(tryRun(() => { JSON.parse('?'); }).orNull()).to.be.null;
  });
  it('example', () => {
    expect(
      tryRun(() => 1) // Some(1)
      .orNull(),
    ).to.be.eq(1);
    expect(
      tryRun(() => { throw new Error(); }) // None
      .orNull(),
    ).to.be.null;
  });
});

describe('parseJson', () => {
  it('parses valid JSON', () => {
    expect(parseJson('{"a": 1}').orNull()).to.be.eql({a: 1});
    expect(parseJson('null').orNull()).to.be.null;
  });
  it('returns none on invalid JSON', () => {
    expect(parseJson('{neko}').orNull()).to.be.null;
    expect(parseJson('Ryoka').orNull()).to.be.null;
  });
});

describe('parseInt', () => {
  it('parses valid integer (according to JS semantics)', () => {
    expect(parseInt('0').orNull()).to.be.eq(0);
    expect(parseInt('-44').orNull()).to.be.eq(-44);
    expect(parseInt('1.1').orNull()).to.be.eq(1);
  });
  it('returns none on invalid integer', () => {
    expect(parseInt('').orNull()).to.be.null;
    expect(parseInt('gin').orNull()).to.be.null;
    expect(parseInt('xFF').orNull()).to.be.null;
  });
});

describe('parseFloat', () => {
  it('parses valid float', () => {
    expect(parseFloat('0').orNull()).to.be.eq(0);
    expect(parseFloat('0.0').orNull()).to.be.eq(0);
    expect(parseFloat('-1.2').orNull()).to.be.eq(-1.2);
  });
  it('returns none on invalid float', () => {
    expect(parseFloat('').orNull()).to.be.null;
    expect(parseFloat('gin').orNull()).to.be.null;
    expect(parseFloat('xFF').orNull()).to.be.null;
  });
});

describe('apply', () => {
  it('applies', () => {
    expect(apply(1)(opt(inc)).orNull()).to.be.eq(2);
    expect(pipe(
      inc,
      opt,
      apply(1),
      orNull,
    )).to.be.eq(2);
  });
});

describe('onFunc', () => {
  it('calls function', () => {
    const f = chai.spy((a: number) => { suppressUnused(a); });
    expect(onFunc(7)(opt(f)).orNull()).to.be.eq(f);
    expect(f).to.have.been.called.once;
    expect(f).to.have.been.called.with(7);
    expect(pipe(
      f,
      opt,
      onFunc(7),
      orNull,
    )).to.be.eq(f);
  });
});

describe('isOrCrash', () => {
  it('narrows', () => {
    const a = isOrCrash(isNumber)(4 as unknown);
    expect(a).to.be.eq(4);
    const b: number = a;
    suppressUnused(b);
  });
  it('crashes', () => {
    expect(() => isOrCrash(isNumber)(null)).to.throw('invalid value');
  });
  it('narrows nullable', () => {
    const isNumberOrNull = (x: unknown): x is number | null => isNumber(x) || x === null;
    const a: number | null = isOrCrash(isNumberOrNull)(null);
    expect(a).to.be.null;
  });
  it('custom message', () => {
    expect(() => isOrCrash(isNumber, 'nope')(null)).to.throw('nope');
  });
  it('throws custom error', () => {
    expect(() => isOrCrash(isNumber, () => new Error('custom error'))(null)).to.throw('custom error');
  })
});

describe('assertType', () => {
  it('doesn\'t crash on valid assertion', () => {
    const a: unknown = 1 as unknown;
    assertType(a, isNumber);
    const b: number = a;
    suppressUnused(b);
  });
  it('throws on wrong assertion', () => {
    const a: unknown = '' as unknown;
    expect(() => assertType(a, isNumber)).to.throw('invalid value');
  });
  it('custom error message', () => {
    const a: unknown = '' as unknown;
    const errMsg = 'not a number';
    expect(() => assertType(a, isNumber, errMsg)).to.throw(errMsg);
  });

  it('custom error factory', () => {
    const a: unknown = '' as unknown;
    expect(() => assertType(a, isNumber, () => new Error('not a number'))).to.throw('not a number');
  });
});

describe('min', () => {
  it('returns minimum of array', () => {
    const res: Opt<number> = min([1, 4]);
    expect(res.orNull()).to.be.eq(1);
  });
  it('returns minimum of read-only array', () => {
    const xs: readonly number[] = [1, 4];
    const res: Opt<number> = min(xs);
    expect(res.orNull()).to.be.eq(1);
  });
});

describe('minIn', () => {
  it('returns minimum of wrapped array', () => {
    const res: Opt<number> = minIn(opt([1, 4]));
    expect(res.orNull()).to.be.eq(1);
  });
  it('returns minimum of wrapped read-only array', () => {
    const xs: readonly number[] = [1, 4];
    const res: Opt<number> = minIn(opt(xs));
    expect(res.orNull()).to.be.eq(1);
  });
});

describe('max', () => {
  it('returns maximum of array', () => {
    const res: Opt<number> = max([1, 4]);
    expect(res.orNull()).to.be.eq(4);
  });
  it('returns maximum of read-only array', () => {
    const xs: readonly number[] = [1, 4];
    const res: Opt<number> = max(xs);
    expect(res.orNull()).to.be.eq(4);
  });
});

describe('maxIn', () => {
  it('returns maximum of wrapped array', () => {
    const res: Opt<number> = maxIn(opt([1, 4]));
    expect(res.orNull()).to.be.eq(4);
  });
  it('returns maximum of wrapped read-only array', () => {
    const xs: readonly number[] = [1, 4];
    const res: Opt<number> = maxIn(opt(xs));
    expect(res.orNull()).to.be.eq(4);
  });
});

describe('min2Num', () => {
  it('returns minimum of two numbers', () => {
    expect(min2Num(1)(2)).to.be.eq(1);
    expect(min2Num(2)(1)).to.be.eq(1);
    expect(min2Num(-2)(1)).to.be.eq(-2);
    expect(min2Num(0)(0)).to.be.eq(0);
    expect(opt(4).map(min2Num(2)).orNull()).to.be.eq(2);
  });
});

describe('min2All', () => {
  it('returns minimum of two numbers', () => {
    expect(min2All(1)(2).orNull()).to.be.eq(1);
    expect(min2All(2)(1).orNull()).to.be.eq(1);
  });
  it('returns none when any input is empty value', () => {
    expect(min2All(1)(undefined).orNull()).to.be.null;
    expect(min2All(1)(null).orNull()).to.be.null;
    expect(min2All(1)(NaN).orNull()).to.be.null;
    expect(min2All(undefined)(1).orNull()).to.be.null;
    expect(min2All(null)(1).orNull()).to.be.null;
    expect(min2All(NaN)(1).orNull()).to.be.null;
    expect(min2All(undefined)(undefined).orNull()).to.be.null;
    expect(min2All(null)(null).orNull()).to.be.null;
    expect(min2All(NaN)(NaN).orNull()).to.be.null;
  });
});

describe('min2Any', () => {
  it('returns minimum of two numbers', () => {
    expect(min2Any(1)(2).orNull()).to.be.eq(1);
    expect(min2Any(2)(1).orNull()).to.be.eq(1);
  });
  it('returns non-empty value when other input is empty', () => {
    expect(min2Any(1)(null).orNull()).to.be.eq(1);
    expect(min2Any(undefined)(1).orNull()).to.be.eq(1);
  });
  it('returns none when both inputs are empty values', () => {
    expect(min2Any(undefined)(NaN).orNull()).to.be.null;
    expect(min2Any(null)(undefined).orNull()).to.be.null;
  });
});

describe('max2Num', () => {
  it('returns maximum of two numbers', () => {
    expect(max2Num(1)(2)).to.be.eq(2);
    expect(max2Num(2)(1)).to.be.eq(2);
    expect(max2Num(-2)(1)).to.be.eq(1);
    expect(max2Num(0)(0)).to.be.eq(0);
    expect(opt(4).map(max2Num(10)).orNull()).to.be.eq(10);
  });
});

describe('max2All', () => {
  it('returns maximum of two numbers', () => {
    expect(max2All(1)(2).orNull()).to.be.eq(2);
    expect(max2All(2)(1).orNull()).to.be.eq(2);
  });
  it('returns none when any input is empty value', () => {
    expect(max2All(1)(undefined).orNull()).to.be.null;
    expect(max2All(1)(null).orNull()).to.be.null;
    expect(max2All(1)(NaN).orNull()).to.be.null;
    expect(max2All(undefined)(1).orNull()).to.be.null;
    expect(max2All(null)(1).orNull()).to.be.null;
    expect(max2All(NaN)(1).orNull()).to.be.null;
    expect(max2All(undefined)(undefined).orNull()).to.be.null;
    expect(max2All(null)(null).orNull()).to.be.null;
    expect(max2All(NaN)(NaN).orNull()).to.be.null;
  });
});

describe('max2Any', () => {
  it('returns maximum of two numbers', () => {
    expect(max2Any(1)(2).orNull()).to.be.eq(2);
    expect(max2Any(2)(1).orNull()).to.be.eq(2);
  });
  it('returns non-empty value when other input is empty', () => {
    expect(max2Any(1)(null).orNull()).to.be.eq(1);
    expect(max2Any(undefined)(1).orNull()).to.be.eq(1);
  });
  it('returns none when both inputs are empty values', () => {
    expect(max2Any(undefined)(NaN).orNull()).to.be.null;
    expect(max2Any(null)(undefined).orNull()).to.be.null;
  });
});

describe('clamp', () => {
  it('clamps number to given interval', () => {
    expect(clamp(0)(10)(5).orNull()).to.be.eq(5);
    expect(clamp(0)(10)(-4).orNull()).to.be.eq(0);
    expect(clamp(0)(10)(12).orNull()).to.be.eq(10);
  });
  it('clamps number if one side of interval is missing', () => {
    expect(clamp(0)(undefined)(5).orNull()).to.be.eq(5);
    expect(clamp(0)(null)(-1).orNull()).to.be.eq(0);
    expect(clamp(NaN)(10)(5).orNull()).to.be.eq(5);
    expect(clamp(undefined)(10)(12).orNull()).to.be.eq(10);
  });
  it('noop when both sides of interval are missing', () => {
    expect(clamp(undefined)(undefined)(5).orNull()).to.be.eq(5);
    expect(clamp(null)(null)(5).orNull()).to.be.eq(5);
    expect(clamp(NaN)(NaN)(5).orNull()).to.be.eq(5);
  });
  it('returns none when value to clamp is an empty value', () => {
    expect(clamp(0)(1)(undefined).orNull()).to.be.null;
    expect(clamp(0)(1)(null).orUndef()).to.be.undefined;
    expect(clamp(0)(1)(NaN).orNull()).to.be.null;
  });
  it('works with opt', () => {
    expect(opt(7).chain(clamp(0)(10)).orNull()).to.be.eq(7);
    expect(opt(-1).chain(clamp(0)(10)).orNull()).to.be.eq(0);
    expect(opt(12).chain(clamp(0)(10)).orNull()).to.be.eq(10);
    expect(opt(5).act(clamp(0)(7), clamp(1)(3)).orNull()).to.be.eq(3);
    expect(opt(5).act(clamp(1)(3), clamp(0)(7)).orNull()).to.be.eq(3);
  });
});

// --- Pitfalls ---

interface TestUser {
  name?: string;
}

describe('pitfalls', () => {
  beforeEach(() => {
    sandbox.on(console, ['log'], noop);
  });
  afterEach(() => {
    sandbox.restore();
  });
  describe('none without explicit type', () => {
    it('bad', () => {
      let a = none;
      // @ts-expect-error TS2741: Property ''@@type'' is missing in type 'Opt<number>' but required in type 'None<any>'.
      a = opt(1);
      suppressUnused(a);
    });
    it('good', () => {
      let a: Opt<number> = none;
      a = opt(1);
      suppressUnused(a);
    });
  });
  describe('empty value in some', () => {
    describe('unexpected null', () => {
      it('bad', () => {
        const getNameOrDefault = (x?: TestUser) => opt(x).map(x => x.name).orElse('John');

        const nameKon = getNameOrDefault({name: 'Kon'}); // 'Kon'
        expect(nameKon).to.be.eq('Kon');

        const nameDefaultFromUndefined = getNameOrDefault(); // 'John'
        expect(nameDefaultFromUndefined).to.be.eq('John');

        const nameDefaultFromEmpty = getNameOrDefault({}); // undefined
        expect(nameDefaultFromEmpty).to.be.undefined;

        suppressUnused(nameKon, nameDefaultFromEmpty, nameDefaultFromUndefined);
      });

      it('good - chainToOpt', () => {
        const getNameOrDefault = (x?: TestUser) => opt(x).chainToOpt(x => x.name).orElse('John');

        const nameKon = getNameOrDefault({name: 'Kon'}); // 'Kon'
        expect(nameKon).to.be.eq('Kon');

        const nameDefaultFromUndefined = getNameOrDefault(); // 'John'
        expect(nameDefaultFromUndefined).to.be.eq('John');

        const nameDefaultFromEmpty = getNameOrDefault({});
        expect(nameDefaultFromEmpty).to.be.eq('John');

        suppressUnused(nameKon, nameDefaultFromEmpty, nameDefaultFromUndefined);
      });

      it('good - prop', () => {
        const getNameOrDefault = (x?: TestUser) => opt(x).prop('name').orElse('John');

        const nameKon = getNameOrDefault({name: 'Kon'}); // 'Kon'
        expect(nameKon).to.be.eq('Kon');

        const nameDefaultFromUndefined = getNameOrDefault(); // 'John'
        expect(nameDefaultFromUndefined).to.be.eq('John');

        const nameDefaultFromEmpty = getNameOrDefault({});
        expect(nameDefaultFromEmpty).to.be.eq('John');

        suppressUnused(nameKon, nameDefaultFromEmpty, nameDefaultFromUndefined);
      });

    });
  });
  it('map vs onSome', () => {
    let a = 0;
    const setA = (newA: number) => { a = newA; };
    expect(a).to.be.eq(0);

    opt(null as number | null).map(setA); // `a` is unchanged
    expect(a).to.be.eq(0);

    opt(null as number | null).onSome(setA); // `a` is unchanged
    expect(a).to.be.eq(0);

    opt(2).map(setA); // `a` is now 2
    expect(a).to.be.eq(2);

    opt(4).onSome(setA); // `a` is now 4
    expect(a).to.be.eq(4);

    expect(
      opt(7).map(setA) // Some(undefined)
            .orNull(),
    ).to.be.undefined;
    expect(
      opt(9).onSome(setA) // Some(9)
            .orNull(),
    ).to.be.eq(9);

    expect(
      opt(2) // Some(2)
      .map(x => x + 1) // Some(3)
      .map(x => x * x) // Some(9)
      .orNull(),
    ).to.be.eq(9);
    expect(
      opt(2) // Some(2)
      .onSome(x => x + 1) // Some(2)
      .onSome(x => x * x) // Some(2)
      .orNull(),
    ).to.be.eq(2);

    const f = (x?: number) => opt(x)
    .onSome(x => console.log('Got value', x))
    .chainToOpt(x => x * x > 9 ? null : x * 2)
    .onSome(x => console.log('First step result', x))
    .map(x => x - 1)
    .onSome(x => console.log('Second step result', x))
    .orNull();
    expect(console.log).to.have.been.called.exactly(0);

    expect(f()).to.be.null;
    expect(console.log).to.have.been.called.exactly(0);

    expect(f(3)).to.be.eq(5);
    expect(console.log).to.have.been.called.exactly(3);
    expect(console.log).to.have.been.called.with.exactly('Got value', 3);
    expect(console.log).to.have.been.called.with.exactly('First step result', 6);
    expect(console.log).to.have.been.called.with.exactly('Second step result', 5);

    expect(f(10)).to.be.null;
    expect(console.log).to.have.been.called.exactly(4);
    expect(console.log).to.have.been.called.with.exactly('Got value', 10);
  });
});

describe('simple lens-like use', () => {
  it('example', () => {
    interface House {
      occupantIds?: number[];
    }

    const h = {
      occupantIds: [7],
    } as House | undefined;

    const r1 =
      opt(h) // Some({ occupantIds: [7] }) :: Opt<House>
      .prop('occupantIds') // Some([7]) :: Opt<number[]>
      .at(0) // Some(7) :: Opt<number>
      .orElse(-1) // 7 :: number
    ;
    expect(r1).to.be.eq(7);

    const r2 =
      opt(h) // Some({ occupantIds: [7] }) :: Opt<House>
      .prop('occupantIds') // Some([7]) :: Opt<number[]>
      .at(99) // None :: Opt<number> (nonexisting index)
      .orElse(-1) // -1 :: number
    ;
    expect(r2).to.be.eq(-1);

    const h2 = {} as House | undefined;
    const r3 =
      opt(h2) // Some({ }) :: Opt<House>
      .prop('occupantIds') // None :: Opt<number[]> (field is undefined)
      .at(0) // None :: Opt<number>
      .orElse(-1) // -1 :: number
    ;
    expect(r3).to.be.eq(-1);
  });
});

describe('data flow', () => {
  it('example', () => {
    const x = 10;
    const f = (a: Opt<number>) => a.map(inc);
    const g = (a: number) => opt(a * 2).map(toString);
    expect(g(f(opt(x)).orElse(4)).orElse('a')).to.be.eq('22');
    expect(g(f(none).orElse(4)).orElse('a')).to.be.eq('8');
    expect(
      pipe(x, opt, f, orElse(4), g, orElse('a')),
    ).to.be.eq('22');
  });
});

describe('not', () => {
  it('negates boolean', () => {
    expect(not(true)).to.be.false;
    expect(not(false)).to.be.true;
  });
});

describe('and', () => {
  it('logical and of two booleans', () => {
    expect(and(true)(false)).to.be.false;
    expect(and(true)(true)).to.be.true;
  });
});

describe('or', () => {
  it('logical or of two booleans', () => {
    expect(or(true)(false)).to.be.true;
    expect(or(false)(false)).to.be.false;
  });
});

describe('xor', () => {
  it('logical xor of two booleans', () => {
    expect(xor(true)(false)).to.be.true;
    expect(xor(true)(true)).to.be.false;
  });
});

describe('bool', () => {
  it('returns one of two values based on condition', () => {
    expect(bool(0)(1)(true)).to.eq(1);
    expect(bool('no')('yes')(false)).to.eq('no');
    expect(opt(true).map(bool(0)(1)).orNull()).to.eq(1);
    expect(opt(false).map(bool(0)(1)).orNull()).to.eq(0);
  });
});

describe('inc', () => {
  it('increments number by 1', () => {
    expect(inc(5)).to.eq(6);
  });
});

describe('dec', () => {
  it('decrements number by 1', () => {
    expect(dec(5)).to.eq(4);
  });
});

describe('prependStr', () => {
  it('prepends string', () => {
    expect(prependStr('Inu')('yasha')).to.eq('Inuyasha');
    expect(prependStr('foo')('bar')).to.eq('foobar');
  });
  it('type checks', () => {
    const inu = 'Inu' as const;
    const yasha = 'yasha' as const;
    const exp = 'Inuyasha' as const;
    const res: typeof exp = prependStr(inu)(yasha);
    expect(res).to.eq(exp);
  });
  it('refuses wrong type', () => {
    // @ts-expect-error type mismatch
    const res: 'x' = prependStr('a' as const)('b' as const);
    suppressUnused(res);
  });
  it('usable with opt', () => {
    expect(opt('bar').map(prependStr('foo')).orNull()).to.eq('foobar');
  });
});

describe('appendStr', () => {
  it('appends string', () => {
    expect(appendStr('yasha')('Inu')).to.eq('Inuyasha');
    expect(appendStr('bar')('foo')).to.eq('foobar');
  })
  it('type checks', () => {
    const inu = 'Inu' as const;
    const yasha = 'yasha' as const;
    const exp = 'Inuyasha' as const;
    const res: typeof exp = appendStr(yasha)(inu);
    expect(res).to.eq(exp);
  });
  it('refuses wrong type', () => {
    // @ts-expect-error type mismatch
    const res: 'x' = appendStr('a' as const)('b' as const);
    suppressUnused(res);
  });
  it('usable with opt', () => {
    expect(opt('foo').map(appendStr('bar')).orNull()).to.eq('foobar');
    const r: 'foobar' | null = opt('foo' as const).map(appendStr('bar')).orNull();
    suppressUnused(r);
  });
})

describe('crash', () => {
  it('throws error with message', () => {
    expect(() => {
      crash('Zeref?');
    }).to.throw('Zeref?');
  });
});

describe('eq', () => {
  it('checks equality', () => {
    expect(eq(5)(5)).to.be.true;
    expect(eq(5)(6)).to.be.false;
    expect(eq('hello')('hello')).to.be.true;
    expect(eq('hello')('Hi')).to.be.false;
    expect(eq({})({})).to.be.false;
    expect(opt(1).map(eq(1)).orNull()).to.be.true;
    // @ts-expect-error type mismatch
    expect(opt('').map(eq(2)).orNull()).to.be.false;
  });
});

describe('eqAny', () => {
  it('checks equality', () => {
    expect(eqAny('hello')('hello')).to.be.true;
    expect(eqAny('hello')('Hi')).to.be.false;
    expect(opt(1).map(eqAny('hello')).orNull()).to.be.false;
    expect(opt(1).map(eqAny(1)).orNull()).to.be.true;
    expect(opt('').map(eqAny(2)).orNull()).to.be.false;
  });
});

describe('noop', () => {
  it('returns undefined', () => {
    // noinspection JSVoidFunctionReturnValueUsed
    expect(noop()).to.be.undefined;
  });
});

describe('jestSnapshotSerializer', () => {
  before(() => {
    chaiJestSnapshot.resetSnapshotRegistry();
  });

  beforeEach(function () {
    chaiJestSnapshot.configureUsingMochaContext(this);
  });

  it('renders correctly Opt prop', () => {
    const Component = (props: Record<string, Opt<unknown>>) => React.createElement('div', null,JSON.stringify(props));
    const component = React.createElement(Component, {num: opt(0), none: none, obj: opt({a: 4}), str: opt('cat ears'), undef: some(undefined), null: some(null)});
    expect(component).to.matchSnapshot('test/jestSnapshotSerializer.snapshot', 'opt prop');
  });
});
