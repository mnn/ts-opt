/* tslint:disable:no-unused-expression no-console */

import * as chai from 'chai';
import * as spies from 'chai-spies';

import {
  ap,
  apFn,
  catOpts,
  isOpt,
  joinOpt,
  mapOpt,
  none,
  opt,
  Opt,
  optEmptyArray,
  optEmptyObject,
  optEmptyString,
  optFalsy,
  optNegative,
  optZero,
  ReduxDevtoolsCompatibilityHelper,
  some,
  orCrash,
  map,
  flatMap,
  fromArray,
  toArray,
  chainToOpt,
  orUndef,
  orNull,
  caseOf,
  contains,
  exists,
  forAll,
  orElse,
  orElseOpt,
  bimap,
  zip,
  zip3,
  zip4,
  zip5,
  filter,
  narrow,
  print,
  prop,
  someOrCrash,
  orFalse,
  orTrue,
  orNaN,
  equals,
  pipe,
} from '../src/Opt';

chai.use(spies);
const {expect} = chai;
chai.should();
const sandbox = chai.spy.sandbox();

const add1 = (x: number) => x + 1;
const gt0 = (x: number): boolean => x > 0;
const lt0 = (x: number): boolean => x < 0;

const randomNumOpt = (): Opt<number> => Math.random() > .5 ? none : some(Math.random());

const isString = (x: any): x is string => typeof x === 'string';
const isNumber = (x: any): x is number => typeof x === 'number';
const isObject = (x: any): x is object => typeof x === 'object';
const isArray = (x: any): x is unknown[] => Array.isArray(x);

const head = <T>(xs: T[]): undefined | T => xs[0];
const eq = (a: unknown) => (b: unknown) => a === b;
const id = <T>(x: T): T => x;

type EnumAB = 'a' | 'b';
type Enum12 = 1 | 2;
type EnumABC = 'a' | 'b' | 'c';
type ObjA = { a: boolean };
type ObjB = { b: number };
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
    sandbox.on(console, ['log']);
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

  it('isSome', () => {
    const f = (a: Opt<number>) => {
      if (a.isSome()) {
        expect(a.value).to.eq(4);
      } else {
        // @ts-expect-error
        expect(a.value).to.be.undefined;
      }
    };
    f(none);
    f(opt(4));
  });

  it('isNone', () => {
    const f = (a: Opt<number>) => {
      if (a.isNone()) {
        // @ts-expect-error
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
  });

  it('optOrCrash', () => {
    expect(() => opt(null).optOrCrash('')).to.throw();
    expect(opt(0).optOrCrash('').orNull()).to.be.eq(0);
    expect(opt([]).optOrCrash('').orNull()).to.be.eql([]);
  });

  it('someOrCrash', () => {
    expect(() => opt(null).someOrCrash('')).to.throw();
    expect(opt(0).someOrCrash('').orNull()).to.be.eq(0);
    expect(opt([]).someOrCrash('').orNull()).to.be.eql([]);
    const some0 = opt(0).someOrCrash('');
    expect(some0.value).to.be.eql(0);
  });

  it('map', () => {
    expect(opt(1).map(add1).orNull()).to.eq(2);
    expect(opt(null as unknown as number).map(add1).orUndef()).to.eq(undefined);
  });

  it('flatMap', () => {
    expect(opt(1).flatMap(() => some(2)).orNull()).to.eq(2);
    expect(opt(1).flatMap(() => none).orNull()).to.eq(null);
    expect(opt(null).flatMap(() => none).orUndef()).to.eq(undefined);
    expect(opt(null).chain(() => none).orUndef()).to.eq(undefined);
  });

  it('toArray', () => {
    expect(opt(1).toArray()).to.eql([1]);
    expect(opt(null).toArray()).to.eql([]);
  });

  describe('caseOf', () => {
    it('fp', () => {
      expect(some(1).caseOf(x => 'x' + x, () => 'y')).to.eql('x1');
      expect(none.caseOf(x => 'x' + x, () => 'y')).to.eql('y');
      expect(randomNumOpt().caseOf(() => 'x', () => 'y')).to.be.oneOf(['x', 'y']);
    });
    it('imperative', () => {
      const noneCb = chai.spy();
      const someCb = chai.spy();
      noneCb.should.have.not.been.called();
      someCb.should.have.not.been.called();
      none.caseOf(someCb, noneCb);
      someCb.should.have.not.been.called();
      noneCb.should.have.been.called.once;
      some(1).caseOf(someCb, noneCb);
      someCb.should.have.been.called.once;
      someCb.should.have.been.called.always.with(1);
      noneCb.should.have.been.called.once;
    });
  });

  it('pipe', () => {
    expect(some(1).pipe(isOpt)).to.be.true;
    expect(none.pipe(isOpt)).to.be.true;
    expect(some(1).pipe(x => x.map(add1), x => x.orNull())).to.be.eq(2);
    expect(some(1).pipe(orElse(7), add1, add1)).to.be.eq(3);
    expect(some(1).pipe(orElse(7), add1, add1, add1)).to.be.eq(4);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1)).to.be.eq(5);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1, add1)).to.be.eq(6);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1, add1, add1)).to.be.eq(7);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1, add1, add1, add1)).to.be.eq(8);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1, add1, add1, add1, add1)).to.be.eq(9);
    expect(some(1).pipe(orElse(7), add1, add1, add1, add1, add1, add1, add1, add1, add1)).to.be.eq(10);
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

  it('orElse', () => {
    expect(none.orElse(0)).to.be.eq(0);
    expect(some(1).orElse(0)).to.be.eq(1);
  });

  it('orElseOpt', () => {
    expect(none.orElseOpt(some(0)).orNull()).to.be.eq(0);
    expect(none.orElseOpt(none).orNull()).to.be.eq(null);
    expect(some(1).orElseOpt(none).orNull()).to.be.eq(1);
  });

  it('bimap', () => {
    expect(none.bimap(x => x, () => 'none').orNull()).to.be.eq('none');
    expect(some('some').bimap(x => x, () => 'none').orNull()).to.be.eq('some');
  });

  it('flatBimap', () => {
    expect(none.flatBimap(x => some(x), () => some('none')).orNull()).to.be.eq('none');
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

  it('zip', () => {
    expect(some(1).zip(some(true)).orNull()).to.be.eql([1, true]);
    expect(some(1).zip(none).orNull()).to.be.null;
    expect(none.zip(some(true)).orNull()).to.be.null;
    expect(none.zip(some(true)).orNull()).to.be.null;
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

  it('noneIf', () => {
    expect(some(1).noneIf(lt0).orNull()).to.be.eq(1);
    expect(some(1).noneIf(gt0).orNull()).to.be.null;
    expect(none.noneIf(lt0).orNull()).to.be.null;
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
    // incorrect uses
    // @ts-expect-error
    const wrongWiden2 = opt(abc).widen<EnumAB, EnumAB>();
    // @ts-expect-error
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
    // @ts-expect-error
    opt(oab).prop('X');
    const a = {x: 1};
    const xValue = opt(a).prop('x').orCrash('missing prop x'); // 1
    expect(xValue).to.be.eq(1);
    // removing of "opty" values from type parameter of result
    const getValue2 = (filter?: FilterPart): boolean | null => opt(filter).prop('values').map(xs => xs[0] === 'true').orNull();
    const getValue3 = (filter?: FilterPart): boolean | null => opt(filter).prop('values').map(head).map(eq('true')).orNull();
    suppressUnused(getValue2, getValue3);
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
});

interface Person {
  name: string;
  surname: string | null;
}

interface Db {[_: string]: Person}

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

    const handleMoveVanilla = (usersMove?: string): void => usersMove ? printSuccess(usersMove) : fireMissiles();
    const handleMove = (usersMove?: string): void => opt(usersMove).caseOf(printSuccess, fireMissiles);

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

describe('joinOpt', () => {
  it('joins', () => {
    expect(joinOpt(some(none)).orNull()).to.eql(null);
    expect(joinOpt(some(some(1))).orNull()).to.eql(1);
    expect(joinOpt(none).orNull()).to.eql(null);
  });
});

describe('fromArray', () => {
  it('creates opt from array', () => {
    expect(fromArray([7]).orNull()).to.be.eq(7);
    expect(fromArray([]).orNull()).to.be.eq(null);
  });
});

describe('toArray', () => {
  it('converts opt to array', () => {
    expect(toArray(opt(1))).to.be.eql([1]);
    expect(toArray(none)).to.be.eql([]);
  });
});

describe('orCrash', () => {
  const f = flow2(opt, orCrash('msg'));
  it('crashes on none', () => {
    expect(() => f(null)).to.throw;
  });
  it('returns value on some', () => {
    expect(f(1)).to.eq(1);
  });
});

describe('map', () => {
  it('is correctly typed', () => {
    expect(map((x: number) => x)([])).to.eql([]);
    // ---
    const r1: number[] = map((x: number) => x + 1)([1]);
    // @ts-expect-error
    const r1Fail: Opt<number> = map((x: number) => x + 1)([1]);
    suppressUnused(r1Fail);
    expect(r1).to.eql([2]);
    expect(map((_x: number) => false)(none).orNull()).to.be.null;
    expect(map((x: number) => x + 1)(some(1)).orNull()).to.eq(2);
    // ---
    const r2: Opt<number> = map((x: number) => x + 1)(opt(1));
    // @ts-expect-error
    const r2Fail: number[] = map((x: number) => x + 1)(opt(1));
    suppressUnused(r2Fail);
    expect(r2.orNull()).to.eq(2);
  });

  it('works with flow', () => {
    const r1 = flow2(id, id)([]);
    expect(r1).to.be.eql([]);
    // ---
    const r2 = flow2(map(add1), id)([1, 2]);
    expect(r2).to.be.eql([2, 3]);
    // ---
    const r3 = flow2(map(add1), id)(opt(1));
    expect(r3.orNull()).to.be.eq(2);
    // ---
    const r4 = flow2(id, id)(opt(1));
    expect(r4.orNull()).to.be.eq(1);
    // ---
    const r5 = flow2(map(add1), id)(opt(1));
    expect(r5.orNull()).to.be.eq(2);
    // ---
    const r6 = flow2(map(add1), map(gt0))(opt(1));
    expect(r6.orNull()).to.be.true;
  });
});

describe('flatMap', () => {
  it('is correctly typed', () => {
    expect(flatMap((x: number) => [x])([])).to.eql([]);
    // ---
    const r1: number[] = flatMap((x: number) => [x + 1])([1]);
    // @ts-expect-error
    const r1Fail: Opt<number> = flatMap((x: number) => [x + 1])([1]);
    suppressUnused(r1Fail);
    expect(r1).to.eql([2]);
    expect(flatMap((_x: number) => none)(none).orNull()).to.be.null;
    expect(flatMap((x: number) => opt(x + 1))(some(1)).orNull()).to.eq(2);
    // ---
    const r2: Opt<number> = flatMap((x: number) => opt(x + 1))(opt(1));
    // @ts-expect-error
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
});

describe('chainToOpt', () => {
  it('chains to opt', () => {
    expect(chainToOpt(_x => undefined)(opt(1)).orNull()).to.be.null;
    expect(chainToOpt(_x => 'x')(opt(1)).orNull()).to.be.eq('x');
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
    expect(caseOf(add1)(() => 7)(opt(1))).to.be.eq(2);
  });
  it('uses onNone with none', () => {
    expect(caseOf(add1)(() => 7)(none)).to.be.eq(7);
  });
});

describe('pipe', () => {
  it('pipes', () => {
    expect(pipe(0, add1)).to.be.eq(1);
    expect(pipe(0, add1, add1)).to.be.eq(2);
    expect(pipe(0, add1, add1, add1)).to.be.eq(3);
    expect(pipe(0, add1, add1, add1, add1)).to.be.eq(4);
    expect(pipe(0, add1, add1, add1, add1, add1)).to.be.eq(5);
    expect(pipe(0, add1, add1, add1, add1, add1, add1)).to.be.eq(6);
    expect(pipe(0, add1, add1, add1, add1, add1, add1, add1)).to.be.eq(7);
    expect(pipe(0, add1, add1, add1, add1, add1, add1, add1, add1)).to.be.eq(8);
    expect(pipe(0, add1, add1, add1, add1, add1, add1, add1, add1, add1)).to.be.eq(9);
    expect(pipe(0, add1, x => x.toString(10), x => x.length)).to.be.eq(1);
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

describe('exists', () => {
  it('positive', () => {
    expect(exists(eq(1))(opt(1))).to.be.true;
  });
  it('negative', () => {
    expect(exists(eq(2))(opt(1))).to.be.false;
    expect(exists(eq(2))(none)).to.be.false;
  });
});

describe('forAll', () => {
  it('positive', () => {
    expect(forAll(eq(1))(opt(1))).to.be.true;
    expect(forAll(eq(2))(none)).to.be.true;
  });
  it('negative', () => {
    expect(forAll(eq(2))(opt(1))).to.be.false;
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

describe('orElseOpt', () => {
  it('returns value on some', () => {
    expect(orElseOpt(opt(0))(opt(1)).orNull()).to.be.eq(1);
  });
  it('returns default value on none', () => {
    expect(orElseOpt(opt(0))(none).orNull()).to.be.eq(0);
  });
});

describe('bimap', () => {
  it('uses onSome with some', () => {
    expect(bimap(add1)(() => 7)(opt(1)).orNull()).to.be.eq(2);
  });
  it('uses onNone with none', () => {
    expect(bimap(add1)(() => 7)(none).orNull()).to.be.eq(7);
  });
});

describe('zip', () => {
  describe('checks types', () => {
    it('opt', () => {
      const a: Opt<[boolean, number]> = zip(opt(1))(opt(true));
      // @ts-expect-error
      const aFail: Opt<[number, boolean]> = zip(opt(1))(opt(true));
      suppressUnused(a, aFail);
    });
    it('array', () => {
      const a: [boolean, number][] = zip([1, 2])([true, false]);
      // @ts-expect-error
      const aFail: [number, boolean][] = zip(opt(1))(opt(true));
      suppressUnused(a, aFail);
    });
    it('mixing', () => {
      // @ts-expect-error
      const aFail: [boolean, number][] = zip(opt(1))(opt(true));
      suppressUnused(aFail);
    });
  });

  it('opt', () => {
    expect(zip(opt(2))(opt(1)).orNull()).to.be.eql([1, 2]);
    expect(zip(opt(1))(none).orNull()).to.be.eql(null);
    expect(zip(none)(opt(2)).orNull()).to.be.eql(null);
    expect(zip(none)(none).orNull()).to.be.eql(null);
  });

  describe('array', () => {
    it('empty', () => {
      expect(zip([])([])).to.be.eql([]);
    });
    it('same length', () => {
      expect(zip([2])([1])).to.be.eql([[1, 2]]);
    });
    it('different length', () => {
      expect(zip([3])([1, 2])).to.be.eql([[1, 3]]);
      expect(zip([3, 4])([1])).to.be.eql([[1, 3]]);
    });
  });

  it('works with flow', () => {
    const a: Opt<[number, string]> = flow2((x: Opt<number>) => x, zip(opt('x')))(opt(2));
    expect(a.orNull()).to.be.eql([2, 'x']);
    const b: Opt<[number, string]> = flow2(zip(opt('x')), id)(opt(2));
    expect(b.orNull()).to.be.eql([2, 'x']);
  });
});

describe('zip3', () => {
  it('zips', () => {
    expect(zip3(opt(1))(opt(2))(opt(3)).orNull()).to.be.eql([3, 1, 2]);
  });
});

describe('zip4', () => {
  it('zips', () => {
    expect(zip4(opt(1))(opt(2))(opt(3))(opt(4)).orNull()).to.be.eql([4, 1, 2, 3]);
  });
});

describe('zip5', () => {
  it('zips', () => {
    expect(zip5(opt(1))(opt(2))(opt(3))(opt(4))(opt(5)).orNull()).to.be.eql([5, 1, 2, 3, 4]);
  });
});

describe('filter', () => {
  it('checks types', () => {
    const a: Opt<number> = filter(gt0)(opt(1));
    // @ts-expect-error
    const aFail: number[] = filter(gt0)(opt(1));
    suppressUnused(a, aFail);
    // ---
    const b: number[] = filter(gt0)([7]);
    // @ts-expect-error
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

describe('print', () => {
  beforeEach(() => {
    sandbox.on(console, ['log']);
  });

  afterEach(() => {
    sandbox.restore();
  });

  it('prints', () => {
    expect(console.log).to.have.been.called.exactly(0);
    print()(opt(1));
    expect(console.log).to.have.been.called.exactly(1);
    expect(console.log).to.have.been.called.with('Some:', 1);
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
    // @ts-expect-error
    prop<ObjA>('b');
  });
});
