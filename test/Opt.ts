/* tslint:disable:no-unused-expression no-console */

import * as chai from 'chai';
import * as spies from 'chai-spies';

import {
  ap,
  apFn,
  catOpts,
  isOpt,
  mapOpt,
  none,
  opt,
  Opt,
  optEmptyArray,
  optEmptyObject,
  optEmptyString,
  optFalsy,
  optZero,
  ReduxDevtoolsCompatibilityHelper,
  some,
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

type EnumAB = 'a' | 'b';
type Enum12 = 1 | 2;

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
