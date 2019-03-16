import * as chai from 'chai';
import * as spies from 'chai-spies';

import { opt, some, none, isOpt, Opt } from '../src/Opt';

chai.use(spies);
const {expect} = chai;
chai.should();

const add1 = (x: number) => x + 1;

const randomNumOpt = (): Opt<number> => Math.random() > .5 ? none : some(Math.random());

describe('opt', () => {
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

  it('orCrash', () => {
    expect(() => opt(null).orCrash('')).to.throw();
    expect(opt(0).orCrash('')).to.eq(0);
    expect(opt([]).orCrash('')).to.eql([]);
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
    some(1).onNone(cb);
    cb.should.have.not.been.called();
    none.onNone(cb);
    cb.should.have.been.called.once;
  });

  it('onSome', () => {
    const cb = chai.spy();
    cb.should.have.not.been.called();
    none.onSome(cb);
    cb.should.have.not.been.called();
    some(1).onSome(cb);
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

interface Person {
  name: string;
  surname: string | null;
}

type Db = { [_: string]: Person };

describe('examples', () => {
  it('1', () => {
    const db: Db = {
      '0': {name: 'John', surname: null},
      '1': {name: 'Worf', surname: 'Mercer'}
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
