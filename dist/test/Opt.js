"use strict";
/* tslint:disable:no-unused-expression */
Object.defineProperty(exports, "__esModule", { value: true });
var chai = require("chai");
var spies = require("chai-spies");
var Opt_1 = require("../src/Opt");
chai.use(spies);
var expect = chai.expect;
chai.should();
var add1 = function (x) { return x + 1; };
var gt0 = function (x) { return x > 0; };
var lt0 = function (x) { return x < 0; };
var randomNumOpt = function () { return Math.random() > .5 ? Opt_1.none : Opt_1.some(Math.random()); };
describe('opt', function () {
    it('construction', function () {
        expect(Opt_1.opt(undefined).isEmpty).to.be.true;
        expect(Opt_1.opt(NaN).isEmpty).to.be.true;
        expect(Opt_1.opt(null).isEmpty).to.be.true;
        expect(Opt_1.opt('').isEmpty).to.be.false;
        expect(Opt_1.opt('a').isEmpty).to.be.false;
        expect(Opt_1.opt(0).isEmpty).to.be.false;
        expect(Opt_1.opt(1).isEmpty).to.be.false;
        expect(Opt_1.opt({}).isEmpty).to.be.false;
        expect(Opt_1.opt([]).isEmpty).to.be.false;
    });
    it('isEmpty', function () {
        expect(Opt_1.opt(null).isEmpty).to.be.true;
        expect(Opt_1.opt(0).isEmpty).to.be.false;
    });
    it('nonEmpty', function () {
        expect(Opt_1.opt(null).nonEmpty).to.be.false;
        expect(Opt_1.opt(0).nonEmpty).to.be.true;
    });
    it('length', function () {
        expect(Opt_1.opt(null).length).to.eq(0);
        expect(Opt_1.opt(0).length).to.eq(1);
    });
    it('orUndef', function () {
        expect(Opt_1.opt(null).orUndef()).to.eq(undefined);
        expect(Opt_1.opt(0).orUndef()).to.eq(0);
    });
    it('orNull', function () {
        expect(Opt_1.opt(null).orNull()).to.eq(null);
        expect(Opt_1.opt(0).orNull()).to.eq(0);
    });
    it('orFalse', function () {
        expect(Opt_1.opt(null).orFalse()).to.be.false;
        expect(Opt_1.opt(0).orFalse()).to.eq(0);
    });
    it('orTrue', function () {
        expect(Opt_1.opt(null).orTrue()).to.be.true;
        expect(Opt_1.opt(0).orTrue()).to.eq(0);
    });
    it('orNaN', function () {
        expect(Opt_1.opt(null).orNaN()).to.be.NaN;
        expect(Opt_1.opt(0).orNaN()).to.eq(0);
    });
    it('orCrash', function () {
        expect(function () { return Opt_1.opt(null).orCrash(''); }).to.throw();
        expect(Opt_1.opt(0).orCrash('')).to.eq(0);
        expect(Opt_1.opt([]).orCrash('')).to.eql([]);
    });
    it('map', function () {
        expect(Opt_1.opt(1).map(add1).orNull()).to.eq(2);
        expect(Opt_1.opt(null).map(add1).orUndef()).to.eq(undefined);
    });
    it('flatMap', function () {
        expect(Opt_1.opt(1).flatMap(function () { return Opt_1.some(2); }).orNull()).to.eq(2);
        expect(Opt_1.opt(1).flatMap(function () { return Opt_1.none; }).orNull()).to.eq(null);
        expect(Opt_1.opt(null).flatMap(function () { return Opt_1.none; }).orUndef()).to.eq(undefined);
        expect(Opt_1.opt(null).chain(function () { return Opt_1.none; }).orUndef()).to.eq(undefined);
    });
    it('toArray', function () {
        expect(Opt_1.opt(1).toArray()).to.eql([1]);
        expect(Opt_1.opt(null).toArray()).to.eql([]);
    });
    describe('caseOf', function () {
        it('fp', function () {
            expect(Opt_1.some(1).caseOf(function (x) { return 'x' + x; }, function () { return 'y'; })).to.eql('x1');
            expect(Opt_1.none.caseOf(function (x) { return 'x' + x; }, function () { return 'y'; })).to.eql('y');
            expect(randomNumOpt().caseOf(function () { return 'x'; }, function () { return 'y'; })).to.be.oneOf(['x', 'y']);
        });
        it('imperative', function () {
            var noneCb = chai.spy();
            var someCb = chai.spy();
            noneCb.should.have.not.been.called();
            someCb.should.have.not.been.called();
            Opt_1.none.caseOf(someCb, noneCb);
            someCb.should.have.not.been.called();
            noneCb.should.have.been.called.once;
            Opt_1.some(1).caseOf(someCb, noneCb);
            someCb.should.have.been.called.once;
            someCb.should.have.been.called.always.with(1);
            noneCb.should.have.been.called.once;
        });
    });
    it('pipe', function () {
        expect(Opt_1.some(1).pipe(Opt_1.isOpt)).to.be.true;
        expect(Opt_1.none.pipe(Opt_1.isOpt)).to.be.true;
    });
    it('onNone', function () {
        var cb = chai.spy();
        cb.should.have.not.been.called();
        Opt_1.some(1).onNone(cb);
        cb.should.have.not.been.called();
        Opt_1.none.onNone(cb);
        cb.should.have.been.called.once;
    });
    it('onSome', function () {
        var cb = chai.spy();
        cb.should.have.not.been.called();
        Opt_1.none.onSome(cb);
        cb.should.have.not.been.called();
        Opt_1.some(1).onSome(cb);
        cb.should.have.been.called.once;
        cb.should.have.been.called.with.exactly(1);
    });
    it('contains', function () {
        expect(Opt_1.none.contains(0)).to.be.false;
        expect(Opt_1.some(1).contains(0)).to.be.false;
        expect(Opt_1.some(0).contains(0)).to.be.true;
    });
    it('exists', function () {
        expect(Opt_1.none.exists(function () { return true; })).to.be.false;
        expect(Opt_1.none.exists(function () { return false; })).to.be.false;
        expect(Opt_1.some(0).exists(function () { return false; })).to.be.false;
        expect(Opt_1.some(0).exists(function () { return true; })).to.be.true;
        var cb = chai.spy(function () { return true; });
        Opt_1.none.exists(cb);
        cb.should.have.not.been.called();
        Opt_1.some(0).exists(cb);
        cb.should.have.been.called.with.exactly(0);
    });
    it('forAll', function () {
        expect(Opt_1.none.forAll(function () { return true; })).to.be.true;
        expect(Opt_1.none.forAll(function () { return false; })).to.be.true;
        expect(Opt_1.some(1).forAll(function () { return true; })).to.be.true;
        expect(Opt_1.some(1).forAll(function () { return false; })).to.be.false;
        var cb = chai.spy(function () { return true; });
        Opt_1.none.forAll(cb);
        cb.should.have.not.been.called();
        Opt_1.some(0).forAll(cb);
        cb.should.have.been.called.with.exactly(0);
    });
    it('orElse', function () {
        expect(Opt_1.none.orElse(0)).to.be.eq(0);
        expect(Opt_1.some(1).orElse(0)).to.be.eq(1);
    });
    it('orElseOpt', function () {
        expect(Opt_1.none.orElseOpt(Opt_1.some(0)).orNull()).to.be.eq(0);
        expect(Opt_1.none.orElseOpt(Opt_1.none).orNull()).to.be.eq(null);
        expect(Opt_1.some(1).orElseOpt(Opt_1.none).orNull()).to.be.eq(1);
    });
    it('bimap', function () {
        expect(Opt_1.none.bimap(function (x) { return x; }, function () { return 'none'; }).orNull()).to.be.eq('none');
        expect(Opt_1.some('some').bimap(function (x) { return x; }, function () { return 'none'; }).orNull()).to.be.eq('some');
    });
    it('flatBimap', function () {
        expect(Opt_1.none.flatBimap(function (x) { return Opt_1.some(x); }, function () { return Opt_1.some('none'); }).orNull()).to.be.eq('none');
        expect(Opt_1.some('some').flatBimap(function (x) { return Opt_1.some(x); }, function () { return Opt_1.some('none'); }).orNull()).to.be.eq('some');
    });
    it('toString', function () {
        expect(Opt_1.some(1).toString()).to.eq('Some(1)');
        expect(Opt_1.some('a').toString()).to.eq('Some("a")');
        expect(Opt_1.some(null).toString()).to.eq('Some(null)');
        expect(Opt_1.none.toString()).to.eq('None');
    });
    it('chainToOpt', function () {
        expect(Opt_1.some(1).chainToOpt(function (x) { return x === 1 ? null : x + 1; }).orUndef()).to.be.eq(undefined);
        expect(Opt_1.some(2).chainToOpt(function (x) { return x === 1 ? null : x + 1; }).orUndef()).to.be.eq(3);
    });
    it('zip', function () {
        expect(Opt_1.some(1).zip(Opt_1.some(true)).orNull()).to.be.eql([1, true]);
        expect(Opt_1.some(1).zip(Opt_1.none).orNull()).to.be.null;
        expect(Opt_1.none.zip(Opt_1.some(true)).orNull()).to.be.null;
        expect(Opt_1.none.zip(Opt_1.some(true)).orNull()).to.be.null;
    });
    it('zip3', function () {
        expect(Opt_1.some(1).zip3(Opt_1.some(true), Opt_1.some('a')).orNull()).to.be.eql([1, true, 'a']);
        expect(Opt_1.some(1).zip3(Opt_1.some(true), Opt_1.none).orNull()).to.be.null;
        expect(Opt_1.some(1).zip3(Opt_1.none, Opt_1.some('a')).orNull()).to.be.null;
        expect(Opt_1.some(1).zip3(Opt_1.none, Opt_1.none).orNull()).to.be.null;
        expect(Opt_1.none.zip3(Opt_1.some(true), Opt_1.some('a')).orNull()).to.be.null;
        expect(Opt_1.none.zip3(Opt_1.some(true), Opt_1.none).orNull()).to.be.null;
        expect(Opt_1.none.zip3(Opt_1.some(true), Opt_1.some('a')).orNull()).to.be.null;
        expect(Opt_1.none.zip3(Opt_1.some(true), Opt_1.none).orNull()).to.be.null;
    });
    it('filter', function () {
        expect(Opt_1.some(1).filter(gt0).orNull()).to.be.eq(1);
        expect(Opt_1.some(1).filter(lt0).orNull()).to.be.null;
        expect(Opt_1.none.filter(lt0).orNull()).to.be.null;
    });
    it('noneIf', function () {
        expect(Opt_1.some(1).noneIf(lt0).orNull()).to.be.eq(1);
        expect(Opt_1.some(1).noneIf(gt0).orNull()).to.be.null;
        expect(Opt_1.none.noneIf(lt0).orNull()).to.be.null;
    });
});
describe('helper functions', function () {
    describe('some', function () {
        it('create', function () {
            expect(Opt_1.some(0).orNull()).to.eq(0);
            expect(Opt_1.some('').orNull()).to.eq('');
            expect(Opt_1.some(NaN).orNull()).to.eql(NaN);
            expect(Opt_1.some(null).orUndef()).to.eq(null);
        });
        it('falsy', function () {
            expect(Opt_1.some(null).orUndef()).to.eq(null);
            expect(Opt_1.some(NaN).orUndef()).to.eql(NaN);
            expect(Opt_1.some(undefined).orNull()).to.eq(undefined);
        });
    });
    it('fromArray', function () {
        expect(Opt_1.Opt.fromArray([]).orNull()).to.eq(null);
        expect(Opt_1.Opt.fromArray([1]).orNull()).to.eq(1);
        // expect(fromArray([1, 2]).orNull()).to.eq(1);
    });
    it('isOpt', function () {
        expect(Opt_1.isOpt(0)).to.be.false;
        expect(Opt_1.isOpt(undefined)).to.be.false;
        expect(Opt_1.isOpt({ value: 0 })).to.be.false;
        expect(Opt_1.isOpt(Opt_1.some(1))).to.be.true;
        expect(Opt_1.isOpt(Opt_1.none)).to.be.true;
    });
});
describe('optFalsy', function () {
    it('construction', function () {
        expect(Opt_1.optFalsy(undefined).isEmpty).to.be.true;
        expect(Opt_1.optFalsy(NaN).isEmpty).to.be.true;
        expect(Opt_1.optFalsy(null).isEmpty).to.be.true;
        expect(Opt_1.optFalsy('').isEmpty).to.be.true;
        expect(Opt_1.optFalsy('a').isEmpty).to.be.false;
        expect(Opt_1.optFalsy(0).isEmpty).to.be.true;
        expect(Opt_1.optFalsy(1).isEmpty).to.be.false;
        expect(Opt_1.optFalsy({}).isEmpty).to.be.false;
        expect(Opt_1.optFalsy([]).isEmpty).to.be.false;
    });
    it('false not in T', function () {
        var x = Opt_1.optFalsy(1).orNull();
        expect(x).to.be.eq(1);
    });
});
describe('optEmptyArray', function () {
    it('construction', function () {
        expect(Opt_1.optEmptyArray(null).isEmpty).to.be.true;
        expect(Opt_1.optEmptyArray(undefined).isEmpty).to.be.true;
        expect(Opt_1.optEmptyArray([]).isEmpty).to.be.true;
        expect(Opt_1.optEmptyArray([0]).isEmpty).to.be.false;
        expect(Opt_1.optEmptyArray([0]).orNull()).to.eql([0]);
    });
});
describe('optEmptyObject', function () {
    it('construction', function () {
        expect(Opt_1.optEmptyObject(null).isEmpty).to.be.true;
        expect(Opt_1.optEmptyObject(undefined).isEmpty).to.be.true;
        expect(Opt_1.optEmptyObject({}).isEmpty).to.be.true;
        expect(Opt_1.optEmptyObject({ a: 1 }).isEmpty).to.be.false;
        expect(Opt_1.optEmptyObject({ a: 1 }).map(function (x) { return x.a; }).orNull()).to.be.eq(1);
    });
});
describe('application', function () {
    it('ap', function () {
        expect(Opt_1.ap(Opt_1.opt(gt0))(Opt_1.opt(1)).orNull()).to.be.true;
        expect(Opt_1.ap(Opt_1.none)(Opt_1.opt(1)).orNull()).to.be.null;
        expect(Opt_1.ap(Opt_1.opt(gt0))(Opt_1.none).orNull()).to.be.null;
        expect(Opt_1.ap(Opt_1.none)(Opt_1.none).orNull()).to.be.null;
    });
    it('apFn', function () {
        expect(Opt_1.apFn(gt0)(Opt_1.opt(1)).orNull()).to.be.true;
        expect(Opt_1.apFn(gt0)(Opt_1.none).orNull()).to.be.null;
    });
});
describe('catOpts', function () {
    it('converts to array', function () {
        expect(Opt_1.catOpts([])).to.eql([]);
        expect(Opt_1.catOpts([Opt_1.some(1)])).to.eql([1]);
        expect(Opt_1.catOpts([Opt_1.none])).to.eql([]);
        expect(Opt_1.catOpts([Opt_1.opt(1), Opt_1.opt(null)])).to.eql([1]);
    });
});
describe('mapOpt', function () {
    it('maps', function () {
        expect(Opt_1.mapOpt(Opt_1.opt)([1, 2, 3])).to.eql([1, 2, 3]);
    });
    it('omits', function () {
        expect(Opt_1.mapOpt(function (_x) { return Opt_1.none; })([1])).to.eql([]);
    });
    it('maps and omits', function () {
        expect(Opt_1.mapOpt(function (x) { return x > 0 ? Opt_1.opt(x) : Opt_1.none; })([-1, 0, 1])).to.eql([1]);
    });
});
describe('examples', function () {
    it('basic', function () {
        // without
        var f = function (name) {
            if (!name || name === '') {
                throw new Error('Missing name.');
            }
            return name[0];
        };
        // with
        var g = function (name) { return Opt_1.opt(name).orCrash('Missing name.')[0]; };
        f('Riker'); // 'R'
        g('Riker'); // 'R'
        // f(undefined); // exception thrown
        // g(undefined); // exception thrown
        expect(f('Riker')).to.eq('R');
        expect(g('Riker')).to.eq('R');
        expect(function () { return f(undefined); }).to.throw();
        expect(function () { return g(undefined); }).to.throw();
    });
    it('caseOf', function () {
        var console = {
            logHistory: [],
            log: function (x) { this.logHistory.push(x); }
        };
        // tslint:disable-next-line:no-console
        var fireMissiles = function () { console.log('FIRING!'); };
        // tslint:disable-next-line:no-console
        var printSuccess = function (x) { console.log(x); };
        var handleMoveVanilla = function (usersMove) { return usersMove ? printSuccess(usersMove) : fireMissiles(); };
        var handleMove = function (usersMove) { return Opt_1.opt(usersMove).caseOf(printSuccess, fireMissiles); };
        handleMoveVanilla(); // prints FIRING!
        handleMove(); // prints FIRING!
        handleMoveVanilla('Build a pylon.'); // prints Build a pylon.
        handleMove('Build a pylon.'); // prints Build a pylon.
        // tslint:disable-next-line:no-console
        expect(console.logHistory).to.eql(['FIRING!', 'FIRING!', 'Build a pylon.', 'Build a pylon.']);
    });
    it('more advanced', function () {
        var db = {
            0: { name: 'John', surname: null },
            1: { name: 'Worf', surname: 'Mercer' }
        };
        // without
        var f = function (id) {
            if (id === undefined) {
                return null;
            }
            var item = db[id];
            if (!item) {
                return null;
            }
            var surname = item.surname ? item.surname.toUpperCase() : '<missing>';
            return item.name + ' ' + surname;
        };
        // with
        var g = function (id) { return Opt_1.opt(id)
            .chainToOpt(function (x) { return db[x]; })
            .map(function (item) { return item.name + ' ' + Opt_1.opt(item.surname).map(function (x) { return x.toUpperCase(); }).orElse('<missing>'); })
            .orNull(); };
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
//# sourceMappingURL=Opt.js.map