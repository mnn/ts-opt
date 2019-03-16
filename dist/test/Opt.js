"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai = require("chai");
var spies = require("chai-spies");
var Opt_1 = require("../src/Opt");
chai.use(spies);
var expect = chai.expect;
chai.should();
var add1 = function (x) { return x + 1; };
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
describe('examples', function () {
    it('1', function () {
        var db = {
            '0': { name: 'John', surname: null },
            '1': { name: 'Worf', surname: 'Mercer' }
        };
        var capitalize = function (x) { return x.split('').map(function (y) { return y.toUpperCase(); }).join(''); };
        // without
        var f = function (id) {
            if (id === undefined) {
                return null;
            }
            var item = db[id];
            if (!item) {
                return null;
            }
            var surname = item.surname ? capitalize(item.surname) : '<missing>';
            return item.name + ' ' + surname;
        };
        // with
        var g = function (id) { return Opt_1.opt(id)
            .chainToOpt(function (x) { return db[x]; })
            .map(function (item) { return item.name + ' ' + Opt_1.opt(item.surname).map(capitalize).orElse('<missing>'); })
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