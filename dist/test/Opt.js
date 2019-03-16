import * as chai from 'chai';
import { opt, some, none, fromArray } from '../src/Opt';
const { expect } = chai;
const add1 = (x) => x + 1;
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
        expect(opt(null).map(add1).orUndef()).to.eq(undefined);
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
    it('fromArray', () => {
        expect(fromArray([]).orNull()).to.eq(null);
        expect(fromArray([1]).orNull()).to.eq(1);
        // expect(fromArray([1, 2]).orNull()).to.eq(1);
    });
});
describe('some', () => {
    it('create', () => {
        expect(some(0).orNull()).to.eq(0);
    });
    it('falsy', () => {
        expect(some(null).orUndef()).to.eq(null);
        expect(some(NaN).orUndef()).to.eql(NaN);
        expect(some(undefined).orNull()).to.eq(undefined);
    });
});
//# sourceMappingURL=Opt.js.map