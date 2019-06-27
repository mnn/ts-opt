"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
var someSymbol = Symbol('Some');
var noneSymbol = Symbol('None');
/**
 * @typeparam T Wrapped value type.
 */
var Opt = /** @class */ (function () {
    function Opt() {
    }
    /**
     * Create Opt instance from an array of one or zero items.
     *
     * ```ts
     * Opt.fromArray([]) // None
     * Opt.fromArray([1]) // Some(1)
     * ```
     *
     * @param x
     */
    Opt.fromArray = function (x) { return exports.opt(x[0]); };
    Object.defineProperty(Opt.prototype, "nonEmpty", {
        /**
         * `false` for [[Some]], `true` for [[None]].
         */
        get: function () { return !this.isEmpty; },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Opt.prototype, "length", {
        /**
         * `1` for [[Some]], `0` for [[None]].
         */
        get: function () { return this.isEmpty ? 0 : 1; },
        enumerable: true,
        configurable: true
    });
    /**
     * Alias of [[flatMap]]
     * @param f
     */
    Opt.prototype.chain = function (f) { return this.flatMap(f); };
    /**
     * Combination of [[flatMap]] and [[opt]] functions.
     *
     * ```ts
     * some(1).chainToOpt(x => x === 1 ? null : x + 1) // None
     * some(2).chainToOpt(x => x === 1 ? null : x + 1) // Some(3)
     * ```
     *
     * @param f
     */
    Opt.prototype.chainToOpt = function (f) { return this.flatMap(function (x) { return exports.opt(f(x)); }); };
    /**
     * Applies passed function to this instance and returns function result.
     * Also known as a function application, `|>` or pipe operator.
     *
     * ```ts
     * some(1).pipe(x => x.isEmpty) // false
     * none.pipe(x => x.isEmpty) // true
     * ```
     *
     * @param f
     */
    Opt.prototype.pipe = function (f) { return f(this); };
    return Opt;
}());
exports.Opt = Opt;
var None = /** @class */ (function (_super) {
    __extends(None, _super);
    function None() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this['@@type'] = noneSymbol;
        return _this;
    }
    None.prototype.toArray = function () { return []; };
    Object.defineProperty(None.prototype, "isEmpty", {
        get: function () { return true; },
        enumerable: true,
        configurable: true
    });
    None.prototype.flatMap = function (_f) { return exports.none; };
    None.prototype.map = function () { return exports.none; };
    None.prototype.orCrash = function (msg) { throw new Error(msg); };
    None.prototype.orNull = function () { return null; };
    None.prototype.orUndef = function () { return undefined; };
    None.prototype.orFalse = function () { return false; };
    None.prototype.orTrue = function () { return true; };
    None.prototype.orNaN = function () { return NaN; };
    None.prototype.caseOf = function (_onSome, onNone) {
        return onNone();
    };
    None.prototype.onNone = function (f) { f(); };
    None.prototype.onSome = function (_f) { };
    None.prototype.contains = function (_x) { return false; };
    None.prototype.exists = function (_p) { return false; };
    None.prototype.forAll = function (_p) { return true; };
    None.prototype.orElse = function (def) { return def; };
    None.prototype.orElseOpt = function (def) { return def; };
    None.prototype.bimap = function (_someF, noneF) { return exports.opt(noneF()); };
    None.prototype.flatBimap = function (_someF, noneF) { return noneF(); };
    None.prototype.toString = function () { return 'None'; };
    None.prototype.zip = function (_other) { return exports.none; };
    None.prototype.zip3 = function (_x, _y) { return exports.none; };
    return None;
}(Opt));
var Some = /** @class */ (function (_super) {
    __extends(Some, _super);
    function Some(_value) {
        var _this = _super.call(this) || this;
        _this._value = _value;
        _this['@@type'] = someSymbol;
        return _this;
    }
    Some.prototype.toArray = function () { return [this._value]; };
    Object.defineProperty(Some.prototype, "isEmpty", {
        get: function () { return false; },
        enumerable: true,
        configurable: true
    });
    Some.prototype.flatMap = function (f) {
        return f(this._value);
    };
    Some.prototype.map = function (f) {
        return new Some(f(this._value));
    };
    Some.prototype.orCrash = function (_msg) { return this._value; };
    Some.prototype.orNull = function () { return this._value; };
    Some.prototype.orUndef = function () { return this._value; };
    Some.prototype.orFalse = function () { return this._value; };
    Some.prototype.orTrue = function () { return this._value; };
    Some.prototype.orNaN = function () { return this._value; };
    Some.prototype.caseOf = function (onSome, _onNone) { return onSome(this._value); };
    Some.prototype.contains = function (x) { return this._value === x; };
    Some.prototype.exists = function (p) { return p(this._value); };
    Some.prototype.forAll = function (p) { return p(this._value); };
    Some.prototype.onNone = function (_f) { };
    Some.prototype.onSome = function (f) { f(this._value); };
    Some.prototype.orElse = function (_def) { return this._value; };
    Some.prototype.orElseOpt = function (_def) { return this; };
    Some.prototype.bimap = function (someF, _noneF) { return exports.opt(someF(this._value)); };
    Some.prototype.flatBimap = function (someF, _noneF) { return someF(this._value); };
    Some.prototype.toString = function () { return "Some(" + JSON.stringify(this._value) + ")"; };
    Some.prototype.zip = function (other) {
        if (other.isEmpty) {
            return exports.none;
        }
        return exports.opt([this._value, other.orCrash('bug in isEmpty or orCrash')]);
    };
    Some.prototype.zip3 = function (x, y) {
        if (x.isEmpty || y.isEmpty) {
            return exports.none;
        }
        var _a = [x.orCrash('bug in isEmpty or orCrash'), y.orCrash('bug in isEmpty or orCrash')], xVal = _a[0], yVal = _a[1];
        return exports.opt([this._value, xVal, yVal]);
    };
    return Some;
}(Opt));
var isNoneValue = function (x) {
    return x === undefined || x === null || Number.isNaN(x);
};
/**
 * Single global instance of [[None]].
 */
exports.none = Object.freeze(new None());
/**
 * Constructs [[Some]].
 * Usually it is [[opt]] you are looking for (only in rare cases you want to have for example `Some(undefined)`).
 * @param x
 */
exports.some = function (x) { return Object.freeze(new Some(x)); };
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
exports.opt = function (x) { return isNoneValue(x) ? exports.none : new Some(x); };
/**
 * For falsy values returns [[None]].
 * @param x
 */
exports.optFalsy = function (x) { return x ? new Some(x) : exports.none; };
/**
 * For empty array (`[]`) returns [[None]].
 * @param x
 */
exports.optEmptyArray = function (x) { return x.length ? new Some(x) : exports.none; };
/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
exports.isOpt = function (x) { return x instanceof Opt; };
//# sourceMappingURL=Opt.js.map