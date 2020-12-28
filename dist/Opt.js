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
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.mapOpt = exports.catOpts = exports.apFn = exports.ap = exports.isOpt = exports.optZero = exports.optEmptyString = exports.optEmptyObject = exports.optEmptyArray = exports.optFalsy = exports.opt = exports.some = exports.none = exports.ReduxDevtoolsCompatibilityHelper = exports.Opt = void 0;
var someSymbol = Symbol('Some');
var noneSymbol = Symbol('None');
var refCmp = function (a, b) { return a === b; };
/**
 * @typeparam T Wrapped value type.
 */
var Opt = /** @class */ (function () {
    function Opt() {
    }
    Object.defineProperty(Opt.prototype, "nonEmpty", {
        /**
         * `false` for [[Some]], `true` for [[None]].
         */
        get: function () { return !this.isEmpty; },
        enumerable: false,
        configurable: true
    });
    /**
     * Is this instance of [[Some]]?
     */
    Opt.prototype.isSome = function () { return this.nonEmpty; };
    /**
     * Is this instance of [[None]]?
     */
    Opt.prototype.isNone = function () { return this.isEmpty; };
    Object.defineProperty(Opt.prototype, "length", {
        /**
         * `1` for [[Some]], `0` for [[None]].
         */
        get: function () { return this.isEmpty ? 0 : 1; },
        enumerable: false,
        configurable: true
    });
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
    /**
     * Returns [[None]] if predicate holds, otherwise passes same instance of [[Opt]].
     * ```ts
     * opt(1).noneIf(x => x > 0); // None
     * opt(-1).noneIf(x => x > 0); // Some(-1)
     * ```
     * @see [[filter]]
     * @param predicate
     */
    Opt.prototype.noneIf = function (predicate) {
        return this.filter(function (x) { return !predicate(x); });
    };
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
    Object.defineProperty(None.prototype, "isEmpty", {
        get: function () { return true; },
        enumerable: false,
        configurable: true
    });
    None.prototype.toArray = function () { return []; };
    None.prototype.flatMap = function (_f) { return exports.none; };
    None.prototype.map = function () { return exports.none; };
    None.prototype.orCrash = function (msg) { throw new Error(msg); };
    None.prototype.optOrCrash = function (msg) { throw new Error(msg); };
    None.prototype.orNull = function () { return null; };
    None.prototype.orUndef = function () { return undefined; };
    None.prototype.orFalse = function () { return false; };
    None.prototype.orTrue = function () { return true; };
    None.prototype.orNaN = function () { return NaN; };
    None.prototype.caseOf = function (_onSome, onNone) {
        return onNone();
    };
    None.prototype.onNone = function (f) {
        f();
        return this;
    };
    None.prototype.onSome = function (_f) { return this; };
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
    None.prototype.zip4 = function (_x, _y, _z) { return exports.none; };
    None.prototype.zip5 = function (_x, _y, _z, _zz) { return exports.none; };
    None.prototype.filter = function (_predicate) { return exports.none; };
    None.prototype.narrow = function (_guard) { return this; };
    None.prototype.print = function (tag) {
        // tslint:disable-next-line:no-console
        console.log.apply(console, __spreadArrays(exports.opt(tag).map(function (x) { return ["[" + x + "]"]; }).orElse([]), ['None']));
        return this;
    };
    None.prototype.equals = function (other, _comparator) {
        if (_comparator === void 0) { _comparator = refCmp; }
        return other.isEmpty;
    };
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
    Object.defineProperty(Some.prototype, "isEmpty", {
        get: function () { return false; },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Some.prototype, "value", {
        get: function () { return this._value; },
        enumerable: false,
        configurable: true
    });
    Some.prototype.toArray = function () { return [this._value]; };
    Some.prototype.flatMap = function (f) {
        return f(this._value);
    };
    Some.prototype.map = function (f) {
        return new Some(f(this._value));
    };
    Some.prototype.orCrash = function (_msg) { return this._value; };
    Some.prototype.optOrCrash = function (_msg) { return this; };
    Some.prototype.orNull = function () { return this._value; };
    Some.prototype.orUndef = function () { return this._value; };
    Some.prototype.orFalse = function () { return this._value; };
    Some.prototype.orTrue = function () { return this._value; };
    Some.prototype.orNaN = function () { return this._value; };
    Some.prototype.caseOf = function (onSome, _onNone) { return onSome(this._value); };
    Some.prototype.contains = function (x) { return this._value === x; };
    Some.prototype.exists = function (p) { return p(this._value); };
    Some.prototype.forAll = function (p) { return p(this._value); };
    Some.prototype.onNone = function (_f) { return this; };
    Some.prototype.onSome = function (f) {
        f(this._value);
        return this;
    };
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
    Some.prototype.zip4 = function (x, y, z) {
        var args = [x, y, z];
        if (args.some(function (a) { return a.isEmpty; })) {
            return exports.none;
        }
        var _a = args.map(function (a) { return a.orCrash('bug in isEmpty or orCrash'); }), xVal = _a[0], yVal = _a[1], zVal = _a[2];
        return exports.opt([this._value, xVal, yVal, zVal]);
    };
    Some.prototype.zip5 = function (x, y, z, zz) {
        var args = [x, y, z, zz];
        if (args.some(function (a) { return a.isEmpty; })) {
            return exports.none;
        }
        var _a = args.map(function (a) { return a.orCrash('bug in isEmpty or orCrash'); }), xVal = _a[0], yVal = _a[1], zVal = _a[2], zzVal = _a[3];
        return exports.opt([this._value, xVal, yVal, zVal, zzVal]);
    };
    Some.prototype.filter = function (predicate) { return predicate(this._value) ? this : exports.none; };
    Some.prototype.narrow = function (guard) {
        return guard(this._value) ? this : exports.none;
    };
    Some.prototype.print = function (tag) {
        // tslint:disable-next-line:no-console
        console.log.apply(console, __spreadArrays(exports.opt(tag).map(function (x) { return ["[" + x + "]"]; }).orElse([]), ['Some:', this._value]));
        return this;
    };
    Some.prototype.equals = function (other, comparator) {
        if (comparator === void 0) { comparator = refCmp; }
        if (other.isEmpty) {
            return false;
        }
        return comparator(this._value, other.orCrash('Some expected'));
    };
    return Some;
}(Opt));
var someSerializedType = 'Opt/Some';
var noneSerializedType = 'Opt/None';
var ReduxDevtoolsCompatibilityHelper = /** @class */ (function () {
    function ReduxDevtoolsCompatibilityHelper() {
    }
    ReduxDevtoolsCompatibilityHelper.replacer = function (_key, value) {
        if (exports.isOpt(value)) {
            var res = value.isEmpty ? { type: noneSerializedType } : {
                type: someSerializedType,
                value: value.orCrash('failed to extract value from Some'),
            };
            return res;
        }
        else {
            return value;
        }
    };
    ReduxDevtoolsCompatibilityHelper.reviver = function (_key, value) {
        if (!value || typeof value !== 'object') {
            return value;
        }
        switch (value.type) {
            case noneSerializedType:
                return exports.none;
            case someSerializedType:
                return exports.some(value.value);
            default:
                return value;
        }
    };
    return ReduxDevtoolsCompatibilityHelper;
}());
exports.ReduxDevtoolsCompatibilityHelper = ReduxDevtoolsCompatibilityHelper;
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
 * For falsy values returns [[None]], otherwise acts same as [[opt]].
 * ```ts
 * optFalsy(''); // None
 * optFalsy(0); // None
 * optFalsy(false); // None
 * optFalsy(NaN); // None
 * ```
 * @param x
 */
exports.optFalsy = function (x) { return x ? new Some(x) : exports.none; };
/**
 * For empty array (`[]`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
exports.optEmptyArray = function (x) { return exports.opt(x).filter(function (y) { return y.length > 0; }); };
/**
 * For empty object (`{}`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
exports.optEmptyObject = function (x) {
    return exports.opt(x).filter(function (y) { return Object.keys(y).length !== 0; });
};
/**
 * For empty string (`''`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
exports.optEmptyString = function (x) { return x === '' ? exports.none : exports.opt(x); };
/**
 * For a number `0` returns [[None]], otherwise acts same as [[opt]].
 */
exports.optZero = function (x) { return x === 0 ? exports.none : exports.opt(x); };
/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
exports.isOpt = function (x) { return x instanceof Opt; };
/**
 * ```ts
 * <A, B>(of: Opt<(_: A) => B>) => (oa: Opt<A>): Opt<B>
 * ```
 * Apply `oa` to function `of`. If any argument is [[None]] then result is [[None]].
 * ```ts
 * ap(opt(x => x > 0))(opt(1)) // Opt(true)
 * ap(opt(x => x > 0))(none) // None
 * ap(none)(opt(1)) // None
 * ap(none)(none) // None
 * ```
 * @typeparam A input of function inside `of`
 * @typeparam B output of function inside `of`
 */
exports.ap = function (of) { return function (oa) {
    return oa.caseOf(function (a) { return of.map(function (f) { return f(a); }); }, function () { return exports.none; });
}; };
/**
 * ```ts
 * <A, B>(f: (_: A) => B) => (oa: Opt<A>): Opt<B>
 * ```
 * Apply `oa` to function `f`. If argument is [[None]] then result is [[None]].
 * ```ts
 * apFn(x => x > 0)(opt(1)) // Opt(true)
 * apFn(x => x > 0)(none) // None
 * ```
 * @typeparam A input of function `f`
 * @typeparam B output of function `f`
 */
exports.apFn = function (f) { return function (oa) { return exports.ap(exports.opt(f))(oa); }; };
/**
 * Transforms array of opts into an array where [[None]]s are omitted and [[Some]]s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
exports.catOpts = function (xs) {
    return xs.reduce(function (acc, x) { return x.caseOf(function (y) { return __spreadArrays(acc, [y]); }, function () { return acc; }); }, []);
};
/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
exports.mapOpt = function (f) { return function (xs) { return exports.catOpts(xs.map(f)); }; };
//# sourceMappingURL=Opt.js.map