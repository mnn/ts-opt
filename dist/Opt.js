"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.zip4 = exports.zip3 = exports.zip = exports.bimap = exports.orElseOpt = exports.orElse = exports.forAll = exports.exists = exports.contains = exports.pipe = exports.onBoth = exports.caseOf = exports.orNaN = exports.orTrue = exports.orFalse = exports.orNull = exports.orUndef = exports.orCrash = exports.someOrCrash = exports.chainToOptFlow = exports.actToOpt = exports.chainToOpt = exports.chainFlow = exports.act = exports.chain = exports.flatMap = exports.mapFlow = exports.map = exports.toArray = exports.fromArray = exports.joinOpt = exports.mapOpt = exports.catOpts = exports.apFn = exports.ap = exports.isOpt = exports.optNegative = exports.optZero = exports.optEmptyString = exports.optEmptyObject = exports.optEmptyArray = exports.optFalsy = exports.opt = exports.some = exports.none = exports.ReduxDevtoolsCompatibilityHelper = exports.Opt = exports.isArray = exports.toString = exports.isString = void 0;
exports.parseInt = exports.parseJson = exports.tryRun = exports.testReOrFalse = exports.testRe = exports.zipToOptArray = exports.last = exports.head = exports.at = exports.id = exports.nonEmpty = exports.isEmpty = exports.uncurryTuple5 = exports.uncurryTuple4 = exports.uncurryTuple3 = exports.uncurryTuple = exports.curryTuple5 = exports.curryTuple4 = exports.curryTuple3 = exports.curryTuple = exports.compose = exports.flow = exports.swap = exports.prop = exports.equals = exports.print = exports.narrow = exports.count = exports.filter = exports.zip5 = void 0;
var someSymbol = Symbol('Some');
var noneSymbol = Symbol('None');
var errorSymbol = Symbol('Error');
var refCmp = function (a, b) { return a === b; };
/* istanbul ignore next */
var OperationNotAvailable = /** @class */ (function () {
    function OperationNotAvailable() {
        this['@@type'] = errorSymbol;
    }
    return OperationNotAvailable;
}());
var isString = function (x) { return typeof x === 'string'; };
exports.isString = isString;
var toString = function (x) { return x.toString(); };
exports.toString = toString;
var isArray = function (x) { return Array.isArray(x); };
exports.isArray = isArray;
var debugPrint = function (tag) {
    var xs = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        xs[_i - 1] = arguments[_i];
    }
    // tslint:disable-next-line:no-console
    console.log.apply(console, __spreadArray(__spreadArray([], exports.opt(tag).map(function (x) { return ["[" + x + "]"]; }).orElse([])), xs));
};
/**
 * Generic container class. It either holds exactly one value - [[Some]], or no value - [[None]] (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent API" in OOP or [[pipe|chain of reverse applications]] in FP).
 *
 * @typeparam T Wrapped value type.
 */
var Opt = /** @class */ (function () {
    function Opt() {
        var _this = this;
        /**
         * Similar to [[map]], but supports more functions which are called in succession, each on a result of a previous one.
         *
         * ```
         * const sq = (x: number) => x * x;
         * const dec = (x: number) => x - 1;
         * opt(4).mapFlow(sq, dec) // Some(15)
         * opt(null).mapFlow(sq, dec) // None
         * ```
         * @param fs
         */
        this.mapFlow = function () {
            var fs = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                fs[_i] = arguments[_i];
            }
            return fs.reduce(function (acc, x) { return acc.map(x); }, _this);
        };
        /**
         * Similar to [[chain]] (in other languages called `bind` or `>>=`), but supports more functions passed at once (resembles `do` notation in Haskell).
         * It is used to model a sequence of operations where each operation can fail (can return [[None]]).
         *
         * ```ts
         * // does addition when first argument is number
         * const f1 =
         *   (x: string | number) => (y: number) => opt(x).narrow(isNumber).map(z => z + y);
         * // passes only even numbers
         * const f2 = (x: number): Opt<number> => x % 2 === 0 ? opt(x) : none;
         *
         * opt(0).act( // Some(0)
         *   f1(-2), // Some(-2)
         *   f2, // Some(-2)
         *   optNegative, // None
         * ); // None
         *
         * opt(0).act( // Some(0)
         *   f1(1), // Some(1)
         *   f2, // None
         *   optNegative, // won't get called, still None
         * ); // None
         *
         * opt(3).act( // Some(3)
         *   f1(1), // Some(4)
         *   f2, // Some(4)
         *   optNegative, // Some(4)
         * ); // Some(4)
         * ```
         *
         * @param fs
         */
        this.act = function () {
            var fs = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                fs[_i] = arguments[_i];
            }
            return fs.reduce(function (acc, x) { return acc.chain(x); }, _this);
        };
        /**
         * Alias of [[act]]
         * @param args
         */
        this.chainFlow = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            return _this.act.apply(_this, args);
        };
        /**
         * Similar to [[act]], but functions return empty values instead of [[Opt]].
         * It is useful for typical JavaScript functions (e.g. lodash), properly handles `undefined`/`null`/`NaN` at any point of the chain.
         *
         * ```ts
         * const data = [{}, {f: true, a: [{b: 7, c: 1}]}, {a: [{}]}];
         * opt(data).actToOpt(
         *   find(x => Boolean(x?.f)), // {f: true, a: [{b: 7, c: 1}]}
         *   x => x?.a, // [{b: 7, c: 1}]
         *   find(x => x.b === 8) // undefined
         * ); // None
         * ```
         *
         * @param fs
         */
        this.actToOpt = function () {
            var fs = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                fs[_i] = arguments[_i];
            }
            return fs.reduce(function (acc, x) { return acc.chainToOpt(x); }, _this);
        };
        /**
         * Alias of [[actToOpt]].
         * @param args
         */
        this.chainToOptFlow = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            return _this.act.apply(_this, args);
        };
        /**
         * Applies passed function to this instance and returns function result.
         * Also known as a reverse function application, `|>` (Reason/ReScript, F#, OCaml), `&` (Haskell), `#` (PureScript) or a pipe operator.
         *
         * ```ts
         * some(1).pipe(x => x.isEmpty) // false
         * none.pipe(x => x.isEmpty) // true
         * ```
         *
         * Supports multiple functions.
         *
         * ```ts
         * opt(1).pipe( // Some(1)
         *   x => x.isEmpty, // false
         *   x => !x, // true
         * ) // true
         * ```
         *
         * @param fs Functions in call chain
         */
        this.pipe = function () {
            var fs = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                fs[_i] = arguments[_i];
            }
            return fs.reduce(function (acc, x) { return x(acc); }, _this);
        };
        /**
         * Constructs a function which returns a value for [[Some]] or an empty value for [[None]] (default is `null`).
         * Optionally takes an empty value as a parameter.
         *
         * ```ts
         * opt(1).const()() // 1
         * opt(undefined).const()() // null
         *
         * // custom empty value
         * opt(NaN).const(undefined)() // undefined
         * ```
         */
        this.const = function () {
            var _this = this;
            if (arguments.length === 1) {
                var e_1 = arguments[0];
                return function () { return _this.isSome() ? _this.value : e_1; };
            }
            return function () { return _this.orNull(); };
        };
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
     * Returns [[None]] if predicate holds, otherwise passes same instance of [[Opt]].
     *
     * @example
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
    /**
     * Returns `0` or `1` for [[Some]] depending on whether the predicate holds.
     * Returns `0` for [[None]].
     *
     * It is a combination of [[Opt.filter]] and [[Opt.length]].
     *
     * @example
     * ```ts
     * opt('Mu').count(x => x.length > 3) // 0
     * opt('Ichi').count(x => x.length > 3) // 1
     * ```
     *
     * @see [[count]]
     * @param predicate
     */
    Opt.prototype.count = function (predicate) {
        return this.filter(predicate).length;
    };
    /**
     * Widen union (typically union of strings to string).
     * Experimental. May be removed if it is later found out it's unsafe and unfixable.
     * ```ts
     * opt(someValueOfUnionType).widen<SuperOfThatUnion>() // :Opt<SuperOfThatUnion>
     * ```
     */
    Opt.prototype.widen = function () {
        return this;
    };
    /**
     * Get a first item of an array.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).head() // Some(1)
     * opt([]).head() // None
     * opt(null).head() // None
     * ```
     */
    Opt.prototype.head = function () { return this.at(0); };
    /**
     * Get a last item of an array.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).last() // Some(3)
     * opt([]).last() // None
     * opt(null).last() // None
     * ```
     */
    Opt.prototype.last = function () { return this.at(-1); };
    /**
     * A convenience function to test this (`Opt<string>`) against a given regular expression.
     *
     * @example
     * ```ts
     * opt('a').testReOrFalse(/a/) // true
     * opt('b').testReOrFalse(/a/)) // false;
     * ```
     *
     * @param re Regular expression
     */
    Opt.prototype.testReOrFalse = function (re) {
        return this.narrow(exports.isString).someOrCrash("testReOrFalse only works on Opt<string>").map(exports.testRe(re)).orFalse();
    };
    Object.defineProperty(Opt.prototype, "end", {
        /**
         * No-op terminator used to end imperative chains.
         *
         * @example
         * ```ts
         * const f = (x: unknown): void => opt(x).onBoth(noop, noop).end;
         * // same as
         * const g = (x: unknown): void => { opt(x).onBoth(noop, noop); };
         * ```
         */
        get: function () { return undefined; },
        enumerable: false,
        configurable: true
    });
    return Opt;
}());
exports.Opt = Opt;
/**
 * Empty [[Opt]].
 * @notExported
 * @see [[Opt]]
 */
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
    /**
     * @deprecated Please use [[someOrCrash]] instead
     */
    None.prototype.optOrCrash = function (msg) { throw new Error(msg); };
    None.prototype.someOrCrash = function (msg) { throw new Error(msg); };
    None.prototype.orNull = function () { return null; };
    None.prototype.orUndef = function () { return undefined; };
    None.prototype.orFalse = function () { return false; };
    None.prototype.orTrue = function () { return true; };
    None.prototype.orNaN = function () { return NaN; };
    None.prototype.caseOf = function (_onSome, onNone) {
        return onNone();
    };
    None.prototype.onBoth = function (_onSome, onNone) {
        onNone();
        return this;
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
        debugPrint(tag, 'None');
        return this;
    };
    None.prototype.equals = function (other, _comparator) {
        if (_comparator === void 0) { _comparator = refCmp; }
        return other.isEmpty;
    };
    None.prototype.prop = function (_key) { return exports.none; };
    None.prototype.swap = function (_newVal) {
        return exports.none;
    };
    None.prototype.at = function (_index) {
        return exports.none;
    };
    return None;
}(Opt));
/**
 * [[Opt]] with a value inside.
 * @notExported
 * @see [[Opt]]
 */
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
        return exports.some(f(this._value));
    };
    Some.prototype.orCrash = function (_msg) { return this._value; };
    /**
     * @deprecated Please use [[someOrCrash]] instead
     */
    Some.prototype.optOrCrash = function (_msg) { return this; };
    Some.prototype.someOrCrash = function (_msg) { return this; };
    Some.prototype.orNull = function () { return this._value; };
    Some.prototype.orUndef = function () { return this._value; };
    Some.prototype.orFalse = function () { return this._value; };
    Some.prototype.orTrue = function () { return this._value; };
    Some.prototype.orNaN = function () { return this._value; };
    Some.prototype.caseOf = function (onSome, _onNone) { return onSome(this._value); };
    Some.prototype.onBoth = function (onSome, _onNone) {
        onSome(this._value);
        return this;
    };
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
        debugPrint(tag, 'Some:', this._value);
        return this;
    };
    Some.prototype.equals = function (other, comparator) {
        if (comparator === void 0) { comparator = refCmp; }
        if (other.isEmpty) {
            return false;
        }
        return comparator(this._value, other.orCrash('Some expected'));
    };
    Some.prototype.prop = function (key) { return exports.opt(this._value[key]); };
    Some.prototype.swap = function (newVal) {
        return exports.some(newVal);
    };
    Some.prototype.at = function (index) {
        var val = this._value;
        if (Array.isArray(val)) {
            var processedIndex = (index < 0 ? val.length : 0) + index;
            return exports.opt(val[processedIndex]);
        }
        else {
            throw new Error("`Opt#at` can only be used on arrays");
        }
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
var some = function (x) { return Object.freeze(new Some(x)); };
exports.some = some;
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @param x
 */
var opt = function (x) { return isNoneValue(x) ? exports.none : exports.some(x); };
exports.opt = opt;
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
var optFalsy = function (x) { return x ? exports.some(x) : exports.none; };
exports.optFalsy = optFalsy;
/**
 * For empty array (`[]`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
var optEmptyArray = function (x) { return exports.opt(x).filter(function (y) { return y.length > 0; }); };
exports.optEmptyArray = optEmptyArray;
/**
 * For empty object (`{}`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
var optEmptyObject = function (x) {
    return exports.opt(x).filter(function (y) { return Object.keys(y).length !== 0; });
};
exports.optEmptyObject = optEmptyObject;
/**
 * For empty string (`''`) returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
var optEmptyString = function (x) { return x === '' ? exports.none : exports.opt(x); };
exports.optEmptyString = optEmptyString;
/**
 * For a number `0` returns [[None]], otherwise acts same as [[opt]].
 * @param x
 */
var optZero = function (x) { return x === 0 ? exports.none : exports.opt(x); };
exports.optZero = optZero;
/**
 * For numbers lesser than `0` returns [[None]], otherwise acts same as [[opt]].
 * Useful for strange functions which return `-1` or other negative numbers on failure.
 * ```ts
 * optNegative(undefined) // None
 * optNegative(1) // Some(1)
 * optNegative(0) // Some(0)
 * optNegative(-1) // None
 * ```
 * @param x
 */
var optNegative = function (x) { return typeof x === 'number' && x < 0 ? exports.none : exports.opt(x); };
exports.optNegative = optNegative;
/**
 * Is given value an instance of [[Opt]]?
 * @param x
 */
var isOpt = function (x) { return x instanceof Opt; };
exports.isOpt = isOpt;
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
var ap = function (of) { return function (oa) {
    return oa.caseOf(function (a) { return of.map(function (f) { return f(a); }); }, function () { return exports.none; });
}; };
exports.ap = ap;
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
var apFn = function (f) { return function (oa) { return exports.ap(exports.opt(f))(oa); }; };
exports.apFn = apFn;
/**
 * Transforms array of opts into an array where [[None]]s are omitted and [[Some]]s are unwrapped.
 * ```ts
 * catOpts([opt(1), opt(null)]) // [1]
 * ```
 * @param xs
 */
var catOpts = function (xs) {
    return xs.reduce(function (acc, x) { return x.caseOf(function (y) { return __spreadArray(__spreadArray([], acc), [y]); }, function () { return acc; }); }, []);
};
exports.catOpts = catOpts;
/**
 * Similar to `Array.map`, but also allows omitting elements.
 * ```ts
 * mapOpt((x: number) => x > 0 ? opt(x) : none)([-1, 0, 1]) // [1]
 * ```
 * @param f
 */
var mapOpt = function (f) { return function (xs) { return exports.catOpts(xs.map(f)); }; };
exports.mapOpt = mapOpt;
/**
 * Unwraps one level of nested [[Opt]]s. Similar to `flatten` in other libraries or languages.
 * ```ts
 * joinOpt(some(none)) // None
 * joinOpt(some(some(1))) // Some(1)
 * ```
 * @param x
 */
var joinOpt = function (x) { return x.caseOf(function (y) { return y; }, function () { return exports.none; }); };
exports.joinOpt = joinOpt;
/**
 * @see [[Opt.fromArray]]
 */
exports.fromArray = Opt.fromArray;
/**
 * @see [[Opt.toArray]]
 */
var toArray = function (x) { return x.toArray(); };
exports.toArray = toArray;
/**
 * Same as [[Opt.map]], but also supports arrays.
 * @see [[Opt.map]]
 */
var map = function (f) { return function (x) { return x.map(f); }; };
exports.map = map;
/** @see [[Opt.mapFlow]] */
var mapFlow = function () {
    var fs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        fs[_i] = arguments[_i];
    }
    return function (x) { return fs.reduce(function (acc, x) { return acc.map(x); }, x); };
};
exports.mapFlow = mapFlow;
/**
 * Same as [[Opt.flatMap]], but also supports arrays.
 * @see [[Opt.flatMap]]
 */
var flatMap = function (f) { return function (x) { return exports.isOpt(x) ? x.flatMap(f) : x.map(f).flat(); }; };
exports.flatMap = flatMap;
/** @see [[Opt.flatMap]] */
exports.chain = exports.flatMap;
/** @see [[Opt.act]] */
var act = function () {
    var fs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        fs[_i] = arguments[_i];
    }
    return function (x) { return fs.reduce(function (acc, x) { return acc.chain(x); }, x); };
};
exports.act = act;
/** @see [[Opt.chainFlow]] */
exports.chainFlow = exports.act;
/** @see [[Opt.chainToOpt]] */
var chainToOpt = function (f) { return function (x) { return x.chainToOpt(f); }; };
exports.chainToOpt = chainToOpt;
/** @see [[Opt.actToOpt]] */
var actToOpt = function () {
    var fs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        fs[_i] = arguments[_i];
    }
    return function (x) { return fs.reduce(function (acc, x) { return acc.chainToOpt(x); }, x); };
};
exports.actToOpt = actToOpt;
/** @see [[Opt.chainToOptFlow]] */
exports.chainToOptFlow = exports.actToOpt;
/** @see [[Opt.someOrCrash]] */
var someOrCrash = function (msg) { return function (x) { return x.someOrCrash(msg); }; };
exports.someOrCrash = someOrCrash;
/** @see [[Opt.orCrash]] */
var orCrash = function (msg) { return function (x) { return x.orCrash(msg); }; };
exports.orCrash = orCrash;
/** @see [[Opt.orUndef]] */
var orUndef = function (x) { return x.orUndef(); };
exports.orUndef = orUndef;
/** @see [[Opt.orNull]] */
var orNull = function (x) { return x.orNull(); };
exports.orNull = orNull;
/** @see [[Opt.orFalse]] */
var orFalse = function (x) { return x.orFalse(); };
exports.orFalse = orFalse;
/** @see [[Opt.orTrue]] */
var orTrue = function (x) { return x.orTrue(); };
exports.orTrue = orTrue;
/** @see [[Opt.orNaN]] */
var orNaN = function (x) { return x.orNaN(); };
exports.orNaN = orNaN;
/** @see [[Opt.caseOf]] */
var caseOf = function (onSome) { return function (onNone) { return function (x) { return x.caseOf(onSome, onNone); }; }; };
exports.caseOf = caseOf;
/** @see [[Opt.onBoth]] */
var onBoth = function (onSome) { return function (onNone) { return function (x) { return x.onBoth(onSome, onNone); }; }; };
exports.onBoth = onBoth;
/**
 * Similar to [[Opt.pipe]], but the first argument is the input.
 * Supports arbitrary input type, not just [[Opt]].
 * @see [[Opt.pipe]]
 */
var pipe = function (x) {
    var fs = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        fs[_i - 1] = arguments[_i];
    }
    return fs.reduce(function (acc, y) { return y(acc); }, x);
};
exports.pipe = pipe;
/** @see [[Opt.contains]] */
var contains = function (y) { return function (x) { return x.contains(y); }; };
exports.contains = contains;
/** @see [[Opt.exists]] */
var exists = function (y) { return function (x) { return x.exists(y); }; };
exports.exists = exists;
/** @see [[Opt.forAll]] */
var forAll = function (p) { return function (x) { return x.forAll(p); }; };
exports.forAll = forAll;
/** @see [[Opt.orElse]] */
var orElse = function (e) { return function (x) { return x.orElse(e); }; };
exports.orElse = orElse;
/** @see [[Opt.orElseOpt]] */
var orElseOpt = function (def) { return function (x) { return x.orElseOpt(def); }; };
exports.orElseOpt = orElseOpt;
/** @see [[Opt.bimap]] */
var bimap = function (someF) { return function (noneF) { return function (x) { return x.bimap(someF, noneF); }; }; };
exports.bimap = bimap;
var zipArray = function (a, b) { return __spreadArray([], Array(Math.min(b.length, a.length))).map(function (_, i) { return [a[i], b[i]]; }); };
/**
 * Same as [[Opt.zip]], but also supports arrays.
 *
 * @example
 * ```ts
 * const formatAddress =
 *   (streetName?: string, streetNumber?: string): string =>
 *     zip(opt(streetName))(opt(streetNumber)).map(join(' ')).orElse('');
 * formatAddress('Strawberry', '12') // 'Strawberry 12'
 * formatAddress('Strawberry', undefined) // ''
 * formatAddress(undefined, '12') // ''
 * formatAddress(undefined, undefined) // ''
 * ```
 *
 * @see [[Opt.zip]]
 */
var zip = function (x) { return function (other) { return exports.isOpt(x) ? x.zip(other) : zipArray(x, other); }; };
exports.zip = zip;
/** @see [[Opt.zip3]] */
var zip3 = function (x) { return function (a) { return function (b) { return x.zip3(a, b); }; }; };
exports.zip3 = zip3;
/** @see [[Opt.zip4]] */
var zip4 = function (x) { return function (a) { return function (b) { return function (c) { return x.zip4(a, b, c); }; }; }; };
exports.zip4 = zip4;
/** @see [[Opt.zip5]] */
var zip5 = function (x) { return function (a) { return function (b) { return function (c) { return function (d) {
    return x.zip5(a, b, c, d);
}; }; }; }; };
exports.zip5 = zip5;
/**
 * Same as [[Opt.filter]], but also supports arrays.
 * @see [[Opt.filter]]
 */
var filter = function (p) { return function (x) { return x.filter(p); }; };
exports.filter = filter;
/**
 * Same as [[Opt.count]], but also supports arrays.
 *
 * @example
 * ```ts
 * const greaterThanZero = (x: number) => x > 0;
 *
 * count(greaterThanZero)([-3, 0, 5, 10]) // 2
 * ```
 *
 * @see [[Opt.count]]
 */
var count = function (p) { return function (x) {
    if (exports.isOpt(x)) {
        return x.count(p);
    }
    if (exports.isArray(x)) {
        return x.filter(p).length;
    }
    throw new Error("Invalid input to count, only Opt and Array are supported: " + JSON.stringify(x));
}; };
exports.count = count;
/** @see [[Opt.narrow]] */
var narrow = function (guard) { return function (x) { return x.narrow(guard); }; };
exports.narrow = narrow;
/**
 * Same as [[Opt.print]], but supports arbitrary argument types.
 * @see [[Opt.print]]
 */
var print = function (tag) { return function (x) {
    if (exports.isOpt(x)) {
        x.print(tag);
    }
    else {
        debugPrint(tag, x);
    }
    return x;
}; };
exports.print = print;
/** @see [[Opt.equals]] */
var equals = function (other, comparator) {
    if (comparator === void 0) { comparator = refCmp; }
    return function (x) {
        return x.equals(other, comparator);
    };
};
exports.equals = equals;
/** @see [[Opt.prop]] */
var prop = function (key) { return function (x) {
    return x.prop(key);
}; };
exports.prop = prop;
/** @see [[Opt.swap]] */
var swap = function (newValue) { return function (x) { return x.swap(newValue); }; };
exports.swap = swap;
/**
 * Takes functions and builds a function which consecutively calls each given function with a result from a previous one.
 * Similar to [[Opt.pipe]], but doesn't take input directly, instead returns a function which can be called repeatedly with different inputs.
 *
 * ```ts
 * flow( // 1. 63
 *   add1, // 2. 64
 *   Math.sqrt, // 3. 8
 * )(63), // 4. 8
 *
 * // gives same result as
 * Math.sqrt(add1(63)) // 8
 * ```
 *
 * ```ts
 * const f = flow(add1, Math.sqrt); // (_: number) => number
 * f(63); // 8
 * f(3);  // 2
 * ```
 *
 * @param fs
 */
var flow = function () {
    var fs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        fs[_i] = arguments[_i];
    }
    return function (x) { return fs.reduce(function (acc, x) { return x(acc); }, x); };
};
exports.flow = flow;
/**
 * Composes given functions (in the mathematical sense).
 *
 * Unlike [[flow]] and [[pipe]], functions passed to [[compose]] are applied (called) from last to first.
 *
 * ```ts
 * const f = (x: number): number => x * x;
 * const g = (x: number): string => x.toFixed();
 * const h = (x: string): boolean => x === '4';
 *
 * compose(
 *   h, // 3. true
 *   g, // 2. 4
 *   f, // 1. 2
 * )(2) // 4. true
 *
 * // gives same result as
 * h(g(f(2))) // true
 * ```
 *
 * @param fs
 */
var compose = function () {
    var fs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        fs[_i] = arguments[_i];
    }
    return function (x) { return fs.reduceRight(function (acc, x) { return x(acc); }, x); };
};
exports.compose = compose;
/**
 * Transforms the given function of two arguments from "tuple curried" format to curried one.
 *
 * ```ts
 * const addPair = ([a, b]: [number, number]) => a + b;
 * opt(1) // Some(1)
 *   .map(
 *     curryTuple(addPair)(4) // same as `(a => b => a + b)(4)`
 *   ) // Some(5)
 * ```
 *
 * @see [[uncurryTuple]]
 * @param f
 */
var curryTuple = function (f) { return function (a) { return function (b) { return f([a, b]); }; }; };
exports.curryTuple = curryTuple;
/**
 * Transforms the given function of three arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
var curryTuple3 = function (f) { return function (a) { return function (b) { return function (c) { return f([a, b, c]); }; }; }; };
exports.curryTuple3 = curryTuple3;
/**
 * Transforms the given function of four arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
var curryTuple4 = function (f) { return function (a) { return function (b) { return function (c) { return function (d) { return f([a, b, c, d]); }; }; }; }; };
exports.curryTuple4 = curryTuple4;
/**
 * Transforms the given function of five arguments from "tuple curried" format to curried one.
 * @see [[curryTuple]]
 * @param f
 */
var curryTuple5 = function (f) { return function (a) { return function (b) { return function (c) { return function (d) { return function (e) { return f([a, b, c, d, e]); }; }; }; }; }; };
exports.curryTuple5 = curryTuple5;
/**
 * Transforms the given function of two arguments from curried format to "tuple curried" which can be used with [[Opt.zip]].
 *
 * ```ts
 * const sub = (x: number) => (y: number) => x - y;
 * opt(4) // Some(4)
 *   .zip(opt(1)) // Some([4, 1])
 *   .map(uncurryTuple(sub)) // Some(3)
 * ```
 *
 * @see [[curryTuple]]
 * @param f
 */
var uncurryTuple = function (f) { return function (_a) {
    var a = _a[0], b = _a[1];
    return f(a)(b);
}; };
exports.uncurryTuple = uncurryTuple;
/**
 * Transforms the given function of three arguments from curried format to "tuple curried" which can be used with [[Opt.zip3]].
 * @see [[uncurryTuple]]
 * @param f
 */
var uncurryTuple3 = function (f) { return function (_a) {
    var a = _a[0], b = _a[1], c = _a[2];
    return f(a)(b)(c);
}; };
exports.uncurryTuple3 = uncurryTuple3;
/**
 * Transforms the given function of four arguments from curried format to "tuple curried" which can be used with [[Opt.zip4]].
 * @see [[uncurryTuple]]
 * @param f
 */
var uncurryTuple4 = function (f) { return function (_a) {
    var a = _a[0], b = _a[1], c = _a[2], d = _a[3];
    return f(a)(b)(c)(d);
}; };
exports.uncurryTuple4 = uncurryTuple4;
/**
 * Transforms the given function of five arguments from curried format to "tuple curried" which can be used with [[Opt.zip5]].
 * @see [[uncurryTuple]]
 * @param f
 */
var uncurryTuple5 = function (f) { return function (_a) {
    var a = _a[0], b = _a[1], c = _a[2], d = _a[3], e = _a[4];
    return f(a)(b)(c)(d)(e);
}; };
exports.uncurryTuple5 = uncurryTuple5;
/**
 * Similar to `isEmpty` from lodash, but also supports [[Opt]]s.
 * Returns `true` for [[None]], `[]`, `null`, `undefined`, empty map, empty set, empty object, `''` and `NaN`.
 * Otherwise returns `false`.
 *
 * @example
 * ```ts
 * isEmpty(opt(1)) // false
 * isEmpty(opt(null)) // true
 * isEmpty([]) // true
 * isEmpty([1]) // false
 * isEmpty(null) // true
 * isEmpty('') // true
 * ```
 *
 * @param x
 */
var isEmpty = function (x) {
    if (exports.isOpt(x)) {
        return x.isEmpty;
    }
    if (Array.isArray(x)) {
        return x.length === 0;
    }
    if (x === null || x === undefined) {
        return true;
    }
    if (x instanceof Map || x instanceof Set) {
        return x.size === 0;
    }
    if (typeof x === 'object') {
        return Object.getOwnPropertyNames(x).length === 0;
    }
    if (typeof x === 'string') {
        return x === '';
    }
    if (typeof x === 'number') {
        return Number.isNaN(x);
    }
    throw new Error("Unexpected input type: " + typeof x);
};
exports.isEmpty = isEmpty;
/**
 * Negated version of [[isEmpty]].
 * `nonEmpty(x)` is the same as `!isEmpty(x)`. It can be useful when composing functions (e.g. via [[pipe]]).
 *
 * @example
 * ```ts
 * nonEmpty(opt(2)) // true
 * nonEmpty([]) // false
 *
 * const inc = (x: number) => x + 1;
 * pipe(
 *   a, // Some(4)
 *   map(inc), // Some(5)
 *   nonEmpty, // true
 * ) // true
 * ```
 *
 * @see [[isEmpty]]
 * @param x
 */
var nonEmpty = function (x) { return !exports.isEmpty(x); };
exports.nonEmpty = nonEmpty;
/**
 * Identity function.
 *
 * ```ts
 * id(1) // 1
 * id(null) // null
 * ```
 *
 * @param x
 */
var id = function (x) { return x; };
exports.id = id;
/**
 * Same as [[Opt.at]], but also supports unwrapped arrays.
 * @see [[Opt.at]]
 * @param index
 */
var at = function (index) { return function (x) { return (exports.isOpt(x) ? x : exports.opt(x)).at(index); }; };
exports.at = at;
/**
 * Same as [[Opt.head]], but also supports unwrapped arrays.
 * @see [[Opt.head]]
 * @param x
 */
var head = function (x) { return (exports.isOpt(x) ? x : exports.opt(x)).head(); };
exports.head = head;
/**
 * Same as [[Opt.last]], but also supports unwrapped arrays.
 * @see [[Opt.last]]
 * @param x
 */
var last = function (x) { return (exports.isOpt(x) ? x : exports.opt(x)).last(); };
exports.last = last;
var lenToZipFn = {
    2: exports.uncurryTuple(exports.zip),
    3: exports.uncurryTuple3(exports.zip3),
    4: exports.uncurryTuple4(exports.zip4),
    5: exports.uncurryTuple5(exports.zip5),
};
/**
 * Takes a tuple, wraps each element in [[Opt]] and applies appropriate [[Opt.zip]] function.
 *
 * @example
 * ```ts
 * zipToOptArray([1, null, '', 7, false]) // None: Opt<[number, boolean, string, number, boolean]>
 * zipToOptArray([1, true, '', 7, false]) // Some<[1, true, '', 7, false]>: Opt<[number, boolean, string, number, boolean]>
 * ```
 *
 * Useful as a replacement to `zip*` functions when construction of [[Opt]]s happens in parameters of the function.
 * ```ts
 * zipToOptArray([1, null, '', 7, false])
 * // is same as
 * zip5(opt(1), opt(null), opt(''), opt(7), opt(false))
 * ```
 *
 * @param xs
 */
var zipToOptArray = function (xs) {
    return exports.opt(lenToZipFn[xs.length]).orCrash("Invalid input array length " + xs.length)(xs.map(exports.opt));
};
exports.zipToOptArray = zipToOptArray;
/**
 * Test string against regular expression.
 *
 * @example
 * ```ts
 * testRe(/a/)('bac') // true
 * testRe(/a/)('xxx') // false
 *
 * opt('abc').map(testRe(/b/)).orFalse() // false
 * ```
 *
 * @param re
 */
var testRe = function (re) { return function (x) { return re.test(x); }; };
exports.testRe = testRe;
/** @see [[Opt.testReOrFalse]] */
var testReOrFalse = function (re) { return function (x) { return x.testReOrFalse(re); }; };
exports.testReOrFalse = testReOrFalse;
/**
 * Runs a given function. Result is wrapped by [[opt]]. Returns [[None]] when the function throws.
 *
 * @example
 * ```ts
 * tryRun(() => 1) // Some(1)
 * tryRun(() => { throw new Error(); }) // None
 * ```
 *
 * @param f
 */
var tryRun = function (f) {
    try {
        return exports.opt(f());
    }
    catch (e) {
        return exports.none;
    }
};
exports.tryRun = tryRun;
/**
 * Parses JSON. The result is passed to [[opt]], any error results in [[None]].
 *
 * @example
 * ```ts
 * parseJson('{"a": 1}') // Some({a: 1})
 * parseJson('Ryoka') // None
 * parseJson('null') // None - valid JSON (according to the new standard), but opt(null) is None
 * ```
 *
 * Typical use is to call [[Opt.narrow]] afterwards to validate parsed data and get proper type.
 *
 * @param x
 */
var parseJson = function (x) { return exports.tryRun(function () { return JSON.parse(x); }); };
exports.parseJson = parseJson;
/**
 * Parses integer (same semantics as `Number.parseInt`).
 * The result is wrapped into [[opt]] (so `NaN` will become [[None]]).
 *
 * @example
 * ```ts
 * parseInt('0') // Some(0)
 * parseInt('gin') // None
 * parseInt('1.1') // Some(1)
 * ```
 *
 * @param x
 */
var parseInt = function (x) { return exports.opt(Number.parseInt(x, 10)); };
exports.parseInt = parseInt;
//# sourceMappingURL=Opt.js.map