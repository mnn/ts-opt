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
exports.orTrue = exports.orFalse = exports.orNull = exports.orUndef = exports.orCrash = exports.someOrCrash = exports.chainToOptFlow = exports.actToOpt = exports.chainToOpt = exports.chainFlow = exports.act = exports.chain = exports.flatMap = exports.mapFlow = exports.map = exports.toObject = exports.fromObject = exports.toArray = exports.fromArray = exports.joinOpt = exports.mapOpt = exports.catOpts = exports.apFn = exports.ap = exports.isOpt = exports.optArrayOpt = exports.optNegative = exports.optZero = exports.optEmptyString = exports.optEmptyObject = exports.optEmptyArray = exports.optFalsy = exports.opt = exports.some = exports.none = exports.deserializeUnsafe = exports.deserializeOrCrash = exports.deserialize = exports.serialize = exports.ReduxDevtoolsCompatibilityHelper = exports.isOptSerialized = exports.Opt = exports.isUnknown = exports.isNumber = exports.isObject = exports.isFunction = exports.isReadonlyArray = exports.isArray = exports.toString = exports.isString = void 0;
exports.last = exports.head = exports.at = exports.id = exports.isFull = exports.nonEmpty = exports.isEmpty = exports.uncurryTuple5 = exports.uncurryTuple4 = exports.uncurryTuple3 = exports.uncurryTuple = exports.curryTuple5 = exports.curryTuple4 = exports.curryTuple3 = exports.curryTuple = exports.compose = exports.flow = exports.swap = exports.genNakedPropOrCrash = exports.propOrCrash = exports.propNaked = exports.prop = exports.equals = exports.print = exports.narrowOrCrash = exports.narrow = exports.find = exports.count = exports.noneWhen = exports.noneIfEmpty = exports.noneIf = exports.filter = exports.zip5 = exports.zip4 = exports.zip3 = exports.zip = exports.flatBimap = exports.bimap = exports.altOpt = exports.alt = exports.orElseAny = exports.orElseLazy = exports.orElse = exports.forAll = exports.exists = exports.contains = exports.pipe = exports.onBoth = exports.caseOf = exports.orNaN = void 0;
exports.noop = exports.eqAny = exports.eq = exports.crash = exports.appendStr = exports.prependStr = exports.dec = exports.inc = exports.bool = exports.xor = exports.or = exports.and = exports.not = exports.clamp = exports.max2Any = exports.max2All = exports.max2Num = exports.min2Any = exports.min2All = exports.min2Num = exports.max = exports.min = exports.assertType = exports.isOrCrash = exports.onFunc = exports.apply = exports.parseFloat = exports.parseInt = exports.parseJson = exports.tryRun = exports.testReOrFalse = exports.testRe = exports.zipToOptArray = void 0;
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
var isReadonlyArray = function (x) { return Array.isArray(x); };
exports.isReadonlyArray = isReadonlyArray;
// eslint-disable-next-line @typescript-eslint/ban-types
var isFunction = function (x) { return typeof x === 'function'; };
exports.isFunction = isFunction;
var isObject = function (value) { return value !== null && typeof value === 'object'; };
exports.isObject = isObject;
var isNumber = function (x) { return typeof x === 'number'; };
exports.isNumber = isNumber;
var isUnknown = function (_) { return true; };
exports.isUnknown = isUnknown;
var debugPrint = function (tag) {
    var xs = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        xs[_i - 1] = arguments[_i];
    }
    console.log.apply(console, __spreadArray(__spreadArray([], exports.opt(tag).map(function (x) { return ["[" + x + "]"]; }).orElse([])), xs));
};
/**
 * Generic container class. It either holds exactly one value - [[Some]], or no value - [[None]] (empty).
 *
 * It simplifies working with possibly empty values and provides many methods/functions which allow creation of processing pipelines (commonly known as "fluent
 * API" in OOP or [[pipe|chain of reverse applications]] in FP).
 *
 * @typeparam T Wrapped value type.
 */
var Opt = /** @class */ (function () {
    /** @internal */
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    function Opt() {
        var _this = this;
        /**
         * Convets [[Opt]] to an object.
         *
         * @example
         * ```ts
         * opt(1).toObject() // {value: 1}
         * opt(undefined).toObject() // {value: null}
         * opt(undefined).toObject('id') // {id: null}
         * ```
         *
         * @param k
         */
        this.toObject = function (k) {
            var _a;
            if (k === void 0) { k = 'value'; }
            return _a = {}, _a[k] = _this.orNull(), _a;
        };
        /**
         * Similar to [[map]], but supports more functions which are called in succession, each on a result of a previous one.
         *
         * @example
         * ```ts
         * const sq = (x: number) => x * x;
         * const dec = (x: number) => x - 1;
         * opt(4).mapFlow(sq, dec) // Some(15)
         * opt(null).mapFlow(sq, dec) // None
         * ```
         * @see [[ts-opt.mapFlow]]
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
         * @see [[ts-opt.act]]
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
         * @alias [[act]]
         * @see [[ts-opt.chainFlow]]
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
         * import {find} from 'lodash/fp';
         *
         * const data = [{}, {f: true, a: [{b: 7, c: 1}]}, {a: [{}]}];
         * opt(data).actToOpt(
         *   find(x => Boolean(x?.f)), // {f: true, a: [{b: 7, c: 1}]}
         *   x => x?.a, // [{b: 7, c: 1}]
         *   find(x => x.b === 8) // undefined
         * ); // None
         * ```
         *
         * @see [[ts-opt.actToOpt]]
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
         * @alias [[actToOpt]]
         * @see [[ts-opt.chainToOptFlow]]
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
         * @example
         * ```ts
         * some(1).pipe(x => x.isEmpty) // false
         * none.pipe(x => x.isEmpty) // true
         * ```
         *
         * Supports multiple functions.
         *
         * @example
         * ```ts
         * opt(1).pipe( // Some(1)
         *   x => x.isEmpty, // false
         *   x => !x, // true
         * ) // true
         * ```
         *
         * @see [[ts-opt.pipe]]
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
         * @example
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
                // eslint-disable-next-line prefer-rest-params
                var e_1 = arguments[0];
                return function () { return _this.isSome() ? _this.value : e_1; };
            }
            return function () { return _this.orNull(); };
        };
    }
    Object.defineProperty(Opt.prototype, "nonEmpty", {
        /**
         * `false` for [[Some]], `true` for [[None]].
         *
         * If you need to narrow a type to [[Some]], use [[Opt.isSome]].
         */
        get: function () { return !this.isEmpty; },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Opt.prototype, "isFull", {
        /**
         * @alias [[Opt.nonEmpty]]
         */
        get: function () { return this.nonEmpty; },
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
     * Serializes [[Opt]] to a plain JavaScript object.
     * @see [[ts-opt.serialize]]
     */
    Opt.prototype.serialize = function () {
        return exports.serialize(this);
    };
    /**
     * Deserializes [[Opt]] from a plain JavaScript object.
     * @see [[ts-opt.deserialize]]
     */
    Opt.deserialize = function (x, guard) {
        return exports.deserialize(x, guard);
    };
    /**
     * @alias [[flatMap]]
     * @see [[ts-opt.chain]]
     * @param f
     */
    Opt.prototype.chain = function (f) { return this.flatMap(f); };
    /**
     * Combination of [[flatMap]] and [[opt]] functions.
     *
     * @example
     * ```ts
     * some(1).chainToOpt(x => x === 1 ? null : x + 1) // None
     * some(2).chainToOpt(x => x === 1 ? null : x + 1) // Some(3)
     * ```
     *
     * @see [[ts-opt.chainToOpt]]
     *
     * @param f
     */
    Opt.prototype.chainToOpt = function (f) { return this.flatMap(function (x) { return exports.opt(f(x)); }); };
    /**
     * Joins (flattens) nested Opt instance, turning an `Opt<Opt<T>>` into an `Opt<T>`.
     * This is equivalent to calling `flatMap` with the identity function: `.join()` ~ `.flatMap(id)`.
     *
     * @example
     * ```ts
     * const nestedOpt: Opt<Opt<number>> = opt(opt(42)); // Some(Some(42))
     * const flattenedOpt: Opt<number> = nestedOpt.join(); // Some(42)
     * ```
     */
    Opt.prototype.join = function () {
        return this.flatMap(exports.id);
    };
    /**
     * Filter by regular expression.
     * It is a shortcut function for [[Opt.filter]] + [[testRe]].
     *
     * @example
     * ```ts
     * opt('Luffy').filterByRe(/f+/) // Some('Luffy')
     * opt('Robin').filterByRe(/f+/) // None
     * ```
     *
     * @param regex
     */
    Opt.prototype.filterByRe = function (regex) {
        if (this.isSome() && !exports.isString(this.value)) {
            throw new Error("Expected string, got " + JSON.stringify(this.value) + ".");
        }
        return this.filter(exports.testRe(regex));
    };
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
     * Returns [[None]] if opt holds a value for which [[isEmpty]] returns `true`, otherwise passes opt unchanged.
     *
     * @example
     * ```ts
     * opt('x').noneIfEmpty() // Some('x')
     * opt('').noneIfEmpty() // None
     * opt([]).noneIfEmpty() // None
     * opt({}).noneIfEmpty() // None
     * ```
     */
    Opt.prototype.noneIfEmpty = function () {
        return this.noneIf(exports.isEmpty);
    };
    /**
     * Returns [[None]] when given `true`, otherwise passes opt unchanged.
     *
     * @example
     * ```ts
     * opt(1).noneWhen(false) // Some(1)
     * opt(1).noneWhen(true) // None
     * ```
     *
     * @param returnNone
     */
    Opt.prototype.noneWhen = function (returnNone) {
        return this.noneIf(function (_) { return returnNone; });
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
     * @see [[ts-opt.count]]
     * @param predicate
     */
    Opt.prototype.count = function (predicate) {
        return this.filter(predicate).length;
    };
    /**
     * Widen union (typically union of strings to string).
     *
     * @experimental May be removed if it is later found out it's unsafe and unfixable.
     *
     * @example
     * ```ts
     * type EnumAB = 'a' | 'b';
     * type EnumABC = 'a' | 'b' | 'c';
     * const ab = 'a' as EnumAB;
     * const abc = 'c' as EnumABC;
     * const correctWiden: Opt<EnumABC> = opt(ab).widen<EnumABC>(); // AB -> ABC: Ok
     * const wrongWiden: Opt<never> = opt(abc).widen<EnumAB>(); // ABC -> AB: Not Ok, C is not in AB
     * ```
     */
    Opt.prototype.widen = function () {
        return this;
    };
    /**
     * Get a field from a wrapped object. Crash if the field is missing or empty, or opt instance is [[None]].
     * Shortcut of [[Opt.prop]] + [[Opt.orCrash]].
     *
     * @param key
     */
    Opt.prototype.propOrCrash = function (key) {
        return this.prop(key).orCrash("missing " + String(key));
    };
    /**
     * Get a first item of an array or a first character of a string.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).head() // Some(1)
     * opt([]).head() // None
     * opt(null).head() // None
     * opt('Palico').head() // Some('P')
     * ```
     *
     * @see [[ts-opt.head]]
     */
    Opt.prototype.head = function () { return this.at(0); };
    /**
     * Get a last item of an array or a last character of a string.
     *
     * @example
     * ```ts
     * opt([1, 2, 3]).last() // Some(3)
     * opt([]).last() // None
     * opt(null).last() // None
     * opt('Palico').last() // Some('o')
     * ```
     *
     * @see [[ts-opt.last]]
     */
    Opt.prototype.last = function () { return this.at(-1); };
    /**
     * A convenience function to test this (`Opt<string>`) against a given regular expression.
     *
     * @example
     * ```ts
     * opt('a').testReOrFalse(/a/) // true
     * opt('b').testReOrFalse(/a/) // false
     * ```
     *
     * @see [[ts-opt.testReOrFalse]]
     *
     * @param re Regular expression
     */
    Opt.prototype.testReOrFalse = function (re) {
        if (this.isEmpty)
            return false;
        return this.narrow(exports.isString).someOrCrash("testReOrFalse only works on Opt<string>").map(exports.testRe(re)).orFalse();
    };
    Object.defineProperty(Opt.prototype, "end", {
        /**
         * No-op terminator used to end imperative chains.
         *
         * @imperative
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
    /**
     * Apply (call) a function inside [[Some]]. Does nothing for [[None]].
     *
     * @example
     * ```ts
     * const add = (a: number, b: number) => a + b;
     * opt(add).apply(2, 3) // Some(5)
     * none.apply(0).orNull() // None
     * ```
     *
     * @example It can also be used with curried functions.
     * ```ts
     * const sub = (a: number) => (b: number) => a - b;
     * opt(sub).apply(10).apply(3) // Some(7)
     * ```
     *
     * @note [[apply]] is only available for functions, otherwise an exception will be thrown when called on [[Some]].
     *
     * @see [[onFunc]] for imperative version
     *
     * @param args Parameters passed to wrapped function.
     * @return `opt`-wrapped result from the function
     */
    Opt.prototype.apply = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        if (this.isSome()) {
            var val = this.value;
            if (exports.isFunction(val)) {
                return exports.opt(val.apply(void 0, args));
            }
            throw new Error("Invalid input - expected function, got " + typeof val + ".");
        }
        return exports.none;
    };
    /**
     * Apply (call) a function inside [[Some]]. Does nothing for [[None]].
     *
     * @example Both lines do the same thing
     * ```ts
     * opt(f).onSome(x => x())
     * opt(f).onFunc()
     * ```
     *
     * @example
     * ```ts
     * const g = (a: number, b: number): void => console.log(a, b);
     * opt(g).onFunc(1, 2) // calls `g` (prints 1 and 2), returns Some(g)
     * none.onFunc(79) // None
     * ```
     *
     * @note [[onFunc]] is only available for functions, otherwise an exception will be thrown when called on [[Some]].
     * @imperative
     *
     * @see [[apply]] for functional version
     *
     * @param args
     * @return Unchanged [[Opt]] instance
     */
    Opt.prototype.onFunc = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        if (this.isSome()) {
            var val = this.value;
            if (exports.isFunction(val)) {
                val.apply(void 0, args);
            }
            else {
                throw new Error("Invalid input - expected function, got " + typeof val + ".");
            }
        }
        return this;
    };
    /**
     * Converts an object to [[Opt]].
     *
     * @example
     * ```ts
     * Opt.fromObject({value: 4}) // Some(4)
     * Opt.fromObject({id: 4, something: '?'}, 'id') // Some(4)
     * Opt.fromObject({value: null}) // None
     * ```
     *
     * @param x
     * @param k
     */
    Opt.fromObject = function (x, k) {
        if (k === void 0) { k = 'value'; }
        return exports.opt(x[k]);
    };
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
    /** @internal */
    function None() {
        var _this = _super.call(this) || this;
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
    None.prototype.orElseLazy = function (def) { return def(); };
    None.prototype.alt = function (def) { return def; };
    None.prototype.altOpt = function (def) { return exports.opt(def); };
    None.prototype.orElseAny = function (def) { return def; };
    None.prototype.bimap = function (_someF, noneF) { return exports.opt(noneF()); };
    None.prototype.flatBimap = function (_someF, noneF) { return noneF(); };
    None.prototype.toString = function () { return 'None'; };
    None.prototype.zip = function (_other) { return exports.none; };
    None.prototype.zip3 = function (_x, _y) { return exports.none; };
    None.prototype.zip4 = function (_x, _y, _z) { return exports.none; };
    None.prototype.zip5 = function (_x, _y, _z, _zz) { return exports.none; };
    None.prototype.filter = function (_predicate) { return exports.none; };
    None.prototype.narrow = function (_guard) { return this; };
    None.prototype.narrowOrCrash = function (guard, _crashMessage) {
        // don't crash on previous none
        return this.narrow(guard);
    };
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
    None.prototype.max = function () {
        return exports.none;
    };
    None.prototype.min = function () {
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
    /** @internal */
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
    Some.prototype.orElseLazy = function (_def) { return this._value; };
    Some.prototype.alt = function (_def) { return this; };
    Some.prototype.altOpt = function (_def) { return this; };
    Some.prototype.orElseAny = function (_def) { return this._value; };
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
    Some.prototype.narrowOrCrash = function (guard, crashMessage) {
        return this.narrow(guard).someOrCrash(crashMessage !== null && crashMessage !== void 0 ? crashMessage : 'Unexpected type in opt.');
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
        if (Array.isArray(val) || exports.isString(val)) {
            var processedIndex = (index < 0 ? val.length : 0) + index;
            return exports.opt(val[processedIndex]);
        }
        else {
            throw new Error("`Opt#at` can only be used on arrays and strings");
        }
    };
    Some.prototype.min = function () {
        var val = this._value;
        if (!exports.isArray(val)) {
            throw new Error('Expected array.');
        }
        if (val.length === 0)
            return exports.none;
        return exports.some(val.reduce(function (acc, x) { return x < acc ? x : acc; }, val[0]));
    };
    Some.prototype.max = function () {
        var val = this._value;
        if (!exports.isArray(val)) {
            throw new Error('Expected array.');
        }
        if (val.length === 0)
            return exports.none;
        return exports.some(val.reduce(function (acc, x) { return x > acc ? x : acc; }, val[0]));
    };
    return Some;
}(Opt));
var someSerializedType = 'Opt/Some';
var noneSerializedType = 'Opt/None';
var isOptSerialized = function (x) {
    if (typeof x !== 'object' || x === null) {
        return false;
    }
    if ('type' in x) {
        // @ts-expect-error because TS can't infer `type` field is actually there, despite it being checked line before...
        return x.type === someSerializedType || x.type === noneSerializedType;
    }
    else {
        return false;
    }
};
exports.isOptSerialized = isOptSerialized;
/**
 * A helper class for providing compatibility with Redux DevTools.
 */
var ReduxDevtoolsCompatibilityHelper = /** @class */ (function () {
    function ReduxDevtoolsCompatibilityHelper() {
    }
    ReduxDevtoolsCompatibilityHelper.replacer = function (_key, value) {
        return exports.isOpt(value) ? exports.serialize(value) : value;
    };
    ReduxDevtoolsCompatibilityHelper.reviver = function (_key, value) {
        if (!value || typeof value !== 'object') {
            return value;
        }
        var deser = exports.deserialize(value, exports.isUnknown);
        if (deser.tag === 'failure')
            return value;
        return deser.value;
    };
    return ReduxDevtoolsCompatibilityHelper;
}());
exports.ReduxDevtoolsCompatibilityHelper = ReduxDevtoolsCompatibilityHelper;
/**
 * Serializes an Opt instance to a plain JavaScript object.
 *
 * @example
 * ```ts
 * serialize(none) // { type: 'Opt/None' }
 * serialize(opt(1)) // { type: 'Opt/Some', value: 1 }
 * ```
 *
 * @param x [[Opt]] instance to serialize
 * @returns serialized Opt instance as an [[OptSerialized]] object
 */
var serialize = function (x) {
    if (x.isEmpty) {
        return { type: noneSerializedType };
    }
    else {
        return {
            type: someSerializedType,
            value: x.orCrash('failed to extract value from Some'),
        };
    }
};
exports.serialize = serialize;
/**
 * Deserializes a plain JavaScript object to an Opt instance.
 *
 * @example
 * ```ts
 * deserialize({ type: 'Opt/None' }, isNumber) // { tag: 'success', value: none }
 * deserialize({ type: 'Opt/Some', value: 0 }, isNumber) // { tag: 'success', value: some(0) }
 * deserialize({ type: 'Opt/Some', value: 'not a number' }, isNumber) // { tag: 'failure', reason: 'failed to validate inner type' }
 * ```
 *
 * @see [[serialize]]
 * @param x serialized Opt object (expected shape is [[OptSerialized]])
 * @param guard function to validate the inner type
 * @returns deserialization result as a [[DeserializationResult]] object
 */
var deserialize = function (x, guard) {
    if (!exports.isOptSerialized(x))
        return { tag: 'failure', reason: 'not OptSerialized' };
    switch (x.type) {
        case noneSerializedType:
            return { tag: 'success', value: exports.none };
        case someSerializedType:
            if (!guard(x.value))
                return { tag: 'failure', reason: 'failed to validate inner type' };
            return { tag: 'success', value: exports.some(x.value) };
    }
};
exports.deserialize = deserialize;
/**
 * Deserializes the input value or throws an error if deserialization fails.
 *
 * @example
 * ```ts
 * deserializeOrCrash({ type: 'Opt/None' }, isNumber) // none
 * deserializeOrCrash({ type: 'Opt/Some', value: 0 }, isNumber) // some(0)
 * deserializeOrCrash({ type: 'Opt/Some', value: 'not a number' }, isNumber) // exception thrown
 * deserializeOrCrash(4, isNumber) // exception thrown
 * ```
 *
 * @param x input value to be deserialized
 * @param guard guard function to validate the inner type
 * @return deserialized value as an [[Opt]] instance
 */
var deserializeOrCrash = function (x, guard) {
    var deser = exports.deserialize(x, guard);
    if (deser.tag === 'failure') {
        throw new Error(deser.reason);
    }
    return deser.value;
};
exports.deserializeOrCrash = deserializeOrCrash;
/**
 * Unsafe version of [[deserializeOrCrash]].
 * Deserialization failure is indistinguishable from deserialized [[None]].
 * It is usually better to use [[deserialize]] or [[deserializeOrCrash]].
 *
 * @example
 * ```ts
 * deserializeUnsafe({ type: 'Opt/None' }) // none
 * deserializeUnsafe({ type: 'Opt/Some', value: 0 }) // some(0)
 * deserializeUnsafe(9) // none
 * ```
 *
 * @param x input value to be deserialized
 * @returns deserialized value as an [[Opt]] instance
 */
var deserializeUnsafe = function (x) {
    return exports.tryRun(function () { return exports.deserializeOrCrash(x, exports.isUnknown); }).join();
};
exports.deserializeUnsafe = deserializeUnsafe;
var isNoneValue = function (x) {
    return x === undefined || x === null || Number.isNaN(x);
};
/**
 * Single global instance of [[None]].
 */
exports.none = Object.freeze(new None());
/**
 * Constructs [[Some]].
 *
 * Warning: Usually it is [[opt]] you are looking for.
 * Only in rare cases you want to have for example `Some(undefined)`.
 * @param x
 */
var some = function (x) { return Object.freeze(new Some(x)); };
exports.some = some;
/**
 * Main constructor function - for `undefined`, `null` and `NaN` returns [[None]].
 * Anything else is wrapped into [[Some]].
 * @example
 * ```ts
 * opt(0) // Some(0)
 * opt(1) // Some(1)
 * opt(true) // Some(true)
 * opt('Kagome') // Some('Kagome')
 * opt([]) // Some([])
 * opt([1]) // Some([1])
 * opt({}) // Some({})
 * opt({a: 0}) // Some({a: 0})
 * opt(undefined) // None
 * opt(null) // None
 * opt(NaN) // None
 * ```
 * @param x
 */
var opt = function (x) {
    return isNoneValue(x) ? exports.none : exports.some(x);
};
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
 * Converts optional array of optional values to opt-wrapped array with empty values discarded.
 *
 * @example
 * ```ts
 * optArrayOpt(undefined) // None
 * optArrayOpt([]) // Some([])
 * optArrayOpt([1]) // Some([1])
 * optArrayOpt([0, null, undefined, 1]) // Some([0, 1])
 * ```
 *
 * @param xs
 */
var optArrayOpt = function (xs) {
    return exports.opt(xs).mapFlow(function (ys) { return ys.map(exports.opt); }, exports.catOpts);
};
exports.optArrayOpt = optArrayOpt;
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
 * @see [[Opt.join]]
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
 * @see [[Opt.fromObject]]
 */
exports.fromObject = Opt.fromObject;
/**
 * @see [[Opt.toObject]]
 */
var toObject = function (k) {
    return function (x) {
        return x.toObject(k);
    };
};
exports.toObject = toObject;
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
/** @see [[Opt.orElseLazy]] */
var orElseLazy = function (e) { return function (x) { return x.orElseLazy(e); }; };
exports.orElseLazy = orElseLazy;
/** @see [[Opt.orElseAny]] */
var orElseAny = function (e) { return function (x) { return x.orElseAny(e); }; };
exports.orElseAny = orElseAny;
/** @see [[Opt.alt]] */
var alt = function (def) { return function (x) { return x.alt(def); }; };
exports.alt = alt;
/** @see [[Opt.altOpt]] */
var altOpt = function (def) { return function (x) { return x.altOpt(def); }; };
exports.altOpt = altOpt;
/** @see [[Opt.bimap]] */
var bimap = function (someF) { return function (noneF) { return function (x) { return x.bimap(someF, noneF); }; }; };
exports.bimap = bimap;
/** @see [[Opt.flatBimap]] */
var flatBimap = function (someF) { return function (noneF) { return function (x) { return x.flatBimap(someF, noneF); }; }; };
exports.flatBimap = flatBimap;
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
/** @see [[Opt.noneIf]] */
var noneIf = function (predicate) { return function (x) { return x.noneIf(predicate); }; };
exports.noneIf = noneIf;
/** @see [[Opt.noneIfEmpty]] */
var noneIfEmpty = function (x) {
    return x.noneIfEmpty();
};
exports.noneIfEmpty = noneIfEmpty;
/** @see [[Opt.noneWhen]] */
var noneWhen = function (returnNone) { return function (x) { return x.noneWhen(returnNone); }; };
exports.noneWhen = noneWhen;
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
/**
 * Find a first item which holds true for a given predicate and return it wrapped in [[Some]].
 * Return [[None]] when no match is found.
 *
 * @example
 * ```ts
 * find((x: number) => x > 0)([-1, 0, 1]) // Some(1)
 * find((x: number) => x > 5)([0, 3, 5]) // None
 * ```
 *
 * @param predicate
 */
var find = function (predicate) { return function (xs) { return exports.opt(xs.find(function (x) { return predicate(x); })); }; };
exports.find = find;
/** @see [[Opt.narrow]] */
var narrow = function (guard) { return function (x) { return x.narrow(guard); }; };
exports.narrow = narrow;
/** @see [[Opt.narrowOrCrash]] */
var narrowOrCrash = function (guard, crashMessage) { return function (x) { return x.narrowOrCrash(guard, crashMessage); }; };
exports.narrowOrCrash = narrowOrCrash;
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
/**
 * Similar to [[Opt.prop]], but it is designed for naked objects (not wrapped in opt).
 *
 * @example
 * ```ts
 * type ObjA = {a: boolean};
 * propNaked<ObjA>('a')(null) // None
 * propNaked<ObjA>('a')({a: true}) // Some(true)
 *
 * type ObjC = {c: string | null};
 * propNaked<ObjC>('c')({c: null}) // None
 * ```
 */
var propNaked = function (key) { return function (x) { return exports.opt(x).prop(key); }; };
exports.propNaked = propNaked;
/**
 * Similar to [[Opt.propOrCrash]], but also supports naked objects.
 *
 * @example
 * ```ts
 * interface Animal {
 *   name?: string;
 * }
 * const a: Animal = {name: 'Spot'};
 * propOrCrash<Animal>('name')(a) // 'Spot'
 * ```
 */
var propOrCrash = function (key) { return function (x) {
    return (exports.isOpt(x) ? x : exports.opt(x)).propOrCrash(key);
}; };
exports.propOrCrash = propOrCrash;
/**
 * Utility function for generating property getter for one specific object.
 * Functionally similar to [[propOrCrash]], but it has swapped arguments and only supports naked objects.
 *
 * @example
 * ```ts
 * interface Animal {
 *   id: number;
 *   name?: string;
 * }
 *
 * const spot: Animal = {id: 36, name: 'Spot'};
 * const getSpotProp = genNakedPropOrCrash(spot);
 * getSpotProp('name') // 'Spot'
 * getSpotProp('id') // 36
 *
 * const cow: Animal = {id: 36};
 * const getCowProp = genNakedPropOrCrash(cow);
 * getCowProp('name') // crashes with 'missing name'
 * ```
 *
 * ---
 *
 * It is a shorter alternative of
 * ```ts
 * const o = opt(obj);
 * o.propOrCrash('fieldA')
 * o.propOrCrash('fieldB')
 * ```
 * ->
 * ```ts
 * const g = genNakedPropOrCrash(obj);
 * g('fieldA')
 * g('fieldB')
 * ```
 *
 * Performance characterics are expected to be similar.
 *
 * @param obj
 */
var genNakedPropOrCrash = function (obj) {
    var o = exports.opt(obj);
    return function (k) { return o.propOrCrash(k); };
};
exports.genNakedPropOrCrash = genNakedPropOrCrash;
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
/** @alias [[nonEmpty]] */
var isFull = function (x) { return exports.nonEmpty(x); };
exports.isFull = isFull;
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
var at = function (index) { return function (x) {
    return (exports.isOpt(x) ? x : exports.opt(x)).at(index);
}; };
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
/**
 * Parses float (same semantics as `Number.parseFloat`).
 * The result is wrapped into [[opt]] (so `NaN` will become [[None]]).
 *
 * @example
 * ```ts
 * parseFloat('0') // Some(0)
 * parseFloat('-1.2') // Some(-1.2)
 * parseFloat('xFF') // None
 * ```
 *
 * @param x
 */
var parseFloat = function (x) { return exports.opt(Number.parseFloat(x)); };
exports.parseFloat = parseFloat;
/** @see [[Opt.apply]] */
var apply = function () {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args[_i] = arguments[_i];
    }
    return function (x) { return x.apply.apply(x, args); };
};
exports.apply = apply;
/** @see [[Opt.onFunc]] */
var onFunc = function () {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args[_i] = arguments[_i];
    }
    return function (x) { return x.onFunc.apply(x, args); };
};
exports.onFunc = onFunc;
/**
 * Verify the given value passes the guard. If not, throw an exception.
 *
 * @example
 * ```ts
 * const a = isOrCrash(isNumber)(4 as unknown); // a is of type number, doesn't throw
 * const b: number = a; // ok
 * ```
 *
 * @param guard
 * @param msg
 */
var isOrCrash = function (guard, msg) {
    if (msg === void 0) { msg = 'invalid value'; }
    return function (x) {
        return exports.some(x).narrow(guard).orCrash(msg);
    };
};
exports.isOrCrash = isOrCrash;
/**
 * Asserts a type via a given guard.
 *
 * @example
 * ```ts
 * const a: unknown = 1 as unknown;
 * assertType(a, isNumber);
 * const b: number = a; // ok
 * ```
 *
 * @param x
 * @param guard
 * @param msg
 */
var assertType = function (x, guard, msg) {
    if (msg === void 0) { msg = 'invalid value'; }
    exports.isOrCrash(guard, msg)(x);
};
exports.assertType = assertType;
/** @see [[Opt.min]] */
var min = function (x) { return exports.isReadonlyArray(x) ? exports.opt(x).min() : x.min(); };
exports.min = min;
/** @see [[Opt.max]] */
var max = function (x) { return exports.isReadonlyArray(x) ? exports.opt(x).max() : x.max(); };
exports.max = max;
// generate a function of two curried optional arguments
// common - two some values lead to call of op, two nones returns none
// all mode - all operands must be some, otherwise return none
// any mode - if one operand is none, return the other (non-none) one
var gen2Op = function (mode, op) {
    return function (x) { return function (y) {
        var ox = exports.opt(x);
        var oy = exports.opt(y);
        var allRes = ox.zip(oy).map(exports.uncurryTuple(op));
        var anyResGen = function () { return allRes.alt(ox).alt(oy); };
        return mode === 'all' ? allRes : anyResGen();
    }; };
};
/**
 * Get a lesser number from two given numbers.
 *
 * @example
 * ```ts
 * min2Num(1)(2) // 1
 * ```
 *
 * @param a
 */
var min2Num = function (a) { return function (b) { return a < b ? a : b; }; };
exports.min2Num = min2Num;
/**
 * Get a lesser number from two possibly empty numbers.
 * Returns `None` when any operand is `None`.
 *
 * @example
 * ```ts
 * min2All(1)(2) // Some(1)
 * min2All(1)(null) // None
 * min2All(null)(null) // None
 * ```
 */
exports.min2All = gen2Op('all', exports.min2Num);
/**
 * Get a lesser number from two possibly empty numbers.
 * Returns `None` when both operands are `None`, otherwise returns the other (nonempty) operand.
 *
 * @example
 * ```ts
 * min2Any(1)(2) // Some(1)
 * min2Any(1)(null) // Some(1)
 * min2Any(null)(undefined) // None
 * ```
 */
exports.min2Any = gen2Op('any', exports.min2Num);
/**
 * Get a larger number from two given numbers.
 *
 * @example
 * ```ts
 * max2Num(1)(2) // 2
 * ```
 *
 * @param a
 */
var max2Num = function (a) { return function (b) { return a > b ? a : b; }; };
exports.max2Num = max2Num;
/**
 * Get a larger number from two possibly empty numbers.
 * Returns `None` when any operand is `None`.
 *
 * @example
 * ```ts
 * max2All(1)(2) // Some(2)
 * max2All(1)(null) // None
 * max2All(null)(null) // None
 * ```
 */
exports.max2All = gen2Op('all', exports.max2Num);
/**
 * Get a larger number from two possibly empty numbers.
 * Returns `None` when both operands are `None`, otherwise returns the other (nonempty) operand.
 *
 * @example
 * ```ts
 * max2Any(1)(2) // Some(2)
 * max2Any(1)(null) // Some(1)
 * max2Any(null)(undefined) // None
 * ```
 */
exports.max2Any = gen2Op('any', exports.max2Num);
/**
 * Given range (where each part may be empty), clamp a given possibly empty number to the given range.
 *
 * @example
 * ```ts
 * clamp(0)(10)(5) // Some(5)
 * clamp(0)(10)(-4) // Some(0)
 * clamp(0)(10)(12) // Some(10)
 *
 * clamp(0)(undefined)(5) // Some(5)
 * clamp(0)(null)(-1) // Some(0)
 *
 * clamp(NaN)(10)(5) // Some(5)
 * clamp(undefined)(10)(12) // Some(10)
 *
 * clamp(undefined)(undefined)(5) // Some(5)
 *
 * clamp(0)(1)(null) // None
 * ```
 *
 * @param minValue
 */
var clamp = function (minValue) { return function (maxValue) { return function (x) {
    return exports.opt(x).act(exports.max2Any(minValue), exports.min2Any(maxValue));
}; }; };
exports.clamp = clamp;
/**
 * Logical negation of a boolean value.
 *
 * @example
 * ```ts
 * not(true) // false
 * not(false) // true
 * ```
 *
 * @param x
 * @returns The negated boolean value
 */
var not = function (x) { return !x; };
exports.not = not;
/**
 * Logical AND of two boolean values.
 *
 * @example
 * ```ts
 * and(true)(false) // false
 * and(true)(true) // true
 * ```
 *
 * @param x
 * @returns
 */
var and = function (x) { return function (y) { return x && y; }; };
exports.and = and;
/**
 * Logical OR of two boolean values.
 *
 * @example
 * ```ts
 * or(true)(false) // true
 * or(false)(false) // false
 * ```
 *
 * @param x
 * @returns
 */
var or = function (x) { return function (y) { return x || y; }; };
exports.or = or;
/**
 * Logical XOR (exclusive OR) of two boolean values.
 *
 * @example
 * ```ts
 * xor(true)(false) // true
 * xor(true)(true) // false
 * ```
 *
 * @param x
 * @returns
 */
var xor = function (x) { return function (y) { return x !== y; }; };
exports.xor = xor;
/**
 * Returns one of two values based on a boolean condition.
 *
 * @example
 * ```ts
 * bool(0)(1)(true) // 1
 * bool('no')('yes')(false) // 'no'
 * ```
 *
 * @param falseValue
 * @returns
 */
var bool = function (falseValue) { return function (trueValue) { return function (cond) { return cond ? trueValue : falseValue; }; }; };
exports.bool = bool;
// TODO: boolLazy
// TODO: imperative variant of bool?
/**
 * Increments a number by 1.
 *
 * @example
 * ```ts
 * inc(5) // 6
 * ```
 *
 * @param x - The number to increment
 * @returns The number incremented by 1
 */
var inc = function (x) { return x + 1; };
exports.inc = inc;
/**
 * Decrements a number by 1.
 *
 * @example
 * ```ts
 * dec(5) // 4
 * ```
 *
 * @param x - The number to decrement
 * @returns The number decremented by 1
 */
var dec = function (x) { return x - 1; };
exports.dec = dec;
/**
 * Prepends a string to a given string.
 *
 * @example
 * ```ts
 * prependStr('foo')('bar') // 'foobar'
 * opt('bar').map(prependStr('foo')) // Some('foobar')
 * ```
 */
var prependStr = function (prefix) { return function (str) {
    return "" + prefix + str;
}; };
exports.prependStr = prependStr;
/**
 * Appends a string to a given string.
 *
 * @example
 * ```ts
 * appendStr('bar')('foo') // 'foobar'
 * opt('foo').map(appendStr('bar')) // Some('foobar')
 * opt('foo' as const).map(appendStr('bar')) // Some('foobar'), type is Opt<'foobar'>
 * ```
 */
var appendStr = function (suffix) { return function (str) {
    return "" + str + suffix;
}; };
exports.appendStr = appendStr;
/**
 * Throws an error with a given message.
 *
 * @example
 * ```ts
 * crash('Zeref?') // throws Error
 * ```
 *
 * @param msg - The error message
 * @returns Nothing (function never returns due to exception)
 */
var crash = function (msg) { throw new Error(msg); };
exports.crash = crash;
// TODO: customizable comparator
/**
 * Checks equality between two values of type T using strict equality.
 *
 * @example
 * ```ts
 * eq(5)(5) // true
 * eq(5)(6) // false
 * eq({})({}) // false
 * ```
 *
 * @param a - The first value to compare
 * @returns A function that takes another value and returns whether it is strictly equal to the first value
 */
var eq = function (a) { return function (b) { return a === b; }; };
exports.eq = eq;
// TODO: customizable comparator
/**
 * Checks equality between two values of any type using strict equality.
 *
 * @example
 * ```ts
 * eqAny('hello')('hello') // true
 * eqAny('hello')('Hi') // false
 * eqAny('')(2) // false
 * ```
 *
 * @param a - The first value to compare
 * @returns A function that takes another value and returns whether it is strictly equal to the first value
 */
var eqAny = function (a) { return function (b) { return a === b; }; };
exports.eqAny = eqAny;
/**
 * A no-operation function that simply returns undefined.
 * Can be used as a placeholder callback.
 *
 * @returns undefined
 */
// eslint-disable-next-line @typescript-eslint/no-empty-function
var noop = function () {
    var _args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        _args[_i] = arguments[_i];
    }
};
exports.noop = noop;
//# sourceMappingURL=Opt.js.map