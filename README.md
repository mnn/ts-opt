[[_TOC_]]

ts-opt
===

[![pipeline status](https://gitlab.com/monnef/ts-opt/badges/master/pipeline.svg)](https://gitlab.com/monnef/ts-opt/-/commits/master)
[![coverage report](https://gitlab.com/monnef/ts-opt/badges/master/coverage.svg)](https://gitlab.com/monnef/ts-opt/-/commits/master)
[![minified + gzipped size](https://badgen.net/bundlephobia/minzip/ts-opt)](https://bundlephobia.com/package/ts-opt)
[![npm version](https://badgen.net/npm/v/ts-opt)](https://npmjs.com/package/ts-opt)
[![license](https://badgen.net/npm/license/ts-opt)](LICENSE.md)
[![dependencies](https://badgen.net/bundlephobia/dependency-count/ts-opt)](https://bundlephobia.com/package/ts-opt)

Typed Option/Maybe for TypeScript and JavaScript (based on Scala, Haskell and Sanctuary), created to simplify code involving optional values.

Features
===
* ⛓️ TypeScript Support
  * ⚙️ Strict types - it's a TypeScript-first library, no wild `any`s
  * but it is fully usable from JavaScript as well
* 🔩 Pragmatic - doesn't force functional programming paradigm
* 📏 100% Test Coverage
* 🗜️ Lightweight - no dependencies, only around 3KiB gzip+minified

Installation
===
```sh
npm i -S ts-opt
```

Examples
===
In all examples `opt` import is presumed to be present:

```typescript
import { opt } from 'ts-opt';
```

Basic use
---
```typescript
    // without
    const f = (name: string | undefined) => {
      if (!name || name === '') { throw new Error('Missing name.'); }
      return name[0];
    };

    // with
    const g = (name: string | undefined) => opt(name).orCrash('Missing name.')[0];

    f('Riker'); // 'R'
    g('Riker'); // 'R'

    f(undefined); // exception thrown
    g(undefined); // exception thrown
```

caseOf
---
```typescript
    const fireMissiles = () => { console.log('FIRING!'); };
    const printSuccess = (x: string) => { console.log(x); };

    const handleMoveVanilla = (usersMove?: string): void => usersMove ? printSuccess(usersMove) : fireMissiles();
    const handleMove = (usersMove?: string): void => opt(usersMove).caseOf(printSuccess, fireMissiles);

    handleMoveVanilla(); // prints FIRING!
    handleMove(); // prints FIRING!
    handleMoveVanilla('Build a pylon.'); // prints Build a pylon.
    handleMove('Build a pylon.'); // prints Build a pylon.
```

More advanced
---
```typescript
interface Person {
  name: string;
  surname: string | null;
}

type Db = { [_: string]: Person };

/* ... */

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
```

Pitfalls
===

`None` without explicit type
---
```ts
let a = none;
a = opt(1); // TS2741: Property ''@@type'' is missing in type 'Opt<number>' but required in type 'None<any>'.
```
The solution is to explicitly state the type:
```ts
let a: Opt<number> = none;
a = opt(1);
```
While it's recommended to not use `let`, so this example may seem unrealistic, a similar issue may happen in other places as well (e.g. default arguments of functions or "exact" type via `as const`).

Empty value in `Some`
---
Be careful to not misuse methods/functions like `map` and end up with `Opt`s like `Opt<string | null>` or `Opt<number | undefined>`. Such `Opt`s are ticking bombs, ready to bite a coworker or later even yourself.

Let's have a simple example of a function returning a `name`. When the name is missing (either whole object is `undefined` or the field), we want it to return a default name, in this case `'John'`.

```ts
interface TestUser {
  name?: string;
}

const getNameOrDefault = (x?: TestUser) => opt(x).map(x => x.name).orElse('John');
const nameKon = getNameOrDefault({name: 'Kon'}); // 'Kon'
const nameDefaultFromUndefined = getNameOrDefault(); // 'John'
```

It seems to be working. But there is a catch, when you pass an empty value in the `name` field, you will get `undefined`:
```ts
const nameDefaultFromEmpty = getNameOrDefault({}); // undefined
```

Types can help us see, what is happening:
```ts
const getNameOrDefault = (x?: TestUser) =>
  opt(x)              // Opt<TestUser>
    .map(x => x.name) // Opt<string | undefined>
    .orElse('John');  // string | undefined
```

The culprit is the `map` call which operates on (converts, does mapping of) the value inside the `Opt`. When we want to transform a value inside an `Opt`, but that transformation may return an empty value (e.g. `null` or `undefined`), we **must not** use the `map`. Otherwise, advantages of `Opt` are severely diminished.

Possible and most general solution is to use the `chainToOpt`. It behaves same as the `map`, but when empty values are returned, it flips the whole `Opt` from `Some` to `None`.

```ts
const getNameOrDefault = (x?: TestUser) =>
  opt(x)                     // Opt<TestUser>
    .chainToOpt(x => x.name) // Opt<string>
    .orElse('John');         // string

getNameOrDefault({}); // 'John'
```

Another alternative, in this case of "zooming" onto a field (which is quite common), is to use the `prop` method which also automatically handles possible empty values:

```ts
const getNameOrDefault = (x?: TestUser) =>
  opt(x)             // Opt<TestUser>
    .prop('name')    // Opt<string>
    .orElse('John'); // string

getNameOrDefault({}); // 'John'
```

Using functional methods instead of its imperative variants
---

Functional methods (functions) are used when we care about the result and passed function(s) are pure.

```ts
const res = opt(2).map(x => x * 5); // res is some(10) and is used later in code
```

Imperative methods are used when we want to call a callback or do impure operations, we don't care about results from passed functions.

```ts
opt(1).onSome(console.log); // prints 1, returns some(1), log method returns undefined which is discarded
```

Functional and imperative methods can be used in one opt chain (e.g. `map` -> `print` -> `map`) without breaking recommendations above (an example is in the [`map` vs `onSome` section](#map-vs-onsome)). If you have an impure part of a computation, one function, it is recommended to rewrite it to functional approach (use in `map` or similar) and any remaining imperative operations (e.g. callbacks) isolate to a next part of opt chain (use `onSome` or similar).

---

A common mistake is using a function from first column instead of one from the second.

| Functional                                                                                         | Imperative |
|----------------------------------------------------------------------------------------------------|------------|
| `map` / `chainToOpt`                                                                               | `onSome`   |
| `caseOf` / `bimap`                                                                                 | `onBoth`   |
| <code>orNull() &#124;&#124;</code> / `orNull() ??` / <br /> `if (!x.orUndef())` / `if (x.isEmpty)` | `onNone`   |

The table isn't exhaustive, if you are unsure whether the method/function is imperative, it probably isn't (vast majority of methods are functional).

An explanation with examples can be found in the [`map` vs `onSome` section](#map-vs-onsome).

`map` vs `onSome`
---
At a first glance, it may look like those methods are the same.

```ts
let a = 0;
const setA = (newA: number) => { a = newA };

opt(null as number | null).map(setA); // a is unchanged
opt(null as number | null).onSome(setA); // a is unchanged

opt(2).map(setA); // a is now 2
opt(4).onSome(setA); // a is now 4
```

It may seem like in specific scenarios they can be used interchangeably (do nothing for `None` and call the function for `Some`). But their purpose differs quite a lot. From a technical perspective, there is a distinction in the return value.

```ts
opt(7).map(setA) // Some(undefined)
opt(9).onSome(setA) // Some(9)
```

Both methods are meant to be chained. `map` allows us to refine a value inside `Opt`:

```ts
opt(2) // Some(2)
  .map(x => x + 1) // Some(3)
  .map(x => x * x) // Some(9)
```

`onSome` can't do that:
```ts
opt(2) // Some(2)
  .onSome(x => x + 1) // Some(2)
  .onSome(x => x * x) // Some(2)
```

That's because the purpose of `onSome` is to "break" the purely functional approach and allows `Opt` to be used in a more imperative way - calling a callback function for the sole purpose of [side-effects](https://softwareengineering.stackexchange.com/a/262000/302966). The callback function changes something somewhere else, e.g. sets a global variable or changes DOM. In such cases we don't care about the return value (in most cases it doesn't return anything, this library assumes the responsibility for an error handling is on the callback function). Thus `Opt` ignores that return value from a callback and `onSome` simply returns a previous `Opt`. Because of this, we can easily create chains with multiple callbacks between processing:

```ts
const f = (x?: number) => opt(x)
    .onSome(x => console.log('Got value', x))
    .chainToOpt(x => x * x > 9 ? null : x * 2)
    .onSome(x => console.log('First step result', x))
    .map(x => x - 1)
    .onSome(x => console.log('Second step result', x))
    .orNull();

f(); // returns null, nothing is printed

f(3); /* returns 5, prints:
Got value 3
First step result 6
Second step result 5
*/

f(10); /* returns null, prints:
Got value 10
*/
```

The final note is about implementation. Since `Opt` library gives a specific meaning to `map` (functional methods) and `onSome` (imperative methods), you should not use them interchangeably. In the future `map` implementation may very well change to not be called until termination (making `Opt` so called lazy), but `onSome` will always force evaluation instantly (even if `Opt` starts supporting lazy approach). This would mean that `map` will not call a mapping function until a result from `Opt` is requested (e.g. termination via `orNull`).

```ts
// Example of possible lazy Opt

opt(1).onSome(console.log); // prints 1

opt(2).map(console.log); // prints nothing

const x = opt(3).map(console.log); // prints nothing
// ...
x.orNull(); // prints 1
```

This could lead to bugs. Ones which are not easy to track down, since evaluation of the opt may be in an entirely different file to which opt was passed across several layers and delayed (e.g. from a helper utility function via props through several React components and used [evaluated] only after a user does some action).

Documentation
===
All methods are documented, if you don't see a description please make sure you are reading the base class page - `Opt`.

* Main [Opt](https://glcdn.githack.com/monnef/ts-opt/-/raw/v3.1.0/doc/modules/opt.html) class
* [Module](https://glcdn.githack.com/monnef/ts-opt/-/raw/v3.1.0/doc/modules.html) (constructors and helper functions)

License
===
**MIT**
