ts-opt
===

[![pipeline status](https://gitlab.com/monnef/ts-opt/badges/master/pipeline.svg)](https://gitlab.com/monnef/ts-opt/-/commits/master)
[![coverage report](https://gitlab.com/monnef/ts-opt/badges/master/coverage.svg)](https://gitlab.com/monnef/ts-opt/-/commits/master)
[![minified + gzipped size](https://badgen.net/bundlephobia/minzip/ts-opt)](https://bundlephobia.com/result?p=ts-opt)

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

Documentation
===
All methods are documented, if you don't see a description please make sure you are reading the base class page - `Opt`.

* Main [Opt](https://glcdn.githack.com/monnef/ts-opt/-/raw/v2.4.0/doc/modules/opt.html) class
* [Module](https://glcdn.githack.com/monnef/ts-opt/-/raw/v2.4.0/doc/modules.html) (constructors and helper functions)

License
===
**MIT**
