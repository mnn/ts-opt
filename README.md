ts-opt
===
Typed Option/Maybe monad for TypeScript and JavaScript (based on Scala and Haskell).

Examples
===
```typescript
import { opt } from 'ts-opt';

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

    const capitalize = (x: string) => x.split('').map(y => y.toUpperCase()).join('');

    // without
    const f = (id: number | undefined): string | null => {
      if (id === undefined) { return null; }
      const item = db[id];
      if (!item) { return null; }
      const surname = item.surname ? capitalize(item.surname) : '<missing>';
      return item.name + ' ' + surname;
    };

    // with
    const g = (id: number | undefined): string | null => opt(id)
      .chainToOpt(x => db[x])
      .map(item => item.name + ' ' + opt(item.surname).map(capitalize).orElse('<missing>'))
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

https://glcdn.githack.com/monnef/ts-opt/raw/master/doc/modules/_opt_.html

License
===
**MIT**
