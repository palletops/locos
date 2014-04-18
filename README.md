# locos

A library for using declarative configuration data, based on logic programming.

## Usage

Add the following to your `:dependencies`:

```clj
[palletops/locos "0.1.0"]
```

## Introduction

The library allows you to write rules as vector triples. Each rule consists of a
pattern, a production and any number of guards.

```clj
(use 'palletops.locos)
(defrules simple
  [{:item :a} {:x 1}]
  [{:item :b} {:x 2}]
  [{:item :b :factor ?f} {:x (* 2 ?f)}]
  [{:item :b :factor ?f} {:x (* 3 ?f)} [> ?f 4]])
```

The rules can be passed to the `config` function to retrieve a map built from
merging all matched rules.

```clj
(config {:item :a} simple) => {:x 1}
(config {:item :b} simple) => {:x 2}
(config {:item :b :factor 2} simple) => {:x 4}
(config {:item :b :factor 5} simple) => {:x 15}
```

## License


Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)

Copyright © 2012-2014 Hugo Duncan, Antoni Batchelli
