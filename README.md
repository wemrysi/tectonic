# tectonic [![Build Status](https://travis-ci.org/slamdata/tectonic.svg?branch=master)](https://travis-ci.org/slamdata/tectonic) [![Bintray](https://img.shields.io/bintray/v/slamdata-inc/maven-public/tectonic.svg)](https://bintray.com/slamdata-inc/maven-public/tectonic) [![Discord](https://img.shields.io/discord/373302030460125185.svg?logo=discord)](https://discord.gg/QNjwCg6)

A columnar fork of [Jawn](https://github.com/non/Jawn). The distinction between "columnar" and its asymmetric opposite, "row-oriented", is in the orientation of data structures which you are expected to create in response to the event stream. Jawn expects a single, self-contained value with internal recursive structure per row, and its `Facade` trait is designed around this idea. Tectonic expects many rows to be combined into a much larger batch with a flat internal structure, and the `Plate` class is designed around this idea.

Tectonic is also designed to support multiple backends, making it possible to write a parser for any sort of input stream (e.g. CSV, XML, etc) while driving a single `Plate`.

These differences have led to some relatively meaningful changes within the parser implementation. Despite that, the bulk of the ideas and, in some areas, the *vast* bulk of the implementation of Tectonic's JSON parser is drawn directly from Jawn. **Special heartfelt thanks to [Erik Osheim](https://github.com/d6) and the rest of the Jawn contributors, without whom this project would not be possible.**

Tectonic is very likely the optimal JSON parser on the JVM for producing columnar data structures. When producing row-oriented structures (such as conventional JSON ASTs), it falls considerably behind Jawn both in terms of performance and usability. Tectonic is also relatively opinionated in that it assumes you will be applying it to variably-framed input stream (corresponding to Jawn's `AsyncParser`) and does not provide any other operating modes.

## Usage

```sbt
libraryDependencies += "com.slamdata" %% "tectonic" % <version>

// if you wish to use Tectonic with fs2 (recommended)
libraryDependencies += "com.slamdata" %% "tectonic-fs2" % <version>
```

If using Tectonic via fs2, you can take advantage of the `StreamParser` `Pipe` to perform all of the heavy lifting:

```scala
import cats.effect.IO

import tectonic.json.Parser
import tectonic.fs2.StreamParser

// assuming MyPlate extends Plate[Foo]
val parserF = 
  IO(Parser(new MyPlate, Parser.ValueStream)) // assuming whitespace-delimited json

val input: Stream[IO, Byte] = ...
input.through(StreamParser(parserF))    // => Stream[IO, Foo]
```

Parse errors will be captured by the stream as exceptions.

## Performance

Broadly-speaking, the performance of Tectonic JSON is roughly on-par with Jawn. Depending on your measurement assumptions, it may actually be slightly faster. It's very very difficult to setup a fair series of measurements, due to the fact that Tectonic produces batched columnar data while Jawn produces rows on an individual basis.

Our solution to this is to use the JMH `Blackhole.consumeCPU` function in a special Jawn `Facade` and Tectonic `Plate`. Each event is implemented with a particular consumption, weighted by the following constants:

```scala
object FacadeTuningParams {
  object Tectonic {
    val VectorCost: Long = 4   // Cons object allocation + memory store
    val TinyScalarCost: Long = 8    // hashmap get + bitset put
    val ScalarCost: Long = 16   // hashmap get + check on array + amortized resize/allocate + array store
    val RowCost: Long = 2   // increment integer + bounds check + amortized reset
    val BatchCost: Long = 1   // (maybe) reset state + bounds check
  }

  val NumericCost: Long = 512   // scalarCost + crazy numeric shenanigans

  object Jawn {
    val VectorAddCost: Long = 32   // hashmap something + checks + allocations + stuff
    val VectorFinalCost: Long = 4   // final allocation + memory store
    val ScalarCost: Long = 2     // object allocation
    val TinyScalarCost: Long = 1   // non-volatile memory read
  }
}
```

- Vectors are arrays or objects
- Tiny scalars are any scalars which can be implemented with particularly concise data structures (e.g. `jnull` for Jawn, or `arr` for Tectonic)
- Scalars are everything else
- Numerics are special, since we assume realistic facades will be performing numerical parsing to ensure maximally efficient representations. Thus, both facades check for decimal and exponent. If these are lacking, then the cost paid is the `ScalarCost`. If these are present, then the cost paid is `NumericCost`, simulating a trip through `BigDecimal`.
- `Add` and `Final` costs for Jawn are referring to the `Context` functions, which are generally implemented with growable, associative data structures set to small sizes

Broadly, these costs were strongly inspired by the internal data structures of [the Mimir database engine](https://github.com/slamdata/quasar) and [the QData representation](https://github.com/slamdata/qdata). In the direct comparison measurements, `Signal.Continue` is universally assumed as the result from `Plate`, voiding any advantages Tectonic can derive from projection/predicate pushdown.

Both frameworks are benchmarked *through* [fs2](https://fs2.io) (in the case of Jawn, using [jawn-fs2](https://github.com/http4s/jawn-fs2)), which is assumed to be the mode in which both parsers will be run. This allows the benchmarks to capture nuances such as `ByteVectorChunk` handling, `ByteBuffer` unpacking and such.

As an aside, even apart from the columnar vs row-oriented data structures, Tectonic does have some meaningful optimizations relative to Jawn. In particular, Tectonic is able to maintain a much more efficient object/array parse state due to the fact that it is not relying on an implicit stack of `Context`s to maintain that state for it. This is particularly noticeable for object/array nesting depth less than 64 levels, which seems to be far-and-away the most common case.

### Benchmark Results

The following were run on my laptop in powered mode with networking disabled, 10 warmup iterations and 10 measurement runs in a forked JVM. You can find all of the sources in the **benchmarks** subproject. Please note that these results are dependent on the assumptions codified in `FacadeTuningParams`.

```
TODO...
```

## License

To the extent that lines of code have been copied from the Jawn codebase, they retain their original copyright and license, which is [The MIT License](https://opensource.org/licenses/MIT). Original code which is unique to Tectonic is licensed under [The Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0), copyright [SlamData](https://slamdata.com). Files which are substantially drawn from Jawn retain *both* copyright headers, as well as a special thank-you to the Jawn contributors.
