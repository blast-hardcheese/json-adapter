# json-adapter

json-adapter, a subset of combinators inspired by jq for doing JSON transformations. A part of the [delegator](https://delegator.dev/) architecture.

A number of operations are available. For a complete list, see [./src/language.rs](./src/language.rs).

```rust
pub enum Language {
    At(String),                            // .foo
    Map(Box<Language>),                    // map( ... )
    Object(Vec<(String, Language)>),       // { foo: .foo, bar: .bar  }
    List(Vec<Language>),                   // [ .foo, .bar, .baz ]
    Splat(Vec<Language>),                  // .foo, .bar
    Set(String),                           // ... | set("foo")
    Get(String),                           // get("bar") | ...
    Const(Value),                          // const(...)
    Identity,                              // .
    AndThen(Box<Language>, Box<Language>), // ... | ...
    Length,                                // [...] | length
    Join(String),                          // [...] | join(",")
    Default(Box<Language>),                // ... | default(<lang>)
    Flatten,                               // ... | flatten | ...
    ToString,                              // ... | tostring | ...
    EmitEvent(String),                     // ... | emit("topic")
}
```

## Example

Below is a simple example demonstrating how to use the library to parse and transform JSON data:

```rust
use serde_json::json;
let data = json!({ "foo": "bar", "nested": { "key": "value" } });

let lang = Language::Object(vec![
    (String::from("foo"), Language::at("foo")),
    (String::from("nested_key"), Language::at("nested").and_then(Language::at("key"))),
]);

let transformed = step(&TranslateContext::noop(), &lang, &data, make_state()).unwrap();
println!("{:?}", transformed); // Prints: {"foo": "bar", "nested_key": "value"}
```
