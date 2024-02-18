use std::borrow::{Borrow, BorrowMut};
use std::fmt::Display;
use std::sync::{Arc, Mutex};

use hashbrown::HashMap;
use serde::Serialize;
use serde_json::{Map, Value};

#[allow(dead_code)]
#[derive(Clone)]
pub struct TranslateContext {
    client: Option<()>,
}

impl TranslateContext {
    pub fn noop() -> TranslateContext {
        TranslateContext { client: None }
    }

    pub fn build(client: ()) -> TranslateContext {
        TranslateContext {
            client: Some(client),
        }
    }
}

// translate
//
// A poor man's jq.

#[derive(Clone, Debug, PartialEq)]
pub enum Language {
    At(String),                            // .foo
    Array(Box<Language>),                  // map( ... )
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

impl Language {
    pub fn array(arr: Language) -> Language {
        Language::Array(Box::new(arr))
    }
    pub fn at(key: &str) -> Language {
        Language::At(String::from(key))
    }
    pub fn default(def: Language) -> Language {
        Language::Default(Box::new(def))
    }
    pub fn get(key: &str) -> Language {
        Language::Get(String::from(key))
    }
    pub fn and_then(&self, next: Language) -> Language {
        Language::AndThen(Box::new(self.clone()), Box::new(next))
    }
    pub fn set(key: &str) -> Language {
        Language::Set(String::from(key))
    }
}

#[derive(Debug, Serialize)]
pub struct StepError {
    history: Vec<String>,
    choices: Option<Value>,
}

impl StepError {
    fn new(root: String) -> StepError {
        StepError {
            history: vec![root],
            choices: None,
        }
    }

    fn prepend_history(mut self, before: String) -> StepError {
        self.history.insert(0, before);
        self
    }
}

impl std::error::Error for StepError {}

impl Display for StepError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "StepError({}, {})",
            self.history.join(", "),
            self.choices
                .clone()
                .map_or(String::from("[]"), |cs| format!("{}", cs))
        )
    }
}

pub type State = Arc<Mutex<HashMap<String, Arc<Value>>>>;

pub fn make_state() -> State {
    Arc::new(Mutex::new(HashMap::new()))
}

pub fn step(
    ctx: &TranslateContext,
    prog: &Language,
    current: &Value,
    state: State,
) -> Result<Value, StepError> {
    match prog {
        Language::At(key) => Ok(current
            .get(key)
            .ok_or_else(|| StepError {
                history: vec![key.clone()],
                choices: current
                    .as_object()
                    .map(|o| Value::Array(o.keys().map(|x| Value::String(x.to_owned())).collect())),
            })?
            .clone()),
        Language::Array(next) => Ok(Value::Array(
            current
                .as_array()
                .ok_or_else(|| StepError::new(String::from("<Not an array>")))?
                .iter()
                .enumerate()
                .map(|(i, x)| {
                    step(ctx, next, x, state.clone())
                        .map_err(|se| se.prepend_history(format!("[{}]", i)))
                })
                .collect::<Result<Vec<Value>, StepError>>()?,
        )),
        Language::Object(pairs) => Ok(Value::Object(
            pairs
                .iter()
                .map(|(k, v)| step(ctx, v, current, state.clone()).map(|v| (k.clone(), v)))
                .collect::<Result<Map<String, Value>, StepError>>()?,
        )),
        Language::List(elems) => Ok(Value::Array(
            elems
                .iter()
                .map(|v| step(ctx, v, current, state.clone()))
                .collect::<Result<Vec<Value>, StepError>>()?,
        )),
        Language::Splat(each) => {
            let result = each
                .iter()
                .map(|next| step(ctx, next, current, state.clone()))
                .collect::<Result<Vec<Value>, StepError>>()?;
            Ok(result.last().unwrap().clone())
        }
        Language::Set(into) => {
            state
                .lock()
                .map_err(|_e| StepError::new(format!("Set({})", into)))?
                .borrow_mut()
                .insert(into.clone(), Arc::new(current.clone()));
            Ok(current.clone())
        }
        Language::Get(from) => {
            let mutex = state
                .lock()
                .map_err(|_e| StepError::new(String::from("<Unable to acquire mutex lock>")))?;
            let _state = mutex.borrow();
            let needle = _state
                .get(from)
                .ok_or_else(|| StepError::new(format!("Get({})", from)))?;
            Ok((**needle).clone())
        }
        Language::Const(value) => Ok(value.clone()),
        Language::Identity => Ok(current.clone()),
        Language::EmitEvent(_topic) => {
            // Once a generic client is added back in, this can be re-added.
            // if let Some(client) = &ctx.client {
            //     client.emit(topic, current);
            // }
            Ok(current.clone())
        }
        Language::AndThen(first, second) => {
            let intermediate = step(ctx, first, current, state.clone())?;
            step(ctx, second, &intermediate, state)
        }
        Language::Length => match current {
            Value::Array(vec) => Ok(Value::Number(serde_json::Number::from(vec.len()))),
            Value::Object(map) => Ok(Value::Number(serde_json::Number::from(map.len()))),
            Value::String(x) => Ok(Value::Number(serde_json::Number::from(x.len()))),
            other => {
                log::warn!("Attempted to call size on an unsized object: {:?}", other);
                Ok(Value::Null)
            }
        },
        Language::Join(by) => match current {
            Value::Array(vec) => {
                let mut elems: Vec<String> = vec![];
                for elem in vec.iter() {
                    match elem {
                        Value::String(x) => elems.push(x.clone()),
                        other => {
                            log::warn!("Attempted to join with a non-stringy array: {:?}", other)
                        }
                    }
                }

                Ok(Value::String(elems.join(by)))
            }
            other => {
                log::warn!("Attempted to join on an unexpected type: {:?}", other);
                Err(StepError {
                    history: vec![],
                    choices: None,
                })
            }
        },
        Language::Default(prog) => match current {
            Value::Null => step(ctx, prog, &Value::Null, state),
            other => Ok(other.clone()),
        },
        Language::Flatten => match current {
            Value::Array(vec) => {
                let mut out: Vec<Value> = vec![];
                for elem in vec.clone().iter_mut() {
                    match elem {
                        Value::Array(vec) => {
                            out.append(vec);
                        }
                        _ => panic!("Child was not an array!"),
                    }
                }
                Ok(Value::Array(out))
            }
            _ => panic!("Child was not an array!"),
        },
        Language::ToString => match current {
            Value::String(value) => Ok(Value::String(value.to_string())),
            other => match serde_json::to_string(other) {
                Ok(value) => Ok(Value::String(value.to_string())),
                Err(_err) => Err(StepError {
                    history: vec![],
                    choices: None,
                }),
            },
        },
    }
}

#[test]
fn translate_error_at() {
    let ctx = TranslateContext::noop();
    use serde_json::json;
    let prog = Language::at("foo");

    let given = json!({ "bar": "baz" });
    if let Some(StepError {
        choices: _,
        history,
    }) = step(&ctx, &prog, &given, make_state()).err()
    {
        assert_eq!(history, vec!["foo"]);
    }
}

#[test]
fn translate_error_array() {
    let ctx = TranslateContext::noop();
    use serde_json::json;
    let prog = Language::array(Language::at("foo"));

    let given = json!([{ "bar": "baz" }]);
    if let Some(StepError {
        choices: _,
        history,
    }) = step(&ctx, &prog, &given, make_state()).err()
    {
        assert_eq!(history, vec!["[0]", "foo"]);
    }
}

#[test]
fn translate_error_focus() {
    let ctx = TranslateContext::noop();
    use serde_json::json;
    let prog = Language::at("foo").and_then(Language::at("bar"));

    let given = json!({ "baz": "blix" });
    if let Some(StepError {
        choices: _,
        history,
    }) = step(&ctx, &prog, &given, make_state()).err()
    {
        assert_eq!(history, vec!["foo"]);
    }
}

#[test]
fn translate_error_object() {
    let ctx = TranslateContext::noop();
    use serde_json::json;
    let prog = Language::Object(vec![
        (String::from("foo"), Language::at("foo")),
        (String::from("bar"), Language::at("bar")),
    ]);

    let given = json!({ "foo": "foo" });
    if let Some(StepError {
        choices: _,
        history,
    }) = step(&ctx, &prog, &given, make_state()).err()
    {
        assert_eq!(history, vec!["bar"]);
    }
}

#[test]
fn translate_test() {
    let ctx = TranslateContext::noop();
    use serde_json::json;
    let prog = Language::at("results").and_then(Language::Object(vec![(
        String::from("ids"),
        Language::array(Language::at("product_variant_id")),
    )]));

    let given = json!({ "q": "Foo", "results": [{"product_variant_id": "12313bb7-6068-4ec9-ac49-3e834181f127"}] });
    let expected = json!({ "ids": [ "12313bb7-6068-4ec9-ac49-3e834181f127" ] });

    assert_eq!(step(&ctx, &prog, &given, make_state()).unwrap(), expected);
}
