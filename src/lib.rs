use emacs::{defun, Env, Value};
use anyhow::Result;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "sem-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded sem-core!")
}

#[defun]
fn hello_world(env: &Env) -> Result<Value<'_>> {
    env.message("Hello world")
}
