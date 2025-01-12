use std::{collections::HashMap, fs::File, io::Write, io::Read, path};

use emacs::{defun, Env, IntoLisp, Value, Vector};
use anyhow::{anyhow, Result};
use ndarray::{Array1, Array2};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "sem-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded sem-core!")
}

struct Store {
    name: String,
    emb_size: usize,
    embeddings: Array2<f64>,
    idx_to_hash: HashMap<usize, String>,
    hash_to_idx: HashMap<String, usize>,
}

fn store_create(dir: &String, name: &String, emb_size: usize) -> Result<()> {
    let dir_path = path::Path::new(dir);

    let embeddings: Array2<f64> = Array2::<f64>::zeros((0, emb_size));
    let idx_to_hash: HashMap<usize, String> = HashMap::new();
    let hash_to_idx: HashMap<String, usize> = HashMap::new();

    let store = Store {
        name: name.to_string(),
        emb_size,
        embeddings,
        idx_to_hash,
        hash_to_idx,
    };

    store_write(dir_path, &store)
}

fn store_write(dir_path: &path::Path, store: &Store) -> Result<()> {
    let mut file = File::create(dir_path.join(format!("{}.hash_to_idx", store.name)))?;
    file.write_all(&bincode::serialize(&store.hash_to_idx).unwrap())?;

    let mut file = File::create(dir_path.join(format!("{}.matrix", store.name)))?;
    file.write_all(&bincode::serialize(&store.embeddings).unwrap())?;

    Ok(())
}

#[defun(user_ptr)]
fn store_new(dir: String, name: String, emb_size: usize) -> Result<Store> {
    if let Ok(_whatever)= store_create(&dir, &name, emb_size) {
        store_load(dir, name)
    } else {
        Err(anyhow!("Unable to create store {}", name))
    }
}

#[defun(user_ptr)]
fn store_load(dir: String, name: String) -> Result<Store> {
    let dir_path = path::Path::new(&dir);

    let mut file = File::open(dir_path.join(format!("{}.hash_to_idx", name)))?;
    let mut encoded_hash_to_idx = Vec::new();
    file.read_to_end(&mut encoded_hash_to_idx)?;
    let hash_to_idx: HashMap<String, usize> = bincode::deserialize(&encoded_hash_to_idx)?;
    let idx_to_hash: HashMap<usize, String> = hash_to_idx
        .clone()
        .into_iter()
        .map(|(key, value)| (value, key))
        .collect();

    let mut file = File::open(dir_path.join(format!("{}.matrix", name)))?;
    let mut encoded_embeddings = Vec::new();
    file.read_to_end(&mut encoded_embeddings)?;
    let embeddings: Array2<f64> = bincode::deserialize(&encoded_embeddings)?;

    Ok(Store {
        name,
        emb_size: embeddings.ncols(),
        embeddings,
        idx_to_hash,
        hash_to_idx,
    })
}

fn hash_content(content: String) -> String {
    sha256::digest(content)
}

// Add given item in the store and return index
#[defun]
fn add<'a>(env: &'a Env, dir: String, store: &mut Store, content: String, emb: Vector) -> Result<usize> {
    let n_emb = emb.len();

    if n_emb != store.emb_size {
        return Err(anyhow!("Embedding dimension doesn't match the store's."));
    }

    let row: Array1<f64> = (0..n_emb)
        .map(|i| env.call("aref", (emb, i)).unwrap().into_rust().unwrap() )
        .collect::<Array1<_>>();
    store.embeddings.push_row(row.view())?;

    let hash = hash_content(content);
    let idx = store.embeddings.nrows() - 1;
    store.hash_to_idx.insert(hash.clone(), idx);
    store.idx_to_hash.insert(idx, hash);

    // On every entry, we rewrite the whole store. This can be improved on.
    let dir_path = path::Path::new(&dir);
    store_write(dir_path, store)?;

    Ok(idx)
}

// If content is present return its index, else return nil
#[defun]
fn item_present_p<'a>(env: &'a Env, store: &mut Store, content: String) -> Result<Value<'a>> {
    let hash = hash_content(content);

    if store.hash_to_idx.contains_key(&hash) {
        let idx = store.hash_to_idx[&hash];
        Ok(idx.into_lisp(env)?)
    } else {
        Ok(env.intern("nil")?)
    }
}

#[defun]
fn similar<'a>(env: &'a Env, store: &mut Store, emb: Vector, k: usize) -> Result<Value<'a>> {
    Ok(env.intern("nil")?)
}
