use std::{fmt::Debug, fs::File, io::{Read, Write}, ops::BitOr, path, sync::Arc};

use emacs::{defun, Env, IntoLisp, Value, Vector};
use anyhow::{anyhow, Result};
use arrow_array::{builder::Float64BufferBuilder, types::{Float32Type, Float64Type}, Datum, FixedSizeListArray, Float64Array, RecordBatch, RecordBatchIterator, StringArray};
use arrow_schema::{DataType, Field, Schema};
use lancedb::query::ExecutableQuery;
use lancedb::index::Index;
use futures_util::TryStreamExt;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "sem-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded sem-core!")
}

struct Store {
    name: String,
    db: lancedb::connection::Connection
}

fn store_create(dir: &String, name: &String, emb_size: usize) -> Result<()> {
    let db_path = path::Path::new(dir).join(name);

    let rt = tokio::runtime::Runtime::new().unwrap();
    let db = rt.block_on(lancedb::connect(db_path.to_str().unwrap()).execute())?;

    let schema = Arc::new(Schema::new(vec![
        Field::new(
            "vector",
            DataType::FixedSizeList(Arc::new(Field::new("item", DataType::Float64, true)), emb_size as i32),
            true,
        ),
        Field::new(
            "content",
            DataType::Utf8,
            true
        )
    ]));

    rt.block_on(db.create_empty_table(name, schema).execute())?;

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
    let db_path = path::Path::new(&dir).join(&name);

    let rt = tokio::runtime::Runtime::new().unwrap();
    let db = rt.block_on(lancedb::connect(db_path.to_str().unwrap()).execute())?;

    Ok(Store { name, db, })
}

// Add given items in the store
#[defun]
fn add_batch<'a>(env: &'a Env, store: &mut Store, contents: Vector, embs: Vector) -> Result<()> {
    let n_items = embs.len();
    // TODO: Get rid of this hardcoding
    let dim = 384;

    let schema = Arc::new(Schema::new(vec![
        Field::new(
            "vector",
            DataType::FixedSizeList(Arc::new(Field::new("item", DataType::Float64, true)), dim),
            true,
        ),
        Field::new(
            "content",
            DataType::Utf8,
            true
        ),
    ]));

    let contents_vec: Vec<String> = (0..n_items)
        .map(|i| env.call("aref", (contents, i)).unwrap().into_rust().unwrap())
        .collect::<Vec<_>>();

    let embs_vec: Vec<Option<Vec<Option<f64>>>> = (0..n_items)
        .map(|i| {
            let row = env.call("aref", (embs, i)).unwrap();
            Some((0..dim).map(|j| Some(env.call("aref", (row, j)).unwrap().into_rust().unwrap())).collect::<Vec<_>>())
        })
        .collect::<Vec<_>>();

    let batches = RecordBatchIterator::new(
        vec![
            RecordBatch::try_new(
                schema.clone(),
                vec![
                    Arc::new(
                        FixedSizeListArray::from_iter_primitive::<Float64Type, _, _>(embs_vec, dim)
                    ),
                    Arc::new(StringArray::from(contents_vec)),
                ],
            )
            .unwrap()
        ]
            .into_iter()
            .map(Ok),
        schema.clone(),
    );

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        let table = store.db.open_table(&store.name)
            .execute().await
            .unwrap();

        table.add(batches)
            .execute().await
            .unwrap();
    });

    Ok(())
}

#[defun]
fn similar<'a>(env: &'a Env, store: &mut Store, emb: Vector, k: usize) -> Result<Value<'a>> {
    let vector: Vec<f64> = (0..emb.len())
        .map(|i| env.call("aref", (emb, i)).unwrap().into_rust().unwrap())
        .collect::<Vec<_>>();

    let rt = tokio::runtime::Runtime::new().unwrap();
    let results = rt.block_on(async {
        store.db.open_table(&store.name)
            .execute().await
            .unwrap()
            .query()
            .nearest_to(vector.clone())
            .unwrap()
            .execute().await
            .unwrap()
            .try_collect::<Vec<_>>().await
            .unwrap()
    });

    let mut output: Vec<Value> = Vec::with_capacity(k);
    let mut n_done: usize = 0;
    let dim = 384;

    for batch in results.into_iter() {
        if n_done >= k { break }
        // 0 -> vector, 1 -> content
        let n_col = batch.num_columns();
        if n_col < 2 {
            return Err(anyhow!("Query result had incorrect number of columns"));
        }
        let embeddings = batch.column(0).as_any().downcast_ref::<FixedSizeListArray>().unwrap();
        let contents = batch.column(1).as_any().downcast_ref::<StringArray>().unwrap();

        for i in 0..batch.num_rows() {
            if n_done >= k { break }
            let embedding: Vec<f64> = (0..dim)
                .map(|j| embeddings.value(i).as_any().downcast_ref::<Float64Array>().unwrap().value(j))
                .collect();

            let content = contents.value(i).to_string();
            // This is something that should be pulled from the library, but
            // since k is limited, we will just compute it right away.  Also the
            // assumption is that the vectors are normalized.
            let score: f64 = vector.iter().zip(embedding.iter()).map(|(x, y)| x * y).sum();
            output.push(env.cons(score, content)?);
            n_done += 1;
        }
    }

    Ok(env.vector(&output)?)
}
