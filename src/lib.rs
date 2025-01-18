use std::{path, sync::Arc};

use emacs::{defun, Env, IntoLisp, Value, Vector};
use anyhow::{anyhow, Result};
use arrow_array::{types::Float64Type, FixedSizeListArray, Float64Array, RecordBatch, RecordBatchIterator, StringArray};
use arrow_schema::{DataType, Field, Schema};
use lancedb::{index::Index, query::ExecutableQuery, table::OptimizeAction};
use futures_util::TryStreamExt;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "sem-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded sem-core!")
}

struct Database {
    name: String,
    connection: lancedb::connection::Connection,
}

fn get_table_dim(db: &mut Database, table_name: &String) -> Result<usize> {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let schema = rt.block_on(async {
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        table.schema().await.unwrap()
    });
    let f = schema.field_with_name("vector");
    if let DataType::FixedSizeList(_, size) = f.unwrap().data_type() {
        Ok(*size as usize)
    } else {
        Err(anyhow!("Unable to read dimension from {} for table: {}", db.name, table_name))
    }
}

fn db_create(dir: &String, name: &String) -> Result<()> {
    let db_path = path::Path::new(dir).join(name);

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(lancedb::connect(db_path.to_str().unwrap()).execute())?;

    Ok(())
}

#[defun(user_ptr)]
fn db_new(dir: String, name: String) -> Result<Database> {
    if let Ok(_whatever)= db_create(&dir, &name) {
        db_load(dir, name)
    } else {
        Err(anyhow!("Unable to create database {}", name))
    }
}

#[defun(user_ptr)]
fn db_load(dir: String, name: String) -> Result<Database> {
    let db_path = path::Path::new(&dir).join(&name);

    let rt = tokio::runtime::Runtime::new().unwrap();
    let connection = rt.block_on(lancedb::connect(db_path.to_str().unwrap()).execute())?;

    Ok(Database { name, connection, })
}

#[defun]
fn table_list<'a>(env: &'a Env, db: &mut Database) -> Result<Value<'a>> {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let table_names = rt.block_on(async {
        db.connection.table_names().execute().await.unwrap()
    });

    Ok(env.list(&table_names
        .iter()
        .map(|it| it.into_lisp(env))
        .collect::<Result<Vec<_>>>()?
    )?)
}

// Create a new table for storing string content mapping to vector
// representation
#[defun]
fn table_new(db: &mut Database, table_name: String, dim: usize) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let schema = Arc::new(Schema::new(vec![
        Field::new(
            "vector",
            DataType::FixedSizeList(Arc::new(Field::new("item", DataType::Float64, true)), dim as i32),
            true,
        ),
        Field::new(
            "content",
            DataType::Utf8,
            true
        )
    ]));

    rt.block_on(async {
        let table = db.connection.create_empty_table(table_name, schema).execute().await.unwrap();
        // Creating a default index on content for operations that match items
        // before insertion
        table.create_index(&["content"], Index::Auto)
            .execute().await
            .unwrap();
    });

    Ok(())
}

#[defun]
fn table_present_p(db: &mut Database, table_name: String) -> Result<bool> {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let table_names = rt.block_on(async {
        db.connection.table_names().execute().await.unwrap()
    });

    Ok(table_names.contains(&table_name))
}

// Delete the table
#[defun]
fn table_delete(db: &mut Database, table_name: String) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
        db.connection.drop_table(table_name).await.unwrap();
    });
    Ok(())
}

// Return dimension of vectors that this table handles
#[defun]
fn table_dim(db: &mut Database, table_name: String) -> Result<usize> {
    get_table_dim(db, &table_name)
}

#[defun]
fn build_index(db: &mut Database, table_name: String) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        // We only need to index vector as we don't find by `content` here.
        table.create_index(&["vector"], Index::Auto)
            .execute().await
            .unwrap();
    });

    Ok(())
}

#[defun]
fn optimize(db: &mut Database, table_name: String) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        table.optimize(OptimizeAction::All).await.unwrap();
    });

    Ok(())
}

// Add given items in the store
#[defun]
fn add_batch<'a>(env: &'a Env, db: &mut Database, table_name: String, contents: Vector, embs: Vector) -> Result<()> {
    let n_items = embs.len();
    let dim = get_table_dim(db, &table_name)?;

    let schema = Arc::new(Schema::new(vec![
        Field::new(
            "vector",
            DataType::FixedSizeList(Arc::new(Field::new("item", DataType::Float64, true)), dim as i32),
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
                        FixedSizeListArray::from_iter_primitive::<Float64Type, _, _>(embs_vec, dim as i32)
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
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        table.add(batches)
            .execute().await
            .unwrap();
    });

    Ok(())
}

#[defun]
fn items_count(db: &mut Database, table_name: String) -> Result<usize> {
    let rt = tokio::runtime::Runtime::new().unwrap();

    Ok(rt.block_on(async {
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        table.count_rows(None).await.unwrap()
    }))
}

#[defun]
fn similar<'a>(env: &'a Env, db: &mut Database, table_name: String, emb: Vector, k: usize) -> Result<Value<'a>> {
    let vector: Vec<f64> = (0..emb.len())
        .map(|i| env.call("aref", (emb, i)).unwrap().into_rust().unwrap())
        .collect::<Vec<_>>();

    let dim = get_table_dim(db, &table_name)?;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let results = rt.block_on(async {
        db.connection.open_table(table_name)
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

// Delete all items from given table
#[defun]
fn delete_all(db: &mut Database, table_name: String) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        let table = db.connection.open_table(table_name)
            .execute().await
            .unwrap();

        table.delete("1 = 1").await.unwrap();
    });

    Ok(())
}

#[defun]
fn db_name(db: &mut Database) -> Result<String> {
    Ok(db.name.clone())
}
