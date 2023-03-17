use std::io::prelude::*;

#[derive(Clone)]
struct State {
    dir: std::sync::Arc<Vec<std::fs::DirEntry>>,
}

impl State {
    fn new() -> tide::Result<Self> {
        let mut dir = std::fs::read_dir("../../Scan")?
            .filter_map(|res| res.ok())
            .collect::<Vec<_>>();
        dir.sort_by_key(|x| x.file_name());
        Ok(Self {
            dir: std::sync::Arc::new(dir),
        })
    }
}

async fn images(req: tide::Request<State>) -> tide::Result<tide::Response> {
    let n: usize = req.param("n")?.parse().unwrap_or(0);
    let state = req.state();
    Ok(tide::Response::builder(tide::StatusCode::Ok)
        .body(
            tide::Body::from_file(
                state
                    .dir
                    .get(n)
                    .ok_or(tide::Error::from_str(tide::StatusCode::Ok, "already last"))?
                    .path(),
            )
            .await?,
        )
        .content_type(tide::http::mime::JPEG)
        .build())
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
enum Split {
    Before,
    After,
    Middle(f64),
}

type Modifications = std::collections::HashMap<u64, Split>;

async fn save_data(mut req: tide::Request<State>) -> tide::Result {
    let mods: Modifications = req.body_json().await?;
    std::fs::File::create("../../saved.json")?.write_all(serde_json::to_string(&mods)?.as_bytes())?;
    println!("mods: {:?}", mods);
    // let state = req.state();
    Ok(format!("Saved {} modifications", mods.len()).into())
}
async fn load_data(_req: tide::Request<State>) -> tide::Result {
    let mut buff = String::new();
    std::fs::File::open("../../saved.json")?.read_to_string(&mut buff)?;
    let mods : Modifications = serde_json::from_str(&buff)?;
    Ok(tide::Response::builder(tide::StatusCode::Ok)
        .body(tide::Body::from_json(&mods)?)
        .content_type(tide::http::mime::JSON)
        .build())
}

#[async_std::main]
async fn main() -> tide::Result<()> {
    let mut app = tide::with_state(State::new()?);
    app.at("/").serve_file("../elm/index.html")?;
    app.at("/images/:n").get(images);
    app.at("/total")
        .get(|req: tide::Request<State>| async move { Ok(req.state().dir.len().to_string()) });
    app.at("/save").post(save_data);
    app.at("/load").get(load_data);
    // app.at("/orders/shoes").post(order_shoes);
    app.listen("127.0.0.1:8888").await?;
    Ok(())
}
