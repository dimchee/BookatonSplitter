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

#[async_std::main]
async fn main() -> tide::Result<()> {
    let mut app = tide::with_state(State::new()?);
    app.at("/").serve_file("../elm/index.html")?;
    app.at("/images/:n").get(images);
    app.at("/total")
        .get(|req: tide::Request<State>| async move { Ok(req.state().dir.len().to_string()) });
    // app.at("/orders/shoes").post(order_shoes);
    app.listen("127.0.0.1:8888").await?;
    Ok(())
}
