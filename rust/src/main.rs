use std::{io::prelude::*, path::PathBuf};

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

#[derive(Debug, serde::Deserialize, serde::Serialize, PartialEq)]
enum Split {
    Before,
    After,
    Middle(f64),
}

type Modifications = std::collections::HashMap<usize, Split>;

async fn save_data(mut req: tide::Request<State>) -> tide::Result {
    let mods: Modifications = req.body_json().await?;
    std::fs::File::create("../../saved.json")?
        .write_all(serde_json::to_string(&mods)?.as_bytes())?;
    println!("mods: {:?}", mods);
    // let state = req.state();
    Ok(format!("Saved {} modifications", mods.len()).into())
}
async fn load_data(_req: tide::Request<State>) -> tide::Result {
    let mut buff = String::new();
    std::fs::File::open("../../saved.json")?.read_to_string(&mut buff)?;
    let mods: Modifications = serde_json::from_str(&buff)?;
    Ok(tide::Response::builder(tide::StatusCode::Ok)
        .body(tide::Body::from_json(&mods)?)
        .content_type(tide::http::mime::JSON)
        .build())
}

fn split_image(percentage: f64, file: &PathBuf) -> (PathBuf, PathBuf) {
    let dir = std::env::temp_dir();
    let name = file.file_name().unwrap().to_str().unwrap().to_owned();
    let (mut start, mut end) = (dir.clone(), dir.clone());
    start.push(name.clone() + "crop-start.jpg");
    end.push(name.clone() + "crop-end.jpg");
    println!("  Spliting file {}", file.display());
    std::process::Command::new("convert")
        .arg(file)
        .arg("-crop")
        .arg("0x".to_owned() + &(percentage + 2.0).to_string() + "%+0")
        .arg(start.clone())
        .output()
        .expect(&format!("Failed to split file-start: {}", start.display()));
    std::process::Command::new("convert")
        .arg(file)
        .args([ "-gravity", "South", "-crop" ])
        .arg("0x".to_owned() + &(102.0 - percentage).to_string() + "%+0")
        .arg(end.clone())
        .output()
        .expect(&format!("Failed to split file-end: {}", end.display()));
    // out.status.exit_ok().unwrap_or_else(|_|
    //     println!("Errors: {}", out.stderr)
    // )
    (start, end)
}

fn to_pdfs(state: &State, mods: &Modifications) {
    state
        .dir
        .iter()
        .enumerate()
        .fold(
            vec![("1".to_string(), vec![])],
            |mut groups, (ind, file)| {
                match mods.get(&ind) {
                    None => groups.last_mut().unwrap().1.push(file.path()),
                    Some(split) => match split {
                        Split::Before => {
                            groups.last_mut().unwrap().0 += &("-".to_string() + &ind.to_string());
                            groups.push(((ind + 1).to_string(), vec![file.path()]))
                        }
                        Split::After => {
                            groups.last_mut().unwrap().1.push(file.path());
                            groups.last_mut().unwrap().0 +=
                                &("-".to_string() + &(ind + 1).to_string());
                            groups.push(((ind + 1).to_string(), vec![]))
                        }
                        Split::Middle(p) => {
                            let (start, end) = split_image(*p, &file.path());
                            groups.last_mut().unwrap().1.push(start);
                            groups.last_mut().unwrap().0 +=
                                &("-".to_string() + &(ind + 1).to_string());
                            groups.push(((ind + 1).to_string(), vec![end]))
                        }
                    },
                }
                groups
            },
        )
        .iter()
        .for_each(|(name, group)| {
            if group.len() < 100 {
                println!("making pdf from group {}", name);
                std::process::Command::new("convert")
                    .args(group)
                    .arg("../../Parts/".to_owned() + name + ".pdf")
                    .output()
                    .expect(&format!("Failed to merge pages {}", name));
            } else {
                println!("Sorry, can't make pdf with more than 100 pages");
            }
        });
}

async fn topdf(req: tide::Request<State>) -> tide::Result {
    let mut buff = String::new();
    std::fs::File::open("../../saved.json")?.read_to_string(&mut buff)?;
    let mods: Modifications = serde_json::from_str(&buff)?;
    to_pdfs(req.state(), &mods);
    Ok("saved to pdfs!".into())
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
    app.at("/topdf").get(topdf);
    // app.at("/orders/shoes").post(order_shoes);
    app.listen("127.0.0.1:8888").await?;
    Ok(())
}
