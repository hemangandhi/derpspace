extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate reqwest;
extern crate cursive;

use std::collections::BTreeMap;
use cursive::Cursive;
use cursive::views::Dialog;
use cursive::views::SelectView;

#[derive (Serialize, Deserialize)]
struct LightState{
    on: bool,
    hue: i32,
    sat: i32,
    bri: i32,
    effect: String,
    xy: Vec<f32>,
    ct: i32,
    alert: String,
    colormode: String,
    reachable: bool
}

#[derive (Serialize, Deserialize)]
struct Light{
    state: LightState,
    #[serde(rename = "type")]
    lighttype: String,
    name: String,
    modelid: String,
    manufacturername: String,
    uniqueid: String,
    swversion: String
}

fn show_error<S>(siv: &mut Cursive, err: S) where S: Into<String>{
    siv.add_layer(Dialog::text(err).button("Exit", |s| s.quit()));
}

mod cursive_io {
    extern crate cursive;

    use cursive::Cursive;
    use cursive::views::Dialog;
    use cursive::views::EditView;

    pub fn prompt_user<Ret, F, S, A>(prompt: S, siv: &mut Cursive, out: &F, and_then: &A)
    where F: Fn(&str) -> Result<Ret, &str> + 'static, S: Into<String> + 'static,
          A: Fn(&mut Cursive, Ret) + 'static{
        siv.add_layer(Dialog::around(EditView::new()
                                        .on_submit(move |s, txt| {
            match out(txt) {
                Ok(r) => and_then(s, r),
                Err(es) => prompt_user(prompt, s, out, and_then)
            }
        })))
    }
}

fn alter_light(siv: &mut Cursive, light: &Light){

}

fn select_light(siv: &mut Cursive, ip: String) {
    let mut resp = match reqwest::get(format!("http://{}/api/newdeveloper/lights", ip).as_str()) {
        Result::Ok(resp) => resp,
        Result::Err(_) => {
            show_error(siv, "Could not connect to API.");
            return
        }
    };

    let body = match resp.text() {
        Result::Ok(json) => json,
        Result::Err(_) => {
            show_error(siv, "Could not connect to API.");
            return
        }
    };

    match serde_json::from_str::<BTreeMap<u8, Light>>(&body){
        Result::Ok(lights) => {
            let mut sel = SelectView::<u8>::new();
            for (id, light) in lights.iter(){
                sel.add_item(format!("Name: {}", light.name), *id);
            }

            siv.add_layer(Dialog::around(sel.on_submit(move |s, it| {
                s.pop_layer();
                alter_light(s, &lights[it]);
            })).title("Choose a light"));
        },
        Result::Err(e) => {
            show_error(siv, format!("Could not parse body: {}", e));
            return
        }
    }
}

fn main() {
    let mut siv = Cursive::new();

    select_light(&mut siv, "192.168.7.23".to_owned());

    siv.run();
}
