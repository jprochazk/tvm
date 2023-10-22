use leptos::*;

fn print_ast(src: &str) -> String {
  let (ast, errors) = tvm::syn::parse(src);
  let ast = format!("{ast:#?}");
  let errors = errors
    .into_iter()
    .map(|e| format!("{}", e.with_src(src)))
    .collect::<Vec<_>>()
    .join("\n");

  if errors.is_empty() {
    ast
  } else {
    [errors, ast].join("\n=================\n\n")
  }
}

fn print_hir(src: &str) -> String {
  match tvm::syn::try_parse(src).and_then(tvm::ty::check) {
    Ok(hir) => format!("{hir}"),
    Err(e) => e
      .into_iter()
      .map(|e| format!("{}", e.with_src(src)))
      .collect::<Vec<_>>()
      .join("\n"),
  }
}

const LOCAL_STORAGE_KEY: &str = "state";

fn set_local_state(v: &str) {
  let Some(storage) = window().local_storage().ok().flatten() else {
    return;
  };
  _ = storage.set_item(LOCAL_STORAGE_KEY, v);
}

fn get_local_state() -> String {
  match window()
    .local_storage()
    .ok()
    .flatten()
    .and_then(|storage| storage.get_item(LOCAL_STORAGE_KEY).ok().flatten())
  {
    Some(value) => value,
    None => String::new(),
  }
}

#[component]
fn App() -> impl IntoView {
  let (source, set_source) = create_signal(get_local_state());
  let print = create_memo(move |_| print_hir(&source()));

  create_effect(move |_| set_local_state(&source()));

  view! {
    <div class="split">
      <textarea
        on:input=move |ev| {
          logging::log!("value changed");
          set_source(event_target_value(&ev));
        }
        prop:value=source
      />
      <textarea readonly prop:value=print />
    </div>
  }
}

fn main() {
  _ = console_log::init_with_level(log::Level::Debug);
  console_error_panic_hook::set_once();
  mount_to_body(|| view! { <App /> })
}
