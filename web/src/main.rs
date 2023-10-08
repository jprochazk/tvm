use leptos::*;

fn to_ast(src: &str) -> String {
  let (ast, errors) = tvm::syn::Parser::new(src).parse();
  let ast = format!("{ast:#?}");
  let errors = errors
    .into_iter()
    .map(|e| format!("{}", e.with_src(src)))
    .collect::<Vec<_>>()
    .join("\n");
  format!("{errors}\n\n{ast}")
}

#[component]
fn App() -> impl IntoView {
  let (source, set_source) = create_signal(String::new());
  let ast = move || to_ast(&source());

  view! {
    <div class="split">
      <textarea
        on:input=move |ev| {
          set_source(event_target_value(&ev));
        }
        prop:value=source
      />
      <textarea readonly prop:value=ast />
    </div>
  }
}

fn main() {
  mount_to_body(|| view! { <App /> })
}
