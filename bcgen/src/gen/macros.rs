macro_rules! q {
    ([$($tt:tt)*]) => {{
        quote::quote!($($tt)*)
    }};
    ($buf:expr, [$($tt:tt)*]) => {{
        let _ = $buf.write_fmt(format_args!("{}\n", quote::quote!($($tt)*)));
    }};
}

macro_rules! p {
    ($buf:expr, $s:expr) => {{
        let _ = $buf.write_str($s);
        let _ = $buf.write_char('\n');
    }};
}

macro_rules! nl {
    ($buf:expr) => {{
        let _ = $buf.write_char('\n');
    }};
}
