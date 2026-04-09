use super::parse;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ok(src: &str) {
    parse(src).unwrap_or_else(|e| panic!("Expected Ok, got error: {e}"));
}

fn err(src: &str) {
    if parse(src).is_ok() {
        panic!("Expected parse error for input: {src:?}");
    }
}

// ---------------------------------------------------------------------------
// Basic constructs
// ---------------------------------------------------------------------------

#[test]
fn empty_input() {
    ok("");
}

#[test]
fn comment_only() {
    ok("-- just a comment\n--[[ block comment ]]");
}

#[test]
fn local_assignment_number() {
    ok("local x = 42");
}

#[test]
fn local_assignment_string() {
    ok(r#"local s = "hello, world""#);
}

#[test]
fn global_assignment() {
    ok("x = 100");
}

#[test]
fn multiple_assignment() {
    ok("local a, b, c = 1, 2, 3");
}

#[test]
fn arithmetic_expression() {
    ok("local x = 1 + 2 * 3 - 4 / 2 % 3");
}

#[test]
fn bitwise_expression() {
    ok("local x = 0xFF & 0x0F | 0x10 ~ 0x01");
}

#[test]
fn exponentiation() {
    ok("local x = 2 ^ 10");
}

#[test]
fn string_concatenation() {
    ok(r#"local s = "hello" .. ", " .. "world""#);
}

#[test]
fn length_operator() {
    ok("local t = {}; local n = #t");
}

#[test]
fn boolean_literals() {
    ok("local a = true; local b = false");
}

#[test]
fn nil_literal() {
    ok("local x = nil");
}

#[test]
fn function_call() {
    ok(r#"print("hello")"#);
}

#[test]
fn method_call() {
    ok(r#"io.write("hello\n")"#);
}

#[test]
fn chained_calls() {
    ok("f()(x)(y)");
}

#[test]
fn if_statement() {
    ok("if true then print(1) end");
}

#[test]
fn if_else_statement() {
    ok("if x then print(1) else print(2) end");
}

#[test]
fn if_elseif_else() {
    ok("if a then x = 1 elseif b then x = 2 elseif c then x = 3 else x = 4 end");
}

#[test]
fn while_loop() {
    ok("while true do break end");
}

#[test]
fn repeat_until() {
    ok("local i = 0; repeat i = i + 1 until i >= 10");
}

#[test]
fn numeric_for() {
    ok("for i = 1, 10 do print(i) end");
}

#[test]
fn numeric_for_with_step() {
    ok("for i = 10, 1, -1 do print(i) end");
}

#[test]
fn generic_for() {
    ok("for k, v in pairs({}) do print(k, v) end");
}

#[test]
fn function_definition() {
    ok("function greet(name) return \"Hello, \" .. name end");
}

#[test]
fn local_function() {
    ok("local function fact(n) if n <= 1 then return 1 end return n * fact(n - 1) end");
}

#[test]
fn anonymous_function() {
    ok("local f = function(a, b) return a + b end");
}

#[test]
fn varargs_function() {
    ok("local function sum(...) local s = 0 for _, v in ipairs({...}) do s = s + v end return s end");
}

#[test]
fn table_constructor_empty() {
    ok("local t = {}");
}

#[test]
fn table_constructor_array() {
    ok("local t = {1, 2, 3, 4, 5}");
}

#[test]
fn table_constructor_record() {
    ok(r#"local t = { name = "Alice", age = 30 }"#);
}

#[test]
fn table_constructor_mixed() {
    ok(r#"local t = { "a", x = 1, [10] = true }"#);
}

#[test]
fn table_access() {
    ok("local t = {}; t.x = 1; local v = t.x");
}

#[test]
fn table_index_access() {
    ok("local t = {}; t[1] = 99; local v = t[1]");
}

#[test]
fn nested_functions_and_closures() {
    ok("
        local function outer(x)
            local function inner(y) return x + y end
            return inner
        end
    ");
}

#[test]
fn multiline_string() {
    ok("local s = [[line one\nline two]]");
}

#[test]
fn multiple_returns() {
    ok("local function swap(a, b) return b, a end");
}

#[test]
fn goto_and_label() {
    ok("::continue:: goto continue");
}

#[test]
fn do_block() {
    ok("do local x = 1 end");
}

#[test]
fn comparison_operators() {
    ok("local r = (1 == 1) and (2 ~= 3) and (1 < 2) and (2 > 1) and (1 <= 1) and (2 >= 2)");
}

#[test]
fn logical_operators() {
    ok("local x = true and false or nil");
}

#[test]
fn not_operator() {
    ok("local x = not true");
}

#[test]
fn unary_minus() {
    ok("local x = -42");
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

#[test]
fn error_unclosed_string() {
    err(r#"local x = "unterminated"#);
}

#[test]
fn error_unexpected_end() {
    err("if true then");
}

#[test]
fn error_bare_end() {
    err("end");
}

#[test]
fn error_missing_then() {
    err("if true print(x) end");
}

// ---------------------------------------------------------------------------
// Lua complete test suite
//
// Set the `LUA_TEST_SUITE_DIR` environment variable to the path of an
// unpacked Lua test suite directory (e.g. lua-5.4.8-tests/) then run:
//
//   cargo test lua_test_suite_complete -- --include-ignored
//
// Every .lua file found in that directory will be fed through the parser and
// is expected to parse without error.
// ---------------------------------------------------------------------------

#[test]
#[ignore = "Requires local Lua test suite. Set LUA_TEST_SUITE_DIR and run with --include-ignored."]
fn lua_test_suite() {
    let suite_dir = std::env::var("LUA_TEST_SUITE_DIR")
        .unwrap_or_else(|_| "lua-tests".to_owned());

    let suite_path = std::path::Path::new(&suite_dir);
    assert!(
        suite_path.is_dir(),
        "LUA_TEST_SUITE_DIR `{suite_dir}` is not a directory"
    );

    let lua_files = collect_lua_files(suite_path);
    assert!(!lua_files.is_empty(), "No .lua files found in `{suite_dir}`");

    let mut failures: Vec<(std::path::PathBuf, String)> = Vec::new();

    for path in &lua_files {
        let src = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("Could not read {}: {e}", path.display()));

        if let Err(e) = parse(src.as_str()) {
            failures.push((path.clone(), e.to_string()));
        }
    }

    if !failures.is_empty() {
        let mut msg = format!("{} file(s) failed to parse:\n", failures.len());
        for (path, e) in &failures {
            msg.push_str(&format!("  {}: {}\n", path.display(), e));
        }
        panic!("{msg}");
    }

    println!("Parsed {} Lua files without error.", lua_files.len());
}

/// Recursively collect all `.lua` files under `dir`.
fn collect_lua_files(dir: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut result = Vec::new();
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                result.extend(collect_lua_files(&path));
            } else if path.extension().and_then(|e| e.to_str()) == Some("lua") {
                result.push(path);
            }
        }
    }
    result
}
