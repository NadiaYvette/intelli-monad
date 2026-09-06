// The Rust island's real logic, rustc-compiled with a C ABI export.
// A pure function; needs no Rust runtime init at the crossing.
// `rs_island_factorial` is the island's real entry — the name passed
// as opsCalleeExport when the crossing is planned haskell→rust.
#[no_mangle]
pub extern "C" fn rs_island_factorial(n: i64) -> i64 {
    (2..=n).product()
}
