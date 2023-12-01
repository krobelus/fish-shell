// Various bug and feature tests. Compiled and run by make test.
#include "config.h"  // IWYU pragma: keep

#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <chrono>
#include <clocale>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cwchar>
#include <functional>
#include <future>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "fds.rs.h"
#include "parse_constants.rs.h"

#ifdef FISH_CI_SAN
#include <sanitizer/lsan_interface.h>
#endif

#include "abbrs.h"
#include "ast.h"
#include "autoload.h"
#include "color.h"
#include "common.h"
#include "complete.h"
#include "cxxgen.h"
#include "enum_set.h"
#include "env.h"
#include "env/env_ffi.rs.h"
#include "env_universal_common.h"
#include "expand.h"
#include "fallback.h"  // IWYU pragma: keep
#include "fd_monitor.rs.h"
#include "fd_readable_set.rs.h"
#include "fds.h"
#include "ffi_baggage.h"
#include "ffi_init.rs.h"
#include "ffi_tests.rs.h"
#include "function.h"
#include "future_feature_flags.h"
#include "global_safety.h"
#include "highlight.h"
#include "history.h"
#include "input.h"
#include "input_common.h"
#include "io.h"
#include "iothread.h"
#include "kill.rs.h"
#include "lru.h"
#include "maybe.h"
#include "operation_context.h"
#include "pager.h"
#include "parse_constants.h"
#include "parse_tree.h"
#include "parse_util.h"
#include "parser.h"
#include "path.h"
#include "proc.h"
#include "reader.h"
#include "redirection.h"
#include "screen.h"
#include "signals.h"
#include "smoke.rs.h"
#include "termsize.h"
#include "threads.rs.h"
#include "tokenizer.h"
#include "util.h"
#include "wcstringutil.h"
#include "wgetopt.h"
#include "wildcard.h"
#include "wutil.h"  // IWYU pragma: keep

static const char *const *s_arguments;
static int s_test_run_count = 0;

// Indicate if we should test the given function. Either we test everything (all arguments) or we
// run only tests that have a prefix in s_arguments.
// If \p default_on is set, then allow no args to run this test by default.
static bool should_test_function(const char *func_name, bool default_on = true) {
    bool result = false;
    if (!s_arguments || !s_arguments[0]) {
        // No args, test if defaulted on.
        result = default_on;
    } else {
        for (size_t i = 0; s_arguments[i] != nullptr; i++) {
            if (!std::strcmp(func_name, s_arguments[i])) {
                result = true;
                break;
            }
        }
    }
    if (result) s_test_run_count++;
    return result;
}

/// The number of tests to run.
#define ESCAPE_TEST_COUNT 100000
/// The average length of strings to unescape.
#define ESCAPE_TEST_LENGTH 100

/// Number of encountered errors.
static int err_count = 0;

/// Print formatted output.
static void say(const wchar_t *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    std::vfwprintf(stdout, fmt, va);
    va_end(va);
    std::fwprintf(stdout, L"\n");
}

/// Print formatted error string.
static void err(const wchar_t *blah, ...) {
    va_list va;
    va_start(va, blah);
    err_count++;

    // Show errors in red.
    std::fputws(L"\x1B[31m", stdout);
    std::fwprintf(stdout, L"Error: ");
    std::vfwprintf(stdout, blah, va);
    va_end(va);

    // Return to normal color.
    std::fputws(L"\x1B[0m", stdout);
    std::fwprintf(stdout, L"\n");
}

/// Joins a std::vector<wcstring> via commas.
wcstring comma_join(const std::vector<wcstring> &lst) {
    wcstring result;
    for (size_t i = 0; i < lst.size(); i++) {
        if (i > 0) {
            result.push_back(L',');
        }
        result.append(lst.at(i));
    }
    return result;
}

static std::vector<std::string> pushed_dirs;

/// Helper to chdir and then update $PWD.
static bool pushd(const char *path) {
    char cwd[PATH_MAX] = {};
    if (getcwd(cwd, sizeof cwd) == nullptr) {
        err(L"getcwd() from pushd() failed: errno = %d", errno);
        return false;
    }
    pushed_dirs.emplace_back(cwd);

    // We might need to create the directory. We don't care if this fails due to the directory
    // already being present.
    mkdir(path, 0770);

    int ret = chdir(path);
    if (ret != 0) {
        err(L"chdir(\"%s\") from pushd() failed: errno = %d", path, errno);
        return false;
    }

    env_stack_principal().set_pwd_from_getcwd();
    return true;
}

static void popd() {
    const std::string &old_cwd = pushed_dirs.back();
    if (chdir(old_cwd.c_str()) == -1) {
        err(L"chdir(\"%s\") from popd() failed: errno = %d", old_cwd.c_str(), errno);
    }
    pushed_dirs.pop_back();
    env_stack_principal().set_pwd_from_getcwd();
}

// Helper to return a string whose length greatly exceeds PATH_MAX.
wcstring get_overlong_path() {
    wcstring longpath;
    longpath.reserve(PATH_MAX * 2 + 10);
    while (longpath.size() <= PATH_MAX * 2) {
        longpath += L"/overlong";
    }
    return longpath;
}

// The odd formulation of these macros is to avoid "multiple unary operator" warnings from oclint
// were we to use the more natural "if (!(e)) err(..." form. We have to do this because the rules
// for the C preprocessor make it practically impossible to embed a comment in the body of a macro.
#define do_test(e)                                             \
    do {                                                       \
        if (e) {                                               \
            ;                                                  \
        } else {                                               \
            err(L"Test failed on line %lu: %s", __LINE__, #e); \
        }                                                      \
    } while (0)

#define do_test1(e, msg)                                           \
    do {                                                           \
        if (e) {                                                   \
            ;                                                      \
        } else {                                                   \
            err(L"Test failed on line %lu: %ls", __LINE__, (msg)); \
        }                                                          \
    } while (0)

// todo!("already ported, delete this");
/// Test that the fish functions for converting strings to numbers work.
static void test_str_to_num() {
    say(L"Testing str_to_num");
    const wchar_t *end;
    int i;
    long l;

    i = fish_wcstoi(L"");
    do_test1(errno == EINVAL && i == 0, L"converting empty string to int did not fail");
    i = fish_wcstoi(L" \n ");
    do_test1(errno == EINVAL && i == 0, L"converting whitespace string to int did not fail");
    i = fish_wcstoi(L"123");
    do_test1(errno == 0 && i == 123, L"converting valid num to int did not succeed");
    i = fish_wcstoi(L"-123");
    do_test1(errno == 0 && i == -123, L"converting valid num to int did not succeed");
    i = fish_wcstoi(L" 345  ");
    do_test1(errno == 0 && i == 345, L"converting valid num to int did not succeed");
    i = fish_wcstoi(L" -345  ");
    do_test1(errno == 0 && i == -345, L"converting valid num to int did not succeed");
    i = fish_wcstoi(L"x345");
    do_test1(errno == EINVAL && i == 0, L"converting invalid num to int did not fail");
    i = fish_wcstoi(L" x345");
    do_test1(errno == EINVAL && i == 0, L"converting invalid num to int did not fail");
    i = fish_wcstoi(L"456 x");
    do_test1(errno == -1 && i == 456, L"converting invalid num to int did not fail");
    i = fish_wcstoi(L"99999999999999999999999");
    do_test1(errno == ERANGE && i == INT_MAX, L"converting invalid num to int did not fail");
    i = fish_wcstoi(L"-99999999999999999999999");
    do_test1(errno == ERANGE && i == INT_MIN, L"converting invalid num to int did not fail");
    i = fish_wcstoi(L"567]", &end);
    do_test1(errno == -1 && i == 567 && *end == L']',
             L"converting valid num to int did not succeed");
    // This is subtle. "567" in base 8 is "375" in base 10. The final "8" is not converted.
    i = fish_wcstoi(L"5678", &end, 8);
    do_test1(errno == -1 && i == 375 && *end == L'8',
             L"converting invalid num to int did not fail");

    l = fish_wcstol(L"");
    do_test1(errno == EINVAL && l == 0, L"converting empty string to long did not fail");
    l = fish_wcstol(L" \t ");
    do_test1(errno == EINVAL && l == 0, L"converting whitespace string to long did not fail");
    l = fish_wcstol(L"123");
    do_test1(errno == 0 && l == 123, L"converting valid num to long did not succeed");
    l = fish_wcstol(L"-123");
    do_test1(errno == 0 && l == -123, L"converting valid num to long did not succeed");
    l = fish_wcstol(L" 345  ");
    do_test1(errno == 0 && l == 345, L"converting valid num to long did not succeed");
    l = fish_wcstol(L" -345  ");
    do_test1(errno == 0 && l == -345, L"converting valid num to long did not succeed");
    l = fish_wcstol(L"x345");
    do_test1(errno == EINVAL && l == 0, L"converting invalid num to long did not fail");
    l = fish_wcstol(L" x345");
    do_test1(errno == EINVAL && l == 0, L"converting invalid num to long did not fail");
    l = fish_wcstol(L"456 x");
    do_test1(errno == -1 && l == 456, L"converting invalid num to long did not fail");
    l = fish_wcstol(L"99999999999999999999999");
    do_test1(errno == ERANGE && l == LONG_MAX, L"converting invalid num to long did not fail");
    l = fish_wcstol(L"-99999999999999999999999");
    do_test1(errno == ERANGE && l == LONG_MIN, L"converting invalid num to long did not fail");
    l = fish_wcstol(L"567]", &end);
    do_test1(errno == -1 && l == 567 && *end == L']',
             L"converting valid num to long did not succeed");
    // This is subtle. "567" in base 8 is "375" in base 10. The final "8" is not converted.
    l = fish_wcstol(L"5678", &end, 8);
    do_test1(errno == -1 && l == 375 && *end == L'8',
             L"converting invalid num to long did not fail");
}

enum class test_enum { alpha, beta, gamma, COUNT };

template <>
struct enum_info_t<test_enum> {
    static constexpr auto count = test_enum::COUNT;
};

// todo!("no need to port, delete this");
static void test_enum_set() {
    say(L"Testing enum set");
    enum_set_t<test_enum> es;
    do_test(es.none());
    do_test(!es.any());
    do_test(es.to_raw() == 0);
    do_test(es == enum_set_t<test_enum>::from_raw(0));
    do_test(es != enum_set_t<test_enum>::from_raw(1));

    es.set(test_enum::beta);
    do_test(es.get(test_enum::beta));
    do_test(!es.get(test_enum::alpha));
    do_test(es & test_enum::beta);
    do_test(!(es & test_enum::alpha));
    do_test(es.to_raw() == 2);
    do_test(es == enum_set_t<test_enum>::from_raw(2));
    do_test(es == enum_set_t<test_enum>{test_enum::beta});
    do_test(es != enum_set_t<test_enum>::from_raw(3));
    do_test(es.any());
    do_test(!es.none());

    do_test((enum_set_t<test_enum>{test_enum::beta} | test_enum::alpha).to_raw() == 3);
    do_test((enum_set_t<test_enum>{test_enum::beta} | enum_set_t<test_enum>{test_enum::alpha})
                .to_raw() == 3);
}

// todo!("no need to port, delete this");
static void test_enum_array() {
    say(L"Testing enum array");
    enum_array_t<std::string, test_enum> es{};
    do_test(es.size() == enum_count<test_enum>());
    es[test_enum::beta] = "abc";
    do_test(es[test_enum::beta] == "abc");
    es.at(test_enum::gamma) = "def";
    do_test(es.at(test_enum::gamma) == "def");
}

/// Helper to convert a narrow string to a sequence of hex digits.
static std::string str2hex(const std::string &input) {
    std::string output;
    for (char c : input) {
        char buff[16];
        size_t amt = snprintf(buff, sizeof buff, "0x%02X ", (int)(c & 0xFF));
        output.append(buff, amt);
    }
    return output;
}

// todo!("already ported, delete this");
/// Test wide/narrow conversion by creating random strings and verifying that the original string
/// comes back through double conversion.
static void test_convert() {
    say(L"Testing wide/narrow string conversion");
    for (int i = 0; i < ESCAPE_TEST_COUNT; i++) {
        std::string orig{};
        while (random() % ESCAPE_TEST_LENGTH) {
            char c = random();
            orig.push_back(c);
        }

        const wcstring w = str2wcstring(orig);
        std::string n = wcs2string(w);
        if (orig != n) {
            err(L"Line %d - %d: Conversion cycle of string:\n%4d chars: %s\n"
                L"produced different string:\n%4d chars: %s",
                __LINE__, i, orig.size(), str2hex(orig).c_str(), n.size(), str2hex(n).c_str());
        }
    }
}

// todo!("already ported, delete this");
/// Verify that ASCII narrow->wide conversions are correct.
static void test_convert_ascii() {
    std::string s(4096, '\0');
    for (size_t i = 0; i < s.size(); i++) {
        s[i] = (i % 10) + '0';
    }

    // Test a variety of alignments.
    for (size_t left = 0; left < 16; left++) {
        for (size_t right = 0; right < 16; right++) {
            const char *start = s.data() + left;
            size_t len = s.size() - left - right;
            wcstring wide = str2wcstring(start, len);
            std::string narrow = wcs2string(wide);
            do_test(narrow == std::string(start, len));
        }
    }

    // Put some non-ASCII bytes in and ensure it all still works.
    for (char &c : s) {
        char saved = c;
        c = 0xF7;
        do_test(wcs2string(str2wcstring(s)) == s);
        c = saved;
    }
}

// todo!("already ported, delete this");
/// fish uses the private-use range to encode bytes that could not be decoded using the user's
/// locale. If the input could be decoded, but decoded to private-use codepoints, then fish should
/// also use the direct encoding for those bytes. Verify that characters in the private use area are
/// correctly round-tripped. See #7723.
static void test_convert_private_use() {
    for (wchar_t wc = ENCODE_DIRECT_BASE; wc < ENCODE_DIRECT_END; wc++) {
        // Encode the char via the locale. Do not use fish functions which interpret these
        // specially.
        char converted[MB_LEN_MAX];
        mbstate_t state{};
        size_t len = std::wcrtomb(converted, wc, &state);
        if (len == static_cast<size_t>(-1)) {
            // Could not be encoded in this locale.
            continue;
        }
        std::string s(converted, len);

        // Ask fish to decode this via str2wcstring.
        // str2wcstring should notice that the decoded form collides with its private use and encode
        // it directly.
        wcstring ws = str2wcstring(s);

        // Each byte should be encoded directly, and round tripping should work.
        do_test(ws.size() == s.size());
        do_test(wcs2string(ws) == s);
    }
}

// todo!("port this");
static void perf_convert_ascii() {
    std::string s(128 * 1024, '\0');
    for (size_t i = 0; i < s.size(); i++) {
        s[i] = (i % 10) + '0';
    }
    (void)str2wcstring(s);

    double start = timef();
    const int iters = 1024;
    for (int i = 0; i < iters; i++) {
        (void)str2wcstring(s);
    }
    double end = timef();
    auto usec = static_cast<unsigned long long>(((end - start) * 1E6) / iters);
    say(L"ASCII string conversion perf: %lu bytes in %llu usec", s.size(), usec);
}

// todo!("no need to port this, delete?");
/// Verify correct behavior with embedded nulls.
static void test_convert_nulls() {
    say(L"Testing convert_nulls");
    const wchar_t in[] = L"AAA\0BBB";
    const size_t in_len = (sizeof in / sizeof *in) - 1;
    const wcstring in_str = wcstring(in, in_len);
    std::string out_str = wcs2string(in_str);
    if (out_str.size() != in_len) {
        err(L"Embedded nulls mishandled in wcs2string");
    }
    for (size_t i = 0; i < in_len; i++) {
        if (in[i] != out_str.at(i)) {
            err(L"Embedded nulls mishandled in wcs2string at index %lu", (unsigned long)i);
        }
    }

    wcstring out_wstr = str2wcstring(out_str);
    if (out_wstr.size() != in_len) {
        err(L"Embedded nulls mishandled in str2wcstring");
    }
    for (size_t i = 0; i < in_len; i++) {
        if (in[i] != out_wstr.at(i)) {
            err(L"Embedded nulls mishandled in str2wcstring at index %lu", (unsigned long)i);
        }
    }
}

// todo!("already ported, delete this");
static void test_iothread() {
    say(L"Testing iothreads");
    std::atomic<int> shared_int{0};
    const int iterations = 64;
    std::promise<void> prom;
    for (int i = 0; i < iterations; i++) {
        iothread_perform([&] {
            int newv = 1 + shared_int.fetch_add(1, std::memory_order_relaxed);
            if (newv == iterations) {
                prom.set_value();
            }
        });
    }
    auto status = prom.get_future().wait_for(std::chrono::seconds(64));

    // Should have incremented it once per thread.
    do_test(status == std::future_status::ready);
    do_test(shared_int == iterations);
    if (shared_int != iterations) {
        say(L"Expected int to be %d, but instead it was %d", iterations, shared_int.load());
    }
}

// todo!("port this");
static void test_pthread() {
    say(L"Testing pthreads");
    std::atomic<int> val{3};
    std::promise<void> promise;
    bool made = make_detached_pthread([&]() {
        val = val + 2;
        promise.set_value();
    });
    do_test(made);
    promise.get_future().wait();
    do_test(val == 5);
}

// todo!("port this");
static void test_debounce() {
    say(L"Testing debounce");
    // Run 8 functions using a condition variable.
    // Only the first and last should run.
    auto db = new_debounce_t(0);
    constexpr size_t count = 8;
    std::array<bool, count> handler_ran = {};
    std::array<bool, count> completion_ran = {};

    bool ready_to_go = false;
    std::mutex m;
    std::condition_variable cv;

    // "Enqueue" all functions. Each one waits until ready_to_go.
    for (size_t idx = 0; idx < count; idx++) {
        do_test(handler_ran[idx] == false);
        std::function<size_t()> performer = [&, idx] {
            std::unique_lock<std::mutex> lock(m);
            cv.wait(lock, [&] { return ready_to_go; });
            handler_ran[idx] = true;
            return idx;
        };
        std::function<void(size_t)> completer = [&](size_t idx) { completion_ran[idx] = true; };
        debounce_perform_with_completion(*db, std::move(performer), std::move(completer));
    }

    // We're ready to go.
    {
        std::unique_lock<std::mutex> lock(m);
        ready_to_go = true;
    }
    cv.notify_all();

    // Wait until the last completion is done.
    while (!completion_ran.back()) {
        iothread_service_main();
    }
    iothread_drain_all();

    // Each perform() call may displace an existing queued operation.
    // Each operation waits until all are queued.
    // Therefore we expect the last perform() to have run, and at most one more.

    do_test(handler_ran.back());
    do_test(completion_ran.back());

    size_t total_ran = 0;
    for (size_t idx = 0; idx < count; idx++) {
        total_ran += (handler_ran[idx] ? 1 : 0);
        do_test(handler_ran[idx] == completion_ran[idx]);
    }
    do_test(total_ran <= 2);
}

// todo!("port this");
static void test_debounce_timeout() {
    using namespace std::chrono;
    say(L"Testing debounce timeout");

    // Verify that debounce doesn't wait forever.
    // Use a shared_ptr so we don't have to join our threads.
    const long timeout_ms = 500;
    struct data_t {
        rust::box<debounce_t> db = new_debounce_t(timeout_ms);
        bool exit_ok = false;
        std::mutex m;
        std::condition_variable cv;
        relaxed_atomic_t<uint32_t> running{0};
    };
    auto data = std::make_shared<data_t>();

    // Our background handler. Note this just blocks until exit_ok is set.
    std::function<void()> handler = [data] {
        data->running++;
        std::unique_lock<std::mutex> lock(data->m);
        data->cv.wait(lock, [&] { return data->exit_ok; });
    };

    // Spawn the handler twice. This should not modify the thread token.
    uint64_t token1 = debounce_perform(*data->db, handler);
    uint64_t token2 = debounce_perform(*data->db, handler);
    do_test(token1 == token2);

    // Wait 75 msec, then enqueue something else; this should spawn a new thread.
    std::this_thread::sleep_for(std::chrono::milliseconds(timeout_ms + timeout_ms / 2));
    do_test(data->running == 1);
    uint64_t token3 = debounce_perform(*data->db, handler);
    do_test(token3 > token2);

    // Release all the threads.
    std::unique_lock<std::mutex> lock(data->m);
    data->exit_ok = true;
    data->cv.notify_all();
}

static parser_test_error_bits_t detect_argument_errors(const wcstring &src) {
    using namespace ast;
    auto ast = ast_parse_argument_list(src, parse_flag_none);
    if (ast->errored()) {
        return PARSER_TEST_ERROR;
    }
    const ast::argument_t *first_arg =
        ast->top()->as_freestanding_argument_list().arguments().at(0);
    if (!first_arg) {
        err(L"Failed to parse an argument");
        return 0;
    }
    return parse_util_detect_errors_in_argument(*first_arg, *first_arg->source(src));
}

/// Test the parser.
// todo!("port this");
static void test_parser() {
    say(L"Testing parser");

    auto detect_errors = [](const wcstring &s) {
        return parse_util_detect_errors(s, nullptr, true /* accept incomplete */);
    };

    say(L"Testing block nesting");
    if (!detect_errors(L"if; end")) {
        err(L"Incomplete if statement undetected");
    }
    if (!detect_errors(L"if test; echo")) {
        err(L"Missing end undetected");
    }
    if (!detect_errors(L"if test; end; end")) {
        err(L"Unbalanced end undetected");
    }

    say(L"Testing detection of invalid use of builtin commands");
    if (!detect_errors(L"case foo")) {
        err(L"'case' command outside of block context undetected");
    }
    if (!detect_errors(L"switch ggg; if true; case foo;end;end")) {
        err(L"'case' command outside of switch block context undetected");
    }
    if (!detect_errors(L"else")) {
        err(L"'else' command outside of conditional block context undetected");
    }
    if (!detect_errors(L"else if")) {
        err(L"'else if' command outside of conditional block context undetected");
    }
    if (!detect_errors(L"if false; else if; end")) {
        err(L"'else if' missing command undetected");
    }

    if (!detect_errors(L"break")) {
        err(L"'break' command outside of loop block context undetected");
    }

    if (detect_errors(L"break --help")) {
        err(L"'break --help' incorrectly marked as error");
    }

    if (!detect_errors(L"while false ; function foo ; break ; end ; end ")) {
        err(L"'break' command inside function allowed to break from loop outside it");
    }

    if (!detect_errors(L"exec ls|less") || !detect_errors(L"echo|return")) {
        err(L"Invalid pipe command undetected");
    }

    if (detect_errors(L"for i in foo ; switch $i ; case blah ; break; end; end ")) {
        err(L"'break' command inside switch falsely reported as error");
    }

    if (detect_errors(L"or cat | cat") || detect_errors(L"and cat | cat")) {
        err(L"boolean command at beginning of pipeline falsely reported as error");
    }

    if (!detect_errors(L"cat | and cat")) {
        err(L"'and' command in pipeline not reported as error");
    }

    if (!detect_errors(L"cat | or cat")) {
        err(L"'or' command in pipeline not reported as error");
    }

    if (!detect_errors(L"cat | exec") || !detect_errors(L"exec | cat")) {
        err(L"'exec' command in pipeline not reported as error");
    }

    if (!detect_errors(L"begin ; end arg")) {
        err(L"argument to 'end' not reported as error");
    }

    if (!detect_errors(L"switch foo ; end arg")) {
        err(L"argument to 'end' not reported as error");
    }

    if (!detect_errors(L"if true; else if false ; end arg")) {
        err(L"argument to 'end' not reported as error");
    }

    if (!detect_errors(L"if true; else ; end arg")) {
        err(L"argument to 'end' not reported as error");
    }

    if (detect_errors(L"begin ; end 2> /dev/null")) {
        err(L"redirection after 'end' wrongly reported as error");
    }

    if (detect_errors(L"true | ") != PARSER_TEST_INCOMPLETE) {
        err(L"unterminated pipe not reported properly");
    }

    if (detect_errors(L"echo (\nfoo\n  bar") != PARSER_TEST_INCOMPLETE) {
        err(L"unterminated multiline subshell not reported properly");
    }

    if (detect_errors(L"begin ; true ; end | ") != PARSER_TEST_INCOMPLETE) {
        err(L"unterminated pipe not reported properly");
    }

    if (detect_errors(L" | true ") != PARSER_TEST_ERROR) {
        err(L"leading pipe not reported properly");
    }

    if (detect_errors(L"true | # comment") != PARSER_TEST_INCOMPLETE) {
        err(L"comment after pipe not reported as incomplete");
    }

    if (detect_errors(L"true | # comment \n false ")) {
        err(L"comment and newline after pipe wrongly reported as error");
    }

    if (detect_errors(L"true | ; false ") != PARSER_TEST_ERROR) {
        err(L"semicolon after pipe not detected as error");
    }

    if (detect_argument_errors(L"foo")) {
        err(L"simple argument reported as error");
    }

    if (detect_argument_errors(L"''")) {
        err(L"Empty string reported as error");
    }

    if (!(detect_argument_errors(L"foo$$") & PARSER_TEST_ERROR)) {
        err(L"Bad variable expansion not reported as error");
    }

    if (!(detect_argument_errors(L"foo$@") & PARSER_TEST_ERROR)) {
        err(L"Bad variable expansion not reported as error");
    }

    // Within command substitutions, we should be able to detect everything that
    // parse_util_detect_errors can detect.
    if (!(detect_argument_errors(L"foo(cat | or cat)") & PARSER_TEST_ERROR)) {
        err(L"Bad command substitution not reported as error");
    }

    if (!detect_errors(L"false & ; and cat")) {
        err(L"'and' command after background not reported as error");
    }

    if (!detect_errors(L"true & ; or cat")) {
        err(L"'or' command after background not reported as error");
    }

    if (detect_errors(L"true & ; not cat")) {
        err(L"'not' command after background falsely reported as error");
    }

    if (!detect_errors(L"if true & ; end")) {
        err(L"backgrounded 'if' conditional not reported as error");
    }

    if (!detect_errors(L"if false; else if true & ; end")) {
        err(L"backgrounded 'else if' conditional not reported as error");
    }

    if (!detect_errors(L"while true & ; end")) {
        err(L"backgrounded 'while' conditional not reported as error");
    }

    if (!detect_errors(L"true | || false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"|| false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"&& false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"true ; && false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"true ; || false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"true || && false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"true && || false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (!detect_errors(L"true && && false")) {
        err(L"bogus boolean statement error not detected on line %d", __LINE__);
    }

    if (detect_errors(L"true && ") != PARSER_TEST_INCOMPLETE) {
        err(L"unterminated conjunction not reported properly");
    }

    if (detect_errors(L"true && \n true")) {
        err(L"newline after && reported as error");
    }

    if (detect_errors(L"true || \n") != PARSER_TEST_INCOMPLETE) {
        err(L"unterminated conjunction not reported properly");
    }

    say(L"Testing basic evaluation");

    // Ensure that we don't crash on infinite self recursion and mutual recursion. These must use
    // the principal parser because we cannot yet execute jobs on other parsers.
    auto parser = parser_principal_parser()->deref().shared();
    say(L"Testing recursion detection");
    parser->deref().eval(L"function recursive ; recursive ; end ; recursive; ", *new_io_chain());

    parser->deref().eval(
        L"function recursive1 ; recursive2 ; end ; "
        L"function recursive2 ; recursive1 ; end ; recursive1; ",
        *new_io_chain());

    say(L"Testing empty function name");
    parser->deref().eval(L"function '' ; echo fail; exit 42 ; end ; ''", *new_io_chain());

    say(L"Testing eval_args");
    wcstring_list_ffi_t comps;
    parser_expand_argument_list_ffi(L"alpha 'beta gamma' delta", expand_flags_t{},
                                    *parser_context(parser->deref()), comps);
    do_test(comps.size() == 3);
    do_test(comps.at(0) == L"alpha");
    do_test(comps.at(1) == L"beta gamma");
    do_test(comps.at(2) == L"delta");
}

static void test_const_strlen() {
    do_test(const_strlen("") == 0);
    do_test(const_strlen(L"") == 0);
    do_test(const_strlen("\0") == 0);
    do_test(const_strlen(L"\0") == 0);
    do_test(const_strlen("\0abc") == 0);
    do_test(const_strlen(L"\0abc") == 0);
    do_test(const_strlen("x") == 1);
    do_test(const_strlen(L"x") == 1);
    do_test(const_strlen("abc") == 3);
    do_test(const_strlen(L"abc") == 3);
    do_test(const_strlen("abc\0def") == 3);
    do_test(const_strlen(L"abc\0def") == 3);
    do_test(const_strlen("abcdef\0") == 6);
    do_test(const_strlen(L"abcdef\0") == 6);
    static_assert(const_strlen("hello") == 5, "const_strlen failure");
}

static void test_const_strcmp() {
    static_assert(const_strcmp("", "a") < 0, "const_strcmp failure");
    static_assert(const_strcmp("a", "a") == 0, "const_strcmp failure");
    static_assert(const_strcmp("a", "") > 0, "const_strcmp failure");
    static_assert(const_strcmp("aa", "a") > 0, "const_strcmp failure");
    static_assert(const_strcmp("a", "aa") < 0, "const_strcmp failure");
    static_assert(const_strcmp("b", "aa") > 0, "const_strcmp failure");
}

// todo!("already ported, delete this");
void test_dir_iter() {
    dir_iter_t baditer(L"/definitely/not/a/valid/directory/for/sure");
    do_test(!baditer.valid());
    do_test(baditer.error() == ENOENT || baditer.error() == EACCES);
    do_test(baditer.next() == nullptr);

    char t1[] = "/tmp/fish_test_dir_iter.XXXXXX";
    const std::string basepathn = mkdtemp(t1);
    const wcstring basepath = str2wcstring(basepathn);
    auto makepath = [&](const wcstring &s) { return wcs2zstring(basepath + L"/" + s); };

    const wcstring dirname = L"dir";
    const wcstring regname = L"reg";
    const wcstring reglinkname = L"reglink";    // link to regular file
    const wcstring dirlinkname = L"dirlink";    // link to directory
    const wcstring badlinkname = L"badlink";    // link to nowhere
    const wcstring selflinkname = L"selflink";  // link to self
    const wcstring fifoname = L"fifo";
    const std::vector<wcstring> names = {dirname,     regname,      reglinkname, dirlinkname,
                                         badlinkname, selflinkname, fifoname};

    const auto is_link_name = [&](const wcstring &name) -> bool {
        return contains({reglinkname, dirlinkname, badlinkname, selflinkname}, name);
    };

    // Make our different file types
    int ret = mkdir(makepath(dirname).c_str(), 0700);
    do_test(ret == 0);
    ret = open(makepath(regname).c_str(), O_CREAT | O_WRONLY, 0600);
    do_test(ret >= 0);
    close(ret);
    ret = symlink(makepath(regname).c_str(), makepath(reglinkname).c_str());
    do_test(ret == 0);
    ret = symlink(makepath(dirname).c_str(), makepath(dirlinkname).c_str());
    do_test(ret == 0);
    ret = symlink("/this/is/an/invalid/path", makepath(badlinkname).c_str());
    do_test(ret == 0);
    ret = symlink(makepath(selflinkname).c_str(), makepath(selflinkname).c_str());
    do_test(ret == 0);
    ret = mkfifo(makepath(fifoname).c_str(), 0600);
    do_test(ret == 0);

    dir_iter_t iter1(basepath);
    do_test(iter1.valid());
    do_test(iter1.error() == 0);
    size_t seen = 0;
    while (const auto *entry = iter1.next()) {
        seen += 1;
        do_test(entry->name != L"." && entry->name != L"..");
        do_test(contains(names, entry->name));
        maybe_t<dir_entry_type_t> expected{};
        if (entry->name == dirname) {
            expected = dir_entry_type_t::dir;
        } else if (entry->name == regname) {
            expected = dir_entry_type_t::reg;
        } else if (entry->name == reglinkname) {
            expected = dir_entry_type_t::reg;
        } else if (entry->name == dirlinkname) {
            expected = dir_entry_type_t::dir;
        } else if (entry->name == badlinkname) {
            expected = none();
        } else if (entry->name == selflinkname) {
            expected = dir_entry_type_t::lnk;
        } else if (entry->name == fifoname) {
            expected = dir_entry_type_t::fifo;
        } else {
            err(L"Unexpected file type");
            continue;
        }
        // Links should never have a fast type if we are resolving them, since we cannot resolve a
        // symlink from readdir.
        if (is_link_name(entry->name)) {
            do_test(entry->fast_type() == none());
        }
        // If we have a fast type, it should be correct.
        do_test(entry->fast_type() == none() || entry->fast_type() == expected);
        do_test(entry->check_type() == expected);
    }
    do_test(seen == names.size());

    // Clean up.
    for (const auto &name : names) {
        (void)unlink(makepath(name).c_str());
    }
    (void)rmdir(basepathn.c_str());
}

static void test_utility_functions() {
    say(L"Testing utility functions");
    test_const_strlen();
    test_const_strcmp();
}

class test_lru_t : public lru_cache_t<int> {
   public:
    static constexpr size_t test_capacity = 16;
    using value_type = std::pair<wcstring, int>;

    test_lru_t() : lru_cache_t<int>(test_capacity) {}

    std::vector<value_type> values() const {
        std::vector<value_type> result;
        for (auto p : *this) {
            result.push_back(p);
        }
        return result;
    }

    std::vector<int> ints() const {
        std::vector<int> result;
        for (auto p : *this) {
            result.push_back(p.second);
        }
        return result;
    }
};

// todo!("port this");
static void test_lru() {
    say(L"Testing LRU cache");

    test_lru_t cache;
    std::vector<std::pair<wcstring, int>> expected_evicted;
    std::vector<std::pair<wcstring, int>> expected_values;
    int total_nodes = 20;
    for (int i = 0; i < total_nodes; i++) {
        do_test(cache.size() == size_t(std::min(i, 16)));
        do_test(cache.values() == expected_values);
        if (i < 4) expected_evicted.emplace_back(to_string(i), i);
        // Adding the node the first time should work, and subsequent times should fail.
        do_test(cache.insert(to_string(i), i));
        do_test(!cache.insert(to_string(i), i + 1));

        expected_values.emplace_back(to_string(i), i);
        while (expected_values.size() > test_lru_t::test_capacity) {
            expected_values.erase(expected_values.begin());
        }
        cache.check_sanity();
    }
    do_test(cache.values() == expected_values);
    cache.check_sanity();

    // Stable-sort ints in reverse order
    // This a/2 check ensures that some different ints compare the same
    // It also gives us a different order than we started with
    auto comparer = [](int a, int b) { return a / 2 > b / 2; };
    std::vector<int> ints = cache.ints();
    std::stable_sort(ints.begin(), ints.end(), comparer);

    cache.stable_sort(comparer);
    std::vector<int> new_ints = cache.ints();
    if (new_ints != ints) {
        auto commajoin = [](const std::vector<int> &vs) {
            wcstring ret;
            for (int v : vs) {
                append_format(ret, L"%d,", v);
            }
            if (!ret.empty()) ret.pop_back();
            return ret;
        };
        err(L"LRU stable sort failed. Expected %ls, got %ls\n", commajoin(new_ints).c_str(),
            commajoin(ints).c_str());
    }

    cache.evict_all_nodes();
    do_test(cache.size() == 0);
}

// todo!("port this")
static void test_abbreviations() {
    say(L"Testing abbreviations");
    {
        auto abbrs = abbrs_get_set();
        abbrs->add(L"gc", L"gc", L"git checkout", abbrs_position_t::command, false);
        abbrs->add(L"foo", L"foo", L"bar", abbrs_position_t::command, false);
        abbrs->add(L"gx", L"gx", L"git checkout", abbrs_position_t::command, false);
        abbrs->add(L"yin", L"yin", L"yang", abbrs_position_t::anywhere, false);
    }

    // Helper to expand an abbreviation, enforcing we have no more than one result.
    auto abbr_expand_1 = [](const wcstring &token, abbrs_position_t pos) -> maybe_t<wcstring> {
        auto result = abbrs_match(token, pos);
        if (result.size() > 1) {
            err(L"abbreviation expansion for %ls returned more than 1 result", token.c_str());
        }
        if (result.empty()) {
            return none();
        }
        return *result.front().replacement;
    };

    auto cmd = abbrs_position_t::command;
    if (abbr_expand_1(L"", cmd)) err(L"Unexpected success with empty abbreviation");
    if (abbr_expand_1(L"nothing", cmd)) err(L"Unexpected success with missing abbreviation");

    auto mresult = abbr_expand_1(L"gc", cmd);
    if (!mresult) err(L"Unexpected failure with gc abbreviation");
    if (*mresult != L"git checkout") err(L"Wrong abbreviation result for gc");

    mresult = abbr_expand_1(L"foo", cmd);
    if (!mresult) err(L"Unexpected failure with foo abbreviation");
    if (*mresult != L"bar") err(L"Wrong abbreviation result for foo");

    maybe_t<wcstring> result;
    auto expand_abbreviation_in_command = [](const wcstring &cmdline,
                                             maybe_t<size_t> cursor_pos = {}) -> maybe_t<wcstring> {
        if (auto replacement = reader_expand_abbreviation_at_cursor(
                cmdline, cursor_pos.value_or(cmdline.size()), parser_principal_parser()->deref())) {
            wcstring cmdline_expanded = cmdline;
            std::vector<highlight_spec_t> colors{cmdline_expanded.size()};
            apply_edit(&cmdline_expanded, &colors, edit_t{replacement->range, *replacement->text});
            return cmdline_expanded;
        }
        return none_t();
    };
    result = expand_abbreviation_in_command(L"just a command", 3);
    if (result) err(L"Command wrongly expanded on line %ld", (long)__LINE__);
    result = expand_abbreviation_in_command(L"gc somebranch", 0);
    if (!result) err(L"Command not expanded on line %ld", (long)__LINE__);

    result = expand_abbreviation_in_command(L"gc somebranch", const_strlen(L"gc"));
    if (!result) err(L"gc not expanded");
    if (result != L"git checkout somebranch")
        err(L"gc incorrectly expanded on line %ld to '%ls'", (long)__LINE__, result->c_str());

    // Space separation.
    result = expand_abbreviation_in_command(L"gx somebranch", const_strlen(L"gc"));
    if (!result) err(L"gx not expanded");
    if (result != L"git checkout somebranch")
        err(L"gc incorrectly expanded on line %ld to '%ls'", (long)__LINE__, result->c_str());

    result =
        expand_abbreviation_in_command(L"echo hi ; gc somebranch", const_strlen(L"echo hi ; g"));
    if (!result) err(L"gc not expanded on line %ld", (long)__LINE__);
    if (result != L"echo hi ; git checkout somebranch")
        err(L"gc incorrectly expanded on line %ld", (long)__LINE__);

    result = expand_abbreviation_in_command(L"echo (echo (echo (echo (gc ",
                                            const_strlen(L"echo (echo (echo (echo (gc"));
    if (!result) err(L"gc not expanded on line %ld", (long)__LINE__);
    if (result != L"echo (echo (echo (echo (git checkout ")
        err(L"gc incorrectly expanded on line %ld to '%ls'", (long)__LINE__, result->c_str());

    // If commands should be expanded.
    result = expand_abbreviation_in_command(L"if gc");
    if (!result) err(L"gc not expanded on line %ld", (long)__LINE__);
    if (result != L"if git checkout")
        err(L"gc incorrectly expanded on line %ld to '%ls'", (long)__LINE__, result->c_str());

    // Others should not be.
    result = expand_abbreviation_in_command(L"of gc");
    if (result) err(L"gc incorrectly expanded on line %ld", (long)__LINE__);

    // Others should not be.
    result = expand_abbreviation_in_command(L"command gc");
    if (result) err(L"gc incorrectly expanded on line %ld", (long)__LINE__);

    // yin/yang expands everywhere.
    result = expand_abbreviation_in_command(L"command yin");
    if (!result) err(L"gc not expanded on line %ld", (long)__LINE__);
    if (result != L"command yang") {
        err(L"command yin incorrectly expanded on line %ld to '%ls'", (long)__LINE__,
            result->c_str());
    }
}

// todo!("port this")
static void test_pager_navigation() {
    say(L"Testing pager navigation");

    // Generate 19 strings of width 10. There's 2 spaces between completions, and our term size is
    // 80; these can therefore fit into 6 columns (6 * 12 - 2 = 70) or 5 columns (58) but not 7
    // columns (7 * 12 - 2 = 82).
    //
    // You can simulate this test by creating 19 files named "file00.txt" through "file_18.txt".
    auto completions = new_completion_list();
    for (size_t i = 0; i < 19; i++) {
        append_completion(*completions, L"abcdefghij");
    }

    pager_t pager;
    pager.set_completions(*completions);
    pager.set_term_size(termsize_default());
    page_rendering_t render = pager.render();

    if (render.term_width != 80) err(L"Wrong term width");
    if (render.term_height != 24) err(L"Wrong term height");

    size_t rows = 4, cols = 5;

    // We have 19 completions. We can fit into 6 columns with 4 rows or 5 columns with 4 rows; the
    // second one is better and so is what we ought to have picked.
    if (render.rows != rows) err(L"Wrong row count");
    if (render.cols != cols) err(L"Wrong column count");

    // Initially expect to have no completion index.
    if (render.selected_completion_idx != (size_t)(-1)) {
        err(L"Wrong initial selection");
    }

    // Here are navigation directions and where we expect the selection to be.
    const struct {
        selection_motion_t dir;
        size_t sel;
    } cmds[] = {
        // Tab completion to get into the list.
        {selection_motion_t::next, 0},

        // Westward motion in upper left goes to the last filled column in the last row.
        {selection_motion_t::west, 15},
        // East goes back.
        {selection_motion_t::east, 0},

        {selection_motion_t::west, 15},
        {selection_motion_t::west, 11},
        {selection_motion_t::east, 15},
        {selection_motion_t::east, 0},

        // "Next" motion goes down the column.
        {selection_motion_t::next, 1},
        {selection_motion_t::next, 2},

        {selection_motion_t::west, 17},
        {selection_motion_t::east, 2},
        {selection_motion_t::east, 6},
        {selection_motion_t::east, 10},
        {selection_motion_t::east, 14},
        {selection_motion_t::east, 18},

        {selection_motion_t::west, 14},
        {selection_motion_t::east, 18},

        // Eastward motion wraps back to the upper left, westward goes to the prior column.
        {selection_motion_t::east, 3},
        {selection_motion_t::east, 7},
        {selection_motion_t::east, 11},
        {selection_motion_t::east, 15},

        // Pages.
        {selection_motion_t::page_north, 12},
        {selection_motion_t::page_south, 15},
        {selection_motion_t::page_north, 12},
        {selection_motion_t::east, 16},
        {selection_motion_t::page_south, 18},
        {selection_motion_t::east, 3},
        {selection_motion_t::north, 2},
        {selection_motion_t::page_north, 0},
        {selection_motion_t::page_south, 3},

    };
    for (size_t i = 0; i < sizeof cmds / sizeof *cmds; i++) {
        pager.select_next_completion_in_direction(cmds[i].dir, render);
        pager.update_rendering(&render);
        if (cmds[i].sel != render.selected_completion_idx) {
            err(L"For command %lu, expected selection %lu, but found instead %lu\n", i, cmds[i].sel,
                render.selected_completion_idx);
        }
    }
}

struct pager_layout_testcase_t {
    int width;
    const wchar_t *expected;

    // Run ourselves as a test case.
    // Set our data on the pager, and then check the rendering.
    // We should have one line, and it should have our expected text.
    void run(pager_t &pager) const {
        pager.set_term_size(termsize_t{this->width, 24});
        page_rendering_t rendering = pager.render();
        const screen_data_t &sd = *rendering.screen_data;
        do_test(sd.line_count() == 1);
        if (sd.line_count() > 0) {
            wcstring expected = this->expected;

            // hack: handle the case where ellipsis is not L'\x2026'
            wchar_t ellipsis_char = get_ellipsis_char();
            if (ellipsis_char != L'\x2026') {
                std::replace(expected.begin(), expected.end(), L'\x2026', ellipsis_char);
            }

            wcstring text = *(sd.line_ffi(0)->text_characters_ffi());
            if (text != expected) {
                std::fwprintf(stderr, L"width %d got %zu<%ls>, expected %zu<%ls>\n", this->width,
                              text.length(), text.c_str(), expected.length(), expected.c_str());
                for (size_t i = 0; i < std::max(text.length(), expected.length()); i++) {
                    std::fwprintf(stderr, L"i %zu got <%lx> expected <%lx>\n", i,
                                  i >= text.length() ? 0xffff : text[i],
                                  i >= expected.length() ? 0xffff : expected[i]);
                }
            }
            do_test(text == expected);
        }
    }
};

// todo!("port this")
static void test_pager_layout() {
    // These tests are woefully incomplete
    // They only test the truncation logic for a single completion
    say(L"Testing pager layout");
    pager_t pager;

    // These test cases have equal completions and descriptions
    auto c1 = new_completion_with(L"abcdefghij", L"1234567890", 0);
    auto c1s = new_completion_list();
    c1s->push_back(*c1);
    pager.set_completions(*c1s);
    const pager_layout_testcase_t testcases1[] = {
        {26, L"abcdefghij  (1234567890)"}, {25, L"abcdefghij  (1234567890)"},
        {24, L"abcdefghij  (1234567890)"}, {23, L"abcdefghij  (12345678…)"},
        {22, L"abcdefghij  (1234567…)"},   {21, L"abcdefghij  (123456…)"},
        {20, L"abcdefghij  (12345…)"},     {19, L"abcdefghij  (1234…)"},
        {18, L"abcdefgh…  (1234…)"},       {17, L"abcdefg…  (1234…)"},
        {16, L"abcdefg…  (123…)"},         {0, nullptr}  // sentinel terminator
    };
    for (size_t i = 0; testcases1[i].expected != nullptr; i++) {
        testcases1[i].run(pager);
    }

    // These test cases have heavyweight completions
    auto c2 = new_completion_with(L"abcdefghijklmnopqrs", L"1", 0);
    auto c2s = new_completion_list();
    c2s->push_back(*c2);
    pager.set_completions(*c2s);
    const pager_layout_testcase_t testcases2[] = {
        {26, L"abcdefghijklmnopqrs  (1)"}, {25, L"abcdefghijklmnopqrs  (1)"},
        {24, L"abcdefghijklmnopqrs  (1)"}, {23, L"abcdefghijklmnopq…  (1)"},
        {22, L"abcdefghijklmnop…  (1)"},   {21, L"abcdefghijklmno…  (1)"},
        {20, L"abcdefghijklmn…  (1)"},     {19, L"abcdefghijklm…  (1)"},
        {18, L"abcdefghijkl…  (1)"},       {17, L"abcdefghijk…  (1)"},
        {16, L"abcdefghij…  (1)"},         {0, nullptr}  // sentinel terminator
    };
    for (size_t i = 0; testcases2[i].expected != nullptr; i++) {
        testcases2[i].run(pager);
    }

    // These test cases have no descriptions
    auto c3 = new_completion_with(L"abcdefghijklmnopqrst", L"", 0);
    auto c3s = new_completion_list();
    c3s->push_back(*c3);
    pager.set_completions(*c3s);
    const pager_layout_testcase_t testcases3[] = {
        {26, L"abcdefghijklmnopqrst"}, {25, L"abcdefghijklmnopqrst"},
        {24, L"abcdefghijklmnopqrst"}, {23, L"abcdefghijklmnopqrst"},
        {22, L"abcdefghijklmnopqrst"}, {21, L"abcdefghijklmnopqrst"},
        {20, L"abcdefghijklmnopqrst"}, {19, L"abcdefghijklmnopqr…"},
        {18, L"abcdefghijklmnopq…"},   {17, L"abcdefghijklmnop…"},
        {16, L"abcdefghijklmno…"},     {0, nullptr}  // sentinel terminator
    };
    for (size_t i = 0; testcases3[i].expected != nullptr; i++) {
        testcases3[i].run(pager);
    }
}

// todo!("port this")
enum word_motion_t { word_motion_left, word_motion_right };
static void test_1_word_motion(word_motion_t motion, move_word_style_t style,
                               const wcstring &test) {
    wcstring command;
    std::set<size_t> stops;

    // Carets represent stops and should be cut out of the command.
    for (wchar_t wc : test) {
        if (wc == L'^') {
            stops.insert(command.size());
        } else {
            command.push_back(wc);
        }
    }

    size_t idx, end;
    if (motion == word_motion_left) {
        idx = *std::max_element(stops.begin(), stops.end());
        end = 0;
    } else {
        idx = *std::min_element(stops.begin(), stops.end());
        end = command.size();
    }
    stops.erase(idx);

    auto sm = new_move_word_state_machine(style);
    while (idx != end) {
        size_t char_idx = (motion == word_motion_left ? idx - 1 : idx);
        wchar_t wc = command.at(char_idx);
        bool will_stop = !sm->consume_char(wc);
        // std::fwprintf(stdout, L"idx %lu, looking at %lu (%c): %d\n", idx, char_idx, (char)wc,
        //          will_stop);
        bool expected_stop = (stops.count(idx) > 0);
        if (will_stop != expected_stop) {
            wcstring tmp = command;
            tmp.insert(idx, L"^");
            const char *dir = (motion == word_motion_left ? "left" : "right");
            if (will_stop) {
                err(L"Word motion: moving %s, unexpected stop at idx %lu: '%ls'", dir, idx,
                    tmp.c_str());
            } else if (!will_stop && expected_stop) {
                err(L"Word motion: moving %s, should have stopped at idx %lu: '%ls'", dir, idx,
                    tmp.c_str());
            }
        }
        // We don't expect to stop here next time.
        if (expected_stop) {
            stops.erase(idx);
        }
        if (will_stop) {
            sm->reset();
        } else {
            idx += (motion == word_motion_left ? -1 : 1);
        }
    }
}

// todo!("port this")
/// Test word motion (forward-word, etc.). Carets represent cursor stops.
static void test_word_motion() {
    say(L"Testing word motion");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_punctuation,
                       L"^echo ^hello_^world.^txt^");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"^echo^ hello^_world^.txt^");

    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_punctuation,
                       L"echo ^foo_^foo_^foo/^/^/^/^/^    ^");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"^echo^ foo^_foo^_foo^/^/^/^/^/    ^");

    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^/^foo/^bar/^baz/^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^echo ^--foo ^--bar^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^echo ^hi ^> ^/^dev/^null^");

    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^echo ^/^foo/^bar{^aaa,^bbb,^ccc}^bak/^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^echo ^bak ^///^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^aaa ^@ ^@^aaa^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^aaa ^a ^@^aaa^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^aaa ^@@@ ^@@^aa^");
    test_1_word_motion(word_motion_left, move_word_style_t::move_word_style_path_components,
                       L"^aa^@@  ^aa@@^a^");

    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"^a^ bcd^");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"a^b^ cde^");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"^ab^ cde^");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_punctuation,
                       L"^ab^&cd^ ^& ^e^ f^&");

    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_whitespace,
                       L"^^a-b-c^ d-e-f");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_whitespace,
                       L"^a-b-c^\n d-e-f^ ");
    test_1_word_motion(word_motion_right, move_word_style_t::move_word_style_whitespace,
                       L"^a-b-c^\n\nd-e-f^ ");
}

// todo!("port this?")
static void test_wcstod() {
    say(L"Testing fish_wcstod");
    auto tod_test = [](const wchar_t *a, const char *b) {
        char *narrow_end = nullptr;
        wchar_t *wide_end = nullptr;
        double val1 = fish_wcstod(a, &wide_end);
        double val2 = strtod(b, &narrow_end);
        do_test((std::isnan(val1) && std::isnan(val2)) || fabs(val1 - val2) <= __DBL_EPSILON__);
        do_test(wide_end - a == narrow_end - b);
    };
    tod_test(L"", "");
    tod_test(L"1.2", "1.2");
    tod_test(L"1.5", "1.5");
    tod_test(L"-1000", "-1000");
    tod_test(L"0.12345", "0.12345");
    tod_test(L"nope", "nope");
}

// todo!("already ported, delete this")
/// Testing colors.
static void test_colors() {
    say(L"Testing colors");
    do_test(rgb_color_t(L"#FF00A0").is_rgb());
    do_test(rgb_color_t(L"FF00A0").is_rgb());
    do_test(rgb_color_t(L"#F30").is_rgb());
    do_test(rgb_color_t(L"F30").is_rgb());
    do_test(rgb_color_t(L"f30").is_rgb());
    do_test(rgb_color_t(L"#FF30a5").is_rgb());
    do_test(rgb_color_t(L"3f30").is_none());
    do_test(rgb_color_t(L"##f30").is_none());
    do_test(rgb_color_t(L"magenta").is_named());
    do_test(rgb_color_t(L"MaGeNTa").is_named());
    do_test(rgb_color_t(L"mooganta").is_none());
}

// todo!("port this")
static void test_1_completion(wcstring line, const wcstring &completion, complete_flags_t flags,
                              bool append_only, wcstring expected, long source_line) {
    // str is given with a caret, which we use to represent the cursor position. Find it.
    const size_t in_cursor_pos = line.find(L'^');
    do_test(in_cursor_pos != wcstring::npos);
    line.erase(in_cursor_pos, 1);

    const size_t out_cursor_pos = expected.find(L'^');
    do_test(out_cursor_pos != wcstring::npos);
    expected.erase(out_cursor_pos, 1);

    size_t cursor_pos = in_cursor_pos;
    wcstring result =
        completion_apply_to_command_line(completion, flags, line, &cursor_pos, append_only);
    if (result != expected) {
        std::fwprintf(stderr, L"line %ld: %ls + %ls -> [%ls], expected [%ls]\n", source_line,
                      line.c_str(), completion.c_str(), result.c_str(), expected.c_str());
    }
    do_test(result == expected);
    do_test(cursor_pos == out_cursor_pos);
}

// todo!("port this")
static void test_completion_insertions() {
#define TEST_1_COMPLETION(a, b, c, d, e) test_1_completion(a, b, c, d, e, __LINE__)
    say(L"Testing completion insertions");
    TEST_1_COMPLETION(L"foo^", L"bar", 0, false, L"foobar ^");
    // An unambiguous completion of a token that is already trailed by a space character.
    // After completing, the cursor moves on to the next token, suggesting to the user that the
    // current token is finished.
    TEST_1_COMPLETION(L"foo^ baz", L"bar", 0, false, L"foobar ^baz");
    TEST_1_COMPLETION(L"'foo^", L"bar", 0, false, L"'foobar' ^");
    TEST_1_COMPLETION(L"'foo'^", L"bar", 0, false, L"'foobar' ^");
    TEST_1_COMPLETION(L"'foo\\'^", L"bar", 0, false, L"'foo\\'bar' ^");
    TEST_1_COMPLETION(L"foo\\'^", L"bar", 0, false, L"foo\\'bar ^");

    // Test append only.
    TEST_1_COMPLETION(L"foo^", L"bar", 0, true, L"foobar ^");
    TEST_1_COMPLETION(L"foo^ baz", L"bar", 0, true, L"foobar ^baz");
    TEST_1_COMPLETION(L"'foo^", L"bar", 0, true, L"'foobar' ^");
    TEST_1_COMPLETION(L"'foo'^", L"bar", 0, true, L"'foo'bar ^");
    TEST_1_COMPLETION(L"'foo\\'^", L"bar", 0, true, L"'foo\\'bar' ^");
    TEST_1_COMPLETION(L"foo\\'^", L"bar", 0, true, L"foo\\'bar ^");

    TEST_1_COMPLETION(L"foo^", L"bar", COMPLETE_NO_SPACE, false, L"foobar^");
    TEST_1_COMPLETION(L"'foo^", L"bar", COMPLETE_NO_SPACE, false, L"'foobar^");
    TEST_1_COMPLETION(L"'foo'^", L"bar", COMPLETE_NO_SPACE, false, L"'foobar'^");
    TEST_1_COMPLETION(L"'foo\\'^", L"bar", COMPLETE_NO_SPACE, false, L"'foo\\'bar^");
    TEST_1_COMPLETION(L"foo\\'^", L"bar", COMPLETE_NO_SPACE, false, L"foo\\'bar^");

    TEST_1_COMPLETION(L"foo^", L"bar", COMPLETE_REPLACES_TOKEN, false, L"bar ^");
    TEST_1_COMPLETION(L"'foo^", L"bar", COMPLETE_REPLACES_TOKEN, false, L"bar ^");

    // See #6130
    TEST_1_COMPLETION(L": (:^ ''", L"", 0, false, L": (: ^''");
}

// todo!("port this")
static void test_autosuggestion_combining() {
    say(L"Testing autosuggestion combining");
    do_test(combine_command_and_autosuggestion(L"alpha", L"alphabeta") == L"alphabeta");

    // When the last token contains no capital letters, we use the case of the autosuggestion.
    do_test(combine_command_and_autosuggestion(L"alpha", L"ALPHABETA") == L"ALPHABETA");

    // When the last token contains capital letters, we use its case.
    do_test(combine_command_and_autosuggestion(L"alPha", L"alphabeTa") == L"alPhabeTa");

    // If autosuggestion is not longer than input, use the input's case.
    do_test(combine_command_and_autosuggestion(L"alpha", L"ALPHAA") == L"ALPHAA");
    do_test(combine_command_and_autosuggestion(L"alpha", L"ALPHA") == L"alpha");
}

// todo!("port this")
static void test_input() {
    say(L"Testing input");
    inputter_t input{parser_principal_parser()->deref()};
    // Ensure sequences are order independent. Here we add two bindings where the first is a prefix
    // of the second, and then emit the second key list. The second binding should be invoked, not
    // the first!
    wcstring prefix_binding = L"qqqqqqqa";
    wcstring desired_binding = prefix_binding + L'a';

    {
        auto input_mapping = input_mappings();
        input_mapping->add(prefix_binding, L"up-line");
        input_mapping->add(desired_binding, L"down-line");
    }

    // Push the desired binding to the queue.
    for (wchar_t c : desired_binding) {
        input.queue_char(c);
    }

    // Now test.
    auto evt = input.read_char();
    if (!evt.is_readline()) {
        err(L"Event is not a readline");
    } else if (evt.get_readline() != readline_cmd_t::down_line) {
        err(L"Expected to read char down_line");
    }
}

// todo!("port this")
static void test_undo() {
    say(L"Testing undo/redo setting and restoring text and cursor position.");

    editable_line_t line;
    do_test(!line.undo());  // nothing to undo
    do_test(line.text().empty());
    do_test(line.position() == 0);
    line.push_edit(edit_t(0, 0, L"a b c"), true);
    do_test(line.text() == L"a b c");
    do_test(line.position() == 5);
    line.set_position(2);
    line.push_edit(edit_t(2, 1, L"B"), true);  // replacement right of cursor
    do_test(line.text() == L"a B c");
    line.undo();
    do_test(line.text() == L"a b c");
    do_test(line.position() == 2);
    line.redo();
    do_test(line.text() == L"a B c");
    do_test(line.position() == 3);

    do_test(!line.redo());  // nothing to redo

    line.push_edit(edit_t(0, 2, L""), true);  // deletion left of cursor
    do_test(line.text() == L"B c");
    do_test(line.position() == 1);
    line.undo();
    do_test(line.text() == L"a B c");
    do_test(line.position() == 3);
    line.redo();
    do_test(line.text() == L"B c");
    do_test(line.position() == 1);

    line.push_edit(edit_t(0, line.size(), L"a b c"), true);  // replacement left and right of cursor
    do_test(line.text() == L"a b c");
    do_test(line.position() == 5);

    say(L"Testing undoing coalesced edits.");
    line.clear();
    line.push_edit(edit_t(line.position(), 0, L"a"), true);
    line.push_edit(edit_t(line.position(), 0, L"b"), true);
    line.push_edit(edit_t(line.position(), 0, L"c"), true);
    line.push_edit(edit_t(line.position(), 0, L" "), true);
    line.undo();
    line.undo();
    line.redo();
    do_test(line.text() == L"abc");
    // This removes the space insertion from the history, but does not coalesce with the first edit.
    line.push_edit(edit_t(line.position(), 0, L"d"), true);
    line.push_edit(edit_t(line.position(), 0, L"e"), true);
    do_test(line.text() == L"abcde");
    line.undo();
    do_test(line.text() == L"abc");
}

// todo!("port this")
static void test_new_parser_correctness() {
    say(L"Testing parser correctness");
    const struct parser_test_t {
        const wchar_t *src;
        bool ok;
    } parser_tests[] = {
        {L"; ; ; ", true},
        {L"if ; end", false},
        {L"if true ; end", true},
        {L"if true; end ; end", false},
        {L"if end; end ; end", false},
        {L"if end", false},
        {L"end", false},
        {L"for i i", false},
        {L"for i in a b c ; end", true},
        {L"begin end", true},
        {L"begin; end", true},
        {L"begin if true; end; end;", true},
        {L"begin if true ; echo hi ; end; end", true},
        {L"true && false || false", true},
        {L"true || false; and true", true},
        {L"true || ||", false},
        {L"|| true", false},
        {L"true || \n\n false", true},
    };

    for (const auto &test : parser_tests) {
        auto ast = ast_parse(test.src);
        bool success = !ast->errored();
        if (success && !test.ok) {
            err(L"\"%ls\" should NOT have parsed, but did", test.src);
        } else if (!success && test.ok) {
            err(L"\"%ls\" should have parsed, but failed", test.src);
        }
    }
    say(L"Parse tests complete");
}

// Given that we have an array of 'fuzz_count' strings, we wish to enumerate all permutations of
// 'len' values. We do this by incrementing an integer, interpreting it as "base fuzz_count".
static inline bool string_for_permutation(const wcstring *fuzzes, size_t fuzz_count, size_t len,
                                          size_t permutation, wcstring *out_str) {
    out_str->clear();

    size_t remaining_permutation = permutation;
    for (size_t i = 0; i < len; i++) {
        size_t idx = remaining_permutation % fuzz_count;
        remaining_permutation /= fuzz_count;

        out_str->append(fuzzes[idx]);
        out_str->push_back(L' ');
    }
    // Return false if we wrapped.
    return remaining_permutation == 0;
}

// todo!("port this")
static void test_new_parser_fuzzing() {
    say(L"Fuzzing parser");
    const wcstring fuzzes[] = {
        L"if",      L"else", L"for", L"in",  L"while", L"begin", L"function",
        L"switch",  L"case", L"end", L"and", L"or",    L"not",   L"command",
        L"builtin", L"foo",  L"|",   L"^",   L"&",     L";",
    };

    // Generate a list of strings of all keyword / token combinations.
    wcstring src;
    src.reserve(128);

    auto errors = new_parse_error_list();

    double start = timef();
    bool log_it = true;
    unsigned long max_len = 5;
    for (unsigned long len = 0; len < max_len; len++) {
        if (log_it) std::fwprintf(stderr, L"%lu / %lu...", len, max_len);

        // We wish to look at all permutations of 4 elements of 'fuzzes' (with replacement).
        // Construct an int and keep incrementing it.
        unsigned long permutation = 0;
        while (string_for_permutation(fuzzes, sizeof fuzzes / sizeof *fuzzes, len, permutation++,
                                      &src)) {
            ast_parse(src);
        }
        if (log_it) std::fwprintf(stderr, L"done (%lu)\n", permutation);
    }
    double end = timef();
    if (log_it) say(L"All fuzzed in %.2f seconds!", end - start);
}

// todo!("port this")
// Parse a statement, returning the command, args (joined by spaces), and the decoration. Returns
// true if successful.
static bool test_1_parse_ll2(const wcstring &src, wcstring *out_cmd, wcstring *out_joined_args,
                             statement_decoration_t *out_deco) {
    using namespace ast;
    out_cmd->clear();
    out_joined_args->clear();
    *out_deco = statement_decoration_t::none;

    auto ast = ast_parse(src);
    if (ast->errored()) return false;

    // Get the statement. Should only have one.
    const decorated_statement_t *statement = nullptr;
    for (auto ast_traversal = new_ast_traversal(*ast->top());;) {
        auto n = ast_traversal->next();
        if (!n->has_value()) break;
        if (const auto *tmp = n->try_as_decorated_statement()) {
            if (statement) {
                say(L"More than one decorated statement found in '%ls'", src.c_str());
                return false;
            }
            statement = tmp;
        }
    }
    if (!statement) {
        say(L"No decorated statement found in '%ls'", src.c_str());
        return false;
    }

    // Return its decoration and command.
    *out_deco = statement->decoration();
    *out_cmd = *statement->command().source(src);

    // Return arguments separated by spaces.
    bool first = true;
    for (size_t i = 0; i < statement->args_or_redirs().count(); i++) {
        const ast::argument_or_redirection_t &arg = *statement->args_or_redirs().at(i);
        if (!arg.is_argument()) continue;
        if (!first) out_joined_args->push_back(L' ');
        out_joined_args->append(*arg.ptr()->source(src));
        first = false;
    }

    return true;
}

// Verify that 'function -h' and 'function --help' are plain statements but 'function --foo' is
// not (issue #1240).
template <ast::type_t Type>
static void check_function_help(const wchar_t *src) {
    using namespace ast;
    auto ast = ast_parse(src);
    if (ast->errored()) {
        err(L"Failed to parse '%ls'", src);
    }

    int count = 0;
    for (auto ast_traversal = new_ast_traversal(*ast->top());;) {
        auto node = ast_traversal->next();
        if (!node->has_value()) break;
        count += (node->typ() == Type);
    }
    if (count == 0) {
        err(L"Failed to find node of type '%ls'", ast_type_to_string(Type));
    } else if (count > 1) {
        err(L"Found too many nodes of type '%ls'", ast_type_to_string(Type));
    }
}

// todo!("port this")
// Test the LL2 (two token lookahead) nature of the parser by exercising the special builtin and
// command handling. In particular, 'command foo' should be a decorated statement 'foo' but 'command
// -help' should be an undecorated statement 'command' with argument '--help', and NOT attempt to
// run a command called '--help'.
static void test_new_parser_ll2() {
    say(L"Testing parser two-token lookahead");

    const struct {
        wcstring src;
        wcstring cmd;
        wcstring args;
        statement_decoration_t deco;
    } tests[] = {{L"echo hello", L"echo", L"hello", statement_decoration_t::none},
                 {L"command echo hello", L"echo", L"hello", statement_decoration_t::command},
                 {L"exec echo hello", L"echo", L"hello", statement_decoration_t::exec},
                 {L"command command hello", L"command", L"hello", statement_decoration_t::command},
                 {L"builtin command hello", L"command", L"hello", statement_decoration_t::builtin},
                 {L"command --help", L"command", L"--help", statement_decoration_t::none},
                 {L"command -h", L"command", L"-h", statement_decoration_t::none},
                 {L"command", L"command", L"", statement_decoration_t::none},
                 {L"command -", L"command", L"-", statement_decoration_t::none},
                 {L"command --", L"command", L"--", statement_decoration_t::none},
                 {L"builtin --names", L"builtin", L"--names", statement_decoration_t::none},
                 {L"function", L"function", L"", statement_decoration_t::none},
                 {L"function --help", L"function", L"--help", statement_decoration_t::none}};

    for (const auto &test : tests) {
        wcstring cmd, args;
        statement_decoration_t deco = statement_decoration_t::none;
        bool success = test_1_parse_ll2(test.src, &cmd, &args, &deco);
        if (!success) err(L"Parse of '%ls' failed on line %ld", test.cmd.c_str(), (long)__LINE__);
        if (cmd != test.cmd)
            err(L"When parsing '%ls', expected command '%ls' but got '%ls' on line %ld",
                test.src.c_str(), test.cmd.c_str(), cmd.c_str(), (long)__LINE__);
        if (args != test.args)
            err(L"When parsing '%ls', expected args '%ls' but got '%ls' on line %ld",
                test.src.c_str(), test.args.c_str(), args.c_str(), (long)__LINE__);
        if (deco != test.deco)
            err(L"When parsing '%ls', expected decoration %d but got %d on line %ld",
                test.src.c_str(), (int)test.deco, (int)deco, (long)__LINE__);
    }

    check_function_help<ast::type_t::decorated_statement>(L"function -h");
    check_function_help<ast::type_t::decorated_statement>(L"function --help");
    check_function_help<ast::type_t::function_header>(L"function --foo; end");
    check_function_help<ast::type_t::function_header>(L"function foo; end");
}

// todo!("port this")
static void test_new_parser_ad_hoc() {
    using namespace ast;
    // Very ad-hoc tests for issues encountered.
    say(L"Testing new parser ad hoc tests");

    // Ensure that 'case' terminates a job list.
    const wcstring src = L"switch foo ; case bar; case baz; end";
    auto ast = ast_parse(src);
    if (ast->errored()) {
        err(L"Parsing failed");
    }

    // Expect two case_item_lists. The bug was that we'd
    // try to run a command 'case'.
    int count = 0;
    for (auto ast_traversal = new_ast_traversal(*ast->top());;) {
        auto n = ast_traversal->next();
        if (!n->has_value()) break;
        count += (n->typ() == type_t::case_item);
    }
    if (count != 2) {
        err(L"Expected 2 case item nodes, found %d", count);
    }

    // Ensure that naked variable assignments don't hang.
    // The bug was that "a=" would produce an error but not be consumed,
    // leading to an infinite loop.

    // By itself it should produce an error.
    ast = ast_parse(L"a=");
    do_test(ast->errored());

    // If we are leaving things unterminated, this should not produce an error.
    // i.e. when typing "a=" at the command line, it should be treated as valid
    // because we don't want to color it as an error.
    ast = ast_parse(L"a=", parse_flag_leave_unterminated);
    do_test(!ast->errored());

    auto errors = new_parse_error_list();
    ast = ast_parse(L"begin; echo (", parse_flag_leave_unterminated, &*errors);
    do_test(errors->size() == 1 &&
            errors->at(0)->code() == parse_error_code_t::tokenizer_unterminated_subshell);

    errors->clear();
    ast = ast_parse(L"for x in (", parse_flag_leave_unterminated, &*errors);
    do_test(errors->size() == 1 &&
            errors->at(0)->code() == parse_error_code_t::tokenizer_unterminated_subshell);

    errors->clear();
    ast = ast_parse(L"begin; echo '", parse_flag_leave_unterminated, &*errors);
    do_test(errors->size() == 1 &&
            errors->at(0)->code() == parse_error_code_t::tokenizer_unterminated_quote);
}

// todo!("port this")
static void test_new_parser_errors() {
    say(L"Testing new parser error reporting");
    const struct {
        const wchar_t *src;
        parse_error_code_t code;
    } tests[] = {
        {L"echo 'abc", parse_error_code_t::tokenizer_unterminated_quote},
        {L"'", parse_error_code_t::tokenizer_unterminated_quote},
        {L"echo (abc", parse_error_code_t::tokenizer_unterminated_subshell},

        {L"end", parse_error_code_t::unbalancing_end},
        {L"echo hi ; end", parse_error_code_t::unbalancing_end},

        {L"else", parse_error_code_t::unbalancing_else},
        {L"if true ; end ; else", parse_error_code_t::unbalancing_else},

        {L"case", parse_error_code_t::unbalancing_case},
        {L"if true ; case ; end", parse_error_code_t::generic},

        {L"true | and", parse_error_code_t::andor_in_pipeline},

        {L"a=", parse_error_code_t::bare_variable_assignment},
    };

    for (const auto &test : tests) {
        const wcstring src = test.src;
        parse_error_code_t expected_code = test.code;

        auto errors = new_parse_error_list();
        auto ast = ast_parse(src, parse_flag_none, &*errors);
        if (!ast->errored()) {
            err(L"Source '%ls' was expected to fail to parse, but succeeded", src.c_str());
        }

        if (errors->size() != 1) {
            err(L"Source '%ls' was expected to produce 1 error, but instead produced %lu errors",
                src.c_str(), errors->size());
            for (size_t i = 0; i < errors->size(); i++) {
                fprintf(stderr, "%ls\n", errors->at(i)->describe(src, false)->c_str());
            }
        } else if (errors->at(0)->code() != expected_code) {
            err(L"Source '%ls' was expected to produce error code %lu, but instead produced error "
                L"code %lu",
                src.c_str(), expected_code, (unsigned long)errors->at(0)->code());
            for (size_t i = 0; i < errors->size(); i++) {
                err(L"\t\t%ls", errors->at(i)->describe(src, true)->c_str());
            }
        }
    }
}

// Given a format string, returns a list of non-empty strings separated by format specifiers. The
// format specifiers themselves are omitted.
static std::vector<wcstring> separate_by_format_specifiers(const wchar_t *format) {
    std::vector<wcstring> result;
    const wchar_t *cursor = format;
    const wchar_t *end = format + std::wcslen(format);
    while (cursor < end) {
        const wchar_t *next_specifier = std::wcschr(cursor, '%');
        if (next_specifier == nullptr) {
            next_specifier = end;
        }
        assert(next_specifier != nullptr);

        // Don't return empty strings.
        if (next_specifier > cursor) {
            result.emplace_back(cursor, next_specifier - cursor);
        }

        // Walk over the format specifier (if any).
        cursor = next_specifier;
        if (*cursor != '%') {
            continue;
        }

        cursor++;
        // Flag
        if (std::wcschr(L"#0- +'", *cursor)) cursor++;
        // Minimum field width
        while (iswdigit(*cursor)) cursor++;
        // Precision
        if (*cursor == L'.') {
            cursor++;
            while (iswdigit(*cursor)) cursor++;
        }
        // Length modifier
        if (!std::wcsncmp(cursor, L"ll", 2) || !std::wcsncmp(cursor, L"hh", 2)) {
            cursor += 2;
        } else if (std::wcschr(L"hljtzqL", *cursor)) {
            cursor++;
        }
        // The format specifier itself. We allow any character except NUL.
        if (*cursor != L'\0') {
            cursor += 1;
        }
        assert(cursor <= end);
    }
    return result;
}

// Given a format string 'format', return true if the string may have been produced by that format
// string. We do this by splitting the format string around the format specifiers, and then ensuring
// that each of the remaining chunks is found (in order) in the string.
static bool string_matches_format(const wcstring &string, const wchar_t *format) {
    bool result = true;
    std::vector<wcstring> components = separate_by_format_specifiers(format);
    size_t idx = 0;
    for (const auto &component : components) {
        size_t where = string.find(component, idx);
        if (where == wcstring::npos) {
            result = false;
            break;
        }
        idx = where + component.size();
        assert(idx <= string.size());
    }
    return result;
}

// todo!("port this")
static void test_error_messages() {
    say(L"Testing error messages");
    const struct error_test_t {
        const wchar_t *src;
        const wchar_t *error_text_format;
    } error_tests[] = {{L"echo $^", ERROR_BAD_VAR_CHAR1},
                       {L"echo foo${a}bar", ERROR_BRACKETED_VARIABLE1},
                       {L"echo foo\"${a}\"bar", ERROR_BRACKETED_VARIABLE_QUOTED1},
                       {L"echo foo\"${\"bar", ERROR_BAD_VAR_CHAR1},
                       {L"echo $?", ERROR_NOT_STATUS},
                       {L"echo $$", ERROR_NOT_PID},
                       {L"echo $#", ERROR_NOT_ARGV_COUNT},
                       {L"echo $@", ERROR_NOT_ARGV_AT},
                       {L"echo $*", ERROR_NOT_ARGV_STAR},
                       {L"echo $", ERROR_NO_VAR_NAME},
                       {L"echo foo\"$\"bar", ERROR_NO_VAR_NAME},
                       {L"echo \"foo\"$\"bar\"", ERROR_NO_VAR_NAME},
                       {L"echo foo $ bar", ERROR_NO_VAR_NAME}};

    auto errors = new_parse_error_list();
    for (const auto &test : error_tests) {
        errors->clear();
        parse_util_detect_errors(test.src, &*errors);
        do_test(!errors->empty());
        if (!errors->empty()) {
            do_test1(string_matches_format(*errors->at(0)->text(), test.error_text_format),
                     test.src);
        }
    }
}

// todo!("port this")
static void test_wwrite_to_fd() {
    say(L"Testing wwrite_to_fd");
    char t[] = "/tmp/fish_test_wwrite.XXXXXX";
    autoclose_fd_t tmpfd{mkstemp(t)};
    if (!tmpfd.valid()) {
        err(L"Unable to create temporary file");
        return;
    }
    tmpfd.close();

    size_t sizes[] = {0, 1, 2, 3, 5, 13, 23, 64, 128, 255, 4096, 4096 * 2};
    for (size_t size : sizes) {
        autoclose_fd_t fd{open(t, O_RDWR | O_TRUNC | O_CREAT, 0666)};
        if (!fd.valid()) {
            wperror(L"open");
            err(L"Unable to open temporary file");
            return;
        }
        wcstring input{};
        for (size_t i = 0; i < size; i++) {
            input.push_back(wchar_t(random()));
        }

        ssize_t amt = wwrite_to_fd(input, fd.fd());
        if (amt < 0) {
            wperror(L"write");
            err(L"Unable to write to temporary file");
            return;
        }
        std::string narrow = wcs2string(input);
        size_t expected_size = narrow.size();
        do_test(static_cast<size_t>(amt) == expected_size);

        if (lseek(fd.fd(), 0, SEEK_SET) < 0) {
            wperror(L"seek");
            err(L"Unable to seek temporary file");
            return;
        }

        std::string contents(expected_size, '\0');
        ssize_t read_amt = read(fd.fd(), &contents[0], expected_size);
        do_test(read_amt >= 0 && static_cast<size_t>(read_amt) == expected_size);
    }
    (void)remove(t);
}

/// Helper for test_timezone_env_vars().
long return_timezone_hour(time_t tstamp, const wchar_t *timezone) {
    env_stack_t vars{parser_principal_parser()->deref().vars_boxed()};
    struct tm ltime;
    char ltime_str[3];
    char *str_ptr;
    size_t n;

    vars.set_one(L"TZ", ENV_EXPORT, timezone);

    const auto var = vars.get(L"TZ", ENV_DEFAULT);
    (void)var;

    localtime_r(&tstamp, &ltime);
    n = strftime(ltime_str, 3, "%H", &ltime);
    if (n != 2) {
        err(L"strftime() returned %d, expected 2", n);
        return 0;
    }
    return strtol(ltime_str, &str_ptr, 10);
}

// todo!("port this")
static void test_env_snapshot() {
    if (system("mkdir -p test/fish_env_snapshot_test/")) err(L"mkdir failed");
    bool pushed = pushd("test/fish_env_snapshot_test");
    do_test(pushed);
    env_stack_t vars{parser_principal_parser()->deref().vars_boxed()};
    vars.push(true);
    wcstring before_pwd = vars.get(L"PWD")->as_string();
    vars.set(L"test_env_snapshot_var", 0, std::vector<wcstring>{L"before"});
    const auto snapshot = vars.snapshot();
    vars.set(L"PWD", 0, std::vector<wcstring>{L"/newdir"});
    vars.set(L"test_env_snapshot_var", 0, std::vector<wcstring>{L"after"});
    vars.set(L"test_env_snapshot_var_2", 0, std::vector<wcstring>{L"after"});

    // vars should be unaffected by the snapshot
    do_test(vars.get(L"PWD")->as_string() == L"/newdir");
    do_test(vars.get(L"test_env_snapshot_var")->as_string() == L"after");
    do_test(vars.get(L"test_env_snapshot_var_2")->as_string() == L"after");

    // snapshot should have old values of vars
    do_test(snapshot->get(L"PWD")->as_string() == before_pwd);
    do_test(snapshot->get(L"test_env_snapshot_var")->as_string() == L"before");
    do_test(snapshot->get(L"test_env_snapshot_var_2") == none());

    // snapshots see global var changes except for perproc like PWD
    vars.set(L"test_env_snapshot_var_3", ENV_GLOBAL, std::vector<wcstring>{L"reallyglobal"});
    do_test(vars.get(L"test_env_snapshot_var_3")->as_string() == L"reallyglobal");
    do_test(snapshot->get(L"test_env_snapshot_var_3")->as_string() == L"reallyglobal");

    vars.pop();
    popd();
}

// todo!("port this")
static void test_illegal_command_exit_code() {
    say(L"Testing illegal command exit code");

    // We need to be in an empty directory so that none of the wildcards match a file that might be
    // in the fish source tree. In particular we need to ensure that "?" doesn't match a file
    // named by a single character. See issue #3852.
    if (!pushd("test/temp")) return;

    struct command_result_tuple_t {
        const wchar_t *txt;
        int result;
    };

    const command_result_tuple_t tests[] = {
        {L"echo -n", STATUS_CMD_OK},
        {L"pwd", STATUS_CMD_OK},
        {L"UNMATCHABLE_WILDCARD*", STATUS_UNMATCHED_WILDCARD},
        {L"UNMATCHABLE_WILDCARD**", STATUS_UNMATCHED_WILDCARD},
        {L"?", STATUS_UNMATCHED_WILDCARD},
        {L"abc?def", STATUS_UNMATCHED_WILDCARD},
    };

    auto empty_ios = new_io_chain();
    const parser_t &parser = parser_principal_parser()->deref();

    for (const auto &test : tests) {
        parser.eval(test.txt, *empty_ios);

        int exit_status = parser.get_last_status();
        if (exit_status != test.result) {
            err(L"command '%ls': expected exit code %d, got %d", test.txt, test.result,
                exit_status);
        }
    }

    popd();
}

// todo!("no need to port, delete this")
void test_maybe() {
    say(L"Testing maybe_t");
    // maybe_t<T> bool conversion is only enabled for non-bool-convertible T types
    do_test(!bool(maybe_t<wcstring>()));
    maybe_t<int> m(3);
    do_test(m.has_value());
    do_test(m.value() == 3);
    m.reset();
    do_test(!m.has_value());
    m = 123;
    do_test(m.has_value());
    do_test(m.has_value() && m.value() == 123);
    m = maybe_t<int>();
    do_test(!m.has_value());
    m = maybe_t<int>(64);
    do_test(m.has_value() && m.value() == 64);
    m = 123;
    do_test(m == maybe_t<int>(123));
    do_test(m != maybe_t<int>());
    do_test(maybe_t<int>() == none());
    do_test(!maybe_t<int>(none()).has_value());
    maybe_t<wcstring> n = none();
    do_test(!bool(n));

    maybe_t<std::string> m0 = none();
    maybe_t<std::string> m3("hi");
    maybe_t<std::string> m4 = m3;
    do_test(m4 && *m4 == "hi");
    maybe_t<std::string> m5 = m0;
    do_test(!m5);

    maybe_t<std::string> acquire_test("def");
    do_test(acquire_test);
    std::string res = acquire_test.acquire();
    do_test(!acquire_test);
    do_test(res == "def");

    // maybe_t<T> should be copyable iff T is copyable.
    using copyable = std::shared_ptr<int>;
    using noncopyable = std::unique_ptr<int>;
    do_test(std::is_copy_assignable<maybe_t<copyable>>::value == true);
    do_test(std::is_copy_constructible<maybe_t<copyable>>::value == true);
    do_test(std::is_copy_assignable<maybe_t<noncopyable>>::value == false);
    do_test(std::is_copy_constructible<maybe_t<noncopyable>>::value == false);

    // We can construct a maybe_t from noncopyable things.
    maybe_t<noncopyable> nmt{make_unique<int>(42)};
    do_test(**nmt == 42);

    maybe_t<std::string> c1{"abc"};
    maybe_t<std::string> c2 = c1;
    do_test(c1.value() == "abc");
    do_test(c2.value() == "abc");
    c2 = c1;
    do_test(c1.value() == "abc");
    do_test(c2.value() == "abc");

    do_test(c2.value_or("derp") == "abc");
    do_test(c2.value_or("derp") == "abc");
    c2.reset();
    do_test(c2.value_or("derp") == "derp");
}

// todo!("already ported, delete this")
void test_normalize_path() {
    say(L"Testing path normalization");
    do_test(normalize_path(L"") == L".");
    do_test(normalize_path(L"..") == L"..");
    do_test(normalize_path(L"./") == L".");
    do_test(normalize_path(L"./.") == L".");
    do_test(normalize_path(L"/") == L"/");
    do_test(normalize_path(L"//") == L"//");
    do_test(normalize_path(L"///") == L"/");
    do_test(normalize_path(L"////") == L"/");
    do_test(normalize_path(L"/.///") == L"/");
    do_test(normalize_path(L".//") == L".");
    do_test(normalize_path(L"/.//../") == L"/");
    do_test(normalize_path(L"////abc") == L"/abc");
    do_test(normalize_path(L"/abc") == L"/abc");
    do_test(normalize_path(L"/abc/") == L"/abc");
    do_test(normalize_path(L"/abc/..def/") == L"/abc/..def");
    do_test(normalize_path(L"//abc/../def/") == L"//def");
    do_test(normalize_path(L"abc/../abc/../abc/../abc") == L"abc");
    do_test(normalize_path(L"../../") == L"../..");
    do_test(normalize_path(L"foo/./bar") == L"foo/bar");
    do_test(normalize_path(L"foo/../") == L".");
    do_test(normalize_path(L"foo/../foo") == L"foo");
    do_test(normalize_path(L"foo/../foo/") == L"foo");
    do_test(normalize_path(L"foo/././bar/.././baz") == L"foo/baz");

    do_test(path_normalize_for_cd(L"/", L"..") == L"/..");
    do_test(path_normalize_for_cd(L"/abc/", L"..") == L"/");
    do_test(path_normalize_for_cd(L"/abc/def/", L"..") == L"/abc");
    do_test(path_normalize_for_cd(L"/abc/def/", L"../..") == L"/");
    do_test(path_normalize_for_cd(L"/abc///def/", L"../..") == L"/");
    do_test(path_normalize_for_cd(L"/abc///def/", L"../..") == L"/");
    do_test(path_normalize_for_cd(L"/abc///def///", L"../..") == L"/");
    do_test(path_normalize_for_cd(L"/abc///def///", L"..") == L"/abc");
    do_test(path_normalize_for_cd(L"/abc///def///", L"..") == L"/abc");
    do_test(path_normalize_for_cd(L"/abc/def/", L"./././/..") == L"/abc");
    do_test(path_normalize_for_cd(L"/abc/def/", L"../../../") == L"/../");
    do_test(path_normalize_for_cd(L"/abc/def/", L"../ghi/..") == L"/abc/ghi/..");
}

// todo!("already ported, delete this")
void test_dirname_basename() {
    say(L"Testing wdirname and wbasename");
    const struct testcase_t {
        const wchar_t *path;
        const wchar_t *dir;
        const wchar_t *base;
    } testcases[] = {
        {L"", L".", L"."},
        {L"foo//", L".", L"foo"},
        {L"foo//////", L".", L"foo"},
        {L"/////foo", L"/", L"foo"},
        {L"/////foo", L"/", L"foo"},
        {L"//foo/////bar", L"//foo", L"bar"},
        {L"foo/////bar", L"foo", L"bar"},

        // Examples given in XPG4.2.
        {L"/usr/lib", L"/usr", L"lib"},
        {L"usr", L".", L"usr"},
        {L"/", L"/", L"/"},
        {L".", L".", L"."},
        {L"..", L".", L".."},
    };
    for (const auto &tc : testcases) {
        wcstring dir = wdirname(tc.path);
        if (dir != tc.dir) {
            err(L"Wrong dirname for \"%ls\": expected \"%ls\", got \"%ls\"", tc.path, tc.dir,
                dir.c_str());
        }
        wcstring base = wbasename(tc.path);
        if (base != tc.base) {
            err(L"Wrong basename for \"%ls\": expected \"%ls\", got \"%ls\"", tc.path, tc.base,
                base.c_str());
        }
    }
    // Ensures strings which greatly exceed PATH_MAX still work (#7837).
    wcstring longpath = get_overlong_path();
    wcstring longpath_dir = longpath.substr(0, longpath.rfind(L'/'));
    do_test(wdirname(longpath) == longpath_dir);
    do_test(wbasename(longpath) == L"overlong");
}

// todo!("port this")
static void test_pipes() {
    say(L"Testing pipes");
    // Here we just test that each pipe has CLOEXEC set and is in the high range.
    // Note pipe creation may fail due to fd exhaustion; don't fail in that case.
    std::vector<autoclose_pipes_t> pipes;
    for (int i = 0; i < 10; i++) {
        if (auto pipe = make_autoclose_pipes()) {
            pipes.push_back(pipe.acquire());
        }
    }
    for (const auto &pipe : pipes) {
        for (int fd : {pipe.read.fd(), pipe.write.fd()}) {
            do_test(fd >= k_first_high_fd);
            int flags = fcntl(fd, F_GETFD, 0);
            do_test(flags >= 0);
            do_test(bool(flags & FD_CLOEXEC));
        }
    }
}

// todo!("port this")
static void test_fd_event_signaller() {
    say(L"Testing fd event signaller");
    fd_event_signaller_t sema;
    do_test(!sema.try_consume());
    do_test(!sema.poll());

    // Post once.
    sema.post();
    do_test(sema.poll());
    do_test(sema.poll());
    do_test(sema.try_consume());
    do_test(!sema.poll());
    do_test(!sema.try_consume());

    // Posts are coalesced.
    sema.post();
    sema.post();
    sema.post();
    do_test(sema.poll());
    do_test(sema.poll());
    do_test(sema.try_consume());
    do_test(!sema.poll());
    do_test(!sema.try_consume());
}

// todo!("port this")
void test_wgetopt() {
    // Regression test for a crash.
    const wchar_t *const short_options = L"-a";
    const struct woption long_options[] = {
        {L"add", no_argument, 'a'},
        {},
    };
    const wchar_t *argv[] = {L"abbr", L"--add", L"emacsnw", L"emacs", L"-nw", nullptr};
    int argc = builtin_count_args(argv);
    wgetopter_t w;
    int opt;
    int a_count = 0;
    std::vector<wcstring> arguments;
    while ((opt = w.wgetopt_long(argc, argv, short_options, long_options, nullptr)) != -1) {
        switch (opt) {
            case 'a': {
                a_count += 1;
                break;
            }
            case 1: {
                // non-option argument
                do_test(w.woptarg != nullptr);
                arguments.push_back(w.woptarg);
                break;
            }
            case '?': {
                // unrecognized option
                if (argv[w.woptind - 1]) {
                    do_test(argv[w.woptind - 1] != nullptr);
                    arguments.push_back(argv[w.woptind - 1]);
                }
                break;
            }
            default: {
                err(L"Unexpected option '%d'", opt);
                break;
            }
        }
    }
    do_test(a_count == 1);
    do_test(arguments.size() == 3);
    do_test(join_strings(arguments, L' ') == L"emacsnw emacs -nw");
}

void test_rust_smoke() {
    size_t x = rust::add(37, 5);
    do_test(x == 42);
}

void test_rust_ffi() { rust::run_ffi_tests(); }

// typedef void (test_entry_point_t)();
using test_entry_point_t = void (*)();
struct test_t {
    const char *group;
    std::function<void()> test;
    bool opt_in = false;

    test_t(const char *group, test_entry_point_t test, bool opt_in = false)
        : group(group), test(test), opt_in(opt_in) {}
};

struct test_comparator_t {
    // template<typename T=test_t>
    int operator()(const test_t &lhs, const test_t &rhs) { return strcmp(lhs.group, rhs.group); }
};

// This magic string is required for CMake to pick up the list of tests
#define TEST_GROUP(x) x
static const test_t s_tests[]{
    {TEST_GROUP("utility_functions"), test_utility_functions},
    {TEST_GROUP("dir_iter"), test_dir_iter},
    {TEST_GROUP("wwrite_to_fd"), test_wwrite_to_fd},
    {TEST_GROUP("env"), test_env_snapshot},
    {TEST_GROUP("str_to_num"), test_str_to_num},
    {TEST_GROUP("enum"), test_enum_set},
    {TEST_GROUP("enum"), test_enum_array},
    {TEST_GROUP("autosuggestion"), test_autosuggestion_combining},
    {TEST_GROUP("new_parser_ll2"), test_new_parser_ll2},
    {TEST_GROUP("test_abbreviations"), test_abbreviations},
    {TEST_GROUP("new_parser_fuzzing"), test_new_parser_fuzzing},
    {TEST_GROUP("new_parser_correctness"), test_new_parser_correctness},
    {TEST_GROUP("new_parser_ad_hoc"), test_new_parser_ad_hoc},
    {TEST_GROUP("new_parser_errors"), test_new_parser_errors},
    {TEST_GROUP("error_messages"), test_error_messages},
    {TEST_GROUP("convert"), test_convert},
    {TEST_GROUP("convert"), test_convert_private_use},
    {TEST_GROUP("convert_ascii"), test_convert_ascii},
    {TEST_GROUP("perf_convert_ascii"), perf_convert_ascii, true},
    {TEST_GROUP("convert_nulls"), test_convert_nulls},
    {TEST_GROUP("iothread"), test_iothread},
    {TEST_GROUP("pthread"), test_pthread},
    {TEST_GROUP("debounce"), test_debounce},
    {TEST_GROUP("debounce"), test_debounce_timeout},
    {TEST_GROUP("parser"), test_parser},
    {TEST_GROUP("lru"), test_lru},
    {TEST_GROUP("wcstod"), test_wcstod},
    {TEST_GROUP("pager_navigation"), test_pager_navigation},
    {TEST_GROUP("pager_layout"), test_pager_layout},
    {TEST_GROUP("word_motion"), test_word_motion},
    {TEST_GROUP("colors"), test_colors},
    {TEST_GROUP("input"), test_input},
    {TEST_GROUP("undo"), test_undo},
    {TEST_GROUP("completion_insertions"), test_completion_insertions},
    {TEST_GROUP("illegal_command_exit_code"), test_illegal_command_exit_code},
    {TEST_GROUP("maybe"), test_maybe},
    {TEST_GROUP("normalize"), test_normalize_path},
    {TEST_GROUP("dirname"), test_dirname_basename},
    {TEST_GROUP("pipes"), test_pipes},
    {TEST_GROUP("fd_event"), test_fd_event_signaller},
    {TEST_GROUP("wgetopt"), test_wgetopt},
    {TEST_GROUP("rust_smoke"), test_rust_smoke},
    {TEST_GROUP("rust_ffi"), test_rust_ffi},
};

void list_tests() {
    std::set<std::string> groups;
    for (const auto &test : s_tests) {
        groups.insert(test.group);
    }

    for (const auto &group : groups) {
        std::fprintf(stdout, "%s\n", group.c_str());
    }
}

/// Main test.
int main(int argc, char **argv) {
    std::setlocale(LC_ALL, "");

    if (argc >= 2 && std::strcmp(argv[1], "--list") == 0) {
        list_tests();
        return 0;
    }

    // Look for the file tests/test.fish. We expect to run in a directory containing that file.
    // If we don't find it, walk up the directory hierarchy until we do, or error.
    while (access("./tests/test.fish", F_OK) != 0) {
        char wd[PATH_MAX + 1] = {};
        if (!getcwd(wd, sizeof wd)) {
            perror("getcwd");
            exit(-1);
        }
        if (!std::strcmp(wd, "/")) {
            std::fwprintf(
                stderr, L"Unable to find 'tests' directory, which should contain file test.fish\n");
            exit(EXIT_FAILURE);
        }
        if (chdir(dirname(wd)) < 0) {
            perror("chdir");
        }
    }

    srandom((unsigned int)time(nullptr));
    configure_thread_assertions_for_testing();

    // Set the program name to this sentinel value
    // This will prevent some misleading stderr output during the tests
    program_name = TESTS_PROGRAM_NAME;
    s_arguments = argv + 1;

    struct utsname uname_info;
    uname(&uname_info);

    say(L"Testing low-level functionality");
    rust_init();
    proc_init();
    rust_env_init(true);
    misc_init();
    reader_init();

    // Set default signal handlers, so we can ctrl-C out of this.
    signal_reset_handlers();

    // Set PWD from getcwd - fixes #5599
    env_stack_principal().set_pwd_from_getcwd();

    for (const auto &test : s_tests) {
        if (should_test_function(test.group)) {
            test.test();
        }
    }

    say(L"Encountered %d errors in low-level tests", err_count);
    if (s_test_run_count == 0) say(L"*** No Tests Were Actually Run! ***");

    // If under ASAN, reclaim some resources before exiting.
    asan_before_exit();

    if (err_count != 0) {
        return 1;
    }
}
