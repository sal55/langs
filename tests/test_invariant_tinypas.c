#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

/* Test that tinypas does not crash/overflow when given oversized identifiers.
   We invoke the real tinypas compiler via a child process with crafted source
   files and assert it exits without a memory-corruption signal (SIGSEGV/SIGABRT). */

static int run_tinypas_with_source(const char *src) {
    char tmpfile[] = "/tmp/tinypas_test_XXXXXX.pas";
    int fd = mkstemps(tmpfile, 4);
    if (fd < 0) return -1;
    write(fd, src, strlen(src));
    close(fd);

    pid_t pid = fork();
    if (pid == 0) {
        /* Redirect stdout/stderr to /dev/null */
        freopen("/dev/null", "w", stdout);
        freopen("/dev/null", "w", stderr);
        execlp("./tinypas", "./tinypas", tmpfile, NULL);
        _exit(127);
    }
    int status = 0;
    waitpid(pid, &status, 0);
    unlink(tmpfile);

    if (WIFSIGNALED(status)) {
        int sig = WTERMSIG(status);
        /* SIGSEGV=11, SIGABRT=6 indicate memory corruption */
        return sig;
    }
    return 0; /* normal exit or non-signal termination */
}

START_TEST(test_oversized_identifier_no_overflow)
{
    /* Invariant: feeding oversized identifiers must never cause a memory-
       corruption signal (SIGSEGV/SIGABRT) from the tinypas compiler. */

    /* 3 payloads: valid short id, boundary ~32 chars, 10x oversized ~320 chars */
    char long_id_32[33];   memset(long_id_32,  'A', 32);  long_id_32[32]  = '\0';
    char long_id_320[321]; memset(long_id_320, 'B', 320); long_id_320[320]= '\0';

    char src_valid[64];
    char src_boundary[128];
    char src_exploit[512];

    snprintf(src_valid,    sizeof(src_valid),    "program p; var x:integer; begin end.\n");
    snprintf(src_boundary, sizeof(src_boundary), "program p; var %s:integer; begin end.\n", long_id_32);
    snprintf(src_exploit,  sizeof(src_exploit),  "program p; var %s:integer; begin end.\n", long_id_320);

    const char *payloads[] = { src_valid, src_boundary, src_exploit };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        int sig = run_tinypas_with_source(payloads[i]);
        ck_assert_msg(sig != 11 && sig != 6,
            "tinypas crashed with signal %d on payload %d — buffer overflow detected", sig, i);
    }
}
END_TEST

Suite *security_suite(void) {
    Suite *s = suite_create("Security");
    TCase *tc = tcase_create("Core");
    tcase_add_test(tc, test_oversized_identifier_no_overflow);
    suite_add_tcase(s, tc);
    return s;
}

int main(void) {
    Suite *s = security_suite();
    SRunner *sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    int failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}