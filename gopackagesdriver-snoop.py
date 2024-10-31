#!/usr/bin/python3

import errno
import json
import os
import pathlib
import shlex
import stat
import subprocess
import sys
import textwrap
import time


OUTDIR = pathlib.Path(os.getenv('HOME')) / "tmp" / "snoops"


def ensure_exists(p: pathlib.Path) -> None:
    try:
        p.mkdir()
    except OSError as err:
        if err.errno != errno.EEXIST:
            raise err


def bazel_cmd(args):
    cmd = ["/home/smerritt/bin/bazel"]

    flags = os.getenv('GOPACKAGESDRIVER_BAZEL_FLAGS')
    if flags:
        cmd += shlex.split(flags)

    cmd += ["run", "--", "@io_bazel_rules_go//go/tools/gopackagesdriver"]
    return cmd + args


def rerun_script(input_file: str, bazel_args) -> str:
    script = """
    #!/bin/sh
    cd "{wd}"
    export GOPACKAGESDRIVER_BAZEL_FLAGS={flags}

    {bazel} < {input_file}

    """.format(
        wd=os.getcwd(),
        flags=os.getenv('GOPACKAGESDRIVER_BAZEL_FLAGS', ''),
        bazel=" ".join(bazel_cmd(bazel_args)),
        input_file=input_file)
    return textwrap.dedent(script)


def main():
    # GOPROXY=off seems to goof things up when using --disk-cache
    os.environ.pop("GOPROXY", None)

    pid = os.getpid()
    now = time.time()
    unique_id = f"{now}-{pid}"

    ensure_exists(OUTDIR)

    captured_stdin_path = OUTDIR / f"snoop-{unique_id}.in"
    captured_stdout_path = OUTDIR / f"snoop-{unique_id}.out"
    captured_stderr_path = OUTDIR / f"snoop-{unique_id}.err"

    bazel_start = time.time()  # Ignore tees; their startup cost is miniscule
    stdin_tee = subprocess.Popen(
        ["tee", captured_stdin_path],
        stdin=sys.stdin,
        stdout=subprocess.PIPE,
    )

    # The typical helper is something like this:
    #
    # exec bazel run -- @io_bazel_rules_go//go/tools/gopackagesdriver "${@}"
    bazel = subprocess.Popen(
        bazel_cmd(sys.argv[1:]),
        stdin=stdin_tee.stdout,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    # Tee these so that we get some output even if bazel crashes (which it does sometimes)
    stdout_tee = subprocess.Popen(
        ["tee", captured_stdout_path],
        stdin=bazel.stdout,
        stdout=sys.stdout,
    )
    stderr_tee = subprocess.Popen(
        ["tee", captured_stderr_path],
        stdin=bazel.stderr,
        stdout=sys.stderr,
    )

    stdin_tee_rc = stdin_tee.wait()
    bazel_rc = bazel.wait()
    stdout_tee_rc = stdout_tee.wait()
    stderr_tee_rc = stderr_tee.wait()
    bazel_end = time.time()

    to_dump = {
        "env": {k: v for k, v in os.environ.items()},     # json.dump needs a dict, not an ItemsView
        "args": sys.argv[1:],
        "bazel_runtime": bazel_end - bazel_start,
        "stdin_tee_rc": stdin_tee_rc,
        "bazel_rc": bazel_rc,
        "stdout_tee_rc": stdout_tee_rc,
        "stderr_tee_rc": stderr_tee_rc,
    }

    with open(OUTDIR / f"snoop-{unique_id}.info", "w") as run_info:
        json.dump(to_dump, run_info, indent=2, sort_keys=True)

    with open(OUTDIR / f"snoop-{unique_id}.rerun.sh", "w") as rerun:
        rerun.write(rerun_script(str(OUTDIR / f"snoop-{unique_id}.in"), sys.argv[1:]))

        # Make it executable
        st = os.fstat(rerun.fileno())
        os.fchmod(rerun.fileno(), st.st_mode | stat.S_IXUSR)


if __name__ == "__main__":
    main()
