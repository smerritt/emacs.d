#!/usr/bin/python3

import errno
import json
import os
import pathlib
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
    return ["/home/smerritt/bin/bazel", "run", "--", "@io_bazel_rules_go//go/tools/gopackagesdriver"] + args


def rerun_script(input_file: str, bazel_args) -> str:
    script = """
    #!/bin/sh
    cd "{wd}"
    {bazel} < {input_file}

    """.format(
        wd=os.getcwd(),
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

    captured_stdin = open(OUTDIR / f"snoop-{unique_id}.in", "wb")
    captured_stdout = open(OUTDIR / f"snoop-{unique_id}.out", "wb")
    captured_stderr = open(OUTDIR / f"snoop-{unique_id}.err", "wb")

    # The typical helper is something like this:
    #
    # exec bazel run -- @io_bazel_rules_go//go/tools/gopackagesdriver "${@}"
    bazel = subprocess.Popen(
        bazel_cmd(sys.argv[1:]),
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    # We get exactly one JSON object on stdin. That's just how a
    # gopackagesdriver script works.
    driver_request = json.load(sys.stdin)
    driver_request_serialized = json.dumps(driver_request).encode()
    captured_stdin.write(driver_request_serialized)

    bazel_start = time.time()
    out, err = bazel.communicate(driver_request_serialized)
    bazel_end = time.time()

    captured_stdout.write(out)
    captured_stderr.write(err)

    sys.stdout.buffer.write(out)
    sys.stderr.buffer.write(err)

    to_dump = {
        "env": {k: v for k, v in os.environ.items()},     # json.dump needs a dict, not an ItemsView
        "args": sys.argv[1:],
        "bazel_runtime": bazel_end - bazel_start,
        "output_size": len(out),
        "outerr_size": len(err),
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
