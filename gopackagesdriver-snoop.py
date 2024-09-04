#!/usr/bin/python3

import json
import os
import subprocess
import sys

def main():
    pid = os.getpid()

    captured_stdin = open(f"/tmp/snoop-{pid}.in", "wb")
    captured_stdout = open(f"/tmp/snoop-{pid}.out", "wb")
    captured_stderr = open(f"/tmp/snoop-{pid}.err", "wb")

    # The typical helper is something like this:
    #
    # exec bazel run -- @io_bazel_rules_go//go/tools/gopackagesdriver "${@}"
    bazel = subprocess.Popen(
        ["bazel", "run", "--", "@io_bazel_rules_go//go/tools/gopackagesdriver"] + sys.argv[1:],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    # We know we get precisely one JSON object on stdin.
    driver_request = json.load(sys.stdin)
    driver_request_serialized = json.dumps(driver_request).encode()
    captured_stdin.write(driver_request_serialized)

    out, err = bazel.communicate(driver_request_serialized)

    captured_stdout.write(out)
    captured_stderr.write(err)

    sys.stdout.buffer.write(out)
    sys.stderr.buffer.write(err)


if __name__ == "__main__":
    main()
