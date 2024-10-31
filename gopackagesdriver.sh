#!/bin/bash
#
# From https://github.com/bazelbuild/rules_go/wiki/Editor-setup#3-editor-setup
#
# This is required to get gopls to play nicely with Bazel.

exec bazel $GOPACKAGESDRIVER_BAZEL_FLAGS run -- @io_bazel_rules_go//go/tools/gopackagesdriver "${@}"
