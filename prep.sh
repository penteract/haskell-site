#!/bin/sh
set -e
stack build
stack exec Prep templates html
