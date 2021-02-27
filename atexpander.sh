#!/bin/zsh
# Expand library, implementation and test files

< wiki/wiki-head.svnwiki > srfi-160.svnwiki

< tests/base-tests.scm > tests/run.scm

for at bytes in u8 1 s8 1 u16 2 s16 2 u32 4 s32 4 u64 8 s64 8 f32 4 f64 8 c64 8 c128 16; do
    from="from"
    to="to"
    body=""
    if [ "$at" = "c64" ] || [ "$at" = "c128" ]; then
        body="${at}vector-body"
        from="($body from)"
        to="($body to)"
    fi
    sed "s/@/$at/g; s/&bytes/$bytes/g; s/&from/$from/g; s/&to/$to/g" at-impl.scm > at/$at-impl.scm
    sed "s/@/$at/g;" at-vector2list.scm > at/$at-vector2list.scm
    sed "s/@/$at/g; s/&body/$body/g" srfi.160.at.scm > at/srfi.160.$at.scm

    sed "s/@/$at/g;" wiki/at-module.svnwiki >> srfi-160.svnwiki
    sed "s/@/$at/g;" tests/at-tests.scm >> tests/run.scm
done

< wiki/wiki-tail.svnwiki >> srfi-160.svnwiki
echo "(test-end)" >> tests/run.scm
echo "(test-exit)" >> tests/run.scm
