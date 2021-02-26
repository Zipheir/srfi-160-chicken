#!/bin/zsh
# Expand library, implementation and test files
for at bytes in u8 1 s8 1 u16 2 s16 2 u32 4 s32 4 u64 8 s64 8 f32 4 f64 8 c64 8 c128 16; do
    from="from"
    to="to"
    if [ "$at" = "c64" ] || [ "$at" = "c128" ]; then
        from="(##sys#slot from 1)"
        to="(##sys#slot to 1)"
    fi
    sed "s/@/$at/g; s/&bytes/$bytes/g; s/&from/$from/g; s/&to/$to/g" at-impl.scm > at/$at-impl.scm
    sed "s/@/$at/g;" at-vector2list.scm > at/$at-vector2list.scm
    sed "s/@/$at/g;" srfi.160.at.scm > at/srfi.160.$at.scm
done

