#!/usr/bin/env nextflow
// A sample pipeline

params.input = 'data.txt'

include { HELLO as GREET } from './modules/hello.nf'

def plain_helper(x) {
    return x
}

String typed_helper(name) {
    return name
}

process SAY_HELLO {
    tag "$params.input"
    cpus 2

    input:
    val x

    output:
    stdout

    script:
    """
    echo Hello $x
    """
}

workflow {
    main:
    ch = Channel.of(1, 2, 3)
    SAY_HELLO(ch)
    result = SAY_HELLO.out
}
