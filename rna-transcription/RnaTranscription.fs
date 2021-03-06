﻿module RnaTranscription

let private nucleotideComplement = function
    | 'C' -> 'G'
    | 'A' -> 'U'
    | 'T' -> 'A'
    | 'U' -> 'A'
    | 'G' -> 'C'
    | _ -> failwith "Unknown nucleotide"

let toRna (dna: string): string =
    String.map nucleotideComplement dna