module NucleotideCount

let updateNucleotides (c: char) (nucleotides: Map<char, int>): Map<char, int> option =
    nucleotides.TryFind c |> Option.map (fun count -> nucleotides.Add(c, count + 1))

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    let nucleotides =
        [ ('A', 0)
          ('C', 0)
          ('G', 0)
          ('T', 0) ]
        |> Map.ofList
    let folder acc x = acc |> Option.bind (updateNucleotides x) 
    
    strand
    |> Seq.fold folder (Some nucleotides)
