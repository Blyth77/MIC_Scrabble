// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty : MultiSet<'a>
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    val subtract: MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val ofList: 'a list -> MultiSet<'a>
    val toList: MultiSet<'a> -> 'a list
    val size  : MultiSet<'a> -> uint32
    val toMap : MultiSet<'a> -> Map<'a, uint32>