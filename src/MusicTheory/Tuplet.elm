module MusicTheory.Tuplet exposing
    ( Tuplet
    , tuplet
    )

import MusicTheory.Note as Note
import MusicTheory.Time as Time


type Tuplet
    = Tuplet Time.Time (List (Maybe Note.Note))


tuplet : Time.Time -> List (Maybe Note.Note) -> Tuplet
tuplet totalDuration notes =
    Tuplet totalDuration notes
