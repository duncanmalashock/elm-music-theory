module MusicTheory.Tuplet exposing
    ( Tuplet
    , tuplet
    )

import MusicTheory.Duration as Duration
import MusicTheory.Note as Note


type Tuplet
    = Tuplet Duration.Duration (List (Maybe Note.Note))


tuplet : Duration.Duration -> List (Maybe Note.Note) -> Tuplet
tuplet totalDuration notes =
    Tuplet totalDuration notes
