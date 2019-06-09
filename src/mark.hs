

data MarkExp = Unmarked | Mark MarkExp deriving (Show)

unmarked = Unmarked
marked = Mark Unmarked
