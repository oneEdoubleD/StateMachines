data State 
    = S0 
    | S1 
    | S2 
    | FinalState
    deriving (Eq,Show)

data Transition 
    = T1 
    | T2 
    | T3 
    | T4
    deriving (Eq,Show)

transition :: Maybe State -> Transition -> Maybe State
transition (Just S0) T1 = Just S1
transition (Just S1) T2 = Just S2
transition (Just S2) T3 = Just S1
transition (Just S1) T4 = Just FinalState
transition s _          = Nothing

project :: [Transition] -> Maybe State
project = foldl transition (Just S0)
