data State
    = Listen
    | Syn_Recd
    | Syn_Sent
    | Ack_Wait
    | Established
    | Fin_Wait_1
    | Fin_Wait_2
    | Closing
    | Closing_1
    | Closing_2
    | Close_Wait
    | Close_Wait_1
    | Last_Ack
    | Last_Ack_1
    | Last_Ack_2
    | Close
    deriving (Eq,Show)

data Transition
    = T1  -- SYN
    | T2  -- SYN
    | T3  -- ACK of SYN, SYN
    | T4  -- SYN
    | T5  -- ACK of SYN, SYN
    | T6  -- ACK
    | T7  -- ACK of SYN, FIN
    | T8  -- RST
    | T9  -- REPEAT ESTABLISHED
    | T10 -- FIN
    | T11 -- FIN
    | T12 -- FIN
    | T13 -- ACK of FIN
    | T14 -- FIN
    | T15 -- ACK of FIN
    | T16 -- ACK of FIN, FIN
    | T17 -- ACK
    | T18 -- ACK of FIN
    | T19 -- ACK
    | T20 -- RST or SYN
    | T21 -- FIN 
    | T22 -- ACK of FIN
    | T23 -- FIN
    | T24 -- ACK of FIN
    | T25 -- ACK of FIN, FIN
    | T26 -- ACK
    | T27 -- ACK of FIN
    | T28 -- ACK
    | T29 -- FIN
    | T30 -- FIN
    | T31 -- ACK of SYN, SYN, FIN
    | T32 -- REPEAT SYN_RECD
    | T33 -- REPEAT SYN_SENT
    | T34 -- REPEAT ACK_WAIT
    | T35 -- REPEAT FIN_WAIT_1
    | T36 -- REPEAT CLOSING
    | T37 -- REPEAT FIN_WAIT_2
    | T38 -- REPEAT CLOSING_2
    | T39 -- 
    | T40 -- REPEAT LAST_ACK_2
    | T41 -- 
    | T42 -- REPEAT LAST_ACK

transition :: Maybe State -> Transition -> Maybe State
transition (Just Listen) T1        = Just Syn_Recd
transition (Just Listen) T2        = Just Syn_Sent
transition (Just Syn_Recd) T3      = Just Ack_Wait
transition (Just Syn_Sent) T4      = Just Syn_Recd
transition (Just Syn_Sent) T5      = Just Established
transition (Just Ack_Wait) T6      = Just Established
transition (Just Ack_Wait) T7      = Just Close_Wait_1
transition (Just Syn_Recd) T8      = Just Listen
transition (Just Established) T9   = Just Established
transition (Just Established) T10  = Just Close_Wait_1
transition (Just Established) T11  = Just Fin_Wait_1
transition (Just Fin_Wait_1) T12   = Just Closing_1
transition (Just Fin_Wait_1) T13   = Just Fin_Wait_2
transition (Just Fin_Wait_2) T14   = Just Closing_2
transition (Just Closing_1) T15    = Just Closing_2
transition (Just Fin_Wait_1) T16   = Just Closing_2
transition (Just Closing_1) T17    = Just Closing
transition (Just Closing) T18      = Just Close
transition (Just Closing_2) T19    = Just Close
transition (Just Established) T20  = Just Close
transition (Just Close_Wait_1) T21 = Just Last_Ack_1
transition (Just Close_Wait_1) T22 = Just Close_Wait
transition (Just Close_Wait) T23   = Just Last_Ack
transition (Just Last_Ack_1) T24   = Just Last_Ack
transition (Just Close_Wait_1) T25 = Just Last_Ack
transition (Just Last_Ack_1) T26   = Just Last_Ack_2
transition (Just Last_Ack_2) T27   = Just Close
transition (Just Last_Ack) T28     = Just Close
transition (Just Syn_Recd) T29     = Just Fin_Wait_1
transition (Just Ack_Wait) T30     = Just Fin_Wait_1
transition (Just Syn_Sent) T31     = Just Close_Wait_1
transition (Just Syn_Recd) T32     = Just Syn_Recd
transition (Just Syn_Sent) T33     = Just Syn_Sent
transition (Just Ack_Wait) T34     = Just Ack_Wait
transition (Just Fin_Wait_1) T35   = Just Fin_Wait_1
transition (Just Closing) T36      = Just Closing
transition (Just Fin_Wait_2) T37   = Just Fin_Wait_2
transition (Just Closing_2) T38    = Just Closing_2
transition (Just Last_Ack_2) T40   = Just Last_Ack_2
transition (Just Last_Ack) T42     = Just Last_Ack
transition s _                     = Nothing

project :: [Transition] -> Maybe State
project = foldl transition (Just Listen)
