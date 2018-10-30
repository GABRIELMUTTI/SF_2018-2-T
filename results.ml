open Syntax

type result = Vnum of int
            | Vbool of bool                               
            | Vpair of result * result
            | Vnil
            | Vcons of result * result 
            | Vclos of variable * expr * env
            | Vrclos of variable * variable * expr * env
            | RRaise
and  
env = (variable * result) list
