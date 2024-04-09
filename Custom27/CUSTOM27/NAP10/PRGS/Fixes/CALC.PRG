PARAMETER lcPO , lcAlias,lceg
store 0 to LN1, LN2,LN3,LN4
IF lcAlias = "27"
SELECT POSLN
IF lceg="E"

sum neactcost1*totqty to ln1 for po = lcPO and trancd$' 2'
sum neactcost2*totqty to ln2 for po = lcPO and trancd$' 2'
sum neactcost3*totqty to ln3  for po = lcPO and trancd$' 2'
sum neactcost4*totqty to ln4 for po = lcPO and trancd$' 2'

ELSE
sum nact_Cst1*totqty to ln1 for po = lcPO and trancd$' 2'
sum nact_Cst2*totqty to ln2 for po = lcPO and trancd$' 2'
sum nact_Cst3*totqty to ln3  for po = lcPO and trancd$' 2'
sum nact_Cst4*totqty to ln4 for po = lcPO and trancd$' 2'
ENDIF
ELSE
SELECT POSLN26
sum nact_Cost1*totqty to ln1 for po = lcPO and trancd$' 2'
sum nact_Cost2*totqty to ln2 for po = lcPO and trancd$' 2'
sum nact_Cost3*totqty to ln3  for po = lcPO and trancd$' 2'
sum nact_Cost4*totqty to ln4 for po = lcPO and trancd$' 2'


ENDIF