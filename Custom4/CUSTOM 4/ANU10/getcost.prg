*T20140219.0024 - Can't save materials PO
* a fix program to apply the comment added to the ticket T20140219.0024 by Mariam

*!*	get the data in this cursor using the commands
*!*	ln = SQLSTRINGCONNECT('driver=sql server;server=ANUE-SERVERDC;database=ANU10_Ldb01;uid=aria4xp;pwd=aria4@xp')
*!*	?SQLEXEC(ln,"select * from itemjrnl where style like '79I8D       -BLK%'","_79I8D")
*!*	then issue the command 
*!*	    replace cowner WITH STR(ncost,16,3) all


LOCATE 
lnStkVal = 0
lnStk = 0
lnAveCost = 0

*- restore the cost from the cowner field that I saved in before applying this program
replace ncost WITH VAL(cowner) all
LOCATE 
*- This loop to get the ave_cost for dates previous to {^2012-06-15} and update for dates after this
SCAN 

  IF dtrdate >= {^2012-06-15} and ( cirtype = 'I' OR (cirtype = 'R' AND ABS(ncost)>10 ))
    replace ncost WITH lnAveCost
    replace nstkval WITH ncost*ntotstk
  ENDIF 

  lnStkVal = lnStkVal + nCost*nTotStk
  lnStk = lnStk + nTotStk
  lnAveCost = ABS(IIF(lnStk>0,lnStkVal/lnStk,lnAveCost ))
  WAIT WINDOW NOWAIT STR(lnAveCost,10,2)
 
  
ENDSCAN

MESSAGEBOX(lnAveCost)

