* This fix was applied at IKEEDI
SELECT TEMPADJ
SCATTER MEMVAR 
DELETE ALL
select STYDYE
LOCATE
ON ERROR 
_SCREEN.VISIBLE = .T.
DEBUG
SUSP
SCAN 
  =SEEK(STYDYE.STYLE,'STYLE')
*  IF STK1<>0 OR STK2<>0 OR STK3<>0 OR STK4<>0 OR STK5<>0 OR STK6<>0 OR STK7<>0 OR STK8<>0 or STYLE.nStkVal<>0
  
    m.STYLE = STYDYE.STYLE
  
    m.UNT_COST  = STYDYE.AVE_COST
    m.OLD_COST =  STYDYE.AVE_COST
    m.Adj1 = -stydye.stk1
    m.Adj2 = -stydye.stk2
    m.Adj3 = -stydye.stk3
    m.Adj4 = -stydye.stk4
    m.Adj5 = -stydye.stk5
    m.Adj6 = -stydye.stk6
    m.Adj7 = -stydye.stk7
    m.Adj8 = -stydye.stk8
    m.totadj = m.adj1+m.adj2+m.adj3+m.adj4+m.adj5+m.adj6+m.adj7+m.adj8
    m.CFROMWARE = STYDYE.CWARECODE 
    
    INSERT INTO TEMPADJ FROM MEMVAR
  
    m.UNT_COST  = STYLE.TOTCOST
    m.OLD_COST =  STYLE.TOTCOST
    m.Adj1 = stydye.stk1
    m.Adj2 = stydye.stk2
    m.Adj3 = stydye.stk3
    m.Adj4 = stydye.stk4
    m.Adj5 = stydye.stk5
    m.Adj6 = stydye.stk6
    m.Adj7 = stydye.stk7
    m.Adj8 = stydye.stk8
    m.totadj = m.adj1+m.adj2+m.adj3+m.adj4+m.adj5+m.adj6+m.adj7+m.adj8
    m.CFROMWARE = STYDYE.CWARECODE   
  
    INSERT INTO TEMPADJ FROM MEMVAR
    
*  ENDIF
  
ENDSCAN

RETURN



****************************************************************************************************
*T20110104.0028 TMI 04/06/2011 [Start] 
*Aria4xp 
*TMI - Tarek Mohammed Ibrahim

*!*	T20110104.0028 - GP report does not reflect cost sheet info 
*!*	Description: 
*!*	Write a fix program that will be call from Aria and will loop the style file 
*!*	and for each style loops all records in STYDYE file
*!*	For each record in STYDYE file the program should do physical inventory transaction 
*!*	that will issue the current stock in STYDYE using the current average cost from STYDYE 

*!*	then will receive the current stock using the style standard cost.

*!*	Then the program should update the average cost in STYLE and STYDYE files by the value of the style standard cost.

*!*	Adding the physical inventory transaction should be done by calling the GFSTYCRL function
****************************************************************************************************


*RUN THIS PART AFTER THE ABOVE ONE

USE (oAriaApplication.DataDir+'STYLE') IN 0 
USE (oAriaApplication.DataDir+'STYDYE') IN 0 ORDER STYDYE             

SELECT STYLE
SCAN
  =SEEK(STYLE.STYLE,'STYDYE')
  SELECT STYDYE
  SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE
    REPLACE AVE_COST  WITH STYLE.TOTCOST      && update the average cost in STYDYE files by the value of the style standard cost.
  ENDSCAN  
  SELECT STYLE
  REPLACE AVE_COST  WITH TOTCOST   
ENDSCAN
