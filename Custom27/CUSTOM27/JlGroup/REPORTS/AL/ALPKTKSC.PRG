*:***********************************************************************************************
*:  Program Name : ALPKTKSC.PRG
*:         System: ARIA 2.7
*:         Module: ALLOCATION (AL).
*:  Program desc.: Picking Ticket Form For Scop Creek
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 13/05/2001
*:***********************************************************************************************
*: For Custom# C102286
*:***********************************************************************************************


IF !llRpOrdLNt
  SELECT (lcTmpOrdL)
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF