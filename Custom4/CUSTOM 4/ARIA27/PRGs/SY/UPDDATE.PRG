*:************************************************************************
*: Program file  : UPDDATE.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Yasser Saad Ibrahim
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
ldNewdate=gdSysDate

DO UPDDATE.SPR

*!***************************************************************************
*!
*!         FUNCTION :lfvNewDate
*!
*!***************************************************************************
*
FUNCTION lfvNewDate
lcDate=DTOC(ldNewDate) 
RETURN !EMPTY(SUBSTR(lcDate,LEN(lcDate)-2,2))

*!***************************************************************************
*!
*!         FUNCTION :lfvNewDate
*!
*!***************************************************************************
*
FUNCTION lfvState
PARAMETERS lnStatus

 DO CASE
   CASE lnStatus=1
     lcdate=DTOC(ldNewDate)
*     RUN DATE &lcDate
     gdSysDate=ldNewDate
   CASE lnStatus=2
     RETURN
 ENDCASE  
