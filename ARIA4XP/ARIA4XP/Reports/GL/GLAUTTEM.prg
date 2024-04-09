*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLAUTTEM.PRG
*:  Module      : General Ledger
*:  Desc.       : Template entries
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303246,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************

*** Report Setup
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables


lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

***   Get the field order ****
lcRpOrder  = "ORDER BY GLAUTHD.cautcode"

lnOldAlias = SELECT()     

IF llOGFltCh 
       
  *** Save escape setting
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')
  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
      
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT
  
  ***   Create select  statment
  
  
  SELECT  &lcRpFields;
   FROM  &lcRpFiles ;
   WHERE  &lcRpExp  .AND. lfWaitMsg();
   &lcRpOrder.;
   INTO CURSOR (lcRpTargt)


 *** Restore the old setting  
  WAIT CLEAR
  SET TALK OFF
  SET ESCAPE &lcSaveEsc 
  ON  ESCAPE &lcSaveOnEs 
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  SELECT (lcRpTargt)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
SELECT (lnOldAlias)  


*!************************************************************************
*!
*!      FUNCTION : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTTED",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTTED"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLAUTTE2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm =   "GLAUTTES"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLAUTTEM')  
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
ENDCASE  
ClearRead()

*!************************************************************************
*!
*!      FUNCTION : lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

DECLARE laRpRetFld(1)
lcBrFields    = 'CAutCode:H="Code",CAutDes:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
*-  Get user position in the laOg??Flt
LOCAL i,lnPos
FOR i=1 TO ALEN(&lcRpCurFld,1)
  IF '.CAUTCODE' $ UPPER(PADR(&lcRpCurFld.[i,1],100))
    lnPos = i
    EXIT 
  ENDIF 
ENDFOR
lcRpCurFld = lcRpCurFld + '['+ALLTRIM(STR(lnPos))+',6]'

SELECT GLAUTHD
SET ORDER TO TAG typecode

IF ('?' $ &lcRpCurFld. .OR. !SEEK('T'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["T"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF 
  &lcRpCurFld = laRpRetFld[1]
ENDIF
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.