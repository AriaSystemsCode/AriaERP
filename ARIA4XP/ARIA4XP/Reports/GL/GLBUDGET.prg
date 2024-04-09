*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLBUDGET.PRG
*:  Module      : General Ledger
*:  Desc.       : Budget version
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303236,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

DO CASE
  CASE lcRpForm = "GLBUDGTS"    && if use the summary report
    SELECT GLBUDHD
    GO TOP
    *E303236,1 TMI 10/08/2012 [Start] 
    *DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 
    =lfCollect()
    SELECT(lcTmpFile)
    DO gfDispRe WITH EVAL('lcRpForm')
    *E303236,1 TMI 10/08/2012 [End  ] 
     
  CASE lcRpForm = "GLBUDGTD"    && If use the detail report
    *** Point to the chart of account file to see the account discription
    *** in a fast way without using select SQL or LOOKUP() funcion in the
    *** report form
    SET ORDER TO TAG ACCTCODE IN GLACCHAR

    lcRpFiles  = "GLBUDDT,GLBUDHD"  && Get slected files name

  ***   Create select  statment
    IF llOGFltCh
      lcRpExp = lcRpExp + ' '
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
      
      lcRpFields = "GlBudHd.cBudCode, GlBudHd.cBudYear, GlBudHd.cBudDes, GlBudHd.cBudComnt,"+;
                   "GlBudDt.cAcctCode, GlBudDt.cBudPrd, GlBudDt.nAmount"
      
      *** Create select  statment and
      *** Select data from file(s)
      SELECT &lcRpFields.;
        FROM  &lcRpFiles. ;
        WHERE  &lcRpExp. .AND. lfWaitMsg();
        INTO CURSOR  (lcRpTargt);
        &lcRpOrder
          
      *** Restore all enviroment 
      WAIT CLEAR
      SET TALK OFF
      ON ESCAPE  &lcSaveOnEs
      SET ESCAPE &lcSaveEsc
        
      IF _TALLY > 0         
        IF EMPTY(RELATION(1))
          *** Make a relation between temporary file and glacchar 
          SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE
          *** Display the report to screen , file or printer
        ENDIF  
      ELSE
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
        RETURN
      ENDIF
    ELSE
      SELECT (lcRpTargt)
    ENDIF        
    DO gfDispRe WITH EVAL('lcRpForm')     
ENDCASE

*!************************************************************************
*!
*!      Function : lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLBUDGTD",.F.,.T.)
DO CASE
  CASE lcRpForm="GLBUDGTD"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLBUDGT2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm="GLBUDGTS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLBUDGET')  
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
ENDCASE  
ClearRead()

*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      FUNCTION : lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

IF NOT USED("ACCOD") 
  SELECT 0
    gcDataDir = oAriaApplication.DataDir
  USE &gcDataDir.ACCOD
  llUesdBefo = .T.
ENDIF

SELECT ACCOD

GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
IF llUesdBefo       && .F.- this file used by the system
  
  USE IN ACCOD
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.