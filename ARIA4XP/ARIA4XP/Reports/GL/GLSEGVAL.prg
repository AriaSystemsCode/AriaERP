*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLSEGVAL.PRG
*:  Module      : General Ledger
*:  Desc.       : Segments' values
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303243,1
*:************************************************************************
*:************************************************************************

*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables


   
lcRpFiles  = "GLSEGVAL"  && Get slected files name

***   Get the field order ****
lcRpOrder  = "ORDER BY GLSEGVAL.csegvalue"

lnCount    = 0
lnTotal    = RECCOUNT('GLSEGVAL')
     
***   Create select  statment
DO CASE 
  CASE lcRpForm = 'GLSEGVAD'
    IF llOGFltCh 

      *** Save escape setting
      lcSaveEsc = SET('ESCAPE')
      lcSaveOnEs = ON('ESCAPE')
      SET ESCAPE ON
      ON ESCAPE DO gpSQLBrak
         
      *** Intialize the varliable that count rows selected
      _TALLY = 0
       
      *** Activate the system select therom.
      SET TALK ON

      IF laOGFxFlt[1,6] = '1'
        lcRpAddExp = " AND GLSEGVAL.ctypecode = GLTYPES.ctypecode"
        lcRpAddFie = ',GLTYPES.ctyplndes'
        lcRpFiles  = lcRpFiles + ",GLTYPES"
      ELSE
        lcRpAddExp = ''  
        lcRpAddFie = ''
      ENDIF
      lcRpFields = lcRpFields + lcRpAddFie
      lcRpExp    = lcRpExp + lcRpAddExp
      
      *** Select data from file(s)
      SELECT    &lcRpFields.;
        FROM  &lcRpFiles. ;
        WHERE  &lcRpExp .AND. lfWaitMsg();
        &lcRpOrder.;
        INTO CURSOR (lcRpTargt)
        
      *** Restore the old setting  
      SET TALK OFF
      SET ESCAPE &lcSaveEsc 
      ON  ESCAPE &lcSaveOnEs 
      
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

  CASE lcRpForm = 'GLSEGVMS'.OR. lcRpForm = 'GLSEGVNS'
    SELECT GLSEGVAL
    SET ORDER TO Acssegval 
    
    *E303243,1 TMI 10/08/2012 [Start] 
    *DO gfDispRe WITH EVAL('lcRpForm'),;
                IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp     
    lfCollect()
    SELECT (lcTmpFile)           
    DO gfDispRe WITH EVAL('lcRpForm')
    *E303243,1 TMI 10/08/2012 [End  ]             
ENDCASE 

*!************************************************************************
*!
*!      FUNCTION lfvSegVal
*!
*!************************************************************************
*
FUNCTION lfvSegVal

llCorrNo  = .F.
lnOGVariable = 2

IF lcRpForm = 'GLSEGVAD'.AND. (VAL(ALLTRIM(laOGFxFlt[1,6])) > 1)
  laOGFxFlt[1,6] = '1'
  WAIT " The detail form is available only for the first segment " WINDOW NOWAIT
  FOR lnCount = 3 TO 8
    laOGObjCnt[lnOGVariable+lnCount] = .T.
  ENDFOR

  loOgScroll.Refresh()
  RETURN  .T. 
ENDIF
IF lcRpForm = 'GLSEGVAD'
  SHOW GET OGSORT ENABLE
ELSE
  SHOW GET OGSORT DISABLE
ENDIF 

IF NOT USED('ACCOD')
  SELECT 0
    gcDataDir = oAriaApplication.DataDir
  USE (gcDataDir + 'ACCOD')
ENDIF

SELECT ACCOD

GO TOP
IF !EOF()
  lnAcsNoSeg = nAcsNoSeg
ELSE
  lnAcsNoSeg = 0
ENDIF

SELECT GLSEGVAL
*E303243,1 TMI 10/10/2012 [Start] add this just to make A4 function same as A27
llCorrNo = .T.
*E303243,1 TMI 10/10/2012 [End  ] 
IF !BETWEEN(VAL(ALLTRIM(laOGFxFlt[1,6])),1,lnAcsNoSeg)
  ** Out of range 
  WAIT "Segment no. should be between 1 and "+ALLTRIM(STR(lnAcsNoSeg)) WINDOW nowait
ELSE
  llCorrNo = .T.
  IF ALLTRIM(laOGFxFlt[1,6]) = '1'
    FOR lnCount = 3 TO 8
       laOGObjCnt[lnOGVariable+lnCount] = .T.
    ENDFOR
    *=lfActvateWin(lnWinDisp)
    IF lcRpForm = 'GLSEGVNS'
      lcRpForm  = 'GLSEGVMS'  
    ENDIF
  ELSE

    FOR lnCount = 3 TO 8
       laOGObjCnt[lnOGVariable+lnCount] = .F.
       laOGFxFlt[lnCount,6] = ''
       =lfOGShowGet('laOGFxFlt['+ALLTRIM(STR(lnCount))+',6]')
    ENDFOR
*    =lfActvateWin(lnWinDisp)
    IF lcRpForm = 'GLSEGVMS'
      lcRpForm  = 'GLSEGVNS'  
    ENDIF
  ENDIF
ENDIF
RETURN llCorrNo 

*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLSEGVAD",.F.,.T.)
lcOGFltr=''
IF lcRpForm = 'GLSEGVAD'
  SHOW GET OGSORT ENABLE
ELSE
  SHOW GET OGSORT DISABLE
ENDIF 

loOgScroll.llOGFltCh = .T.
ClearRead()

*!************************************************************************
*!
*!      FUNCTION : lfvSeg
*!
*!************************************************************************
*
FUNCTION lfvSeg

lcBrFields    = 'cacssegno:H="Seg. No.",csegvalue:H="Seg.Value",cseglndes:H="Segment Description"'
lfGetBrow('CSEGVALUE','GLSEGVAL','ACSSEGVAL','Account Code Segment Value')


*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED('ACCOD')
  USE IN ACCOD
ENDIF

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.