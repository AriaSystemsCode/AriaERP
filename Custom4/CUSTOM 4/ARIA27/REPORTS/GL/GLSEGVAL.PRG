*:************************************************************************
*:
*: Procedure file: GLSEGVAL.RPR
*:               : Segments' values
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:06/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                lfvSegVal
*:                lfClearRep
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : 
*B600344,1 Reham On 05/28/95 cash flow code, Ratio group code
*B600351,1 Reham On 05/29/95 In the detail form the heading is allow 
*B600351,1 Reham On 05/29/95 GL posting & the object is Seq. no.
*B600351,1 Reham On 05/29/95 It display inactive for all the active rec.
*B600351,1 Reham On 05/29/95 Fix the Ratio groups validation.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600358,1 Reham On 05/30/95 Add validation for both seg. value & acc. type code.

*B600344,4 Reham On 06/11/95 
*B600344,4 1- Fix the ratio group validation.
*B600344,4 2- Change the picture of the segment no. in the dictionary to be
*B600344,4    numeric & change the wait message to be more clear.
*E300789,7 WALID 03/05/99 Make all modifications needed due to
*E300789,7       removing ccomp_id field . 
*E301077,71 WALID 03/05/99 CLOSE ACCOD FILE .
*B802197,1  AKA 07/20/99 Order the segment value report. 
*** Report Setup
   
lcRpFiles  = "GLSEGVAL"  && Get slected files name

***   Get the field order ****
lcRpOrder  = "ORDER BY GLSEGVAL.csegvalue"

lnCount    = 0
lnTotal    = RECCOUNT('GLSEGVAL')
     
***   Create select  statment
DO CASE 
  CASE lcRpForm = 'GLSEGVAD'
    IF llOGFltCh .OR. glEscPrs

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
    glEscPrs = .F.

  CASE lcRpForm = 'GLSEGVMS'.OR. lcRpForm = 'GLSEGVNS'
    SELECT GLSEGVAL
    *B802197,1  AKA (Start)
    SET ORDER TO Acssegval 
    *B802197,1  AKA (End)
    
    DO gfDispRe WITH EVAL('lcRpForm'),;
                IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp     
ENDCASE 

*!************************************************************************
*!
*!      FUNCTION lfvSegVal
*!
*!************************************************************************
*
FUNCTION lfvSegVal

llCorrNo  = .F.

IF lcRpForm = 'GLSEGVAD'.AND. (VAL(ALLTRIM(laOGFxFlt[1,6])) > 1)
 laOGFxFlt[1,6] = '1'
 SHOW GET laOGFxFlt[1,6] 
 WAIT " The detail form is available only for the first segment " WINDOW NOWAIT
    FOR lnCount = 3 TO 8
       laOGObjCnt[lnOGVariable+lnCount] = .T.
    ENDFOR
    =lfActvateWin(lnWinDisp)
 RETURN  .T. 
ENDIF
IF lcRpForm = 'GLSEGVAD'
  SHOW GET OGSORT ENABLE
ELSE
  SHOW GET OGSORT DISABLE
ENDIF 

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED('SYCACCOD')
*  SELECT 0
*  USE (gcSysHome+'SYCACCOD') ORDER TAG COMPID
*ENDIF
*
*SELECT SYCACCOD
IF NOT USED('ACCOD')
  SELECT 0
  *E300789,7 WALID (WAM) [BEGIN] Open with no index
  *USE (gcDataDir + 'ACCOD') ORDER TAG COMPID
  USE (gcDataDir + 'ACCOD')
  *E300789,7 WALID (WAM) [END..]
ENDIF

SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 WALID (WAM) [BEGIN]
GO TOP
IF !EOF()
*IF SEEK(gcAct_Comp)
*E300789,7 WALID (WAM) [END..]
  lnAcsNoSeg = nAcsNoSeg
ELSE
  lnAcsNoSeg = 0
ENDIF

SELECT GLSEGVAL

IF !BETWEEN(VAL(ALLTRIM(laOGFxFlt[1,6])),1,lnAcsNoSeg)
  ** Out of range 
  *B600344,4 Change the picture of this object in the dictionary to be
  *B600344,4 numeric & change the wait message to be more clear.
  *WAIT "Out of range " WINDOW
  WAIT "Segment no. should be between 1 and "+ALLTRIM(STR(lnAcsNoSeg)) WINDOW
ELSE
  llCorrNo = .T.
  IF ALLTRIM(laOGFxFlt[1,6]) = '1'
    FOR lnCount = 3 TO 8
       laOGObjCnt[lnOGVariable+lnCount] = .T.
    ENDFOR
    =lfActvateWin(lnWinDisp)
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

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF
glEscPrs = .F.

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

*!************************************************************************
*!
*!      FUNCTION lfvRatGrp
*!
*!************************************************************************
*
FUNCTION lfvRatGrp

lcRpCurFld = EVALUATE(VARREAD())

SELECT GLRACOD
LOCATE FOR GLRACOD.csegratio = lcRpCurFld
IF !FOUND()
  *B600344,4 Fix the validation because it was accepting the 
  *B600344,4 type if it's entered
  DECLARE laRpRetFld(1)
  lcOldBrFld    = lcBrFields
  lcBrFields    = 'CSEGRATIO:H="Ratio group code",CRATDESC:H="Ratio group description"'
  laRpRetFld[1] = ''
  
  lcRpCurFld = VARREAD()
  IF !EMPTY(&lcRpCurFld)
    SELECT GLRACOD
    =gfBrows([],'CSEGRATIO',"laRpRetFld",'Ratio Group File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  lcBrFields = lcOldBrFld
ENDIF
SELECT GLSEGVAL

*!************************************************************************
*!
*!      FUNCTION : lfvSeg
*!
*!************************************************************************
*
FUNCTION lfvSeg

* Reham On 05/30/95

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'cacssegno:H="Seg. No.",csegvalue:H="Seg.Value",cseglndes:H="Segment Description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

SELECT GLSEGVAL

*MAB E3 Fix order bug [Begin
SET ORDER TO 1
*MAB E3 Fix order bug [End

IF "?" $ &lcRpCurFld .OR. !SEEK(laOgFxFlt[1,6]+&lcRpCurFld)
  =gfBrows('laOgFxFlt[1,6]','csegvalue',"laRpRetFld",'Segment File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF

lcBrFields = lcOldBrFld
*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*E301077,71 [BEGIN] Close file if it's not in OG open file array [Begin
IF (ASCAN(laSelFile,'ACCOD') = 0) AND USED('ACCOD')
  USE IN ACCOD
ENDIF
*E301077,71 [BEGIN]
