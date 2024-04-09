*:************************************************************************
*:
*: Procedure file: GLUNTRAN.RPR
*:               : Unposted transactions  
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:25/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.J.
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600361,1 Reham On 05/31/95 Validate the period if the year is empty
*B600361,1 to prevent make it accept anything.
*E300789,7 WAM WALID 03/04/99 Make all needed modification due to the 
*          removing of ccomp_id field .
*E300789,6 MAB 03/06/99 Make all needed modification due to the 
*          removing of ccomp_id field .
*B602244,1 KAM 05/09/2000 Increase the width of the debit and credit fields
*B602244,1                in the GLUNTRAD.FRX,also we justify the address
*B602244,1                of debit and credit in the same field    
****************************************************************************
*** Report Setup
SELECT GLTRNSHD
DO CASE
  CASE lcRpGroup == 'cTrnStat'
    SET ORDER TO TAG TRANSTAT
  OTHERWISE
    SET ORDER TO TAG BATCHTRN
ENDCASE
IF lcRpForm = "GLUNTRAD"
  SELECT GLTRNSHD
  SET ORDER TO TAG BATCHTRN IN GLTRNSDT
  SET ORDER TO TAG ACCTCODE IN GLACCHAR
  SET RELATION TO gltrnshd.cbatchno + gltrnshd.ctranno INTO GLTRNSDT ADDITIVE
  SELECT GLTRNSDT
  SET RELATION TO gltrnsdt.cacctcode INTO GLACCHAR ADDITIVE
  SELECT GLTRNSHD
  SET SKIP TO GLTRNSDT
ENDIF
DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcRpExp),'','FOR ')+lcRpExp
SET RELATION TO
SET SKIP TO
IF lcRpForm = "GLUNTRAD"
  SELECT GLTRNSHD
  SET RELATION TO
  SELECT GLTRNSDT
  SET RELATION TO  
ENDIF
RETURN

DO CASE
  CASE lcRpForm = "GLUNTRAS"
    lcRpFiles   = "GLTRNSHD"  && Get slected files name 
    lnTotal     =  RECCOUNT('GLTRNSHD')
  CASE lcRpForm = "GLUNTRAD"
    lnTotal     =  RECCOUNT('GLTRNSDT')
    lcRpFiles   = "GLTRNSHD,GLTRNSDT,GLACCHAR"  && Get slected files name 
ENDCASE   

***   Get the field order ****
lcPrOrder  = ''
lnCount    = 0
lnOldAlias = SELECT()     
lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                'ORDER BY '+lcRpGroup+;
                IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
                
***   Create select  statment
IF llOGFltCh .OR. (lcRpOldOrd <> lcPrOrder) .OR. glEscPrs
  lcRpOldOrd = lcPrOrder
  
  *** Save escape setting
  lcSaveEsc = SET('ESCAPE')
  lcSaveOnEs = ON('ESCAPE')
  SET ESCAPE ON
  ON ESCAPE DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
       
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT     
       
  *** Select data from file(s)
  SELECT  DISTINCT  &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
    &lcPrOrder.;
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
glEscPrs = .F.    
    
*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

DO CASE
  CASE lcRpForm =   "GLUNTRAD"
      =lfChangeGrid('GLUNTRA2')
  CASE lcRpForm =   "GLUNTRAS"
      =lfChangeGrid('GLUNTRAN')  
ENDCASE  

*!************************************************************************
*!
*!      FUNCTION lfClearRep
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
*!      Function lfvPosYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvPosYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
*B600356,1 Move the alltrim from the year value to seek right.
*lcRpPostYr = ALLTRIM(EVALUATE(SYS(18)))
lcRpPostYr = EVALUATE(SYS(18))

lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpPostYr)  
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFISHD ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *** Search for the current company+year
    *E300789,7 [BEGIN]
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcPrnt_Cmp+lcRpPostYr)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    *    =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(lcRpPostYr)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)    
    *E300789,7 [END..]
      &lcRpCurFld = laRpRetFld[1]
      lcRpPostYr = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    
    *USE IN SYCFISHD
    USE IN FISHD
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF    
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvPosPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvPosPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(lcRpPostYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = ALIAS()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
    
    *IF NOT USED("SYCFSPRD") 
    *  SELECT 0
    *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
    *  llUesdBefo = .T.
    *ENDIF
    *SELECT SYCFSPRD
    IF NOT USED("FSPRD") 
      SELECT 0
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
    
    *** Search for the current company+year+Prd
    *E300789,6 [begin
    *E IF ('?' $ &lcRpCurFld. .OR.;  
    *E   !SEEK(gcAct_comp+ALLTRIM(lcRpPostYr)+ALLTRIM(&lcRpCurFld.)));
    *E   .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpPostYr)+ALLTRIM(&lcRpCurFld.)));
      .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

        *E =gfBrows([gcAct_comp+ALLTRIM(lcRpPostYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows([ALLTRIM(lcRpPostYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)

      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 [end 
    
    IF llUesdBefo       && .F.- this file used by the system
      
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
      *USE IN SYCFSPRD
      USE IN FSPRD
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
      
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF    
  ENDIF
ELSE
  *B600361,1 Reham On 05/31/95 Validate the period if the year is empty
  *B600361,1 to prevent make it accept anything.
  &lcRpCurFld = "  "
  SHOW GET (lcRpCurFld)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))))

*!************************************************************************
*!
*!      FUNCTION lfvTrnCode
*!
*!************************************************************************
*
FUNCTION lfvTrnCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CTranNO:H="Code",CTrnDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
SELECT GLTRNSHD
*E300789,6 Set order [begin
SET ORDER TO 1
*E300789,6 Set order [end

SET FILTER TO ALLTRIM(CTRNSTAT) <> 'P'.AND. ALLTRIM(CTRNSTAT) <> 'Z'
LOCATE FOR ALLTRIM(CTRANNO) = &lcRpCurFld.
IF ('?' $ &lcRpCurFld. .OR. !FOUND()) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

  *E =gfBrows([''],'CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)
  =gfBrows('','CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET FILTER TO

*!************************************************************************
*!
*!      FUNCTION lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode
PARAMETERS lcWhichFile

lcWhichFile=IIF(TYPE('lcWhichFile') $ 'ULND','GLAUTHD',lcWhichFile)
IF !EMPTY(EVAL(VARREAD()))
  DECLARE laRpRetFld(2)
  lcOldBrFld    = lcBrFields
  lcRpVldEnt="|Recaring|Distribuation|Template|Automatic|"
  lcAutType=[IIF(EMPTY(CautType) OR !(CautType $ 'RDTA'),'',SUBSTR(lcRpVldEnt,]+;
                    [ATC('~',lcRpVldEnt,ATC(CautType,'RDTA'))+1,]+;
                    [(ATC('~',lcRpVldEnt,ATC(CautType,'RDTA')+1)-1)-]+;
                    [(ATC('~',lcRpVldEnt,ATC(CautType,'RDTA')))))]
  lcBrFields    = 'CautType='+lcAutType+':H="Automatic type",CAutCode:H="Code",CAutDes:H="Description"'
  laRpRetFld[1] = ''
  lcRpCurFld      = VARREAD()
  llGLAUTHD = .F.
  IF !USED('GLAUTHD')
    SELECT 0
    USE &gcDataDir.GLAUTHD
    llGLAUTHD = .T.
  ENDIF
  SELECT GLAUTHD
  SET ORDER TO TAG typecode
  DIMENSION laOGFltr[ALEN(laOGHDFlt,1)+ALEN(laOGFXFlt,1)+ALEN(laOGVRFlt,1),ALEN(laOGHDFlt,2)]
  =ACOPY(laOGHDFlt,laOGFltr)
  =ACOPY(laOGFXFlt,laOGFltr,1,ALEN(laOGFXFlt),ALEN(laOGHDFlt)+1)
  =ACOPY(laOGVRFlt,laOGFltr,1,ALEN(laOGVRFlt),ALEN(laOGHDFlt)+ALEN(laOGFxFlt)+1)
  lnAutCodType= ASCAN(laOGFltr,lcWhichFile+'.CAUTTYPE  ')
  lcAutCodType= IIF(lnAutCodType<>0,laOGFltr[ASUBSCR(laOGFltr,lnAutCodType,1),6],'')
  llScanAll=EMPTY(lcAutCodType)    
  llFoundHD = .F.
  lnAutCount=1
  DO WHILE !llFoundHD AND lnAutCount<=4
    lcAutCodType = IIF(llScanAll,SUBSTR('ADRT',lnAutCount,1),lcAutCodType)
    llFoundHD=  SEEK(lcAutCodType+&lcRpCurFld.) 
    IF llFoundHD OR !llScanAll
      EXIT
    ENDIF
    lnAutCount=lnAutCount+1
  ENDDO
  IF !llFoundHD
    =gfBrows(IIF(llScanAll,.f.,"lcAutCodType"),'CAutCode,CAutType',"laRpRetFld",'Codes File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ELSE
    llScanAll=.F.
  ENDIF
  IF llScanAll
    DO CASE
      CASE BETWEEN(lnAutCodType,ALEN(laOGHDFlt)+1,ALEN(laOGHDFlt)+ALEN(laOGFXFlt)+1)
        lnAutCodType= ASCAN(laOGFXFlt,lcWhichFile+'.CAUTTYPE  ')
        IF lnAutCodTyp>0
          laOGFxFlt[ASUBSCR(laOGFxFlt,lnAutCodType,1),6]=laRpRetFld[2]
           =lfOGShowGet()
        ENDIF
       CASE BETWEEN(lnAutCodType,ALEN(laOGHDFlt)+ALEN(laOGFXFlt)+2,ALEN(laOGFltr))
         lnAutCodType= ASCAN(laOGVRFlt,lcWhichFile+'.CAUTTYPE  ')
         IF lnAutCodTyp>0
           laOGVRFlt[ASUBSCR(laOGVRFlt,lnAutCodType,1),6]=laRpRetFld[2]
           =lfOGShowGet()
         ENDIF    
    ENDCASE  
  ENDIF  

  lcBrFields = lcOldBrFld
  SET ORDER TO
  IF llGLAUTHD
    USE IN GLAUTHD
  ENDIF
ENDIF  

*!************************************************************************
*!
*!      FUNCTION lfvSorMod
*!
*!************************************************************************
*
FUNCTION lfvSorMod

DECLARE laRpRetFld(1)
IF EMPTY(EVAL(VARREAD()))
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'Capp_id:H="Modul code",Capp_name:H="Modul name"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
IF !USED('SYDAPPL')
  SELECT 0
  USE (gcSysHome+'SYDAPPL')
ENDIF
SELECT sydAPPL
SET ORDER TO TAG CAPP_ID
IF !SEEK(EVAL(lcRpCurFld)) AND LASTKEY()=13
  =gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",'Moduls',.F.)
  &lcRpCurFld = laRpRetFld[1]
ELSE
  IF !SEEK(EVAL(lcRpCurFld))    
    &lcRpCurFld=""
  ENDIF
ENDIF
SHOW GET (lcRpCurFld)
lcBrFields = lcOldBrFld
SET ORDER TO
