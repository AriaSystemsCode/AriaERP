*:************************************************************************
*:
*: Procedure file: GLTRABAL.RPR
*:                 Trial balance
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:15/05/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : acc. group
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600367,1 Reham On 06/01/95 Fix the acc. code picture in the report 
*B600367,1 dictionary & add validation for this field.
*B800537,1 Hesham El-Sheltawi 03/28/96
*B800537,1 removing the ALLTRIMING of the account code mask that the user
*B800537,1 select to make the LIKE() function work properly
*E500035,1 Hesham 03/28/96
*E500035,1 Getting the Report Year priod ending date
*E300789,6 MAB 03/04/1999 Deal with files which have CompId in its index.
*B602416,1 WMA 04/16/00 Voiding error by force the user to enter the period range
*B603948,1 SSE 10/08/2000 Fix bug of "Table is not ordered" when entering a valid 
*B603948,1                Account Group Code in the Account Group selection.
*B603948,1                This was fixed in Report Header (SydReprt)
*B301551,1 KAM 30/01/2001 convert glrabal.frx,gltraba2.frx,gltraba3.frx from text format
*B301551,1 KAM            to graphic format
*B605676,1 SSE 04/01/2002 Fix bug of not printing properly in Normal Layout graphics mode.
*B605826,1 BWA 08/12/2002 Fix bug of disable all the buttons after printing.
*B804426,1 BWA 09/05/2002 Fix bug of spelling mistake of 'grand total'[FIX IN FRX'S]
*B607407,1 ALB 07/13/2003 Execlude the closing entry from the account balance
**********************************************************************************
*** Report Setup

=lfRpchngeGrp()
lcRpAddExp = " ABS(GLACBALS.NACBOPBAL) + ABS(GLACBALS.NACBCLBAL) +"+;
             " GLACBALS.NACBPTDDR + GLACBALS.NACBPTDCR > 0"
lcPrExp = lcRpExp
IF !EMPTY(laOGFxFlt[3,6])
* lcRpAddCod = [!EOF('GLGRPDT')]
*"GLACBALS.CacctCode = GLGRPDT.CacctCode AND "+;
             "GLGRPDT.CGRPCODE = GLGRPHD.CGRPCODE"
             
  lcPrExp    = STRTRAN(lcRpExp,[ GLGRPDT.CGRPCODE = "]+laOGFxFlt[3,6]+'"',[ !EOF('GLGRPDT')]) 
ENDIF  
  lcPrExp    = IIF(lcRpZerAct='N',lcPrExp + ' AND '+ lcRpAddExp,;
                   lcPrExp) 
                                 
*lnTotal     =  RECCOUNT('GLACBALS')
&& Get slected files name 
*lcRpFiles = IIF(EMPTY(ALLTRIM(laOGFxFlt[3,6])),"GLACBALS",;
                              "GLACBALS,GLGRPDT,GLGRPHD")
*lcPrExp = lcPrExp+lfSegExp()
*B800537,1 Hesham (Start)                              
*B800537,1 if the user entered account code mask then change the expression
*B800537,1 to use the like function or if the user entered a complete 
*B800537,1 account code then leave  the expression using the cacctcode= value
lcMaskExp = lfSegExp()
IF !EMPTY(STRTRAN(laOGFxFlt[4,6],'-'))
  IF ATC(' ',laOGFxFlt[4,6]) > 0
    lcPrExp    = STRTRAN(lcPrExp,[ GLACBALS.CACCTCODE = "]+laOGFxFlt[4,6]+'"',lcMaskExp)
  ENDIF  
ELSE  
  lcPrExp    = STRTRAN(lcPrExp,[AND  GLACBALS.CACCTCODE = "]+laOGFxFlt[4,6]+'"','') 
ENDIF  
*B800537,1 Hesham (End)
***   Get the field order ****

*lnCount    = 0
lnOldAlias = SELECT()

***   Create select  statment
*IF llOGFltCh .OR. glEscPrs

  *** Save escape setting
 * lcSaveEsc = SET('ESCAPE')
 * lcSaveOnEs = ON('ESCAPE')
 * SET ESCAPE ON
 * ON ESCAPE DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
 * _TALLY = 0
       
  *** Activate the system select therom.
 * SET TALK ON
  
 * WAIT 'Collecting data...' WINDOW NOWAIT          
  
  *** Select data from file(s)
*   SELECT  &lcRpFields.;
    FROM  &lcRpFiles. ;
    WHERE  &lcPrExp.;
    &lcRpOrder.;
    INTO CURSOR &lcRpTargt.
  
  *** Restore the old setting
 * WAIT CLEAR
 * SET TALK OFF  
 * SET ESCAPE &lcSaveEsc 
 *ON  ESCAPE &lcSaveOnEs 
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
*  IF _TALLY = 0        && No records collected
*    ** NO recoeds hove been collected
*    =gfModalGen("INM00052B00000","DIALOG")
*  ELSE
    SELECT GLACBALS
    =lfSetRel()
*--hesham el-sheltawi
lnRpColWid=18
*B601150,1 Hesham El-Sheltawi (Start)
*B601150,1 change the return picture of the amount for the currency settings
lnRPTempCol=lnRpColWid
lnRpColWid=lnRpColWid -LEN(SET('CURR',1))
*B601150,1 Hesham El-Sheltawi (End)
lnRpColDis=VAL(lcRpDecPla)
lcRPTemPic=REPL('?',lnRpColWid)
lcRPTemPic=IIF(lnRpColDis> 0,STRTRAN(lcRpTemPic,'?','.',lnRpColWid-lnRpColDis,1),lcRPTemPic)
lcRpColPic=''
lnNoDecPoint = IIF(lnRpColDis>0,lnRpColWid-lnRpColDis-1,lnRpColWid)
FOR lnColCount = 1 TO (lnNoDecPoint)-MOD(lnNoDecPoint,4)
  lcRpColPic=lcRpColPic+IIF(MOD(lnColCount,4)=1,',','9')
ENDFOR
lcRpColPic=PADL(lcRpColPic,lnNoDecPoint,'9')
lcRpColPic=STUFF(lcRPTemPic,1,LEN(lcRpColPic),lcRpColPic)
lcRpColPic='@$ '+STRTRAN(lcRpColPic,'?','9')
*B601150,1 Hesham El-Sheltawi (Start)
*B601150,1 change the return picture of the amount for the currency settings
lnRpColWid = lnRPTempCol
*B601150,1 Hesham El-Sheltawi (End)
*E500035,1 Hesham (Start)
*E500035,1 Getting the Report Year priod ending date
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

*E300789,6 search current year+Prd only[Begin
*** Search for the current company+year+Prd
*= SEEK(gcAct_comp+ALLTRIM(laOGFxFlt[1,6])+RIGHT(laOGFxFlt[2,6],2)))
= SEEK(ALLTRIM(laOGFxFlt[1,6])+RIGHT(laOGFxFlt[2,6],2)))
ldYearPrd = DFSPPENDT
*E300789,6 search current year+Prd only[END 

IF llUesdBefo 
  
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
  *USE IN SYCFSPRD
  USE IN FSPRD
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
  
ENDIF

*E500035,1 Hesham (End)
SELECT GLACBALS

*B605826,1 BWA 08/12/2002 Fix bug of disable all the buttons after printing.[START]
*DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcPrExp),'For ','')+lcPrExp

SET DEVICE TO PRINT
DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcPrExp),'For ','')+lcPrExp
SET DEVICE TO SCREEN
*B605826,1 BWA 08/12/2002.[END]

*  ENDIF
*ELSE
*  SELECT (lcRpTargt)
*   =lfSetRel()
* DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(lcPrExp),'For ','')+lcPrExp
*ENDIF 
*SET RELATION TO
=lfClrRel()
SELECT (lnOldAlias)  
glEscPrs = .F.    

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*SET RELATION TO

*IF USED(lcRpTargt)
*  USE IN ALIAS(lcRpTargt)
*ENDIF
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfSetRel
*!
*!************************************************************************
*
FUNCTION lfSetRel
IF !USED('GLACCHAR')
  SELECT 0
  USE &gcDataDir.GLACCHAR
ENDIF
SELECT GLACCHAR
SET ORDER TO TAG ACCTCODE
SELECT GLACBALS
SET RELATION TO CACCTCODE INTO GLACCHAR ADDITIVE
IF !USED('GLTYPES')
  SELECT 0
  USE &gcDataDir.GLTYPES
ENDIF
SELECT GLTypes
SET ORDER TO TAG TYPECODE

SELECT glacchar
SET RELATION TO glacchar.ctypecode INTO GLTYPES ADDITIVE

IF !EMPTY(laOGFxFlt[3,6])
  SET RELATION TO laOGFxFlt[3,6]+ glacbals.cacctcode INTO GLGRPDT ADDITIVE
ENDIF  
*SELECT (lcRpTargt)
SELECT GLACBALS

*!************************************************************************
*!
*!      FUNCTION lfClrRel
*!
*!************************************************************************
*
FUNCTION lfClrRel

SELECT GLACCHAR
SET RELATION TO 
SELECT GLACBALS
SET RELATION TO

*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
*B600356,1 Move the alltrim from the year value to seek right.
*lcRpFicsYr = ALLTRIM(EVALUATE(SYS(18)))
lcRpFicsYr = EVALUATE(SYS(18))

lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpFicsYr)  
  lcOldAlias = SELECT()    && Save the current alias
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
  
    *E300789,6 search current year only[Begin
    *** Search for the current company+year
    *E IF ('?' $ &lcRpCurFld. .OR. !SEEK(gcPrnt_Cmp+lcRpFicsYr)) 

     IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr)) 
        *E =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 search current year only[End
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(lcRpFicsYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = SELECT()    && Save the current alias
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
    
    *E300789,6 search current year+Prd only[Begin
    *** Search for the current company+year+Prd
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *E  !SEEK(gcAct_comp+ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        *E =gfBrows([gcAct_comp+ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)

    *E300789,6 search current year+Prd only[end
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
      *USE IN SYCFSPRD
      USE IN FSPRD
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
      
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvGrpCode
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
Function lfvGrpCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CGrpCode:H="Code",CGrplnHed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(&lcRpCurFld.) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = ALIAS()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    IF NOT USED("GLGRPHD") 
      SELECT 0
      USE &gcSysHome.GLGRPHD ORDER TAG grpcode
      llUesdBefo = .T.
    ENDIF
    SELECT GLGRPHD
    *** Search for the current Group code
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(&lcRpCurFld.)))
        =gfBrows([],'CGrpCode',"laRpRetFld",'Group Codes ',.F.)
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      USE IN GLGRPHD
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF    
  ENDIF
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

*!************************************************************************
*!
*!      FUNCTION lfSegExp
*!
*!************************************************************************
FUNCTION lfSegExp

IF !EMPTY(STRTRAN(laOGFxFlt[4,6],'-'))
*B800537,1 Hesham (Start)
*B800537,1 removing the ALLTRIMING of the account code mask that the user
*B800537,1 select to make the LIKE() function work properly
*lcRpSegMas = IIF(EMPTY(ALLTRIM(laOGFxFlt[4,6])),;
              STRTRAN(STRTRAN(lcRpPic, 'X', '?'),'9','?'),;
              STRTRAN(ALLTRIM(laOGFxFlt[4,6]), ' ', '?'))

lcRpSegMas = IIF(EMPTY(ALLTRIM(laOGFxFlt[4,6])),;
              STRTRAN(STRTRAN(lcRpPic, 'X', '?'),'9','?'),;
              STRTRAN(PADR(laOGFxFlt[4,6],LEN(GLACBALS.cacctCode)), ' ', '?'))
*B800537,1 Hesham (End)              
ELSE 
  RETURN ''              
ENDIF              
RETURN [ LIKE("]+lcRpSegMas+[",GLACBALS.cacctCode)]

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED("SYCACCOD") 
*  SELECT 0
*  USE (gcSysHome+'SYCACCOD') ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*SELECT SYCACCOD

IF NOT USED("ACCOD") 
  SELECT 0
  *E300789,6 now COMPID not found in file[begin
  *USE (gcDataDir+'ACCOD') ORDER TAG COMPID
  USE (gcDataDir+'ACCOD')
  *E300789,6 now COMPID not found in file[end
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*E300789,6 now open file with out active index[begin
*E SET ORDER TO TAG COMPID
GO TOP
*E300789,6 now open file with out active index[end

*E300789,6 comment out the followin if block code [begin
*E300789,6 and transfer it to if eof block.
*E IF SEEK(gcAct_Comp)
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *lcRpSegMak = ALLTRIM(SYCACCOD.cacsmask)
  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
*E   lcRpSegMak = ALLTRIM(ACCOD.cacsmask)
*E   lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
*E   lcRpSegMak = STRTRAN(lcRpSegMak, '#', '9',2) 
*E   lcRpSegMak = STRTRAN(lcRpSegMak, '#', 'X',1,1) 
*E ELSE
*E   lcRpSegMak = " "
*E ENDIF

IF EOF()
  lcRpSegMak = " "
  
ELSE
  lcRpSegMak = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  lcRpSegMak = STRTRAN(lcRpSegMak, '#', '9',2) 
  lcRpSegMak = STRTRAN(lcRpSegMak, '#', 'X',1,1) 

ENDIF
*E300789,6 comment out the followin if block code [end

IF llUesdBefo       && .F.- this file not used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *USE IN SYCACCOD
  USE IN ACCOD
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMak

*!************************************************************************
*!
*!      FUNCTION lfWGLTraial
*!
*!************************************************************************
*
FUNCTION lfWGLTraial
IF EMPTY(laOGFxFlt[1,6])
  laOGFxFlt[1,6] = lfGetCurYr()
ENDIF

IF EMPTY(laOGFxFlt[2,6])
  laOGFxFlt[2,6] = lfGetCurPD()
  laOGFxFlt[2,6] = laOGFxFlt[2,6]+lcElmSep+laOGFxFlt[2,6]
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfChkValue
*!
*!************************************************************************
*
FUNCTION lfChkValue


*B602416,1 WMA 04/16/00 Call Function lfEmptyPrd() to check if the Period ID is empty [Begin]
*IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6])))
IF (EMPTY(ALLTRIM(laOGFxFlt[1,6]))  .OR.  lfEmptyPrd())
*B602416,1 WMA 04/16/00 [End]
  ** NO recoeds hove been collected
  =gfModalGen("INM02219B00000","DIALOG")
  RETURN .F.
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurYr

llCompUsd=.f.
lcRetVal = '    '
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF
SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(gcAct_Comp,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurPD

llCompUsd=.f.
lcRetVal='  '
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF
SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(gcAct_Comp,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_PRD
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

*!************************************************************************
*!
*!      FUNCTION lfdum
*!
*!************************************************************************
*
FUNCTION lfdum
PARAMETERS lnPrdToDisp

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
lnEndPrd  = VAL(RIGHT(laOGFxFlt[2,6],2))
lcRetVal=''
FOR lnCount = 1 TO lnEndPrd-lnStrtPrd+1 
  lnPrdToDisp=lnCount 
  lcStrtPrd = SUBSTR(laOGFxFlt[2,6],1,2)
  lcEndPrd  = RIGHT(laOGFxFlt[2,6],2)
  lcTotVar  = 'lnTotPr'+ALLTRIM(STR(lnStrtPrd+lnPrdToDisp-1,2))
  lcRetVal = lcRetVal+'Pr '+STR(lnStrtPrd+lnPrdToDisp-1,2)+':'+STR(&lcTotVar)+CHR(13)+CHR(10)
ENDFOR
WAIT lcRetVal WINDOW
RETURN lcRetVal

*!*********************************************************************************
*!
*!             FUNCTION : lfTotPriod
*!
*!*********************************************************************************
*
FUNCTION lfTotPriod
PARAMETERS lnPrdToDisp

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
*lnEndPrd  = VAL(RIGHT(laOGFxFlt[2,6],2))
*lcRetVal=''
lcRetVal = 'Period '+STRTRAN(STR(lnStrtPrd+lnPrdToDisp-1,2),' ','0')+': '
RETURN lcRetVal

*!*********************************************************************************
*!
*!             FUNCTION : lfTotBBal
*!
*!*********************************************************************************
* 
FUNCTION lfTotBBal
PARAMETERS lnPrdToDisp,lcRepVar

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
*lnEndPrd  = VAL(RIGHT(laOGFxFlt[2,6],2))
*lcRetVal=''
lcRetVal =  EVAL(lcRepVar+ALLTRIM(STR(lnStrtPrd+lnPrdToDisp-1,2)))
*EVAL(lcRepVar+ALLTRIM(STR(VAL(SUBSTR(laOGFxFlt[2,6],1,2))+lnPrdToDisp-1,2)))
RETURN lcRetVal

*!*********************************************************************************
*!
*!             FUNCTION : lfRpchngeGrp
*!
*!*********************************************************************************

FUNCTION lfRpchngeGrp
PRIVATE lcErrHand,llError

llError=.F.
lcErrHand = ON('ERROR')
ON ERROR llError=.T.
IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
  IF !EMPTY(lcOGTmpForm) 
    lcOGSelect=SELECT()
    IF !USED(lcOGTmpForm)
      SELECT 0
      USE (gcWorkDir+lcOGTmpForm+'.FRX')
      
      *B605676,1 Fix bug of Empty lines in FRX DOS. [Begin]
      IF llError AND UPPER(MESSAGE())='STRUCTURAL CDX FILE NOT FOUND'
        llError=.F.
        lcErrHand = ''
        USE (gcWorkDir+lcOGTmpForm+'.FRX')
      ENDIF
      *B605676,1 Fix bug of Empty lines in FRX DOS. [End]

    ENDIF
    IF !llError
      LOCATE FOR PLATFORM=lcOGPlatForm AND OBJTYPE=9 AND OBJCODE=5
      IF FOUND()
        REPLACE HEIGHT WITH HEIGHT-(13-(VAL(RIGHT(laOGFxFlt[2,6],2))-VAL(SUBSTR(laOGFxFlt[2,6],1,2))+1))
      ENDIF
    ENDIF   
    IF USED(lcOGTmpForm)
      USE IN (lcOGTmpForm)
    ENDIF
    SELECT(lcOGSelect)  
  ENDIF  
ENDIF
ON ERROR &lcErrHand 
RETURN

*!*********************************************************************************
*!
*!        FUNCTION : lfRpAlltrim
*!
*!*********************************************************************************
*
FUNCTION lfRpAlltrim
PARAMETERS lcVarName

&lcVarName=ALLTRIM(&lcVarName)
 
*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
*
FUNCTION lfvActCod

*B600367,1 Reham On 06/01/95 Add this function to browse if acc. does not
*B600367,1 exist in the file.

*B800537,1 Hesham On 03/28/96
*B800537,1 change the function validation to make it check with
*B800537,1 the account code mask that the user select to suit the 
*B800537,1 report functionality


DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

IF !EMPTY(&lcRpCurFld)
  SELECT GLACCHAR
*B800537,1 Hesham (Start) 
*B800537,1 remark the activating the browse function for the accout
*B800537,1 code to use a mask instead of using single account
*  IF "?" $ &lcRpCurFld .OR. !SEEK(&lcRpCurFld)
*    =gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
*    &lcRpCurFld = laRpRetFld[1]
*    SHOW GET (lcRpCurFld)
*  ENDIF
*B800537,1 Hesham (Hesham) 

*B800537,1 Hesham (Start) 
*B800537,1 changing the validation to check if the mask of the account
*B800537,1 code the user entered is valid mask or not
IF !EMPTY(STRTRAN(EVAL(VARREAD()),'-'))
  lcRpSegMas = STRTRAN(PADR(EVAL(VARREAD()),LEN(cacctCode)), ' ', '?')
  lcRpSegMas= [ LIKE("]+lcRpSegMas+[",cacctCode)]
  LOCATE FOR &lcRpSegMas
  IF !FOUND() .AND. gfModalGen("INM02245B00000",'DIALOG')=1
    &lcRpCurFld = lcRpOld
    SHOW GET (lcRpCurFld)    
  ENDIF
ENDIF    
*B800537,1 
ENDIF
lcBrFields = lcOldBrFld



FUNCTION lfGetCur
PARAMETERS lnAmm,lnWid,lnDec
*B601150,1 Hesham El-Sheltawi (Start)
*B601150,1 change the return picture of the amount for the currency settings
*RETURN TRAN(lnAmm,lcRpColPic)
RETURN PADL(ALLTR(TRAN(lnAmm,lcRpColPic)),lnRpColWid)
*B601150,1 Hesham El-Sheltawi (End)


*!*************************************************************
*! Name      : lfEmptyPrd
*! Developer : Wael 
*! Date      : 04/16/2000
*! Purpose   : Check whether the period is empty or not.
*!*************************************************************
*! Called from : lfChkValue()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  True, means the period range is empty 
*! 			   :  False, means the period range is not empty  	
*!*************************************************************
*!B602416,1 WMA 04/16/00 Add Function to check if the Period ID is empty. 
FUNCTION lfEmptyPrd
lnFltPos=ASCAN(laOgFxflt,'GLACBALS.CFSPPRDID')
IF lnFltPos > 0
  lnFltRow = ASUBSCRIPT(laOgFxflt,lnFltPos,1)
  lnPID    = ASUBSCRIPT(laOgFxflt,ASCAN(laOgFxflt,'GLACBALS.CFSPPRDID'),1)
  lcFrom   = ALLTRIM(SUBSTR(LAOGFXFLT[lnPID,6],1 , 2))
  lcTo     = ALLTRIM(SUBSTR(LAOGFXFLT[lnPID,6],4 , 5))
  IF EMPTY(lcFrom) .OR. EMPTY(lcTo) 
    RETURN .T.
  ELSE
	RETURN .F.  
  ENDIF
ENDIF  
RETURN .T.


*!*************************************************************
*! Name      : lfCloseAmnt
*! Developer : Wael 
*! Date      : 07/13/2003
*! Purpose   : Summation the closing amount for sertine account
*!*************************************************************
*! Called from : lfCloseAmnt()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  Sum The closing amount
*!*************************************************************
*!B607407,1 ALB 07/13/2003 Execlude the closing entry from the account balance

FUNCTION lfCloseAmnt

PRIVATE lnCloseAmnt
STORE 0 TO lnCloseAmnt,lnSubTot

IF llRpClose
  lcOldAlias = ALIAS()
  SCATTER MEMVAR MEMO
  IF !USED('GLPTRNHD')
    =gfOpenFile(gcDataDir+'GLPTRNHD','TRANSTAT','SH')
  ENDIF
  SELECT GLPTRNHD
  SET ORDER TO TAG Transtat
    
  IF !USED('GLPTRNDT')
    =gfOpenFile(gcDataDir+'GLPTRNDT','BATCHTRN','SH')
    SET ORDER TO TAG Batchtrn
  ENDIF
  SELECT GLPTRNDT
  SET ORDER TO TAG Batchtrn
  
  IF !USED('GLBATCH')
    =gfOpenFile(gcDataDir+'GLBATCH','BATTYPE','SH')
  ENDIF
  SELECT GLBATCH
  SET ORDER TO TAG BATTYPE
  SEEK("CP")
  SCAN REST WHILE cbattype+cbatstat = "CP" FOR CBATPYR = m.CFISFYEAR
    SELECT Glptrnhd
    SEEK('P')
    SCAN REST WHILE ctrnstat+cbatchno+ctranno = 'P' FOR cbatchno = GLBATCH.cbatchno
      SELECT Glptrndt
      SEEK(Glptrnhd.CBATCHNO + Glptrnhd.ctranno)
      SUM REST IIF(CDRORCR = 'C',namount,namount * -1) ;
          WHILE CBATCHNO+ctranno = Glptrnhd.CBATCHNO + Glptrnhd.ctranno FOR (cacctcode  = ALLTRIM(m.cacctcode);
          AND ctrnpyr+ctrnpprd= m.CFISFYEAR + m.CFSPPRDID )  TO lnSubTot
      lnCloseAmnt = lnCloseAmnt + lnSubTot 
    ENDSCAN
  ENDSCAN
  SELECT (lcOldAlias)
ENDIF

RETURN lnCloseAmnt

