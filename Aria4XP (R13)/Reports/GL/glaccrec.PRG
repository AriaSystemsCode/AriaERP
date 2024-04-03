*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLACCREC.PRG
*:  Module      : General Ledger
*:  Desc.       : Account reconciliation
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : *E303229,1
*:************************************************************************
*Modificatins
*B610471,1 TMI 08/19/2013 fix a problem that selecting Group filter does not return data [T20130812.0014] 
*E303484,1 TMI 06/24/2014 add custom code to run portrait version of GLACCREC         [T20140611.0025 ] ( Custom fix C201625 
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011]
*E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011]
*B611101,1 MMT 01/14/2016 Custom GL reconsiliation report does not show vendor in case of AP accounts[T20150716.0011 - ISSUE#3]
*B611145,1 MMT 05/15/2016 Fix bug of report slow performance at GMA[T20160311.0001]
*B611546,1 SAH 04/04/2018  Modify the report to comment the line(SET SKIP TO) AS Account reconciliation report does not print accounts data for certain period at GAY10 Company 02 [T20180320.0010]

*:************************************************************************
*** Report Setup
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\gl\glaccrec.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

SET ENGINEBEHAVIOR 70

PRIVATE lcAcctCode
lcAcctCode = ''
lcAcBals = loOgScroll.gfTempName()

*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

SET ORDER TO TAG ACCYRPRD IN GLACBALS

IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6]))) AND;
   lcRpDayPd = 'PERIODS'
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

IF (EMPTY(DTOS(lcRpFrDay)) .OR. EMPTY(DTOS(lcRpToDay))) AND;
   lcRpDayPd = 'DAYS'
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

IF lcRpDayPd = 'DAYS' AND (lcRpFrDay > lcRpToDay)
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *WAIT 'END DATE MUST GREATER THAN OR EQUAL TO BEGINING DATE' WINDOW
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDGREATER,oAriaApplication.GetHeaderText("LANG_ENDGREATER",AHEADERFILE)) WINDOW
  *N000682,1 MMT 02/10/2013 Globalization changes[END]
  RETURN
ENDIF

llDone     = .F.
lnBalance  = 0

DO CASE
  CASE lcRpSumzBy = 'D'
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNDT.DTRNPDATE ,] +;
              [ GLPTRNDT.CTRNPYR  , GLPTRNDT.CTRNPPRD , GLPTRNDT.CDRORCR]
    lcOrderBy = [DTOS(DTRNPDATE)]
  CASE lcRpSumzBy = 'S'
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNhd.CSRCJRNL ,] +;
              [GLPTRNDT.CTRNPYR , GLPTRNDT.CTRNPPRD   , GLPTRNDT.CDRORCR]
    lcOrderBy = [CSRCJRNL]
  CASE lcRpSumzBy = 'B'
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNHD.CSRCJRNL , GLPTRNDT.DTRNPDATE ,]+;
              [GLPTRNDT.CTRNPYR   , GLPTRNDT.CTRNPPRD , GLPTRNDT.CDRORCR   ]
    lcOrderBy = [CSRCJRNL+DTOS(DTRNPDATE)]
ENDCASE

IF lcRpSumzBy<>'N'
  IF lcRpForm = 'GLACCRED'
    lcWhere = "BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")
  ELSE
    *- Variable assignments
    lcRpYrPrF = laOGFxFlt[1,6]+LEFT(laOGFxFlt[2,6],2)	&&Assign From period
    lcRpYrPrT = laOGFxFlt[1,6]+RIGHT(laOGFxFlt[2,6],2)  &&Assign To period
    lcWhere = "BETWEEN(GLPTRNDT.CTRNPYR+GLPTRNDT.CTRNPPRD,lcRpYrPrF,lcRpYrPrT)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")
  ENDIF

  gcDataDir = oAriaApplication.DataDir
  SELECT GLPTRNDT.cdrorcr , GLPTRNDT.DTRNPDATE , GLPTRNDT.CACCTCODE ,;
         GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD                         ,;
         SUM(GLPTRNDT.NAMOUNT) AS nAmount , GLPTRNhd.CSRCJRNL          ;
    FROM &gcDataDir.GLPTRNDT,&gcDataDir.GLPTRNHD                     ;
   WHERE GLPTRNDT.cbatchno + GLPTRNDT.ctranno = GLPTRNHD.cbatchno +  ;
         GLPTRNHD.ctranno AND GLPTRNHD.CTRNTYPE <> 'B' AND &lcWhere  ;
   GROUP BY &lcRPGrp                                                 ;
   INTO DBF (oAriaApplication.WorkDir+lcRpTargt)

  INDEX ON CACCTCODE+CTRNPYR+CTRNPPRD+&lcOrderBy TAG TmpTag

  SELECT GLACBALS
  SET RELATION TO glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid INTO &lcRpTargt ADDITIVE
ENDIF

lcRpTrgFil = IIF(lcRpSumzBy<>'N',lcRpTargt,'GLPTRNDT')

lcRpExp=IIF(lcRpExp='.T.','',lcRpExp)

IF lcRpSumzBy='N'
  lcRpExp= lcRpExp+IIF(!EMPTY(lcRpExp),'AND ','')+[GLPTRNhd.CTRNTYPE<>'B']+;
           IIF(!EMPTY(lcRpSjGrop),[ AND GLPTRNHD.CSRCJRNL = lcRpSjGrop ]," ")
ENDIF

IF lcRpDayPd = 'DAYS'
  lcExpr=lcRpExp+IIF(!EMPTY(lcRpExp),'AND ','')+'BETWEEN(GLACBALS.CFISFYEAR+GLACBALS.cfspprdid,lcRpYrPrF,lcRpYrPrT)'+;
         'AND &lcRpTrgFil..dtrnPdate <= lcRpToDay'
ELSE
  lcExpr=lcRpExp
ENDIF

IF 'CGRPCODE'$ lcRpExp
  SELECT GLGRPDT
  SET ORDER TO TAG ACCYRPRD IN GLACBALS
  SET RELATION TO glgrpdt.cacctcode INTO GLACBALS ADDITIVE
  SET FILTER TO CGRDSTAT  = 'I'
  GO TOP
ENDIF

SELECT GLACBALS

SET ORDER TO TAG ACCTCODE IN GLACCHAR
SET RELATION TO glacbals.cacctcode INTO GLACCHAR ADDITIVE

SET ORDER TO TAG ACCTCODE IN GLPTRNDT
SET RELATION TO glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid INTO GLPTRNDT ADDITIVE

SELECT glptrndt
SET ORDER TO TAG BATCHTRN IN GLPTRNHD
SET RELATION TO glptrndt.cbatchno + glptrndt.ctranno INTO GLPTRNHD ADDITIVE

IF 'CGRPCODE'$ lcRpExp
  SELECT GLGRPDT
  SET SKIP TO GLACBALS,&lcRpTrgFil
ELSE
  SELECT GLACBALS
   IF lcRpSumzBy<>'N'
    SET SKIP TO glptrndt,&lcRpTargt
   ELSE
     SET SKIP TO glptrndt
   ENDIF
ENDIF

* Summarization is (N/A) Not Applicable and Sort by source journal
IF lcRpSumzBy='N' AND llRpGrBySJ
  lnAliass = SELECT(0)		&&Save ALIAS before continue, to reuse it for data collecting.

  SET ORDER TO TAG SRCJRNL IN GLSUBJOR

  DIMENSION laFileStru[1,4],laTemp[1,4],laFileFlds[4,2]
  STORE SPACE(0) TO laFileStru,laTemp

  *Files used in collecting data
  laFileFlds[1,1] = 'GLACBALS'
  laFileFlds[2,1] = 'GLACCHAR'
  laFileFlds[3,1] = 'GLPTRNDT'
  laFileFlds[4,1] = 'GLSUBJOR'

  laFileFlds[1,2] = 'CACCTCODE,CFSPPRDID,CFISFYEAR,NACBOPBAL'
  laFileFlds[2,2] = 'CACCNLDES'
  laFileFlds[3,2] = 'CBATCHNO,CTRANNO,CTRDTEXP,DTRNPDATE,NAMOUNT,CDRORCR'
  laFileFlds[4,2] = 'CSRCJRNL,CJORLNDES'

  *-  [BEGIN] The aim is to:
  *-  construct the temporary table from related
  *-  tables, to be easy for any one to increase or decrease it.
  *-  Fill structure array calling a function called lfMakeStru, with
  *-  two parameters,1st is file name and 2nd is fields required.
  lnElement = 1 			&& To have the start position in the structure array.
  FOR lnItems = 1 TO ALEN(laFileFlds,1)
    =lfMakeStru(laFileFlds[lnItems,1],laFileFlds[lnItems,2])
    lnDim = IIF(ALEN(laFileStru,1)=1,ALEN(laTemp,1) ,;
                ALEN(laFileStru,1)+ALEN(laTemp,1))  &&Redimension the array for structure array.
    DIMENSION laFileStru[lnDim,4]
    =ACOPY(laTemp,laFileStru,1,-1,AELEMENT(laFileStru,lnElement,1))
    lnElement = lnElement + ALEN(laTemp,1)
  ENDFOR

  *-  IF it is a Days case, Add a field Contains description of
  *-  begin balance.
  IF lcRpDayPd = 'DAYS'
    lnFileStru = ALEN(laFileStru,1)+1
    DIMENSION laFileStru[lnFileStru, 4]
    laFileStru[lnFileStru ,1] = 'cSt_Bal'
    laFileStru[lnFileStru ,2] = 'C'
    laFileStru[lnFileStru ,3] = 78
    laFileStru[lnFileStru ,4] = 0
  ENDIF

  lcTempFile = loOgScroll.gfTempName()      && Variable to hold Temp. name to create cursor
  CREATE CURSOR (lcTempFile) ;
         FROM ARRAY laFileStru

  INDEX ON cSRcjrnl+CACCTCODE+CFISFYEAR+CFSPPRDID TAG (lcTempFile)

  *-  The aim is to:[END]
  lcScanExpr = IIF(EMPTY(lcExpr)                                     ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.)'        ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.) AND ') + ;
               lcExpr		&&Expression used for scanning last open alias

  lcOldAcct = ''		&& E500153,1  Used to compare with Account Code	

  SELECT (lnAliass)		&&Using the last alias used before creating CURSOR
  SCAN &lcScanExpr
    *- IF By Days and the Account Code has changed
    IF (lcRpDayPd = 'DAYS') AND (lcOldAcct != CACCTCODE)
      lcAccCode = CACCTCODE
      lcSt_Bal = lfInit()      && calculate Begining balance
      lcOldAcct = CACCTCODE	   && Store Account Code Value
      LOCATE &lcScanExpr AND CACCTCODE = lcAccCode
    ENDIF    && End of IF

    *- Fill all required fields from corresponding tables.
    INSERT INTO (lcTempFile)                           ;
             (CACCTCODE,CFSPPRDID,CFISFYEAR,NACBOPBAL ,;
              CACCNLDES                               ,;
              CBATCHNO,CTRANNO,CTRDTEXP               ,;
              CSRCJRNL                                ,;
              DTRNPDATE,NAMOUNT,CDRORCR)               ;
      VALUES (GLACBALS.CACCTCODE    ,;
              GLACBALS.CFSPPRDID    ,;
              GLACBALS.CFISFYEAR    ,;
              GLACBALS.NACBOPBAL    ,;
              GLACCHAR.CACCNLDES    ,;
              GLPTRNDT.CBATCHNO     ,;
              GLPTRNDT.CTRANNO      ,;
              GLPTRNDT.CTRDTEXP     ,;
              GLPTRNHD.CSRCJRNL     ,;
              &lcRpTrgFil..DTRNPDATE,;
              &lcRpTrgFil..NAMOUNT  ,;
              &lcRpTrgFil..CDRORCR   )

    IF (lcRpDayPd = 'DAYS')
      REPLACE &lcTempFile..cSt_Bal    WITH lcSt_Bal ,;
              &lcTempFile..NACBOPBAL  WITH lnBalance
    ENDIF

    =SEEK(GLPTRNHD.CSRCJRNL,'GLSUBJOR')
    REPLACE &lcTempFile..CJORLNDES WITH GLSUBJOR.CJORLNDES
  ENDSCAN

  SELECT (lcTempFile)		&&Use Cursor to be active alias before report

  lcExpr     = ""				&&Clear expresion
  lcRpTrgFil = lcTempFile		&&Assign target file
ENDIF

*- Collect the data in a new tmp. file in the case there is no summarize[Begin]
IF lcRpSumzBy='N' AND !llRpGrBySJ
  lcAccTmp = loOgScroll.gfTempName()
  lcRpTrgFil = lcAccTmp
  lcTrgFil2 = lcAccTmp
  lcAlias = ALIAS()
  SELECT GLPTRNDT
  COPY STRUCTURE TO (oAriaApplication.WorkDir+lcAccTmp) WITH CDX
  USE (oAriaApplication.WorkDir+lcAccTmp) IN 0 EXCLUSIVE
  SELECT (lcAccTmp)
  *E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
  IF TYPE('lcRpGMALayout') = 'C' AND lcRpGMALayout = 'P' &&  "G M A ACCESSORIES, INC.       "
    *E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][Start]
    *ALTER table (lcAccTmp) ADD COLUMN 'TransType' M(10)
    CREATE CURSOR 'ReadTrans' (glaccount C(24),glbatch C(6),ctrnsledn C(8))
    SELECT 'ReadTrans' 
    INDEX On glaccount +glbatch +ctrnsledn  TAG 'ReadTrans' 
    ALTER table (lcAccTmp) ADD COLUMN 'TransType' C(30)
    SELECT (lcAccTmp)
    *E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][End]
  ENDIF
  *E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][End]
  SET ORDER TO TAG Acctcode
  SET RELATION TO &lcAccTmp..cacctcode+&lcAccTmp..ctrnpyr+&lcAccTmp..ctrnpprd INTO GLACBALS ADDITIVE

  SELECT GLGRPDT
  SET RELATION TO
  SET ORDER TO TAG Grcodacc

  SELECT GLACBALS
   *B611546,1 SAH 04/04/2018  Modify the report to comment the line(SET SKIP TO) AS Account reconciliation report does not print accounts data for certain period at GAY10 Company 02 [T20180320.0010] [begin]
   *SET SKIP TO
   *B611546,1 SAH 04/04/2018  Modify the report to comment the line(SET SKIP TO) AS Account reconciliation report does not print accounts data for certain period at GAY10 Company 02 [T20180320.0010] [end]

  IF 'CGRPCODE'$ lcRpExp
    *B610471,1 TMI 08/19/2013 [Start] replace the " with '
    *lcstat = "GLGRPDT.CGRPCODE = "+'"'+laOGFxFlt[3,6]+'"'
    lcstat = "GLGRPDT.CGRPCODE = "+"'"+laOGFxFlt[3,6]+"'"
    *B610471,1 TMI 08/19/2013 [End  ] 
    lcExpr = STRTRAN(lcExpr,lcstat,".T.")
  ENDIF
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTDATA,oAriaApplication.GetHeaderText("LANG_COLLECTDATA",AHEADERFILE)) NOWAIT
  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  IF lcRpDayPd="PERIODS"
    SET ORDER TO FisfYear
    =SEEK(laOgFxFlt[1,6],'GLACBALS')
    SCAN REST WHILE cFisfYear+cFspprdid = laOgFxFlt[1,6] FOR &lcExpr

      STORE .F. TO llCont , llExpr
      IF BETWEEN(GLACBALS.cFspprdid, LEFT(laOGFxFlt[2,6],2), RIGHT(laOGFxFlt[2,6],2))AND;
         ALLTRIM(GLACBALS.cAcctCode) = ALLTRIM(laOGVrFlt[2,6])
         llExpr = .T.
      ENDIF

      IF !EMPTY(laOGFxFlt[3,6])
        IF SEEK(laOGFxFlt[3,6]+GLACBALS.cAcctCode,'GLGRPDT')
          llCont = .T.
        ENDIF
      ELSE
        llCont = .T.
      ENDIF
      
      IF llCont
        *N000682,1 MMT 02/10/2013 Globalization changes[Start]
        *WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
		WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTDATA,oAriaApplication.GetHeaderText("LANG_COLLECTDATA",AHEADERFILE))  NOWAIT
        *N000682,1 MMT 02/10/2013 Globalization changes[ENd]
        SELECT GLPTRNDT
        SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd ;
                  FOR GLPTRNHD.CTRNTYPE<>'B'
          SCATTER MEMVAR MEMO
          INSERT INTO (lcAccTmp) FROM MEMVAR
          *E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
          IF TYPE('lcRpGMALayout') = 'C' AND lcRpGMALayout = 'P' &&  "G M A ACCESSORIES, INC.       "
            *E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][Start]
            *REPLACE &lcAccTmp..TransType WITH IIF(EMPTY(ALLTRIM(GLPTRNDT.ctrdtexp)),lfGetTransTypes(GLACBALS.CACCTCODE ,GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO),GLPTRNDT.ctrdtexp)
            *B611101,1 MMT 01/14/2016 Custom GL reconsiliation report does not show vendor in case of AP accounts[T20150716.0011 - ISSUE#3][Start]
            REPLACE &lcAccTmp..TransType WITH GLPTRNDT.ctrdtexp
            *B611101,1 MMT 01/14/2016 Custom GL reconsiliation report does not show vendor in case of AP accounts[T20150716.0011 - ISSUE#3][End]
            IF !SEEK(GLACBALS.CACCTCODE+GLPTRNDT.CBATCHNO+GLPTRNDT.CTRANNO,'ReadTrans')
  			  lfGetTransTypes(GLACBALS.CACCTCODE ,GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO)
			ENDIF
            *E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][End]
	      ENDIF
          *E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][End]        
        ENDSCAN
      ENDIF

      SELECT GLACBALS
    ENDSCAN
    SELECT GLACBALS
    SET ORDER TO AccYrPrd
  ELSE
    SCAN FOR &lcExpr  AND IIF(!EMPTY(laOGFxFlt[3,6]),SEEK(laOGFxFlt[3,6]+GLACBALS.cAcctCode,'GLGRPDT'),.T.)
      *N000682,1 MMT 02/10/2013 Globalization changes[Start]
      *WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
      WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTDATA,oAriaApplication.GetHeaderText("LANG_COLLECTDATA",AHEADERFILE))   NOWAIT
      *N000682,1 MMT 02/10/2013 Globalization changes[End]
      SELECT GLPTRNDT
      SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd ;
                FOR GLPTRNHD.CTRNTYPE<>'B' AND BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)
        SCATTER MEMVAR MEMO
        INSERT INTO (lcAccTmp) FROM MEMVAR
      ENDSCAN
    ENDSCAN
  ENDIF
  WAIT CLEAR

  SELECT (lcAccTmp)
   
  DO gfDispRe WITH EVAL('lcRpForm')
ELSE
  lcTrgFil2 = "GLPTRNDT"
  IF lcRpSumzBy <> 'N'
    IF 'CGRPCODE'$ lcRpExp
      IF lcRpDayPd = 'DAYS'
        lcTrgFil2 = lcRpTrgFil
        GO TOP IN GLGRPDT
      ENDIF
    ELSE
      IF lcRpDayPd = 'DAYS'
        lcTrgFil2 = lcRpTrgFil
      ENDIF
      SET SKIP TO
      SET SKIP TO &lcRpTrgFil
    ENDIF
    =lf1AcSlctd()
  ENDIF
  
  DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcExpr),'FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.)','FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.) AND ')+lcExpr
  gcDataDir = oAriaApplication.DataDir
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON
  IF USED('GLACBALS') AND UPPER(oAriaApplication.WorkDir) $ DBF('GLACBALS')
    USE IN GLACBALS
    ERASE (oAriaApplication.WorkDir+lcAcBals+'.DBF')
    ERASE (oAriaApplication.WorkDir+lcAcBals+'.CDX')
    =gfOpenFile(gcDataDir+'GLACBALS','ACCYRPRD','SH')
  ENDIF
  SET FULLPATH &lcFullPath
ENDIF

lcExpr = ""
IF lcRpSumzBy<>'N'
   USE IN ALIAS(lcRpTargt)
   ERASE (oAriaApplication.WorkDir+lcRpTargt+'.BDF')
   ERASE (oAriaApplication.WorkDir+lcRpTargt+'.CDX')

ELSE    && Else  IF Summerization is N/A and Group By S.J.
  IF llRpGrBySJ
    USE IN (lcTempFile) 		&& Note that it is a cursor.
  ELSE
    USE IN ALIAS(lcAccTmp)
    ERASE (oAriaApplication.WorkDir+lcAccTmp+'.BDF')
    ERASE (oAriaApplication.WorkDir+lcAccTmp+'.CDX')
  ENDIF

ENDIF

SELECT GLGRPDT
SET SKIP TO
SET RELATION TO

SELECT GLACBALS
SET SKIP TO
SET RELATION TO

SELECT glptrndt
SET RELATION TO

IF USED(lcRpTargt)
  USE IN (lcRpTargt)
ENDIF
SET ENGINEBEHAVIOR 90


*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvFisYer

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpFicsYr = loOgScroll.ActiveControl.Value

    gcDataDir = oAriaApplication.DataDir

lnPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'GLACBALS.CFISFYEAR'),1)
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray + '['+ALLTRIM(STR(lnPos))+',6]'

IF .NOT.EMPTY(lcRpFicsYr)
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FISHD")
    SELECT 0
    USE (gcDataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD

  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr))
      *N000682,1 MMT 02/10/2013 Globalization changes[Start]
      *=gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
      =gfBrows('','CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANCODE,oAriaApplication.GetHeaderText("LANG_TRANCODE",AHEADERFILE)),.F.)
      *N000682,1 MMT 02/10/2013 Globalization changes[End]
      IF EMPTY(laRpRetFld[1])
        laRpRetFld[1] = loFld.OldValue
      ENDIF
      loOgScroll.&lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
    ENDIF

  IF llUesdBefo       && .F.- this file used by the system

    USE IN FISHD

  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!************************************************************************
*!
*!      Function lfvToDay
*!
*!************************************************************************
* Check if the lcRpFrDay is smaller than lcRpToDay or not
Function lfvToDay

IF lcRpFrDay > lcRpToDay
  _CUROBJ = OBJNUM(_CUROBJ)
ENDIF

*!************************************************************************
*!
*!      Function lfvActBy
*!
*!************************************************************************
* Control the variable related to Activity way
Function lfvActBy
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE

*-get the names in the array laOGObjType to upper case
LOCAL i
FOR i = 1 TO ALEN(laOGObjType,1)
  laOGObjType[i,1] = UPPER(laOGObjType[i,1])
ENDFOR

*-   Transfer assigning of lcRpForm in this valid function ,
*-   rather than using valid entries, because
*-   it affected with changes in three options.
*-   A -] Activity by   (Periods - Days)
*-   B -] Summarization (N/A - DATE - S.J. - DATE/S.J.)
*-   C -] Sort By S.J.  (YES - NO)
*-   Note: This valid function is identical for the three options.


*E303484,1 TMI 06/24/2014 21:06 [Start] check for the GMA layout
SET STEP ON 
IF TYPE('lcRpGMALayout') = 'C' AND lcRpGMALayout = 'P' &&  "G M A ACCESSORIES, INC.       "
  IF lcRpSumzBy='N' AND llRpGrBySJ
    lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCRSG','GLACCRSJ')
  
  ELSE	&&Summarization is not (N/A) OR not (Sorted By S.J.) 	
    lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCREG','GLACCREJ')
  ENDIF
ELSE
  *E303484,1 TMI 06/24/2014 21:06 [End  ] 

  *-   IF Summarization is (N/A) and (Sorted By S.J.)
  IF lcRpSumzBy='N' AND llRpGrBySJ
    lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCRSC','GLACCRSD')
  
  ELSE	&&Summarization is not (N/A) OR not (Sorted By S.J.) 	
    lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCREC','GLACCRED')
  ENDIF
  
  *E303484,1 TMI 06/24/2014 21:06 [Start] 
ENDIF 
*E303484,1 TMI 06/24/2014 21:06 [End  ] 

laOGObjCnt[3] = IIF(lcRpSumzBy='N',.T.,.F.) 	&&IF Summarization !(N/A),DISABLE (Group By S.J.) OPTION
DO CASE
  CASE lcRpDayPd = 'PERIODS'

    laOGObjCnt[5]  = .F.    && From Day
    laOGObjCnt[6]  = .F.    && To Day
    laOGObjCnt[9]  = .T.    && Fiscal year
    laOGObjCnt[10] = .T.    && Period

    lcRpFrDay     = {  /  /    }
    lcRpToDay     = {  /  /    }
    lcRpYrPrF     = ""		&& Clear this variables avoiding Confusing mades by it.
    lcRpYrPrT     = ""		&& Clear this variables avoiding Confusing mades by it.
    =lfWReadRep('P')

  CASE lcRpDayPd = 'DAYS'

    laOGObjCnt[5]  = .T.
    laOGObjCnt[6]  = .T.
    laOGObjCnt[9]  = .F.
    laOGObjCnt[10] = .F.
    *-  Clear contents of fixcal calendar fields
    laOGFxFlt [1,6] = ''	&&E500153,1 Clear Contents of fisical year field
    laOGFxFlt [2,6] = ''	&&E500153,1 Clear Contents of period field

    =lfWReadRep('D')
ENDCASE

*E303229,1 TMI 10/02/2012 [Start] refresh OG
lfOGShowGet('LAOGFXFLT[1,6]')
lfOGShowGet('LAOGFXFLT[2,6]')
lfOGShowGet('LCRPFRDAY')
lfOGShowGet('LCRPTODAY')
lfOGShowGet('LCRPYRPRF')
lfOGShowGet('LCRPYRPRT')
loOgScroll.Refresh()

*- intiate the period from to values
LOCAL lnPos
lnPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'GLACBALS.CFSPPRDID'),1)
loOgScroll.laOgFxFlt[lnPos,6] = oAriaApplication.CurrentPeriod+'|'+oAriaApplication.CurrentPeriod

lnPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'GLACBALS.CFISFYEAR'),1)
loOgScroll.laOgFxFlt[lnPos,6] = oAriaapplication.CurrentYear


*!************************************************************************
*!
*!      Function lfvGrpCode
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
Function lfvGrpCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF
lnPos = lfGetPos('laOgFxFlt','GLGRPDT.CGRPCODE')

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = 'CGrpCode:H="Code",CGrplnHed:H="Description"'
lcBrFields    = "CGrpCode:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",AHEADERFILE))+;
				"',CGrplnHed:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,oAriaApplication.GetHeaderText("LANG_DESC",AHEADERFILE))+"'"
*N000682,1 MMT 02/10/2013 Globalization changes[End]
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
&& Check If year field is empty
    gcDataDir = oAriaApplication.DataDir
IF .NOT. EMPTY(&lcRpCurFld.[lnPos,6])
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.[lnPos,6]))
    lcOldAlias = ALIAS()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    IF NOT USED("GLGRPHD")
      SELECT 0
      USE &gcDataDir.GLGRPHD ORDER TAG grpcode
      llUesdBefo = .T.
    ENDIF
    SELECT GLGRPHD
    SET ORDER TO TAG grpcode
    *** Search for the current Group code
    IF ('?' $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6]))
      IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
        GOTO RECNO(0)
      ENDIF
      =gfBrows([],'CGrpCode',"laRpRetFld",'Group Codes ',.F.)
      loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      USE IN GLGRPHD
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF
  ENDIF
ENDIF
RETURN

*!************************************************************************
*!
*!      FUNCTION lfGetPic
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


*!************************************************************************
*!
*!      FUNCTION lfvDate
*!
*!************************************************************************
FUNCTION lfvDate

lcCurObj = loOgScroll.ActiveControl.Parent.Parent.cOgArray
loFld = loOgScroll.ActiveControl

IF EVAL(lcCurObj)<>loFld.OldValue

  IF !USED('FSPRD')
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FSPRD
  ENDIF
  SELECT FSPRD


  SET ORDER TO TAG COMFYRPRDI

   IF SEEK(ALLTRIM(STR(YEAR(EVAL(lccurobj)))))
    LOCATE REST WHILE  DFSPPBGDT<=EVAL(lcCurObj) ;
    FOR BETWEEN(EVAL(lcCurObj),DFSPPBGDT,DFSPPENDT)

    IF FOUND()
      lcWhichVar=SUBSTR(lcCurObj,5,1)
      lcRpYrPr&lcWhichVar = cfisfyear+cfspprdid
    ELSE
      WAIT 'INVALID DATE 1' WINDOW
    ENDIF
  ELSE
    WAIT 'INVALID DATE 2' WINDOW
    _CUROBJ=_CUROBJ
  ENDIF
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfGetDet
*!
*!************************************************************************
*
FUNCTION lfGetDet
PARAMETER llParam

IF PARAM()<>0
 lldone=.F.
 RETURN ''
ENDIF

IF lldone
  RETURN ""
ELSE
  lnBalance = IIF(EVAL(lcRpTrgFil+'.dtrnpdate')<lcRpFrDay,IIF(EVAL(lcRpTrgFil+'.cdrorcr')='D',EVAL(lcRpTrgFil+'.namount'),-EVAL(lcRpTrgFil+'.namount')),0)
  IF &lcRpTrgFil..DTRNPDATE >= lcRpFrDay
    lldone = .T.
    *N000682,1 MMT 02/10/2013 Globalization changes[Start]
    *RETURN 'Detail entries from '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance))
    RETURN IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAILENT,oAriaApplication.GetHeaderText("LANG_DETAILENT",AHEADERFILE))+' '+DTOC(lcRpFrDay)+' '+;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TO,oAriaApplication.GetHeaderText("LANG_TO",AHEADERFILE))+' '+DTOC(lcRpToDay)+' '+;
		   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGBAL,oAriaApplication.GetHeaderText("LANG_BEGBAL",AHEADERFILE))+' '+ALLTRIM(STR(lnBalance))
    *N000682,1 MMT 02/10/2013 Globalization changes[END]
  ELSE
    RETURN ""
  ENDIF
ENDIF

*!************************************************************************
*!
*!      FUNCTION : lfInit
*!
*!************************************************************************
*
FUNCTION lfInit

lldone = .F.

IF lcAcctCode <> GLACBALS.cAcctCode
  lnBalance  = GLACBALS.NACBOPBAL
ENDIF

lcOldAlias = ALIAS()
select glacbals
IF !EOF()
  go recno()
ENDIF

SELECT GLPTRNDT
IF !EOF()
  go recno()
ENDIF
lnSaveRec = RECNO()

IF lcAcctCode <> GLACBALS.cAcctCode

  SUM REST IIF(GLPTRNDT.cDrOrCr='D',GLPTRNDT.nAmount,-GLPTRNDT.nAmount)   ;
      WHILE CACCTCODE+CTRNPYR+CTRNPPRD = GLACBALS.cAcctCode ;
      FOR GLPTRNDT.dTrnPDate < lcRpFrDay AND GLPTRNHD.cTrnType <> 'B' TO lnTmp
  lnBalance = lnBalance + lnTmp

  lcAcctCode = GLACBALS.cAcctCode
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
  GO RECNO()
  IF BETWEEN(lnSaveRec, 1, RECCOUNT('GLPTRNDT'))
    GO lnSaveRec IN GLPTRNDT		&& Go to saved record in GLPTRNDT File
  ENDIF
ENDIF
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*RETURN 'Detail entries From '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance,10,2))
RETURN IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAILENT,oAriaApplication.GetHeaderText("LANG_DETAILENT",AHEADERFILE))+' '+DTOC(lcRpFrDay)+' '+;
       IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TO,oAriaApplication.GetHeaderText("LANG_TO",AHEADERFILE))+' '+DTOC(lcRpToDay)+' '+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGBAL,oAriaApplication.GetHeaderText("LANG_BEGBAL",AHEADERFILE))+' '+ALLTRIM(STR(lnBalance,10,2))
*N000682,1 MMT 02/10/2013 Globalization changes[ENd]
*!************************************************************************
*!
*!      FUNCTION : lfWReadRep
*!
*!************************************************************************
*
FUNCTION lfWReadRep
PARAMETERS lcDirect

IF lcDirect='P' AND !EMPTY(LAOGFXFLT[1,6]) AND !EMPTY(LAOGFXFLT[2,6])
  RETURN
ENDIF
IF lcDirect='D' AND !EMPTY(lcRpFrDay) AND !EMPTY(lcRpToDay)
  RETURN
ENDIF

lnCurAlias = SELECT()
llCompUsd=.f.
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF

SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF

IF llCompUsd
  USE IN SYCCOMP
ENDIF

IF lcDirect = 'P'
  LAOGFXFLT[1,6] = LEFT(lcRetVal,4)
  LAOGFXFLT[2,6] = RIGHT(lcRetVal,2)+loOgScroll.lcElmSep+RIGHT(lcRetVal,2)
ELSE

  IF !USED('FSPRD')
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FSPRD IN SELECT(1)
    llCompUsd=.T.
  ENDIF
  SELECT FSPRD


  SET ORDER TO TAG COMFYRPRDI
  IF SEEK(lcRetVal)
    lcRpFrDay=DFSPPBGDT
    lcRpToDay=DFSPPENDT

    lcRpYrPrF = cfisfyear+cfspprdid
    lcRpYrPrT = cfisfyear+cfspprdid
  ENDIF
  IF llCompUsd

    USE IN FSPRD

  ENDIF
ENDIF
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfMakeStru
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/06/97
*! Purpose   : Fill array with specific fields from specific file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcThisFile,lcThisFd
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfMakeStru("GLSUBJOR","CSRCJRNL,CJORLNDES")
*!*************************************************************
*-  This Function was added by MAB for the Enhancement
*!*************************************************************
*
FUNCTION lfMakeStru
PARAMETERS lcThisFile,lcThisFd

lnDelItem = 1
SELECT &lcThisFile
=AFIELDS(laTemp)

*Loop for required fields	
DO WHILE lnDelItem <= ALEN(laTemp,1)
  IF ! ( laTemp[lnDelItem,1] $ lcThisFd )
    = ADEL(laTemp,lnDelItem)
    IF ALEN(laTemp,1) != 1
      DIMENSION laTemp[ALEN(laTemp,1)-1,18]
    ENDIF
  ELSE   && Else
    lnDelItem = lnDelItem + 1
  ENDIF
ENDDO    &&END Loop for required fields

*- restore the array to the standard behaviour of Fox2.6 where there is only 4 columns
LOCAL laXTmp,i
DIMENSION laXTmp[ALEN(laTemp,1),4]
FOR i = 1 TO ALEN(laTemp,1)
  laXTmp[i,1] = laTemp[i,1]
  laXTmp[i,2] = laTemp[i,2]
  laXTmp[i,3] = laTemp[i,3]
  laXTmp[i,4] = laTemp[i,4]
ENDFOR
DIMENSION laTemp[ALEN(laTemp,1),4]
ACOPY(laXTmp,laTemp)
*End of lfMakeStru.

*!*	*-- End OF lfvActCod.
*:**************************************************************************
*:* Name        : lf1AcSlctd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/22/2003
*:* Purpose     : if one account is selected , copy the file glacbals to a temp file with
*:*             : only the account selected and rebuild all relations
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lf1AcSlctd()
*:***************************************************************************
FUNCTION lf1AcSlctd
lcSlct = SELECT()

lnAccPos = ASCAN(laOgVrFlt,"GLACCHAR.CACCTCODE ")
IF lnAccPos > 0
  lnRow = ASUBSCRIPT(laOgVrFlt,lnAccPos,1)
  IF !EMPTY(laOgVrFlt[lnRow,6])
    *-- Save relation
    SELECT GLACBALS
    lcRelation = SET('RELATION')
    lcSkip = SET('SKIP')
    SET RELATION TO
    SET RELATION TO CACCTCODE INTO GLACCHAR

    lcKey = PADR(laOgVrFlt[lnRow,6],24) + laOGFxFlt[1,6]+ ALLT(LEFT(laOGFxFlt[2,6],2))
    SEEK lcKey
    COPY TO (oAriaApplication.WorkDir+lcAcBals) WITH CDX ;
         WHILE CACCTCODE = laOgVrFlt[2,6] ;
         FOR EVAL(lcExpr)
    USE (oAriaApplication.WorkDir+lcAcBals) ORDER ACCYRPRD ALIAS GLACBALS

    *--Restore relations
    SET RELATION TO &lcRelation
    SET SKIP TO &lcSkip
    GO TOP
  ENDIF
ENDIF

SELECT (lcSlct)
*-- end of lf1AcSlctd.
*!************************************************************************


************************************************************
*! Name      : lfGetPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get array position
************************************************************
FUNCTION lfGetPos
PARAMETERS lcArr,lcFld
LOCAL lnPos
lnPos = ASUBSCRIPT(loOgScroll.&lcArr.,ASCAN(loOgScroll.&lcArr.,lcFld),1)
RETURN lnPos
*- End of lfGetPos.

************************************************************
*! Name      : lfDatesEnDs
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/02/2012
*! Purpose   : Enable or Disable dates in case of period / days
************************************************************
FUNCTION lfDatesEnDs
lnDayPd = ASCAN(laOGObjType,'LCRPDAYPD')
IF lnDayPd > 0
  lnDayPd = ASUBSCRIPT(laOGObjType,lnDayPd,1)
  laOGObjCnt[lnDayPd] = (lcRpSumDet = "D") AND (lcRpSortBy = "P")
  IF (lcRpSumDet = 'S') OR (lcRpSortBy <> "P")
    lcRPNote = 'N'
  ENDIF
ENDIF

= lfOGShowGet('LCRPDAYPD')
*- End of lfDatesEnDs.
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
************************************************************
*! Name      : lfGetTransTypes
*! Developer : Mariam Mazhar [MMT ]
*! Date      : 08/27/2015
*! Purpose   : Get Tran. types
************************************************************
FUNCTION lfGetTransTypes
PARAMETERS lcCACCTCODE ,lcCBATCHNO,lcCTRANNO
lnOldAliasSel = SELECT(0)
lcReturnValue = ""
IF !USED('GLDIST_TYPE')
  *B611145,1 MMT 05/15/2016 Fix bug of report slow performance at GMA[T20160311.0001][Start]
  *=gfOpenTable('GLDIST','GLDISTAC','SH','GLDIST_TYPE')
  =gfOpenTable('GLDIST','GLACCBTRN','SH','GLDIST_TYPE')
  *B611145,1 MMT 05/15/2016 Fix bug of report slow performance at GMA[T20160311.0001][End]  
ENDIF
SELECT GLDIST_TYPE
=gfSeek(lcCACCTCODE)
*E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][Start]
*!*	SCAN REST WHILE GLACCOUNT = lcCACCTCODE FOR glbatch = lcCBATCHNO AND ctrnsledn = lcCTRANNO
*!*	  lcTranType = lfTranType()
*!*	  IF !(lcTranType $ lcReturnValue)
*!*	    lcReturnValue = lcReturnValue + IIF(EMPTY(ALLTRIM(lcReturnValue)),""," - ")+lcTranType
*!*	  ENDIF
*!*	ENDSCAN
*!*	SELECT(lnOldAliasSel)
*!*	RETURN (lcReturnValue) 
*B611145,1 MMT 05/15/2016 Fix bug of report slow performance at GMA[T20160311.0001][Start]
*!*	SELECT SUM(neqvamnt) as 'TotAmount' ,Tran_Type FROM GLDIST_TYPE WHERE ;
*!*	       GLACCOUNT = lcCACCTCODE AND glbatch = lcCBATCHNO AND;
*!*	       ctrnsledn = lcCTRANNO GROUP BY Tran_Type INTO CURSOR 'AcctTrans'
if USED('AcctTrans')       
  USE IN 'AcctTrans'
ENDIF
CREATE CURSOR 'AcctTrans' (TotAmount N (17,2),Tran_Type C(2))  
Sele 'AcctTrans' 
INDEX ON Tran_Type TAG 'AcctTrans' 
Sele GLDIST_TYPE 
=gfseek(lcCACCTCODE +lcCBATCHNO+lcCTRANNO )
Scan Rest while GLACCOUNT+GLBATCH+CTRNSLEDN+TRAN_TYPE = lcCACCTCODE +lcCBATCHNO+lcCTRANNO 
  if !SEEK(GLDIST_TYPE.Tran_Type, 'AcctTrans' , 'AcctTrans' )
    INSeRT INTO  'AcctTrans' (Tran_Type) Values(GLDIST_TYPE.Tran_Type)
  ENDIF
  repl AcctTrans.TotAmount with AcctTrans.TotAmount + GLDIST_TYPE.neqvamnt in AcctTrans
ENDSCAN
*B611145,1 MMT 05/15/2016 Fix bug of report slow performance at GMA[T20160311.0001][End]       
SELECT 'AcctTrans'
LOCATE 
IF !EOF()
  llFirst = .T.
  SCAN 
    lcTransType = lfTranType()
    IF llFirst 
      llFirst = .F.
      SELECT (lcAccTmp)
      REPLACE TransType WITH lcTransType,;
              nAmount   WITH ABS(AcctTrans.TotAmount),;
              Cdrorcr    WITH IIF(AcctTrans.TotAmount < 0,'C','D')  IN (lcAccTmp)
    ELSE
      m.TransType = lcTransType
      m.nAmount   = ABS(AcctTrans.TotAmount )
      M.Cdrorcr   = IIF(AcctTrans.TotAmount  < 0,'C','D')
      INSERT INTO (lcAccTmp) FROM MEMVAR
   ENDIF
 ENDSCAN 
ENDIF       
SELECT(lnOldAliasSel)
*E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][End]
************************************************************
*! Name      : lfTranType
*! Developer : Mariam Mazhar [MMT ]
*! Date      : 08/27/2015
*! Purpose   : Get Tran. types Desc.
************************************************************
FUNCTION lfTranType
#INCLUDE R:\ARIA4XP\PRGS\SM\SMGLREL.H
lcTranType   =  ' '
DO CASE
CASE Tran_Type =  'IN'
  lcTranTYpe   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICE,loFormSet.GetHeaderText("LANG_INVOICE",loFormSet.HeaderAlias))
CASE Tran_Type =  'VI'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOID_INVOICE,loFormSet.GetHeaderText("LANG_VOID_INVOICE",loFormSet.HeaderAlias))
CASE Tran_Type =  'CR'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASH_RECEIPT,loFormSet.GetHeaderText("LANG_CASH_RECEIPT",loFormSet.HeaderAlias))
CASE Tran_Type =  'CA'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDIT_ADJUSTMENT,loFormSet.GetHeaderText("LANG_CREDIT_ADJUSTMENT",loFormSet.HeaderAlias))
CASE Tran_Type =  'DA'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEBIT_ADJUSTMENT,loFormSet.GetHeaderText("LANG_DEBIT_ADJUSTMENT",loFormSet.HeaderAlias))
CASE Tran_Type =  'RM'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RETURN_MERCHANDISE,loFormSet.GetHeaderText("LANG_RETURN_MERCHANDISE",loFormSet.HeaderAlias))
CASE Tran_Type =  'VR'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOID_RETURN,loFormSet.GetHeaderText("LANG_VOID_RETURN",loFormSet.HeaderAlias))
CASE Tran_Type =  'IP'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVENTORY_PHYSICAL,loFormSet.GetHeaderText("LANG_INVENTORY_PHYSICAL",loFormSet.HeaderAlias))
CASE Tran_Type =  'IA'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVENTORY_ADJUSTMENT,loFormSet.GetHeaderText("LANG_INVENTORY_ADJUSTMENT",loFormSet.HeaderAlias))
CASE Tran_Type =  'PO'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PO_RECEIVING,loFormSet.GetHeaderText("LANG_PO_RECEIVING",loFormSet.HeaderAlias))
CASE Tran_Type =  'CT'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CT_RECEIVING,loFormSet.GetHeaderText("LANG_CT_RECEIVING",loFormSet.HeaderAlias))
CASE Tran_Type =  'ZE'
  lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ZERO_OUT_STOCK,loFormSet.GetHeaderText("LANG_ZERO_OUT_STOCK",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'A '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DM_application,loFormSet.GetHeaderText("LANG_DM_application",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'B '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Bank_adjustment,loFormSet.GetHeaderText("LANG_Bank_adjustment",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'H '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cash_payments,loFormSet.GetHeaderText("LANG_Cash_payments",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'I '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AP_Invoice,loFormSet.GetHeaderText("LANG_AP_Invoice",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'M '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Manual_checks,loFormSet.GetHeaderText("LANG_Manual_checks",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'N '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Non_check_payment,loFormSet.GetHeaderText("LANG_Non_check_payment",loFormSet.HeaderAlias))
CASE TRAN_TYPE = 'P '
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Printed_checks,loFormSet.GetHeaderText("LANG_Printed_checks",loFormSet.HeaderAlias))
*E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][Start]
CASE TRAN_TYPE = 'KO'
  lcTranTYpe   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_KEY_OFF,loFormSet.GetHeaderText("LANG_KEY_OFF",loFormSet.HeaderAlias))
*E303620,1 MMT 11/24/2015 Modify GL Account reconciliation report to print trans. details[T20150716.0011][End]
ENDCASE
RETURN (lcTranTYpe)
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][End]