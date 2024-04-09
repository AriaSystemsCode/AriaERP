*:************************************************************************
*:
*: Procedure file: GLACCREC.RPR
*:               : Account reconciliation
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:12/05/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B500647,1 HESHAM 04/26/1995
*B600344,1 Reham On 05/28/95 Fix the upper case for : Acc. group
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600344,1 Reham On 06/11/95 Fix the group code validation.
*B801087,1 MAB   10/29/97 Fix the group code expression.
*E500153,1 MAB   10/29/97 1-Added the Option Sort by source journal
*E500153,1                2-Reprint account code if details overflow
*E500153,1                  one page 
*E500153,1                The following Changes was done 
*E500153,1                - Changes in this program
*E500153,1                - Changes in Reports GLACCREC and GLACCRED
*E500153,1                - Changes REPORT GENERATOR, i.e. Adding new options and variables. 
*E500153,1                - Adding two new reports GLACCRSC and GLACCRSD
*E300789,7 WALID (WAM) 03/04/99 make required modifications due to
*          removing ccomp_id field .
*B803801,1 ABD 11/08/2000 fixed bug 'Too many picture characters specified' 
*B803801,1 ABD            Account Reconcil when change the accoound code 
*B803801,1 ABD            From advanced filter from like to bettwen.
*B604162,1 KAM 01/02/2001 Sort Chart of Account by Account + Period
*B604370,1 AAN 22/04/2001 Enhance the performance of the report. 
*B604976,1 SSE 09/30/2001 Bug of displaying wrong data when selecting by Days.
*B604976,1                All modifications is in FRX (GLACCRED.FRX)
*B605370,1 AKA 12/20/2001 Fixing bug "Microsoft Foxpro Internal Consistency" 
*B605464,1 SSE 02/10/2002 Fix bug of changing begining balance when period is changed for one account.
*B605828,1 BWA 22/05/2002 Fix the bug of the filter "Account Type Code" doesn't work.
*B606615,1 TMI 12/12/2002 Fix the bug that Wrong Calculations - As a result of inclucluding the line of beggining balance within transactions
*B606728,1 TMI The criteria "llExpr" is evaluated in "lcExpr" in the scan on the file 
*B606728,1 TMI GLACBALS , Also it do not cover all cases in Advanced option for the account code
*B604462,1 TMI 01/21/2003 Fix bug that report is very slow when selecting an account
*B607315,1 ALB 06/15/2003 Adj. FRX to print properly with A4 and Letter page sizes.  
*:******************************************************************************************
*

*B605464,1 Define a new variable for saving Account Code. [Begin]
PRIVATE lcAcctCode
lcAcctCode = ''
*B605464,1 Define a new variable for saving Account Code. [End]
*B604462 TMI [Start] lcAcBals : variable holds temp file name used instead of GLACBALS in case if one account is selected
lcAcBals = gfTempName()
*B604462 TMI [End  ] 

*** Report Setup
*B604162,1 KAM set order for account code[start]
SET ORDER TO TAG ACCYRPRD IN GLACBALS
*B604162,1 KAM[end]
*E500153,1 Change this line to use the new variable lcRpDayPd
*IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6]))) AND;
*   lcRpForm = 'GLACCREC'

IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6]))) AND;
   lcRpDayPd = 'PERIODS'
*E500153,1 end   
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

*E500153,1 Change this line to use the new variable lcRpDayPd
*IF (EMPTY(DTOS(lcRpFrDay)) .OR. EMPTY(DTOS(lcRpToDay))) AND;
   lcRpForm = 'GLACCRED'
IF (EMPTY(DTOS(lcRpFrDay)) .OR. EMPTY(DTOS(lcRpToDay))) AND;
   lcRpDayPd = 'DAYS'
*E500153,1 end   
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

*E500153,1 Change this line to use the new variable lcRpDayPd
*IF lcRpForm = 'GLACCRED' AND (lcRpFrDay>lcRpToDay)
IF lcRpDayPd = 'DAYS' AND (lcRpFrDay > lcRpToDay)
*E500153,1 end     

  WAIT 'END DATE MUST GREATER THAN OR EQUAL TO BEGINING DATE' WINDOW
  RETURN
ENDIF

llDone     = .F.
lnBalance  = 0

*B801087,1 In all the next cases, The groups used must keep in mind the
*B801087,1 type of transaction (i.e: Debit or Credit) to have one record for
*B801087,1 each, thus we add field GLPTRNDT.CDRORCR to all groups. [Begin]
DO CASE
  CASE lcRpSumzBy = 'D'
    *B801087,1 Add the field GLPTRNDT.CDRORCR to the group
    *lcRPGrp=[GLPTRNDT.CACCTCODE,GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD,GLPTRNDT.DTRNPDATE] 
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNDT.DTRNPDATE ,] +;
              [ GLPTRNDT.CTRNPYR  , GLPTRNDT.CTRNPPRD , GLPTRNDT.CDRORCR] 
    *B801087,1
    lcOrderBy = [DTOS(DTRNPDATE)]
    *B801087,1
  CASE lcRpSumzBy = 'S'  
    *B801087,1 Add the field GLPTRNDT.CDRORCR to the group
    *lcRPGrp=[GLPTRNDT.CACCTCODE,GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD,GLPTRNhd.CSRCJRNL]   
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNhd.CSRCJRNL ,] +;
              [GLPTRNDT.CTRNPYR , GLPTRNDT.CTRNPPRD   , GLPTRNDT.CDRORCR]   
    *B801087,1
    lcOrderBy = [CSRCJRNL]
    *B801087,1
  CASE lcRpSumzBy = 'B'
    *B801087,1 Add the field GLPTRNDT.CDRORCR to the group
    *lcRPGrp=[GLPTRNDT.CACCTCODE,GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD,GLPTRNDT.DTRNPDATE,GLPTRNHD.CSRCJRNL]   
    lcRPGrp = [GLPTRNDT.CACCTCODE , GLPTRNHD.CSRCJRNL , GLPTRNDT.DTRNPDATE ,]+;
              [GLPTRNDT.CTRNPYR   , GLPTRNDT.CTRNPPRD , GLPTRNDT.CDRORCR   ]   
    *B801087,1
    lcOrderBy = [CSRCJRNL+DTOS(DTRNPDATE)]
    *B801087,1
ENDCASE
*B801087,1 In all the next cases, [End]

IF lcRpSumzBy<>'N'
  IF lcRpForm = 'GLACCRED'
    *B801087,1 We need to get transactions between Begining date of period
    *B801087,1 and From date to calculate the Begin balance
    *B801087,1 Note that the Reports [GLACCRED.FRX , GLACCRSD.FRX]
    *B801087,1 Prints For dates Greater than or Equal lcRpFrDay Only.
    *E500153,1 Include source journal in the report expression
    *lcWhere = "BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)"
    *B606615,1 TMI [Start] The date must be "from to "
    *lcWhere = "GLPTRNDT.DTRNPDATE <= lcRpToDay" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")
    lcWhere = "BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")              
    *B606615,1 TMI [End  ]               
  ELSE 
    *E500153,1 Variable assignments
    lcRpYrPrF = laOGFxFlt[1,6]+LEFT(laOGFxFlt[2,6],2)	&&Assign From period
    lcRpYrPrT = laOGFxFlt[1,6]+RIGHT(laOGFxFlt[2,6],2)  &&Assign To period
    *E500153,1 Include source journal in the report expression
    *lcWhere = "BETWEEN(GLPTRNDT.CTRNPYR+GLPTRNDT.CTRNPPRD,lcRpYrPrF,lcRpYrPrT)"
    lcWhere = "BETWEEN(GLPTRNDT.CTRNPYR+GLPTRNDT.CTRNPPRD,lcRpYrPrF,lcRpYrPrT)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")
    *E500153,1 end
  ENDIF

  *B801087,1 Change this line Because we want have one recored for each
  *B801087,1 transaction ,Note that the Sum used in SELECT SQL is unlogical
  *B801087,1 because we want Sum of all debit in one line and sum of all
  *B801087,1 credit in the another line. [BEGIN]
  *SELECT GLPTRNDT.cdrorcr,GLPTRNDT.DTRNPDATE,GLPTRNDT.CACCTCODE,GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD,IIF(GLPTRNDT.cDRORCR='D',;
  * SUM(GLPTRNDT.NAMOUNT),SUM(-GLPTRNDT.NAMOUNT)) AS nAmount,;
  * GLPTRNhd.CSRCJRNL FROM &gcDataDir.GLPTRNDT,&gcDataDir.GLPTRNhd;
  *  WHERE GLPTRNDT.cbatchno + GLPTRNDT.ctranno = GLPTRNHD.cbatchno + GLPTRNHD.ctranno AND GLPTRNhd.CTRNTYPE<>'B';
  *        AND &lcWhere ; 
  *  GROUP BY &lcRPGrp INTO DBF (gcWorkDir+lcRpTargt)

 *SELECT GLPTRNDT.cdrorcr , GLPTRNDT.DTRNPDATE , GLPTRNDT.CACCTCODE ,;
         GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD                         ,;
         SUM(IIF(GLPTRNDT.CDRORCR = "D" , GLPTRNDT.NAMOUNT          ,;
         -GLPTRNDT.NAMOUNT)) AS nAmount , GLPTRNhd.CSRCJRNL          ;
    FROM &gcDataDir.GLPTRNDT,&gcDataDir.GLPTRNHD                     ;
   WHERE GLPTRNDT.cbatchno + GLPTRNDT.ctranno = GLPTRNHD.cbatchno +  ;
         GLPTRNHD.ctranno AND GLPTRNHD.CTRNTYPE <> 'B' AND &lcWhere  ; 
   GROUP BY &lcRPGrp INTO DBF (gcWorkDir+lcRpTargt)

  SELECT GLPTRNDT.cdrorcr , GLPTRNDT.DTRNPDATE , GLPTRNDT.CACCTCODE ,;
         GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD                         ,;
         SUM(GLPTRNDT.NAMOUNT) AS nAmount , GLPTRNhd.CSRCJRNL          ;
    FROM &gcDataDir.GLPTRNDT,&gcDataDir.GLPTRNHD                     ;
   WHERE GLPTRNDT.cbatchno + GLPTRNDT.ctranno = GLPTRNHD.cbatchno +  ;
         GLPTRNHD.ctranno AND GLPTRNHD.CTRNTYPE <> 'B' AND &lcWhere  ; 
   GROUP BY &lcRPGrp                                                 ;
   INTO DBF (gcWorkDir+lcRpTargt)
  *B801087,1 Adjust the previous line [END]

  *B801087,1 Adjust the Next line to order data by selected summarization
  *INDEX ON CACCTCODE+CTRNPYR+CTRNPPRD TAG TmpTag
  INDEX ON CACCTCODE+CTRNPYR+CTRNPPRD+&lcOrderBy TAG TmpTag
  *B801087,1 Adjust the Next line  [end.]
  
  *E500153,1 Remove the following statement because it makes no sense,
  *E500153,1 And we adjust SQL SELECT to include true values.
  *REPLACE ALL cdrorcr WITH IIF(NAMOUNT<0,'C','D'),;
              NAMOUNT WITH ABS(NAMOUNT)
  *E500153,1 Remove the following statement [end.]

  SELECT GLACBALS
  SET RELATION TO glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid INTO &lcRpTargt ADDITIVE
ENDIF

lcRpTrgFil = IIF(lcRpSumzBy<>'N',lcRpTargt,'GLPTRNDT')

lcRpExp=IIF(lcRpExp='.T.','',lcRpExp)

IF lcRpSumzBy='N'
  *E500153,1 Include source journal in the report expression
  *lcRpExp= lcRpExp+IIF(!EMPTY(lcRpExp),'AND ','')+[GLPTRNhd.CTRNTYPE<>'B']
  lcRpExp= lcRpExp+IIF(!EMPTY(lcRpExp),'AND ','')+[GLPTRNhd.CTRNTYPE<>'B']+;
           IIF(!EMPTY(lcRpSjGrop),[ AND GLPTRNHD.CSRCJRNL = lcRpSjGrop ]," ")           
  *E500153,1 end           
ENDIF

*E500153,1 Change this line to use the new variable lcRpDayPd
*IF lcRpForm = 'GLACCRED'
IF lcRpDayPd = 'DAYS'
*E500153,1 end     
  lcExpr=lcRpExp+IIF(!EMPTY(lcRpExp),'AND ','')+'BETWEEN(GLACBALS.CFISFYEAR+GLACBALS.cfspprdid,lcRpYrPrF,lcRpYrPrT)'+;
         'AND &lcRpTrgFil..dtrnPdate <= lcRpToDay'
ELSE
  lcExpr=lcRpExp       
ENDIF

IF 'CGRPCODE'$ lcRpExp
  SELECT GLGRPDT  
  SET ORDER TO TAG ACCYRPRD IN GLACBALS
  SET RELATION TO glgrpdt.cacctcode INTO GLACBALS ADDITIVE
  *B606615,1 TMI [Start] Set a filter to only included Accounts in a group
  SET FILTER TO CGRDSTAT  = 'I'
  GO TOP
  *B606615,1 TMI [End  ] 

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

*E500153,1 Added this lines to collect data
*E500153,1 in case of sort by source journal [BEGIN]

*E500153,1 Summarization is (N/A) Not Applicable and Sort by source journal
IF lcRpSumzBy='N' AND llRpGrBySJ
  lnAliass = SELECT(0)		&&Save ALIAS before continue, to reuse it for data collecting.

  SET ORDER TO TAG SRCJRNL IN GLSUBJOR

  DIMENSION laFileStru[1,4],laTemp[1,4],laFileFlds[4,2]
  STORE SPACE(0) TO laFileStru,laTemp

  *E500153,1 Files used in collecting data
  laFileFlds[1,1] = 'GLACBALS'
  laFileFlds[2,1] = 'GLACCHAR'
  laFileFlds[3,1] = 'GLPTRNDT'
  laFileFlds[4,1] = 'GLSUBJOR'

  *E500153,1 Fields used in collecting data
  laFileFlds[1,2] = 'CACCTCODE,CFSPPRDID,CFISFYEAR,NACBOPBAL'
  laFileFlds[2,2] = 'CACCNLDES'
  laFileFlds[3,2] = 'CBATCHNO,CTRANNO,CTRDTEXP,DTRNPDATE,NAMOUNT,CDRORCR'
  laFileFlds[4,2] = 'CSRCJRNL,CJORLNDES'

  *E500153,1 [BEGIN] The aim is to:
  *E500153,1 construct the temporary table from related 
  *E500153,1 tables, to be easy for any one to increase or decrease it.  
  *E500153,1 Fill structure array calling a function called lfMakeStru, with 
  *E500153,1 two parameters,1st is file name and 2nd is fields required.  
  lnElement = 1 			&& To have the start position in the structure array.
  FOR lnItems = 1 TO ALEN(laFileFlds,1)
    =lfMakeStru(laFileFlds[lnItems,1],laFileFlds[lnItems,2])
    lnDim = IIF(ALEN(laFileStru,1)=1,ALEN(laTemp,1) ,;
                ALEN(laFileStru,1)+ALEN(laTemp,1))  &&Redimension the array for structure array.
    DIMENSION laFileStru[lnDim,4]
    =ACOPY(laTemp,laFileStru,1,-1,AELEMENT(laFileStru,lnElement,1))
    lnElement = lnElement + ALEN(laTemp,1)
  ENDFOR  

  *E500153,1 IF it is a Days case, Add a field Contains description of
  *E500153,1 begin balance.
  IF lcRpDayPd = 'DAYS'  
    lnFileStru = ALEN(laFileStru,1)+1
    DIMENSION laFileStru[lnFileStru, 4]
    laFileStru[lnFileStru ,1] = 'cSt_Bal'
    laFileStru[lnFileStru ,2] = 'C'
    laFileStru[lnFileStru ,3] = 78
    laFileStru[lnFileStru ,4] = 0
  ENDIF

  lcTempFile = gfTempName()      && Variable to hold Temp. name to create cursor
  CREATE CURSOR (lcTempFile) ;
         FROM ARRAY laFileStru

  INDEX ON cSRcjrnl+CACCTCODE+CFISFYEAR+CFSPPRDID TAG (lcTempFile)

  *E500153,1 The aim is to:[END]
  lcScanExpr = IIF(EMPTY(lcExpr)                                     ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.)'        ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.) AND ') + ;
               lcExpr		&&Expression used for scanning last open alias

  lcOldAcct = ''		&& E500153,1  Used to compare with Account Code	
  
  SELECT (lnAliass)		&&Using the last alias used before creating CURSOR
  SCAN &lcScanExpr
    *E500153,1 IF By Days and the Account Code has changed
    IF (lcRpDayPd = 'DAYS') AND (lcOldAcct != CACCTCODE)
      *B606615,1 TMI [Start] Save Account code
      lcAccCode = CACCTCODE 
      *B606615,1 TMI [End  ] 
      lcSt_Bal = lfInit()      && calculate Begining balance
      lcOldAcct = CACCTCODE	   && Store Account Code Value
      *B606615,1 TMI [Start] relocate pointer in selected file after calling lfInit() fn.
      LOCATE &lcScanExpr AND CACCTCODE = lcAccCode
      *B606615,1 TMI [End  ] 
    ENDIF    && End of IF

    *E500153,1 Fill all required fields from corresponding tables.
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

    *E500153,1 IF By Days
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
*E500153,1 ADDED BLOCK BY MAB [END]

*B604370,1 AAN Collect the data in a new tmp. file in the case there is no summarize[Begin]
IF lcRpSumzBy='N' AND !llRpGrBySJ
  lcAccTmp = gfTempName()
  lcRpTrgFil = lcAccTmp
  lcTrgFil2 = lcAccTmp
  lcAlias = ALIAS()
  SELECT GLPTRNDT
  COPY STRUCTURE TO (gcWorkDir+lcAccTmp) WITH CDX
  USE (gcWorkDir+lcAccTmp) IN 0 EXCLUSIVE  
  SELECT (lcAccTmp)
  SET ORDER TO TAG Acctcode 
  SET RELATION TO &lcAccTmp..cacctcode+&lcAccTmp..ctrnpyr+&lcAccTmp..ctrnpprd INTO GLACBALS ADDITIVE

  SELECT GLGRPDT
  SET RELATION TO
  SET ORDER TO TAG Grcodacc 

  SELECT GLACBALS

  *B605370,1 AKA  Fixing bug "Microsoft Foxpro Internal Consistency" [Start]
  SET SKIP TO 
  *B605370,1 Fixing bug "Microsoft Foxpro Internal Consistency"  [End]
  
  IF 'CGRPCODE'$ lcRpExp
    lcstat = "GLGRPDT.CGRPCODE = "+'"'+laOGFxFlt[3,6]+'"'
    lcExpr = STRTRAN(lcExpr,lcstat,".T.")
  ENDIF
  
  WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT

  IF lcRpDayPd="PERIODS"  
    SET ORDER TO FisfYear
    =SEEK(laOgFxFlt[1,6],'GLACBALS')
    *B605370,1 AKA  Fixing bug "Microsoft Foxpro Internal Consistency" [Start]
    *SCAN REST WHILE cFisfYear+cFspprdid = laOgFxFlt[1,6] FOR &lcExpr AND IIF(!EMPTY(laOGFxFlt[3,6]),SEEK(laOGFxFlt[3,6]+GLACBALS.cAcctCode,'GLGRPDT'),.T.)

    *B605828,1 BWA 22/05/2002 Fix the bug of the filter "Account Type Code" doesn't work.[SATRT]
    *SCAN REST WHILE cFisfYear+cFspprdid = laOgFxFlt[1,6]
    SCAN REST WHILE cFisfYear+cFspprdid = laOgFxFlt[1,6] FOR &lcExpr
    *B605828,1 BWA.[END]

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
    *B605370,1 AKA  Fixing bug "Microsoft Foxpro Internal Consistency" [End]
      *B606728 TMI [Start] The criteria "llExpr" is evaluated in "lcExpr" in the scan on the file 
      *B606728 TMI         GLACBALS , Also it do not cover all cases in Advanced option for the account code
      *IF llCont AND llExpr 
      IF llCont 
      *B606728 TMI [End  ] 
        WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
        SELECT GLPTRNDT
        *B606615,1 TMI [Start] Exclude beggining balance line
        *SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd
        SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd ;
                  FOR GLPTRNHD.CTRNTYPE<>'B'
        *B606615,1 TMI [End  ] 
          SCATTER MEMVAR MEMO
          INSERT INTO (lcAccTmp) FROM MEMVAR
        ENDSCAN 
      ENDIF
      
      SELECT GLACBALS
    ENDSCAN
    SELECT GLACBALS
    SET ORDER TO AccYrPrd
  ELSE    
    SCAN FOR &lcExpr  AND IIF(!EMPTY(laOGFxFlt[3,6]),SEEK(laOGFxFlt[3,6]+GLACBALS.cAcctCode,'GLGRPDT'),.T.)
      WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
      SELECT GLPTRNDT
      *B606615,1 TMI [Start] Check that the record is not a Begginig balance one
      *SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd
      SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd ;
                FOR GLPTRNHD.CTRNTYPE<>'B' AND BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)
        *B606615,1 TMI [End  ] 
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
  *B606615,1 TMI [Start] if summarization select the correct temp file with skip is set only to one file
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
    *B604462,1 TMI [Start] if one account is selected , copy the file glacbals to a temp file with  
    *B604462,1             only the account selected and rebuild all relations 
    =lf1AcSlctd()
    *B604462,1 TMI [End  ] 
  ENDIF
  *B606615,1 TMI [End  ] 
  DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcExpr),'FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.)','FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.) AND ')+lcExpr
  *B604462,1 TMI [Start] remove file (gcWorkDir+lcAcBals) if found
  *B604462,4 TMI [Start] Ask about 'GLACBALS' not lcAcBals
  *IF USED(lcAcBals)  
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON 
  IF USED('GLACBALS') AND UPPER(gcWorkDir) $ DBF('GLACBALS')
    USE IN GLACBALS
    ERASE (gcWorkDir+lcAcBals+'.DBF')
    ERASE (gcWorkDir+lcAcBals+'.CDX')
    =gfOpenFile(gcDataDir+'GLACBALS','ACCYRPRD','SH')
  ENDIF
  SET FULLPATH &lcFullPath
  *B604462,1 TMI [End  ] 
ENDIF
*B604370,1 AAN Collect the data in a new tmp. file in the case there is no summarize[End]

*B604370,1 AAN Remove the following line[Begin]
*DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcExpr),'FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.)','FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.) AND ')+lcExpr
*B604370,1 AAN Remove the following line[End]

lcExpr = ""
IF lcRpSumzBy<>'N'
   USE IN ALIAS(lcRpTargt)
   ERASE (gcWorkDir+lcRpTargt+'.BDF')
   ERASE (gcWorkDir+lcRpTargt+'.CDX')

ELSE    && Else  IF Summerization is N/A and Group By S.J.
  *B604370,1 AAN Remove the Temp. file[Begin]
  *IF llRpGrBySJ
    *USE IN (lcTempFile) 		&& Note that it is a cursor.
  *ENDIF
  IF llRpGrBySJ
    USE IN (lcTempFile) 		&& Note that it is a cursor.
  ELSE
    USE IN ALIAS(lcAccTmp)
    ERASE (gcWorkDir+lcAccTmp+'.BDF')
    ERASE (gcWorkDir+lcAccTmp+'.CDX')
  ENDIF
  *B604370,1 AAN Remove the Temp. file[End]

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
    
*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*IF USED(lcRpTargt)
*  USE IN ALIAS(lcRpTargt)
*  ERASE (gcWorkDir+lcRpTargt+'.BDF')
*  ERASE (gcWorkDir+lcRpTargt+'.CDX')
*  ERASE (gcWorkDir+lcRpTargt+'.FPT')
*ENDIF
*glEscPrs = .F.

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
  *  USE (gcSysHome+'SYCFISHD') ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE (gcDataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *** Search for the current company+year
    *E300789,7 ADJUST SEEK COMMAND [BEGIN
    *IF ('?' $ &lcRpCurFld. .OR. !SEEK(gcPrnt_Cmp+lcRpFicsYr)) 
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr)) 
    *E300789,7 ADJUST SEEK COMMAND [END
    
        *E300789,7 [BEGIN]
       * =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)     
        *E300789,7 [END..]
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  
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
    
    *** Search for the current company+year+Prd

    *E300789,7 [BEGIN] REMOVE gcAct_comp FROM SEEK FUNCTION

    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcAct_comp+ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
    *    =gfBrows([gcAct_comp+ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
      =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)

    *E300789,7 [END..]

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

*E500153,1  Transfer assigning of lcRpForm in this valid function ,
*E500153,1  rather than using valid entries, because 
*E500153,1  it affected with changes in three options.
*E500153,1  A -] Activity by   (Periods - Days)
*E500153,1  B -] Summarization (N/A - DATE - S.J. - DATE/S.J.)
*E500153,1  C -] Sort By S.J.  (YES - NO)
*E500153,1  Note: This valid function is identical for the three options.

*E500153,1  IF Summarization is (N/A) and (Sorted By S.J.) 
IF lcRpSumzBy='N' AND llRpGrBySJ
  lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCRSC','GLACCRSD')

ELSE	&&Summarization is not (N/A) OR not (Sorted By S.J.) 	
  lcRpForm   = IIF(lcRpDayPd = 'PERIODS','GLACCREC','GLACCRED')
ENDIF

laOGObjCnt[3] = IIF(lcRpSumzBy='N',.T.,.F.) 	&&IF Summarization !(N/A),DISABLE (Group By S.J.) OPTION

DO CASE
  *E500153,1 Change this line to use the new variable lcRpDayPd
  *CASE lcRpForm = 'GLACCREC'
  CASE lcRpDayPd = 'PERIODS'

    *E500153,1 Change this lines to adjust the array elements used to
    *E500153,1 control the Enabling and Disabling of options in OPTION GRID.
    *laOGObjCnt[4] = .F.    && From Day
    *laOGObjCnt[5] = .F.    && To Day
    *laOGObjCnt[7] = .T.    && Fiscal year
    *laOGObjCnt[8] = .T.    && Period
    laOGObjCnt[5]  = .F.    && From Day
    laOGObjCnt[6]  = .F.    && To Day
    laOGObjCnt[9]  = .T.    && Fiscal year
    laOGObjCnt[10] = .T.    && Period
    *E500153,1 end
    lcRpFrDay     = {  /  /    }
    lcRpToDay     = {  /  /    }
    lcRpYrPrF     = ""		&& Clear this variables avoiding Confusing mades by it.
    lcRpYrPrT     = ""		&& Clear this variables avoiding Confusing mades by it.
    =lfWReadRep('P')    

  *E500153,1 Change this line to use the new variable lcRpDayPd
  *CASE lcRpForm = 'GLACCRED'
  CASE lcRpDayPd = 'DAYS'
    
    *E500153,1 Change this lines to adjust the array elements used to
    *E500153,1 control the Enabling and Disabling of options in OPTION GRID.
    *laOGObjCnt[4] = .T.
    *laOGObjCnt[5] = .T.
    *laOGObjCnt[7] = .F.
    *laOGObjCnt[8] = .F.
    laOGObjCnt[5]  = .T.
    laOGObjCnt[6]  = .T.
    laOGObjCnt[9]  = .F.
    laOGObjCnt[10] = .F.
    *E500153,1 Clear contents of fixcal calendar fields
    laOGFxFlt [1,6] = ''	&&E500153,1 Clear Contents of fisical year field
    laOGFxFlt [2,6] = ''	&&E500153,1 Clear Contents of period field
	*E500153,1 end
    =lfWReadRep('D')
ENDCASE
=lfActvateWin(lnWinDisp)

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
lcRpCurFld    = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(&lcRpCurFld.) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
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
    *B600344,1 Remove the alltrim from the condition to seek with entered value.
    *IF ('?' $ &lcRpCurFld. .OR. !SEEK(ALLTRIM(&lcRpCurFld.)))
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.))
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
RETURN 

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
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*SELECT SYCACCOD
IF NOT USED("ACCOD")
  SELECT 0
  *E300789,7 [BEGIN] OPEN WITHOUT ORDER
  *USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD 
  *E300789,7 [END..] 
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]

*E300789,7 [BEGIN] The following code will be commented till *E300789,7 [END..]
*to reflect the changes on ACCOD structure (i.e removing ccomp_id field)
*IF SEEK(gcAct_Comp)
  
*  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
*  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
*  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
*  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
*  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
*  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
*ELSE
*  lcRpSegMas = " "
*ENDIF
*E300789,7 [BEGIN] Start the new code
GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
*E300789,7 [END..] End the new code

IF llUesdBefo       && .F.- this file used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *USE IN SYCACCOD
  USE IN ACCOD
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas

*!************************************************************************
*!
*!      FUNCTION lfWOldVal
*!
*!************************************************************************
*B500647,1 Closed the function because it was exist before
*FUNCTION lfWOldVal
*lcOldVal=EVAL(VARREAD())

*!************************************************************************
*!
*!      FUNCTION lfvDate
*!
*!************************************************************************
FUNCTION lfvDate

lcCurObj=VARREAD()
*B500647,1 Changed the var lcOldVal to lcRpOld because this is the var 
*B500647,1 defined in function lfwOldVal
*IF EVAL(lcCurObj)<>lcOldVal
IF EVAL(lcCurObj)<>lcRpOld
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF !USED('SYCFSPRD')
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD 
  *ENDIF
  *SELECT SYCFSPRD 
  IF !USED('FSPRD')
    SELECT 0
    USE &gcDataDir.FSPRD 
  ENDIF
  SELECT FSPRD 
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  SET ORDER TO TAG COMFYRPRDI
  *E300789,7 [BEGIN] REMOVE CCOMP_ID FIELD
  *IF SEEK(gcAct_Comp+alltrim(str(year(eval(lccurobj)))))
  *  LOCATE REST WHILE CCOMP_ID+CFISFYEAR+CFSPPRDID=gcAct_Comp AND DFSPPBGDT<=EVAL(lcCurObj) ;
    FOR BETWEEN(EVAL(lcCurObj),DFSPPBGDT,DFSPPENDT)
   
   IF SEEK(ALLTRIM(STR(YEAR(EVAL(lccurobj)))))
    LOCATE REST WHILE  DFSPPBGDT<=EVAL(lcCurObj) ;
    FOR BETWEEN(EVAL(lcCurObj),DFSPPBGDT,DFSPPENDT) 
  *E300789,7 [END]
    
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
    RETURN 'Detail entries from '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance))
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

*B605464,1 Check if Account Code is changed get its open balance. [Begin]
*lnBalance  = GLACBALS.NACBOPBAL
IF lcAcctCode <> GLACBALS.cAcctCode
  lnBalance  = GLACBALS.NACBOPBAL  
ENDIF
*B605464,1 Check if Account Code is changed get its open balance. [End]

lcOldAlias = ALIAS()
select glacbals
IF !EOF()
  go recno()
ENDIF  

SELECT GLPTRNDT
IF !EOF()
  go recno()
ENDIF  
*B801087,1 Save record to go again after sum is done
lnSaveRec = RECNO()
*B801087,1 Save record [end.]
*WAIT WINDOW "my renee " + DTOC(lcRpFrDay)

*B605464,1 Check if Account Code is changed get its open balance. [Begin]
IF lcAcctCode <> GLACBALS.cAcctCode
*B605464,1 Check if Account Code is changed get its open balance. [Begin]

  SUM REST IIF(GLPTRNDT.cDrOrCr='D',GLPTRNDT.nAmount,-GLPTRNDT.nAmount)   ;
      WHILE CACCTCODE+CTRNPYR+CTRNPPRD = GLACBALS.cAcctCode ;
      FOR GLPTRNDT.dTrnPDate < lcRpFrDay AND GLPTRNHD.cTrnType <> 'B' TO lnTmp 
  lnBalance = lnBalance + lnTmp

*B605464,1 Assign the new account code to this variable. [Begin]
  lcAcctCode = GLACBALS.cAcctCode
ENDIF
*B605464,1 Assign the new account code to this variable. [End]

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
  GO RECNO()
  *B801087,1 Add line
  IF BETWEEN(lnSaveRec, 1, RECCOUNT('GLPTRNDT'))
    GO lnSaveRec IN GLPTRNDT		&& Go to saved record in GLPTRNDT File
  ENDIF
  *B801087,1 Add line [end.]
ENDIF  
*B604462,1 TMI 01/21/2003 (Begin) Fix the bug of not printing decimals
*RETURN 'Detail entries From '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance))    
RETURN 'Detail entries From '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance,10,2))    
*B604462,1 TMI 01/21/2003 (End)

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
IF SEEK(gcAct_Comp,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF

IF llCompUsd
  USE IN SYCCOMP
ENDIF

IF lcDirect = 'P'
  LAOGFXFLT[1,6] = LEFT(lcRetVal,4)
  LAOGFXFLT[2,6] = RIGHT(lcRetVal,2)+lcElmSep+RIGHT(lcRetVal,2)  
ELSE  
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF !USED('SYCFSPRD')
  *  USE &gcSyshome.SYCFSPRD IN SELECT(1)
  *  llCompUsd=.T.
  *ENDIF
  *SELECT SYCFSPRD
  IF !USED('FSPRD')
    USE &gcDataDir.FSPRD IN SELECT(1)
    llCompUsd=.T.
  ENDIF
  SELECT FSPRD
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  SET ORDER TO TAG COMFYRPRDI 
  *E300789,7 [BEGIN] REMOVE gcAct_Comp FROM SEEK FUNCTION
  *IF SEEK(gcAct_Comp+lcRetVal)
  IF SEEK(lcRetVal)
  *E300789,7 [END..]
    lcRpFrDay=DFSPPBGDT
    lcRpToDay=DFSPPENDT

    *B801087,1 Added this lines to Restore the Default Values of lcRpYrPrF
    *B801087,1 and lcRpYrPrT corressponding to Default Dates [Begin]
    lcRpYrPrF = cfisfyear+cfspprdid
    lcRpYrPrT = cfisfyear+cfspprdid
    *B801087,1 Added this lines to Restore the Default Values [End]
  ENDIF 
  IF llCompUsd
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
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
*E500153,1 This Function was added by MAB for the Enhancement
*!*************************************************************
*
FUNCTION lfMakeStru
PARAMETERS lcThisFile,lcThisFd

lnDelItem = 1
SELECT &lcThisFile
= AFIELDS(laTemp)

*Loop for required fields	
DO WHILE lnDelItem <= ALEN(laTemp,1)
  IF ! ( laTemp[lnDelItem,1] $ lcThisFd )
    = ADEL(laTemp,lnDelItem)
    IF ALEN(laTemp,1) != 1
      DIMENSION laTemp[ALEN(laTemp,1)-1,4]
    ENDIF  
  ELSE   && Else
    lnDelItem = lnDelItem + 1
  ENDIF
ENDDO    &&END Loop for required fields

*!*************************************************************
*! Name      : lfvActCod
*! Developer : Abdou ElGendi
*! Date      : 11/08/2000
*! Purpose   : Get order and browse file.
*! Job No.   : B803801,1
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvActCod()
*!*************************************************************
*B803801,1 ABD Account Reconcil when change the accoound code 
*B803801,1 ABD From advanced filter from like to bettwen.

FUNCTION lfvActCod
PRIVATE lcOldOrder
lcOldOrder = ''
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

IF !EMPTY(&lcRpCurFld)
  SELECT GLACCHAR
  lcOldOrder = ORDER()
  SET ORDER TO ACCTCODE
  IF "?" $ &lcRpCurFld .OR. !SEEK(&lcRpCurFld)
    =gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  SET ORDER TO &lcOldOrder
ENDIF

lcBrFields = lcOldBrFld
*-- End OF lfvActCod.
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
*B604462,1
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
    *B604462,4 TMI [Start] Set the relation between GLACBALS AND GLACCHAR
    SET RELATION TO CACCTCODE INTO GLACCHAR
    *B604462,4 TMI [End  ] 
    
    lcKey = PADR(laOgVrFlt[lnRow,6],24) + laOGFxFlt[1,6]+ ALLT(LEFT(laOGFxFlt[2,6],2))
    SEEK lcKey    
    COPY TO (gcWorkDir+lcAcBals) WITH CDX ;
         WHILE CACCTCODE = laOgVrFlt[2,6] ;
         FOR EVAL(lcExpr) 
    USE (gcWorkDir+lcAcBals) ORDER ACCYRPRD ALIAS GLACBALS
    
    *--Restore relations
    SET RELATION TO &lcRelation
    SET SKIP TO &lcSkip    
    GO TOP        
  ENDIF  
ENDIF

SELECT (lcSlct)
*-- end of lf1AcSlctd.
*!************************************************************************


