*:***************************************************************************
*: Program file  : GLACCMUL.RPR
*: Program desc. : CUSTOMIZED ACCOUNT RECONCILIATION FOR MULBERRY.
*: Date          : 01/16/2003
*: System        : Aria Advantage Series.
*: Module        : GENERAL LEDGER
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : 
*: Tracking Job Number: C102795
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO GLACCMUL
*:***************************************************************************
*: Modifications :
***************************************************************************

*--Section to Initial the vaariables.
PRIVATE lcAcctCode
lcAcctCode = ''
lnSubTotal = 0
lnSubClose = 0
lcAcCode = ""

*--Report Setup.
SET ORDER TO TAG ACCYRPRD IN GLACBALS
IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6]))) AND;
   lcRpDayPd = 'PERIODS'
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

IF (EMPTY(DTOS(lcRpFrDay)) .OR. EMPTY(DTOS(lcRpToDay))) AND;
   lcRpDayPd = 'DAYS'
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

IF lcRpDayPd = 'DAYS' AND (lcRpFrDay > lcRpToDay)
  WAIT 'END DATE MUST GREATER THAN OR EQUAL TO BEGINING DATE' WINDOW
  RETURN
ENDIF

llDone    = .F.
lnBalance = 0

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

IF lcRpSumzBy <> 'N'
  IF lcRpForm = 'GLMULRED'
    lcWhere = "BETWEEN(GLPTRNDT.DTRNPDATE,lcRpFrDay,lcRpToDay)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")              
  ELSE 
    lcRpYrPrF = laOGFxFlt[1,6]+LEFT(laOGFxFlt[2,6],2)	&&Assign From period
    lcRpYrPrT = laOGFxFlt[1,6]+RIGHT(laOGFxFlt[2,6],2)  &&Assign To period
    lcWhere = "BETWEEN(GLPTRNDT.CTRNPYR+GLPTRNDT.CTRNPPRD,lcRpYrPrF,lcRpYrPrT)" +;
              IIF(!EMPTY(lcRpSjGrop)," AND GLPTRNHD.CSRCJRNL = lcRpSjGrop "," ")
  ENDIF

  SELECT GLPTRNDT.cdrorcr , GLPTRNDT.DTRNPDATE , GLPTRNDT.CACCTCODE ,;
         GLPTRNDT.CTRNPYR,GLPTRNDT.CTRNPPRD                         ,;
         SUM(GLPTRNDT.NAMOUNT) AS nAmount , GLPTRNhd.CSRCJRNL        ;
    FROM &gcDataDir.GLPTRNDT,&gcDataDir.GLPTRNHD                     ;
   WHERE GLPTRNDT.cbatchno + GLPTRNDT.ctranno = GLPTRNHD.cbatchno +  ;
         GLPTRNHD.ctranno AND GLPTRNHD.CTRNTYPE <> 'B' AND &lcWhere  ; 
   GROUP BY &lcRPGrp                                                 ;
   INTO DBF (gcWorkDir+lcRpTargt)

  INDEX ON CACCTCODE+CTRNPYR+CTRNPPRD+&lcOrderBy TAG TmpTag

  SELECT GLACBALS
  SET RELATION TO glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid INTO &lcRpTargt ADDITIVE
ENDIF

lcRpTrgFil = IIF(lcRpSumzBy<>'N',lcRpTargt,'GLPTRNDT')
lcRpExp = IIF(lcRpExp='.T.','',lcRpExp)

IF lcRpSumzBy='N'
  lcRpExp = lcRpExp+IIF(!EMPTY(lcRpExp),' .AND. ','')+[GLPTRNhd.CTRNTYPE<>'B']+;
            IIF(!EMPTY(lcRpSjGrop),[ AND GLPTRNHD.CSRCJRNL = lcRpSjGrop ]," ")
ENDIF

IF lcRpDayPd = 'DAYS'
  lcExpr = lcRpExp + IIF(!EMPTY(lcRpExp),' .AND. ','')+'BETWEEN(GLACBALS.CFISFYEAR+GLACBALS.cfspprdid,lcRpYrPrF,lcRpYrPrT)'+;
           ' .AND. &lcRpTrgFil..dtrnPdate <= lcRpToDay'
ELSE
  lcExpr = lcRpExp       
ENDIF

*--Section to change the mask of the Account code mask filter.
lcMaskExp = lfSegExp()
lnAcCodPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACBALS.CACCTCODE'),1)
IF lnAcCodPos > 0
  IF !EMPTY(STRTRAN(laOGFxFlt[lnAcCodPos,6],'-'))
    IF ATC(' ',laOGFxFlt[lnAcCodPos,6]) > 0
      lcExpr = STRTRAN(lcExpr,[ GLACBALS.CACCTCODE = "]+laOGFxFlt[lnAcCodPos,6]+'"',lcMaskExp)
    ENDIF  
  ELSE  
    lcExpr = STRTRAN(lcExpr,[AND  GLACBALS.CACCTCODE = "]+laOGFxFlt[lnAcCodPos,6]+'"','')
  ENDIF  
ENDIF

IF 'CGRPCODE'$ lcRpExp
  SELECT GLGRPDT  
  SET ORDER TO TAG ACCYRPRD IN GLACBALS
  SET RELATION TO glgrpdt.cacctcode INTO GLACBALS ADDITIVE
  SET FILTER TO CGRDSTAT  = 'I'
  LOCATE
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
   IF lcRpSumzBy <> 'N'
    SET SKIP TO glptrndt,&lcRpTargt
   ELSE
     SET SKIP TO glptrndt
   ENDIF
ENDIF

IF lcRpSumzBy = 'N' AND llRpGrBySJ
  lnAliass = SELECT(0)		&&Save ALIAS before continue, to reuse it for data collecting.
  SET ORDER TO TAG SRCJRNL IN GLSUBJOR

  DIMENSION laFileStru[1,4],laTemp[1,4],laFileFlds[4,2]
  STORE SPACE(0) TO laFileStru,laTemp

  laFileFlds[1,1] = 'GLACBALS'
  laFileFlds[2,1] = 'GLACCHAR'
  laFileFlds[3,1] = 'GLPTRNDT'
  laFileFlds[4,1] = 'GLSUBJOR'

  laFileFlds[1,2] = 'CACCTCODE,CFSPPRDID,CFISFYEAR,NACBOPBAL'
  laFileFlds[2,2] = 'CACCNLDES'
  laFileFlds[3,2] = 'CBATCHNO,CTRANNO,CTRDTEXP,DTRNPDATE,NAMOUNT,CDRORCR'
  laFileFlds[4,2] = 'CSRCJRNL,CJORLNDES'

  lnElement = 1 			&& To have the start position in the structure array.
  FOR lnItems = 1 TO ALEN(laFileFlds,1)
    =lfMakeStru(laFileFlds[lnItems,1],laFileFlds[lnItems,2])
    lnDim = IIF(ALEN(laFileStru,1)=1,ALEN(laTemp,1) ,;
                ALEN(laFileStru,1)+ALEN(laTemp,1))  &&Redimension the array for structure array.
    DIMENSION laFileStru[lnDim,4]
    =ACOPY(laTemp,laFileStru,1,-1,AELEMENT(laFileStru,lnElement,1))
    lnElement = lnElement + ALEN(laTemp,1)
  ENDFOR  

  IF lcRpDayPd = 'DAYS'  
    lnFileStru = ALEN(laFileStru,1)+1
    DIMENSION laFileStru[lnFileStru, 4]
    laFileStru[lnFileStru ,1] = 'cSt_Bal'
    laFileStru[lnFileStru ,2] = 'C'
    laFileStru[lnFileStru ,3] = 78
    laFileStru[lnFileStru ,4] = 0
  ENDIF

  lcTempFile = gfTempName()      && Variable to hold Temp. name to create cursor
  CREATE CURSOR (lcTempFile) FROM ARRAY laFileStru
  INDEX ON cSRcjrnl+CACCTCODE+CFISFYEAR+CFSPPRDID TAG (lcTempFile)

  lcScanExpr = IIF(EMPTY(lcExpr)                                     ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.)'        ,;
               'FOR IIF(llRpExEAct , !EOF(lcRpTrgFil) , .T.) AND ') + ;
               lcExpr		&&Expression used for scanning last open alias

  lcOldAcct = ''		&&Used to compare with Account Code.
  
  SELECT (lnAliass)		&&Using the last alias used before creating CURSOR
  SCAN &lcScanExpr
    IF (lcRpDayPd = 'DAYS') AND (lcOldAcct != CACCTCODE)
      lcAccCode = CACCTCODE 
      lcSt_Bal = lfInit()      && calculate Begining balance
      lcOldAcct = CACCTCODE	   && Store Account Code Value
      LOCATE &lcScanExpr AND CACCTCODE = lcAccCode
    ENDIF

    INSERT INTO (lcTempFile)                           ;
             (CACCTCODE,CFSPPRDID,CFISFYEAR,NACBOPBAL ,;
              CACCNLDES                               ,;
              CBATCHNO,CTRANNO,CTRDTEXP               ,;
              CSRCJRNL                                ,;
              DTRNPDATE,NAMOUNT,CDRORCR)               ;
      VALUES (GLACBALS.CACCTCODE                      ,;
              GLACBALS.CFSPPRDID                      ,;
              GLACBALS.CFISFYEAR                      ,;
              GLACBALS.NACBOPBAL                      ,;
              GLACCHAR.CACCNLDES                      ,;
              GLPTRNDT.CBATCHNO                       ,;
              GLPTRNDT.CTRANNO                        ,;
              GLPTRNDT.CTRDTEXP                       ,;
              GLPTRNHD.CSRCJRNL                       ,;
              &lcRpTrgFil..DTRNPDATE                  ,;
              &lcRpTrgFil..NAMOUNT                    ,;
              &lcRpTrgFil..CDRORCR)

    IF (lcRpDayPd = 'DAYS')
      REPLACE &lcTempFile..cSt_Bal    WITH lcSt_Bal  ,;
              &lcTempFile..NACBOPBAL  WITH lnBalance
    ENDIF

    =SEEK(GLPTRNHD.CSRCJRNL,'GLSUBJOR')
    REPLACE &lcTempFile..CJORLNDES WITH GLSUBJOR.CJORLNDES
  ENDSCAN
  
  SELECT (lcTempFile)		&&Use Cursor to be active alias before report 

  lcExpr     = ""				&&Clear expresion
  lcRpTrgFil = lcTempFile		&&Assign target file
ENDIF

IF lcRpSumzBy = 'N' AND !llRpGrBySJ
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
  SET SKIP TO 
  
  IF 'CGRPCODE'$ lcRpExp
    lcstat = "GLGRPDT.CGRPCODE = "+'"'+laOGFxFlt[3,6]+'"'
    lcExpr = STRTRAN(lcExpr,lcstat,".T.")
  ENDIF
  
  WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT

  IF lcRpDayPd = "PERIODS"
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
        WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
        SELECT GLPTRNDT
        SCAN REST WHILE glacbals.cacctcode + glacbals.cfisfyear + glacbals.cfspprdid  = cacctcode+ctrnpyr+ctrnpprd ;
                  FOR GLPTRNHD.CTRNTYPE<>'B'
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
  ENDIF
  DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcExpr),'FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.)','FOR IIF(llRpExEAct,!EOF(lcRpTrgFil),.T.) AND ')+lcExpr
ENDIF

lcExpr = ""
IF lcRpSumzBy<>'N'
   USE IN ALIAS(lcRpTargt)
   ERASE (gcWorkDir+lcRpTargt+'.BDF')
   ERASE (gcWorkDir+lcRpTargt+'.CDX')

ELSE    && Else  IF Summerization is N/A and Group By S.J.
  IF llRpGrBySJ
    USE IN (lcTempFile) 		&& Note that it is a cursor.
  ELSE
    USE IN ALIAS(lcAccTmp)
    ERASE (gcWorkDir+lcAccTmp+'.BDF')
    ERASE (gcWorkDir+lcAccTmp+'.CDX')
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
                       *-- End of the Program --*    
*!*************************************************************
*! Name      : lfvFisYer
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Check if current company has this entried year or not
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvFisYer()
*!*************************************************************
FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpFicsYr = EVALUATE(SYS(18))
lcRpCurFld = VARREAD()
IF !EMPTY(lcRpFicsYr)  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  IF !USED("FISHD") 
    SELECT 0
    USE (gcDataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr)) 
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)     
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    USE IN FISHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvFisYer.
*!*************************************************************
*! Name      : lfvFisPrd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Check if current company has this entried period or not.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvFisPrd()
*!*************************************************************
FUNCTION lfvFisPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF !EMPTY(lcRpFicsYr) 
  IF !EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = SELECT()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    IF !USED("FSPRD") 
      SELECT 0
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD

    *** Search for the current company+year+Prd
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
      =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      USE IN FSPRD
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ENDIF
RETURN 

*--End of lfvFisPrd.
*!*************************************************************
*! Name      : lfvToDay
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Check if the lcRpFrDay is smaller than lcRpToDay or not.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvToDay()
*!*************************************************************
Function lfvToDay

IF lcRpFrDay > lcRpToDay
  _CUROBJ = OBJNUM(_CUROBJ)
ENDIF

*--End of lfvToDay.
*!*************************************************************
*! Name      : lfvActBy
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Control the variable related to Activity way.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvActBy()
*!*************************************************************
Function lfvActBy

IF lcRpSumzBy ='N' AND llRpGrBySJ
  lcRpForm = IIF(lcRpDayPd = 'PERIODS','GLMULRSC','GLMULRSD')
ELSE	&&Summarization is not (N/A) OR not (Sorted By S.J.) 	
  lcRpForm = IIF(lcRpDayPd = 'PERIODS','GLACCMUL','GLMULRED')
ENDIF

*--Section Empty the mask filter in the case of the days section.
lnMaskPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACBALS.CACCTCODE'),1)
IF lcRpDayPd = 'DAYS'
  IF !EMPTY(laOGFxFlt[lnMaskPos,6])
    laOGFxFlt[lnMaskPos,6] = ""
  ENDIF
ENDIF

laOGObjCnt[3] = IIF(lcRpSumzBy = 'N',.T.,.F.) 	&&IF Summarization !(N/A),DISABLE (Group By S.J.) OPTION

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
    laOGObjCnt[5]   = .T.
    laOGObjCnt[6]   = .T.
    laOGObjCnt[9]   = .F.
    laOGObjCnt[10]  = .F.
    laOGFxFlt [1,6] = ''	&&Clear Contents of fisical year field
    laOGFxFlt [2,6] = ''	&&Clear Contents of period field
    =lfWReadRep('D')
ENDCASE
=lfActvateWin(lnWinDisp)

*--End of lfvActBy.
*!*************************************************************
*! Name      : lfvGrpCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Check if current company has this entried year or not.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvGrpCode()
*!*************************************************************
Function lfvGrpCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CGrpCode:H="Code",CGrplnHed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

*--Check If year field is empty
IF !EMPTY(&lcRpCurFld.) 
  IF !EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = ALIAS()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    IF NOT USED("GLGRPHD") 
      SELECT 0
      USE &gcDataDir.GLGRPHD ORDER TAG grpcode
      llUesdBefo = .T.
    ENDIF
    SELECT GLGRPHD
    SET ORDER TO TAG grpcode

    *--Search for the current Group code
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

*--End of lfvGrpCode.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Hold the old value.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

lcRpOld = EVAL(VARREAD())

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfGetPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Get the picture of the field.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfGetPic()
*!*************************************************************
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

IF !USED("ACCOD")
  SELECT 0
  USE &gcDataDir.ACCOD 
  llUesdBefo = .T.
ENDIF
SELECT ACCOD
LOCATE

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
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF
RETURN lcRpSegMas

*--End of lfGetPic.
*!*************************************************************
*! Name      : lfvDate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Validate the date.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfvDate()
*!*************************************************************
FUNCTION lfvDate

lcCurObj = VARREAD()
IF EVAL(lcCurObj)<>lcRpOld
  IF !USED('FSPRD')
    SELECT 0
    USE &gcDataDir.FSPRD 
  ENDIF
  SELECT FSPRD 
  SET ORDER TO TAG COMFYRPRDI
   IF SEEK(ALLTRIM(STR(YEAR(EVAL(lccurobj)))))
    LOCATE REST WHILE  DFSPPBGDT<=EVAL(lcCurObj) ;
    FOR BETWEEN(EVAL(lcCurObj),DFSPPBGDT,DFSPPENDT) 
   
    IF FOUND()
      lcWhichVar = SUBSTR(lcCurObj,5,1)
      lcRpYrPr&lcWhichVar = cfisfyear+cfspprdid
    ELSE
      WAIT 'INVALID DATE 1' WINDOW
    ENDIF
  ELSE
    WAIT 'INVALID DATE 2' WINDOW
    _CUROBJ=_CUROBJ
  ENDIF
ENDIF

*--End of lfvDate.
*!*************************************************************
*! Name      : lfGetDet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : 
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfGetDet()
*!*************************************************************
FUNCTION lfGetDet
PARAMETER llParam

IF PARAM()<>0
 lldone = .F.
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

*--End of lfGetDet.
*!*************************************************************
*! Name      : lfInit
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : 
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfInit()
*!*************************************************************
FUNCTION lfInit

lldone = .F.

IF lcAcctCode <> GLACBALS.cAcctCode
  lnBalance  = GLACBALS.NACBOPBAL  
ENDIF

lcOldAlias = ALIAS()
SELECT glacbals
IF !EOF()
  GO RECNO()
ENDIF  

SELECT GLPTRNDT
IF !EOF()
  GO RECNO()
ENDIF  

lnSaveRec = RECNO()
IF lcAcctCode <> GLACBALS.cAcctCode
  SUM REST IIF(GLPTRNDT.cDrOrCr='D',GLPTRNDT.nAmount,-GLPTRNDT.nAmount)   ;
      WHILE CACCTCODE+CTRNPYR+CTRNPPRD = GLACBALS.cAcctCode               ;
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

RETURN 'Detail entries From '+DTOC(lcRpFrDay)+' TO '+DTOC(lcRpToDay)+' Begining bal. '+ALLTRIM(STR(lnBalance))    

*--End of lfInit.
*!*************************************************************
*! Name      : lfWReadRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : When the report called.
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfWReadRep()
*!*************************************************************
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
  IF !USED('FSPRD')
    USE &gcDataDir.FSPRD IN SELECT(1)
    llCompUsd=.T.
  ENDIF

  SELECT FSPRD
  SET ORDER TO TAG COMFYRPRDI 
  IF SEEK(lcRetVal)
    lcRpFrDay = DFSPPBGDT
    lcRpToDay = DFSPPENDT
    lcRpYrPrF = cfisfyear+cfspprdid
    lcRpYrPrT = cfisfyear+cfspprdid
  ENDIF 
  IF llCompUsd
    USE IN FSPRD
  ENDIF
ENDIF    
SELECT (lnCurAlias)  

*--End of lfWReadRep.
*!*************************************************************
*! Name      : lfMakeStru
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Fill array with specific fields from specific file
*!*************************************************************
*! Called from : GLACCMUL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfMakeStru()
*!*************************************************************
FUNCTION lfMakeStru
PARAMETERS lcThisFile,lcThisFd

lnDelItem = 1
SELECT &lcThisFile
= AFIELDS(laTemp)

*--Loop for required fields	
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

*--End of lfMakeStru.
*!*************************************************************
*! Name      : lfvActCod
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Get order and browse file.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvActCod()
*!*************************************************************
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
*!*************************************************************
*! Name      : lfvActMask
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : Function to get the begin of the account code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvActMask()
*!*************************************************************
FUNCTION lfvActMask

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

IF !EMPTY(&lcRpCurFld)
  SELECT GLACCHAR
  IF !EMPTY(STRTRAN(EVAL(VARREAD()),'-'))
    lcRpSegMas = STRTRAN(PADR(EVAL(VARREAD()),LEN(cacctCode)), ' ', '?')
    lcRpSegMas= [ LIKE("]+lcRpSegMas+[",cacctCode)]
    LOCATE FOR &lcRpSegMas
    IF !FOUND() .AND. gfModalGen("INM02245B00000",'DIALOG')=1
      &lcRpCurFld = lcRpOld
      SHOW GET (lcRpCurFld)    
    ENDIF
  ENDIF    
ENDIF
lcBrFields = lcOldBrFld

IF lcRpDayPd = 'DAYS'
  IF !EMPTY(&lcRpCurFld)
    &lcRpCurFld = ""
  ENDIF
ENDIF

*--End of lfvActMask.
*!*************************************************************
*! Name      : lfwOldMsk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : It must be exit for the close of the report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOldMsk()
*!*************************************************************
FUNCTION lfwOldMsk

lcRpOld = EVAL(VARREAD())

*--End of lfwOldMsk.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : It must be exit for the close of the report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfSegExp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : to get the mask of the Account code mask filter.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSegExp()
*!*************************************************************
FUNCTION lfSegExp

IF !EMPTY(STRTRAN(laOGFxFlt[4,6],'-'))
  lcRpSegMas = IIF(EMPTY(ALLTRIM(laOGFxFlt[4,6])),;
                STRTRAN(STRTRAN(lcRpPic, 'X', '?'),'9','?'),;
                STRTRAN(PADR(laOGFxFlt[4,6],LEN(GLACBALS.cacctCode)), ' ', '?'))
ELSE
  RETURN ''              
ENDIF

RETURN [ LIKE("]+lcRpSegMas+[",GLACBALS.cacctCode)]

*--End of lfSegExp.
*!*************************************************************
*! Name      : lfSubTot
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/16/2003
*! Purpose   : To calculate the subtotal of the account code.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSubTot()
*!*************************************************************
FUNCTION lfSubTot

IF lcAcCode # glacbals.cacctcode
  lcAcCode = glacbals.cacctcode
  lnSubTotal = 0
  lnSubClose = 0
ENDIF

IF lcRpForm = 'GLMULRSC' OR lcRpForm = 'GLMULRSD'
  lnSubTotal = lnSubTotal + nacbopbal
ELSE
  lnSubTotal = lnSubTotal + glacbals.nacbopbal
ENDIF

*--Field of the closing balance.
lnSubClose = lnSubClose + lnclose

lnx = IIF(EVAL(lcRpTrgFil+'.cdrorcr')='D',EVAL(lcRpTrgFil+'.namount'),-EVAL(lcRpTrgFil+'.namount'))
RETURN ""

*--End of lfSubTot.