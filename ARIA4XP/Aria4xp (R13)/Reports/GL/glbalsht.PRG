*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLBALSHT.PRG
*:  Module      : General Ledger
*:  Desc.       : Trial Balance
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/03/2012
*:  Reference   : *E303228,1
 *                 E303256 ( Contains the shared files between GLINCSTA,GLBALSHT )
*:************************************************************************
*Modifications
*B610116,1 TMI 10/16/2012 [T20121016.0004] fix a bug Subscript is out of bound
*B610454,1 TMI create the temp form if the var lcOGTmpForm is empty [T20130627.0025]
*B610461,1 TMI 08/14/2013 [Start] fix the problem that dummy post does not work 
**************************************************************************
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#include r:\aria4xp\reports\gl\glbalsht.H
*N000682,1 MMT 02/10/2013 Globalization changes[END]
*- Define variable.
lcRpTemp = ''
lnBegTime = 0
lnEndTime = 0
lnTotTime = 0
lcRpRelVar = ''    && Hold the relation condition
=''
lcRpTypeDes = '~Assets~Liability~Owner equity~'
lcRpTotDes  = '~ ~ ~ ~ ~'
lcRpTotExp  = '~ ~S|C~S|C|E~S|C|E|I~ ~'

* 09/05/2012 [Start] Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
lnRpColWid = loOgScroll.lnRpColWid
lcRpSegMsk = loOgScroll.lcRpSegMsk
lnRpTotSeg = loOgScroll.lnRpTotSeg

lcWorkFile = loOgScroll.lcWorkFile

*lnRpTotSeg = 0     && Hold the number of segment

lnRpIdnNo  = 0     && Hold the identation
*** Create array hold the structuer of temp file
DIMENSION laDbfName[3,4]

*** Create array that hold the summation of main type
*** that used in the percentage of option
DIMENSION laRpMainTo(ALEN(laRpCol,1))


*** Hold the needed segment information
DIMENSION laSegInf(1,2)
=lfGetActInf()

*** If the user change any thing in the opion grid or
*** in the tool (Rows & Columns ) rebuild the data

*IF llOgFltCh OR llRpToolCh
  IF lcOGManRep <> 'GLBALSHT'
    IF !lfInitCol()
      RETURN
    ENDIF

    lcOldAlias = SELECT()
    llUsed = .F.
    IF NOT USED('GLTYPES')
      SELECT 0
      USE (oAriaApplication.DataDir+'GLTYPES') ORDER TAG TypeCode
      llUsed = .T.
    ENDIF
    SELECT GLTYPES
    SET ORDER TO TAG TypeCode
    IF EMPTY(laRpMaType)
      =lfColData()
    ELSE
      =lfUpdData()
    ENDIF
    SELECT GLTYPES
    IF llUsed
      USE
    ENDIF
    SELECT(lcOldAlias)
  ENDIF
*** Check if there is informations in rows or columns
IF EMPTY(laRpMaType) OR EMPTY(laRpCol)

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT "No columns has been defined " WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_columns_has_been_defined,oAriaApplication.GetHeaderText("LANG_No_columns_has_been_defined",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]

 RETURN
ENDIF
  *** Initialize the array by 0
  FOR lnCount = 1 TO ALEN(laRpMainTo,1)
    laRpMainTo[lnCount] = 0
  ENDFOR

  *** Create the dbf file structuer

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT "Creating temporary files " WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temporary_files,oAriaApplication.GetHeaderText("LANG_Creating_temporary_files",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]
  laDbfName[1,1] = 'CacctCode'
  laDbfName[1,2] = 'C'
  laDbfName[1,3] = 24
  laDbfName[1,4] = 0

  laDbfName[2,1] = 'CTypeCode'
  laDbfName[2,2] = 'C'
  laDbfName[2,3] = 3
  laDbfName[2,4] = 0

  laDbfName[3,1] = 'CRowDes'
  laDbfName[3,2] = 'C'
  laDbfName[3,3] = 71
  laDbfName[3,4] = 0

  lnAryElem      = 3
  FOR lnCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnCount,1] $ 'BA'
      lnAryElem = lnAryElem + 1
      DIMENSION laDbfName(lnAryElem,4)
      laDbfName[lnAryElem,1] = 'Ncol'+ALLTRIM(STR(lnCount))
      laDbfName[lnAryElem,2] = 'N'
      laDbfName[lnAryElem,3] = 18
      laDbfName[lnAryElem,4] = 2
    ENDIF
  ENDFOR

  CREAT DBF (oAriaApplication.WorkDir+loOgScroll.lcTempFile) FROM ARRAY laDbfName
  INDEX ON CacctCode  TAG CacctCode


  llRpToolCh = .F.
  lcRpFiles  = "GLACCHAR"+IIF(lcRpExp='.T.','',",GLGRPDT")
  lnCount    = 0
  lnTotal    = RECCOUNT('GLACCHAR')
  lcWhereExp    =  IIF(lcRpExp='.T.','',;
               'GLACCHAR.CACCTCODE = GLGRPDT.CACCTCODE .AND. '+lcRpExp+' .AND.')+;
               'LEFT(CTypeCode,1) IN ("A","L","Q")'

  lcPrExp    =  IIF(EMPTY(lcWhereExp),'',lcWhereExp)
  llOGEscPrs = .F.
  lcOGEscPrm = SET("Escape")
  lcOGEscHnd  = SET("Escape")
  =lfTrapEsc(.T.,0)


  lnBegTime = SECONDS()

  *** Collect the main and sub account types orderd by the arrangement
  *** provided by the user throguh the tool program linked to this report


  SELECT  &lcRpFields;
    FROM  &lcRpFiles;
    WHERE &lcPrExp .AND. lfRpThermo(lnTotal,'lnCount');
    ORDER BY 1;
    INTO DBF (oAriaApplication.WorkDir+loOgScroll.lcRpTargt)

  lnEndTime = SECONDS()
  lnTotTime = lnTotTime +(lnEndTime-lnBegTime)

  *** End the key trap of ESC
  =lfTrapEsc(.F.)

  *** If the user did not ontrrupt the selection of the account type
  IF !llOGEscPrs
    *** Close the thermometer if it's still open
    *Old:IF lnCount < lnTotal
    *Old:  =lfClosThrm(lnTotal,lnCount,.f.,INT(ABS(lnTotal-_tally)/10))
    *Old:ENDIF

    *** If no recordes collected inform the user and terminat
    IF _TALLY = 0        && No records collected
      ** NO recoeds hove been collected
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE

      *** Esatablish the relations between the main temp file and
      *** the temp files represinting the each column
      =lfSetRela()

      *** Collect the report data from actual or budget files

      * N000682 ,1 Thabet Handle globalization issues [Start]
	  *WAIT 'Collecting data from files' WINDOW NOWAIT
	  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collectin_data_from_files,oAriaApplication.GetHeaderText("LANG_Collectin_data_from_files",AHEADERFILE)) WINDOW NOWAIT
      * N000682 ,1 Thabet Handle globalization issues [END]

      =lfSelData()
      IF _TALLY > 0
        *** if it is result or indentation .....
        *** Know the type of each column in the report


        DIMENSION laColInf[1,lnRpColUsd]
        =lfGetColInf()

        =lfUpdWorkFile()
        *************************************

        *B610454,1 TMI [START] create the temp form if the var lcOGTmpForm is empty
        IF EMPTY(lcOGTmpForm )
          lcOGTmpForm = gfTempName()
          gcAct_Appl = 'GL'
          =gfCrtFrm(lcRpForm,"",llOGRefForm)  
        ENDIF 
        *B610454,1 TMI [end  ] create the temp form if the var lcOGTmpForm is empty

        SELECT (lcWorkFile)
        DO gfDispRe WITH EVAL('lcRpForm')

        *E300789,6 Close temp. Cursor [Begin
        IF USED(lcRpTemp)
          SELECT (lcRpTemp)
          SET RELATION TO
          USE IN (lcRpTemp)
        ENDIF
        *E300789,6 Close temp. Cursor [End

      ELSE
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ENDIF
    ENDIF
  ELSE
    IF lnCount < lnTotal
      lnCount = lnTotal-1
      =lfClosThrm(lnTotal,lnCount)
    ENDIF
  ENDIF

*!************************************************************************
*!
*!      FUNCTION : lfvDumPost
*!
*!************************************************************************
*
FUNCTION lfvDumPost

lcTempPost = loOgScroll.lcTempPost
IF USED(lcTempPost)
  SELECT(lcTempPost)
  ZAP
ENDIF

PRIVATE lcProgDir, lcSaveAppl
lcProgDir = oAriaApplication.ApplicationHome+loOgScroll.gcAct_Appl+'\'

PRIVATE lnColNo

lcSaveAppl = gcWinAppl
gcWinAppl  = 'GL'

DO CASE

  *** Case Journal batches
  CASE lnDumPost = '1'
    glFirsTime = .T.
    DO (lcProgDir+'GLBPOST.FXP') WITH lcTempPost


  *** Case Single transaction
  CASE lnDumPost = '2'
    glFirsTime = .T.
    DO (lcProgDir+'GLTPOST.FXP') WITH lcTempPost

  *** Case Begining balance baches
  CASE lnDumPost = '3'
     glFirsTime = .T.
    DO (lcProgDir+'GLBGPST.FXP') WITH lcTempPost
ENDCASE

*B610461,1 TMI 08/14/2013 [Start] fix the problem that dummy post does not work 
IF !USED(lcTempPost) AND FILE(oAriaApplication.WorkDir+lcTempPost+'.DBF')
  USE (oAriaApplication.WorkDir+lcTempPost) IN 0 EXCLUSIVE 
ENDIF 
*B610461,1 TMI 08/14/2013 [End  ] 



gcWinAppl  = lcSaveAppl

*!************************************************************************
*!
*!      FUNCTION : lfvGrpCode
*!
*!************************************************************************
*
FUNCTION lfvGrpCode

loFld = loOgScroll.ActiveControl
loFld.OldValue = ALLTRIM(loFld.OldValue)
loFld.Value    = ALLTRIM(loFld.Value)
IF loFld.Value == loFld.OldValue
  RETURN
ENDIF

lcRpCurFld = 'LAOGFXFLT[1,6]'
DECLARE laRpRetFld(1)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
lcBrFields    = 'CGrpCode:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Code,oAriaApplication.GetHeaderText("LANG_Code",AHEADERFILE))+;
				'",CGrplnhed:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
lcOldAlias    = SELECT()
llUsedBef     = .T.

IF !USED('GLGRPHD')
  SELECT 0
  USE (oAriaApplication.DataDir+'GLGRPHD')
  llUsedBef = .F.
ENDIF

SELECT GLGRPHD
SET ORDER TO TAG grpcode

IF !EMPTY(&lcRpCurFld)
  IF ('?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld))
    IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
      GOTO RECNO(0)
    ENDIF
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([""],'CGrpCode',"laRpRetFld",'Codes File',.F.)
	=gfBrows([""],'CGrpCode',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODEFILE,oAriaApplication.GetHeaderText("LANG_CODEFILE",AHEADERFILE)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [End]
    loOgScroll.&lcRpCurFld = laRpRetFld[1]
  ENDIF
ENDIF
*lcBrFields = lcOldBrFld

IF !llUsedBef
  USE IN GLGRPHD
ENDIF

SET ORDER TO
SELECT(lcOldAlias)


*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF (ASCAN(loOgScroll.laSelFile,'ACCOD') = 0) AND USED('ACCOD')
  USE IN ACCOD
ENDIF

IF (ASCAN(loOgScroll.laSelFile,'GLSETUP') = 0) AND  USED('GLSETUP')
  USE IN GLSETUP
ENDIF

IF (ASCAN(loOgScroll.laSelFile,'GLBUDDT') = 0) AND  USED('GLBUDDT')
  USE IN GLBUDDT
ENDIF

IF (ASCAN(loOgScroll.laSelFile,'GLACBALS') = 0) AND  USED('GLACBALS')
  USE IN GLACBALS
ENDIF

IF (ASCAN(loOgScroll.laSelFile,'GLSEGVAL') = 0) AND  USED('GLSEGVAL')
  USE IN GLSEGVAL
ENDIF

*E301077,72 Close file if it's not in OG open file array [End

IF !EMPTY(ALIAS())
  SET RELATION TO
ENDIF

DIMENSION laRpMaType(1,6),laRpAType(1,6),laRpLType(1,6),laRpQType(1,6)
laRpMaType[1,1] = ''
DIMENSION laRpCol(1,24)     && Column information

DIMENSION laTempCol(1,23)   && Temp Column
DIMENSION laDbfName(1),laRpMainTo[1,1],laRpSeg(1,2)
DIMENSION laRpColInf(47,2)


*!************************************************************************
*!
*!      FUNCTION : lfvApprove
*!
*!************************************************************************
*
FUNCTION lfvApprove
PARAMETER lnRpVarType

loFld = loOgScroll.ActiveControl

lcObjName = loFld.Parent.cOgArray
*lnNoCol   = INT((lnRpRepWid-lnRpFirIde-lnRpRowTit)/(lnRpColWid+lnRpColIde))
lnNoCol   = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
IF lnRpNoCol+lnRpIdnCol > lnNoCol
  lnRpVarType = 0
ELSE
  DO CASE
    CASE lnRpVarType = 1 AND !BETWEEN(lnRpRepWid,80,255)
      lnRpVarType = 0
    CASE lnRpVarType = 2 AND !BETWEEN(lnRpColWid,4,18)
      lnRpVarType = 0
    CASE lnRpVarType = 3 AND !BETWEEN(lnRpFirIde,1,lnRpRepWid)
      lnRpVarType = 0
    CASE lnRpVarType = 4 AND !BETWEEN(lnRpColIde,1,lnRpRepWid)
      lnRpVarType = 0
    CASE lnRpVarType = 5 AND !BETWEEN(lnRpColIde,0,190)
      lnRpVarType = 0
    CASE lnRpVarType = 6 AND !BETWEEN(lnRpDesWid,10,65)
      lnRpVarType = 0
  ENDCASE

ENDIF

IF lnRpVarType = 0
  *&lcObjName = lcRpOld            && Assign the old value
  &lcObjName = loFld.OldValue
  RETURN .F.
ELSE
*  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpRowTit)/(lnRpColWid+lnRpColIde))
    lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
ENDIF

*!*************************************************************************
*!
*!         Function : lfvExcZe
*!
*!*************************************************************************
*
FUNCTION lfvExcZe

*IF llRpExcZe <> lcRpOld
  llRpToolCh = .T.
*ENDIF

*** This part control the summarization screen
*!************************************************************************
*!
*!      FUNCTION : lfvSumzBy
*!
*!************************************************************************
*
FUNCTION lfvSumzBy

lcOldAlias = SELECT()

IF !USED('ACCOD')
  USE (oAriaApplication.DataDir+'ACCOD')
ENDIF
SELECT ACCOD

GO TOP

lnTotSegSz = NACSSEGSZ
lnTotSeg   = NACSNOSEG
lnFirsPos  = (59 - (lnTotSegSz + (lnTotSeg * 4)))/2

SELECT NACSSIZE,CACSSHDES,.T. ,0;
  FROM ACCOD ;
  WHERE NACSSEGSZ < 1 ;
  INTO ARRAY laRpSeg

DIMENSION laRpSeg(6,4)

laRpSeg[1,4] = lnFirsPos
FOR lnSegCount = 2 TO lnTotSeg
  laRpSeg[lnSegCount,4] = laRpSeg[lnSegCount-1,4]+laRpSeg[lnSegCount-1,1]+5
ENDFOR
FOR lnSegCount = lnTotSeg+1 TO 6
  lcSegNo = 'lnRpSeg'+ALLTRIM(STR(lnSegCount))
  laRpSeg[lnSegCount,1] = 1
  laRpSeg[lnSegCount,2] = ''
  laRpSeg[lnSegCount,4] = 0
  &lcSegNo = 0
ENDFOR

llFisTime = .T.

*** Store the old values
lnTemSumzBy = loOgScroll.lnRpSumzBy
lnSeg1 = loOgScroll.lnRpSeg1
lnSeg2 = loOgScroll.lnRpSeg2
lnSeg3 = loOgScroll.lnRpSeg3
lnSeg4 = loOgScroll.lnRpSeg4
lnSeg5 = loOgScroll.lnRpSeg5
lnSeg6 = loOgScroll.lnRpSeg6



DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLSUMZBY.SCX')

SELECT (lcOldAlias)

*!************************************************************************
*!
*!      FUNCTION : lfwSumzType
*!
*!************************************************************************
*
FUNCTION lfwSumzType

IF llFisTime
  IF lnRpSumzBy < 3
    SHOW GET lnRpSeg2 DISABLE
    SHOW GET lnRpSeg3 DISABLE
    SHOW GET lnRpSeg4 DISABLE
    SHOW GET lnRpSeg5 DISABLE
    SHOW GET lnRpSeg6 DISABLE
  ELSE
    SHOW GET lnRpSeg2 ENABLE
    SHOW GET lnRpSeg3 ENABLE
    SHOW GET lnRpSeg4 ENABLE
    SHOW GET lnRpSeg5 ENABLE
    SHOW GET lnRpSeg6 ENABLE
  ENDIF
  llFisTime = .F.
ENDIF
SHOW GETS

*!************************************************************************
*!
*!      FUNCTION : lfvSumzType
*!
*!************************************************************************
*
FUNCTION lfvSumzType

IF lnRpSumzBy < 3
  SHOW GET lnRpSeg2 DISABLE
  SHOW GET lnRpSeg3 DISABLE
  SHOW GET lnRpSeg4 DISABLE
  SHOW GET lnRpSeg5 DISABLE
  SHOW GET lnRpSeg6 DISABLE
ELSE
  SHOW GET lnRpSeg2 ENABLE
  SHOW GET lnRpSeg3 ENABLE
  SHOW GET lnRpSeg4 ENABLE
  SHOW GET lnRpSeg5 ENABLE
  SHOW GET lnRpSeg6 ENABLE
ENDIF

*!************************************************************************
*!
*!      FUNCTION : lfvCanSumz
*!
*!************************************************************************
*
FUNCTION lfvCanSumz

WITH loOgScroll
  .lnRpSumzBy = lnTemSumzBy
  .lnRpSeg1 = lnSeg1
  .lnRpSeg2 = lnSeg2
  .lnRpSeg3 = lnSeg3
  .lnRpSeg4 = lnSeg4
  .lnRpSeg5 = lnSeg5
  .lnRpSeg6 = lnSeg6
ENDWITH

*!************************************************************************
*!
*!      FUNCTION : lfvOkSumz
*!
*!************************************************************************
*
FUNCTION lfvOkSumz

llRpToolCh = .T.

*** The end of summarization  screen

*!************************************************************************
*!
*!      FUNCTION : lfMaSuType
*!
*!************************************************************************
*
FUNCTION lfMaSuType
PARAMETER lcFieldVal,lnArrType

IF LEFT(lcFieldVal,1) $ 'ALQ'

  IF lnArrType = 1
    lcFieldVal = LEFT(lcFieldVal,1)
    RETURN (ALLTRIM(STR(ASUBSCRIPT(laRpMaType,ASCAN(laRpMaType,lcFieldVal),1))))
  ELSE
    lcRpArrName = 'laRp'+LEFT(lcFieldVal,1)+'Type'
    RETURN (ALLTRIM(STR(ASUBSCRIPT(&lcRpArrName,ASCAN(&lcRpArrName,lcFieldVal),1))))
  ENDIF
ELSE
  RETURN '0'
ENDIF


*** This part select the columns information from
*** files . Only the budget and actual columns .
***
*!************************************************************************
*!
*!      FUNCTION : lfSelData
*!
*!************************************************************************
* This fnction will build the data for each column in the report comming
* from eather the balance or the budget files
*
FUNCTION lfSelData

lcRpTargt = loOgScroll.lcRpTargt
lcTempFile = loOgScroll.lcTempFile
lcTempPost = loOgScroll.lcTempPost

lnBegTime = SECONDS()
IF !USED(lcTempPost)
  lnDumPost = '0'
ENDIF


SELECT (lcRpTargt)
SCAN
  SELECT (lcTempFile)
  APPEND BLANK
  REPLACE CacctCode WITH &lcRpTargt..CacctCode
  REPLACE CTypeCode WITH &lcRpTargt..CTypeCode
  REPLACE CRowDes WITH SPACE(65)

  SELECT (lcRpTargt)
  lnColField=0
  FOR lnCount = 1 TO ALEN(laRpCol,1)
    lnOpResult = 0
    DO CASE
      CASE laRpCol[lnCount,1] = 'B'
      lnColField=lnColField+1
      lcFieldName = 'Ncol'+ALLTRIM(STR(lnCount))
        lcRpRelVar = laRpCol[lnCount,8]+laRpCol[lnCount,9]+;
      IIF(laRpCol[lnCount,10]<10,'0','')+ALLTRIM(STR(laRpCol[lnCount,10]))+;
                     CacctCode
        GO RECNO()

        GO TOP IN ACCOD
        lnNoSegts = ACCOD.Nacsnoseg
        lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                           ACCOD.NACSSIZE , 'ACCSEGNO')

         IF LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))

          SELECT GLBUDDT
          SUM REST Namount WHILE ;
           GLBUDDT.CbudCode+GLBUDDT.CbudYear+GLBUDDT.CBUDPRD+GLBUDDT.cacctcode <= ;
           laRpCol[lnCount,8]+laRpCol[lnCount,9]+STRTRAN(STR(laRpCol[lnCount,11],2),' ','0')+&lcRpTargt..cacctcode;
            TO lnOpResult

            lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI',-lnOpResult,lnOpResult)
        ELSE
          lnOpResult = 0
        ENDIF
        SELECT (lcTempFile)
        REPLACE (lcFieldName) WITH lnOpResult

      *** If the data colleged form balance file
      CASE laRpCol[lnCount,1] = 'A'
        lnColField=lnColField+1
        lcFieldName = 'Ncol'+ALLTRIM(STR(lnCount))
        *** If only one record colleged
        IF laRpCol[lnCount,12] = 1
          lcRpRelVar = CacctCode+laRpCol[lnCount,14]+laRpCol[lnCount,13]
          GO RECNO()
          IF lnDumPost <> '0'
            SELECT (lcTempPost)
            IF EOF()
              SELECT GLACBALS
            ENDIF
          ELSE
            SELECT GLACBALS
          ENDIF

          GO TOP IN ACCOD
          lnNoSegts = ACCOD.Nacsnoseg
          lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                             ACCOD.NACSSIZE , 'ACCSEGNO')

          IF LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))

            DO CASE
              CASE LEFT(&lcRpTargt..CTypeCode,1) = 'A'
                lnOpResult = NACBCLBAL
              CASE LEFT(&lcRpTargt..CTypeCode,1) = 'L'
                lnOpResult = 0 - NACBCLBAL
              CASE LEFT(&lcRpTargt..CTypeCode,1) = 'Q'
                lnOpResult = NACBPTDCR - NACBPTDDR - NACBOPBAL
            ENDCASE
            lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI',-lnOpResult,lnOpResult)
          ELSE
            lnOpResult = 0
          ENDIF
        ENDIF

        SELECT (lcTempFile)
        REPLACE (lcFieldName) WITH lnOpResult
    ENDCASE
    SELECT (lcRpTargt)
    *** Sum The Main type to used in the percentage of
    IF laRpCol[lnCount,22] = AT(LEFT(CTypeCode,1),'ALQ')
      laRpMainTo[lnCount] = laRpMainTo[lnCount] + lnOpResult
    ENDIF
  ENDFOR
  SELECT (lcRpTargt)
ENDSCAN
SELECT (lcTempFile)
lnNoCol=FCOUNT()-3
lcFieldsUsed=''

FOR lnColCount= 4 TO FCOUNT()
   lcFieldName=FIELD(lnColCount)
   lcFieldsUsed=lcFieldsUsed+IIF(lnColCount>4,',','')+'SUM('+lcFieldName+') '+;
                 'AS '+lcFieldName
ENDFOR

lcRpSumzBy   = lfGetSumzBy()
lcFieldsUsed=lcRpSumzBy+',CRowDes,CACCTCODE,'+IIF(lnRpSumzBy=1,;
               [LEFT(CTYPECODE,1)+'00' AS CTYPECODE,],'CTYPECODE,');
              +lcFieldsUsed

SET RELATION TO


* 09/14/2012 [Start] add the display fields in the frx then calculate them
lcFrxFields = ''
lnW = lnRpColWid+1
FOR lnI = 1 TO lnRpColUsd
  lcI = ALLTRIM(STR(lnI))
  *09/19/2012 [Start] use COL1,COL2,... instead
  lcFrxFields = lcFrxFields + "'"+REPLICATE(' ',lnW)+"' AS COL&lcI.,"
ENDFOR
lcFrxFields = LEFT(lcFrxFields,LEN(lcFrxFields)-1)

_TALLY = 0


lcTempFile = loOgScroll.lcTempFile
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT &lcFieldsUsed,&lcFrxFields,;
       lfMaSuType(CTYPECODE,1) AS 'NMainTy',;
       PADL(lfMaSuType(CTYPECODE,2),2,'0') AS 'NSubTy';
FROM &lcTempFile;
WHERE IIF(llRpExcZe,lfExcZe(),.T.);
GROUP BY 1;
ORDER BY NMAINTY,NSUBTY;
INTO DBF (oAriaApplication.WorkDir+lcWorkFile)

SET ENGINEBEHAVIOR lnEngineBehavior

IF _TALLY > 0
  =lfModiFile()
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfExcZe
*!
*!************************************************************************
*
FUNCTION lfExcZe

llTotalAm = .F.
FOR lnColCount= 4 TO FCOUNT()
  IF EVAL(FIELD(lnColCount)) <> 0
    llTotalAm = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llTotalAm

*!************************************************************************
*!
*!      FUNCTION lfModiFil
*!
*!************************************************************************
*
FUNCTION lfModiFil
PRIVATE lcOldVal,lcNewVal

SELECT GLACCHAR
SET ORDER TO ACCTCODE

SELECT (lcWorkFile)
SET RELATION TO CACCTCODE  INTO GLACCHAR ADDITIVE
GO TOP
IF lnRpSumzBy = 1
  lcOldVal = LEFT(CTYPECODE,1)
  SCAN
    lcNewVal = LEFT(CTYPECODE,1)
    lnRowPos = ASCAN(laRpMaType,lcOldVal)
    *** Put the main description into file
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
    IF lcOldVal <> LEFT(CTYPECODE,1)
      lnRowPos = ASCAN(laRpMaType,lcOldVal)
      INSERT BEFORE BLANK
      lnRowPos = ASCAN(laRpMaType,lcOldVal)

      IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'MF1'

        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
        ENDIF
        INSERT BLANK
      ENDIF
      IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'MF2'

        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
        ENDIF
        *** Added blank line before new main type
        INSERT  BLANK
      ENDIF
    ENDIF
    IF lcOldVal = 'Q'
*      INSERT  BLANK
      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH lcRpNetDes
      REPLACE CGROUP WITH 'MF2'
      =lfSumQRow('SETIC')
      INSERT  BLANK
    ENDIF
    lcOldVal = lcNewVal
  ENDSCAN
  lnRowPos = ASCAN(laRpMaType,lcOldVal)
  IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'MF1'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
    ENDIF
  ENDIF
  IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
    INSERT BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'MF2'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
    ENDIF
  ENDIF
  IF lcOldVal = 'Q'
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH lcRpNetDes
    REPLACE CGROUP WITH 'MF2'
    =lfSumQRow('SETIC')
  ENDIF

ELSE
***  For the subtype or account summarization level
  lcOldVal  = LEFT(CTYPECODE,1)
  lcOldVal2 = CTYPECODE
  lcNewVal  = lcOldVal
  lcNewVal2 = lcOldVal2

  lnRowPos = ASCAN(laRpMaType,lcOldVal)

  *** Put the main description into file
  INSERT  BEFORE BLANK
  REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]),;
          CTYPECODE WITH lcOldVal+'00',;
          CGROUP WITH 'H'

  *#** ADDED blank line after main type
  INSERT BLANK

  IF lnRpSumzBy = 3
    INSERT   BLANK
    REPLACE CGROUP WITH 'H'
  ELSE
    SKIP 1
  ENDIF

  *** Put the Subtype description
  lcRpArrName = 'laRp'+lcOldVal+'Type'
  lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
  REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2]),;
          CTYPECODE WITH lcOldVal2
  IF lnRpSumzBy = 3
    *#** ADDED blank line after subtype
    INSERT BLANK
  ENDIF

  llFirsTime = .T.

  SCAN
    IF llFirsTime
      IF lnRpSumzBy = 3
        SKIP 4
      ELSE
        SKIP 3
      ENDIF
      llFirsTime = .F.
    ENDIF

    IF lnRpSumzBy = 3
      REPLACE  CRowDES WITH lfGetAcDes(CACCTCODE)
    ENDIF

    lcNewVal  = IIF(EMPTY(LEFT(CTYPECODE,1)),lcNewVal,LEFT(CTYPECODE,1))
    lcNewVal2 = IIF(EMPTY(CTYPECODE),lcNewVal2,CTYPECODE)
    lcRpArrName = 'laRp'+lcOldVal+'Type'
    *** Insert subtotal of subType

    IF lcOldVal2 <> lcNewVal2
      lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
      INSERT BEFORE BLANK
      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
      ENDIF

      IF lnRpSumzBy = 3
        REPLACE CGROUP WITH 'LN,ST'
        INSERT  BLANK
        REPLACE CRowDES WITH PADL('SubTotal of '+IIF(lnRowPos = 0,'',ALLTRIM(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
        REPLACE CGROUP WITH 'STS'
        =lfSumMain(lcOldVal2)
        INSERT BLANK
        IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
          REPLACE CGROUP WITH 'LN,SF'
          *****
          INSERT  BLANK
        ENDIF
      ENDIF
      lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)

      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5]) OR;
         !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
        REPLACE CGROUP WITH 'SF1'
        IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
          =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5],'|','00,')+'00')
        ENDIF
        INSERT  BLANK
      ENDIF

      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6]) OR;
         !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
        REPLACE CGROUP WITH 'LN,SF'
        INSERT  BLANK
        lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
        REPLACE CGROUP WITH 'SF2'
        IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6])
          =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6],'|','00,')+'00')
        ENDIF
        INSERT  BLANK
      ENDIF

      *** Put subtype header if the summarization is account level and
      *** there are more subtypes in this main type
      IF  lcOldVal = lcNewVal
        *#** Added blank line before subtype header
*        INSERT   BLANK
        IF lnRpSumzBy = 3
          INSERT   BLANK
          REPLACE CGROUP WITH 'H'
          REPLACE CTYPECODE WITH lcNewVal2
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
          INSERT BLANK
        ELSE
          SKIP 1
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
        ENDIF
      ENDIF
    ENDIF

    IF lcOldVal <> lcNewVal
    *********** add  to add the subtotal of the main type
      lnRowPos = ASCAN(laRpMaType,lcOldVal)
      INSERT BLANK
      REPLACE CGROUP WITH 'LN,MT'
      INSERT  BLANK
      REPLACE CRowDES WITH PADL('Subtotal of '+ALLTRIM(IIF(lnRowPos = 0,lcOldVal,laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])),lnRpDesWid)
      REPLACE CGROUP WITH 'MT'
      =lfSumMain(lcOldVal)
    **********
      INSERT  BLANK
      IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5]) OR ;
         !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'MF1'
        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
        ENDIF
        INSERT  BLANK
      ENDIF
      IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6]) OR ;
         !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'MF2'
        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
        ENDIF
        INSERT  BLANK   && For empty line bettween each main type
      ENDIF
      IF lcOldVal = 'Q'
*      INSERT  BLANK
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH lcRpNetDes
        REPLACE CGROUP WITH 'MF2'
        =lfSumQRow('SETIC')
        INSERT  BLANK
      ENDIF

      *** Put the main header top the new main type
      lnRowPos = ASCAN(laRpMaType,lcNewVal)
      INSERT   BLANK
      REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
      REPLACE CTYPECODE WITH lcNewVal+'00'
      REPLACE CGROUP WITH 'H'
      INSERT BLANK

      *** Put subtype header if the summarization is account level
      IF lnRpSumzBy = 3
        *#** Added blank line after main type
        INSERT   BLANK
        REPLACE CGROUP WITH 'H'
        *** Put the Subtype description
        lcRpArrName = 'laRp'+lcNewVal+'Type'
        lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        REPLACE CTYPECODE WITH lcNewVal2
        *#** Added blank line after subtype
        INSERT BLANK
      ELSE
        SKIP 1
        lcRpArrName = 'laRp'+lcNewVal+'Type'
        lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        =lfSumMain(lcNewVal2)
      ENDIF
    ENDIF
    lcOldVal = lcNewVal
    lcOldVal2 = lcNewVal2
  ENDSCAN

  IF lnRpSumzBy = 3
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,ST'
    INSERT  BLANK
    lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
    REPLACE CRowDES WITH PADL('SubTotal of '+ALLTRIM(IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
    REPLACE CGROUP WITH 'STS'
    =lfSumMain(lcOldVal2)
  ENDIF
  lcRpArrName = 'laRp'+lcNewVal+'Type'
  lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
  INSERT  BLANK

  *** Insert the subtype footer 1
  IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5]) OR ;
     !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
    REPLACE CGROUP WITH 'LN,SF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
    REPLACE CGROUP WITH 'SF1'
    IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
      =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5],'|','00,')+'00')
    ENDIF
    INSERT  BLANK
  ENDIF
  *** Insert the subtype footer 2
  IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6]) OR;
     !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
    REPLACE CGROUP WITH 'LN,SF'
    INSERT  BLANK
    lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
    REPLACE CGROUP WITH 'SF2'
    IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6])
      =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6],'|','00,')+'00')
    ENDIF
    INSERT  BLANK
  ENDIF
  lnRowPos = ASCAN(laRpMaType,lcOldVal)

  REPLACE CGROUP WITH 'LN,MT'
  INSERT  BLANK
  REPLACE CRowDES WITH PADL('Subtotal of '+ALLTRIM(IIF(lnRowPos = 0,lcOldVal,laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])),lnRpDesWid)
  REPLACE CGROUP WITH 'MT'
  =lfSumMain(lcOldVal)
  INSERT  BLANK

  IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5]) OR ;
     !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'MF1'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
    ENDIF
    INSERT  BLANK
  ENDIF
  IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6]) OR ;
     !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'MF2'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
    ENDIF
  ENDIF
  IF lcOldVal = 'Q'
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH lcRpNetDes
    REPLACE CGROUP WITH 'MF2'
    =lfSumQRow('SETIC')
  ENDIF

ENDIF

SET RELATION TO

*!************************************************************************
*!
*!      FUNCTION : lfSumQRow
*!
*!************************************************************************
* Sum the column for the Q row
FUNCTION lfSumQRow
PARAMETERS lcChekVal
PRIVATE lnRecNo

lnRecNo = RECNO()
lcOldAlais = SELECT()

_TALLY = 0

lcRpTemp = gfTempName()

DIMENSION laRpTotal(ALEN(laRpCol,1))
FOR lnColCount = 1 TO ALEN(laRpTotal,1)
  laRpTotal[lnColCount] = 0
ENDFOR

lcPrExp    =  IIF(lcRpExp='.T.','',;
             'GLACCHAR.CACCTCODE = GLGRPDT.CACCTCODE .AND. &lcRpExp .AND.')+;
             [LEFT(CTypeCode,1) $ "SITEC"]

SELECT  &lcRpFields;
    FROM  &lcRpFiles;
    WHERE &lcPrExp;
    INTO CURSOR (lcRpTemp)

*** If there is row or more
lcTempPost = loOgScroll.lcTempPost
IF _TALLY > 0
  lcRpRelVar = ' '
  SELECT GLBUDDT
  SET ORDER TO TAG CDYRPRACC

  SELECT GLACBALS
  SET ORDER TO TAG ACCYRPRD

  IF lnDumPost <> '0'
    SELECT (lcTempPost)
    SET ORDER TO TAG ACCYRPRD
  ENDIF

  SELECT (lcRpTemp)
  SET RELATION TO lcRpRelVar INTO GLBUDDT ADDITIVE
  SET RELATION TO lcRpRelVar INTO GLACBALS ADDITIVE

  IF lnDumPost <> '0'
    SET RELATION TO lcRpRelVar INTO &lcTempPost ADDITIVE
  ENDIF
  SCAN
    FOR lnColCount = 1 TO ALEN(laRpCol,1)
      DO CASE
        *** If this column is from budget
        CASE laRpCol[lnColCount,1] = 'B'
          lcRpRelVar = laRpCol[lnColCount,8]+laRpCol[lnColCount,9]+;
                         '01'+CacctCode
          IF !EOF() AND !BOF()
            GO RECNO()
          ENDIF
          IF LIKE(STRTRAN(STRTRAN(laRpCol[lnColCount,24],' ','?'),'*','?'),SUBSTR(cacctcode,AT('-',cacctcode,1)+1))
            SELECT GLBUDDT
            SUM REST Namount WHILE ;
             lcRpRelVar = CbudCode+CbudYear+CBUDPRD+cacctcode AND;
             VAL(CBUDPRD) <= laRpCol[lnColCount,10];
             TO lnOpResult
            laRpTotal[lnColCount] = laRpTotal[lnColCount] + lnOpResult
          ENDIF

        *** If this column is from actual
        CASE laRpCol[lnColCount,1] = 'A'
          lcRpRelVar = CacctCode+laRpCol[lnColCount,14]+laRpCol[lnColCount,13]
         IF !EOF() AND !BOF()
            GO RECNO()
         ENDIF
          IF lnDumPost <> '0'
            SELECT (lcTempPost)
            IF EOF()
              SELECT GLACBALS
            ENDIF
          ELSE
            SELECT GLACBALS
          ENDIF
          laRpTotal[lnColCount] = laRpTotal[lnColCount] - ;
                                   (NACBOPBAL + NACBPTDDR - NACBPTDCR)
             *                     NACBPTDCR - NACBPTDDR - NACBOPBAL

      ENDCASE
      SELECT(lcRpTemp)
    ENDFOR
  ENDSCAN
ENDIF

*** Replace the total column to its postion into the file
SELECT (lcOldAlais)
GO lnRecNO
FOR lnColCount = 1 TO ALEN(laRpCol,1)
  IF laRpCol[lnColCount,1] $ 'BA'
    lcColCount= ALLTRIM(STR(lnColCount))
    REPLACE NCOL&lcColCount WITH laRpTotal[lnColCount]
  ENDIF
ENDFOR


*!************************************************************************
*!
*!      FUNCTION : lfGetAcDes
*!
*!************************************************************************
*
FUNCTION lfGetAcDes
PARAMETERS lcAcctCode
PRIVATE lcRetVal


lnOldRecNo = RECNO()
lcOldAlias = SELECT()

lcSegCond = IIF(lnRpTotSeg < 2 ,' lnRpSeg1=1 ',IIF(lnRpTotSeg < 3 , ' lnRpSeg1=1 AND lnRpSeg2=1 ' ,;
            IIF(lnRpTotSeg < 4 , ' lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 ' , IIF(lnRpTotSeg < 5,;
            ' lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 AND lnRpSeg4=1  ' , IIF(lnRpTotSeg < 6 ,;
            ' lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 AND lnRpSeg4=1 AND lnRpSeg5=1   '     ,;
             IIF(lnRpTotSeg < 7,' lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 AND lnRpSeg4=1 AND lnRpSeg5=1 AND lnRpSeg6=1 ',;
             '.T.'))))))
IF (&lcSegCond)

  lcRetVal = GLACCHAR.CACCNLDES
ELSE
  llUsedBr   = .F.
  IF !USED('GLSEGVAL')
    SELECT 0
    USE (oAriaApplication.DataDir+'GLSEGVAL')
    llUsedBr = .T.
  ENDIF
  SELECT GLSEGVAL
  SET ORDER TO TAG ACSSEGVAL

  =SEEK ('1'+SUBSTR(lcAcctCode,1,laSegInf[1,1]))
  lcRetVal = ALLTRIM(CSEGSHDES)

  lcRetTemp = lcRetVal

  FOR lnSegCount = 2 To lnRpTotSeg
    lcSegNo = 'lnRpSeg'+ALLTRIM(STR(lnSegCount))
    IF &lcSegNo = 1
      =SEEK (ALLTRIM(STR(lnSegCount))+SUBSTR(lcAcctCode,AT('-',lcAcctCode,lnSegCount-1)+1,laSegInf[lnSegCount,1]))
      lcRetVal = lcRetVal + '-'+ ALLTRIM(CSEGSHDES)
    ENDIF
  ENDFOR

  IF lcRetTemp = lcRetVal
    lcRetVal = GLACCHAR.CACCNLDES
  ENDIF

  *** Restore and close the files
  IF llUsedBr
    USE IN ALIAS('GLSEGVAL')
  ENDIF
ENDIF

SELECT (lcOldAlias)
GO lnOldRecNo
RETURN lcRetVal

*!************************************************************************
*!
*!      FUNCTION : lfSumCol
*!
*!************************************************************************
* Sum the column for the footer summation
FUNCTION lfSumCol
PARAMETERS lcChekVal,lnTypeOfSum
PRIVATE lnRecNo

lnRecNo = RECNO()


GO TOP IN ACCOD
lnNoSegts = ACCOD.Nacsnoseg
lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                   ACCOD.NACSSIZE , 'ACCSEGNO')


IF lnTypeOfSum         && Sum the main type
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
      SUM IIF(LEFT(CTYPECODE,1) $ 'A',NCOL&lcColCount,-NCOL&lcColCount) FOR LEFT(ctypecode,1) $ lcChekVal;
      AND;
      LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode)) ;
       TO lnSumm

      GO lnRecNO
      REPLACE NCOL&lcColCount WITH lnSumm
    ENDIF
  ENDFOR
ELSE
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
      SUM NCOL&lcColCount FOR ctypecode $ lcChekVal AND ;
      SUM NCOL&lcColCount FOR ctypecode $ lcChekVal AND ;
      LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode));
      TO lnSumm

*      SUM NCOL&lcColCount FOR ctypecode $ lcChekVal AND ;
      LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,oAriaApplication.Activecompanyid+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnColCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode));
      TO lnSumm

      GO lnRecNO
      REPLACE NCOL&lcColCount WITH lnSumm
    ENDIF
  ENDFOR
ENDIF
GO lnRecNO

*!************************************************************************
*!
*!      FUNCTION : lfSumMain
*!
*!************************************************************************
*
FUNCTION lfSumMain
PARAMETERS lcChekVal

lnRecNo = RECNO()
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
  lnSumm=0
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
      SUM NCOL&lcColCount FOR  lcChekVal $ CTYPECODE   TO lnSumm
      GO lnRecNO
      REPLACE NCOL&lcColCount WITH lnSumm
    ENDIF
  ENDFOR
GO lnRecNO

*!************************************************************************
*!
*!      FUNCTION : lfGetSumzBy
*!
*!************************************************************************
* Return the group of summarization
FUNCTION lfGetSumzBy

DO CASE
  CASE lnRpSumzBy = 1
    RETURN ('LEFT(CTYPECODE,1)+SPACE(4) AS CGROUP')
  CASE lnRpSumzBy = 2
    RETURN ('CTYPECODE +SPACE(2) AS CGROUP')
  CASE lnRpSumzBy = 3
    lcRetVal =  'SUBSTR(CACCTCODE,1,'+ALLTRIM(STR(laSegInf[1,1]))+')'
    FOR lnSegCount = 2 TO lnRpTotSeg
      lcSegNo = 'lnRpSeg' + ALLTRIM(STR(lnSegCount))
      IF &lcSegNo = 1
        lcRetVal = lcRetVal + "+SUBSTR(CACCTCODE," + ;
        ALLTRIM(STR(AT('-',lcRpSegMsk,lnSegCount-1)+1)) + "," +;
         ALLTRIM(STR(laSegInf[lnSegCount,1])) + ")"
      ENDIF
    ENDFOR
    RETURN lcRetVal+' AS CGROUP'
ENDCAS

*!************************************************************************
*!
*!      FUNCTION : lfSetRela
*!
*!************************************************************************
*
FUNCTION lfSetRela

IF !USED('GLBUDDT')
  SELECT 0
  USE (oAriaApplication.DataDir+'GLBUDDT')
ENDIF
SELECT GLBUDDT
SET ORDER TO TAG CDYRPRACC

IF !USED('GLACBALS')
  SELECT 0
  USE (oAriaApplication.DataDir+'GLACBALS')
ENDIF

lcTempPost = loOgScroll.lcTempPost
SELECT GLACBALS
SET ORDER TO TAG ACCYRPRD
IF lnDumPost <> '0'
  IF USED(lcTempPost)
    SELECT (lcTempPost)
    INDEX ON CACCTCODE+CFISFYEAR+CFSPPRDID TAG ACCYRPRD
    SET ORDER TO TAG ACCYRPRD
  ENDIF
ENDIF

SELECT (loOgScroll.lcRpTargt)

SET RELATION TO lcRpRelVar INTO GLBUDDT  ADDITIVE
SET RELATION TO lcRpRelVar INTO GLACBALS ADDITIVE

IF lnDumPost <> '0' AND USED(lcTempPost)
  SET RELATION TO lcRpRelVar INTO &lcTempPost ADDITIVE
ENDIF

***************************************************************************
* This part is the validations controle the tool provided with this report
***************************************************************************
*!************************************************************************
*!
*!      FUNCTION : lfBalSht
*!
*!************************************************************************
*
FUNCTION lfBalSht

lcRpTypeDes = '~Assets~Liability~Owner equity~'
lcRpTotDes  = '~ ~ ~ ~ ~'

DEFINE POPUP poTypes MARGIN SCROLL
DEFINE POPUP poCol MARGIN SCROLL
lsCol = 1

DIMENSION laTemMaType(1,6),laTemAType(1,6),laTemLType(1,6),laTemQType(1,6)
DIMENSION laTemCOl(1,24),laSegInf(1,2)

*--------- For Columns ------------------------------------

IF lnRpRepWid = 0
  lnRpInvNo = 0
  RETURN
ELSE
  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
ENDIF
*-------------------------------------------------------------------------

llFirTim   = .T.            && Enter the columns for the first time
lnRpCol    = 1              && Column Index
lcOldColType = ''           && Old column type
lnTypesNo = 0               && Number of types
llDoneArr = .F.
llSubType = .F.
lsTypes = 1
lcRpArrName = ' '
lnTpColUsd=lnRpColUsd
lcOldAlias = SELECT()
llUsed = .F.
IF NOT USED('GLTYPES')
  SELECT 0
  USE (oAriaApplication.DataDir+'GLTYPES') ORDER TAG TypeCode
  llUsed = .T.
ENDIF
SELECT GLTYPES
SET ORDER TO TAG TypeCode

llDataFound = .F.

FOR lnCount = 1 TO 3
 LOCATE FOR CTYPECODE=SUBSTR('ALQ',lnCount,1) AND !EMPT(cTypUacNo)
 IF FOUND()
   llDataFound = .T.
   EXIT
 ENDIF
ENDFOR
IF !llDataFound

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT 'There is no Data in type and ranges' WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_There_is_no_Data_in_type_and_ranges,oAriaApplication.GetHeaderText("LANG_There_is_no_Data_in_type_and_ranges",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]
  RETURN
ENDIF

IF EMPTY(laRpMaType)
  =lfColData()
ELSE
  =lfUpdData()
ENDIF
SELECT GLTYPES
IF llUsed
  USE
ENDIF
SELECT(lcOldAlias)

*!*	=lfDisData()
*!*	=lfDisCol()
=lfSaveArr()
*** Save the environment
PUSH KEY
lcOldProc = SET('PROCEDURE')
SET PROCEDURE TO

=lfGetActInf()
* Call only one program, maintian only one version, put an if statement to differentiat between the actions required for the two versions
DO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLHOSROW.FXP')

POP KEY
SET PROCEDURE TO (lcOldProc)

*!************************************************************************
*!
*!      FUNCTION : lfColData
*!
*!************************************************************************
* Collect Main types and Subtype from file
FUNCTION lfColData
* Select the main types from the file

lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70

SELECT LEFT(CTypeCode,1),CTypeDesc;
  FROM (oAriaApplication.DataDir+'GLTYPES');
  WHERE LEFT(CTypeCode,1) $ 'ALQ';
  AND   SUBSTR(CTypeCode,2,2) <> '00';
  ORDER BY CTypeCode;
  GROUP BY 1;
  INTO ARRAY laRpTemp

SET ENGINEBEHAVIOR lnEngineBehavior

IF _TALLY > 0
  lnTypesNo = ALEN(laRpTemp,1)*4

  SELECT GLTYPES
  lcSaveTag = TAG(VAL(SYS(21)))

  DIMENSION laRpMaType(ALEN(laRpTemp,1),6)

  FOR lnCount = 1 To ALEN(laRpTemp,1)
    SET ORDER TO TAG TYPECODE
    =lfInsType(laRpTemp[lnCount,1],laRpTemp[lnCount,2])
    lcRpArrName = "laRp"+laRpTemp[lnCount,1]+"Type"

    SET ORDER TO
    SELECT CTypeCode,CTypeDesc,' ',' ','','';
      FROM GLTYPES;
      WHERE CTypeCode LIKE laRpTemp[lnCount,1]+"__";
      AND   SUBSTR(CTypeCode,2) <> '00';
      ORDER BY CTypeCode;
      INTO ARRAY &lcRpArrName.
    *Acoumilate the inv.bot. numbers
    lnTypesNo = lnTypesNo + (ALEN(&lcRpArrName.,1)*4)
  ENDFOR
  IF !EMPTY(lcSaveTag)
    SET ORDER TO TAG lcSaveTag
  ENDIF
ENDIF

*!************************************************************************
*!
*!      FUNCTION : lfUpdData
*!
*!************************************************************************
* Update Main types and Subtype from file
FUNCTION lfUpdData

*** Delete non exited main type
lnMCount = 0
DO WHILE lnMCount < ALEN(laRpMaType,1)
  lnMCount = lnMCount + 1
  IF !SEEK(laRpMaType[lnMCount,1])
    =ADEL(laRpMaType,lnMCount)
    lnMCount = lnMCount - 1
    IF ALEN(laRpMaType,1) > 1
      DIMENSION laRpMaType(ALEN(laRpMaType,1)-1,6)
    ENDIF
  ELSE
    *** Delete non exited Subtype
    lcRpArrName = "laRp"+laRpMatype[lnMCount,1]+"Type"
    lnSCount = 0
    DO WHILE lnSCount <  ALEN(&lcRpArrName.,1)
      lnSCount = lnSCount + 1
      IF !EMPTY(&lcRpArrName)
        IF !SEEK(&lcRpArrName.[lnSCount,1])
          =ADEL(&lcRpArrName.,lnSCount)
          lnSCount = lnSCount - 1
          IF ALEN(&lcRpArrName.,1) > 1
            DIMENSION &lcRpArrName.(ALEN(&lcRpArrName.,1)-1,6)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    IF EMPTY(&lcRpArrName)
      =ADEL(laRpMaType,lnMCount)
      lnMCount = lnMCount - 1
      IF ALEN(laRpMaType,1) > 1
        DIMENSION laRpMaType(ALEN(laRpMaType,1)-1,6)
      ENDIF
    ENDIF
  ENDIF
ENDDO

*Select the new main type and insert them into the end of array
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT SUBSTR(CTypeCode,1,1) ,CTypeDesc;
  FROM (oAriaApplication.DataDir+'GLTYPES');
  WHERE LEFT(CTypeCode,1) $ 'ALQ' ;
  AND   SUBSTR(CTypeCode,2,2) <> '00';
  AND   ASCAN(laRpMaType,LEFT(CTypeCode,1)) = 0  ;
  ORDER BY CTypeCode;
  GROUP BY 1;
  INTO ARRAY laRpTemp
SET ENGINEBEHAVIOR lnEngineBehavior
IF _TALLY > 0

  DIMENSION laRpMaType(ALEN(laRpMaType,1)+ALEN(laRpTemp,1),6)
  FOR lnColCount  = 1 TO ALEN(laRpTemp)
    *** Inccrement the main array by 1

    =lfInsType(laRpTemp[lnColCount,1],laRpTemp[lnColCount,2])
  ENDFOR
ENDIF

FOR lnMCount = 1 TO ALEN(laRpMaType,1)
  lcRpArrName = "laRp"+laRpMatype[lnMCount,1]+"Type"
    SELECT CTypeCode,CTypeDesc,' ',' ','','';
    FROM GLTYPES;
    WHERE SUBSTR(CTypeCode,1,1) = laRpMatype[lnMCount,1];
    AND   SUBSTR(CTypeCode,2,2) <> '00';
    AND   ASCAN(&lcRpArrName.,CTypeCode) = 0  ;
    ORDER BY CTypeCode;
    INTO ARRAY lcRpSuTemp
  IF  _TALLY > 0
    FOR lnSCount = 1 TO ALEN(lcRpSuTemp,1)
      DIMENSION &lcRpArrName.(ALEN(&lcRpArrName.,1)+1,6)
      &lcRpArrName.[ALEN(&lcRpArrName.,1),1] = lcRpSuTemp[lnSCount,1]
      &lcRpArrName.[ALEN(&lcRpArrName.,1),2] = lcRpSuTemp[lnSCount,2]
      &lcRpArrName.[ALEN(&lcRpArrName.,1),3] = lcRpSuTemp[lnSCount,3]
      &lcRpArrName.[ALEN(&lcRpArrName.,1),4] = lcRpSuTemp[lnSCount,4]
      &lcRpArrName.[ALEN(&lcRpArrName.,1),5] = lcRpSuTemp[lnSCount,5]
    ENDFOR
  ENDIF
ENDFOR

*!************************************************************************
*!
*!      Function : lfInsType
*!
*!************************************************************************
* insert the main type into array
Function lfInsType
PARAMETER lcRpCode,lcRpDes

llFound = .F.
lnRpMPos = 0

DO WHILE !llFound
  lnRpMPos = lnRpMPos + 1
  IF EMPTY(laRpMaType[lnRpMPos,1])
    llFound = .T.
  ELSE
    IF AT(LEFT(laRpMaType[lnRpMPos,1],1),'SCEIT') > AT(lcRpCode,'SCEIT')
      llFound = .T.
    ENDIF
  ENDIF
ENDDO
=AINS(laRpMaType,lnRpMPos)
laRpMaType[lnRpMPos,1] = lcRpCode
laRpMaType[lnRpMPos,2] = lcRpDes
laRpMaType[lnRpMPos,3] = lfRpName(lcRpCode,lcRpTotDes)
laRpMaType[lnRpMPos,4] = ' '
laRpMaType[lnRpMPos,5] = ''
laRpMaType[lnRpMPos,6] = ''

* Search for user define description
IF !SEEK(lcRpCode+'00')
  * assign the system default description
  laRpMaType[lnRpMPos,2] = lfRpName(lcRpCode,lcRpTypeDes)
ELSE
  * Assign the user description
  laRpMaType[lnRpMPos,2] = IIF(!EMPTY(ALLTRIM(GLTYPES.CTypeDesc)),;
                   GLTYPES.CTypeDesc,' ')
ENDIF

*!************************************************************************
*!
*!      Function : lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue,lcRpVldEnt

RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'ALQ'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'ALQ')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'ALQ'))))


*---------------------------------------------------------------------
*------------------ Fill Array  --------------------------------------
*---------------------------------------------------------------------
*!************************************************************************
*!
*!      FUNCTION lfGetNewBar
*!
*!************************************************************************
*
FUNCTION lfGetNewBar
PARAMETERS lnOldBarNo

FOR lnPoCount = 1 TO CNTBAR('poArrTypes')
  IF GETBAR('poArrTypes',lnPoCount) = lnOldBarNo
    EXIT
  ENDIF
ENDFOR
RETURN lnPoCount

*!*************************************************************************
*!
*!         Function: lfMovShow
*!
*!*************************************************************************
* Mover show function
FUNCTION lfMovShow

IF ALEN('laTarget',1) = ALEN('laSource',1)  AND !EMPTY(laTarget[1,2]);
  OR EMPTY(laSource[1])
  SHOW GET lsSource     DISABLE
  SHOW GET pbMove       DISABLE
  SHOW GET pbAll        DISABLE
ELSE
  SHOW GET lsSource     ENABLE
  SHOW GET pbMove       ENABLE
  SHOW GET pbAll        ENABLE
ENDIF

IF EMPTY(laTarget[1,2])
  SHOW GET lsTarget    DISABLE
  SHOW GET pbRemove    DISABLE
  SHOW GET pbRAll      DISABLE
ELSE
  SHOW GET lsTarget    ENABLE
  SHOW GET pbRemove    ENABLE
  SHOW GET pbRAll      ENABLE
ENDIF

*!*************************************************************************
*!
*!              Function: lfvSrc
*!
*!*************************************************************************
*
FUNCTION lfvSrc

* Change the name of the function from lfvSource to lfvSrc
IF lsSource <= ALEN('laSource',1) AND lsSource <> 0
  SET SKIP OF BAR lsSource OF puSource .T.

  IF !EMPTY(laTarget[1,2])
    DIMENSION laTarget[ALEN(laTarget,1)+1,6]
  ENDIF
  laTarget[ALEN(laTarget,1),lnDesPos]=laSource[lsSource,lnCoPos]
  laTarget[ALEN(laTarget,1),lnCoPos]= laSource[lsSource,lnDesPos]

ENDIF

lnStart  = lsSource
lsSource = 0

FOR lnCount = lnStart TO CNTBAR('puSource')
  IF !SKPBAR('puSource',lnCount)
    lsSource = lnCount
    EXIT
  ENDIF
ENDFOR

IF lsSource = 0
  FOR lnCount = 1 TO CNTBAR('puSource')
    IF !SKPBAR('puSource',lnCount)
      lsSource = lnCount
      EXIT
    ENDIF
  ENDFOR
ENDIF

_CUROBJ = OBJNUM(lsSource)

*!*************************************************************************
*!
*!         Function: lfvTrgt
*!
*!*************************************************************************
*
FUNCTION lfvTrgt

IF lsTarget <= ALEN('laTarget',1) AND lsTarget <> 0
  lsSource = 1
  FOR lnSorCount = 1 TO ALEN(laSource,1)
    IF laSource[lnSorCount,1] = laTarget[lsTarget,2]
      lsSource = lnSorCount
      EXIT
    ENDIF
  ENDFOR

*  lsSource  = T('laSource',lnRowPos,1)
  SET MARK OF POPUP puSource .F.
  SET SKIP OF BAR lsSource OF puSource .F.

  =ADEL(laTarget,lsTarget)
  IF ALEN(laTarget,1) > 1
    DIMENSION laTarget[ALEN(laTarget,1)-1,6]
  ELSE
    laTarget[1,lnDesPos] =' '
    laTarget[1,lnCoPos] =' '
  ENDIF
ENDIF


_CUROBJ = OBJNUM(lsTarget)

*!*************************************************************************
*!
*!        Function : lfwCol
*!
*!*************************************************************************
*
FUNCTION lfwCol

IF CNTBAR('poCol') = 0
  IF LASTKEY() = 9 OR LASTKEY() = 4
    _CUROBJ = _CUROBJ + 1
  ELSE
    _CUROBJ = _CUROBJ - 1
  ENDIF
ENDIF

*!*************************************************************************
*!
*!         Function : lfBigNo
*!
*!*************************************************************************
* Return the biggest number in the serten row in array from column lnFromCol
* to column lnToCol
FUNCTION lfBigNo
PARAMETERS lcPaArrName,lnRowNo,lnFromCol,lnToCol

lnBigNo = 0
FOR lnArrIndx = lnFromCol TO lnToCol
   lnBigNo=MAX(lnBigNo,&lcPaArrName[lnRowNo,lnArrIndx])
ENDFOR
RETURN lnBigNo

*!*************************************************************************
*!
*!          Function : lfShowColTp
*!
*!*************************************************************************
*
FUNCTION lfShowColTp

IF llFirTim
  SHOW GETS WINDOW lwColTpB DISABLE ONLY
  SHOW GETS WINDOW lwColTpA DISABLE ONLY
  SHOW GETS WINDOW lwColTpO DISABLE ONLY

  ACTIVATE  WINDOW lwColTp&laRpCol[lnRpCol,1] TOP
  SHOW GETS WINDOW lwColTp&laRpCol[lnRpCol,1] ENABLE ONLY
  llFirTim = .F.
ENDIF

FOR lnColCount = 4 TO 7
  lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
  &lcCbIdent =  IIF(lnColCount = 4,laRpCol[lnRpCol,lnColCount],laRpCol[lnRpCol,lnColCount]-laRpCol[lnRpCol,lnColCount-1])
  SHOW GET (lcCbIdent)
ENDFOR

IF lnRpNoCol+lnRpIdnCol-lnNewCol+lfBigNo('laRpCol',lnRpCol,4,7)+laRpCol[lnRpCol,23]  >= lnRpInvNo
  IF laRpCol[lnRpCol,23] = 0
    SHOW GET laRpCol[lnRpCol,23] DISABLE
  ELSE
    SHOW GET laRpCol[lnRpCol,23] ENABLE
  ENDIF
  FOR lnColCount = 4 TO 7
    lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
    IF &lcCbIdent = 0
      SHOW GET (lcCbIdent) DISABLE
    ENDIF
  ENDFOR
  llFirTim = .F.
ELSE
  llFirTim = .F.
  FOR lnColCount = 4 TO 7
    lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
    SHOW GET (lcCbIdent) ENABLE
  ENDFOR
ENDIF

*=lfvCol12()

IF laRpCol[lnRpCol,23] = 0
  SHOW GET laRpCol[lnRpCol,22] DISABLE
ELSE
  SHOW GET laRpCol[lnRpCol,22] ENABLE
ENDIF

lcOldColType = laRpCol[lnRpCol,1]

IF lnRpNOCol < 3
  SHOW GET rbColmntype,3 DISABLE
ELSE
  SHOW GET rbColmntype,3 ENABLE
ENDIF


*-------------------------------------------------------------------*
*-------------------------------------------------------------------*
*--------- Data Validation for column information ------------------*
*----- Start with 'lfvCol' + the column no in the array ------------*
*-like lfvCol13 ------> the valid function for laRpCol[lnRpCol,13]--*
*-------------------------------------------------------------------*
*-------------------------------------------------------------------*


*!*************************************************************************
*!
*!           Function : lfvCol10
*!
*!*************************************************************************
*
FUNCTION lfvCol10
PARAMETERS loHosColFormSet,loFld
lcRpCurFld      = loFld.ControlSource

*IF !BETWEEN(PADL(laRpCol[lnRpCol,10],2,'0'),'01','13') AND ;
        IIF(EMPTY(laRpCol[lnRpCol,11]),.T.,laRpCol[lnRpCol,11]<laRpCol[lnRpCol,10])
IF !BETWEEN(PADL(laRpCol[lnRpCol,10],2,'0'),'01','13')
  RETURN .F.
ENDIF

*!*************************************************************************
*!
*!          Function : lfvCol11
*!
*!*************************************************************************
*
FUNCTION lfvCol11

IF !BETWEEN(PADL(laRpCol[lnRpCol,11],2,'0'),PADL(laRpCol[lnRpCol,10],2,'0'),'13')
  RETURN .F.
ENDIF

*---------------------------------------------------------------------
*---------------------------------------------------------------------
*------------ This part for control Ok or Cancel in the --------------
*------------------------ Main Screen --------------------------------
*---------------------------------------------------------------------
*---------------------------------------------------------------------
*!*************************************************************************
*!
*!           Function : lfSaveArr
*!
*!*************************************************************************
* Save all arrays information for retreving if push cancel
FUNCTION lfSaveArr

DIMENSION laTemMaType(ALEN(laRpMaType,1),6)
=ACOPY(laRpMaType,laTemMaType)

DIMENSION laTemAType(ALEN(laRpAType,1),6)
=ACOPY(laRpAType,laTemAType)

DIMENSION laTemLType(ALEN(laRpLType,1),6)
=ACOPY(laRpLType,laTemLType)

DIMENSION laTemQType(ALEN(laRpQType,1),6)
=ACOPY(laRpQType,laTemQType)

DIMENSION laTemCol(ALEN(laRpCol,1),24)
=ACOPY(laRpCol,laTemCol)

*!*************************************************************************
*!
*!          Function : lfvCancAll
*!
*!*************************************************************************
* Cancel all information that was added
FUNCTION lfvCancAll
PARAMETERS loSummFormSet

DIMENSION laRpMaType(ALEN(laTemMaType,1),6)
=ACOPY(laTemMaType,laRpMaType)

DIMENSION laRpAType(ALEN(laTemAType,1),6)
=ACOPY(laTemAType,laRpAType)

DIMENSION laRpLType(ALEN(laTemLType,1),6)
=ACOPY(laTemLType,laRpLType)

DIMENSION laRpQType(ALEN(laTemQType,1),6)
=ACOPY(laTemQType,laRpQType)

DIMENSION laRpCol(ALEN(laTemCol,1),24)
=ACOPY(laTemCol,laRpCol)
lnRpColUsd=lnTpColUsd

loSummFormSet.Release


*!*************************************************************************
*!
*!          Function : lfGetColInf
*!
*!*************************************************************************
*
FUNCTION lfGetColInf
PRIVATE lnColNo,lnColCount,lnColIndent,lcColString,lnIndent,lnBudAct

STORE 0 TO lnColNo,lnColCount,lnColIndent,lnBudAct
lnColIndent=1
lcColString=[,ST,SF,MT,MF,]
STORE '' TO laColInf

FOR  lnColNo = 1 TO ALEN(laRpCol,1)
     IF laRpCol[lnColNo,1] $ 'AB'
       lnTmpInd_7=laRpCol[lnColNo,7]
       lnTmpInd_6=laRpCol[lnColNo,6]
       lnTmpInd_5=laRpCol[lnColNo,5]
       lnTmpInd_4=laRpCol[lnColNo,4]
       IF lnRpSumzBy=1
         laRpCol[lnColNo,7] = IIF(laRpCol[lnColNo,7]>0,1,0)
         laRpCol[lnColNo,6] = 0
         laRpCol[lnColNo,5] = 0
         laRpCol[lnColNo,4] = 0
       ENDIF
       lnBudAct=lnColNo
       lnIndent=MAX(1,IIF(laRpCol[lnColNo,23]=1,1,0)+laRpCol[lnColNo,7]+1)
       laColInf[1,lnColIndent]='RS'+ALLTRIM(STR(lnBudAct))
       FOR lnColCount = 1 TO 4
         lnWhichcol=IIF(lnColCount>2,IIF(lnRpSumzBy=1,2,0),0)
         laColInf[1,lnColIndent+laRpCol[lnColNo,lnColCount+3]]=;
         laColInf[1,lnColIndent+laRpCol[lnColNo,lnColCount+3]]+;
         IIF(EMPTY(laColInf[1,lnColIndent+laRpCol[lnColNo,lnColCount+3]]),'',',');
         +SUBSTR(lcColString,ATC(',',lcColString,lnColCount)+1,;
               ATC(',',lcColString,lnColCount+1)-ATC(',',lcColString,lnColCount)-1)+;
               +ALLTRIM(STR(lnBudAct))
       ENDFOR
       IF laRpCol[lnColNo,23]=1
         laColInf[1,lnColIndent+lnIndent-1]='PS,'+ALLTRIM(STR(lnBudAct));
             +','+ALLTRIM(STR(lnColNo))
       ENDIF
     ELSE
       laColInf[1,lnColIndent]='OP,';
                +ALLTRIM(STR(laRpCol[lnColNo,19]))+',';
                +ALLTRIM(STR(laRpCol[lnColNo,20]))+',';
                +ALLTRIM(STR(lnColNo))+',';
                +ALLTRIM(STR(laRpCol[lnColNo,21]))
     ENDIF
   lnColIndent=MIN(lnRpColUsd,lnColIndent+IIF(laRpCol[lnColNo,1] $ 'AB',IIF(laRpCol[lnColNo,23]=1,1,0)+laRpCol[lnColNo,7]+1,1))
   laRpCol[lnColNo,7]=lnTmpInd_7
   laRpCol[lnColNo,6]=lnTmpInd_6
   laRpCol[lnColNo,5]=lnTmpInd_5
   laRpCol[lnColNo,4]=lnTmpInd_4
ENDFOR

*!*************************************************************************
*!
*!           Function : lfGetVal
*!
*!*************************************************************************
*
FUNCTION lfGetVal
PARAMETERS lnColNo

lcColTyp=LEFT(cGroup,2)
lcGroup=ALLTRIM(cGroup)
IF EMPTY(lcColTyp)
  RETURN ''
ENDIF
DO CASE
  CASE lcColTyp = 'LN'
     RETURN IIF(RIGHT(lcGroup,2) $ laColInf[1,lnColNo] ;
     OR ('PS' $ laColInf[1,lnColNo]) OR ('OP' $ laColInf[1,lnColNo]);
     ,REPLICATE('-',lnRpColWid),' ')

  CASE  ('RS' $ laColInf[1,lnColNo]) AND !INLIST(lcColTyp,'MT','ST','SF','MF')
    lcColTyp='RS'
    lcColNo=SUBSTR(laColInf[1,lnColNo],ATC(lcColTyp,laColInf[1,lnColNo])+2)
    lnComPos=ATC(',',lcColNo)
    lcColNo=IIF(lnComPos>1,SUBSTR(lcColNo,1,lnComPos-1),SUBSTR(lcColNo,1))
    lnRetVal=nCol&lcColNo
  CASE  ('PS' $ laColInf[1,lnColNo])
     lcColNo=SUBSTR(laColInf[1,lnColNo],4,ATC(',',laColInf[1,lnColNo],2)-4)
     lnRetVal=nCol&lcColNo/laRpMainTo[EVAL(;
             RIGHT(laColInf[1,lnColNo],LEN(laColInf[1,lnColNo]);
             -RAT(',',laColInf[1,lnColNo])))]*100
  CASE  'OP' $ laColInf[1,lnColNo]
     lcColNo=SUBSTR(laColInf[1,lnColNo],4,ATC(',',laColInf[1,lnColNo],2)-4)
     lcCol2=RIGHT(laColInf[1,lnColNo],LEN(laColInf[1,lnColNo]);
              -RAT(',',laColInf[1,lnColNo]))
    lcCol1=SUBSTR(laColInf[1,lnColNo],ATC(',',laColInf[1,lnColNo],2)+1,;
               ATC(',',laColInf[1,lnColNo],3)-ATC(',',laColInf[1,lnColNo],2)-1)
    lccommnd= 'ncol&lcCol1' + IIF(lcColno='1','-',IIF(lcColNo='2','+','/'));
              +'ncol&lcCol2'+ IIF(lcColno='3','*100','')
    lnRetVal=EVAL(lcCommnd)
  CASE  (lcColTyp $ laColInf[1,lnColNo])
    lcColNo=SUBSTR(laColInf[1,lnColNo],ATC(lcColTyp,laColInf[1,lnColNo])+2)
    lnComPos=ATC(',',lcColNo)
    lcColNo=IIF(lnComPos>1,SUBSTR(lcColNo,1,lnComPos-1),SUBSTR(lcColNo,1))
    lnRetVal= nCol&lcColNo
  OTHERWISE
        RETURN ' '
ENDCASE
lnRPTempCol=lnRpColWid
lnRpColWid=lnRpColWid -LEN(SET('CURR',1))
lcRPTemPic=REPL('?',lnRpColWid)
lcRPTemPic=IIF(lnRpColDis> 0,STRTRAN(lcRpTemPic,'?','.',lnRpColWid-lnRpColDis,1),lcRPTemPic)
lcRpColPic=''
lnNoDecPoint = IIF(lnRpColDis>0,lnRpColWid-lnRpColDis-1,lnRpColWid)
FOR lnColCount = 1 TO (lnNoDecPoint)-MOD(lnNoDecPoint,4)
  lcRpColPic=lcRpColPic+IIF(MOD(lnColCount,4)=1,',','9')
ENDFOR
lcRpColPic=PADL(lcRpColPic,lnNoDecPoint,'9')
lcRpColPic=STUFF(lcRPTemPic,1,LEN(lcRpColPic),lcRpColPic)
lcRpMarker = IIF(('PS' $ laColInf[1,lnColNo] OR 'OP' $ laColInf[1,lnColNo]),'','@$ ')
lcRetVal=IIF(LIKE(lcRpTemPic,STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis)),;
            TRANS(lnRetVal/lnRpRound,lcRpMarker+STRTRAN(lcRpColPic,'?','9')),;
            IIF(('PS' $ laColInf[1,lnColNo]),'N/A';
            ,STRTRAN(lcRpColPic,'?','*')))

*lcRetVal=IIF(LIKE(lcRpTemPic,STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis)),;
            TRANS(lnRetVal/lnRpRound,'@$ '+lcRpColPic),;
            IIF(('PS' $ laColInf[1,lnColNo]),'N/A';
            ,STRTRAN(lcRpTemPic,'?','*')))

lnRpColWid=lnRPTempCol
IF ('PS' $ laColInf[1,lnColNo] OR 'OP' $ laColInf[1,lnColNo]) AND LEFT(STR(lnRetVal),1)='*'
*  RETURN IIF(SET('CURR')='LEFT',SPACE(LEN(SET('CURR',1))),'')+STRTRAN(lcRpColPic,'?','*')+IIF(SET('CURR')='RIGHT',SPACE(LEN(SET('CURR',1))),'')
   RETURN PADL('N/A',lnRpColWid)
ENDIF
IF LEFT(STR(lnRetVal),1)='*'
  RETURN IIF(SET('CURR')='LEFT',SET('CURR',1),'')+STRTRAN(lcRpTemPic,'?','*')+IIF(SET('CURR')='RIGHT',SET('CURR',1),'')
ENDIF

RETURN PADL(ALLTRIM(lcRetVal),lnRpColWid)
*IIF(LIKE(lcRpTemPic,STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis)),;
            STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis),;
            STRTRAN(lcRpTemPic,'?','*'))

*!*************************************************************************
*!
*!          Function : lfGetHead
*!
*!*************************************************************************
*
FUNCTION lfGetHead
PARAMETERS lnColNo,lnTitNo

IF !('RS' $ laColInf[1,lnColNo]) ;
     AND ('PS' $ laColInf[1,lnColNo]) AND !('OP' $ laColInf[1,lnColNo])
     RETURN ' '
ELSE
   IF 'RS' $ laColInf[1,lnColNo]
      lcColTyp='RS'
      lcColNo=SUBSTR(laColInf[1,lnColNo],ATC(lcColTyp,laColInf[1,lnColNo])+2)
        lnComPos=ATC(',',lcColNo)
        lcColNo=IIF(lnComPos>1,SUBSTR(lcColNo,1,lnComPos-1),SUBSTR(lcColNo,1))
   ELSE
     lcColNo=SUBSTR(laColInf[1,lnColNo],ATC(',',laColInf[1,lnColNo],3)+1,;
               ATC(',',laColInf[1,lnColNo],4)-ATC(',',laColInf[1,lnColNo],3)-1)
   ENDIF
  RETURN  IIF(TYPE(lcColNo)<>'N','',laRpCol[EVAL(lcColNo),lnTitNo+1])
ENDIF

*!*************************************************************************
*!
*!          Function : lfGetActInf
*!
*!*************************************************************************
*
FUNCTION lfGetActInf


lcOldAlias = SELECT()


*IF !USED('SYCACCOD')
*  SELECT 0
*  USE &gcSysHome.SYCACCOD
*ENDIF
*SELECT SYCACCOD
IF !USED('ACCOD')
  SELECT 0
  USE (oAriaApplication.DataDir+'ACCOD')
ENDIF
SELECT ACCOD


*E300789,7 [BEGIN] OPEN WITH NO ORDER AND GO TOP INSTEAD OF SEEK
*SET ORDER TO TAG COMPID
*SEEK oAriaApplication.Activecompanyid
GO TOP
*E300789,7 [END..]

lcSegMask    = CACSMASK
lnRpTotSeg   = NACSNOSEG
DIMENSION laSegInf[1,2]
STORE '' TO laSegInf

SELECT NACSSIZE,CACSSHDES;
FROM (oAriaApplication.DataDir+'ACCOD') ;
  WHERE NACSSEGSZ < 1 ;
  INTO ARRAY laSegInf
*E300789,7 [END..]


SELECT (lcOldAlias)
RETURN lcSegMask

*!*************************************************************************
*!
*!          Function : lfGetPic
*!
*!*************************************************************************
*
FUNCTION lfGetPic

RETURN ;
       SUBSTR(ALLTRIM(lcRpSegMsk),AT('-',lcRpSegMsk,1)+1,24)

*!*************************************************************************
*!
*!          Function : lfGetSegDis
*!
*!*************************************************************************
*
FUNCTION lfGetSegDis

lcRpRetVal = ''
FOR lnSegCount = 2 TO lnRpTotSeg-1
  lcRpRetVal = lcRpRetVal + ALLTRIM(laSegInf[lnSegCount,2])+'-'
ENDFOR
RETURN (lcRpRetVal + ALLTRIM(laSegInf[lnRpTotSeg,2]))

*!*************************************************************************
*!
*!           Function : lfDesWidth
*!
*!*************************************************************************
*
FUNCTION lfDesWidth

lcObjName = VARREAD()
IF &lcObjName
  lnRpDesWid=lnRpRowTit+IIF(llRpSubInd,lnRpSubInd,0)+IIF(llRpAccInd,lnRpAccInd,0)
  laOGObjCnt[_CUROBJ+1] = .T.
ELSE
  IF lcObjName = 'LLRPSUBIND'
    lnRpSubInd = 0
  ELSE
    lnRpAccInd = 0
  ENDIF
  laOGObjCnt[_CUROBJ+1] = .F.
ENDIF
=lfActvateWin(lnWinDisp)

*!*************************************************************************
*!
*!          Function : lfRowDes
*!
*!*************************************************************************
*
FUNCTION lfRowDes

DO CASE
  CASE (CGROUP ='H' AND RIGHT(CTYPECODE,2)='00') OR 'MT' $ CGROUP
    RETURN cRowDes
  CASE (CGROUP ='H' AND RIGHT(CTYPECODE,2)<>'00') OR 'ST' $ CGROUP
     RETURN SPACE(IIF(llRpSubInd,lnRpSubInd,0))+cRowDes
  OTHERWISE
     RETURN  SPACE(IIF(llRpSubInd,lnRpSubInd,0))+SPACE(IIF(llRpAccInd,lnRpAccInd,0))+crowdes
ENDCASE

*!*************************************************************************
*!
*!          Function : lfRepShow
*!
*!*************************************************************************
*
FUNCTION lfRepShow

IF llRpSubInd
  laOGObjCnt[OBJNUM(lnRpSubInd)] = .T.
ELSE
  laOGObjCnt[OBJNUM(lnRpSubInd)] = .F.
  lnRpSubInd = 0
ENDIF

IF llRpAccInd
  laOGObjCnt[OBJNUM(lnRpAccInd)] = .T.
ELSE
  laOGObjCnt[OBJNUM(lnRpAccInd)] = .T.
  lnRpAccInd = 0
ENDIF

*!*************************************************************************
*!
*!          Function : lfUpDaCol
*!
*!*************************************************************************
*
FUNCTION lfUpDaCol

IF !EMPTY(laRpCol)
  lnRpCol = 1
  DIMENSION laUpDaCol(1,2)
  DIMENSION laSegInf(1,2)
  lnUpCount = 0
  DIMENSION laFldName(ALEN(laRpCol,2))

  FOR lnCount = 1 TO ALEN(laRpCol,2)
    laFldName[lnCount] = laRpCol[lnRpCol,lnCount]
  ENDFOR

  *** Get segment information
  =lfGetActInf()

  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lnUpCount = lnUpCount + 1
      *** Redimension the array
      IF lnUpCount>ALEN(laUpDaCol,1)
        DIMENSION laUpDaCol(lnUpCount,2)
      ENDIF
      laUpDaCol[lnUpCount,1] = ALLTRIM(laRpCol[lnColCount,2])+' '+ALLTRIM(laRpCol[lnColCount,3])
      laUpDaCol[lnUpCount,2] = lnColCount
    ENDIF
  ENDFOR

  DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLBUPDAT.SCX')
ENDIF

*!*************************************************************************
*!
*!          Function : lfwUpDaCol
*!
*!*************************************************************************
*
FUNCTION lfwUpDaCol

lnRpCol = laUpDaCol[lsUpDaCol,2]
FOR lnCount = 1 TO ALEN(laRpCol,2)
  laFldName[lnCount] = laRpCol[lnRpCol,lnCount]
ENDFOR

SHOW GETS WINDOW lwColTpB DISABLE ONLY
SHOW GETS WINDOW lwColTpA DISABLE ONLY
ACTIVATE  WINDOW lwColTp&laRpCol[lnRpCol,1] TOP
SHOW GETS WINDOW lwColTp&laRpCol[lnRpCol,1] ENABLE ONLY
SHOW GETS
=lfvCangeTy()

*!*************************************************************************
*!
*!         Function : lfvCangeTy
*!
*!*************************************************************************
*
FUNCTION lfvCangeTy

DO CASE
  CASE laFldName[12] = 1 AND laFldName[1] = 'A'
    SHOW GET laFldName[13]  ENABLE
    SHOW GET laFldName[14]  ENABLE
    SHOW GET laFldName[15]  DISABLE
    SHOW GET laFldName[16]  DISABLE
    SHOW GET laFldName[17]  DISABLE
    SHOW GET laFldName[18]  DISABLE
  CASE laFldName[12] = 2 AND laFldName[1] = 'A'
    SHOW GET laFldName[13]  DISABLE
    SHOW GET laFldName[14]  DISABLE
    SHOW GET laFldName[15]  ENABLE
    SHOW GET laFldName[16]  ENABLE
    SHOW GET laFldName[17]  ENABLE
    SHOW GET laFldName[18]  ENABLE
ENDCASE

*!*************************************************************************
*!
*!         Function : lfvOkUpd
*!
*!*************************************************************************
*
FUNCTION lfvOkUpd

PRIVATE llRetVal
llRetVal = .T.
DO CASE
  CASE laRpCol[lnRpCol,1] = 'B'
    IF EMPTY(laFldName[8]) OR EMPTY(laFldName[9]) OR;
       EMPTY(laFldName[10]) OR EMPTY(laFldName[11])
      llRetVal = .F.
    ENDIF
  CASE laFldName[1]  = 'A'
    IF laFldName[12] = 1
      IF EMPTY(laFldName[13]) OR EMPTY(laFldName[14])
        llRetVal = .F.
      ENDIF
    ELSE
      IF EMPTY(laFldName[15]) OR EMPTY(laFldName[16]) OR;
         EMPTY(laFldName[17]) OR EMPTY(laFldName[18])
        llRetVal = .F.
      ENDIF
    ENDIF
ENDCASE
IF !llRetVal

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT 'You have to enter the column information' WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_have_to_enter_the_column_information,oAriaApplication.GetHeaderText("LANG_You_have_to_enter_the_column_information",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]
  _CUROBJ = _CUROBJ
ELSE
  FOR lnCount = 1 TO ALEN(laRpCol,2)
    laRpCol[lnRpCol,lnCount] = laFldName[lnCount]
  ENDFOR
  CLEAR READ
ENDIF
*---------------------------------------------------------------------
*---------------------------------------------------------------------
*----------------- EOF THE END OF PROGRAM LIST -----------------------
*---------------------------------------------------------------------
*---------------------------------------------------------------------
* EOF

***********************************************

*!*************************************************************************
*!
*!          Function : lfGetCurPr
*!
*!*************************************************************************
*
FUNCTION lfGetCurPr

DECLARE laRpRetFld(1)

loFld = loOgScroll.ActiveControl
IF ALLTRIM(loFld.Value) == ALLTRIM(loFld.OldValue)
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = ALLTRIM(loFld.OldValue)


* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
&& Check If year field is empty
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
    llUsedsyc = .f.


  IF NOT USED("FISHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FISHD')
    llUsedsyc = .T.
  ENDIF

  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD') ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD

  lcYearVal = LEFT(lcRpCurFld,6)+'Year'
  *** Search for the current company+year+Prd
  *E300789,7 [BEGIN]
  *E IF '?' $ &lcRpCurFld. OR !SEEK(oAriaApplication.Activecompanyid+&lcYearVal+&lcRpCurFld)
  *E   =gfBrows(IIF(EMPTY(&lcYearVal),[oAriaApplication.Activecompanyid],[oAriaApplication.Activecompanyid+&lcYearVal]),'CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
     IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
       * N000682 ,1 Thabet Handle globalization issues [Start]
       *=gfBrows(IIF(EMPTY(&lcYearVal),'',[&lcYearVal]),'CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
       =gfBrows(IIF(EMPTY(&lcYearVal),'',[&lcYearVal]),'CFSPPRDID',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISCYEAR,oAriaApplication.GetHeaderText("LANG_FISCYEAR",AHEADERFILE)),.F.)
       * N000682 ,1 Thabet Handle globalization issues [End]
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
    ENDIF
  ELSE
      &lcRpCurFld         = CFSPPRDID
  ENDIF
IF lcRpStYear+lcRpStPrid>lcRpEdYear+lcRpEdPrid

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT 'Beginning year/period must be less than or equal to the Ending year/period' window nowait
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Beginning,oAriaApplication.GetHeaderText("Lang_Beginning",AHEADERFILE)) window nowait
  * N000682 ,1 Thabet Handle globalization issues [END]
  &lcRpCurFld=lcRpOld
  RETURN .F.
ENDIF
IF lcOGManRep <> 'GLINCST4' AND lcOGManRep <> 'GLBALS13'
  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpStYear , 'COMPFYEAR')

  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpEdYear , 'COMPFYEAR')

  lcTmpStPr  = lcRpStPrid
  lcTmpEdPr  = lcRpEdPrid
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol

  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
  IF lnNoOfCol<=lnRpInvNo
    lnRpColUsd = lnNoOfCol
  ELSE

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *WAIT 'The report width not enough for this range of periods' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_The_report_width,oAriaApplication.GetHeaderText("LANG_The_report_width",AHEADERFILE)) WINDOW NOWAIT
    * N000682 ,1 Thabet Handle globalization issues [END]
    lcRpStPrid  = lcTmpStPr
    lcRpEdPrid  = lcTmpEdPr
    &lcRpCurFld = lcRpOld
  ENDIF
ENDIF
  IF llUsedsyc

    USE IN FISHD

  ENDIF
  IF llUsedBefo       && .F.- this file used by the system

    USE IN FSPRD

  ENDIF
  SELECT (lcOldAlias)
RETURN

*!*************************************************************************
*!
*!         Function : lfvCurYear
*!
*!*************************************************************************
*
FUNCTION lfvCurYear

loFld = loOgScroll.ActiveControl
IF ALLTRIM(loFld.Value) == ALLTRIM(loFld.OldValue)
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = ALLTRIM(loFld.OldValue)

DECLARE laRpRetFld(1)

llUsedBefo = .F.        && Check if used before or this the first time
lcOldAlias = SELECT()    && Save the current alias

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
				'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]

laRpRetFld[1] = ''
&& Check If year field is empty
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
    llUsedsyc = .f.

  IF NOT USED("FISHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FISHD')
    llUsedsyc = .T.
  ENDIF

  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD') ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
	=gfBrows('','CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISCYEAR,oAriaApplication.GetHeaderText("LANG_FISCYEAR",AHEADERFILE)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [End]
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
    ENDIF
  ELSE
      &lcRpCurFld         = CFisFyear
  ENDIF
IF lcRpStYear>lcRpEdYear

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT 'Beginning year must be less than or equal to the Ending year' window nowait
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Beginning_year,oAriaApplication.GetHeaderText("LANG_Beginning_year",AHEADERFILE)) window nowait
  * N000682 ,1 Thabet Handle globalization issues [END]
  &lcRpCurFld=lcRpOld
  RETURN .F.
ENDIF
IF lcOGManRep <> 'GLINCST4' AND lcOGManRep <> 'GLBALS13'

  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpStYear ,;
                       'COMPFYEAR')

  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpEdYear ,;
                      'COMPFYEAR')

  lcTmpStPr  = lcRpStPrid
  lcTmpEdPr  = lcRpEdPrid
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol

  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
  IF lnNoOfCol<=lnRpInvNo
    lnRpColUsd = lnNoOfCol
  ELSE

    * N000682 ,1 Thabet Handle globalization issues [Start]
	*    WAIT 'The report width not enough for this range of periods' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_The_report_width,oAriaApplication.GetHeaderText("LANG_The_report_width",AHEADERFILE)) WINDOW NOWAIT
    * N000682 ,1 Thabet Handle globalization issues [END]
    lcRpStPrid  = lcTmpStPr
    lcRpEdPrid  = lcTmpEdPr
    &lcRpCurFld = lcRpOld
  ENDIF
ENDIF
  IF llUsedsyc

    USE IN FISHD

  ENDIF
 IF llUsedBefo       && .F.- this file used by the system

   USE IN FSPRD

 ENDIF
 SELECT (lcOldAlias)
RETURN

*!************************************************************************
*!
*!          FUNCTION : lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurYr

llCompUsd=.f.
IF !USED('SYCCOMP')
  USE (oAriaApplication.SysPath+'SYCCOMP') IN SELECT(1)
  llCompUsd=.T.
ENDIF

SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(oAriaApplication.Activecompanyid,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

************************************************************
*! Name      : lfvSegArr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/15/2012
*! Purpose   : valid fn for the segment ID
************************************************************
FUNCTION lfvSegArr
DIMENSION laSegInf[1,2]
LOCAL lcSegs,i
lcRpSeg = ALLTRIM(lcRpSeg)
=lfGetActInf()
lcSegs = ''
FOR i=1 TO ALEN(laSegInf,1)
  lcSegs = lcSegs + ALLTRIM(laSegInf[i,2]) + ','
ENDFOR
lcSegs = LEFT(lcSegs,LEN(lcSegs)-1)
laSegInf[1,2]
lnSegNo=ASCAN(laSegInf,lcRpSeg)
IF lnSegNo = 0
  * 42138 Invalid ð.

* N000682 ,1 Thabet Handle globalization issues [Start]
*  gfModalGen('INM42138B00000','DIALOG','Valid Segment IDs are:'+lcSegs)
gfModalGen('INM42138B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Valid_Segment_IDs_are,oAriaApplication.GetHeaderText("LANG_Valid_Segment_IDs_are",AHEADERFILE)) +lcSegs)
* N000682 ,1 Thabet Handle globalization issues [END]
  lcRpSeg = loOgScroll.ActiveControl.OldValue
ENDIF
*- End of lfvSegArr.


************************************************************
*! Name      : lfSegVal
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/18/2012
*! Purpose   : Get segment value of the 2nd segment part
************************************************************
FUNCTION lfSegVal
LOCAL lcRet
lcRet = ''
DIMENSION laSegInf[1,2]
=lfGetActInf()
IF ALEN(laSegInf,1)>=2
  *IF EMPTY(lcRpSeg)
    lcRet=ALLTRIM(laSegInf[2,2])
  *ELSE
  *  lcRet = lcRpSeg
  *ENDIF
ELSE
 lcRet=SPACE(LEN(ALLTRIM(laSegInf[1,2])))
ENDIF
RETURN lcRet
*- End of lfSegVal.


*!*************************************************************************
*!
*!         Function : lfGetSegLs
*!
*!*************************************************************************
* Get segment information and return the @M picture
FUNCTION lfGetSegLs

*** Call the function that let laSegInf hold the information
DIMENSION laSegInf[1,2]
=lfGetActInf()
lcSegPic = ''
FOR lnSegCount = 2 TO ALEN(laSegInf,1)-1
  lcSegPic = lcSegPic +  ALLTRIM(laSegInf[lnSegCount,2]) +','
ENDFOR
lcSegPic = '@M '+lcSegPic +  ALLTRIM(laSegInf[ALEN(laSegInf,1),2])
* I don't understand how this works , I just added the segment picture value as freformat text
RETURN REPLICATE('X',15)


*!************************************************************************
*!
*!      FUNCTION : lfvSelSegV
*!
*!************************************************************************
*
******** MULTI SEGMENT VALUES
FUNCTION  lfvSelSegV
DIMENSION laSelSegV[1],laSegInf[1,2]

lcRpSeg = ALLTRIM(lcRpSeg)
lcSetProc=SET('PROCEDURE')
SET PROCEDURE TO
=lfGetActInf()
lnSegNo=ASCAN(laSegInf,lcRpSeg)
lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
lnSegNo=STR(lnSegNo,1)
SELECT cSegValue;
      FROM (oAriaApplication.DataDir+'glsegval');
      WHERE CACSSEGNO=lnSegNo;
      INTO ARRAY laSelSegV
DO WHILE .T.
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *=lfOGMover(@laSelSegV,@laRpSegV,'Select Segment Values',.T.,'')
  =lfOGMover(@laSelSegV,@laRpSegV,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTSEG,oAriaApplication.GetHeaderText("LANG_SELECTSEG",AHEADERFILE)),.T.,'')
  * N000682 ,1 Thabet Handle globalization issues [End]
  IF ALEN(laRpSegV,1)>8

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *    WAIT 'Maximum number of segment values is 8' window nowait
    WAIT  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Maximum_number,oAriaApplication.GetHeaderText("LANG_Maximum_number",AHEADERFILE)) window nowait
    * N000682 ,1 Thabet Handle globalization issues [END]
  ELSE
    EXIT
  ENDIF
ENDDO
SET PROCEDURE TO &lcSetProc
IF lcOGManRep= 'GLINCST2'
  lnRpColUsd = IIF(EMPTY(laRpSegV[1]),0,ALEN(laRpSegV,1))
ENDIF
**************************

*!*************************************************************************
*!
*!          Function : lfvOneYear
*!
*!*************************************************************************
* This function valid the only one year
FUNCTION lfvOneYear
PARAMETER lcOtherFld

loFld = _Screen.ActiveForm.ActiveControl

IF loFld.Value == loFld.OldValue
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = loFld.OldValue

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H= "'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty


  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD') ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD


  *** Search for the current company+year+Prd
  *E300789,7 [BEGIN]
  *IF '?' $ &lcRpCurFld. OR !SEEK(oAriaApplication.Activecompanyid+&lcRpCurFld)
  *  =gfBrows([oAriaApplication.Activecompanyid],'CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
   IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
	=gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISCYEAR,oAriaApplication.GetHeaderText("LANG_FISCYEAR",AHEADERFILE)) ,.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
  *E300789,7 [END..]
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE
      &lcRpCurFld         = CFisFyear
  ENDIF

IF llUsedBefo       && .F.- this file used by the system

  *USE IN SYCFSPRD
  USE IN FSPRD

ENDIF
SELECT (lcOldAlias)
RETURN

*!*************************************************************************
*!
*!          Function : lfvOnePrd
*!
*!*************************************************************************
* This function valid the only one period
FUNCTION lfvOnePrd
PARAMETER lcOtherFld

loFld = _Screen.ActiveForm.ActiveControl
IF loFld.Value == loFld.OldValue
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = loFld.OldValue

DECLARE laRpRetFld(2)


* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty


  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD') ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD


  lcYearVal = LEFT(lcRpCurFld,6)+'Year'
  *** Search for the current company+year+Prd
  *E300789,7 [BEGIN]
  *IF '?' $ &lcRpCurFld. OR !SEEK(oAriaApplication.Activecompanyid+&lcYearVal+&lcRpCurFld)
  *  =gfBrows(IIF(EMPTY(&lcOtherFld),[oAriaApplication.Activecompanyid],[oAriaApplication.Activecompanyid+&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)

  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows(IIF(EMPTY(&lcOtherFld),'',[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
    =gfBrows(IIF(EMPTY(&lcOtherFld),'',[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISCYEAR,oAriaApplication.GetHeaderText("LANG_FISCYEAR",AHEADERFILE)) ,.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
  *E300789,7 [END..]
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE
      &lcRpCurFld         = CFSPPRDID
  ENDIF

IF llUsedBefo       && .F.- this file used by the system

  *USE IN SYCFSPRD
  USE IN FSPRD

ENDIF
SELECT (lcOldAlias)
RETURN

*!*************************************************************************
*!
*!          Function : lfvBudCode
*!
*!*************************************************************************
*
FUNCTION lfvBudCode
PARAMETER lcOtherFld

loFld = loOgScroll.ActiveControl
IF ALLTRIM(loFld.Value) == ALLTRIM(loFld.OldValue)
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = ALLTRIM(loFld.OldValue)

DECLARE laRpRetFld(2)


* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
lcBrFields    = 'CBUDCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget_code,oAriaApplication.GetHeaderText("LANG_Budget_code",AHEADERFILE))+;
                '",CBUDYEAR:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+'",CBUDDES:H="'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'GLBUDHD') ORDER TAG BdCodYr
    llUsedBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. ) .OR. ;
     (EMPTY(&lcOtherFld) AND !SEEK(&lcRpCurFld)) .OR. ;
     (!EMPTY(&lcOtherFld) AND !SEEK(&lcRpCurFld+&lcOtherFld))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
    =gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDANDFIS,oAriaApplication.GetHeaderText("LANG_BUDANDFIS",AHEADERFILE)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld = laRpRetFld[1]
    *** Budget year variable
    &lcOtherFld = laRpRetFld[2]

  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!*************************************************************************
*!
*!           Function : lfvBudYear
*!
*!*************************************************************************
*
FUNCTION lfvBudYear
PARAMETER lcOtherFld

loFld = _Screen.ActiveForm.ActiveControl
IF loFld.Value == loFld.OldValue
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = loFld.OldValue

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
lcBrFields    = 'CBUDCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget_code,oAriaApplication.GetHeaderText("LANG_Budget_code",AHEADERFILE))+;
                '",CBUDYEAR:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",AHEADERFILE))+;
'" ,CBUDDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'GLBUDHD') ORDER TAG BdCodYr
    llUsedBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR.;
    !SEEK(&lcOtherFld+ALLTRIM(&lcRpCurFld.)))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
	=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget_code,oAriaApplication.GetHeaderText("LANG_Budget_code",AHEADERFILE))+' & Fiscal year ',.F.)
    * N000682 ,1 Thabet Handle globalization issues [ENd]
    &lcRpCurFld  = laRpRetFld[2]
    &lcOtherFld  = laRpRetFld[1]

  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!*************************************************************************
*!
*!          Function  : lfvBudPer
*!
*!*************************************************************************
*
FUNCTION lfvBudPer

loFld = loOgScroll.ActiveControl
IF ALLTRIM(loFld.Value) == ALLTRIM(loFld.OldValue)
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = loFld.OldValue
lcRpSeg = ALLTRIM(lcRpSeg)

&lcRpCurFld. = PADL(ALLTRIM(&lcRpCurFld.),2,'0')
IF !EMPTY(&lcRpCurFld) .AND. !BETWEEN(VAL(&lcRpCurFld),1,13)
  *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[Start]
  *WAIT [Invalid period. Period ranges between '01','13'] WINDOW NOWAIT
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDPERIOD,oAriaApplication.GetHeaderText("LANG_INVALIDPERIOD",AHEADERFILE))  NOWAIT
  *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[End]
  &lcRpCurFld = loFld.OldValue
  RETURN .F.
ENDIF

*!*************************************************************************
*!
*!          Function  : lfDefBudCo
*!
*!*************************************************************************
*
FUNCTION lfDefBudCo

*** If it is used by me
llMyUse = .F.
*** Save the old workarea
lcOldAlias = SELECT()

lcBudCode = ' '

IF !USED('GLSETUP')
  llMyUse = .T.
  SELECT 0
  USE (oAriaApplication.DataDir+'GLSETUP')
ENDIF
SELECT GLSETUP
GO TOP
lcBudCode = GLSETUP.CSETDEBUD

IF llMyUse
  USE IN GLSETUP
ENDIF


*** Select the old workarea
SELECT(lcOldAlias)

RETURN lcBudCode

*!*************************************************************************
*!
*!          Function : lfvSegVal
*!
*!*************************************************************************
*
FUNCTION lfvSegVal

loFld = loOgScroll.ActiveControl
IF ALLTRIM(loFld.Value) == ALLTRIM(loFld.OldValue)
  RETURN
ENDIF
lcRpCurFld = loFld.Parent.cOgArray
lcRpOld  = loFld.OldValue
lcRpSeg = ALLTRIM(lcRpSeg)

DIMENSION laSegInf[1,2]
=lfGetActInf()
lnSegNo=ASCAN(laSegInf,lcRpSeg)
lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
DECLARE laRpRetFld(1)
* N000682 ,1 Thabet Handle globalization issues [Start]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = 'CSEGVALUE:H="'+LANG_Segment_value+'"'
lcBrFields    = 'CSEGVALUE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Segment_value,oAriaApplication.GetHeaderText("LANG_Segment_value",AHEADERFILE))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*lcBrFields    = 'CSEGVALUE:H="Segment value"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLSEGVAL")
    SELECT 0
    USE (oAriaApplication.DataDir+'GLSEGVAL') ORDER TAG ACSSEGVAL
    llUsedBefo = .T.
  ENDIF
  SELECT GLSEGVAL
  *** Search for the current Segment no.+segment value
  IF ('?' $ &lcRpCurFld. .OR.;
    !SEEK(STR(lnSegNo,1)+&lcRpCurFld.))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([STR(lnSegNo,1)],'CSEGVALUE',"laRpRetFld",'Segment values ',.F.)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfBrows([STR(lnSegNo,1)],'CSEGVALUE',"laRpRetFld",LANG_Segment_value,.F.)
=gfBrows([STR(lnSegNo,1)],'CSEGVALUE',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Segment_value,oAriaApplication.GetHeaderText("LANG_Segment_value",AHEADERFILE)),.F.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld  = laRpRetFld[1]

  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLSEGVAL
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!*************************************************************************
*!
*!          Function : lfvPercent
*!
*!*************************************************************************
*
FUNCTION lfvPercent

lnRpColUsd = 1+IIF(lnRpPerc= 0,0,1)+IIF(lnRpIdent=0,0,3)
lnRpRepWid=IIF(lnRpIdent=0 .AND. lnRpPerc= 0,80,132)
lnRpColWid=IIF(lnRpIdent=0 .AND. lnRpPerc= 0,18,16)
DO CASE
  CASE lnRpIdent<>0 .AND. lnRpPerc<> 0
    lnRpDesWid = 45
    lnRpFirIde = 1
    lnRpColIDe = 1
  CASE lnRpIdent<>0
    lnRpDesWid = 45
    lnRpColWid = 18
    lnRpFirIde = 3
    lnRpColIDe = 2
  CASE lnRpPerc<> 0
    lnRpDesWid = 65
    lnRpColWid = 18
    lnRpFirIde = 5
    lnRpColIDe = 5
ENDCASE
*****************************************************************

*!*************************************************************************
*!
*!          Function : lfInitCol
*!
*!*************************************************************************
*
FUNCTION lfInitCol
lcRpSeg = ALLTRIM(loogscroll.lcRpSeg)
llRetVal = .T.
DO CASE
   CASE lcOGManRep= 'GLBALSH3'
     lnCount = 1
     STORE 0 TO laRpCol
     STORE 'A'        TO laRpCol[lnCount,1]
     laRpCol[1,4]=IIF(lnRpIdent=0,0,1)
     laRpCol[1,5]=IIF(lnRpIdent=0,0,1)
     laRpCol[1,6]=IIF(lnRpIdent=0,0,2)
     laRpCol[1,7]=IIF(lnRpIdent=0,0,3)
     STORE PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPrid,lnRpColWid) TO laRpCol[lnCount,2]
     STORE PADL(lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]
     STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
     *** Replace YTD or PTD
     STORE lnRpYOrP   TO laRpCol[lnCount,12]
     STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
     STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
     IF lnRpPerc <> 0
       STORE 1 TO laRpCol[lnCount,23]
       STORE lnRpPerc TO laRpCol[lnCount,22]
     ELSE
       STORE 0 TO laRpCol[lnCount,23]
     ENDIF
     STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]

   CASE lcOGManRep= 'GLBALSH5' OR lcOGManRep= 'GLBALSH6'
     lnRpColUsd=3
     DIMENSION laRpCol[3,24]
     STORE 0 TO laRpCol
     FOR lnCount = 1 TO IIF(lcOGManRep= 'GLBALSH5',2,3)
       lcColNo=STR(lnCount,1)
       STORE 'A'        TO laRpCol[lnCount,1]
       STORE '' TO laRpCol[lnCount,2]
       STORE PADC(lcRpStPr&lcColNo+'/'+lcRpStYr&lcColNo,lnRpColWid) TO laRpCol[lnCount,3]
       STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
       *** Replace YTD or PTD
       STORE 1   TO laRpCol[lnCount,12]
       STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
       STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
       STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
    ENDFOR
      IF lcOGManRep= 'GLBALSH5'
         STORE 'O'        TO laRpCol[3,1]
         STORE ' Percentage' TO laRpCol[3,2]
         STORE 'Col1 & Col2'  TO laRpCol[3,3]
         STORE 3          TO laRpCol[3,19]
         STORE 1          TO laRpCol[3,20]
         STORE 2          TO laRpCol[3,21]
     ENDIF
     =lfGetActInf()
     lnSegNo=ASCAN(laSegInf,lcRpSeg)
     lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
     lnSegPos=IIF(lnSegNo>1,ATC('-',lcRpSegMsk,lnSegNo-1)+1,1)
     lnSegLen=laSegInf[lnSegNo,1]
     lcPRExp=''
     IF !EMPTY(laRpSegV[1])
       lcPRExp=[SUBSTR(GLACCHAR.CACCTCODE,]+ALLTRIM(STR(lnSegPos))+','+ALLTRIM(STR(lnSegLen))+;
               [) IN  (]
       FOR lnSegVToAdd = 1 TO ALEN(laRpSegV,1)
          lcPRExp=lcPRExp+IIF(lnSegVToAdd>1,',','')+'"'+laRpSegV[lnSegVToAdd]+'"'
       ENDFOR
       lcPRExp=lcPRExp+IIF(EMPTY(lcPRExp),'',')')
     ENDIF

   CASE lcOGManRep= 'GLBALSH7'
     IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr) OR EMPTY(lcRpBudPr)
       llRetVal = .F.
       =gfModalGen('TRM02221B00000','DIALOG')
     ELSE
       lnRpColUsd=3
       DIMENSION laRpCol[3,24]
       STORE 0 TO laRpCol
       FOR lnCount = 1 TO 2
         lcColNo=STR(lnCount,1)
         STORE IIF(lnCount=1,'A','B')     TO laRpCol[lnCount,1]
         STORE IIF(lnCount=1,PADC(lcRpStPr&lcColNo,lnRpColWid),'Budget '+lcrpBudCod) TO laRpCol[lnCount,2]
         STORE IIF(lnCount=1,PADC(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+lcRpBudPr) TO laRpCol[lnCount,3]
         STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
         *** Replace YTD or PTD
         IF lnCount=1
           STORE 1   TO laRpCol[lnCount,12]
           STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
           STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
         ELSE
           STORE lcRpBudCod   TO laRpCol[lnCount,8]
           STORE lcRpBudYr TO laRpCol[lnCount,9]
           STORE VAL(lcRpBudPr) TO laRpCol[lnCount,10],laRpCol[lnCount,11]
         ENDIF
         STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
        ENDFOR
        STORE 'O'        TO laRpCol[3,1]
        STORE ' Percentage' TO laRpCol[3,2]
        STORE 'Col1 & Col2'  TO laRpCol[3,3]
        STORE 3          TO laRpCol[3,19]
        STORE 1          TO laRpCol[3,20]
        STORE 2          TO laRpCol[3,21]

        =lfGetActInf()
        lnSegNo=ASCAN(laSegInf,lcRpSeg)
        lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
        lnSegPos=IIF(lnSegNo>1,ATC('-',lcRpSegMsk,lnSegNo-1)+1,1)
        lnSegLen=laSegInf[lnSegNo,1]
        lcPRExp=''
        IF !EMPTY(laRpSegV[1])
          lcPRExp=[SUBSTR(GLACCHAR.CACCTCODE,]+ALLTRIM(STR(lnSegPos))+','+ALLTRIM(STR(lnSegLen))+;
                  [) IN  (]
          FOR lnSegVToAdd = 1 TO ALEN(laRpSegV,1)
             lcPRExp=lcPRExp+IIF(lnSegVToAdd>1,',','')+'"'+laRpSegV[lnSegVToAdd]+'"'
          ENDFOR
          lcPRExp=lcPRExp+IIF(EMPTY(lcPRExp),'',')')
        ENDIF
      ENDIF
    CASE lcOGManRep= 'GLBALSH8'
      IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr) OR EMPTY(lcRpBudPr)
        llRetVal = .F.
        =gfModalGen('TRM02221B00000','DIALOG')
      ELSE
        lnRpColUsd=3
        DIMENSION laRpCol[3,24]
        STORE 0 TO laRpCol
        FOR lnCount = 1 TO 3
          lcColNo=STR(lnCount,1)
          STORE IIF(lnCount<3,'A','B')     TO laRpCol[lnCount,1]
          STORE IIF(lnCount<3,PADL(lcRpStPr&lcColNo,lnRpColWid),'Budget '+lcrpBudCod) TO laRpCol[lnCount,2]
          STORE IIF(lnCount<3,PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+lcRpBudPr) TO laRpCol[lnCount,3]
          STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
          *** Replace YTD or PTD
          IF lnCount<3
            STORE 1   TO laRpCol[lnCount,12]
            STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
            STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
          ELSE
            STORE lcRpBudCod   TO laRpCol[lnCount,8]
            STORE lcRpBudYr TO laRpCol[lnCount,9]
            STORE VAL(lcRpBudPr) TO laRpCol[lnCount,10],laRpCol[lnCount,11]
          ENDIF
          STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
        ENDFOR
        =lfGetActInf()
        lnSegNo=ASCAN(laSegInf,lcRpSeg)
        lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
        lnSegPos=IIF(lnSegNo>1,ATC('-',lcRpSegMsk,lnSegNo-1)+1,1)
        lnSegLen=laSegInf[lnSegNo,1]
        lcPRExp=''
        IF !EMPTY(laRpSegV[1])
          lcPRExp=[SUBSTR(GLACCHAR.CACCTCODE,]+ALLTRIM(STR(lnSegPos))+','+ALLTRIM(STR(lnSegLen))+;
                  [) IN  (]
          FOR lnSegVToAdd = 1 TO ALEN(laRpSegV,1)
             lcPRExp=lcPRExp+IIF(lnSegVToAdd>1,',','')+'"'+laRpSegV[lnSegVToAdd]+'"'
          ENDFOR
          lcPRExp=lcPRExp+IIF(EMPTY(lcPRExp),'',')')
        ENDIF
      ENDIF
    CASE lcOGManRep= 'GLBALSH9'
      IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr1) OR EMPTY(lcRpBudPr1);
        OR EMPTY(lcrpBudCo2) OR EMPTY(lcRpBudYr2) OR EMPTY(lcRpBudPr2)
        llRetVal = .F.
        =gfModalGen('TRM02221B00000','DIALOG')
      ELSE
        lnRpColUsd=2
        DIMENSION laRpCol[2,24]
        STORE 0 TO laRpCol
        FOR lnCount = 1 TO 2
          lcColNo=STR(lnCount,1)
          STORE 'B'                     TO laRpCol[lnCount,1]
          STORE 'Budget '+lcRpBudCod    TO laRpCol[lnCount,2]
          *** Store the budget year and budget peroid header
          STORE lcRpBudYr&lcColNo+'/'+lcRpBudPr&lcColNo TO laRpCol[lnCount,3]
          STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11],laRpCol[lnCount,12]
          *** Store the budget year and budget peroid header
          STORE lcRpBudCod   TO laRpCol[lnCount,8]
          STORE lcRpBudYr&lcColNo TO laRpCol[lnCount,9]
          STORE VAL(lcRpBudPr&lcColNo) TO laRpCol[lnCount,10],laRpCol[lnCount,11]
          STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
        ENDFOR
        =lfGetActInf()
        lnSegNo=ASCAN(laSegInf,lcRpSeg)
        lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
        lnSegPos=IIF(lnSegNo>1,ATC('-',lcRpSegMsk,lnSegNo-1)+1,1)
        lnSegLen=laSegInf[lnSegNo,1]
        lcPRExp=''
        IF !EMPTY(laRpSegV[1])
          lcPRExp=[SUBSTR(GLACCHAR.CACCTCODE,]+ALLTRIM(STR(lnSegPos))+','+ALLTRIM(STR(lnSegLen))+;
                  [) IN  (]
          FOR lnSegVToAdd = 1 TO ALEN(laRpSegV,1)
             lcPRExp=lcPRExp+IIF(lnSegVToAdd>1,',','')+'"'+laRpSegV[lnSegVToAdd]+'"'
          ENDFOR
          lcPRExp=lcPRExp+IIF(EMPTY(lcPRExp),'',')')
        ENDIF
      ENDIF
   CASE lcOGManRep= 'GLBALS12'
     =lfGetActInf()
     lnSegNo=ASCAN(laSegInf,lcRpSeg)
     lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
      DIMENSION laRpCol[3,24]
      STORE 0 TO laRpCol
      FOR lnCount = 1 TO 3
        STORE IIF(lnCount<3,'A','O')     TO laRpCol[lnCount,1]
        STORE PADC(lcRpStPrid+'/'+lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]
        *** Store headers
        DO CASE
          CASE lnCount = 1
            STORE IIF(EMPTY(lcRpAcMask),PADL('Total company',lnRpColWid),lcRpSeg+' '+lcRpAcMask) TO laRpCol[lnCount,2]
            STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
            IF !EMPTY(lcRpAcMask)
              lnSegPos=IIF(lnSegNo>2,ATC('-',laRpCol[lnCount,24],lnSegNo-2)+1,1)
              lnSegLen=laSegInf[lnSegNo,1]
              STORE STUFF(STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*');
                 ,lnSegPos,lnSegLen,ALLTRIM(lcRpAcMask)) TO laRpCol[lnCount,24]
            ENDIF
          CASE lnCount = 2
            STORE PADL('Total company',lnRpColWid) TO laRpCol[lnCount,2]
            STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
          CASE lnCount = 3
            STORE 'All other accounts' TO laRpCol[lnCount,2]
            STORE 1          TO laRpCol[3,19]
            STORE 2          TO laRpCol[3,20]
            STORE 1          TO laRpCol[3,21]
        ENDCASE
        STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11],laRpCol[lnCount,12]

        *** Replace YTD or PTD
        STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
        STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
      ENDFOR

    CASE lcOGManRep= 'GLBALS13'
      lnRpColUsd = 3
      =lfGetActInf()
      lnSegNo=ASCAN(laSegInf,lcRpSeg)
      lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
      DIMENSION laRpCol[3,24]
      STORE 0 TO laRpCol
      FOR lnCount = 1 TO 3
        STORE IIF(lnCount<3,'A','O')     TO laRpCol[lnCount,1]
        *STORE PADL('PTD '+lcRpStPrid+'/'+lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]
        STORE lcRpStprid+'/'+lcRpStYear +' To '+ lcRpEdprid+'/'+lcRpEdYear TO laRpCol[lnCount,3]
        *** Store headers
        DO CASE
          CASE lnCount = 1
            STORE IIF(EMPTY(lcRpAcMask),PADL('Total company',lnRpColWid),lcRpSeg+' '+lcRpAcMask) TO laRpCol[lnCount,2]
            STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
            IF !EMPTY(lcRpAcMask)
              lnSegPos=IIF(lnSegNo>2,ATC('-',laRpCol[lnCount,24],lnSegNo-2)+1,1)
              lnSegLen=laSegInf[lnSegNo,1]
              STORE STUFF(STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*');
                   ,lnSegPos,lnSegLen,ALLTRIM(lcRpAcMask)) TO laRpCol[lnCount,24]
            ENDIF
          CASE lnCount = 2
            STORE PADL('Total company',lnRpColWid) TO laRpCol[lnCount,2]
            STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
          CASE lnCount = 3
            STORE 'All other accounts' TO laRpCol[lnCount,2]
            STORE 1          TO laRpCol[3,19]
            STORE 2          TO laRpCol[3,20]
            STORE 1          TO laRpCol[3,21]
        ENDCASE
        STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11],laRpCol[lnCount,12]

        STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
        STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]

        *** Replace YTD or PTD
*        STORE lcRpStPrid TO laRpCol[lnCount,15]
*        STORE lcRpEdPrid TO laRpCol[lnCount,17]
*        STORE lcRpStYear TO laRpCol[lnCount,16]
*        STORE lcRpEdYear TO laRpCol[lnCount,18]
      ENDFOR

 ENDCASE

RETURN llRetVal

*!*************************************************************************
*!
*!          Function : lfPrepair
*!
*!*************************************************************************
*
FUNCTION lfPrepair
* ?? use this to add variables to the SYREPUVR
DO CASE
  CASE lcOGRepID = 'GLBALSH3'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE lcOGRepID = 'GLBALSH5'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLBALSH6'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLBALSH7'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLBALSH8'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLBALSH9'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE lcOGRepID = 'GLBALS12'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE lcOGRepID = 'GLBALS13'
    lnRpRepWid = 132
    lnRpColWid = 18
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
ENDCASE

*!*************************************************************************
*!
*!          Function : lfPrepair1
*!
*!*************************************************************************
*
FUNCTION lfPrepair1

DO CASE
  CASE ALLTRIM(UPPER(PROMPT())) == 'TOTAL COMPANY'
    lcOGRepID = 'GLBALSH3'
    lcOGManRep= 'GLBALSH3'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL VS. OTHER PERIOD ACTUAL"
    lcOGRepID = 'GLBALSH5'
    lcOGManRep= 'GLBALSH5'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL PERIODS COMPARISON"
    lcOGRepID = 'GLBALSH6'
    lcOGManRep= 'GLBALSH6'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL VS. OTHER PERIOD BUDGET"
    lcOGRepID = 'GLBALSH7'
    lcOGManRep= 'GLBALSH7'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL COL. VS. ACTUAL AND BUDGET COL."
    lcOGRepID = 'GLBALSH8'
    lcOGManRep= 'GLBALSH8'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "TWO BUDGET COLUMN COMPARISON"
    lcOGRepID = 'GLBALSH9'
    lcOGManRep= 'GLBALSH9'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE ALLTRIM(UPPER(PROMPT())) == "SINGLE SEGMENT VS. REST OF COMPANY"
    lcOGRepID = 'GLBALS12'
    lcOGManRep= 'GLBALS12'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "SGL. SEG. VS. REST OF CO. RANGE OF PERIODS"
    lcOGRepID = 'GLBALS13'
    lcOGManRep= 'GLBALS13'
    lnRpRepWid = 132
    lnRpColWid = 18
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
ENDCASE

*!************************************************************************
*!
*!      FUNCTION : lfModiFile
*!
*!************************************************************************
*
FUNCTION lfModiFile
PRIVATE lcOldVal,lcNewVal

  DIMENSION laLibSum[1,ALEN(laRpCol,1)]
  STORE 0 TO laLibSum

SELECT GLACCHAR
SET ORDER TO ACCTCODE

SELECT (lcWorkFile)
SET RELATION TO CACCTCODE  INTO GLACCHAR ADDITIVE
GO TOP
IF lnRpSumzBy = 1
  lcOldVal = LEFT(CTYPECODE,1)

  SCAN
    lcNewVal = LEFT(CTYPECODE,1)
    lnRowPos = ASCAN(laRpMaType,lcOldVal)
    *** Put the main description into file
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
    IF lcOldVal <> LEFT(CTYPECODE,1)
      lnRowPos = ASCAN(laRpMaType,lcOldVal)
      INSERT BEFORE BLANK
      lnRowPos = ASCAN(laRpMaType,lcOldVal)

      IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'MF1'

        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
        ENDIF
        INSERT BLANK
      ENDIF
      IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'MF2'

        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
        ENDIF
        *** Added blank line before new main type
        INSERT  BLANK
      ENDIF
    ENDIF
    IF lcOldVal = 'Q'
*      INSERT  BLANK
      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH lcRpNetDes
      REPLACE CGROUP WITH 'MF2'
      =lfSumQRow('SETIC')

      DIMENSION laNetInc[1,ALEN(laRpCol,1)]
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        *E301077,71 [BEGIN]
        * This condition (IF laRpCol[lnTmpCount,1] $ 'BA') controled
        * the creation of the temp file at the bigening of the program
        * each time this condition is true A NEW FIELD is added to the tempfile
        * with name NCOL1,2,3,..... and so on
        * so we must check this condition now .
*E        lcColCount = ALLTRIM(STR(lnTmpCount))
*E        laNetInc[1,lnTmpCount] = NCOL&lcColCount
        IF laRpCol[lnTmpCount,1] $ 'BA'
          lcColCount = ALLTRIM(STR(lnTmpCount))
          laNetInc[1,lnTmpCount] = NCOL&lcColCount
        ENDIF
        *E301077,71 [END..]
      ENDFOR
      INSERT  BLANK

      INSERT  BLANK
      lcRowDesc = ''
      lnRowPos = ASCAN(laRpMaType,'Q')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+lcTmpTitl
      ENDIF

      lnRowPos = ASCAN(laRpMaType,'L')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+IIF(EMPTY(lcRowDesc),'',' AND ')+lcTmpTitl
      ELSE
       lcRowDesc = lcTmpTitl
      ENDIF

      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH 'Total '+lcRowDesc,CGROUP WITH 'MF'
      *=lfSumCol('QL',.T.)
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        lcColCount = ALLTRIM(STR(lnTmpCount))
        IF laRpCol[lnTmpCount,1] $ 'BA'
          *REPLACE  NCOL&lcColCount WITH NCOL&lcColCount+laNetInc[1,lnTmpCount]
          REPLACE  NCOL&lcColCount WITH laLibSum[1,lnTmpCount]+laNetInc[1,lnTmpCount]
        ENDIF
      ENDFOR



    ENDIF
    lcOldVal = lcNewVal
  ENDSCAN
  lnRowPos = ASCAN(laRpMaType,lcOldVal)
  IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'MF1'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
    ENDIF
  ENDIF
  IF !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
    INSERT BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'MF2'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
    ENDIF
  ENDIF
  IF lcOldVal = 'Q'
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH lcRpNetDes
    REPLACE CGROUP WITH 'MF2'
    =lfSumQRow('SETIC')
      DIMENSION laNetInc[1,ALEN(laRpCol,1)]
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        *E301077,71 [BEGIN]
        * This condition (IF laRpCol[lnTmpCount,1] $ 'BA') controled
        * the creation of the temp file at the bigening of the program
        * each time this condition is true A NEW FIELD is added to the tempfile
        * with name NCOL1,2,3,..... and so on
        * so we must check this condition now .
*E        lcColCount = ALLTRIM(STR(lnTmpCount))
*E        laNetInc[1,lnTmpCount] = NCOL&lcColCount
        IF laRpCol[lnTmpCount,1] $ 'BA'
          lcColCount = ALLTRIM(STR(lnTmpCount))
          laNetInc[1,lnTmpCount] = NCOL&lcColCount
        ENDIF
        *E301077,71 [END..]
      ENDFOR



      INSERT  BLANK
      lcRowDesc = ''
      lnRowPos = ASCAN(laRpMaType,'Q')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+lcTmpTitl
      ENDIF

      lnRowPos = ASCAN(laRpMaType,'L')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+IIF(EMPTY(lcRowDesc),'',' AND ')+lcTmpTitl
      ELSE
       lcRowDesc = lcTmpTitl
      ENDIF

      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH 'Total '+lcRowDesc,CGROUP WITH 'MF'
      *=lfSumCol('QL',.T.)
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        lcColCount = ALLTRIM(STR(lnTmpCount))
        IF laRpCol[lnTmpCount,1] $ 'BA'
          *REPLACE  NCOL&lcColCount WITH NCOL&lcColCount+laNetInc[1,lnTmpCount]
          REPLACE  NCOL&lcColCount WITH laLibSum[1,lnTmpCount]+laNetInc[1,lnTmpCount]
        ENDIF
      ENDFOR


  ENDIF

ELSE
***  For the subtype or account summarization level
  lcOldVal  = LEFT(CTYPECODE,1)
  lcOldVal2 = CTYPECODE
  lcNewVal  = lcOldVal
  lcNewVal2 = lcOldVal2

  lnRowPos = ASCAN(laRpMaType,lcOldVal)

  *** Put the main description into file
  INSERT  BEFORE BLANK
  REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]),;
          CTYPECODE WITH lcOldVal+'00',;
          CGROUP WITH 'H'

  *#** ADDED blank line after main type
  INSERT BLANK

  IF lnRpSumzBy = 3
    INSERT   BLANK
    REPLACE CGROUP WITH 'H'
  ELSE
    SKIP 1
  ENDIF

  *** Put the Subtype description
  lcRpArrName = 'laRp'+lcOldVal+'Type'
  lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
  REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2]),;
          CTYPECODE WITH lcOldVal2
  IF lnRpSumzBy = 3
    *#** ADDED blank line after subtype
    INSERT BLANK
  ENDIF

  llFirsTime = .T.

  SCAN
    IF llFirsTime
      IF lnRpSumzBy = 3
        SKIP 4
      ELSE
        SKIP 3
      ENDIF
      llFirsTime = .F.
    ENDIF

    IF lnRpSumzBy = 3
      REPLACE  CRowDES WITH lfGetAcDes(CACCTCODE)
    ENDIF

    lcNewVal  = IIF(EMPTY(LEFT(CTYPECODE,1)),lcNewVal,LEFT(CTYPECODE,1))
    lcNewVal2 = IIF(EMPTY(CTYPECODE),lcNewVal2,CTYPECODE)
    lcRpArrName = 'laRp'+lcOldVal+'Type'
    *** Insert subtotal of subType

    IF lcOldVal2 <> lcNewVal2
      lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
      INSERT BEFORE BLANK
      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
      ENDIF

      IF lnRpSumzBy = 3
        REPLACE CGROUP WITH 'LN,ST'
        INSERT  BLANK
        REPLACE CRowDES WITH PADL('SubTotal of '+IIF(lnRowPos = 0,'',ALLTRIM(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
        REPLACE CGROUP WITH 'STS'
        =lfSumMain(lcOldVal2)
        INSERT BLANK
        IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
          REPLACE CGROUP WITH 'LN,SF'
          *****
          INSERT  BLANK
        ENDIF
      ENDIF
      lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)

      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5]) OR;
         !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
        REPLACE CGROUP WITH 'SF1'
        IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
          =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5],'|','00,')+'00')
        ENDIF
        INSERT  BLANK
      ENDIF

      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6]) OR;
         !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
        REPLACE CGROUP WITH 'LN,SF'
        INSERT  BLANK
        lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
        REPLACE CGROUP WITH 'SF2'
        IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6])
          =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6],'|','00,')+'00')
        ENDIF
        INSERT  BLANK
      ENDIF

      *** Put subtype header if the summarization is account level and
      *** there are more subtypes in this main type
      IF  lcOldVal = lcNewVal
        *#** Added blank line before subtype header
*        INSERT   BLANK
        IF lnRpSumzBy = 3
          INSERT   BLANK
          REPLACE CGROUP WITH 'H'
          REPLACE CTYPECODE WITH lcNewVal2
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
          INSERT BLANK
        ELSE
          SKIP 1
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
        ENDIF
      ENDIF
    ENDIF

    IF lcOldVal <> lcNewVal
    *********** add to add the subtotal of the main type
      lnRowPos = ASCAN(laRpMaType,lcOldVal)
      INSERT BLANK
      REPLACE CGROUP WITH 'LN,MT'
      INSERT  BLANK
      REPLACE CRowDES WITH PADL('Subtotal of '+ALLTRIM(IIF(lnRowPos = 0,lcOldVal,laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])),lnRpDesWid)
      REPLACE CGROUP WITH 'MT'
      =lfSumMain(lcOldVal)
      IF lcOldVal $ 'QL'
        FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
          IF laRpCol[lnTmpCount,1] = 'O'
            LOOP
          ENDIF
          lcColCount = ALLTRIM(STR(lnTmpCount))
          laLibSum[1,lnTmpCount] = laLibSum[1,lnTmpCount]+NCOL&lcColCount
        ENDFOR
      ENDIF

    **********
      INSERT  BLANK
      IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5]) OR ;
         !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
        REPLACE CGROUP WITH 'MF1'
        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
        ENDIF
        INSERT  BLANK
      ENDIF
      IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6]) OR ;
         !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
        REPLACE CGROUP WITH 'MF2'
        IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
          =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
        ENDIF
        INSERT  BLANK   && For empty line bettween each main type
      ENDIF
      IF lcOldVal = 'Q'
*      INSERT  BLANK
        REPLACE CGROUP WITH 'LN,MF'
        INSERT  BLANK
        REPLACE CRowDES WITH lcRpNetDes
        REPLACE CGROUP WITH 'MF2'
        =lfSumQRow('SETIC')
      DIMENSION laNetInc[1,ALEN(laRpCol,1)]
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        lcColCount = ALLTRIM(STR(lnTmpCount))
        laNetInc[1,lnTmpCount] = NCOL&lcColCount
      ENDFOR
      INSERT  BLANK

      INSERT  BLANK
      lcRowDesc = ''
      lnRowPos = ASCAN(laRpMaType,'Q')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+lcTmpTitl
      ENDIF

      lnRowPos = ASCAN(laRpMaType,'L')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+IIF(EMPTY(lcRowDesc),'',' AND ')+lcTmpTitl
      ELSE
       lcRowDesc = lcTmpTitl
      ENDIF

      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH 'Total '+lcRowDesc,CGROUP WITH 'MF'
      *=lfSumCol('QL',.T.)

      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        lcColCount = ALLTRIM(STR(lnTmpCount))
        IF laRpCol[lnTmpCount,1] $ 'BA'
          *REPLACE  NCOL&lcColCount WITH NCOL&lcColCount+laNetInc[1,lnTmpCount]
          REPLACE  NCOL&lcColCount WITH laLibSum[1,lnTmpCount]+laNetInc[1,lnTmpCount]
        ENDIF
      ENDFOR


      ENDIF

      *** Put the main header top the new main type
      lnRowPos = ASCAN(laRpMaType,lcNewVal)
      INSERT   BLANK
      REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
      REPLACE CTYPECODE WITH lcNewVal+'00'
      REPLACE CGROUP WITH 'H'
      INSERT BLANK

      *** Put subtype header if the summarization is account level
      IF lnRpSumzBy = 3
        *#** Added blank line after main type
        INSERT   BLANK
        REPLACE CGROUP WITH 'H'
        *** Put the Subtype description
        lcRpArrName = 'laRp'+lcNewVal+'Type'
        lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        REPLACE CTYPECODE WITH lcNewVal2
        *#** Added blank line after subtype
        INSERT BLANK
      ELSE
        SKIP 1
        lcRpArrName = 'laRp'+lcNewVal+'Type'
        lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        =lfSumMain(lcNewVal2)
      ENDIF
    ENDIF
    lcOldVal = lcNewVal
    lcOldVal2 = lcNewVal2
  ENDSCAN

  IF lnRpSumzBy = 3
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,ST'
    INSERT  BLANK
    lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
    REPLACE CRowDES WITH PADL('SubTotal of '+ALLTRIM(IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
    REPLACE CGROUP WITH 'STS'
    =lfSumMain(lcOldVal2)
  ENDIF
  lcRpArrName = 'laRp'+lcNewVal+'Type'
  lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
  INSERT  BLANK

  *** Insert the subtype footer 1
  IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5]) OR ;
     !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
    REPLACE CGROUP WITH 'LN,SF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),3])
    REPLACE CGROUP WITH 'SF1'
    IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
      =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5],'|','00,')+'00')
    ENDIF
    INSERT  BLANK
  ENDIF
  *** Insert the subtype footer 2
  IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6]) OR;
     !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
    REPLACE CGROUP WITH 'LN,SF'
    INSERT  BLANK
    lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),4])
    REPLACE CGROUP WITH 'SF2'
    IF lnRowPos>0 AND !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6])
      =lfSumCol(STRTRAN(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),6],'|','00,')+'00')
    ENDIF
    INSERT  BLANK
  ENDIF
  lnRowPos = ASCAN(laRpMaType,lcOldVal)

  REPLACE CGROUP WITH 'LN,MT'
  INSERT  BLANK
  REPLACE CRowDES WITH PADL('Subtotal of '+ALLTRIM(IIF(lnRowPos = 0,lcOldVal,laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])),lnRpDesWid)
  REPLACE CGROUP WITH 'MT'
  =lfSumMain(lcOldVal)
      IF lcOldVal $ 'QL'
        FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
          IF laRpCol[lnTmpCount,1] = 'O'
            LOOP
          ENDIF

          lcColCount = ALLTRIM(STR(lnTmpCount))
          laLibSum[1,lnTmpCount] = laLibSum[1,lnTmpCount]+NCOL&lcColCount
        ENDFOR
      ENDIF

  INSERT  BLANK

  IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5]) OR ;
     !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),3])
    REPLACE CGROUP WITH 'MF1'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),5],.T.)
    ENDIF
    INSERT  BLANK
  ENDIF
  IF !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6]) OR ;
     !EMPTY(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),4])
    REPLACE CGROUP WITH 'MF2'
    IF lnRowPos>0 AND !EMPTY(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6])
      =lfSumCol(larpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),6],.T.)
    ENDIF
  ENDIF
  IF lcOldVal = 'Q'
    INSERT  BLANK
    REPLACE CGROUP WITH 'LN,MF'
    INSERT  BLANK
    REPLACE CRowDES WITH lcRpNetDes
    REPLACE CGROUP WITH 'MF2'
    =lfSumQRow('SETIC')
      DIMENSION laNetInc[1,ALEN(laRpCol,1)]
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        IF laRpCol[lnTmpCount,1] = 'O'
           LOOP
        ENDIF
        lcColCount = ALLTRIM(STR(lnTmpCount))
        laNetInc[1,lnTmpCount] = NCOL&lcColCount
      ENDFOR

      INSERT  BLANK
      lcRowDesc = ''
      lnRowPos = ASCAN(laRpMaType,'Q')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+lcTmpTitl
      ENDIF
      lnRowPos = ASCAN(laRpMaType,'L')
      lcTmpTitl=IIF(lnRowPos = 0,'',ALLTRIM(laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]))
      IF lnRowPos>0
        lcRowDesc = lcRowDesc+IIF(EMPTY(lcRowDesc),'',' AND ')+lcTmpTitl
      ELSE
        lcRowDesc = lcTmpTitl
      ENDIF

      REPLACE CGROUP WITH 'LN,MF'
      INSERT  BLANK
      REPLACE CRowDES WITH 'Total '+lcRowDesc,CGROUP WITH 'MF'
      *=lfSumCol('QL',.T.)
      FOR lnTmpCount = 1 TO ALEN(laRpCol,1)
        lcColCount = ALLTRIM(STR(lnTmpCount))
        IF laRpCol[lnTmpCount,1] $ 'BA'
          *REPLACE  NCOL&lcColCount WITH NCOL&lcColCount+laNetInc[1,lnTmpCount]
          REPLACE  NCOL&lcColCount WITH laLibSum[1,lnTmpCount]+laNetInc[1,lnTmpCount]
        ENDIF
      ENDFOR


  ENDIF

ENDIF

SET RELATION TO


************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/17/2012
*! Purpose   : When fn. for the report
************************************************************
FUNCTION lfRepWhen

lcRpSegPic = loOgScroll.lcRpSegPic
gcDataDir = oAriaApplication.DataDir
lcRpSeg    = loOgScroll.lcRpSeg

*- define some arrays used in the program
*B610116,1 TMI 10/16/2012 [Start] define each array if not defined or empty
*DIMENSION laRpAType[1], laRpLType[1], laRpQType[1], laRpCol[1,24],laRpSegV[1]
=lfReDefArr('laRpAType','1')
=lfReDefArr('laRpLType','1')
=lfReDefArr('laRpQType','1')
=lfReDefArr('laRpCol','1,24')
=lfReDefArr('laRpSegV','1')
*B610116,1 TMI 10/16/2012 [End  ]

*- End of lfRepWhen.

************************************************************
*! Name      : lfReDefArr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/16/2012
*! Purpose   : Re-Define array if not
************************************************************
*B610116,1 TMI 10/16/2012
FUNCTION lfReDefArr
PARAMETERS lcArr,lcDim
IF TYPE('&lcArr')='U' OR EMPTY(&lcArr)
  DIMENSION &lcArr[&lcDim]
ENDIF

*- End of lfReDefArr.

*!********************************************************************
*!
*!              Function:
*!
*!********************************************************************
*
FUNCTION lfTrapEsc
PARAMETERS llOgSwitch,lnOGThrStart
lnOGThrStart = IIF(TYPE('lnOGThrStart')<>'N',0,lnOGThrStart)
IF llOGSwitch
  SET ESCAPE ON
  lcOGEscHnd = ON('ESCAPE')
*  lcOGEscPrm = [DO lfOGEscHnd WITH ] +STR(lnOGThrStart)
  lcOGEscPrm = [DO lfOGEscHnd ]
  ON ESCAPE &lcOGEscPrm
ELSE
  SET ESCAPE OFF
  ON ESCAPE &lcOGEscHnd
ENDIF




*!*********************************************************************************
*!
*!             FUNCTION : lfRpThermo
*!
*!*********************************************************************************
*
FUNCTION lfRpThermo
PARAMETERS lnRpTotal,lnRpCount,lcMessage
PRIVATE lcOldEsc
* N000682 ,1 Thabet Handle globalization issues [Start]
*lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,'Collecting Data')
lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLDATA,oAriaApplication.GetHeaderText("LANG_COLLDATA",AHEADERFILE)))
* N000682 ,1 Thabet Handle globalization issues [END]
&lnRpCount = &lnRpCount + 1
lcOldEsc=SET('ESCAPE')
SET ESCAPE OFF
IF &lnRpCount<=lnRpTotal
  *=gfThermo(lnRpTotal,&lnRpCount,lcMessage,' ')
ENDIF
SET ESCAPE &lcOldEsc


************************************************************
*! Name      : lfUpdWorkFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/14/2012
*! Purpose   : Update the work file before previewing
************************************************************
FUNCTION lfUpdWorkFile
LOCAL lnSlct,lcFldList,lnI,lcI

lcFldList = 'CRowDes,'
FOR lnI = 1 TO lnRpColUsd
  lcI = ALLTRIM(STR(lnI))
  * 09/19/2012 [Start] use COL1,COL2,... instead
  lcFldList = lcFldList + 'COL&lcI.,'
ENDFOR
lcFldList = LEFT(lcFldList,LEN(lcFldList)-1)

lnSlct = SELECT(0)
SELECT (loOgScroll.lcWorkFile)

SCAN
  m.CRowDes = lfRowDes()
  FOR lnI = 1 TO lnRpColUsd
    * 09/19/2012 [Start] use COL1,COL2,... instead
    lcFld = 'm.COL'+ALLTRIM(STR(lnI))
    &lcFld = IIF(cGroup='H','',lfGetVal(lnI))
  ENDFOR
  GATHER MEMVAR FIELDS &lcFldList
ENDSCAN

* removes the nCol1,nCol2
lcWorkFile = loOgScroll.lcWorkFile
ALTER TABLE (loOgScroll.lcWorkFile) DROP COLUMN NCOL1
IF TYPE('&lcWorkFile..NCOL2') <> 'U'
  ALTER TABLE (loOgScroll.lcWorkFile) DROP COLUMN NCOL2
ENDIF

*- update the colum's title in the temp file
LOCATE
FOR lnLine = 1 TO 2
  GO lnLine
  FOR lnI = 1 TO ALEN(laRpCol,1)
    lcI = ALLTRIM(STR(lnI))
    replace COL&lcI WITH laRpCol[lnI,lnLine+1]
  ENDFOR
ENDFOR

SELECT (lnSlct)
*- End of lfUpdWorkFile.


************************************************************
*! Name      : lfOGRefresh
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/20/2012
*! Purpose   : For some reason the tab order in the Option grid was missordered , when the user clicks refresh on the top
*!             of the option grid tool bar it restored ok, I added this function to the when of the first control that shows
*!             up in the Option grid which calls the refresh by clicking on the Refresh button
************************************************************
FUNCTION lfOGRefresh
IF TYPE('loOgScroll.llFirstRun')='U'
  loOgScroll.Addproperty('llFirstRun')
  loogscroll.Parent.ogtoolbar.cntsetting.cmdreset.Click()
ENDIF
*- End of lfOGRefresh.
