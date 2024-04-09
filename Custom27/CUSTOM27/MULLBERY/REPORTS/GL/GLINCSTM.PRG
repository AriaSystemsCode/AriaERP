*:***************************************************************************
*: Program file  : GLINCSTM.PRG
*: Program desc. : CUSTOMIZED INCOME STATMENT REPORTS FOR MULBERRY.
*: Date          : 11/26/2002
*: System        : Aria Advantage Series.
*: Module        : GENERAL LEDGER (GL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : USAMA
*: Tracking Job Number: C102759
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:---------- Summarization tool -------------------------------
*: lfvOkSumz , lfvCanSumz , lfvSumzType , lfwSumzType , lfvSumzBy
*: lfvDumPost
*:---------- Income statment tool -------------------------------
*: lfIncStat , lfColData , lfUpdData , lfInsType  , lfRpName
*: lfDisData , lfDisItem , lfvTypes  , lfvArrange , lfGetNewBar
*: lfvArrOk  , lfModiArr
*:------------  Rows summation --------------------------------
*: lfvSum     , lfMover , lfMovShow , lfvSrc , lfvTrgt , lfvMovm
*: lfvCancMov , lfvOkMov
*:-------------   Columns --------------------------------------------
*: lfColShow  , lfwCol    , lfvCol   , lfvAddCol , lfvRemCol   , lfvOkCol
*: lfvCancCol , lfvIdnCol , lfDisCol , lfBigNo   , lfShowColTp , lfvCol1
*: lfvCol8    , lfvCol9   , lfvCol10 , lfvCol11  , lfwCol12    , lfvCol12
*: lfvCol13   , lfvCol14  , lfvCol15 , lfvCol16  , lfvCol17    , lfvCol18
*: lfvCol20   , lfvCol21  , lfvCol23 , lfSaveArr , lfvCancAll  , lfGetSegDis
*: lfGetVal   , lfGetHead , lfGetPic , lfvOkAll  , lfGetActInf
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO GLINCSTM
*:***************************************************************************
*: Modifications :
*: B606900,1 BWA 01/23/2003 1) Increase the account printing to 4 Chr.[FIX IN FRX]
*: B606900,1 BWA 01/23/2003 2) Make the report work Text and Graphics.
*****************************************************************************

*--BAS
*--N.P Please for any programer get to this program don't delete the string "*--bas"

*--lcrpBudCod   && Column Two Budget Code.
*--lcRpBudYr    && Column Two Budget Year.
*--lcRpBudPr    && Column Two Budget Per.
*--lnRpYOrP     && Variable ["PTD" NO 2 ] OR ["YTD" NO 1].
*--lcRpStPr1    && Column One Period.
*--lcRpStYr1    && Column One Year.

*laRpCol[1,1]  = "A"                             && Point to Actual
*laRpCol[1,2]  = ('PTD ','YTD ')  + lcRpStPr1    && Header + value lcRpStPr1
*laRpCol[1,3]  = " The Year "                    && Hold the year from variable lcRpStYr1.
*laRpCol[1,4]  = "A"
*laRpCol[1,5]  = "A"
*laRpCol[1,6]  = "A"
*laRpCol[1,7]  = "A"
*laRpCol[1,8]  = "A"
*laRpCol[1,9]  = "A"
*laRpCol[1,10] = 1
*laRpCol[1,11] = 1
*laRpCol[1,12] = 1 or 2                          && value of lnRpYOrP
*laRpCol[1,13] = The period                      && value of lcRpStPr1
*laRpCol[1,14] = The Year                        && value of lcRpStYr1
*laRpCol[1,15] = The period                      && value of lcRpStPr1
*laRpCol[1,16] = The Year                        && value of lcRpStYr1
*laRpCol[1,17] = The period                      && value of lcRpStPr1
*laRpCol[1,18] = The Year                        && value of lcRpStYr1
*laRpCol[1,19] = "A"
*laRpCol[1,20] = "A"
*laRpCol[1,21] = "A"
*laRpCol[1,22] = "A"
*laRpCol[1,23] = "A"
*laRpCol[1,24] = Change # in segmant to *       && STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*')

*laRpCol[2,1]  = "B"                          && Point to Budget
*laRpCol[2,2]  = 'Budget ' + lcrpBudCod       && Header + Budget code
*laRpCol[2,3]  = " The year and Period "      && Hold the year and period from variables [ lcRpBudYr+'/'+PADL(lcRpBudPr,2,'0')) ]
*laRpCol[2,4]  = "A"
*laRpCol[2,5]  = "A"
*laRpCol[2,6]  = "A"
*laRpCol[2,7]  = "A"
*laRpCol[2,8]  = The budget code              && From variable lcRpBudCod.
*laRpCol[2,9]  = The budget Year              && From variable lcRpBudYr.
*laRpCol[2,10] = The budget period            && From variable lcRpBudPr.
*laRpCol[2,11] = The budget period            && From variable lcRpBudPr.
*laRpCol[2,12] = "A"
*laRpCol[2,13] = "A"
*laRpCol[2,14] = "A"
*laRpCol[2,15] = "A"
*laRpCol[2,16] = "A"
*laRpCol[2,17] = "A"
*laRpCol[2,18] = "A"
*laRpCol[2,19] = "A"
*laRpCol[2,20] = "A"
*laRpCol[2,21] = "A"
*laRpCol[2,22] = "A"
*laRpCol[2,23] = "A"
*laRpCol[2,24] = Change # in segmant to *       && STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*')

*--To avoid delete the records with the value in case the YTD and Zero value in PTD.

&& llOgGrdVal ... Variable hold the value of the filter in the option grid.
*llOgGrdVal = llRpExcZe
*llRpExcZe  = .F.          && To collect all the data and in case the delete function check the data.

*--Run the program.
FOR lnLop = 1 To 2
  IF lnLop = 1
    lnRpYOrP = 2            &&'PTD'
    lcOrgnlExp = lcRpExp
    DIMENSION laStorage[1,1]
    STORE "" TO laStorage
  ELSE
    lnRpYOrP = 1            &&'YTD'
    laRpMaType = ""
    lcRpExp    = lcOrgnlExp
    DIMENSION laTypNum[1,3]     && Array hold all the Typecodes to check the 
                                && typecode that has 1 account to delete the subtotal of it.
  ENDIF
*--bas

lnBegTime = 0
lnEndTime = 0
lnTotTime = 0
lcRpRelVar = ''    && Hold the relation condition

*--Create array hold the structuer of temp file
DIMENSION laDbfName[3,4]

*--Create array that hold the summation of main type 
*--that used in the percentage of option
DIMENSION laRpMainTo(ALEN(laRpCol,1))

lcPRExp = ''
lnRpIdnNo = 0     && Hold the identation

*--Hold the needed segment information
lcRpTypeDes = '~sales~Cost of goods sold~Expenses~Other income~Taxes~'
lcRpTotDes  = '~ ~Gross margin~Net income from operation~Net income before taxes~ ~'
lcRpTotExp  = '~ ~S|C~S|C|E~S|C|E|I~ ~'

DIMENSION laSegInf(1,2)  
=lfGetActInf()

*--If the user change any thing in the opion grid or 
*--in the tool (Rows & Columns ) rebuild the data

IF lcOGManRep <> 'GLINCSTA'   
  IF !lfInitCol()
    RETURN
  ENDIF
  lcOldAlias = SELECT()
  llUsed = .F.
  IF !USED('GLTYPES')
    SELECT 0
    USE (gcDataDir+'GLTYPES') ORDER TAG TypeCode
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

*--Check if there is informations in rows or columns.
IF EMPTY(laRpMaType) OR EMPTY(laRpCol)  
  WAIT "No columns has been defined " WINDOW NOWAIT
 RETURN
ENDIF

  *--Initialize the array by 0
  FOR lnCount = 1 TO ALEN(laRpMainTo,1)
    laRpMainTo[lnCount] = 0
  ENDFOR

  *--Create the dbf file structuer
  WAIT "Creating temporary files " WINDOW NOWAIT
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

  CREAT DBF (gcWorkDir+lcTempFile) FROM ARRAY laDbfName
  INDEX ON CacctCode TAG CacctCode

  llRpToolCh = .F.
  lcRpFiles  = "GLACCHAR" + IIF(lcRpExp = '.T.' , '' , ",GLGRPDT")
  lnCount    = 0
  lnTotal    = RECCOUNT('GLACCHAR')   
  lcRpExp    =  IIF(lcRpExp='.T.','',;
               'GLACCHAR.CACCTCODE = GLGRPDT.CACCTCODE .AND. &lcRpExp .AND.')+;
               'LEFT(CTypeCode,1) IN ("S","C","E","I","T")'
  lcRpExp    =  lcRpExp+IIF(EMPTY(lcPRExp),'',' .AND. '+lcPRExp)

  *--Start Calculate the time.
  lnBegTime = SECONDS()    

  *--Collect the main and sub account types orderd by the arrangement
  *--provided by the user throguh the tool program linked to this report
  SELECT  &lcRpFields              ;
    FROM  &lcRpFiles               ;
    WHERE &lcRpExp                 ;
    ORDER BY 1                     ;
    INTO DBF &gcWorkDir.&lcRpTargt

  lnEndTime = SECONDS()
  lnTotTime = lnTotTime +(lnEndTime-lnBegTime)

  *** If the user did not ontrrupt the selection of the account type
  IF !llOGEscPrs

    *--If no recordes collected inform the user and terminat.
    IF _TALLY = 0
      *--NO recoeds hove been collected
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE
      *--Esatablish the relations between the main temp file and
      *--the temp files represinting the each column
      =lfSetRela()

      *--Collect the report data from actual or budget files
      WAIT 'Collecting data from files' WINDOW NOWAIT
      
      =lfSelData()
      IF _TALLY > 0
        *******************************
        *--Determine the columns type if it is result or indentation.
        *--Know the type of each column in the report
        DIMENSION laColInf[1,lnRpColUsd]
        =lfGetColInf()
        *************************************

*--bas
        SELECT (lcWorkFile)
        IF lnRpYOrP = 2                &&'PTD'
          =lfCopyOne()  
        ELSE
          =lfCopyTwo()                 &&'YTD'
          =lfDelet()
          SELECT (lcWorkFile)
          DO gfDispRe WITH EVAL('lcRpForm')
        ENDIF
*--BAS
      ELSE
        *--NO recoeds hove been collected.
        =gfModalGen("INM00052B00000","DIALOG")
*--BAS
        EXIT
*--BAS
      ENDIF
    ENDIF
  ELSE
    IF lnCount < lnTotal
      lnCount = lnTotal-1
      =lfClosThrm(lnTotal,lnCount)
    ENDIF
  ENDIF

*--BAS
ENDFOR
*--bas
*!*************************************************************
*! Name      : lfDelet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Delete the extra line as specs.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDelet

PRIVATE lcTypCode , lcHdrText
STORE SPACE(0) TO lcTypCode , lcHdrText
FOR lnLop = 1 TO ALEN(laTypNum,1)
  IF laTypNum[lnLop,2] = 1
    lcTypCode = lcTypCode + laTypNum[lnLop,3]
  ENDIF
ENDFOR

SELECT (lcWorkFile)
SCAN

  *--Section delete the subtotal.
  IF LEFT(cTypecode,1) $ lcTypCode AND ALLTRIM(cGroup) = "H" AND SUBSTR(cTypecode,2,2) = "00"
    lcHdrText = ALLTRIM(CRowDES)
  ENDIF

  *--Section delete the header.
  IF ALLTRIM(cGroup) = "H" AND SUBSTR(cTypecode,2,2) = "00"
    DELETE
    SKIP 1
    DELETE
  ENDIF

  *--Section complete deleting the subtotal.
  IF ALLTRIM(cGroup) = "MT" AND UPPER(lchdrtext) $ UPPER(ALLTRIM(CROWDES)) AND !EMPTY(lcHdrText)
    DELETE
    SKIP - 1
    DELETE
    SKIP 1
    DELETE
  ENDIF
ENDSCAN

SELECT (lcWorkFile)
LOCATE
*--End of lfDelet.
*!*************************************************************
*! Name      : lfCopyTwo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Get the data of the YTD.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfCopyTwo

SELECT (lcWorkFile)
=AFIELDS(laTmpStru2)
CREATE TABLE (gcWorkDir + lcYtdFile) FROM ARRAY laTmpStru2
APPEND FROM (gcWorkDir + lcWorkFile)

IF USED(lcWorkFile)
  USE IN ALIAS(lcWorkFile)
  ERASE (gcWorkDir + lcWorkFile + '.BDF')
  ERASE (gcWorkDir + lcWorkFile + '.FPT')
ENDIF

lnTmpStru2 = ALEN(laTmpStru2,1)
DIMENSION laTmpStru2[lnTmpStru2 + 6 ,4]

*-- Two Fields to add the YTD data.
laTmpStru2[lnTmpStru2 + 1 ,1] = 'Ncol3'
laTmpStru2[lnTmpStru2 + 1 ,2] = 'N'
laTmpStru2[lnTmpStru2 + 1 ,3] = 16
laTmpStru2[lnTmpStru2 + 1 ,4] = 2

laTmpStru2[lnTmpStru2 + 2 ,1] = 'Ncol4'
laTmpStru2[lnTmpStru2 + 2 ,2] = 'N'
laTmpStru2[lnTmpStru2 + 2 ,3] = 16
laTmpStru2[lnTmpStru2 + 2 ,4] = 2

*-- The Variance field for PTD.
laTmpStru2[lnTmpStru2 + 3 ,1] = 'VariancP'
laTmpStru2[lnTmpStru2 + 3 ,2] = 'N'
laTmpStru2[lnTmpStru2 + 3 ,3] = 11
laTmpStru2[lnTmpStru2 + 3 ,4] = 2

*-- The Variance field for YTD.
laTmpStru2[lnTmpStru2 + 4 ,1] = 'VariancY'
laTmpStru2[lnTmpStru2 + 4 ,2] = 'N'
laTmpStru2[lnTmpStru2 + 4 ,3] = 11
laTmpStru2[lnTmpStru2 + 4 ,4] = 2

*-- Logic field to print the Account Number.
laTmpStru2[lnTmpStru2 + 5 ,1] = 'llPrnAcct'
laTmpStru2[lnTmpStru2 + 5 ,2] = 'L'
laTmpStru2[lnTmpStru2 + 5 ,3] = 1
laTmpStru2[lnTmpStru2 + 5 ,4] = 0

*-- Logic field to not recalculate in case exclude zero = 'Yes'.
laTmpStru2[lnTmpStru2 + 6 ,1] = 'llNoCal'
laTmpStru2[lnTmpStru2 + 6 ,2] = 'L'
laTmpStru2[lnTmpStru2 + 6 ,3] = 1
laTmpStru2[lnTmpStru2 + 6 ,4] = 0

CREATE TABLE (gcWorkDir + lcWorkFile) FROM ARRAY laTmpStru2
SELECT (lcWorkFile)
*--1ST file...
APPEND FROM (gcWorkDir + lcPTDFile)

*--2nd file...
*--In the same time calculate the Variance fields.

*--BAS
GOTO TOP IN (lcYtdFile)
lnRecCont = RECCOUNT(lcYtdFile)
*--BAS

IF llRpExcZe
  SCAN
    lnGoToRec = RECNO()
    SELECT (lcYtdFile)
    GOTO lnGoToRec IN (lcYtdFile)

    IF ALLTRIM(EVAL(lcWorkFile+'.CGROUP')) + ALLTRIM(EVAL(lcWorkFile+'.CRowDES')) == ;
       ALLTRIM(EVAL(lcYtdFile+'.CGROUP')) + ALLTRIM(EVAL(lcYtdFile+'.CRowDES'))
      SELECT (lcWorkFile)
    ELSE

      lcValue = ALLTRIM(EVAL(lcYtdFile+'.CGROUP')) + ALLTRIM(EVAL(lcYtdFile+'.CRowDES'))
      SELECT (lcYtdFile)
      SCATTER MEMVAR MEMO
      llFondRc = .T.
      SELECT (lcWorkFile)
      SET FILTER TO SUBSTR(CTYPECODE,2,2) # "00" AND !EMPTY(CTYPECODE)
      SCAN
        IF ALLTRIM(EVAL(lcWorkFile+'.CGROUP')) + ALLTRIM(EVAL(lcWorkFile+'.CRowDES')) == lcValue
          EXIT
          llFondRc = .F.
        ENDIF
      ENDSCAN

      SET FILTER TO
      GOTO lnGoToRec IN (lcWorkFile)
      IF llFondRc
        INSERT BEFORE BLANK
        M.NCOL3 = M.NCOL1
        M.NCOL4 = M.NCOL2
        M.VariancY = M.Ncol3 - M.Ncol4
        STORE 0 TO M.NCOL1 , M.NCOL2

        GATHER MEMVAR MEMO
        REPLACE llNoCal WITH .T.
      ENDIF
    ENDIF

    SELECT (lcWorkFile)
    IF !EMPTY(&lcWorkFile..cGroup) .AND. ALLTRIM(&lcWorkFile..cGroup) # "H" ;
       .AND. !("LN" $ ALLTRIM(&lcWorkFile..cGroup)) AND !llNoCal

      REPLACE Ncol3 WITH EVAL(lcYtdFile+'.Ncol1') ,;
              Ncol4 WITH EVAL(lcYtdFile+'.Ncol2')
      REPLACE VariancP WITH Ncol1 - Ncol2 ,;
              VariancY WITH Ncol3 - Ncol4
    ENDIF

    IF !EMPTY(cGroup) .AND. ALLTRIM(cGroup) # "H"                       ;
       .AND. !("LN" $ ALLTRIM(cGroup)) .AND. !("STS" $ ALLTRIM(cGroup)) ;
       .AND. !("MF" $ ALLTRIM(cGroup)) .AND. !("SF" $ ALLTRIM(cGroup))  ;
       .AND. !("MT" $ ALLTRIM(cGroup))  && .AND. !llNoCal
      REPLACE llPrnAcct WITH .T.
    ENDIF

  ENDSCAN
ELSE

  SCAN
    lnGoToRec = RECNO()
    GOTO lnGoToRec IN (lcYtdFile)

    IF !EMPTY(&lcWorkFile..cGroup) .AND. ALLTRIM(&lcWorkFile..cGroup) # "H" ;
       .AND. !("LN" $ ALLTRIM(&lcWorkFile..cGroup))

      REPLACE Ncol3 WITH EVAL(lcYtdFile+'.Ncol1') ,;
              Ncol4 WITH EVAL(lcYtdFile+'.Ncol2')
      REPLACE VariancP WITH Ncol1 - Ncol2 ,;
              VariancY WITH Ncol3 - Ncol4
    ENDIF

    IF !EMPTY(cGroup) .AND. ALLTRIM(cGroup) # "H"                       ;
       .AND. !("LN" $ ALLTRIM(cGroup)) .AND. !("STS" $ ALLTRIM(cGroup)) ;
       .AND. !("MF" $ ALLTRIM(cGroup)) .AND. !("SF" $ ALLTRIM(cGroup))  ;
       .AND. !("MT" $ ALLTRIM(cGroup))
      REPLACE llPrnAcct WITH .T.
    ENDIF
  ENDSCAN
ENDIF

*--End of lfCopyTwo
*!*************************************************************
*! Name      : lfCopyOne
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Get the data of the PTD.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfCopyOne

SELECT (lcWorkFile)
=AFIELDS(laTempStru)
CREATE TABLE (gcWorkDir + lcPTDFile) FROM ARRAY laTempStru
APPEND FROM (gcWorkDir + lcWorkFile)

IF USED(lcWorkFile)
  USE IN ALIAS(lcWorkFile)
  ERASE (gcWorkDir + lcWorkFile + '.BDF')
  ERASE (gcWorkDir + lcWorkFile + '.FPT')
ENDIF

*--End of lfCopyOne.
*!*************************************************************
*! Name      : lfvDumPost
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvDumPost

IF USED(lcTempPost)
  SELECT(lcTempPost)
  ZAP
ENDIF

PRIVATE lcProgDir, lcSaveAppl , laCpyPrgGl
IF !EMPTY(laprgtemps)
  DIMENSION laCpyPrgGl[ALEN(laprgtemps,1),ALEN(laprgtemps,2)]
  =ACOPY(laPrgTemps,laCpyPrgGl)
ENDIF

lcProgDir = gcAppHome+gcAct_Appl+'\'

lcSaveAppl = gcWinAppl 
gcWinAppl  = 'GL'

DO CASE

  *--Case Journal batches
  CASE lnDumPost = '1'
    glFirsTime = .T.
    gcBaseWind = 'AWDGLBPOST'
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLBPOST WITH lcTempPost"

  *--Case Single transaction
  CASE lnDumPost = '2'
    glFirsTime = .T.
    gcBaseWind = 'AWDGLTPOST'
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLTPOST WITH lcTempPost"
        
  *--Case Begining balance bacthes.
  CASE lnDumPost = '3'
    glFirsTime = .T.
    gcBaseWind = 'AWDGLBGPST'
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLBGPST WITH lcTempPost"
ENDCASE

IF !EMPTY(laCpyPrgGl)
  DIMENSION laprgtemps[ALEN(laCpyPrgGl,1),ALEN(laCpyPrgGl,2)]
  =ACOPY(laCpyPrgGl,laPrgTemps)
ENDIF

gcWinAppl = lcSaveAppl

*--End of lfvDumPost.
*!*************************************************************
*! Name      : lfvGrpCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvGrpCode

DECLARE laRpRetFld(1)

lcOldBrFld    = lcBrFields
lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()
lcOldAlias    = SELECT()
llUsedBef     = .T.

IF !USED('GLGRPHD')
  SELECT 0
  USE (gcDataDir+'GLGRPHD')
  llUsedBef = .F.
ENDIF

SELECT GLGRPHD
SET ORDER TO TAG grpcode

IF !EMPTY(&lcRpCurFld)
  IF ('?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld))
    =gfBrows('','CGrpCode',"laRpRetFld",'Codes File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
ENDIF
lcBrFields = lcOldBrFld

IF !llUsedBef AND (ASCAN(laSelFile,'GLGRPHD') = 0)
  USE IN GLGRPHD
ENDIF

SET ORDER TO
SELECT(lcOldAlias)

*--End of lfvGrpCode.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfClearRep

IF (ASCAN(laSelFile,'ACCOD') = 0) AND  USED('ACCOD')
  USE IN ACCOD
ENDIF

IF (ASCAN(laSelFile,'GLSEGVAL') = 0) AND  USED('GLSEGVAL')
  USE IN GLSEGVAL
ENDIF

IF (ASCAN(laSelFile,'GLACBALS') = 0) AND  USED('GLACBALS')
  USE IN GLACBALS
ENDIF

IF (ASCAN(laSelFile,'GLBUDDT') = 0) AND  USED('GLBUDDT')
  USE IN GLBUDDT
ENDIF

IF (ASCAN(laSelFile,'GLGRPHD') = 0) AND  USED('GLGRPHD')
  USE IN GLGRPHD
ENDIF

IF (ASCAN(laSelFile,'FSPRD') = 0) AND  USED('FSPRD')
  USE IN FSPRD
ENDIF

IF !EMPTY(ALIAS())
  SET RELATION TO
ENDIF

*--Close and remove all tempry files.
IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  ERASE (gcWorkDir + lcRpTargt + '.BDF')
  ERASE (gcWorkDir + lcRpTargt + '.FPT')
ENDIF

IF USED(lcTempFile)
  USE IN ALIAS(lcTempFile)
  ERASE (gcWorkDir + lcTempFile + '.BDF')
  ERASE (gcWorkDir + lcTempFile + '.CDX')
  ERASE (gcWorkDir + lcTempFile + '.FPT')
ENDIF

IF USED(lcTempPost)
  USE IN ALIAS(lcTempPost)
  ERASE (gcWorkDir + lcTempPost + '.BDF')
  ERASE (gcWorkDir + lcTempPost + '.CDX')
  ERASE (gcWorkDir + lcTempPost + '.FPT')
ENDIF

IF USED(lcWorkFile)
  USE IN ALIAS(lcWorkFile)
  ERASE (gcWorkDir + lcWorkFile + '.BDF')
  ERASE (gcWorkDir + lcWorkFile + '.FPT')
ENDIF

DIMENSION laRpMaType(1,6) , laRpSType(1,6) , laRpCType(1,6) , laRpEType(1,6),;
          laRpIType(1,6)  , laRpTType(1,6)
laRpMaType[1,1] = ''

DIMENSION laRpCol(1,24)     && Column information
DIMENSION laTempCol(1,23)   && Temp Column

DIMENSION laDbfName(1),laRpMainTo[1,1],laRpSeg(1,2)
DIMENSION laRpColInf(47,2)

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfwOldVal

lcRpOld = EVAL(VARREAD())

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfvApprove
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvApprove
PARAMETER lnRpVarType

lcObjName = VARREAD()
lnNoCol   = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
IF lnRpNoCol + lnRpIdnCol > lnNoCol
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
  &lcObjName = lcRpOld            && Assign the old value
  _CUROBJ    = _CUROBJ
  SHOW GET (lcObjName)
ELSE   
  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
ENDIF

*--End of lfvApprove.
*!*************************************************************
*! Name      : lfvExcZe
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvExcZe

llRpToolCh = .T.

*--End of lfvExcZe.

              *--This part control the summarization screen--*
*!*************************************************************
*! Name      : lfvSumzBy
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvSumzBy

lcOldAlias = SELECT()
IF !USED('ACCOD')
  USE &gcDataDir.ACCOD
ENDIF
SELECT ACCOD
LOCATE
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
lnTemSumzBy = lnRpSumzBy      
lnSeg1 = lnRpSeg1
lnSeg2 = lnRpSeg2
lnSeg3 = lnRpSeg3
lnSeg4 = lnRpSeg4
lnSeg5 = lnRpSeg5
lnSeg6 = lnRpSeg6

DO (gcRepHome + gcAct_Appl + '\GLSUMZBY.SPR')   
SELECT (lcOldAlias)

*--End of lfvSumzBy.
*!*************************************************************
*! Name      : lfwSumzType
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfwSumzType.
*!*************************************************************
*! Name      : lfvSumzType
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfvSumzType.
*!*************************************************************
*! Name      : lfvCanSumz
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCanSumz

lnRpSumzBy = lnTemSumzBy

lnRpSeg1 = lnSeg1
lnRpSeg2 = lnSeg2
lnRpSeg3 = lnSeg3
lnRpSeg4 = lnSeg4
lnRpSeg5 = lnSeg5
lnRpSeg6 = lnSeg6

*--End of lfvCanSumz.
*!*************************************************************
*! Name      : lfvCanSumz
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOkSumz

llRpToolCh = .T.

*--End of lfvOkSumz.

                 *--The end of summarization  screen--*
*!*************************************************************
*! Name      : lfMaSuType
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfMaSuType
PARAMETER lcFieldVal,lnArrType

IF LEFT(lcFieldVal,1) $ 'ESCIT'
  
  IF lnArrType = 1
    lcFieldVal = LEFT(lcFieldVal,1)
    RETURN (ALLTRIM(STR(ASUBSCRIPT(laRpMaType,ASCAN(laRpMaType,lcFieldVal),1))))
  ELSE
    lcRpArrName = 'laRp'+LEFT(lcFieldVal,1)+'Type'
    RETURN PADL((ALLTRIM(STR(ASUBSCRIPT(&lcRpArrName ,;
             ASCAN(&lcRpArrName , lcFieldVal) , 1)))) , 2 , '0')
  ENDIF
ELSE  
  RETURN '0'
ENDIF

*--End of lfMaSuType.

           *-- This part select the columns information from 
           *-- files . Only the budget and actual columns . 
*!*************************************************************
*! Name      : lfSelData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
* This fnction will build the data for each column in the report comming 
* from eather the balance or the budget files
FUNCTION lfSelData

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
  lnColField = 0
  FOR lnCount = 1 TO ALEN(laRpCol,1)
    lnOpResult = 0
    DO CASE
      CASE laRpCol[lnCount,1] = 'B'
        lnColField  = lnColField + 1
        lcFieldName = 'Ncol'+ALLTRIM(STR(lnCount))
        lcRpRelVar  = laRpCol[lnCount,8] + laRpCol[lnCount,9] +;
                      CacctCode + PADL(laRpCol[lnCount,10],2,'0')

        GO RECNO()
        GO TOP IN ACCOD
        lnNoSegts = ACCOD.Nacsnoseg
        lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                           ACCOD.NACSSIZE , 'ACCSEGNO')
        IF LIKE(REPL('?',lnSegLen ) + IIF(lnNoSegts=1,'','-') + IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))
          SELECT GLBUDDT
          IF lnRpYOrP = 1                 &&In case the Year to date only.
            =SEEK(laRpCol[lnCount,8]+laRpCol[lnCount,9]+EVAL(lcRpTargt+'.cacctcode'))
          ENDIF

          SUM REST Namount WHILE ;
           GLBUDDT.CbudCode + GLBUDDT.CbudYear + GLBUDDT.cacctcode + GLBUDDT.CBUDPRD <= ;
           laRpCol[lnCount,8] + laRpCol[lnCount,9] + &lcRpTargt..cacctcode + PADL(laRpCol[lnCount,11],2,'0'); 
           TO lnOpResult

           lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI' , -lnOpResult , lnOpResult)
        ELSE
          lnOpResult = 0
        ENDIF
        SELECT (lcTempFile)
        REPLACE (lcFieldName) WITH lnOpResult
        
      *** If the data colleged form balance file  
      CASE laRpCol[lnCount,1] = 'A'
        lnColField  = lnColField + 1
        lcFieldName = 'Ncol' + ALLTRIM(STR(lnCount))      
        *** If only one record colleged
        IF laRpCol[lnCount,12] = 1
          lcRpRelVar = CacctCode + laRpCol[lnCount,14] + laRpCol[lnCount,13]
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
          IF LIKE(REPL('?',lnSegLen ) + IIF(lnNoSegts=1,'','-') + IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))
            lnOpResult = NACBOPBAL + NACBPTDDR - NACBPTDCR
            lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI',-lnOpResult,lnOpResult)
          ELSE
            lnOpResult = 0
          ENDIF
        ELSE
          lcRpRelVar = CacctCode + laRpCol[lnCount,16] + laRpCol[lnCount,15]
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
            SUM REST (NACBPTDDR - NACBPTDCR) WHILE ;
              CacctCode + CFISFYEAR + CFSPPRDID <= ;                       
              &lcRpTargt..cAcctCode + laRpCol[lnCount,18] + laRpCol[lnCount,17] TO lnOpResult
              lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI',-lnOpResult,lnOpResult)
          ELSE
            lnOpResult = 0
          ENDIF
        ENDIF
        SELECT (lcTempFile)
        REPLACE (lcFieldName) WITH lnOpResult
    ENDCASE

    SELECT (lcRpTargt)
    *--Sum The Main type to used in the percentage of  
    IF laRpCol[lnCount,22] = AT(LEFT(CTypeCode,1),'SCEIT')
      laRpMainTo[lnCount] = laRpMainTo[lnCount] + lnOpResult
    ENDIF
  ENDFOR
  SELECT (lcRpTargt)
ENDSCAN
SELECT (lcTempFile)
lnNoCol = FCOUNT() - 3
lcFieldsUsed = ''

FOR lnColCount  = 4 TO FCOUNT()
   lcFieldName  = FIELD(lnColCount)
   lcFieldsUsed = lcFieldsUsed + IIF(lnColCount > 4 ,',','') + 'SUM('+lcFieldName+') ' +;
                 'AS ' + lcFieldName
ENDFOR

lcRpSumzBy   = lfGetSumzBy()
lcFieldsUsed = lcRpSumzBy+',CRowDes,CACCTCODE,'+IIF(lnRpSumzBy=1,;
               [LEFT(CTYPECODE,1)+'00' AS CTYPECODE,],'CTYPECODE,');
              +lcFieldsUsed

SET RELATION TO
_TALLY = 0

SELECT &lcFieldsUsed                        ,;
       lfMaSuType(CTYPECODE,1) AS 'NMainTy' ,;
       lfMaSuType(CTYPECODE,2) AS 'NSubTy'   ;
FROM &lcTempFile                             ;
WHERE IIF(llRpExcZe , lfExcZe(), .T.)        ;
GROUP BY 1                                   ;
ORDER BY NMAINTY,NSUBTY                      ;
INTO DBF &gcWorkDir.&lcWorkFile.

IF _TALLY > 0
  =lfModiFile()
ENDIF

*--End of lfSelData.
*!*************************************************************
*! Name      : lfExcZe
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfExcZe

llTotalAm = .F.
FOR lnColCount= 4 TO FCOUNT()
  IF EVAL(FIELD(lnColCount)) <> 0
    llTotalAm = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llTotalAm

*--End of lfExcZe.
*!*************************************************************
*! Name      : lfModiFile
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfModiFile
PRIVATE lcOldVal,lcNewVal

*--BAS
IF lnRpYOrP = 1            &&'YTD' in the 2nd time only to check the unique account.
  SELECT (lcWorkFile)
  GOTO BOTTOM
  DIMENSION laTypNum[ROUND(VAL(NMAINTY),0),3]
  STORE SPACE(0) TO laTypNum
  STORE 0 TO lnTypNum
  SCAN
    lnPos = ASCAN(laTypNum , ALLTRIM(Nmainty))
    IF lnPos > 0
      lnPos = ASUBSCRIPT(laTypNum,ASCAN(laTypNum , ALLTRIM(Nmainty)),1)
    ENDIF
    IF lnPos = 0
      lnTypNum = lnTypNum + 1
      laTypNum[lnTypNum,1] = ALLTRIM(Nmainty)
      laTypNum[lnTypNum,2] = ROUND(VAL(NSUBTY),0)
      laTypNum[lnTypNum,3] = LEFT(cTypecode,1)
    ELSE
      laTypNum[lnPos,2] = ROUND(VAL(NSUBTY),0)
      laTypNum[lnPos,3] = LEFT(cTypecode,1)
    ENDIF
  ENDSCAN
  SELECT (lcWorkFile)
  LOCATE
ENDIF
*--BAS

SELECT GLACCHAR
SET ORDER TO ACCTCODE

SELECT (lcWorkFile)
SET RELATION TO CACCTCODE  INTO GLACCHAR ADDITIVE
LOCATE
IF lnRpSumzBy = 1
  lcOldVal = LEFT(CTYPECODE,1)
  SCAN
    lcNewVal = LEFT(CTYPECODE,1)    
    lnRowPos = ASCAN(laRpMaType,lcOldVal) 
    *** Put the main description into file
    REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
    IF lcOldVal <> LEFT(CTYPECODE,1) 
      lnRowPos = ASCAN(laRpMaType,lcOldVal) 
      ** ADD BY HESHAM
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

ELSE
  *-- For the subtype or account summarization level
  lcOldVal  = LEFT(CTYPECODE,1)
  lcOldVal2 = CTYPECODE
  lcNewVal  = lcOldVal
  lcNewVal2 = lcOldVal2
  lnRowPos  = ASCAN(laRpMaType,lcOldVal) 

  *** Put the main description into file
  INSERT  BEFORE BLANK
  REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2]),;
          CTYPECODE WITH lcOldVal+'00',;
          CGROUP WITH 'H'

  *--ADDED blank line after main type
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
    *-- ADDED blank line after subtype
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
      ***** ADDED BY HESHAM
      INSERT BEFORE BLANK      

      IF lnRpSumzBy = 3
        REPLACE CGROUP WITH 'LN,ST'      
        INSERT  BLANK
        REPLACE CRowDES WITH PADL('SubTotal of '+IIF(lnRowPos = 0,'',ALLTRIM(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
        REPLACE CGROUP WITH 'STS'
        =lfSumMain(lcOldVal2)
        INSERT BLANK  
        IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
          REPLACE CGROUP WITH 'LN,SF'      
          INSERT BLANK
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
        IF lnRpSumzBy = 3
          INSERT   BLANK
          REPLACE CGROUP WITH 'H'
          REPLACE CTYPECODE WITH lcNewVal2
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          lnRowPos = ASCAN(&lcRpArrName , lcNewVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
          INSERT BLANK
        ELSE
          SKIP 1
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        ENDIF
      ENDIF
    ENDIF

    IF lcOldVal <> lcNewVal
    *********** add by Hesham to add the subtotal of the main type
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

      *** Put the main header top the new main type
      lnRowPos = ASCAN(laRpMaType,lcNewVal) 
      INSERT   BLANK
      REPLACE CRowDES WITH IIF(lnRowPos = 0,'',laRpMaType[ASUBSCRIPT(laRpMaType,lnRowPos,1),2])
      REPLACE CTYPECODE WITH lcNewVal+'00'
      REPLACE CGROUP WITH 'H'
      INSERT BLANK

      *** Put subtype header if the summarization is account level
      IF lnRpSumzBy = 3
        *--Added blank line after main type
        INSERT   BLANK
        REPLACE CGROUP WITH 'H'
        *** Put the Subtype description
        lcRpArrName = 'laRp'+lcNewVal+'Type'
        lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
        REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
        REPLACE CTYPECODE WITH lcNewVal2
        *--Added blank line after subtype
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
ENDIF

INSERT  BLANK
REPLACE CGROUP WITH 'LN,MF'      
INSERT  BLANK
REPLACE CRowDES WITH lcRpNetDes,CGROUP WITH 'MF'
=lfSumCol('SITEC',.T.)
SET RELATION TO

*--End of lfModiFile.
*!*************************************************************
*! Name      : lfGetAcDes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetAcDes
PARAMETERS lcAcctCode

PRIVATE lcRetVal
lnOldRecNo = RECNO()
lcOldAlias = SELECT()
IF (lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 AND lnRpSeg4=1 AND lnRpSeg5=1 AND lnRpSeg6=1)
  lcRetVal = GLACCHAR.CACCNLDES
ELSE
  llUsedBr   = .F.
  IF !USED('GLSEGVAL')
    = gfSysOpen(gcDataDir+'GLSEGVAL')
    llUsedBr = .T.
  ENDIF
  SELECT GLSEGVAL
  SET ORDER TO TAG ACSSEGVAL
  
  =SEEK ('1'+SUBSTR(lcAcctCode,1,laSegInf[1,1]))
  lcRetVal = ALLTRIM(CSEGSHDES)
 
  FOR lnSegCount = 2 To lnRpTotSeg
    lcSegNo = 'lnRpSeg'+ALLTRIM(STR(lnSegCount))
    IF &lcSegNo = 1
      =SEEK (ALLTRIM(STR(lnSegCount))+SUBSTR(lcAcctCode,AT('-',lcAcctCode,lnSegCount-1)+1,laSegInf[lnSegCount,1]))
      lcRetVal = lcRetVal + '-'+ ALLTRIM(CSEGSHDES)
    ENDIF
  ENDFOR
  *** Restore and close the files
  IF llUsedBr
    =gfSysClose('GLSEGVAL')
  ENDIF
ENDIF
 
SELECT (lcOldAlias)
IF BETWEEN(lnOldRecNo,1,RECCOUNT())
  GO lnOldRecNo
ENDIF

RETURN lcRetVal

*--End of lfGetAcDes.
*!*************************************************************
*! Name      : lfSumCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Sum the column for the footer summation.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSumCol
PARAMETERS lcChekVal,lnTypeOfSum

PRIVATE lnRecNo
lnRecNo = RECNO()

IF lnTypeOfSum         && Sum the main type
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
      SUM IIF(LEFT(CTYPECODE,1) $ 'SI',NCOL&lcColCount,-NCOL&lcColCount) FOR LEFT(ctypecode,1) $ lcChekVal;
      TO lnSumm       
      GO lnRecNO
      REPLACE NCOL&lcColCount WITH lnSumm
    ENDIF
  ENDFOR
ELSE
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
      SUM NCOL&lcColCount FOR ctypecode $ lcChekVal TO lnSumm      
      GO lnRecNO
      REPLACE NCOL&lcColCount WITH lnSumm
    ENDIF
  ENDFOR
ENDIF
GO lnRecNO

*--End of lfSumCol.
*!*************************************************************
*! Name      : lfSumMain
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfSumMain.
*!*************************************************************
*! Name      : lfGetSumzBy
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Return the group of summarization.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfGetSumzBy.
*!*************************************************************
*! Name      : lfSetRela
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSetRela

IF !USED('GLBUDDT')
  SELECT 0
  USE (gcDataDir+'GLBUDDT')
ENDIF
SELECT GLBUDDT
SET ORDER TO TAG CDYRACCPRD

IF !USED('GLACBALS')
  SELECT 0
  USE (gcDataDir+'GLACBALS')
ENDIF

SELECT GLACBALS
SET ORDER TO TAG ACCYRPRD
IF lnDumPost <> '0'
  IF USED(lcTempPost)
    SELECT (lcTempPost)
    INDEX ON CACCTCODE+CFISFYEAR+CFSPPRDID TAG ACCYRPRD
    SET ORDER TO TAG ACCYRPRD
  ENDIF  
ENDIF

SELECT (lcRpTargt)

SET RELATION TO lcRpRelVar INTO GLBUDDT  ADDITIVE
SET RELATION TO lcRpRelVar INTO GLACBALS ADDITIVE

IF lnDumPost <> '0' AND USED(lcTempPost)
  SET RELATION TO lcRpRelVar INTO &lcTempPost ADDITIVE
ENDIF

*--End of lfSetRela.
***************************************************************************
* This part is the validations controle the tool provided with this report
***************************************************************************

*!*************************************************************
*! Name      : lfIncStat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfIncStat

lcRpTypeDes = '~sales~Cost of goods sold~Expenses~Other income~Taxes~'
lcRpTotDes  = '~ ~Gross margin~Net income from operation~Net income before;
              taxes~ ~'
lcRpTotExp  = '~ ~S|C~S|C|E~S|C|E|I~ ~'
             
  DEFINE POPUP poTypes MARGIN SCROLL          
  DEFINE POPUP poCol MARGIN SCROLL          
  lsCol = 1
  DIMENSION laTemMaType(1,6),laTemSType(1,6),laTemEType(1,6),laTemCType(1,6)
  DIMENSION laTemIType(1,6),laTemTType(1,6),laTemCOl(1,24),laSegInf(1,2)
*--------- For Columns ------------------------------------

IF lnRpRepWid = 0 
  lnRpInvNo = 0
  RETURN
ELSE
  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
ENDIF
*-------------------------------------------------------------------------

llFirTim     = .T.            && Enter the columns for the first time
lnRpCol      = 1              && Column Index
lcOldColType = ''           && Old column type
lnTypesNo    = 0               && Number of types
llDoneArr    = .F.
llSubType    = .F.
lsTypes      = 1
lcRpArrName  = ' '
lnTpColUsd   = lnRpColUsd
lcOldAlias   = SELECT()
llUsed       = .F.
IF !USED('GLTYPES')
  SELECT 0
  USE (gcDataDir+'GLTYPES') ORDER TAG TypeCode
  llUsed = .T.
ENDIF
SELECT GLTYPES
SET ORDER TO TAG TypeCode
llDataFound = .F.
FOR lnCount = 1 TO 5
 LOCATE FOR CTYPECODE=SUBSTR('SITEC',lnCount,1) AND !EMPT(cTypUacNo)
 IF FOUND()
   llDataFound = .T.
   EXIT
 ENDIF
ENDFOR
IF !llDataFound
  WAIT 'There is no Data in type and ranges' WINDOW NOWAIT
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

=lfDisData()
=lfDisCol()
=lfSaveArr()

*--Save the environment
PUSH KEY
lcOldProc = SET('PROCEDURE')
SET PROCEDURE TO

=lfGetActInf()
DO (gcRepHome + gcAct_Appl + '\GLHOSROW.SPR')   

POP KEY
SET PROCEDURE TO (lcOldProc)

*--End of lfIncStat.
*!*************************************************************
*! Name      : lfColData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Collect Main types and Subtype from file. 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfColData

SELECT LEFT(CTypeCode,1),CTypeDesc;
  FROM &gcDataDir.GLTYPES;
  WHERE LEFT(CTypeCode,1) IN ("S","C","E","I","T");
  AND   SUBSTR(CTypeCode,2,2) <> '00';
  ORDER BY CTypeCode;
  GROUP BY 1;
  INTO ARRAY laRpTemp

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
    lnTypesNo = lnTypesNo + (ALEN(&lcRpArrName.,1)*4)
  ENDFOR
  IF !EMPTY(lcSaveTag)
    SET ORDER TO TAG lcSaveTag
  ENDIF
ENDIF

*--End of lfColData.
*!*************************************************************
*! Name      : lfUpdData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Update Main types and Subtype from file.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--Select the new main type and insert them into the end of array
SELECT SUBSTR(CTypeCode,1,1) ,CTypeDesc;
  FROM &gcDataDir.GLTYPES;
  WHERE LEFT(CTypeCode,1) $ 'SCEIT' ;
  AND   SUBSTR(CTypeCode,2,2) <> '00';
  AND   ASCAN(laRpMaType,LEFT(CTypeCode,1)) = 0  ;
  ORDER BY CTypeCode;
  GROUP BY 1;
  INTO ARRAY laRpTemp

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

*--End of lfUpdData.
*!*************************************************************
*! Name      : lfInsType
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Insert the main type into array.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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
laRpMaType[lnRpMPos,5] = lfRpName(lcRpCode,lcRpTotExp)
laRpMaType[lnRpMPos,6] = ''

*--Search for user define description.
IF !SEEK(lcRpCode+'00') 
  laRpMaType[lnRpMPos,2] = lfRpName(lcRpCode,lcRpTypeDes)
ELSE
  laRpMaType[lnRpMPos,2] = IIF(!EMPTY(ALLTRIM(GLTYPES.CTypeDesc)),;
                   GLTYPES.CTypeDesc,' ')
ENDIF

*--End of lfInsType.
*!*************************************************************
*! Name      : lfRpName
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Return the expersion accourding to its character.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
Function lfRpName
PARAMETERS lcRpValue,lcRpVldEnt

RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT'))))

*--End of lfRpName.
*!*************************************************************
*! Name      : lfDisData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Display Main types and Subtype on screen Colling from show procedure.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDisData

*--Display the Main type.
lcOldArrName = lcRpArrName
RELEASE BARS ALL OF POTYPES
lnBarNo  = 0
lnColNum = 2
FOR lnMaCount = 1 TO ALEN(laRpMaType,1)
  lcRpArrName = "laRp"+laRpMatype[lnMaCount,1]+"Type"
  lnBarNo = lnBarNo + 1
  DEFINE BAR lnBarNo OF poTypes ;
         PROMPT laRpMaType[lnMaCount,1]+"00 Description Ú  "+;
         (ALLTRIM(SUBSTR(laRpMatype[lnMaCount,lnColNum],1,30)))
 
  IF !EMPTY(&lcRpArrName.[1,1])
    FOR lnSCount = 1 to ALEN(&lcRpArrName.,1)
      IF EMPTY(ALLTRIM(&lcRpArrName.[lnSCount,2]))
        &lcRpArrName.[lnSCount,2] = &lcRpArrName.[lnSCount,1]
      ENDIF
       lnBarNo = lnBarNo + 1
        DEFINE BAR lnBarNo OF poTypes ;
         PROMPT "  "+ ALLTRIM(&lcRpArrName.[lnSCount,1])+" Desc.     ³Ú "+;
         (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum],1,30)))
 
      * Display Sub type total and footers
      lnBarNo = lnBarNo + 1
      DEFINE BAR lnBarNo OF poTypes ;
         PROMPT "                ³³ "+;
         'SubTotal of '+(ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum],1,18)))
      lnBarNo = lnBarNo + 1
      DEFINE BAR lnBarNo OF poTypes ;
         PROMPT "  "+ALLTRIM(&lcRpArrName.[lnSCount,1])+" Footer 1  ³³ "+;
         (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum+1],1,30)))
      lnBarNo = lnBarNo + 1
      DEFINE BAR lnBarNo OF poTypes ;
         PROMPT "  "+ALLTRIM(&lcRpArrName.[lnSCount,1])+" Footer 2  ³À "+;
        (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum+2],1,30)))
    
    ENDFOR
  ENDIF
  * Display Main type total and footers
  lnBarNo = lnBarNo + 1
  DEFINE BAR lnBarNo OF poTypes ;
         PROMPT "                ³  "+;
         'SubTotal of '+(ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum],1,18)))
  lnBarNo = lnBarNo + 1
  DEFINE BAR lnBarNo OF poTypes ;
         PROMPT laRpMaType[lnMaCount,1]+"00 Footer 1    ³  "+;
         (ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum+1],1,30)))
  lnBarNo = lnBarNo + 1
  DEFINE BAR lnBarNo OF poTypes ;
         PROMPT laRpMaType[lnMaCount,1]+"00 Footer 2    À  "+;
         (ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum+2],1,30)))
ENDFOR
*--Restore the old array name
lcRpArrName = lcOldArrName

*--End of lfDisData.
*!*************************************************************
*! Name      : lfDisData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Display one Item.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDisItem
PARAMETER llDoChang,lcRpStat,lnRpPos,lnChPosNo

DEFINE BAR lnRpPos OF poTypes ;
  PROMPT SUBSTR(PRMBAR('poTypes',lnRpPos),1,19)+;
  (ALLTRIM(SUBSTR(lcRpStat,1,30)))

IF llDoChang
  DEFINE BAR lnChPosNo OF poTypes ;
         PROMPT SUBSTR(PRMBAR('poTypes',lnChPosNo),1,19)+;
         'SubTotal of '+(ALLTRIM(SUBSTR(lcRpStat,1,18)))
ENDIF
SHOW GET lsTypes

*--End of lfDisItem.
*!*************************************************************
*! Name      : lfvTypes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvTypes

lnMCount = 0
llFound = .F.
lnRpTot = 0
DO WHILE !llFound
  lnMCount = lnMCount + 1
  lcRpArrName = "laRp"+laRpMaType[lnMCount,1]+"Type"
  lnRpTot = ALEN(&lcRpArrName,1)*4+4+lnRpTot
  IF lsTypes <= lnRpTot
    llFound = .T.
  ENDIF
ENDDO
lnRpLen = ALEN(&lcRpArrName,1)*4+4
lnRpPos = lsTypes - (lnRpTot-lnRpLen)
DO CASE
  CASE lsTypes - (lnRpTot-lnRpLen) = 1
    lcRpDesc     = laRpMaType[lnMCount,2]
    lcVarToCahnge= "laRpMaType[lnMCount,2]"
    llSubType = .T.
    lnOldVal = lnMCount
    lnRpYPos = 1
	DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
    =lfDisItem(.T.,laRpMaType[lnOldVal,2],lsTypes,lnRpTot-2)

  CASE INT(lnRpPos / (lnRpLen-2)) = 0
    lnRpPos = lsTypes - (lnRpTot-lnRpLen)
    lnRpYPos = lnRpPos -((ROUND((lnRpPos/4),0)-1)*4)-1
    lnRpXPos = ROUND((lnRpPos/4),0)
    IF lnRpYPos <> 2
      IF lnRpYPos = 1
        llChange = .T.
        lcVarToCahnge= "&lcRpArrName[lnRpXPos,lnRpYPos+1]"
        lcRpDesc     =  &lcRpArrName[lnRpXPos,lnRpYPos+1]
      ELSE  
        llChange = .F.
        lcVarToCahnge= "&lcRpArrName[lnRpXPos,lnRpYPos]"
        lcRpDesc     =  &lcRpArrName[lnRpXPos,lnRpYPos]
      ENDIF
      llSubType = .F.
	  DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
      =lfDisItem(llChange,&lcVarToCahnge,lsTypes,lsTypes+1)
    ENDIF   
         
  CASE INT(lnRpPos / (lnRpLen-2)) <> 0
    lnRpPos = lsTypes - (lnRpTot-lnRpLen)
    lnRpYPos = ROUND(lnRpPos-(lnRpLen-4),0)
    lnRpXPos = lnMCount
    IF lnRpYPos <> 2
      IF lnRpYPos = 1
        lcVarToCahnge= "laRpMaType[lnRpXPos,lnRpYPos+1]"
        lcRpDesc     =  laRpMaType[lnRpXPos,lnRpYPos+1]
      ELSE  
        lcVarToCahnge= "laRpMaType[lnRpXPos,lnRpYPos]"
        lcRpDesc     =  laRpMaType[lnRpXPos,lnRpYPos]
      ENDIF
      llSubType = .T.
	  DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
      =lfDisItem(.F.,&lcVarToCahnge,lsTypes,1)
    ENDIF   
ENDCASE
llSubType = .F.
IF llDoneArr
  =lfDisData()
  llDoneArr = .F.
ENDIF
_CUROBJ = _CUROBJ

*--End of lfvTypes.
*!*************************************************************
*! Name      : lfvArrange
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Arrange Main Types or Subtypes.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvArrange
PARAMETER lnRpArrType,lcPasArrName

IF ALEN(&lcPasArrName,1) > 1
  DEFINE POPUP poArrTypes MARGIN MOVER

  =gfFillPop('poArrTypes',lcPasArrName,2)
  lnRpArrSize = ALEN(&lcPasArrName,1) + 2
  DO (gcRepHome + gcAct_Appl + '\GLINCARR.SPR')   

  IF llDoneArr 
    llDoneArr = .F.
    DO CASE
      CASE lnRpArrType = 1 OR lnRpArrType = 2
        =lfDisData()
        SHOW GET lsTypes    
      CASE lnRpArrType = 3
        =lfDisCol()
        SHOW GET lsCol
    ENDCASE
  ENDIF
ENDIF

*--End of lfvArrange.

*---------------------------------------------------------------------
*------------------ Fill Array  --------------------------------------
*---------------------------------------------------------------------

*!*************************************************************
*! Name      : lfGetNewBar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetNewBar
PARAMETERS lnOldBarNo

FOR lnPoCount = 1 TO CNTBAR('poArrTypes')
  IF GETBAR('poArrTypes',lnPoCount) = lnOldBarNo
    EXIT 
  ENDIF
ENDFOR
RETURN lnPoCount

*--End of lfGetNewBar.
*!*************************************************************
*! Name      : lfvArrOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvArrOk

IF lnRpArrType = 3
  FOR lnBarCount = 1 To CNTBAR('poArrTypes')
    IF GETBAR('poArrTypes',lnBarCount) <> lnBarCount AND;
      laRpCol[lnBarCount,1] = 'O'
      laRpCol[lnBarCount,20] = lfGetNewBar(laRpCol[lnBarCount,20])
      laRpCol[lnBarCount,21] = lfGetNewBar(laRpCol[lnBarCount,21])
    ENDIF
  ENDFOR
ENDIF
=lfModiArr(lcPasArrName,'poArrTypes')
llDoneArr = .T.

*--End of lfvArrOk.
*!*************************************************************
*! Name      : lfModiArr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfModiArr
PARAMETERS lcArrName,lcPopName

lnArrRow = ALEN(&lcArrName,1)
lnArrCol = ALEN(&lcArrName,2)
IF lnArrCol>0
  DIMENSION laTemp[lnArrRow,lnArrCol]
ELSE
  DIMENSION laTemp[lnArrRow]    
ENDIF
IF CNTBAR(lcPopname)>0
  FOR lnCount=1 TO CNTBAR(lcPopname)
    =ACOPY(&lcArrName,laTemp,(GETBAR(lcPopName,lnCount)-1)*IIF(lnArrCol>0,lnArrCol,1)+1,;
           IIF(lnArrCol>0,lnArrCol,1),(lnCount-1)*IIF(lnArrCol>0,lnArrCol,1)+1)
  ENDFOR
  =ACOPY(laTemp,&lcArrName)
ENDIF  

*--End of lfModiArr.

*---------------------------------------------------------------------
*---------------------- Mover ----------------------------------------
*---------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvSum
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvSum
PARAMETER lcRpPasArr

DIMENSION laTemp(1)
lnCoPos = 1
lnDesPos = 2

=gfSubStr(&lcRpPasArr[lnRpXPos,lnRpYPos+2],@laTemp,'|')

DIMENSION laTarget(ALEN(laTemp,1),6)

IF !EMPTY(laTemp)
  FOR lnCount = 1 To ALEN(laTemp,1) 
    FOR lnArrCount = 1 to ALEN(&lcRpPasArr,1)
      IF laTemp[lnCount] = &lcRpPasArr[lnArrCount,1]
        laTarget[lnCount,1] = &lcRpPasArr[lnArrCount,2]
        laTarget[lnCount,2] = laTemp[lnCount]
      ENDIF
    ENDFOR
  ENDFOR
ENDIF
=lfMover(@&lcRpPasArr,@laTarget)

*--End of lfvSum.
*!*************************************************************
*! Name      : lfMover
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfMover
PARAMETERS laSource,laTarget

EXTERNAL ARRAY laSource,laTarget
PUSH KEY CLEAR
lnOldDim =ALEN(laTarget,1)
DECLARE laOldTarg[lnOldDim,6]

pbExit = 2

=ACOPY(laTarget,laOldTarg)

IF ALEN(laTarget,1) = 1 .AND. TYPE('laTarget[1]')="L"
  laTarget[1,1] =' '
  laTarget[1,2] =' '
ENDIF  

DEFINE POPUP puSource  MARGIN RELATIVE SCROLL MARK CHR(16)
FOR lnCount = 1 TO ALEN(laSource,1)
  DEFINE BAR lnCount OF puSource PROMPT (laSource[lnCount,lnDesPos])
  FOR lnTarCount = 1 TO ALEN(laTarget,1)
    IF laTarget[lnTarCount,2] = laSource[lnCount,lnCoPos] 
      SET SKIP OF BAR lnCount OF puSource .T.
    ENDIF
    EXIT
  ENDFOR
ENDFOR

STORE 1  TO lsSource,lsTarget
DO (gcRepHome + gcAct_Appl + '\GLMOVER.SPR')   
POP KEY

*--End of lfMover.
*!*************************************************************
*! Name      : lfMovShow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfMovShow.
*!*************************************************************
*! Name      : lfvSrc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvSrc

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
SHOW GETS

*--End of lfvSrc.
*!*************************************************************
*! Name      : lfvTrgt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvTrgt

IF lsTarget <= ALEN('laTarget',1) AND lsTarget <> 0
  lsSource = 1
  FOR lnSorCount = 1 TO ALEN(laSource,1)
    IF laSource[lnSorCount,1] = laTarget[lsTarget,2] 
      lsSource = lnSorCount
      EXIT
    ENDIF
  ENDFOR

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
SHOW GETS

*--End of lfvTrgt.
*!*************************************************************
*! Name      : lfvMovm
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvMovm
PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    _CUROBJ = OBJNUM(lsSource)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 2
    DECLARE laTarget[ALEN('laSource',1),6]
    FOR lnSorCount = 1 TO ALEN(laSource,1)
      laTarget[lnSorCount,1] = laSource[lnSorCount,2]
      laTarget[lnSorCount,2] = laSource[lnSorCount,1]
      laTarget[lnSorCount,3] = laSource[lnSorCount,3]
      laTarget[lnSorCount,4] = laSource[lnSorCount,4]
      laTarget[lnSorCount,5] = laSource[lnSorCount,5]
      laTarget[lnSorCount,6] = laSource[lnSorCount,6]
    ENDFOR
    SET SKIP OF POPUP puSource .T.
    SHOW GETS
  CASE lnMovmnts = 3
    _CUROBJ = OBJNUM(lsTarget)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 4
    DECLARE laTarget[1,6]
    laTarget =''
    SET SKIP OF POPUP puSource .F.
    SHOW GETS
ENDCASE

*--End of lfvMovm.
*!*************************************************************
*! Name      : lfvCancMov
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCancMov

DECLARE laTarget[lnOldDim,6]
=ACOPY(laOldTarg,laTarget)

*--End of lfvCancMov.
*!*************************************************************
*! Name      : lfvOkMov
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOkMov

laSource[lnRpXPos,lnRpYPos+2] = ''
FOR lnCount = 1 To ALEN(laTarget,1)
  IF LEN(laTarget[lnCount,lnCoPos])>0
    laSource[lnRpXPos,lnRpYPos+2] = laSource[lnRpXPos,lnRpYPos+2] +;
                           IIF(EMPTY(laSource[lnRpXPos,lnRpYPos+2]),'','|')+;
                           laTarget[lnCount,lnDesPos]
                          
   ENDIF                          
ENDFOR                        

*--End of lfvOkMov.

*-------------------------------------------------------------------*
*---------------------------- COLOMNS ------------------------------*
*-------------------------------------------------------------------*
*!*************************************************************
*! Name      : lfColShow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfColShow

DO CASE
  CASE CNTBAR('poCol') = 0
    SHOW GET pbRemove DISABLE
    SHOW GET pbArrCol DISABLE
    SHOW GET pbAdd ENABLE
  CASE CNTBAR('poCol') + lnRpIdnCol >= lnRpInvNo
    SHOW GET pbRemove ENABLE
    SHOW GET pbAdd DISABLE  
    SHOW GET pbArrCol ENABLE
  CASE  CNTBAR('poCol') <= 1
    SHOW GET pbArrCol DISABLE
    SHOW GET pbRemove ENABLE
    SHOW GET pbAdd ENABLE  
  OTHERWISE
    SHOW GET pbArrCol ENABLE
    SHOW GET pbRemove ENABLE
    SHOW GET pbAdd ENABLE  
ENDCASE

*--End of lfColShow.
*!*************************************************************
*! Name      : lfwCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfwCol

IF CNTBAR('poCol') = 0 
  IF LASTKEY() = 9 OR LASTKEY() = 4
    _CUROBJ = _CUROBJ + 1
  ELSE
    _CUROBJ = _CUROBJ - 1
  ENDIF
ENDIF

*--End of lfwCol.
*!*************************************************************
*! Name      : lfvCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol

llEditMode = .T.
DIMENSION laTempCol(ALEN(laRpCol,1),24)
=ACOPY (laRpCol,laTempCol)
lnNewCol = 0
lnRpCol = lsCol
lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                         lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lnRpCol,23])
llFirTim = .T.

DO (gcRepHome + gcAct_Appl + '\GLHOSCOL.SPR')   
SHOW GETS

*--End of lfvCol.
*!*************************************************************
*! Name      : lfvAddCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvAddCol

llEditMode = .F.
DIMENSION laTempCol(ALEN(laRpCol,1),24)
=ACOPY (laRpCol,laTempCol)
llFirTim = .T.
lnNewCol = 0
lnRpNoCol = CNTBAR('poCol') + 1
lnRpCol   = lnRpNoCol

IF ALEN(laRpCol,1) < lnRpNoCol
  DIMENSION laRpCol(lnRpNoCol,24)
ENDIF

laRpCol[lnRpCol,1]  = 'B'
laRpCol[lnRpCol,2]  = 'Co'+ALLTRIM(STR(lnRpCol))
laRpCol[lnRpCol,3]  = 'Co'+ALLTRIM(STR(lnRpCol))
laRpCol[lnRpCol,4]  = 0
laRpCol[lnRpCol,5]  = 0
laRpCol[lnRpCol,6]  = 0
laRpCol[lnRpCol,7]  = 0
laRpCol[lnRpCol,8]  = ' '
laRpCol[lnRpCol,9]  = ' '
laRpCol[lnRpCol,10] = 1
laRpCol[lnRpCol,11] = 1
laRpCol[lnRpCol,12] = 1
laRpCol[lnRpCol,13] = ''
laRpCol[lnRpCol,14] = ''
laRpCol[lnRpCol,15] = ''
laRpCol[lnRpCol,16] = ''
laRpCol[lnRpCol,17] = ''
laRpCol[lnRpCol,18] = ''
laRpCol[lnRpCol,19] = 1
laRpCol[lnRpCol,20] = 0
laRpCol[lnRpCol,21] = 0
laRpCol[lnRpCol,22] = 1
laRpCol[lnRpCol,23] = 0
laRpCol[lnRpCol,24] = STRTRAN(SUBSTR(ALLTRIM(lcRpSegMsk),;
                              AT('-',lcRpSegMsk,1)+1,24),'#','*')

DO (gcRepHome + gcAct_Appl + '\GLHOSCOL.SPR')   
SHOW GETS

*--End of lfvAddCol.
*!*************************************************************
*! Name      : lfvRemCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvRemCol

IF CNTBAR('poCol') > 0
  lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                          lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lsCol,23])
  lnRpColUsd = lnRpColUsd-IIF(laRpCol[lsCol,1] $ 'AB',MAX(1,IIF(laRpCol[lsCol,23]=1,1,0)+laRpCol[lsCol,7]+1),1)                          
  =ADEL(laRpCol,lsCol)
  IF ALEN(laRpCol,1) > 1
    DIMENSION laRpCol(ALEN(laRpCol,1)-1,24)
  ENDIF
  =lfDisCol()
ENDIF
SHOW GETS

*--End of lfvRemCol.
*!*************************************************************
*! Name      : lfvOkCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOkCol
PRIVATE llRetVal 

llRetVal = .T.
IF ATC(laRpCol[lnRpCol,1],'BAOP') > 2
  laRpCol[lnRpCol,4] = 0
  laRpCol[lnRpCol,5] = 0 
  laRpCol[lnRpCol,6] = 0
  laRpCol[lnRpCol,7] = 0
ENDIF
DO CASE
  CASE laRpCol[lnRpCol,1] = 'B'
    IF EMPTY(laRpCol[lnRpCol,8]) OR EMPTY(laRpCol[lnRpCol,9]) OR;
       EMPTY(laRpCol[lnRpCol,10]) OR EMPTY(laRpCol[lnRpCol,11])
      llRetVal = .F. 
    ENDIF
  CASE laRpCol[lnRpCol,1]  = 'A'
    IF laRpCol[lnRpCol,12] = 1
      IF EMPTY(laRpCol[lnRpCol,13]) OR EMPTY(laRpCol[lnRpCol,14])
        llRetVal = .F. 
      ENDIF
    ELSE
      IF EMPTY(laRpCol[lnRpCol,15]) OR EMPTY(laRpCol[lnRpCol,16]) OR;
         EMPTY(laRpCol[lnRpCol,17]) OR EMPTY(laRpCol[lnRpCol,18]) 
        llRetVal = .F. 
      ENDIF
    ENDIF
ENDCASE
IF !llRetVal 
  WAIT 'You have to enter the column information' WINDOW NOWAIT 
  _CUROBJ = _CUROBJ
ELSE
  IF llEditMode
    lnRpColUsd = lnRpColUsd-IIF(laTempCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laTempCol[lnRpCol,23]=1,1,0)+laTempCol[lnRpCol,7]+1),1)  
  ENDIF
  CLEAR READ
****
  lnRpColUsd = lnRpColUsd+IIF(laRpCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laRpCol[lnRpCol,23]=1,1,0)+laRpCol[lnRpCol,7]+1),1)
ENDIF
=lfDisCol()

*--End of lfvOkCol.
*!*************************************************************
*! Name      : lfvCancCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCancCol

DIMENSION laRpCol(ALEN(laTempCol,1),24)
=ACOPY (laTempCol,laRpCol)
CLEAR READ
=lfDisCol()

*--End of lfvCancCol.
*!*************************************************************
*! Name      : lfvIdnCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvIdnCol
PARAMETERS lnColNo

lnDefCol = lnRpInvNo - (lnRpIdnCol + laRpCol[lnRpCol,23]+1)
lcIdent = 'cbIdent'+ALLTRIM(STR(lnColNo-3))
IF &lcIdent = 1
  IF lnDefCol >= 1
    FOR lnColCount = lnColNo TO 7
      IF  (laRpCol[lnRpCol,lnColCount] + 1) <= lnDefCol
        laRpCol[lnRpCol,lnColCount] = laRpCol[lnRpCol,lnColCount] + 1
      ELSE
        laRpCol[lnRpCol,lnColCount] = laRpCol[lnRpCol,lnColCount-1]
      ENDIF
    ENDFOR
  ENDIF
ELSE
  FOR lnColCount = lnColNo TO 7
    lcIdent = 'cbIdent'+ALLTRIM(STR(lnColCount-3))
    laRpCol[lnRpCol,lnColCount] = laRpCol[lnRpCol,lnColCount] - 1
    SHOW GET (lcIdent) ENABLE
  ENDFOR
  SHOW GET laRpCol[lnRpCol,23] ENABLE
ENDIF
SHOW GETS 

*--End of lfvIdnCol.
*!*************************************************************
*! Name      : lfDisCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDisCol

lnRpIdnCol = 0
lnCount = 1 
RELEASE BAR ALL OF poCol
lnRpNoCol = 0
FOR lnCount = 1 TO ALEN(laRpCol,1) 
  lnRpNoCol = lnRpNoCol + 1
  IF !EMPTY(laRpCol[lnCount,1])
    lnRpIdnCol = lnRpIdnCol + IIF(ATC(laRpCol[lnCount,1],'BAO') > 2,0,;
                         lfBigNo('laRpCol',lnCount,4,7)+laRpCol[lnCount,23])
    DEFINE BAR lnRpNoCol OF poCol ;
         PROMPT IIF(EMPTY(ALLTRIM(laRpCol[lnCount,2]+laRpCol[lnCount,3])),;
                   'Col '+ALLTRIM(STR(lnCount)),laRpCol[lnCount,2]+' '+;
                   laRpCol[lnCount,3])
  ENDIF                
ENDFOR
SHOW GET lsCol 

*--End of lfDisCol.
*!*************************************************************
*! Name      : lfBigNo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfBigNo
PARAMETERS lcPaArrName,lnRowNo,lnFromCol,lnToCol

lnBigNo = 0
FOR lnArrIndx = lnFromCol TO lnToCol
   lnBigNo=MAX(lnBigNo,&lcPaArrName[lnRowNo,lnArrIndx])
ENDFOR
RETURN lnBigNo

*--End of lfBigNo.
*!*************************************************************
*! Name      : lfShowColTp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

=lfvCol12()

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

*--End of lfShowColTp.

*-------------------------------------------------------------------*
*--------- Data Validation for column information ------------------*
*----- Start with 'lfvCol' + the column no in the array ------------*
*-like lfvCol13 ------> the valid function for laRpCol[lnRpCol,13]--*
*-------------------------------------------------------------------*

*!*************************************************************
*! Name      : lfvCol1
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol1

llRetVal = .T.

IF rbColmntype = 3 AND laRpCol[lnRpCol,1] $ 'BA'
  FOR lnCount = 1 TO lnRpNoCol
  
    IF (laRpCol[lnCount,20] = lnRpCol OR;
        laRpCol[lnCount,21] = lnRpCol) AND;
       laRpCol[lnCount,1] $ 'OP'
       
       rbColmntype = ATC(lcOldColType,'BAOP')
       SHOW GET rbColmntype
       llRetVal = .F.
       WAIT "This column is used by column "+ALLTRIM(STR(lnCount)) WINDOW NOWAIT
       EXIT
    ENDIF
  ENDFOR
ENDIF
IF llRetVal  
   SHOW GETS WINDOW ('LWCOLTP'+SUBSTR('BAO',rbColmnType,1)) ENABLE ONLY   
   ACTI WINDOW ('LWCOLTP'+SUBSTR('BAO',rbColmnType,1)) TOP
   IF laRpCol[lnRpCol,1] <> SUBSTR('BAO',rbColmnType,1)
     SHOW GETS WINDOW ('LWCOLTP'+laRpCol[lnRpCol,1]) DISABLE ONLY
   ENDIF
   =lfvCol12()
ENDIF  
RETURN llRetVal

*--End of lfvCol1.
*!*************************************************************
*! Name      : lfvCol8
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol8

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD") 
    SELECT 0
    USE &gcDataDir.GLBUDHD ORDER TAG BdCodYr
    llUesdBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *--Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. ) .OR. ;
     (EMPTY(laRpCol[lnRpCol,9]) AND !SEEK(&lcRpCurFld)) .OR. ;
     (!EMPTY(laRpCol[lnRpCol,9]) AND !SEEK(&lcRpCurFld+laRpCol[lnRpCol,9]))
    =gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,9] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,9]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol8.
*!*************************************************************
*! Name      : lfvCol9
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol9

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF !EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD") 
    SELECT 0
    USE &gcDataDir.GLBUDHD ORDER TAG BdCodYr
    llUesdBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(laRpCol[lnRpCol,8]+&lcRpCurFld.))
    =gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,8]  = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,8]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol9.
*!*************************************************************
*! Name      : lfvCol10
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol10

IF !BETWEEN(laRpCol[lnRpCol,10],1,13) OR ;
        IIF(EMPTY(laRpCol[lnRpCol,11]),.F.,laRpCol[lnRpCol,11]<laRpCol[lnRpCol,10])
  _CUROBJ = _CUROBJ
ENDIF

*--End of lfvCol10.
*!*************************************************************
*! Name      : lfvCol11
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol11

IF !BETWEEN(laRpCol[lnRpCol,11],laRpCol[lnRpCol,10],13)
 _CUROBJ = _CUROBJ
ENDIF

*--End of lfvCol11.
*!*************************************************************
*! Name      : lfwCol12
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfwCol12

lcTempTy = SUBSTR('BAO',rbColmntype,1)
DO CASE
  CASE laRpCol[lnRpCol,12] = 1 AND lcTempTy = 'A'
    SHOW GET laRpCol[lnRpCol,13]  ENABLE
    SHOW GET laRpCol[lnRpCol,14]  ENABLE
    SHOW GET laRpCol[lnRpCol,15]  DISABLE
    SHOW GET laRpCol[lnRpCol,16]  DISABLE
    SHOW GET laRpCol[lnRpCol,17]  DISABLE
    SHOW GET laRpCol[lnRpCol,18]  DISABLE

  CASE laRpCol[lnRpCol,12] = 2 AND lcTempTy = 'A'
    SHOW GET laRpCol[lnRpCol,13]  DISABLE
    SHOW GET laRpCol[lnRpCol,14]  DISABLE
    SHOW GET laRpCol[lnRpCol,15]  ENABLE
    SHOW GET laRpCol[lnRpCol,16]  ENABLE
    SHOW GET laRpCol[lnRpCol,17]  ENABLE
    SHOW GET laRpCol[lnRpCol,18]  ENABLE
ENDCASE

*--End of lfwCol12.
*!*************************************************************
*! Name      : lfvCol13
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol13

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
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
  IF ('?' $ &lcRpCurFld. )
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,14] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,14]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol13.
*!*************************************************************
*! Name      : lfvCol14
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Check if current company has this entried period or not.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol14

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF !EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUesdBefo = .T.
  ENDIF
  SELECT FSPRD

  *-- Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR.;  
    !SEEK(ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,13] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,13]
  ENDIF

  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol14.
*!*************************************************************
*! Name      : lfvCol15
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol15

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
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
  IF ('?' $ &lcRpCurFld. OR EMPTY(laRpCol[lnRpCol,16]))
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,16] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,16]
  ELSE
    IF !EMPTY(laRpCol[lnRpCol,17]) AND;
             (laRpCol[lnRpCol,18]<> laRpCol[lnRpCol,16] OR;
              laRpCol[lnRpCol,17] < laRpCol[lnRpCol,15])        
      =gfBrows("FOR CFisFyear+CFsppRdid<=;
             laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
         'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
      laRpCol[lnRpCol,16] = laRpRetFld[1]
      laRpCol[lnRpCol,15] = laRpRetFld[2]
    ENDIF
  ENDIF
  IF laRpCol[lnRpCol,16] <> laRpCol[lnRpCol,18]
    laRpCol[lnRpCol,18] = ''
    laRpCol[lnRpCol,17] = ''
    SHOW GET laRpCol[lnRpCol,18]
    SHOW GET laRpCol[lnRpCol,17]
  ENDIF

  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol15.
*!*************************************************************
*! Name      : lfvCol16
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol16

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
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
    !SEEK(ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,15] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,15]
  ELSE
    IF !EMPTY(laRpCol[lnRpCol,17]) AND;
       (laRpCol[lnRpCol,18]<> laRpCol[lnRpCol,16] OR;
        laRpCol[lnRpCol,17] < laRpCol[lnRpCol,15])
        
      =gfBrows("FOR CFisFyear+CFsppRdid<=;
             laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
         'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

      laRpCol[lnRpCol,16] = laRpRetFld[1]
      laRpCol[lnRpCol,15] = laRpRetFld[2]
      SHOW GET (lcRpCurFld)
      SHOW GET laRpCol[lnRpCol,15]    
    ENDIF
  ENDIF
  IF laRpCol[lnRpCol,16] <> laRpCol[lnRpCol,18]
    laRpCol[lnRpCol,18] = ''
    laRpCol[lnRpCol,17] = ''
    SHOW GET laRpCol[lnRpCol,18]
    SHOW GET laRpCol[lnRpCol,17]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol16.
*!*************************************************************
*! Name      : lfvCol17
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol17

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUesdBefo = .T.
  ENDIF
  SELECT FSPRD
  
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. )
    =gfBrows("FOR CFisFyear = laRpCol[lnRpCol,16];
        AND CFsppRdid >= laRpCol[lnRpCol,15]",;
    'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,18] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,18]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol17.
*!*************************************************************
*! Name      : lfvCol18
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol18

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
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
  IF ('?' $ &lcRpCurFld  OR;  
    !SEEK(ALLTRIM(&lcRpCurFld)+laRpCol[lnRpCol,17])) OR;
    (laRpCol[lnRpCol,17] <laRpCol[lnRpCol,15] OR ;
    laRpCol[lnRpCol,18] <> laRpCol[lnRpCol,16])
    
    =gfBrows("FOR CFisFyear = laRpCol[lnRpCol,16];
        AND CFsppRdid >= laRpCol[lnRpCol,15]",;
    'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,17] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,17]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN FSPRD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvCol18.
*!*************************************************************
*! Name      : lfvCol20
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol20

IF laRpCol[lnRpCol,20] > 0
  IF !BETWEEN(laRpCol[lnRpCol,20],1,lnRpNoCol) OR;
            laRpCol[lnRpCol,20] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,20],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,20],1] <> 'A')
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF  

*--End of lfvCol20.
*!*************************************************************
*! Name      : lfvCol21
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol21

IF laRpCol[lnRpCol,21] > 0
  IF !BETWEEN(laRpCol[lnRpCol,21],0,lnRpNoCol) OR;
            laRpCol[lnRpCol,21] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,21],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,21],1] <> 'A')
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

*--End of lfvCol21.
*!*************************************************************
*! Name      : lfvCol23
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCol23

IF laRpCol[lnRpCol,23] = 0
  SHOW GET laRpCol[lnRpCol,22] DISABLE
  FOR lnColCount = 4 TO 7
    lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
    SHOW GET (lcCbIdent) ENABLE
  ENDFOR
ELSE
  SHOW GET laRpCol[lnRpCol,22] ENABLE
  IF lnRpNoCol+lnRpIdnCol+lfBigNo('laRpCol',lnRpCol,4,7)+laRpCol[lnRpCol,23]  >= lnRpInvNo 
    FOR lnColCount = 4 TO 7
      lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
      IF &lcCbIdent = 0
        SHOW GET (lcCbIdent) DISABLE
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--End of lfvCol23.

*---------------------------------------------------------------------
*------------ This part for control Ok or Cancel in the --------------
*------------------------ Main Screen --------------------------------
*---------------------------------------------------------------------

*!*************************************************************
*! Name      : lfSaveArr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Save all arrays information for retreving if push cancel.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSaveArr

DIMENSION laTemMaType(ALEN(laRpMaType,1),6)
=ACOPY(laRpMaType,laTemMaType)

DIMENSION laTemSType(ALEN(laRpSType,1),6)
=ACOPY(laRpSType,laTemSType)

DIMENSION laTemEType(ALEN(laRpEType,1),6)
=ACOPY(laRpEType,laTemEType)

DIMENSION laTemCType(ALEN(laRpCType,1),6)
=ACOPY(laRpCType,laTemCType)

DIMENSION laTemIType(ALEN(laRpIType,1),6)
=ACOPY(laRpIType,laTemIType)

DIMENSION laTemTType(ALEN(laRpTType,1),6)
=ACOPY(laRpTType,laTemTType)

DIMENSION laTemCol(ALEN(laRpCol,1),24)
=ACOPY(laRpCol,laTemCol)

*--End of lfSaveArr.
*!*************************************************************
*! Name      : lfvCancAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Cancel all information that was added.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCancAll

DIMENSION laRpMaType(ALEN(laTemMaType,1),6)
=ACOPY(laTemMaType,laRpMaType)

DIMENSION laRpSType(ALEN(laTemSType,1),6)
=ACOPY(laTemSType,laRpSType)

DIMENSION laRpEType(ALEN(laTemEType,1),6)
=ACOPY(laTemEType,laRpEType)

DIMENSION laRpCType(ALEN(laTemCType,1),6)
=ACOPY(laTemCType,laRpCType)

DIMENSION laRpIType(ALEN(laTemIType,1),6)
=ACOPY(laTemIType,laRpIType)

DIMENSION laRpTType(ALEN(laTemTType,1),6)
=ACOPY(laTemTType,laRpTType)

DIMENSION laRpCol(ALEN(laTemCol,1),24)
=ACOPY(laTemCol,laRpCol)
lnRpColUsd = lnTpColUsd

*--End of lfvCancAll.
*!*************************************************************
*! Name      : lfvCancAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Save all information that was added
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOkAll

llRpToolCh = .T.

*--End of lfvOkAll.
*!*************************************************************
*! Name      : lfGetColInf
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetColInf
PRIVATE lnColNo,lnColCount,lnColIndent,lcColString,lnIndent,lnBudAct

STORE 0 TO lnColNo,lnColCount,lnColIndent,lnBudAct
lnColIndent = 1
lcColString = [,ST,SF,MT,MF,]
STORE '' TO laColInf
FOR  lnColNo = 1 TO ALEN(laRpCol,1)
     IF laRpCol[lnColNo,1] $ 'AB'
       lnTmpInd_7 = laRpCol[lnColNo,7]
       lnTmpInd_6 = laRpCol[lnColNo,6]       
       lnTmpInd_5 = laRpCol[lnColNo,5]       
       lnTmpInd_4 = laRpCol[lnColNo,4]       
       IF lnRpSumzBy = 1
         laRpCol[lnColNo,7] = IIF(laRpCol[lnColNo,7]>0,1,0)
         laRpCol[lnColNo,6] = 0         
         laRpCol[lnColNo,5] = 0
         laRpCol[lnColNo,4] = 0
       ENDIF
       lnBudAct = lnColNo
       lnIndent = MAX(1,IIF(laRpCol[lnColNo,23] = 1 , 1 , 0 ) + laRpCol[lnColNo,7]+1)
       laColInf[1,lnColIndent] = 'RS' + ALLTRIM(STR(lnBudAct))  
       FOR lnColCount = 1 TO 4
         lnWhichcol = IIF(lnColCount > 2 , IIF(lnRpSumzBy = 1 , 2 , 0 ) , 0 )
         laColInf[1,lnColIndent + laRpCol[lnColNo,lnColCount+3]] = ;
         laColInf[1,lnColIndent + laRpCol[lnColNo,lnColCount+3]] + ;
         IIF(EMPTY(laColInf[1,lnColIndent+laRpCol[lnColNo,lnColCount+3]]),'',',')    ;
         +SUBSTR(lcColString,ATC(',',lcColString,lnColCount)+1                      ,;
               ATC(',',lcColString,lnColCount+1)-ATC(',',lcColString,lnColCount)-1) +;
               +ALLTRIM(STR(lnBudAct))
       ENDFOR
       IF laRpCol[lnColNo,23] = 1
         laColInf[1,lnColIndent+lnIndent-1] = 'PS,' + ALLTRIM(STR(lnBudAct)) ;
             + ',' + ALLTRIM(STR(lnColNo))
       ENDIF
     ELSE
       laColInf[1,lnColIndent] = 'OP,' ;
                +ALLTRIM(STR(laRpCol[lnColNo,19]))+',';
                +ALLTRIM(STR(laRpCol[lnColNo,20]))+',';
                +ALLTRIM(STR(lnColNo))+',';
                +ALLTRIM(STR(laRpCol[lnColNo,21]))
     ENDIF
   lnColIndent = MIN(lnRpColUsd,lnColIndent+IIF(laRpCol[lnColNo,1] $ 'AB',IIF(laRpCol[lnColNo,23]=1,1,0)+laRpCol[lnColNo,7]+1,1))
   laRpCol[lnColNo,7] = lnTmpInd_7
   laRpCol[lnColNo,6] = lnTmpInd_6
   laRpCol[lnColNo,5] = lnTmpInd_5
   laRpCol[lnColNo,4] = lnTmpInd_4
ENDFOR

*--bas
IF lnRpYOrP = 2                               &&'PTD'
  ln1stDem = ALEN(laColInf,1)
  ln2ndDem = ALEN(laColInf,2)
  DIMENSION laStorage[ln1stDem , ln2ndDem]
  =ACOPY(laColInf , laStorage)
ELSE                                          &&'YTD'
  FOR lnLopYtd = 1 TO ALEN(laColInf,1)

*    laStorage[lnLopYtd , 4 ] = laColInf[lnLopYtd , 1]
*    laStorage[lnLopYtd , 5 ] = laColInf[lnLopYtd , 2]
*    laStorage[lnLopYtd , 6 ] = laColInf[lnLopYtd , 3]

    laStorage[lnLopYtd , 4 ] = STRTRAN(laColInf[lnLopYtd , 1] , "1" , "3")
    laStorage[lnLopYtd , 5 ] = STRTRAN(laColInf[lnLopYtd , 2] , "2" , "4")
    laStorage[lnLopYtd , 6 ] = STRTRAN(laColInf[lnLopYtd , 3] , "1" , "3")
    laStorage[lnLopYtd , 6 ] = STRTRAN(laStorage[lnLopYtd , 6 ] , "2" , "4")

  ENDFOR
  STORE '' TO laColInf
  =ACOPY(laStorage , laColInf)
ENDIF
*--bas
*--End of lfGetColInf.
*!*************************************************************
*! Name      : lfGetVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetVal
PARAMETERS lnColNo

lcColTyp = LEFT(cGroup,2)
lcGroup  = ALLTRIM(cGroup)
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
lnRpColWid=lnRpColWid-LEN(SET('CURR',1))
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
            ,STRTRAN(lcRpTemPic,'?','*')))            

lnRpColWid = lnRPTempCol
IF ('PS' $ laColInf[1,lnColNo] OR 'OP' $ laColInf[1,lnColNo]) AND LEFT(STR(lnRetVal),1)='*'
   RETURN PADL('N/A',lnRpColWid)
ENDIF
IF LEFT(STR(lnRetVal),1)='*'
  RETURN IIF(SET('CURR')='LEFT',SET('CURR',1),'')+STRTRAN(lcRpTemPic,'?','*')+IIF(SET('CURR')='RIGHT',SET('CURR',1),'')
ENDIF
RETURN PADL(ALLTRIM(lcRetVal),lnRpColWid)

*--End of lfGetVal.
*!*************************************************************
*! Name      : lfGetHead
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetHead
PARAMETERS lnColNo,lnTitNo

IF !('RS' $ laColInf[1,lnColNo]) ;
  AND ('PS' $ laColInf[1,lnColNo]) AND !('OP' $ laColInf[1,lnColNo])
  RETURN ' '
ELSE
  IF 'RS' $ laColInf[1,lnColNo]
    lcColTyp = 'RS'
    lcColNo  = SUBSTR(laColInf[1,lnColNo],ATC(lcColTyp,laColInf[1,lnColNo])+2)
    lnComPos = ATC(',',lcColNo)
    lcColNo  = IIF(lnComPos>1,SUBSTR(lcColNo,1,lnComPos-1),SUBSTR(lcColNo,1))      
  ELSE
    lcColNo  = SUBSTR(laColInf[1,lnColNo],ATC(',',laColInf[1,lnColNo],3)+1,;
               ATC(',',laColInf[1,lnColNo],4)-ATC(',',laColInf[1,lnColNo],3)-1)         
  ENDIF
  RETURN  IIF(TYPE(lcColNo)<>'N','',laRpCol[EVAL(lcColNo),lnTitNo+1])
ENDIF     

*--End of lfGetHead.
*!*************************************************************
*! Name      : lfGetActInf
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetActInf

lcOldAlias = SELECT()

IF !USED('ACCOD')
  SELECT 0
  USE (gcDataDir+"ACCOD")
ENDIF
SELECT ACCOD
LOCATE

lcSegMask  = CACSMASK 
lnRpTotSeg = NACSNOSEG
DIMENSION laSegInf[1,2]
STORE '' TO laSegInf

SELECT NACSSIZE,CACSSHDES;
FROM &gcDataDir.ACCOD ;
  WHERE NACSSEGSZ < 1 ;
  INTO ARRAY laSegInf  

SELECT (lcOldAlias)
RETURN lcSegMask

*--End of lfGetActInf.
*!*************************************************************
*! Name      : lfGetPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetPic

RETURN SUBSTR(ALLTRIM(lcRpSegMsk),AT('-',lcRpSegMsk,1)+1,24)

*--ENd of lfGetPic.
*!*************************************************************
*! Name      : lfGetSegDis
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetSegDis

lcRpRetVal = ''
FOR lnSegCount = 2 TO lnRpTotSeg-1
  lcRpRetVal = lcRpRetVal + ALLTRIM(laSegInf[lnSegCount,2])+'-'
ENDFOR
RETURN (lcRpRetVal + ALLTRIM(laSegInf[lnRpTotSeg,2]))

*--End of lfGetSegDis.
*!*************************************************************
*! Name      : lfDesWidth
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfDesWidth.
*!*************************************************************
*! Name      : lfRowDes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfRowDes

DO CASE
  CASE (CGROUP = 'H' AND RIGHT(CTYPECODE,2) = '00') OR 'MT' $ CGROUP
    RETURN cRowDes
  CASE (CGROUP = 'H' AND RIGHT(CTYPECODE,2) <> '00') OR 'ST' $ CGROUP
     RETURN SPACE(IIF(llRpSubInd , lnRpSubInd , 0 )) + cRowDes
  OTHERWISE
     RETURN SPACE(IIF(llRpSubInd , lnRpSubInd , 0 )) +;
            SPACE(IIF(llRpAccInd , lnRpAccInd , 0 )) +;
            crowdes
ENDCASE

*--End of lfRowDes.
*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfRepShow.
*!*************************************************************
*! Name      : lfZoomWin
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfZoomWin

DO CASE
  CASE _DOS
    laRpCol[lnRpCol,1]=SUBSTR('BAOPU',rbColmntype,1)
    IF laRpCol[lnRpCol,1] $ 'BA' 
      IF WROWS('lwHosColum')<=12
        MOVE WINDOW LWCOLTYPE2 BY -1,0
        MOVE WINDOW LWCOLTYPE3 TO 16,0
        ZOOM WINDOW lwHosColum  NORM  SIZE 20,78
        MOVE WINDOW lwHosColum CENTER
        SHOW GETS ONLY    
      ENDIF  
    ELSE
      IF WROWS('lwHosColum')>12
        ZOOM WINDOW lwHosColum NORM SIZE 12,78
        MOVE WINDOW LWCOLTYPE2 BY  1,0
        MOVE WINDOW LWCOLTYPE3 TO 9,0
        SHOW GETS ONLY   
      ENDIF  
    ENDIF
  
  CASE _WINDOWS
    laRpCol[lnRpCol,1]=SUBSTR('BAOPU',rbColmntype,1)
    IF laRpCol[lnRpCol,1] $ 'BA' 
      IF WROWS('lwHosColum')<=22
        MOVE WINDOW LWCOLTYPE2 BY -4,0
        MOVE WINDOW LWCOLTYPE3 TO 28,0
        ZOOM WINDOW lwHosColum  NORM  SIZE 35,70
        MOVE WINDOW lwHosColum CENTER
        SHOW GETS ONLY
      ENDIF  
    ELSE
      IF WROWS('lwHosColum')>22
        ZOOM WINDOW lwHosColum NORM SIZE 22,70
        MOVE WINDOW LWCOLTYPE2 BY  4,0
        MOVE WINDOW LWCOLTYPE3 TO 15,0
        SHOW GETS ONLY
      ENDIF
    ENDIF
ENDCASE

*--End of lfZoomWin.
*!*************************************************************
*! Name      : lfUpDaCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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
  DO (gcRepHome + gcAct_Appl + '\GLUPDATE.SPR')   
ENDIF

*--End of lfUpDaCol.
*!*************************************************************
*! Name      : lfwUpDaCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfwUpDaCol.
*!*************************************************************
*! Name      : lfvCangeTy
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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

*--End of lfvCangeTy.
*!*************************************************************
*! Name      : lfvOkUpd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
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
  WAIT 'You have to enter the column information' WINDOW NOWAIT 
  _CUROBJ = _CUROBJ
ELSE
  FOR lnCount = 1 TO ALEN(laRpCol,2)
    laRpCol[lnRpCol,lnCount] = laFldName[lnCount]  
  ENDFOR
  CLEAR READ
ENDIF

*--End of lfvOkUpd.
*---------------------------------------------------------------------
*----------------- EOF THE END OF PROGRAM LIST -----------------------
*---------------------------------------------------------------------

*!*************************************************************
*! Name      : lfGetCurPr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetCurPr

DECLARE laRpRetFld(1)
lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
&& Check If year field is empty
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
    llUsedsyc = .f.  
  
  IF !USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD 
    llUsedsyc = .T.
  ENDIF  
  
  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  lcYearVal = LEFT(lcRpCurFld,6)+'Year'
  *--Search for the current company+year+Prd
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    =gfBrows(IIF(EMPTY(&lcYearVal),'',[&lcYearVal]),'CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFSPPRDID
  ENDIF
  SHOW GET (lcRpCurFld)  
IF lcRpStYear+lcRpStPrid>lcRpEdYear+lcRpEdPrid
  WAIT 'Beginning year/period must be less than or equal to the Ending year/period' window nowait
  &lcRpCurFld=lcRpOld
  SHOW GET (lcRpCurFld)
  RETURN 
ENDIF
IF lcOGManRep <> 'GLINCST4' AND lcOGManRep <> 'GLINCS13'
  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpStYear , FISHD.CFISFYEAR ,'COMPFYEAR')
  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpEdYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  
  lcTmpStPr=lcRpStPrid
  lcTmpEdPr=lcRpEdPrid  
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol

  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
  IF lnNoOfCol<=lnRpInvNo
    lnRpColUsd = lnNoOfCol
  ELSE
    WAIT 'The report width not enough for this range of periods' WINDOW NOWAIT
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
  SHOW GET lcRpStPrid
  SHOW GET lcRpEdPrid
  SHOW GET (lcRpCurFld)
RETURN 

*--End of lfGetCurPr.
*!*************************************************************
*! Name      : lfvCurYear
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvCurYear

DECLARE laRpRetFld(1)
lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF
llUsedBefo = .F.        && Check if used before or this the first time
lcOldAlias = SELECT()    && Save the current alias
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
&& Check If year field is empty
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  llUsedsyc  = .F. 

  IF !USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD 
    llUsedsyc = .T.
  ENDIF    

  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD

  *-- Search for the current company+year+Prd
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    =gfBrows('','CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld = lcRpOld
    ELSE
      &lcRpCurFld = laRpRetFld[1]
    ENDIF
  ELSE 
    &lcRpCurFld   = CFisFyear
  ENDIF
  SHOW GET (lcRpCurFld)  
IF lcRpStYear>lcRpEdYear
  WAIT 'Beginning year must be less than or equal to the Ending year' window nowait
  &lcRpCurFld=lcRpOld
  SHOW GET (lcRpCurFld)
  RETURN 
ENDIF
IF lcOGManRep <> 'GLINCST4' AND lcOGManRep <> 'GLINCS13'
  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , lcRpStYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD , lcRpEdYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  
  lcTmpStPr=lcRpStPrid
  lcTmpEdPr=lcRpEdPrid  
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol

  lnRpInvNo = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))                    
  IF lnNoOfCol<=lnRpInvNo
    lnRpColUsd = lnNoOfCol
  ELSE
    WAIT 'The report width not enough for this range of periods' WINDOW NOWAIT
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
  SHOW GET lcRpStPrid
  SHOW GET lcRpEdPrid
  SHOW GET (lcRpCurFld)
RETURN 

*--End of lfvCurYear.
*!*************************************************************
*! Name      : lfvApprov1
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvApprov1
PARAMETER lnRpVarType

lcObjName = VARREAD()
lnNoCol   = INT((lnRpRepWid-lnRpFirIde-lnRpDesWid)/(lnRpColWid+lnRpColIde))
IF lnRpColUsd > lnNoCol
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
  ENDCASE  
  
ENDIF

IF lnRpVarType = 0
  &lcObjName = lcRpOld            && Assign the old value
  _CUROBJ    = _CUROBJ
  SHOW GET (lcObjName)
ENDIF

*--End of lfvApprov1.
*!*************************************************************
*! Name      : lfGetCurYr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetCurYr

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
RETURN lcRetVal

*--End of lfGetCurYr.
*!*************************************************************
*! Name      : lfvSegArr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvSegArr

IF EMPTY(lcRpSeg)
  lcRpSeg = lcRpOld
  SHOW GET lcRpSeg
  RETURN
ENDIF
IF lcRpOld<>lcRpSeg
  DIMENSION laRpSegV[1]
  STORE '' TO laRpSegV,lcRpAcMask
  SHOW GET lcRpAcMask
ENDIF

*--End of lfvSegArr.
*!*************************************************************
*! Name      : lfGetSegLs
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : Get segment information and return the @M picture.
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetSegLs

*--Call the function that let laSegInf hold the information
DIMENSION laSegInf[1,2]
=lfGetActInf()
lcSegPic = ''
FOR lnSegCount = 2 TO ALEN(laSegInf,1)
  lcSegPic = lcSegPic +IIF(EMPTY(lcSegPic),'',',')+  ALLTRIM(laSegInf[lnSegCount,2]) 
ENDFOR
lcSegPic = IIF(ALEN(laSegInf,1)>2,'@M '+lcSegPic ,'@M '+lcSegPic+',' )
RETURN lcSegPic

*--End of lfGetSegLs.
*!*************************************************************
*! Name      : lfvSelSegV
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION  lfvSelSegV
DIMENSION laSelSegV[1],laSegInf[1,2]

lcSetProc=SET('PROCEDURE')
SET PROCEDURE TO
=lfGetActInf()
IF ALEN(laSegInf,1)>=2
  IF EMPTY(lcRpSeg)
    lcRpSeg=laSegInf[2,2]
    SHOW GET lcRpSeg
  ENDIF  
ELSE
 lcRpSeg=''
 SET PROCEDURE TO &lcSetProc
 RETURN 
ENDIF
lnSegNo = ASCAN(laSegInf,lcRpSeg)
lnSegNo = ASUBSCRIPT(laSegInf,lnSegNo,1)
lnSegNo = STR(lnSegNo,1)
SELECT cSegValue;
      FROM &gcDataDir.glsegval;
      WHERE CACSSEGNO=lnSegNo;
      INTO ARRAY laSelSegV
DO WHILE .T.      
  =gfMover(@laSelSegV,@laRpSegV)
  IF ALEN(laRpSegV,1)>8
    WAIT 'Maximum number of segment values is 8' window nowait
  ELSE
    EXIT  
  ENDIF
ENDDO 
SET PROCEDURE TO &lcSetProc
IF lcOGManRep= 'GLINCST2'
  lnRpColUsd = IIF(EMPTY(laRpSegV[1]),0,ALEN(laRpSegV,1))  
  IF !EMPTY(laRpSegV[1])
    lnRpColWid = 10
    IF ALEN(laRpSegV,1)<8
       lnRpColWid = MIN(18,(255-(lnRpDesWid+lnRpFirIde)-(lnRpColUsd*lnRpColIde))/lnRpColUsd)
       lnRpRepWid = (lnRpColWid*lnRpColUsd)+lnRpDesWid+lnRpFirIde+(lnRpColUsd*lnRpColIde)
       lnRpRepWid = MIN(lnRpRepWid,255)
    ENDIF  
  ENDIF  
ENDIF  

*--End of lfvSelSegV.
*!*************************************************************
*! Name      : lfvOneYear
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOneYear
PARAMETER lcOtherFld

DECLARE laRpRetFld(2)
lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty
  
  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD

  *--Search for the current company+year+Prd
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    =gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFisFyear
  ENDIF
  SHOW GET (lcRpCurFld)  
  SHOW GET (lcOtherFld)  

IF llUsedBefo       && .F.- this file used by the system
  USE IN FSPRD
ENDIF
SELECT (lcOldAlias)  
RETURN 

*--End of lfvOneYear.
*!*************************************************************
*! Name      : lfvOnePrd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvOnePrd
PARAMETER lcOtherFld

DECLARE laRpRetFld(2)
lcRpCurFld      = VARREAD()
IF lcRpOld = &lcRpCurFld
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
&& Check If year field is empty

  IF !USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD

  lcYearVal = LEFT(lcRpCurFld,6)+'Year'
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    =gfBrows(IIF(EMPTY(&lcOtherFld),[gcAct_comp],[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
    IF EMPTY(laRpRetFld[1])
      &lcRpCurFld         = lcRpOld
    ELSE
      &lcRpCurFld         = laRpRetFld[1]
      &lcOtherFld         = laRpRetFld[2]
    ENDIF
  ELSE 
      &lcRpCurFld         = CFSPPRDID
  ENDIF
  SHOW GET (lcRpCurFld)  
  SHOW GET (lcOtherFld)  

IF llUsedBefo       && .F.- this file used by the system
  USE IN FSPRD
ENDIF
SELECT (lcOldAlias)  
RETURN 

*--End of lfvOnePrd.
*!*************************************************************
*! Name      : lfvBudCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvBudCode
PARAMETER lcOtherFld

lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF
DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD") 
    SELECT 0
    USE &gcDataDir.GLBUDHD 
    llUsedBefo = .T.
  ENDIF
  SELECT GLBUDHD
  SET ORDER TO TAG BdCodYr
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. ) .OR. ;
     (EMPTY(&lcOtherFld) AND !SEEK(&lcRpCurFld)) .OR. ;
     (!EMPTY(&lcOtherFld) AND !SEEK(&lcRpCurFld+&lcOtherFld))
    =gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
    &lcRpCurFld = laRpRetFld[1]
    *** Budget year variable
    &lcOtherFld = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET (lcOtherFld)
  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvBudCode.
*!*************************************************************
*! Name      : lfvBudYear
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvBudYear
PARAMETER lcOtherFld

lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF

DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
laRpRetFld[1] = ''
laRpRetFld[2] = ''

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD") 
    SELECT 0
    USE &gcDataDir.GLBUDHD ORDER TAG BdCodYr
    llUsedBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcOtherFld+ALLTRIM(&lcRpCurFld.)))
 
    =gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
    &lcRpCurFld  = laRpRetFld[2]
    &lcOtherFld  = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET (lcOtherFld)
  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvBudYear.
*!*************************************************************
*! Name      : lfDefBudCo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDefBudCo

*** If it is used by me
llMyUse = .F.
*** Save the old workarea
lcOldAlias = SELECT()

lcBudCode = ' '

IF !USED('GLSETUP')
  llMyUse = .T.
  SELECT 0
  USE &gcDataDir.GLSETUP
ENDIF
SELECT GLSETUP
LOCATE
lcBudCode = GLSETUP.CSETDEBUD
IF llMyUse
  USE IN GLSETUP
ENDIF

*** Select the old workarea
SELECT(lcOldAlias)

RETURN lcBudCode

*--End of lfDefBudCo.
*!*************************************************************
*! Name      : lfvSegVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvSegVal

lcRpCurFld = VARREAD()
IF lcRpOld = &lcRpCurFld 
  RETURN
ENDIF

=lfGetActInf()   
lnSegNo = ASCAN(laSegInf,lcRpSeg)
lnSegNo = ASUBSCRIPT(laSegInf,lnSegNo,1)   
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CSEGVALUE:H="Segment value"'
laRpRetFld[1] = ''

&& Check If year field is empty
IF !EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF !USED("GLSEGVAL") 
    SELECT 0
    USE &gcDataDir.GLSEGVAL ORDER TAG ACSSEGVAL
    llUsedBefo = .T.
  ENDIF
  SELECT GLSEGVAL
  *** Search for the current Segment no.+segment value
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(STR(lnSegNo,1)+&lcRpCurFld.))
    =gfBrows([STR(lnSegNo,1)],'CSEGVALUE',"laRpRetFld",'Segment values ',.F.)
    &lcRpCurFld  = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLSEGVAL
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*--End of lfvSegVal.
*!*************************************************************
*! Name      : lfvPrdRang
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvPrdRang

lcVldPrd = VARREAD()
IF !BETWEEN(PADL(EVAL(lcVldPrd),2,'0'),'01','13')
  WAIT [Invalid period. Period ranges between '01','13'] WINDOW NOWAIT
  _CUROBJ=_CUROBJ
  RETURN
ENDIF

*--End of lfvPrdRang.
*!*************************************************************
*! Name      : lfvPercent
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvPercent

lnRpColUsd = 1+IIF(lnRpPerc= 0,0,1)+IIF(lnRpIdent=0,0,3)
lnRpRepWid = IIF(lnRpIdent=0 .AND. lnRpPerc= 0,80,132)
lnRpColWid = IIF(lnRpIdent=0 .AND. lnRpPerc= 0,18,16)

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

*--End of lfvPercent.
*!*************************************************************
*! Name      : lfInitCol
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfInitCol

llRetVal = .T.
IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr) OR EMPTY(lcRpBudPr)
  =gfModalGen('TRM02221B00000','DIALOG')     
  llRetVal = .F.
ELSE
  lnRpColUsd = 6           
  DIMENSION laRpCol[3,24]
  STORE 0 TO laRpCol
  FOR lnCount = 1 TO 2
    lcColNo = STR(lnCount,1)
    STORE IIF(lnCount = 1 , 'A' , 'B') TO laRpCol[lnCount,1]
    STORE IIF(lnCount = 1 , PADL(IIF(lnRpYOrP = 2 , 'PTD ' , 'YTD ') + lcRpStPr&lcColNo,lnRpColWid),PADL('Budget '+lcrpBudCod,lnRpColWid)) TO laRpCol[lnCount,2]
    STORE IIF(lnCount = 1 , PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+PADL(lcRpBudPr,2,'0')) TO laRpCol[lnCount,3]
    STORE 1                            TO laRpCol[lnCount,10] , laRpCol[lnCount,11]

    *--Replace YTD or PTD
    IF lnCount=1
      STORE lnRpYOrP                   TO laRpCol[lnCount,12]
      STORE lcRpStPr&lcColNo           TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
      STORE lcRpStYr&lcColNo           TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
    ELSE
      STORE lcRpBudCod                 TO laRpCol[lnCount,8]
      STORE lcRpBudYr                  TO laRpCol[lnCount,9]
      IF lnRpYOrP = 2 
        STORE PADL(lcRpBudPr,2,'0')    TO laRpCol[lnCount,10],laRpCol[lnCount,11]
      ELSE
        STORE '01'                     TO laRpCol[lnCount,10]
        STORE PADL(lcRpBudPr,2,'0')    TO laRpCol[lnCount,11]
      ENDIF
    ENDIF  
    STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
  ENDFOR
  STORE 'O'                            TO laRpCol[3,1]
  STORE ' Percentage'                  TO laRpCol[3,2]
  STORE 'Col1 & Col2'                  TO laRpCol[3,3]
  STORE 3                              TO laRpCol[3,19]
  STORE 1                              TO laRpCol[3,20]
  STORE 2                              TO laRpCol[3,21]

  =lfGetActInf()
  lnSegNo  = ASCAN(laSegInf,lcRpSeg)
  lnSegNo  = ASUBSCRIPT(laSegInf,lnSegNo,1)       
  lnSegPos = IIF(lnSegNo > 1 , ATC('-',lcRpSegMsk,lnSegNo-1)+1 , 1)
  lnSegLen = laSegInf[lnSegNo,1]
  lcPRExp  = ''
  IF !EMPTY(laRpSegV[1])
    lcPRExp = [SUBSTR(GLACCHAR.CACCTCODE,]+ALLTRIM(STR(lnSegPos))+','+ALLTRIM(STR(lnSegLen))+;
              [) IN  (]
    FOR lnSegVToAdd = 1 TO ALEN(laRpSegV,1)
      lcPRExp = lcPRExp + IIF(lnSegVToAdd>1,',','')+'"'+laRpSegV[lnSegVToAdd]+'"'
    ENDFOR
    lcPRExp = lcPRExp + IIF(EMPTY(lcPRExp),'',')')
  ENDIF
ENDIF
 
RETURN llRetVal

*--End of lfInitCol.
*!*************************************************************
*! Name      : lfPrepair
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/28/2002
*! Purpose   : 
*!*************************************************************
*! Called from : GLINCSTM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrepair

lnRpColWid = 18
lnRpDesWid = 40
lnRpColUsd = 6

*--End of lfPrepair.

*--bas
*      lcValue = ALLTRIM(EVAL(lcWorkFile+'.CGROUP')) + ALLTRIM(EVAL(lcWorkFile+'.CRowDES'))
*      FOR lnToEnd = lnGoToRec TO lnRecCont
*        IF ALLTRIM(EVAL(lcYtdFile+'.CGROUP')) + ALLTRIM(EVAL(lcYtdFile+'.CRowDES')) == lcValue
*          EXIT
*        ELSE
*           lnRecCopy = RECNO()
*           llChckFnd = .T.
*           FOR lnCheck = lnRecCopy TO lnRecCont
*             IF ALLTRIM(EVAL(lcYtdFile+'.CGROUP')) + ALLTRIM(EVAL(lcYtdFile+'.CRowDES')) == lcValue
*               llChckFnd = .F.
*               EXIT
*             ELSE
*               SKIP 1
*             ENDIF
*           ENDFOR
*           IF llChckFnd
*             GOTO lnRecCopy IN (lcYtdFile)
*             SCATTER MEMVAR MEMO
*             SKIP 1
*             SELECT (lcWorkFile)
*             INSERT BEFORE BLANK
*             GATHER MEMVAR MEMO
*             SELECT (lcYtdFile)
*          ELSE
*            EXIT
*          ENDIF
*        ENDIF
*      ENDFOR
*--bas