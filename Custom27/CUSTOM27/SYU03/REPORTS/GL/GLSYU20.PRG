*:************************************************************************
*:
*: Procedure file: GLINCSTA.PRG
*:               : Income statment reports
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Sameh Saiid Ezzat
*:  Last modified: 01/24/2002
*:
*:  Procs & Fncts: 
*:---------- Summarization tool -------------------------------
*:                lfvOkSumz
*:                lfvCanSumz
*:                lfvSumzType
*:                lfwSumzType
*:                lfvSumzBy
*:                lfvDumPost            && Calling the posting PRG
*:---------- Income statment tool -------------------------------
*:                lfIncStat             && Income statement tool
*:                lfColData             && Collect rows information
*:                lfUpdData             && Update rows information
*:                lfInsType
*:                lfRpName
*:                lfDisData
*:                lfDisItem
*:                lfvTypes
*:                lfvArrange
*:                lfGetNewBar           && Return the new colunm postion
*:                lfvArrOk
*:                lfModiArr
*:---------- Rows summation --------------------------------
*:                lfvSum
*:                lfMover
*:                lfMovShow
*:                lfvSrc
*:                lfvTrgt
*:                lfvMovm
*:                lfvCancMov
*:                lfvOkMov
*:-------------   Columns --------------------------------------------
*:                lfColShow             && Control the push bottums
*:                lfwCol                && Check before inter the columns
*:                lfvCol                && Change an existed column
*:                lfvAddCol             && Add new column
*:                lfvRemCol             && Remove column
*:                lfvOkCol              && Confirm column information
*:                lfvCancCol            && Cancel column information
*:                lfvIdnCol             && Control the identations
*:                lfDisCol              && Display columns
*:                lfBigNo               && Return the bigger idn. in column
*:                lfShowColTp           && Show column type in the first time
*:                lfvColType            && Check the columns type
*:                lfvCol1
*:                lfvCol8
*:                lfvCol9
*:                lfvCol10
*:                lfvCol11
*:                lfwCol12
*:                lfvCol12
*:                lfvCol13
*:                lfvCol14
*:                lfvCol15
*:                lfvCol16
*:                lfvCol17
*:                lfvCol18
*:                lfvCol20
*:                lfvCol21
*:                lfvCol23
*:                lfSaveArr             && Save the rows & columns information
*:                lfvCancAll            && Cancel all informations
*:                lfvOkAll              && Confirm all entry informations
*:                lfKnowCol
*:                lfGetVal              && Return the column value to the report
*:                lfGetHead             && Return the column header to the report
*:                lfGetActInf           && Get the account code information
*:                lfGetPic              && Return the account picture
*:                lfGetSegDis           && Return the segments description 
*:      Documented   /  /
*:************************************************************************
*

*--mhm2000
llRpAccInd = .T.
lnRpAccInd = 1
lnCountr = 0
*--create temp files for levels
*lcFrstLevl = gfTempName()
*lcScndLevl = gfTempName()
*lcThrdLevl = gfTempName()

*--mhm2000
lnBegTime = 0
lnEndTime = 0
lnTotTime = 0
lcRpRelVar = ''    && Hold the relation condition
*lnRpTotSeg = 0     && Hold the number of segment

*** Create array hold the structuer of temp file
DIMENSION laDbfName[3,4]

*** Create array that hold the summation of main type 
*** that used in the percentage of option
DIMENSION laRpMainTo(ALEN(laRpCol,1))

lcPRExp=''
lnRpIdnNo  = 0     && Hold the identation
*** Hold the needed segment information
lcRpTypeDes = '~sales~Cost of goods sold~Expenses~Other income~Taxes~'
lcRpTotDes  = '~ ~Gross margin~Net income from operation~Net income before taxes~ ~'
lcRpTotExp  = '~ ~S|C~S|C|E~S|C|E|I~ ~'

DIMENSION laSegInf(1,2)  
=lfGetActInf()

*** If the user change any thing in the opion grid or 
*** in the tool (Rows & Columns ) rebuild the data

*IF llOgFltCh OR llRpToolCh
  IF lcOGManRep <> 'GLINCSTA'
    IF !lfInitCol()
      RETURN
    ENDIF
    lcOldAlias = SELECT()
    llUsed = .F.
    IF NOT USED('GLTYPES')
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
*** Check if there is informations in rows or columns
IF EMPTY(laRpMaType) OR EMPTY(laRpCol)  
  WAIT "No columns has been defined " WINDOW NOWAIT
 RETURN
ENDIF  

  *** Initialize the array by 0
  FOR lnCount = 1 TO ALEN(laRpMainTo,1)
    laRpMainTo[lnCount] = 0
  ENDFOR

  *** Create the dbf file structuer
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
  INDEX ON CacctCode  TAG CacctCode


  llRpToolCh = .F.
  lcRpFiles  = "GLACCHAR"+IIF(lcRpExp='.T.','',",GLGRPDT") 
  lnCount    = 0
  lnTotal    = RECCOUNT('GLACCHAR')   
  lcRpExp    =  IIF(lcRpExp='.T.','',;
               'GLACCHAR.CACCTCODE = GLGRPDT.CACCTCODE .AND. &lcRpExp .AND.')+;
               'LEFT(CTypeCode,1) IN ("S","C","E","I","T")'
               
  lcRpExp    =  lcRpExp+IIF(EMPTY(lcPRExp),'',' .AND. '+lcPRExp)
  *** Start trapping the ESC
  =lfTrapEsc(.T.,0)

  
  lnBegTime = SECONDS()    
  
  *** Collect the main and sub account types orderd by the arrangement
  *** provided by the user throguh the tool program linked to this report
  SELECT  &lcRpFields;
    FROM  &lcRpFiles;
    WHERE &lcRpExp .AND. lfRpThermo(lnTotal,'lnCount');
    ORDER BY 1;
    INTO DBF &gcWorkDir.&lcRpTargt
    
  lnEndTime = SECONDS()
  lnTotTime = lnTotTime +(lnEndTime-lnBegTime)
  
  *** End the key trap of ESC
  =lfTrapEsc(.F.)
  
  *** If the user did not ontrrupt the selection of the account type
  IF !llOGEscPrs
    *** Close the thermometer if it's still open
    IF lnCount < lnTotal
      =lfClosThrm(lnTotal,lnCount,.f.,INT(ABS(lnTotal-_tally)/10))
    ENDIF

    *** If no recordes collected inform the user and terminat 
    IF _TALLY = 0        && No records collected
      ** NO recoeds hove been collected
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE

      *** Esatablish the relations between the main temp file and
      *** the temp files represinting the each column
      =lfSetRela()
      
      *** Collect the report data from actual or budget files
      WAIT 'Collecting data from files' WINDOW NOWAIT
      
      =lfSelData()
      IF _TALLY > 0
        *--mhm2000
        lnRpColUsd = 4
        *--mhm2000
        *******************************
        *** added by hesham to determine the columns type
        *** if it is result or indentation .....
        *** Know the type of each column in the report
        DIMENSION laColInf[1,lnRpColUsd]
        =lfGetColInf()
        *************************************
        SELECT (lcWorkFile)
        *--mhm2000

        *C102529,1 Handle the division case. [Begin]
        IF llRpStByDv AND EMPTY(lcRpDivNo)
          DO lpAddAllDv
        ENDIF  
        *C102529,1 Handle the division case. [End]
        
        =lfGetFil()
                
        *C102529,1 Handle the division case. [Begin]
        IF llRpStByDv AND EMPTY(lcRpDivNo)
          SELECT (lcTmpAllDv)
          SET ORDER TO TAG LineNo
        ENDIF  
        *C102529,1 Handle the division case. [End]

        SELECT (lcTempWork)
        LOCATE

        DO gfDispRe WITH EVAL('lcRpForm')
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
*ELSE
      *******************************
      *** added by hesham to determine the columns type
      *** if it is result or indentation .....
*      DIMENSION laColInf[1,lnRpColUsd]
*      =lfGetColInf()
      *************************************
*  SELECT (lcWorkFile)              
*  IF lcOGManRep <> 'GLINCSTA'   
*    =lfInitCol()
*  ENDIF  
*  BROWS NORMAL
*  DO gfDispRe WITH EVAL('lcRpForm')
*ENDIF  

*!************************************************************************
*!
*!      FUNCTION : lfvDumPost
*!
*!************************************************************************
*
FUNCTION lfvDumPost
IF USED(lcTempPost)
  SELECT(lcTempPost)
  ZAP
ENDIF

*B602306,1 MAB 12/13/1998  Bypass program directory with program calling [BEGIN]
PRIVATE lcProgDir, lcSaveAppl
lcProgDir = gcAppHome+gcAct_Appl+'\'

*B602306,1 Save gcWinAppl, and fill it with 'GL' (for callign screens)
lcSaveAppl = gcWinAppl 
gcWinAppl  = 'GL'
*B602306,1 MAB 12/13/1998  Bypass program directory with program calling [END  ]

DO CASE

  *** Case Journal batches
  CASE lnDumPost = '1'
    glFirsTime = .T.
    gcBaseWind = 'AWDGLBPOST'
 
 
    *B602306,1 MAB 12/13/1998  Add lcProgDir to the following line.
    *DO (gcAppHome+'GL.APP') WITH "GLBPOST WITH lcTempPost"
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLBPOST WITH lcTempPost"
    *B602306,1 end 
    
    *DO gpDoProg WITH 'AWDGLBPOST WITH lcTempPost'

  *** Case Single transaction
  CASE lnDumPost = '2'
    glFirsTime = .T.
    gcBaseWind = 'AWDGLTPOST'

    *B602306,1 MAB 12/13/1998  Add lcProgDir to the following line.
    *DO (gcAppHome+'GL.APP') WITH "GLTPOST WITH lcTempPost"
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLTPOST WITH lcTempPost"
    *B602306,1 end 
        
  *** Case Begining balance baches
  CASE lnDumPost = '3'
     glFirsTime = .T.
    gcBaseWind = 'AWDGLBGPST'

    *B602306,1 MAB 12/13/1998  Add lcProgDir to the following line.
    *DO (gcAppHome+'GL.APP') WITH "GLBGPST WITH lcTempPost"
    DO (gcAppHome+'GL.APP') WITH lcProgDir + "GLBGPST WITH lcTempPost"
    *B602306,1 end
ENDCASE

*B602306,1 Restore gcWinAppl
gcWinAppl  = lcSaveAppl
*B602306,1 MAB 12/13/1998  Bypass program directory with program calling [END  ]

*!************************************************************************
*!
*!      FUNCTION : lfvGrpCode
*!
*!************************************************************************
*
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

*MAB
IF !llUsedBef AND (ASCAN(laSelFile,'GLGRPHD') = 0)
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

*E301077,72 Close file if it's not in OG open file array [Begin
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
*E301077,72 Close file if it's not in OG open file array [End

IF !EMPTY(ALIAS())
  SET RELATION TO
ENDIF  
*** Close and remove all tempry files
IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  
  *C102529,1 Correct extension name. [Begin]
  *ERASE (gcWorkDir+lcRpTargt+'.BDF')
  ERASE (gcWorkDir+lcRpTargt+'.DBF')
  *C102529,1 Correct extension name. [End]

  ERASE (gcWorkDir+lcRpTargt+'.FPT')

ENDIF

IF USED(lcTempFile)
  USE IN ALIAS(lcTempFile)
  
  *C102529,1 Correct extension name. [Begin]
  *ERASE (gcWorkDir+lcTempFile+'.BDF')
  ERASE (gcWorkDir+lcTempFile+'.DBF')
  *C102529,1 Correct extension name. [End]

  ERASE (gcWorkDir+lcTempFile+'.CDX')
  ERASE (gcWorkDir+lcTempFile+'.FPT')
ENDIF

IF USED(lcTempPost)
  USE IN ALIAS(lcTempPost)

  *C102529,1 Correct extension name. [Begin]
  *ERASE (gcWorkDir+lcTempPost+'.BDF')
  ERASE (gcWorkDir+lcTempPost+'.DBF')
  *C102529,1 Correct extension name. [End]

  ERASE (gcWorkDir+lcTempPost+'.CDX')
  ERASE (gcWorkDir+lcTempPost+'.FPT')
ENDIF

IF USED(lcWorkFile)
  USE IN ALIAS(lcWorkFile)

  *C102529,1 Correct extension name. [Begin]
  *ERASE (gcWorkDir+lcWorkFile+'.BDF')
  ERASE (gcWorkDir+lcWorkFile+'.DBF')
  *C102529,1 Correct extension name. [End]

  ERASE (gcWorkDir+lcWorkFile+'.FPT')
ENDIF

*C102529,1 Erase the Temp Work file. [Begin]
IF USED(lcTempWork)
  USE IN (lcTempWork)
  ERASE (gcWorkDir+lcTempWork+'.DBF')
  ERASE (gcWorkDir+lcTempWork+'.CDX')
  ERASE (gcWorkDir+lcTempWork+'.FPT')
ENDIF

IF USED(lcTmpAllDv)
  USE IN (lcTmpAllDv)
  ERASE (gcWorkDir+lcTmpAllDv+'.DBF')
  ERASE (gcWorkDir+lcTmpAllDv+'.CDX')
  ERASE (gcWorkDir+lcTmpAllDv+'.FPT')
ENDIF
*C102529,1 Erase the Temp Work file. [End]

DIMENSION laRpMaType(1,6),laRpSType(1,6),laRpCType(1,6),laRpEType(1,6);
          ,laRpIType(1,6),laRpTType(1,6)
laRpMaType[1,1] = ''

DIMENSION laRpCol(1,24)     && Column information
DIMENSION laTempCol(1,23)   && Temp Column

DIMENSION laDbfName(1),laRpMainTo[1,1],laRpSeg(1,2)
DIMENSION laRpColInf(47,2)

*!************************************************************************
*!
*!      FUNCTION : lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

*!************************************************************************
*!
*!      FUNCTION : lfvApprove
*!
*!************************************************************************
*
FUNCTION lfvApprove
PARAMETER lnRpVarType

lcObjName = VARREAD()
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
  &lcObjName = lcRpOld            && Assign the old value
  _CUROBJ    = _CUROBJ
  SHOW GET (lcObjName)
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

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF !USED('SYCACCOD')
*  USE &gcSysHome..SYCACCOD
*ENDIF
*SELECT SYCACCOD
IF !USED('ACCOD')
  USE &gcDataDir.ACCOD
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 [BEGIN] OPEN WITHOUT ORDER
*SET ORDER TO TAG COMPID
*SEEK gcAct_Comp
*SET ORDER TO TAG COMPID
*SEEK gcAct_Comp
GO TOP
*E300789,7 [END..]
*GO TOP
lnTotSegSz = NACSSEGSZ 
lnTotSeg   = NACSNOSEG
lnFirsPos  = (59 - (lnTotSegSz + (lnTotSeg * 4)))/2

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*SELECT NACSSIZE,CACSSHDES,.T. ,0;
*  FROM SYCACCOD ;
*  WHERE NACSSEGSZ < 1 AND CCOMP_ID=gcAct_Comp;
*  INTO ARRAY laRpSeg
*E300789,7 [BEGIN]
*SELECT NACSSIZE,CACSSHDES,.T. ,0;
  FROM ACCOD ;
  WHERE NACSSEGSZ < 1 AND CCOMP_ID=gcAct_Comp;
  INTO ARRAY laRpSeg
SELECT NACSSIZE,CACSSHDES,.T. ,0;
  FROM ACCOD ;
  WHERE NACSSEGSZ < 1 ;
  INTO ARRAY laRpSeg
*E300789,7 [END..]    
*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

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

*E300683,6 Call .SPR from the REPORTS directory
*DO GLSUMZBY.SPR
DO (gcRepHome + gcAct_Appl + '\GLSUMZBY.SPR')   
*E300683,6 end
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

lnRpSumzBy = lnTemSumzBy

lnRpSeg1 = lnSeg1
lnRpSeg2 = lnSeg2
lnRpSeg3 = lnSeg3
lnRpSeg4 = lnSeg4
lnRpSeg5 = lnSeg5
lnRpSeg6 = lnSeg6

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

IF LEFT(lcFieldVal,1) $ 'ESCIT'
  
  IF lnArrType = 1
    lcFieldVal = LEFT(lcFieldVal,1)
    RETURN (ALLTRIM(STR(ASUBSCRIPT(laRpMaType,ASCAN(laRpMaType,lcFieldVal),1))))
  ELSE
    lcRpArrName = 'laRp'+LEFT(lcFieldVal,1)+'Type'
    
    *B601845,4 Change this line to pad the returned value with '0' to the left
    *to fix the order of the file [Begin]
    *RETURN (ALLTRIM(STR(ASUBSCRIPT(&lcRpArrName,ASCAN(&lcRpArrName,lcFieldVal),1))))
    RETURN PADL((ALLTRIM(STR(ASUBSCRIPT(&lcRpArrName ,;
           ASCAN(&lcRpArrName , lcFieldVal) , 1)))) , 2 , '0')
    *B601845,4 Change this line to pad the returned value with '0' [End]
    
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
*Man      
*        lcRpRelVar = laRpCol[lnCount,8]+laRpCol[lnCount,9]+;
      IIF(laRpCol[lnCount,10]<10,'0','')+ALLTRIM(STR(laRpCol[lnCount,10]))+;
                     CacctCode
        lcRpRelVar = laRpCol[lnCount,8]+laRpCol[lnCount,9]+;
                     CacctCode+PADL(laRpCol[lnCount,10],2,'0')
                     
        GO RECNO()
        *B601150,1 Hesham (Start)
        *B601150,1 change the like function to check if the number of seqm. is 1 never add '-' to
        *B601150,1 the picture
         
        *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
        *lnNoSegts = LOOKUP(SYCACCOD.Nacsnoseg,gcAct_Comp,SYCACCOD.NACSSIZE,'COMPID')        
        *lnSegLen = LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO')        
        
        *E300789,6 Account code files opened with out active index, [begin
        GO TOP IN ACCOD
        *lnNoSegts = LOOKUP(ACCOD.Nacsnoseg , gcAct_Comp , ACCOD.NACSSIZE ,;
        *                   'COMPID')        
        lnNoSegts = ACCOD.Nacsnoseg
        
        *lnSegLen  = LOOKUP(ACCOD.NACSSIZE , gcAct_Comp + '1' ,;
        *                   ACCOD.NACSSIZE , 'ACCSEGNO')        
        *B602836,1 Get the length of the first segment.
        *lnSegLen  = ACCOD.NACSSIZE
        lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                           ACCOD.NACSSIZE , 'ACCSEGNO')
        *B602836,1 end

        
        *E300789,6 Account code files opened with out active index, [end

        *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
        
        IF LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))         
*         IF LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode))
        *B601150,1 Hesham (Start)         
          SELECT GLBUDDT
*          SUM REST Namount WHILE ;
           lcRpRelVar = CbudCode+CbudYear+CBUDPRD+cacctcode ; 
           AND ;
           VAL(CBUDPRD) <= laRpCol[lnCount,11];
            TO lnOpResult
            *Added By Hesham
* MAN            
*          SUM REST Namount WHILE ;
           GLBUDDT.CbudCode+GLBUDDT.CbudYear+GLBUDDT.CBUDPRD+GLBUDDT.cacctcode <= ;
           laRpCol[lnCount,8]+laRpCol[lnCount,9]+STRTRAN(STR(laRpCol[lnCount,11],2),' ','0')+&lcRpTargt..cacctcode; 
            TO lnOpResult
     
          SUM REST Namount WHILE ;
           GLBUDDT.CbudCode+GLBUDDT.CbudYear+GLBUDDT.cacctcode+GLBUDDT.CBUDPRD <= ;
           laRpCol[lnCount,8]+laRpCol[lnCount,9]+&lcRpTargt..cacctcode+PADL(laRpCol[lnCount,11],2,'0'); 
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
        *B601150,1 Hesham (Start)
        *B601150,1 change the like function to check if the number of seqm. is 1 never add '-' to
        *B601150,1 the picture
          
          *HAYTHAR Change these lines to Use ACCOD instead of
          *SYCACCOD [Begin]
          *lnNoSegts = LOOKUP(SYCACCOD.Nacsnoseg,gcAct_Comp,SYCACCOD.NACSSIZE,'COMPID')        
          *lnSegLen = LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO')        
          *E300789,6 Account code files opened with out active index, [begin
          GO TOP IN ACCOD
          *lnNoSegts = LOOKUP(ACCOD.Nacsnoseg , gcAct_Comp ,;
          *                   ACCOD.NACSSIZE , 'COMPID')
          lnNoSegts = ACCOD.Nacsnoseg
          
          *lnSegLen  = LOOKUP(ACCOD.NACSSIZE , gcAct_Comp + '1' ,;
          *                   ACCOD.NACSSIZE , 'ACCSEGNO')
          *B602836,1 Get the length of the first segment.
          *lnSegLen  = ACCOD.NACSSIZE
          lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                           ACCOD.NACSSIZE , 'ACCSEGNO')
          *B602836,1 end
          
          *E300789,6 Account code files opened with out active index, [end
          *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
          IF LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))         
*         IF LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode))
        *B601150,1 Hesham (Start)         
            lnOpResult = NACBOPBAL + NACBPTDDR - NACBPTDCR
            *Added By Hesham
            lnOpResult = IIF(LEFT(&lcRpTargt..CTYPECODE,1) $ 'SI',-lnOpResult,lnOpResult)
          ELSE
            lnOpResult = 0
          ENDIF
        ELSE
          lcRpRelVar = CacctCode+laRpCol[lnCount,16]+laRpCol[lnCount,15]
          GO RECNO()
          
          IF lnDumPost <> '0'
            SELECT (lcTempPost)
            IF EOF()
              SELECT GLACBALS
            ENDIF
          ELSE
            SELECT GLACBALS
          ENDIF 
        *B601150,1 Hesham (Start)
        *B601150,1 change the like function to check if the number of seqm. is 1 never add '-' to
        *B601150,1 the picture
          
          *HAYTHAR Change these lines to Use ACCOD instead of
          *SYCACCOD [Begin]
          *lnNoSegts = LOOKUP(SYCACCOD.Nacsnoseg,gcAct_Comp,SYCACCOD.NACSSIZE,'COMPID')        
          *lnSegLen = LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO')        

          *E300789,6 Account code files opened with out active index, [begin
          GO TOP IN ACCOD
          *lnNoSegts = LOOKUP(ACCOD.Nacsnoseg , gcAct_Comp ,;
          *                   ACCOD.NACSSIZE , 'COMPID')
          
          *lnSegLen  = LOOKUP(ACCOD.NACSSIZE , gcAct_Comp + '1' ,;
          *                   ACCOD.NACSSIZE , 'ACCSEGNO')
          lnNoSegts = ACCOD.Nacsnoseg
          *B602836,1 Get the length of the first segment.
          *lnSegLen  = ACCOD.NACSSIZE
          lnSegLen  = LOOKUP(ACCOD.NACSSIZE , '1' ,;
                           ACCOD.NACSSIZE , 'ACCSEGNO')
          *B602836,1 end
         

          *E300789,6 Account code files opened with out active index, [END
          *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
          
          IF LIKE(REPL('?',lnSegLen )+IIF(lnNoSegts=1,'','-')+IIF(lnNoSegts >1,STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),'*'),ALLTRIM(cacctcode))         
*         IF LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode))
        *B601150,1 Hesham (Start)         
   *         SUM REST (NACBPTDDR - NACBPTDCR) WHILE;
              lcRpRelVar = CacctCode+CFISFYEAR+CFSPPRDID AND ;                       
              CFISFYEAR <= laRpCol[lnCount,18] AND ;
              CFSPPRDID <= laRpCol[lnCount,17] TO lnOpResult
              *Added By Hesham
         SUM REST (NACBPTDDR - NACBPTDCR) WHILE;
              CacctCode+CFISFYEAR+CFSPPRDID <=;                       
              &lcRpTargt..cAcctCode+laRpCol[lnCount,18]+laRpCol[lnCount,17] TO lnOpResult
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
    IF laRpCol[lnCount,22] = AT(LEFT(CTypeCode,1),'SCEIT')
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
*   lcFieldsUsed=lcFieldsUsed+IIF(lnColCount>1,',','')+'SUM(nCol'+ALLTRIM(STR(lnColCount))+') '+;
                 'AS '+'nCol'+ALLTRIM(STR(lnColCount))
   lcFieldsUsed=lcFieldsUsed+IIF(lnColCount>4,',','')+'SUM('+lcFieldName+') '+;
                 'AS '+lcFieldName
ENDFOR

lcRpSumzBy   = lfGetSumzBy()
lcFieldsUsed=lcRpSumzBy+',CRowDes,CACCTCODE,'+IIF(lnRpSumzBy=1,;
               [LEFT(CTYPECODE,1)+'00' AS CTYPECODE,],'CTYPECODE,');
              +lcFieldsUsed
              
SET RELATION TO

_TALLY = 0

SELECT &lcFieldsUsed,;
       lfMaSuType(CTYPECODE,1) AS 'NMainTy',;
       lfMaSuType(CTYPECODE,2) AS 'NSubTy';
FROM &lcTempFile;
WHERE IIF(llRpExcZe,lfExcZe(),.T.);
GROUP BY 1;
ORDER BY NMAINTY,NSUBTY;
INTO DBF &gcWorkDir.&lcWorkFile.
IF _TALLY > 0
  =lfModiFile()
ENDIF

*!************************************************************************
*!
*!      FUNCTION : lfExcZe
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
*!      FUNCTION : lfModiFile
*!
*!************************************************************************
* 
FUNCTION lfModiFile
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
      ***** ADDED BY HESHAM
      INSERT BEFORE BLANK      
      IF !EMPTY(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),5])
*        REPLACE CGROUP WITH 'LN,ST'
*        INSERT  BLANK      
      ENDIF
      
      IF lnRpSumzBy = 3
        REPLACE CGROUP WITH 'LN,ST'      
        INSERT  BLANK
        REPLACE CRowDES WITH PADL('SubTotal of '+IIF(lnRowPos = 0,'',ALLTRIM(&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])),lnRpDesWid)
        REPLACE CGROUP WITH 'STS'
        =lfSumMain(lcOldVal2)
        *!**** ADDED BY HESHAM
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
          
          *B601845,4 Change this lines to use the variable lcNewVal2 [that hold
          *the account type that we are adding a header record for] to get
          *the description instead of using lcOldVal2 and then taking the
          *description from the next row [Begin]
          *lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          *REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
          lnRowPos = ASCAN(&lcRpArrName , lcNewVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
          *B601845,4 Change this lines [End]
          
          INSERT BLANK
        ELSE
          SKIP 1
          *** Put the Subtype description
          lcRpArrName = 'laRp'+lcOldVal+'Type'
          lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          
          *B601845,4 Change this lines to use the variable lcNewVal2 [that hold
          *the account type that we are adding a header record for] to get
          *the description instead of using lcOldVal2 and then taking the
          *description from the next row [Begin]
          *lnRowPos = ASCAN(&lcRpArrName,lcOldVal2)
          *REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1)+1,2])
          lnRowPos = ASCAN(&lcRpArrName,lcNewVal2)
          REPLACE CRowDES WITH IIF(lnRowPos = 0,'',&lcRpArrName[ASUBSCRIPT(&lcRpArrName,lnRowPos,1),2])
          *B601845,4 Change this lines [End]          
          
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
      ***** ADDED BY HESHAM
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
ENDIF

INSERT  BLANK
REPLACE CGROUP WITH 'LN,MF'      
INSERT  BLANK
REPLACE CRowDES WITH lcRpNetDes,CGROUP WITH 'MF'
=lfSumCol('SITEC',.T.)
SET RELATION TO

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
IF (lnRpSeg1=1 AND lnRpSeg2=1 AND lnRpSeg3=1 AND lnRpSeg4=1 AND lnRpSeg5=1 AND lnRpSeg6=1)
  lcRetVal = GLACCHAR.CACCNLDES
ELSE
  llUsedBr   = .F.
  IF !USED('GLSEGVAL')
    *B603000,1 Use gfSysOpen() to open this file [Start]
    *USE &gcDataDir..GLSEGVAL
    = gfSysOpen(gcDataDir+'GLSEGVAL')
    *B603000,1 Use gfSysOpen() to open this file [End..]
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
    *B603000,1 Use gfSysClose() to close this file [Start]
    *USE IN ALIAS('GLSEGVAL')
    =gfSysClose('GLSEGVAL')
    *B603000,1 Use gfSysClose() to close this file [End..]
  ENDIF
ENDIF
 
SELECT (lcOldAlias)
*B603000,1 Check the physical record befor going to it [Start]
*GO lnOldRecNo

IF BETWEEN(lnOldRecNo,1,RECCOUNT())
  GO lnOldRecNo
ENDIF
*B603000,1 Check the physical record befor going to it [End..]
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

IF lnTypeOfSum         && Sum the main type
  FOR lnColCount = 1 TO ALEN(laRpCol,1)
    IF laRpCol[lnColCount,1] $ 'BA'
      lcColCount= ALLTRIM(STR(lnColCount))
     **B600280,1 Hesham El_Sheltawi 03/23/95
     **B600280,1 CHANGED THE SUMMITION WAY WE REMOVED THE LIKE
     **B600280,1 LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnColCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode)) 
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
     **B600280,1 Hesham El_Sheltawi 03/23/95
     **B600280,1 CHANGED THE SUMMITION WAY WE REMOVED THE LIKE
     **B600280,1 LIKE(REPL('?',LOOKUP(SYCACCOD.NACSSIZE,gcAct_Comp+'1',SYCACCOD.NACSSIZE,'ACCSEGNO'))+'-'+STRTRAN(STRTRAN(laRpCol[lnColCount,24],' ','?'),'*','?'),ALLTRIM(cacctcode))       
     SUM NCOL&lcColCount FOR ctypecode $ lcChekVal TO lnSumm      
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
  USE (gcDataDir+'GLBUDDT')
ENDIF
SELECT GLBUDDT
*MAN
*SET ORDER TO TAG CDYRPRACC
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

***************************************************************************
* This part is the validations controle the tool provided with this report
***************************************************************************
*!************************************************************************
*!
*!      FUNCTION : lfIncStat
*!
*!************************************************************************
*
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
*** Save the environment
PUSH KEY
lcOldProc = SET('PROCEDURE')
SET PROCEDURE TO

=lfGetActInf()


*E300683,6 Call .SPR from the REPORTS directory
*DO GLHOSROW.SPR
DO (gcRepHome + gcAct_Appl + '\GLHOSROW.SPR')   
*E300683,6 end

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
    *Acoumilate the inv.bot. numbers
    lnTypesNo = lnTypesNo + (ALEN(&lcRpArrName.,1)*4)
  ENDFOR
  IF !EMPTY(lcSaveTag)
    SET ORDER TO TAG lcSaveTag
  ENDIF
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfUpdData
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

*!************************************************************************
*!
*!      Function lfInsType
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
laRpMaType[lnRpMPos,5] = lfRpName(lcRpCode,lcRpTotExp)
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
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character

Function lfRpName
PARAMETERS lcRpValue,lcRpVldEnt
RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'SCEIT'))))

*!************************************************************************
*!
*!      FUNCTION lfDisData
*!
*!************************************************************************
* Display Main types and Subtype on screen  Colling from show procedure
FUNCTION lfDisData

* Display the  Main type
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
*** Restore the old array name
lcRpArrName = lcOldArrName

*!************************************************************************
*!
*!      FUNCTION lfDisItem
*!
*!************************************************************************
* Display one Item
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

*!************************************************************************
*!
*!      FUNCTION lfvTypes
*!
*!************************************************************************
* 
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

    *E300683,6 Call .SPR from the REPORTS directory
	*DO glincDes.SPR
	DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
	*E300683,6 end
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
      
      *E300683,6 Call .SPR from the REPORTS directory
	  *DO glincDes.SPR
	  DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
	  *E300683,6 end
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

      *E300683,6 Call .SPR from the REPORTS directory
	  *DO glincDes.SPR
	  DO (gcRepHome + gcAct_Appl + '\glincDes.SPR')   
	  *E300683,6 end
      =lfDisItem(.F.,&lcVarToCahnge,lsTypes,1)
    ENDIF   
ENDCASE
llSubType = .F.
IF llDoneArr
  =lfDisData()
  llDoneArr = .F.
ENDIF
_CUROBJ = _CUROBJ

*!************************************************************************
*!
*!      FUNCTION lfvArrange
*!
*!************************************************************************
* Arrange Main Types or Subtypes
FUNCTION lfvArrange
PARAMETER lnRpArrType,lcPasArrName

IF ALEN(&lcPasArrName,1) > 1
  DEFINE POPUP poArrTypes MARGIN MOVER

  =gfFillPop('poArrTypes',lcPasArrName,2)
  lnRpArrSize = ALEN(&lcPasArrName,1) + 2
  
  *E300683,6 Call .SPR from the REPORTS directory
  *DO GLINCARR.SPR
  DO (gcRepHome + gcAct_Appl + '\GLINCARR.SPR')   
  *E300683,6 end

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

*!************************************************************************
*!
*!      FUNCTION lfvArrOk
*!
*!************************************************************************
*
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

*!*************************************************************************
*!
*!              Function: lfModiArr
*!
*!*************************************************************************
*
FUNCTION lfModiArr
PARAMETERS lcArrName,lcPopName

lnArrRow=ALEN(&lcArrName,1)
lnArrCol=ALEN(&lcArrName,2)
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

*---------------------------------------------------------------------
*---------------------- Mover ----------------------------------------
*---------------------------------------------------------------------
*!*************************************************************************
*!
*!              FUNCTION lfvSum
*!
*!*************************************************************************
*
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

*!*************************************************************************
*!
*!           Function: lfMover
*!
*!*************************************************************************
*
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

*E300683,6 Call .SPR from the REPORTS directory
*DO GLMOVER.SPR
DO (gcRepHome + gcAct_Appl + '\GLMOVER.SPR')   
*E300683,6 end

POP KEY

*!*************************************************************************
*!
*!              Function: lfMovShow
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
*MAN Change the name of the function from lfvSource to lfvSrc

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

*!*************************************************************************
*!
*!              Function: lfvTrgt
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
SHOW GETS


*!*************************************************************************
*!
*!              Function: lfvMovm
*!
*!*************************************************************************
*
FUNCTION lfvMovm
*MAN Change the name of the function from lfvMovmnts to lfvMovm
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

*!*************************************************************************
*!
*!              Function: lfvCancMov
*!
*!*************************************************************************
*
FUNCTION lfvCancMov
*MAN Changed the function name from lfvCancel
DECLARE laTarget[lnOldDim,6]
=ACOPY(laOldTarg,laTarget)

*!*************************************************************************
*!
*!              Function: lfvOkMov
*!
*!*************************************************************************
*
FUNCTION lfvOkMov
*MAN Change the name of the function from lfvOk

laSource[lnRpXPos,lnRpYPos+2] = ''
FOR lnCount = 1 To ALEN(laTarget,1)
  IF LEN(laTarget[lnCount,lnCoPos])>0
    laSource[lnRpXPos,lnRpYPos+2] = laSource[lnRpXPos,lnRpYPos+2] +;
                           IIF(EMPTY(laSource[lnRpXPos,lnRpYPos+2]),'','|')+;
                           laTarget[lnCount,lnDesPos]
                          
   ENDIF                          
ENDFOR                        
*-------------------------------------------------------------------*
*-------------------------------------------------------------------*
*---------------------------- COLOMNS ------------------------------*
*-------------------------------------------------------------------*
*-------------------------------------------------------------------*

*!*************************************************************************
*!
*!              Function lfColShow
*!
*!*************************************************************************
*
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

*!*************************************************************************
*!
*!              Function lfwCol
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
*!              Function lfvCol
*!
*!*************************************************************************
*
FUNCTION lfvCol

llEditMode = .T.
DIMENSION laTempCol(ALEN(laRpCol,1),24)
=ACOPY (laRpCol,laTempCol)
lnNewCol = 0
lnRpCol = lsCol
lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                         lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lnRpCol,23])
llFirTim = .T.

*E300683,6 Call .SPR from the REPORTS directory
*DO GLHOSCOL.SPR
DO (gcRepHome + gcAct_Appl + '\GLHOSCOL.SPR')   
*E300683,6 end

SHOW GETS

*!*************************************************************************
*!
*!              Function lfvAddCol
*!
*!*************************************************************************
*
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

*E300683,6 Call .SPR from the REPORTS directory
*DO GLHOSCOL.SPR
DO (gcRepHome + gcAct_Appl + '\GLHOSCOL.SPR')   
*E300683,6 end

SHOW GETS

*!*************************************************************************
*!
*!              Function lfvRemCol
*!
*!*************************************************************************
*
FUNCTION lfvRemCol

IF CNTBAR('poCol') > 0
  lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                          lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lsCol,23])
  lnRpColUsd=lnRpColUsd-IIF(laRpCol[lsCol,1] $ 'AB',MAX(1,IIF(laRpCol[lsCol,23]=1,1,0)+laRpCol[lsCol,7]+1),1)                          
  =ADEL(laRpCol,lsCol)
  IF ALEN(laRpCol,1) > 1
    DIMENSION laRpCol(ALEN(laRpCol,1)-1,24)
  ENDIF
  =lfDisCol()
ENDIF
SHOW GETS

*!*************************************************************************
*!
*!              Function lfvOkCol
*!
*!*************************************************************************
*
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
    lnRpColUsd=lnRpColUsd-IIF(laTempCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laTempCol[lnRpCol,23]=1,1,0)+laTempCol[lnRpCol,7]+1),1)  
  ENDIF
  CLEAR READ
****
  lnRpColUsd=lnRpColUsd+IIF(laRpCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laRpCol[lnRpCol,23]=1,1,0)+laRpCol[lnRpCol,7]+1),1)
ENDIF
=lfDisCol()

*!*************************************************************************
*!
*!          Function lfvCancCol
*!
*!*************************************************************************
*
FUNCTION lfvCancCol

DIMENSION laRpCol(ALEN(laTempCol,1),24)
=ACOPY (laTempCol,laRpCol)
CLEAR READ
=lfDisCol()

*!*************************************************************************
*!
*!          Function lfvIdnCol
*!
*!*************************************************************************
*
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

*!*************************************************************************
*!
*!          Function lfDisCol
*!
*!*************************************************************************
*
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
         
*         IIF(EMPTY(ALLTRIM(laRpCol[lnCount,2])),;
                IIF(EMPTY(ALLTRIM(laRpCol[lnCount,3])),;
                'Col '+ALLTRIM(STR(lnCount));
                ,laRpCol[lnCount,3]),laRpCol[lnCount,2])
  ENDIF                
ENDFOR
SHOW GET lsCol 

*!*************************************************************************
*!
*!           Function lfBigNo
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
*!          Function lfShowColTp
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

*-------------------------------------------------------------------*
*-------------------------------------------------------------------*
*--------- Data Validation for column information ------------------*
*----- Start with 'lfvCol' + the column no in the array ------------*
*-like lfvCol13 ------> the valid function for laRpCol[lnRpCol,13]--*
*-------------------------------------------------------------------*
*-------------------------------------------------------------------*

*!*************************************************************************
*!
*!              Function lfvCol1
*!
*!*************************************************************************
* 
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

*!*************************************************************************
*!
*!           Function lfvCol8
*!
*!*************************************************************************
* 
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
  *** Search for the current company+year+Prd
  *B600368,1 Reham On 06/01/95 Browse even if the year is empty.
  *IF ('?' $ &lcRpCurFld. )
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

*!*************************************************************************
*!
*!           Function lfvCol9
*!
*!*************************************************************************
* 
FUNCTION lfvCol9

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
  *** Search for the current company+year+Prd
  *B600356,1 Move the alltrim from the year value to seek right.
  *IF ('?' $ &lcRpCurFld. .OR. !SEEK(laRpCol[lnRpCol,8]+ALLTRIM(&lcRpCurFld.)))
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

*!*************************************************************************
*!
*!              Function lfvCol10
*!
*!*************************************************************************
* 
FUNCTION lfvCol10

IF !BETWEEN(laRpCol[lnRpCol,10],1,13) OR ;
        IIF(EMPTY(laRpCol[lnRpCol,11]),.F.,laRpCol[lnRpCol,11]<laRpCol[lnRpCol,10])
  _CUROBJ = _CUROBJ
ENDIF
*!*************************************************************************
*!
*!              Function lfvCol11
*!
*!*************************************************************************
* 
FUNCTION lfvCol11

IF !BETWEEN(laRpCol[lnRpCol,11],laRpCol[lnRpCol,10],13)
 _CUROBJ = _CUROBJ
ENDIF

*!*************************************************************************
*!
*!          Function lfwCol12
*!
*!*************************************************************************
* 
FUNCTION lfwCol12

=lfvCol12()

*!*************************************************************************
*!
*!          Function lfvCol12
*!
*!*************************************************************************
* 
FUNCTION lfvCol12

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

*!*************************************************************************
*!
*!          Function lfvCol13
*!
*!*************************************************************************
* 
FUNCTION lfvCol13

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
  IF ('?' $ &lcRpCurFld. )
    *E300789,6 Now Company Id field removed from file
    *=gfBrows([gcAct_comp],'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,14] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,14]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!              Function lfvCol14
*!
*!*************************************************************************
*  Check if current company has this entried period or not
FUNCTION lfvCol14

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
  *E300789,7 [begin
  *IF ('?' $ &lcRpCurFld. .OR.;  
  *  !SEEK(gcAct_comp+ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
  *  =gfBrows([gcAct_comp],'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

  IF ('?' $ &lcRpCurFld. .OR.;  
    !SEEK(ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,13] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,13]
  ENDIF
  *E300789,7 [end

  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!              Function lfvCol15
*!
*!*************************************************************************
* 
FUNCTION lfvCol15

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
  IF ('?' $ &lcRpCurFld. OR EMPTY(laRpCol[lnRpCol,16]))
  *E300789,6 [BEGIN
    *=gfBrows([gcAct_comp],'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
  *E300789,6 [END


    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,16] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,16]
  ELSE
    IF !EMPTY(laRpCol[lnRpCol,17]) AND;
             (laRpCol[lnRpCol,18]<> laRpCol[lnRpCol,16] OR;
              laRpCol[lnRpCol,17] < laRpCol[lnRpCol,15])        
              
      *E300789,6 [BEGIN
      *=gfBrows("FOR Ccomp_id=gcAct_comp AND CFisFyear+CFsppRdid<=;
      *       laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
      *   'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

      =gfBrows("FOR CFisFyear+CFsppRdid<=;
             laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
         'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

      *E300789,6 [END
    
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
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!              Function lfvCol16
*!
*!*************************************************************************
* 
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

  *E300789,6[begin  
  *** Search for the current company+year+Prd
  *IF ('?' $ &lcRpCurFld. .OR.;  
  *  !SEEK(gcAct_comp+ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    
  *  =gfBrows([gcAct_comp],'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

  IF ('?' $ &lcRpCurFld. .OR.;  
    !SEEK(ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    
    =gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

  *E300789,6[end

    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,15] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,15]
  ELSE
    IF !EMPTY(laRpCol[lnRpCol,17]) AND;
       (laRpCol[lnRpCol,18]<> laRpCol[lnRpCol,16] OR;
        laRpCol[lnRpCol,17] < laRpCol[lnRpCol,15])
        
      *E300789,6[begin  
      *=gfBrows("FOR Ccomp_id=gcAct_comp AND CFisFyear+CFsppRdid<=;
      *       laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
      *   'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

      =gfBrows("FOR CFisFyear+CFsppRdid<=;
             laRpCol[lnRpCol,18]+laRpCol[lnRpCol,17]",;
         'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

      *E300789,6[end
    
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
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!          Function lfvCol17
*!
*!*************************************************************************
* 
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
  IF ('?' $ &lcRpCurFld. )
  
    *E300789,6 [Begin
    *=gfBrows("FOR Ccomp_id=gcAct_comp AND CFisFyear = laRpCol[lnRpCol,16];
    *    AND CFsppRdid >= laRpCol[lnRpCol,15]",;
    *'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    =gfBrows("FOR CFisFyear = laRpCol[lnRpCol,16];
        AND CFsppRdid >= laRpCol[lnRpCol,15]",;
    'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    *E300789,6 [End
    
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,18] = laRpRetFld[1]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,18]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!          Function lfvCol18
*!
*!*************************************************************************
* 
FUNCTION lfvCol18

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
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *E300789,6 [Begin
  *** Search for the current company+year+Prd
  *IF ('?' $ &lcRpCurFld  OR;  
  *  !SEEK(gcAct_comp+ALLTRIM(&lcRpCurFld)+laRpCol[lnRpCol,17])) OR;
  *  (laRpCol[lnRpCol,17] <laRpCol[lnRpCol,15] OR ;
  *  laRpCol[lnRpCol,18] <> laRpCol[lnRpCol,16])
    
  *  =gfBrows("FOR Ccomp_id=gcAct_comp AND CFisFyear = laRpCol[lnRpCol,16];
  *      AND CFsppRdid >= laRpCol[lnRpCol,15]",;
  *  'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
    

  IF ('?' $ &lcRpCurFld  OR;  
    !SEEK(ALLTRIM(&lcRpCurFld)+laRpCol[lnRpCol,17])) OR;
    (laRpCol[lnRpCol,17] <laRpCol[lnRpCol,15] OR ;
    laRpCol[lnRpCol,18] <> laRpCol[lnRpCol,16])
    
    =gfBrows("FOR CFisFyear = laRpCol[lnRpCol,16];
        AND CFsppRdid >= laRpCol[lnRpCol,15]",;
    'CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)

    *E300789,6 [End 
    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,17] = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
    SHOW GET laRpCol[lnRpCol,17]
  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!*************************************************************************
*!
*!          Function lfvCol20
*!
*!*************************************************************************
* 
FUNCTION lfvCol20

IF laRpCol[lnRpCol,20] > 0
  IF !BETWEEN(laRpCol[lnRpCol,20],1,lnRpNoCol) OR;
            laRpCol[lnRpCol,20] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,20],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,20],1] <> 'A')
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF  

*!*************************************************************************
*!
*!              Function lfvCol21
*!
*!*************************************************************************
* 
FUNCTION lfvCol21

IF laRpCol[lnRpCol,21] > 0
IF !BETWEEN(laRpCol[lnRpCol,21],0,lnRpNoCol) OR;
            laRpCol[lnRpCol,21] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,21],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,21],1] <> 'A')
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

*!*************************************************************************
*!
*!              Function lfvCol23
*!
*!*************************************************************************
* 
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

*---------------------------------------------------------------------
*---------------------------------------------------------------------
*------------ This part for control Ok or Cancel in the --------------
*------------------------ Main Screen --------------------------------
*---------------------------------------------------------------------
*---------------------------------------------------------------------

*!*************************************************************************
*!
*!              Function lfSaveArr
*!
*!*************************************************************************
* Save all arrays information for retreving if push cancel
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

*!*************************************************************************
*!
*!              Function lfvCancAll
*!
*!*************************************************************************
* Cancel all information that was added
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
lnRpColUsd=lnTpColUsd

*!*************************************************************************
*!
*!              Function lfvOkAll
*!
*!*************************************************************************
* Save all information that was added
FUNCTION lfvOkAll

llRpToolCh = .T.

*!*************************************************************************
*!
*!              Function lfGetColInf
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
*!          Function lfGetVal
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
*WAIT STR(LEN(lcRetVal)) WINDOW NOWAIT
RETURN PADL(ALLTRIM(lcRetVal),lnRpColWid)

*IIF(LIKE(lcRpTemPic,STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis)),;
            STR(lnRetVal/lnRpRound,lnRpColWid,lnRpColDis),;
            STRTRAN(lcRpTemPic,'?','*'))            

*!*************************************************************************
*!
*!              Function lfGetHead
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
*!              Function lfGetActInf
*!
*!*************************************************************************
* 
FUNCTION lfGetActInf

lcOldAlias = SELECT()

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF !USED('SYCACCOD')
*  SELECT 0
*  USE (gcSysHome+"SYCACCOD")
*ENDIF
*SELECT SYCACCOD
IF !USED('ACCOD')
  SELECT 0
  USE (gcDataDir+"ACCOD")
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 [BEGIN] OPEN WITHOUT ORDER
*SET ORDER TO TAG COMPID
*SEEK gcAct_Comp
GO TOP
*E300789,7 [END..]
lcSegMask    = CACSMASK 
lnRpTotSeg   = NACSNOSEG
DIMENSION laSegInf[1,2]
STORE '' TO laSegInf

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*SELECT NACSSIZE,CACSSHDES;
*FROM &gcSysHome.SYCACCOD ;
*  WHERE NACSSEGSZ < 1 AND CCOMP_ID=gcAct_Comp;
*  INTO ARRAY laSegInf
*E300789,7 [BEGIN]
*SELECT NACSSIZE,CACSSHDES;
FROM &gcDataDir.ACCOD ;
  WHERE NACSSEGSZ < 1 AND CCOMP_ID=gcAct_Comp;
  INTO ARRAY laSegInf
SELECT NACSSIZE,CACSSHDES;
FROM &gcDataDir.ACCOD ;
  WHERE NACSSEGSZ < 1 ;
  INTO ARRAY laSegInf  
*E300789,7 [END..]
*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]

SELECT (lcOldAlias)
RETURN lcSegMask

*!*************************************************************************
*!
*!          Function lfGetPic
*!
*!*************************************************************************
* 
FUNCTION lfGetPic

RETURN ;
       SUBSTR(ALLTRIM(lcRpSegMsk),AT('-',lcRpSegMsk,1)+1,24)

*!*************************************************************************
*!
*!          Function lfGetSegDis
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
*!          Function lfDesWidth
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
*!          Function lfRowDes
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
*!              Function lfRepShow
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
*!              Function lfZoomWin
*!
*!*************************************************************************
* 
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

*!*************************************************************************
*!
*!              Function lfUpDaCol
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
  *E300683,6 Call .SPR from the REPORTS directory
  *DO GLUPDATE.SPR
  DO (gcRepHome + gcAct_Appl + '\GLUPDATE.SPR')   
  *E300683,6 end
ENDIF

*!*************************************************************************
*!
*!              Function lfwUpDaCol
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
*!              Function lfvCangeTy
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
*!              Function lfvOkUpd
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
  WAIT 'You have to enter the column information' WINDOW NOWAIT 
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
*!              Function lfGetCurPr
*!
*!*************************************************************************
* 
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
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("sycfishd") 
  *  SELECT 0
  *  USE &gcSysHome.sycfishd 
  *  llUsedsyc = .T.
  *ENDIF  
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD 
    llUsedsyc = .T.
  ENDIF  
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ENDIF
  *SELECT SYCFSPRD
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  lcYearVal = LEFT(lcRpCurFld,6)+'Year'
  *** Search for the current company+year+Prd
  *E300789,7 [BEGIN]
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcPrnt_Cmp+&lcYearVal+&lcRpCurFld)
  *  =gfBrows(IIF(EMPTY(&lcYearVal),[gcPrnt_Cmp],[gcPrnt_Cmp+&lcYearVal]),'CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    =gfBrows(IIF(EMPTY(&lcYearVal),'',[&lcYearVal]),'CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)
  *E300789,7 [BEGIN]

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
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *lnBegYrPrd = LOOKUP(SYCFISHD.CFISNOPRD,gcPrnt_Cmp+lcRpStYear,SYCFISHD.CCOMP_ID,'COMPFYEAR')
  *lnENDYrPrd = LOOKUP(SYCFISHD.CFISNOPRD,gcPrnt_Cmp+lcRpEdYear,SYCFISHD.CCOMP_ID,'COMPFYEAR')

  *E300789,7 [BEGIN]
  *lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpStYear ,;
                      FISHD.CCOMP_ID , 'COMPFYEAR')
  *lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpEdYear ,;
                      FISHD.CCOMP_ID , 'COMPFYEAR')
  

  *B-803389,1 Ramy [Begin]

  *lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpStYear ,'COMPFYEAR')
  *lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpEdYear , 'COMPFYEAR')

  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpStYear , FISHD.CFISFYEAR ,'COMPFYEAR')
  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpEdYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  *B-803389,1 Ramy [End]

  
  *E300789,7 [END..]                    

  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  lcTmpStPr=lcRpStPrid
  lcTmpEdPr=lcRpEdPrid  
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  *E300789,7 [BEGIN]
  *=SEEK(gcPrnt_Cmp+lcRpStYear+lcRpStPrid)
  *COUNT  REST WHILE CCOMP_ID+cFisFYear+cFspPrdid <= gcPrnt_Cmp+;
                 lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol
  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol
  *E300789,7 [END..]               

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
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN sycfishd
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
    *USE IN SYCFSPRD
    USE IN FSPRD
    *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
    
  ENDIF
  SELECT (lcOldAlias)  
  SHOW GET lcRpStPrid
  SHOW GET lcRpEdPrid
  SHOW GET (lcRpCurFld)
RETURN 

*!*************************************************************************
*!
*!              Function lfvCurYear
*!
*!*************************************************************************
* 
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
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("sycfishd") 
  *  SELECT 0
  *  USE &gcSysHome.sycfishd 
  *  llUsedsyc = .T.
  *ENDIF    
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD 
    llUsedsyc = .T.
  ENDIF    
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ENDIF
  *SELECT SYCFSPRD
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  *** Search for the current company+year+Prd
  *E300789,7 [BEGIN]
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcPrnt_Cmp+&lcRpCurFld)
  *  =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    =gfBrows('','CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
  *E300789,7 [END..]
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
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *lnBegYrPrd = LOOKUP(SYCFISHD.CFISNOPRD,gcPrnt_Cmp+lcRpStYear,SYCFISHD.CCOMP_ID,'COMPFYEAR')
  *lnENDYrPrd = LOOKUP(SYCFISHD.CFISNOPRD,gcPrnt_Cmp+lcRpEdYear,SYCFISHD.CCOMP_ID,'COMPFYEAR')
  *E300789,7 [BEGIN]
  *lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpStYear ,;
                      FISHD.CCOMP_ID , 'COMPFYEAR')
  
  *lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD , gcPrnt_Cmp + lcRpEdYear ,;
                      FISHD.CCOMP_ID , 'COMPFYEAR')
  

  *B-803389,1 Ramy [Begin]
  *lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , lcRpStYear , 'COMPFYEAR')
  *lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD ,  lcRpEdYear, 'COMPFYEAR')

  lnBegYrPrd = LOOKUP(FISHD.CFISNOPRD , lcRpStYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  lnENDYrPrd = LOOKUP(FISHD.CFISNOPRD , lcRpEdYear , FISHD.CFISFYEAR , 'COMPFYEAR')
  *B-803389,1 Ramy [End]
  *E300789,7 [END..]
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  lcTmpStPr=lcRpStPrid
  lcTmpEdPr=lcRpEdPrid  
  lcRpStPrid = IIF(lnBegYrPrd<lcRpStPrid,'01',lcRpStPrid)
  lcRpEdPrid = IIF(lnEndYrPrd<lcRpEdPrid,'01',lcRpEdPrid)

  *E300789,7 [BEGIN]
  *=SEEK(gcPrnt_Cmp+lcRpStYear+lcRpStPrid)
  *COUNT  REST WHILE CCOMP_ID+cFisFYear+cFspPrdid <= gcPrnt_Cmp+;
                 lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol
  =SEEK(lcRpStYear+lcRpStPrid)
  COUNT  REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid  TO  lnNoOfCol
  *E300789,7 [END..]

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
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN sycfishd
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
 IF llUsedBefo       && .F.- this file used by the system
   
   *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
   *USE IN SYCFSPRD
   USE IN FSPRD
   *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
   
 ENDIF
 SELECT (lcOldAlias)  
  SHOW GET lcRpStPrid
  SHOW GET lcRpEdPrid
  SHOW GET (lcRpCurFld)
RETURN 

*!************************************************************************
*!
*!      FUNCTION lfvApprov1
*!
*!************************************************************************
*
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

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
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

*!************************************************************************
*!
*!      FUNCTION lfvSegArr
*!
*!************************************************************************
*
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

*!*************************************************************************
*!
*!              Function lfGetSegLs
*!
*!*************************************************************************
* Get segment information and return the @M picture
FUNCTION lfGetSegLs

*** Call the function that let laSegInf hold the information
DIMENSION laSegInf[1,2]
=lfGetActInf()
lcSegPic = ''
FOR lnSegCount = 2 TO ALEN(laSegInf,1)
  lcSegPic = lcSegPic +IIF(EMPTY(lcSegPic),'',',')+  ALLTRIM(laSegInf[lnSegCount,2]) 
ENDFOR
lcSegPic = IIF(ALEN(laSegInf,1)>2,'@M '+lcSegPic ,'@M '+lcSegPic+',' )
RETURN lcSegPic

*!************************************************************************
*!
*!      FUNCTION lfvSelSegV
*!
*!************************************************************************
*
******** MULTI SEGMENT VALUES
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
 *B602307,1 WMA [Begin]
 SET PROCEDURE TO &lcSetProc
 *B602307,1 WMA [End]
 RETURN 
ENDIF
lnSegNo=ASCAN(laSegInf,lcRpSeg)
lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)
lnSegNo=STR(lnSegNo,1)
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
    **E700069,1 HESHAM EL_SHELTAWI MADE THE COLUMN WIDTH AND REPORT WIDTH VARIABLE 
    **E700069,1 ACCORDING TO THE NO. OF SEGMENTS SELECTED BY THE USER  
    lnRpColWid = 10
    IF ALEN(laRpSegV,1)<8
       lnRpColWid = MIN(18,(255-(lnRpDesWid+lnRpFirIde)-(lnRpColUsd*lnRpColIde))/lnRpColUsd)
       lnRpRepWid = (lnRpColWid*lnRpColUsd)+lnRpDesWid+lnRpFirIde+(lnRpColUsd*lnRpColIde)
       lnRpRepWid = MIN(lnRpRepWid,255)
    ENDIF  
    **E700069,1
  ENDIF  
ENDIF  

************************** 
*!*************************************************************************
*!
*!              Function lfvOneYear
*!
*!*************************************************************************
* This function valid the only one year
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
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ENDIF
  *SELECT SYCFSPRD
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  *E300789,6 [Begin
  *** Search for the current company+year+Prd
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcAct_comp+&lcRpCurFld)
  *  =gfBrows([gcAct_comp],'CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)

  IF '?' $ &lcRpCurFld. OR !SEEK(&lcRpCurFld)
    =gfBrows('','CFisFyear,CFSPPRDID',"laRpRetFld",'Fiscal year ',.F.)

  *E300789,6 [end

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
  
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
  *USE IN SYCFSPRD
  USE IN FSPRD
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
  
ENDIF
SELECT (lcOldAlias)  
RETURN 

*!*************************************************************************
*!
*!           Function lfvOnePrd
*!
*!*************************************************************************
* This function valid the only one period
FUNCTION lfvOnePrd
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
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
  
  *IF NOT USED("SYCFSPRD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
  *  llUsedBefo = .T.
  *ENDIF
  *SELECT SYCFSPRD
  IF NOT USED("FSPRD") 
    SELECT 0
    USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
    llUsedBefo = .T.
  ENDIF
  SELECT FSPRD
  
  *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
  
  lcYearVal = LEFT(lcRpCurFld,6)+'Year'

  *E300789,6 [Begin
  *** Search for the current company+year+Prd
  *IF '?' $ &lcRpCurFld. OR !SEEK(gcAct_comp+&lcYearVal+&lcRpCurFld)
  *  =gfBrows(IIF(EMPTY(&lcOtherFld),[gcAct_comp],[gcAct_comp+&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)

  IF '?' $ &lcRpCurFld. OR !SEEK(&lcYearVal+&lcRpCurFld)
    =gfBrows(IIF(EMPTY(&lcOtherFld),[gcAct_comp],[&lcOtherFld]),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal year ',.F.)

  *E300789,6 [End
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
  
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
  *USE IN SYCFSPRD
  USE IN FSPRD
  *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
  
ENDIF
SELECT (lcOldAlias)  
RETURN 

*!*************************************************************************
*!
*!              Function lfvBudCode
*!
*!*************************************************************************
* 
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
  *MAN
  *IF ('?' $ &lcRpCurFld. ) .OR. (TYPE(lcOtherFld) = 'C' .AND. !EMPTY(EVAL(lcOtherFld)) .AND. !SEEK(&lcRpCurFld.))
  *B600344,4 Validate the budget code.
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

*!*************************************************************************
*!
*!              Function lfvBudYear
*!
*!*************************************************************************
* 
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

*!*************************************************************************
*!
*!              Function lfDefBudCo
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
  USE &gcDataDir.GLSETUP
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
*!              Function lfvSegVal
*!
*!*************************************************************************
* 
FUNCTION lfvSegVal

lcRpCurFld      = VARREAD()
IF lcRpOld=&lcRpCurFld 
  RETURN
ENDIF

=lfGetActInf()
lnSegNo=ASCAN(laSegInf,lcRpSeg)
lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)   
DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CSEGVALUE:H="Segment value"'
STORE '' TO laRpRetFld

*lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
  lcOldAlias = SELECT()    && Save the current alias
  llUsedBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLSEGVAL") 
    SELECT 0
    USE &gcDataDir.GLSEGVAL ORDER TAG ACSSEGVAL
    llUsedBefo = .T.
  ENDIF
  SELECT GLSEGVAL
  *** Search for the current Segment no.+segment value
  *B600344,4 Remove the alltrim from the seeking value.
  *IF ('?' $ &lcRpCurFld. .OR. !SEEK(STR(lnSegNo,1)+ALLTRIM(&lcRpCurFld.)))
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(STR(lnSegNo,1)+&lcRpCurFld.))
 
    =gfBrows([STR(lnSegNo,1)],'CSEGVALUE,CSEGLNDES',"laRpRetFld",'Segment values ',.F.)
    &lcRpCurFld  = laRpRetFld[1]
    lcDivName    = laRpRetFld[2]
    
    SHOW GET (lcRpCurFld)
  
  *C102529,1 Get the description. [Begin]
  ELSE
    lcDivName = GLSegVal.cSegLnDes
  *C102529,1 Get the description. [End]

  ENDIF
  IF llUsedBefo       && .F.- this file used by the system
    USE IN GLSEGVAL
  ENDIF
  SELECT (lcOldAlias)
ENDIF

*C102529,1 Set the report form . [Begin]
DO lpSetForm
*C102529,1 Set the report form . [End]
RETURN 
     
*!*************************************************************************
*!
*!              Function lfvPrdRang
*!
*!*************************************************************************
* 
FUNCTION lfvPrdRang

lcVldPrd = VARREAD()
IF !BETWEEN(PADL(EVAL(lcVldPrd),2,'0'),'01','13')
  WAIT [Invalid period. Period ranges between '01','13'] WINDOW NOWAIT
  _CUROBJ=_CUROBJ
  RETURN
ENDIF

*!*************************************************************************
*!
*!              Function lfvPercent
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
*!              Function lfInitCol
*!
*!*************************************************************************
* 
FUNCTION lfInitCol

llRetVal = .T.
DO CASE
  CASE lcOGManRep= 'GLINCST1'
     lcOldAlias = SELECT()    && Save the current alias
     llUsedBefo = .F.        && Check if used before or this the first time
     
     *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
     
     *IF NOT USED("SYCFSPRD") 
     *  SELECT 0
     *  USE &gcSysHome.SYCFSPRD 
     *  llUsedBefo = .T.
     *ENDIF
     *SELECT SYCFSPRD    
     IF NOT USED("FSPRD") 
       SELECT 0
       USE &gcDataDir.FSPRD 
       llUsedBefo = .T.
     ENDIF
     SELECT FSPRD    
     
     *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
     
     SET ORDER TO TAG comfyrprdi
     *E300789,6 [Begin
     *IF SEEK(gcAct_Comp+lcRpStYear+lcRpStPrid)
     IF SEEK(lcRpStYear+lcRpStPrid)
     *E300789,6 [End
       DIMENSION laRpCol[lnRpColUsd,24]
       STORE 0 TO laRpCol
       lnCount = 1     
       *E300789,6 [Begin
       *SCAN REST WHILE CCOMP_ID+cFisFYear+cFspPrdid <= gcAct_Comp+;
       *          lcRpEdYear+lcRpEdPrid 
       SCAN REST WHILE cFisFYear+cFspPrdid <= lcRpEdYear+lcRpEdPrid 
       *E300789,6 [End
         STORE 'A'        TO laRpCol[lnCount,1]
         STORE PADL('PTD '+cFspPrdid,lnRpColWid) TO laRpCol[lnCount,2]
         STORE PADL(cFisFYear,lnRpColWid) TO laRpCol[lnCount,3]       
         STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
         STORE 2          TO laRpCol[lnCount,12]
         STORE cFspPrdid TO laRpCol[lnCount,15],laRpCol[lnCount,17]
         STORE cFisFYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18]
         STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
         lnCount = lnCount + 1
      ENDSCAN   
    ELSE
      =gfModalGen('TRM02221B00000','DIALOG')
      RETURN .F.  
    ENDIF       
   CASE lcOGManRep= 'GLINCST2'
     *B600719,1 Hesham El-Shletawi check if there is no selected segments 
     *B600719,1 then dont dimention the array of the report columns
     IF lnRpColUsd>0
       DIMENSION laRpCol[lnRpColUsd,24]
     ENDIF
     DIMENSION laSegInf[1,2]
     STORE 0 TO laRpCol
     =lfGetActInf()   
     lnSegNo=ASCAN(laSegInf,lcRpSeg)
     lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)   
     STORE 0 TO laRpCol
     IF !EMPTY(laRpSegV[1])
       FOR lnCount = 1 TO ALEN(laRpSegV,1)
         STORE 'A'        TO laRpCol[lnCount,1]
         STORE PADC(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPrid+'/'+;
               RIGHT(lcRpStYear,2),lnRpColWid) TO laRpCol[lnCount,2]
         STORE PADC(lcRpSeg+' '+laRpSegV[lnCount],lnRpColWid) TO laRpCol[lnCount,3]       
         STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
         *** Replace YTD or PTD
         STORE lnRpYOrP   TO laRpCol[lnCount,12]
         STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
         STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
         *** Delete and replace the right segment according with the segment no.
         *** with the value in the segment values
         STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','?') TO laRpCol[lnCount,24] 
         lnSegPos=IIF(lnSegNo>2,ATC('-',laRpCol[lnCount,24],lnSegNo-2)+1,1)
         lnSegLen=laSegInf[lnSegNo,1]
         
         *B602307,1 WMA [Begin]
         *STORE STUFF(laRpCol[lnCount,24],lnSegPos,lnSegLen,laRpSegV[lnCount]) TO laRpCol[lnCount,24]
         STORE STUFF(laRpCol[lnCount,24],lnSegPos,lnSegLen,ALLTRIM(laRpSegV[lnCount])) TO laRpCol[lnCount,24]
         *B602307,1 WMA [End]
       
       ENDFOR
*      ELSE 
        *B600719,1 Hesham El-Shletawi return .f. if there is no selected segments
*        llRetVal = .F.
      ENDIF 
   *CASE lcOGManRep= 'GLINCST3'
   CASE lcOGManRep= 'GLINCST3' OR lcOGManRep='GLSYU20'

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

   CASE lcOGManRep= 'GLINCST4'
     lnCount = 1
     STORE 0 TO laRpCol
     STORE 'A'        TO laRpCol[lnCount,1]
     laRpCol[1,4]=IIF(lnRpIdent=0,0,1)
     laRpCol[1,5]=IIF(lnRpIdent=0,0,1)     
     laRpCol[1,6]=IIF(lnRpIdent=0,0,2)     
     laRpCol[1,7]=IIF(lnRpIdent=0,0,3)     
     STORE PADL(lcRpStPrid+'/'+lcRpStYear,lnRpColWid) TO laRpCol[lnCount,2]
     STORE PADL(lcRpEdPrid+'/'+lcRpEdYear,lnRpColWid) TO laRpCol[lnCount,3]       
     STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
     *** Replace PTD
     STORE 2   TO laRpCol[lnCount,12]
     STORE lcRpStPrid TO laRpCol[lnCount,15]
     STORE lcRpStYear TO laRpCol[lnCount,16]
     STORE lcRpEdPrid TO laRpCol[lnCount,17]
     STORE lcRpEdYear TO laRpCol[lnCount,18]
     IF lnRpPerc <> 0
       STORE 1 TO laRpCol[lnCount,23]
       STORE lnRpPerc TO laRpCol[lnCount,22]
     ELSE
       STORE 0 TO laRpCol[lnCount,23]
     ENDIF
     STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
     
   CASE lcOGManRep= 'GLINCST5' OR lcOGManRep= 'GLINCST6'
     lnRpColUsd=3         
     DIMENSION laRpCol[3,24]
     STORE 0 TO laRpCol
     FOR lnCount = 1 TO IIF(lcOGManRep= 'GLINCST5',2,3)
       lcColNo=STR(lnCount,1)
       STORE 'A'        TO laRpCol[lnCount,1]
       STORE PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPr&lcColNo,lnRpColWid) TO laRpCol[lnCount,2]
       STORE PADL(lcRpStYr&lcColNo,lnRpColWid) TO laRpCol[lnCount,3]       
       STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
       *** Replace YTD or PTD
       STORE lnRpYOrP   TO laRpCol[lnCount,12]
       STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
       STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
       STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
    ENDFOR
      IF lcOGManRep= 'GLINCST5'
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
     
   CASE lcOGManRep= 'GLINCST7' 
     IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr) OR EMPTY(lcRpBudPr)
      =gfModalGen('TRM02221B00000','DIALOG')     
       llRetVal = .F.
     ELSE
       lnRpColUsd=3           
       DIMENSION laRpCol[3,24]
       STORE 0 TO laRpCol
       FOR lnCount = 1 TO 2
         lcColNo=STR(lnCount,1)
         STORE IIF(lnCount=1,'A','B')     TO laRpCol[lnCount,1]
         STORE IIF(lnCount=1,PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPr&lcColNo,lnRpColWid),PADL('Budget '+lcrpBudCod,lnRpColWid)) TO laRpCol[lnCount,2]
*MAN         
*         STORE IIF(lnCount=1,PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+STR(lcRpBudPr,2)) TO laRpCol[lnCount,3]
         STORE IIF(lnCount=1,PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+PADL(lcRpBudPr,2,'0')) TO laRpCol[lnCount,3]
         STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
         *** Replace YTD or PTD
         IF lnCount=1
           STORE lnRpYOrP   TO laRpCol[lnCount,12]
           STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
           STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
         ELSE
           STORE lcRpBudCod   TO laRpCol[lnCount,8]
           STORE lcRpBudYr TO laRpCol[lnCount,9]
           IF lnRpYOrP = 2 
             STORE PADL(lcRpBudPr,2,'0') TO laRpCol[lnCount,10],laRpCol[lnCount,11]
           ELSE
             STORE '01'                  TO laRpCol[lnCount,10]
             STORE PADL(lcRpBudPr,2,'0') TO laRpCol[lnCount,11]
           ENDIF  
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
    CASE lcOGManRep= 'GLINCST8'
      IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr) OR EMPTY(lcRpBudPr)
       =gfModalGen('TRM02221B00000','DIALOG')
       llRetVal = .F.
      ELSE
        lnRpColUsd=3      
        DIMENSION laRpCol[3,24]
        STORE 0 TO laRpCol
        FOR lnCount = 1 TO 3
          lcColNo=STR(lnCount,1)
          STORE IIF(lnCount<3,'A','B')     TO laRpCol[lnCount,1]
          STORE IIF(lnCount<3,PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPr&lcColNo,lnRpColWid),'Budget '+lcrpBudCod) TO laRpCol[lnCount,2]
*MAN          
*         STORE IIF(lnCount<3,PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+STR(lcRpBudPr,2)) TO laRpCol[lnCount,3]
          STORE IIF(lnCount<3,PADL(lcRpStYr&lcColNo,lnRpColWid),lcRpBudYr+'/'+PADL(lcRpBudPr,2,'0')) TO laRpCol[lnCount,3]
          STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
          *** Replace YTD or PTD
          IF lnCount<3
            STORE lnRpYOrP   TO laRpCol[lnCount,12]
            STORE lcRpStPr&lcColNo TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
            STORE lcRpStYr&lcColNo TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
          ELSE
            STORE lcRpBudCod   TO laRpCol[lnCount,8]
            STORE lcRpBudYr TO laRpCol[lnCount,9]
            STORE lcRpBudPr TO laRpCol[lnCount,10],laRpCol[lnCount,11]
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
    CASE lcOGManRep= 'GLINCST9'
      IF EMPTY(lcrpBudCod) OR EMPTY(lcRpBudYr1) OR EMPTY(lcRpBudPr1);
        OR EMPTY(lcrpBudCo2) OR EMPTY(lcRpBudYr2) OR EMPTY(lcRpBudPr2)
        =gfModalGen('TRM02221B00000','DIALOG')
        llRetVal = .F.
      ELSE
        lnRpColUsd=2
        DIMENSION laRpCol[2,24]
        STORE 0 TO laRpCol
        FOR lnCount = 1 TO 2
          lcColNo=STR(lnCount,1)
          STORE 'B'                     TO laRpCol[lnCount,1]
          STORE 'Budget '+lcRpBudCod    TO laRpCol[lnCount,2]
          *** Store the budget year and budget peroid header
*MAN    
*         STORE lcRpBudYr&lcColNo+'/'+STR(lcRpBudPr&lcColNo,2) TO laRpCol[lnCount,3]
          STORE lcRpBudYr&lcColNo+'/'+PADL(lcRpBudPr&lcColNo,2,'0') TO laRpCol[lnCount,3]
          STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
          *** Store the budget year and budget peroid header
          IF lnCount = 1
            STORE lcRpBudCod   TO laRpCol[lnCount,8]
          ELSE
            STORE lcRpBudCod2   TO laRpCol[lnCount,8]
          ENDIF
          STORE lcRpBudYr&lcColNo TO laRpCol[lnCount,9]
          STORE lcRpBudPr&lcColNo TO laRpCol[lnCount,10],laRpCol[lnCount,11]
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
    CASE lcOGManRep= 'GLINCS10'
      DIMENSION laRpCol[1,24]
      lnCount = 1
      STORE 0 TO laRpCol
      STORE 'A'        TO laRpCol[lnCount,1]
      STORE PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPrid,lnRpColWid) TO laRpCol[lnCount,2]
      STORE PADL(lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]
      laRpCol[1,4]=IIF(lnRpIdent=0,0,1)
      laRpCol[1,5]=IIF(lnRpIdent=0,0,1)     
      laRpCol[1,6]=IIF(lnRpIdent=0,0,2)     
      laRpCol[1,7]=IIF(lnRpIdent=0,0,3)                 
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
      
    CASE lcOGManRep= 'GLINCS11'
      DIMENSION laRpCol[1,24]
      lnCount = 1
      STORE 0 TO laRpCol
      STORE 'A'        TO laRpCol[lnCount,1]
      laRpCol[1,4]=IIF(lnRpIdent=0,0,1)
      laRpCol[1,5]=IIF(lnRpIdent=0,0,1)     
      laRpCol[1,6]=IIF(lnRpIdent=0,0,2)     
      laRpCol[1,7]=IIF(lnRpIdent=0,0,3)           
      STORE PADL(lcRpStPrid+'/'+lcRpStYear,lnRpColWid) TO laRpCol[lnCount,2]
      STORE PADL(lcRpEdPrid+'/'+lcRpEdYear,lnRpColWid) TO laRpCol[lnCount,3]       
      STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
      *** Replace PTD
      STORE 2   TO laRpCol[lnCount,12]
      STORE lcRpStPrid TO laRpCol[lnCount,15]
      STORE lcRpStYear TO laRpCol[lnCount,16]
      STORE lcRpEdPrid TO laRpCol[lnCount,17]
      STORE lcRpEdYear TO laRpCol[lnCount,18]
      IF lnRpPerc <> 0
        STORE 1 TO laRpCol[lnCount,23]
        STORE lnRpPerc TO laRpCol[lnCount,22]
      ELSE
        STORE 0 TO laRpCol[lnCount,23]
      ENDIF
      STORE STRTRAN(ALLTRIM(SUBSTR(lcRpSegMsk,ATC('-',lcRpSegMsk)+1)),'#','*') TO laRpCol[lnCount,24]
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
      
   CASE lcOGManRep= 'GLINCS12'
     =lfGetActInf()   
     lnSegNo=ASCAN(laSegInf,lcRpSeg)
     lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)   
      DIMENSION laRpCol[3,24]
      STORE 0 TO laRpCol
      FOR lnCount = 1 TO 3
        STORE IIF(lnCount<3,'A','O')     TO laRpCol[lnCount,1]
        STORE PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPrid+'/'+lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]
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
        STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
     
        *** Replace YTD or PTD
        STORE lnRpYOrP   TO laRpCol[lnCount,12]
        STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
        STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
      ENDFOR
      
    CASE lcOGManRep= 'GLINCS13'
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
        STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
     
        *** Replace YTD or PTD
        STORE 2   TO laRpCol[lnCount,12]
        STORE lcRpStPrid TO laRpCol[lnCount,15]
        STORE lcRpEdPrid TO laRpCol[lnCount,17]
        STORE lcRpStYear TO laRpCol[lnCount,16]
        STORE lcRpEdYear TO laRpCol[lnCount,18]
      ENDFOR
      
    CASE lcOGManRep= 'GLINCS14'
      DIMENSION laRpCol[2,24]
      STORE 0 TO laRpCol
      FOR lnCount = 1 TO 2
        
        STORE 'A'        TO laRpCol[lnCount,1]
        
        *B801652,1 Change this line to fix the title of the (Period to Date)
        *column [Begin]
        *STORE PADL(IIF(lnRpYOrP=2,'PTD ','YTD ')+lcRpStPrid,lnRpColWid) TO laRpCol[lnCount,2]
        STORE PADL(IIF(lnCount = 2 , 'PTD ' , 'YTD ') + lcRpStPrid ,;
                   lnRpColWid) TO laRpCol[lnCount,2]
        *B801652,1 Change this line to fix the title [End]

        
        STORE PADL(lcRpStYear,lnRpColWid) TO laRpCol[lnCount,3]       
        STORE 1          TO laRpCol[lnCount,10],laRpCol[lnCount,11]
     
        *** Replace YTD or PTD
        STORE IIF(lnCount = 1,1,2)   TO laRpCol[lnCount,12]
        STORE lcRpStPrid TO laRpCol[lnCount,15],laRpCol[lnCount,17],laRpCol[lnCount,13]
        STORE lcRpStYear     TO laRpCol[lnCount,16],laRpCol[lnCount,18],laRpCol[lnCount,14]
        STORE 0 TO laRpCol[lnCount,23]
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
 ENDCASE
 
RETURN llRetVal

*!*************************************************************************
*!
*!              Function lfPrepair
*!
*!*************************************************************************
* 
FUNCTION lfPrepair

DO CASE
  CASE lcOGRepID = 'GLINCST1'
    lnRpColUsd=1
  CASE lcOGRepID = 'GLINCST2'
    DIMENSION laRpSegVal[1]
    *** Hold segment information
    DIMENSION laSegInf[1,2]
    lnRpRepWid = 132
    lnRpDesWid = 40
    lnRpColUsd=1
  CASE lcOGRepID = 'GLINCST3'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE lcOGRepID = 'GLINCST4'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE lcOGRepID = 'GLINCST5'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3  
  CASE lcOGRepID = 'GLINCST6'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3      
  CASE lcOGRepID = 'GLINCST7'
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLINCST8'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 3
  CASE lcOGRepID = 'GLINCST9'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE lcOGRepID = 'GLINCS10'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE lcOGRepID = 'GLINCS11'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE lcOGRepID = 'GLINCS12'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE lcOGRepID = 'GLINCS13'
    lnRpRepWid = 132
    lnRpColWid = 18
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE lcOGRepID = 'GLINCS14'
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 65
    lnRpColUsd = 2
    lnRpFirIde = 5
    lnRpColIDe = 5
ENDCASE  

*!*************************************************************************
*!
*!              Function lfPrepair1
*!
*!*************************************************************************
* 
FUNCTION lfPrepair1

DO CASE
  CASE ALLTRIM(UPPER(PROMPT())) == 'INCOME STATEMENT GENERATOR'
    lcOGRepID = 'GLINCSTA'
    lcOGManRep= 'GLINCSTA'   
  CASE ALLTRIM(UPPER(PROMPT())) == 'MULTI PERIOD COMPARISON'
    lcOGRepID = 'GLINCST1'
    lcOGManRep= 'GLINCST1'
    lnRpColUsd=1
  CASE ALLTRIM(UPPER(PROMPT())) == 'MULTI SEGMENT COMPARISON'    
    lcOGRepID = 'GLINCST2'
    lcOGManRep= 'GLINCST2'  
    DIMENSION laRpSegVal[1]
    *** Hold segment information
    DIMENSION laSegInf[1,2]
    lnRpRepWid = 132
    lnRpDesWid = 40
    lnRpColUsd=1
  CASE ALLTRIM(UPPER(PROMPT())) == 'TOTAL COMPANY PTD/YTD'
    lcOGRepID = 'GLINCST3'
    lcOGManRep= 'GLINCST3'  
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE ALLTRIM(UPPER(PROMPT())) == 'TOTAL COMPANY RANGE OF PERIODS'
    lcOGRepID = 'GLINCST4'
    lcOGManRep= 'GLINCST4'  
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 1
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL VS. OTHER PERIOD ACTUAL"
    lcOGRepID = 'GLINCST5'
    lcOGManRep= 'GLINCST5'  
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3  
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL PERIODS COMPARISON"
    lcOGRepID = 'GLINCST6'
    lcOGManRep= 'GLINCST6'  
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3      
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL VS. OTHER PERIOD BUDGET"
    lcOGRepID = 'GLINCST7'
    lcOGManRep= 'GLINCST7'  
    lnRpColWid = 18
    lnRpDesWid = 40
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "ACTUAL COL. VS. ACTUAL AND BUDGET COL."
    lcOGRepID = 'GLINCST8'
    lcOGManRep= 'GLINCST8'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "TWO BUDGET COLUMN COMPARISON"
    lcOGRepID = 'GLINCST9'
    lcOGManRep= 'GLINCST9'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE ALLTRIM(UPPER(PROMPT())) == "SINGLE SEGMENT"
    lcOGRepID = 'GLINCS10'
    lcOGManRep= 'GLINCS10'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE ALLTRIM(UPPER(PROMPT())) == "SINGLE SEGMENT RANGE OF PERIODS"
    lcOGRepID = 'GLINCS11'
    lcOGManRep= 'GLINCS11'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 45
    lnRpColUsd = 2
  CASE ALLTRIM(UPPER(PROMPT())) == "SINGLE SEGMENT VS. REST OF COMPANY PTD/YTD"
    lcOGRepID = 'GLINCS12'
    lcOGManRep= 'GLINCS12'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "SGL. SEG. VS. REST OF CO. RANGE OF PERIODS"
    lcOGRepID = 'GLINCS13'
    lcOGManRep= 'GLINCS13'  
    lnRpRepWid = 132
    lnRpColWid = 18
    lnRpDesWid = 55
    lnRpColUsd = 3
    lnRpFirIde = 5
    lnRpColIDe = 3
  CASE ALLTRIM(UPPER(PROMPT())) == "SINGLE SEGMENT PTD AND YTD"
    lcOGRepID = 'GLINCS14'
    lcOGManRep= 'GLINCS14'  
    lnRpColWid = 18
    lnRpRepWid = 132
    lnRpDesWid = 65
    lnRpColUsd = 2
    lnRpFirIde = 5
    lnRpColIDe = 5
ENDCASE  


***********MHM**************************************************

*!**************************************************************************
*! Name      : lpDivRest
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/27/2001
*! Purpose   : To add the rest of divisions to the temp file.
*!**************************************************************************
*! Example   : DO lpDivRest.
*!**************************************************************************
*
PROCEDURE lpDivRest
PRIVATE lcAlias , lnNumber
lcAlias = ALIAS()

SELECT (lcTempWork)

FOR lnNumber = 1 TO lnLastPage
  *-- get Merchandise Sales
  =lfFrstLevl("S00")

  *-- get Cost of goods sold
  =lfFrstLevl("IC00")

  *-- get General  Expenses                 
  =lfFrstLevl("E00")

  *-- get Other Income
  =lfFrstLevl("I00")

  *-- get Tax
  =lfFrstLevl("T00")

  *-- get Statistical
  =lfFrstLevl("Y00")
ENDFOR  

SELECT (lcAlias)
*-- End of lpDivRest.

*!**************************************************************************
*! Name      : lpAddAllDv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/27/2001
*! Purpose   : To add the division segment level to the temp file.
*!**************************************************************************
*! Example   : DO lpAddAllDv
*!**************************************************************************
*
PROCEDURE lpAddAllDv
PRIVATE lcAlias , llGLSegVal , laTmpAllDv , lnPageNo
lcAlias = ALIAS()
llGLSegVal = .F.

=lfGetActInf()
lnSegNo=ASCAN(laSegInf,lcRpSeg)
lnSegNo=ASUBSCRIPT(laSegInf,lnSegNo,1)   

IF !USED("GLSEGVAL") 
  SELECT 0
  USE &gcDataDir.GLSEGVAL ORDER TAG ACSSEGVAL
  llGLSegVal = .T.
ENDIF
SELECT GLSEGVAL
IF SEEK(STR(lnSegNo,1))
  DIMENSION laTmpAllDv[4,4]

  laTmpAllDv[1,1] = 'cSegValue'
  laTmpAllDv[1,2] = 'C'
  laTmpAllDv[1,3] = 15
  laTmpAllDv[1,4] = 0
  laTmpAllDv[2,1] = 'cSegLnDes'
  laTmpAllDv[2,2] = 'C'
  laTmpAllDv[2,3] = 40
  laTmpAllDv[2,4] = 0  
  laTmpAllDv[3,1] = 'cPageNo'
  laTmpAllDv[3,2] = 'C'
  laTmpAllDv[3,3] = 4
  laTmpAllDv[3,4] = 0
  laTmpAllDv[4,1] = 'cLineNo'
  laTmpAllDv[4,2] = 'C'
  laTmpAllDv[4,3] = 4
  laTmpAllDv[4,4] = 0
  
  CREATE DBF (gcWorkDir+lcTmpAllDv) FROM ARRAY laTmpAllDv
  INDEX ON cLineNo TAG LineNo
  INDEX ON cSegValue TAG cSegValue
  SET ORDER TO TAG cSegValue
  
  lnPageNo = 0    && Page no is incremented every 4 loops.
  SELECT GLSEGVAL
  SCAN REST WHILE cAcssegno + cSegValue = STR(lnSegNo,1)
    m.cSegValue = cSegValue
    m.cSegLnDes = cSegLnDes
    lnPageNo = lnPageNo + 1
    m.cLineNo = ALLTRIM(STR(lnPageNo,4))
    
    *Generic
    *m.cPageNo = ALLTRIM(STR(CEILING(lnPageNo/4),4))
    m.cPageNo = ALLTRIM(STR(CEILING(lnPageNo/5),4))
    
    INSERT INTO (lcTmpAllDv) FROM MEMVAR
    
    *Generic
    *lnLastPage = CEILING(lnPageNo/4)
    lnLastPage = CEILING(lnPageNo/5)
  ENDSCAN
ENDIF

IF llGLSegVal
  USE IN GLSegVal
ENDIF
SELECT (lcAlias)
*-- End of lpAddAllDv.

*!*************************************************************
*! Name      : lfGetFil
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function collect data for the report
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFil()
*!*************************************************************
FUNCTION lfGetFil

=lfCreatTmp()
SELECT (lcWorkFile)
INDEX ON CacctCode  TAG CacctCode

IF NOT USED('CODES')
  SELECT 0
  =gfOpenFile(gcDataDir+'CODES','Codes','SH')
ENDIF

IF !USED('GLGRPHD')
  =gfOpenFile(gcDataDir+'GLGRPHD','grpcode','SH')
ELSE
  SELECT GLGRPHD
  SET ORDER TO TAG grpcode
ENDIF

SELECT CODES

*C102529,1 Handle the division case. [Begin]
IF llRpStByDv AND EMPTY(lcRpDivNo)
  DO lpDivRest
ELSE
  *-- get Merchandise Sales
  =lfFrstLevl("S00")

  *-- get Cost of goods sold
  =lfFrstLevl("IC00")

  *-- get General  Expenses                 
  =lfFrstLevl("E00")

  *-- get Other Income
  =lfFrstLevl("I00")

  *-- get Tax
  =lfFrstLevl("T00")

  *-- get Statistical
  =lfFrstLevl("Y00")
ENDIF
*C102529,1 Handle the division case. [End]
        
SELECT (lcWorkFile)
*--End of lfGetFil
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to create temp File
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp

*--create temp files for levels

*** Create the dbf file structuer to get Work file

*C102529,1 Define array size. [Begin]
*DIMENSION laDbfWork[6,4]
IF llRpStByDv
  IF EMPTY(lcRpDivNo)
    DIMENSION laDbfWork[17,4]
  ELSE
    DIMENSION laDbfWork[8,4]
  ENDIF
ELSE
  DIMENSION laDbfWork[6,4]
ENDIF
*C102529,1 Define array size. [End]

laDbfWork[1,1] = 'Clevel1'
laDbfWork[1,2] = 'C'
laDbfWork[1,3] = 40
laDbfWork[1,4] = 0
  
laDbfWork[2,1] = 'Clevel2'
laDbfWork[2,2] = 'C'
laDbfWork[2,3] = 30
laDbfWork[2,4] = 0
  
laDbfWork[3,1] = 'Clevel3'
laDbfWork[3,2] = 'C'
laDbfWork[3,3] = 40
laDbfWork[3,4] = 0

laDbfWork[4,1] = 'nlevel4'
laDbfWork[4,2] = 'N'
laDbfWork[4,3] = 13
laDbfWork[4,4] = 2

laDbfWork[5,1] = 'Nlevel5'
laDbfWork[5,2] = 'N'
laDbfWork[5,3] = 15
laDbfWork[5,4] = 2

laDbfWork[6,1] = 'NOrder'
laDbfWork[6,2] = 'N'
laDbfWork[6,3] = 4
laDbfWork[6,4] = 0

*C102529,1 Add the field needed for divisions. [Begin]
IF llRpStByDv
  laDbfWork[7,1] = 'nDiv1'
  laDbfWork[7,2] = 'N'
  laDbfWork[7,3] = 13
  laDbfWork[7,4] = 2

  laDbfWork[8,1] = 'nTot1'
  laDbfWork[8,2] = 'N'
  laDbfWork[8,3] = 15
  laDbfWork[8,4] = 2
  
  IF EMPTY(lcRpDivNo)
    laDbfWork[9,1] = 'nDiv2'
    laDbfWork[9,2] = 'N'
    laDbfWork[9,3] = 13
    laDbfWork[9,4] = 2

    laDbfWork[10,1] = 'nTot2'
    laDbfWork[10,2] = 'N'
    laDbfWork[10,3] = 15
    laDbfWork[10,4] = 2  

    laDbfWork[11,1] = 'nDiv3'
    laDbfWork[11,2] = 'N'
    laDbfWork[11,3] = 13
    laDbfWork[11,4] = 2

    laDbfWork[12,1] = 'nTot3'
    laDbfWork[12,2] = 'N'
    laDbfWork[12,3] = 15
    laDbfWork[12,4] = 2  

    laDbfWork[13,1] = 'nDiv4'
    laDbfWork[13,2] = 'N'
    laDbfWork[13,3] = 13
    laDbfWork[13,4] = 2

    laDbfWork[14,1] = 'nTot4'
    laDbfWork[14,2] = 'N'
    laDbfWork[14,3] = 15
    laDbfWork[14,4] = 2  

    laDbfWork[15,1] = 'nDiv5'
    laDbfWork[15,2] = 'N'
    laDbfWork[15,3] = 13
    laDbfWork[15,4] = 2

    laDbfWork[16,1] = 'nTot5'
    laDbfWork[16,2] = 'N'
    laDbfWork[16,3] = 15
    laDbfWork[16,4] = 2  

    laDbfWork[17,1] = 'cPageNo'
    laDbfWork[17,2] = 'C'
    laDbfWork[17,3] = 4
    laDbfWork[17,4] = 0
  ENDIF
ENDIF
*C102529,1 Add the field needed for divisions. [End]

CREAT DBF (gcWorkDir+lcTempWork) FROM ARRAY laDbfWork

*INDEX ON STR(NOrder,4)  TAG NOrder
IF llRpStByDv
  IF EMPTY(lcRpDivNo)
    INDEX ON cPageNo + STR(nOrder,4) TAG NOrder
  ELSE
    INDEX ON STR(nOrder,4) TAG NOrder
  ENDIF
ELSE
  INDEX ON STR(nOrder,4) TAG NOrder
ENDIF

*--End of lfCreatTmp
*!*************************************************************
*! Name      : lfFrstLevl
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to get all records from Codes file that 
*!           : required for Balance Sheet
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFrstLevl()
*!*************************************************************
FUNCTION lfFrstLevl
PARA lcType

SELECT * FROM CODES A  WHERE cFld_name  == "CFINSTAT" AND ; 
        cRltField == 'Y' AND EXISTS (SELECT * FROM CODES B WHERE ;
        cRltd_Vlu == 'IS' AND A.cCode_no=B.cCode_no ) INTO cursor lcFrstLevl

=lfScndLevl(lcType)

*--End of lfFrstLevl
*!*************************************************************
*! Name      : lfScndLevl
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to get Second level 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfScndLevl()
*!*************************************************************
FUNCTION lfScndLevl
PARA lcType

PRIVATE llCHeck,lnTotal,lnReturn
llCHeck  = .F.
lnTotal  = 0
lnReturn = 0 

SELECT * FROM lcFrstLevl A WHERE ALLTRIM(cRltd_Nam) == "NSEQNO" AND ;
         EXISTS (SELECT * FROM lcFrstLevl B WHERE ALLTRIM(cRltd_Vlu );
         == lcType AND A.cCode_no==B.cCode_no ) ORDER BY cRltd_Vlu INTO CURSOR lcScndLevl

SELECT lcScndLevl
LOCATE 
IF EOF()
  RETURN
ENDIF

SELECT lcScndLevl
SCAN
  lnCountr = lnCountr +1        
  =SEEK(cdefcode+ccode_no+'N'+cfld_name,'CODES')
  SELECT (lcTempWork)
  APPEND BLANK
  REPLACE cLevel2 WITH Codes.cDiscrep,;
          cLevel1 WITH lcType ,;
          NOrder WITH lnCountr
          
  *C102529,1 Add Page no to the file. [Begin]
  IF llRpStByDv AND EMPTY(lcRpDivNo)
    REPLACE cPageNo WITH ALLTRIM(STR(lnNumber,4))
  ENDIF    
  *C102529,1 Add Page no to the file. [End]

  lnReturn =lfThrdLevl(lcScndLevl.cCode_No)
  lnTotal = lnTotal +lnReturn
  IF EMPTY(lnReturn) AND llRpExcZe
    llCHeck  = .T.
    DELE
  ENDIF
ENDSCAN

*-End OF lfScndLevl
*!*************************************************************
*! Name      : lfThrdLevl
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to get therd level 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfThrdLevl()
*!*************************************************************
FUNCTION lfThrdLevl
PARA lcGroup

PRIVATE lnSbTotl
STORE 0 TO lnSbTotl

SELECT * FROM lcFrstLevl WHERE ALLTRIM(cCode_no) == lcGroup;
         AND LEFT(Crltd_Nam,5) == "GLGRP" ORDER BY Crltd_nam INTO CURSOR lcThrdLevl

SELECT lcThrdLevl

*C102529,1 we need to handle the division. [Begin]
*SCAN
*  =SEEK(LEFT(UPPER(crltd_vlu),8),'GLGRPHD')
*  lnCountr = lnCountr + 1        
*  SELECT (lcTempWork)
*  
*  lnLevel4 = lfGetAmnt(lcThrdLevl.crltd_vlu)
*  IF (EMPTY(lnlevel4)  AND llRpExcZe)
*    LOOP
*  ENDIF
*  APPEND BLANK
*  REPLACE CLEVEL3 WITH GLGRPHD.cgrpshhed ,;
*          cLevel1 WITH lcType ,;
*          NOrder WITH lnCountr,;
*          nLevel4 WITH lnLevel4
*  lnSbTotl = lnSbTotl + nLevel4
*ENDSCAN

IF llRpStByDv
  IF EMPTY(lcRpDivNo)
    
    *Generic
    *DIMENSION laDivValue[4] , laDivTotal[4]
    DIMENSION laDivValue[5] , laDivTotal[5]

    laDivTotal = 0
    SCAN
      =SEEK(LEFT(UPPER(crltd_vlu),8),'GLGRPHD')
      lnCountr = lnCountr + 1        
      SELECT (lcTempWork)
      laDivValue = 0
      lnLevel4 = lfGetDvAmt(lcThrdLevl.crltd_vlu)
      IF (EMPTY(lnlevel4)  AND llRpExcZe)
        LOOP
      ENDIF
      APPEND BLANK
      REPLACE CLEVEL3 WITH GLGRPHD.cgrpshhed ,;
              cLevel1 WITH lcType ,;
              NOrder WITH lnCountr,;
              cPageNo WITH ALLTRIM(STR(lnNumber,4)) ,;
              nLevel4 WITH lnLevel4 ,;
              nDiv1 WITH laDivValue[1] ,;
              nDiv2 WITH laDivValue[2] ,;
              nDiv3 WITH laDivValue[3] ,;
              nDiv4 WITH laDivValue[4] ,;
              nDiv5 WITH laDivValue[5]
      lnSbTotl = lnSbTotl + nLevel4
    ENDSCAN
  ELSE

    PRIVATE lnDivValue , lnDivTotal
    lnDivTotal = 0
    SCAN
      =SEEK(LEFT(UPPER(crltd_vlu),8),'GLGRPHD')
      lnCountr = lnCountr + 1        
      SELECT (lcTempWork)
      lnDivValue = 0
      lnLevel4 = lfGetOneDv(lcThrdLevl.crltd_vlu)
      IF (EMPTY(lnlevel4)  AND llRpExcZe)
        LOOP
      ENDIF
      APPEND BLANK
      REPLACE CLEVEL3 WITH GLGRPHD.cgrpshhed ,;
              cLevel1 WITH lcType ,;
              NOrder WITH lnCountr,;
              nLevel4 WITH lnLevel4 ,;
              nDiv1 WITH lnDivValue
      lnSbTotl = lnSbTotl + nLevel4
    ENDSCAN

  ENDIF  
ELSE
  SCAN
    =SEEK(LEFT(UPPER(crltd_vlu),8),'GLGRPHD')
    lnCountr = lnCountr + 1        
    SELECT (lcTempWork)  
    lnLevel4 = lfGetAmnt(lcThrdLevl.crltd_vlu)
    IF (EMPTY(lnlevel4)  AND llRpExcZe)
      LOOP
    ENDIF
    APPEND BLANK
    REPLACE CLEVEL3 WITH GLGRPHD.cgrpshhed ,;
            cLevel1 WITH lcType ,;
            NOrder WITH lnCountr,;
            nLevel4 WITH lnLevel4
    lnSbTotl = lnSbTotl + nLevel4
  ENDSCAN
ENDIF  
*C102529,1 we need to handle the division. [End]

*--get totals
SELECT (lcTempWork)
IF !llRpExcZe
  lnCountr = lnCountr +1
  APPEND BLANK
  REPLACE CLEVEL2 WITH "*** " + Codes.cDiscrep,;
          cLevel1 WITH lcType ,;
          NOrder WITH lnCountr,;
          nLevel5 WITH lnSbTotl
  
  *C102529,1 Add the divisions to file. [Begin]
  IF llRpStByDv
    IF EMPTY(lcRpDivNo)    
      REPLACE nTot1 WITH laDivTotal[1] ,;
              nTot2 WITH laDivTotal[2] ,;
              nTot3 WITH laDivTotal[3] ,;
              nTot4 WITH laDivTotal[4] ,;
              nTot5 WITH laDivTotal[5] ,;
              cPageNo WITH ALLTRIM(STR(lnNumber,4))
    ELSE
      REPLACE nTot1 WITH lnDivTotal
    ENDIF
  ENDIF
  *C102529,1 Add the divisions to file. [End]
ELSE
  IF !EMPTY(lnSbTotl)
    lnCountr = lnCountr +1
    APPEND BLANK
    REPLACE CLEVEL2 WITH "*** " + Codes.cDiscrep,;
            cLevel1 WITH lcType ,;
            NOrder WITH lnCountr,;
            nLevel5 WITH lnSbTotl            

    *C102529,1 Add the divisions to file. [Begin]
    IF llRpStByDv
      IF EMPTY(lcRpDivNo)    
        REPLACE nTot1 WITH laDivTotal[1] ,;
                nTot2 WITH laDivTotal[2] ,;
                nTot3 WITH laDivTotal[3] ,;
                nTot4 WITH laDivTotal[4] ,;
                nTot5 WITH laDivTotal[5] ,;
                cPageNo WITH ALLTRIM(STR(lnNumber,4))
      ELSE
        REPLACE nTot1 WITH lnDivTotal
      ENDIF
    ENDIF
    *C102529,1 Add the divisions to file. [End]

  ENDIF
ENDIF

RETURN lnSbTotl

*--End of lfThrdLevl

*!**************************************************************************
*! Name      : lfGetOneDv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/28/2001
*! Purpose   : Function to get Amount for one divisions
*!**************************************************************************
*! Example   : = lfGetOneDv()
*!**************************************************************************
*
FUNCTION lfGetOneDv
PARAMETERS lcGroup
PRIVATE lcAlias , lnTotal
lnTotal = 0
lcAlias = SELECT()

SELECT GLGRPDT
SET ORDER TO TAG Grcodacc
=SEEK(LEFT(UPPER(lcGroup),8))
SCAN REST WHILE cGrpCode = LEFT(UPPER(lcGroup),8)
  =SEEK(cAcctCode,lcWorkFile)
  IF ALLTRIM(SUBSTR(cAcctCode,laSegInf[1,1]+2)) = ALLTRIM(lcRpDivNo)
    lnDivValue = lnDivValue + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
    lnDivTotal = lnDivTotal + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
  ENDIF
  lnTotal = lnTotal + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
ENDSCAN
SELECT (lcAlias)
RETURN lnTotal
*-- End of lfGetDvAmt

*!**************************************************************************
*! Name      : lfGetDvAmt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/28/2001
*! Purpose   : Function to get Amount for divisions
*!**************************************************************************
*! Example   : = lfGetDvAmt()
*!**************************************************************************
*
FUNCTION lfGetDvAmt
PARAMETERS lcGroup
PRIVATE lcAlias , lnTotal , lnDivNo
lnTotal = 0
lcAlias = SELECT()

SELECT GLGRPDT
SET ORDER TO TAG Grcodacc
=SEEK(LEFT(UPPER(lcGroup),8))
SCAN REST WHILE cGrpCode = LEFT(UPPER(lcGroup),8)
  =SEEK(cAcctCode,lcWorkFile)
  IF SEEK(ALLTRIM(SUBSTR(cAcctCode,laSegInf[1,1]+2)),(lcTmpAllDv)) AND &lcTmpAllDv..cPageNo = ALLTRIM(STR(lnNumber,4))
    *Generic    
    *IF MOD(RECCOUNT(lcTmpAllDv),4) = 0
    IF MOD(VAL(&lcTmpAllDv..cLineNo),5) = 0
      *Generic 
      *lnDivNo = 4
      lnDivNo = 5
    ELSE
      *Generic
      *lnDivNo = MOD(RECCOUNT(lcTmpAllDv),4)
      lnDivNo = MOD(VAL(&lcTmpAllDv..cLineNo),5)
    ENDIF
    laDivValue[lnDivNo] = laDivValue[lnDivNo] + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
    laDivTotal[lnDivNo] = laDivTotal[lnDivNo] + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
  ENDIF
  lnTotal = lnTotal + IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
ENDSCAN
SELECT (lcAlias)
RETURN lnTotal
*-- End of lfGetDvAmt

*!*************************************************************
*! Name      : lfGetAmnt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to get Amount 
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetAmnt()
*!*************************************************************
FUNCTION lfGetAmnt
PARA lcGroup
PRIVATE lnTotal , lnAlias

lcOldAlias = SELECT()

lnTotal = 0
SELECT GLGRPDT
SET ORDER TO TAG Grcodacc
=SEEK(LEFT(UPPER(lcGroup),8))
SCAN REST WHILE cGrpCode = LEFT(UPPER(lcGroup),8)
  =SEEK(cacctcode,lcWorkFile)
  lnTotal = lnTotal +IIF(LEFT(&lcWorkFile..cTypeCode,1) $ 'SI',&lcWorkFile..nCol1,&lcWorkFile..nCol1*-1)
ENDSCAN
SELECT (lcOldAlias)
RETURN lnTotal

*-End of lfGetAmnt
*!*************************************************************
*! Name      : LFGETGROUP
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to get Group Disc.
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetGroup()
*!*************************************************************
FUNCTION lfGetGroup
PARAMETER lcDummy
PRIVATE lnAlias ,lcDummy
lcAlias = ALIAS()   && Save Current alias.
DO CASE

    CASE LEFT(EVAL(lcTempWork+'.cLevel1'),3)='S00'
      lcGroupDis = "Sales Revenue"

    CASE LEFT(EVAL(lcTempWork+'.cLevel1'),4)='IC00'
      lcGroupDis = "Cost of Goods sold"
    
    CASE LEFT(EVAL(lcTempWork+'.cLevel1'),3)='E00'
      lcGroupDis = "Expenses"
    
    CASE LEFT(EVAL(lcTempWork+'.cLevel1'),3)='I00'
      lcGroupDis = "Other Income/Expenses"

    CASE LEFT(EVAL(lcTempWork+'.cLevel1'),3)='T00'
      lcGroupDis = "Taxes"
    
ENDCASE    
SELECT (lcAlias)    && Restore before function alias.
RETURN ''

*!*************************************************************
*! Name      : lfvSelDiv
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/31/2001
*! Purpose   : Function to refrish Screen .
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSelDiv()
*!*************************************************************
FUNCTION lfvSelDiv

*CLEAR READ
laOGObjCnt[lnDivPos] = llRpStByDv
lcRpDivNo = ''
=lfOGShowGet('lcRpDivNo')  && Show get Object .
DO lpSetForm

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/28/2001
*! Purpose   : When function for the report.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnDivPos = lfVarPos('lcRpDivNo')
=lfvSelDiv()
DO lpSetForm
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/28/2001
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : to get the position of the Variable in OG
*!***************************************************************************
*! Example   : = lfVarPos()
*!***************************************************************************
*
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.

*!**************************************************************************
*! Name      : lpSetForm
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/28/2001
*! Purpose   : Set the report form.
*!**************************************************************************
*! Example   : lpSetForm
*!**************************************************************************
*
PROCEDURE lpSetForm
*C102529,1 Handle the form name. [Begin]
IF llRpStByDv
  IF EMPTY(lcRpDivNo)
    lcRpForm = 'GLSyu21'
  ELSE
    lcRpForm = 'GLSyu20'
  ENDIF  
ELSE
  lcRpForm = 'GLSyu20'
ENDIF  
*C102529,1 Handle the form name. [End]
*-- End of lpSetForm.