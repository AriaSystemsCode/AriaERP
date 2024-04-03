*E303256,1 TMI 02/25/2013 Balance sheet and income statement, this file is a comment used with both reports

* N000682 ,1 Thabet Handle globalization issues [Start]
#include r:\aria4xp\reports\gl\glhosrow.H
* N000682 ,1 Thabet Handle globalization issues [END]
*E303228,1 TMI 09/19/2012 [Start] Call only one program, maintian only one version,
*                                 put an if statement to differentiat between the actions required for the two versions
DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLHOSROW.SCX')

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

DO FORM (oAriaApplication.ReportHome+oAriaApplication.ActiveModuleID+'\GLSUMZBY.SCX')
*E300683,6 end

SELECT (lcOldAlias)


*!************************************************************************
*!
*!      FUNCTION lfDisData
*!
*!************************************************************************
* Display Main types and Subtype on screen  Colling from show procedure
FUNCTION lfDisData
PARAMETERS loFormSet

* Display the  Main type
lcOldArrName = lcRpArrName
RELEASE BARS ALL OF POTYPES
lnBarNo  = 0
lnColNum = 2


LOCAL lcLine
WITH loFormSet.Ariaform1.lsTypes
.Clear()
lcChr1 = CHR(3)
lcChr2 = CHR(4)
lcChr3 = CHR(5)

FOR lnMaCount = 1 TO ALEN(laRpMaType,1)
  lcRpArrName = "laRp"+laRpMatype[lnMaCount,1]+"Type"
  lcLine = laRpMaType[lnMaCount,1]+"00 Description Ú  "+;
         (ALLTRIM(SUBSTR(laRpMatype[lnMaCount,lnColNum],1,30)))
  .AddItem(lcLine)

  IF !EMPTY(&lcRpArrName.[1,1])
    FOR lnSCount = 1 to ALEN(&lcRpArrName.,1)
      IF EMPTY(ALLTRIM(&lcRpArrName.[lnSCount,2]))
        &lcRpArrName.[lnSCount,2] = &lcRpArrName.[lnSCount,1]
      ENDIF
       lnBarNo = lnBarNo + 1
         lcLine = "  "+ ALLTRIM(&lcRpArrName.[lnSCount,1])+" Desc.     ³Ú "+;
         (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum],1,30)))
  .AddItem(lcLine)

      * Display Sub type total and footers
      lnBarNo = lnBarNo + 1
         lcLine = "                ³³ "+;
         'SubTotal of '+(ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum],1,18)))
  .AddItem(lcLine)
      lnBarNo = lnBarNo + 1
         lcLine = "  "+ALLTRIM(&lcRpArrName.[lnSCount,1])+" Footer 1  ³³ "+;
         (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum+1],1,30)))
  .AddItem(lcLine)
      lnBarNo = lnBarNo + 1
         lcLine = "  "+ALLTRIM(&lcRpArrName.[lnSCount,1])+" Footer 2  ³À "+;
        (ALLTRIM(SUBSTR(&lcRpArrName[lnSCount,lnColNum+2],1,30)))
  .AddItem(lcLine)

    ENDFOR
  ENDIF
  * Display Main type total and footers
  lnBarNo = lnBarNo + 1
         lcLine = "                ³  "+;
         'SubTotal of '+(ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum],1,18)))
  .AddItem(lcLine)
  lnBarNo = lnBarNo + 1
         lcLine = laRpMaType[lnMaCount,1]+"00 Footer 1    ³  "+;
         (ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum+1],1,30)))
  .AddItem(lcLine)
  lnBarNo = lnBarNo + 1
         lcLine = laRpMaType[lnMaCount,1]+"00 Footer 2    À  "+;
         (ALLTRIM(SUBSTR(laRpMaType[lnMaCount,lnColNum+2],1,30)))
  .AddItem(lcLine)
ENDFOR
ENDWITH
*** Restore the old array name
lcRpArrName = lcOldArrName

*!************************************************************************
*!
*!      X_FUNCTION lfDisItem
* FUNCTION LFDISITEM
*!
*!************************************************************************
* Display one Item
FUNCTION lfDisItem
PARAMETER loFormSet,llDoChang,lcRpStat,lnRpPos,lnChPosNo

*DEFINE BAR lnRpPos OF poTypes ;
  PROMPT SUBSTR(PRMBAR('poTypes',lnRpPos),1,19)+;
  (ALLTRIM(SUBSTR(lcRpStat,1,30)))
WITH loFormSet.Ariaform1.lsTypes
  lcLine = SUBSTR(.Value,1,19)+;
    (ALLTRIM(SUBSTR(lcRpStat,1,30)))
  .List[.ListIndex] = lcLine

IF llDoChang
  *DEFINE BAR lnChPosNo OF poTypes ;
         PROMPT SUBSTR(PRMBAR('poTypes',lnChPosNo),1,19)+;
         'SubTotal of '+(ALLTRIM(SUBSTR(lcRpStat,1,18)))
  lcLine = SUBSTR(.List[lnChPosNo],1,19)+;
         'SubTotal of '+(ALLTRIM(SUBSTR(lcRpStat,1,18)))
  .List[lnChPosNo] = lcLine
ENDIF
ENDWITH

*!************************************************************************
*!
*!      FUNCTION lfvTypes
*!
*!************************************************************************
*
FUNCTION lfvTypes
PARAMETERS loFormSet

lsTypes = loFormSet.Ariaform1.lsTypes.ListIndex

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
llOk = .T.
DO CASE
  CASE lsTypes - (lnRpTot-lnRpLen) = 1
    lcRpDesc     = laRpMaType[lnMCount,2]
    lcVarToCahnge= "laRpMaType[lnMCount,2]"
    llSubType = .T.
    lnOldVal = lnMCount
    lnRpYPos = 1

	DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glincDes.SCX') WITH loFormSet
	IF !llOk
	  RETURN
	ENDIF

    =lfDisItem(loFormSet,.T.,laRpMaType[lnOldVal,2],lsTypes,lnRpTot-2)

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

	  DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glincDes.SCX') WITH loFormSet
  	  IF !llOk
	    RETURN
 	  ENDIF

      =lfDisItem(loFormSet,llChange,&lcVarToCahnge,lsTypes,lsTypes+1)
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

	  DO form (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glincDes.SCX') WITH loFormSet
	  IF !llOk
	    RETURN
	  ENDIF

      =lfDisItem(loFormSet,.F.,&lcVarToCahnge,lsTypes,1)
    ENDIF
ENDCASE
llSubType = .F.
IF llDoneArr
  =lfDisData(loFormSet)
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
PARAMETER loFormSet,lnRpArrType,lcPasArrName
IF ALEN(&lcPasArrName,1) > 1

  =gfFillPop('poArrTypes',lcPasArrName,2)
  lnRpArrSize = ALEN(&lcPasArrName,1) + 2

  DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLINCARR.scx') WITH lcPasArrName

  IF llDoneArr
    llDoneArr = .F.
    DO CASE
      CASE lnRpArrType = 1 OR lnRpArrType = 2
        =lfDisData(loFormSet)
      CASE lnRpArrType = 3
        =lfDisCol(loFormSet)
    ENDCASE
  ENDIF
ENDIF
*- End of lfvArrange.


*!************************************************************************
*!
*!      FUNCTION lfvArrOk
*!
*!************************************************************************
*
FUNCTION lfvArrOk
PARAMETERS loArrFormSet

IF lnRpArrType = 3
  FOR lnBarCount = 1 To CNTBAR('poArrTypes')
    IF GETBAR('poArrTypes',lnBarCount) <> lnBarCount AND;
      laRpCol[lnBarCount,1] = 'O'
      laRpCol[lnBarCount,20] = lfGetNewBar(laRpCol[lnBarCount,20])
      laRpCol[lnBarCount,21] = lfGetNewBar(laRpCol[lnBarCount,21])
    ENDIF
  ENDFOR
ENDIF

=lfModiArr(loArrFormSet,lcPasArrName,'poArrTypes')
llDoneArr = .T.

*!*************************************************************************
*!
*!              Function: lfModiArr
*!
*!*************************************************************************
FUNCTION lfModiArr
* FUNCTION LFMODIARR
PARAMETERS loArrFormSet,lcArrName,lcPopName

lnArrRow=ALEN(&lcArrName,1)
lnArrCol=ALEN(&lcArrName,2)
IF lnArrCol>0
  DIMENSION laTemp[lnArrRow,lnArrCol]
ELSE
  DIMENSION laTemp[lnArrRow]
ENDIF
WITH loArrFormSet.Ariaform1.lsArrTypes
IF .ListCount > 0
  FOR lnCount=1 TO .ListCount
    .ListIndex = lnCount
    =ACOPY(&lcArrName,laTemp,(.ListItemId-1)*IIF(lnArrCol>0,lnArrCol,1)+1 ,;
           IIF(lnArrCol>0,lnArrCol,1),(lnCount-1)*IIF(lnArrCol>0,lnArrCol,1)+1)
  ENDFOR
  =ACOPY(laTemp,&lcArrName)
ENDIF
ENDWITH

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
PARAMETER loGLIncDesFormset,lcRpPasArr

DIMENSION laTemp(1)
lnCoPos = 1
lnDesPos = 2

=gfSubStr(&lcRpPasArr[lnRpXPos,lnRpYPos+2],@laTemp,'|')

DIMENSION laTarget(ALEN(laTemp,1),2)

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

LOCAL laSource,i,lcTrgt
DIMENSION laSource[ALEN(&lcRpPasArr,1),2]
FOR i=1 TO ALEN(&lcRpPasArr,1)
  laSource[i,1] = &lcRpPasArr[i,2]
  laSource[i,2] = &lcRpPasArr[i,1]
ENDFOR
=lfOGMover(@laSource,@laTarget,' ',.T.,'')

IF !EMPTY(laTarget[1])
  lcTrgt = ''
  FOR i=1 TO ALEN(laTarget,1)
    lcTrgt = lcTrgt + laTarget[i,2] + '|'
  ENDFOR
  &lcRpPasArr[lnRpXPos,lnRpYPos+2] = LEFT(lcTrgt,LEN(lcTrgt)-1)
ENDIF


*!*************************************************************************
*!
*!              Function lfvCol
*!
*!*************************************************************************
*
FUNCTION lfvCol
PARAMETERS loFormSet

llEditMode = .T.
DIMENSION laTempCol(ALEN(laRpCol,1),24)
=ACOPY (laRpCol,laTempCol)
lnNewCol = 0
lsCol = loFormset.Ariaform1.lsCol.ListIndex
lnRpCol = lsCol
lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                         lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lnRpCol,23])
llFirTim = .T.

DO FORM (loOGScroll.gcRepHome + loOGScroll.gcAct_Appl + '\GLHOSCOL.scx') WITH loFormSet


*!*************************************************************************
*!
*!              Function lfvAddCol
*!
*!*************************************************************************
FUNCTION lfvAddCol
PARAMETERS loFormSet

llEditMode = .F.
DIMENSION laTempCol(ALEN(laRpCol,1),24)
=ACOPY (laRpCol,laTempCol)
llFirTim = .T.
lnNewCol = 0
lnRpNoCol = loFormSet.Ariaform1.lsCol.ListCount + 1

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

DO FORM (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\GLHOSCOL.scx')   WITH loFormSet

*loFormSet.Ariaform1.pbAdd.Enabled = !( loFormSet.Ariaform1.lsCol.ListCount + lnRpIdnCol >= lnRpInvNo )
loFormSet.Ariaform1.lsCol.ListIndex = loFormSet.Ariaform1.lsCol.ListCount

*!*************************************************************************
*!
*!              Function lfvRemCol
*!
*!*************************************************************************
*
FUNCTION lfvRemCol
PARAMETERS loFormSet
LOCAL lnCnt

lsCol = loFormSet.Ariaform1.lsCol.ListIndex
IF loFormSet.Ariaform1.lsCol.ListCount>0 AND lsCol>0
  lnRpIdnCol = lnRpIdnCol - IIF(ATC(laRpCol[lsCol,1],'BAO') > 2,0,;
                          lfBigNo('laRpCol',lsCol,4,7)+laRpCol[lsCol,23])
  lnRpColUsd=lnRpColUsd-IIF(laRpCol[lsCol,1] $ 'AB',MAX(1,IIF(laRpCol[lsCol,23]=1,1,0)+laRpCol[lsCol,7]+1),1)
  =ADEL(laRpCol,lsCol)
  IF ALEN(laRpCol,1) > 1
    DIMENSION laRpCol(ALEN(laRpCol,1)-1,24)
  ENDIF
  =lfDisCol(loFormSet)
  loFormSet.Ariaform1.pbAdd.Enabled = .T.
  loFormSet.Ariaform1.pbArrCol.Enabled = loFormSet.Ariaform1.lsCol.ListCount>1
ENDIF


*!*************************************************************************
*!
*!              Function lfvOkCol
*!
*!*************************************************************************
*
FUNCTION lfvOkCol
PARAMETERS loFormSet


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
  * 09/20/2012 [Start] in case of operation, force keying a value in the fields for Col#, Col#
  OTHERWISE
    IF EMPTY(laRpCol[lnRpCol,20]) OR EMPTY(laRpCol[lnRpCol,21])
      llRetVal = .F.
    ENDIF
ENDCASE
IF !llRetVal
  *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[Start]
  *WAIT 'You have to enter the column information' WINDOW NOWAIT
  IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
    CHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLHOSROW_H.XML')
  ENDIF  
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENTERCOLUMNINFO,oAriaApplication.GetHeaderText("LANG_ENTERCOLUMNINFO",CHeaderFile))  WINDOW NOWAIT
  *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[End]
  *_CUROBJ = _CUROBJ
  RETURN .F.
ELSE
  IF llEditMode
    lnRpColUsd=lnRpColUsd-IIF(laTempCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laTempCol[lnRpCol,23]=1,1,0)+laTempCol[lnRpCol,7]+1),1)
  ENDIF
****
  lnRpColUsd=lnRpColUsd+IIF(laRpCol[lnRpCol,1] $ 'AB',MAX(1,IIF(laRpCol[lnRpCol,23]=1,1,0)+laRpCol[lnRpCol,7]+1),1)
  =lfDisCol(loFormSet)
ENDIF
*=lfDisCol(loFormSet)


*!*************************************************************************
*!
*!          Function lfvCancCol
*!
*!*************************************************************************
*
FUNCTION lfvCancCol
PARAMETERS loFormSet
DIMENSION laRpCol(ALEN(laTempCol,1),24)
=ACOPY (laTempCol,laRpCol)
=lfDisCol(loFormSet)
************************************************************
*! Name      : lfvIdnCol
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/16/2012
*! Purpose   : Enable / Disable controls on the add screen
************************************************************
FUNCTION lfvIdnCol
PARAMETERS loHosColFormset,loFld,lnColNo
LOCAL i,lcI
WITH loHosColFormset.Ariaform1
  FOR i = lnColNo TO 7
    lcI = STR(i,1)
    .lnRpCol_&lcI..Value = .lnRpCol_&lcI..Value + IIF(loFld.Value=1,1,-1)
  ENDFOR
ENDWITH
=lfvIdnEnDs(loHosColFormset,loFld,lnColNo)
************************************************************
*! Name      : lfvIdnEnDs
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/24/2012
*! Purpose   : Enable Disable checkboxes
************************************************************
FUNCTION lfvIdnEnDs
PARAMETERS loHosColFormset,loFld,lnColNo
WITH loHosColFormset.Ariaform1
  FOR i = 1 TO 4
    lcI = STR(i,1)
    IF i+3=lnColNo
      .cbIdent&lcI..Enabled = .T.
    ELSE
      IF .lnRpCol_23.Value = 1
        .cbIdent&lcI..Enabled = loFld.Value = 0
      ELSE
        .cbIdent&lcI..Enabled = .lnRpCol_7.Value =< 1 OR .cbIdent&lcI..Value = 1
      ENDIF
    ENDIF
  ENDFOR
  .lnRpCol_23.Enabled = .lnRpCol_7.Value < 2
  .lnRpCol_22.Enabled = .lnRpCol_23.Value=1
ENDWITH
*- End of lfvIdnEnDs.

*- End of lfvIdnCol.

*!*************************************************************************
*!
*!          Function lfDisCol
*!
*!*************************************************************************
FUNCTION lfDisCol
PARAMETERS loFormSet
LOCAL lcLine
lnRpIdnCol = 0
lnCount = 1
lnRpNoCol = 0
loFormSet.Ariaform1.lsCol.Clear()
FOR lnCount = 1 TO ALEN(laRpCol,1)
  lnRpNoCol = lnRpNoCol + 1
  IF !EMPTY(laRpCol[lnCount,1])
    lnRpIdnCol = lnRpIdnCol + IIF(ATC(laRpCol[lnCount,1],'BAO') > 2,0,;
                         lfBigNo('laRpCol',lnCount,4,7)+laRpCol[lnCount,23])
     lcLine = IIF(EMPTY(ALLTRIM(laRpCol[lnCount,2]+laRpCol[lnCount,3])),;
                   'Col '+ALLTRIM(STR(lnCount)),laRpCol[lnCount,2]+' '+;
                   laRpCol[lnCount,3])
    loFormSet.Ariaform1.lsCol.AddItem(lcLine)
  ENDIF
ENDFOR
WITH loFormSet.Ariaform1
  .pbRemove.Enabled = .lsCol.ListCount>=1
  .pbArrCol.Enabled = .lsCol.ListCount>=2
ENDWITH

=lfColShow(loFormSet)


*!*************************************************************************
*!
*!              Function lfColShow
*!
*!*************************************************************************
*
FUNCTION lfColShow
PARAMETERS loFormSet

WITH loFormSet.Ariaform1
DO CASE
  CASE .lsCol.ListCount = 0
    .pbRemove.Enabled =  .F.
    .pbArrCol.Enabled =  .F.
    .pbAdd.Enabled =  .T.
  CASE .lsCol.ListCount + lnRpIdnCol >= lnRpInvNo
    .pbRemove.Enabled =  .T.
    .pbAdd.Enabled =  .F.
    .pbArrCol.Enabled =  .T.
  CASE  .lsCol.ListCount <= 1
    .pbArrCol.Enabled =  .F.
    .pbRemove.Enabled =  .T.
    .pbAdd.Enabled =  .T.
  OTHERWISE
    .pbArrCol.Enabled =  .T.
    .pbRemove.Enabled =  .T.
    .pbAdd.Enabled =  .T.
ENDCASE
ENDWITH


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
PARAMETERS loHosColFormSet

llRetVal = .T.

IF rbColmntype = 3 AND laRpCol[lnRpCol,1] $ 'BA'
  FOR lnCount = 1 TO lnRpNoCol

    IF (laRpCol[lnCount,20] = lnRpCol OR;
        laRpCol[lnCount,21] = lnRpCol) AND;
       laRpCol[lnCount,1] $ 'OP'

       rbColmntype = ATC(lcOldColType,'BAOP')
       llRetVal = .F.
       *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[Start]
       *WAIT "This column is used by column "+ALLTRIM(STR(lnCount)) WINDOW NOWAIT
       IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
         CHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLHOSROW_H.XML')
	   ENDIF  
       WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_USEDCOLUMN,oAriaApplication.GetHeaderText("LANG_USEDCOLUMN",CHeaderFile))+" "+ALLTRIM(STR(lnCount)) WINDOW NOWAIT
       *N000682,1 MMT 03/17/2013 Fix issues of Globalization Testing Phase#2[End]
       EXIT
    ENDIF
  ENDFOR
ENDIF
IF llRetVal
   IF loOgScroll.lcOgRepID = 'GLINCSTA'
     =lfvCol12(loHosColFormSet)
   ENDIF
ENDIF
RETURN llRetVal

*!*************************************************************************
*!
*!           Function lfvCol8
*!
*!*************************************************************************
*
FUNCTION lfvCol8
PARAMETERS loHosColFormSet,loFld
lcRpCurFld      = loFld.ControlSource

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
lcBrFields    = 'CBUDCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget_code,oAriaApplication.GetHeaderText("LANG_Budget_code",BHeaderFile))+;
                '",CBUDYEAR:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",BHeaderFile))+;
'",CBUDDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",BHeaderFile))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'GLBUDHD')ORDER TAG BdCodYr
    llUesdBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. ) .OR. ;
     (EMPTY(laRpCol[lnRpCol,9]) AND !SEEK(&lcRpCurFld)) .OR. ;
     (!EMPTY(laRpCol[lnRpCol,9]) AND !SEEK(&lcRpCurFld+laRpCol[lnRpCol,9]))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
	=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDANDISC,oAriaApplication.GetHeaderText("LANG_BUDANDISC",BHeaderFile)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,9] = laRpRetFld[2]
    loFld.parent.lnRpCol_9.Value = laRpRetFld[2]
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
PARAMETERS loHosColFormSet,loFld

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBUDCODE:H="Budget code",CBUDYEAR:H="Year",CBUDDES:H="Description"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF 
lcBrFields    = 'CBUDCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget_code,oAriaApplication.GetHeaderText("LANG_Budget_code",BHeaderFile))+;
                '",CBUDYEAR:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",BHeaderFile))+;
'",CBUDDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",BHeaderFile))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = loFld.ControlSource

&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("GLBUDHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'GLBUDHD')ORDER TAG BdCodYr
    llUesdBefo = .T.
  ENDIF
  SELECT GLBUDHD
  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(laRpCol[lnRpCol,8]+&lcRpCurFld.))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",'Budget code & Fiscal year ',.F.)
	=gfBrows([],'CBUDCODE,CBUDYEAR',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDANDISC,oAriaApplication.GetHeaderText("LANG_BUDANDISC",BHeaderFile)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,8]  = laRpRetFld[1]
    loFld.parent.lnRpCol_8.Value = laRpRetFld[1]

  ENDIF
  IF llUesdBefo       && .F.- this file used by the system
    USE IN GLBUDHD
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!*************************************************************************
*!
*!          Function lfvCol13
*!
*!*************************************************************************
*
FUNCTION lfvCol13
PARAMETERS loHosColFormSet,loFld
loFld.Value = PADL(ALLTRIM(loFld.Value),2,'0')

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF 
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",BHeaderFile))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Period,oAriaApplication.GetHeaderText("LANG_Period",BHeaderFile))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",BHeaderFile))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]

laRpRetFld[1] = ''
laRpRetFld[2] = ''
lcRpCurFld      = loFld.ControlSource
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))

  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD')ORDER TAG comfyrprdi
    llUesdBefo = .T.
  ENDIF
  SELECT FSPRD

  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. ) OR !Between(VAL(loFld.Value),1,12)
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
	=gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISYEAR,oAriaApplication.GetHeaderText("LANG_FISYEAR",BHeaderFile)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld         = laRpRetFld[2]
    laRpCol[lnRpCol,14] = laRpRetFld[1]
    loHosColFormSet.Refresh()

  ENDIF
  IF llUesdBefo       && .F.- this file used by the system

    USE IN FSPRD

  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!*************************************************************************
*!
*!              X_Function lfvCol14
* FUNCTION LFVCOL14
*!
*!*************************************************************************
*  Check if current company has this entried period or not
FUNCTION lfvCol14
PARAMETERS loHosColFormSet,loFld

DECLARE laRpRetFld(2)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF 
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year,oAriaApplication.GetHeaderText("LANG_Year",BHeaderFile))+;
                '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Period,oAriaApplication.GetHeaderText("LANG_Period",BHeaderFile))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Month,oAriaApplication.GetHeaderText("LANG_Month",BHeaderFile))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
laRpRetFld[2] = ''

lcRpCurFld      = loFld.ControlSource
&& Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FSPRD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FSPRD')ORDER TAG comfyrprdi
    llUesdBefo = .T.
  ENDIF
  SELECT FSPRD

  *** Search for the current company+year+Prd
  IF ('?' $ &lcRpCurFld. .OR.;
    !SEEK(ALLTRIM(&lcRpCurFld.)+laRpCol[lnRpCol,13]))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",'Fiscal year ',.F.)
	=gfBrows('','CFisFyear,CFsppRdid',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISYEAR,oAriaApplication.GetHeaderText("LANG_FISYEAR",BHeaderFile)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    &lcRpCurFld         = laRpRetFld[1]
    laRpCol[lnRpCol,13] = laRpRetFld[2]
    loHosColFormSet.Refresh()
  ENDIF

  IF llUesdBefo       && .F.- this file used by the system

    USE IN FSPRD

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
PARAMETERS loHosColFormset,loFld
IF laRpCol[lnRpCol,20] > 0
  IF !BETWEEN(laRpCol[lnRpCol,20],1,lnRpNoCol) OR;
            laRpCol[lnRpCol,20] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,20],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,20],1] <> 'A')
    RETURN .F.
  ENDIF
ENDIF

*!*************************************************************************
*!
*!              Function lfvCol21
*!
*!*************************************************************************
*
FUNCTION lfvCol21
PARAMETERS loHosColFormset,loFld

IF laRpCol[lnRpCol,21] > 0
IF !BETWEEN(laRpCol[lnRpCol,21],0,lnRpNoCol) OR;
            laRpCol[lnRpCol,21] = lnRpCol    OR ;
           (laRpCol[laRpCol[lnRpCol,21],1] <> 'B';
            AND laRpCol[laRpCol[lnRpCol,21],1] <> 'A')
    RETURN .F.
  ENDIF
ENDIF

*!*************************************************************************
*!
*!              Function lfvCol23
*!
*!*************************************************************************
*
FUNCTION lfvCol23
PARAMETERS loHosColFormSet

WITH loHosColFormSet.Ariaform1
IF laRpCol[lnRpCol,23] = 0
  .lnRpCol_22.Enabled = .F.
  FOR lnColCount = 4 TO 7
    lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
    .&lcCbIdent..Enabled = .T.
  ENDFOR
ELSE
  .lnRpCol_22.Enabled = .T.
  IF lnRpNoCol+lnRpIdnCol+lfBigNo('laRpCol',lnRpCol,4,7)+laRpCol[lnRpCol,23]  >= lnRpInvNo
    FOR lnColCount = 4 TO 7
      lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
      IF &lcCbIdent = 0
        .&lcCbIdent..Enabled = .F.
      ENDIF
    ENDFOR
  ENDIF
ENDIF
ENDWITH

*!*************************************************************************
*!
*!              Function lfvOkAll
*!
*!*************************************************************************
* Save all information that was added
FUNCTION lfvOkAll
PARAMETERS loFormSet

loFormSet.Ariaform1.Release

llRpToolCh = .T.


*!*************************************************************************
*!
*!              Function lfZoomWin
*!
*!*************************************************************************
*
FUNCTION lfZoomWin
PARAMETERS loHosColFormSet
WITH loHosColFormSet.Ariaform1
  laRpCol[lnRpCol,1]=SUBSTR('BAOPU',rbColmntype,1)
  IF laRpCol[lnRpCol,1] $ 'BA'
    lnHeigh = 323
    .Height = lnHeigh + 44
    .shpOk.Top = lnHeigh
    .cmdOk.Top = .shpOk.Top + 10
    .cmdCan.Top = .shpOk.Top + 10
  ELSE
    .shpOk.Top = 195
    .cmdOk.Top = .shpOk.Top + 10
    .cmdCan.Top = .shpOk.Top + 10
    .Height = .shpOk.Top + 42
  ENDIF
ENDWITH

************************************************************
*! Name      : lfGLHosCol
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/24/2012
*! Purpose   : init function for the GLHOSCOL screen
************************************************************
FUNCTION lfGLHosCol
PARAMETERS loHosColFormSet
* N000682 ,1 Thabet Handle globalization issues [Start]
*loHosColFormSet.Ariaform1.Caption = 'Add/Edit Columns'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
loHosColFormSet.Ariaform1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ADDEDIT,oAriaApplication.GetHeaderText("LANG_ADDEDIT",BHeaderFile))
* N000682 ,1 Thabet Handle globalization issues [End]
LOCAL lcMsk, lcSeg
lcMsk = LFGETPIC()
lcSeg = lfGetSegDis()
WITH loHosColFormSet.AriaForm1.pgfTypes
  .Ariapage1.lnRpCol_24.InputMask = lcMsk
  .Ariapage1.GetSegDis.Value = lcSeg
  .Ariapage2.lnRpCol_24.InputMask = lcMsk
  .Ariapage2.GetSegDis.Value = lcSeg
ENDWITH
*lfShowColTp()
loHosColFormSet.AriaForm1.rbColmntype.Ariaoptionbutton3.Enabled = lnRpNOCol >= 3

IF llEditMode
  rbColmntype = IIF(laRpCol[lnRpCol,1] $ 'BA',AT(laRpCol[lnRpCol,1],'BA'),3)
  loHosColFormSet.AriaForm1.rbColmntype.Click()
ENDIF

*- add the work "Totals " to the check boxes
LOCAL lnI,lcI

FOR lnColCount = 4 TO 7
  lcCbIdent = 'cbIdent' + ALLTRIM(STR(lnColCount-3))
  &lcCbIdent =  IIF(lnColCount = 4,laRpCol[lnRpCol,lnColCount],laRpCol[lnRpCol,lnColCount]-laRpCol[lnRpCol,lnColCount-1])
ENDFOR

LOCAL lsCol
lsCol = loHosColFormSet.loFormSet.Ariaform1.lsCol.ListIndex
WITH loHosColFormSet.AriaForm1
FOR lnI = 1 TO 4
  lcI = STR(lnI,1)
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *.cbIdent&lcI..Caption = ALLTRIM(.cbIdent&lcI..Caption)+IIF(loOgScroll.lcOgRepID = 'GLBALSHT',' totals','')
  .cbIdent&lcI..Caption = ALLTRIM(.cbIdent&lcI..Caption)+IIF(loOgScroll.lcOgRepID = 'GLBALSHT',' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALS,oAriaApplication.GetHeaderText("LANG_TOTALS",BHeaderFile)),'')
  * N000682 ,1 Thabet Handle globalization issues [End]
  lcCbIdent = 'cbIdent'+lcI
  .cbIdent&lcI..Value = &lcCbIdent.
  =IIF(llEditMode AND lsCol>0 AND .cbIdent&lcI..Value>0, ;
       lfvIdnEnDs(loHosColFormset,.cbIdent&lcI.,lnI+3) , '' )
ENDFOR
.lnRpCol_22.Enabled = .lnRpCol_23.Value=1
ENDWITH


*- if this is the Balance Sheet report then set the options of lnRpCol_22 to \<Assets,\<Liability,\<Equity, hide the col15,...,col17
IF loOgScroll.lcOgRepID = 'GLBALSHT'
  WITH loHosColFormSet.AriaForm1.lnRpCol_22
  .ButtonCount = 3
  * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  .Ariaoptionbutton1.Caption = '\<Assets'
*!*	  .Ariaoptionbutton2.Caption = '\<Liability'
*!*	  .Ariaoptionbutton3.Caption = '\<Equity'
.Ariaoptionbutton1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ASSET,oAriaApplication.GetHeaderText("LANG_ASSET",BHeaderFile ))
.Ariaoptionbutton2.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LIAB,oAriaApplication.GetHeaderText("LANG_LIAB",BHeaderFile ))
.Ariaoptionbutton3.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EQUITY,oAriaApplication.GetHeaderText("LANG_EQUITY",BHeaderFile ))
  * N000682 ,1 Thabet Handle globalization issues [END]
  ENDWITH

  WITH loHosColFormSet.AriaForm1.pgfTypes.Ariapage2
  .lblCol15.Visible = .F.
  .lblCol17.Visible = .F.
  .lblSlash1.Visible = .F.
  .lblSlash2.Visible = .F.
  .lnRpCol_15.Visible = .F.
  .lnRpCol_16.Visible = .F.
  .lnRpCol_17.Visible = .F.
  .lnRpCol_18.Visible = .F.
  .lnRpCol_12.ButtonCount = 1
  ENDWITH

  WITH loHosColFormSet.AriaForm1.pgfTypes.Ariapage1
  .lblForPrd.Caption = 'For period'
  .Arialabel10.Visible = .F.
  .Arialabel11.Visible = .F.
  .lnRpCol_11.Visible = .F.
  .lnRpCol_10.Left = ;
  .lnRpCol_9.Left
  ENDWITH

ENDIF

*- End of lfGLHosCol.
