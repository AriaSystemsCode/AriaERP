*:*************************************************************************
*: Program file  : GFSTYBRW.PRG
*: Program desc. : Style Global Browse.
*:        System : SY
*:        Module : SY
*:     Developer : Hesham El-Sheltawi
*:*************************************************************************
*: Passed parameters  : lcMajorOrNon,
*:                      lcMajor,
*:                      lcNonMajor,
*:                      llCanAdd
*:*************************************************************************
*: Modifications:
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[T20080820.0006]
*! [T20080808.0001], MMT 06/07/2009 fix error of style file is empty
*! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [T20121109.0006]
*:*************************************************************************
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
*PARAMETERS lcMajorOrNon,lcMajor,lcNonMajor,llCanAdd
PARAMETERS lcMajorOrNon,lcMajor,lcNonMajor,llCanAdd,lcCursor,lcSelFld,lcValidFun
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

PRIVATE laOpenFiles,laSeg,laSegSz,laSegVal,laAcDes,;
        lcReturn,laSeg,laAcDes,lcMask,lcSegVal,lcHeader,;
        lcBrTitl2,lcWith,lcEmptyErr,lnAlias,lnStyRec,lnSegRec,lcBtMpCan,lcBtMpSel,;
        lcMajorOrNon,lcMajor,lcNonMajor,llCanAdd,lnNoSeg,;
        lnPosition,lnMajorStart,lnMajorEnd,lcBarDesc,lcBarCode,;
        lcBarType,lcUsedType,llStructUse,llStyleUse,llSegvalUse,llScaleUse,llCodesUse,lcStyleTag,llStyleData,;
        llCanCont,lnCount,lnStartSeg,lnEndSeg,lnActSeg,lcExpr,lcAlias,lnLastFold,;
        lnNonMajStart,lnNonMajEnd,llUseColor,lnClrStrt,lnClrWidth


DECLARE laOpenFiles[1,1],laSeg[9,9],laSegSz[9,1],laSegVal[9],;
        laAcDes[9],laSegInfo[1,9]
STORE '' TO lcfolder,lcReturn,laSeg,laAcDes,lcMask,lcSegVal,lcHeader,lcBrTitl2,lcWith,lcEmptyErr,lcStyleTag
STORE 0 TO laSegVal,lnAlias,lnStyRec,lnSegRec
llUseColor  = .F.
LANG_Adding = 'Adding'
lnAlias = SELECT()

*lcWith = 'Style,Browse,Scale,cDivision,Domestic,Quality,Other,Free,Group'


lcDash = '-'
FOR lnCount = 1 TO 9
  laSeg[lnCount,1] = 0
ENDFOR
llStructUse = .F.
IF !USED('ICISTRU')
  USE (oAriaApplication.DataDir+'ICISTRU') IN 0
  llStructUse = .T.
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO
=SEEK('U1')
lnNoSeg = 0
lnPosition = 1
lnMajorStart = 1
lnMajorEnd = 0
lcBarDesc = 'Color   DivisionSeason  Scale   DomesticQuality Group   Other   Free    '
lcBarCode = 'CDZSTQGOF'
lcBarType = 'CCCSVVCOF'
lcUsedType =''

SCAN WHILE citemrecty+cisegno = 'U'
  lnNoSeg = lnNoSeg + 1
  laSeg[lnNoSeg,1]   = nisegsize
  laSeg[lnNoSeg,2]   = ALLT(cisegtype)
  laSeg[lnNoSeg,3]   = lnPosition
  IF laSeg[lnNoSeg,2] = 'C'
    llUseColor = .T.
    lnClrStrt  = laSeg[lnNoSeg,3]
    lnClrWidth = laSeg[lnNoSeg,1]
  ENDIF

  IF laSeg[lnNoSeg,2] $ 'CDGZ'
    laSeg[lnNoSeg,4]   = 'CODES'
    laSeg[lnNoSeg,5]   = ["]+'N'+PADR(IIF(laSeg[lnNoSeg,2]='C','COLOR',;
                             IIF(laSeg[lnNoSeg,2]='G','CSTYGROUP',;
                             IIF(laSeg[lnNoSeg,2]='Z','SEASON','CDIVISION'))),10)+["]
    laSeg[lnNoSeg,6]   = 'CCODE_NO'
    laSeg[lnNoSeg,8] =  [CCODE_NO:H='Code',CDISCREP:H='Description']
    laSeg[lnNoSeg,9] =   [CODES.CCODE_NO]
  ELSE
    laSeg[lnNoSeg,4]   = IIF(laSeg[lnNoSeg,2]='S','Scale','ICSEGVAL')
    laSeg[lnNoSeg,5]   = [']+IIF(laSeg[lnNoSeg,2]='S','S',PADR(lnNoSeg,1))+[']
    laSeg[lnNoSeg,6]   = IIF(laSeg[lnNoSeg,2]='S','SCALE','SEGVAL')
    laSeg[lnNoSeg,8] =  IIF(laSeg[lnNoSeg,2]='S',[Scale,Cscl_desc:H='Description'],;
                        [cisegval:H='Value',cisgvalSd:H='Short Description',cisgvalld:H='Long Description'])
    laSeg[lnNoSeg,9] =   IIF(laSeg[lnNoSeg,2]='S',[SCALE],[CISEGVAL])
  ENDIF
  lcUsedType = lcUsedType +IIF(laSeg[lnNoSeg,4] $ lcUsedType,'',IIF(EMPTY(lcUsedType),'',',')+PADR(laSeg[lnNoSeg,4],10))
  laSeg[lnNoSeg,7] =  SUBSTR(lcBarDesc,(ATC(laSeg[lnNoSeg,2],lcBarCode)-1)*8+1,8)
  laAcDes[lnNoSeg] = SUBSTR(cisegsdes,1,laSeg[lnNoSeg,1])
  lcMask = lcMask+REPL('X',laSeg[lnNoSeg,1])+ALLT(CISEGSEPR)
  lcSegVal = lcSegVal +REPL(' ',laSeg[lnNoSeg,1])+ALLT(CISEGSEPR)
  lnPosition         = lnPosition + laSeg[lnNoSeg,1] + LEN(ALLT(CISEGSEPR))
  lcHeader = IIF(lnNoSeg=1,cIsegHead,lcHeader)
  lnMajorEnd = IIF(lSegEndMaj,lnNoSeg,lnMajorEnd)
ENDSCAN


IF llStructUse AND USED('ICISTRU')
  USE IN ICISTRU
ENDIF
STORE .F. TO llStyleUse,llScaleUse,llCodesUse,llSegvalUse
lnStyRec = 0


IF !USED('STYLE')
  USE (oAriaApplication.DataDir+'STYLE') IN 0
  llStyleUse = .T.
ELSE
  lcStyleTag = ORDER('STYLE')
ENDIF

lnNonMajStart = lnMajorEnd+1
lnNonMajEnd = lnNoSeg
llValidate = .F.

SELECT STYLE
lnStyRec = RECNO()
SET ORDER TO TAG STYLE
LOCATE
llStyleData = FOUND()
llCanCont = llCanAdd OR FOUND()

*--Not able to continue.
IF !llCanCont
  =gfModalGen('QRM00275B00000','DIALOG')

  IF llStyleUse AND USED('STYLE')
    USE IN STYLE
  ELSE
    SELECT STYLE
    IF !EMPTY(lcStyleTag)
      SET ORDER TO TAG (lcStyleTag)
    ELSE
      SET ORDER TO
    ENDIF
  ENDIF
  SELECT (lnAlias)
  *[T20080808.0001], MMT 06/07/2009 fix error of style file is empty[Start]
  IF  (TYPE('lcCursor')= 'C' AND !EMPTY(lcCursor)) AND (TYPE('lcSelFld')= 'C' AND !EMPTY(lcSelFld))
    RETURN .F.
  ENDIF
  *[T20080808.0001], MMT 06/07/2009 fix error of style file is empty[End]
  RETURN lcReturn
ENDIF


DIMENSION laUseFile[1,1]
=gfSubStr(lcUsedType,@laUseFile)
DIMENSION laOpenFiles[ALEN(laUseFile,1),4]
STORE '' TO laOpenFiles
*! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [Start]
*!*	FOR lnCount = 1 TO ALEN(laUseFile,1)
*!*	  lcFileName = ALLT(laUseFile[lnCount,1])
*!*	  laOpenFiles[lnCount,1] = lcFileName
*!*	  laOpenFiles[lnCount,2] = !USED(lcFileName)
*!*	  IF laOpenFiles[lnCount,2]
*!*	    USE (oAriaApplication.DataDir+laOpenFiles[lnCount,1]) IN 0
*!*	  ELSE
*!*	    laOpenFiles[lnCount,3] = FILTER(laOpenFiles[lnCount,1])
*!*	    laOpenFiles[lnCount,4] = ORDER(laOpenFiles[lnCount,1])
*!*	  ENDIF
*!*	  SELECT (laOpenFiles[lnCount,1])
*!*	  lnFilePos = ASCAN(laSeg,lcFileName)
*!*	  lnFilePos = ASUBSC(laSeg,lnFilePos,1)
*!*	  SET ORDER TO TAG (laSeg[lnFilePos,6])
*!*	  SET FILTER TO
*!*	ENDFOR
FOR lnFileCount = 1 TO ALEN(laUseFile,1)
  lcFileName = ALLT(laUseFile[lnFileCount,1])
  laOpenFiles[lnFileCount,1] = lcFileName
  laOpenFiles[lnFileCount,2] = !USED(lcFileName)
  IF laOpenFiles[lnFileCount,2]
    lnFilePos = ASCAN(laSeg,lcFileName)
    lnFilePos = ASUBSC(laSeg,lnFilePos,1)
    =gfOpenTable(oAriaApplication.DataDir+laOpenFiles[lnFileCount,1], laSeg[lnFilePos,6], 'SH')
  ELSE
    laOpenFiles[lnFileCount,3] = FILTER(laOpenFiles[lnFileCount,1])
    laOpenFiles[lnFileCount,4] = ORDER(laOpenFiles[lnFileCount,1])
  ENDIF
  SELECT (laOpenFiles[lnFileCount,1])
  lnFilePos = ASCAN(laSeg,lcFileName)
  lnFilePos = ASUBSC(laSeg,lnFilePos,1)
  SET ORDER TO TAG (laSeg[lnFilePos,6])
  SET FILTER TO
ENDFOR
*! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [END]
SELECT STYLE
DO CASE
  CASE lcMajorOrNon='M' AND !lfEmpty(lcMajor,'M')
      DO CASE
        CASE llCanAdd AND !SEEK(lcMajor,'STYLE')
          lnOption = gfModalGen('QRM00001B42003','Dialog',ALLT(gfItemMask('HM'))+": "+ALLTRIM(lcMajor))
          DO CASE
            CASE  lnOption = 3
              llCanCont = .F.
              lcReturn = ''
            CASE lnOption = 1
              llCanCont = !(lcMajor = lfvSeg(lcMajor,'M'))
              IF !llCanCont
                lcReturn = lcMajor
              ELSE
                lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lfvSeg(lcMajor,'M'))
                llStyleData = .F.
              ENDIF
              IF !llAddRec
                =gfModalGen("TRM00088B00000","ALERT",LANG_Adding)
                llCanCont = .F.
                lcReturn = ''
              ENDIF
            OTHERWISE
              llCanCont = llStyleData
              SELECT STYLE
              lnRecNO = IIF(SEEK(ALLT(lcMajor),'STYLE'),RECNO(),RECNO(0))
              IF  lnRecNO > 0
                GO lnRecNO
              ELSE
                GO TOP
              ENDIF
              IF lcMajorOrNon $'NI'
                lcMajor = ''
              ENDIF
              lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
          ENDCASE
        CASE !llCanAdd
            lcMajor = lfvSeg(lcMajor,'M')
            lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
            llStyleData = .F.
      ENDCASE

  CASE lcMajorOrNon='N' AND lfEmpty(lcMajor,'M')
    lcMajorOrNon = 'I'

  CASE lcMajorOrNon='N' AND lfEmpty(lcNonMajor,'N')
    lcReturn = STRTRAN(gfItemMask('PN'),'X','*')
    llCanCont = .F.

  CASE lcMajorOrNon='N'
    lcSegVal = STUFF(lcSegVal,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
    lcMask = STUFF(lcMask,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1,lcMajor)
    llStyleData = .F.
    llValidate = .T.

  CASE lcMajorOrNon='M'
     lnNoSeg = lnMajorEnd
     lcMask  = SUBSTR(lcMask,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1)
     lcHeader = SUBSTR(lcHeader ,1,laSeg[lnMajorEnd,3]+laSeg[lnMajorEnd,1]-1)

  CASE lcMajorOrNon = 'I' AND !EMPTY(lcMajor)
    SELECT STYLE
    llMajorFound = SEEK(lcMajor)
    IF TYPE('llbrowse')='L' AND llbrowse
       llCanCont = .T.
    ELSE
       IF llMajorFound
         lcReturn = STYLE.STYLE
         llCanCont = .F.
       ELSE
         lcMajor = PADR(lcMajor,LEN(gfItemMask('PM')))
         lnRecNO = IIF(SEEK(lcMajor,'STYLE'),RECNO(),RECNO(0))
         llCanCont = .T.
       ENDIF
       IF llCanCont
         IF  lnRecNO>0
           GO lnRecNO
           IF PADR(STYLE.STYLE,LEN(ALLT(lcMajor)))=ALLT(lcMajor)
             SKIP
             IF PADR(STYLE.STYLE,LEN(ALLT(lcMajor)))<>ALLT(lcMajor)
               SKIP -1
               lcReturn = STYLE.STYLE
               llCanCont = .F.
             ELSE
               SKIP -1
             ENDIF
           ELSE
             lcMajor = ''
           ENDIF
         ELSE
           GO TOP
           lcMajor = ''
         ENDIF
       ENDIF
    ENDIF
ENDCASE

*lcMask = '@! '+lcMask
lnStartSeg = IIF(lcMajorOrNon = 'N',lnNonMajStart,1)
lnEndSeg   = IIF(lcMajorOrNon = 'M',lnMajorEnd,lnNonMajEnd)
lnActSeg    = lnStartSeg
IF lcMajorOrNon = 'M'
  lcMajor = IIF(lfEmpty(lcMajor,'M'),'',lcMajor)
  SELECT STYLE
  SET ORDER TO TAG CSTYLE
ENDIF



lnSegWidth = lfStyInfo('M')
FOR lnLoop = 1 TO ALEN(laSegInfo,1)
  IF laSegInfo[lnLoop,3] $'OTQZCDGS' .AND. laSegInfo[lnLoop,1] = 'M'
    llSegBrow = .T.
    EXIT
  ELSE
    llSegBrow = .F.
  ENDIF
ENDFOR

IF llCanCont

  *--Open tables required by multi segment browse.
  IF llSegBrow
    IF !USED('icsegval')
      *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [Start]
      *USE (oAriaApplication.DataDir+'icsegval') IN 0
      =gfOpenTable(oAriaApplication.DataDir+'ICSEGVAL', '', 'SH')
      *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [End]
      llSegvalUse = .T.
    ENDIF
    SELECT icsegval
    SET ORDER TO Tag segval
    IF !USED('scale')
      USE (oAriaApplication.DataDir+'scale') IN 0
      llScaleUse = .T.
    ENDIF
    SELECT Scale
    SET ORDER TO TAG scale
    IF !USED('Codes')
      USE (oAriaApplication.DataDir+'Codes') IN 0
      llCodesUse = .T.
    ENDIF
    SELECT Codes
    SET ORDER TO TAG ccode_no
  ENDIF


  IF llStyleData
    lcSegVal = STYLE.STYLE
  ENDIF


  IF llCanCont
    DO CASE
      CASE llSegBrow = .F.
        lcStSeason=IIF(lcMajorOrNon = 'N' , '' , '*')
        llMajor = IIF(ALLTRIM(lcMajorOrNon) = 'M', .T. , .F.)
        *--Call STYBROW proceedure instead of STYLEBR.SPX
        lnSegWidth = lfStyInfo('M')

        *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
        *DO STYBROW WITH lcMajor,lcNonMajor ,lcStSeason,.T.,lcMajorOrNon
        llSelected= .F.
        IF  (TYPE('lcCursor')= 'C' AND !EMPTY(lcCursor)) AND (TYPE('lcSelFld')= 'C' AND !EMPTY(lcSelFld))
          llSelected =STYBROW (lcMajor,lcNonMajor ,lcStSeason,.T.,lcMajorOrNon,lcCursor,lcSelFld,lcValidFun )
        ELSE
          DO STYBROW WITH lcMajor,lcNonMajor ,lcStSeason,.T.,lcMajorOrNon
        ENDIF
        *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

        IF lcMajorOrNon='M'
          lcMajor = SUBSTR(lcMajor,1,lnSegWidth)
        ELSE
          IF lcMajorOrNon='N'
            lcMajor = SUBSTR(lcMajor,lnSegWidth + 2,LEN(lcMajor))
          ENDIF
        ENDIF
        IF !EMPTY(lcStyleTag)
          SELECT STYLE
          SET ORDER TO TAG (lcStyleTag)
        ELSE
          SELECT STYLE
          SET ORDER TO
        ENDIF
        SELECT (lnAlias)
        lcReturn = lcMajor
        *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
        IF  (TYPE('lcCursor')= 'C' AND !EMPTY(lcCursor)) AND (TYPE('lcSelFld')= 'C' AND !EMPTY(lcSelFld))
          lcReturn = llSelected
        ENDIF
        *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]


      CASE llSegBrow = .T.
        *********************

        DO FORM (oAriaApplication.ScreenHome+"IC\stylebr.scx") WITH lcMajorOrNon,llUseColor
        *DO STYLEBR.SPX

        *********************

    ENDCASE
  ENDIF
  *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [Start]
*!*	  FOR lnCount = 1 TO ALEN(laOpenFiles,1)
*!*	    IF laOpenFiles[lnCount,2] AND USED(laOpenFiles[lnCount,1])
*!*	      USE IN (laOpenFiles[lnCount,1])
*!*	    ELSE
*!*	      SELECT (laOpenFiles[lnCount,1])
*!*	      SET FILTER TO &laOpenFiles[lnCount,3]
*!*	      IF !EMPTY(laOpenFiles[lnCount,4])
*!*	        SET ORDER TO TAG (laOpenFiles[lnCount,4])
*!*	      ELSE
*!*	        SET ORDER TO
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDFOR
  FOR lnFileCount = 1 TO ALEN(laOpenFiles,1)
    IF laOpenFiles[lnFileCount,2] AND USED(laOpenFiles[lnFileCount,1])
      =gfCloseTable(laOpenFiles[lnFileCount,1])      
    ELSE
      SELECT (laOpenFiles[lnFileCount,1])
      SET FILTER TO &laOpenFiles[lnFileCount,3]
      IF !EMPTY(laOpenFiles[lnFileCount,4])
        SET ORDER TO TAG (laOpenFiles[lnFileCount,4])
      ELSE
        SET ORDER TO
      ENDIF
    ENDIF
  ENDFOR
  *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [End]
ELSE     && NOT llCanCont
*  =gfModalGen('QRM00276B00000','DIALOG',lcEmptyErr)
ENDIF

*--Close tables that was opened by this function.
IF llSegBrow
  IF llSegvalUse AND USED('icsegval')
    *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [Start]
    *USE IN icsegva
    =gfCloseTable('ICSEGVAL')
    *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [END]
  ENDIF
  IF llScaleUse AND USED('scale')
    USE IN scale
  ENDIF
  IF llCodesUse AND USED('Codes')
    USE IN Codes
  ENDIF
ENDIF


IF llStyleUse AND USED('STYLE')
  USE IN STYLE
ELSE
  SELECT STYLE
  IF !EMPTY(lcStyleTag)
    SET ORDER TO TAG (lcStyleTag)
  ELSE
    SET ORDER TO
  ENDIF

  IF EMPTY(lcReturn) AND BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN lcReturn
***END...



*!*************************************************************
*! Name      : lfStyInfo
*! Developer : Hossam eletreby
*! Date      : 06/02/97
*! Purpose   : Biuld array for segment information.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : array elements as :
*!-1 MAJOR,NONMAJOR
*!-2 SEGMENT NUMBER
*!-3 SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
*!-4 SEGMENT START
*!-5 SEGMENT SIZE
*!-6 SEGMENT FILE
*!-7 SEGMENT FILE TAG
*!-8 SEGMENT FILE KEY
*!-9 SEGMENT FILE FIELD
*!*************************************************************
*! Example   : =lfStyInfo('M')
*!*************************************************************

FUNCTION lfStyInfo
PARAMETER lcSegment
PRIVATE lnAlias    , lnMajSeg   , lnNonSeg , lcItemTl   , lcMjrTtl   , lcNMjrTl , lcMjrPct,;
                     lcNMjrPt   , lcIMjrPt , lnstylewid , lncolorwid , lcSepart

lnAlias=SELECT()
llStruOp=gfOpenFile(oAriaApplication.DataDir+'ICISTRU','Segno','SH')
IF !SEEK('U','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF
IF USED('ICISTRU') AND llStruOp
  USE IN ICISTRU
ENDIF


*DIME laMajSeg[1,1]
*=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

*FOR lnCnt=1 TO ALEN(laMajSeg,1)
FOR lnCnt=1 TO ALEN(laSeg,1)

  DIME laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M'
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N'
    ELSE
      laSegInfo[lnCnt,1] = ' '
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  *laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,3]=laSeg[lnCnt,2]
  *laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,4]=laSeg[lnCnt,3]
  *laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,5]=laSeg[lnCnt,1]
  *laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,6]=laSeg[lnCnt,4]
  *laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL',IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  laSegInfo[lnCnt,7]=IIF(laSeg[lnCnt,2]$'FOQT','SEGVAL',IIF(laSeg[lnCnt,2]='S','SCALE','Idrltfname'))
  *laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),IIF(laMajSeg[lnCnt,1]='S','S','NN'))
  laSegInfo[lnCnt,8]=IIF(laSeg[lnCnt,2]$'FOQT',STR(lnCnt,1),IIF(laSeg[lnCnt,2]='S','S','NN'))

  IF laSeg[lnCnt,2]$'CZDGF'
    DO CASE
      CASE laSeg[lnCnt,2]='C'
        lcCdTyp = 'COLOR     '
      CASE laSeg[lnCnt,2]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSeg[lnCnt,2]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSeg[lnCnt,2]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSeg[lnCnt,2]='F'
        lcCdTyp  =''
    ENDCASE
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laSeg[lnCnt,2]$'FOQT',"ciSegVal",IIF(laSeg[lnCnt,2]='S',"Scale","cCode_no"))
ENDFOR

SELECT(lnalias)
RETURN IIF(lcSegment='M',lnstylewid , lncolorwid)


*!*************************************************************
FUNCTION LFEMPTY
PARAMETERS lcValue,lcMajORNon
PRIVATE lnStartSeg,lnEndSeg,lnPosition,lcValue,lcMajORNon,llEmpty
lnStartSeg = IIF(lcMajORNon= 'N',lnNonMajStart,1)
lnEndSeg   = IIF(lcMajORNon= 'M',lnMajorEnd,lnNonMajEnd)
lnPosition = laSeg[lnStartSeg,3] - 1
llEmpty = .T.
IF !EMPTY(lcValue)
  FOR lnCount = lnStartSeg TO lnEndSeg
    IF !EMPTY(SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1]))
      llEmpty = .F.
      EXIT
    ENDIF
  ENDFOR
ENDIF
RETURN llEmpty


*!*************************************************************
FUNCTION lfvSeg
PARAMETERS lcValue,lcMajORNon
PRIVATE lnStartSeg,lnEndSeg,lnPosition,lcValue,lcMajORNon,llEmpty
lnStartSeg = IIF(lcMajORNon= 'N',lnNonMajStart,1)
lnEndSeg   = IIF(lcMajORNon= 'M',lnMajorEnd,lnNonMajEnd)
lnPosition = laSeg[lnStartSeg,3] - 1
IF !EMPTY(lcValue)
  FOR lnCount = lnStartSeg TO lnEndSeg
    lcAlias = laSeg[lnCount,4]
    SELECT (lcAlias)
    lcExpr = SYS(14,VAL(SYS(21)))
    *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [Start]
    *IF (laSeg[lnCount,2] # 'F' OR EMPTY(SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1])))AND !SEEK(EVAL(laSeg[lnCount,5])+SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1]))
    IF (laSeg[lnCount,2] # 'F' OR EMPTY(SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1])))AND !gfSeek(EVAL(laSeg[lnCount,5])+SUBSTR(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1]))    
    *! B610154,1 SAB 12/04/2012 Fix Error Selecting Style from Style Summary Option Grid [End]    
      GO TOP
      lcValue = STUFF(lcValue,laSeg[lnCount,3]-lnPosition,laSeg[lnCount,1],;
      PADR(EVAL(laSeg[lnCount,9]),laSeg[lnCount,1]))
    ENDIF
  ENDFOR
ENDIF
RETURN lcValue



*!*************************************************************************
*! Name      : LFINCSEG
*! Developer : Hesham El-Sheltawi
*! Date      : 07/21/97
*! Purpose   :
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfBrowCod
*!*************************************************************************
*: Passed parameters  : lnincseg
*:*************************************************************************
*! Returns   :
*:*************************************************************************
FUNCTION lfIncSeg
PARAMETERS lnIncSeg
lnActSeg = lnActSeg + lnIncSeg
lnActSeg = IIF(lnActSeg>lnEndSeg,lnStartSeg,IIF(lnActSeg<lnStartSeg,lnEndSeg,lnActSeg))


