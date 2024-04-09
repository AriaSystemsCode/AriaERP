*!**************************************************************************
*! Name      : FLOMAIN.PRG
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   : Flower By zoe Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200809,1
*!**************************************************************************
*! Modifications
*! C200900,1 MMT 12/02/2007 Add Style on the fly Customization[T20070920.0032]
*! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(T20080214.0006)
*! C201210,1 MMT 01/12/2010 Add New Triggers to Enable configuration usage[T20090421.0032]
*! C201210,2 MMT 01/21/2010 Add New Triggers to Scan Barcode For FLO09 in Manul PL[T20090421.0032]
*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen[T20090421.0032]
*! C201211,2 HES 01/25/2010 Handle Scanning Barcod to be per piece[T20090421.0032]
*! C201211,3 HES 01/25/2010 Allow the over recieving [T20090421.0032]
*! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[T20090421.0032]
*! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[T20090421.0032]
*! E303165,1 04/06/2012 HIA, Don't rload query and enable filter in view mode  [T20120424.0007]
*! B610061,1 MMT 08/28/2012 Cannot add new style if selected to add default configuration[T20120625.0019]
*! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfCRTTEMP
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/05/2007
*! Purpose   : Create temp file .
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfCRTTEMP

*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine+'A'

CREATE CURSOR (lcTrgCursr) (lcTmpFle C(8),llExtLines L(1),llNShowScr L(1))

*Store .F. as a Default Value for llNShowScr instead of .T.
INSERT INTO (lcTrgCursr) (lcTmpFle,llExtLines,llNShowScr) VALUES (gfTempName(),.F.,.F.)

lcTmpCode  = &lcTrgCursr..lcTmpFle

=gfOpenTable(oAriaApplication.DataDir +'SOCODES',oAriaApplication.DataDir +'SOCODES','SH')
SELECT SOCODES
=AFIELDS(laFileStru)
=gfCrtTmp(lcTmpCode,@laFileStru,[cordtype+order+STR(lineno,6)],lcTmpCode)

*-- END OF FUNCTION lfCRTTEMP

*!**************************************************************************
*! Name      : lfGetData
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/05/2007
*! Purpose   : Get data from SoCodes file For an Exist Order.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfGetData

PRIVATE lnOldAlS
STORE 0 TO lnOldAlS

lnOldAlS = SELECT(0)

*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine+'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle

SELECT SOCODES
*=SEEK(_screen.ActiveForm.Parent.lcOrdType+laData[1])
=SEEK(_SCREEN.ACTIVEFORM.PARENT.lcOrdType+ordhdr.ORDER)
SCAN REST WHILE cordtype+ORDER+STR(LINENO,6) = _SCREEN.ACTIVEFORM.PARENT.lcOrdType+ordhdr.ORDER
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpCode) FROM MEMVAR
ENDSCAN

SELECT(lnOldAlS)

*-- END OF FUNCTION lfGetData
*!**************************************************************************
*! Name      : lfDOFORM
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/05/2007
*! Purpose   : Function to Call the Flowers By Zoe Operation Manager Screen
*!**************************************************************************
*! Example   : lfDoForm()
*!**************************************************************************
*! C200809,1 07/12/2007 07/12/2007
*!**************************************************************************
FUNCTION lfDOFORM
&& if the screen Displayed from the Option Menu,that mean it's an exist Line
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine+'A'
REPLACE &lcTrgCursr..llExtLines WITH .T.
=_SCREEN.ACTIVEFORM.PARENT.mDoTrigger(PADR('DISPSCR',10))
*!**************************************************************************
*! Name      : lfDispScr
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/05/2007
*! Purpose   : Function to Display the Flowers By Zoe Operation Manager Screen
*!**************************************************************************
*! Example   : lfDispScr()
*!**************************************************************************
*! C200809,1 HIA 07/12/2007
*!**************************************************************************
FUNCTION lfDispScr

PARAMETER llExtLin

DECLARE laCodeInfo[3,10] , laSblName[1,2] , laSblColor[1,2] , laTrmColor[1,2]
STORE '' TO laCodeInfo , laSblName , laSblColor , laTrmClr
STORE 1 TO lnSblName , lnSblColor , lnTrmColor
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)

*_screen.ActiveForm.parent.oFormEnvironment.lcOrdLine
*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = loFormSet.oFormEnvironment.lcOrdLine+'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle
IF &lcTrgCursr..llNShowScr
  REPLACE &lcTrgCursr..llNShowScr WITH .F.
  RETURN
ENDIF

DO FORM (oAriaApplication.ScreenHome + oAriaApplication.ActiveModuleID +"\SOCODES.SCX")

*-- END OF FUNCTION lfDispScr

*!**************************************************************************
*! Name      : lfCodesOK
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/05/2007
*! Purpose   : Valid function of the push button pbOk in SoCodes Screen.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfCodesOK

PARAMETERS lCSblName, lCSblColor, lCTrmColor

PRIVATE lnRecNo,lnOldAlS
STORE 0 TO lnRecNo,lnOldAlS
LOCAL lcOrdLine

IF TYPE('loFormSet')='O'
  loFrmSET = loFormSet
ELSE
  loFrmSET = _SCREEN.ACTIVEFORM.PARENT
ENDIF

*
*MMT
*loFormSet.ariaForm1.extendedsizescaleentry1.lctempfile
*lcOrdLine = loFrmSET.oFormEnvironment.lcOrdLine
*lcOrdLine  = IIF(type('loFormSet.oEditRegion')='O',loFormSet.ariaForm1.extendedsizescaleentry1.lctempfile ,loFormSet.oFormEnvironment.lcOrdLine)
*lcOrdLine1 = IIF(type('loFormSet.oEditRegion')='O',loFormSet.oEditRegion.DetailFile ,loFormSet.oFormEnvironment.lcOrdLine)
*MMT

lcOrdLine  = loFormSet.oFormEnvironment.lcOrdLine
lnOldAlS = SELECT(0)
lnRecNo  = RECNO(loFormSet.oFormEnvironment.lcOrdLine)
lcTrgCursr = loFormSet.oFormEnvironment.lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle
IF !(&lcTrgCursr..llExtLines)
  SELECT (lcOrdLine)

  *Scan for Style And Lineno To be able to input different Sablon
  *info. for the same style-color but in another line
  *Seek for LINENO <= M.LINENO because if you input a style
  *with 2 dim. M.lineno get with NO. (2) so the last line only has Codes.
  SCAN FOR (STYLE = LEFT(&lcOrdLine..STYLE,LEN(ALLTRIM(&lcOrdLine..STYLE))-3) AND LINENO <= &lcOrdLine..LINENO)

    SELECT (lcTmpCode)
    *in case we add new lines (&lcTrgCursr..llExtLines=.F.) then we need only to append no update
    IF !SEEK(&lcOrdLine..cordtype + &lcOrdLine..ORDER + STR(&lcOrdLine..LINENO,6))

      APPEND BLANK
      REPLACE cordtype    WITH &lcOrdLine..cordtype      ,;
        ORDER       WITH &lcOrdLine..ORDER         ,;
        LINENO      WITH &lcOrdLine..LINENO        ,;
        CSblName    WITH ALLTRIM(lCSblName) ,;
        CSblColor   WITH ALLTRIM(lCSblColor),;
        CTRMCOLOR   WITH ALLTRIM(lCTrmColor)
    ENDIF
    SELECT (lcOrdLine)
  ENDSCAN
ELSE
  SELECT (lcTmpCode)
  IF SEEK(&lcOrdLine..cordtype + &lcOrdLine..ORDER + STR(&lcOrdLine..LINENO,6),lcTmpCode)

    REPLACE  CSblName    WITH ALLTRIM(lCSblName)  ,;
      CSblColor   WITH ALLTRIM(lCSblColor) ,;
      CTRMCOLOR   WITH ALLTRIM(lCTrmColor)
  ELSE
    APPEND BLANK
    REPLACE cordtype    WITH &lcOrdLine..cordtype      ,;
      ORDER       WITH &lcOrdLine..ORDER         ,;
      LINENO      WITH &lcOrdLine..LINENO        ,;
      CSblName    WITH ALLTRIM(lCSblName) ,;
      CSblColor   WITH ALLTRIM(lCSblColor),;
      CTRMCOLOR   WITH ALLTRIM(lCTrmColor)

  ENDIF
ENDIF
SELECT(lnOldAlS)
IF lnRecNo <> 0 AND BETWEEN(lnRecNo,1,RECCOUNT(lcOrdLine))
  GOTO lnRecNo IN (lcOrdLine)
ENDIF
REPLACE &lcTrgCursr..llExtLines WITH .F.
REPLACE &lcTrgCursr..llNShowScr  WITH .F.
*-- END OF FUNCTION lfCodesOK


*!**************************************************************************
*! Name      : lfCodesSav
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   : Save records from the temp file to SoCodes Files.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfCodesSav

PRIVATE lnOldAlS , lnLineNo
STORE 0 TO lnOldAlS , lnLineNo

lnOldAlS = SELECT(0)
*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle

SELECT (lcTmpCode)
REPLACE ALL ORDER WITH ordhdr.ORDER

SCAN
  SCATTER MEMVAR

  *Stop the Second Two Lines because we'll get the LineNo
  *from temp file lcTmpCode not as counter.

  SELECT SOCODES
  IF SEEK(m.cordtype + m.Order + STR(m.LINENO,6))
    GATHER MEMVAR
  ELSE
    INSERT INTO SOCODES FROM MEMVAR
  ENDIF
ENDSCAN

SELECT(lnOldAlS)
*-- END OF FUNCTION lfCodesSav

*!**************************************************************************
*! Name      : lfAddMPad
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   :
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfAddMPad

*--check if the option pad is already defined on the sysmenu
PRIVATE llFound

STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', lnCount)) = 'Options'
    llFound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llFound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  lnBarNo = CNTBAR('_INQURYPOP') + 1

  *-- Skip The New BAR if this is not the detail Folder or this not ADD Or EDIT Mode
  *-- Or there's No lines

  *DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Modify Sablon Information' SKIP FOR ;
  *           (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline)) OR laScrMode[2]
  DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Modify Sablon Information' SKIP FOR _SCREEN.ACTIVEFORM.PARENT.mDoTrigger(PADR('GTSKIP',10))

  *1* the .h file
  *2* EOF() can not used because I can not find the detail alias

  *lcTrgCursr = _screen.ActiveForm.parent.oFormEnvironment.lcOrdLine+'A'
  *IF USED(lcTrgCursr)
  *  REPLACE &lcTrgCursr..llExtLines WITH .T.
  *ENDIF
  ON SELECTION BAR lnBarNo OF _INQURYPOP _SCREEN.ACTIVEFORM.PARENT.mDoTrigger(PADR('DOFORM',10))


ENDIF
RETURN

*-- END OF FUNCTION lfAddMPad.
*!**************************************************************************
*! Name      : lfGtSkip
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   : Change llExtLines to .T. to know that user didn't use ExtScr.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfGtSkip

RETURN     (IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
  (_SCREEN.ACTIVEFORM.ariapageframe1.ACTIVEPAGE<>2) .OR. ;
  (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='A' AND EOF(_SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine)) .OR. ;
  (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='V') ;
  ,.T.))

*!**************************************************************************
*! Name      : lfChngPar
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   : Change llExtLines to .T. to know that user didn't use ExtScr.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfChngPar
PRIVATE lnOldAlS
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine + 'A'

REPLACE &lcTrgCursr..llExtLines WITH .T.
SELECT(lnOldAlS)

*-- END OF FUNCTION lfChngPar.
*!**************************************************************************
*! Name      : lfNShowScr
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 07/12/2007
*! Purpose   : Change llExtLines to .T. to know that user didn't use ExtScr.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfNShowScr
PRIVATE lnOldAlS
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)

*lcTrgCursr = lcOrdLine + 'A'
lcTrgCursr = _SCREEN.ACTIVEFORM.PARENT.oFormEnvironment.lcOrdLine + 'A'
REPLACE &lcTrgCursr..llNShowScr WITH .T.
SELECT(lnOldAlS)
*-- END OF FUNCTION lfNShowScr.

*!*	*!**************************************************************************
*!*	*! Name      : lfDispScr
*!*	*! Developer : Hassan Ibrahim Ali [HIA]
*!*	*! Date      : 07/05/2007
*!*	*! Purpose   : Function to Display the Flowers By Zoe Operation Manager Screen
*!*	*!**************************************************************************
*!*	*! Example   : lfDispFEXT()
*!*	*!**************************************************************************
*!*	*! C200809,1 HIA 07/12/2007
*!*	*!**************************************************************************
*!*	FUNCTION lfDispFEXT

*!*	PARAMETER llExtLin

*!*	DECLARE laCodeInfo[3,10] , laSblName[1,2] , laSblColor[1,2] , laTrmColor[1,2]
*!*	STORE '' TO laCodeInfo , laSblName , laSblColor , laTrmClr
*!*	STORE 1 TO lnSblName , lnSblColor , lnTrmColor
*!*	STORE 0 TO lnOldAlS
*!*	lnOldAlS = SELECT(0)

*!*	*_screen.ActiveForm.parent.oFormEnvironment.lcOrdLine
*!*	*lcTrgCursr = lcOrdLine + 'A'
*!*	lcTrgCursr = loFormSet.oEditRegion.DetailFile+'A'
*!*	lcTmpCode  = &lcTrgCursr..lcTmpFle
*!*	IF &lcTrgCursr..llNShowScr
*!*	  REPLACE &lcTrgCursr..llNShowScr WITH .F.
*!*	  RETURN
*!*	ENDIF

*!*	DO FORM (oAriaApplication.ScreenHome + oAriaApplication.ActiveModuleID +"\SOCODES.SCX")

*!*	*-- END OF FUNCTION lfDispFEXT

*!**************************************************************************
*! C200900,1 MMT 12/02/2007 Add Style on the fly Customization[Start]
*!**************************************************************************
*! Name      : lfADDSTYCLR
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/02/2007
*! Purpose   : Add Style Color on the Fly
*!**************************************************************************
FUNCTION lfADDSTYCLR

lcStyValue  = loFormset.ariaform1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.VALUE
lnmajlength = LEN(gfItemMask('PM'))
STORE  0 TO lnClrLen ,lnClrPos,lnScalPos

DECLARE laItemSeg[1]
PRIVATE lnCount
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR


FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScalPos= laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR



lcMajValue = SUBSTR(lcStyValue  ,1,lnmajlength)
lcColorVAlue = SUBSTR(lcStyValue  ,lnClrPos,lnClrLen)

IF EMPTY(lcColorVAlue)
  RETURN .F.
ENDIF

llColorFound = .F.
lcAlias = SELECT(0)
lcOldOrd = ORDER('Style')
IF !SEEK(PADR(lcMajValue,19) ,'Style','CSTYLE')
  SELECT(lcAlias )
  RETURN .F.
ELSE
  SELECT STYLE
  SET ORDER TO STYLE   && STYLE
  =SEEK(lcMajValue)
  SCAN REST WHILE STYLE =lcMajValue FOR SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) =lcColorVAlue
    llColorFound = .T.
    EXIT
  ENDSCAN
  IF !llColorFound
    IF  gfModalGen("QRM52027B32000","ALERT",lcColorVAlue) = 1
      IF !SEEK("N"+"COLOR     "+lcColorVAlue,'Codes','CCODE_NO')
        =gfModalGen("INM42155B00000","ALERT",lcColorVAlue)
        SELECT STYLE
        SET ORDER TO (lcOldOrd)
        SELECT(lcAlias )
        RETURN .F.
      ELSE
        * lcStyle  Add New color lines to Style file
        SELECT STYLE
        =SEEK(lcMajValue)
        llExtSize = gfGetMemVar('M_USEEXSSC')
        lcFirstStyle = IIF(llExtSize ,SUBSTR(STYLE.STYLE,1,lnScalPos-1),STYLE.STYLE)

        SELECT * FROM STYLE WHERE STYLE =lcFirstStyle INTO CURSOR 'TempSty'

        SELECT * FROM Stydye WHERE STYLE+CWARECODE+DYELOT=lcFirstStyle INTO CURSOR 'TempStyDye'

        SELECT 'TempSty'
        SCAN
          SCATTER MEMO MEMVAR
          lcOldClr =SUBSTR(m.Style,lnClrPos,lnClrLen)
          m.Style = STRTRAN(m.Style,lcOldClr,lcColorVAlue)
          FOR lnI = 1 TO 8
            lcI = ALLTRIM(STR(lnI ,1))
            m.Stk&lcI. = 0
            m.ord&lcI. = 0
            m.wip&lcI. = 0
            m.Alo&lcI. = 0
            m.Shp&lcI. = 0
            m.Ret&lcI. = 0
            m.Ra&lcI. = 0
            m.Plan&lcI. = 0
            m.Intrans&lcI. = 0
            m.Nwo&lcI. = 0
          ENDFOR
          m.NSTKVAL = 0
          m.NtotWO = 0
          m.totStk = 0
          m.totOrd = 0
          m.totWIP = 0
          m.totalo = 0
          m.totShp = 0
          m.totRet = 0
          m.totRA = 0
          m.totplan = 0
          m.totintrn = 0
          INSERT INTO 'Style' FROM MEMVAR
          =gfAdd_Info('Style')
        ENDSCAN

        SELECT 'TempStyDye'
        SCAN
          SCATTER MEMO MEMVAR
          lcOldClr =SUBSTR(m.Style,lnClrPos,lnClrLen)
          m.Style = STRTRAN(m.Style,lcOldClr,lcColorVAlue)
          FOR lnI = 1 TO 8
            lcI = ALLTRIM(STR(lnI ,1))
            m.Stk&lcI. = 0
            m.ord&lcI. = 0
            m.wip&lcI. = 0
            m.Alo&lcI. = 0
            m.Shp&lcI. = 0
            m.Ret&lcI. = 0
            m.Ra&lcI. = 0
            m.Plan&lcI. = 0
            m.Intrans&lcI. = 0
            m.Nwo&lcI. = 0
          ENDFOR
          m.NtotWO = 0
          m.NSTKVAL = 0
          m.totStk = 0
          m.totOrd = 0
          m.totWIP = 0
          m.totalo = 0
          m.totShp = 0
          m.totRet = 0
          m.totRA = 0
          m.totplan = 0
          m.totintrn = 0
          INSERT INTO 'StyDye' FROM MEMVAR
          =gfAdd_Info('StyDye')
        ENDSCAN

        SELECT STYLE
        SET ORDER TO (lcOldOrd)
        SELECT(lcAlias )
        RETURN .T.
      ENDIF
    ELSE
      SELECT STYLE
      SET ORDER TO (lcOldOrd)
      SELECT(lcAlias )
      RETURN .F.
    ENDIF
  ELSE
    SELECT STYLE
    SET ORDER TO (lcOldOrd)
    SELECT(lcAlias )
    RETURN .F.
  ENDIF
  SELECT STYLE
  SET ORDER TO (lcOldOrd)
  SELECT(lcAlias )
ENDIF

*!**************************************************************************
*! Name      : lfChkSabDef
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/02/2007
*! Purpose   : Get the lineNo of the first style
*!**************************************************************************
FUNCTION lfChkSabDef
PARAMETERS lnCurLinNo

STORE 0 TO lnRetLine
lcStyValue  = loFormset.ariaform1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.VALUE

lnMajLength = LEN(gfItemMask('PM'))

lcMajValue = SUBSTR(lcStyValue  ,1,lnmajlength)

lcDetFile = loFormset.ariaform1.ariapageframe1.page2.ariaeditregion1.detailfile

lnCurrRec = RECNO(lcDetFile)

SELECT(lcDetFile)
SCAN FOR SUBSTR(STYLE ,1,lnMajLength ) = lcMajValue  AND LINENO < lnCurLinNo
  lnRetLine = &lcDetFile..LINENO
  EXIT
ENDSCAN
IF BETWEEN(lnCurrRec,1,RECCOUNT(lcDetFile))
  GO RECORD  lnCurrRec IN (lcDetFile)
ENDIF
RETURN lnRetLine
*! C200900,1 MMT 12/02/2007 Add Style on the fly Customization[End]

*! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(Start)
*!**************************************************************************
*! Name      : lfUPDCODLIN
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/02/2007
*! Purpose   : Update lineNo in the SOCODES
*!**************************************************************************
FUNCTION lfUPDCODLIN
lcTrgCursr = loFormSet.oFormEnvironment.lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle
IF SEEK(m.Cordtype+SPACE(6)+STR(EVALUATE(loFormSet.oFormEnvironment.lcOrdLine+'.Lineno'),6),lcTmpCode)
  REPLACE LINENO WITH m.Lineno IN (lcTmpCode)
ENDIF
*! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(End)

*! C201210,1 MMT 01/12/2010 Add New Triggers to Enable configuration usage[Start]
*!**************************************************************************
*! Name      : lfENBCONFG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/12/2009
*! Purpose   : Enable Config. check box in Style Screen
*!**************************************************************************
FUNCTION lfENBCONFG
IF loFormSet.llUseStyleConfiguration
  IF loFormSet.ActiveMode $ 'AE'
    loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.ENABLED = .T.
  ELSE
    loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.ENABLED = .F.
  ENDIF
ELSE
  loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.ENABLED = .F.
ENDIF


*!**************************************************************************
*! Name      : lfCONFGMNU
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/12/2009
*! Purpose   : add new options in Style Screen
*!**************************************************************************
FUNCTION lfCONFGMNU
IF TYPE('_Screen.ActiveForm.Parent.llGenAut') = 'U'
  _SCREEN.ACTIVEFORM.PARENT.ADDPROPERTY('llGenAut',.T.)
ENDIF
LOCAL lnCntBar
lnCntBar = CNTBAR('_lPopOpt')+1
*DEFINE BAR lnCntBar  OF _lPopOpt PROMPT 'Generate Configuration code Automatically' SKIP FOR !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode = "V") OR (_SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfStyleInfo.ACTIVEPAGE <> 2)
DEFINE BAR lnCntBar  OF _lPopOpt PROMPT 'Generate Configuration code Automatically' SKIP ;
  FOR !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode = "V") OR (_SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfStyleInfo.ACTIVEPAGE <> 2)  OR !_SCREEN.ACTIVEFORM.PARENT.ariaForm1.pgfStyleInfo.page1.chkDyelot.VALUE
SET MARK OF BAR lnCntBar  OF _lPopOpt TO ;
  TYPE('_Screen.ActiveForm.Parent.llGenAut')='L' AND (_SCREEN.ACTIVEFORM.PARENT.llGenAut)
ON SELECTION BAR lnCntBar OF _lPopOpt DO lfGenAutMan

lnCntBar = CNTBAR('_lPopOpt')+1

*DEFINE  BAR lnCntBar OF _lPopOpt PROMPT 'Generate New Configuration ' SKIP FOR ;
!(_Screen.ActiveForm.Parent.ActiveMode = 'V') OR  (_SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfStyleInfo.ACTIVEPAGE <> 2)
DEFINE  BAR lnCntBar OF _lPopOpt PROMPT 'Generate New Configuration ' SKIP FOR ;
  !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode = 'V') OR  (_SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfStyleInfo.ACTIVEPAGE <> 2)   OR !_SCREEN.ACTIVEFORM.PARENT.ariaForm1.pgfStyleInfo.page1.chkDyelot.VALUE

ON SELECTION BAR lnCntBar OF _lPopOpt DO lfGenConf


lnCntBar = CNTBAR('_lPopOpt')+1
DEFINE  BAR lnCntBar OF _lPopOpt PROMPT 'Inquire a Configuration by Barcode' SKIP FOR ;
  !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'SV')
ON SELECTION BAR lnCntBar OF _lPopOpt DO lfScanConfg
*!**************************************************************************
*! Name      : lfGenAutMan
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/12/2009
*! Purpose   : Generate config. auto. in Style Screen
*!**************************************************************************
FUNCTION lfGenAutMan
lnBar = BAR()
loParentForm = _SCREEN.ACTIVEFORM.PARENT
loParentForm.llGenAut = !loParentForm.llGenAut
llGenAut = loParentForm.llGenAut
SET MARK OF BAR lnBar OF _lPopOpt TO ;
  TYPE('_Screen.ActiveForm.Parent.llGenAut')='L' AND (_SCREEN.ACTIVEFORM.PARENT.llGenAut)

*!**************************************************************************
*! Name      : lfGenConf
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/12/2009
*! Purpose   : Generate config. in Style Screen
*!**************************************************************************
FUNCTION lfGenConf
lcAlias = SELECT(0)

loParentForm = _SCREEN.ACTIVEFORM.PARENT
llGenAut = loParentForm.llGenAut
LOCAL lcWareCode
lcWareCode = loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALUE
lcWareOld = loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALUE
lcDyeOld = loParentForm.Ariaform1.pgfStyleInfo.page2.cboDyelot.VALUE

IF llGenAut
  lcCongSequence = gfSequence(PADR('CONFIG',9))
  IF !USED('Style_Conf')
    =gfOpenTable('Style','Style','Sh','Style_Conf')
  ENDIF
  IF !USED('Stydye_Conf')
    =gfOpenTable('Stydye','STYDYE','Sh','Stydye_Conf')
  ENDIF
  IF loParentForm.llAllColors
    SELECT 'Style_Conf'
    =gfSeek(IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey))
    SCAN REST WHILE STYLE =IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey)
      lcStyleClr = Style_Conf.STYLE
      IF lcWareCode  = "******"
        SELECT 'Stydye_Conf'
        =gfSeek(PADR(lcStyleClr,LEN(Stydye_Conf.STYLE)),'Stydye_Conf')
        SCAN REST WHILE STYLE+CWARECODE+DYELOT = PADR(lcStyleClr,LEN(Stydye_Conf.STYLE)) FOR !EMPTY(CWARECODE) AND EMPTY(DYELOT)
          DO gpAdStyWar WITH lcStyleClr  ,  PADL(lcCongSequence ,10,'0'),Stydye_Conf.CWARECODE
        ENDSCAN
      ELSE
        IF !gfSEEK(PADR(lcStyleClr,LEN(Stydye_Conf.STYLE))+PADR(lcWareCode ,6),'Stydye_Conf')
          IF gfModalGen('QRM40012B40002','ALERT',TRIM(lcStyleClr)+'|'+TRIM(lcWareCode)) = 1
            DO gpAdStyWar WITH lcStyleClr,SPACE(10),lcWareCode
            DO gpAdStyWar WITH lcStyleClr, PADL(lcCongSequence ,10,'0') ,lcWareCode
          ENDIF
        ELSE
          DO gpAdStyWar WITH lcStyleClr  ,  PADL(lcCongSequence ,10,'0'),lcWareCode
        ENDIF
      ENDIF
    ENDSCAN
  ELSE
    *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[Start]
    IF loParentForm.llAllScales
      IF lcWareCode  = "******"
        SELECT 'Stydye_Conf'
        =gfSeek(ALLTRIM(loParentForm.lcStyleKey),'Stydye_Conf')
        SCAN REST WHILE STYLE+CWARECODE+DYELOT = ALLTRIM(loParentForm.lcStyleKey) FOR !EMPTY(CWARECODE) AND EMPTY(DYELOT)
          DO gpAdStyWar WITH Stydye_Conf.STYLE,  PADL(lcCongSequence ,10,'0'),Stydye_Conf.CWARECODE
        ENDSCAN
      ELSE
        SELECT Style_Conf
        =gfSeek(ALLTRIM(loParentForm.lcStyleKey))
        SCAN REST WHILE STYLE = ALLTRIM(loParentForm.lcStyleKey)
          IF !gfSEEK(Style_Conf.STYLE+PADR(lcWareCode ,6),'Stydye_Conf')
            IF gfModalGen('QRM40012B40002','ALERT',TRIM(Style_Conf.STYLE)+'|'+TRIM(lcWareCode)) = 1
              DO gpAdStyWar WITH Style_Conf.STYLE,SPACE(10),lcWareCode
              DO gpAdStyWar WITH Style_Conf.STYLE , PADL(lcCongSequence ,10,'0') ,lcWareCode
            ENDIF
          ELSE
            DO gpAdStyWar WITH Style_Conf.STYLE ,  PADL(lcCongSequence ,10,'0'),lcWareCode
          ENDIF
        ENDSCAN
      ENDIF
    ELSE
      *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[End]
      IF lcWareCode  = "******"
        SELECT 'Stydye_Conf'
        =gfSeek(PADR(loParentForm.lcStyleKey,LEN(Stydye_Conf.STYLE)),'Stydye_Conf')
        SCAN REST WHILE STYLE+CWARECODE+DYELOT = PADR(loParentForm.lcStyleKey,LEN(Stydye_Conf.STYLE)) FOR !EMPTY(CWARECODE) AND EMPTY(DYELOT)
          DO gpAdStyWar WITH loParentForm.lcStyleKey,  PADL(lcCongSequence ,10,'0'),Stydye_Conf.CWARECODE
        ENDSCAN
      ELSE
        IF !gfSEEK(PADR(loParentForm.lcStyleKey,LEN(Stydye_Conf.STYLE))+PADR(lcWareCode ,6),'Stydye_Conf')
          IF gfModalGen('QRM40012B40002','ALERT',TRIM(loParentForm.lcStyleKey)+'|'+TRIM(lcWareCode)) = 1
            DO gpAdStyWar WITH loParentForm.lcStyleKey,SPACE(10),lcWareCode
            DO gpAdStyWar WITH loParentForm.lcStyleKey , PADL(lcCongSequence ,10,'0') ,lcWareCode
          ENDIF
        ELSE
          DO gpAdStyWar WITH loParentForm.lcStyleKey  ,  PADL(lcCongSequence ,10,'0'),lcWareCode
        ENDIF
      ENDIF
      *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[Start]
    ENDIF
    *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[End]
  ENDIF
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Configuration "+PADL(lcCongSequence ,10,'0')+" has been generated")
  SELECT (lcAlias)
  loParentForm.mFillCutSold(loParentForm.lcStyleKey)
  loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALUE = lcWareOld
  loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALID()
  loParentForm.Ariaform1.pgfStyleInfo.page2.cboDyelot.VALUE = lcDyeOld
  loParentForm.Ariaform1.pgfStyleInfo.page2.cboDyelot.VALID()
  RETURN
ELSE
  lcStylePct = gfItemMask("PI")
  lnStyleWid = 19
  *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[Start]
  *!*    IF loParentForm.llAllColors
  *!*      =gfConfgBrw(loParentForm.lcStyMajor,IIF(lcWareCode  = "******",'',lcWareCode),.T.,.T.,'','A',.T.,.T.)
  *!*    ELSE
  *!*      =gfConfgBrw(loParentForm.lcStyleKey,IIF(lcWareCode  = "******",'',lcWareCode),.T.,.T.,'','A',.F.,.T.)
  *!*    ENDIF
  IF loParentForm.llAllColors
    =gfConfgBrw(loParentForm.lcStyMajor,IIF(lcWareCode  = "******",'',lcWareCode),.T.,.T.,'','A',.T.,.T.,loParentForm.llAllScales)
  ELSE
    =gfConfgBrw(loParentForm.lcStyleKey,IIF(lcWareCode  = "******",'',lcWareCode),.T.,.T.,'','A',.F.,.T.,loParentForm.llAllScales)
  ENDIF
  *! C201210,3 MMT 02/10/2010 Fix problem while creating conf. for Style-Color-All Sizes[End]
ENDIF
loParentForm.mFillCutSold(loParentForm.lcStyleKey)
loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALUE = lcWareOld
loParentForm.Ariaform1.pgfStyleInfo.page2.cboLocation.VALID()
loParentForm.Ariaform1.pgfStyleInfo.page2.cboDyelot.VALUE = lcDyeOld
loParentForm.Ariaform1.pgfStyleInfo.page2.cboDyelot.VALID()
SELECT (lcAlias)
*! C201210,1 MMT 01/12/2010 Add New Triggers to Enable configuration usage[End]

*! C201210,2 MMT 01/21/2010 Add New Triggers to Scan Barcode For FLO09 in Manul PL[Start]
*!**************************************************************************
*! Name      : lfADDMNUOP
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/21/2010
*! Purpose   : add Menu Option To Pl Screen
*!**************************************************************************
FUNCTION lfADDMNUOP

IF ALLTRIM(gfGetMemVar('M_STYCNFG')) <> 'Y'
  RETURN
ENDIF

IF TYPE('_Screen.ActiveForm.Parent.llUseConf') = 'U'
  _SCREEN.ACTIVEFORM.PARENT.ADDPROPERTY('llUseConf',.F.)
ENDIF

LOCAL lnCntBar
lnCntBar = CNTBAR('_OPTPOP')+1
DEFINE BAR lnCntBar  OF _OPTPOP PROMPT 'Scan Configuration Barcode' SKIP FOR !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode $ "EA") OR (_SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfPacking.ACTIVEPAGE <> 3)
SET MARK OF BAR lnCntBar  OF _OPTPOP  TO ;
  TYPE('_Screen.ActiveForm.Parent.llUseConf')='L' AND (_SCREEN.ACTIVEFORM.PARENT.llUseConf)
ON SELECTION BAR lnCntBar OF _OPTPOP  DO lfUseConf

*!**************************************************************************
*! Name      : lfUseConf
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/21/2010
*! Purpose   : Validate menu option value
*!**************************************************************************
FUNCTION lfUseConf
lnBar = BAR()
loParentForm = _SCREEN.ACTIVEFORM.PARENT
loParentForm.llUseConf= !loParentForm.llUseConf
SET MARK OF BAR lnBar OF _OPTPOP  TO ;
  TYPE('_Screen.ActiveForm.Parent.llUseConf')='L' AND (_SCREEN.ACTIVEFORM.PARENT.llUseConf)
IF loParentForm.llUseConf
  _SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfPacking.CartonInfo.lblUPC.CAPTION = 'Config. Barcode'
ELSE
  _SCREEN.ACTIVEFORM.PARENT.Ariaform1.pgfPacking.CartonInfo.lblUPC.CAPTION = 'UPC #'
ENDIF

*!**************************************************************************
*! Name      : lfVLDBARCODE
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/21/2010
*:* Purpose  : Valid function to Scanned Barcode entered
*:***************************************************************************
FUNCTION lfVLDBARCODE


IF !INLIST(LASTKEY(),13,6) OR EMPTY(loFormSet.ariaForm1.pgfPacking.cartonInfo.txtUPC.VALUE)
  RETURN
ENDIF

IF !loFormSet.llUseConf
  lfvUPC(loFormSet,loFormSet.ariaForm1.pgfPacking.cartonInfo.txtUPC.VALUE)
  RETURN
ENDIF

lcUpc =   loFormSet.ariaForm1.pgfPacking.cartonInfo.txtUPC.VALUE
PRIVATE lnRecNo,lnPicked,lnOrdered ,lnAlias ,lcOldOrder
lcOldOrder = ''
lnAlias = SELECT(0)
lcUpc = PADR(ALLTRIM(lcUpc),13)
*-- Clear keyboard buffer as the scanner somtimes issue CHR(13) causing any

IF !USED('CONFGUPC')
  =gfOpenTable('CONFGUPC','CONFGUPCUP')
ELSE
  SELECT CONFGUPC
  =gfSetOrder('CONFGUPCUP')
ENDIF

CLEAR TYPEAHEAD


lcStyUpcOrd = ORDER('CONFGUPC')
lcOrdlineOrd = ORDER('ORDLINE')
loFormSet.OrdLine.SetOrder('OrdLinSt')

lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPackNo   = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)

IF !EMPTY(lcUpc)
  IF !gfSEEK(lcUpc,'CONFGUPC')
    *- Message Text   :- UPC XXXX is not found.
    *- Message No.    :- 44065.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Barcode "+lcUpc+" is not found.")
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF
  SELECT OrdLine
  =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+CONFGUPC.STYLE)
  LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
    'O'+lcOrderNo+lcStore+CONFGUPC.STYLE;
    FOR PikTkt = lcPackNo AND DYELOT = CONFGUPC.DYELOT

  IF !FOUND()
    *- Message Text   :- UPC XXX is not found in piktkt# XXX.
    *- Message No.    :- 44066.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000
    IF !EMPTY(lcPackNo)
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Barcode "+lcUpc+" is not found in piktkt# "+lcPackNo)
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Barcode "+lcUpc+" is not found in Order# "+lcOrderNo)
    ENDIF
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF

  llCUpdate  = .T.
  SELECT (loFormSet.lcCtnDtl)
  lnRecNo = RECNO()
  SET KEY TO
  SUM TotQty TO lnPacked FOR cUPc = lcUpc
  SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  SELECT OrdLine
  STORE 0   TO lnPicked,lnOrdered
  =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+CONFGUPC.STYLE)
  SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
      'O'+lcOrderNo+lcStore+CONFGUPC.STYLE FOR PikTkt = lcPackNo AND DYELOT = CONFGUPC.DYELOT
    lnPicked  = lnPicked  + EVAL('PIK'+ALLTRIM(CONFGUPC.SIZE))
    lnOrdered = lnOrdered + EVAL('QTY'+ALLTRIM(CONFGUPC.SIZE))
    IF lnPacked+1 <= lnPicked
      EXIT
    ENDIF
  ENDSCAN

  =loFormSet.STYLE.SEEK(CONFGUPC.STYLE,'Style')
  =loFormSet.SCALE.SEEK('S'+STYLE.SCALE)
  *- Message Text   :- Packed Quantity for Style/Size XXXXX exceeds
  *- Message Text   :- Ordered Quantity. Can not modify the Ordered Quantity?
  *- Message No.    :- 44112.
  *- Buttom Message :- Ok
  *- Buttom Number  :-00000

  IF lnPacked+1 > lnOrdered
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+CONFGUPC.STYLE)
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Packed Quantity for Style/Size/Configuration "+;
      EVALUATE(loFormSet.lcCtnDtl+'.Style')+'/'+EVAL('SCALE.SZ'+ALLTRIM(CONFGUPC.SIZE))+'/'+;
      CONFGUPC.DYELOT +" exceeds Ordered Quantity. Can not modify the Ordered Quantity")
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF

  IF lnPacked > lnPicked
    *- Message Text   :- Packed quantity for ð exceeds picked quantity.
    *- Message Text   :- Do you want to add in the packing list?
    *- Message No.    :- 44073.
    *- Buttom Message :- Ok
    *- Buttom Number  :-00000
    IF gfModalGen('QRM00000B44009',.F.,.F.,.F.,"Packed Quantity for Style/Size/Configuration "+;
        CONFGUPC.STYLE+'/'+EVAL('SCALE.SZ'+ALLTRIM(CONFGUPC.SIZE))+'/'+;
        CONFGUPC.DYELOT +" exceeds picked quantity. Do you want to add in the packing list?") = 2

      loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)

      loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
      lfSetControlSource(loFormSet)
      SELECT(lnAlias)
      RETURN
    ENDIF

    SELECT OrdLine
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+CONFGUPC.STYLE)
    LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
      'O'+lcOrderNo+lcStore+CONFGUPC.STYLE;
      FOR PikTkt = lcPackNo AND DYELOT = CONFGUPC.DYELOT
  ENDIF
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  SELECT OrdLine
  =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+CONFGUPC.STYLE)
  LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
    'O'+lcOrderNo+lcStore+CONFGUPC.STYLE;
    FOR PikTkt = lcPackNo AND DYELOT = CONFGUPC.DYELOT

  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
  SELECT (loFormSet.lcCtnDtl)
  lfvCtnStyDye(loFormSet,.F.,CONFGUPC.STYLE,CONFGUPC.DYELOT,.T.)
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED  = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)

  loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
  SELECT(lnAlias)
ELSE
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED  = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(13)

  loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
  lfSetControlSource(loFormSet)
  SELECT(lnAlias)
  RETURN
ENDIF
SELECT (lnAlias)
lcFrmCnfrm = loFormSet.lcSetConf
SET CONFIRM &lcFrmCnfrm
*!*************************************************************
*! Name      : lfvCtnStyDye
*! Developer : Mariam Mazhar
*! Date      : 01/21/2010
*! Purpose   : Validat lcCtnSty field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfStyBrow,lfChkSty
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnSty()
*!*************************************************************
FUNCTION lfvCtnStyDye
LPARAMETERS loFormSet,llBrowse,lcCtnSty,lcCtnDyelot,llUpcAdd
lnCurAlias = SELECT(0)
SET ORDER TO (loFormSet.lcPackLines) IN (loFormSet.lcPackLines)
SELECT (loFormSet.lcSumPck)
SET FILTER TO
SELECT (loFormSet.lcPckLin)
SET FILTER TO

lnOldRec = RECNO(loFormSet.lcPackLines)

IF llBrowse OR (!EMPTY(lcCtnSty) AND !EMPTY(lcCtnDyelot) AND !SEEK(PADR(lcCtnSty,19)+PADR(lcCtnDyelot,10),loFormSet.lcPackLines))
  llBrowse = .T.
ELSE
  IF !EMPTY(lcCtnSty) AND !llUpcAdd
    lcCtnSty  = EVALUATE(loFormSet.lcPackLines+'.STYLE')
  ENDIF
ENDIF

IF llBrowse
  llBrowse  = .F.
  llNoThing = lfStyDyeBrow(loFormSet)
  lcCtnSty  = IIF(llNoThing,EVALUATE(loFormSet.lcPackLines+'.Style'),SPACE(19))
  lcCtnDyelot= IIF(llNoThing,EVALUATE(loFormSet.lcPackLines+'.DYELOT'),SPACE(10))
ELSE
  IF BETWEEN(lnOldRec,1,RECCOUNT(loFormSet.lcPackLines))
    GO RECORD lnOldRec IN (loFormSet.lcPackLines)
  ENDIF
ENDIF

IF llUpcAdd
  =SEEK(lcCtnSty+lcCtnDyelot+STR(ORDLINE.LINENO,6)+ALLTRIM(CONFGUPC.SIZE),loFormSet.lcPackLines,loFormSet.lcPackLines)
ENDIF

IF EMPTY(lcCtnSty)
  lfSetControlSource(loFormSet)
  RETURN
ENDIF
lcStySz   = EVALUATE(loFormSet.lcPackLines+'.cSizeCod')
lcStyno   = EVALUATE(loFormSet.lcPackLines+'.cSizeno')
lnStyQty  = IIF(llUpcAdd,1,EVALUATE(loFormSet.lcPackLines+'.OQty'))
lnOrdLine = EVALUATE(loFormSet.lcPackLines+'.nOrdLineNo')
lnWgh     = IIF(llUpcAdd,1,EVALUATE(loFormSet.lcPackLines+'.OQty'))*EVALUATE(loFormSet.lcPackLines+'.StyWgh')
lcDyelot  = EVALUATE(loFormSet.lcPackLines+'.Dyelot')
*-- if the style with order record No. is not found in the carton
SELECT (loFormSet.lcCtnDtl)
IF (!SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') ;
    AND lnStyQty <> 0)
  APPEND BLANK
  REPLACE Cart_No WITH EVALUATE(loFormSet.lcCtnHdr+'.Cart_NO'),;
    SzCnt   WITH 1,;
    Br      WITH .T.

  = RLOCK(loFormSet.lcCtnDtl)
  UNLOCK IN (loFormSet.lcCtnDtl)
  llAlowAdd = .T.
  IF SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+SPACE(19),loFormSet.lcCtnDtl)
    REPLACE STYLE      WITH lcCtnSty ,;
      Dyelot     WITH lcDyelot ,;
      SIZE       WITH lcStySz  ,;
      cSizeno    WITH lcStyno  ,;
      Qty        WITH lnStyQty ,;
      Weight     WITH lnWgh    ,;
      nOrdLineNo WITH lnOrdLine,;
      Br         WITH .T.,;
      SzCnt      WITH EVALUATE(loFormSet.lcPackLines+'.SzCnt'),;
      OrgWgh     WITH EVALUATE(loFormSet.lcPackLines+'.StyWgh'),;
      cStatus    WITH "A",;
      TotQty     WITH lnStyQty,;
      TotWeight  WITH lnWgh
    IF loFormSet.ActiveMode = 'E' AND EMPTY(PackLineNo)
      loFormSet.lnLastNo = loFormSet.lnLastNo + 1
      REPLACE PackLineNo  WITH loFormSet.lnLastNo
    ENDIF

    IF llUpcAdd
      REPLACE Cupc WITH lcUpc
    ENDIF
    loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVAL(loFormSet.lcCtnDtl+'.Weight')
    loFormSet.lnPackQty = loFormSet.lnPackQty + EVAL(loFormSet.lcCtnDtl+'.Qty')
    SELECT(loFormSet.lcCtnHdr)
    REPLACE TotPcs WITH TotPcs + EVAL(loFormSet.lcCtnDtl+'.Qty'),;
      TotWgh WITH TotWgh + EVAL(loFormSet.lcCtnDtl+'.Weight'),;
      EMPTY  WITH IIF(TotPcs>0,'N','Y')
    = RLOCK(loFormSet.lcCtnHdr)
    UNLOCK IN (loFormSet.lcCtnHdr)
    SELECT(loFormSet.lcCtnDtl)
  ENDIF
  IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcPckLin,'StySze')
    IF llAlowAdd
      SELECT(loFormSet.lcPckLin)
      REPLACE PQty WITH PQty   + lnStyQty,;
        PWgh WITH PWgh   + lnWgh   ,;
        OQty WITH OrdQty - PQty,;
        cOPen WITH IIF(OQty > 0 ,'T','N')
      REPLACE CtnQty     WITH IIF(!loFormSet.llClrSel AND lSelect,;
        EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
        CtnTotQty  WITH CtnQty,;
        lSelect    WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
        SELECTED   WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)
      = RLOCK(loFormSet.lcPckLin)
      UNLOCK IN (loFormSet.lcPckLin)
    ENDIF
  ENDIF
  SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
  IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+PADR(lcStyNo,3),loFormSet.lcSumPck)
    IF llAlowAdd
      SELECT(loFormSet.lcSumPck)
      REPLACE PTotQty WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
        PTotWgh WITH EVALUATE(loFormSet.lcPckLin+'.PWgh')  ,;
        OTotQty WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
        cOPen   WITH IIF(OTotQty > 0 ,'T','N')
      = RLOCK(loFormSet.lcPckLin)
      UNLOCK IN (loFormSet.lcPckLin)
    ENDIF
  ENDIF
ELSE
  IF (SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') ;
      AND lnStyQty <> 0)
    llAlowAdd = .T.
    SELECT(loFormSet.lcCtnDtl)

    REPLACE STYLE      WITH lcCtnSty ,;
      Dyelot     WITH lcDyelot ,;
      SIZE       WITH lcStySz  ,;
      cSizeno    WITH lcStyno  ,;
      Qty        WITH Qty + lnStyQty ,;
      Weight     WITH Weight + lnWgh    ,;
      nOrdLineNo WITH lnOrdLine,;
      Br         WITH .T.,;
      SzCnt      WITH EVALUATE(loFormSet.lcPackLines+'.SzCnt'),;
      OrgWgh     WITH EVALUATE(loFormSet.lcPackLines+'.StyWgh'),;
      cStatus    WITH "M",;
      TotQty     WITH TotQty+lnStyQty,;
      TotWeight  WITH TotWeight + lnWgh
    IF loFormSet.ActiveMode = 'E' AND EMPTY(PackLineNo)

      loFormSet.lnLastNo = loFormSet.lnLastNo + 1
      REPLACE PackLineNo  WITH loFormSet.lnLastNo
    ENDIF

    IF llUpcAdd
      REPLACE Cupc WITH lcUpc
    ENDIF

    loFormSet.lnPackWgh = loFormSet.lnPackWgh + lnWgh
    loFormSet.lnPackQty = loFormSet.lnPackQty + lnStyQty
    SELECT(loFormSet.lcCtnHdr)
    REPLACE TotPcs WITH TotPcs + lnStyQty ,;
      TotWgh WITH TotWgh + lnWgh    ,;
      EMPTY  WITH IIF(TotPcs>0,'N','Y')

    = RLOCK(loFormSet.lcCtnHdr)
    UNLOCK IN (loFormSet.lcCtnHdr)
    SELECT(loFormSet.lcCtnDtl)

    IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcPckLin,'StySze')
      IF llAlowAdd
        SELECT(loFormSet.lcPckLin)
        REPLACE PQty WITH PQty   + lnStyQty,;
          PWgh WITH PWgh   + lnWgh   ,;
          OQty WITH OrdQty - PQty,;
          cOPen WITH IIF(OQty > 0 ,'T','N')
        REPLACE CtnQty     WITH IIF(!loFormSet.llClrSel AND lSelect,;
          EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
          CtnTotQty  WITH CtnQty,;
          lSelect    WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
          SELECTED   WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)
        = RLOCK(loFormSet.lcPckLin)
        UNLOCK IN (loFormSet.lcPckLin)
      ENDIF
    ENDIF
    SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
    IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+PADR(lcStyNo,3),loFormSet.lcSumPck)
      IF llAlowAdd
        SELECT(loFormSet.lcSumPck)

        REPLACE PTotQty WITH PTotQty + lnStyQty,;
          PTotWgh WITH PTotWgh + lnWgh    ,;
          OTotQty WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
          cOPen   WITH IIF(OTotQty > 0 ,'T','N')
        = RLOCK(loFormSet.lcPckLin)
        UNLOCK IN (loFormSet.lcPckLin)
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF llUpcAdd
  LOCAL lcStyKey,lcCrtnNo,lnTotWeight
  SELECT (loFormSet.lcCtnDtl)
  lcStyKey = STR(CART_NO,4)+STYLE+DYELOT+STR(NORDLINENO,6)+CSIZENO
  lcCrtnNo = STR(CART_NO,4)
  =SEEK(lcCrtnNo)
  lnTotWeight = 0
  SUM TotWeight TO lnTotWeight REST WHILE STR(CART_NO,4)+STYLE+DYELOT+STR(NORDLINENO,6)+CSIZENO = lcCrtnNo
  =SEEK(lcStyKey)
  loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE = lnTotWeight
ENDIF
lfSetControlSource(loFormSet)


loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.VALUE = lcCtnSty
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbConfiguration.VALUE = lcDyelot
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbSize.VALUE =lcStySz
=lfwCtnDtlBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfStyDyeBrow
*! Developer : Mariam Mazhar
*! Date      : 01/21/2010
*! Purpose   : Browse order styles/sizes.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfStyBrow()
*!*************************************************************
FUNCTION lfStyDyeBrow
LPARAMETERS loFormSet

PRIVATE lcFields,lnCurAlias,llReturn,lcBrFields,lcFile_Ttl

STORE SPACE(0) TO lcFields
llReturn = .F.

lnCurAlias = SELECT(0)
lcFields    = "Style,cSize,nOrdLineNo,OQty,StyWgh"
lcBrFields  = [StyCode=Style:H='Style',DYELOT:H='Configuration',]+;
  [SzCode =EVAL('cSizeCod'):H='Size',]+;
  [SzWgh  =StyWgh     :H='Wgh\Unit',]+;
  [Open   =EVAL('OQty'):H='Open Quantity']
lcFile_Ttl  = 'Style Open Quantities'
lcForExp = "FOR " + IIF(loFormSet.llShwOpn,"EVAL('OQty')>0 AND EVAL('AvlQty')>0","EVAL('AvlQty')>0")

SELECT (loFormSet.lcPackLines)

DECLARE laTemp[1]

llReturn  = gfBrows(lcForExp,'Style','laTemp')

SELECT(lnCurAlias)

RETURN llReturn
*!*************************************************************
*! Name      : lfCHNGMODE
*! Developer : Mariam Mazhar
*! Date      : 01/21/2010
*! Purpose   : Chnage mode
*!*************************************************************
FUNCTION lfCHNGMODE
IF loFormSet.ActiveMode $ 'SV' OR !loFormSet.llUseConf
  loFormSet.Ariaform1.pgfPacking.CartonInfo.lblUPC.CAPTION = 'UPC #'
ELSE
  IF loFormSet.llUseConf
    loFormSet.Ariaform1.pgfPacking.CartonInfo.lblUPC.CAPTION = 'Config. Barcode'
  ELSE
    loFormSet.Ariaform1.pgfPacking.CartonInfo.lblUPC.CAPTION = 'UPC #'
  ENDIF
ENDIF
*! C201210,2 MMT 01/21/2010 Add New Triggers to Scan Barcode For FLO09 in Manul PL[End]

*! C201210,3 MMT 02/01/2010 Add New Triggers to Scan Barcode For FLO09 in Manul Credit memo and Style Screen[Start]
*!*************************************************************
*! Name      : lfMNUSCNCONG
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Add Option menu to credit memo screen
*!*************************************************************
FUNCTION lfMNUSCNCONG
LOCAL lcHostFormName
IF TYPE('_Screen.ActiveForm.Parent.llFrstChk') = 'U'
  _SCREEN.ACTIVEFORM.PARENT.ADDPROPERTY('llFrstChk',.F.)
ENDIF
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo  OF _OPTIONPOP  PROMPT 'Scan Configuration Barcode' SKIP FOR !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode = "A") OR (_SCREEN.ACTIVEFORM.PARENT.ariaForm1.pgfReCrmem.ACTIVEPAGE <> 2)
ON SELECTION BAR lnBarNo  OF _OPTIONPOP DO lfScanConf
lcCrMemLin = _SCREEN.ACTIVEFORM.PARENT.ariaForm1.rmcrmbus.lcCrMemLin
IF USED(lcCrMemLin)
  SELECT (lcCrMemLin)
  lcOrderOld = ORDER()
  INDEX ON  ACCOUNT+STYLE+DYELOT TAG 'RETLDYELOT' ADDITIVE
  SET ORDER TO (lcOrderOld)
ENDIF

*!*************************************************************
*! Name      : lfScanConf
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Scan barcode in credit memo screen
*!*************************************************************
FUNCTION lfScanConf
loParentForm = _SCREEN.ACTIVEFORM.PARENT
loParentForm.ARIAFORM1.PGFRECRMEM.PgDetail.CntDetail.GRdDetail.SETFOCUS()
IF !USED('Raline_a')
  =gfOpenTable('RALINE','RALINE','SH','Raline_a')
ENDIF
IF !USED('INVLINE_a')
  =gfOpenTable('INVLINE','INVLINE','SH','INVLINE_a')
ENDIF

IF !loParentForm.llFrstChk
  loParentForm.llFrstChk = .T.
  lcInvoice  = PADR(loParentForm.AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbinvoice.keytextbox.VALUE,6)
  lcRanNum =loParentForm.AriaForm1.kbRetAuth.KeyTextBox.VALUE
  IF !EMPTY(lcRanNum)
    lcSeekVal = PADR(lcRanNum,6)
    SELECT Raline_a
    IF gfSEEK(lcSeekVal)
      SELECT Raline_a
      SCAN REST WHILE RANO = lcSeekVal
        IF SEEK(Raline_a.ACCOUNT+Raline_a.STYLE,loParentForm.ariaForm1.rmcrmbus.lcCrMemLin,loParentForm.ariaForm1.rmcrmbus.lcCrMmStyl)
          SELECT (loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
          lnCurrRec = RECNO(loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
          loParentForm.ariaForm1.rmcrmbus.mvRemove()
          IF BETWEEN(lnCurrRec ,1,RECCOUNT(loParentForm.ariaForm1.rmcrmbus.lcCrMemLin))
            GO RECORD lnCurrRec  IN (loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
          ENDIF
        ENDIF
        SELECT Raline_a
      ENDSCAN
    ENDIF
  ELSE
    IF !EMPTY(lcInvoice)
      SELECT (loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
      SCAN FOR !EMPTY(Invoice) AND Invoice = lcInvoice
        lnCurrRec = RECNO()
        loParentForm.ariaForm1.rmcrmbus.mvRemove()
        SELECT (loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
        IF BETWEEN(lnCurrRec ,1,RECCOUNT(loParentForm.ariaForm1.rmcrmbus.lcCrMemLin))
          GO RECORD lnCurrRec  IN (loParentForm.ariaForm1.rmcrmbus.lcCrMemLin)
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDIF
*! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
IF oAriaApplication.MULTIINST
  DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH loParentForm,'C'
ELSE
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
  DO FORM (oAriaApplication.ScreenHome + 'POSNCBR.scx') WITH loParentForm,'C'
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
ENDIF
*! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
*!*************************************************************
*! Name      : lfValidateBrCd
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Validate scanned barcode
*!*************************************************************
FUNCTION lfValidateBrCd
LPARAMETERS loparentformset ,lcScreentype,lcUpcValue

DO CASE

  *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen[Start]
CASE lcScreentype = 'R' && Recieve PO Screen
  SET DATASESSION TO (loparentformset.DATASESSIONID)
  RETURN lfVScnBrCd(lcUpcValue,loparentformset)
  *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen[End]

CASE  lcScreentype = 'C' && Credit memo Screen
  SET DATASESSION TO (loparentformset.DATASESSIONID)
  lcAccount   = PADR(loparentformset.AriaForm1.KbAccount.KeyTextBox.VALUE,5)
  lcCrMemLin = loparentformset.ariaForm1.rmcrmbus.lcCrMemLin
  lcCrMmStyl = loparentformset.ariaForm1.rmcrmbus.lcCrMmStyl
  lcRanNum =loparentformset.AriaForm1.kbRetAuth.KeyTextBox.VALUE
  IF !USED('CONFGUPC')
    =gfOpenTable('CONFGUPC','CONFGUPCUP')
  ELSE
    SELECT CONFGUPC
    =gfSetOrder('CONFGUPCUP')
  ENDIF
  SELECT CONFGUPC
  IF gfSeek(lcUpcValue)
    lcSelStyle = CONFGUPC.STYLE
    lcSelConfg = CONFGUPC.DYELOT
    lcInvoice  = PADR(loparentformset .AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbinvoice.keytextbox.VALUE,6)
    lnSize     = VAL(CONFGUPC.SIZE)
    DO CASE
    CASE !EMPTY(lcRanNum)
      SELECT Raline_a
      =gfSeek(lcRanNum ,'Raline_a')
      LOCATE REST WHILE RANO+STYLE+CRA_LINNO  = lcRanNum  FOR STYLE = lcSelStyle  AND DYELOT = lcSelConfg
      IF !FOUND()
        IF !SEEK(lcAccount +PADR(lcSelStyle,19)+PADR(lcSelConfg ,10) ,lcCrMemLin,'RETLDYELOT')
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Configuration code is Invalid for the selected R/A#/Invoice")
        ENDIF
        loparentformset.ariaForm1.rmcrmbus.llAddLine = .T.
        lfValdStyCnfg(loparentformset,lcSelStyle ,lcSelConfg,lnSize)
      ELSE
        loparentformset.ariaForm1.rmcrmbus.llAddLine = .T.
        lfGetRAStyCnfg(loparentformset,lcSelStyle ,lcSelConfg,lnSize)
      ENDIF

    CASE !EMPTY(lcInvoice)
      SELECT INVLINE_a
      =gfSeek(lcInvoice)
      LOCATE REST WHILE INVOICE+STR(LINENO,6) = lcInvoice FOR STYLE = lcSelStyle AND DYELOT =lcSelConfg
      IF FOUND()
        loparentformset.ariaForm1.rmcrmbus.llAddLine = .T.
        lfGetInvStyCnfg(loparentformset,lcSelStyle ,lcSelConfg,lnSize)
      ELSE
        IF !SEEK(lcAccount +PADR(lcSelStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Configuration code is Invalid for the selected R/A#/Invoice")
        ENDIF
        loparentformset.ariaForm1.rmcrmbus.llAddLine = .T.
        lfValdStyCnfg(loparentformset,lcSelStyle ,lcSelConfg,lnSize)
      ENDIF
    OTHERWISE
      loparentformset.ariaForm1.rmcrmbus.llAddLine = .T.
      lfValdStyCnfg(loparentformset,lcSelStyle ,lcSelConfg,lnSize)
    ENDCASE
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Barcode "+lcUpcValue+" is not found.")
  ENDIF
  loparentformset.ARIAFORM1.PGFRECRMEM.PgDetail.CntDetail.GRdDetail.REFRESH()

CASE lcScreentype = 'S' && Style Screen
  _SCREEN.ACTIVEFORM.PARENT.RELEASE()
  IF !USED('CONFGUPC')
    =gfOpenTable('CONFGUPC','CONFGUPCUP')
  ELSE
    SELECT CONFGUPC
    =gfSetOrder('CONFGUPCUP')
  ENDIF
  SELECT CONFGUPC
  IF gfSeek(lcUpcValue)
    lcSelStyle = CONFGUPC.STYLE
    lcSelConfg  = CONFGUPC.DYELOT
    SET DATASESSION TO (loparentformset.DATASESSIONID)
    loparentformset.ACTIVATE()
    lcWareOld = loparentformset.Ariaform1.pgfStyleInfo.page2.cboLocation.VALUE
    lcMajor  = SUBSTR(lcSelStyle ,1,loparentformset.lnstylewid)
    lcNonMjr = IIF(!EMPTY(SUBSTR(lcSelStyle ,loparentformset.lnstylewid+2,loparentformset.lncolorwid)),SUBSTR(lcSelStyle ,loparentformset.lnstylewid+2,loparentformset.lncolorwid), "")
    IF !(!EMPTY(loParentForm.lcStyleKey) AND loParentForm.lcStyleKey =  lcSelStyle)
      IF gfSeek(lcMajor+loparentformset.lcSepart+IIF(EMPTY(lcNonMjr),"",lcNonMjr),'STYLE')
        *--Get style information.
        loparentformset.mGetInformation(lcMajor,lcNonMjr,'V')
        *--Start in view mode to inquire the passed style.
        loparentformset.ChangeMode('V')
      ENDIF
    ENDIF
    lcSelWareHouse = lcWareOld
    lcConfg = lcSelConfg
    lcStyleSel  = lcSelStyle
  ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfValdStyCnfg
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Validate scanned barcode Style in Credit memo
*!*************************************************************
FUNCTION lfValdStyCnfg
LPARAMETERS loparentformset,lcStyle,lcConfg,lnSize
SET DATASESSION TO (loparentformset.DATASESSIONID)
WITH loparentformset.ariaForm1.rmcrmbus
  lcCrMemLin = .lcCrMemLin
  lcCrMmStyl = .lcCrMmStyl
  lcCrMemHdr = .lcCrMemHdr
ENDWITH

SELECT STYLE

WITH loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader
  lcDivision  = .cboDivision.VALUE
  lcWareCode  = .cboLocation.VALUE
  lcInvoice   = PADR(.kbinvoice.keytextbox.VALUE,6)
ENDWITH

lcInvoice   = IIF(EMPTY(lcInvoice),"",lcInvoice)
WITH loparentformset.AriaForm1
  lcAccount   = PADR(.KbAccount.KeyTextBox.VALUE,5)
  lcRetVal    = PADR(.kbRetAuth.KeyTextBox.VALUE,6)
  lcOrderNo   = PADR(.pgfReCrmem.pgHeader.cntHeader.kbOrder.keytextbox.VALUE,6)
  lcCurrcode  = PADR(.pgfReCrmem.pgHeader.cntHeader.kbCurrency.keytextbox.VALUE,3)
  lDtPostDate = .DtPostDate.Text1.VALUE
  lnDiscLnPc  = .pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALUE
  .rmcrmbus.lndisclnpcnt = lnDiscLnPc
ENDWITH
lcSize = ALLTRIM(STR(lnSize))
STORE 0  TO lnLstPrice

WITH loparentformset.AriaForm1.pgfReCrmem
  lnTaxRate  = .pgHeader.cntHeader.txtTax.VALUE
  lnHstRate  = .pgDetail.cntDetail.txtHstRat.VALUE
  lnPstRate  = .pgDetail.cntDetail.txtPstRat.VALUE
ENDWITH

IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.lliscanada
  IF loparentformset.ariaForm1.rmcrmbus.llStatHst
    lnTaxRate  = 0
    lnHstRate  = customer.ntaxrate
    lnPstRate  = 0
  ELSE
    lnHstRate  = 0
    lnPstRate  = customer.ntaxrate
  ENDIF
ENDIF


WITH loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail
  IF !EMPTY(lcStyle)
    IF loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(PADR(lcStyle,19))
      *-- Get the discount code for the selected style.
      lcDiscCode = STYLE.cDiscCode
      IF STYLE.cDivision <> lcDivision
        *** Style / Division confilct! ***
        *** <  Ok  > ***
        =gfModalGen("INM46003B00000" , "DIALOG")
        lcStyle = lcOldValue
        WITH loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail
          .KbStyle.VALUE = lcStyle
          .kbConfig.lcStyleCode = SPACE(19)
          .kbConfig.lcWareCode = lcWareCode
        ENDWITH
        RETURN .F.
      ENDIF
      lcStyGrade = STYLE.cStyGrade
      lcStyDesc  = STYLE.DESC
      .txtStyDesc.VALUE = lcStyDesc
      .cboQuality.VALUE = STYLE.cStyGrade
      .cboQuality.REQUERY()
      *-- Get the scale & scale count.
      lcScale    = STYLE.SCALE
      IF loparentformset.ariaForm1.rmcrmbus.SCALE.SEEK("S" + lcScale)
        .SbrkBackToStk.SCALE = SCALE.SCALE
        .SbrkBackToStk.REFRESH()
        .SbrkBackToStk.txtTotQty.ENABLED = .F.
      ELSE
        *** Scale (lcScale) has been deleted.  Cannot proceed! ***
        *** <  Ok  > ***
        =gfModalGen("INM46006B00000" , "DIALOG" , lcScale)
        lcStyle = ''
        WITH loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail
          .KbStyle.VALUE = lcStyle
          .kbConfig.lcStyleCode = SPACE(19)
          .kbConfig.lcWareCode = lcWareCode
        ENDWITH
        RETURN .F.
      ENDIF
      lnInvRec = RECNO('INVLINE')
      loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
      IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
        GO RECORD lnInvRec IN INVLINE
      ENDIF
      IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
        SELECT StyDye
        IF !loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(PADR(lcStyle,19) + lcWareCode + SPACE(10))
          lcTmpTxt = ALLTRIM(lcStyle) + "|" + ALLTRIM(lcWareCode)
          IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
            DO gpAdStyWar WITH PADR(lcStyle,19) , lcConfg , lcWareCode
          ELSE
            lcStyle = ''
            loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
            loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = SPACE(19)
            loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
            RETURN .F.
          ENDIF
          IF !loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(PADR(lcStyle,19) + lcWareCode + PADR(lcConfg,10))
            *** Style: ALLTRIM(lcStyle) ***
            *** is not assigned to warehouse: ALLTRIM(lcWareCode)  ***
            *** <  Add  > - < Reenter > ***
            lcTmpTxt = ALLTRIM(lcStyle) +"|"+ALLTRIM(lcConfg)+ "|" + ALLTRIM(lcWareCode)
            IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
              DO gpAdStyWar WITH PADR(lcStyle,19) , lcConfg , lcWareCode
            ELSE
              lcStyle = ''
              loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
              loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = SPACE(19)
              loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
              RETURN .F.
            ENDIF
          ENDIF
        ELSE
          IF !loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(PADR(lcStyle,19) + lcWareCode + PADR(lcConfg,10))
            *** Style: ALLTRIM(lcStyle) ***
            *** is not assigned to warehouse: ALLTRIM(lcWareCode)  ***
            *** <  Add  > - < Reenter > ***
            lcTmpTxt = ALLTRIM(lcStyle) +"|"+ALLTRIM(lcConfg)+ "|" + ALLTRIM(lcWareCode)
            IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
              DO gpAdStyWar WITH PADR(lcStyle,19) , lcConfg , lcWareCode
            ELSE
              lcStyle = ''
              loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
              loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = SPACE(19)
              loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
              RETURN .F.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      lnInvRec = RECNO('INVLINE')
      loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
      IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
        GO RECORD lnInvRec IN INVLINE
      ENDIF
      *-- Initialize cost and cost link code from style file
      IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
        lcGlCost = IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF')
        IF loparentformset.ariaForm1.rmcrmbus.llMultiWH
          *-- Get cost and cost link code for this warehouse.
          lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
        ENDIF
      ENDIF

      lcPriceLvl = IIF(EMPTY(CUSTOMER.PRICELVL) , "A" , CUSTOMER.PRICELVL)
      llRetry    = .F.
      llShipped  = .F.
      STORE lcConfg TO .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE , lcDyelot

      STORE "" TO lcCOGSAcnt , lcICAcnt , lcSalesAcnt , lcDiscAcnt

      IF !EMPTY(lcInvoice)
        *-- Get the tax rate from the invoice header file.
        =loparentformset.ariaForm1.rmcrmbus.INVHDR.SEEK(lcInvoice)
        SELECT INVLINE
        loparentformset.ariaForm1.rmcrmbus.INVLINE.SEtOrder('INVLINES')
        = loparentformset.ariaForm1.rmcrmbus.INVLINE.SEEK(PADR(lcStyle,19) + lcInvoice)
        LOCATE REST WHILE STYLE+INVOICE+STR(LINENO,6)=PADR(lcStyle,19) + lcInvoice FOR DYELOT = lcConfg
        IF FOUND()
          lnLstPrice  = INVLINE.PRICE
          lnDiscLnPc  = INVHDR.DISCPCNT
          .txtDiscount.VALUE = INVHDR.DISCPCNT
          loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt  = INVHDR.DISCPCNT
          llShipped   = .T.
          STORE INVLINE.DYELOT TO .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE , lcDyelot


          *-- Calculate the tax in the case of england.
          IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
            *-- This tax will be for the current line.
            lnTaxRate = loparentformset.ariaForm1.rmcrmbus.mMechDisc(PADR(lcStyle,19))
            loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = lnTaxRate
          ENDIF
          .txtPstRat.VALUE = INVHDR.nPstRate
          lnPstRate  = INVHDR.nPstRate
          .txtHstRat.VALUE = INVHDR.nHstRate
          lnHstRate  = INVHDR.nHstRate
          loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs = INVHDR.trde_disc
          *loparentformset.ariaForm1.rmcrmbus.lnCost     = INVLINE.COST
          lnInvRec = RECNO('INVLINE')
          loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
          IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
            GO RECORD lnInvRec IN INVLINE
          ENDIF
          IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
            lcGLCost = IIF(!EMPTY(INVLINE.GL_COST) , INVLINE.GL_COST , lcGLCost)
          ENDIF
          IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.lliscanada
            IF loparentformset.ariaForm1.rmcrmbus.llStatHst
              .txtPstRat.VALUE = 0
              lnPstRate  = 0
              lnHstRate  = INVHDR.nHstRate
              .txtHstRat.VALUE = INVHDR.nHstRate
              lnTaxRate = 0
              loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = 0
            ELSE
              lnHstRate  = 0
              .txtHstRat.VALUE = 0
            ENDIF
          ENDIF

          loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk  = InvLine.GL_Sales
          lcCOGSAcnt  = Invline.ccogsacnt
          lcICAcnt    = Invline.cicacnt
          lcDiscAcnt  = Invline.cdiscacnt
        ELSE
          llRetry   = .T.
          *-- Get the lnCost variable.
          lnInvRec = RECNO('INVLINE')
          loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
          IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
            GO RECORD lnInvRec IN INVLINE
          ENDIF
          loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
        ENDIF
        SELECT INVLINE
        loparentformset.ariaForm1.rmcrmbus.invline.SetOrder('INVLINE')
      ELSE
        llRetry   = .T.
        *-- Get the lnCost variable.
        lnInvRec = RECNO('INVLINE')
        loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
        IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
          GO RECORD lnInvRec IN INVLINE
        ENDIF
        loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
      ENDIF

      *-- Search for the style in the invoice header file if there is no invoice #.
      IF llRetry
        SELECT INVLINE
        loparentformset.ariaForm1.rmcrmbus.Invline.SetORder('INVLINES')
        SET ORDER TO
        SELECT INVHDR
        *-- Make invhdra the active index tag
        SET ORDER TO TAG INVHDRA DESCENDING
        IF loparentformset.ariaForm1.rmcrmbus.INVHDR.SEEK(lcAccount)
          *-- Searching for the last invoice this style was shipped on using
          *-- the same current currency.
          loparentformset.ariaForm1.rmcrmbus.INVLINE.SEtOrder('INVLINES')
          SCAN REST WHILE ACCOUNT = lcAccount FOR InvHdr.STATUS <> 'V'
            IF (loparentformset.ariaForm1.rmcrmbus.llMulCurr .AND. (INVHDR.cCurrCode = lcCurrcode)) .OR.;
                !loparentformset.ariaForm1.rmcrmbus.llMulCurr
              lcSrchInv = INVHDR.INVOICE
              lcSeekVal = IIF(EMPTY(lcSrchInv),PADR(lcStyle,19),PADR(lcStyle,19)+lcSrchInv)
              SELECT INVLINE
              =loparentformset.ariaForm1.rmcrmbus.INVLINE.SEEK(lcSeekVal)
              LOCATE REST WHILE STYLE+INVOICE+STR(LINENO,6) = lcSeekVal ;
                FOR DYELOT =lcConfg
              IF FOUND()
                lnLstPrice = PRICE
                lnDiscLnPc = INVHDR.DISCPCNT
                .txtDiscount.VALUE = INVHDR.DISCPCNT
                llShipped  = .T.
                STORE DYELOT TO .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE , lcDyelot
                *-- Calculate the tax in the case of england.
                IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
                  *** This tax will be for the current line.
                  lnTaxRate = loparentformset.ariaForm1.rmcrmbus.mMechDisc(PADR(lcStyle,19))
                  loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = lnTaxRate
                ENDIF
                lnPstRate  = INVHDR.nPstRate
                lnHstRate  = INVHDR.nHstRate
                *-- Get the lnCost variable all the time
                *-- regardless the GL link condition.

                *E302618,1 MMT 06/17/2009 Enable and Disable PST AND HST Fields For Candian Companies[Start]
                IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.lliscanada
                  IF loparentformset.ariaForm1.rmcrmbus.llStatHst
                    lnPstRate  = 0

                    lnHstRate  = customer.ntaxrate
                    loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.txtHSTRat.VALUE = lnHstRate
                    lnTaxRate = 0
                    loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = 0
                  ELSE
                    lnHstRate  = 0
                    lnPstRate  = customer.ntaxrate
                    loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.TXtPstRat.VALUE =lnPstRate
                  ENDIF
                ENDIF
                lnInvRec = RECNO('INVLINE')
                loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
                IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
                  GO RECORD lnInvRec IN INVLINE
                ENDIF
                EXIT
              ENDIF
            ENDIF
            SELECT INVHDR
          ENDSCAN
          SELECT INVLINE
          loparentformset.ariaForm1.rmcrmbus.invline.SetOrder('INVLINE')
        ENDIF
        SELECT INVHDR
        loparentformset.ariaForm1.rmcrmbus.invhdr.SetOrder('INVHDR')
      ENDIF

      IF !llShipped
        *** This style was not shipped to this customer! continue? ***
        *** <  Yes  > - <  No  > ***
        IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
          IF gfModalGen("QRM46005B00006" , "DIALOG") = 2
            lcStyle = ''
            loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
            loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = SPACE(19)
            loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
            RETURN .F.
          ENDIF
        ENDIF
        lnPstRate  = loparentformset.ariaForm1.rmcrmbus.lnDfltPstR
        lnHstRate  = loparentformset.ariaForm1.rmcrmbus.lnDefHst

        IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.lliscanada
          IF loparentformset.ariaForm1.rmcrmbus.llStatHst
            lnPstRate  = 0
            lnHstRate  = customer.ntaxrate
            lnTaxRate = 0
            loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = 0
          ELSE
            lnHstRate  = 0
          ENDIF
        ENDIF
        IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
          lnLstPrice = MAX(loparentformset.ariaForm1.rmcrmbus.mGetprice(lcStyle,lcPriceLvl,.txtTotAlQty.VALUE),0)
        ELSE
          lnLstPrice = &lcCrMemLin..GROS_PRICE
        ENDIF

        *-- If the discount code for the selected style not empty.
        IF !EMPTY(lcDiscCode)
          *-- Get the discount percentage for this discount code from the codes file.
          DECLARE laStyDisc[1,2]
          lnDiscPcnt = loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt
          laStyDisc[1,1] = 'DISCPCNT'
          laStyDisc[1,2] = 'lnDiscPcnt'
          =gfRltFld(lcDiscCode , @laStyDisc , "CDISCCODE")
          loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt = lnDiscPcnt
          *-- If the discount percentage greater than zero...
          IF loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt > 0
            lcTmpStr = ALLTRIM(STR(loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt)) +"|" + ALLTRIM(STR(lnLstPrice,10,3)) + ;
              "|" + ALLTRIM(STR((lnLstPrice * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100),10,3)) + "|" + ;
              ALLTRIM(STR(lnLstPrice - (lnLstPrice * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/ 100),10,3))

            *** There is discount percentage on this style : {lnDiscPcnt} ***
            *** Gross Price is     : {lnLstPrice}
            *** Discount amount is : {lnLstPrice * lnDiscPcnt /100}
            *** Net Price is       : {lnLstPrice - (lnLstPrice * lnDiscPcnt / 100)}
            IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
              =gfModalGen("INM46030B00000" , "DIALOG" , lcTmpStr)
              lnLstPrice = lnLstPrice - (lnLstPrice * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/ 100)
            ENDIF
            *-- Get a discount on the total gross price, & inform the user.

          ENDIF
        ENDIF
        lcTerms = loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboterms.VALUE
        *-- Calculate the tax in the case of england.
        IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
          *** This tax will be for the current line.
          =loparentformset.ariaForm1.rmcrmbus.Customer.SEEK('M' + lcAccount)
          lnTaxRate  = loparentformset.ariaForm1.rmcrmbus.mMechDisc(PADR(lcStyle,19))
          loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTax.VALUE = lnTaxRate
          lcTerms  = IIF(EMPTY(lcTerms) , Customer.cTermCode , lcTerms)

          *-- Call local function to get the trade discount.
          DO lfGetTrdDs IN (oAriaApplication.ApplicationHome + 'RMSave.FXP') WITH  lcTerms , lcInvoice , loparentformset.ariaForm1.rmcrmbus
        ELSE
          lcTerms  = IIF(EMPTY(lcTerms) , IIF(loparentformset.ariaForm1.rmcrmbus.CUSTOMER.SEEK(lcAccount) , CUSTOMER.cTermCode , SPACE(6)) , lcTerms)
          *-- Call local function to get the trade discount.
          DO lfGetTrdDs IN (oAriaApplication.ApplicationHome + 'RMSave.FXP') WITH  lcTerms , lcInvoice , loparentformset.ariaForm1.rmcrmbus

        ENDIF
        loparentformset.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboterms.VALUE = lcTerms
        loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt= 0

        *-- Get the lnCost variable.
        lnInvRec = RECNO('INVLINE')
        loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.T.) && Assign the appropriatr cost
        IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
          GO RECORD lnInvRec IN INVLINE
        ENDIF
      ENDIF


      lnCost = loparentformset.ariaForm1.rmcrmbus.lnCost
      IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
        IF oAriaApplication.User_Level ='O'

          IF gfUserPriv('IC','ICSTYLE','COSTING') AND !loparentformset.ariaForm1.rmcrmbus.llStdCost
            loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mGetCost(lcStyle,lnCost)
          ENDIF
        ELSE
          IF  !loparentformset.ariaForm1.rmcrmbus.llStdCost
            loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mGetCost(lcStyle,lnCost)
          ENDIF
        ENDIF
      ELSE
        lnCost = &lcCrMemLin..COST
      ENDIF
      *-- Add the current line in the credit memo temp. file.
      SELECT (lcCrMemLin)
      IF loparentformset.ariaForm1.rmcrmbus.llAddLine
        *-- If there was no lines & this is the first line to be added,
        *-- Refresh the objects related to the line no. in the header folder.
        *-- Find an empty record to reuse it instead of adding new line.
        IF !SEEK(lcAccount +PADR(lcStyle,19)+PADR(lcSelConfg ,10),lcCrMemLin,'RETLDYELOT')
          IF loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0
            *-- Increment the lines counter with the first line.
            loparentformset.ariaForm1.rmcrmbus.lnCrMemNo  = 1
            *-- Get the right refreshing for the objects on the screen.
            loparentformset.AriaForm1.llRAStat  = (loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
            loparentformset.AriaForm1.llInvStat = (loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
            loparentformset.AriaForm1.llOrdStat = (EMPTY(lcOrderNo) .AND. EMPTY(lcRetVal) .AND. loparentformset.ActiveMode = 'A'  .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
            loparentformset.AriaForm1.llEntrStat= (loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
          ELSE
            *-- increment the lines counter with one.
            loparentformset.ariaForm1.rmcrmbus.lnCrMemNo  = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo + 1
          ENDIF
          loparentformset.ariaForm1.rmcrmbus.lnCrLinCnt = loparentformset.ariaForm1.rmcrmbus.lnCrLinCnt + 1
          loparentformset.ariaForm1.rmcrmbus.llAddLine  = .F.
          LOCATE FOR EMPTY(STYLE)
          IF !FOUND()
            APPEND BLANK
          ENDIF
          BLANK
        ENDIF
        *-- Lock the record to grantee the phiscal update.
        REPLACE STYLE      WITH PADR(lcStyle,19) ;
          DESC        WITH lcStyDesc,;
          CRET_TRNCD WITH "2" ;
          CSTYGRADE  WITH lcStyGrade ;
          ACCOUNT    WITH lcAccount ;
          INVOICE    WITH lcInvoice ;
          CRDATE     WITH IIF(!EMPTY(lDtPostDate) , lDtPostDate , oAriaApplication.SystemDate) ;
          DYELOT     WITH lcDyelot ;
          REASON     WITH .cboReason.VALUE ;
          GROS_PRICE WITH lnLstPrice ;
          DISC_PCNT  WITH lnDiscLnPC ;
          PRICE      WITH GROS_PRICE * (1 - DISC_PCNT/100) ;
          TAX_RATE   WITH lnTaxRate ;
          nPstRate   WITH lnPstRate ;
          COST       WITH loparentformset.ariaForm1.rmcrmbus.lnCost,;
          cReason    WITH .cboReason.DISPLAYVALUE ;
          nHstRate   WITH lnHstRate;
          lShipped   WITH llShipped;
          cRet_LinNo WITH ALLTRIM(STR(loparentformset.ariaForm1.rmcrmbus.lnCrLinCnt))
      ENDIF
      IF loparentformset.ariaForm1.rmcrmbus.RALINE.SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..STYLE)
        SELECT RALINE
        LOCATE REST WHILE RANO+STYLE+CRA_LINNO = &lcCrMemHdr..RaNo + &lcCrMemLin..STYLE FOR Dyelot = lcDyelot
        IF FOUND()
          SELECT (lcCrMemLin)
          REPLACE nOpnQty1   WITH RALINE.nOpnQty1 ;
            nOpnQty2   WITH RALINE.nOpnQty2 ;
            nOpnQty3   WITH RALINE.nOpnQty3 ;
            nOpnQty4   WITH RALINE.nOpnQty4 ;
            nOpnQty5   WITH RALINE.nOpnQty5 ;
            nOpnQty6   WITH RALINE.nOpnQty6 ;
            nOpnQty7   WITH RALINE.nOpnQty7 ;
            nOpnQty8   WITH RALINE.nOpnQty8 ;
            nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
        ENDIF
      ENDIF

      IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
        loparentformset.ariaForm1.rmcrmbus.lcStySlLnk = STYLE.cslsgllink
        IF EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk)
          *-- Get the customer link & the style link.
          =loparentformset.ariaForm1.rmcrmbus.customer.SEEK("M" + lcAccount)
          IF loparentformset.ariaForm1.rmcrmbus.llDiv_Link
            STORE '' TO lcCustLink,lcCstSlLnk
            DECLARE laDRltFld[2,2]
            laDRltFld[1,1] = 'LINK_CODE'
            laDRltFld[1,2] = 'lcCustLink'
            laDRltFld[2,1] = 'CSLSGLLINK'
            laDRltFld[2,2] = 'lcCstSlLnk'
            =gfRltFld(lcDivision , @laDRltFld,'CDIVISION')
            loparentformset.ariaForm1.rmcrmbus.lcCustLink = lcCustLink
            loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = lcCstSlLnk
            loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk), ;
              PADR(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk,3) ,;
              IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
          ELSE
            loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
          ENDIF
        ENDIF

        SELECT (lcCrMemLin)
        REPLACE GL_COST  WITH lcGLCost ;
          GL_SALES WITH IIF(EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) ,;
          ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk) + ;
          ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk) ,;
          ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) + ;
          ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk))

        REPLACE cCOGSAcnt  WITH lcCOGSAcnt  ;
          cICAcnt    WITH lcICAcnt    ;
          cSalesAcnt WITH lcSalesAcnt ;
          cDiscAcnt  WITH lcDiscAcnt
      ENDIF

      loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail.grdDetail.REFRESH
      loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail.grdDetail.AFTERROWCOLCHANGE ()
      loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcSize..VALUE = &lcCrMemLin..QTY&lcSize.+1
      loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = &lcCrMemLin..GROS_PRICE
      loparentformset.AriaForm1.rmcrmbus.mvQty(lnSize,&lcCrMemLin..QTY&lcSize.)

      SELECT (lcCrMemLin)
      lcStyDesc = STYLE.DESC
      loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = PADR(lcStyle,19)
      loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
    ELSE
      lcStyle = lcOldValue
      loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
      loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = PADR(lcOldValue,19)
      loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
      RETURN .F.
    ENDIF
  ELSE
    lcStyle = lcOldValue
    loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
    loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcStyleCode = PADR(lcOldValue,19)
    loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.lcWareCode = lcWareCode
    RETURN  .F.
  ENDIF
  loparentformset.AriaForm1.llDyeStat  = (loparentformset.ariaForm1.rmcrmbus.lnCrMemNo > 0 .AND.;
    loparentformset.ariaForm1.rmcrmbus.llUseDyes .AND.;
    loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(PADR(lcStyle,19)) .AND.;
    STYLE.cdye_flg = 'Y')
  loparentformset.AriaForm1.llSizestat = !EMPTY(lcStyle) AND loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(PADR(lcStyle,19))
  lcConfig = loparentformset.ariaform1.pgfReCrmem.pgDetail.cntDetail.kbConfig.keytextBox.VALUE
  loparentformset.AriaForm1.llSizestat = !(STYLE.cdye_flg = 'Y' AND EMPTY(lcConfig))
  loparentformset.ariaForm1.rmcrmbus.lcinvsllnk = ""
  =loparentformset.ariaForm1.rmcrmbus.mwBrowLin(.grdDetail)
ENDWITH
loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = lcStyle
loparentformset.AriaForm1.mShowObj()

*!*************************************************************
*! Name      : lfGetRAStyCnfg
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : get scanned barcode Style info RA Selected (Credit memo Screen)
*!*************************************************************
FUNCTION lfGetRAStyCnfg
LPARAMETERS loparentformset,lcSelStyle ,lcSelConfg,lnSize
PRIVATE lcRetVal,lcAccount,lcStore,lcInvoice,lcOrder,lcCrMemLin
lcRetVal   = PADR(loparentformset.AriaForm1.kbRetAuth.KeyTextBox.VALUE,6)
lcAccount  = PADR(loparentformset.AriaForm1.KbAccount.KeyTextBox.VALUE,5)
lcStore    = PADR(loparentformset.AriaForm1.KbStore.KeyTextBox.VALUE,8)
lcInvoice  = PADR(loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbinvoice.keytextbox.VALUE,6)
lcOrder    = PADR(loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbOrder.keytextbox.VALUE,6)
lcCrMemLin = loparentformset.ariaForm1.rmcrmbus.lcCrMemLin
lcCrMmStyl = loparentformset.ariaForm1.rmcrmbus.lcCrMmStyl
lcSize = ALLTRIM(STR(lnSize))
WITH loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader
  *-- Get the lines from the Raline file.
  SELECT RALINE
  IF loparentformset.ariaForm1.rmcrmbus.RALINE.SEEK(lcRetVal)
    lnLineCnt = 1

    IF !EMPTY(lcInvoice)
      IF !loparentformset.ariaForm1.rmcrmbus.llIsCanada .AND. !loparentformset.ariaForm1.rmcrmbus.llTax
        .txtTaxAmnt.VALUE = IIF(loparentformset.ariaForm1.rmcrmbus.llIsEnglnd , loparentformset.ariaForm1.rmcrmbus.mchgTax() , IIF(loparentformset.ariaForm1.rmcrmbus.llTax , InvHdr.Tax_Amt , 0))
      ENDIF
      IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.llIsCanada
        IF loparentformset.ariaForm1.rmcrmbus.llstathst
          .txtTaxAmnt.VALUE = 0
          .txtPstTax.VALUE   = 0
          .txtTax.VALUE   = 0
        ELSE
          .txtHstTax.VALUE = 0
        ENDIF
      ENDIF
    ELSE
      .txtTaxAmnt.VALUE = IIF(loparentformset.ariaForm1.rmcrmbus.llIsEnglnd , loparentformset.ariaForm1.rmcrmbus.mchgTax() , 0)
      IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.llIsCanada
        IF loparentformset.ariaForm1.rmcrmbus.llstathst
          .txtTaxAmnt.VALUE = 0
          .txtHstTax.VALUE = 0
          .txtPstTax.VALUE   = 0
          .txtTax.VALUE   = 0
        ELSE
          .txtTaxAmnt.VALUE =  0
          .txtHstTax.VALUE = 0
          .txtPstTax.VALUE = 0
          .txtTax.VALUE   = loparentformset.ariaForm1.rmcrmbus.lnDfTaxRat
        ENDIF
      ENDIF
    ENDIF
    .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)

    *-- Loop in the RA lines to be sure that all styles exist in the style file.
    SCAN WHILE RALINE.RANO = lcRetVal FOR STYLE = lcSelStyle AND DYELOT = lcSelConfg
      IF loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(RALINE.STYLE)
        SELECT (lcCrMemLin)
        IF !SEEK(RALINE.ACCOUNT+RALINE.STYLE,lcCrMemLin,lcCrMmStyl)
          APPEND BLANK
          *-- Lock the record to grantee the phiscal update.
          IF loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0
            loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 1
          ELSE
            loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo + 1
          ENDIF
          lnLineCnt = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo
          REPLACE ACCOUNT    WITH RALINE.ACCOUNT ;
            STYLE      WITH RALINE.STYLE ;
            DESC       WITH STYLE.DESC,;
            CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
            CRET_TRNCD WITH "2" ;
            Dyelot     WITH RaLine.Dyelot ;
            REASON     WITH RALINE.REASON ;
            cREASON    WITH gfCodDes(RALINE.REASON,'REASON') ;
            cStyGrade  WITH STYLE.cStyGrade,;
            PRICE      WITH RALINE.PRICE ;
            AMOUNT     WITH RALINE.AMOUNT ;
            INVOICE    WITH lcInvoice ;
            CRDATE     WITH IIF(!EMPTY(.DtCrdDate.Text1.VALUE) , .DtCrdDate.Text1.VALUE , oAriaApplication.SystemDate) ;
            nPstRate   WITH RALINE.nPstRate ;
            TAX_RATE   WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax , .txtTax.VALUE , 0) ;
            nOpnQty1   WITH RALINE.nOpnQty1 ;
            nOpnQty2   WITH RALINE.nOpnQty2 ;
            nOpnQty3   WITH RALINE.nOpnQty3 ;
            nOpnQty4   WITH RALINE.nOpnQty4 ;
            nOpnQty5   WITH RALINE.nOpnQty5 ;
            nOpnQty6   WITH RALINE.nOpnQty6 ;
            nOpnQty7   WITH RALINE.nOpnQty7 ;
            nOpnQty8   WITH RALINE.nOpnQty8 ;
            nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
        ENDIF
        IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
          REPLACE TAX_RATE WITH loparentformset.ariaForm1.rmcrmbus.mMechDisc(&lcCrMemLin..STYLE)
        ENDIF
        IF loparentformset.ariaForm1.rmcrmbus.llTax AND loparentformset.ariaForm1.rmcrmbus.llIsCanada
          SELECT (lcCrMemLin)
          IF loparentformset.ariaForm1.rmcrmbus.llstathst

            REPLACE nHstRate WITH IIF(!EMPTY(lcInvoice),Invhdr.nHstRate,customer.ntaxrate),;
              nPstRate WITH 0,;
              TAX_RATE WITH 0

          ELSE
            REPLACE nHstRate WITH 0
          ENDIF
        ENDIF
        *-- Get the numeric values.
        REPLACE GROS_PRICE WITH (RALINE.PRICE/ (1-loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100)) ;
          DISC_PCNT  WITH loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt ;
          DISC_AMT   WITH (GROS_PRICE * TOTQTY * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100) ;
          TRDE_AMT   WITH AMOUNT * loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs/100 ;
          cStyGrade  WITH IIF(loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE) , STYLE.cStyGrade , "")

        REPLACE AMOUNT  WITH (TOTQTY * PRICE) * (1-(loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100))



        *-- Get the Style cost.

        IF loparentformset.ariaForm1.rmcrmbus.llStdCost
          lnInvRec = RECNO('INVLINE')
          loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.F.) && Assign the appropriatr cost
          IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
            GO RECORD lnInvRec IN INVLINE
          ENDIF

          STORE "" TO lcCOGSAcnt , lcICAcnt , lcSalesAcnt , lcDiscAcnt
        ELSE
          IF !EMPTY(&lcCrMemLin..Invoice) AND SEEK(&lcCrMemLin..Invoice , "INVLINE")
            lnInvRec = RECNO('INVLINE')
            loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.F.) && Assign the appropriatr cost
            IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
              GO RECORD lnInvRec IN INVLINE
            ENDIF
            SELECT(lcCrMemLin)
            loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = InvLine.GL_Sales
            lcCOGSAcnt  = Invline.ccogsacnt
            lcICAcnt    = Invline.cicacnt
            lcSalesAcnt = ""
            lcDiscAcnt  = Invline.cdiscacnt
            REPLACE lShipped  WITH .T.

          ELSE
            lnInvRec = RECNO('INVLINE')
            loparentformset.ariaForm1.rmcrmbus.lnCost = loparentformset.ariaForm1.rmcrmbus.mcostassign(.F.) && Assign the appropriatr cost
            IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
              GO RECORD lnInvRec IN INVLINE
            ENDIF
            SELECT(lcCrMemLin)
            loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
            STORE "" TO lcCOGSAcnt , lcICAcnt , lcSalesAcnt , lcDiscAcnt
          ENDIF
        ENDIF
        SELECT (lcCrMemLin)
        REPLACE COST WITH loparentformset.ariaForm1.rmcrmbus.lnCost
        REPLACE cCOGSAcnt  WITH lcCOGSAcnt  ;
          cICAcnt    WITH lcICAcnt    ;
          cSalesAcnt WITH lcSalesAcnt ;
          cDiscAcnt  WITH lcDiscAcnt

        *-- If invoice number was entered, Read the cost and cost link code
        *-- invoice file. Else read average cost and cost link code from
        *-- style file.
        IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
          lcGlCost   = "DEFDEF"
          IF !EMPTY(&lcCrMemLin..Invoice) .AND. loparentformset.ariaForm1.rmcrmbus.invline.SEEK(&lcCrMemLin..Invoice)
            lcGlCost   = IIF(!EMPTY(InvLine.Gl_Cost) , InvLine.Gl_Cost , "DEFDEF")
          ELSE
            IF !loparentformset.ariaForm1.rmcrmbus.llMultiWH
              IF loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE)
                lcGlCost =IIF(!EMPTY(STYLE.Link_Code) , STYLE.Link_Code , "DEFDEF")
              ENDIF
            ELSE
              *- If multi warehouse and the invoice number left empty, Get
              *- average cost and G.L. cost link code for that warehouse.
              IF loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(&lcCrMemLin..STYLE + .cboLocation.VALUE + PADR(&lcCrMemLin..DYELOT,10))
                lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
              ENDIF
            ENDIF
          ENDIF

          *-- Get the customer link & the style link.
          =loparentformset.ariaForm1.rmcrmbus.CUSTOMER.SEEK("M" + lcAccount)
          IF EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk)
            IF loparentformset.ariaForm1.rmcrmbus.llDiv_Link
              STORE '' TO lcCustLink,lcCstSlLnk
              DECLARE laDRltFld[2,2]
              laDRltFld[1,1] = 'LINK_CODE'
              laDRltFld[1,2] = 'lcCustLink'
              laDRltFld[2,1] = 'CSLSGLLINK'
              laDRltFld[2,2] = 'lcCstSlLnk'
              =gfRltFld(.cboDivision.VALUE , @laDRltFld,'CDIVISION')
              loparentformset.ariaForm1.rmcrmbus.lcCustLink = lcCustLink
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = lcCstSlLnk
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk), PADR(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
            ELSE
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
            ENDIF
          ENDIF

          =loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE)
          loparentformset.ariaForm1.rmcrmbus.lcStySlLnk = STYLE.cslsgllink
          REPLACE GL_COST   WITH lcGlCost ;
            GL_SALES  WITH IIF(EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk))

        ENDIF
        loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail.grdDetail.REFRESH
        loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail.grdDetail.AFTERROWCOLCHANGE ()
        loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcSize..VALUE = &lcCrMemLin..QTY&lcSize.+1
        loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = &lcCrMemLin..GROS_PRICE
        loparentformset.AriaForm1.rmcrmbus.mvQty(lnSize,&lcCrMemLin..QTY&lcSize.)


      ENDIF
      SELECT RALINE
    ENDSCAN

    *-- Calculate the cahrge back.
    IF loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs <> 0
      .txtChrgBckRep1.VALUE = ROUND((loparentformset.ariaForm1.rmcrmbus.lnamount * (100 - loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs)/100) * (.spnRepComm1.VALUE/100),2) * -1
      .txtChrgBckRep2.VALUE = ROUND((loparentformset.ariaForm1.rmcrmbus.lnamount * (100 - loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs)/100) * (.spnRepComm2.VALUE/100),2) * -1
    ELSE
      .txtChrgBckRep1.VALUE = ROUND(loparentformset.ariaForm1.rmcrmbus.lnamount * (.spnRepComm1.VALUE/100),2) * -1
      .txtChrgBckRep2.VALUE = ROUND(loparentformset.ariaForm1.rmcrmbus.lnamount * (.spnRepComm2.VALUE/100),2) * -1
    ENDIF
    .txtOpenPcs.VALUE  = MAX(loparentformset.ariaForm1.rmcrmbus.lnRAQty - .txtReceivedPcs.VALUE , 0)
    .txtOpenAmnt.VALUE = MAX(loparentformset.ariaForm1.rmcrmbus.lnRAAmt - .txtReceivedAmnt.VALUE , 0)

    *-- Disable or enable the objects related to the R/A # & existance of lines.
    *-- Refresh store objects.
    loparentformset.AriaForm1.llStorStat= (loparentformset.ActiveMode = 'A' .AND. EMPTY(lcRetVal))
    loparentformset.AriaForm1.llEdtStat= (loparentformset.ActiveMode = 'A' .AND. EMPTY(lcRetVal))
    *-- Refresh R/A objects.
    loparentformset.AriaForm1.llRAStat  = (loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
    *-- Refresh invoice objects.
    loparentformset.AriaForm1.llInvStat = (loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
    *-- Refresh order objects.
    loparentformset.AriaForm1.llOrdStat = (EMPTY(lcOrder) .AND. EMPTY(lcRetVal) .AND. loparentformset.ActiveMode = 'A' .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0)
    *-- Refresh check box "return entire invoice" object.
    loparentformset.AriaForm1.llEntrStat= (loparentformset.ActiveMode = 'A'.AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0 AND !EMPTY(lcInvoice))
    *-- Refresh currency code objects.
    loparentformset.AriaForm1.llCurStat = (loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. !loparentformset.ariaForm1.rmcrmbus.RALINE.SEEK(lcRetVal))
    *-- Refresh exchange rate object.
    loparentformset.AriaForm1.llExRStat = (loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. !loparentformset.ariaForm1.rmcrmbus.RALINE.SEEK(lcRetVal) .AND. !EMPTY(.kbCurrency.keytextbox.VALUE) .AND. loparentformset.ariaForm1.rmcrmbus.llEditExRt .AND. .kbCurrency.keytextbox.VALUE <> oAriaApplication.BaseCurrency)

    IF !EMPTY(lcRetVal) .AND. loparentformset.ariaForm1.rmcrmbus.llIsCanada .AND. loparentformset.ariaForm1.rmcrmbus.llTax
      .txtTaxAmnt.VALUE = ((loparentformset.ariaForm1.rmcrmbus.lnamount+IIF(loparentformset.ariaForm1.rmcrmbus.lcTax_Meth="A" , .txtCharges.VALUE , 0)) * (.txtTax.VALUE/100))
      .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)
    ENDIF
    .txtTotCredit.VALUE = .txtCharges.VALUE + loparentformset.ariaForm1.rmcrmbus.lnamount + .txtTaxAmnt.VALUE + .txtPstTax.VALUE + .txtHstTax.VALUE

    *-- Set flag to execute the browse.
    loparentformset.AriaForm1.llReBrowse = .T.

    SELECT (lcCrMemLin)
    GO TOP
  ENDIF
  loparentformset.ariaForm1.rmcrmbus.lcinvsllnk = ""
ENDWITH
loparentformset.AriaForm1.mShowObj()



*!*************************************************************
*! Name      : lfGetInvStyCnfg
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : get scanned barcode Style info Invoice Selected (Credit memo Screen)
*!*************************************************************
FUNCTION lfGetInvStyCnfg
LPARAMETERS loparentformset,lcSelStyle ,lcSelConfg,lnSize


PRIVATE lcRetVal,lcAccount,lcStore,lcInvoice,lcOrder,lcCrMemLin
lcRetVal   = PADR(loparentformset.AriaForm1.kbRetAuth.KeyTextBox.VALUE,6)
lcAccount  = PADR(loparentformset.AriaForm1.KbAccount.KeyTextBox.VALUE,5)
lcStore    = PADR(loparentformset.AriaForm1.KbStore.KeyTextBox.VALUE,8)
lcInvoice  = PADR(loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbinvoice.keytextbox.VALUE,6)
lcOrder    = PADR(loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.kbOrder.keytextbox.VALUE,6)
lcCrMemLin = loparentformset.ariaForm1.rmcrmbus.lcCrMemLin
lcCrMmStyl = loparentformset.ariaForm1.rmcrmbus.lcCrMmStyl
lcWareCode = loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.cboLocation.VALUE
llFondLins = .T.
lcSize = ALLTRIM(STR(lnSize))
WITH loparentformset.AriaForm1.pgfReCrmem.pgHeader.cntHeader
  SELECT INVHDR
  loparentformset.ariaForm1.rmcrmbus.INVHDR.SEEK(lcInvoice)
  *-- If the invoice is consolidated, get the lines from CONSINVL.
  IF INVHDR.CONSOL = 'Y'
    SELECT CONSINVH
    IF loparentformset.ariaForm1.rmcrmbus.CONSINVH.SEEK(lcInvoice)

      IF loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0
        loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 1
      ELSE
        loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo + 1
      ENDIF
      lnLineCnt = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo

      SELECT INVLINE
      loparentformset.ariaForm1.rmcrmbus.Invline.SetOrder('INVLINES')
      SELECT CONSINVH
      .txtTaxAmnt.VALUE = IIF(loparentformset.ariaForm1.rmcrmbus.llisenglnd , loparentformset.ariaForm1.rmcrmbus.mchgtax(),0)
      .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)

      SCAN WHILE Invoice = lcInvoice FOR cWareCode = lcWareCode
        IF !loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
          .txtTaxAmnt.VALUE = .txtTaxAmnt.VALUE + IIF(loparentformset.ariaForm1.rmcrmbus.llTax , CONSINVH.Tax_Amt , 0)
        ENDIF
        .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)

        SELECT CONSINVL
        IF loparentformset.ariaForm1.rmcrmbus.CONSINVL.SEEK(lcInvoice + CONSINVH.STORE+CONSINVH.ORDER)
          *-- Loop in the invoice lines to be sure of its existance in the style file.
          SCAN REST WHILE Invoice+STORE+ORDER = lcInvoice+CONSINVH.STORE+CONSINVH.ORDER FOR STYLE =lcSelStyle AND dyelot =lcSelConfg  ;
              .AND. loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(CONSINVL.STYLE)
            IF PIKTKT <> CONSINVH.PIKTKT
              LOOP
            ENDIF
            SELECT (lcCrMemLin)
            lnPstRate  = loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtPstRat.VALUE
            lnHstRate  = loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtHstRat.VALUE
            IF !SEEK(CONSINVL.ACCOUNT+CONSINVL.STYLE,lcCrMemLin,lcCrMmStyl)
              APPEND BLANK
              REPLACE ACCOUNT   WITH CONSINVL.ACCOUNT ;
                STYLE     WITH CONSINVL.STYLE ;
                DESC      WITH STYLE.DESC,;
                CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
                CRET_TRNCD WITH "2" ;
                INVOICE   WITH lcInvoice ;
                CRDATE    WITH IIF(!EMPTY(.DtCrdDate.Text1.VALUE) , .DtCrdDate.Text1.VALUE , oAriaApplication.SystemDate) ;
                Dyelot    WITH CONSINVL.Dyelot ;
                PRICE     WITH CONSINVL.PRICE * ( 1 - CONSINVH.DISCPCNT/100) ;
                AMOUNT    WITH TOTQTY * (CONSINVL.PRICE * ( 1 - CONSINVH.DISCPCNT/100)) ;
                nPstRate  WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax .AND. loparentformset.ariaForm1.rmcrmbus.llIsCanada , lnPstRate , 0) ;
                TAX_RATE  WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax , .txtTax.VALUE , 0) ;
                lInvoice  WITH loparentformset.ariaForm1.rmcrmbus.llInsrtInv
              REPLACE REASON     WITH .cboReason.VALUE,;
                cREASON    WITH .cboReason.DISPLAYVALUE
              REPLACE nHstRate  WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax .AND. loparentformset.ariaForm1.rmcrmbus.llIsCanada , lnHstRate , 0)

            ENDIF



            IF USED('OrdLine') AND gfSeek('O'+CONSINVH.ORDER+CONSINVH.STORE+CONSINVL.STYLE,'Ordline') AND ;
                !EMPTY(Ordline.Employee)
              REPLACE Employee WITH Ordline.Employee
            ENDIF
            IF loparentformset.ariaForm1.rmcrmbus.llTax AND  loparentformset.ariaForm1.rmcrmbus.lliscanada
              IF loparentformset.ariaForm1.rmcrmbus.llStatHst
                REPLACE nPstRate WITH  0,;
                  nHstRate WITH CONSINVH.NHSTRATE ,;
                  TAX_RATE WITH 0
                .txtTax.VALUE = 0
                .txtTaxAmnt.VALUE = 0
              ELSE
                REPLACE nPstRate WITH  CONSINVH.NPSTRATE ,;
                  nHstRate WITH 0 ,;
                  TAX_RATE WITH consINVH.tax_rate
                .txtTax.VALUE = consINVH.tax_rate
              ENDIF
            ENDIF
            *-- Get the tax if england.
            IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
              REPLACE TAX_RATE WITH loparentformset.ariaForm1.rmcrmbus.mMechDisc(&lcCrMemLin..STYLE)
            ENDIF
            *-- Get the numeric value.
            REPLACE GROS_PRICE WITH CONSINVL.PRICE ;
              DISC_PCNT  WITH loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt ;
              DISC_AMT   WITH (GROS_PRICE * TOTQTY * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt / 100) ;
              TRDE_AMT   WITH AMOUNT * loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs / 100 ;
              cStyGrade  WITH IIF(loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE) , STYLE.cStyGrade , "")
            SELECT INVLINE
            IF loparentformset.ariaForm1.rmcrmbus.INVLINE.SEEK(CONSINVL.STYLE+CONSINVL.invoice+STR(CONSINVL.LINENO,6))
              SELECT (lcCrMemLin)
              lnInvRec = RECNO('INVLINE')
              REPLACE COST WITH loparentformset.ariaForm1.rmcrmbus.mcostassign(.F.) && Assign the appropriatr cost
              IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
                GO RECORD lnInvRec IN INVLINE
              ENDIF
            ENDIF
            SELECT (lcCrMemLin)
            REPLACE lShipped  WITH .T.
            *-- If the Gl linked to the current company, get the cost.
            IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
              SELECT INVLINE
              loparentformset.ariaForm1.rmcrmbus.INVLINE.SETORDER('INVLINES')
              =loparentformset.ariaForm1.rmcrmbus.INVLINE.SEEK(&lcCrMemLin..STYLE + lcInvoice)
              LOCATE REST WHILE STYLE+INVOICE+STR(LINENO,6)= &lcCrMemLin..STYLE + lcInvoice FOR DYELOT =lcSelConfg
              loparentformset.ariaForm1.rmcrmbus.INVLINE.SETORDER('INVLINE')
              SET ORDER TO INVLINE
              lcGlCost = "DEFDEF"
              IF !EMPTY(InvLine.Gl_Cost)
                lcGlCost   = InvLine.Gl_Cost
              ELSE
                IF !loparentformset.ariaForm1.rmcrmbus.llMultiWH
                  IF loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE)
                    lcGlCost =IIF(!EMPTY(STYLE.Link_Code) , STYLE.Link_Code , "DEFDEF")
                  ENDIF
                ELSE
                  *- If multi warehouse and the invoice number left empty, Get
                  *- average cost and G.L. cost link code for that warehouse.
                  IF loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(&lcCrMemLin..STYLE + lcWareCode + SPACE(10))
                    lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
                  ENDIF
                ENDIF
              ENDIF

              IF !EMPTY(INVLINE.GL_Sales)
                loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = INVLINE.GL_Sales
              ELSE
                loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
                *-- Get the customer link & the style link.
                =loparentformset.ariaForm1.rmcrmbus.CUSTOMER.SEEK("M" + lcAccount)

                IF loparentformset.ariaForm1.rmcrmbus.llDiv_Link
                  STORE '' TO lcCustLink,lcCstSlLnk
                  DECLARE laDRltFld[2,2]
                  laDRltFld[1,1] = 'LINK_CODE'
                  laDRltFld[1,2] = 'lcCustLink'
                  laDRltFld[2,1] = 'CSLSGLLINK'
                  laDRltFld[2,2] = 'lcCstSlLnk'
                  =gfRltFld(.cboDivision.VALUE , @laDRltFld,'CDIVISION')
                  loparentformset.ariaForm1.rmcrmbus.lcCustLink = lcCustLink
                  loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = lcCstSlLnk
                  loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk), PADR(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
                ELSE
                  loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
                ENDIF
              ENDIF

              SELECT (lcCrMemLin)
              loparentformset.ariaForm1.rmcrmbus.lcStySlLnk = IIF(loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE) , STYLE.cslsgllink , "")
              REPLACE GL_COST   WITH lcGlCost ;
                GL_SALES  WITH IIF(EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk))
              REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
                cICAcnt    WITH InvLine.cICAcnt    ;
                cDiscAcnt  WITH InvLine.cDiscAcnt
            ENDIF
            WITH loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail
              .grdDetail.REFRESH
              .grdDetail.AFTERROWCOLCHANGE ()
              .SbrkBackToStk.txtQty&lcSize..VALUE = &lcCrMemLin..QTY&lcSize.+1
              .txtGrsPrice.VALUE = &lcCrMemLin..GROS_PRICE
            ENDWITH
            loparentformset.AriaForm1.rmcrmbus.mvQty(lnSize,&lcCrMemLin..QTY&lcSize.)
            *-- Calculate the pieces, amount, discount, tax, pst amount.
            SELECT CONSINVL
          ENDSCAN
          .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)

        ENDIF
        SELECT CONSINVH
      ENDSCAN
      llFondLins = .T.  && Found all the invoice lines.

      SELECT INVLINE
      loparentformset.ariaForm1.rmcrmbus.INVLINE.SETORDER('INVLINE')
    ELSE
      llFondLins = .F.  && No lines have been found for the selected invoice.
    ENDIF
  ELSE
    *-- Get the lines from the invline file in case the invoice is not consolidated.
    SELECT INVLINE
    loparentformset.ariaForm1.rmcrmbus.Invline.SetOrder('INVLINE')
    IF loparentformset.ariaForm1.rmcrmbus.INVLINE.SEEK(lcInvoice)
      .txtTaxAmnt.VALUE = IIF(loparentformset.ariaForm1.rmcrmbus.llIsEnglnd , loparentformset.ariaForm1.rmcrmbus.mchgTax() , IIF(loparentformset.ariaForm1.rmcrmbus.llTax , InvHdr.Tax_Amt , 0))
      .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)

      loparentformset.ariaForm1.rmcrmbus.lnDueTAX = 0
      IF loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0
        loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 1
      ELSE
        loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo + 1
      ENDIF
      lnLineCnt = loparentformset.ariaForm1.rmcrmbus.lnCrMemNo
      SELECT INVLINE
      *-- Loop in the invoice lines to be sure of its existance in the style file.
      SCAN REST WHILE INVLINE.Invoice = lcInvoice FOR STYLE = lcSelStyle AND ;
          DYELOT = lcSelConfg AND loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(INVLINE.STYLE)
        SELECT (lcCrMemLin)
        lnPstRate  = loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtPstRat.VALUE
        lnHstRate  = loparentformset.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtHstRat.VALUE
        IF !SEEK(INVLINE.ACCOUNT+INVLINE.STYLE,lcCrMemLin,lcCrMmStyl)
          APPEND BLANK
          REPLACE ACCOUNT    WITH INVLINE.ACCOUNT ;
            STYLE      WITH INVLINE.STYLE ;
            DESC       WITH STYLE.DESC,;
            CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
            CRET_TRNCD WITH "2" ;
            INVOICE    WITH INVLINE.INVOICE ;
            CRDATE     WITH IIF(!EMPTY(.DtCrdDate.Text1.VALUE) , .DtCrdDate.Text1.VALUE , oAriaApplication.SystemDate) ;
            PRICE      WITH INVLINE.PRICE*(1-loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100) ;
            AMOUNT     WITH TOTQTY * (INVLINE.PRICE*(1-loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100)) ;
            DYELOT     WITH INVLINE.DYELOT ;
            nPstRate   WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax .AND. loparentformset.ariaForm1.rmcrmbus.llIsCanada , lnPstRate , 0) ;
            TAX_RATE   WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax , .txtTax.VALUE, 0) ;
            lInvoice   WITH loparentformset.ariaForm1.rmcrmbus.llInsrtInv
          REPLACE REASON     WITH .cboReason.VALUE,;
            cREASON    WITH .cboReason.DISPLAYVALUE
          REPLACE nHstRate   WITH IIF(loparentformset.ariaForm1.rmcrmbus.llTax .AND. loparentformset.ariaForm1.rmcrmbus.llIsCanada , lnHstRate , 0)
        ENDIF

        *-- Lock the record to grantee the phiscal update.

        *-- Changed the calculating of AMOUNT for
        *-- each ret. line to be on Price before deducting
        *-- the discount and calcuating the discount on
        *-- the total merch. of the line.


        IF USED('OrdLine') AND  gfSeek('O'+INVLINE.ORDER+INVLINE.STORE+INVLINE.STYLE,'Ordline') AND ;
            !EMPTY(Ordline.Employee)
          REPLACE Employee WITH Ordline.Employee
        ENDIF

        IF loparentformset.ariaForm1.rmcrmbus.llTax AND  loparentformset.ariaForm1.rmcrmbus.lliscanada
          IF loparentformset.ariaForm1.rmcrmbus.llStatHst
            REPLACE nPstRate WITH  0,;
              nHstRate WITH InvHdr.NHSTRATE ,;
              TAX_RATE WITH 0
            .txtTax.VALUE = 0
            .txtTaxAmnt.VALUE =  0
          ELSE
            REPLACE nPstRate WITH InvHdr.NPSTRATE ,;
              nHstRate WITH 0 ,;
              TAX_RATE WITH InvHdr.tax_rate
            .txtTax.VALUE = InvHdr.tax_rate
            .txtTaxAmnt.VALUE =  InvHdr.Tax_Amt
          ENDIF
        ENDIF

        *-- Save correct line tax if england.
        IF loparentformset.ariaForm1.rmcrmbus.llIsEnglnd
          REPLACE TAX_RATE WITH loparentformset.ariaForm1.rmcrmbus.mMechDisc(&lcCrMemLin..STYLE)
        ENDIF

        lnInvRec = RECNO('INVLINE')
        REPLACE GROS_PRICE WITH INVLINE.PRICE ;
          DISC_PCNT  WITH loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt ;
          DISC_AMT   WITH (&lcCrMemLin..GROS_PRICE * &lcCrMemLin..TOTQTY * loparentformset.ariaForm1.rmcrmbus.lnDiscPcnt/100) ;
          TRDE_AMT   WITH &lcCrMemLin..AMOUNT * loparentformset.ariaForm1.rmcrmbus.lnInvTrdDs/100 ;
          COST       WITH loparentformset.ariaForm1.rmcrmbus.mcostassign(.F.) ;
          cStyGrade  WITH IIF(loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE) , STYLE.cStyGrade , "")

        IF BETWEEN(lnInvRec,1,RECCOUNT('INVLINE'))
          GO RECORD lnInvRec IN INVLINE
        ENDIF
        SELECT (lcCrMemLin)
        REPLACE lShipped  WITH .T.

        *-- If there is Gl linked to the current company, get the cost.
        IF loparentformset.ariaForm1.rmcrmbus.llLink_GL
          lcGlCost = "DEFDEF"
          IF !EMPTY(InvLine.Gl_Cost)
            lcGlCost   = InvLine.Gl_Cost
          ELSE
            IF !loparentformset.ariaForm1.rmcrmbus.llMultiWH
              IF loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE)
                lcGlCost =IIF(!EMPTY(STYLE.Link_Code) , STYLE.Link_Code , "DEFDEF")
              ENDIF
            ELSE
              *- If multi warehouse and the invoice number left empty, Get
              *- average cost and G.L. cost link code for that warehouse.
              IF loparentformset.ariaForm1.rmcrmbus.StyDye.SEEK(&lcCrMemLin..STYLE + lcWareCode + lcSelConfg)
                lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
              ENDIF
            ENDIF
          ENDIF

          lcCstSlLnk = "DEF"
          IF !EMPTY(INVLINE.GL_Sales)
            loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = INVLINE.GL_Sales
          ELSE
            loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
            *-- Get the customer link & the style link.
            =loparentformset.ariaForm1.rmcrmbus.CUSTOMER.SEEK("M" + lcAccount)

            IF loparentformset.ariaForm1.rmcrmbus.llDiv_Link
              STORE '' TO lcCustLink,lcCstSlLnk
              DECLARE laDRltFld[2,2]
              laDRltFld[1,1] = 'LINK_CODE'
              laDRltFld[1,2] = 'lcCustLink'
              laDRltFld[2,1] = 'CSLSGLLINK'
              laDRltFld[2,2] = 'lcCstSlLnk'
              =gfRltFld(.cboDivision.VALUE , @laDRltFld,'CDIVISION')
              loparentformset.ariaForm1.rmcrmbus.lcCustLink = lcCustLink
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = lcCstSlLnk
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk), PADR(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
            ELSE
              loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
            ENDIF
          ENDIF

          loparentformset.ariaForm1.rmcrmbus.lcStySlLnk = IIF(loparentformset.ariaForm1.rmcrmbus.STYLE.SEEK(&lcCrMemLin..STYLE) , STYLE.cslsgllink , "")
          REPLACE GL_COST  WITH lcGlCost ;
            GL_SALES WITH IIF(EMPTY(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcCstSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk) , ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk) + ALLTRIM(loparentformset.ariaForm1.rmcrmbus.lcStySlLnk))
          REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
            cICAcnt    WITH InvLine.cICAcnt    ;
            cDiscAcnt  WITH InvLine.cDiscAcnt
        ENDIF

        WITH loparentformset.ariaForm1.pgfReCrmem.pgDetail.cntDetail
          .grdDetail.REFRESH
          .grdDetail.AFTERROWCOLCHANGE ()
          .SbrkBackToStk.txtQty&lcSize..VALUE = &lcCrMemLin..QTY&lcSize.+1
          .txtGrsPrice.VALUE = &lcCrMemLin..GROS_PRICE
        ENDWITH
        loparentformset.AriaForm1.rmcrmbus.mvQty(lnSize,&lcCrMemLin..QTY&lcSize.)

        *-- Calculate the pieces, amount, discount, tax, pst amount.
        lnLineCnt = lnLineCnt + 1
        SELECT INVLINE
      ENDSCAN
      .txtTaxAmnt.VALUE = ROUND(.txtTaxAmnt.VALUE,2)
      llFondLins = .T.  && Found all the invoice lines.
    ELSE
      llFondLins = .F.  && No lines have been found for the selected invoice.
    ENDIF
  ENDIF

  *-- If there is no lines for the current invoice, give the user a warning message.
  IF !llFondLins
    *** The invoice lines are missing. ***
    *** <  Ok  > ***
    =gfModalGen("INM46002B00000" , "DIALOG")
  ELSE
    *-- Count the inserted lines in the current credit memo.
    loparentformset.AriaForm1.rmcrmbus.lnCrMemNo  = lnLineCnt - 1
    loparentformset.AriaForm1.rmcrmbus.lnCrLinCnt = lnLineCnt - 1

    *-- Calculate the cahrge back.
    IF loparentformset.AriaForm1.rmcrmbus.lnInvTrdDs <> 0
      .txtChrgBckRep1.VALUE = ROUND((loparentformset.AriaForm1.rmcrmbus.lnamount * (100 - loparentformset.AriaForm1.rmcrmbus.lnInvTrdDs)/100) * (.spnRepComm1.VALUE/100),2) * -1
      .txtChrgBckRep2.VALUE = ROUND((loparentformset.AriaForm1.rmcrmbus.lnamount * (100 - loparentformset.AriaForm1.rmcrmbus.lnInvTrdDs)/100) * (.spnRepComm2.VALUE/100),2) * -1
    ELSE
      .txtChrgBckRep1.VALUE = ROUND(loparentformset.AriaForm1.rmcrmbus.lnamount * (.spnRepComm1.VALUE/100),2) * -1
      .txtChrgBckRep2.VALUE = ROUND(loparentformset.AriaForm1.rmcrmbus.lnamount * (.spnRepComm2.VALUE/100),2) * -1
    ENDIF
    .txtOpenPcs.VALUE  = MAX(loparentformset.AriaForm1.rmcrmbus.lnRAQty - .txtReceivedPcs.VALUE , 0)
    .txtOpenAmnt.VALUE = MAX(loparentformset.AriaForm1.rmcrmbus.lnRAAmt - .txtReceivedAmnt.VALUE , 0)
    *-- calculate the total credit amount.
    .txtTotCredit.VALUE = .txtCharges.VALUE + loparentformset.AriaForm1.rmcrmbus.lnamount + .txtTaxAmnt.VALUE + .txtPstTax.VALUE + .txtHstTax.VALUE

    *-- Disable or enable the objects related to the selected
    *-- Invoice # & existance of lines.
    *-- Refresh R/A objects.

    loparentformset.AriaForm1.llRAStat  = IIF(loparentformset.ActiveMode = 'A' .AND. loparentformset.AriaForm1.rmcrmbus.lnCrMemNo = 0 , .T. , .F.)
    *-- Refresh invoice objects.
    loparentformset.AriaForm1.llInvStat = IIF(loparentformset.ActiveMode = 'A' .AND. loparentformset.AriaForm1.rmcrmbus.lnCrMemNo = 0 , .T. , .F.)
    *-- Refresh order objects.
    loparentformset.AriaForm1.llOrdStat = IIF(EMPTY(lcOrder) .AND. EMPTY(lcRetVal) .AND. loparentformset.ActiveMode = 'A' .AND. loparentformset.AriaForm1.rmcrmbus.lnCrMemNo = 0 , .T. , .F.)
    *-- Refresh check box "return entire invoice".
    loparentformset.AriaForm1.llEntrStat= IIF(loparentformset.ActiveMode = 'A' .AND. loparentformset.AriaForm1.rmcrmbus.lnCrMemNo = 0 AND !EMPTY(lcInvoice), .T. , .F.)
    *-- Refresh inquire invoice button.
    loparentformset.AriaForm1.llInqStat = IIF(!EMPTY(lcInvoice) .AND. loparentformset.ActiveMode <> 'S' , .T. , .F.)
    *-- Refresh the division popup.
    loparentformset.AriaForm1.llDivStat = IIF(loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. IIF(loparentformset.AriaForm1.rmcrmbus.llDiv_Seq , EMPTY(lcRetVal) , .T.) .AND. loparentformset.ariaForm1.rmcrmbus.lnCrMemNo = 0 AND (EMPTY(lcRetVal) OR .llFoldFrst), .T. , .F.)
    *-- Refresh currency code objects.
    loparentformset.AriaForm1.llCurStat = IIF(loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. !loparentformset.AriaForm1.rmcrmbus.RALINE.SEEK(lcRetVal) , .T. , .F.)
    *-- Refresh exchange rate object.
    loparentformset.AriaForm1.llExRStat = IIF(loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. !loparentformset.AriaForm1.rmcrmbus.RALINE.SEEK(lcRetVal) .AND. !EMPTY(.kbCurrency.keytextbox.VALUE) .AND. loparentformset.ariaForm1.rmcrmbus.llEditExRt .AND. .kbCurrency.keytextbox.VALUE <> oAriaApplication.BaseCurrency, .T. , .F.)
    *-- Refresh GL link objects.
    loparentformset.AriaForm1.llGLStat  = IIF(loparentformset.ActiveMode = 'A' .AND. EMPTY(lcInvoice) .AND. loparentformset.AriaForm1.rmcrmbus.llLink_GL .AND. loparentformset.AriaForm1.rmcrmbus.llDiv_Link , .T. , .F.)
    *-- Set flag to execute the browse.
    loparentformset.AriaForm1.llReBrowse = .T.

    SELECT (lcCrMemLin)
    GO TOP
  ENDIF
  loparentformset.ariaForm1.rmcrmbus.lcInvSlLnk = ""
ENDWITH
loparentformset.AriaForm1.mShowObj()

*!*************************************************************
*! Name      : lfRMCHNMODE
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Rest llFrstChk Value in Add Mode (Credit memo Screen)
*!*************************************************************
FUNCTION lfRMCHNMODE
IF TYPE('loFormSet.llFrstChk') = 'U'
  loFormSet.ADDPROPERTY('llFrstChk',.F.)
ENDIF
IF loFormSet.ActiveMode = 'A'
  loFormSet.llFrstChk = .F.
ENDIF
*!*************************************************************
*! Name      : lfMACTFLODR
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : refresh grid in Credit memo Screen
*!*************************************************************
FUNCTION lfMACTFLODR
lcCrMemLin = loFormSet.ariaForm1.rmcrmbus.lcCrMemLin
SELECT (lcCrMemLin)
GO TOP
loFormSet.ARIAFORM1.PGFRECRMEM.PgDetail.CntDetail.GRdDetail.SETFOCUS()
*!*************************************************************
*! Name      : lfScanConfg
*! Developer : Mariam Mazhar
*! Date      : 02/01/2010
*! Purpose   : Scan Confg. in Style Screen
*!*************************************************************
FUNCTION lfScanConfg
loParentForm = _SCREEN.ACTIVEFORM.PARENT
lcSelWareHouse = ''
lcConfg = ''
lcStyleSel = ''
*! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
IF oAriaApplication.MULTIINST
  DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH loParentForm,'S'
ELSE
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
  DO FORM (oAriaApplication.ScreenHome + 'POSNCBR.scx') WITH loParentForm,'S'
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
ENDIF
*! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
WITH loParentForm.Ariaform1.pgfStyleInfo.page2
  IF !EMPTY(lcSelWareHouse) AND (lcSelWareHouse <> "******") AND gfSeek(PADR(lcStyleSel ,19)+PADR(lcSelWareHouse ,6)+PADR(lcConfg ,10),'STYDYE','STYDYE')
    .cboLocation.VALUE = lcSelWareHouse
    .cboLocation.VALID()
  ELSE
    .cboLocation.VALUE = "******"
    .cboLocation.VALID()
  ENDIF
  IF !EMPTY(lcConfg)
    .cboDyelot.VALUE = lcConfg
    .cboDyelot.VALID()
  ENDIF
ENDWITH
*! C201210,3 MMT 02/01/2010 Add New Triggers to Scan Barcode For FLO09 in Manul Credit memo and Style Screen[End]

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen[Start]
*!*************************************************************************************************
*! Name      : lfScanBarcd
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Open Scanner Screen to Scan Barcodes
*!*************************************************************************************************
FUNCTION lfScanBarcd

loParentForm = _SCREEN.ACTIVEFORM.PARENT

=lfPrepData()

IF loParentForm.llDiff
  RETURN
ENDIF

IF EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
  lnRes = gfModalGen('QRM34126B34216','DIALOG')
  loParentForm.lcAuto = IIF(lnRes = 1,'M',IIF(lnRes = 2,'A','S'))
  IF loParentForm.lcAuto = 'S'
    loParentForm.llFbzProc = .T.
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
    IF oAriaApplication.MULTIINST
      DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH loParentForm,'R'
    ELSE
      *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
      DO FORM (oAriaApplication.ScreenHome+'POSNCBR.SCX') WITH loParentForm,'R'
      *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
    ENDIF
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
  ELSE
    loParentForm.llFbzProc = .F.
  ENDIF
ELSE
  lnRes = gfModalGen('QRM34216B34217','DIALOG')
  IF lnRes = 2
    loParentForm.llFbzProc = .T.
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
    IF oAriaApplication.MULTIINST
      DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH  loParentForm,'R'
    ELSE
      *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
      DO FORM (oAriaApplication.ScreenHome+'POSNCBR.SCX') WITH loParentForm,'R'
      *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
    ENDIF
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
  ELSE
    loParentForm.llFbzProc = .F.
    RETURN .F.
  ENDIF
ENDIF
* End of lfScanBarcd

*!*************************************************************************************************
*! Name      : lfVScnBrCd
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Validate Scanned Barcode
*!*************************************************************************************************
FUNCTION lfVScnBrCd
PARAMETERS lcBrCode, loParentForm

lcStyle  = ""
lcConfig = ""

SELECT CONFGUPC
gfSeek(lcBrCode)
IF FOUND()
  lcStyle  = STYLE
  lcConfig = Dyelot
  lnSize   = SIZE
  IF EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
    SELECT POSLN
    lcOrder = ORDER()
    gfSetOrder('POSLNS')
    lcKey = "0001"+lcStyle+"PP"+loParentForm.AriaForm1.kbPONo.KeyTextBox.VALUE
    IF gfSeek(lcKey)
      LOCATE REST WHILE "0001"+STYLE+CSTYTYPE+CBUSDOCU+PO = lcKey FOR Dyelot = lcConfig
      IF FOUND()
        && Function in FLOMAIN.PRG to process Scanned Barcode from Reciev PO screen -By PO- .
        =lfRecScand(lcKey,lnSize,lcConfig,loParentForm)
      ELSE
        gfModalGen('TRM34220B00000','DIALOG')
      ENDIF
    ELSE
      gfModalGen('TRM34218B00000','DIALOG')
    ENDIF
  ELSE
    && Function in FLOMAIN.PRG to process Scanned Barcode from Reciev PO screen -By Shipment- .
    RETURN lfRecShScnd(lnSize,loParentForm)
  ENDIF
ELSE
  gfModalGen('TRM34217B00000','DIALOG')
ENDIF
* End of lfVScnBrCd()

*!*************************************************************************************************
*! Name      : lfRecScand
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Validate Scanned Barcode and automatically recieved its STYLE-CONFIG line from POSLN
*!*************************************************************************************************
FUNCTION lfRecScand
PARAMETERS lcKey,lnSize,lcConf,loParentForm

*! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [Start]
lnSize = VAL(lnSize)
loParentForm.lnFbzSize = lnSize
lcLstPO = ''
*! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [End]

lcPosHdr = loParentForm.lcPosHdr
=lfGetBomLn(loParentForm,&lcPosHdr..cStyType, &lcPosHdr..PO, PADR(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE,6))
lcCarton = ''

SELECT (loParentForm.lcPosLn)
*!*	IF loParentForm.lcAuto = 'S'
loParentForm.lcAuto = 'A'
lcFTrnCd = IIF(loParentForm.lcPType$'OE','6','1')
SCAN FOR Trancd = lcFTrnCd AND "0001"+STYLE+CSTYTYPE+CBUSDOCU+PO = lcKey AND Dyelot = lcConf
  loParentForm.lcStyle   = STYLE
  lcLstPO = PO
  *-- Get the style information
  IF loParentForm.lcInvType = "0001"
    lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loParentForm.lcStyle + "'"
  ELSE
    lcItemValue = loParentForm.lcStyle
    lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
      "' AND Style = ?m.lcItemValue"
  ENDIF
  =lfGetItmInf( loParentForm.lcInvType,;
    IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+loParentForm.lcStyle,;
    loParentForm.lcTmpItem,;
    IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
    loParentForm.lcInvType = "0002")
  =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+loParentForm.lcStyle,;
    loParentForm.lcTmpItem)
  SELECT (loParentForm.lcPosLn)
  lnRecNo   = RECNO()
  IF SEEK('1'+SPACE(3)+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6),loParentForm.lcTmpLine)

    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [Start]
    loParentForm.llFbzFound = .T.
  ELSE
    loParentForm.llFbzFound = .F.
    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [End]

  ENDIF
  =lfvItem(loParentForm,LINENO)
  loParentForm.lcAuto = 'S'
  SELECT (loParentForm.lcPosLn)
  GOTO lnRecNo
  IF loParentForm.lcAuto = 'X'
    *--The style xxxxx will be ignored.
    = gfModalGen('INM34203B42000','DIALOG',;
      IIF(loParentForm.lcInvType='0001','Style','Fabric')+ALLTRIM(loParentForm.lcStyle))
    loParentForm.lcAuto = 'A'
  ENDIF
ENDSCAN
SELECT (loParentForm.lcTmpLine)
LOCATE FOR PO = lcLstPO
IF !FOUND()
  LOCATE
ENDIF
=lfActBrow(loParentForm)
=lfwBrow(loParentForm)
STORE .F. TO loParentForm.AriaForm1.dtPickerPostingDate.ENABLED   ,;
  loParentForm.AriaForm1.dtpickerReceivingDate.ENABLED ,;
  loParentForm.AriaForm1.cboReceivingTypes.ENABLED
SELECT(loParentForm.lnCAlias)
loParentForm.llDone = .T.
*!*	ENDIF
* End of lfRecScand

*!*************************************************************************************************
*! Name      : lfRecShScnd
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Validate Scanned Barcode and automatically recieved its STYLE-CONFIG line from POSLN
*!*************************************************************************************************
FUNCTION lfRecShScnd
PARAMETERS lnSize, loParentForm

lnSize = VAL(lnSize)
loParentForm.lnFbzSize = lnSize

SELECT (loParentForm.lcPosLn)
RECALL ALL
LOCATE FOR STYLE = lcStyle AND Dyelot = lcConfig
IF FOUND()
  SELECT (loParentForm.lcPosLn)
  DELETE FOR STYLE <> lcStyle AND Dyelot <> lcConfig
  loParentForm.llDone = .T.
  SELECT (loParentForm.lcPosLn)
  LOCATE

  *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [Start]
  IF SEEK('1'+SPACE(3)+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6),loParentForm.lcTmpLine)
    loParentForm.llFbzFound = .T.
  ELSE
    loParentForm.llFbzFound = .F.
  ENDIF
  *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [End]

  IF EOF()
    IF loParentForm.lcPType = 'C'
      = gfModalGen('TRM34205B42000','DIALOG')
    ELSE
      *--The shipment lines have not been found! unable to proceed.
      = gfModalGen('TRM34077B42000','DIALOG')
    ENDIF
    loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE = ""
    RETURN .F.
  ENDIF
  =lfGetBomLn(loParentForm,'P', '', loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
  llShpPO = .F.
  llRet = lfGetInfo(loParentForm,.F.)
  RETURN llRet
ELSE
  gfModalGen('TRM34219B00000','DIALOG')
ENDIF
* End of lfRecShScnd()

*!*************************************************************************************************
*! Name      : lfMNGFNDLN1
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Manage found line in lcTmpLine while TranTyp = 1
*!*************************************************************************************************
FUNCTION lfMNGFNDL1
PARAMETERS llEmpWare, loParentForm

SELECT (loParentForm.lcTmpLine)
IF EOF()
  IF EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
    lcPO = loParentForm.AriaForm1.KbPONo.Keytextbox.VALUE
    LOCATE FOR PO = lcPO
  ENDIF
ENDIF

IF loParentForm.llFbzProc
  IF !loParentForm.llFbzFound
    APPEND BLANK
  ENDIF
ELSE
  IF !EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
    APPEND BLANK
  ELSE
    APPEND BLANK
    m.cWareCode = IIF(llEmpWare,'',m.cWareCode)
  ENDIF
ENDIF
*End of lfMNGFNDLN

*!*************************************************************************************************
*! Name      : lfMNGFNDLN2
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Manage found line in lcTmpLine while TranTyp = 2
*!*************************************************************************************************
FUNCTION lfMNGFNDL2
PARAMETERS loParentForm

IF loParentForm.llFbzProc
  LOCAL lnSize, lnOrgQty, lnRecNo, lnTotStk, lnOrgStk, lnRevRec, lnCurStk, lnTotBal, lnOrgStks, lnCurStks, lnTotls,;
    lnFvl, lnOrStk, llhasVl, lcFPO, lcFStyle, lcFDylt, lcFWare, lnFLinNo, lnBudLn

  STORE 0 TO lnSize, lnOrgQty, lnRecNo, lnTotStk, lnOrgStk, lnRevRec, lnCurStk, lnTotBal, lnOrgStks, lnCurStks, ;
    lnTotls, lnOrStk, lnBudLn
  STORE 1 TO lnFvl
  STORE .F. TO llhasVl
  DIMENSION laStks[2,8]
  DIMENSION laFStks[2,8]
  laStks  = 0
  laFStks = 0

  SELECT (loParentForm.lcPosLn)
  lnFLno = RECNO()
  lcFPO  = PO
  lcFStyle = STYLE
  lcFDylt = Dyelot
  lcFWare = cWareCode
  lnFLinNo = STR(LINENO,6)
  lnOrStk = TotQty
  LOCATE FOR  TRANCD+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6) = '2'+lcFPO+lcFStyle+lcFDylt+lcFWare+lnFLinNo
  IF FOUND()
    llhasVl = .T.
    lnFvl = 0
    lnLlNo = RECNO()
    GOTO lnFLno
    FOR lnY = 1 TO 8
      lcY = ALLTRIM(STR(lnY))
      laFStks[1,lnY] = QTY&lcY
    ENDFOR
    SCAN FOR TRANCD+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6) = '2'+lcFPO+lcFStyle+lcFDylt+lcFWare+lnFLinNo
      FOR lnX = 1 TO 8
        lcX = ALLTRIM(STR(lnX))
        laFStks[2,lnX] = laFStks[2,lnX] + QTY&lcX
      ENDFOR
    ENDSCAN
    FOR lnZ = 1 TO 8
      lnFvl = lnFvl + MIN(laFStks[1,lnZ],laFStks[2,lnZ])
    ENDFOR
  ENDIF

  SELECT (loParentForm.lcTmpLine)
  SET FILTER TO
  lnRecNo = RECNO()
  lnSize = loParentForm.lnFbzSize

  loParentForm.lnMaxSiz = MAX(loParentForm.lnMaxSiz,lnSize)

  lcSize = ALLTRIM(STR(lnSize))
  IF !loParentForm.llFbzFound
    lnTSvRc = RECNO()
    IF llhasVl
      REPLACE TotBal WITH lnOrStk - lnFvl ;
        TotStk WITH 1
    ELSE
      REPLACE TotBal WITH TotStk - 1 ;
        TotStk WITH 1
    ENDIF
    SCATTER MEMVAR
    APPEND BLANK
    GATHER MEMVAR
    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [Start]
    FOR lnX = 1 TO 8
      lcX = ALLTRIM(STR(lnX))
      IF lnSize = lnX
        REPLACE QTY&lcX WITH 1 ;
          TotQty  WITH 1
      ELSE
        REPLACE QTY&lcX WITH 0
      ENDIF
    ENDFOR
    IF !EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
      REPLACE TranCd WITH IIF(loParentForm.lcWorkOrd = 'U','6','2')
      GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    ELSE
      REPLACE TranCd WITH "2"
    ENDIF
    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [End]
    IF BETWEEN(lnTSvRc,1,RECCOUNT())
      GOTO lnTSvRc
    ENDIF
  ELSE
    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [Start]
    SELECT (loParentForm.lcPosLn)
    GOTO lnFLno
    IF SEEK('1'+SPACE(3)+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6),loParentForm.lcTmpLine)
      SELECT (loParentForm.lcTmpLine)
      lnBudLn = RECNO()
      lnOrgQty = QTY&lcSize
      lnOrgStk = TotStk
      FOR lnY = 1 TO loParentForm.lnMaxSiz
        lcY = ALLTRIM(STR(lnY))
        laStks[1,lnY] = QTY&lcY
      ENDFOR
    ENDIF
    SELECT (loParentForm.lcPosLn)
    IF SEEK('2'+SPACE(3)+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6),loParentForm.lcTmpLine)
      SELECT (loParentForm.lcTmpLine)
      lnRevRec = RECNO()
      SELECT (loParentForm.lcTmpLine)
      REPLACE QTY&lcSize WITH QTY&lcSize + 1 ;
        TotQty     WITH TotQty     + 1 ;
        TotStk     WITH TotStk     + 1
      lnTotStk = TotStk
      lnCurStk = QTY&lcSize

      && To calculate the right Balance
      FOR lnX = 1 TO loParentForm.lnMaxSiz
        lcX = ALLTRIM(STR(lnX))
        laStks[2,lnX] = QTY&lcX
      ENDFOR
      FOR lnZ = 1 TO loParentForm.lnMaxSiz
        lnTotls = lnTotls + MIN(laStks[1,lnZ],laStks[2,lnZ])
      ENDFOR

      IF !EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
        REPLACE TranCd WITH IIF(loParentForm.lcWorkOrd = 'U','6','2')
        GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
      ELSE
        REPLACE TranCd WITH "2"
      ENDIF
    ENDIF
    GOTO lnBudLn
    REPLACE TotStk WITH lnTotStk ;
      TotBal WITH lnOrgStk - lnTotls
    *! C201211,2 HES 01/20/2010 Handle Scanning Barcod to be per piece [End]
  ENDIF
  SELECT (loParentForm.lcTmpLine)
  loParentForm.lnTotStk = 0
  SCAN FOR !EOF() AND TranCd = "2"
    loParentForm.lnTotStk = loParentForm.lnTotStk + TotStk
  ENDSCAN
  GOTO lnRecNo
ELSE
  && Standard way
  IF !EMPTY(loParentForm.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE)
    loParentForm.lnTotStk = loParentForm.lnTotStk + TotStk
    SCATTER MEMVAR
    APPEND BLANK
    GATHER MEMVAR
    REPLACE TranCd WITH IIF(loParentForm.lcWorkOrd = 'U','6','2')
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
  ELSE
    SCATTER MEMVAR
    APPEND BLANK
    GATHER MEMVAR
    REPLACE TranCd WITH '2'
    IF BETWEEN(lnTSvRc,1,RECCOUNT())
      GOTO lnTSvRc
    ENDIF
    loParentForm.lnTotStk = loParentForm.lnTotStk + TotStk
  ENDIF
ENDIF
*End of lfMNGFNDLN

*!*************************************************************************************************
*! Name      : lfSCNMENOP
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Add Scan barcode Option
*!*************************************************************************************************
FUNCTION lfSCNMENOP

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo  OF _OPTIONPOP PROMPT 'Scan Configuration Barcode' SKIP FOR !(_SCREEN.ACTIVEFORM.PARENT.ActiveMode = "A")
ON SELECTION BAR lnBarNo  OF _OPTIONPOP DO lfSCNFRM
* End of lfSCNMENOP

*!*************************************************************************************************
*! Name      : lfSCNFRM
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Appearing the Scan Screen from Option menu
*!*************************************************************************************************
FUNCTION lfSCNFRM

loParentForm = _SCREEN.ACTIVEFORM.PARENT
SET DATASESSION TO (loParentForm.DATASESSIONID)

IF !USED('SYUSTATC')
  = gfOpenTable(oAriaApplication.DataDir + 'SYUSTATC' ,'CUSER_ID','SH')
ENDIF

=lfPrepData()

SELECT (loParentForm.lcPosLn)
IF EOF()
  LOCATE
ENDIF

lcPO = PO && The last selected and valid PO#
*-* when the grid contains lines for more than one PO and the the user select a line related to specific PO
*-*  from the Grid and press Option to choose "Scan Configuration Barcode", we have to check if this choosen
*-*   PO for the related line is the PO selected and validated from the kbPo# or not, because if it differenet, we
*-*    we have to revalidate this PO to update the TEMPs used by the POSTREC program and POSREC screen.
IF lcPO <> loParentForm.AriaForm1.KbPONo.Keytextbox.VALUE
  loParentForm.lcAuto = 'S'
  loParentForm.llDiff = .T.
  =lfVPO(loParentForm,.F.)
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
  IF oAriaApplication.MULTIINST
    DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH  loParentForm,'R'
  ELSE
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
    DO FORM (oAriaApplication.ScreenHome+'POSNCBR.SCX') WITH loParentForm,'R'
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
  ENDIF
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
  loParentForm.llDiff = .F.
ELSE
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
  IF oAriaApplication.MULTIINST
    DO FORM ("X:\Aria4xp\Screens\"+ 'POSNCBR.scx') WITH  loParentForm,'R'
  ELSE
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
    DO FORM (oAriaApplication.ScreenHome+'POSNCBR.SCX') WITH loParentForm,'R'
    *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[Start]
  ENDIF
  *! B609158,1 MMT 03/01/2010 calling the Scan screen in Custom FBZ program gives error[End]
ENDIF
* End of lfSCNFRM

*!*************************************************************************************************
*! Name      : lfPrepData
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : Preparing data
*!*************************************************************************************************
FUNCTION lfPrepData

loParentForm = _SCREEN.ACTIVEFORM.PARENT

IF !USED('POSHDR')
  = gfOpenTable(oAriaApplication.DataDir + 'POSHDR' ,'POSHDR','SH')
ENDIF
IF !USED('POSLN')
  = gfOpenTable(oAriaApplication.DataDir + 'POSLN' ,'POSLNS','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenTable(oariaapplication.datadir +'STYLE',oariaapplication.datadir +'STYLE','SH')
ENDIF
IF !USED('STYDYE')
  =gfOpenTable(oariaapplication.datadir +'STYDYE',oariaapplication.datadir +'STYDYE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenTable(oariaapplication.datadir +'SCALE',oariaapplication.datadir +'SCALE','SH')
ENDIF
IF !USED('CONFGUPC')
  =gfOpenTable(oariaapplication.datadir +'CONFGUPC',oariaapplication.datadir +'confgupcup','SH')
ENDIF

IF TYPE('loParentForm.llDone') = 'U'
  && Used to check if scanned Barcode valid and processed or not
  loParentForm.ADDPROPERTY('llDone',.F.)
ENDIF
IF TYPE('loParentForm.llFbzFirst') = 'U'
  loParentForm.ADDPROPERTY('llFbzFirst',.T.)
ENDIF
IF TYPE('loParentForm.llFbzProc') = 'U'
  loParentForm.ADDPROPERTY('llFbzProc',.F.)
ENDIF
IF TYPE('loParentForm.llFbzFound') = 'U'
  loParentForm.ADDPROPERTY('llFbzFound',.F.)
ENDIF
IF TYPE('loParentForm.lnFbzSize') = 'U'
  loParentForm.ADDPROPERTY('lnFbzSize',0)
ENDIF

IF TYPE('loParentForm.lnMaxSiz') = 'U'
  loParentForm.ADDPROPERTY('lnMaxSiz',0)
ENDIF

IF TYPE('loParentForm.llDiff') = 'U'
  loParentForm.ADDPROPERTY('llDiff',.F.)
ENDIF

* End of lfPrepData()

*!*************************************************************************************************
*! Name      : lfVLDEXIST
*! Developer : Hesham Elmasry (HES)
*! Date      : 01/20/2010
*! Purpose   : validate Exist message
*!*************************************************************************************************
FUNCTION lfVLDEXIST

loParentForm = _SCREEN.ACTIVEFORM.PARENT
lcLstPO = loParentForm.AriaForm1.KbPONo.Keytextbox.VALUE

IF loParentForm.llDiff
  RETURN .T.
ELSE
  SELECT (loParentForm.lcTmpLine)
  LOCATE FOR PO = lcLstPO
  IF !FOUND()
    LOCATE
  ENDIF
  =lfActBrow(loParentForm)
  =lfwBrow(loParentForm)
  STORE .F. TO loParentForm.AriaForm1.dtPickerPostingDate.ENABLED   ,;
    loParentForm.AriaForm1.dtpickerReceivingDate.ENABLED ,;
    loParentForm.AriaForm1.cboReceivingTypes.ENABLED
  RETURN .F.
ENDIF
* End of lfVLDEXIST()
*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen[End]

*! E303165,1 04/06/2012 HIA, Don't rload query and enable filter in view mode  [T20120424.0007][Begin]
*!*************************************************************************************************
*! Name      : lfaddcfg
*! Developer : Hassan Ibrahim Ali (HIA)
*! Date      : 05/05/2012
*! Purpose   : Add confg for new styles, depend on message
*!*************************************************************************************************
FUNCTION lfADDCFG

loParentForm = _SCREEN.ACTIVEFORM.PARENT
*! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][Start]
lcOldActiveMode = loParentForm.ActiveMode
*! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][End]
IF  gfModalGen("QRM52040B32000","ALERT") = 1

  SELECT STYLE
  *B610061,1 MMT 08/28/2012 Cannot add new style if selected to add default configuration[T20120625.0019][Start]
  =gfTableUpdate()
  *B610061,1 MMT 08/28/2012 Cannot add new style if selected to add default configuration[T20120625.0019][End]
  lcpStyle = loParentForm.lcstylekey && test11-
  lcMajor  = SUBSTR(lcpStyle,1,loParentForm.lnstylewid)
  *--Read NonMAjor
  *!*	  lcpColor = padr("",loParentForm.lnColorWid,"*")
  *!*	  lcNonMjr = IIF(!EMPTY(lcpColor),lcpColor,;
  *!*	    IIF(!EMPTY(SUBSTR(lcpStyle,loParentForm.lnstylewid+2,loParentForm.lnColorWid)),SUBSTR(lcpStyle,loParentForm.lnstylewid+2,loParentForm.lnColorWid), ""))

  =gfSeek(lcMajor)

  IF FOUND()  && WSH
    *--Get style information.
    loParentForm.mGetInformation(lcMajor,'','V')
    *--Start in view mode to inquire the passed style.
    loParentForm.ChangeMode('V')
    oldvalue = loParentForm.llGenAut

    loParentForm.llGenAut = .T.
    llGenAut = loParentForm.llGenAut
    =lfGenConf()
    loParentForm.llGenAut = oldvalue
    llGenAut = loParentForm.llGenAut
    SELECT STYDYE
    =GFTABLEUPDATE()
    SELECT STYLE
    *! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][Start]
    *loParentForm.ChangeMode('S')
    *! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][End]
  ENDIF
  *! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][Start]
  loParentForm.ActiveMode = lcOldActiveMode  
  *! B610737,1 MMT 06/02/2014 Style profiles saved incorrectly in profvalu table[T20140509.0006][END]
  SELECT STYLE
ENDIF

ENDFUNC
*! E303165,1 04/06/2012 HIA, Don't rload query and enable filter in view mode  [T20120424.0007][End]
