*!**************************************************************************
*! Name      : FLOMAIN.PRG
*! Developer : Nader NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Flower By zoe Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C123158,1
*!**************************************************************************
*! Modifications
*! B124731,1 NNA 10/21/2004 Fix bugs : 
*! B124731,1 NNA            1 - when you add or modify Sablon info. you need to click on the 
*! B124731,1 NNA                modify Sablon Info. Option Twice.
*! B124731,1 NNA            2 - if you add a new style to an exist order the Sablon Info. doesn't Save
*! B124731,1 NNA            3 - if you try to input the same style-color it overwrites on the Sablon info.
*! B125330,1 NNA 11/19/2004 Fix bug that if made an order that have sablon codes for some styles
*! B125330,1 NNA            and no Codes (By Pressing Close Button on the Sablon Screen) for other
*! B125330,1 NNA            style ,you'll find that these codes are shifted with styles , and also
*! B125330,1 NNA            if you input a style with 2 dim. and input it codes you'll find that
*! B125330,1 NNA            the Second line only have these codes .
*! B608132,1 NNA 06/18/2007 (T20070507.0010)  Fix bug that if you input a new line with sablon screen and color
*! B608132,1 NNA            then you input another line with the same style/color/scale but with different sablon screen/color
*! B608132,1 NNA            the new one will overwrite the previous line with the new sablon screen/color
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'
*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!**************************************************************************
*! Name      : lfCRTTEMP
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Create temp file .
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfCRTTEMP

lcTrgCursr = lcOrdLine + 'A'
CREATE CURSOR (lcTrgCursr) (lcTmpFle C(8),llExtLines L(1),llNShowScr L(1))

*B124731,1 NNA 10/21/2004 (Begin) Store .F. as a Default Value for llNShowScr instead of .T.
*INSERT INTO (lcTrgCursr) (lcTmpFle,llExtLines,llNShowScr) VALUES (gfTempName(),.F.,.T.)
INSERT INTO (lcTrgCursr) (lcTmpFle,llExtLines,llNShowScr) VALUES (gfTempName(),.F.,.F.)
*B124731,1 NNA (End)

lcTmpCode  = &lcTrgCursr..lcTmpFle

=gfOpenFile(gcDataDir+'SOCODES',gcDataDir+'SOCODES','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(lcTmpCode,@laFileStru,[cordtype+order+STR(lineno,6)],lcTmpCode)

*-- END OF FUNCTION lfCRTTEMP

*!**************************************************************************
*! Name      : lfGetData
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
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

lcTrgCursr = lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle

SELECT SOCODES
=SEEK(lcOrdType+laData[1])
SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpCode) FROM MEMVAR
ENDSCAN

SELECT(lnOldAlS)

*-- END OF FUNCTION lfGetData

*!**************************************************************************
*! Name      : lfDispScr
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Function to Display the Flowers By Zoe Operation Manager Screen
*!**************************************************************************
*! Example   : lfDispScr()
*!**************************************************************************
*! C123158,1 NNA 07/18/2004
*!**************************************************************************
FUNCTION lfDispScr

PARAMETER llExtLin

DECLARE laCodeInfo[3,10] , laSblName[1,2] , laSblColor[1,2] , laTrmColor[1,2]
STORE '' TO laCodeInfo , laSblName , laSblColor , laTrmClr
STORE 1 TO lnSblName , lnSblColor , lnTrmColor 
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)

lcTrgCursr = lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle
IF &lcTrgCursr..llNShowScr
  REPLACE &lcTrgCursr..llNShowScr WITH .F.
  RETURN
ENDIF

IF llExtLin		&& if the screen Displayed from the Option Menu,that mean it's an exist Line
  REPLACE &lcTrgCursr..llExtLines WITH .T.
ENDIF

laCodeInfo[1,1] = 'CSBLNAME'
laCodeInfo[1,2] = 'laSblName'
laCodeInfo[1,3] = 'lnSblName'
laCodeInfo[1,4] = ''
laCodeInfo[1,5] = .F.
laCodeInfo[1,6] = .F.
laCodeInfo[1,10] = 'CSBLNAME'

laCodeInfo[2,1] = 'CSBLCOLOR'
laCodeInfo[2,2] = 'laSblColor'
laCodeInfo[2,3] = 'lnSblColor'
laCodeInfo[2,4] = ''
laCodeInfo[2,5] = .F.
laCodeInfo[2,6] = .F.
laCodeInfo[2,10] = 'CSBLCOLOR'

laCodeInfo[3,1] = 'CTRMCOLOR'
laCodeInfo[3,2] = 'laTrmColor'
laCodeInfo[3,3] = 'lnTrmColor'
laCodeInfo[3,4] = ''
laCodeInfo[3,5] = .F.
laCodeInfo[3,6] = .F.
laCodeInfo[3,10] = 'CTRMCOLOR'

*-- Get the Default Code
= gfwCodePop(@laCodeInfo , "CSBLNAME"   , 'D')
= gfwCodePop(@laCodeInfo , "CSBLCOLOR"  , 'D')
= gfwCodePop(@laCodeInfo , "CTRMCOLOR" , 'D')

*-- Get List of Codes Starting with the Default code
= gfwCodePop(@laCodeInfo , "CSBLNAME"   , 'L')
= gfwCodePop(@laCodeInfo , "CSBLCOLOR"  , 'L')
= gfwCodePop(@laCodeInfo , "CTRMCOLOR" , 'L')


SELECT (lcTmpCode)
IF SEEK('O'+laData[1]+STR(&lcOrdLine..LineNo,6))
  lnSblName  = ASCAN(laSblName , &lcTmpCode..CSblName )
  lnSblName  = IIF(lnSblName = 0 , 1 , ASUBSCRIPT(laSblName , lnSblName ,1))

  lnSblColor = ASCAN(laSblColor, &lcTmpCode..CSblColor)
  lnSblColor = IIF(lnSblColor = 0 , 1 , ASUBSCRIPT(laSblColor, lnSblColor , 1))

  lnTrmColor  = ASCAN(laTrmColor , &lcTmpCode..CTrmColor)
  lnTrmColor  = IIF(lnTrmColor = 0 , 1 , ASUBSCRIPT(laTrmColor , lnTrmColor , 1))
ENDIF

DO (gcScrDir+gcWinAppl+"\SOCODES.SPX")

*-- END OF FUNCTION lfDispScr

*!**************************************************************************
*! Name      : lfCodesOK
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Valid function of the push button pbOk in SoCodes Screen.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfCodesOK

PRIVATE lnRecNo,lnOldAlS
STORE 0 TO lnRecNo,lnOldAlS

lnOldAlS = SELECT(0)
lnRecNo  = RECNO(lcOrdLine)
lcTrgCursr = lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle
IF !(&lcTrgCursr..llExtLines)
  SELECT (lcOrdLine)

  *B124731,1 NNA 10/21/2004 (Begin) Scan for Style And Lineno To be able to input different Sablon
  *B124731,1 NNA            info. for the same style-color but in another line
  *SCAN FOR STYLE = LEFT(m.style,LEN(ALLTRIM(m.style))-3) 

  *B125330,1 NNA 11/19/2004 (Begin) Seek for LINENO <= M.LINENO because if you input a style
  *B125330,1 NNA            with 2 dim. M.lineno get with NO. (2) so the last line only has Codes.
  *SCAN FOR STYLE = LEFT(m.style,LEN(ALLTRIM(m.style))-3) AND LINENO = M.LINENO
  SCAN FOR STYLE = LEFT(m.style,LEN(ALLTRIM(m.style))-3) AND LINENO <= M.LINENO
  *B125330,1 NNA (End)
  
    SELECT (lcTmpCode)
    *B608132,1 NNA 06/18/2007 (Begin) in case we add new lines (&lcTrgCursr..llExtLines=.F.) then we need only to append no update
    *IF SEEK(&lcOrdLine..CORDTYPE + &lcOrdLine..ORDER + STR(&lcOrdLine..LINENO,6))
    *  REPLACE cSblName    WITH ALLTRIM(laSblName[lnSblName,2])  ,;
    *          cSblColor   WITH ALLTRIM(laSblColor[lnSblColor,2]),;
    *          CTRMCOLOR   WITH ALLTRIM(laTrmColor[lnTrmColor,2])
    *ELSE
    IF !SEEK(&lcOrdLine..CORDTYPE + &lcOrdLine..ORDER + STR(&lcOrdLine..LINENO,6))
    *B608132,1 NNA (End)

      APPEND BLANK
      REPLACE CORDTYPE    WITH &lcOrdLine..CORDTYPE                       ,;
              ORDER       WITH &lcOrdLine..ORDER                          ,;
              LINENO      WITH &lcOrdLine..LINENO                         ,;
              CSblName    WITH ALLTRIM(laSblName[lnSblName,2])            ,;
              CSblColor   WITH ALLTRIM(laSblColor[lnSblColor,2])          ,;
              CTRMCOLOR   WITH ALLTRIM(laTrmColor[lnTrmColor,2])          
    ENDIF
  ENDSCAN
ELSE
  IF SEEK(&lcOrdLine..CORDTYPE + &lcOrdLine..ORDER + STR(&lcOrdLine..LINENO,6),lcTmpCode)
     REPLACE CSblName    WITH ALLTRIM(laSblName[lnSblName,2])             ,;
             CSblColor   WITH ALLTRIM(laSblColor[lnSblColor,2])           ,;
             CTRMCOLOR   WITH ALLTRIM(laTrmColor[lnTrmColor,2])           
  ELSE
    APPEND BLANK
    REPLACE CORDTYPE    WITH &lcOrdLine..CORDTYPE                         ,;
            ORDER       WITH &lcOrdLine..ORDER                            ,;
            LINENO      WITH &lcOrdLine..LINENO                           ,;
            CSblName    WITH ALLTRIM(laSblName[lnSblName,2])              ,;
            CSblColor   WITH ALLTRIM(laSblColor[lnSblColor,2])            ,;
            CTRMCOLOR   WITH ALLTRIM(laTrmColor[lnTrmColor,2])            

  ENDIF
ENDIF
SELECT(lnOldAlS)
IF lnRecNo <> 0
  GOTO lnRecNo IN (lcOrdLine)
ENDIF
REPLACE &lcTrgCursr..llExtLines WITH .F.
REPLACE &lcTrgCursr..llNShowScr  WITH .F.
*-- END OF FUNCTION lfCodesOK


*!**************************************************************************
*! Name      : lfCodesSav
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
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
lcTrgCursr = lcOrdLine + 'A'
lcTmpCode  = &lcTrgCursr..lcTmpFle

SELECT (lcTmpCode)
REPLACE ALL ORDER WITH ORDHDR.ORDER
SCAN
  SCATTER MEMVAR

  *B125330,1 NNA 11/19/2004 (Begin) Stop the Second Two Lines because we'll get the LineNo 
  *B125330,1 NNA            from temp file lcTmpCode not as counter.  
  *lnLineNo = lnLineNo + 1
  *m.LineNo = lnLineNo
  *B125330,1 NNA (End)
  
  SELECT SOCODES
  IF SEEK(m.CordType + m.Order + STR(m.LineNo,6))
    GATHER MEMVAR  
  ELSE
    INSERT INTO SOCODES FROM MEMVAR
  ENDIF
ENDSCAN

SELECT(lnOldAlS)
*-- END OF FUNCTION lfCodesSav

*!**************************************************************************
*! Name      : lfAddMPad
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
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
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  lnBarNo = CNTBAR('_INQURYPOP') + 1

  *-- Skip The New BAR if this is not the detail Folder or this not ADD Or EDIT Mode 
  *-- Or there's No lines
  DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Modify Sablon Information' SKIP FOR ;
             (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline)) OR laScrMode[2]
  ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfDispScr IN FLOMAIN WITH .T.
ENDIF
RETURN

*-- END OF FUNCTION lfAddMPad.

*!**************************************************************************
*! Name      : lfChngPar
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
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
lcTrgCursr = lcOrdLine + 'A'
REPLACE &lcTrgCursr..llExtLines WITH .T.
SELECT(lnOldAlS)

*-- END OF FUNCTION lfChngPar.
*!**************************************************************************
*! Name      : lfNShowScr
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
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
lcTrgCursr = lcOrdLine + 'A'
REPLACE &lcTrgCursr..llNShowScr WITH .T.
SELECT(lnOldAlS)
*-- END OF FUNCTION lfNShowScr.