*:***************************************************************************
*: Program file  : MFSIMTEX (C102717,1)
*: Program desc. : Create TXT file for Simtex Style Information
*: For Report    : None
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFSIMSTY
*:***************************************************************************

*-- Get the selected C/T if any
lcCutFile = ''
llCutRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutFile = laOGFxFlt[lnPosition,6]
  llCutRang = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF

*-- Get the selected Style if any
lcStyFile = ''
llStyRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  llStyRang = IIF(!EMPTY(lcStyFile) .AND. USED(lcStyFile) .AND. RECCOUNT(lcStyFile)>0,.T.,.F.)
ENDIF

*-- Get the selected Color if any
lcClrExpr = ''
llClrRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'BOMLINE.SCLR'),1)
IF lnPosition > 0
  lcClrExpr = laOGFxFlt[lnPosition,6]
  llClrRang = IIF(!EMPTY(lcClrExpr),.T.,.F.)
ENDIF

*-- Get the selected Style group if any
lcGrpExpr = ''
llGrpRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGrpExpr = laOGFxFlt[lnPosition,6]
  llGrpRang = IIF(!EMPTY(lcGrpExpr),.T.,.F.)
ENDIF

*-- Get the selected Season if any
lcSeaExpr = ''
llSeaRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeaExpr = laOGFxFlt[lnPosition,6]
  llSeaRang = IIF(!EMPTY(lcSeaExpr),.T.,.F.)
ENDIF

*--Use extended size scale.  
llMScale = gfGetMemVar('M_USEEXSSC')

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

*--Create cursor to export data from it.
lcTmpCrsr = gfTempName()
CREATE CURSOR (lcTmpCrsr) (cStyle C(19),cColor C(6),cCuttkt C(6),cDim C(5),cSize C(5),;
                           cOperSeq C(2),cOperCode C(6))
INDEX ON CCUTTKT+CSTYLE+CCOLOR+COPERSEQ TAG (lcTmpCrsr) OF (lcTmpCrsr)

lnFileHand = 0
IF llCutRang
  SELECT (lcCutFile)
  SCAN
    lcCuttkt = CUTTKT
    =SEEK(lcCuttkt,'CUTTKTH')
    IF llStyRang .AND. !SEEK(CUTTKTH.STYLE,lcStyFile)
      LOOP
    ENDIF
    IF SEEK('M1'+lcCuttkt,'BOMLINE')
      IF !lfScan(lcCuttkt)
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
  WAIT CLEAR
ELSE
  IF llStyRang
    SELECT CUTTKTH
    lcOrder = SET('ORDER')
    SET ORDER TO TAG CUTTKTHS
    SELECT (lcStyFile)
    llCont = .T.
    SCAN
      lcStyle = CSTYMAJOR
      IF SEEK(lcStyle,'CUTTKTH')
        SELECT CUTTKTH
        SCAN REST WHILE STYLE+CUTTKT = lcStyle
          IF SEEK('M1'+CUTTKT,'BOMLINE')
            IF !lfScan(CUTTKT)
              llCont = .F.
              EXIT
            ENDIF
          ENDIF
        ENDSCAN
        IF !llCont
          EXIT
        ENDIF
      ENDIF
    ENDSCAN
    SELECT CUTTKTH
    SET ORDER TO &lcOrder.
    WAIT CLEAR
  ELSE
    IF SEEK('M1','BOMLINE')
      =lfScan('')
    ENDIF
  ENDIF
ENDIF

IF RECCOUNT(lcTmpCrsr) > 0
  SELECT (lcTmpCrsr)
  SCAN
    lcTxtLine = ALLTRIM(CSTYLE) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(CCOLOR) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(CCUTTKT) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(CDIM) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(CSIZE) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(COPERSEQ) + '|'
    lcTxtLine = lcTxtLine + ALLTRIM(COPERCODE)
    =FPUTS(lnFileHand,lcTxtLine)
  ENDSCAN
  USE IN (lcTmpCrsr)
ENDIF

IF lnFileHand > 0
  =FCLOSE(lnFileHand)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Exporting data completed successfully.')
ELSE
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'No cutting tickets to export.')
ENDIF

*!*************************************************************
*! Name      : lfvFlName
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/07/2002
*! Purpose   : Validate the file name.
*!*************************************************************
*! Called from : O.G.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFlName()
*!*************************************************************

FUNCTION lfvFlName

PRIVATE lcNewFile,lcDefDir,lcNewDir,lnOccur,lnDirLen
lnOccur = OCCURS('\',lcFileName)
lcDefDir = FULLPATH('')
IF lnOccur > 0
  lnDirLen = AT('\',lcFileName,lnOccur) - IIF(lnOccur=1,0,1)
  lcNewDir = SUBSTR(lcFileName,1,lnDirLen)
  SET DEFAULT TO (lcNewDir)
ENDIF
lcNewFile = PUTFILE('File Name',lcFileName,'TXT')
IF !EMPTY(lcNewFile)
  lcFileName = lcNewFile
ENDIF
IF lnOccur > 0
  SET DEFA TO (lcDefDir)
ENDIF

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/07/2002
*! Purpose   : the when function of OG
*!*************************************************************
*! Called from : O.G.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************

FUNCTION lfwOgWhen

SHOW GET pbOGpreview DISABLE

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/07/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AMH-AHMED MAHER
*! Date      : 10/07/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : AMH-AHMED MAHER
*! Date      : 10/07/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfScan
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/07/2002
*! Purpose   : Scan the BomLine file
*!*************************************************************
*! Passed Parameters : lcTktNo
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfScan()
*!*************************************************************
FUNCTION lfScan
PARAMETERS lcTktNo

llRet = .T.
SELECT BOMLINE
SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                'M1'+lcTktNo FOR CCATGTYP = 'M' AND !EMPTY(MFGCODE)
  IF llClrRang .AND. !(SUBSTR(STYLE,lnClrSrt,lnClrEnd) $ lcClrExpr)
    LOOP
  ENDIF
  =SEEK(STYLE,'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  IF llGrpRang .AND. !(STYLE.CSTYGROUP $ lcGrpExpr)
    LOOP
  ENDIF
  IF llSeaRang .AND. !(STYLE.SEASON $ lcSeaExpr)
    LOOP
  ENDIF
  WAIT WINDOW 'Export C/T # : '+CTKTNO+' Style : '+ALLTRIM(STYLE)+' ...' NOWAIT
  IF lnFileHand = 0
    lnFileHand = FCREATE(lcFileName)
    IF lnFileHand = -1
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot create the txt file.')
      llRet = .F.
      EXIT
    ENDIF
  ENDIF
  =SEEK('M'+CTKTNO+MFGCODE,'MFGOPRHD')
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    INSERT INTO (lcTmpCrsr) (cStyle,cColor,cCuttkt,cDim,cSize,cOperSeq,cOperCode);
                     VALUES (STYLE.CSTYMAJOR,SUBSTR(BOMLINE.STYLE,lnClrSrt,lnClrEnd),BOMLINE.CTKTNO,;
                             IIF(EMPTY(SCALE.CDIM1),EVALUATE('SCALE.SZ'+lcI),SCALE.CDIM1),;
                             IIF(EMPTY(SCALE.CDIM1),'',EVALUATE('SCALE.SZ'+lcI)),;
                             MFGOPRHD.COPERSEQ,BOMLINE.MFGCODE)
  ENDFOR
ENDSCAN
RETURN llRet