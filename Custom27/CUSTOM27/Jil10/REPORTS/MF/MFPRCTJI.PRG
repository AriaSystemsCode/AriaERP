*:***************************************************************************
*: Program file  : MFPRCTJI.PRG
*: Program desc. : Print Cutting Tickets for JILL STUART
*: For Report    : MFPRCTJI.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : Ahmed Maher (AMH)
*: Reference     : C102169,1
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*:Modifications:
* C102169,4 AMH 04/29/2001 Fix some bugs in this report.
*:***************************************************************************
*:
*-- Temporary file of the cutting ticket report.
lcTmpFile = gfTempName()
lcVendName = ''

lcRltColor = SPACE(0)
DECLARE laColor[1,2]
laColor[1,1] = 'CLRLNAME'
laColor[1,2] = 'lcRltColor'

*C102169,4 AMH Zap the file that is used to print the bill of materials [Start]
*              because we added a new option that will be used in the filter.
IF llrPrtCs
  SELECT (lcCostF)
  DELETE ALL
ENDIF
*C102169,4 AMH [End]

SELECT CUTTKTL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+6,4]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cNoteFlag'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cNotes'
laFileStru[lnFileStru,2] = 'M'
laFileStru[lnFileStru,3] = 0
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cVendor'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOpration1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOpration2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'ColorLName'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 50
laFileStru[lnFileStru,4] = 0

Create Table (gcWorkDir+lcTmpFile) FROM ARRAY laFileStru
INDEX ON CUTTKT+cWareCode+Style+Dyelot+cNoteFlag TAG (lcTmpFile)

*-- To get the color segement position.
=lfGetColor()

SELECT CUTTKTH
SCAN FOR &lcRpExp
  SELECT CUTTKTL
  IF  !lfVldVen() .AND. !EMPTY (lcRpVendor)
    LOOP
  ENDIF
  
  IF SEEK(CUTTKTH.CutTkt)
    lcOpr1 = lfGetOpr('1')
    lcOpr2 = lfGetOpr('2')
    SCAN REST WHILE CutTkt = CutTktH.CutTkt FOR TranCD='1'
      SCATTER MEMVAR
      m.cNoteFlag   = 'N'
      m.cNotes      = ''
      m.cVendor     = lcVendName
      m.cOpration1  = lcOpr1
      m.cOpration2  = lcOpr2
      = gfRltFld(SUBSTR(m.Style,lnColorStr,lnColorLen) , @laColor , 'COLOR     ')
      m.ColorLName = lcRltColor
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    *-- Print notes for C/T.
    IF llRPrtCtn .AND. SEEK('I'+m.CutTkt,'NotePad')
      m.cNoteFlag   = 'R'
      m.cNotes      = Notepad.MNotes
      m.cVendor     = lcVendName
      m.cOpration1  = lcOpr1
      m.cOpration2  = lcOpr2
      = gfRltFld(SUBSTR(m.Style,lnColorStr,lnColorLen) , @laColor , 'COLOR     ')
      m.ColorLName = lcRltColor
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDIF
    *-- Print notes for Style.
    IF llRPrtSn .AND. SEEK('F'+CutTktH.Style,'NotePad')
      m.cNoteFlag   = 'S'
      m.cNotes      = Notepad.MNotes
      m.cVendor     = lcVendName
      m.cOpration1  = lcOpr1
      m.cOpration2  = lcOpr2
      = gfRltFld(SUBSTR(m.Style,lnColorStr,lnColorLen) , @laColor , 'COLOR     ')
      m.ColorLName = lcRltColor
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDIF
  ENDIF

  *C102169,4 AMH If we will print cost sheet , insert cost sheet records [Start]
  *              in the temprorary file.
  IF llrPrtCs .AND. SEEK('M'+CUTTKTH.CutTkt,'CTKTBOM')
    SELECT CTKTBOM
    SCAN WHILE cImTyp+CutTkt = 'M'+CutTktH.CutTkt FOR cCatgTyp $ 'FTS'
      SCATTER MEMVAR
      INSERT INTO (lcCostF) FROM MEMVAR 
    ENDSCAN
  ENDIF
 *C102169,4 AMH [End]
 
ENDSCAN  && End loop of CUTTKTH file.
SELECT (lcTmpFile)
LOCATE

*C102169,4 AMH Comment next lines [Start]
*IF EOF() AND llrPrtCs
*  SELECT (lcCostF)
*  ZAP
*  SELECT (lcTmpFile)
*ENDIF
*C102169,4 AMH [End]

SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO CutTkt INTO CUTTKTH ADDITIVE

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 04/19/2001
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfVldVen
*! Developer : Ahmed Maher
*! Date      : 04/17/2001
*! Purpose   : Check Vendors of C/T.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T./.F.
*!*************************************************************
*! Example   : lfVldVen()
*!*************************************************************
*
FUNCTION lfVldVen

PRIVATE llRet, lcOrder, lnAlias
llRet = .F.
lnAlias = SELECT(0)
SELECT MFGOPRHD
lcOrder = SET('ORDER')
SET ORDER TO TAG TKTOPER
IF EMPTY(lcRpVendor)
  llRet = .T.
  *khalid
  *IF SEEK ('M'+CUTTKTH.CUTTKT+'1') .AND. !LINHOUSE
  IF SEEK ('M'+CUTTKTH.CUTTKT) .AND. !LINHOUSE
    lcVendName = CCONTNAME
  ELSE
    lcVendName = ''
  ENDIF
ELSE
  IF SEEK('M'+CUTTKTH.CUTTKT)
    LOCATE REST WHILE cImTyp+cTktNo+cOperSeq = 'M'+CUTTKTH.CUTTKT;
                FOR !LINHOUSE .AND. (CCONTCODE = lcRpVendor)
    IF FOUND()
      lcVendName = CCONTNAME
      llRet = .T.
    ENDIF
  ENDIF
ENDIF
SET ORDER TO &lcOrder.
SELECT (lnAlias)
RETURN llRet
*-- end of lfVldVen.

*!*************************************************************
*! Name      : lfGetOpr
*! Developer : Ahmed Maher
*! Date      : 04/19/2001
*! Purpose   : Get Opration No. X of C/T.
*!*************************************************************
*! Parameters: lcSequance
*!*************************************************************
*! Returns   : Opration Description
*!*************************************************************
*! Example   : lfGetOpr()
*!*************************************************************
*
FUNCTION lfGetOpr
PARAMETERS lcSequance

PRIVATE lcRet, lcOrder, lnAlias
lcRet = ''
lnAlias = SELECT(0)
SELECT MFGOPRHD
lcOrder = SET('ORDER')
SET ORDER TO TAG TKTOPER
SEEK 'M'+CUTTKTH.CUTTKT+lcSequance
IF FOUND()
  lcRet = gfCodDes(COPRCODE, 'MFGCODE')
ENDIF
SET ORDER TO &lcOrder.
SELECT (lnAlias)
RETURN lcRet
*-- end of lfGetOpr.

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/22/2001
*! Purpose   : Validation function for the Vendor field
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvVendor
PARAMETERS lcDammy
PRIVATE lcObjName , lcObjVal , llObjRet
lcDammy = .T.

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Vendor is not in the file
IF LASTKEY() = 13 AND !MDOWN() .OR. !EMPTY(lcObjVal) 
 IF !SEEK(lcObjVal ,'APVENDOR')
   SELECT APVENDOR
   llObjRet = gfApVnBrow(@lcObjVal)
   lcObjVal = IIF(llObjRet , lcObjVal , '')
   &lcObjName = lcObjVal
 ENDIF  
ENDIF    && End of IF
RETURN .T.