*!************************************************************************
*: Program file  : MFHNGCR.PRG (REFERE TO E500452)
*: Program desc. : HANG TAG & CARE LABEL.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:      Developer: AHMED MAHER (AMH)
*:           Date: 06/05/2003
*!************************************************************************
*: Passed Parameters  :
*!************************************************************************
*: Modifications      : 
*:************************************************************************
PARAMETERS llFromWz

PRIVATE lnColorStr, lnColorLen, laLblFld
STORE 0 TO lnColorStr, lnColorLen

*-- Get Color Segment.
=lfGetColor()

lnStyleWid = LEN(gfItemMask("PM"))
lcTmpData  = gfTempName()

DO CASE
  CASE gcAct_Appl = "MF"
    DECLARE laLblFld[11,4],laLblVal[11]
    laLblFld[01,1] = 'CUTTKT'
    laLblFld[02,1] = 'STYLE'
    laLblFld[03,1] = 'SIZE'
    laLblFld[04,1] = 'CLRDESC'
    laLblFld[05,1] = 'DYELOT'
    laLblFld[06,1] = 'STYDESC'
    laLblFld[07,1] = 'PRICEA'
    laLblFld[08,1] = 'FABDESC'
    laLblFld[09,1] = 'FABCONT'
    laLblFld[10,1] = 'COLOR'
    laLblFld[11,1] = 'SIZEPOS'
    
    laLblFld[01,2] = 'C'
    laLblFld[02,2] = 'C'
    laLblFld[03,2] = 'C'
    laLblFld[04,2] = 'C'
    laLblFld[05,2] = 'C'
    laLblFld[06,2] = 'C'
    laLblFld[07,2] = 'N'
    laLblFld[08,2] = 'C'
    laLblFld[09,2] = 'C'
    laLblFld[10,2] = 'C'
    laLblFld[11,2] = 'C'
    
    laLblFld[01,3] = 6
    laLblFld[02,3] = lnStyleWid
    laLblFld[03,3] = 5
    laLblFld[04,3] = 30
    laLblFld[05,3] = 10
    laLblFld[06,3] = 20
    laLblFld[07,3] = 12
    laLblFld[08,3] = 20
    laLblFld[09,3] = 30
    laLblFld[10,3] = 6
    laLblFld[11,3] = 1
    
    laLblFld[01,4] = 0
    laLblFld[02,4] = 0
    laLblFld[03,4] = 0
    laLblFld[04,4] = 0
    laLblFld[05,4] = 0
    laLblFld[06,4] = 0
    laLblFld[07,4] = 2
    laLblFld[08,4] = 0
    laLblFld[09,4] = 0
    laLblFld[10,4] = 0
    laLblFld[11,4] = 0
    
    laLblVal[01] = 'CUTTKTH.CUTTKT'
    laLblVal[02] = 'STYLE.CSTYMAJOR'
    laLblVal[03] = 'EVALUATE("SCALE.SZ"+lcI)'
    laLblVal[04] = 'lcClrDesc'
    laLblVal[05] = 'CUTTKTL.DYELOT'
    laLblVal[06] = 'STYLE.DESC'
    laLblVal[07] = 'STYLE.PRICEA'
    laLblVal[08] = 'FABRIC.DESC'
    laLblVal[09] = 'FABRIC.CONTENT'
    laLblVal[10] = 'lcColor'
    laLblVal[11] = 'lcI'
    
    lcHdrFile  = 'CUTTKTH'
    lcLineFile = 'CUTTKTL'
    lcSeekExpr = lcHdrFile + '.' + laLblFld[01,1]
    lcKeyExp   = 'CUTTKT+STYLE+DYELOT+TRANCD'
    lcIndex    = laLblFld[01,1]+IIF(lcRpSort='S','+SIZEPOS','+COLOR')
  CASE gcAct_Appl = "PO"
    DECLARE laLblFld[10,4],laLblVal[10]
    laLblFld[01,1] = 'PO'
    laLblFld[02,1] = 'STYLE'
    laLblFld[03,1] = 'SIZE'
    laLblFld[04,1] = 'CLRDESC'
    laLblFld[05,1] = 'STYDESC'
    laLblFld[06,1] = 'PRICEA'
    laLblFld[07,1] = 'FABDESC'
    laLblFld[08,1] = 'FABCONT'
    laLblFld[09,1] = 'COLOR'
    laLblFld[10,1] = 'SIZEPOS'
    
    laLblFld[01,2] = 'C'
    laLblFld[02,2] = 'C'
    laLblFld[03,2] = 'C'
    laLblFld[04,2] = 'C'
    laLblFld[05,2] = 'C'
    laLblFld[06,2] = 'N'
    laLblFld[07,2] = 'C'
    laLblFld[08,2] = 'C'
    laLblFld[09,2] = 'C'
    laLblFld[10,2] = 'C'
    
    laLblFld[01,3] = 6
    laLblFld[02,3] = lnStyleWid
    laLblFld[03,3] = 5
    laLblFld[04,3] = 30
    laLblFld[05,3] = 20
    laLblFld[06,3] = 12
    laLblFld[07,3] = 20
    laLblFld[08,3] = 30
    laLblFld[09,3] = 6
    laLblFld[10,3] = 1
    
    laLblFld[01,4] = 0
    laLblFld[02,4] = 0
    laLblFld[03,4] = 0
    laLblFld[04,4] = 0
    laLblFld[05,4] = 0
    laLblFld[06,4] = 2
    laLblFld[07,4] = 0
    laLblFld[08,4] = 0
    laLblFld[09,4] = 0
    laLblFld[10,4] = 0
    
    laLblVal[01] = 'POSHDR.PO'
    laLblVal[02] = 'STYLE.CSTYMAJOR'
    laLblVal[03] = 'EVALUATE("SCALE.SZ"+lcI)'
    laLblVal[04] = 'lcClrDesc'
    laLblVal[05] = 'STYLE.DESC'
    laLblVal[06] = 'STYLE.PRICEA'
    laLblVal[07] = 'FABRIC.DESC'
    laLblVal[08] = 'FABRIC.CONTENT'
    laLblVal[09] = 'lcColor'
    laLblVal[10] = 'lcI'
    
    lcHdrFile  = 'POSHDR'
    lcLineFile = 'POSLN'
    lcSeekExpr = "'P'+"+ lcHdrFile + '.' + laLblFld[01,1]
    lcKeyExp   = 'CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'
    lcIndex    = laLblFld[01,1]+'+STYLE+'+IIF(lcRpSort='S','SIZEPOS','COLOR')
  CASE gcAct_Appl = "SO"
    DECLARE laLblFld[8,4],laLblVal[8]
    laLblFld[01,1] = 'ORDER'
    laLblFld[02,1] = 'STYLE'
    laLblFld[03,1] = 'SIZE'
    laLblFld[04,1] = 'CLRDESC'
    laLblFld[05,1] = 'STYDESC'
    laLblFld[06,1] = 'PRICEA'
    laLblFld[07,1] = 'COLOR'
    laLblFld[08,1] = 'SIZEPOS'
    
    laLblFld[01,2] = 'C'
    laLblFld[02,2] = 'C'
    laLblFld[03,2] = 'C'
    laLblFld[04,2] = 'C'
    laLblFld[05,2] = 'C'
    laLblFld[06,2] = 'N'
    laLblFld[07,2] = 'C'
    laLblFld[08,2] = 'C'
    
    laLblFld[01,3] = 6
    laLblFld[02,3] = lnStyleWid
    laLblFld[03,3] = 5
    laLblFld[04,3] = 30
    laLblFld[05,3] = 20
    laLblFld[06,3] = 12
    laLblFld[07,3] = 6
    laLblFld[08,3] = 1
    
    laLblFld[01,4] = 0
    laLblFld[02,4] = 0
    laLblFld[03,4] = 0
    laLblFld[04,4] = 0
    laLblFld[05,4] = 0
    laLblFld[06,4] = 2
    laLblFld[07,4] = 0
    laLblFld[08,4] = 0
    
    laLblVal[01] = 'ORDHDR.ORDER'
    laLblVal[02] = 'STYLE.CSTYMAJOR'
    laLblVal[03] = 'EVALUATE("SCALE.SZ"+lcI)'
    laLblVal[04] = 'lcClrDesc'
    laLblVal[05] = 'STYLE.DESC'
    laLblVal[06] = 'STYLE.PRICEA'
    laLblVal[07] = 'lcColor'
    laLblVal[08] = 'lcI'
    
    lcHdrFile  = 'ORDHDR'
    lcLineFile = 'ORDLINE'
    lcSeekExpr = "'O'+" + lcHdrFile + '.' + laLblFld[01,1]
    lcKeyExp   = 'CORDTYPE+ORDER+STR(LINENO,6)'
    lcIndex    = IIF(lcRpSort='O','STYLE+'+laLblFld[01,1]+'+',laLblFld[01,1]+'+STYLE+')+;
                 IIF(lcRpSort='S','SIZEPOS','COLOR')
ENDCASE
=lfScanData(gcAct_Appl)

SELECT (lcTmpData)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

IF !llFromWz
  =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcFormName)
  SHOW GET lcRepMode DISABLE

  SELECT (lcTmpData)
  LOCATE
  lcSaveForm = lcFormName
  DO gfDispRe WITH lcFormName,'.T.',.F.,'L'
  lcFormName = lcSaveForm
ELSE
  LABEL FORM (lcFormName) PREVIEW
ENDIF

USE IN (lcTmpData)
*-- end of main program.

*!*************************************************************
*! Name      : lfwOpGrid
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : the when funtion of the OG.
*!*************************************************************
FUNCTION lfwOpGrid

IF FILE(gcWorkDir+'LABEL.MEM')
  RESTORE FROM (gcWorkDir+'LABEL.MEM') ADDITIVE
  lcFormName = lcRpLabel
  laOGObjCnt[lfVarPos('lcRpLabel')] = .F.
  =lfOGShowGet('lcRpLabel')
  SHOW GET pbOGpreview DISABLE
ENDIF

SHOW GET lcRepMode DISABLE
IF EMPTY(lcFormName)
  =gfModalGen('TRM38263B00000','ALERT')
  llOgTrmnat = .T.
  CLEAR READ
ENDIF
*-- end of lfwOpGrid.

*!*************************************************************
*! Name      : lfvOpGrid
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : the valid funtion of the OG.
*!*************************************************************
FUNCTION lfvOpGrid

IF FILE(gcWorkDir+'LABEL.MEM')
  ERASE (gcWorkDir+'LABEL.MEM')
ENDIF
*-- end of lfvOpGrid.

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : Valid function of the vendor field.
*!*************************************************************
FUNCTION lfvVendor

PRIVATE lcObj,lcVendor
lcObj = SYS(18)
lcVendor = EVALUATE(lcObj)
IF !EMPTY(lcVendor) .AND. !SEEK(lcVendor,'ApVendor')
  =gfApVnBrow(@lcVendor,.F.,IIF(gcAct_Appl="MF",'C','S'))
  IF EMPTY(lcVendor)
    &lcObj. = lcRpOldVal
  ELSE
    &lcObj. = lcVendor
  ENDIF
ENDIF
*-- end of lfvVendor.

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : Valid function of the Customer field.
*!*************************************************************
FUNCTION lfvCustomer

PRIVATE lcObj,lcAccount
lcObj = SYS(18)
lcAccount = EVALUATE(lcObj)
IF !EMPTY(lcAccount) .AND. !SEEK('M'+lcAccount,'CUSTOMER')
  DO CUSBROWM WITH lcAccount
  IF EMPTY(lcAccount)
    &lcObj. = lcRpOldVal
  ELSE
    &lcObj. = lcAccount
  ENDIF
ENDIF
*-- end of lfvCustomer.

*!*************************************************************
*! Name      : lfvSelFlt
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : Valid function of the Select popup.
*!*************************************************************
FUNCTION lfvSelFlt

CLEAR READ
*-- end of lfvSelFlt.

*!*************************************************************
*! Name      : lfvLabel
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : Valid function of the Label Name field.
*!*************************************************************
FUNCTION lfvLabel

lcFormName = lcRpLabel
*-- end of lfvLabel.

*!*************************************************************
*! Name      : lfGetSlct
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : To fill the Select Arrays.
*!*************************************************************
FUNCTION lfGetSlct

DECLARE laRpSelDsc[4,1],laRpSelId[4,1]

laRpSelDsc[1,1] = 'Entered'
laRpSelDsc[2,1] = 'Complete'

laRpSelId[1,1] = 'E'
laRpSelId[2,1] = 'C'

DO CASE
  CASE gcAct_Appl = "MF"
    laRpSelDsc[3,1] = 'Cutting Ticket #'
    laRpSelDsc[4,1] = 'Vendor'
    
    laRpSelId[3,1] = 'T'
    laRpSelId[4,1] = 'V'
  CASE gcAct_Appl = "PO"
    laRpSelDsc[3,1] = 'Style PO #'
    laRpSelDsc[4,1] = 'Vendor'
    
    laRpSelId[3,1] = 'P'
    laRpSelId[4,1] = 'V'
  CASE gcAct_Appl = "SO"
    laRpSelDsc[3,1] = 'Order #'
    laRpSelDsc[4,1] = 'Customer'
    
    laRpSelId[3,1] = 'S'
    laRpSelId[4,1] = 'U'
ENDCASE
*-- end of lfGetSlct.

*!*************************************************************
*! Name      : lfGetStats
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : To fill the Status Arrays.
*!*************************************************************
FUNCTION lfGetStats

DECLARE laRpStsDsc[IIF(gcAct_Appl="SO",3,4),1],laRpStsId[IIF(gcAct_Appl="SO",3,4),1]

laRpStsDsc[1,1] = 'All'
laRpStsDsc[2,1] = 'Open'
laRpStsDsc[3,1] = 'Hold'

laRpStsId[1,1] = 'L'
laRpStsId[2,1] = 'O'
laRpStsId[3,1] = 'H'

IF !(gcAct_Appl = "SO")
  laRpStsDsc[4,1] = 'Actual'
  
  laRpStsId[4,1]  = 'A'
ENDIF
*-- end of lfGetStats.

*!*************************************************************
*! Name      : lfGetSort
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : To fill the Sort Arrays.
*!*************************************************************
FUNCTION lfGetSort

DECLARE laRpSrtDsc[IIF(gcAct_Appl="SO",3,2),1],laRpSrtId[IIF(gcAct_Appl="SO",3,2),1]

lcStyleHdr = ALLTRIM(gfItemMask("HI"))
IF (gcAct_Appl = "SO")
  laRpSrtDsc[1,1] = 'Order/'+lcStyleHdr+'/Size'
  laRpSrtDsc[2,1] = 'Order/'+lcStyleHdr
  laRpSrtDsc[3,1] = lcStyleHdr+'/Size/Order'
  
  laRpSrtId[1,1] = 'S'
  laRpSrtId[2,1] = 'C'
  laRpSrtId[3,1] = 'O'
ELSE
  laRpSrtDsc[1,1] = lcStyleHdr+'/Size'
  laRpSrtDsc[2,1] = lcStyleHdr
  
  laRpSrtId[1,1] = 'S'
  laRpSrtId[2,1] = 'C'
ENDIF
*-- end of lfGetSort.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/08/2003
*! Purpose   : When function to save the old value
*!*************************************************************
FUNCTION lfwOldVal

lcRpOldVal = EVALUATE(SYS(18))
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSROrder
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/08/2003
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
*-- Set Relation with customer file.
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    SET ORDER TO Customer IN Customer
    SET RELATION TO 'M' + Account INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- end of lfSROrder.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/09/2003
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
*--end of lfGetColor.

*!*************************************************************
*! Name      : lfInsrtRec
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/08/2003
*! Purpose   : To insert data record in case of MF or PO
*!*************************************************************
FUNCTION lfInsrtRec

SELECT (lcLineFile)
SCAN REST WHILE &lcKeyExp. = EVALUATE(lcSeekExpr) FOR IIF(lcModule='SO',.T.,TRANCD='1')
  lnScaleCnt = SCALE.CNT
  IF lcRpSize = 'A'
    lnStrLoop = 1
    lnEndLoop = lnScaleCnt
  ELSE
    IF VAL(lcRpSize) > lnScaleCnt
      LOOP
    ENDIF
    STORE VAL(lcRpSize) TO lnStrLoop,lnEndLoop
  ENDIF
  FOR lnI = lnStrLoop TO lnEndLoop
    lcI = STR(lnI,1)
    lnQty = CEILING(EVALUATE('QTY'+lcI) * lnRpPercnt / 100)
    IF lnQty > 0
      FOR lnJ = 1 TO lnQty
        lcColor   = SUBSTR(STYLE,lnColorStr,lnColorLen)
        lcClrDesc = gfCodDes(lcColor,PADR('COLOR',10))
        lcClrDesc = IIF(EMPTY(lcClrDesc),lcColor,lcClrDesc)
        SELECT (lcTmpData)
        APPEND BLANK
        FOR lnK = 1 TO ALEN(laLblFld,1)
          REPLACE (laLblFld[lnK,1]) WITH EVALUATE(laLblVal[lnK])
        ENDFOR
        SELECT (lcLineFile)
        IF llFromWz
          RETURN
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
ENDSCAN
*--end of lfInsrtRec.

*!*************************************************************
*! Name      : lfScanData
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/08/2003
*! Purpose   : To Scan the data header file.
*!*************************************************************
FUNCTION lfScanData
PARAMETERS lcModule

CREATE CURSOR (lcTmpData) FROM ARRAY laLblFld
INDEX ON &lcIndex. TAG (lcTmpData) OF (lcTmpData)

SELECT (lcLineFile)
SET RELATION TO STYLE INTO STYLE
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
IF lcModule # 'SO'
  SET RELATION TO FABRIC INTO FABRIC ADDITIVE
ENDIF

lcStatus = IIF(lcRpStatus='L','.T.','STATUS = lcRpStatus')
llVendSel = .F.
IF lcRpSelect = "V" .AND. lcRpExp # ".T."
  lcVendCode = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
  IF lnPosition > 0
    lcVendCode = laOGFxFlt[lnPosition,6]
  ENDIF
  IF lcModule = 'MF'
    lcRpExp = ".T."
    llVendSel = .T.
  ELSE
    lcRpExp = "POSHDR.VENDOR = lcVendCode"
  ENDIF
ENDIF

IF lcRpSelect = "U" .AND. lcRpExp # ".T."
  lcAccount = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
  IF lnPosition > 0
    lcAccount = laOGFxFlt[lnPosition,6]
  ENDIF
  lcRpExp = "ORDHDR.ACCOUNT = lcAccount"
ENDIF

SELECT (lcHdrFile)
SCAN FOR &lcRpExp. .AND. &lcStatus.
  WAIT WINDOW laLblFld[01,1] + ' : ' + EVALUATE(laLblVal[01]) NOWAIT
  llFound = .F.
  IF llVendSel
    IF SEEK('M'+CUTTKT,'MFGOPRHD')
      SELECT MFGOPRHD
      LOCATE REST WHILE cImTyp+cTktNo+cOprCode = 'M'+CUTTKTH.CUTTKT;
             FOR !lInHouse AND cContCode = lcVendCode
      IF FOUND()
        llFound = .T.
      ENDIF
    ENDIF
  ELSE
    llFound = .T.
  ENDIF
  
  SELECT (lcHdrFile)
  IF llFound .AND. SEEK(EVALUATE(lcSeekExpr),lcLineFile)
    =lfInsrtRec()
    IF llFromWz .AND. RECCOUNT(lcTmpData) > 0
      EXIT
    ENDIF
  ENDIF        
ENDSCAN
WAIT CLEAR

SELECT (lcLineFile)
SET RELATION TO
SELECT STYLE
SET RELATION TO
*--end of lfScanData.

*!*************************************************************
*! Name      : lfGetLabel
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/05/2003
*! Purpose   : To fill the Label Arrays.
*!*************************************************************
FUNCTION lfGetLabel

PRIVATE lnAlias
lnAlias = SELECT(0)
DECLARE laRpLblDsc[1,1],laRpLblId[1,1]
STORE 'No Label Found' TO laRpLblDsc[1,1],laRpLblId[1,1]

lcFullPath = SET('FULLPATH')
SET FULLPATH ON
lcCurDir = SET('DEFAULT') + CURDIR()
SET DEFAULT TO (gcRepHome+gcAct_Appl)
lnLabelCnt = ADIR(laLabel,gcAct_Appl+'*.LBX')
IF lnLabelCnt > 0
  lnCntLabel = 0
  FOR lnI = 1 TO ALEN(laLabel,1)
    USE (gcRepHome+gcAct_Appl+'\'+laLabel[lnI,1]) SHARED ALIAS LABELFILE IN 0
    SELECT LABELFILE
    LOCATE FOR PLATFORM = 'WIZARD  '
    IF FOUND()
      lnCntLabel = lnCntLabel + 1
      DECLARE laRpLblDsc[lnCntLabel,1],laRpLblId[lnCntLabel,1]
      laRpLblDsc[lnCntLabel,1] = SUBSTR(STRTRAN(laLabel[lnI,1],'.LBX'),3) + ' - ' + COMMENT
      laRpLblId[lnCntLabel,01] = STRTRAN(laLabel[lnI,1],'.LBX')
    ENDIF
    USE IN LABELFILE
  ENDFOR
  IF lnCntLabel > 0
    lcFormName = laRpLblId[1,1]
  ENDIF
ENDIF
SET DEFAULT TO (lcCurDir)
SET FULLPATH &lcFullPath.
SELECT (lnAlias)
*-- end of lfGetLabel.

*!**************************************************************************
*! Name      : lfVarPos
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/10/2003
*! Purpose   : To get the position of the Variable in OG
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
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
*! Name      : lfvModify
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/10/2003
*! Purpose   : Valid function of Modify Label option.
*!**************************************************************************
*! Example   : = lfvModify()
*!**************************************************************************
*
FUNCTION lfvModify

MODIFY LABEL (gcRepHome+gcAct_Appl+'\'+lcFormName)
USE (gcRepHome+gcAct_Appl+'\'+lcFormName+'.LBX') IN 0 ALIAS TMPLABEL
SELECT TMPLABEL
SCAN FOR OBJTYPE = 17
  lnOccurs = OCCURS('\',PICTURE)
  lnStart = AT('\',PICTURE,lnOccurs)
  lcPicture = SUBSTR(PICTURE,lnStart+1)
  REPLACE PICTURE WITH '"' + gcBmpHome + lcPicture
ENDSCAN
USE IN TMPLABEL
*-- End of lfvModify.