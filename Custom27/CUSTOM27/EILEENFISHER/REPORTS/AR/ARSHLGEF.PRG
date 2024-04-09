*:***************************************************************************
*: Program file  : ARSHLGEF.PRG
*: Program desc. : Shipping Log Report FOR EILEEN FISHER.
*: For Report    : (ARSHLGEN.FRX,ARSHLGES.FRX,ARSHLGC1.FRX,ARSHLGC2.FRX,ARSHLGC3.FRX,ARSHLGC4.FRX
*: System        : Aria Advantage Series.
*: Date          : 12/10/2000
*: Module        : Account Receivable (AR)
*: Developer     : BASSEM RAFFAT ERNEST (BWA)
*: Job Number    : C102003
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfBrows,gfStyBrw,CusBrowM.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,lfWorkFile,lfvShowSz
*:               : lfCollData,lfCollTime,lfvSortBy,lfEvalVars,lfvOGStyle
*:               : lfStitle,lfvAcc,lfvLoc,gfBrowWare,gfOpenFile,gfItemMask
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARSHLGEF
*:***************************************************************************
*----------------------- Report Code Begin -----------------------------
PRIVATE lcExactCas
lcStTime   = TIME()    && Time in which we start collect data.

IF llClearFn OR llOGFltCh
  llClearFn  = .F.       && If you run filter you must create cursor again.
  lcLastTag  = ''        && to fill index field with its corresponding data.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  *-- if you have previous data clear workfile then recreate it. [Begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [End]

  lcRpExp    = STRTRAN(lcRpExp,'INVLINE.','')
  lcRpExp    = [invoice+STR(lineno,6) = '' AND ] + lcRpExp

  SELECT InvLine
  SET ORDER TO InvLine

  SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer

  *-- Scan to fill Temp. File with filter data.
  SCAN FOR &lcRpExp

    SCATTER MEMVAR MEMO
    M.Fabric  = STYLE.FABRIC
    M.COLOR   = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    IF !EMPTY(STYLE.FABRIC)
      =SEEK(STYLE.FABRIC,'FABRIC')
      M.FabDesc = FABRIC.DESC
    ELSE
      M.FabDesc = SPACE(0)
    ENDIF
    INSERT INTO (lcWorkFile) FROM MEMVAR

  ENDSCAN  && end Scan to fill Temp. File with filter data.

  SELECT InvLine
  SET ORDER TO
  SET RELATION TO

ENDIF

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (lcWorkFile)
SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer

*-- ReIndex work file if first time collect data or user change sort By.
IF !(lcRpIndTag == lcLastTag)
  lcLastTag = lcRpIndTag
  REPLACE ALL cTempKey WITH EVALUATE(lcRpIndTag)
ENDIF
GO TOP

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*-- Call Report [lcRpForm = 'ARSHLGES.FRX']
DO gfDispRe WITH EVAL('lcRpForm')
RETURN
*----------------------- Report Code End -----------------------------

*-- Function Section.
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfOpenFile,lfFltState,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Country state title.
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************
FUNCTION lfStitle

SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')   
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')

llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
RETURN (SYCINT.CPART4LAB)

*-- End of lfStitle.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- if it's first time to run the report.
IF TYPE('lcLastTag') = 'N'

  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.
  SET ORDER TO INVHDR IN INVHDR
  SET ORDER TO INVLINE IN INVLINE
  SET ORDER TO STYLE IN STYLE

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVLINE.INVDATE'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.CCURRCODE'),1)
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF

  IF llMultLoc
    SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  ENDIF

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT INVLINE
  = AFIELDS(laTempStru)

  DIMENSION laTempStru[ALEN(laTempStru,1) + 4, 4]

  *-- Fabric Descreption : Field comes from the fabric file and used to get the fabric descreption value
  laTempStru[ALEN(laTempStru,1) - 3  ,1] = 'FabDesc'
  laTempStru[ALEN(laTempStru,1) - 3  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) - 3  ,3] = 20
  laTempStru[ALEN(laTempStru,1) - 3  ,4] = 0

  *-- Fabric : Field comes from the style file and used to get the fabric value
  laTempStru[ALEN(laTempStru,1) - 2  ,1] = 'Fabric'
  laTempStru[ALEN(laTempStru,1) - 2  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) - 2  ,3] = 7
  laTempStru[ALEN(laTempStru,1) - 2  ,4] = 0

  *-- Color : Field comes from the invline file and used to get the color value
  laTempStru[ALEN(laTempStru,1) - 1  ,1] = 'Color'
  laTempStru[ALEN(laTempStru,1) - 1  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) - 1  ,3] = 6
  laTempStru[ALEN(laTempStru,1) - 1  ,4] = 0

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 55
  laTempStru[ALEN(laTempStru,1)  ,4] = 0

  =lfWorkFile()

ENDIF  && END IF you first time enter when function.

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Create work cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
FUNCTION lfWorkFile

CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru

SELECT (lcWorkFile)
INDEX ON cTempKey TAG (lcWorkFile)

*-- End of lfWorkFile.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- End of lfwOldVal.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*-- End of lfCollTime.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

llClearFn = .T.    && If you run filter you must create cursor again.

*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF

*-- End of lfClearRep.
*!*************************************************************
*! Name      : lfEvalVars
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfOpenFile,gfItemMask,gfGetMemVar,lfvSortBy
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars

llMultCurr  = gfGetMemVar('llMulCurr')
lcStyGrp    = lcStyMajor + ' Group'

*-- Evaluate sort by arrays. [Begin]
DIMENSION laSortDesc[6,1],laSortVal[6,1]
laSortDesc[1] = 'Account'
laSortDesc[2] = lcStyMajor
laSortDesc[3] = lcStyGrp
laSortDesc[4] = lcSTitle       && State variable Title
laSortDesc[5] = 'Fabric'
laSortDesc[6] = 'Account+Fabric'

laSortVal[1] = 'A'
laSortVal[2] = 'S'
laSortVal[3] = 'G'
laSortVal[4] = 'T'
laSortVal[5] = 'F'
laSortVal[6] = 'B'
*-- Evaluate sort by arrays. [End]

*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays [End]

*-- Evaluate non Segment values [begin]

lcStyTitle  = gfItemMask('HI')  && Full Style title.
lnMajSeg    = gfItemMask('SM')  && No. of major segments.

*-- Compute Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)

  *-- If you Find segment of Color Type
  IF laMajSegs[lnI,1] = 'C'

    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = laMajSegs[lnI,3]
    lcColorTlt = PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3]))
    lnColorLen = LEN(lcNonMajPi)
    EXIT

  ENDIF

ENDFOR    && end Loop Around Non Major elements.
*-- Compute Free/Color Items in Style code Structure. [End]
*-- Evaluate non Segment values [end]

*-- Fill default sort options... [Begin]
lcRpSortBy = 'A'
llRpShowSz = .F.
=lfvSortBy()
*-- Fill default sort options... [End]

*-- End of lfSortDumy.
*!*************************************************************
*! Name      : lfvShowSz
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Valid function for show sizes .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfRepPltFr,lfvSortBy
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvShowSz()
*!*************************************************************
FUNCTION lfvShowSz
PARAMETERS lcFilter

*-- ARSHLGC1      Sort by Fabric and detail and show size breakdown = NO
*-- ARSHLGC2      Sort by Fabric and detail and show size breakdown = Yes
*-- ARSHLGC3      Sort by Fabric and summery and show size breakdown = NO
*-- ARSHLGC4      Sort by Fabric and summery and show size breakdown = Yes

DO CASE
  CASE lcFilter = '1'
    lcRpForm = IIF(llRpShowSz,'ARSHLGES','ARSHLGEN')

  CASE lcFilter = '2'
   IF lcRpPrint = 'D'
      lcRpForm = IIF(llRpShowSz,'ARSHLGC2','ARSHLGC1')
    ELSE
      lcRpForm = IIF(llRpShowSz,'ARSHLGC4','ARSHLGC3')
    ENDIF

ENDCASE

= lfRepPltFr(lcRpForm)
=lfvSortBy(.T.)

*-- End of lfvShowSz...
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : .T. or .F.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
PARAMETERS llDetOnly

*-- Case sort by Fabric , Account+Fabric
IF lcRpSortBy $ 'FB'
  IF lcRpPrint = 'D'
    lcRpForm = IIF(llRpShowSz,'ARSHLGC2','ARSHLGC1')
  ELSE
    lcRpForm = IIF(llRpShowSz,'ARSHLGC4','ARSHLGC3')
  ENDIF
ENDIF

DO CASE
  CASE lcRpSortBy = 'F'		&& Sort by Fabric

    IF !llDetOnly
      lcRpHedTlt = "Fabric"
      lcRpIndTag = [FABRIC + COLOR + STYLE]
      lcRpGroup1 = [FABRIC]
      lcRpGroup2 = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]

      lcRpGrpHd1 = ['FABRIC ' + SPACE(12) + ' : ' + FABRIC + ' - ' + FabDesc]
      lcRpGrpHd2 = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),lnColorLen+1) +;
                       ' - ' + Style.Desc1]
      lcRpGrpFt1 = ['*   ' + FABRIC + ' Totals ==>']
      lcRpGrpFt2 = ['**  ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),lnColorLen) + ' Totals ==>']

      STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
    ENDIF

    IF lcRpPrint = 'S'
      lcRpGrpHd1 = ['FABRIC ' + SPACE(12) + ' : ' + FABRIC]
    ELSE
      lcRpGrpHd1 = ['FABRIC ' + SPACE(12) + ' : ' + FABRIC + ' - ' + FabDesc]
    ENDIF

    IF llRpShowSz
      lcRpRepHed = 'Acct#' + ' ' + 'Store   ' + PADR(ALLTRIM(lcStyMajor),12)                  + ' ' + PADR(ALLTRIM(lcColorTlt),6)
      lcRpDetLin = [ACCOUNT+ ' ' + Store      + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
    ELSE
      lcRpRepHed = 'Acct#'   + SPACE(1) + 'Name' + SPACE(27)                  + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),2) + SPACE(1) + PADR(ALLTRIM(lcStyMajor),12) + ' ' + PADR(ALLTRIM(lcColorTlt),6)
      lcRpDetLin = [ACCOUNT+ ' ' + Customer.Btname +' '+ Store + ' '+PADR(ALLTRIM(Customer.cAddress4),2) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
    ENDIF
                          *******************************
  CASE lcRpSortBy = 'B'		&& Sort by Account + Fabric

    IF !llDetOnly
      lcRpHedTlt = 'Accont + Fabric'
      lcRpIndTag = [ACCOUNT + FABRIC + COLOR + STYLE]
      lcRpGroup1 = [ACCOUNT]
      lcRpGroup2 = [FABRIC]
      lcRpGroup3 = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]

      lcRpGrpHd1 = ['ACCOUNT ' + ' : ' + Account + ' - ' + Customer.BtName]
      lcRpGrpFt1 = ['*   ' + ACCOUNT + ' Totals ==>']

      lcRpGrpHd2 = ['FABRIC ' + SPACE(1) + ' : ' + FABRIC]
      lcRpGrpHd3 = [PADR(lcColorTlt,8) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),lnColorLen+1)]

      lcRpGrpFt1 = ['**  ' + ACCOUNT + ' Totals ==>']
      lcRpGrpFt2 = ['*   ' + "FABRIC     :" +  FABRIC + ' Totals ==>']
      lcRpGrpFt3 = ['**  ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),lnColorLen) + ' Totals ==>']
    ENDIF

    IF llRpShowSz
      lcRpRepHed = 'Store'+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
      lcRpDetLin = [Store+ ' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
    ELSE
      lcRpRepHed = 'Store'+'    ' + PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
      lcRpDetLin = [Store+ ' '    + PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + Style.Desc1]
    ENDIF
                          *******************************
  CASE lcRpSortBy = 'S'		&& Sort by Style Case

    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor)
    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF !llDetOnly
        lcRpIndTag = [STYLE + ACCOUNT + INVOICE]
        lcRpGroup1 = [STYLE]

        lcRpGrpHd1 = [lcStyTitle + ' : ' + Style + ' - ' + Style.Desc1]
        lcRpGrpFt1 = ['*   ' + Style + ' Totals ==>']

        STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
      ENDIF

      IF llRpShowSz
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Store'        + SPACE(4)  + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin = [Account + SPACE(1) +  PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6)]
      ELSE
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Name' + SPACE(27)                  + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
        lcRpDetLin = [Account + SPACE(1) + PADR(Customer.btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),16)]
      ENDIF

    ELSE  && else style non major has color segment.

      IF !llDetOnly
        lcRpIndTag = [SUBSTR(STYLE,1,LEN(lcMajorPic)) + SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                      ACCOUNT + INVOICE]
        lcRpGroup1 = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup2 = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]

        lcRpGrpHd1 = [PADR(lcStyMajor,19) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        lcRpGrpHd2 = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                      ' - ' + Style.Desc1]
        lcRpGrpFt1 = ['*   ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ' Totals ==>']
        lcRpGrpFt2 = ['**  ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) + ' Totals ==>']

        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF

      IF llRpShowSz
        lcRpRepHed = 'Acct#'+' '      +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Account  + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed = 'Acct#'+' '+'Name'+SPACE(27)                     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Account + ' ' + PADR(Customer.btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ENDIF

    ENDIF  && end if style non major does not has color segment.

                          *******************************
  CASE lcRpSortBy = 'G'		&& Sort by Style Group Case

    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor) + ' Group'
      lcRpGrpHd1 = ['Group              ' + ' : ' + PADR(LEFT(cTempKey,6),19) +;
                        ' - ' + gfCodDes(LEFT(cTempKey,6),"CSTYGROUP")]
      lcRpGrpFt1 = ['*   ' + PADR(LEFT(cTempKey,6),19) + ' Totals ==>']
    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF !llDetOnly
        lcRpIndTag = [STYLE.cStyGroup + STYLE + ACCOUNT + INVOICE]
        lcRpGroup1 = [LEFT(cTempKey,6)]
        lcRpGroup2 = [STYLE]

        lcRpGrpHd2 = [PADR(lcStyTitle,19) + ' : ' + PADR(Style,19) + ' - ' + Style.Desc1]
        lcRpGrpFt2 = ['**  ' + PADR(Style,19) + ' Totals ==>']

        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF

      IF llRpShowSz
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin = [Account + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6)]
      ELSE
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Name' + SPACE(27)                  + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
        lcRpDetLin = [Account + SPACE(1) + PADR(Customer.btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),16)]
      ENDIF

    ELSE  && else style non major has color segment.

      IF !llDetOnly
        lcRpIndTag = [STYLE.cStyGroup + SUBSTR(STYLE,1,LEN(lcMajorPic)) +;
                                        SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                                        ACCOUNT + INVOICE]
        lcRpGroup1 = [LEFT(cTempKey,6)]
        lcRpGroup2 = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup3 = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]

        lcRpGrpHd2 = [PADR(lcStyMajor,19) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        lcRpGrpHd3 = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                      ' - ' + Style.Desc1]
        lcRpGrpFt2 = ['**  ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ' Totals ==>']
        lcRpGrpFt3 = ['*** ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) + ' Totals ==>']
      ENDIF

      IF llRpShowSz
        lcRpRepHed = 'Acct#'+' '     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Account + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed = 'Acct#'+' '+'Name'+SPACE(27)                     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Account + ' ' + PADR(Customer.btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ENDIF

    ENDIF  && end if style non major does not has color segment.

                          *******************************
  CASE lcRpSortBy = 'A'		&& Sort by Account Case
 
    IF !llDetOnly
      lcRpIndTag = [ACCOUNT + STORE + INVOICE]
      lcRpGroup1 = [ACCOUNT]

      lcRpGrpHd1 = ['Account' + ' : ' + Account + ' - ' + Customer.BtName]
      lcRpGrpFt1 = ['*   ' + Account + ' Totals ==>']

      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)
        lcRpDetLin = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + Style.Desc1]
      ENDIF
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + Style.Desc1]
      ENDIF
    ENDIF

                          *******************************
  CASE lcRpSortBy = 'T'		&& Sort by State Case

    IF !llDetOnly

      lcRpHedTlt = ALLTRIM(lcSTitle)
      lcRpIndTag = [Customer.cAddress4 + ACCOUNT + INVOICE]
      lcRpGroup1 = [LEFT(cTempKey,30)]

      lcRpGrpHd1 = [lcSTitle + ' : ' + ALLTRIM(Customer.cAddress4)]
      lcRpGrpFt1 = ['*   ' + PADR(ALLTRIM(Customer.cAddress4),20) + ' Totals ==>']

      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF llRpShowSz
        lcRpRepHed = 'Acct# '        +'Store'+'    '+ PADR(lcStyTitle,19)
        lcRpDetLin = [Account + ' ' + Store+ ' '   +  Style]
      ELSE
        lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11)                 + 'Store'+'    '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin = [Account + ' ' + PADR(Customer.BtName,14) +  ' '  + Store+ ' '   +  Style + ' ' + PADR(Style.Desc,12)]
      ENDIF
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Acct# '        +'Store'+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin = [Account + ' '  + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11)                 +'Store'+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin = [Account + ' ' + PADR(Customer.BtName,14) +  ' '   + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + PADR(Style.Desc,12)]
      ENDIF
    ENDIF

ENDCASE
*-- End of lfvSortBy.

              *-- Section of the valid functions called from OG --*
*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvOGStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle

lcObjName = SYS(18)
lcObjVal = EVALUATE(SYS(18))

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*-- End of lfvOGStyle.
*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Validate function for the Customer Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc

PRIVATE lcItsName , lcItsVal , llObjRet

lcItsName = SYS(18)
lcItsVal = EVALUATE(SYS(18))

IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF

*-- End of lfvAcc.
*!*************************************************************
*! Name      : lfvLoc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Validate location Code field
*!*************************************************************
*! Called from : Location field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)
lcObjVal = EVALUATE(SYS(18))

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF

*-- End of lfvLoc.
*!*************************************************************
*! Name      : lfvStates
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [State  Object]- title is changes with country.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStates()
*!*************************************************************
FUNCTION lfvStates

PRIVATE lcStateObj , lcStateVal

lcStateObj = SYS(18)
lcStateVal = EVALUATE(SYS(18))

IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = ALLTRIM(lcSTitle) + ' Codes'
   lcBrfields = 'cCode_No :H= ALLTRIM(lcSTitle) + " Code " ,cDiscrep :H="Description" :30'
   IF gfBrows('FOR cdefcode+cfld_name+ccode_no+cdiscrep = ;
     "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
     lcStateVal = laCodeFld[1]
   ENDIF
   SELECT (lnCurAlias)
ENDIF
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal

*-- End of lfvStates.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : Refresh the options in the option grid.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClrRead()
*!*************************************************************
FUNCTION lfClrRead

CLEAR READ

*-- End of lfClrRead.
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 12/10/2000
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE

*-- End of lfSRVFab.