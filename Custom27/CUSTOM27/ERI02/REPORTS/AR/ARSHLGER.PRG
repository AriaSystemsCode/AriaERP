*:***************************************************************************
*: Program file  : ARSHLGER.PRG
*: Program desc. : Shipping Log Report For ERIC JAVITS.
*: For Report    : (ARSHLGRS.FRX,ARSHLGRN.FRX)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*! Date          : 06/02/2004
*: Developer     : BASSEM RAAFAT ERNEST (BWA)
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
*: Example : DO ARSHLGER
*:***************************************************************************
*: This Report Program is due to CP121845 ...
*:***************************************************************************
*: Modifications :
*:C123892,1 BWA 09/23/2004 Change the style group filter to let the user select more than one.
*:C123892,1 BWA 09/23/2004 Change in the SYDREPRT.
*:B124667,1 BWA 10/20/2004 Fix the bug printing wrong data.
*:***************************************************************************
*----------------------- Report Code Begin -----------------------------

PRIVATE lcExactCas
lcStTime   = TIME()    && Time in which we start collect data.

*-- Get the Variables its Initial values and add the conditions to lcrpexp in case account.
STORE SPACE(0) TO lcCustPo , lcStore
IF lcRpSortBy = 'A'
  =lfvAddCond()
ENDIF

*--Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR llOGFltCh
  llClearFn  = .F.
  lcLastTag  = ''     && to fill index field with its corresponding data.

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]
  lcRpExp    = STRTRAN(lcRpExp,'INVLINE.','')
  lcRpExp    = [invoice+STR(lineno,6) = '' AND ] + lcRpExp
  SELECT InvLine
  SET ORDER TO InvLine
  SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer

  *-- Scan to fill Temp. File with filter data.
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    *--Assign zero value to the following variables.
    IF InvHdr.STATUS="V" 
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,;
                 m.Qty8,m.TotQtY,m.price
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
LOCATE
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*-- Call Report [lcRpForm = 'ARSJOUR.FRX']
DO gfDispRe WITH EVAL('lcRpForm')
RETURN
*----------------------- Report Code End -----------------------------

                     *-- Function Section --*
*!*************************************************************
*! Name      : lfStitle
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

*--End of lfStitle.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 55
  laTempStru[ALEN(laTempStru,1)  ,4] = 0

  =lfWorkFile()
  
ENDIF  && END IF you first time enter when function.

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

*--End of lfWorkFile.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

lcItsName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF

*--End of lfvAcc.
*!*************************************************************
*! Name      : lfvLoc
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

lcObjName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*--End of lfvLoc.
*!*************************************************************
*! Name      : lfvStates 
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]

   lcFile_Ttl = ALLTRIM(lcSTitle) + ' Codes'
   lcBrfields = 'cCode_No :H= ALLTRIM(lcSTitle) + " Code " ,cDiscrep :H="Description" :30'

   IF gfBrows('FOR cdefcode+cfld_name+ccode_no+cdiscrep = ;
     "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
     
     lcStateVal = laCodeFld[1]
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal

*--End of lfvStates.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

*--End of lfCollTime.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfEvalVars
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lcStyGrp    = lcStyMajor + ' Group'

*-- Evaluate sort by arrays. [Begin]
DIMENSION laSortDesc[4,1],laSortVal[4,1]
laSortDesc[1] = 'Account'
laSortDesc[2] = lcStyMajor
laSortDesc[3] = lcStyGrp
laSortDesc[4] = lcSTitle       && State variable Title

laSortVal[1] = 'A'
laSortVal[2] = 'S'
laSortVal[3] = 'G'
laSortVal[4] = 'T'
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
*-- if multi currency evaluate currency arrays [Begin]

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

  ENDIF  && end If you Find segment of Color Type.                   

ENDFOR    && end Loop Around Non Major elements.
*-- Compute Free/Color Items in Style code Structure. [End]
*-- Evaluate non Segment values [end]

*-- Fill default sort options... [Begin]
lcRpSortBy = 'A'
llRpShowSz = .F.
=lfvSortBy()
*-- Fill default sort options... [End]

*--End of lfEvalVars.
*!*************************************************************
*! Name      : lfvShowSz
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

lcRpForm = IIF(llRpShowSz,'ARSHLGRS','ARSHLGRN')
= lfRepPltFr(lcRpForm)
=lfvSortBy(.T.)

*--End of lfvShowSz.
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

DO CASE
  CASE lcRpSortBy = 'S'		&& Sort by Style Case
    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor)
    ENDIF  
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   
      IF !llDetOnly
        lcRpIndTag = [STYLE + ACCOUNT + INVOICE]
        lcRpGroup1   = [STYLE]
        lcRpGrpHd1   = [lcStyTitle + ' : ' + Style + ' - ' + Style.Desc1]
        lcRpGrpFt1   = ['*   ' + Style + ' Totals ==>']
        STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
      ENDIF  
      IF llRpShowSz
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Store'        + SPACE(4)  + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin  = [Account + SPACE(1) +  PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6)]
      ELSE 
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Name' + SPACE(27)                  + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
        lcRpDetLin  = [Account + SPACE(1) + PADR(Customer.btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),16)]
      ENDIF
    ELSE  && else style non major has color segment.
      IF !llDetOnly
        lcRpIndTag = [SUBSTR(STYLE,1,LEN(lcMajorPic)) + SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                      ACCOUNT + INVOICE]
        lcRpGroup1   = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup2   = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
        lcRpGrpHd1   = [PADR(lcStyMajor,14) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        lcRpGrpHd2   = [PADR(lcColorTlt,19) + ': ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                      ' - ' + Style.Desc1]
        lcRpGrpFt1   = ['*   ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ' Totals ==>']
        lcRpGrpFt2   = ['**  ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) + ' Totals ==>']
        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF  
      IF llRpShowSz
        lcRpRepHed = 'Acct#'+' '      +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account  + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed = 'Acct#'+' '+'Name'+SPACE(27)                     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(Customer.btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ENDIF
    ENDIF  && end if style non major does not has color segment.
  CASE lcRpSortBy = 'G'		&& Sort by Style Group Case
    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor) + ' Group'
      lcRpGrpHd1   = ['Group            ' + ' : ' + PADR(LEFT(cTempKey,6),19) +;
                       ' - ' + gfCodDes(LEFT(cTempKey,6),"CSTYGROUP")]
      lcRpGrpFt1   = ['*'+space(2) + PADR(LEFT(cTempKey,6),19) + ' Totals ==>']
    ENDIF  
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   
      IF !llDetOnly
        lcRpIndTag = [STYLE.cStyGroup + STYLE + ACCOUNT + INVOICE]
        lcRpGroup1   = [LEFT(cTempKey,6)]
        lcRpGroup2   = [STYLE]

        lcRpGrpHd2   = [PADR(lcStyTitle,19) + ' : ' + PADR(Style,19) + ' - ' + Style.Desc1]
        lcRpGrpFt2   = ['**  ' + PADR(Style,19) + ' Totals ==>']
        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF  
      IF llRpShowSz
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin  = [Account + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6)]
      ELSE
        lcRpRepHed = 'Acct#'   + SPACE(1) + 'Name' + SPACE(27)                  + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
        lcRpDetLin  = [Account + SPACE(1) + PADR(Customer.btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),16)]
      ENDIF  
    ELSE  && else style non major has color segment.
      IF !llDetOnly
        lcRpIndTag = [STYLE.cStyGroup + SUBSTR(STYLE,1,LEN(lcMajorPic)) +;
                                        SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                                        ACCOUNT + INVOICE]
        lcRpGroup1   = [LEFT(cTempKey,6)]
        lcRpGroup2   = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup3   = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
        lcRpGrpHd2   = [PADR(lcStyMajor,14) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
     
        lcRpGrpHd3   = [PADR(lcColorTlt,19) + ': ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                          ' - ' + Style.Desc1]
        lcRpGrpFt2   = ['**  ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ' Totals ==>']
        lcRpGrpFt3   = ['***' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) + ' Totals ==>']
      ENDIF  
      IF llRpShowSz
        lcRpRepHed = 'Acct#'+' '     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed = 'Acct#'+' '+'Name'+SPACE(27)                     +'Store'+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(Customer.btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ENDIF  
    ENDIF  && end if style non major does not has color segment.

  CASE lcRpSortBy = 'A'		&& Sort by Account Case  
    IF !llDetOnly
      lcRpIndTag = [ACCOUNT + STORE + INVOICE]
      lcRpGroup1   = [ACCOUNT]

      lcRpGrpHd1   = ['Account' + ' : ' + Account + ' - ' + Customer.BtName]
      lcRpGrpFt1   = ['*   ' + Account + ' Totals ==>']

      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF  

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   

      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + &lcWorkFile..Desc1]
      ENDIF  

    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + &lcWorkFile..Desc1]
      ENDIF  

    ENDIF

  CASE lcRpSortBy = 'T'		&& Sort by State Case
     
    IF !llDetOnly
      
      lcRpHedTlt = ALLTRIM(lcSTitle)
      lcRpIndTag = [Customer.cAddress4 + ACCOUNT + INVOICE]
      lcRpGroup1   = [LEFT(cTempKey,30)]

      lcRpGrpHd1   = [lcSTitle + ' : ' + ALLTRIM(Customer.cAddress4)]
      lcRpGrpFt1   = ['*   ' + PADR(ALLTRIM(Customer.cAddress4),20) + ' Totals ==>']

      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    
    ENDIF  

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   

      IF llRpShowSz
        lcRpRepHed = 'Acct# '        +'Store'+'    '+ PADR(lcStyTitle,19)
        lcRpDetLin  = [Account + ' ' + Store+ ' '   +  Style]
        
      ELSE
        lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11)                 + 'Store'+'    '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin  = [Account + ' ' + PADR(Customer.BtName,14) +  ' '  + Store+ ' '   +  Style + ' ' + PADR(Style.Desc,12)]
      ENDIF  

    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Acct# '        +'Store'+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  =[Account + ' '  + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11)                 +'Store'+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[Account + ' ' + PADR(Customer.BtName,14) +  ' '   + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + PADR(Style.Desc,12)]
      ENDIF  

    ENDIF
ENDCASE

*--To refresh the option Grid when the user change the sort.
CLEAR READ

*--End of lfvSortBy.
*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/02/2004
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

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal

ENDIF    && End of IF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*--End of lfvOGStyle.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to fill CustPo,Store.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll

DIMENSION laRpCstSo[1,1],laRpCstTr[1,1],laRpStorS[1,1],laRpStorT[1,1]
STORE '' TO laRpCstSo,laRpCstTr,laRpStorS,laRpStorT

*--The Store.
SELECT CUSTOMER
SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" INTO ARRAY laRpStorS
LOCATE

*--The CustPo.
SELECT INVHDR
SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) INTO ARRAY laRpCstSo
LOCATE

*-- End of lfFillAll.
*!*************************************************************
*! Name      : Header for the functions calling the mover.
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to call mover CustPo,Store.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : =lfvCustPo(),lfvStore()
*!*************************************************************
*--The CustPo.
FUNCTION lfvCustPo

=lfCustpo()
=gfMover(@laRpCstSo,@laRpCstTr,'Customer Po ',.T.,'')

*--The Store.
FUNCTION lfvStore

=lfStorAcc()
=gfMover(@laRpStorS,@laRpStorT,'Store',.T.,'')

*-- End of Calling Mover for all codes.
*!*************************************************************
*! Name      : lfvAddCond
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvAddCond

llClearFn = .T.    && If you run filter you must create cursor again.

*--The filter
lcRpExp = IIF(!EMPTY(lcRpExp),lcRpExp,'.T.')

*--The Custpo.
FOR lnInd = 1 TO ALEN(laRpCstTr)
  lcCustPo = lcCustPo + PADR(laRpCstTr[lnInd],15) + ' | '
ENDFOR
lcCustPo = IIF(ALLTRIM(lcCustPo) = '|','',lcCustPo)

IF !EMPTY(lcCustPo)
  lcRpExp = lcRpExp + ' AND ALLTRIM(INVHDR.CUSTPO) $ lcCustPo'
ENDIF

*--The Store.
FOR lnInd = 1 TO ALEN(laRpStorT)
  lcStore = lcStore + PADR(laRpStorT[lnInd],8) + ' | '
ENDFOR
lcStore = IIF(ALLTRIM(lcStore) = '|','',lcStore)

IF !EMPTY(lcStore)
  *B124667,1 BWA 10/20/2004 Remove the ALLTRIM() function.[START]
  *lcRpExp = lcRpExp + ' AND ALLTRIM(INVHDR.STORE) $ lcStore '
  lcRpExp = lcRpExp + ' AND INVHDR.STORE $ lcStore '
  *B124667,1 BWA 10/20/2004.[END]
ENDIF

*-- End of lfvAddCond.
*!*************************************************************
*! Name      : lfCustpo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 15/07/2004
*! Purpose   : Function to prepare the movers hold the Custpo.
*!*************************************************************
*! Called from : lfvCustPo()
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCustpo

PRIVATE lnSavAlias
lnSavAlias  = SELECT(0)  && Variable to save the alias.

lnDataPos = ASCAN(laOgVrFlt,'INVLINE.ACCOUNT')
IF lnDataPos > 0
  DIMENSION laRpCstSo[1,1]
  STORE '' TO laRpCstSo
  SELECT INVHDR

  DO CASE
    CASE AT('Like',laOgVrFlt[lnDataPos,5])             <> 0       && Like
	    SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
	    AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., INVHDR.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpCstSo

    CASE AT('Greater Than',laOgVrFlt[lnDataPos,5])     <> 0       && Greater Than
	    SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
   		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., INVHDR.ACCOUNT > laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpCstSo

    CASE AT('Less Than',laOgVrFlt[lnDataPos,5])        <> 0       && Less Than
	  	SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., INVHDR.ACCOUNT < laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpCstSo

    CASE AT('Greater Or Equal',laOgVrFlt[lnDataPos,5]) <> 0       && Greater Or Equal
		SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., ( INVHDR.ACCOUNT > laOgVrFlt[lnDataPos,6] OR INVHDR.ACCOUNT == laOgVrFlt[lnDataPos,6])) INTO ARRAY laRpCstSo

    CASE AT('Less Or Equal',laOgVrFlt[lnDataPos,5])    <> 0       && Less Or Equal
	 	SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., ( INVHDR.ACCOUNT < laOgVrFlt[lnDataPos,6] OR INVHDR.ACCOUNT == laOgVrFlt[lnDataPos,6])) INTO ARRAY laRpCstSo

    CASE AT('Between',laOgVrFlt[lnDataPos,5])          <> 0       && Between
		SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., BETWEEN(INVHDR.ACCOUNT, LEFT(laOgVrFlt[lnDataPos,6],5) , SUBSTR(laOgVrFlt[lnDataPos,6],7))) INTO ARRAY laRpCstSo

    CASE AT('Contains',laOgVrFlt[lnDataPos,5])          <> 0      && In Contains
		SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., INVHDR.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpCstSo

    CASE AT('In List',laOgVrFlt[lnDataPos,5])          <> 0       && In List
		SELECT DISTINCT CUSTPO FROM INVHDR WHERE !EMPTY(CUSTPO) ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., INVHDR.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpCstSo
  ENDCASE
  IF !EMPTY(laRpCstTr)
    DIMENSION laCopyFond[1,1]
    STORE '' TO laCopyFond
    lnCount = 0
    FOR lnI = 1 TO ALEN(laRpCstTr,1)
      IF ASCAN(laRpCstSo,ALLTRIM(laRpCstTr[lnI])) > 0
        lnCount = lnCount + 1
        DIMENSION laCopyFond[lnCount,1]
        laCopyFond[lnCount] = laRpCstTr[lnI]
      ENDIF
    ENDFOR
    
    DIMENSION laRpCstTr[1,1]
    STORE '' TO laRpCstTr
    =ACOPY(laCopyFond , laRpCstTr)
  ENDIF
ENDIF
SELECT (lnSavAlias)

*-- End of lfCustpo.
*!*************************************************************
*! Name      : lfStorAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 15/07/2004
*! Purpose   : Function to prepare the movers hold the Store.
*!*************************************************************
*! Called from : lfvStore()
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfStorAcc

PRIVATE lnSavAlias
lnSavAlias  = SELECT(0)  && Variable to save the alias.

lnDataPos = ASCAN(laOgVrFlt,'INVLINE.ACCOUNT')
IF lnDataPos > 0
  DIMENSION laRpStorS[1,1]
  STORE '' TO laRpStorS
  SELECT CUSTOMER

  DO CASE
    CASE AT('Like',laOgVrFlt[lnDataPos,5])             <> 0       && Like
	    SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
	    AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., CUSTOMER.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpStorS

    CASE AT('Greater Than',laOgVrFlt[lnDataPos,5])     <> 0       && Greater Than
	    SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
   		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., CUSTOMER.ACCOUNT > laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpStorS

    CASE AT('Less Than',laOgVrFlt[lnDataPos,5])        <> 0       && Less Than
	    SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., CUSTOMER.ACCOUNT < laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpStorS

    CASE AT('Greater Or Equal',laOgVrFlt[lnDataPos,5]) <> 0       && Greater Or Equal
	    SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., ( CUSTOMER.ACCOUNT > laOgVrFlt[lnDataPos,6] OR CUSTOMER.ACCOUNT == laOgVrFlt[lnDataPos,6])) INTO ARRAY laRpStorS

    CASE AT('Less Or Equal',laOgVrFlt[lnDataPos,5])    <> 0       && Less Or Equal
	 	SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., ( CUSTOMER.ACCOUNT < laOgVrFlt[lnDataPos,6] OR CUSTOMER.ACCOUNT == laOgVrFlt[lnDataPos,6])) INTO ARRAY laRpStorS

    CASE AT('Between',laOgVrFlt[lnDataPos,5])          <> 0       && Between
		SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., BETWEEN(CUSTOMER.ACCOUNT, LEFT(laOgVrFlt[lnDataPos,6],5) , SUBSTR(laOgVrFlt[lnDataPos,6],7))) INTO ARRAY laRpStorS

    CASE AT('Contains',laOgVrFlt[lnDataPos,5])          <> 0      && In Contains
		SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., CUSTOMER.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpStorS

    CASE AT('In List',laOgVrFlt[lnDataPos,5])          <> 0       && In List
		SELECT DISTINCT STORE FROM CUSTOMER WHERE TYPE = "S" ;
		AND IIF(laOgVrFlt[lnDataPos,6] == SPACE(0) , .T., CUSTOMER.ACCOUNT $ laOgVrFlt[lnDataPos,6]) INTO ARRAY laRpStorS
  ENDCASE
  IF !EMPTY(laRpStorT)
    DIMENSION laCopyFond[1,1]
    STORE '' TO laCopyFond
    lnCount = 0
    FOR lnI = 1 TO ALEN(laRpStorT,1)
      IF ASCAN(laRpStorS,ALLTRIM(laRpStorT[lnI])) > 0
        lnCount = lnCount + 1
        DIMENSION laCopyFond[lnCount,1]
        laCopyFond[lnCount] = laRpStorT[lnI]
      ENDIF
    ENDFOR
    
    DIMENSION laRpStorT[1,1]
    STORE '' TO laRpStorT
    =ACOPY(laCopyFond , laRpStorT)
  ENDIF
ENDIF
SELECT (lnSavAlias)

*-- End of lfStorAcc.
*B124667,1 BWA 10/20/2004 Comment this function.[START]
*!*************************************************************
*! Name      : lfvOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 15/07/2004
*! Purpose   : Function to validate the accounts to the movers.
*!*************************************************************
*! Called from : Inlist.SPR from V\aria27\screens\inlist.spr
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*FUNCTION lfvOk
*=lfFillAll()

*-- This 2 lines of code from the lfvOk() from the INLIST.PRG. 
*llOk=.T.
*CLEAR READ

*-- End of lfvOk.