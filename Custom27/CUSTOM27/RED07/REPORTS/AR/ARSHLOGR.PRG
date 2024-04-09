*:***************************************************************************
*: Program file  : ARSHLOGR
*: Program desc. : Shipping Log Report FOR RED ENGINE JEANS
*: For Report    : (ARSHLORS.FRX,ARSHLORN.FRX)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : NADER NABIL ABD ELMONEM (NNA)
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
*: Example : DO ARSHLOGR
*:***************************************************************************
*: Reference  : CP# 037938 ...
*:***************************************************************************
*: Modification :
*B608001,1 TMI 03/11/2007 use the customized frx instead of the standard ones ( they are just a copy of the standard to avoide changes in standard)  ticket T20070213.0015
*:***************************************************************************

*----------------------- Report Code Begin -----------------------------

PRIVATE lcExactCas
lcStTime   = TIME()    && Time in which we start collect data.
*-- Declare variable if coming from standard.
IF TYPE("llTrigFG")="U"
  llTrigFG=.F.
ENDIF

*-- Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR llOGFltCh
  llClearFn  = .F.
  lcLastTag  = ''     && to fill index field with its corresponding data.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.
  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]
  *-- Allow void invoice printing [start]
  IF llTrigFG
    lcRpExp    = STRTRAN(lcRpExp,'INVHDR.STATUS <> "V" AND','')
  ENDIF
  lcRpExp    = STRTRAN(lcRpExp,'INVLINE.','')
  lcRpExp    = [invoice+STR(lineno,6) = '' AND ] + lcRpExp
  SELECT InvLine
  SET ORDER TO InvLine
  SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer
  SELECT InvHdr
  SET RELATION TO Invhdr.rep1 INTO Salesrep ADDITIVE
  SELECT InvLine
  *-- Scan to fill Temp. File with filter data.
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    *-- Assign zero value to the following variables.
    IF InvHdr.STATUS="V" 
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,;
                 m.Qty8,m.TotQtY,m.price
    ENDIF
    INSERT INTO (lcWorkFile) FROM MEMVAR
    REPLACE &lcWorkFile..REPCode   WITH INVHDR.REP1
    REPLACE &lcWorkFile..REPName   WITH Salesrep.Name

    *-- Get the Color Description in case we used sorted By color.
    lcClr = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    REPLACE &lcWorkFile..cColorDesc WITH ALLTRIM(gfCodDes(lcClr,'COLOR     '))

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

*-- Call Report [lcRpForm = 'ARSJOUR.FRX']
DO gfDispRe WITH EVAL('lcRpForm')
RETURN
*----------------------- Report Code End -----------------------------
*-- Function Section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*-- end of lfStitle.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
  lnFileStru = ALEN(laTempStru,1)
  DIMENSION laTempStru[lnFileStru + 4, 4]

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[lnFileStru + 01  ,1] = 'RepCode'
  laTempStru[lnFileStru + 01  ,2] = 'C'
  laTempStru[lnFileStru + 01  ,3] = 3
  laTempStru[lnFileStru + 01  ,4] = 0
  
  laTempStru[lnFileStru + 02  ,1] = 'RepName'
  laTempStru[lnFileStru + 02  ,2] = 'C'
  laTempStru[lnFileStru + 02  ,3] = 24
  laTempStru[lnFileStru + 02  ,4] = 0

  laTempStru[lnFileStru + 03 ,1] = 'cTempKey'
  laTempStru[lnFileStru + 03 ,2] = 'C'
  laTempStru[lnFileStru + 03 ,3] = 55
  laTempStru[lnFileStru + 03 ,4] = 0

  laTempStru[lnFileStru + 04 ,1] = 'cColorDesc'
  laTempStru[lnFileStru + 04 ,2] = 'C'
  laTempStru[lnFileStru + 04 ,3] = 30
  laTempStru[lnFileStru + 04 ,4] = 0

  =lfWorkFile()
  
ENDIF  && END IF you first time enter when function.
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfWorkFile
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru
CREATE TABLE (lcWorkFile) ;
   FROM ARRAY laTempStru

SELECT (lcWorkFile)
INDEX ON cTempKey TAG (lcWorkFile)
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF
*-- end of lfvAcc.

*!*************************************************************
*! Name      : lfvLoc
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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

*--IF The user want to Browse or if the location he entered is not in the
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvLoc.

*!*************************************************************
*! Name      : lfvStates 
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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

*--IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]

   lcFile_Ttl = ALLTRIM(lcSTitle) + ' Codes'
   lcBrfields = 'cCode_No :H= ALLTRIM(lcSTitle) + " Code " ,cDiscrep :H="Description" :30'

   *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
   *  gcAct_Comp+"STATE" AND ;
   *  cRltField="N"','cCode_No,cDiscrep','laCodeFld')
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
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
DIMENSION laSortDesc[6,1],laSortVal[6,1]
laSortDesc[1] = 'Account'
laSortDesc[2] = lcStyMajor
laSortDesc[3] = lcStyGrp
laSortDesc[4] = lcSTitle       && State variable Title
laSortDesc[5] = 'Sales Rep.'
laSortDesc[6] = 'Color'

laSortVal[1] = 'A'
laSortVal[2] = 'S'
laSortVal[3] = 'G'
laSortVal[4] = 'T'
laSortVal[5] = 'R'
laSortVal[6] = 'C'


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
*-- end of lfSortDumy.

*!*************************************************************
*! Name      : lfvShowSz
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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

*B608001,1 TMI [Start] use the customized frx instead of the standard ones ( they are just a copy of the standard )
*lcRpForm = IIF(llRpShowSz,'ARSHLOGS','ARSHLOGN')
lcRpForm = IIF(llRpShowSz,'ARSHLORS','ARSHLORN')
*B608001,1 TMI [End  ] 
= lfRepPltFr(lcRpForm)
=lfvSortBy(.T.)

*-- End of lfvShowSz...
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
        *C200135,1 KAM for report adjustment[start]
        *lcRpGrpHd1   = [PADR(lcStyMajor,19) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        *lcRpGrpHd2   = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                      ' - ' + Style.Desc1]
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
        lcRpRepHed = 'Store'+ SPACE(4) + PADR(ALLTRIM(lcSTitle),6)           + SPACE(1) + PADR(lcStyTitle,19)
        lcRpDetLin  = [Store+ SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6) + SPACE(1) + Style]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
        *-- Use Style Long Description Where Possible
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + &lcWorkFile..Desc1]
      ENDIF  
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
         *-- Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print.
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
       ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + &lcWorkFile..Desc1]
      ENDIF  
    ENDIF

  CASE lcRpSortBy = 'R'		&& Sort by Sales Rep. Case  
    IF !llDetOnly
      lcRpIndTag = [REPCODE + ACCOUNT + STORE + INVOICE]
      lcRpGroup1   = [RepCode]                             && Group By Sales Rep. Code
      lcRpGroup2   = [ACCOUNT]                             && Group By Sales Rep. Code      
      lcRpGrpHd1   = ['Sales Rep.' + ' : ' + RepCode + ' - ' + RepName]
      lcRpGrpHd2   = [SPACE(2) + 'Account' + ' : ' + Account + ' - ' + Customer.BtName]
      lcRpGrpFt1  =  ['*   ' + RepCode + ' Totals ==>']
      lcRpGrpFt2   = ['**   ' + Account + ' Totals ==>']
      
      STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3      
    ENDIF  
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   
      IF llRpShowSz
        lcRpRepHed = 'Store'        +SPACE(4) + PADR(lcStyTitle,19)
        lcRpDetLin  = [PADR(Store,8) +SPACE(1) + Style]
      ELSE
        lcRpRepHed = 'Store'        +SPACE(4) +PADR(ALLTRIM(lcSTitle),6)           +SPACE(1)+ PADR(lcStyTitle,19)+' Description'
        *-- Use Style Long Description Where Possible
        lcRpDetLin  = [PADR(Store,8) +SPACE(1) +PADR(ALLTRIM(Customer.cAddress4),6)+SPACE(1)+ Style +SPACE(1)+ &lcWorkFile..Desc1]
      ENDIF  
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'        +SPACE(4) +PADR(ALLTRIM(lcSTitle),6)           +SPACE(1)+ PADR(lcStyMajor,11)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
         *-- Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print.
        lcRpDetLin  =[PADR(Store,8) +SPACE(1)  +PADR(ALLTRIM(Customer.cAddress4),6)+SPACE(1)+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
       ELSE
        lcRpRepHed = 'Store'       +SPACE(4) +PADR(ALLTRIM(lcSTitle),6)          +SPACE(1)+PADR(lcStyMajor,12)                           +SPACE(1)+ PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[PADR(Store,8) +SPACE(1) +PADR(ALLTRIM(Customer.cAddress4),6)+SPACE(1)+PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) +SPACE(1)+ SUBSTR(STYLE,lnNonMajSt,lnColorLen) +SPACE(1)+ &lcWorkFile..Desc1]
      ENDIF  
    ENDIF

  *-- Sort by COLOR Case  
  CASE lcRpSortBy = 'C'		
    IF !llDetOnly
      lcRpIndTag = [SUBSTR(STYLE,lnNonMajSt,lnColorLen) + SUBSTR(STYLE,1,LEN(lcMajorPic)) + ;
                      ACCOUNT + INVOICE]
      lcRpGroup1   = [ALLTRIM(SUBSTR(STYLE,lnNonMajSt,lnColorLen))]  && Group By Color (Style NonMajor)
      lcRpGrpHd1   = ['Color' + ' : ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' - ' + cColorDesc]
      lcRpGrpFt1  =  ['*   ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' Totals ==>']
      
       STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3      
    ENDIF  
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)   
      IF llRpShowSz
        lcRpRepHed = 'Store'        + SPACE(4) + PADR(ALLTRIM(lcSTitle),6)           + SPACE(1) + PADR(lcStyTitle,19)        
        lcRpDetLin  = [PADR(Store,8)+ SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6) + SPACE(1) + Style]
      ELSE
        lcRpRepHed  = 'Store'        +SPACE(4) +PADR(ALLTRIM(lcSTitle),6)           + SPACE(1)+ PADR(lcStyTitle,19)+SPACE(1)+'Description'
        *-- Use Style Long Description Where Possible
        lcRpDetLin  = [PADR(Store,8) +SPACE(1) +PADR(ALLTRIM(Customer.cAddress4),6) + SPACE(1)+ Style              +SPACE(1)+&lcWorkFile..Desc1]
      ENDIF  
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Acct#'  + SPACE(1) +'Store'       + SPACE(4) +PADR(ALLTRIM(lcSTitle),6)           +SPACE(1)+ PADR(lcStyMajor,12)
         *-- Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print.
        lcRpDetLin  =[Account + SPACE(1) +PADR(Store,8) + SPACE(1) +PADR(ALLTRIM(Customer.cAddress4),6) +SPACE(1)+SUBSTR(STYLE,1,LEN(lcMajorPic))]
       ELSE
        lcRpRepHed = 'Acct#'  + SPACE(1) + 'Name' + SPACE(27) + 'Store'       + SPACE(4) + PADR(ALLTRIM(lcSTitle),6)           + SPACE(1) + PADR(lcStyMajor,12)
        lcRpDetLin  =[Account + SPACE(1) + PADR(Customer.btname,30)+SPACE(1)+ PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(Customer.cAddress4),6) + SPACE(1) + SUBSTR(STYLE,1,LEN(lcMajorPic))]
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
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
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
*
FUNCTION lfvOGStyle
PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal

ENDIF    && End of IF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- end of lfvOGStyle.

*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
*! Purpose   : Go top in Sales Rep file.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSerSRep()
*!**************************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SalesRep 
    SET ORDER TO 
ENDCASE
*-- End of lfSetSRep.

*!*************************************************************
*! Name      : lfStySum
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
*! Purpose   : sum a specific field for the current style in style file
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
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfsrSty
*! Developer : NADER NABIL (NNA)
*! Date      : 05/09/2004
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- end of lfSRSty.

