*:***************************************************************************
*: Program file  : ARGMASLG
*: Program desc. : GMA Shipping Log Report
*: For Report    : (ARGMASLS.FRX,ARGMASLN.FRX)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Nader Anis Mitry (NAD)
*: Note          : Converted from  ARSHLOG.PRG
*: Ref           : C101798,1 
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
*: Example : DO ARGMASLG
*:***************************************************************************
*B603844,1 NAD 08/22/2000 Fix bug Round Ext amount in GMA Shipping log report
*B603844,1                By removing the CIEL Function from the Field in the 2 frx's
*B603844,1                ARGMASLN.FRX ,ARGMASLS.FRX
*----------------------- Report Code Begin -----------------------------
PRIVATE lcExactCas

lcStTime   = TIME()    && Time in which we start collect data.

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

  lcRpExp    = STRTRAN(lcRpExp,'INVLINE.','')
  lcRpExp    = [invoice+STR(lineno,6) = '' AND ] + lcRpExp
  
  *C101798,1 (Start) if summerize multi store order change the index
  IF llSumrze 
    SELECT(lcWorkFile)
    INDEX ON ORDER+STYLE TAG Order ADDI
  ENDIF      
  *C101798,1 (End) 
  
  SELECT InvLine
  SET ORDER TO InvLine
  
  SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer
                  
  *C101798,1 (Start) relation to the icdeptdt 
  SET RELATION TO style INTO Icdeptdt ADDITIVE                  
  *C101789,1 (End) 

  *-- Scan to fill Temp. File with filter data.
  SCAN FOR &lcRpExp
    
    SCATTER MEMVAR MEMO 
    *C101798,1 (Start)  Fill the temp file.
    *INSERT INTO (lcWorkFile) FROM MEMVAR
    m.Dept  = ICDEPTDT.Dept   
     
    IF llSumrze AND !(lcRpSortBy $ "IHR");
                    AND SEEK(m.Order+m.Style,lcWorkFile)
      SELECT (lcWorkfile)
      FOR lnIndex = 1 to 8
        lcIndex = STR(lnIndex,1)
        REPLACE Qty&lcIndex WITH Qty&lcIndex+InvLine.Qty&lcIndex
      ENDFOR
      REPLACE  TotQty     WITH TotQty     + InvLine.TotQty ,;
               Edi_PK_Amt WITH Edi_PK_Amt + (InvLine.TotQty*InvLine.Price) ,;
               PRICE      WITH IIF(TotQty=0,0,Edi_PK_Amt/TotQty)  ,; 
               Invoice    WITH '' ,;
               InvDate    WIth {}
    ELSE
      m.Store = IIF(llSumrze AND !(lcRpSortBy $ "IHR"),SPACE(8),Store)
      INSERT INTO (lcWorkFile) FROM MEMVAR
      SELECT (lcWorkFile)
      REPLACE  Edi_PK_Amt WITH (InvLine.TotQty*InvLine.Price)
    ENDIF
    *C101798,1 (End) 
  ENDSCAN  && end Scan to fill Temp. File with filter data.
  
  *C101798,1 (Start) Restore the last index 
  IF llSumrze  
    SELECT(lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)    
  ENDIF      
  *C101798,1 (End)
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/13/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
  
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]  
  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 55
  laTempStru[ALEN(laTempStru,1)  ,4] = 0

  *C101798,1 (Start) Add the dept field to the temp file
  lnStrArr = ALEN(laTempStru,1)
  DIMENSION laTempStru[lnStrArr+1,4]
  laTempStru[lnStrArr+1,1]='Dept'
  laTempStru[lnStrArr+1,2]='C'
  laTempStru[lnStrArr+1,3]='5'
  laTempStru[lnStrArr+1,4]='0' 
  *C101798,1 (End)
  =lfWorkFile()
  
ENDIF  && END IF you first time enter when function.
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/12/1998
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
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/12/1998
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

*IF The user want to Browse or if the location he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvLoc.

*!*************************************************************
*! Name      : lfvStates 
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/09/98
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

*IF The user want to Browse or if the state code he entered is not in the file.
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
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/17/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/16/1998
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
*C101798,1 (Start) Add the new sort by department,invoice,shipping date ,sales rep
*DIMENSION laSortDesc[4,1],laSortVal[4,1]
DIMENSION laSortDesc[8,1],laSortVal[8,1]
*C101798,1 (End)

laSortDesc[1] = 'Account'
laSortDesc[2] = lcStyMajor
laSortDesc[3] = lcStyGrp
laSortDesc[4] = lcSTitle       && State variable Title

*C101798,1 (Start) Add the new sort by department,invoice,shipping date ,sales rep
laSortDesc[5] = 'Department'
laSortDesc[6] = 'Invoice'
laSortDesc[7] = 'Shipping date'
laSortDesc[8] = 'Sales Rep'
*C101798,1 (End)

laSortVal[1] = 'A'
laSortVal[2] = 'S'
laSortVal[3] = 'G'
laSortVal[4] = 'T'

*C101798,1 (Start) Add the new sort by department,invoice,shipping date ,sales rep
laSortVal[5] = 'D' && Sort by Department 
laSortVal[6] = 'I' && Sort by Invoice 
laSortVal[7] = 'H' && Sort by Ship Date
laSortVal[8] = 'R' && Sort by Sales Rep 
*C101798,1 (End)
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/16/1998
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

lcRpForm = IIF(llRpShowSz,'ARGMASLS','ARGMASLN')
= lfRepPltFr(lcRpForm)
=lfvSortBy(.T.)
*-- end of lfvShowSz...

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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

        lcRpGrpHd1   = [PADR(lcStyMajor,19) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        lcRpGrpHd2   = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
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
      lcRpGrpHd1   = ['Group              ' + ' : ' + PADR(LEFT(cTempKey,6),19) +;
                        ' - ' + gfCodDes(LEFT(cTempKey,6),"CSTYGROUP")]
      lcRpGrpFt1   = ['*   ' + PADR(LEFT(cTempKey,6),19) + ' Totals ==>']
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

        lcRpGrpHd2   = [PADR(lcStyMajor,19) + ' : ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]
        lcRpGrpHd3   = [PADR(lcColorTlt,19) + ' : ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                        ' - ' + Style.Desc1]
        lcRpGrpFt2   = ['**  ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ' Totals ==>']
        lcRpGrpFt3   = ['*** ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) + ' Totals ==>']
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
        lcRpDetLin =[Store + ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin  =[Store + ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + Style.Desc1]
      ENDIF  

    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
        lcRpDetLin  =[Store + ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[Store + ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + Style.Desc1]
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
        lcRpDetLin  = [Account + ' '+ Store + ' '   +  Style]
      ELSE
        lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11)                 + 'Store'+'    '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin  = [Account + ' ' + PADR(Customer.BtName,14) +  ' '  + Store + ' '   +  Style + ' ' + PADR(Style.Desc,12)]
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
  *C101798,1 (Start) Add new sort by Department ,ShipDate ,SalesRep And Invoice
  CASE lcRpSortBy = 'D'
    IF !llDetOnly
      lcRpHedTlt = 'DEPARTMENT'
      lcRpIndTag = [DEPT + Style]
      lcRpGroup1   = [Dept]
      lcRpGrpHd1   = ['Department' + ' : ' + Dept + ' - ' + IIF(SEEK(&lcWorkFile..DEPT,'ICDEPTHD'),ICDEPTHD.cDeptDesc,SPACE(30))]
      lcRpGrpFt1   = ['*   ' + Dept + ' Totals ==>']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF  
    
    IF EMPTY(lcFree_Clr)
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + Style.Desc1]
      ENDIF
    ELSE
      IF llRpShowSz
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + Style.Desc1]
      ENDIF
    ENDIF
    
  CASE lcRpSortBy = 'I'
    llSumrze=.F.
    IF !llDetOnly
      lcRpHedTlt = 'INVOICE'
      lcRpIndTag = [Invoice + Style]
      lcRpGroup1   = [Invoice]
      lcRpGrpHd1   = ['Invoice No.' + ' : ' + Invoice  ]
      lcRpGrpFt1   = ['*   ' + Invoice + ' Totals ==>']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF  
    =lfRepHdDt()
        
  CASE lcRpSortBy = 'H'
    llSumrze=.F.
    IF !llDetOnly
      lcRpHedTlt = 'SHIPDATE'
      lcRpIndTag = [DTOS(InvHdr.ShipDate)+Invoice+Style]
      lcRpGroup1   = [DTOS(InvHdr.ShipDate)]
      lcRpGrpHd1   = ['SHIPDATE' + ' : ' + DTOC(InvHdr.ShipDate)]
      lcRpGrpFt1   = ['*   ' + DTOC(InvHdr.ShipDate) + ' Totals ==>']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF 
    =lfRepHdDt()
       
  CASE lcRpSortBy = 'R'
    llSumrze=.F. 
    lcRpHedTlt = 'SALESREP'
    IF !llDetOnly
      lcRpIndTag = [InvHdr.REP1 + Invoice + Style]
      lcRpGroup1   = [InvHdr.REP1]
      lcRpGrpHd1   = ['SALESREP' + ' : ' +InvHdr.REP1  ]
      lcRpGrpFt1   = ['*   ' + InvHdr.REP1 + ' Totals ==>']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF 
    =lfRepHdDt()
   *C101798,1 (End) 
ENDCASE
*-- end of lfvSortBy.


*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : MAB
*! Date      : 09/07/1997
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

*IF The user want to Browse or if the Style he entered is not in the file
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
*! Name      : lfStDepart
*! Developer : Nader Anis (NAD)
*! Date      : 05/09/2000
*! Purpose   : Go top in the icdepthd file when browse.
*! Ref       : C101798,1
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfStDepart()
*!**************************************************************************
*
FUNCTION lfStDepart

PARAMETERS OpGrdParm
SELECT ICDEPTHD   
DO CASE
  CASE  OpGrdParm='S'           
    lcTmpIndex =gfTempName()
    SELECT  DISTINCT DEPT,cDeptDesc FROM ICDEPTHD INTO DBF (gcWorkDir + lcTmpIndex)
     USE IN ICDEPTHD
    use in (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS ICDEPTHD
    SELECT ICDEPTHD
    INDEX ON DEPT TAG Depthd
    GO TOP
  CASE  OpGrdParm='R'
    USE IN ICDEPTHD
    =gfOpenFile(gcDataDir+'ICDEPTHD',gcDataDir+'DEPTHD ','SH')
ENDCASE
*-- End of lfStDepart.
*!**************************************************************************
*! Name      : lfClrRead
*! Developer : Nader Anis (NAD)
*! Date      : 05/09/2000
*! Purpose   : Clear the read to suppress the summerize multi store orders
*! Ref       : C101798,1
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : lfClrRead()
*!**************************************************************************
FUNCTION lfClrRead
CLEAR READ

*-- End of function  lfClrRead

*!****************************************************************************************
*! Name      : lfRepHdDt
*! Developer : Nader Anis (NAD)
*! Date      : 05/09/2000
*! Purpose   : Print the header and line if sort was by Invoivce or shipdate or sales rep
*! Ref       : C101798,1
*!****************************************************************************************
*! Calls     : 
*!****************************************************************************************
*! Passed Parameters  : None
*!****************************************************************************************
*! Returns            : None
*!****************************************************************************************
*! Example   : lfRepHdDt()
*!****************************************************************************************
FUNCTION lfRepHdDt
IF EMPTY(lcFree_Clr)
  IF llRpShowSz
    lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)
    lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style]
  ELSE
    lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' Description'
    *Use Style Long Description Where Possible
    lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ Style + ' ' + Style.Desc1]
  ENDIF  
ELSE
  IF llRpShowSz
    lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)      
    *Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print. 
    lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]               
  ELSE
    lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' Description'
    *Use Style Long Description Where Possible
    lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + Style.Desc1]
  ENDIF  
ENDIF      

*!****************************************************************************************
*! Name      : lfSRInv
*! Developer : Nader Anis (NAD)
*! Date      : 05/09/2000
*! Purpose   : change the order when browsing the Styles then restore it again
*! Ref       : C101798,1
*!****************************************************************************************
*! Calls     : 
*!****************************************************************************************
*! Passed Parameters  : None
*!****************************************************************************************
*! Returns            : None
*!****************************************************************************************
*! Example   : lfSRInv()
*!****************************************************************************************
FUNCTION lfSRInv
PARAMETERS lcParm
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Style

DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO TAG cStyle
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SET ORDER TO TAG style    && restore old Index
    SELECT (lnAlias)
ENDCASE
*-- End of lfSRInv.
*!**************************************************************************
*! Name      : lfAccount
*! Developer : Nader Anis (NAD)
*! Date      : 05/09/2000
*! Purpose   : Go top in the icdepthd file when browse.
*! Ref       : C101798,1
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAccount()
*!**************************************************************************
*
FUNCTION lfAccount
PARAMETERS OpGrdParm
SELECT CUSTOMER
GO TOP

