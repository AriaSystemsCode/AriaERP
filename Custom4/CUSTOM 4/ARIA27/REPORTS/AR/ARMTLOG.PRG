*:***************************************************************************
*: Program file  : ARMTLOG
*: Program desc. : Material Shipping Log Report
*: For Report    : ARMTLOGN.FRX
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : N000392
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfBrows,gfStyBrw,CusBrowM.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,lfWorkFile,lfvShowSz
*:               : lfCollData,lfCollTime,lfvSortBy,lfEvalVars,lfvFabric
*:               : lfStitle,lfvAcc,lfvLoc,gfBrowWare,gfOpenFile,gfItemMask
*:***************************************************************************
*: Notes : This report is an exact copy of standard Shipping Log Report.
*:***************************************************************************
*: Example : DO ARMTLOG
*:***************************************************************************
*:E126684,1 TMI 03/03/2005 Remove zeros in the right of the decimal point.
*:***************************************************************************

PRIVATE lcExactCas
lcStTime   = TIME()    && Time in which we start collect data.
IF TYPE("llTrigFG")="U"
  llTrigFG=.F.
ENDIF
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
  IF llTrigFG
    lcRpExp    = STRTRAN(lcRpExp,'MInvHdr.Status <> "V" AND','')
  ENDIF
  lcRpExp    = STRTRAN(lcRpExp,'MInvLine.','')
  lcRpExp    = [cMatInv+STR(lineno,6) = '' AND ] + lcRpExp
  SELECT MInvLine
  SET ORDER TO MInvLine
  SET RELATION TO cMatInv INTO MInvHdr, Fabric+Color INTO Fabric,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer
  *-- Scan to fill Temp. File with filter data.
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    IF MInvHdr.Status="V" 
      STORE 0 TO m.FabQty,m.nSellNetpr
    ENDIF
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDSCAN  && end Scan to fill Temp. File with filter data.
  SELECT MInvLine
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
SET RELATION TO cMatInv INTO MInvHdr, Fabric+Color INTO Fabric,;
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
*-- End of Report.

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
*-- End of lfStitle.

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
  SET ORDER TO MInvHdr IN MInvHdr
  SET ORDER TO MInvLine IN MInvLine
  SET ORDER TO Fabric IN Fabric

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MINVLINE.INVDATE'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MINVHDR.CCURRCODE'),1)
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF

  IF llMultLoc
    SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  ENDIF  

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT MInvLine
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]

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
*-- End of lfWorkFile.

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
*-- End of lfwOldVal.

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
*-- End of lfvAcc.

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
*-- End of lfvLoc.

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
*-- End of lfvStates.

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
*-- End of lfCollTime.

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
*-- End of lfClearRep.

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

IF !('MA' $ gcCmpModules)
  *-- Message : 42083
  *-- Material module is not installed, Cannot proceed.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM42083B00000','ALERT','Material')
  llOgTrmnat = .T.
  RETURN
ENDIF

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.

*-- Evaluate sort by arrays. [Begin]
DIMENSION laSortDesc[3,1],laSortVal[3,1]
laSortDesc[1] = 'Account'
laSortDesc[2] = 'Fabric'
laSortDesc[3] = lcSTitle       && State variable Title

laSortVal[1] = 'A'
laSortVal[2] = 'S'     && Leave the old letter as it is (S for STYLE)
laSortVal[3] = 'T'
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

*-- Fill default sort options... [Begin]
lcRpSortBy = 'A'
=lfvSortBy()
*-- Fill default sort options... [End]
*-- End of lfSortDumy.

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
DO CASE
  CASE lcRpSortBy = 'S'		&& Sort by Style Case
    lcRpHedTlt = 'Fabric'
    lcRpIndTag = [FABRIC+COLOR+ACCOUNT + CMATINV]
    lcRpGroup1   = [FABRIC]
    lcRpGroup2   = [COLOR]
    lcRpGrpHd1   = [PADR('Fabric',14) + ' : ' + PADR(Fabric,19)]
    lcRpGrpHd2   = [PADR('Color',15) + ': ' + PADR(COLOR,19) + ' - ' + Fabric.Desc]
    lcRpGrpFt1   = ['*   ' + PADR(Fabric,19) + ' Totals ==>']
    lcRpGrpFt2   = ['**  ' + PADR(Color,19) + ' Totals ==>']
    STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
    lcRpRepHed = 'Acct#'+' '+'Name'+SPACE(21)+'Store'+SPACE(4)+PADR(ALLTRIM(lcSTitle),9)+SPACE(1)+PADR('Color',6)
    lcRpDetLin  = [Account + ' ' + PADR(Customer.btname,24) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(Customer.cAddress4),9) + ' ' +COLOR]
  CASE lcRpSortBy = 'A'		&& Sort by Account Case
    lcRpIndTag = [ACCOUNT + STORE + CMATINV]
    lcRpGroup1   = [ACCOUNT]
    lcRpGrpHd1   = ['Account' + ' : ' + Account + ' - ' + Customer.BtName]
    lcRpGrpFt1   = ['*   ' + Account + ' Totals ==>']
    STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3

    lcRpRepHed = 'Store'+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR('Fabric',12) + ' ' + PADR('Color',6) + ' Description'
    lcRpDetLin = [Store+ ' '   +PADR(ALLTRIM(Customer.cAddress4),6) +' '+ PADR(Fabric,12) + ' ' + Color + ' ' + Fabric.Desc]

  CASE lcRpSortBy = 'T'		&& Sort by State Case
    lcRpHedTlt = ALLTRIM(lcSTitle)
    lcRpIndTag = [Customer.cAddress4 + ACCOUNT + CMATINV]
    lcRpGroup1   = [LEFT(cTempKey,30)]
    lcRpGrpHd1   = [lcSTitle + ' : ' + ALLTRIM(Customer.cAddress4)]
    lcRpGrpFt1   = ['*   ' + PADR(ALLTRIM(Customer.cAddress4),20) + ' Totals ==>']
    STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    
    lcRpRepHed = 'Acct# '        +'Name'+ SPACE(11) +'Store'+'    '+ PADR('Fabric',7) + ' ' + PADR('Color',6) + ' Description'
    lcRpDetLin = [Account + ' ' + PADR(Customer.BtName,14) +  ' ' + Store +' ' + PADR(Fabric,7) + ' ' + Color + ' ' + PADR(Fabric.Desc,11)]
ENDCASE
*-- End of lfvSortBy.

*!***************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/14/2002
*! Purpose   : Vaildate the fabric code.
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example     : = lfvFabric()
*!***************************************************************************
*
FUNCTION lfvFabric
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))

SELECT FABRIC
lcOldOrd = ORDER()
SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcObjVal) .AND. ('?' $ lcObjVal .OR. !SEEK(lcObjVal,'FABRIC'))
  =FABROW (@lcObjVal ,'*')
ENDIF 
&lcObjNam    = lcObjVal 
IF lcObjNam  = "LCOGVALUEF" 
  lcRpLowFab = lcObjVal 
ELSE
  lcRpHigFab = lcObjVal 
ENDIF

SELECT FABRIC
SET ORDER TO &lcOldOrd
*-- End of lfvFabric.

*:**************************************************************************
*:* Name        : lfRmZr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/03/2005
*:* Purpose     : Remove zeros in the right of the decimal point
*:***************************************************************************
*:* Called from : ARMATIA.FRX
*:***************************************************************************
FUNCTION lfRmZr
PARAMETERS lnValue
PRIVATE lcValue,lnI
lcValue = ALLTRIM(STR(lnValue,15,5))+' '
FOR lnI = 1 TO 5
  lcValue = STRTRAN(lcValue,'0 ','  ')
ENDFOR
lcValue = LEFT(lcValue,LEN(lcValue)-1)
lcValue = IIF(VAL(lcValue)=0,'',PADL(lcValue,9))
RETURN  lcValue
*-- end of lfRmZr.