*:***************************************************************************
*: Program file  : SRSLSREP
*: Program desc. : Sales Representative Master List
*: System        : Aria Apparel System (Aria4XP).
*: Module        : Sales Representative (SR )
*: Developer     : Hassan Ibrahim Ali (HIA)
*: Task          :
*:***************************************************************************
*: Calls :
*:    Functions  : lfwRepWhen,lfwOldVal,lfvRepCode,lfvDate
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO SRSLSREP
*:***************************************************************************
*:Modifications:
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include R:\aria4xp\Reports\SR\srslsrep.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*-- Some Variables Needed by frx
lcTime = TIME()

IF !EMPTY(lcTempRep) AND !USED(lcTempRep)
  CREATE CURSOR (lcTempRep) ;
         FROM ARRAY laFileStru
ENDIF
  DECLARE laSalesAdd[5,1]
*-- Sorting Cases

STORE '' TO lcRepPhone , lcRepFax
lcPhonPict = gfPhoneTem()

DO CASE
  CASE lcRpSort = 'C'
    lcIndexTag = 'REPCODE'
    *N000682,1 MMT 02/11/2013 Globalization changes[Start]
    *lcSortTtl='Rep. Code'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl=LANG_REPOCODE
lcSortTtl=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPOCODE,oAriaApplication.GetHeaderText("LANG_REPOCODE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 02/11/2013 Globalization changes[END]
  CASE lcRpSort = 'R'
    lcIndexTag = 'REGION'
    *N000682,1 MMT 02/11/2013 Globalization changes[Start]
    *lcSortTtl='Region'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl=LANG_REGION
lcSortTtl=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REGION,oAriaApplication.GetHeaderText("LANG_REGION",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 02/11/2013 Globalization changes[END]
  CASE lcRpSort = 'S'
    lcIndexTag = 'CADDRESS4'
    *N000682,1 MMT 02/11/2013 Globalization changes[Start]
    *lcSortTtl = 'State'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl = LANG_STATE
lcSortTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATE,oAriaApplication.GetHeaderText("LANG_STATE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 02/11/2013 Globalization changes[END]
  CASE lcRpSort = 'Z'
    lcIndexTag = 'CADDRESS5'
    *N000682,1 MMT 02/11/2013 Globalization changes[Start]
    *lcSortTtl='Zip'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl=LANG_ZIP
lcSortTtl=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ZIP,oAriaApplication.GetHeaderText("LANG_ZIP",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 02/11/2013 Globalization changes[END]
ENDCASE

*-- Activate the index According to user selection
IF !(lcIndexTag == lcOldIndex)
  SELECT (lcTempRep)
  lcOldIndex = lcIndexTag
  INDEX ON &lcIndexTag TAG &lcTempRep
ENDIF


IF llOGFltCh

  IF RECCOUNT(lcTempRep) > 0
    SELECT (lcTempRep)
    ZAP
  ENDIF
  lcLastExpr = lcRpExp
  SELECT SALESREP
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempRep) FROM MEMVAR
  ENDSCAN

ENDIF
*-- When the user wants to print sales rep notepad==> Set an order
IF llRpNote
  SELECT NOTEPAD
  SET ORDER TO NOTEPAD
ENDIF
SELECT (lcTempRep)
*-- Run the FRX
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcFormName')

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*-- Just to get the structure of the sales rep file
IF EMPTY(lcTempRep)
  DECLARE laSalesAdd[5,1], laFileStru[1,4]

  STORE '' TO lcOldIndex , lcIndexTag , lcLastExpr
  lcTempRep = gfTempName()
  **-- Get company Address [begin].

  SELECT SALESREP
  = AFIELDS(laFileStru)
ENDIF
*-- END OF lfwRepWhen.

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Evaluates the oject's old value incase of 'ESC'
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************

FUNCTION lfOldVal
*E302403, HIA [BEGIN]
*laOldVal = EVALUATE(SYS(18))
laOldVal = EVALUATE(SYS18())
*E302403, HIA [END]

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfHeader
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Evaluates the oject's old value incase of 'ESC'
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeader()
*!*************************************************************

FUNCTION lfHeader

laSalesAdd[1] = gfGetAdr(lcTempRep , '' , '' , '' , 1) && , '2')
laSalesAdd[2] = gfGetAdr(lcTempRep , '' , '' , '' , 2)
laSalesAdd[3] = gfGetAdr(lcTempRep , '' , '' , '' , 3)
laSalesAdd[4] = gfGetAdr(lcTempRep , '' , '' , '' , 4)
laSalesAdd[5] = gfGetAdr(lcTempRep , '' , '' , '' , 5)

*laSalesAdd[6] = gfGetAdr(lcTempRep , '' , '' , '' , 6)
*WAIT WINDOW &lcTempRep..caddress1
*WAIT WINDOW laSalesAdd[1]

lcRepPhone = TRANSFORM(&lcTempRep..Phone,'@R '+lcPhonPict)
lcRepFax   = TRANSFORM(&lcTempRep..FAX,'@R '+lcPhonPict)

=lfAdrShift('laSalesAdd')
RETURN '' && Print nothing
*--

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Evaluates the Optional Title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfOptMsg
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************

FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'  && 1st. line Variable
laOptMsg[1,2] = 65          && Line length
= gfOptMsg('laOptMsg')      && Call Function to write optional message.
*---------------------------------------------------------------------------

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
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
*-- Delete temporary SALES REP. file.
IF USED(lcTempRep)
 USE IN (lcTempRep)
ENDIF
*-- end of lfClearRep.
*--------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************

FUNCTION lfStitle

SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath   +'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(oAriaApplication.ActiveCompanyID  ,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB

RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************


FUNCTION lfSortDumy

DIMENSION laSortDesc[4,1] , laSortVal[4,1]
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	laSortDesc[1] = 'Sales Representative'
*!*	laSortDesc[2] = 'Region'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1] = LANG_SALESREP
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SALESREP,oAriaApplication.GetHeaderText("LANG_SALESREP",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2] = LANG_REGION
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REGION,oAriaApplication.GetHeaderText("LANG_REGION",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[END]
laSortDesc[3] = lcSTitle
laSortDesc[4] = lcZipTitle

laSortVal[1] = 'C'
laSortVal[2] = 'R'
laSortVal[3] = 'S'
laSortVal[4] = 'Z'

*--The End

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
*--HDM =>FOR lnCount = 1 TO 5
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
