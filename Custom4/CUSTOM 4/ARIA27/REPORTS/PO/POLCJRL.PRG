*:***************************************************************************
*: Program file  : Polcjrl   (E301431)
*: Program desc. : Letter of Credit Journal
*: For Report    : PolcjrlA.FRX , PolcjrlB.FRX
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : AHMED MOHAMED EL_ANWAR (AME)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwRepWhen,lfFormName,
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO Polcjrl
*:***************************************************************************
*: This Program is due to E301431 ...
*:***************************************************************************
*

*SSE 
*we have to add a record in SYREPUVR for Temp File Name (lcTmpLcJ)
*lcTmpLcJ = gfTempName()

SELECT LCJOURNL
SELECT lcJournl.*, LC.Vendor ,LC.Bank,LC.Created,LC.Expire,;
                   LC.Us_amt,LC.Branch,LC.cLcBank, LC.CCurrCode ;
       FROM LcJournl , Lc ;
       WHERE LcJournl.cLcNo = LC.cLcNo ;
             AND &lcRpExp ;
       INTO DBF (gcWorkDir) + (lcTmpLcJ)

SELECT (lcTmpLcJ)
DO CASE
  CASE lcRpSort $ 'BV'
    IF lcRpSort = 'V'
      INDEX ON Vendor+cLCno TAG (lcTmpLcJ)
    ELSE
      INDEX ON Bank+cLCno TAG (lcTmpLcJ)
    ENDIF
  OTHER   && lcRpSort = 'L'
    INDEX ON cLCno TAG (lcTmpLcJ)
ENDCASE

SELECT (lcTmpLcJ)
SET RELATION TO "N"+Bank+"NBANK" INTO CODES        ADDITIVE
SET RELATION TO Vendor   INTO APVENDOR ADDITIVE
GO TOP

DO gfDispRe WITH EVAL('lcRpName') 


*!****************************************************************
*! Name : lfTType()
*! Developer : Ahmed Mohamed_EL_Anwar Abd_El_Fattah
*! Date : 09/07/2000
*!****************************************************************
*! Purpose : Return tran type as descreption not number.
*!****************************************************************         
FUNCTION lfTType

xTrnTyp=""
DO CASE
  CASE clctrntyp ='1'
    xTrnTyp = "ADDED     "
  CASE clctrntyp ='2'  
    xTrnTyp = "DELETED   "
  CASE clctrntyp ='3'
    xTrnTyp = "MODIFIED  "
  CASE clctrntyp ='4'
    xTrnTyp = "CANCELLED "
  CASE clctrntyp ='5'
    xTrnTyp = "PAID      "
  CASE clctrntyp ='6'
    xTrnTyp = "AMENDMENT "
ENDCASE
RETURN (xTrnTyp)

*!***************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 25/07/2000
*! Purpose   : Option Grid When function
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example     : = lfwRepWhen()
*!***************************************************************************
*
FUNCTION lfWRepWhen
lnCurrPost = lfItmPos('LC.CCURRCODE')  && Currency Position 
laOGFxFlt[lnCurrPost,6] = gcBaseCurr   && Assign Base Currency to Currency
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!***********************************************************************
*! Name      : lfStrtBal
*! Developer : AHMED MOHAMED EL_ANWAR (AME)
*! Date      : 12/07/2000
*! Purpose   : Get the total Starting balane.
*!***********************************************************************
*
FUNCTION lfStrtBal

PRIVATE lnSBalAmnt

lnAls=SELECT()
lnSvRec=RECNO()
lnSBalAmnt=0.00
SELECT LcJournl
SEEK &lcTmpLcJ..cLcNo
LOCATE REST WHILE LcJournl.cLcNo=&lcTmpLcJ..cLcNo ;
            FOR BETWEEN(LCJournl.dTranDate,ldLCAL,ldLCAH)
SKIP - 1
IF ( LcJournl.cLcNo = &lcTmpLcJ..cLcNo ) .AND. !BOF() 
  lnSBalAmnt=LcJournl.nLCBal
ENDIF
SELECT(lnAls)
GOTO lnSvRec
RETURN lnSBalAmnt



*!****************************************************************
*! Name      : lfTotBal()
*! Developer : AHMED MOHAMED EL_ANWAR (AME)
*! Date      : 17/07/2000
*! Purpose   : Get the total balane for all (Grand total).
*!****************************************************************
*
FUNCTION lfTotBal
PRIVATE lnTotAmnt

lnAls=SELECT()
lnSvRec=RECNO()
SELECT (lcTmpLcJ)
GO TOP
lnTotAmnt = 0
lcLCnumber=cLCno
SCAN
  IF lcLCnumber <> cLCno
    lnTotAmnt = lnTotAmnt + lnBalance
    lcLCnumber= cLCno
  ENDIF
  lnBalance=nLCBal
ENDSCAN
lnTotAmnt = lnTotAmnt + lnBalance
SELECT(lnAls)
GOTO lnSvRec
RETURN lnTotAmnt


*!****************************************************************
*! Name      : lfGetBal()
*! Developer : AHMED MOHAMED EL_ANWAR (AME)
*! Date      : 17/07/2000
*! Purpose   : Get the total balane for (Vendor and bank).
*!****************************************************************        
*
FUNCTION lfGetBal
PRIVATE lnTotAmnt

lnAls=SELECT()
lnSvRec=RECNO()
SELECT (lcTmpLcJ)
lnTotAmnt = 0
lcKey = IIF(lcRpSort='B',Bank,Vendor)
lcCondit = IIF(lcRpSort='B','Bank=lcKey','Vendor=lcKey')
SEEK lcKey
lcLCnumber = lcKey + cLCno
SCAN WHILE &lcCondit
  IF lcLCnumber <> lcKey + cLCno
    lnTotAmnt = lnTotAmnt + lnBalance
    lcLCnumber = lcKey + cLCno
  ENDIF
  lnBalance=nLCBal
ENDSCAN
lnTotAmnt = lnTotAmnt + lnBalance
SELECT(lnAls)
GOTO lnSvRec
RETURN lnTotAmnt


*!*************************************************************
*! Name      : lfvVend  
*! Developer : Ahmed Mohamed_El_anwar Abd_El_Fattah
*! Date      : 18/07/2000
*! Purpose   : Validation function for the Vendor field
*!*************************************************************
*! Called from : Vendor field [Option Grid]
*!*************************************************************
*
FUNCTION lfvVend
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
*--IF Statment to check if we are going to Browse
SELECT ApVendor
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK (lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "cVendCode :R :H= 'Vendor' ,"+  ;
               "cVenComp :R :H= 'Name' ,"+     ;
               "cPhoneNo :R :H= 'Phone' ,"+    ;
               "cVenOurAc :R :H= 'Our Account' "
  lcFile_Ttl = "Vendor.."
  = gfBrows('','CVendCode','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ''
  ENDIF
ENDIF    && End of IF !EMPTY(lcObj) AND ...etc.
&lcVar = lcObj      && Update the field
SELECT (lcAlias)

*!****************************************************************
*! Name      : lfvSortBy
*! Developer : Ahmed Mohamed_EL_Anwar Abd_El_Fattah
*! Date      : 19/07/2000
*! Purpose   : Adjust The .Frx Name. 
*!****************************************************************         
*
FUNCTION lfvSortBy
IF lcRpSort = "L"
  lcRpName = "POLCJRLA"
ELSE
  lcRpName = "POLCJRLB"
ENDIF
= lfRepPltFr(lcRpName)
*-- End of lfvSortBy.

*!*************************************************************
*! Name      : lfwOldCurr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/19/1999
*! Purpose   : When function to get the Old Currency Value
*!*************************************************************
*! Example   : = lfwOldCurr()
*!*************************************************************
*
FUNCTION lfwOldCurr
lcOldCurr = EVALUATE(SYS(18))      && Varible to hold the old value
*-- End of lfwOldCurr.

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*
FUNCTION lfvCurCode
lnAlias=SELECT(0)

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*-- Allow blank curreny in case user have not selected foreign currency.
IF EMPTY(lcObj)
  *SSE Default Currency to Base in case of empty Currency 
  &lcVar = gcBaseCurr

  RETURN
ENDIF

IF !SEEK(lcObj,'SYCCURR') .OR. ATC("?",lcObj) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  IF EMPTY(laTemp[1])
    lcObj = lcOldCurr
  ELSE
    lcObj = laTemp[1]
  ENDIF
ENDIF

&lcVar = lcObj      && Update the field

SELECT(lnAlias)
*-- End of lfvCurCode.