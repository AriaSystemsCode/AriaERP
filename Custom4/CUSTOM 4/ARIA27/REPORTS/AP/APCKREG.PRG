*B600301,1 MAN 05/01/95
*B600301,1 Changing the Bank-Chk Acct group total in both FRXs APCKREGD,APCKREGS 
*B600301,1 from summation on APPAYMNT.NPAYAMNT to summation on -APDIST.NAPDAMNT 
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*B601087,1 M.H 06/03/96 Add the sort group to the report in the file 'SYREPUVR.DBF'
*B800720,1 HS 11/27/96 Made some change in the function [lfvChecks] and add
*B800720,1             some functions [lfwOpGrid , lfwBnCod , lfvBnCod , 
*E300793,1 HS 07/06/98  Add a new option in the grid [Include void payments]
*E300793,1              Note : I made some changes in APCKREGD.FRX,
*E300793,1                     APCKREGS.FRX and in the Report generator.
*B602899,1 MAN_NY 05/10/99. If we have more than one payment done for different
*B602899,1 					vendors, having the same payment number, their details
*B602899,1 					are summed under each payment in the report.
*B603679,1 KHM 07/10/2000  Adding lfAmntDisp function to be used in both
*B603679,1                 FRX instead of using gfAmntDisp.
*B803359,1 RAMY 07/12/2000 Add valid function to the transaction No. field
*B803603,1 SSE 08/21/2000 Fix bug of not having the option (Include void payments)
*B803603,1                all modification is in Syrepuvr by adding the 2 removed var lines 
*B803603,1                Also make Payment Method in OG works 
*B604005,1 MHM 11/02/00 Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM          we modify fields (amount and total in both frx{summary and details})
*B604005,1 MHM          in summary we change width of apcompany from 30 to 28 and increase amount and discount
*B604452,1 AAN 05/08/2001 Adding a voiding date to the report. This bug done only in the FRX report
*C102777,1 ALB 12/30/2002 Generate text file hold checks information for customer
*********************************************************************************************

SELECT APPAYMNT
IF SUBSTR(UPPER(lcrpgrp),1,17) = 'APPAYMNT.CBNKCODE'
  SET ORDER TO TAG CHKTMNO
ELSE
  SET ORDER TO TAG TYPCLNO
ENDIF
*lcRpGrp = UPPER([appaymnt.cbnkcode+appaymnt.cchkacct+apchecks.cchkglacc])

*E300793,1 Remove the IF condition to establish the relation between APDIST
* and APINVHDR in the case of Summary format too [Begin]

*IF lcRpForm=="APCKREGD"

*E300793,1 Remove the IF condition to establish the relation between [End]


SET ORDER TO TAG BANKCHECK IN APCHECKS
SET ORDER TO TAG PAYMNTS   IN APDIST
SET ORDER TO TAG INVVEND   IN APINVHDR
SET RELATION TO appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno INTO APDIST ADDITIVE
SET RELATION TO appaymnt.cbnkcode+ appaymnt.cchkacct INTO APCHECKS ADDITIVE
SELECT APDIST
SET RELATION TO apdist.cinvno+ apdist.cvendcode INTO APINVHDR ADDITIVE
SELECT APPAYMNT

*E300793,1 Remove these lines for we have removed the IF condition to
*establish the relation between APDIST and APINVHDR in the case of
*Summary format too [Begin]

*ELSE
*  SET ORDER TO TAG BANKCHECK IN APCHECKS
*  SET ORDER TO TAG PAYMNTS   IN APDIST
*  SET RELATION TO appaymnt.cbnkcode+ appaymnt.cchkacct INTO APCHECKS ADDITIVE
*  SET RELATION TO appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno INTO APDIST ADDITIVE
*ENDIF

*E300793,1 Remove these lines for we have removed the IF condition [End]

SET SKIP TO APDIST
*B602899,1 Add an extra filtering condition on the vendor code
IF !EMPTY(lcRpExp)
  lcRpExp = lcRpExp + ".AND. "
ENDIF  
lcRpExp = lcRpExp + "APDIST.cVendCode=APPAYMNT.cPayClNo"
*B602899,1 end

*B803359,1 RAMY [start]
IF !EMPTY(lcTrnNo)
  lcRpExp = lcRpExp + " AND APDIST.cApdRef=lcTrnNo"
ENDIF
*B803359,1 RAMY [end]

*B803603,1 Add Payment Method in OG to Filter Expression (lcRpExp) [Begin] 
IF lcMethod <> "A"
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ])
  DO CASE
    CASE lcMethod = 'M'
      lcRpExp = lcRpExp + [ cPayMeth = 'M' ]
    CASE lcMethod = 'N'
      lcRpExp = lcRpExp + [ cPayMeth = 'N' ]
    CASE lcMethod = 'P'
      lcRpExp = lcRpExp + [ cPayMeth = 'P' ]
  ENDCASE
ENDIF    
*B803603,1 Add Payment Method in OG to Filter Expression (lcRpExp) [End]
*C102777,1 ALB Generate text file hold checks information for customer [Begin]
=gfDoTriger('APCKREG',PADR('GENTXT',10))
*C102777,1 ALB Generate text file hold checks information for customer [end]
DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp
SET RELATION TO
SELECT APDIST
SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks

*B800720,1 This lines was add by HS [Begin]
*B800720,1 IF Statment to check if the Checking Account is empty
IF EMPTY(LAOGVRFLT(lnChCodEl,6))
   RETURN
ENDIF      &&End of IF
*B800720,1 This lines was add by HS [End]
    
SELECT APCHECKS
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK

*B800720,1 This lines was add by HS [Begin]
lcBnCod = ' '     &&Varible to hold the Bank Code
*B800720,1 IF Statment to check if there is a Bank Code GET FIELD
IF lnBnCodEl <> 0
   lcBnCod = PADR(LAOGVRFLT(lnBnCodEl,6) , 8 , ' ')    &&Varible to hold the Bank Code
ENDIF      &&End of IF
*B800720,1 This lines was add by HS [End]

*B800720,1 There is no need for this lines now [Begin]
*B800720,1 Use LAOGVRFLT(lnChCodEl,6) instead of 'lcFldNam' 
*lcFldNam = SYS(18)
*IF SEEK(EVAL(SYS(18)))
*  &lcFldNam=APCHECKS.CCHKACCT
*B800720,1 There is no need for this lines now [End]

IF !SEEK(lcBnCod + LAOGVRFLT(lnChCodEl,6))

*B800720,1 There was no need for this lines [Begin]
*ELSE
* IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
*B800720,1 There was no need for this lines [End]

    DIMENSION laTemp[2]   && Define an array get Vendors codes and company.
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CBNKCODE :H= 'Bank Code',;
                CCHKACCT :H= 'Check Code'"
    lcFile_Ttl= "Checks"

*B800709,1 Change this line [Begin]
*   =gfBrows(.F.,'CBNKCODE,CCHKACCT','laTemp')
    =gfBrows(IIF(EMPTY(lcBnCod),.F.,[FOR CBNKCODE = lcBnCod]),'CBNKCODE,CCHKACCT','laTemp')
*B800709,1 Change this line [End]

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    IF !EMPTY(laTemp[2])
      
      *B800720,1 Change this line [Bigen]
      *&lcFldNam=laTemp[2]
      LAOGVRFLT(lnChCodEl,6) = laTemp[2]
      *B800720,1 Change this line [End]
      
      *B800720,1 This lines was add by HS [Begin]
      *B800720,1 IF Statment to check if there is a Bank Code GET FIELD
      IF lnBnCodEl <> 0
         LAOGVRFLT(lnBnCodEl,6) = laTemp[1]
         SHOW GETS       
      ENDIF      &&End of IF
     *B800720,1 This lines was add by HS [End]

    ELSE
      
      *B800720,1 Change this line [Bigen]
      *&lcFldNam=SPACE(24)
      LAOGVRFLT(lnChCodEl,6) = lcOldChAcc
      *B800720,1 Change this line [End]
      
    ENDIF

*B800720,1 There was no need for the IF Statment
* ENDIF

ENDIF
SET ORDER TO &lcSavOrder
SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
laOGObjCnt[3] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 End

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)


*!*************************************************************
*! Name      : lfwOpGrid
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to save the array element number
*!             of the Bank code and Checking account code  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800720,1 This function was added by HS
*!*************************************************************

FUNCTION lfwOpGrid

lnBnCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Bank Code  
lnChCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Checking Account 
*FOR Loop to find the LAOGVRFLT element number of the Bank Code and Checking Account
FOR lnCount = 1 TO ALEN(LAOGVRFLT , 1)
    *IF Statment to check if the curent element of the LAOGVRFLT
    *array is the Bank Code element
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CBNKCODE'
       lnBnCodEl = lnCount
    ENDIF    &&End of IF
    *IF Statment to check if the curent element of the LAOGVRFLT
    *array is the Checking Account element
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CCHKACCT'
       lnChCodEl = lnCount
    ENDIF    &&End of IF
ENDFOR   &&End of the FOR Loop     

*C102777,1 ALB Generate text file hold checks information for customer [Begin]
=gfDoTriger('APCKREG',PADR('WCUSTOG',10))
*C102777,1 ALB Generate text file hold checks information for customer [end]


*!*************************************************************
*! Name      : lfwBnCod
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to save the old Bank code  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800720,1 This function was added by HS
*!*************************************************************


FUNCTION lfwBnCod

lcOldBnCod = LAOGVRFLT(lnBnCodEl,6)    &&Varible to hold the old Bank Code

*!*************************************************************
*! Name      : lfvBnCod
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to valid the Bank code  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800720,1 This function was added by HS
*!*************************************************************


FUNCTION lfvBnCod

*IF Statment to check if the Bank Code was changed
IF lcOldBnCod <> LAOGVRFLT(lnBnCodEl,6)
   *IF Statment to check if the new Bank Code is empty
   IF EMPTY(LAOGVRFLT(lnBnCodEl,6)) .AND. lnChCodEl <> 0
      LAOGVRFLT(lnChCodEl,6) = SPACE(12)   &&Clear the Checking Account
      SHOW GETS 
      RETURN
   ENDIF    &&End of IF
   lcSavAlias = ALIAS()     && Save old alias
   SELECT APCHECKS
   lcSavOrder = SET('ORDER')   && Save old order
   lcSavExact = SET('EXACT')   && Save old exact status
   SET ORDER TO TAG BANKCHECK
   SET EXACT OFF
   *IF Statment to check if the new Bank Code exist in the file
   IF SEEK(LAOGVRFLT(lnBnCodEl,6)) .AND. lnChCodEl <> 0 
      LAOGVRFLT(lnChCodEl,6) = APCHECKS.CCHKACCT
      SHOW GETS
   ENDIF     &&End of IF
   SET ORDER TO &lcSavOrder
   SET EXACT &lcSavExact
   SELECT &lcSavAlias
ENDIF   &&End of IF
         
      


*!*************************************************************
*! Name      : lfwChecks
*! Developer : Haytham El_Sheltawi
*! Date      : 11/28/1996
*! Purpose   : This function is to save the old Checking Account  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800720,1 This function was added by HS
*!*************************************************************


FUNCTION lfwChecks

lcOldChAcc = LAOGVRFLT(lnChCodEl,6)     &&Varible to hold the old Checking Account

*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 07/10/2000
*! Purpose   : To display the payment amount according to the
*!             currency selected.
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! B603679,1 KHM 07/10/2000 
*!*************************************************************
FUNCTION lfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    *B603679,1 KHM 07/10/2000 (Begin) Check if the payment currency is
    *B603679,1                the same as the base currency then display 
    *B603679,1                the amount as it is.
    IF CCURRCODE = gcBaseCurr  
      SELECT (lnSavAlias)
      RETURN lnAmount  
    ENDIF
    *B603679,1 KHM 07/10/2000 (End)

    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
        
    
    lnExRate = 0

    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF         
  
    *B603679,1 KHM 07/10/2000 (Begin) Get the rate from the ApDist file
    *B603679,1                in case of payment currency is different 
    *B603679,1                than the base currency.
    IF lcCurrCode = APDIST.CCURRCODE 
   	  lnExRate  = APDIST.nExRate
    ENDIF
    *B603679,1 KHM 07/10/2000 (End)
    
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)    
    
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF
      
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'U'

    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount

*!*************************************************************
*! Name      : lfvTrnNo
*! Developer : Ramy Mabrouk
*! Date      : 07/16/2000
*! Purpose   : Valid function for the transaction No. field
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : gfBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
* This function was added by RAMY due to B803359,1
*!*************************************************************
FUNCTION lfvTrnNo

PRIVATE lnTrnType

PRIVATE  lcPriorDbf , lcPriorCdx  , lcPriorFlt
lcPriorDbf  = SELECT(0)
lcPriorCdx  = ORDER()
lcPriorFlt  = FILTER()

PRIVATE lcFiltExp
lcFiltExp = ""


PRIVATE lcApDstCdx 
SELECT APDIST
lcApDstCdx  = ORDER()
SET ORDER TO Invvend 

* Assign no space to lcInvNo 
lcInvNo = ''  


IF !EMPTY(lcTrnNo)
  IF !SEEK(lcTrnNo) .OR. ATC("?",lcTrnNo) > 0
    lnClosRec  = RECNO(0)
    DIMENSION laTemp[4]
    laTemp = ''
    lcSavBrFld = lcBrFields
    lcSavTitle = lcFile_Ttl
    lcBrFields = "CVENDCODE :H= 'Vendor code'    ,;
                  CINVNO    :H= 'Invoice number' ,;    
                  DAPDTRDAT :H= 'Invoice date'   ,;
                  CAPDREF   :H= 'Reference'"
    IF BETWEEN(lnClosRec,1,RECCOUNT('APDIST'))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF
    
    * APDIST.cApdTrTyp  
    * 'A' --- DM Application    
    * 'B' --- Bank Adj.
    * 'H' --- Cash Payment
    * 'I' --- Invoice
    * 'M' --- Manual Payment
    * 'N' --- Non Manual Payment
    * 'P' --- Printed Checks
    DIMENSION  laTrnType[3]
    

    DO CASE
      CASE lcMethod = 'M'
        lnTrnType = 1
        lcFile_Ttl =  "Manual checks"
      CASE lcMethod = 'N'
        lnTrnType = 2
        lcFile_Ttl =  "Non check payments"
      CASE lcMethod =  "P"
        lnTrnType = 3     
        lcFile_Ttl =  "Printed checks" 
    ENDCASE
    
    
    laTrnType[1]  = [ cApdTrTyp = 'M' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[2]  = [ cApdTrTyp = 'N' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[3]  = [ cApdTrTyp = 'P' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]

    lcFiltExp = laTrnType(lnTrnType)   

    * Add vendor code to filter if not bank adjustment.
    IF !EMPTY(laOGVRFLT[1,6]) AND lnTrnType <> 3 
      lcFiltExp = lcFiltExp  +  " AND  APDIST.CVENDCODE = '"+laOGVRFLT[1,6]+"'  " 
    ENDIF 
    
    GO TOP IN 'APDIST'
    
    =gfBrows( 'FOR ' + lcFiltExp  ,'CVENDCODE,CINVNO,DAPDTRDAT,CAPDREF','laTemp')
    
    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle
    IF !EMPTY(laTemp[4])
      * Assign selected vendor no. to lcVenNo variable
      lcVenNo   = laTemp[1]
      * Assign selected Reference no. to lcTrnNo variable
      lcTrnNo   = laTemp[4]
      * Assign invoice no to lcInvNo variable      
      lcInvNo   = laTemp[2]
    ELSE
      * Assign no space to lcTrnNo and lcInvNo 
      lcTrnNo = lcOldVal
      lcInvNo = ''
      
    ENDIF
  ENDIF
ENDIF
SHOW GET lcTrnNo


SELECT APDIST 
SET  ORDER TO &lcApDstCdx  

SELECT (lcPriorDbf)
SET ORDER TO &lcPriorCdx
SET FILTER TO  &lcPriorFlt  



*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ramy Mabrouk
*! Date      : 07/16/2000
*! Purpose   : When function for the transaction No. field
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
* This function was added by RAMY due to B803359,1
*!*************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))


*!*************************************************************
*! Name      : lfvClrRead
*! Developer : Ramy Mabrouk
*! Date      : 07/16/2000
*! Purpose   : Valid function to clear the suppress expr.
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
* This function was added by RAMY due to B803359,1
*!*************************************************************

FUNCTION lfvClrRead

CLEAR READ