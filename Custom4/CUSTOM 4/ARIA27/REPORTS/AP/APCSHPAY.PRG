*:***************************************************************************
*: Program file  : APCSHPAY.PRG
*: Program desc. : STANDARD CASH PAYMENT REPORT 
*: Date          : 06/14/2005
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS PAYABLE (AP)
*: Developer     : EHAB ISMAIL HAMED(EIH)
*: Tracking Job Number: E127262
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO APCSHPAY
*:***************************************************************************
*: Modifications :
*:***************************************************************************

SELECT APPAYMNT
SET ORDER TO TAG PAYMNTS   IN APDIST
SET ORDER TO TAG INVVEND   IN APINVHDR
SET RELATION TO appaymnt.cpaymeth+ appaymnt.cbnkcode+ appaymnt.cchkacct+ appaymnt.cpaydocno INTO APDIST ADDITIVE

SELECT APDIST
SET RELATION TO apdist.cinvno+ apdist.cvendcode INTO APINVHDR ADDITIVE

SELECT APPAYMNT
SET SKIP TO APDIST
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ])
lcRpExp = lcRpExp + "APDIST.cVendCode=APPAYMNT.cPayClNo" 
lcRpExp = IIF(EMPTY(lcAccCode), lcRpExp , STRTRAN(lcRpExp,'GLACCHAR.CACCTCODE','APDIST.capdglact') )
lcRpExp = IIF(EMPTY(lcTrnNo),lcRpExp ,lcRpExp + " AND APDIST.cApdRef=lcTrnNo AND APDIST.capdactid = 'C'" )
*FOR VOID PAYMENTS
lcRpExp = IIF(llRpIncVPy,lcRpExp ,lcRpExp + " AND APPAYMNT.cpaystat <>'V'" )

lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ])
lcRpExp = lcRpExp + [ cPayMeth = 'H' ]


lcMainF    = gfTempName()
*Create Temp File
=lPCreatTmp()
*HERE COLLECTING DATA
=lfColldata()

SELECT APPAYMNT
SET RELATION TO
SELECT APDIST
SET RELATION TO

SELECT (lcMainF)
DO gfDispRe WITH EVAL('LCRPFORM')
                      *-- End of the Program --*

*!*************************************************************
*! Name      : lfRepShow
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : 
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfRepShow()
*!*************************************************************
FUNCTION lfRepShow

laOGObjCnt[3] = gfGetMemVar('LLMULCURR')

=lfOGShowGet("lnRepCurr")

*--End of lfRepShow.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : 
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfwOpGrid
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : This function is to save the array element number
*!             of the Bank code and Checking account code.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfwOpGrid()
*!*************************************************************
FUNCTION lfwOpGrid

lnBnCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Bank Code  
lnChCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Checking Account 
*--FOR Loop to find the LAOGVRFLT element number of the Bank Code and Checking Account
FOR lnCount = 1 TO ALEN(LAOGVRFLT , 1)
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CBNKCODE'
       lnBnCodEl = lnCount
    ENDIF    &&End of IF
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CCHKACCT'
       lnChCodEl = lnCount
    ENDIF
ENDFOR

*--End of lfwOpGrid.



*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : To display the payment amount according to the
*!             currency selected.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfAmntDisp()
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
    IF CCURRCODE = gcBaseCurr  
      SELECT (lnSavAlias)
      RETURN lnAmount  
    ENDIF

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
  
    IF lcCurrCode = APDIST.CCURRCODE 
   	  lnExRate  = APDIST.nExRate
    ENDIF
    
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

*--End of lfAmntDisp.
*!*************************************************************
*! Name      : lfvTrnNo
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : Valid function for the transaction No. field
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvTrnNo()
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

*--Assign no space to lcInvNo 
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
    DIMENSION  laTrnType[4]
    
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
      *eih
      CASE lcMethod =  "H"
        lnTrnType = 4     
        lcFile_Ttl =  "Cash Payment"   
      *eih  
    ENDCASE
    
    
    laTrnType[1]  = [ cApdTrTyp = 'M' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[2]  = [ cApdTrTyp = 'N' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[3]  = [ cApdTrTyp = 'P' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[4]  = [ cApdTrTyp = 'H' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]

    lcFiltExp = laTrnType(lnTrnType)   

    *--Add vendor code to filter if not bank adjustment.
    IF !EMPTY(laOGVRFLT[1,6]) AND lnTrnType <> 3 
      lcFiltExp = lcFiltExp  +  " AND  APDIST.CVENDCODE = '"+laOGVRFLT[1,6]+"'  " 
    ENDIF 
    
    GO TOP IN 'APDIST'
    
    =gfBrows( 'FOR ' + lcFiltExp  ,'CVENDCODE,CINVNO,DAPDTRDAT,CAPDREF','laTemp')
    
    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle
    IF !EMPTY(laTemp[4])
      *--Assign selected vendor no. to lcVenNo variable
      lcVenNo   = laTemp[1]
      *--Assign selected Reference no. to lcTrnNo variable
      lcTrnNo   = laTemp[4]
      *--Assign invoice no to lcInvNo variable      
      lcInvNo   = laTemp[2]
    ELSE
      *--Assign no space to lcTrnNo and lcInvNo 
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

*--End of lfvTrnNo.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/06/2005
*! Purpose   : When function for the transaction No. field
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfvClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/06/2005
*! Purpose   : Valid function to clear the suppress expr.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvClrRead()
*!*************************************************************
FUNCTION lfvClrRead

CLEAR READ

*--End of lfvClrRead.





*EIH 
*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!*************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED("SYCACCOD") 
*  SELECT 0
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*SELECT SYCACCOD
IF NOT USED("ACCOD") 
  SELECT 0
  *E300789,7 WALID (WAM) [BEGIN] Open with no index
  *USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD 
  *E300789,7 WALID (WAM) [END..]
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 WALID (WAM) [BEGIN]
GO TOP
IF !EOF()
*IF SEEK(gcAct_Comp)
*E300789,7 WALID (WAM) [END..]
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
IF llUesdBefo       && .F.- this file used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *USE IN SYCACCOD
  USE IN ACCOD
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas







*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
*
FUNCTION lfvActCod
*B803801,1 ABD Fix Bug Table is not ordered. [Begin]
PRIVATE lcOldOrder
lcOldOrder = ''
*B803801,1 ABD 

*B600366,1 Reham On 06/01/95 Add this function to browse if acc. does not
*B600366,1 exist in the file.

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

IF !EMPTY(&lcRpCurFld)
  SELECT GLACCHAR
  *B803801,1 ABD Fix Bug Table is not ordered save old order and set order to acctcode. [Begin]  
  lcOldOrder = ORDER()
  SET ORDER TO ACCTCODE
  *B803801,1 ABD   [End]
  IF "?" $ &lcRpCurFld .OR. !SEEK(&lcRpCurFld)
    =gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
    &lcRpCurFld = laRpRetFld[1]
    *EIH
    lcAccCode =laRpRetFld[1]
    *EIH
    SHOW GET (lcRpCurFld)
  ENDIF
  *B803801,1 ABD Return the old order. [Begin]
  SET ORDER TO &lcOldOrder  
  *B803801,1 ABD   [End]  
ENDIF

lcBrFields = lcOldBrFld









*!*************************************************************
*! Name      : lPCreatTmp
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : Create Temperory File to collect data in it.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lPCreatTmp()
*!*************************************************************
PROCEDURE lPCreatTmp
* Create the temporary file
SELECT APPAYMNT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+6,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAPDGLACT'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 24
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'capsessno'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cinvno'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 12
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cinvstat'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cinvref'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 15
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'napdamnt'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 15
laFileStru[lnFileStru,4] = 2

CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON &lcRpGrp TAG hh ADDITIVE 

*!*************************************************************
*! Name      : lfColldata
*! Developer : EHAB ISMAIL HAMED (EIH)
*! Date      : 06/14/2005
*! Purpose   : Collecting Data .
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : APCSHPAY.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfColldata()
*!*************************************************************
FUNCTION lfColldata
SELECT APPAYMNT
STORE '' TO   lcNewPay
lcOldPay = 'ANY'
SCAN FOR &lcRpExp
  lcNewPay = APPAYMNT.cpaydocno
  IF lcNewPay <> lcOldPay 
    SCATTER MEMVAR MEMO
    *search for APDIST.capdactid = 'C'
    SELECT APDIST
    lnRecNo = RECNO()
    SCAN 
      IF APDIST.capdactid = 'C' &&AND APDIST.napdamnt < 0
        EXIT
      ENDIF
    ENDSCAN
    GO lnRecNo
    SELECT (lcMainF)
    APPEND BLANK
    m.capdglact = APDIST.capdglact
    m.capsessno = APDIST.capsessno
    m.napdamnt  = APDIST.napdamnt
    m.cinvno    = apinvhdr.cinvno
    m.cinvstat  = apinvhdr.cinvstat
    m.cinvref   = apinvhdr.cinvref
    GATHER MEMVAR MEMO
    lcOldPay =lcNewPay 
    SELECT APPAYMNT
  ENDIF
ENDSCAN
