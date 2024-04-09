*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*B500795,1 MAN 06/21/95 Adding printing theYear & period in the FRX
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*B800556,1 Reham On 04/14/96
*B800556,1 Add function to validate the account code.
*B800868,1 HS 11/27/96 Add function to validate the AP Session [lfvSession]
*B601506,1 HS 12/22/96 Blank the account code if the value equal 0  
*B601506,1             [and ther is GL link]
*B802483,1 AKA 08/01/99 1) Release/Unrelease/Both option added to report
*B802483,1              2) Sort by Gl Account / Transaction Type added to report
*B802483,1              3) Transaction Number added to report
*B603522,1 SSE 03/19/2000 Convert all objects in FRX to TRANSPARENT
*C101831,1 SSE 03/27/2000 Add another FRX (132 Char.) in case of sorting by
*C101831,1                GL Account to show Trans. Date and Vendor Name
*B604005,1 MHM 11/02/2000 Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM            modifiy Debit/Credit field in form B graphics FRX , in form T Text We 
*B604005,1 MHM            modifiy Debit/Credit field too but decrease field description
*B606225,1 ALB 10/29/2002 Report should not display the void record
*E301982,1 ALB 04/30/2003 Add Vendor / Invoice sort to OG
*B607477,1 ALB 08/14/2003 Enhanced the report performance
*B119592,1 ALB 09/07/2003 Correct bug when sort by GL account
*B120507,1 ABD 12/28/2003 Fix bug that the AP Journal does not include Void records within the date range.
**************************************************************************************************************************
SELECT APDIST

*B607477,1 ALB Enhanced the report performance [BEGIN]
PRIVATE lcTempDist
lcTempDist = ''
lcTempDist = gfTempName()
*B607477,1 ALB Enhanced the report performance [END]

*B802483,1 (Start)
*SET ORDER TO ACCTYRPR
=lfvRelStat()
=lfvSort()
IF !EMPTY(lcTrnNo)
  lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " APDIST.cApdRef= '" + lcTrnNo+ "'  AND APDIST.cInvNo= '" + lcInvNo+"'  AND cVendCode = '"+lcVenNo+"'   " 
ENDIF  
lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " APDIST.nApdAmnt <> 0  " 
*B802483,1 (End)
*B606225,1 ALB Report should not display the void record [Begin]

*B120507,1 ABD - Remark the next line. [Begin]
*lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " APDIST.capdstat <> 'V'  " 
*B120507,1 ABD - [End]
*B606225,1 ALB Report should not display the void record [end]


*B120507,1 ABD - Fix bug that the AP Journal does not include Void records within the date range. [Begin]
STORE {} TO ldFrmDate , ldEnddate
lnDatePos  = lfItmPos('APDIST.DAPDTRDAT ')

IF !EMPTY(laOGFxFlt[lnDatePos,6])
    lnPipPos = ATC('|',laOgFxFlt[1,6]) - 1
    ldFrmDate = CTOD(SUBSTR(laogFxFlt[1,6],1,lnPipPos))
    ldEnddate = CTOD(SUBSTR(laogFxFlt[1,6],lnPipPos+ 2,LEN(laOGFxFlt[lnDatePos,6])))
ENDIF

lcRpExp = lcRpExp + IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " (APDIST.capdstat <> 'V' .OR. (APDIST.capdstat = 'V' .AND. APDIST.cApdTrTyp $ 'MHNP' .AND. BETWEEN(APDIST.DAPDTRDAT,ldFrmDate,ldEnddate))) " 
*B120507,1 ABD - [End]


*E301982,1 ALB Add Vendor / Invoice sort to OG [BEGIN]
lcRpExp = lcRpExp + IIF(!llRpPrven AND lcRpSort = 'V',IIF(EMPT(lcRpExp) , '' , ' AND ' )  + " !EMPTY(APDIST.cvendcode) " ,'')
*E301982,1 ALB Add Vendor / Invoice sort to OG [END]


*B607477,1 ALB Enhanced the report performance [BEGIN]
IF lcRpSort = 'G'
  SELECT * FROM APDIST WHERE &lcRpExp;
  INTO CURSOR &lcTempDist. ;
  ORDER BY CAPDGLACT,CFISFYEAR,CFSPPRDID
  SELECT (lcTempDist)
  DO gfDispRe WITH EVAL('LCRPFORM'),.T.
  IF _TALLY > 0
    SELECT APDIST
    USE IN &lcTempDist.
  ENDIF
ELSE
*B607477,1 ALB Enhanced the report performance [END]
  DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp
ENDIF

SET RELATION TO
SET ORDER TO

*!*************************************************************
*! Name      : lfvApAcCod
*! Developer : Reham Al-Allamy
*! Date      : 04/14/1996
*! Purpose   : This function is to validate the accounts from 
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfvApAcCod()
*!*************************************************************
*
FUNCTION lfvApAcCod

lcSavAlias  = ALIAS()  && Variable to save the selected alias.

lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

*B800556,1 Prevent executing the browse if the account code is empty.
IF !EMPTY(STRTRAN(lcFieldCont,"-",""))

  IF llApGlLink .AND. lcFieldCont <> lcEmptyAcs

    SELECT('lcLinkChar')
    SET ORDER TO TAG ACCTCODE

    IF !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
      DIMENSION laTemp[2]
      laTemp = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description'"

      lcFile_Ttl="Chart of accounts"
        
      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      IF !EMPTY(laTemp[1])
        lcFieldCont = ALLTRIM(laTemp[1])
       ELSE
        lcFieldCont = REPLICATE('0',lnApsAcLen)  
      ENDIF
    ENDIF  
 
    IF !EMPTY(lcSavAlias)
      SELECT(lcSavAlias)
    ENDIF  
    lcVarName = SYS(18)
    
    *B800556,1 Blank the account code if the value equal 0.
    &lcVarName= IIF(VAL(lcFieldCont) = 0 , "" , lcFieldCont)

  ENDIF
ENDIF

*B800556,1 Blank the account code if the value equal 0.

*B601506,1 Change this line [Begin]
*IF VAL(lcFieldCont) = 0
*B601506,1 Blank the account code if the value equal 0 and ther 
*          is GL link.
IF VAL(lcFieldCont) = 0 .AND. llApGlLink 
*B601506,1 Change this line [End]

  lcVarName  = SYS(18)
  &lcVarName = ""
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  


*!*************************************************************
*! Name      : lfvSession
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to validate the AP Session from 
*!             the Option Grid 
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800868,1 This function was added by HS
*!*************************************************************


FUNCTION lfvSession

lcSession = VARREAD()          &&Varible to hold the name of the memory varible used to create the AP Session @ ... GET field   
IF EMPTY(&lcSession)
   RETURN
ENDIF
&lcSession = PADL(ALLTRIM(&lcSession),8,'0')


*!*************************************************************
*! Name      : lfvSort
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : This function is used to order the report based on 
*!             Gl account or transaction type.
*!*************************************************************
*! Called from : APJORN.prg
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to solve bug no. 
*!*************************************************************

FUNCTION lfvSort
SELECT APDIST
*E301982,1 ALB Add Vendor / Invoice sort to OG [BEGIN]

*IF lcRpSort = 'G'
*  SET ORDER TO ACCTYRPR

  *C101831,1 SSE 03/27/2000 Ask about Report Format [Begin]
  *lcRpForm = 'APJORN'
*  lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')    
  *C101831,1 SSE 03/27/2000 [End]
  
*ELSE
*  lcRpForm = 'APJORNT'
  * If index name is already defined 
  * IF TYPE("lcJourTmp") <> 'U'
    * If index file is already creadted 
  * IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') 
  *    lcJourTmp = gfTempName()        
  *    INDEX ON capdtrtyp+cApdRef + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')          
  *  ELSE
  *    SET ORDER TO TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')      
  *  ENDIF
  *ELSE    && ELASE OF  "If index name is already defined "
  *  lcJourTmp = gfTempName()          
  *  INDEX ON capdtrtyp+cApdRef + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')    
  *ENDIF  && ENDIF OF  "If index name is already defined "
*ENDIF

DO CASE
  CASE lcRpSort = 'G'
    SET ORDER TO ACCTYRPR
    lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')    
    llRpPrven = .T.
    
  CASE lcRpSort = 'V'

    lcRpForm = 'APJORNV'
    * If index name is already defined 
    IF TYPE("lcJourTmp") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') 
        lcJourTmp = gfTempName()        
        INDEX ON cVendCode + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')          
      ELSE
        SET ORDER TO TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')      
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcJourTmp = gfTempName()          
      INDEX ON cVendCode + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')    
    ENDIF  && ENDIF OF  "If index name is already defined "

  CASE lcRpSort = 'T'
    lcRpForm = 'APJORNT'
    llRpPrven = .T.
    * If index name is already defined 
    IF TYPE("lcJourTmp") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcJourTmp+ '.CDX') 
        lcJourTmp = gfTempName()        
        INDEX ON capdtrtyp+cApdRef + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')          
      ELSE
        SET ORDER TO TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')      
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcJourTmp = gfTempName()          
      INDEX ON capdtrtyp+cApdRef + cInvNo TAG cJourTag OF (gcWorkDir +  lcJourTmp + '.CDX')    
    ENDIF  && ENDIF OF  "If index name is already defined "
ENDCASE
*E301982,1 ALB Add Vendor / Invoice sort to OG [END]

*C101831,1 SSE 03/27/2000 Read when function and refresh values [Begin]
CLEAR READ
*C101831,1 SSE 03/27/2000 [End]

RETURN 


*!*************************************************************
*! Name      : lfvRelStat
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : This function is used to determine wehther the user 
*!             wants only the released transactions or unreleased 
*!             transactions or both.
*!*************************************************************
*! Called from : APJORN.prg
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to soled bug no. 
*!*************************************************************
FUNCTION lfvRelStat
DO CASE
  CASE lcRpRel = 'B'
    * 'Both'  Nothing to do 
  CASE lcRpRel = 'R'
    * Only Rleased tran.  
    lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ' )  + " APDIST.lApdPost "
  CASE lcRpRel = 'U'
    * Only Unrleased tran.  
    lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ' )  + " !APDIST.lApdPost "
ENDCASE
RETURN 



*!*************************************************************
*! Name      : lfwApJour
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : This function is used as a when function for this report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to solve bug no. 
*!*************************************************************
FUNCTION lfwApJour

*C101831,1 SSE 03/27/2000 Commented out [Begin]
*PRIVATE  lnTrnType

*IF TYPE('INVB(4)') <> 'N'
*  lcTrnNo = ''
*  SHOW GET lcTrnNo  DISABLE   
*ELSE
*  lnTrnType = INVB(4)
*  IF lnTrnType > 1 
*    SHOW GET lcTrnNo ENABLE
*    =lfvTrnNo(lnTrnType)
*  ELSE
*    lcTrnNo = ''  
*    SHOW GET lcTrnNo DISABLE
*  ENDIF
*ENDIF
*C101831,1 SSE 03/27/2000 [End]

*C101831,1 SSE 03/27/2000 To Enable/Disable Objects in OG [Begin]
lnTranPos  = lfVarPos('lcTrnNo')      && Transaction Number Var Position
lnFormtPos = lfVarPos('lcRpFormat')   && Report Format Var Position

lnTrnTyPos = ASCAN(laOGVrFlt,'APDIST.CAPDTRTYP')
IF lnTrnTyPos > 0
  lnTrnTyPos = ASUBSCRIPT(laOGVrFlt,lnTrnTyPos,1)
ENDIF

DO lpShowObj
*C101831,1 SSE 03/27/2000 [End]

RETURN 


*!*************************************************************
*! Name      : lfvTrnNo
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : browse transaction no. from APDIST file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  : Transaction type
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to solve bug no. 
*!*************************************************************
FUNCTION lfvTrnNo

*C101831,1 SSE 03/27/2000 Commented out [Begin]
*PARAMETERS  lnTrnType
PRIVATE lnTrnType
*C101831,1 SSE 03/27/2000 [End]

PRIVATE  lcPriorDbf , lcPriorCdx  , lcPriorFlt
lcPriorDbf  = SELECT(0)
lcPriorCdx  = ORDER()
lcPriorFlt  = FILTER()

PRIVATE lcFiltExp
lcFiltExp = ""


SELECT APDIST
SET ORDER TO Invvend 

* Assign no space to lcInvNo 
lcInvNo = ''  

IF !EMPTY(lcTrnNo) 
  IF !SEEK(lcTrnNo) .OR. ATC("?",lcTrnNo) > 0
    ** MESSAGE : " This record is not found in the  "
    **           " data file.                       "
    **           "      < Browse >   < Reenter>     "
    lnClosRec  = RECNO(0)
    DIMENSION laTemp[4]
    laTemp = ''
    lcSavBrFld = lcBrFields
    lcSavTitle = lcFile_Ttl
    lcBrFields = "CVENDCODE :H= 'Vendor code'    ,;
                  CINVNO    :H= 'Invoice number' ,;    
                  DAPDTRDAT :H= 'Invoice date'   ,;
                  CAPDREF   :H= 'Reference'"
                  
    lcFile_Ttl = "Invoice"
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
    DIMENSION  laTrnType[8]
    
    *C101831,1 SSE 03/27/2000 Adjust cases of Transaction Types [Begin]
    DO CASE
      CASE EMPTY(laOGVrFlt[lnTrnTyPos,6])
        lnTrnType = 1
      CASE laOGVrFlt[lnTrnTyPos,6] = "A"
        lnTrnType = 2
      CASE laOGVrFlt[lnTrnTyPos,6] = "B"
        lnTrnType = 3
      CASE laOGVrFlt[lnTrnTyPos,6] = "H"
        lnTrnType = 4
      CASE laOGVrFlt[lnTrnTyPos,6] = "I"
        lnTrnType = 5
      CASE laOGVrFlt[lnTrnTyPos,6] = "M"
        lnTrnType = 6
      CASE laOGVrFlt[lnTrnTyPos,6] = "N"
        lnTrnType = 7
      CASE laOGVrFlt[lnTrnTyPos,6] = "P"
        lnTrnType = 8      
    ENDCASE
    *C101831,1 SSE 03/27/2000 [End]
    
    laTrnType[1]  = [ .T. ]
    laTrnType[2]  = [ cApdTrTyp = 'A' AND cApdActId = 'A' AND nApdAmnt >  0  AND cApdStat <> 'V' ]
    laTrnType[3]  = [ cApdTrTyp = 'B' AND cApdActId = 'D' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[4]  = [ cApdTrTyp = 'H' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[5]  = [ cApdTrTyp = 'I' AND cApdActId = 'A' AND nApdAmnt <  0  AND cApdStat <> 'V' ]
    laTrnType[6]  = [ cApdTrTyp = 'M' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[7]  = [ cApdTrTyp = 'N' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]
    laTrnType[8]  = [ cApdTrTyp = 'P' AND cApdActId = 'A' AND nApdAmnt <> 0  AND cApdStat <> 'V' ]

    lcFiltExp = laTrnType(lnTrnType)   

    * Add vendor code to filter if not bank adjustment.
    IF !EMPTY(laOGVRFLT[1,6]) AND lnTrnType <> 3 
      lcFiltExp = lcFiltExp  +  " AND  APDIST.CVENDCODE = '"+laOGVRFLT[1,6]+"'  " 
    ENDIF 
    
    =gfBrows( 'FOR' + lcFiltExp  ,'CVENDCODE,CINVNO,DAPDTRDAT,CAPDREF','laTemp')
    
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
      lcTrnNo = ''
      lcInvNo = ''  
    ENDIF
  ENDIF
ENDIF
SHOW GET lcTrnNo

SELECT (lcPriorDbf)  
SET ORDER TO &lcPriorCdx
SET FILTER TO  &lcPriorFlt  


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
**B802483,1 AKA 
FUNCTION lfClearRep
IF FILE(gcWorkDir +lcJourTmp+ '.CDX') 
  SELECT APDIST
  CLOSE INDEX
  ERASE (gcWorkDir +lcJourTmp+ '.CDX')
ENDIF


*!**************************************************************************
*!
*!      FUNCTION: lfvGetTrTyp
*!
*!**************************************************************************
* *B802483,1 AKA 
FUNCTION lfvGetTrTyp
RETURN IIF(cApdTrTyp = 'A' , 'DM application' ,IIF(cApdTrTyp = 'B' , 'Bank adjustment', IIF(cApdTrTyp = 'H' , 'Cash payments',IIF(cApdTrTyp = 'I' , 'Invoice', IIF(cApdTrTyp = 'M' , 'Manual checks',IIF(cApdTrTyp = 'N' , 'Non check payment',IIF(cApdTrTyp = 'P' , 'Printed checks' ,'')))))))  




*!**************************************************************************
*!
*!      FUNCTION: lfRepshow
*!
*!**************************************************************************
* *B802483,1 AKA 
FUNCTION lfRepshow 
*lcTrnNo = ''
*SHOW GET lcTrnNo  DISABLE 


*!***************************************************************************
*! Name      : lfvFormat
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/26/2000
*! Purpose   : Valid for changing Report Format 
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : = lfvFormat()
*!***************************************************************************
*C101831,1 SSE 03/27/2000 Adding the Report Format option in OG
*
FUNCTION lfvFormat
lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')
*-- End of lfvFormat.

*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/26/2000
*! Purpose   : Enable/Disable Report Format option in OG 
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
*C101831,1 SSE 03/27/2000
*
PROCEDURE lpShowObj

*-- If Sort By is GL Account
IF lcRpSort = 'G'
  laOGObjCnt[lnFormtPos] = .T.     && Enable Report format
  laOGObjCnt[lnTranPos] = .F.      && Disable Transaction Number
ELSE   && Else Sort By is Transaction
  laOGObjCnt[lnFormtPos] = .F.     && Disable Report format
  lcRpFormat = "A"                 && Turn Report format to 'A'

  *-- If Transaction Type is Like (Array)
  IF laOGVrFlt[lnTrnTyPos,5] = "Like"

    *-- If Transaction Type is ALL
    IF EMPTY(laOGVrFlt[lnTrnTyPos,6])
      laOGObjCnt[lnTranPos] = .F.      && Enable Transaction Number
    ELSE  && Transaction Type is any not ALL 
      laOGObjCnt[lnTranPos] = .T.      && Enable Transaction Number    
    ENDIF  
    *-- EndIf of Transaction Type is ALL

  ELSE   && Transaction Type is In List (Mover)
    laOGObjCnt[lnTranPos] = .F.      && Enable Transaction Number 
  ENDIF
ENDIF
*-- Endif of Sort By

=lfOGShowGet('lcRpFormat')   && Show get Object .
=lfOGShowGet('lcTrnNo')      && Show get Object .
*-- End of lpShowObj.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/26/2000
*! Purpose   : get position of some variable in OG
*!***************************************************************************
*! Called from : OG When Function
*!***************************************************************************
*! Example   : = lfVarPos()
*!***************************************************************************
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
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Abdou Elgendy (Abd)
*! Date      : 12/28/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B120507,1 ABD - [Begin]
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- end of lfItmPos.
*B120507,1 ABD - [End]
*!***************************************************************************

FUNCTION lfVTranTyp
LCTRNNO = ''
CLEAR READ
