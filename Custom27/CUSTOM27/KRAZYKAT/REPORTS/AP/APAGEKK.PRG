*:************************************************************************
*: Report name   : APAGEKK.PRG
*: Program desc. : Accounts Payable aged open items [detail and summary]
*:                 Report [APAGEDKK.FRX & APAGESKK.FRX]
*: System		 : Accounts payable
*: Developer	 : Albert Raif
*: Date      	 : 07/14/2002
*:************************************************************************
*: Calls : APAGEKK.SPR 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*:
lnTotPaymnt = 0  
llCanPrint  = .F.
lcTmpRpCur = gfTempName()     && Varible to hold a temp. name 

IF !EMPTY(ALLTRIM(lcRpVenFrm)) AND !EMPTY(ALLTRIM(lcRpVenFrm))
  lcRpExp = lcRpExp + " AND BETWEEN(APINVHDR.CVENDCODE,lcRpVenFrm,lcRpVenTo)"
ENDIF

lcRpExp = lcRpExp + " AND " + IIF(llRpConPay," nInvAmnt <> nInvPaid +  nInvDisTk +  nInvAdj ",;
	"nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj) +lfSumall(APINVHDR.cVendCode,APINVHDR.cInvNo)<>0 ")

*SELECT CODES
*SET FILTER TO crltfield + cfld_name = 'N'+"CTERMCODE"

SELECT APINVHDR
SET ORDER TO TAG VENDINV IN apinvhdr
IF !llRpConPay							&& Set relation
  SET ORDER TO TAG INVVEND IN APDIST
  SET RELATION TO CINVNO+CVENDCODE INTO APDIST ADDITIVE
  LOCAT
  SELECT APDIST
  SET ORDER TO TAG TYPMETHDOC IN APPAYMNT
  SET RELATION TO "P"+APDIST.CAPDTRTYP+PADL(ALLTRIM(APDIST.CAPDREF),8,"0")+;
                   APDIST.CBNKCODE+APDIST.CCHKACCT INTO APPAYMNT ADDITIVE
  LOCAT
ENDIF

IF lcRpForm = 'APAGEDKK'
  SELECT cAPAcct ,;
         cVendCode ,;
         cInvNo ,;
         dPostDate ,;
         dInvDate ,;
         dInvDuDat ,;
         nInvAmnt ,;
         nInvPaid ,;
         nInvDisTk ,;
         nInvAdj ;
    FROM APINVHDR;
    INTO CURSOR &lcTmpRpCur ;
    WHERE &lcRpExp ;
    ORDER BY cAPAcct ,;
             cVendCode ,;
             cInvNo
ELSE    && Else [IF the user want the Summary format] 

  SELECT cAPAcct ,;
         cVendCode ,;
         SUM(IIF(llRpConPay,0,lfSumall(APINVHDR.cVendCode,APINVHDR.cInvNo))) + ;
         SUM(nInvAmnt - nInvPaid - nInvDisTk - nInvAdj)  AS SumAmnt ;
     FROM APINVHDR ;
     INTO CURSOR &lcTmpRpCur ;
    WHERE &lcRpExp ;
    GROUP BY cAPAcct ,;
             cVendCode ;
    ORDER BY cAPAcct ,;
             cVendCode ,;
             cInvNo

ENDIF     && End of IF
SELECT APINVHDR
SELECT (lcTmpRpCur)
SET RELATION TO cVendCode  INTO APVENDOR
LOCAT
DO gfDispRe WITH EVAL('LCRPFORM')

SELECT APINVHDR
SET RELATION TO 

IF USED('APDIST')
  SELECT APDIST
  SET RELATION TO 
ENDIF

SELECT CODES
SET FILTER TO 

SELECT (lcTmpRpCur)
USE

*!*************************************************************
*! Name      : lfvApAcCod
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate the accounts from 
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvApAcCod()
*!*************************************************************
*
FUNCTION lfvApAcCod

lcSavAlias  = ALIAS()  && Variable to save the selected alias.

lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

*Prevent executing the browse if the account code is empty.
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
    
    *Blank the account code if the value equal 0.
    &lcVarName= IIF(VAL(lcFieldCont) = 0 , "" , lcFieldCont)

  ENDIF
ENDIF

*Blank the account code if the value equal 0 and ther is no GL link.
IF VAL(lcFieldCont) = 0 .AND. llApGlLink 
  lcVarName  = SYS(18)
  &lcVarName = ""
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  

*!*************************************************************
*! Name      : lfvVenCode
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to activate the Vendor Range  
*!             screen
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVenCode()
*!*************************************************************
*
FUNCTION lfvVenCode

IF USED('APVENDOR')
  llUsed = .T.     &&Flag to know if the file is open 
  SELECT APVENDOR
  lnOldRec = RECNO()          && Varible to hold the record number
  lcOldOrder = SET('ORDER')   && Varible to hold the old ORDER
  SET ORDER TO TAG VENCODE
ELSE    && Else
  llUsed = .F.     &&Flag to know if the file is not open
  SELECT 0
  USE (gcDataDir + 'APVENDOR') ORDER TAG VENCODE
ENDIF   && End of IF

lcOldVenFr = lcRpVenFrm     && Varible to hold the old Vendor code valeu of the From field 
lcOldVenTo = lcRpVenTo      && Varible to hold the old Vendor code valeu of the To field
lcscrName = gcRepHome + "AP\APAGEKK.SPR"
DO &lcscrName.

   
*!*************************************************************
*! Name      : lfvOk
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate Push Butoon OK in
*!             the Vendor Range screen
*!*************************************************************
*! Called from : The Vendor Range screen
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOk()
*!*************************************************************
*
FUNCTION lfvOk

llOk = .T.   && Flag to know if the From value is Less than or equal to the To value

IF EMPTY(lcRpVenTo)
  lcRpVenTo = ''
ENDIF    && End of IF
	
IF !EMPTY(lcRpVenFrm) .AND. !EMPTY(lcRpVenTo)
  llOk = IIF(lcRpVenFrm > lcRpVenTo , .F. , .T.) 
ENDIF    && End of IF  

IF !llOk
  WAIT WINDOW '"From" value must be Less than or equal to the "To" Value' NOWAIT
  _CUROBJ = OBJNUM(lcRpVenTo)
ELSE     && Else
  CLEAR READ
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate Push Butoon Cancel in
*!             the Vendor Range screen
*!*************************************************************
*! Called from : The Vendor Range screen
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

lcRpVenFrm = lcOldVenFr    && Restore the old valeu
lcRpVenTo = lcOldVenTo     && Restore the old valeu

*!*************************************************************
*! Name      : lfvVenFrm
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate Vendor code From Field 
*!             in the Vendor Range screen
*!*************************************************************
*! Called from : The Vendor Range screen
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVenFrm()
*!*************************************************************
*
FUNCTION lfvVenFrm

IF !EMPTY(lcRpVenFrm)
  SELECT APVENDOR
  lcSetExact = SET('EXACT')   && Varible to hold the SET EXACT status
  SET EXACT ON
  lcRpVenFrm = ALLTRIM(lcRpVenFrm)
  lcRpVenFrm = PADR(lcRpVenFrm , 8 , ' ')
  *IF Statment to check if the From valeu is not found in the APVENDOR file 
  * or if the user want to browse the file 
  IF !SEEK(lcRpVenFrm) .OR. ATC('?',lcRpVenFrm) > 0
    DIMENSION laTemp[1]
    laTemp = ''
    lcSavBrFld=lcBrfields
    lcSavTitle=lcFile_Ttl

    lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                  CVENCOMP :H= 'Company',;
                  CPHONENO :H= 'Phone'"
 
    lcFile_Ttl = "Vendor"
        
    =gfbrows(' ','CVENDCODE','laTemp')

    lcFile_Ttl=lcSavTitle
    lcBrfields=lcSavBrFld

    *IF Statment to check if the user has select from the browse 
    IF !EMPTY(laTemp[1])
      lcRpVenFrm = laTemp[1]
    ELSE   && Else
      lcRpVenFrm = lcOldVenFr  
    ENDIF  && End of IF
  ENDIF  && End of IF
  SET EXACT &lcSetExact           
ENDIF  && End of IF

*!*************************************************************
*! Name      : lfvVenTo
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate Vendor code To Field 
*!             in the Vendor Range screen
*!*************************************************************
*! Called from : The Vendor Range screen
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  llOk
*!*************************************************************
*! Example            :  =lfvVenTo()
*!*************************************************************
*
FUNCTION lfvVenTo

*IF Statment to check that the To valeu is not empty
IF !EMPTY(lcRpVenTo)
  SELECT APVENDOR
  lcSetExact = SET('EXACT')      && Varible to hold the SET EXACT status 
  SET EXACT ON
  lcRpVenTo = ALLTRIM(lcRpVenTo)
  lcRpVenTo = PADR(lcRpVenTo , 8 , ' ')
  *IF Statment to check if the To valeu is not found in the APVENDOR file 
  * or if the user want to browse the file 
  IF !SEEK(lcRpVenTo) .OR. ATC('?',lcRpVenTo) > 0
    DIMENSION laTemp[1]
    laTemp = ''
    lcSavBrFld=lcBrfields
    lcSavTitle=lcFile_Ttl

    lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                  CVENCOMP :H= 'Company',;
                  CPHONENO :H= 'Phone'"
 
    lcFile_Ttl = "Vendor"
        
    =gfbrows(' ','CVENDCODE','laTemp')

    lcFile_Ttl=lcSavTitle
    lcBrfields=lcSavBrFld

    *IF Statment to check if the user did not select from the browse 
    IF !EMPTY(laTemp[1])
      lcRpVenTo = laTemp[1]
    ELSE    && Else
      lcRpVenTo = lcOldVenTo  
    ENDIF   && End of IF
  ENDIF   && End of IF
  SET EXACT &lcSetExact   
ENDIF   && End of IF 

*!*************************************************************
*! Name      : lfvAging
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is to validate the Aging Date Field 
*!             in the The Option Grid
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAging()
*!*************************************************************
*
FUNCTION lfvAging

*IF Statment to check if the Aging date Field was empty
IF EMPTY(ldRpAging)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'Aging date')
  ldRpAging = ldRpOldAge    && Restore the old valeu
  _CUROBJ=_CUROBJ
ENDIF   && End of IF

*!*************************************************************
*! Name      : lfwAging
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : This function is the WHEN function of the Aging  
*!             Date Field in the The Option Grid
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAging()
*!*************************************************************
*
FUNCTION lfwAging

ldRpOldAge = ldRpAging    && Varible to save the old valeu


*!*************************************************************
*! Name      : lfSumALL
*! Developer : Albert Raif
*! Date      : 07/14/2002
*! Purpose   : Calculate the paid amount for passed vendor and invoice 
*!             after aging date
*! Reference : *C200069,1 AMM
*!*************************************************************
*! Called from : APAGEKK.RPR, APAGEDKK.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSumALL()
*!*************************************************************

FUNCTION lfSumALL
PARAMETERS lcVend,lcInv

=SEEK(lcVend+lcInv,'apinvhdr')
IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj = 0
  llCanPrint  = .T.
  lnTotPaymnt = 0
  RETURN 0
ENDIF

llCanPrint  = .F.
lnRPAlias=SELECT()
SELECT APDIST

=SEEK(lcInv+lcVend)

SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
    FOR ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
    AND dAPDtrdat > ldRpAging  AND cApdStat  <> 'V' TO lnTotPaymnt 

IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj
  SELECT APDIST
  lcOldTag = SET("ORDER")
  SET ORDER TO PAYMNTS
  SEEK "A"+REPL(" ",FSIZE("CBNKCODE"))+REPL(" ",FSIZE("CCHKACCT"))+lcInv
  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
                          REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
                      FOR cVendCode = lcVend ;
                      AND dAPDtrdat > ldRpAging  ;
                      AND cApdStat  <> 'V';
                      AND nApdAmnt > 0 TO lnTotDebit

  lnTotPaymnt = lnTotPaymnt + lnTotDebit
  SET ORDER TO &lcOldTag
ENDIF

SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN IIF(llCanPrint,lnTotPaymnt,0)
