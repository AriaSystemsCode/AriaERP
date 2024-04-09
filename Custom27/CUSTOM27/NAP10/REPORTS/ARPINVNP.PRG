*:***************************************************************************
*: Program file  : ARPINVNP.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR NAP.
*: Date          : 06/14/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : AHMED MOHAMED EL_ANWER (AME)
*: Tracking Job Number: C102343
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVNP
*:***************************************************************************
*:Modifications:
*:B804414,1 MHM 09/18/2001 make some changes in FRX and remove lfCountLin from FRX 
*:B804414,1                to speed program 
*:B604992,1 BWA 10/04/2001 Fix the bug of in many invoices there is a deleted line.
*:B605563,1 NAD 02/17/2002 Fix subscript out of bounds while printing from the invoice screen 
*:B605805,1 SSE 04/14/2002 Fix bug of variable qty0 not found.
*:***************************************************************************
*B605563,1 NAD (Start)
*DIMENSION laData[1]
DIMENSION laDat[1]
*B605563,1 NAD (End)
*LCTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
*XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
*XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
*XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
*lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*B604992,1 BWA 10/04/2001 Fix the bug of in many invoices there is a deleted line.[START]
*-- Open the INVLINE.DBF again to get the last lineno for the invoice.
IF !USED(lcInvLine_B)
  =gfOpenFile(gcDataDir+"InvLine","InvLine",'SH', @lcInvLine_B, .T.)
ENDIF
*B604992,1 BWA 10/04/2001.[END]

*lcColorNp = ''
IF !USED(lcCustomer)
  = gfOpenFile(gcDataDir+'CUSTOMER' ,gcDataDir+'CUSTOMER', 'SH', @lcCustomer, .T.)
ENDIF
IF !USED(lcSPCK_Lin)
  = gfOpenFile(gcDataDir+'SPCK_Lin' ,gcDataDir+'SPCKLins', 'SH', @lcSPCK_Lin, .T.)
ENDIF

*IF !USED('CUST_A')
*  USE (gcDataDir+'Customer') AGAIN ALIAS CUST_A ORDER TAG CUSTOMER IN 0
*ENDIF

*IF !USED('SPCK_LNA')
*  USE (gcDataDir+'Spck_lin') AGAIN ALIAS SPCK_LNA ORDER TAG Spcklins IN 0
*ENDIF
SELECT CUSTOMER
SET RELATION TO 'S'+ACCOUNT+DIST_CTR INTO &lcCustomer ADDITIVE
SET RELATION TO 'P' INTO SKUTMPL ADDITIVE
*--The Color , Style Length 
STORE 0 TO lnClrLnNp , lnClrPosNp
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnItCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnItCount,1]='C'
    lnClrPosNp = laItemSeg[lnItCount,4]
    lnClrLnNp  = LEN(laItemSeg[lnItCount,3])
    EXIT
  ENDIF
ENDFOR

SELECT INVHDR

*B605805,1 Define new array for sku sizes. [Begin]
*lcSkip = SET('SKIP')
*IF !EMPTY(lcSkip)
*  SET SKIP TO &lcSkip , SPCK_LIN
*ELSE
*  SET SKIP TO SPCK_LIN
*ENDIF
DIMENSION laSku1[8]
STORE SPACE(0) TO laSku1
STORE SPACE(0) TO lcStrToPr1
*B605805,1 Define new array for sku sizes. [End]

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Ahmed Mohammed ElAnwer (AME)
*! Date      : 06/17/2001
*! Purpose   : To print the right CAddress2 and CAddress3+CAddress4+CAddress5
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr
PARAMETERS lcPrnHdr,lcType
*-- The case of printing the CAddress2
IF lcType = "A"
  *C102343,4 AME [Start] Return value should be in variable to deal with lfvldfn
  *RETURN IIF(!EOF(lcCustomer),IIF(!EMPTY(&lcCustomer..CAddress2),&lcCustomer..CAddress2,;
  LEFT(&lcCustomer..CAddress3,15)+' '+LEFT(&lcCustomer..CAddress4,3)+' '+LEFT(&lcCustomer..CAddress5,10)),;
  IIF(!EMPTY(CUSTOMER.CAddress2),CUSTOMER.CAddress2,;
  LEFT(CUSTOMER.CAddress3,15)+' '+LEFT(CUSTOMER.CAddress4,3)+' '+LEFT(CUSTOMER.CAddress5,10)))
  
  lcPrnHdr = IIF(!EOF(lcCustomer),IIF(!EMPTY(&lcCustomer..CAddress2),&lcCustomer..CAddress2,;
  LEFT(&lcCustomer..CAddress3,15)+' '+LEFT(&lcCustomer..CAddress4,3)+' '+LEFT(&lcCustomer..CAddress5,10)),;
  IIF(!EMPTY(CUSTOMER.CAddress2),CUSTOMER.CAddress2,;
  LEFT(CUSTOMER.CAddress3,15)+' '+LEFT(CUSTOMER.CAddress4,3)+' '+LEFT(CUSTOMER.CAddress5,10)))
  *C102343,4 AME [End]  
ELSE

  *C102343,4 AME [Start] Return value should be in variable to deal with lfvldfn
  *-- The case of printing the CAddress3+CAddress4+CAddress5
  *RETURN IIF(!EOF(lcCustomer),IIF(!EMPTY(&lcCustomer..CAddress2),;
  LEFT(&lcCustomer..CAddress3,15)+' '+LEFT(&lcCustomer..CAddress4,3)+' '+LEFT(&lcCustomer..CAddress5,10),' '),;
  IIF(!EMPTY(CUSTOMER.CAddress2),;
  LEFT(CUSTOMER.CAddress3,15)+' '+LEFT(CUSTOMER.CAddress4,3)+' '+LEFT(CUSTOMER.CAddress5,10),' '))

  lcPrnHdr = IIF(!EOF(lcCustomer),IIF(!EMPTY(&lcCustomer..CAddress2),;
  LEFT(&lcCustomer..CAddress3,15)+' '+LEFT(&lcCustomer..CAddress4,3)+' '+LEFT(&lcCustomer..CAddress5,10),' '),;
  IIF(!EMPTY(CUSTOMER.CAddress2),;
  LEFT(CUSTOMER.CAddress3,15)+' '+LEFT(CUSTOMER.CAddress4,3)+' '+LEFT(CUSTOMER.CAddress5,10),' '))
  *C102343,4 AME [End]

ENDIF  

RETURN lcPrnHdr

*!*************************************************************
*! Name      : lfCountLin
*! Developer : Ahmed Mohammed ElAnwer (AME)
*! Date      : 06/17/2001
*!*************************************************************
*! Example            :  lfCountLin()
*!*************************************************************
FUNCTION lfCountLin
PARAMETERS lcCountLin,lcInvoice

lcAlias = ALIAS()
SELECT invoice,'S' as type,account,style,;
space(16) as pack_id,LINENO from (GCDataDir+'invline') Invline_a where invoice+STR(lineno,6) = lcInvoice;
union SELECT invoice,Spck_lin_A.type,Spck_lin_A.account,Spck_lin_A.style,Spck_lin_A.pack_id,0;
 FROM (GCDataDir+'Spck_lin') Spck_lin_A,(GCDataDir+'Invline') Invline_A;
 WHERE Spck_lin_A.type+Spck_lin_A.account+Spck_lin_A.style+Spck_lin_A.pack_id = 'S'+Invline_A.account+ Invline_A.STYLE;
   AND Invline_A.invoice = lcInvoice order by 1,2,4,5 INTO DBF (GCWorkDir+lcTemp)
lnSelect = _TALLY

USE
*B605563,1 NAD (Start)
*SELECT DIST account,style FROM  (GCWorkDir+lcTemp) WHERE LINENO=0 AND !EMPTY(pack_id) into array ladata
SELECT DIST account,style FROM  (GCWorkDir+lcTemp) WHERE LINENO=0 AND !EMPTY(pack_id) into array ladat
*B605563,1 NAD (END)
USE in (lcTemp)

lnCount = lnSelect-_tally

SELECT (lcAlias)
RETURN ''


*!*************************************************************
*! Name        : lfInc
*! Developer   : Ahmed Mohammed ElAnwer (AME)
*! Date        : 06/17/2001
*! Purpose     : this function to increment the variable lnTemp
*!             : initialize the variable llprn with .t. 
*!             : if the invoice lines is printed completely 
*! Called From : ARPINVNP.FRX
*!*************************************************************
*! Example            :  lfInc()
*!*************************************************************
FUNCTION lfInc
PARAMETER lcInt

lntemp = lntemp + 1

*B604992,1 BWA 10/04/2001 Fix the bug of in many invoices there is a deleted line.[START]
*llprn = lnTemp = lnCount
llprn = lnTemp = lnCountNp
*B604992,1 BWA 10/04/2001.[END]

RETURN ''

*!*************************************************************
*! Name        : lfInt
*! Developer   : Ahmed Mohammed ElAnwer (AME)
*! Date        : 06/17/2001
*! Purpose     : this function to initialize the variable lnTemp and llPrn
*!             : to be use in checking if the invoice lines had been printed.
*! Called From : ARPINVNP.FRX
*!*************************************************************
*! Example            :  lfInt()
*!*************************************************************
FUNCTION lfInt
PARAMETER lcInt

lntemp = 0
llprn = .F.
RETURN ''



*!*************************************************************
*! Name        : lfClrDesc
*! Developer   : Ahmed Mohammed ElAnwer (AME)
*! Date        : 06/18/2001
*! Purpose     : Eval Color Code.
*! Called From : ARPINVNP.FRX
*!*************************************************************
*! Example            :  lfClrDesc()
*!*************************************************************
FUNCTION lfClrDesc
PARAMETERS lcClrDesc
lcColorNp = gfCodDes(SUBSTR(InvLine.Style,lnClrPosNp ,lnClrLnNp ) , 'COLOR')
RETURN ''


*!*************************************************************
*! Name      : lfPrntSku
*! Developer : Ahmed Mohammed ElAnwar (AME)
*! Date      : 06/18/2001
*! Purpose   : To print the SKU# per size 
*!*************************************************************
*! Example            :  lfPrntSku()
*!*************************************************************
*B605805,1 This function is not used anymore.
FUNCTION lfPrntSku
PARAMETER lcPrntSku
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcSPCK_Lin)
lcSpckLin = STR(lnSpkLin,1)
IF SEEK("S"+InvLine.Account+InvLine.Style)
  LOCATE REST WHILE Type+Account+Style+Pack_Id =;
                    "S"+InvLine.Account+InvLine.Style;
                    FOR Qty&lcSpckLin = 1
  IF FOUND()
    SELECT(lnAlias)  
    *C102343,4 AME [Start] Return value should be in variable to deal with lfvldfn
    *RETURN'Size:'+lcSpckLin+' '+ALLT(&lcSPCK_Lin..pack_id)
    lcPrntSku = 'Size:'+lcSpckLin+' '+ALLT(&lcSPCK_Lin..pack_id)
    RETURN lcPrntSku
    *C102343,4 AME [End]
  ELSE
    SELECT(lnAlias)
    RETURN ''
  ENDIF
ELSE
  SELECT(lnAlias)
  RETURN ''
ENDIF

*B604992,1 BWA 10/04/2001 Fix the bug of in many invoices there is a deleted line.[START]
*!*************************************************************
*! Name        : lfRecNo
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 10/09/2000
*! Purpose     : To get the Record count of the InvLine file.
*!             : To get the last No. of the invoice line.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfRecNo()
*!*************************************************************
FUNCTION lfRecNo
PARAMETER lcDummy
PRIVATE lcAlias
lcDummy = .T.

*-- Case get the count of the invoice lines.
lnCountNp = 0
lcAlias = ALIAS()
SELECT (lcInvLine_B)
=SEEK(InvHdr.Invoice,lcInvLine_B)
SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
  lnCountNp = lnCountNp + 1
ENDSCAN

SELECT (lcAlias)
RETURN ""
*-- End of lfRecNo.
*B604992,1 BWA 10/04/2001.[END]

*!***************************************************************************
*! Name        : lfGetSku
*! Developer   : Sameh Saiid Ezzat (SSE)
*! Date        : 04/16/2002
*! Purpose     : To get the SKU.
*!***************************************************************************
*! Called from : ARPINVNP.FRX
*!***************************************************************************
*! Example     : = lflnc()
*!***************************************************************************
*
FUNCTION lfGetSku
PARAMETERS lcDumdy

IF !SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF

STORE SPACE(0) TO laSku1
STORE SPACE(0) TO lcStrToPr1
PRIVATE lnSelect
lnSelect = SELECT (0)

SELECT Spck_Lin
IF !EMPTY(Pack_Id)

  lnI = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  
  lcStrToPr1 = SUBSTR(Pack_Id,1,lnDime1) + ' '  
  
  SCAN REST WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style AND lnI < 9
    FOR lnX = 1 TO 8
      Z = STR(lnX,1)
      IF QTY&Z > 0
        laSku1[lnX] = 'Size:'+Z+' '+ALLT(Spck_Lin.pack_id)
        EXIT      
      *ELSE
      *  IF NLINE1 <= LNCOUNTNP
      *    laSku1[lnX] = 'SKU # :   '+RIGHT(Spck_lin.pack_id,IIF(EMPTY(Customer.SkuTmpl),8,SKUTMPL.LEN4))
      *  ENDIF
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
ENDIF

SELECT (lnSelect)
IF EMPTY(lcStrToPr1)
  STORE SPACE(0) TO laSku1
  llPrtSku = .F.
ELSE
  llPrtSku = .T.
ENDIF
RETURN ''
*-- End of lfGetSku.