*!*************************************************************
*! Name : ICCOMRP.PRG
*! Auth : Ashraf Sherif Mohammad (ASH)
*! Date : 09/15/99
*! Note : This program is customized for customer "Robyn Mer."
*!      : based on custom program # 100878
*! Converted to 27 on C#101518.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Purpose     : Print "Comer. Invoice Report".
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Modifications:
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ


lmnote=''
llApLink   = (OCCURS('AP',gcCmpModules)<>0)
lnCount   = 0
lnPrtLine = 9
* -- Opening the necessary files

SELECT ComInvLn
SET RELATION TO CCOMINVNO INTO ComInvHd
SET FILTER TO &lcRpExp
LOCATE
IF EOF()
  =gfModalGen('QRM42188B42001','DIALOG','records selected')
  RETURN
ENDIF

R_WIDTH    = 'N'
SET CENT ON
DO gfDispRe WITH EVALUATE('lcFormName') 
SET CENT OFF
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 09/15/99
*! Purpose   : To validate the invoice number.
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!                          lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvInvNo()
*!*************************************************************

FUNCTION lfvInvNo
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT ComInvHd
*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal)
  llBrowse = .T.
  lcBrowTitl = "COMMERCIAL INVOICE"
  lcBrFields = [CCOMINVNO:8:H = "Invoice",Complete:9:H = "Invoice Date",;
                CSHPRNAME:30:H = "Shipper Name",CADDRESS1:30:H = "Shipper Address 1",;
                Vendor:8:H = "Consignee",CATTEN:30:H = "Attention",; 
                NWTOTAL:10:H = "Total Weight"]

  DIMENSION latemp[1]
  latemp = ''
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','ComInvHd.CCOMINVNO','laTemp')
    lcObjVal = laTemp[1]
    llBrowse = .F.
  ENDIF    && End of IF
  &lcObjNam = lcObjVal
ENDIF


FUNCTION lfwRepWhen
