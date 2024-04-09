*!*************************************************************
*! Name : ICDECRP.PRG
*! Auth : Ashraf Sherif Mohammad (ASH)
*! Date : 11/21/99
*! Note : This program is customized for customer "Robyn Mer."
*!      : based on custom program # 100878
*! Converted to 27 on C#101518.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Purpose     : Print "DECLARATION OF VALUE REPORT".
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Modifications:
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ


*--Get a temp file name.
lcRepTemp=gfTempName()


*--Initialize grid variables.
STORE SPACE(6) TO lcPOL,lcPOH
STORE SPACE(12) TO lcSTYL,lcSTYH
lcRpExp = STRTRAN(lcRpExp,'DECVALHD.PO','PO')
lcRpExp = STRTRAN(lcRpExp,"STYLE.STYLE","STYLE")
SELECT * FROM DECVALCM WHERE &lcRpExp INTO DBF (gcWorkDir+lcRepTemp)
SELECT (lcRepTemp)
INDEX ON PO+CSTYMAJOR TAG (lcRepTemp)
GO TOP
IF EOF()
  =gfModalGen('QRM42188B42001','DIALOG','records selected')
  RETURN
ENDIF

STORE '' TO lcVendor,lcName,lcAddres1,lcAddres2,lcAddres3,;
            lcPattern,lcStylDesc,lcFibrCont,lcDate

*--Start printing the report.
R_WIDTH    = 'N'
SET CENT ON
DO gfDispRe WITH EVALUATE('lcFormName') 

SET CENT OFF


RETURN
*--End...


*!****************************************************************
*! Name : lfvPO
*! Auth : Essmat M. Abdel Zaher
*! Date : 07/23/97
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Synopsis : Validate the PO number.
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
FUNCTION lfvPo
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT DecValHd
*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal)
  llBrowse = .T.
  lcBrowTitl = "PO HEADER"
  lcBrFields = [PO:8:H = "PO Number",cManf:12:H = "Vendor",;
   				cCompany:30:H = "COMPNAME",CADDRESS1:30:H = "COMPADD1",;
   				CADDRESS2:30:H = "COMPADD2",CADDRESS3:3:H = "COMPADD3",;
   				CADDRESS4:15:H = "COMPADD4",CADDRESS5:10:H = "COMPADD5"]

  DIMENSION latemp[1]
  latemp = ''
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','DecValHd.PO','laTemp')
    lcObjVal = laTemp[1]
    llBrowse = .F.
  ENDIF    && End of IF
  &lcObjNam = lcObjVal
ENDIF




*!****************************************************************
*! Name : lfvSTY
*! Auth : Essmat M. Abdel Zaher
*! Date : 07/23/97
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Synopsis : Validate the STYLE.
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         

FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!****************************************************************
*! Name : lfGetInfo
*! Auth : Essmat M. Abdel Zaher
*! Date : 07/27/97
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Synopsis : Get report header information.
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         

FUNCTION lfGetInfo

IF SEEK (Po,'DecValHd')
  lcName    = DecValHd.Ccompany
  lcAddres1 = DecValHd.CADDRESS1
  lcAddres2 = DecValHd.CADDRESS2
  lcAddres3 = DecValHd.cADDRESS3
  lcVendor  = DecValHd.cManf
  lcName    = SPACE(INT((LEN(lcName)-LEN(ALLTRIM(lcName)))/2))+ALLTRIM(lcName)+SPACE(INT((LEN(lcName)-LEN(ALLTRIM(lcName)))/2))   
  lcAddres1 = SPACE(INT((LEN(lcAddres1)-LEN(ALLTRIM(lcAddres1)))/2))+ALLTRIM(lcAddres1)+SPACE(INT((LEN(lcAddres1)-LEN(ALLTRIM(lcAddres1)))/2))   
  lcAddres2 = SPACE(INT((LEN(lcAddres2)-LEN(ALLTRIM(lcAddres2)))/2))+ALLTRIM(lcAddres2)+SPACE(INT((LEN(lcAddres2)-LEN(ALLTRIM(lcAddres2)))/2))   
  lcAddres3 = SPACE(INT((LEN(lcAddres3)-LEN(ALLTRIM(lcAddres3)))/2))+ALLTRIM(lcAddres3)+SPACE(INT((LEN(lcAddres3)-LEN(ALLTRIM(lcAddres3)))/2))   
ELSE
  STORE ' ' TO lcName,lcAddres1,lcAddres2,lcAddres3,lcVendor
ENDIF

=SEEK(Po+cStyMajor,'DECVALST')
lcDate     = DTOC(DECVALST.Date)
lcStylDesc = DECVALST.Desc
lcPattern  = DECVALST.Pattern
lcFibrCont = DECVALST.Fibrecont
RETURN ''

FUNCTION lfwRepWhen