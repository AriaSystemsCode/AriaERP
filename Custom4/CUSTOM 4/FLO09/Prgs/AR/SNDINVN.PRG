*:*********************************************************************************
*: Program file  : SNDINVN.PRG
*: Program desc. : SEND INVOICES TO HILLDUN
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Waleed Hamed (WLD)
*!          Date : 6/19/2007
*:     Entry     : Convert from C#124052-C200762 aria27 to C#200799 Aria4XP
*:*********************************************************************************
*! Passed Parameters  :  
*:*********************************************************************************
*: Modifications :
*! B608910,1 WAM 06/28/2009 Read the template excel file from the X drive [T20090625.0022]
*:*********************************************************************************
PRIVATE lcExpr , lcTmpFile
STORE '' TO lcExpr , lcTmpFile


=gfOpentable(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDRA','SH')
=gfOpentable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpentable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')
=gfOpentable(oAriaApplication.DataDir+'BOL_LIN',oAriaApplication.DataDir+'ORD_BOL','SH')
=gfOpentable(oAriaApplication.DataDir+'CONSINVH',oAriaApplication.DataDir+'CONSINVH','SH')

SELECT INVHDR
=gfseek('')
LOCATE FOR !EMPTY(cFacCode)
IF !FOUND()
      =gfModalGen("INM00000B00000","DIALOG",'','','There is no factored invoice')
     RETURN .F.
ENDIF

lcEscapeKy = ON("KEY","ESC")
*---- Display Option Grid
lcExpr     = gfOpGrid('SNDINVN' , .T.) &&,.F.,.F.,.T.,.T.)
IF LCEXPR='.F.'
  RETURN
ENDIF

ON KEY LABEL ESCAPE &lcEscapeKy
lcTmpFile  = gfTempName()

*---- Create Temp File
=lfCreatTmp()
*---- Collect Data
=lfColData()
IF RECCOUNT(lcTmpFile) > 0
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
  RETURN
ENDIF

WAIT WINDOW NOWAIT 'File created, pls check the Excel application.'
*---- Create XLS Output File 
=lfCreatXLS()


*!***************************************************************************
*! Name      : lfCreatTmp
*! Developer : Waleed Hamed (WLD)
*! Date      : 6/19/2007
*! Purpose   : Create the Temp files that will use to print the XLS File
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCreatTmp()
*!***************************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) (Account C(5) , Name C(30) , Address1 C(30) ,;
              Address2 C(30) , City C(17) , State C(2) , Zip C(5) , Store C(8) ,; 
              Invoice C(6) , InvDate D(8) , InvAmt N(10,2) , CTerms C(6) , ;
              TermDesc C(30) , CustPo C(15),SHIPVIA C(6),SHIPVIADSC C(30),BOL_NO C(6))
INDEX ON Invoice TAG &lcTmpFile

*--End of Function lfCreatTmp.

*!*************************************************************
*! Name      : lfColData
*! Developer : Waleed Hamed (WLD)
*! Date      : 6/19/2007
*! Purpose   : Collecting Data.
*!*************************************************************
*! Calls     : from the Prg.
*!*************************************************************
*! Example   :  =lfColData()
*!*************************************************************
FUNCTION lfColData

PRIVATE lcCustomer , lcTermDesc
STORE '' TO lcCustomer , lcTermDesc
WAIT 'Creating outbound Invoice file...' WINDOW NOWAIT
*-- First write Invoice records
SELECT INVHDR
SCAN FOR &lcExpr AND !EMPTY(cFacCode)
  lcTermDesc = IIF(gfSEEK('N' + INVHDR.CTermCode + 'N' + 'CTERMCODE ','CODES'),CODES.Cdiscrep,'')
  *Adjust the terms as the factor needs
  IF !EMPTY(lcTermDesc)
    lnPos = AT('NET',UPPER(lcTermDesc))
    DO CASE
    CASE lnPos>0
      lcTermDesc = SUBSTR(lcTermDesc,lnPos,6)
    CASE '10'$lcTermDesc
      lcTermDesc = '10'
    OTHERWISE
      lcTermDesc = ''
    ENDCASE    
  ENDIF
  lcCustomer = Account
  IF gfSEEK('M'+lcCustomer,'Customer') 
    SELECT (lcTmpFile)
    IF !SEEK(INVHDR.Invoice)
      
      *Get Bol# if any
      lcBol = lfGetBol()

      APPEND BLANK
      REPLACE Account  WITH INVHDR.Account				 ,;
              Name     WITH CUSTOMER.bTname         	 ,;
              Address1 WITH CUSTOMER.cAddress12			 ,;
              Address2 WITH CUSTOMER.cAddress22			 ,;
              City     WITH CUSTOMER.cAddress32			 ,;
              State    WITH CUSTOMER.cAddress42			 ,;
              Zip      WITH CUSTOMER.cAddress52			 ,;
              Store    WITH INVHDR.STORE            	 ,;
              Invoice  WITH INVHDR.INVOICE          	 ,;
              InvDate  WITH INVHDR.INVDATE          	 ,;
			  InvAmt   WITH INVHDR.TOTALCHG              ,;
              CTerms   WITH INVHDR.CTermCode             ,;
              TermDesc WITH ALLTRIM(lcTermDesc)          ,;
              CustPo   WITH INVHDR.CUSTPO
      
      *Update the ship via
      REPLACE SHIPVIA     WITH INVHDR.SHIPVIA   ;
              SHIPVIADSC  WITH gfCodDes(INVHDR.SHIPVIA,'SHIPVIA') ;
              BOL_NO      WITH lcBol
      
    ENDIF
  ENDIF
ENDSCAN
*--End of Function lfColData.
*:**************************************************************************
*:* Name        : lfGetBol
*:* Developer   : Waleed Hamed (WLD)
*:* Date        : 6/19/2007
*:* Purpose     : Get Bol# if any
*:***************************************************************************
FUNCTION lfGetBol
PRIVATE lcBol,lnSlct
lnSlct = SELECT()
lcBol = ' '
IF !EMPTY(INVHDR.ORDER) 
  IF gfSEEK(INVHDR.ORDER,'BOL_LIN')
    lcBol = BOL_LIN.BOL_NO
  ENDIF
ELSE
  IF gfSEEK(INVHDR.INVOICE+INVHDR.STORE,'CONSINVH') .AND. gfSEEK(CONSINVH.ORDER,'BOL_LIN')
    lcBol = BOL_LIN.BOL_NO  
  ENDIF    
ENDIF
SELECT (lnSlct)
RETURN lcBol
*-- end of lfGetBol.
*:**************************************************************************
*:* Name        : lfCreatXLS
*:* Developer   : Waleed Hamed (WLD)
*:* Date        : 6/19/2007
*:* Purpose     : Add invoice data at XLS file SendInv.xls
*:***************************************************************************
FUNCTION lfCreatXLS
PARAMETERS lcMemfile


WAIT WINDOW NOWAIT 'Opening the Excel Tamplete file'
oObj = GETOBJECT('','excel.sheet')
oXls = oObj.Application

*B608910,1 WAM 06/28/2009 Read the template excel file from the X drive
*oXls.Workbooks.Add(oAriaApplication.Defaultpath+'SendInv.xls')
IF oAriaApplication.MULTIINST 
  oXls.Workbooks.Add('X:\ARIA4XP\SendInv.xls')
ELSE
  oXls.Workbooks.Add(oAriaApplication.Defaultpath+'SendInv.xls')
ENDIF
*B608910,1 WAM 06/28/2009 (End)

*- Open the temp file created to be sent
ROW = 2
WITH oXls

SELECT (lcTmpFile)
SCAN
  WAIT WINDOW NOWAIT &lcTmpFile..INVOICE     
  .Cells(Row,1).Value = &lcTmpFile..INVOICE     && InvNum
  .Cells(Row,2).Value = &lcTmpFile..INVAMT      && InvAmt
  .Cells(Row,3).Value = &lcTmpFile..INVDATE     && InvDate
  .Cells(Row,4).Value = &lcTmpFile..TERMDESC    && InvTerms
  .Cells(Row,5).Value = &lcTmpFile..ACCOUNT     && CustCode
  .Cells(Row,6).Value = &lcTmpFile..NAME        && CustName
  .Cells(Row,7).Value = &lcTmpFile..ADDRESS1    && CustAdd1
  .Cells(Row,8).Value = &lcTmpFile..ADDRESS2    && CustAdd2
  .Cells(Row,9).Value = &lcTmpFile..CITY        && CustCity
  .Cells(Row,10).Value = &lcTmpFile..STATE      && CustState
  .Cells(Row,11).Value = &lcTmpFile..ZIP        && CustZip
  .Cells(Row,12).Value = &lcTmpFile..SHIPVIADSC && Carrier
  .Cells(Row,13).Value = &lcTmpFile..BOL_NO && Tracking
  ROW = ROW + 1
ENDSCAN  
WAIT CLEAR
ENDWITH

oXls.Visible = .T.
RELEASE oXls
 
USE IN &lcTmpFile
ERASE (oAriaApplication.WorkDir+lcTmpFile+'.*')