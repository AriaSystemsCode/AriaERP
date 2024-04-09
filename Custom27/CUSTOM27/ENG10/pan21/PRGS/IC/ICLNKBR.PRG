*!***************************************************************************
*! Program file  : ICLNKBR.PRG
*! Program desc. : Link to bernard Freight for Panache Lingerie (PAN27-Tony-UK)
*! Date          : 03/14/2006
*! System        : Aria Advantage Series.
*! Module        : Inventory Control (IC)
*! Developer     : NADER NABIL (NNA)
*! Tracking Job Number: C130448
*!***************************************************************************
*! Calls : Functions :
*!    
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO ICLNKBR
*!***************************************************************************
*! Modifications :
*!***************************************************************************
PRIVATE lcFilesDir,lcWindTitl,lcSegLine
DIMENSION laDir[1]
STORE '' TO lcFilesDir,lcWindTitl,lcSegLine,laDir
STORE 0 TO lnOutFile
STORE .F. TO llVendor,llproduct,llRemoval,llinitial,llReturns,llExpVnd,llExpPrd,llExpRem,llExpIni,llExpRetn
lcFilesDir = gfGetMemVar('M_BRFILEDR',gcAct_Comp)
IF EMPTY(ALLTRIM(lcFilesDir))
  =gfModalGen("INM00000B00000","DIALOG",'','',"You didn't nominate a directory that files will be saved to . " +;
    "please nominate it in the IC setup first then create it physically on the hard drive")
  RETURN
ELSE
  =ADIR(laDir,lcFilesDir,'D')
  IF EMPTY([laDir])
    =gfModalGen("INM00000B00000","DIALOG",'','',"Your Bernard File Directory not found on the hard disk. please create it " + ;
    "create it physically to continue.")
    RETURN
  ENDIF
ENDIF
IF !lfOpnFiles("APVENDOR,STYLE,INVLINE,INVHDR,CUSTOMER,RETHDR,RETLINE,STYINVJL,POSHDR,FABRIC,MATINVJL",;
               "VENCODE,CSTYLE,INVLINE,INVHDR,CUSTOMER,RETHDR,RETLINE,MFGOPR,POSHDR,FABRIC,MFGOPR")
  RETURN
ENDIF
USE (gcDataDir+'STYINVJL') AGAIN ALIAS STYINVFIFO ORDER TAG Fifo IN 0

lcFilesDir = IIF(INLIST(RIGHT(ALLTRIM(lcFilesDir),1),'\','/'),lcFilesDir,lcFilesDir+'\')
lcWindTitl = "Link to Bernard Freight"

DO (gcScrDir+"IC\"+"ICLNKBR.SPX")
=lfClsFile("APVENDOR,STYLE,INVLINE,CUSTOMER,RETHDR,RETLINE,STYINVJL,POSHDR,STYINVFIFO,SYCINT")
RETURN
*!*************************************************************
*! Name      : lfvChkBox
*! Developer : NADER NABIL
*! Date      : 06/20/2004
*! Purpose   : Function to Unable or Disable Thermo Bars.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvChkBox()
*!*************************************************************
FUNCTION lfvChkBox
PARAMETERS lcThrmNo,llEnable
PRIVATE lcBar,lcPrc
lcBar = 'lcD' + lcThrmNo + 'Thrm'
lcPrc  = 'lcD' + lcThrmNo + 'ThPrc'
IF llEnable
  SHOW GET &lcBar  ENABLE
  SHOW GET &lcPrc  ENABLE
ELSE
  SHOW GET &lcBar  DISABLE
  SHOW GET &lcPrc DISABLE
ENDIF
*--End of lfvChkBox.

*!***************************************************************************
*!* Name        : lfvExport
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Validation function on Ok Button
*!***************************************************************************
*!* Called from : ICLNKBR.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvExport()
*!***************************************************************************
FUNCTION lfvExport
IF !llVendor AND !llproduct AND !llRemoval AND !llinitial AND !llReturns
  =gfModalGen("INM00000B00000","DIALOG",'','',"You didn't select any option to export")
  RETURN
ENDIF
IF llVendor
  = lfVendor()
ENDIF
IF llproduct
  =lfproduct()
ENDIF
IF llRemoval
  =lfRemoval()
ENDIF
IF llinitial
  =lfInitial()
ENDIF
IF llReturns
  =lfReturns()
ENDIF

=lfEndMassg()
*IF llExpVnd OR llExpPrd OR llExpRem OR llExpIni OR llExpRetn
*  =gfModalGen("INM00000B00000","DIALOG",'','',"Exporting completed successfully")
*ELSE
*  =gfModalGen("INM00000B00000","DIALOG",'','',"Exporting failed")
*ENDIF
=lfThrmo(0,1,'1')
=lfThrmo(0,1,'2')
=lfThrmo(0,1,'3')
=lfThrmo(0,1,'4')
=lfThrmo(0,1,'5')
STORE .F. TO llVendor,llproduct,llRemoval,llinitial,llReturns
=lfvChkBox('1',.F.)
=lfvChkBox('2',.F.)
=lfvChkBox('3',.F.)
=lfvChkBox('4',.F.)
=lfvChkBox('5',.F.)
SHOW GETS
RETURN
*-- End of Function lfvExport.
*!***************************************************************************
*!* Name        : lfVendor
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Export Vendor file to Supplieryyyymmddhhnnss.txt file
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfVendor()
*!***************************************************************************
FUNCTION lfVendor
PRIVATE lnRecCount,lnRecNo
STORE '' TO lcSegLine
STORE 0 TO lnRecCount,lnRecNo
lnOutFile = FCREATE(lcFilesDir + 'Vendor.Txt')
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot create a file for exporting of vendor file. Cannot proceed with this option')
  llExpVnd = .F.
  RETURN
ENDIF
SELECT APVENDOR
COUNT FOR lBrVendor TO lnRecCount
IF lnRecCount = 0
  llExpVnd = .F.
ENDIF
SCAN FOR lBrVendor
  lnRecNo = lnRecNo + 1
   =lfThrmo(lnRecNo , lnRecCount ,'1')

  *--Note : we Separate between fields with "+"
  lcSegLine = ''
  *-- Data Code - Always "R05"
  lcSegLine = lcSegLine + "R05"

  *-- Company - Always "PANACH"
  lcSegLine = lcSegLine + "+" +"PANACH"

  *-- Supplier Code  
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(APVENDOR.cVendCode),17,' ')

  *-- Vendor Full Name
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(APVENDOR.cVenComp),40,' ')

  *-- Vendor Country (new user defined field) 
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(APVENDOR.cBrCountry),3,' ')

  *-- Bernard Transport Mode (new user defined field)
  lcSegLine = lcSegLine + "+" + APVENDOR.cBrTrnsMod

  *-- Bernard Payment Term (new user defined field)
  lcSegLine = lcSegLine + "+" + APVENDOR.cBrPayTrms

  *-- Bernard Uplift% (new user defined field)
  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(APVENDOR.nBrUplift,10,2))

  *-- Bernard Supplier Group (new user defined field)
  lcSegLine = lcSegLine + "+" + APVENDOR.cBrSupGrp

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(APVENDOR.cBrUsrfld1),'',APVENDOR.cBrUsrfld1)

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(APVENDOR.cBrUsrfld2),'',APVENDOR.cBrUsrfld2)
  lcSegLine = lcSegLine + "+"
  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
=FFLUSH(lnOutFile)
=FCLOSE(lnOutFile)
llExpVnd = .T.
=lfRename('VENDOR.TXT','SUPPLIER')

*-- End of Function lfVendor.
*!***************************************************************************
*!* Name        : lfproduct
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Export Style file to Productyyyymmddhhnnss.txt file
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfproduct()
*!***************************************************************************
FUNCTION lfproduct
PRIVATE lnRecCount,lnRecNo
STORE '' TO lcSegLine
STORE 0 TO lnRecCount,lnRecNo
lnOutFile = FCREATE(lcFilesDir + 'STYLE.Txt')
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot create a file for exporting product information. Cannot proceed with this option')
  llExpPrd = .F.
  RETURN
ENDIF
SELECT STYLE
SET ORDER TO TAG CSTYLE
COUNT TO lnRecCount
IF lnRecCount = 0
  llExpPrd = .F.
ENDIF
SCAN 
  lnRecNo = lnRecNo + 1
   =lfThrmo(lnRecNo , lnRecCount ,'2')

  *--Note : we Separate between fields with "+"
  lcSegLine = ''
  *-- Data Code - Always "R18"
  lcSegLine = lcSegLine + "R18"

  *-- Company - Always "PANACH"
  lcSegLine = lcSegLine + "+" +"PANACH"

  *-- Product Code  
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(STYLE.CSTYMAJOR),24,' ')

  *-- Product Description.
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(STYLE.DESC),40,' ')

  *-- Product Group (new user defined field) 
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(Style.cBrStyGrp),4,' ')

  *-- Default Nett.
  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(Style.nStyweight,5,2))

  *-- Stat Qty 2 (Always set to 1)
  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(1))

  *-- Commodity
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(STYLE.cComCode),22,' ')

  *-- Vat Rate ID (Always set to 'S')
  lcSegLine = lcSegLine + "+" + 'S'

  *-- %IPR (Leave Blank)
  lcSegLine = lcSegLine + "+" + ''

  *-- IPR Multiplier (Leave Blank).
  lcSegLine = lcSegLine + "+" + ''

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(Style.cBrUsrfld1),'',Style.cBrUsrfld1)

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(Style.cBrUsrfld2),'',Style.cBrUsrfld2)
  lcSegLine = lcSegLine + "+"
  
  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
=FFLUSH(lnOutFile)
=FCLOSE(lnOutFile)
llExpPrd = .T.
=lfRename('STYLE.TXT','PRODUCT')
*-- End of Function lfproduct.
*!***************************************************************************
*!* Name        : lfRemoval
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Export Invline file to Removeyyyymmddhhnnss.txt file
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRemoval()
*!***************************************************************************
FUNCTION lfRemoval

PRIVATE lnRecCount,lnRecNo,lcTmpHdr,lcTmpLine,lcCountOrg,lnInvLnNo,lcOldInvNo,lcOpr_Auth
STORE '' TO lcSegLine,lcTmpHdr,lcTmpLine,lcCountOrg,lcOldInvNo,lcOpr_Auth
STORE 0 TO lnRecCount,lnRecNo,lnInvLnNo

lcTmpHdr  = gfTempName()
lcTmpLine = gfTempName()

CREATE TABLE (gcWorkDir+lcTmpHdr)(Invoice C(6),REM_DATE D(10),COUNTRY C(2),SHP_DATE D(10),USRFLD1 C(24),USRFLD2 C(24))
INDEX ON (INVOICE) TAG lcTmpHdr OF lcTmpHdr

CREATE TABLE (gcWorkDir+lcTmpLine)(Invoice C(6),lineNo C(5),Style C(19),Qty N(10),USRFLD1 C(24),USRFLD2 C(24),OPR_AUTH C(4))
INDEX ON (INVOICE+STYLE+LINENO) TAG lcTmpLine OF lcTmpLine

IF !USED('SYCINT')
  =gfOpenFile(gcSysHome+'SYCINT','CCONTCODE'),'SH')
ENDIF

SET ORDER TO TAG STYLE IN STYLE

SELECT APVENDOR
SET RELATION OFF INTO SYCINT
SET RELATION TO Apvendor.ccont_code INTO Sycint ADDITIVE

SELECT INVLINE
SET RELATION TO
SET RELATION TO IIF(EMPTY(INVLINE.STORE),'M'+INVLINE.ACCOUNT,'S'+INVLINE.ACCOUNT+INVLINE.STORE) INTO Customer ADDITIVE
SET RELATION TO Style INTO Style ADDITIVE

SELECT INVHDR
COUNT FOR INVHDR.STATUS<>'V' TO lnRecCount
SET RELATION TO
SET RELATION TO INVOICE INTO INVLINE
IF lnRecCount = 0
  llExpRem = .F.
ENDIF
SCAN FOR STATUS<>'V'
  SELECT INVLINE
  SCAN REST WHILE INVOICE+STR(LINENO,6) = INVHDR.INVOICE FOR !(INVLINE.LBRSENT) AND !INVLINE.LNQUALIFY
    REPLACE lNQualify WITH .T.
    IF !(INVLINE.INVOICE==lcOldInvNo)
      lcOldInvNo=INVLINE.INVOICE
      lnInvLnNo = 0
    ENDIF
    lcOpr_Auth = ''
    lcCountOrg = CUSTOMER.CBRCOUNTRY
    IF SEEK(INVLINE.INVOICE,'STYINVJL')
      SELECT STYINVJL
      SCAN REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE = INVLINE.INVOICE ;
           FOR STYINVJL.STYLE=INVLINE.STYLE AND !EMPTY(STYINVJL.CRSESSION)
        *--Seek for the receiving Session to get the Purchase Number then the Vendor
        IF SEEK(STYLE+CWARECODE+CDYELOT+CRSESSION,'STYINVFIFO')
          SELECT STYINVFIFO
          SCAN REST WHILE STYLE+CWARECODE+CDYELOT+CRSESSION+CISESSION = STYINVJL.STYLE+STYINVJL.CWARECODE+;
               STYINVJL.CDYELOT+STYINVJL.CRSESSION FOR CTRTYPE='6'
            IF SEEK('P'+STYINVFIFO.CTRCODE,'POSHDR')
              *--Seek for fabrics that issued for the previous PO
              IF SEEK(STYINVFIFO.CTRCODE,'MATINVJL')
                SELECT MATINVJL
                SCAN REST WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode = STYINVFIFO.CTRCODE
                  *--seek for the Matinvjl fabrics to see if its Item_type='000026' or not
                  IF !EMPTY(MATINVJL.CFABRIC) AND SEEK(MATINVJL.CFABRIC,'FABRIC')
                    SELECT FABRIC
                    SCAN REST WHILE Fabric+Color = MATINVJL.CFABRIC
                      IF FABRIC.ITEM_TYPE =='000026'
                        lcOpr_Auth = "OPR8"
                        EXIT
                      ENDIF
                    ENDSCAN
                  ENDIF
                ENDSCAN
              ENDIF
              IF SEEK(POSHDR.VENDOR,'APVENDOR') AND !SYCINT.LEUROPCOM AND APVENDOR.cCont_Code<>'GB'
                IF !SEEK(INVLINE.INVOICE,lcTmpHdr)
                  SELECT (lcTmpHdr)
                  APPEND BLANK
                  REPLACE INVOICE  WITH INVLINE.INVOICE    ,;
                          REM_DATE WITH INVLINE.INVDATE    ,;
                          COUNTRY  WITH lcCountOrg         ,;
                          SHP_DATE WITH INVLINE.INVDATE    ,;
                          USRFLD1  WITH INVLINE.CBRUSRFLD1 ,;
                          USRFLD2  WITH INVLINE.CBRUSRFLD2
                ENDIF
                SELECT INVLINE
                REPLACE lBrSent WITH .T.,lNQualify WITH .F.
                *--Update the Detail file
                SELECT(lcTmpLine)
                IF !SEEK(INVLINE.INVOICE+STYLE.CSTYMAJOR)
                  APPEND BLANK
                  lnInvLnNo = lnInvLnNo + 1
                  REPLACE INVOICE  WITH INVLINE.INVOICE                ,;
                          LINENO   WITH ALLTRIM(STR(lnInvLnNo))        ,;
                          QTY      WITH ABS(STYINVJL.NTOTSTK)          ,;
                          STYLE    WITH STYLE.CSTYMAJOR                ,;
                          USRFLD1  WITH INVLINE.CBRUSRFLD1             ,;
                          USRFLD2  WITH INVLINE.CBRUSRFLD2             ,;
                          OPR_AUTH WITH lcOpr_Auth
                ELSE
                  REPLACE QTY      WITH Qty+ABS(STYINVJL.NTOTSTK)
                ENDIF
              ENDIF
            ENDIF                 
          ENDSCAN
        ENDIF
      ENDSCAN       
    ENDIF
  ENDSCAN
  lnRecNo = lnRecNo + 1
  =lfThrmo(lnRecNo , lnRecCount ,'3')
ENDSCAN

IF RECCOUNT(lcTmpHdr)=0 AND RECCOUNT(lcTmpLine)=0
  =lfThrmo(100 , 100 ,'3')
  RETURN
ELSE
  lnOutFile = FCREATE(lcFilesDir + 'Remove.Txt')
  IF lnOutFile < 0
    =gfModalGen('TRM00000B00000','ALERT','','','Cannot create a file for Removal of goods from the warehouse via invoicing. Cannot proceed with this option')
    llExpRem = .F.
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpHdr)
SET RELATION OFF INTO (lcTmpLine)
SET RELATION TO INVOICE INTO (lcTmpLine) ADDITIVE
SCAN
  *--Note : we Separate between fields with "+"
  lcSegLine = ''
  *-- Data Code - Always "R60"
  lcSegLine = lcSegLine + "R60"

  *-- Company - Always "PANACH"
  lcSegLine = lcSegLine + "+" +"PANACH"

  *-- Transaction No.
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(&lcTmpHdr..INVOICE),15,' ')

  *-- REMOVAL DATE
  lcSegLine = lcSegLine + "+" + DTOC(&lcTmpHdr..REM_DATE)

  *-- Destination Country 
  lcSegLine = lcSegLine + "+" + &lcTmpHdr..COUNTRY

  *-- Shipment Date
  lcSegLine = lcSegLine + "+" + DTOC(&lcTmpHdr..SHP_DATE)

  *-- Transport Mode (Always '30')
  lcSegLine = lcSegLine + "+" + '30'

  *-- Bernard User Field1
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(&lcTmpHdr..USRFLD1),'',&lcTmpHdr..USRFLD1)

  *-- Bernard User Field2
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(&lcTmpHdr..USRFLD2),'',&lcTmpHdr..USRFLD2)
  lcSegLine = lcSegLine + "+"
  = FPUTS(lnOutFile,lcSegLine)
  IF SEEK(&lcTmpHdr..INVOICE,lcTmpLine)
    SELECT(lcTmpLine)
    SCAN REST WHILE INVOICE+STYLE+LINENO = &lcTmpHdr..INVOICE
	  lcSegLine = ''
	  *-- Data Code - Always "R61"
	  lcSegLine = lcSegLine + "R61"
	
	  *-- Company - Always "PANACH"
	  lcSegLine = lcSegLine + "+" +"PANACH"
	
	  *-- Transaction No.
	  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(&lcTmpLine..INVOICE),15,' ')

	  *-- Transaction Line.
	  lcSegLine = lcSegLine + "+" + PADL(ALLTRIM(&lcTmpLine..LINENO),5,'0')

	  *-- REMOVAL Qty
	  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(&lcTmpLine..Qty))

	  *-- Product Ref.
	  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(&lcTmpLine..Style),24,' ')

	  *-- Community Transit doc No (Leave Blank)
	  lcSegLine = lcSegLine + "+" + ''

	  *-- Bernard User Field1
	  lcSegLine = lcSegLine + "+" + IIF(EMPTY(&lcTmpHdr..USRFLD1),'',&lcTmpHdr..USRFLD1)

	  *-- Bernard User Field2
	  lcSegLine = lcSegLine + "+" + IIF(EMPTY(&lcTmpHdr..USRFLD2),'',&lcTmpHdr..USRFLD2)

	  *-- OPR Authorisation
	  lcSegLine = lcSegLine + "+" + &lcTmpLine..OPR_AUTH
      lcSegLine = lcSegLine + "+"
	  = FPUTS(lnOutFile,lcSegLine)
    ENDSCAN
  ENDIF
ENDSCAN
=FFLUSH(lnOutFile)
=FCLOSE(lnOutFile)
llExpRem = .T.
=lfRename('REMOVE.TXT','REMOVE')
*-- End of Function lfRemoval.
*!***************************************************************************
*!* Name        : lfInitial
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Export Invline file to Removeyyyymmddhhnnss.txt file
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfInitial()
*!***************************************************************************
FUNCTION lfInitial
PRIVATE lnRecCount,lnRecNo
STORE '' TO lcSegLine
STORE 0 TO lnRecCount,lnRecNo
lnOutFile = FCREATE(lcFilesDir + 'INITIAL.Txt')
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot create a file Initial input of inventory figures. Cannot proceed with this option')
  llExpIni = .F.
  RETURN
ENDIF
SELECT STYLE
SET ORDER TO TAG STYLE
SELECT Style.cstymajor, SUM(Style.TOTSTK) AS TOTQTY, Style.cbrusrfld1,Style.cbrusrfld2;
 FROM Style;
 INTO CURSOR STYCURSOR   ;
 GROUP BY Style.cstymajor;
 HAVING SUM(Style.totstk) <> 0;
 ORDER BY Style.cstymajor

SELECT STYCURSOR
COUNT TO lnRecCount
IF lnRecCount = 0
  llExpIni = .F.
ENDIF
SCAN 
  lnRecNo = lnRecNo + 1
   =lfThrmo(lnRecNo , lnRecCount ,'4')

  *--Note : we Separate between fields with "+"
  lcSegLine = ''
  *-- Data Code - Always "R97"
  lcSegLine = lcSegLine + "R97"

  *-- Company - Always "PANACH"
  lcSegLine = lcSegLine + "+" +"PANACH"

  *-- Product Reference --> Style.cStyMajor
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(CSTYMAJOR),24,' ')

  *-- Supplier Code (Always 'FCS')
  lcSegLine = lcSegLine + "+" + PADR('FCS',17,' ')

  *-- Receipt Date (System Date)
  lcSegLine = lcSegLine + "+" + DTOC(gdSysDate)

  *-- Invoice Number (System Date)
  lcSegLine = lcSegLine + "+" + DTOC(gdSysDate)

  *-- Invoice Line (Always set to 1)
  lcSegLine = lcSegLine + "+" + PADL(ALLTRIM(STR(1)),5,'0')

  *-- Receipt Qty
  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(TOTQTY))

  *-- Country of Origin (Always set to 'GB')
  lcSegLine = lcSegLine + "+" + 'GB'

  *-- Bernard User Field1 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(cBrUsrfld1),'',cBrUsrfld1)

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(cBrUsrfld2),'',cBrUsrfld2)
  lcSegLine = lcSegLine + "+"
  
  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
=FFLUSH(lnOutFile)
=FCLOSE(lnOutFile)
llExpIni = .T.
=lfRename('INITIAL.TXT','STOCK')
*-- End of Function lfInitial.
*!***************************************************************************
*!* Name        : lfReturns
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Export RetHdr file to Stockyyyymmddhhnnss.txt file
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfReturns()
*!***************************************************************************
FUNCTION lfReturns
PRIVATE lnRecCount,lnRecNo,lcStyMajor,lcTmpRtrn,lcSupCode,lcCountOrg
STORE '' TO lcSegLine,lcStyMajor,lcTmpRtrn,lcSupCode,lcCountOrg
STORE 0 TO lnRecCount,lnRecNo
lcTmpRtrn = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpRtrn)(cRmemo C(6),STYLE C(19),CRET_LINNO C(5),SUP_CODE C(17),RCPT_DATE D(10),RCPT_QTY N(10),COUNTRY C(2),;
    USRFLD1 C(24),USRFLD2 C(24))
INDEX ON (cRmemo+STYLE) TAG lcTmpRtrn OF lcTmpRtrn

IF !USED('SYCINT')
  =gfOpenFile(gcSysHome+'SYCINT','CCONTCODE'),'SH')
ENDIF
SET ORDER TO TAG STYLE IN STYLE

SELECT CUSTOMER
SET RELATION OFF INTO SYCINT
SET RELATION TO Customer.ccont_code INTO Sycint ADDITIVE

SELECT RETLINE
SET RELATION TO
SET RELATION TO Style INTO Style ADDITIVE

SELECT RETHDR
SET RELATION TO
SET RELATION TO Rethdr.crmemo INTO Retline ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO Customer ADDITIVE

COUNT TO lnRecCount FOR !lBrSent AND STATUS<>'V' AND SYCINT.LEUROPCOM 
LOCATE
IF lnRecCount = 0
  llExpRetn = .F.
ENDIF
SCAN FOR !lBrSent AND STATUS<>'V' AND SYCINT.LEUROPCOM
  lnRecNo = lnRecNo + 1
  =lfThrmo(lnRecNo , lnRecCount ,'5')
  IF SEEK(RETHDR.CRMEMO,'RETLINE')
    SELECT RETLINE
    SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = RETHDR.CRMEMO FOR Style.linvsty AND INLIST(CSTYGRADE,'1','2')
      STORE '' TO lcCountOrg,lcSupCode
      *--IF the credit memo is raised against an invoice (Entire Invoice=yes)
      IF RETHDR.CINTR_INV='Y'
        *--Seek for the credit memo invocie at the styinvjl file to get receiving session for the style in this invoice        
        IF !EMPTY(RETLINE.INVOICE) AND SEEK(RETLINE.INVOICE,'STYINVJL')
          SELECT STYINVJL
          SCAN REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE = RETLINE.INVOICE ;
               FOR STYINVJL.STYLE=RETLINE.STYLE AND !EMPTY(STYINVJL.CRSESSION)
            *--Seek for the receiving Session to get the Purchase Number then the Vendor
            IF SEEK(STYLE+CWARECODE+CDYELOT+CRSESSION,'STYINVFIFO')
              SELECT STYINVFIFO
              SCAN REST WHILE STYLE+CWARECODE+CDYELOT+CRSESSION+CISESSION = STYINVJL.STYLE+STYINVJL.CWARECODE+;
                   STYINVJL.CDYELOT+STYINVJL.CRSESSION FOR CTRTYPE='6'
                IF SEEK('P'+STYINVFIFO.CTRCODE,'POSHDR')
                  IF SEEK(POSHDR.VENDOR,'APVENDOR')
                    lcSupCode  = APVENDOR.CVendCode
                    lcCountOrg = SUBSTR(APVENDOR.CBRCOUNTRY,1,2)
                  ENDIF
                ENDIF                 
              ENDSCAN
            ENDIF
          ENDSCAN       
        ENDIF
      ENDIF
      *--Update Temp file with the collected Data.
      SELECT (lcTmpRtrn)
      IF !SEEK(RETLINE.cRmemo+STYLE.CSTYMAJOR)
        APPEND BLANK
        REPLACE cRmemo     WITH RETLINE.CRMEMO    ,;
                STYLE      WITH STYLE.CSTYMAJOR   ,;
                CRET_LINNO WITH RETLINE.CRET_LINNO,;
                SUP_CODE   WITH IIF(EMPTY(lcSupCode),'GB',lcSupCode) ,;
                RCPT_DATE  WITH RETHDR.CRDATE     ,;
                RCPT_QTY   WITH RETLINE.TOTQTY    ,;
                COUNTRY    WITH IIF(EMPTY(lcCountOrg),'GB',lcCountOrg) ,;
                USRFLD1    WITH RETHDR.CBRUSRFLD1 ,;
                USRFLD2    WITH RETHDR.CBRUSRFLD2 
      ELSE
        REPLACE RCPT_QTY  WITH RCPT_QTY+RETLINE.TOTQTY   
      ENDIF
    ENDSCAN
  ENDIF
  SELECT RETHDR
  REPLACE lBrSent WITH .T.
ENDSCAN
IF lnRecNo = 0 
  =lfThrmo(100 , 100 ,'5')
  Return
ELSE
  lnOutFile = FCREATE(lcFilesDir + 'Return.Txt')
  IF lnOutFile < 0
    =gfModalGen('TRM00000B00000','ALERT','','','Cannot create a file for Returns of inventory via Credit Memos. Cannot proceed with this option')
    llExpRetn = .F.
    RETURN
  ENDIF
ENDIF
SELECT(lcTmpRtrn)
SCAN
  *--Note : we Separate between fields with "+"
  lcSegLine = ''
  *-- Data Code - Always "R97"
  lcSegLine = lcSegLine + "R97"

  *-- Company - Always "PANACH"
  lcSegLine = lcSegLine + "+" +"PANACH"

  *-- Product Code (Ref)
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(STYLE),24,' ')

  *-- Supplier Code 
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(Sup_Code),17,' ')

  *-- Receipt Date 
  lcSegLine = lcSegLine + "+" + DTOC(RCPT_DATE)

  *-- Invoice Number
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(CRMEMO),17,' ')

  *-- Invoice Line (Always set to 1)
  lcSegLine = lcSegLine + "+" + PADL(ALLTRIM(CRET_LINNO),5,'0')

  *-- Receipt Qty 
  lcSegLine = lcSegLine + "+" + ALLTRIM(STR(RCPT_QTY))

  *-- Country of Origin
  lcSegLine = lcSegLine + "+" + PADR(ALLTRIM(COUNTRY),2,' ')

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(UsrFld1),'',Usrfld1)

  *-- Bernard User Field2 (new user defined field)
  lcSegLine = lcSegLine + "+" + IIF(EMPTY(UsrFld2),'',UsrFld2)
  lcSegLine = lcSegLine + "+"

  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
=FFLUSH(lnOutFile)
=FCLOSE(lnOutFile)
llExpRetn = .T.
*--Wait 5 second to get a difference in nominating the previous file in the initial Option and the next file
WAIT WINDOW 'Please wait...' TIMEOUT 2
=lfRename('Return.TXT','STOCK')
IF USED(lcTmpRtrn)
  USE IN (lcTmpRtrn)
  ERASE (gcWorkDir+lcTmpRtrn+'.DBF')
  ERASE (gcWorkDir+lcTmpRtrn+'.CDX')
ENDIF
*-- End of Function lfReturns.
*!***************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Open the Needed files 
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
FUNCTION lfOpnFiles
PARAMETERS lcFilesExp,lcTages
DIMENSION laOpnFiles[1],laOpnTages[1]
=GFSUBSTR(lcFilesExp,@laOpnFiles,',')
=GFSUBSTR(lcTages,@laOpnTages,',')
FOR I=1 TO ALEN(laOpnFiles,1)
  IF !USED(laOpnFiles[I])
    =gfOpenFile(gcDataDir+laOpnFiles[I],laOpnTages[I],'SH')
  ELSE
    SET ORDER TO TAG laOpnTages[I] IN laOpnFiles[I]
  ENDIF
ENDFOR
RETURN .T.
*-- End of Function lfOpnFiles.
*!***************************************************************************
*!* Name        : lfClsFile
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : close the opened files.
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfClsFile()
*!***************************************************************************
FUNCTION lfClsFile
PARAMETERS lcFilesExp
DIMENSION laClsFiles[1]
=GFSUBSTR(lcFilesExp,@laClsFiles,',')
FOR I=1 TO ALEN(laClsFiles,1)
  IF USED(laClsFiles[I])
    USE IN (laClsFiles[I])
  ENDIF
ENDFOR

*-- End of Function lfClsFile.
*:*************************************************************
*: Name      : lfThrmo
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/10/2003
*: Purpose   : Thermometers.
*:*************************************************************
FUNCTION lfThrmo
PARAMETERS lnCurrRc,lnMaxum,lcTrmNo

PRIVATE lcThrm,lcPrc
lcThrm = 'lcD' + lcTrmNo + 'Thrm'
lcPrc  = 'lcD' + lcTrmNo + 'ThPrc'

&lcThrm. = REPLICATE(CHR(219),CEILING((lnCurrRc*45)/lnMaxum))
&lcPrc.  = ALLTRIM(STR((lnCurrRc/lnMaxum)*100,3))+'%'
SHOW GET &lcThrm.
SHOW GET &lcPrc.
*!*************************************************************
*! Name        : lfRename
*! Developer   : NNA - NADER NABIL ABD-ALMONAM
*! Date        : 03/14/2006
*! Purpose     : Rename the Text file in the VFP with long name
*!*************************************************************
*! Called from : Iclnkbr.prg
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRename()
*!*************************************************************
FUNCTION lfRename
PARAMETERS lcOldFile,lcNewFile
STORE '' TO lcDate,lcTime
WAIT WINDOW NOWAIT 'Please wait...'
lcOldFile = lcFilesDir + lcOldFile
*--lcDate = YEAR + MONTH + DAY (20060314)
lcDate = STR(YEAR(DATE()),4) + PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0') + PADL(ALLTRIM(STR(DAY(DATE()))),2,'0')
*--lcTime = Hour + minute + Seconds (113022)
lcTime = substr(time(),1,2) + substr(time(),4,2) + substr(time(),7,2)
*--lcNewFile = lcNewFile + lcDate + lcTime (
lcNewFile = lcFilesDir+lcNewFile + lcDate + lcTime + ".TXT"
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcAppHome+'IC\'+[ICVLNKBR.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
WAIT CLEAR
*-- End of Function lfRename.

*!***************************************************************************
*!* Name        : lfEndMassg
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 03/14/2006
*!* Purpose     : Display a massage after the process finished.
*!***************************************************************************
*!* Called from : ICLNKBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfEndMassg()
*!***************************************************************************
FUNCTION lfEndMassg
STORE '' TO lcMassage1,lcMassage2

lcMassage1 = "Files created : "
lcMassage2 = "Files not created: "
IF llVendor 
  IF llExpVnd
    lcMassage1 = lcMassage1 + 'Vendors, '
  ELSE
    lcMassage2 = lcMassage2 + 'Vendors, '
  ENDIF
ENDIF

IF llproduct 
  IF llExpPrd
    lcMassage1 = lcMassage1 + 'Products, '
  ELSE
    lcMassage2 = lcMassage2 + 'Products, '
  ENDIF
ENDIF

IF llRemoval 
  IF llExpRem
    lcMassage1 = lcMassage1 + 'Removal of Goods, '
  ELSE
    lcMassage2 = lcMassage2 + 'Removal of Goods, '
  ENDIF
ENDIF

IF llinitial 
  IF llExpIni
    lcMassage1 = lcMassage1 + 'Initial Inventory, '
  ELSE
    lcMassage2 = lcMassage2 + 'Initial Inventory, '
  ENDIF
ENDIF

IF llReturns 
  IF llExpRetn
    lcMassage1 = lcMassage1 + 'Returns via Credit Memos'
  ELSE
    lcMassage2 = lcMassage2 + 'Returns via Credit Memos'
  ENDIF
ENDIF

IF LEN(lcMassage1)<=16
  lcMassage1 = ''
ENDIF
IF LEN(lcMassage2)<=19
  lcMassage2 = ''
ENDIF
IF !EMPTY(lcMassage1)
  lcMassage2 = CHR(10)+lcMassage2
ENDIF
IF !EMPTY(lcMassage1) OR !EMPTY(lcMassage2)
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMassage1+lcMassage2)
ENDIF
RETURN
*-- End of Function lfEndMassg.

