*!***************************************************************************
*! Program file  : SO2CSV.PRG
*! Program desc. : Export SO Orders Information to CSV file for LIN20
*! Date          : 03/25/2005
*! System        : Aria Advantage Series.
*! Module        : SALES ORDERS (SO)
*! Developer     : NADER NABIL (NNA)
*! Tracking Job Number: C126963
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO SO2CSV
*!***************************************************************************
*! Modifications :
*!***************************************************************************
PRIVATE lcExpr , lcTmpFile , lcOutFile,lnOutFile
STORE '' TO lcExpr , lcTmpFile , lcOutFile
STORE 0 TO lnOutFile
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH') 
ENDIF
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH') 
ENDIF
IF !USED('CONTACT')
  = gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH') 
ENDIF
IF !USED('CODES')
  = gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH') 
ENDIF
IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH') 
ENDIF

lcEscapeKy = ON("KEY","ESC")
lcExpr     = gfOpGrid('SO2CSV' , .T.)

IF EMPTY(LCEXPR)
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ENDIF
IF EMPTY(lcOutFile)
  =gfModalGen("INM00000B00000","DIALOG",'','','There is no Output file selected')
  RETURN
ENDIF
ON KEY LABEL ESCAPE &lcEscapeKy
lcTmpFile  = gfTempName()

=lfCreatTmp()
=lfColData()
IF RECCOUNT(lcTmpFile) > 0
  =lfCsvRec()
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ENDIF
=FFLUSH(lcOutFile)
=FCLOSE(lnOutFile)

*-Message : 000000
*-Output file has been created
*-Button : 00000
*-Ok
=gfModalGen('TRM00000B00000','ALERT','','','Ascii .CSV file '+lcOutFile+' has been created.')
IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF USED('CONTACT')
  USE IN CONTACT
ENDIF
IF USED('CODES')
  USE IN CODES
ENDIF

*!***************************************************************************
*! Name      : lfCreatTmp
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Create the Temp files that will use to print the Text File
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCreatTmp()
*!***************************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (gcWorkDir+lcTmpFile) (ORDER C(6),PIKTKT C(6),Account C(5) , Name C(30) ,;
              Contact C(30),Address1 C(30) , Address2 C(30) , City C(17) , State C(2),;
              Zip C(5),TermDesc C(30),ShipVia C(30))
INDEX ON (ORDER+PIKTKT) TAG (lcTmpFile)  OF (lcTmpFile)

*--End of Function lfCreatTmp.
*!*************************************************************
*! Name      : lfColData
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Collecting Data.
*!*************************************************************
*! Calls     : SO2CSV.PRG
*!*************************************************************
*! Example   :  =lfColData()
*!*************************************************************
FUNCTION lfColData
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file

lnOutFile = FCREATE(lcOutFile)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
=lfWriteOrd()

*--End of Function lfColData.
*!*************************************************************
*! Name      : lfWriteOrd
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Write an output Order text file with ext. CSV
*!*************************************************************
*! Called from : SO2CSV.PRG
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWriteOrd
PRIVATE lcCustomer,lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5
STORE '' TO lcCustomer,lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,;
            lcTerms , lcShipVia
WAIT 'Creating outbound Orders file...' WINDOW NOWAIT
*-- First write Invoice records
SELECT CUSTOMER
SET RELATION TO
SET RELATION TO 'C'+ Customer.account+ Customer.store INTO Contact ADDITIVE
SELECT ORDHDR
SET RELATION TO
SET RELATION TO 'M' + ORDHDR.ACCOUNT INTO CUSTOMER ADDITIVE
SET RELATION TO Ordhdr.order INTO Piktkt ADDITIVE
SCAN FOR &lcExpr 
lcCustomer = Account
lcShipVia  = gfCodDes(OrdHdr.ShipVia , 'SHIPVIA')
lcTerms    = gfCodDes(OrdHdr.cTermCode , 'CTERMCODE')
  IF SEEK('M'+lcCustomer,'Customer') 
    lcAddress1 = STRTRAN(CUSTOMER.cAddress1, ',', '-')
    lcAddress2 = STRTRAN(CUSTOMER.cAddress2, ',', '-')
    lcAddress3 = STRTRAN(CUSTOMER.cAddress3, ',', '-')
    lcAddress4 = STRTRAN(CUSTOMER.cAddress4, ',', '-')
    lcAddress5 = STRTRAN(CUSTOMER.cAddress5, ',', '-')
    IF SEEK(OrdHdr.Order,'PIKTKT')
      SELECT PIKTKT
      SCAN REST WHILE Order+PikTkt = OrdHdr.Order FOR STATUS<>'X'
        SELECT (lcTmpFile)
        IF !SEEK(ORDHDR.ORDER+PIKTKT.PIKTKT)
          APPEND BLANK
          REPLACE  Order    WITH ORDHDR.ORDER               ,;          	 
                   PIKTKT   WITH PIKTKT.PIKTKT              ,;
                   Account  WITH ORDHDR.Account             ,;
                   Name     WITH CUSTOMER.STname            ,;
                   Contact  WITH Contact.Contact            ,;
                   Address1 WITH lcAddress1                 ,;
                   Address2 WITH lcAddress2                 ,;
                   City     WITH lcAddress3                 ,;
                   State    WITH lcAddress4                 ,;
                   Zip      WITH lcAddress5                 ,;
                   TermDesc WITH lcTerms                    ,;
                   ShipVia  WITH lcShipVia 
        ENDIF
      ENDSCAN        
    ELSE
      SELECT (lcTmpFile)
      IF !SEEK(ORDHDR.ORDER)
        APPEND BLANK
        REPLACE  Order    WITH ORDHDR.ORDER               ,;          	 
                 Account  WITH ORDHDR.Account             ,;
                 Name     WITH CUSTOMER.STname            ,;
                 Contact  WITH Contact.Contact            ,;
                 Address1 WITH lcAddress1                 ,;
                 Address2 WITH lcAddress2                 ,;
                 City     WITH lcAddress3                 ,;
                 State    WITH lcAddress4                 ,;
                 Zip      WITH lcAddress5                 ,;
                 TermDesc WITH lcTerms                    ,;
                 ShipVia  WITH lcShipVia 
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
*--End of Function lfWriteOrd.
*!*************************************************************
*! Name      : lfCsvRec
*! Developer : NADER NABIL (NNA)
*! Date      : 03/25/2005
*! Purpose   : Write O records in the output orders text file
*!*************************************************************
*! Called from : SO2CSV
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfCsvRec
PRIVATE lcSegLine , lcHeader
*--Insert a Header Line
lcHeader = ''
lcHeader = "Sales Order#,Pick Ticket#,Customer Acct.#,Customer Name,Address1,Address2,"
lcHeader = lcHeader + "City,State,Zip,Contact Name,Terms,Service Type"
= FPUTS(lnOutFile,lcHeader)
SELECT (lcTmpFile)
SCAN
  lcSegLine = ''
  *-- Order Number
  lcSegLine = lcSegLine + ALLTRIM(ORDER)+','

  *-- Piktkt No.
  lcSegLine = lcSegLine + ALLTRIM(PIKTKT)+','

  *-- Customer Code (Ship To customer Code)
  lcSegLine = lcSegLine + Account + ','

  *-- Customer Name (Ship To customer name)
  lcSegLine = lcSegLine + Name + ','

  *-- Customer Address12 (Ship To address 1)
  lcSegLine = lcSegLine +Address1 + ','

  *-- Customer Address22 (Ship To address 2)
  lcSegLine = lcSegLine +Address2 + ','

  *-- Customer Address32 (Ship To city)
  lcSegLine = lcSegLine +City + ','

  *-- Customer Address42 (Ship To state)
  lcSegLine = lcSegLine +State + ','

  *-- Customer Address52  (Ship To zip code)
  lcSegLine = lcSegLine +ALLTRIM(Zip) +','

  *-- Customer'S Contact Name 
  lcSegLine = lcSegLine + Contact + ','

  *--Terms Code
  lcSegLine = lcSegLine + ALLTRIM(TermDesc) + ','

  *--ShipVia
  lcSegLine = lcSegLine + ALLTRIM(ShipVia)

  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
*--End of Function lfCsvRec.
*!*************************************************************
*! Name : lfGetFile
*! Auth : Hassan Ibrahim ALi
*! Date : 10/29/2001
*! Ref  : C102494,1
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfGetFile
PRIVATE ltOutFile , lcTalk
lcOutFile = ""

ltOutFile = PUTFILE('Select the output file' , lcOutFile , "CSV" )

IF !EMPTY(ltOutFile)
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
ENDIF  

