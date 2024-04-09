*:**************************************************************************
*: Program file  : ARRFS10.PRG
*: Program desc. : Customer Master Conversion 
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102644,1
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*B606326,1 SSSE 08/01/2002 Fix bug of updating Ship address with Bill address.

*lcExcelDir --> Variable to hold the directory and path of the Excel file.
*llCustOpen --> Variable Flag to detect whether Customer Master file is opened or not.
*llCodeOpen --> Variable Flag to detect whether Codes file is opened or not.
*llSRepOpen --> Variable Flag to detect whether SalesRep Master file is opened or not.
*llCstHstOp --> Variable Flag to detect whether Customer History file is opened or not.
*llFisHdOpn --> Variable Flag to detect whether Fiscal Header file is opened or not.
*lcCurrPath --> Variable to hold Current path for Imported DBF file.
*lccShipTo  --> Variable to hold Alias of cShipTo Imported DBF file.
*lcCustomer --> Variable to hold Alias of Customer Imported DBF file.
*lcSalesMan --> Variable to hold Alias of SalesMen Imported DBF file.
*laExcelCol --> Array to hold the Excel Columns sorted as sent by Excel attached E-mail.
*laExFields --> Array to hold name of Excel Columns which is the 1st record of Imported file.
*lcPrevYear --> Variable to hold the Previous Fiscal Year.
*lcCurrYear --> Variable to hold the Current Fiscal Year.
*lcNextYear --> Variable to hold the Next Fiscal Year.
*llErrOccur --> Variable Flag to detect that an Error happened.
*llAccExist --> Variable Flag to detect that this account , store exist for the same Customer.
*lcExceptTb --> Variable to hold the table for the exceptional file.
*lcDefClass --> Variable to hold the default Class code in codes file.
*lcDefRegon --> Variable to hold the default Region code in codes file.
*lcDefTerms --> Variable to hold the default Term code in codes file.
*lcDefSpcIn --> Variable to hold the default Special instructions code in codes file.
*lcDefShipV --> Variable to hold the default Shipvia code in codes file.
*llNotePad  --> Variable Flag to detect whether NotePad file is opened or not.
*lcFacCode  --> Variable to hold the one and only factor in Factor Master file.
*llFactor   --> Variable Flag to detect whether Factor file is opened or not.

PRIVATE lcExcelDir , llCustOpen , llCodeOpen , llSRepOpen , llCstHstOp , llFisHdOpn , ;
        lcCurrPath , lccShipTo , lcCustomer , lcSalesMan , laExcelCol , laExFields , ;
        lcPrevYear , lcCurrYear , lcNextYear , llErrOccur , llAccExist , lcExceptTb , ;
        lcDefClass , lcDefRegon , lcDefTerms , lcDefSpcIn
        
lcExcelDir = GETDIR('XLS','Select Excel Folder','Select')

*-- Checking if User pressed Cancel.
IF EMPTY(lcExcelDir)
  *-- Message : < Operation has been cancelled >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  RETURN
ENDIF

*-- Checking if user selected wrong folder.
lcCustomer = "CUSTOMER.XLS"
lccShipTo  = "CSHIPTO.XLS"
lcSalesMan = "SALESMAN.XLS"
IF !FILE(lcExcelDir+lccShipTo) OR !FILE(lcExcelDir+lcCustomer) OR !FILE(lcExcelDir+lcSalesMan)
  *-- Message : < One or more of the following files lccShipTo.XLS, lcCustomer.XLS, lcSalesMan.XLS are not found.>
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','One or more of the following files lccShipTo.XLS, lcCustomer.XLS, lcSalesMan.XLS are not found.')
  RETURN
ENDIF

*-- Opening Codes file to check for some Codes
llCodeOpen = gfOpenFile(gcDataDir+'Codes',gcDataDir+'cCode_No','SH')

*-- Checking for Terms Code in Codes file
IF !SEEK('D'+PADR('CTERMCODE',10),'Codes')
  *-- Message : < Term Codes is not found in Codes file, Add it and try again. >
  *-- Buttons : <                                  OK                                 >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Term Codes is not found in Codes file, Add it and try again.')  
  RETURN
ENDIF

*-- Checking for Special instruction in Codes file
IF !SEEK('D'+PADR('SPCINST',10),'Codes')
  *-- Message : < Special Instruction Codes is not found in Codes file, Add it and try again. >
  *-- Buttons : <                                         OK                                         >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Special Instruction Codes is not found in Codes file, Add it and try again.')
  RETURN
ENDIF

*-- Checking for Class Code in Codes file
IF !SEEK('DCLASS','Codes')
  *-- Message : < There is no class codes , Can not proceed. >
  *-- Buttons : <                    OK                      >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no class codes , Cannot proceed.')  
  RETURN
ENDIF

*-- Checking for Region Code in Codes file
IF !SEEK('DREGION','Codes')
  *-- Message : < There is no Region codes , Can not proceed. >
  *-- Buttons : <                    OK                       >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Region codes , Cannot proceed.')  
  RETURN
ENDIF

*-- Checking for ShipVia Code in Codes file
IF !SEEK('DSHIPVIA','Codes')
  *-- Message : < There is no Region codes , Can not proceed. >
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Region codes , Can not proceed.')
  RETURN
ENDIF

*-- Store the default codes into variables
lcDefClass = IIF(SEEK('D'+PADR('CLASS',10),'Codes'),Codes.cCode_No,'')
lcDefRegon = IIF(SEEK('D'+PADR('REGION',10),'Codes'),Codes.cCode_No,'')
lcDefTerms = IIF(SEEK('D'+PADR('CTERMCODE',10),'Codes'),Codes.cCode_No,'')
lcDefSpcIn = IIF(SEEK('D'+PADR('SPCINST',10),'Codes'),Codes.cCode_No,'')
lcDefShipV = IIF(SEEK('D'+PADR('SHIPVIA',10),'Codes'),Codes.cCode_No,'')

*lcUsrDfFlds = ''
*IF FILE(gcDataDir+'ARCUST.MEM')
*  RESTORE FROM (gcDataDir+'ARCUST.MEM') ADDITIVE
*  PRIVATE lnLoop , lnLoop
*  FOR lnLoop = 1 TO 10
*    lcLoop = ALLTRIM(STR(lnLoop,2))
*    IF UPPER("EMAIL") $ UPPER(lcField&lcLoop) OR UPPER("E-MAIL") $ UPPER(lcField&lcLoop) OR UPPER("E-MAIL") $ UPPER(lcFields&lcLoop)
*      lcUsrDfFlds = ALLTRIM(STR(lnLoop,1,2))
*      EXIT
*    ENDIF
*  ENDFOR
*ENDIF  

*-- Defining Array to hold the Expected sorted Excel columns.
*DIMENSION laExcelCol[28]
*laExcelCol[1]  = "DIV"

*-- Importing from Excel Data to Temp Sales Man file.
SELECT 0                            && Prepare Empty Work Area.
lcCurrPath = SYS(5) + SYS(2003)     && get Default Drive and Directory.
IMPORT FROM (lcExcelDir+lcSalesMan) TYPE XLS
lcSalesMan = ALIAS()                && get Current File Alias.
LOCATE
DELETE
INDEX ON A TAG SalesMan ADDITIVE

*-- Importing from Excel Data to Temp cShipTo file.
SELECT 0                            && Prepare Empty Work Area.
lcCurrPath = SYS(5) + SYS(2003)     && get Default Drive and Directory.
IMPORT FROM (lcExcelDir+lcCShipTo) TYPE XLS
lcCShipTo  = ALIAS()                && get Current File Alias.
LOCATE
DELETE
INDEX ON B TAG CShipTo ADDITIVE

*-- Importing from Excel Data to Temp Customer file.
SELECT 0                            && Prepare Empty Work Area.
lcCurrPath = SYS(5) + SYS(2003)     && get Default Drive and Directory.
IMPORT FROM (lcExcelDir+lcCustomer) TYPE XLS
lcCustomer = ALIAS()                && get Current File Alias.

*-- Need to check if structure of Excel file was changed.
LOCATE                              && Move to first record.
SCATTER TO laExFields               && Scatter Heading of Excel Columns.

*-- Comparing the 2 Arrays together.
*FOR lnCount = 1 TO ALEN(laExFields,1)
*  IF !(ALLTRIM(laExFields[lnCount]) == ALLTRIM(laExcelCol[lnCount]))
*    *-- Message : < Structure of Excel file has been changed, Can not proceed!. >
*    *-- Buttons : <                              OK                             >
*    =gfModalGen("TRM000000B00000","DIALOG",'','','Structure of Excel file has been changed, Can not proceed!.')  
*    ERASE (lcCurrPath + "\" + lcCustomer + ".DBF")
*    ERASE (lcCurrPath + "\" + lcCustomer + ".CDX")    
*    RETURN  
*  ENDIF
*ENDFOR

*-- First record that contains Excel Heading (Name of Columns) must be deleted.
*-- Scatter Heading of imported DBF file before deleting it to insert it in the Exceptional file
DELETE
INDEX ON A TAG Customer ADDITIVE

*-- Create Exceptional file that will hold all the Account+Store which exists 
*-- in Customer file or has some data error.
lcExceptTb = gfTempName()
SELECT (lcCustomer)      && Select Temp Imported excel file.
=AFIELDS(laExcelFil)

*-- Add another Row to Array for Reason Field
DIMENSION laExcelFil[ALEN(laExcelFil,1)+1,4] , laExFields[ALEN(laExFields,1)+1]
laExcelFil[ALEN(laExcelFil,1),1] = "REASON"
laExcelFil[ALEN(laExcelFil,1),2] = "C"
laExcelFil[ALEN(laExcelFil,1),3] = 150
laExcelFil[ALEN(laExcelFil,1),4] = 0

laExFields[ALEN(laExFields,1)] = "Rejected Reasons"
CREATE TABLE (gcWorkDir+lcExceptTb) FROM ARRAY laExcelFil

= gfOpenFile(gcWorkDir+lcExceptTb,'','SH')
SELECT (lcExceptTb)

INSERT INTO (lcExceptTb) FROM ARRAY laExFields

*-- Opening Customer Master , SalesRep Master , Customer History , Fiscal Year files.
llCustOpen = gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH','Cust',.T.)
llSRepOpen = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
llCstHstOp = gfOpenFile(gcDataDir+'ArCusHst','','SH')
llFisHdOpn = gfOpenFile(gcDataDir+'FisHd','','SH')
llNotePad  = gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
llFactor   = gfOpenFile(gcSysHome+'SycFact','','SH')

*-- Get the First factor from Factor Master file.
SELECT SycFact
LOCATE
lcFacCode = IIF(EOF(),'',cFacCode)

*-- First we need to get Current Year from Fiscal Header file
SELECT FisHd
SCAN
  IF cFisYStat = "C"
    lcCurrYear = cFisFYear
    lcPrevYear = ALLTRIM(STR(VAL(cFisFYear)-1))
    lcNextYear = ALLTRIM(STR(VAL(cFisFYear)+1))
    EXIT
  ENDIF
ENDSCAN

*-- Close Fiscal Year File (not used anymore)
IF llFisHdOpn
  USE IN FisHd
ENDIF

*-- Scan loop around Temp Imported excel file.
SELECT (lcCustomer)      && Select Temp Imported excel file.
llStrExist = .F.         && First there is no Account + Store exist before 

SCAN
  STORE '' TO M.TYPE , M.ACCOUNT , M.STORE , M.CADDRESS12 , M.CADDRESS22 , M.CADDRESS32 ,;
              M.CADDRESS42 , M.CADDRESS52 , M.CADDRESS62 , M.STATUS , M.CDIVISION , M.DBA ,;
              M.BTNAME , M.LINK_CODE , M.CONSOL , M.DUNS , M.BUYER , M.KEEPER , M.STNAME ,;
              M.CADDRESS1 , M.CADDRESS2 , M.CADDRESS3 , M.CADDRESS4 , M.CADDRESS5 , M.CLASS ,;
              M.PRIORITY , M.CTERMCODE , M.SPCINST , M.CINSUR , M.PRICELVL , M.PRNT_STATM ,;
              M.CCONT_CODE , M.CCONT_COD2 , M.PHONE1 , M.PHONE2 , M.FAX , M.SALESREP , ;
              M.REGION , M.SHIPVIA , M.BILLTO , M.SKUTMPL , M.CTAXRULE , M.CSLSGLLINK , ;
              M.CCURRCODE

  STORE 0 TO M.CRLIMIT , M.CRAVAIL , M.COMM
  
  *-- If Account is less than 5 Char complete it with "0" to reach 5 char. 
  M.ACCOUNT = UPPER(STRTRAN(SUBSTR(C,1,3)," ","0") + ALLTRIM(SUBSTR(I,1,2)))
   
  *-- If record is not found in Customer file
  IF SEEK('M'+M.ACCOUNT,'Cust') OR LEN(M.ACCOUNT) < 5
  
    llStrExist = .T.
    SCATTER MEMVAR MEMO
    
    DO CASE
      CASE SEEK('M'+M.ACCOUNT,'Cust')
        WAIT WINDOW 'Account ' + M.ACCOUNT + ' exists in customer file' NOWAIT
        M.REASON = "This Record was not added in Customer file because it has the same Account."
      CASE LEN(M.ACCOUNT) < 5
        WAIT WINDOW 'Account ' + M.ACCOUNT + ' less than 5 characters' NOWAIT
        M.REASON = "This Record was not added in Customer file because it has Account code which is less than 5 characters."
    ENDCASE

    INSERT INTO (lcExceptTb) FROM MEMVAR

  ELSE
    *-- A message to inform user of the conversion process.
    WAIT WINDOW 'Converting Account ' + M.ACCOUNT + ' To Customer File.' NOWAIT

    *-- Call get data procedure
    DO lpGetData

  ENDIF
  *-- Endif of record is not found in Customer file
ENDSCAN
WAIT CLEAR

*-- Scan loop around Temp Imported excel file.
SELECT (lcCShipTo )      && Select Temp Imported Ship to excel file.
SCAN
     
  *-- If record is not found in Customer file
  IF SEEK(B,lcCustomer)

    M.ACCOUNT = UPPER(STRTRAN(SUBSTR(&lcCustomer..C,1,3)," ","0") + ALLTRIM(SUBSTR(&lcCustomer..I,1,2)))
    M.STORE   = UPPER(LTRIM(SUBSTR(C,1,5)) + RTRIM(SUBSTR(J,1,3)))
    IF SEEK('S'+M.ACCOUNT+M.STORE,'Cust')
      
      *B606326,1 Update the ship addresses. [Begin]
      SELECT Cust
      REPLACE StName    WITH ALLTRIM(&lcCShipTo..C) , ;
              cAddress1 WITH ALLTRIM(&lcCShipTo..F) , ;
              cAddress2 WITH ALLTRIM(&lcCShipTo..G) , ;
              cAddress3 WITH ALLTRIM(&lcCShipTo..H) , ;
              cAddress4 WITH ALLTRIM(&lcCShipTo..I) , ;
              cAddress5 WITH ALLTRIM(&lcCShipTo..J)
      *B606326,1 Update the ship addresses. [End]
      
      M.A = &lcCustomer..A      
      STORE '' TO M.B , M.C , M.D , M.E , M.F , M.G , M.H , M.I , M.J , M.K , M.L , M.M , M.N , ;
                  M.O , M.P , M.Q , M.R , M.S , M.T , M.U , M.V , M.W , M.X , M.Y , M.Z , M.AA 
      WAIT WINDOW 'Store ' + M.Store + ' for Account ' + M.ACCOUNT + ' exists in customer file' NOWAIT
      M.REASON = 'cShipto has a record with Ship_Idx no ' + &lcCShipTo..A + ' and Cust_Idx no ' + &lcCShipTo..B + ' which was not added in Customer file because it has the same Account & Store'
      INSERT INTO (lcExceptTb) FROM MEMVAR
    ELSE
  
      *-- A message to inform user of the conversion process.
      WAIT WINDOW 'Converting Store ' + M.STORE + ' for Account : ' + M.ACCOUNT + ' To Customer File.' NOWAIT

      STORE '' TO M.TYPE , M.CADDRESS12 , M.CADDRESS22 , M.CADDRESS32 ,;
                  M.CADDRESS42 , M.CADDRESS52 , M.CADDRESS62 , M.STATUS , M.CDIVISION , M.DBA ,;
                  M.BTNAME , M.LINK_CODE , M.CONSOL , M.DUNS , M.BUYER , M.KEEPER , M.STNAME ,;
                  M.CADDRESS1 , M.CADDRESS2 , M.CADDRESS3 , M.CADDRESS4 , M.CADDRESS5 , M.CLASS ,;
                  M.PRIORITY , M.CTERMCODE , M.SPCINST , M.CINSUR , M.PRICELVL , M.PRNT_STATM ,;
                  M.CCONT_CODE , M.CCONT_COD2 , M.PHONE1 , M.PHONE2 , M.FAX , M.SALESREP , ;
                  M.REGION , M.SHIPVIA , M.BILLTO , M.SKUTMPL , M.CTAXRULE , M.CSLSGLLINK , ;
                  M.CCURRCODE
      STORE 0 TO M.CRLIMIT , M.CRAVAIL , M.COMM

      *-- Call get data procedure
      DO lpGetStore
    ENDIF

  ELSE
    llStrExist = .T.
    SCATTER MEMVAR MEMO    
    STORE '' TO M.A , M.B , M.C , M.D , M.E , M.F , M.G , M.H , M.I , M.J , M.K , M.L , M.M , ;
                M.N , M.O , M.P , M.Q , M.R , M.S , M.T , M.U , M.V , M.W , M.X , M.Y , M.Z , M.AA 
    M.REASON = 'cShipto has a record with Ship_Idx no ' + &lcCShipTo..A + ' and Cust_Idx no ' + &lcCShipTo..B + ' which was not added in Customer file because its account code is not found in Imported Customer excel file.'

    INSERT INTO (lcExceptTb) FROM MEMVAR
  ENDIF
  *-- Endif of record is not found in Customer file
ENDSCAN
WAIT CLEAR

*-- Export the Excel File to Excel after notifing the User.
IF llStrExist
  PRIVATE lcExcptDir

  *-- Message : < Some records has Exceptional data or data error , So you will be prompted to 
  *--             export this to an Excel file. >
  *-- Buttons : <                                  OK                                   >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Some records has Exceptional data or data error , So you will be prompted to export this to an Excel file.')  

  SELECT (lcExceptTb)
  lcExcptDir = PUTFILE('Exceptional file','Except.XLS','XLS')

  IF EMPTY(lcExcptDir)
    *-- Message : < Operation has been cancelled >
    *-- Buttons : <             OK               >
    =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  ELSE
    EXPORT TO (lcExcptDir) TYPE XLS
  ENDIF

ENDIF

*-- Close the Customer & SalesRep Master file.
IF llCustOpen
  USE IN CUST
ENDIF

IF llNotePad
  USE IN NotePad
ENDIF

*-- Close Sales Rep file if opened
IF llSRepOpen
  USE IN SALESREP
ENDIF

*-- Close Codes file if opened
IF llCodeOpen
  USE IN CODES
ENDIF

*-- Close the Imported DBF File and Erase it
IF USED(lcCustomer)
  USE IN (lcCustomer)
ENDIF
ERASE (lcCurrPath + "\" + lcCustomer + ".DBF")
ERASE (lcCurrPath + "\" + lcCustomer + ".CDX")

IF USED(lccShipTo)
  USE IN (lccShipTo)
ENDIF
ERASE (lcCurrPath + "\" + lccShipTo + ".DBF")
ERASE (lcCurrPath + "\" + lccShipTo + ".CDX")

IF USED(lcSalesMan)
  USE IN (lcSalesMan)
ENDIF
ERASE (lcCurrPath + "\" + lcSalesMan + ".DBF")
ERASE (lcCurrPath + "\" + lcSalesMan + ".CDX")

*-- Close the Exceptional File and Erase it
IF USED(lcExceptTb)
  USE IN (lcExceptTb)
ENDIF
ERASE (gcWorkDir + lcExceptTb + ".DBF")
ERASE (gcWorkDir + lcExceptTb + ".CDX")

*-- Notify that the conversion is completed.
*-- Message : < Conversion has been completed successfully. >
*-- Buttons : <                      OK                     >
=gfModalGen("TRM000000B00000","DIALOG",'','','Conversion has been completed successfully.')
*-- End of Program

*!**************************************************************************
*! Name      : lpGetData
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/19/2000
*! Purpose   : get data from imported Dbf file
*!**************************************************************************
*! Example   : DO lpGetData 
*!**************************************************************************
*
PROCEDURE lpGetData

*-- Adding Phone number and Fax.
M.PHONE1 = ALLTRIM(L)
M.PHONE2 = ALLTRIM(O)
M.FAX    = ALLTRIM(N)
  
*-- Adding DBA , DUNS , Consolidated , Buyers name , Credit Limit , Keeper , Priority , Term Codes , 
*-- Special Instruction , Insurance , Price Level , Print Statement , Country Code ,
*-- BillTo , PriceLvl , Country Code 1 & 2 , SKUTmpl , cTaxRule , Sales Link Code , Currency Code
M.STATUS     = "A"
M.CONSOL     = "N"

*-- Store default codes
M.CTERMCODE = lcDefTerms
M.SPCINST   = lcDefSpcIn
M.REGION = lcDefRegon
M.CLASS = lcDefClass
M.SHIPVIA = lcDefShipV
M.CFACCODE = lcFacCode

*-- DEFAULTS
M.CINSUR     = "Y"
M.PRICELVL   = "A"
M.PRNT_STATM = "Y"
M.CCONT_CODE = "USA"
M.CCONT_COD2 = "USA"
M.BILLTO     = "M"
M.PRIORITY   = "5"
M.PRICELVL   = "A"
M.SKUTMPL    = 'DEF'
M.CTAXRULE   = ' 1'
M.CSLSGLLINK = 'DEF'
M.LINK_CODE = 'DEFDEF'
M.CCURRCODE  = gcBaseCurr

M.USR_DFND1 = ALLTRIM(SUBSTR(P,1,30))
  
IF !EMPTY(ALLTRIM(Q))
  INSERT INTO NotePad (Type , Key , cDesc , mNotes) VALUES ('A' , M.ACCOUNT , 'Notes For Account Number : ' + M.ACCOUNT , &lcCustomer..Q)
ENDIF
M.SALESREP = ''

IF !EMPTY(ALLTRIM(&lcCustomer..Y)) AND SEEK(PADL(ALLTRIM(&lcCustomer..Y),9),lcSalesMan) AND SEEK(ALLTRIM(&lcSalesMan..B),'SalesRep')
  M.SALESREP = &lcSalesMan..B
ENDIF

*-- If Account is not found.
IF !SEEK('M'+M.ACCOUNT,'Cust')
  *-- Adding a record for the Main account.
  M.TYPE  = "M"
  M.STORE = ""

  *-- Adding the Bill To (Name and Addresses). 
  M.BTNAME     = SUBSTR(ALLTRIM(C),1,30)
  M.CADDRESS12 = SUBSTR(ALLTRIM(E),1,30)
  M.CADDRESS22 = SUBSTR(ALLTRIM(F),1,30)
  M.CADDRESS32 = SUBSTR(ALLTRIM(G),1,30)
  
  IF !SEEK('N'+PADR('STATE',10)+SUBSTR(ALLTRIM(&lcCustomer..H),1,2),'Codes')    
    INSERT INTO Codes (cDefCode,cFld_Name,cCode_No,cRltField,cRltd_Nam,cRltd_Typ) VALUES ;
                      ("N","STATE",SUBSTR(ALLTRIM(&lcCustomer..H),1,2),"N","","")
    INSERT INTO Codes (cDefCode,cFld_Name,cCode_No,cRltField,cRltd_Nam,cRltd_Typ) VALUES ;
                      ("N","STATE",SUBSTR(ALLTRIM(&lcCustomer..H),1,2),"Y","NTAXRATE","N")
  ENDIF
  M.CADDRESS42 = SUBSTR(ALLTRIM(H),1,2)
  M.CADDRESS52 = SUBSTR(ALLTRIM(I),1,30)
  
  *-- Adding the Ship To (Name and Addresses).
  M.STNAME    = M.BTNAME
  M.CADDRESS1 = M.CADDRESS12
  M.CADDRESS2 = M.CADDRESS22
  M.CADDRESS3 = M.CADDRESS32
  M.CADDRESS4 = M.CADDRESS42
  M.CADDRESS5 = M.CADDRESS52

  INSERT INTO Cust FROM MEMVAR
                      
  *-- Adding 3 records for Customer in ArCusHst for Prev , Current , Next Year
  M.cFisFYear = lcPrevYear
  INSERT INTO ArCusHst FROM MEMVAR
  
  M.cFisFYear = lcCurrYear
  INSERT INTO ArCusHst FROM MEMVAR

  M.cFisFYear = lcNextYear
  INSERT INTO ArCusHst FROM MEMVAR
    
ENDIF
*-- EndIf of Account is not found.

*-- End of lpGetData. 

*!**************************************************************************
*! Name      : lpGetStore
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/19/2000
*! Purpose   : get data from imported Dbf file
*!**************************************************************************
*! Example   : DO lpGetStore
*!**************************************************************************
*
PROCEDURE lpGetStore

IF LEN(M.ACCOUNT) = 5 AND SEEK('M'+M.ACCOUNT,'Cust') 
  *-- Adding Phone number and Fax.
  M.PHONE1 = ALLTRIM(L)
  
  *-- Adding DBA , DUNS , Consolidated , Buyers name , Credit Limit , Keeper , Priority , Term Codes , 
  *-- Special Instruction , Insurance , Price Level , Print Statement , Country Code ,
  *-- BillTo , PriceLvl , Country Code 1 & 2 , SKUTmpl , cTaxRule , Sales Link Code , Currency Code
  M.TYPE = "S" 
  M.STATUS = "A"

  *-- Store default codes
  M.SHIPVIA = lcDefShipV

  M.CCONT_CODE = "USA"
  M.CCONT_COD2 = "USA"
  M.BILLTO     = "M"
  M.SKUTMPL    = 'DEF'
  M.CTAXRULE   = ' 1'

  *-- Adding the Bill To (Name and Addresses).
  M.SALESREP   = Cust.SalesRep
  M.BTNAME     = Cust.BtName
  M.CADDRESS12 = Cust.cAddress12
  M.CADDRESS22 = Cust.cAddress22
  M.CADDRESS32 = Cust.cAddress32
  M.CADDRESS42 = Cust.cAddress42
  M.CADDRESS52 = Cust.cAddress52
  
  *-- Adding the Ship To (Name and Addresses).
  *B606326,1 Update the ship addresses. [Begin]
  *M.STNAME    = M.BTNAME
  *M.CADDRESS1 = M.CADDRESS12
  *M.CADDRESS2 = M.CADDRESS22
  *M.CADDRESS3 = M.CADDRESS32
  *M.CADDRESS4 = M.CADDRESS42
  *M.CADDRESS5 = M.CADDRESS52
  M.STNAME    = ALLTRIM(C)
  M.CADDRESS1 = ALLTRIM(F)
  M.CADDRESS2 = ALLTRIM(G)
  M.CADDRESS3 = ALLTRIM(H)
  M.CADDRESS4 = ALLTRIM(I)
  M.CADDRESS5 = ALLTRIM(J)
  *B606326,1 Update the ship addresses. [End]

  INSERT INTO Cust FROM MEMVAR
ENDIF  
*-- End of lpGetStore.
