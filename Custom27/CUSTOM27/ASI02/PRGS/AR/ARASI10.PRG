*:**************************************************************************
*: Program file  : AREAG10.PRG
*: Program desc. : Customer Master Conversion 
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C101932
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*B603858,1 SSE 08/29/2000 1) Fix Bug of Adding the Commission from SalesRep master file 
*B603858,1                into Customer file if this SalesRep exists in Customer file.
*B603858,1                Also adding Zeros to SalesRep Code if less than 3 Char.
*B603858,1                And Adding SalesRep in customer file if not found 
*B603858,1                2) Also Commenting out Error Handler
*B603858,1                3) Adding a reason field to the Exceptional file
*B603858,1                4) Remove the Zeros from the Phone number

*lcExcelDir --> Variable to hold the directory and path of the Excel file.
*llCustOpen --> Variable Flag to detect whether Customer Master file is opened or not.
*llCodeOpen --> Variable Flag to detect whether Codes file is opened or not.
*llSRepOpen --> Variable Flag to detect whether SalesRep Master file is opened or not.
*llCstHstOp --> Variable Flag to detect whether Customer History file is opened or not.
*llFisHdOpn --> Variable Flag to detect whether Fiscal Header file is opened or not.
*lcCurrPath --> Variable to hold Current path for Imported DBF file.
*lcCurAlias --> Variable to hold Alias of Imported DBF file.
*laExcelCol --> Array to hold the Excel Columns sorted as sent by Excel attached E-mail.
*laExFields --> Array to hold name of Excel Columns which is the 1st record of Imported file.
*lcPrevYear --> Variable to hold the Previous Fiscal Year.
*lcCurrYear --> Variable to hold the Current Fiscal Year.
*lcNextYear --> Variable to hold the Next Fiscal Year.
*lcSavError --> Variable to hold saved Error Handler.
*llErrOccur --> Variable Flag to detect that an Error happened.
*llAccExist --> Variable Flag to detect that this account , store exist for the same Customer.
*lcExceptTb --> Variable to hold the table for the exceptional file.

PRIVATE lcExcelDir , llCustOpen , llCodeOpen , llSRepOpen , llCstHstOp , llFisHdOpn , ;
        lcCurrPath , lcCurAlias , laExcelCol , laExFields , lcPrevYear , lcCurrYear , ;
        lcNextYear , lcSavError , llErrOccur , llAccExist , lcExceptTb
        
lcExcelDir = GETFILE('XLS','Select your Excel File','Select')

*-- Checking if User pressed Cancel.
IF EMPTY(lcExcelDir)
  *-- Message : < Operation has been cancelled >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  RETURN
ENDIF

*-- Checking if User selected File not of Type Excel Extension.
IF !(RIGHT(lcExcelDir,3)=="XLS")
  *-- Message : < Only Excel files are allowed for conversion >
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Only Excel files are allowed for conversion.')  
  RETURN
ENDIF

*-- Opening Codes file to check for some Codes
llCodeOpen = gfOpenFile(gcDataDir+'Codes',gcDataDir+'cCode_No','SH')

*-- Checking for Terms Code in Codes file
IF !SEEK('N'+PADR('CTERMCODE',10)+'000028','Codes')
  *-- Message : < Term Code# 000028 is not found in Codes file, Add it and try again. >
  *-- Buttons : <                                  OK                                 >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Term Code# 000028 is not found in Codes file, Add it and try again.')  
  RETURN
ENDIF

*-- Checking for Special instruction in Codes file
IF !SEEK('N'+PADR('SPCINST',10)+'000048','Codes')
  *-- Message : < Special Instruction Code# 000048 is not found in Codes file, Add it and try again. >
  *-- Buttons : <                                         OK                                         >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Special Instruction Code# 000048 is not found in Codes file, Add it and try again.')  
  RETURN
ENDIF

*-- Checking for Class Code in Codes file
IF !SEEK('NCLASS','Codes')
  *-- Message : < There is no class codes , Can not proceed. >
  *-- Buttons : <                    OK                      >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no class codes , Cannot proceed.')  
  RETURN
ENDIF

*-- Checking for Region Code in Codes file
IF !SEEK('NREGION','Codes')
  *-- Message : < There is no Region codes , Can not proceed. >
  *-- Buttons : <                    OK                       >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Region codes , Cannot proceed.')  
  RETURN
ENDIF

*-- Checking for ShipVia Code in Codes file
IF !SEEK('NSHIPVIA','Codes')
  *-- Message : < There is no Region codes , Can not proceed. >
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Region codes , Can not proceed.')
  RETURN
ENDIF

*-- Defining Array to hold the Expected sorted Excel columns.
DIMENSION laExcelCol[28]
laExcelCol[1]  = "DIV"
laExcelCol[2]  = "ACCOUNT"
laExcelCol[3]  = "STORE"
laExcelCol[4]  = "NAME"
laExcelCol[5]  = "ADDRESS 1"
laExcelCol[6]  = "ADDRESS 2"
laExcelCol[7]  = "CITY"
laExcelCol[8]  = "STATE"
laExcelCol[9]  = "REGION"
laExcelCol[10] = "ZIP CODE"
laExcelCol[11] = "PHONE"
laExcelCol[12] = "PHONE # 2"
laExcelCol[13] = "NAME (SHIP TO)"
laExcelCol[14] = "ADDRESS LINE1 (SHIP TO)"
laExcelCol[15] = "ADDRESS LINE2 (SHIP TO)"
laExcelCol[16] = "CITY"
laExcelCol[17] = "STATE"
laExcelCol[18] = "ZIP"
laExcelCol[19] = "DBA"
laExcelCol[20] = "SHIP VIA"
laExcelCol[21] = "PRIOR.CODE"
laExcelCol[22] = "SALES.REP"
laExcelCol[23] = "UPS INSURANCE"
laExcelCol[24] = "D&B NUMBER"
laExcelCol[25] = "BUYERS NAME"
laExcelCol[26] = "CREDIT LIMIT"
laExcelCol[27] = "CONTACT NAME"
laExcelCol[28] = "TERMS"

*-- Importing from Excel Data to Temp Customer file.
SELECT 0                            && Prepare Empty Work Area.
lcCurrPath = SYS(5) + SYS(2003)     && get Default Drive and Directory.
IMPORT FROM (lcExcelDir) TYPE XLS
lcCurAlias = ALIAS()                && get Current File Alias.

*-- Need to check if structure of Excel file was changed.
LOCATE                              && Move to first record.
SCATTER TO laExFields               && Scatter Heading of Excel Columns.

*-- Comparing the 2 Arrays together.
FOR lnCount = 1 TO ALEN(laExFields,1)
  IF !(ALLTRIM(laExFields[lnCount]) == ALLTRIM(laExcelCol[lnCount]))
    *-- Message : < Structure of Excel file has been changed, Can not proceed!. >
    *-- Buttons : <                              OK                             >
    =gfModalGen("TRM000000B00000","DIALOG",'','','Structure of Excel file has been changed, Can not proceed!.')  
    ERASE (lcCurrPath + "\" + lcCurAlias + ".DBF")
    ERASE (lcCurrPath + "\" + lcCurAlias + ".CDX")    
    RETURN  
  ENDIF
ENDFOR

*-- First record that contains Excel Heading (Name of Columns) must be deleted.
*-- Scatter Heading of imported DBF file before deleting it to insert it in the Exceptional file
*SCATTER MEMVAR MEMO     
DELETE

*-- Adding an Index to Excel File (Account+Store).
INDEX ON B + C TAG (lcCurAlias) ADDITIVE

*-- Create Exceptional file that will hold all the Account+Store which exists 
*-- in Customer file or has some data error.
lcExceptTb = gfTempName()
SELECT (lcCurAlias)      && Select Temp Imported excel file.

*B603858,1 3)Add a Reason field to the structure of Exceptional file [Begin]
*COPY STRUCTURE TO (gcWorkDir+lcExceptTb)
=AFIELDS(laExcelFil)

*-- Add another Row to Array for Reason Field
DIMENSION laExcelFil[ALEN(laExcelFil,1)+1,4] , laExFields[ALEN(laExFields,1)+1]
laExcelFil[ALEN(laExcelFil,1),1] = "REASON"
laExcelFil[ALEN(laExcelFil,1),2] = "C"
laExcelFil[ALEN(laExcelFil,1),3] = 150
laExcelFil[ALEN(laExcelFil,1),4] = 0

laExFields[ALEN(laExFields,1)] = "Rejected Reasons"
CREATE TABLE (gcWorkDir+lcExceptTb) FROM ARRAY laExcelFil
*B603858,1 3)Add a Reason field to the structure of Exceptional file [End]

= gfOpenFile(gcWorkDir+lcExceptTb,'','SH')
SELECT (lcExceptTb)

*B603858,1 3)Insert heading from Array instead of Memory variables [Begin]
*INSERT INTO (lcExceptTb) FROM MEMVAR
INSERT INTO (lcExceptTb) FROM ARRAY laExFields
*B603858,1 3)Insert heading from Array instead of Memory variables [End]

*-- Opening Customer Master , SalesRep Master , Customer History , Fiscal Year files.
llCustOpen = gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
llSRepOpen = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
llCstHstOp = gfOpenFile(gcDataDir+'ArCusHst','','SH')
llFisHdOpn = gfOpenFile(gcDataDir+'FisHd','','SH')

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
SELECT (lcCurAlias)      && Select Temp Imported excel file.

*B603858,1 2)Commented out [Begin]
*lcSavError = ON('ERROR')
*B603858,1 2)Commented out [End]

llStrExist = .F.         && First there is no Account + Store exist before 

*B603858,1 2)Commented out [Begin]
*llErrOccur = .F.         && First initialize the variable with false
*ON ERROR llErrOccur = .T.
*B603858,1 2)Commented out [End]

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
  *M.ACCOUNT    = IIF(LEN(ALLTRIM(B))<5,REPLICATE("0",5-LEN(ALLTRIM(B)))+ALLTRIM(B),ALLTRIM(B))
  M.ACCOUNT    = PADL(ALLTRIM(B),5,"0")
   
  *-- If record is not found in Customer file
  IF !SEEK('M'+M.ACCOUNT,'Customer') OR !SEEK('S'+M.ACCOUNT+ALLTRIM(C),'Customer')
  
    *-- A message to inform user of the conversion process.
    WAIT WINDOW 'Converting Account ' + M.ACCOUNT + ' To Customer File.' NOWAIT

    *-- Call get data procedure
    DO lpGetData

  ELSE
    llStrExist = .T.
    WAIT WINDOW 'Account ' + M.ACCOUNT + ' exists in customer file' NOWAIT
    
    SCATTER MEMVAR MEMO

    *B603858,1 3)Adding Reason to indicate Same Account and Store [Begin]
    M.REASON = "This Record was not added in Customer file because it has the same Account and Store."
    *B603858,1 3)Adding Reason to indicate Same Account and Store [End]

    INSERT INTO (lcExceptTb) FROM MEMVAR
  ENDIF
  *-- Endif of record is not found in Customer file
ENDSCAN
WAIT CLEAR

*B603858,1 2)Commented out [Begin]
*ON ERROR &lcSavError
*B603858,1 2)Commented out [End]

*-- Export the Excel File to Excel after notifing the User.
IF llStrExist
  PRIVATE lcExcptDir

  *-- Message : < Some Account has duplicate Stores , So you will be prompted to export
  *--             this to an Excel file. >
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
  USE IN CUSTOMER
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
IF USED(lcCurAlias)
  USE IN (lcCurAlias)
ENDIF
ERASE (lcCurrPath + "\" + lcCurAlias + ".DBF")
ERASE (lcCurrPath + "\" + lcCurAlias + ".CDX")

*-- Close the Exceptional File and Erase it
IF USED(lcExceptTb)
  USE IN (lcExceptTb)
ENDIF
ERASE (gcWorkDir + lcExceptTb + ".DBF")
ERASE (gcWorkDir + lcExceptTb + ".CDX")

*-- Notify that the conversion is completed.
*-- Message : < Conversion has been completed successfully. >
*-- Buttons : <                      OK                     >
*B603858,1 2)Remove the llErrOccur Variable [Begin]
*=gfModalGen("TRM000000B00000","DIALOG",'','','Conversion has ' + ;
*           IIF(llErrOccur,'not ','')+'been completed successfully.')
=gfModalGen("TRM000000B00000","DIALOG",'','','Conversion has been completed successfully.')
*B603858,1 2)Remove the llErrOccur Variable [Begin]
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
*B603858,1 4)Replacing the Zeros in phone number with Empty string [Begin]
*M.PHONE1 = IIF(SUBSTR(ALLTRIM(K),1,1)="1",SUBSTR(ALLTRIM(K),2),ALLTRIM(K))
*M.PHONE2 = IIF(SUBSTR(ALLTRIM(L),1,1)="1",SUBSTR(ALLTRIM(L),2),ALLTRIM(L))
* If the phone # is 0 only , replace it with blank. This because this sheet 
* includes around 600 records are replaced with zero in phone no. field.
M.PHONE1 = IIF(ALLTRIM(K)<>'0',IIF(SUBSTR(ALLTRIM(K),1,1)="1",SUBSTR(ALLTRIM(K),2),ALLTRIM(K)),"")
M.PHONE2 = IIF(ALLTRIM(L)<>'0',IIF(SUBSTR(ALLTRIM(L),1,1)="1",SUBSTR(ALLTRIM(L),2),ALLTRIM(L)),"")
*B603858,1 4)Replacing the Zeros in phone number with Empty string [Begin]

M.FAX    = M.PHONE2
  
*-- Adding DBA , DUNS , Consolidated , Buyers name , Credit Limit , Keeper , Priority , Term Codes , 
*-- Special Instruction , Insurance , Price Level , Print Statement , Country Code ,
*-- BillTo , PriceLvl , Country Code 1 & 2 , SKUTmpl , cTaxRule , Sales Link Code , Currency Code
M.STATUS     = "A"
M.DBA        = SUBSTR(ALLTRIM(S),1,30)
M.DUNS       = SUBSTR(ALLTRIM(X),1,11)
M.CONSOL     = "N"
M.BUYER      = SUBSTR(ALLTRIM(Y),1,30)
M.CRLIMIT    = VAL(ALLTRIM(Z))
M.CRAVAIL    = VAL(ALLTRIM(Z))
M.KEEPER     = SUBSTR(ALLTRIM(Aa),1,20)
M.PRIORITY   = SUBSTR(ALLTRIM(U),1,3)
M.CTERMCODE  = "000028"                   
M.SPCINST    = "000048"                   
M.CINSUR     = SUBSTR(ALLTRIM(W),1,1)
M.PRICELVL   = "A"
M.PRNT_STATM = "Y"
M.CCONT_CODE = "USA"
M.CCONT_COD2 = "USA"
M.BILLTO     = "M"
M.PRICELVL   = "A"
M.SKUTMPL    = 'DEF'
M.CTAXRULE   = ' 1'
M.CSLSGLLINK = 'DEF'
M.CCURRCODE  = gcBaseCurr

*B603858,1 1)Pad left Sales Rep with 0 [Begin]
*M.SALESREP = IIF(LEN(ALLTRIM(V))<3,REPLICATE("0",3-LEN(ALLTRIM(V)))+ALLTRIM(V),ALLTRIM(V))
M.SALESREP = PADL(ALLTRIM(V),3,"0")
*B603858,1 1)Pad left Sales Rep with 0 [End]

*-- Adding the Sales Rep Code.
IF SEEK(M.SALESREP,"SALESREP")
  *B603858,1 1)if SalesRep found get commission [Begin]
  M.COMM     = SALESREP.COMM  
  *B603858,1 1)if SalesRep found get commission [End]
ELSE
  M.SALESREP = ""
  IF !EMPTY(ALLTRIM(V))
    SCATTER MEMVAR MEMO
    M.REASON = "This Record was added in Customer file with an empty SalesRep Code because this SalesRep which is in Excel file is not found in SalesRep Master file." 
    INSERT INTO (lcExceptTb) FROM MEMVAR  
  ENDIF
ENDIF
  
*-- Adding the Region Code.
DO CASE
  CASE ALLTRIM(I) == "MIDWEST"
    M.REGION = "000037"
  CASE ALLTRIM(I) == "SOUTHWEST"
    M.REGION = "000038"
  CASE ALLTRIM(I) == "SOUTHEAST"
    M.REGION = "000040"
  CASE ALLTRIM(I) == "MID-ATLANTIC"
    M.REGION = "000041"
  CASE ALLTRIM(I) == "NEW ENGLAND"
    M.REGION = "000042"
  CASE ALLTRIM(I) == "CENTRAL STATES"
    M.REGION = "000158"
  CASE ALLTRIM(I) == "WEST"
    M.REGION = "000159"
ENDCASE

*B603858,1 3)Adding Reason to indicate Error data in Region field [Begin]
*M.REGION = IIF(SEEK('N'+PADR('REGION',10)+M.REGION,'Codes'),;
*           M.REGION,IIF(SEEK('D'+PADR('REGION',10),'Codes'),Codes.cCode_No,M.REGION))
IF EMPTY(M.REGION)
  SCATTER MEMVAR MEMO
  IF EMPTY(ALLTRIM(I))
    M.REASON = "This Record was added in Customer file with an error in Region field due to an empty Region Description in the imported Excel file."
  ELSE
    M.REASON = "This Record was added in Customer file with an error in Region field due to a wrong Region Description in the imported Excel file."
  ENDIF
  INSERT INTO (lcExceptTb) FROM MEMVAR  
ENDIF
*B603858,1 3)Adding Reason to indicate Error data in Region field [End]
                 
*-- Adding the Class Code.
DO CASE
  CASE M.PRIORITY == "4"
    M.CLASS = "000007"
  CASE M.PRIORITY == "2"
    M.CLASS = "000004"
  OTHERWISE
    M.CLASS = "000006"      
ENDCASE

M.CLASS = IIF(SEEK('N'+PADR('CLASS',10)+M.CLASS,'Codes'),;
          M.CLASS,IIF(SEEK('D'+PADR('CLASS',10),'Codes'),Codes.cCode_No,M.CLASS))

*-- Adding the Ship Via Code.
DO CASE
  CASE ALLTRIM(T) == "SPECIAL INSTRUCTION"
    M.SHIPVIA = "000153"
  CASE ALLTRIM(T) == "UPS GROUND"
    M.SHIPVIA = "000092"
  CASE ALLTRIM(T) == "PG"
    M.SHIPVIA = "000092"
  CASE ALLTRIM(T) == "CG"
    M.SHIPVIA = "000092"
  OTHERWISE
    M.SHIPVIA = "000092"      
ENDCASE

M.SHIPVIA = IIF(SEEK('N'+PADR('SHIPVIA',10)+M.SHIPVIA,'Codes'),;
            M.SHIPVIA,IIF(SEEK('D'+PADR('SHIPVIA',10),'Codes'),Codes.cCode_No,M.SHIPVIA))
                
*-- Adding Division code.
M.CDIVISION  = SUBSTR(ALLTRIM(A),1,2)
 
*-- Adding the Link_Code.
DO CASE
  CASE M.CDIVISION == "01"
    M.LINK_CODE = "DEFDEF"
  CASE M.CDIVISION == "06"
    M.LINK_CODE = "KIDDEF"
  CASE M.CDIVISION == "08"
    M.LINK_CODE = "PLBDEF"
ENDCASE  

M.CDIVISION = IIF(SEEK('N'+PADR('CDIVISION',10)+M.CDIVISION,'Codes'),;
              M.CDIVISION,'')

*B603858,1 3)Adding Reason to indicate Error data in Link_Code field [Begin]
IF EMPTY(M.LINK_CODE)
  SCATTER MEMVAR MEMO
  M.REASON = "This Record was added in Customer file with an empty data in Link Code field due to a wrong Division code, Whereby Link Code is based on Division." 
  INSERT INTO (lcExceptTb) FROM MEMVAR
ENDIF
*B603858,1 3)Adding Reason to indicate Error data in Link_Code field [End]
  
*-- If Account is not found.
IF SEEK('M'+M.ACCOUNT,'Customer')
  STORE "" TO M.CONSOL , M.PRIORITY , M.REGION , M.CLASS , M.CTERMCODE , M.CDIVISION , ;
              M.SPCINST , M.CINSUR , M.PRICELVL , M.PRNT_STATM , M.CCURRCODE , ;
              M.CSLSGLLINK , M.LINK_CODE
      
  *-- Adding a record for the Store only.
  M.TYPE    = "S"
  M.STORE   = ALLTRIM(C)
      
  *-- Adding the Bill To (Name and Addresses). 
  M.BTNAME     = Customer.BtName
  M.CADDRESS12 = Customer.cAddress12
  M.CADDRESS22 = Customer.cAddress22
  M.CADDRESS32 = Customer.cAddress32
  M.CADDRESS42 = Customer.cAddress42
  M.CADDRESS52 = Customer.cAddress52

  *-- Adding the Ship To (Name and Addresses).
  M.STNAME    = SUBSTR(ALLTRIM(M),1,30)
  M.CADDRESS1 = SUBSTR(ALLTRIM(N),1,30)
  M.CADDRESS2 = SUBSTR(ALLTRIM(O),1,30)
  M.CADDRESS3 = SUBSTR(ALLTRIM(P),1,30)
  M.CADDRESS4 = SUBSTR(ALLTRIM(Q),1,30)
  M.CADDRESS5 = SUBSTR(ALLTRIM(R),1,30)
  
  INSERT INTO Customer FROM MEMVAR
ELSE         && Else Account is not found
  *-- Adding a record for the Main account.
  M.TYPE  = "M"
  M.STORE = ""

  *-- Adding the Bill To (Name and Addresses). 
  M.BTNAME     = SUBSTR(ALLTRIM(D),1,30)
  M.CADDRESS12 = SUBSTR(ALLTRIM(E),1,30)
  M.CADDRESS22 = SUBSTR(ALLTRIM(F),1,30)
  M.CADDRESS32 = SUBSTR(ALLTRIM(G),1,30)
  M.CADDRESS42 = SUBSTR(ALLTRIM(H),1,30)
  M.CADDRESS52 = SUBSTR(ALLTRIM(J),1,30)
  
  *-- Adding the Ship To (Name and Addresses).
  M.STNAME    = M.BTNAME
  M.CADDRESS1 = M.CADDRESS12
  M.CADDRESS2 = M.CADDRESS22
  M.CADDRESS3 = M.CADDRESS32
  M.CADDRESS4 = M.CADDRESS42
  M.CADDRESS5 = M.CADDRESS52

  INSERT INTO Customer FROM MEMVAR
      
  STORE "" TO M.CONSOL , M.PRIORITY , M.REGION , M.CLASS , M.CTERMCODE , M.CDIVISION , ;
              M.SPCINST , M.CINSUR , M.PRICELVL , M.PRNT_STATM , M.CCURRCODE , ;
              M.CSLSGLLINK , M.LINK_CODE
      
  *-- Adding a record for the 1st Store only.
  M.TYPE    = "S"
  M.STORE   = ALLTRIM(C)
      
  *-- Adding the Ship To (Name and Addresses).
  M.STNAME    = SUBSTR(ALLTRIM(M),1,30)
  M.CADDRESS1 = SUBSTR(ALLTRIM(N),1,30)
  M.CADDRESS2 = SUBSTR(ALLTRIM(O),1,30)
  M.CADDRESS3 = SUBSTR(ALLTRIM(P),1,30)
  M.CADDRESS4 = SUBSTR(ALLTRIM(Q),1,30)
  M.CADDRESS5 = SUBSTR(ALLTRIM(R),1,30)

  INSERT INTO Customer FROM MEMVAR
                
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
