*:********************************************************************
*: Program file  : SOGI100.PRG        (Refere To. C#101856)
*: Program desc. : DATA CONVERT PROGRAM
*:                 This program is used to convert the (Custmas, and ShipTo)
*:                 text file to Aria Customer.dbf file
*:                 The program is a conversion from 2.6 ==> 2.7
*: For screen    : 
*:           Date: 05/25/2000
*:         System: ARIA APPAREL SYSTEM Ver 2.7 
*:         Module: PROGRAM
*:      Developer: Wael Mohammed Anwar (WMA)
*!********************************************************************
*: Calls         : lfOpnFile()
*:                 lfCustConv()
*:                 lfShipConv()
*:                 lfUpdCust()
*:                 lfCustDep()
*:                 lfCustNote()
*:********************************************************************
*: Notes :
*: Steps to excute this program
*: - This program should be run as a custom program from Aria. 
*: - The files that will be converted (CUSTMAS.TXT, and SHIP_TO_.TXT, 
*:   should be in the same directory where the database files are.
*: - The CUSTOMER.DBF, CUSTDEPT.DBF, and NOTEPAD files should be empty 
*:   and also in the same directory.
*:********************************************************************
*: Passed Parameters  :
*:********************************************************************
*: Example            : DO SOOGI100.PRG
*:********************************************************************

*-- Initialize variables. 
CustTmp = gfTempName()
ShipTmp = gfTempName()
lnOpnFile  = 0
lnOpnFile1 = 0

DIMENSION laCodes [5,2]
STORE '' TO lcClass , lcTermCode , lcRegion , lcShipVia , lcSpcinst

*-- End initialize variables. 

*-- This function to open the necessary files
= lfOpnFile()

*-- Cheak if fiels code was setup or not.
SELECT CODES
SET ORDER TO TAG Ccode_no IN CODES
IF !SEEK('N'+'CLASS','CODES')     OR ;
   !SEEK('N'+'CTERMCODE','CODES') OR ;
   !SEEK('N'+'REGION','CODES')    OR ;
   !SEEK('N'+'SHIPVIA','CODES')   OR ;
   !SEEK('N'+'SPCINST','CODES')
  *- Message No. : 40150
  *- Message Txet: One or more codes have not been setup. Please setup and try again.
  *- Button No   : 00000
  *- Button Text : Ok
  =gfModalGen('TRM40150B00000','ALERT')
  RETURN
ENDIF

*-Get the default codes set up.
=SEEK('D'+'CLASS','CODES')
lcClass    = Codes.ccode_no

=SEEK('D'+'CTERMCODE','CODES')
lcTermCode = Codes.ccode_no

=SEEK('D'+'REGION','CODES')
lcRegion   = Codes.ccode_no

=SEEK('D'+'SHIPVIA','CODES')
lcShipVia  = Codes.ccode_no

=SEEK('D'+'SPCINST','CODES')
lcSpcinst  = Codes.ccode_no

IF lfCustConv() AND  lfShipConv()
  = lfUpdCust()  
  = gfModalGen('INM00370B00000','DIALOG')
  = lfEraseTemp()  
ENDIF
= FCLOSE(lnOpnFile)
= FCLOSE(lnOpnFile1)
*- End OF Program

*!*************************************************************
*! Name      : lfOpnFile
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To open the required files for selecting data 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfOpnFile()
*!*************************************************************
FUNCTION lfOpnFile

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
=gfOpenFile(gcDataDir+'NOTEPAD' ,gcDataDir+'NOTEPAD' ,'SH')
=gfOpenFile(gcDataDir+'CODES' ,gcDataDir+'CODES' ,'SH')




*!*************************************************************
*! Name      : lfCustConv
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To Converte the custmas.Txt to a CustTmp database
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfCustConv()
*!*************************************************************
FUNCTION lfCustConv
*-- Create the Tmporary file to hold the custmas data
CREATE TABLE (gcWorkDir+CustTmp);
             (cField1  C(5)  , cField2  C(16) , cField3  C(16), cField4  C(3) , cField5  C(3) ,;
              cField6  N(6,3), cField7  N(5,2), cField8  N(9) , cField9  C(6) , cField10 N(3) ,;
              cField11 C(1)  , cField12 C(30) , cField13 C(30), cField14 C(30), cField15 C(30),;
              cField16 C(30) , cField17 C(30) , cField18 C(30), cField19 M    , cField20 M)

INDEX ON cField1 TAG (CustTmp)

*-- The fields in the temporary file that are equivelatn to the one
*-- in the customer file and they are as the following :
*cField1  = Account  , cField2  = Phone1   , cField3  = Fax      , cField4  = SelesRep , cField5  = Rep2 
*cField6  = nTaxRate , cField7  = Comm     , cField8  = Crlimit  , cField9  = CFacCode , cField10 = ShipToNo
*cField11 = Status   , cField12 = CAddress1, cField13 = CAddress2, cField14 = CAddress3, cField15 = ""
*cField16 = CAddress5, cField17 = CAddress4, cField18 = BtName   , cField19 = mNote1   , cField20 = mNote2 

IF !FILE(gcDataDir+'CUSTMAS.TXT')
  =gfModalGen(.f.,.f.,.f.,.f.,'The custmas.txt file does not exists')  
  RETURN(.F.)
ELSE
  WAIT WINDOW 'Opening  the Custmas.Txt'  NOWAIT
  lnOpnFile = FOPEN(gcDataDir+'CUSTMAS.TXT')
  
  IF lnOpnFile = -1
    =gfModalGen(.f.,.f.,.f.,.f.,'The file cannot be open.')
    return .F.
  ENDIF
  
  WAIT WINDOW 'Converting the Custmas.Txt'  NOWAIT  
  *-- Determine file size, assign to lnFileSize
  STORE FSEEK(lnOpnFile, 0, 2) TO lnFileSize
   
  *-- To move the pointer to the begining of the file
  =FSEEK(lnOpnFile, 0)

  DO WHILE !FEOF(lnOpnFile)
    SELECT (CustTmp)
    APPEND BLANK
    lcText = FGETS(lnOpnFile,lnFileSize)   

    FOR lnCounter = 1 TO 20
      lcCounter   = ALLTRIM(STR(lnCounter,2))
      lnFirstPos  = ATC(",",lctext,lnCounter)
      lnSecndPos  = ATC(",",lctext,lnCounter+1)
      lcValue     = SUBSTR(lctext,lnFirstPos+1,lnSecndPos-lnFirstPos-1)
      
      IF TYPE('cField'+lcCounter) = "N"
        IF OCCURS(".", lcValue) > 0
          SET DECIMALS TO LEN(SUBSTR(lcValue,ATC(".",lcValue)+1))
        ENDIF  
        lnToReplac = VAL(lcValue)
      ELSE
        lnToReplac = lcValue
      ENDIF  
      REPLACE cField&lcCounter WITH lnToReplac
    ENDFOR 
  ENDDO  
ENDIF

*- Cheak if account is exist before don't convert.
IF SEEK('M'+cField1,'CUSTOMER')
  =gfModalGen(.f.,.f.,.f.,.f.,'This account is exist ,Cannot proceed!.')
  RETURN .F.
ENDIF

WAIT CLEAR

*!*************************************************************
*! Name      : lfShipConv
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To Converte the ShipTo.Txt to a ShipTmp database
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfShipConv()
*!*************************************************************
FUNCTION lfShipConv

*-- Create the Tmporary file to hold the ShipTo data
CREATE TABLE (gcWorkDir+ShipTmp); 
             (cField1 C(5)  , cField2 C(8) , cField3 C(30), cField4 C(16), cField5 C(16) ,;
              cField6 C(5)  , cField7 C(30), cField8 C(30), cField9 C(30), cField10 C(10),;
              cField11 C(30), cField12 M   , cField13 M   , cField14 M   , cField15 N(6,3))
INDEX ON cField1 TAG (ShipTmp)

*-- The fields in the temporary file that are equivelatn to the one
*-- in the customer file and they are as the following :
*cField1  = Account   , cField2  = Store , cField3  = CAddress42 , cField4  = Pohne1
*cField5  = Fax       , cField6  = Dept  , cField7  = CAddress12 , cField8  = CAddress22
*cField9  = CAddress32, cField10 = ""    , cField11 = CAddress52 , cField12 = Note1
*cField13 = Note2     , cField14 = Note3 , cField15 = nTaxRate

IF !FILE(gcDataDir+'SHIP_TO_.TXT')
  *= gfModalGen('TRM00273B00000','DIALOG')
  =gfModalGen(.f.,.f.,.f.,.f.,'The ship_to_.txt file does not exists')  
  RETURN(.F.)
ELSE
  WAIT WINDOW 'Converting the Ship_To.Txt' NOWAIT

  lnOpnFile1 = FOPEN(gcDataDir+'SHIP_TO_.TXT')
  
  *-- Determine file size, assign to lnFileSize1
  STORE FSEEK(lnOpnFile1, 0, 2) TO lnFileSize1

  =FSEEK(lnOpnFile1, 0)
  DO WHILE !FEOF(lnOpnFile1)
    SELECT (ShipTmp)
    APPEND BLANK
    lcText = FGETS(lnOpnFile1,lnFileSize1)
    FOR lnCounter = 1 TO 15
      lcCounter   = ALLTRIM(STR(lnCounter,2))
      lnFirstPos  = ATC(",",lctext,lnCounter)
      lnSecndPos  = ATC(",",lctext,lnCounter+1)
      lcValue = SUBSTR(lctext,lnFirstPos+1,lnSecndPos-lnFirstPos-1)

      
      IF TYPE('cField'+lcCounter) = "N"
        IF OCCURS(".", lcValue) > 0
          SET DECIMALS TO LEN(SUBSTR(lcValue,ATC(".",lcValue)+1))
        ENDIF  
        lnToReplac = VAL(lcValue)
      ELSE
        lnToReplac = lcValue
      ENDIF  
      REPLACE cField&lcCounter WITH lnToReplac
      
    ENDFOR 
  ENDDO  
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfUpdCust
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To update the customer file from the two temporary files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfUpdCust()
*!*************************************************************
FUNCTION lfUpdCust

SELECT (ShipTmp)
GOTO TOP

SELECT (CustTmp)
GOTO TOP
SCAN 
  IF VAL(cField11) <> 1     && 1 means that it is a non-active customer 
                            && therefore it should not be replaced   
    SELECT Customer
    APPEND BLANK
    REPLACE Type       WITH 'M'        ,;
            Account    WITH &CustTmp..cField1  ,;
            consol     WITH 'N'                ,;
            Phone1     WITH &CustTmp..cField2  ,;
            Fax        WITH &CustTmp..cField3  ,;
            SalesRep   WITH &CustTmp..cField4  ,;
            Rep2       WITH &CustTmp..cField5  ,;
            nTaxRate   WITH &CustTmp..cField6  ,;
            cTaxRule   WITH '1'                ,;
            Comm       WITH &CustTmp..cField7  ,;
            Crlimit    WITH &CustTmp..cField8  ,;
            CFacCode   WITH &CustTmp..cField9  ,;
            Status     WITH 'A'        ,;
            CAddress1  WITH &CustTmp..cField12 ,;
            CAddress2  WITH &CustTmp..cField13 ,;
            CAddress3  WITH &CustTmp..cField14 ,;
            CAddress5  WITH &CustTmp..cField16 ,;
            CAddress4  WITH &CustTmp..cField17 ,;
            BtName     WITH &CustTmp..cField18 ,;
            CAddress12 WITH CAddress1  ,;
            CAddress22 WITH CAddress2  ,;
            CAddress32 WITH CAddress3  ,;
            CAddress52 WITH &CustTmp..cField16 ,;
            CAddress42 WITH &CustTmp..cField17 ,;
            StName     WITH &CustTmp..cField18             
    REPLACE Class      WITH lcClass    ,;
            CtermCode  WITH lcTermCode ,;
            ShipVia    WITH lcShipVia  ,;
            Priority   WITH '5'        ,;
            PriceLvl   WITH 'A'        ,;
            Region     WITH lcRegion   ,;
            SpcInst    WITH lcSpcinst  ,;
            CInsur     WITH 'N'        ,;
            Billto     WITH 'M'        ,;
            Link_Code  WITH 'DEFDEF'   ,;
            cSlsGlLink WITH 'DEF'      ,;
            SkuTmpl    WITH 'DEF'      ,;
            Ccont_Code WITH gcContCode ,;
            Ccont_Cod2 WITH gcContCode ,;
            CcurrCode  WITH gcBaseCurr ,;
            dAdd_Date  WITH Date()     ,;
            cAdd_Time  WITH gfGettime(),;
            cAdd_User  WITH gcUser_Id  

    IF !EMPTY(&CustTmp..cField19) .OR. !EMPTY(&CustTmp..cField20) 
      REPLACE lhasnotes WITH .T.
    ENDIF 
    *-- Function to replace the notes in the notepad
    =lfCustNote(&CustTmp..cField19+CHR(13)+&CustTmp..cField20+CHR(13))

    *-- if there is at least one store or more         
    SELECT (ShipTmp)
    IF  SEEK (&CustTmp..cField1)

      *-- Save the pointer of the file
      lcExp = &CustTmp..cField1
      
      *-- Count how many stores for a specific account
      COUNT REST WHILE cField1 = &CustTmp..cField1 TO lnRecNo
      
      *-- Restore the pointer's file
      SEEK lcExp
      
      *-- If there is only one store for the customer
      IF lnRecNo = 1 
        *-- If the ship to address is not the same as Bill to address
        lcString1 = cField7+cField8+cField9+cField11+cField3   
        lcString2 = &CustTmp..cField12+&CustTmp..cField13+&CustTmp..cField14+&CustTmp..cField16+&CustTmp..cField17
        IF lcString1 <> lcString2
          =lfAddStore()
        ENDIF  
      ELSE
        *-- cField1 = Account
        SCAN WHILE cField1 = &CustTmp..cField1
          =lfAddStore()
        ENDSCAN
      ENDIF  
    ENDIF  
    WAIT WINDOW 'Account : ' +&CustTmp..cField1  NOWAIT
  ENDIF  
ENDSCAN

*!*************************************************************
*! Name      : lfCustDep
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To replace the Dept from the ShipTmp file to CustDept file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfCustDep()
*!*************************************************************
FUNCTION lfCustDep
PRIVATE lnAlias

lnAlias = SELECT()

SELECT CustDept
IF !EMPTY(&ShipTmp..cField6) AND !SEEK(Customer.Account+&ShipTmp..cField6)
  APPEND BLANK
  REPLACE Account WITH Customer.Account ,;
          Dept    WITH &ShipTmp..cField6,;
          Rep1    WITH &CustTmp..cField4
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCustNote
*! Developer : Wael Mohammed Anwar (WMA) 
*! Date      : 05/25/2000
*! Purpose   : To replace the notes in the notepad file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcToAdd,lcType
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfCustNote()
*!*************************************************************
FUNCTION lfCustNote
PARAMETERS lcToAdd, lcType
PRIVATE lnAlias

lnAlias = SELECT()
SELECT NotePad

IF !SEEK ('A' + Customer.Account) 
  APPEND BLANK
  REPLACE Type   WITH 'A'              ,;
          Key    WITH Customer.Account ,;
          cDesc  WITH 'Notes For Account Number : '+ Customer.Account
ENDIF
REPLACE mNotes WITH mNotes + CHR(13) + lcToAdd

SELECT (lnAlias)                            

*!*************************************************************
*! Name      : lfAddStore
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To Add a store record in the customer file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfAddStore()
*!*************************************************************
FUNCTION lfAddStore
PRIVATE lnAlias

lnAlias = SELECT()

SELECT Customer
APPEND BLANK
REPLACE Type       WITH 'S'        ,;
        Account    WITH &ShipTmp..cField1  ,;
        consol     WITH 'N'                ,;
        Store      WITH &ShipTmp..cField2  ,;
        CAddress42 WITH &ShipTmp..cField3  ,;
        Phone1     WITH &ShipTmp..cField4  ,;
        Phone2     WITH &ShipTmp..cField5  
                     
REPLACE CAddress12 WITH &ShipTmp..cField7  ,;
        CAddress22 WITH &ShipTmp..cField8  ,;
        CAddress32 WITH &ShipTmp..cField9  ,;            
        CAddress52 WITH &ShipTmp..cField11 ,;
        nTaxRate   WITH &ShipTmp..cField15 ,;
        StName     WITH &CustTmp..cField17 ,;
        CAddress1  WITH &CustTmp..cField12 ,;
        CAddress2  WITH &CustTmp..cField13 ,;
        CAddress3  WITH &CustTmp..cField14 ,;
        CAddress4  WITH &CustTmp..cField15 ,;
        CAddress5  WITH &CustTmp..cField16 ,;
        CAddress4  WITH &CustTmp..cField17 ,;
        Status     WITH 'A'        ,;
        Class      WITH lcClass    ,;
        CtermCode  WITH lcTermCode ,;
        ShipVia    WITH lcShipVia  ,;
        Priority   WITH '5'        ,;
        PriceLvl   WITH 'A'        ,;
        Region     WITH lcRegion   ,;
        SpcInst    WITH lcSpcinst  ,;
        CInsur     WITH 'N'        ,;
        Billto     WITH 'M'        ,;
        Link_Code  WITH 'DEFDEF'   ,;
        cSlsGlLink WITH 'DEF'      ,;
        SkuTmpl    WITH 'DEF'      ,;
        Ccont_Code WITH gcContCode ,;
        Ccont_Cod2 WITH gcContCode ,;
        CcurrCode  WITH gcBaseCurr ,;
        dAdd_Date  WITH Date()     ,;
        cAdd_Time  WITH gfGettime(),;
        cAdd_User  WITH gcUser_Id  
        
                   
        *-- Function to save the notes in the notepad
        =lfCustNote(&ShipTmp..cField12+CHR(13)+&ShipTmp..cField13+CHR(13)+&ShipTmp..cField14+CHR(13))

        *-- Function to replace the department code in the
        *-- CustDept file
        =lfCustDep() 

SELECT (lnAlias) 



*!*************************************************************
*! Name      : lfEraseTemp
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/25/2000
*! Purpose   : To Erase Temporary Files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfEraseTemp()
*!*************************************************************
FUNCTION lfEraseTemp

SELECT (CustTmp)
IF FILE(gcWorkDir+CustTmp)
  ERASE (gcWorkDir + lcRpTmp + '.DBF')
  ERASE (gcWorkDir + lcRpTmp + '.CDX')
  ERASE (gcWorkDir + lcRpTmp + '.FPT')
ENDIF

SELECT (ShipTmp)                           
IF FILE(gcWorkDir+ShipTmp)
  ERASE (gcWorkDir + lcRpTmp + '.DBF')
  ERASE (gcWorkDir + lcRpTmp + '.CDX')
  ERASE (gcWorkDir + lcRpTmp + '.FPT')
ENDIF
*!*************************************************************