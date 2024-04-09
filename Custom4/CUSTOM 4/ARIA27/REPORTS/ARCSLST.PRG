*:***************************************************************************
*: Program file  : ARCSLST
*: Program desc. : Customers Master List
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : Hossam El Etreby (HDM)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO ARCSLST
*:***************************************************************************
*: Modifications :
*:               B602375,1 HDM 12/22/1998 If No records found Return
*:               B602383,1 HDM 01/04/1999 Bug of Ship to priority
*:               B802045,1 HDM 03/17/1999 Increase the length of 
*:                                        printed division description
*:                                        to 23 Chars.
*:               B802062,1 HDM 03/24/1999 Fix Bug of state and zip sorting
*:                                        was not working
*:               B802194,1 HDM 04/26/1999 Fixing bug of not printing some ship to(s)
*:                                        case sorting by zip
*:               B602924,1 BWA 05/23/1999 Fix the bug of leaving the temporary files in 
*:               B602924,1 BWA            the work directory. 
*:               B802307,1 SSE 05/27/1999 Correct Spelling mistakes in OG
*:               E301353,1 SSE 12/29/1999 Convert to Graphics format
*:B603650,1 BWA 05/17/2000 Fix the bug of Title not centered in the reports [Fix in Long & short FRX ].
*:B802965,1 MHM 07/02/2000 Adding fax # in [ Long & short FRX ].
*:B803412,1 BWA 07/25/2000 Fix the bug of cancel account doesn't print.
*:B603886,1 BWA 09/11/2000 Fix the bug of salesrep option dosen't filter for the 2nd customer salesrep.
*!B802599,1 MHM 11/16/2000 1- Fix the bug of no stores if we filter with Class and print Ship To is true .
*!B802599,1                2- Fix the bug of Spelling mistakes .
*!B802599,1                3-FIX some bugs found in testing {missing .).}
*!B604709,1 AME 07/25/2001 some chages in arcslstl.frx
*:B606438,1 ADEL 10/23/02  Fix the bug of overwriting the Stores' Shipvias with Main's one
*:B606438,1                as it can be changed at the store level.
*:***************************************************************************

*E301353 Declare laSoldTo,laShipTo array to be used in FRX for Sold to,Ship to Addresses [Begin.]
DIMENSION laSoldTo[5] , laShipTo[5]
DIMENSION laCodDesc[5,3]     && Declare Code Description Array 
STORE '' TO laSoldTo , laShipTo , laCodDesc , lcShipVia , lcSRepName
*E301353 Declare laSoldTo,laShipTo array to be used in FRX for Sold to,Ship to Addresses [End.]

*!B802599,1 MHM 11/16/2000 [start]
*STORE '' TO lcNotes
STORE '' TO lcNotes,lcTempFl
lcTempFl = gfTempName() && Cursor Temp Name
*!B802599,1 MHM 11/16/2000 [end]

TEMP = gfTempName() && Cursor Temp Name
STORE '' TO XFILTER
STORE .F. TO llNoRec

QSTATE = lcSTitle && State Title
QZIP   = lcZTitle && Zip Title
QSTATESORT = lcRpSort
QZIPSORT   = lcRpSort
*-- Get Account Values
lcAccVal   = laOGFxFlt[lnRepPos,6]
lnSepPos   = ATC('|',lcAccVal)
LAC        = SUBSTR(lcAccVal , 1 , lnSepPos - 1)
HAC        = SUBSTR(lcAccVal , lnSepPos + 1 ,LEN(lcAccVal))

*-- Get Rest of old 2.6 Values
XFC        = laOGFxFlt[lnFactCode,6]
XSALS      = laOGFxFlt[lnRepPos,6]
*XPRT       = laOGFxFlt[lnPrtCode,6]
*--HDM B602383,1 [START]
lcPrtExp = '.T.'
lcXAcc=''
XPRT = ''

IF !EMPTY(lcPrt)
  *XPRT = [.AND. PRIORITY='] + lcPrt + [' .OR. PRIORITY =' ' AND TYPE='S']
  XPRT = [.AND. PRIORITY='] + lcPrt + [']
  lcRpExp = IIF(EMPTY(lcRpExp), SUBSTR(XPRT,5,LEN(XPRT)) ,lcRpExp + XPRT)
ENDIF

*--HDM B602383,1 [END]
ST         = IIF(llRpShipTo,'Y','N')
ST1        =''
ST2        =''
ST3        =''
ST4        =''
XRG        = laOGFxFlt[lnRegion  , 6]
XTR        = laOGFxFlt[lnTerms   , 6]
XCLS       = laOGFxFlt[lnClass   , 6]
XSP        = laOGFxFlt[lnSpInst  , 6]
XSHP       = laOGFxFlt[lnShipVia , 6]
XZP        = laOGFxFlt[lnZip     , 6]
XST        = lcRpStatus
XDV        = laOGFxFlt[lnDivision, 6]

XPRINTNOTE = llRpNote
XREP_FORM  = lcRpType
XTITLE     = lcRpOptTtl
R_TITLE   = 'CUSTOMER MASTER LIST'
R_WIDTH   = 'W'      && STANDARD SIZE OF REPORT (WIDE)
ST        = 'N'
SORTCD    = lcRpSort
CNT1      = 0
CNT2      = 0
DO WHILE .T.
SELECT CUSTOMER
Z = ' '
TST = ST1+Z+ST2+Z+ST3+Z+ST4
*------------------------
* SORT PER USER REQUEST
*------------------------
    DO CASE
      CASE SORTCD $ ' A'
        SORTFLD= "ACCOUNT+TYPE+STORE"

      CASE SORTCD='N'
        SORTFLD="STNAME+TYPE+STORE"
      
      *-- B802062,1 HDM 03/24/1999[Start] We should Check for Letter 'S' For State Sorting
      *CASE SORTCD = UPPER(SUBSTR(QSTATESORT,ATC('<',QSTATESORT)+1,1))
      CASE SORTCD = 'S'
      
        *SORTFLD = "STSTATE+ACCOUNT+TYPE+STORE"
        SORTFLD = "CADDRESS4+ACCOUNT+TYPE+STORE"
      *-- B802062,1 HDM 03/24/1999[End]
                  
      CASE SORTCD='R'
        SORTFLD='REGION+SALESREP+ACCOUNT+TYPE+STORE'

      *-- B802062,1 HDM 03/24/1999[Start] We should Check for Letter 'Z' For Zip Sorting
      *CASE SORTCD = UPPER(SUBSTR(QZIPSORT,ATC('<',QZIPSORT)+1,1))
      *-- B802062,1 HDM 03/24/1999[End]

      CASE SORTCD = 'Z'
        SORTFLD='CADDRESS5+ACCOUNT+TYPE+STORE'

    ENDCASE
    EXIT
  ENDDO

ST = IIF(llRpShipTo = .T. , 'Y' , 'N')
IF ST='N'
  XFILTER = XFILTER + ".AND.TYPE='M'"
ENDIF

*-- B602375,1 HDM 12/22/1998 [start]
IF !EMPTY(XST)
  XFILTER= XFILTER + ".AND. STATUS $ (XST)"
ENDIF
*-- B602375,1 HDM 12/22/1998 [start]


*B603886,1 BWA 09/11/2000 Fix the bug of salesrep option dosen't filter for the 2nd customer salesrep.[START]
= lfAddRp2()
*B603886,1 [END]

XFILTER = lcRpExp + XFILTER

SELECT CUSTOMER
*!B802599,1 MHM 11/16/2000 [start]
*GO TOP
IF llRpShipTo
  =lpCreaTemp()
  =lfCollct()
  SELECT (lcTempFl)
ENDIF  
LOCATE 
  
*LOCATE ALL FOR &XFILTER
*IF EOF()
*  WAIT WINDOW 'NO RECORDS FOUND'
*  llNoRec = .T.
*  *-- B602375,1 HDM 12/22/1998 If No records found Return[start]
*  RETURN
*  *-- B602375,1 HDM 12/22/1998 If No records found Return[end]
*ENDIF
*!B802599,1 MHM 11/16/2000 [End]
IF !EMPTY(XFILTER)
  COPY REST TO (gcWorkDir + Temp) FOR &XFILTER

  *E301353,1 [Begin.] scan Customer file to add all Stores associated 
  *                   with main Account case collecting priority info.
  IF llRpShipTo AND !EMPTY(XPRT)
    IF !USED(Temp)
      USE (gcWorkDir + TEMP) IN 0
    ENDIF
    SELECT (Temp)
    
    *!B802599,1 MHM 11/16/2000 No need to go to first record, SCAN if enough[start]
    *GO TOP
    *!B802599,1 MHM 11/16/2000 [End]
    SCAN
      lcAccount = Account
      *!B802599,1 MHM 11/16/2000 [START]
      *IF SEEK('S'+lcAccount,'Customer')
      *SCAN REST WHILE Type+Account+Store = 'S'+lcAccount
      *  SCATTER MEMVAR MEMO
      *  INSERT INTO (Temp) FROM MEMVAR 
      *ENDSCAN
      *ENDIF
      SELECT (lcTempFl)
      IF SEEK('S'+lcAccount)
        SCAN REST WHILE Type+Account+Store = 'S'+lcAccount
          SCATTER MEMVAR MEMO
          INSERT INTO (Temp) FROM MEMVAR 
        ENDSCAN
      ENDIF
      SELECT (Temp)
       *!B802599,1 MHM 11/16/2000 [END]
    ENDSCAN
  ENDIF
  *E301353,1 scan Customer file to add all Stores associated with main Account [End.]    
ELSE
  COPY ALL TO (gcWorkDir + TEMP)
ENDIF

IF !USED(TEMP)
  USE (gcWorkDir + TEMP) IN 0
ENDIF

Z = LTRIM(STR(RECCOUNT(),6))

SELECT &TEMP
INDEX ON &SORTFLD  TAG &TEMP
SET ORDER TO TAG &TEMP

*E301353 Define laCodDesc Array and call Report FRX [Begin.]

laCodDesc[1,2] = "CTERMCODE"
laCodDesc[2,2] = "CDIVISION"
laCodDesc[3,2] = "REGION"
laCodDesc[4,2] = "SPCINST"
laCodDesc[5,2] = "CLASS"

DO gfDispRe WITH EVAL('lcRpName')
*E301353 Define laCodDesc Array and call Report FRX [End.]


IF .F.  && SSE
* CUSTOMER MASTER LIST
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3
*ACCOUNT#                     STORE#                        PHONE1              TERMS                    PRIORITY       UPS ZONE
*BILL TO NAME                 SHIP TO NAME                  SHIPVIA             DIVISION                 STATUS
*BILL TO ADDRESS              SHIP TO ADDRESS               BUYER               REGION                   FACTOR         UPS INSUR
*BILL TO ADDRESS              SHIP TO ADDRESS               SALESREP            SPECIAL INSTRUCS         FACT ACCT #    CREDIT LN.
*BILL TO <CITY/STATE/ZIP>     SHIP TO <CITY/STATE/ZIP>      COMM                CLASS                    DUNS#          CREDIT AVL.

H1 ='ACCOUNT#                      STORE#                        PHONE1              TERMS                    PRIORITY       UPS ZONE'
H2 ='BILL TO NAME                  SHIP TO NAME                  SHIPVIA             DIVISION                 STATUS'
H3 ='BILL TO ADDRESS               SHIP TO ADDRESS               BUYER               REGION                   FACTOR         UPS INSUR'
H4 ='BILL TO ADDRESS               SHIP TO ADDRESS               SALESREP            SPECIAL INSTRUCS         FACT ACCT #    CREDIT LN.'
H5 ='BILL TO '+PADR('<CITY/'+UPPER(QSTATE)+'/'+UPPER(SUBSTR(QZIP,1,4))+'>',21,' ')+' SHIP TO '+PADR('<CITY/'+UPPER(QSTATE)+'/'+UPPER(SUBSTR(QZIP,1,4))+'>',21,' ')+' COMM                CLASS                    DUNS#          CREDIT AVL.'

XCOUNTM = 0
XCOUNTS = 0

ROW     = 99
PAGENO  = 0
MAXROW  = 55
XREPORT = 'ARCUST'
*AAAAAAAAAAAAAAA

*AAAAAAAAAAAAAAA
*---------------------------------------------------------------------------
*************  START PRINTING *********************************************************

SET DEVICE TO SCREEN

lnInKey = IIF(qBatchMod,24,32)

*-- MAB
SET DEVICE TO PRINT
*-- MAB
lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

lcxAcc = ACCOUNT

DO WHILE .T.
 *--HDM B602383,1 [START]
 IF lcxAcc = ACCOUNT
 *--HDM B602383,1 [END]
  IF ROW > MAXROW
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,IIF(XREP_FORM='L','W','N')
    @ 05,00 SAY IIF(XREP_FORM='L',H1,SUBSTR(H1,1,79))
    @ 06,00 SAY IIF(XREP_FORM='L',H2,SUBSTR(H2,1,79))
    @ 07,00 SAY IIF(XREP_FORM='L',H3,SUBSTR(H3,1,79))
    @ 08,00 SAY IIF(XREP_FORM='L',H4,SUBSTR(H4,1,79))
    @ 09,00 SAY IIF(XREP_FORM='L',H5,SUBSTR(H5,1,79))

    @ 10,00 SAY REPLICATE('*',IIF(XREP_FORM='L',130,80))
    ROW = 11
  ENDIF

  SELECT CODES
  SET ORDER TO CODES IN CODES 
  
  *TERMS
  XTR_D = gfCodDes(&TEMP..CTERMCODE,'CTERMCODE')
  
  *REGIO
  XRG_D = gfCodDes(&TEMP..REGION,'REGION')

  *SPC INST.
  XSP_D= gfCodDes(&TEMP..SPCINST,'SPCINST')
  
  * CLASS
  XCLS_D = gfCodDes(&TEMP..CLASS,'CLASS')
  
  * SHIPVIA
  XSHP_D = gfCodDes(&TEMP..SHIPVIA,'SHIPVIA')
  
  * DIVISION
  XDIV_D = gfCodDes(&TEMP..CDIVISION,'CDIVISION')

  SELE SALESREP
  SEEK &TEMP->SALESREP
  XREP = NAME

  SELE &TEMP
  IF LEN(TRIM(STORE)) = 0
    XCOUNTM = XCOUNTM + 1
  ELSE
    XCOUNTS = XCOUNTS + 1
  ENDIF

  *LINE 1
  @ ROW,00 SAY ACCOUNT
  @ ROW,30 SAY IIF(LEN(TRIM(STORE))= 0 , '*MAIN*' , STORE)

  *B500772,1 HISH 06/27/95. ( Begin ) Display phone with new format.
   @ ROW,60 SAY PHONE1  PICTURE lcPhonPict SIZE 1,16
  *@ ROW,60 SAY PHONE1
  *B500772,1 HISH 06/27/95. ( End )
  
  IF XREP_FORM = 'L'
    @ ROW,080 SAY ALLTRIM(XTR_D)
    @ ROW,105 SAY ALLTRIM(PRIORITY)
    @ ROW,120 SAY ALLTRIM(UPSZONE)
  ENDIF
  ROW = ROW + 1

  *LINE 2
  
  @ ROW,00 SAY ALLTRIM(BTNAME)
  @ ROW,30 SAY ALLTRIM(STNAME)
  *--HDM 03/24/1999 [Start]print only 19 CHRs For Ship Via Field
  *@ ROW,60 SAY ALLTRIM(XSHP_D)
  @ ROW,60 SAY SUBSTR(ALLTRIM(XSHP_D),1,19)
  *--HDM 03/24/1999 [End]
  IF XREP_FORM='L'
    *--B802045,1 HDM 03/17/1999 Increase the length of printed division description to 23 Chars.[Start]
    *@ ROW,080 SAY SUBSTR(ALLTRIM(XDIV_D),1,2)
    @ ROW,080 SAY SUBSTR(ALLTRIM(XDIV_D),1,23)
    *--B802045,1 [End]
    @ ROW,105 SAY IIF (STATUS='A','ACTIVE','CANCEL')
  ENDIF
  ROW = ROW + 1

  *LINE 3
  @ ROW,00 SAY ALLTRIM(CADDRESS12)
  @ ROW,30 SAY ALLTRIM(CADDRESS1)
  @ ROW,60 SAY ALLTRIM(BUYER)
  IF XREP_FORM = 'L'
    @ ROW,080 SAY ALLTRIM(XRG_D)
    @ ROW,105 SAY IIF(LEN(TRIM(CFACCODE))=0,'NOT FACTORED',CFACCODE)
    @ ROW,120 SAY IIF(CINSUR='N','NO INSUR.','INSURED')
  ENDIF
  ROW = ROW + 1

  *LINE 4
  @ ROW,00 SAY ALLTRIM(CADDRESS22)
  @ ROW,30 SAY ALLTRIM(CADDRESS2)
  @ ROW,60 SAY ALLTRIM(SALESREP)
  @ ROW,64 SAY SUBSTR(XREP,1,15)
  IF XREP_FORM ='L'
    @ ROW,080 SAY ALLTRIM(XSP_D)
    @ ROW,105 SAY ALLTRIM(FACTACCT)
    @ ROW,120 SAY CRLIMIT
  ENDIF
  ROW = ROW + 1

  *LINE 5
  @ ROW,00 SAY SUBSTR(ALLTRIM(CADDRESS32),1,12)+' '+SUBSTR(ALLTRIM(CADDRESS42),1,9)+' '+SUBSTR(ALLTRIM(CADDRESS52),1,6)
  @ ROW,30 SAY SUBSTR(ALLTRIM(CADDRESS3),1,12)+' '+SUBSTR(ALLTRIM(CADDRESS4),1,9)+' '+SUBSTR(ALLTRIM(CADDRESS5),1,6)
  @ ROW,60 SAY COMM PICTURE '99.99'
  IF XREP_FORM='L'
    @ ROW,080 SAY ALLTRIM(XCLS_D)
    @ ROW,105 SAY ALLTRIM(DUNS)
    @ ROW,120 SAY CRAVAIL
  ENDIF
  ROW = ROW + 1

  XACCOUNT = IIF(TYPE='M',ACCOUNT,SPACE(5) )
  IF XPRINTNOTE .AND. !EMPTY(XACCOUNT)
    DO PRT_NOTE WITH 'A',XACCOUNT
  ENDIF

  @ ROW,00 SAY REPLICATE('-',IIF(XREP_FORM='L',130,80))
  ROW = ROW + 1
  SKIP
  IF EOF()
    EXIT
  ENDIF
  *--HDM B602383,1 [START]

  *--HDM B802194,1 if Account changed only we have to read from the file [START]
  *lcxAcc = IIF(TYPE = 'M' , ACCOUNT , lcxAcc)
  *--HDM B602383,1 [END]
  IF ACCOUNT <> lcXAcc
    lcXAcc = ACCOUNT
  ENDIF
  *--HDM B802194,1[end]
 ELSE

  SKIP
  IF EOF()
    EXIT
  ENDIF
  LOOP
 ENDIF
  IF EOF()
    EXIT
  ENDIF
ENDDO

@ ROW,00 SAY 'There are '+STR(XCOUNTM,4)+' main accounts'
ROW = ROW + 1
@ ROW,00 SAY 'There are '+STR(XCOUNTS,4)+" ship to's"

*-- MAB
*-- MAB DO ENDPRT

IF !llNoRec
  DO ENDREPORT
ENDIF
*-- MAB

SET DEVICE TO SCREEN

*B602924,1 05/23/1999 BWA Close the temporary files and erase it from the work directory
IF USED  (TEMP)
  SELECT (TEMP)
  USE 
ENDIF

*-- ERASE THE lcTempFile
ERASE (gcWorkDir+TEMP+'.DBF')
ERASE (gcWorkDir+TEMP+'.CDX')  
*B602924,1 05/23/1999 BWA  [END]

RETURN
ENDIF    && SSE

*---- MAB comment
* Every time you issue RETURN COMMAND in your code 
* set device to screen before you return.
*---- MAB

*-----                FUNCTIONS SECTION                 --------------------
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
DECLARE laRpSource[4],laRpTarget[1]

*--- Get Filter Postions in OG.
lnAccPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
lnFactCode = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CFACCODE'),1)
lnRepPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SALESREP'),1)
*--B602383,1 HDM 01/04/1999 Bug of Ship to priority[START]
*lnPrtCode  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.PRIORITY'),1)
*--B602383,1 HDM 01/04/1999 Bug of Ship to priority[END]
lnDivision = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CDIVISION'),1)
lnRegion   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.REGION'),1)
lnTerms    = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CTERMCODE'),1)
lnClass    = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
lnSpInst   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SPCINST'),1)
lnShipVia  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SHIPVIA'),1)
lnZip      = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CADDRESS5'),1)
*lnStatus   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.STATUS'),1)
lnStatus = lcRpStatus
STORE 'Active'    TO laRpSource[1] 
STORE ''          TO laRpTarget[1]
STORE 'Hold'      TO laRpSource[2] &&,laRpTarget[2]
STORE 'Potential' TO laRpSource[3]
STORE 'Canceled'  TO laRpSource[4]

*B803412,1 BWA 07/25/2000 Fix the bug of cancel account doesn't print.[START]
*lcRpStatus = 'AH'
lcRpStatus = 'AHPX'
*B803412,1 [END]

*---------------------
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*---------------------------------------------------------------------------

*!**************************************************************************
*! Name      : lfvFactor
*! Developer : Sameh Saiid Ezzat
*! Date      : 01/12/2000
*! Purpose   : Valid function for Factor
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvFactor()
*!**************************************************************************
*
FUNCTION lfvFactor
PRIVATE lcVar , lcObj , lnAlias

lnAlias = SELECT(0)
lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))

*-- IF Statment to check if we are going to Browse
IF '?' $ lcObj .OR. (!EMPTY(lcObj)) .AND. !(SEEK(ALLTRIM(lcObj),'SycFact'))
  SELECT SycFact
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  lcObj = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
&lcVar = lcObj      && Update the field

SELECT (lnAlias)
*-- End of lfvFactor.

*!*************************************************************
*! Name      : lfvSlsRep
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Valid function for salesrep
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSlsRep()
*!*************************************************************

FUNCTION lfvSlsRep
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 3 , '0'))
SELECT SALESREP
SET ORDER TO SALESREP

*-- IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND !(SEEK(ALLtRIM(lcObj) , 'SALESREP'))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  *!B802599,1 MHM 11/16/2000 fix Spelling mistakes [start]
  *lcBrFields = "RepCode :H = 'Sales Rerasentitive Code',;
                Name    :H = 'Name'"
               
  *lcFile_Ttl = "Sales Rerasentitive"
  lcBrFields = "RepCode :H = 'Sales Representative Code',;
                Name    :H = 'Name'"
               
  lcFile_Ttl = "Sales Representative"
  
  *!B802599,1 MHM 11/16/2000 fix Spelling mistakes [start]
  lcBrowCond = ''
  lNThing    = gfBrows(lcBrowCond,'REPCODE','laTemp')

  *-- IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF
&lcVar = lcObj      && Update the field
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Evaluates the oject's old value incase of 'ESC'
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************

FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************

FUNCTION lfStitle

SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStates
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Valid for the states selection
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStates()
*!*************************************************************

FUNCTION lfvStates

PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = PADR(EVALUATE(SYS(18)),6)  && Varible to hold  the value of the current GET field
*IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR('STATE',10) + ALLTRIM(lcStateVal),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   
   *-- HDM Fixing Bug of 'cComp_ID' Not Found[START]
   
   *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
     gcAct_Comp+"STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')

   IF gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = ;
     "N" + "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
   
     *-- HDM Fixing Bug of 'cComp_ID' Not Found[End]
   
     lcStateVal = laCodeFld[1]
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal
*-- end of lfvStates.
*---------------------------------------------------------------------------
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************

FUNCTION lfSortDumy

DIMENSION laSortDesc[5,1] , laSortVal[5,1]

laSortDesc[1] = 'Customer Account'
laSortDesc[2] = 'Customer Name'
laSortDesc[3] = lcSTitle
laSortDesc[4] = lcZTitle
laSortDesc[5] = 'Region'

laSortVal[1] = 'A'
laSortVal[2] = 'N'
laSortVal[3] = 'S'
laSortVal[4] = 'Z'
laSortVal[5] = 'R'
*---------------------------------------------------------------------------
FUNCTION lfvZip
*---------------------------------------------------------------------------

FUNCTION lfZtitle
SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB

RETURN (SYCINT.CPART5LAB)
*-- end of lfStitle.
*--------------------------------------------------------------------------
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
*!B802599,1 MHM 11/16/2000 this is for Customer not Order[start]
*- gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= gfMover(@laRpSource,@laRpTarget,'Select Customer Status',.T.,'')  && call mover function.
*!B802599,1 MHM 11/16/2000 [end]
lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active','A',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Potential','P',;
                            IIF(laRpTarget[lnI] = 'Canceled','X',''))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.

*!**************************************************************************
*! Name      : lfvType
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Valid for Report Type
*!**************************************************************************
*! Called from : OG 
*!**************************************************************************
*! Example     : = lfvType()
*!**************************************************************************
*E301353,1 it's done especially to assign Short or Long form to Report name Frx
FUNCTION lfvType
IF lcRpType = 'S'
  lcRpName = 'ARCSLSTS'      && Short form 
ELSE
  lcRpName = 'ARCSLSTL'      && Long form
ENDIF
*-- End of lfvType.

*!**************************************************************************
*! Name      : lfAddrDesc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Counts Main Account Or Stores
*!**************************************************************************
*! Called from : FRX 
*!**************************************************************************
*! Return      : Arrays filled with Sold To, Ship To Addresses , Code Description
*!**************************************************************************
*! Example     : = lfAddrDesc()
*!**************************************************************************
*E301353,1 it's done especially for FRX to get addresses and Code descriptions
FUNCTION lfAddrDesc

laCodDesc[1,1] = &Temp..CTERMCODE
laCodDesc[2,1] = &Temp..CDIVISION
laCodDesc[3,1] = &Temp..REGION
laCodDesc[4,1] = &Temp..SPCINST
laCodDesc[5,1] = &Temp..CLASS

=gfGetAdr(Temp , '' , '' , '' , @laSoldTo,'2')
=gfGetAdr(Temp , '' , '' , '' , @laShipTo) 

lcShipVia = gfCodDes(ShipVia,'ShipVia')   && get code description for ShipVia
IF lcRpType = 'L'
  =gfCodDes(@laCodDesc)
ENDIF

= SEEK ('A' + IIF(TYPE='M',Account,SPACE(5)),'NotePad')  && Seek in NotePad file to get Customer Notes
IF !EOF('NOTEPAD')
  lcNotes = NOTEPAD.mNotes
ELSE
  lcNotes = ''
ENDIF

lcSRepName = IIF(SEEK(SalesRep,'SalesRep'),SalesRep.Name,'') 

RETURN ''
*-- End of lfAddrDesc.

*!**************************************************************************
*! Name      : lfPrtNotes
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Detect whether to print Customer Notepad or not
*!**************************************************************************
*! Called from : Windows FRX only
*!**************************************************************************
*! Return      : True or False
*!**************************************************************************
*! Example     : = lfPrtNotes()
*!**************************************************************************
*E301353,1 it's done especially for FRX to get addresses and Code descriptions
FUNCTION lfPrtNotes
RETURN llRpNote AND !EMPTY(lcNotes)
*-- End of lfPrtNotes.


*B603886,1 BWA 09/11/2000 Fix the bug of salesrep option dosen't filter for the 2nd customer salesrep.[START]
*!*************************************************************
*! Name      : lfAddRp2
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/09/2000
*! Purpose   : Add the sales rep2 to the filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddRp2()
*!*************************************************************
FUNCTION lfAddRp2
PRIVATE lcDatRep
STORE  "" TO lcDatRep
STORE  0 TO lnDatRep
 
lnDataRep = ASCAN(laOgFxFlt,'CUSTOMER.SALESREP')
IF lnDataRep > 0
  lnDataRep  = ASUBSCRIPT(laOgFxFlt,lnDataRep,1)
  lnDatRep = AT('CUSTOMER.SALESREP',LCRPEXP)

  IF lnDatRep > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatRep))
    IF lnPos1 > 0
      *!B802599,1 MHM 11/16/2000 [start]
      *lcDatRep = SUBSTR(lcRpExp ,lnDatRep , lnPos1-1)
      lcDatRep = SUBSTR(lcRpExp ,lnDatRep , lnPos1-2)
      *!B802599,1 MHM 11/16/2000 [end]
      lcRpExp  = STRTRAN(lcRpExp , SUBSTR(lcRpExp,lnDatRep,lnPos1+3),"")
    ELSE
      lnPos2   = LEN(lcRpExp)
      lcDatRep = SUBSTR(lcRpExp ,lnDatRep)
      lcRpExp  = STRTRAN(lcRpExp , SUBSTR(lcRpExp,lnDatRep,lnPos2),"")
    ENDIF
    lcDatRep2 = STRTRAN(lcDatRep,"CUSTOMER.SALESREP","CUSTOMER.REP2")
    lcDatRep  = "(" + lcDatRep + [ OR ] + lcDatRep2 + ")"
  ENDIF
ENDIF

IF !EMPTY(lcDatRep)
  lcRpExp = lcRpExp + IIF(lnPos1 > 0 , " AND " + lcDatRep , lcDatRep)
ENDIF
*B603886,1 [END]

*!*************************************************************
*! Name      : lfCollct
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 16/11/2000
*! Purpose   : Collect data in new temp file.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollct()
*!*************************************************************
*B802599
FUNCTION lfCollct
PRIVATE lcExtract,lcDatFlt 
STORE "" TO lcExtract,lcDatFlt,lcDivision, lcRegion,lcTermCode,lcClass,lcSpcInst,lcShipVia   
STORE "" TO lcAccount,lcFacCode ,lcFactAcct,lcDuns,lcPriority

*--get only account to filter with
lcExtract = SUBSTR(lcRpExp,AT('BETWEEN(CUSTOMER.ACCOUNT' , LCRPEXP))
*-- If it is last expresion 
IF EMPTY(SUBSTR(lcExtract,1,AT('AND' , lcExtract)-1))
  lcDatFlt = lcExtract
ELSE  
  lcDatFlt = SUBSTR(lcExtract,1,AT('AND' , lcExtract)-2)
ENDIF
*--to change filter with new temp file
XFILTER =STRTRAN(XFILTER , 'CUSTOMER','&LCTEMPFL')

SELECT CUSTOMER
IF EMPTY(lcDatFlt)
  lcDatFlt = ".T."
ENDIF
*--collect data in temp file
SCAN FOR &lcDatFlt
  IF Type = "M"
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempFl) FROM MEMVAR 
    lcDivision  = cDivision  
    lcRegion    = Region
    lcTermCode  = cTermCode
    lcClass     = Class
    lcSpcInst   = SpcInst
    lcShipVia   = ShipVia
    lcAccount   = Account
    lcPriority  = Priority
    lcFacCode  = cFacCode
    lcFactAcct  = FactAcct
    lcDuns      = Duns
    IF SEEK('S'+lcAccount)
      SCAN REST WHILE TYPE+ACCOUNT = 'S'+lcAccount
        SCATTER MEMVAR MEMO
        m.cDivision  =lcDivision 
        m.Region     =lcRegion 
        m.cTermCode  =lcTermCode
        m.Class      =lcClass
        m.SpcInst    =lcSpcInst
        *B606438,1 (Begin) Remark. Don't overwrite it as it can be changed at the store level.
        *m.ShipVia    =lcShipVia
        *B606438,1 (Begin)  Don't overwrite it.
        m.Priority   =lcPriority
        m.cFacCode  =lcFacCode  
        m.FactAcct   =lcFactAcct
        m.Duns       =lcDuns
        INSERT INTO (lcTempFl) FROM MEMVAR 
      ENDSCAN  
    ENDIF
  ELSE
    EXIT
  ENDIF
  =SEEK('M'+lcAccount)
ENDSCAN
SELECT (lcTempFl)
*-end lfCollct
*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 16/11/2000
*! Purpose   : Procedure to Customer Temporary file.
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************
*!B802599
PROCEDURE lpCreaTemp
*-- check If File is created or not
IF USED(lcTempFl) AND RECCOUNT(lcTempFl) > 0
  USE IN (lcTempFl)
ENDIF
*-- Create File
IF !USED(lcTempFl)
  IF TYPE("laTempStru[1,1]") $ "UL" 
    DIMENSION laTempStru[1,4]
    SELECT CUSTOMER
    =AFIELDS(laTempStru)
  ENDIF
  CREATE CURSOR (lcTempFl) FROM ARRAY laTempStru          
  ZAP
  INDEX ON &SORTFLD TAG (lcTempFl) 
ENDIF
*--end lpCreaTemp