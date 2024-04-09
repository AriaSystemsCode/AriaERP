**********************************************************************
*: Program file  : ARFRE400.PRG  (Converted from 26 (LEM400) to 27)
*: Program desc. : CUSTOMER MASTER LIST         (FOR Frensh)
*: System        : Aria Apparel System (A26)
*! Developer     : Adel Mohammed El Gazzar (ADEL)
*! Date          : 09/26/2000
*: Refer to      : (C101980)
*:********************************************************************
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************

*-- Declare needed variables------------------------------
STORE '' TO lcFilter
STORE .F. TO llNoRec
QSTATE = lcSTitle && State Title
QZIP   = lcZTitle && Zip Title
QSTATESORT = lcRpSort
QZIPSORT   = lcRpSort
*-- Get Account Values
lcAccVal   = laOGFxFlt[lnAccPos,6]
lnSepPos   = ATC('|',lcAccVal)
LAC        = SUBSTR(lcAccVal , 1 , lnSepPos - 1)
HAC        = SUBSTR(lcAccVal , lnSepPos + 1 ,LEN(lcAccVal))
*-- Get Rest of old 2.6 Values
XFC        = laOGFxFlt[lnFactCode,6]
XSALS      = laOGFxFlt[lnRepPos,6]
lcPrtExp = '.T.'
lcXAcc=''
XPRT = ''
XREPORT = 'ARFREM400'
*--Factor code and salesrep code can be deffirent in Main from Stores, so don't exclude Main
*--accounts before scaning Stores.
STORE ' ' TO lcSalesFlt,lcFactFlt
lcTmpNam = gfTempName() && Cursor Temp Name
IF !EMPTY(lcPrt)
  XPRT = [.AND. PRIORITY='] + lcPrt + [']
  lcRpExp = IIF(EMPTY(lcRpExp), SUBSTR(XPRT,5,LEN(XPRT)) ,lcRpExp + XPRT)
ENDIF
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
lcRpOptTtl     = lcRpOptTtl
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
    CASE SORTCD = 'S'
      SORTFLD = "CADDRESS4+ACCOUNT+TYPE+STORE"
    CASE SORTCD='R'
      SORTFLD='REGION+SALESREP+ACCOUNT+TYPE+STORE'
    CASE SORTCD = 'Z'
      SORTFLD='CADDRESS5+ACCOUNT+TYPE+STORE'
    CASE SORTCD $ ' P'
      SORTFLD= "SALESREP+ACCOUNT+TYPE+STORE"
  ENDCASE
  EXIT
ENDDO
ST = IIF(llRpShipTo = .T. , 'Y' , 'N')
IF ST='N'
  lcFilter = lcFilter + ".AND.TYPE='M'"
ENDIF
IF !EMPTY(XST)
  lcFilter= lcFilter + ".AND. STATUS $ (XST)"
ENDIF
*-- dd the sales rep2 to the filter.
= lfAddRp2()
lcFilter = lcRpExp + lcFilter
IF !llRpShipTo
  *--Print stores also
  DO lpMainOnly
ELSE
  *--Print mains only
  DO lpMainStore
ENDIF


*!*************************************************************
*! Name      : lpMainOnly
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Get mains only and print them.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpMainOnly
*!*************************************************************
PROCEDURE lpMainOnly

SELECT CUSTOMER

LOCATE ALL FOR &lcFilter
IF EOF()
  WAIT WINDOW 'NO RECORDS FOUND'
  llNoRec = .T.
  RETURN
ENDIF
IF !EMPTY(lcFilter)
  COPY REST TO (gcWorkDir + lcTmpNam) FOR &lcFilter
ELSE
  COPY ALL TO (gcWorkDir + lcTmpNam)
ENDIF
=gfOpenFile(gcWorkDir+lcTmpNam),'','EX')
Z = LTRIM(STR(RECCOUNT(),6))
SELECT (lcTmpNam)
INDEX ON &SORTFLD  TAG (lcTmpNam)
SET ORDER TO TAG (lcTmpNam)

*--Start printing
* CUSTOMER MASTER LIST
     *....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
H1 = 'NAME#   STORE#                          PHONE1        REP   ADDRESS1             ADDRESS2             CITY            STAT ZIP'
XCOUNTM = 0
XCOUNTS = 0
ROW     = 99
PAGENO  = 0
MAXROW  = 55
GO TOP               

*************  START PRINTING *********************************************************
SET DEVICE TO PRINT
lcFacAndSls = IIF(!EMPTY(lcFactFlt),"AND "+lcFactFlt,"")+IIF(!EMPTY(lcSalesFlt),lcSalesFlt,"")
lcFacAndSls = STRTRAN(lcFacAndSls,'CUSTOMER.SALESREP','SALESREP')
DO WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing press <SPACE BAR> to abort ' NOWAIT
  IF ROW > MAXROW
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,lcRpOptTtl,'W'
    @ 05,00 SAY H1
    @ 06,000 SAY REPLICATE('=',132)    
    ROW = 07
  ENDIF
  SELE SALESREP
  =SEEK(&lcTmpNam..SALESREP)
  XREP = NAME
  SELE (lcTmpNam)
  IF EMPTY(STORE)
    XCOUNTM = XCOUNTM + 1 
  ELSE
    XCOUNTS = XCOUNTS + 1
  ENDIF
  =lfPrintBody()
  SKIP
  IF EOF()
    EXIT
  ENDIF
ENDDO
@ ROW,00 SAY 'There are '+STR(XCOUNTM,4)+' main accounts'
ROW = ROW + 1
@ ROW,00 SAY 'There are '+STR(XCOUNTS,4)+" ship to's"
DO ENDREPORT
RETURN
*--End of lpMainOnly

*!*************************************************************
*! Name      : lpMainStore
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Get mains and their stores and print them.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpMainStore
*!*************************************************************
PROCEDURE lpMainStore

lcNewFlt = IIF(!EMPTY(LAC) OR !EMPTY(HAC),"BETWEEN(Customer.Account,LAC,HAC)",".T.")
lcNewFlt = lcNewFlt + IIF(!EMPTY(XST),".AND. STATUS $ (XST)","")
lcNewFlt = lcNewFlt + IIF(!EMPTY(XSALS),".AND. (CUSTOMER.SALESREP = XSALS OR CUSTOMER.REP2 = XSALS )","")
lcNewFlt = lcNewFlt + IIF(!EMPTY(XZP ),".AND. CUSTOMER.CADDRESS4 = PADR(XZP,30)","")
lnState  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CADDRESS5'),1)
lcState  = laOGFxFlt[lnState, 6]
lcNewFlt = lcNewFlt + IIF(!EMPTY(lcState),".AND. CUSTOMER.CADDRESS5 = PADR(lcState,30)","")

SELECT CUSTOMER

LOCATE ALL FOR &lcNewFlt
IF EOF()
  WAIT WINDOW 'NO RECORDS FOUND'
  llNoRec = .T.
  RETURN
ENDIF
IF !EMPTY(lcNewFlt)
  COPY REST TO (gcWorkDir + lcTmpNam) FOR &lcNewFlt
ELSE
  COPY ALL TO (gcWorkDir + lcTmpNam)
ENDIF
=gfOpenFile(gcWorkDir+lcTmpNam),'','EX')
Z = LTRIM(STR(RECCOUNT(),6))
SELECT (lcTmpNam)
INDEX ON TYPE+ACCOUNT+STORE  TAG (lcTmpNam)
GO TOP
=SEEK('S')
SCAN WHILE TYPE+ACCOUNT+STORE = 'S'
  =SEEK('M'+ACCOUNT,'CUSTOMER')
  REPLACE PRIORITY   WITH CUSTOMER.PRIORITY,;
          CFACCODE   WITH CUSTOMER.CFACCODE,;
          CDIVISION  WITH CUSTOMER.CDIVISION,;
          REGION     WITH CUSTOMER.REGION,;
          CTERMCODE  WITH CUSTOMER.CTERMCODE,;
          CTERMCODE  WITH CUSTOMER.CTERMCODE,;
          CLASS      WITH CUSTOMER.CLASS,;
          SPCINST    WITH CUSTOMER.SPCINST,;
          SHIPVIA    WITH CUSTOMER.SHIPVIA
ENDSCAN
INDEX ON &SORTFLD  TAG (lcTmpNam)
SET ORDER TO TAG (lcTmpNam)
lcRpExp = STRTRAN(lcRpExp,'CUSTOMER.','')
SET FILTER TO &lcRpExp
LOCATE 
IF EOF()
  WAIT WINDOW 'NO RECORDS FOUND'
  llNoRec = .T.
  RETURN
ENDIF
*--Start printing
* CUSTOMER MASTER LIST
     *....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
H1 = 'NAME#   STORE#                          PHONE1        REP   ADDRESS1             ADDRESS2             CITY            STAT ZIP'
XCOUNTM = 0
XCOUNTS = 0
ROW     = 99
PAGENO  = 0
MAXROW  = 55
GO TOP               

*************  START PRINTING *********************************************************
SET DEVICE TO PRINT
DO WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing press <SPACE BAR> to abort ' NOWAIT
  IF ROW > MAXROW
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,lcRpOptTtl,'W'
    @ 05,00 SAY H1
    @ 06,000 SAY REPLICATE('=',132)    
    ROW = 07
  ENDIF
  SELE SALESREP
  =SEEK(&lcTmpNam..SALESREP)
  XREP = NAME
  SELE (lcTmpNam)
  IF EMPTY(STORE)
    XCOUNTM = XCOUNTM + 1 
  ELSE
    XCOUNTS = XCOUNTS + 1
  ENDIF
  =lfPrintBody()
  SKIP
  IF EOF()
    EXIT
  ENDIF
ENDDO
@ ROW,00 SAY 'There are '+STR(XCOUNTM,4)+' main accounts'
ROW = ROW + 1
@ ROW,00 SAY 'There are '+STR(XCOUNTS,4)+" ship to's"
DO ENDREPORT
RETURN
*--End of lpMainStore

*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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

DIMENSION laSortDesc[6,1] , laSortVal[6,1]

laSortDesc[1] = 'Customer Account'
laSortDesc[2] = 'Customer Name'
laSortDesc[3] = lcSTitle
laSortDesc[4] = lcZTitle
laSortDesc[5] = 'Region'
laSortDesc[6] = 'Sales Representative'

laSortVal[1] = 'A'
laSortVal[2] = 'N'
laSortVal[3] = 'S'
laSortVal[4] = 'Z'
laSortVal[5] = 'R'
laSortVal[6] = 'P'

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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

  lcBrFields = "RepCode :H = 'Sales Rerasentitive Code',;
                Name    :H = 'Name'"
               
  lcFile_Ttl = "Sales Rerasentitive"
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
lnZip      = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CADDRESS4'),1)
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

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

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
*!*************************************************************
*! Name      : lfAddRp2
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
    lnPos11    = AT('.AND' , SUBSTR(LCRPEXP,lnDatRep))
    lnPos1     = IIF(lnPos11 <> 0 ,lnPos11,lnPos1)
    IF lnPos1 > 0
      lcDatRep = SUBSTR(lcRpExp ,lnDatRep , lnPos1-1)
      lcRpExp  = STRTRAN(lcRpExp , SUBSTR(lcRpExp,lnDatRep,lnPos1+IIF(lnPos11>0,5,3)),"")
    ELSE
      lnPos2   = LEN(lcRpExp)
      lcDatRep = SUBSTR(lcRpExp ,lnDatRep)
      lcRpExp  = STRTRAN(lcRpExp , SUBSTR(lcRpExp,lnDatRep,lnPos2),".T.")
    ENDIF
    lcDatRep2 = STRTRAN(lcDatRep,"CUSTOMER.SALESREP","CUSTOMER.REP2")
    lcDatRep  = "(" + lcDatRep + [ OR ] + lcDatRep2 + ")"
    lcRpExp   = lcRpExp + 'AND ' +lcDatRep
  ENDIF
  
ENDIF

*!*************************************************************
*! Name      : lfPrintBody
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Print report body.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddRp2()
*!*************************************************************
FUNCTION lfPrintBody

@ ROW,000 SAY Account
@ ROW,008 SAY IIF(EMPTY(STORE), '*MAIN*' , Store)
*-- Let teh phone be 12 not 13 as the picture function make the 13th "/".
@ ROW,040 SAY SUBSTR(PHONE1,1,12)  PICTURE gfPhoneTem() SIZE 1,13
@ ROW,054 SAY IIF(EMPTY(XSALS),SALESREP,IIF(SALESREP=XSALS,SALESREP,REP2))
lcAdd = SUBSTR(cAddress1,1,20)+','+SUBSTR(cAddress2,1,20)+;
          ','+SUBSTR(cAddress3,1,15)+','+SUBSTR(cAddress4,1,3)+','+SUBSTR(cAddress5,1,10)
*--If no addresses exist clear commas
lcOldAdd = lcAdd
lcOldAdd = STRTRAN(lcOldAdd,',','')
IF EMPTY(lcOldAdd)
  lcAdd = ''  
ENDIF
@ ROW,060 SAY lcAdd
ROW = ROW + 1
IF ROW > MAXROW
  PAGENO = PAGENO + 1
  DO RPT_HDR WITH XREPORT,lcRpOptTtl,'W'
  @ 05,00 SAY H1
  @ 06,000 SAY REPLICATE('=',132)    
  ROW = 07
ELSE
  @ ROW,000 SAY REPLICATE('-',132)
  ROW = ROW + 1
ENDIF
