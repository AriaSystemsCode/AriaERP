*:***************************************************************************
*: Program file  : SOREN400.PRG
*: Program desc. : CUSTOMIZED CUSTOMER ACTIVITY BY SPECIAL INSTRUCTION/ACCOUNT FOR REN.
*: Date          : 11/10/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102756
*:***************************************************************************
*: Calls : lfvCrATVar , lfBuildTmp , lfPrntRep  , lfPrnHdr   , lfPrtSubTl
*:       : lfPrtGrdTl , lfCalUnDlr , lfvDateRng , lfvpbOk    , lfvRepCode
*:       : lfSeTSRep  , lfFillAll  , lfvSpcIns  , lfBasToClr
*: Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOREN400
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*--Check the value of the Season.
IF EMPTY(lcRpSeson)
  =gfModalGen('INM00000B00000','F','ALERT',' ','Season should not be empty.')
  RETURN
ELSE
  lcSeason = lcRpSeson
ENDIF

*--Get the value of the SalesRep.
lnSlsRep = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SALESREP.REPCODE'),1)
IF lnSlsRep > 0
  lcSalesRep = laOGFxFlt[lnSlsRep,6]
ELSE
  lcSalesRep = SPACE(3)
ENDIF

*--Get the value of the Devision.
lnDvsion = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CDIVISION'),1)
IF lnDvsion > 0
  lcDivision = laOGFxFlt[lnDvsion,6]
ELSE
  lcDivision = SPACE(3)
ENDIF

*--Get the value of the Devision.
lnRegion = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.REGION'),1)
IF lnRegion > 0
  lcRegion = laOGFxFlt[lnRegion,6]
ELSE
  lcRegion = SPACE(3)
ENDIF

*--Initial the global array and variables.
DIMENSION laSpcialIn[5,2]
STORE SPACE(0) TO laSpcialIn
lcOrdFltr = '.T.'
llNoData  = .T.

*--Get the value of the Special Instruction.
STORE SPACE(0) TO lcAllSpIns
=lfvCrATVar()

*-- lfBuildTmp function to build the temporary file.
IF lfBuildTmp()
  IF llNoData
    SET DEVICE TO PRINT
    =lfPrntRep()
    DO ENDREPORT
    SET DEVICE TO SCREEN
  ENDIF
ENDIF

*--Call the function of deleting the temprory files.
=lfBasToClr(lcTmpOrd , 'F')
=lfBasToClr(lcTmpCust , 'F')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfBuildTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To build the temporary file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfBuildTmp()
*!*************************************************************
FUNCTION lfBuildTmp
PRIVATE llNoRec

CREATE CURSOR (lcTmpOrd) (cAccount C(5), cAccName C(30), cStore C(8)                ,;
                          nUnit1 N(7)  , nAmount1 N(8) , nUnit2 N(7) , nAmount2 N(8) ,;
                          nUnit3 N(7)  , nAmount3 N(8) , nUnit4 N(7) , nAmount4 N(8) ,;
                          nUnit5 N(7)  , nAmount5 N(8))
ZAP
INDEX ON cAccount + cStore TAG(lcTmpOrd) OF (gcWorkDir + lcTmpOrd)


llNoRec   = .F. && This variable to check if there is any order for the selected customer.
lcAccFltr = "Type $ 'MS'" + IIF(!EMPTY(lcRegion)," AND Region = lcRegion",'')
lcAccFltr = lcAccFltr + IIF(!llNonActiv ," AND Status = 'A'",'')

lcOrdFltr = lcOrdFltr + IIF(ldLwEnterD <> {}  , " AND Entered>=ldLwEnterD",'')
lcOrdFltr = lcOrdFltr + IIF(ldHiEnterD <> {}  , " AND Entered<=ldHiEnterD",'')
lcOrdFltr = lcOrdFltr + IIF(ldLwCompD  <> {}  , " AND Complete>=ldLwCompD",'')
lcOrdFltr = lcOrdFltr + IIF(ldHiCompD  <> {}  , " AND Complete<=ldHiCompD",'')
lcOrdFltr = lcOrdFltr + IIF(!EMPTY(lcSalesRep)," AND Rep1 = lcSalesRep",'')
lcOrdFltr = lcOrdFltr + " AND SpcInst $ lcAllSpIns"
lcOrdFltr = lcOrdFltr + IIF(!EMPTY(lcDivision), " AND CDIVISION = lcDivision",'')
lcOrdFltr = lcOrdFltr + IIF(!EMPTY(lcSeason)  , " AND Season = lcSeason",'')

SELE CUSTOMER
LOCATE
WAIT WINDOW 'Selecting Records For The Report ...' NOWAIT
LOCATE ALL FOR &lcAccFltr

IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  llNoData = .F.
  RETURN
ENDIF

COPY REST TO (gcWorkDir + lcTmpCust) FOR &lcAccFltr FIELD Type,Account,Store,Status,;
        												  BtName,StName,Phone1
=gfOpenFile(gcWorkDir + lcTmpCust, '', 'EX')

SELECT (lcTmpCust)
lnIndxRec = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'Sorting &lnIndxRec Records for the report.' NOWAIT
INDEX ON Account + Store TAG(lcTmpCust) OF (gcWorkDir + lcTmpCust)
SET ORDER TO TAG &lcTmpCust

SCAN
  lcAccount = Account
  lcStore   = Store  
  SELECT ORDHDR
  IF SEEK(lcAccount)
    SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR &lcOrdFltr ;
                                         AND (Status<>'X'.AND. Bulk = 'N')
      IF !SEEK(lcAccount+lcStore,lcTmpOrd)
        INSERT INTO (lcTmpOrd) (cAccount , cAccName         , cStore);
               VALUES          (lcAccount,&lcTmpCust..BtName,lcStore)
      ENDIF
      llNoRec = .T.

      DO CASE
        *-- If single order or order for main account.
        CASE Multi = "N" AND ORDHDR.Store == lcStore 
          WAIT WINDOW "Account/Order : " + lcAccount+"/"+Order NOWAIT
          *-- To calculate the unit and dollar for a specific Special Inst.
          =lfCalUnDlr(ORDHDR.Open,ORDHDR.Ship,ORDHDR.OpenAmt,ORDHDR.ShipAmt)
          
        *-- If the order was a multi store order
        CASE Multi = "Y"
          STORE 0 TO lnTotQty,lnAmount,lnShpQty,lnShpAmt

          IF SEEK("O" + ORDHDR.ORDER + lcStore,'ORDLINE')
            SELECT ORDLINE
            SUM REST TotQty,(TotQty*Price);
            WHILE CORDTYPE + ORDER + STORE + STYLE + STR(LINENO,6) = ;
                  "O" + ORDHDR.ORDER + lcStore TO lnTotQty,lnAmount
          ENDIF
          IF SEEK(lcAccount,'INVHDR')
            SELECT INVHDR
            LOCATE REST WHILE Account + Invoice = lcAccount;
                        FOR ORDER + STORE = ORDHDR.ORDER + lcStore
            IF FOUND()
              IF INVHDR.CONSOL <> "Y"
                SELECT INVHDR
                SUM REST Ship,ShipAmt WHILE Account + Invoice = lcAccount;
                                   FOR ORDER + STORE = ORDHDR.ORDER+lcStore;
                                   TO lnShpQty,lnShpAmt
              ELSE
                IF SEEK(INVHDR.INVOICE,'CONSINVH')
                  SUM REST Ship,ShipAmt WHILE Account + Invoice = lcAccount;
                                     FOR ORDER + STORE = ORDHDR.ORDER + lcStore;
                                     TO lnShpQty,lnShpAmt                      
                ENDIF
              ENDIF
            ENDIF            
          ENDIF
          IF lnTotQty+lnShpQty+lnAmount+lnShpAmt > 0
            =lfCalUnDlr(lnTotQty,lnShpQty,lnAmount,lnShpAmt)
          ENDIF  
      ENDCASE
    ENDSCAN
  ENDIF  
  SELECT (lcTmpCust)
ENDSCAN
WAIT CLEAR

SELECT (lcTmpOrd)
LOCATE
IF EOF() OR !llNoRec
  =gfModalGen('INM00000B00000','F','ALERT',' ','No orders were found for the selected criteria. Cannot proceed.')
  RETURN (.F.)
ENDIF

*--End of lfBuildTmp.
*!*************************************************************
*! Name      : lfPrntRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To print the report.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfPrntRep()
*!*************************************************************
FUNCTION lfPrntRep

PAGENO  = 0
lnRow   = 99
R_WIDTH = 'W'
R_TITLE = 'ORDER STATUS SUMMARY REPORT'
XTITLE  = ''

IF ldLwCompD <> {} OR ldHiCompD <> {}
  XTITLE='ORDERS FOR COMPLETION DATES := '+DTOC(ldLwCompD) + ' TO '+DTOC(ldHiCompD)
ELSE
  IF ldLwEnterD <> {} OR ldHiEnterD <> {}
    XTITLE='ORDERS FOR ENTERED DATES := '+DTOC(ldLwEnterD) + ' TO '+DTOC(ldHiEnterD)
  ENDIF
ENDIF

STORE 0 TO lnSubUnt1,lnSubUnt2,lnSubUnt3,lnSubUnt4,lnSubUnt5,;
           lnGrdUnt1,lnGrdUnt2,lnGrdUnt3,lnGrdUnt4,lnGrdUnt5,;
           lnSubDlr1,lnSubDlr2,lnSubDlr3,lnSubDlr4,lnSubDlr5,;
           lnGrdDlr1,lnGrdDlr2,lnGrdDlr3,lnGrdDlr4,lnGrdDlr5

SELECT (lcTmpOrd)
LOCATE
DO WHILE !EOF()
  =IIF(lnRow >= 54,lfPrnHdr(),.F.)
  lcAccount = cAccount
  STORE 0 TO lnSubUnt1,lnSubUnt2,lnSubUnt3,lnSubUnt4,lnSubUnt5,;
             lnSubDlr1,lnSubDlr2,lnSubDlr3,lnSubDlr4,lnSubDlr5

  SCAN REST WHILE cAccount+cStore = lcAccount
    =IIF(lnRow >= 55,lfPrnHdr(),.F.)
    @ lnRow,00 SAY cAccount
    @ lnRow,06 SAY SUBSTR(cAccName,1,14)
    @ lnRow,21 SAY ALLTRIM(cStore)
    lnCol = 31
    lnSubUnt6 = 0
    lnSubDlr6 = 0
    FOR lnCounter = 1 TO 5
      lcCounter = STR(lnCounter,1)
      @ lnRow,lnCol   SAY nUnit&lcCounter PICTURE '9999999'
      @ lnRow,lnCol+9 SAY ROUND(nAmount&lcCounter,0) PICTURE '9999999'
      lnSubUnt&lcCounter = lnSubUnt&lcCounter + nUnit&lcCounter
      lnSubDlr&lcCounter = lnSubDlr&lcCounter + nAmount&lcCounter      
      lnGrdUnt&lcCounter = lnGrdUnt&lcCounter + nUnit&lcCounter
      lnGrdDlr&lcCounter = lnGrdDlr&lcCounter + nAmount&lcCounter
      lnSubUnt6 = lnSubUnt6 + nUnit&lcCounter
      lnSubDlr6 = lnSubDlr6 + nAmount&lcCounter
      lnCol = lnCol + 17
    ENDFOR
    @ lnRow,115 SAY lnSubUnt6 PICTURE "99999999"
    @ lnRow,124 SAY ROUND(lnSubDlr6,0) PICTURE "99999999"
    lnRow = lnRow + 1
  ENDSCAN

  *-- To print the sub total by account.
  =lfPrtSubTl()
ENDDO

*-- To print the grand total of all accounts.
=lfPrtGrdTl()

*--End of lfPrntRep.
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To print the report's header.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PageNo = PageNo + 1
DO RPT_HDR WITH 'SOREN400',XTITLE,R_WIDTH

lnCol = 33
IF !EMPTY(laRpTSPIn)
  FOR lnCounter = 1 TO IIF(ALEN(laRpTSPIn,1) < 5 , ALEN(laRpTSPIn,1) , 5 )
    IF !EMPTY(laRpTSPIn[lnCounter])
      @ 05,lnCol SAY LEFT(SUBSTR(laRpTSPIn[lnCounter],10),15)
      lnCol = lnCol + 17
    ENDIF
  ENDFOR
ELSE
  FOR lnCounter = 1 TO IIF(ALEN(laRpSSpIn,1) < 5 , ALEN(laRpSSpIn,1) , 5 )
    IF !EMPTY(laRpSSpIn[lnCounter,1])
      IF !EMPTY(laSpcialIn[lnCounter,2])
        @ 05,lnCol SAY ALLTRIM(LEFT(laSpcialIn[lnCounter,2],15))
      ENDIF
      lnCol = lnCol + 17
    ENDIF
  ENDFOR
ENDIF

@ 05,123 SAY "TOTAL"
*!0----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8----*----9----*----0----*----1----*----2----*----3--
@ 06,00 SAY 'ACCT  .....NAME..... STORE       UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR'
@ 07,00 SAY REPLICATE('-',132)
lnRow = 8

*--End of lfPrnHdr.
*!*************************************************************
*! Name      : lfPrtSubTl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To print the subtotal by account.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfPrtSubTl()
*!*************************************************************
FUNCTION lfPrtSubTl

@ lnRow,00 SAY REPLICATE('-',132)
lnRow = lnRow + 1
@ lnRow,00 SAY "** SubTotal **  " + lcAccount
lnCol = 30
FOR lnCounter = 1 TO 5
  lcCounter = STR(lnCounter,1)
  @ lnRow,lnCol   SAY lnSubUnt&lcCounter PICTURE '99999999'
  @ lnRow,lnCol+9 SAY ROUND(lnSubDlr&lcCounter,0) PICTURE '99999999'
  lnCol = lnCol + 17
ENDFOR
@ lnRow,115 SAY lnSubUnt1+lnSubUnt2+lnSubUnt3+lnSubUnt4+lnSubUnt5 PICTURE "99999999"
@ lnRow,124 SAY ROUND(lnSubDlr1+lnSubDlr2+lnSubDlr3+lnSubDlr4+lnSubDlr5,0) PICTURE "99999999"
lnRow = lnRow + 1
@ lnRow,00 SAY REPLICATE('-',132)
lnRow = IIF (EOF(lcTmpOrd),lnRow + 1,lnRow + 2)

*--End of lfPrtSubTl.
*!*************************************************************
*! Name      : lfPrtGrdTl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To print the grand total.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfPrtGrdTl()
*!*************************************************************
FUNCTION lfPrtGrdTl

lnRow = lnRow + 1
=IIF(lnRow >= 58,lfPrnHdr(),.F.)
@ lnRow,00 SAY REPLICATE('-',132)
lnRow = lnRow + 1
=IIF(lnRow >= 58,lfPrnHdr(),.F.)
@ lnRow,00 SAY "** Grand Total **"
lnCol = 30
FOR lnCounter = 1 TO 5
  lcCounter = STR(lnCounter,1)
  @ lnRow,lnCol   SAY lnGrdUnt&lcCounter PICTURE '99999999'
  @ lnRow,lnCol+9 SAY ROUND(lnGrdDlr&lcCounter,0) PICTURE '99999999'
  lnCol = lnCol + 17
ENDFOR
@ lnRow,115 SAY lnGrdUnt1+lnGrdUnt2+lnGrdUnt3+lnGrdUnt4+lnGrdUnt5 PICTURE "99999999"
@ lnRow,124 SAY ROUND(lnGrdDlr1+lnGrdDlr2+lnGrdDlr3+lnGrdDlr4+lnGrdDlr5,0) PICTURE "99999999"
lnRow = lnRow + 1
=IIF(lnRow >= 58,lfPrnHdr(),.F.)
@ lnRow,00 SAY REPLICATE('-',132)

*--End of lfPrtGrdTl.
*!*************************************************************
*! Name      : lfCalUnDlr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : To calculate the unit and dollar for a specific Sp Inst.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfCalUnDlr()
*!*************************************************************
FUNCTION lfCalUnDlr
PARAMETERS lnOpnQty,lnShipQty,lnOpnAmt,lnShipAmt
PRIVATE lnAlias

lnAlias = SELECT(0)
IF !EMPTY(laRpTSPIn)
  FOR lnSpcInCnt = 1 TO IIF(ALEN(laRpTSPIn,1) < 5 , ALEN(laRpTSPIn,1) , 5 )
    lcSpcInCnt   = STR(lnSpcInCnt,1)
    IF ALLTRIM(LEFT(laRpTSPIn[lnSpcInCnt],7)) = ALLTRIM(ORDHDR.SPCINST)
      SELECT (lcTmpOrd)
      REPLACE nUnit&lcSpcInCnt   WITH nUnit&lcSpcInCnt   + (lnOpnQty + lnShipQty) ,;
              nAmount&lcSpcInCnt WITH nAmount&lcSpcInCnt + (lnOpnAmt + lnShipAmt)
    ENDIF
  ENDFOR

ELSE
  FOR lnSpcInCnt = 1 TO 5
    lcSpcInCnt   = STR(lnSpcInCnt,1)
    IF ALLTRIM(laSpcialIn[lnSpcInCnt,1]) = ALLTRIM(ORDHDR.SPCINST)
  
      SELECT (lcTmpOrd)
      REPLACE nUnit&lcSpcInCnt   WITH nUnit&lcSpcInCnt   + (lnOpnQty + lnShipQty) ,;
              nAmount&lcSpcInCnt WITH nAmount&lcSpcInCnt + (lnOpnAmt + lnShipAmt)
    ENDIF
  ENDFOR
ENDIF
SELECT(lnAlias)

*--End of lfCalUnDlr.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PARAMETERS lcParm
PRIVATE ldFrom,ldTo

IF lcParm = "A"
  ldFrom  = ldLwEnterD
  ldTo    = ldHiEnterD
  lcTitle = 'Entered Dates (Orders)'
ELSE
  ldFrom  = ldLwCompD
  ldTo    = ldHiCompD
  lcTitle = 'Completion Dates (Orders)'
ENDIF

DO DateRng.Spx

IF lcParm = "A"
  ldLwEnterD = ldFrom
  ldHiEnterD = ldTo
ELSE
  ldLwCompD  = ldFrom
  ldHiCompD  = ldTo
ENDIF

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*--End of lfvpbOk.
*!*************************************************************
*! Name      : lfvRepCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ""
  ENDIF
  
ENDIF
&lcVar = lcObj      && Update the field

*--End of lfvRepCode.
*:*************************************************************
*: Name      : lfSeTSRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*: Purpose   : Go top in Sales Rep file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetSRep()
*:*************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
  GO TOP IN SALESREP
ENDIF

*-- End of lfSetSRep.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Function to fill Special Instruction.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll
PRIVATE lcAlasCod

lcAlasCod = SELECT(0)
DIMENSION laRpSSpIn[1,1],laRpTSPIn[1,1]

STORE '' TO laRpSSpIn,laRpTSPIn

*--The Special Instruction.
SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SPCINST   ' AND crltField = 'N';
                                        INTO ARRAY laRpSSpIn

SELECT(lcAlasCod)

*-- End of lfFillAll.
*!*************************************************************
*! Name      : lfvSpcIns
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Function to call mover Special Instruction.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : =lfvSpcIns()
*!*************************************************************
FUNCTION lfvSpcIns
= gfMover(@laRpSSpIn,@laRpTSPIn,'Special Instruction',.T.,'')

*-- End of lfvSpcIns.
*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--The Special Instruction.
FOR lnInd = 1 TO IIF(ALEN(laRpTSPIn,1) < 5 , ALEN(laRpTSPIn,1) , 5 )
  lcAllSpIns = lcAllSpIns + PADR(laRpTSPIn[lnInd],6) + ' | '
ENDFOR
lcAllSpIns = IIF(ALLTRIM(lcAllSpIns) = '|','',lcAllSpIns)

IF EMPTY(laRpTSPIn)
  FOR lnInd = 1 TO ALEN(laRpSSpIn,1)
    IF lnInd <=5
      IF !EMPTY(laRpSSpIn[lnInd,1])
        lcAllSpIns = lcAllSpIns + PADR(laRpSSpIn[lnInd],6) + ' | '
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDFOR
  lcAllSpIns = IIF(ALLTRIM(lcAllSpIns) = '|','',lcAllSpIns)
ENDIF

*--Section to fill the "laSpcialIn" array.
FOR lnLopSp = 1 TO IIF(ALEN(laRpSSpIn,1) < 5 , ALEN(laRpSSpIn,1) , 5 )
  IF !EMPTY(laRpSSpIn[lnLopSp,1])
    laSpcialIn[lnLopSp,1] = LEFT(laRpSSpIn[lnLopSp],6)
    laSpcialIn[lnLopSp,2] = ALLTRIM(SUBSTR(laRpSSpIn[lnLopSp],10))
  ENDIF
ENDFOR 

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*!*************************************************************
*! Name      : lfvSeason
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/10/2002
*! Purpose   : Validate Season in codes file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSeason()
*!*************************************************************
FUNCTION lfvSeason
PRIVATE lcVar , lcObj , laTemp , lcAlascod , lcOrder

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcAlascod = SELECT(0)
SELECT CODES
lcOrder = ORDER()
SET ORDER TO TAG Ccode_no

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK('NSEASON    ' + lcObj , 'CODES'))
  SELECT CODES
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "cCode_no   :R :H= 'Season' , "   +;
               "cdiscrep   :R :H= 'Discreption' "

  lcFile_Ttl = "Season ..."
  = gfBrows('FOR cdefcode = "N" AND cfld_name = "SEASON    " ','CCODE_NO','laTemp')
    
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ""
  ENDIF
  
ENDIF
&lcVar = lcObj      && Update the field

SELECT CODES
SET ORDER TO TAG &lcOrder
SELECT(lcAlascod)

*--End of lfvRepCode.