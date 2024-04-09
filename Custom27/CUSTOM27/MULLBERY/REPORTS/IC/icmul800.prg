****************************************************************************
*: Program file      : ICMUL800.PRG        (C101622)
*: Program desc.     : STYLE LOG REPORT 
*: System            : Aria Apparel System (A27)
*: Module            : Inventory Control   (IC)
*: Developer         : ABDOU ELGENDI -     (ABD)
*: Date              : 08/29/1999
*:**************************************************************************
*: Calls : FUNCTIONS : lfCompuTot , lfGetHdr   , lfwRepWhen , lfvFram
*:                   : lfvMonth   , lfvDate    , lfvPrint   , lfWoldVal
*:                   : lfWorkFile , lfCollTime , lfCalAcT   , lfBrows
*:                   : lfwBrow    , lfActivate , lfDeactiv  , lpTrapKey
*:                   : lfvSelect  , lfvSelAll  , lfvSelNon  , lfvShowSel
*:                   : lfwClose.
*:         PROCEDURE : ........
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications     :
*: B803609,1 AMH 08/29/2000 Duplication in the Style log report for Mulberry
*:**************************************************************************
*
*-- Variable Declaration.
DIMENSION laDivLName[1,2]
lcMulDiv =''
STORE {} TO  ldStrtDate , ldHDate1 , ldHDate2  , ldHDate3  , ldHDate4 ,;
              ldtmpdate , ldHDate5 , ldHDate6  , ldHDate7  , ldHDate8 ,;
                          ldHDate9 , ldHDate10 , ldHDate11 , ldHDate12
*-- lcStTime   : Variable to hold the start Time
lcStTime  = TIME()
lcMajPict  = gfItemMask("PM")
lnMajorLen = LEN(lcMajPict)
lcMajorTit = gfItemMask('HM')
*-- END Of Variable Declaration.                       
IF FILE(gcDataDir+'MulAcc.dbf') 
  IF !USED('MulAcc')
    = gfOpenFile(gcDataDir+'MulAcc',gcDataDir+'MulAcc','SH')
  ENDIF
ELSE  
  *-- TEXT :'File MulAcc IS Not found'
  =gfModalGen('TRM04002B00000','DIALOG','File MulAcc is') 
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SELECT MulAcc
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG') 
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*-- if user change filter criteria then you must collect data again [Begin]              
IF llOGFltCh
  lcDivLName  = ''
  WAIT WINDOW "Collecting Date..." NOWAIT 
  *-- Collecting Code...
  IF !USED(lcTmpCurs) OR (RECCOUNT(lcTmpCurs) > 0)
    *-- FUNCTION TO CREATE WORK FILES.
    =lfWorkFile()
  ENDIF
  *--  GET THE  NEW DIVISION FROM OPTION GRID.
  FOR lnInd   = 1 TO ALEN(laOgFxFlt,1)
    IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'INVHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcMulDiv = SUBSTR(laOgFxFlt[lnInd,6],1,6)
      EXIT
    ENDIF
  ENDFOR
  laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long Name.
  laDivLName[1,2] = 'lcDivLName'
  lcSetCen = SET('CENTURY')
  SET CENTURY ON
  ldtmpdate  = ldRpDate
  ldRpeDate  = IIF(lcRpSelBy='D' , ldRpDate,CTOD(PADL(lnRpMonth,2,'0')+"/"+"01/"+RIGHT(DTOC(DATE()),4)))
  ldtmpdate  = IIF(lcRpSelBy='D' , GOMONTH(ldRpDate,-1)+1,CTOD(PADL(lnRpMonth,2,'0')+"/"+"01/"+RIGHT(DTOC(DATE()),4)))
  IF lcRpSelBy = 'D'
    ldFrstRang  = ldRpeDate
  ELSE
    lnFebDays   = IIF(MOD(YEAR(DATE()),4)=0,29,28)
    lnDays      = IIF(INLIST(lnRpMonth,1,3,5,7,8,10,12), 31, 30)
    lnDays      = IIF(lnRpMonth=2, lnFebDays, lnDays)
    lcM         = ALLTRIM(STR(lnRpMonth,2)) + "\"
    lcD         = ALLTRIM(STR(lnDays,2))+"\"
    lcY         = ALLTRIM(STR(YEAR(DATE()),4))
    ldFrstRang  = CTOD(lcM+lcD+lcY)
    ldRpeDate   = ldFrstRang
  ENDIF
  ldEndDate  = IIF(lcRpSelBy='M',;
                 GOMONTH(CTOD(PADL(lnRpMonth,2,'0')+"/"+"01/"+RIGHT(DTOC(ldtmpdate),4)),1)-1,;
                 ldRpeDate)
  SET CENTURY &lcSetCen
 
  ldFrstRang = ldEndDate 
  lnNoOfPerd = CEILING(lnRpTimFr / 12)
  ldFrstRang = GOMONTH(ldFrstRang,lnNoOfPerd*12-lnRpTimFr)
  SELECT(TmpPeriod)
  FOR lnCounter = 1 TO lnNoOfPerd 
    *-- Getting the equivalent 12 months of each period.  
    SELECT(TmpPeriod)
    APPEND BLANK
    REPLACE nGroupNo WITH lnCounter
  ENDFOR  
  lnNoOfMnt= 0
  FOR lnCounter = lnNoOfPerd  TO 1 STEP -1
    FOR lnRepeat = 12 TO 0 STEP -1
      lcRepeat   = ALLTRIM(STR(lnRepeat,2))
      REPLACE dMonth&lcRepeat WITH GOMONTH(ldFrstRang,-lnNoOfMnt)
      IF MONTH(ldEndDate) <> MONTH(ldEndDate+1) 
        lnFebDays  = IIF(MOD(YEAR(dMonth&lcRepeat),4)=0,29,28)
        lnDecrVal2  = IIF(INLIST(Month(dMonth&lcRepeat),1,3,5,7,8,10,12), 31, 30)
        lnDecrVal2  = IIF(Month(dMonth&lcRepeat)=2, lnFebDays,lnDecrVal2)
        IF (lnDecrVal2 = 31 or lnDecrVal2 = 30) AND MONTH(dMonth&lcRepeat) = MONTH(dMonth&lcRepeat+1) 
          DO WHILE MONTH(dMonth&lcRepeat) = MONTH(dMonth&lcRepeat+1)
            REPLACE dMonth&lcRepeat WITH dMonth&lcRepeat + 1
          ENDDO
        ENDIF
      ENDIF
      IF lnRepeat <> 0
        lnNoOfMnt = lnNoOfMnt + 1
      ENDIF
    ENDFOR
    SKIP -1
  ENDFOR
 
  SELECT(TmpPeriod)
  GOTO TOP
  ldBegnDate =  dMonth1
  *-- TO GET THE RELATED FILED FOR THE DIVISION IF NOT EMPTY.
  IF !EMPTY(lcMulDiv) 
    =gfRltFld(lcMulDiv , @laDivLName , 'CDIVISION') 
  ENDIF
  SELECT MulAcc
  GOTO TOP
  IF !EOF()
    lcRpExp = lcRpExp + " .AND. !EOF('MulAcc') .AND. BETWEEN(ShipDate,ldBegnDate,ldEndDate) .AND. Ship > 0  " 
  ELSE
    lcRpExp = lcRpExp + " .AND. BETWEEN(ShipDate,ldBegnDate,ldEndDate) .AND. Ship > 0  " 
  ENDIF
  SELECT InvHdr
  GOTO TOP
  *B803609,1 AMH removing relation with OrdHdr from InvHdr [Start]
  *SET RELATION TO Order   INTO OrdHdr   ,;
  *               Invoice  INTO ConsInvL ,;
  *               Invoice  INTO InvLine  ,;
  *               ACCOUNt  INTO MulAcc
  SET RELATION TO Invoice  INTO ConsInvL ,;
                  Invoice  INTO InvLine  ,;
                  ACCOUNt  INTO MulAcc
  *B803609,1 AMH removing relation with OrdHdr from InvHdr [End  ]
  SELECT InvHdr
  SCAN FOR &lcRpExp 
    WAIT WINDOW "Selecting records..." NOWAIT
    *-- To know which date does this invoice fall in.
    SELECT (TmpPeriod)
    llExit = .F.
    SCAN
      lnElem = nGroupNo
      FOR lnFieldNo = 1 TO 12
        lnDNo     = lnFieldNo - 1
        lcDNo     = ALLTRIM(STR(lnDNo,2))
        lcFieldNo = ALLTRIM(STR(lnFieldNo,2))        
        IF BETWEEN(InvHdr.ShipDate,dMonth&lcDNo+1,dMonth&lcFieldNo)
          llExit = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llExit
        EXIT
      ENDIF
    ENDSCAN
    *-- To indicate which file to use. If its a consolidated invoice then
    *-- use the ConsinvL file else use the InvLine file.
    *B803609,1 AMH OrdHdr.Multi ="Y" not needed [Start]
    *IF OrdHdr.Multi = "Y" AND InvHdr.Consol = "Y"
    IF InvHdr.Consol = "Y"
    *B803609,1 AMH OrdHdr.Multi ="Y" not needed [End  ]
      SELECT ConsInvL
    ELSE  
      SELECT InvLine
    ENDIF  
  *B803609,1 AMH set relation to OrdHdr with selected file [Start]
  SET RELATION TO 'O'+Order INTO OrdHdr
  *B803609,1 AMH set relation to OrdHdr with selected file [End  ]
    SCAN REST WHILE Invoice = InvHdr.Invoice
      WAIT WINDOW "Invoice No. : " + ALLTRIM(Invoice) NOWAIT
      IF !EMPTY(lcMulDiv) AND InvHdr.CDivision <> lcMulDiv
        LOOP
      ENDIF
      =SEEK(Style,'Style')
      SELECT (lcTmpCurs)
      *B803609,1 AMH OrdHdr.Multi ="Y" not needed [Start]
      *IF !SEEK(InvHdr.Account+STR(lnElem,4)+;
      *  IIF(OrdHdr.Multi="Y" AND InvHdr.Consol="Y",SUBSTR(ConsInvL.Style,1,lnMajorLen),SUBSTR(InvLine.Style,1,lnMajorLen)))
      IF !SEEK(InvHdr.Account+STR(lnElem,4)+;
        IIF(InvHdr.Consol="Y",SUBSTR(ConsInvL.Style,1,lnMajorLen),SUBSTR(InvLine.Style,1,lnMajorLen)))
        APPEND BLANK
        *REPLACE cAccount WITH InvHdr.Account,;
        *        nGroupNo WITH lnElem         ,;
        *        cStyle   WITH IIF(OrdHdr.Multi="Y" AND InvHdr.Consol="Y",;
        *                          ConsInvL.Style,InvLine.Style),;
        *        cDesc    WITH Style.Desc,;
        *        cFlag    WITH "1"
        REPLACE cAccount WITH InvHdr.Account,;
                nGroupNo WITH lnElem         ,;
                cStyle   WITH IIF(InvHdr.Consol="Y",ConsInvL.Style,InvLine.Style),;
                cDesc    WITH Style.Desc,;
                cFlag    WITH "1"
      ENDIF
      *REPLACE nMonth&lcFieldNo WITH nMonth&lcFieldNo+IIF(OrdHdr.Multi="Y" AND InvHdr.Consol="Y",;
      *                                    ConsInvL.TotQty,InvLine.TotQty),;
      *       nTotShip WITH nMonth1+nMonth2+nMonth3+nMonth4+nMonth5+nMonth6+;
      *                     nMonth7+nMonth8+nMonth9+nMonth10+nMonth11+nMonth12          
      REPLACE nMonth&lcFieldNo WITH nMonth&lcFieldNo+IIF(InvHdr.Consol="Y",ConsInvL.TotQty,InvLine.TotQty),;
             nTotShip WITH nMonth1+nMonth2+nMonth3+nMonth4+nMonth5+nMonth6+;
                           nMonth7+nMonth8+nMonth9+nMonth10+nMonth11+nMonth12          
      *IF IIF(OrdHdr.Multi="Y",;
      *  IIF(InvHdr.Consol="Y",!EMPTY(ConsInvL.Store),!EMPTY(InvLine.Store)),;
      *  !EMPTY(InvHdr.Store)) AND; 
      *  !SEEK(InvHdr.Account+;
      *  IIF(OrdHdr.Multi="Y" AND InvHdr.Consol="Y",;
      *  ConsInvL.Style,InvLine.Style)+;
      *  IIF(OrdHdr.Multi="Y",;
      *  IIF(InvHdr.Consol="Y",;
      *  ConsInvL.Store,InvLine.Store),InvHdr.Store),TmpStyStor)
      IF IIF(InvHdr.Consol="Y",!EMPTY(ConsInvL.Store),!EMPTY(InvLine.Store));
         AND !SEEK(InvHdr.Account+IIF(InvHdr.Consol="Y",;
        PADR(SUBSTR(ConsInvL.Style,1,lnMajorLen),19),PADR(SUBSTR(InvLine.Style,1,lnMajorLen),19));
        +IIF(InvHdr.Consol="Y",ConsInvL.Store,InvLine.Store),TmpStyStor)
        *INSERT INTO(TmpStyStor) (cAccount,cStyle,cStore) VALUES;
        *                        (InvHdr.Account,;
        *                       IIF(OrdHdr.Multi="Y",IIF(InvHdr.Consol="Y",ConsInvL.Style,InvLine.Style),InvLine.Style),;
        *                       IIF(OrdHdr.Multi="Y" ,IIF(InvHdr.Consol="Y",ConsInvL.Store,InvLine.Store),InvHdr.Store))
        INSERT INTO(TmpStyStor) (cAccount,cStyle,cStore) VALUES;
                                (InvHdr.Account,;
                               IIF(InvHdr.Consol="Y",SUBSTR(ConsInvL.Style,1,lnMajorLen),SUBSTR(InvLine.Style,1,lnMajorLen)),;
                               IIF(InvHdr.Consol="Y",ConsInvL.Store,InvLine.Store))
        REPLACE nStores WITH nStores + 1
      ENDIF                                
    ENDSCAN
    SET RELATION TO
    *B803609,1 AMH OrdHdr.Multi ="Y" not needed [End  ]
  ENDSCAN  
  SELECT(lcTmpCurs)
  GOTO TOP
  IF EOF()
    SELECT (TmpPeriod)
    DELETE ALL
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG') 
    SET DEVICE TO SCREEN
    RETURN
  ENDIF 
  WAIT WINDOW "Computing Totals" NOWAIT
  REPLACE ALL nAvgShip WITH IIF(nStores<>0,nTotShip/nStores,nTotShip)
 
  *-- To Calculate The Totals For Styles and Accounts.
  =lfCompuTot()
  SELECT(lcTmpCurs)
  SET ORDER TO TAG (lcScndIndx)
  FOR lnNoOfGrp = 1 TO lnNoOfPerd
    IF SEEK(STR(lnNoOfGrp,4))
      APPEND BLANK
      REPLACE cFlag    WITH "2" ,;
              cAccount WITH "ZZZZZ",;
              nGroupNo WITH lnNoOfGrp
    ENDIF          
  ENDFOR 
ENDIF   &&-- END IF FOR THE FLAG llOGFltCh

*-- [ BEGIN ] OF REPORT PRINT
SELECT(lcTmpCurs)
IF lcRpSuDt = "S"
  SELECT (lcTmpCurs)
  DELETE ALL FOR cFlag = "1"
ENDIF
SET ORDER TO TAG (lcTmpCurs)
GOTO TOP
SET RELATION TO cFlag+STR(nGroupNo,4) INTO (TmpNCursor)
SET SKIP TO (TmpNCursor) 
R_WIDTH = "N"

lcEdTime = TIME()  && Time in which we finish collect data.
WAIT WINDOW 'Selected Records in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 1
*-- CALL THE FRX
 DO gfDispRe WITH EVAL('lcRpForm')
*-- [ END ] OF REPORT PRINT.
*!**************************************************************************
*! Name      : lfCompuTot
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/30/1999
*! Purpose   : To Calculate The Totals For Styles and Accounts.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   : =lfCompuTot()
*!**************************************************************************
*
FUNCTION lfCompuTot
SELECT(lcTmpCurs)
SCAN 
  SELECT (TmpNCursor)
  
  *-- Records to hold the styles (ctype = "11")
  IF !SEEK("2"+STR(&lcTmpCurs..nGroupNo,4)+"11"+SPACE(5)+&lcTmpCurs..cStyle)
    APPEND BLANK
    REPLACE cFlag    WITH "2"                 ,;
            nGroupNo WITH &lcTmpCurs..nGroupNo,;
            cAccount WITH SPACE(5)            ,; 
            cType    WITH "11"                ,;
           cStyle    WITH LEFT(&lcTmpCurs..cStyle,lnMajorLen),;
           cDesc     WITH &lcTmpCurs..cDesc
           
  ENDIF
  REPLACE nMonth1  WITH nMonth1+&lcTmpCurs..nMonth1,;
          nMonth2  WITH nMonth2+&lcTmpCurs..nMonth2,;
          nMonth3  WITH nMonth3+&lcTmpCurs..nMonth3,;
          nMonth4  WITH nMonth4+&lcTmpCurs..nMonth4,;
          nMonth5  WITH nMonth5+&lcTmpCurs..nMonth5,;
          nMonth6  WITH nMonth6+&lcTmpCurs..nMonth6,;
          nMonth7  WITH nMonth7+&lcTmpCurs..nMonth7,;
          nMonth8  WITH nMonth8+&lcTmpCurs..nMonth8,;
          nMonth9  WITH nMonth9+&lcTmpCurs..nMonth9,;
          nMonth10 WITH nMonth10+&lcTmpCurs..nMonth10,;
          nMonth11 WITH nMonth11+&lcTmpCurs..nMonth11,;
          nMonth12 WITH nMonth12+&lcTmpCurs..nMonth12,;
          nTotShip WITH nTotShip+&lcTmpCurs..nTotShip,;
          nStores  WITH nStores+&lcTmpCurs..nStores  ,;
          nAvgShip WITH nAvgShip + &lcTmpCurs..nAvgShip 

  IF !SEEK("2"+STR(&lcTmpCurs..nGroupNo,4)+"20")
    APPEND BLANK
            REPLACE cFlag     WITH "2"         ,;
            nGroupNo  WITH &lcTmpCurs..nGroupNo,;
            cType     WITH "20"
  ENDIF
  
  IF !SEEK("2"+STR(&lcTmpCurs..nGroupNo,4)+"21")
    APPEND BLANK
    REPLACE cFlag     WITH "2"                 ,;
            nGroupNo  WITH &lcTmpCurs..nGroupNo,;          
            cType     WITH "21"
  ENDIF
  
  IF !SEEK("2"+STR(&lcTmpCurs..nGroupNo,4)+"23")
    APPEND BLANK
    REPLACE cFlag     WITH "2"                 ,;
            nGroupNo  WITH &lcTmpCurs..nGroupNo,;
            cType     WITH "23"
  ENDIF
  
  *-- Records to hold the accounts (ctype = "22")
  IF !SEEK("2"+STR(&lcTmpCurs..nGroupNo,4)+"22"+&lcTmpCurs..cAccount+SPACE(12))
    APPEND BLANK
    REPLACE cFlag    WITH "2"                 ,;
            nGroupNo WITH &lcTmpCurs..nGroupNo,;          
            cAccount WITH &lcTmpCurs..cAccount,;
            cType    WITH "22"                ,;
            cStyle   WITH SPACE(19)           
  ENDIF
  REPLACE nMonth1  WITH nMonth1+&lcTmpCurs..nMonth1,;
          nMonth2  WITH nMonth2+&lcTmpCurs..nMonth2,;
          nMonth3  WITH nMonth3+&lcTmpCurs..nMonth3,;
          nMonth4  WITH nMonth4+&lcTmpCurs..nMonth4,;
          nMonth5  WITH nMonth5+&lcTmpCurs..nMonth5,;
          nMonth6  WITH nMonth6+&lcTmpCurs..nMonth6,;
          nMonth7  WITH nMonth7+&lcTmpCurs..nMonth7,;
          nMonth8  WITH nMonth8+&lcTmpCurs..nMonth8,;
          nMonth9  WITH nMonth9+&lcTmpCurs..nMonth9,;
          nMonth10 WITH nMonth10+&lcTmpCurs..nMonth10,;
          nMonth11 WITH nMonth11+&lcTmpCurs..nMonth11,;
          nMonth12 WITH nMonth12+&lcTmpCurs..nMonth12,;
          nTotShip WITH nTotShip+&lcTmpCurs..nTotShip,;
          nStores  WITH nStores+&lcTmpCurs..nStores  ,;
          nAvgShip WITH nAvgShip + &lcTmpCurs..nAvgShip 
ENDSCAN
*-- END OF lfCompuTot

*!*************************************************************
*! Name      : lfGetHdr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/30/1999
*! Purpose   : To get the starting date for each period.
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : =lfGetHdr()
*!**************************************************************
*
FUNCTION lfGetHdr
PRIVATE lnAlias

lnAlias = SELECT()
=SEEK(STR(EVAL(lcTmpCurs+'.nGroupNo'),4),TmpPeriod)

lnPrevDate = 0
FOR lnCntField = 1 TO 12
  lcFieldNo1         = ALLTRIM(STR(lnCntField,2))
  lcPrevDate         = ALLTRIM(STR(lnPrevDate,2))  
  ldHDate&lcFieldNo1 = &TmpPeriod..dMonth&lcPrevDate + 1
  ldHDate&lcFieldNo1 = PADL(MONTH(ldHDate&lcFieldNo1),2,'0')+"/"+;
                            PADL(DAY(ldHDate&lcFieldNo1),2,'0')
  lnPrevDate = lnPrevDate + 1
ENDFOR
SELECT(lnAlias)
RETURN ''
*-- END OF lfGetHdr
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/30/1999 
*! Purpose   : SET THE REPORT TYPE TO THE DEFAULT.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF ASCAN(laOgObjType,'ldRpDate') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'ldRpDate'),1)
  laOGObjCnt[lnPos] = (lcRpSelBy = 'D') 
  = lfOGShowGet('ldRpDate')
ENDIF
IF ASCAN(laOgObjType,'lnRpMonth') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lnRpMonth'),1)
  laOGObjCnt[lnPos] = (lcRpSelBy = 'M') 
  = lfOGShowGet('lnRpMonth')
ENDIF
llRpStatus = .F.
*-- END OF lfwRepWhen
*!*************************************************************
*! Name      : lfvFram
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/30/1999 
*! Purpose   : VALID THE NUMBER OF FRAMS
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvFram()
*!*************************************************************
*
FUNCTION lfvFram
IF EMPTY(lnRpTimFr) .OR.  lnRpTimFr < 0
  *---Text : 'Frame number should be greater than zero.'
  =gfModalGen('TRM00234B00000','DIALOG','Frame number')
  SET DEVICE TO SCREEN
  lnRpTimFr = laOldVal
  RETURN
ENDIF
*-- END OF lfvFram

*!*************************************************************
*! Name      : lfvMonth
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/30/1999 
*! Purpose   : VALID THE NUMBER OF MONTHS IN THE YEAR
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvMonth()
*!*************************************************************
*
FUNCTION lfvMonth
IF !EMPTY(lnRpMonth)
  IF  lnRpMonth < 0 .OR. lnRpMonth > 12
    *---Text : invalid month . 
    =gfModalGen('TRM42138B00000','DIALOG','month.')
    SET DEVICE TO SCREEN
    lnRpMonth = laOldVal
    RETURN
  ENDIF
ENDIF
*-- END OF lfvMonth

*!*************************************************************
*! Name      : lfvDate
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/30/1999 
*! Purpose   : VALID THE DATE.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDate()
*!*************************************************************
*
FUNCTION lfvDate
IF EMPTY(ldRpDate)
  *---Text : 'Report Date Cannot be empty..!!'
  =gfModalGen('TRM04074B00000','DIALOG','Report date')
  SET DEVICE TO SCREEN
  ldRpDate = laOldVal
  RETURN
ENDIF
*-- END OF lfvDate

*!*************************************************************
*! Name        : lfvPrint
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/29/1999 
*! Purpose     : Enable "Print Price After Discount" Setting in Case 
*!             : Of Inter location PO, Else Disable.
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Return      : 
*!*************************************************************
*! Example     : = lfvPrint()
*!*************************************************************
*
FUNCTION lfvPrint

IF ASCAN(laOgObjType,'ldRpDate') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'ldRpDate'),1)
  laOGObjCnt[lnPos] = (lcRpSelBy = 'D') 
  = lfOGShowGet('ldRpDate')
ENDIF  
IF ASCAN(laOgObjType,'lnRpMonth') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lnRpMonth'),1)
  laOGObjCnt[lnPos] = (lcRpSelBy = 'M') 
  = lfOGShowGet('lnRpMonth')
ENDIF
*-- END OF lfvPrint

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/30/1999 
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

laOldVal = EVALUATE(SYS(18))
*-- END OF lfWoldVal
*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/30/1999
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Return    : None
*!**************************************************************************
*! Example   : = lfWorkFile()
*!**************************************************************************
*
FUNCTION lfWorkFile

*-- This cursor is used to hold the data that will print on the report.
CREATE CURSOR (lcTmpCurs) ;
              (cFlag    C(1) , cAccount C(5), nGroupNo N(4), cStyle C(19) ,;
               cDesc    C(20), nMonth1  N(6), nMonth2  N(6), nMonth3 N(6) ,;
               nMonth4  N(6) , nMonth5  N(6), nMonth6  N(6), nMonth7 N(6) ,;
               nMonth8  N(6) , nMonth9  N(6), nMonth10 N(6), nMonth11 N(6),;
               nMonth12 N(6) , nTotShip N(8), nStores  N(3), nAvgShip N(10,2))
SELECT (lcTmpCurs)
ZAP
INDEX ON STR(nGroupNo,4) TAG (lcScndIndx) OF (lcTmpCurs)
INDEX ON cAccount+STR(nGroupNo,4)+cStyle TAG (lcTmpCurs) OF (lcTmpCurs) ADDITIVE

CREATE CURSOR (TmpNCursor) ;
              (cAccount C(5), nGroupNo N(4), cFlag    C(1),  cStyle C(19) ,;
               nMonth1  N(6), nMonth2  N(6), nMonth3  N(6),  nMonth4 N(6) ,;
               nMonth5  N(6), nMonth6  N(6), nMonth7  N(6),  nMonth8 N(6) ,;
               nMonth9  N(6), nMonth10 N(6), nMonth11 N(6),  nMonth12 N(6),;
               nTotShip N(8), nStores  N(5), nAvgShip N(13,2),cType C(2)  ,;
               lPrintLin L  , cDesc    C(20))
SELECT (TmpNCursor)
ZAP
INDEX ON cFlag+STR(nGroupNo,4)+cType+cAccount+cStyle TAG (TmpNCursor) OF (TmpNCursor)

*-- This cursor is used to hold the periods of dates for each year.
CREATE CURSOR (TmpPeriod) ;
              (nGroupNo N(4), dMonth0  D, dMonth1  D, dMonth2 D, dMonth3 D,;
               dMonth4 D    , dMonth5  D, dMonth6  D, dMonth7 D, dMonth8 D,; 
               dMonth9 D    , dMonth10 D, dMonth11 D, dMonth12 D)
SELECT (TmpPeriod)
ZAP
INDEX ON STR(nGroupNo,4) TAG (TmpPeriod) OF (TmpPeriod)

*-- To be used in the computing of the stores per style.
CREATE CURSOR (TmpStyStor) ;
              (cAccount C(5), cStyle C(19), cStore C(8))
SELECT (TmpStyStor)
ZAP
INDEX ON cAccount+cStyle+cStore TAG (TmpStyStor) OF (TmpStyStor)
*-- End Of lfWorkFile.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example            : =lfCollTime()
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.

*!*************************************************************
*! Name      : lfCalAcT
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/09/1999 
*! Purpose   : To call screen accounts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCalAcT()
*!*************************************************************
*
FUNCTION lfCalAcT

IF  FILE(gcDataDir+'MulAcc.dbf')
  IF !USED('MulAcc') 
    = gfOpenFile(gcDataDir+'MulAcc',gcDataDir+'MulAcc','SH')
  ENDIF
ELSE
  *-- TEXT :'File MulAcc IS Not found'
  =gfModalGen('TRM04002B00000','DIALOG','File MulAcc is') 
  SET DEVICE TO SCREEN
  RETURN
ENDIF  
CREATE CURSOR (TmpAccFile);
              (Account C(5), StName C(30),cFlag C(2))
INDEX ON cFlag+Account TAG (TmpAccFil2) OF (TmpAccFile)
INDEX ON Account TAG (TmpAccFile) OF (TmpAccFile) ADDITIVE
zap

SELECT Customer
=SEEK("M")
SCAN REST WHILE Type+Account+Store = "M"
  WAIT WINDOW "Please Wait.. Selecting Account.." + Account NOWAIT
  SCATTER MEMVAR
  INSERT INTO (TmpAccFile) FROM MEMVAR
  IF SEEK(Account,'MulAcc')
    SELECT (TmpAccFile)
    REPLACE cFlag WITH '>>'
  ENDIF  
ENDSCAN

*-- To check the status of the Select All and Select non buttons at
*-- the first time you enter the screen.
SET ORDER TO (TmpAccFil2) IN (TmpAccFile)
lcStatus  = IIF(SEEK(' ',TmpAccFile),"ENABLE","DISABLE")
lcStatus1 = IIF(SEEK('>>',TmpAccFile),"ENABLE","DISABLE")
SET ORDER TO (TmpAccFile) IN (TmpAccFile)

*--  call the account screen
DO (gcRepHome+gcAct_Appl+'\ICMUL8A.SPX')

*-- END OF lfCalAcT

*!*************************************************************
*! Name      : lfBrows
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/09/1999 
*! Purpose   : To browse all the accounts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrows()
*!*************************************************************
*
FUNCTION lfBrows
PRIVATE lnAlias

lnAlias = SELECT()

*-- File that will hold all the selected and non-selected customers.
SELECT (TmpAccFile)
GOTO TOP
BROWSE FIELDS ;
       cFlag            :H = '  '      :R,;
       Account          :H = 'Account' :R,;
       StName           :H = 'Name'    :R;
       WINDOW    MUL800B  ;
       IN WINDOW MUL800A;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       WHEN lfwBrow()    ;
       FONT "MS Sans Serif",9  ;
       TITLE lcBrTit
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)
*-- END OF lfBrows.
*!*************************************************************
*! Name      : lfwBrow
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : To be done whenever the pointer of the browse changed.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrow()
*!*************************************************************
*
FUNCTION lfwBrow

IF !EMPTY(&TmpAccFile..cFlag)
  SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
ELSE
  SHOW GET pbSelect,1 PROMPT '\<Select' 
ENDIF
*-- END OF lfwBrow.
*!*************************************************************
*! Name      : lfActivate
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActivate()
*!*************************************************************
*
FUNCTION lfActivate

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 

*-- END OF lfActivate.
*!*************************************************************
*! Name      : lfDeactiv
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The READ Deactivate function of screen NAP500
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDeactiv()
*!*************************************************************
*
FUNCTION lfDeactiv

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'MUL800C', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'MUL800C', 'pbSelect'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'MUL800C', 'PbClose'
ENDIF
RETURN .F.
*--END OF lfDeactiv.
*!*************************************************************
*! Name      : lpTrapKey
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDeactiv()
*!*************************************************************
*
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF
*-- END OF lpTrapKey.

*!*************************************************************
*! Name      : lfvSelect
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The valid function of select button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************
*
FUNCTION lfvSelect

SELECT (TmpAccFile)

REPLACE cFlag WITH IIF(EMPTY(cFlag),'>>',' ')
IF EMPTY(cFlag)
  SHOW GET pbSelect,1 PROMPT '\<Select' 
ELSE
  SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
ENDIF
lcOldOrd = ORDER(TmpAccFile)

SET ORDER TO TAG(TmpAccFil2) IN (TmpAccFile)
IF SEEK(' ')
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF

IF SEEK('>>',TmpAccFile)
  SHOW GET pbSelNon ENABLE
ELSE
  SHOW GET pbSelNon DISABLE
ENDIF
SET ORDER TO &lcOldOrd

SHOW WINDOW (lcBrTit) REFRESH SAME
*-- END OF lfvSelect.

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The valid function of select all button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelAll()
*!*************************************************************
*
FUNCTION lfvSelAll

SELECT (TmpAccFile)
REPLACE ALL cFLAG WITH '>>'
= SEEK(" ")
SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
SHOW GET pbSelNon ENABLE
SHOW GET pbSelAll DISABLE

SHOW WINDOW (lcBrTit) REFRESH SAME
*-- END OF lfvSelAll.
*!*************************************************************
*! Name      : lfvSelNon
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The valid function of select non button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelNon()
*!*************************************************************
*
FUNCTION lfvSelNon

SELECT (TmpAccFile)
REPLACE ALL cFLAG WITH SPACE(1)
= SEEK (" ")
SHOW GET pbSelect,1 PROMPT '\<Select' 
SHOW GET pbSelAll ENABLE
SHOW GET pbSelNon DISABLE

SHOW WINDOW (lcBrTit) REFRESH SAME

*--END OF lfvSelNon.
*!*************************************************************
*! Name      : lfvShowSel
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The valid function of Show selected button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvShowSel()
*!*************************************************************
*
FUNCTION lfvShowSel
PRIVATE lnAlias
lnAlias = SELECT(0)

IF !llRpStatus
  SELECT (TmpAccFile)
  SET ORDER TO (TmpAccFil2)
  IF !SEEK('>>')
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG') 
    SET ORDER TO (TmpAccFile)
    = SEEK (" ")
    RETURN
  ENDIF
  SHOW GET pbShowSel,1 PROMPT 'Show A\<ll'
  SET KEY TO '>>'  
  llRpStatus = .T.
ELSE
  SHOW GET pbShowSel,1 PROMPT 'Show selecte\<d'
  SELECT (TmpAccFile)
  SET KEY TO
  IF SEEK(' ')
    SHOW GET pbSelAll ENABLE
  ELSE
    SHOW GET pbSelAll DISABLE
  ENDIF
  
  IF SEEK('>>',TmpAccFile)
    SHOW GET pbSelNon ENABLE
  ELSE
    SHOW GET pbSelNon DISABLE
  ENDIF
  SET ORDER TO (TmpAccFile)
  llRpStatus = .F.
ENDIF
SHOW WINDOW (lcBrTit) REFRESH SAME

SELECT(lnAlias)
*-- END OF lfvShowSel.
*!*************************************************************
*! Name      : lfwClose
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/31/1999
*! Purpose   : The Save The Selected Record In MulAcc file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwClose()
*!*************************************************************
*
FUNCTION lfwClose

*-- Removing all the accounts from the customized file.
SELECT MulAcc
DELETE ALL
*-- If there were selected accounts then replace them in the
*-- customized file otherwise leave the file emtpy.
SELECT (TmpAccFile)
SET ORDER TO (TmpAccFil2)

IF SEEK ('>>')
  SCAN REST WHILE cFlag = '>>'
    WAIT WINDOW "Wait.. Updating accounts.. " + Account NOWAIT
    SELECT MulAcc
    lcDele = SET('DELETE')
    SET DELETE OFF 
    IF !SEEK(&TmpAccFile..Account,'MulAcc')
      INSERT INTO MulAcc (Account,StName);
      VALUES (&TmpAccFile..Account,&TmpAccFile..StName)
    ELSE
      SELECT MulAcc
      IF DELETED()
        RECALL
      ENDIF  
    ENDIF
    llOGFltCh = .T.   
    SET DELETE &lcDele
  ENDSCAN
ENDIF
*-- END OF lfwClose.

*!*************************************************************
