*****************************************************************************
*: Program file  : SOAND50.PRG (C# 101731)
*: Program desc. : Custom Analysis By Style for AndMark.
*: Module        : SO (Sales Order) 
*: Called From   : Menu/OutPut/Transaction
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*:        PROCEDURES : lpOpenFiles,lpMainProc,lpTab,lpBackTab.
*:----------------------------------------------------------------------------
*:        FUNCTIONS  : lfColByA,lfUpOrdL,lfCreTemp,lfsrAcc,lfSRVSty,lfMajTtGet,
*:                     lfNonMaj,lfMajPic,lfvStyle,lfStySum,lfUpdFltVar,lfPrtOpg,
*:                     lfClearRep.
*****************************************************************************
*: Passed Parameters : None 
*:*****************************************************************************
*: Commentes  : designed only on Landscape form and Graphices.
*:              FRX == SOAnd50.FRX
*:              New page Per Season+Devision
*:*****************************************************************************
*: Modification      :
*:B803096,1 SSH 12/03/00 1-Print the selection criteria of the report only 
*:                         on the first page.
*:                        2-Prints on LETTER paper.
*:                        To do this we made all modification on the FRX.
*:B803187,1 AME 10/04/00 1- [a] Group By Season+Division+Account+Style+Color+Customer+Customer PO#
*:B803187,1 AME             [b] add The inv date to the index to make inv date proper to
*:B803187,1 AME             the last inv date of the same Customer PO#
*:B803187,1 AME          2- When Chosed all status Cancel Status mustn't appear
*:B603635,1 ABD 05/11/2000 Subrtract canceled from the booked quantity and amount.
*:B804076,1 AME 04/05/2001 Make Order status option to be in list in OG
*:B804076,1 AME            fix the bug of wrong sum. of qtys (in FRX)
*:B804076,1 AME            fix the bug of assign qty both ship and picked.
*:B804155,1 AME 05/06/2001 Modifications in FRX In Book unit printing and in title 'Balance dollars'
*:B804155,1 AME            To be 'Balance to  Ship dollars'
*:B604782,1 AAN 08/21/2001 1- We don't need to print any record with zero qty.
*:B604782,1 AAN 08/21/2001 2- Fix the bug that the picked qty is zero and the picked amount is not equal to zero.
*:*****************************************************************************

*--- Decleare nessesary variable.
lcSeason  = ''
lcDiv     = ''
lcSelAcc  = ''
lcSelSty  = ''
llSelSty  = .F.
llSelAct  = .F.
lcTmpAcc  = ''
lcClr     = ''
lcClr1    = ''
lcRpSDExp = ''
lcRpInvExp= '.T.'
lcRpOrdExp= '.T.'
lcStyExp  = '.T.'
ldStr1    = {}
ldStr2    = {}
ldComp1   = {}
ldComp2   = {}
ldInv1    = {}
ldInv2    = {}
lnMajLen  = LEN(gfItemMask("PM"))
lcStyPic  = gfItemMask('HI')
*--- Function to update variable to be used in selecting records.
=lfUpdFltVar()
*--- Variable to indicate that if there is any thinghs changed in the option
*--- grid. To recalculate data again.
IF llOgFltCh
  =lfCreTemp()
  =lfColByA()
ENDIF
SELECT (lcTempFil)
SET ORDER TO (lcTempFil)
GOTO TOP
*:B604782,1 AAN Show a mesage if a temp. file is empty.[Begin]
*DO gfDispRe WITH EVAL('lcRpForm')
IF EOF()
  WAIT CLEAR
  =gfModalGen('TRM00052B00000','DIALOG' )
ELSE
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF
*:B604782,1 AAN Show a mesage if a temp. file is empty.[End]
SET DEVICE TO SCREE

*!*************************************************************
*! Name      : lfColByA      (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Function to collect by account.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfColByA()
*!*************************************************************
*!
FUNCTION  lfColByA

PRIVATE lnAlis
lnAlis = SELECT(0)
IF llSelAct
  SELECT (lcSelAcc)
ELSE
  SELECT OrdHdr
ENDIF
GOTO TOP
lcAcctOpg=''
*---- AME[start]
*SCAN  FOR  IIF(llSelAct,.T.,IIF(EMPTY(lcRpCusPo),.T. ,ALLTRIM(CustPo) = ALLTRIM(lcRpCusPo)) ;
                        AND IIF(lcRpBulk='N',OrdHdr.Bulk='N',.T.))
SCAN  FOR  IIF(llSelAct,.T.,IIF(EMPTY(lcRpCusPo),.T. ,IIF(OrdHdr.MultiPo , .T. ,ALLTRIM(CustPo) == ALLTRIM(lcRpCusPo))) ;
                        AND IIF(lcRpBulk='N',OrdHdr.Bulk='N',.T.))
*---- AME[end]                    
 
  lcTmpAcc = Account
  IF llSelAct
    SELECT OrdHdr
    SET ORDER TO OrdAcct
    *--- account+cordtype+order
    =SEEK(lcTmpAcc+'O')
    *B803187,2 AME [start]
    *IF lcRPOrdSta<>'A' AND ALLTRIM(Status) <> ALLTRIM(lcRPOrdSta)
    *  LOOP
    *ENDIF
    *B803187,2 AME [End]
    lcAcctOpg   = lcAcctOpg+IIF(!EMPTY(lcAcctOpg),"| ",'')+ ALLTRIM(lcTmpAcc)
    *--- AME [start]
    *SCAN REST WHILE Account+cOrdType = lcTmpAcc+'O';
               FOR  IIF(EMPTY(lcRpCusPo),.T. ,ALLTRIM(CustPo) = ALLTRIM(lcRpCusPo)) ;
                    AND IIF(lcRpBulk='N',OrdHdr.Bulk='N',.T.)
    SCAN REST WHILE Account+cOrdType = lcTmpAcc+'O';
               FOR  IIF(EMPTY(lcRpCusPo),.T. ,IIF(OrdHdr.MultiPo , .T. ,ALLTRIM(CustPo) == ALLTRIM(lcRpCusPo))) ;
                    AND IIF(lcRpBulk='N',OrdHdr.Bulk='N',.T.)
    *---- AME [end]                                          
      *B803187,2 AME [start]
      *B804076,1 AME [Start] Use the inlist of status instead of choosing one status
      *IF ALLTRIM(Status) = 'X' OR (lcRPOrdSta<>'A' AND ALLTRIM(Status) <> ALLTRIM(lcRPOrdSta))
      IF !(ALLTRIM(Status) $ lcRpStatus)
      *B804076,1 AME [End]
        LOOP
      ENDIF
      *B803187,2 AME [End]
      =lfUpOrdL(Order)
    ENDSCAN
  ELSE
    *B803187,2 AME [start]
    *B804076,1 AME [Start]Use the inlist of status instead of choosing one status
    *IF ALLTRIM(Status) = 'X' OR (lcRPOrdSta<>'A' AND ALLTRIM(Status) <> ALLTRIM(lcRPOrdSta))
    IF !(ALLTRIM(Status) $ lcRpStatus)
    *B804076,1 AME [End]
      LOOP
    ENDIF
    *B803187,2 AME [End]
    =lfUpOrdL(Order)
  ENDIF
ENDSCAN
SELECT(lnAlis)

*!*************************************************************
*! Name      : lfUpOrdL   (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Function to collect Order Line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpOrdL()
*!*************************************************************
*!
FUNCTION lfUpOrdL
PARAMETER lcTmpOrd

 

SELECT OrdLine
=SEEK('O'+lcTmpOrd)
*---- AME [start]
*SCAN REST WHILE cOrdType+Order = 'O'+lcTmpOrd;
           FOR &lcStyExp 
SCAN REST WHILE cOrdType+Order = 'O'+lcTmpOrd;
           FOR &lcStyExp .AND. IIF(EMPTY(lcRpCusPo),.T. ,IIF(OrdHdr.MultiPo,ALLTRIM(CustPo) == ALLTRIM(lcRpCusPo), .T.))
*---- AME [end]           
  lcOrdLinNo = STR(OrdLine.LineNo,6)
  SELECT OrdHdr
  *B803187,2 AME [start]
  *IF !(&lcRpSDExp) OR (lcRPOrdSta<>'A' AND ALLTRIM(Status) <> ALLTRIM(lcRPOrdSta))
  *B804076,1 AME [Start] Use the inlist of status instead of choosing one status  
  *IF !(&lcRpSDExp) OR ALLTRIM(Status) = 'X' OR (lcRPOrdSta<>'A' AND ALLTRIM(Status) <> ALLTRIM(lcRPOrdSta))
  IF !(&lcRpSDExp) OR !(ALLTRIM(Status) $ lcRpStatus)
  *B804076,1 AME [End]
  *B803187,2 AME [End] 
    LOOP
  ENDIF
  SET ORDER TO OrdAcct
  SELECT OrdLine
  lcSty = PADR(SUBSTR(Style,1,lnMajLen),19)
  lcClr1 = SUBSTR(Style,lnClrPo,lnColorLen)
  IF (!EMPTY(lcClr) AND !(lcClr1 $ lcClr)) OR !(&lcRpOrdExp)
    LOOP
  ENDIF
  SET ORDER TO Invlineo IN InvLine
  =SEEK(Order+STR(LineNo,6),'InvLine')
  SELECT InvLine
  llInvVld = (&lcStyExp )
  SELECT InvHdr
  =SEEK(InvLine.Invoice)
  llInvVld = IIF(llInvVld,(&lcRpSDExp),.F.)
  SELECT InvLine
  lnShip    = 0
  lnShipAmt = 0
  ldInvDate = {}
  IF !(&lcRpInvExp)
    LOOP
  ELSE
    SCAN REST WHILE Order+STR(LineNo,6) = lcTmpOrd+lcOrdLinNo
      =SEEK(Invoice,'InvHdr')
      lnShip    = lnShip    + IIF(llInvVld AND InvHdr.Status <> 'V',InvLine.TotQty,0)
      lnShipAmt = lnShipAmt + IIF(llInvVld AND InvHdr.Status <> 'V',(InvLine.TotQty*InvLine.Price),0)
      ldInvDate = InvDate
    ENDSCAN
  ENDIF
  *--- Start Get PikQty
  SELECT PikTkt
  SET ORDER TO Ordpik
  =SEEK(lcTmpOrd)
  lnTotPik = 0
  lnBikAmt = 0
  SCAN REST WHILE order+piktkt = lcTmpOrd
    lcPik = piktkt
    SELECT PikLine
    *--- piktkt+order+STR(lineno,6)
    =SEEK(lcPik+lcTmpOrd+lcOrdLinNo)
    lnTotPik = lnTotPik + PikLine.TotPik
    lnBikAmt = lnBikAmt + (PikLine.TotPik*PikLine.Price)
  ENDSCAN
  SELECT OrdLine
  WAIT WINDOW "Selecting record for account : " +lcTmpAcc NOWAIT
  WAIT WINDOW "Selecting record for order : " +lcTmpOrd + ' '+lcStyPic+'\' + Style NOWAIT
  
  
  *B603635,1 ABD If find canceled amount from order line . [Begin]
  llCanQty = SEEK(OrdLine.cOrdType+OrdLine.Order+STR(OrdLine.lineno,6),'Ordcanln')
  lnCanQty = 0
  IF llCanQty
    SELECT  Ordcanln
    SCAN For CordType+Order+STR(lineno,6) = ;
             OrdLine.CordType+OrdLine.Order+STR(OrdLine.lineno,6)
      lnCanQty = lnCanQty + TotQty
    ENDSCAN
  ELSE
    lnCanQty = 0
  ENDIF
  *B603635,1 ABD [End]

  *--- INDEX ON Account+Style+Color
  SELECT (lcTempFil)
  APPEND BLANK
  *--- lcTempFil  (Account  , AccountN , Style    , Season , Division ,;
                   Color    , CustPo   , BookUnit , PikUnit ;
                   ShipUnit , BookAmt  , PikAmt , ShipAmt , Start  , Complete )
  =SEEK(OrdLine.Style,'Style')
*--- AME [BEGIN]
*  REPLACE Account   WITH lcTmpAcc,;
          AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
          Style     WITH lcSty,;
          Color     WITH lcClr1,;
          CustPo    WITH OrdHdr.CustPo,;
          BookUnit  WITH OrdLine.TotBook,;
          PikUnit   WITH IIF(OrdLine.TotPik=0,lnTotPik,OrdLine.TotPik),;
          ShipUnit  WITH ShipUnit+lnShip,;
          BookAmt   WITH BookUnit*OrdLine.Price,;
          PikAmt    WITH IIF(OrdLine.TotPik=0,lnBikAmt,PikUnit*OrdLine.Price),;
          ShipAmt   WITH ShipAmt+lnShipAmt,;
          Start     WITH OrdLine.Start,;
          Complete  WITH OrdLine.Complete,;
          Season    WITH Style.Season,;
          Division  WITH Style.cDivision,;
          InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)
  *B603635,1 ABD Remark the next lines and Subrtract canceled from 
  *B603635,1 ABD The booked quantity and amount.[Begin]
  *REPLACE Account   WITH lcTmpAcc,;
           AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
           Style     WITH lcSty,;
           Color     WITH lcClr1,;
           CustPo    WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo ,OrdHdr.CustPo),;
           BookUnit  WITH OrdLine.TotBook,;
           PikUnit   WITH IIF(OrdLine.TotPik=0,lnTotPik,OrdLine.TotPik),;
           ShipUnit  WITH ShipUnit+lnShip,;
           BookAmt   WITH BookUnit*OrdLine.Price,;
           PikAmt    WITH IIF(OrdLine.TotPik=0,lnBikAmt,PikUnit*OrdLine.Price),;
           ShipAmt   WITH ShipAmt+lnShipAmt,;
           Start     WITH OrdLine.Start,;
           Complete  WITH OrdLine.Complete,;
           Season    WITH Style.Season,;
           Division  WITH Style.cDivision,;
           InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)
  
  *B804076,1 AME [Start]
  *REPLACE Account   WITH lcTmpAcc,;
          AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
          Style     WITH lcSty,;
          Color     WITH lcClr1,;
          CustPo    WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo ,OrdHdr.CustPo),;
          BookUnit  WITH IIF(llCanQty,OrdLine.TotBook - lnCanQty , OrdLine.TotBook) ,;
          PikUnit   WITH IIF(OrdLine.TotPik=0,lnTotPik,OrdLine.TotPik),;
          ShipUnit  WITH ShipUnit+lnShip,;
          BookAmt   WITH BookUnit*OrdLine.Price,;
          PikAmt    WITH IIF(OrdLine.TotPik=0,lnBikAmt,PikUnit*OrdLine.Price),;
          ShipAmt   WITH ShipAmt+lnShipAmt,;
          Start     WITH OrdLine.Start,;
          Complete  WITH OrdLine.Complete,;
          Season    WITH Style.Season,;
          Division  WITH Style.cDivision,;
          InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)
  
  *B804155,4 AME [Start]
  *REPLACE Account   WITH lcTmpAcc,;
          AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
          Style     WITH lcSty,;
          Color     WITH lcClr1,;
          CustPo    WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo ,OrdHdr.CustPo),;
          BookUnit  WITH IIF(llCanQty,OrdLine.TotBook - lnCanQty , OrdLine.TotBook) ,;
          PikUnit   WITH IIF(OrdLine.TotPik=0,IIF(lnShip>0 ,IIF(lnTotPik>=lnShip,lnTotPik-lnship,0),lnTOtPik),OrdLine.TotPik),;
          ShipUnit  WITH ShipUnit+lnShip,;
          BookAmt   WITH BookUnit*OrdLine.Price,;
          PikAmt    WITH IIF(OrdLine.TotPik=0,lnBikAmt,PikUnit*OrdLine.Price),;
          ShipAmt   WITH ShipAmt+lnShipAmt,;
          Start     WITH OrdLine.Start,;
          Complete  WITH OrdLine.Complete,;
          Season    WITH Style.Season,;
          Division  WITH Style.cDivision,;
          InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)

  *:B604782,1 AAN Fix the bug that the picked qty is zero and the picked amount<> zero.[Begin]
  *REPLACE Account   WITH lcTmpAcc,;
          AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
          Style     WITH lcSty,;
          Color     WITH lcClr1,;
          CustPo    WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo ,OrdHdr.CustPo),;
          BookUnit  WITH IIF(llCanQty,IIF(OrdLine.TotBook - lnCanQty<ShipUnit+lnShip,ShipUnit+lnShip,OrdLine.TotBook - lnCanQty) , IIF(OrdLine.TotBook<ShipUnit+lnShip,ShipUnit+lnShip,OrdLine.TotBook)) ,;
          PikUnit   WITH IIF(OrdLine.TotPik=0,IIF(lnShip>0 ,IIF(lnTotPik>=lnShip,lnTotPik-lnship,0),lnTOtPik),OrdLine.TotPik),;
          ShipUnit  WITH ShipUnit+lnShip,;
          BookAmt   WITH BookUnit*OrdLine.Price,;
          PikAmt    WITH IIF(OrdLine.TotPik=0,lnBikAmt,PikUnit*OrdLine.Price),;
          ShipAmt   WITH ShipAmt+lnShipAmt,;
          Start     WITH OrdLine.Start,;
          Complete  WITH OrdLine.Complete,;
          Season    WITH Style.Season,;
          Division  WITH Style.cDivision,;
          InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)
  PRIVATE lnnBook,lnnPik,lnnShip
  STORE 0 TO lnnBook,lnnPik,lnnShip
  lnnBook = IIF(llCanQty,IIF(OrdLine.TotBook - lnCanQty<ShipUnit+lnShip,ShipUnit+lnShip,OrdLine.TotBook - lnCanQty) , IIF(OrdLine.TotBook<ShipUnit+lnShip,ShipUnit+lnShip,OrdLine.TotBook))
  lnnPik = IIF(OrdLine.TotPik=0,IIF(lnShip>0 ,IIF(lnTotPik>=lnShip,lnTotPik-lnship,0),lnTOtPik),OrdLine.TotPik)
  lnnShip = ShipUnit+lnShip
  IF lnnBook+lnnPik+lnnShip<>0
    REPLACE Account   WITH lcTmpAcc,;
            AccountN  WITH IIF(SEEK('M'+lcTmpAcc,'Customer'),Customer.btName,''),;
            Style     WITH lcSty,;
            Color     WITH lcClr1,;
            CustPo    WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo ,OrdHdr.CustPo),;
            BookUnit  WITH lnnBook,;
            PikUnit   WITH lnnPik,;
            ShipUnit  WITH lnnShip,;
            BookAmt   WITH BookUnit*OrdLine.Price,;
            PikAmt    WITH IIF(OrdLine.TotPik=0,IIF(lnShip>0 ,IIF(lnTotPik>=lnShip,lnBikAmt-lnShipAmt,0),lnBikAmt),PikUnit*OrdLine.Price),;
            ShipAmt   WITH ShipAmt+lnShipAmt,;
            Start     WITH OrdLine.Start,;
            Complete  WITH OrdLine.Complete,;
            Season    WITH Style.Season,;
            Division  WITH Style.cDivision,;
            InvDate   WITH IIF(llInvVld AND ldInvDate <> {} AND ldInvDate>InvDate,ldInvDate,InvDate)
  ELSE
    DELETE
  ENDIF
  *:B604782,1 AAN [End]
  *B804155,4 AME [End]
  *B804076,1 AME [End]
  *B603635,1 ABD [End]
*--- AME [END]
ENDSCAN

*!*************************************************************
*! Name      : lfCreTemp   (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Function to Create Temp File.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreTemp()
*!*************************************************************
*!
FUNCTION lfCreTemp

CREATE TABLE &gcWorkDir.&lcTempFil  (Account C(5),AccountN C(30) , Style C(19),Season C(6),Division C(6),;
                                     Color C(06),CustPo C(15), BookUnit N(8),PikUnit N(8),;
                                     ShipUnit N(8),  BookAmt N(12),PikAmt N(12),;
                                     ShipAmt N(12) ,InvDate D,Start D ,Complete D)
*B803187,1 AME Add InvDate to the index expression [Start]
*INDEX ON Season+Division+Account+Style+Color+CustPo+DTOS(Start)+DTOS(Complete) TAG (lcTempFil)
INDEX ON Season+Division+Account+Style+Color+CustPo+DTOS(Invdate)+DTOS(Start)+DTOS(Complete) TAG (lcTempFil)
*B803187,1 AME [End]

*!*************************************************************
*! Name      : lfsrAcc  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    *llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.


*!*************************************************************
*! Name      : lfsrvSty  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfMajTtlGet  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfMajPic     (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfvStyle     (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfStySum  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfUpdFltVar   (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Option Grid When function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar


FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeason = laOgFxFlt[lnInd,6]
      lcSesOpG   = IIF(lcRpGrd='Y',laOgFxFlt[lnInd,6],'')
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcDiv = laOgFxFlt[lnInd,6]
      lcDivOpg   = IIF(lcRpGrd='Y',laOgFxFlt[lnInd,6],'')      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSelAcc = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYMAJOR' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSelSty = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcClr = laOgFxFlt[lnInd,6]
      lcClrOpg   = IIF(lcRpGrd='Y',laOgFxFlt[lnInd,6],'')
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.START' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldStr1 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldStr2 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
      lcStartOpg   = IIF(lcRpGrd='Y',SUBSTR(laOgFxFlt[lnInd,6],1,10)+'|'+SUBSTR(laOgFxFlt[lnInd,6],12,20),'')
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldComp1 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldComp2 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
      lcCompOpg   = IIF(lcRpGrd='Y',SUBSTR(laOgFxFlt[lnInd,6],1,10)+'|'+SUBSTR(laOgFxFlt[lnInd,6],12,20),'')
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'INVHDR.INVDATE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldInv1 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldInv2 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
      lcInvOpg   = IIF(lcRpGrd='Y',SUBSTR(laOgFxFlt[lnInd,6],1,10)+'|'+SUBSTR(laOgFxFlt[lnInd,6],12,20),'')
  ENDCASE
ENDFOR
*B804076,1 AME [Start] 
*lcOrdOpg   = IIF(lcRpGrd='Y',lcRPOrdSta,'')
*B804076,1 AME [End]
lcBulkOpg   = IIF(lcRpGrd='Y',lcRpBulk,'')
IF USED(lcSelAcc)
  SELECT (lcSelAcc)
  GOTO TOP
  llSelAct =  !EOF(lcSelAcc)
ENDIF
IF USED(lcSelSty)
  SELECT (lcSelSty)
  GOTO TOP
  llSelSty =  !EOF(lcSelSty)
  IF llSelSty
    =lfPrtOpg()
  ENDIF
ENDIF
lcRpSDExp = '.T.'
DO CASE
  CASE  !EMPTY(lcSeason) AND !EMPTY(lcDiv)
    lcRpSDExp = lcRpSDExp + '.AND. SEASON $ lcSeason .AND. cDivision $lcDiv'
  CASE  EMPTY(lcSeason) AND !EMPTY(lcDiv)
    lcRpSDExp = lcRpSDExp + '.AND. cDivision $lcDiv'
  CASE  !EMPTY(lcSeason) AND EMPTY(lcDiv)
    lcRpSDExp = lcRpSDExp + '.AND. SEASON $ lcSeason'
ENDCASE

lcRpOrdExp = '.T.'
DO CASE
  CASE  !EMPTY(ldStr2)
    lcRpOrdExp = lcRpOrdExp + '.AND. BETWEEN(Start,ldStr1,ldStr2)'
  CASE  !EMPTY(ldComp2)
    lcRpOrdExp = lcRpOrdExp + '.AND. BETWEEN(Complete,ldComp1,ldComp2)'
ENDCASE

lcRpInvExp = '.T.'
IF  !EMPTY(ldInv2)
  lcRpInvExp = lcRpInvExp + '.AND. BETWEEN(InvDate,ldInv1,ldInv2)'
ENDIF
lcStyExp = '.T.'
IF llSelSty
  lcStyExp = 'SEEK(SUBSTR(Style,1,lnMajLen),lcSelSty)'
ENDIF
*-- END OF lfUpdFltVar

*!*************************************************************
*! Name      : lfWRep   (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Function to Create Temp File.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWRep()
*!*************************************************************
*!
FUNCTION lfWRep

*B804076,1 AME [Start] Declare source & target order status Variables
DECLARE laRpSource[4],laRpTarget[1]

STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Bid'      TO laRpSource[3]
STORE 'Complete' TO laRpSource[4]
lcRpStatus = 'OHCB'
*B804076,1 AME [End]

*!*************************************************************
*! Name      : lfPrtOpg  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Function to Print the selected Style from OG.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrtOpg()
*!*************************************************************
*!
FUNCTION lfPrtOpg

lcStyOpg = ''
SELECT (lcSelSty)
GOTO TOP
SCAN
  lcStyOpg   = lcStyOpg + IIF(!EMPTY(lcStyOpg),"| ",'')+ ALLTRIM(cStyMajor)
ENDSCAN

*!*************************************************************
*! Name      : lfClearRep  (C# 101731)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/01/2000
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcTempFil),lcTempFil,0)
ERASE &gcWorkDir.&lcTempFil+'.DBF'
ERASE &gcWorkDir.&lcTempFil+'.CDX'

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed Mohamed ELanwar (AME)
*! Date      : 04/03/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*B804076,1 
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Complete','C',''))))

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHCB',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

IF !(lcOldStat == lcRpStatus) && If difference happened in status expression.
  *-- if length of current selected status differ from previous length 
  IF LEN(lcOldStat) != LEN(lcRpStatus) 
    llOGFltCh = .T.
  
  ELSE  && else if length of current selected status equal previous length
    *-- loop to check if it's the same selected status or not.
    FOR lnJ = 1 TO LEN(lcOldStat)
      lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
      IF !(lcCurrChr $ lcRpStatus)
        llOGFltCh = .T.
        EXIT
      ENDIF
    ENDFOR  && end loop to check if it's the same selected status or not.
  ENDIF
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.
