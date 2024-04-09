*:***************************************************************************
*: Program file  : SOGROSSP
*: Program desc. : Gross profit Based on sales order
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : MOHAMED SHOKRY (MHM)
*! Date          : 13/06/2000
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOGROSSP
*:***************************************************************************
*: Modifications:
*:B603713,11  MHM 10/16/2000 increase the length of the fields (Gros_Price ,Price, 
*:B603713,11                 PriceA , PriceB , PriceC , Amount) With 2 digit in FRX and add 
*:B603713,11                 record in SYREPUVR to get print dicemal Yes/No 
*:B604649,1   AME 07/31/2001 give the user the feature of leavin the currency empty 
*:B604649,1                  and in this case the report will print all currenceis.
*:B604781,1  WMA 08/18/2002 to avoid deviding Profit Percentage by zero
*:B606555,1 ADEL 10/23/02 Fix the bug of "No records to diplay" when filtering any style group
*:B606519,1 BWA 10/30/2002 Add the Season and Division filters to the Option Grid.[Fix in SYDREPRT.DBF]
*:B606498,1 BWA 12/10/2002 Add the trade discount to the net amount field.
*:B606813,1 BWA 12/23/2002 1) Calculate the Gross amount and Profit % Fields.
*:                         2) Don't let the field of the currency empty. 
*B607043,1 KHM 05/19/2003 Give the user the ability to leave the currency field empty in 
*B607043,1                case of printing report in equivalent amount.
*B607475,1 ASH 08/07/2003 Compute the net amount based on the merchandise discount not only the trade disc.
*B607347,1 ASH 08/07/2003 Increase the width of net and gross amout grand total (in FRX only)
*:********************************************************************************************

*B607043,1 KHM 05/19/2003 (Begin) Check if the user selects foreign currency and did not select
*B607043,1                a currency then use the base currency.
IF lcRpCurr = 'F' AND EMPTY(lcCurrCod)
  lcCurrCod = gcBaseCurr
ENDIF  
*B607043,1 KHM 05/19/2003 (End)

*B606498,1 BWA 12/10/2002 Creat array hold the trade discount value from the codes file.[SATRT]
lnTradDisc = 0
DECLARE laTrdDis[1,2]
laTrdDis[1,1] = 'NTERDISCR'
laTrdDis[1,2] = 'lnTradDisc'
*:B606498,1 BWA 12/10/2002.[END]

*B606555,1 (Begin) Make sure the filter runs if the second line in the order contains
*B606555,1         the selected one sryle group.
lcStyGroup = " .T. "
lnStyPos = ATC("INLIST(STYLE.CSTYG",lcRpExp)
IF lnStyPos <> 0
  llLast = (ATC('AND',SUBSTR(lcRpExp,lnStyPos)) =0)
  lcStyGroup = SUBSTR(lcRpExp,lnStyPos,IIF(llLast,LEN(lcRpExp),ATC('AND',SUBSTR(lcRpExp,lnStyPos))-1))
  lcRpExp = STRTRAN(lcRpExp,lcStyGroup,".T.")
ENDIF
*B606555,1 (End)

lcSPTime = TIME()         && Variable to hold the print Time
lcNewExp = SPACE(0)            && VARIABLE TO CARRY lcRpExp + sales rep
IF "REP1" $ lcRpExp
  =lfAddFxOr("ORDHDR.REP1","ORDHDR.REP2")
ELSE
  lcNewExp = lcRpExp
ENDIF
*-- To get orders only
*B604649,1   AME [Start] Handle the case of choose empty currency
IF EMPTY(lcCurrCod)
  lcNewExp= lcNewExp +'.AND. ORDHDR.CORDTYPE$"O"'+'.AND. ORDHDR.STATUS$lcRpStatus'
ELSE
*B604649,1   AME[End]
  lcNewExp= lcNewExp +'.AND. ORDHDR.CORDTYPE$"O"'+'.AND. ORDHDR.STATUS$lcRpStatus'+'.AND. ORDHDR.cCurrcode$lcCurrCod'
*B604649,1   AME[Start]The end of If Epmty(lccurrcod)
ENDIF
*B604649,1   AME[End]
SELECT ORDHDR
GO TOP
*SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
LOCATE FOR &lcNewExp
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF
*--VARIABLES SECTION
*--lcRpTmpN                         Temp file to collect data(difined at syrepuver)
*--lcRpTmpNam                        Temp file for currancy (difined at syrepuver)
*--lcRpTitle                         variable to print title at FRX. (difined at syrepuver)
*-- llMltiWr : variable for Multi ware house (difined at syrepuver)
*-- lcMarkUp : variable for Mark up (difined at syrepuver)
*--lcCostMth                        variable to hold cost method (define at syrepuvr)
STORE 0 TO lnCostValu,lnpieces,lns_price,lnGrsAmnt,lnNetAmnt, lnProfit, lnPrftPrct ,LNTAMNT
*-- lnCostValu                        && variable to get cost
*-- lnpieces                          && variable to get pieces
*-- lns_price                         && variable to get price
*-- lnGrsAmnt                         && variable to get gross amount
*-- lnNetAmnt                         && variable to get net amount
*-- lnProfit                          && variable to get profit
*-- lnPrftPrct                        && variable to get profit percentage
*B604649,1   AME [Start]
*--llRpshow                           && Variable to show Grand Total
*B604649,1   AME [End]

lcGrpVar = IIF(lcRpSortBy = 'O','ORDER',IIF(lcRpSortBy = 'A','ACCOUNT',IIF(lcRpSortBy = 'S','STYLE','CLASS')))
lcGrpCurr = cCurrCode
llSameGrp = .T.
*-- lcSvAlis                         to get alias
*-- END VARIABLE SECTION

*--COLLECT DATA
DO lpCreateTmp
=lfIndex()              && SET INDEX TO THE FILE ACCORDING TO TYPE 

DO lpColect
SELECT (lcRpTmpN)
*!-- Printing Section
GO TOP

*--Select Summary / Detailes reports
IF   lcRPSumDet ="S"
  lcRpName = "SOGROSPS"
ELSE
  lcRpName = "SOGROSPD"
ENDIF 
DO gfDispRe WITH EVALUATE('lcRpName')
*!-- End of Printing Section

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************
*!
PROCEDURE lpColect
*-- Relation to collect data

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
SET RELATION TO CordType + order INTO ORDLINE ADDITIVE
SELECT ORDLINE
SET RELATION TO Style INTO STYLE
*-SELECT STYLE
*B606555,1 (Begin) Don't overwrite the relation: add ADDITIVE
*SET RELATION TO Style INTO STYDYE
SET RELATION TO Style INTO STYDYE ADDITIVE
*B606555,1 (End)
SELECT ORDHDR
=SEEK('O'+ORDER,'ORDHDR')
*--SCAN WHILE cordtype = "O" FOR &lcNewExp

SCAN FOR &lcNewExp

  *B606498,1 BWA 12/10/2002 Get the trade discount for the order.[START]
  =gfRltFld(ORDHDR.CTERMCODE , @laTrdDis , 'CTERMCODE')
  *B606498,1 BWA 12/10/2002.[END]

  SELECT ORDLINE
    *B606555,1 (Begin) Make sure the filter runs if the second line in the order contains
    *B606555,1         the selected one sryle group.
    *SCAN REST WHILE order = ORDHDR.order 
    SCAN REST WHILE order = ORDHDR.order
      IF !&lcStyGroup
        LOOP
      ENDIF
    *B606555,1 (Begin) Make sure the filter runs if the second line in the order contains  

      SCATTER MEMVAR MEMO
      *-- check if avarge cost or standerd , if avarge check if multi warehouse   
      *-- lnCostValu = IIF(lcCostMth = 'S',Style.TotCost,IIF(llMltiWr = .T.,StyDye.Ave_cost,Style.Ave_cost))
      IF lcCostMth = 'A'
        IF llMltiWr = .T.
          =SEEK(style+cwarecode,'STYDYE')
          *B604649,1   AME [Start] In Case of muli curr. calculate the var by function lfGetFcurr
          IF llMultCurr
            SELECT ORDHDR          
            lnCostValu = lfGetFCurr(StyDye.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)          
            SELECT ORDLINE          
          ELSE
          *B604649,1   AME [End]
            lnCostValu = StyDye.Ave_cost
          *B604649,1   AME [start]
          ENDIF
          *B604649,1   AME [End]
        ELSE
          =SEEK(style,'STYLE')
          *B604649,1   AME [Start] In Case of muli curr. calculate the var by function lfGetFcurr
          IF llMultCurr
            SELECT ORDHDR          
            lnCostValu = lfGetFCurr(Style.Ave_cost, lcRpCurr , ldRpExDate , lcRpTmpNam)          
            SELECT ORDLINE            
          ELSE
          *B604649,1   AME [End]
          lnCostValu = Style.Ave_cost
          *B604649,1   AME [start]
          ENDIF
          *B604649,1   AME [End]
        ENDIF
      ELSE
        =SEEK(style,'STYLE')
        lnCostValu = Style.TotCost
      ENDIF  
      *-- get CCurrCode to use at  gfAmntDisp and insert into temp table 
      m.CCurrCode = ORDHDR.CCURRCODE
      lnpieces =Totqty
      lns_price =price

      *B606813,1 BWA 12/23/2002 Calculate the gros amount.[START]
      *lnGrsAmnt =TotQty*Gros_Price
      lnGrsAmnt =TotQty*Price
      *B606813,1 BWA 12/23/2002.[END]

      lnNetAmnt =TotQty*Price
      lnNetAmnt1=lnNetAmnt
      lnProfit =lnNetAmnt-lnCostValu*TotQty
      *-- get profit percentage
      *---mhm lcMarkUp
      *IF lcRpMark =
      IF lcRpMark = 'T'
        IF lcRpCurr = "F"
          lcRpCurr = "O"
          lcSvAlis = ALIAS()
          lnNetAmnt1=lnNetAmnt
          SELECT ORDHDR
          price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
          lnNetAmnt =TotQty*Price
          LNTAMNT = lnNetAmnt
          lnProfit =lnNetAmnt-lnCostValu*TotQty
          *lnPrftPrct = ROUND(IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0),2)
          lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0)
          lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
          * B#604781, 1 WMA 08/18/2002 [Start] To avoid deviding by zero
          *lnPrftPrct = IIF(lnCostValu = 0, 0,(lnProfit/(lnCostValu*TotQty))*100)
          lnPrftPrct = IIF(lnCostValu =0 .OR. TotQty = 0,0,(lnProfit/(lnCostValu*TotQty))*100)
          * B#604781, 1 WMA 08/18/2002 [End] To avoid deviding by zero ".OR. TotQty"
          SELECT (lcSvAlis)
          lcRpCurr = "F"
          lnNetAmnt=lnNetAmnt1
          
        ELSE
          lcSvAlis = ALIAS()
          lnNetAmnt1=lnNetAmnt
          SELECT ORDHDR
          price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
          lnNetAmnt =TotQty*Price
          LNTAMNT = lnNetAmnt
          lnProfit =lnNetAmnt-lnCostValu*TotQty

          *lnPrftPrct = ROUND(IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0),2)
          lnPrftPrct = IIF(lnCostValu <> 0,(lnProfit/(lnCostValu*TotQty))*100,0)
          lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnCostValu<0),-lnPrftPrct,lnPrftPrct)
          * B#604781, 1 WMA 08/18/2002 [Start] To avoid deviding by zero
          *lnPrftPrct = IIF(lnCostValu = 0, 0,(lnProfit/(lnCostValu*TotQty))*100)
          lnPrftPrct = IIF(lnCostValu = 0 .OR. TotQty = 0, 0,(lnProfit/(lnCostValu*TotQty))*100)
          * B#604781, 1 WMA 08/18/2002 [End] To avoid deviding by zero ".OR. TotQty"
          SELECT (lcSvAlis)
          lnNetAmnt=lnNetAmnt1

        ENDIF
      ELSE
        IF lcRpCurr = "F"
          lcRpCurr = "O"
          lcSvAlis = ALIAS()
          lnNetAmnt1=lnNetAmnt          
          SELECT ORDHDR
          price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
          lnNetAmnt =TotQty*Price
          LNTAMNT = lnNetAmnt
          lnProfit =lnNetAmnt-lnCostValu*TotQty
          *lnPrftPrct = ROUND(IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0),2)
          lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
          lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
          lnPrftPrct = IIF(lnNetAmnt = 0 ,0,(lnProfit/lnNetAmnt)*100)
          lcRpCurr = "F"
          SELECT (lcSvAlis)
          lcRpCurr = "F"
          lnNetAmnt=lnNetAmnt1

        ELSE
        
          lcSvAlis = ALIAS()
          lnNetAmnt1=lnNetAmnt          
          SELECT ORDHDR
          price =gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam)
          lnNetAmnt =TotQty*Price
          LNTAMNT = lnNetAmnt
          lnProfit =lnNetAmnt-lnCostValu*TotQty
          *lnPrftPrct = ROUND(IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0),2)
          lnPrftPrct = IIF(lnNetAmnt <> 0,(lnProfit/lnNetAmnt)*100,0)
          lnPrftPrct = IIF(((lnNetAmnt - (lnCostValu*TotQty))<0 .AND. lnNetAmnt<0),-lnPrftPrct,lnPrftPrct)
          lnPrftPrct = IIF(lnNetAmnt = 0 ,0,(lnProfit/lnNetAmnt)*100)
          SELECT (lcSvAlis)
          lnNetAmnt=lnNetAmnt1

        ENDIF  
      ENDIF
      *--mhm
      *--IIF(lcRpCurr="F" OR NETAMNT=0,NETAMNT,gfAmntDisp(NETAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam))
      *--IF multi currancy calculate it [ began]
      IF lcRpCurr = "F" OR lnNetAmnt = 0
        m.NetAmnt = lnNetAmnt
      ELSE
        lcSvAlis = ALIAS()
        SELECT ORDHDR
        m.NetAmnt =gfAmntDisp(lnNetAmnt,lcRpCurr,ldRpExDate,lcRpTmpNam)
        lnProfit =NetAmnt-lnCostValu*TotQty
        SELECT (lcSvAlis)
      ENDIF
      
      IF lcRpCurr="F" OR lnGrsAmnt=0
        m.GROS_PRICE = lnGrsAmnt 
      ELSE
        lcSvAlis = ALIAS()
        SELECT ORDHDR
        m.GROS_PRICE =gfAmntDisp(lnGrsAmnt,lcRpCurr,ldRpExDate,lcRpTmpNam)
        SELECT (lcSvAlis)
      ENDIF
      
      IF lcRpCurr="F" OR lns_price=0
        m.PRICE = lns_price
      ELSE
        lcSvAlis = ALIAS()
        SELECT ORDHDR
        m.PRICE =gfAmntDisp(lns_price,lcRpCurr,ldRpExDate,lcRpTmpNam)
        SELECT (lcSvAlis)
      ENDIF
      *-cost cannot be change(cost in base currancy) Only in foreign case
      *--get totl cost     
      m.Cost = lnCostValu*TotQty

      *IF lcRpCurr="F" OR lnProfit=0
      *  m.Profit  = lnProfit
      *ELSE
      *  lcSvAlis = ALIAS()
      *  SELECT ORDHDR
      *  m.Profit  =gfAmntDisp(lnProfit,lcRpCurr,ldRpExDate,lcRpTmpNam)
      *  SELECT (lcSvAlis)
      *ENDIF
      *--IF multi currancy calculate it [ end]
      m.CodDes=gfCodDes(&lcRpTmpN..CLASS,'CLASS')  && variable to get code description
      m.TOTQTY  = lnpieces
      m.ProfitPer = lnPrftPrct
      m.Rep1 = ORDHDR.Rep1
      m.CLASS = CUSTOMER.CLASS
      m.S_GROUP = STYLE.CSTYGROUP
      *-m.CCurrCode = ORDHDR.CCURRCODE
      m.Name = CUSTOMER.Btname
      m.Profit = lnProfit

      *B606498,1 BWA 12/10/2002 Calculate the net amount.[START]
      *B607475,1 ASH 08/07/2003 (Begin)Compute the net amount based on the merchandise discount not only the trade disc.
      *m.NetAmnt = m.NetAmnt - (m.NetAmnt * lnTradDisc/100)
      m.NetAmnt = m.NetAmnt - (m.NetAmnt * lnTradDisc/100) - (m.NetAmnt * OrdHdr.Disc / 100)
      *B607475,1 ASH 08/07/2003 (End)
      m.Profit  = m.NetAmnt - m.Cost

      *B606813,1 BWA 12/23/2002 Calculate the Profit % .[START]
      IF lcRpMark = 'T'
        lnPrftPrct = IIF(lnCostValu <> 0 , (m.Profit/m.Cost)*100 , 0)
        lnPrftPrct = IIF(((m.NetAmnt - m.Cost )<0 .AND. lnCostValu<0) , -lnPrftPrct , lnPrftPrct)
        lnPrftPrct = IIF(lnCostValu = 0 .OR. TotQty = 0 , 0 , (m.Profit/m.Cost)*100)
      ELSE
        lnPrftPrct = IIF(m.NetAmnt <> 0,(m.Profit/m.NetAmnt)*100,0)
        lnPrftPrct = IIF(((m.NetAmnt - m.Cost)<0 .AND. m.NetAmnt<0),-lnPrftPrct,lnPrftPrct)
        lnPrftPrct = IIF(m.NetAmnt = 0 ,0,(m.Profit/m.NetAmnt)*100)
      ENDIF
      m.ProfitPer = ROUND(lnPrftPrct,2)
      *B606813,1 BWA 12/23/2002.[END]
      *B606498,1 BWA 12/10/2002.[END]

      INSERT INTO (lcRpTmpN) FROM MEMVAR
    ENDSCAN
ENDSCAN
SELECT ORDHDR
SET RELATION TO


*- end lpColect
*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 14/06/2000
*! Purpose   : Craete temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreateTmp
*!*************************************************************

PROCEDURE lpCreateTmp
*--PRIVATE lcFullPath
IF USED(lcRpTmpN)
  SELECT (lcRpTmpN)
  ZAP
ELSE
  *:B603713,11  MHM 10/10/2000 [start]
  *CREATE CURSOR (lcRpTmpN);
   ( Order C(6) , Style C(19), CLASS C(6) ,S_GROUP C(6) , Profit N(9,2) , ProfitPer N(7,3),;
    LINENO N(6) , PRICE N(9,2) , GROS_PRICE N(9,2) , TOTQTY N(6)  , COST N(10,2) ,;
    cCurrCode C(3) , Nexrate N(9,4) ,Ncurrunit N(4,0) , REP1 C(3) ,;
     NetAmnt N(9,2),Account C(5),Status C(1) , Name C(30),CodDes C(30))
  CREATE CURSOR (lcRpTmpN);
   ( Order C(6) , Style C(19), CLASS C(6) ,S_GROUP C(6) , Profit N(13,2) , ProfitPer N(7,3),;
    LINENO N(6) , PRICE N(12,2) , GROS_PRICE N(12,2) , TOTQTY N(6)  , COST N(10,2) ,;
    cCurrCode C(3) , Nexrate N(9,4) ,Ncurrunit N(4,0) , REP1 C(3) ,;
     NetAmnt N(13,2),Account C(5),Status C(1) , Name C(30),CodDes C(30))
   *:B603713,11  MHM 10/10/2000  [end]
    ZAP  
ENDIF
*--end lpCreateTmp
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 15/06/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
*--DECLARE array to store order status and make mover
  DECLARE laRpSource[3],laRpTarget[1]
  STORE 'Open'     TO laRpSource[1]
  STORE 'Hold'     TO laRpSource[2]
  STORE 'Bid'      TO laRpSource[3]
  lcRpStatus = 'OHB'
  *B604649,1   AME [Start] Value to be adjusted from syrepuvr file
  *lcCurrCod = gcBaseCurr
  *B604649,1   AME [End]
  SHOW GET lcCurrCod

*-- END lfwRepWhen 
*!*************************************************************
*! Name      : lfIndex
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 15/06/2000
*! Purpose   : Set index to filter accourding to our selection
*!            :(Style,Account,Order,Class,Sales Rep)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfIndex()
*!*************************************************************
FUNCTION lfIndex
PRIVATE lcSvAlias
lcSvAlias = SELECT(0)
SELECT (lcRpTmpN)
DO CASE
  CASE lcRPSortBy='A'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Account+cCurrCode+Order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Account+Order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF  
    lcHBreak='ACCOUNT'
    
  CASE lcRPSortBy='O'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON cCurrCode+Order+Style+Account+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Order+Style+Account+STR(RECNO(),7) TAG &lcRpTmpN  
    ENDIF
    lcHBreak='ORDER'    
    
  CASE lcRPSortBy='S'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Style+cCurrCode+Order+STR(RECNO(),7) TAG &lcRpTmpN  
    ELSE
      INDEX ON Style+Order+STR(RECNO(),7) TAG &lcRpTmpN  
    ENDIF       
    lcHBreak='STYLE'
  CASE lcRPSortBy='C'
    IF llMultCurr                          && IF MULTI CURR
      INDEX ON Class+cCurrCode+order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ELSE
      INDEX ON Class+order+Style+STR(RECNO(),7) TAG &lcRpTmpN
    ENDIF     
    lcHBreak='CLASS'
    
ENDCASE
SELECT (lcSvAlias)
*--end lfIndex
*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*B603547,1
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 18/06/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep
*LLOGFLTCH = .T.
*IF llOpenComp AND USED('SYCCOMP')
*  USE IN SYCCOMP
*ENDIF

IF llMultCurr
*  SET CURRENCY TO lcCurrSymb
*  SET CURRENCY &lcCurAlign

*  IF llOpenInt AND USED("SYCINT")
*    USE IN SYCINT 
*  ENDIF

*  IF llOpenCurr AND USED("SYCCURR")
*    USE IN SYCCURR
*  ENDIF

*  IF llOpenExch AND USED("SYCEXCH")
*    USE IN SYCEXCH
*  ENDIF  
ENDIF
*-- End Of lfClearRep.
*!**************************************************************************
*! Name      : lfAddFxOr
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Create expretion Filter  
*!**************************************************************************
*! Calls     : gfGenFlt()
*!**************************************************************************
*! Called from : SOGrossp.prg
*!**************************************************************************
*! Passed Parameters  : Exist Field / New Field
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAddFxOr()
*!**************************************************************************
FUNCTION lfAddFxOr
PARAMETER lcExstFld,lcNewFld
*-- We will extract the filter part concerning with REP1 and add REP2 to the filter expression
IF RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) )) # 0
  *-- Get the expression part before the rep1 expression
  lcTempV = SUBSTR(lcRpExp,RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) ))+4   )
ELSE
  lcTempV = lcRpExp
ENDIF
IF ATC(')AND',lcTempV) # 0 .OR. ATC(') AND',lcTempV) # 0
  *-- Get the exact REP1 expression
  lcTempV = LEFT(lcTempV,ATC('AND',lcTempV)-1 )
ENDIF
*-- Add an expression pretty much like REP1 but of REP2
lcExpr  = '(' + lcTempV + '.OR.' + STRTRAN(lcTempV,'ORDHDR.REP1','ORDHDR.REP2') + ')'
*-- Form the new expression
lcNewExp = STRTRAN(lcRpExp,lcTempV,lcExpr)

RETURN
*-- End Of lfAddFxOr.
*!*************************************************************
*! Name      : lfvOStatus
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 15/06/2000
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus +IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Bid','B','')))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHB',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

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
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the SALESREP IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfSROrder
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the ORDHDR IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    GO TOP
  CASE lcParm = 'R'
    SELECT ORDHDR
ENDCASE
*-- end of lfSROrder.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : get the description of Curr.
*!*************************************************************
*! Example     : = lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)
llOGFltCh = .T.

*B607043,1 KHM 05/19/2003 (Begin) Refersh currency field.
IF lcRpCurr = 'F' AND EMPTY(lcCurrCod)
  lcCurrCod = gcBaseCurr
ENDIF  
SHOW GET lcCurrCod
*B607043,1 KHM 05/19/2003 (End)
*-- end of lfvCurDisp.
*!*************************************************************
*! Name      : lfDefCurr
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
*!B801811,1 MAB
*!
FUNCTION lfDefCurr

RETURN IIF(llMultCurr,'F','O')
*-- end of lfDefCurr.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Mohamed Shokry
*! Date      : 13/06/2000
*! Purpose   : Function to validate browse of Currancy
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvCurCode
lnAlias=SELECT(0)

*B604649,1   AME [Start] In case of empty currency and forign currency
*B604649,1   AME         we can't show grand total (eg. we can't add lirra to Dollars)
*B604649,1   AME         so assign show varible to False.
*B606813,1 BWA 12/23/2002 Comment this lines to don't let the field of the currency empty.[START]

*B607043,1 KHM 05/19/2003 (Begin) Uncomment these lines to give the user the ability to leave
*B607043,1                the currency field empty in case of printing report in equivalent amount.
*llRpshow  = .F. 
*IF ( lcRpCurr = 'F'  AND !EMPTY(lcCurrCod) )  OR  lcRpCurr <> 'F'  
*  llRpshow = .T.   
*ENDIF

*IF EMPTY(lcCurrCod)  && if empty currency don't browse
*  RETURN
*ENDIF
llRpshow  = .F. 
IF ( lcRpCurr = 'F'  AND !EMPTY(lcCurrCod) )  OR  lcRpCurr <> 'F'  
  llRpshow = .T.   
ENDIF

*B607043,1 KHM 05/19/2003 (Begin) Add the check of currency 
*IF EMPTY(lcCurrCod)  && if empty currency don't browse
IF lcRpCurr <> 'F' AND EMPTY(lcCurrCod)  && if empty currency don't browse
*B607043,1 KHM 05/19/2003 (End)
  RETURN
ENDIF
*B607043,1 KHM 05/19/2003 (End)
*B606813,1 BWA 12/23/2002.[END]

*B604649,1   AME [End] 

SELECT SYCCURR
*B604649,1   AME [Start]

*B606813,1 BWA 12/23/2002 Check if the currency code is empty or not.[START]
IF EMPTY(lcCurrCod) .OR. !SEEK(lcCurrCod)
*IF !SEEK(lcCurrCod)
*B606813,1 BWA 12/23/2002.[END]

*B604649,1   AME [End] 
  GO TOP
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcCurrCod = laTemp[1]
  SHOW GET lcCurrCod
ENDIF

*B606813,1 BWA 12/23/2002 Check if the currency code is empty or not.[START]
IF EMPTY(lcCurrCod)
  _CUROBJ = OBJNUM(lcCurrCod)
ENDIF
*B606813,1 BWA 12/23/2002.[END]

SELECT(lnAlias)
*-- end 

*!*************************************************************
*! Name      : lfvChngCur               (B604649,1)
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the Report
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvChngCur()
*!*************************************************************
*B604649,1
FUNCTION lfvChngCur
llRpshow  = .F. 
IF ( lcRpCurr = 'F'  AND !EMPTY(lcCurrCod) )  OR  lcRpCurr <> 'F'  
  llRpshow = .T.   
ENDIF
*--[END] 

*!********************************************************************
*! Name      : lfGetFCurr  (B604649,1)
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : Return the Foreign amount From Base currency.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
*B604649,1 
FUNCTION lfGetFCurr
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
IF lcRpDispCur = 'F'
  lnExRate   = 0
  lnUnit     = 0
  
  IF EMPTY(lcGetFile)
    lcRpCurCde = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
  ELSE
    lcRpCurCde = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
  ENDIF
  
  IF lcRpCurCde = gcBaseCurr
    lnExRate = 1
    lnUnit   = 1
  ELSE
    
    ldExRateDt = Entered
    lnExRate = OrdHdr.Nexrate   
    lnUnit   = OrdHdr.Ncurrunit      
  ENDIF
  
  lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
  lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
  
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcRpCurCde)
  lcExSin1 = IIF(lcExSin1 = '/','*','/')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  
  lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
  
ENDIF  
  
SELECT (lnSavAlias)
RETURN lnAmount
*-- end of lfGetFCurr.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the currency field to
*!           : validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B604649,1
FUNCTION lfwCurCode

lcOldCurr = lcCurrcod

*!*************************************************************
*! Name      : lfvChange
*! Developer : AHMED MOHAMED ELANWER (AME)
*! Date      : 07/31/2001
*! Purpose   : This function called from the Report type to
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvChange()
*!*************************************************************
*B604649,1 
FUNCTION lfvChange

IF   lcRPSumDet ="S"
  lcRpName = "SOGROSPS"
ELSE
  lcRpName = "SOGROSPD"
ENDIF