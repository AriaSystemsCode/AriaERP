*:***************************************************************************
*: Program file  : ARGRSPAN.PRG
*: Program desc. : CUSTOMIZED GROSS PROFIT FOR REVUE.
*: Date          : 10/08/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102426
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGRSPAN
*:***************************************************************************
*: Modifications :
***************************************************************************
*B605596,1 SSE 03/03/2002 Fix bug of differences between Summary and Detail.
*B605654,1 SSE 03/12/2002 Fix bug of asteriks in Net Amount, Profit%, Grand Total by adding a decimal options.
*B606469,1 ADEL 10/14/02  Fix the bugs of incorrect Net qty,Amnt,gross profit
*B606469,1                (All done in the FRX ONLY)
*B606628,1 SSE 11/10/2002 Fix bug of differences between Summary & Detail.
*B606667,1 SSE 11/19/2002 Fix bug of "No records to display."
***************************************************************************

*-- lcRPTitle  Variable hold the optional Title
*-- lcRPSortBy Variable hold the sorted item.
*-- lcRPSumDet Variable hold the kind of the layout.
*-- llRPRecap  Variable check if the user want to print the summery or not.
*-- lcRPBasdOn Variable hold the report based on which transaction.

                *--Section to get the style , color length and postion--*
IF llFrstTmAp
  STORE 0 TO lnClrLnAp , lnClrPosAp , lnStyLnAp
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)

  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLnAp  = LEN(laItemSeg[lnCount,3])
      lnClrPosAp = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR
  lnStyLnAp  = LEN(gfItemMask('PM'))
  llFrstTmAp = .F.
ENDIF
               *--Section to get the style , color length and postion--*
=lfCreatTmp()

*B606667,1 Cut lcRpExp. [Begin]
PRIVATE lcStyleExp , lcAcctExp , lcInvHdExp , lcOrdHdExp , lcRetHdExp
STORE '.T.' TO lcStyleExp , lcAcctExp , lcInvHdExp , lcOrdHdExp , lcRetHdExp
  
*-- If user Selected Account, remove them from lcRpExp 
IF ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnInvHdPos,1]) $ lcRpExp OR ;
  ALLTRIM(laOGFxFlt[lnOrdHdPos,1]) $ lcRpExp
  DO lpGenExpr
ENDIF
*B606667,1 Cut lcRpExp. [End]

=lfcolctDat()

*--Section for repair the report table.
IF lcRPSortBy = "C"
  lcRpForm = IIF(lcRPSumDet = "S" , 'ARGRSPCS' , 'ARGRSPCD')
ELSE
  lcRpForm = 'ARGRSPST'
ENDIF
=gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
=lfRepPltFr(lcRpForm)

SELECT (lclinesAn)
LOCATE

IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "D"
    IF lcRPBasdOn = "B"
      STORE 0 TO lnUntSold , lnAmount
    ENDIF
    lcExpPrnt = SPACE(0)
    lcExpPrnt = &lclinesAn..Account
    lcPrnSson = &lclinesAn..Account + &lclinesAn..Season
    SCAN
      IF lcExpPrnt # &lclinesAn..Account OR lcPrnSson # (&lclinesAn..Account + &lclinesAn..Season)
        IF lcExpPrnt # &lclinesAn..Account
          lcExpPrnt = &lclinesAn..Account
          SKIP - 1
          REPLACE &lclinesAn..llPrntDet WITH .T.
          SKIP
        ENDIF
        IF lcPrnSson # (&lclinesAn..Account + &lclinesAn..Season)
          lcPrnSson = &lclinesAn..Account + &lclinesAn..Season
          SKIP - 1
          REPLACE &lclinesAn..llPrntSum WITH .T.
          SKIP
        ENDIF     
        IF &lclinesAn..Account = "ZZZZZ"
  
          *B605596,1 SSE Commented out. [Begin]        
          *IF lcRPBasdOn = "B"
          *  REPLACE &lclinesAn..Cost WITH lnAmount / lnUntSold
          *  STORE SPACE(0) TO lnUntSold , lnAmount
          *ENDIF
          *B605596,1 SSE Commented out. [End]

          EXIT
        ENDIF

      *B605596,1 SSE Commented out. [Begin]        
      *ELSE
      *  IF lcRPBasdOn = "B"
      *    lnUntSold = lnUntSold + &lclinesAn..UntSold
      *    lnAmount  = lnAmount  + (&lclinesAn..UntSold * &lclinesAn..Cost)
      *  ENDIF
      *B605596,1 SSE Commented out. [End]
      
      ENDIF
    ENDSCAN
    IF !llRPRecap
      GOTO BOTTOM
      REPLACE &lclinesAn..llPrntDet WITH .T. ,;
              &lclinesAn..llPrntSum WITH .T.
    ENDIF
  ENDIF
ENDIF

LOCATE
REPLACE ALL &lclinesAn..Season WITH SPACE(0) FOR &lclinesAn..ACCOUNT = "ZZZZZ"
IF llRPRecap
  GOTO BOTTOM
  IF &lclinesAn..Account = "ZZZZZ"
    REPLACE &lclinesAn..llPrntDet WITH .T.
  ENDIF
ENDIF
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*B605596,1 Increase the decimal places. [Begin]
*CREATE CURSOR (lclinesAn) (Account C(5)     , Btname C(30)     , SeasonDesc C(30) ,;
*                           UntSold N(6)     , PriceSty N(9,2)  , Amount N(9,2)    ,;
*                           UntRtrn N(5)     , AmntRtrn N(9,2)  , Dscont N(8,2)    ,;
*                           NetSals N(11,2)  , NetUntSold N(6)  , Cost N(11,2)     ,;
*                           AmntCost N(11,2) , AmntPrft N(11,2) , StyDesc C(30)    ,;
*                           Season C(6)      , llCsSum L(1)     , llPrntDet L(1)   ,;
*                           llPrntSum L(1)   , ShpCostD N(11,2) , OpnCostD N(11,2) ,;
*                           OpnQty N(6)      , ShpQty N(6)      , Style C(lnStyLnAp+lnClrLnAp+1))

*B606628,1 Increase the decimal fields in Cost field. [Begin]
*CREATE CURSOR (lclinesAn) (Account C(5)     , Btname C(30)     , SeasonDesc C(30) ,;
                           UntSold N(6)     , PriceSty N(11,4)  , Amount N(11,4)    ,;
                           UntRtrn N(5)     , AmntRtrn N(11,4)  , Dscont N(10,4)    ,;
                           NetSals N(13,4)  , NetUntSold N(6)  , Cost N(13,4)     ,;
                           AmntCost N(13,4) , AmntPrft N(13,4) , StyDesc C(30)    ,;
                           Season C(6)      , llCsSum L(1)     , llPrntDet L(1)   ,;
                           llPrntSum L(1)   , ShpCostD N(13,4) , OpnCostD N(13,4) ,;
                           OpnQty N(6)      , ShpQty N(6)      , Style C(lnStyLnAp+lnClrLnAp+1))
CREATE CURSOR (lclinesAn) (Account C(5)     , Btname C(30)     , SeasonDesc C(30) ,;
                           UntSold N(6)     , PriceSty N(11,4)  , Amount N(11,4)    ,;
                           UntRtrn N(5)     , AmntRtrn N(11,4)  , Dscont N(10,4)    ,;
                           NetSals N(13,4)  , NetUntSold N(6)  , Cost N(17,8)     ,;
                           AmntCost N(13,4) , AmntPrft N(13,4) , StyDesc C(30)    ,;
                           Season C(6)      , llCsSum L(1)     , llPrntDet L(1)   ,;
                           llPrntSum L(1)   , ShpCostD N(13,4) , OpnCostD N(13,4) ,;
                           OpnQty N(6)      , ShpQty N(6)      , Style C(lnStyLnAp+lnClrLnAp+1))
*B606628,1 Increase the decimal fields in Cost field. [End]

*B605596,1 Increase the decimal places. [End]

ZAP
IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "S"
    INDEX ON Account + Season TAG CustSmry OF (gcWorkDir + lcLinesAn)
  ELSE
    INDEX ON Account + Season + Style TAG CustDetal OF (gcWorkDir + lcLinesAn)
  ENDIF
ELSE
  INDEX ON Style + STR(PriceSty,9,2) TAG CustDetal OF (gcWorkDir + lcLinesAn)
ENDIF

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function to Collect the date from the credit and 
*!           : the debit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias

lcAlias = ALIAS()
STORE SPACE(0) TO lnCostVal , lnInvCost
STORE SPACE(0) TO lcDescSesn

*B606628,1 Define a new variable for returned cost. [Begin]
lnRetCost = 0
*B606628,1 Define a new variable for returned cost. [End]

*--Case Customer Summary
IF lcRPSortBy = "C"                  && Customer
  IF lcRPSumDet = "S"                && Customer / Summary.

    *--First option : Open quantities.
    IF lcRPBasdOn = "O"              && Case Open quantities.
      =lfOpnQnt()
    ENDIF

    *--2nd option : Shipped quantities.
    IF lcRPBasdOn = "S"              && Case Shipped quantities.    
      =lfInvQnt()
      =lfRetQnt()
    ENDIF

    *--3rd option : Booked quantities.
    IF lcRPBasdOn = "B"              && Case Booked quantities.
      =lfBokQnt()
    ENDIF

  ELSE                               && && Customer / Detail.

    *--First option : Open quantities.
    IF lcRPBasdOn = "O"              && Case Open quantities.
      =lfOpnDet()
    ENDIF

    *--2nd option : Shipped quantities.
    IF lcRPBasdOn = "S"              && Case Shipped quantities.
      =lfInvDet()
      =lfRetDet()
    ENDIF

    *--3rd option : Booked quantities.
    IF lcRPBasdOn = "B"              && Case Booked quantities.
      =lfBokDet()
    ENDIF
  ENDIF

ELSE                                 && Style

  *--First option : Open quantities.
  IF lcRPBasdOn = "O"              && Case Open quantities.
    =lfOpnSty()
  ENDIF

  *--2nd option : Shipped quantities.
  IF lcRPBasdOn = "S"              && Case Shipped quantities.
    =lfInvSty()
    =lfRetSty()
  ENDIF

  *--3rd option : Booked quantities.
  IF lcRPBasdOn = "B"              && Case Booked quantities.
    =lfBokSty()
  ENDIF

ENDIF
SELECT (lcAlias)

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClrRead()
*!*************************************************************
*--This function called from lcRPSortBy , lcRPBasdOn.
FUNCTION lfClrRead
CLEAR READ

*--End of lfClrRead.
*!*************************************************************
*! Name      : lfsrSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfsAccount
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Set and Rest functions for account filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsAccount()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF

*-- End of lfsAccount.
*!*************************************************************
*! Name      : lfOpnQnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Open Quntities in summery.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnQnt()
*!*************************************************************
FUNCTION lfOpnQnt
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO ORDHDR ADDITIVE
SELECT ORDHDR
SET RELATION TO ORDHDR.CORDTYPE + ORDHDR.ORDER INTO ORDLINE ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT ORDHDR
  
  *B606667,1 Add lcOrdHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH"
  SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH" AND &lcOrdHdExp.
  *B606667,1 Add lcOrdHdExp. [End]

    SELECT (lclinesAn)
    IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON)
    
      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                          ,;
      *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                       ,;
      *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDHDR.OPENAMT)/100) ,;
      *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + lfGetCost()

      REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
      REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                          ,;
              &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                       ,;
              &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDHDR.OPENAMT)/100) ,;
              &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)

        *B605596,1 Fix the summary option. [Begin]
        *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
        *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
        *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
        *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + lnCostVal

        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
        REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
                &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        *B605596,1 Fix the summary option. [End]

      ENDIF
    ELSE
      APPEND BLANK
      lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')
      
      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
      *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
      *        &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
      *        &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
      *        &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
      *        &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
      *        &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
      *        &lclinesAn..Cost       WITH lfGetCost()
      REPLACE &lclinesAn..OpnCostD   WITH lfTotCost() 
      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
              &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
              &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
              &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
              &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
              &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.

          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
          *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
          *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
          *        &lclinesAn..Cost    WITH lnCostVal

          REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost() 
          REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDHDR.OPEN                        ,;
                  &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDHDR.OPENAMT                     ,;
                  &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
          *B605596,1 Fix the summary option. [End]

        ELSE
          APPEND BLANK
          
          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
          *        &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
          *        &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
          *        &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
          *        &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
          *        &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
          *        &lclinesAn..llCsSum    WITH .T.                                ,;
          *        &lclinesAn..Cost       WITH lnCostVal
          
          REPLACE &lclinesAn..OpnCostD WITH lfTotCost() 
          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
                  &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
                  &lclinesAn..UntSold    WITH ORDHDR.OPEN                        ,;
                  &lclinesAn..Amount     WITH ORDHDR.OPENAMT                     ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
                  &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..llCsSum    WITH .T.                                ,;
                  &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
          *B605596,1 Fix the summary option. [End]

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT ORDHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfOpnQnt.
*!*************************************************************
*! Name      : lfGetCost
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the style cost.
*!*************************************************************
*! Called from : lfOpnQnt()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetCost()
*!*************************************************************
FUNCTION lfGetCost
PRIVATE lcAlias , lcKeyOrd

lcAlias = ALIAS()
lnCostVal = 0
SELECT ORDLINE
lcKeyOrd = EVAL(KEY())
=SEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER,'ORDLINE')
SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = ORDHDR.CORDTYPE + ORDHDR.ORDER
  IF SEEK(ORDLINE.STYLE , 'STYLE')
    IF STYLE.MAKE
      lnCostVal = lnCostVal + STYLE.NMCOST1 + STYLE.NMCOST2 + STYLE.NMCOST3 + STYLE.NMCOST4 + STYLE.NMCOST5
    ELSE
      lnCostVal = lnCostVal + STYLE.NICOST1 + STYLE.NICOST2 + STYLE.NICOST3 + STYLE.NICOST4 + STYLE.NICOST5
    ENDIF
  ENDIF
ENDSCAN

=SEEk(lcKeyOrd,'ORDLINE')
SELECT (lcAlias)
RETURN lnCostVal

*--End of lfGetCost.

*!***************************************************************************
*! Name      : lfTotCost
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/27/2002
*! Purpose   : Function used to collect total cost * Units.
*!***************************************************************************
*! Called from : lfOpnQnt()
*!***************************************************************************
*! Example     : =lfTotCost()
*!***************************************************************************
*
FUNCTION lfTotCost
PRIVATE lcAlias , lcKeyOrd

lcAlias = ALIAS()
lnCostVal = 0
SELECT ORDLINE
lcKeyOrd = EVAL(KEY())
=SEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER,'ORDLINE')
SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = ORDHDR.CORDTYPE + ORDHDR.ORDER
  IF SEEK(ORDLINE.STYLE , 'STYLE')
    IF STYLE.MAKE
      lnCostVal = lnCostVal + ((STYLE.NMCOST1 + STYLE.NMCOST2 + STYLE.NMCOST3 + STYLE.NMCOST4 + STYLE.NMCOST5) * OrdLine.TotQty)
    ELSE
      lnCostVal = lnCostVal + ((STYLE.NICOST1 + STYLE.NICOST2 + STYLE.NICOST3 + STYLE.NICOST4 + STYLE.NICOST5) * OrdLine.TotQty)
    ENDIF
  ENDIF
ENDSCAN

=SEEk(lcKeyOrd,'ORDLINE')
SELECT (lcAlias)
RETURN lnCostVal
*-- End of lfTotCost.

*!*************************************************************
*! Name      : lfInvQnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the invoice Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvQnt()
*!*************************************************************
FUNCTION lfInvQnt
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO INVHDR ADDITIVE
SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT INVHDR

  *B606667,1 Add lcInvHdExp. [Begin]
  *SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V"
  SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND &lcInvHdExp.
  *B606667,1 Add lcInvHdExp. [End]

    SELECT (lclinesAn)
    IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON)
   
      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                              ,;
      *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                           ,;
      *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) ,;
      *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                              ,;
      *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lfInvCost())             ,;
      *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

      *B606667,1 Add the trade discount. [Begin]
      *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                              ,;
      *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                           ,;
      *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) ,;
      *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                              ,;
      *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()                             ,;
      *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
      REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                              ,;
              &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                           ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
              &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                              ,;
              &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()                             ,;
              &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
      *B606667,1 Add the trade discount. [End]

      *B605596,1 Fix the summary option. [End]
              
      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
        
        *B605596,1 Fix the summary option. [Begin]
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lnInvCost)             ,;
        *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty
        
        *B606667,1 Add the trade discount. [Begin]
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
        *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
        *B606667,1 Add the trade discount. [End]
                
        *B605596,1 Fix the summary option. [End]

      ENDIF
    ELSE
      APPEND BLANK
      lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')

      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
      *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
      *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
      *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
      *        &lclinesAn..UntSold    WITH INVHDR.SHIP                            ,;
      *        &lclinesAn..Amount     WITH INVHDR.SHIPAMT                         ,;
      *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
      *        &lclinesAn..ShpQty     WITH INVHDR.SHIP                            ,;
      *        &lclinesAn..ShpCostD   WITH INVHDR.SHIP * lfInvCost()              ,;
      *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

      *B606667,1 Add the trade discount. [Begin]
      *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
      *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
      *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
      *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
      *        &lclinesAn..UntSold    WITH INVHDR.SHIP                            ,;
      *        &lclinesAn..Amount     WITH INVHDR.SHIPAMT                         ,;
      *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
      *        &lclinesAn..ShpQty     WITH INVHDR.SHIP                            ,;
      *        &lclinesAn..ShpCostD   WITH lfInvCost()                            ,;
      *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
              &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
              &lclinesAn..UntSold    WITH INVHDR.SHIP                            ,;
              &lclinesAn..Amount     WITH INVHDR.SHIPAMT                         ,;
              &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
              &lclinesAn..ShpQty     WITH INVHDR.SHIP                            ,;
              &lclinesAn..ShpCostD   WITH lfInvCost()                            ,;
              &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
      *B606667,1 Add the trade discount. [End]
              
      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lnInvCost)             ,;
          *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

          *B606667,1 Add the trade discount. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
          *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVHDR.SHIP                            ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVHDR.SHIPAMT                         ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVHDR.SHIP                            ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                             ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
          *B606667,1 Add the trade discount. [End]
    
          *B605596,1 Fix the summary option. [End]

        ELSE
          APPEND BLANK
          
          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                          ,;
          *        &lclinesAn..Season     WITH INVHDR.SEASON                                    ,;
          *        &lclinesAn..UntSold    WITH INVHDR.SHIP                                      ,;
          *        &lclinesAn..Amount     WITH INVHDR.SHIPAMT                                   ,;
          *        &lclinesAn..SeasonDesc WITH lcDescSesn                                       ,;
          *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100           ,;
          *        &lclinesAn..llCsSum    WITH .T.                                              ,;
          *        &lclinesAn..ShpQty     WITH &lclinesAn..ShpQty  + INVHDR.SHIP                ,;
          *        &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lnInvCost) ,;
          *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

          *B606667,1 Add the trade discount. [Begin]
          *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                          ,;
          *        &lclinesAn..Season     WITH INVHDR.SEASON                                    ,;
          *        &lclinesAn..UntSold    WITH INVHDR.SHIP                                      ,;
          *        &lclinesAn..Amount     WITH INVHDR.SHIPAMT                                   ,;
          *        &lclinesAn..SeasonDesc WITH lcDescSesn                                       ,;
          *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100           ,;
          *        &lclinesAn..llCsSum    WITH .T.                                              ,;
          *        &lclinesAn..ShpQty     WITH &lclinesAn..ShpQty  + INVHDR.SHIP                ,;
          *        &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + lnInvCost                 ,;
          *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                          ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON                                    ,;
                  &lclinesAn..UntSold    WITH INVHDR.SHIP                                      ,;
                  &lclinesAn..Amount     WITH INVHDR.SHIPAMT                                   ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                                       ,;
                  &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                  &lclinesAn..llCsSum    WITH .T.                                              ,;
                  &lclinesAn..ShpQty     WITH &lclinesAn..ShpQty  + INVHDR.SHIP                ,;
                  &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + lnInvCost                 ,;
                  &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty = 0,1,&lclinesAn..ShpQty)
          *B606667,1 Add the trade discount. [End]

          *B605596,1 Fix the summary option. [End]

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT INVHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfInvQnt.
*!*************************************************************
*! Name      : lfInvCost
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the style cost from invline.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvCost()
*!*************************************************************
FUNCTION lfInvCost
PRIVATE lcAlias , lcKeyInv

lcAlias = ALIAS()
lnInvCost = 0
SELECT INVLINE
lcKeyInv = EVAL(KEY())
=SEEK(INVHDR.INVOICE ,'INVLINE')
SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE

  *B605596,1 Modify the way we calculate cost. [Begin]
  *lnInvCost = lnInvCost + INVLINE.COST
  lnInvCost = lnInvCost + (InvLine.TotQty * InvLine.Cost)
  *B605596,1 Modify the way we calculate cost. [End]

ENDSCAN

=SEEk(lcKeyInv,'INVLINE')
SELECT (lcAlias)
RETURN lnInvCost

*--End of lfInvCost.
*!*************************************************************
*! Name      : lfRetQnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the return Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetQnt()
*!*************************************************************
FUNCTION lfRetQnt
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO RETHDR ADDITIVE
SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO RETLINE ADDITIVE
SET RELATION TO RETHDR.ACCOUNT + RETHDR.INVOICE INTO INVHDR ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*IF ATC("INVHDR.INVDATE" , lcRpExp) > 0
*  lcRpExp = STRTRAN(lcRpExp,"INVHDR.INVDATE" , "RETHDR.CRDATE")
*ENDIF
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
IF ATC("INVHDR.INVDATE" , lcInvHdExp) > 0
  lcRetHdExp = STRTRAN(lcInvHdExp,"INVHDR.INVDATE" , "RETHDR.CRDATE")
ENDIF
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT RETHDR
  
  *B606628,1 Add void condition to report. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount 
  
  *B606667,1 Add lcRetHdExp. [Begin] 
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V'
  SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND &lcRetHdExp.
  *B606667,1 Add lcRetHdExp. [End] 
  
  *B606628,1 Add void condition to report. [End]
    
    *B606628,1 Add empty invoice case. [Begin]
    *IF !EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.    
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + '_zzzzz')
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
                  
        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = 'Ret. Without Inv.'
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH '_zzzzz'         ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES    ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ELSE
            APPEND BLANK
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH '_zzzzz'        ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1 ,;
                    &lclinesAn..llCsSum    WITH .T.

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

          ENDIF
        ENDIF
      ENDIF
    ELSE
    *B606628,1 Add empty invoice case. [End]
  
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON)
        
        *B606628,1 Reduce the cost & modify amount. [Begin]
        *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
        *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETHDR.DISC_AMT
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        *B606628,1 Reduce the cost & modify amount. [End]

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          *B606628,1 Reduce the cost & modify amount. [Begin]
          *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
          *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETHDR.DISC_AMT
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          *B606628,1 Reduce the cost & modify amount. [End]
          
        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')

        *B606628,1 Reduce the cost & modify amount. [Begin]
        *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
        *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
        *        &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
        *        &lclinesAn..Season     WITH INVHDR.SEASON    ,;
        *        &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
        *        &lclinesAn..AmntRtrn   WITH RETHDR.AMOUNT    ,;
        *        &lclinesAn..Dscont     WITH RETHDR.DISC_AMT
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        *B606628,1 Reduce the cost & modify amount. [End]

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

            *B606628,1 Reduce the cost & modify amount. [Begin]
            *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
            *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
            *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETHDR.DISC_AMT
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            *B606628,1 Reduce the cost & modify amount. [End]

          ELSE
            APPEND BLANK

            *B606628,1 Reduce the cost & modify amount. [Begin]
            *REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
            *        &lclinesAn..Season     WITH INVHDR.SEASON   ,;
            *        &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
            *        &lclinesAn..AmntRtrn   WITH RETHDR.AMOUNT   ,;
            *        &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
            *        &lclinesAn..Dscont     WITH RETHDR.DISC_AMT ,;
            *        &lclinesAn..llCsSum    WITH .T.
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH INVHDR.SEASON   ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1,;
                    &lclinesAn..llCsSum    WITH .T.
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            *B606628,1 Reduce the cost & modify amount. [End]

          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT RETHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfRetQnt.
*!*************************************************************
*! Name      : lfBokQnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Book Quntities in summery.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokQnt()
*!*************************************************************
FUNCTION lfBokQnt
PRIVATE lcAlias

lcAlias = ALIAS()
*--Section of making the relations.
=lfRelation()

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("INVHDR.INVDATE" , lcRpExpBok)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*B606667,1 SSE Commented out. [End]

SELECT CUSTOMER

*B606667,1 Replace lcRpExpBok with lcAcctExp. [Begin]
*LOCATE FOR &lcRpExpBok
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExpBok with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT ORDHDR
  
  *B606667,1 Add lcOrdHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC"
  SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC" AND &lcOrdHdExp
  *B606667,1 Add lcOrdHdExp. [End]

    SELECT (lclinesAn)
    IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON)
      
      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.BOOK                          ,;
      *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.BOOKAMT                       ,;
      *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDHDR.OPENAMT)/100) ,;
      *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                          ,;
      *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfGetCost() * ORDHDR.OPEN)          ,;
      *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

      REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + OrdHdr.Open                          ,;
              &lclinesAn..Amount   WITH &lclinesAn..Amount   + OrdHdr.OpenAmt                       ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((OrdHdr.Disc * OrdHdr.OpenAmt)/100) ,;
              &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + OrdHdr.OPEN                          ,;
              &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lfTotCost()                          ,;
              &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)

        *B605596,1 Fix the summary option. [Begin] 
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.BOOK                        ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.BOOKAMT                     ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
        *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
        *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDHDR.OPEN)          ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.Open                        ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.OpenAMT                     ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
                &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lnCostVal                          ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B605596,1 Fix the summary option. [End]
                
      ENDIF
    ELSE
      APPEND BLANK
      lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')

      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
      *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
      *        &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
      *        &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
      *        &lclinesAn..UntSold    WITH ORDHDR.BOOK                        ,;
      *        &lclinesAn..Amount     WITH ORDHDR.BOOKAMT                     ,;
      *        &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
      *        &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
      *        &lclinesAn..OpnCostD   WITH lfGetCost() * ORDHDR.OPEN          ,;
      *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

      REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                   ,;
              &lclinesAn..Btname     WITH CUSTOMER.BTNAME                    ,;
              &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
              &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
              &lclinesAn..UntSold    WITH ORDHDR.Open                        ,;
              &lclinesAn..Amount     WITH ORDHDR.OpenAMT                     ,;
              &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
              &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
              &lclinesAn..OpnCostD   WITH lfTotCost()                        ,;
              &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap
        IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.
        
          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.BOOK                        ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.BOOKAMT                     ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
          *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
          *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDHDR.OPEN)          ,;
          *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDHDR.Open                        ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDHDR.OpenAMT                     ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDHDR.OPEN                        ,;
                  &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + lnCostVal                          ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B605596,1 Fix the summary option. [End]
           
        ELSE
          APPEND BLANK

          *B605596,1 Fix the summary option. [Begin]
          *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
          *        &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
          *        &lclinesAn..UntSold    WITH ORDHDR.BOOK                        ,;
          *        &lclinesAn..Amount     WITH ORDHDR.BOOKAMT                     ,;
          *        &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
          *        &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
          *        &lclinesAn..llCsSum    WITH .T.                                ,;
          *        &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
          *        &lclinesAn..OpnCostD   WITH lnCostVal * ORDHDR.OPEN            ,;
          *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          REPLACE &lclinesAn..Account    WITH "ZZZZZ"                            ,;
                  &lclinesAn..Season     WITH ORDHDR.SEASON                      ,;
                  &lclinesAn..UntSold    WITH ORDHDR.Open                        ,;
                  &lclinesAn..Amount     WITH ORDHDR.OpenAMT                     ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn                         ,;
                  &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDHDR.OPENAMT)/100 ,;
                  &lclinesAn..llCsSum    WITH .T.                                ,;
                  &lclinesAn..OpnQty     WITH ORDHDR.OPEN                        ,;
                  &lclinesAn..OpnCostD   WITH lnCostVal                          ,;
                  &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B605596,1 Fix the summary option. [End]

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

*--Section to collect the shipped quantity.

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok2 = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("ORDHDR.COMPLETE" , lcRpExpBok2)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok2,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*B606667,1 SSE Commented out. [End]

SELECT CUSTOMER

*B606667,1 Replace lcRpExpBok with lcAcctExp. [Begin]
*LOCATE FOR &lcRpExpBok2
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok2
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExpBok with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT INVHDR
  
  *B606667,1 Add lcInvHdExp. [Begin]
  *SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V"
  SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND &lcInvHdExp.
  *B606667,1 Add lcInvHdExp. [End]

    SELECT (lclinesAn)
    IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON)
      
      *B605596,1 Fix the summary option. [Begin]
      *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) ,;
      *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lfInvCost())            ,;
      *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                            ,;
      *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
     
      *B606667,1 Add the trade discount. [Begin]
      *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
      *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
      *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) ,;
      *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()            ,;
      *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                            ,;
      *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
      REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
              &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
              &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100),;
              &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lfInvCost()            ,;
              &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                            ,;
              &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
      *B606667,1 Add the trade discount. [End]

      *B605596,1 Fix the summary option. [End]

      *--Case the Recap.
      IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
      
        *B605596,1 Fix the summary option. [Begin]
        *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVHDR.SHIP * lnInvCost)            ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                          ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *B606667,1 Add the trade discount. [Begin]
        *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
        *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100 ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                            ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                          ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVHDR.SHIP                        ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVHDR.SHIPAMT        ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVHDR.SHIPAMT)/100) + ((InvHdr.Trde_Disc * InvHdr.ShipAmt)/100) ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + lnInvCost                            ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVHDR.SHIP                          ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B606667,1 Add the trade discount. [End]
                
        *B605596,1 Fix the summary option. [End]

      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT INVHDR
SET RELATION TO
SELECT ORDHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

*--Section to collect the Credit memo quantity.
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO RETHDR ADDITIVE
SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO RETLINE ADDITIVE
SET RELATION TO RETHDR.ACCOUNT + RETHDR.INVOICE INTO INVHDR ADDITIVE

SELECT CUSTOMER

*B606667,1 Add lcRetHdExp. [Begin]
*IF AT("INVHDR.INVDATE" , lcRpExpBok2) > 0
*  lcRpExpBok2 = STRTRAN(lcRpExpBok2 , "INVHDR.INVDATE" , "RETHDR.CRDATE")
*ENDIF
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok2
IF AT("INVHDR.INVDATE" , lcInvHdExp) > 0
  lcRetHdExp = STRTRAN(lcInvHdExp , "INVHDR.INVDATE" , "RETHDR.CRDATE")
ENDIF
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Add lcRetHdExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT RETHDR

  *B606628,1 Add void condition to report. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount
  
  *B606667,1 Add lcRetHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V'
  SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND &lcRetHdExp.
  *B606667,1 Add lcRetHdExp. [End]
  
  *B606628,1 Add void condition to report. [End]
  
    *B606628,1 Add empty invoice case. [Begin]
    *IF !EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.       
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + '_zzzzz')
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = 'Ret. Without Inv.'
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                &lclinesAn..Season     WITH '_zzzzz'         ,;
                &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
                &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt    ,;
                &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1

        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ELSE
            APPEND BLANK
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
                    &lclinesAn..Season     WITH '_zzzzz'        ,;
                    &lclinesAn..UntRtrn    WITH RETHDR.PIECES   ,;
                    &lclinesAn..AmntRtrn   WITH RETHDR.Gross_Amt ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn      ,;
                    &lclinesAn..Dscont     WITH RETHDR.DISC_AMT * -1,;
                    &lclinesAn..llCsSum    WITH .T.

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        ENDIF
      ENDIF
    ELSE
    *B606628,1 Add empty invoice case. [End]
  
      SELECT (lclinesAn)
      IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON)

        *B606628,1 Reduce the cost & modify amount. [Begin]
        *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
        *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETHDR.DISC_AMT
        REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
        REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B606628,1 Reduce the cost & modify amount. [End]

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          *B606628,1 Reduce the cost & modify amount. [Begin]
          *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
          *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETHDR.DISC_AMT
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.Gross_Amt ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETHDR.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetHdr.Pieces ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - lfRetCost()  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B606628,1 Reduce the cost & modify amount. [End]

        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN

SELECT RETHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfBokQnt.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function to prepare the relations of the lfBokQnt().
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT CUSTOMER
SET RELATION TO Customer.Account INTO ORDHDR ADDITIVE
SET RELATION TO Customer.Account INTO INVHDR ADDITIVE

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

SELECT ORDHDR
SET RELATION TO ORDHDR.CORDTYPE + ORDHDR.ORDER INTO ORDLINE ADDITIVE

*-- End of lfRelation.

                       *-- End of Summary code--*
*!*************************************************************
*! Name      : lfOpnDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Open Quntities in Detail.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnDet()
*!*************************************************************
FUNCTION lfOpnDet

PRIVATE lcAlias
lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO ORDHDR ADDITIVE
SELECT ORDHDR
SET RELATION TO ORDHDR.CORDTYPE + ORDHDR.ORDER INTO ORDLINE ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT ORDHDR
  
  *B606667,1 Add lcOrdHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH"
  SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OH" AND &lcOrdHdExp.
  *B606667,1 Add lcOrdHdExp. [End]
   
    SELECT ORDLINE
    =SEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER)
    SCAN REST WHILE CORDTYPE + ORDER =  ORDHDR.CORDTYPE + ORDHDR.ORDER
      SELECT (lclinesAn)
      IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON + ORDLINE.STYLE)
      
        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                       ,;
        *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
        *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
        *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + lfDetCost()

        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
        REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                                       ,;
                &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE                       ,;
                &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        *B605596,1 Fix the detail option. [End]
        
        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)
          
          *B605596,1 Fix the detail option. [Begin]
          *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
          *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
          *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
          *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + lnCostVal
          
          REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
          REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                  &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                  &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                  &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
          *B605596,1 Fix the detail option. [End]

        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')

        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                    ,;
        *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                     ,;
        *        &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
        *        &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
        *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
        *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
        *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
        *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
        *        &lclinesAn..llCsSum    WITH .F.                                 ,;
        *        &lclinesAn..Cost       WITH lfDetCost()

        REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                    ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                     ,;
                &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
                &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
                &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
                &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                &lclinesAn..llCsSum    WITH .F.                                 ,;
                &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
        *B605596,1 Fix the detail option. [End]
        
        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + ORDHDR.SEASON)     &&Case the account change and the same season.

            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
            *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
            *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100)  ,;
            *        &lclinesAn..Cost    WITH lnCostVal
            REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
            REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                    &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE      ,;
                    &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100)  ,;
                    &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
            *B605596,1 Fix the detail option. [End]
                                          
          ELSE
            APPEND BLANK            

            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                             ,;
            *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
            *        &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
            *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
            *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
            *        &lclinesAn..llCsSum    WITH .T.                                 ,;
            *        &lclinesAn..Cost       WITH lnCostVal
            REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                             ,;
                    &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
                    &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
                    &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
                    &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                    &lclinesAn..llCsSum    WITH .T.                                 ,;
                    &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
            *B605596,1 Fix the detail option. [End]
                    
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN

SELECT ORDHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfOpnDet.
*!*************************************************************
*! Name      : lfDetCost
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the style cost in detail.
*!*************************************************************
*! Called from : lfOpnDet()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDetCost()
*!*************************************************************
FUNCTION lfDetCost
PRIVATE lcAlias , lcKeyOrd

lcAlias = ALIAS()
lnCostVal = 0
SELECT ORDLINE
lcKeyOrd = EVAL(KEY())
IF SEEK(ORDLINE.STYLE , 'STYLE')
  IF STYLE.MAKE
    lnCostVal = lnCostVal + STYLE.NMCOST1 + STYLE.NMCOST2 + STYLE.NMCOST3 + STYLE.NMCOST4 + STYLE.NMCOST5
  ELSE
    lnCostVal = lnCostVal + STYLE.NICOST1 + STYLE.NICOST2 + STYLE.NICOST3 + STYLE.NICOST4 + STYLE.NICOST5
  ENDIF
ENDIF

=SEEk(lcKeyOrd,'ORDLINE')
SELECT (lcAlias)
RETURN lnCostVal

*--End of lfDetCost.
*!*************************************************************
*! Name      : lfInvDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the invoice Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvDet()
*!*************************************************************
FUNCTION lfInvDet
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO INVHDR ADDITIVE
SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]
  
  lcAccount = CUSTOMER.ACCOUNT
  SELECT INVHDR
  
  *B606667,1 Add lcInvHdExp. [Begin]
  *SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V"
  SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND &lcInvHdExp.
  *B606667,1 Add lcInvHdExp. [End]
  
    SELECT INVLINE
    =SEEK(INVHDR.INVOICE)
    SCAN WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
      SELECT (lclinesAn)
      IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON + INVLINE.STYLE)
        
        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVLINE.TOTQTY                   ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE   ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100) ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                   ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
        *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty
        
        *B606667,1 Add the trade discount. [Begin]
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVLINE.TOTQTY                   ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE   ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                   ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
        *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + INVLINE.TOTQTY                   ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE   ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                   ,;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
                &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        *B606667,1 Add the trade discount. [End]
                
        *B605596,1 Fix the detail option. [End]
        
        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          *B605596,1 Fix the detail option. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                    ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE    ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                   ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
          *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

          *B606667,1 Add the trade discount. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                    ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE    ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                   ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
          *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                    ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE    ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          *B606667,1 Add the trade discount. [End]

          *B605596,1 Fix the detail option. [End]

        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
        
        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
        *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
        *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
        *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
        *        &lclinesAn..Style      WITH INVLINE.STYLE                          ,;
        *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
        *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
        *        &lclinesAn..Dscont     WITH (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
        *        &lclinesAn..llCsSum    WITH .F.                                    ,;
        *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
        *        &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
        *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

        *B606667,1 Add the trade discount. [Begin]
        *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
        *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
        *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
        *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
        *        &lclinesAn..Style      WITH INVLINE.STYLE                          ,;
        *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
        *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
        *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
        *        &lclinesAn..llCsSum    WITH .F.                                    ,;
        *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
        *        &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
        *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                       ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                        ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
                &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
                &lclinesAn..Style      WITH INVLINE.STYLE                          ,;
                &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
                &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
                &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..llCsSum    WITH .F.                                    ,;
                &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
                &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
                &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
        *B606667,1 Add the trade discount. [End]

        *B605596,1 Fix the detail option. [End]
        
        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                         ,;
            *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE         ,;
            *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
            *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                        ,;
            *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)      ,;
            *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

            *B606667,1 Add the trade discount. [Begin]
            *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                         ,;
            *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE         ,;
            *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
            *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                        ,;
            *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)      ,;
            *        &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                         ,;
                    &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE         ,;
                    &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                    &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  + INVLINE.TOTQTY                        ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)      ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            *B606667,1 Add the trade discount. [End]
                    
            *B605596,1 Fix the detail option. [End]
                    
          ELSE
            APPEND BLANK
            
            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                ,;
            *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
            *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
            *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
            *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
            *        &lclinesAn..Dscont     WITH (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
            *        &lclinesAn..llCsSum    WITH .T.                                    ,;
            *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
            *        &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
            *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / &lclinesAn..ShpQty

            *B606667,1 Add the trade discount. [Begin]
            *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                ,;
            *        &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
            *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
            *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
            *        &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
            *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
            *        &lclinesAn..llCsSum    WITH .T.                                    ,;
            *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
            *        &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
            *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                                ,;
                    &lclinesAn..Season     WITH INVHDR.SEASON                          ,;
                    &lclinesAn..UntSold    WITH INVLINE.TOTQTY                         ,;
                    &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE         ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                             ,;
                    &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                    &lclinesAn..llCsSum    WITH .T.                                    ,;
                    &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                         ,;
                    &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST          ,;
                    &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            *B606667,1 Add the trade discount. [End]

            *B605596,1 Fix the detail option. [End]
            
           ENDIF
        ENDIF
      ENDIF
     ENDSCAN
  ENDSCAN
ENDSCAN

SELECT INVHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfInvDet.
*!*************************************************************
*! Name      : lfRetDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the return Quntities.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetDet()
*!*************************************************************
FUNCTION lfRetDet
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO RETHDR ADDITIVE
SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO RETLINE ADDITIVE
SET RELATION TO RETHDR.ACCOUNT + RETHDR.INVOICE INTO INVHDR ADDITIVE

SELECT CUSTOMER

*B606667,1 Replace lcRpExp with lcAcctExp. [Begin]
*lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*IF ATC("INVHDR.INVDATE" , lcRpExp) > 0
*  lcRpExp = STRTRAN(lcRpExp,"INVHDR.INVDATE" , "RETHDR.CRDATE")
*ENDIF
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExp
IF ATC("INVHDR.INVDATE" , lcInvHdExp) > 0
  lcRetHdExp = STRTRAN(lcInvHdExp,"INVHDR.INVDATE" , "RETHDR.CRDATE")
ENDIF
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExp with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT RETHDR

  *B606628,1 Add void condition to report. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount

  *B606667,1 Add lcRetHdExp. [Begin] 
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V'
  SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND &lcRetHdExp.
  *B606667,1 Add lcRetHdExp. [End] 

  *B606628,1 Add void condition to report. [End]
    
    *B606628,1 Add empty invoice case. [Begin]
    *IF !EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has NO invoice.
      SELECT RETLINE
      =SEEK(RETHDR.CRMEMO)
      SCAN WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO
        
        *B606628,1 Check for Tran_CD = 4. [Begin] 
        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
        *B606628,1 Check for Tran_CD = 4. [End]

        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + '_zzzzz' + RETLINE.STYLE)                
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price)   ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
                  
          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          ENDIF
        ELSE
          APPEND BLANK
          lcDescSesn = 'Ret. Without Inv.'
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH '_zzzzz'         ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn   WITH RetLine.TotQty * RetLine.Gros_Price ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          REPLACE &lclinesAn..Style WITH RetLine.Style
          
          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH '_zzzzz'         ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH RetLine.TotQty * RetLine.Gros_Price ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1,;
                      &lclinesAn..llCsSum    WITH .T.

              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)

            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
    *B606628,1 Add empty invoice case. [End]
    
      SELECT RETLINE
      =SEEK(RETHDR.CRMEMO)
      SCAN WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        *B606628,1 Check for Tran_CD = 4. [Begin] 
        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
        *B606628,1 Check for Tran_CD = 4. [End]
      
        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON + RETLINE.STYLE)

          *B606628,1 Get units from Return lines. [Begin]
          *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
          *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETLINE.DISC_AMT
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
          *B606628,1 Get units from Return lines. [End]

          *B606628,1 Reduce the cost. [Begin]
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          *B606628,1 Reduce the cost. [End]

          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
            
            *B606628,1 Get units from Return lines. [Begin]
            *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
            *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
            *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETLINE.DISC_AMT
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            *B606628,1 Get units from Return lines. [End]
            
            *B606628,1 Reduce the cost. [Begin]
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                    &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
            *B606628,1 Reduce the cost. [End]

          ENDIF
        ELSE
          APPEND BLANK
          lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')

          *B606628,1 Get units from Return lines. [Begin]
          *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
          *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
          *        &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
          *        &lclinesAn..Season     WITH INVHDR.SEASON    ,;
          *        &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
          *        &lclinesAn..AmntRtrn   WITH RETHDR.AMOUNT    ,;
          *        &lclinesAn..Dscont     WITH RETLINE.DISC_AMT
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty    ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1
          *B606628,1 Get units from Return lines. [End]

          *B606628,1 Reduce the cost. [Begin]
          REPLACE &lclinesAn..Style WITH RetLine.Style
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
          *B606628,1 Reduce the cost. [End]

          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.

              *B606628,1 Get units from Return lines. [Begin]
              *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
              *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT    ,;
              *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETLINE.DISC_AMT
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              *B606628,1 Get units from Return lines. [End]
                      
              *B606628,1 Reduce the cost. [Begin]
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
              *B606628,1 Reduce the cost. [End]

            ELSE
              APPEND BLANK
              
              *B606628,1 Get units from Return lines. [Begin]
              *REPLACE &lclinesAn..Account    WITH "ZZZZZ"         ,;
              *        &lclinesAn..Season     WITH INVHDR.SEASON    ,;
              *        &lclinesAn..UntRtrn    WITH RETHDR.PIECES    ,;
              *        &lclinesAn..AmntRtrn   WITH RETHDR.AMOUNT    ,;
              *        &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
              *        &lclinesAn..Dscont     WITH RETLINE.DISC_AMT ,;
              *        &lclinesAn..llCsSum    WITH .T.
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT  * -1 ,;
                      &lclinesAn..llCsSum    WITH .T.
              *B606628,1 Get units from Return lines. [End]

              *B606628,1 Reduce the cost. [Begin]
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..ShpQty=0,1,&lclinesAn..ShpQty)
              *B606628,1 Reduce the cost. [End]

            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDSCAN

SELECT RETHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfRetDet.
*!*************************************************************
*! Name      : lfBokDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Book Quntities in Detail.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokDet()
*!*************************************************************
FUNCTION lfBokDet
PRIVATE lcAlias

lcAlias = ALIAS()
*--Section of making the relations.
=lfRlatDet()

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("INVHDR.INVDATE" , lcRpExpBok)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*B606667,1 SSE Commented out. [End]

SELECT CUSTOMER

*B606667,1 Replace lcRpExpBok with lcAcctExp. [Begin]
*LOCATE FOR &lcRpExpBok
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExpBok with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT ORDHDR

  *B606667,1 Add lcOrdHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC"
  SCAN WHILE ACCOUNT + CORDTYPE + ORDER = lcAccount FOR ORDHDR.STATUS $ "OHC" AND &lcOrdHdExp
  *B606667,1 Add lcOrdHdExp. [End]

    SELECT ORDLINE
    =SEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER)
    SCAN REST WHILE CORDTYPE + ORDER =  ORDHDR.CORDTYPE + ORDHDR.ORDER
      SELECT (lclinesAn)
      IF SEEK(ORDHDR.ACCOUNT + ORDHDR.SEASON + ORDLINE.STYLE)
        
        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
        *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
        *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
        *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)       ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + (ORDLINE.TOTQTY * ORDLINE.PRICE)  ,;
                &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
                &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)       ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B605596,1 Fix the detail option. [End]

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + ORDHDR.SEASON)
          
          *B605596,1 Fix the detail option. [Begin]
          *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
          *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty  + ORDLINE.TOTQTY                       ,;
          *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)        ,;
          *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                      ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount  + (ORDLINE.TOTQTY * ORDLINE.PRICE) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
                  &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty  + ORDLINE.TOTQTY                       ,;
                  &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)        ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B605596,1 Fix the detail option. [End]

        ENDIF
      ELSE
        APPEND BLANK
        lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')

        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                     ,;
        *        &lclinesAn..Btname     WITH CUSTOMER.BTNAME                      ,;
        *        &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
        *        &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
        *        &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
        *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
        *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
        *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
        *        &lclinesAn..llCsSum    WITH .F.                                  ,;
        *        &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
        *        &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
        *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT                     ,;
                &lclinesAn..Btname     WITH CUSTOMER.BTNAME                      ,;
                &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
                &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
                &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
                &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
                &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
                &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                &lclinesAn..llCsSum    WITH .F.                                  ,;
                &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
                &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
                &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B605596,1 Fix the detail option. [End]

        *--Case the Recap.
        IF llRPRecap
          IF SEEK("ZZZZZ" + ORDHDR.SEASON)         &&Case the account change and the same season.
          
            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
            *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
            *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100),;
            *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
            *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)         ,;
            *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                       ,;
                    &lclinesAn..Amount   WITH &lclinesAn..Amount   + (ORDLINE.TOTQTY * ORDLINE.PRICE)  ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100),;
                    &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                       ,;
                    &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lnCostVal * ORDLINE.TOTQTY)         ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            *B605596,1 Fix the detail option. [End]
            
          ELSE
            APPEND BLANK
            
            *B605596,1 Fix the detail option. [Begin]
            *REPLACE &lclinesAn..Account    WITH "ZZZZZ"                              ,;
            *        &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
            *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
            *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
            *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
            *        &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
            *        &lclinesAn..llCsSum    WITH .T.                                  ,;
            *        &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
            *        &lclinesAn..OpnCostD   WITH lnCostVal * ORDLINE.TOTQTY           ,;
            *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            REPLACE &lclinesAn..Account    WITH "ZZZZZ"                              ,;
                    &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
                    &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
                    &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
                    &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
                    &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
                    &lclinesAn..llCsSum    WITH .T.                                  ,;
                    &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
                    &lclinesAn..OpnCostD   WITH lnCostVal * ORDLINE.TOTQTY           ,;
                    &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            *B605596,1 Fix the detail option. [End]

          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN

*--Section to collect the shipped quantity.

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok2 = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("ORDHDR.COMPLETE" , lcRpExpBok2)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok2,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*B606667,1 SSE Commented out. [End]

SELECT CUSTOMER

*B606667,1 Replace lcRpExpBok with lcAcctExp. [Begin]
*LOCATE FOR &lcRpExpBok2
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok2
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Replace lcRpExpBok with lcAcctExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT INVHDR

  *B606667,1 Add lcInvHdExp. [Begin]
  *SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V"
  SCAN WHILE ACCOUNT + INVOICE = lcAccount FOR INVHDR.STATUS <> "V" AND &lcInvHdExp.
  *B606667,1 Add lcInvHdExp. [End]

    SELECT INVLINE
    =SEEK(INVHDR.INVOICE)
    SCAN WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
      SELECT (lclinesAn)
      IF SEEK(INVHDR.ACCOUNT + INVHDR.SEASON + INVLINE.STYLE)

        *B605596,1 Fix the detail option. [Begin]
        *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100) ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
        *        &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE                                  ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

        *B606667,1 Add the trade discount. [Begin]
        *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) ,;
        *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
        *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
        *        &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
        *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                  ,;
        *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
                &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
                &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
                &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
                &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                  ,;
                &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
        *B606667,1 Add the trade discount. [End]
                
        *B605596,1 Fix the detail option. [End]

        *--Case the Recap.
        IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)

          *B605596,1 Fix the detail option. [Begin]
          *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                               ,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                ,;
          *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          *B606667,1 Add the trade discount. [Begin]
          *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
          *        &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
          *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                ,;
          *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                               ,;
          *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                ,;
          *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100) ,;
                  &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
                  &lclinesAn..Amount   WITH &lclinesAn..Amount   + (INVLINE.TOTQTY * INVLINE.PRICE)                                ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                               ,;
                  &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B606667,1 Add the trade discount. [End]

          *B605596,1 Fix the detail option. [End]

        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN

SELECT INVHDR
SET RELATION TO
SELECT ORDHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

*--Section to collect the Credit memo quantity.
SELECT CUSTOMER
SET RELATION TO CUSTOMER.ACCOUNT INTO RETHDR ADDITIVE
SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO RETLINE ADDITIVE
SET RELATION TO RETHDR.ACCOUNT + RETHDR.INVOICE INTO INVHDR ADDITIVE

SELECT CUSTOMER
*B606667,1 Add lcRetHdExp. [Begin]
*IF AT("INVHDR.INVDATE" , lcRpExpBok2) > 0
*  lcRpExpBok2 = STRTRAN(lcRpExpBok2 , "INVHDR.INVDATE" , "RETHDR.CRDATE")
*ENDIF
*LOCATE FOR &LCRPEXP
*SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcRpExpBok2
IF AT("INVHDR.INVDATE" , lcInvHdExp) > 0
  lcRetHdExp = STRTRAN(lcInvHdExp , "INVHDR.INVDATE" , "RETHDR.CRDATE")
ENDIF
LOCATE FOR &lcAcctExp
SCAN WHILE TYPE + ACCOUNT + STORE ='M' FOR &lcAcctExp
*B606667,1 Add lcRetHdExp. [End]

  lcAccount = CUSTOMER.ACCOUNT
  SELECT RETHDR

  *B606628,1 Add void condition to report. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount

  *B606667,1 Add lcRetHdExp. [Begin]
  *SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V'
  SCAN WHILE ACCOUNT + CRMEMO = lcAccount FOR RetHdr.Status <> 'V' AND &lcRetHdExp.
  *B606667,1 Add lcRetHdExp. [End]

  *B606628,1 Add void condition to report. [End]

    *B606628,1 Get empty invoice also. [Begin]
    *IF !EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
    IF EMPTY(RETHDR.INVOICE)                 &&Case the credit memo has invoice.
      SELECT RETLINE
      =SEEK(RETHDR.CRMEMO)
      SCAN WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        *B606628,1 Check for Tran_CD = 4. [Begin] 
        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
        *B606628,1 Check for Tran_CD = 4. [End]
      
        SELECT (lclinesAn)
        IF SEEK(RETHDR.ACCOUNT + '_zzzzz' + RETLINE.STYLE)
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
                  
          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + '_zzzzz')
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          ENDIF
        ELSE
          APPEND BLANK
          lcDescSesn = 'Ret. Without Inv.'
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH '_zzzzz'         ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1

          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          REPLACE &lclinesAn..Style WITH RetLine.Style

          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + '_zzzzz')         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT

            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH '_zzzzz'         ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1,;
                      &lclinesAn..llCsSum    WITH .T.

              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
    *B606628,1 Get empty invoice also. [End]
  
      SELECT RETLINE
      =SEEK(RETHDR.CRMEMO)
      SCAN WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RETHDR.CRMEMO

        *B606628,1 Check for Tran_CD = 4. [Begin] 
        IF cRet_TrnCD = '4'
          LOOP
        ENDIF
        *B606628,1 Check for Tran_CD = 4. [End]

        SELECT (lclinesAn)
        
        *B606628,1 Add the style to the seek. [Begin]
        *IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON)
        IF SEEK(RETHDR.ACCOUNT + INVHDR.SEASON + RETLINE.STYLE)
        *B606628,1 Add the style to the seek. [End]

          *B606628,1 Reduce the cost. [Begin]
          *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
          *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
          *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETLINE.DISC_AMT
          REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                  &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
          *B606628,1 Reduce the cost. [End]

          *--Case the Recap.
          IF llRPRecap AND SEEK("ZZZZZ" + INVHDR.SEASON)
            
            *B606628,1 Reduce the cost. [Begin]
            *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETHDR.PIECES   ,;
            *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETHDR.AMOUNT   ,;
            *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + RETLINE.DISC_AMT
            REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETline.Totqty   ,;
                    &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                    &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
            REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RETline.Totqty ,;
                    &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RETline.Totqty * RETline.Cost)  ,;
                    &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            *B606628,1 Reduce the cost. [End]

          ENDIF
        
        *B606628,1 Add case of not finding style in temp file. [Begin]
        ELSE
          APPEND BLANK
          lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
          REPLACE &lclinesAn..Account    WITH CUSTOMER.ACCOUNT ,;
                  &lclinesAn..Btname     WITH CUSTOMER.BTNAME  ,;
                  &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                  &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                  &lclinesAn..UntRtrn    WITH RETline.Totqty    ,;
                  &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                  &lclinesAn..Dscont     WITH RETLINE.DISC_AMT * -1
          REPLACE &lclinesAn..Style    WITH RetLine.Style
          REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                  &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                  &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

          *--Case the Recap.
          IF llRPRecap
            IF SEEK("ZZZZZ" + INVHDR.SEASON)         &&Case the account change and the same season.
              REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - RETLINE.DISC_AMT
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            ELSE
              APPEND BLANK
              REPLACE &lclinesAn..Account    WITH "ZZZZZ"          ,;
                      &lclinesAn..Season     WITH INVHDR.SEASON    ,;
                      &lclinesAn..UntRtrn    WITH RetLine.TotQty   ,;
                      &lclinesAn..AmntRtrn   WITH (RetLine.TotQty * RetLine.Gros_Price) ,;
                      &lclinesAn..SeasonDesc WITH lcDescSesn       ,;
                      &lclinesAn..Dscont     WITH RETLINE.DISC_AMT  * -1 ,;
                      &lclinesAn..llCsSum    WITH .T.
              REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty  - RetLine.TOTQTY                   ,;
                      &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
                      &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
            ENDIF
          ENDIF
          *B606628,1 Add case of not finding style in temp file. [End]
                
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDSCAN

SELECT RETHDR
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO

SELECT (lcAlias)

*--End of lfBokDet.
*!*************************************************************
*! Name      : lfRlatDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function to prepare the relations of the lfBokQnt().
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : lfBokDet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRlatDet()
*!*************************************************************
FUNCTION lfRlatDet

SELECT CUSTOMER
SET RELATION TO Customer.Account INTO ORDHDR ADDITIVE
SET RELATION TO Customer.Account INTO INVHDR ADDITIVE

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

SELECT ORDHDR
SET RELATION TO ORDHDR.CORDTYPE + ORDHDR.ORDER INTO ORDLINE ADDITIVE

*-- End of lfRlatDet.
                       *-- End of Detail code--*
*--Style section.
*!*************************************************************
*! Name      : lfOpnSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Open Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpnSty()
*!*************************************************************
FUNCTION lfOpnSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
SELECT ORDLINE
SET ORDER TO TAG ORDLINES
SET RELATION TO ORDLINE.CORDTYPE + ORDLINE.ORDER INTO ORDHDR ADDITIVE
SET RELATION TO ORDLINE.STYLE INTO STYLE ADDITIVE

lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
IF ATC("ORDHDR.COMPLETE" , lcRpExp) > 0
  lcRpExp = STRTRAN(lcRpExp,"ORDHDR.COMPLETE" , "ORDLINE.COMPLETE")
ENDIF
LOCATE FOR &LCRPEXP

SCAN FOR &lcRpExp AND ORDHDR.STATUS $ "OH"
  SELECT (lclinesAn)
  IF SEEK(ORDLINE.STYLE + STR(ORDLINE.GROS_PRICE,9,2))

    *B605596,1 Fix Open qtys sort by Style. [Begin] 
    *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                          ,;
    *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE     ,;
    *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
    *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + lfDetCost()

    REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty)
    REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + ORDLINE.TOTQTY                          ,;
            &lclinesAn..Amount  WITH &lclinesAn..Amount  + ORDLINE.TOTQTY * ORDLINE.PRICE     ,;
            &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
            &lclinesAn..Cost    WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    
    *B606667,1 Add Open cost to Ship cost. [Begin]        
    *REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    *B606667,1 Add Open cost to Ship cost. [End]

    *B605596,1 Fix Open qtys sort by Style. [End] 

  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')

    *B605596,1 Fix Open qtys sort by Style. [Begin] 
    *REPLACE &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
    *        &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
    *        &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)              ,;
    *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
    *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE ,;
    *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH ORDLINE.GROS_PRICE                  ,;
    *        &lclinesAn..Cost       WITH lfDetCost()
    
    REPLACE &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * OrdLine.TotQty) ,;
            &lclinesAn..Style      WITH ORDLINE.STYLE                       ,;
            &lclinesAn..Season     WITH ORDHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)              ,;
            &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                      ,;
            &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE ,;
            &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
            &lclinesAn..PriceSty   WITH ORDLINE.PRICE                  ,;
            &lclinesAn..Cost       WITH &lclinesAn..OpnCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)

    *B606667,1 Add Open cost to Ship cost. [Begin]        
    *REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    *B606667,1 Add Open cost to Ship cost. [End]

    *B605596,1 Fix Open qtys sort by Style. [End]

  ENDIF
ENDSCAN

SELECT ORDLINE
SET RELATION TO

SELECT (lcAlias)

*--End of lfOpnSty.
*!*************************************************************
*! Name      : lfInvSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the invoice Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvSty()
*!*************************************************************
FUNCTION lfInvSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
SET ORDER TO TAG INVHDR
SELECT INVLINE
SET ORDER TO TAG INVLINES
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
SET RELATION TO INVLINE.STYLE INTO STYLE ADDITIVE

lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
IF ATC("INVHDR.INVDATE" , lcRpExp) > 0
  lcRpExp = STRTRAN(lcRpExp,"INVHDR.INVDATE" , "INVLINE.INVDATE")
ENDIF
LOCATE FOR &LCRPEXP

SCAN FOR &lcRpExp AND INVHDR.STATUS <> "V"
  SELECT (lclinesAn)
  
  *B606667,1 Replace style with style major. [Begin]
  *IF SEEK(INVLINE.STYLE + STR(INVLINE.GROS_PRICE,9,2))
  IF SEEK(PADR(SUBSTR(INVLINE.STYLE,1,lnStyLnAp),19) + STR(INVLINE.PRICE,9,2))
  *B606667,1 Replace style with style major. [End]

    *B605596,1 Fix the shipped quantities. [Begin]
    *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                          ,;
    *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.GROS_PRICE     ,;
    *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100) ,;
    *        &lclinesAn..Cost    WITH &lclinesAn..Cost    + INVLINE.COST
    
    *B606667,1 Add the trade discount. [Begin]
    *REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                          ,;
    *        &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE     ,;
    *        &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) ,;
    *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
    *        &lclinesAn..Cost    WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    REPLACE &lclinesAn..UntSold WITH &lclinesAn..UntSold + INVLINE.TOTQTY                          ,;
            &lclinesAn..Amount  WITH &lclinesAn..Amount  + INVLINE.TOTQTY * INVLINE.PRICE     ,;
            &lclinesAn..Dscont  WITH &lclinesAn..Dscont  + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST) ,;
            &lclinesAn..Cost    WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    *B606667,1 Add the trade discount. [End]

    *B605596,1 Fix the shipped quantities. [End]

  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')

    *B605596,1 Fix the shipped quantities. [Begin]
    *REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                       ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
    *        &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)              ,;
    *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                      ,;
    *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.GROS_PRICE ,;
    *        &lclinesAn..Dscont     WITH (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH INVLINE.GROS_PRICE                  ,;
    *        &lclinesAn..Cost       WITH INVLINE.COST
    
    *B606667,1 Add the trade discount. [Begin]
    *REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                       ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
    *        &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)              ,;
    *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                      ,;
    *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE ,;
    *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
    *        &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST  ,;
    *        &lclinesAn..PriceSty   WITH INVLINE.PRICE                  ,;
    *        &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    REPLACE &lclinesAn..Style      WITH SUBSTR(INVLINE.STYLE,1,lnStyLnAp)  ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)              ,;
            &lclinesAn..UntSold    WITH INVLINE.TOTQTY                      ,;
            &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE ,;
            &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100) ,;
            &lclinesAn..ShpCostD   WITH INVLINE.TOTQTY * INVLINE.COST  ,;
            &lclinesAn..PriceSty   WITH INVLINE.PRICE                  ,;
            &lclinesAn..Cost       WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold=0,1,&lclinesAn..UntSold)
    *B606667,1 Add the trade discount. [End]
            
    *B605596,1 Fix the shipped quantities. [End]

  ENDIF
ENDSCAN

SELECT INVLINE
SET RELATION TO

SELECT (lcAlias)

*--End of lfInvSty.
*!*************************************************************
*! Name      : lfRetSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the return Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRetSty()
*!*************************************************************
FUNCTION lfRetSty
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT RETHDR
SET ORDER TO TAG RETHDR
SELECT RETLINE
SET ORDER TO TAG RETLINES
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE
SET RELATION TO RETLINE.STYLE INTO STYLE ADDITIVE

lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
IF ATC("INVLINE.INVDATE" , lcRpExp) > 0
  lcRpExp = STRTRAN(lcRpExp,"INVLINE.INVDATE" , "RETLINE.CRDATE")
ENDIF
LOCATE FOR &LCRPEXP

*B606628,1 Select all Credit memos. [Begin]
*SCAN FOR &lcRpExp AND !EMPTY(RETHDR.INVOICE)
SCAN FOR &lcRpExp AND RetHdr.Status <> 'V'
  *-- Check for Tran_CD = 4
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF
*B606628,1 Select all Credit memos. [End]

  SELECT (lclinesAn)

  *B606667,1 Replace style with style major. [Begin]
  *IF SEEK(RETLINE.STYLE + STR(RETLINE.GROS_PRICE,9,2))  
  IF SEEK(PADR(SUBSTR(RETLINE.STYLE,1,lnStyLnAp),19) + STR(RETLINE.GROS_PRICE,9,2))  
  *B606667,1 Replace style with style major. [End]
  
    *B606628,1 Reduce the cost & adjust discount. [Begin]
    *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
    *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
    *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
            &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold-&lclinesAn..UntRtrn=0,1,&lclinesAn..UntSold-&lclinesAn..UntRtrn)
    *B606628,1 Reduce the cost & adjust discount. [End]

  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    
    *B606628,1 Get Style description from Style file. [Begin]    
    *REPLACE &lclinesAn..Style      WITH RETLINE.STYLE                       ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
    *        &lclinesAn..StyDesc    WITH LEFT(RETLINE.DESC,30)               ,;
    *        &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
    *        &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
    *        &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    REPLACE &lclinesAn..Style      WITH SUBSTR(RETLINE.STYLE,1,lnStyLnAp)  ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH IIF(EMPTY(RetLine.Desc),LEFT(Style.Desc1,30),LEFT(RETLINE.DESC,30)),;
            &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
            &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
            &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 * -1,;
            &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    *B606628,1 Get Style description from Style file. [End]
            
    *B606628,1 Reduce the cost. [Begin]
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH &lclinesAn..ShpCostD / IIF(&lclinesAn..UntSold-&lclinesAn..UntRtrn=0,1,&lclinesAn..UntSold-&lclinesAn..UntRtrn)
    *B606628,1 Reduce the cost. [End]

  ENDIF
ENDSCAN

SELECT RETLINE
SET RELATION TO

SELECT (lcAlias)

*--End of lfRetSty.
*!*************************************************************
*! Name      : lfBokSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/08/2001
*! Purpose   : Function used to collect the Book Quntities in Style.
*!*************************************************************
*! Called from : lfcolctDat()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokSty()
*!*************************************************************
FUNCTION lfBokSty
PRIVATE lcAlias

lcAlias = ALIAS()

*--Section of making the relations.
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
SELECT ORDLINE
SET ORDER TO TAG ORDLINES
SET RELATION TO ORDLINE.CORDTYPE + ORDLINE.ORDER INTO ORDHDR ADDITIVE
SET RELATION TO ORDLINE.STYLE INTO STYLE ADDITIVE

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("INVHDR.INVDATE" , lcRpExpBok)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok = STRTRAN(lcRpExpBok , SUBSTR(lcRpExpBok , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*IF ATC("ORDHDR.COMPLETE" , lcRpExpBok) > 0
*  lcRpExpBok = STRTRAN(lcRpExpBok ,"ORDHDR.COMPLETE" , "ORDLINE.COMPLETE")
*ENDIF
*B606667,1 SSE Commented out. [End]

*B606667,1 Change the expression to the new order header one. [Begin]
*LOCATE FOR &lcRpExpBok
*SCAN FOR &lcRpExp AND ORDHDR.STATUS $ "OHC"
PRIVATE lcOrderExp  
lcOrderExp = lcRpExp
lcOrderExp = STRTRAN(lcOrderExp ,"ORDHDR.COMPLETE" , "ORDLINE.COMPLETE")
lcOrderExp = STRTRAN(lcOrderExp ,lcInvHdExp , ".T.")
LOCATE FOR &lcOrderExp
SCAN FOR &lcOrderExp AND ORDHDR.STATUS $ "OHC"
*B606667,1 Change the expression to the new order header one. [End]

  SELECT (lclinesAn)
  IF SEEK(ORDLINE.STYLE + STR(ORDLINE.GROS_PRICE,9,2))

    *B605596,1 Fix Booked qtys sort by Style. [Begin]
    *REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                        ,;
    *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDLINE.TOTQTY * ORDLINE.GROS_PRICE   ,;
    *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100) ,;
    *        &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                        ,;
    *        &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)        ,;
    *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    REPLACE &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + ORDLINE.TOTQTY                        ,;
            &lclinesAn..Amount   WITH &lclinesAn..Amount   + ORDLINE.TOTQTY * ORDLINE.PRICE   ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100) ,;
            &lclinesAn..OpnQty   WITH &lclinesAn..OpnQty   + ORDLINE.TOTQTY                        ,;
            &lclinesAn..OpnCostD WITH &lclinesAn..OpnCostD + (lfDetCost() * ORDLINE.TOTQTY)        ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    *B606667,1 Add Open cost to Ship cost. [Begin]        
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    *B606667,1 Add Open cost to Ship cost. [End]

    *B605596,1 Fix Booked qtys sort by Style. [End]

  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(ORDHDR.SEASON,'SEASON')

    *B605596,1 Fix Booked qtys sort by Style. [Begin]
    *REPLACE &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
    *        &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
    *        &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)               ,;
    *        &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
    *        &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.GROS_PRICE  ,;
    *        &lclinesAn..Dscont     WITH (ORDLINE.DISC_PCNT * ORDLINE.TOTQTY * ORDLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH ORDLINE.GROS_PRICE                   ,;
    *        &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
    *        &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
    *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    REPLACE &lclinesAn..Style      WITH ORDLINE.STYLE                        ,;
            &lclinesAn..Season     WITH ORDHDR.SEASON                        ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
            &lclinesAn..StyDesc    WITH LEFT(ORDLINE.DESC1,30)               ,;
            &lclinesAn..UntSold    WITH ORDLINE.TOTQTY                       ,;
            &lclinesAn..Amount     WITH ORDLINE.TOTQTY * ORDLINE.PRICE  ,;
            &lclinesAn..Dscont     WITH (ORDHDR.DISC * ORDLINE.TOTQTY * ORDLINE.PRICE)/100 ,;
            &lclinesAn..PriceSty   WITH ORDLINE.PRICE                   ,;
            &lclinesAn..OpnQty     WITH ORDLINE.TOTQTY                       ,;
            &lclinesAn..OpnCostD   WITH lfDetCost() * ORDLINE.TOTQTY         ,;
            &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    *B606667,1 Add Open cost to Ship cost. [Begin]        
    REPLACE &lclinesAn..ShpCostD WITH &lclinesAn..OpnCostD
    *B606667,1 Add Open cost to Ship cost. [End]

    *B605596,1 Fix Booked qtys sort by Style. [End]

  ENDIF
ENDSCAN

SELECT ORDLINE
SET RELATION TO

*--Section to collect the shipped quantity.

SELECT INVHDR
SET ORDER TO TAG INVHDR
SELECT INVLINE
SET ORDER TO TAG INVLINES
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
SET RELATION TO INVLINE.STYLE INTO STYLE ADDITIVE

*B606667,1 SSE Commented out. [Begin]
*lcRpExpBok2 = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*lnInvPos = AT("ORDHDR.COMPLETE" , lcRpExpBok2)
*IF lnInvPos > 0
*  lnInvPos = lnInvPos - 13
*  lnPos = AT('AND' , SUBSTR(lcRpExpBok2,lnInvPos))
*  IF lnPos > 0
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos , lnPos-1) , " .T. ")
*  ELSE
*    lcRpExpBok2 = STRTRAN(lcRpExpBok2 , SUBSTR(lcRpExpBok2 , lnInvPos) , ".T.")
*  ENDIF
*ENDIF
*IF ATC("INVHDR.INVDATE" , lcRpExpBok2) > 0
*  lcRpExp = STRTRAN(lcRpExpBok2,"INVHDR.INVDATE" , "INVLINE.INVDATE")
*ENDIF
*B606667,1 SSE Commented out. [End]

*B606667,1 Change the expression to the new Invoice header one. [Begin]
*LOCATE FOR &lcRpExpBok2
*SCAN FOR &lcRpExpBok2 AND INVHDR.STATUS <> "V"
PRIVATE lcInvoicEx
lcInvoicEx = lcRpExp
lcInvoicEx = STRTRAN(lcInvoicEx ,"INVHDR.INVDATE" , "INVLINE.INVDATE")
lcInvoicEx = STRTRAN(lcInvoicEx ,lcOrdHdExp , ".T.")
LOCATE FOR &lcInvoicEx
SCAN FOR &lcInvoicEx. AND INVHDR.STATUS <> "V"
*B606667,1 Change the expression to the new Invoice header one. [End]

  SELECT (lclinesAn)
  IF SEEK(INVLINE.STYLE + STR(INVLINE.GROS_PRICE,9,2))   
  
    *B605596,1 Fix booked qtys sort by Style. [Begin]
    *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100) ,;
    *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
    *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
    *        &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
    *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE                                  ,;
    *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    *B606667,1 Add the trade discount. [Begin]
    *REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) ,;
    *        &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
    *        &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
    *        &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
    *        &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE                                  ,;
    *        &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
    REPLACE &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD + (INVLINE.TOTQTY * INVLINE.COST)                                 ,;
            &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty   + INVLINE.TOTQTY                                                  ,;
            &lclinesAn..UntSold  WITH &lclinesAn..UntSold  + INVLINE.TOTQTY                                                  ,;
            &lclinesAn..Amount   WITH &lclinesAn..Amount   + INVLINE.TOTQTY * INVLINE.PRICE                                  ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))            
    *B606667,1 Add the trade discount. [End]

    *B605596,1 Fix booked qtys sort by Style. [End]

  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')

    *B605596,1 Fix booked qtys sort by Style. [Begin]
    *REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                        ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                        ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
    *        &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)               ,;
    *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                       ,;
    *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE       ,;
    *        &lclinesAn..Dscont     WITH (INVLINE.DISC_PCNT * INVLINE.TOTQTY * INVLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH INVLINE.GROS_PRICE                   ,;
    *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                       ,;
    *        &lclinesAn..ShpCostD   WITH INVLINE.COST * INVLINE.TOTQTY        ,;
    *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))

    *B606667,1 Add the trade discount. [Begin]
    *REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                        ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                        ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
    *        &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)               ,;
    *        &lclinesAn..UntSold    WITH INVLINE.TOTQTY                       ,;
    *        &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE       ,;
    *        &lclinesAn..Dscont     WITH (INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH INVLINE.PRICE                   ,;
    *        &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                       ,;
    *        &lclinesAn..ShpCostD   WITH INVLINE.COST * INVLINE.TOTQTY        ,;
    *        &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
    REPLACE &lclinesAn..Style      WITH INVLINE.STYLE                        ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                        ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                           ,;
            &lclinesAn..StyDesc    WITH LEFT(INVLINE.DESC1,30)               ,;
            &lclinesAn..UntSold    WITH INVLINE.TOTQTY                       ,;
            &lclinesAn..Amount     WITH INVLINE.TOTQTY * INVLINE.PRICE       ,;
            &lclinesAn..Dscont     WITH ((INVHDR.DISCPCNT * INVLINE.TOTQTY * INVLINE.PRICE)/100) + ((InvHdr.Trde_Disc * InvLine.TotQty * InvLine.Price)/100),;
            &lclinesAn..PriceSty   WITH INVLINE.PRICE                   ,;
            &lclinesAn..ShpQty     WITH INVLINE.TOTQTY                       ,;
            &lclinesAn..ShpCostD   WITH &lclinesAn..ShpCostD + (INVLINE.COST * INVLINE.TOTQTY)        ,;
            &lclinesAn..Cost       WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
    *B606667,1 Add the trade discount. [End]

    *B605596,1 Fix booked qtys sort by Style. [End]

  ENDIF
ENDSCAN

SELECT INVLINE
SET RELATION TO

*--Section to collect the Credit memo quantity.

SELECT RETHDR
SET ORDER TO TAG RETHDR
SELECT RETLINE
SET ORDER TO TAG RETLINES
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE
SET RELATION TO RETLINE.STYLE INTO STYLE ADDITIVE

*B606667,1 Change the expression to be on Return line date. [Begin]
*IF AT("INVLINE.INVDATE" , lcRpExpBok2) > 0
*  lcRpExpBok2 = STRTRAN(lcRpExpBok2 , "INVLINE.INVDATE" , "RETLINE.CRDATE")
*ENDIF
*LOCATE FOR &lcRpExpBok2
PRIVATE lcReturnEx
lcReturnEx = lcRpExp
lcReturnEx = STRTRAN(lcReturnEx ,"INVHDR.INVDATE" , "RETLINE.CRDATE")
lcReturnEx = STRTRAN(lcReturnEx ,lcOrdHdExp , ".T.")
LOCATE FOR &lcReturnEx
*B606667,1 Change the expression to be on Return line date. [End]

*B606628,1 Select all Credit memos. [Begin] 
*SCAN FOR &lcRpExpBok2 AND !EMPTY(RETHDR.INVOICE)

*B606667,1 Change the expression to be on Return line date. [Begin]
*SCAN FOR &lcRpExpBok2 AND RetHdr.Status <> 'V'
SCAN FOR &lcReturnEx AND RetHdr.Status <> 'V'
*B606667,1 Change the expression to be on Return line date. [End]

  *-- Check for Tran_CD = 4.
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF
*B606628,1 Select all Credit memos. [End]

  SELECT (lclinesAn)
  IF SEEK(RETLINE.STYLE + STR(RETLINE.GROS_PRICE,9,2))

    *B606628,1 Reduce the cost & adjust discount. [Begin]
    *REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
    *        &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
    *        &lclinesAn..Dscont   WITH &lclinesAn..Dscont   + ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..UntRtrn  WITH &lclinesAn..UntRtrn  + RETLINE.TOTQTY                          ,;
            &lclinesAn..AmntRtrn WITH &lclinesAn..AmntRtrn + RETLINE.TOTQTY * RETLINE.GROS_PRICE     ,;
            &lclinesAn..Dscont   WITH &lclinesAn..Dscont   - ((RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100)
    REPLACE &lclinesAn..ShpQty WITH &lclinesAn..ShpQty - RetLine.TOTQTY                      ,;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
    *B606628,1 Reduce the cost & adjust discount. [End]
            
  *B606628,1 Select credit memo with empty invoices. [Begin]
  ELSE
    APPEND BLANK
    lcDescSesn = gfCodDes(INVHDR.SEASON,'SEASON')
    
    *B606628,1 Get Style description from Style file. [Begin]
    *REPLACE &lclinesAn..Style      WITH RETLINE.STYLE                       ,;
    *        &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
    *        &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
    *        &lclinesAn..StyDesc    WITH LEFT(RETLINE.DESC,30)               ,;
    *        &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
    *        &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
    *        &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 ,;
    *        &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    REPLACE &lclinesAn..Style      WITH RETLINE.STYLE                       ,;
            &lclinesAn..Season     WITH INVHDR.SEASON                       ,;
            &lclinesAn..SeasonDesc WITH lcDescSesn                          ,;
            &lclinesAn..StyDesc    WITH IIF(EMPTY(RetLine.Desc),LEFT(Style.Desc1,30),LEFT(RETLINE.DESC,30)) ,;
            &lclinesAn..UntRtrn    WITH RETLINE.TOTQTY                      ,;
            &lclinesAn..AmntRtrn   WITH RETLINE.TOTQTY * RETLINE.GROS_PRICE ,;
            &lclinesAn..Dscont     WITH (RETLINE.DISC_PCNT * RETLINE.TOTQTY * RETLINE.GROS_PRICE)/100 * -1,;
            &lclinesAn..PriceSty   WITH RETLINE.GROS_PRICE
    *B606628,1 Get Style description from Style file. [End]
              
    *B606628,1 Reduce the cost. [Begin]
    REPLACE &lclinesAn..ShpQty   WITH &lclinesAn..ShpQty - RetLine.TOTQTY ,;
            &lclinesAn..ShpCostD WITH &lclinesAn..ShpCostD - (RetLine.TOTQTY * RetLine.COST) ,;
            &lclinesAn..Cost     WITH IIF((&lclinesAn..OpnQty + &lclinesAn..ShpQty) = 0 , 0 ,  (&lclinesAn..OpnCostD + &lclinesAn..ShpCostD) / (&lclinesAn..OpnQty + &lclinesAn..ShpQty))
    *B606628,1 Reduce the cost. [End]

  *B606628,1 Select credit memo with empty invoices. [End]
  
  ENDIF
ENDSCAN

SELECT RETLINE
SET RELATION TO

SELECT (lcAlias)

*--End of lfBokSty.

*!***************************************************************************
*! Name      : lfRetCost
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/12/2002
*! Purpose   : Function used to collect the style cost from RetLine.
*!***************************************************************************
*! Example   : =lfRetCost()
*!***************************************************************************
FUNCTION lfRetCost
PRIVATE lcAlias , lcKeyRet

lcAlias = ALIAS()
lnRetCost = 0
SELECT RetLine
lcKeyRet = EVAL(KEY())
=SEEK(RetHdr.CrMemo ,'RetLine')
SCAN REST WHILE CRMEMO + STYLE + CRET_LINNO + CRET_TRNCD = RetHdr.CrMemo
  IF cRet_TrnCD = '4'
    LOOP
  ENDIF
  lnRetCost = lnRetCost + (RetLine.TotQty * RetLine.Cost)
ENDSCAN

=SEEk(lcKeyRet,'RetLine')
SELECT (lcAlias)
RETURN lnRetCost
*--End of lfRetCost.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/19/2002
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*B606667,1
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnInvHdPos = lfItmPos('INVHDR.INVDATE')   && get Invoice date filter Position.
lnOrdHdPos = lfItmPos('ORDHDR.COMPLETE') && get Complete date filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/19/2002
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*B606667,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/19/2002
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*B606667,1
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laBrFldFlt,laBrTmpFlt)        && Copy Browse Filter Array to Temp Array.

*-- If user selected Account.
IF ALLTRIM(laTempExpr[lnAcctPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnAcctPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),7)
  
  *-- Generate expression for Account.
  lcAcctExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  *lcRpExp = STRTRAN(lcRpExp,lcAcctExp,".T.")
ENDIF  
*-- EndIf of user selected Account.

*-- If user selected Invoice Date.
IF ALLTRIM(laTempExpr[lnInvHdPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnInvHdPos,1),7)
  
  *-- Generate expression for Month Range.  
  lcInvHdExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  *lcRpExp = STRTRAN(lcRpExp,lnInvHdExp,".T.")  
ENDIF  
*-- If user selected Invoice Date.

*-- If user selected Order Complete date.
IF ALLTRIM(laTempExpr[lnOrdHdPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnOrdHdPos,1),7)
  
  *-- Generate expression for Month Range.  
  lcOrdHdExp= gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  *lcRpExp = STRTRAN(lcRpExp,lnOrdHdExp,".T.")  
ENDIF  
*-- If user selected Order Complete date.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.
