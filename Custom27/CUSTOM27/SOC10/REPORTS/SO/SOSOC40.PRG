*:**************************************************************************
*: Program file  : SOSOC40
*: Program desc. : Print Order Form for (Soccer Select).
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102132
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExp , lpCollData
*:
*: Functions     : lfwRepWhen , lfItmPos , lfEvalSegs , lfSROrder , 
*:                 lfSRRepCod , lfSRAcct , lfPrnHd
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
* B606908,1 ASH 01/23/2003 Fix bug of printing line for each color.
*:**************************************************************************

*-- If filter is changed collect data again
IF llOGFltCh
  *-- Create all the Temp files needed.
  DO lpCreatFil

  *-- If user Selected SalesRep or Orders or Accounts , remove them from lcRpExp 
  IF ALLTRIM(laOGFxFlt[lnSlRepPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ;
    ALLTRIM(laOGFxFlt[lnOrderPos,1]) $ lcRpExp
    DO lpGenExp    
  ENDIF

  *-- Add the Multi Store option to lcRpExp
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [OrdHdr.Multi=lcRpMulti]
  
  *-- Add the ReOrder option to lcRpExp
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [OrdHdr.cReOrder=lcRpReOrd]

  *-- Add the Bulk option to lcRpExp
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [OrdHdr.Bulk=lcRpBulk]

  DO lpCollData          && Procedure to collect data
  WAIT CLEAR

ENDIF

SELECT (lcTmpLoop)
SET RELATION TO

SELECT (lcOrdHTemp)
SET RELATION TO
LOCATE

llPrntHd = .F.
SELECT (lcOrdHTemp)
SET RELATION TO "" INTO (lcTmpLoop) ADDITIVE
SET RELATION TO 'M' + Account INTO Customer  ADDITIVE
SET RELATION TO cOrdType + Order INTO Soccer ADDITIVE
SET RELATION TO 'O' + Order INTO (lcSocTemp1) ADDITIVE
SET RELATION TO 'O' + Order INTO (lcLogoBmp) ADDITIVE
SELECT (lcTmpLoop)
SET RELATION TO 'O' + &lcOrdHTemp..Order INTO (lcSocTemp) ADDITIVE
SELECT (lcOrdHTemp)
SET SKIP TO (lcTmpLoop),(lcSocTemp)
lnPageNo = 0
DO gfDispRe WITH EVAL('lcRpName')

*-- EndIf of filter is changed

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

DIMENSION laSoldTo [5,1]
laSoldTo = ''

lnSlRepPos = lfItmPos('ORDHDR.REP1')          && Sales Rep Position
lnAcctPos  = lfItmPos('ORDHDR.ACCOUNT')       && Account Position
lnOrderPos = lfItmPos('ORDHDR.ORDER')         && Order Position
lnRegionPo = lfItmPos('CUSTOMER.REGION')      && Order Position
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
SELECT OrdHdr
=AFIELDS(laFileStru)
CREATE CURSOR (lcOrdHTemp) FROM ARRAY laFileStru
INDEX ON cOrdType + Order TAG (lcOrdHTemp)
INDEX ON Account + cOrdType + Order TAG (lcTmpAcCdx)

CREATE CURSOR (lcLogoBmp) (cOrdType C(1) , Order C(6) , gPic G)
INDEX ON cOrdType + Order TAG (lcLogoBmp)

CREATE CURSOR (lcTmpLoop) (cLoop C(1))
ZAP
INDEX ON cLoop TAG (lcTmpLoop)
INSERT INTO (lcTmpLoop) (cLoop) VALUES ("A")
INSERT INTO (lcTmpLoop) (cLoop) VALUES ("B")

CREATE CURSOR (lcSocTemp) (cOrdType C(1) , Order C(6) , Div C(4) , Style C(19) , ;
                           NoOfTeams N(2) , Youth N(7) , Adult N(7) , ;
                           YthPrice N(13,2) , AdlPrice N(13,2) , TotPrice N(13,2))
INDEX ON cOrdType + Order + Div + Style TAG (lcSocTemp) 
SET ORDER TO TAG (lcSocTemp)

CREATE CURSOR (lcSocTemp1) (cOrdType C(1) , Order C(6), Total N(7) , ;
               TotPrice N(13,2) , Taxes N(9,2))
INDEX ON cOrdType + Order TAG (lcSocTemp1) 
SET ORDER TO TAG (lcSocTemp1)

*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = 1 TO ALEN(laMajSeg,1)
  DO CASE
    CASE laMajSeg[lnI,1] = "F"            && Free
      lcMajTtl   = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnStyleLen = LEN(laMajSeg[lnI,3])
      lnStyStPos = laMajSeg[lnI,4]
    CASE laMajSeg[lnI,1] = "C"            && Color 
      lnClrPo    = laMajSeg[lnI,4]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(laMajSeg[lnI,3])
  ENDCASE
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfSROrder
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Rise change order flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!**************************************************************************
*
FUNCTION lfSROrder
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    SET ORDER TO TAG OrdHdr
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER
    LOCATE
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- End of lfsChOrder.

*!***************************************************************************
*! Name      : lfSRRepCod
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Control the InList for Sales Rep file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRRepCod()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRRepCod
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT SalesRep
  LOCATE
ENDIF    
*-- End of lfSRRepCod.

*!***************************************************************************
*! Name      : lfSRAcct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Control the InList for Account file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRAcct()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRAcct
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  LOCATE
ENDIF    
*-- End of lfSRAcct.

*!**************************************************************************
*! Name      : lpGenExp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Generate Division Expression.
*!**************************************************************************
*! Example   : DO lpGenExp
*!**************************************************************************
*
PROCEDURE lpGenExp
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laBrFldFlt,laBrTmpFlt)        && Copy Browse Filter Array to Temp Array.

*-- If user selected Sales Rep.
IF ALLTRIM(laTempExpr[lnSlRepPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnSlRepPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Sales Rep expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnSlRepPos,1),7)
  
  *-- Generate expression for Sales Rep.
  PRIVATE lcSalesExp
  lcSalesExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert (OrdHdr.Rep1) in filter Expression if found
  *-- to be (OrdHdr.Rep1 OR OrdHdr.Rep2)  
  lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,"(" + lcSlRepExp + [ .OR. ] + ;
            STRTRAN(lcSlRepExp,".REP1",".REP2") + ")")     
ENDIF  
*-- EndIf of user selected Sales Rep.

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
  PRIVATE lcAcctExp
  lcAcctExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcAcctExp,".T.")
ENDIF  
*-- EndIf of user selected Account.

*-- If user selected Order.
IF ALLTRIM(laTempExpr[lnOrderPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnOrderPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Order expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnOrderPos,1),7)
  
  *-- Generate expression for Order.
  PRIVATE lcOrderExp
  lcOrderExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Order in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcOrderExp,".T.")
ENDIF  
*-- EndIf of user selected Orders.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExp.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Collect data and insert it into Work file.
*!**************************************************************************
*! Example   : DO lpCollData.
*!**************************************************************************
*
PROCEDURE lpCollData

PRIVATE lcAlias , lcOrdLnKey , lnOther , lcDiv , lcTeam , lnTotP , ;
        lcOrdLnKey , lnYthPrice , lnAdlPrice , llYouth , llApp
        
lcAlias = ALIAS()

SELECT OrdHdr
*-- If user entered Region 
IF !EMPTY(laOGFxFlt[lnRegionPo,6])
  SET RELATION TO 'M'+Account + IIF(EMPTY(Store),'',Store) INTO Customer ADDITIVE
ENDIF

*-- if user selected some orders from Orders InList
IF USED(laOGFxFlt[lnOrderPos,6]) AND RECCOUNT(laOGFxFlt[lnOrderPos,6])>0
  PRIVATE lcOrder
  
  SET ORDER TO TAG OrdHdr
  SELECT (laOGFxFlt[lnOrderPos,6])    
  SET RELATION TO 'O' + Order INTO OrdHdr  

  *-- if user selected some Accounts from Customer InList
  IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0

    *-- Scan loop around selected Orders.
    SCAN      
      *-- If this Order account has been selected.
      IF SEEK(OrdHdr.Account,(laOGFxFlt[lnAcctPos,6]))
        
        lcOrder = EVALUATE(laOGFxFlt[lnOrderPos,6]+'.Order')
        SELECT OrdHdr
        *-- Scan loop around Order Header for lcRpExp.
        SCAN REST WHILE cOrdType + Order = 'O' + lcOrder FOR &lcRpExp
          WAIT WINDOW "Collecting Data for Order# " + Order NOWAIT
          SCATTER MEMVAR MEMO
          INSERT INTO (lcOrdHTemp) FROM MEMVAR
        ENDSCAN
      ENDIF        
      *-- EndIf of this Order account has been selected.
    ENDSCAN
    *-- EndScan loop around Order Header for lcRpExp.
    
  ELSE          && User selected only Orders
    *-- Scan loop around selected Orders.
    SCAN
      lcOrder = EVALUATE(laOGFxFlt[lnOrderPos,6]+'.Order')
      SELECT OrdHdr
      *-- Scan loop around Order Header for lcRpExp.
      SCAN REST WHILE cOrdType + Order = 'O' + lcOrder FOR &lcRpExp
        WAIT WINDOW "Collecting Data for Order# " + Order NOWAIT
        SCATTER MEMVAR MEMO
        INSERT INTO (lcOrdHTemp) FROM MEMVAR
      ENDSCAN
    ENDSCAN
    *-- EndScan loop around Order Header for lcRpExp.
    
  ENDIF
  *-- Endif of user selected some Accounts from Customer InList

  SELECT (laOGFxFlt[lnOrderPos,6])    
  SET RELATION TO    
            
ELSE       && Else user selected some Orders from Orders InList    

  *-- if user selected some Accounts from Customer InList
  IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0
    SELECT (lcOrdHTemp)
    SET ORDER TO TAG (lcTmpAcCdx)

    SELECT OrdHdr     
    SET ORDER TO TAG OrdAcct
    
    SELECT (laOGFxFlt[lnAcctPos,6])
    SET RELATION TO Account + 'O' INTO OrdHdr

    *-- Scan loop around selected Orders.
    SCAN        
      *-- Scan loop around Order Header for lcRpExp.
      SELECT OrdHdr
      SCAN REST WHILE Account + cOrdType + Order = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account') + 'O' FOR &lcRpExp
      WAIT WINDOW "Collecting Data for Order# " + Order NOWAIT
      SCATTER MEMVAR MEMO
        INSERT INTO (lcOrdHTemp) FROM MEMVAR
      ENDSCAN
    ENDSCAN
    *-- EndScan loop around Order Header for lcRpExp.
    
    SELECT (laOGFxFlt[lnAcctPos,6])
    SET RELATION TO
  ELSE
    SELECT OrdHdr
    *-- Scan loop around the Order Header Master File.
    SCAN FOR &lcRpExp
      WAIT WINDOW "Collecting Data for Order# " + Order NOWAIT
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdHTemp) FROM MEMVAR      
    ENDSCAN
  ENDIF
  *-- Endif of user selected some Accounts from Customer InList
    
ENDIF
*-- EndIf of user selected some orders from Orders InList

SELECT (lcOrdHTemp)
SET RELATION TO
SET RELATION TO 'M' + Account INTO Customer  ADDITIVE
SET RELATION TO cOrdType + Order INTO Soccer ADDITIVE

SELECT OrdLine
SET RELATION TO Style INTO Style ADDITIVE
SET RELATION TO cOrdType + Order + Store INTO Soccer ADDITIVE

USE (gcDataDir+'OrdLine') AGAIN ALIAS OrdLine_A IN 0 ORDER TAG Ordlinst
SELECT OrdLine_A 
SET RELATION TO Style INTO Style ADDITIVE

SELECT (lcOrdHTemp)
SCAN 
  STORE ' ' TO lcDiv , lcTeam
  STORE 0 TO lnOther , lnTotP
  
  IF SEEK(cOrdType + Order,'OrdLine')
    IF !EMPTY(ALLTRIM(Soccer.Logo))
      SELECT (lcLogoBmp)
      APPEND BLANK
      REPLACE cOrdType WITH 'O' , Order WITH &lcOrdHTemp..Order
      APPEND GENERAL gPic FROM ALLTRIM(Soccer.Logo) LINK
    ENDIF
    SELECT OrdLine
    STORE ' ' TO lcOrdLnKey
    SCAN WHILE cOrdType + Order + STR(LineNo,6) = 'O' + &lcOrdHTemp..Order
      IF Style.cStyGroup = 'JY'
        IF lcOrdLnKey <> cOrdType + Order + Store + Style
          lcOrdLnKey = cOrdType + Order + Store + Style
        ELSE
          SELECT (lcSocTemp)
          REPLACE AdlPrice WITH OrdLine.Price ,;
                  TotPrice WITH Youth * YthPrice + Adult * AdlPrice
          LOOP        
        ENDIF
        
        STORE .T. TO llYouth , llApp
        STORE 0 TO lnYthPrice , lnAdlPrice
        *B606908,1 ASH 01/23/2003 (Begin) Fix bug of printing line for each color.        
        *IF SEEK(OrdLine.cOrdType + OrdLine.Order + SUBSTR(OrdLine.Store,1,4) + OrdLine.Style,lcSocTemp) 
        IF SEEK(OrdLine.cOrdType + OrdLine.Order + SUBSTR(OrdLine.Store,1,4) + SUBSTR(OrdLine.Style,1,12),lcSocTemp) 
        *B606908,1 ASH 01/23/2003 (End)
          IF SEEK(OrdLine.cOrdType + OrdLine.Order + OrdLine.Store + OrdLine.Style,'OrdLine_A') 
            SELECT OrdLine_A
            SCAN WHILE cOrdType + Order + Store + Style + STR(LineNo,6)= OrdLine.cOrdType + OrdLine.Order + SUBSTR(OrdLine.Store,1,4)
              IF Style = OrdLine.Style AND Style.cStyGroup = 'JY'
                IF llYouth
                  lnYthPrice = Price
                  llYouth = .F.
                ELSE
                  lnAdlPrice = Price
                  EXIT
                ENDIF
              ENDIF
            ENDSCAN
          ENDIF
          IF &lcSocTemp..YthPrice = lnYthPrice  AND &lcSocTemp..AdlPrice = lnAdlPrice 
            SELECT (lcSocTemp)
            REPLACE Youth     WITH Youth + Soccer.YouthPkg  ,;
                    Adult     WITH Adult + Soccer.AdultPkg  ,;
                    NoOfTeams WITH NoOfTeams + 1
            llApp = .F.                  
          ELSE
            llApp = .T.          
          ENDIF
          IF lcTeam <> SUBSTR(OrdLine.Store,5,4)
            lcTeam = SUBSTR(OrdLine.Store,5,4)
          ENDIF
        ENDIF
        IF llApp
          lcTeam = ' ' 
          SELECT (lcSocTemp)
          APPEND BLANK
          REPLACE cOrdType  WITH 'O' ,;
                  Order     WITH &lcOrdHTemp..Order ,;
                  Div       WITH Soccer.Div       ,;
                  Style     WITH OrdLine.Style    ,;
                  NoOfTeams WITH 1                ,;
                  Youth     WITH Soccer.YouthPkg  ,;
                  Adult     WITH Soccer.AdultPkg  ,;
                  YthPrice  WITH OrdLine.Price    ,;
                  TotPrice  WITH Youth*YthPrice + Adult*AdlPrice
        ENDIF
      ELSE
        *B606908,1 ASH 01/23/2003 (Begin) The codes in 2.7 is 6 chrs not 2.
        *IF !(STYLE.cStyGroup $ 'SOSH')
        IF STYLE.cStyGroup <> 'SO' AND STYLE.cStyGroup <> 'SH' 
        *B606908,1 ASH 01/23/2003 (End)
          lnOther = lnOther + TotQty
          lnTotP  = lnTotP  + TotQty*Price
        ENDIF
      ENDIF
    ENDSCAN  
    SELECT (lcSocTemp1)
    APPEND BLANK
    REPLACE cOrdType WITH 'O' ,;
            Order    WITH &lcOrdHTemp..Order ,;
            Total    WITH lnOther          ,;
            TotPrice WITH lnTotP           ,;
            Taxes    WITH Customer.ntaxrate
  ENDIF        
          
ENDSCAN

SELECT OrdLine
SET RELATION TO 

SELECT OrdLine_A
SET RELATION TO
USE

SELECT (lcAlias)    && Restore Old Alias.
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lfPrnHd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/01/2001
*! Purpose   : Function called from FRX to detect Page No.
*!**************************************************************************
*! Example   : =lfPrnHd()
*!**************************************************************************
*
FUNCTION lfPrnHd
PARAMETERS lcPrn
llPrntHd = lcPrn
IF !llPrntHd 
  lnPageNo = 1
ELSE
  lnPageNo = lnPageNo + 1
ENDIF
*-- End of lfPrnHd.