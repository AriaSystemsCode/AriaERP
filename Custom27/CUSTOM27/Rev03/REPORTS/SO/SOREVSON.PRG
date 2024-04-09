*:***************************************************************************
*: Program file  : SOREVSON
*: Program desc. : Season to Season Customer Analysis for REV10
*: For Report    : SOVENSON.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry (MHM)
*: Date          : 10/25/2001
*: Reference     : C102479
*:**************************************************************************
*: Procedures    : lpCreatFil , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: B606861,1 ASH 01/12/03 Fix some problems in the report.
*:**************************************************************************

SELECT Codes
SET ORDER TO TAG cCode_No
IF !SEEK("NSEASON")
  *-- Message <This company has no Season codes, Cannot proceed.>
  *-- Buttons <                             OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no  Season codes, Cannot proceed.")
  RETURN  
ENDIF

DO lpCreatFil
  
SELECT OrdHdr
SET RELATION TO "M" + Account INTO Customer ADDITIVE

*--102479,5  
SELECT InvHdr
SET RELATION TO "M" + Account INTO Customer ADDITIVE
*--102479,5  

DO lpCollData            && Collect the data for report.
  
SELECT InvLine
SET RELATION TO
  

SELECT OrdHdr
SET RELATION TO
*-- Endif of Filter change.  

SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
IF llRpRecap
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcRpRecap,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
  SELECT (lcWorkFile)
  SET ORDER TO TAG lcWorkFil
  LOCATE
  
  DO gfDispRe WITH EVAL('lcRpRecap')
EndIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/25/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     
lcRpExp = STRTRAN(lcRpExp,'ORDHDR.SEASON','SEASON')
*-- If Temp. Account file is used.
IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0

  SELECT OrdLine 
  SET ORDER TO TAG OrdLine
  
  SELECT InvLine 
  SET ORDER TO TAG InvLine
  *B606861,1 ASH 01/12/03 (Begin) Remove this relation and use the seek and scan instead.
  *SELECT InvHdr
  *SET RELATION TO Invoice INTO InvLine ADDITIVE
  *B606861,1 ASH 01/12/03 (End)
  SELECT OrdHdr 
  SET RELATION TO cOrdType + Order INTO OrdLine ADDITIVE
  
  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO Account INTO OrdHdr ADDITIVE

  *--102479,5
  *SET RELATION TO Account INTO InvHdr ADDITIVE
  *--102479,5
  
  SCAN
    *--for Open Qty (from ordhdr file)
    SELECT OrdHdr
    SCAN REST WHILE Account+cOrdType+Order = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
      *--102479,5  
      IF  OrdHdr.Status <> "X" 
      *--102479,5  
        SELECT OrdLine
        SCAN REST WHILE cOrdType + Order = OrdHdr.cOrdType + OrdHdr.Order ;
                  FOR &lcRpExp 
          WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT

          *--102479,5  
          *DO lpInsrtRec WITH 'O',Ordline.Account,
          DO lpInsrtRec WITH 'O',Ordline.Account,Ordline.Season
          *--102479,5  

        ENDSCAN
      *--102479,5  
      ENDIF  
      *--102479,5  
    ENDSCAN
      
    *--for Open Qty (from InvHdr file)
    SELECT InvHdr

    *--102479,5
    =SEEK( &laOGFxFlt[lnAcctPos,6]..Account)
    *--102479,5
    *B606861,1 ASH 01/12/03 (Begin) Use the season from invline.
    lcRpExp = STRTRAN(lcRpExp,'ORDHDR.','')
    *B606861,1 ASH 01/12/03 (End)
    SCAN REST WHILE Account+Invoice = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
      IF  InvHdr.Status <> "V" 
        SELECT InvLine
        *B606861,1 ASH 01/12/03 (Begin) Use the new expression (lcRpExp1)
        *SCAN REST WHILE Invoice = InvHdr.Invoice ;
                  FOR &lcRpExp
        =SEEK(InvHdr.Invoice)          
        SCAN REST WHILE Invoice = InvHdr.Invoice 
          IF &lcRpExp
        *B606861,1 ASH 01/12/03 (End)
            WAIT WINDOW "Collecting data for Invoice# " + Invline.Invoice NOWAIT

            *--102479,5
            *DO lpInsrtRec WITH 'I',InvLine.Account
            DO lpInsrtRec WITH 'I',InvLine.Account,InvLine.Season
            *--102479,5
          ENDIF  
        ENDSCAN
      ENDIF  
    ENDSCAN

  ENDSCAN  

  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO
ELSE           && Else user did not select Account.
  *-- For orders ( Open Qty )
  SELECT OrdLine 
  SET ORDER TO TAG OrdLines
  SET RELATION TO Account + cOrdType + Order INTO OrdHdr ADDITIVE

  SCAN FOR &lcRpExp. AND cOrdType = 'O'

    *--102479,5
    *DO lpInsrtRec WITH'O',Ordline.Account,Ordline.Season
    *B606861,1 ASH 01/12/03 (Begin) Remove this condition to get the cancelled qty.
    *IF  OrdHdr.Status <> "X" 
    *B606861,1 ASH 01/12/03 (End)
      DO lpInsrtRec WITH'O',Ordline.Account,Ordline.Season
    *ENDIF  
    *--102479,5

  ENDSCAN  

  *-- For invoice (shiped Qty)
  SELECT InvLine 
  SET ORDER TO TAG InvLines
  SET RELATION TO Account +Invoice INTO InvHdr ADDITIVE
  SCAN FOR &lcRpExp. 
    *B606861,1 ASH 01/12/03 (Begin) There is no status 'X' in invhdr.
    *IF  InvHdr.Status <> "X" 
    IF  InvHdr.Status <> "V" 
    *B606861,1 ASH 01/12/03 (End)    
      DO lpInsrtRec WITH 'I',InvLine.Account,InvLine.SEASON
    EndIF
  ENDSCAN  
ENDIF

*-- End of lpCollData.
*!**************************************************************************
*! Name      : lpInsrtRec
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : To insert a new record or update an old one in temp file.
*!**************************************************************************
*! Example   : DO lpInsrtRec
*!**************************************************************************
*
PROCEDURE lpInsrtRec

*--102479,5
*PARAMETER lcFrmInOrd , lcAccount
PARAMETER lcFrmInOrd , lcAccount,lcSeason
*--102479,5

PRIVATE llUpdate
STORE .F. TO llUpdate

*--Intialize variables for quantity
IF lcFrmInOrd <> 'O'
  STORE 0 TO m.nShip ,m.nShipUnit
ELSE  
  STORE 0 TO m.nOpen ,m.nBook , m.nCancel , m.nOpenUnit , m.nBookUnit , m.nCanclUnit 
ENDIF  

DO CASE
  CASE lcFrmInOrd   <> 'O'
      *B606861,1 ASH 01/12/03 (Begin) Use the right variables.
      *m.nShip1      = INVLINE.TotQty * INVLINE.price
      *m.nShipUnit1  = INVLINE.TotQty 
      m.nShip      = INVLINE.TotQty * INVLINE.price
      m.nShipUnit  = INVLINE.TotQty 
      *B606861,1 ASH 01/12/03 (End)
      llUpdate      = .T.
      
  CASE lcFrmInOrd   = 'O'
      *B606861,1 ASH 01/12/03 (Begin) Compute the open orders for only open and hold orders.
      IF OrdHdr.Status $ 'OH'
      *B606861,1 ASH 01/12/03 (End)
        m.nOpen       = OrdLine.TotQty * OrdLine.price
        m.nOpenUnit   = OrdLine.TotQty 
      ENDIF  
      m.nBook       = OrdLine.TotBook * OrdLine.price
      m.nBookUnit   = OrdLine.TotBook 
      
      m.nCancel     = OrdHdr.cancelamt
      m.nCanclUnit = OrdHdr.cancel

      llUpdate      = .T.
ENDCASE

*--Update work temp file

*--102479,5
*IF SEEK(lcAccount,(lcWorkFile)) AND llUpdate 
IF SEEK(lcAccount+lcSeason,(lcWorkFile)) AND llUpdate 
*--102479,5

  SELECT (lcWorkFile)
  IF lcFrmInOrd <> 'O'
  
    REPLACE nShip       WITH nShip+m.nShip,;
            nShipUnit   WITH nShipUnit +m.nShipUnit
  ELSE
    *B606861,1 ASH 01/12/03 (Begin) Don't accumilate the cancel qty for eash line as we compute it from ordhdr file.
    *REPLACE nOpen     WITH nOpen+m.nOpen,;
            nBook       WITH nBook+m.nBook,;
            nCancel     WITH nCancel+m.nCancel,;
            nOpenUnit WITH nOpenUnit+m.nOpenUnit,;
            nBookUnit   WITH nBookUnit +m.nBookUnit,;
            nCanclUnit WITH nCanclUnit +m.nCanclUnit
    REPLACE nOpen     WITH nOpen+m.nOpen,;
            nBook       WITH nBook+m.nBook,;
            nOpenUnit WITH nOpenUnit+m.nOpenUnit,;
            nBookUnit   WITH nBookUnit +m.nBookUnit
    *B606861,1 ASH 01/12/03 (End)
  ENDIF          
ELSE
  IF llUpdate 
    m.Account = lcAccount
    =SEEK("M"+lcAccount,"CUSTOMER")
    m.Acc_name   = CUSTOMER.BtName
    m.cSeason    = Season
    m.cSeasonDes =gfCodDes(Season,'SEASON')
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF  
ENDIF
*-- End of lpInsrtRec.

*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnSeasnPos = lfItmPos('ORDHDR.SEASON')    && get Class filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : To get the position of the fixed filter in OG.
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
*! Name      : lfsrAcc
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lpCreatFil
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
*PRIVATE lcPerFrom , lcPerTo , lcLoop

*-- Create a temporary table to hold data which will be displayed in report.

CREATE TABLE (gcWorkDir+lcWorkFile) (Account C(5),cSeason C(6),cSeasonDes C(30),Acc_name C(30),nShip N(9,2), nShipUnit N(5),;
nOpen N(9,2), nOpenUnit N(5), nBook N(9,2),nBookUnit N(5),nCancel N(9,2), nCanclUnit N(5))

INDEX ON cSeason TAG (lcWorkFil) OF (gcWorkDir+lcWorkFile)

*--102479,5
*INDEX ON Account  TAG (lcWorkFile)
INDEX ON Account + cSeason TAG (lcWorkFile)
*--102479,5

SET ORDER TO TAG lcWorkFile
*-- End of lpCreatFil.

