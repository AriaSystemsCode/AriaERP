*:**************************************************************************
*: Program file  : SOTRI20
*: Program desc. : Custom Sales Report For TRIO.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/19/2001
*: Reference     : C102374
*:**************************************************************************
*: Procedures    : lpCreatFil, lpGenExpr, lpCollData, lpInsrtLin, lpPrintRep
*:                 lpPrintHdr, lpAccTotal, lpPrintDet,
*: Functions     : lfwRepWhen, lfItmPos, lfsrAcc, lfSRVSty, lfStySum
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

*-- IF filter change collect data again.
IF llOGFltCh
  DO lpCreatFil
  
  STORE '.T.' TO lcDateExp , lcSeasnExp , lcStyleExp , lcStyGrExp
  *-- If user Selected Style or Style Group or Accounts , remove them from lcRpExp 
  IF ALLTRIM(laOGFxFlt[lnStylePos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ;
     ALLTRIM(laOGFxFlt[lnStyGrPos,1]) $ lcRpExp
    DO lpGenExpr           && get the Date expression out from lcRpExp
  ENDIF

  SELECT ConsInvL
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  
  SELECT InvLine
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  
  SELECT OrdLine 
  SET RELATION TO Style INTO Style ADDITIVE

  DO lpCollData            && Collect the data for report
    
  SELECT ConsInvL
  SET RELATION TO

  SELECT InvLine
  SET RELATION TO
    
  SELECT OrdHdr
  SET RELATION TO 
  
  SELECT OrdLine
  SET RELATION TO
ENDIF
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
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT  
  DO lpPrintRep              && Print the @Say data  
  DO ENDREPORT
  SET DEVICE TO SCREEN  
ENDIF
*-- End of Report progarm.  

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcStyMajor
*-- If Temp. Account file empty. (User selected Account)
IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0
  SELECT OrdLine
  SET ORDER TO TAG OrdLinSt
  
  SELECT OrdHdr
  SET RELATION TO cOrdType + Order INTO OrdLine ADDITIVE
    
  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO Account INTO OrdHdr ADDITIVE
  
  SCAN
    SELECT OrdHdr
    SCAN REST WHILE Account = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account') FOR &lcRpExp
      SELECT OrdLine
      SCAN REST WHILE cOrdType + Order + Store + Style + STR(LineNo,6) = OrdHdr.cOrdType + OrdHdr.Order ;
           FOR &lcStyleExp AND &lcStyGrExp
         
        DO lpInsrtLin             && Insert Lines into Temp file.        
      ENDSCAN
    ENDSCAN
  ENDSCAN  
    
  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO
ELSE           && Else user did not select Account.
    
  SELECT OrdLine
  SET ORDER TO TAG OrdLinSt

  SELECT OrdHdr
  SET RELATION TO cOrdType + Order INTO OrdLine ADDITIVE
    
  SCAN FOR &lcRpExp    
    SELECT OrdLine
    SCAN REST WHILE cOrdType + Order + Store + Style + STR(LineNo,6) = OrdHdr.cOrdType + OrdHdr.Order ;
         FOR &lcStyleExp AND &lcStyGrExp
         
      DO lpInsrtLin             && Insert Lines into Temp file.       
    ENDSCAN
  ENDSCAN        
ENDIF
*-- Endif of Temp. Account file empty.
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpInsrtLin
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Insert a line into Temp work File.
*!**************************************************************************
*! Example   : DO lpInsrtLin.
*!**************************************************************************
*
PROCEDURE lpInsrtLin
PRIVATE lcStyMajor , lcAlias
lcAlias = ALIAS()
lcStyMajor = SUBSTR(OrdLine.Style,1,lnMajorLen)

IF !SEEK(OrdHdr.Account + lcStyMajor, lcWorkFile)
  SELECT (lcWorkFile)
  APPEND BLANK
  REPLACE Account WITH OrdLine.Account, Style WITH lcStyMajor, nFrstPric WITH OrdLine.Price
        
  *-- Searching for the style in the Style file and get its description then replace it in the Temp file.
  IF SEEK (lcStyMajor, "Style")
    REPLACE nPriceB WITH Style.PriceB , cStyDesc WITH Style.Desc
  ENDIF
ENDIF

IF SEEK(OrdHdr.Order,'ConsInvL')
  SELECT ConsInvL
  SUM ALL TotQty FOR Order + STR(LineNo,6) + Invoice = OrdHdr.Order+STR(OrdLine.LineNo,6) AND InvHdr.Status <> "V" TO lnTotShp  
ELSE
  SELECT InvLine
  SUM ALL TotQty FOR Order + STR(LineNo,6) + Invoice = OrdHdr.Order+STR(OrdLine.LineNo,6) AND InvHdr.Status <> "V" TO lnTotShp
ENDIF

SELECT (lcWorkFile)
REPLACE Shipped WITH Shipped + lnTotShp , Booked WITH Booked + lnTotShp + OrdLine.TotQty ,;
        P_Diff WITH  nFrstPric - nPriceB , Extn WITH P_Diff * Shipped
        
SELECT (lcAlias)        
*-- End of lpInsrtLin.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
*-- Creating the temporary file that will used report print.
CREATE TABLE (gcWorkDir+lcWorkFile) ;
             (Account C(5), Style C(19), cStyDesc C(20), Booked N(7), Shipped N(7),;
              nFrstPric N(13,2), nPriceB N(13,2), P_Diff N(13,2) , Extn N(13,2))
INDEX ON Account + Style TAG (lcWorkFile)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lpPrintRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : To print the @ Say of collected data.
*!**************************************************************************
*! Calls     : DO lpPrintHdr
*!           : DO lpPrintDet
*!           : DO lpAccTotal
*!**************************************************************************
*! Example   : DO lpPrintRep.
*!**************************************************************************
*
PROCEDURE lpPrintRep
PRIVATE lnSubBook, lnSubShip, lnSubExtn, lnGrndBook, lnGrndShip, lnGrndExtn
STORE 0 TO lnSubBook, lnSubShip, lnSubExtn, lnGrndBook, lnGrndShip, lnGrndExtn

R_WIDTH = 'N'
R_TITLE = 'Custom Sales Report'
STORE 0 TO ROW , PAGENO
DO lpPrintHdr

SELECT (lcWorkFile)
LOCATE
lcAccount = Account
DO WHILE !EOF()
  IF ROW > 51
    DO lpPrintHdr
  ENDIF
  @ ROW,00 SAY Account
  @ ROW,06 SAY IIF(SEEK ('M'+ACCOUNT,"Customer"),Customer.BTName,"") 
  
  ROW = ROW + 1
  SCAN WHILE Account = lcAccount
    IF ROW > 51
      DO lpPrintHdr  
    ENDIF
    
    @ ROW,11 SAY SUBSTR(Style,1,lnMajorLen)
    @ ROW,24 SAY cStyDesc

    *-- This Proc is used to print the booked, shipped, p_Diff, and Extn 
    DO lpPrintDet WITH Booked, Shipped, P_Diff, Extn, ""

    *-- this Proc. is responsible for accumolating the sub-total and the 
    *-- the grand total for the booked, shipped, and extention fields
    DO lpAccTotal
  ENDSCAN

  *-- This Proc is used to print the sub-total of the same fields
  DO lpPrintDet WITH lnSubBook, lnSubShip, 0, lnSubExtn, "-"

  STORE 0 TO lnSubBook, lnSubShip, lnSubExtn
  lcAccount = Account
ENDDO  

*-- This Proc is used to print the grand-total of the same fields
DO lpPrintDet WITH lnGrndBook, lnGrndShip, 0, lnGrndExtn, "*"
*-- End of lpPrintRep.

*!**************************************************************************
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Date      : 11/17/1996
*! Purpose   : To print the header of the report.
*!**************************************************************************
*! Example   :  DO lpPrintHdr
*!**************************************************************************
*
PROCEDURE lpPrintHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'SOTRI20','',R_WIDTH

*-- We can also use the lcStyMajor variable in SyRepuvr instead of printing 
*-- The word 'Style#' hardcoded
@ 5,11 SAY 'Style#       Style Description    Booked   Ship  P. Diff.  Extension'
@ 6,00 SAY REPLICATE('*',80)
ROW = 7
*-- End of lpPrintHdr.

*!**************************************************************************
*! Name      : lpAccTotal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : To accumlate the booked, shipped, and extention fields
*!**************************************************************************
*! Example   :  DO lpAccTotal
*!**************************************************************************
*
PROCEDURE lpAccTotal

*-- This proc will accumolating these fields : booked, shipped, and 
*-- Extension
lnSubBook = lnSubBook + Booked
lnSubShip = lnSubShip + Shipped
lnSubExtn = lnSubExtn + Extn
lnGrndBook = lnGrndBook + Booked
lnGrndShip = lnGrndShip + Shipped
lnGrndExtn = lnGrndExtn + Extn
*-- End of lpAccTotal.

*!**************************************************************************
*! Name      : lpPrintDet
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : To accumolate the booked, shipped, and extention fields
*!**************************************************************************
*! Passed Parameters  : lnCol1, lnCol2, lnCol3, lnCol4, lcType
*!**************************************************************************
*! Layout             :
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
*!           STYLE#       STYLE DESCRIPTION    BOOKED   SHIP   P. DIFF.  EXTENSION
*!같같� 같같같같같같같같같같같같같같같                                            
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
*!           같같같같같같 같같같같같같같같같같 999999 999999 9999999.99 9999999.99
*!           같같같같같같 같같같같같같같같같같 999999 999999 9999999.99 9999999.99
*!           같같같같같같 같같같같같같같같같같 999999 999999 9999999.99 9999999.99
*!           같같같같같같 같같같같같같같같같같 999999 999999 9999999.99 9999999.99
*!           같같같같같같 같같같같같같같같같같 999999 999999 9999999.99 9999999.99
*!**************************************************************************
*! Example  :  DO lpPrintDet WITH lnSubBook, lnSubShip, 0, lnSubExtn, "-"
*!**************************************************************************
*
PROCEDURE lpPrintDet
PARAMETERS lnCol1, lnCol2, lnCol3, lnCol4, lcType

*-- This to draw "-" for subtotals, and "*" for the grandtotals
IF INLIST(lcType,"-","*")
  @ ROW,00 SAY REPLICATE(lcType,80)
  ROW = ROW + 1
  @ ROW,00 SAY IIF(lcType = "*", 'Grand Total', 'Subtotal') 
ENDIF

@ ROW,45 SAY lnCol1 PICTURE '999999'
@ ROW,52 SAY lnCol2 PICTURE '999999'
IF EMPTY(lcType)
  @ ROW,59 SAY lnCol3 PICTURE '9999999.99'
ENDIF  
@ ROW,70 SAY lnCol4 PICTURE '9999999.99'
ROW = ROW + 1

IF !EMPTY(lcType) AND !(lcType = "-" AND EOF())
  @ ROW,00 SAY REPLICATE(lcType,80)
  ROW = ROW + 1
ENDIF
*-- End of lpPrintDet.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Report When Func.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('ORDHDR.ACCOUNT')  && get Account Fixed filter Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR') && get Style Fixed filter Position.
lnStyGrPos = lfItmPos('STYLE.CSTYGROUP') && get Style Group Fixed filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
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
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
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
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
*
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT Style
    SET ORDER TO TAG Cstyle
    SET RELATION TO Style.Style INTO STYLE_X
    GO TOP IN Style
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT Style
    SET ORDER TO TAG Style
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
*
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
*-- End of lfStySum.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/19/2001
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laBrFldFlt,laBrTmpFlt)        && Copy Browse Filter Array to Temp Array.

*-- If user selected Style.
IF ALLTRIM(laTempExpr[lnStylePos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnStylePos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Sales Rep expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
  *-- Generate expression for Sales Rep.
  lcStyleExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcStyleExp,".T.")
  
  *style+DTOS(complete)+cordtype+order+store+STR(lineno,6)
  *lcStyleExp = STRTRAN(lcStyleExp,"STYLE.CSTYMAJOR",;
  *             "STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)")
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

  *account+cordtype+order
  *lcAcctExp = STRTRAN(lcAcctExp,"ORDHDR.ACCOUNT",;
  *             "ACCOUNT+CORDTYPE+ORDER")
ENDIF  
*-- EndIf of user selected Account.

*-- If user selected Style Group.
IF ALLTRIM(laTempExpr[lnStyGrPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only division expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStyGrPos,1),7)
  
  *-- Generate expression for Division.
  lcStyGrExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Division from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcStyGrExp,".T.")  
ENDIF  
*-- EndIf of user selected Style Group.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.
