*:**************************************************************************
*: Program file  : SOREV10
*: Program desc. : Custom Bookings Analysis Report for Revue.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 09/25/2001
*: Reference     : C102412
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtLin
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
* Modifications:
*: B605628,1 ADEL 03/10/2002 Fix bug of not filtering data by season when
*: B605628,1 ADEL            selecting styles besides seasons.
*: B605961,1 ALB  09/15/2002 Fix the thru % and MU%
*:**************************************************************************

SELECT Codes
SET ORDER TO TAG cCode_No
IF !SEEK("NCLASS")
  *-- Message <This company has no customer class codes, Cannot proceed.>
  *-- Buttons <                             OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no customer class codes, Cannot proceed.")
  RETURN  
ENDIF

*-- IF filter change collect data again.
IF llOGFltCh
  PRIVATE lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp , lcKeyValue
  lcKeyValue = ''
  STORE '.T.' TO lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp
  
  DO lpCreatFil
  
  *-- If user Selected Account, remove them from lcRpExp 
  IF ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnStylePos,1]) $ lcRpExp OR ;
     ALLTRIM(laOGFxFlt[lnDivsnPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnClassPos,1]) $ lcRpExp OR ;
     EMPTY(laOGFxFlt[lnClassPos,6])
    DO lpGenExpr           && get the Date expression out from lcRpExp
  ENDIF
        
  SELECT InvLine
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  
  SELECT PosLn
  SET RELATION TO "P" + PO INTO PosHdr ADDITIVE
  
  SELECT CutTktl
  SET RELATION TO CutTkt INTO CutTktH ADDITIVE
  
  SELECT OrdHdr
  SET RELATION TO "M" + Account INTO Customer ADDITIVE
  
  SELECT OrdLine
  SET RELATION TO Style INTO Style ADDITIVE
  
  DO lpCollData            && Collect the data for report.
  
  SELECT InvLine
  SET RELATION TO
  
  SELECT PosLn
  SET RELATION TO
  
  SELECT CutTktl
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
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     

*-- If Temp. Account file is used.
IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6])>0

  SELECT OrdLine 
  SET ORDER TO TAG OrdLine
  
  SELECT OrdHdr
  SET RELATION TO cOrdType + Order INTO OrdLine ADDITIVE
  
  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO Account INTO OrdHdr ADDITIVE

  SCAN
    IF &lcClassExp.
      SELECT OrdHdr
      SCAN REST WHILE Account+cOrdType+Order = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
        IF OrdHdr.Status $ 'COH' AND &lcDivsnExp. && new lines
          SELECT OrdLine
          SCAN REST WHILE cOrdType + Order = OrdHdr.cOrdType + OrdHdr.Order ;
                    FOR &lcRpExp AND &lcStyleExp
            WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
            DO lpInsrtRec
          ENDSCAN
        ENDIF  
      ENDSCAN
    ENDIF  
  ENDSCAN  

  SELECT(laOGFxFlt[lnAcctPos,6])
  SET RELATION TO
ELSE           && Else user did not select Account.
  PRIVATE lcStyMajor
  SELECT OrdLine 
  SET ORDER TO TAG OrdLines
  SET RELATION TO Account + cOrdType + Order INTO OrdHdr ADDITIVE

  *-- If user selected style.
  IF USED(laOGFxFlt[lnStylePos,6]) AND RECCOUNT(laOGFxFlt[lnStylePos,6])>0    
    SELECT(laOGFxFlt[lnStylePos,6])
    SET RELATION TO SUBSTR(cStyMajor,1,lnMajorLen) INTO OrdLine ADDITIVE

    SCAN
      SELECT OrdLine
      *B605628,1 (Begin) Add lcRpExp to the filter as it contains Season filter.
      *SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = SUBSTR(EVALUATE(laOGFxFlt[lnStylePos,6]+'.cStyMajor'),1,lnMajorLen) ;
                FOR &lcClassExp. AND &lcDivsnExp.
      SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = SUBSTR(EVALUATE(laOGFxFlt[lnStylePos,6]+'.cStyMajor'),1,lnMajorLen) ;
                FOR &lcClassExp. AND &lcDivsnExp. AND &lcRpExp
      *B605628,1 (End)
        IF OrdHdr.Status $ 'COH'
          DO lpInsrtRec
        ENDIF                  
      ENDSCAN
    ENDSCAN
    
    SELECT(laOGFxFlt[lnStylePos,6])
    SET RELATION TO    
  ELSE   && else user didn't select style
    SCAN FOR &lcRpExp. AND &lcAcctExp. AND &lcClassExp. AND &lcDivsnExp.
      IF OrdHdr.Status $ 'COH'
        DO lpInsrtRec
      ENDIF  
    ENDSCAN  
  ENDIF
ENDIF
*-- Endif of Temp. Account file empty.

SELECT (lcWorkFile)
LOCATE
IF EOF()
  RETURN
ELSE
  DO lpContColl
ENDIF
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpContColl
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To continue collecting data for the report.
*!**************************************************************************
*! Example   : DO lpContColl
*!**************************************************************************
*
PROCEDURE lpContColl
PRIVATE lcDivision
lcDivision = ".T."
SELECT (lcWorkFile)
SCAN

  *-- Update the Unit cut with the pieces recieved from purchased order lines.
  IF SEEK(Style,'PosLn')
    lcDivision = STRTRAN(lcDivsnExp,"ORDHDR.CDIVISION","POSHDR.CDIVISION")
    m.cSeason = cSeason
    m.cStyGroup = cStyGroup
    m.Style = Style
    m.nUnitCut = 0
    SELECT PosLn
    SCAN REST WHILE Style + cStyType + PO + STR(LineNo,6) + TranCD  = &lcWorkFile..Style ; 
         FOR PosHdr.Status <> "X" AND TranCD = "2" AND &lcDivision.
      m.nUnitCut = m.nUnitCut + TotQty
    ENDSCAN
      
    IF SEEK(&lcKeyValue.,(lcWorkFile))
      SELECT (lcWorkFile)
      REPLACE nUnitCut WITH m.nUnitCut
    ENDIF    
  ENDIF

  *-- Update the Unit cut with the pieces recieved from Cut ticket lines.
  SELECT (lcWorkFile)
  IF SEEK(Style,'CutTktL')
    lcDivision = STRTRAN(lcDivsnExp,"ORDHDR.CDIVISION","CUTTKTH.CDIVISION")
    m.cSeason = cSeason
    m.cStyGroup = cStyGroup
    m.nUnitCut = 0 
    m.Style = Style
    SELECT CutTktL
    SCAN REST WHILE Style + CutTkt + TranCD  = &lcWorkFile..Style ; 
         FOR CutTktH.Status <> "X" AND TranCD = "2" AND &lcDivision.
      m.nUnitCut = m.nUnitCut + TotQty
    ENDSCAN
    
    IF SEEK(&lcKeyValue.,(lcWorkFile))
      SELECT (lcWorkFile)
      REPLACE nUnitCut WITH nUnitCut + m.nUnitCut
    ENDIF    
  ENDIF

  *-- Update the total shipped amount from invoice line file. 
  SELECT (lcWorkFile)
  IF SEEK(Style,'InvLine')
    lcDivision = STRTRAN(lcDivsnExp,"ORDHDR.CDIVISION","INVHDR.CDIVISION")
    m.cSeason = cSeason
    m.cStyGroup = cStyGroup
    m.Style = Style
    PRIVATE lnAmount , lnQuantity
    STORE 0 TO  m.nSellPrice , lnAmount , lnQuantity
    
    SELECT InvLine
    SCAN REST WHILE Style + Invoice + STR(LineNo,6) = &lcWorkFile..Style ; 
         FOR InvHdr.Status <> "X" AND &lcDivision.
      IF SEEK('M'+Account,'Customer') AND &lcAcctExp AND Customer.Class $ lcClassExp
        lnAmount = lnAmount + (TotQty * Price)
        lnQuantity = lnQuantity + TotQty
      ENDIF 
    ENDSCAN
    *B605961,1 ALB Fix the thru % and MU% [Begin]
    *m.nSellPrice = lnAmount/IIF(lnQuantity>0,lnQuantity,1)
    *IF SEEK(&lcKeyValue.,(lcWorkFile))
    *  SELECT (lcWorkFile)
    *  REPLACE nSellPrice WITH m.nSellPrice
    *ENDIF    
    *B605961,1 ALB Fix the thru % and MU% [End]
  ENDIF

ENDSCAN

*-- End of lpContColl.

*!**************************************************************************
*! Name      : lpInsrtRec
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To insert a new record or update an old one in temp file.
*!**************************************************************************
*! Example   : DO lpInsrtRec
*!**************************************************************************
*
PROCEDURE lpInsrtRec
m.cSeason = OrdLine.Season
m.cStyGroup = Style.cStyGroup
m.Style = OrdLine.Style
m.cDesc = Style.Desc
m.ntotBook = OrdLine.TotBook
m.nTotalQty = IIF(OrdHdr.Status $ 'OH',OrdLine.TotQty,0)
m.nBookAmt  = OrdLine.TotBook * OrdLine.Price
*B605961,1 ALB Fix the thru % and MU% [Begoin]
m.nSellPrice = STYLE.pricea
*B605961,1 ALB Fix the thru % and MU% [End]
STORE 0 TO m.nClass1 , m.nClass2 , m.nClass3
DO CASE
  CASE AT(Customer.Class,lcClassExp) = 24
    m.nClass1 = OrdLine.TotBook
  CASE AT(Customer.Class,lcClassExp) = 33
    m.nClass2 = OrdLine.TotBook
  CASE AT(Customer.Class,lcClassExp) = 42
    m.nClass3 = OrdLine.TotBook
ENDCASE

IF SEEK(&lcKeyValue.,(lcWorkFile))
  SELECT (lcWorkFile)
  REPLACE ntotBook WITH ntotBook + m.ntotBook , ;
          nTotalQty WITH nTotalQty + m.nTotalQty , ;
          nBookAmt WITH nBookAmt + m.nBookAmt , ;
          nClass1 WITH nClass1 + m.nClass1 , ;
          nClass2 WITH nClass2 + m.nClass2 , ;
          nClass3 WITH nClass3 + m.nClass3 
ELSE
  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDIF
*-- End of lpInsrtRec.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR')  && get Style Group Fixed filter Position.
lnClassPos = lfItmPos('CUSTOMER.CLASS')   && get Class filter Position.
lnDivsnPos = lfItmPos('ORDHDR.CDIVISION') && get Season filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
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
*! Date      : 09/25/2001
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
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
PRIVATE lcPerFrom , lcPerTo , lcLoop

*-- Create a temporary table to hold data which will be displayed in report.
CREATE TABLE (gcWorkDir+lcWorkFile) (cSeason C(6) , cStyGroup C(6) , Style C(19) , ;
              cDesc C(20) , nTotBook N(13) , nBookAmt N(15,2) , nUnitCut N(13) , ;
              nTotalQty N(13) , nUnitCost N(15,2) , nSellPrice N(15,2) , ;
              nClass1 N(13) , nClass2 N(13) , nClass3 N(13))
IF lcRpSortBy = "S"   && sort by style group.
  lcKeyValue = "m.cStyGroup + m.cSeason + m.Style"
  INDEX ON cStyGroup + cSeason + Style TAG (lcWorkFile)  
ELSE                  && sort by season.
  lcKeyValue = "m.cSeason + m.cStyGroup + m.Style"
  INDEX ON cSeason + cStyGroup + Style TAG (lcWorkFile)
ENDIF  
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Generate Expression.
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

*-- If user selected Customer class.
IF ALLTRIM(laTempExpr[lnClassPos,1]) $ lcRpExp OR EMPTY(laOGFxFlt[lnClassPos,6])

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnClassPos,1),7)
  
  *-- Select only 3 classes.
  IF EMPTY(laOGFxFlt[1,6])
    SELECT Codes
    SET ORDER TO
    SELECT cDiscRep , cCode_No ;
    FROM   Codes ;
    WHERE  cDefCode + cFld_Name + cCode_No + cDiscRep + cRltd_Nam = "NCLASS" ;
    INTO ARRAY  laArray
    =ASORT(laArray)
    PRIVATE lnBound , lnCount
    lnBound = IIF(ALEN(laArray,1)>2,3,IIF(ALEN(laArray,1)>1,2,IIF(ALEN(laArray,1)=1,1,0)))

    laOGFxFlt[1,6] = ""
    FOR lnCount = 1 TO lnBound
      laOGFxFlt[1,6] = laOGFxFlt[1,6] + laArray[lnCount,2] + SPACE(1)      
    ENDFOR
    laOGFxFlt[1,6] = STRTRAN(ALLTRIM(laOGFxFlt[1,6]),' ',"|")
  ENDIF
  
  SELECT Codes
  SET ORDER TO TAG cCode_No
  lcClass1 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(laOGFxFlt[1,6],1,6),'Codes'),ALLTRIM(cDiscRep),'')

  IF !EMPTY(SUBSTR(laOGFxFlt[1,6],8,6))
    lcClass2 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(laOGFxFlt[1,6],8,6),'Codes'),ALLTRIM(cDiscRep),'')  
  ENDIF
    
  IF !EMPTY(SUBSTR(laOGFxFlt[1,6],15,6))
    lcClass3 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(laOGFxFlt[1,6],15,6),'Codes'),ALLTRIM(cDiscRep),'')  
  ENDIF

  *-- Generate expression for Month Range.  
  lcClassExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcClassExp,".T.")  
ENDIF  
*-- If user selected Customer class.

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
  lcRpExp = STRTRAN(lcRpExp,lcAcctExp,".T.")
ENDIF  
*-- EndIf of user selected Account.

*-- If user selected Style.
IF ALLTRIM(laTempExpr[lnStylePos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrwStyPo
  lcBrwStyPo = ALLTRIM(STR(lnStylePos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrwStyPo" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
  *-- Generate expression for Month Range.
  lcStyGrExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcStyGrExp,".T.")  
ENDIF  
*-- If user selected Style.

*-- If user selected Division.
IF ALLTRIM(laTempExpr[lnDivsnPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDivsnPos,1),7)
  
  *-- Generate expression for Month Range.  
  lcDivsnExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcDivsnExp,".T.")  
ENDIF  
*-- If user selected Customer class.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
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
  IF laMajSeg[lnI,1] = "C"            && Color
    lnClrPo    = laMajSeg[lnI,4]
    lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    lnColorLen = LEN(laMajSeg[lnI,3])
  ENDIF
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
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
*! Date      : 09/25/2001
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
*! Date      : 09/25/2001
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
