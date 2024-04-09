*:**************************************************************************
*: Program file  : SOSOC30
*: Program desc. : Print ITC form and Box label for an Order.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102127 , C102131
*:**************************************************************************
*: Procedures    : lpCreatFil
*:
*: Functions     : lfwRepWhen , lfItmPos , lfEvalSegs , lfwOldVal
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

PRIVATE ALL LIKE llMessage* , lcJSize* , lcSSize* , lnCount , lcNumber
STORE .F. TO llMessage1 , llMessage2 , llMessage3
FOR lnCount = 1 TO 8
  lcNumber = ALLTRIM(STR(lnCount,1))
  lcJSize&lcNumber = ''
  lcSSize&lcNumber = ''  
ENDFOR

*-- If user changed the criteria selected. [Begin]
IF llOGFltCh
  *-- Create both Temp. Order Header and Temp. Loop cursors
  DO lpCreatFil
  
  PRIVATE lnStyleNo , lcOldStore , lnTeamCount , lcInvoice , lcCount

  SELECT OrdLine
  SET RELATION TO Style INTO Style

  SELECT OrdHdr  
  SCAN FOR &lcRpExp
    IF !SEEK(cOrdType+Order,'Soccer') 
      llMessage1 = .T.
      LOOP
    ELSE
      DO CASE
        CASE Soccer.OStation='N'
          llMessage2 = .T.
          LOOP
        CASE Soccer.OStation='A' AND lcRpPrtFrm = 'L'
          llMessage3 = .T.
          LOOP
      ENDCASE
    ENDIF
    
    lnStyleNo = 1
    lcOldStore  = SPACE(8)
    lnTeamCount = 0
    
    SELECT OrdLine
    =SEEK(Ordhdr.cOrdType + Ordhdr.Order)
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType + OrdHdr.Order
      SELECT (lcOrdHTemp)
      IF OrdLine.Store <> lcOldStore
        lnStyleNo   = 1
        lnTeamCount = lnTeamCount + 1
        SELECT InvLine
        = SEEK(OrdLine.Order,'InvLine')
        IF FOUND()
          LOCATE FOR Order+STR(Lineno,6)+Invoice = OrdLine.Order AND STORE = OrdLine.Store
          lcInvoice = IIF(FOUND(),InvLine.Invoice,SPACE(06))
        ELSE
          lcInvoice = SPACE(06)
        ENDIF
        SELECT (lcOrdHTemp)
        APPEND BLANK
        REPLACE  cOrdType WITH 'O'                       ,;
                 Order    WITH OrdLine.Order             ,;
                 Div      WITH SUBSTR(OrdLine.Store,1,4) ,;
                 Team     WITH SUBSTR(OrdLine.Store,5,8) ,;
                 Account  WITH OrdLine.Account           ,;
                 ShipVia  WITH OrdHdr.ShipVia            ,;
                 DueDate  WITH OrdHdr.Complete           ,;
                 CusName  WITH IIF(SEEK('M'+OrdLine.Account,'Customer'),Customer.BtName,SPACE(30)),;
                 League   WITH OrdHdr.Dept               ,;
                 Invoice  WITH lcInvoice,;
                 TeamNo   WITH lnTeamCount
        IF SEEK('O'+Order+Div+Team,'Soccer')
          REPLACE InkColor  WITH Soccer.InkColorD        ,;
                  PrInst    WITH Soccer.PrintInst        ,;
                  SpName    WITH Soccer.SpName           ,;
                  LogoName  WITH Soccer.LogoName         ,;
                  TeamsNo   WITH Soccer.TeamsNo          ,;
                  SpInst    WITH Soccer.SpInst     

        ENDIF
        lcOldStore = OrdLine.Store
      ENDIF

      DO CASE
        CASE Style.cStyGroup = 'JY'
          REPLACE JYStyle WITH SUBSTR(OrdLine.Style,1,lnStyleLen) ,;
                  JYColor WITH SUBSTR(OrdLine.Style,lnClrPo,lnColorLen) ,;
                  JYScale WITH OrdLine.Scale,;
                  JYQty1  WITH JYQty1+OrdLine.Qty1 ,;
                  JYQty2  WITH JYQty2+OrdLine.Qty2 ,;
                  JYQty3  WITH JYQty3+OrdLine.Qty3 ,;
                  JYQty4  WITH JYQty4+OrdLine.Qty4 ,;
                  JYQty5  WITH JYQty5+OrdLine.Qty5 ,;
                  JYQty6  WITH JYQty6+OrdLine.Qty6 ,;
                  JYQty7  WITH JYQty7+OrdLine.Qty7 ,;
                  JYQty8  WITH JYQty8+OrdLine.Qty8 ,;
                  JYTot   WITH JYQty1+JYQty2+JYQty3+JYQty4+JYQty5+JYQty6+JYQty7+JYQty8

        CASE Style.cStyGroup = 'SH'
          REPLACE SHStyle WITH SUBSTR(OrdLine.Style,1,lnStyleLen) ,;
                  SHColor WITH SUBSTR(OrdLine.Style,lnClrPo,lnColorLen) ,;
                  SHScale WITH OrdLine.Scale,;
                  SHQty1  WITH OrdLine.Qty1 ,;
                  SHQty2  WITH OrdLine.Qty2 ,;
                  SHQty3  WITH OrdLine.Qty3 ,;
                  SHQty4  WITH OrdLine.Qty4 ,;
                  SHQty5  WITH OrdLine.Qty5 ,;
                  SHQty6  WITH OrdLine.Qty6 ,;
                  SHQty7  WITH OrdLine.Qty7 ,;
                  SHQty8  WITH OrdLine.Qty8 ,;
                  SHTot   WITH OrdLine.TotQty      

        CASE Style.cStyGroup = 'SO'
          REPLACE SOStyle WITH SUBSTR(OrdLine.Style,1,lnStyleLen) ,;
                  SOColor WITH SUBSTR(OrdLine.Style,lnClrPo,lnColorLen) ,;
                  SOScale WITH OrdLine.Scale,;
                  SOQty1  WITH OrdLine.Qty1 ,;
                  SOQty2  WITH OrdLine.Qty2 ,;
                  SOQty3  WITH OrdLine.Qty3 ,;
                  SOTot   WITH SOQty1+SOQty2+SOQty3
        OTHERWISE
          DO CASE 
            CASE OrdLine.Style = Style1
              REPLACE Qty1 WITH Qty1 + OrdLine.TotQty  
            CASE OrdLine.Style = Style2
              REPLACE Qty2 WITH Qty2 + OrdLine.TotQty  
            CASE OrdLine.Style = Style3
              REPLACE Qty3 WITH Qty3 + OrdLine.TotQty  
            OTHERWISE
              IF lnStyleNo <= 3
                lcCount = STR(lnStyleNo,1)
                REPLACE Style&lcCount WITH SUBSTR(OrdLine.Style,1,lnStyleLen)   ,;
                        Color&lcCount WITH SUBSTR(OrdLine.Style,lnClrPo,lnColorLen)   ,;
                        Desc&lcCount  WITH Style.Desc      ,;
                        Qty&lcCount   WITH OrdLine.TotQty           
                lnStyleNo = lnStyleNo + 1                  
              ENDIF
          ENDCASE
      ENDCASE
    ENDSCAN
    SELECT (lcOrdHTemp)
    IF SEEK(OrdHdr.cOrdType+OrdHdr.Order)
      REPLACE REST NoOfTeam WITH lnTeamCount ;
              WHILE cOrdType + Order = OrdHdr.cOrdType + OrdHdr.Order
    ENDIF
  ENDSCAN

  IF llMessage1
    *-- Message <One or more orders has no printing information!>
    *-- Buttons <                      OK                       >
    =gfModalGen("TRM000000B00000","DIALOG",'','','One or more orders has no printing information!')
  ENDIF
  IF llMessage2
    *-- Message <One or more orders is not accepted yet, please change the order station to Accepted before printing ITCs or Box Labels.>
    *-- Buttons <                                                          OK                                                           >
    =gfModalGen("TRM000000B00000","DIALOG",'','','One or more orders is not accepted yet, please change the order station to Accepted before printing ITCs or Box Labels.')
  ENDIF
  IF llMessage3
    *-- Message <One or more orders are accepted. You cannot print Box Labels for Accepted orders.>
    *-- Buttons <                                       OK                                        >
    =gfModalGen("TRM000000B00000","DIALOG",'','','One or more orders are accepted. You cannot print Box Labels for Accepted orders.')
  ENDIF 
ENDIF
*-- EndIf of user changed the criteria selected. [End] 

SELECT (lcOrdHTemp)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <              OK               >
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  SELECT (lcOrdHTemp)
  LOCATE
  IF lnLabelNum > 1 AND lcRpPrtFrm = "L"
    SET RELATION TO "" INTO (lcTmpLoop)
    SET SKIP TO (lcTmpLoop)
  ENDIF  

  DO gfDispRe WITH EVAL('lcRpName')
  SELECT (lcOrdHTemp)
  LOCATE
  IF lcRpPrtFrm = "I"
    SCAN
      IF SEEK(&lcOrdHTemp..cOrdType + &lcOrdHTemp..Order,'Soccer')
        SELECT Soccer
        REPLACE REST OStation WITH 'C' WHILE cOrdType + Order= 'O' + &lcOrdHTemp..Order
      ENDIF
    ENDSCAN
  ELSE 
    SCAN
      IF SEEK(&lcOrdHTemp..cOrdType + &lcOrdHTemp..Order,'Soccer')
        SELECT Soccer
        REPLACE REST TStation WITH 'K' WHILE cOrdType + Order= 'O' + &lcOrdHTemp..Order
      ENDIF
    ENDSCAN
  ENDIF
ENDIF 
*-- End of Report.

*!**************************************************************************
*! Name      : lfScale
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
*! Purpose   : Get the style scale.
*!**************************************************************************
*! Example   : = lfScale()
*!**************************************************************************
*
FUNCTION lfScale
PRIVATE lnSizes, lcNumber
=SEEK('S'+&lcOrdHTemp..JYScale,'Scale')
FOR lnSizes = 1 TO 8
  lcNumber  = ALLTRIM(STR(lnSizes,1))
  lcJSize&lcNumber = Scale.Sz&lcNumber
ENDFOR

=SEEK('S'+&lcOrdHTemp..SOScale,'Scale')
FOR lnSizes = 1 TO 3
  lcNumber = ALLTRIM(STR(lnSizes,1))
  lcSSize&lcNumber = Scale.Sz&lcNumber
ENDFOR
*-- End of lfScale.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnEnterDat = lfItmPos('ORDHDR.ENTERED')   && get Entered date Position

*-- If Entered Date found in Fixed filter option.
IF lnEnterDat > 0
  IF lcRpSelcBy = "D"
    *-- Replace TO DATE in Entered date with Current Date (System Date)
    *-- If system uses 4 Char. for Century 
    laOGFxFlt[lnEnterDat,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
    *-- EndIf of system uses 4 Char. for Century     
  ELSE
    *-- Replace TO DATE in Entered date with null date
    *-- If system uses 4 Char. for Century 
    IF "ON" $ SET('CENTURY')
      laOGFxFlt[lnEnterDat,6] = '  /  /    |  /  /    '
    ELSE    && Else system uses 2 char. for Century
      laOGFxFlt[lnEnterDat,6] = '  /  /  |  /  /  '
    ENDIF
    *-- EndIf of system uses 4 Char. for Century   
  ENDIF  
ENDIF
*-- EndIf of Entered Date found in Fixed filter option
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
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
*! Name      : lfvSelcBy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
*! Purpose   : Validate select by option in option grid.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvSelcBy()
*!**************************************************************************
*
FUNCTION lfvSelcBy
llOGFltCh = .T.
IF lcRpSelcBy = "D"
  lcRpName = 'SOSOC32'
  *-- Replace TO DATE in Entered date with Current Date (System Date)
  *-- If system uses 4 Char. for Century 
  laOGFxFlt[lnEnterDat,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  *-- EndIf of system uses 4 Char. for Century       
ELSE
  lcRpName = 'SOSOC31'
  *-- Replace TO DATE in Entered date with null date
  *-- If system uses 4 Char. for Century 
  IF "ON" $ SET('CENTURY')
    laOGFxFlt[lnEnterDat,6] = '  /  /    |  /  /    '
  ELSE    && Else system uses 2 char. for Century
    laOGFxFlt[lnEnterDat,6] = '  /  /  |  /  /  '
  ENDIF
  *-- EndIf of system uses 4 Char. for Century     
ENDIF
CLEAR READ
*-- End of lfvSelcBy.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
PRIVATE lnLoop , lcNumber
CREATE CURSOR (lcOrdHTemp) ;
        (cOrdType C(1),;
        Order C(6) , Div C(4) , Team C(4) , Account C(5) , CusName  C(30) ,;
        League C(5) , InkColor C(10) , PrInst C(40) , SpName C(40) , LogoName C(40),;
        JYStyle C(lnStyleLen) , SHStyle C(lnStyleLen) , SOStyle C(lnStyleLen) , ;
        JYColor C(lnColorLen) , SHColor C(lnColorLen) , SOColor C(lnColorLen) , ;
        JYQty1 N(6) , JYQty2 N(6) , JYQty3 N(6) , JYQty4 N(6) , JYQty5 N(6) , ;
        JYQty6 N(6) , JYQty7 N(6) , JYQty8 N(6) , SHQty1 N(6) , SHQty2 N(6) , ;
        SHQty3 N(6) , SHQty4 N(6) , SHQty5 N(6) , SHQty6 N(6) , SHQty7 N(6) , ;
        SHQty8 N(6) , SOQty1 N(6) , SOQty2 N(6) , SOQty3 N(6) , DueDate D(8), ;
        TeamsNo C(20) , Invoice C(6) , JYScale C(3) , SHScale C(3) , SOScale  C(3) ,;
        SHTot  N(7) , JYTot N(7) , SOTot N(7) , SpInst M , NoOfTeam N(3) ,;
        TeamNo  N(4) , Style1 C(lnStyleLen) , Style2 C(lnStyleLen) , Style3 C(lnStyleLen),;
        Color1  C(lnColorLen) , Color2 C(lnColorLen) , Color3 C(lnColorLen) , Desc1 C(20),;
        Desc2 C(20) , Desc3 C(20) , Qty1 N(6) , Qty2 N(6) , Qty3 N(6) , ShipVia  C(6) )         
INDEX ON cOrdType + Order TAG (lcOrdHTemp)

*-- If Label number equal 1 no need to create this file
IF lnLabelNum > 1 AND lcRpPrtFrm = "L"
  CREATE CURSOR (lcTmpLoop) (cLoop C(4))
  ZAP
  INDEX ON cLoop TAG (lcTmpLoop)

  *-- Add number of records equal to labels number in Temp Loop file.
  FOR lnLoop = 1 TO lnLabelNum
    lcNumber = ALLTRIM(STR(lnLoop,4))
    INSERT INTO (lcTmpLoop) (cLoop) VALUES (lcNumber)
  ENDFOR
ENDIF  
*-- EndIf of Label number equal 1 no need to create this file

*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
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
*! Name      : lfvPrtForm
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
*! Purpose   : Validation for the Print form (ITC or Label Box)
*!**************************************************************************
*! Called from : [Option Grid]
*!**************************************************************************
*! Example     : = lfvPrtForm()
*!**************************************************************************
*
FUNCTION lfvPrtForm
llClearOrd = .T.
IF lcRpPrtFrm = "I"
  lcRpName = "SOSOC31"
ELSE
  lcRpName = "SOSOC32"
ENDIF
*-- End of lfvPrtForm.

*!**************************************************************************
*! Name      : lfSROrder
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/08/2001
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
