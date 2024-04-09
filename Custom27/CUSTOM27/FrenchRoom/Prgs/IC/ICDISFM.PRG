*:************************************************************************
*: Program file  : ICDISFM.PRG
*: Program desc. : Style Discount Program
*:         System: ARIA 2.7
*:         Module: Inventory Control
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 26/09/2000
*:
*:  Procs & Fncts: lfProcced
*:               : lfvDisct
*:
*:************************************************************************
*: For custom# C101977
*:***************************************************************************
*:

*-- lcExpr           && variable to hold the OG filter Expresion.
*-- lnRpDisc         && OG variabel hold the Discount.
*-- lcStyMajor       && Variable to hold the style Major.
*-- lcStylePic       && Variable to hold the style Picture.
*-- lnO_T_S          && Variable to hold Open To Sell quantity 


STORE .T. TO lcExpr 
STORE 0 TO lnRpDisc , lnO_T_S
STORE "" TO lcStyMajor , lcStylePic       

lcStyMajor=gfItemMask('HM')
lcStylePic='@! ' + ALLTRIM(gfItemMask("PM"))

*-- Open Style File
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'cSTYLE','SH')

*-- Call the selection grid
lcExpr = gfOpGrid('ICDISFM' , .T.)  


*-- IF lcExpr have a value. which means that the user selecte certain criteria.
IF TYPE('lcExpr') # 'L'
  =lfProcced()
ENDIF 

*!*************************************************************
*! Name      : lfProcced
*! Developer : Hend Ghanem (HBG)
*! Date      : 26/09/2000
*! Purpose   : To Update price B For the style
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfProcced()
*!*************************************************************
FUNCTION lfProcced
PRIVATE llFound , lnTotRec , lnCurRec 

*-- lnTotRec       && Variabel to hold no of selected records
*-- lnCurRec       && Variabel to encrement the thermo

*-- Flag to chech if There is Records to procced or not
llFound = .F.


SELECT STYLE
*-- Get no of selected records
COUNT TO lnTotRec FOR &lcExpr
*-- Initialize the encrementing Variabel
lnCurRec = 0        

SCAN FOR &lcExpr
  lnCurRec = lnCurRec  + 1
  llFound = .T.
  REPLACE PriceB WITH ROUND(PriceA - (PriceA * lnRpDisc/100),2)
  =gfThermo(lnTotRec , lnCurRec , "Calculating Price B For Style : " , STYLE )
ENDSCAN

*-- Displaying a message to indicate that no records were found to display.
IF !llFound
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No Records have been selected')
ELSE   && Displaying a message to indicate that no records were found to display.
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'All styles have updated successfully.')
ENDIF

*-- End of lfProcced

*!*************************************************************
*! Name      : lfvDisct
*! Developer : Hend Ghanem (HBG)
*! Date      : 26/09/2000
*! Purpose   : To validate the discount.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfvDisct()
*!*************************************************************
FUNCTION lfvDisct

IF lnRpDisc < 0 Or lnRpDisc > 100
  lnRpDisc = 0
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'The discount percentage you have ;
              entered is invalid. Enter a discount percentage between 0 and 100.')
  _CUROBJ = _CUROBJ
ENDIF  


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Hend Ghanem (HBG)
*! Date      : 26/09/2000
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
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Hend Ghanem (HBG)
*! Date      : 26/09/2000
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
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.
