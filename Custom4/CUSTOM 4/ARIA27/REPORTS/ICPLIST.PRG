*:************************************************************************
*: Program file  : ICPList.prg
*: Program desc. : Price List.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: IC.
*:      Developer: TAK - Timour Abdalla Khalil 
*:           Date: 06/10/98
*:************************************************************************
*: Modifications :
*:E301127,1 AMM 02/01/99 Add an option to print the retail price after discount
*:B603157,1 RAMY 09/14/99 Fixed the position field in SYREPUVR
*                         for Print  price after disc.
*:B603631,1 AMM 05/18/2000 Centeralaize titles of the report (done in .frx)
*:************************************************************************
lcPrice = IIF(EMPTY(lcPrice),'L',lcPrice)
*E301127,1 AMM Initialize variables to get discount percentage
lnDisc  = 0
DIMENSION laDisc[1,2]
laDisc[1,1] = 'DISCPCNT'
laDisc[1,2] = "lnDisc"
*E301127,1 AMM end

SELECT STYLE
SET FILTER TO &lcRpExp
GO TOP
IF EOF()
  *E301127,1 AMM Replace the wait window with message
  *WAIT WINDOW 'NOTHING TO PRINT!'
  *E301127,1 AMM Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  *E301127,1 AMM end
ELSE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
SET FILTER TO
RETURN


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 06/10/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lcStyTtl  = gfItemMask('HI')
lcStyPict = '@! '+gfItemMask('PI')

*--Open report files.
= gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
= gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
RETURN



*!*************************************************************
*! Name      : lfvStyle
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate style code.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
lcMStyle = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF
lcFldStyNam = SYS(18)
IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
  lcMStyle = gfStyBrw('I',"","",.F.)
  &lcFldStyNam = IIF(!EMPTY(lcMStyle),lcMStyle,lcOldValue)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfGetScl
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to get style scale.
*!*************************************************************
FUNCTION lfGetScl
PARA lcScale

lcSclDesc =''
IF SEEK('S'+lcScale,'SCALE')
  FOR I=1 TO SCALE.Cnt
    lcZ = STR(I,1)
    lcSclDesc = lcSclDesc + ALLTRIM(SCALE.SZ&lcZ)+IIF(I=SCALE.Cnt,'',' - ')
  ENDFOR
ENDIF
*E301127,1 AMM Get the discount percentage
IF EMPTY(STYLE.cDiscCode)
  lnDisc = 0
ELSE
  =gfRltFld(STYLE.cDiscCode,@laDisc,'CDISCCODE')
ENDIF
*E301127,1 AMM end
RETURN lcSclDesc
