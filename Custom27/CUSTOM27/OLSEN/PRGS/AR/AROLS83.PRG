*!*******************************************************************************
*! Program file  : AROLS83.PRG
*! Program desc. : Custom Postdated Cheques Deletion.
*! System        : Aria Apparel System
*! Module        : Accounts Receivable
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 10/31/2001
*! Reference     : C102477,1
*!*******************************************************************************
*
PRIVATE lcPostDExp
lcPostDExp  = gfOpGrid('AROLS83', .T.)

PRIVATE llPostDChq
llPostDChq = gfOpenFile(gcDataDir+'PostDChq','','SH') 

*-- Check if user pressed the close button in option grid.
IF TYPE('lcPostDExp') = 'L'
  RETURN
ENDIF

IF lcPostDExp = ".T."
  =gfModalGen("TRM000000B00000","DIALOG",'','','End date cannot be empty!')
  RETURN
ENDIF

SELECT PostDChq
LOCATE
*-- Message < No Postdated cheques found for deletion. >
*-- Buttons <                    OK                    >
IF EOF()
  =gfModalGen("TRM000000B00000","DIALOG",'','','No Postdated cheques found for deletion.')
  RETURN
ENDIF
*-- Check if user didn't typed a Month Range. [End]

LOCATE FOR &lcPostDExp
IF !FOUND()
  =gfModalGen("TRM000000B00000","DIALOG",'','','No postdated cheques in this date range found, Cannot proceed with deletion.')
  RETURN
ENDIF
IF gfModalGen("TRM00000B00006","ALERT",'','','Are you sure you want to delete all the postdated cheques within the specified date.') = 1
  WAIT WINDOW 'Deleting the postdated cheques...' NOWAIT
  SELECT PostDChq
  SCAN FOR &lcPostDExp
    BLANK
    DELETE
  ENDSCAN
ENDIF
WAIT CLEAR

IF llPostDChq
  USE IN PostDChq
ENDIF
*-- End of Program

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/31/2001
*! Purpose   : Report When Function
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnPayDate
lnPayDate = lfItmPos('POSTDCHQ.PAYDATE')   && get Tran. Date Fixed filter Position

*-- If Pay Date found in Fixed filter option
IF lnPayDate > 0
  *-- Replace TO DATE in Transaction date with Current Date (System Date)
  *-- If system uses 4 Char. for Century 
  IF "ON" $ SET('CENTURY')
    laOGFxFlt[lnPayDate,6] = '  /  /    |' + DTOC(gdSysDate)
  ELSE    && Else system uses 2 char. for Century
    laOGFxFlt[lnPayDate,6] = '  /  /  |' + DTOC(gdSysDate)
  ENDIF
  *-- EndIf of system uses 4 Char. for Century 
ENDIF
*-- Endif of Pay Date found in Fixed filter option 
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/31/2001
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
