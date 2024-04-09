*:**************************************************************************
*: Program file  : ARCSTMPA
*: Program desc. : Amendments to customer statment form B for panache lingerie ltd .
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Ehab Ismail Hamed (EIH)
*: Date          : 07/31/2005
*: Reference     : CP#128479
*:**************************************************************************
*: Functions     : ARCSTMPA
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications:
*:**************************************************************************


*!**************************************************************************
*! Name      : lfNoteTemp
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 07/31/2005
*! Purpose   : Print notepad templete Called from arcstmb.frx. 
*!**************************************************************************
*! Example   : =lfNoteTemp()
*!**************************************************************************
FUNCTION lfNoteTemp
PARAMETER lcDumdy
lcNoteTmp =''
IF SEEK('M'+&lcTmpTrans..Account , 'CUSTOMER')
  IF SEEK('T'+CUSTOMER.CCURRCODE+"/"+CUSTOMER.CconT_Code,'Notepad')
    lcNoteTmp = NOTEPAD.MNOTES
  ELSE
    lcNoteTmp = ''
  ENDIF
ELSE
  lcNoteTmp =''
ENDIF
RETURN ''
*-- End of lfNoteTemp.

