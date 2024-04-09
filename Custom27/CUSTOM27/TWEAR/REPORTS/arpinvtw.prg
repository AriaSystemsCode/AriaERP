*:***************************************************************************
*: Program file  : ARPINVTW
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR TEE WEAR
*! Date          : 09/15/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVTW
*:***************************************************************************
*: This Report Program is due to C101661 ...
*:***************************************************************************

PRIVATE lcAlias  , lcCurSCale , laTempStru, InNo
lcAlias = ALIAS()

*-- Open the Ordline file to get from it the OrdLine TotQty. which is the back order quantity.

IF !USED(lcOrdLine)  
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

SELECT InvLine
SET RELATION TO 'O'+order+store+style+STR(lineno,6) INTO &lcOrdLine ADDITIVE

SELECT STYLE
SET ORDER TO TAG SCALE IN SCALE
SET RELATION TO 'S' + Scale INTO SCALE

CREAT TABLE (gcWorkDir+lcInvTemp) ( Invoice C(10), LineCnt n(10))
INDEX ON Invoice TAG Invoice of (gcWorkDir+lcInvTemp)

SELECT InvHdr
SCAN FOR &lcRpExp

  IF !SEEK(InvHdr.Invoice,lcInvTemp) .AND. EVAL(lcTmpDbt+'.cfile_num') # '2'
    SCATTER FIELDS LIKE Invoice TO M.Invoice
    SELECT(lcInvTemp)
    INSERT INTO (lcInvTemp) (INVOICE) VALUES (M.Invoice)
  ENDIF
  IF EVAL(lcTmpDbt+'.cfile_num') # '2'
    SELECT(lcInvTemp)
    *-- We put '2' in the case false because we will print the style
    *-- in the first line and in the seconed line the scale quantities 
    REPLACE LineCnt WITH LineCnt + 2
  ENDIF                                                                    
ENDSCAN
SET ORDER TO TAG INVOICE IN (lcInvTemp)
SELECT InvHdr
SET RELATION TO INVOICE INTO (lcInvTemp) ADDITIVE

SELECT (lcAlias)
*!*************************************************************
*! Name        : lfInvNote
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/14/1999
*! Purpose     : To seek with the invoice in the notepad
*!*************************************************************
*! Called from : ARPINVTW.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfInvNote()
*!*************************************************************

FUNCTION lfInvNote
PARAMETERS lcDumdy

=SEEK('C' + InvHdr.Invoice,'Notepad') 

*--This is a for loop to get the first tow lines of the header notepad to 
*--put it in the comment of the invoice.
lcInvNote1 = MLINE(NOTEPAD.mNotes , 1) 
lcInvNote2 = MLINE(NOTEPAD.mNotes , 2) 

RETURN !EMPTY(lcInvNote1) .AND. !EMPTY(lcInvNote2)

*!*************************************************************
*! Name        : lflcOldInv
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/14/1999
*! Purpose     : To get the variable its value
*!*************************************************************
*! Called from : ARPINVTW.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lflcOldInv()
*!*************************************************************

FUNCTION lflcOldInv
PARAMETERS lcDumdy

lcOldInv = InvHdr.invoice

RETURN ''