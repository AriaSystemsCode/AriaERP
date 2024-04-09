*:***************************************************************************
*: Program file  : ARPINVCD.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR Cathy Daniels.
*: Date          : 01/24/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102167
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVCD
*:***************************************************************************

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnCd  = LEN(laItemSeg[lnCount,3])
    lnClrPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnCd  = LEN(laItemSeg[lnCount,3])
    lnStyPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
                       *-- End of Program --*
*!*************************************************************
*! Name      : lfInvPad
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/24/2001
*! Purpose   : Function to return the first 2 line of the notepad.
*!*************************************************************
*! Called from : ARPINVCD.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfInvPad
PARAMETERS lcDumdy

IF SEEK('C' + INVHDR.Invoice , 'NOTEPAD') .AND. ;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes))    .AND. ;
       LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' 

  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 100
  FOR lnContCd = 1 TO 2
    lcNum = ALLTRIM(STR(lncontcd))
    lcInvPad&lcNum = LEFT(MLINE(NOTEPAD.MNOTES,lnContCd),35)
  ENDFOR
  SET MEMOWIDTH TO lnOldMemW
ENDIF

RETURN ""

*--End of lfInvPad.