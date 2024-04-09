*:***************************************************************************
*: Program file  : ARPINVPR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR PRISM.
*: Date          : 10/16/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101992
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVPR
*:***************************************************************************
*:B803784 BWA 11/02/2000 Modify the formatting of some fields to match the pre printed form.
*:B604117 BWA 01/15/2001 Adjustment some fields in the layout[The fix in the FRX]
*:***************************************************************************
*-- Initial the vaiables.
*-lnContLin >> Hold the number of the invoice lineno in the invline
*-lnPageNo  >> Hold the number of the count of the invoice lines and notepade.
STORE 0  TO lnPageNo , lnContLin

                  *--Section to get the color length--*
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLenPr = LEN(laItemSeg[lnCount,3])
    lnClrPosPr = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
                  *--End Section to get the color length--*
CREATE CURSOR (lcTempPag) (Invoice C(6),PageNo N(5))
INDEX ON INVOICE TAG (lcTempPag)

SELECT INVHDR
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE
lcSkip = SET('SKIP')
lcRcnoInv = RECNO()
SET SKIP TO
SCAN FOR &lcRpExp
  WAIT WINDOW 'Selecting Records For The Report ...' + INVHDR.INVOICE NOWAIT
  INSERT INTO (lcTempPag) (Invoice,PageNo) VALUES (INVHDR.INVOICE,lfpageno())
ENDSCAN

SELECT (lcTempPag)
GOTO TOP

SELECT INVHDR
GOTO lcRcnoInv IN INVHDR
SET SKIP TO &lcSkip
SET RELATION TO INVOICE INTO (lcTempPAg) ADDITIVE
WAIT CLEAR

*!*************************************************************
*! Name      : lfpageno
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/16/2000
*! Purpose   : Function to Get page no.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnPageNo
*!*************************************************************
FUNCTION lfpageno
PRIVATE lcAlias , lnTotal

lnTotal = 0
lcAlias = ALIAS()
SELECT (lcInvLine_PR)
=SEEK(InvHdr.Invoice,lcInvLine_PR)
SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
 lnContLin = lnContLin + 1
ENDSCAN
lnNotcont = IIF(llRpInvNot,MEMLINES(NOTEPAD.MNOTES) , 0)
lnTotal = (lnContLin * 4.5) + lnNotcont
lnPageNo = CEILING(lnTotal/36)

SELECT (lcAlias)
RETURN lnPageNo
*-- End of lfpageno.