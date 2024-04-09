*:***************************************************************************
*: Program file  : ARPINVVT.PRG
*: Program desc. : Customized Invoice Form VT for Vital
*: Date          : 02/25/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number: C102555
*:***************************************************************************
*: Example : DO ARPINVVT
*:***************************************************************************
*

IF lcRpPrint = "P" AND lcFactrInv <> 'B'
  IF lcFactrInv = 'F'          && Factored. 
    lcRpExp = STRTRAN(lcRpExp,'!EMPTY(INVHDR.cFacCode)','.T.')
  ELSE                         && Non Factored.
    lcRpExp = STRTRAN(lcRpExp,'EMPTY(INVHDR.cFacCode)','.T.')
  ENDIF
ENDIF
*-- End of Report.