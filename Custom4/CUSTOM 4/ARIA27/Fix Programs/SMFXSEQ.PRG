*:***************************************************************************
*: Program file  : SMFXSEQ.PRG
*: Program desc. : Fix the sequence file
*! Date          : 02/09/2003
*: System        : Aria Advantage Series.
*: Module        : System Manager (SM)
*: Developer     : Khalid Mohi El-din (KHM)
*: Tracking Job Number: B606902
*:***************************************************************************
*: Example : DO SMFXSEQ.PRG
*:***************************************************************************
PARAMETER lcDataDir

lcVarToRep = ""
IF USED("SEQUENCE")
  USE IN SEQUENCE
ENDIF
USE lcDataDir+'SEQUENCE.DBF' IN 0 ORDER Cseq_type
SELECT Sequence

SCAN
  WAIT WINDOW "Fixing sequence file " NOWAIT
  *-- If cSeq_Chr is empty replace it with CHR(0)
  IF EMPTY(cSeq_Chr)
    lcVarToRep = CHR(0)
  ELSE    
    lcVarToRep = CHR(AT(cSeq_Chr,"ABCDEFGHIGKLMNOPQRSTUVWXYZ"))
  ENDIF
  REPLACE cSeq_Chr WITH lcVarToRep
ENDSCAN
WAIT CLEAR
USE IN Sequence