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
*B040098,1 02/16/2006 Oms ZF sequence error  [START]
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
    *B040098,1 02/16/2006 Oms ZF sequence error  [START]
    *lcVarToRep = CHR(AT(cSeq_Chr,"ABCDEFGHIGKLMNOPQRSTUVWXYZ"))
    lcVarToRep =IIF(AT(cSeq_Chr,"ABCDEFGHIGKLMNOPQRSTUVWXYZ") > 0,CHR(AT(cSeq_Chr,"ABCDEFGHIGKLMNOPQRSTUVWXYZ")+64),CHR(0))
    *B040098,1 02/16/2006 Oms ZF sequence error  [END]
  ENDIF
  REPLACE cSeq_Chr WITH lcVarToRep
ENDSCAN
WAIT CLEAR
USE IN Sequence