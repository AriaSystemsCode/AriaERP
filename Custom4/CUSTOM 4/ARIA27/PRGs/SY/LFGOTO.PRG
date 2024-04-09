*:************************************************************************
*: Program file  : LFGOTO.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
lcCurRec=RECNO()
lnRecord=0
DO lwGoTo.SPR

*!**************************************************************************
*!
*!      Function : lfvOption
*!
*!**************************************************************************
*
FUNCTION lfvOption
IF rbGoTo = 3
  SHOW GET lnRecord ENABLE
  _CUROBJ=OBJNUM(lnRecord)
ELSE
  SHOW GET lnRecord DISABLE  
ENDIF
 
*!**************************************************************************
*!
*!      Function : lfvRecord
*!
*!**************************************************************************
*
FUNCTION lfvRecord
 IF lnRecord > RECCOUNT()
   WAIT WINDOW "Record No. is out of range" NOWAIT
   lnRecord = 0  
   _CUROBJ=OBJNUM(lnRecord)
 ENDIF
   
*!**************************************************************************
*!
*!      Function : lfvGoTo
*!
*!**************************************************************************
*
*FUNCTION lfvGoTo
*DO CASE
*  CASE rbGoTo = 1
*    IF pbGoTo = 2
*      GO lcCurRec
*    ELSE
*      GO TOP  
*    ENDIF
*  CASE rbGoTo = 2
*    IF pbGoTo = 2
*      GO lcCurRec
*    ELSE
*      GO BOTTOM
*    ENDIF  
*  CASE rbGoTo = 3
*    IF pbGoTo = 1
*      IF lnRecord >0
*        GO lnRecord
*      ENDIF
*    ELSE
*      GO lcCurRec
*    ENDIF  
*ENDCASE
*!**************************************************************************
*!
*!      Function : lfvGoTo
*!
*!**************************************************************************
*
FUNCTION lfvGoTo
DO CASE
  CASE rbGoTo = 1
    GO TOP  
  CASE rbGoTo = 2
    GO BOTTOM
  CASE rbGoTo = 3
    IF lnRecord >0
      GO lnRecord
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
GO lcCurRec
