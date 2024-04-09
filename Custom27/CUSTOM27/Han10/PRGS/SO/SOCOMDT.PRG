*:**************************************************************************
*: Program file  : SOCOMDT - for HAN10
*: Program desc. : replace the empty complete date on ordhdr by system date
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Abdel Naby (AAN)
*: Date          : 09/20/2001
*: Reference     : C102447
*:**************************************************************************
*
PRIVATE llOpen

IF gfModalGen('QRM32095B32000','ALERT') = 1
  llOpen = .F.
  llOpen=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
  SELECT OrdHdr
  SCAN
    WAIT WINDOW "Update the complete date for order "+Order NOWAIT
    IF EMPTY(Complete)
      IF RLOCK()
        REPLACE complete WITH gdSysDate
        UNLOCK IN OrdHdr
      ENDIF
    ENDIF
  ENDSCAN
  IF llOpen
    USE IN OrdHdr
  ENDIF

  llOpen = .F.
  llOpen=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
  SELECT OrdLine
  SCAN
    WAIT WINDOW "Update the complete date for order lines "+Order NOWAIT
    IF EMPTY(Complete)
      IF RLOCK()
        REPLACE complete WITH gdSysDate
        UNLOCK IN OrdLine
      ENDIF
    ENDIF
  ENDSCAN
  IF llOpen
    USE IN OrdLine
  ENDIF
ENDIF
WAIT CLEAR
