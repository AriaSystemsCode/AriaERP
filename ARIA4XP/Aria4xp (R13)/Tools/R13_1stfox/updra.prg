PARAMETER lcDataPath
PRIVATE llUsed , llNoteUsed

IF TYPE('lcDataPath') <> 'C'
  lcDataPath = GETDIR('','Select Data Dir.')
ENDIF

IF EMPTY(lcDataPath)
  WAIT WINDOW 'Invalid Data Path'
  RETURN
ENDIF
WAIT WINDOW 'Opening Data Files...' NOWAIT
IF !FILE(lcDataPath + 'RETAUTH.DBF')
  WAIT WINDOW "Return authorization file doesn't exist...!" 
  RETURN
ENDIF
IF !USED('RETAUTH')
  USE (lcDataPath + 'RETAUTH.DBF') IN 0 SHARED
  llUsed = .F.
ELSE
  llUsed = .T.
ENDIF
IF TYPE('lHasNotes') <> 'L'
  IF !llUsed
    USE IN RETAUTH
  ENDIF
  RETURN
ENDIF
IF !USED('NOTEPAD')
  USE (lcDataPath + 'NotePad.DBF') IN 0 SHARED
  llNoteUsed = .F.
ELSE
  llNoteUsed = .T.
ENDIF

SELECT RETAUTH
SET ORDER TO TAG RETAUTH
GO TOP

SELECT NOTEPAD

SCAN FOR TYPE = 'A'
  WAIT WINDOW 'Searching Return Authorization File : ' + RETAUTH.RANO NOWAIT
  IF SEEK(PADR(KEY,6),'RETAUTH')
    REPLACE TYPE WITH 'Z'
  ENDIF
ENDSCAN
IF !llNoteUsed
  USE IN NOTEPAD
ENDIF

IF !llUsed
  USE IN RETAUTH
ENDIF
RETURN