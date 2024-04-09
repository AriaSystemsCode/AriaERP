*E037351,1 HBG 23/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*E037351,1                Aria27 and records belong to Aria4.
PARAMETER lcSysPath
PRIVATE llDelete, llClsFrmd, llClsRpvr, llClsRprt

STORE .F. TO llClsFrmd, llClsRpvr, llClsRprt

WAIT WINDOW 'Fixing report variables file. Please wait' NOWAIT

IF !USED('SYFRMCDD')
  SELECT 0
  USE (lcSysPath+'SYFRMCDD') ORDER TAG UpgrdLvl
  llClsFrmd = .T.
ENDIF

IF !USED('SYREPUVR')
  SELECT 0
  USE (lcSysPath+'SYREPUVR') ORDER TAG Fld_Name
  llClsRpvr = .T.
ENDIF

IF !USED('SYDREPRT')
  SELECT 0
  USE (lcSysPath+'SYDREPRT') ORDER TAG CRep_ID
  llClsRprt = .T.
ENDIF

SCAN FOR cUpgrdLvl = 'A'
  llDelete = .F.
  lcRep_ID = cRep_ID
  IF SEEK(lcRep_ID,'SYREPUVR')
    SELECT SYREPUVR
    *E037351,1 HBG 23/12/2003 Locate for records belong to Aria27 only [Begin]
    *LOCATE REST WHILE cRep_ID = lcRep_ID FOR cUpgrdLvl = 'U'
    LOCATE REST WHILE cRep_ID = lcRep_ID FOR (cUpgrdLvl = 'U' AND (EMPTY(CVER) OR CVER = 'A27'))
    *E037351,1 [End]
    IF FOUND()
      llDelete = .T.
      *Renee
      *IF SEEK('U'+lcRep_ID, 'SYFRMCDD')
      IF SEEK('U'+ALLTRIM(lcRep_ID), 'SYFRMCDD')
      *Renee end
        llDelete = .F.
      ENDIF
    ENDIF
  ENDIF
  IF llDelete
    WAIT WINDOW 'Fixing report variables file. Please wait' NOWAIT
    SELECT SYREPUVR
    =SEEK(lcRep_ID)
    *E037351,1 HBG 23/12/2003 Delete records belong to Aria27 only [Begin]
    *DELETE REST WHILE cRep_ID = lcRep_ID FOR cUpgrdLvl = 'U'
    DELETE REST WHILE cRep_ID = lcRep_ID FOR (cUpgrdLvl = 'U' AND (EMPTY(CVER) OR CVER = 'A27'))
    *E037351,1 [End]
  ENDIF
  SELECT SYDREPRT
ENDSCAN

SELECT SYREPUVR
*E037351,1 HBG 23/12/2003 Delete records belong to Aria27 only [Begin]
*DELETE ALL FOR !SEEK(cRep_ID,'SYDREPRT') AND cUpgrdLvl = 'U'
DELETE ALL FOR !SEEK(cRep_ID,'SYDREPRT') AND cUpgrdLvl = 'U' AND (EMPTY(CVER) OR CVER = 'A27')
*E037351,1 [End]


IF llClsFrmd
  USE IN SYFRMCDD
ENDIF

IF llClsRpvr
  USE IN SYREPUVR
ENDIF

IF llClsRprt
  USE IN SYDREPRT
ENDIF

WAIT CLEAR