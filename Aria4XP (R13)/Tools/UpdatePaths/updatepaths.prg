SET CPDIALOG OFF
SET DELETED ON

_SCReen.Visible = .F. 
lcNewDataPath = ADDBS(FULLPATH(''))
lcNewSysPath = lcNewDataPath+'SysFiles\'
lcDataFolder = lcNewDataPath+'DBFs\'
IF !DIRECTORY(lcNewSysPath)
  MESSAGEBOX('SysFiles folder is missing. cannot continue')
  RETURN .F.
ENDIF
IF !DIRECTORY(lcDataFolder)
  MESSAGEBOX('DBFs folder is missing. cannot continue')
  RETURN .F.
ENDIF


USE (lcNewSysPath+'SYCCOMP.DBF') SHARED IN 0
SELECT SYCCOMP
LOCATE 
SCAN FOR !DELETED()
  lcOldDataDir = ALLTRIM(syccomp.ccom_ddir)
  IF RIGHT(lcOldDataDir,1) = '\'
    lcOldDataDir = SUBSTR(lcOldDataDir,1,LEN(lcOldDataDir)-1)
  ENDIF
  lcCoDBFFolder = JUSTSTEM(lcOldDataDir)
  IF DIRECTORY(lcDataFolder+lcCoDBFFolder)
    REPLACE ccom_ddir WITH ADDBS(lcDataFolder+lcCoDBFFolder)
    lcNewFolderCo = ADDBS(lcDataFolder+lcCoDBFFolder)
    IF FILE(lcNewFolderCo+'Objects.DBF') AND DIRECTORY(lcNewFolderCo+'OBJECTS')
      USE (lcNewFolderCo+'Objects.DBF') IN 0 SHARED 
      lcNewObjectsPath = lcNewFolderCo+'OBJECTS\'
      SELECT Objects
      SCAN FOR !DELETED() AND (!EMPTY(ALLTRIM(objects.mimgpath)) OR !EMPTY(ALLTRIM(objects.cimgpath)))
        IF !EMPTY(ALLTRIM(objects.cimgpath))
          IF FILE(lcNewObjectsPath+JUSTFNAME(ALLTRIM(objects.cimgpath)))
            REPLACE objects.cimgpath WITH lcNewObjectsPath+JUSTFNAME(ALLTRIM(objects.cimgpath))
          ENDIF  
        ENDIF
        IF !EMPTY(ALLTRIM(objects.mimgpath))
          IF FILE(lcNewObjectsPath+JUSTFNAME(ALLTRIM(objects.mimgpath)))
            REPLACE objects.mimgpath WITH lcNewObjectsPath+JUSTFNAME(ALLTRIM(objects.mimgpath))
          ENDIF  
        ENDIF 
  	ENDSCAN 		
  	USE IN Objects
    ENDIF
  ENDIF  
ENDSCAN 
USE IN SYCCOMP