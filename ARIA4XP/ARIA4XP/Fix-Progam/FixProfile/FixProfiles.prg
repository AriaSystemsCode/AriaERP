SET CPDIALOG OFF 
SET RESOURCE OFF 
_SCREen.Visible = .F. 
lcA27SysfilesPath = GETDIR("","Select Aria27 Sysfiles Folder")
IF EMPTY(lcA27SysfilesPath) OR (!EMPTY(lcA27SysfilesPath) AND !DIRECTORY(lcA27SysfilesPath))
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF 
lcA27SysfilesPath = ADDBS(lcA27SysfilesPath)
USE lcA27SysfilesPath + 'SYCCOMP.DBF' SHARED 
SELECT SYCCOMP
SCAN FOR LRUNFROMA4
  lcDbfsFolder = ADDBS(syCComp.ccom_ddir)
  IF FILE(lcDbfsFolder +'PROFVALU.DBF')
    WAIT WINDOW 'Fixing Profile table of company:'+SYCCOMP.CCOMP_ID NOWAIT 
    USE lcDbfsFolder +'PROFVALU.DBF' SHARED IN 0
    SELECT 'PROFVALU'
    SET ORDER TO 
    *PROFILE   && CPRO_TYPE+CKEY+CPRO_CODE
    *=SEEK('SOO')
    SCAN FOR CPRO_TYPE ='SO'
    ** Oxxxxxxddddddnnnnnn
      IF SUBSTR(CKEY,1,1) = 'O' AND LEN(ALLTRIM(ckey)) = 19
        lcOrderNumber = SUBSTR(CKEY,2,6)
        lcLineNo      = SUBSTR(CKEY,8,6)
        lcDesignNO    = SUBSTR(CKEY,14,6)
        REPLACE cKey WITH "O"+lcOrderNumber+lcLineNo
      ENDIF  
    ENDSCAN
    USE IN 'PROFVALU'
  ENDIF
  SELECT SYCCOMP
ENDSCAN
MESSAGEBOX("Profile Values table is Fixed")