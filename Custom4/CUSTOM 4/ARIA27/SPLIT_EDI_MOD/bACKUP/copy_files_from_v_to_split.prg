*CTarget_Folder = "D:\SOURCE\"
CTarget_Folder = GETDIR('','Select Target Folder')
IF !(!EMPTY(CTarget_Folder) AND DIRECTORY(CTarget_Folder))
  WAIT WIND 'Target folder '+ CTarget_Folder + ' not found'
  RETURN
ENDIF
SET TALK OFF
SET SAFETY OFF

CLOSE ALL
CLEAR ALL
RELEASE ALL

DIMENSION ARR(4)
ARR(1) = 'EB'
ARR(2) = 'AS' 
ARR(3) = 'NC'
ARR(4) = 'UP'
***********************************************************************************************
USE STD_Error.DBF EXCLUSIVE
ZAP

USE FINAL.DBF IN 0
SELECT FINAL

*EB**********************************************************************************************

FOR I = 1 TO 4
WAIT WINDOW ARR(I)+'Module' nowait

IF !DIRECTORY(CTarget_Folder+ARR(I))
MD (CTarget_Folder+ARR(I))
ENDIF

SCAN 
IF ALLTRIM(MOD) = ARR(I)

  lcfolder = ALLTRIM(folder)+'\'
  lcfolder = ALLTRIM(STRTRAN(lcfolder,'Z:\DISTRO27\Split Visual\WorkStation\Source\',SPACE(1)))
  lcfolder = ALLTRIM(STRTRAN( lcfolder,'SHARED\',SPACE(1)))
  IF LEN(lcfolder) > 3
    lcfolder = SUBSTR(lcfolder,4,LEN(lcfolder))
  ENDIF

  lcSPCFolder = ALLTRIM(folder)+'\'
  lcSPCFolder = STRTRAN(lcSPCFolder,"Z:\DISTRO27\Split Visual\WorkStation\Source\",CTarget_Folder)


  lcfile = ALLTRIM(filenm)

  WAIT WIND 'Copy '+("V:\aria27_edi_9\" + lcfolder + lcfile) NOWAIT
  IF FILE("V:\aria27_edi_9\" + lcfolder + lcfile )
     IF !DIRECTORY(lcSPCFolder) THEN
       MD (lcSPCFolder)
     ENDIF    
    COPY FILE ( "V:\aria27_edi_9\" + lcfolder + lcfile ) TO ( lcSPCFolder + lcfile )
  ELSE
    IF FILE("V:\aria27_edi_9\" + lcfolder+ARR(I)+'\'+ lcfile )
      IF !DIRECTORY(lcSPCFolder) THEN
        MD (lcSPCFolder)
      ENDIF  
      COPY FILE ( "V:\aria27_edi_9\" + lcfolder +ARR(I)+'\'+ lcfile ) TO ( lcSPCFolder + lcfile )
    ELSE
      
      WAIT WIND 'Copy '+("V:\aria27_edi_9\" + lcfolder+ARR(I)+'\' + lcfile) + 'ERROR !!!!!!!!' NOWAIT
      SCATTER MEMVAR MEMO
      SELECT STD_Error
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ENDIF
 ENDIF
ENDSCAN
ENDFOR
***************************************************************************************************

WAIT WIND 'Done'
