SET TALK OFF
SET SAFETY OFF

CLOSE ALL
CLEAR ALL
RELEASE ALL
*---------------------------------------------------------------
STORE "" TO CSource_Folder, CTarget_Folder
CSource_Folder = GETDIR('','Select Source Folder')
CTarget_Folder = GETDIR('','Select Target Folder')
*---------------------------------------------------------------
IF EMPTY(CSource_Folder)
*  CSource_Folder = "V:\aria27\"
ENDIF 
IF EMPTY(CTarget_Folder)
  CTarget_Folder = "Z:\DISTRO27\SPLIT VISUAL\WORKSTATION\SOURCE\"
ENDIF 

IF !(!EMPTY(CSource_Folder) AND DIRECTORY(CSource_Folder))
  WAIT WIND 'Sarget folder '+ CSource_Folder + ' not found'
  RETURN
ENDIF

IF !(!EMPTY(CTarget_Folder) AND DIRECTORY(CTarget_Folder))
  WAIT WIND 'Target folder '+ CTarget_Folder + ' not found'
  RETURN
ENDIF


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
  WAIT WINDOW ARR(I)+'Module' NOWAIT

  IF !DIRECTORY(CTarget_Folder+ARR(I))
    MD (CTarget_Folder+ARR(I))
  ENDIF

  SCAN
    IF ALLTRIM(MOD) = ARR(I)

      lcfolder = ALLTRIM(folder)+'\'
      lcfolder = ALLTRIM(STRTRAN(lcfolder,'Z:\DISTRO27\SPLIT VISUAL\WORKSTATION\SOURCE\',SPACE(1)))
      lcfolder = ALLTRIM(STRTRAN( lcfolder,'SHARED\',SPACE(1)))
      IF LEN(lcfolder) > 3
        lcfolder = SUBSTR(lcfolder,4,LEN(lcfolder))
      ENDIF

      lcSPCFolder = ALLTRIM(folder)+'\'
      lcSPCFolder = STRTRAN(lcSPCFolder,"Z:\DISTRO27\SPLIT VISUAL\WORKSTATION\SOURCE\",CTarget_Folder)

      lcfile = ALLTRIM(filenm)

      WAIT WIND 'Copy '+(CSource_Folder + lcfolder + lcfile) NOWAIT
      IF FILE(CSource_Folder + lcfolder + lcfile )
        IF !DIRECTORY(lcSPCFolder) THEN
          MD (lcSPCFolder)
        ENDIF
        COPY FILE ( CSource_Folder + lcfolder + lcfile ) TO ( lcSPCFolder + lcfile )
      ELSE
        IF FILE(CSource_Folder + lcfolder+ARR(I)+'\'+ lcfile )
          IF !DIRECTORY(lcSPCFolder) THEN
            MD (lcSPCFolder)
          ENDIF
          COPY FILE ( CSource_Folder + lcfolder +ARR(I)+'\'+ lcfile ) TO ( lcSPCFolder + lcfile )
        ELSE

          IF FILE(CSource_Folder + lcfolder+'99\'+ lcfile )
            IF !DIRECTORY(lcSPCFolder) THEN
              MD (lcSPCFolder)
            ENDIF
            COPY FILE ( CSource_Folder + lcfolder +'99\'+ lcfile ) TO ( lcSPCFolder + lcfile )
          ELSE
          IF FILE(CSource_Folder + lcfolder+'\SY'+ SUBSTR(lcfile,3,LEN(lcfile)) )
            IF !DIRECTORY(lcSPCFolder) THEN
              MD (lcSPCFolder)
            ENDIF
            COPY FILE (CSource_Folder + lcfolder+'\SY'+ SUBSTR(lcfile,3,LEN(lcfile)) ) TO ( lcSPCFolder + lcfile)
            *--------------------------------------------------------------------------------------
            *Handel menu file case
            IF 'CMENU.DBF' $ lcfile
              USE (lcSPCFolder + lcfile) EXCLUSIVE IN 0 ALIAS CMENU
              DELETE FOR (CAPP_ID <> 'EB') AND !(CAPP_ID = 'SY' AND ALLTRIM(CPROSS_ID) == 'EB') IN CMENU
              PACK IN CMENU
              USE IN CMENU
            ENDIF
            *--------------------------------------------------------------------------------------
            *--------------------------------------------------------------------------------------
            *Handel application file case
            IF 'DAPPL.DBF' $ lcfile
              USE (lcSPCFolder + lcfile) EXCLUSIVE IN 0 ALIAS DAPPL
              DELETE FOR (CAPP_ID <> lcSPCFolder)  IN DAPPL
              PACK IN DAPPL
              USE IN DAPPL
            ENDIF
            *--------------------------------------------------------------------------------------            
          ELSE


            WAIT WIND 'Copy '+(CSource_Folder + lcfolder+ARR(I)+'\' + lcfile) + 'ERROR !!!!!!!!' NOWAIT
            SCATTER MEMVAR MEMO
            SELECT STD_Error
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDIF
        ENDIF          
          
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDFOR
*Copy visual labels files
COPY FILE CSource_Folder+"REPORTS\AS\*.FR*" TO (CTarget_Folder+'AS\REPORTS\*.*')


***************************************************************************************************

WAIT WIND 'Done'
