PARAMETERS lnGenSrc
*!*	lcDefaultDir = SET("Default")
LOCAL lcSQLDictionaryDir
DO CASE
  CASE lnGenSrc =1
    lcSQLDictionaryDir= "R:\Aria4XP\SQLDictionary\"
  CASE lnGenSrc =2
    lcSQLDictionaryDir= "R:\A4XPDemo\Aria4XP\SQLDictionary\"
  CASE lnGenSrc =3
    lcSQLDictionaryDir= "V:\Aria4XP\SQLDictionary\"
ENDCASE

lcSAFETY=SET("Safety")
SET SAFETY OFF
lnFilesCount=ADIR(laFiles,lcSQLDictionaryDir+"SY*.DBF")
FOR lnFile =1 TO lnFilesCount
  lcFile=laFiles[lnFile,1]
  lcXXFile=STUFF(UPPER(lcFile), 1,2,'XX')
  lcFile = lcSQLDictionaryDir + lcFile 
  lcXXFile = lcSQLDictionaryDir +lcXXFile 
  *COPY FILE (lcFile) TO STUFF(UPPER(lcFile), 1,2,'XX')
  COPY FILE (lcFile) TO (lcXXFile)
  lcFile=FORCEEXT(lcFile, "CDX")
  IF FILE(lcFile)
    COPY FILE (lcFile) TO FORCEEXT(lcXXFile, "CDX")
  ENDIF
  lcFile=FORCEEXT(lcFile, "FPT")
  IF FILE(lcFile)
    COPY FILE (lcFile) TO FORCEEXT(lcXXFile, "FPT")
  ENDIF
ENDFOR

SET SAFETY &lcSAFETY

*!*	WAIT WINDOW "SQL Dictionary files generated successfully" NOWAIT
MESSAGEBOX("SQL Dictionary files generated successfully",0,"")
