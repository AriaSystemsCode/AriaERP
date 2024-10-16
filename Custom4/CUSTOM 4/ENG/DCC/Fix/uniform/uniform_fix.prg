CLOSE TABLES ALL 

LCA27SYSFILESPATH = GETDIR("","Select Aria27 Sysfiles Folder")
IF EMPTY(LCA27SYSFILESPATH) OR (!EMPTY(LCA27SYSFILESPATH) AND !DIRECTORY(LCA27SYSFILESPATH))
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF

lcA4SqlDicPath = GETDIR("","Select Aria4 SQL Dictionary Folder")
IF EMPTY(lcA4SqlDicPath) OR (!EMPTY(lcA4SqlDicPath) AND !DIRECTORY(lcA4SqlDicPath))
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF


IF FILE(ADDBS(LCA27SYSFILESPATH)+'SYCCOMP.DBF')
  USE ADDBS(LCA27SYSFILESPATH)+'SYCCOMP.DBF' IN 0
ELSE
  MESSAGEBOX('Selected Directory Does not Contain SYccomp Table')
  RETURN .F.
ENDIF


SELECT SYCCOMP
SCAN

  LCSTYLEDATAPATH = ALLTRIM(SYCCOMP.CCOM_DDIR)    &&GETDIR("","Select Aria27 Dbfs directory")
  LCSTYLEDATAPATH = ADDBS(LCSTYLEDATAPATH)
  IF !DIRECTORY(LCSTYLEDATAPATH)
    MESSAGEBOX('Could Not update company '+SYCCOMP.CCOMP_ID)
    LOOP
  ENDIF
  WAIT WINDOW "Updating Aria27 data Files of Company "+SYCCOMP.CCOMP_ID NOWAIT
  *USE ADDBS(lcA27SysfilesPath)+"\SYDFLFLD.DBF" SHARED IN 0
   = MAIN()
  SELECT SYCCOMP
ENDSCAN
=AFTER_LOOP()

********************************************************************************************
FUNCTION Main

  USE ADDBS(LCSTYLEDATAPATH )+"\UNIFORM.DBF" EXCLUSIVE  IN 0
  USE ADDBS(LCSTYLEDATAPATH )+"\STYHIST.DBF" EXCLUSIVE  IN 0
  
  SELECT UNIFORM
  IF TYPE('UNIFORM.Color')<>'C'
    ALTER TABLE UNIFORM ADD COLUMN COLOR C(6) 
  ENDIF   
  REPLACE COLOR WITH COLOUR ALL 
  
  SELECT STYHIST
  IF TYPE('STYHIST.Color')<>'C'
    ALTER TABLE STYHIST ADD COLUMN COLOR C(6) 
  ENDIF   
  REPLACE COLOR WITH COLOUR ALL 
  
  USE IN UNIFORM
  USE IN STYHIST
  
ENDFUNC 
***********************************************************************************************
FUNCTION AFTER_LOOP
USE ADDBS(LCA27SYSFILESPATH)+'SYDFLFLD.DBF' IN 0
*USE ADDBS(LCA27SYSFILESPATH)+'SYCNVLIB.DBF' IN 0

*!*	SELECT SYCNVLIB
*!*	SET ORDER TO SYCNVLIB   && CFILE

*!*	IF SEEK('UNIFORM   ')
*!*	  REPLACE SYCNVLIB.cfile_a26 WITH 'UNIFORM'
*!*	  REPLACE SYCNVLIB.lext_call WITH .T.
*!*	ENDIF 

*!*	IF SEEK('STYHIST   ')
*!*	  REPLACE SYCNVLIB.cfile_a26 WITH 'STYHIST'
*!*	  REPLACE SYCNVLIB.lext_call WITH .T.
*!*	ENDIF 


SELECT SYDFLFLD
SET ORDER TO CFLFLD   && CFILE_NAM+CFLD_NAME
IF SEEK('UNIFORM ')
  
  nLast = 0
  SCAN REST WHILE CFILE_NAM+CFLD_NAME ='UNIFORM '
    IF nLast < NFLD_POS
      nLast = NFLD_POS
    ENDIF   
  ENDSCAN 
  =SEEK('UNIFORM ')
  SCATTER MEMVAR memo 
  
  m.NFLD_POS  = nLast +1
  m.CFLD_NAME = 'COLOR'
  APPEND BLANK 
  GATHER MEMVAR MEMO 
ENDIF 

IF SEEK('STYHIST ')
  
  nLast = 0
  SCAN REST WHILE CFILE_NAM+CFLD_NAME ='STYHIST '
    IF nLast < NFLD_POS
      nLast = NFLD_POS
    ENDIF   
  ENDSCAN 
  =SEEK('STYHIST ')
  SCATTER MEMVAR memo 
  m.NFLD_POS  = nLast +1
  m.CFLD_NAME = 'COLOR'
    APPEND BLANK 
  GATHER MEMVAR MEMO 
ENDIF 

USE IN SYDFLFLD
*!*	USE IN SYCNVLIB

**************************************************************************
*!*	USE ADDBS(lcA4SqlDicPath )+'SYDFLFLD.DBF' IN 0
USE ADDBS(lcA4SqlDicPath )+'SYCNVLIB.DBF' IN 0

SELECT SYCNVLIB
SET ORDER TO SYCNVLIB   && CFILE

IF SEEK('UNIFORM   ')
  REPLACE SYCNVLIB.cfile_a26 WITH 'UNIFORM'
  REPLACE SYCNVLIB.lext_call WITH .T.
ENDIF 

IF SEEK('STYHIST   ')
  REPLACE SYCNVLIB.cfile_a26 WITH 'STYHIST'
  REPLACE SYCNVLIB.lext_call WITH .T.
ENDIF 


*!*	SELECT SYDFLFLD
*!*	SET ORDER TO CFLFLD   && CFILE_NAM+CFLD_NAME
*!*	IF SEEK('UNIFORM ')
*!*	  
*!*	  nLast = 0
*!*	  SCAN REST WHILE CFILE_NAM+CFLD_NAME ='UNIFORM '
*!*	    IF nLast < NFLD_POS
*!*	      nLast = NFLD_POS
*!*	    ENDIF   
*!*	  ENDSCAN 
*!*	  =SEEK('UNIFORM ')
*!*	  SCATTER MEMVAR memo 
*!*	  m.NFLD_POS  = nLast +1
*!*	  m.CFLD_NAME = 'COLOR'
*!*	    APPEND BLANK 
*!*	  GATHER MEMVAR MEMO 
*!*	ENDIF 

*!*	IF SEEK('STYHIST ')
*!*	  
*!*	  nLast = 0
*!*	  SCAN REST WHILE CFILE_NAM+CFLD_NAME ='STYHIST '
*!*	    IF nLast < NFLD_POS
*!*	      nLast = NFLD_POS
*!*	    ENDIF   
*!*	  ENDSCAN 
*!*	  =SEEK('STYHIST ')
*!*	  SCATTER MEMVAR memo 
*!*	  m.NFLD_POS  = nLast +1
*!*	  m.CFLD_NAME = 'COLOR'
*!*	    APPEND BLANK 
*!*	  GATHER MEMVAR MEMO 
*!*	ENDIF 

*!*	USE IN SYDFLFLD
USE IN SYCNVLIB

ENDFUNC 
