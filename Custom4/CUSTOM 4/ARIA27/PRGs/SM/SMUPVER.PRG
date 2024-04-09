*:************************************************************************
*: Program file  : SMUPVER.PRG
*: Program desc. : Update all files With A27
*: For screen    : NONE
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: SM
*:      Developer: Abdou Elgendy
*:           Date: 01/09/2003
*:************************************************************************
*: Passed Parameters : none
*:************************************************************************
* Run alone from Dotwin.	
*:************************************************************************
*:
PARAMETER lcDataDir

PRIVATE llSysF ,llUseF ,lnRecN ,lnrecn1 ,llUSeF1 ,lcFToUpd

llUSeF1=.F.
llUseF =.F.
STORE 1 TO lnRecN ,lnrecn1
IF TYPE('lcDataDir') <> 'C'
  lcDataDir = GETDIR('','Select data directory')
ENDIF

IF EMPTY(lcDataDir)
  RETURN
ENDIF  

SELECT 0
IF ! USED ('SYDFILES')
  USE(gcSysHome+'sydfiles.dbf') in 0
  llUseF =.T.
ELSE
  lnRecN =RECNO('SYDFILES')
  llUseF =.F.
ENDIF
 SELECT sydfiles
LOCATE
SCAN
  lcFToUpd= ALLTRIM(sydfiles.cFile_Nam)
  llSYSF = 'SM' $ sydfiles.mFile_app
  WAIT WINDOW 'Update File ' + cFile_ttl NOWAIT
  *-- Check is the file exist.
  IF FILE(IIF(llsysf,gcSysHome,lcDataDir)+lcFToUpd+'.DBF')
    
    IF USED(lcFToUpd)
      SELECT(lcFToUpd)
      
      lnrecn1=RECNO(lcFToUpd)
       llUSeF1 =.F.
    ELSE
      SELECT 0
      USE(IIF(llsysf,gcSysHome,lcDataDir)+lcFToUpd) SHARED
      llUSeF1 =.T.
    ENDIF
    LOCATE
    *-- Check on the Filed.
    IF TYPE('CADD_VER') = "C"
     SCAN
       IF EMPTY(CADD_VER)
         REPLACE CADD_VER WITH 'A27'
       ENDIF
       
       IF EMPTY(CEDT_VER)  
         REPLACE  CEDT_VER WITH 'A27'
       ENDIF
     ENDSCAN
    ENDIF
    
    IF llUSeF1
      USE IN (lcFToUpd)
    ELSE 
      
      IF BETWEEN(lnrecn1,1,RECCOUNT(lcFToUpd))
        GOTO lnrecn1 IN  (lcFToUpd)
      ENDIF
    ENDIF   
  ENDIF
ENDSCAN

IF llUseF 
  USE IN SYDFILES
ELSE
  IF BETWEEN(lnRecN ,1,RECCOUNT('SYDFILES'))
    GOTO  lnRecN  in  SYDFILES
  ENDIF
ENDIF

*-- END CODE
*:*************************************************************
