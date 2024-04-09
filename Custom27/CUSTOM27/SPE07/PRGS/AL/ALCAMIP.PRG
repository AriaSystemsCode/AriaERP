*:****************************************************************
*: Program file  : SMCAMUPS.PRG
*: Program desc. : Import UPS shipments.
*: For screen    : SMCAMUPS.SPR
*: System        : Aria Apparel System - Version 2.7.
*: Module        : System Manager (SM)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 01/01/2003
*: Tracking Job Number : C#102873,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#102783,1.
*:****************************************************************
*:Modifications  :
*:B607304,1 ABD 06/29/2003 Don't let the user to select the source 
*:B607304,1 ABD            file as a target file.
*:****************************************************************
*:
*-- lcPathName :- Variable hold the import path.
*-- lcImprtFil : temp name for the import file.
STORE '' TO lcPathName , lcImprtFil

llOpenUps = .F.
llOpenUps =gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
=AFIELDS(laFileStru)


PUSH KEY
DO (gcScrDir+gcWinAppl+"\ALCAMIPR.SPX")
POP KEY

IF llOpenUps
  =gfCloseFile('ARUPSSHP')
ENDIF

*-- End Of Code.
*:*************************************************************
*: Name      : lfvProceed
*: Developer : Abdou Elgendy
*: Date      : 01/01/2003
*: Purpose   : Function to Import the Ups shipments file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : = lfOpenFiles ()
*:*************************************************************
*:
FUNCTION lfvProceed
PRIVATE lcPiktkt

*-- FUNCTION to check if the 2 files has the same Structure.
IF !lfChekFils ()
  RETURN
ENDIF


SELECT ARUPSSHP
LOCATE
lcPiktkt = ''
SCAN FOR !EMPTY(Piktkt)
  SCATTER MEMVAR MEMO
  WAIT WINDOW 'Piktkt # ' + Piktkt NOWAIT
  SELECT lcImprtFil
  IF SEEK(M.Piktkt) .AND. lcPiktkt # PIKTKT
    DELETE REST WHILE Piktkt = M.Piktkt
  ENDIF
  APPEND BLANK
  GATHER MEMVAR MEMO
  SELECT ARUPSSHP
  lcPiktkt= M.Piktkt
ENDSCAN

IF USED('lcImprtFil')
  USE IN lcImprtFil
ENDIF


=gfModalGen('TRM42219B00000','DIALOG','Import UPS Shipments')

RETURN
*-- End OF lfvProceed
*:*************************************************************
*: Name      : lfvgetFile
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/01/2003
*: Purpose   : Function to get the Excel file data Dir & File.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvgetFile ()
*:*************************************************************
*:
FUNCTION lfvgetFile
PRIVATE lcOldPath

lcOldPath = FULLPATH('')
*- Abdou
PRIVATE lcSelctUps
lcSelctUps =''

*B607304,1 ABD - Don't let the user to select the source file as a target file. [Begin]
*lcPathName  = GETFILE('DBF', 'UPS Shipments Path : ','Select') 
*SET DEFA TO &lcOldPath

*IF EMPTY(lcPathName)
*  SHOW GET PbProceed DISABLE
*  lcPathName = ''
*ELSE
*  SHOW GET PbProceed ENABLE
*ENDIF
*SHOW GET lcPathName

llSelUPS = .F.

DO WHILE !llSelUPS
  lcPathName  = GETFILE('DBF', 'UPS Shipments Path : ','Select') 
  
  IF EMPTY(lcPathName)
    IF gfModalGen('QRM00000B32000','F','ALERT',' ','Are you sure you do not want to select the UPS shipment file?') = 1
      EXIT
    ENDIF
  ELSE
    lcSelctUps = UPPER(lcPathName)
    IF lcSelctUps = UPPER(gcDataDir)+'ARUPSSHP.DBF'
      = gfModalGen('QRM00000B00000','F','ALERT',' ','You have selected the target file by mistake. Please select the source file which you will get the information from.')
    ELSE  
      STORE .T. TO llSelUPS
    ENDIF
  ENDIF  
ENDDO
SHOW GET lcPathName

*B607304,1 ABD - [End]

*-- End Of lfvgetFile
*:*************************************************************
*: Name      : lfChekFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/01/2003
*: Purpose   : To check if the files has the same structure.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfChekFils ()
*:*************************************************************
*:
FUNCTION lfChekFils
Private llReturn , lnalias
llReturn = .T.

lnalias = SELECT(0)
SELECT 0
USE &lcPathName SHARED ALIAS  lcImprtFil ORDER Arupsshp

=AFIELDS(laCopyStru)

IF ALEN(laCopyStru,1) = ALEN(laFileStru,1)
  FOR I = 1 To ALEn(laFileStru,1)
    IF !(laCopyStru[I,1] = laFileStru[I,1] .AND. laCopyStru[I,2] = laFileStru[I,2] .AND.;
         laCopyStru[I,3] = laFileStru[I,3] .AND. laCopyStru[I,4] = laFileStru[I,4])
      llReturn = .F.
      EXIT
    ENDIF
  ENDFOR
  
ELSE
  llReturn = .F.
ENDIF

IF !llReturn
  *-- message : different files.
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,'The Import Ups shipments files has an invalid format, Can not proceed')
  SELECT lcImprtFil
  USE
ENDIF

SELECT (lnalias)
RETURN llReturn

*-- End OF lfChekFils
*:*************************************************************