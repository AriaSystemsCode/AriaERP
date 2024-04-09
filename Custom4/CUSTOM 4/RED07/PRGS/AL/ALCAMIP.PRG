*:****************************************************************
*: Program file  : ALCAMIP.PRG
*: Program desc. : Import UPS shipments.
*: System        : Aria Apparel System - Version 4XP.
*: Module        : Alloaction (AL)
*: Developer     : Mariam Mazhar
*: Date          : 24/08/2006
*:****************************************************************
STORE '' TO lcPathName , lcImprtFil
llOpenUps = .F.

DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+"\alcamipr.SCX")

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
PARAMETERS loFormSet
PRIVATE lcOldPath

lcOldPath = FULLPATH('')
lcPathName  = GETFILE('DBF', 'UPS Shipments Path : ','Select') 
SET DEFA TO &lcOldPath
loFormSet.lcName =lcPathName

*:*************************************************************
*: Name      : lfvProceed
*: Developer : Mariam Mazhar [MMT]
*: Date      : 24/08/2006
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


llOpenUps =gfOpenFile(oAriaApplication.DataDir+'ARUPSSHP',oAriaApplication.DataDir+'ARUPSSHP','SH')
=AFIELDS(laFileStru)

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
*: Name      : lfChekFils
*: Developer : Mariam Mazhar [MMT]
*: Date      : 24/08/2006
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

*=gfOpenFile(&lcPathName ,'Arupsshp','SH',lcImprtFil)
IF !FILE(lcPathName) OR UPPER(JUSTEXT(lcPathName)) <> 'DBF'
  * "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  llReturn = .F.
  SELECT (lnalias)
  RETURN llReturn
ENDIF 

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