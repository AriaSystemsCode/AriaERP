*:************************************************************************
*: Program file  : CONVERT.PRG
*: Program desc. : Program To convert excel tabel to Data Base tabel.
*: System        : ARIA BUSINESS SYSTEM
*: Module        : Inventory control Module
*: Developer     : Hend Ghanem (HBG)
*: Date          : 05/09/2000
*: Refer to      : C101963
*:************************************************************************

PRIVATE lcTempUpc ,lcRejReas ,lcFileName ,lcCancel ,lcProceed 

IF ALLTRIM(gfGetMemVar('M_UPC_USE')) <> 'Y'
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"System doesn't use UPC")
  RETURN
ENDIF

DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'CUPCMAN'
laDRltFld[1,2] = 'lcDivUCC'

STORE "" TO lcFileName ,lcTempUpc,lcRejReas
 
lcTempUpc  = gfTempName()
lcRejReas  = gfTempName()

lcCancel  = gcBmpHome + "Can.bmp"
lcProceed = gcBmpHome + "proceed.bmp"

*-- Creat temp files
=lfCreatFle()

*--Open the needed files
=lfOpenFile()

DO (gcScrDir+gcWinAppl+"\ICCONUPC.SPX")

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Open files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************
*
FUNCTION lfOpenFile

=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'Styupcn','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'UPCSEQ',gcDataDir+'UPCSEQ','SH')

*-- end of lfOpenFile

*!*************************************************************
*! Name      : lfCreatFle
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Creat temp files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCreatFle()
*!*************************************************************
*
FUNCTION lfCreatFle

CREATE TABLE (gcWorkDir+lcTempUpc) (Division C(30), Season C(30), Style C(12), COlor C(6), Size C(10), UPC C(13),lValid L)
INDEX ON STyle+Color+Size TAG (lcTempUpc)

CREATE TABLE (gcDataDir+lcRejReas) (Type C(1),Key C(30),Reason C(30))
INDEX ON Type+Key TAG (lcRejReas)


*--End of lfCreatFle


*!*************************************************************
*! Name      : lfvGetFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Get the file to be convert
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGetFile()
*!*************************************************************
*
FUNCTION lfvGetFile

lcFileName = GETFILE('XLS','Select File to be convert','Convert')
=lfRefresh('ICCONUPC')
*--End of lfvGetFile


*!*************************************************************
*! Name      : lfvProceed
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Get the file to be convert
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProceed()
*!*************************************************************
*
FUNCTION lfvProceed
PRIVATE llError ,llValid ,llAtDiv 
PRIVATE lcStyClr ,lcCompUcc ,lcReasone ,lcDivUCC ,lcUccMan ,lcSize ,;
        lcCount ,lcCupcnum1 ,lcCupcnum2 ,lcCupcnum3 ,lnCount,lcOnError

llError = .F.
lcOnError = ON('ERROR')
ON ERROR llError = .T.
SELECT (lcTempUpc)
WAIT 'Prepare data. Please wait...' WINDOW NOWAIT
APPEND FROM &lcFileName TYPE XLS
WAIT CLEAR
IF llError
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invaled Excel file or invalid format. Make sure that you select excel file format 4.0,3.0, or 2.1")
  lcFileName = ""
  =lfRefresh('ICCONUPC')
  RETURN
ENDIF
ON ERROR &lcOnError

lcCompUcc = ALLTRIM(gfGetMemVar('XMANUFID'))
llAtDiv  = ALLTRIM(gfGetMemVar('M_UCCDIV'))='N'

IF SEEK(PADR("STYLE",12)+PADR("COLOR",6)+PADR("SIZE",10))
  BLANK
  DELETE
ELSE
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invaled Excel file. Make sure that you select The UPC Excel file")
  lcFileName = ""
  =lfRefresh('ICCONUPC')
  RETURN
ENDIF  

GO TOP
DO WHILE !EOF()
  lcStyClr = Style+Color
  llValid = .T.
  IF !SEEK('N'+PADR(LEFT(Division,2),6)+'N'+'CDIVISION','Codes')
    llValid = .F.
    IF !SEEK('D'+Division,lcRejReas)
      INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ;
                           ('D',&lcTempUpc..Division,'Invalid Division')
    ENDIF
  ENDIF
  IF !SEEK('N'+PADR(LEFT(Season,2),6)+'N'+'SEASON','Codes')
    llValid = .F.
    IF !SEEK('N'+Season,lcRejReas)
      INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ;
                           ('N',&lcTempUpc..Season,'Invalid Season')
    ENDIF
  ENDIF
  IF !SEEK('N'+PADR(Color,6)+'N'+'COLOR','Codes')
    llValid = .F.
    IF !SEEK('C'+Color,lcRejReas)
      INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ;
                           ('C',&lcTempUpc..Color,'Invalid Color')
    ENDIF
  ENDIF
  IF !SEEK(PADR(Style,12)+'-'+PADR(Color,6),'STYLE')
    llValid = .F.
    IF !SEEK('S'+PADR(Style,12)+'-'+PADR(Color,6),lcRejReas)
      INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ;
     ('S',PADR(&lcTempUpc..Style,12)+'-'+PADR(&lcTempUpc..Color,6),'Invalid Style/Color')
    ENDIF
  ENDIF
  =SEEK('S'+STYLE.scale,'SCALE')
  lcDivUCC = ''
  IF llAtDiv
    =gfRltFld(PADR(LEFT(Division,2),6),@laDRltFld,'CDIVISION')
  ENDIF  
  lcUccMan = PADR(IIF(llAtDiv,lcDivUCC,lcCompUcc),6)

  SCAN REST WHILE Style+Color+Size = lcStyClr
    WAIT 'Style/Color/Size: '+ALLTRIM(Style)+'/'+ALLTRIM(Color)+'/'+ALLTRIM(Size) WINDOW NOWAIT
    IF LEFT(UPC,6) <> lcUccMan
      llValid = .F.
      IF !SEEK('M'+LEFT(UPC,6),lcRejReas)
        INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ;
                             ('M',LEFT(&lcTempUpc..UPC,6),'Invalid UCC manufacturer ID')
      ENDIF
    ENDIF
    lcSize = SPACE(1)
    FOR lnCount = 1 TO Scale.cnt
      lcCount = STR(lnCount,1)
      *IF ALLTRIM(Size) = ALLTRIM(Scale.Sz&lcCount)
      IF PADR(ALLTRIM(Size),5) = PADR(Scale.Sz&lcCount,5)
        lcSize = lcCount
        Exit
      ENDIF
    ENDFOR
    IF EMPTY(lcSize)
      llValid = .F.
      IF !SEEK('Z'+&lcTempUpc..Size,lcRejReas)
        INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ('Z',&lcTempUpc..Size,'Invalid Size')
      ENDIF
    ENDIF
    IF SEEK(UPC,'STYLEUPC')
      llValid = .F.
      IF !SEEK('U'+UPC,lcRejReas)
        INSERT INTO (lcRejReas) (Type,Key,Reason ) VALUES ('U',&lcTempUpc..UPC,'UPC# already exists')
      ENDIF
    ENDIF
    IF llValid
      lcCupcnum1 = SUBSTR(UPC,1,6)
      lcCupcnum2 = SUBSTR(UPC,7,5) 
      lcCupcnum3 = SUBSTR(UPC,12)
      INSERT INTO STYLEUPC (Style,Size,cupcnum1,cupcnum2,cupcnum3) VALUES ;
                (PADR(&lcTempUpc..Style,12)+'-'+PADR(&lcTempUpc..Color,6),lcSize,lcCupcnum1,lcCupcnum2,lcCupcnum3)
      IF !SEEK(lcCupcnum1,'UPCSEQ')
        INSERT INTO UPCSEQ (cUpcMan) VALUES (lcCupcnum1)
      ENDIF
      SELECT UPCSEQ
      REPLACE cUpcLast WITH ALLTRIM(STR(MAX(VAL(lcCupcnum2),VAL(cUpcLast))))
    ENDIF
    SELECT (lcTempUpc)
    REPLACE lValid WITH llValid
  ENDSCAN
ENDDO
SELECT (lcTempUpc)
DELETE ALL FOR lValid
PACK
GO TOP
IF !EOF()
  lcFIle = LEFT(lcFileName,RAT('\',lcFileName))+'RjectUpc.XLS'
  IF FILE(lcFIle)
    ERASE (lcFile)
  ENDIF
  EXPORT TO (LEFT(lcFileName,RAT('\',lcFileName))+'RjectUpc.XLS') FIELDS Division , Season,Style,Color,Size,UPC  TYPE XLS
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Rejected UPCs have been reported and stored in "+lcFile)
ENDIF
SELECT (lcRejReas)
GO TOP
IF !EOF()
  lcFIle = LEFT(lcFileName,RAT('\',lcFileName))+'RjReason.XLS'
  IF FILE(lcFIle)
    ERASE (lcFile)
  ENDIF
  EXPORT TO (LEFT(lcFileName,RAT('\',lcFileName))+'RjReason.XLS') FIELDS Reason,Key  TYPE XLS
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Rejection Reasons have been reported and stored in "+lcFile)
ELSE
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'UPCs have been converted successfully.')
ENDIF
USE IN (lcRejReas)
ERASE (gcDataDir+lcRejReas+'.*')
USE IN (lcTempUpc)
ERASE (gcDataDir+lcTempUpc+'.*')
WAIT CLEAR
CLEAR READ