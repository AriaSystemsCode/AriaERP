*:-----------------------------------------------------------------------------
*: Program file        : POSTYPEE
*: Program desc.       : PO form for customer AIS10
*: For Report          : POSTYPEE.FRX
*:                       POSTYPE1.FRX
*:                       POSTYPE2.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Purchase Order (PO)
*: Developer           : RAMY MABROUK (RAMY)
*: Tracking Job Number : C101958,1
*: Date                : September/2000
*:-----------------------------------------------------------------------------
*: Calls :
*:    Procedures : ....
*:    Functions  : gfTempname()
*:                 gfDispRe()
*:                 gfItemMask()
*:                 gfOpenFile()
*:-----------------------------------------------------------------------------
*: Called From         : POSTYP.PRG
*:-----------------------------------------------------------------------------
*: Passed Parameters  : None
*:-----------------------------------------------------------------------------
*: Example :
*:-----------------------------------------------------------------------------
*: Modification        :
*:-----------------------------------------------------------------------------

llFrst     = .T.
llLastpage = .F.
lnPage     = 1

SELECT POSHDR
SET RELATION OFF INTO Apvendor
SET RELATION OFF INTO &TMPObjlink
SET RELATION OFF INTO POSLN

COPY STRUCTURE TO (gcWorkDir + lcTmpPoHd)
=gfOpenFile(gcWorkDir + lcTmpPoHd , '' , 'EX')
SELECT (lcTmpPoHd)
INDEX ON cStyType + PO TAG (lcTmpPoHd) OF (gcWorkDir+lcTmpPoHd+".CDX")
SET RELATION TO &lcTmpPoHd..vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P' + &lcTmpPoHd..PO INTO &TMPObjlink ADDITIVE
SET RELATION TO lcRpForm + PO INTO POSLN ADDITIVE
SET SKIP TO POSLN
=gfOpenFile(gcDataDir + 'STYLEUPC' , 'STYLEUPC')
SELECT POSLN


*--New
DIMENSION laArray[1,2]
laArray[1,1] = 'CNRFCODE'
laArray[1,2] = 'lcCode'
lcCode = ""

lcOrder = ORDER()
INDEX ON cStyType+PO+STR(LineNo,6) TAG (lcLinOrd) OF (gcWorkDir+lcLinOrd+".CDX")
*SET RELATION TO STYLE INTO STYLEUPC ADDITIVE

lcFltExp = lcRpExp 
SELECT POSHDR

SCAN FOR &lcRpExp
  *--Prepare the Temp file record to print the first report (The PO Header)
  SCATTER MEMVAR MEMO
  SELECT (lcTmpPoHd)
  ZAP
  INSERT INTO (lcTmpPoHd) FROM MEMVAR
  lnLineNo = 0
  lcFormName = "POSTYPEE"
  SELECT POSLN
  
  *SET SKIP TO
  COUNT REST WHILE cStyType + PO = lcRpForm+&lcTmpPoHd..PO FOR TRANCD = '1' TO lnLineNo
  *SET SKIP TO STYLEUPC
  lcNotes    = lfGetNotes()
  lnPages = CEILING((MEMLINES(lcNotes) + (lnLineNo * 3))/27) +1
  *lnPages = CEILING((MEMLINES(lcNotes) + (EVALUATE(lcTmpPoHd + '.LastLine') * 3))/33) +1
  *khm
  lnTotLins = (lnLineNo * 3) + MEMLINES(lcNotes)
  
  =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  SELECT (lcTmpPoHd)
  SET SKIP TO POSLN
  DO gfDispRe WITH EVAL('lcFormName')
  
  *--Print the the back of the first page
  =lfPrnSAY()

  *--Prepare the Temp file record to print the second report (The PO Lines)
  lcFormName = "POSTYPE1"
  =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  SELECT (lcTmpPoHd)
  DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcFltExp

  *--Prepare the Temp file record to print the Third report (The Hang Tag Form) 
  lcFormName = "POSTYPE2"
  =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  SELECT POSLN

  *--Print the Form tow times to change the Fotter
  llFrst = .T.
  =lfCollect()
  GO TOP IN (lcTmpPoHd)
  SELECT (lcTmpUPC)
  GOTO TOP
  IF !EOF()
    FOR lnI = 1 TO 2
      SELECT (lcTmpUPC)
      GOTO TOP    
      DO gfDispRe WITH EVAL('lcFormName')
      llFrst = .F.
    ENDFOR
  ENDIF  
  GO lnLstLn in POSLN
ENDSCAN

*--Delete the temp index and restore the orignal index
SELECT POSLN
CLOSE INDEX
ERASE (gcWorkDir + lcLinOrd + ".CDX")
SET ORDER TO (lcOrder)
SET RELATION TO



llrPrtCs = .F.
SELECT STYLEUPC
USE

*--Delete the temp header file

SELECT (lcTmpPoHd)
SET RELATION TO
USE
ERASE (gcWorkDir + lcTmpPoHd + ".DBF")
ERASE (gcWorkDir + lcTmpPoHd + ".CDX")

*!----------------------------------------------------------------------------
*! Name      : lfGetCodes
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : To get the some codes descreption
*!----------------------------------------------------------------------------
*! Calls       : gfCodDes()
*!               gfGetAdr()
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfGetCodes()
*!----------------------------------------------------------------------------

FUNCTION lfGetCodes

PARAMETER lcDummy

lnAlias = SELECT(0)
=gfRltFld(&lcTmpPoHd..cDivision , @laDivLName , 'CDIVISION')
llEndGroup  = .F.
lcDivDesc   = gfCodDes(&lcTmpPoHd..cDIVISION, 'CDIVISION')
lcShipVia   = gfCodDes(&lcTmpPoHd..ShipVia , 'SHIPVIA')
lcTerms     = gfCodDes(&lcTmpPoHd..CTERMCODE, 'CTERMCODE')

lcFabGroup = gfCodDes(&lcTmpPoHd..CPURCODE, 'CPURCODE')

IF &lcTmpPoHd..cStyType # 'N'
  lcVenName   = APVENDOR.CVenComp
  lcFax       = APVENDOR.CFAXNO
  laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
  =SEEK(LEFT(&lcTmpPoHd..Vendor,6),'WAREHOUS')
  lcVenName   = WAREHOUS.cDesc
  lcFax       = WAREHOUS.cFAX
  laVenAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF

SELECT (lnAlias)
RETURN ''


*!----------------------------------------------------------------------------
*! Name      : lfCheck
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Check if we are in the last page or not
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfCheck()
*!----------------------------------------------------------------------------

FUNCTION lfCheck
PARAMETERS lcReturn , llWhat

lcReturn = ''
IF llWhat
  llCheck = .T.
ELSE
  llCheck = .F.
ENDIF


*!----------------------------------------------------------------------------
*! Name      : lfGetNotes
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Get the PO notes
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfGetNotes()
*!----------------------------------------------------------------------------

FUNCTION lfGetNotes
PRIVATE lnAlias
lcNotes = ''
lnAlias = SELECT(0)

*-- If we are to print both notes.
IF llRpPrtPn .AND. SEEK('P' + &lcTmpPoHd..PO , 'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcNotes = ALLTRIM(NotePad.mNotes)

ENDIF
SELECT (lnAlias)
RETURN lcNotes

*!----------------------------------------------------------------------------
*! Name      : lfPrnSAY
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Print the notepad on the back of the first page
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfPrnSAY()
*!----------------------------------------------------------------------------

FUNCTION lfPrnSAY

IF gcDevice = 'PRINTER'
  lcDevice = SET('DEVICE')
  lcPrinter = SET('PRINTER',1)
  SET PRINTER TO (gcWorkDir + lcOutFile + '.txt')
  SET DEVICE TO PRINTER
  *--This line was added for the teting only Please Keep it
  *@ 0,0 SAY CHR(27) + '&l1O'
  @ 0,0 SAY CHR(27) + '&l1S'
  @ 2,2 SAY lcNotTerms
  SET PRINTER TO &lcPrinter
  SET DEVICE TO &lcDevice
  COPY FILE (gcWorkDir + lcOutFile + '.txt') TO LPT1
ENDIF

*!----------------------------------------------------------------------------
*! Name      : lfGetMemo
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Restore from the memo file that holds the user notes to be printed 
*!             on the back of the first page
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfGetMemo()
*!----------------------------------------------------------------------------

FUNCTION lfGetMemo

PARAMETER lcDummy

IF FILE(gcRepHome + 'POTerms.mem')
  RESTORE FROM (gcRepHome + 'POTerms.mem') ADDIT
ELSE
  DO (gcRepHome + 'POEE1.spx')
ENDIF

*!----------------------------------------------------------------------------
*! Name      : lfvMsgSave
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Valid function the the Save button on the notes screen
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfvMsgSave()
*!----------------------------------------------------------------------------

FUNCTION lfVMsgSave

PARAMETER lcDummy

SAVE TO (gcRepHome + 'POTerms.mem') ALL LIKE lcNotTerms
CLEAR READ


*!----------------------------------------------------------------------------
*! Name      : lfCollect
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Function to collect the style UPCs in temp file
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfCollect()
*!----------------------------------------------------------------------------

FUNCTION lfCollect

SELECT (lcTmpPoHd)
GO TOP
SELECT (lcTmpUPC)
ZAP

lnTotals = 0

SELECT POSLN
SET RELATION TO STYLE INTO STYLEUPC ADDITIVE
SET SKIP TO STYLEUPC
SCAN REST WHILE cStyType + PO + STR(LineNo,6) = lcRpForm + &lcTmpPoHd..PO FOR TRANCD = '1'
  SELECT STYLEUPC
  lnTotals = lnTotals + 1
  SCAN REST WHILE Style = POSLN.Style
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpUPC) FROM MEMVAR
    SELECT (lcTmpUPC)
    REPLACE LineNo   WITH POSLN.LineNo ;
            Color    WITH SUBSTR(POSLN.Style,lnMajSize+2,lnNonMaj);
            SizeDesc WITH EVALUATE('SCALE.SZ' + ALLTRIM(STYLEUPC.SIZE));
            Qty      WITH EVALUATE('POSLN.Qty' + ALLTRIM(STYLEUPC.SIZE))
            
  ENDSCAN 
  SELECT POSLN
ENDSCAN

SELECT POSLN
SET RELATION OFF INTO STYLEUPC

lnPages = CEILING(RECCOUNT(lcTmpUpc)/ 32)

*!----------------------------------------------------------------------------
*! Name      : lfBuldTmp
*! Developer : RAMY MABROUK (RAM)
*! Date      : 09/25/2000
*! Purpose   : Function to bulid the temp file that we use in the Hang tag form
*!----------------------------------------------------------------------------
*! Calls       :
*!----------------------------------------------------------------------------
*! Passed Parameters : None.
*!----------------------------------------------------------------------------
*! Return      : None
*!----------------------------------------------------------------------------
*! Example     : = lfBuldTmp()
*!----------------------------------------------------------------------------

FUNCTION lfBuldTmp

PARAMETER lcDummy

=gfOpenFile(gcDataDir + 'STYLEUPC' , 'STYLEUPC')
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru + 1 , 1] = 'LineNo'
laFileStru[lnFileStru + 1, 2] = 'N'
laFileStru[lnFileStru + 1 , 3] = 6
laFileStru[lnFileStru + 1 , 4] = 0

DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru + 2 , 1] = 'Color'
laFileStru[lnFileStru + 2 , 2] = 'C'
laFileStru[lnFileStru + 2 , 3] = 6
laFileStru[lnFileStru + 2 , 4] = 0

DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru + 3 , 1] = 'SizeDesc'
laFileStru[lnFileStru + 3 , 2] = 'C'
laFileStru[lnFileStru + 3 , 3] = 5
laFileStru[lnFileStru + 3 , 4] = 0

DIMENSION laFileStru[lnFileStru+4,4]
laFileStru[lnFileStru + 4 , 1] = 'Qty'
laFileStru[lnFileStru + 4 , 2] = 'N'
laFileStru[lnFileStru + 4 , 3] = 6
laFileStru[lnFileStru + 4 , 4] = 0

CREATE TABLE (gcWorkDir + lcTmpUPC) FROM ARRAY laFileStru
INDEX ON STR(LineNo,6) TAG (lcTmpUPC) OF (gcWorkDir + lcTmpUPC + '.CDX')


SELECT STYLEUPC
USE