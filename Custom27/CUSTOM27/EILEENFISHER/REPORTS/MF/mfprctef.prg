*:***************************************************************************
*: Program file  : MFPRCTEF
*: Program desc. : Print CUTTING TICKETS for Elien Fisher
*: For Report    : MFPRCTEF.FRX,MFPRCSEF.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : RAMY MABROUK
*: Reference     : C101696,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Modifications:
*:B802967,1 AMM 02/03/2000 Fix some bugs and add an option to print certain lot#
*:B603952,1 KHM 10/11/2000 Fix the bug of alias Mfgoprhd not found.
*:***************************************************************************

*B802967,1 AMM Set the outer and inner groups of the .FRX
llUseLot = !EMPTY(lcRpLotNo)
IF llUseLot 
  lcOutGr = "CutTkt+cSeqNo+MFGOPRDT.ITEM"
  lcInGr  = "CutTkt+cSeqNo+ MFGOPRDT.ITEM +MFGOPRDT.CDYELOT"
ELSE
  lcOutGr = "CutTkt+cSeqNo+EVALUATE(lcMainF +'.Style')"
  lcInGr  = "CutTkt+cSeqNo+ EVALUATE(lcMainF + '.Style') + EVALUATE(lcMainF + '.Dyelot') "
ENDIF

STORE .F. TO llrPrtAlo,llrPrtSn,llrPrtctn
*B802967,1 AMM end

*-- Open used files
IF !USED('MFGOPRHD')
  =gfOpenFile(gcDataDir+'MFGOPRHD','MFGOPRHD','SH')
*B802967,1 AMM Set the proper index if openned
ELSE
  SET ORDER TO TAG MFGOPRHD IN MFGOPRHD
*B802967,1 AMM end
ENDIF

IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOMITEM','SH')
ENDIF

IF !USED('APVENDOR')
  =gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
ENDIF
*B802967,1 AMM Open the operation detail file and set relation
IF llUseLot
  IF !USED('MFGOPRDT')
    =gfOpenFile(gcDataDir+'MFGOPRDT','MFGOPRDT','SH')
  ENDIF
  SELECT MFGOPRHD
  SET RELATION TO 'M'+ctktno+coprcode+lcRplotno+'1' INTO MFGOPRDT
ENDIF
*B802967,1 AMM end

SELECT CUTTKTH
SET RELATION TO 'M' + CUTTKT INTO MFGOPRHD

*--Get the structre of the CUTTKTH file

=AFIELDS(laTablStr)
*--Increase the width of the warecode field
lnFieldPos = ASCAN(laTablStr , 'CWARECODE')
lnFieldPos = lnFieldPos + 2
laTablStr[lnFieldPos] = 8
*--Increase the width [end]

*--Add new filed to header file to hold the second contractor in case 
*--of the cuttkt status is open

lnTablStr = ALEN(laTablStr , 1)
DIMENSION laTablStr[lnTablStr+1,4]
laTablStr[lnTablStr+1 , 1] = 'cVend2'
laTablStr[lnTablStr+1 , 2] = 'C'
laTablStr[lnTablStr+1 , 3] = 8
laTablStr[lnTablStr+1 , 4] = 0
*--Add new filed [end]
*B802967,1 AMM Add a new field to hold the operation sequence to use it in sorting
DIMENSION laTablStr[lnTablStr+2,4]
laTablStr[lnTablStr+2 , 1] = 'cSeqno'
laTablStr[lnTablStr+2 , 2] = 'C'
laTablStr[lnTablStr+2 , 3] = 2
laTablStr[lnTablStr+2 , 4] = 0
*B802967,1 AMM end

CREATE TABLE (gcWorkDir+lcTmpMfg) FROM ARRAY laTablStr
*B802967,1 AMM add the operation sequence to the index
*INDEX ON CUTTKT TAG CUTTKT OF (gcWorkDir) + (lcTmpMfg) + '.CDX'
INDEX ON CUTTKT+CSEQNO TAG CUTTKT OF (gcWorkDir) + (lcTmpMfg) + '.CDX'
*B802967,1 AMM end

*--Get the Cut Tickets numbers from the lcRpExp if there any.
lcFilter = lcRpExp
lcFilter = STRTRAN(lcRpExp, ' .AND. ' + IIF(lcstatus='L',"Status $'CHOA'",;
  "Status = lcStatus"))
lcFilterPos = AT('INLIST' , lcFilter)
lcFilterlEN = LEN(lcFilter)
lcFilter= SUBSTR(lcFilter, lcFilterPos , lcFilterLen)
lcFilter = STRTRAN(lcFilter , 'INLIST(CUTTKTH.CUTTKT,')
lcFilter = STRTRAN(lcFilter , 'INLIST(CUTTKTH.CUTTKT,"')
lcFilter = STRTRAN(lcFilter , ')')
lcFilter = STRTRAN(lcFilter , '"')
*--Get the Cut Tickets numbers [end]

*--Collect the header records data

*--Check if we select any cuttkts from the Option Grid
IF !EMPTY(lcFilter)
  lnOccurs = OCCURS(',' , lcFilter)
  lnIncr   = 1
  *-- Loop to put the records in the temp header file
  FOR lnCount = 1 TO lnOccurs + 1
    lcSeekVal = SUBSTR(lcFilter , lnIncr , 6)
    IF SEEK(lcSeekVal , 'CUTTKTH')
      SELECT CUTTKTH
      SCATTER MEMVAR MEMO
      IF STATUS = 'O' .AND. SEEK('M' + lcSeekVal , 'MFGOPRHD')
        SELECT MFGOPRHD
        SCAN REST FOR 'M'+CTKTNO = 'M'+ lcSeekVal
          M.cVend2 = cContCode
          *B802967,1 AMM get the operation sequence in memory variable to be saved in the table
          m.cSeqNo = cOperSeq
          *B802967,1 AMM end
          SKIP 1
          IF CTKTNO = lcSeekVal
            M.cWareCode = cContCode
          ELSE
            M.cWareCode = CUTTKTH.cWareCode
          ENDIF
          SKIP -1
          INSERT INTO (lcTmpMfg) FROM MEMVAR
        ENDSCAN
      ELSE
        IF STATUS = 'H'
          M.cWareCode = ''
        ELSE
          =SEEK('M' + lcSeekVal , 'MFGOPRHD')
          M.cVend2 = MFGOPRHD.cContCode
        ENDIF
        INSERT INTO (lcTmpMfg) FROM MEMVAR
      ENDIF
    ENDIF
    lnIncr    = lnIncr + 7
  ENDFOR
ELSE && Now we didn`t select any cuttkts from the Option Grid and we will preview all 
  SELECT CUTTKTH
  
  SCAN  FOR &lcRpExp && Scan the cuttkth file to get all the cuttks in this file
    SCATTER MEMVAR MEMO
    IF STATUS = 'O' .AND. SEEK('M' + CUTTKTH.CUTTKT , 'MFGOPRHD')
      SELECT MFGOPRHD
      SCAN REST FOR 'M'+CTKTNO = 'M'+ CUTTKTH.CUTTKT
        M.cVend2 = cContCode
        *B802967,1 AMM put the operation sequence in memory variable to be saved in the table
        m.cSeqNo = cOperSeq
        *B802967,1 AMM end
        SKIP 1
        IF CTKTNO = CUTTKTH.CUTTKT
          M.cWareCode = cContCode
        ELSE
          M.cWareCode = CUTTKTH.cWareCode
        ENDIF
        SKIP -1
        INSERT INTO (lcTmpMfg) FROM MEMVAR
      ENDSCAN
    ELSE
      INSERT INTO (lcTmpMfg) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF && End of IF !EMPTY(lcFilter)
*B802967,1 AMM set the proper index to fit the relation with the header file
SET ORDER TO TAG TKTOPER IN MFGOPRHD
*B802967,1 AMM end

*B802967,1 AMM (START)
*SELECT (lcMainF)
*SET RELATION TO
*SET RELATION TO STYLE INTO STYLE ADDITIVE
*SCAN
  *IF NoteFlag # 'N'
    *REPLACE Qty1   WITH 0 ;
            Qty2   WITH 0 ;
            Qty3   WITH 0 ;
            Qty4   WITH 0 ;
            Qty5   WITH 0 ;
            Qty6   WITH 0 ;
            Qty7   WITH 0 ;
            Qty8   WITH 0 ; 
            TotQty WITH 0
  *ENDIF
  *IF SEEK('1' + Style.FABRIC , 'BOM')
    *REPLACE nYield WITH BOM.nBOMTotQty
  *ENDIF
*ENDSCAN
*B802967,1 AMM Select the proper file and set relation
IF llUseLot
  SELECT MFGOPRDT
  SET RELATION TO ITEM INTO STYLE
ELSE
  SELECT (lcMainF)
  SET RELATION TO STYLE INTO STYLE
  *-- Get the yield field value from the BOM file
  SCAN
    IF NoteFlag # 'N'
      REPLACE Qty1   WITH 0 ;
              Qty2   WITH 0 ;
              Qty3   WITH 0 ;
              Qty4   WITH 0 ;
              Qty5   WITH 0 ;
              Qty6   WITH 0 ;
              Qty7   WITH 0 ;
              Qty8   WITH 0 ; 
              TotQty WITH 0
    ELSE
      *B802967,1 AMM update the yield field in (lcMainF) table
      =lfgetYld()
    ENDIF
  ENDSCAN
ENDIF
*B802967,1 AMM end
*-- Set some relation
SELECT CUTTKTH
SET RELATION TO

SELECT (lcTmpMfg)
*B802967,1 AMM set relation to the proper file and set skip to it.
*SET RELATION TO CutTkt INTO (lcMainF) ADDITIVE
*SET SKIP TO (lcMainF)
IF llUseLot
  SET RELATION TO 'M'+CUTTKT+CSEQNO INTO MFGOPRHD
  SET SKIP TO MFGOPRHD,MFGOPRDT
  SET FILTER TO !EOF('MFGOPRDT')
ELSE
  SET RELATION TO CutTkt INTO (lcMainF) ADDITIVE
  SET SKIP TO (lcMainF)
ENDIF
*B802967,1 AMM end


*--End of Program code


*!*************************************************************
*! Name      : lfClsFiles
*! Developer : RAMY MABROUK
*! Date      : 10/25/1999
*! Purpose   : Close the created cursors
*!*************************************************************
*! Called from : MFPRCTMX.FRX
*!*************************************************************
*! Calls       : gfCodDes()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClsFiles()
*!*************************************************************

FUNCTION lfClsFiles

PARAMETER lcDummy

IF USED(lcTmpMfg)
  USE IN (lcTmpMfg)
ENDIF

IF USED('BOM')
  SET ORDER TO
  USE IN BOM
ENDIF

*B603952,1 KHM 10/11/2000 (Begin) Commenting the following line as the file was opened in the
*B603952,1                option grid.
*IF USED('MFGOPRHD')
*  SET ORDER TO
*  USE IN MFGOPRHD
*ENDIF
*B603952,1 KHM 10/11/2000 (End)

*B802967,1 AMM Close the file
IF USED('MFGOPRDT')
  SET ORDER TO
  USE IN MFGOPRDT
ENDIF
*B802967,1 AMM end

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : RAMY MABROUK
*! Date      : 11/29/1999
*! Purpose   : Get the ship to addresses and the contractor addresses
*!*************************************************************
*! Called from : MFPRCTEF.FRX
*!*************************************************************
*! Calls       : gfGetAdr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************

FUNCTION lfGetCodes

PARAMETER lcDummy

DO CASE

  CASE EVALUATE(lcTmpMfg+ '.Status')='O'
    lcSt = 'OPEN'

  CASE EVALUATE(lcTmpMfg+ '.Status')='H'
    lcSt = 'HOLD'

  CASE EVALUATE(lcTmpMfg+ '.Status')='A'
    lcSt = 'ACTUAL'

  CASE EVALUATE(lcTmpMfg+ '.Status')='C'
    lcSt = 'COMPLETED'

ENDCASE
*-- Get the addresses
IF SEEK(ALLTRIM(&lcTmpMfg..cwarecode),'WAREHOUS')
  lcShpName   = WAREHOUS.cDesc
  laShpAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laShpAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laShpAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laShpAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laShpAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  DO lfShiftArr WITH laShpAdr
ELSE
  IF SEEK(ALLTRIM(&lcTmpMfg..cwarecode),'APVENDOR')
    lcShpName   = APVENDOR.cVenComp
    laShpAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
    laShpAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
    laShpAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
    laShpAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
    laShpAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
    DO lfShiftArr WITH laVenAdr
  ELSE
    STORE SPACE(0) TO lcShpName , laShpAdr
  ENDIF
ENDIF

IF !EMPTY(&lcTmpMfg..cVend2) .AND. SEEK(ALLTRIM(&lcTmpMfg..cVend2),'APVENDOR')
  lcVenName   = APVENDOR.cVenComp
  laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
  DO lfShiftArr WITH laVenAdr
ELSE
  STORE SPACE(0) TO lcVenName , laVenAdr
ENDIF
*B802967,1 AMM Set it to false
llEndGroup = .F.
*B802967,1 AMM end

*!*************************************************************
*! Name      : lfGetColr
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/03/2000
*! Purpose   : to get the color segment in the style code structure
*! Reference : *B802967,1 AMM 
*!*************************************************************
*! Called from : MFPRCTEF.FRX
*!*************************************************************
*! Calls       : gfItemMask()
*!*************************************************************
*! Passed Parameters : lcReturn
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetColr()
*!*************************************************************
FUNCTION lfGetColr
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
PARAMETERS lcreturn
DIMENSION laMajSeg[1,4]

= gfItemMask(@laMajSeg)
FOR lnC = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnC,1] = 'C'
      lnCStrt = laMajSeg[lnC,4]
      lnCLengt = LEN(laMajSeg[lnC,3])
      EXIT
  ENDIF
ENDFOR
lcreturn = SPACE(0)


*!*************************************************************
*! Name      : lfGetLngD
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/03/2000
*! Purpose   : to get the color long description.
*! Reference : *B802967,1 AMM 
*!*************************************************************
*! Called from : MFPRCTEF.FRX
*!*************************************************************
*! Calls       : gfRltFld()
*!*************************************************************
*! Passed Parameters : lcReturn
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLngD()
*!*************************************************************
FUNCTION lfGetLngD
PARAMETERS lcReturn
DIMENSION laClrLName[1,2]

laClrLName[1,1] = 'CLRLNAME'      && Array to get the Division long name
laClrLName[1,2] = 'lcReturn'
lcColor = SUBSTR(EVAL(lcMainF+'.Style'),lnCStrt,lnCLengt)
=gfRltFld(lcColor, @laClrLName , 'COLOR')

*!*************************************************************
*! Name      : lfvLotNo
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/03/2000
*! Purpose   : Valid function of the LOT# setting in the option grid
*! Reference : *B802967,1 AMM 
*!*************************************************************
*! Called from : MFPRCTEF.FRX
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : llRetVal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvLotNo()
*!*************************************************************
FUNCTION lfvLotNo
PARAMETERS llRetVal
llRetVal = .T.
STORE .F. TO llrPrtAlo,llrPrtSn,llrPrtctn
*-- Disable or enable the print allocation setting in the option grid
lnPos = ASCAN(laOgObjType,'llrPrtAlo')
IF lnPos > 0
  lnPos = ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrPrtAlo'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpLotNo)
  = lfOGShowGet('llrPrtAlo')
ENDIF
*-- Disable or enable the print Style notes setting in the option grid
lnPos = ASCAN(laOgObjType,'llrPrtSn') 
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrPrtSn'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpLotNo)
  = lfOGShowGet('llrPrtSn')
ENDIF
*-- Disable or enable the print CT notes setting in the option grid
lnPos = ASCAN(laOgObjType,'llrPrtctn') 
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrPrtctn'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpLotNo)
  = lfOGShowGet('llrPrtctn')
ENDIF

*!*************************************************************
*! Name      : lfgetYld
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/03/2000
*! Purpose   : to get the yield value
*! Reference : *B802967,1 AMM 
*!*************************************************************
*! Called from : MFPRCTEF.FRX, MFPRCTEF.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lnRetVal,llCFrmFRX
*!*************************************************************
*! Return      : Yeild value
*!*************************************************************
*! Example     : = lfgetYld()
*!*************************************************************
FUNCTION lfgetYld
PARAMETERS lnRetVal,llCFrmFRX
lnRetVal = 0
lcColor = SUBSTR(Style.Style,lnCStrt,lnCLengt)
IF SEEK('1' + PADR(Style.FABRIC,19)+lcColor+LEFT(STYLE.STYLE,lnMajLen) , 'BOM')
  IF llCFrmFRX
    lnRetVal = BOM.nBOMTotQty
  ELSE
    REPLACE nYield WITH BOM.nBOMTotQty
  ENDIF
ELSE
  IF SEEK('1' + PADR(Style.FABRIC,19)+REPLICATE('*',6)+LEFT(STYLE.STYLE,lnMajLen) , 'BOM')
    IF llCFrmFRX
      lnRetVal = BOM.nBOMTotQty
    ELSE
      REPLACE nYield WITH BOM.nBOMTotQty
    ENDIF
  ENDIF
ENDIF

