*:***************************************************************************
*: Program file  : POFORCST
*: Program desc. : P/O Forecast        C#123140             
*: For Report    : TIBI.
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POFORCST
*:***************************************************************************
*: Modifications:
*:***************************************************************************
*
*-- SELECT REPORT FILE & INITIALIZE MEMORY
=gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT poshdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO POSHDR.cSTYType+Poshdr.PO INTO POSLN ADDITIVE
=lfRecreate()
lnMajorLen = LEN(gfItemMask("PM"))

WAIT 'Selecting records for report ...' WINDOW NOWAIT
SELECT POSHDR
=lfGetData()
SELECT (WORKFILE)
LOCATE
IF EOF()
  =gfDialog('I',"No records to display")
  RETURN
ENDIF
=lfPrint()

*!*************************************************************
*! Name      : lfvPO
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/27/1998
*! Purpose   : validate purchase order
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
*C#123140
FUNCTION lfvPO
PRIVATE lnCurrAlis
lnCurrAlis = ALIAS()
lcPOFld = VARREAD()

lcPONo = &lcPOFld
SELECT POSHDR
*--mhm2004
*SET FILTER TO STATUS = "O"
SET FILTER TO STATUS $ "OH"
*--mhm2004
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK("P"+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"","P"
ENDIF

&lcPOFld = lcPONo
SELECT (lnCurrAlis)
*!*************************************************************
*! Name      : lfvVendor
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
*C#123140
FUNCTION lfvVendor

lcVenFld = VARREAD()

lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfRecreate
*! Developer : Mohamed Shokry (MMH)
*! Date      : 04/09/2003
*! Purpose   : Create temp. File
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfRecreate()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C#123140
FUNCTION lfRecreate

DIMENSION laTempStru[1,4]
SELECT POSHDR
= AFIELDS(laTempStru)
lnArrLen = ALEN(laTempStru,1)
DIMENSION laTempStru[lnArrLen + 21, 4]

*-- cSeekStyle     :  field save style to seek it in style file.
laTempStru[lnArrLen+1  ,1] = 'cFabDesc'
laTempStru[lnArrLen+1  ,2] = 'C'
laTempStru[lnArrLen+1  ,3] = 20
laTempStru[lnArrLen+1  ,4] = 0

laTempStru[lnArrLen+2  ,1] = 'cSeason'
laTempStru[lnArrLen+2  ,2] = 'C'
laTempStru[lnArrLen+2  ,3] = 20
laTempStru[lnArrLen+2  ,4] = 0

laTempStru[lnArrLen+3  ,1] = 'cMatVen'
laTempStru[lnArrLen+3  ,2] = 'C'
laTempStru[lnArrLen+3  ,3] = 8
laTempStru[lnArrLen+3  ,4] = 0

laTempStru[lnArrLen+4  ,1] = 'dFabPo'
laTempStru[lnArrLen+4  ,2] = 'D'
laTempStru[lnArrLen+4  ,3] = 8
laTempStru[lnArrLen+4  ,4] = 0

laTempStru[lnArrLen+5  ,1] = 'nFabAmnt'
laTempStru[lnArrLen+5  ,2] = 'N'
laTempStru[lnArrLen+5  ,3] = 10
laTempStru[lnArrLen+5  ,4] = 2

laTempStru[lnArrLen+6  ,1] = 'cFabLC'
laTempStru[lnArrLen+6  ,2] = 'C'
laTempStru[lnArrLen+6  ,3] = 18
laTempStru[lnArrLen+6  ,4] = 0

laTempStru[lnArrLen+7  ,1] = 'dFabRelPo'
laTempStru[lnArrLen+7  ,2] = 'D'
laTempStru[lnArrLen+7  ,3] = 8
laTempStru[lnArrLen+7  ,4] = 0

laTempStru[lnArrLen+8  ,1] = 'dConfPo'
laTempStru[lnArrLen+8  ,2] = 'D'
laTempStru[lnArrLen+8  ,3] = 8
laTempStru[lnArrLen+8  ,4] = 0

laTempStru[lnArrLen+9  ,1] = 'dStyPoLc'
laTempStru[lnArrLen+9  ,2] = 'D'
laTempStru[lnArrLen+9  ,3] = 8
laTempStru[lnArrLen+9  ,4] = 0

laTempStru[lnArrLen+10  ,1] = 'nStyPoLc'
laTempStru[lnArrLen+10  ,2] = 'N'
laTempStru[lnArrLen+10  ,3] = 10
laTempStru[lnArrLen+10  ,4] = 2

laTempStru[lnArrLen+11  ,1] = 'dGMTLC'
laTempStru[lnArrLen+11  ,2] = 'D'
laTempStru[lnArrLen+11  ,3] = 8
laTempStru[lnArrLen+11  ,4] = 0

laTempStru[lnArrLen+12  ,1] = 'nWrPyMnt'
laTempStru[lnArrLen+12  ,2] = 'N'
laTempStru[lnArrLen+12  ,3] = 10
laTempStru[lnArrLen+12  ,4] = 2

laTempStru[lnArrLen+13  ,1] = 'dQuotWir'
laTempStru[lnArrLen+13  ,2] = 'D'
laTempStru[lnArrLen+13  ,3] = 8
laTempStru[lnArrLen+13  ,4] = 0

laTempStru[lnArrLen+14  ,1] = 'dFrghtWir'
laTempStru[lnArrLen+14  ,2] = 'D'
laTempStru[lnArrLen+14  ,3] = 8
laTempStru[lnArrLen+14  ,4] = 0

laTempStru[lnArrLen+15  ,1] = 'dDutyWir'
laTempStru[lnArrLen+15  ,2] = 'D'
laTempStru[lnArrLen+15  ,3] = 8
laTempStru[lnArrLen+15  ,4] = 0

laTempStru[lnArrLen+16  ,1] = 'nCommAmt'
laTempStru[lnArrLen+16  ,2] = 'N'
laTempStru[lnArrLen+16  ,3] = 12
laTempStru[lnArrLen+16  ,4] = 2

laTempStru[lnArrLen+17  ,1] = 'dCommAmt'
laTempStru[lnArrLen+17  ,2] = 'D'
laTempStru[lnArrLen+17  ,3] = 8
laTempStru[lnArrLen+17  ,4] = 0

laTempStru[lnArrLen+18  ,1] = 'nTotalAmt'
laTempStru[lnArrLen+18  ,2] = 'N'
laTempStru[lnArrLen+18  ,3] = 14
laTempStru[lnArrLen+18  ,4] = 2

laTempStru[lnArrLen+19  ,1] = 'nQouta'
laTempStru[lnArrLen+19  ,2] = 'N'
laTempStru[lnArrLen+19  ,3] = 12
laTempStru[lnArrLen+19  ,4] = 2

laTempStru[lnArrLen+20  ,1] = 'nFreight'
laTempStru[lnArrLen+20  ,2] = 'N'
laTempStru[lnArrLen+20  ,3] = 12
laTempStru[lnArrLen+20  ,4] = 2

laTempStru[lnArrLen+21  ,1] = 'nDuty'
laTempStru[lnArrLen+21  ,2] = 'N'
laTempStru[lnArrLen+21  ,3] = 12
laTempStru[lnArrLen+21  ,4] = 2

*CREATE CURSOR (WORKFILE) ;
*   FROM ARRAY laTempStru
CREATE DBF (gcWorkDir+WORKFILE)FROM ARRAY laTempStru

INDEX ON cSeason TAG (WORKFILE)


*!*************************************************************
*! Name      : lfGetData
*! Developer : Mohamed Shokry (MMH)
*! Date      : 04/09/2003
*! Purpose   : Get data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetData()
*!*************************************************************
*! Note      : 
*!*************************************************************
*C#123140
FUNCTION lfGetData
PRIVATE lcRecNo , lnFabAmt,lnCurrAlis
lnCurrAlis = ALIAS()

SELECT (WORKFILE)
SCATTER MEMVAR MEMO
lnFabAmt = 0
SELECT POSHDR   
SCAN FOR &lcRpExp
*--mhm2004
  *IF POSHDR.STATUS<> "O"
  IF !(POSHDR.STATUS $ "OH")
*--mhm2004
    LOOP
  ENDIF
  
  lcRecNo  = RECNO()
  SCATTER MEMVAR MEMO
  =lfMaData()
  =lfPoData()
  IF EMPTY(m.cGmtType)
    m.cGmtType = "CMTQ"
  ENDIF
  INSERT INTO (WORKFILE) FROM MEMVAR
  IF lcRecNo  > 0 .AND. lcRecNo   <= RECCOUNT()
    GOTO lcRecNo  
  ENDIF
ENDSCAN
SELECT (lnCurrAlis)

*!*************************************************************
*! Name      : lfPoData
*! Developer : Mohamed Shokry (MMH)
*! Date      : 04/09/2003
*! Purpose   : Get Po needed data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPoData()
*!*************************************************************
*! Note      : 
*!*************************************************************
*C#123140
FUNCTION lfPoData
PRIVATE lnCurrAlis
lnCurrAlis = ALIAS()

*--confirmation date
m.dConfPo   = POSHDR.Complete - 45
m.dGMTLC    = POSHDR.Complete - 5
m.dQuotWir  = POSHDR.Complete - 60
m.dFrghtWir = POSHDR.Complete + 14
m.dDutyWir  = POSHDR.Complete + 10
m.dCommAmt  = POSHDR.Complete + 30

IF !EMPTY(cStyLC)
  m.dStyPoLc = POSHDR.Complete - 30
ELSE
  m.dStyPoLc  =  {}
ENDIF

*--get style Season if single or multi
lcSeasn = Style.Season
llchckSesn = .F.
SELECT POSLN
lcPoSRecNo = RECNO()
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cstytype+PosHdr.po
  IF lcSeasn <> Style.Season
    m.cSeason = "Multi"
    llchckSesn = .T.
    EXIT
  ENDIF
ENDSCAN
IF lcPoSRecNo  > 0 .AND. lcPoSRecNo   <= RECCOUNT()
  GOTO lcPoSRecNo  
ENDIF

IF !llchckSesn
  m.cSeason  = gfCodDes(lcSeasn  , 'SEASON')
ENDIF  

*----get open data
SELECT POSLN
lcPoSRecNo = RECNO()
lnTotAmt     = 0
lnTotQty     = 0
lnTotQut     = 0
lnTotFrght   = 0
lnTotComm    = 0
lnTotDuty    = 0
lnQutaCst    = 0
lnFrtCst     = 0
lnCommCst    = 0
lnDutyCst    = 0
lnPoPrice    = 0
lcCurSty     = ''
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cstytype+PosHdr.po
  IF TRANCD = '3'
    LOOP
  ENDIF

  IF lcCurSty <> Style
    lcCurSty = Style
    lnTotQty = IIF( lnTotQty > 0 , lnTotQty , 0)
    lnTotAmt   = lnTotAmt + lnTotQty * lnPoPrice
    lnTotQut   = lnTotQut + lnTotQty * lnQutaCst 
    lnTotFrght = lnTotFrght + lnTotQty * lnFrtCst 
    lnTotComm  = lnTotComm  + lnTotQty * lnCommCst 
    lnTotDuty  = lnTotDuty  + lnTotQty * lnDutyCst
    lnTotQty   = 0
    lnPoPrice  = 0
  ENDIF
  IF PosHdr.Status = 'O'
    IF TRANCD = '1'
      *--Get Qouta element from Manf Operation from BomLine file
      IF SEEK("I"+"1"+PO+STR(lineno,6)+"4"+style+SPACE(6)+SPACE(19)+SPACE(6)+"QUO","BOMLINE")
        lnQutaCst = BOMLINE.UnitCost
      ENDIF

      *--Get Freight element from Manf Operation from BomLine file
      IF SEEK("I"+"1"+PO+STR(lineno,6)+"4"+style+SPACE(6)+SPACE(19)+SPACE(6)+"FRT","BOMLINE")
        lnFrtCst = BOMLINE.UnitCost
      ENDIF
    
      *--Get Commission element from Manf Operation from BomLine file
      IF SEEK("I"+"1"+PO+STR(lineno,6)+"4"+style+SPACE(6)+SPACE(19)+SPACE(6)+"08","BOMLINE")
        lnCommCst = BOMLINE.UnitCost
      ENDIF

      *--Get Duty element from BomLine file
      IF SEEK("I"+"1"+PO+STR(lineno,6)+"2"+style,"BOMLINE")
        lnDutyCst = BOMLINE.UnitCost
      ENDIF

      lnTotQty   = lnTotQty + TotQty
      lnPoPrice  = nCost1
    ELSE
      lnTotQty   = lnTotQty - TotQty
      lnPoPrice  = nCost1
    ENDIF
  ELSE
    IF PosHdr.Status = 'H'
      IF TRANCD = '1'
        *--Get Qouta element from Manf Operation from Bom file
                  
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMajorLen),19,' ')+"4"+PADR(Style,lnMajorLen)+'-'+'******'+"QUO","BOM")
          lnQutaCst = BOM.UntCost
        ENDIF

        *--Get Freight element from Manf Operation from Bom file
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMajorLen),19,' ')+"4"+PADR(Style,lnMajorLen)+'-'+'******'+"FRT","BOM")
          lnFrtCst = BOM.UntCost
        ENDIF
    
        *--Get Commission element from Manf Operation from Bom file
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMajorLen),19,' ')+"4"+PADR(Style,lnMajorLen)+'-'+'******'+"08","BOM")
          lnCommCst = BOM.UntCost
        ENDIF

        *--Get Duty element from Bom file
        IF SEEK(PADR(SUBSTR(STYLE,1,lnMajorLen),19,' ')+"5"+PADR(Style,lnMajorLen)+'-'+'******',"BOM")
          lnDutyCst = BOM.UntCost
        ENDIF

        lnTotQty   = lnTotQty + TotQty
        lnPoPrice  = nCost1
      ELSE
        lnTotQty   = lnTotQty - TotQty
        lnPoPrice  = nCost1
      ENDIF
    ENDIF  
  ENDIF  
ENDSCAN

IF lcCurSty <> Style
  lnTotAmt   = lnTotAmt   + lnTotQty * lnPoPrice
  lnTotQut   = lnTotQut   + lnTotQty * lnQutaCst 
  lnTotFrght = lnTotFrght + lnTotQty * lnFrtCst 
  lnTotComm  = lnTotComm  + lnTotQty * lnCommCst 
  lnTotDuty  = lnTotDuty  + lnTotQty * lnDutyCst
ENDIF

IF !EMPTY(cStyLC)
  m.nStyPoLc = lnTotAmt
  m.nWrPymnt = 0
ELSE
  m.nStyPoLc = 0
  m.nWrPymnt = lnTotAmt
ENDIF
m.nQouta    = lnTotQut
m.nFreight  = lnTotFrght
m.nCommAmt  = lnTotComm
m.nDuty     = lnTotDuty  
m.nTotalAmt = lnFabAmt + m.nStyPoLc +m.nWrPymnt+m.nQouta +m.nFreight +m.nCommAmt+m.nDuty

IF lcPoSRecNo  > 0 .AND. lcPoSRecNo   <= RECCOUNT()
  GOTO lcPoSRecNo  
ENDIF

SELECT (lnCurrAlis)

*!*************************************************************
*! Name      : lfMaData
*! Developer : Mohamed Shokry (MMH)
*! Date      : 04/09/2003
*! Purpose   : Get Material data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfMaData()
*!*************************************************************
*! Note      : 
*!*************************************************************
*C#123140
FUNCTION lfMaData
PRIVATE lnCurrAlis
lnCurrAlis = ALIAS()

IF SEEK(STYLE.FABRIC,'FABRIC')
  m.cFabDesc = FABRIC.Desc
ELSE
  m.cFabDesc = ''
ENDIF
  
IF SEEK("P"+POSHDR.cFabPo,'POFHDR')
  m.cMatVen = POFHDR.Vendor
  m.dFabPo  = POFHDR.Entered
  m.cFabLC  = POFHDR.cMatLc
  IF !EMPTY(POFHDR.cMatLc)
    m.dFabRelPo = POSHDR.complete - 30
  ELSE
    *--to get No of days in payment terms
    lnTrmDay = 0
    DECLARE laTrmDay[1,2]
    laTrmDay[1,1] = 'NTERDUED  '     && Array to get the terms day
    laTrmDay[1,2] = 'lnTrmDay'
    = gfRltFld(POSHDR.CTERMCODE , @laTrmDay , 'CTERMCODE ')  && Get the division long name.
    m.dFabRelPo = POSHDR.complete + lnTrmDay
  ENDIF
  SELECT POFLN
  lcPoFRecNo = RECNO()
  lnTotFab  = 0
  lnFabPric = 0
  IF SEEK("P"+POFHDR.PoMat+Style.fabric)
    SCAN REST WHILE cmattype+pomat+fabric+color+trancd = "P"+POFHDR.PoMat+Fabric.fabric
      IF trancd ='1'
        lnTotFab = nfabtotqty+lnTotFab
        lnFabPric = nCost1
      ELSE
        lnTotFab = lnTotFab - nfabtotqty
      ENDIF  
    ENDSCAN
    IF lcPoFRecNo  > 0 .AND. lcPoFRecNo   <= RECCOUNT()
      GOTO lcPoFRecNo  
    ENDIF
  ENDIF
  m.nFabAmnt = lnTotFab * lnFabPric
ELSE
  m.nFabAmnt = 0
  m.cMatVen  = "None"
  m.dFabPo   = {}
ENDIF
lnFabAmt = m.nFabAmnt

SELECT (lnCurrAlis)

****************************************************************************
*! PROG      : lfPrint.PRG   (C#123140)
*! DESC      : Function to Print To EXCEl Sheet
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 08/08/2004
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrint

lcRpDtDir  = gcDataDir  && Save The Data Directory.
lcRpWrDir  = gcWorkDir  && Save The Work Directory.
lcRpSysDir = gcSysHome  && Save The system  Directory.
lcRpRpDir  = gcRepHome  && Save The report Directory.
lcRpComp   = gcAct_Comp && Save The Active Company.
lcRpDefPth = gcDef_Path && Save The DefPath.
lcRWorkFil = WorkFile  && Save The TempName .

lcRpDefPth = ALLTRIM(lcRpDefPth)
lcSorCop = ALLTRIM(lcRpDefPth) +'POFORCST.XLS'

PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') ALL LIKE l?R* 

lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

SELECT (lcRWorkFil)
USE  

IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'PO\'+[POFORVFP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

RETURN

