*:***************************************************************************
*: Program file  : POROB240.PRG
*: Program desc. : PO LC assignment report for (ROBYN MERDITH)
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Robyn Merdith
*:***************************************************************************
*C101336,1 KHM 11/30/98
*:***************************************************************************

*-- Initializing the temporary file.
PosHTemp   = gfTempName()

*-- Initializing the necessary variables that will be used in the filter
XVENDOR    = lcRpVendor
LPO        = lcRpLowPo
HPO        = lcRpHigPo
XSTATUS    = ""

*-- To get the position of the status option in the OG, then get the 
*-- contents of the selections.
lnOptionNo = ASCAN(laOgFxFlt,"POSHDR.STATUS")
IF lnOptionNo > 0
  lnStatPos  = ASUBSCRIPT(laOgFxFlt,lnOptionNo,1)
  XSTATUS    = laOgFxFlt[lnStatPos,6]
ENDIF
LENTERED   = ldLEntDate
HENTERED   = ldHEntDate
LCANCELD   = ldLComDate
HCANCELD   = ldLComDate
XDIV       = ""
*-- To get the position of the division option in the OG, then get the 
*-- contents of the selections.
lnOptionNo = ASCAN(laOgFxFlt,"POSHDR.CDIVISION")
IF lnOptionNo > 0
  lnStatPos  = ASUBSCRIPT(laOgFxFlt,lnOptionNo,1)
  XDiv    = laOgFxFlt[lnStatPos,6]
ENDIF

XPRINTNOTE = lcRpPoNote
lcTitle    = lcRpTitle
XLONG      = 'Y'
lcAssigned = lcRpAssign
lcSortBy   =lcRpSortBy

*-- Getting the sort option. Vendor,Po, or complete date.
DO CASE
 CASE lcSortBy='P'
   SORTFIELD = 'PO'
 CASE lcSortBy='V'
   SORTFIELD = 'VENDOR+PO'
 CASE lcSortBy='D'
   SORTFIELD = 'SYS(11,COMPLETE)+VENDOR+PO'
ENDCASE

SELECT POSHDR
SET ORDER TO
SET RELATION TO
SET FILTER   TO
GOTO TOP

*-- Building the Filter
XFILTER = '.T.'
XFILTER = IIF(!EMPTY(XVendor),XFILTER + ' .AND. VENDOR=XVENDOR',xFilter)

IF !EMPTY(LPO) OR !EMPTY(HPO)
  IF HPO = ' '
    HPO = LPO
  ENDIF
  XFILTER = XFILTER +'.AND.PO>=LPO.AND.PO<=HPO'
ENDIF

IF DTOC(LENTERED) <> ' ' .OR. DTOC(HENTERED) <> ' '
  IF DTOC(HENTERED)=' '
    HENTERED=LENTERED
  ENDIF
  XFILTER = XFILTER+'.AND.ENTERED>=LENTERED.AND.ENTERED<=HENTERED'
ENDIF

IF DTOC(LCANCELD)<>' '.OR.DTOC(HCANCELD)<>' '
  IF DTOC(HCANCELD)=' '
    HCANCELD=LCANCELD
  ENDIF
  XFILTER = XFILTER+'.AND.COMPLETE>=LCANCELD.AND.COMPLETE<=HCANCELD'
ENDIF

XFILTER = IIF(!EMPTY(XStatus),XFILTER+'.AND.STATUS$XSTATUS',XFILTER)

IF lcAssigned = 'Y'
  XFILTER = XFILTER+'.AND. cLcStatus="S"'
ELSE
  XFILTER = XFILTER+'.AND. !(cLcStatus$"SA") '
ENDIF

IF !EMPTY(XDIV)
  XFILTER = XFILTER+'.AND.cDIVISION=XDIV'
ENDIF


*-- Selecting the data.
SELECT POSHDR
LOCATE ALL FOR &XFILTER

IF EOF()
  =gfModalGen('TRM34130B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

COPY REST TO &gcWorkDir.&PosHTemp FOR &XFILTER

=gfOpenFile(gcWorkDir+PosHTemp," ","EX")  
IF SORTFIELD<>' '
  INDEX ON &SORTFIELD TAG &PosHTemp
  SET ORDER TO TAG &PosHTemp
ENDIF
IF XPRINTNOTE = 'Y'
  SET RELATION TO 'P'+Po INTO NotePad 
ENDIF
SET RELATION TO "S"+clcNo INTO LC ADDITIVE
SELECT &PosHTemp

R_WIDTH   = 'N'
DO gfDispRe WITH 'POROB240.FRX'
*=gfDispRep('POROB240.frx')
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 11/30/1998
*! Purpose   : To validate the vendor code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor


lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
lcRpVendor = lcVendor
&lcVenFld  = lcVendor


*!*************************************************************
*! Name      : lfvPO
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 11/30/1998
*! Purpose   : To validate the Po range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
FUNCTION lfvPO

lcObjNam = SYS(18)
lcPoNo   = EVALUATE(SYS(18))

SELECT PosHdr
SET ORDER TO PosHdr
IF lcObjNam = "LCOGVALUEF" 
  IF !EMPTY(lcPoNo) AND !SEEK('P'+lcPoNo, 'POSHDR')
    DO POSBrow WITH lcPONo,"",'P'
  ENDIF
  lcRpLowPo = lcPoNo
ELSE
  IF !EMPTY(lcPoNo) AND !SEEK('P'+lcPoNo, 'POSHDR')
    DO POSBrow WITH lcPONo,"",'P'
  ENDIF 
  lcRpHigPo = lcPoNo
ENDIF
&lcObjNam = lcPoNo

*!*************************************************************
*! Name      : lfvDate
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 11/30/1998
*! Purpose   : To validate the enter and complete dates ranges.
*!*************************************************************
*! Parameters: lcType : "E" : Entered Date
*!                      "C" : Complete Date.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvDate('E')
*!*************************************************************
FUNCTION lfvDate
PARAMETERS lcType
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)
lcObjVal = EVALUATE(lcObjNam)


IF lcObjNam = "LCOGVALUEF"
  IF lcType = "E"
    ldLEntDate = lcObjVal
  ELSE  
    ldLComDate = lcObjVal  
  ENDIF
ELSE
  IF lcType = "E"
    ldHEntDate = lcObjVal
  ELSE
    ldHComDate = lcObjVal
  ENDIF  
ENDIF  
&lcObjNam = lcObjVal
