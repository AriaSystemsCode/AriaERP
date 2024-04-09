*:***************************************************************************
*: Program file  : Soorcnmo
*: Program desc. : Order Confirmation Custom program for Margaret O'Leary
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Hossam El Etreby[HDM]
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
PRIVATE lcTmpIndx

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'
lcRelExp  = 'ORDHDR.ACCOUNT + ORDHDR.STORE + ORDHDR.CTERMCODE + ORDHDR.SHIPVIA + CUSTPO'
lcDummy = ''

SELECT ORDHDR
SET RELATION OFF INTO &lcTempOrd

SELECT (lcTempOrd)
*INDEX ON &lcTmpIndx TAG (lcTempOrd)

SET RELATION TO &lcTempOrd..cOrdType + &lcTempOrd..Order INTO ORDHDR ADDITIVE

RETURN lcDummy

*:***************************************************************************
*: Program file  : lfAddFlds
*: Program desc. : Add Fields function.
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Hossam El Etreby[HDM]
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
FUNCTION lfAddFlds
PARAMETER lcX

lnFileStru = ALEN(laFilStruc,1)
DIMENSION laFilStruc[lnFileStru+4,4]

laFilStruc[lnFileStru+1,1] = 'CtermCode'
laFilStruc[lnFileStru+1,2] = 'C'
laFilStruc[lnFileStru+1,3] = 6
laFilStruc[lnFileStru+1,4] = 0

laFilStruc[lnFileStru+2,1] = 'ShipVia'
laFilStruc[lnFileStru+2,2] = 'C'
laFilStruc[lnFileStru+2,3] = 6
laFilStruc[lnFileStru+2,4] = 0

laFilStruc[lnFileStru+3,1] = 'mOrders'
laFilStruc[lnFileStru+3,2] = 'M'
laFilStruc[lnFileStru+3,3] = 0
laFilStruc[lnFileStru+3,4] = 0

laFilStruc[lnFileStru+4,1] = 'DontPrn'
laFilStruc[lnFileStru+4,2] = 'C'
laFilStruc[lnFileStru+4,3] = 10
laFilStruc[lnFileStru+4,4] = 0

*:***************************************************************************
*: Program file  : lfRepFlds
*: Program desc. : Replace function
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Hossam El Etreby[HDM]
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
FUNCTION lfRepFlds
PARAMETER lcY
IF llEndGroup
  lcOrdsNum = ''
ENDIF
lcOrdsNum = IIF(ORDHDR.ORDER $ lcOrdsNum , lcOrdsNum , lcOrdsNum + '-' + ORDHDR.ORDER)

M.CtermCode = ORDHDR.CtermCode
M.ShipVia   = ORDHDR.ShipVia

IF !EMPTY(ORDHDR.CUSTPO)
  M.CUSTPO = ORDHDR.CUSTPO
ENDIF

*:***************************************************************************
*: Program file  : lfWritOrds
*: Program desc. : Write order function
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Hossam El Etreby[HDM]
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
FUNCTION lfWritOrds
PARAMETER lcZ


*:***************************************************************************
*: Program file  : lfChcFlds
*: Program desc. : Chech the consolidated order function
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Hossam El Etreby[HDM]
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
FUNCTION lfChcFlds
PARAMETER lcD

PRIVATE lcTmpIndx , lnCounter,lcOrds
STORE '' TO lcOrds
STORE .F. TO llSesAst
lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'
SELECT (lcTempOrd)
GO TOP
lcLastSes = &lcTempOrd..SEASON
lcCompVal = &lcTmpIndx
SCAN
  IF lcCompVal = EVAL(lcTmpIndx)
    IF !EMPTY(lcOrds)
      IF !(ORDER $ lcOrds)
        lcOrds = lcOrds + '-' + ORDER
      ENDIF
    ELSE
      lcOrds = ORDER
    ENDIF
    IF lcLastSes <> &lcTempOrd..SEASON
      llSesAst = .T.
    ENDIF
    *ahmed
    lcOrds = IIF(llSesAst , lcOrds + ' |*******' , lcOrds)
    lnTmpRcNo = IIF(EOF() , 0 , RECNO())
    REPLACE ALL mOrders WITH lcOrds;
    FOR &lcTmpIndx = lcCompVal
    IF lnTmpRcNo > 0
      GO (lnTmpRcNo)
    ENDIF
    lcCompVal = &lcTmpIndx
  ELSE
    lnTmpRcNo = IIF(EOF() , 0 , RECNO())
    *IF EMPTY(lcOrds)
    *  lcOrds = ORDER
    *ENDIF
    lcOrds = IIF(llSesAst , lcOrds + ' |*******' , lcOrds)
    REPLACE ALL mOrders WITH lcOrds;
    FOR &lcTmpIndx = lcCompVal
    IF lnTmpRcNo > 0
      GO (lnTmpRcNo)
    ENDIF
    lcLastSes = &lcTempOrd..SEASON
    llSesAst = .F.
    lcOrds = ''
    lcCompVal = &lcTmpIndx
    IF lcCompVal = EVAL(lcTmpIndx)
      IF !EMPTY(lcOrds)
        IF !(ORDER $ lcOrds)
          lcOrds = lcOrds + '-' + ORDER
        ENDIF
      ELSE
        lcOrds = ORDER
      ENDIF
      IF lcLastSes <> &lcTempOrd..SEASON
        llSesAst = .T.
      ENDIF
      lnTmpRcNo = IIF(EOF() , 0 , RECNO())
      REPLACE ALL mOrders WITH lcOrds;
      FOR &lcTmpIndx = lcCompVal
      IF lnTmpRcNo > 0
        GO (lnTmpRcNo)
      ENDIF
      lcCompVal = &lcTmpIndx
    ENDIF
  ENDIF
ENDSCAN
RETURN


IF RECNO() = RECCOUNT()
  lnTmpRcNo = IIF(EOF() , 0 , RECNO())
  IF EMPTY(lcOrds)
    lcOrds = ORDER
  ENDIF
  lcOrds = IIF(llSesAst , lcOrds + ' |*******' , lcOrds)
  REPLACE ALL mOrders WITH lcOrds;
  FOR &lcTmpIndx = lcCompVal
  IF lnTmpRcNo > 0
    GO (lnTmpRcNo)
  ENDIF
  lcLastSes = &lcTempOrd..SEASON
  llSesAst = .F.
  lcOrds = ''
  lcCompVal = &lcTmpIndx
ENDIF

*:***************************************************************************
*: Program file  : lfDntPrn
*: Program desc. : Print **** incase of consolidated order.
*: For Report    : SOORCNAMO.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Salah Shalaby -(SSH)
*:***************************************************************************
*: Due To        : C101563,1
*:***************************************************************************
FUNCTION lfDntPrn
PARAMETER lcX

SELECT (lcTempOrd)
GO TOP
DO WHILE !EOF()
  lcACCOUNT   = ACCOUNT
  lcSTORE     = STORE 
  lcCTERMCODE = CTERMCODE
  lcSHIPVIA   = SHIPVIA
  lcCUSTPO    = CUSTPO   
  ldComp      = DTOS(COMPLETE)
*--- Variable to check if changing
  ldEnte  = OrdHdr.Entered
  ldStart = OrdHdr.Start
  lcStatus= Ordhdr.Status
  lcLoc   = Ordhdr.cWareCode
  lcSpIns = OrdHdr.SpcInst
  lcDept  = OrdHdr.Dept
  lcRp1   = OrdHdr.Rep1
  lcRp2   = OrdHdr.Rep2
  lcSeas  = OrdHdr.SEASON
  lcString = ''
  
  SCAN REST WHILE ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE) =;
                  lcACCOUNT+lcSTORE+lcCTERMCODE +lcSHIPVIA+lcCUSTPO+ldComp
    IF ldEnte <> OrdHdr.Entered
      lcString = lcString+'E'
    ENDIF
    IF ldStart <> OrdHdr.Start
      lcString = lcString+'S'
    ENDIF
    IF lcStatus<> Ordhdr.Status
      lcString = lcString+'U'
    ENDIF
    IF lcLoc   <> Ordhdr.cWareCode
      lcString = lcString+'L'
    ENDIF
    IF lcSpIns <> OrdHdr.SpcInst
      lcString = lcString+'P'
    ENDIF
    IF lcDept  <> OrdHdr.Dept
      lcString = lcString+'D'
    ENDIF
    IF lcRp1   <> OrdHdr.Rep1
      lcString = lcString+'1'
    ENDIF
    IF lcRp2   <> OrdHdr.Rep2
      lcString = lcString+'2'
    ENDIF
    IF lcSeas  <> OrdHdr.SEASON
      lcString = lcString+'N'
    ENDIF
  ENDSCAN
  =SEEK(lcACCOUNT+lcSTORE+lcCTERMCODE +lcSHIPVIA+lcCUSTPO+ldComp)
  REPLACE REST DontPrn WITH lcString WHILE ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)=;
                  lcACCOUNT+lcSTORE+lcCTERMCODE +lcSHIPVIA+lcCUSTPO+ldComp
ENDDO