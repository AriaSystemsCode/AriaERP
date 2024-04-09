*:************************************************************************
*: Program file  : SMWFSYER.PRG  N#000185
*: Program desc. : Defaul locatoin
*:         System: Aria advantage series
*:         Module: SYSTEM MANAGER
*:      Developer: 
*:           Date: 
*:************************************************************************
*: Calls : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*: Modifications
*:************************************************************************
EXTERNAL ARRAY laData
lcLocatoin = ""
llBrowse = .F.

IF !USED("SYWWAREH")
  =gfOpenFile(gcSysHome+"SYWWAREH",'','SH')
ENDIF
SELECT SYWWAREH
GO TOP
lcScFields ='cWareCode ,cDesc ,    cAddress1  ,cAddress2 ,cAddress3 ,'+;
            'cAddress4 ,cAddress5, cAddress6,  cPhone,    cFax,      '+;
            'lStyInv,   lMatInv  , cCont_Code, uPs   ,    lFlag , cSiteId ' 

IF !EOF()
  SCATTER FIELDS &lcScFields MEMO TO laData
ENDIF
DO (gcScrDir + gcWinAppl + '\SMWLOC.SPR')
IF USED("SYWWAREH")
  USE IN SYWWAREH
ENDIF

FUNCTION lfwCurrency

lcOldCurr=laData[17]

FUNCTION lfvCurrency

IF (!EMPTY(laData[17]) AND !SEEK(laData[17],'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laTemp")
  laData[17]=laTemp[1]
  SELECT (lcSelect)
  SHOW GET laData[17]
ENDIF

IF EMPTY(laData[17])
  laData[17] = lcOldCurr
ENDIF
llBrowse = .F.
=lfRefresh(WOUTPUT())

FUNCTION lfGetCur

laData[17] = LOOKUP(SYCINT.cCurrCode,laData[16],sycint.ccont_code,'CCONTCODE')
SHOW GET laData[17]
=lfRefresh(WOUTPUT())


FUNCTION lfVPre

lcNextActn = 'B'
SELECT SYWWAREH
GO TOP
IF EOF()
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
REPLACE lStyInv WITH .T.,; 
        lMatInv WITH .T.

FUNCTION lfvNxt
lcNextActn = 'N'

SELECT SYWWAREH
GO TOP
IF EOF()
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
REPLACE lStyInv WITH .T.,; 
        lMatInv WITH .T.

FUNCTION lfvRes
lcNextActn = 'U'
SELECT SYWWAREH
GO TOP
IF EOF()
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
REPLACE lStyInv WITH .T.,; 
        lMatInv WITH .T.
