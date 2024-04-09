*!********************************************************************
*: Program file  : SMCAFIX.PRG  B#804012
*: Program desc. : Fix Style/Stydye file.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: IC
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************

llFrstCon = .F.
llScndCon = .F.
IF !gfSetup()
  RETURN
ENDIF
llFrstCon = (gfModalGen('INM00000B00008',.F.,.F.,.F.,"It is highly recommended to back up STYLE and STYDYE files before process with this fix.") = 1)
llScndCon = llFrstCon .AND. (gfModalGen('INM00000B00008',.F.,.F.,.F.,"This program required EXCLUSIVE open for STYLE and STYDYE. Make sure that all users are logged off.") = 1)
=gfOpenFile(gcDataDir +'Style', gcDataDir +'Style','EX')
=gfOpenFile(gcDataDir +'StyDye', gcDataDir +'StyDye','EX')
IF llScndCon
  =lfUpdate("Style")
  =lfUpdate("StyDye")
ENDIF

FUNCTION lfUpdate
PARAMETER lcFile

SELECT &lcFile
WAIT WINDOW "Wait while preparing "+lcFile+" file ." NOWAIT
PACK
IF lcFile = "Style"
  SELECT Style
  SET ORDER TO Style
ELSE
  SELECT StyDye
  SET ORDER TO StyDye
ENDIF
SELECT &lcFile
WAIT WINDOW "Reindex "+lcFile+" file. Please wait." NOWAIT
REINDEX
GO TOP
lcCurrSty = ""
lcRecNo   = ""
DO WHILE !EOF()
  lcCurrSty = Style
  WAIT WINDOW "Fix Season-Style-Color: "+lcCurrSty NOWAIT
  lnRec     = RECNO()
  lcRecNo   = ALLTRIM(STR(lnRec))
  IF lcFile = "Style"
    WAIT WINDOW "Delete repeated records for Season-Style-Color: "+lcCurrSty NOWAIT
    DELETE REST WHILE Style = lcCurrSty FOR ALLTRIM(STR(RECNO())) <> lcRecNo
  ELSE
    DELETE REST WHILE style+cwarecode+dyelot = lcCurrSty FOR ALLTRIM(STR(RECNO())) <> lcRecNo
  ENDIF
ENDDO
WAIT CLEAR