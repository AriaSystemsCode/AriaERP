*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
 
DO gpSetup
 
*E300683,4 Call *.SPR from screens directory
* DO SMTrydg.SPR 
DO (gcScrDir + gcWinAppl + '\SMTrydg.SPR')
*E300683,4 end    
PRIVATE lcMtoDsp
lcMtoDsp="" 
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

lcBvar = laData[3]
lcMvar = laData[3]
SHOW GET lcBvar
SHOW GET lcMvar
Show GET pbShow ENABLE
RETURN


FUNCTION lfvlcBvar
IF LEN(lcMvar)>0
SHOW GET pbShow ENABLE
ENDIF
RETURN .T.


FUNCTION lfvlcMvar
IF LEN(lcBvar)>0
SHOW GET pbShow ENABLE
ENDIF

RETURN .T.

FUNCTION lfvpbShow
lcMtoDsp="INM"+lcMvar+"B"+lcBvar
=gfModalGen(lcMtoDsp)
RETURN .T.

FUNCTION lfvpbcancl
RETURN .T.
