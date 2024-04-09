
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs

PARAMETERS llDisp_New

IF PARAMETER() > 0
  llDisp_New = .F.
ELSE
  llDisp_New = .T.
ENDIF

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
lsFl_List=1
*E300683,4 Call *.SPR from screens directory
* DO SMFlLst.SPR 
DO (gcScrDir + gcWinAppl + '\SMFlLst.SPR')
*E300683,4 end   
*!**************************************************************************
*!
*!      Function lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

lsFl_List = 0
CLEAR READ

*!**************************************************************************
*!
*!      Function lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

lsFl_List = -1
CLEAR READ

*!**************************************************************************
*!
*!      Function lfvList
*!
*!**************************************************************************
*
FUNCTION lfvList

IF lsFl_List <> 0
  _CUROBJ = OBJNUM(pbSelect)
  KEYBOARD "{ENTER}"
ENDIF

*!**************************************************************************
*!
*!      Function lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect

IF lsFl_List= 0
  = gfModalGen('QRM00008B00000','Dialog'," ")  
  RETURN .F.
ENDIF
