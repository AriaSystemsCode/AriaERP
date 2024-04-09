*:************************************************************************
*: Program file  : GFZOOM.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcZoomFld,llField
EXTERNAL ARRAY LASCRMODE

lcZoomMem = &lcZoomFld
lcNoModify  = IIF(laScrMode[2]," NOMODIFY","")

DO syZOOM.SPR
 
SHOW GET (lcZoomFld) 

*!**************************************************************************
*!
*!      FUNCTION : lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow
EXTERNAL ARRAY LASCRMODE
IF laScrMode[1] 
  SHOW GETS DISABLE  ONLY
  SHOW GET  pbZom_OK ENABLE
ENDIF



*!**************************************************************************
*!
*!      FUNCTION : lfvZomOK
*!
*!**************************************************************************
*
FUNCTION lfvZomOK
lcZoomSel=SELECT()
SELECT 0
IF TYPE(lcZoomFld)<>'U'
  &lcZoomFld = lcZoomMem
ELSE 
 SELECT(lcZoomSel)  
 REPLACE &lcZoomFld WITH lcZoomMem  
ENDIF
 SELECT(lcZoomSel)  
IF llField
 REPLACE &lcZoomFld WITH lcZoomMem
ENDIF
=gfUpdate()