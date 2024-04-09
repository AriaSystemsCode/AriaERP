PRIVATE lcSelected,llErrorUsed

lcSelected  = SELECT()
llErrorUsed = .F.

IF !USED('SYUEROR')
  SELECT 0
  USE (gcSysHome+"SYUEROR")
  llErrorUsed=.T.  
ELSE
  SELECT SYUEROR  
  GO TOP
ENDIF

IF pSetup()
  SET CONSOL OFF
  DO gfDispRep WITH "ERROR.FRX"
  SET CONSOL ON
ENDIF  

IF llErrorUsed
  USE IN SYUEROR
ENDIF

SELECT(lcSelected)
