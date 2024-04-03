PARAMETER lcSysPath
PRIVATE llDelete, llClsCnfg, llClsRpvr, llClsRprt

STORE .F. TO llClsCnfg, llClsRpvr, llClsRprt

WAIT WINDOW 'Fixing Setups File. Please wait' NOWAIT

IF !USED('SYCCONFG')
  SELECT 0
  USE (lcSysPath+'SYCCONFG') 
  llClsCnfg = .T.
ELSE
  SELECT SYCCONFG
ENDIF
REPLACE cUpgrdLvl WITH "S" FOR EMPTY(cUpgrdLvl)

IF llClsCnfg
  USE IN SYCCONFG
ENDIF

WAIT CLEAR