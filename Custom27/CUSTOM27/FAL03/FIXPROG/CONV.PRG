lcDataDir = GETDIR(SET('DEFAULT'),'Select Data Directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

IF FILE(lcDataDir+'STYINVJL.DBF')
  USE (lcDataDir+'STYINVJL') IN 0 SHARED
ELSE
  WAIT WINDOW 'STYINVJL file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'STYLE.DBF')
  USE (lcDataDir+'STYLE') IN 0 SHARED ORDER TAG STYLE
ELSE
  WAIT WINDOW 'STYLE file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'STYDYE.DBF')
  USE (lcDataDir+'STYDYE') IN 0 SHARED ORDER TAG STYDYE
ELSE
  WAIT WINDOW 'STYDYE file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'INVLINE.DBF')
  USE (lcDataDir+'INVLINE') IN 0 SHARED ORDER TAG INVLINE
ELSE
  WAIT WINDOW 'INVLINE file does not exist, Cannot procced'
  RETURN
ENDIF

SELECT STYLE
WAIT WINDOW 'Fix the average cost in the STYLE file.' NOWAIT
 repl all Ave_cost with TotCost

SELECT STYINVJL
SCAN
  IF SEEK(STYINVJL.Style,'STYLE')
     SELECT STYLE 
    SCAN WHILE STYINVJL.Style == STYLE
      WAIT WINDOW 'Fixing the STYINVJL file.' NOWAIT
	    REPLACE STYINVJL.Ncost WITH STYLE.Ave_cost,;
          STYINVJL.nStkVal with STYINVJL.Ncost * STYINVJL.NTotStk
    ENDSCAN       
  ENDIF 
ENDSCAN

SET ORDER TO TAG MFGOPR IN STYINVJL
SELECT INVLINE
SCAN
  WAIT WINDOW 'Fix the cost in the INVLINE file.' NOWAIT
  IF SEEK(INVOICE,'STYINVJL')
    REPLACE COST WITH STYINVJL.NCOST
  ELSE
    IF SEEK(STYLE,'STYLE')
      REPLACE COST WITH STYLE.AVE_COST
    ENDIF
  ENDIF
ENDSCAN

SELECT STYDYE
SCAN
  IF SEEK(STYLE,'STYLE')
    WAIT WINDOW 'Fix the average cost in the STYDYE file.' NOWAIT
    REPLACE AVE_COST WITH STYLE.AVE_COST,;
            NSTKVAL  WITH AVE_COST * TOTSTK
  ENDIF
ENDSCAN

WAIT WINDOW 'Fixing completed successfully'