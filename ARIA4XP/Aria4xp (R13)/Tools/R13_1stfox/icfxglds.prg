*:**************************************************************************
*: Program file  : ICFXGLDS
*: Program desc. : Custom Order Detail Report for Le Mystere
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 10/24/2000
*: Reference     : B603983 
*:**************************************************************************
*: Example : DO ICFXGLDS
*B607845,1 TMI 11/28/2006 FIx a bug that the program is very slow  , check ticket # T20061031.0009
*:**************************************************************************
*

PARAMETERS lcDataDir

PRIVATE lcDataDir
IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  lcDataDir = ""
  lcDataDir = GETDIR('','Select data directory')
ENDIF

IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'GLDist.DBF') OR !FILE(lcDataDir+'STYINVJL.DBF')
  WAIT WINDOW 'Wrong company data directory!' TIMEOUT 3
  RETURN
ENDIF

PRIVATE llGLDist , llStyInvJl , lcDistTag , lcInvTag , lnActAlias

STORE '' TO lcDistTag , lcInvTag
STORE .F. TO llGLDist , llStyInvJl

lnActAlias = SELECT(0)

*-- Opening the GLDist file. 
IF !llGLDist AND !USED('GLDist')
  USE (lcDataDir+'GLDist.DBF') IN 0 
  llGLDist = .T.
ELSE
  SELECT GLDist
  lcDistTag = ORDER() 
  SET ORDER TO
ENDIF
     
*-- Opening the Style Inventory Journal file. 
IF !llStyInvJl AND !USED('StyInvJl')
  USE (lcDataDir+'StyInvJl.DBF') IN 0
  llStyInvJl = .T.
ELSE
  SELECT StyInvJl
  lcInvTag = ORDER()
  SET ORDER TO
ENDIF

*B607845,1 TMI [Start] Enhance updating gldist file
SELECT GLDIST
LOCATE FOR TRAN_TYPE = 'IA'
IF FOUND()
  REPLACE Tran_Type WITH 'LK' , Tran_Desc WITH 'INVENTORY LOCKING' ;
          REST FOR TRAN_NO = '      '

  WAIT WINDOW NOWAIT 'Indexing ... '
  INDEX ON TRAN_NO TO GLTRN_NO FOR !EMPTY(TRAN_NO)
  *B607845,1 TMI [End  ] 
  
  *-- Scan loop around Style Inventory journal for Type = '9' (Inventory Locking) 
  *B607845,1 TMI [Start] replace this scan loop with enhanced scan loop
  *SELECT StyInvJl
  *SCAN FOR cTrType = '9'
  *  SELECT GlDist    
  *  *-- Scan loop around GlDist file to change all Inventory Locking from "IA" to "LK"
  *  REPLACE Tran_Type WITH 'LK' , Tran_Desc WITH 'INVENTORY LOCKING' ;
  *          FOR Tran_No + Tran_Type + glSession + Catg_Key = StyInvJl.cTrCode AND RLOCK()
  *  UNLOCK
  *ENDSCAN  
  SELECT STYINVJL
  SCAN FOR cTrType = '9' AND !EMPTY(CTRCODE) 
    IF SEEK(CTRCODE,'GLDIST')
      WAIT WINDOW NOWAIT StyInvJl.cTrCode
      SELECT GLDIST
      REPLACE Tran_Type WITH 'LK' ;
              Tran_Desc WITH 'INVENTORY LOCKING' ;
                WHILE TRAN_NO = StyInvJl.cTrCode ;
                  FOR RLOCK()
    ENDIF
  ENDSCAN
ENDIF
WAIT CLEAR
*B607845,1 TMI [End  ] 


*-- Close the GlDist.
IF llGLDist
  USE IN GlDist
ELSE
  SELECT GlDist 
  SET ORDER TO &lcDistTag
ENDIF
  
*-- Close the StyInvJl.
IF llStyInvJl
  USE IN StyInvJl
ELSE
  SELECT StyInvJl
  SET ORDER TO &lcInvTag
ENDIF

SELECT (lnActAlias)  
WAIT WINDOW 'Completed successfully' TIMEOUT 3
*-- End of ICFXGLDS.