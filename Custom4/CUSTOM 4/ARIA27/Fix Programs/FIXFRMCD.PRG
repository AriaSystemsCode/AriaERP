*****************************************************************
* Program          : FIXFRMCD.PRG
*----------------------------------------------------------------
* Purpose          : PROGRAM TO FIX FORMCDHD FILE
*----------------------------------------------------------------
* Passed Parameter : System Files Path
*----------------------------------------------------------------
* Developer        : AHMED MAHER (AMH)
*----------------------------------------------------------------
* Date             : 02/15/2003
*----------------------------------------------------------------
* Due to           : Bug 606950
*****************************************************************

PARAMETER lcSysPath
PRIVATE lcDataDir

*-- If System files directory not passed 
*-- get it from the user
IF TYPE('gcSysHome') <> 'C'
  gcSysHome = GETDIR('','Select System Files Directory')
ENDIF
*-- IF CANCEL OR EMPTY RETURN
IF EMPTY(gcSysHome)
  WAIT WINDOW 'No Sysfiles directory found...!'
  RETURN
ENDIF
STORE .F. TO llSycComp , llSyFrmCdH

WAIT WINDOW 'Opening Companies file' NOWAIT

IF !USED('SYFRMCDH')
  USE (gcSysHome+'SYFRMCDH') SHARED IN 0
  llSyFrmCdH = .T.
ENDIF

SELECT SYFRMCDH
SET ORDER TO 1
IF SEEK('ALPKLS')
  SCATTER MEMO MEMVAR FIELDS MFORMSETS
ELSE
  IF llSyFrmCdH
    USE IN SYFRMCDH
  ENDIF
  RETURN
ENDIF

IF llSyFrmCdH
  USE IN SYFRMCDH
ENDIF

IF !USED('SYCCOMP')
  USE (lcSysPath+'SYCCOMP') SHARED IN 0
  llSycComp = .T.
ENDIF

SELECT SYCCOMP
SCAN
  lcDataDir = ALLTRIM(CCOM_DDIR)
  USE (lcDataDir+'FORMCDHD') ORDER TAG FORMCDHD AGAIN ALIAS ('FORMCDHD'+SYCCOMP.cComp_ID) IN 0
  SELECT ('FORMCDHD'+SYCCOMP.cComp_ID)
  WAIT WINDOW 'Fixing Company' + SYCCOMP.cComp_ID+'-'+ SYCCOMP.cCom_Name NOWAIT
  IF SEEK('ALPKLS') .AND. EMPTY(MFORMSETS)
    GATHER MEMO MEMVAR FIELDS MFORMSETS
  ENDIF
  USE IN ('FORMCDHD'+SYCCOMP.cComp_ID)
ENDSCAN
IF llSycComp
  USE IN SYCCOMP 
ENDIF
WAIT CLEAR