*****************************************************************
* Program          : UPDSEQ.PRG
*----------------------------------------------------------------
* Purpose          : PROGRAM TO FIX Codes and Sequence FILES
*----------------------------------------------------------------
* Passed Parameter : System Files Path
*----------------------------------------------------------------
* Developer        : Hossam El Etreby [HDM]
*----------------------------------------------------------------
* Date             : 02/02/2000
*----------------------------------------------------------------
* Due to           : Bug 802982
*****************************************************************

PARAMETER lcSysPath
PRIVATE lcDataDir

*-- If System files directory not passed 
*-- get it from the user
IF TYPE('lcSysPath') <> 'C'
  lcSysPath = GETDIR('','Select System Files Directory')
ENDIF
*-- IF CANCEL OR EMPTY RETURN
IF EMPTY(lcSysPath)
  WAIT WINDOW 'No Sysfiles directory found...!'
  RETURN
ENDIF
STORE .F. TO llSycComp , llCodeOp , llSeqOp

WAIT WINDOW 'Opening Companies file' NOWAIT

IF !USED('SYCCOMP')
  USE (lcSysPath+'SYCCOMP') SHARED IN 0
  llSycComp = .T.
ENDIF
SCAN
  *-- if SEQ is not based on division we have to
  *-- delete sequence group data
  IF gfGetMemVar('M_DIV_SEQ ',ALLTRIM(SYCCOMP.cComp_ID)) <> 'Y'
    lcDataDir = ALLTRIM(CCOM_DDIR)
    *-- Start Fixing the codes file
    USE (lcDataDir+'CODES') ORDER TAG Idrltfname AGAIN ALIAS ('CODES'+SYCCOMP.cComp_ID) IN 0
    SELECT ('CODES'+SYCCOMP.cComp_ID)
    WAIT WINDOW 'Checking Company' + SYCCOMP.cComp_ID+'-'+ SYCCOMP.cCom_Name NOWAIT
    IF SEEK('N'+'Y'+'CDIVISION ')
      DELETE REST WHILE cdefcode+crltfield+cfld_name  = 'N'+'Y'+'CDIVISION ' ;
                FOR CRLTD_NAM = 'DIVGROUP  '
    ENDIF
    *-- Close Codes File
    USE IN ('CODES'+SYCCOMP.cComp_ID)
    
    *-- Start Fixing The Sequence file
    USE (lcDataDir+'SEQUENCE') ORDER TAG Cseq_type AGAIN ALIAS ('SEQUENCE'+SYCCOMP.cComp_ID) IN 0
    SELECT ('SEQUENCE'+SYCCOMP.cComp_ID)
    SCAN
      lcCurrSeq = cSeq_Type
      WAIT WINDOW 'Checking Sequences for '+ lcCurrSeq NOWAIT
      lnBigSeq  = 0
      SCAN REST WHILE cSeq_Type = lcCurrSeq
        *-- Get the biggest sequence No.
        lnBigSeq = MAX(lnBigSeq,nSeq_NO)
      ENDSCAN
      =SEEK(lcCurrSeq+SPACE(3))
      =RLOCK()
      REPLACE nSeq_NO WITH lnBigSeq
      UNLOCK
      DELETE REST WHILE CSEQ_TYPE+CSEQ_GROUP = lcCurrSeq FOR !EMPTY(CSEQ_GROUP)
      *-- Seek The current seq. to continue the scan
      =SEEK(lcCurrSeq+SPACE(3))
    ENDSCAN
    USE IN ('SEQUENCE'+SYCCOMP.cComp_ID)
  ENDIF
ENDSCAN
IF llSycComp
  USE IN SYCCOMP 
ENDIF
WAIT CLEAR