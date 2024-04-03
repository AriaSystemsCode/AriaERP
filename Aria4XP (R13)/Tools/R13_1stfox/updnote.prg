PARAMETER lcSysPath
DECLARE laNoteKey[14,2]
laNoteKey[1,1] = 'FABRIC'
laNoteKey[1,2] = 'GFABRIC|FABRIC'

laNoteKey[2,1] = 'POFHDR'
laNoteKey[2,2] = 'MCMATTYPE+POMAT|POFHDR'

laNoteKey[3,1] = 'CUTTKTH'
laNoteKey[3,2] = 'ICUTTKT|CUTTKTH'

laNoteKey[4,1] = 'MMFGORDH'
laNoteKey[4,2] = 'OCMFGORDNO|MMFGORDH'

laNoteKey[5,1] = 'INVHDR'
laNoteKey[5,2] = 'CINVOICE|INVHDR'

laNoteKey[6,1] = 'LC'
laNoteKey[6,2] = 'L*cLCType+CLCNO|LC'

laNoteKey[7,1] = 'POSHDR'
laNoteKey[7,2] = "P*cStyType+PO|POSHDR"


laNoteKey[8,1] = 'STYLE'
laNoteKey[8,2] = "FSUBSTR(CSTYMAJOR,1,LEN(gfItemMask('PM')))|STYLE"

laNoteKey[9,1] = 'APVENDOR'
laNoteKey[9,2] = 'HCVENDCODE|VENCODE'

laNoteKey[10,1] = 'RETAUTH'
laNoteKey[10,2] = 'ZRANO|RETAUTH'

laNoteKey[11,1] = 'RETHDR'
laNoteKey[11,2] = 'RCRMEMO|RETHDR'

laNoteKey[12,1] = 'CUSTOMER'
laNoteKey[12,2] = "A*'M'+ACCOUNT|CUSTOMER"

laNoteKey[13,1] = 'ORDHDR'
laNoteKey[13,2] = "B*cOrdType+ORDER|ORDHDR"

laNoteKey[14,1] = 'SALESREP'
laNoteKey[14,2] = 'JREPCODE|SALESREP'

IF TYPE('lcSysPath') <> 'C'
  lcSysPath = GETDIR('','Select System Files Directory')
ENDIF
IF EMPTY(lcSysPath)
  WAIT WINDOW 'No Sysfiles directory found...!'
ENDIF
PRIVATE llUsedByMe , llIndex , llCompUsed
STORE .F. TO llUsedByMe , llIndex , llCompUsed
IF !USED('SYDFILES')
  lcError = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.
  USE (lcSysPath + "SYDFILES.DBF") SHARED IN 0
  ON ERROR &lcError
  IF llError
    WAIT WINDOW 'No Sysfiles directory found in' +FULLPATH('')
    RETURN
  ENDIF
  llUsedByMe = .T.
ELSE
  llUsedByMe = .F.
ENDIF

IF !USED('SYDINDEX')
  USE (lcSysPath + "SYDINDEX.DBF") SHARED IN 0
  llIndex = .T.
ELSE
  llIndex = .F.
ENDIF
SELECT SYDINDEX
SET ORDER TO TAG Cfile_nam

SELECT SYDFILES
SET RELATION TO Sydfiles.cfile_nam INTO Sydindex ADDITIVE

*IF TYPE('CNOTEKEY') <> 'C'
*  WAIT WINDOW 'You Have to Update System files Structure First ...'
*  IF llUsedByMe
*    USE IN SYDFILES
*  ENDIF
*  =lfClosData()
*  RETURN
*ENDIF


IF !USED('SYCCOMP')
  USE (lcSysPath + "SYCCOMP.DBF") SHARED IN 0
  llCompUsed = .T.
ELSE
  llCompUsed = .F.
ENDIF
SELECT SYCCOMP
GO TOP
IF EOF()
  IF llCompUsed
    USE IN SYCCOMP
  ENDIF
  =lfClosData()
  RETURN
ENDIF

*- Start Fix for all Companies

SELECT SYCCOMP
SCAN
  *SELECT SYDFILES
  *-- Start Fix depending on data files that has cNoteKey Filled
  gcDataDir = alltrim(SYCCOMP.cCom_DDir)
  *SCAN FOR !EMPTY(CNOTEKEY)
  FOR lnLoop = 1 TO 14
    *-- Open Data Files
    *IF FILE(ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(SYDFILES.cFile_Nam)+'.DBF')
    IF FILE(ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1])+'.DBF')
    
      lcError = ON('ERROR')
      llError = .F.
      ON ERROR llError = .T.
      USE(ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(ALLTRIM(laNoteKey[lnLoop,1]))) EXCL IN 0
      ON ERROR &lcError
      IF llError
        WAIT WINDOW 'Cannot open data file'+(ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1]))
        LOOP
      ENDIF
    ELSE
      LOOP
    ENDIF
    USE (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM('NOTEPAD.DBF')) SHARED IN 0

    SELECT NOTEPAD
    SET ORDER TO TAG NOTEPAD
    
    lcKey     = SUBSTR(ALLTRIM(laNoteKey[lnLoop,2]),1,1)
    lnStick   = ATC('|',ALLTRIM(laNoteKey[lnLoop,2]))
    lnStarPos = ATC('*',ALLTRIM(laNoteKey[lnLoop,2]))
    
    lcNoteKey = ALLTRIM(ALLTRIM(laNoteKey[lnLoop,2]))
    
    IF lnStarPos <> 0
      lcNoteKey = SUBSTR(lcNoteKey,lnStarPos+1,lnStick - lnStarPos - 1)
    ELSE
      lcNoteKey = ALLTRIM(SUBSTR(lcNoteKey,2,lnStick-2))
    ENDIF

    SELECT (ALLTRIM(laNoteKey[lnLoop,1]))
    IF TYPE('lHasNotes') <> 'L'
      =lfUpdStruc(ALLTRIM(laNoteKey[lnLoop,1]))
      IF !USED(ALLTRIM(laNoteKey[lnLoop,1]))
        USE (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1])) SHARED IN 0
      ENDIF
    ENDIF
    SELECT (ALLTRIM(laNoteKey[lnLoop,1]))
    SET RELATION TO lcKey + &lcNoteKey INTO Notepad ADDITIVE
    GO TOP
    SCAN
      WAIT WINDOW 'Updating ' + ALLTRIM(laNoteKey[lnLoop,1]) + ' Data For Company ' + ALLTRIM(SYCCOMP.CCOMP_ID) NOWAIT
      IF EOF('NOTEPAD')
        REPLACE lHasNotes WITH .F.
      ELSE
        REPLACE lHasNotes WITH .T.
      ENDIF
    ENDSCAN
    USE IN NOTEPAD
    USE IN ALLTRIM(laNoteKey[lnLoop,1])
  *ENDSCAN
  ENDFOR
ENDSCAN
WAIT CLEAR
=lfClosData()
RETURN


FUNCTION lfUpdStruc
PARAMETER lcFileName

SELECT (lcFileName)

= AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]

laFileStru[lnFileStru+1,1] = 'lHasNotes'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lcTmpName = ("X"+SUBSTR(SYS(2015),4))

WAIT WINDOW 'Updating ' + lcFileName + ' File Structure For Company ' + ALLTRIM(SYCCOMP.CCOMP_ID) NOWAIT
CREATE TABLE (ALLTRIM(SYCCOMP.cCom_DDir) + lcTmpName) FROM ARRAY laFileStru

WAIT WINDOW 'Re-Building ' + lcFileName + ' Data For Company ' + ALLTRIM(SYCCOMP.CCOMP_ID) NOWAIT
APPEND FROM (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1]))
REPLACE ALL lHasNotes WITH .T.

USE IN (lcFileName)

USE IN (lcTmpName)

ERASE (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1])+'.DBF')


RENAME (ALLTRIM(SYCCOMP.cCom_DDir) + lcTmpName + '.DBF') TO (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1]) + '.DBF')
= SEEK(laNoteKey[lnLoop,1] , 'SYDINDEX')
USE (ALLTRIM(SYCCOMP.cCom_DDir) + ALLTRIM(laNoteKey[lnLoop,1])) EXCL IN 0
SELECT(ALLTRIM(laNoteKey[lnLoop,1]))
SET INDEX TO (ALLTRIM(SYCCOMP.cCom_DDir) + SYDINDEX.Cindx_Nam )


FUNCTION gfItemMask

PARAMETERS lcMaskOrHead, lcDataDr
IF TYPE('lcDataDr') # 'C' .OR. EMPTY(lcDataDr)
  lcDataDr = gcDataDir
ENDIF

PRIVATE lcReturn,llStructUse,lnRecNo,lcStructOrd,lnCurAlias,llArray,;
        laSeg,lcItemDim,lcHeader,lnStartNonM,lnNoSeg,lnPosistion
STORE '' TO lcReturn        
llArray = TYPE('lcMaskOrHead[1]') # 'U'
lcItemDim = 'I'
IF !llArray
  IF TYPE('lcMaskOrHead')<>'C'
    RETURN .F.
  ENDIF
  lcMaskOrHead = UPPER(lcMaskOrHead)
  lcItemDim = IIF(LEN(lcMaskOrHead)>1,RIGHT(lcMaskOrHead,1),'I')  
  lcMaskOrHead = LEFT(lcMaskOrHead,1)  
ENDIF
lcLoopExt = lcItemDim 
lnCurAlias = SELECT()
llStructUse = .F.
IF !USED('ICISTRU')
  *E300998,1 AMM Open the table of the required company.
  *USE (gcDataDir+'ICISTRU') IN 0
  USE (lcDataDr+'ICISTRU') IN 0
  *E300998,1 AMM end
  llStructUse = .T.
ELSE
  SELECT ICISTRU
  lcStructOrd = ORDER()
  lnRecNo = RECNO()
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO
*GO TOP
=SEEK('U1')
lcHeader = cIsegHead
lnNoSeg = 0
lnPosistion = 1
SCAN REST WHILE citemrecty+cisegno = 'U'
  IF lcLoopExt <> 'N'
    lnNoSeg = lnNoSeg + 1
    DIMEN laSeg[lnNoSeg,7]
    laSeg[lnNoSeg,1] = cisegType
    laSeg[lnNoSeg,2] = ALLT(cisegsdes)
    laSeg[lnNoSeg,3] = REPL('X',nisegsize)
    laSeg[lnNoSeg,4] = lnPosistion
    laSeg[lnNoSeg,5] = ALLT(CISEGLDES)
    laSeg[lnNoSeg,6] = CISEGSEPR
    laSeg[lnNoSeg,7] = LSEGENDMAJ  
    lcReturn = lcReturn+REPL('X',nisegsize)+ALLT(CISEGSEPR)
  ENDIF
  lnPosistion = lnPosistion + nisegsize + LEN(ALLT(CISEGSEPR))
  IF lcLoopExt = 'N' AND lSegEndMaj
    lcLoopExt = 'I'
  ENDIF
  IF lcItemDim = 'M' AND lSegEndMaj
     EXIT
  ENDIF    
ENDSCAN
IF llArray
  DIMEN lcMaskOrHead[ALEN(laSeg,1),ALEN(laSeg,2)]
  lcReturn=ACOPY(laSeg,lcMaskOrHead)
ELSE  
  DO CASE
    CASE  lcMaskOrHead = 'S'
      lcReturn = lnNoSeg
    CASE  lcMaskOrHead = 'P' AND  lcItemDim='M'
      IF gfItemMask('SN')>0
        lcReturn = SUBSTR(lcReturn,1,LEN(lcReturn)-1)
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='M'
      IF gfItemMask('SN')>0
        lcReturn = SUBSTR(lcHeader,1,laSeg[lnNoSeg,4]+LEN(laSeg[lnNoSeg,2])-1)
      ELSE
        lcReturn = lcHeader        
      ENDIF  
    CASE lcMaskOrHead = 'H' AND lcItemDim='N'  AND lnNoSeg>0
      lcReturn = SUBSTR(lcHeader,laSeg[1,4])  
    CASE lcMaskOrHead = 'H' AND lcItemDim = 'I'
      lcReturn = lcHeader  
  ENDCASE
ENDIF  

IF llStructUse 
    USE IN ICISTRU
ELSE    
  SELECT ICISTRU
  SET ORDER TO TAG (lcStructOrd)
  IF lnRecNo>0 AND lnRecNo<=RECCOUNT()
    GO lnRecNo
  ENDIF  
ENDIF
SELECT (lnCurAlias)
RETURN lcReturn

FUNCTION lfClosData

IF llUsedByMe
  USE IN SYDFILES
ENDIF
IF llIndex
  USE IN SYDINDEX
ENDIF

IF llCompUsed
  USE IN SYCCOMP
ENDIF
