*:************************************************************
*: Program file  : SMFLDFX.PRG
*: Program desc. : Add the edit audit fields to sysfiles that do not contains them
*:        System : Aria Advantage Series.
*:        Module : SM
*:     Developer : tarek Mohammed Ibrahim TMI
*:     Entry     : B606401
*: Note          : this program is generated as a stand alone exe to run at one customer 
*:               : till to make it standard fix
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO icPack
*:************************************************************
*: Modifications :
*:************************************************************
SET ESCAPE OFF
*WAIT WINDOW 'This program will update SYDFLFLD file by adding edit audit fields '+;
             'to files that do not contain them.'

*WAIT WINDOW '(ESC) to cancel,any other key to continue..'
*IF LASTKEY() = 27
*  RETURN
*ENDIF

  
gcSysHome = GETDIR('','Select Project System Files')
IF EMPTY(gcSysHome)
  WAIT WINDOW 'No folder is selected.'
  RETURN 
ELSE
  SET DEFAULT TO (gcSysHome)
  IF !FILE(gcSysHome+'SYDFIELD.DBF') OR ;
     !FILE(gcSysHome+'SYDFILES.DBF') OR ;
     !FILE(gcSysHome+'SYDFLFLD.DBF')
    WAIT WINDOW 'This is not a system files folder.'
    RETURN
  ENDIF
ENDIF 

*--Edit Audit fields
DIMENSION laAudit[3,4]
laAudit[1,1] = 'CEDIT_USER'
laAudit[1,2] = 'C'
laAudit[1,3] = 10
laAudit[1,4] = 'Edit User'
laAudit[2,1] = 'DEDIT_DATE'
laAudit[2,2] = 'D'
laAudit[2,3] = 8
laAudit[2,4] = 'Edit date'
laAudit[3,1] = 'CEDIT_TIME'
laAudit[3,2] = 'C'
laAudit[3,3] = 11
laAudit[3,4] = 'Edit time'

*--Open needed files
USE (gcSysHome+'SYDFIELD') IN 0 ORDER TAG 'CFLD_NAME' SHARE    && Key : CFLD_NAME
USE (gcSysHome+'SYDFILES') IN 0 ORDER TAG 'CFILE_NAM' SHARE    && Key : CFILE_NAM
USE (gcSysHome+'SYDFLFLD') IN 0 ORDER TAG 'CFLFLD' SHARE       && Key : CFILE_NAM+CFLD_NAME

 *--Check first they are in the sydfield file
SELECT SYDFIELD
SCATT MEMV MEMO BLANK
FOR lnCnt = 1 TO ALEN(laAudit,1)
  IF !SEEK(laAudit[lnCnt,1],'SYDFIELD')      
    m.CFLD_NAME = laAudit[lnCnt,1]
    m.CDATA_TYP = laAudit[lnCnt,2]
    m.NFLD_WDTH = laAudit[lnCnt,3]
    m.CFLD_HEAD = laAudit[lnCnt,4]
    m.CUPGRDLVL = 'A'
    m.DADD_DATE = DATE()
    m.CADD_TIME = TIME()
    INSERT INTO SYDFIELD FROM MEMVAR           
  ENDIF
ENDFOR

*--if edit audit fields are not exist in sydflfld file for a certain system file , add them
SELECT SYDFLFLD
SCATT MEMV MEMO BLANK

SELECT SYDFILES
SCAN
  IF SEEK(SYDFILES.CFILE_NAM+'CADD_USER ','SYDFLFLD')
    FOR lnCnt = 1 TO ALEN(laAudit,1)
      IF !SEEK(SYDFILES.CFILE_NAM+laAudit[lnCnt,1],'SYDFLFLD')
        WAIT WINDOW NOWAIT 'Updating file '+SYDFILES.CFILE_NAM
        lcFile = SYDFILES.CFILE_NAM
        SELECT MAX(NFLD_POS) FROM SYDFLFLD WHERE CFILE_NAM = lcFile INTO ARRAY laFld_Pos
        m.CFILE_NAM = SYDFILES.CFILE_NAM
        m.CFLD_NAME = laAudit[lnCnt,1]
        m.CUPGRDLBL = 'S'
        m.NFLD_POS  = laFld_Pos+1
        m.DADD_DATE = DATE()
        m.CADD_TIME = TIME()
        INSERT INTO SYDFLFLD FROM MEMVAR
      ENDIF
    ENDFOR    
  ENDIF
ENDSCAN

WAIT CLEAR

*WAIT WINDOW "All files are updated with edit audit fields. You need to Update file structure."

USE IN SYDFIELD
USE IN SYDFLFLD
USE IN SYDFILES