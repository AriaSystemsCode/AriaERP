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
PRIVATE lcDataDir, lnCurrAlis

lnCurrAlis = SELECT(0)
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

=lfFixForm('ALPKLS')
=lfFixForm('ALPKTK')

STORE .F. TO llRepUvr, llRepvr, llMenuUsed, lcMenuOrd

WAIT WINDOW 'Opening Companies file' NOWAIT

IF !USED('SYDREPRT')
  USE (gcSysHome+'SYDREPRT') SHARED IN 0 ORDER 1
  llRepvr = .T.
ENDIF

IF !USED('SYREPUVR')
  USE (gcSysHome+'SYREPUVR') SHARED IN 0 ORDER TAG Crep_id
  llRepUvr = .T.
ENDIF

SELECT SYDREPRT
IF SEEK("MAVMREF")
  DELETE
ENDIF
IF llRepvr
  USE IN SYDREPRT
ENDIF

SELECT SYREPUVR
IF SEEK('MFWIPAC POSHDR.PO')
  DELETE REST WHILE crep_id+PADR(SUBSTR(mfld_name,1,100),100," ") = 'MFWIPAC POSHDR.PO';
                FOR CUPGRDLVL = 'U'
ENDIF

SET ORDER TO 1
IF SEEK("MAVMREF")
  DELETE REST WHILE CREP_ID = "MAVMREF"
ENDIF

*-- Add the Missed record in this service pack.
*-- 
SET ORDER TO CREP_ID
IF !SEEK("SOORCN  llPrtCmpdt")
  APPEND BLANK
ENDIF 
REPLACE CREP_ID WITH "SOORCN",;
        mfld_name WITH "llPrtCmpdt",;
        cdata_typ WITH "L",;
        cfld_head WITH "Checking Comp. Date by",;
        nfld_wdth WITH 1,;
        cdefa_typ WITH "E",;
        mdata_def WITH "gfGetMemVar('M_CMPDOLN')",;
        nVarPos   WITH 0
SET ORDER TO 2   && CREP_ID+CEXPTYPE+STR(NVARPOS)
PRIVATE lnVarPos
lnVarPos = 0
SCAN FOR CREP_ID = "SOORCN" AND NVARPOS > 0
  lnVarPos = lnVarPos + 1
  REPLACE nVarPos WITH lnVarPos  
ENDSCAN

IF llRepUvr
  USE IN SYREPUVR
ENDIF

*-- Menu record.
IF !USED('SYCMENU')
  USE (gcSysHome+'SYCMENU') SHARED IN 0 ORDER 2
  llMenuUsed = .T.
ENDIF
SELECT SYCMENU
LOCATE FOR CAPP_ID + CPAD_POS = "MA05" AND CPROSS_ID = "MAVMREF   "
IF FOUND()
  REPLACE CPROSS_ID WITH "MAMATREF  "
ENDIF

IF llMenuUsed
  USE IN SYCMENU
ENDIF 

IF (TYPE("gcRepHome") = "C") AND FILE(gcRepHome + "MA\MAVMREF.FXP")
  ERASE (gcRepHome + "MA\MAVMREF.FXP")
  ERASE (gcRepHome + "MA\MAVMREF.FRX")
  ERASE (gcRepHome + "MA\MAVMREF.FRT")
ENDIF


WAIT CLEAR
SELECT (lnCurrAlis)
*-- end of the Fix Program.

*!*************************************************************
*! Name      : lfFixForm
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/23/2003
*! Purpose   : Fix form code header file.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : form code id
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfFixForm
PARAMETERS lcFormId

STORE .F. TO llSycComp , llSyFrmCdH

WAIT WINDOW 'Opening Companies file' NOWAIT

IF !USED('SYFRMCDH')
  USE (gcSysHome+'SYFRMCDH') SHARED IN 0
  llSyFrmCdH = .T.
ENDIF

SELECT SYFRMCDH
SET ORDER TO 1
IF SEEK(lcFormId)
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
  USE (gcSysHome+'SYCCOMP') SHARED IN 0
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