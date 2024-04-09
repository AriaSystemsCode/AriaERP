*:************************************************************************
*: Program file  : ICCONFG ( N037930)
*: Program desc. : Update Default Configuration Code
*: For screen    : ICCONFG.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: WSH - WAEL M. ABO-SHAWAREB
*:************************************************************************
*: Calls : 
*:         Functions  : gfSetup,gfOpenFile ,gfModalGen
*:                    : gfBrows 
*:*************************************************************
*: Passed Parameters  : .... 
*:*************************************************************
*: Modifications      :
*:****************************************************************

*--Variable Declaration
*--lcConfCode   --------------------> Variable to get New Configuration Code
*--lcOldConf    --------------------> Variable to Hold Old Configuration Code
*--llUseDye     --------------------> Variable to Hold "Use Dyelot" Setup Vayiable
*--lcUseConf    --------------------> Variable to Hold "Use Configuration" Setup Vayiable

PRIVATE llUseDye, llUseConf

IF !gfSetup()
  RETURN
ENDIF

Store Space(10) To lcConfCode
Store Space(10) To lcOldConf
= lfSetMemVar('M_DYELOT', 'Y')
= lfSetMemVar('M_STYCNFG', 'Y')
lcOldConf  = lfGetMemVar('M_CONFCODE')

lcOldConf = IIF(EMPTY(lcOldConf), SPACE(10), lcOldConf)
lcConfCode = lcOldConf

PUSH KEY
ON KEY

DO (gcScrDir + gcWinAppl + '\ICConfg.SPX')

POP KEY

*!*************************************************************
*! Name      : lfVUPDATE
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/11/2004
*! Purpose   : Update Transaction Files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVUPDATE()
*!*************************************************************

FUNCTION lfVUPDATE

PRIVATE llAddRec, lcUpdTable, lcForCond, lcDyeName, lcUpdTables

lcUpdTables = 'INVTADJ,CUTTKTL,RALINE,CTKTBOM,POSLN,BOMLINE,CONSINVL,INVLINE,ORDCANLN,RETLINE,' +;
              'PIKLINE,ORDLINE,BOMCOST,STYINVJL,APINVTKT,APVINVDT,CTKTRCVL,SPCK_LIN,SPCK_HDR'

WAIT WINDOW 'Updating Configuration...' NOWAIT
=gfOpenFile(gcDataDir + 'STYLE', 'STYLE', 'SH')
=gfOpenFile(gcDataDir + 'STYDYE', 'STYDYE', 'SH')
=gfOpenFile(gcDataDir + 'WAREHOUS', 'WAREHOUS', 'SH')
=gfOpenFile(gcSysHome + 'SYDFILES', 'CFILE_NAM', 'SH')

SELECT SYDFILES
LOCATE FOR UPPER(CFILE_NAM) = 'STYDYE'

SELECT STYLE
SCAN
   WAIT WINDOW 'Updating ' + ALLT(SydFiles.cFile_ttl) + ' - Style : "' + Style.Style + '"' NOWAIT
   REPLACE cdye_flg WITH 'Y'
   SELECT WAREHOUS
   SCAN
      SELECT STYDYE
      llAddRec = .F.
      =SEEK(style.style+warehous.cWareCode,"STYDYE")
      SCAN REST WHILE Style+cWareCode+dyelot = style.style+warehous.cWareCode
         IF EMPTY(dyelot)
            SCATTER TO MEMVAR
            llAddRec = .T.
         ELSE
            llAddRec = .F.
            EXIT
         ENDIF
      ENDSCAN
      IF llAddRec
         SELECT STYDYE
         APPEND BLANK
         GATHER FROM MEMVAR
         REPLACE dyelot    WITH lcConfCode, ;
                 desc      WITH Style.desc, ;
                 ave_cost  WITH 0, ;
                 cadd_user WITH 'ICCONFG', ;
                 cadd_time WITH TIME(), ;
                 dadd_date WITH gdSysDate, ;
                 nStkVal   WITH 0
      ENDIF
   ENDSCAN
ENDSCAN

USE IN STYLE
USE IN STYDYE
USE IN WAREHOUS

=gfOpenFile(gcSysHome + 'SYDFLFLD', 'CFLD_NAME', 'SH')
SELECT SYDFLFLD
SCAN FOR cFld_Name = 'DYELOT' OR cFld_Name = 'CDYELOT'
   lcTable = UPPER(ALLTRIM(cFile_Nam))
   IF NOT ((lcTable $ lcUpdTables) AND FILE(gcDataDir + lcTable + '.DBF'))
      LOOP
   ENDIF
   =gfOpenFile(gcDataDir + lcTable, '', 'SH')
   lcDyeName = ALLTRIM(SYDFLFLD.cFld_Name)
   lcForCond = 'FOR  Empty(' + lcDyeName + ')' && + lcDyeName + ' = "' + lcOldConf + '"'
   DO CASE
      CASE lcTable = 'CTKTBOM'
         lcForCond = lcForCond + " AND CCATGTYP = 'S'"
      CASE lcTable = 'BOMCOST'
         lcForCond = lcForCond + " AND CCOSTTYPE = 'S'"
      CASE lcTable = 'APINVTKT' OR lcTable = 'APVINVDT'
         lcForCond = lcForCond + " AND CIMTYP $ 'SIR'"
      CASE lcTable = 'APINVTKT' OR lcTable = 'SPCK_HDR'
         lcForCond = lcForCond + " AND Type == 'S'" + ;
                     IIF(TYPE('SPCK_HDR.cSkuType') == 'C', " AND cSkuType == 'S'", "")
   ENDCASE
   SELECT SYDFILES
   LOCATE FOR UPPER(CFILE_NAM) = lcTable
   WAIT WINDOW 'Updating ' + ALLT(cFile_ttl) + ' ...' NOWAIT
   SELECT (lcTable)
   REPLACE ALL (lcDyeName) WITH lcConfCode &lcForCond
   USE IN (lcTable)
ENDSCAN

=lfSetMemVar('M_CONFCODE', lcConfCode)
WAIT WINDOW 'Updating complete...' NOWAIT

RETURN

*!*************************************************************
*! Name      : lfVREENTER
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/11/2004
*! Purpose   : Restore lcConfCode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVREENTER()
*!*************************************************************

FUNCTION lfVREENTER

lcConfCode = Space(10)
_CUROBJ = 1

RETURN

*!*************************************************************
*! Name      : lfGETMEMVAR
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/11/2004
*! Purpose   : Get Setup Variable Value
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcVarName .... Variable to return its Value
*!*************************************************************
*! Returns            : Value For the Setup Variable
*!*************************************************************
*! Example   : = lfGETMEMVAR('M_CONFCODE')
*!*************************************************************

FUNCTION lfGetMemVar
PARAMETER lcVarName

PRIVATE RetVal

RetVal = ''

=gfOpenFile(gcDataDir + 'SETUPS', 'MODVAR', 'SH')
SELECT SETUPS
LOCATE FOR cApp_id + cFld_name = 'IC' + lcVarName
IF !Found()
   =gfOpenFile(gcSysHome + 'SYCCONFG', 'MODVAR', 'SH')
   SELECT SYCCONFG
   LOCATE FOR cApp_id + cFld_name = 'IC' + lcVarName
   If Found()
      RetVal = mData_Def
   EndIf
   SELECT SYCCONFG
   USE
Else
   RetVal = mData_Def
EndIf

SELECT SETUPS
USE
RETURN RetVal

*!*************************************************************
*! Name      : lfSETMEMVAR
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/11/2004
*! Purpose   : Set Setup Variable Value
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcVarName .... Variable to Change its Value
*!                    : lcValue   .... New Variable Value to Set
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSETMEMVAR('M_CONFCODE', lcValue)
*!*************************************************************

FUNCTION lfSetMemVar
PARAMETER lcVarName, lcValue

=gfOpenFile(gcDataDir + 'SETUPS', 'MODVAR', 'SH')
SELECT SETUPS
LOCATE FOR cApp_id + cFld_name = 'IC' + lcVarName
IF !Found()
   =gfOpenFile(gcSysHome + 'SYCCONFG', 'MODVAR', 'SH')
   SELECT SYCCONFG
   LOCATE FOR cApp_id + cFld_name = 'IC' + lcVarName
   If Found()
      SCATTER TO MEMVAR MEMO
      SELECT SETUPS
      APPEND BLANK
      GATHER FROM MEMVAR MEMO
      REPLACE mData_Def WITH lcValue
   EndIf
   SELECT SYCCONFG
   USE
Else
   REPLACE mData_Def WITH lcValue
EndIf

SELECT SETUPS
USE
RETURN
