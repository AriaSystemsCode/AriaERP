*:***************************************************************************
*: Program file  : POCRTMP
*: Program desc. : Critical Path Template Code
*: For screen    : POCRPTMP.SCX,POCRTMP1.SCX,POCRTMP2.SCX,POCRTMP3.SCX
*:        System : Aria Advantage Series.
*:        Module : Style Purchase Order (PO)
*:        Date   : 12/04/2001
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POCRTMP
*:***************************************************************************
*: Due to C200249,1
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*

EXTERNAL ARRAY laDefProc,laData,laKeyField

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
DECLARE laKeyField [1,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CRPTMPHD'
laKeyField[1,4] = 1

STORE .T. TO llAlowNew
STORE ' ' TO laCodInfo
STORE ' ' TO laGroup
STORE 1   TO lnGroup
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
glFromBrow = .T.
lcTmpCrLn  = gfTempName()     && File to hold CrPTmpLn records and to brows from it.
lcTmpCrLn1 = gfTempName()     && Temp. index for the temp file of CrPTmpLn.
lcCrLnBrow = 'Operations'
llBrowse = .F.

IF !gfSetup()
  RETURN
ENDIF

*-- Create Group Array [Begin]
DIMENSION laCodInfo [1,10]
laCodInfo[1,01] = "CCRPTHGRP"
laCodInfo[1,02] = "laGroup"
laCodInfo[1,03] = "lnGroup"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,07] = "CRPTMPLN"
laCodInfo[1,08] = "CRPTMPLN"
laCodInfo[1,09] = ""
laCodInfo[1,10] = "cCrPthGrp"

DIMENSION laGroup [1,2]
STORE "" TO laGroup

=gfwCodePop ( @laCodInfo, "CCRPTHGRP" ,"L" )

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = "cCrPTmpCod,cCrPTmpDes,nLineNo"
  SCATTER FIELDS &lcScFields. TO laData BLANK
  =lfCreatTmp()
ENDIF

DO (gcScrDir+gcWinAppl+"\POCRPTMP.SPX")
ON KEY LABEL ALT+B 

*!**************************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : procedure to refresh screen objects.
*!**************************************************************************
*
PROCEDURE lpShow
 
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[2] .OR. laScrMode[1] && VIEW & SELECT Mode
    =lfGetInfo()
    =lfwBrows()
  CASE laScrMode[3]                   && EDIT Mode
    IF EOF(lcTmpCrLn)
      SHOW GETS WINDOW POCRTMP3 DISABLE ONLY
      SHOW GET pbAdd     ENABLE
      _CUROBJ = OBJNUM(pbAdd)
    ENDIF
  CASE laScrMode[4]                   && ADD  Mode
    =lfGetInfo()
    =lfwBrows()
    SHOW GETS WINDOW POCRTMP1 DISABLE ONLY
    SHOW GETS WINDOW POCRTMP3 DISABLE ONLY
    SHOW GET pbSav DISABLE
    SHOW GET laData[1] Enable
    
    *C200249,4 AMH Fix the cursor position when press Ctrl+Ins [Start]
    ACTIVATE WINDOW POCRTMP1 TOP
    *C200249,4 AMH [End]
    
    _CUROBJ = OBJNUM(laData[1])
ENDCASE

*C200249,4 AMH Disable the user defined screen [Start]
SHOW GET pbUsrFields DISABLE
*C200249,4 AMH [End]

*-- end of lpShow.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcCrLnBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B DO lfDispBrow
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW POCRTMP3 TOP
  _CUROBJ = OBJNUM(lnGroup)

ELSE  && Backtab Case.
  ACTI WINDOW POCRTMP1 TOP
  _CUROBJ = OBJNUM(laData[2])
ENDIF
*-- end of lfTraps.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcCrLnBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpCrLn)
lcBrowFild = "nLineNo                                     :H = 'Line No.'        :6  :R,"+;
             "lcGroup = gfCodDes(CCRPTHGRP , 'CCRPTHGRP') :H = 'Group'           :45 :R,"+;
             "cCrPthOpr                                   :H = 'Operation'       :60 :R,"+;
             "nLeadTime                                   :H = 'Lead Time(Days)' :6  :R,"+;
             "llShowCust = IIF(lShowCust,'Y','N')         :H = 'Show Customer'   :2  :R,"+;
             "cUser_Id                                    :H = 'User'            :15 :R,"+;
             "llAutRecalc = IIF(lAutRecalc,'Y','N')       :H = 'Auto Recalc.'    :2  :R"
BROWSE FIELDS &lcBrowFild     ;
       WINDOW POCRTMP2        ;
       WHEN lfwBrows()        ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcCrLnBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
SCATTER MEMVAR
IF ASCAN(laGroup,m.cCrPthGrp) > 0
  lnGroup = ASUBSCRIPT(laGroup,ASCAN(laGroup,m.cCrPthGrp),1)
ENDIF
SHOW GET m.nLineNo
SHOW GET lnGroup
SHOW GET m.cCrPthOpr
SHOW GET m.nLeadTime
SHOW GET m.cUser_Id
SHOW GET m.lShowCust
SHOW GET m.lAutRecalc
SHOW WINDOW (lcCrLnBrow) REFRESH SAME
SELECT (lnAlias)
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Function to create tmp file of CRPTMPLN.
*!*************************************************************
*
FUNCTION lfCreatTmp

SELECT CRPTMPLN
=AFIELDS(laFStru)

DIMENSION laFStru[ALEN(laFStru,1)+2,4]

laFStru[ALEN(laFStru,1),1] = 'nOldLineNo'
laFStru[ALEN(laFStru,1),2] = 'N'
laFStru[ALEN(laFStru,1),3] = 4
laFStru[ALEN(laFStru,1),4] = 0

laFStru[ALEN(laFStru,1)-1,1] = 'cStatus'
laFStru[ALEN(laFStru,1)-1,2] = 'C'
laFStru[ALEN(laFStru,1)-1,3] = 1
laFStru[ALEN(laFStru,1)-1,4] = 0

CREATE CURSOR (lcTmpCrLn) FROM ARRAY laFStru
SELECT (lcTmpCrLn)
INDEX ON cCrPTmpCod+STR(nLineNo,4)+cStatus TAG (lcTmpCrLn) OF (lcTmpCrLn)
INDEX ON cCrPTmpCod+cStatus+STR(nLineNo,4) TAG (lcTmpCrLn1) OF (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
*-- end of lfCreatTmp.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/04/2001
*! Purpose   : Function to collect data from CRPTMPLN to tmp file.
*!*************************************************************
*
FUNCTION lfGetInfo

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
ZAP
IF SEEK(laData[1],'CRPTMPLN')
  SELECT CRPTMPLN
  SCAN REST WHILE CCRPTMPCOD+STR(NLINENO,4) = laData[1]
    SCATTER MEMVAR
    m.nOldLineNo = m.nLineNo
    m.cStatus    = 'O'
    SELECT (lcTmpCrLn)
    APPEND BLANK
    GATHER MEMVAR
  ENDSCAN
ENDIF
SELECT (lcTmpCrLn)
LOCATE
=lfDispBrow()
SELECT (lnAlias)
*-- end of lfGetInfo.

*!*************************************************************
*! Name      : lfvCode
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/05/2001
*! Purpose   : Valid function of code field.
*!*************************************************************
*
FUNCTION lfvCode

PRIVATE lnAlias
IF llBrowse .OR. LASTKEY()=13 .OR. LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

lnAlias = SELECT(0)
IF laScrMode[4]
  IF !EMPTY(laData[1])
    IF SEEK(laData[1],'CRPTMPHD')
      =gfModalGen('INM34050B34000','','',.F.,;
                  'This critical path template code already exists. Please enter another code.')
      laData[1] = ''
      _CUROBJ = OBJNUM(laData[1])
      SHOW GET laData[1] ENABLE
    ELSE
      SHOW GETS WINDOW POCRTMP1 DISABLE ONLY
      SHOW GET laData[2] ENABLE
      SHOW GET pbAdd     ENABLE
      SHOW GET pbSav     ENABLE
    ENDIF
  ENDIF
ELSE
  IF llbrowse OR (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'CRPTMPHD'))
    SELECT CRPTMPHD
    llbrowse = .F.
    DIMENSION laTempData[1]
    STORE '' TO laTempData
    
    *C200249,4 AMH Fix browse is empty if press in code browse [Start]
    *lcKeyVal = IIF(EOF(),"[]","[laData(1)]")
    lcKeyVal = "[]"
    *C200249,4 AMH [End]

    lcFile_Ttl = 'Critical Path Template Code'
    lcBrFields = "cCrPTmpCod  :H='Code',"+;
                 "cCrPTmpDes  :H='Description'"
    =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'cCrPTmpCod','laTempData')
    laData[1] = laTempData[1]
  ENDIF
  IF !EMPTY(laData[1])
    laScrMode    = .F.
    laScrMode[2] = .T.
    =gfSeekRec()
    =lfGetInfo()
  ENDIF
ENDIF
SELECT (lnAlias)
*-- end of lfvCode.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*-- IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfvAdd
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Add button.
*!*************************************************************
*
FUNCTION lfvAdd

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
APPEND BLANK
laData[3] = laData[3] + 1
REPLACE NLINENO    WITH laData[3],;
        NOLDLINENO WITH NLINENO,;
        CCRPTMPCOD WITH laData[1],;
        CSTATUS    WITH 'N',;
        CCRPTHGRP  WITH laGroup[1,2],;
        LSHOWCUST  WITH .F.,;
        LAUTRECALC WITH .T.
=lfwBrows()
SHOW GETS WINDOW POCRTMP3 ENABLE ONLY
_CUROBJ = OBJNUM(lnGroup)
SELECT (lnAlias)
*-- end of lfvAdd.

*!*************************************************************
*! Name      : lfvGroup
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Group field.
*!*************************************************************
*
FUNCTION lfvGroup

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
m.cCrPthGrp = laGroup[lnGroup,2]
=lfChnStats('CCRPTHGRP')
REPLACE CCRPTHGRP  WITH m.cCrPthGrp
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvGroup.

*!*************************************************************
*! Name      : lfvOpr
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Operation field.
*!*************************************************************
*
FUNCTION lfvOpr

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('CCRPTHOPR')
REPLACE CCRPTHOPR  WITH m.cCrPthOpr
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvOpr.

*!*************************************************************
*! Name      : lfvLdTime
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Lead Time field.
*!*************************************************************
*
FUNCTION lfvLdTime

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('NLEADTIME')
REPLACE NLEADTIME WITH m.nLeadTime
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvLdTime.

*!*************************************************************
*! Name      : lfvShCust
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Show to Customer field.
*!*************************************************************
*
FUNCTION lfvShCust

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('LSHOWCUST')
REPLACE LSHOWCUST WITH m.lShowCust
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvShCust.

*!*************************************************************
*! Name      : lfvAutRecl
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Auto Recalc. field.
*!*************************************************************
*
FUNCTION lfvAutRecl

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('LAUTRECALC')
REPLACE LAUTRECALC WITH m.lAutRecalc
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvAutRecl.

*!*************************************************************
*! Name      : lfvUser
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of User field.
*!*************************************************************
*
FUNCTION lfvUser

PRIVATE lnAlias
IF llBrowse .OR. LASTKEY()=13 .OR. LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

lnAlias = SELECT(0)
IF llbrowse OR (!EMPTY(m.cUser_Id) .AND. !SEEK(m.cUser_Id,'SYUUSER'))
  SELECT SYUUSER
  llbrowse = .F.
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  
  *C200249,4 AMH Fix browse is empty if press in user browse [Start]
  *lcKeyVal = IIF(EOF(),"[]","[m.cUser_Id]")
  lcKeyVal = "[]"
  *C200249,4 AMH [End]
  
  lcFile_Ttl = 'Users'
  lcBrFields = "cUser_Id  :H='User Id',"+;
               "cUsr_Name :H='User Name',"+;
               "cUsr_Loca :H='User Location',"+;
               "cUsr_Phon :H='Phone No.',"+;
               "cUsr_Levl :H='User Level'"
  =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'cUser_Id','laTempData')
  m.cUser_Id = laTempData[1]
ENDIF
IF !EMPTY(m.cUser_Id)
  SELECT (lcTmpCrLn)
  =lfChnStats('CUSER_ID')
  REPLACE CUSER_ID WITH m.cUser_Id
  =lfwBrows()
ENDIF
SELECT (lnAlias)
*-- end of lfvUser.

*!*************************************************************
*! Name      : lfChnStats
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : function to change status
*!*************************************************************
*
FUNCTION lfChnStats
PARAMETERS lcField

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
IF EVALUATE(lcField) # EVALUATE('m.'+lcField) .AND. CSTATUS = 'O'
  REPLACE CSTATUS WITH 'M'
ENDIF
SELECT (lnAlias)
*-- end of lfChnStats.

*!*************************************************************
*! Name      : lfvInsert
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Insert button.
*!*************************************************************
*
FUNCTION lfvInsert

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
laData[3] = laData[3] + 1
=lfReNumber(1)
APPEND BLANK
REPLACE NLINENO    WITH m.nLineNo+1,;
        NOLDLINENO WITH NLINENO,;
        CCRPTMPCOD WITH laData[1],;
        CSTATUS    WITH 'N',;
        CCRPTHGRP  WITH laGroup[1,2],;
        LSHOWCUST  WITH .F.,;
        LAUTRECALC WITH .T.
=lfwBrows()
_CUROBJ = OBJNUM(lnGroup)
SELECT (lnAlias)
*-- end of lfvInsert.

*!*************************************************************
*! Name      : lfvRemove
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : Valid function of Remove button.
*!*************************************************************
*
FUNCTION lfvRemove

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
laData[3] = laData[3] - 1
REPLACE CSTATUS    WITH 'D'
DELETE
=lfReNumber(-1)
LOCATE
=lfwBrows()
_CUROBJ = OBJNUM(lnGroup)
IF EOF(lcTmpCrLn)
  SHOW GETS WINDOW POCRTMP3 DISABLE ONLY
  SHOW GET pbAdd     ENABLE
  _CUROBJ = OBJNUM(pbAdd)
ENDIF
SELECT (lnAlias)
*-- end of lfvRemove.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : To Save record in CrPTmpHd file & related records in CrPTmpLn file.
*!*************************************************************
*!
PROCEDURE lpSavScr

PRIVATE lnAlias, lcDelSet
lnAlias = SELECT(0)

*-- Updating the header file.
SELECT CRPTMPHD
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FIELDS &lcScFields. FROM laData
=lfUpdUsrFl('CCRPTMPCOD',laData[1])
=lfUpdUsrFl('CCRPTMPDES',laData[2])

*-- Updating the lines file.
SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn1)

*-- Updating deleted records.
lcDelSet = SET('DELETE')
SET DELETE OFF
=lfScan('D')
SET DELETE &lcDelSet.

*-- Updating modified records.
=lfScan('M')

*-- Updating new records.
=lfScan('N')

SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
SELECT (lnAlias)
*--end of lpSavScr.

*!*************************************************************
*! Name      : lfScan
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : function to update CrPTmpLn file
*!*************************************************************
*
FUNCTION lfScan
PARAMETERS lcType

SEEK laData[1]+lcType
SCAN REST WHILE CCRPTMPCOD+CSTATUS+STR(NLINENO,4) = laData[1]+lcType
  DO CASE
    CASE lcType = 'N'
      SCATTER MEMVAR
      SELECT CRPTMPLN
      APPEND BLANK
      GATHER MEMVAR
    OTHERWISE
      IF SEEK(laData[1]+STR(nOldLineNo,4),'CRPTMPLN')
        IF lcType = 'M'
          SCATTER MEMVAR
          SELECT CRPTMPLN
          GATHER MEMVAR
        ELSE
          SELECT CRPTMPLN
          LOCATE REST WHILE CCRPTMPCOD+STR(NLINENO,4) = ;
                            laData[1]+STR(EVALUATE(lcTmpCrLn+'.nOldLineNo'),4);
                      FOR !DELETED()
          IF FOUND()
            DELETE
          ENDIF
        ENDIF
      ENDIF
  ENDCASE
ENDSCAN
*-- end of lfScan.

*!*************************************************************
*! Name      : lfRenumber
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/06/2001
*! Purpose   : function to recalculate nlineno field when insert or remove line
*!*************************************************************
*
FUNCTION lfRenumber
PARAMETERS lnSign

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
IF lnSign = 1
  SET ORDER TO TAG (lcTmpCrLn) DESCENDING
  LOCATE
ELSE
  SEEK laData[1]+STR(m.nlineno+1,4)
ENDIF

*C200249,4 AMH Fix error when insert after the last line [Start]
*SCAN REST WHILE CCRPTMPCOD+STR(NLINENO,4)+CSTATUS = laData[1]
SCAN REST WHILE CCRPTMPCOD+STR(NLINENO,4)+CSTATUS = laData[1];
          FOR IIF(lnSign=1,NLINENO>m.nLineNo,.T.)
*C200249,4 AMH [End]

  REPLACE NLINENO WITH NLINENO + lnSign
  IF CSTATUS = 'O'
    REPLACE CSTATUS WITH 'M'
  ENDIF
  
  *C200249,4 AMH Comment the next lines [Start]
  *IF lnSign = 1 .AND. NLINENO = m.nLineNo + 2
  *  EXIT
  *ENDIF
  *C200249,4 AMH [End]
  
ENDSCAN
IF lnSign = 1
  SET ORDER TO TAG (lcTmpCrLn) ASCENDING
ENDIF
*-- end of lfRenumber.

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/09/2001
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/09/2001
*! Purpose   : To Delete record in CrPTmpHd file and related records in CrPTmpLn file.
*!*************************************************************
*!
PROCEDURE lpDelScr

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Delete this record from the header file.
IF SEEK(laData[1],'CRPTMPHD')
  SELECT CRPTMPHD
  =gfObj_lock(.F.)
  DELETE
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF

*-- Delete all lines.
SELECT (lcTmpCrLn)
REPLACE ALL CSTATUS WITH 'D'
SET ORDER TO TAG (lcTmpCrLn1)
=lfScan('D')
SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
SELECT (lnAlias)
*--end of lpDelScr.