*:************************************************************
*: Program file  : SOGTEMPL.PRG
*: Program desc. : Employee/Wearer File:
*:               : This screen is a copy of "SOEMPL" screen that is called as modal from soord screen
*:               : so we can select or add a new employee
*: For screen    : SOEMPL.SPR
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (SO).
*:     Developer : TMI - TAREK MOHAMMED IBRAHIM
*: Date          : 12/09/2004
*: Entry         : C # 123847,1
*:************************************************************
PARAMETERS lcAccount,lcStore


PRIVATE lcScFields, lcSvProc, lcKeyStat, lcObjStat,;
        lcUnfrm, lcEntUsed, lcOldValue, lcUniDesc,;
        lcScrTitle
STORE '' TO lcScFields, lcSvProc, lcKeyStat, lcObjStat,;
        lcUnfrm, lcEntUsed, lcOldValue, lcUniDesc

lcKeyStat = 'ENABLE'
lcObjStat = 'DISABLE'

*lcScrTitle = 'Employees Assigned to Account '+lcAccount + IIF(!EMPTY(lcStore),',  Store : '+lcStore,'')
lcScrTitle = 'Assign a new employee to the Account '+lcAccount + IIF(!EMPTY(lcStore),',  Store : '+lcStore,'')
lcSvProc = SET('PROCEDURE')
SET PROCEDURE TO (gcAppHome+'SO\SOEMPL.PRG') ADDITIVE

*- Browse variable
llBrowse = .F.

*- Initialize state variables for the screen

=gfOpenFile(gcDataDir+'UNIFORM' ,'UNIFORMLN','SH')
=gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE' ,'SH')
=gfOpenFile(gcDataDir+'STYHIST' ,'STYHIST'  ,'SH')

PRIVATE laScrMode, laData,lcTmpEmpl

lcTmpEmpl = gfTempName()
lcScFields = 'ACCOUNT   ,STORE     ,EMPLOYEE  ,EMPNAME   ,EMPADD1   ,' +;
             'EMPADD2   ,EMPADD3   ,EMPADD4   ,EMPADD5   ,PAYROLLNO ,' +;
             'SITE_NO   ,DSTART    ,UCODE     ,NPERIOD   ,DEND      '
DIMENSION laData[15]
DIMENSION laScrMode[4]

SELECT EMPLOYEE
SET FILTER TO ACCOUNT+STORE+EMPLOYEE = lcAccount+lcStore
SET RELATION TO UCODE INTO UNIFORM
GO TOP

SCATTER FIELDS &lcScFields TO laData BLANK
laData[1] = lcAccount
laData[2] = lcStore

lcUnfrm   = gfTempName()
lcEntUsed = gfTempName()
=lfFilTmpBs()
  
SELECT EMPLOYEE

PUSH KEY                                     && To save the the current on key label
ON KEY
DO (gcScrDir + gcWinAppl + '\SOGTEMPL.SPR')    && calling the screen "SOEMPL"
POP KEY                                      && To Restore the previous assignments for on key label

IF USED(lcUnfrm)
  USE IN &lcUnfrm
ENDIF
ERASE (gcWorkDir+lcUnfrm+'.DBF')
ERASE (gcWorkDir+lcUnfrm+'.CDX')

IF USED(lcEntUsed)
  USE IN &lcEntUsed
ENDIF
ERASE (gcWorkDir+lcEntUsed+'.DBF')
ERASE (gcWorkDir+lcEntUsed+'.CDX')

SET PROCEDURE TO &lcSvProc


*:**************************************************************************
*:* Name        : lfvEmployee
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on DATE CREATED field
*:***************************************************************************
FUNCTION lfvEmployee
PRIVATE lcModalMes,lcEmp

IF !llBrowse .AND. EMPTY(laData[3])
  SELECT EMPLOYEE
  SCATTER FIELDS &lcScFields TO laData BLANK
  =lfRefresh()
  RETURN
ENDIF
IF CHRTRAN(laData[3],'?/\','') <> laData[3]
  llBrowse = .T.
ENDIF
IF llBrowse
  =lfBrowEmp()
  IF !EMPTY(laData[3])
    =SEEK(laData[1]+laData[2]+laData[3],'EMPLOYEE')
    SELECT EMPLOYEE
    SCATTER FIELDS &lcScFields TO laData
    STORE 'ENABLE' TO lcKeyStat,lcObjStat
    =lfShowGets(.F.)
    
    SHOW GET pbNx      ENABLE
    SHOW GET pbPr      ENABLE
    SHOW GET pbSelect  ENABLE
    SHOW GET pbCancel  ENABLE
    SHOW GET pbSave    DISABLE
    SHOW GET pbStyHist DISABLE
  ENDIF
ELSE
  IF !EMPTY(laData[3])
    IF SEEK(laData[1]+laData[2]+laData[3],'EMPLOYEE')
      
      SELECT EMPLOYEE
      SCATTER FIELDS &lcScFields TO laData
      STORE 'ENABLE' TO lcKeyStat,lcObjStat
      =lfShowGets(.F.)

      SHOW GET pbNx      ENABLE
      SHOW GET pbPr      ENABLE
      SHOW GET pbSelect  ENABLE
      SHOW GET pbCancel  ENABLE
      SHOW GET pbSave    DISABLE
      SHOW GET pbStyHist DISABLE
      
    ELSE
      
      lcModalMes = 'Employee Code : ' + ALLTRIM(laData[1])+' '
      lnResp = gfModalGen('QRM00001B42003','DIALOG',lcModalMes)
      
      DO CASE
      CASE lnResp = 1 && Add
        SELECT EMPLOYEE
        lcEmp = laData[3]
        SCATTER FIELDS &lcScFields TO laData BLANK
        laData[1] = lcAccount
        laData[2] = lcStore
        laData[3] = lcEmp
        STORE 'ENABLE' TO lcKeyStat,lcObjStat
        =lfShowGets(.T.)

        SHOW GET pbNx      DISABLE
        SHOW GET pbPr      DISABLE
        SHOW GET pbSelect  DISABLE
        SHOW GET pbCancel  ENABLE
        SHOW GET pbSave    ENABLE
        SHOW GET pbStyHist ENABLE
        
      CASE lnResp = 2 && Browse
        =lfBrowEmp()
        IF !EMPTY(laData[3])
        
          SELECT EMPLOYEE
          SCATTER FIELDS &lcScFields TO laData
          STORE 'ENABLE' TO lcKeyStat,lcObjStat
          =lfShowGets(.F.)

          SHOW GET pbNx      ENABLE
          SHOW GET pbPr      ENABLE
          SHOW GET pbSelect  ENABLE
          SHOW GET pbCancel  ENABLE
          SHOW GET pbSave    DISABLE
          SHOW GET pbStyHist DISABLE
          
        ELSE
        
          SELECT EMPLOYEE
          SCATTER FIELDS &lcScFields TO laData BLANK
          laData[1] = lcAccount
          laData[2] = lcStore
          STORE 'ENABLE' TO lcKeyStat,lcObjStat
          =lfShowGets(.F.)
          
          SHOW GET pbNx      DISABLE
          SHOW GET pbPr      DISABLE
          SHOW GET pbSelect  DISABLE
          SHOW GET pbCancel  ENABLE
          SHOW GET pbSave    DISABLE
          SHOW GET pbStyHist DISABLE
        
        ENDIF
          
        
      CASE lnResp = 3 && Reenter
        SELECT EMPLOYEE
        SCATTER FIELDS &lcScFields TO laData BLANK
        laData[1] = lcAccount
        laData[2] = lcStore
        lcKeyStat = 'ENABLE'
        lcObjStat = 'DISABLE'
        =lfShowGets(.F.)
        SHOW GET pbSave DISABLE
        _CUROBJ = OBJNUM(laData[3])
    
      ENDCASE
    ENDIF  
  
  ENDIF
ENDIF  
=lfRefresh()
llBrowse = .F.
 
*:**************************************************************************
*:* Name        : lfBrowEmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Get employee from a browse
*:***************************************************************************
FUNCTION lfBrowEmp
PRIVATE laTemp,lcFieldsNam,lcBrFields

SELECT ACCOUNT,STORE,EMPLOYEE,EMPNAME ;
  FROM EMPLOYEE ;
  WHERE ACCOUNT = lcAccount .AND. STORE = lcStore ;
  INTO TABLE (gcWorkDir+lcTmpEmpl)
INDEX ON ACCOUNT+STORE+EMPLOYEE TAG &lcTmpEmpl
GO TOP
DIMENSION laTemp[4]
latemp = ''
lcFieldsNam = 'ACCOUNT,STORE,EMPLOYEE,EMPNAME'
lcBrFields  = 'EMPLOYEE:H="Employee Code",'+;
              'EMPNAME :H="Employee Name"'
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl
=Ariabrow('','Employees',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
             lcFieldsNam,'laTemp')
USE IN &lcTmpEmpl
ERASE(gcWorkDir+lcTmpEmpl+'.DBF')
ERASE(gcWorkDir+lcTmpEmpl+'.CDX')
POP KEY
laData[3] = ''
IF !EMPTY(laTemp[1])
  SELECT EMPLOYEE
  =SEEK(laTemp[1]+laTemp[2]+laTemp[3],'EMPLOYEE')
  laData[1] = laTemp[1]
  laData[2] = laTemp[2]
  laData[3] = laTemp[3]
  laData[4] = laTemp[4]
ENDIF
*-- end of lfBrowEmp.

*:**************************************************************************
*:* Name        : lfvNext
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Skip to next employee
*:***************************************************************************
FUNCTION lfvNext
SELECT EMPLOYEE
SKIP
IF EOF()
  GO BOTTOM
ENDIF
SCATTER FIELDS &lcScFields TO laData
lcUniDesc = UNIFORM.UDESC
STORE 'ENABLE' TO lcKeyStat,lcObjStat
=lfShowGets(.F.)

SHOW GET pbSelect  ENABLE
SHOW GET pbCancel  ENABLE
SHOW GET pbSave    DISABLE
SHOW GET pbStyHist DISABLE
=lfRefresh()
*-- end of lfvNext.
*:**************************************************************************
*:* Name        : lfvPrev
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Skip to previous employee
*:***************************************************************************
FUNCTION lfvPrev
SELECT EMPLOYEE
SKIP -1
IF BOF()
  GO TOP
ENDIF
SCATTER FIELDS &lcScFields TO laData
lcUniDesc = UNIFORM.UDESC
STORE 'ENABLE' TO lcKeyStat,lcObjStat
=lfShowGets(.F.)

SHOW GET pbSelect  ENABLE
SHOW GET pbCancel  ENABLE
SHOW GET pbSave    DISABLE
SHOW GET pbStyHist DISABLE
=lfRefresh()
*-- end of lfvPrev.

*:**************************************************************************
*:* Name        : lfvCan
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Close temp screen for adding employees
*:***************************************************************************
FUNCTION lfvCan
lcEmployee = ''
CLEAR READ

*-- end of lfvCan.

*:**************************************************************************
*:* Name        : lfvSlct
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Select an employee
*:***************************************************************************
FUNCTION lfvSlct
lcSto      = laData[2]
lcEmployee = laData[3]
CLEAR READ

*-- end of lfvSlct.

*:**************************************************************************
*:* Name        : lfvSv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Save an added employee
*:***************************************************************************
FUNCTION lfvSv
PRIVATE lnSlct,llCSave
lnSlct = SELECT()

llCSave = .T.
DO CASE
CASE EMPTY(laData[4])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Employee Name, Can not save')
  _CUROBJ = OBJNUM(laData[4])
  llCSave = .F.   
CASE EMPTY(laData[12])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Start Date, Can not save')
  _CUROBJ = OBJNUM(laData[12])
  llCSave = .F.   
CASE EMPTY(laData[14])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Calculation Period, Can not save')
  _CUROBJ = OBJNUM(laData[14])
  llCSave = .F.   
CASE EMPTY(laData[15])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty End Date, Can not save')
  _CUROBJ = OBJNUM(laData[15])
  llCSave = .F.   
ENDCASE  

IF llCSave
  *- Save Entitlment Used field in StyHist file
  =lfSvUsed()
  
  SELECT EMPLOYEE 
  APPEND BLANK
  GATHER FIELDS &lcScFields FROM laData
  =lfAudtFlds()
  
  =lfShowGets(.F.)
  SHOW GET pbSelect  ENABLE
  SHOW GET pbCancel  ENABLE
  SHOW GET pbSave    DISABLE
  SHOW GET pbStyHist DISABLE
ENDIF

*-- end of lfvSv.

*:**************************************************************************
*:* Name        : lfShowGets
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Enable / disable fields
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShowGets()
*:***************************************************************************
FUNCTION lfShowGets
PARAMETERS llEnable
PRIVATE lcStat

lcStat = IIF(llEnable,'ENABLE','DISABLE')
lcPrNx = IIF(llEnable,'DISABLE','ENABLE')

SHOW GET laData[3 ]

SHOW GET laData[4 ] &lcStat
SHOW GET laData[5 ] &lcStat
SHOW GET laData[6 ] &lcStat
SHOW GET laData[7 ] &lcStat
SHOW GET laData[8 ] &lcStat
SHOW GET laData[9 ] &lcStat
SHOW GET laData[10] &lcStat
SHOW GET laData[11] &lcStat
SHOW GET laData[12] &lcStat
SHOW GET laData[13] &lcStat
SHOW GET laData[14] &lcStat
SHOW GET laData[15] &lcStat
SHOW GET pbUniform  &lcStat

SHOW GET pbNx       &lcPrNx
SHOW GET pbPr       &lcPrNx

*-- end of lfShowGets.

*:**************************************************************************
*:* Name        : lfvEdtUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Edit used field
*:***************************************************************************
FUNCTION lfvEdtUsed
laScrMode[4] = .T.
=lfEdtUsed()
*-- end of lfvEdtUsed.
