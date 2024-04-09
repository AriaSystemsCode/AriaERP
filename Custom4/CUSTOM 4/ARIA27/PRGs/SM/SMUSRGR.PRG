*:************************************************************************
*:
*: Procedure file: SMUSRGR.PRG 
*:                 (User groups)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*:E300683,4 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:B802353,1 M.H_NY 06/16/99 Increase the NRECNO field to No.
*:B804124,1 AMM 04/17/2001 Fix the bug of not saving users to groups
*:B038431,1 NNA 09/07/2004 Fix bug that you want to input user Group and there were no 
*:B038431,1 NNA            users input you'll error massage (Invalid Function Agrument Value)
*:B038431,1 NNA            that when you try to input 'New Member'
*:************************************************************************
EXTERNAL ARRAY laData,laScrMode,lakeyFields,laDefProc
DECLARE laMember[1],laKeyFields[1,4],laNewMem[1]

lakeyFields[1,1] = "laData[1]"
lakeyFields[1,2] = .T.
lakeyFields[1,3] = "cGroup_ID"
lakeyFields[1,4] = 1

laDefProc[9] = .F.
ladefproc[7] = .F.       

laMember     = ""
lcLastData   = ""
llUpdPriv    = .F.
llBrowse     = .F.

lcPrv_Tmp1   = " "
lcPrv_Tmp2   = " "
lcCopUsTmp   = " "

*B038431,1 NNA 09/07/2004 (Begin) if there are no Users ,Show this massage and Exit
DECLARE laUsers[1]
STORE '' TO laUsers
SELECT cUser_ID FROM (gcSysHome+"syuUser") INTO ARRAY laUsers 
IF EMPTY(laUsers)
  =gfModalGen("TRM00417B40011","DIALOG",'Users Group')
  RETURN
ENDIF
*B038431,1 NNA (End)

IF !gfSetup()    
  RETURN
ENDIF  

IF !WEXIST(lcBaseWind)
  SCATTER  FIELDS &lcScFields TO laData BLANK
  llUpdPriv  = .F.
  *** Get name for the user privilege temp. file. ***
  lcPrv_Tmp1 = gfTempName()
  lcPrv_Tmp2 = gfTempName()
  lcCopUsTmp = gfTempName()

  *** Create the user privilege temp. file. ***
  SELECT SYUUSRPR
  DIMENSION laUserPrvl[1,4]
  =AFIELDS(laUserPrvl)
  DIMENSION laUserPrvl[ALEN(laUserPrvl,1)+3,4]

  laUserPrvl[ALEN(laUserPrvl,1)-2,1] = 'NRECNO'
  laUserPrvl[ALEN(laUserPrvl,1)-2,2] = 'N'
  *B802353,1 M.H Begin.
  *laUserPrvl[ALEN(laUserPrvl,1)-2,3] = 3
  laUserPrvl[ALEN(laUserPrvl,1)-2,3] = 6
  *B802353,1 M.H End.
  laUserPrvl[ALEN(laUserPrvl,1)-2,4] = 0

  laUserPrvl[ALEN(laUserPrvl,1)-1,1] = 'CSTATUS'
  laUserPrvl[ALEN(laUserPrvl,1)-1,2] = 'C'
  laUserPrvl[ALEN(laUserPrvl,1)-1,3] = 1
  laUserPrvl[ALEN(laUserPrvl,1)-1,4] = 0

  laUserPrvl[ALEN(laUserPrvl,1),1] = 'LEXIST'
  laUserPrvl[ALEN(laUserPrvl,1),2] = 'L'
  laUserPrvl[ALEN(laUserPrvl,1),3] = 1
  laUserPrvl[ALEN(laUserPrvl,1),4] = 0
  
  CREATE DBF (gcWorkDir+lcPrv_Tmp1) FROM ARRAY laUserPrvl
  
  CREATE DBF (gcWorkDir+lcPrv_Tmp2) FROM ARRAY laUserPrvl
  
  CREATE DBF (gcWorkDir+lcCopUsTmp) FROM ARRAY laUserPrvl
  
  SELECT (lcPrv_Tmp2)
  INDEX ON ALLTRIM(CAPP_ID)+ALLTRIM(CPROSS_ID)+CPROCTYPE+ALLTRIM(CCOMP_ID) TAG CAPP_ID
  SET ORDER TO TAG CAPP_ID
ENDIF  

SELECT SYUGROUP
*E300683,4 Call *.SPR from screens directory
* DO SMUsrGr.SPR 
DO (gcScrDir + gcWinAppl + '\SMUsrGr.SPR')
*E300683,4 end   

IF glQuitting
  IF USED(lcPrv_Tmp1)
    USE IN ALIAS(lcPrv_Tmp1)
  ENDIF
  ERASE (gcWorkDir+lcPrv_Tmp1+".DBF")
  ERASE (gcWorkDir+lcPrv_Tmp1+".FPT")

  IF USED(lcPrv_Tmp2)
    USE IN ALIAS(lcPrv_Tmp2)
  ENDIF
  ERASE (gcWorkDir+lcPrv_Tmp2+".DBF")
  ERASE (gcWorkDir+lcPrv_Tmp2+".CDX")
  ERASE (gcWorkDir+lcPrv_Tmp2+".FPT")
  
  IF USED(lcCopUsTmp)
    USE IN ALIAS(lcCopUsTmp)
  ENDIF
  ERASE (gcWorkDir+lcCopUsTmp+".DBF")
  ERASE (gcWorkDir+lcCopUsTmp+".FPT")
ENDIF  

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

lcLastData = " "

DO CASE
  CASE laScrMode[1] 
    DIMENSION laMember[1]
    laMember =""
    lsMember = 1
  CASE laScrMode[2] OR laScrMode[3]
    IF laScrMode[2]
      *** Get privilege for the current group. ***
      SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
             FROM (gcSysHome+"SYUUSRPR") ;
             INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
             WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1])+"G"
    ENDIF
    
    DIMENSION laMember[1]
    laMember=""
    SELECT cUser_ID+"-"+cUsr_Name ;
          FROM (gcSysHome+"syuUser") ;
          INTO ARRAY laMember ;
          WHERE cUsr_Grup=laData[1]
  
    lsMember = 1

    IF laScrMode[3] .AND. !EMPTY(laMember[1])  
      SHOW GET pbRemMem ENABLE
    ELSE
      SHOW GET pbRemMem DISABLE
    ENDIF    
ENDCASE

IF !laScrMode[1]
  IF !EMPTY(laMember[1])
    SELECT " "+cUser_ID+"-"+cUsr_Name ;
           FROM (gcSysHome+"syuUser") ;
           INTO ARRAY laNewMem ;
           WHERE EMPTY(syuUser.cUsr_grup) .AND. ;
           ASCAN(laMember,cuser_ID+"-"+cUsr_Name) = 0 
  ELSE
    SELECT " "+cUser_ID+"-"+cUsr_Name ;
           FROM (gcSysHome+"syuUser") ;
           INTO ARRAY laNewMem ;
           WHERE EMPTY(syuUser.cUsr_grup)
  ENDIF
ENDIF

SHOW GET lsMember ENABLE

SELECT SYUGROUP

*!**************************************************************************
*!
*!      Procedure: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()=13)
  =gfSeekRec()
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Procedure: lfvNewMemb
*!
*!**************************************************************************
*
FUNCTION lfvNewMemb

*E300683,4 Call *.SPR from screens directory
* DO SMNWMEM.SPR 
DO (gcScrDir + gcWinAppl + '\SMNWMEM.SPR')
*E300683,4 end   

lsMember   = ALEN(laMember,1)

SHOW GET lsMember

IF !EMPTY(laMember[1])  
  SHOW GET pbRemMem ENABLE
ELSE
  SHOW GET pbRemMem DISABLE
ENDIF      

*!**************************************************************************
*!
*!      Procedure: lfvRemMemb
*!
*!**************************************************************************
*
FUNCTION lfvRemMemb

DIMENSION laNewMem[ALEN(laNewMem,1)+1]
=AINS(laNewMem,1)
laNewMem[1] = " "+laMember[lsMember]

=ADEL(laMember,lsMember)

IF ALEN(laMember,1) > 1
  DIMENSION laMember[ALEN(laMember,1)-1]
ELSE
 laMember[1] = " "
ENDIF

IF lsMember > 1
  lsMember = lsMember - 1
ENDIF

SHOW GET lsMember

IF !EMPTY(laMember[1])  
  SHOW GET pbRemMem ENABLE
ELSE
  SHOW GET pbRemMem DISABLE
ENDIF    

*!**************************************************************************
*!
*!      Procedure: lfvNewMem
*!
*!**************************************************************************
*
FUNCTION lfvNewMem

IF LASTKEY() = 13
 _CUROBJ = OBJNUM(LSNEWMEM)
ENDIF
IF LEFT(laNewMem[lsNewMem],1) = ""
  laNewMem[lsNewMem] = " "+SUBSTR(laNewMem[lsNewMem],2)
ELSE
  laNewMem[lsNewMem] = ""+SUBSTR(laNewMem[lsNewMem],2) 
ENDIF
SHOW GET lsNewMem

*!**************************************************************************
*!
*!      Procedure: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

lnHidnCont  = 1
FOR lnCount = 1 TO ALEN(laNewMem,1)
  IF LEFT(laNewMem[lnHidnCont],1) = ""
    IF !EMPTY(laMember[1])
      DIMENSION laMember[ALEN(laMember,1)+1]
    ENDIF
    laMember[ALEN(laMember,1)] = SUBSTR(laNewMem[lnHidnCont],2)
    =ADEL(laNewMem,lnHidnCont)
    IF ALEN(laNewMem,1) > 1
      DIMENSION laNewMem[ALEN(laNewMem,1)-1]
    ELSE
      laNewMem[1] = " "
    ENDIF
  ELSE
    lnHidnCont = lnHidnCont + 1
  ENDIF
ENDFOR

*!**************************************************************************
*!
*!      Procedure: lfvPrivil
*!
*!**************************************************************************
*
FUNCTION lfvPrivil

lcSavAlias=SELECT(0)

SELECT SYUGROUP
lnRecUser = RECNO("SYUGROUP")

IF lcLastData <> laData[1]
  SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
         FROM (gcSysHome+"SYUUSRPR") ;
         INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
         WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1])+"G"
  
  SELECT (lcPrv_Tmp2)
  ZAP
  APPEND FROM (gcWorkDir+lcPrv_Tmp1)
  
  llUpdPriv  = .F.
  *E300683,4 Call *.PRG from screens directory
  *=SMprvl('Group',laData[1],laData[2])
  lcLastData = laData[1]
  DO (gcAppHome + gcWinAppl + '\SMprvl') WITH 'Group',laData[1],laData[2]
  *E300683,4 end   
ELSE
  *E300683,4 Call *.PRG from screens directory
  *=SMprvl('Group',laData[1],laData[2])
  DO (gcAppHome + gcWinAppl + '\SMprvl') WITH 'Group',laData[1],laData[2]
  *E300683,4 end   
ENDIF

SELECT SYUGROUP
IF lnRecUser > 0 .AND. lnRecUser <=RECCOUNT("SYUGROUP")
  GO lnRecUser
ENDIF

SELECT (lcSavAlias)

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

EXTERNAL ARRAY laScrMode

IF !EMPTY(laMember)
  lnOption=gfModalGen('QRM00024B00006','Dialog'," ")
  DO CASE  
    CASE lnOption=1
      DO lfClnGrp
      *** Delete all priv. for this group. ***
      lcSavAlias=SELECT(0)
      SELECT (lcPrv_Tmp1)
      REPLACE ALL cStatus WITH 'D'
      DELETE ALL
      =gfTmp2Mast("SYUUSRPR",lcPrv_Tmp1,;
                  'Deleting user privilege information...')  
          
      SELECT(lcSavAlias)
      DIMENSION laMember[1]
      laMember=" "
    CASE lnOption=2
      RETURN
  ENDCASE  
ENDIF  

SELECT SYUGROUP
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE

laScrMode    = .F.
laScrMode[1] = .T.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode,laData

IF EMPTY(laData[2])
  *** You cannot save this user without name. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00154B00000","DIALOG","group")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

SELECT SYUGROUP

IF laScrMode[4]
  APPEND BLANK
ENDIF
 
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

IF llUpdPriv  
  llUpdPriv  = .F.
  lcSavAlias = SELECT(0)
  SELECT (lcPrv_Tmp1)
  DELETE ALL FOR cStatus = "D"
  SELECT SYUUSRPR
  =gfTmp2Mast("SYUUSRPR",lcPrv_Tmp1,'Saving group privilege information...')  

  SELECT (lcSavAlias)
ENDIF  

IF laScrMode[4]
  DO lfSavGrp
ELSE
  DO lfClnGrp
  DO lfSavGrp
ENDIF

DIMENSION laMember[1]
laMember = " "

SELECT SYUGROUP
 
*!**************************************************************************
*!
*!      Procedure: lfClnGrp
*!
*!**************************************************************************
*
FUNCTION lfClnGrp

lcSavAlias = SELECT(0)
SELECT syuUser
GO TOP
SCAN FOR cUsr_grup=laData[1]
  REPLACE cUsr_grup WITH " "
ENDSCAN  
SELECT(lcSavAlias)

*!**************************************************************************
*!
*!      Procedure: lfSavGrp
*!
*!**************************************************************************
*
FUNCTION lfSavGrp

lcSavAlias=SELECT(0)
IF !EMPTY(laMember)
  SELECT syuUser
  SET ORDER TO 1
  FOR lnCount=1 to ALEN(laMember,1)
    *B804124,1 AMM search for the whole user ID length wich is 10 characters.
    *IF SEEK (SUBSTR(laMember[lncount],1,4))
    IF SEEK (LEFT(laMember[lncount],10))
    *B804124,1 AMM end
      REPLACE cUsr_Grup WITH laData[1]
    ENDIF
  ENDFOR
ENDIF 

SELECT(lcSavAlias)

