*:************************************************************************
*: Program file  : GFNEWPAS.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Yasser Saad Ibrahime
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it

FUNCTION gfNewPas

lcGetFile = ALIAS()
*E301077,78 Hesham (Start)
llUsrFile = gfSysOpen(gcSysHome+"syuuser","CUSER_ID")
*E301077,78 Hesham (End)
*IF !USED('syuuser')
*  SELECT 0
*  USE (gcSysHome+"syuuser") ORDER TAG CUSER_ID
*ELSE
*  SELECT SYUUSER
*  SET ORDER TO TAG CUSER_ID   
*ENDIF  
*E301077,78 Hesham (End)
=SEEK(ALLTRIM(gcUser_ID),'SYUUSER')

DECLARE laUsr_Pass[8],laUsr_Pas2[8]

laUsr_Pass    = SPACE(1)
lcUsr_Pass    = SPACE(8)

laUsr_Pas2   = SPACE(1)
lcUsr_Pas2   = SPACE(8)


lcBellStat    = SET("BELL")
llRightPass   = .F.

SET BELL OFF
SET CONFIRM OFF

*B603617,1  AME[Start]
llInstOpen = gfSysOpen(gcSysHome+"sycinst")
*B603617,1  AME[End]

DO syOldPas.SPR

*B603617,1  AME[Start]
IF llInstOpen
  USE IN SYCINST
ENDIF
*B603617,1  AME[End]

IF llRightPass 
  IF _DOS 
    @ 0,0 TO 4,30 CLEAR 
  ENDIF
  IF _WINDOWS
    @ 1,1 TO 8,31 CLEAR 
  ENDIF
    
  CLEAR GETS
  laUsr_Pass    = SPACE(1)
  lcUsr_Pass    = SPACE(8)

  *B603617,1  AME[Start]
  llInstOpen = gfSysOpen(gcSysHome+"sycinst")
  *B603617,1  AME[End]
  
  DO syNewPas.SPR

  *B603617,1  AME[Start]
  IF llInstOpen
    USE IN SYCINST
  ENDIF
  *B603617,1  AME[End]
  
ELSE
  RELEASE WINDOW SYCHNGPASS
ENDIF


SET CONFIRM ON
SET BELL &lcBellStat

*E301077,78 Hesham (Start)
USE IN IIF(llUsrFile,"syuuser",0)
*E301077,78 Hesham (End)

IF !EMPTY(lcGetFile)
  SELECT (lcGetFile)
ENDIF


*!*******************************************************************
*!
*!             FUNCTION: lfvUsrPass
*!
*!*******************************************************************
*
FUNCTION lfvUsrPass

lnChrPos = VAL(SUBSTR(SYS(18),AT("(",SYS(18))+1))

IF  EVAL(SYS(18)) <> "*" .AND. ASC(EVAL(SYS(18))) > 32
  lcUsr_Pass = STUFF(lcUsr_Pass,lnChrPos,1,EVAL(SYS(18)))
  laUsr_Pass[lnChrPos] = "*"
  SHOW GET laUsr_Pass[lnChrPos] 
ENDIF  

DO CASE
  CASE INLIST(LASTKEY(),13,9,24,3) 
    _CUROBJ = OBJNUM(PBOK)  
  CASE INLIST(LASTKEY(),15,5,18,5) 
    _CUROBJ = OBJNUM(PBCANCEL)
  CASE LASTKEY() = 27
    llRightPass   = .F.
    CLEAR READ
ENDCASE


*!*******************************************************************
*!
*!             FUNCTION: lfvUsrPas1
*!
*!*******************************************************************
*
FUNCTION lfvUsrPas1

lnChrPos = VAL(SUBSTR(SYS(18),AT("(",SYS(18))+1))

IF  EVAL(SYS(18)) <> "*" .AND. ASC(EVAL(SYS(18))) > 32
  lcUsr_Pass = STUFF(lcUsr_Pass,lnChrPos,1,EVAL(SYS(18)))
  laUsr_Pass[lnChrPos] = "*"
  SHOW GET laUsr_Pass[lnChrPos] 
ENDIF  

DO CASE
  CASE INLIST(LASTKEY(),13,9,24,3) 
    _CUROBJ = OBJNUM(laUsr_Pas2[1])  
  CASE INLIST(LASTKEY(),15,5,18,5) 
    _CUROBJ = OBJNUM(PBCANCEL)
  CASE LASTKEY() = 27
    CLEAR READ
ENDCASE

*!*******************************************************************
*!
*!             FUNCTION: lfvUsrPas2
*!
*!*******************************************************************
*
FUNCTION lfvUsrPas2

lnChrPos = VAL(SUBSTR(SYS(18),AT("(",SYS(18))+1))

IF  EVAL(SYS(18)) <> "*" .AND. ASC(EVAL(SYS(18))) > 32
  lcUsr_Pas2 = STUFF(lcUsr_Pass,lnChrPos,1,EVAL(SYS(18)))
  laUsr_Pas2[lnChrPos] = "*"
  SHOW GET laUsr_Pass[lnChrPos] 
ENDIF  

DO CASE
  CASE INLIST(LASTKEY(),13,9,24,3) 
    _CUROBJ = OBJNUM(PBOK)  
  CASE INLIST(LASTKEY(),15,5,18,5) 
    _CUROBJ = OBJNUM(laUsr_Pass[1])
  CASE LASTKEY() = 27
    llRightPass   = .F.
    CLEAR READ
ENDCASE

*!*******************************************************************
*!
*!             FUNCTION: lfvOk
*!
*!*******************************************************************
*
FUNCTION lfvOk

IF ALLTRIM(SYS(2007,ALLTRIM(lcUsr_Pass))) == ALLTRIM(syuuser.cUsr_Pass)  
  llRightPass   = .T.
  CLEAR READ
ELSE
  WAIT "Invalid password, try again...!" WINDOW NOWAIT
  lcUsr_Pass = SPACE(8)
  laUsr_Pass = " "
  SHOW GETS
  _CUROBJ = OBJNUM(laUsr_Pass[1])
  llRightPass   = .F.
ENDIF

*!*******************************************************************
*!
*!             FUNCTION: lfvAccept
*!
*!*******************************************************************
*
FUNCTION lfvAccept

IF ALLTRIM(lcUsr_Pass) == ALLTRIM(lcUsr_Pas2) .AND. ;
   !EMPTY(lcUsr_Pass) .AND. !EMPTY(lcUsr_Pas2)
  REPLACE  syuuser.cUsr_Pass WITH SYS(2007,ALLTRIM(lcUsr_Pass))
  CLEAR READ
ELSE
  WAIT "The two password entries do not match. please try again." WINDOW NOWAIT
  lcUsr_Pass  = SPACE(8)
  laUsr_Pass  = " "
  lcUsr_Pas2 = SPACE(8)
  laUsr_Pas2 = " "
  SHOW GETS
  _CUROBJ = OBJNUM(laUsr_Pass[1])
ENDIF


