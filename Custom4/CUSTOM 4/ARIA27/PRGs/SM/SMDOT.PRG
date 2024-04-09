
*B800760,1 ASH 09/23/96 (Begin)Fix the bug of looking for Dot when you run DotDos.
*SET PROCEDURE TO DOT
*B800760,1 ASH 09/23/96 (End).
*E301314,1 Walid Abou El-Magd (WAM) 09/13/99 Run this program from the aria system

*E301314,1 To run an external program you need the PARAMETER statment [Start]
PARAMETER T
llFromAria=IIF("AAS_WIN" $ SYS(16,1),.T.,.F.)
llCancel=.F.
lcFullPath=""
*E301314,1 To run an external program you need the PARAMETER statment [End..]

DO MAINDOT
************************************************************
PROCEDURE MAINDOT

ON ERROR DO SEE_ERR WITH MESSAGE()

*E301314,1 DEFINE a window To run the program inside it [Start]
IF llFromAria
  CLOSE DATA
  DEFINE WINDOW Dotwin ;
    AT 0.000, 0.000 ;
    SIZE 33.333,79.625 ;
    TITLE "Dotwin Utility" ;
    FONT "FoxFont", 9 ;
    NOFLOAT ;
    NOCLOSE ;
    NOMINIMIZE ;
    COLOR RGB(,,,192,192,192) ;
    IN WINDOW FNDATION 
  SET SYSMENU OFF
ENDIF
*E301314,1 DEFINE a window To run the program inside it [End..]

ON KEY LABEL F1 HELP
SET SAFETY OFF
SET BELL OFF
SET ECHO OFF
SET TALK OFF
SET SCOREBOARD OFF
SET STATUS OFF
SET COLOR TO W+/B,W+/B
CLEAR

*E301314,1 increase the number of recorded commands to 200 [Start]
*DIMENSION COMMHIST(25)
DIMENSION COMMHIST(200)
*E301314,1 increase the number of recorded commands to 200 [End..]

HISTINDX=1

*E301314,1 What about using this syntax [Start]
*DO WHILE HISTINDX<=25
*  COMMHIST(HISTINDX)=SPACE(100)
*  HISTINDX=HISTINDX+1
*ENDDO
COMMHIST=SPACE(100)
*E301314,1 What about using this syntax [End..]

HISTINDX=1
RETCODE=0
LASTINDX=1
CORR_FLAG=.F.
*E301314,1 [Start]
IF llFromAria
  lcFullPath=STRTRAN(FULLPATH(''),'\PRGS\SM\')
  
  *-- waDataDir : global variable in the calling program and will be released when return to aria
  IF LEN(waDataDir) > 1
    *lcFullPath=lcFullPath+'\'+'DBFS\'+lcCompany+'\' 
    lcFullPath=waDataDir
  ENDIF
  SET DEFA TO &lcFullPath
ENDIF

*E301314,1 [End..]

DO WHILE .T.
  *E301314,1 [Start]
  *@ 0,0 CLEAR TO 2,79
  @ 0,0 CLEAR TO 3,79
  *E301314,1 [End..]
    
  @ 1,71 SAY 'F1 = HELP'
  IF LEN(DBF()) # 0
    *E301314,1 [Start]
    
    *@ 0,0 SAY 'Current Database File : '+DBF()
    @ 0,0 SAY 'Current Database File : '+FULLPATH('')+ALIAS()
    *E301314,1 [End..]
  ENDIF
  *E301314,1 [Start]
  @ 3,0 SAY  '        Full Path is  : '+FULLPATH('')        
  *IF LEN(NDX(1)) # 0
  IF LEN(CDX(1)) # 0
  *E301314,1 [End..]
    *E301314,1 [Start]
    *@ 1,0 SAY '        Active Index  : '+ndx(1)
    @ 1,0 SAY  '        Active Index  : '+IIF(!EMPTY(ORDER()),CDX(1),'None')
    
    *@ 2,0 say '           Index key  : '+SYS(14,1)
    
     @ 2,0 say '           Index key  : '+ORDER()
    
    
    *E301314,1 [End..]
  ENDIF
  MCMD=COMMHIST(HISTINDX)
  
  @ 24,0 SAY '.' GET MCMD PICTURE '@S80'
  READ
  
  *E301314,1 [Start]
  MCMD=UPPER(ALLTRIM(MCMD))
  *E301314,1 [End..]      
  
  RETCODE=IIF(READKEY() >= 256,READKEY()-256,READKEY())
  IF RETCODE # 4 .AND. RETCODE # 5
*    SCROLL 3,0,24,79,1
  ENDIF
  DO CASE
    *E301314,1 [Start]
    *CASE UPPER(MCMD) = "ZAP"
    CASE MCMD = "ZAP"
    *E301314,1 [End..]
      SET SAFETY ON
      ZAP
      SET SAFETY OFF
    ENDCASE
    IF RETCODE # 4 .AND. RETCODE # 5
      *E301314,1 [Start]
      *XCMD="'EXIT','RETURN'"
      XCMD="'EXIT','RETURN','RETUR','RETU','QUIT'"
      *IF INLIST(ALLTRIM(MCMD),&XCMD)
      IF INLIST(MCMD,&XCMD)
      *E301314,1 [End..]
        
        *E301314,1 [Start]
        IF MCMD ='QUIT'
          MCMD='RETURN'
        ENDIF
        
        IF llFromAria
          RELEASE WINDOW Dotwin
          RELEASE WINDOW FNDATION
          SET SYSMENU ON
        ENDIF
        *E301314,1 [End..]
        
        ON ERROR
        ON KEY
        CLOSE DATA
        CLEA
      ENDIF   

      *E301314,1 Don't allow any RELEASE COMMAND and transform CLEAR X to CLEAR [Start]
      IF LEFT(MCMD,4)='RELE'
        WAIT WIND 'Not allowed command .' NOWAIT
        MCMD=SPACE(100)
      ELSE
        IF LEFT(MCMD,4)='CLEA' AND LEN(MCMD) > 6
          WAIT WIND 'You can use CLEAR command only.'
          MCMD='CLEAR'
        ENDIF
      ENDIF
      *E301314,1 Don't allow any RELEASE COMMAND and transform CLEAR X to CLEAR [End..]      

      
      &MCMD
      
      *E301314,1 [Start]
      IF llFromAria
        IF llCancel
          EXIT
        ENDIF
      ENDIF
      *E301314,1 [End..]
      
      IF CORR_FLAG
        DO ARR_CHK
        LOOP
      ENDIF  
    ENDIF RETCODE
    
    DO CASE
      CASE RETCODE=4
        IF HISTINDX - 1 >= 1
           HISTINDX=HISTINDX-1
         ENDIF

      CASE RETCODE=5
        *E301314,1 increase the number of recorded commands to 200 [Start]
        *IF HISTINDX+1<25
        IF HISTINDX+1 < 200
        *E301314,1 increase the number of recorded commands to 200 [End..]
          HISTINDX=HISTINDX+1
        ENDIF
      OTHERWISE
        DO ARR_CHK
    ENDCASE
ENDDO  WHILE .T.

*E301314,1 [Start]
IF llFromAria
  RELEASE WINDOW Dotwin
  RELEASE WINDOW FNDATION
  SET SYSMENU ON
ENDIF
*E301314,1 [End..]

ON ERROR
ON KEY
CLOSE DATA
CLEA
RETURN
************************************************************
PROCEDURE SEE_ERR
parameters merror

*E301314,1 Change the error handler to message box in case of aria call [Start]
*SAVE SCREEN
*DO FSHADOBX WITH 11,10,60,1,'W+/R','D','N'
*DO CENTER WITH 11,'<An error has occured in the program!!!>'
*ACTION=2
*@  12,15 say '    MESSAGE: '+MERROR
*@ 13,30 prompt 'CANCEL'
*@ 13,COL()+10 PROMPT 'IGNORE'
*MENU TO ACTION
*SET COLOR TO W+/B,W+/B
*RESTORE SCREEN

IF "AAS_WIN" $ SYS(16,1) 

  *-- An error XXX has occured in the program
  ACTION=gfModalGen('QRM00355B00012','ALERT',merror)  
  *-- This message return 1 to proceed so
  ACTION=IIF(ACTION = 1 ,2,1)

ELSE
  SAVE SCREEN
  DO FSHADOBX WITH 11,10,60,1,'W+/R','D','N'
  DO CENTER WITH 11,'<An error has occured in the program!!!>'
  ACTION=2
  @  12,15 say '    MESSAGE: '+MERROR
  @ 13,30 prompt 'CANCEL'
  @ 13,COL()+10 PROMPT 'IGNORE'
  MENU TO ACTION
  SET COLOR TO W+/B,W+/B
  RESTORE SCREEN
ENDIF
*E301314,1 Change the error handler to message box in case of aria call [End..]

DO CASE
  CASE ACTION=1
    *E301314,1 [Start]
    IF llFromAria
      llCancel=.T.
    ELSE
      CLEA
      CANCEL
    ENDIF
    *E301314,1 [End..]
  CASE ACTION=2
    CORR_FLAG=.T.
ENDCASE
RETURN
************************************************************
PROCEDURE FSHaDOBX
PARAMETERS R,C,W,H,COL,O,S
PRIVATE R,C,H,W,COL,O,S

W=W+3
H=H+1

IF O="N"
  W=W-2
  H=H-2
ENDIF
SET COLOR TO &S/&S
@ R+1,C+2 CLEAR TO R+H+1,C+W+1
SET COLOR TO &COL
@ R,C CLEAR TO R+H,C+W+3
IF O="S" .OR. O="D"
  IF O="S"
   @ R,C TO R+H,C+W+3
  ELSE
   @ R,C TO R+H,C+W+3 DOUBLE
  ENDIF
ENDIF

IF .NOT. ISCOLOR()
  @ R+H+1,C+1,R+H+1,C+W+1 BOX CHR(176)
  @ R+1,C+W+1,R+H+1,C+W+1 BOX CHR(176)
ENDIF
RETURN
************************************************************
PROCEDURE CENTER
PARAMETERS ROW,CSTRING
MEANSTRNG=LEN(TRIM(CSTRING))/2
@ ROW,40 - MEANSTRNG SAY TRIM(CSTRING)
RETURN
************************************************************
PROCEDURE HELP
SAVE SCREEN
@ 0,0 SAY '(C) Copyright Aria Systems Inc. 1993'
do fshadobox with 2,2,70,19,'w+/bg','D','N'
@ 3,5 SAY 'Thanks for using this program ! Even though this is a short prog.'
do center with 22,'< Press any key to continue ...>'
wait ""
set color to w+/b,w+/b
restore screen
return

************************************************************
PROCEDURE ARR_CHK
*E301314,1 [Start]
*IF MCMD # SPACE(100)
IF !EMPTY(MCMD)
*E301314,1 [End..]
  *E301314,1 increase the number of recorded commands to 200 [Start]
  *IF HISTINDX + 1 < 25
  IF HISTINDX + 1 < 200
  *E301314,1 increase the number of recorded commands to 200 [End..]
    HISTINDX=LASTINDX
    COMMHIST(HISTINDX)=MCMD
    STORE HISTINDX +1 TO HISTINDX,LASTINDX
  ELSE
    HISTINDX=1
    
    *E301314,1 What about using this syntax [Start]
    *DO WHILE HISTINDX<=25
    *  COMMHIST(HISTINDX)=SPACE(100)
    *  HISTINDX=HISTINDX+1
    *ENDDO
    COMMHIST=SPACE(100)
    *E301314,1 What about using this syntax [End..]
    
    HISTINDX=1
    LASTINDX=1               && ARH 06/22/93
    COMMHIST(HISTINDX)=MCMD
  ENDIF
ELSE
  MCMD=SPACe(100)
ENDIF
RETURN