*!***************************************************
*!
*!             FUNCTION gfSetError
*!***************************************************
*FUNCTION gfSetError
*IF !USED('sycError')
*SELECT 0
*USE SYCERROR ORDER 1
*ENDIF
* ON ERROR DO TRAPERR WITH Error(), MESSAGE(), ;
    MESSAGE(1), SYS(16), LINENO(), SYS(102), ;
    SYS(100), SYS(101), LASTKEY(), ALIAS(), ;
    SYS(18), SYS(5), SYS(12), SYS(6), SYS(2003), ;
    WONTOP(), SYS(2011), SYS(2018), SET("CURSOR")



*****************************************************************

*   lnError =    The numeric code of the number provided by ERROR()
*   lcmsg =     The error message provided by MESSAGE()
*   lckode =    The contents of the line of code which
*                 triggered the error as provided by MESSAGE(1)
*   lcmodul =   The name of the code module, SYS(16)
*   lnline =    The number of the line of code triggering the
*                 error, LINENO()
*   lcprint =    Current PRINTER setting as per SYS(102)
*   lckonsol =   Current CONSOLE setting as per SYS(100)
*   lcdevice =   Current DEVICE setting as per SYS(101)
*   keypress =  LASTKEY()
*   curr_dbf =  Selected .DBF when error occurred
*   getfield =  Field of GET...READ at time error occurred
*   def_drive = Default drive at time error occurred - SYS(5)
*   run_memry = Amount of DOS memory available for RUN command - SYS(12)
*   prnt_devic= Current SET PRINTER TO setting - SYS(6)
*   cur_direc = Current directory at time error occurred - SYS(2003)
*   top_win =   Top window at time error occurred
*   llocked =   Record or file lock status at time error occurred -
*                 SYS(2011)
*   lcmissing =  Missing file, window, etc.
*   curs_set =  Cursor setting at time error occurred.
*
*  In addition to this information, the routine saves the information
*  from LIST MEMORY and LIST STATUS is saved into the memo field.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


*FUNCTION Trap_Eror
PARAMETERS lnError, lcmsg, lckode, lcmodul, lnline, lcprint, ;
    lckonsol, lcdevice, keypress, curr_dbf, getfield, def_drive, ;
    run_memry, prnt_devic, cur_direc, top_win, llocked, ;
    lcmissing, curs_set,lcLpmHit
*********************************************    
testing=.F.
glMultiUsr=.T.
lcContact="Company : ARIA SYSTEMS INC."
lcPhone="Phone : (202)3783590"
*********************************************    
*wait curr_dbf window
private lnCount
ON ERROR
IF USED('SYCERROR')
  SELECT SYCERROR
  SET ORDER TO 1
ELSE
  SELECT 0
  USE &GCSYSHOME.SYCERROR
  SET ORDER TO 1
ENDIF

IF !SEEK(lnError)
  SEEK 2000
ENDIF  

lnNoLine=IIF(ATCLINE('"END"',MEXECUTE)>0,;
             ATCLINE('"END"',MEXECUTE)-1,MEMLINES(MEXECUTE))
FOR lnCount=1 TO lnNoLine
  lnOption=0
  lcComm=MLINE(mExecute,lnCount)
    &lcComm
  IF lnOption<>0  
    IF ATCLINE('"PUSH BUTTON"',mExecute)>0
    lnCondNo=ATCLINE('"CONDITION '+STR(lnOption,1,1)+'"',mExecute)+1
    FOR lnProc=lnCondNo TO MEMLINES(sycError.mExecute)
      IF USED('sycError') AND (SYCERROR.NERROR_NO=lnError OR;
         SYCERROR.NERROR_NO=2000)
        IF '"CONDITION' $ UPPER(MLINE(sycerror.mExecute,lnProc)) OR;
         '"PUSH BUTTON"' $ UPPER(MLINE(sycerror.mExecute,lnProc))
          EXIT
        ELSE
         lcToProc=MLINE(sycerror.mExecute,lnProc)
         &lcToProc
        ENDIF
      ENDIF 
    ENDFOR
   ENDIF
  ENDIF
ENDFOR


PROCEDURE ShowEror
_WRAP=.T.
_LMARGIN = 2
_RMARGIN = IIF(_DOS,48,55)
_ALIGNMENT = "LEFT"
lcPushB=IIF(ATCLINE('"PUSH BUTTON"',mExecute)>0,;
           MLINE(mExecute,ATCLINE('"PUSH BUTTON"',;
           mExecute)+1),"\!  \<OK  ")
lcPb=lcPushB             

lcMessage = lfGetMemLn(mmumsg)
lnWinH=LEN(lcMessage)/46+4

DEFINE WINDOW gwdError FROM INT((SROW()-lnWinH)/2),INT((SCOL()-50)/2) ;
        TO INT((SROW()-lnWinH)/2)+lnWinH;
		+IIF(_WINDOWS,5,1),INT((SCOL()-50)/2)+50;
				FLOAT ;
        		NOCLOSE ;
        		SHADOW ;
        		NOMINIMIZE ;
        		DOUBLE ;
        		COLOR SCHEME 5;
        		TITLE " ERROR ";
        		FONT "SYSTEM",10;
        		STYLE 'B'        		

MOVE WINDOW gwdError CENTER
ACTIVATE WINDOW gwdError
?lcMessage
  DIMENSION laButtons[1]
  =gfSubStr(lcPushB,@laButtons,";")               

FOR lnCount=1 TO ALEN(laButtons,1)
  laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\<") 
  laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\!") 
  laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\?")     
ENDFOR
lnPbLngth=LEN(laButtons[1])
FOR lnCount=1 TO ALEN(laButtons,1)
  lnPbLngth=IIF(lnPbLngth>LEN(laButtons[lnCount]),lnPbLngth,LEN(laButtons[lnCount]))
ENDFOR
lnPbLngth=(lnPbLngth*ALEN(laButtons,1))+(5*ALEN(laButtons,1))
lnxPush=INT((50-lnPbLngth)/2)+IIF(_DOS,0,IIF(ALEN(laButtons,1)=1,3,4))  &&-IIF(ALEN(laButtons,1)=1 AND _WINDOWS,5,0)
lnOption=lfReadCycl('gwdError',lcpb,lnWinH-IIF(_DOS,1,2.2),lnxPush,3)
IF WEXIST('gwdError')
  RELEASE WINDOW gwdError
ENDIF  

RETURN lnOption


FUNCTION lfGetMemLn
PARAMETER lcMemo
PRIVATE lnCount
lcRet=''
FOR lnCount = 1 TO MEMLINES(lcMemo)
lcret=lcRet+lfGetMac(ALLTRIM(STRTRAN(MLINE(lcMemo,lnCount),CHR(13)+CHR(10),' ',1)))+' '
ENDFOR
RETURN lcRet


PROCEDURE ShowEror1
  PRIVATE lnCount
  lcMemoNam=IIF(glMultiUsr,"mMuMsg","mSuMsg")
  lnWinH=MEMLINES(&lcMemoNam)+3
  lnWinW=LEN(lfgetmac(MLINE(&lcMemoNam,1)))
  FOR lnCount=2 TO MEMLINES(&lcMemoNam)
    lcNewStr=lfGetmac(MLINE(&lcMemoNam,lnCount))
    lnWinW=IIF(LEN(lcNewStr)>lnWinW,;
            LEN(lcNewStr),lnWinW)
  ENDFOR
  lnMsgPos=lnWinW
  lcPushB=IIF(ATCLINE('"PUSH BUTTON"',mExecute)>0,;
               MLINE(mExecute,ATCLINE('"PUSH BUTTON"',;
               mExecute)+1),"\!  \<OK  ")
  lcPb=lcPushB             
  DIMENSION laButtons[1]
  =gfSubStr(lcPushB,@laButtons,";")               

  FOR lnCount=1 TO ALEN(laButtons,1)
    laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\<") 
    laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\!") 
    laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\?")     
  ENDFOR
  lnPbLngth=LEN(laButtons[1])
  FOR lnCount=1 TO ALEN(laButtons,1)
    lnPbLngth=IIF(lnPbLngth>LEN(laButtons[lnCount]),lnPbLngth,LEN(laButtons[lnCount]))
  ENDFOR
  lnPbLngth=(lnPbLngth*ALEN(laButtons,1))+(5*ALEN(laButtons,1))
  lnWinW=IIF(lnPbLngth>lnWinW,lnPbLngth,lnWinW)
  lnWinH=lnWinH+2
  lnWinW=lnWinW+4
  lnxPush=INT((lnWinW-lnPbLngth)/2)+2
  DEFINE WINDOW gwdError FROM INT((SROW()-lnWinH)/2),INT((SCOL()-lnWinW)/2) ;
		TO INT((SROW()-lnWinH)/2)+lnWinH+IIF(_DOS,0,lnWinH/2);
		+IIF(_WINDOWS,1,0),INT((SCOL()-lnWinW)/2)+lnWinW+IIF(_DOS,1,-7) ;
				FLOAT ;
        		NOCLOSE ;
        		SHADOW ;
        		NOMINIMIZE ;
        		DOUBLE ;
        		COLOR SCHEME 5;
        		TITLE " "+ALLTRIM(cTitle)+" ";
        		FONT "SYSTEM",9
        		
MOVE WINDOW gwdError CENTER        		
  ACTIVATE WINDOW gwdError NOSHOW       		
  FOR lnCount=1 TO MEMLINES(&lcMemoNam)        		
    lcNewStr=lfGetMac(MLINE(&lcMemoNam,lnCount))
    @ lnCount,INT((lnWinW-LEN(lcNewStr))/2) SAY lcNewStr ;
    FONT "SYSTEM",9
  ENDFOR
  lnPbLngth=INT(lnPbLngth/ALEN(laButtons)-3)
lnOption=lfReadCycl('gwdError',lcpb,lnCount,lnxPush,2)
IF WEXIST('gwdError')
  RELEASE WINDOW gwdError
ENDIF  
RETURN lnOption

FUNCTION lfGetMac
PARAMETER lcNewStr
    lcMacCon=""
    IF '&' $ lcNewStr
      IF OCCURS(' ',lcNewStr)>0
        FOR I=1 TO OCCURS(' ',lcNewStr)
          IF ATC(' ',lcNewStr,I)>ATC('&',lcNewStr)
            lcMacCon=SUBSTR(lcNewStr,ATC('&',lcNewStr),;
            ATC(' ',lcNewStr,I)-ATC('&',lcNewStr))
            EXIT
          ELSE
            lcMacCon=SUBSTR(lcNewStr,ATC('&',lcNewStr))
          ENDIF
        ENDFOR
      ELSE
         lcMacCon=lcNewStr
      ENDIF
      ENDIF
   IF !EMPTY(lcMacCon)
     lcMacCon=SUBSTR(lcMacCon,2)
     lcNewStr=STRTRAN(lcNewStr,"&"+lcMacCon,&lcMacCon)
   ENDIF

RETURN lcNewStr      		

*********************************************************************
FUNCTION lfReadCycl
PARAMETERS lcWinNam,lcpb,lnRow,lnCol,pbSep
PRIVATE lnCount
DIMENSION laButtons[1]
=gfSubStr(lcpb,@laButtons,';')
PUSH KEY
ON KEY
DIMENSION laPos[ALEN(laButtons,1),4],laHotkeys[ALEN(laButtons,1)]
lchotkeys=""
STORE 0 TO lnEsc,lnEnter,lnpblng
FOR lnCount=1 TO ALEN(laButtons,1)
   laPos[lnCount,4]=LEN(laButtons[lnCount])
   IF OCCURS("\<",laButtons[lnCount])>0
      laHotkeys[lnCount]=UPPER(SUBSTR(laButtons[lnCount],ATC("\<",laButtons[lnCount])+2,1))
      lcHotkeys=lcHotkeys+SUBSTR(laButtons[lnCount],ATC("\<",laButtons[lnCount])+2,1)
       laPos[lnCount,4]=laPos[lnCount,4]-2
   ENDIF     
   IF OCCURS("\!",laButtons[lnCount])>0
     lnEnter=lnCount
     laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\!") 
     laPos[lnCount,4]=laPos[lnCount,4]-2
   ENDIF     
   IF OCCURS("\?",laButtons[lnCount])>0
     lnEsc=lnCount
     laButtons[lnCount]=STRTRAN(laButtons[lnCount],"\?")     
     laPos[lnCount,4]=laPos[lnCount,4]-2     
   ENDIF   
   laButtons[lnCount]=laButtons[lnCount]
   laPos[lnCount,3]=ATC(laHotKeys[lnCount],laButtons[lnCount])
   laPos[lnCount,4]=laPos[lnCount,4]+2
   lnpblng=IIF(lnpblng>laPos[lnCount,4],lnpblng,laPos[lnCount,4])
ENDFOR
lnpblng=IIF(ALEN(laButtons,1)=1,10,lnpblng)
lcHotkeys=LOWER(lcHotkeys)+UPPER(lcHotkeys)
ACTIVATE WINDOW &lcWinNam NOSHOW
  @ lnRow,lnCol GET lnOption;
 	PICTURE "@*HN "+lcPB ;
     DEFAULT 1 ;
     SIZE 1+IIF(_WINDOWS,1,0),lnpblng,pbSep;
     FONT "SYSTEM",10
laPos[1,1]=lnRow
laPos[1,2]=lnCol
FOR lnCount=2 to ALEN(laButtons,1)
  laPos[lnCount,1]=lnRow
  laPos[lnCount,2]=lnCol+(lnpblng+pbSep)*(lnCount-1)
ENDFOR
ACTIVATE WINDOW &lcWinNam
llSelection= .F.
lnSelect=1
DO lfHighBtn WITH lnSelect
DO WHILE !llSelection
inval1 = INKEY(0,'HM')
DO CASE
  CASE inval1=0
     LOOP
  CASE inval1 = 151 && Single mouse click
    DO lfMouse WITH MROW(lcWinNam),MCOL(lcWinNam)
  CASE inval1 = 4
    DO lfModiSel WITH "RIGHTARROW"
  CASE inval1 = 19
    DO lfModiSel WITH "LEFTARROW"
  CASE inval1 = 9
    DO lfModiSel WITH "RIGHTARROW"
  CASE inval1 =15
    DO lfModiSel WITH "LEFTARROW"
  CASE inval1 = 13
    DO lfSelect WITH 1000
  CASE inval1 = 27
    IF lnEsc>0
      DO lfSelect WITH lnEsc
    ENDIF  
  CASE inval1 = 10  
    IF lnEnter>0
      DO lfSelect WITH lnEnter
    ENDIF    
  CASE CHR(inval1)   $ lcHotkeys
    DO lfSelect WITH ASCAN(laHotkeys,UPPER(CHR(inval1)))
ENDCASE
ENDDO
CLEAR GETS
POP KEY
RETURN lnSelect

*!**********************************************************************
*!                            FUNCTION lfSelect
*!
*!**********************************************************************

FUNCTION lfSelect
PARAMETERS lnSetSel
DO lfUnhighBtn WITH lnSelect
IF lnSetSel<>1000
  lnSelect=lnSetSel
ENDIF
DO lfHighBtn WITH lnSelect
llSelection=.T.
RETURN
 
*!**********************************************************************
*!                            FUNCTION lfModiSel
*!
*!**********************************************************************

FUNCTION lfModiSel
PARAMETER lcArrow
DO lfUnhighBtn WITH lnSelect
  IF lcArrow="LEFTARROW"
    lnSelect=IIF(lnSelect-1<1,ALEN(laButtons,1),lnSelect-1)
  ELSE
    lnSelect=IIF(lnSelect+1>ALEN(laButtons,1),1,lnSelect+1)
  ENDIF
DO lfHighBtn WITH lnSelect

*!**********************************************************************
*!                            FUNCTION lfMouse
*!
*!**********************************************************************    
FUNCTION lfMouse
PARAMETERS lnMRow,lnMCol
PRIVATE lnCount
llFound = .F.
lnCount=0

IF lnMRow>=lnRow AND lnMRow<=lnRow+IIF(_DOS,0,2.000) ;
  AND lnMCol<=laPos[ALEN(laButtons),2]+lnpblng AND lnMCol>=lnCol
  DO WHILE !llFound AND lnCount<ALEN(laButtons,1)
   lnCount=lnCount+1
     llFound=(lnMRow >= lnRow AND lnMCol>=laPos[lnCount,2];
                AND lnMCol<=laPos[lnCount,2]+;
                lnpblng);
                AND lnMRow<=laPos[lnCount,1]+IIF(_DOS,0,2.000)
  ENDDO
ENDIF
IF llFound
  DO lfSelect WITH lnCount
ELSE
  IF lnMrow<0
    ??CHR(7)  
  ENDIF  
ENDIF
RETURN

FUNCTION lfHighBtn
PARAMETERS lnHighPb
IF _DOS  
  @lnRow,lnCol+(lnpblng+pbsep)*(lnHighPb-1)  FILL TO lnRow,;
    lnCol+(lnpblng+pbsep)*(lnHighPb-1)+lnpblng-1 COLOR (SCHEME(5,6))
ELSE
  @lnRow+0.400,lnCol+(lnpblng+pbsep)*(lnHighPb-1)+0.600 TO lnRow+1.600,;
    lnCol+(lnpblng+pbsep)*(lnHighPb-1)+lnpblng-0.600 ;    
	PEN 0, 1 ;
	STYLE "0" ;
	COLOR RGB(0,0,0,,,)    
ENDIF

FUNCTION lfUnhighBtn
PARAMETERS lnHighPb
IF _DOS
  @lnRow,lnCol+(lnpblng+pbsep)*(lnHighPb-1)  FILL TO lnRow,;
    lnCol+(lnpblng+pbsep)*(lnHighPb-1)+lnpblng-1 COLOR SCHEME(5)
  IF laPos[lnHighpb,4]=lnpblng  
    @lnRow,laPos[lnHighPb,2]+laPos[lnHighPb,3]-2  FILL TO laPos[lnHighPb,1],;
      laPos[lnHighPb,2]+laPos[lnHighPb,3]-2 COLOR (SCHEME(5,7))
  ELSE
    @lnRow,laPos[lnHighPb,2]+laPos[lnHighPb,3]-2+INT((lnpblng-laPos[lnHighPb,4])/2);
      FILL TO laPos[lnHighPb,1],;
      laPos[lnHighPb,2]+laPos[lnHighPb,3]-2;
      +INT((lnpblng-laPos[lnHighPb,4])/2);
      COLOR (SCHEME(5,7))       
  ENDIF  
ELSE
  @lnRow+0.400,lnCol+(lnpblng+pbsep)*(lnHighPb-1)+0.600 TO lnRow+1.600,;
    lnCol+(lnpblng+pbsep)*(lnHighPb-1)+lnpblng-0.600 ;    
	PEN 0, 8 ;
	STYLE "0" ;
	COLOR RGB(192,192,192,192,192,192)
ENDIF    

*!**********************************************************
*!              PROCEDURE gpGo_Mastr
*!
*!**********************************************************
PROCEDURE gpGo_Mastr
DO gfsetenvmt
DO gfsaveErr
RELEASE lstext, lmtext
CLEAR TYPEAHEAD
*IF !glFromSav
* DO CASE
*    CASE RIGHT(lcmodul,4) $ ".SPX,.FXP"   
*      IF lfFromlpM()
*        glQuitting = .T.
*      ENDIF  
*      CLEAR READ 
*    CASE RIGHT(lcmodul,4) $ ".APP"   
*      glErrOccur = .T.
*      glFlNotOpn = .T.
*  ENDCASE       
*ELSE
*  glErrOccur = .T.
*  glFlNotOpn = .T.
*ENDIF  
glQuitting = .T.
glErrOccur = .T.
glFlNotOpn = .T.
CLEAR READ ALL
DO ERORSET
RETURN



FUNCTION lfFromlpM
  IF ! USED("SYUEROR")
      SELECT 0
      USE &gcSysHome.SYUEROR
  ELSE
      SELECT SYUEROR
  ENDIF 
  lnCount=1
  lcCallProg=""
  DO WHILE !EMPTY(SYS(16,lnCount))
    lcCallProg = lcCallProg + SYS(16,lnCount) + CHR(13)+CHR(10)
    lnCount=lnCount+1
  ENDDO
lnNoEvent=OCCURS('ON ',lcCallPrg)
lcCheck=SUBSTR(lcCallPrg,ATC('ON ',lcCallPrg,lnNoEvent-1))
IF ATC('LPMHIT',lcCallPrg)>ATC('ON ',lcCallPrg,lnNoEvent-1);
   AND ATC('LPMHIT',lcCallPrg)<ATC('ON ',lcCallPrg,lnNoEvent)
ENDIF

FUNCTION gfSetEnvmt
  PRIVATE disk_space
  STORE DISKSPACE() TO disk_space
  SET CONSOLE OFF
  PUBLIC lstext, lmtext
  STORE gcWorkDir+ustring() + ".TXT" TO lstext
  LIST STATUS TO (lstext)
  STORE gcWorkDir+ustring() + ".TXT" TO lmtext
  LIST MEMORY TO (lmtext)
  SET CONSOLE ON
  ON KEY && Null out hot keys
  CLEAR READ ALL
  CLEAR FIELDS
  CLEAR MACROS
  DEACTIVATE MENUS
  CLEAR MENUS
  DEACTIVATE POPUPS
  CLEAR POPUPS
  CLEAR PROMPT
  UNLOCK ALL
  SET DEVICE TO SCREEN
RETURN
* End FUNCTION  gfSETENVMT


*!*****************************************************************************
*!
*!      Procedure: gfSaveErr
*!
*!      Called by: Trap_EROR.PRG                      
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!
*!          Calls: LOGVIEW            (procedure in FP25EROR.PRG)
*!
*!           Uses: SYSDATA.DBF        
*!               : FP25EROR.DBF       
*!
*!*****************************************************************************

FUNCTION gfSaveErr
  PRIVATE uname
  uName=""
  IF glmultiusr
      STORE gcUser_ID TO uname
  ENDIF

* ==========================
* Store info to error file
* ==========================
  IF ! USED("SYUEROR")
      SELECT 0
      USE &gcSysHome.SYUEROR
  ELSE
      SELECT SYUEROR
  ENDIF 
  lnCount=1
  lcCallProg=""
  DO WHILE !EMPTY(SYS(16,lnCount))
    lcCallProg = lcCallProg + SYS(16,lnCount) + CHR(13)+CHR(10)
    lnCount=lnCount+1
  ENDDO
  SET CONSOLE OFF
  APPEND BLANK
  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), line_no WITH lnline;
      ProgCalls WITH lcCallProg

  IF TYPE("UNAME") <> "U" .AND. ! EMPTY(uname)
      REPLACE lan_user WITH uname
  ELSE
      REPLACE lan_user WITH "NOT AVAILABL"
  ENDIF (TYPE("UNAME") <> "U" .AND. ! EMPTY(uname))
* ==================================================
*$ Put info from the files created with LIST MEMORY
*$ and LIST STATUS into the memo field and then
*$ erase the text files
* ==================================================
  APPEND MEMO Status FROM (lstext)
  ERASE (lstext)
  APPEND MEMO Memory FROM (lmtext)
  ERASE (lmtext)
  SET CONSOLE ON

  IF TYPE("TESTING") <> "U"
      IF testing
          DO logview
      ENDIF (testing)
  ENDIF (TYPE("TESTING") <> "U")

*  CLOSE DATA
RETURN
* End procedure save_it



*!*****************************************************************************
*!
*!      Procedure: gfSaveErr
*!
*!      Called by: Trap_EROR.PRG                      
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!
*!          Calls: LOGVIEW            (procedure in FP25EROR.PRG)
*!
*!           Uses: SYSDATA.DBF        
*!               : FP25EROR.DBF       
*!
*!*****************************************************************************

FUNCTION gfSaveErrNo
  PRIVATE uname
  uName=""
  IF glmultiusr
      STORE gcUser_ID TO uname
  ENDIF

* ==========================
* Store info to error file
* ==========================
  IF ! USED("SYUEROR")
      SELECT 0
      USE &gcSysHome.SYUEROR
  ELSE
      SELECT SYUEROR
  ENDIF 
  lnCount=1
  lcCallProg=""
  DO WHILE !EMPTY(SYS(16,lnCount))
    lcCallProg = lcCallProg + SYS(16,lnCount) + CHR(13)+CHR(10)
    lnCount=lnCount+1
  ENDDO
  SET CONSOLE OFF
  APPEND BLANK
  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), line_no WITH lnline;
      ProgCalls WITH lcCallProg

  IF TYPE("UNAME") <> "U" .AND. ! EMPTY(uname)
      REPLACE lan_user WITH uname
  ELSE
      REPLACE lan_user WITH "NOT AVAILABL"
  ENDIF (TYPE("UNAME") <> "U" .AND. ! EMPTY(uname))

  SET CONSOLE ON

  IF TYPE("TESTING") <> "U"
      IF testing
          DO logview
      ENDIF (testing)
  ENDIF (TYPE("TESTING") <> "U")

*  CLOSE DATA
RETURN
* End procedure save_it


PROCEDURE logview
*******************************************************************************
*& View error log if user so requests
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  PRIVATE do_it
  llOpen=.F.
  STORE 1 TO do_it
  STORE SET("CURSOR") TO cursset
  IF USED("SYCERROR")
    SELECT SYCERROR
    SET ORDER TO 1
  ELSE
    SELECT 0
    USE SYCERROR 
    SET ORDER TO 1
    llOpen=.T.
  ENDIF  
  lnLastRec=RECNO()
  SEEK 2002
  do_it=SHOWEROR()
  go lnLastRec
  SELECT SYUEROR
  GO BOTTOM
  IF do_it=1
*    DO ShowErr.prg
  ENDIF (do_it)
  IF llOpen
    SELECT SYCERROR
    USE
    SELECT SYUEROR
  ENDIF
RETURN
* End procedure LOGVIEW

PROCEDURE qquit

  CLOSE DATA
  SET CURSOR ON
  SET LIBRARY TO
  SET COLOR TO W/N
  CLEAR
  SET CLEAR OFF && Suppresses "Normal Shutdown" message
  QUIT
* End procedure QQUIT


PROCEDURE kancel

  ON KEY && Null out hot keys
  CLEAR GETS
  CLEAR READ ALL
  CLEAR FIELDS
  CLEAR MACROS
  CLEAR WINDOWS
  DEACTIVATE MENUS
  CLEAR MENUS
  DEACTIVATE POPUPS
  CLEAR POPUPS
  CLEAR PROMPT
  SET REFRESH TO 0, 200
  SET DEVICE TO SCREEN
  CLOSE DATA
  *DO ERORSET
  SET LIBRARY TO
*CLOSE PROCEDURE
  CLEAR MEMORY
  PUBLIC testing
  STORE .T. TO testing
  CANCEL
* End procedure KANCEL


PROCEDURE ustring
  PARAMETERS sendback
  STORE SUBSTR(SYS(3), 4, 4) + ;
  SUBSTR(SYS(2015),7,4) TO sendback
RETURN sendback



*!*****************************************************************************
*!
*!      Procedure: gfGo_Again
*!
*!
*!          Calls: ERORSET            (procedure in FP25EROR.PRG)
*!
*!    Other Files: &PRNT_DEVIC
*!
*!*****************************************************************************
FUNCTION gfGo_Again
*******************************************************************************

  IF USED("SYUEROR")
    SELECT SYUeror
    USE
  ENDIF (USED("SYUEROR"))

  IF ! EMPTY(curr_dbf)
    SELECT (curr_dbf)
    GO RECNO()
  ENDIF ! empty(curr_dbf)

  IF ! EMPTY(top_win)
    ACTIVATE WINDOW &top_win
  ENDIF (! EMPTY(top_win))

  SET CONSOLE &lckonsol
  SET DEVICE TO &lcdevice
  SET PRINTER TO &prnt_devic
  SET PRINT &lcprint
  RELEASE lstext, lmtext
  DO erorset
  CLEAR TYPEAHEAD
  SET CURSOR &curs_set
RETURN
* End procedure gfGo_Again



*!*****************************************************************************
*!
*!      Procedure: ERORSET
*!
*!      Called by: FP25EROR.PRG                      
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!               : gfGo_Again           (procedure in FP25EROR.PRG)
*!
*!          Calls: Trap_Eror.PRG
*!
*!*****************************************************************************
PROCEDURE erorset

ON ERROR DO TRAPERR WITH ERROR(), MESSAGE(), ;
    MESSAGE(1), SYS(16), LINENO(), SYS(102), ;
    SYS(100), SYS(101), LASTKEY(), ALIAS(), ;
    SYS(18), SYS(5), SYS(12), SYS(6), SYS(2003), ;
    WONTOP(), SYS(2011), SYS(2018), SET("CURSOR")
RELEASE lstext, lmtext
RETURN
* End procedure ERORSET


PROCEDURE to_prn
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  PRIVATE prnstatus
  STORE 1 TO prnstatus
  SEEK 2001
  DO WHILE SYS(13) # "READY" AND prnstatus=1
   prnstatus=SHOWEROR()
  ENDDO
 
  IF prnstatus=1 AND SYS(13) = "READY"
      SET CONSOLE OFF
      LIST STATUS TO PRINT
      LIST MEMORY TO PRINT
      SET CONSOLE ON
  ELSE
     SET CONSOLE ON
     HIDE WINDOW ALL
     DISP STATUS 
     DISP MEMORY
     SHOW WINDOW ALL
  ENDIF prnstatus (prnstatus OR SYS(13) = "READY")
SET CURSOR ON
CLEAR
SET CLEAR OFF
QUIT  
RETURN
* End procedure TO_PRN


FUNCTION lfTrap91
IF "RELEASE" $ lcKode
  DO gfSaveErrNo
  SET CONSOLE &lcKonsol
  SET DEVICE TO &lcDevice
  DO ERORSET
ELSE
   DO gpGo_Mastr
ENDIF  

FUNCTION gfIndexErr
DO gfSetEnvmt
DO gfSaveErr
IF !glFromSav
  DO CASE
    CASE RIGHT(lcmodul,4) $ ".SPX,.FXP"   
      glQuitting = .T.
      CLEAR READ 
    CASE RIGHT(lcmodul,4) $ ".APP"   
      DO QQUIT
  ENDCASE       
ELSE
  glErrOccur = .T.
  glFlNotOpn = .T.
ENDIF  
DO ERORSET
