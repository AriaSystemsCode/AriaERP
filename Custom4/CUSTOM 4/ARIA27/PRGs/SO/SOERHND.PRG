*:************************************************************************
*: Procedure file: SOERHND.PRG (Copied from gfEHan.prg with some modifications.)
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Adel Mohammed El Gazzar (ADEL)
*:  Date         : 02/18/2001
*: Refer to      : (B604182)
*:***************************************************************************

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

PARAMETERS lnError, lcmsg, lckode, lcmodul, lnline, lcprint, ;
    lckonsol, lcdevice, keypress, curr_dbf, getfield, def_drive, ;
    run_memry, prnt_devic, cur_direc, top_win, llocked, ;
    lcmissing, curs_set,lcLpmHit
PRIVATE lcOrderTag
testing=.F.
glMultiUsr=.T.
lcContact="your system administrator."&&+" Phone: (202) 3357791"
lcPhone=""
CLEAR TYPEAHEAD
private lnCounter
ON ERROR
llUsrErr = !USED('syueror')
llError = .F.
ON ERROR llError = .T.
llUsrErr = IIF(llUsrErr,gfSysOpen(gcSysHome+'SYUEROR'),.T.)
llErrFile = gfSysOpen(gcSysHome+'SYCERROR','NERROR_NO')
ON ERROR
llErrFile = IIF(llError,.F.,llErrFile OR llUsrErr)
DIMENSION laErrTrac[1,1]
STORE '' TO lcErrWoutPut,lnCounter,laErrTrac,lcErrProgm,lcErrScr,lcErrApp,;
            lcErrMainPrg,lcErrCallFun,lcErrWorkAr,lcErrCallPrg
*--  lcErrWoutPut       the window output name at the error point   
*--  laErrTrac          array have the program path till the error occurs
*--  lcErrProgm         the program name '.fxp' where the error occurs
*--  lcErrScr           the screen name '.spx' where the error occurs
*--  lcErrCallFun       the function that call the place where the error occurs
*--  lcErrApp           the module name '.app' where the error occurs
*--  lcErrMainPrg       the main program name "function in aria3"  or Aria3 itself if empty
*--  lcErrWorkAr        work aria when the error occurs
*--get the current output window at the error point
*--if it is the control panel then get the previous window
*--to the controll panel because we can not depend on the
*--control panel to Know if we will have to terminate the
*--read or not

lcErrWorkAr = SELECT()
lcErrWoutPut=WOUTPUT()

IF lcErrWoutPut = 'GWDTHERMO'
  lcErrWoutPut = ''
  RELEASE WINDOW GWDTHERMO
ENDIF
lcErrWoutPut=IIF(lcErrWoutPut='GWCCONTRL1',WLAST(),lcErrWoutPut)
lcErrWoutPut=IIF(lcErrWoutPut='FNDATION','',lcErrWoutPut)

lnCounter=1
*--Initialize the array of the program path
*--to know the place of error and where to return
DO WHILE !EMPTY(SYS(16,lnCounter))
  DIMENSION laErrTrac[lnCounter,1]
  laErrTrac[lnCounter,1]=SYS(16,lnCounter)
  lnCounter=lnCounter+1
  lnCounter=lnCounter+IIF(EMPTY(SYS(16,lnCounter+2)),2,0)
ENDDO

*--get the screen that the error occurs at if exist
lnCounter =ALEN(laErrTrac,1)
llFound = .F.

DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
    lcErrScr=laErrTrac[lnCounter,1]
    llFound=.T.
  lnCounter=lnCounter-1
ENDDO

*--get the program that the error occurs at if exist

lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) <> 'PROCEDURE ' AND RIGHT(laErrTrac[lnCounter,1],4)='.FXP'
    IF ATC('\',laErrTrac[lnCounter,1]) >0
      lcErrProgm = SUBSTR(laErrTrac[lnCounter,1],RAT('\',laErrTrac[lnCounter,1])+1)
    ELSE
      lcErrProgm = laErrTrac[lnCounter,1]
    ENDIF  
    llFound=.T.
  ENDIF  
  lnCounter=lnCounter-1
ENDDO

*--get the Function that calls the place where error occurs at if exist

lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     (RIGHT(laErrTrac[lnCounter,1],4)='.APP' OR RIGHT(laErrTrac[lnCounter,1],4)='.EXE';
     OR RIGHT(laErrTrac[lnCounter,1],4)='.FXP')
    lcErrCallFun=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    llFound=.T.
  ENDIF  
  lnCounter=lnCounter-1
ENDDO


*--get the Application "Module" that the error occurs at if exist
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
llFound = .F.
lcErrCallPrg = ''
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     (RIGHT(laErrTrac[lnCounter,1],4)='.APP' OR RIGHT(laErrTrac[lnCounter,1],4)='.EXE';
     OR RIGHT(laErrTrac[lnCounter,1],4)='.FXP')
    lcErrCallPrg=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    llFound=.T.
  ENDIF  
  lnCounter=lnCounter-1
ENDDO


llFound = .F.
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))

DO WHILE !llFound AND lnCounter>0 &&AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) <> 'PROCEDURE ' AND ;
     RIGHT(laErrTrac[lnCounter,1],4)='.APP'
    IF ATC('\',laErrTrac[lnCounter,1])>0
      lcErrApp=SUBSTR(laErrTrac[lnCounter,1],RAT('\',laErrTrac[lnCounter,1])+1)
    ELSE
      lcErrApp=laErrTrac[lnCounter,1]
    ENDIF 
    llFound=.T.
  ENDIF  
  lnCounter=lnCounter-1
ENDDO

*--get the main program of the error program
*--if empty then we will go to master
*--if not empty then it will have a function from
*--the aria3.prg that we can return to
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1)-1)
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     (RIGHT(laErrTrac[lnCounter,1],4)='.APP' OR RIGHT(laErrTrac[lnCounter,1],4)='.EXE');
     AND ATC(':\',laErrTrac[lnCounter,1])>0
    lcErrMainPrg=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    llFound=.T.
  ENDIF  
  lnCounter=lnCounter-1
ENDDO
PRIVATE lcToProc
IF USED('SYCERROR')
  IF !SEEK(lnError)
    SEEK 2000
  ENDIF  
  lcErrProcess = MEXECUTE
  lcErrMsg = mmumsg
ELSE  
  lcErrMsg = "Unable to find error description."+CHR(13)+CHR(10)+;
             "Please Contact: your system administrator."
  lcErrProcess = "lnOption=SHOWEROR()"+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess + ["END"] +CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +["PUSH BUTTON"]+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +"\!\?\<Ok"+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +["CONDITION 1"]+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +"DO gpGo_Mastr"+CHR(13)+CHR(10)
ENDIF
lnNoLine=IIF(ATCLINE('"END"',lcErrProcess)>0,;
             ATCLINE('"END"',lcErrProcess)-1,MEMLINES(lcErrProcess))
USE IN IIF(llErrFile,'SYCERROR',0)
FOR lnCounter=1 TO lnNoLine
  lnOption=0
    IF ATCLINE('"PUSH BUTTON"',lcErrProcess)>0
      lnCondNo=ATCLINE('"CONDITION '+STR(lnOption,1,1)+'"',lcErrProcess)+1
      FOR lnProc=lnCondNo TO MEMLINES(lcErrProcess)
          IF '"CONDITION' $ UPPER(MLINE(lcErrProcess,lnProc)) OR;
             '"PUSH BUTTON"' $ UPPER(MLINE(lcErrProcess,lnProc)) OR;
             ["END"] $ UPPER(MLINE(lcErrProcess,lnProc))
             EXIT
          ELSE
          lcToProc=MLINE(lcErrProcess,12)
          &lcToProc
          ENDIF
        ENDFOR
   ENDIF
ENDFOR


PROCEDURE ShowEror
ACTI MENU _MSYSMENU NOWAIT

_WRAP=.T.
_LMARGIN = 2
_RMARGIN = IIF(_DOS,48,55)
_ALIGNMENT = "LEFT"
lcPushB=IIF(ATCLINE('"PUSH BUTTON"',lcErrProcess)>0,;
         MLINE(lcErrProcess,ATCLINE('"PUSH BUTTON"',;
         lcErrProcess)+1),"\!  \<OK  ")
lcPb=lcPushB             
lcMessage = IIF(RDLEVEL()>4,lfGetMemLn(IIF(EMPTY(lcmsg),MESSAGE(),lcmsg)),lfGetMemLn(ALLT(lcErrMsg)))
*IF RDLEVEL()>4
  lnWinH = LEN(lcMessage)/46+4
  lnWinH = IIF(_DOS,INT(lnWinH),lnWinH)
*ELSE
*  lnWinH = 20
*ENDIF  
llSwap = .F.
IF RDLEVEL()>4
  lnWinH = MIN(lnWinH,20)
  llSwap = lnWinH=20
ELSE  
  lnWinH = lnWinH + 5
ENDIF
lcIcon = IIF(FILE("ARIA.ICO"),[ICON FILE "ARIA.ICO"],"")
DEFINE WINDOW gwdError FROM INT((SROW()-lnWinH)/2),INT((SCOL()-50)/2) ;
        TO INT((SROW()-lnWinH)/2)+lnWinH;
    	+IIF(_WINDOWS,5,1),INT((SCOL()-50)/2)+50;
        FLOAT ;
   		NOCLOSE ;
   		SHADOW ;
   		NOMINIMIZE ;
   		DOUBLE ;
   		COLOR RGB(,,,192,192,192) ;
   		&lcIcon ;
   		TITLE " ERROR ";
   		FONT "SYSTEM",10;
   		STYLE 'B' IN WINDOW FNDATION       		

MOVE WINDOW gwdError CENTER
ACTIVATE WINDOW gwdError TOP

IF RDLEVEL()>4
  *lcMessage = lfGetMemo(lcErrMsg)
  ?lcMessage
    DIMENSION laButtons[1]
    =gfSubStr(lcPushB,@laButtons,";")               

  FOR lnCounter=1 TO ALEN(laButtons,1)
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\<") 
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\!") 
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\?")     
  ENDFOR
  lnPbLngth=LEN(laButtons[1])
  FOR lnCounter=1 TO ALEN(laButtons,1)
    lnPbLngth=IIF(lnPbLngth>LEN(laButtons[lnCounter]),lnPbLngth,LEN(laButtons[lnCounter]))
  ENDFOR
  lnPbLngth=(lnPbLngth*ALEN(laButtons,1))+(5*ALEN(laButtons,1))
  lnxPush=INT((50-lnPbLngth)/2)+IIF(_DOS,0,IIF(ALEN(laButtons,1)=1,3,4))  &&-IIF(ALEN(laButtons,1)=1 AND _WINDOWS,5,0)
  lnOption=lfReadCycl('gwdError',lcpb,lnWinH-IIF(_DOS,1,2.2),lnxPush,3)
ELSE
  *MESSAGE()+CHR(13)+CHR(10)+
  lcMessage = lfGetMemo(ALLT(lcErrMsg)) &&lfGetMemLn(ALLT(lcErrMsg))
  lnWinH = lnWinH - IIF(llSwap,3,0)
  lcPushB= '"'+'@*HT '+lcPushB+'"'
  lnNoButt = OCCURS(';',lcPushB)+1
  lnPbLngth=13
  lnxPush= INT((50-(lnPbLngth*lnNoButt))/2)+IIF(lnNoButt=1,3,-2)
  @ 0,2 SAY PROPER("Error: "+IIF(EMPTY(lcmsg),MESSAGE(),lcmsg)) SIZE 1.25,50
  @ 1.00,2 SAY"Number : "+ALLT(STR(lnError)) SIZE 1.25,30
  @ 1.00,40 SAY "Line: "+ALLT(STR(lnLine)) SIZE 1.25,15    
  @ 2.00,2 SAY PROPER(PADR("Program: "+ALLT(lcModul),53)) SIZE 1.25,53
  @ WROW('gwdError')-1.8, lnxPush GET lnOption PICTURE &lcPushB;
                SIZE 1.769,13.000,lnxPush ;
        	  	  DEFAULT 1 

  @ 3.250,2 EDIT lcMessage SCROLL NoMod COLOR RGB(,,,192,192,192);
  SIZE WROW('gwdError')-5.250,53
  ACTIVATE WINDOW gwdError TOP  
  READ CYCLE MODAL 
ENDIF
IF WEXIST('gwdError')
  RELEASE WINDOW gwdError
ENDIF  

RETURN MAX(lnOption,1)


FUNCTION lfGetMemLn
PARAMETER lcMemo
PRIVATE lnCounter
lcRet=''
FOR lnCounter = 1 TO MEMLINES(lcMemo)
lcret=lcRet+lfGetMac(ALLTRIM(STRTRAN(MLINE(lcMemo,lnCounter),CHR(13)+CHR(10),' ',1)))+' '
ENDFOR
RETURN lcRet

FUNCTION lfGetMemo
PARAMETER lcMemo
PRIVATE lnCounter
lcRet=''
FOR lnCounter = 1 TO MEMLINES(lcMemo)
  lcRet= lcRet+lfGetMac(ALLTRIM(MLINE(lcMemo,lnCounter)))+CHR(13)+CHR(10)
ENDFOR
RETURN lcRet



PROCEDURE ShowEror1
  PRIVATE lnCounter
  lcMemoNam=IIF(glMultiUsr,"mMuMsg","mSuMsg")
  lnWinH=MEMLINES(&lcMemoNam)+3
  lnWinW=LEN(lfgetmac(MLINE(&lcMemoNam,1)))
  FOR lnCounter=2 TO MEMLINES(&lcMemoNam)
    lcNewStr=lfGetmac(MLINE(&lcMemoNam,lnCounter))
    lnWinW=IIF(LEN(lcNewStr)>lnWinW,;
            LEN(lcNewStr),lnWinW)
  ENDFOR
  lnMsgPos=lnWinW
  lcPushB=IIF(ATCLINE('"PUSH BUTTON"',lcErrProcess)>0,;
               MLINE(lcErrProcess,ATCLINE('"PUSH BUTTON"',;
               lcErrProcess)+1),"\!  \<OK  ")
  lcPb=lcPushB             
  DIMENSION laButtons[1]
  =gfSubStr(lcPushB,@laButtons,";")               

  FOR lnCounter=1 TO ALEN(laButtons,1)
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\<") 
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\!") 
    laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\?")     
  ENDFOR
  lnPbLngth=LEN(laButtons[1])
  FOR lnCounter=1 TO ALEN(laButtons,1)
    lnPbLngth=IIF(lnPbLngth>LEN(laButtons[lnCounter]),lnPbLngth,LEN(laButtons[lnCounter]))
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
        		COLOR RGB(,,,192,192,192) ;
        		TITLE " "+ALLTRIM(cTitle)+" ";
        		FONT "SYSTEM",9
        		
MOVE WINDOW gwdError CENTER        		
  ACTIVATE WINDOW gwdError NOSHOW       		
  FOR lnCounter=1 TO MEMLINES(&lcMemoNam)        		
    lcNewStr=lfGetMac(MLINE(&lcMemoNam,lnCounter))
    @ lnCounter,INT((lnWinW-LEN(lcNewStr))/2) SAY lcNewStr ;
    FONT "SYSTEM",9
  ENDFOR
  lnPbLngth=INT(lnPbLngth/ALEN(laButtons)-3)
lnOption=lfReadCycl('gwdError',lcpb,lnCounter,lnxPush,2)
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
PRIVATE lnCounter
DIMENSION laButtons[1]
=gfSubStr(lcpb,@laButtons,';')
PUSH KEY
*ON KEY
DIMENSION laPos[ALEN(laButtons,1),4],laHotkeys[ALEN(laButtons,1)]
lchotkeys=""
STORE 0 TO lnEsc,lnEnter,lnpblng
FOR lnCounter=1 TO ALEN(laButtons,1)
   laPos[lnCounter,4]=LEN(laButtons[lnCounter])
   IF OCCURS("\<",laButtons[lnCounter])>0
      laHotkeys[lnCounter]=UPPER(SUBSTR(laButtons[lnCounter],ATC("\<",laButtons[lnCounter])+2,1))
      lcHotkeys=lcHotkeys+SUBSTR(laButtons[lnCounter],ATC("\<",laButtons[lnCounter])+2,1)
       laPos[lnCounter,4]=laPos[lnCounter,4]-2
   ENDIF     
   IF OCCURS("\!",laButtons[lnCounter])>0
     lnEnter=lnCounter
     laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\!") 
     laPos[lnCounter,4]=laPos[lnCounter,4]-2
   ENDIF     
   IF OCCURS("\?",laButtons[lnCounter])>0
     lnEsc=lnCounter
     laButtons[lnCounter]=STRTRAN(laButtons[lnCounter],"\?")     
     laPos[lnCounter,4]=laPos[lnCounter,4]-2     
   ENDIF   
   laButtons[lnCounter]=laButtons[lnCounter]
   laPos[lnCounter,3]=ATC(laHotKeys[lnCounter],laButtons[lnCounter])
   laPos[lnCounter,4]=laPos[lnCounter,4]+2
   lnpblng=IIF(lnpblng>laPos[lnCounter,4],lnpblng,laPos[lnCounter,4])
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
FOR lnCounter=2 to ALEN(laButtons,1)
  laPos[lnCounter,1]=lnRow
  laPos[lnCounter,2]=lnCol+(lnpblng+pbSep)*(lnCounter-1)
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
PRIVATE lnCounter

IF lnMrow<-1
  ??CHR(7)  
  RETURN
ENDIF
llFound = .F.
lnCounter=0
IF lnMRow>=lnRow AND lnMRow<=lnRow+IIF(_DOS,0,2.000) ;
  AND lnMCol<=laPos[ALEN(laButtons),2]+lnpblng AND lnMCol>=lnCol
  DO WHILE !llFound AND lnCounter<ALEN(laButtons,1)
   lnCounter=lnCounter+1
     llFound=BETWEEN(lnMRow,lnRow,lnMRow+IIF(_DOS,0,2.000)) AND;
             BETWEEN(lnMCol,laPos[lnCounter,2],laPos[lnCounter,2]+lnpblng)
     
*     (lnMRow >= lnRow AND lnMCol>=laPos[lnCounter,2];
                AND lnMCol<=laPos[lnCounter,2]+;
                lnpblng);
                AND lnMRow<=laPos[lnCounter,1]+IIF(_DOS,0,2.000)
  ENDDO
ENDIF
IF llFound
  DO lfSelect WITH lnCounter
*ELSE
*  IF lnMrow<0
*    ??CHR(7)  
*  ENDIF  
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
*!              PROCEDURE gfPosOfScr
*! Purpose : Handling the position off the screen error
*!**********************************************************
PROCEDURE gfPosOfScr
WAIT "Setting Environment" WINDOW NOWAIT
DO gfsetenvmt
WAIT "Saving Error Log file." WINDOW NOWAIT
DO gfsaveErr
WAIT CLEAR
llErrClosWin = .F.
IF !EMPTY(lcErrApp)
  lcReturnTo = 'TO '+lcErrMainPrg
ELSE
  llErrClosWin = .T.
  lcReturnTo = 'TO MASTER'
ENDIF
IF ALLTRIM(lcReturnTo) == "TO" .OR. EMPTY(lcReturnTo) 
  RETURN TO MASTER
ELSE
  RETURN &lcReturnTo
ENDIF

*!**********************************************************
*!              PROCEDURE gpGo_Mastr
*!
*!**********************************************************
PROCEDURE gpGo_Mastr

WAIT "Setting Environment" WINDOW NOWAIT
DO gfsetenvmt
WAIT "Saving Error Log file." WINDOW NOWAIT
DO gfsaveErr
WAIT CLEAR
RELEASE lstext, lmtext
CLEAR TYPEAHEAD
*POP KEY ALL
*ON KEY
llErrClosWin = .F.
llErrClrRd   = .F.
SELECT (lcErrWorkAr)
if .f.
DO CASE
  CASE !EMPTY(lcErrScr) AND !EMPTY(lcErrProgm)

    IF WREAD(lcErrWOutPut)
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')
*        glQuitting = .T.    
      ELSE
        llErrClosWin=.T.  
      ENDIF 
      llErrClrRd   = .T.      
      lcReturnTo = 'TO '+lcErrProgm
    ELSE
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;      
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')
        glQuitting = .T.    
      ENDIF  
      lcReturnTo = 'TO '+lcErrProgm
 *     lcReturnTo = 'TO '+IIF(!EMPTY(lcErrApp),lcErrApp,lcErrMainPrg)      
    ENDIF
  CASE !EMPTY(lcErrScr) AND !EMPTY(lcErrCallFunc)    
    IF WREAD(lcErrWOutPut)
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;      
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')   
        glQuitting = .T.    
      ELSE
        llErrClosWin=.T.        
      ENDIF  
      llErrClrRd   = .T.      
      lcReturnTo = 'TO '+lcErrCallFunc
    ELSE
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;      
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')   
        glQuitting = .T.    
      ENDIF  
      lcReturnTo = 'TO '+lcErrCallFunc
    ENDIF  
  CASE !EMPTY(lcErrScr) AND EMPTY(lcErrProgm)
    IF WREAD(lcErrWOutPut)
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;      
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')   
        glQuitting = .T.    
      ELSE
        llErrClosWin=.T.        
      ENDIF  
      llErrClrRd   = .T.      
      lcReturnTo = 'TO '+IIF(!EMPTY(lcErrApp),lcErrApp,lcErrMainPrg)
    ELSE
      IF INLIST(SUBSTR(lcErrWOutPut,2,2),'AR','AD') OR ;
      LEFT(lcErrWOutPut,2)='CW' OR IIF(!EMPTY(WPARENT(lcErrWOutPut)),;
      INLIST(SUBSTR(WPARENT(lcErrWOutPut),2,2),'AR','AD') OR ;
      lcErrWOutPut = "GWCCONTRL1" OR ;      
      LEFT(WPARENT(lcErrWOutPut),2)='CW',.F.) OR ;
      INLIST(SUBSTR(lfGetWParnt(lcErrWOutPut),2,2),'AR','AD')   
        glQuitting = .T.    
      ENDIF  
      lcReturnTo = 'TO '+IIF(!EMPTY(lcErrApp),lcErrApp,lcErrMainPrg)
    ENDIF  
  CASE EMPTY(lcErrScr) AND !EMPTY(lcErrProgm)
    glquitting = .T.
    lcReturnTo = 'TO '+lcErrApp    
  CASE !EMPTY(lcErrCallPrg)  
    lcReturnTo = 'TO '+lcErrCallPrg
  CASE !EMPTY(lcErrApp)
    lcReturnTo = 'TO '+lcErrMainPrg
  OTHERWISE
  *E00000,1 Hesham (Start)
  *E00000,1 if the error occurd in the main program and there is no calling
  *E00000,1 program or screen or module then terminate the main program
    llErrClrRd = .T.
    lcReturnTo = 'TO MASTER'
ENDCASE
IF 'ARIABROW' $ lcErrProgm  OR 'GFOPGRID' $ lcErrProgm 
  llErrClosWin = .T.
  llErrClrRd = .T.
  llBrowTmnat = .T. &&glQuitting
  llogtrmnat = .T.
  llSelcted = .F.
  glQuitting = .F.
  IF 'GFOPGRID' $ lcErrProgm 
    llErrClrRd =WREAD('gwdgridopt')
    RELE WINDOW GWDGRID
    llErrClosWin = .F.
  ENDIF
ENDIF  

glFirstime = .F.
IF llErrClosWin
  ACTI SCREEN
  DO WHILE .T.
    lcErrWinNam=WCHILD(1)
    IF EMPTY(lcErrWinName)
      EXIT
    ELSE
      IF TYPE('lcTempWin')='C' AND lcErrWinNam=lcTempWin
        RELEASE WINDOW (lcErrWinNam)
      ENDIF
      IF WREAD(lcErrWinNam) 
        RELEASE WINDOW (lcErrWinNam)
      ENDIF          
    ENDIF
  ENDDO  
ENDIF
IF llErrClrRd
*  CLEAR READ
ENDIF
endif
SET EXACT OFF
DO ERORSET
*YMO
*IF ALLTRIM(lcReturnTo) == "TO" .OR. EMPTY(lcReturnTo) 
*  RETURN TO MASTER
*ELSE
*RETURN &lcReturnTo
*ENDIF
return


FUNCTION lfFromlpM
IF !USED('SYUEROR')
  RETURN
ENDIF
  lnCounter=1
  lcCallProg=""
  DO WHILE !EMPTY(SYS(16,lnCounter))
    lcCallProg = lcCallProg + SYS(16,lnCounter) + CHR(13)+CHR(10)
    lnCounter=lnCounter+1
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
  STORE gcWorkDir+ustring() + ".TXT" TO lmtext
  lcPsetup = _PDSETUP
  _PDSETUP = ''
  LIST MEMORY TO (lmtext)
  _PDSETUP = lcPsetup
*  ON KEY && Null out hot keys
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
IF !USED('SYUEROR')
  RETURN
ENDIF

SELECT SYUEROR
lnCounter=1
lcCallProg=""
FOR lnCounter =1 TO ALEN(laErrTrac,1)
  lcCallProg = lcCallProg + IIF(lnCounter=1,'',CHR(13)+CHR(10))+laErrTrac[lnCounter,1]
ENDFOR
IF !LLERROR    
  SET CONSOLE OFF
  APPEND BLANK
  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), nline_no WITH lnline;
      ProgCalls WITH lcCallProg, cSession WITH IIF(TYPE('lcSession')='C',lcSession,'')
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
  APPEND MEMO Memory FROM (lmtext)
  ERASE (lmtext)
  SET CONSOLE ON
ENDIF
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
IF !USED('SYUEROR')
  RETURN
ENDIF

* ==========================
* Store info to error file
* ==========================
  SELECT SYUEROR
  lnCounter=1
  lcCallProg=""
  DO WHILE !EMPTY(SYS(16,lnCounter))
    lcCallProg = lcCallProg + SYS(16,lnCounter) + CHR(13)+CHR(10)
    lnCounter=lnCounter+1
  ENDDO
IF !llError  
  SET CONSOLE OFF
  APPEND BLANK

*E300615,1 Hesham El-Sheltawi (Start)  
*E300615,1 change the name of the fields line_no to nline_no
*E300615,1 to prevent the conflict with the apparel fields
*E300615,1 to update the dictionary
*  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), line_no WITH lnline;
      ProgCalls WITH lcCallProg

*E300637,1 WAM 04/08/97 Add new field in the SyuEror file to save transaction
*E300637,1              session number
*  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), nline_no WITH lnline;
      ProgCalls WITH lcCallProg
  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), nline_no WITH lnline;
      ProgCalls WITH lcCallProg, cSession WITH IIF(TYPE('lcSession')='C',lcSession,'')
*E300637,1 WAM 04/08/97 (End)
      
*E300615,1 Hesham El-Sheltawi (End)  
  IF TYPE("UNAME") <> "U" .AND. ! EMPTY(uname)
      REPLACE lan_user WITH uname
  ELSE
      REPLACE lan_user WITH "NOT AVAILABL"
  ENDIF (TYPE("UNAME") <> "U" .AND. ! EMPTY(uname))

  SET CONSOLE ON
ENDIF
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

PROCEDURE kancel

  CLOSE DATA
  SET CURSOR ON
  SET LIBRARY TO
  SET COLOR TO W/N
  CLEAR
  SET CLEAR OFF && Suppresses "Normal Shutdown" message
  QUIT
* End procedure KANCEL


PROCEDURE qquit
  ON SHUTDOWN
*  ON KEY && Null out hot keys
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
  QUIT
* End procedure qquit


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
    llTrpErr = .F.
    ON ERROR llTrpErr = .T.
    SELECT (curr_dbf)
    *B603481,1 Due to error "record is out of range" I changed the following line to use
    *B603481,1 variables instead of using the functions !!
    *IF BETWEEN(RECNO(),1,RECCOUNT())    
    PRIVATE lnCurDBRec,lnMaxDBRec
    lnCurDBRec = RECNO()
    lnMaxDBRec = RECCOUNT()
    IF BETWEEN(lnCurDBRec,1,lnMaxDBRec)
      GO lnCurDBRec
      *B603481,1 End
    ELSE
      GO TOP
    ENDIF  
    ON ERROR
  ENDIF ! empty(curr_dbf)

  IF ! EMPTY(top_win) AND WEXIST(top_win)
    ACTIVATE WINDOW (top_win)
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
*E301077,78 Hesham (Start)
USE IN IIF(llUsrErr AND USED('SYUEROR'),'SYUEROR',0)
*E301077,78 Hesham (End)
ON ERROR DO gfEHan WITH ERROR(), MESSAGE(), ;
    MESSAGE(1), SYS(16), LINENO(), SYS(102), ;
    SYS(100), SYS(101), LASTKEY(), ALIAS(), ;
    SYS(18), SYS(5), SYS(12), SYS(6), SYS(2003), ;
    WONTOP(), SYS(2011), SYS(2018), SET("CURSOR")
RELEASE lstext, lmtext
RETURN
* End procedure ERORSET


PROCEDURE to_prn
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DO qquit
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
  ENDIF && prnstatus OR SYS(13) = "READY")
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
DO gpGo_Mastr
RETURN
*DO gfSetEnvmt
*DO gfSaveErr
*IF !glFromSav
*  DO CASE
*    CASE RIGHT(lcmodul,4) $ ".SPX,.FXP"   
*      glQuitting = .T.
*      CLEAR READ 
*    CASE RIGHT(lcmodul,4) $ ".APP"   
*      DO QQUIT
*  ENDCASE       
*ELSE
*  glErrOccur = .T.
*  glFlNotOpn = .T.
*ENDIF  
*DO ERORSET


*!*****************************************************************************
*!
*!      Function : lfDbfFix
*!
*!      Called by: 
*!               : 
*!
*!          Calls: gpGo_Mastr          
*!               : CHECKVAL
*!*****************************************************************************
*E300999,1 Hesham (Start)
*E300999,1 Function to Fix dbf if corrupted
*E301316,1 Walid Abou El-Magd Stop using this function and call gfFixDbf()
*E301316,1 I will just put a RETURN statment to do that .

FUNCTION lfDbfFix
PRIVATE lcOldOrder
lcOldOrder=""
DIMENSION laFixInfo[10]
laFixInfo=" "
laFixInfo[1]=" "
*-- Set the what to do parameter 
laFixInfo[2]="F" &&-- AutoFix  
*-- Set the detail parameter 
laFixInfo[3]="F" &&-- Summary  

*-- Set the backup parameter 
laFixInfo[4]="A" &&-- Ask

*-- Set the last backup parameter
laFixInfo[5]="A" &&-- Ask

*-- If error from gfOpenFile() or gfSysOpen() or gfSetup() 


IF TYPE("lcMacroSub")='C' AND "USE" $ UPPER(lcMacroSub)
  lcOldOrder=lcMacroSub
  lcMacroSub=UPPER(ALLTRIM(STRTRAN(UPPER(lcMacroSub),'USE ')))
  DO CASE
    *-- Call from gfOpenFile() or gfSysOpen()
    CASE "NFILE"$lcMacroSub
      laFixInfo[1]=NFILE
    *-- Call from gfOpenFile() or gfSetup()          
    CASE "LCPATH"$lcMacroSub OR "LAFILENAME"$lcMacroSub
      laFixInfo[1]=EVAL(lcMacroSub)
  ENDCASE
  =gfFixDbf(@laFixInfo)    
  IF UPPER(laFixInfo[10])='FIXED'
    DO erorset
    &lcOldOrder
  ELSE
    DO erorset
    DO gpGo_Mastr
  ENDIF
  RETURN
  
ELSE
  *--The system is unable to detect the name of corrupted file , Please run maintain dictionary to check for corrupted files.
  =gfModalGen('QRM00362B00000','Dialog')
  DO erorset
  DO gpGo_Mastr
ENDIF
RETURN



IF "USE " $ lckode AND FILE('FFIX4_25.FLL')
  SET LIBR TO FFIX4_25.FLL ADDI
ELSE
  DO gpGo_Mastr
ENDIF  
PRIVATE lcFileNam,llExtCheck,lcDirec
llExtCheck = .F.
lcFileNam = STRTRAN(UPPER(lckode),'USE ')
lcFileNam = ALLT(SUBSTR(lcFileNam,1,ATC(' ',lcFileNam)-1))
lcFileNam = IIF(lcFileNam='...','',lcFileNam)
IF EMPTY(lcFileNam)
*  RELEASE LIBRARY FFIX4-25.FLL
  DO gpGo_Mastr
ENDIF
IF TYPE(lcFileNam)<>'U'
  lcFileNam = EVAL(lcFileNam)
ENDIF

lcDirec = FULL(SET('DEFA'))
SET DEFA TO (SUBSTR(lcFileNam ,1,RAT('\',lcFileNam)-1))
lcFileNam = SUBSTR(lcFileNam,RAT('\',lcFileNam)+1)
lcFileNam = IIF(ATC('.DBF',lcFileNam)=0,lcFileNam+'.DBF',lcFileNam)
lnDbCheck = FIXDBF(lcFileNam,0)
lnFnCheck = FIXVFP(lcFileNam,0)
IF lnDbCheck>=0 AND lnFnCheck>=0
  IF lnDbCheck = lnFnCheck
    llExtCheck = .T.
  ENDIF
  lnDbCheck = MIN(lnDbCheck,lnFnCheck)
ENDIF
IF lnDbCheck<>0
  IF lnDbCheck<0
*    RELEASE LIBR FFIX4-25.FLL
    SET DEFA TO (lcDirec)
    DO gpGo_Mastr
  ELSE
    IF llExtCheck
      DIME laFix[1,1]
      = FIXDBF(lcFileNam,1)
      = CHECKVAL(lcFileNam,@laFix)
      Length1Ok = laFix[10,3] >= laFix[2,3]+laFix[3,3]*laFix[4,3]
      DIME laFix[1,1]
      =FIXVFP(lcFileNam,1)
      = CHECKVAL(lcFileNam,@laFix)      
      Length2Ok = laFix[10,3] >= laFix[2,3]+laFix[3,3]*laFix[4,3]      
      IF Length1Ok
        =FIXDBF(lcFileNam,1)
        lnDbCheck = 1
        lnFnCheck = 2
      ELSE
        lnDbCheck = 2
        lnFnCheck = 1
      ENDIF
    ELSE
      IF lnDbCheck < lnFnCheck
        =FIXDBF(lcFileNam,1)
      ELSE
        =FIXVFP(lcFileNam,1)  
      ENDIF
    ENDIF
  ENDIF
  IF FIXDBF(lcFileNam,0) <> 0
    SET DEFA TO (lcDirec)  
*    RELEASE LIBR FFIX4-25.FLL
    DO gpGo_Mastr
  ENDIF
ELSE
  SET DEFA TO (lcDirec)
*  RELEASE LIBR FFIX4-25.FLL
  DO erorset
  RETRY  
ENDIF
SET DEFA TO (lcDirec)
DO erorset
*RELEASE LIBR FFIX4-25.FLL
*E300999,1 Hesham (End)


*!*****************************************************************************
*!
*!      Function : CHECKVAL 
*!
*!      Called by: lfDbfFix
*!               : 
*!
*!          Calls:          
*!               : 
*!*****************************************************************************
*E300999,1 Hesham (Start)
*E300999,1 function to return the dbf file header in an array
FUNCTION CHECKVAL
PARAMETERS PDBFIX,FIXA
 SET SAFETY OFF
 SET CONSOLE OFF
 SET ALTERNATE TO FIX.XI
 SET ALTERNATE ON
 RV = FIXDBF(PDBFIX,2)
 SET ALTERNATE OFF
 SET ALTERNATE TO
 SET CONSOLE ON
 SET SAFETY ON
 IF RV<0
      RETURN RV
 ENDIF
 MEXT1 = RIGHT(UPPER(PDBFIX), 3)
 MATCH1 = "DBF DBC SCX VCX PJX FRX LBX MNX"
 MATCH2 = "FPT DBT SCT VCT PJT FRT LBT MNT"
 MEMNAME = LEFT(PDBFIX, LEN(PDBFIX)-3)+SUBSTR(MATCH2, AT(MEXT1, MATCH1), 3)
 IF FILE(MEMNAME)
      = ADIR(AFPTMEM, MEMNAME)
      MEMOATTACH = .T.
 ELSE
      MEMOATTACH = .F.
 ENDIF
 XI1 = FOPEN("FIX.XI")
 = FGETS(XI1)
 = FGETS(XI1)
* ARRAY FIXA : (VALUE TYPE(C),STORED(N),CALCULATED(N)
 DIMEN FIXA(13, 3)
 FOR LI = 1 TO 13
      FIXA(LI, 1) = FREAD(XI1, 15)
      IF EMPTY(FIXA(LI,1))
           EXIT
      ENDIF
      IF LI=7 .AND. !LEFT(FIXA(LI,1), 3)$'FPT'
*      WAIT WINDOW 'HELLO :'+FIXA(7,1)
           FIXA(9, 1) = FIXA(LI,1)
           LI = 9
           FIXA(7, 1) = 'FPT Next Block:'
           FIXA(7, 2) = -1
           FIXA(7, 3) = -1
           FIXA(8, 1) = 'FPT Block Size:'
           FIXA(8, 2) = -1
           FIXA(8, 3) = -1
      ENDIF
      IF "DBF Length"$FIXA(LI,1)
           FIXA(LI, 1) = LEFT(FIXA(LI,1), 11)+'    '
           = FSEEK(XI1, -1, 1)
           FIXA(LI, 2) = INT(VAL(FGETS(XI1)))
           FIXA(LI, 3) = FIXA(LI,2)
           DIMENSION FIXA(LI, 3)
           EXIT
      ELSE
           IF 'Has'$FIXA(LI,1) .OR. 'Container'$FIXA(LI,1)
                FIXA(LI, 2) = 'Yes'$FREAD(XI1, 11)
                FIXA(LI, 3) = 'Yes'$FGETS(XI1)
           ELSE
                FIXA(LI, 2) = INT(VAL(FREAD(XI1, 11)))
                FIXA(LI, 3) = INT(VAL(FGETS(XI1)))
           ENDIF
      ENDIF
 ENDFOR
 = FCLOSE(XI1)
 RSZ = ALEN(FIXA, 1)
 DIMENSION TEMPAR(RSZ, 3)
 = ACOPY(FIXA, TEMPAR, 7, -1)
 = ACOPY(TEMPAR, FIXA)
 IF MEMOATTACH
      DIMENSION FIXA(RSZ-1, 3)
      FIXA(RSZ-1, 1) = 'Memo size'
      FIXA(RSZ-1, 2) = AFPTMEM(1,2)
      FIXA(RSZ-1, 3) = 512+(FIXA(5,2)-8)*FIXA(6,2)
 ELSE
      DIMENSION FIXA(RSZ-2, 3)
 ENDIF
*E300999,1 Hesham (End)


FUNCTION lfGetWParnt
PARAMETERS lcWindPar
DO WHILE !EMPTY(WPARENT(lcWindPar)) AND WPARENT(lcWindPar)<>'FNDATION'
  lcWindPar=WPARENT(lcWindPar)
ENDDO
RETURN lcWindPar

*B603686,1 Hesham (Start)
*B603686,1 Get numeric overflow Fields
FUNCTION lfGetOvrf
PRIVATE lnCount,lcVarN
SELECT (curr_dbf)
SCATT MEMVAR 
lcmissing = "File Name = "+DBF()+CHR(13)+CHR(10)+;
            "Key Value = " + EVAL(KEY()) + CHR(13)+CHR(10)+;
            "Fields = "
FOR lnCount = 1 TO FCOUNT()
  IF TYPE(FIELD(lnCount)) = 'N'
    lcVarN = "M."+field(lnCount)
    IF LEFT(STR(&lcVarN),1) = "*"
      *lcmissing = lcmissing + " " + field(lnCount)
      lcmissing = lcmissing + field(lnCount) + CHR(13)+CHR(10)
    ENDIF
  ENDIF
ENDFOR


*B603686,1 Hesham (Start)
*B603686,1 Control the numeric overflow error
FUNCTION lfTrapOvrf
DO gfSaveErrNo
SET CONSOLE &lcKonsol
SET DEVICE TO &lcDevice
SELECT (curr_dbf)
SCATT MEMVAR 
lnFldNo = 0
lcmissing = ""
FOR lnCount = 1 TO FCOUNT()
  IF TYPE(FIELD(lnCount)) = 'N'
    lcVarN = "M."+field(lnCount)
    IF LEFT(STR(&lcVarN),1) = "*"
      lnFldNo = lnFldNo + 1
      DIME laFldErr[lnFldNo,2]
      laFldErr[lnFldNo,1] = field(lnCount)
      laFldErr[lnFldNo,2] = lnCount
    ENDIF
  ENDIF
ENDFOR
IF lnFldNo > 0
  FOR lnCount = 1 TO lnFldNo
    REPLACE (FIELD(laFldErr[lnCount,2])) WITH 0
  ENDFOR
ENDIF
DO ERORSET
