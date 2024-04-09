*:************************************************************************
*: Program file  : GFERRHAN.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Yasser Saad Ibrahim
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
* The global variable glErrOccur Will be affected in case the error handler 
* want to tell the system to behave defrently in case of error
#DEFINE pnWinWidth 60
#DEFINE pnWinHight 15

PARAMETERS gnErrNO,gcErrMsg,lcErrCode,lcErrModul,lnErrLine, xprint, ;
           xkonsol, xdevice, keypress, curr_dbf, getfield, def_drive, ;
           run_memry, prnt_devic, cur_direc, top_win, xlocked, ;
           xmissing, curs_set

*** Save current setting to be restored latter
gcCurError = ON("ERROR")      
gcCurFile  = ALIAS()
gcCurEsc   = SET("ESCAPE")

*** Default value for some variables
gcErrRpTyp = 1                  && To identify the button combination 
gnErrRplay = 1                  && To hold the user replay
glErrTerm  = .F.
glErrLong  = .F.
glDevelop  = !EMPTY(GETENV('DEVELOP'))
glDemo     = !EMPTY(GETENV('DEMO'))
glRunTime  = !glDevelop         && Flag to tell the error handler to 
                                && behave diffrently and display more
                                && frendly information to the user.


SET ESCAPE  OFF
PUSH KEY
ON KEY
ON ERROR
ON ESCAPE


*** Open the system file that hold all the error messages and how
*** to behave with each of them.
IF !USED('syErrMsg')
  SELECT 0
  USE (gcSysHome+"syErrMsg")
ELSE
  SELECT syErrMsg
ENDIF

SET ORDER TO TAG NERRNUM

IF !SEEK(gnErrNO)
  *** In case of Unreportd error in the error message file creat 
  *** a new record in the file to be used latter.
  INSERT INTO syErrMsg ;
    (nerrnum,crepltyp,cbehav,cerrdis,mmoreinf,msolution)  VALUES ;
    (gnErrNO,'0','0',gcErrMsg,gcErrMsg," ")
ENDIF 

*** This flag is to be used latter to tell which error have happend
*** in this system and which did not.
REPLACE lErrHapen WITH .T.

*** Collect information needed from the error record

gcErrRpTyp = cReplTyp

*** With some errors we can ignor them with no harm to the functionality 
*** of the program or to the opend data files.

glErrTerm  = glDemo .OR. (cBehav = 'I')

IF glRunTime
  gcErrMsg = ALLTRIM(mmoreinf) 
ENDIF

*** If the error message is too long so it will be displaied 
*** in a scrollable box
glErrLong = (LEN(gcErrMsg) > 253)

*** Reselect the current file
IF !EMPTY(gcCurFile)
  SELECT (gcCurFile)
ENDIF

IF !glErrTerm 
  DO CASE 
    *** Ok
    CASE gcErrRpTyp = '0' 
      lcButton    = "\<OK"
      lnButtSize  = 4
      lnButtNo    = 1
      gcMsgCatgry = "TR"
    *** Ok,Cancel
    CASE gcErrRpTyp = '1' 
      lcButton    = "\<OK;\<Cancel"
      lnButtSize  = 6
      lnButtNo    = 2
      gcMsgCatgry = "TR"
    *** Abort,Retry,Ignore
    CASE gcErrRpTyp = '2'
      lcButton    = "\<Abort;\<Retry;\<Ignore"
      lnButtSize  = 6
      lnButtNo    = 3
      gcMsgCatgry = "QR"  
    *** Yes,No,Cancel
    CASE gcErrRpTyp = '3'
      lcButton    = "\<Yes;\<No;\<Cancel"
      lnButtSize  = 6
      lnButtNo    = 3
      gcMsgCatgry = "QR"   
    *** Yes,No
    CASE gcErrRpTyp = '4'
      lcButton    = "\<Yes;\<No"
      lnButtSize  = 3
      lnButtNo    = 2
      gcMsgCatgry = "QR"   
    *** Retry, Cancel
    CASE gcErrRpTyp = '5'
      lcButton    = "\<Retry;\<Cancel"
      lnButtSize  = 6
      lnButtNo    = 2
      gcMsgCatgry = "QR"   
  ENDCASE

  lnButtSize  = lnButtSize  + 4
  
  *** Calculat window width accordin to total buttons size
  IF glErrLong
    lnWinWidth = MAX(70,(lnButtSize * lnButtNo)+6)
  ELSE  
    lnWinWidth = MAX(50,(lnButtSize * lnButtNo)+6)
  ENDIF
  
  *** Calculat space between buttons 
  lnBetwButt = INT((lnWinWidth -(lnButtSize * lnButtNo))/lnButtNo)

  *** Calculat X position of the button to be centerd
  lnButXPos  = (lnWinWidth/2)-((lnButtSize * lnButtNo)+;
             (lnBetwButt*(lnButtNo-1)))/2

  *** Calculat window higth according to the length of the messag
  lnWinHight = INT(LEN(ALLTRIM(gcErrMsg))/lnWinWidth) + 7 

  *** Define dialog window with right width, hight and color

  DEFINE WINDOW gwErrMesg ;
         FROM 0,0 TO lnWinHight,lnWinWidth ;
         SHADOW;
         FLOAT ;
         DOUBLE;
         COLOR SCHEME 7
			  
  IF glDevelop
	DEFINE WINDOW sydeverr ;
		FROM 18, 9 ;
		TO 23,70       ;
		FLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		DOUBLE ;
		  COLOR SCHEME 7
  ENDIF


  *** Activate dialog window no show so the window will pop with messag 
  *** when activated
  ACTIVATE WINDOW gwErrMesg NOSHOW

  *** Move the dialog window to center of the screen
  MOVE WINDOW gwErrMesg CENTER

  @ 0,1,2,5 BOX
  @ 1,3  SAY SUBSTR ("!?I",(AT(gcMsgCatgry,"TRQRIN")+1)/2,1)
  @ 1,7  SAY "Error No.: "+STR(gnErrNO,4)
  IF glErrLong
    @ 2,7  EDIT gcErrMsg NOMODIFY SCROLL SIZE lnWinHight-3,lnWinWidth-10 COLOR SCHEME 7
  ELSE
    @ 2,7  SAY gcErrMsg SIZE lnWinHight-3,lnWinWidth-10 
  ENDIF

  @ lnWinHight -2 ,lnButXPos ;
      GET gnErrRplay ;
      PICTURE "@*HT "+lcButton;
      SIZE 1.5,lnButtSize,lnBetwButt
      *;
      MESSAGE gfObj_msg()


  IF glDevelop

    ACTIVATE WINDOW sydeverr NOSHOW

    @ 0,2 GET pbRetry ;
	    PICTURE "@*HT Retry" ;
	    SIZE 1,12,1 ;
  	    DEFAULT 1 ;
    	VALID _qwb0kufns()

    @ 0,17 GET pbDebug ;
	    PICTURE "@*HN Debug" ;
	    SIZE 1,12,1 ;
        DEFAULT 1 ;
        VALID _qwb0kufrt()

    @ 0,31 GET pbTrace ;
	    PICTURE "@*HN Trace" ;
	    SIZE 1,12,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kufvp()

    @ 0,47 GET pbCode ;
	    PICTURE "@*HN See Code" ;
	    SIZE 1,12,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kufzj()

    @ 2,1 GET pbSet ;
  	    PICTURE "@*HN See open files" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kug3i()

    @ 2,21 GET pbClrTrp ;
 	    PICTURE "@*HN Clear key trap" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kug7a()

    @ 2,41 GET pbDefMEnu ;
	    PICTURE "@*HN Default menu" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kugb1()

    @ 3,1 GET pbCommand ;
	    PICTURE "@*HN Command window" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kugez()

    @ 3,21 GET pbDispmem ;
	    PICTURE "@*HN Display memory" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kugjd()

    @ 3,41 GET pbDispSts ;
	    PICTURE "@*HN Display status" ;
	    SIZE 1,18,1 ;
	    DEFAULT 1 ;
	    VALID _qwb0kugn5()

	ACTIVATE WINDOW sydeverr     
  ENDIF  

  *** When the read is activated fox will activate the window 
  ACTIVATE WINDOW gwErrMesg

  READ CYCLE MODAL WITH "VIEW","COMMAND"
  RELEASE WINDOWS gwErrMesg,sydeverr
  

  *** Do action according to the behave type and the user selection
  IF glDevelop
    * gcErrRpTyp = "D"
  ENDIF
  
  
  DO CASE
      *** Ok
      CASE gcErrRpTyp = '0' 
         *** Ok
         glErrOccur  = .T.
      *** Ok, Cancel 
      CASE gcErrRpTyp = '1' 
        IF gnErrRplay =1 
          *** Ok 

        ELSE
          *** Cancel

        ENDIF   
      *** Abort, Retry, Ignore
      CASE gcErrRpTyp = '2' 
 
        DO CASE
          *** Abort
          CASE gnErrRplay =1
             glErrOccur  = .T.
          *** Retry   
          CASE gnErrRplay =2 
             =gfErrRest()             
             RETRY
          *** Ignore
          CASE gnErrRplay =3  


        ENDCASE
      *** Yes, No, Cancel
      CASE gcErrRpTyp = '3' 
        DO CASE
          *** Yes
          CASE gnErrRplay =1

          *** No
          CASE gnErrRplay =2

          *** Cancel
          CASE gnErrRplay =3


        ENDCASE
      *** Yes, No  
      CASE gcErrRpTyp = '4' 
        *** Yes
        IF gnErrRplay =1

        *** NO
        ELSE


        ENDIF

      *** Retry, Cancel 
      CASE gcErrRpTyp = '5' 
        IF gnErrRplay =1
          *** Retry
          =gfErrRest()             
          RETRY
        ELSE
          *** Cancel 

        ENDIF
  ENDCASE
ENDIF

IF glDemo 
  WAIT "Error.. " WINDOW NOWAIT
ENDIF 


=gfErrRest()

*!*************************************************************************
*!
*!                Function : gfErrRest
*!
*!*************************************************************************
* Restore the old environment
*
FUNCTION gfErrRest

ON ERROR  &gcCurError
SET ESCAP &gcCurEsc
POP KEY









*!*************************************************************************
*!
*!
*!
*!*************************************************************************
*

FUNCTION _qwb0kufns     &&  pbRetry VALID

=gfErrRest()
RETRY


FUNCTION _qwb0kufrt     &&  pbDebug VALID

ACTIVATE WINDOW DEBUG TOP


FUNCTION _qwb0kufvp     &&  pbTrace VALID

ACTIVATE WINDOW TRACE TOP

FUNCTION _qwb0kufzj     &&  pbCode VALID

WAIT lcErrCode WINDOW


FUNCTION _qwb0kug3i     &&  pbSet VALID

SET

FUNCTION _qwb0kug7a     &&  pbClrTrp VALID

ON KEY


FUNCTION _qwb0kugb1     &&  pbDefMEnu VALID

SET SYSMENU TO DEFAULT


FUNCTION _qwb0kugez     &&  pbCommand VALID

ACTIVATE WINDOW COMMAND TOP


FUNCTION _qwb0kugjd     &&  pbDispmem VALID

DO DISPMEM.SPR


FUNCTION _qwb0kugn5     &&  pbDispSts VALID

DO DISPSTAT.SPR
