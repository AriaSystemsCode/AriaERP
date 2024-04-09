*:************************************************************************
*: Program file  : TOOLBAR.PRG
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
PARAMETERS lcNowait

IF  glToolActv 
  IF EMPTY(gaBmpNams[1,1])
    glToolActv  = .F.
    RETURN
  ENDIF
ELSE
  RETURN
ENDIF  

glToolActv  = .T.

lcNowait    = IIF(TYPE('lcNowait')='C',UPPER(lcNowait)," ")
lnWinWid    = gnMaxButt * gnButtW 

lcActivWnd = WOUTPUT()

IF !WEXIST("toolbar") 
  DEFINE WINDOW toolbar ;
    IN WINDOW fndation	;
	AT 0.000,4.000 ;
	SIZE 2.999,75.900 ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192)
    
ENDIF

IF !WEXIST("toolbar1") 
  DEFINE WINDOW toolbar1 ;
    IN WINDOW fndation	;
	AT 0.000,0.000 ;
	SIZE 2.999,3.900 ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192)
ENDIF

IF !WEXIST("toolbar2") 
  DEFINE WINDOW toolbar2 ;
	AT 0.000,0.000 ;
	SIZE 2.999,lnWinWid ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192);
	IN WINDOW toolbar
ENDIF

IF WVISIBLE("toolbar1")
  ACTIVATE WINDOW toolbar1 SAME
ELSE
  ACTIVATE WINDOW toolbar1 NOSHOW
ENDIF

@ 0.000,0.000 GET pbright ;
   PICTURE "@*HN >" ;
   SIZE 1.5,3.400,0.000 ;
   DEFAULT 1 ;
   FONT "FoxFont", 9 ;
   STYLE "B" ;
   VALID gfvMovRight()

@ 1.5,0.000 GET pbleft ;
   PICTURE "@*HN <" ;
   SIZE 1.5,3.400,0.000 ;
   DEFAULT 1 ;
   FONT "FoxFont", 9 ;
   STYLE "B" ;
   VALID gfvMovLeft()

IF WVISIBLE("toolbar")
  ACTIVATE WINDOW toolbar SAME
ELSE
  ACTIVATE WINDOW toolbar NOSHOW
ENDIF

IF WVISIBLE("toolbar2")
  ACTIVATE WINDOW toolbar2 SAME
ELSE
  ACTIVATE WINDOW toolbar2 NOSHOW
ENDIF

*** Build all the tool bar buttons from array holding the names
*** of the bmps

FOR lnButtNo = 1 TO ALEN(gaBmpNams,1)
  lcButtStat = IIF(BETWEEN(lnButtNo,gnButtPos,gnButtPos+7),;
                   "ENABLE","DISABLE")  

  @ gnButtY,gnButtX +(gnButtW * (lnButtNo-1)) GET pbButt ;
	  PICTURE "@*BHN " + gcBmpHome+ gaBmpNams[lnButtNo,1];
	  SIZE gnButtH,gnButtW,gnButtB ;
	  DEFAULT 1 ;
      FONT "FoxFont", 9 ;
      VALID gfvToolVld("&gaBmpNams[lnButtNo,2]","&gaBmpNams[lnButtNo,3]");
      MESSAGE gfObj_msg("&gaBmpNams[lnButtNo,4]");
	  &lcButtStat
ENDFOR  

IF !WVISIBLE("toolbar2")
  ACTIVATE WINDOW toolbar2     
ENDIF     

IF !WVISIBLE("toolbar")
  ACTIVATE WINDOW toolbar
ENDIF     

IF !WVISIBLE("toolbar1")
	ACTIVATE WINDOW toolbar1
ENDIF     


IF lcNowait <> "NOWAIT"
  glTooAlon = .T.
  READ CYCLE 
  IF !glToolActv
    HIDE WINDOW toolbar 
    RELEASE WINDOWS toolbar,toolbar1
  ENDIF  
ELSE
  glTooAlon = .F.  
ENDIF	  


IF !EMPTY(lcActivWnd) .AND. WEXIST(lcActivWnd)
  ACTIVATE WINDOW (lcActivWnd) NOSHOW
ENDIF
