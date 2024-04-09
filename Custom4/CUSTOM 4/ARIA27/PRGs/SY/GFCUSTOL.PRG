*:************************************************************************
*: Program file  : GFCUSTOL.PRG
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
* Program to enable the user to cusomize the tool bar by adding new programs 
* or remove any of them or even change the order of the programes in the tool bar
* 
DECLARE laSysMdls [1,2],laBmpNams[1],laAllMdprg[1]
DECL lapbButt[1]

lnMaxWdth  = 45
lnPopHit   = 20
lnMaxHit   = 150

lnTxt1XPos = 13
lnTxt1YPos = .5
lnTxt2XPos = 13
lnTxt2YPos = 2
lnIconYpos = .5
lnLineYPos = 3.9

lnYShift   = 4
lnButtNo   = -1
lnFirstIcn = 0

ibProgNam  = 1
pbGotop    = 1
pbGoBottom = 1



***>

apbButt   = 0
pbButt    = 0
pbright   = 1
pbleft    = 1

lnButtPos = 1
lnMaxButt = 0
lnButtX   = 0.000
lnButtY   = 0.009
lnButtH   = 3.000
lnButtW   = 10.857
lnButtB   = 0.000
lnMaxNum  = 40

laBmpNams  = " "
laAllMdprg = " "
lcLastButt = " "
 
lcprompt   = " "


*** Collect avaleble modules in array
SELECT SYDAPPL.CAPP_ID+" - "+CAPP_NAME,CAPP_ID;
    FROM (gcSysHome+"SYDAPPL") ;
    WHERE CAPP_ID <> 'SY';
    INTO ARRAY laSysMdls



*** Collect module programs in array
SELECT cApObjNam,CPRGLNAME,RECNO() AS "nRecno" ;
      FROM (gcSyshome+"SYDOBJCT") ;
      WHERE CAPP_ID = laSysMdls[1] .AND. capobjtyp = "P" ;
      INTO ARRAY laAllMdprg ;


IF !USED('syuToolB')
  SELECT 0
  USE (gcSysHome+"syuToolB") ORDER 1
ELSE
  SELECT syuToolB 
  SET ORDER TO 1 
ENDIF


*** Check if the user have any icons define fform before and collect
*** them if there is 

IF SEEK(gcUser_Id)
  
  _TALLY = 0    
  ** Collect the names of the BMPs from the lwTolView file for the
  *** Current user
   SELECT cBmp_Nam,cProgNam,capp_id,cfld_msg FROM (gcSysHome+"syuToolB") ;
           WHERE cUser_id = gcUser_ID;
           INTO ARRAY laBmpNams 

   
   *** Open the array to the max number of icons to be selected.
   DECL laBmpNams [lnMaxNum,4]
   FOR lnClear = _TALLY + 1 TO ALEN (laBmpNams,1)
     laBmpNams [lnClear,1] = "  "
     laBmpNams [lnClear,2] = "  "
     laBmpNams [lnClear,3] = "  "
     laBmpNams [lnClear,4] = "  "
   ENDFOR

   lnMaxButt = lnMaxNum
    
   *IF lnMaxButt > 0
     DECL lapbButt[lnMaxNum]
     lapbButt = 0
   *ENDIF
ELSE
 
ENDIF

    

DO SYCUST.SPR


RETURN




*!**************************************************************************
*!
*!              Function : lfBuldTBar
*!
*!**************************************************************************
*
FUNCTION lfBuldTBar
PARAMETERS lcHostWin,lnWinYPos,lnWinXPos



***Build the tool bar in window

lnWinWid    = lnMaxButt * lnButtW 

DEFINE WINDOW lwTolDirct ;
    AT 0.000,0.000 ;
	SIZE 2.999,3.900 ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192);
    IN WINDOW (lcHostWin)

MOVE WINDOW lwTolDirct TO lnWinYPos,lnWinXPos


DEFINE WINDOW lwTolView ;
	AT 0.000,0.000 ;
	SIZE 2.999,43.400 ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192);
	IN WINDOW (lcHostWin)

MOVE WINDOW lwTolView TO lnWinYPos,lnWinXPos+4.0
    

DEFINE WINDOW lwTolIcon ;
	AT 0.000,0.000 ;
	SIZE 2.999,lnWinWid ;
	FONT "FoxFont", 9 ;
	NOFLOAT ;
	NOCLOSE ;
	NOMINIMIZE ;
	NONE;
	COLOR RGB(0,0,0,192,192,192);
	IN WINDOW lwTolView


*** Window with navigation buttons
ACTIVATE WINDOW lwTolDirct NOSHOW


@ 0.000,0.000 GET pbright ;
   PICTURE "@*HN >" ;
   SIZE 1.5,3.400,0.000 ;
   DEFAULT 1 ;
   FONT "FoxFont", 9 ;
   STYLE "B" ;
   VALID lfvMovRight()

@ 1.5,0.000 GET pbleft ;
   PICTURE "@*HN <" ;
   SIZE 1.5,3.400,0.000 ;
   DEFAULT 1 ;
   FONT "FoxFont", 9 ;
   STYLE "B" ;
   VALID lfvMovLeft()


*** Window view 
ACTIVATE WINDOW lwTolView NOSHOW

*** Window with icon butoons
ACTIVATE WINDOW lwTolIcon NOSHOW


*** Build all the tool bar buttons from array holding the names
*** of the bmps
IF lnMaxButt > 0

	FOR lnButtCnt = 1 TO ALEN(laBmpNams,1)
*	  lcButtStat = IIF(BETWEEN(lnButtCnt,lnButtPos,lnButtPos+7),;
	                   "ENABLE","DISABLE")  

*		  &lcButtStat 

      *** If the icon still exist in the same directory show it else show 
      *** blanck icon.
      IF FILE (gcBmpHome+ laBmpNams[lnButtCnt,1])
       @ lnButtY,lnButtX +(lnButtW * (lnButtCnt-1)) GET lapbButt[lnButtCnt] ;
	  	  PICTURE "@*CB " + gcBmpHome+ laBmpNams[lnButtCnt,1];
		  SIZE lnButtH,lnButtW,lnButtB ;
		  VALID lfVldButt() ;
		  DEFAULT 1 ;
	      FONT "FoxFont", 9 ;
	      MESSAGE gfObj_msg("&laBmpNams[lnButtCnt,4]")
	   ELSE
	    @ lnButtY,lnButtX +(lnButtW * (lnButtCnt-1)) GET lapbButt[lnButtCnt] ;
     	  PICTURE "@*CB " +"CLEAR.BMP";
		  SIZE lnButtH,lnButtW,lnButtB ;
		  VALID lfVldButt() ;
		  DEFAULT 1 ;
	      FONT "FoxFont", 9 ;
	      MESSAGE gfObj_msg("&laBmpNams[lnButtCnt,4]")

	   ENDIF   


	ENDFOR  

ENDIF


*** Activate all windows
ACTIVATE WINDOW lwTolIcon     
ACTIVATE WINDOW lwTolView
ACTIVATE WINDOW lwTolDirct



*** Build the content of the icon popup

lnYpos    = WLROW("SYCUST1")-WLROW("sycustool")-2.1
lnXpos    = WLCOL("SYCUST1")-WLCOL("sycustool")-.4


*** Define All windows used in the icon popup
DEFINE WINDOW syprglist ;
		AT 0.000, 0.000 ;
		SIZE lnPopHit,lnMaxWdth ;
		FONT "FOXFONT", 9 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		COLOR RGB(,,,255,255,255);
		IN WINDOW (lcHostWin)

MOVE WINDOW syprglist TO lnYpos-lnPopHit,lnXpos

DEFINE WINDOW SYTOP ;
		AT 0.00, 0.000 ;
		SIZE 2.00,lnMaxWdth ;
		FONT "FOXFONT", 9 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		NONE ;
		COLOR RGB(,,,255,255,255) ;
		IN WINDOW syprglist

DEFINE WINDOW SYBOTTOM ;
		AT 18.10, 0.000 ;
		SIZE 2.00,lnMaxWdth ;
		FONT "FOXFONT", 9 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		NONE ;
        COLOR RGB(,,,255,255,255) ;
		IN WINDOW syprglist


DEFINE WINDOW syview ;
		AT 2.00, 0.000 ;
		SIZE 16.000,lnMaxWdth ;
		FONT "FOXFONT", 9 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		NONE ;
		COLOR RGB(,,,255,255,255) ;
		IN WINDOW syprglist

DEFINE WINDOW SYSCROLL ;
		AT 0.000, 0.000 ;
		SIZE lnMaxHit,lnMaxWdth ;
		FONT "FOXFONT", 9 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		NONE ;
		COLOR RGB(0,0,0,255,255,255) ;
		IN WINDOW syview


*** Define the top button
ACTIVATE WINDOW SYTOP NOSHOW


@ 0.000,0.000 GET pbGotop ;
   PICTURE "@*BHN prior.bmp";
	  SIZE 0.000,lnMaxWdth+.1,0.800 ;
	  DEFAULT 1 ;
	VALID lfMoveUp()


*** Define all programs icons
ACTIVATE WINDOW SYSCROLL NOSHOW


FOR lnIcon = 1 TO ALEN(laAllMdprg,1)

  *** Show the program icon if ther is one 
  IF FILE (gcBmpHome+ALLTRIM(laAllMdprg[lnIcon,1])+".BMP")

    @ lnIconYpos,1.5 GET b1 ;
	    PICTURE "@*BHN " + ;
            (gcBmpHome+ALLTRIM(laAllMdprg[lnIcon,1])+".BMP");
	    SIZE 3.000,11.000,0.800 ;
	   	FONT "FOXFONT", 9 ;
	    DEFAULT 1 ;
        WHEN .f.

  ENDIF
  
  @ lnTxt1YPos,lnTxt1XPos SAY ALLTRIM(laAllMdprg[lnIcon,2]) ;
	FONT "Times New Roman", 12 ;
	STYLE "BIT" ;
	  COLOR RGB(0,0,255,,,,)
	  

  @ lnTxt2YPos,lnTxt1XPos SAY ALLTRIM(laAllMdprg[lnIcon,2])  ;
	FONT "MS SANS SERIF", 10 ;
	STYLE "T" ;
	  COLOR RGB(0,0,0,,,,)

  @ lnLineYPos,0.00 TO lnLineYPos,lnMaxWdth ;
	PEN 2, 8 ;
	STYLE "1"

   lnIconYpos = lnIconYpos + lnYShift
   lnTxt1YPos = lnTxt1YPos + lnYShift
   lnTxt2YPos = lnTxt2YPos + lnYShift
   lnLineYPos = lnLineYPos + lnYShift
   lnButtNo   = lnButtNo   + 1

ENDFOR

*** Define the invisible button to select program

@ 0.100,0.100 GET ibProgNam ;
  	  PICTURE "@*IV "+REPLICATE(";",lnButtNo) ;
	  SIZE 3.958,lnMaxWdth,0.1 ;
      WHEN lfwProgNam() ;
      VALID lfvProgNam() ;
	  DEFAULT 1 ;
	  COLOR ,,,,,,,,RGB(0,255,0,,,,)



*** Define the bottom button
ACTIVATE WINDOW SYBOTTOM NOSHOW


@  0.000,0.000 GET pbGoBottom ;
	  PICTURE "@*BHN next.bmp";
	  SIZE 0.000,lnMaxWdth+.1,0.800 ;
	  DEFAULT 1 ;
      VALID lfMoveDon()


ACTIVATE WINDOW SYBOTTOM
ACTIVATE WINDOW SYSCROLL 
ACTIVATE WINDOW syview 
ACTIVATE WINDOW SYTOP




*!**************************************************************************
*!
*!     Function : lfvMovRight
*!
*!**************************************************************************
*
* Move tool bar icons to reight 
FUNCTION lfvMovRight     

IF lnButtPos = 1
  RETURN
ELSE
*  SHOW OBJECT (_CUROBJ+ lnButtPos ) ENABLE
  MOVE WINDOW lwTolIcon BY 0,lnButtW
*  SHOW OBJECT (_CUROBJ+5 + lnButtPos) DISABLE
  lnButtPos = lnButtPos - 1
ENDIF  


*!**************************************************************************
*!
*!     Function : lfvMovLeft
*!
*!**************************************************************************
* 
* Move tool bar icons to left
FUNCTION lfvMovLeft    


IF  lnButtPos = lnMaxButt - 4 .OR. (lnMaxButt - 4)<0
  RETURN
ELSE 
 * SHOW OBJECT (_CUROBJ+5 + lnButtPos) ENABLE 
  MOVE WINDOW lwTolIcon BY 0,-lnButtW-0.1
 * SHOW OBJECT (_CUROBJ+ lnButtPos ) DISABLE
  lnButtPos  = lnButtPos + 1
ENDIF  




*!**************************************************************************
*!
*!     Function : lfVldButt
*!
*!**************************************************************************
* 
* 
*
FUNCTION lfVldButt

IF !EMPTY(lcLastButt)
  &lcLastButt = 0
  SHOW GET &lcLastButt
ENDIF

lcLastButt = SYS(18)



*!**************************************************************************
*!
*!              Function : lfMoveUp
*!
*!**************************************************************************
*
* Move icon popup up
FUNCTION lfMoveUp     

IF lnFirstIcn <> 0 
  MOVE WINDOW SYSCROLL BY lnYShift,0
  lnFirstIcn = lnFirstIcn - lnYShift
ENDIF  



*!**************************************************************************
*!
*!              Function : lfMoveDon
*!
*!**************************************************************************
*
* Move icon popup down
FUNCTION lfMoveDon    

IF lnFirstIcn <> (LNBUTTNO-3)*lnYShift
  MOVE WINDOW SYSCROLL BY -lnYShift,0
  lnFirstIcn = lnFirstIcn + lnYShift
ENDIF  

  

*!**************************************************************************
*!
*!              Function : lfwProgNam
*!
*!**************************************************************************
*
* Scroll the popup content if moving with the keyboard
FUNCTION lfwProgNam

RETURN

IF INLIST(LASTKEY(),5,24)
  DO CASE 
    CASE  _CUROBJ-(LNBUTTNO+1*2) > (lnFirstIcn/4+1)+3
      = lfMoveDon()
    CASE  _CUROBJ-(LNBUTTNO+1*2) < lnFirstIcn/4+1
      =lfMoveUp ()
  ENDCASE  
ENDIF  


*!**************************************************************************
*!
*!              Function : lfvProgNam
*!
*!**************************************************************************
*
FUNCTION lfvProgNam


lcProgName = ALLTRIM(laAllMdprg[ibProgNam ,2])
lcProgDes  = ALLTRIM(laAllMdprg[ibProgNam ,2])
show get lcProgName
show get lcProgDes

IF FILE (gcBmpHome+ALLTRIM(laAllMdprg[ibProgNam ,1])+".bmp")
  lcprompt   = (gcBmpHome+ALLTRIM(laAllMdprg[ibProgNam ,1])+".bmp")
  show get pbprgicon,1 PROMPT lcprompt 
ELSE

ENDIF  

HIDE WINDOW syprglist

_CUROBJ = OBJNUM(pbSelect)



*!**************************************************************************
*!
*!              Function : lfvOpnPop
*!
*!**************************************************************************
*
FUNCTION lfvOpnPop


SHOW WINDOW syprglist
_CUROBJ = OBJNUM(ibProgNam)



*!**************************************************************************
*!
*!              Function : lfActivate
*!
*!**************************************************************************
*
FUNCTION lfActivate

RETURN


IF !(INLIST(WONTOP(),"SYBOTTOM","SYSCROLL","SYTOP"))
  HIDE WINDOW syprglist
ENDIF



*!**************************************************************************
*!
*!              Function : lfvInsert
*!
*!**************************************************************************
*
FUNCTION lfvInsert



SHOW GET &lcLastButt,1 PROMPT lcprompt



