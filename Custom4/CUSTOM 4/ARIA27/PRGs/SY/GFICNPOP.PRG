*:************************************************************************
*: Program file  : GFICNPOP.PRG
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
PARAMETERS lcHostWin,lnYpos,lnXpos


*** Define All windows
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

@ 0.000,0.000 GET SDF ;
   PICTURE "@*BHN prior.bmp";
	  SIZE 0.000,lnMaxWdth+.1,0.800 ;
	  DEFAULT 1 ;
	VALID lfMoveUp()


*** Define all programs icons
ACTIVATE WINDOW SYSCROLL NOSHOW


FOR lnIcon = 1 TO ALEN(laAllMdprg,1)

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
  	  PICTURE "@*IVT "+REPLICATE(";",lnButtNo) ;
	  SIZE 3.958,lnMaxWdth,0.1 ;
      WHEN lfwProgNam() ;
      VALID lfvProgNam() ;
	  DEFAULT 1 ;
	  COLOR ,,,,,,,,RGB(0,255,0,,,,)


*** Define the bottom button
ACTIVATE WINDOW SYBOTTOM NOSHOW

@  0.000,0.000 GET SDF ;
	  PICTURE "@*BHN next.bmp";
	  SIZE 0.000,lnMaxWdth+.1,0.800 ;
	  DEFAULT 1 ;
      VALID lfMoveDon()


ACTIVATE WINDOW SYTOP
ACTIVATE WINDOW SYBOTTOM
ACTIVATE WINDOW SYSCROLL 
ACTIVATE WINDOW syview 
ACTIVATE WINDOW syprglist 


READ CYCLE 

RELEASE WINDOW syprglist

RETURN ibProgNam

***> End of program



