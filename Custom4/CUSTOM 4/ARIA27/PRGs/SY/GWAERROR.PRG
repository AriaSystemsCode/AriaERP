*:************************************************************************
*: Program file  : GWAERROR.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcProgram,lcProgram1,lnLineNo
DEFINE WINDOW _qgu0hz2j6 ;
		FROM INT((SROW()-11)/2),INT((SCOL()-63)/2) ;
		TO INT((SROW()-11)/2)+10,INT((SCOL()-63)/2)+62 ;
		FLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		DOUBLE ;
		COLOR SCHEME 7

ACTIVATE WINDOW _qgu0hz2j6 NOSHOW

@ 1,1 SAY MESSAGE() ;
	SIZE 2,59 ;
	PICTURE "@I"
@ 4,32 SAY lcProgram ;
	SIZE 1,10
@ 5,17 SAY "From program :" ;
	SIZE 1,14, 0
@ 5,32 SAY lcProgram1 ;
	SIZE 1,10
@ 4,17 SAY "Program .....:" ;
	SIZE 1,14, 0
@ 6,17 SAY "Line No. ....:" ;
	SIZE 1,14, 0
@ 6,32 SAY lnLineNo ;
	SIZE 1,10
@ 8,26 GET pbExit ;
	PICTURE "@*HT \?\!\<Ok" ;
	SIZE 1,8,1 ;
	DEFAULT 1

ACTIVATE WINDOW _qgu0hz2j6
READ CYCLE MODAL
RELEASE WINDOW _qgu0hz2j6

