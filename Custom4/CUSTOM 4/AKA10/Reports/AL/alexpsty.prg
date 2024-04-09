*:*********************************************************************************
*: Program file  : ALEXPSTY.PRG
*: Program desc. : Custom Export Style Master file for AKA10
*:        System : Aria4 XP.
*:        Module : (AL).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201214,C201215
*:*********************************************************************************
*: Modifications :
*:********************************************************************************
IF EMPTY(lcRpPath) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name or Path")
  RETURN .F.
ENDIF 
IF !EMPTY(lcRpPath) 
  lcDir = JUSTPATH(lcRpPath)
  IF !DIRECTORY(lcDir)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Path")
    RETURN .F.
  ENDIF   
ENDIF 

IF llOgFltCh
  lfCreateTmp()
	lcStySel = ''
	llStyleSelect = .F.
	lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.STYLE")
	IF lnPosSty > 0 
	  lnPosSty= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty ,1)
	  lcStySel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosSty,6]),loOGScroll.laOgFxFlt[lnPosSty,6],'')
	  IF !EMPTY(lcStySel) AND USED(lcStySel)
	    SELECT(lcStySel)
	    LOCATE
	    IF !EOF()
	      llStyleSelect  = .T.
	    ENDIF 
	  ENDIF 
	ENDIF 

	STORE  0 TO lnClrLen ,lnClrPos

	DECLARE laItemSeg[1]
	PRIVATE lnCount 
	=gfItemMask(@laItemSeg)
	FOR lnCount = 1 TO ALEN(laItemSeg,1)
	  IF laItemSeg[lnCount,1]='C'
	    lnClrLen = LEN(laItemSeg[lnCount,3])
	    lnClrPos = laItemSeg[lnCount,4]
	    lcClrSpr = ALLT(laItemSeg[lnCount,6])
	    EXIT
	  ENDIF
	ENDFOR


	IF llStyleSelect  
	  SELECT(lcStySel)
	  SCAN 
	    WAIT WINDOW 'Collecting Date For...' + &lcStySel..STYLE  NOWAIT
	    SELECT STYLE
	    =gfSeek(&lcStySel..STYLE)
	    =gfSeek('S'+STYLE.Scale,'SCALE')
	    FOR lnA = 1 TO Scale.cnt
	      lcA = ALLTRIM(STR(lnA))
	      lcScale =Scale.SZ&lcA. 
	      lcUpc = ''
	      IF gfSeek(STYLE.STYLE+lcA,'STYLEUPC')
	        lcUpc = STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
	      ENDIF 
	      IF !SEEK(STYLE.STYLE+lcScale ,lcStyleTmp)
	        SELECT (lcStyleTmp)
	        APPEND BLANK 
	        REPLACE STYLE WITH STYLE.STYLE,;
	        		COLOR WITH SUBSTR(STYLE.STYLE,lnClrPos ,lnClrLen ),;
	        		SIZE  WITH lcScale ,;
	        		UPCCODE with lcUpc ,;
	        		PRICE with STYLE.PRICEA,;
	        		DESC  with STYLE.DESC,;
	        		CSTYMAJOR WITH STYLE.cSTYMAJOR
	      ENDIF 
	    ENDFOR   
	  ENDSCAN 
	ELSE
	  SELECT STYLE 
	  =gfSeek('')
	  SCAN 
	    WAIT WINDOW 'Collecting Date For...' + STYLE.STYLE  NOWAIT
	    =gfSeek('S'+STYLE.Scale,'SCALE')
	    FOR lnA = 1 TO Scale.cnt
	      lcA = ALLTRIM(STR(lnA))
	      lcScale =Scale.SZ&lcA. 
	      lcUpc = ''
	      IF gfSeek(STYLE.STYLE+lcA,'STYLEUPC')
	        lcUpc = STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
	      ENDIF 
	      IF !SEEK(STYLE.STYLE+lcScale ,lcStyleTmp)
	        SELECT (lcStyleTmp)
	        APPEND BLANK 
	        REPLACE STYLE WITH STYLE.STYLE,;
	        		COLOR WITH SUBSTR(STYLE.STYLE,lnClrPos ,lnClrLen ),;
	        		SIZE  WITH lcScale ,;
	        		UPCCODE WITH  lcUpc ,;
	        		PRICE with STYLE.PRICEA,;
	        		DESC  with STYLE.DESC,;
	        		CSTYMAJOR WITH STYLE.cSTYMAJOR
	      ENDIF 
	    ENDFOR   
	  ENDSCAN 
	ENDIF
ENDIF 

SELECT (lcStyleTmp)
SET ORDER TO 
LOCATE
IF !EOF()
  lnOutFile = FCREATE(lcRpPath,0)
  IF lnOutFile < 0
    =gfModalGen('TRM00000B00000','ALERT','','','Could not create file. Cannot proceed.')
    RETURN .F.
  ENDIF
  = FPUTS( lnOutFile ,"STYLE,COLOR,SIZE,UPCCODE,DESCRIPTION,PRICE")
  lcFileLine = ""
  SCAN 
    lcFileLine = ""+CSTYMAJOR+","+COLOR+","+SIZE+","+UPCCODE+","+DESC  +","+STR(PRICE ,12,2)+""
    = FPUTS( lnOutFile ,lcFileLine)
  ENDSCAN 
  =FCLOSE(lnOutFile)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcRpPath)+" has been exported successfully")
ELSE
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF 

*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2010
*! Purpose   : Validate Path/File Name
*!************************************************************************** 
FUNCTION lfvPath
IF !EMPTY(lcRpPath) AND '?' $ lcRpPath  
  lcRpPath = GETFILE('CSV','Select the Output File Location and Name')
ENDIF 
RETURN .T.
*!**************************************************************************
*! Name      : lfCreateTmp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2010
*! Purpose   : Create temp method
*!************************************************************************** 
FUNCTION lfCreateTmp
DIMENSION laFileStruct[7,4]
laFileStruct[1,1] = 'STYLE'
laFileStruct[1,2] = 'C'
laFileStruct[1,3] = 19
laFileStruct[1,4] = 0

laFileStruct[2,1] = 'COLOR'
laFileStruct[2,2] = 'C'
laFileStruct[2,3] = 6
laFileStruct[2,4] = 0

laFileStruct[3,1] = 'SIZE'
laFileStruct[3,2] = 'C'
laFileStruct[3,3] = 5
laFileStruct[3,4] = 0

laFileStruct[4,1] = 'DESC'
laFileStruct[4,2] = 'C'
laFileStruct[4,3] = 20
laFileStruct[4,4] = 0

laFileStruct[5,1] = 'UPCCODE'
laFileStruct[5,2] = 'C'
laFileStruct[5,3] = 13
laFileStruct[5,4] = 0

laFileStruct[6,1] = 'PRICE'
laFileStruct[6,2] = 'N'
laFileStruct[6,3] = 12
laFileStruct[6,4] = 2

laFileStruct[7,1] = 'CSTYMAJOR'
laFileStruct[7,2] = 'C'
laFileStruct[7,3] = 12
laFileStruct[7,4] = 0

=gfCrtTmp(lcStyleTmp,@laFileStruct,'STYLE+SIZE',lcStyleTmp,.f.)
*!**************************************************************************
*! Name      : lfWWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2010
*! Purpose   : When method
*!************************************************************************** 
FUNCTION lfWWhen
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdPrint.enabled = .F.
=gfOpenTable('STYLE','STYLE')
=gfOpenTable('SCALE','SCALE')
=gfOpenTable('STYLEUPC','STYLEUPC')

