*:************************************************************************
*: Program file  : GETFILE.PRG
*: Program desc. : Main system
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
PARAMETERS lcProgram,lcTitle
lnGpPag=1
lcNOPage=''
lnTypeAhead=SET('TYPEAHEAD')
SET TYPEAHEAD TO 128
lcTitle=IIF(TYPE('lcTitle')='C' AND !EMPTY(ALLTRIM(lcTitle)),ALLTRIM(lcTitle),'Report')
lcTitle=ALLTRIM(lcTitle)
*UPPER(ALLTRIM(lcTitle))
lcConS=SET('CONSOLE')
SET CONSOLE ON
     SET SHADOWS OFF
*     DEFINE WINDOW Opm_Btns;
      FROM SROWS()-IIF(_DOS,2,2), 0 ;
      TO   SROWS()-IIF(_DOS,2,0),;
           SCOLS()-IIF(_DOS,1,0) ;
      NONE NOFLOAT NOGROW NOCLOSE;
      COLOR W/W+,,,,,,R*/W,,B/W+,
*      COLOR SCHEME 9


*  ACTIVATE WINDOW opm_btns NOSHOW

  PUSH KEY
  ON KEY LABEL Ctrl+W DO LFVOK
  ON KEY LABEL ESCAPE DO LFVOK
  ON KEY LABEL Ctrl+Q DO LFVOK  
  ON KEY LABEL O DO LFVOK    
  ON KEY LABEL CTRL+ENTER DO LFVOK  
  ON KEY LABEL TAB DO LFKEBOARD WITH 94


*  WAIT CLEAR

  DO MODIFIL.SPR
  POP KEY
SET SHADOWS ON
SET CONSOLE &lcConS
SET TYPEAHEAD TO lnTypeAhead

FUNCTION lfModiPrg
 MODI COMM (lcProgram)  WINDOW AWDOUTfl IN WINDOW MODFILHST  NOEDIT  NOWAIT SAVE 
 SET SKIP OF POPUP P02PU02 .F.

	
FUNCTION lfvModFile
RETURN !WEXIST('AWDOUTFL') AND !WEXIST('OPM_BTNS') AND !WEXIST(lcTitle) AND !WEXIST('MODFILHST')

FUNCTION lfvOk
RELEASE WINDOW MODFILHST,awdoutfl, Opm_Btns 
IF WEXIST((lcTitle))
  RELEASE WINDOW (lcTitle)
ENDIF  
CLEAR READ

RETURN TO GETFILE



FUNCTION LFKEBOARD
PARAMETER lnKeyPres
IF WONTOP()='OPM_BTNS'
  ACTIVATE WINDOW (lcTitle) TOP
ELSE
  ACTIVATE WINDOW  TOP Opm_Btns
ENDIF


FUNCTION lfvGoPage
PUSH KEY
on key
llGo=.F.
DO gopage.spr
POP KEY
IF llGo
  lcnopage=STR(lnGpPag,3,0)
  keyboard "{TAB}"+"{CTRL+HOME}"+"{CTRL+F}"+"Page : "+ALLTRIM(lcNoPage)+"{CTRL+ENTER}"
* =lfPageNo()
ENDIF

FUNCTION lfpageno
ACTIVATE WINDOW  TOP Opm_Btns
lcnopage=STR(lnGpPag,3,0)
*lcNoPage=STRTRAN(lcNoPage,' ',[+"{PGDN}"])
*lcNOPage=IIF(WONTOP()='OPM_BTNS',["{TAB}"+],'')+["{CTRL+HOME}"]+lcNoPage
*lcNOPage="1,ctrlh,"+ALLTRIM(STR(lnGpPag))+',pgDn'
*IIF(WONTOP()='OPM_BTNS',"1,TAB,",'')+
*=lfPlayMacro(lcNoPage)


FUNCTION lfGoFirst
ACTIVATE WINDOW  TOP Opm_Btns
lnGpPage=1
lcNOPage=IIF(WONTOP()='OPM_BTNS',["{TAB}"+],'')+["{CTRL+HOME}"]
keyboard &lcNoPage

FUNCTION lfvGoLast
ACTIVATE WINDOW  TOP Opm_Btns
lcNOPage=IIF(WONTOP()='OPM_BTNS',["{TAB}"+],'')+["{CTRL+END}"+"{PGUP}"]
keyboard &lcNoPage


FUNCTION lfNextPage
ACTIVATE WINDOW  TOP Opm_Btns
lcNOPage=IIF(WONTOP()='OPM_BTNS',["{TAB}"+],'')+["{PGDN}"]
keyboard &lcNoPage

FUNCTION lfvPrev
ACTIVATE WINDOW  TOP Opm_Btns
lcNOPage=IIF(WONTOP()='OPM_BTNS',["{TAB}"+],'')+["{PGUP}"]
keyboard &lcNoPage



FUNCTION lfPlayMacro
PARAMETERS lcMacro,lcMacName,lcHotKey
*PRIVATE ALL LIKE lc*,la*
lcHotKey=IIF(TYPE('lcHotKey')='C',lcHotKey,'˜q')
lcHotKey=PADR(lcHotKey,2)
lcMacName=IIF(TYPE('lcMacName')='C',lcMacName,'MYMACRO')
lcSelect=SELECT()
DIMENSION laKeys[1,2]
pgDn='Q'
pgUp='I'
CtrlH='w!'
dnArr='P'
UPARR='H'
Tab  = CHR(9)+CHR(0)
home = 'G'
End  = 'O'
=gfSubStr(lcMacro,@laKeys)
DIMENSION laKeys[ALEN(laKeys,1)/2,2]
*lcMacro='activate window (lctitle)'
*lcStrMac=''

*FOR lnCount=1 to len(lcMacro)
*  lcStrMac=lcStrMac+SUBSTR(lcMacro,lnCount,1)+CHR(0)
*ENDFOR
*lnPage=LEN(lcMacro)+1
*lcMacro=lcStrMac++CHR(13)+CHR(0)
lcMacro=''
lnPage=0
FOR lnCount=1 TO ALEN(laKeys,1)
   lcMacro=lcMacro+REPLI(&laKeys[lnCount,2],VAL(laKeys[lnCount,1]))
   lnPage=lnPage+VAL(laKeys[lnCount,1])
ENDFOR
CREATE CURSOR CURMACRO (lckey M)
LC='K'+CHR(255)+'y'+SPACE(13)+CHR(0)+CHR(1)+CHR(0)+lcMacName+REPLICATE(CHR(0),13)+CHR(lnPage)+CHR(0)+lcHotKey+lcMacro+CHR(26)
APPEND BLANK
SAVE MACRO TO MEMO lcKey
lnRecSave=RECNO()
CLEAR MACRO
append blank
replace lckey with lc
RESTORE MACRO FROM MEMO LCKEY
PLAY MACRO &lcMacName
CLEAR MACRO
GO lnRecSave
IF !EMPTY(lcKey)
  RESTORE MACRO FROM MEMO LCKEY
ENDIF  
USE
SELECT(lcSelect)

