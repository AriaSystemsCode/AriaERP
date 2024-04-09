*:************************************************************************
*:
*: Procedure file: GLACBROW.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfvAcnTyp
*:               : lfvSelAcn
*:               : lfvBrCanc
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B602858,1 AMM 05/02/99 Fix the bug in the filter on account when browsing
*B605999,1 ASH 05/20/2002 Add inceremntal search in the account browse.
*:************************************************************************
*

*** Default values & defining variables ***
DO CASE
  CASE _DOS
    lcAcLen    = ALLTRIM(STR(lnAcsSegSz))   && Var to hold lenth of Acc. code.
  CASE _WINDOWS
    lcAcLen    = ALLTRIM(STR(lnAcsSegSz+2)) && Var to hold lenth of Acc. code.
ENDCASE

DECLARE       laConType[9,1] , laDescrip[9,2] , laDescTyp[9,2] , laTypes[9,1]

*B605999,1 ASH 06/03/2002 (Begin) Define array to be used in the popup that will be added in the browse.
SELECT GLACCHAR
DECLARE laTags[3,2]
laTags[1,1] = lcAcSegDes
laTags[1,2] = TAG(3)      
laTags[2,1] = 'Long Desc.'
laTags[2,2] = TAG(6)      
laTags[3,1] = 'Short Desc.'
laTags[3,2] = TAG(7)      
lcOldOrd = ORDER()
*B605999,1 ASH 06/03/2002 (End)

STORE ""   TO lcBrowFltr , lcTypeFltr
STORE 0    TO lnOldRow,lnOldtype,lnBrRec,lnOldSec,lnTypRec

lcTitleBro   = "Chart of accounts"         && Title for the account browse
lcTitleTyp   = "Accounts' Types"           && Title for Type browse

llFirstent   = .T.      && Flage to enter the browse window only at the first time
llTypFirst   = .T.      && Flage to enter the TYPE browse window only at the first time
llActQuit    = .F.
llTypQuit    = .F.

puTypes      = 1

laTypes[1,1] = "Asset"
laTypes[2,1] = "Cost of sales"
laTypes[3,1] = "Equity"
laTypes[4,1] = "Other Income"
laTypes[5,1] = "Liability"
laTypes[6,1] = "Sales"
laTypes[7,1] = "Taxes"
laTypes[8,1] = "Expenses"
laTypes[9,1] = "Statistical"

=lfArryPop()   && Build types array for the popup in the type screen ..


*** The concept of haveing a dinamic filtered browse is to have in the 
*** right hand side of the for scope (variables) which can be dinamicaly 
*** changed and with show the brows refresh we can update the for scope
*** of the browse to display the new records
*** If we want to exclude one of them from the filter give it null value ""
lcTypeCode  = ""
lcStandard = ALLTRIM(SUBSTR(" YN",AT(lcAcctype1,"ATS"),1))
lcSegActiv = ALLTRIM(SUBSTR(" AI",AT(lcAccStat1,"ACI"),1))
lcSegAlPos = ALLTRIM(SUBSTR(" YN",AT(lcAccPost1,"ALN"),1))
*B602858,1 AMM Make sure that Set Exact is off 
*lcBrowFltr = "CTYPECODE+CSEGACTIV+CSEGALPOS='' AND CTYPECODE=(lcTypeCode) AND CSTANDARD=(lcStandard)  AND CSEGACTIV=(lcSegActiv) AND  CSEGALPOS=(lcSegAlPos)"
PRIVATE lcSetExF
lcSetExF = SET('EXACT')
SET EXACT OFF
lcBrowFltr = "CTYPECODE+CSEGACTIV+CSEGALPOS=(lcTypeCode) AND CSTANDARD=(lcStandard)  AND CSEGACTIV=(lcSegActiv) AND  CSEGALPOS=(lcSegAlPos)"
*B602858,1 AMM end
****
lcTyp2Brow = ""
lcTypeFltr = "CTYPECODE=(lcTyp2Brow) AND CSTANDARD=(lcStandard)"


SELECT GLACCHAR
COUNT FOR &lcBrowFltr
GO TOP

IF _TALLY = 0
  =gfModalGen("INM02051B00000","DIALOG",lcTitleBro)
  llCallRet  = .F.
  lcAccDes1  = SPACE(65)
  STORE ""   TO lcTypCode1 , lcTypDesc1
  RETURN 
ENDIF

PUSH KEY
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1
ON KEY LABEL TAB        DO lfTabTrpAc
ON KEY LABEL BACKTAB    DO lfTabTrpAc

*E300683,5 Call *.SPR from screens directory
* DO GLACBROW.SPR 
DO (gcScrDir + gcWinAppl + '\GLACBROW.SPR')
*E300683,5 end   
ON KEY 
POP KEY
*B602858,1 AMM Set exact to its old value
SET EXACT &lcSetExF
*B602858,1 AMM end


*!**************************************************************************
*!
*!      Function: lfActBr
*!
*!**************************************************************************
*
FUNCTION lfActBr

lnBrRec=RECNO()
DO CASE
  CASE _DOS
	BROWSE FIELDS dummi=IIF(lnBrRec=RECNO(),'>',' '):H='':P="@K",cacctcode:R:H=lcAcSegDes:&lcAcLen,caccnldes:R:H='Description';
	       NOAPPEND;
	       NODELETE; 
	       NOCLEAR;
	       NOMENU;
	       LOCK 0;
	       NOEDIT;
	       FREEZE dummi;
	       NOWAIT;
	       SAVE;
	       WHEN lfwActBr() ;
	       VALID :F lfvActBr() ;         
	       TITLE lcTitleBro;
	       COLOR SCHEME 13 ;
	       WINDOW GLACBRO3 IN WINDOW AWDGLACBRO;
	       FOR &lcBrowFltr
  CASE _WINDOWS
    *B605999,1 ASH 06/03/2002 (Begin) Add the short desc. to the browse fields.
	BROWSE FIELDS dummi=IIF(lnBrRec=RECNO(),'>',' '):H='':P="@K",cacctcode:R:H=lcAcSegDes:&lcAcLen,caccnldes:R:H='Long Description',caccnsdes:R:H='Short Description';
	       NOAPPEND;
	       NODELETE; 
	       NOCLEAR;
	       NOMENU;
	       LOCK 0;
	       NOEDIT;
	       FREEZE dummi;
	       NOWAIT;
	       SAVE;
	       WHEN lfwActBr() ;
	       VALID :F lfvActBr() ;         
	       TITLE lcTitleBro;
	       WINDOW GLACBRO3 IN WINDOW AWDGLACBRO;
	       FOR &lcBrowFltr
 
    ** To Enable the soft seek.
    lnRecNO = IIF(SEEK(ALLT(&lcObj_Nam),'GLACCHAR'),RECNO(),RECNO(0))
    IF  lnRecNO > 0 
      GO lnRecNO 
    ELSE
      GO TOP
    ENDIF
    *B605999,1 ASH 06/03/2002 (End)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfwActBr
*!
*!**************************************************************************
*
FUNCTION lfwActBr

lnBrRec=RECNO()
SHOW WINDOW (lcTitleBro) REFRESH

*!**************************************************************************
*!
*!      Function: lfvActBr
*!
*!**************************************************************************
*
FUNCTION lfvActBr

IF !WONTOP(lcTitleBro)
  ON KEY LABEL ENTER  
  ON KEY LABEL ESC    
  ON KEY LABEL LEFTMOUSE 
  *B605999,1 ASH 05/20/2002 (Begin) Remove the trapping key.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
  *B605999,1 ASH 05/20/2002 (End)
ENDIF  


*!**************************************************************************
*!
*!      Function: lfwSelAcn
*!
*!**************************************************************************
*
FUNCTION lfwSelAcn

IF llFirstent
  llFirstent = .F.
  DEFINE BAR 100 OF P01PU01  PROMPT "BROWSE" KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01  ACTIVATE WINDOW (lcTitleBro)
  KEYBOARD "{ALT+B}" CLEAR
  RETURN .F.
ENDIF


*!**************************************************************************
*!
*!      Function: lfvSelAcn
*!
*!**************************************************************************
*** Valid function for the select button in the account screen ***
FUNCTION lfvSelAcn

ACTIVATE WINDOW GLACBRO1

IF FSIZE(lcObj_Nam) > 0
  REPLACE &lcObj_Nam WITH GLACCHAR.cacctcode
ELSE
  &lcObj_Nam  = GLACCHAR.cacctcode
ENDIF  

IF !EMPTY(&lcObj_Nam)
  llFoundAcn = .T.
  lcAccDes1  = GLACCHAR.caccnldes
  lcTypCode1 = GLACCHAR.ctypecode
  lcTypDesc1 = LOOKUP(GLTYPES.cTypedesc,lcTypCode1,GLTYPES.cTypecode,'Typecode')
ELSE
  llCallRet = .F.
  lcAccDes1  = SPACE(65)
  lcTypCode1 = ""
  lcTypDesc1 = ""
ENDIF
*B605999,1 ASH 06/03/2002 (Begin) Return the original index.
SET ORDER TO &lcOldOrd IN GLACCHAR
*B605999,1 ASH 06/03/2002 (End)
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvBrCanc
*!
*!**************************************************************************
*** Valid function for the cancel button in the account screen ***
FUNCTION lfvBrCanc

ACTIVATE WINDOW GLACBRO1
llActQuit  = .T.
llCallRet  = .F.
lcAccDes1  = SPACE(65)
lcTypCode1 = ""
lcTypDesc1 = ""

CLEAR READ


*!**************************************************************************
*!
*!      Function: lfvAcnTyp
*!
*!**************************************************************************
*
FUNCTION lfvAcnTyp

IF rbSelTyp = lnOldtype .AND. rbSelTyp = 1
  RETURN
ENDIF

*** Valid function for the radio button ***
IF rbSelTyp = 1
  lcTypeCode  = ""
ELSE

  PUSH KEY

  ON KEY LABEL TAB        DO lfTabTrpTy
  ON KEY LABEL BACKTAB    DO lfTabTrpTy

  llTypFirst   = .T.
  llTypQuit    = .F.  
  *E300683,5 Call *.SPR from screens directory
  * DO GLACTYPE.SPR                  && Browse the accounts types
  DO (gcScrDir + gcWinAppl + '\GLACTYPE.SPR')
  *E300683,5 end   
  POP KEY 
ENDIF

SELECT GLACCHAR
LOCATE
SHOW WINDOW (lcTitleBro) REFRESH

DEFINE BAR 100 OF P01PU01  PROMPT "BROWSE" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01  ACTIVATE WINDOW (lcTitleBro)
KEYBOARD "{ALT+B}" CLEAR

*!**************************************************************************
*!
*!      PROCEDURE: lpMcheck
*!
*!**************************************************************************
*
PROCEDURE lpMcheck

IF MROW(lcTitleBro) > 2 .AND. ;
   MROW(lcTitleBro) < (WROWS(lcTitleBro)-1) .AND. ;
   MCOL(lcTitleBro) > 0 .AND. ;
   MCOL(lcTitleBro) < (WCOLS(lcTitleBro)-1) 
    
  IF lnOldRow = MROW(lcTitleBro) .AND. (lnOldSec+_DBLCLICK >= SECONDS())
    =lfvSelAcn()
  ELSE  
    lnOldRow = MROW(lcTitleBro)
    lnOldSec = SECONDS()
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfTabTrpAc
*!
*!**************************************************************************
*** Trapping the tab key for the account screen ***
FUNCTION lfTabTrpAc

DO CASE
  CASE LASTKEY() = 9
    IF WONTOP() = lcTitleBro
      ACTIVATE WINDOW GLACBRO2
      _CUROBJ = OBJNUM(pbSelAcn)
    ELSE
      IF _CUROBJ = 4
        ACTIVATE WINDOW (lcTitleBro)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF

  CASE LASTKEY() = 15
    IF WONTOP() = lcTitleBro
      ACTIVATE WINDOW GLACBRO2
      _CUROBJ = OBJNUM(rbSelTyp)
    ELSE
      IF _CUROBJ = OBJNUM(pbSelAcn)
        ACTIVATE WINDOW (lcTitleBro)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF
ENDCASE


*!**************************************************************************
*!
*!      Function: lfActDeact
*!
*!**************************************************************************
FUNCTION lfActDeact

IF WONTOP(lcTitleBro)
  ON KEY LABEL ENTER      DO lfvSelAcn
  ON KEY LABEL ESC        DO lfvBrCanc
  ON KEY LABEL LEFTMOUSE  DO lpMcheck
  *B605999,1 ASH 05/20/2002 (Begin) Add inceremntal search in the account browse.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
  ENDFOR
  *B605999,1 ASH 05/20/2002 (End)
  RELEASE BAR 100 OF P01PU01
ENDIF

RETURN llActQuit

****************************************************************************
****************************************************************************

*!**************************************************************************
*!
*!      Function: lfTypBro
*!
*!**************************************************************************
*
FUNCTION lfTypBro

lnWid1 = STR(MAX(LEN('Lower Limit'),lnFrsSegSz))
lnWid2 = STR(MAX(LEN('Upper Limit'),lnFrsSegSz))

SELECT GLTYPES
lnTypRec=RECNO()

DO CASE
  CASE _DOS
	BROWSE FIELDS dummi2=IIF(lnTypRec=RECNO(),'>',' '):H='':P="@K",ctypecode:R:H='Type',ctypedesc:R:H='Type  Description':29,ctyplacno:R:H='Lower Limit':&lnWid1,ctypuacno:R:H='Upper Limit':&lnWid2;
	       NOAPPEND;
	       NODELETE;
	       NOMENU;
	       LOCK 0;
	       FREEZE dummi2;
	       NOEDIT;
	       SAVE;
	       NOWAIT;
	       WHEN lfwTypBro();
	       VALID :F lfvTypBro();
	       TITLE lcTitleTyp;
	       COLOR SCHEME 13  ;
	       WINDOW GLACTYP2 IN WINDOW AWDGLACTYP;
	       FOR &lcTypeFltr
  CASE _WINDOWS
	BROWSE FIELDS dummi2=IIF(lnTypRec=RECNO(),'>',' '):H='':P="@K",ctypecode:R:H='Type',ctypedesc:R:H='Type  Description':29,ctyplacno:R:H='Lower Limit':&lnWid1,ctypuacno:R:H='Upper Limit':&lnWid2;
	       NOAPPEND;
	       NODELETE;
	       NOMENU;
	       LOCK 0;
	       FREEZE dummi2;
	       NOEDIT;
	       SAVE;
	       NOWAIT;
	       WHEN lfwTypBro();
	       VALID :F lfvTypBro();
	       TITLE lcTitleTyp;
	       WINDOW GLACTYP2 IN WINDOW AWDGLACTYP;
	       FOR &lcTypeFltr

ENDCASE
*!**************************************************************************
*!
*!      Function: lfwTypBro
*!
*!**************************************************************************
FUNCTION lfwTypBro

lnTypRec=RECNO()
SHOW WINDOW (lcTitleTyp) REFRESH SAME 

*!**************************************************************************
*!
*!      Function: lfvTypBro
*!
*!**************************************************************************
FUNCTION lfvTypBro

IF !WONTOP(lcTitleTyp)
  ON KEY LABEL ENTER  
  ON KEY LABEL ESC    
  ON KEY LABEL LEFTMOUSE 
  *B605999,1 ASH 05/20/2002 (Begin) Remove the ket trapping.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
  *B605999,1 ASH 05/20/2002 (End)
ENDIF  


*!**************************************************************************
*!
*!      Function: lfwSelTyp
*!
*!**************************************************************************

FUNCTION lfwSelTyp

IF llTypFirst
  llTypFirst = .F.
  DEFINE BAR 100 OF P01PU01  PROMPT "BROWSE" KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01  ACTIVATE WINDOW (lcTitleTyp)
  KEYBOARD "{ALT+B}" CLEAR
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSelTyp
*!
*!**************************************************************************
*** Valid function for the select button in the type screen ****
FUNCTION lfvSelTyp

ACTIVATE WINDOW glactyp1

IF RIGHT(GLTYPES.ctypecode,2) ="00"
  lcTypeCode = LEFT(GLTYPES.ctypecode,1)
ELSE 
  lcTypeCode = GLTYPES.ctypecode
ENDIF  

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvTyCancel
*!
*!**************************************************************************
*
FUNCTION lfvTyCancel

ACTIVATE WINDOW glactyp1
llTypQuit    = .T.
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvTypes
*!
*!**************************************************************************
*
FUNCTION lfvTypes

lcTyp2Brow = SUBSTR('ACQILSTEY',puTypes-1,1)

LOCATE
SHOW WINDOW (lcTitleTyp) REFRESH

DEFINE BAR 100 OF P01PU01  PROMPT "BROWSE" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01  ACTIVATE WINDOW (lcTitleTyp)
KEYBOARD "{ALT+B}" CLEAR

*!**************************************************************************
*!
*!      PROCEDURE: lpTMcheck
*!
*!**************************************************************************
*
PROCEDURE lpTMcheck

IF MROW(lcTitleTyp) > 2 .AND. ;
   MROW(lcTitleTyp) < (WROWS(lcTitleTyp)-1) .AND. ;
   MCOL(lcTitleTyp) > 0 .AND. ;
   MCOL(lcTitleTyp) < (WCOLS(lcTitleTyp)-1) 

  IF lnOldRow = MROW(lcTitleTyp) .AND. (lnOldSec+_DBLCLICK >= SECONDS())
    =lfvSelTyp()
  ELSE  
    lnOldRow = MROW(lcTitleTyp)
    lnOldSec = SECONDS()
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfTabTrpTy
*!
*!**************************************************************************
*** Trapping the tab key for the Type screen ***
FUNCTION lfTabTrpTy

DO CASE
  CASE LASTKEY() = 9
    IF WONTOP() = lcTitleTyp
      ACTIVATE WINDOW GLACTYP1
      _CUROBJ = OBJNUM(puTypes)
    ELSE
      IF _CUROBJ = OBJNUM(pbTyCancel)
        ACTIVATE WINDOW (lcTitleTyp)
      ELSE
       _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF
  CASE LASTKEY() = 15
    IF WONTOP() = lcTitleTyp
      ACTIVATE WINDOW GLACTYP1
      _CUROBJ = OBJNUM(puTypes)
    ELSE
      IF _CUROBJ = OBJNUM(pbTyCancel)
        ACTIVATE WINDOW (lcTitleTyp)
      ELSE
       _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF
ENDCASE


*!**************************************************************************
*!
*!      Function: lfTypDeact
*!
*!**************************************************************************
FUNCTION lfTypDeact

IF WONTOP(lcTitleTyp)
  ON KEY LABEL ENTER      DO lfvSelTyp
  ON KEY LABEL ESC        DO lfvTyCancel
  ON KEY LABEL LEFTMOUSE  DO lpTMcheck
  RELEASE BAR 100 OF P01PU01
ENDIF

RETURN llTypQuit

*!**************************************************************************
*!
*!      Function: lfArryPop
*!
*!**************************************************************************
*
FUNCTION lfArryPop

*** Build the popup array in the types screen ***
*** which display all the types if you want to ***
*** add new account ...

SELECT cTypecode,cTypedesc FROM &gcDataDir.GLTYPES;
       WHERE SUBSTR(cTypeCode,2,2) = '00';
       INTO ARRAY laDescrip

DIMENSION laDescrip[9,2],laDescTyp[9,2]
=ACOPY(laDescrip,laDescTyp)
=gfADel(@laDescTyp,2,2)

laConType = ' '

FOR lnCount = 1 TO 9
  laConType [lnCount] = laTypes[lnCount] + ;
  SPACE(19-LEN(ALLTRIM(laTypes[lnCount]))) + '   ' + ;
  IIF(ASCAN(laDescTyp,SUBSTR('ACQILSTEY',lnCount,1))>0 , ;
  laDescrip[ASCAN(laDescTyp,SUBSTR('ACQILSTEY',lnCount,1)),2] , ' ')
ENDFOR
DIMENSION laConType[10,1]
=AINS(laConType,1,1)
laConType [1,1] = "             All Types              "

DO CASE
  CASE lcAccType1 = 'T'
    =ADEL(laConType,10)
    DIMENSION laConType[9,1]
    SHOW GET puTypes ENABLE
  CASE lcAccType1 = 'S'
   laConType[1,1] = laConType[10,1]
   DIMENSION laConType[1,1]
   SHOW GET puTypes DISABLE
ENDCASE




*!*************************************************************
*! Name      : lfIncSearch
*! Developer : Ashraf Sherif
*! Date      : 05/20/2002
*! Purpose   : Incremental search from the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIncSearch()
*!*************************************************************
*B605999,1 ASH 05/20/2002 Trap the incremental search in the browse.
*!*************************************************************
*
FUNCTION lfIncSearch
PRIVATE lnBrRecNO , lcOrgTag , lcExpToSeek

PUSH KEY CLEAR
ON KEY
ON KEY LABEL F1 llDumi = .T.
CLEAR TYPEAHEAD

*-- Charcter to be searched.
lcExpToSeek = UPPER(CHR(LASTKEY()))

IF INLIST(LASTKEY(),27,13,9,15)
  CLEAR TYPEAHEAD
  POP KEY
  RETURN
ENDIF

*-- Define the window of the incremental search function.
IF _DOS .OR. _UNIX
  DEFINE WINDOW lwIncSrch ;
    FROM 20, 5 ;
      TO 22,74 ;
   FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR SCHEME 5
  ACTIVATE WINDOW lwIncSrch NOSHOW
ELSE
  DEFINE WINDOW lwIncSrch ;
    AT 21.000, 10.000 ;
    SIZE 4.167,55.444 ;
    FONT "FoxFont", 9 ;
    STYLE "B" TITLE "Aria Apparel System" ;
    FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)
  ACTIVATE WINDOW lwIncSrch NOSHOW
ENDIF

PUSH KEY CLEAR
CLEAR TYPEAHEAD
KEYBOARD "{END}"
*-- Call function to activate the search screen.
DO lfActInSr
POP KEY
*-- Refresh the account browse.
=lfwActBr()

*!*************************************************************
*! Name      : lfActInSr
*! Developer : Ashraf Sherif
*! Date      : 05/20/2002
*! Purpose   : the second part of the incremental search function
*!           : to get the value of searching.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActInSr()
*!*************************************************************
*B605999,1 ASH 05/20/2002 Trap the incremental search in the browse.
*!*************************************************************
*
FUNCTION lfActInSr

  @ 1.500,14.333 GET lcExpToSeek ;
    SIZE 1.167,41.000 PICTURE '@! XXXXXXXXXXXXXXXXXXX' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"

  @ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
    STYLE "B"
	
  @ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
*ENDIF

ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch

*-- Expression to be searched.
lcExpToSeek = ALLTRIM(lcExpToSeek)
*-- If press enter, search for the typed expression.
IF LASTKEY() = 13
    *-- Save the record pointer of the GLACCHAR file.
    lnOrdRecNO = IIF(RECNO("GLACCHAR") > RECCOUNT("GLACCHAR") , 0 , RECNO("GLACCHAR"))
    IF SEEK(lcExpToSeek)
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      *-- restore the record pointer if the typed expression was not found.
      IF lnOrdRecNO > 0 .AND. lnOrdRecNO <= RECCOUNT("GLACCHAR")
        GO lnOrdRecNO IN GLACCHAR
      ENDIF
    ENDIF
  *--First Folder 
ELSE
  lcExpToSeek = ''
ENDIF

ON KEY
CLEAR TYPEAHEAD
POP KEY
RETURN

*!*************************************************************
*! Name      : lfvTags
*! Developer : Ashraf Sherif
*! Date      : 05/20/2002
*! Purpose   : Valid function for the new 'sort by ' popup
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActInSr()
*!*************************************************************
*B605999,1 ASH 05/20/2002 
*!*************************************************************
FUNCTION lfvTags
SELECT GLACCHAR
SET ORDER TO laTags[putags,2]
=lfwActBr()