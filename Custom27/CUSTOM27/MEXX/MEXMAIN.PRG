*!**************************************************************************
*! Name      : MEXMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Consolidates Casual Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200253,1
*!**************************************************************************
*!B605489,1 ADEL 02/05/02 Fix 'gfDispRe.prg not found'
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.


*!**************************************************************************
*! Name      : lfGETCONT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Get contactor
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200253

FUNCTION lfGETCONT

PRIVATE lnAlias
lnAlias = SELECT()
STORE "" TO lcMFCont,lcMFContN
IF !USED('ApVendor')  
  =gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
ENDIF

IF !USED('Mfgoprhd')
  =gfOpenFile(gcDataDir+'Mfgoprhd' , 'Mfgoprhd' , 'SH')
ENDIF

SELECT Mfgoprhd
IF SEEK('T'+laData[1])
  lcMFCont = cContCode
  IF !lInHouse
    IF SEEK(lcMFCont,'ApVendor')
      lcMFContN= ApVendor.cVenComp
    ENDIF  
  ELSE
    DIMENSION laMfgRFld[2,2]
    STORE "" TO lcContName,lcContCode
    laMfgRFld[1,1] = 'CCONTCODE'
    laMfgRFld[1,2] = 'lcContCode'
    laMfgRFld[2,1] = 'CCONTNAME'
    laMfgRFld[2,2] = 'lcContName'
    =gfRltFld(cOprCode,@laMfgRFld,'MFGCODE')
    lcMFCont = ALLTRIM(lcContCode)
    lcMFContN= ALLTRIM(lcContName)
  ENDIF  
  =lfRefresh(lcCTChWin2)
ENDIF
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfGETVOUT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/20/2001
*! Purpose   : Get Voucher No.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!.. C200255
FUNCTION lfGETVOUT

lcVoucNo = ""
llContVout = .T.
*--Open MATINVJL
llMatjOpn = USED('MATINVJL')
IF !llMatjOpn
  =gfOpenFile(gcDataDir+"MatInvJl","Voucherno","SH")
ELSE
  SELECT MatInvJl
  lcOldOrd = ORDER()
  SET ORDER TO TAG Voucherno
ENDIF
*B605489,1 (Begin) Don't change it.
*gcWinAppl = IIF(gcWinAppl = 'MF','MA',gcWinAppl)
*DO (gcScrDir+gcWinAppl+'\MAVOUCNO.SPX')
lcOldApp = IIF(gcWinAppl = 'MF','MA',gcWinAppl)
DO (gcScrDir+lcOldApp+'\MAVOUCNO.SPX')
*B605489,1 (End)

IF !llMatjOpn
  USE IN MATINVJL
ELSE
  SELECT MatInvJl
  lcOldOrd = ORDER()
  SET ORDER TO &lcOldOrd IN MATINVJL
ENDIF

IF !llContVout
  RETURN
ENDIF


*!**************************************************************************
*! Name      : lfvVoOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/20/2001
*! Purpose   : Validate OK button.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!.. C200255
FUNCTION lfvVoOk

IF EMPTY(lcVoucNo)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Voucher No. cannot be empty.')
  _CUROJB = OBJNUM(lcVoucNo)
  RETURN
ELSE
  IF SEEK(lcVoucNo,'MATINVJL')
    lnChoice= gfModalGen('QRM00000B36018',.F.,.F.,.F.,"Voucher#: " + ALLTRIM(lcVoucNo) + " already exists." + ;
                          " Would you like to add to it?")
    DO CASE
      CASE lnChoice = 3
        *--Cancel
        llContVout = .F.
      CASE lnChoice = 2
        *--No
        _CUROJB = OBJNUM(lcVoucNo)
        RETURN
    ENDCASE                       
  ENDIF
ENDIF
CLEAR READ

*!**************************************************************************
*! Name      : lfPrntit
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 27/20/2001
*! Purpose   : Print Issued/Received Qty.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!.. C200256
FUNCTION lfPrntit

*--Get the no of compies to print.
DO lpGetCopies
*--Print it.
LOCATE
IF !USED('WAREHOUS')
  lnAlias = SELECT()
  =gfOpenFile(gcDataDir+'WAREHOUS' , 'WAREHOUS' , 'SH')
  SELECT (lnAlias)
ENDIF
lcWareName = ""
IF SEEK(IIF(!llFromCost,CWARECODE,lcWareCode),'WAREHOUS')
  lcWareName = WAREHOUS.CDESC
ENDIF
lcRPForm     = 'MARECQT'
lcOgPlatform = 'WINDOW'
lcOldDev     = gcDevice
gcDevice     = 'PRINTER'
_PCOPIES     = lnNoOfCop
lcSetCent    = SET("CENT")
SET CENT OFF
lcVenCode    = IIF(TYPE("lcVenCode") = 'U',lfVENCODE(),lcVenCode)
IF llFromCost
  *--Create dummy cursor with 1 rec for the Frx to work
  lnSelect = SELECT()
  SELECT 0
  CREATE CURSOR Tmp (OrdNo c(10))
  APPE BLAN
ENDIF

*B605489,1 (Begin) Call the report in a proper way.
*gcWinAppl = IIF(gcWinAppl = 'MF','MA',gcWinAppl)
*DO gfDispRe WITH EVAL('lcRPForm')
lcOldApp = IIF(gcWinAppl = 'MF','MA',gcWinAppl)

lcCons = SET('CONS')
SET CONS OFF
DO (gcRepHome +  'MFREPORT.APP') WITH 'MFPRNTV', .T.
SET CONS &lcCons
*B605489,1 (End)

IF llFromCost
  USE IN Tmp
  SELECT (lnSelect)
ENDIF
gcDevice     = lcOldDev
SET FILTER TO
IF lcSetCent = 'ON'
  SET CENT ON
ENDIF

*!**************************************************************************
*! Name      : lpGetCopies
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 27/20/2001
*! Purpose   : Get no of copies to print.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!.. C200256
PROCEDUrE lpGetCopies

#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS

m.rborder = SET("READBORDER")
SET READBORDER ON
m.currarea = SELECT()
lcIcon = IIF(FILE("ARIA.ICO"),[ICON FILE "ARIA.ICO"],"")
IF NOT WEXIST("macopies") ;   
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.PJX";
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.SCX";      
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.MNX";      
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.PRG";
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.FRX";
	OR UPPER(WTITLE("MACOPIES")) == "MACOPIES.QPR"      
	lcGrow = ''
	IF _WINDOWS 
	  lcGrow = IIF(RIGHT(OS(1),2)='95','GROW','')
	ENDIF
	DEFINE WINDOW macopies ;
		AT  0.000, 0.000  ;
		SIZE 9.167,19.000 ;
		TITLE "Copies No." ;
		FONT "FoxFont", 9 ;
		FLOAT ;
		NOCLOSE ;
		MINIMIZE ;
		SYSTEM ;
		  COLOR RGB(,,,192,192,192) ;
		&lcIcon ;
		IN WINDOW FNDATION &LCGROW
	MOVE WINDOW macopies CENTER
ENDIF
#REGION 1
#REGION 1
IF WVISIBLE("macopies")
	ACTIVATE WINDOW macopies SAME
    llSayCnt=.F.
ELSE
	ACTIVATE WINDOW macopies NOSHOW
    llSayCnt=.T.   
ENDIF
  @ 0.333,0.500 TO 0.333,18.125 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 8.917,0.000 TO 8.917,18.625 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 0.333,0.500 TO 8.583,0.500 ;
    PEN 1, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 0.000,18.625 TO 8.917,18.625 ;
    PEN 1, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 0.333,18.125 TO 8.583,18.125 ;
    PEN 1, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 0.000,0.000 TO 8.917,0.000 ;
    PEN 1, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 0.000,0.000 TO 0.000,18.625 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 8.583,0.500 TO 8.583,18.125 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 1.917,8.875 TO 1.917,16.000 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 3.167,9.000 TO 3.167,16.000 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 2.000,8.875 TO 3.250,8.875 ;
    PEN 2, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 2.000,15.750 TO 3.250,15.750 ;
    PEN 2, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 2.167,1.875 SAY "Copies   :"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"

  @ 2.083,9.125 GET lnNoOfCop ;
	SIZE 1.000,10.400 ;
	RANGE 1 ;
	DEFAULT 0 ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@B 999"

  @ 4.583,2.000 TO 4.583,16.625 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 4.500,1.875 TO 4.500,16.625 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 5.500,5.125 GET pbOk ;
	  PICTURE "@*HT \<Ok" ;
	  SIZE 1.769,11.667,0.667 ;
	  DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B"

lcWindNoGr = ''
IF NOT WVISIBLE("macopies")
  MODIFY WINDOW  "macopies" NOGROW     
	ACTIVATE WINDOW macopies     
ENDIF     
READ CYCLE ;
	ACTIVATE lfAriaAct() ;
	OBJECT LNCUROBJ COLOR ,&GCREADCLR

  RELEASE WINDOW macopies
SELECT (m.currarea)
#REGION 0
SET READBORDER &rborder
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF

FUNCTION lfAriaAct     && Read Level Activate
IF TYPE('lcWindNoGr') = 'C' AND !EMPTY(lcWindNoGr)
  DO CASE
    CASE WONTOP() $ lcWindNoGr
      MODIFY WINDOW (WONTOP()) NOGROW
      lcWindNoGr = STRTRAN(lcWindNoGr,WONTOP())
    CASE WPARENT(WONTOP()) $ lcWindNoGr
      MODIFY WINDOW (WPARENT(WONTOP())) NOGROW
      lcWindNoGr = STRTRAN(lcWindNoGr,WPARENT(WONTOP()))
  ENDCASE
ENDIF

FUNCTION lfvY2K     && Valid 1900 year
PRIVATE lcY2kCent , ldY2kDate , lcY2kObj , lnY2kIncr , lnY2KNew , lcY2KAlias
lcY2KAlias = SELECT(0)
SELECT 0
IF TYPE(SYS(18)) = 'U'
  SELECT (lcY2KAlias)
ENDIF
IF YEAR(EVALUATE(VARREAD())) <= 1930
  lcY2KCent = SET('CENT')
  SET CENT ON
  lcY2KObj     = SYS(18)
  lnY2KIncr    = YEAR(EVALUATE(VARREAD())) 
  lnY2KNew     = 2000+(lnY2KIncr/100-int(lnY2KIncr/100))*100
  ldY2KDate    = DTOC(EVALUATE(VARREAD()))
  ldY2KDate    = STRTRAN(ldY2KDate , ALLTRIM(STR(YEAR(EVALUATE(VARREAD())))) , ALLTRIM(STR(lnY2KNew)))
  ldY2KDate    = CTOD(ldY2KDate)
  &lcY2KObj    = ldY2KDate
  SET CENT &lcY2KCent
ENDIF
SELECT (lcY2KAlias)


*!**************************************************************************
*! Name      : lfVENCODE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Get contactor
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200256
FUNCTION lfVENCODE

PRIVATE lnAlias
lnAlias = SELECT()
STORE "" TO lcVenCode

IF !USED('Mfgoprhd')
  =gfOpenFile(gcDataDir+'Mfgoprhd' , 'Mfgoprhd' , 'SH')
ENDIF
SELECT Mfgoprhd
IF SEEK(IIF(!llFromCost,'T'+PADL(EVAL(lcTrans),6,"0"),'M'+laData[1]))
  lcVenCode = 'Mfgoprhd.cContCode'
ENDIF
SELECT (lnAlias)
RETURN lcVenCode


*!**************************************************************************
*! Name      : lfRUNNOTE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/07/2002
*! Purpose   : Run custom V.foxpro Arabic notepad
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200259
FUNCTION lfRUNNOTE

lcEditMode = "NOEDIT"
lcRpNotes  = ALLTRIM(&lcToEdit..mEdtNotes)
llRpUpdate = .F.
lcRpDir    = gcScrDir
lcRpMod    = gcWinAppl
SHOW GET (lcToEdit)+'.mEdtNotes' disab
IF !FILE('NOTES.DBF')
  CREATE TABLE NOTES.DBF (NOTES M)
ELSE
  IF !USED('NOTES')
    USE NOTES.DBF IN 0
  ENDIF  
ENDIF
SAVE TO MEMO NOTES.NOTES ALL LIKE l?RP*
RUN /N1 CustNote.exe
IF laScrMode[4]
  =lfUPDNOTE()
ENDIF

*!**************************************************************************
*! Name      : lfUPDNOTE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/07/2002
*! Purpose   : Get notes from V.FOXPRO For Mexx.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200259
FUNCTION lfUPDNOTE

IF FILE('NOTES.DBF') AND (laModes[2] OR laScrMode[4])
  DO WHILE .T.
    USE IN NOTES
    USE NOTES.DBF IN 0
    REST FROM MEMO NOTES.NOTES ADDI
    IF llRpUpdate
      REPLACE &lcToEdit..mEdtNotes WITH lcRpNotes    
      SHOW GET (lcToEdit)+'.mEdtNotes'
      EXIT
    ENDIF
    IF llRpFrmCan
      EXIT
    ENDIF
  ENDDO
ENDIF
SHOW GET (lcToEdit)+'.mEdtNotes' ENAB
llNotesUpd = .T.
llRpFrmCan = .F.
   