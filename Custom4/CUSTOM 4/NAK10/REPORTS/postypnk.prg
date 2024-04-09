*:***************************************************************************
*: Program file  : POSTYPNK
*: Program desc. : Custom PO for Naeem Khan
*: For Report    : POSTYPNK.FRX
*: System        : Aria Advantage Series ARIA4XP
*: Module        : Purchase Order (PO)
*: Developer     : Ahmed Khalifa (AKM)
*: Date          : 08/14/2007
*: C200737,C200738 T20070322.0005          
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:
*:***************************************************************************
*: Passed Parameters  : None
*: Requires RichTX32.OCX to be installed on the system, if not installed please install it.
*:***************************************************************************
*: Modifications:
*: B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[T20070322.0005]
*: B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[T20070322.0005]
*: B611152,1 MMT 06/13/2016 Custom PO form NK prints PO notes without CRLF[T20160531.0003]
*:***************************************************************************


*!* Open Object, ObjLInk files uising gfOPenFile
*!* seek in it for tis style, ]


loogScroll.cCrOrientation = 'L'
=gfOpenTable('ITEM','STYLE','SH')
=gfOpenTable('NOTEPAD','NOTEPAD','SH')

lcCurSession = SELECT()
tSafety=SET ("SAFETY")
SET SAFETY OFF
* CREATE CURSOR NotePads (PO Character(6), gColorText general)
DIMENSION laNotePadcolor[3,4]
laNotePadcolor[1,1]='PO'
laNotePadcolor[1,2]='C'
laNotePadcolor[1,3]= 6
laNotePadcolor[1,4]= 0

laNotePadcolor[2,1]='gColorText'
laNotePadcolor[2,2]='G'
laNotePadcolor[2,3]= 10
laNotePadcolor[2,4]= 0

laNotePadcolor[3,1]='POSeason'
laNotePadcolor[3,2]='C'
laNotePadcolor[3,3]= 6
laNotePadcolor[3,4]= 0
=gfCrtTmp(lcNotePadColor , @laNotePadcolor ,'PO','PO',.T.)


SELECT POsHdr
SET SKIP TO 

SET RELATION TO PO INTO (lcNotePadColor ) ADDITIVE
SCAN
    =lfNoteColorize()
ENDSCAN

SET SAFETY &tSafety

*!* ****************************************************************************************
*!* Function: lfGetPONote
*!* 					Colorize notes for current PO
*!* Parameters:
*!*
*!*	Developer: Ahmed Khalifa Mohamed (AKM) 08/21/2007
*!* ****************************************************************************************
FUNCTION lfNoteColorize
    LOCAL lcExtractedTXT , lcNote, lnOccurs, lnCount, lcStartDelm, lcEndDelm
    &&Create a Richtext OLE bound control in the general field of the dColor table

    _rtfFile = ADDBS(oAriaApplication.WorkDir)+loogScroll.gfTempName()+POsHdr.PO+".rtf"
    SET TEXTMERGE TO &_rtfFile NOSHOW
    SET TEXTMERGE ON

    SELECT (lcNotePadColor)
    APPEND BLANK
    REPLACE PO WITH POsHdr.PO &&, POSeason WITH lfGetsEASON( lcStyle ) &&poshdr.po)
    lcExtractedTXT=''
    lcNote= lfGetPONote(POsHdr.PO)
    *: B611152,1 MMT 06/13/2016 Custom PO form NK prints PO notes without CRLF[T20160531.0003][Start]
    lcNote = STRTRAN(lcNote ,CHR(13),"\"+CHR(13))
    *: B611152,1 MMT 06/13/2016 Custom PO form NK prints PO notes without CRLF[T20160531.0003][End]
    lnOccurs=OCCURS('++',lcNote)
    lnCount=1
    lcEndDelm='++'
    lcExtractedTXT='{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fswiss\fcharset0 Arial;}}{\colortbl ;\red255\green0\blue0;}{\*\generator Msftedit 5.41.15.1507;}\viewkind4\uc1\pard\'

    IF(LEFT(lcNote,2)='++')
        lcStartDelm='++'
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = lcExtractedTXT +'cf1\b\f0\fs20'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = lcExtractedTXT +'cf1\b\f0\fs24'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
        lcExtractedTXT = lcExtractedTXT +'cf1\b\f0\fs28'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
        lnCount = lnCount + 1
    ELSE
        lcStartDelm=''
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs20'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs24'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs28'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
    ENDIF


    lcStartDelm='++'
    DO WHILE lnCount < lnOccurs
        IF (MOD(lnCount,  2) = 1)
	        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
	        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'cf1\b\f0\fs20'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
	        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
	        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'cf1\b\f0\fs24'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
            lcExtractedTXT = ADDBS(lcExtractedTXT) +'cf1\b\f0\fs28'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+'\cf0\b0'
            *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
            *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
        ELSE
	        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
	        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs20'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
	        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
	        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs24'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
            lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs28'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
            *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
            *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
        ENDIF
        lnCount = lnCount + 1
    ENDDO


    IF lnOccurs = 0
	  *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
	  * lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs20'+lcNote+''
	  *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
	  *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs24'+lcNote+''
      lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs28'+lcNote+''
      *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
      *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
    ELSE
      IF(!RIGHT(lcNote,2)='++')
        lcEndDelm=''
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs20'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[Start]
        *lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs24'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        lcExtractedTXT = ADDBS(lcExtractedTXT) +'f0\fs28'+(lfGetSubStr(lcNote, lcStartDelm, lcEndDelm, lnCount))+''
        *B608308,2 MMT 10/24/2007 Fix bug of Small font of notes printed in Layout[End]
        *B608308,1 MMT 10/08/2007 Fix bug of Small font of notes printed in Layout[End]
      ENDIF
    ENDIF  




    lcExtractedTXT = lcExtractedTXT + 	'\par}'

    \\<<(lcExtractedTXT)>>

    SET TEXTMERGE TO
    APPEND GENERAL gColorText FROM &_rtfFile CLASS "RICHTEXT.RICHTEXTCTRL.1"

    ERASE &_rtfFile
ENDFUNC

*!* ****************************************************************************************
*!* Function: lfGetSubStr
*!* 					Gets a substring from passed string based on a start and end delimeter
*!* Parameters:
*!* 		lcStr 		 String		, Source string
*!* 	  lcStartDel String   , Starting delimeter
*!*   	lcEndDel 	 String 	, the ending delimeter
*!*  		lnOccur 	 Integer  , The occurence no.
*!*
*!*	Developer: Ahmed Khalifa Mohamed (AKM) 08/21/2007
*!* ****************************************************************************************
FUNCTION lfGetSubStr(_lcStr AS STRING, _lcStartDel AS STRING , _lcEndDel AS STRING , _lnOccur AS INTEGER ) AS STRING
    LOCAL lcSubStr

    lcSubStr=STREXTRACT(_lcStr , _lcStartDel  ,_lcEndDel ,_lnOccur )

    RETURN lcSubStr
ENDFUNC

*!* ****************************************************************************************
*!* Function: lfGetPONote
*!* 					Gets a note for current PO
*!* Parameters:
*!*			_PONUM: PO num.
*!*
*!*	Developer: Ahmed Khalifa Mohamed (AKM) 08/21/2007
*!* ****************************************************************************************
FUNCTION lfGetPONote
    LPARAMETERS _PONUM
    =GFSEEK(('P' + _PONUM),'NOTEPAD','NOTEPAD',.T.)

    RETURN NOTEPAD.MNOTES
ENDFUNC

*!* ****************************************************************************************
*!* Function: lfGetFabricDesc
*!* 					returns fabric description
*!* Parameters:
*!*					_Fabric: Fabric code
*!*
*!*	Developer: Ahmed Khalifa Mohamed (AKM) 08/21/2007
*!* ****************************************************************************************
FUNCTION lfGetFabricDesc
    LPARAMETERS _Fabric
    =GFSEEK(('0002' + _Fabric),'ITEM','STYLE',.T.)
    RETURN ITEM.DESC
ENDFUNC

*!* ****************************************************************************************
*!* Function: lfGetsEASON
*!* 					returns style season
*!* Parameters:
*!*					_Fabric: Fabric code
*!*
*!*	Developer: Ahmed Khalifa Mohamed (AKM) 08/21/2007
*!* ****************************************************************************************
FUNCTION lfGetsEASON
    LPARAMETERS _Style 
    LOCAL lcSeason , lcSelect
    lcSelect=SELECT()
    =GFSEEK(_Style ,'STYLE','STYLE',.T.)
    lcSeason =STYLE.season
    SELECT (lcSelect)
    RETURN  gfCodDes(lcSeason , 'SEASON')
ENDFUNC