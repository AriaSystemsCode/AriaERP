*:***************************************************************************
*: Program file  : GLDDATE
*! For Screen    : GLDDATE
*! Date          : 14/02/2000
*: System        : Aria Advantage Series.
*: Module        : GENERAL LEDGER (GL)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : CheckPrd , gfModalGen
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO GLDDATE
*:***************************************************************************
*: This Report Program is due to B#803032
*:***************************************************************************

*-- Variable hold the name of the screen.
lcScNam='GLDDATE'

*-- calling the screen GLDDATE

DO (gcScrDir  + 'GL\GLDDATE.SPR')

*!*************************************************************
*! Name      : lfvOK
*! Developer : BWA
*! Date      : 13/02/2000
*! Purpose   : Validate OK botton
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOK()
*!*************************************************************
FUNCTION lfvOK

PRIVATE lcAlias , ldDatTrn
lcAlias = ALIAS()
ldFirst = {}
ldSecnd = {}

IF !EMPTY(ldDate)
  llVldDt=CheckPrd(ldDate,"_FYEAR","_PRDID",'_TRANTYP')
  
  IF EMPTY(_FYEAR) OR EMPTY(_PRDID) OR !llVldDt
    =gfOpenFile(gcDataDir+'Fishd',gcDataDir+'Compfyear','SH')
    ldDatTrn = IIF(!EMPTY(_TRANDAT),_TRANDAT,DATE())
    LOCATE FOR BETWEEN(ldDatTrn , DFISBGDAT , DFISENDAT)
    ldFirst = DTOC(Fishd.dFisBgdat)
    ldSecnd = DTOC(Fishd.dFisEndat)
    
    * Transaction date must be between ð and ð.
    =gfModalGen('TRM02256B00000','ALERT',ldFirst+'|'+ldSecnd)
    _CUROBJ = OBJNUM(ldDate)
    RETURN
  ENDIF
ELSE
  * You must enter a transaction date before adding an entry.
  =gfModalGen('TRM02015B00000','ALERT','')
  _CUROBJ = OBJNUM(ldDate)
  RETURN
ENDIF
SELECT (lcAlias)
CLEAR READ