* Documetation :
*C201363,1 RAS 7/10/2011 Get payment terms code code from BRN 850 raw file [T20110608.0001]
*:****************************************************************************************

*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed [WLD]
*: Date      : 06/16/2009
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*C201162,1 WLD 6/16/2009 New modifications on the Wildcat EDI 850 mappings -Ship Via\Season code -T20090610.0017.
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.
******************************************************************************************
*: Name      : lfGetShipVia
*: Developer : WLD
*: Date      : 06/16/2009
*: Purpose   : Get Ship Via code from WLD 850 raw file -T20090610.0017.
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C201162,1 WLD 6/16/2009 New modifications on the Wildcat EDI 850 mappings
*E302885,1 RAS 4/10/2011 make brandwise make the same behaviour as WildCat
FUNCTION lfGETSHIPVIA

  IF MMAPSET = 'WLD' OR MMAPSET = 'BRN'
    lcShipVia = IIF(!EMPTY(MShipVia),ALLTRIM(MShipVia),lcShipVia)
  ENDIF

ENDFUNC
******************************************************************************************
*: Name      : lfGetSeason
*: Developer : WLD
*: Date      : 06/16/2009
*: Purpose   : Get Season code from WLD 850 raw file - T20090610.0017.
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C201162,1 WLD 6/16/2009 New modifications on the Wildcat EDI 850 mappings
*E302885,1 RAS 4/10/2011 get season from 850 not style season
FUNCTION lfGETSEASON

  IF MMAPSET = 'WLD' OR MMAPSET = 'BRN'
    xSeason   = xSeason
  ELSE
    xSeason   = STYLE.Season
  ENDIF

ENDFUNC
******************************************************************************************
*: Name      : lfGETTERM
*: Developer : RAS
*: Date      : 07/10/2011
*: Purpose   : Get payment terms code code from BRN 850 raw file - T20110608.0001.
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C201363,1 RAS 7/10/2011 Get payment terms code code from BRN 850 raw file

 FUNCTION lfGETTERM
  IF MMAPSET = 'WLD' OR MMAPSET = 'BRN'
  lcTmpAlias= ALIAS()
    IF !EMPTY(Mterms)
      select codes
      Locate for cfld_name='CTERMCODE' and crltfield='N' and UPPER(ALLT(cdiscrep))=UPPER(ALLT(Mterms))
      IF FOUND()
        select ORDHDR
        REPLACE cTermCode WITH codes.ccode_no IN ORDHDR
        SELECT (lcTmpAlias)
        Return .T.
      ELSE
        SELECT (lcTmpAlias)
        Return .F.
      ENDIF
    ENDIF
  ELSE
    Return .F.
  ENDIF
ENDFUNC
