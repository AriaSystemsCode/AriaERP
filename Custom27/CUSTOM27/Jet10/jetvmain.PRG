*!**************************************************************************
*! Name      : JETMAIN.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/10/2004
*! Purpose   : JET Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications
*! B121372,1 MHM 02/05/2004 Fix bug of Calculate wrong piece per pack in Modify # of Packs in Sales order Screen
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfISA_Wrap
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/10/2000
*! Purpose   : Wrap ISA line for CIT only.
*!**************************************************************************
*B607502,1 SSE 02/17/2004 Trigger to wrap only ISA segment for CIT if more than 80 Char.
FUNCTION lfISA_Wrap
  *-- MFixed changed to 80 hard coded
  IF lcISASeg = 'ISA' AND lcPartCode = 'CITCIT' AND lcEDITrnTyp = '810'
    DO WHILE LEN(lcSegLine) > 0
      IF LEN(lcSegLine)+oObject.CurrentPos >= 80
        lnWrite =FPUTS(lnISAPoint,lcSegLine,80-oObject.CurrentPos)-2
        oObject.CurrentPos = 0
      ELSE
        *lnWrite =FWRITE(lnISAPoint,lcSegLine)
        *oObject.CurrentPos =lnWrite + oObject.CurrentPos
        =FPUTS(lnISAPoint,lcSegLine)
      ENDIF
      lcSegLine = SUBSTR(lcSegLine,lnWrite+1)
    ENDDO
  ELSE
    =FPUTS(lnISAPoint,lcSegLine)
  ENDIF
  *--End of lfISA_Wrap.
  *!**************************************************************************
  *! Name      : CitClein
  *! Developer : Abdelaziz Awadallah Hassan (AAH)
  *! Date      : 05/10/2005
  *! Purpose   : Custom Client ID for CIT only.
  *!**************************************************************************
FUNCTION lfCitClein
  MCLIENTNO	=	'NOT FOUND # '		&&REF14
  MCLIENTID	=	'NOT FOUND ID'      &&REF23
  DO CASE
    CASE INLIST(OARIAAPPLICATION.ACTIVECOMPANYID,'01','05')
      MCLIENTNO	=	'1723'
      MCLIENTID	=	'1724'
    CASE INLIST(OARIAAPPLICATION.ACTIVECOMPANYID,'03','06')
      MCLIENTNO	=	'1723'
      MCLIENTID	=	'1723'
    CASE INLIST(OARIAAPPLICATION.ACTIVECOMPANYID,'04','08')
      MCLIENTNO	=	'1723'
      MCLIENTID	=	'406X'
  ENDCASE
  RETURN
ENDFUNC
