*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah
*: Date      : 11/14/2005
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*C129880,1 WLD 11/14/2005 Get PO ShortCut with Order Confirmation - Do Selection of PO ShortCut with Order Confirmation . 
*C200779,1 WLD 04/23/2007 Replace cAddress2 with cAddress1 and cAddress1 with GLN for Account W1000
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*: Name      : lfGTSHCTPO
*: Developer : WLD
*: Date      : 11/14/2005
*: Purpose   : Get PO ShortCut with Order Confirmation .
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C129880,1 WLD 11/14/2005
FUNCTION lfGTSHCTPO
PUBLIC gcINDStrPO,gcINDStatusPO

lnReturn = 8
lcString = "\<Update,\-,\<View,View \<Source,\<Order Confirmation,\-,\<Check errors"
lcStatus = ""

DO CASE
  CASE EMPTY(EDILIBDT.cStatus)
    lcString = "\<Process," + lcString
     lcStatus = "TFFFTFFF"
    
  CASE EDILIBDT.cStatus = 'A'
    
    lcString = "\<Reprocess," + lcString
     lcStatus = "TTFTTTFT"
    
  CASE EDILIBDT.cStatus = 'R'
    lcString = "\<Reprocess," + lcString
     lcStatus = "TFFTTTFT"
    
  OTHERWISE
    lcString = ""
    lnReturn = 0
    
ENDCASE

gcINDStrPO     =   lcString
gcINDStatusPO  =   lcStatus

RETURN lnReturn

*-- End of lfGTSHCTPO.

*:****************************************************************************************
*: Name      : lfDOSELCPO
*: Developer : WLD
*: Date      : 11/14/2005
*: Purpose   : Do Selection of PO ShortCut with Order Confirmation .
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C129880,1 WLD 11/14/2005
FUNCTION lfDOSELCPO
lnChoice = gnChoiceIND
  DO CASE
    CASE lnChoice = 1
      RETURN oObject.DO(.F.,EDILIBDT.cFileCode,"cPartCode+cEdiTrnTyp+cIntChgSeq+cGroupSeq+cTranSeq='"+;
        EDILIBDT.cPartCode+EDILIBDT.cEdiTrnTyp+EDILIBDT.cIntChgSeq+EDILIBDT.cGroupSeq+EDILIBDT.cTranSeq+"'")
    CASE lnChoice = 2
      RETURN oObject.UPDATE(.F.)
    CASE lnChoice = 4
      =oObject.ViewTransaction()
    CASE lnChoice = 5
      =oObject.ViewSource()
    CASE lnChoice = 6
    DO FORM (oAriaApplication.ScreenHome + 'eb\ebOrdCnf') WITH EDILIBDT.cfilecode,;
  																			EDILIBDT.ceditrntyp ,;
																			  EDILIBDT.ceditrnst,;
																			  EDILIBDT.cstatus,;
																			  EDILIBDT.ctranseq																			  
    SELECT EDILIBDT
    CASE lnChoice = 8
      =oObject.CheckErrors()
      =TABLEUPDATE(1 , .F. , 'EDILIBDT')
  ENDCASE
*-- End of lfDOSELCPO.
*:****************************************************************************************
*: Name      : lfUPDADD
*: Developer : WLD
*: Date      : 04/23/2007
*: Purpose   : Get PO ShortCut with Order Confirmation .
******************************************************************************************
*: Called from : Class "EDIPROCESSOR"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C200779,1 WLD 04/23/2007
FUNCTION lfUPDADD
IF edipd.cmapset='WLM' AND !EMPTY(CUSTOMER.cStoreGln)
  REPLACE   cAddress2  WITH EDISTORE.cAddress1 ,;
            cAddress1  WITH 'GLN '+ ALLTRIM(EdiStore.cStoreGln)
            
ENDIF          
*-- End of lfUPDADD.
