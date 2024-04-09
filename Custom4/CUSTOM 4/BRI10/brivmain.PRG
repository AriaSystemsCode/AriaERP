*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200894,1 HIA 11/26/2007 custom the name of the outgoing file for partner FLYING-J 
*C201096,1 HIA 01/19/2009 - T20080711.0012 Custom EDI 850 Receving Program to update ;
*C201096,1                  default SKU Profiles+To add SKU Profile.
*B608906,1 WLD 06/24/2009 Get the PROFVALU file with SKU# incoming in EDI PO
*B609284,1 WLD 06/03/2010 Update the profiles for SKU with the values Account+Header Part+Style\Color [T20100505.0024]
*C201260,1 WLD 08/11/2010 custom the name of the outgoing file for partner Pilot Travel Center 
*B609540,1 WAM 02/28/2011 Profile is linked to order line directly not to design line [T20110216.0050]
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfCHGNAM
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : custom the name of the outgoing file for partner FLYING-J
*:****************************************************************************************
*! Called from : Class "GETOUTGOINGFILE"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*C200894,1 HIA 11/26/2007 custom the name of the outgoing file for partner FLYING-J 
*C201260,1 WLD 08/11/2010 custom the name of the outgoing file for partner Pilot Travel Center
*:****************************************************************************************
FUNCTION lfCHGNAM
  *C201260,1 WLD 08/11/2010 custom the name of the outgoing file for partner Pilot Travel Center [Begin]
  *IF EDIACPRT.CPARTCODE = 'FLNGJI'
  IF INLIST(EDIACPRT.CPARTCODE ,'FLNGJI','PLTTRV')
  *C201260,1 WLD 08/11/2010 custom the name of the outgoing file for partner Pilot Travel Center [End]
    *lcOutFlNam = SUBSTR(ALLTRIM(EdiNet.cOutFile),1,ATC('.',ALLTRIM(EdiNet.cOutFile))-1)+'_OTS'+RIGHT(ALLTRIM(EdiNet.cOutFile),4)
    lcOutFlNam = PADR(JUSTSTEM(ALLTRIM(EdiNet.cOutFile)),4,'N')+ALLTRIM(lcEdiTrnTyp)+'.'+DTOS(DATE())+ALLTRIM(STR(HOUR(DATETIME())))+ALLTRIM(STR(MINUTE(DATETIME())))+ALLTRIM(STR(SEC(DATETIME())))
 ENDIF 
ENDFUN
*:****************************************************************************************

*:****************************************************************************************
*! Name      : lfOpen
*! Developer : Hassan Ali (HIA)
*! Date      : 01/19/2009
*! Purpose   : Update the array used to open db files when update sales order
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO" method "init".
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
Function lfFOpen

nAlen     = Alen(oThis.FilesToOpen,1)
nAlen_Col = Alen(oThis.FilesToOpen,2)
*B609540,1 WAM 02/28/2011 Profile is linked to order line directly not to design line
*Dimension oThis.FilesToOpen(nAlen+3,nAlen_Col)
Dimension oThis.FilesToOpen(nAlen+2,nAlen_Col)
*B609540,1 WAM 02/28/2011 (End)

nAlen = nAlen + 1
oThis.FilesToOpen[nAlen,1] = 'D'
oThis.FilesToOpen[nAlen,2] = 'PROFVALU'
oThis.FilesToOpen[nAlen,3] = 'PROFVALU'
oThis.FilesToOpen[nAlen,5] = 'PROFILE'   && CPRO_TYPE+CKEY+CPRO_CODE
oThis.FilesToOpen[nAlen,7] = 1
&&
nAlen = nAlen + 1
oThis.FilesToOpen[nAlen,1] = 'D'
oThis.FilesToOpen[nAlen,2] = 'PROFVALU'
oThis.FilesToOpen[nAlen,3] = ''
oThis.FilesToOpen[nAlen,4] = 'PROFSKU'
oThis.FilesToOpen[nAlen,5] = 'PROFILE'   && CPRO_TYPE+CKEY+CPRO_CODE
oThis.FilesToOpen[nAlen,7] = 1
&&
*B609540,1 WAM 02/28/2011 Profile is linked to order line directly not to design line
*B609540,1 WAM 02/28/2011 Following lines are commented out
*!*	nAlen = nAlen + 1
*!*	oThis.FilesToOpen[nAlen,1] = 'D'
*!*	oThis.FilesToOpen[nAlen,2] = 'ORDDSGN'
*!*	oThis.FilesToOpen[nAlen,3] = 'ORDDSGN'
*!*	oThis.FilesToOpen[nAlen,5] = 'ORDLINE'   && order+cordline+STR(lineno,6)
*!*	oThis.FilesToOpen[nAlen,7] = 1
*B609540,1 WAM 02/28/2011 (End)

&&OrdLine = order+cordline+STR(lineno,6)
&&ORDDSGN = cdesignid+order+cordline

Endfunc

*:****************************************************************************************
*! Name      : lfOrdLp
*! Developer : Hassan Ali (HIA)
*! Date      : 01/19/2009
*! Purpose   : Loop on ordline file
*:****************************************************************************************
*! Called from : Class "EDIPROCESSPO" method "update"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
Function lfOrdLp
Local cAlias, cOrder, cHD_SKU
cAlias = Alias()
Select Ordline
cOrder = Order()
Set Order To Ordline   && CORDTYPE+ORDER+STR(LINENO,6)

If Seek('O'+laData[1])
  Scan Rest While  CORDTYPE+Order+Str(Lineno,6) = 'O'+laData[1]
    cHD_SKU = Get_HD_SKU()
    IF !EMPTY(cHD_SKU)
      *B609540,1 WAM 02/28/2011 Profile is linked to order line directly not to design line
      *= Fill_OrdDsgn()
      *B609540,1 WAM 02/28/2011 (End)
      = Fill_ProfValu(cHD_SKU)
    ENDIF   
    Select Ordline
  Endscan
ENDIF

SELECT OrdLine
IF !EMPTY(cOrder)
  SET ORDER TO (cOrder)
ENDIF 

IF !EMPTY(cAlias)
  SELECT (cAlias)
ENDIF   

Endfunc

*:****************************************************************************************
*! Name      : Fill_OrdDsgn
*! Developer : Hassan Ali (HIA)
*! Date      : 01/19/2009
*! Purpose   : Fill Orddsgn
*:****************************************************************************************
*! Called from : "BriVmain" program, Fun "lfOrdLp"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
Function Fill_OrdDsgn

Select OrdDsgn
&&ORDLINE   && ORDER+CORDLINE+STR(LINENO,6)
If !Seek(laData[1]+Str(OrdLine.Lineno,6))
  Insert Into OrdDsgn(cdesignid,cordline             ,Lineno,Order    ,price,nimprints);
              Values ('DESIGN1',Str(OrdLine.Lineno,6),1     ,laData[1],0    ,0)

Endif

Endfunc

*:****************************************************************************************
*! Name      : Fill_ProfValu
*! Developer : Hassan Ali (HIA)
*! Date      : 01/19/2009
*! Purpose   : Fill ProfValu
*:****************************************************************************************
*! Called from : "BriVmain" program, Fun "lfOrdLp"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
Function Fill_ProfValu

PARAMETERS pcHD_SKU
pcHD_SKU = padr(pcHD_SKU,8,SPACE(1))
Select ProfValu
&&PROFILE   && CPRO_TYPE+CKEY+CPRO_CODE
If !Seek('SO'+'O'+laData[1]+Str(OrdLine.Lineno,6))
  SELECT PROFSKU 
  *B609284,1 WLD 06/03/2010 Update the profiles for SKU with the values Account+Header Part+Style\Color [Begin]
  *IF SEEK('ST'+OrdLine.Account+pcHD_SKU)
  *  SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'ST'+OrdLine.Account+pcHD_SKU
   IF SEEK('ST'+OrdLine.Account+pcHD_SKU+OrdLine.Style)
    SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'ST'+OrdLine.Account+pcHD_SKU+OrdLine.Style
      *B609284,1 WLD 06/03/2010 Update the profiles for SKU with the values Account+Header Part+Style\Color [End]
      SCATTER MEMVAR MEMO
      
      m.cPRO_TYPE = 'SO'

      *B609540,1 WAM 02/28/2011 Profile is linked to order line directly not to design line
      *m.cKEY      = "O"+laData[1]+Str(OrdLine.Lineno,6)+Str(1,6)
      m.cKEY  = "O"+laData[1]+Str(OrdLine.Lineno,6)
      *B609540,1 WAM 02/28/2011 (End)
      
      SELECT ProfValu
      APPEND BLANK 
      GATHER MEMVAR MEMO 
      
    ENDSCAN 
  Endif
Endif

ENDFUNC

*:****************************************************************************************
*! Name      : Get_HD_SKU
*! Developer : Hassan Ali (HIA)
*! Date      : 01/19/2009
*! Purpose   : Get Main SKU [ On the header level not in size level]
*:****************************************************************************************
*! Called from : "BriVmain" program, Fun "lfOrdLp"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*B608906,1 WLD Get the PROFVALU file with SKU# incoming in EDI PO 06/24/2009
Function Get_HD_SKU
  LOCAL cHD_SKU, cOrder
  STORE "" TO cHD_SKU
  && Get Main SKU [ On the header level not in size level]
  SELECT SPCK_HDR
  cOrder = ORDER()
  SET ORDER TO SKU_STYLE   && TYPE+ACCOUNT+STYLE+PACK_ID
  *B608906,1 WLD Get the PROFVALU file with SKU# incoming in EDI PO 06/24/2009 [Begin]
  *IF SEEK('S'+Ordline.account+Ordline.style)
  *  cHD_SKU = SPCK_HDR.SKU
  *ENDIF
  lcOrderLineNote = OrdLine.NOTE_MEM
  lcPRFSku = ''
  IF AT('PRF|',lcOrderLineNote) > 0
    lcOrderLineNote=SUBSTR(lcOrderLineNote,AT('PRF|',lcOrderLineNote)+LEN('PRF')+1)
    lcPRFSku = SUBSTR(lcOrderLineNote,1,AT('|',lcOrderLineNote)-1)
  ENDIF
  =SEEK('S'+Ordline.account+Ordline.STYLE)
  SCAN REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+Ordline.account+Ordline.STYLE ;
      FOR IIF(!EMPTY(lcPRFSku),(LEFT(ALLTRIM(lcPRFSku),LEN(ALLTRIM(SPCK_HDR.SKU)))=ALLTRIM(SPCK_HDR.SKU)),.T.)
    cHD_SKU = SPCK_HDR.SKU
  ENDSCAN
  *B608906,1 WLD Get the PROFVALU file with SKU# incoming in EDI PO 06/24/2009 [End]
  IF !EMPTY(cOrder)
    SET ORDER TO &cOrder.
  ENDIF 
  RETURN cHD_SKU
ENDFUNC 
*:****************************************************************************************
*! Name      : SaveSKU
*! Developer : Walid Hamed (WLD)
*! Date      : 06/24/2009
*! Purpose   : Save SKU# incoming in EDI PO raw file at Order Line Note with Qualf. PRF
*:****************************************************************************************
*! Called from : Class EDIPROCESSPO method "ProcessPO"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*B608906,1 WLD Get the PROFVALU file with SKU# incoming in EDI PO 06/24/2009
FUNCTION lfSaveSKU
  IF !EMPTY(MSTYLE)
    MLNOTES = MLNOTES + 'PRF|'+ ALLTRIM(MSTYLE) + '|'
  ENDIF

ENDFUNC
