*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah
*: Date      : 05/31/2006
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.
*:****************************************************************************************
*: Name      : lfSTRSTRUC
*: Developer : WLD
*: Date      : 11/14/2005
*: Purpose   : Ship To Store # will be string of Ref 19(Supper Territory) [PadL with 2 digits] +
*:             Ref 2P(Territory)[PadL with 3 digits]+Ref 31 (Region) [PadL with 3 digits] .
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C131661,1 WLD 05/31/2006
FUNCTION lfSTRSTRUC
  IF EDIPD.CMAPSET='EST'
    IF !EMPTY(MSupTrr)  AND !EMPTY(MTerritory) AND !EMPTY(MRegion)
      MStore = PADL(ALLTRIM(MSupTrr),2) + PADL(ALLTRIM(MTerritory),3) + PADL(ALLTRIM(MRegion),3)
      MStore = SUBSTR(ALLTRIM(MStore)+SPACE(8),1,8)
    ENDIF
  ENDIF
  *-- End of lfSTRSTRUC.
  *:****************************************************************************************
  *: Name      : lfSTOREINFO
  *: Developer : WLD
  *: Date      : 05/31/2006
  *: Purpose   : Save Ship To Store addresses.
  ******************************************************************************************
  *: Called from : Class "EDIPROCESSPO"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C131661,1 WLD 05/31/2006
FUNCTION lfSTOREINFO
  IF EDIPD.CMAPSET='EST'
    REPLACE StName    WITH MStName , cAddress1 WITH MStAdd1  , cAddress2 WITH MStAdd2  ,;
      cAddress3 WITH MStCity , cAddress4 WITH MStState , cAddress5 WITH MStZip IN EDIORDHD
    REPLACE Alt_ShpTo WITH .T. IN EDIORDHD
  ENDIF
  *-- End of lfSTOREINFO.
  *:****************************************************************************************
  *: Name      : lfADDSTSTR
  *: Developer : WLD
  *: Date      : 05/31/2006
  *: Purpose   : Adding Ship To Store in the customer file .
  ******************************************************************************************
  *: Called from : Class "EDIPROCESSPO"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C131661,1 WLD 05/31/2006
FUNCTION lfADDSTSTR
  IF EDIPD.CMAPSET = 'EST'
    IF SEEK('T' + EDILIBDT.cEDITranNo,'ORDHDR') AND ORDHDR.STATUS <> 'X' AND EDILIBDT.cEDITrnTyp = '850'
      =oObject.CheckErrors(,.T.,llMultiUpdate)
      IF !llStrFnd
        lnAddstore = MESSAGEBOX("One or more received stores do not exist. Would you like to add them?" ,4,_SCREEN.CAPTION)
        IF lnAddStore = 6
          **ADD Ship To STORE
          IF SEEK('M' + ORDHDR.ACCOUNT , 'CUSTOMER')
            m.BtName     = CUSTOMER.BtName
            m.cCont_code = CUSTOMER.cCont_code
            m.ShipVia    = CUSTOMER.ShipVia
            m.SKUTMPL    = CUSTOMER.SKUTMPL
            m.nTAXRATE   = CUSTOMER.nTAXRATE
            m.CTAXRULE   = CUSTOMER.CTAXRULE
            m.cAddress1  = CUSTOMER.cAddress1
            m.cAddress2  = CUSTOMER.cAddress2
            m.cAddress3  = CUSTOMER.cAddress3
            m.cAddress4  = CUSTOMER.cAddress4
            m.cAddress5  = CUSTOMER.cAddress5
            m.cAddress6  = CUSTOMER.cAddress6
          ENDIF
          MStName    = ORDHDR.StName
          MStAdd1  = ORDHDR.cAddress1
          MStAdd2  = ORDHDR.cAddress2
          MStCity  = ORDHDR.cAddress3
          MStState = ORDHDR.cAddress4
          MStZip   = ORDHDR.cAddress5
          SELECT customer
          APPEND BLANK
          REPLACE Account WITH ORDHDR.ACCOUNT ,STORE WITH ORDHDR.STORE,TYPE WITH 'S',BILLTO WITH 'M',STATUS WITH 'A'
          REPLACE StName    WITH MStName , cCont_code WITH m.cCont_code,;
            cAddress1 WITH MStAdd1,cAddress2 WITH MStAdd2,cAddress3 WITH MStCity,cAddress4 WITH MStState,cAddress5  WITH MStZip,cAddress6  WITH m.cAddress6
          REPLACE BtName    WITH m.BtName   ,cAddress12 WITH m.cAddress1,cAddress22 WITH m.cAddress2,;
            cAddress32 WITH m.cAddress3,cAddress42 WITH m.cAddress4,cAddress52 WITH m.cAddress5,cAddress62 WITH m.cAddress6 ,cCont_cod2 WITH m.cCont_code
          REPLACE SKUTMPL WITH m.SKUTMPL,nTAXRATE WITH m.nTAXRATE,CTAXRULE WITH m.CTAXRULE,ShipVia    WITH m.ShipVia
          REPLACE CADD_USER WITH oariaapplication.User_ID, CADD_TIME WITH TIME(),DADD_DATE WITH oAriaApplication.SystemDate,cADD_VER WITH 'A27'

          REPLACE StName WITH '',cAddress1 WITH '',cAddress2 WITH '',cAddress3 WITH '',;
            cAddress4 WITH '',cAddress5 WITH '' IN OrdHdr
          REPLACE Alt_ShpTo WITH .F. IN OrdHdr
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *-- End of lfADDSTSTR.
  *:****************************************************************************************
  *: Name      : lfGETUPSNO
  *: Developer : WLD
  *: Date      : 05/31/2006
  *: Purpose   : Pick Up the UPS tracking#
  ******************************************************************************************
  *: Called from : Class "Send856"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : UPS Tracking#
  ******************************************************************************************
  *B131663,1 WLD 05/31/2006 by using B125267
FUNCTION lfGETUPSNO
  PRIVATE lnUPCNO
  lctrckno   = ""
  CCurentWrk = ALIAS()
  IF !USED('ARUPSSHP')
    USE (oAriaApplication.DataDir+ 'ARUPSSHP') IN 0 ORDER TAG 'ARUPSSHP' SHARED
  ELSE
    SET ORDER TO ARUPSSHP IN ARUPSSHP
  ENDIF

  IF SEEK(MPACKNO,'ARUPSSHP','ARUPSSHP') then
    lctrckno = ARUPSSHP.ctrack_no

  ENDIF

  RETURN lctrckno
  SELECT CCurentWrk
  *-- End of GETUPSNO.
  
  