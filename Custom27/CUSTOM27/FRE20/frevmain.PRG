*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Sameh Saiid Ezzat
*: Date      : 10/17/2004
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
*: Name      : lfPIDwrite
*: Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 10/17/2004
*: Purpose   : Write the Prepack PID line.
******************************************************************************************
*: Called from : Class "Send832"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C123378,1 SSE 10/17/2004
FUNCTION lfPIDwrite
  *if  A) Extended Size scale
  *and B) Style is Case Pack one having a 'P' in the 2nd character of the scale code
  *and C) this Case pack is found in the scale file as a Prepack
  PRIVATE lcCurKey 
  lcCurKey = SCALE.TYPE+SCALE.SCALE+SCALE.prepak
  *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [Begin]
  *IF llExtSzScl AND SUBSTR(STYLE,18,1) = 'P' AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
  IF EdiAcPrt.lCasePack AND SUBSTR(RIGHT(ALLTRIM(STYLE),2),1,1) = EdiAcPrt.cPrePckLtr AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
    *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [End]
  
    MUOM = 'PU'
  *B131929,1 WLD In case of not a PrePack MUOM will be empty - no PID seg will be sent  5/7/2006 [begin]  
  else  
    MUOM = ''
   *B131929,1 WLD In case of not a PrePack MUOM will be empty - no PID seg will be sent  5/7/2006 [End] 
  ENDIF
  =SEEK(lcCurKey,'Scale','Scale')
  *-- End of lfPIDwrite.

  *:****************************************************************************************
  *: Name      : lfSLNwrite
  *: Developer : Sameh Saiid Ezzat (SSE)
  *: Date      : 10/17/2004
  *: Purpose   : Write the Prepack SLN line.
  ******************************************************************************************
  *: Called from : Class "Send832"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C123378,1 SSE 10/17/2004
FUNCTION lfSLNwrite
  *if  A) Extended Size scale
  *and B) Style is Case Pack one having a 'P' in the 2nd character of the scale code
  *and C) this Case pack is found in the scale file as a Prepack
  PRIVATE lcCurKey
  lcCurKey = SCALE.TYPE+SCALE.SCALE+SCALE.prepak

  *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [Begin]
  *IF llExtSzScl AND SUBSTR(STYLE,18,1) = 'P' AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
  IF EdiAcPrt.lCasePack AND SUBSTR(RIGHT(ALLTRIM(STYLE),2),1,1) = EdiAcPrt.cPrePckLtr AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
    *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [End]
    SKIP VAL(lcSize)-1 IN SCALE
    PRIVATE mSLNLineNo, lnSLNLoop, mSLNUPC, lcSLNLoop, lcPrePak, lcNextScl
    *lcNextScl = SUBSTR(Scale.Scale,1,1)+PADL(VAL(SUBSTR(Scale.Scale,2,2))+1,2,'0')
    lcNextScl = SUBSTR(SCALE.SCALE,1,1)+PADL(ALLTRIM(STR(VAL(SUBSTR(SCALE.SCALE,2,2))+1,2)),2,'0')
    lcNextSty = SUBSTR(STYLE,1,16) + lcNextScl
    lcPrepack = SCALE.prepak
    lnLineNo = 0
    FOR lnSLNLoop = 1 TO 8
      lcSLNLoop = ALLTRIM(STR(lnSLNLoop,1))
      IF SCALE.Pp&lcSLNLoop > 0 AND SEEK(STRTRAN(STYLE,'P','0',OCCURS('P',STYLE))+lcSLNLoop,'StyleUPC','StyleUPC')
        lnLineNo = lnLineNo + 1
        mSLNLineNo = ALLTRIM(STR(MLINENO))+'.'+ALLTRIM(STR(lnLineNo))
        mSLNUPC = StyleUPC.cUPCnum1+StyleUPC.cUPCnum2+StyleUPC.cUPCnum3
        mSLNpp  = SCALE.Pp&lcSLNLoop
        llWriteDoc=IIF(oField2Seg.DO(ln832Pointer,'041','SycEdisc',EDIPD.cMapSet),llWriteDoc,.F.)
      ENDIF
    ENDFOR

    *Write the second line of the extended size scale
    =SEEK('P'+lcNextScl+lcPrepack,'Scale','Scale')
    FOR lnSLNLoop = 1 TO 8
      lcSLNLoop = ALLTRIM(STR(lnSLNLoop,1))
      IF SCALE.Pp&lcSLNLoop > 0 AND SEEK(lcNextSty+lcSLNLoop,'StyleUPC','StyleUPC')
        lnLineNo = lnLineNo + 1
        mSLNLineNo = ALLTRIM(STR(MLINENO))+'.'+ALLTRIM(STR(lnLineNo))
        mSLNUPC = StyleUPC.cUPCnum1+StyleUPC.cUPCnum2+StyleUPC.cUPCnum3
        mSLNpp  = SCALE.Pp&lcSLNLoop
        llWriteDoc=IIF(oField2Seg.DO(ln832Pointer,'041','SycEdisc',EDIPD.cMapSet),llWriteDoc,.F.)
      ENDIF
    ENDFOR
  ENDIF
  =SEEK(lcCurKey,'Scale','Scale')
  *-- End of lfSLNwrite.

  *:****************************************************************************************
  *: Name      : lfIT1write
  *: Developer : Sameh Saiid Ezzat (SSE)
  *: Date      : 10/17/2004
  *: Purpose   : Write the Prepack IT1 line with changing quantity and price ONLY.
  ******************************************************************************************
  *: Called from : Class "Send810"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C123378,1 SSE 10/17/2004
FUNCTION lfIT1write
  *if  A) Extended Size scale
  *and B) Style is Case Pack one having a 'P' in the 2nd character of the scale code
  *and C) this Case pack is found in the scale file as a Prepack
  PRIVATE lcCurKey
  lcCurKey = SCALE.TYPE+SCALE.SCALE+SCALE.prepak

  *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [Begin]
  *IF EDIPD.cMapSet = 'DLD' AND llExtSzScl AND SUBSTR(STYLE,18,1) = 'P' AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
  IF EdiAcPrt.lCasePack AND SUBSTR(RIGHT(ALLTRIM(STYLE),2),1,1) = EdiAcPrt.cPrePckLtr AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
    *C128072,1 SSE 05/29/2005 Make changes in the code to comply with 128047 & 128072. [End]
    SKIP VAL(lcCount)-1 IN SCALE
    PRIVATE lnIT1Loop, lcIT1Loop, lcPrePak, lcNextScl, lnPpkQty
    *lcNextScl = SUBSTR(Scale.Scale,1,1)+PADL(VAL(SUBSTR(Scale.Scale,2,2))+1,2,'0')
    lcNextScl = SUBSTR(SCALE.SCALE,1,1)+PADL(ALLTRIM(STR(VAL(SUBSTR(SCALE.SCALE,2,2))+1,2)),2,'0')
    lcPrepack = SCALE.prepak
    lnPpkQty = 0

    FOR lnIT1Loop = 1 TO 8
      lcIT1Loop = ALLTRIM(STR(lnIT1Loop,1))
      IF SCALE.Pp&lcIT1Loop > 0
        lnPpkQty = lnPpkQty + SCALE.Pp&lcIT1Loop
      ENDIF
    ENDFOR

    *Write the second line of the extended size scale
    =SEEK('P'+lcNextScl+lcPrepack,'Scale','Scale')
    FOR lnIT1Loop = 1 TO 8
      lcIT1Loop = ALLTRIM(STR(lnIT1Loop,1))
      IF SCALE.Pp&lcIT1Loop > 0
        lnPpkQty = lnPpkQty + SCALE.Pp&lcIT1Loop
      ENDIF
    ENDFOR

    XTOTQTY = XTOTQTY / IIF(lnPpkQty=0,1,lnPpkQty)
    XPRICE  = Price * IIF(lnPpkQty=0,1,lnPpkQty)
  ENDIF
  =SEEK(lcCurKey,'Scale','Scale')
  *-- End of lfIT1write.
  *:****************************************************************************************
  *: Name      : lfGETUPSNO
  *: Developer : Ramez Raafat (RRE)
  *: Date      : 12/19/2004
  *: Purpose   : Pick Up the UPS tracking#
  ******************************************************************************************
  *: Called from : Class "Send856"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : UPS Tracking#
  ******************************************************************************************
  *B125267,1 RRE 12/19/2004
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
    *B126606 AHH wrong field name     03/02/2005 [begin]
    *lctrckno = ARUPSSHP.CTRCK_NO
    lctrckno = ARUPSSHP.ctrack_no
    *B126606 AHH wrong field name     03/02/2005 [end]

  ENDIF

  RETURN lctrckno
  SELECT CCurentWrk
  *-- End of GETUPSNO.
  *:****************************************************************************************
  *: Name      : lfPpQtyPrc
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 01/23/2005
  *: Purpose   : Get the Prepack scale.
  ******************************************************************************************
  *: Called from : Class "EdiProcessPO"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C125102,1 WLD 01/23/2005 [Begin]
FUNCTION lfPpQtyPrc
  *if  A) Extended Size scale
  *and B) Style is Case Pack one having a 'P' in the 2nd character of the scale code
  *and C) this Case pack is found in the scale file as a Prepack

  *!*	IF (EDIPD.cMapSet = 'DLD' OR EDIPD.cMapSet = 'NOR') AND llExtSzScl AND MUOM = 'CA' AND SUBSTR(STYLE,18,1) = 'P' AND SEEK('P'+STRTRAN(SCALE.SCALE,'P','0'),'Scale','Scale')
  *!*	  SKIP VAL(lcCount)-1 IN SCALE
  *!*	  PRIVATE lnPO1Loop, lcPO1Loop, lcPrePak, lcNextScl, lnPpkQty
  *!*	  lcNextScl = SUBSTR(SCALE.SCALE,1,1)+PADL(ALLTRIM(STR(VAL(SUBSTR(SCALE.SCALE,2,2))+1,2)),2,'0')
  *!*	  lcPrepack = SCALE.prepak
  *!*	  lnPpkQty = 0

  *!*	  FOR lnPO1Loop = 1 TO 8
  *!*	    lcPO1Loop = ALLTRIM(STR(lnPO1Loop,1))
  *!*	    IF SCALE.Pp&lcPO1Loop > 0
  *!*	      lnPpkQty = lnPpkQty + SCALE.Pp&lcPO1Loop
  *!*	    ENDIF
  *!*	  ENDFOR

  *!*	*Write the second line of the extended size scale
  *!*	  =SEEK('P'+lcNextScl+lcPrepack,'Scale','Scale')
  *!*	  FOR lnPO1Loop = 1 TO 8
  *!*	    lcPO1Loop = ALLTRIM(STR(lnPO1Loop,1))
  *!*	    IF SCALE.Pp&lcPO1Loop > 0
  *!*	      lnPpkQty = lnPpkQty + SCALE.Pp&lcPO1Loop
  *!*	    ENDIF
  *!*	  ENDFOR

  *!*	  *XTOTQTY = XTOTQTY / IIF(lnPpkQty=0,1,lnPpkQty)
  *!*	  *XPRICE  = Price * IIF(lnPpkQty=0,1,lnPpkQty)
  *!*	  MQTY = MQTY * IIF(lnPpkQty=0,1,lnPpkQty)
  *!*	  MPRI  = MQTY * IIF(lnPpkQty=0,1,lnPpkQty) / MPri
  *!*
  *!*	ENDIF
  *!*	=SEEK(lcCurKey,'Scale','Scale')

  *C128072,1 SSE 05/29/2005 Make issue # 125102 comply with 128047 & 128072. [Begin]
  *There is major modifications in the Case Pack issue # 125102 where we should
  *adjust the code to work on the requested modifications (issues 128047 & 128072)
  *!*	IF ALLTRIM(oAriaApplication.ActiveCompanyId) ='37'
  *!*	  IF Mqty>0 AND lnStQty <> mqty
  *!*	    lnStQty = Mqty
  *!*	 ENDIF
  *!*	  PRIVATE lcsclKey
  *!*	  lctrgAlias = ALIAS()
  *!*	  lcPpTot = 0
  *!*	  IF (EDIPD.cMapSet = 'DLD' OR EDIPD.cMapSet = 'NOR') AND MUOM = 'CA'
  *!*	    SELECT SCALE
  *!*	    ln_prepack_total = 0
  *!*	    IF SEEK('P'+ SUBSTR(STYLE.STYLE,17,1),'Scale','Scale')
  *!*	      lc_counter = 0
  *!*	      lc_suffex  = SUBSTR(SCALE.SCALE,2,2)
  *!*	      SCAN REST WHILE TYPE+SCALE+prepak = 'P'+ SUBSTR(STYLE.STYLE,17,1)
  *!*	        ** StyleUPC.SIZE  values in [1,2...8]
  *!*	        ** we need to locate the record no (StyleUPC.SIZE) at the scale set.
  *!*	        IF lc_suffex  = SUBSTR(SCALE.SCALE,2,2)
  *!*	          lc_counter  = lc_counter + 1
  *!*	          IF ALLTRIM(StyleUPC.SIZE) = ALLTRIM(STR(lc_counter))
  *!*	            ln_prepack_total = ln_prepack_total + SCALE.PpTot
  *!*	          ENDIF
  *!*	        ELSE
  *!*	          lc_counter  = 1
  *!*	          lc_suffex  = SUBSTR(SCALE.SCALE,2,2)
  *!*	        ENDIF
  *!*	      ENDSCAN
  *!*	    ENDIF
  *!*	    lnStQty = lnStQty * IIF(ln_prepack_total=0,1,ln_prepack_total)
  *!*	    lnPrice    = lnPrice /IIF(ln_prepack_total=0,1,ln_prepack_total)
  *!*	  ENDIF
  *!*	  SELECT (lctrgAlias)
  *!*	ENDIF
  IF EdiAcPrt.lCasePack AND EdiAcPrt.cCasePack = MUOM AND SUBSTR(RIGHT(ALLTRIM(STYLE.STYLE),2),1,1) = EdiAcPrt.cPrePckLtr
    PRIVATE lcOldAlias, ln_PrePack_Total, lnCounter, lcSuffix
    lcOldAlias = ALIAS()
    *-- Asking if the 2 characters which we select from the Partner Setup screen is the same
    *-- as the unit of measurement and also the Scale Case Pack letter is the same as the selected Pre Pack Letter
    SELECT SCALE
    ln_PrePack_Total = 0
    IF SEEK('P'+ SUBSTR(RIGHT(ALLTRIM(STYLE.STYLE),3),1,1),'Scale','Scale')
      lnCounter = 0
      lcSuffix  = SUBSTR(SCALE.SCALE,2,2)
      SCAN REST WHILE TYPE + SCALE + prepak = 'P'+ SUBSTR(RIGHT(ALLTRIM(STYLE.STYLE),3),1,1)
        IF lcSuffix  = SUBSTR(SCALE.SCALE,2,2)
          lnCounter  = lnCounter + 1
          IF ALLTRIM(StyleUPC.SIZE) = ALLTRIM(STR(lnCounter))
            ln_PrePack_Total = ln_PrePack_Total + SCALE.PpTot
          ENDIF
        ELSE
          lnCounter  = 1
          lcSuffix  = SUBSTR(SCALE.SCALE,2,2)

          *C128072,1 SSE 07/12/2005 Make issue # 125102 comply with 128047 & 128072. [Begin]
          IF ALLTRIM(StyleUPC.SIZE) = ALLTRIM(STR(lnCounter))
            ln_PrePack_Total = ln_PrePack_Total + SCALE.PpTot
          ENDIF
          *C128072,1 SSE 07/12/2005 Make issue # 125102 comply with 128047 & 128072. [End]

        ENDIF
      ENDSCAN
    ENDIF
      *B132399,1 HIA handel multi store case [Begin]
      * MQty = MQty * IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)
      * MPri = MPri /IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)   
       *lnPrice = MPri
       *IF TYPE('lnStQty') <> "U"
       *lnStQty = MQTY
       * endif
    IF TYPE('lnStQty') <> "U"
      lnStQty = lnStQty * IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)
       lnPrice = MPri /IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)  
    ELSE
       MQty = MQty * IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)
       MPri = MPri /IIF(ln_PrePack_Total=0,1,ln_PrePack_Total)   
       lnPrice = MPri
    ENDIF
    *B132399,1 HIA handel multi store case [End]
    SELECT (lcOldAlias)
  ENDIF
  *C128072,1 SSE 05/29/2005 Make issue # 125102 comply with 128047 & 128072. [End]
  *-- End of lfPpQtyPrc.
  *C125102,1 WLD 01/23/2005 [End]

  *:****************************************************************************************
  *: Name      : lfPrRefrsh
  *: Developer : Sameh Saiid Ezzat (SSE)
  *: Date      : 05/30/2005
  *: Purpose   : Refresh the Case Pack folder for the 1st time in the Partner Setup Screen.
  ******************************************************************************************
  *: Called from : Partner Setup Screen
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C128072,1 SSE 05/30/2005
FUNCTION lfPrRefrsh
  *if  A) Extended Size scale
  *and B) Style is Case Pack one having a 'P' in the 2nd character of the scale code
  *and C) this Case pack is found in the scale file as a Prepack
  oGetMemVar   = CREATEOBJECT("GetMemVar")
  PRIVATE llExtSzScl
  llExtSzScl = oGetMemVar.DO("M_USEEXSSC",oAriaApplication.ActiveCompanyId)
  IF llExtSzScl
    WITH oPrtSetObj.ARIAFORM1.PAGEFRAME
      .PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).oCasePack.cboCasePack.ENABLED = INLIST(oPrtSetObj.ActiveMode,'E','A') AND EdiAcPrt.lCasePack
      .PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).oCasePack.cboPrePackLetter.ENABLED = INLIST(oPrtSetObj.ActiveMode,'E','A') AND EdiAcPrt.lCasePack
    ENDWITH
  ENDIF
  RELEASE oGetMemVar
  *-- End of lfPrRefrsh.

  *:****************************************************************************************
  *: Name      : lfPrStInit
  *: Developer : Sameh Saiid Ezzat (SSE)
  *: Date      : 05/30/2005
  *: Purpose   : Initialization of the Case Pack folder in the Partner Setup Screen.
  ******************************************************************************************
  *: Called from : Partner Setup Screen
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
  *C128072,1 SSE 05/30/2005
FUNCTION lfPrStInit
  oGetMemVar   = CREATEOBJECT("GetMemVar")
  PRIVATE llExtSzScl
  llExtSzScl = oGetMemVar.DO("M_USEEXSSC",oAriaApplication.ActiveCompanyId)
  IF llExtSzScl       && Display the Case Pack page folder in case of Extended Size Scale companies only
    *Adjust the new added Page folder settings (Caption,Font,...etc)
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT = oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT + 1
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).CAPTION = "Case Pack Settings"
    *oPrtSetObj.Ariaform1.PageFrame.Pages(oPrtSetObj.Ariaform1.PageFrame.PageCount).FontName = "MS Sans Serif"
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).FONTNAME =   oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(1).FONTNAME
    *oPrtSetObj.Ariaform1.PageFrame.Pages(oPrtSetObj.Ariaform1.PageFrame.PageCount).FontSize = 8
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).FONTSIZE =   oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(1).FONTSIZE

    *oCasePack = CREATEOBJECT("CasePack")
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).ADDOBJECT('oCasePack','CasePack')
    oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGES(oPrtSetObj.ARIAFORM1.PAGEFRAME.PAGECOUNT).oCasePack.VISIBLE = .T.
  ENDIF
  RELEASE oGetMemVar
*-- End of lfPrStInit.
*:****************************************************************************************
*: Name      : lfBTNTMRSH
*: Developer : WLD
*: Date      : 01/26/2006
*: Purpose   : Add timer button to Network screen
******************************************************************************************
*: Called from : Class "network"
FUNCTION lfBTNTMRSH

  THISFORM.ADDOBJECT('btntimer1','ariacommandbutton')
  THISFORM.btntimer1.VISIBLE = .T.
  THISFORM.btntimer1.LEFT    = 290
  THISFORM.btntimer1.WIDTH   = 110
  THISFORM.btntimer1.TOP     = 130

ENDFUNC &&lfBTNTMRSH
*-- End of lfBTNTMRSH.
******************************************************************************************
*:****************************************************************************************
*: Name      : lfTMRPRT
*: Developer : WLD
*: Date      : 02/10/2006
*: Purpose   : Add timer button to Network screen
******************************************************************************************
*: Called from : Timer class method receive850
FUNCTION lfTMRPRT
  lcPartCode =  'INTER'
ENDFUNC && lfTMRPRT
*-- End of lfTMRPRT.
******************************************************************************************
*:****************************************************************************************
*: Name      : lfTMRWAR
*: Developer : WLD
*: Date      : 02/10/2006
*: Purpose   : Add timer button to Network screen
******************************************************************************************
*: Called from : Timer class method UPDATEPO
FUNCTION lfTMRWAR
  MCWARECODE = 'INTER'
ENDFUNC && lfTMRWAR
*-- End of lfTMRWAR.
******************************************************************************************