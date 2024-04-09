*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah
*: Date      : 04/18/2006
*: Purpose   : Main program for EDI Triggers
*:C200749,T20070219.0003
*****************************************************************************************
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
*: Name      : lfNOSTRCMP
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Don't preview the Start and Complete date at header .
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
*C129880,1 WLD 04/18/2006
FUNCTION lfNOSTRCMP

  REPLACE llCustom WITH .T. IN EdiPrvPOHD

  *-- End of lfNOSTRCMP.
  *:****************************************************************************************
  *: Name      : lfPRVTR1
  *: Developer : WLD
  *: Date      : 04/18/2006
  *: Purpose   : PREIVEW PO REPORT
  ******************************************************************************************
  *: Called from : Class "EDIPROCESSPO"
  ******************************************************************************************
  *: Calls       : None
  ******************************************************************************************
  *: Return      : None
  ******************************************************************************************
FUNCTION LFPRVTR1
  IF TYPE('llforce')='L' AND llforce= .t.
   LVARPRTSYS = .T.
    llPrtSysFnd = .T.
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ENDFUNC
*:****************************************************************************************
*: Name      : lfADJFLS
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust the System files to locate the partner records for 850
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfADJFLS

  =SEEK(cPartCode+'850','SYCEDIPD','TRANS')
  =SEEK(cPartCode,'SYCEDIPH')

ENDFUNC
*:****************************************************************************************
*: Name      : lfPRVTR2
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Prepare to PREIVEW PO REPORT
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFPRVTR2
  IF LVARPRTSYS
    ************************************************************************************
    STORE '' TO MPrtCode,MCONTREF,MSDRL,MTRANMETHOD,MREQSHPST,MTXTMSG
    STORE '' TO Mkey, lcPack_Id, lcPack_Sz, lcPack_Clr, lcPack_Vrn, cBlkOrd
    STORE 0 TO MPck_Price,MPo4Qs,MPo4Qi,MLINE
    STORE .F. TO llPack_Rng, llPack_PrPk
    lRange_Process = .F.

    lnSelect = SELECT(0)
    IF !USED('EdiSoHd')
      USE (oAriaApplication.DataDir+'EdiSoHd') IN 0
    ENDIF
    IF !USED('EdiSoDt')
      USE (oAriaApplication.DataDir+'EdiSoDt') IN 0
    ENDIF
    IF CURSORGETPROP("Buffering",'EDISOHD') != 5
      lSuccess = CURSORSETPROP("Buffering",5,'EDISOHD')
    ENDIF
    IF CURSORGETPROP("Buffering",'EDISODT') != 5
      lSuccess = CURSORSETPROP("Buffering",5,'EDISODT')
    ENDIF

    MPrtCode =  SycEdiPD.cPartCode

    IF !EMPTY(EDILIBDT.cEDIRef)
      *=oThis.DeletePrvTrans()
      =FNDELPRV()
    ENDIF
    *Header, Detail or Summary
    lcProcessLevel = "Header"

    xPoId      = 'PO'

    *-- Read the order header from the file
    RFlag = .T.
    llReject = .F.

    STORE '' TO MNote1, MNote2, MCustPo, MStatus, MOrder, MDept,MDEPTDESC, ;
      MClass, MVend , MEvent , MMERC_TYPE, MBLANK_ORD

    *-- Initialize Modification Date
    MEntered  = oAriaApplication.SystemDate
    STORE {} TO MStart,MCancel,MComplete,MModDate,MREQSTDATE
    MStart    = oAriaApplication.SystemDate
    MDISC     = 0              &&  Discount percent
    MBillNo   = '    '
    MStore    = ''
    MBStore   = ''
    XSeason   = '  '
    xWareCode = ''
    MPower    = 0
    MPower1   = 0
    MMulti    = 'N'
    llMulti   = .F.
    MType     = ' '
    MCodId    = ' '
    MPOType   = ' '
    MQty      = 0
    MPo1Qty   = 0
    MPri      = 0
    MPO4Qty   = 1
    mPAck = ''
    MTrnStNum = ''
    MSRP = 0
    MEDIQUALFR = ""

    *Check Stores GLN
    DECLARE MMStoreGln(10)

    STORE '' TO MStoreGln,MBStoreGln,MBillNoGln,MMStoreGln

    STORE SPACE(1) TO MStyle, MStyl, MColor, MColo, MSize, MCodId, MUPC,MDESC1,MLNOTES,MUOM
    STORE 1 TO MInerPack,MPackUnit

    *-- Intialize Alternative Ship to adderss
    STORE '' TO MStore,MStName,MStAdd1,MStAdd2,MStCity,MStState,MStZip
    STORE '' TO MBYSTORE,MBYName,MByAdd1,MByAdd2,MByCity,MByState,MByZip
    STORE '' TO MFASTORE,MFAName,MFAAdd1,MFAAdd2,MFACity,MFAState,MFAZip
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    STORE '' TO MBTStore,MBtName,MBtAdd1,MBtAdd2,MBtCity,MBtState,MBtZip
    STORE '' TO MFMSTore,MFMName,MFMAdd1,MFMAdd2,MFMCity,MFMState,MFMZip
    DECLARE mSDQstore(1)
    STORE '' TO mSDQstore
    *C200746 HIA EDI Purchase Order Analysis Report   HIA

    DECLARE MMStore(10),MMQty(10)
    STORE 0   TO MMQty,MBook,MBookAmt
    STORE ' ' TO MMStore
    STORE ''  TO MTDisc,MTType,MTBasis,MFob,MSpcInst,MTDisc1
    STORE 0   TO MTDSCU,MTDays,MTNet
    STORE 'N' TO MEOM

    XPO_P_ID = SycEdiPD.cPartID
    oProgress.lblFirstLabel.CAPTION = 'Analysis Customer PO# '+ALLTRIM(EDILIBDT.cEDIRef)+' from: '+ALLTRIM(SycEDIPH.cPartName)
    lcTxtMsg = ''
    STORE SPACE(2) TO lcPoRcvLine

    SELECT SYCEDISG
    * read the order HEADER
    *-- Note: BEG  will be read within this loop
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    OLD_MBSTORE = ''
    OLD_MBILLNO = ''
    LBTSTORE_FLG = .F.
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    DO WHILE xLoop_Id <> '03' .AND. xFlag
      *C200746 HIA EDI Purchase Order Analysis Report
      OLD_MBSTORE = MBStore
      OLD_MBILLNO = MBILLNO
      *C200746 HIA EDI Purchase Order Analysis Report
      xFlag = oThis.ReadSeg()
      IF xLoop_Id <> '03'
        lcProcessLevel = "Detail"
      ENDIF
      =oThis.SegToField()
      *C200746 HIA EDI Purchase Order Analysis Report
      IF LBTSTORE_FLG AND INLIST(MSEGID , 'N2','N3','N4')
        MBTStore = IIF(!EMPTY(MBILLNO),MBILLNO,IIF(!EMPTY(MBStore),MBStore,MStore))
        MBtName  = MStName
        MBtAdd1  = MStAdd1
        MBtAdd2  = MStAdd2
        MBtCity  = MStCity
        MBtState = MStState
        MBtZip   = MStZip
      ELSE
        LBTSTORE_FLG = .F.
      ENDIF
      IF (MBStore <>  OLD_MBSTORE OR MBILLNO <>  OLD_MBILLNO)AND MSEGID = 'N1'
        LBTSTORE_FLG = .T.
      ENDIF
      *C200746 HIA EDI Purchase Order Analysis Report
      IF MSEGID = 'ITD'
        MEOM   = IIF(INLIST(XFIELD(1),'02','12'),'Y','N')
        *-- Terms Discount Days Due
        MTDays = VAL(XFIELD(5))
        *-- Terms Discount Percent
        MTDSCU = VAL(XFIELD(3))
        *-- Terms Net Days
        MTNet  = VAL(XFIELD(7))
        *-- Description
        MTDisc = XFIELD(12)
        *N128672,2 AAH Preview PO in a report [Begin]
        MTDisc1 = MTDisc1+IIF(EMPTY(MTDisc1),'',';')+XFIELD(12)
        *N128672,2 AAH Preview PO in a report [End]

      ENDIF
      IF (MSEGID = 'MSG'  OR MSEGID = 'TXT') AND !EMPTY(MTXTMSG)
        lcTxtMsg = lcTxtMsg +  ALLTRIM(MTXTMSG)  + CHR(13)
      ENDIF
      =oProgress.CurrentProgress(lnTransNo+FSEEK(EDI_PO,0,1))
    ENDDO
    *Header, Detail or Summary [Begin]
    lcProcessLevel = "Detail"

    IF EMPTY(MTDisc)
      MTDisc = MTType+' ' +ALLTRIM(STR(MTDSCU,10,3))+'% From '+MTBasis+'DAYS DUE ';
        +ALLTRIM(STR(MTDays))+'NET '+ALLTRIM(STR(MTNet))
    ENDIF
    MCustPo  = ALLTRIM(MCustPo)

    IF EMPTY(MStore)
      MStore = MBStore
    ENDIF
    MStore = SUBSTR(ALLTRIM(MStore)+SPACE(8),1,8)
    IF EMPTY(MBillNo)
      MBillNo = MBStore
    ENDIF
    MBillNo = SUBSTR(ALLTRIM(MBillNo)+SPACE(8),1,8)

    *Fill EdiPrvPOHD
    =SEEK(MPrtCode,'Sycediph','PARTNER')
    SELECT EdiPrvPOHD
    IF !SEEK(MPrtCode+PADR(MCustPo,15,' '))
      APPEND BLANK
    ENDIF

    REPLACE cPartCode WITH  ALLTRIM(MPrtCode) ,;
      cPartName		WITH SycEDIPH.cPartName ,;
      CustPo          WITH MCustPo,;
      POdate          WITH EDILIBDT.dDate,;
      START           WITH MStart,;
      COMPLETE   WITH MComplete,;
      REQSTDATE WITH MREQSTDATE,;
      cStstore        WITH MStore ,cStStoreNm   WITH MStName,cStAdd1  WITH MStAdd1 ,cStAdd2  WITH MStAdd2,StCtyStZp  WITH MStCity +','+ MStState+','+MStZip,;
      cFAstore       WITH MFASTORE,cFAStoreNm WITH MFAName,cFAAdd1 WITH MFAAdd1 ,cFAAdd2 WITH MFAAdd2 ,FACtyStZp WITH MFACity +','+ MFAState+','+MFAZip,;
      cTrmTyp        WITH MTType ,	cTrmDesc WITH MTDisc1,;
      CSDRL         WITH MSDRL,;
      Notes           WITH lcTxtMsg,;
      DEPT           WITH MDept,;
      VEND          WITH MVend
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    REPLACE ;
      cBTstore WITH MBTStore,;
      cBTStoreNm WITH MBtName,;
      cBTAdd1 WITH MBtAdd1 ,;
      cBTAdd2 WITH MBtAdd2 ,;
      BTCtyStZp WITH MBtCity +','+ MBtState+','+MBtZip
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    *WLD add new column to hold custom Flag [Begin]
    *Using the new field to pevent preview the Start\Complete Date in the report
    *=gfDoTriger('EBMAIN',PADR('NOSTRCMP',10))
    REPLACE llCustom WITH .T. IN EdiPrvPOHD
    *WLD add new column to hold custom Flag [End]

    *Detail Loop PO1
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    SELECT EdiPrvPOSL
    DELETE FOR cPartCode+CustPo+cLine+cSLNLNNO = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef)
    SELECT EdiPrvPODt
    *C200746 HIA EDI Purchase Order Analysis Report   HIA
    IF xLoop_Id = '03' .OR. MStatus = 'L' .OR. (MStatus='G' .AND. MSEGID='DTM')

      SELECT EdiPrvPODt
      MQty     = 0
      MPo1Qty  = 0
      *-- Initialize line status as new one
      MPri      = 0
      MSRP = 0
      MPckQty = 1
      MPck_Price = 0
      STORE SPACE(1) TO MStyle, MStyl, MColor, MColo, MSize, MCodId, MUPC,MKUPC,MDESC1,MLNOTES,MUOM,;
        MPKGTYPE,MLINE,MGRNDSHADE,MPRMCONT,MPRTDRW,MSTYNUM,MBARCODESZ
      STORE 1 TO MInerPack,MPackUnit,MSRP
      STORE {} TO MStart,MCancel,MComplete,MModDate,MREQSTDATE

      *-- READ THE ITEM LINES
      DO WHILE xLoop_Id  <>  '01' .AND. xFlag
        IF xLoop_Id  = '03'
          MQty    = 0
          MPo1Qty = 0
          *-- Initialize line status as new one
          MPri   = 0
          MSRP = 0
          STORE SPACE(1) TO MStyle, MStyl, MColor, MColo, MSiz, MSize, MCodId,MUPC,MKUPC,MPKGTYPE,MLINE,MGRNDSHADE

          STORE SPACE(1) TO MDESC1,MLNOTES,MUOM,MPRMCONT,MPRTDRW,MSTYNUM,MFINECLASS,MDept,MBARCODESZ
          STORE 1 TO MInerPack,MPackUnit,MSRP
          STORE {} TO MStart,MCancel,MComplete,MModDate,MREQSTDATE
          STORE SPACE(1) TO MKUPC

          =oThis.SegToField()

          IF EMPTY(MUOM)
            MUOM  = IIF(xPoId='PO',XFIELD(3),XFIELD(5))
          ENDIF

          xFlag = oThis.ReadSeg()

          *-- CTP Segment - New Price - Will be read within this loop
          *C200746 HIA EDI Purchase Order Analysis Report   HIA
          *DO WHILE INLIST(xLoop_Id,'02','04') .AND. xFlag
          STORE ' ' TO MMStore,mSDQstore
          STORE ' ' TO MMStoreGln
          STORE '' TO MSLNLINES
          STORE 0   TO MMQty ,N_SDQ

          MPo1Qty = MQty
          MPO1PRI = MPri
          MPO1UPC = MUPC
          MPO1STYLE  = MStyle
          MPO1STYL   = MStyl
          MPO1COLOR  = MColor
          MPO1SIZE  = MSize
          MPO1PRI = MPri
          MPO1UOM = MUOM

          DO WHILE INLIST(xLoop_Id,'02','04','05','33') .AND. xFlag
            IF xLoop_Id= '33'
              MSLNQTY = MQty
              MSLNPRI = MPri
              MSLNUPC = MUPC
              MSLNSTYLE  = MStyle
              MSLNSTYL   = MStyl
              MSLNCOLOR  = MColor
              MSLNSIZE  = MSize
              MSLNPRI = MPri
              MSLNUOM = MUOM
            ENDIF


            =oThis.SegToField()
            IF xLoop_Id = '05'
              N_SDQ = N_SDQ + 1
              IF ALEN(mSDQstore,1) < N_SDQ
                DIMENSION mSDQstore(N_SDQ)
              ENDIF
              mSDQstore(N_SDQ) = ''
              FOR I = 1 TO ALEN(MMStore,1)
                IF !EMPTY(MMStore(I))
                  mSDQstore(N_SDQ) = mSDQstore(N_SDQ) + MMStore(I)+ '   QTY: '+  ALLTRIM(STR(MMQty(I))) +CHR(13)
                ENDIF
              ENDFOR

            ENDIF
            IF MSEGID = 'SLN'
              MSLNLINES = MSLNLINES + '(*) UPC: ' + IIF(MSLNUPC <> MUPC,MUPC,SPACE(14))
              MSLNLINES = MSLNLINES + ' SKU: ' + IIF(MSLNSTYLE  <> MStyle,MStyle,SPACE(14))
              MSLNLINES = MSLNLINES + ' QTY: ' + IIF(MSLNQTY <> MQty,ALLTRIM(STR(MQty)),SPACE(5)) + CHR(13)
              
              MSLNLINES = MSLNLINES + SPACE(5) + ' QTY[SLN]: ' + ALLTRIM(STR(MSLNQTY ))
              MSLNLINES = MSLNLINES + ' Pri: ' + IIF(MSLNPRI <> MPri,ALLTRIM(STR(MPri)),SPACE(5))
              MSLNLINES = MSLNLINES + ' STY: ' + IIF(MSLNSTYL   <> MStyl,ALLTRIM(MStyl),SPACE(15))
              MSLNLINES = MSLNLINES + ' COL: ' + IIF(MSLNCOLOR  <> MColor,ALLTRIM(MColor),SPACE(15)) 
              
              MSLNLINES = MSLNLINES + ' SZ: '  + IIF(MSLNSIZE  <> MSize,ALLTRIM(MSize),SPACE(15))
              MSLNLINES = MSLNLINES + ' UOM: ' + IIF(MSLNUOM <> MUOM,ALLTRIM(MUOM),SPACE(15))                            
              
              MSLNLINES = MSLNLINES + CHR(13)
            ENDIF
            *C200746 HIA EDI Purchase Order Analysis Report   HIA

            IF MSEGID = 'PO4'
              MInerPack = VAL(XFIELD(1))
              MPackUnit = IIF(EMPTY(XFIELD(2)),IIF(EMPTY(XFIELD(14)),1,VAL(XFIELD(14))),VAL(XFIELD(2)))
            ENDIF

            xFlag = oThis.ReadSeg()
          ENDDO


          IF !EMPTY(MKUPC)
            MUPC = IIF(VAL(SUBSTR(ALLTRIM(MKUPC),2,1))=0,SUBSTR(ALLTRIM(MKUPC),3),SUBSTR(ALLTRIM(MKUPC),2))
          ENDIF

          SELECT EdiPrvPODt
          APPEND BLANK

          REPLACE cPartCode WITH EDILIBDT.cPartCode ,;
            CustPo     WITH MCustPo ,;
            cLine      WITH MLINE ,;
            VndSty     WITH MPO1STYL ,;
            DEPT       WITH MDept ,;
            cClass     WITH MFINECLASS ,;
            STYLE      WITH MSTYNUM ,;
            COLOR      WITH MPO1COLOR ,;
            StyDesc    WITH MDESC1 ,;
            BarCodeSz  WITH MBARCODESZ ,;
            SKU        WITH MPO1STYLE ,;
            nRetPrc    WITH MSRP ,;
            price      WITH MPO1PRI ,;
            SIZE       WITH MPO1SIZE ,;
            PRMCONT    WITH MPRMCONT ,;
            PRTDRW     WITH MPRTDRW ,;
            PKGTYPE    WITH MPKGTYPE ,;
            Qty        WITH MPo1Qty ,;
            TotCost    WITH Qty * price

          *C200746 HIA EDI Purchase Order Analysis Report
          REPLACE cupc WITH MPO1UPC
          IF !EMPTY(MMStore)
            Multi_stores = ''
            FOR I = 1 TO ALEN(mSDQstore,1)
              Multi_stores = Multi_stores + mSDQstore(I) + CHR(13)
            ENDFOR
            REPLACE mstores WITH Multi_stores
          ENDIF
          *C200746 HIA EDI Purchase Order Analysis Report

          REPLACE START WITH MStart,;
            COMPLETE WITH MComplete,;
            ColorCode WITH MColo
          *E302335,1 HIA add line notes [begin]
          MLNOTES = ALLTRIM(STRTRAN(MLNOTES,'|',CHR(13)))
          REPLACE mNotes WITH MLNOTES ADDITIVE
          *E302335,1 HIA add line notes [End]
          *C200746 HIA EDI Purchase Order Analysis Report
          IF !EMPTY(MSLNLINES)
            SELECT EdiPrvPOSL
            APPEND BLANK
            REPLACE cPartCode WITH EDILIBDT.cPartCode ,;
              CustPo WITH MCustPo ,;
              cLine WITH MLINE ,;
              VndSty  WITH MStyl ,;
              MSLNLINE WITH MSLNLINES
          ENDIF
          *C200746 HIA EDI Purchase Order Analysis Report
        ELSE
          xFlag = oThis.ReadSeg()

        ENDIF
        =oProgress.CurrentProgress(lnTransNo+FSEEK(EDI_PO,0,1))
      ENDDO
    ELSE
      xFlag = oThis.ReadSeg()
    ENDIF

    RET_ALIAS = ALIAS()
    SELECT &RET_ALIAS.

    =TABLEREVERT(.T. , 'EDISOHD')
    =TABLEREVERT(.T. , 'EDISODT')


    SELECT (lnSelect)
    llReturn = RFlag

    *PREVIEW************************************************************************************
    =oThis.CloseEdiFile()
    lcTran  = EDILIBDT.mTranText
    lnTransNo = lnTransNo + VAL(SUBSTR(lcTran,AT('|LENGTH|',lcTran)+8))
    SET ORDER TO TAG FILEISAGS IN EDILIBDT
    IF !llBatch

      SELECT EdiPrvPOHD
      COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'TmpPrvPOHD.dbf'  FOR cPartCode+CustPo = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX
      *COPY TO 'D:\EDI3\Work\XG1258GX\' + 'TmpPrvPOHD.dbf'  FOR cPartCode+CustPo = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX
      SELECT EdiPrvPODt
      *E302335,1 HIA add line notes [Begin]
      *C200746 HIA EDI Purchase Order Analysis Report   HIA
      COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'TmpPrvPODT.dbf'  FOR cPartCode+CustPo+cLine = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX
      *COPY TO 'D:\EDI3\Work\XG1258GX\' + 'TmpPrvPODT.dbf'  FOR cPartCode+CustPo+cLine = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX

      *COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'TmpPrvPODT.dbf'  FOR cPartCode+CustPo+cLine = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) AND RIGHT(ALLTRIM(cLine),1)='A' FOX2X WITH CDX
      SELECT EdiPrvPOSL
      COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'TmpPrvPOSL.dbf'  FOR cPartCode+CustPo+cLine+cSLNLNNO = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX
      *COPY TO 'D:\EDI3\Work\XG1258GX\' + 'TmpPrvPOSL.dbf'  FOR cPartCode+CustPo+cLine+cSLNLNNO = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef) FOX2X WITH CDX

      *C200746 HIA EDI Purchase Order Analysis Report   HIA
      *E302335,1 HIA add line notes [End]


      DO FORM oAriaApplication.ScreenHome + 'PrvTrnPO.scx'  WITH oThis.ReportDir
    ENDIF
    *PREVIEW************************************************************************************
    RETURN .T.
  ELSE
    RETURN .F.

  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : lfPRVRPT
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : PREIVEW PO REPORT
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfPRVRPT

  IF llBatch AND  llPrtSysFnd
    SELECT EdiPrvPOHD
    COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' +'TmpPrvPOHD.dbf' FOX2X WITH CDX
    SELECT EdiPrvPODt
    COPY TO oAriaApplication.WorkDir + oThis.ReportDir + '\' +'TmpPrvPODT.dbf' FOX2X WITH CDX
    DO FORM oAriaApplication.ScreenHome + 'PrvTrnPO.scx'  WITH oThis.ReportDir
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF

ENDFUNC
******************************************************************************************
FUNCTION FNDELPRV
  LOCAL lnSelect
  lnSelect = SELECT(0)
  SELECT EdiPrvPOHD
  DELETE FOR ALLTRIM(cPartCode) + ALLTRIM(CustPo) == ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef)
  SELECT EdiPrvPODt
  DELETE FOR ALLTRIM(cPartCode) + ALLTRIM(CustPo)+ cLine  = ALLTRIM(EDILIBDT.cPartCode)+ALLTRIM(EDILIBDT.cEDIRef)

  SELECT (lnSelect)
ENDFUNC
******************************************************************************************
*: Name      : lfSHRTCT
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Create PO short cut in case of raw file with sysfiles receiving
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfSHRTCT
  IF  EDILIBDT.cEDITrnSt = 'A'  AND  EDILIBDT.LPRTSYS
    lcString_trg = "\<Preview Entire Transaction," + lcString_trg
    lcStatus_trg = "TFFFFFF"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : lfGETDSC
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Get PO description in case of raw file with sysfiles receiving
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfGETDSC
  IF EMPTY(EDILIBDT.cStatus)
    lcString2_trg = 'Received from: ' + IIF(SEEK(EDILIBDT.cPartCode , 'SYCEDIPH') ,ALLTRIM(SycEDIPH.cPartName) , '')
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : lfCRTTAB
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Create PO temp files in case of raw file with sysfiles receiving
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfCRTTAB

  oThis.ReportDir  = oGetTempName.DO()
  MD (oAriaApplication.WorkDir + oThis.ReportDir)

  *C200683,1 WLD Increase address length - Add Color Code 09/19/2006 [Begin]
  *CREATE TABLE (oAriaApplication.WorkDir + THis.ReportDir +'\' + 'EdiPrvPOHD') (cPartCode c(6),cPartname c(30),CustPo c(15),POdate DATE ,Start DATE ,complete DATE ,;
  REQSTDATE DATE,cStstore c(8),cStStoreNm c(30),cStAdd1 c(30),cStAdd2 c(30),StCtyStZp c(30),;
  cFAstore c(8),cFAStoreNm c(30),cFAAdd1 c(30),cFAAdd2 c(30),FACtyStZp c(30),;
  cTrmTyp c(2),cTrmDesc c(200),Notes memo,CSDRL C(30),DEPT C(5),VEND C(15),llCustom L )
  *B607967,1 WLD Increase the width of Factor ID 02/08/2007 [Begin]
  *  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir +'\' + 'EdiPrvPOHD') (cPartCode c(6),cPartName c(30),CustPo c(15),POdate DATE ,START DATE ,COMPLETE DATE ,;
  *    REQSTDATE DATE,cStstore c(8),cStStoreNm c(50),cStAdd1 c(60),cStAdd2 c(60),StCtyStZp c(60),;
  *    cFAstore c(8),cFAStoreNm c(60),cFAAdd1 c(60),cFAAdd2 c(60),FACtyStZp c(60),;
  *    cTrmTyp c(2),cTrmDesc c(200),Notes MEMO,CSDRL c(30),DEPT c(5),VEND c(15),llCustom L )

  *C200746 HIA EDI Purchase Order Analysis Report   HIA
  *!*	  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir +'\' + 'EdiPrvPOHD') (cPartCode c(6),cPartName c(30),CustPo c(15),POdate DATE ,START DATE ,COMPLETE DATE ,;
  *!*	    REQSTDATE DATE,cStstore c(8),cStStoreNm c(50),cStAdd1 c(60),cStAdd2 c(60),StCtyStZp c(60),;
  *!*	    cFAStore c(12),cFAStoreNm c(60),cFAAdd1 c(60),cFAAdd2 c(60),FACtyStZp c(60),;
  *!*	    cTrmTyp c(2),cTrmDesc c(200),Notes MEMO,CSDRL c(30),DEPT c(5),VEND c(15),llCustom L )

  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir +'\' + 'EdiPrvPOHD') (cPartCode c(6),cPartName c(30),CustPo c(15),POdate DATE ,START DATE ,COMPLETE DATE ,;
    REQSTDATE DATE,cStstore c(8),cStStoreNm c(50),cStAdd1 c(60),cStAdd2 c(60),StCtyStZp c(60),;
    cFMstore c(8),cFMStoreNm c(50),cFMAdd1 c(60),cFMAdd2 c(60),FMCtyStZp c(60),;
    cBTstore c(8),cBTStoreNm c(50),cBTAdd1 c(60),cBTAdd2 c(60),BTCtyStZp c(60),;
    cFAstore c(12),cFAStoreNm c(60),cFAAdd1 c(60),cFAAdd2 c(60),FACtyStZp c(60),;
    cTrmTyp c(2),cTrmDesc c(200),Notes MEMO,CSDRL c(30),DEPT c(5),VEND c(15),llCustom L )
  *C200746 HIA EDI Purchase Order Analysis Report   HIA
  *B607967,1 WLD Increase the width of Factor ID 02/08/2007 [End]
  *C200683,1 WLD Increase address length - Add Color Code 09/19/2006 [End]
  INDEX ON cPartCode+CustPo  TAG EdiPrvPOHD

  *C200683,1 WLD Increase address length - Add Color Code 09/19/2006 [Begin]
  *CREATE TABLE (oAriaApplication.WorkDir + This.ReportDir + '\' + 'EdiPrvPODT') (cPartCode c(6),CustPo c(15),cLine C(6),VndSty c(19),Dept c(5),;
  cClass c(5),Style c(19),color c(30),StyDesc c(30),BarCodeSz c(5),SKU c(16),nRetPrc n(12,2), price n(12,2),Size c(10),PRMCONT c(5),;
  PRTDRW c(5) ,PKGTYPE c(5), qty n(6),TotCost n(12,2),Start Date,complete Date )



  *E302335,1 HIA add filed to hold the line notes [begin]
  *!*	  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'EdiPrvPODT') (cPartCode c(6),CustPo c(15),cLine c(6),VndSty c(19),DEPT c(5),;
  *!*	    cClass c(5),STYLE c(19),COLOR c(30),ColorCode c(5),StyDesc c(30),BarCodeSz c(5),SKU c(16),nRetPrc N(12,2), price N(12,2),SIZE c(10),PRMCONT c(5),;
  *!*	    PRTDRW c(5) ,PKGTYPE c(5), Qty N(6),TotCost N(12,2),START DATE,COMPLETE DATE )
  *C200746 HIA EDI Purchase Order Analysis Report   HIA
  *!*	  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'EdiPrvPODT') (cPartCode c(6),CustPo c(15),cLine c(6),VndSty c(19),DEPT c(5),;
  *!*	    cClass c(5),STYLE c(19),COLOR c(30),ColorCode c(5),StyDesc c(30),BarCodeSz c(5),SKU c(16),nRetPrc N(12,2), price N(12,2),SIZE c(10),PRMCONT c(5),;
  *!*	    PRTDRW c(5) ,PKGTYPE c(5), Qty N(6),TotCost N(12,2),START DATE,COMPLETE DATE ,mNotes memo)


  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'EdiPrvPOSL') (cPartCode c(6),CustPo c(15),cLine c(6),;
    cSLNLNNO c(6),VndSty c(19),DEPT c(5),cClass c(5),STYLE c(19),COLOR c(30),ColorCode c(5),StyDesc c(30),BarCodeSz c(5),;
    SKU c(16),nRetPrc N(12,2), price N(12,2),SIZE c(10),PRMCONT c(5),PRTDRW c(5) ,PKGTYPE c(5), Qty N(6),TotCost N(12,2),;
    START DATE,COMPLETE DATE ,mNotes MEMO,MSLNLINE MEMO)
  INDEX ON cPartCode+CustPo+cLine+cSLNLNNO  TAG EdiPrvPOSL



  CREATE TABLE (oAriaApplication.WorkDir + oThis.ReportDir + '\' + 'EdiPrvPODT') (cPartCode c(6),CustPo c(15),cLine c(6),VndSty c(19),DEPT c(5),;
    cClass c(5),STYLE c(19),COLOR c(30),ColorCode c(5),StyDesc c(30),BarCodeSz c(5),SKU c(16),nRetPrc N(12,2), price N(12,2),SIZE c(10),PRMCONT c(5),;
    PRTDRW c(5) ,PKGTYPE c(5), Qty N(6),TotCost N(12,2),START DATE,COMPLETE DATE ,mNotes MEMO, cupc c(30), mstores MEMO)
  *C200746 HIA EDI Purchase Order Analysis Report   HIA
  *E302335,1 HIA add filed to hold the line notes [End]


  *C200683,1 WLD Increase address length - Add Color Code 09/19/2006 [End]
  INDEX ON cPartCode+CustPo+cLine  TAG EdiPrvPODt




ENDFUNC

****************************************************************************************************************
*: Name      : lfADJLIB
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : ADJUST THE SYCEDIPD FILE
******************************************************************************************
*: Called from : Class "EDIMAIN"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfADJLIB
  IF EDILIBDT.cEDITrnSt = 'A'
    =SEEK(EDILIBDT.cPartCode + EDILIBDT.cEDITrnTyp,'SYCEDIPD','TRANS')
  ENDIF
  RETURN .T.
ENDFUNC

****************************************************************************************************************
*: Name      : lfADJVAR
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : ADJUST THE MAPPING-VERSION VARIABLES
******************************************************************************************
*: Called from : Class "EDIMAIN"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfADJVAR

  IF EDILIBDT.cEDITrnSt = 'A'
    IF EDILIBDT.LPRTSYS && copied as special code without comment at 04/11/2006 WLD and AAH added to correct the code
      mMapSet   = SycEdiPD.cMapSet
      mVersion  = PADR(LEFT(SycEdiPD.cVersion,6),12)
    ENDIF
  ENDIF

ENDFUNC
******************************************************************************************
*: Name      : lfclsst
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : ADJUST THE CLOSE ST METHOD
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfclsst
  REPLACE cAckStatus WITH IIF(llPrtFndSys,"A",'R') ,;
    cEDITrnSt  WITH 'R' ,;
    cRejReason WITH 'FTFFF'
ENDFUNC
******************************************************************************************
*: Name      : lfclsISA
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : ADJUST THE CLOSE ST METHOD
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCLSISA
  oThis.ErrorFlg = IIF(EMPTY(EDIERORH.mErrorCode),oThis.ErrorFlg,.T.)
ENDFUNC
******************************************************************************************
*: Name      : lfCLSGS
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : ADJUST THE CLOSE GS METHOD
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCLSGS
  oThis.ErrorFlg = IIF(EMPTY(EDIERORD.mErrorCode),oThis.ErrorFlg,.T.)
ENDFUNC
******************************************************************************************
*: Name      : LFVARINI
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Initilize the 'llPrtFndSys' variable
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFVARINI
  llPrtFndSys = .F.
ENDFUNC
******************************************************************************************
*: Name      : LFCHKPR1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Check if raw file id's exist at the system files.
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCHKPR1

  llPrtFndSys = .F.
  IF !llPartFund_trg AND SEEK(PADR(ALLTRIM(oThis.laMain[1,6]),15),'SYCEDIPD','PARTID') AND SEEK(SycEdiPD.cPartCode,'SYCEDIPH')
    llPrtFndSys = .T.
    llExistAccpISA = llExistAccpISA OR llPrtFndSys
    REPLACE cPartCode  WITH ALLTRIM(SycEdiPD.cPartCode) ,;
      cintchgseq WITH IIF(lnFieldNo_trg >= 13 ,oThis.laMain[1,13] , '') IN EDIERORH
    lcErrMsg = ''
  ENDIF
ENDFUNC
**********************************************************************************************
*: Name      : LFADJMAP
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust the class properties in case of raw file Id's found at the system files
**********************************************************************************************
*: Called from : Class "EBRECEIVE"
**********************************************************************************************
*: Calls       : None
**********************************************************************************************
*: Return      : None
**********************************************************************************************
FUNCTION LFADJMAP

  oThis.PartnerCode     = IIF(llPrtFndSys,SycEDIPH.cPartCode,EDIPH.cPartCode)
  oThis.PartnerLineSep  = IIF(llPrtFndSys,lcLineSep_trg,EVALUATE(EDIPH.cLineSep))
  oThis.PartnerFieldSep = IIF(llPrtFndSys,lcFieldSep_trg,EVALUATE(EDIPH.cFieldSep))
  oThis.AckTran         = IIF(llPrtFndSys,.T.,EDIPH.lSendAck)
  RETURN .T.
ENDFUNC
******************************************************************************************
*: Name      : LFADJAC1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class properties in case of raw file Id's found at the system files
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJAC1

  oThis.laAcknowledge[5]  = IIF(!llPrtFndSys,EDIACPRT.cCmpISAQl,oThis.laMain[1,7])

ENDFUNC
******************************************************************************************
*: Name      : LFCHKPR2
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Check if raw file id's exist at the system files.
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCHKPR2

  llPrtFndSys = .F.
  IF !llPartFund_trg AND SEEK(PADR(ALLTRIM(oThis.laMain[1,6]),15),'SYCEDIPD','PARTID') AND SEEK(SycEdiPD.cPartCode,'SYCEDIPH')
    llPrtFndSys = .T.

    llExistAccpISA = llExistAccpISA OR llPrtFndSys

    llFrstISA=.T.

    lcErrMsg = ''
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFCHKPR3
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Check if raw file id's exist at the system files.
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCHKPR3
  llPrtFndSys = .F.
  IF !llPartFund_trg AND SEEK(PADR(ALLTRIM(oThis.laMain[1,6]),15),'SYCEDIPD','PARTID') AND SEEK(SycEdiPD.cPartCode,'SYCEDIPH')
    llPrtFndSys = .T.
    llExistAccpISA = llExistAccpISA OR llPrtFndSys
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFADJGERR
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class properties in case of raw file Id's found at the system files
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfADJGERR
  IF llPrtFndSys
    oThis.GSTypVerErr = .T.
    oThis.GSTypVerErrCode = ''
    IF lctrantype_trg <> '997' AND (EMPTY(lctrantype_trg) .OR. !FOUND('SYCEDIPD') .OR.;
        SUBSTR(ALLTRIM(oThis.laMain[2,8]),1,6) <> SUBSTR(SycEdiPD.cVersion,1,6) OR ;
        !SEEK(SycEdiPD.cMapSet+LEFT(SycEdiPD.cVersion,6),'SYCEDISG') OR ;
        !SEEK(SycEdiPD.cMapSet+LEFT(SycEdiPD.cVersion,6),'SYCEDISF'))

      oThis.GSTypVerErr = .T.
      IF EMPTY(lctrantype_trg) .OR. !FOUND('SYCEDIPD') .OR. !FOUND('SYCEDISG') .OR. !FOUND('SYCEDISF')
        oThis.GSTypVerErrCode = '1'
      ELSE
        oThis.GSTypVerErrCode = '2'
      ENDIF
    ELSE
      oThis.GSTypVerErr = .F.
    ENDIF

    oThis.MapSet         = SycEdiPD.cMapSet
    oThis.VERSION        = PADR(LEFT(SycEdiPD.cVersion,6),12)

  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFCHKPR4
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Check if raw file id's exist at the system files.
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFCHKPR4
  IF !llPartFund_trg  AND SEEK(PADR(ALLTRIM(oThis.laMain[1,6]),15),'SYCEDIPD','PARTID') AND SEEK(SycEdiPD.cPartCode,'SYCEDIPH')
    llPrtFndSys = .T.
    llExistAccpISA = llExistAccpISA OR llPrtFndSys
    lcErrMsg = ''
  ENDIF
  RETURN llPrtFndSys
ENDFUNC


******************************************************************************************
*: Name      : LFADJST
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class properties in case of raw file Id's found at the system files
*:           : And fill the temp files data
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJST
  IF llPrtFndSys
    IF oThis.SendAck .AND. oThis.AckTran .AND. llToBeAck_trg
      *-- Write the Transaction Set Response Header "AK2" [Begin]
      DIMENSION oThis.laAcknowledge[2]
      oThis.laAcknowledge[1] = oThis.laMain[3,1]
      oThis.laAcknowledge[2] = oThis.laMain[3,2]
      =oThis.WriteSeg('AK2' , 2)
      lnAkSTSeCnt = lnAkSTSeCnt + 1
      *-- Write the Transaction Set Response Header "AK2" [End]
    ENDIF
    oThis.PartnerCode = SycEdiPD.cPartCode
    SELECT (oThis.TempLibFile)
    APPEND BLANK
    REPLACE cGsId      WITH ALLTRIM(STR(oThis.GsCount)) ,;
      cEDIFilTyp WITH 'R' ,;
      cFileCode  WITH lcFileCode_Trg ,;
      cPartCode  WITH oThis.PartnerCode ,;
      cintchgseq WITH oThis.laMain[1,13] ,;
      cGroupSeq  WITH oThis.laMain[2,6] ,;
      cTranSeq   WITH oThis.laMain[3,2] ,;
      cEDITrnTyp WITH oThis.laMain[3,1] ,;
      cAckStatus WITH '' ,;
      dAckDate   WITH oAriaApplication.SystemDate ,;
      cEDITrnSt  WITH 'R' ,;
      dDate		 WITH ldSentDate,;
      cRecFldSep WITH lcFieldSep_trg ,;
      mTranText  WITH 'START|'+ALLTRIM(STR(lnPosition))+'|' ADDITIVE
    REPLACE LPRTSYS WITH llPrtFndSys,  cEDITrnSt  WITH 'A'
    REPLACE cPartCode WITH oThis.PartnerCode IN EDIERORH
    REPLACE cPartCode WITH oThis.PartnerCode IN EDIERORD

  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFADJSEG
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust the next segment variable in case of raw file ID in exist sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSEG
  IF llPrtFndSys
    lcSegToGo = ''
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class properties in case of raw file Id's found at the system files
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE1
  oThis.laAcknowledge[1] = IIF(llPrtFndSys,oThis.laAcknowledge[1],'R')
  RETURN .T.
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE2
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE2
  REPLACE cStatus    WITH 'R' ,;
    mErrorCode WITH 'Tran.# '+oThis.laMain[3,2]+' Transaction Set Control Number in Header and Trailer Do Not Match'+CHR(13);
    ADDITIVE IN EDIERORD
  oThis.ErrorFlg = IIF(EMPTY(EDIERORD.mErrorCode),oThis.ErrorFlg,.T.)
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE3
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE3

  REPLACE cStatus    WITH 'R' ,;
    mErrorCode WITH 'Tran.# '+oThis.laMain[3,2]+' Number of Included Segments Does Not Match Actual Count'+CHR(13);
    ADDITIVE IN EDIERORD
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE4
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE4
  REPLACE cStatus    WITH 'R' ,;
    mErrorCode WITH 'Tran.# '+oThis.laMain[3,2]+' One or More Segments in Error ' + lcSegErrNam + CHR(13);
    ADDITIVE IN EDIERORD
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE5
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE5
  lcSegErrNam = ''
ENDFUNC
******************************************************************************************
*: Name      : LFADJFLG
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJFLG
  oThis.ErrorFlg = .T.
ENDFUNC
******************************************************************************************
*: Name      : LFADJSE6
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSE6
  REPLACE cAckStatus WITH IIF(llPrtFndSys ,IIF(!('T' $ lcSTRejRes),"A",'R'),oThis.laAcknowledge[1])
ENDFUNC
******************************************************************************************
*: Name      : LFADJGE1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJGE1
  IF llPrtFndSys
    oThis.laAcknowledge[1] ="A"
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFADJIEA
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJIEA
  oThis.ErrorFlg = IIF(EMPTY(EDIERORH.mErrorCode),oThis.ErrorFlg,.T.)
ENDFUNC
******************************************************************************************
*: Name      : LFADJOTH
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJOTH
  IF lcSegId ='BEG'
    lcSegToGo =  'SE /'
  ENDIF
  RETURN llPrtFndSys
ENDFUNC
******************************************************************************************
*: Name      : LFADJSYS
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFADJSYS
  lcSegErrNam = IIF(EMPTY(lcSegErrNam),lcSegErrNam,lcSegErrNam+' / ') + lcSegId
ENDFUNC
******************************************************************************************
*: Name      : LFRUNRE1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFRUNRE1
  IF oThis.ErrorFlg
    =MESSAGEBOX('The received raw file has some errors.Please check the Transaction Status Report.',16, _SCREEN.CAPTION)
  ENDIF
ENDFUNC
******************************************************************************************
*: Name      : LFRUNRE1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFFLOOP
  RETURN .T.
ENDFUNC
******************************************************************************************
*: Name      : LFRUNRE1
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Adjust class prop. and fill data in case of raw file Id's found at sysfiles
******************************************************************************************
*: Called from : Class "EBRECEIVE"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION LFFCOND
  llForCurrComp = llForCurrComp OR llExistAccpISA
  RETURN .T.
ENDFUNC
******************************************************************************************
******************************************************************************************
*: Name      : lfSHRTCT2
*: Developer : WLD
*: Date      : 04/18/2006
*: Purpose   : Create PO short cut in case of raw file with sysfiles receiving
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfSHRTCT2
  IF !EMPTY(lcString_trg)
    lcString_trg = lcString_trg + ",\<Preview Entire Transaction" 
    lcStatus_trg = lcStatus_trg + 'T'
  ENDIF   
ENDFUNC
******************************************************************************************
******************************************************************************************
*: Name      : lfDOSELC
*: Developer : HIA
*: Date      : 04/18/2006
*: Purpose   : Create PO short cut in case of raw file with sysfiles receiving
******************************************************************************************
*: Called from : Class "EDIPROCESSPO"
******************************************************************************************
*: Calls       : None
******************************************************************************************
*: Return      : None
******************************************************************************************
FUNCTION lfDOSELC
IF gnChoiceIND = 8 
  llforce = .T.
  =oObject.Do(.F.,EDILIBDT.cFileCode,"cPartCode+cEdiTrnTyp+cIntChgSeq+cGroupSeq+cTranSeq='"+;
          EDILIBDT.cPartCode+EDILIBDT.cEdiTrnTyp+EDILIBDT.cIntChgSeq+EDILIBDT.cGroupSeq+EDILIBDT.cTranSeq+"'")
  llforce = .F.        
  LVARPRTSYS = .F.
ENDIF   
ENDFUNC