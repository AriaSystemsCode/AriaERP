*!*************************************************************
*! Name      : lfSaveStyle
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save new or modified Style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcStyleKey          : Style Key (Style Major, Style - Color, or Style - Color - Scale)
*!                      lcStyMajor          : Style Major
*!                      lcNonMajor          : Style Non - Major
*!                      LCSTYLE             : Style 
*!                      lcColorFil          : Colors temporary file
*!                      llAllColors         : All Colors flag
*!                      llAllScales         : All Scales  flag
*!                      llClearBOM          : Clear BOM Files flag
*!                      lcBomHeadr          : BomHeadr temporary file
*!                      lcBOM               : Bom temporary file
*!                      lcTmpStyPr          : Style Prices temporary file
*!                      lcprofilevalue      : Style Profile values temporary file
*!                      lcprofilelist       : Style Profile List temporary file
*!                      lcPriceCurr         : Style price currency
*!                      lcDutyCurr          : Style Duty currency
*!                      lnPriceRate         : Price Exchange rate
*!                      lnDutyRate          : Duty Exchange rate
*!                      laMCost             : M. Costs Array
*!                      laICost             : I. Costs Array
*!                      lncurrunt1          : Price Currency unit
*!                      lncurrunt2          : Duty Currency unit
*!                      lcWareFil           : WareHouse temporary file
*!                      lnSclNo             : Style Scale Count
*!                      llImprtSty          : Imported style flag
*!                      lextendedattributes : Extended attributes flag
*!                      loFormSet           : Screen formset object
*!*************************************************************
*! Returns            :  True or False
*!*************************************************************
*! Example            :  =lfSaveStyle()
*!*************************************************************
*! Modifications :
*! E303825,1 MMT 05/25/2017 Modify the Style screen saving to be called from anywhere[P20170503.0001]
*! B612610,1 MMT 08/02/2022 Modify the lfSaveBOM in style screen saving function to use SEEK and Locate REST instead of LOCATE FOR{T20220729.0001}
*!*************************************************************
#INCLUDE r:\aria4xp\screens\ic\icstyle.h 
FUNCTION lfSaveStyle
PARAMETERS lcStyleKey,lcStyMajor,lcNonMajor,LCSTYLE,lcColorFil,llAllColors,llAllScales,llClearBOM,lcBomHeadr,lcBOM,lcTmpStyPr,lcprofilevalue,lcprofilelist,;
           lcPriceCurr,lcDutyCurr,lnPriceRate,lnDutyRate,laMCost,laICost,lncurrunt1,lncurrunt2,;
           lcWareFil,lnSclNo,llImprtSty,lextendedattributes,loFormSet
            
lcFromStyle =''
lcActiveMode = 'A'
llCopyContinue = .F.
LLDONTSAVEPROFILE = .F.
IF TYPE('loFormSet') ='O'
  lcActiveMode = loFormSet.ActiveMode
  lcFromStyle  = loFormSet.lcFromStyle 
  llCopyContinue = loFormSet.llCopyContinue
  LLDONTSAVEPROFILE = loFormSet.LLDONTSAVEPROFILE
  lccBrowseTableDBEngine = loFormSet.cBrowseTableDBEngine
ENDIF            
lccBrowseTableDBEngine = "NATIVE"
lcStyT = gfGetMemVar('M_SYSTYPE')
lcSysType  = IIF(EMPTY(lcStyT),'M',lcStyT)
llMultiWh = (gfGetMemVar('M_WAREHOUS')='Y')
llUseStyleConfiguration = (gfGetMemVar('M_STYCNFG')="Y")
llMulCurr = gfGetMemVar('llMulCurr')
llMScale = gfGetMemVar('M_USEEXSSC')
lcDefConfCode = gfGetMemVar('M_CONFCODE')
STORE 0  TO lnscaleln,lnscalepos ,lnstylewid,lncolorwid
DIMENSION laMajSeg[1,1]
DIME laSegInfo[1,9]
lnNonSeg = 0
lnMajSeg  = 0
lcSepart = ''
lcIMjrPt = ''
lfGetSegmentinfo()            
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
SELECT(lcColorFil)

IF !USED('Style')
  = gfOpenTable(oAriaApplication.DataDir + 'Style', 'Style', 'SH')
ENDIF  

IF !lfStyleBusinessRule(lcStyMajor,llCostPrv,lcbomheadr,lcBom)
  RETURN .F.
ENDIF

IF !USED('StyDye')
= gfOpenTable(oAriaApplication.DataDir + 'StyDye', 'StyDye', 'SH')
ENDIF
IF !USED('ICSegVal')
= gfOpenTable(oAriaApplication.DataDir + 'ICSegVal', 'SegVal', 'SH')
ENDIF
IF !USED('WareHous')
= gfOpenTable(oAriaApplication.DataDir + 'WareHous', 'WareHous', 'SH')
ENDIF
IF !USED('Gl_Link')
= gfOpenTable(oAriaApplication.DataDir + 'Gl_Link', 'Gl_Link1', 'SH')
ENDIF
IF !USED('Codes')
  = gfOpenTable(oAriaApplication.DataDir + 'Codes', 'cCode_No', 'SH')
ENDIF
IF !USED('Scale')
  = gfOpenTable(oAriaApplication.DataDir + 'Scale', 'Scale', 'SH')
ENDIF
llSPInstld = (OCCURS('SP',oAriaApplication.CompanyInstalledModules)<>0)
IF llSPInstld 
  llOpenPDMFl = !USED('PDMSTYLE') AND FILE(oAriaApplication.DataDir+'PDMSTYLE.DBF')
  llOpenPDMFl =  llOpenPDMFl AND !USED('PDMLOG') AND FILE(oAriaApplication.DataDir+'PDMLOG.DBF')
  IF llOpenPDMFl
    = gfOpenTable(oAriaApplication.DataDir+'PDMSTYLE','Cstymajor','SH')
    = gfOpenTable(oAriaApplication.DataDir+'PDMLOG'  ,'PDMITEMCD','SH')
  ENDIF
ENDIF


PRIVATE lcNonMjrFlds, lcNonNeededFlds  && non common fields in style.
lcNonMjrFlds = "STYLE     ,SCALE     ,DESC      ,DESC1     ,NMCOST1   ,NMCOST2   ,"+;
  "NMCOST3   ,NMCOST4   ,NMCOST5   ,NICOST1   ,NICOST2   ,NICOST3   ,"+;
  "NICOST4   ,NICOST5   ,NPRCOST2  ,NPRCOST3  ,NPRCOST4  ,NPRCOST5  ,"+;
  "NPRCOST6  ,NPRCOST7  ,NICOST6   ,NICOST7   ,NMCOST6   ,NMCOST7   ,"+;
  "TOTCOST   ,AVE_COST  ,PRICEA    ,PRICEB    ,PRICEC    ,"+;
  "MARKA     ,MARKB     ,MARKC     ,NSUGRETPRI,NMARKRET  ,GROS_PRICE,"+;
  "TRD_DISCA ,TRD_DISCB ,TRD_DISCC ,"+;
  "DISC_PCNT ,CPRICECUR ,CDUTYCUR,LUPCYN"
lcNonNeededFlds = "ord1     ,ord2     ,ord3     ,ord4     ,ord5     ,ord6     ,ord7     ,ord8     ,totord   ,"+;
  "wip1     ,wip2     ,wip3     ,wip4     ,wip5     ,wip6     ,wip7     ,wip8     ,totwip   ,"+;
  "stk1     ,stk2     ,stk3     ,stk4     ,stk5     ,stk6     ,stk7     ,stk8     ,totstk   ,"+;
  "alo1     ,alo2     ,alo3     ,alo4     ,alo5     ,alo6     ,alo7     ,alo8     ,totalo   ,"+;
  "shp1     ,shp2     ,shp3     ,shp4     ,shp5     ,shp6     ,shp7     ,shp8     ,totshp   ,"+;
  "ret1     ,ret2     ,ret3     ,ret4     ,ret5     ,ret6     ,ret7     ,ret8     ,totret   ,"+;
  "ra1      ,ra2      ,ra3      ,ra4      ,ra5      ,ra6      ,ra7      ,ra8      ,totra    ,"+;
  "intrans1 ,intrans2 ,intrans3 ,intrans4 ,intrans5 ,intrans6 ,intrans7 ,intrans8 ,totintrn ,"+;
  "nwo1     ,nwo2     ,nwo3     ,nwo4     ,nwo5     ,nwo6     ,nwo7     ,nwo8     ,ntotwo, NSTKVAL"
lcNonNeededFlds = lcNonNeededFlds + ", cAdd_User, dAdd_Date, cAdd_Time, lLok_stat, cLok_User, dLok_Date, cLok_Time, cEdit_User, dEdit_Date, cEdit_Time"

IF TYPE('loFormSet') ='O'
*--Trigger to save any custom data before updating the style information.
  loFormSet.mDoTrigger(PADR('STARTSAVE',10))
ENDIF  


IF llAllColors OR llAllScales

  SELECT STYLE
  *--Empty record values array, used if you will replace the data on previous used record in style file.
  SCATTER TO laEmptyRec BLANK
  SEEK lcStyleKey  && Bounded record.

  *--First style non major values (Bounded values).
  SCATTER TO laAllBoundFields   &&Read all bounded fields values.

  DECLARE laModfiedFields[1]
  STORE '' TO laModfiedFields,lcModfiedFields
  lnModfiedFields = AFIELDS(laModfiedFields,'STYLE')
  FOR lnCount = 1 TO lnModfiedFields
    IF EVALUATE('STYLE.'+laModfiedFields[lnCount,1]) <> OLDVAL(laModfiedFields[lnCount,1],'STYLE')
      lcModfiedFields = lcModfiedFields + IIF(EMPTY(lcModfiedFields),'',',') + laModfiedFields[lnCount,1]
    ENDIF
  ENDFOR
  IF !EMPTY(lcModfiedFields)
    SCATTER FIELDS &lcModfiedFields TO laModfiedFields
  ENDIF

  SET DELETED OFF
  *--Update Style File with added non majors.
  SELECT (lcColorFil)

  SEEK lcStyleKey
  SCAN REST WHILE STYLE = lcStyleKey FOR !DELETED()
    lcStyle = STYLE
    SCATTER FIELDS &lcNonNeededFlds TO laNonNeededFlds && WSH
    SCATTER FIELDS &lcNonMjrFlds TO laNonMjrFields   &&Read color specific values.
     IF cStatus = 'A'
      GATHER FROM laAllBoundFields                     &&Update bounded values for all colors all fields.
      GATHER FROM laNonNeededFlds FIELDS &lcNonNeededFlds
     ELSE
      IF !EMPTY(lcModfiedFields)
        GATHER FROM laModfiedFields FIELDS &lcModfiedFields
      ENDIF
    ENDIF
 
    GATHER FROM laNonMjrFields FIELDS &lcNonMjrFlds  &&Replace back with color specific values.

 
    SCATTER MEMVAR MEMO  &&Copy record to memvar.
    *--Update Style
    SELECT STYLE
 
    =SEEK(lcStyle)
    LOCATE REST WHILE STYLE = lcStyle FOR !DELETED()
    IF FOUND()
      IF TYPE('loFORMSET') ='O' AND ASCAN(loFORMSET.laEvntTrig,PADR('POPMEMVR',10),1,ALEN(loFORMSET.laEvntTrig,1),1) > 0
        =loFORMSET.mDoTrigger(PADR('POPMEMVR',10))
      ENDIF

      IF EVALUATE(lcColorFil+'.cStatus') $ 'MA'
        GATHER MEMVAR MEMO   && Restore coped record from memvar.
      ELSE
        IF !EMPTY(lcModfiedFields)
          GATHER FROM laModfiedFields FIELDS &lcModfiedFields
          IF 'DESC' $ UPPER(lcModfiedFields)
            REPLACE DESC WITH m.DESC,;
                    DESC1 WITH m.DESC1
          ENDIF
        ENDIF
      ENDIF
      =gfReplace('')
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO   && Restore coped record from memvar.
      *-- WSH [Start] Remove CutSold Quantity Values from New Added Colors
      SCATTER FIELDS &lcNonNeededFlds TO laNonNeededFlds BLANK
      GATHER FROM laNonNeededFlds FIELDS &lcNonNeededFlds
      IF TYPE("Rec_No") = 'C'
        REPLACE Rec_No WITH ''
      ENDIF
      IF  TYPE('loFORMSET') ='O' AND lcActiveMode == 'E' AND ASCAN(loFORMSET.laEvntTrig,PADR('EXPCTOLD',10),1,ALEN(loFORMSET.laEvntTrig,1),1) = 0
        lfUDFDefaultValue()
      ENDIF
      gfReplace('')
      SELECT (lcColorFil)
      SCATTER FIELDS &lcNonNeededFlds TO laNonNeededFlds BLANK
      GATHER FROM laNonNeededFlds FIELDS &lcNonNeededFlds
    ENDIF
    =gfAdd_info('Style')
    gfReplace()
  ENDSCAN

  SET DELETED ON
ENDIF

IF TYPE('loFORMSET') ='O' AND ASCAN(loFORMSET.laEvntTrig,PADR('SVBNSZDA',10),1,ALEN(loFORMSET.laEvntTrig,1),1) > 0
  =loFORMSET.mDoTrigger(PADR('SVBNSZDA',10))
ENDIF

=gfOpenTable('ICSTYHST','STYHST','SH')
SELECT ICSEGVAL
=AFIELDS(laTempStru)

IF !oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'ICSEGVAL')].llNative
  CREATE CURSOR TempSegVal FROM ARRAY laTempStru
  INDEX ON cInvType+CISEGNO+CISEGVAL TAG TempSegVal
ENDIF

IF llClearBOM
  DELETE FROM (lcBomHeadr)
  DELETE FROM (lcBOM)
ENDIF
LOCAL nClrNo

nClrNo = 0
SELECT STYLE
=SEEK(lcStyleKey)

IF lcActiveMode = 'A'
  REPLACE cAdd_User WITH ""
ENDIF

SCAN REST WHILE STYLE = lcStyleKey

  =gfReplace('')

  nClrNo = nClrNo + 1
  *- 1) Update Style History file.***************************************************************************
  SELECT ICSTYHST

  IF !gfSeek(STYLE.STYLE+STR(VAL(oAriaApplication.CurrentYear)-1,4))
    gfAppend()
    gfReplace([Style     WITH STYLE.Style,]+;
      [cFisFYear WITH STR(VAL(oAriaApplication.CurrentYear)-1,4)])
  ENDIF

  IF !gfSeek(STYLE.STYLE+oAriaApplication.CurrentYear)
    gfAppend()
    gfReplace([Style     WITH STYLE.Style,]+;
      [cFisFYear WITH STR(VAL(oAriaApplication.CurrentYear),4)])
  ENDIF

  IF !gfSeek(STYLE.STYLE+STR(VAL(oAriaApplication.CurrentYear)+1,4))
    gfAppend()
    gfReplace([Style     WITH STYLE.Style,]+;
      [cFisFYear WITH STR(VAL(oAriaApplication.CurrentYear)+1,4)])
  ENDIF
  *-*********************************************************************************************************

  *- 2) Update the segment value file if there is a free segment.********************************************
  SELECT ICSEGVAL
  FOR lnCnt=1 TO lnMajSeg
    IF laSegInfo[lnCnt,3] = 'F'
      lnFreeSegNum  = laSegInfo[lnCnt,2]
      lcFreeSegment = SUBSTR(PADR(STYLE.STYLE,lnstylewid),laSegInfo[lnCnt,4],laSegInfo[lnCnt,5])

      IF oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'ICSEGVAL')].llNative
        SELECT ICSEGVAL
        IF !SEEK(lnFreeSegNum+lcFreeSegment)
          gfAppend()
          gfReplace([cISegNo   WITH lnFreeSegNum,]+;
            [cISegVal  WITH lcFreeSegment,]+;
            [cISgValSd WITH STYLE.Desc,]+;
            [cISgValLd WITH STYLE.Desc1])
        ENDIF
      ELSE
        SELECT ICSEGVAL
        IF !SEEK('0001'+lnFreeSegNum+lcFreeSegment, "TempSegVal") AND !gfSeek('0001'+lnFreeSegNum+lcFreeSegment)
          gfAppend()
          gfReplace([cInvType  WITH '0001',]+;
            [cISegNo   WITH lnFreeSegNum,]+;
            [cISegVal  WITH lcFreeSegment,]+;
            [cISgValSd WITH STYLE.Desc,]+;
            [cISgValLd WITH STYLE.Desc1])
          INSERT INTO TempSegVal (cInvType,CISEGNO,CISEGVAL,cISgValSd,cISgValLd) VALUES ('0001',lnFreeSegNum,lcFreeSegment,STYLE.DESC,STYLE.Desc1)
        ENDIF
        *N039535,1 WSH [End]

      ENDIF
    ENDIF
  ENDFOR
  SELECT STYLE
  *-*********************************************************************************************************
  *- 3) Add/Update the BOM and all cost elements.************************************************************
  lfSaveBom(STYLE.lDetCost,nClrNo,llCostPrv,lcStyMajor,lcBomHeadr,lcBom,lcPriceCurr ,lcDutyCurr  ,lnPriceRate ,lnDutyRate,@laMCost,@laICost,lncurrunt1,lncurrunt2)
  
  IF !STYLE.lDetCost  &&Case No Detail Costing.
    *  ThisFormSet.mSaveBom()
  ELSE && Case of Detail Costing.
    *- 4) Calculate cost from bom only if detail costing for not extended scale for new added colors only *****
    IF !llMScale AND lcActiveMode='E' AND SEEK(lcStyMajor,lcBOM, 'BOM')
      IF SEEK(STYLE.STYLE,lcColorFil) AND &lcColorFil..cStatus = 'A'
        lcColor = SUBSTR(STYLE.STYLE,lnstylewid+2,lnColorWid)
        IF !lfSaveCosting(lcColor,STYLE.SCALE,lcBOM,lnSclNo,lcStyMajor,@laSegInfo,llMulcurr,loFormSet)
          RETURN .F.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *-*********************************************************************************************************
  *- 5) Update StyDye file default warehouse record in Case of Single Location Setup.************************
  IF !llMultiWh
    SELECT STYDYE
    IF !gfSeek(STYLE.STYLE+STYLE.cDefWare,'STYDYE')
      gfAppend()
      gfReplace([Style     WITH STYLE.Style,]+;
        [cWareCode WITH STYLE.cDefWare,]+;
        [Desc      WITH Style.Desc])
    ENDIF
    gfReplace([Ave_Cost  WITH STYLE.Ave_Cost,]+;
      [Gl_link   WITH STYLE.Link_code,]+;
      [cDiscCode WITH STYLE.cDiscCode])
    =gfAdd_info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), ALIAS())].lcCursorUpdate)
    *-- [Start], WSH Create a record in StyDye for every Style-Color, WareHouse, & Default Configuration
    IF llUseStyleConfiguration AND STYLE.cdye_flg = 'Y' AND !EMPTY(lcDefConfCode)
      SCATTER TO MEMVAR
      =SEEK(STYLE.STYLE+STYLE.cDefWare)
      LOCATE REST FOR !EMPTY(dyelot)
      IF !FOUND()
        gfAppend(' IN STYDYE', .T.)
        m.dyelot = lcDefConfCode
        gfReplace([dyelot WITH m.Dyelot])
        =gfAdd_info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), ALIAS())].lcCursorUpdate)
      ENDIF
    ENDIF
  ENDIF
  *-*********************************************************************************************************
ENDSCAN

IF USED("TempSegVal")
  USE IN TempSegVal
ENDIF

IF llMultiWh
  lfSaveLocations(lcWareFil,lcStyleKey,lcActiveMode,llUseStyleConfiguration ,lcDefConfCode,loFormSet)
ENDIF

*--Update Style Foreign Prices.****************************************************************************
IF llMulCurr
  lfSaveStyFPrices(lcActiveMode,lcTmpStyPr,lcStyleKey)
ENDIF
*-*********************************************************************************************************

*- Delete the imported style from the PDMstyle file, Also we will change status to be complete.************
IF llImprtSty
  lfSavePDMfiles(lcStyMajor)
ENDIF

*-*********************************************************************************************************
llNCInstl  = (OCCURS('NC',oAriaApplication.CompanyInstalledModules)<>0)
*- if the Inter-Company EDI module is installed and setup and the "System Type" is set to "Back Office"****
IF lcActiveMode='E' AND llNCInstl AND lcSysType = "B"
  lfSaveEDIfiles(lcStyMajor,lcStyleKey,llallcolors)
ENDIF
*-*********************************************************************************************************
lfSaveSQLBOM(lcbomheadr,lcBom)

***N

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

llSusses = gfTABLEUPDATE(lcTranCode, 'STYDYE')
llSusses = llSusses AND gfTABLEUPDATE(lcTranCode, 'ICSTYHST')
llSusses = llSusses AND gfTABLEUPDATE(lcTranCode, 'ICSEGVAL')
llSusses = llSusses AND (!llMulCurr  OR !USED(lcTmpStyPr) OR gfTABLEUPDATE(lcTranCode, 'STYPRICE'))
llSusses = llSusses AND (!llImprtSty OR gfTABLEUPDATE(lcTranCode, 'PDMSTYLE'))
llSusses = llSusses AND (!llImprtSty OR gfTABLEUPDATE(lcTranCode, 'PDMLOG'))
IF lcActiveMode = 'E' AND llNCInstl AND lcSysType = "B"
  llSusses = llSusses AND gfTABLEUPDATE(lcTranCode, 'EDICatgD')
  llSusses = llSusses AND gfTABLEUPDATE(lcTranCode, 'EDICatgH')
  llSusses = llSusses AND gfTABLEUPDATE(lcTranCode, 'EDITrans')
ENDIF

*--Run Saving Trigger.
*--Update Master STYLE file (Buffer Tables).***************************************************************
SELECT STYLE
SEEK lcStyleKey


IF lcActiveMode = 'A' AND TYPE('loFormSet')='O' AND !EMPTY(lcFromStyle) AND ASCAN(loFormSet.laEvntTrig ,PADR('SAVNOTES',10))<>0
  =LOFORMSET.mDoTrigger(PADR('SAVNOTES' ,10))
ENDIF


IF lcActiveMode  ='E' AND ALLTRIM(gfGetMemVar('M_Cost_Met')) = 'S' AND !STYLE.lDetCost
  SELECT STYLE
  gfReplace("AVE_COST with TOTCOST")
ENDIF



IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig ,PADR('UPDPOCST',10))<>0
  =loFormSet.mDoTrigger(PADR('UPDPOCST' ,10))
ENDIF



IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig ,PADR('UPDPOHTS',10))<>0
  =loFormSet.mDoTrigger(PADR('UPDPOHTS' ,10))
ENDIF


IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig ,PADR('ADDNEWSKU',10))<>0
  =loFormSet.mDoTrigger(PADR('ADDNEWSKU' ,10))
ENDIF



IF lcActiveMode ='A' AND TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig,PADR('ADDCFG',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(PADR('ADDCFG',10))
  SELECT STYLE
ENDIF

IF llCopyContinue = .T.
  && Copy Style Major Profile and profile list, to be saved or used in screen sysetprf Display.
  SELECT PROFVALU
  lcFromStyle = PADR(lcFromStyle,lnstylewid)
  lcNotFound  = lcsepart +PADR('*',lnColorWid,'*')
  lcToStyle   = lcStyMajor

  =SEEK('ST'+lcFromStyle)
  SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'ST'+lcFromStyle
    IF NOT (lcNotFound $ CKEY)
      SELECT PROFVALU
      SCATTER MEMVAR MEMO
      m.CKEY = STRTRAN(UPPER(M.CKEY),lcFromStyle,lcStyMajor)

      SELECT (lcColorFil)
      IF !SEEK(ALLTRIM(m.CKEY))
        LOOP
      ENDIF

      SELECT PROFVALU
      m.STATUS = "A"
      m.cPro_Desc = gfCodDes(m.CPRO_CODE,'CPRO_CODE ')
      SELECT (lcprofilevalue)
      APPEND BLANK
      GATHER MEMVAR MEMO

      && Copy Profile List values if Exist, to be used in the screen sysetprf Display
      IF SEEK(m.CPRO_CODE,'PROFLIST')
        SELECT PROFLIST
        SCAN REST WHILE CPRO_CODE+CPRO_VALUE = m.CPRO_CODE
          SCATTER MEMVAR
          m.STATUS = "S"
          m.OldValue = m.CPRO_VALUE
          INSERT INTO (lcprofilelist) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDIF
  ENDSCAN
  SELECT STYLE
ENDIF

IF LLDONTSAVEPROFILE = .T.

  LLDONTSAVEPROFILE = .F.
  IF USED(lcprofilevalue)
    SELECT (lcprofilevalue)
    llModified = .F.
    SCAN
      IF STATUS $ 'MA'
        llModified = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF llModified = .T.
      IF  llMScale AND llAllScales
        REPLACE CKEY   WITH PADR(lcStyMajor,lnstylewid)+lcsepart+lcNonMajor  ALL
      ELSE
        REPLACE CKEY   WITH lcStyleKey  ALL
      ENDIF
      REPLACE STATUS WITH "A" ALL
      LCPROFILEKEY ="LCSTYLEKEY"
    ENDIF
  ENDIF
  SELECT STYLE
ENDIF
IF lcActivemode ='E' AND !Style.lDetCost  
  lnTotOldCost = OLDVAL('TotCost','Style')
  lcOldMode = lcActivemode 
ENDIF

IF !LLALLCOLORS AND !LLALLSCALES AND lextendedattributes AND lcActiveMode ='E'
  lfSaveAttributes(.F.,LCSTYLE)
ENDIF  

*!*************************************************************
*! Name      : lfSaveAttributes
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save attributes
*!*************************************************************
FUNCTION lfSaveAttributes
LPARAMETERS llfromDelete,lcStyle

IF !USED('STYLEATTRIBUTES')
  =gfOpenTable('STYLEATTRIBUTES','STYATTR')
ENDIF
IF !USED('STYLESIZEATTRIBUTES')
  =gfOpenTable('STYLESIZEATTRIBUTES','STYSATTR')
ENDIF

llHasStyAtt = GFSEEK(LCSTYLE,"STYLEATTRIBUTES")
llHasStySzAtt = GFSEEK(LCSTYLE,"STYLESIZEATTRIBUTES")
IF llHasStyAtt 
  SELECT STYLEATTRIBUTES
  =gfDelete()
ENDIF
IF llHasStySzAtt 
  SELECT STYLESIZEATTRIBUTES
  SCAN REST WHILE STYLE + sizecode = LCSTYLE
     =gfDelete()
  ENDSCAN
ENDIF
SELECT "STYLEATTRIBUTES"
=gfTableUpdate()
SELECT "STYLESIZEATTRIBUTES"
=gfTableUpdate()
IF llfromDelete
  RETURN 
ENDIF
*Saving Color Attribute [Start]
SELECT 'ColorAtt'
LOCATE
SCAN FOR !lSystem
  lcAttrName = ColorAtt.cfld_name
  
  IF !llHasStyAtt OR !SEEK(LCSTYLE,'STYLEATTRIBUTES')
    SELECT STYLEATTRIBUTES
    APPEND BLANK 
    REPLACE Style WITH LCSTYLE
    llHasStyAtt= .T.
  ENDIF
  *C(1),Length N(5),Decimals N(3)
  REPLACE &lcAttrName. WITH IIF(ColorAtt.Type ='M',ColorAtt.mAttributeValue,IIF(ColorAtt.Type ='C',ColorAtt.AttributeValue,IIF(ColorAtt.Type ='N',ColorAtt.nAttributeValue,IIF(ColorAtt.Type ='D',CTOD(ColorAtt.AttributeValue),ColorAtt.AttributeValue)))) IN 'STYLEATTRIBUTES'
  IF  ColorAtt.measured
    lcUomFld = IIF(LEN(ALLTRIM(ColorAtt.cfld_name))<= 27,ALLTRIM(ColorAtt.cfld_name)+"UOM",SUBSTR(ALLTRIM(ColorAtt.cfld_name),1,27)+"UOM")
    REPLACE &lcUomFld. WITH  ColorAtt.cUomCode IN 'STYLEATTRIBUTES'
  ENDIF
ENDSCAN
SELECT 'STYLEATTRIBUTES'
 =gfReplace('')
SELECT 'ColorAtt'
LOCATE
SCAN FOR !lSystem AND ColorAtt.Type ='M'
  lcAttrName = ColorAtt.cfld_name
  SELECT 'STYLEATTRIBUTES'
  =gfReplace(''+lcAttrName+' WITH "'+ColorAtt.mAttributeValue+'"')
ENDSCAN
*Saving Color Attribute [End]

SELECT 'SzClrAtt'
LOCATE 
SCAN FOR !lSystem
  lcSzFldName = SzClrAtt.cfld_name
  lcSzAttName = SzClrAtt.attribute
  =gfSeek('S'+Style.Scale,'Scale','Scale')
  FOR lnA = 1 TO Scale.Cnt
    lcA =STR(lnA,1) 
    IF !SEEK(LCSTYLE+lcA,"STYLESIZEATTRIBUTES")
      SELECT STYLESIZEATTRIBUTES
      APPEND BLANK 
      replace style  WITH LCSTYLE ,;
              sizecode WITH lcA IN 'STYLESIZEATTRIBUTES'
    ENDIF 
    REPLACE &lcSzFldName. WITH IIF(SzClrAtt.Type ='C',SzClrAtt.sz&lcA.,IIF(SzClrAtt.Type ='N',SzClrAtt.nsz&lcA.,IIF(SzClrAtt.Type ='D',CTOD(SzClrAtt.sz&lcA.),SzClrAtt.sz&lcA.))) IN 'STYLESIZEATTRIBUTES'
    IF SzClrAtt.measured
      lcSzUomFld = IIF(LEN(ALLTRIM(SzClrAtt.cfld_name))<= 27,ALLTRIM(SzClrAtt.cfld_name)+"UOM",SUBSTR(ALLTRIM(SzClrAtt.cfld_name),1,27)+"UOM") 
      REPLACE &lcSzUomFld. WITH  SzClrAtt.cSzUomCode IN 'STYLESIZEATTRIBUTES'
    ENDIF
    SELECT 'STYLESIZEATTRIBUTES'
    =gfReplace('')
  ENDFOR 
ENDSCAN 
SELECT "STYLEATTRIBUTES"
=gfTableUpdate()
SELECT "STYLESIZEATTRIBUTES"
=gfTableUpdate()

*!*************************************************************
*! Name      : lfGetSegmentinfo
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Get Style segment info.
*!*************************************************************
FUNCTION lfGetSegmentinfo
lnAlias=SELECT()
llStruOp=gfOpenTable(oAriaApplication.DataDir+'ICISTRU','Segno','SH')
IF !gfSeek('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    gfCloseTable([ICISTRU])
  ENDIF
  RETURN .F.
ENDIF


oGetItemMask = CREATEOBJECT('GetItemMask')
oGetItemMask.Do(@laMajSeg)
lnMajSeg  = oGetItemMask.Do('SM')   &&Number of major segments.
lnNonSeg  = oGetItemMask.Do('SN')   &&Number of non major segments
lcItemTl  = oGetItemMask.Do('HI')   &&Item Title
lcMjrTtl  = oGetItemMask.Do('HM')   &&Major part tille
lcNMjrTl  = oGetItemMask.Do('HN')   &&Non Major part title
lcMjrPct  = oGetItemMask.Do('PM')   &&Major picture
lcNMjrPt  = oGetItemMask.Do('PN')   &&Non Major picture
lcIMjrPt  = oGetItemMask.Do('PI')   &&Item pictue
lcstylemask = oGetItemMask.Do("M")
PRIVATE lnSegVal
FOR lnSegVal=1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnSegVal,1]="C"
    lncolorwid1=LEN(laMajSeg[lnSegVal,3])
  ENDIF
ENDFOR
lnstylewid=LEN(lcMjrPct)       &&Major Part lenth
lncolorwid=LEN(lcNMjrPt)       &&Non Major Part lenth
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)   &&Major-Nonmajor separator.
llFreeExst = .F.   &&Non Major free segment existance flag.
lnFSegNo   = 0     &&Non Major free segment number.
lcFSgPct   = ""    &&Non Major free segment picture.

 
*--Fill Item structure array.
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DIMENSION laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M' 
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N' 
    ELSE
      laSegInfo[lnCnt,1] = ' ' 
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL', IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),IIF(laMajSeg[lnCnt,1]='S','S','NN'))
  
  IF laMajSeg[lnCnt,1]$'CZDGF'
    DO CASE
      CASE laSegInfo[lnCnt,3]='C'
        lcCdTyp = 'COLOR     '
      CASE laSegInfo[lnCnt,3]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSegInfo[lnCnt,3]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSegInfo[lnCnt,3]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSegInfo[lnCnt,3]='F'
        lcCdTyp  =''
        lcFreeDc = laMajSeg[lnCnt,2]
    ENDCASE    
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laMajSeg[lnCnt,1]$'FOQT',"ciSegVal",IIF(laMajSeg[lnCnt,1]='S',"Scale","cCode_no"))

  *--Checking for free segments in Non Major.
  IF laSegInfo[lnCnt,1] = 'N'  AND  laSegInfo[lnCnt,3] = 'F'
    lcFSgPct   = REPLICATE('X',laSegInfo[lnCnt,5])
    lnFSegNo   = lnCnt - lnMajSeg
    llFreeExst = .T.
  ENDIF
  IF llMScale AND laMajSeg[lnCnt,1] = 'S'
    lnscaleln  = LEN(laMajSeg[lnCnt,3])
    lnscalepos = laMajSeg[lnCnt,4]
  ENDIF
ENDFOR


*--If extended size scale, check on scale separator and scale separator posision.
IF llMScale
  lcSclSepr =IIF(SUBSTR(lcIMjrPt,laSegInfo[ALEN(laSegInfo,1),4]-1,1)<>'X',;
        SUBSTR(lcIMjrPt,laSegInfo[ALEN(laSegInfo,1),4]-1,1),'')
  SELECT ICISTRU
  gfSeek('')
  LOCATE FOR cItemRecty='U' AND cISegType='S'
  IF FOUND()
    SKIP -1
    lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
  ELSE
    lnSepMins = 3
  ENDIF
ELSE
  lnSepMins = 0
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfSaveBom
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save BOM
*!*************************************************************
FUNCTION lfSaveBom
PARAMETERS llDetailCosting, nClrNo,llCostPrv,lcStyMajor,lcBomHeadr,lcBom,lcPriceCurr ,lcDutyCurr  ,lnPriceRate ,lnDutyRate,laMCost,laICost,lncurrunt1,lncurrunt2  

LOCAL lnBOMUpdated

lcItmMajor = PADR(lcStyMajor,19)

IF !llDetailCosting
  *-- Call function to add MFGCODE if not found in CODES file
  *-- MFGCODE Will be added as (*1 *2 *3 *4 *5)
  SELECT CODES
  lcCodesTag = TAG()
  gfSetOrder([Ccode_no])
  FOR lnCdCount = 1 TO 7
    SELECT CODES
    lcCdCount = ALLTRIM(STR(lnCdCount))
    IF !gfSeek('N'+PADR('MFGCODE',10)+PADR('*'+lcCdCount,6))
      FOR lnI=1 TO 8
        
        gfAppend()
        m.CDISCREP = laICost[lnCdCount,2]
        gfReplace([CDEFCODE    WITH 'N'                     ,]+;
                  [CFLD_NAME   WITH 'MFGCODE'               ,]+;
                  [CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount) ,]+;
                  [CDISCREP    WITH m.CDISCREP              ,]+;
                  [LRLTFIELDS  WITH .F.                     ,]+;
                  [CRLTFIELD   WITH IIF(lnI=1,'N','Y')])

        
        IF lnI>1
          lcRFValue = IIF(lnI=6,'0',IIF(lnI=7 OR lnI=8,'F',''))
          lcRFtype = IIF(lnI=6,'N',IIF(lnI=7 OR lnI=8,'L','C'))
          DO CASE
            CASE lnI=2
              lcRFName = 'CCONTCODE'
            CASE lnI=3
              lcRFName = 'CCONTNAME'
            CASE lnI=4
              lcRFName = 'COPERSEQ'
            CASE lnI=5
              lcRFName = 'GLACCOUNT'
            CASE lnI=6
              lcRFName = 'LEADTIME'
            CASE lnI=7
              lcRFName = 'LINHOUSE'
            CASE lnI=8
              lcRFName = 'LMFGOPR'
          ENDCASE
          gfReplace([CRLTD_NAM   WITH lcRFName,]+;
                    [CRLTD_TYP   WITH lcRFtype,]+;
                    [CRLTD_VLU   WITH lcRFValue])
        ENDIF
        =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'CODES')].lcCursorUpdate )
      ENDFOR
    ENDIF
  ENDFOR
  SELECT Codes
  gfSetOrder(lcCodesTag)
ENDIF
IF !llDetailCosting
  lnBOMUpdated = lfUpdateBomHdr(llDetailCosting,lcBomHeadr,lcStyMajor)
  IF lnBOMUpdated <> -1 && -1: Record not found in BOMHEADR & not saved because no costs found
    lfUpdateBom(lcItmMajor,llDetailCosting,IIF(Style.Make, "M", "I"), nClrNo,llCostPrv,lcPriceCurr ,lcDutyCurr  ,lnPriceRate ,lnDutyRate,lcBom,@laMCost,@laICost,lncurrunt1,lncurrunt2)
  ENDIF
ENDIF
*-- [End] WSH
SELECT STYLE
RETURN

*!*************************************************************
*! Name      : lfUpdateBomHdr
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Update BOMHEADR
*!*************************************************************
FUNCTION lfUpdateBomHdr
LPARAMETERS llDetailCosting,lcBomHeadr,lcStyMajor
SELECT (lcBomHeadr)  &&Index : CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID
LOCAL llRetVal

*Add the default record for Imported Cost Sheet
llRetVal = lfAddDefCstSht(PADR(lcStyMajor,19),"I",llDetailCosting,lcBomHeadr,lcStyMajor)

IF llRetVal <> -1
    *Add the default record for Manufactured Cost Sheet
   llRetVal = lfAddDefCstSht(PADR(lcStyMajor,19),"M",llDetailCosting,lcBomHeadr,lcStyMajor)
ENDIF
RETURN llRetVal

*!*************************************************************
*! Name      : lfAddDefCstSht
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Add default cost sheet
*!*************************************************************
FUNCTION lfAddDefCstSht
LPARAMETERS lcStyle2Update,lcCostShetType,llDetailCosting,lcBomHeadr,lcStyMajor

LOCAL lnTotCosts, i, lcI, llRet
*! B612610,1 MMT 08/02/2022 Modify the lfSaveBOM in style screen saving function to use SEEK and Locate REST instead of LOCATE FOR{T20220729.0001}[Start]
*LOCATE FOR CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = lcStyle2Update+lcCostShetType;
           AND lDefCstSht
=SEEK(lcStyle2Update+lcCostShetType)
LOCATE REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = lcStyle2Update+lcCostShetType;
           FOR lDefCstSht           
*! B612610,1 MMT 08/02/2022 Modify the lfSaveBOM in style screen saving function to use SEEK and Locate REST instead of LOCATE FOR{T20220729.0001}[End]
IF !FOUND()
  * Check if User Entered Costs in Costs Tab Page
  lnTotCosts = 0
  FOR i = 1 TO 7
    lcI = STR(i, 1)
    lnTotCosts = lnTotCosts + IIF(Style.Make, Style.nMCost&lcI, Style.nICost&lcI)
    IF i > 1
      lnTotCosts = lnTotCosts + Style.nPrCost&lcI
    ENDIF
  ENDFOR
  IF lnTotCosts <= 0
    *** RETURN -1  && Def. Cost Sheet not Found and not Added
    llRet = 1  && Def. Cost Sheet not Found and added
  ENDIF
  m.cinvtype    = '0001'
  m.cItmMajor   = lcStyMajor
  m.cCstSht_Id  = IIF(llDetailCosting,"","DEFCST")
  m.cCstShtDsc  = "Default cost sheet"
  m.cCstShtTyp  = lcCostShetType
  m.lDefCstSht  = .T.
  m.cStatus     = "A"
  INSERT INTO (lcBomHeadr) FROM MEMVAR
  llRet = 1  && Def. Cost Sheet not Found and added
ELSE
  IF EMPTY(cCstSht_Id) AND !llDetailCosting
    SCATTER MEMVAR MEMO 
    DELETE 
    APPEND BLANK 
    GATHER MEMVAR MEMO
    REPLACE cCstSht_Id WITH "DEFCST"
  ENDIF
  llRet = 0  && Def. Cost Sheet Found
ENDIF


lnTotCosts = 0
FOR I = 1 TO 7
  lcI = STR(I, 1)
  REPLACE nCost&lcI WITH IIF(CCSTSHTTYP = 'M', Style.nMCost&lcI, Style.nICost&lcI)
  lnTotCosts = lnTotCosts + nCost&lcI
ENDFOR
REPLACE TotCost WITH lnTotCosts

RETURN llRet
*-- [End] WSH

*!*************************************************************
*! Name      : lfUpdateBom
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Update BOM
*!*************************************************************
FUNCTION lfUpdateBom
PARAMETERS lcItmMajor,llDetailCosting,lcCostType,nClrNo,llCostPrv,lcPriceCurr ,lcDutyCurr  ,lnPriceRate ,lnDutyRate ,lcBom ,laMCost,laICost,lncurrunt1,lncurrunt2
PRIVATE lnOldAlais
IF !llCostPrv
   RETURN
ENDIF

lnOldAlais = SELECT(0)
SELECT (lcBom)
SET ORDER TO TAG BOM
GO TOP

LOCAL lnI, lcI   && WSH

FOR lnI = 1 TO 7
  SELECT (lcBom)
  lcI = STR(lnI,1)
  llUpdBom = .F.
  *! B612610,1 MMT 08/02/2022 Modify the lfSaveBOM in style screen saving function to use SEEK and Locate REST instead of LOCATE FOR{T20220729.0001}[Start]
  *LOCATE FOR CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK = lcCostType+"DEFCST"+lcI+STYLE.Style
  =SEEK(PADR(lcItmMajor,19)+lcI+STYLE.Style)
  LOCATE REST WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM = PADR(lcItmMajor,19)+lcI++STYLE.Style FOR CCSTSHT_ID=CCSTSHT_ID AND CCSTSHTTYP = lcCostType
  *! B612610,1 MMT 08/02/2022 Modify the lfSaveBOM in style screen saving function to use SEEK and Locate REST instead of LOCATE FOR{T20220729.0001}[End]
  IF !FOUND()
    IF IIF(!STYLE.Make,STYLE.nICost&lcI,STYLE.nMCost&lcI) > 0
      APPEND BLANK
      llUpdBom = .T.
    ENDIF
  ELSE
    llUpdBom = .T.
  ENDIF
  IF llUpdBom

    LOCAL lcUOMRel
    lcUOMRel = SPACE(6)
    
    =gfGetUOMData(@lcUOMRel, '', '', 1, .F.)
    SELECT (lcBom)
    REPLACE cItmMajor  WITH lcItmMajor,;
            Typ        WITH lcI,;
            cItmMask   WITH STYLE.Style,;
            MFGCODE    WITH PADR('*'+lcI,LEN(MfgCode)),;
            nLineNo    WITH (nClrNo - 1) * 7 + lnI,;
            cUOMCode   WITH lcUOMRel,;
            UntCost    WITH IIF(!STYLE.Make,STYLE.nICost&lcI,STYLE.nMCost&lcI),;
            nBomTotQty WITH 1,;
            nestbomqty WITH 1,;
            desc       WITH IIF(!STYLE.Make,laICost[lnI,2],laMCost[lnI,2]),;
            TotCost    WITH UntCost,;
            cCatgTyp   WITH IIF(!STYLE.Make,laICost[lnI,1],laMCost[lnI,1])
    REPLACE cCUrrCode  WITH IIF(cCatgTyp='P',lcPriceCurr,lcDutyCurr) ,;
            nExRate    WITH IIF(cCatgTyp='P',lnPriceRate,lnDutyRate) ,;
            nCurrUnit  WITH IIF(cCatgTyp='P',lncurrunt1,lncurrunt2) ,;
            cBasedOn   WITH 'A' ,;
            cCostType  WITH 'S' ,;
            cItemLevel WITH 'S' ,;
            cUsage     WITH 'O' ,;
            lCostItem  WITH .T. ,;
            lEditValue WITH .T.
    REPLACE  cinvtype    WITH "0001",;
             cCstSht_Id  WITH "DEFCST",;
             cCstShtTyp  WITH lcCostType
    =gfAdd_info(lcBom)
  ENDIF
ENDFOR
SELECT(lnOldAlais)

*!*************************************************************
*! Name      : lfSaveLocations
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save Locations
*!*************************************************************
FUNCTION lfSaveLocations
LPARAMETERS lcWareFil,lcStyleKey,lcActiveMode,llUseStyleConfiguration ,lcDefConfCode,loFormSet
*--Update STYDYE file with added/modified or removed locations for the Saveed Styles
IF !USED(lcWareFil)
  lfvWareHouse(lcStyleKey,.F.)
ENDIF

SELECT (lcWareFil)
SET FILTER TO
GO TOP
SCAN
  SCATTER MEMVAR
  SELECT STYLE
  =SEEK(lcStyleKey)
  SCAN REST WHILE Style = lcStyleKey
    m.Style = Style
    m.Ave_cost = Ave_cost
    SELECT STYDYE
    IF !gfSeek(m.Style+m.cWareCode+SPACE(10))
      IF gfSeek(m.Style) OR lcActiveMode = 'A' OR m.cWareCode = Style.cDefWare
        IF m.cStatus # 'D'
          gfAppend()
          gfReplace([Style     WITH m.Style,;
                  Desc      WITH Style.Desc,;
                  cWareCode WITH m.cWareCode,;
                  Ave_Cost  WITH m.Ave_Cost,;
                  Gl_link   WITH m.gl_link,;
                  cDiscCode WITH m.cDiscCode])
          =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), ALIAS())].lcCursorUpdate)
          *-- Create a record in StyDye for every Style-Color, WareHouse, & Default Configuration
          IF llUseStyleConfiguration AND Style.cdye_flg = 'Y' AND !EMPTY(lcDefConfCode)
             gfAppend()
             m.Dyelot = lcDefConfCode
             gfReplace([Style     WITH m.Style,]+;
                       [cWareCode WITH m.cWareCode,]+;
                       [Ave_Cost  WITH m.Ave_Cost,]+;
                       [dyelot    WITH m.Dyelot,]+;
                       [Desc      WITH Style.Desc,]+;
                       [Gl_link   WITH m.gl_link,]+;
                       [cDiscCode WITH m.cDiscCode])
             =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), ALIAS())].lcCursorUpdate)
          ENDIF
          *-- [End] WSH
          *--Run custom trigger for saving locations.
          IF TYPE('loFormSet') ='O'
            loFormSet.mDoTrigger(PADR('SAVE_WARE',10))  
          ENDIF  
        ENDIF  && m.cStatus # 'D'
      ENDIF    && ThisFormSet.ActiveMode = 'A' OR m.cWareCode = Style.lDefWare
    ELSE  && Exist
      IF m.cStatus # 'D'
        gfReplace([Ave_Cost  WITH IIF(m.cStatus='A',m.Ave_Cost,Ave_Cost),]+;
                  [Gl_link   WITH m.gl_link,]+;
                  [cDiscCode WITH m.cDiscCode])
        =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), ALIAS())].lcCursorUpdate)
        *-- [Start], WSH Create a record in StyDye for every Style-Color, WareHouse, & Default Configuration
        IF llUseStyleConfiguration AND Style.cdye_flg = 'Y' AND !EMPTY(lcDefConfCode)
           SCATTER TO MEMVAR
           gfSeek(m.Style+m.cWareCode)
           LOCATE FOR Style+cWareCode+dyelot = m.Style+m.cWareCode AND !EMPTY(dyelot)
           IF !FOUND()
              gfAppend(' IN STYDYE', .T.)
              m.Dyelot = lcDefConfCode
              gfReplace([dyelot WITH m.Dyelot])
           ENDIF
        ENDIF
      ELSE
        SCATTER FIELDS stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8 TO laWarStk
        gfDelete([REST WHILE Style+cWareCode = m.Style+m.cWareCode])
        SELECT STYLE
        gfReplace("Stk1   WITH Stk1   - laWarStk[1] ,"+;
                  "Stk2   WITH Stk2   - laWarStk[2] ,"+;
                  "Stk3   WITH Stk3   - laWarStk[3] ,"+;
                  "Stk4   WITH Stk4   - laWarStk[4] ,"+;
                  "Stk5   WITH Stk5   - laWarStk[5] ,"+;
                  "Stk6   WITH Stk6   - laWarStk[6] ,"+;
                  "Stk7   WITH Stk7   - laWarStk[7] ,"+;
                  "Stk8   WITH Stk8   - laWarStk[8] ,"+;
                  "Totstk WITH Stk1+Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8")
      ENDIF
      *--Run custom trigger for saving locations.
      IF TYPE('loFormSet') ='O'
        loFormSet.mDoTrigger(PADR('SAVE_WARE',10))  
      ENDIF  
    ENDIF
  ENDSCAN
ENDSCAN

*!*************************************************************
*! Name      : lfSaveCosting
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save Costing
*!*************************************************************
FUNCTION lfSaveCosting
LPARAMETERS lcPColor,lcPScale,lcBOM,lnSclNo,lcStyMajor,laSegInfo,llMulcurr,loFormSet

LOCAL lnConv

LOCAL lcItem

lcItem = gfTempName()
llMAInstld = (OCCURS('MA',oAriaApplication.CompanyInstalledModules)<>0)
STORE 0.00 TO lncostval1,lncostval2,lncostval3,lncostval4,lncostval5,lnCostVal6,lnCostVal7

*--Check if the color segment exist and get the segno.
lnClrPnt=0
FOR lnI=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[lnI,1]='N' AND laSegInfo[lnI,3]='C'
    lnClrPnt=lnI
    EXIT
  ENDIF
ENDFOR
lcMajor = lcStyMajor
SELECT (lcBom)
SCAN REST WHILE &lcBom..cItmMajor = lcMajor ;
     FOR LIKE(STRTRAN(cItmMask,'*','?'),lcMajor+lcSepart+lcPColor) AND (lcPScale $ mSizes OR EMPTY(mSizes))
  lccurvar = 'lnCostVal' + Typ
  lnNoOfSizes = OCCURS(MLINE(mSizes,ATCLINE(lcPScale,mSizes)),',')
  lnNoOfSizes = lnNoOfSizes + IIF(lnNoOfSizes=0,lnSclNo,1)
  DO CASE
    CASE llMAInstld AND (&lcBom..ccatgtyp = 'F' OR (&lcBom..ccatgtyp = 'T' AND &lcBom..trim_invt))       && FABRIC
      lcCmpClr = IIF(lnClrPnt <> 0, SUBSTR(cItmMask,laSegInfo[lnClrPnt,4],laSegInfo[lnClrPnt,5]),"")
      *****************************************************************************
      lnConv = 1
      =gfGetUOMData(EVALUATE(lcBOM + '.cUOMCode'), '', '', @lnConv)
      *****************************************************************************
      *--Get Segment Information for the BOM Component
      LOCAL oGetItemMask, lnFabClrStart, lnFabClrLen, llFabClrFound
      DIMENSION laMajSeg[1,1]
      llFabClrFound = .F.
      oGetItemMask = CREATEOBJECT('GetItemMask')
      oGetItemMask.Do(@laMajSeg, '', &lcBOM..cInvTypC)
      FOR lnI = 1 TO ALEN(laMajSeg, 1)
        IF laMajSeg[lnI,1] = 'C'
          lnFabClrStart = laMajSeg[lnI, 4]
          lnFabClrLen   = LEN(ALLTRIM(laMajSeg[lnI, 3]))
          llFabClrFound = .T.
        ENDIF
      ENDFOR

      *--Open Item File
      LOCAL lcCond
      lcCond = "cInvType = '" + &lcBOM..cInvTypC + "' AND " +;
                "Style LIKE '" + SUBSTR(&lcBom..Item, laMajSeg[1,4], LEN(ALLTRIM(laMajSeg[1,3]))) + "%'"
      
      IF !lfOpenSQL('ITEM',lcItem,;
                               'cInvType+Style|',;
                               'INVITEM|',;
                               lcCond,;
                               'Style')
        RETURN .F.
      ENDIF

      *****************************************************************************
      IF AT('*',lcCmpClr) <> 0  AND SUBSTR(&lcBom..Item, lnFabClrStart, 1) = '*'
        IF llFabClrFound AND !SEEK(&lcBOM..cInvTypC + SUBSTR(&lcBom..Item, laMajSeg[1,4], LEN(ALLTRIM(laMajSeg[1,3]))) + '-' + lcPColor, lcItem)
          *--Fabric/Color ' +item)+'/'+color+' is required for costing.
          *--It will be added to the material file,
          *--Please update the style cost sheet after adding this color.
          =gfModalGen('TRM42019B42001','DIALOG',SUBSTR(&lcBom..Item, laMajSeg[1,4], LEN(ALLTRIM(laMajSeg[1,3])))+'|'+ALLTRIM(lcPColor))
          SELECT (lcItem)
          SEEK &lcBOM..cInvTypC + SUBSTR(&lcBom..Item, laMajSeg[1,4], LEN(ALLTRIM(laMajSeg[1,3])))
          SCATTER MEMVAR MEMO
          APPEND BLANK
          GATHER MEMVAR MEMO

          REPLACE cInvType   WITH &lcBOM..cInvTypC,;
                  Style      WITH SUBSTR(&lcBom..Item, laMajSeg[1,4], LEN(ALLTRIM(laMajSeg[1,3]))) + laMajSeg[1,6] + lcPColor,;
                  nStkVal    WITH 0,;
                  NREORDER1  WITH 0,;
                  NTOTREORD  WITH 0,;
                  cAdd_user  WITH " "

          REPLACE NTOTHUSAGE WITH 0,;
                  NTOTCUSA   WITH 0,;
                  TOTORD     WITH 0,;
                  TOTWIP     WITH 0,;
                  TOTSTK     WITH 0,;
                  NTOTWO     WITH 0,;
                  TOTSHP     WITH 0,;
                  TOTALO     WITH 0,;
                  TOTINTRN   WITH 0,;
                  TOTRET     WITH 0,;
                  TOTRA      WITH 0,;
                  NREORDER1  WITH 0

          *-- Get The Fabric Real Cost values [Start]
          REPLACE AVE_COST   WITH (m.nMCost1 + m.nMCost2 + m.nMCost3 + m.nMCost4),;
                  nAveCstBuy WITH AVE_COST
          =gfAdd_Info(lcItem)
          SELECT (lcBom)
        ENDIF

        &lccurvar = &lccurvar+ROUND(&lcBom..nBomTotqty*&lcItem..TOTCOST/lnConv,3)*lnNoOfSizes

      ELSE

        IF SEEK(&lcBom..cInvTypC + &lcBom..Item, lcItem)
          &lccurvar = &lccurvar+ROUND(&lcBom..nBomTotqty*&lcItem..TOTCOST/lnConv,3)*lnNoOfSizes
         ENDIF
      ENDIF

      *--Saving Item File
      LOCAL lnConnectionHandlar, lcTranCode

      IF TYPE("loconnstring") <> 'O'  
        loconnstring = CREATEOBJECT('remotedataaccess')  
      ENDIF 
      lcTranCode = loconnstring.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

      IF TYPE('lcTranCode') = 'N'
        =loconnstring.CheckRetResult("BeginTran",lcTranCode,.T.)
        RETURN .F.
      ENDIF

      SELECT (lcItem)
      *-- Start update the POSLN with the deleted records
      lnConnectionHandlar = loconnstring.SQLUpdate(lcItem,lcTranCode,SET("Datasession"),"CINVTYPE,STYLE")
      IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
        =loconnstring.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
        llReturn = .F.
      ENDIF

      lnConnectionHandlar = loconnstring.CommitTran(lcTranCode)
      IF lnConnectionHandlar # 1
        =loconnstring.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
        Return .F.
      ENDIF

    CASE (ccatgtyp = 'T' AND !trim_invt) OR ccatgtyp $ 'MDP'
      &lccurvar = &lccurvar+&lcBom..totcost*lnNoOfSizes
    CASE ccatgtyp = 'S'  && STYLE COMPONENT
      lcCmpClr=IIF(lnClrPnt<>0,SUBSTR(Item,laSegInfo[lnClrPnt,4],laSegInfo[lnClrPnt,5]),"")
      IF lnClrPnt<>0 OR AT('*',lcCmpClr)=0 
        lcSTToSeek=&lcBom..Item
        FOR I=1 TO LEN(lcIMjrPt)
          IF SUBSTR(lcSTToSeek,I,1)='*'
            lcSTToSeek=SUBSTR(lcSTToSeek,1,I-1)+SUBSTR(lcMajor+lcSepart+lcPColor,I,1)+SUBSTR(lcSTToSeek,I+1,LEN(lcIMjrPt)-I)
          ENDIF
        ENDFOR
        lcOldStyle = STYLE.Style
        IF !SEEK(lcSTToSeek,'STYLE','STYLE')
          =gfModalGen('TRM42257B42001','DIALOG',SUBSTR(&lcBom..Item, 1, lnStyleWid)+'|'+ALLTRIM(lcPColor))
          SELECT STYLE
          =SEEK(SUBSTR(&lcBom..Item, 1, lnStyleWid))
          SCATTER MEMVAR MEMO
          gfAppend(' IN Style', .T.)
          m.Style = SUBSTR(&lcBom..Item, 1, lnStyleWid) + lcSepart + lcPColor
          gfReplace([Style    WITH m.Style,]+;
                  [stk1     WITH 0, stk2    WITH 0, stk3    WITH 0, stk4    WITH 0,stk5     WITH 0, stk6    WITH 0, stk7    WITH 0, stk8    WITH 0,]+;
                  [alo1     WITH 0, alo2    WITH 0, alo3    WITH 0, alo4    WITH 0,alo5     WITH 0, alo6    WITH 0, alo7    WITH 0, alo8    WITH 0,]+;
                  [wip1     WITH 0, wip2    WITH 0, wip3    WITH 0, wip4    WITH 0,wip5     WITH 0, wip6    WITH 0, wip7    WITH 0, wip8    WITH 0,]+;
                  [ord1     WITH 0, ord2    WITH 0, ord3    WITH 0, ord4    WITH 0,ord5     WITH 0, ord6    WITH 0, ord7    WITH 0, ord8    WITH 0,]+;
                  [shp1     WITH 0, shp2    WITH 0, shp3    WITH 0, shp4    WITH 0,shp5     WITH 0, shp6    WITH 0, shp7    WITH 0, shp8    WITH 0,]+;
                  [ret1     WITH 0, ret2    WITH 0, ret3    WITH 0, ret4    WITH 0,ret5     WITH 0, ret6    WITH 0, ret7    WITH 0, ret8    WITH 0,]+;
                  [ra1      WITH 0, ra2     WITH 0, ra3     WITH 0, ra4     WITH 0,ra5      WITH 0, ra6     WITH 0, ra7     WITH 0, ra8     WITH 0,]+;
                  [nwo1     WITH 0, nwo2    WITH 0, nwo3    WITH 0, nwo4    WITH 0,nwo5     WITH 0, nwo6    WITH 0, nwo7    WITH 0, nwo8    WITH 0,]+;
                  [intrans1 WITH 0,intrans2 WITH 0,intrans3 WITH 0,intrans4 WITH 0,intrans5 WITH 0,intrans6 WITH 0,intrans7 WITH 0,intrans8 WITH 0,]+;
                  [totstk   WITH 0, totalo  WITH 0, totwip  WITH 0, totord  WITH 0,totshp   WITH 0, totret  WITH 0, totra   WITH 0, ntotwo  WITH 0,Totintrn WITH 0,]+;
                  [dmgqty   WITH 0, nstkval WITH 0])
          =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'STYLE')].lcCursorUpdate)
          
          SELECT StyDye
          gfAppend()
          gfReplace([Style     WITH Style.Style,;
                  Desc      WITH Style.Desc,;
                  cWareCode WITH Style.cDefWare,;
                  Ave_Cost  WITH Style.ave_cost,;
                  Gl_link   WITH Style.gl_link,;
                  cDiscCode WITH Style.cDiscCode])
          =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'STYDYE')].lcCursorUpdate)
        ENDIF
        &lccurvar = &lccurvar+ROUND(&lcBom..nBomTotqty*STYLE.TotCost,3)*lnNoOfSizes
        SELECT STYLE
        SEEK lcOldStyle
      ENDIF
  ENDCASE
ENDSCAN

FOR lnCount = 1 TO 7
  lcCount = STR(lnCount,1)
  lncostval&lcCount = ( lncostval&lcCount / lnSclNo )
ENDFOR

lnCostPrc2=IIF(lnCostval1=0,0,(lnCostVal2/lnCostVal1)*100)
lnCostPrc3=IIF(lnCostval1=0,0,(lnCostVal3/lnCostVal1)*100)
lnCostPrc4=IIF(lnCostval1=0,0,(lnCostVal4/lnCostVal1)*100)
lnCostPrc5=IIF(lnCostval1=0,0,(lnCostVal5/lnCostVal1)*100)
lnCostPrc6=IIF(lnCostval1=0,0,(lnCostVal6/lnCostVal1)*100)
lnCostPrc7=IIF(lnCostval1=0,0,(lnCostVal7/lnCostVal1)*100)


*--Update the TotCost and AveCost.
lnTotCost = lnCostval1 + lnCostval2 + lnCostval3 + lnCostval4 + lnCostval5+lnCostVal6+lnCostVal7
SELECT STYLE

LOCAL lnCursLoc
lnCursLoc = gfGetRemoteTable(SET("Datasession"), 'STYLE')

SELECT (oAriaApplication.laRemoteTable[lnCursLoc].lcCursorUpdate)
  
REPLACE TotCost  WITH lnTotCost,;
        Ave_Cost WITH lnTotCost

IF !STYLE.Make
  REPLACE nICost1  WITH IIF(nICost1=0,IIF(!llMulcurr,lnCostval1,STYLE.nICost1),nICost1),;
          nICost2  WITH IIF(nICost2=0,IIF(!llMulcurr,lnCostval2,STYLE.nICost2),nICost2),;
          nICost3  WITH IIF(nICost3=0,IIF(!llMulcurr,lnCostval3,STYLE.nICost3),nICost3),;
          nICost4  WITH IIF(nICost4=0,IIF(!llMulcurr,lnCostval4,STYLE.nICost4),nICost4),;
          nICost5  WITH IIF(nICost5=0,IIF(!llMulcurr,lnCostval5,STYLE.nICost5),nICost5)
  REPLACE nICost6  WITH IIF(nICost6=0,IIF(!llMulcurr,lnCostval6,STYLE.nICost6),nICost6),;
          nICost7  WITH IIF(nICost7=0,IIF(!llMulcurr,lnCostval7,STYLE.nICost7),nICost7)

ELSE
  REPLACE nMCost1  WITH IIF(nMCost1=0,lncostval1,nMCost1),;
          nMCost2  WITH IIF(nMCost2=0,lncostval2,nMCost2),;
          nMCost3  WITH IIF(nMCost3=0,lncostval3,nMCost3),;
          nMCost4  WITH IIF(nMCost4=0,lncostval4,nMCost4),;
          nMCost5  WITH IIF(nMCost5=0,lncostval5,nMCost5)
  REPLACE nMCost6  WITH IIF(nMCost6=0,lncostval6,nMCost6),;
          nMCost7  WITH IIF(nMCost7=0,lncostval7,nMCost7)

ENDIF
REPLACE nprcost2  WITH IIF(nprcost2=0,lnCostPrc2,nprcost2),;
        nprcost3  WITH IIF(nprcost3=0,lnCostPrc3,nprcost3),;
        nprcost4  WITH IIF(nprcost4=0,lnCostPrc4,nprcost4),;
        nprcost5  WITH IIF(nprcost5=0,lnCostPrc5,nprcost5)
 REPLACE nprcost6  WITH IIF(nprcost6=0,lnCostPrc6,nprcost6),;
        nprcost7  WITH IIF(nprcost7=0,lnCostPrc7,nprcost7)
        

IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig ,PADR('CLRCOST',10))<>0 
  =loFormSet.mDoTrigger(PADR('CLRCOST' ,10))
ENDIF


*****************************************************************************************
IF USED(lcItem)
  USE IN (lcItem)
ENDIF
SELECT Style

*!*************************************************************
*! Name      : lfOpenSQL
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Open SQL
*!*************************************************************
FUNCTION lfOpenSQL
LPARAMETERS lcTable, lcCursor, lcIndex, lcTages, lcAdditionCondition, lcSearchIndex
LOCAL lnConnectionHandlar,lnBuffering, lcSqlStatment, lcWhereCond, lcTranCode
PRIVATE lcIndex1,lcIndex2,lcTages1
lcIndex1 = lcIndex
IF AT("|",lcIndex,1) > 0
  lcIndex1 = SUBSTR(lcIndex,1,AT("|",lcIndex,1)-1)
ENDIF
IF TYPE('lcSearchIndex') = 'C' AND !EMPTY(lcSearchIndex)
  lcSearchIndex = " WITH (Index = " + lcSearchIndex + ") "
ELSE
   lcSearchIndex = ''
ENDIF
lcWhereCond = IIF(!EMPTY(lcAdditionCondition), " WHERE " + lcAdditionCondition, "")
lcSQLStatment = "SELECT * FROM " + lcTable + lcSearchIndex + lcWhereCond 

IF TYPE("loconnstring") <> 'O'  
  loconnstring = CREATEOBJECT('remotedataaccess')
ENDIF 

DO WHILE .T.
  lnConnectionHandlar = loconnstring.sqlrun(lcSqlStatment,lcCursor,lcTable,;
                 oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("Datasession"))


  IF lnConnectionHandlar = 1
    lnBuffering = CURSORGETPROP("Buffering",lcCursor)
    =CURSORSETPROP("Buffering",3,lcCursor)
    SELECT (lcCursor)
    lcTages1 = lcTages
    lcIndex1 = lcIndex
    lnTempIndex = 1
    DO WHILE AT("|",lcIndex1,1) <> 0
      lcIndExp = SUBSTR(lcIndex1,1,AT("|",lcIndex1,1)-1)
      lcIndex1 = STRTRAN(lcIndex1,lcIndExp+"|","")
      lcTages = SUBSTR(lcTages1,1,AT("|",lcTages1,1)-1)
      lcTages1 = STRTRAN(lcTages1,lcTages+"|","")
       INDEX ON &lcIndExp. TAG (lcTages)
    ENDDO
    =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
    RETURN .T.
  ELSE
    =loconnstring.CheckRetResult("sqlrun",lnConnectionHandlar,.F.)
    IF MESSAGEBOX(LANG_CONNERRMSG, 5+16, LANG_CONNERRTTL) = 2
      RETURN .F.
    ENDIF
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfSaveStyFPrices
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save Style foreign prices
*!*************************************************************
FUNCTION lfSaveStyFPrices
LPARAMETERS lcActiveMode,lcTmpStyPr,lcstylekey
*--Update Style foreign prices, when Saving the style.
IF lcActiveMode='A' AND !USED(lcTmpStyPr)
  RETURN
ENDIF

IF !USED(lcTmpStyPr)
  lfGetForeignPrice(lcStyleKey,.F.,lcTmpStyPr,lcActiveMode)
ENDIF

SET DELETE OFF
SELECT (lcTmpStyPr)
SET ORDER TO TAG (lcTmpStyPr)
GO TOP 
SCAN FOR Style = lcstylekey
  SCATTER MEMVAR 

  SELECT STYLE
  =SEEK(lcStyleKey)

  SCAN REST WHILE Style = lcStyleKey FOR !DELETED('STYLE')
    m.Style = STYLE.Style
    *--Style price exist.
    IF gfSeek(m.Style+m.cCurrCode,'StyPrice') 
      SELECT STYPRICE
      LOCATE FOR STYLE+CCURRCODE = m.Style+m.cCurrCode AND !DELETED('StyPrice') 
      IF FOUND()
        IF m.cStatus='D'  &&Delete
          SELECT STYPRICE
          gfDelete()
        ELSE  && Modify
          SELECT STYPRICE
          GATHER MEMVAR
          gfReplace('')
        ENDIF
      ELSE
        IF m.cStatus#'D'  &&Not Deleted
          gfAppend(' IN StyPrice', .T.)
        ENDIF
      ENDIF 
    ELSE
      IF m.cStatus#'D'  &&Not Deleted
        gfAppend(' IN StyPrice', .T.)
      ENDIF
    ENDIF
    =gfAdd_Info(oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'STYPRICE')].lcCursorUpdate)
  ENDSCAN
ENDSCAN
SET DELETE ON
RETURN

*!*************************************************************
*! Name      : lfGetForeignPrice
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Get Style foreign prices
*!*************************************************************
FUNCTION lfGetForeignPrice
*--Create foreign prices temp file and/or display the foreign price screen.
LPARAMETERS lcGetStyle,llDisplay,lcTmpStyPr,lcActiveMode

lcGetStyle = IIF(TYPE('lcGetStyle')$'UL' OR EMPTY(lcGetStyle),lcStyleKey,lcGetStyle)

LOCAL lnAlias,llNewKey
lnAlias = SELECT()
llNewKey = .F.  && New key for style , required to create new records.
= gfOpenTable(oAriaApplication.DataDir+'styprice','styprice','SH')

IF !USED(lcTmpStyPr)
  *--If temp foreign prices file does not used, create new one.
  llNewKey = .T.
  SELECT STYPRICE
  =AFIELDS(lafilestru)
  lnFFlds = ALEN(lafilestru,1)
  DIMENSION lafilestru[lnFFlds+1,18]
  lafilestru[lnFFlds+1,1] = 'CSTATUS'
  lafilestru[lnFFlds+1,2] = 'C'
  lafilestru[lnFFlds+1,3] = 1
  lafilestru[lnFFlds+1,4] = 0
  lafilestru[lnFFlds+1,5] = .T.
  lafilestru[lnFFlds+1,6] = .T.
  STORE "" TO lafilestru[lnFFlds+1,7],lafilestru[lnFFlds+1,8],lafilestru[lnFFlds+1,9],lafilestru[lnFFlds+1,10],lafilestru[lnFFlds+1,11],;
          lafilestru[lnFFlds+1,12],lafilestru[lnFFlds+1,13],lafilestru[lnFFlds+1,14],lafilestru[lnFFlds+1,15],lafilestru[lnFFlds+1,16]
  STORE 0 TO lafilestru[lnFFlds+1,17],lafilestru[lnFFlds+1,18]
  =gfCrtTmp(lctmpstypr,@lafilestru)
  SELECT (lcTmpStyPr)
  INDEX ON cCurrCode TAG (lcTmpStyPr) UNIQUE 
  SET ORDER TO
ELSE
  SELECT (lcTmpStyPr)
  SET ORDER TO
  GO TOP
  *--If used but the key was changed, required to zap and fill new records for the new key.
  IF EOF() OR Style # lcGetStyle  &&New Key
    llNewKey = .T.
    ZAP
  ENDIF
ENDIF

*--fill style prices temp.
IF llNewKey
  SELECT STYPRICE
  gfSeek(lcGetStyle)
  SCAN WHILE Style=lcGetStyle
    SCATTER MEMVAR
    SELECT (lcTmpStyPr)
    APPEND BLANK
    GATHER MEMVAR 
    REPLACE cStatus WITH 'S'
    IF lcGetStyle # lcStyleKey
      REPLACE Style WITH lcStyleKey
    ENDIF
  ENDSCAN
ELSE
  IF lcGetStyle # lcStyleKey
    SELECT (lcTmpStyPr)
    REPLACE ALL Style WITH lcStyleKey
  ENDIF
ENDIF

SELECT (lcTmpStyPr)
SET ORDER TO TAG (lcTmpStyPr)
GO TOP

IF llDisplay  && Require to display a foreign prices form.
  DO FORM (oAriaApplication.ScreenHome+'IC\FrnPrc.scx') WITH lcTmpStyPr,lcActiveMode,lcGetStyle
ENDIF
= SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfSavePDMfiles
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save PDM Files 
*!*************************************************************
FUNCTION lfSavePDMfiles
lPARAMETERS lcStyMajor 
*--Update PDM related tables, when Saving the style.
*-- Delete the seletced item from the PDMSTYLE File.
SET DELETED OFF
SELECT PDMSTYLE
lcOldOrder = ORDER()
gfSetOrder([STYLE])
gfSeek(lcStyMajor)
SCAN REST WHILE PADR(Style,lnStyleWid) = lcStyMajor
  gfDelete()
ENDSCAN
gfSetOrder([&lcOldOrder ])
SET DELETED ON
*-- Mark the style in the log file as an imported Style.
IF gfSeek('PROTOTYPE'+lcStyMajor,"PDMLOG")
  SELECT PDMLOG
  gfReplace([cImpstatus With 'C'])
ENDIF


*!*************************************************************
*! Name      : lfSaveEDIfiles
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save EDI Files 
*!*************************************************************
FUNCTION lfSaveEDIfiles
LPARAMETERS lcStyMajor,lcStyleKey,llallcolors
*--Update EDI related Tables when Saving the style.
*-- Add these lines to mark any "Selection Codes" that contain the edited style(s)
*-- as "To Be Transmitted", if the Inter-Company EDI module is installed and
*-- setup and the "System Type" system setup is set to "Back Office"

PRIVATE lcCatgSty
*-- Open EDI Files
IF !USED('EDICatgH')
  =gfOpenTable(oAriaApplication.DataDir+'EDICatgH' ,'Account' , 'SH')
ENDIF
IF !USED('EDICatgD')
  =gfOpenTable(oAriaApplication.DataDir+'EDICatgD' ,'Account' , 'SH')
ENDIF
IF !USED('EDITRANS')
  =gfOpenTable(oAriaApplication.DataDir+'EDITRANS' ,'TYPEKEY' , 'SH')
ENDIF
    
*-- If the user was editing the style major
IF llallcolors
  lcCatgSty = lcStyMajor
ELSE    && Else, If the user was editing a certain style
  lcCatgSty = lcStyleKey
ENDIF
    
*-- Mark any "Selection Codes" that contain the edited style(s) as "To Be Transmitted"
SELECT EDICatgH
SCAN FOR cEDIStatus = "A"
  IF gfSeek(Type + cPartner + cSelcCode + lcCatgSty , "EDICatgD")
    SELECT EDICatgD
    LOCATE FOR cEDIDStat = "A"  REST;
           WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type + EDICatgH.cPartner + EDICatgH.cSelcCode + lcCatgSty
    IF FOUND()
      gfReplace([lTransmit WITH .T. REST FOR cEDIDStat = "A"] +;
                [ WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type + EDICatgH.cPartner + EDICatgH.cSelcCode + lcCatgSty])
      SELECT EDITrans
      IF gfSeek("832" + PADR(EDICatgH.cSelcCode , 20) + EDICatgH.Type + EDICatgH.cPartner)
        IF cStatus <> "N"
          gfReplace([cStatus WITH "N"])
        ENDIF
      ELSE
        gfAppend()
        gfReplace([cEDITrnTyp WITH "832"              ,]+;
                  [Key        WITH EDICatgH.cSelcCode ,]+;
                  [Type       WITH EDICatgH.Type      ,]+;
                  [cPartner   WITH EDICatgH.cPartner  ,]+;
                  [cStatus    WITH "N"])
      ENDIF
      SELECT EDICatgH
      gfReplace([lTransmit WITH .T.])
    ENDIF
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfSaveSQLBOM
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Save SQL BOM
*!*************************************************************
FUNCTION lfSaveSQLBOM
LPARAMETERS lcbomheadr,lcBom
LOCAL lnConnectionHandlar, lcTranCode
IF TYPE("loconnstring") <> 'O'  
  loconnstring = CREATEOBJECT('remotedataaccess')
ENDIF 
lcTranCode = loconnstring.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

IF TYPE('lcTranCode') = 'N'
  loconnstring.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

SELECT (lcbomheadr)
*-- Start update the POSLN with the deleted records
lnConnectionHandlar = loconnstring.sqlupdate(lcbomheadr,lcTranCode,SET("Datasession"),"CITMMAJOR,CCSTSHTTYP,CCSTSHT_ID")
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =loconnstring.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

SELECT (lcBom)
*-- Start update the POSLN with the deleted records
lnConnectionHandlar = loconnstring.sqlupdate(lcBom,lcTranCode,SET("Datasession"),"citmmajor,typ,citmmask,mfgcode,item")
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =loconnstring.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF


lnConnectionHandlar = loconnstring.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =loconnstring.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
RETURN .T.


*!*************************************************************
*! Name      : lfStyleBusinessRule
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Apply business rules
*!*************************************************************
FUNCTION lfStyleBusinessRule
LPARAMETERS lcStyMajor,llCostPrv,lcbomheadr,lcBom
PRIVATE llAcceptStySav
llAcceptStySav = .F.    && Accept saving (broken rules flag).
llDuplicatekey = .F.
*--Check for Business rules.
DO WHILE .T.
  *--Property to hold duplicate key status
  llDuplicatekey = .F.
  IF !lfSQLFiles(lcStyMajor,"",IIF(Style.Make,'M','I'), .F., .T.,lcbomheadr,lcBom,llCostPrv)
    RETURN .F.
  ENDIF

  IF lcActiveMode = 'A'
    SELECT Style
    = gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH',"TMPSTYLE")
    IF gfSeek(lcStyMajor, 'TMPSTYLE') AND RECNO('TMPSTYLE') > 0
    *N039535,1 WSH [End]

      *--Message: This record has just been entered. What would you like to do? "Overwrite \ Cancel"
      =gfModalGen('TRM00437B00000', 'DIALOG')
      SELECT Style
      gfCloseTable('TMPSTYLE')
      RETURN .F.
      *--Property to hold duplicate key status
      llDuplicatekey = .T.
    ENDIF

    SELECT Style
    gfCloseTable('TMPSTYLE')
  ENDIF
 
  IF EMPTY(STYLE.Scale)
    *--You have to enter a scale code. Cannot update.
    =gfModalGen('TRM42014B42001','DIALOG')
    EXIT
  ENDIF

  *--If the multiple warehouse feature is available.
  *-In add mode for all colors & the default warehouse was empty.
  IF llMultiWh AND llAllColors AND EMPTY(STYLE.cDefWare)
*--You have to set a default warehouse for this style.
    =gfModalGen('TRM42015B42001','DIALOG')
    EXIT
  ENDIF

  *--There is a non major segments exist then check that they was entered.
  IF lnNonSeg # 0
    GO TOP IN (lcColorfil)
    IF EOF(lcColorfil)
      *--No non majors was entered. Cannot update.
      =gfModalGen('TRM42016B42001','DIALOG')
      EXIT
    ENDIF
  ENDIF

  llAcceptStySav = .T.
  EXIT
ENDDO

RETURN llAcceptStySav

*!*************************************************************
*! Name      : lfSQLFiles
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Open SQL Files
*!*************************************************************
FUNCTION lfSQLFiles
LPARAMETERS lcMajor,lcCostsheetId,lcImportedItem,llDefaultCstShet,llNoCondition,lcbomheadr,lcBom,llCostPrv
IF !EMPTY(lcMajor)
  lcCostsheetId  = IIF(TYPE("lcCostsheetId")<>"C","",lcCostsheetId)

  LOCAL lcWhereCond
  IF llNoCondition
    lcAdditional = ""
  ELSE
    lcAdditional = " AND ccStsht_id <> '' "
  ENDIF
    
  lcWhereCond = " cInvType = '0001' AND cItmMajor = '" + PADR(lcMajor,lnStylewid) + "'";
   + lcAdditional
   
  IF !lfOpenSQL('BOMHEADR',lcbomheadr,;
                           'CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID|',;
                           lcbomheadr+"|",;
                           lcWhereCond,;
                           "BOMHEADR")
    RETURN .F.
  ENDIF
  
  lcCostsheetId = lfGetDefCstSht(lcCostsheetId,lcImportedItem,llDefaultCstShet, .F.,lcStyMajor,lcBomHeadr)

  IF llNoCondition
    lcAdditional = " AND ccStsht_id <> ''"
  ELSE
    lcAdditional = " AND Ccstshttyp = '"+lcImportedItem+"'" + IIF(!EMPTY(lcCostsheetId), " AND ccStsht_id = '"+lcCostsheetId +"'", "")
  ENDIF

  lcWhereCond = " cInvType = '0001' AND cItmMajor = '" + PADR(lcMajor,lnStylewid) + ;
      "' " + lcAdditional

  IF !lfOpenSQL('BOM',lcBom,;
                           'citmmajor+typ+citmmask+mfgcode+item|typ+item+citmmajor+citmmask|',;
                           "Bom|Bomitem|",;
                           lcWhereCond,;
                           "MULTIBOM")
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfGetDefCstSht
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Get Default Cost sheet
*!*************************************************************
FUNCTION lfGetDefCstSht
LPARAMETERS lcCostSheetId,lcManufactured,llDEfault, llDispDetails,lcStyMajor,lcBomHeadr

IF !llCostPrv
  RETURN SPACE(6)
ENDIF

PRIVATE lnOldAlias,lc2Return,lcHeadedrbom, lnI, lcI, lcCSTSHTType
LOCAL laCosts(7)

LOCAL laECosts(7), laCurrency[7,3]
STORE '' TO laCurrency

lnOldAlias = SELECT(0)
SELECT (lcBomHeadr)
lcHeadedrbom = lcBomHeadr

LOCATE FOR CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = ;
           PADR(lcStyMajor,19)+lcManufactured+lcCostSheetId .AND. lDefCstSht = llDEfault

lc2Return = CCSTSHT_ID
lcDefCstSht = lc2Return
lcCSTSHTType = CCSTSHTTYP
lcCostSheetType = lcCSTSHTType

LOCAL llDomestic,llImprtdForeign

llDomestic      = lcCSTSHTType = 'M'
llImprtdForeign = llMulCurr AND lcCSTSHTType # 'M'

SELECT(lnOldAlias)
RETURN (lc2Return)

*!*************************************************************
*! Name      : lfUDFDefaultValue
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Get UDF Default values
*!*************************************************************
FUNCTION lfUDFDefaultValue
lcCurrentAlias = ALIAS()
lcFileNameToBrowse = "STYLE"
IF ALLTRIM(lccBrowseTableDBEngine) == oAriaApplication.cSQLDBID
  lnFldResult = oAriaApplication.RemoteSystemData.Execute("SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name AND "+;
                 "SYDFLFLD.CUPGRDLVL ='U' AND SYDFLFLD.cfile_nam = '"+PADR('STYLE',oAriaApplication.FileW) + "'  AND (SYDFIELD.CVER='A40' OR EMPTY(SYDFIELD.CVER)) ", '', "sydfield_tmp", ;
                 "", oAriaApplication.cAria4Sysfiles, 3, "", SET("Datasession"))
ELSE
  lnFldResult = oAriaApplication.remotesystemdata.execute("SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name = SYDFLFLD.cfld_name  AND "+;
                " SYDFLFLD.CUPGRDLVL ='U' AND SYDFLFLD.cfile_nam = '"+PADR('STYLE',oAriaApplication.FileW)+"'   AND (SYDFIELD.CVER='A27' OR EMPTY(SYDFIELD.CVER)) ",'',"sydfield_tmp","",oAriaApplication.SystemConnectionString,3,;
	  			  			"",SET("Datasession"))
ENDIF

IF !USED('Codes_Def')
  =gfOpenTable('Codes','CCODE_NO','SH','Codes_Def')
ENDIF

IF lnFldResult  > 0  AND USED('SydField_Tmp')
 SELECT SydField_Tmp
 SCAN
   lcCurrFldName = ALLTRIM(SydField_Tmp.cFld_Name)
   DO CASE
   CASE lvldentry
     IF gfSeek('D'+PADR(lcCurrFldName,10),'Codes_Def')
       REPLACE &lcCurrFldName.  WITH  Codes_Def.CCODE_NO IN (lCCurrentAlias)
     ENDIF
     
   CASE !EMPTY(mventries)
     lnWngPos = ATC('~', SydField_tmp.mVEntries)
     IF lnWngPos > 0
       lcVldEntriesStr = SUBSTR(SydField_tmp.mVEntries, lnWngPos+1)
       DIMENSION laValEntValues[1]
       laValEntValues = ''
       =gfSubStr(lcVldEntriesStr, @laValEntValues, '|')
       IF !EMPTY(laValEntValues[1])
         lcValueReplace = laValEntValues[1] 
         FOR lnCntArr = 1 TO ALEN(laValEntValues,1)
           IF '@' = SUBSTR(laValEntValues[lnCntArr],1,1)
             lcValueReplace = ALLTRIM(SUBSTR(laValEntValues[lnCntArr],2))
             EXIT 
           ENDIF
         ENDFOR 
         REPLACE &lcCurrFldName.  WITH lcValueReplace  IN (lcCurrentAlias)
       ENDIF
     ENDIF
   OTHERWISE
     DO CASE
     CASE SydField_Tmp.cData_Typ = 'C'
      REPLACE &lcCurrFldName. WITH ''  IN (lcFileNameToBrowse)
       
     CASE SydField_Tmp.cData_Typ = 'N' 
       REPLACE &lcCurrFldName. WITH 0   IN (lcFileNameToBrowse)
       
     CASE SydField_Tmp.cData_Typ = 'L' 
       REPLACE &lcCurrFldName. WITH .F. IN (lcFileNameToBrowse)
       
     ENDCASE
   ENDCASE   
 ENDSCAN 
ENDIF
SELECT(lcCurrentAlias)

*!*************************************************************
*! Name      : lfvWareHouse
*! Developer : Mariam Mazhar
*! Date      : 05/25/2017
*! Purpose   : Get Style locations
*!*************************************************************
FUNCTION lfvWareHouse
LPARAMETERS lcGetStyle,llDisplay

lcGetStyle = IIF(TYPE('lcGetStyle')$'UL' OR EMPTY(lcGetStyle),lcStyleKey,lcGetStyle)

LOCAL lnAlias,llNewKey
lnAlias=SELECT()
llNewKey = .F.
*lcWareFil = ThisFormSet.lcWareFil

*--Create Style Warehouse file if not created and refresh it if the style key was changed.
IF !USED(lcWareFil)
  llNewKey = .T.
  SELECT StyDye
  =AFIELDS(lafilestru)
  lnFFlds = ALEN(lafilestru,1)
  DIMENSION lafilestru[lnFFlds+1,18]
  lafilestru[lnFFlds+1,1] = 'CSTATUS'
  lafilestru[lnFFlds+1,2] = 'C'
  lafilestru[lnFFlds+1,3] = 1
  lafilestru[lnFFlds+1,4] = 0
  lafilestru[lnFFlds+1,5] = .T.
  lafilestru[lnFFlds+1,6] = .T.
  STORE "" TO lafilestru[lnFFlds+1,7],lafilestru[lnFFlds+1,8],lafilestru[lnFFlds+1,9],lafilestru[lnFFlds+1,10],lafilestru[lnFFlds+1,11],;
              lafilestru[lnFFlds+1,12],lafilestru[lnFFlds+1,13],lafilestru[lnFFlds+1,14],lafilestru[lnFFlds+1,15],lafilestru[lnFFlds+1,16]
  STORE 0 TO lafilestru[lnFFlds+1,17],lafilestru[lnFFlds+1,18]
  *CREATE DBF (oAriaApplication.WorkDir+lcwarefil) FROM ARRAY lafilestru
  =gfCrtTmp(lcwarefil,@lafilestru)
  SELECT (lcWareFil)
  INDEX ON cwarecode TAG cwarecode Unique
  SET ORDER TO       TAG cwarecode
ELSE
  SELECT (lcWareFil)
  SET ORDER TO
  GO TOP
  IF EOF() OR Style # lcGetStyle  &&New Key
    llNewKey = .T.
    ZAP
  ENDIF
ENDIF

*--Create warehouses temp.
IF llNewKey
  SELECT STYDYE
  gfSeek(lcGetStyle)
  SCAN WHILE Style=lcGetStyle FOR EMPTY(Dyelot)
    SELECT (lcWareFil)
    APPEND BLANK
    REPLACE Style     WITH lcStyleKey  ,;
            Desc      WITH Style.Desc ,; 
            cWareCode WITH STYDYE.cWareCode ,;
            gl_link   WITH STYDYE.Gl_Link   ,;
            cStatus   WITH 'S'              ,;
            cdisccode WITH STYDYE.cDiscCode 
  ENDSCAN
ELSE
  IF lcGetStyle # lcStyleKey
    SELECT (lcWareFil)
    REPLACE ALL Style WITH lcStyleKey,Desc WITH Style.Desc 
  ENDIF
ENDIF

SELECT (lcWareFil)
SET ORDER TO TAG cwarecode
GO TOP

SELECT(lnAlias)
RETURN
