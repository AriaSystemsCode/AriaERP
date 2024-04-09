*:***************************************************************************
*: Program file  : icstcmre
*: Program desc. : Custom Style Component/Construction Pack report required for Baker Street Clothing
*: System        : Aria4XP
*: Module        : IC
*: Developer     : Mariam Mazhar (MMT)
*: TRACKING      : C200989  05/05/2008 {T20080221.0001}
*:***************************************************************************
*: Modifications:
*: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[T20080221.0001]
*: C200989,3 MMT 05/29/2008 Fix bugs of wrong header items order[T20080221.0001]
*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[T20080221.0001]
*:***************************************************************************
lcSelectedStyle = '' 
IF lcRPSlctBy = 'S'
  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectedStyle  = lcStySel
    ENDIF 
  ENDIF 
  
  IF EMPTY(lcSelectedStyle)
    = gfModalGen('TRM00052B40011','ALERT')
    RETURN  
  ENDIF 
  
ELSE
  lcSelectPO = ''
  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.PO")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectPO = lcStySel
    ENDIF 
  ENDIF   
  IF EMPTY(lcSelectPO)
    = gfModalGen('TRM00052B40011','ALERT')
    RETURN  
  ENDIF 
ENDIF 

IF loogScroll.llOgFltCh
  lfCrtTemp()
  lfCollect()

  SELECT Distinct STYLE,COLOR FROM (lcDataFile)  INTO Cursor 'TempStyClr'
  SELECT 'TempStyClr'
  SCAN 
    =SEEK(TempStyClr.STYLE+TempStyClr.COLOR,lcDataFile,lcDataFile)
    SELECT(lcDataFile)
    SCATTER MEMO MEMVAR 
    SELECT (lcSPTemp)
  
    SCAN FOR !SEEK(TempStyClr.STYLE+TempStyClr.COLOR+STR(&lcSPTemp..nOrder,6)+STR(&lcSPTemp..nInOrd,6),lcDataFile)
      =SEEK(STR(&lcSPTemp..nOrder,5),lcHdTemp,lcHdTemp)           
      m.NHDORD = &lcSPTemp..nOrder
      m.Header = &lcHdTemp..Chead
      m.NstyOrd = &lcSPTemp..nInOrd
      m.Qty = 0 
      m.CSTYLEPART = &lcSPTemp..CSTYLEPART
      m.CSTYPDesc = gfCodDes(m.CSTYLEPART, 'CSTYLEPART')
      m.Descr = ''
      m.Refer = ''
      m.WIDTH = ''
      INSERT INTO (lcDataFile) from MEMVAR 
    ENDSCAN 
  ENDSCAN 
  
  SELECT(lcHdTemp)
  SCAN 
    SELECT (lcDataFile)
    LOCATE FOR NHDORD = &lcHdTemp..nOrder
    IF !FOUND()
      SELECT 'TempStyClr'
      SCAN 
        =SEEK(TempStyClr.STYLE+TempStyClr.COLOR,lcDataFile,lcDataFile)
        SELECT(lcDataFile)
        SCATTER MEMO MEMVAR 
        m.NHDORD = &lcHdTemp..nOrder
        m.Header = &lcHdTemp..Chead
        m.NstyOrd = 0
        m.Qty = 0 
        m.CSTYLEPART = ''
        m.CSTYPDesc = ''
        m.Descr = ''
        m.Refer = ''
        m.WIDTH = ''
        INSERT INTO (lcDataFile) from MEMVAR 
      ENDSCAN 
    ENDIF 
  ENDSCAN 
ELSE
 IF FILE(oAriaApplication.WorkDir + lcDataFile + ".DBF")  AND !USED(lcDataFile)
   USE (oAriaApplication.WorkDir + lcDataFile + ".DBF") IN 0
 ENDIF 
 
 IF FILE(oAriaApplication.WorkDir + lcObjFile+ ".DBF") AND !USED(lcObjFile)   
   USE ( oAriaApplication.WorkDir + lcObjFile+ ".DBF") IN 0
 ENDIF 
ENDIF 

SELECT(lcDataFile)
LOCATE 
IF EOF()
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF    


loOGScroll.lcLogoPath = ''
loOGScroll.cCROrientation='L'
SELECT(lcDataFile)

*!*  IF llObjSel
*!*    SELECT(lcObjFile)
*!*    
*!*    lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"BOMHEADR.CCSTSHT_ID")
*!*    IF lnPosSty > 0 
*!*      lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
*!*      lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
*!*      IF !EMPTY(lcStySel)
*!*        lcSelectedItem = lcStySel
*!*      ENDIF 
*!*    ENDIF 
*!*    
*!*    lcSelectStyle = ''
*!*    lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
*!*    IF lnPosSty > 0 
*!*      lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
*!*      lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
*!*      IF !EMPTY(lcStySel)
*!*        lcSelectStyle = lcStySel
*!*      ENDIF 
*!*    ENDIF 
*!*    
*!*    IF !EMPTY(lcSelectStyle) AND !EMPTY(lcSelectedItem)
*!*    
*!*      IF Seek(PADR(lcSelectStyle,LEN(lcMajorPic))+'I'+lcSelectedItem,lcObjFile)
*!*        SELECT(lcDataFile)
*!*        SET RELATION TO PADR(Style,LEN(lcMajorPic))+'I'+CstShtID INTO (lcObjFile)
*!*      ELSE
*!*        SELECT(lcDataFile)
*!*        SET RELATION TO PADR(Style,LEN(lcMajorPic)) INTO (lcObjFile)
*!*      ENDIF   
*!*      
*!*    ENDIF 
*!*    
*!*    IF !EMPTY(lcSelectStyle) AND EMPTY(lcSelectedItem) 
*!*      SELECT(lcDataFile)
*!*      SET RELATION TO PADR(Style,LEN(lcMajorPic)) INTO (lcObjFile)
*!*    ENDIF 
*!*    SELECT(lcDataFile)
*!*    SET SKIP TO (lcObjFile)
*!*  ENDIF 


IF FILE(oAriaApplication.ReportHome  + '\IC\'+'icstcmre.RPT')
  lcReportFileName = oAriaApplication.ReportHome  + '\IC\'+'icstcmre.RPT'
ENDIF   

SELECT (lcDataFile)
USE 
SELECT(lcObjFile)

*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[Start]
LOCATE FOR EMPTY(CIMGPATH)
IF !FOUND()
  llObjSel = .F.
ELSE
  llObjSel = .T.
ENDIF 
*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[End]

USE 

DIMENSION loOgScroll.laCRParams[1,2]
loOgScroll.laCRParams[1,1] = 'llObjPrint'
loOgScroll.laCRParams[1,2] = IIF(llObjSel,1,0)


DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcDataFile + ".DBF"

loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
loMain = CREATEOBJECT('CrystalRuntime.Report') 
loMain = loMainCr.OpenReport(lcReportFileName)
loMain.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcDataFile + ".DBF")
loMain.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcDataFile + ".DBF",'','')
loMain.DiscardSavedData()
loMain.ConvertDateTimeType = 1  
loMain.CaseInsensitiveSQLData = .T.
loMain.OpenSubreport ('Objlnk')
loSub1 = loMain.OpenSubreport ('Objlnk')
loSub1.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcObjFile+ ".DBF")  
loSub1.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcObjFile+ ".DBF",'','')
loSub1.Database.Verify() && verify database 
loSub1.DiscardSavedData()
loSub1.ConvertDateTimeType = 1  
loSub1.CaseInsensitiveSQLData = .T.
lcTempCrFile = loogscroll.gfTempName()
loMain.Save (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
loMainCr  = NULL
loMain    = NULL
loSub1    = NULL 




=gfDispRe()


*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[Start]
TRY 
IF FILE(oAriaApplication.WorkDir + lcObjFile+ ".DBF")
  USE (oAriaApplication.WorkDir + lcObjFile+ ".DBF") SHARED IN 0
  loXls = ''
  loWord = ''
  SELECT (lcObjFile)
  SCAN FOR !EMPTY(CIMGPATH)
    IF "PDF" $ CIMGPATH
      IF TYPE('loogscroll.oleBound1') ='U'
        loogscroll.addobject('oleBound1',"Oleboundcontrol")
      ENDIF 
      loogscroll.oleBound1.ControlSource = '&lcObjFile..Gobject'
      loogscroll.oleBound1.DoVerb (-1)
      LOOP 
    ENDIF   
    IF ("XLS" $ CIMGPATH)
      IF TYPE('loXls') <> 'O'
        loXls =  CREATEOBJECT('EXCEL.application')
      ENDIF 
      loXls.Workbooks.Open (CIMGPATH)  
      LOOP 
    ENDIF
    
    IF ("DOC" $ CIMGPATH)
      IF TYPE('loWord') <> 'O'
        loWord  =  CREATEOBJECT('WORD.APPLICATION')
      ENDIF 
      loWord.Documents.Open (CIMGPATH)
      LOOP 
    ENDIF
  ENDSCAN 
  
  IF TYPE('loXls') = 'O' AND   loXls.Windows.Count > 0
    FOR lnI = 1 TO loXls.Windows.Count 
      loXls.Windows.Item[lnI].visible= .T.
	ENDFOR 
	loXls.Visible = .T.
  ENDIF 
  IF TYPE('loWord') = 'O' AND  loWord.Windows.Count > 0
	loWord.Visible = .T.
  ENDIF 
ENDIF 
loWord = Null
loXls = Null
CATCH 
ENDTRY 
*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[End]

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Function called from the validation of the style option grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
  LOCAL lcObjNam , lcObjVal
  SELECT STYLE 
  gfSetorder('STYLE')
  lcObjNam = OGSYS18()
  lcObjVal = SUBSTR(EVALUATE(lcObjNam),1,LEN(lcMajorPic))
  IF !EMPTY(lcObjVal) .AND. !gfSEEK(lcObjVal , 'STYLE')
    llBrowse = .T.
    
    *: C200989,3 MMT 05/29/2008 Fix bugs of wrong header items order[Start]
    *lcObjVal = gfStyBrw('M',"","",.F.)
    lcObjVal = gfStyBrw('M',lcObjVal ,"",.F.)
    *: C200989,3 MMT 05/29/2008 Fix bugs of wrong header items order[End]
    
    llBrowse = .F.
  ENDIF
  &lcObjNam. = lcObjVal
  
  lcObjSel = ''
  lnPosObj = ASCAN(loOgScroll.laOgFXFlt,"OBJLINK.COBJECT_ID")
  IF lnPosObj> 0 
    lnPosObj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosObj,1)
    lcObjSel  = loOGScroll.laOgFxFlt[lnPosObj,6]
    IF !EMPTY(lcObjSel) AND USED(lcObjSel)
      SELECT(lcObjSel)
      ZAP 
    ENDIF 
  ENDIF 

  
  
*!*************************************************************
*! Name      : lfvPo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Function called from the validation of the PO option grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPO()
*!*************************************************************
FUNCTION lfvPo
LOCAL lcObjNam , lcObjVal
lcObjNam = OGSYS18()
lcObjVal = EVALUATE(lcObjNam)
IF !EMPTY(lcObjVal) .AND. !gfSEEK('PP'+lcObjVal , 'POSHDR')
    llBrowse = .T.
    DIMENSION laSelecPO[1]
    SELECT POSHDR
    gfSeek('PP')
    lcBrFields = "PO ,Vendor"
     =AriaBrow('','PO', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "PO",'laSelecPO')
    
    llBrowse = .F.
    IF !EMPTY(laSelecPO[1])
      &lcObjNam. = laSelecPO[1]
    ELSE
      &lcObjNam. = ''
    ENDIF 
ENDIF 
 lcObjSel = ''
  lnPosObj = ASCAN(loOgScroll.laOgFXFlt,"OBJLINK.COBJECT_ID")
  IF lnPosObj> 0 
    lnPosObj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosObj,1)
    lcObjSel  = loOGScroll.laOgFxFlt[lnPosObj,6]
    IF !EMPTY(lcObjSel) AND USED(lcObjSel)
      SELECT(lcObjSel)
      ZAP 
    ENDIF 
  ENDIF 

*!*************************************************************
*! Name      : LFREPWHEN   
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : When Function of the Option grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =LFREPWHEN   
*!*************************************************************
FUNCTION LFREPWHEN   

lcHdTemp = lcRPHdTe
lcSPTemp = lcRPSPTe

IF loogscroll.lnOGSeting > 1
  IF !EMPTY(lcHdTemp) AND USED(lcHdTemp) AND RECCOUNT(lcHdTemp) > 0
    SELECT (lcHdTemp)
    COPY TO oAriaApplication.WorkDir+'HeadFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF"
  ENDIF 
  
  IF !EMPTY(lcSPTemp) AND USED(lcSPTemp) AND RECCOUNT(lcSPTemp) > 0
    SELECT (lcSPTemp)
    COPY TO oAriaApplication.WorkDir+'StyPrFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF"
  ENDIF 
ENDIF 

 =gfOpenTable(oAriaApplication.DataDir +'OBJLINK',oAriaApplication.DataDir +'OBJLNKTY','SH')
 =gfOpenTable(oAriaApplication.DataDir +'STYLE',oAriaApplication.DataDir +'STYLE','SH')
 =gfOpenTable('POSHDR','POSHDR','SH')
 =gfOpenTable('BOMHEADR','BOMHEADR','SH')
 =gfOpenTable('POSLN','POSLN','SH')
 =gfOpenTable('Codes','CCODE_NO','SH')
 =gfOpenTable('BOM','multibom','SH')
 =gfOpenTable('ITEM','STYLE','SH')
 =gfOpenTable('ctktbom','ctktyp','SH')
  =gfOpenTable(oAriaApplication.DataDir +'OBJects',oAriaApplication.DataDir +'OBJECTID','SH')

DIMENSION laTmpStruct[4,4]

laTmpStruct[1,1] = 'Chead'
laTmpStruct[1,2] = 'C'
laTmpStruct[1,3] = 35
laTmpStruct[1,4] = ''

laTmpStruct[2,1] = 'lAssPrt'
laTmpStruct[2,2] = 'L'
laTmpStruct[2,3] = 1
laTmpStruct[2,4] = ''

laTmpStruct[3,1] = 'nOrder'
laTmpStruct[3,2] = 'N'
laTmpStruct[3,3] = 5
laTmpStruct[3,4] = 0

laTmpStruct[4,1] = 'lOrdered'
laTmpStruct[4,2] = 'L'
laTmpStruct[4,3] = 1
laTmpStruct[4,4] = ''


=gfCrtTmp(lcHdTemp,@laTmpStruct,'Str(nOrder,5)',lcHdTemp,.F.)

DIMENSION laTmpStruct[4,4]
laTmpStruct[2,1] = 'CSTYLEPART'
laTmpStruct[2,2] = 'C'
laTmpStruct[2,3] = 6
laTmpStruct[2,4] = ''

laTmpStruct[1,1] = 'nOrder'
laTmpStruct[1,2] = 'N'
laTmpStruct[1,3] = 5
laTmpStruct[1,4] = 0

laTmpStruct[3,1] = 'nInOrd'
laTmpStruct[3,2] = 'N'
laTmpStruct[3,3] = 5
laTmpStruct[3,4] = 0

laTmpStruct[4,1] = 'InOrder'
laTmpStruct[4,2] = 'L'
laTmpStruct[4,3] = 1
laTmpStruct[4,4] = ''

=gfCrtTmp(lcSPTemp,@laTmpStruct,'CSTYLEPART +STR(nInOrd,5)+ Str(nOrder,5)',lcSPTemp,.F.) 
SELECT (lcSPTemp)
INDEX on STR(nInOrd,5)+ Str(nOrder,5) TAG 'Ordered'



IF loogscroll.lnOGSeting > 1
  IF FILE(oAriaApplication.WorkDir+'HeadFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF")
    SELECT (lcHdTemp)
    APPEND FROM oAriaApplication.WorkDir+'HeadFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF"
  ENDIF 
  
  IF FILE(oAriaApplication.WorkDir+'StyPrFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF")
    SELECT (lcSPTemp)
    APPEND FROM oAriaApplication.WorkDir+'StyPrFlt'+ALLTRIM(STR(loogscroll.lnOGSeting))+".DBF"
  ENDIF 
ENDIF 

 
*!*************************************************************
*! Name      : lfvSlctBy
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Select by option Validation function
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSlctBy
*!*************************************************************
FUNCTION lfvSlctBy
=loOGScroll.RefreshScroll()
*clearread()
lcObjSel = ''
  lnPosObj = ASCAN(loOgScroll.laOgFXFlt,"OBJLINK.COBJECT_ID")
  IF lnPosObj> 0 
    lnPosObj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosObj,1)
    lcObjSel  = loOGScroll.laOgFxFlt[lnPosObj,6]
    IF !EMPTY(lcObjSel) AND USED(lcObjSel)
      SELECT(lcObjSel)
      ZAP 
    ENDIF 
  ENDIF 
*!*************************************************************
*! Name      : lfvCst
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Validation function of the cost sheet field in option grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCst
*!************************************************************* 
FUNCTION lfvCst
lcSelectedItem = ''
lcObjNam = OGSYS18()
lcObjVal = EVALUATE(lcObjNam)

IF lcRPSlctBy = 'S'
  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectedItem = lcStySel
    ENDIF 
  ENDIF 
  IF !EMPTY(lcObjVal) AND !gfSEEK('0001'+PADR(lcSelectedItem,19)+'I'+lcObjVal,'BOMHEADR')
    SELECT BOMHEADR
    gfSeek('0001'+PADR(lcSelectedItem,19)+'I')
    DIMENSION laCstSelect[1]
    
    *: C200989,3 MMT 05/29/2008 Fix bugs of wrong header items order[Start]
    *lcBrFields = "ccstsht_id :R :H= 'Cost Sheet' , Ccstshtdsc :R :H= 'Description' "
    lcBrFields = "ccstsht_id :R :H= 'Cost Sheet' , Ccstshtdsc :R :H= 'Description',ldefcstsht :R :H='Default' "
    *: C200989,3 MMT 05/29/2008 Fix bugs of wrong header items order[End]
    
    =AriaBrow('','Cost Sheet', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "ccstsht_id",'laCstSelect')
            
    IF !EMPTY(laCstSelect[1])
      &lcObjNam. = laCstSelect[1]
    ELSE
      &lcObjNam. = ''
    ENDIF 
  ENDIF 
ENDIF 


lcObjSel = ''
lnPosObj = ASCAN(loOgScroll.laOgFXFlt,"OBJLINK.COBJECT_ID")
IF lnPosObj> 0 
  lnPosObj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosObj,1)
  lcObjSel  = loOGScroll.laOgFxFlt[lnPosObj,6]
  IF !EMPTY(lcObjSel) AND USED(lcObjSel)
    SELECT(lcObjSel)
    ZAP 
  ENDIF 
ENDIF 


*!*************************************************************
*! Name      : lfSeTObj
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Set Reset Function of Object Opy\tion in option grid
*!*************************************************************
*! Parameters: lcSetParam
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSeTObj
*!************************************************************* 
FUNCTION  lfSeTObj
PARAMETERS lcSetParam


IF lcSetParam = 'S'
  IF lcRPSlctBy = 'S'
    lcSelectedItem = ''
    
    lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"BOMHEADR.CCSTSHT_ID")
    IF lnPosSty > 0 
      lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
      lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
      IF !EMPTY(lcStySel)
        lcSelectedItem = lcStySel
      ENDIF 
    ENDIF 
    
    lcSelectStyle = ''
    lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
    IF lnPosSty > 0 
      lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
      lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
      IF !EMPTY(lcStySel)
        lcSelectStyle = lcStySel
      ENDIF 
    ENDIF 
    
    IF !EMPTY(lcSelectStyle) AND !EMPTY(lcSelectedItem)
      IF gfSeek('H'+PADR(lcSelectStyle,LEN(lcMajorPic))+'I'+lcSelectedItem,'OBJLINK')
        SELECT OBJLINK
        SET key TO 'H'+PADR(lcSelectStyle,LEN(lcMajorPic))+'I'+lcSelectedItem
      ELSE
        SELECT OBJLINK
        SET KEY TO 'S'+PADR(lcSelectStyle,LEN(lcMajorPic))
      ENDIF   
    ENDIF 
    
    IF !EMPTY(lcSelectStyle) AND EMPTY(lcSelectedItem) 
      SELECT OBJLINK
      SET key TO 'S'+PADR(lcSelectStyle,LEN(lcMajorPic))
    ENDIF 
    
    IF EMPTY(lcSelectStyle) 
      SELECT OBJLINK
      SET key TO '************************'
    ENDIF 
    
    
  ELSE
    lcSelectPO = ''
    lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.PO")
    IF lnPosSty > 0 
      lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
      lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
      IF !EMPTY(lcStySel)
        lcSelectPO = lcStySel
      ENDIF 
    ENDIF 
  
    IF !EMPTY(lcSelectPO) AND gfSeek('PP'+lcSelectPO,'POSLN')
       IF !EMPTY(POSLN.CCSTSHT_ID) AND gfSeek('H'+PADR(POSLN.STYLE,LEN(lcMajorPic))+'I'+POSLN.CCSTSHT_ID,'OBJLINK')
         SELECT OBJLINK
         SET key TO 'H'+PADR(POSLN.STYLE,LEN(lcMajorPic))+'I'+POSLN.CCSTSHT_ID
       ELSE
         SELECT OBJLINK
         SET key TO 'S'+PADR(POSLN.STYLE,LEN(lcMajorPic))
       ENDIF
    ENDIF 
  ENDIF 
ELSE
  IF lcSetParam = 'R'
    SELECT OBJLINK
    SET key TO 
  ENDIF 
ENDIF 

*!*************************************************************
*! Name      : lfvComHD
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Function to call the header screen from Option grid
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvComHD
*!*************************************************************
FUNCTION lfvComHD
DO FORM (oAriaApplication.ScreenHome+"IC\ICstCoHD.scx") WITH lcHdTemp,lcSPTemp


*!*************************************************************
*! Name      : lfvComSq
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Function to call the Sequence screen from Option grid
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvComSq
*!*************************************************************
FUNCTION lfvComSq
DO FORM (oAriaApplication.ScreenHome+"IC\ICstCoSq.scx") WITH lcHdTemp,lcSPTemp

FUNCTION lfCrtTemp
DIMENSION laDataFStr[16,4]

laDataFStr[1,1] = 'Style' 
laDataFStr[1,2] = 'C'
laDataFStr[1,3] = 19
laDataFStr[1,4] = 0


laDataFStr[2,1] = 'Pattern' 
laDataFStr[2,2] = 'C'
laDataFStr[2,3] = 10
laDataFStr[2,4] = 0


laDataFStr[3,1] = 'Color' 
laDataFStr[3,2] = 'C'
laDataFStr[3,3] = 6
laDataFStr[3,4] = 0

laDataFStr[4,1] = 'CSAMPLE' 
laDataFStr[4,2] = 'C'
laDataFStr[4,3] = 12
laDataFStr[4,4] = 0

laDataFStr[5,1] = 'PO' 
laDataFStr[5,2] = 'C'
laDataFStr[5,3] = 6
laDataFStr[5,4] = 0

laDataFStr[6,1] = 'CstShtID' 
laDataFStr[6,2] = 'C'
laDataFStr[6,3] = 6
laDataFStr[6,4] = 0

laDataFStr[7,1] = 'CstShtDesc' 
laDataFStr[7,2] = 'C'
laDataFStr[7,3] = 30
laDataFStr[7,4] = 0

laDataFStr[8,1] = 'HEADER' 
laDataFStr[8,2] = 'C'
laDataFStr[8,3] = 35
laDataFStr[8,4] = 0

laDataFStr[9,1] = 'Qty' 
laDataFStr[9,2] = 'N'
laDataFStr[9,3] = 10
laDataFStr[9,4] = 3

laDataFStr[10,1] = 'CSTYLEPART' 
laDataFStr[10,2] = 'C'
laDataFStr[10,3] = 6
laDataFStr[10,4] = 0

laDataFStr[11,1] = 'CSTYPDesc' 
laDataFStr[11,2] = 'C'
laDataFStr[11,3] = 30
laDataFStr[11,4] = 0

laDataFStr[12,1] = 'Descr' 
laDataFStr[12,2] = 'C'

*: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
*laDataFStr[12,3] = 30
laDataFStr[12,3] = 60
*: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]

laDataFStr[12,4] = 0

laDataFStr[13,1] = 'Refer' 
laDataFStr[13,2] = 'C'
laDataFStr[13,3] = 19
laDataFStr[13,4] = 0

laDataFStr[14,1] = 'WIDTH' 
laDataFStr[14,2] = 'C'
laDataFStr[14,3] = 30
laDataFStr[14,4] = 0

laDataFStr[15,1] = 'NHDORD' 
laDataFStr[15,2] = 'N'
laDataFStr[15,3] = 5
laDataFStr[15,4] = 0

laDataFStr[16,1] = 'NstyOrd' 
laDataFStr[16,2] = 'N'
laDataFStr[16,3] = 5
laDataFStr[16,4] = 0

=gfCrtTmp(lcDataFile,@laDataFStr,'STYLE+COLOR+STR(NHDORD,6)+STR(NstyOrd,6)',lcDataFile,.F.)

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Function to collect data
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCollect
*!*************************************************************
FUNCTION lfCollect
STORE 0 TO lnClrLen,lnClrPos
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])    
    EXIT 
  ENDCASE  
ENDFOR

STORE 0 TO lnFClrSrt ,lnFClrEnd 
DIMENSION  laMajSeg[1]
=gfItemMask(@laMajSeg, '', '0002')
FOR lnCnt = 1 TO ALEN(laMajSeg, 1)
  *-- Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1] = 'C'
    *-- Get the color length and width.
    lnFClrSrt = laMajSeg[lnCnt,4]
    lnFClrEnd = LEN(laMajSeg[lnCnt,3])
  ENDIF
ENDFOR


lcMajLen =LEN(gfItemMask('PM'))

lcSelectedStyle = '' 
lcSelectedCstID = '' 


IF lcRPSlctBy = 'S'

  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectedStyle  = lcStySel
    ENDIF 
  ENDIF 
  
  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"BOMHEADR.CCSTSHT_ID")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectedCstID = lcStySel
    ENDIF 
  ENDIF 


  SELECT Style
  gfSetOrder('Style')
  gfSeek(ALLTRIM(lcSelectedStyle))
  SCAN REST WHILE STYLE = lcSelectedStyle
    m.PO = ''
    m.Style = lcSelectedStyle
    m.Color = SUBSTR(Style.Style,lnClrPos,lnClrLen)
    m.Pattern = Style.Pattern
    m.CSAMPLE = Style.CSAMPLE
    IF !EMPTY(lcSelectedCstID)
      SELECT BOM   
      gfSeek('0001'+ PADR(lcSelectedStyle,19)+ 'I'+ lcSelectedCstID)
      SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ccstsht_id+ typ+ citmmask+ mfgcode+ cinvtypc+ item+ STR(nlineno,6) =;
                   '0001'+ PADR(lcSelectedStyle,19)+ 'I'+ lcSelectedCstID FOR Ccatgtyp $ 'FT' AND !EMPTY(CSTYLEPART) AND SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp)
                   
                   
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
          IF !(SUBSTR(BOM.cItmMask,lnClrPos, 6) = REPLICATE('*',6))
            IF !LIKE(STRTRAN(BOM.cItmMask,'*','?'),Style.Style)
              LOOP 
            ENDIF 
          ENDIF   
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
           
      
        m.CSTYLEPART = BOM.CSTYLEPART
        m.CSTYPDesc = gfCodDes(m.CSTYLEPART, 'CSTYLEPART')  
        m.CstShtID = BOM.ccstsht_id
        
        *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
        *gfSeek('0001'+ PADR(lcSelectedStyle,19)+'I','BOMHEADR')
        gfSeek('0001'+ PADR(lcSelectedStyle,19)+'I'+ lcSelectedCstID,'BOMHEADR')
        *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
        
        m.CstShtDesc = BOMHEADR.ccstshtdsc
      
        IF SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp) AND SEEK(STR(&lcSPTemp..nOrder,5),lcHdTemp,lcHdTemp)           
          m.NHDORD = &lcSPTemp..nOrder
          m.Header = &lcHdTemp..Chead
          m.NstyOrd = &lcSPTemp..nInOrd
        ENDIF 
        IF SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd) = '******'
          gfSeek('0002'+SUBSTR(BOM.Item,1, lnFClrSrt-1)+m.Color,'ITEM','Style')
          m.Descr = Item.desc1
          m.Refer = Item.cstymajor
          m.WIDTH = Item.citemfld1
        ELSE
          gfSeek('0002'+BOM.Item,'ITEM','Style')
          m.Descr = Item.desc1   
          m.Refer = Item.cstymajor
          m.WIDTH = Item.citemfld1
        ENDIF 
        m.Qty = nbomtotqty
        INSERT INTO (lcDataFile) FROM MEMVAR 
      ENDSCAN                
    ELSE
      lcDefCostSheet = ''
      IF gfSeek('0001'+ PADR(lcSelectedStyle,19)+'I','BOMHEADR')
        SELECT BOMHEADR 
        SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id = '0001'+ PADR(lcSelectedStyle,19)+'I' FOR ldefcstsht
          lcDefCostSheet = BOMHEADR.ccstsht_id 
          EXIT 
        ENDSCAN 
      ENDIF 
      
      *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[T20080221.0001]
      lcDefCostSheet = ''
      *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
      
      IF !EMPTY(lcDefCostSheet)
        m.CstShtDesc = BOMHEADR.ccstshtdsc      
        SELECT BOM   
        gfSeek('0001'+ PADR(lcSelectedStyle,19)+ 'I'+ lcDefCostSheet)
        SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ccstsht_id+ typ+ citmmask+ mfgcode+ cinvtypc+ item+ STR(nlineno,6) =;
                   '0001'+ PADR(lcSelectedStyle,19)+ 'I'+ lcDefCostSheet FOR Ccatgtyp $ 'FT' AND !EMPTY(CSTYLEPART) AND SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp)
      
          m.CSTYLEPART = BOM.CSTYLEPART
          m.CSTYPDesc = gfCodDes(m.CSTYLEPART, 'CSTYLEPART')  
          m.CstShtID = BOM.ccstsht_id
          
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
          *gfSeek('0001'+ PADR(lcSelectedStyle,19)+'I','BOMHEADR')
          gfSeek('0001'+ PADR(lcSelectedStyle,19)+'I'+lcDefCostSheet,'BOMHEADR')
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
          
          m.CstShtDesc = BOMHEADR.ccstshtdsc
      
          IF SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp) AND SEEK(STR(&lcSPTemp..nOrder,5),lcHdTemp,lcHdTemp)           
            m.NHDORD = &lcSPTemp..nOrder
            m.Header = &lcHdTemp..Chead
            m.NstyOrd = &lcSPTemp..nInOrd
          ENDIF 
          IF SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd) = '******'
            gfSeek('0002'+SUBSTR(BOM.Item,1, lnFClrSrt-1)+m.Color,'ITEM','Style')
            m.Descr = Item.desc1
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ELSE
            gfSeek('0002'+BOM.Item,'ITEM','Style')
            m.Descr = Item.desc1   
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ENDIF 
          m.Qty = bom.nbomtotqty
          INSERT INTO (lcDataFile) FROM MEMVAR 
        ENDSCAN
      ELSE
        *No Default Cost Sheet For Style We will add the Headers and Style Parts in Table
        SELECT (lcHdTemp)
        SCAN
          m.Header = &lcHdTemp..Chead
           m.NHDORD = &lcHdTemp..nOrder 
          SELECT(lcSPTemp)
          SCAN FOR nOrder = &lcHdTemp..nOrder 
            m.NstyOrd = &lcSPTemp..nInOrd
            m.CSTYLEPART = &lcSPTemp..CSTYLEPART
            m.CSTYPDesc =gfCodDes(m.CSTYLEPART, 'CSTYLEPART')
            INSERT INTO (lcDataFile) from MEMVAR 
          ENDSCAN  
        ENDSCAN 
                          
      ENDIF 
    ENDIF   
  ENDSCAN 
ELSE
  lcSelectPO = ''
  lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.PO")
  IF lnPosSty > 0 
    lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
    lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
    IF !EMPTY(lcStySel)
      lcSelectPO = lcStySel
    ENDIF 
  ENDIF   
  gfSeek('PP'+lcSelectPO ,'POSLN','POSLN')
  lcStylePO = POsLN.Style
  lcCostShtPO = POSLN.CCSTSHT_ID
  
  
  IF !EMPTY(lcCostShtPO)
    SELECT Style 
    gfSetOrder('Style')
    gfSeek(lcStylePO)
    m.po =lcSelectPO
    SCAN REST WHILE Style = lcStylePO
      m.Style = Style.CStyMajor
      m.Color = SUBSTR(Style.Style,lnClrPos,lnClrLen)
      m.Pattern = Style.Pattern
      m.CSAMPLE = Style.CSAMPLE
      SELECT BOM       
      gfSeek('0001'+ PADR(m.Style,19)+ 'I'+ lcCostShtPO)      
      SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ccstsht_id+ typ+ citmmask+ mfgcode+ cinvtypc+ item+ STR(nlineno,6) =;
                   '0001'+ PADR(m.Style,19)+ 'I'+ lcCostShtPO FOR Ccatgtyp $ 'FT' AND !EMPTY(CSTYLEPART) AND SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp)




          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
          IF !(SUBSTR(BOM.cItmMask,lnClrPos, lnClrLen) = REPLICATE('*',lnClrLen))
            IF !LIKE(STRTRAN(BOM.cItmMask,'*','?'),Style.Style)
              LOOP 
            ENDIF 
          ENDIF   
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
      
          m.CSTYLEPART = BOM.CSTYLEPART
          m.CSTYPDesc = gfCodDes(m.CSTYLEPART, 'CSTYLEPART')  
          m.CstShtID = BOM.ccstsht_id
          gfSeek('0001'+ PADR(m.Style,19)+'I'+lcCostShtPO,'BOMHEADR')
          m.CstShtDesc = BOMHEADR.ccstshtdsc
          IF SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp) AND SEEK(STR(&lcSPTemp..nOrder,5),lcHdTemp,lcHdTemp)           
            m.NHDORD = &lcSPTemp..nOrder
            m.Header = &lcHdTemp..Chead
            m.NstyOrd = &lcSPTemp..nInOrd
          ENDIF 
          IF SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd) = '******'
            gfSeek('0002'+SUBSTR(BOM.Item,1, lnFClrSrt-1)+m.Color,'ITEM','Style')
            m.Descr = Item.desc1
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ELSE
            gfSeek('0002'+BOM.Item,'ITEM','Style')
            m.Descr = Item.desc1   
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ENDIF 
          *cimtyp, cuttkt, cinvtype, item, mfgcode, dyelot
          IF gfSeek('I'+lcSelectPO+'0002'+;
                    IIF(SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd) = '******',;
                    SUBSTR(BOM.Item,1, lnFClrSrt-1)+m.Color,BOM.Item),'CTKTBOM')
                    
            m.Qty = CTKTBOM.untqty && PO QTY
          ELSE
            m.Qty = bom.nbomtotqty
          ENDIF   
          INSERT INTO (lcDataFile) FROM MEMVAR 
      ENDSCAN                
    ENDSCAN 
  ELSE
    lcDefCostSheet = ''
    IF gfSeek('0001'+ PADR(SUBSTR(lcStylePO ,1,lcMajLen) ,19)+'I','BOMHEADR')
      SELECT BOMHEADR 
      SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id = '0001'+ PADR(SUBSTR(lcStylePO ,1,lcMajLen) ,19)+'I';
        FOR ldefcstsht
        lcDefCostSheet = BOMHEADR.ccstsht_id 
        EXIT 
      ENDSCAN 
    ENDIF 
    
    *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
    lcDefCostSheet = ''
    *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
    
    IF !EMPTY(lcDefCostSheet)
      m.CstShtDesc = BOMHEADR.ccstshtdsc      
      SELECT BOM   
      gfSeek('0001'+ PADR(lcSelectedStyle,19)+ 'I'+ lcDefCostSheet)
       SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ccstsht_id+ typ+ citmmask+ mfgcode+ cinvtypc+ item+ STR(nlineno,6) =;
                   '0001'+ PADR(SUBSTR(lcStylePO ,1,lcMajLen) ,19) + 'I'+ lcDefCostSheet FOR Ccatgtyp $ 'FT' AND !EMPTY(CSTYLEPART) AND SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp)


          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
          IF !LIKE(STRTRAN(cItmMask,'*','?'),Style.Style)
            LOOP 
          ENDIF 
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]

      
          m.CSTYLEPART = BOM.CSTYLEPART
          m.CSTYPDesc = gfCodDes(m.CSTYLEPART, 'CSTYLEPART')  
          m.CstShtID = BOM.ccstsht_id
          
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[Start]
          *gfSeek('0001'+PADR(SUBSTR(lcStylePO ,1,lcMajLen) ,19)+'I','BOMHEADR')
          gfSeek('0001'+PADR(SUBSTR(lcStylePO ,1,lcMajLen) ,19)+'I'+lcDefCostSheet,'BOMHEADR')
          *: C200989,2 MMT 05/21/2008 Fix bugs of wrong header items order[End]
          
          m.CstShtDesc = BOMHEADR.ccstshtdsc
      
          IF SEEK(Bom.CSTYLEPART,lcSPTemp,lcSPTemp) AND SEEK(STR(&lcSPTemp..nOrder,5),lcHdTemp,lcHdTemp)           
            m.NHDORD = &lcSPTemp..nOrder
            m.Header = &lcHdTemp..Chead
            m.NstyOrd = &lcSPTemp..nInOrd
          ENDIF 
          IF SUBSTR(BOM.Item, lnFClrSrt, lnFClrEnd) = '******'
            gfSeek('0002'+SUBSTR(BOM.Item,1, lnFClrSrt-1)+m.Color,'ITEM','Style')
            m.Descr = Item.desc1
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ELSE
            gfSeek('0002'+BOM.Item,'ITEM','Style')
            m.Descr = Item.desc1   
            m.Refer = Item.cstymajor
            m.WIDTH = Item.citemfld1
          ENDIF 
          m.Qty = bom.nbomtotqty
          INSERT INTO (lcDataFile) FROM MEMVAR 
        ENDSCAN                
     ELSE
       *No Default Cost Sheet For Style We will add the Headers and Style Parts in Table
        SELECT (lcHdTemp)
        SCAN
          m.Header = &lcHdTemp..Chead
           m.NHDORD = &lcHdTemp..nOrder 
          SELECT(lcSPTemp)
          SCAN FOR nOrder = &lcHdTemp..nOrder 
            m.NstyOrd = &lcSPTemp..nInOrd
            m.CSTYLEPART = &lcSPTemp..CSTYLEPART
            m.CSTYPDesc =gfCodDes(m.CSTYLEPART, 'CSTYLEPART')
            INSERT INTO (lcDataFile) from MEMVAR 
          ENDSCAN  
        ENDSCAN    
     ENDIF  
  ENDIF
ENDIF   

llObjSel = .F.
lcObjSel = ''
lnPosObj = ASCAN(loOgScroll.laOgFXFlt,"OBJLINK.COBJECT_ID")
IF lnPosObj> 0 
  lnPosObj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosObj,1)
  lcObjSel  = loOGScroll.laOgFxFlt[lnPosObj,6]
  IF !EMPTY(lcObjSel) AND USED(lcObjSel)
    SELECT(lcObjSel)
    LOCATE 
    IF !EOF()
      llObjSel = .T.
    ENDIF   
  ENDIF 
ENDIF 
*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[Start]
*  DIMENSION laStArr[3,4]
 DIMENSION laStArr[4,4]
*: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[End]
  laStArr[1,1] =  "ObjID"
  laStArr[1,2] =  "C"
  laStArr[1,3] = 10
  laStArr[1,4] = 0
  
  laStArr[2,1] =  "CKey"
  laStArr[2,2] =  "C"
  laStArr[2,3] = 20
  laStArr[2,4] = 0

  laStArr[3,1] =  "Gobject"
  laStArr[3,2] =  "G"
  laStArr[3,3] = 10
  laStArr[3,4] = 0
  
  *: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[Start]
  laStArr[4,1] =  "CIMGPATH"
  laStArr[4,2] =  "C"
  laStArr[4,3] = 100
  laStArr[4,4] = 0
  *: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[End]

  =gfCrtTmp(lcObjFile,@laStArr,'CKey',lcObjFile,.F.)

IF llObjSel   
  SCAN FOR gfSEEK(&lcObjSel..COBJECT_ID,'OBJects','OBJECTID')
    =gfSEEK(&lcObjSel..COBJECT_ID,'OBJLINK','COBJLINK')
    m.ObjID = &lcObjSel..COBJECT_ID
    m.cKey  = PADR(objlink.cobjlink,LEN(lcMajorPic))
    INSERT INTO (lcObjFile) FROM MEMVAR 
    SELECT(lcObjFile)
    REPLACE Gobject WITH OBJects.Gobject
    
    *: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[Start]
    IF TYPE('loogscroll.oleBound1') ='U'
      loogscroll.addobject('oleBound1',"Oleboundcontrol")
    ENDIF 
    loogscroll.oleBound1.ControlSource = 'OBJects.Gobject'
    loogscroll.oleBound1.AutoSize = .T.
    IF "WORD" $ UPPER(loogscroll.oleBound1.OleClass)
      lcFileName = gfTempName()
      loogscroll.oleBound1.SaveAs(oAriaapplication.workDir+lcFileName+".DOC")
      REPLACE CIMGPATH WITH oAriaapplication.workDir+lcFileName+".DOC" IN (lcObjFile)
    ELSE
      IF "EXCEL" $ UPPER(loogscroll.oleBound1.OleClass)
       lcFileName = gfTempName()
       loogscroll.oleBound1.SaveAs(oAriaapplication.workDir+lcFileName+".XLS")
       REPLACE CIMGPATH WITH oAriaapplication.workDir+lcFileName+".XLS" IN (lcObjFile)
      ELSE
        IF "PACKAGE" $ UPPER(loogscroll.oleBound1.OleClass) OR 'ACRO' $ UPPER(loogscroll.oleBound1.OleClass) 
          lcRecNo = RECNO(lcObjFile) 
          lcFileName = loogscroll.gfTempName()
          COPY TO (oAriaapplication.workDir+lcFileName+".DBF") FOR ObjID =&lcObjSel..COBJECT_ID
          IF FILE(oAriaapplication.workDir+lcFileName+".FPT")
            lcFpt = FILETOSTR(oAriaapplication.workDir+lcFileName+".FPT")
            IF 'PDF' $ UPPER(lcFpt) OR 'ACRO' $ UPPER(loogscroll.oleBound1.OleClass) 
              SELECT (lcObjFile) 
              IF BETWEEN(lcRecNo ,1,RECCOUNT())
                GO lcRecNo 
              ENDIF   
              REPLACE CIMGPATH WITH 'PDF File' 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDIF 
    ENDIF 
    *: C200989,4 MMT 06/04/2008 Fix bugs of wrong header items order[End]

  ENDSCAN 
ENDIF 

