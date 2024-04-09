*:*********************************************************************************
*: Program file  : ALEXPPIK.PRG
*: Program desc. : Export Styles and Piktkts to Excel
*:        System : Aria4 XP.
*:        Module : Inventory Control(AL).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201028(Aria27),C201029(Aria4)
*:*********************************************************************************
*: Modifications :
*: C201029,2 MMT 08/14/2008 Add Style Major to style exported excel[t20080422.0003]
*:********************************************************************************

IF !llOgFltCh
  RETURN 
ENDIF 

IF EMPTY(lcRpPath) OR (NOT DIRECTORY(lcRpPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Directory")
  RETURN 
ENDIF 

IF SUBSTR(ALLTRIM(lcRpPath),LEN(ALLTRIM(lcRpPath)),1) <> '\'
  lcRpPath = ALLTRIM(lcRpPath) + "\"
ENDIF



IF llRpExpSty
  *Check on Season and Division and warehouse
  lfCreatStyCurs()
  
  llDivSelect = .F.
  lcDivFile = ''
  lnPosDiv = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
  IF lnPosDiv > 0 
    lnPosDiv = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDiv,1)
    lcDivSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosDiv,6]),loOGScroll.laOgFxFlt[lnPosDiv,6],'')
    IF !EMPTY(lcDivSel) 
      lcDivFile = loOGScroll.gfTempName()
      llDivSelect  = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
    ENDIF   
  ENDIF 
  
  llSeaSelect = .F.
  lcSeaFile= ''
  lnPosSeas = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
  IF lnPosSeas > 0 
    lnPosSeas = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSeas,1)
    lcSeasSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosSeas,6]),loOGScroll.laOgFxFlt[lnPosSeas,6],'')
    IF !EMPTY(lcSeasSel) 
      lcSeaFile = loOGScroll.gfTempName()
      llSeaSelect = IIF(LEN(lcSeasSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeasSel,'SEASON',lcSeaFile)
    ENDIF   
  ENDIF 

  llWareSelect = .F.
  lcWareSel = ''
  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  IF lnPosWare  > 0 
    lnPosWare  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare ,1)
    lcWareSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosWare ,6]),loOGScroll.laOgFxFlt[lnPosWare ,6],'')
    IF !EMPTY(lcWareSel)
      SELECT(lcWareSel)
      LOCATE
      IF !EOF()
        llWareSelect = .T.
      ENDIF 
    ENDIF 
  ENDIF 
  
  lnMajLen  = LEN(gfItemMask("PM"))
  STORE  0 TO lnClrLen ,lnClrPos

  DECLARE laItemSeg[1]
  PRIVATE lnCount 
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lcClrSpr = ALLT(laItemSeg[lnCount,6])
      EXIT
    ENDIF
  ENDFOR
  
  
  SELECT Style 
  gfSeek('')
  SCAN FOR IIF(llDivSelect ,SEEK(Style.cDivision,lcDivFile ),.T.) AND  IIF(llSeaSelect ,SEEK(Style.Season,lcSeaFile ),.T.)
    m.Style    = Style.Style
    m.Desc  = Style.desc
    m.Desc1 = Style.desc1
    m.cStyMajor = SUBSTR(Style.Style,1,lnMajLen)
    m.Color = gfCodDes(SUBSTR(m.Style,lnClrPos,lnClrLen), 'COLOR     ')
    
    =gfSeek('S'+Style.Scale,'Scale')
    FOR lnI = 1 TO Scale.cnt
      lcI =STR(lnI,1)
      IF gfSeek(Style.Style+lcI,'STYLEUPC')
        m.StyleUpc = STYLEUPC.cupcnum1+STYLEUPC.cupcnum2+STYLEUPC.cupcnum3
      ELSE
        m.StyleUpc = "9999999999999"
      ENDIF
       
      m.Size =   Scale.Sz&lcI.
      m.Stk = 0 
      IF llWareSelect
        =gfSeek(Style.Style,'Stydye')
        SELECT Stydye 
        SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style FOR SEEK(Stydye.CWARECODE,lcWareSel)
          m.Stk = m.Stk + Stydye.stk&lcI. - Stydye.alo&lcI.
        ENDSCAN 
      ELSE
        m.Stk = Style.stk&lcI. - Style.alo&lcI.
      ENDIF 
      INSERT INTO (lcStyleExport) FROM MEMVAR 
    ENDFOR 
  ENDSCAN 
  SELECT (lcStyleExport)   
  LOCATE 
  IF !EOF()
    lcFileNameX = ALLTRIM(lcRpPath) + "STYLES"+PADL(ALLTRIM(STR(DAY(oAriaApplication.SystemDate))),2,'0')+;
                        PADL(ALLTRIM(STR(MONTH(oAriaApplication.SystemDate))),2,'0')+;
                        ALLTRIM(STR(YEAR(oAriaApplication.SystemDate)))+".XLS"
                        
    *: C201029,2 MMT 08/14/2008 Add Style Major to style exported excel[Start]
    *EXPORT TO (lcFileNameX) TYPE XLS FIELDS StyleUpc ,Color,Size,Desc,Stk       
    EXPORT TO (lcFileNameX) TYPE XLS FIELDS StyleUpc ,Color,Size,Desc,Stk,cStyMajor       
    *: C201029,2 MMT 08/14/2008 Add Style Major to style exported excel[End]
    
    =gfModalGen('INM00000B00000','ALERT','','','Styles are exported successfully ') 
  ELSE
    =gfModalGen('INM00000B00000','ALERT','','','No Styles to Export')
  ENDIF  
ENDIF 

  
*Check if Piktkt will be exported
llPikSelect = .F.
lcPikSel = ''
lnPosPIK = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
IF lnPosPIK > 0 
  lnPosPIK  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPIK,1)
  lcPikSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosPIK,6]),loOGScroll.laOgFxFlt[lnPosPIK ,6],'')
  IF !EMPTY(lcPikSel) AND USED(lcPikSel)
    SELECT(lcPikSel)
    LOCATE
    IF !EOF()
      llPikSelect = .T.
    ENDIF 
  ENDIF 
ENDIF 

  
IF llPikSelect 
  SELECT(lcPikSel)
  lcFileName =  ALLTRIM(lcRpPath) + "PICKTICKETS"+PADL(ALLTRIM(STR(DAY(oAriaApplication.SystemDate))),2,'0')+;
                        PADL(ALLTRIM(STR(MONTH(oAriaApplication.SystemDate))),2,'0')+;
                        ALLTRIM(STR(YEAR(oAriaApplication.SystemDate)))+PADL(ALLTRIM(STR(HOUR(dateTIME()))),2,'0')+PADL(ALLTRIM(STR(MINUTE(dateTIME()))),2,'0')+".CSV"
                        
  lnHandle = FCREATE(lcFileName)
  IF lnHandle < 0
    RETURN .F.
  ENDIF 
  SCAN 
    =gfSeek(&lcPikSel..PIKTKT,'PIKTKT')   
    =gfSeek('O'+PIKTKT.Order,'Ordhdr')   
    =gfSeek('O'+PIKTKT.Order,'ORDLINE') 
    =gfSeek(IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE,'CUSTOMER')
    
    IF PIKTKT.lexpwarh 
      LOOP 
    ENDIF  
     
    lcExpr = '"1","'+'000'+&lcPikSel..PIKTKT+'","'+PIKTKT.Account+'","'+IIF(ORDHDR.Alt_ShpTo,ordhdr.stname,Customer.stname)+'","'+;
             IIF(ORDHDR.Alt_ShpTo,ordhdr.caddress1,Customer.caddress1)+'","'+ IIF(ORDHDR.Alt_ShpTo,ordhdr.caddress2,Customer.caddress2)+;
             '","'+IIF(ORDHDR.Alt_ShpTo,ordhdr.caddress3,Customer.caddress3)+'","'+IIF(ORDHDR.Alt_ShpTo,ordhdr.caddress4,Customer.caddress4)+;
             '","'+IIF(ORDHDR.Alt_ShpTo,ordhdr.caddress5,Customer.caddress5)+'","'+SPACE(5)+'"'
             
    FPUTS(lnHandle,lcExpr)
    lnTotal = 0
    SELECT Ordline 
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+PIKTKT.Order FOR !EMPTY(PIKTKT) AND PIKTKT = &lcPikSel..PIKTKT
      =gfSeek(Ordline.Style,'Style')
      =gfSeek('S'+Style.Scale,'Scale')
      FOR lnI = 1 TO scale.cnt
        lcI = STR(lnI,1)
        lcStyleUpc =''
        IF gfSeek(Style.Style+lcI,'STYLEUPC')
          lcStyleUpc = STYLEUPC.cupcnum1+STYLEUPC.cupcnum2+STYLEUPC.cupcnum3 
        ELSE
          lcStyleUpc = "9999999999999"
        ENDIF
        IF ordline.PIk&lcI. <> 0
          *MMT
          *lcExpr = '"2","'+'000'+&lcPikSel..PIKTKT+'","'+lcStyleUpc +'","'+STR(ordline.PIk&lcI.,6)+'"'
          lcExpr = '"2","'+'000'+&lcPikSel..PIKTKT+'","'+lcStyleUpc+STR(ordline.PIk&lcI.,6)+'"'
          *MMT
          lnTotal = lnTotal + ordline.PIk&lcI.
          FPUTS(lnHandle,lcExpr)
        ENDIF   
      ENDFOR 
    ENDSCAN 
    lcExpr = '"3","'+'000'+&lcPikSel..PIKTKT+'","'+STR(lnTotal ,7)+'"'
    FPUTS(lnHandle,lcExpr)
    SELECT PIKTKT
    gfReplace("lexpwarh with .T.")
    gfReplace("cexpfile with '"+JUSTSTEM(lcFileName) +"'")
  ENDSCAN   
  FCLOSE(lnHandle)
  SELECT PIKTKT
  gfTableUpdate()
  IF FILE(lcFileName)
    =gfModalGen('INM00000B00000','ALERT','','','File '+ALLTRIM(JUSTSTEM(lcFileName))+' has been exported successfully')
    RETURN
  ELSE
    IF !llRpExpSty
      =gfModalGen('TRM00000B00000','ALERT','','','No Records to Export')
      RETURN 
    ENDIF   
  ENDIF 
ELSE
  IF !llRpExpSty
    =gfModalGen('TRM00000B00000','ALERT','','','No Records to Export')
    RETURN 
  ENDIF   
ENDIF 



*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/21/2008
*! Purpose   : Refresh grid
*!*************************************************************
FUNCTION lfExpSty
ClearRead()


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/21/2008
*! Purpose   : Check Path
*!*************************************************************
FUNCTION lfvPath
IF ALLTRIM(lcRpPath)  = "?"
 lcRpPath  = GETDIR()
ENDIF

*!*************************************************************
*! Name      : lfwOgWhen
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/21/2008
*! Purpose   : When function of report
*!*************************************************************
FUNCTION lfwOgWhen
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.
gfOpenTable('Style','Style') 
gfOpenTable('PIKTKT','PIKTKT') 
gfOpenTable('Scale','Scale') 
gfOpenTable('StyleUpc','StyleUpc') 
gfOpenTable('ordline','Ordline') 
gfOpenTable('STYDYE','STYDYE') 
gfOpenTable('ORDHDR','ORDHDR') 
gfOpenTable('CUSTOMER','CUSTOMER') 

IF EMPTY(lcRpPath)
  lcRpPath  = gfGetMemVar('M_ALEXPDIR')
ENDIF   

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/21/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6 
    laTempacstru[1,4]= 0
    
  CASE   ALLTRIM(lcFieldName) = 'SEASON'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6 
    laTempacstru[1,4]= 0
ENDCASE 

 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfCreatStyCurs
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/21/2008
*! Purpose   : Create style cursor
*!*************************************************************
FUNCTION lfCreatStyCurs
DIMENSION laStyleFile[8,4]

laStyleFile[1,1] = 'StyleUpc'
laStyleFile[1,2] = 'C'
laStyleFile[1,3] = 15
laStyleFile[1,4] = 0

laStyleFile[2,1] = 'cStyMajor'
laStyleFile[2,2] = 'C'
laStyleFile[2,3] = 19
laStyleFile[2,4] = 0

laStyleFile[3,1] = 'Color'
laStyleFile[3,2] = 'C'
laStyleFile[3,3] = 35
laStyleFile[3,4] = 0

laStyleFile[4,1] = 'Size'
laStyleFile[4,2] = 'C'
laStyleFile[4,3] = 5
laStyleFile[4,4] = 0

laStyleFile[5,1] = 'Desc'
laStyleFile[5,2] = 'C'
laStyleFile[5,3] = 20
laStyleFile[5,4] = 0

laStyleFile[6,1] = 'Desc1'
laStyleFile[6,2] = 'C'
laStyleFile[6,3] = 60
laStyleFile[6,4] = 0

laStyleFile[7,1] = 'Stk'
laStyleFile[7,2] = 'N'
laStyleFile[7,3] = 7
laStyleFile[7,4] = 0

laStyleFile[8,1] = 'Style'
laStyleFile[8,2] = 'C'
laStyleFile[8,3] = 19
laStyleFile[8,4] = 0

 = gfCrtTmp(lcStyleExport,@laStyleFile,'Style' ,lcStyleExport,.T.)
 
