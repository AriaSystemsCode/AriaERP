*:*********************************************************************************
*: Program file  : ICEXTOWB.PRG
*: Program desc. : Export Style to Web
*:        System : Aria4 XP.
*:        Module : Inventory Control(IC).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C200887,C200888
*:*********************************************************************************
*: Modifications :
*MMT 11/21/2007
*MMT 11/27/2007 Add Division Code
*:********************************************************************************
IF EMPTY(lcRpPath) OR (NOT DIRECTORY(lcRpPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Directory")
  RETURN 
ENDIF 

IF SUBSTR(ALLTRIM(lcRpPath),LEN(ALLTRIM(lcRpPath)),1) <> '\'
  lcRpPath = ALLTRIM(lcRpPath) + "\"
ENDIF

lcFileName = ''

lnHour = hour(DATETIME())
IF lnHour  > 12
  lnHour  = lnHour  - 12 
ENDIF 

*MMT 11/21/2007 [Start]
*lcFileName ='PROD'+PADL(ALLTRIM(STR(DAY(oAriaApplication.SystemDate))),2,'0')+PADL(ALLTRIM(STR(MONTH(oAriaApplication.SystemDate))),2,'0')+;
            ALLTRIM(STR(Year(oAriaApplication.SystemDate)))+PADL(ALLTRIM(STR(lnHour)),2,'0')+PADL(ALLTRIM(STR(MINUTE(DATETIME()))),2,'0')

lcFileName ='PROD'+PADL(ALLTRIM(STR(DAY(oAriaApplication.SystemDate))),2,'0')+PADL(ALLTRIM(STR(MONTH(oAriaApplication.SystemDate))),2,'0')+;
            ALLTRIM(STR(Year(oAriaApplication.SystemDate)))+;
            PADL(ALLTRIM(STR(lnHour)),2,'0')+PADL(ALLTRIM(STR(MINUTE(DATETIME()))),2,'0')+;
            PADL(ALLTRIM(STR(SEC(DATETIME()))),2,'0')
            
*MMT 11/21/2007 [End]

lcFileFullPath = ALLTRIM(lcRpPath)+lcFileName+".STK"

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

llWareSelect = .F.
lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"WAREHOUS.CWARECODE")
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


*MMT 11/27/2007 Add Division Code [Start]
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
*MMT 11/27/2007 Add Division Code [End]



SELECT Style 
gfSeek('')

STORE 0 TO lnPublish

*MMT 11/27/2007 Add Division Code [Start]
*COUNT FOR cPublish = .T. TO lnPublish
COUNT FOR cPublish = .T. AND IIF(llDivSelect ,SEEK(Style.Cdivision,lcDivFile),.T.) TO lnPublish
*MMT 11/27/2007 Add Division Code [End]

IF lnPublish > 0
 lnOutFile = FCREATE(lcFileFullPath,0)
 IF lnOutFile < 0
   =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
   RETURN
  ENDIF
  opross = CREATEOBJECT('ariaprogressbar')  
  oPross.TotalProgress = lnPublish
  oPross.AutoCenter = .T.
  oPross.Show()
ELSE
  =gfModalGen('TRM00000B00000','ALERT','','','No Records to Export')
  RETURN
ENDIF

lnPrepRec = 0


lcDateSt = SET("Date")
SET DATE BRITISH 

*MMT 11/27/2007 Add Division Code [Start]
*SCAN FOR cPublish = .T.
SCAN FOR cPublish = .T. AND IIF(llDivSelect ,SEEK(Style.Cdivision,lcDivFile),.T.) 
*MMT 11/27/2007 Add Division Code [End]

  lcFileLine = ""
  *MMT 11/21/2007 [Start]
  *STORE '' TO m.cStyMajor ,m.cStyClr,m.cStyUPC, m.cColrDesc,m.cStyType,m.cStyTypDesc,m.cGender,m.cSizeCode,;
              m.cSizeDesc,m.cStyDesc,m.cStyDesc1,m.cActive,;
              m.cProdCode,m.cFitDesc,m.cStyGendDesc
              
  STORE '' TO m.cStyMajor ,m.cStyClr,m.cStyUPC, m.cColrDesc,m.cStyType,m.cStyTypDesc,m.cGender,m.cSizeCode,;
              m.cSizeDesc,m.cStyDesc,m.cStyDesc1,m.cActive,;
              m.cProdCode,m.cFitDesc,m.cStyGendDesc,m.Season
  *MMT 11/21/2007 [End]
              
  STORE {} to m.DActiveFrom,m.DActiveTo,m.dStyEdtDate            
              
  STORE 0 TO m.nStyStck,m.nEurRetPrice,m.nStyRetPrice,m.nVatRate
  
              
  lnPrepRec = lnPrepRec + 1            
  oPross.CurrentProgress(lnPrepRec)
  
  lcStyle = Style.Style 
  
  oPross.Caption = 'Collecting data for Style:' + Style.Style 
  
  m.cStyMajor = PADR(SUBSTR(Style.Style ,1,lnMajLen),12)
  
  m.cStyClr   = PADR(SUBSTR(Style.Style ,lnClrPos,lnClrLen),6)
  
  m.cColrDesc = SUBSTR(gfCodDes(m.cStyClr, 'COLOR     '),1,20)
  
  m.cStyType  =  Style.cstygroup
  
  m.cStyTypDesc = SUBSTR(gfCodDes(m.cStyType  , 'CSTYGROUP '),1,20)
  
  m.cGender  = Style.Cgender
  
  m.cStyGendDesc = SUBSTR(gfCodDes(m.cGender  , 'CGENDER   '),1,20)
  
  *MMT 11/21/2007 [Start]
  m.Season = SUBSTR(gfCodDes(Style.Season  , 'SEASON    '),1,20)
  *MMT 11/21/2007 [End]
  
  
  m.cStyDesc =Style.desc
  
  m.cStyDesc1 = Style.desc1
  
  m.dStyEdtDate = Style.dedit_date
  
  DIMENSION laRelFldTax[1,2]
  laRelFldTax[1,1] = 'NTAXRATE'
  laRelFldTax[1,2] = 'lnTaxRate'
  lnTaxRate = 0
  = gfRltFld(style.ctaxcode, @laRelFldTax, 'CTAXCODE  ')
  m.nVatRate = lnTaxRate
  m.nStyRetPrice = Style.nsugretpri
  IF  gfSeek(Style.Style+"EUR",'STYPRICE')  
    m.nEurRetPrice = STYPRICE.nsugretpri
  ENDIF 
  
  m.DActiveFrom = style.dactfrom 
  m.DActiveTo   = style.dactvto 
  m.cProdCode   = Style.Cvensty 
  m.cFitDesc    = gfCodDes(style.cfit  , 'CFIT      ')
  
  
  =gfSeek('S'+Style.Scale,'Scale')
 
  IF Style.Cremove = .T.
    SELECT Style
    gfReplace ("cPublish WITH  .F.") 
    gfTableUpdate()
    m.cActive = 'NO'
  ELSE
    m.cActive = 'YES'   
  ENDIF

  FOR lnI  =  1 TO Scale.cnt
    
    IF (lnI >= Style.ntaxbreak)
      m.nVatRate = lnTaxRate
    ELSE
      m.nVatRate = 0
    ENDIF 
      
  
    lcFileLine = ""
    lcI = ALLTRIM(STR(lnI),1)
    
    m.cSizeDesc = PADR(Scale.Sz&lcI,5)
    m.cSizeCode = PADR(Style.Scale+lcI,5)
    =gfSeek(STYLE.STYLE+lcI,'STYLEUPC')  
    m.cStyUPC = PADR(STYLEUPC.cupcnum1+STYLEUPC.cupcnum2+STYLEUPC.cupcnum3,13)
       
    *Stock Calculations
    *Phyiscal Inventory|
    *Physical Inventory less Allocated|
    *Physical Inventory less Open Sales Orders|
    *Physical Inventory + Work in Progress|
    *Physical Inventory+Work in Progress less Open Sales Order|
    *Physical Inventory less (Open Sales Orders + Allocated)   
    *~P|A|O|W|S|L
    m.nStyStck = 0
    
    DO CASE 
      CASE  lcRpCalcMth = 'P'
        IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  STYDYE.STK&lcI 
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  STYDYE.STK&lcI 
          ENDSCAN 
        ENDIF 
        
     CASE  lcRpCalcMth = 'A'
       IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - STYDYE.ALO&lcI)
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - STYDYE.ALO&lcI)
          ENDSCAN 
        ENDIF 
      
      
      CASE  lcRpCalcMth = 'O'
      
        IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - STYDYE.ORD&lcI)
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - STYDYE.ORD&lcI)
          ENDSCAN 
        ENDIF 
       
     CASE  lcRpCalcMth = 'W'
      
        IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI + STYDYE.wip&lcI)
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI + STYDYE.wip&lcI)
          ENDSCAN 
        ENDIF 
        
      CASE  lcRpCalcMth = 'S'
      
        IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI + STYDYE.wip&lcI - STYDYE.ord&lcI)
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI + STYDYE.wip&lcI - STYDYE.ord&lcI)
          ENDSCAN 
        ENDIF 
        
      CASE  lcRpCalcMth = 'L'
      
        IF llWareSelect 
          SELECT(lcWareSel)
          LOCATE
          SCAN 
            =gfSeek(Style.Style+&lcWareSel..CWARECODE,'STYDYE')
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - (STYDYE.ord&lcI+STYDYE.alo&lcI))
          ENDSCAN 
        ELSE
          =gfSeek(Style.Style,'STYDYE')
          SELECT STYDYE
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style
            m.nStyStck = m.nStyStck +  (STYDYE.STK&lcI - (STYDYE.ord&lcI+STYDYE.alo&lcI))
          ENDSCAN 
        ENDIF 
        
        
    ENDCASE 
    
    *MMT 11/21/2007 [Start]
*    lcFileLine = '"'+m.cStyUPC+'","'+m.cStyMajor+'","'+m.cStyClr+'","'+m.cColrDesc +'","'+;
                 m.cStyType+'","'+m.cStyTypDesc+'","'+m.cGender+'","'+ m.cStyGendDesc +'","'+;
                 ALLTRIM(STR(m.nVatRate,6,2))+'","'+m.cSizeCode+'","'+m.cSizeDesc+'","'+;
                 ALLTRIM(STR(m.nStyRetPrice,8,2))+'","'+ ALLTRIM(STR(m.nEurRetPrice,8,2)) +'","'+;
                 ALLTRIM(STR(m.nStyStck,7,0))+'","'+ m.cStyDesc+'","'+ m.cStyDesc1+'","'+DTOC(m.dStyEdtDate)+'","'+;
                 m.cActive +'","'+ DTOC(m.DActiveFrom )+'","'+DTOC(m.DActiveTo)+'","'+ m.cProdCode +'","'+ ;
                 m.cFitDesc+'","'
                 
    lcFileLine = '"'+m.cStyUPC+'","'+m.cStyMajor+'","'+m.cStyClr+'","'+m.cColrDesc +'","'+;
                 m.cStyType+'","'+m.cStyTypDesc+'","'+m.cGender+'","'+ m.cStyGendDesc +'","'+;
                 ALLTRIM(STR(m.nVatRate,6,2))+'","'+m.cSizeCode+'","'+m.cSizeDesc+'","'+;
                 ALLTRIM(STR(m.nStyRetPrice,8,2))+'","'+ ALLTRIM(STR(m.nEurRetPrice,8,2)) +'","'+;
                 ALLTRIM(STR(m.nStyStck,7,0))+'","'+ m.cStyDesc+'","'+ m.cStyDesc1+'","'+DTOC(m.dStyEdtDate)+'","'+;
                 m.cActive +'","'+ DTOC(m.DActiveFrom )+'","'+DTOC(m.DActiveTo)+'","'+ m.cProdCode +'","'+ ;
                 m.cFitDesc+'","'+m.Season +'"'
                 
    *MMT 11/21/2007 [End]
                 
    = FPUTS( lnOutFile ,lcFileLine)
    *Stock Calculations

  
  ENDFOR 

  

ENDSCAN 
oPross = null
=FCLOSE(lnOutFile)
SET date &lcDateSt 
=gfModalGen('INM00430B40011','ALERT')
*!***********************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/18/2007
*! Purpose   : Report When Function
*!*************************************************************
FUNCTION lfwRepWhen
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.

*MMT 11/27/2007 Add Division Code [Start]
IF EMPTY(lcRpPath)
*MMT 11/27/2007 Add Division Code [End]

  lcRpPath  = gfGetMemVar('M_EXPDIR')

*MMT 11/27/2007 Add Division Code [Start]
ENDIF   
*MMT 11/27/2007 Add Division Code [End]

=gfOpenTable('STYLEUPC','STYLEUPC','SH')
=gfOpenTable('STYLE','STYLE','SH','STYLE')
=gfOpenTable('SCALE','SCALE','SH')
=gfOpenTable('STYPRICE','STYPRICE','SH')
=gfOpenTable('STYDYE','STYDYE','SH')

*MMT 11/27/2007 Add Division Code [Start]
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/27/2007
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
*MMT 11/27/2007 Add Division Code [End]