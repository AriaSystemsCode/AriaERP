*:*********************************************************************************
*: Program file  : POEXPEXL.PRG
*: Program desc. : Send PO Hang Tag File program (SHA13)
*:        System : Aria4 XP.
*:        Module : PO(Purchase Order).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201065,C201066[T20081021.0023]
*:*********************************************************************************
*: Modifications :
*:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[T20081021.0023]
*:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[T20090109.0019]
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[T20100409.0006]
*:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [T20121130.0007]
*:********************************************************************************
IF EMPTY(lcRpPath) OR (NOT DIRECTORY(lcRpPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Path")
  RETURN 
ENDIF 

IF EMPTY(lcRpFile) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name")
  RETURN 
ENDIF 

lcExt = JUSText(lcRpFile)
IF EMPTY(lcExt)
  lcExt  = "CSV"
ENDIF 
lcOutFile = ADDBS(ALLTRIM(lcRpPath))+ALLTRIM(JUSTSTEM(lcRpFile))+"."+lcExt  

lcTempFile = loogscroll.gfTempName()

*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
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
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]


lfCrtTemp()
lfCollect()

SELECT(lcTempFile )
LOCATE 
IF EOF()
  =gfModalGen('TRM00000B00000','ALERT','','','No Records to Export')
  RETURN
ENDIF 

lnOutFile = FCREATE(lcOutFile ,0)
IF lnOutFile < 0
   =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
   RETURN
ELSE
  SELECT(lcTempFile )
  LOCATE 
  SCAN 
    *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
    *lcFileLine = PO+","+Vendor+","+Cvensty+","+STYLE+","+COLOR+","+ClrName+","+UPC+","+GrpName+","+SIZE+","+STR(PRICEA,12,2)    
    lcFileLine = PO+","+Vendor+","+Cvensty+","+STYLE+","+COLOR+","+ClrName+","+UPC+","+GrpName+","+SIZE+","+STR(PRICEA,12,2)+","+STR(QTY,7)
    *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]
    *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
    lcFileLine = lcFileLine + "," + Desc
    *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
     = FPUTS( lnOutFile ,lcFileLine)
  ENDSCAN 
  =FCLOSE(lnOutFile)
ENDIF

IF FILE(lcOutFile )
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcOutFile)+" has been exported successfully")
ENDIF   

*!*************************************************************
*! Name      : lfCollect
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/05/2008
*! Purpose   : Collect data
*!*************************************************************
FUNCTION lfCollect
llStyleSelect = .F.
lcStyleSel = ''
lnPosSty  = ASCAN(loOgScroll.laOgFXFlt,"STYLE.STYLE")
IF lnPosSty > 0 
  lnPosSty = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty ,1)
  lcStyleSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosSty ,6]),loOGScroll.laOgFxFlt[lnPosSty ,6],'')
  IF !EMPTY(lcStyleSel) AND USED(lcStyleSel)
    SELECT(lcStyleSel)
    LOCATE
    IF !EOF()
      llStyleSelect = .T.
    ENDIF 
  ENDIF 
ENDIF 
  
  
llPOSelect = .F.
lcPOSel = ''
lnPosPO = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.PO")
IF lnPosPO > 0 
  lnPosPO = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPO,1)
  lcPOSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosPO,6]),loOGScroll.laOgFxFlt[lnPosPO,6],'')
  IF !EMPTY(lcPOSel ) AND USED(lcPOSel)
    SELECT(lcPOSel )
    LOCATE
    IF !EOF()
      llPOSelect = .T.
    ENDIF 
  ENDIF 
ENDIF 




IF llPOSelect 
  SELECT POSLN
  gfSetOrder('POSLN')
  SELECT (lcPOSel)
  SCAN 
    WAIT WINDOW 'Collecting Data For PO:'+&lcPOSel..PO NOWAIT 
    =gfSeek('PP'+&lcPOSel..PO,'POSHDR','POSHDR')
    m.PO = POSHDR.PO
    IF gfSeek('PP'+&lcPOSel..PO,'POSLN')
      SELECT POSLN
      SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =  'PP'+&lcPOSel..PO FOR ;
                      Trancd = '1' AND IIF(llStyleSelect ,SEEK(POSLN.Style,lcStyleSel),.T.)
                      
           m.Vendor = POSLN.Vendor            
           =gfSeek(POSLN.Style,'Style','Style')           
           m.Cvensty = style.cvensty 
           *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
           m.Desc = Style.Desc
           *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
*!*             m.STYLE = SUBSTR(POSLN.Style,1,12)
*!*             m.COLOR = RIGHT(POSLN.Style,6)
           m.STYLE = SUBSTR(POSLN.Style,1,lnMajLen)
           m.COLOR = SUBSTR(POSLN.Style,lnClrPos,lnClrLen)
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]
           m.ClrName =ALLTRIM(gfCodDes(m.COLOR, 'COLOR     '))
           m.GrpName = ALLTRIM(gfCodDes(style.cstygroup , 'CSTYGROUP '))
           
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[Start]
           *m.PRICEA = style.PRICEA 
           m.PRICEA = STYLE.NSUGRETPRI
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[End]
           
           =gfSeek('S'+Style.Scale,'Scale','Scale')           
           FOR lnI = 1 TO Scale.cnt
             lcI = STR(lnI,1)
             IF POSLN.QTY&lcI. > 0
               *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
               m.QTY = POSLN.QTY&lcI.
               *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]
               m.SIZE = Scale.SZ&lcI.
               m.UPC = ''
               IF gfSeek(POSLN.STYLE+lcI,'STYLEUPC','STYLEUPC')
                 m.UPC = styleupc.cupcnum1+styleupc.cupcnum2 +styleupc.cupcnum3                
               ENDIF   
               IF !SEEK(m.PO +m.STYLE+m.COLOR+m.SIZE,lcTempFile)
                 INSERT INTO  (lcTempFile) FROM MEMVAR 
               ENDIF 
             ENDIF 
           ENDFOR 
      ENDSCAN 
    ENDIF 
  ENDSCAN  
ELSE
  IF llStyleSelect
    SELECT POSLN
    gfSetOrder('POSLNS')
    SELECT(lcStyleSel)
    SCAN
      WAIT WINDOW 'Collecting Data For Style:'+&lcStyleSel..Style NOWAIT 
      IF gfSeek('0001'+&lcStyleSel..Style+'PP','POSLN')
        SELECT POSLN
        SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001'+&lcStyleSel..Style+'PP' FOR TRANCD = '1'                                                               
          =gfSeek('PP'+POSLN.PO,'POSHDR')
          m.PO = POSHDR.PO
          m.Vendor = POSLN.Vendor            
          =gfSeek(POSLN.Style,'Style','Style')           
          m.Cvensty = style.cvensty 
          *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
          m.Desc = Style.Desc
          *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
*!*            m.STYLE = SUBSTR(POSLN.Style,1,12)
*!*            m.COLOR = RIGHT(POSLN.Style,6)
           m.STYLE = SUBSTR(POSLN.Style,1,lnMajLen)
           m.COLOR = SUBSTR(POSLN.Style,lnClrPos,lnClrLen)
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]
          m.ClrName =ALLTRIM(gfCodDes(m.COLOR, 'COLOR     '))
          m.GrpName = ALLTRIM(gfCodDes(style.cstygroup ,'CSTYGROUP '))
          
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[Start]
          *m.PRICEA = style.PRICEA 
          m.PRICEA = STYLE.NSUGRETPRI
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[End]
          
          =gfSeek('S'+Style.Scale,'Scale','Scale')           
          FOR lnI = 1 TO Scale.cnt
            lcI = STR(lnI,1)
            IF POSLN.QTY&lcI. > 0
              *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
              m.QTY = POSLN.QTY&lcI.
              *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]
              m.SIZE = Scale.SZ&lcI.
              m.UPC = ''
              IF gfSeek(POSLN.STYLE+lcI,'STYLEUPC','STYLEUPC')
                m.UPC = styleupc.cupcnum1+styleupc.cupcnum2 +styleupc.cupcnum3                
              ENDIF   
              IF !SEEK(m.PO +m.STYLE+m.COLOR+m.SIZE,lcTempFile)
                INSERT INTO  (lcTempFile) FROM MEMVAR 
              ENDIF 
            ENDIF 
          ENDFOR 
        ENDSCAN
      ENDIF 
    ENDSCAN 
  ELSE
    =gfSeek('PP','POSHDR')
    SELECT POSLN
    gfSetOrder('POSLN')
    SELECT POSHDR
    SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO = 'PP'  
      WAIT WINDOW 'Collecting Data For PO:'+POSHDR.PO NOWAIT                                                                                                  
      m.PO = POSHDR.PO
      IF gfSeek('PP'+POSHDR.PO,'POSLN')
        SELECT POSLN
        SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =  'PP'+POSHDR.PO FOR ;
                      Trancd = '1' 
                      
           m.Vendor = POSLN.Vendor            
           =gfSeek(POSLN.Style,'Style','Style')           
           m.Cvensty = style.cvensty 
           *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
           m.Desc = Style.Desc
           *:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
*!*             m.STYLE = SUBSTR(POSLN.Style,1,12)
*!*             m.COLOR = RIGHT(POSLN.Style,6)
           m.STYLE = SUBSTR(POSLN.Style,1,lnMajLen)
           m.COLOR = SUBSTR(POSLN.Style,lnClrPos,lnClrLen)
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]
           m.ClrName =ALLTRIM(gfCodDes(m.COLOR, 'COLOR     '))
           m.GrpName = ALLTRIM(gfCodDes(style.cstygroup , 'CSTYGROUP '))
           
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[Start]
           *m.PRICEA = style.PRICEA 
           m.PRICEA = STYLE.NSUGRETPRI
           *:B608796,1 MMT 02/02/2009 Print retail price instead of sale price in send PO info prg[End]
           
           =gfSeek('S'+Style.Scale,'Scale','Scale')           
           FOR lnI = 1 TO Scale.cnt
             lcI = STR(lnI,1)
             IF POSLN.QTY&lcI. > 0
               *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
               m.QTY = POSLN.QTY&lcI.
               *:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]
               m.SIZE = Scale.SZ&lcI.
                m.UPC = ''
               IF gfSeek(POSLN.STYLE+lcI,'STYLEUPC','STYLEUPC')
                 m.UPC = styleupc.cupcnum1+styleupc.cupcnum2 +styleupc.cupcnum3                
               ENDIF   
               IF !SEEK(m.PO +m.STYLE+m.COLOR+m.SIZE,lcTempFile)
                 INSERT INTO  (lcTempFile) FROM MEMVAR 
               ENDIF 
             ENDIF 
           ENDFOR 
        ENDSCAN 
      ENDIF 
    ENDSCAN 
  ENDIF 
ENDIF 




*!*************************************************************
*! Name      : lfSRVPO   
*: Developer : Mariam Mazhar (MMT)
*: Date      : 11/05/2008
*! Purpose   : P/O In Range
*!*************************************************************
FUNCTION lfSRVPO   
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO POSHDR.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'
    SELECT POSHDR
    SET RELATION OFF INTO APVENDOR
ENDCASE


*!*************************************************************
*! Name      : lfvPath
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/05/2008
*! Purpose   : Check Path
*!*************************************************************
FUNCTION lfvPath
IF  "?" $ ALLTRIM(lcRpPath)  
 lcRpPath  = GETDIR()
ENDIF
*!*************************************************************
*! Name      : lfwOgWhen
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/05/2008
*! Purpose   : When Function of Option grid
*!*************************************************************
FUNCTION lfwOgWhen
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.

gfOpenTable('Style','Style') 
gfOpenTable('POSHDR','POSHDR') 
gfOpenTable('Scale','Scale') 
gfOpenTable('StyleUpc','StyleUpc') 
gfOpenTable('POSLN','POSLN') 
*!*************************************************************
*! Name      : lfCrtTemp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 11/05/2008
*! Purpose   : Create Temp.
*!*************************************************************
FUNCTION lfCrtTemp
*:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
**:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
**DIMENSION laFilStruct[10,4]
*DIMENSION laFilStruct[11,4]
**:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]
DIMENSION laFilStruct[12,4]
*:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
laFilStruct[1,1] = 'PO' 
laFilStruct[1,2] = 'C'
laFilStruct[1,3] = 6
laFilStruct[1,4] = 0

laFilStruct[2,1] = 'Vendor' 
laFilStruct[2,2] = 'C'
laFilStruct[2,3] = 8
laFilStruct[2,4] = 0

laFilStruct[3,1] = 'Cvensty' 
laFilStruct[3,2] = 'C'
laFilStruct[3,3] = 19
laFilStruct[3,4] = 0

laFilStruct[4,1] = 'STYLE' 
laFilStruct[4,2] = 'C'
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
*laFilStruct[4,3] = 12
laFilStruct[4,3] = lnMajLen  
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]
laFilStruct[4,4] = 0

laFilStruct[5,1] = 'COLOR' 
laFilStruct[5,2] = 'C'
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[Start]
*laFilStruct[5,3] = 6
laFilStruct[5,3] = lnClrLen
*:B609215,1 MMT 04/22/2010 Wrong color Exported in Extened Size Scale companies[End]
laFilStruct[5,4] = 0

laFilStruct[6,1] = 'ClrName' 
laFilStruct[6,2] = 'C'
laFilStruct[6,3] = 30
laFilStruct[6,4] = 0

laFilStruct[7,1] = 'UPC' 
laFilStruct[7,2] = 'C'
laFilStruct[7,3] = 13
laFilStruct[7,4] = 0

laFilStruct[8,1] = 'GrpName' 
laFilStruct[8,2] = 'C'
laFilStruct[8,3] = 30
laFilStruct[8,4] = 0

laFilStruct[9,1] = 'SIZE' 
laFilStruct[9,2] = 'C'
laFilStruct[9,3] = 5
laFilStruct[9,4] = 0

laFilStruct[10,1] = 'PRICEA' 
laFilStruct[10,2] = 'N'
laFilStruct[10,3] = 12
laFilStruct[10,4] = 2

*:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[Start]
laFilStruct[11,1] = 'QTY' 
laFilStruct[11,2] = 'N'
laFilStruct[11,3] = 7
laFilStruct[11,4] = 0
*:C201066,1 MMT 11/13/2008 Add QTY Field to the end of the exported file lines[End]

*:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [Start]
laFilStruct[12,1] = 'DESC' 
laFilStruct[12,2] = 'C'
laFilStruct[12,3] = 20
laFilStruct[12,4] = 0
*:C201544,1 SAB 12/27/2012 Add the Style Description to the end of exported files [End]
 = gfCrtTmp(lcTempFile ,@laFilStruct,"PO+STYLE+COLOR+SIZE" ,lcTempFile ,.T.)

