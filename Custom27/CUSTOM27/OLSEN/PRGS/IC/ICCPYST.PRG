*:*****************************************************************************************
*: Program file  : ICCPYST.PRG
*: Program desc. : Copying styles from one company to another with it's corresponding data.
*:               : For Olsen (C#200239)
*:               : Converted from Ols1200 (2.6 ver.)
*: For screen    : ICCPYSTY.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: HEND GHANEM (HBG)
*:          Date : 18/10/2001
*:*****************************************************************************************
STORE SPACE(20) TO lcComp1n,lcComp2n
STORE SPACE(12) TO lcStyle1,lcStyle2
STORE SPACE(2) TO lcComp1,lcComp2,lcseason,lcgroup
STORE SPACE(6) TO lcdir1,lcdir2,lcCdir1,lcCdir2
STORE 1   TO lnSea,lnGrp
lcWinTitl = gfTempName()
DIMENSION laCodInfo [2,10]
laCodInfo = SPACE(0)

STORE .F. TO llbrowse

*** Fill the "Codes Information" array.
*-Season array.
laCodInfo[1,01] = "SEASON"
laCodInfo[1,02] = "laSea"
laCodInfo[1,03] = "lnSea"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .T.            
laCodInfo[1,07] = "STYLE"     
laCodInfo[1,08] = "STYLE"     
laCodInfo[1,09] = "STYLE"     
laCodInfo[1,10] = "Season"     
*-Style Group array.
laCodInfo[2,01] = "CSTYGROUP"
laCodInfo[2,02] = "laGrp"
laCodInfo[2,03] = "lnGrp"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.            
laCodInfo[2,06] = .T.            
laCodInfo[2,07] = "STYLE"     
laCodInfo[2,08] = "STYLE"     
laCodInfo[2,09] = "STYLE"
laCodInfo[2,10] = "cStyGroup"  

*** Initialize Codes arrays.
DIMENSION  laSea[1],laGrp[1]
STORE ''  TO laSea,laGrp
STORE 1   TO lnSea,lnGrp

= gfwCodePop(@laCodInfo, "SEASON"   ,"N" )
= gfwCodePop(@laCodInfo, "CSTYGROUP","N" )

=gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')

ON KEY LABEL esc        DO lpFPesc

DO (gcScrDir+gcWinAppl+"\ICCPYST.SPX")

ON KEY LABEL ESC
ON KEY LABEL ENTER
*!*************************************************************
*! Name   : FUNCTION lfvComp1
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose: The valid function of the source company.
*!*************************************************************
FUNCTION lfvComp1

DIMENSION laSelected[3]
SELECT SYCCOMP
IF llbrowse OR !EMPTY(lcComp1) AND !SEEK(lcComp1)
  llbrowse = .F.
  lcbrfields = "ccomp_id  :15:H='Company ID',"+;
               "cCom_Name :30:H='Company Name'"
  
  llComp = AriaBrow('',"Companies",;
                15, 01,15,02,;
                .F.,.F.,"ccomp_id,cCom_Name,cCom_Ddir","laSelected")

  IF llComp 
    lcComp1  = ALLTRIM(laSelected[1])
    lcComp1n = ALLTRIM(laSelected[2])
    lcCdir1  = ALLTRIM(laSelected[3])
  ELSE
    _CUROBJ = OBJNUM(lcCOMP1)
    SHOW GET lcComp1 ENABLE
    lcComp1n = " " 
    lcComp1 = "  "         
  ENDIF 

ELSE
  IF !EMPTY(lcComp1)
    lcComp1n = ALLTRIM(cCom_Name)
    lcCdir1  = ALLTRIM(cCom_Ddir)
  ELSE
    lcComp1n = " " 
    lcComp1  = "  "         
  ENDIF  
ENDIF

*--Check if copy the styles to the same company
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(lcComp1) AND ALLTRIM(lcComp1) = ALLTRIM(lcComp2)
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'You cannot copy the styles to the same company.')
  _CUROBJ = OBJNUM(lcCOMP1)
  SHOW GET lcComp1 ENABLE
  lcComp1n = " " 
  lcComp1 = "  "
ENDIF
SET EXACT &lcExact

lcStyle1= SPACE(12)
lcStyle2= SPACE(12)
lnSea    = 1
lnGrp    = 1
lcseason =laSea[lnSea,2]
lcgroup  =laGrp[lnGrp,2]

IF !EMPTY(lcComp1) .AND. !EMPTY(lcComp2)

  IF USED('CODES')
    SELECT CODES
    USE
  ENDIF
  = gfOpenFile(lcdir1+'CODES' ,'CODES','SH')
  SHOW GET lcstyle1 ENABLE
  SHOW GET lcstyle2 ENABLE
  SHOW GET ibstyle1 ENABLE
  SHOW GET ibstyle2 ENABLE
  SHOW GET lnSea    ENABLE
  SHOW GET lnGrp    ENABLE
  SHOW GET Pbproceed ENABLE
  SHOW GET pbCancel,1 PROMPT '\<Cancel '
ELSE
  SHOW GET Pbproceed DISABLE
  SHOW GET pbCancel,1 PROMPT '\<Close '
ENDIF

lcdir1 = ALLTRIM(lcCdir1)
lcdir2 = ALLTRIM(lcCdir2)
=lfRefresh()


*!*************************************************************
*! Name   : FUNCTION lfvComp1
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose:The valid function of the target company.
*!*************************************************************
FUNCTION lfvComp2

DIMENSION laSelected[3]
SELECT SYCCOMP
IF llbrowse OR !EMPTY(lcComp2) AND !SEEK(lcComp2)
  llbrowse = .F.
  lcbrfields = "ccomp_id  :15:H='Company ID',"+;
               "cCom_Name :30:H='Company Name'"
  
  llComp = AriaBrow('',"Companies",;
                15, 01,15,02,;
                .F.,.F.,"ccomp_id,cCom_Name,cCom_Ddir","laSelected")

  IF llComp 
    lcComp2  = ALLTRIM(laSelected[1])
    lcComp2n = ALLTRIM(laSelected[2])
    lcCdir2  = ALLTRIM(laSelected[3])
  ELSE
    _CUROBJ = OBJNUM(lcCOMP2)
    SHOW GET lcComp1 ENABLE
    lcComp2n = " " 
    lcComp2  = "  "         
  ENDIF 
ELSE
  IF !EMPTY(lcComp2)
    lcComp2n = ALLTRIM(cCom_Name)
    lcCdir2  = ALLTRIM(cCom_Ddir)
  ELSE
    lcComp2n = " " 
    lcComp2  = "  "     
  ENDIF  
ENDIF

*--Check if copy the styles to the same company
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(lcComp2) AND ALLTRIM(lcComp1) = ALLTRIM(lcComp2)
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'You cannot copy the styles to the same company.')
  _CUROBJ = OBJNUM(lcCOMP2)
  SHOW GET lcComp2 ENABLE
  lcComp2n = " " 
  lcComp2 = "  "
ENDIF
SET EXACT &lcExact

IF !EMPTY(lcComp1) .AND. !empty(lcComp2)

  IF USED('CODES')
    SELECT CODES
    USE
  ENDIF
  = gfOpenFile(lcdir1+'CODES' ,'CODES','SH')

  SHOW GET lcstyle1  ENABLE
  SHOW GET lcstyle2  ENABLE
  SHOW GET ibstyle1  ENABLE
  SHOW GET ibstyle2  ENABLE
  SHOW GET lnSea     ENABLE
  SHOW GET lnGrp     ENABLE
  SHOW GET Pbproceed ENABLE
  SHOW GET pbCancel,1 PROMPT '\<Cancel '
ELSE
  SHOW GET Pbproceed DISABLE
  SHOW GET pbCancel,1 PROMPT '\<Close '
ENDIF
lcdir1 = " " 
lcdir2 = " " 
lcdir1 = ALLTRIM(lcCdir1)
lcdir2 = ALLTRIM(lcCdir2)
=lfRefresh()


*!*************************************************************
*! Name   : FUNCTION lfvStyle1
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose:The valid function of the source style.
*!*************************************************************
FUNCTION lfvStyle1

IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF
= gfOpenFile(lcdir1+'STYLE' ,'cStyle','SH')

*--Get the style major length 
IF USED('ICISTRU')
  SELECT ICISTRU
  USE
ENDIF
= gfOpenFile(lcdir1+'ICISTRU' ,'SEGNO','SH')
=SEEK('U1')
lnstylelen = ICISTRU.nisegsize

lcStyle1 = PADR(lcStyle1,lnstylelen)

IF llbrowse OR !SEEK(lcStyle1,'STYLE') .AND. !EMPTY(lcStyle1)
  llbrowse = .F.
  lcStyle1 = gfStyBrw('M',"","",.F.)
ENDIF

*!*************************************************************
*! Name   : FUNCTION lfvStyle2
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose:The valid function of the target style.
*!*************************************************************
FUNCTION lfvStyle2

IF llbrowse OR !EMPTY(lcStyle2)
  
  *--Get the style major length 
  IF USED('ICISTRU')
    SELECT ICISTRU
    USE
  ENDIF
  = gfOpenFile(lcdir1+'ICISTRU' ,'SEGNO','SH')
  =SEEK('U1')
  lnstylelen = ICISTRU.nisegsize
    
  lcStyle2 = PADR(lcStyle2,lnstylelen)
  IF USED('STYLE')
    SELECT STYLE
    USE
  ENDIF
  = gfOpenFile(lcdir1+'STYLE' ,'cStyle','SH')

  IF llbrowse OR !EMPTY(lcStyle2) .AND. !SEEK(lcStyle2,'STYLE') 
    llbrowse = .F.
    lcStyle2 = gfStyBrw('M',"","",.F.)
  ENDIF
  
  IF lcStyle2 < lcStyle1 
    =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'From/thru style selection error!')
    lcStyle2 = SPACE(12)
    _CUROBJ = OBJNUM(lcStyle2)
    SHOW GET ibStyle2 ENABLE
    SHOW GET lcStyle2 ENABLE
  ENDIF
ENDIF

*!*************************************************************
*! Name   : FUNCTION lfvCopy
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose:The valid function of the Proceed button.
*!*************************************************************
FUNCTION lfvCopy


*--Create the temp files.
lcTmpSty   = gfTempName ()
lcTmpCode  = gfTempName()
lcTmpScale = gfTempName()

*-- Copy source Codes file to a temp file to be used insted
IF USED('CODES')
  SELECT CODES
  USE
ENDIF
= gfOpenFile(lcdir1+'CODES' ,'CODES','SH')
COPY ALL TO (gcWorkDir+lcTmpCode)

*-- Copy source Scale file to a temp file to be used instead
IF USED('SCALE')
  SELECT SCALE
  USE
ENDIF
=gfOpenFile(lcdir1+'SCALE','SCALE','SH')
COPY ALL TO (gcWorkDir+lcTmpScale)

=gfOpenFile(gcWorkDir+lcTmpCode,'','EX')
INDEX ON cdefcode+ccode_no+crltfield+cfld_name TAG &lcTmpCode

=gfOpenFile(gcWorkDir+lcTmpScale,'','EX')
INDEX ON TYPE+SCALE+PREPAK TAG &lcTmpScale

IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF
= gfOpenFile(lcdir1+'STYLE' ,'cStyle','SH')

*-- Select te selected styles into a temp file 
SELECT STYLE
GO TOP
WAIT WINDOW "Collecting data" NOWAIT
DO CASE
   CASE !EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cStyMajor >= lcStyle1 .AND. cstymajor <= lcStyle2 .AND. SEASON=lcSeason;
	 .AND. cStyGroup = lcgroup INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY Style
	
   CASE !EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >=lcStyle1 .AND. cstymajor =lcSeason;
	 .AND. cStyGroup = lcgroup INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY Style
	
   CASE EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE SEASON = lcSeason .AND. cStyGroup = lcgroup;
     INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY Style
    
   CASE EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor <=lcStyle2 .AND. SEASON = lcSeason .AND.;
     cStyGroup = lcgroup INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
	
   CASE !EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 .AND. cstymajor <= lcStyle2;
     .AND. cStyGroup = lcgroup INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style

   CASE !EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 .AND. cStyGroup = lcgroup ;
     INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
     	
   CASE !EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 .AND. cstymajor <= lcStyle2;
     .AND. SEASON = lcSeason	INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style

   CASE !EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 .AND. SEASON = lcSeason ;
     INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
	
   CASE !EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 .AND. cstymajor <=lcStyle2;
     INTO TABLE (gcWorkDir+lcTmpSty)	ORDER BY style

   CASE !EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor >= lcStyle1 INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
	
   CASE EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cStyGroup = lcGroup INTO TABLE (gcWorkDir+lcTmpSty);
     ORDER BY style
    
   CASE EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. !EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor <= lcSTYLE2 .AND. cStyGroup = lcGroup INTO TABLE (gcWorkDir+lcTmpSty);
     ORDER BY style
    
   CASE EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE SEASON = lcSeason INTO TABLE (gcWorkDir+lcTmpSty);
     ORDER BY style
    
   CASE EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. !EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE WHERE cstymajor <= lcStyle2 .AND. SEASON = lcSeason INTO TABLE (gcWorkDir+lcTmpSty);
     ORDER BY style

   CASE EMPTY(lcStyle1) .AND. EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. EMPTY(lcGroup)
     SELECT * FROM STYLE INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
   
  CASE EMPTY(lcStyle1) .AND. !EMPTY(lcStyle2) .AND. EMPTY(lcSeason) .AND. EMPTY(lcGroup)
    SELECT * FROM STYLE WHERE cstymajor <= lcStyle2 INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
  
  OTHERWISE
    SELECT * FROM STYLE WHERE .F. INTO TABLE (gcWorkDir+lcTmpSty) ORDER BY style
ENDCASE

*-- If no styles selected , display a massage then return from the function
SELECT &lcTmpSty
LOCATE
IF EOF()
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'No Styles selected.')
  STORE 1   TO lnSea,lnGrp 
  lcseason =laSea[lnSea,2]
  lcgroup  =laGrp[lnGrp,2]
  SHOW GET lnSea
  SHOW GET lnGrp 
  =lfRefresh()
  RETURN
ENDIF

*-- Open the distenation files "STYLE,CODES,SCALE"
IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF  
=gfOpenFile(lcdir2+'STYLE',lcdir2+'STYLE','SH')

IF USED('CODES')
  SELECT CODES
  USE
ENDIF
= gfOpenFile(lcdir2+'CODES' ,'CODES','SH')

IF USED('SCALE')
  SELECT SCALE
  USE
ENDIF
=gfOpenFile(lcdir2+'SCALE','SCALE','SH')

*-- Get the Style major and non major length from the distenation company
IF USED('ICISTRU')
  SELECT ICISTRU
  USE
ENDIF
= gfOpenFile(lcdir2+'ICISTRU' ,'SEGNO','SH')

=SEEK('U1')
lnstylelen = ICISTRU.nisegsize
=SEEK('U2')
lnNMjrlen  = ICISTRU.nisegsize

= gfOpenFile(lcdir2+'WAREHOUS' ,'WAREHOUS','SH')

*-- Scan Temp style file to update the selected styles into the 
*-- files "STYLE,CODES,SCALE" in the distenation company.
llFlag = .F.
SELECT &lcTmpSty
SCAN
  Xdyelot     = CDYE_FLG
  Xpattern    = PATTERN
  Xstatus     = STATUS
  Xgroup      = CSTYGROUP  
  Xscale      = SCALE
  Xprepak     = PREPAK
  Xdivision   = CDIVISION
  Xdesc       = DESC
  Xdesc1      = DESC1
  Xseason     = SEASON
  Xweight     = NSTYWEIGHT
  Xcommision  = COMMISSION
  Xlink_code  = LINK_CODE
  Xlocation   = LOCATION
  Xcconsinfo1 = CCONSINFO1  
  Xcconsinfo2 = CCONSINFO2
  Xcontent1   = CONTENT1
  Xcontent2   = CONTENT2
  IF !SEEK(CDEFWARE,'WAREHOUS')
    GO TOP IN WAREHOUS
    Xcdefware   = WAREHOUS.cwarecode
  ELSE
    Xcdefware   = CDEFWARE
  ENDIF  
  *-- get Royalty code
  IF SEEK('N' + ROYALTY,'CODES')
    Xroyalty    = ROYALTY
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'ROYALTY','CODES')
    Xroyalty    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  *-- get Purches code
  IF SEEK('N' + CPURCODE  ,'CODES')
    XPurCode    = CPURCODE  
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'CPURCODE','CODES')
    XPurCode    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  *-- get Discount code
  IF SEEK('N' + CDISCCODE ,'CODES')
    XdiscCode    = CDISCCODE 
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'CDISCCODE','CODES')
    XdiscCode    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  Xctaxcode   = CTAXCODE
  Xccomcode   = CCOMCODE
  Xcvensty    = CVENSTY
  Xqty_ctn    = QTY_CTN
  Xstart      = START
  Xsoldout    = SOLDOUT
  Xmake       = MAKE
  lcSty       = STYLE
  XStyMaj     = CSTYMAJOR
  *--If the style was copied before or already found in the distenation company ignor it.
  IF SEEK(lcSty,'STYLE')
  	WAIT WINDOW "STYLE " + ALLTRIM(lcsty) +" ALREADY EXIST" NOWAIT
  ELSE    && If the style is not found in the distenation company update it
    llFlag = .T.
    SELECT STYLE
    APPEND BLANK
    REPLACE STYLE      WITH lcsty      ,CDYE_FLG   WITH Xdyelot    ,PATTERN   WITH Xpattern,;
            STATUS     WITH Xstatus    ,CSTYGROUP  WITH Xgroup     ,SCALE     WITH Xscale,;
            PREPAK     WITH Xprepak    ,CDIVISION  WITH Xdivision  ,DESC      WITH Xdesc,;
            DESC1      WITH Xdesc1     ,SEASON     WITH Xseason    ,LOCATION  WITH Xlocation,;
            NSTYWEIGHT WITH XWEIGHT    ,COMMISSION WITH Xcommision ,LINK_CODE WITH Xlink_code,;
            CCONSINFO1 WITH Xcconsinfo1,CCONSINFO2 WITH Xcconsinfo2,CONTENT1  WITH Xcontent1,;
            CONTENT2   WITH Xcontent2  ,CDEFWARE   WITH Xcdefware  ,CTAXCODE  WITH Xctaxcode,;
            CCOMCODE   WITH Xccomcode  ,CVENSTY    WITH Xcvensty   ,QTY_CTN   WITH Xqty_ctn,;
            START      WITH Xstart     ,SOLDOUT    WITH Xsoldout   ,MAKE      WITH Xmake,;
            CSTYMAJOR  WITH XStyMaj    ,CDISCCODE  WITH XdiscCode  ,CPURCODE  WITH XPurCode,;
            ROYALTY    WITH Xroyalty           
 
    
    SELECT &lcTmpCode
    SEEK 'N' + SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen )
    SCATTER TO laClr
    
    SEEK 'N'+&lcTmpSty..cStyGroup
    SCATTER TO laGroup
    
    SEEK 'N'+&lcTmpSty..cDivision
    SCATTER TO laDiv
    
    SEEK 'N'+&lcTmpSty..Season
    SCATTER TO laSeason

    IF SEEK('S'+&lcTmpSty..SCALE,lcTmpScale)
      SELECT &lcTmpScale
      SCATTER TO laSca
      SELECT SCALE
      IF !EMPTY(&lcTmpSty..SCALE) .AND. !SEEK('S'+PADR(&lcTmpScale..SCALE,3))
 	    APPEND BLANK
        GATHER FROM laSca
      ENDIF
    ELSE
       WAIT WINDOW 'Scale '+&lcTmpSty..SCALE+' not found,cannot copy.'  NOWAIT
    ENDIF
  
    SELECT &lcTmpScale
    IF SEEK('P'+&lcTmpSty..SCALE)
      SCAN WHILE('P'+&lcTmpScale..SCALE = 'P'+&lcTmpSty..SCALE)
        SCATTER TO laPre
        SELECT SCALE    
        IF !EMPTY(&lcTmpScale..Prepak) AND;
           !SEEK('P'+PADR(&lcTmpScale..SCALE,3)+ALLTRIM(&lcTmpScale..Prepak))
          APPEND BLANK
          GATHER FROM laPre
        ENDIF
      ENDSCAN
    ELSE
      WAIT WINDOW 'Prepak '+&lcTmpSty..Prepak+' not found,cannot copy.' NOWAIT
    ENDIF 

    SELECT CODES
    IF !EMPTY(laClr(1)) .AND. !SEEK('N'+SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen)) .AND. ;
                               !SEEK('D'+SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen))
      APPEND BLANK
      GATHER FROM laClr 
    ENDIF
    IF !EMPTY(laGroup(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..CSTYGROUP,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..CSTYGROUP,6))
      APPEND BLANK
      GATHER FROM laGroup 
    ENDIF 		
    IF !EMPTY(laDiv(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..cDivision,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..cDivision,6))
      APPEND BLANK
      GATHER FROM laDiv 
    ENDIF
    IF !EMPTY(laSeason(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..Season,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..Season,6))
      APPEND BLANK   
      GATHER FROM laSeason 
    ENDIF
  ENDIF

  SELECT &lcTmpSty
ENDSCAN

WAIT CLEAR
IF llFlag = .T.
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'The styles are successfully copied.')
ENDIF  

STORE SPACE(20) TO lcComp1n,lcComp2n
STORE SPACE(12) TO lcStyle1,lcStyle2
STORE SPACE(2) TO lcComp1,lcComp2
STORE 1   TO lnSea,lnGrp
lcseason =laSea[lnSea,2]
lcgroup  =laGrp[lnGrp,2]
SHOW GET lcComp1
SHOW GET lcComp2
SHOW GET lnSea
SHOW GET lnGrp
STORE SPACE(6) TO lcdir1,lcdir2,lcCdir1,lcCdir2
SHOW GET lcstyle1 DISABLE
SHOW GET lcstyle2 DISABLE
SHOW GET ibstyle1 DISABLE
SHOW GET ibstyle2 DISABLE
SHOW GET lnSea    DISABLE
SHOW GET lnGrp    DISABLE
SHOW GET Pbproceed DISABLE
SHOW GET pbCancel,1 PROMPT '\<Close '
=lfRefresh()



*!*************************************************************
*! Name      : lfOldValue
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose: Function to store old value of the current field.
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name   : lpFPesc
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose: Traping for ESC 
*!*************************************************************
PROCEDURE lpFPesc
_CUROBJ = OBJNUM(pbCancel)
KEYBOARD CHR(17)+CHR(13)
