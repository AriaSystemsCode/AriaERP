*:***********************************************************************
*:  Program file : ICCRTPK.PRG
*:  Program desc.: Create Automatic Pack(Custom for NIR10)
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 05/22/2011
*:      Reference: C201341.exe,C201339.122[T20110223.0011]
*:************************************************************************
*: Modifications:
*:************************************************************************
lcExpr = gfOpGrid('ICCRTPKS' , .T.)
!*************************************************************
*! Name      : lfMajTtlGet
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*!*************************************************************
*! Name      : lfMajPic
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic
*!**************************************************************************
*! Name      : lfSetSTY 
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    =gfOpenTable('Style','Style','SH','STYLE_X')
    *USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.

  CASE lcParm = 'R'  && Reset code
    gfCloseTable('STYLE_X')
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*!*************************************************************
*! Name      : lfCreatPack
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : Create Packs
*!*************************************************************
FUNCTION lfCreatPack
lcCursorStyle = ''
llSelectStyle = .F. 
lnPosStyle = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0 
  lnPosStyle = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosStyle,1)
  lcCursorStyle= loOgScroll.laOgVRFlt[lnPosStyle,6]
  IF !EMPTY(lcCursorStyle) AND USED(lcCursorStyle)
    SELECT(lcCursorStyle)
    LOCATE
    IF !EOF()
      llSelectStyle = .T. 
    ENDIF  
  ENDIF  
ENDIF    
lcStyColorCursor =''
llSelColor = .F.
lnPosStyColor = ASCAN(loOgScroll.laOgvrFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnPosStyColor > 0 
  lnPosStyColor = ASUBSCRIPT(loOGScroll.laOgvrFlt,lnPosStyColor,1)
  lcStyleColor = loOgScroll.laOgvrFlt[lnPosStyColor,6]
  IF !EMPTY(lcStyleColor)
    llSelColor = .T.
    lcStyColorCursor = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='cStyColor'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= lnColorLen
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcStyColorCursor,@laTempacstru,"cStyColor",lcStyColorCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcStyleColor)
    DO WHILE lnEnd <> 0
      SELECT(lcStyColorCursor) 
      APPEND BLANK 
      REPLACE cStyColor WITH SUBSTR(lcStyleColor,lnStart,lnEnd-1)
      lcStyleColor = STUFF(lcStyleColor ,lnStart,lnEnd,"") 
      lnEnd=AT('|',lcStyleColor)
    ENDDO 
    IF lnEnd = 0
      SELECT(lcStyColorCursor) 
      APPEND BLANK 
      REPLACE cStyColor WITH lcStyleColor
    ENDIF 
  ENDIF
ENDIF   

lcSeasonCursor = ''
llSeasonSel = .F.
lnPosSeason = ASCAN(loOgScroll.laOgvrFlt,"STYLE.SEASON")
IF lnPosSeason > 0 
  lnPosSeason = ASUBSCRIPT(loOGScroll.laOgvrFlt,lnPosSeason,1)
  lcSeasons= loOgScroll.laOgvrFlt[lnPosSeason,6]
  IF !EMPTY(lcSeasons)
    llSeasonSel = .T.
    lcSeasonCursor = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='Season'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcSeasonCursor,@laTempacstru,"SEASON",lcSeasonCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcSeasons)
    DO WHILE lnEnd <> 0
      SELECT(lcSeasonCursor) 
      APPEND BLANK 
      REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
      lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"") 
      lnEnd=AT('|',lcSeasons)
    ENDDO 
    IF lnEnd = 0
      SELECT(lcSeasonCursor) 
      APPEND BLANK 
      REPLACE SEASON WITH lcSeasons
   ENDIF
 ENDIF  
ENDIF  
lcDivCursor = ''
llSelDiv = .F.
lnPosDivision = ASCAN(loOgScroll.laOgvrFlt,"STYLE.CDIVISION")
IF lnPosDivision > 0 
 lnPosDivision = ASUBSCRIPT(loOGScroll.laOgvrFlt,lnPosDivision,1)
  lcDivisions = loOgScroll.laOgvrFlt[lnPosDivision,6]
  IF !EMPTY(lcDivisions)
    llSelDiv = .T.
    lcDivCursor = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcDivisions)
    DO WHILE lnEnd <> 0
      SELECT(lcDivCursor) 
      APPEND BLANK 
      REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
      lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"") 
      lnEnd=AT('|',lcDivisions)
    ENDDO 
    IF lnEnd = 0
      SELECT(lcDivCursor) 
      APPEND BLANK 
      REPLACE CDIVISION WITH lcDivisions
    ENDIF 
  ENDIF
ENDIF  
lcStatus = ''
lnPosStatus= ASCAN(loOgScroll.laOgvrFlt,"STYLE.STATUS")  
IF lnPosStatus > 0 
  lnPosStatus = ASUBSCRIPT(loOGScroll.laOgvrFlt,lnPosStatus,1)
  lcStatus= loOgScroll.laOgvrFlt[lnPosStatus,6]
ENDIF 
IF !USED('Style_D')
  =gfOpenTable('Style','Style','SH','Style_D')
ENDIF
SELECT Style_D
DIMENSION laStyCur[4,4]
laStyCur[1,1] = 'STYLE'
laStyCur[1,2] = 'C'
laStyCur[1,3] = 19
laStyCur[1,4] = 0
laStyCur[2,1] = 'SCALE'
laStyCur[2,2] = 'C'
laStyCur[2,3] = 3
laStyCur[2,4] = 0

laStyCur[3,1] = 'SEASON'
laStyCur[3,2] = 'C'
laStyCur[3,3] = 6
laStyCur[3,4] = 0

laStyCur[4,1] = 'CDIVISION'
laStyCur[4,2] = 'C'
laStyCur[4,3] = 6
laStyCur[4,4] = 0

=gfCrtTmp(lcStyTmp, @laStyCur, "Style", lcStyTmp)
lnmajlength = LEN(gfItemMask('PM'))

IF llSelectStyle 
  SELECT(lcCursorStyle)
  LOCATE
  SCAN 
    =gfSeek(SUBSTR(&lcCursorStyle..cStyMajor,1,lnmajlength),'Style_D')  
    SELECT Style_D
    SCAN REST WHILE SUBSTR(Style,1,lnmajlength)  = SUBSTR(&lcCursorStyle..cStyMajor,1,lnmajlength) FOR IIF(!EMPTY(lcStatus),Style_d.Status $ lcStatus,.T.) AND ;
    				IIF(llSeasonSel ,SEEK(Style_d.Season,lcSeasonCursor),.T. ) AND IIF(llSelDiv ,SEEK(Style_d.CDIVISION ,lcDivCursor ),.T. ) AND ;
    				IIF(llSelColor ,SEEK(SUBSTR(Style_D.Style,lnClrPo,lnColorLen),lcStyColorCursor ),.T. ) 
      IF !SEEK(Style_D.STYLE,lcStyTmp)    				
        INSERT INTO (lcStyTmp) VALUES (Style_D.STYLE,Style_D.SCALE,Style_D.SEASON,Style_D.CDIVISION)
      ENDIF
    ENDSCAN 
  ENDSCAN  
ELSE
  SELECT Style_D
  SCAN FOR IIF(!EMPTY(lcStatus),Style_d.Status $ lcStatus,.T.) AND ;
				IIF(llSeasonSel ,SEEK(Style_d.Season,lcSeasonCursor),.T. ) AND IIF(llSelDiv ,SEEK(Style_d.CDIVISION ,lcDivCursor ),.T. ) AND ;
				IIF(llSelColor ,SEEK(SUBSTR(Style_D.Style,lnClrPo,lnColorLen),lcStyColorCursor ),.T. ) 
    IF !SEEK(Style_D.STYLE,lcStyTmp)    				
      INSERT INTO (lcStyTmp) VALUES (Style_D.STYLE,Style_D.SCALE,Style_D.SEASON,Style_D.CDIVISION)
    ENDIF
  ENDSCAN 
ENDIF
SELECT (lcStyTmp) 
LOCATE 
IF EOF()
  = gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF

IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF
IF !USED('SPCK_HDR')
  =gfOpenTable('SPCK_HDR','SPCK_HDR')
ENDIF
IF !USED('SPCK_LIN')
  =gfOpenTable('SPCK_LIN','SPCK_LIN')
ENDIF

lcGenAcc = '*****'

*Create PACK
llOpenRep   = .F.
lcFilHandl = 0
SCAN
  =gfSeek('S'+&lcStyTmp..Scale,'SCALE')
  FOR lnX=1 TO scale.cnt
    lcSz=STR(lnX,1)
    lcScaleSz = Scale.Sz&lcSz.
    lcPackID = UPPER(PADR(ALLTRIM(SUBSTR(&lcStyTmp..STYLE,1,lnmajlength))+"/"+ALLTRIM(SUBSTR(&lcStyTmp..Style,lnClrPo,lnColorLen))+"/"+ALLTRIM(lcScaleSz),16))
    IF gfSeek('P'+lcGenAcc +lcPackID ,'SPCK_HDR')
       lfAddLog('Pack: '+lcPackID +"  is already existing in data file.")
       LOOP       
    ENDIF
    WAIT WINDOW "Creating Pack: "+lcPackID  NOWAIT 
    SELECT SPCK_HDR
    APPEND BLANK 
    REPLACE TYPE WITH 'P',;
    		Pack_ID WITH lcPackID ,;
    		STYLE WITH '',;
    		Desc WITH '',;
    		SKU WITH '',;
    		Account WITH lcGenAcc ,;
    		SEASON  WITH &lcStyTmp..SEASON ,;
    		CDIVISION WITH &lcStyTmp..CDIVISION

    =gfAdd_Info('SPCK_HDR')    		
    SELECT SPCK_LIN
    APPEND BLANK 
    REPLACE TYPE WITH 'P',;
    		Pack_ID WITH lcPackID ,;
    		STYLE WITH &lcStyTmp..STYLE,;
    		Account WITH lcGenAcc ,;
    		SKU WITH '',;
    		Qty1 With 0,;
    		Qty2 With 0,;
    		Qty3 With 0,;
    		Qty4 With 0,;
    		Qty5 With 0,;
    		Qty6 With 0,;
    		Qty7 With 0,;
    		Qty8 With 0,;
    		Qty&lcSz. WITH 3,;
    		TOTQTY WITH 3
    =gfAdd_Info('SPCK_LIN')    		    		
  ENDFOR
ENDSCAN 

IF !llOpenRep   
  lfAddLog('No Errors Found.')    
ENDIF

IF llOpenRep   
  =FCLOSE(lcFilHandl)
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (oAriaApplication.WorkDir + lcLogTmp+'.txt') OVERWRITE
  SCATTER MEMVAR MEMO
  APPEND BLANK
  GATHER MEMVAR MEMO 
  LOCATE
  REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
                       "*                                Error log                                 *" + CHR(13) +;
                       REPLICATE('*',68) + CHR(13) + ' ' + CHR(13) + ;
                       mStrRep
  
  
  IF oAriaApplication.MULTIINST 
    =gfCallForm('ICERRLOG','IC')
  ELSE
    DO FORM (oAriaApplication.ScreenHome + 'IC\ICERRLOG')
  ENDIF 
  USE IN TMPSTR
ENDIF

SELECT SPCK_LIN
=gfTableUpdate()
SELECT SPCK_HDR
=gfTableUpdate()
RETURN .F.
*!*************************************************************
*! Name      : lfAddLog
*: Developer : Mariam Mazhar
*: Date		 : 05/22/2011
*! Purpose   : Add Line to Error Log 
*!*************************************************************
FUNCTION lfAddLog
LPARAMETERS lcTxtMsg  
IF !llOpenRep  
  llOpenRep = .T.
  lcFilHandl = FCREAT(oAriaApplication.WorkDir + lcLogTmp+'.txt')
ENDIF
=FPUTS(lcFilHandl,lcTxtMsg)
