***********************************************************************
*:  Program file : ICEXQTY.PRG
*:  Program desc.: Export Style Qty to CSV
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 03/16/2015
*:      Reference: C201658[T20150203.0006]
**************************************************************************
* Modifications:
*:************************************************************************
PARAMETERS lcRequestID, lcXMLFileName, ClientID

IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  
  LOCAL loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientId = ClientId
  
  LOCAL lcCurrentProcedure
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()  
  SET DEFAULT TO &lcCurrentProcedure.
 
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  oAriaEnvironment.REPORT.gcAct_Appl = 'IC'
  PUBLIC gcAct_Appl
  gcAct_Appl = 'IC'
  oAriaEnvironment.activeModuleID = 'IC'
  lfExport()
ELSE
  lcExpr = gfOpGrid('ICEXPQT' , .T.)
ENDIF

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2015
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

*-- Major length
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
lnNonMajSt = 0
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnClrPo = laMajSegs[lnI,4]      && This item hold seg. start position.
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's'

*-- Compute Free/Color Items in Style Structure. [End]
lcIMjrPt   = gfItemMask('PI')
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)
lcSepart   = SUBSTR(lcIMjrPt,lnstylewid+1,1)

RETURN ''
*--End of lfEvalSegs.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2016
*! Purpose   : Raise change style flag, in range browse screen.
*!*************************************************************
*! Called from : ICINVAG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.

  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*--End of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mariam Mazhar[MMT]
*! Date      : 03/16/2015
*! Purpose   : Sum a specific field for the current style in style file.
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar

PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

RETURN INT(lnTotcomp)
*--End of lfStySum.
*!*************************************************************
*! Name      : lfWhenExp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2015
*! Purpose   : When Function
*!*************************************************************
FUNCTION lfWhenExp
*!*************************************************************
*! Name      : lfExport
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2016
*! Purpose   : Export function
*!*************************************************************
FUNCTION lfExport
IF TYPE("gcRequestXMLFileName") = 'C'
  RETURN 
ENDIF
**Collecting data
lcCursorStyle = ''
llSelectStyle = .F. 
lnPosStyle = ASCAN(laOgFXFLT,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0 
  lnPosStyle = ASUBSCRIPT(laOgFXFLT,lnPosStyle,1)
  lcCursorStyle= laOgFXFLT[lnPosStyle,6]
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
lnPosStyColor = ASCAN(laOgFXFLT,"SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)")
IF lnPosStyColor > 0 
  lnPosStyColor = ASUBSCRIPT(laOgFXFLT,lnPosStyColor,1)
  lcStyleColor = laOgFXFLT[lnPosStyColor,6]
  IF !EMPTY(lcStyleColor)
    llSelColor = .T.
    lcStyColorCursor = gfTempName()

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

lcPatt = ''
lnPatPos = ASCAN(laOgFXFLT,"STYLE.PATTERN")
IF lnPatPos >0
  lnPatPos  = ASUBSCRIPT(laOgFXFLT,lnPatPos ,1)
  lcPatt  = laOgFXFLT[lnPatPos ,6]
ENDIF

lcSeasonCursor = ''
llSeasonSel = .F.
lnPosSeason = ASCAN(laOgFXFLT,"STYLE.SEASON")
IF lnPosSeason > 0 
  lnPosSeason = ASUBSCRIPT(laOgFXFLT,lnPosSeason,1)
  lcSeasons= laOgFXFLT[lnPosSeason,6]
  IF !EMPTY(lcSeasons)
    llSeasonSel = .T.
    lcSeasonCursor = gfTempName()

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
lnPosDivision = ASCAN(laOgFXFLT,"STYLE.CDIVISION")
IF lnPosDivision > 0 
 lnPosDivision = ASUBSCRIPT(laOgFXFLT,lnPosDivision,1)
  lcDivisions = laOgFXFLT[lnPosDivision,6]
  IF !EMPTY(lcDivisions)
    llSelDiv = .T.


    lcDivCursor = gfTempName()

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


lcGrpCursor = ''
llSelGrp = .F.
lnPosGroup = ASCAN(laOgFXFLT,"STYLE.CSTYGROUP")
IF lnPosGroup > 0 
 lnPosGroup = ASUBSCRIPT(laOgFXFLT,lnPosGroup ,1)
  lcGroups = laOgFXFLT[lnPosGroup ,6]
  IF !EMPTY(lcGroups)
    llSelGrp = .T.


    lcGrpCursor = gfTempName()

    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CSTYGROUP'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcGrpCursor ,@laTempacstru,"CSTYGROUP",lcGrpCursor ,.T.)
    lnStart=1
    lnEnd=AT('|',lcGroups)
    DO WHILE lnEnd <> 0
      SELECT(lcGrpCursor) 
      APPEND BLANK 
      REPLACE CSTYGROUP WITH SUBSTR(lcGroups,lnStart,lnEnd-1)
      lcGroups = STUFF(lcGroups,lnStart,lnEnd,"") 
      lnEnd=AT('|',lcGroups)
    ENDDO 
    IF lnEnd = 0
      SELECT(lcGrpCursor) 
      APPEND BLANK 
      REPLACE CSTYGROUP WITH lcGroups
    ENDIF 
  ENDIF
ENDIF  


lcStatus = ''
lnPosStatus= ASCAN(laOgFXFLT,"STYLE.STATUS")  
IF lnPosStatus > 0 
  lnPosStatus = ASUBSCRIPT(laOgFXFLT,lnPosStatus,1)
  lcStatus= laOgFXFLT[lnPosStatus,6]
ENDIF 

lfCreateTemp()
lcMjrlen   = LEN(gfItemMask('PM'))
IF !USED('Style_Select')
  =gfOpenTable('STYLE','STYLE','SH','Style_Select')
ENDIF
IF !USED('StyDye_Select')
  =gfOpenTable('STYDYE','STYDYE','SH','StyDye_Select')
ENDIF

IF !USED('Scale_Select')
  =gfOpenTable('SCALE','SCALE','SH','Scale_Select')
ENDIF

lcQtyType ='S'
IF llSelectStyle 
  SELECT (lcCursorStyle)
  SCAN
   SELECT Style_Select
   =gfSeek(SUBSTR(&lcCursorStyle..cStyMajor,1,lcMjrlen))
   SCAN REST WHILE STYLE = SUBSTR(&lcCursorStyle..cStyMajor,1,lcMjrlen) FOR !EMPTY(STYLE) AND !EMPTY(cStyMajor) AND IIF(!EMPTY(lcStatus),Style_Select.STATUS $ lcStatus,.T.) AND ;
   	               IIF(llSelGrp ,SEEK(Style_Select.CSTYGROUP,lcGrpCursor),.T.) AND IIF(llSelDiv,SEEK(Style_Select.CDIVISION,lcDivCursor),.T.)  AND ;
   	               IIF(llSeasonSel,SEEK(Style_Select.SEASON,lcSeasonCursor),.T.) AND IIF(llSelColor ,SEEK(SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen),lcStyColorCursor),.T.) AND ;
   	               IIF(!EMPTY(ALLTRIM(lcPatt)),Style_Select.pattern=lcPatt,.T.)
     IF TYPE('lcXMLFileName')<> 'C'
       WAIT WINDOW 'Collecting Data for Style: '+Style_Select.STyle NOWAIT                     
     ELSE
       IF MOD(RECNO(lcCursorStyle), CEILING(RECCOUNT(lcCursorStyle)/10)) = 0
         loProgress.Percent = (RECNO(lcCursorStyle) /RECCOUNT(lcCursorStyle)) * 0.9
         loProgress.DESCRIPTION = 'Collecting Data for Style: '+Style_Select.STyle 
         loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
       ENDIF  
    
     ENDIF  
     =gfSeek('S'+Style_Select.SCALE,'Scale_Select','Scale')               
     m.Style = Style_Select.cStyMajor
     m.Desc = Style_Select.DESC
	 m.DESC1 = Style_Select.DESC1
     m.Scale = Scale_Select.Scale
	 m.Color = SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen)
     m.ClrDesc = gfCodDes(SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen),'COLOR')
     m.PriceA= Style_Select.PRICEA 
	 m.PriceB= Style_Select.PRICEB
	 m.PriceC= Style_Select.PRICEC 
	 m.nsugretpri = Style_Select.nsugretpri
     m.Season = Style_Select.Season
     m.Division  = Style_Select.cDivision
     FOR lnQtyCnt = 1 TO 8
       lcQtyCnt  = STR(lnQtyCnt ,1)
       STORE 0 TO m.Qty&lcQtyCnt.,m.TOTQTY
       STORE '' TO m.Size&lcQtyCnt.
     ENDFOR 
     FOR lnQtyCnt = 1 TO 8
       lcQtyCnt  = STR(lnQtyCnt ,1)
       STORE Scale_Select.SZ&lcQtyCnt. TO m.Size&lcQtyCnt.
     ENDFOR 
     SELECT StyDye_Select
     =gfSeek(Style_Select.Style)
     SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style_Select.Style FOR EMPTY(DYELOT) AND IIF(!EMPTY(lcRpWrh),CWARECODE =lcRpWrh,.T.)
       FOR lnQtyCnt = 1 TO Scale_Select.Cnt
         lcQtyCnt  = STR(lnQtyCnt ,1)
         m.Qty&lcQtyCnt. = m.Qty&lcQtyCnt. + IIF(lcQtyType = 'S',StyDye_Select.STK&lcQtyCnt.,IIF(lcQtyType = 'I',(StyDye_Select.STK&lcQtyCnt. - StyDye_Select.ORD&lcQtyCnt.),(StyDye_Select.STK&lcQtyCnt.+StyDye_Select.WIP&lcQtyCnt. - StyDye_Select.ORD&lcQtyCnt.)))
         m.TOTQTY = m.TOTQTY +  StyDye_Select.Totstk
       ENDFOR        
     ENDSCAN 
     INSERT INTO (lcWrkTmp) FROM MEMVAR 
   ENDSCAN   
  ENDSCAN
ELSE
 **
 SELECT Style_Select
 =gfSeek('')
 SCAN FOR !EMPTY(STYLE) AND !EMPTY(cStyMajor) AND IIF(!EMPTY(lcStatus),Style_Select.STATUS $ lcStatus,.T.) AND ;
                    IIF(llSelGrp ,SEEK(Style_Select.CSTYGROUP,lcGrpCursor),.T.) AND IIF(llSelDiv,SEEK(Style_Select.CDIVISION,lcDivCursor),.T.)  AND ;
                    IIF(llSeasonSel,SEEK(Style_Select.SEASON,lcSeasonCursor),.T.) AND IIF(llSelColor ,SEEK(SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen),lcStyColorCursor),.T.) AND ;
    	            IIF(!EMPTY(ALLTRIM(lcPatt)),Style_Select.pattern=lcPatt,.T.)
    IF TYPE('lcXMLFileName')<> 'C'                    
      WAIT WINDOW 'Collecting Data for Style: '+Style_Select.STyle NOWAIT 
    ELSE  
      IF MOD(RECNO('Style_Select'), CEILING(RECCOUNT('Style_Select')/10)) = 0
        loProgress.Percent = (RECNO('Style_Select') /RECCOUNT('Style_Select'))*0.9
        loProgress.DESCRIPTION = 'Collecting Data for Style: '+Style_Select.STyle 
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      ENDIF  
    ENDIF  
    =gfSeek('S'+Style_Select.SCALE,'Scale_Select','Scale')               
    m.Style = Style_Select.cStyMajor
    m.Desc = Style_Select.DESC
    m.DESC1 = Style_Select.DESC1
    m.Scale = Scale_Select.Scale
    m.Color = SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen)

	m.ClrDesc = gfCodDes(SUBSTR(Style_Select.STYLE,lnNonMajSt,lnColorLen),'COLOR')


    m.PriceA= Style_Select.PRICEA 
    m.PriceB= Style_Select.PRICEB
    m.PriceC= Style_Select.PRICEC 
    m.nsugretpri = Style_Select.nsugretpri
    m.Season = Style_Select.Season
    m.Division  = Style_Select.cDivision
    FOR lnQtyCnt = 1 TO 8
      lcQtyCnt  = STR(lnQtyCnt ,1)
      STORE 0 TO m.Qty&lcQtyCnt.,m.totQty
      STORE '' TO m.Size&lcQtyCnt.
    ENDFOR 
    FOR lnQtyCnt = 1 TO 8
      lcQtyCnt  = STR(lnQtyCnt ,1)
      STORE Scale_Select.SZ&lcQtyCnt. TO m.Size&lcQtyCnt.
    ENDFOR 
    SELECT StyDye_Select
    =gfSeek(Style_Select.Style)
    SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style_Select.Style FOR EMPTY(DYELOT) AND IIF(!EMPTY(lcRpWrh),CWARECODE =lcRpWrh,.T.)
      FOR lnQtyCnt = 1 TO Scale_Select.Cnt
        lcQtyCnt  = STR(lnQtyCnt ,1)
        m.Qty&lcQtyCnt. = m.Qty&lcQtyCnt. + IIF(lcQtyType = 'S',StyDye_Select.STK&lcQtyCnt.,IIF(lcQtyType = 'I',(StyDye_Select.STK&lcQtyCnt. - StyDye_Select.ORD&lcQtyCnt.),(StyDye_Select.STK&lcQtyCnt.+StyDye_Select.WIP&lcQtyCnt. - StyDye_Select.ORD&lcQtyCnt.)))
        m.TOTQTY = m.TOTQTY +  StyDye_Select.Totstk
      ENDFOR        
    ENDSCAN 
    INSERT INTO (lcWrkTmp) FROM MEMVAR 
  ENDSCAN   
ENDIF
SELECT(lcWrkTmp)
LOCATE 

IF EOF()
  IF TYPE('lcXMLFileName')<> 'C'
    =gfModalGen('TRM00000B00000','ALERT','','','No Records to Export')
  ENDIF  
  RETURN .F.
ENDIF
lcRpOFile = oAriaApplication.Workdir+gfTempName()+".csv"
llBytesWrote = 0
lfCreateCsvFile()
IF TYPE('lcXMLFileName')<> 'C'
  IF llBytesWrote > 0
*    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcRpOFile)+" has been exported successfully")
  ELSE
    =gfModalGen('TRM00000B00000','ALERT','','','Could not create output file. Cannot proceed.')
    RETURN .F.
  ENDIF
ELSE
  IF  llBytesWrote = 0
    ERROR "Could not create output file "+ALLTRIM(lcRpOFile)+". Cannot proceed."
    RETURN .F.
  ENDIF 
ENDIF  

IF lfUploadFile()
   =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File has been uploaded successfully")
ELSE
   =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File could not be uploaded")
   RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2015
*! Purpose   : Create Temp. Cursor
*!*************************************************************
FUNCTION lfCreateTemp
DIMENSION laFileTmpStru[29,4]
laFileTmpStru[1,1] = 'Style'
laFileTmpStru[1,2] = 'C'
laFileTmpStru[1,3] = 19
laFileTmpStru[1,4] = 0

laFileTmpStru[2,1] = 'Desc'
laFileTmpStru[2,2] = 'C'
laFileTmpStru[2,3] = 20
laFileTmpStru[2,4] = 0

laFileTmpStru[3,1] = 'Desc1'
laFileTmpStru[3,2] = 'C'
laFileTmpStru[3,3] = 60
laFileTmpStru[3,4] = 0

laFileTmpStru[4,1] = 'Color'
laFileTmpStru[4,2] = 'C'
laFileTmpStru[4,3] = 6
laFileTmpStru[4,4] = 0

laFileTmpStru[5,1] = 'Scale'
laFileTmpStru[5,2] = 'C'
laFileTmpStru[5,3] = 3
laFileTmpStru[5,4] = 0

laFileTmpStru[6,1] = 'Size1'
laFileTmpStru[6,2] = 'C'
laFileTmpStru[6,3] = 5
laFileTmpStru[6,4] = 0

laFileTmpStru[7,1] = 'Size2'
laFileTmpStru[7,2] = 'C'
laFileTmpStru[7,3] = 5
laFileTmpStru[7,4] = 0

laFileTmpStru[8,1] = 'Size3'
laFileTmpStru[8,2] = 'C'
laFileTmpStru[8,3] = 5
laFileTmpStru[8,4] = 0

laFileTmpStru[9,1] = 'Size4'
laFileTmpStru[9,2] = 'C'
laFileTmpStru[9,3] = 5
laFileTmpStru[9,4] = 0

laFileTmpStru[10,1] = 'Size5'
laFileTmpStru[10,2] = 'C'
laFileTmpStru[10,3] = 5
laFileTmpStru[10,4] = 0

laFileTmpStru[11,1] = 'Size6'
laFileTmpStru[11,2] = 'C'
laFileTmpStru[11,3] = 5
laFileTmpStru[11,4] = 0

laFileTmpStru[12,1] = 'Size7'
laFileTmpStru[12,2] = 'C'
laFileTmpStru[12,3] = 5
laFileTmpStru[12,4] = 0

laFileTmpStru[13,1] = 'Size8'
laFileTmpStru[13,2] = 'C'
laFileTmpStru[13,3] = 5
laFileTmpStru[13,4] = 0

laFileTmpStru[14,1] = 'PriceA'
laFileTmpStru[14,2] = 'N'
laFileTmpStru[14,3] = 13
laFileTmpStru[14,4] = 3

laFileTmpStru[15,1] = 'PriceB'
laFileTmpStru[15,2] = 'N'
laFileTmpStru[15,3] = 13
laFileTmpStru[15,4] = 3

laFileTmpStru[16,1] = 'PriceC'
laFileTmpStru[16,2] = 'N'
laFileTmpStru[16,3] = 13
laFileTmpStru[16,4] = 3

laFileTmpStru[17,1] = 'nsugretpri'
laFileTmpStru[17,2] = 'N'
laFileTmpStru[17,3] = 13
laFileTmpStru[17,4] = 3

laFileTmpStru[18,1] = 'Season'
laFileTmpStru[18,2] = 'C'
laFileTmpStru[18,3] = 30
laFileTmpStru[18,4] = 0

laFileTmpStru[19,1] = 'Division'
laFileTmpStru[19,2] = 'C'
laFileTmpStru[19,3] = 30
laFileTmpStru[19,4] = 0

laFileTmpStru[20,1] = 'QTY1'
laFileTmpStru[20,2] = 'N'
laFileTmpStru[20,3] = 13
laFileTmpStru[20,4] = 0

laFileTmpStru[21,1] = 'QTY2'
laFileTmpStru[21,2] = 'N'
laFileTmpStru[21,3] = 13
laFileTmpStru[21,4] = 0

laFileTmpStru[22,1] = 'QTY3'
laFileTmpStru[22,2] = 'N'
laFileTmpStru[22,3] = 13
laFileTmpStru[22,4] = 0

laFileTmpStru[23,1] = 'QTY4'
laFileTmpStru[23,2] = 'N'
laFileTmpStru[23,3] = 13
laFileTmpStru[23,4] = 0

laFileTmpStru[24,1] = 'QTY5'
laFileTmpStru[24,2] = 'N'
laFileTmpStru[24,3] = 13
laFileTmpStru[24,4] = 0

laFileTmpStru[25,1] = 'QTY6'
laFileTmpStru[25,2] = 'N'
laFileTmpStru[25,3] = 13
laFileTmpStru[25,4] = 0

laFileTmpStru[26,1] = 'QTY7'
laFileTmpStru[26,2] = 'N'
laFileTmpStru[26,3] = 13
laFileTmpStru[26,4] = 0

laFileTmpStru[27,1] = 'QTY8'
laFileTmpStru[27,2] = 'N'
laFileTmpStru[27,3] = 13
laFileTmpStru[27,4] = 0

laFileTmpStru[28,1] = 'ClrDesc'
laFileTmpStru[28,2] = 'C'
laFileTmpStru[28,3] = 30
laFileTmpStru[28,4] = 0

laFileTmpStru[29,1] = 'TOTQTY'
laFileTmpStru[29,2] = 'N'
laFileTmpStru[29,3] = 13
laFileTmpStru[29,4] = 0

=gfCrtTmp(lcWrkTmp,@laFileTmpStru,'STYLE+COLOR',lcWrkTmp)

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/16/2015
*! Purpose   : Return the selected Payment Method in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC
*!*************************************************************
*! Name      : lfvWrH
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2015
*! Purpose   : Validate WareHouse
*!*************************************************************
FUNCTION lfvWrH
IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS','SH')
ENDIF
*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcRpWrh .OR. (!EMPTY(lcRpWrh) .AND. !gfSEEK(lcRpWrh, 'WAREHOUS'))
  lcRpWrh = gfBrowWare(.T.)
ENDIF    && End of IF
*!*************************************************************
*! Name      : lfCreateCsvFile
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/16/2015
*! Purpose   : Create CSV File
*!*************************************************************
FUNCTION lfCreateCsvFile

lcHeader = '"sty","clr","color","sku","onhand"'+CHR(13)+CHR(10)
llBytesWrote = STRTOFILE(lcHeader ,lcRpOFile,0)
SELECT (lcWrkTmp)
SCAN
  lcDetLine = '"'+SUBSTR(Style,1,lcMjrlen)+'","'+Color+'","'+ClrDesc+'","'+ALLTRIM(SUBSTR(Style,1,lcMjrlen))+" "+Color+'","'+STR(totQty,13,0)+'"'+CHR(13)+CHR(10)
  llBytesWrote = STRTOFILE(lcDetLine ,lcRpOFile,1)              
ENDSCAN 

*Ftp functions
FUNCTION lfUploadFile

#DEFINE GENERIC_READ 2147483648 && &H80000000
#DEFINE GENERIC_WRITE 1073741824 && &H40000000

Local m.ftpServer, m.ftpUserName, m.ftpUserPass

PUBLIC hOpen, hFtpSession
DECLARE INTEGER InternetOpen IN wininet.dll;
STRING sAgent,;
INTEGER lAccessType,;
STRING sProxyName,;
STRING sProxyBypass,;
STRING lFlags

DECLARE INTEGER InternetCloseHandle IN wininet.dll;
INTEGER hInet

DECLARE INTEGER InternetConnect IN wininet.dll;
INTEGER hInternetSession,;
STRING sServerName,;
INTEGER nServerPort,;
STRING sUsername,;
STRING sPassword,;
INTEGER lService,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER FtpOpenFile IN wininet.dll;
INTEGER hFtpSession,;
STRING sFileName,;
INTEGER lAccess,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER InternetWriteFile IN wininet.dll;
INTEGER hFile,;
STRING @ sBuffer,;
INTEGER lNumBytesToWrite,;
INTEGER @ dwNumberOfBytesWritten
#DEFINE FTP_TRANSFER_TYPE_BINARY       2

*!*	m.ftpServer="klingon"
*!*	m.ftpServer="172.10.1.3"
*!*	m.ftpUserName="e2userdp"
*!*	m.ftpUserPass="e2user"
m.ftpUserName = "erp@ericjavits.com" && ftp user Name
m.ftpUserPass = "yqQW4tAuHBY1D2WUCP5R" &&Ftp Password
m.ftpServer   = "199.47.230.95"
lcTargetPath = "/"
lcFirstFileToFind = ""
IF connect2ftp (m.ftpServer, m.ftpUserName, m.ftpUserPass)
  lcFirstFile = DTOS(DATE())
  llFoundFile = .F.
  lnCnt = 0
  DO WHILE !llFoundFile 
    lcCnt = STR(lnCnt,2)
    lcCnt = PADL(ALLTRIM(lcCnt),2,"0")
    lcFirstFileToFind = lcFirstFile+"-"+lcCnt 
    hFile = FtpOpenFile(hFtpSession, lcTargetPath +lcFirstFileToFind+".csv",;
                GENERIC_READ, FTP_TRANSFER_TYPE_BINARY, 0)

    IF hFile =< 0
      llFoundFile =.T.
      EXIT 
    ELSE
      llFoundFile = .F.
    ENDIF
    lnCnt = lnCnt + 1 
  ENDDO
  = InternetCloseHandle (hFile)
ELSE
  RETURN .F.  
ENDIF  

IF connect2ftp (m.ftpServer, m.ftpUserName, m.ftpUserPass)
  lcSourcePath = oAriaApplication.WorkDir && local folder   && remote folder (ftp server)
  lnFiles = ADIR (arr, lcRpOFile)

  FOR lnCnt=1 TO lnFiles
    lcSource = lcSourcePath + LOWER (arr [lnCnt, 1])
    lcTarget = lcTargetPath +lcFirstFileToFind+".csv"
    *lcTargetPath + LOWER (arr [lnCnt, 1])
    * lcSource + " -> " + lcTarget
    local2ftp (hFtpSession, lcSource, lcTarget)
  ENDFOR
  = InternetCloseHandle (hFtpSession)
  = InternetCloseHandle (hOpen)
ELSE
  RETURN .F.  
ENDIF
RETURN .T.

FUNCTION connect2ftp (strHost, strUser, strPwd)
** Open the access
sProxyName = CHR(0) &&... no proxy
sProxyBypass = CHR(0) &&... nothing to bypass
lFlags = 0 &&... no flags used
sAgent = 'vfp'
hOpen = InternetOpen (sAgent, 1,sProxyName, sProxyBypass, lFlags)

IF hOpen = 0
  RETURN .F.
ENDIF

** Connect to FTP.
hFtpSession = InternetConnect (hOpen, strHost, 0, strUser, strPwd, 1, 0,0)


IF hFtpSession = 0
** Close
= InternetCloseHandle (hOpen)
*? "FTP " + strHost + " not ready"
RETURN .F.
ELSE
*? "Connected to " + strHost + " as: [" + strUser + ", *****]"
ENDIF
RETURN .T.
 
FUNCTION local2ftp (hConnect, lcSource, lcTarget)
** Upload local file to ftp server
hSource = FOPEN (lcSource)
IF (hSource = -1)
RETURN -1
ENDIF

** New file in ftp server
hTarget = FtpOpenFile(hConnect, lcTarget, GENERIC_WRITE, 2, 0)
IF hTarget = 0
= FCLOSE (hSource)
RETURN -2
ENDIF

lnBytesWritten = 0
lnChunkSize = 512 && 128, 512
DO WHILE Not FEOF(hSource)
lcBuffer = FREAD (hSource, lnChunkSize)
lnLength = Len(lcBuffer)
IF lnLength > 0
IF InternetWriteFile (hTarget, @lcBuffer, lnLength, @lnLength) =1
lnBytesWritten = lnBytesWritten + lnLength
*? lnBytesWritten
** Show Progress
ELSE
EXIT
ENDIF
ELSE
EXIT
ENDIF
ENDDO

= InternetCloseHandle (hTarget)
= FCLOSE (hSource)

RETURN lnBytesWritten 