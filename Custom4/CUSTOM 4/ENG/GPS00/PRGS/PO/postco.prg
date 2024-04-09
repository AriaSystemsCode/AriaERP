*!**************************************************************************
*! Name      : postco.PRG
*! Developer : ALAA AbdelWahed [HIA]
*! Date      : 06/16/2008
*! Purpose   : CUSTOM PROGRAM FOR GPS .
*! Entry     : A27 : C201015,A40 :C201016[T20070118.0004]
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications
*! C201016,2 MMT 06/24/2008 Fix bug of Not printing Shipemnt Cost sheet[T20070118.0004]
*!**************************************************************************
*-- Declarations of Variables
LCTempBom    = gfTempName()
lctemppo     = gfTempName()
lcTempBomDet = gfTempName()
lcTempCst    = ''
lcTempStyle  = ''
DO FORM (oAriaApplication.ScreenHome+"PO\POSTCO.SCX") WITH LCTempBom ,lctemppo ,lcTempBomDet

*-- functions body
*!**************************************************************************
*! Name      : lfCallOp
*! Developer : aLAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfCallOp FUNCTION.
*!**************************************************************************
*! Parameters:loFormSet
*!**************************************************************************
*! Returns   :t&f
*!**************************************************************************
FUNCTION lfCallOp
PARAMETERS loFormSet

lcDataSessI = loFormSet.DatasessionID  &&SET("Datasession" )&&THIS.loFormSet.DatasessionID
lcExpr = gfOpGrid('POSTCOST',.T.)&&,.F.,.F.,.T.,.T.) && CREATE OPTION GRID
SET DATASESSION TO lcDataSessI


IF lcExpr <> ".F."
  RETURN .T.
ELSE
  IF USED(loFormSet.LCTempBom)
    SELECT (loFormSet.LCTempBom)
    ZAP
  ENDIF 

  IF USED(loFormSet.lctemppo)
    SELECT (loFormSet.lctemppo)
    ZAP
  ENDIF 
  
  IF USED(loFormSet.lcTempBomDet)
    SELECT (loFormSet.lcTempBomDet)
    ZAP
  ENDIF 
  
  RETURN .F.  
ENDIF 
*!**************************************************************************
*! Name      : lfRepWhen
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfRepWhen FUNCTION
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfRepWhen
DIMENSION laTableStuct[3,4]

laTableStuct[1,1] = 'CCSTSHT_ID'
laTableStuct[1,2] = 'C'
laTableStuct[1,3] = 6 
laTableStuct[1,4] = 0
laTableStuct[2,1] = 'CITMMAJOR'
laTableStuct[2,2] = 'C'
laTableStuct[2,3] = 19
laTableStuct[2,4] = 0
laTableStuct[3,1] = 'ccstshtdsc'
laTableStuct[3,2] = 'C'
laTableStuct[3,3] = 30
laTableStuct[3,4] = 0

=gfCrtTmp(lcTempBomhd,@laTableStuct,'CITMMAJOR',lcTempBomhd,.T.)

IF !USED("BOMHEADR")
  =gfOpenTable("BOMHEADR","BOMHEADR")
ENDIF 

=gfSeek('0001',"BOMHEADR")
SELECT "BOMHEADR"
SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id = '0001' FOR CCSTSHTTYP = 'I'
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcTempBomhd) FROM MEMVAR 
ENDSCAN 
*!**************************************************************************
*! Name      : lfSRVSTYLE
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfSRVSTYLE FUNCTION
*!**************************************************************************
*! Parameters:lcParam
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfSRVSTYLE
PARAMETERS lcParam

IF !USED("BOMHEADR")
  =gfOpenTable("BOMHEADR","BOMHEADR")
ENDIF 

DO CASE 
  CASE lcParam  = 'V'
  
  CASE lcParam  = 'S'
    *SEASON
    llUseSeason  = .F.
    lnSeaPos = ASCAN(loOgScroll.laOgfxFlt,"STYLE.SEASON")
  	IF lnSeaPos > 0 
      lnSeaPos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnSeaPos,1)
      lcSeaSel =IIF(!EMPTY(loOgScroll.laOgfxFlt[lnSeaPos,6]),loOgScroll.laOgfxFlt[lnSeaPos,6],'')
      IF !EMPTY(lcSeaSel) 
        lcSeaFile = loOGScroll.gfTempName()
        llUseSeason  = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
     ENDIF 
   ENDIF 
   
  *DIVISION
   llUseDiv  = .F.
   lnDivPos = ASCAN(loOgScroll.laOgfxFlt,"STYLE.CDIVISION")
   IF lnDivPos > 0 
     lnDivPos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnDivPos,1)
     lcDivSel =IIF(!EMPTY(loOgScroll.laOgfxFlt[lnDivPos,6]),loOgScroll.laOgfxFlt[lnDivPos,6],'')
     IF !EMPTY(lcDivSel) 
       lcDivFile = loOGScroll.gfTempName()
       llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
     ENDIF 
   ENDIF 
    
    SELECT style 
    SET ORDER TO CSTYLE
    SET FILTER TO Seek(Style.cstymajor,lcTempBomhd) AND IIF(lluseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND ;
      IIF(llUseDiv ,SEEK(Style.CDIVISION,lcDivFile ),.T.) 


  CASE lcParam  = 'R'
    SELECT style 
    SET FILTER TO 
 ENDCASE  
*!**************************************************************************
*! Name      : lfSRVCSTSH
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfSRVCSTSH FUNCTION.
*!**************************************************************************
*! Parameters:lcParam
*!**************************************************************************
*! Returns   :
*!**************************************************************************
 FUNCTION lfSRVCSTSH
 PARAMETERS lcParam  

 lcStyCursor = ''
 lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
 IF lnPosSty > 0 
   lnPosSty  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
   lcStySel  = loOGScroll.laOgFxFlt[lnPosSty,6]
   IF !EMPTY(lcStySel)
    lcStyCursor= lcStySel
   ENDIF 
 ENDIF 


 DO CASE 
    CASE lcParam  = 'S'
      SELECT (lcTempBomhd)
      IF USED(lcStyCursor) AND RECCOUNT(lcStyCursor) > 0
        SET FILTER TO Seek(CITMMAJOR,lcStyCursor)
      ENDIF
    
    CASE lcParam  = 'R'
      SELECT (lcTempBomhd)
      SET FILTER TO 
 ENDCASE 
 *!**************************************************************************
*! Name      :lfConvertToCursor
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfConvertToCursor FUNCTION.
*!**************************************************************************
*! Parameters:lcStrToConv,lcFieldName ,lcNewFile
*!**************************************************************************
*! Returns   :
*!**************************************************************************
 FUNCTION lfConvertToCursor
 PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
 lcCursorTemp = lcNewFile &&Cursor Hold Selected values
 DIMENSION laTempacstru[1,4]
 laTempacstru[1,1] = lcFieldName 
 
 DO CASE 
  
 CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

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
*!**************************************************************************
*! Name      : lfCreatExp
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/03/2008
*! Purpose   :lfCreatExp FUNCTION
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfCreatExp

lnStyPos = ASCAN(loOgScroll.laOgfxFlt,"STYLE.CSTYMAJOR")
IF lnStyPos   > 0 
  lnStyPos   = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnStyPos  ,1)
  lcTempStyle = IIF(!EMPTY(loOgScroll.laOgfxFlt[lnStyPos ,6]),loOgScroll.laOgfxFlt[lnStyPos  ,6],'')
ENDIF 
*Check if user select style or not
IF (!EMPTY(lcTempStyle) AND USED(lcTempStyle) AND RECCOUNT(lcTempStyle) = 0) OR ;
   (!EMPTY(lcTempStyle) AND !USED(lcTempStyle)) OR ;
   EMPTY(lcTempStyle) 
  =gfModalGen('INM00000B00000',.f.,.f.,.f.,'You must Select at least one style')
  RETURN .F.
ENDIF  

lnCstPos = ASCAN(loOgScroll.laOgfxFlt,"BOMHEADR.CCSTSHT_ID")
IF lnCstPos > 0 
  lnCstPos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnCstPos,1)
  lcTempCst = IIF(!EMPTY(loOgScroll.laOgfxFlt[lnCstPos,6]),loOgScroll.laOgfxFlt[lnCstPos,6],'')
ENDIF 

SELECT (lcTempStyle)
llNoCstSht = .F.

IF (!EMPTY(lcTempCst) AND USED(lcTempCst) AND RECCOUNT(lcTempCst) = 0) OR ;
   (!EMPTY(lcTempCst) AND !USED(lcTempCst)) OR ;
   EMPTY(lcTempCst) 
   
  SELECT (lcTempStyle)
  SCAN 
    lcStyMajor  = &lcTempStyle..CSTYMAJOR
    lcCostSheetID =''
    lnCstCnt = 0
    IF SEEK(&lcTempStyle..CSTYMAJOR,lcTempBomhd)
      SELECT(lcTempBomhd)
      SCAN REST WHILE CITMMAJOR = &lcTempStyle..CSTYMAJOR
        lnCstCnt = lnCstCnt + 1
      ENDSCAN 
    ENDIF
    llNoCstSht = (lnCstCnt > 1)
    IF llNoCstSht 
      EXIT
    ELSE
      IF EMPTY(lcTempCst)  
        lcCostSheetID = EVALUATE(lcTempBomhd+".CCSTSHT_ID")
        lcTempCst = gfTempName()
        lnOpDataSess = SET("Datasession")
        SET DATASESSION TO (lcDataSessI)
        CREATE CURSOR (lcTempCst) (KEYEXP C(26))
        SELECT (lcTempCst)
        INDEX on KEYEXP TAG (lcTempCst)
        APPEND BLANK 
        REPLACE KEYEXP WITH lcStyMajor  + '_' +lcCostSheetID 
        SET DATASESSION TO (lnOpDataSess)
      ELSE
        lcCostSheetID = EVALUATE(lcTempBomhd+".CCSTSHT_ID")
        lnOpDataSess = SET("Datasession")
        SET DATASESSION TO (lcDataSessI)
        IF USED(lcTempCst)  
          SELECT (lcTempCst)
          APPEND BLANK 
          REPLACE KEYEXP WITH lcStyMajor  + '_' +lcCostSheetID 
        ELSE
          CREATE CURSOR (lcTempCst) (KEYEXP C(26))
          SELECT (lcTempCst)
          INDEX on KEYEXP TAG (lcTempCst)
          APPEND BLANK 
          REPLACE KEYEXP WITH lcStyMajor  + '_' +lcCostSheetID 
        ENDIF 
        SET DATASESSION TO (lnOpDataSess) 
      ENDIF 
    ENDIF   
  ENDSCAN
  IF llNoCstSht 
    =gfModalGen('INM00000B00000',.f.,.f.,.f.,'One or more style has more than one cost sheet and none of them is selected')
    RETURN .F.
  ENDIF
ENDIF 
*!**************************************************************************
*! Name      : lfcollct
*! Developer : ALAA AbdelWahed[ALA]
*! Date      : 05/03/2008
*! Purpose   : lfcollct FUNCTION.
*!**************************************************************************
*! Parameters:loFormSet
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfcollct
PARAMETERS loFormSet

DIMENSION POTableStuct[11,4]

  
  POTableStuct[1,1] = 'CITMMAJOR'&&tsyle
  POTableStuct[1,2] = 'C'
  POTableStuct[1,3] = 19
  POTableStuct[1,4] = 0
  POTableStuct[2,1] = 'CCSTSHT_ID'
  POTableStuct[2,2] = 'C'
  POTableStuct[2,3] = 6 
  POTableStuct[2,4] = 0
  POTableStuct[3,1] = 'Description'
  POTableStuct[3,2] = 'C'
  POTableStuct[3,3] = 20
  POTableStuct[3,4] = 0
  POTableStuct[4,1] = 'PRICECUUR'
  POTableStuct[4,2] = 'C'
  POTableStuct[4,3] = 3
  POTableStuct[4,4] = 0
  POTableStuct[5,1] = 'DUTYCURR'
  POTableStuct[5,2] = 'C'
  POTableStuct[5,3] = 3 
  POTableStuct[5,4] = 0
  POTableStuct[6,1] = 'Freight'
  POTableStuct[6,2] = 'N'
  POTableStuct[6,3] = 7
  POTableStuct[6,4] = 3
  POTableStuct[7,1] = 'Commission'
  POTableStuct[7,2] = 'N'
  POTableStuct[7,3] = 7
  POTableStuct[7,4] = 2
  POTableStuct[8,1] = 'Duty_perc'
  POTableStuct[8,2] = 'N'
  POTableStuct[8,3] = 5
  POTableStuct[8,4] = 2
  POTableStuct[9,1] = 'Price'
  POTableStuct[9,2] = 'N'
  POTableStuct[9,3] = 7
  POTableStuct[9,4] = 3
  POTableStuct[10,1] = 'FOB'
  POTableStuct[10,2] = 'N' 
  POTableStuct[10,3] = 7
  POTableStuct[10,4] = 3
  POTableStuct[11,1] = 'lDPerCent'
  POTableStuct[11,2] = 'L' 
  POTableStuct[11,3] = 1
  POTableStuct[11,4] = 0
  
=gfCrtTmp(loFormSet.LCTempBom,@POTableStuct,'CITMMAJOR',loFormSet.LCTempBom,.T.)

IF !USED("BOM")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'BOM'  , 'multibom' , 'SH')
ENDIF 


DIMENSION laBomStruct[1,18]
SELECT BOM
lnArrLen = AFIELDS(laBomStruct) 

DIMENSION laBomStruct[lnArrLen +3,18]
laBomStruct[lnArrLen +1,1] =  'nCstVal'
laBomStruct[lnArrLen +1,2] =  'N'
laBomStruct[lnArrLen +1,3] =  11
laBomStruct[lnArrLen +1,4] =  3

laBomStruct[lnArrLen +2,1] =  'CSTATUS'
laBomStruct[lnArrLen +2,2] =  'C'
laBomStruct[lnArrLen +2,3] =  1
laBomStruct[lnArrLen +2,4] =  0

laBomStruct[lnArrLen +3,1] =  'nOldVal'
laBomStruct[lnArrLen +3,2] =  'N'
laBomStruct[lnArrLen +3,3] =  11
laBomStruct[lnArrLen +3,4] =  3


STORE '' TO laBomStruct[lnArrLen +1,7],laBomStruct[lnArrLen +1,8],laBomStruct[lnArrLen +1,9],;
          laBomStruct[lnArrLen +1,10],laBomStruct[lnArrLen +1,11],laBomStruct[lnArrLen +1,12],;
          laBomStruct[lnArrLen +1,13],laBomStruct[lnArrLen +1,14],laBomStruct[lnArrLen +1,15],;
          laBomStruct[lnArrLen +1,16]
STORE 0 TO laBomStruct[lnArrLen +1,17],laBomStruct[lnArrLen +1,18]

STORE '' TO laBomStruct[lnArrLen +2,7],laBomStruct[lnArrLen +2,8],laBomStruct[lnArrLen +2,9],;
          laBomStruct[lnArrLen +2,10],laBomStruct[lnArrLen +2,11],laBomStruct[lnArrLen +2,12],;
          laBomStruct[lnArrLen +2,13],laBomStruct[lnArrLen +2,14],laBomStruct[lnArrLen +2,15],;
          laBomStruct[lnArrLen +2,16]
STORE 0 TO laBomStruct[lnArrLen +2,17],laBomStruct[lnArrLen +2,18]


STORE '' TO laBomStruct[lnArrLen +3,7],laBomStruct[lnArrLen +3,8],laBomStruct[lnArrLen +3,9],;
          laBomStruct[lnArrLen +3,10],laBomStruct[lnArrLen +3,11],laBomStruct[lnArrLen +3,12],;
          laBomStruct[lnArrLen +3,13],laBomStruct[lnArrLen +3,14],laBomStruct[lnArrLen +3,15],;
          laBomStruct[lnArrLen +3,16]
STORE 0 TO laBomStruct[lnArrLen +3,17],laBomStruct[lnArrLen +3,18]



=gfCrtTmp(loFormSet.lcTempBomDet,@laBomStruct,'citmmajor+ccstsht_id+ typ+STR(nlineno,6)',loFormSet.lcTempBomDet,.T.)

IF !USED("STYLE")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'STYLE'  , 'STYLE' , 'SH')
ENDIF 

IF !USED("SCALE")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'SCALE'  , 'SCALE' , 'SH')
ENDIF 


DIMENSION BMTableStuct[7,4]


BMTableStuct[1,1] = 'PO'
BMTableStuct[1,2] = 'C'
BMTableStuct[1,3] =6
BMTableStuct[1,4] = 0

BMTableStuct[2,1] = 'CITMMAJOR'
BMTableStuct[2,2] = 'C'
BMTableStuct[2,3] = 19 
BMTableStuct[2,4] = 0

BMTableStuct[3,1] = 'ShpCstSht'
BMTableStuct[3,2] = 'C'
BMTableStuct[3,3] = 20
BMTableStuct[3,4] = 0

BMTableStuct[4,1] = 'CCURRCODE'
BMTableStuct[4,2] = 'C'
BMTableStuct[4,3] = 3
BMTableStuct[4,4] = 0

BMTableStuct[5,1] = 'Price'
BMTableStuct[5,2] = 'N'
BMTableStuct[5,3] = 7
BMTableStuct[5,4] = 2

BMTableStuct[6,1] = 'CCSTSHT_ID'
BMTableStuct[6,2] = 'C'
BMTableStuct[6,3] = 6 
BMTableStuct[6,4] = 0

BMTableStuct[7,1] = 'llUpdate'
BMTableStuct[7,2] = 'L'
BMTableStuct[7,3] = 1
BMTableStuct[7,4] = 0
  
=gfCrtTmp(loFormSet.LCTempPo,@BMTableStuct,'CITMMAJOR+CCSTSHT_ID+po',loFormSet.LCTempPo,.T.)
IF !USED("shprlfld")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'shprlfld'  , 'shprlfld' , 'SH')
ENDIF 
IF !USED("STYLE")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'STYLE'  , 'STYLE' , 'SH')
ENDIF 
IF !USED("posln")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'posln'  , 'poslns' , 'SH')
ENDIF 

IF !USED("poshdr")
  =gfOpenTable(oAriaApplication.DAtaDir+ 'poshdr'  , 'poshdr' , 'SH')
ENDIF 
*--style file
SELECT Style 
gfSetOrder('cStyle')
SELECT (lcTempStyle)
SCAN 
  WAIT WINDOW  "Collecting data for Style :"+&lcTempStyle..CSTYMAJOR NOWAIT 
  IF SEEK(&lcTempStyle..CSTYMAJOR,'Style')
    lfscan(style.cstymajor)
  ENDIF   
ENDSCAN

SELECT(loFormSet.LCTempBom) 
LOCATE 
 *!**************************************************************************
*! Name      :lfscan()
*! Developer : ALAA AbdelWahed [ALA]
*! Date      : 05/20/2008
*! Purpose   : lfscan() FUNCTION.
*!**************************************************************************
*! Parameters:lcCall 
*!**************************************************************************
*! Returns   :
*!********************************************************
FUNCTION lfScan
PARAMETERS lcStyMajor

SELECT(lcTempCst)
IF SEEK(ALLTRIM(lcStyMajor),lcTempCst)
 SCAN REST WHILE KEYEXP = ALLTRIM(lcStyMajor)
 
   SELECT(loFormSet.lcTempBom)
   APPEND BLANK 
   REPLACE citmmajor        WITH style.cstymajor,;   && &LCTempBom..season
           Description      WITH style.desc1,;
           PRICECUUR        with Style.cPriceCur ,;
           DUTYCURR         with Style.cdutycur 
           
           
   SELECT(lcTempCst)
   
   IF gfSeek("0001"+SUBSTR(KEYEXP,1,AT('_',KEYEXP)-1)+"I"+SUBSTR(KEYEXP,AT('_',KEYEXP)+1),'BOM')
     SELECT BOM 
     REPLACE ccstsht_id WITH BOM.ccstsht_id IN (loFormSet.LCTempBom)
     SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id+ typ+citmmask+mfgcode+ cinvtypc+item+ STR(nlineno,6)=;
       "0001"+SUBSTR(&lcTempCst..KEYEXP,1,AT('_',&lcTempCst..KEYEXP)-1)+"I"+SUBSTR(&lcTempCst..KEYEXP,AT('_',&lcTempCst..KEYEXP)+1)
       
       SCATTER MEMO MEMVAR 
       m.nCstVal = IIF(m.npercent > 0 ,m.npercent ,m.untcost)
       m.nOldVal = IIF(m.npercent > 0 ,m.npercent ,m.untcost)
       SELECT (loFormSet.lcTempBomDet)
       
       APPEND BLANK 
       GATHER MEMO MEMVAR 

       SELECT(loFormSet.LCTempBom)
       
     ENDSCAN 
   ENDIF 
 ENDSCAN 
ENDIF  
SELECT(loFormSet.LCTempBom)
LOCATE 

*! C201016,2 MMT 06/24/2008 Fix bug of Not printing Shipemnt Cost sheet[Start]
*=gfSQLRUN("Select * from posln[index = posln] where cstytype = 'P'"+; 
           " AND cbusdocu = 'P' and cinvtype = '0001' and style like '"+;
           ALLTRIM(lcStyMajor)+"%' AND ccstsht_ID <> '' and trancd = '1'",'POsLN')
=gfSQLRUN("Select * from posln[index = posln] where cstytype = 'P'"+; 
           " AND cbusdocu = 'P' and cinvtype = '0001' and style like '"+;
           ALLTRIM(lcStyMajor)+"%' AND ccstsht_ID <> ''",'POsLN')
*! C201016,2 MMT 06/24/2008 Fix bug of Not printing Shipemnt Cost sheet[End]

SELECT POSLN           
LOCATE 
IF !EOF()
  SELECT posln
  SCAN FOR SEEK(lcStyMajor+"_"+POSlN.ccstsht_ID,lcTempCst)
  
    
    IF gfSeek(POSlN.cbusdocu+POSlN.cstytype+POSlN.po,'poshdr') AND poshdr.Status  <> 'O'
      LOOP 
    ENDIF 
    
   
    m.CITMMAJOR = lcStyMajor
    m.po =posln.po
    m.ccurrcode = poshdr.cpricecur
    m.CCSTSHT_ID = posln.ccstsht_ID
    IF !EMPTY(POSLN.Shipno) AND gfSeek(POSLN.Shipno+POSLN.PO+STR(POsln.lineno,6),'shprlfld')
      m.SHPCSTSHT = POSLN.Shipno
    ELSE
      m.SHPCSTSHT = ''
    ENDIF 

    IF m.ccurrcode <> Style.cPriceCur
       m.SHPCSTSHT = 'Currency'
    ELSE
      IF EMPTY(m.SHPCSTSHT)
        m.SHPCSTSHT = ''  
      ENDIF   
    ENDIF 

    
    SELECT (loFormSet.LCTempPo)
    IF !SEEK(m.CITMMAJOR +m.CCSTSHT_ID+m.po,loFormSet.LCTempPo)
      APPEND BLANK 
    ENDIF 
    GATHER MEMO MEMVAR 
  ENDSCAN 
 ENDIF 
 *!**************************************************************************
*! Name      :lfConvertToCursor
*! Developer : ALAA [ALA]
*! Date      : 05/03/2008
*! Purpose   : lfConvertToCursor FUNCTION.
*!**************************************************************************
*! Parameters:lcStrToConv,lcFieldName ,lcNewFile
*!**************************************************************************
*! Returns   :
*!********************************************************
FUNCTION lfGetType
PARAMETERS loFormSet

IF EMPTY(loFormSet.laCstLbl[1])
  DECLARE laMainSetp[7,2]
  FOR lnCount = 1 TO 7
    lcCount = STR(lnCount,1)
    laMainSetp[lnCount,1] = 'M_CISLBL'+lcCount
  ENDFOR
  =gfGetMemVar(@laMainSetp, oAriaApplication.ActiveCompanyID)
  FOR lnCount = 1 TO 7
    loFormSet.laCstLbl[lnCount] = laMainSetp[lnCount,2]
  ENDFOR
ENDIF 
lcRetVal = ''
TRY 
  lnTYpe = VAL(EVALUATE(loFormSet.lcTempBomDet+'.typ'))
  lcRetVal = loFormSet.laCstLbl[lnTYpe]
CATCH
  lcRetVal = ""
ENDTRY 
RETURN lcRetVal 
*!**************************************************************************
*! Name      : lfIsPerCent
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/16/2008
*! Purpose   : check if Cost element is Percentage
*!**************************************************************************
FUNCTION lfIsPerCent
PARAMETERS loFormSet

IF EVALUATE(loFormSet.lcTempBomDet+'.npercent') > 0
  RETURN "%"
ELSE
  RETURN ''
ENDIF 
*!**************************************************************************
*! Name      : lfScrSave
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/16/2008
*! Purpose   : Saving Function
*!**********************************************
FUNCTION lfScrSave
PARAMETERS loFormSet

SELECT style
gfsetOrder('Style')


IF !USED('CTKTBOM')
  =gfOpenTable('CTKTBOM','CTKTBOM')
ENDIF 


IF !USED("BOMLINE")
  =gfOpenTable("BOMLINE","BOMLINE")
ENDIF 

SELECT BOMLINE
AFIELDS(laBomLStr)
=gfCrtTmp(loFormSet.ariaForm1.mainworkorder1.BOmline,@laBomLStr,'cimtyp+ctype+ctktno+STR(lineno,6)+ cbomtyp+ cinvtype+style+cinvtypc+ item+ mfgcode','BOmline',.T.)


SELECT(loFormSet.lcTempBomDet)
LOCATE FOR CSTATUS = 'M'
IF !FOUND()
  RETURN .F.
ELSE
  SELECT(loFormSet.lcTempBomDet)
  SCAN FOR CSTATUS = 'M'
    IF gfSeek(cinvtype + citmmajor + ccstshttyp + ccstsht_id + typ + citmmask + mfgcode + cinvtypc+ item+ALLTRIM(STR(nlineno,6)),'BOM')
       SCATTER MEMO MEMVAR 
       SELECT BOM 
       GATHER MEMO MEMVAR 
       =gfAdd_Info('BOM') 
       gfReplace()
       
       SELECT(loFormSet.LCTempPo)
       =SEEK(EVALUATE(loFormSet.lcTempBomDet+'.citmmajor')+EVALUATE(loFormSet.lcTempBomDet+'.ccstsht_id'))
       SCAN REST WHILE CITMMAJOR+CCSTSHT_ID+po = EVALUATE(loFormSet.lcTempBomDet+'.citmmajor')+EVALUATE(loFormSet.lcTempBomDet+'.ccstsht_id') FOR llUpdate
       
        lcPONO = EVALUATE(loFormSet.LCTempPo+'.PO')
       	loFormSet.ariaForm1.mainworkorder1.mopenwrkordsql('loFormSet.ariaForm1.mainworkorder1.oPosHdrCon','POSHDR',loFormSet.ariaForm1.mainworkorder1.cPosHdr,;
                     "cBusDocu= 'P' AND "+;
                     "cStyType='P' AND  PO = '"+lcPONO +"'",.T.," (INDEX=POSHDR) ")		  
       
        loFormSet.ariaForm1.mainworkorder1.mopenwrkordsql('loFormSet.ariaForm1.mainworkorder1.oBomLineCon' ,'BomLine',loFormSet.ariaForm1.mainworkorder1.BomLine,;
	  				 "cImTyp = 'I'" + " AND cTktNo ='" +EVALUATE(loFormSet.LCTempPo+".PO") + "'")

       
         SELECT POSLN
         
         gfSetOrder('POSLN')
         IF !SEEK('PP'+EVALUATE(loFormSet.LCTempPo+".PO"))
           =gfSeek('PP'+EVALUATE(loFormSet.LCTempPo+".PO"),'POSLN')
         ENDIF   
         
         SCAN REST WHILE cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd='PP'+EVALUATE(loFormSet.LCTempPo+".PO") for;
            cinvtype = '0001' AND style = ALLTRIM(EVALUATE(loFormSet.lcTempBomDet+'.citmmajor')) AND !EMPTY(Ccstsht_id) AND Ccstsht_id = EVALUATE(loFormSet.lcTempBomDet+'.ccstsht_id');
            AND POsln.Trancd = '1'
            
            



	        SCATTER MEMO MEMVAR 
          IF !SEEK('PP'+POSLN.PO+POSLN.cinvtype+POSLN.style+STR(POSLN.lineno,6),loFormSet.ariaForm1.mainworkorder1.cPoLine)
  	        INSERT INTO (loFormSet.ariaForm1.mainworkorder1.cPosLn) FROM MEMVAR 
            INSERT INTO (loFormSet.ariaForm1.mainworkorder1.cPoLine) FROM MEMVAR 
          ENDIF   

          llMultiCur = loFormSet.ariaForm1.mainworkorder1.lMultiCurrency
          lcPriceCurr = EVALUATE(loFormSet.ariaForm1.mainworkorder1.cPosHdr+'.cpricecur')
          lnPricRate= EVALUATE(loFormSet.ariaForm1.mainworkorder1.cPosHdr+'.npricerat')
          lcDutyCurr= EVALUATE(loFormSet.ariaForm1.mainworkorder1.cPosHdr+'.cdutycur')
	    
            LOCAL laOldECst, laNewECst, laOldFCst, laNewFCst, lcTmpPoLn, lnI
            PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2, lcTmpPoLn
            DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
            STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
            STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
            STORE 1  TO lnCurrUnt1, lnCurrUnt2
            lnDutyRate    = EVALUATE(loFormSet.ariaForm1.mainworkorder1.cPosHdr+'.ndutyrat')
            lcTmpPoLn     = loFormSet.ariaForm1.mainworkorder1.cPoLine
            SELECT (lcTmpPoLn)
            SCATTER MEMVAR
            *-- Get the old estimated and foreign costs
            FOR lnI = 1 TO 7
              lcI = STR(lnI,1)
              STORE m.nICost&lcI TO laOldECst[lnI], laNewECst[lnI]
              STORE m.nFCost&lcI TO laOldFCst[lnI], laNewFCst[lnI]
            ENDFOR 
            lcType = EVALUATE(loFormSet.lcTempBomDet+'.TYP')
            IF  EVAL(loFormSet.lcTempBomDet+'.ccatgtyp') ='P'   
              laOldFCst[1] = m.nfcost1
              lcNewCst1    = m.gros_price - EVALUATE(loFormSet.lcTempBomDet+'.nOldVal')+EVALUATE(loFormSet.lcTempBomDet+'.untCost')
              lfvCost('1')
              laNewECst[1] = &lcTmpPoLn..nICost1
              laNewFCst[1] = &lcTmpPoLn..nFCost1
            ELSE
              lcType = EVALUATE(loFormSet.lcTempBomDet+'.TYP')
              lcNewCst1    = m.nfcost&lcType - EVALUATE(loFormSet.lcTempBomDet+'.nOldVal')+EVALUATE(loFormSet.lcTempBomDet+'.untCost')
              lfvCost(lcType)
              laNewECst[VAL(EVALUATE(loFormSet.lcTempBomDet+'.TYP'))] = &lcTmpPoLn..nICost&lcType
              laNewFCst[VAL(EVALUATE(loFormSet.lcTempBomDet+'.TYP'))] = &lcTmpPoLn..nFCost&lcType
            ENDIF 
            loFormSet.ariaForm1.mainworkorder1.mCalEstCst(POSLn.TotQty,POSLn.TotQty,@laOldECst,@laNewECst,;
                                                          @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
                                                          lcDutyCurr)

            SELECT (lcTmpPoLn)
            SCATTER MEMO MEMVAR 
            SELECT POSLN
            GATHER MEMO memva
            gfReplace()
            
            SELECT CTKTBOM
            IF !SEEK("I"+lcPONO+ EVALUATE(loFormSet.lcTempBomDet+'.TYP'),'CTKTBOM')
              =gfSeek("I"+lcPONO+ EVALUATE(loFormSet.lcTempBomDet+'.TYP'),'CTKTBOM')
            ENDIF 
            
            *! C201016,2 MMT 06/24/2008 Fix bug of Not printing Shipemnt Cost sheet[Start]
            *gfReplace("UntCost WITH "+ STR((Est_Cost - (&lcTmpPoLn..TotQty * laOldECst[VAL(lcType)])+ (&lcTmpPoLn..TotQty * &lcTmpPoLn..nICost&lcType))/req_qty,11,3)+"")
            lnUntCost = (Est_Cost - (&lcTmpPoLn..TotQty * laOldECst[VAL(lcType)])+ (&lcTmpPoLn..TotQty * &lcTmpPoLn..nICost&lcType))/req_qty
            IF ROUND(lnUntCost,2) = ROUND(&lcTmpPoLn..nICost&lcType,2)
              lnUntCost = &lcTmpPoLn..nICost&lcType
            ENDIF 
            gfReplace("UntCost WITH "+ STR(lnUntCost,11,3)+"")
            *! C201016,2 MMT 06/24/2008 Fix bug of Not printing Shipemnt Cost sheet[End]
            
            gfReplace("Est_Cost with "+STR(req_qty* UntCost,11,3)+"")
          
            
            IF Seek("I"+ '1'+EVALUATE(loFormSet.LCTempPo+".PO") +STR(POSLN.lineno,6),loFormSet.ariaForm1.mainworkorder1.BomLine,'bomline')
              SELECT (loFormSet.ariaForm1.mainworkorder1.BomLine)
              SCAN REST WHILE cimtyp+ ctype+ ctktno+ STR(lineno,6) +cbomtyp+ cinvtype+ style+cinvtypc+ item+ mfgcode = "I"+ '1'+EVALUATE(loFormSet.LCTempPo+".PO") +STR(POSLN.lineno,6) FOR ;
           		 ccatgtyp = eval(loFormSet.lcTempBomDet+'.ccatgtyp')
           		 
           		 SELECT (loFormSet.ariaForm1.mainworkorder1.BOmline)
           		 IF npercent > 0
             	   REPLACE npercent  WITH npercent - EVALUATE(loFormSet.lcTempBomDet+'.nOldVal')+EVALUATE(loFormSet.lcTempBomDet+'.npercent') 
           		 ELSE
           		   REPLACE unitCost WITH unitCost - EVALUATE(loFormSet.lcTempBomDet+'.nOldVal')+IIF(eval(loFormSet.lcTempBomDet+'.ccatgtyp')='P', &lcTmpPoLn..nFCost1,EVALUATE(loFormSet.lcTempBomDet+'.untCost') )
               ENDIF 		 
               loFormSet.ariaForm1.mainworkorder1.mupdbom ('M')    
              ENDSCAN 
            ENDIF  
        ENDSCAN   
        
        SELECT(loFormSet.ariaForm1.mainworkorder1.BOmline)
    		IF loFormSet.ariaForm1.mainworkorder1.mupdatesql(loFormSet.ariaForm1.mainworkorder1.oBomLineCon,loFormSet.ariaForm1.mainworkorder1.BOmline,;
		        'cIMTyp,cType,cTktNo,ShipNo,LineNo,cBomTyp,cInvType,Style,cInvTypC,Item,MfgCode,cRsession,cStyGrade,nLineNo',;
		        'BOMLINE','BOMLINEU')
  		    =TABLEUPDATE(.T.,.T.)
	    	ELSE
		       =TABLEREVERT(.T.)
    		ENDIF
		
		    IF loFormSet.ariaForm1.mainworkorder1.mupdatesql(loFormSet.ariaForm1.mainworkorder1.oPosHdrCon,loFormSet.ariaForm1.mainworkorder1.cPosHdr,'cBusDocu,cStyType,PO','POSHDR','POSHDR')
    		   =TABLEUPDATE(.T.,.T.)
	      ELSE
	         =TABLEREVERT(.T.)
      	ENDIF
        
        SELECT POsln
        gfTableUpdate()

        SELECT CTKTBOM
        gfTableUpdate()
        
       ENDSCAN 
    ENDIF 
    
  ENDSCAN   
ENDIF
SELECT BOM
gfTableUpdate()

*!**************************************************************************
*! Name      : lfvCost
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/16/2008
*! Purpose   : Validate new Cost
*!**********************************************
FUNCTION lfvCost
PARAMETERS lcCstNo

IF lcNewCst1 <> m.nfCost&lcCstNo
  lnAlias = SELECT()
  SELECT (loFormSet.ariaForm1.mainworkorder1.cPoLine)
  lnOldVal = nICost&lcCstNo
  lnOldFVal= nFCost&lcCstNo
  m.Gros_Price = lcNewCst1
  m.nFCost&lcCstNo = lcNewCst1
  m.nfCost1 = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,3)
  
  
  m.nICost&lcCstNo = ;
  loFormSet.ariaForm1.mainworkorder1.mEquivCost(lcCstNo, m.nfCost1 ,IIF(lcCstNo='1',lnPricRate,lnDutyRate),IIF(lcCstNo='1',lnCurrUnt1,lnCurrUnt2),lcPriceCurr,lcDutyCurr)
 
  REPLACE nICost&lcCstNo WITH nICost&lcCstNo + m.nICost&lcCstNo - lnOldVal ,;
          nFCost&lcCstNo WITH nFCost&lcCstNo + m.nFCost&lcCstNo - lnOldFVal

  IF lcCstNo = '1'
    SELECT (loFormSet.ariaForm1.mainworkorder1.cPoLine)
    REPLACE Gros_Price WITH lcNewCst1 
  ENDIF  
ENDIF
