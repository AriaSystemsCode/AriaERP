*:************************************************************************
*: Program file  : SOCTITM.Prg
*: Program desc. : Catalog Sales By Item and Color by Page FOR CAM09
*: System        : Aria 4XP 
*: Module        : SO
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Date          : 12/03/2006
*: TASK NO       : T20061012.0020 
*: Entry No #    : C200718
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARSUMAG
*:************************************************************************

IF llOgFltCh
	llDontPrn=.F.
	lcStyCond=' .T. '
	lcHDCond=' .T. '
	lnMajor  = LEN(gfItemMask('PM'))
	lnNoMajor = LEN(gfItemMask('PN'))

  *Check style
	lcStyFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
	llStyFltr= !EMPTY(lcStyFltr) AND USED(lcStyFltr) AND RECCOUNT(lcStyFltr) > 0
	IF llStyFltr
	  SELECT (lcStyFltr)
	  INDEX ON CSTYMAJOR TAG (lcStyFltr)
	  lcStyCond=lcStyCond+" AND SEEK( CSTYMAJOR,'"+lcStyFltr+"')"
	ELSE
	  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
	    USE IN (lcStyFltr)
	  ENDIF
	  lcStyFltr= ''
	ENDIF
  * Check if there is a filter on CPURCODE code
	lcPURName = lfCheckFilter(1, 'STYLE.CPURCODE')  
	lcPUR  = loOgScroll.gfTempName()
	llPUR  = !EMPTY(lcPURName ) AND lfStr2Curs(lcPURName ,lcPUR  ,"CPUR")
	IF llPUR  
	  SELECT (lcPUR  )
	  INDEX on CPUR TAG (lcPUR  )
	  lcStyCond=lcStyCond+" AND SEEK(CPURCODE,'"+lcPUR  +"')"
	ENDIF
	
	* Check if there is a filter on division code
	lcDivName = lfCheckFilter(1, 'STYLE.CDIVISION')  
	lcDivs  = loOgScroll.gfTempName()
	llDivName  = !EMPTY(lcDivName ) AND lfStr2Curs(lcDivName ,lcDivs  ,"Cdiv")
	IF llDivName  
	  SELECT (lcDivs  )
	  INDEX on Cdiv TAG (lcDivs  )
	  lcStyCond=lcStyCond+" AND SEEK(CDIVISION,'"+lcDivs  +"')"
	ENDIF
  * Check if there is a filter on season code
	lcSeaName = lfCheckFilter(1, 'STYLE.SEASON')  
	lcSea  = loOgScroll.gfTempName()
	llSea  = !EMPTY(lcSeaName ) AND lfStr2Curs(lcSeaName ,lcSea  ,"CSea")
	IF llSea  
	  SELECT (lcSea  )
	  INDEX on CSea TAG (lcSea  )
	  lcStyCond=lcStyCond+" AND SEEK(SEASON,'"+lcSea  +"')"
	ENDIF
  * Check if there is a filter on GROUP code
	lcGrpName = lfCheckFilter(1, 'STYLE.CSTYGROUP')  
	lcGrp  = loOgScroll.gfTempName()
	llGrp  = !EMPTY(lcGrpName ) AND lfStr2Curs(lcGrpName ,lcGrp,"CGrp")
	IF llGrp  
	  SELECT (lcGrp)
	  INDEX on CGrp TAG (lcGrp)
	  lcStyCond=lcStyCond+" AND  SEEK(CSTYGROUP,'"+lcGrp+"')"
	ENDIF
  *CPAGE
	lcPGFltr= lfCheckFilter(1, 'STYLE.NPAGE_NO')
	llPGFltr= !EMPTY(lcPGFltr) AND USED(lcPGFltr) AND RECCOUNT(lcPGFltr) > 0
	IF llPGFltr
	  SELECT (lcPGFltr)
	  INDEX ON NPAGE_NO TAG (lcPGFltr)
	  lcStyCond=lcStyCond+" AND SEEK( STR(NPAGE_NO,3),'"+lcPGFltr+"')"
	ELSE
	  IF TYPE("lcPGFltr") = "C" AND USED(lcPGFltr)
	    USE IN (lcPGFltr)
	  ENDIF
	  lcPGFltr= ''
	ENDIF
  *ROYALITY
	lcRYLName = lfCheckFilter(1, 'STYLE.ROYALTY')  
	lcRYL  = loOgScroll.gfTempName()
	llRYL  = !EMPTY(lcRYLName ) AND lfStr2Curs(lcRYLName ,lcRYL,"CRYL")
	IF llRYL  
	  SELECT (lcRYL)
	  INDEX on CRYL TAG (lcRYL)
	  lcStyCond=lcStyCond+" AND  SEEK(ROYALTY,'"+lcRYL+"')"
	ENDIF
  *DISCOUNT
  lcDISName = lfCheckFilter(1, 'STYLE.CDISCCODE')  
	lcDIS  = loOgScroll.gfTempName()
	llDIS  = !EMPTY(lcDISName ) AND lfStr2Curs(lcDISName ,lcDIS,"CDIS")
	IF llDIS  
	  SELECT (lcDIS)
	  INDEX on CDIS TAG (lcDIS)
	  lcStyCond=lcStyCond+" AND  SEEK(CDISCCODE,'"+lcDIS+"')"
	ENDIF
  *COLOR
	lcCOLName = lfCheckFilter(1, 'SUBSTR(STYLE.STYLE,lnMajor+2,lnNoMajor)')  
	lcCOL  = loOgScroll.gfTempName()
	llCOL  = !EMPTY(lcCOLName ) AND lfStr2Curs(lcCOLName ,lcCOL,"CCOL")
	IF llCOL  
	  SELECT (lcCOL)
	  INDEX on CCOL TAG (lcCOL)
	  lcStyCond=lcStyCond+" AND  SEEK(SUBSTR(STYLE.STYLE,lnMajor+2,lnNoMajor),'"+lcCOL+"')"
	ENDIF
  lnCompPos  = lfItmPos('INVLINE.INVDATE')
  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1, ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],   ATC('|',laOGFxFlt[lnCompPos,6])+1)))
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  IF !llCmpSides 
    lcHDCond=lcHDCond+" AND  BETWEEN(DTOS(INVDATE),lcCompSt  ,lcCompEd  )"
  ENDIF 
	lcWorkfile =loOgScroll.gfTempName()
	=lfBuildTmp()
	=lfCollectData()
	SELECT (lcWorkfile )
	INDEX ON PAGE_NO+STYLE+LINNO TAG (lcWorkfile )
  IF !RECCOUNT()>0
	  llDontPrn=.T.
	  *-- Message : There are no records to display...!
	  *--                < Ok > 
	    =gfModalGen('TRM00052B40011','ALERT')
	  RETURN
	ENDIF  
	=lfAdjustCRSettings()
	IF USED(lcWorkfile )
	    USE IN (lcWorkfile )
	ENDIF

	=gfDispRe()
ELSE
  IF llDontPrn
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  
*************************************************************
*! Name      : lfwRepWhen
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 11/29/2006
*! Purpose   : Collecting the Data
*!*************************************************************
FUNCTION lfwRepWhen
lcStyPage=loOgScroll.gfTempName()
SELECT DISTINCT STR(NPAGE_NO,3) AS NPAGE_NO FROM STYLE WHERE NPAGE_NO<>0 INTO TABLE(oAriaApplication.WorkDir +  lcStyPage+ ".DBF")

*************************************************************
*! Name      : lfCollectData
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 11/29/2006
*! Purpose   : Collecting the Data
*!*************************************************************
FUNCTION lfCollectData

gfOpenTable(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
gfOpenTable(oAriaApplication.DataDir+'INVLINE',oAriaApplication.DataDir+'INVLINES','SH')
lcHDCond=lcHDCond+" AND TOTQTY>0 "
SELECT STYLE
GFSEEK('')
lcStyle=''
NLINE=1
NCOL=1
SCAN REST WHILE STYLE='' FOR &lcStyCond
  WAIT WINDOW "Collecting Data for Style #"+STYLE NOWAIT 
  SELECT INVLINE
  GFSEEK(STYLE.STYLE)
  LNTOTSHP=0
  SCAN REST WHILE STYLE=STYLE.STYLE FOR &lcHDCond
    LNTOTSHP=LNTOTSHP+TOTQTY
  ENDSCAN
  SELECT STYLE
  IF !(CSTYMAJOR==lcStyle)
 		IF LNTOTSHP>0
		  STORE '' TO M.COL1,M.COL2,M.COL3,M.COL4,M.COL5,M.COL6,M.COL7,M.COL8
		  STORE 0 TO M.VAL1,M.VAL2,M.VAL3,M.VAL4,M.VAL5,M.VAL6,M.VAL7,M.VAL8
		  lcStyle=CSTYMAJOR
		  NLINE=1
		  NCOL=1
		  M.style=STYLE.cstymajor
			M.desc=STYLE.desc
			M.page_no=IIF(STYLE.NPAGE_NO=0,'',STR(STYLE.NPAGE_NO,3))
			M.linno='1'
			M.COL1=gfCodDes(SUBSTR(STYLE.STYLE,lnMajor+2,lnNoMajor),'COLOR') 
			M.VAL1=LNTOTSHP
		  INSERT INTO (lcWorkfile) FROM MEMVAR 
	  ENDIF  
	ELSE
	  IF NCOL=8
   		IF LNTOTSHP>0
		    NCOL=1
		    NLINE=NLINE+1
		    Z=STR(NCOL,1) 
		    SELECT (lcWorkfile) 
		    APPEND BLANK 
		    REPLACE    &lcWorkfile..STYLE   WITH style.cstymajor;
		        			 &lcWorkfile..desc    WITH STYLE.desc;
		        			 &lcWorkfile..page_no WITH IIF(STYLE.NPAGE_NO=0,'',STR(STYLE.NPAGE_NO,3));
		        			 &lcWorkfile..linno   WITH ALLTRIM(STR(NLINE));
		        			 &lcWorkfile..COL1    WITH gfCodDes(SUBSTR(style.STYLE,lnMajor+2,lnNoMajor),'COLOR') ;
		        			 &lcWorkfile..VAL1    WITH LNTOTSHP
		  ENDIF       			 
	  ELSE
    		IF LNTOTSHP>0
		      NCOL=NCOL+1
			    Z=STR(NCOL,1) 
			    REPLACE    &lcWorkfile..COL&Z    WITH gfCodDes(SUBSTR(style.STYLE,lnMajor+2,lnNoMajor),'COLOR') ;
		          			 &lcWorkfile..VAL&Z WITH LNTOTSHP
      ENDIF
	  ENDIF
  ENDIF
ENDSCAN 
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[1,2]

loOgScroll.lcOGLastForm ='SOCTITM'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"
  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'Catalog Sales By Item and Color by Page'

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[20,18] ,laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON

laTempStru[1,1] = 'STYLE'
laTempStru[1,2] = 'C'
laTempStru[1,3] = lnMajor  
laTempStru[1,4] = 0

laTempStru[2,1] = 'DESC'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 30
laTempStru[2,4] = 0

laTempStru[3,1] = 'PAGE_NO'
laTempStru[3,2] = 'C'
laTempStru[3,3] = 3
laTempStru[3,4] = 0


laTempStru[4,1] = 'LINNO'
laTempStru[4,2] = 'C'
laTempStru[4,3] = 3
laTempStru[4,4] = 0

laTempStru[5,1] = 'COL1'
laTempStru[5,2] = 'C'
laTempStru[5,3] = 15
laTempStru[5,4] = 0

laTempStru[6,1] = 'VAL1'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 15
laTempStru[6,4] = 2


laTempStru[7,1] = 'COL2'
laTempStru[7,2] = 'C'
laTempStru[7,3] = 15
laTempStru[7,4] = 0

laTempStru[8,1] = 'VAL2'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 15
laTempStru[8,4] = 2


laTempStru[9,1] = 'COL3'
laTempStru[9,2] = 'C'
laTempStru[9,3] = 15
laTempStru[9,4] = 0

laTempStru[10,1] = 'VAL3'
laTempStru[10,2] = 'N'
laTempStru[10,3] = 15
laTempStru[10,4] = 2

laTempStru[11,1] = 'COL4'
laTempStru[11,2] = 'C'
laTempStru[11,3] = 15
laTempStru[11,4] = 0

laTempStru[12,1] = 'VAL4'
laTempStru[12,2] = 'N'
laTempStru[12,3] = 15
laTempStru[12,4] = 2

laTempStru[13,1] = 'COL5'
laTempStru[13,2] = 'C'
laTempStru[13,3] = 15
laTempStru[13,4] = 0

laTempStru[14,1] = 'VAL5'
laTempStru[14,2] = 'N'
laTempStru[14,3] = 15
laTempStru[14,4] = 2

laTempStru[15,1] = 'COL6'
laTempStru[15,2] = 'C'
laTempStru[15,3] = 15
laTempStru[15,4] = 0

laTempStru[16,1] = 'VAL6'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 15
laTempStru[16,4] = 2

laTempStru[17,1] = 'COL7'
laTempStru[17,2] = 'C'
laTempStru[17,3] = 15
laTempStru[17,4] = 0

laTempStru[18,1] = 'VAL7'
laTempStru[18,2] = 'N'
laTempStru[18,3] = 15
laTempStru[18,4] = 2

laTempStru[19,1] = 'COL8'
laTempStru[19,2] = 'C'
laTempStru[19,3] = 15
laTempStru[19,4] = 0

laTempStru[20,1] = 'VAL8'
laTempStru[20,2] = 'N'
laTempStru[20,3] = 15
laTempStru[20,4] = 2
=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat  


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Abdou Elgendi [ABD]
*! Date      : 07/03/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt

PRIVATE lnItmPos
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos


*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
*! Purpose   : Go top in the style IN RANGE
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
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE
