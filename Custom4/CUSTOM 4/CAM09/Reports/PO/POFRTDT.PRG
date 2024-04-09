*:************************************************************************
*: Program file  : POFRTDT.Prg
*: Program desc. : LANDED FREIGHT AND DUTY CHARGES
*: System        : Aria 4XP 
*: Module        : PO
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Date          : 11/26/2006
*: TASK NO       : T20061012.0021 
*: Entry No #    : C200716
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO POFRTDT
*:************************************************************************



IF llOgFltCh
	llDontPrn=.F.
	lcseek=' .T. '
	lcHdrCond=' .T.'
	lcLnCond=' .T.'
lnMajor  = LEN(gfItemMask('PM'))
	*Check PO NUMBER
	lcPOFltr= lfCheckFilter(1, 'POSHDR.PO')
	llPOFltr   = !EMPTY(lcPOFltr) AND USED(lcPOFltr) AND RECCOUNT(lcPOFltr) > 0
	IF llPOFltr   
	  SELECT (lcPOFltr)
	  INDEX ON PO  TAG (lcPOFltr)
	  lcHdrCond=lcHdrCond+" AND SEEK(PO ,'"+lcPOFltr+"')"
	ELSE
	  IF TYPE("lcPOFltr") = "C" AND USED(lcPOFltr)
	    USE IN (lcPOFltr)
	  ENDIF
	  lcPOFltr= ''
	ENDIF
* Check if there is a filter on SHIPVIA code
	lcShpName = lfCheckFilter(1, 'POSHDR.SHIPVIA')  
	lcShp  = loOgScroll.gfTempName()
	llShp  = !EMPTY(lcShpName ) AND lfStr2Curs(lcShpName ,lcShp  ,"CShp")
	IF llShp  
	  SELECT (lcShp  )
	  INDEX on CShp TAG (lcShp  )
	  lcHdrCond=lcHdrCond+" AND SEEK(SHIPVIA,'"+lcShp  +"')"
	ENDIF
	* Check if there is a filter on division code
	lcDivName = lfCheckFilter(1, 'POSHDR.CDIVISION')  
	lcDivs  = loOgScroll.gfTempName()
	llDivName  = !EMPTY(lcDivName ) AND lfStr2Curs(lcDivName ,lcDivs  ,"Cdiv")
	IF llDivName  
	  SELECT (lcDivs  )
	  INDEX on Cdiv TAG (lcDivs  )
	  lcHdrCond=lcHdrCond+" AND SEEK(CDIVISION,'"+lcDivs  +"')"
	ENDIF

*Check style
	lcStyFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
	llStyFltr= !EMPTY(lcStyFltr) AND USED(lcStyFltr) AND RECCOUNT(lcStyFltr) > 0
	IF llStyFltr
	  SELECT (lcStyFltr)
	  INDEX ON CSTYMAJOR TAG (lcStyFltr)
	  lcLnCond=lcLnCond+" AND SEEK(LEFT(POSLN.STYLE,lnMajor) ,'"+lcStyFltr+"')"
	ELSE
	  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
	    USE IN (lcStyFltr)
	  ENDIF
	  lcStyFltr= ''
	ENDIF
* Check if there is a filter on season code
	lcSeaName = lfCheckFilter(1, 'STYLE.SEASON')  
	lcSea  = loOgScroll.gfTempName()
	llSea  = !EMPTY(lcSeaName ) AND lfStr2Curs(lcSeaName ,lcSea  ,"CSea")
	IF llSea  
	  SELECT (lcSea  )
	  INDEX on CSea TAG (lcSea  )
	  lcLnCond=lcLnCond+" AND SEEK(POSHDR.SEASON,'"+lcSea  +"')"
	ENDIF

* Check if there is a filter on GROUP code
	lcGrpName = lfCheckFilter(1, 'STYLE.CSTYGROUP')  
	lcGrp  = loOgScroll.gfTempName()
	llGrp  = !EMPTY(lcGrpName ) AND lfStr2Curs(lcGrpName ,lcGrp,"CGrp")
	IF llGrp  
	  SELECT (lcGrp)
	  INDEX on CGrp TAG (lcGrp)
	  lcLnCond=lcLnCond+" AND GFSEEK(POSLN.STYLE,'STYLE') AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
	ENDIF

  lnCompPos  = lfItmPos('POSHDR.COMPLETE')
  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  IF !llCmpSides 
    lcLnCond=lcLnCond+" AND  BETWEEN(DTOS(DATE),lcCompSt  ,lcCompEd  )"
  ENDIF 

	lcWorkfile =loOgScroll.gfTempName()

	=lfBuildTmp()
  SELECT (lcWorkfile )
  INDEX ON shipvia+STYLE +po+DTOC(DATE) TAG (lcWorkfile )
  SET ORDER TO &lcWorkfile 
	=lfCollectData()

	SELECT (lcWorkfile )
	DO CASE 
		CASE lcSrtBy='S'
	    INDEX ON shipvia+STYLE TAG (lcWorkfile )
		CASE lcSrtBy='P'
	    INDEX ON shipvia+po TAG (lcWorkfile )
		CASE lcSrtBy='D'
	    INDEX ON shipvia+DTOC(DATE) TAG (lcWorkfile )
  ENDCASE 
  SET ORDER TO &lcWorkfile 
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


FUNCTION lfwRepWhen
*************************************************************
*! Name      : lfCollectData
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 11/29/2006
*! Purpose   : Collecting the Data
*!*************************************************************
FUNCTION lfCollectData

* Open Tables poshdr, posln,style,codes
gfOpenTable(oAriaApplication.DataDir+'POSLN',oAriaApplication.DataDir+'POSLN','SH')
gfOpenTable(oAriaApplication.DataDir+'POSHDR',oAriaApplication.DataDir+'POSHDR','SH')
gfOpenTable(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
IF !(llPOFltr OR llDivName OR llShp  ) AND (llStyFltr OR llGrp  OR llSea  )
  SELECT POSLN
	GFSEEK('PP')
  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD='PP' FOR &lcLnCond AND posln.TRANCD='2'
	  WAIT WINDOW "Collecting Data for PO #"+po NOWAIT 
    =lfInsrt(.t.)
  ENDSCAN 
  
ELSE
	SELECT POSHDR
	GFSEEK('PP')
	SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO='PP' FOR &lcHdrCond
	  WAIT WINDOW "Collecting Data for PO.... #"+po NOWAIT 
	  lcPoseek=CBUSDOCU+CSTYTYPE+PO
	  SELECT POSLN
	  IF GFSEEK(lcPoseek)
	    SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=lcPoseek FOR &lcLnCond AND posln.TRANCD='2'
	      =lfInsrt(.f.)
	    ENDSCAN 
	  ENDIF   
	ENDSCAN 
ENDIF 
*!*************************************************************
*! Name      : lfInsrt
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 28/11/2006
*! Purpose   : Inset Data Into TempFile
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfInsrt
LPARAMETERS llPoHdr
IF llPoHdr
  =GFSEEK('PP'+POSLN.PO,'POSHDR')
ENDIF 
M.style=LEFT(POSLN.STYLE,lnMajor  )
M.desc=IIF(gfseek(M.style,'STYLE'),STYLE.desc,'')
M.SHIPVIA=POSLN.shipvia
M.DATE=POSLN.DATE
M.PO=POSLN.PO

IF SEEK(M.SHIPVIA+M.style+M.PO+DTOC(M.DATE),lcWorkfile)
  REPLACE  &lcWorkfile..P_COST  WITH (&lcWorkfile..P_COST * &lcWorkfile..NTOTQTY+POSLN.nlan_cost1*POSLN.TOTQTY)/(&lcWorkfile..NTOTQTY+POSLN.TOTQTY);
	         &lcWorkfile..FRT_pc  WITH (&lcWorkfile..FRT_pc * &lcWorkfile..NTOTQTY+POSLN.nlan_cost3*POSLN.TOTQTY)/(&lcWorkfile..NTOTQTY+POSLN.TOTQTY);
	         &lcWorkfile..DUT_pc  WITH (&lcWorkfile..DUT_pc * &lcWorkfile..NTOTQTY+POSLN.nlan_cost4*POSLN.TOTQTY)/(&lcWorkfile..NTOTQTY+POSLN.TOTQTY);
	         &lcWorkfile..MISC_pc WITH (&lcWorkfile..MISC_pc* &lcWorkfile..NTOTQTY+POSLN.nlan_cost5*POSLN.TOTQTY)/(&lcWorkfile..NTOTQTY+POSLN.TOTQTY);
	         &lcWorkfile..NTOTQTY WITH (&lcWorkfile..NTOTQTY+POSLN.TOTQTY)
ELSE
  M.SHIP_NAME=gfCodDes(M.SHIPVIA , 'SHIPVIA')
	M.COUNTRY=poshdr.origin
  M.P_COST =POSLN.nlan_cost1
	M.FRT_pc =POSLN.nlan_cost3
	M.DUT_pc =POSLN.nlan_cost4
	M.MISC_pc=POSLN.nlan_cost5
	M.NTOTQTY=POSLN.TOTQTY
	M.CCDATE=DTOC(POSLN.DATE)
  INSERT INTO (lcWorkfile ) FROM MEMVAR 
ENDIF 

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[3,2]

loOgScroll.lcOGLastForm ='POFRTDT'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'LANDED FREIGHT AND DUTY CHARGES'

loOgScroll.laCRParams[2,1] = 'GRP'
loOgScroll.laCRParams[2,2]= lcSrtBy
DO CASE 
	CASE lcSrtBy='S'
    loOgScroll.laCRParams[2,2]= 'S'
	CASE lcSrtBy='P'
    loOgScroll.laCRParams[2,2]= 'P'
	CASE lcSrtBy='D'
    loOgScroll.laCRParams[2,2]= 'D'
ENDCASE 



loOgScroll.laCRParams[3,1] = 'LAYOUT'
DO CASE 
	CASE lcSrtBy='S'
    loOgScroll.laCRParams[3,2]= 'Style'
	CASE lcSrtBy='P'
    loOgScroll.laCRParams[3,2]= 'PO #'
	CASE lcSrtBy='D'
    loOgScroll.laCRParams[3,2]= 'Date'
ENDCASE 





*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[13,18] ,laTempCOM[1,18]
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

laTempStru[3,1] = 'PO'
laTempStru[3,2] = 'C'
laTempStru[3,3] = 6
laTempStru[3,4] = 0


laTempStru[4,1] = 'DATE'
laTempStru[4,2] = 'D'
laTempStru[4,3] = 8
laTempStru[4,4] = 0

laTempStru[5,1] = 'COUNTRY'
laTempStru[5,2] = 'C'
laTempStru[5,3] = 20
laTempStru[5,4] = 0

laTempStru[6,1] = 'P_COST'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 15
laTempStru[6,4] = 2

laTempStru[7,1] = 'SHIPVIA'
laTempStru[7,2] = 'C'
laTempStru[7,3] = 6
laTempStru[7,4] = 0

laTempStru[8,1] = 'SHIP_NAME'
laTempStru[8,2] = 'C'
laTempStru[8,3] = 30
laTempStru[8,4] = 0

laTempStru[9,1] = 'FRT_PC'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 15
laTempStru[9,4] = 2

laTempStru[10,1] = 'DUT_PC'
laTempStru[10,2] = 'N'
laTempStru[10,3] = 15
laTempStru[10,4] = 2

laTempStru[11,1] = 'MISC_PC'
laTempStru[11,2] = 'N'
laTempStru[11,3] = 15
laTempStru[11,4] = 2

laTempStru[12,1] = 'NTOTQTY'
laTempStru[12,2] = 'N'
laTempStru[12,3] = 15
laTempStru[12,4] = 2

laTempStru[13,1] = 'CCDATE'
laTempStru[13,2] = 'C'
laTempStru[13,3] = 8
laTempStru[13,4] = 0

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
