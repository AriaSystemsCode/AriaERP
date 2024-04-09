*:*****************************************************************************************
*: Program file  : ICOPSTY.PRG
*: Program desc. : Copying styles from one company to another with it's corresponding data.
*:               : For GMA (C#200406)
*: For screen    : Option Grid
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*!     Developer : Mohamed Shokry (MHM)
*!     Date      : 09/05/2002
*:*****************************************************************************************

*--if come from aria program
llRnFrmOg = IIF(TYPE('lllog_in')='U',.F.,.T.)


IF !llRnFrmOg
  gcWorkDir  = "R:\ARIA27\WORK\"
  gcWorkDir  = ALLTRIM(gcWorkDir) 
  lcMemoFile = (gcWorkDir+'lcTmpMemIc.MEM')
  IF !FILE(lcMemoFile)
    WAIT WINDOW " Please Select Correct Path"
    RETURN
  ENDIF

  IF TYPE('lcMemoFile')#'C'
  	RETURN
  ENDIF
  RESTORE FROM (lcMemoFile) ADDI
  gcDataDir  = lcRpDtDir     && Save The Data Directory.
  gcWorkDir  = lcRpWrDir     && Save The Work Directory.
  gcSysHome  = lcRpSysDir    && Save The system  Directory.
  gcRepHome  = lcRpRpDir     && Save The report Directory.
  gcAct_Comp = lcRpComp      && Save The Active Company.
  gcDef_Path = lcRpDefPth    && Save The DefPath.
ENDIF

STORE '' TO lcRpType, lcRpCompId ,lcRpDateAd,lcRpStatus,lcFilHandl
STORE .F. TO llCallOG,llStyOpn,llDeptOpn
lcStyFile  = gfTempName()
lcDeptFile = gfTempName()
lcGropFile = gfTempName()
lcCompFile = gfTempName()

=gfOpenFile(gcDataDir+'STYDYE' ,gcDataDir+'STYDYE','SH')
=gfOpenFile(gcDef_Path+'ICTOCOMP',gcDef_Path+'ICTOCOMP','EX')
=gfOpenFile(gcDef_Path+'ICGROUP',gcDef_Path+'ICGROUP','EX')
=gfOpenFile(gcDef_Path+'ICDEPT',gcDef_Path+'DEPT','EX')
=gfOpenFile(gcDef_Path+'ICSTYLE',gcDef_Path+'ICSTYLE','EX')
=gfOpenFile(gcDef_Path+'ICGETVAR','','SH')
*-- Running as Separeate File Case 
IF llRNFrmOG
  lcExpr = gfOpGrid('ICOPSTY' , .T.)  
  
  IF  TYPE('lcExpr')<>'L'
    IF !USED(lcCompFile)
      WAIT WINDOW " Please choose to Company"
      RETURN
    ENDIF
    IF llCallOG AND lcRpType $ "BS"  && if called from Option grid and press select save setting
      =lfSavSet()
    ENDIF
  ELSE
    RETURN
  ENDIF
ELSE
  SELECT ICTOCOMP
  LOCATE
  IF EOF()
    WAIT WINDOW " Please Go back to Option Grid to choose target Company"
    RETURN
  ENDIF
  =lfRnFrmPrg()
ENDIF

IF llRNFrmOG AND lcRpType $ "S"
  RETURN
ENDIF

*--open TXT file to write on it
=lfOpenTxt()
SELECT (lcCompFile)
LOCATE
SCAN
  IF lcRpCompId = cComp_id
    LOOP
  ENDIF  
  =lfvCopy(lcRpCompId,cComp_id)
ENDSCAN 
=FCLOSE(lcFilHandl)
IF !llRnFrmOg
  CLEA ALL
ENDIF  
*!*************************************************************
*! Name      : FUNCTION lfvCopy
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   :The valid function of the Proceed button.
*!*************************************************************
FUNCTION lfvCopy
PARAMETER lcdir1,lcdir2

gcDataDir = ALLTRIM(gcDataDir)
lcDir1 = LEFT(gcDataDir,LEN(gcDataDir)-3) + lcDir1+'\'
lcDir2 = LEFT(gcDataDir,LEN(gcDataDir)-3) + lcDir2+'\'
*--Create the temp files.
lcTmpSty   = gfTempName ()
lcTmpCode  = gfTempName()
lcTmpScale = gfTempName()
lcRpStatus = IIF(lcRpStatus="B","AH",lcRpStatus)

IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF  
= gfOpenFile(lcdir1+'STYLE' ,lcdir1+'STYLE','SH')

IF USED('STYDYE')
  SELECT STYDYE
  USE
ENDIF  
= gfOpenFile(lcdir1+'STYDYE' ,lcdir1+'STYDYE','SH')


IF USED('ICDEPTHD')
  SELECT ICDEPTHD
  USE
ENDIF  
= gfOpenFile(lcdir1+'ICDEPTHD' ,lcdir1+'DEPTHD','SH')

IF USED('SCALE')
  SELECT SCALE
  USE
ENDIF  
= gfOpenFile(lcdir1+'SCALE' ,lcdir1+'SCALE','SH')

IF USED('CODES')
  SELECT CODES
  USE
ENDIF  
= gfOpenFile(lcdir1+'CODES' ,lcdir1+'CODES','SH')
    
IF USED('ICISTRU')
  SELECT ICISTRU
  USE
ENDIF  
= gfOpenFile(lcdir1+'ICISTRU' ,lcdir1+'SEGNO','SH')


SELECT CODES
COPY ALL TO (gcWorkDir+lcTmpCode)

*-- Copy source Scale file to a temp file to be used instead
SELECT SCALE
COPY ALL TO (gcWorkDir+lcTmpScale)

=gfOpenFile(gcWorkDir+lcTmpCode,'','EX')
INDEX ON cdefcode+ccode_no+crltfield+cfld_name TAG &lcTmpCode

=gfOpenFile(gcWorkDir+lcTmpScale,'','EX')
INDEX ON TYPE+SCALE+PREPAK TAG &lcTmpScale

=lfTempSty()
IF USED(lcStyFile)
  SELECT (lcStyFile)
  LOCATE
  IF !EOF()
    =lfDatBySty()
  ELSE
    =lfDataByAl()
  ENDIF
ELSE
  =lfDataByAl()
ENDIF  

*-- If no styles selected , display a massage then return from the function
SELECT &lcTmpSty
LOCATE
IF EOF()
  WAIT WINDOW 'No Styles selected.'
  RETURN
ENDIF

*-- Open the distenation files "STYLE,CODES,SCALE"
IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF  
=gfOpenFile(lcdir2+'STYLE',lcdir2+'STYLE','SH')

*-- Open the distenation files "STYLE,CODES,SCALE"
IF USED('STYDYE')
  SELECT STYDYE
  USE
ENDIF  
=gfOpenFile(lcdir2+'STYDYE',lcdir2+'STYDYE','SH')

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
  Xcdefware   = cdefware
  Xroyalty    = royalty
  XPurCode    = CPURCODE  
  XdiscCode    = CDISCCODE 

  Xctaxcode   = CTAXCODE
  Xccomcode   = CCOMCODE
  Xcvensty    = CVENSTY
  Xqty_ctn    = QTY_CTN
  Xstart      = START
  Xsoldout    = SOLDOUT
  Xmake       = MAKE
  lcSty       = STYLE
  XStyMaj     = CSTYMAJOR
  *--GMA User defind field
  Xstycatg    = cstycatg
  Xstysbclas  = cstysbclas
  Xstyclass   = cstyclass
  Xstysbcls2  = cstysbcls2
  Xstysbcls3  = cstysbcls3
  XDEPT       = DEPT
  XNINPACK    = NINPACK
  XMPACK      = NMPACK
  XNMPCUBE    = NMPCUBE
  XNMPDEPTH   = NMPDEPTH
  XNMPDIM     = NMPDIM
  XNMPWEIGHT  = NMPWEIGHT
  XNMSPRICE   = NMSPRICE
  XCHTSNO     = CHTSNO
  
  
  *--If the style was copied before or already found in the distenation company ignor it.
  IF SEEK(lcSty,'STYLE')
  	WAIT WINDOW "STYLE " + ALLTRIM(lcsty) +" ALREADY EXIST" NOWAIT
    =lfVryRport(lcSty,"STYLE " + ALLTRIM(lcsty) +" ALREADY EXIST",lcRpCompId)
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
   REPLACE cstycatg    WITH  Xstycatg ,;
           cstysbclas  WITH Xstysbclas,;
           cstyclass   WITH Xstyclass ,;
           cstysbcls2  WITH Xstysbcls2,;           
           cstysbcls3  WITH Xstysbcls3,;
           DEPT        WITH XDEPT     ,;
           NINPACK     WITH XNINPACK  ,;
           NMPACK      WITH XMPACK    ,;
           NMPCUBE     WITH XNMPCUBE  ,;
           NMPDEPTH    WITH XNMPDEPTH ,;
           NMPDIM      WITH XNMPDIM   ,;
           NMPWEIGHT   WITH XNMPWEIGHT,;
           NMSPRICE    WITH XNMSPRICE ,;
           CHTSNO      WITH XCHTSNO 
           
    *--StyDye updates
    SELECT STYDYE
    APPEND BLANK
    REPLACE STYLE      WITH lcsty,;      
            DESC       WITH Xdesc,;
            CWARECODE  WITH Xcdefware,;
            Gl_Link    WITH Xlink_code
    
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
       =lfVryRport(&lcTmpSty..STYLE,'Scale '+&lcTmpSty..SCALE+' not found,cannot copy.',lcRpCompId)
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
       =lfVryRport(&lcTmpSty..STYLE,'Prepak '+&lcTmpSty..Prepak+' not found',lcRpCompId)
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
  WAIT WINDOW 'The styles are successfully copied.'
ELSE
  
ENDIF  


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
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
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec

lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
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
*-- end of lfStySum.
*!**************************************************************************
*! Name      : lfStDepart
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfStDepart()
*!**************************************************************************
*
FUNCTION lfStDepart
PARAMETERS OpGrdParm
SELECT ICDEPTHD   

DO CASE
  CASE  OpGrdParm='S'           
    lcTmpIndex =gfTempName()
    SELECT  DISTINCT DEPT,cDeptDesc FROM ICDEPTHD INTO DBF (gcWorkDir + lcTmpIndex)
     USE IN ICDEPTHD
    use in (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS ICDEPTHD
    SELECT ICDEPTHD
    INDEX ON DEPT TAG Depthd
    GO TOP
  CASE  OpGrdParm='R'
    USE IN ICDEPTHD
    =gfOpenFile(gcDataDir+'ICDEPTHD',gcDataDir+'DEPTHD ','SH')
ENDCASE
*-- End of lfStDepart.

*!*************************************************************
*! Name      : lfDefComp                              
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   : Function to get the default value of the company id
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Company ID
*!*************************************************************
*! Example            :  =lfDefComp()
*!*************************************************************
*
FUNCTION lfDefComp

*IF Statment to check if there is an active company
IF !EMPTY(gcAct_Comp)
  RETURN gcAct_Comp
ELSE    && Else
  SELECT SYCCOMP
  GO TOP
  RETURN SYCCOMP.cComp_Id
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvComp                              
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
*! Purpose   : Function to get the default value of the company id
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Company ID
*!*************************************************************
*! Example            :  =lfvComp()
*!*************************************************************

FUNCTION lfvComp
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'cComp_ID:H="Company ID",cCom_Name:H="Company Name"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpCurFld = VARREAD()
IF .NOT. EMPTY(&lcRpCurFld)  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  IF NOT USED("SYCCOMP") 
    SELECT 0
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id
    llUesdBefo = .T.
  ENDIF
  SELECT SYCCOMP
  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld)) 
        =gfBrows(.F.,'cComp_ID',"laRpRetFld",'Company information',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  lcCom_Ddir  = ALLTRIM(cCom_Ddir)

  IF llUesdBefo       && .F.- this file used by the system
    USE IN SYCFISHD
  ENDIF

  IF gcAct_Comp <> lcRpCompId

    SELECT STYLE
    USE
    = gfOpenFile(lcCom_Ddir+'STYLE' ,lcCom_Ddir+'STYLE','SH')

    IF USED('STYDYE')
      SELECT STYDYE
      USE
    ENDIF  
    = gfOpenFile(lcCom_Ddir+'STYDYE' ,lcCom_Ddir+'STYDYE','SH')

    SELECT ICDEPTHD
    USE
    = gfOpenFile(lcCom_Ddir+'ICDEPTHD' ,lcCom_Ddir+'DEPTHD','SH')

    SELECT SCALE
    USE
    = gfOpenFile(lcCom_Ddir+'SCALE' ,lcCom_Ddir+'SCALE','SH')
    SELECT CODES
    USE
    = gfOpenFile(lcCom_Ddir+'CODES' ,'lcCom_Ddir+CODES','SH')
    = gfOpenFile(lcCom_Ddir+'ICISTRU' ,lcCom_Ddir+'SEGNO','SH')
    
    CLEAR READ
  ELSE
    SELECT STYLE
    USE
    = gfOpenFile(gcDataDir+'STYLE' ,gcDataDir+'STYLE','SH')

    IF USED('STYDYE')
      SELECT STYDYE
      USE
    ENDIF  

    = gfOpenFile(gcDataDir+'STYDYE' ,gcDataDir+'STYDYE','SH')

    SELECT ICDEPTHD
    USE
    = gfOpenFile(gcDataDir+'ICDEPTHD' ,gcDataDir+'DEPTHD','SH')

    SELECT SCALE
    USE
    = gfOpenFile(gcDataDir+'SCALE' ,gcDataDir+'SCALE','SH')
    SELECT CODES
    USE
    = gfOpenFile(gcDataDir+'CODES' ,gcDataDir+'CODES','SH')
    
    IF USED('ICISTRU')
      SELECT ICISTRU
      USE
    ENDIF  
    = gfOpenFile(gcDataDir+'ICISTRU' ,gcDataDir+'SEGNO','SH')
    
  ENDIF
  *--to get From Company 
  SELECT (lcOldAlias)
ENDIF

RETURN 
*!*************************************************************
*! Name      : lfSavSet                  
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save Setting to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : ICOPSTY
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfSavSet()
*!*************************************************************
*!
FUNCTION lfSavSet

*-- To Save the selected Styles if any
llSelSty  = .F.
IF !EMPTY(lcStyFile) AND USED(lcStyFile)
  SELECT (lcStyFile)
  GOTO TOP
  llSelSty  = !EOF()
ENDIF  
IF llSelSty
  =lfSavSty()
ENDIF

*-- To Save the selected Departement if any
llSelDept  = .F.
IF !EMPTY(lcDeptFile) AND USED(lcDeptFile)
  SELECT (lcDeptFile)
  GOTO TOP
  llSelDept = !EOF()
ENDIF  
IF llSelDept
  =lfSavDpt()
ENDIF

*-- To Save the selected Group if any
llSelGrop  = .F.
IF !EMPTY(lcGropFile) AND USED(lcGropFile)
  SELECT (lcGropFile)
  GOTO TOP
  llSelGrop = !EOF()
ENDIF  
IF llSelGrop
  =lfSavGrp()
ENDIF

*-- To Save the selected Group if any
llSelComp  = .F.
IF !EMPTY(lcCompFile) AND USED(lcCompFile)
  SELECT (lcCompFile)
  GOTO TOP
  llSelComp = !EOF()
ENDIF  
IF llSelComp
  =lfSavComp()
ENDIF

*-- Save all Other Settings -Not in list-
=lfvAllSet()

*!*************************************************************
*! Name      : lfSavSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save Style to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfSavSet 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfSavSty()
*!*************************************************************
*!
FUNCTION lfSavSty

*--open custom IcStyle file
SELECT ICSTYLE
ZAP
SELECT (lcStyFile)
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO ('ICSTYLE') FROM MEMVAR
ENDSCAN

*!*************************************************************
*! Name      : lfSavDpt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save Departement to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfSavSet 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfSavDpt()
*!*************************************************************
*!
FUNCTION lfSavDpt

*--open custom ICDEPT file
SELECT ICDEPT
ZAP
SELECT (lcDeptFile)
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO ('ICDEPT') FROM MEMVAR
ENDSCAN

*!*************************************************************
*! Name      : lfSavGrp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save Group to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfSavSet 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfSavGrp()
*!*************************************************************
*!
FUNCTION lfSavGrp

*--open custom ICGROUP file
SELECT ICGROUP
ZAP
SELECT (lcGropFile)
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO ('ICGROUP') FROM MEMVAR
ENDSCAN

*!*************************************************************
*! Name      : lfSavComp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save to Company to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfSavSet 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfSavComp()
*!*************************************************************
*!
FUNCTION lfSavComp

*--open custom ICTOCOMP file
SELECT ICTOCOMP
ZAP
SELECT (lcCompFile)
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO ('ICTOCOMP') FROM MEMVAR
ENDSCAN
*!*************************************************************
*! Name      : lfvAllSet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/09/2002
*! Purpose   : Function to Save to allother variables to Custom files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfSavSet 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfvAllSet()
*!*************************************************************
*!
FUNCTION lfvAllSet

SELECT ICGETVAR
LOCATE
IF !EOF()
  REPLACE ccomp_id   WITH lcRpCompId,;
          DATE       WITH lcRpDateAd,;
          Status     WITH lcRpStatus
ELSE
  APPEND BLANK
  REPLACE ccomp_id WITH lcRpCompId,;
          DATE     WITH lcRpDateAd,;
          Status   WITH lcRpStatus
ENDIF
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 09/03/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

STORE .T. TO llCallOG

lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
ENDIF

*-- To Save the selected Departement if any
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ICDEPTHD.DEPT'),1)
IF lnPosition > 0
  lcDeptFile = laOGFxFlt[lnPosition,6]
ENDIF

*-- To Save the selected Group if any
lnPosition = ASUBSCRIPT(laOGVrFlt,ASCAN(laOGVrFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGropFile= laOGVrFlt[lnPosition,6]
ENDIF

*-- To Save the selected to Company if any
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SYCCOMP.CCOMP_ID'),1)
IF lnPosition > 0
  lcCompFile= laOGFxFlt[lnPosition,6]
ENDIF

*-- check if there is data found Retrive
=lfRnFrmPrg()

*-- END lfwRepWhen 
*!*************************************************************
*! Name      : lfRnFrmPrg
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 09/03/2002
*! Purpose   : Initilize variable to run In casde of run from schadule
*!           : Or retrive it in when initilize Option Grid
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRnFrmPrg()
*!*************************************************************
*!
FUNCTION lfRnFrmPrg

SELECT ICSTYLE
GOTO TOP
IF !EOF()
  IF USED(lcStyFile)
    SELECT (lcStyFile)
    ZAP
    SELECT ICSTYLE
    COPY ALL TO (gcWorkDir+lcStyFile)
  ELSE
    COPY ALL TO (gcWorkDir+lcStyFile)
    =gfOpenFile(gcWorkDir+lcStyFile,'','EX')
    INDEX ON cStyMajor TAG &lcStyFile
  ENDIF
ENDIF

SELECT ICDEPT
GOTO TOP
IF !EOF()
  IF USED(lcDeptFile)
    SELECT (lcDeptFile)
    ZAP
    SELECT ICDEPT
    COPY ALL TO (gcWorkDir+lcDeptFile)
  ELSE
    COPY ALL TO (gcWorkDir+lcDeptFile)
    =gfOpenFile(gcWorkDir+lcDeptFile,'','EX')
    INDEX ON Dept TAG &lcDeptFile
  ENDIF  
ENDIF

SELECT ICGROUP
GOTO TOP
IF !EOF()
  IF USED(lcGropFile)
    SELECT (lcGropFile)
    ZAP
    SELECT ICGROUP
    COPY ALL TO (gcWorkDir+lcGropFile)
  ELSE
    COPY ALL TO (gcWorkDir+lcGropFile)
    =gfOpenFile(gcWorkDir+lcGropFile,'','EX')
    INDEX ON cCode_No TAG &lcGropFile  
  ENDIF  
ENDIF

SELECT ICTOCOMP
GOTO TOP
IF !EOF()
  IF USED(lcCompFile)
    SELECT (lcCompFile)
    ZAP
    SELECT ICTOCOMP
    COPY ALL TO (gcWorkDir+lcCompFile)
  ELSE
    COPY ALL TO (gcWorkDir+lcCompFile)
    =gfOpenFile(gcWorkDir+lcCompFile,'','EX')
    INDEX ON cComp_Id TAG &lcCompFile  
  ENDIF  
ENDIF

SELECT ICGETVAR
LOCATE
IF !EOF()
  lcRpCompId = ALLTRIM(ICGETVAR.ccomp_id)
  lcRpDateAd = ICGETVAR.DATE
  lcRpStatus = ICGETVAR.Status
ENDIF  
IF llRnFrmOg
  *--store global variables in  memo file
  lcRpDtDir  = gcDataDir  && Save The Data Directory.
  lcRpWrDir  = gcWorkDir  && Save The Work Directory.
  lcRpSysDir = gcSysHome  && Save The system  Directory.
  lcRpRpDir  = gcRepHome  && Save The report Directory.
  lcRpComp   = gcAct_Comp && Save The Active Company.
  lcRpDefPth = gcDef_Path && Save The DefPath.
  SAVE TO (gcWorkDir+'lcTmpMemIc.MEM') ALL LIKE l?RP* 
ENDIF  

*!*************************************************************
*! Function  : lfVryRport
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 09/03/2002
*! Purpose   : Update log file
*!*************************************************************
FUNCTION lfVryRport
PARAMETER lcStyRj,lcReason,lcCom_id
=FPUTS(lcFilHandl,"Style " + lcStyRj + " Rejected Because " + lcReason+ " For Company " + lcCom_id)

*!*************************************************************
*! Name      : lfOpenTxt
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 09/03/2002
*! Purpose   : Open or create Text File
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpenTxt()
*!*************************************************************
*!
FUNCTION lfOpenTxt

lcFilHandl= FCREATE('Log.txt') 	  	  && If not create it 

****************************************************************************
*! PROG      : lfDatBySty.PRG   (C#200156)
*! DESC      : function To scan for the selected Styles
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/09/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*-- 
FUNCTION lfDatBySty

SELECT (lcStyFile)
GOTO TOP
SCAN
  =SEEK(ALLTRIM(cStyMajor),'STYLE')
  SELECT STYLE
  SCAN REST WHILE STYLE = ALLTRIM(&lcStyFile..cStyMajor) FOR (dAdd_Date > lcRpDateAd )
    IF !(Status $ lcRpStatus)
      =lfVryRport(STYLE,'Status is '+Status+' ,cannot copy.',lcRpCompId)
      LOOP
    ENDIF  
    WAIT WINDOW "Collecting data for style " + style NOWAIT

    IF USED(lcDeptFile)
      IF !SEEK(ALLTRIM(Dept),lcDeptFile)
        =lfVryRport(STYLE,'Department '+Dept+' not found,cannot copy.',lcRpCompId)
        LOOP
      ENDIF
    ENDIF
      
    IF USED(lcGropFile)
      IF !SEEK(ALLTRIM(cStyGroup),lcGropFile)
        =lfVryRport(STYLE,'Group '+cStyGroup+' not found,cannot copy.',lcRpCompId)
        LOOP
      ENDIF
    ENDIF
      
    SCATTER MEMVAR MEMO
    INSERT INTO(lcTmpSty) FROM MEMVAR
  ENDSCAN
ENDSCAN

****************************************************************************
*! PROG      : lfDataByAl.PRG   
*! DESC      : function To collect all lines from posln
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/05/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByAl

SELECT STYLE
SCAN FOR (dAdd_Date > lcRpDateAd )

  IF !(Status $ lcRpStatus)
    =lfVryRport(STYLE,'Status is '+Status+' ,cannot copy.',lcRpCompId)
    LOOP
  ENDIF  
  WAIT WINDOW "Collecting data for style " + style NOWAIT
  
  IF USED(lcDeptFile)
    IF !SEEK(ALLTRIM(Dept),lcDeptFile)
      =lfVryRport(STYLE,'Department '+Dept+' not found,cannot copy.',lcRpCompId)
      LOOP
    ENDIF
  ENDIF  
  
  IF USED(lcGropFile)
    IF !SEEK(ALLTRIM(cStyGroup),lcGropFile)
      =lfVryRport(STYLE,'Group '+cStyGroup+' not found,cannot copy.',lcRpCompId)
      LOOP
    ENDIF
  ENDIF
    
  SCATTER MEMVAR MEMO
  INSERT INTO(lcTmpSty) FROM MEMVAR
ENDSCAN

****************************************************************************
*! PROG      : lfTempSty   
*! DESC      : function To Create temp style file
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfTempSty

SELECT STYLE
=AFIELDS(laStyFld)
CREATE TABLE (gcWorkDir+lcTmpSty) FROM ARRAY laStyFld
INDEX ON Style TAG &lcTmpSty
=gfOpenFile (gcWorkDir+lcTmpSty,' ','EX')


*!*************************************************************
*! Name      : gfTempName
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Creat temp file name
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfTempName

lcRetName = ("X"+SUBSTR(SYS(2015),4))
IF TYPE('laPrgTemps')#'U' 
  IF !EMPTY(laPrgTemps[1])
    DIMEN laPrgTemps[ALEN(laPrgTemps,1)+1,1]
  ELSE  
    DIMEN laPrgTemps[1,1]  
  ENDIF
  laPrgTemps[ALEN(laPrgTemps,1),1] = lcRetName
ENDIF
RETURN lcRetName


*!*************************************************************
*! Name      : gfOpenFile
*! Developer : MALAK - Malak Hanna
*! Date      : 04/18/1995
*! Purpose   : To open database needed by the program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name, Index Tag, 
*!                          Open Mode "EX" ----> "EXCLUSIVE"
*!                                    "SH" ----> "SHARED"
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
*! Example            : =gfOpenFile(QDD+'ORDHDR',QDD+'ORDHDR','SH')
*!*************************************************************

FUNCTION gfOpenFile

PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp

PRIVATE MODE,lcFileName,lcPath,llReturnVal,lcMsg,lcSetExact

PRIVATE lcMacroSub
lcMacroSub=""

lcFileName  = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,RAT('\',nfile)+1),NFILE)
lcOpenMode  = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
lcOrderTag  = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')


PRIVATE llOpen 
llOpen = .F.


IF TYPE('lcAliasNam') <> 'C'
  lcAliasNam = lcFilename
ENDIF

lcMsg = 'Opening '+NFILE+IIF(EMPTY(lcIndex),'', ' Index Tag '+lcOrderTag)+'....'
lcMsg = PROPER(lcMsg)
lnCurRow = ROW()
lnCurCol = COL()
IF 'SCREEN' $ SYS(101)
  DO CASE
    CASE _DOS OR _UNIX
      lcActWin = WONTOP()
      ACTIVATE SCREEN
      @ 24,00 SAY gfMsgStr(lcMsg) COLOR &gcMsgClr
      IF !EMPTY(lcActWin)
        ACTIVATE WINDOW (lcActWin) 
      ENDIF  
    CASE _WINDOWS OR _MAC
      SET MESSAGE TO lcMsg
  ENDCASE
ENDIF


llReturnVal = .T.

lcFPathSt   = SET('FULLPATH')
SET FULLPATH ON
IF USED(lcFilename)
  lcOpenMode = "SHARED"
  *-- if the file is used and it is from the same data directory
  IF DBF(lcFilename) == ALLTRIM(STRTRAN(UPPER(nFile), ".DBF") + ".DBF")
    *-- if forced open is desired
    IF llForceOp
      lcAliasNam  = gfTempName()

      lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &lcMacroSub


      llOpen = .T.
      IF !EMPTY(lcOrderTag)
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF    &&IF !EMPTY(lcOrderTag)
    ELSE
      *-- if forced open is not desired
      llReturnVal = .F.
      lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
      *-- if there is no tag is desired to set order to
      IF EMPTY(lcOrderTag)
        SET ORDER TO 0 IN (lcAliasNam)
      ELSE
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF   &&IF EMPTY(lcOrderTag)
    ENDIF     &&IF llForceOp
  ELSE
    *-- if the file is used but not from the same data directory
    lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),gfTempName(),lcAliasNam)

    lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
    &lcMacroSub

    llOpen = .T.
    IF !EMPTY(lcOrderTag)
      SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
    ENDIF  &&IF !EMPTY(lcOrderTag)
  ENDIF   &&IF DBF(lcFilename) == .......
ELSE
  *-- if the file is not used
  lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
  lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
  &lcMacroSub

  llOpen = .T.
  IF !EMPTY(lcOrderTag)
    SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
  ENDIF    &&IF !EMPTY(lcOrderTag)
ENDIF    &&IF IF USED(lcFilename)
SELECT (lcAliasNam)
SET FULLPATH &lcFPathSt


IF !UPPER(gcWorkDir) $ UPPER(NFILE)

  lcErrOn = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.
  IF TYPE('laFileName')<>'U'
    lcSetExact = SET('Exact')
    SET EXACT ON
    FOR lnFilePos = 1 TO ALEN(laFileName,1)
      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
        EXIT
      ENDIF
    ENDFOR
    IF lnFilePos > ALEN(laFileName,1)
      lnFilePos = 0
    ENDIF
    
    IF lnFilePos = 0
      IF !EMPTY(laFileName[1,1])
        DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
      *-- MAN Added ELSE Cond.  
      ELSE
        DIME laFileName[1,4] 
      ENDIF
      laFileName[ALEN(laFileName,1),1] = lcAliasNam
      laFileName[ALEN(laFileName,1),2] = lcOrderTag
      laFileName[ALEN(laFileName,1),3] = NFILE
      laFileName[ALEN(laFileName,1),4] = lcFilename
      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
        IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
          EXIT
        ENDIF
      ENDFOR
      IF lnFileElm > ALEN(gaMnu_Fl,1)
        lnFileElm = 0
      ENDIF
      IF lnFileElm > 0 
         lnRowNo = lnFileElm 
         gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
      ELSE
         DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
         =AINS(gaMnu_Fl,1)
         gaMnu_Fl[1,1] = lcAliasNam
         gaMnu_Fl[1,2] = lcOrderTag
         gaMnu_Fl[1,3] = SELECT(0)
         gaMnu_Fl[1,4] = 1
         gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
         gaMnu_Fl[1,6] = " "
      ENDIF
    ENDIF
    SET EXACT &lcSetExact
  ENDIF
  ON ERROR &lcErrOn
ENDIF  

IF 'SCREEN' $ SYS(101)
  DO CASE
    CASE _DOS OR _UNIX
      lcActWin = WONTOP()
      ACTIVATE SCREEN
      @ 24,00 SAY gfMsgStr("") COLOR &gcMsgClr
      IF !EMPTY(lcActWin)
        ACTIVATE WINDOW (lcActWin)
      ENDIF  
      @ lnCurRow,lnCurCol SAY ""
    CASE _WINDOWS OR _MAC
      SET MESSAGE TO ""
  ENDCASE
ENDIF

RETURN llReturnVal

