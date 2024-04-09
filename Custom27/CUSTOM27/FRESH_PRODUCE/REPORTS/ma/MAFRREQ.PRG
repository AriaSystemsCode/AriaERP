*!**********************************************************************************************
*: Program file  : MATIMREQ.PRG
*: Program desc. : Time Phased Material Requirement.
*: Module        : Aria Apparel Series.
*: Date          : 02/24/2002
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*!**********************************************************************************************
*! Refer to 	 : (C102515)
*!**********************************************************************************************
*!B605782,1 WAB 04/09/2002 Some errors in the Custom program.
*!**********************************************************************************************
*--Empty date not accepted.
IF EMPTY(ldStart)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Start month cannot be empty. Cannot proceed!')
  RETURN
ENDIF

lcIFabTyp = ''
lcITrmTyp = ''
lcMFabTyp = ''
lcMTrmTyp = ''
lcType    = ''
llMFIstall = (OCCURS('MF',gcCmpModules)<>0)

 
DIMENSION laSetups[5,2] 

laSetups[1,1]  = 'M_cMType1'
laSetups[2,1]  = 'M_cMType2'
laSetups[3,1]  = 'M_cMType3'
laSetups[4,1]  = 'M_cMType4'
laSetups[5,1]  = 'M_cMType5'
  
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetups,gcAct_Comp)

lcMType1   = laSetups[1,2]
lcMType2   = laSetups[2,2]
lcMType3   = laSetups[3,2]
lcMType4   = laSetups[4,2]
lcMType5   = laSetups[5,2]
lcPacks = ''
FOR lnCheck = 1 TO 5
  lcFabVar  = "lcMType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lcMFabTyp = ALLTRIM(STR(lnCheck))
  ELSE
    IF EVAL(lcFabVar) = 'T'
      lcMTrmTyp = ALLTRIM(STR(lnCheck))
    ENDIF
  ENDIF
ENDFOR

DIMENSION laSetups[5,2] 

laSetups[1,1]  = 'M_cIType1'
laSetups[2,1]  = 'M_cIType2'
laSetups[3,1]  = 'M_cIType3'
laSetups[4,1]  = 'M_cIType4'
laSetups[5,1]  = 'M_cIType5'
  
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetups,gcAct_Comp)

lcIType1   = laSetups[1,2]
lcIType2   = laSetups[2,2]
lcIType3   = laSetups[3,2]
lcIType4   = laSetups[4,2]
lcIType5   = laSetups[5,2]
lcPacks = ''

FOR lnCheck = 1 TO 5
  lcFabVar  = "lcIType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lcIFabTyp = ALLTRIM(STR(lnCheck ))
  ELSE
    IF EVAL(lcFabVar) = 'T'
      lcITrmTyp = ALLTRIM(STR(lnCheck))
    ENDIF
  ENDIF
ENDFOR

DIMENSION laFabType[4,1]
laFabType[1,1] = lcIFabTyp
laFabType[2,1] = lcITrmTyp
laFabType[3,1] = lcMFabTyp
laFabType[4,1] = lcMTrmTyp


*--Initialize needed variables.
lcMjrPct  = gfItemMask('PM')
lcStyTtl  = gfItemMask('HI')
lCnMJPct  = gfItemMask('PN')

lnMajorLn = LEN(lcMjrPct)
lnNMjrLn  = LEN(lCnMJPct)

llMScale  = gfGetMemVar('M_USEEXSSC')
DIMENSION laMonths[9]
laMonths = ""
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd,lnMonthEd
=lfGetColor()

*--Calc months end dates.
=lfGetStrt(ldStart)

*--Open needed files

DO lpOpenFile

*--Create MR temp file with structure.
IF USED(lcMatReq)
  SELECT (lcMatReq)
  ZAP
ELSE
  =lfCreateFl()
  = gfOpenFile(gcWorkDir+lcMatReq,'','EX')
  *--Open needed indeces.
  INDEX ON Item+IClr TAG Matreq ADDITIVE
ENDIF  

lcRpExp = STRTRAN(lcrpexp,'Bom.iclr','Bom.iclr,"******"')
*-Cut the Color from the filter.
lcClrExp = ""
lnClrPos = AT("INLIST(Bom.iclr",lcRpExp)
IF lnclrPos > 0
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnClrPos))
  lcClrExp  = SUBSTR(lcRpExp,lnClrPos,lnExpEnd)
ENDIF
lcClrExp = STRTRAN(lcclrExp,'Bom.iclr','lcIClr')

IF !lfCollect()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
ENDIF
SET DEVICE TO SCREEN
IF USED('cuttkth')
  USE IN cuttkth
ENDIF


RETURN

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/13/2002
*! Purpose   : Validate primary fabric and fabrics in range
*!             entered in grid.
*!*************************************************************
*! Calls     : FaBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

SET ORDER TO TAG FABRIC IN FABRIC
lcFabric = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcFabric = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric,'FABRIC')
  = FaBrow(@lcFabric,'*')
  &lcFldLocNam = IIF(!EMPTY(lcFabric),lcFabric,lcOldValue)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/13/2002
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lfCreateFl
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/13/2002
*! Purpose   : Create the report temp file.
*!*************************************************************
FUNCTION lfCreateFl

DIMENSION laFlSruc[45,4]

*--First array element [Name].
laFlSruc[1,1] = "STYLE"
laFlSruc[2,1] = "ITEM"
laFlSruc[3,1] = "ICLR"
laFlSruc[4,1] = "DESC"

laFlSruc[5,1]= "CURRHND"
laFlSruc[6,1]= "CURRORD"
laFlSruc[7,1]= "CURRREQ"
laFlSruc[8,1]= "CURRBAL"

laFlSruc[9,1 ]= "ONHND1"
laFlSruc[10,1]= "ONORD1"
laFlSruc[11,1]= "ONREQ1"
laFlSruc[12,1]= "ONBAL1"

laFlSruc[13,1]= "ONHND2"
laFlSruc[14,1]= "ONORD2"
laFlSruc[15,1]= "ONREQ2"
laFlSruc[16,1]= "ONBAL2"

laFlSruc[17,1]= "ONHND3"
laFlSruc[18,1]= "ONORD3"
laFlSruc[19,1]= "ONREQ3"
laFlSruc[20,1]= "ONBAL3"

laFlSruc[21,1]= "ONHND4"
laFlSruc[22,1]= "ONORD4"
laFlSruc[23,1]= "ONREQ4"
laFlSruc[24,1]= "ONBAL4"

laFlSruc[25,1]= "ONHND5"
laFlSruc[26,1]= "ONORD5"
laFlSruc[27,1]= "ONREQ5"
laFlSruc[28,1]= "ONBAL5"

laFlSruc[29,1]= "ONHND6"
laFlSruc[30,1]= "ONORD6"
laFlSruc[31,1]= "ONREQ6"
laFlSruc[32,1]= "ONBAL6"

laFlSruc[33,1]= "ONHND7"
laFlSruc[34,1]= "ONORD7"
laFlSruc[35,1]= "ONREQ7"
laFlSruc[36,1]= "ONBAL7"

laFlSruc[37,1]= "ONHND8"
laFlSruc[38,1]= "ONORD8"
laFlSruc[39,1]= "ONREQ8"
laFlSruc[40,1]= "ONBAL8"

laFlSruc[41,1]= "ONHND9"
laFlSruc[42,1]= "ONORD9"
laFlSruc[43,1]= "ONREQ9"
laFlSruc[44,1]= "ONBAL9"

laFlSruc[45,1]= "TOTAL"

*--Second array element [Type].
STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2]
STORE "N" TO laFlSruc[5,2] ,laFlSruc[6,2],laFlSruc[7,2],laFlSruc[8,2],;
             laFlSruc[9,2] ,laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],;
             laFlSruc[13,2],laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],;
             laFlSruc[17,2],laFlSruc[18,2],laFlSruc[19,2],laFlSruc[20,2],;
             laFlSruc[21,2],laFlSruc[22,2],laFlSruc[23,2],laFlSruc[24,2],;
             laFlSruc[25,2],laFlSruc[26,2],laFlSruc[27,2],laFlSruc[28,2],;
             laFlSruc[29,2],laFlSruc[30,2],laFlSruc[31,2],laFlSruc[32,2],;
             laFlSruc[33,2],laFlSruc[34,2],laFlSruc[35,2],laFlSruc[36,2],;
             laFlSruc[37,2],laFlSruc[38,2],laFlSruc[39,2],laFlSruc[40,2],;             
             laFlSruc[41,2],laFlSruc[42,2],laFlSruc[43,2],laFlSruc[44,2],;
             laFlSruc[45,2]

*--Third array element [Length].
STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
STORE  6  TO laFlSruc[3,3] 
STORE 20  TO laFlSruc[4,3]
STORE 12  TO laFlSruc[5,3] ,laFlSruc[6,3 ],laFlSruc[7,3],laFlSruc[8,3],;
             laFlSruc[9,3] ,laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],;
             laFlSruc[13,3],laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3],;
             laFlSruc[17,3],laFlSruc[18,3],laFlSruc[19,3],laFlSruc[20,3],;
             laFlSruc[21,3],laFlSruc[22,3],laFlSruc[23,3],laFlSruc[24,3],;
             laFlSruc[25,3],laFlSruc[26,3],laFlSruc[27,3],laFlSruc[28,3],;
             laFlSruc[29,3],laFlSruc[30,3],laFlSruc[31,3],laFlSruc[32,3],;
             laFlSruc[33,3],laFlSruc[34,3],laFlSruc[35,3],laFlSruc[36,3],;
             laFlSruc[37,3],laFlSruc[38,3],laFlSruc[39,3],laFlSruc[40,3],;
             laFlSruc[41,3],laFlSruc[42,3],laFlSruc[43,3],laFlSruc[44,3],;
             laFlSruc[45,3]

*--Forth array element [Decemal].
STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4]
STORE  3  TO laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4],laFlSruc[8,4],;
             laFlSruc[9,4] ,laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],;
             laFlSruc[13,4],laFlSruc[14,4],laFlSruc[15,4],laFlSruc[16,4],;
             laFlSruc[17,4],laFlSruc[18,4],laFlSruc[19,4],laFlSruc[20,4],;
             laFlSruc[21,4],laFlSruc[22,4],laFlSruc[23,4],laFlSruc[24,4],;
             laFlSruc[25,4],laFlSruc[26,4],laFlSruc[27,4],laFlSruc[28,4],;
             laFlSruc[29,4],laFlSruc[30,4],laFlSruc[31,4],laFlSruc[32,4],;
             laFlSruc[33,4],laFlSruc[34,4],laFlSruc[35,4],laFlSruc[36,4],;
             laFlSruc[37,4],laFlSruc[38,4],laFlSruc[39,4],laFlSruc[40,4],;
             laFlSruc[41,4],laFlSruc[42,4],laFlSruc[43,4],laFlSruc[44,4],;
             laFlSruc[45,4]

CREATE DBF (gcWorkDir+lcMatReq) FROM ARRAY laFlSruc
RETURN

*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/10/2002
*! Purpose   : Open needed files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpOpenFiles

=gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
=gfOpenFile(gcDataDir+'Matinvjl',gcDataDir+'Matinvjl','SH')
=gfOpenFile(gcDataDir+'Mmfgordh',gcDataDir+'Mmfgordd','SH')
=gfOpenFile(gcDataDir+'Mmfgordd',gcDataDir+'Mmfgordd','SH')
=gfOpenFile(gcDataDir+'pofhdr',gcDataDir+'pofhdr','SH')
=gfOpenFile(gcDataDir+'Pofln',gcDataDir+'Poflnf','SH')
=gfOpenFile(gcDataDir+'Bomline',gcDataDir+'Bomline','SH')


IF llMFIstall 
  =gfOpenFile(gcDataDir+'cuttkth',gcDataDir+'cuttkth','SH')
  =gfOpenFile(gcDataDir+'Cuttktl',gcDataDir+'CuttktlS','SH')
  SET RELATION TO CUTTKTL.cuttkt INTO CUTTKTH
ENDIF



=gfOpenFile(gcDataDir+'BOMCOST','','SH')
=gfOpenFile(gcDataDir+'Ctktbom','Ctktyp','SH')

SET ORDER TO TAG BOM IN BOM
SET ORDER TO TAG FABRIC IN FABRIC

*!*************************************************************
*! Name      : lfCollect
*:     Date  : 12/24/2001
*:  Developer: Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Collect data.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect

*--Get styles having OG Fabrics/Colors
IF !lfStyles()
  RETURN .F.
ENDIF

*--Get OnHnad for OG Fabrics/Colors.
DO lpAllQty
*SET FILTER TO TOTAL <> 0
LOCATE
IF EOF()
  RETURN .F.
ENDIF
DO gfDispRe WITH EVAL('lcRpRName')
SET FILTER TO

*!*************************************************************
*! Name      : lfStyles
*: Date      : 01/10/2002
*: Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Collect Styles.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfStyles()
*!*************************************************************
FUNCTION lfStyles

SELECT STYLE
LOCATE

SCAN
  *--Check if the style has a cost sheet.
  IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
    LOOP
  ENDIF
  *--Current style code and style scale.
  lcCStyle = STYLE.Style
  lnCnt    = IIF( SEEK("S"+STYLE.Scale,'SCALE'), SCALE.Cnt , 8 )
  WAIT WINDOW 'Collecting the cost sheet information for Style : '+lcCStyle NOWAIT
  SELECT BOM
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(lcCStyle,1,lnMajorLn) ;
         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
             cCatgTyp $ 'FTS'
    *--Check if maririal/color entered in grid is valid.
    *wab
    *IF (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt)) AND !( &lcRpExp) 
    IF !( &lcRpExp) 
      LOOP
    ENDIF
    *--Don't include non inventory trims if not setup to use it.
    IF (cCatgTyp='T' AND !Trim_Invt)
      LOOP
    ENDIF
    *--Read an item code and an item color[lcItem,lcIClr].
    STORE '' TO lcItem,lcIClr
    IF !lfReadItem()
      LOOP
    ENDIF 
    
    IF !EMPTY(lcClrExp)
      IF !(&lcClrExp )
        LOOP
      ENDIF
    ENDIF 
    
    *--Append or Update record in (lcMatReq) file. 
    =lfUpdReq()
  ENDSCAN
ENDSCAN
WAIT CLEAR
RETURN RECCOUNT(lcMatReq) > 0

*!*************************************************************
*! Name      : lfReadItem
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/10/2002
*! Purpose   : Read item.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfReadItem()
*!*************************************************************
FUNCTION lfReadItem

*--Get style component code.
IF cCatGTyp = 'S'
 *--If style size not used in cost sheet.
 IF !(STYLE.Scale $ BOM.MSIZES)
   RETURN .F.
 ENDIF
 *--Get an equevelent item non major part.
 lcCompNmj=''
 *--Non major length without scale segment.
 lnNmjPart = lnNMjrLn - IIF(llMScale,3,0)
 FOR lnI = 1 TO lnNmjPart
   lcCutChr  = SUBSTR(BOM.Item ,lnMajorLn+1+lnI,1)
   lcCompNmj = lcCompNmj + ;
      IIF(lcCutChr='*',SUBSTR(STYLE.Style, lnMajorLn+1+lnI ,1),lcCutChr)
 ENDFOR
 lcItem = SUBSTR(Item,1,lnMajorLn+1)+lcCompNmj
 *--Get an equevelent item scale part.       
 IF llMScale
   lcEqSCSz = STYLE.Scale
   FOR lnMI = 1 TO MEMLINES(BOM.MSZCROSREF)
     lcMemSLine = MLINE(BOM.MSZCROSREF,lnMI)
     IF STYLE.Scale $ lcMemSLine
       lcEqSCSz = SUBSTR(lcMemSLine,AT('~',lcMemSLine)+1,3)
       EXIT
     ENDIF 
   ENDFOR
   lcItem = lcItem + lcEqSCSz
 ENDIF
 *--Check existance of style component.
 lnRcSv=IIF(!EOF('STYLE'),RECNO('STYLE'),0)
 SELECT STYLE
 lcStyOrder=ORDER('STYLE')
 SET ORDER TO TAG Style
 llSComFund = SEEK(lcItem,'STYLE')
 IF lnRcSv<>0
   GOTO lnRcSv IN STYLE
 ENDIF
 SET ORDER TO &lcStyOrder
 SELECT BOM
 IF ! llSComFund
   RETURN .F.
 ENDIF   
 lcIClr = ''
ELSE  && CCATGTYP $ 'FT'
  lcItem = BOM.Item
  lcIClr = IIF(IClr='*',SUBSTR(lcCStyle,lnClrSrt,lnClrEnd),BOM.IClr)
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfUpdReq
*: Date      : 01/10/2002
*: Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Collect Styles.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfStyles()
*!*************************************************************
FUNCTION lfUpdReq

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcMatReq)
IF !SEEK(lcItem+lcIClr)
  APPEND BLANK  
  *REPLACE Style    WITH lcCStyle     ,;
          Item     WITH lcItem       ,;
          IClr     WITH lcIClr       ,;
          Desc     WITH BOM.Desc     
  REPLACE Item     WITH lcItem       ,;
          IClr     WITH lcIClr       ,;
          Desc     WITH BOM.Desc     
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpAllQty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/10/2002
*! Purpose   : Get All Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpAllQty

SELECT (lcMatReq)
SCAN

  ldThisDate = ldStart-DAY(ldStart)
  *--Calc Current
  =lfGetAll(.T.,ldThisDate,"CURRHND","CURRORD","CURRREQ","CURRBAL","ONHND1")
  *--Calc Next 9 months
  ldThisDate = ldThisDate+lnMonthEd
	  FOR lnFrom = 1 TO 9
    *--Get months titles for the first Item/color only as they are fixed.
    IF EMPTY(laMonths[lnFrom])
      laMonths[lnFrom]= DTOC(ldThisDate)
    ENDIF  
    =lfGetAll(.F.,ldThisDate,"ONHND"+STR(lnFrom,1),"ONORD"+STR(lnFrom,1),"ONREQ"+STR(lnFrom,1),"ONBAL"+STR(lnFrom,1),;
              IIF(lnFrom<9,"ONHND"+STR(lnFrom+1,1),""))
    =lfGetStrt(ldThisDate+1)
    ldThisDate = ldThisDate+lnMonthEd
  ENDFOR
  SELECT (lcMatReq)
ENDSCAN

*!*************************************************************
*! Name      : lfGetAll
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/14/2002
*! Purpose   : Get all Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetAll()
*!*************************************************************
FUNCTION lfGetAll
PARAMETERS llCurr,ldDate,FrstFld,ScndFld,ThrdFld,FrthFld,FithFld

*--Get the first day of the month
ldFrstDay = ldDate-DAY(ldDate-1)
*--No date enterd or it's today, Today is the start date.
IF llCurr
  *B605782,1 WAB (START)
  ldOldDate = ldFrstDay
  ldFrstDay = {01/01/1900}
  *B605782,1 WAB (End)
  IF SEEK(PADR(Item,7)+ICLR,'Fabric')
    *--Get OnOrder
    IF Fabric.Make
      lnOnOrder = 0
      SELECT MMfgOrdH
      IF SEEK(PADR(&lcMatReq..Item,7))
        *B605782,1 WAB (START) -only open MMO
        *SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
        *     Complete <= ldDate .AND. STATUS $ 'OH'
        SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
             Complete <= ldDate .AND. STATUS = 'O'
        *B605782,1 WAB (End)
          SELECT MMfgOrdd
          lnMMOrder = 0
          IF SEEK(MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
            SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR
              lnMMOrder = lnMMOrder +   ( nMFGTotQty * IIF(TranCd='1',1,-1) )
            ENDSCAN
            lnOnOrder = lnOnOrder + MAX(lnMMOrder,0)
          ENDIF
        ENDSCAN
      ENDIF
    ELSE
      SELECT POFLN
      lnOnOrder = 0
      IF SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
        SCAN REST WHILE fabric+color+cmattype+pomat+trancd = PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR 
          IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status ='O' AND POFHDR.Complete <=ldDate AND cMatType $ 'PR' 
            IF cMatType = 'P' 
              lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
            ELSE
              lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    lnRequied = lfCalReq(ldDate)
    SELECT (lcMatReq)    
    REPLACE &FrstFld WITH Fabric.OnHand,;
            &ScndFld WITH lnOnOrder;
            &ThrdFld WITH lnRequied;
            &FrthFld WITH Fabric.OnHand+lnOnOrder-lnRequied;
            &FithFld WITH Fabric.OnHand+lnOnOrder-lnRequied;
            TOTAL    WITH TOTAL + &FrstFld+&ScndFld+&ThrdFld+&FrthFld+&FithFld
  ENDIF
  *B605782,1 WAB (START)
  ldFrstDay =   ldOldDate 
  *B605782,1 WAB (End)
ELSE
  *--Get OnOrder
  =SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR,'Fabric')
  IF Fabric.Make
    lnOnOrder = 0
    SELECT MMfgOrdH
    IF SEEK(PADR(&lcMatReq..Item,7))
       *B605782,1 WAB (START) - Only Onorder Qty
       *SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
       *    BETWEEN(Complete,ldFrstDay,ldDate) .AND. STATUS $ 'OH'
       SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
           BETWEEN(Complete,ldFrstDay,ldDate) .AND. STATUS = 'O'
       *B605782,1 WAB (End)
        SELECT MMfgOrdd
        lnMMOrder = 0
        IF SEEK(MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
          SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR
            lnMMOrder = lnMMOrder +   ( nMFGTotQty * IIF(TranCd='1',1,-1) )
          ENDSCAN
          lnOnOrder = lnOnOrder + MAX(lnMMOrder,0)
        ENDIF
      ENDSCAN
    ENDIF
  ELSE
    SELECT POFLN
    lnOnOrder = 0
    IF SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
      SCAN REST WHILE fabric+color+cmattype+pomat+trancd = PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR 
        IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status ='O' AND BETWEEN(POFHDR.Complete,ldFrstDay,ldDate) AND cMatType $ 'PR' 
          IF cMatType = 'P' 
            lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
          ELSE
            lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  *--Get Required
  lnRequied = lfCalReq(ldDate)
  *--Update
  SELECT (lcMatReq)
  REPLACE &ScndFld WITH lnOnOrder;
          &ThrdFld WITH lnRequied;
          &FrthFld WITH &FrstFld+lnOnOrder-lnRequied
  IF !EMPTY(FithFld)
    REPLACE &FithFld WITH &FrstFld+lnOnOrder-lnRequied
    REPLACE TOTAL WITH TOTAL + &FithFld
  ENDIF
  REPLACE TOTAL WITH TOTAL + &ScndFld+&ThrdFld+&FrthFld
ENDIF

*!*************************************************************
*! Name      : lfCalReq
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/14/2002
*! Purpose   : Get Required Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCalReq()
*!*************************************************************
FUNCTION lfCalReq
PARAMETERS ldStrDat

PRIVATE lnAlias
lnAlias = SELECT()
lnRequied = 0

SELECT Ctktbom
IF llMFIstall
  SUM (Req_Qty-Issue_Qty) TO lnRequied FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR;
  AND SEEK(CUTTKT,'CUTTKTH') AND CutTktH.Status = 'O' AND BETWEEN(CutTktH.Complete,ldFrstDay,ldStrDat);
  AND (Req_Qty-Issue_Qty) >0
ENDIF

SELECT (lnAlias)
RETURN lnRequied

*!**********************************************************************
*! Name      : lfGetColor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/13/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfGetStrt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/10/2002
*! Purpose   : Get Start and End of the month.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetStrt()
*!*************************************************************
FUNCTION lfGetStrt

PARAMETERS ldStrDat

DO CASE
  CASE INLIST(MONTH(ldStrDat),1,3,5,7,8,10,12)
    lnMonthEd = 31
  CASE MONTH(ldStrDat) = 2
    lnMonthEd = IIF(YEAR(ldStrDat)/4-INT(YEAR(ldStrDat)/4)<>0,28,29)
  OTHERWISE
    lnMonthEd = 30
ENDCASE

FUNCTION lfGetDesc

=SEEK(PADR(Item,7)+ICLR,'Fabric')
lcDesc      = Fabric.Desc
lcColorDesc = SUBSTR(gfCodDes(IClr , 'COLOR'),1,15)
RETURN lcDesc + " "+lcColorDesc
