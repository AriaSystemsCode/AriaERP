*!*************************************************************
*! Name      : SoChCnfg
*! Developer : hend Ghanem
*! Date      : 08/01/2002
*! Purpose   : Change configuration for multible SO lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcTmpOrdLn  : Temp order line file
*!*************************************************************
*! Returns            :  None
*!*************************************************************
#INCLUDE R:\ARIA4XP\PRGS\SOCHCNFG.H

FUNCTION lfChgConfg
PARAMETERS loPrntFrmSt,lcTmpOrdLn,llFromInv,lcInvStore

DIMENSION laRpSource[1,1],laRpTarget[1,1] , laColors[1,1] , laConfgsrc[1,1] , laConfgTrg[1,1] , laConfig[1,1] ,;
		  laGroupSrc[1,1] , laGroupTrg[1,1] , laGroup[1,1] ,laStruct[1,1] , laSelData[1,1]
STORE '' TO laRpSource,laRpTarget,laColors  , laConfgsrc , laConfgTrg , laConfig , laGroupSrc , laGroupTrg , laGroup,;
			lcTStyPath,lcTPckPath,lcTStrPath
STORE  0 TO lnO_T_S , lnMajorLen , lnMajSeg , lnNonMjLen , lnSelect
STORE "" TO lcDataSess  , lcRpNConfg , lcRpColor , lcStyleTtl , lcMajorPic , lcFree_Clr , lcNonMajPi , lcRpConfig , lcRpGroup ,;
 			lcStyFile , lcStoreFile , lcPackFile , lcForExpr , lcCondExpr
lcRpStyle  = 'STYLE+Config'
lcTmpStyle = gfTempName()  && Temp Style files to be used in In range
lcTmpPack  = gfTempName()  && Temp pack files to be used in In range
lcTmpStore = gfTempName()  && Temp Store files to be used in In range
lcStyleFle = gfTempName()  && Temp STYLE files to open master STYLE file
lcSPCK_HDR = gfTempName()  && Temp SPCK_HDR files to open master SPCK_HDR file
lcStyleUPC = gfTempName()  && Temp STYLEUPC files to open master STYLEUPC file
lcCustomer = gfTempName()  && Temp CUSTOMER files to open master CUSTOMER file
lcPack_Hdr = gfTempName()  && Temp PACK_HDR files to open master PACK_HDR file
lcTmpInvL  = gfTempName()  && Temp INVLINE  files to open master INVLINE  file
lcTmpInvH  = gfTempName()  && Temp INVHDR   files to open master INVHDR   file
lcCutPick  = gfTempName()  && Temp CUTPICK  files to open master CUTPICK  file

*-- Get Item segments information
oGetItemMask = CREATEOBJECT('GetItemMask')
lcStyleTtl = oGetItemMask.Do("HI")
lnStyleLen = LEN(oGetItemMask.Do("PI"))
lcMajorPic = oGetItemMask.Do('PM')
lnMajorLen = LEN(lcMajorPic)
lnMajSeg   = oGetItemMask.Do('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
oGetItemMask.Do(@laMajSegs)
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,3],lcNonMajPi+laMajSegs[lnI-1,6]+laMajSegs[lnI,3])
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
     EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnNonMjLen = LEN(lcNonMajPi)

*-- Create Temp files
=lfCrtFile()
*-- Collect Data to in range files
=lfCollectLn()
IF EOF(lcTmpStyle)
  = gfModalGen('TRM00000B00000','ALERT',.F.,.F.,"There are no style use configuration in this order.")
  RETURN
ENDIF
*-- Call the option grid
lcExpr = gfOpGrid('SOCHCNFG',.T.,.F.,.F.,.T.,.T.)

IF lcExpr <> ".F."
  DO FORM oAriaApplication.ScreenHome+ "\SOCHCNFG.SCX"
ENDIF

*!*************************************************************
*! Name      : lfCrtFile
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Create temp file
*!*************************************************************
*! Called from : SoChCnfg.prg
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrtFile

*-- temp Style file
DIMENSION laFileStru[11,18]

laFileStru[1,1] = 'Style'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 19
laFileStru[1,4] = 0

laFileStru[2,1] = 'DESC'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 20
laFileStru[2,4] = 0

laFileStru[3,1] = 'SEASON'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 6
laFileStru[3,4] = 0

laFileStru[4,1] = 'CDIVISION'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 6
laFileStru[4,4] = 0

laFileStru[5,1] = 'PRICEA'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 12
laFileStru[5,4] = 2

laFileStru[6,1] = 'nTOTWIP'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 7
laFileStru[6,4] = 0

laFileStru[7,1] = 'nTOTSTK'
laFileStru[7,2] = 'N'
laFileStru[7,3] = 7
laFileStru[7,4] = 0

laFileStru[8,1] = 'nTOTORD'
laFileStru[8,2] = 'N'
laFileStru[8,3] = 7
laFileStru[8,4] = 0

laFileStru[9,1] = 'nOToSell'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 7
laFileStru[9,4] = 0

laFileStru[10,1] = 'FABRIC'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 7
laFileStru[10,4] = 0

laFileStru[11,1] = 'Config'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 10
laFileStru[11,4] = 0

FOR lnCount = 1 TO 11
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0  TO laFileStru[lnCount,17],  laFileStru[lnCount,18]
ENDFOR

DIMENSION laIndx[1,2]
laIndx[1,1] = "Style+Config"
laIndx[1,2] = lcTmpStyle

=gfCrtTmp(lcTmpStyle,@laFileStru,@laIndx)
lcTStyPath = DBF(lcTmpStyle)

*-- Temp Pack file
DIMENSION laFileStru[9,18]

laFileStru[1,1] = 'Pack_ID'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 16
laFileStru[1,4] = 0

laFileStru[2,1] = 'Pack_Desc'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 16
laFileStru[2,4] = 0
*-- Add trigger for GMA tu use the Full Pack key
IF ASCAN(loPrntFrmSt.laEvntTrig,PADR('ADDPKFLD',10),1,ALEN(loPrntFrmSt.laEvntTrig,1),1) > 0
 =loPrntFrmSt.mdotrigger(PADR('ADDPKFLD',10))
ENDIF

laFileStru[3,1] = 'Desc'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 20
laFileStru[3,4] = 0

laFileStru[4,1] = 'cDivision'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 6
laFileStru[4,4] = 0

laFileStru[5,1] = 'Season'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 6
laFileStru[5,4] = 0

laFileStru[6,1] = 'NPKSLPRICE'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 8
laFileStru[6,4] = 2

laFileStru[7,1] = 'CUPCNUM1'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 6
laFileStru[7,4] = 0

laFileStru[8,1] = 'CUPCNUM2'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 5
laFileStru[8,4] = 0

laFileStru[9,1] = 'CUPCNUM3'
laFileStru[9,2] = 'C'
laFileStru[9,3] = 2
laFileStru[9,4] = 0

FOR lnCount = 1 TO 9
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0  TO laFileStru[lnCount,17],  laFileStru[lnCount,18]
ENDFOR

DIMENSION laIndx[1,2]
laIndx[1,1] = "Pack_ID"
laIndx[1,2] = lcTmpPack

=gfCrtTmp(lcTmpPack,@laFileStru,@laIndx)
lcTPckPath = DBF(lcTmpPack)

*-- Temp Pack file
DIMENSION laFileStru[15,18]

laFileStru[1,1] = 'STORE'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 8
laFileStru[1,4] = 0

laFileStru[2,1] = 'stName'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 30
laFileStru[2,4] = 0

laFileStru[3,1] = 'cAddress1'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 33
laFileStru[3,4] = 0

laFileStru[4,1] = 'cAddress2'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 30
laFileStru[4,4] = 0

laFileStru[5,1] = 'cAddress3'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 30
laFileStru[5,4] = 0

laFileStru[6,1] = 'cAddress4'
laFileStru[6,2] = 'C'
laFileStru[6,3] = 30
laFileStru[6,4] = 0

laFileStru[7,1] = 'cAddress5'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 30
laFileStru[7,4] = 0

laFileStru[8,1] = 'cAddress12'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 30
laFileStru[8,4] = 0

laFileStru[9,1] = 'cAddress22'
laFileStru[9,2] = 'C'
laFileStru[9,3] = 30
laFileStru[9,4] = 0

laFileStru[10,1] = 'cAddress32'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 30
laFileStru[10,4] = 0

laFileStru[11,1] = 'cAddress42'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 30
laFileStru[11,4] = 0

laFileStru[12,1] = 'cAddress52'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 30
laFileStru[12,4] = 0

laFileStru[13,1] = 'Phone1'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 16
laFileStru[13,4] = 0

laFileStru[14,1] = 'Buyer'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 30
laFileStru[14,4] = 0

laFileStru[15,1] = 'salesrep'
laFileStru[15,2] = 'C'
laFileStru[15,3] = 3
laFileStru[15,4] = 0

FOR lnCount = 1 TO 15
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0  TO laFileStru[lnCount,17],  laFileStru[lnCount,18]
ENDFOR

DIMENSION laIndx[1,2]
laIndx[1,1] = "Store"
laIndx[1,2] = lcTmpStore

=gfCrtTmp(lcTmpStore,@laFileStru,@laIndx)
lcTStrPath = DBF(lcTmpStore)

*!*************************************************************
*! Name      : lfCollectLn
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Collect Styles and packs and colors from order line
*!*************************************************************
*! Called from : SoChCnfg.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCollectLn

SET ORDER TO (lcTmpStyle) IN (lcTmpStyle)
PRIVATE lnClrCount
STORE 0 TO lnClrCount , lnConfgCount , lnGroupCount
SELECT (lcTmpOrdLn)
SCAN
  SCATTER MEMVAR MEMO
  lcAccount = m.Account
  *-- Update temp Style file
  IF !SEEK(m.Style+m.Dyelot,lcTmpStyle)
    lnO_T_S = 0
    =lfopenFox(loPrntFrmSt,'STYLE',lcStyleFle,"Style = '"+m.Style+"'")
    IF &lcStyleFle..cDye_Flg = 'Y'
      m.Desc      = &lcStyleFle..Desc
      m.SEASON    = &lcStyleFle..SEASON
      m.CDIVISION = &lcStyleFle..CDIVISION
      m.PRICEA    = &lcStyleFle..PRICEA
      m.nTOTWIP   = lfStySum(&lcStyleFle..Style,'TotWip',1)
      m.nTOTSTK   = lfStySum(&lcStyleFle..Style,'TotStk',2)
      m.nTOTORD   = lfStySum(&lcStyleFle..Style,'TotOrd',3)
      m.nOToSell  = lnO_T_S
      m.FABRIC    = &lcStyleFle..FABRIC
      m.Config    = &lcTmpOrdLn..Dyelot
      INSERT INTO (lcTmpStyle) FROM MEMVAR
    ENDIF
  ENDIF
  *-- Update temp Store file
  IF !EMPTY(m.Store) AND !SEEK(m.Store,lcTmpStore)
    =lfopenFox(loPrntFrmSt,'CUSTOMER',lcCustomer,"type+account+store= 'S"+lcAccount+m.Store+"'")
    m.stName     = &lcCustomer..stName
    m.cAddress1  = &lcCustomer..cAddress1
    m.cAddress2  = &lcCustomer..cAddress2
    m.cAddress3  = &lcCustomer..cAddress3
    m.cAddress4  = &lcCustomer..cAddress4
    m.cAddress5  = &lcCustomer..cAddress5
    m.cAddress12 = &lcCustomer..cAddress12
    m.cAddress22 = &lcCustomer..cAddress22
    m.cAddress32 = &lcCustomer..cAddress32
    m.cAddress42 = &lcCustomer..cAddress42
    m.cAddress52 = &lcCustomer..cAddress52
    m.Phone1     = &lcCustomer..Phone1
    m.Buyer      = &lcCustomer..Buyer
    m.salesrep   = &lcCustomer..salesrep
    INSERT INTO (lcTmpStore) FROM MEMVAR
  ENDIF
  *-- Update configuration Array
  IF !EMPTY(m.Dyelot) AND ASCAN(laConfig,m.Dyelot) = 0
    lnConfgCount = lnConfgCount + 1
    DIMENSION laConfig[lnConfgCount,1]
    laConfig[lnConfgCount,1] = m.Dyelot
  ENDIF
  *-- Update Group Array
  IF !EMPTY(m.Group) AND ASCAN(laGroup,m.Group) = 0
    lnGroupCount = lnGroupCount + 1
    DIMENSION laGroup[lnGroupCount,1]
    laGroup[lnGroupCount,1] = m.Group
  ENDIF
  *-- Update temp Pack file
  lcPack_ID = m.Pack_id
  lcPackUpc = m.Pack_id
  *-- Add trigger for GMA tu use the Full Pack key
  IF ASCAN(loPrntFrmSt.laEvntTrig,PADR('PACKCODE',10),1,ALEN(loPrntFrmSt.laEvntTrig,1),1) > 0
    =loPrntFrmSt.mdotrigger(PADR('PACKCODE',10))
  ENDIF
  IF !EMPTY(lcPack_ID) AND !SEEK(lcPack_ID,lcTmpPack)
    =lfopenFox(loPrntFrmSt,'SPCK_HDR',lcSPCK_HDR,"type+account+pack_id = 'P"+lcAccount+lcPack_ID+"' OR "+;
        												   "type+account+pack_id = 'P*****"+lcPack_ID+"'")
    =lfopenFox(loPrntFrmSt,'STYLEUPC',lcStyleUPC,"account+style = '"+lcAccount+lcPackUpc+"'")
    *-- Add trigger for GMA tu use the Full Pack key
    IF ASCAN(loPrntFrmSt.laEvntTrig,PADR('OPENFXFL',10),1,ALEN(loPrntFrmSt.laEvntTrig,1),1) > 0
      =loPrntFrmSt.mdotrigger(PADR('OPENFXFL',10))
    ENDIF              												
    m.Pack_Desc  = &lcTmpOrdLn..Pack_id
    m.Pack_ID    = &lcTmpOrdLn..Pack_id
    *-- Add trigger for GMA tu use the Full Pack key
    IF ASCAN(loPrntFrmSt.laEvntTrig,PADR('PACKINFO',10),1,ALEN(loPrntFrmSt.laEvntTrig,1),1) > 0
      =loPrntFrmSt.mdotrigger(PADR('PACKINFO',10))
    ENDIF
    m.Desc       = &lcSPCK_HDR..Desc
    m.cDivision  = &lcSPCK_HDR..cDivision
    m.Season     = &lcSPCK_HDR..Season
    m.CUPCNUM1   = &lcStyleUPC..CUPCNUM1
    m.CUPCNUM2   = &lcStyleUPC..CUPCNUM2
    m.CUPCNUM3   = &lcStyleUPC..CUPCNUM3
    INSERT INTO (lcTmpPack) FROM MEMVAR
  ENDIF
  *-- Get colors
  IF ASCAN(laColors,SUBSTR(&lcTmpOrdLn..Style,lnMajorLen+2,lnNonMjLen)) = 0
    lnClrCount = lnClrCount+ 1
    DIMENSION laColors[lnClrCount,1]
    laColors[lnClrCount,1] = SUBSTR(&lcTmpOrdLn..Style,lnMajorLen+2,lnNonMjLen)
  ENDIF
ENDSCAN
SELECT (lcTmpOrdLn)
LOCATE
*!*************************************************************
*! Name      : lfSetRange
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Set data session for In range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSetStyle
PARAMETERS lcParam
DO CASE
  CASE lcParam = 'S'
    lcDataSess = SET("Datasession")
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    SELECT (lcTmpStyle)
    USE
    SET DATASESSION TO lcDataSess
    IF !USED(lcTmpStyle)
      USE (lcTStyPath) IN 0
    ENDIF
  CASE lcParam = 'R'
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    IF !USED(lcTmpStyle)
      USE (lcTStyPath) IN 0
    ENDIF
    SET DATASESSION TO lcDataSess
ENDCASE


*!*************************************************************
*! Name      : lfSetStore
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Set data session for In range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSetStore
PARAMETERS lcParam
DO CASE
  CASE lcParam = 'S'
    lcDataSess = SET("Datasession")
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    SELECT (lcTmpStore)
    USE
    SET DATASESSION TO lcDataSess
    IF !USED(lcTmpStore)
      USE (lcTStrPath) IN 0
    ENDIF
  CASE lcParam = 'R'
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    IF !USED(lcTmpStore)
      USE (lcTStrPath) IN 0
    ENDIF
    SET DATASESSION TO lcDataSess
ENDCASE

*!*************************************************************
*! Name      : lfSetPack
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Set data session for In range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSetPack
PARAMETERS lcParam
DO CASE
  CASE lcParam = 'S'
    lcDataSess = SET("Datasession")
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    SELECT (lcTmpPack)
    USE
    SET DATASESSION TO lcDataSess
    IF !USED(lcTmpPack)
      USE (lcTPckPath) IN 0
    ENDIF
  CASE lcParam = 'R'
    SET DATASESSION TO loPrntFrmSt.DataSessionId
    IF !USED(lcTmpPack)
      USE (lcTPckPath) IN 0
    ENDIF
    SET DATASESSION TO lcDataSess
ENDCASE

*!*************************************************************
*! Name      : lfStySum
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec

lnTotcomp = 0
IF RECCOUNT(lcStyleFle) != 0
  SELECT (lcStyleFle)
  SUM &lcCOMP TO lnTotcomp WHILE STYLE = ALLTRIM(lcSty)
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfopenFox
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfopenFox
LPARAMETERS loFormSet,lcTable,lcCursor,lcWhereCond
lcCurrAlis = ALIAS()
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT * FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
loSqlConnection = CREATEOBJECT('remotedataaccess')

lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'SAVE',loFormSet.DataSessionId)
IF lnConnectionHandlar = 1
  loSqlConnection = NULL
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  loSqlConnection = NULL
  RETURN .F.
ENDIF
SELECT (lcCurrAlis)
*-- end of lfopenFox.

*!*************************************************************
*! Name      : lfopenSql
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfopenSql
LPARAMETERS loFormSet,lcTable,lcCursor,lcWhereCond
lcCurrAlis = ALIAS()
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT * FROM " + lcTable + lcWhereCond
loSqlConnection = CREATEOBJECT('remotedataaccess')

lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'SAVE',loFormSet.DataSessionId)
IF lnConnectionHandlar = 1
  loSqlConnection = NULL
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  loSqlConnection = NULL
  RETURN .F.
ENDIF
SELECT (lcCurrAlis)
*-- end of lfopenFox.

*!*************************************************************
*! Name      : lfvColor
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to call mover for the Colors.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvColor

DIMENSION laRpSource[ALEN(laColors,1),1]
FOR lnClrCount = 1 TO ALEN(laColors,1)
  laRpSource[lnClrCount,1] = laColors[lnClrCount,1] + '-' + gfCodDes(laColors[lnClrCount,1],'COLOR')
ENDFOR 												
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laRpSource,@laRpTarget,LANG_ChngConfig_HeaderMoverColor,.T.,'')
= gfMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_HeaderMoverColor,loFormSet.GetHeaderText("LANG_ChngConfig_HeaderMoverColor",loFormSet.HeaderAlias)),.T.,'')
*N000682,1 11/20/2012 MMT Globlization changes[End]


*!*************************************************************
*! Name      : lfvConfig
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to call mover for the Configurations.
*!*************************************************************
*! Called from : Option Grif
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvConfig

DIMENSION laConfgSrc[ALEN(laConfig,1),1]
FOR lnCnfgCount = 1 TO ALEN(laConfig,1)
  laConfgSrc[lnCnfgCount,1] = laConfig[lnCnfgCount,1]
ENDFOR 												
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laConfgsrc,@laConfgTrg,LANG_ChngConfig_HeaderMoverConfig,.T.,'')
= gfMover(@laConfgsrc,@laConfgTrg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_HeaderMoverConfig,loFormSet.GetHeaderText("LANG_ChngConfig_HeaderMoverConfig",loFormSet.HeaderAlias)),.T.,'')
*N000682,1 11/20/2012 MMT Globlization changes[End]


*!*************************************************************
*! Name      : lfvGroup
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to call mover for the Groups.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvGroup

DIMENSION laGroupSrc[ALEN(laGroup,1),1]
FOR lnGrpCount = 1 TO ALEN(laGroup,1)
  laGroupSrc[lnGrpCount,1] = laGroup[lnGrpCount,1]
ENDFOR 												
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laGroupSrc,@laGroupTrg,LANG_ChngConfig_HeaderMoverGroup,.T.,'')
= gfMover(@laGroupSrc,@laGroupTrg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_HeaderMoverGroup,loFormSet.GetHeaderText("LANG_ChngConfig_HeaderMoverGroup",loFormSet.HeaderAlias)),.T.,'')
*N000682,1 11/20/2012 MMT Globlization changes[End]


******** Methodes of the screen
*!*************************************************************
*! Name      : lfInit
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Init Method of the screen
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.Caption = LANG_ChngConfig_FormCaption + EVALUATE(lcTmpOrdLn+'.Order')
loFormSet.AriaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_FormCaption,loFormSet.GetHeaderText("LANG_ChngConfig_FormCaption",loFormSet.HeaderAlias)) + EVALUATE(lcTmpOrdLn+'.Order')
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.lctmplines = gfTempName()
=lfCrtTmpFle(loFormSet)
=lfBundFile(loFormSet)
=lfCrtExpr(loFormSet)
=lfCollect(loFormSet)
IF EOF(loFormSet.lctmplines)
  =gfModalGen('TRM44051B00000','ALERT')
ENDIF
=lfRefGrid(loFormSet)

*!*************************************************************
*! Name      : lfCrtTmpFle
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Create temp file
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrtTmpFle
PARAMETERS loFormSet

DIMENSION laFileStru[15,18]

laFileStru[1,1] = 'Pack_ID'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 16
laFileStru[1,4] = 0

laFileStru[2,1] = 'Pack_Desc'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 16
laFileStru[2,4] = 0
*-- Add trigger for GMA tu use the Full Pack key
IF ASCAN(loFormSet.laEvntTrig,PADR('ADDPKFLD',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mdotrigger(PADR('ADDPKFLD',10))
ENDIF

laFileStru[3,1] = 'Style'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 19
laFileStru[3,4] = 0

laFileStru[4,1] = 'Config'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 10
laFileStru[4,4] = 0

laFileStru[5,1] = 'Store'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 8
laFileStru[5,4] = 0

laFileStru[6,1] = 'Complete'
laFileStru[6,2] = 'D'
laFileStru[6,3] = 8
laFileStru[6,4] = 0

laFileStru[7,1] = 'Group'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 1
laFileStru[7,4] = 0

laFileStru[8,1] = 'cReason'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 100
laFileStru[8,4] = 0

laFileStru[9,1] = 'lnSel'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 1
laFileStru[9,4] = 0

laFileStru[10,1] = 'NewConfig'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 10
laFileStru[10,4] = 0

laFileStru[11,1] = 'llReject'
laFileStru[11,2] = 'L'
laFileStru[11,3] = 1
laFileStru[11,4] = 0

laFileStru[12,1] = 'lineNo'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'Order'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 6
laFileStru[13,4] = 0

laFileStru[14,1] = 'cOrdType'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 1
laFileStru[14,4] = 0

laFileStru[15,1] = 'Dist_Ctr'
laFileStru[15,2] = 'C'
laFileStru[15,3] = 8
laFileStru[15,4] = 0

FOR lnCount = 1 TO 15
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0  TO laFileStru[lnCount,17],  laFileStru[lnCount,18]
ENDFOR

DIMENSION laIndx[1,2]
laIndx[1,1] = "Style+Config"
laIndx[1,2] = loFormSet.lctmplines

=gfCrtTmp(loFormSet.lctmplines,@laFileStru,@laIndx)

*!*************************************************************
*! Name      : lfBundFile
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Bound the temp file to the grid
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfBundFile
PARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdOrdLines
  lcFile = loFormSet.lctmplines
  .RecordSource  = lcFile
  .Sel.ControlSource       = lcFile+'.lnSel'
  .Style.ControlSource     = lcFile+'.Style'
  .Config.ControlSource    = lcFile+'.Config'
  .Store.ControlSource     = lcFile+'.Store'
  .Pack_Id.ControlSource   = lcFile+'.Pack_Desc'
  .Complete.ControlSource  = lcFile+'.Complete'
  .Group.ControlSource     = lcFile+'.Group'
  .RejReason.ControlSource = lcFile+'.cReason'
  .SETALL('READONLY',.T.)
  .Sel.ReadOnly = .F.
ENDWITH

*-- Add trigger for GMA tu use the Full Pack key
IF ASCAN(loFormSet.laEvntTrig,PADR('ADJGRID',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
 =loFormSet.mdotrigger(PADR('ADJGRID',10))
ENDIF

*!*************************************************************
*! Name      : lfCrtExpr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Create Expresssion
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrtExpr
PARAMETERS loFormSet

lcForExpr  = ".T."
IF !EMPTY(lcStyFile) AND !EOF(lcStyFile)
  SELECT (lcStyFile)
  INDEX ON KeyExp TAG (lcStyFile)
  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "SEEK(STYLE+DYELOT,lcStyFile)"
ENDIF

IF llFromInv
  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "Store = PADR(lcInvStore,8)"
ELSE
  IF !EMPTY(lcStoreFile) AND !EOF(lcStoreFile)
    SELECT (lcStoreFile)
    INDEX ON KeyExp TAG (lcStoreFile)
    lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "SEEK(Store,lcStoreFile)"
  ENDIF
ENDIF

IF !EMPTY(lcPackFile) AND !EOF(lcPackFile)
  SELECT (lcPackFile)
  INDEX ON KeyExp TAG (lcPackFile)
  lcSeek = "SEEK(Pack_id,lcPackFile)"
  *-- Add trigger for GMA tu use the Full Pack key
  IF ASCAN(loFormSet.laEvntTrig,PADR('SEEKEXPR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    =loFormSet.mdotrigger(PADR('SEEKEXPR',10))
  ENDIF

  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + lcSeek
ENDIF

IF !EMPTY(laRpTarget[1])
  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "ASCAN(laRpTarget,UPPER(SUBSTR(Style,lnMajorLen+2,lnNonMjLen))) > 0"
ENDIF

IF !EMPTY(laConfgTrg[1])
  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "ASCAN(laConfgTrg,UPPER(Dyelot)) > 0"
ENDIF

IF !EMPTY(laGroupTrg[1])
  lcForExpr = lcForExpr + IIF(!EMPTY(lcForExpr)," AND ","") + "ASCAN(laGroupTrg,Group) > 0"
ENDIF

*!*************************************************************
*! Name      : lfCollect
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Collect lines which will be updated
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCollect
PARAMETERS loFormSet

SELECT (loFormSet.lcTmpLines)
ZAP
SELECT (lcTmpOrdLn)
SCAN FOR &lcForExpr
  SCATTER MEMVAR MEMO
  =lfopenFox(loFormSet,'STYLE',lcStyleFle,"Style = '"+m.Style+"'")
  IF &lcStyleFle..cDye_Flg = 'Y'
    lcAccount   = m.Account
    m.NewConfig = lcRpNConfg
    m.Config    = &lcTmpOrdLn..Dyelot
    m.Pack_ID   = &lcTmpOrdLn..Pack_Id
    m.Pack_Desc = &lcTmpOrdLn..Pack_Id

    IF !EMPTY(m.Pack_ID)
      *-- Add trigger for GMA tu use the Full Pack key
      IF ASCAN(loFormSet.laEvntTrig,PADR('PACKINFO',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
        =loFormSet.mdotrigger(PADR('PACKINFO',10))
      ENDIF
    ENDIF
    m.lnSel = 1
    m.llReject = .F.
    m.cReason  = ""
    IF llFromInv OR loPrntFrmSt.ActiveMode <> 'A'
      IF !EMPTY(&lcTmpOrdLn..piktkt) AND lfopenFox(loFormSet,'PACK_HDR',lcPack_Hdr,"order+store+pack_no = '"+;
  	  										&lcTmpOrdLn..Order+&lcTmpOrdLn..Store+&lcTmpOrdLn..PikTkt+"'") AND !EOF(lcPack_Hdr)
        m.lnSel = 0
        m.llReject = .T.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cReason = LANG_ChngConfig_RejectReasonPL
m.cReason = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_RejectReasonPL,loFormSet.GetHeaderText("LANG_ChngConfig_RejectReasonPL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
      IF lfopensql(loFormSet,'CutPick',lcCutPick,;
      				  " WHERE CutPick.TranCd+CutPick.[Order]+STR(CutPick.cOrdLine,6) LIKE '1"+&lcTmpOrdLn..Order+STR(&lcTmpOrdLn..LineNo,6)+"%'") AND !EOF(lcCutPick)
        m.lnSel = 0
        m.llReject = .T.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cReason = LANG_ChngConfig_RejectReasonPO
m.cReason = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_RejectReasonPO,loFormSet.GetHeaderText("LANG_ChngConfig_RejectReasonPO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
      IF lfopensql(loFormSet,'CutPick',lcCutPick,;
      				  " WHERE CutPick.TranCd+CutPick.[Order]+STR(CutPick.cOrdLine,6) LIKE '2"+&lcTmpOrdLn..Order+STR(&lcTmpOrdLn..LineNo,6)+"%'") AND !EOF(lcCutPick)
        m.lnSel = 0
        m.llReject = .T.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cReason = LANG_ChngConfig_RejectReasonCT
m.cReason = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_RejectReasonCT,loFormSet.GetHeaderText("LANG_ChngConfig_RejectReasonCT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
      IF lfopenFox(loFormSet,'INVHDR',lcTmpInvH,"Order = '"+&lcTmpOrdLn..Order+"' AND Status <> 'V'") AND !EOF(lcTmpInvH)  AND;
         lfopenFox(loFormSet,'INVLINE',lcTmpInvL,"order+STR(lineno,6)+invoice = '"+;
   											&lcTmpOrdLn..Order+STR(&lcTmpOrdLn..LineNo,6)+&lcTmpInvH..Invoice+"'") AND !EOF(lcTmpInvL)
        m.lnSel = 0
        m.llReject = .T.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cReason = LANG_ChngConfig_RejectReasonShipped
m.cReason = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_RejectReasonShipped,loFormSet.GetHeaderText("LANG_ChngConfig_RejectReasonShipped",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
    ENDIF
    INSERT INTO (loFormSet.lcTmpLines) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT (loFormSet.lcTmpLines)
LOCATE

*!*************************************************************
*! Name      : lfMakeExpr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to Build the Exprission
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfMakeExpr

IF EMPTY(lcRpNConfg)
  =gfModalGen('TRM32128B00000','ALERT')
  RETURN .F.
ENDIF

lnStyPos = ASCAN(laOGFxFlt,UPPER('ORDLINE.Style'))
IF lnStyPos > 0
  lnStyPos = ASUBSCRIPT(laOGFxFlt,lnStyPos,1)
ENDIF
lcStyFile = laOGFxFlt[lnStyPos,6]

lnStorePos = ASCAN(laOGFxFlt,UPPER('ORDLINE.Store'))
IF lnStorePos > 0
  lnStorePos = ASUBSCRIPT(laOGFxFlt,lnStorePos,1)
ENDIF
lcStoreFile = laOGFxFlt[lnStorePos,6]

lnPackPos = ASCAN(laOGFxFlt,UPPER('ORDLINE.Pack_Id'))
IF lnPackPos > 0
  lnPackPos = ASUBSCRIPT(laOGFxFlt,lnPackPos,1)
ENDIF
lcPackFile = laOGFxFlt[lnPackPos,6]

*!*************************************************************
*! Name      : lfvSelect
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to validate the Check box in the grid
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvSelChk
PARAMETERS loFormSet

SELECT (loFormSet.lcTmpLines)
IF lnSel = 1 AND llReject
  lcMessage = ALLTRIM(cReason)
  =gfModalGen('INM00000B00000','ALERT',.F.,.F.,lcMessage)
  REPLACE lnSel WITH 0
ENDIF
=lfRefGrid(loFormSet)

*!*************************************************************
*! Name      : lfvSelect
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to validate selection buttons
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS loFormSet,lcParam

SELECT (loFormSet.lcTmpLines)
lnRecNo = RECNO()
DO CASE
  CASE lcParam = 'S'    && Case Select
    IF lnSel = 0 AND llReject
      lcMessage = cReason
      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,lcMessage)
    ELSE
      REPLACE lnSel WITH IIF(lnSel = 0 , 1, 0)
    ENDIF

  CASE lcParam = 'A'    && Case Select All
    REPLACE ALL lnSel WITH 1 FOR !llReject
    COUNT FOR llReject TO lnRejected
    IF lnRejected > 0
      =gfModalGen('INM32129B00000','ALERT')
    ENDIF

  CASE lcParam = 'N'    && Case Select None
    REPLACE ALL lnSel WITH 0

  CASE lcParam = 'I'    && Case Inver
    REPLACE ALL lnSel WITH IIF(lnSel = 0 , 1, 0) FOR !llReject
ENDCASE
LOCATE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
=lfRefGrid(loFormSet)

*!*************************************************************
*! Name      : lfRefGrid
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : When Function of the Grid
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRefGrid
PARAMETERS loFormSet

SELECT (loFormSet.lcTmpLines)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ARIaForm1.cmdSelect.Caption = IIF(lnSel = 1 , LANG_ChngConfig_UnSelectCaption ,LANG_ChngConfig_SelectCaption)
loFormSet.ARIaForm1.cmdSelect.Caption = IIF(lnSel = 1 , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_UnSelectCaption,loFormSet.GetHeaderText("LANG_ChngConfig_UnSelectCaption",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChngConfig_SelectCaption,loFormSet.GetHeaderText("LANG_ChngConfig_SelectCaption",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lnRecNo = RECNO()
COUNT FOR lnSel = 1 TO lnSelecled
loFormSet.ARIaForm1.cmdSelectAll.Enabled  = !(lnSelecled = RECCOUNT())
loFormSet.ARIaForm1.cmdSelectNone.Enabled = lnSelecled > 0
loFormSet.ARIaForm1.cmdReplace.Enabled    = lnSelecled > 0
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

*!*************************************************************
*! Name      : lfScope
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function for scope button
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfScope
PARAMETERS loFormSet

lcExpr = gfOpGrid('SOCHCNFG',.T.,.F.,.F.,.T.,.T.)

IF lcExpr <> ".F."
  =lfCrtExpr(loFormSet)
  =lfCollect(loFormSet)
  IF EOF(loFormSet.lctmplines)
    =gfModalGen('TRM44051B00000','ALERT')
  ENDIF
  =lfRefGrid(loFormSet)
ENDIF

*!*************************************************************
*! Name      : lfReplace
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function for Replace button
*!*************************************************************
*! Called from : SoChCnfg.scx
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfReplace
PARAMETERS loFormSet

IF llFromInv
  loInvThis = loPrntFrmSt.AriaForm1.AriaPageFrame1.Page3.InvoiceEditRegion1
ELSE
  loOrdThis = loPrntFrmSt.AriaForm1.AriaPageFrame1.Page2.AriaEditRegion1
  loPrntFrmSt.llDspCanMsg = .T.
ENDIF
llAddAll = .F.
=gfOpenFile(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
SELECT (loFormSet.lcTmpLines)
SCAN FOR lnSel = 1
  SET ORDER TO CONFIGLIN IN (lcTmpOrdLn)
  IF SEEK(Order+Store+STYLE+Config+STR(LineNo,6)+IIF(llFromInv,Dist_Ctr+'N',""),lcTmpOrdLn)
    lcStyle    = EVALUATE(loFormSet.lcTmpLines+'.Style')
    lcStore    = IIF(llFromInv,lcInvStore,EVALUATE(loFormSet.lcTmpLines+'.Store'))
    lcWareCode = EVALUATE(lcTmpOrdLn+'.cWareCode')
    lcConfig   = PADR(lcRpNConfg,10)
    IF !SEEK(PADR(lcStyle,lnStyleLen)+PADR(lcWareCode,6)+lcConfig,'STYDYE')
      IF !llAddAll
        lcStyDesc = PADR(lcStyle,lnStyleLen) + '-' + PADR(lcWareCode,6)
        lnOption = gfModalGen('TRM32130B32016','ALERT',lcStyDesc )
        DO CASE
          CASE lnOption = 1
            llAddAll = .T.
            DO gpAdStyWar WITH lcstyle , lcConfig , lcWareCode
          CASE lnOption = 2
            DO gpAdStyWar WITH lcstyle , lcConfig , lcWareCode
          CASE lnOption = 3
            LOOP
        ENDCASE
      ELSE
        DO gpAdStyWar WITH lcstyle , lcConfig , lcWareCode
      ENDIF
    ENDIF
    SELECT (lcTmpOrdLn)
    IF llFromInv
      *-- Case of Invoice sales order screen
      loInvThis.lfvstyle(lcStyle,lcConfig)
    ELSE
      *-- Case of sales order screen and temporary sales order screen
      loOrdThis.lfvstyle(lcStyle,lcStore,lcConfig)
      loPrntFrmSt.llDspCanMsg = .F.
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcTmpOrdLn)
LOCATE
IF !llFromInv
  loPrntFrmSt.llDspCanMsg = .T.
ENDIF
loFormSet.Release

