*:************************************************************************
*: Program file  : SoAlCst.PRG
*: Program desc. : Sales Order Cost Sheet For Alena
*:         System: Aria Apparel System
*:      Developer: (MHM) Mohamed Shokry
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modifications      :
*: B609056,1 HES 10/21/2009 Problem with Alena Sales Order Cost Sheets [T20091001.0001]
*:*************************************************************************
*: Due to  C#037892,1.
*:*************************************************************************
PARAMETERS lcOrderPar
EXTERNAL ARRAY laData , laKeyField , laDefProc

*----Declaration Part----*
*--lafolders       ---------------->> Array to keep Folders
*--lafoldwinds     ---------------->> Array to keep windows of Folders
*--laWndObj        ---------------->> Array to keep Objects
*--laCodes         ---------------->> Array to get codes from Code file
*--laReason        ---------------->> Array to Get Reason from code file

DECLARE lafolders[2,2],lafoldwinds[2,2],laWndObj[2,3]
DECLARE laCodes[1,10],laReason[1,2]

*--lcStyleTyp   -------------------->> Type of cost sheet I---- Imported 
*               -------------------->>                    M---- Manefactured
*               -------------------->>                    T---- Fabric
*-- we use her for only Imported
lcStyleTyp = 'I'

lcItemFile = IIF(lcStyleTyp = 'T' , 'Fabric' , 'Style')

*--lcStyle     --------------------->>  Vriable to store Style 
*-- lcStyMaj   --------------------->> Variable to store Major of the style
lcStyle = ''
lcStyMaj = ''
*--Types of Duty in Duty types screen
lntype = 1
llUpdExRat = .F.

SHOW GET pbNotePad   DISABLE
SHOW GET pbUsrFields DISABLE

*--laStySeg        ---------------->> Array to keep Style Seg.
*--laStyCSeg       ---------------->> Array to keep Styel Cost 
*--laDutyable      ---------------->> Array to keep Dutyable values
*--laOprCode       ---------------->> Array to get Operation codes from Code file
*--laMfgCode       ---------------->> Array to get Mfg codes from Code file
*--laCodInfo       ---------------->> Array to Get Codes info.
*--laCostType      ---------------->> Array to Get Cost types.

DECLARE laStySeg[1,1] , laStyCSeg[1,9] , laDutyable[1,1] , ;
        laOprCode[1,2] , laMfgCode[1] , laCodInfo[1,10] , ;
        laCostType[IIF(lcStyleTyp='T',4,5),2]
        
DECLARE laMainSetp[IIF(lcStyleTyp = 'T' , 15 , 18) ,2]
lcStyleHdr = IIF(lcStyleTyp='T' , "Fabric" , gfItemMask("HI"))

*-- Get the prompt of the available pictures on the screen.
lcSBrowBmp = gcBmpHome + "ExtKey.Bmp"
lcSNewBmp  = gcBmpHome + "New1.Bmp"
lcSRemBmp  = gcBmpHome + "Rem1.Bmp"
lcSSerBmp  = gcBmpHome + "Search.Bmp"
lcSCpyBmp  = gcBmpHome + "Copy1.Bmp"
lcSOkBmp   = gcBmpHome + "Ok.Bmp"
lcSCanBmp  = gcBmpHome + "Can.Bmp"

DECLARE laKeyField [2,4]
lcOrdType='O'
laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2

*-- Define the variables that hold the screen names.
STORE ' ' To lcCstSh0 , lcCstSh1 , lcCstSh2 , lcCstSh3 , ;
             lcCstSh4 , lcCstSh5 , lcCstSh6 , lcTmpFbric , ;
             lcTmpStyle , lcTmpBom , lcTmpCopy , lcBomTemp ,lcAllBom

STORE ' ' TO lcTmpCopy1

*-- Define the variables that hold cost labels.
FOR lnCount = 1 TO IIF(lcStyleTyp = 'T' , 4 , 5)
  lcCount  = STR(lnCount,1)
  STORE "" TO lc&lcStyleTyp.SLBL&lcCount , lc&lcStyleTyp.COST&lcCount , ;
              lc&lcStyleTyp.TYPE&lcCount
ENDFOR

*-- Define the variables used in the base screen.
STORE ''  TO lcCstSLbl , lcCostItmF , lcCostItmS , lcCstItmDD , ;
             lcCstItmDF , lcCstItmDS , lcCurrSmbl , lcCurSmbl1 , ;
             lcBsCrSmbl , lcItemDesc , lcUom , lcBrCstShT , ;
             lcRateSign , lcUnitSign , lcOprCode , lcIclr , ;
             lcDutyable , lcOldDuty , lcPricCur , lcDutyCur , ;
             lcPrimFbr , lcBasStat , lcSizStat , lcLinStat , ;
             lcObjStat , lcSrcStat

STORE 0   TO lnEstQtyA , lnEstQtyB , lnUnitQtyA , lnUnitQtyB , ;
             lnWstg , lnUnitCst , lnCstPerc , lnTotFCst , ;
             lnTotEquCst , lnNMfgCode , lnMarkB , lnMarkC , ;
             lnOldPerc,lnTotalFab

STORE 1   TO lnExRate , lnCurrUnit

llOnSize  = .F.

STORE .F. TO llBrowse , llStyDye , llTrimInv , ;
             llMfgOpr , cbOnSize , cbSelSize , llSelSize , ;
             llNewEntry , llMulCurr , llEdit, llOrdChang

STORE '*' TO lcRateSign
STORE '/' TO lcUnitSign

llFrmSelct  = .T.    && Flag to know if coming from select mode.
lnBomLnNo   = 0      && Var. hold no. of lines in the BOM Browse.
lcBrCstShT  = 'Cost Sheet Items'   && Browse title

*-- Define all the variables related to the style code structure.
STORE ''  TO lcNonMjHdr , lcItemExp , lcItemPic , lcStyPic , ;
             lcMajorPic , lcSeprator , lcStyMask , lcSizeSep
STORE 0   TO lnNonMjLen , lnItemLen , lnMajorLen , lnStyLen , ;
             lnColorStr , lnColorLen , lnSizePos , lnSizeLen
STORE .F. TO llNonMjExt , llColorExt , llExtSizSc
STORE .F. TO llChkPrim 

*--for Amend forgin price
STORE 0 TO m.UsdPrice ,m.ExchRate,m.DutyRate

*-- None of the screen's objects has been updated.
llCUpDate  = .F.

llPwInst = .T.

llImport = .F.
IF 'SP' $ gcComp_Mdl
  llOpnPdm   = FILE(gcDataDir+'PDMBOM.DBF')
  llOpnPdm   = FILE(gcDataDir+'PDMLOG.DBF')
ENDIF

STORE SPACE(10) TO lcOldMrker

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.

laDefProc[7]  = .F.     && Delete procedure(lpDelScr)
laDefProc[9]  = .F.     && Force to local Save procedure  (lpSavScr)

STORE ''  TO laCodes,laReason,lcTempSty,lcSelect,lcApproval,lcFolder,lcExpToSeek,;
             lcBrowseTl,lcAccount,lcOrderNo, lcAccName

STORE '' TO lcOrdNo,lcAcct
STORE .F. TO llOrdFlag

*--temp styles file
STORE '' TO lcOrdLine,lcOrdSty
laData[1] = ''


STORE ''  TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3
STORE 0   TO lnReason,lnApproved,lnStatus,lnAMarker,lnOMarker,lnAvailable
STORE 1   TO lnActFolder

lcTmpCdx  = gfTempName()
lcOrdLine = gfTempName()
lcOrdSty  = gfTempName()


IF !gfSetup()
  RETURN
ENDIF

*-- Array for the header data, & variable hold the header fields.
*--open needed files 

lcScFields =  'Order'

llNoShow  = .F.     && Flag to force the execution of the show procedure.

=gfOpenFile(gcDataDir+'BOMDEF','','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
=gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDBOM','SH')
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')

IF llMulCurr
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'CCURRCODE','SH')
  =gfOpenFile(gcSysHome+'SYCEXCH',gcSysHome+'CURRENCY','SH')
ENDIF

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  laMainSetp[1,1] = 'M_TINVT'
  laMainSetp[2,1] = 'M_USEEXSSC'
  laMainSetp[3,1] = 'llMulCurr'
  lnNo = 1
  FOR lnCount = 4 TO IIF(lcStyleTyp = 'T' , 15 , 18) STEP 3
    lcNo = STR(lnNo,1)
    laMainSetp[lnCount  ,1] = 'M_C'+lcStyleTyp+'SLBL'+lcNo
    laMainSetp[lnCount+1,1] = 'M_C'+lcStyleTyp+'COST'+lcNo
    laMainSetp[lnCount+2,1] = 'M_C'+lcStyleTyp+'TYPE'+lcNo
    lnNo = lnNo + 1
  ENDFOR
  =gfGetMemVar(@laMainSetp , gcAct_Comp)

  llTrimInv  = ALLTRIM(laMainSetp[1,2]) = 'N'

  llExtSizSc = laMainSetp[2,2]
  llMulCurr = (lcStyleTyp='I') .AND. laMainSetp[3,2]
  
  lnNo = 1
  FOR lnCount = 4 TO IIF(lcStyleTyp = 'T' , 15 , 18) STEP 3
    lcNo = STR(lnNo,1)
    lc&lcStyleTyp.SLBL&lcNo = laMainSetp[lnCount  ,2]
    lc&lcStyleTyp.COST&lcNo = laMainSetp[lnCount+1,2]
    lc&lcStyleTyp.TYPE&lcNo = laMainSetp[lnCount+2,2]
    laCostType[lnNo,1]      = lc&lcStyleTyp.SLBL&lcNo
    laCostType[lnNo,2]      = lnNo
    lnNo = lnNo + 1
  ENDFOR
  *-- Get the picture of the style.
  lcStyPic   = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PI"))
  
  *-- Get the major picture or material picture.
  lcItemPic  = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM"))
  *-- Get the non major picture.
  lcNMjrPic  = IIF(lcStyleTyp='T' , "XXXXXX" , gfItemMask("PN"))
  
  *-- Get the style lenght.
  lnStyLen   = LEN(lcStyPic)
  *-- Get the major length or the material legnth.
  lnItemLen  = LEN(lcItemPic)
  
  lcMajorPic = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM"))
  
  *-- Lenght of the the style major.
  lnMajorLen = LEN(lcMajorPic)
  
  lnNonMjLen = IIF(lcStyleTyp='T' , 6 , LEN(gfItemMask("PN")))
  
  *-- Get the whole style picture.
  lcStyMask  = IIF(lcStyleTyp='T' , "*******" , STRTRAN(gfItemMask("PI") , "X" , "*"))
  
  *-- Get the style mask separator.
  lcSeprator = IIF(lcStyleTyp='T' , "" , SUBSTR(lcStyleHdr , lnMajorLen+1 , 1))
  
  *-- Get the non-major header.
  lcNonMjHdr = IIF(lcStyleTyp='T' , 'Color' , gfItemMask("HN"))
  *-- Get the major mask (header).
  lcItemExp  = IIF(lcStyleTyp='T' , 'Material' , gfItemMask("HM"))
  
  *-- See if there is non-major segments or not.
  llNonMjExt = !EMPTY(lcNonMjHdr)
  
  IF lcStyleTyp = 'T'
    STORE "" TO laStyCSeg , laStyNSeg , laStySeg
  ELSE
    *-- Count of the major part.
    lnMjorCnt  = gfItemMask("SM")
    
    *-- Fill an array with the segments strucure, & loop in it to 
    *-- know if there a color segment in the style code strucure.
    =gfItemMask(@laStySeg)
    STORE "" TO laStyCSeg
    FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
      IF laStySeg[lnCnt , 1] = "C"
        *-- Flag to know if there is color in the style code strucure.
        llColorExt = .T.
        *-- Var. hold the start position of the color segment in the style code strucure.
        lnColorStr = laStySeg[lnCnt , 4]
        *-- Var. hold the color segment lenght in the style code strucure.
        lnColorLen = LEN(laStySeg[lnCnt , 3])
      ELSE
        *-- See if there is extended size scale in the style structure or not.
        IF llExtSizSc .AND. laStySeg[lnCnt , 1] = "S"
          lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
          lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(lcSizeSep) , 1 , 0)
          lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(lcSizeSep) , 1 , 0)
        ENDIF
      ENDIF
      IF !EMPTY(laStyCSeg[1,1])
        DECLARE laStyCSeg[ALEN(laStyCSeg,1)+1,9]
      ENDIF
      
      laStyCSeg[ALEN(laStyCSeg,1),1] = IIF(!EMPTY(laStySeg[lnCnt , 5]) , laStySeg[lnCnt , 5] , laStySeg[lnCnt , 2])
      
      laStyCSeg[ALEN(laStyCSeg,1),2] = laStySeg[lnCnt , 1] && Type
      laStyCSeg[ALEN(laStyCSeg,1),3] = laStySeg[lnCnt , 3] && Picture
      laStyCSeg[ALEN(laStyCSeg,1),4] = laStySeg[lnCnt , 4] && Start Position
      laStyCSeg[ALEN(laStyCSeg,1),5] = laStySeg[lnCnt , 2] && Short Desc.
      laStyCSeg[ALEN(laStyCSeg,1),6] = laStySeg[lnCnt , 6] && Separator
      laStyCSeg[ALEN(laStyCSeg,1),7] = laStySeg[lnCnt , 7] 
      laStyCSeg[ALEN(laStyCSeg,1),8] = "A"      && All Values or Selected values.
      laStyCSeg[ALEN(laStyCSeg,1),9] = lnCnt
    ENDFOR
  ENDIF
  
  *-- Adjust the non major info if there is extended size scale.
  IF llExtSizSc .AND. lcStyleTyp <> "T"
    lnNonMjLen = lnNonMjLen - lnSizeLen
    IF lnNonMjLen < 0
      lnNonMjLen = 0
      llNonMjExt = .F.
    ENDIF
    lcNMjrPic  = SUBSTR(lcNMjrPic  , 1 , (LEN(lcNMjrPic )-lnSizeLen))
    lcNonMjHdr = SUBSTR(lcNonMjHdr , 1 , lnNonMjLen)
  ENDIF
  
  *-- Array hold the codes info.
  laCodInfo[1,01] = "MFGCODE"      && Field Name
  laCodInfo[1,02] = "laMfgCode"    && Array Name
  laCodInfo[1,03] = "lnNMfgCode"   && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = lcTmpBom       && Alternative File (For default val.)
  laCodInfo[1,08] = lcTmpBom       && Use this index for the Alternative file.
  laCodInfo[1,09] = [citmmajor+typ+IIF(.NOT.(ccatgtyp$"PMD"),citmmask,mfgcode)+item+iclr]
  laCodInfo[1,10] = "MFGCODE"      && Alternative Field Name
  
  IF llMulCurr
    =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'CCURRCODE','SH')
    =gfOpenFile(gcSysHome+'SYCEXCH',gcSysHome+'CURRENCY','SH')
    =SEEK(gcBaseCurr,'SycCurr')  
    lcBsCrSmbl = SycCurr.cCurrSmbl
  ENDIF
  
  *-- Get the cost different status data in the laDutyable array from the dictionary.
  *--
  DECLARE laDutyable[4,2]
  
  laDutyable[1,1] = "N/A"
  laDutyable[2,1] = "Duty"
  laDutyable[3,1] = "Material Loss"
  laDutyable[4,1] = "Both"
  
  laDutyable[1,2] = " "
  laDutyable[2,2] = "1"
  laDutyable[3,2] = "2"
  laDutyable[4,2] = "3"


  *-- Get temp. variables for the screen names.
  lcCstSh0   = gfTempName() && "CW"+gfTempName()
  lcCstSh1   = gfTempName() && "CW"+gfTempName()
  lcCstSh2   = gfTempName() && "CW"+gfTempName()
  lcCstSh3   = gfTempName() && "CW"+gfTempName()
  lcCstSh4   = gfTempName() && "CW"+gfTempName()
  lcCstSh5   = gfTempName() && "CW"+gfTempName()
  lcCstSh6   = gfTempName() && "CW"+gfTempName()
  *-- Get temp. names for the files used in this program.
  lcTmpFbric = gfTempName()
  lcTmpStyle = gfTempName()
  lcTmpBom   = gfTempName()
  lcTmpCopy  = gfTempName()
  *--get all temp BOM in this file
  lcAllBom   = gfTempName()
  
  lcTmpCopy1 = gfTempName()
  
  lcBomTemp  = gfTempName()
  llPwInst = (OCCURS('PW',gcComp_Mdl)<>0)
  llFromCtk = .F.
  IF llPwInst .AND. lcStyleTyp = 'M'
    lcDetLin  = gfTempName()
    lcPWBom   = gfTempName()
    lcPWTol   = gfTempName()
    llNewSess = .T.
  ENDIF
  *-- Create temp. fabric file.
  SELECT FABRIC

  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcTmpFbric) FROM ARRAY laFileStru
  INDEX ON Fabric+Color TAG (lcTmpFbric)
  
  *-- Create temp. style file.
  SELECT STYLE
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcTmpStyle) FROM ARRAY laFileStru
  INDEX ON Style TAG (lcTmpStyle)
  
  *-- Create temp. BOM (bill of material file).
  SELECT BOM
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+10,4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  
  laFileStru[lnFileStru+3,1] = 'WIDTH'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 6
  laFileStru[lnFileStru+3,4] = 0


  laFileStru[lnFileStru+4,1] = 'Order'
  laFileStru[lnFileStru+4,2] = 'C'
  laFileStru[lnFileStru+4,3] = 6
  laFileStru[lnFileStru+4,4] = 0
  
  laFileStru[lnFileStru+5,1] = 'lineno'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] = 6
  laFileStru[lnFileStru+5,4] = 0
  
  laFileStru[lnFileStru+6,1] = 'Style'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 19
  laFileStru[lnFileStru+6,4] = 0

  laFileStru[lnFileStru+7,1] = 'lprimary'
  laFileStru[lnFileStru+7,2] = 'L'
  laFileStru[lnFileStru+7,3] = 1
  laFileStru[lnFileStru+7,4] = 0
  
  laFileStru[lnFileStru+8,1] = 'nPrUntQty'
  laFileStru[lnFileStru+8,2] = 'N'
  laFileStru[lnFileStru+8,3] = 12
  laFileStru[lnFileStru+8,4] = 3

  laFileStru[lnFileStru+9,1] = 'lFrstprm'
  laFileStru[lnFileStru+9,2] = 'L'
  laFileStru[lnFileStru+9,3] = 1
  laFileStru[lnFileStru+9,4] = 0

  laFileStru[lnFileStru+10,1] = 'nDutyRate'
  laFileStru[lnFileStru+10,2] = 'N'
  laFileStru[lnFileStru+10,3] = 12
  laFileStru[lnFileStru+10,4] = 2

  
  

  CREATE TABLE (gcWorkDir+lcTmpBom) FROM ARRAY laFileStru   && Temp Bom.
  INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)
  *-- Create Temp file lcAllBom to Store all BOM record
  SELECT (lcTmpBom)

  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcAllBom) FROM ARRAY laFileStru
  INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcAllBom)
  
  *-- Fill the Mfg Operation with N/A.
  = gfwCodePop(@laCodInfo , "MFGCODE" , "N")
  
  *-- Create temp. file with the same structure as the temp. bom file
  *-- plus one field to give the abillity to select and unselect cost
  *-- items to copy from another item.
  DIMENSION laFileStru[ALEN(laFileStru,1)+1,4]
  laFileStru[ALEN(laFileStru,1),1] = 'cIncl'
  laFileStru[ALEN(laFileStru,1),2] = 'C'
  laFileStru[ALEN(laFileStru,1),3] = 1
  laFileStru[ALEN(laFileStru,1),4] = 0
  CREATE TABLE (gcWorkDir+lcTmpCopy) FROM ARRAY laFileStru  && Copy Bom.
  
  INDEX ON cIncl TAG (lcTmpCopy1) OF (lcTmpCopy)
  
  lcCstSLbl  = 'Cost Item'   && Var. hold the cost label in the main screen.
  *-- create Temp for OrdBom
  *-- lcBomTemp  
  SELECT ORDBOM
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0

  
  CREATE TABLE (gcWorkDir+lcBomTemp) FROM ARRAY laFileStru
  INDEX ON Style TAG (lcBomTemp)

  SELECT ORDLINE

  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'CCOSTSHT'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  DECLARE laIndex[3,2]
  laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
  laIndex[1,2] = 'ORDLINST'
  laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
  laIndex[2,2] = 'ORDLINES'
  laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
  laIndex[3,2] = 'ORDLINE'

  =gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
  =gfCrtTmp(lcOrdSty,@laFileStru,@laIndex)
ENDIF

*-- Variables hold the different status for the available object in the 
*-- screen according to the current mode .. (select . view . edit . ..)
lcBasStat = IIF((laScrMode[3] .OR. laScrMode[4]) AND !llOnSize , "ENABLE" , "DISABLE" )
lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
lcObjStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE" )
lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )

*-- Set order descending in the exchange rate file to get the latest exchange.
IF llMulCurr .AND. lcStyleTyp = 'I'
  SELECT SycExch
  SET ORDER TO TAG Currency DESCENDING
ENDIF

*-- Set filter to the fabric or style cost items according to the program type.
SELECT BOM
SET FILTER TO IIF(lcStyleTyp='T', lMaterial, !lMaterial)

*-- set filter to purchase or manufactered styles.

SELECT (lcItemFile)
DO CASE
  CASE lcStyleTyp = 'I'
    SET FILTER TO !Style.Make
  CASE lcStyleTyp = 'M'
    SET FILTER TO Style.Make
  CASE lcStyleTyp = 'T'
    SET FILTER TO Fabric.Make
ENDCASE

lcMarkrLbl = "RGB(192,192,192,192,192,192)"
lcMarkrFld = ",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl"
lcMarkr3DG = lcMarkrLbl
lcMarkr3DW = lcMarkrLbl
STORE SPACE(0) TO lcMarker

STORE .T. TO llBrowOrd
lcAccTitl = 'Styles'  && Accounts browse title.
lcOrdTitl = 'Cost Sheet'    && Orders browse title.


lcKeyBmp   = gcBmpHome + "ExtKey.BMP"

lafolders[1,1] = 'Styles'
lafolders[1,2] = 1
lafolders[2,1] = 'Cost Sheet'
lafolders[2,2] = 2

lnnofld = 2
lcwfoldchng = '=lfActFolder()'
lnfoldercend = 102.00
lnfolderrend = 2.000
=gfOpenFile(gcSysHome+'SycInt',gcSysHome+'cContCode','SH')

SELECT ORDHDR

SET RELATION TO ORDER INTO ORDBOM
SELECT CUSTOMER
IF !WEXIST(gcBaseWind)
  
  lcTempSty = gfTempName() 
  lcSelect   = gfTempName()
  lcWinCh0   = gfTempName()
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  
  lcfolder   = gfTempName()   && Folder Window Name
  lcfoldprnt = gcBaseWind     && window parent name for the folder
  lnactfolder = 1             && active folder
  lafoldwinds[1,1] = 'Styles'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Cost Sheet'
  lafoldwinds[2,2] = lcWinCh2
  
  CREATE TABLE (gcWorkDir+lcTempSty) (Order C(5), Style C(19),lineno N(6))
  INDEX ON Style TAG (lcTempSty)
  
  laCodes[1,1]  = 'DECL_CODE'
  laCodes[1,2]  = 'laReason'
  laCodes[1,3]  = 'lnReason'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,7]  = 'ORDHDR'
  laCodes[1,8]  = 'ORDACCT'
  laCodes[1,9]  = 'ACCOUNT+CORDTYPE+ORDER'
  laCodes[1,10] = 'DECL_CODE'
ENDIF

laWndObj[1,1] = 'GWCCONTRL1'
laWndObj[1,2] = 'pbcptask'
laWndObj[1,3] = 'PBCLS'

=lfClearKey()
=lfActPad()

DO (gcScrDir+gcWinAppl+"\SOALCST.SPX")

=lfClearKey()
=gfCloseFille('SycInt')
RELEASE WINDOW (lcAccTitl)
RELEASE PAD _Option OF _MSYSMENU

IF glQuitting
  USE IN (lcTempSty)
  ERASE (gcWorkDir+lcTempSty+'.DBF')
  ERASE (gcWorkDir+lcTempSty+'.CDX')

  USE IN (lcTmpFbric)
  ERASE (gcWorkDir+lcTmpFbric+'.DBF')
  ERASE (gcWorkDir+lcTmpFbric+'.CDX')

  USE IN (lcTmpBom)
  ERASE (gcWorkDir+lcTmpBom+'.DBF')
  ERASE (gcWorkDir+lcTmpBom+'.CDX')

  USE IN (lcTmpCopy)
  ERASE (gcWorkDir+lcTmpCopy+'.DBF')
  ERASE (gcWorkDir+lcTmpCopy1+'.CDX')

  IF USED(lcOrdLine)
    USE IN (lcOrdLine)
  ENDIF
  ERASE (gcWorkDir+lcOrdLine+".DBF")
  ERASE (gcWorkDir+lcOrdLine+".CDX")
  ERASE (gcWorkDir+lcOrdLine+".FPT")

  IF USED(lcOrdSty)
    USE IN (lcOrdSty)
  ENDIF
  ERASE (gcWorkDir+lcOrdSty+".DBF")
  ERASE (gcWorkDir+lcOrdSty+".CDX")
  ERASE (gcWorkDir+lcOrdSty+".FPT")

  
  SELECT ORDHDR
  CLOSE INDEXES
  ERASE (gcWorkDir+lcTmpCdx+'.CDX')
ENDIF

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Change Folder
*!*************************************************************
*! Calls     : lfBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
DO CASE
  CASE lnActFolder=1
    SELECT (lcTmpBom)
    SET FILTER TO 

  CASE lnActFolder=2
    SELECT ORDHDR

    lcStyle = &lcOrdSty..Style
    lcStyMaj = PADR(lcStyle,lnMajorLen)
    
    WAIT WINDOW "Collecting data for Style " + lcStyMaj + " ...!" NOWAIT
    SET ORDER TO TAG ORDHDR

    =lfwOrdBrow()
    laWndObj[1,1] = lcWinCH2
    laWndObj[1,2] = 'ibOrder'
    laWndObj[1,3] = IIF(ORDHDR.STATUS='O','lnApproved','lnReason')

    WAIT CLEAR
        
    SELECT (lcTmpBom)
    LOCATE FOR &lcTmpBom..lBasOnSiz
    cbOnSize = IIF(FOUND() , .T. , .F.)
    LOCATE
    lnBomLnNo  = RECCOUNT()
    *-- Update the status varibles with the right values.
    lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    SHOW GET cbOnSize &lcBasStat

    IF EOF(lcOrdSty)
      SELECT(lcOrdSty)
      LOCATE
    ENDIF
    
    SELECT (lcTmpBom)
    SET FILTER TO (citmmajor= &lcOrdSty..Style) 
    =SEEK(lcStyMaj,'STYLE')
    lcPricCur  = Style.cPriceCur 
    lcDutyCur  = Style.cDutyCur  
    
    GOTO TOP
    IF laScrMode[3] AND EOF()
      =lfCreatBom()
    ENDIF  
    =lfBrwCstSh()
ENDCASE

*!*************************************************************
*! Name      : lfwScreen
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Main screen When function
*!*************************************************************
*! Calls     : lfChngFolder,lfwAccBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwScreen()
*!*************************************************************
FUNCTION lfwScreen


SELECT (lcOrdSty)
lcStyle = &lcOrdSty..Style
lcStyMaj = PADR(lcStyle,lnMajorLen)

BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnAMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       lcStyMaj =PADR(Style,lnMajorLen)     :H= 'Style'    :R,;
       CCOSTSHT  :H= 'Sales Order Cost Sheet'    :R;
       WINDOW (lcWinCh3)  ;
       IN WINDOW (lcWinCh1) ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfwAccBrow();
       TITLE lcAccTitl
=lfChngFolder(lnActFolder)

IF TYPE('lcOrderPar') = 'C' 
  laData[1]  = lcOrderPar
  =lfvOrder()
ENDIF
*!*************************************************************
*! Name      : lfReadAct
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : READ Activate function of main screen
*!*************************************************************
*! Calls     : lfClearKey
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=lfClearKey()

*!*************************************************************
*! Name      : lfwAccBrow
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Refresh Customer Browse
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwAccBrow()
*!*************************************************************
FUNCTION lfwAccBrow

lnAMarker = RECNO('ORDLINE')
SHOW WINDOW (lcAccTitl) REFRESH SAME

=lfRefresh(lcWinCh0)

*!*************************************************************
*! Name      : lfwOrdBrow
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Refresh Order Browse
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwOrdBrow()
*!*************************************************************
FUNCTION lfwOrdBrow

lnOMarker = RECNO('ORDHDR')

=SEEK(ALLTRIM(&lcOrdSty..Style),'STYLE')

IF OrdHdr.Status = 'O'
  SHOW GET lnStatus   ENABLE
  SHOW GET lcApproval ENABLE
  SHOW GET lnApproved ENABLE
  =gfwCodePop(@laCodes,'DECL_CODE','N')
  SHOW GET lnReason DISABLE
ENDIF
IF OrdHdr.Status = 'H'
  SHOW GET lnStatus   ENABLE
  SHOW GET lcApproval DISABLE
  SHOW GET lnApproved DISABLE
  lnReason=ASCAN(laReason,DECL_CODE)/2
  =gfwCodePop(@laCodes,'DECL_CODE','T')
  SHOW GET lnReason ENABLE
ENDIF
=lfLocateRec('O')

*!*************************************************************
*! Name      : lfOrders
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Inquire Customer Orders
*!*************************************************************
*! Calls     : gfOpenFile,ORDBROWA,gpDoProg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOrders()
*!*************************************************************
FUNCTION lfOrders
* Raises Orders Button Flag & Keeps the value of the Selected Order in variable
llOrdFlag=.T.
lcAcct=lcAccount
lcOrdNo=lcOrderNo

IF !EOF('ORDHDR')
  laData[1] = "'"+ORDHDR.cOrdType+"','"+ORDHDR.ORDER+"'"
  DO gpDoProg WITH 'AWRSOORD',.F.,'SO',laData[1]
ENDIF  

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Customer Inquire
*!*************************************************************
*! Calls     : gpDoProg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCustomer()
*!*************************************************************
FUNCTION lfvCustomer
* Raises Orders Button Flag & Keeps the value of the Selected Account in variable
llOrdFlag =.T.
lcAcct=lcAccount
lcOrdNo=lcOrderNo

lcCust = "'"+CUSTOMER.ACCOUNT+"',''"
DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcCust

*!*************************************************************
*! Name      : lpTab
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S
ON KEY LABEL ALT+A
ON KEY LABEL ALT+N
ON KEY LABEL ALT+I
ON KEY LABEL ALT+P
ON KEY LABEL ALT+V

FOR lnChrToTrap = 32 TO 126
  ON KEY LABEL (CHR(lnChrToTrap))
ENDFOR

*!*************************************************************
*! Name      : lfLocateRec
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Raises a Flag when pressing pbCustoer or pbCusOrd buttons
*!           : with true .The Order number & Account are kept in two variables 
*!           : to Seek & Select them when returning to screen & browsing any .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcBrowTyp 'O': Orders
*!                                 'A': Account
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfLocateRec('O')
*!*************************************************************
*B802389,1 Locate the selected record inn browse & make it the current one.
*!*************************************************************
FUNCTION lfLocateRec
PARAMETERS lcBrowTyp
DO CASE 
  CASE lcBrowTyp ='O'
    IF llOrdFlag 
      *--To Seek the correct order
      =SEEK(lcAcct+'O'+lcOrdNo)
      *--To Seek the correct Account
      =SEEK('M'+lcAcct,'CUSTOMER')
      llOrdFlag = .F.
    ELSE
      lcOrderNo = OrdHdr.Order
    ENDIF
  CASE lcBrowTyp ='A'
    IF llOrdFlag 
      =SEEK('M'+lcAcct)
      SHOW WINDOW (lcAccTitl) REFRESH SAME
      llOrdFlag = .F.
    ELSE
      lcAccount = Customer.Account
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Order number
*!*************************************************************
*! Calls     : lfOrdBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lnAlias

IF !llBrowse AND EMPTY(laData[1])
  RETURN
ENDIF
lnAlias = SELECT()
SELECT ORDHDR
lcOldOrd = ORDER()
SET ORDER TO TAG ORDHDR
LOCATE
DIME laTempData[1]
STORE '' TO laTempData

lcForExpr = "'O' FOR cOrdType ='O' AND !(STATUS$'XC')"

lcTitle = "Sales Order"
IF llBrowse OR !EMPTY(laData[1]) AND !SEEK("O"+laData[1]) OR (ORDHDR.STATUS$'XC')
  =ARIABROW(lcForExpr ,"Sales Order",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order,Account','laTempData')
  laData[1] = laTempData[1]
  llOrdChang= .F.
ENDIF
IF !EMPTY(laData[1]) AND SEEK('O'+laData[1],'ORDLINE')
    =SEEK('M'+ORDLINE.Account,'CUSTOMER')
    lcAccount = CUSTOMER.Account
    lcAccName = CUSTOMER.BTName  
    
    SELECT (lcOrdSty)
    DELE ALL
    SET ORDER TO ORDLINES

    SELECT (lcOrdLine)
    DELE ALL

    SELECT ORDLINE
    SCAN REST WHILE cOrdType + Order ='O'+laData[1]
      SCATT MEMVAR MEMO
      INSERT INTO (lcOrdLine) FROM MEMVAR
      
      *: B609056,1 HES 10/21/2009 Problem with Alena Sales Order Cost Sheets [Start]
*!*	      IF !SEEK(cOrdType + Order+ALLTRIM(PADR(Ordline.Style,lnMajorLen)),lcOrdSty)
*!*	        m.Style = ALLTRIM(PADR(Ordline.Style,lnMajorLen))
      IF !SEEK(cOrdType + Order+PADR(LEFT(Ordline.Style , lnMajorLen) , 19),lcOrdSty)
        m.Style = PADR(LEFT(Ordline.Style , lnMajorLen) , 19)
      *: B609056,1 HES 10/21/2009 Problem with Alena Sales Order Cost Sheets [End]  
            
        INSERT INTO (lcOrdSty) FROM MEMVAR
      ENDIF
    ENDSCAN

    SELECT (lcOrdSty)
    SET ORDER TO ORDLINE
    LOCATE
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ELSE
    laScrMode    = .F.
    laScrMode[1] = .T.
    SHOW GETS
ENDIF  

SELECT (lcOrdSty)
LOCATE
llBrowse = .F.
=lfwAccBrow()

SELECT ORDHDR
SET ORDER TO (lcOldOrd)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Show function for the whole screen.
*!*************************************************************
*! Calls     : lfBrwCstSh
*!             lfCpyCstSht
*!             gfObj_Lock
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*
FUNCTION lpShow

SELECT (lcItemFile)

DO CASE
  CASE laScrMode[1]
    STORE gcBaseCurr TO lcPricCur , lcDutyCur
    llFrmSelct    = .T.  && Flag to know if coming from select mode.
    llNewEntry    = .F.
    lcStyMaj     = SPACE(lnItemLen)
    lcItemDesc    = " "
    lcPrimFbr     = " "    && Variable hold the primary fabric.
    DECLARE laOprCode[1,2]
    laOprCode[1,1] = "Select Operation"
    laOprCode[1,2] = SPACE(6)
    STORE .F. TO cbOnSize , cbSelSize , llEdit
    lnBomLnNo     = 0      && Var. hold no. of lines in the BOM Browse.
    STORE "DISABLE" TO lcBasStat , lcObjStat , lcLinStat , lcSizStat , lcSrcStat

    lnActFolder= 1
    =lfchngfolder(lnactfolder)
    
    *-- Blank all the available temp. files.
    SELECT (lcTmpFbric)
    ZAP
    SELECT (lcTmpStyle)
    ZAP
    SELECT (lcTmpBom)
    ZAP
    SELECT (lcAllBom)
    ZAP
    SELECT (lcBomTemp)
    ZAP
    
    SELECT (lcOrdLine)
    DELETE ALL

    SELECT (lcOrdSty)
    DELETE ALL
    laData[1] = ''

    *-- Call the browse function to blank the browse.
    =lfwAccBrow()
    *-- Disable the notes & the cost buttons if select mode.
    SHOW GET pbNotes   DISABLE
    SHOW GET pbCosting DISABLE
    
  CASE laScrMode[2]
    *-- Blank all the available temp. files.
    IF !EMPTY(laData[1])
      SELECT (lcOrdLine)
      DELE ALL

      SELECT (lcOrdSty)
      SET ORDER TO ORDLINES
      DELE ALL
      
      SELECT ORDLINE
      =SEEK('O'+laData[1])
      =SEEK('M'+ORDLINE.Account,'CUSTOMER')
      lcAccount = CUSTOMER.Account
      lcAccName = CUSTOMER.BTName  
      SCAN REST WHILE cOrdType + Order ='O'+laData[1]
        SCATT MEMVAR MEMO
        INSERT INTO (lcOrdLine) FROM MEMVAR
        
        *: B609056,1 HES 10/21/2009 Problem with Alena Sales Order Cost Sheets [Start]
*!*	        IF !SEEK(cOrdType + Order+ALLTRIM(PADR(Style,lnMajorLen)),lcOrdSty)
*!*	          m.Style = ALLTRIM(PADR(Style,lnMajorLen))
        IF !SEEK(cOrdType + Order+PADR( LEFT(Style , lnMajorLen) , 19),lcOrdSty)
          m.Style = PADR( LEFT(Style , lnMajorLen) , 19)
        *: B609056,1 HES 10/21/2009 Problem with Alena Sales Order Cost Sheets [End]  
                
          INSERT INTO (lcOrdSty) FROM MEMVAR
        ENDIF
      ENDSCAN
      *SELECT (lcOrdLine)
      SELECT (lcOrdSty)
      SET ORDER TO ORDLINE
      LOCATE
      =lfwAccBrow()
    ENDIF

    lnActFolder= 1
    =lfchngfolder(lnactfolder)
    SELECT (lcTmpFbric)
    ZAP
    SELECT (lcTmpStyle)
    ZAP
    SELECT (lcTmpBom)
    ZAP
    SELECT (lcAllBom)
    ZAP
    SELECT (lcBomTemp)
    ZAP
    STORE .F. TO llOrdChang

    SELECT (lcOrdSty)
    SCAN
      IF !EOF()
        IF SEEK(order,'ORDBOM')
           SELECT ORDBOM
           SCAN REST WHILE order+STR(lineno,6)+Style = &lcOrdSty..order
             *IF PADR(&lcOrdSty..Style,lnMajorLen) <> PADR(Style,lnMajorLen)
             IF &lcOrdSty..Style <> cItmMajor
               LOOP
             ENDIF
             IF (ccatgtyp$"PMD") OR cItmMajor = &lcOrdSty..Style
               REPLACE &lcOrdSty..CCOSTSHT WITH "YES"
               *IF nExRate<>0
               llOrdChang= .T.
               EXIT
               *ENDIF  
             ENDIF
           ENDSCAN
           IF !llOrdChang
             REPLACE &lcOrdSty..CCOSTSHT WITH "NO"
           ENDIF  
         ELSE
           REPLACE &lcOrdSty..CCOSTSHT WITH "NO"
        ENDIF
      ENDIF
    ENDSCAN

    SELECT (lcOrdSty)
    IF !llOrdChang
      SHOW GET pbDlt DISABLE
      laCtrStat[8]  = "DISABLE"    && Delete button
    ELSE
      SHOW GET pbDlt ENABLE
      laCtrStat[8]  = "ENABLE"    && Delete button
    ENDIF

    =lfGtCstSht()
    =lfGetBomrc()
    *-- initialize variable for Update Ex rate
    llUpdExRat = .F.    
    SELECT (lcTmpBom)
    lnBomLnNo  = RECCOUNT()

    IF  lnBomLnNo>0
      llOrdChang= .T.
    ENDIF
        
    lcPrimFbr = " "    && Variable hold the primary fabric.
    *-- Disable all the objects in the main screen.
    STORE "DISABLE" TO lcBasStat , lcObjStat , lcLinStat , lcSizStat
    STORE .F. TO llNewEntry , llEdit
    *-- Fill the description.
    lcItemDesc = IIF(lcStyleTyp = 'T' , &lcItemFile..Desc , &lcItemFile..Desc1)
    *-- See if the item with dyelots yes or no.
    llStyDye   = (&lcItemFile..cDye_Flg ='Y')
    *-- Get the duty & mfg operation currency for the current style.
    *lcPricCur  = IIF(lcStyleTyp = "I" , Style.cPriceCur , gcBaseCurr)
    *lcDutyCur  = IIF(lcStyleTyp = "I" , Style.cDutyCur  , gcBaseCurr)
    
    *-- Get the cost items for the current item.
    STORE 0 TO lnBomLnNo
    WAIT 'Preparing the work area ......' WINDOW NOWAIT
    
    *-- If there is lines for the current cost sheet, get the available 
    *-- Mfg operations from the lines.
    IF lnBomLnNo > 0
      DECLARE laOprCode[1,2]
      laOprCode = ""
        
       SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
       WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
             cdefcode+ccode_no+crltfield+cfld_name="N"+MfgCode+"N"+"MFGCODE" AND;
             !EMPTY(Codes.cDiscrep) ;
        FROM (lcTmpBom) , Codes ;
        INTO ARRAY laOprCode 

      IF !EMPTY(laOprCode[1,1])
        DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
        =AINS(laOprCode,1)
      ENDIF
      laOprCode[1,1] = "Select Operation"
      laOprCode[1,2] = SPACE(6)
      
      *-- See if there is at least 1 record has sizes to be based on sizes.
      LOCATE FOR &lcTmpBom..lBasOnSiz
      cbOnSize = IIF(FOUND() , .T. , .F.)
      GO TOP
      *-- Enable the notes & the costing buttons if there is lines in the cost sheet.
      SHOW GET pbNotes   ENABLE
      SHOW GET pbCosting ENABLE
      lcSrcStat = "ENABLE"
      
      SET SKIP OF PAD _OPTION OF _MSYSMENU (.F.)
    ELSE
      laOprCode[1,1] = "Select Operation"
      laOprCode[1,2] = SPACE(6)
      cbOnSize   = .F.
      *-- Disable the notes & the costing buttons if there is no lines in the cost sheet.
      SHOW GET pbNotes   DISABLE
      SHOW GET pbCosting DISABLE
      lcSrcStat = "DISABLE"
    ENDIF
    SHOW GET pbSearch &lcSrcStat
    WAIT CLEAR

    =lfwAccBrow()
    
     *-- we move this part of code to lfcostdata function 
  CASE laScrMode[3]

    IF ORDHDR.STATUS$'XC'
      lcCurStat = IIF( ORDHDR.STATUS='X','Canceld','Complete')
      lcMsg2 = 'Order Status is ' + lcCurStat + ' Can not edit'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    
      SELECT ORDHDR
      =gfObj_lock(.F.)
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      SHOW GETS
      RETURN
    ENDIF
    lnActFolder= 1
    =lfchngfolder(lnactfolder)

    SHOW GET cbPrim DISABLE
    SHOW GET lnTotalFab DISABLE
      
    SELECT (lcOrdSty)
    LOCATE
      
    SELECT STYLE

    =SEEK(&lcOrdSty..Style,'STYLE')
    IF !Style.lDetCost
      *** The current style has not been setup to use detailed ***
      *** costing.  Cannot proceed. ***
      *** <  Ok  > ***
      =gfModalGen("INM38148B00000" , "DIALOG")
      =gfObj_lock(.F.)
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
      RETURN
    ENDIF
    *-- None of the screen's objects has been updated.
    llCUpDate = .T.
    lcObjStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE" )
    IF lnBomLnNo  = 0 .AND. !llEdit
      llEdit      = .T.
      llNewEntry  = .T.
      
      IF lnBomLnNo > 0
        SELECT (lcTmpBom)
        LOCATE FOR &lcTmpBom..lBasOnSiz
        cbOnSize  = IIF(FOUND() , .T. , .F.)
        
        LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
        llOnSize  = IIF(FOUND() , .T. , .F. )
        lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
        GO TOP
      ELSE
        cbOnSize  = .F.
        lcBasStat = "ENABLE"
      ENDIF
    ELSE
      llEdit      = .T.
      llNewEntry  = .F.
      SELECT (lcTmpBom)
      LOCATE FOR &lcTmpBom..lBasOnSiz
      cbOnSize  = IIF(FOUND() , .T. , .F.)

      LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
      llOnSize  = IIF(FOUND() , .T. , .F. )
      lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
      
      GO TOP IN (lcTmpBom)
    ENDIF
    IF lnBomLnNo > 0
      *-- Enable the costing button in the edit or add mode.
      SHOW GET pbCosting ENABLE
      SHOW GET pbNotes   ENABLE
    ELSE
      *-- Disable the costing button in the edit or add mode.
      SHOW GET pbCosting DISABLE
      SHOW GET pbNotes   DISABLE
    ENDIF

    *-- Update the status varibles with the right values.
    lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
    
    *-- Loop in the mfg operations array to delete the rows that was not
    *-- set in the code file as parent operation.
    IF lnBomLnNo > 0 .AND. !EMPTY(laOprCode[1,1])
      DECLARE laOprRltFd[1,2]
      laOprRltFd[1,1] = 'LMFGOPR'
      laOprRltFd[1,2] = 'llMfgOpr'
      lnOprNo   = ALEN(laOprCode,1)
      lnHidnCnt = 2
      FOR lnCnt = 2 TO lnOprNo
        IF ALEN(laOprCode,1) >= lnHidnCnt
          =gfRltFld(laOprCode[lnHidnCnt,2] , @laOprRltFd , "MFGCODE")
          IF !llMfgOpr
            =ADEL(laOprCode , lnHidnCnt)
            IF ALEN(laOprCode,1) > 1
              DECLARE laOprCode[ALEN(laOprCode,1)-1 , 2]
            ELSE
              DECLARE laOprCode[1 , 2]
            ENDIF
          ELSE
            lnHidnCnt = lnCnt
          ENDIF
        ENDIF
      ENDFOR
    ELSE
      SHOW GET puOprCode &lcLinStat
    ENDIF
    
    *-- Function to refresh the line info, called from :_
    *-- (show procedure - browse when function)
    =lfRefLin()
    IF llOrdChang
      llOrdChang=.F.
      lcMsg2 = 'Update Exchange Rates'
      lnMsgNo=gfModalGen("TRM00000B32014","DIALOG",.F.,.F.,lcMsg2)
      
      IF lnMsgNo =2
        =lfUpdExRat()
        llUpdExRat = .T.
      ELSE
      llUpdExRat = .F.  
      ENDIF
    ENDIF  
ENDCASE
SHOW GET pbNotePad DISABLE
SHOW GET pbUsrFields DISABLE

SHOW GET cbOnSize   &lcBasStat
SHOW GET lcItemDesc &lcObjStat
SHOW GET pbNewEle   &lcObjStat
SHOW GET pbAudTrail DISABLE

SELECT (lcItemFile)


*!*************************************************************
*! Name      : lfBrwCstSh
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Browse Style/Material Cost Sheet.
*!*************************************************************
*! Calls     : lfShowDet, lfvBrows
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrwCstSh()
*!*************************************************************
*
FUNCTION lfBrwCstSh

SELECT (lcTmpBom)
lnMarkB = RECNO()

=SEEK(PADR(&lcOrdSty..Style,lnMajorLen),'STYLE')

lcCurSeek = IIF("*" $ &lcTmpBom..IClr , PADR(&lcTmpBom..Item , 7) , PADR(&lcTmpBom..Item , 7)+&lcTmpBom..IClr)
=SEEK(lcCurSeek , "FABRIC")

lcItemDesc = Style.Desc1
SHOW GET lcItemDesc DISABLE
lcStyle    = &lcItemFile..Style
lcStyMaj = PADR(lcStyle,lnMajorLen)

IF llNonMjExt .OR. lcStyleTyp = "T"
  *-- If there is non-major exist.

  IF 'SP' $ gcComp_Mdl             
    lcCstShFld = "cMarkr = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
                 "lcMask  = IIF(lcStyleTyp='T',cItmMask,SUBSTR(cItmMask,lnMajorLen+LEN(ALLTRIM(lcSeprator))+1)) :H= lcNonMjHdr :R:18 ,"+;
                 "lcDesc  = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
                 "Item       :H= 'Item'     :R:20 ,"+;
                 "IClr       :H= 'I. Clr'   :R:9  ,"+;
                 "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
                 "UOM        :H= 'UOM' :R:6 ,"+;
                 "nBomTotQty :H= 'Unt Qty'  :P='999.999' :R:7 ,"+;
                 "UntCost    :H= 'Unt Cst'  :P='999999999.999' :R:13 ,"+;
                 "TotCost    :H= 'Tot. Cst' :P='9999999999.99':R:13,"+;
                 "cMarker    :H= 'Marker'   :R:13"
  ELSE
      lcCstShFld = "cMarkr = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
                 "lcMask  = IIF(lcStyleTyp='T',cItmMask,SUBSTR(cItmMask,lnMajorLen+LEN(ALLTRIM(lcSeprator))+1)) :H= lcNonMjHdr :R:18 ,"+;
                 "lcDesc  = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
                 "Item       :H= 'Item'     :R:20 ,"+;
                 "IClr       :H= 'I. Clr'   :R:9  ,"+;
                 "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
                 "UOM        :H= 'UOM' :R:6 ,"+;
                 "nBomTotQty :H= 'Unt Qty'  :P='999.999' :R:7 ,"+;
                 "UntCost    :H= 'Unt Cst'  :P='999999999.999' :R:13 ,"+;
                 "TotCost    :H= 'Tot. Cst' :P='9999999999.99':R:13"
  ENDIF
ELSE
  *-- If there is no non-major exist in the style.
  lcCstShFld = "cMarkr = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
             "lcDesc    = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
             "Item       :H= 'Item'   :R:20  ,"+;
             "IClr       :H= 'I. Clr' :R:9   ,"+;
             "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
             "Desc       :H= 'Description' :R:16 ,"+;
             "UOM        :H= 'UOM' :R:6 ,"+;
             "nBomTotQty :H= 'Unt Qty' :P='999.999' :R:7 ,"+;
             "UntCost :H= 'Unt Cst' :P='999999999.999':R:13 ,"+;
             "TotCost :H= 'Tot. Cst':P='9999999999.99':R:13,"+;
             IIF('SP' $ gcComp_Mdl,"cMarker    :H= 'Marker'   :R:13","")
  
ENDIF

BROWSE FIELDS &lcCstShFld ;
       WHEN lfShowDet()   ;
       VALID :F lfvBrows();
       NOEDIT           ;
       LOCK 0           ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WINDOW (lcCstSh1)  ;
       IN WINDOW (lcWinCh2) ;
       TITLE lcBrCstShT

=lfShowDet()

SELECT (lcItemFile)

*!*************************************************************
*! Name      : lfShowDet
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Show Style/Material Cost Sheet line details
*!*************************************************************
*! Calls     : lfRefresh, gfwCodePop, gfRltFld, gfGetExSin
*!           : lfRefLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShowDet()
*!*************************************************************
*
FUNCTION lfShowDet
PRIVATE lcCurrency

lnRecNo = RECNO()
lnMarkB    = RECNO(lcTmpBom)
glFromBrow = .T.

lcCstSLbl  = IIF(EMPTY(&lcTmpBom..Typ) .OR. lnBomLnNo = 0 , 'Cost Item' , EVALUATE('lc'+lcStyleTyp+'SLbl'+&lcTmpBom..Typ))

lnTotalFab  = &lcTmpBom..nPrUntQty
IF &lcTmpBom..lprimary
  cbPrim = 1
ELSE
  cbPrim = 0
ENDIF  
SHOW GET cbPrim DISABLE
SHOW GET lnTotalFab DISABLE

*-- Variables hold the different status for the available object in the 
*-- screen according to the current mode .. (select . view . edit . ..)
lcBasStat = IIF((laScrMode[3] .OR. laScrMode[4]) AND !llOnSize , "ENABLE" , "DISABLE" )
lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
lcObjStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE" )
lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )

DO CASE
  *-- If the cost item is Mfg operation.
  CASE &lcTmpBom..cCatgTyp = "M" .AND. lnBomLnNo <> 0
    STORE "" TO lcCostItmF , lcCostItmS , lcIclr
    =gfwCodePop(@laCodInfo, "MFGCODE", "L")
    lnNMfgCode  = IIF(ASCAN('laMfgCode',&lcTmpBom..MfgCode) > 0 , ;
                      ASUBSCRIPT('laMfgCode' , ASCAN('laMfgCode',&lcTmpBom..MfgCode) , 1) , 1)
  *-- If the cost item is purchase price or duty.
  CASE &lcTmpBom..cCatgTyp $ "PD" .AND. lnBomLnNo <> 0
    STORE "" TO lcCostItmF , lcCostItmS , lcIclr
    lcCstItmDD  = &lcTmpBom..DESC
  *-- If the cost item is fabric or trim.
  CASE &lcTmpBom..cCatgTyp $ "FT" .AND. lnBomLnNo <> 0
    lcCostItmF  = &lcTmpBom..Item
    lcCostItmS  = ""
    lcIclr      = &lcTmpBom..IClr
    lcCstItmDF  = &lcTmpBom..DESC
    =SEEK(ALLTRIM(lcCostItmF),'FABRIC')
    IF &lcTmpBom..lprimary
      cbPrim = 1
    ELSE
      cbPrim = 0
    ENDIF
    
    *--To control Showing Primary Fabric
    IF laScrMode[3]
      IF &lcTmpBom..cCatgTyp = "F"
        SHOW GET cbPrim ENABLE
      ELSE
        SHOW GET cbPrim DISABLE
      ENDIF
      
      IF &lcTmpBom..lFrstprm 
        SHOW GET lnTotalFab ENABLE
      ELSE  
        SHOW GET lnTotalFab DISABLE
      ENDIF
    ELSE
      SHOW GET cbPrim DISABLE
      SHOW GET lnTotalFab DISABLE
    ENDIF
    
  *-- If the cost item is style.
  CASE &lcTmpBom..cCatgTyp = "S" .AND. lnBomLnNo <> 0
    lcCostItmS  = &lcTmpBom..Item
    lcCostItmF  = ""
    lcIclr      = ""
    lcCstItmDS  = &lcTmpBom..DESC
  *-- Otherwise, blank all objects.
  OTHERWISE
    STORE "" TO lcCostItmF , lcCostItmS , lcCstItmDD , lcCstItmDS , ;
                lcCostItmS , lcCstItmDF , lcIclr
    lnNMfgCode  = 1
ENDCASE

IF laScrMode[1]
  SHOW GET pbCosting DISABLE
ELSE
  IF lnBomLnNo = 0
    SHOW GET pbCosting DISABLE
  ELSE
    SHOW GET pbCosting ENABLE
  ENDIF
ENDIF

*-- Define all the needed objects with the right values according to the current record.
lcUom       = IIF(EMPTY(&lcTmpBom..UOM) .OR. lnBomLnNo = 0 , "" , &lcTmpBom..UOM)
lnEstQtyA   = IIF(EMPTY(&lcTmpBom..nEstBomQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nEstBomQty)
lnEstQtyB   = IIF(EMPTY(&lcTmpBom..nEstBomQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nEstBomQty)
lnUnitQtyA  = IIF(EMPTY(&lcTmpBom..nBomTotQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomTotQty)
lnUnitQtyB  = IIF(EMPTY(&lcTmpBom..nBomTotQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomTotQty)
lnWstg      = IIF(EMPTY(&lcTmpBom..nBomWastge) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomWastge)

IF lcStyleTyp = "I" .AND. llMulCurr .AND. !(&lcTmpBom..cCurrCode = gcBaseCurr) AND (&lcTmpBom..cCatgTyp $ "DM")
  llFound    = SEEK(gcBaseCurr + &lcTmpBom..cCurrCode , "SYCEXCH")
  lnPExRate  = IIF(llFound , SYCEXCH.nExRate , 1)
  llFound    = SEEK(&lcTmpBom..cCurrCode , "SYCCURR")
  lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
  lcPRatSign = "*"
  lcPUntSign = "/"
  lcPRatSign = gfGetExSin(@lcPUntSign , &lcTmpBom..cCurrCode)
  lnPExRate = IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
  lcPRatSign = IIF(lcPRatSign = "/" , "*" , "/")
  lcPUntSign = IIF(lcPUntSign = "/" , "*" , "/")
  lnUnitCst  = IIF(EMPTY(&lcTmpBom..UntCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..UntCost)
  lnTotFCst  = IIF(EMPTY(&lcTmpBom..TotCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..TotCost)
  lnUnitCst  = lnUnitCst &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
  lnTotFCst  = lnTotFCst &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
ELSE
  lnUnitCst   = IIF(EMPTY(&lcTmpBom..UntCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..UntCost)
  lnTotFCst   = IIF(EMPTY(&lcTmpBom..TotCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..TotCost)
ENDIF

*---
lnCstPerc   = IIF(EMPTY(&lcTmpBom..nPercent) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nPercent)
lcOprCode   = IIF(EMPTY(&lcTmpBom..cOprCode) .OR. lnBomLnNo = 0 , "" , &lcTmpBom..cOprCode)
puOprCode   = IIF(EMPTY(lcOprCode) .OR. lnBomLnNo = 0 , 1 , IIF(ASCAN('laOprCode',lcOprCode) > 0 , ;
                 ASUBSCRIPT('laOprCode' , ASCAN('laOprCode',lcOprCode) , 1) , 1))
cbSelSize   = IIF(EMPTY(ALLTRIM(&lcTmpBom..mSizes)) .OR. lnBomLnNo = 0 , .F. , .T.)
lcDutyable  = IIF(EMPTY(&lcTmpBom..cCostStat) .OR. lnBomLnNo = 0 , " " , &lcTmpBom..cCostStat)
ibDutyabl   = IIF(EMPTY(&lcTmpBom..cCostStat) .OR. lnBomLnNo = 0 , 1 , VAL(&lcTmpBom..cCostStat)+1)
lcMarker    = &lcTmpBom..cMarker

IF llMulCurr
  *-- Get purchase price currency, duty currency or base currency
  lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' , Style.cPriceCur , ;
                   IIF(&lcTmpBom..cCatgTyp $ 'MD' , Style.cDutyCur , gcBaseCurr)))
  lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  *-- Get currency unit and exchange rate. 
  llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')

  IF !llUpdExRat 
    lnExRate = IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
  ELSE 
    lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  ENDIF
  *-- Get currency symbol.
  llFound    = SEEK(lcCurrency , 'SycCurr')
  lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
  STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
  lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
  IF (&lcTmpBom..cCatgTyp = "P")
    lnTotEquCst = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,3))
  ELSE
    lnTotEquCst = IIF(lnBomLnNo = 0 , 0 , &lcTmpBom..TotCost )
  ENDIF
  *-- Control Showing Objects
  IF  &lcTmpBom..cCatgTyp $ "FT" .AND. lnBomLnNo <> 0
    IF SEEK(ALLTRIM(lcCostItmF),'FABRIC') AND (Fabric.cpricecur <>gcBaseCurr )
      =lfFrnFab()
    ENDIF  
  ENDIF  

ENDIF


*-- See if theis Mfg Operation have flag considered as operation yes .or. no.
IF &lcTmpBom..cCatgTyp = 'M'
  DECLARE laMfgRltFd[1,2]
  laMfgRltFd[1,1] = 'LMFGOPR'
  laMfgRltFd[1,2] = 'llMfgOpr'
  =gfRltFld(&lcTmpBom..MfgCode , @laMfgRltFd , "MFGCODE")
  
  IF llMfgOpr
    SHOW GET puOprCode DISABLE
  ELSE
    SHOW GET puOprCode &lcLinStat
  ENDIF
ELSE
  SHOW GET puOprCode &lcLinStat
ENDIF

lcMarkrLbl = IIF(&lcTmpBom..cCatgTyp='F',"RGB(0,0,,,,,)","RGB(192,192,192,192,192,192)")
lcMarkrFld = IIF(&lcTmpBom..cCatgTyp='F',gcObjColor,",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl")
lcMarkr3DG = IIF(&lcTmpBom..cCatgTyp='F',"RGB(128,128,128,128,128,128)",lcMarkrLbl)
lcMarkr3DW = IIF(&lcTmpBom..cCatgTyp='F',"RGB(255,255,255,192,192,192)",lcMarkrLbl)

=lfRefresh()
SHOW GET pbNewEle   &lcObjStat

lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )

*-- Function to refresh the line info, called from :_
*-- (show procedure - browse when function)
=lfRefLin()

*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH TOP

*!*************************************************************
*! Name      : lfRefLin
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : function to refresh a line info.
*!*************************************************************
*! Called from : lpShow() - lfShowDet()
*!*************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfRefLin()
*!*************************************************************
*
FUNCTION lfRefLin

*-- Refresh the objects in the main screen.
SHOW GET cbSelSize   &lcSizStat
SHOW GET pbRemEle    &lcLinStat
SHOW GET pbSearch    &lcSrcStat
SHOW GET lcCostItmF  &lcLinStat
SHOW GET lcCostItmS  &lcLinStat
SHOW GET lcIClr      &lcLinStat
SHOW GET lnTotEquCst &lcLinStat

DO CASE
  *-- If the cost item is purchase price.
  CASE &lcTmpBom..cCatgTyp = 'P'
    SHOW WINDOW (lcCstSh6) TOP
    SHOW GET lcCstItmDD &lcLinStat
    SHOW GETS WINDOW (lcCstSh3) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  DISABLE
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA DISABLE
    SHOW GET lnWstg     DISABLE
    SHOW GET lnUnitCst  &lcLinStat
    SHOW GET lcUom      DISABLE
  *-- If the cost item is duty.
  CASE &lcTmpBom..cCatgTyp = 'D'
    SHOW WINDOW (lcCstSh6) TOP
    SHOW GET lcCstItmDD &lcLinStat
    SHOW GETS WINDOW (lcCstSh3) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  DISABLE
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA DISABLE
    SHOW GET lnWstg     DISABLE
    SHOW GET lnUnitCst  &lcLinStat
    SHOW GET lcUom      &lcLinStat
  *-- If the cost item is Mfg Operation.
  CASE &lcTmpBom..cCatgTyp = 'M'
    SHOW WINDOW (lcCstSh5) TOP
    SHOW GETS WINDOW (lcCstSh3) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    SHOW GET lnNMfgCode &lcLinStat

    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat

    SHOW GET lnUnitCst  &lcLinStat
    SHOW GET lcUom      &lcLinStat

  *-- If the cost item is Style.
  CASE &lcTmpBom..cCatgTyp = 'S'
    SHOW WINDOW (lcCstSh4) TOP
    SHOW GETS WINDOW (lcCstSh3) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    SHOW GET lcCstItmDS &lcLinStat
    SHOW GET lnEstQtyA  DISABLE
    SHOW GET lnEstQtyB  &lcLinStat
    SHOW GET lnUnitQtyA DISABLE
    SHOW GET lnUnitQtyB &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    SHOW GET lnUnitCst  DISABLE
    SHOW GET lcUom      &lcLinStat
  *-- If the cost item is Fabric.
  CASE &lcTmpBom..cCatgTyp = 'F'
    SHOW WINDOW (lcCstSh3) TOP
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    

    SHOW GET lcCostItmF &lcLinStat
    SHOW GET lcIClr     &lcLinStat
    SHOW GET ibFabric   &lcLinStat
    SHOW GET ibColor    &lcLinStat
    
    SHOW GET lcCstItmDF &lcLinStat
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    SHOW GET lnUnitCst  DISABLE
    IF Fabric.cPriceCur = gcbasecurr AND laScrMode[3]
      SHOW GET lnUnitCst  ENABLE
    ENDIF
    SHOW GET lcUom      DISABLE
  *-- If the cost item is trim.
  CASE &lcTmpBom..cCatgTyp = 'T'
    SHOW WINDOW (lcCstSh3) TOP
    SHOW GET lcCstItmDF &lcLinStat
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    

    SHOW GET lcCostItmF &lcLinStat
    SHOW GET lcIClr     &lcLinStat
    SHOW GET ibFabric   &lcLinStat
    SHOW GET ibColor    &lcLinStat

    
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    IF &lcTmpBom..trim_invt
      SHOW GET lnUnitCst  DISABLE
      SHOW GET lcUom      DISABLE
    ELSE
      SHOW GET lnUnitCst  &lcLinStat
      SHOW GET lcUom      &lcLinStat
    ENDIF
    IF Fabric.cPriceCur = gcbasecurr AND laScrMode[3]
      SHOW GET lnUnitCst  ENABLE
    ENDIF

  *-- Otherwise , blank all objects & disable them.
  OTHERWISE
    SHOW WINDOW (lcCstSh3) TOP
    SHOW GETS WINDOW (lcCstSh3) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    SHOW GET lcCstItmDF DISABLE
    SHOW GET lnEstQtyA  DISABLE
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnUnitQtyA DISABLE
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnWstg     DISABLE
    SHOW GET lnUnitCst  DISABLE
    SHOW GET lcUom      DISABLE
ENDCASE

SHOW GET lnTotFCst   &lcLinStat
SHOW GET lcCurrSmbl  &lcLinStat
SHOW GET lcCurSmbl1  &lcLinStat
SHOW GET lnTotEquCst &lcLinStat
SHOW GET lcBsCrSmbl  &lcLinStat
SHOW GET lnExRate    &lcLinStat

*-- If purchase style & cost greater than zero & the price currency = duty
*-- currency & the cost item is Mfg operation or duty, enable the percentage.

IF lcStyleTyp = 'I' .AND. lnCstPerc > 0 .AND. &lcTmpBom..cCatgTyp $ 'MD'
  SHOW GET lnCstPerc &lcLinStat
ELSE
  SHOW GET lnCstPerc DISABLE
ENDIF

*-- Disable the duty popup in case of duty cost item.
*-- will appear at all cases
SHOW GET ibDutyabl  &lcLinStat

*!*************************************************************
*! Name      : lfvNewEle
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Show Style/Material Cost Sheet line details.
*!*************************************************************
*! Calls     : MFCSTSC.PRG, lfBrwCstSh, lfShowDet, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewEle()
*!*************************************************************
*
FUNCTION lfvNewEle
PRIVATE lnItemRec

*-- Clear the bom filter.
SELECT BOM
SET FILTER TO

*-- Save record pointer for the item file.
SELECT (lcItemFile)
lnItemRec = RECNO()
SET FILTER TO

*-- Clear all the traps.
PUSH KEY
ON KEY

*-- Call the program that add a new cost item.
DO (gcAppHome+"MFCSTSC") WITH lcStyleTyp , lcStyMaj , lcTmpBom , ;
                              lcTmpFbric , lcTmpStyle , IIF(cbOnSize , "Y" , "N")
POP KEY
SELECT (lcTmpBom)
SET FILTER TO

SET FILTER TO (citmmajor= &lcOrdSty..Style) 

LOCATE
lnCurrDuty = 0
SCAN 
  IF  EMPTY(ALLTRIM(STYLE))
    REPLACE &lcTmpBom..Style WITH &lcOrdSty..Style
  ENDIF  
  *--to initiate Fabric and trim with Cost status Both

  IF cStatus = 'A' 
    IF llMulCurr
      *-- Get purchase price currency, duty currency or base currency
      lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' , Style.cPriceCur , ;
                       IIF(&lcTmpBom..cCatgTyp $ 'MD' , Style.cDutyCur , gcBaseCurr)))
      lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
      *-- Get currency unit and exchange rate. 
      *-- Get currency symbol.
      llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
      lnExRate  = IIF(llFound , sycexch.nExRate , 1)
      llFound    = SEEK(lcCurrency , 'SycCurr')
      lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
      STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
      lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
      lnTotEquCst = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,3))
      REPLACE  &lcTmpBom..nexrate WITH IIF(llFound , sycexch.nExRate , 1),;
               &lcTmpBom..nexcost WITH lnTotEquCst
      REPLACE  &lcTmpBom..cCurrCode WITH lcCurrency 
    ENDIF
  
    IF &lcTmpBom..cCatgTyp $ "FT" AND EMPTY(&lcTmpBom..cCostStat)
      REPLACE cCostStat WITH '3'
      IF llMulCurr
        SELECT sycexch
        *lcCurOrd = ORDER()
        *SET ORDER TO Exchdate
        llFound   = SEEK(gcBaseCurr +Fabric.cPriceCur+DTOS(Fabric.introduced) , 'SycExch')
        IF llFound
          lnExRate  =  sycexch.nExRate 
        ELSE
          llFound   = SEEK(gcBaseCurr + Fabric.cPriceCur, 'SycExch')
          lnExRate  = IIF(llFound , sycexch.nExRate , 1)
        ENDIF
        
        SELECT (lcTmpBom)      
        REPLACE cfabcurr   WITH Fabric.cPriceCur,;
                nFabExRate WITH lnExRate
      ENDIF
    ENDIF
    
  ENDIF  
  *--to calculate Duty and material Loss
  IF &lcTmpBom..cCatgTyp $ "DM"
    lnCstPerc =&lcTmpBom..nPercent
    =lfvCstPrc()
    IF &lcTmpBom..cCatgTyp $ "D"
      lnCurrDuty = &lcTmpBom..nPercent
    ENDIF
  ENDIF
ENDSCAN

SELECT (lcTmpBom)

SET FILTER TO (citmmajor= &lcOrdSty..Style) 
REPLACE ALL &lcTmpBom..nDutyRate WITH lnCurrDuty FOR ccoststat $ '13'
SET ORDER TO 1
  

SELECT (lcTmpBom)
LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
llOnSize  = IIF(FOUND() , .T. , .F. )
lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE" )
SHOW GET cbOnSize &lcBasStat

GO TOP IN (lcTmpBom)

*-- If the main browse was released, call the browse function again.
IF !WEXIST(lcBrCstShT)
  =lfBrwCstSh()
ELSE
  =lfShowDet()
ENDIF

*-- Restore the bom file filter.
SELECT BOM
SET FILTER TO IIF(lcStyleTyp='T', lMaterial, !lMaterial)

*-- Restore the record pointer for the temp. BOM file.
SELECT (lcItemFile)
IF lnItemRec > 0 .AND. lnItemRec <= RECCOUNT()
  GOTO lnItemRec
ENDIF

*-- Restore the main file filter.
DO CASE
  CASE lcStyleTyp = 'I'
    SET FILTER TO !Style.Make
  CASE lcStyleTyp = 'M'
    SET FILTER TO Style.Make
ENDCASE

*-- Update the status variables.
lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
SHOW GET pbRemEle &lcLinStat
SHOW GET pbSearch &lcSrcStat


*!*************************************************************
*! Name      : lfvBrows
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrows()
*!*************************************************************
*
FUNCTION lfvBrows

IF !WONTOP(lcBrCstShT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*!*************************************************************
*! Name      : lfChckCost
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to check the changed cost for the styles, 
*!           : fabrics & trims cost items in their files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChckCost()
*!*************************************************************
*B802394,1 Reham On 08/03/1999
*
FUNCTION lfChckCost
PRIVATE lnStyRec , lnFbrRec , llFondChng , llChngCost , llDispMsg , lnBomRec

*-- Release the main file filter.
SELECT (lcItemFile)
SET FILTER TO

*-- Save the record pointer in th style file.
SET ORDER TO TAG STYLE  IN STYLE
lnStyRec = RECNO("STYLE")

*-- Save the record pointer in the fabric file.
SET ORDER TO TAG FABRIC IN FABRIC
lnFbrRec = RECNO("FABRIC")

llFondChng = .F.  && Flag to know if there is any cost item has changed cost.
llChngCost = .F.  && Flag to know if the user choose to change the cost or not.
llDispMsg  = .F.  && Flag to display the query message just once.

*-- Scan only for styles, fabrics & trim cost items.
SELECT (lcTmpBom)
SCAN FOR cCatgTyp $ "FS" OR (cCatgTyp = "T" .AND. Trim_Invt)
  llFondChng = .F.
  lnCurUnCst = &lcTmpBom..UntCost
  IF cCatgTyp $ "FT"
    *-- If the cost item type is fabric or trim, check the cost in the fabric file.
    SELECT FABRIC
    lcCurSeek = IIF("*" $ &lcTmpBom..IClr , PADR(&lcTmpBom..Item , 7) , PADR(&lcTmpBom..Item , 7)+&lcTmpBom..IClr)
    =SEEK(lcCurSeek , "FABRIC")
    *E301842,1 AMH Rounding to make a correct check [Start]
    *llFondChng = IIF(&lcTmpBom..UntCost <> (Fabric.CostBuy/Fabric.Conv) , .T. , .F.)
    llFondChng = IIF(&lcTmpBom..UntCost <> ROUND(Fabric.CostBuy/Fabric.Conv,3) , .T. , .F.)
    *E301842,1 AMH [End]
  ELSE
    *-- If the cost item type is style, check the cost in the style file.
    SELECT STYLE
    =SEEK(PADR(&lcTmpBom..Item , lnMajorLen) , "STYLE")
    LOCATE FOR LIKE(STRTRAN(&lcTmpBom..Item , "*" , "?") , STYLE.Style)
    *-- Calculate the cost for the current cost item.
    llFondChng = IIF(FOUND() .AND. &lcTmpBom..UntCost <> Style.TotCost , .T. , .F.)
  ENDIF
  
  *-- If there is change in the current cost item.
  IF llFondChng
    *-- Display the query message only once.
    IF !llDispMsg
      llDispMsg = .T.
      *** There is one or more cost items(s) have changed cost. Would ***
      *** you like to update the cost items with the changed cost? ***
      *** <  Yes  > - <  No  > ***
      *-- Set flag to true if the user choosed to change all the changed cost items.
      llChngCost = (gfModalGen("QRM38175B00006" , "DIALOG") = 1)
    ENDIF
    IF !llChngCost
      *-- If the user did not choose to change the current cost item, exit from the current scan.
      EXIT
    ELSE
      
      SELECT (lcTmpBom)
      *-- Save the old total cost before updating with the new one.
      lnOld = TotCost
      
      *-- Update unit cost and total cost.
      REPLACE UntCost WITH IIF(cCatgTyp $ "FT" , (Fabric.CostBuy/Fabric.Conv) , Style.TotCost) ;
              TotCost WITH nBomTotQty * UntCost ;
              cStatus WITH IIF(cStatus = "S" , "M" , cStatus)
      
      *-- If the current cost item is dutyable call function to update
      *-- all the manufacutered records that has percentage.
      IF cCostStat = '1' .AND. !(lnOld == TotCost)
        =lfUpdCost(+1 , lnOld)
      ENDIF
      
      *-- If style cost sheet only...
      IF lcStyleTyp <> "T" .AND. !(lnOld == TotCost)
        *-- Save current bom file record pointer.
        lnBomRec = RECNO()
        *-- Call local function to calculate the current parent cost in its
        *-- parent records...
        DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH cItmMask
        *-- Restore record pointer in bom temp file.
        IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT()
          GOTO lnBomRec
        ENDIF
      ENDIF

    ENDIF
  ENDIF
  SELECT (lcTmpBom)
ENDSCAN

*-- Restore the record pointer in the style file.
SELECT STYLE
SET ORDER TO TAG CSTYLE IN STYLE
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT()
  GOTO lnStyRec
ENDIF

*-- Restore the record pointer in the fabric file.
SELECT FABRIC
SET ORDER TO TAG CFABRIC IN FABRIC
IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT()
  GOTO lnFbrRec
ENDIF

*-- Restore the main file filter.
SELECT (lcItemFile)
DO CASE
  CASE lcStyleTyp = 'I'
    SET FILTER TO !Style.Make
  CASE lcStyleTyp = 'M'
    SET FILTER TO Style.Make
ENDCASE

SELECT (lcTmpBom)
GO TOP


*!*************************************************************
*! Name      : lfvOnSize
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function of check box "base cost sheet on size"
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvOnSize()
*!*************************************************************
*
FUNCTION lfvOnSize
PRIVATE lnCurRec

IF lnBomLnNo > 0
  *-- Save record pointer for the temp. BOM file.
  SELECT (lcTmpBom)
  lnCurRec = RECNO()
  
  *-- Replace the field of "BASE COST SHEET ON SIZE" with the check box value.
  REPLACE ALL lBasOnSiz WITH cbOnSize ;
              cStatus   WITH IIF(cStatus = "S" , "M" , cStatus)
  
  *-- Restore the record pointer in the temp. BOM file.
  IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
    GOTO lnCurRec
  ENDIF
  
  *-- Change the status of selected sizes on the level of the line.
  lcSizStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. (cbOnSize .OR. &lcTmpBom..cCatgTyp = "S") .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
  SHOW GET cbSelSize  &lcSizStat
  SHOW GET cbOnSize
  
  SELECT (lcItemFile)
ENDIF

*!*************************************************************
*! Name      : lfvSelSize
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function of check box "Select sizes."
*!*************************************************************
*! Calls     : MFCSTSZ.PRG, lfUpdPerc, lfUpdCost
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvSelSize()
*!*************************************************************
*
FUNCTION lfvSelSize
PRIVATE lnCnt , lnStyRec
  

*-- Array hold the parent scale.
DECLARE laScale[1,3]
laScale   = ""

*-- Collect all the sizes for the parent scales..
*-- Get all the available scale for the current mask.
SELECT STYLE
*-- Save the style record pointer.
lnStyRec = RECNO()
SET ORDER TO TAG STYLE
=SEEK(PADR(&lcTmpBom..Style,lnMajorLen))
SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..Style,lnMajorLen) ;
            FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
  *-- Get the scale info.
  IF SEEK("S" + Style.Scale , "SCALE")
    *-- Get the sizes for the current scale.
    lcCurSize = ""
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt     = STR(lnCnt,1)
      lcCurSize = lcCurSize + IIF(EMPTY(lcCurSize) , "" , ",") + lcCnt
    ENDFOR
    
    *-- Insert the current scale to the scale array.
    
    lcScale = PADR(Style.Scale,3) + " * " + LOOKUP(Scale.cscl_desc , "S"+Style.Scale , Scale.Scale , "SCALE")
    
    IF ASCAN(laScale , PADR(Scale.Scale,3)) = 0
      IF !EMPTY(laScale[1,1])
        DECLARE laScale[ALEN(laScale,1)+1,3]
      ENDIF
      laScale[ALEN(laScale,1),1] = lcScale
      laScale[ALEN(laScale,1),2] = IIF(EMPTY(&lcTmpBom..mSizes) , lcCurSize , "")
      laScale[ALEN(laScale,1),3] = PADR(Scale.Scale,3)
    ENDIF
  ENDIF
ENDSCAN
*-- Restore the style order.
SET ORDER TO TAG CSTYLE IN STYLE
*-- Restore the record pointer.
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF

*-- If not empty of the parent sizes field, assign the selected 
*-- sizes to the parent scale array.
IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
  FOR lnCnt = 1 TO ALEN(laScale,1)
    lnLineNo = ATCLINE(laScale[lnCnt,3] , ALLTRIM(&lcTmpBom..mSizes))
    IF lnLinENo > 0
      *-- Get the scale size
      laScale[lnCnt,2] = SUBSTR(ALLTRIM(MLINE(&lcTmpBom..mSizes,lnLineNo)),5)
    ENDIF
  ENDFOR
ENDIF

*-- If the type of the current record is : STYLE COMPONENT.
IF &lcTmpBom..cCatgTyp = "S"
  *-- Initialize the needed data for the scale size screen.
  puScale2   = 1
  STORE 0 TO lnOldScale , lnCurScCnt
  STORE .F. TO cbPrntSz1 , cbPrntSz2 , cbPrntSz3 , cbPrntSz4 , ;
               cbPrntSz5 , cbPrntSz6 , cbPrntSz7 , cbPrntSz8
  STORE " " TO lcPrmt1 , lcPrmt2 , lcPrmt3 , lcPrmt4 , ;
               lcPrmt5 , lcPrmt6 , lcPrmt7 , lcPrmt8
  *-- Fill the array that hold the scales of the style component.
  DECLARE laChldSz[1,3]
  laChldSz[1,1] = "N/A"
  laChldSz[1,2] = 0
  laChldSz[1,3] = ""
  *-- Scan to collect the different scales of the style component.
  SELECT STYLE
  SET FILTER TO
  SET ORDER TO TAG STYLE
  =SEEK(PADR(&lcTmpBom..Item,lnMajorLen) , "STYLE")
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..Item,lnMajorLen) ;
              FOR LIKE(STRTRAN(&lcTmpBom..Item , "*" , "?") , STYLE.Style)
    IF SEEK("S" + Style.Scale , "SCALE")
      FOR lnCnt = 1 TO Scale.Cnt
        lcCnt   = STR(lnCnt,1)

        lcSize  = PADR(Scale.Scale,3)+" * "+PADR(Scale.cScl_Desc,10)+" * "+Scale.Sz&lcCnt
        
        IF ASCAN(laChldSz , lcSize) = 0
          IF !EMPTY(laChldSz[1,1])
            DECLARE laChldSz[ALEN(laChldSz,1)+1,3]
          ENDIF
          laChldSz[ALEN(laChldSz,1),1] = lcSize && Hold scale description +size
          laChldSz[ALEN(laChldSz,1),2] = lnCnt  && Hold where is size in its scale.
          laChldSz[ALEN(laChldSz,1),3] = PADR(Style.Scale,3) && Hold scale
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
  SET ORDER TO TAG CSTYLE IN STYLE
  DO CASE
    CASE lcStyleTyp = 'I'
      SET FILTER TO !Style.Make
    CASE lcStyleTyp = 'M'
      SET FILTER TO Style.Make
  ENDCASE
  *-- Restore the record pointer.
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF
  
  *-- If the size cross reference field is empty. Assign default style
  *-- component sizes to the parent style sizes.
  IF EMPTY(&lcTmpBom..mSzCrosRef)
    *-- Adjust the assigning of the style component sizes to the
    *-- style parent sizes.
    DECLARE laMainScal[1,2]
    laMainScal = ""
    lnChldSz   = 2
    FOR lnCnt1 = 1 TO ALEN(laScale,1)
      *-- Seek to get the current scale sizes.
      =SEEK("S" + SUBSTR(laScale[lnCnt1,3],1,3) , "SCALE")
      FOR lnCnt2 = 1 TO Scale.Cnt
        lcCnt2 = STR(lnCnt2,1)
        *-- Add new row in the main scale array.
        IF !EMPTY(laMainScal[1,1])
          DIMENSION laMainScal[ALEN(laMainScal,1)+1 , 2]
        ENDIF
        *-- first column hold the current parent scale + current parent scale size.
        laMainScal[ALEN(laMainScal,1),1] = "*"+SUBSTR(laScale[lnCnt1,3],1,3)+","+lcCnt2
        *-- Second column hold the child scale size of the style comonent + its size.
        laMainScal[ALEN(laMainScal,1),2] = "#"+SUBSTR(laChldSz[lnChldSz,3],1,3)+","+STR(laChldSz[lnChldSz,2],1)
        *-- Adjust the child array counter.
        lnChldSz   = lnChldSz + IIF(lnChldSz < ALEN(laChldSz,1) , 1 , 0)
      ENDFOR
    ENDFOR
  ELSE
    *-- Adjust the assigning of the style component sizes to the
    *-- style parent sizes.
    DECLARE laMainScal[1,2]
    laMainScal = ""
    lnChldSz   = 2
    FOR lnCnt1 = 1 TO ALEN(laScale,1)
      *-- Seek to get the current scale sizes.
      =SEEK("S" + SUBSTR(laScale[lnCnt1,3],1,3) , "SCALE")
      FOR lnCnt2 = 1 TO Scale.Cnt
        lcCnt2 = STR(lnCnt2,1)
        *-- Add new row in the main scale array.
        IF !EMPTY(laMainScal[1,1])
          DIMENSION laMainScal[ALEN(laMainScal,1)+1 , 2]
        ENDIF
        *-- first column hold the current parent scale + current parent scale size.
        laMainScal[ALEN(laMainScal,1),1] = "*"+SUBSTR(laScale[lnCnt1,3],1,3)+","+lcCnt2
        *-- Get the no. of the parent size line in the memo field if this 
        *-- size is assigned to child size.
        lnLineNo = ATCLINE(SUBSTR(laMainScal[ALEN(laMainScal,1),1],2)+"~" , &lcTmpBom..mSzCrosRef)
        IF lnLineNo > 0
          *-- Get the line string from the memo field.
          lcCurScal = ALLTRIM(MLINE(&lcTmpBom..mSzCrosRef , lnLineNo))
          *-- Fill the second row with the child scale + child size.
          laMainScal[ALEN(laMainScal,1),2] = "#"+SUBSTR(lcCurScal , AT("~" , lcCurScal)+1)
        ELSE
          *-- If nothing was assigned to the current parent scale.
          laMainScal[ALEN(laMainScal,1),2] = ""
        ENDIF
        lnChldSz   = lnChldSz + IIF(lnChldSz < ALEN(laChldSz,1) , 1 , 0)
      ENDFOR
    ENDFOR
  ENDIF
  llSaveSize = .F.  && Flag to know if save the sizes or not.
  
  *-- Call the size program & screen.
  DO (gcAppHome+"MFCSTSZ") WITH &lcTmpBom..cCatgTyp
  *-- If save the sizes
  IF llSaveSize
    lcCrosRef = ""  && Hold the parent sizes & its child sizes.
    lcPrntSiz = ""  && Hold the parent scales & its parent sizes.
    
    lcScale   = ""  && To hold the current parent scale.
    lcSizes   = ""  && To hold the sizes of the parent scale.
    lcOldScal = SUBSTR(laMainScal[1,1],2,3)  && Save first parent scale.
    lcOldSize = SUBSTR(laMainScal[1,1],6,1)  && Save first parent scale size.
    
    *-- Loop in the main scale to concatinate the cross reference 
    *-- sizes & the parent sizes.
    FOR lnCnt = 1 TO ALEN(laMainScale,1)
      IF !EMPTY(laMainScal[lnCnt,2])
        lcCrosRef = lcCrosRef + SUBSTR(laMainScal[lnCnt,1],2) + "~" + ;
                    SUBSTR(laMainScal[lnCnt,2],2) + CHR(13)
        
        IF lcOldScal <> SUBSTR(laMainScal[lnCnt,1],2,3)
          lcPrntSiz = lcPrntSiz + lcOldScal + "~" + lcSizes + CHR(13)
          lcScale   = SUBSTR(laMainScal[lnCnt,1],2,3)
          lcSizes   = SUBSTR(laMainScal[lnCnt,1],6,1)
        ELSE
          lcSizes = lcSizes + IIF(EMPTY(lcSizes) , "" , ",") + SUBSTR(laMainScal[lnCnt,1],6,1)
        ENDIF
        lcOldScal = SUBSTR(laMainScal[lnCnt,1],2,3)
      ENDIF
    ENDFOR
    
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to subtract the current cost item from all the manufacutered 
    *-- or duty cost items records that has percentage before updating 
    *-- with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      =lfUpdCost(-1 , 0)
    ENDIF
    
    lcPrntSiz = lcPrntSiz + lcOldScal + "~" + lcSizes + CHR(13)
    *-- Update the sizes in the bom file.
    SELECT (lcTmpBom)
    REPLACE &lcTmpBom..mSizes     WITH lcPrntSiz ;
            &lcTmpBom..mSzCrosRef WITH lcCrosRef ;
            &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
    
    *-- Update the duty or manufactered cost items if modify the 
    *-- sizes in case of :
    *-- 1- For duty, Get cost as a percent of dutiable items.
    *-- 2- For manufacturin operations Get cost as a percent of the price.
    IF &lcTmpBom..cCatgTyp $ "MD" .AND. &lcTmpBom..nPercent <> 0
      =lfUpdPerc(&lcTmpBom..cCatgTyp)
    ENDIF
    
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to add the current cost to all the manufacutered or duty records 
    *-- that has percentage after updating with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      =lfUpdCost(+1 , 0)
    ENDIF
  ENDIF
  
  cbSelSize = IIF(!EMPTY(ALLTRIM(&lcTmpBom..mSzCrosRef)) , .T. , .F.)
ELSE
  *-- Initialize the needed data for the scale size screen.
  lnOldScale = 0
  STORE 1 TO puScale1 , puAllSiz , lsAllSiz , lsSelSiz
  DECLARE laAllSiz[1,1] , laSelSiz[1,1]
  STORE "" TO laAllSiz , laSelSiz
  *-- Fill the source array with the sizes of the first scale in the popup.
  SELECT SCALE
  IF SEEK("S" + SUBSTR(laScale[1,3],1,3) , "SCALE")
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt = STR(lnCnt,1)
      IF !EMPTY(laAllSiz[1,1])
        DECLARE laAllSiz[ALEN(laAllSiz,1)+1,1]
      ENDIF
      laAllSiz[ALEN(laAllSiz,1),1] = PADR(SZ&lcCnt,40) + lcCnt
      *-- Fill the target array.
      lnLineNo = ATCLINE(laScale[puScale1,2] , &lcTmpBom..mSizes)
      IF lcCnt $ laScale[puScale1,2]
        IF !EMPTY(laSelSiz[1,1])
          DECLARE laSelSiz[ALEN(laSelSiz,1)+1,1]
        ENDIF
        laSelSiz[ALEN(laSelSiz,1),1] = PADR(SZ&lcCnt,40) + lcCnt
      ENDIF
    ENDFOR
  ENDIF
  
  *-- Define a popup from array laAllSiz that hold all the sizes 
  *-- for the first scale in the popup.
  DEFINE POPUP puAllSiz MARGIN RELATIVE SCROLL MARK CHR(16)
  FOR lnCnt = 1 TO ALEN('laAllSiz',1)
    DEFINE BAR lnCnt OF puAllSiz PROMPT (ALLTRIM(laAllSiz[lnCnt,1]))
    IF ASCAN('laSelSiz',ALLTRIM(laAllSiz[lnCnt,1])) > 0
      SET SKIP OF BAR lnCnt OF puAllSiz .T.
    ENDIF
  ENDFOR
  
  llSaveSize = .F.
  
  *-- Call the size screen.
  DO (gcAppHome+"MFCSTSZ") WITH &lcTmpBom..cCatgTyp
  
  IF llSaveSize
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to subtract the current cost item from all the manufacutered 
    *-- or duty cost items records that has percentage before updating 
    *-- with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      *DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH -1 , 0
        =lfUpdCost(-1 , 0)
    ENDIF
    
    *-- Update the sizes in the bom file.
    SELECT (lcTmpBom)
    lcSizes = ""
    FOR lnCnt = 1 TO ALEN(laScale,1)
      IF !EMPTY(laScale[lnCnt,2])
        lcSizes = lcSizes + SUBSTR(laScale[lnCnt,3],1,3) + "~" + laScale[lnCnt,2] + CHR(13)
      ENDIF
    ENDFOR
    REPLACE &lcTmpBom..mSizes  WITH lcSizes ;
            &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
    
    *-- Update the duty or manufactered cost items if modify the 
    *-- sizes in case of :
    *-- 1- For duty, Get cost as a percent of dutiable items.
    *-- 2- For manufacturin operations Get cost as a percent of the price.
    IF &lcTmpBom..cCatgTyp $ "MD" .AND. &lcTmpBom..nPercent <> 0
      =lfUpdPerc(&lcTmpBom..cCatgTyp)
    ENDIF
    
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to add the current cost to all the manufacutered or duty records 
    *-- that has percentage after updating with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      =lfUpdCost(1 , 0)
    ENDIF
  ENDIF
  
  cbSelSize = IIF(!EMPTY(ALLTRIM(&lcTmpBom..mSizes)) , .T. , .F.)
ENDIF

SHOW GET cbSelSize

lcAls = ALIAS()
SELECT (lcTmpBom)
lnCrBomRc = RECNO()
LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
llOnSize  = IIF(FOUND() , .T. , .F. )
IF lnCrBomRc > 0 AND lnCrBomRc <= RECCOUNT()
  GOTO lnCrBomRc
ENDIF
SELECT (lcAls)
lcBasStat = IIF(!llOnSize , "ENABLE" , "DISABLE" )

SHOW GET cbOnSize  &lcBasStat

SELECT (lcTmpBom)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Cost Items (Fabric,Trims)
*!*************************************************************
*! Calls     : FaBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvFabric()
*!*************************************************************
*
FUNCTION lfvFabric
PRIVATE lnAlias

IF EMPTY(lcCostItmF)
  =gfModalGen("INM00000B00000","DIALOG",'','',ALLTRIM(lcCstSLbl)+" cannot be empty.")
  lcCostItmF = EVALUATE(lcTmpBom+'.Item')
  RETURN
ENDIF

lnAlias = SELECT(0)
DO CASE
  *-- If the cost type is fabric.
  CASE EVALUATE(lcTmpBom+'.cCatgTyp') = 'F'
    IF llBrowse .OR. !SEEK(PADR(lcCostItmF,7) , 'Fabric')
      llBrowse = .F.
      *-- Call the fabric global browse.
      =FaBrow(@lcCostItmF,'*')
    ENDIF
    IF !EMPTY(lcCostItmF)
      lnUnitCst  = Fabric.CostBuy/Fabric.Conv
      lnTotFCst  = ROUND(ROUND((lnEstQtyA*(1+(lnWstg/100))) , 3) * lnUnitCst , 2)
      lcCstItmDF = Fabric.Desc
      lcUom      = Fabric.UomUse
    ELSE
      _CUROBJ = OBJNUM(lcCostItmF)
    ENDIF
  *-- If the cost item is trim.
  CASE EVALUATE(lcTmpBom+'.cCatgTyp') = 'T'
    *-- If the system was setup to use non inventory trims.
    IF llTrimInv
      llItmTrInv = IIF(SEEK(PADR(lcCostItmF,7),'FABRIC') , .T. , .F.)
      
      *** Trim not found in the file.  Keep as non-inventory item? ***
      *** < Yes > - < No > ***
      IF llBrowse .OR. ("?" $ lcCostItmF) .OR. (!llItmTrInv .AND. gfModalGen("INM38022B00006" , "DIALOG") = 2)
        llBrowse = .F.
        SELECT Fabric
        IF !FaBrow(@lcCostItmF , '*')
          lcCostItmF = SPACE(7)
          _CUROBJ    = OBJNUM(lcCostItmF)
          SELECT (lnAlias)
          RETURN
        ENDIF
        llItmTrInv = .T.
      ENDIF
    ELSE
      *-- If the system was setup to not use non inventory trims.
      IF llBrowse .OR. !SEEK(PADR(lcCostItmF,7),'FABRIC')
        llBrowse = .F.
        SELECT Fabric
        IF !FaBrow(@lcCostItmF , '*')
          lcCostItmF = SPACE(7)
          _CUROBJ    = OBJNUM(lcCostItmF)
          SELECT (lnAlias)
          RETURN
        ENDIF
      ENDIF
      llItmTrInv = IIF(SEEK(PADR(lcCostItmF,7),'FABRIC') , .T. , .F.)
    ENDIF
    IF llItmTrInv .AND. !EMPTY(lcCostItmF)
      *-- If entering material cost sheet , trim cannot be same as main item.
      lnUnitCst  = Fabric.CostBuy/Fabric.Conv
      lnTotFCst  = ROUND(ROUND((lnEstQtyA*(1+(lnWstg/100))) , 3) * lnUnitCst , 2)
      lcCstItmDF = Fabric.Desc
      lcUom      = Fabric.UomUse
    ENDIF
ENDCASE

IF (EVALUATE(lcTmpBom+'.cCatgTyp') = 'F' .OR. (EVALUATE(lcTmpBom+'.cCatgTyp') = 'T' .AND. llItmTrInv));
   .AND. !EMPTY(lcCostItmF)

  *-- Check if need to add colors
  IF llNonMjExt .OR. lcStyleTyp = "T"
    lcMask = IIF(lcStyleTyp='T',EVALUATE(lcTmpBom+'.cItmMask'),;
                 SUBSTR(EVALUATE(lcTmpBom+'.cItmMask'),lnMajorLen+LEN(ALLTRIM(lcSeprator))+1))
    IF lcMask = '******' .AND. EVALUATE(lcTmpBom+'.ICLR') = '******'
      IF !lfScheme()
        lcCostItmF = EVALUATE(lcTmpBom+'.Item')
        lnUnitcst  = EVALUATE(lcTmpBom+'.UntCost')
        lnTotFCst  = EVALUATE(lcTmpBom+'.TotCost')
        lcCstItmDF = EVALUATE(lcTmpBom+'.DESC')
        lcUom      = EVALUATE(lcTmpBom+'.UOM')
        RETURN
      ENDIF
    ENDIF
  ENDIF

  *-- Refresh the cost & Desc objects in the screen.
  SHOW GET lnUnitCst
  SHOW GET lnTotFCst
  SHOW GET lcCstItmDF
  SHOW GET lcUom

  *-- Update the temp. bom file.
  REPLACE &lcTmpBom..ITEM    WITH lcCostItmF;
          &lcTmpBom..UntCost WITH lnUnitCst ;
          &lcTmpBom..TotCost WITH lnTotFCst ;
          &lcTmpBom..DESC    WITH lcCstItmDF;
          &lcTmpBom..Uom     WITH lcUom     ;
          &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

  *-- Refresh the main browse.
  SHOW WINDOW (lcBrCstShT) REFRESH SAME
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvFabClr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Fabric/trim color.
*!*************************************************************
*! Calls     : FABrow, gfBrows
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvFabClr()
*!*************************************************************
*
FUNCTION lfvFabClr
PRIVATE lcAlias , lnBomRec

IF EMPTY(lcIClr)
  =gfModalGen("INM00000B00000","DIALOG",'','',"Color cannot be empty.")
  lcIClr = EVALUATE(lcTmpBom+'.ICLR')
  RETURN
ENDIF

DO CASE
  *-- If the cost type is fabric, browse the available colors 
  *-- for the selected fabric.
  CASE EVALUATE(lcTmpBom+'.cCatgTyp') = 'F'
    SET ORDER TO FABRIC IN FABRIC
    IF llBrowse .OR. !SEEK(PADR(lcCostItmF,7)+PADR(lcIClr,6) , 'Fabric')
      llBrowse = .F.
      lcIClr = CHR(240)
      =FaBrow(PADR(lcCostItmF,7),@lcIClr)
      SET ORDER TO CFABRIC IN FABRIC
      IF EMPTY(lcIClr)
        lcIClr  = SPACE(6)
        _CUROBJ = OBJNUM(lcIClr)
        RETURN
      ELSE
        lcIClr = PADR(lcIClr,6)
      ENDIF
    ENDIF
    SET ORDER TO CFABRIC IN FABRIC
  *-- If the cost type is Trim.
  CASE EVALUATE(lcTmpBom+'.cCatgTyp') = 'T'
    llItmTrInv = IIF(SEEK(PADR(lcCostItmF,7),'FABRIC') , .T. , .F.)
    IF llItmTrInv
      *-- Browse from the fabric file.
      SET ORDER TO FABRIC IN FABRIC
      IF llBrowse .OR. !SEEK(PADR(lcCostItmF,7)+PADR(lcIClr,6) , 'Fabric')
        llBrowse = .F.
        lcIClr = CHR(240)
        =FaBrow(PADR(lcCostItmF,7),@lcIClr)
        SET ORDER TO CFABRIC IN FABRIC
        IF EMPTY(lcIClr)
          lcIClr  = SPACE(6)
          _CUROBJ = OBJNUM(lcIClr)
          RETURN
        ELSE
          lcIClr = PADR(lcIClr,6)
        ENDIF
      ENDIF
      SET ORDER TO CFABRIC IN FABRIC
    ELSE
      *-- Browse colors from the codes file.
      lcOldAlias = ALIAS()
      lcOldTag   = ORDER("CODES")
      SELECT CODES
      SET ORDER TO cCode_No IN CODES
      IF llBrowse .OR. !SEEK("N" + PADR("COLOR",10) + PADR(lcIClr,6) , "CODES")
        llBrowse = .F.
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT("CODES")
          GO RECNO(0)
        ELSE
          GO TOP
        ENDIF
        lcSaveBrow = lcBrFields
        lcBrfields = "cCode_No :H='Color' , cDiscrep :H='Description' "
        lcOld_ttl  = lcFile_ttl
        lcFile_ttl = "Colors"
        DECLARE laClr[1]
        laClr[1]   = lcIClr
        
        SET FILTER TO cRltField = "N"
        =gfBrows(["N" + "COLOR"],"cCode_No","laClr")
        SET FILTER TO

        lcBrFields = lcSaveBrow
        lcFile_ttl = lcOld_ttl
        lcIClr = IIF(lcIClr = laClr[1] , SPACE(6) , laClr[1])
      ENDIF
      SET ORDER TO &lcOldTag IN CODES
      SELECT (lcOldAlias)
    ENDIF
ENDCASE

*-- Refresh the cost objects in the screen.
IF (EVALUATE(lcTmpBom+'.cCatgTyp') = 'F' .OR. (EVALUATE(lcTmpBom+'.cCatgTyp') = 'T' .AND. llItmTrInv)) ;
   .AND. !EMPTY(lcIClr)
  lnUnitCst  = Fabric.CostBuy/Fabric.Conv
  lnTotFCst  = ROUND(ROUND((lnEstQtyA*(1+(lnWstg/100))) , 3) * lnUnitCst , 2)

  *-- Refresh the cost & Desc objects in the screen.
  SHOW GET lnUnitCst
  SHOW GET lnTotFCst

  *-- Update the temp. bom file.
  REPLACE &lcTmpBom..ICLR    WITH lcIClr   ;
          &lcTmpBom..UntCost WITH lnUnitCst;
          &lcTmpBom..TotCost WITH lnTotFCst;
          &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

  *-- Refresh the main browse.
  SHOW WINDOW (lcBrCstShT) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfScheme
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Selected Color Scheme.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfScheme()
*!*************************************************************
*
FUNCTION lfScheme
PRIVATE laItemRec , lnCurRec , lnFbrRec , lnStyRec

*-- If "same as color" , cross refrencing the colors or the majors.
IF EVALUATE(lcTmpBom+'.cCatgTyp') = 'F' .OR.;
   (EVALUATE(lcTmpBom+'.cCatgTyp') = 'T' .AND. llItmTrInv)
  lnFbrRec = RECNO("FABRIC")
  lnStyRec = RECNO("STYLE")
  WAIT IIF(EVALUATE(lcTmpBom+'.cCatgTyp')='F','Cross referencing Fabric colors',;
                                              'Cross referencing Trim colors') WINDOW NOWAIT
  SELECT FABRIC
  =SEEK(PADR(lcCostItmF,7))
  SCATTER TO laItemRec
  
  SET ORDER TO FABRIC IN FABRIC
  SET ORDER TO STYLE  IN STYLE
  lcScanExp = IIF(lcStyleTyp = "T" , " FOR Fabric.Make" , "")
  SELECT (lcItemFile)
  =SEEK(PADR(lcStyMaj,lnItemLen))
  SCAN REST WHILE PADR(&lcItemFile,lnItemLen) = PADR(lcStyMaj,lnItemLen) &lcScanExp
    lnCurRec = RECNO()
    *-- If the item file is fabric, use the color field.
    *-- If the item file is style, cut the color according to 
    *-- the color start position & color segment lenght.
    lcColor  = IIF(lcStyleTyp ='T' , &lcItemFile..Color , ;
               SUBSTR(&lcItemFile..Style , lnColorStr , lnColorLen))
    IF !SEEK(SUBSTR(lcCostItmF,1,7)+PADR(lcColor,6) , 'Fabric') .AND. ;
       !SEEK(SUBSTR(lcCostItmF,1,7)+PADR(lcColor,6) , lcTmpFbric)
      
      *** {lcNonMjHdr} : {lcColor} not found for ***
      *** {IIF(lcCstType= 'F','Fabric','Trim')}: ***
      *** SUBSTR(lcNFabItem,1,7). Do you wish to add it? ***
      *** < Yes > - < No > ***
      lcTmpStr = IIF(lcStyleTyp="T",lcItemFile+"/"+"color",lcNonMjHdr)+"|"+lcColor+"|"+IIF(EVALUATE(lcTmpBom+'.cCatgTyp')= 'F','Fabric','Trim')+"|"+SUBSTR(lcCostItmF,1,7)
      IF gfModalGen("QRM38012B00006" , "DIALOG" , lcTmpStr) = 2
        RETURN(.F.)
      ENDIF
      
      *-- If the fabric + color was not found, add it in the temp. fabric file.
      SELECT (lcTmpFbric)
      APPEND BLANK
      GATHER FROM laItemRec
      REPLACE COLOR      WITH lcColor ;
              ONHAND     WITH 0 ;
              ONORDER    WITH 0 ;
              USAGE      WITH 0 ;
              nStkVal    WITH 0 ;
              nMatWip    WITH 0
      
      *-- Call global function to add audit fields info.
      =gfAdd_Info(lcTmpFbric)
    ENDIF
    SELECT (lcItemFile)
    *-- Restore the record pointer in the item file.
    IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcItemFile)
      GOTO lnCurRec
    ENDIF
  ENDSCAN
  SET ORDER TO CFABRIC IN FABRIC
  SET ORDER TO CSTYLE  IN STYLE
  *-- Restore the record pointer in the fabric file.
  IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
    GOTO lnFbrRec IN FABRIC
  ENDIF
  *-- Restore the record pointer in the style file.
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfvMfg
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Cost Items (Manufactering Operations)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvMfg()
*!*************************************************************
*
FUNCTION lfvMfg
PRIVATE lnAlias,lcOldMfg,lcNewMfg,lnRecNo

*-- Update the temp. bom file.
lnAlias = SELECT(0)
SELECT (lcTmpBom)
lcOldMfg = MFGCODE
lnRecNo = RECNO()
REPLACE MFGCODE WITH laMfgCode[lnNMfgCode,2];
        CSTATUS WITH IIF(CSTATUS = "S" , "M" , CSTATUS)
lcNewMfg = MFGCODE
REPLACE ALL FOR COPRCODE = lcOldMfg;
        COPRCODE WITH lcNewMfg,;
        CSTATUS  WITH IIF(CSTATUS = "S" , "M" , CSTATUS)
GOTO lnRecNo
SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
      cdefcode+ccode_no+crltfield+cfld_name="N"+MfgCode+"N"+"MFGCODE" AND;
      !EMPTY(Codes.cDiscrep) ;
FROM (lcTmpBom) , Codes ;
INTO ARRAY laOprCode 
IF !EMPTY(laOprCode[1,1])
  DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
  =AINS(laOprCode,1)
ENDIF
laOprCode[1,1] = "Select Operation"
laOprCode[1,2] = SPACE(6)
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

SELECT (lnAlias)


*!*************************************************************
*! Name      : lfvCstItmDs
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validation function item description field.
*!*************************************************************
*! Calls     :None
*!*************************************************************
*! Passed Parameters  : lcVar -> Define which description is passed
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvCstItmDs()
*!*************************************************************
*
FUNCTION lfvCstItmDs
PARAMETERS lcVar
PRIVATE lcVar

*-- Update the bom file.
REPLACE &lcTmpBom..DESC    WITH lcCstItmD&lcVar ;
        &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvOprCode
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for the valid Mfg. Opr. popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvOprCode()
*!*************************************************************
*
FUNCTION lfvOprCode

lcOprCode = laOprCode[puOprCode,2]

*-- Update the bom file.
REPLACE &lcTmpBom..cOprCode WITH lcOprCode ;
        &lcTmpBom..cStatus  WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvDuty
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for the dutyable popup.
*!*************************************************************
*! Calls     : lfUpdCost
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvDuty()
*!*************************************************************
*
FUNCTION lfvDuty

PRIVATE lnCurRec
lnCurRec   = RECNO(lcTmpBom)        && Save the BOM record pointer.
lcDutyable = laDutyable[ibDutyabl,2]
IF &lcTmpBom..cCatgTyp = "D" 
  IF lcDutyable = "1"
    ibDutyabl = 1
    SHOW GET ibDutyabl  &lcLinStat
    RETURN
  ENDIF
ENDIF

IF &lcTmpBom..cCatgTyp = "M" 
  IF lcDutyable = "2"
    IF &lcTmpBom..MfgCode= "MATERI"
      ibDutyabl = 1
      SHOW GET ibDutyabl  &lcLinStat
      RETURN
    ENDIF  
  ENDIF
ENDIF

IF !(lcDutyable == lcOldDuty)
  IF lcOldDuty $ "123"
    =lfUpdCost(-1 , 0)
  ENDIF
ENDIF
lnCurrDuty = 0
IF &lcTmpBom..cCatgTyp <> "D"
  SCAN FOR &lcTmpBom..cCatgTyp = "D"
    lnCurrDuty = &lcTmpBom..nPercent  
  ENDSCAN
ENDIF
*-- Restore record no.
IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
  GOTO lnCurRec IN (lcTmpBom)
ENDIF

*-- Update the temp. bom file.
REPLACE &lcTmpBom..cCostStat WITH lcDutyable ;
        &lcTmpBom..nDutyRate WITH lnCurrDuty ;
        &lcTmpBom..cStatus   WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
*-- Update the Duty cost items with all the dutyable cost items.
IF (lcDutyable $ "123" .OR. lcOldDuty = "123") .AND. !(lcDutyable == lcOldDuty)
  =lfUpdCost(IIF(lcDutyable $ "123",+1 ,-1) , 0)
ENDIF
IF !(lcDutyable $ "123")
  REPLACE &lcTmpBom..nDutyRate WITH 0
ENDIF

*!*************************************************************
*! Name      : lfvUnitQty
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validation function for Unit quantity in the modify mode
*!*************************************************************
*! Calls     : lfUpdCost
*!*************************************************************
*! Passed Parameters  : lcType-> Define which unit qty.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvUnitQty()
*!*************************************************************
*
FUNCTION lfvUnitQty
PARAMETERS lcType
PRIVATE lcType , lnBomRec

IF lcType = 'A'
  lnEstQtyB = lnEstQtyA
ELSE
  lnEstQtyA = lnEstQtyB
ENDIF

IF lnEstQtyA < 0
  *** Unit quantity must be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM38045B00000" , "DIALOG")
  lnEstQtyA = &lcTmpBom..nEstBomQty
  lnEstQtyB = &lcTmpBom..nEstBomQty
  _CUROBJ    = _CUROBJ
  RETURN
ENDIF

*-- Get the total cost.
lnUnitQtyA  = lnEstQtyA*(1+(lnWstg/100))
lnUnitQtyB  = CEILING(lnEstQtyB*(1+(lnWstg/100)))
lnTotFCst   = IIF(&lcTmpBom..cCatgTyp="S" , (CEILING(lnEstQtyB*(1+(lnWstg/100))) * lnUnitCst) , ((lnEstQtyA*(1+(lnWstg/100))) * lnUnitCst))

lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2)

*-- Save the old total cost before updating with the new one.
lnOld = &lcTmpBom..TotCost

*-- Update the temp. bom file.
REPLACE &lcTmpBom..nEstBomQty WITH lnEstQtyA ;
        &lcTmpBom..nBomTotQty WITH lnEstQtyA*(1+(&lcTmpBom..nBomWastge/100)) ;
        &lcTmpBom..TotCost    WITH lnTotFCst ;
        &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- If the current cost item is price call function to update
*-- all the manufacutered records that has percentage.
IF (&lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1') ;
   .AND. !(lnOld == &lcTmpBom..TotCost)

  =lfUpdCost(1 , lnOld)
  
ENDIF

*-- If style cost sheet only...
IF lcStyleTyp <> "T" .AND. !(lnOld == &lcTmpBom..TotCost)
  *-- Save current bom file record pointer.
  lnBomRec = RECNO()
  *-- Call local function to calculate the current parent cost in its
  *-- parent records...
  DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH &lcTmpBom..cItmMask
  *-- Restore record pointer in bom temp file.
  IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT()
    GOTO lnBomRec
  ENDIF
ENDIF

*-- Refresh the related objects.
IF lcType = 'A'
  SHOW GET lnUnitQtyA
ELSE
  SHOW GET lnUnitQtyB
ENDIF
SHOW GET lnTotFCst
SHOW GET lnTotEquCst

*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvWstg
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validation function for Estimated Unit quantity 
*!           : in the modify mode
*!*************************************************************
*! Calls     : lfUpdCost
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvWstg()
*!*************************************************************
*
FUNCTION lfvWstg
PRIVATE lnBomRec

*-- Get the total cost.
lnUnitQtyA  = &lcTmpBom..nEstBomQty * (1+(lnWstg/100))
lnUnitQtyB  = CEILING(&lcTmpBom..nEstBomQty * (1+(lnWstg/100)))
lnTotFCst   = IIF(&lcTmpBom..cCatGTyp="S" , ;
              (CEILING(&lcTmpBom..nEstBomQty*(1+(lnWstg/100))) * lnUnitCst),;
              ((&lcTmpBom..nEstBomQty*(1+(lnWstg/100))) * lnUnitCst))

lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2)

*-- Save the old total cost before updating with the new one.
lnOld = &lcTmpBom..TotCost

*-- Update the temp. bom file.
REPLACE &lcTmpBom..nBomWastge WITH lnWstg ;
        &lcTmpBom..nBomTotQty WITH IIF(&lcTmpBom..cCatGTyp="S" , ;
           CEILING(&lcTmpBom..nEstBomQty*(1+(&lcTmpBom..nBomWastge/100))),;
           &lcTmpBom..nEstBomQty*(1+(&lcTmpBom..nBomWastge/100)) ) ;
        &lcTmpBom..TotCost    WITH lnTotFCst ;
        &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- If the current cost item is price call function to update
*-- all the manufacutered records that has percentage.
IF (&lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1') ;
   .AND. !(lnOld == &lcTmpBom..TotCost)

  =lfUpdCost(1 , lnOld)
ENDIF

*-- If style cost sheet only...
IF lcStyleTyp <> "T" .AND. !(lnOld == &lcTmpBom..TotCost)
  *-- Save current bom file record pointer.
  lnBomRec = RECNO()
  *-- Call local function to calculate the current parent cost in its
  *-- parent records...
  DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH &lcTmpBom..cItmMask
  *-- Restore record pointer in bom temp file.
  IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT()
    GOTO lnBomRec
  ENDIF
ENDIF

*-- Refresh the related objects.
IF &lcTmpBom..cCatGTyp = "S"
  SHOW GET lnUnitQtyB
ELSE
  SHOW GET lnUnitQtyA
ENDIF
SHOW GET lnTotFCst
SHOW GET lnTotEquCst

*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvUom
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validation function UOM field.
*!*************************************************************
*! Calls     :None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvUom()
*!*************************************************************
*
FUNCTION lfvUom

*-- Update the temp. bom file.
REPLACE &lcTmpBom..UOM     WITH lcUom ;
        &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvUnitCst
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validation function for Unit cost in the modify mode.
*!*************************************************************
*! Calls     : lfUpdCost
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvUnitCst()
*!*************************************************************
*
FUNCTION lfvUnitCst

PRIVATE lnBomRec


SELECT (lcTmpBom)
*-- If new value equal the old value, return.
IF lnUnitCst = UntCost
  RETURN
ENDIF

IF lnUnitCst = 0 
  *IF lcStyleTyp = 'I' .AND. &lcTmpBom..cCatgTyp $ 'MD' .AND. (lcPricCur = lcDutyCur)
  IF lcStyleTyp = 'I' .AND. &lcTmpBom..cCatgTyp $ 'MD' 
  
    *-- If the unit cost is zero for imported style for cost type 
    *-- mfg operation or duty & the price currency the same as the
    *-- duty currency, enable the cost percentage.
    SHOW GET lnCstPerc ENABLE
    _CUROBJ  = OBJNUM(lnCstPerc)
  ELSE
    *** Unit {cost} must be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("INM38019B00000" , "DIALOG" , "cost")
    _CUROBJ  = _CUROBJ
    SHOW GET lnCstPerc DISABLE
  ENDIF
  RETURN
ELSE
  lnCstPerc = 0
  SHOW GET lnCstPerc DISABLE
ENDIF

*-- Recalculate total cost and equevlent cost.
lnTotFCst   = (lnEstQtyA*(1+(lnWstg/100))) * lnUnitCst

lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,2)

*-- Save the old total cost before updating with the new one.
lnOld = &lcTmpBom..TotCost

*-- Update unit cost and total cost.

REPLACE &lcTmpBom..nPercent   WITH lnCstPerc ;
        &lcTmpBom..UntCost    WITH lnUnitCst ;
        &lcTmpBom..TotCost    WITH lnTotFCst ;
        &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

IF &lcTmpBom..cCatgTyp $ 'MD' 
SELECT (lcTmpBom)           
  lnRecNo = RECNO(lcTmpBom)           
  

  SELECT (lcTmpBom)
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF
 
ENDIF
*-- If the current cost item is price call function to update
*-- all the manufacutered records that has percentage.
IF (&lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1') ;
   .AND. !(lnOld == &lcTmpBom..TotCost)

  =lfUpdCost(1 , lnOld)
ENDIF

*-- If style cost sheet only...
IF lcStyleTyp <> "T" .AND. !(lnOld == &lcTmpBom..TotCost)
  *-- Save current bom file record pointer.
  lnBomRec = RECNO()
  *-- Call local function to calculate the current parent cost in its
  *-- parent records...
  DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH &lcTmpBom..cItmMask
  *-- Restore record pointer in bom temp file.
  IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT()
    GOTO lnBomRec
  ENDIF
ENDIF

*-- Refresh the related objects.
SHOW GET lnTotFCst
SHOW GET lnTotEquCst
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvCstPrc
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valide function for cost percent for Duty and 
*!             Manufactuing operations while modify cost item.
*!*************************************************************
*! Calls     : lfUpdPerc
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvCstPrc()
*!*************************************************************
*
FUNCTION lfvCstPrc
PRIVATE lnCurRec , lcType

SELECT (lcTmpBom)
*-- If the percent was zero, update the bom file.
IF lnCstPerc = 0
  *-- Update cost percent, unit cost and total cost
  REPLACE &lcTmpBom..nPercent WITH lnCstPerc ;
          &lcTmpBom..cStatus  WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)
  
  *-- Disable the cost percentage & enable the unit cost object.
  SHOW GET lnCstPerc  DISABLE
  SHOW GET lnUnitCst  ENABLE
  _CUROBJ = OBJNUM(lnUnitCst)
  RETURN
ENDIF

*-- Update unit cost and total cost.
REPLACE &lcTmpBom..nPercent WITH lnCstPerc ;
        &lcTmpBom..cStatus  WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- For duty, Get cost as a percent of dutiable items.
*-- For manufacturin operations Get cost as a percent of the price.
lcType     = &lcTmpBom..cCatgTyp
IF lcType $ "MD" .AND. lnCstPerc <> 0 .AND. !(lnOldPerc == lnCstPerc)

  =lfUpdPerc(lcType)
ENDIF

*-- Recalculate total and equevlent cost
lnUnitCst   = &lcTmpBom..UntCost
lnTotFCst   = &lcTmpBom..TotCost

lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2)

*-- Refresh the related objects.
SHOW GET lnUnitCst 

SHOW GET lnTotFCst
SHOW GET lnTotEquCst
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lfvMarker
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function of Marker field.
*!*************************************************************
*! Example   : = lfvMarker()
*!*************************************************************
*!
FUNCTION lfvMarker

*PRIVATE lnAlias, lcFile_Ttl, lcOrder, lcOldBrwFl, lcPattern
*DIMENSION laTempData[1]
*STORE '' TO laTempData, lcPattern

*lnAlias = SELECT(0)

*IF TYPE('laAStySeg') # 'U'
*  IF !EMPTY(laAStySeg[1,1])
*    llReturn = (lcOldMrker == lcMarker7)
*  ELSE
*    llReturn = (lcOldMrker == lcMarker6)
*  ENDIF
*ELSE
*  llReturn = (lcOldMrker == lcMarker)
*ENDIF


*IF FILE(gcDataDir+"PDMMARK"+".DBF") .AND. !llReturn
*  =gfOpenFile(gcDataDir+"PDMMARK" , gcDataDir+"CFABRIC" , "SH")
*ELSE
*  RETURN
*ENDIF

*SELECT STYLE
*lcOrder = SET('ORDER')
*SET ORDER TO TAG CSTYLE
*SEEK(PADR(laData[1],19))
*SET ORDER TO &lcOrder

*SELECT PDMMARK
*IF TYPE('laAStySeg') # 'U'
*  =SEEK(PADR(lcNFabItem,7))
*ELSE
*  =SEEK(PADR(lcCostItmF,7))
*ENDIF

*lcOldBrwFl = lcBrFields 

*lcFile_Ttl = 'Markers'
*lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
*             "CMARK_DESC                  :H='Description',"+;
*             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
*             "NTOTQTY                     :H='Gross Unit'"

*IF TYPE('laAStySeg') # 'U'
*  IF !EMPTY(laAStySeg[1,1])
*    lcMarker = lcMarker7
*  ELSE
*    lcMarker = lcMarker6
*  ENDIF
*ENDIF

*IF !EMPTY(lcMarker)
*  IF EOF()
*    =gfModalGen("INM000000B00000","DIALOG",'','',"No markers have been defined for this fabric.")
   
*    IF TYPE('laAStySeg') # 'U'
*      IF !EMPTY(laAStySeg[1,1])
*        lcMarker7 = ''
*        SHOW GET lcMarker7
*      ELSE
*        lcMarker6 = ''
*        SHOW GET lcMarker6
*      ENDIF
*    ENDIF
   
*  ELSE
*    =SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
*          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
*          RIGHT(ALLTRIM(STYLE.PATTERN),2))
*    IF EOF()
*      =AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))],;
*                lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
*      lcMarker = IIF(TYPE('laAStySeg') # 'U',laTempData[1],;
*                 IIF(EMPTY(laTempData[1]),&lcTmpBom..cMarker,laTempData[1]))

*      lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No

*    ELSE
*      IF !SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
*          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
*          RIGHT(ALLTRIM(STYLE.PATTERN),2)+lcMarker)
*        =AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))+;
*                  PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
*                  RIGHT(ALLTRIM(STYLE.PATTERN),2)],;
*                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')

*        lcMarker = IIF(TYPE('laAStySeg') # 'U',laTempData[1],;
*                   IIF(EMPTY(laTempData[1]),&lcTmpBom..cMarker,laTempData[1]))

*        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No

*      ELSE
*        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
*      ENDIF
*    ENDIF
*  ENDIF
*ENDIF

*lcBrFields = lcOldBrwFl 

*IF TYPE('laAStySeg') # 'U'
*  IF !EMPTY(laAStySeg[1,1])
*    lcMarker7 = lcMarker
*  ELSE
*    lcMarker6 = lcMarker
*  ENDIF
*ELSE
*  SELECT (lcTmpBom)
*  REPLACE cMarker WITH lcMarker,;
*          cStatus WITH IIF(cStatus = "S" , "M" , cStatus)
*ENDIF

*IF !EMPTY(lcMarker) .AND. !EOF('PDMMARK') .AND.;
*   (PDMMARK.NTOTGOODS<>0 .OR. PDMMARK.NLOSS<>0 .OR. PDMMARK.NTOTQTY<>0) .AND.;
*   gfModalGen("QRM00000B00006","DIALOG",'','',"Do you want to update the units from marker data ?")=1

*  IF TYPE('laAStySeg') # 'U'
*    IF !EMPTY(laAStySeg[1,1])
*      lnNEstUnt7 = PDMMARK.NTOTQTY
*      lnNWstg7   = 0
*      lnNUntQty7 = PDMMARK.NTOTQTY
*      SHOW GET lnNEstUnt7
*      SHOW GET lnNWstg7
*      SHOW GET lnNUntQty7
*      =lfvNUntQty('7')
*    ELSE
*      STORE PDMMARK.NTOTQTY   TO lnNEsUnt6B, lnNEsUnt6A
*      STORE 0                 TO lnNWstg6
*      STORE PDMMARK.NTOTQTY   TO lnNUntQt6B, lnNUntQt6A
*      SHOW GET lnNEsUnt6B
*      SHOW GET lnNEsUnt6A
*      SHOW GET lnNWstg6
*      SHOW GET lnNUntQt6B
*      SHOW GET lnNUntQt6A
*      =lfvNUntQty('6')
*    ENDIF
*  ELSE
*    =SEEK(PADR(lcCostItmF,7)+lcPattern+lcMarker,'PDMMARK')
*    STORE PDMMARK.NTOTQTY   TO lnEstQtyB, lnEstQtyA
*    STORE 0                 TO lnWstg
*    STORE PDMMARK.NTOTQTY   TO lnUnitQtyB, lnUnitQtyA
*    SHOW GET lnEstQtyB
*    SHOW GET lnEstQtyA
*    SHOW GET lnWstg
*    SHOW GET lnUnitQtyB
*    SHOW GET lnUnitQtyA
*    =lfvUnitQty('A')
*  ENDIF
*  IF ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
*    =gfDoTriger('MFSCTSH',PADR('UPDBOMCA',10))
*  ENDIF
*ENDIF
*IF EMPTY(lcMarker) .AND. ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
*  =SEEK(SPACE(27),'PDMMARK')
*  =gfDoTriger('MFSCTSH',PADR('UPDBOMCA',10))
*ENDIF

*=gfCloseFile('PDMMARK')

*SELECT (lnAlias)
*-- end of lfvMarker.

*!*************************************************************
*! Name      : lfvSearch
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for the search button.
*!*************************************************************
*! Calls     : MFSerch.SPX, lfShowDet
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvSearch()
*!*************************************************************
*
FUNCTION lfvSearch

*-- Flag to know if press ok or not.
llPressOk = .F.

*-- Arrays hold the popups value.
DECLARE laSCstTyp[1,1] , laSItmMsk[1,1] , laSItems[1,1] , laSItmClr[1,1]
*-- Default the popup values to 1.
STORE 1 TO puSCstTyp , puSItmMsk , puSItems , puSItmClr

*-- Save the current record in the temp. bom file.
lnSrchRec = RECNO(lcTmpBom)

*-- select all the available types in the temp. bom file.
SELECT DISTINCT SPACE(1) , Typ ;
  FROM (lcTmpBom) ;
  INTO ARRAY laSCstTyp

*-- Get the labels for all the selected types.
FOR lnCount = 1 TO ALEN(laSCstTyp,1)
  laSCstTyp[lnCount,1] = EVALUATE("lc"+lcStyleTyp+"SLBL"+laSCstTyp[lnCount,2])
ENDFOR

lcSrType   = laSCstTyp[1,2]   && Default to the first type.
lcSrCstTyp = laSCstTyp[1,1]   && Default to the first label.

*-- Select the available masks for the first cost type.
SELECT DISTINCT citmmask ;
  FROM (lcTmpBom) ;
 WHERE Typ = lcSrType ;
  INTO ARRAY laSItmMsk ;
  GROUP BY citmmask

*-- Default to the first maks.
lcSrItmMsk = IIF(_Tally > 0 , laSItmMsk[1,1] , SPACE(19))

*-- Select MfgCode or Item field according to the current type.
lcField = IIF(EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD","MFGCODE","ITEM")
SELECT &lcField ;
  FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk ;
  INTO ARRAY laSItems ;
 GROUP BY &lcField

*-- Default to the first item.
lcSrItem   = IIF(_Tally > 0 , laSItems[1,1] , SPACE(19))

*-- Select the available colors according to the current item.
SELECT IClr ;
  FROM (lcTmpBom) ;
 WHERE Typ  = lcSrType .AND. ;
       citmmask = lcSrItmMsk .AND. ;
       Item = lcSrItem ;
  INTO ARRAY laSItmClr ;
 GROUP BY IClr

*-- Default to the first color.
lcSrItmClr = IIF(_Tally > 0 , laSItmClr[1,1] , SPACE(6))

*-- Call the search screen.
DO (gcScrDir+"MFSerch.SPX")

*-- If back from the search screen with cancel, go to previous record.
IF !llPressOk
  IF lnSrchRec > 0 .AND. lnSrchRec <= RECCOUNT(lcTmpBom)
    GOTO lnSrchRec IN (lcTmpBom)
  ENDIF
ENDIF

*-- Refresh the browse & its objects in the main screen.
=lfShowDet()

*!*************************************************************
*! Name      : lfvDetOpr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid Function for detail Operation Buttons.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDetOpr()
*!*************************************************************
*!
FUNCTION lfvDetOpr

*--- This function called only if PW module are instaled and
*--- Current style is Mf style
*--- if the current Cost Item is not mfgCode or has't detail operation
*--- the button will be disable.
IF llPwInst  .AND. lcStyleTyp = 'M'
  PRIVATE lnOldAls
  lnOldAls  = SELECT(0)
  lcMfgCode = laMfgCode[lnNMfgCode,2]
  *--- Call this function to get data and create temp files.
  =lfGetData(lcMfgCode)
  PUSH KEY
  ON KEY
  *--- Call detail operation program from the shared prgs directory
  *--- with the following parameter
  *--- Mfg Code , TempFile name Which will hold detail operaiton data,
  *--- name of variable that hold the MfgCode Cost,
  *--- The last parameter .T. if called from the Style Cost Sheet screen
  *--- .F. if from the New Cost Item Screen.
  DO (gcAppHome+"MFDetOpr") WITH lcMfgCode , lcDetLin , 'lnUnitCst' , .T.
  POP KEY
  *--- Refresh main browse
  IF !WEXIST(lcBrCstShT)
    =lfBrwCstSh()
  ENDIF
  SELECT(lnOldAls)
  *--- This function is one of style cost sheet functions that valid 
  *--- the unit cost .
  *--- so after runnig detail operation program which will calculate
  *--- MfgCode unit Cost we will call this function to valid the calculate
  *--- unit cost the same standard calculation. i.e. we did not change
  *--- any thing in this function.
  =lfvUnitCst()
  *--- Refresh unit cost Object.
  SHOW GET lnUnitCst DISABLE
  SELECT(lnOldAls)
ENDIF

*!*************************************************************
*! Name      : lfvRemEle
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Delete element from cost sheet.
*!*************************************************************
*! Calls     : lfUpdCost, lfShowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvRemEle()
*!*************************************************************
*
FUNCTION lfvRemEle
PRIVATE lnCurRec

*** Are you sure you want to delete this Cost Sheet line? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM38009B00006" , "DIALOG") = 1
  SELECT (lcTmpBom)
  *-- If the deleted record was mfg operation.
  IF &lcTmpBom..cCatgTyp = "M"
    *-- Save the current record pointer in the temp. bom file.
    lnCurRec  = RECNO(lcTmpBom)
    *-- Search in the temp. bom file for cost item use the deleted 
    *-- mfg operation as parent operation.
    lcLoctExp = &lcTmpBom..MfgCode
    LOCATE FOR cOprCode = lcLoctExp
    *-- Give the user a warning message that this mfg operation is used 
    *-- as a parent in other cost items.
    IF FOUND()
      *** This MFG operation will be released from all the cost items. ***
      *** < Ok > ***
      =gfModalGen("INM38063B00000" , "DIALOG")
      *-- Update the temp. bom file.
      REPLACE ALL &lcTmpBom..cOprCode WITH "" ;
                  &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus) ;
              FOR &lcTmpBom..cOprCode = lcLoctExp
      
      *-- Remove the current mfg operation from the operations array.
      IF ASCAN('laOprCode',lcLoctExp) > 0
        lnDelElm = ASUBSCRIPT('laOprCode',ASCAN('laOprCode',lcLoctExp),1)
        
        *-- Delete the array row from the MFG operations array.
        =ADEL(laOprCode,lnDelElm)
        IF ALEN(laOprCode,1) > 1
          DIMENSION laOprCode [ALEN(laOprCode,1) -1,2]
        ENDIF
      ENDIF
    ENDIF
    *-- Restore the record pointer in the bom file.
    IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
      GOTO lnCurRec
    ENDIF
  ENDIF
  *-- If there is percent in any MFG or Duty cost item, calculate the
  *-- cost from the participated items.
  IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
    =lfUpdCost(-1 , 0)
  ENDIF
  
  *-- Save the current parent mask of the deleted record.
  lcDelMask = &lcTmpBom..cItmMask
  
  llEnblSize = IIF(!EMPTY(mSizes) OR cCatgTyp <> "S" , .T. , .F.)
  IF llPwInst .AND. lcStyleTyp = 'M'
    =lfPwRem()
  ENDIF

  IF &lcTmpBom..lprimary
    llChckFrst = .F.
    IF &lcTmpBom..lFrstprm 
      REPLACE &lcTmpBom..lFrstprm    WITH .F.
      llChckFrst = .T.
    ENDIF
    *-- divid by No of primary fabrics
    lnRecNo = RECNO(lcTmpBom)           
    lcCurItem = ALLTRIM(Item)
    lcIclr    = Iclr
    lnCntRec = 0
    lnTotQty = 0
    lnCurWastg = 0
    lnCount  = 1
    SELECT (lcTmpBom)
    DIMENSION laFabric[1,2]
    SCAN 
      IF &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND lcIclr = Iclr

        IF  ASCAN(laFabric,ALLTRIM(Item))=0
          DIMENSION laFabric[lnCount,2]
          laFabric[lnCount,1]   = ALLTRIM(Item)
          laFabric[lnCount,2]   = 1
          lnCount = 1 +lnCount
          lnTotQty = IIF(!EMPTY(&lcTmpBom..nPrUntQty),&lcTmpBom..nPrUntQty,lnTotQty)
          lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
        ELSE
          laFabric[lnCount-1,2]   = laFabric[lnCount-1,2] + 1
          lnTotQty = IIF(!EMPTY(&lcTmpBom..nPrUntQty),&lcTmpBom..nPrUntQty,lnTotQty)
          lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
        ENDIF  
      ENDIF
    ENDSCAN
    *-- divid by No of primary fabrics
    *lnRecNo = RECNO(lcTmpBom)           
    lnAscn = ASCAN(laFabric,ALLTRIM(lcCurItem))
    lnPos = IIF(lnAscn>1,CEILING(lnAscn/2),1)
    IF laFabric[lnPos,2] >1
      lnCntRec = ALEN(laFabric,1)
    ELSE
      lnCntRec = ALEN(laFabric,1)-1
    ENDIF  
  
    SELECT (lcTmpBom)
    IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
      GOTO lnRecNo
    ENDIF
    REPLACE &lcTmpBom..lprimary    WITH .F.,;
            &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
    SELECT (lcTmpBom)
    LOCATE
    SCAN FOR ccatgtyp = 'F'
      IF &lcTmpBom..lprimary AND lcIclr = Iclr
        REPLACE &lcTmpBom..nEstBomQty WITH (lnTotQty*100/(lnCurWastg+100))/IIF(lnCntRec=0,1,lnCntRec),; 
                &lcTmpBom..nbomtotqty WITH (lnTotQty/IIF(lnCntRec=0,1,lnCntRec)),;
                &lcTmpBom..TotCost    WITH &lcTmpBom..untcost*&lcTmpBom..nEstBomQty,;
                &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
      ELSE
        REPLACE &lcTmpBom..npruntqty WITH 0
      ENDIF  
    ENDSCAN
    SELECT (lcTmpBom)
    IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
      GOTO lnRecNo
    ENDIF
  ENDIF
  
  IF &lcTmpBom..ccoststat $ "123"
    =lfUpdCost(-1 , 0)
  ENDIF

  *-- Change the status to be deleted & delete the record.
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  *-- If style cost sheet only...
  IF lcStyleTyp <> "T"
    *-- Call local function to calculate the current parent cost in its
    *-- parent records...
    DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH lcDelMask
  ENDIF
  
  IF llEnblSize
    LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
    llOnSize  = IIF(FOUND() , .T. , .F. )
    lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
    SHOW GET cbOnSize &lcBasStat
    *-- Restore the record pointer in the bom file.
  ENDIF
  GO TOP

  *-- Subtract 1 from the bom records no.
  *lnBomLnNo = lnBomLnNo - 1
  SELECT (lcTmpBom)
  COUNT TO lnBomLnNo FOR cStatus <>'D'
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
   GOTO lnRecNo
  ENDIF

  *-- Update the enabling & disabling variables in case there is 
  *-- no lines in the browse at all.
  lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
  lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" ) 
  *-- Call local function to refresh the objects in the main screen 
  *-- according to the current cost items.
  =lfBrwCstSh()
ENDIF

*!*************************************************************
*! Name      : lfCostData
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : When function of Marker field.
*!*************************************************************
*! Example   : = lfCostData()
*!*************************************************************
*!
FUNCTION lfCostData

STORE gcBaseCurr TO lcPricCur , lcDutyCur
llFrmSelct    = .T.  && Flag to know if coming from select mode.
llNewEntry    = .F.

lcPrimFbr     = " "    && Variable hold the primary fabric.
DECLARE laOprCode[1,2]
laOprCode[1,1] = "Select Operation"
laOprCode[1,2] = SPACE(6)
STORE .F. TO cbOnSize , cbSelSize , llEdit
lnBomLnNo     = 0      && Var. hold no. of lines in the BOM Browse.
STORE "DISABLE" TO lcBasStat , lcObjStat , lcLinStat , lcSizStat , lcSrcStat
    
    *-- Blank all the available temp. files.
SELECT (lcTmpFbric)
ZAP
SELECT (lcTmpStyle)
ZAP
SELECT (lcTmpBom)
ZAP

lcPrimFbr = " "    && Variable hold the primary fabric.
*-- Disable all the objects in the main screen.
STORE "DISABLE" TO lcBasStat , lcObjStat , lcLinStat , lcSizStat
STORE .F. TO llNewEntry , llEdit
*-- Fill the description.
lcItemDesc = IIF(lcStyleTyp = 'T' , &lcItemFile..Desc , &lcItemFile..Desc1)
*-- See if the item with dyelots yes or no.
llStyDye   = (&lcItemFile..cDye_Flg ='Y')
*-- Get the duty & mfg operation currency for the current style.
lcPricCur  = IIF(lcStyleTyp = "I" , Style.cPriceCur , gcBaseCurr)
lcDutyCur  = IIF(lcStyleTyp = "I" , Style.cDutyCur  , gcBaseCurr)
    
*-- Get the cost items for the current item.
STORE 0 TO lnBomLnNo
WAIT 'Preparing the work area ......' WINDOW NOWAIT
SELECT *,RECNO() AS nRecNo , "S" AS cStatus ;
  FROM BOM ;
  WHERE citmmajor+typ+citmmask+mfgcode+item+iclr= lcStyMaj .AND. IIF(lcStyleTyp='T' , lMaterial , !lMaterial) ;
   INTO DBF (gcWorkDir+lcTmpBom)  
lnBomLnNo  = _TALLY
*-- Call the browse function to refresh the browse with right records.
SELECT (lcTmpBom)
INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)

*-- If there is lines for the current cost sheet, get the available 
*-- Mfg operations from the lines.
IF lnBomLnNo > 0
  DECLARE laOprCode[1,2]
  laOprCode = ""
  SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
  WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
      cdefcode+ccode_no+crltfield+cfld_name="N"+MfgCode+"N"+"MFGCODE" AND;
      !EMPTY(Codes.cDiscrep) ;
  FROM (lcTmpBom) , Codes ;
  INTO ARRAY laOprCode 

  IF !EMPTY(laOprCode[1,1])
    DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
    =AINS(laOprCode,1)
  ENDIF
  laOprCode[1,1] = "Select Operation"
  laOprCode[1,2] = SPACE(6)
      
  *-- See if there is at least 1 record has sizes to be based on sizes.
  LOCATE FOR &lcTmpBom..lBasOnSiz
  cbOnSize = IIF(FOUND() , .T. , .F.)
  GO TOP
  SHOW GET pbDlt ENABLE
  laCtrStat[8]  = "ENABLE"    && Delete button
  *-- Enable the notes & the costing buttons if there is lines in the cost sheet.
  SHOW GET pbNotes   ENABLE
  SHOW GET pbCosting ENABLE
  lcSrcStat = "ENABLE"
      
    ELSE
      cbOnSize   = .F.
      SHOW GET pbDlt DISABLE
      laCtrStat[8]  = "DISABLE"    && Delete button
      *-- Disable the notes & the costing buttons if there is no lines in the cost sheet.
      SHOW GET pbNotes   DISABLE
      SHOW GET pbCosting DISABLE
      lcSrcStat = "DISABLE"

    ENDIF
    
    SHOW GET pbSearch &lcSrcStat
    WAIT CLEAR
    
    *-- Set the select mode flag to false.
    llFrmSelct = .F.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Update Style/Material Cost Sheet.
*!*************************************************************
*! Calls     : lpDelScr, lfCostInfo, gfTmp2Mast
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr
PRIVATE lnItemRec , lnFbrRec , lnStyRec

SELECT (lcTmpBom)
SET FILTER TO 
LOCATE
*-- If the BOM file is empty.
IF EOF(lcTmpBom)
  *  *** Nothing updated to the Cost Sheet. ***
  *  *** <  Ok  > ***
  =gfModalGen("INM38053B00000" , "DIALOG")
ELSE
  WAIT 'Updating the costing information to the Order cost sheet file' WINDOW NOWAIT
  *SELECT (lcOrdLine)
  lnFbrRec = RECNO("FABRIC")     && Save record pointer in the fabric file.
  lnStyRec = RECNO("STYLE")      && Save record pointer in the style file.
  
  *-- Clear the filter in the current item file (Style or fabric).
  SELECT (lcItemFile)
  SET FILTER TO
  
  *-- Call local function to calculate the cost.
  =lfCostInfo()
  
  *-- Restore the record pointer in the fabric file.
  IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
    GOTO lnFbrRec IN FABRIC
  ENDIF
  *-- Restore the record pointer in the style file.
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF

  *-- Save the current record pointer in the fabric file.
  lnFbrRec = RECNO("FABRIC")
  *-- Change the fabric file index.
  SET ORDER TO FABRIC IN FABRIC
  *-- Check if any fabrics to be add to the master fabric file
  SELECT (lcTmpFbric)
  SCAN
    *-- If the fabric color was not found, add it to the fabric file.
    IF !SEEK(Fabric+Color , 'Fabric')
      SCATTER MEMVAR
      SELECT FABRIC
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
  ENDSCAN
  *-- Blank the temp. fabric file.
  ZAP
  *-- Restore the index in the fabric file.
  SET ORDER TO CFABRIC IN FABRIC
  *-- Restore the record pointer in the fabric file.
  IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
    GOTO lnFbrRec IN FABRIC
  ENDIF

  
  SELECT (lcOrdSty)
  lnOrdCost = 0
  SCAN 
    SELECT (lcTmpBom)
    SET DELE OFF
    SELECT (lcTmpBom)
    SET FILTER TO 

    SCAN FOR cItmMajor = &lcOrdSty..Style
      SCATTER MEMVAR MEMO
      IF SEEK(citmmajor+typ+citmmask+mfgcode+item+iclr,'BOM')      
        m.nRecNo = RECNO("BOM")
        IF m.cstatus <> "D"
          m.cstatus = "M"
        ENDIF  
      ENDIF
      IF cstatus <> "D"
        lnOrdCost = lnOrdCost +TotCost
      ENDIF
      *--line no not working correctly now
      m.lineno  = &lcOrdLine..lineno
      INSERT INTO (lcAllBom) FROM MEMVAR
      REPLACE Order  WITH laData[1],;
              lineno WITH &lcOrdLine..lineno
    ENDSCAN
    SET DELE ON
    SELECT ORDLINE
    *--Update Ordline.cost
    =SEEK(&lcOrdLine..cOrdType+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6))
    REPLACE OrdLine.Cost WITH lnOrdCost
    
    SELECT (lcAllBom)
    
    DELETE ALL FOR cstatus = "D"
    =gfTmp2Mast('BOM' , lcAllBom, 'Saving cost sheet items for : ' + &lcOrdLine..Style + ' ...')
  ENDSCAN
  =gfTmp2Mast('ORDBOM' , lcTmpBom, 'Saving sales order cost sheet items for : ' + laData[1] + ' ...')
ENDIF  
SELECT (lcTmpBom)
PACK
*!*************************************************************
*! Name      : lfvSCstTyp
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for cost type popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSCstTyp()
*!*************************************************************
*
FUNCTION lfvSCstTyp

*-- Get the cost type description & its value from its popup.
lcSrCstTyp = laSCstTyp[puSCstTyp,1]
lcSrType   = laSCstTyp[puSCstTyp,2]

*-- Select the available masks for the first cost type.
SELECT citmmask ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType ;
  INTO ARRAY laSItmMsk ;
 GROUP BY citmmask

*-- Default to the first mask.
lcSrItmMsk = IIF(_Tally > 0 , laSItmMsk[1,1] , SPACE(19))
puSItmMsk  = 1

*-- Select MfgCode or Item field according to the current type.
lcField = IIF(EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD","MFGCODE","ITEM")
SELECT &lcField ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk ;
  INTO ARRAY laSItems ;
  GROUP BY &lcField

*-- Default to the first item.
lcSrItem = IIF(_Tally > 0 , laSItems[1,1] , SPACE(19))
puSItems = 1

*-- Select the available colors for the first item.
SELECT IClr ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk .AND. ;
       Item = lcSrItem ;
  INTO ARRAY laSItmClr ;
 GROUP BY IClr

*-- Default to the first color.
lcSrItmClr = IIF(_Tally > 0 , laSItmClr[1,1] , SPACE(6))
puSItmClr  = 1

*-- Refresh the mask & the items popup.
SHOW GET puSItmMsk
SHOW GET puSItems

*-- Refresh the mask & the item colors popups.
IF EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPDS"
  SHOW GET puSItmClr DISABLED
ELSE
  SHOW GET puSItmClr ENABLED
ENDIF

*!*************************************************************
*! Name      : lfvSItmMsk
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for item mask popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSItmMsk()
*!*************************************************************
*
FUNCTION lfvSItmMsk

*-- Get the selected mask from its popup.
lcSrItmMsk = laSItmMsk[puSItmMsk,1]

*-- Select MfgCode or Item field according to the current type.
lcField = IIF(EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD","MFGCODE","ITEM")
SELECT &lcField ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk ;
  INTO ARRAY laSItems ;
  GROUP BY &lcField

*-- Default to the first item.
lcSrItem = IIF(_Tally > 0 , laSItems[1,1] , SPACE(19))
puSItems = 1

*-- Select the available colors for the current item.
SELECT IClr ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk .AND. ;
       Item = lcSrItem ;
  INTO ARRAY laSItmClr ;
 GROUP BY IClr

*-- Default to the first item color.
lcSrItmClr = IIF(_Tally > 0 , laSItmClr[1,1] , SPACE(6))
puSItmClr  = 1

*-- Refresh the items popup & the items colors popup.
SHOW GET puSItems
SHOW GET puSItmClr

*!*************************************************************
*! Name      : lfvSItems
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for items popup
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSItems()
*!*************************************************************
*
FUNCTION lfvSItems

*-- Get the selected item value.
lcSrItem = laSItems[puSItems , 1]

*-- Select the available colors for the selected item.
SELECT IClr ;
 FROM (lcTmpBom) ;
 WHERE Typ = lcSrType .AND. ;
       citmmask = lcSrItmMsk .AND. ;
       Item = lcSrItem ;
  INTO ARRAY laSItmClr ;
 GROUP BY IClr

*-- Default to the first color.
lcSrItmClr = IIF(_Tally > 0 , laSItmClr[1,1] , SPACE(6))
puSItmClr  = 1
*-- Refresh the colors popup.
SHOW GET puSItmClr

*!*************************************************************
*! Name      : lfvSItmClr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for item colors popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSItmClr()
*!*************************************************************
*
FUNCTION lfvSItmClr

*-- Get the selected color.
lcSrItmClr = laSItmClr[puSItmClr,1]

*!*************************************************************
*! Name      : lfvSrOk
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for push button <ok> in the search screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSrOk()
*!*************************************************************
*
FUNCTION lfvSrOk

*-- Set flag to know that the OK button was pressed.
llPressOk = .T.

*-- Blank the item color field if the cost item type was (Mfg , Price or duty)
IF EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD"
  lcSrItmClr = SPACE(6)
ENDIF

*-- Seek for the selected criteria in the temp. bom file.
SELECT (lcTmpBom)
IF !SEEK(PADR(lcStyMaj,19) + lcSrType + PADR(lcSrItmMsk,19) + ;
         IIF(EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD" , ;
             PADR(lcSrItem,6) , SPACE(6) + PADR(lcSrItem,19) + lcSrItmClr))
  *-- If the record not found, go back to the previous saved record.
  IF lnSrchRec > 0 .AND. lnSrchRec <= RECCOUNT(lcTmpBom)
    GOTO lnSrchRec
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGtCstSht
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Get Cost Sheet for Order Style 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGtCstSht()
*!*************************************************************
*
FUNCTION lfGtCstSht

SELECT ORDBOM
SET ORDER TO ORDBOM
SELECT (lcOrdSty)
SCAN
  IF SEEK(&lcOrdSty..Order,'ORDBOM')
    SELECT ORDBOM
    SCAN REST WHILE Order+STR(lineno,6)+style=&lcOrdSty..Order
      IF  !(ALLTRIM(cItmMajor) == ALLTRIM(PADR(&lcOrdSty..Style,lnMajorLen)))
        LOOP
      ENDIF
      SCATT MEMVAR MEMO
      m.nRecNo  = RECNO()
      m.cStatus = "S"
      INSERT INTO (lcBomTemp) FROM MEMVAR
    ENDSCAN
    *--get data from Order BOM into lcBomTemp 
  ELSE
    *--get data from Last Order for the style  BOM into lcBomTemp 

    SELECT ORDBOM
    lcOldOrder = ORDER('ORDBOM')
    SET ORDER TO ORDBOM DESCENDING
    
    LOCATE FOR  ALLTRIM(PADR(Style,lnMajorLen))= ALLTRIM(PADR(&lcOrdSty..Style,lnMajorLen))
    IF !EOF()
      lcCurOrd = Order
      lnCurLine = LineNo
      lcCurrSty = ALLTRIM(&lcOrdSty..style)

      SELECT ORDBOM
          
      SCAN REST WHILE Order+STR(lineno,6)+style=lcCurOrd
        IF  ALLTRIM(cItmMajor) <> ALLTRIM(PADR(&lcOrdSty..Style,lnMajorLen)) 
          LOOP
        ENDIF
        
        IF SUBSTR(cItmMask, lnColorStr , lnColorLen) <>'******'
          IF !lfChkColor(SUBSTR(cItmMask, lnColorStr , lnColorLen))
            LOOP
          ENDIF
        ENDIF
        SCATT MEMVAR MEMO
        m.cStatus = "A"
        INSERT INTO (lcBomTemp) FROM MEMVAR
      ENDSCAN
      *--lcBomTemp 
    ELSE
      *-- get data from BOM file into lcBomTmp 
      
      IF SEEK(PADR(&lcOrdSty..style,lnMajorLen),'BOM')
        =lfGetBom()
      ELSE
        *--Write message to get Style cost sheet 
        
        IF laScrMode[3]
          lcMsg2 = 'Style Cost Sheet for Style ' +ALLTRIM(PADR(&lcOrdLine..Style,lnMajorLen))+ ' does not exist '
          lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        ENDIF
          
      ENDIF  
    ENDIF
    SET ORDER TO &lcOldOrder IN ORDBOM
  ENDIF
ENDSCAN  
lnBomLnNo = RECCOUNT(lcBomTemp)

*!*************************************************************
*! Name      : lfGetBom
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Get Cost Sheet for Style 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetBom()
*!*************************************************************
*
FUNCTION lfGetBom

DECLARE laItemSeg[1],laReq[8]

PRIVATE    lcMfgGlAcnt,lcExSign,lcUntSin,lnAlias,lcPriceCur,lnPriceRate,;
           lnPriceUnit,lcDutyCur,lnDutyRate,lnDutyUnit, lcCostElem,lnOldTrnQt

STORE ''  TO M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC
=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',gcAct_Comp)

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrMsk = laItemSeg[lnCount,3]
    lnClrPos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='S'
    lnSizePos = laItemSeg[lnCount,4]
  ENDIF
ENDFOR

*m.Style = &lcOrdLine..Style 
m.Style = &lcOrdSty..Style 
lnPrice = &lcOrdLine..Price
lcItem   = m.Style
lcColor  = ''
lcDyelot = Dyelot
llDyelot = SEEK(lcItem+lcColor,lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
lcScale  = Style.Scale
STORE 1   TO lnPriceRate,lnPriceUnit,lnDutyRate,lnDutyUnit
STORE '/' TO lcExSign,lcUntSin

lcPriceCur  = IIF(EMPTY(ORDHDR.ccurrcode),gcBaseCurr,ORDHDR.ccurrcode)
lnPriceRate = IIF(ORDHDR.nExRate=0,1,ORDHDR.nExRate)
lnPriceUnit = IIF(ORDHDR.nCurrUnit=0,1,ORDHDR.nCurrUnit)

lcDutyCur   = gcBaseCurr
lnDutyRate  = 1
lnDutyUnit  = 1

lcMjrHdr    = gfItemMask("HM")
lcMjrMsk    = gfItemMask("PM")
lcItmHdr    = gfItemMask("HI")
lcItmMsk    = gfItemMask("PI")
lcNMjrMsk   = gfItemMask("PN")

*--here we handle EX.SIZE SCALE
lcTmpBomSh = gfTempName()
SELECT BOM
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
CREATE TABLE (gcWorkDir+lcTmpBomSh) FROM ARRAY laFileStru
INDEX ON typ+item+iclr+citmmajor+citmmask TAG (lcTmpBomSh)
*--check if it works for all styles or not
llChkCSTSH = .F.
SELECT BOM
=SEEK(PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19))
SCAN REST WHILE cItmMajor = PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19)
  llChkCSTSH = .T.
  EXIT
ENDSCAN

IF llChkCSTSH
  SELECT BOM
  =SEEK(PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19))
  SCAN REST WHILE cItmMajor = PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19)
    *IF !((citmmask = lcItem) OR ((PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19) = PADR(SUBSTR(citmmask ,1,LEN(lcMjrMsk)),19))  AND ('**' $ citmmask)))
    *  LOOP
    *ENDIF
    IF ((citmmask = lcItem) AND !('**' $ citmmask))
      SELECT (lcOrdLine)
      LOCATE FOR STyle = citmmask
      IF !FOUND()
        LOOP
      ENDIF  
    ENDIF
    SCATTER MEMVAR MEMO
    m.nRecNo  = RECNO()
    IF cCatGTyp='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
       SUBSTR(ALLTRIM(Ite7m),lnSizePos) = STRTRAN(SUBSTR(lcItmMsk,lnSizePos),'X','*')
      FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
        lcLine = MLINE(MSZCROSREF,lnCount)
        IF SUBSTR(lcLine,1,3) <> lcScale
          LOOP
        ENDIF
        m.Item = PADR(SUBSTR(m.Item,1,lnSizePos-1)+SUBSTR(lcLine,AT('~',lcLine)+1,3),19)
        IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
          INSERT INTO (lcTmpBomSh) FROM MEMVAR
        ENDIF
      ENDFOR
    ELSE

      IF llMulCurr AND m.cCatgTyp $ "FT"
        IF SEEK(ALLTRIM(m.Item),'FABRIC') AND (Fabric.cpricecur <>gcBaseCurr )
          SELECT sycexch
          lcCurOrd = ORDER()
          SET ORDER TO Exchdate
          llFound   = SEEK(gcBaseCurr +DTOS(Fabric.introduced)+ Fabric.cPriceCur , 'SycExch')
          lnExRate  = IIF(llFound , sycexch.nExRate , 1)
          SET ORDER TO &lcCurOrd
        
          m.cfabcurr   = Fabric.cPriceCur
          m.nFabExRate = lnExRate
        ENDIF  
      ENDIF
      INSERT INTO (lcTmpBomSh) FROM MEMVAR
    ENDIF
  ENDSCAN
ELSE
  SELECT BOM
  =SEEK(PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19))
  SCAN REST WHILE cItmMajor = PADR(SUBSTR(lcItem,1,LEN(lcMjrMsk)),19)
    SCATTER MEMVAR MEMO
    m.nRecNo  = RECNO()
    IF cCatGTyp='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
       SUBSTR(ALLTRIM(Ite7m),lnSizePos) = STRTRAN(SUBSTR(lcItmMsk,lnSizePos),'X','*')
      FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
        lcLine = MLINE(MSZCROSREF,lnCount)
        IF SUBSTR(lcLine,1,3) <> lcScale
          LOOP
        ENDIF
        m.Item = PADR(SUBSTR(m.Item,1,lnSizePos-1)+SUBSTR(lcLine,AT('~',lcLine)+1,3),19)
        IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
          INSERT INTO (lcTmpBomSh) FROM MEMVAR
        ENDIF
      ENDFOR
    ELSE
      INSERT INTO (lcTmpBomSh) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF


SELECT (lcTmpBomSh)
SCAN
  *--MAN Added IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
  IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
    IF !llMsgDispd
      *--One or more cost items does not have a proper cost tyoe.
      *--Message : 38171
      =gfModalgen("INM38171B00000","ALERT",cItmMask)
      llMsgDispd = .T.
    ENDIF
    LOOP
  ENDIF
  
  IF !LIKE(STRTRAN(cItmMask,'*','?'),lcItem) .OR. ;
     (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
     (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)     
    LOOP
  ENDIF
  lcItemType = Typ
  IF EMPTY(MSIZES)
    lnTranQty = &lcOrdLine..TotQty
    lcSizes   = '1,2,3,4,5,6,7,8'
  ELSE
    lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(lcScale+'~',MSIZES)),5)
    lnTranQty = 0
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      lnTranQty = lnTranQty + IIF(STR(lnCount,1) $ lcSizes,&lcOrdLine..Qty&lcCount)
    ENDFOR
  ENDIF
  STORE '' TO lcCstClr,lcCstItm,lcCmSizes
  lcCstItmDye = SPACE(10)
  DO CASE
    CASE cCatGTyp='S'
      lcCmSizes='12345678'
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        laReq[lnCount]=&lcOrdLine..Qty&lcCount
      ENDFOR
      lcCstItm=''
      FOR lnCount = 1 TO LEN(ITEM)
        IF SUBSTR(ITEM,lnCount,1)='*'
          lcCstItm = lcCstItm + SUBSTR(lcItem,lnCount,1)
        ELSE
          lcCstItm = lcCstItm + SUBSTR(ITEM,lnCount,1)
        ENDIF
      ENDFOR
      IF !SEEK(lcCstItm,'Style')
        LOOP
      ENDIF
      lcCsItmSc = Style.Scale
      =SEEK(lcItem,'Style')
      IF !EMPTY(MSZCROSREF)
        lcCmSizes = ''
        STORE 0 TO laReq
        FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
          lcLine = MLINE(MSZCROSREF,lnCount)
          IF SUBSTR(lcLine,1,3) = lcScale .AND. SUBSTR(lcLine,7,3)=lcCsItmSc
            laReq[VAL(SUBSTR(lcLine,11,1))] = laReq[VAL(SUBSTR(lcLine,11,1))]+;
            laQty[VAL(SUBSTR(lcLine,5,1))]
            lcCmSizes = lcCmSizes + SUBSTR(lcLine,11,1)
          ENDIF
        ENDFOR
      ENDIF
      IF M_WAREHOUSE='Y' .AND. !EMPTY(lcStyWare) .AND. ;
         !SEEK(lcCstItm+lcStyWare+SPACE(10),'STYDYE')
        *E300725,1 Message : 38029
        *E300725,1 Style xxxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel
        
        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
        IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
          DO gpAdStyWar WITH lcCstItm,SPACE(10),lcStyWare
        ENDIF   
      
      ENDIF

    CASE INLIST(cCatGTyp,'F','T')
      lcCstItm = SUBSTR(Item,1,7)
      IF IClr = '******'
        lcCstClr = SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk))
      ELSE
        lcCstClr = IClr
      ENDIF
      IF (cCatGTyp='F' OR Trim_Invt) AND !SEEK(lcCstItm+lcCstClr,'FABRIC')
        LOOP
      ENDIF

      IF (cCatGTyp='F' .OR. Trim_Invt) .AND. M_WAREHOUSE='Y' .AND. ;
         !EMPTY(lcMatWare) .AND. ;
         !SEEK(PADR(lcCstItm,7)+PADR(lcCstClr,6)+PADR(lcMatWare,6)+SPACE(10),'FABDYE')
       
        *E300725,1 Message : 38029
        *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel
        
        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
        IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
          DO gpAdFabWar WITH lcCstItm,lcCstClr,SPACE(10),lcMatWare
        ENDIF   
      ENDIF
    CASE cCatGTyp='M'
    
       lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
    CASE cCatGTyp='P'
      lcExSign = gfGetExSin(@lcUntSin,lcPriceCur)
    CASE !INLIST(cCatGTyp,'S','F','T') 
      lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
  ENDCASE

  SELECT (lcTmpBomSh)
  DO CASE
    CASE &lcTmpBomSh..CCATGTYP = 'P'
      lnUnitPri = lnPrice
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'M','D')

      IF &lcTmpBomSh..nPercent > 0
        lnUnitPri  = lnPrice*(&lcTmpBomSh..nPercent/100)
      ELSE
        lnUnitPri = &lcTmpBomSh..UntCost
      ENDIF  
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T') .AND. SEEK(SUBSTR(lcCstItm,1,7)+lcCstClr,'Fabric')
      lnUnitPri = Fabric.CostBuy/Fabric.Conv
    OTHERWISE
      lnUnitPri = &lcTktSheet..UntCost
  ENDCASE
ENDSCAN
SELECT (lcTmpBomSh)
SCAN
  SCATTER MEMVAR MEMO
  m.cStatus = "A"
  
  IF llMulCurr
    
    *-- Get purchase price currency, duty currency or base currency
    lcCurrency = IIF(m.cCatgTyp='P' , Style.cPriceCur , ;
                     IIF(m.cCatgTyp $ 'MD' , Style.cDutyCur , gcBaseCurr))
    IF m.cCatgTyp $ 'FT'
      IF SEEK(ALLTRIM(m.Item),'FABRIC') AND (Fabric.cpricecur <>gcBaseCurr )
        lcCurrency = Fabric.cpricecur
      ENDIF
    ENDIF
                     
    lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
    *-- Get currency unit and exchange rate. 
    *-- Get currency symbol.
    llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
    lnExRate  = IIF(llFound , sycexch.nExRate , 1)
    llFound    = SEEK(lcCurrency , 'SycCurr')
    lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
    STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
    lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
    lnTotEquCst = IIF(lnBomLnNo = 0 , 0 , ROUND(m.TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))
    m.nexrate   =IIF(llFound , sycexch.nExRate , 1)
    m.nexcost   = lnTotEquCst
    m.cCurrCode = lcCurrency
    m.cFabCurr  = lcCurrency
    m.nFabExRate= m.nexrate
  ENDIF
  INSERT INTO (lcBomTemp) FROM MEMVAR
  
ENDSCAN

*!*************************************************************
*! Name      : lfActPad
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
FUNCTION lfActPad

*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' 
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Default Cost"  SKIP FOR (lnActFolder<>2) OR laScrMode[1] OR laScrMode[2]
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfvCostSc
lnBarNo = lnBarNo+1  

DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Amend Foreign Price/Duty rates"  ;
       SKIP FOR (lnActFolder<>2) OR laScrMode[1] OR laScrMode[2]
       
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfvAmdPrc
lnBarNo = lnBarNo+1  
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Fabric Width"  ;
       SKIP FOR (lnActFolder<>2) OR !(&lcTmpBom..TYP $'23') OR laScrMode[1] OR laScrMode[2]
       
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfvFabWdth

lnBarNo = lnBarNo+1  

DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Duty Type"  ;
       SKIP FOR (lnActFolder<>2) OR laScrMode[1] OR laScrMode[2]
       
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfvDtyTyp

RETURN
*!*************************************************************
*! Name      : lfFoundPad
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037892,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.
*!*************************************************************
*! Name      : lfvCostSc
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Add new menu Option for defult costing 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfvCostSc()
*!*************************************************************
*
FUNCTION lfvCostSc
SELECT BOMDEF
LOCATE
m.IPRRate=nIPRRate 
m.OPRRate=nOPRRate
m.EURRate=nEURRate
m.MatLosRate=nmatlosrat

DO (gcScrDir+gcWinAppl+"\sodfcst.SPX")

*!*************************************************************
*! Name      : lfvAmdPrc
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Add new menu Option amend foreign Price 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037892,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfvAmdPrc

IF (&lcTmpBom..nDutyRate =0) AND (&lcTmpBom..NExRate = 0) AND (&lcTmpBom..NExCost= 0)
  llFound    = SEEK( gcBaseCurr+Fabric.cPriceCur , 'SycExch')
  m.ExchRate = IIF(llFound , sycexch.nExRate , 1)
  m.UsdPrice = Fabric.nMCost1
  m.DutyRate = 0
ELSE
  IF (Fabric.cPriceCur = gcBaseCurr) 
    llFound     = SEEK( gcBaseCurr+Fabric.cPriceCur , 'SycExch')
    m.ExchRate  = IIF(llFound , sycexch.nExRate , 1)
    *m.UsdPrice = Fabric.nMCost1
    m.UsdPrice  = &lcTmpBom..UntCost
  ELSE
    m.ExchRate  = IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
    *m.UsdPrice = &lcTmpBom..NExCost
    m.UsdPrice  = lnUnitCst
  ENDIF
  m.DutyRate = &lcTmpBom..nDutyRate
ENDIF  

m.ExchRate = lnExRate    
m.UsdPrice = lnUnitCst

*--in case of Dutable record
IF &lcTmpBom..cCatgTyp = "D"
  m.DutyRate = &lcTmpBom..nPercent
ENDIF

DO (gcScrDir+gcWinAppl+"\soamfrpr.SPX")

*!*************************************************************
*! Name      : lfvsavAmd
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid save for amend foreign Price 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037892,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfvSavAmd
PRIVATE lnCurRec
lnCurRec   = RECNO(lcTmpBom)        && Save the BOM record pointer.

lnUntCost = m.UsdPrice
lnUnitCst = m.UsdPrice
IF &lcTmpBom..cCatgTyp $ "FT"
  IF Fabric.cPriceCur <> gcBaseCurr AND Fabric.CDutyCur = gcBaseCurr
    lnUntCost = (((m.UsdPrice/m.ExchRate)/Fabric.Conv)+( Fabric.nMCost2 + ;
                Fabric.nMCost3+Fabric.nMCost4)/Fabric.Conv)
  ENDIF
  IF Fabric.cPriceCur <> gcBaseCurr AND Fabric.CDutyCur <> gcBaseCurr
    lnUntCost = ((((m.UsdPrice/m.ExchRate)/Fabric.Conv) + ( Fabric.nMCost2 + ;
                   Fabric.nMCost3+Fabric.nMCost4)/m.ExchRate)/Fabric.Conv)
  ENDIF
  REPLACE &lcTmpBom..NFabExRate    WITH m.ExchRate
ENDIF
    
REPLACE &lcTmpBom..NExCost    WITH m.UsdPrice,;
        &lcTmpBom..NExRate    WITH m.ExchRate,;
        &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)

IF lcDutyable $ '13'
  *-- store old duty to subtract first in case of 'Update rate for all items?' No
  lnOldDuty = &lcTmpBom..nDutyRate
  REPLACE &lcTmpBom..nDutyRate WITH m.DutyRate  
  lcMsg2 = 'Update rate for all items?'
  lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
  
  IF lnMsgNo =1
    lnCurrRec = RECNO()
    lcCatType = &lcTmpBom..cCatgTyp
    lnOldPrcnt = 0

    LOCATE FOR &lcTmpBom..cCostStat $ '13' AND &lcTmpBom..cCatgTyp <> lcCatType  
    lnOldPrcnt = &lcTmpBom..nDutyRate
    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH lnOldDuty
    ENDSCAN    
    SCAN 
      IF &lcTmpBom..cCostStat $ '13' AND &lcTmpBom..cCatgTyp = lcCatType  
        =lfUpdCost(-1, 0)
      ENDIF  
    ENDSCAN

    LOCATE 
    lnCoun = 0
    lnPercnt = 0
    COUNT  TO lnCoun FOR &lcTmpBom..cCostStat $ '13' AND &lcTmpBom..cCatgTyp = lcCatType
      
    SCAN FOR &lcTmpBom..cCostStat $ '13' AND &lcTmpBom..cCatgTyp = lcCatType
      lnPercnt = lnPercnt + m.DutyRate
    ENDSCAN
    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH m.DutyRate
    ENDSCAN    
    LOCATE 
    SCAN 
      IF &lcTmpBom..cCostStat $ '13' AND &lcTmpBom..cCatgTyp = lcCatType
        IF &lcTmpBom..cCatgTyp <> "D"
          REPLACE &lcTmpBom..nDutyRate WITH m.DutyRate
        ENDIF
        =lfUpdCost(1 , 0)
      ENDIF  
    ENDSCAN
    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH ((lnPercnt/lnCoun)+lnOldPrcnt)/2
    ENDSCAN    
    GOTO  lnCurrRec
  ELSE
    *---first subtract old value then add new value
    lnCurrRec = RECNO()
    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH lnOldDuty
    ENDSCAN    
    GOTO  lnCurrRec
    =lfUpdCost(-1 , 0)
    *lnCurrRec = RECNO()
    LOCATE 
    lnCoun = 0
    lnPercnt = 0
    COUNT  TO lnCoun FOR &lcTmpBom..cCostStat $ '13'
      
    SCAN FOR &lcTmpBom..cCostStat $ '13'
      lnPercnt = lnPercnt +&lcTmpBom..nDutyRate
    ENDSCAN

    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH m.DutyRate
    ENDSCAN    

    GOTO  lnCurrRec
    =lfUpdCost(1 , 0)
    
    SCAN FOR &lcTmpBom..cCatgTyp = "D"
        REPLACE &lcTmpBom..nPercent  WITH lnPercnt/lnCoun
    ENDSCAN    
    GOTO  lnCurrRec
  ENDIF
ENDIF
          
lnUnitCst = lnUntCost
=lfvUnitCst()

*-- Restore record no.
IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
  GOTO lnCurRec IN (lcTmpBom)
ENDIF

=lfBrwCstSh()

CLEAR READ

*!*************************************************************
*! Name      : lfvsavCst
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid save for Defualt Costing
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*
FUNCTION lfvsavCst
SELECT BOMDEF
LOCATE
IF EOF()
  APPEND BLANK
  REPLACE nIPRRate   WITH m.IPRRate,;
          nOPRRate   WITH m.OPRRate,;
          nEURRate   WITH m.EURRate,; 
          nmatlosrat WITH m.MatLosRate
          =gfAdd_Info('BOMDEF')
ELSE
  REPLACE nIPRRate   WITH m.IPRRate,;
          nOPRRate   WITH m.OPRRate,;
          nEURRate   WITH m.EURRate,;
          nmatlosrat WITH m.MatLosRate
ENDIF

*!*************************************************************
*! Name      : lfvsavCst
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid save for Defualt Costing
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*
FUNCTION lfvFabWdth

m.FabWidth = &lcTmpBom..Width

DO (gcScrDir+gcWinAppl+"\sofabwdt.SPX")

*!*************************************************************
*! Name      : lfDOrdApr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Deactivate function of main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDOrdApr()
*!*************************************************************
FUNCTION lfDOrdApr

IF WONTOP()=lcAccTitl .OR. WONTOP()=lcOrdTitl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvDtyTyp
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid Duty type
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037892,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfvDtyTyp

DO (gcScrDir+gcWinAppl+"\SoDtyTyp.SPX")

*!*************************************************************
*! Name      : lfvsavWDth
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid Duty type
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037892,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfvSavWDth

REPLACE &lcTmpBom..Width    WITH m.FabWidth ,;
        &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
*!*************************************************************
*! Name      : lfvPrmFab
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid Primary Fabric 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfvPrmFab()
*!*************************************************************
*
FUNCTION lfvPrmFab

lnCount = 1
IF cbPrim = 1
  REPLACE &lcTmpBom..lprimary    WITH .T.,;
          &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
  *-- divid by No of primary fabrics
  lnRecNo = RECNO(lcTmpBom)          
  *--to get only FABRIC 
  SELECT (lcTmpBom)
  lnCurRecNo = RECNO(lcTmpBom)          
  lcCurItem = ALLTRIM(Item)
  lcIclr    = Iclr
  lnCntRec = 0
  lnTotQty = 0
  lnCurWastg = 0
  lnTPrmQty  = 0
  lnCount  = 1
  lcItmMask = &lcTmpBom..citmmask        
  SELECT (lcTmpBom)
  DIMENSION laFabric[1,2]
  SCAN 
    IF &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND lcIclr = Iclr 
      IF  ASCAN(laFabric,ALLTRIM(Item))=0
        DIMENSION laFabric[lnCount,2]
        laFabric[lnCount,1]   = ALLTRIM(Item)
        laFabric[lnCount,2]   = 1
        lnCount = 1 +lnCount
      ELSE
        laFabric[lnCount-1,2]   = laFabric[lnCount-1,2] + 1
      ENDIF  
      lnTotQty = IIF(!EMPTY(&lcTmpBom..nPrUntQty),&lcTmpBom..nPrUntQty,lnTotQty)
      lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
      IF !EMPTY(&lcTmpBom..npruntqty)
        lnTPrmQty  =&lcTmpBom..npruntqty
      ENDIF
    ENDIF
  ENDSCAN

  *lnRecNo = RECNO(lcTmpBom)           
  lnAscn = ASCAN(laFabric,ALLTRIM(lcCurItem))
  lnPos = IIF(lnAscn>1,CEILING(lnAscn/2),1)
  lnCntRec = ALEN(laFabric,1)

  SELECT (lcTmpBom)
  LOCATE
  lnPCount = 0
  SCAN FOR ccatgtyp = 'F' AND lcIclr = Iclr
    IF &lcTmpBom..lprimary
      lnOld = &lcTmpBom..TotCost   
      REPLACE &lcTmpBom..nEstBomQty WITH (lnTotQty*100/(lnCurWastg+100))/IIF(lnCntRec=0,1,lnCntRec),;
              &lcTmpBom..nbomtotqty WITH (lnTotQty/IIF(lnCntRec=0,1,lnCntRec)),;
              &lcTmpBom..npruntqty  WITH lnTPrmQty,;
              &lcTmpBom..TotCost    WITH &lcTmpBom..untcost*&lcTmpBom..nbomtotqty,;
              &lcTmpBom..nbomwastge WITH lnCurWastg,;
              &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
      =lfUpdCost(1 , lnOld)              
      lnPCount = lnPCount+1
    ENDIF  
  ENDSCAN
  lnTotalFab = lnTPrmQty
  
  IF lnTotalFab = 0 AND lnPCount = 1        &&Modify
    LOCATE FOR ccatgtyp = 'F'
    REPLACE &lcTmpBom..lFrstprm WITH .T.
    lnTotalFab =lnUnitQtyB
    SHOW GET lnTotalFab ENABLE
    _CUROBJ = OBJNUM(lnTotalFab)
  ELSE
    SHOW GET lnTotalFab DISABLE
  ENDIF

  LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask AND &lcTmpBom..lFrstprm
  IF !FOUND()
    LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask 
    IF FOUND()
      REPLACE &lcTmpBom..lFrstprm WITH .T.,;
              &lcTmpBom..npruntqty WITH lnTotalFab
    ENDIF          
  ENDIF
  
  SELECT (lcTmpBom)
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF
  
  IF lnPCount <> 1        &&Modify
    lnEstQtyA   = &lcTmpBom..nEstBomQty 
    lnUnitQtyA  = &lcTmpBom..nbomtotqty 
    lnWstg      = &lcTmpBom..nbomwastge
    SHOW GET lnEstQtyA  &lcLinStat 
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    =lfBrwCstSh()
  ENDIF  
ELSE
  *--<DELETE>        <Modify>            <Keep>              
  lcMsg2 = 'Please specify what you want to do with this fabric; delete this fabric and redistribute its quantity on the other primary fabrics, or keep the fabric with the same quantity, or keep the fabric and modify its quantity' 
  lnMsgNo=gfModalGen("TRM00000B32013","DIALOG",.F.,.F.,lcMsg2)
  llChckFrst = .F.
  IF &lcTmpBom..lFrstprm 
    REPLACE &lcTmpBom..lFrstprm    WITH .F.
    llChckFrst = .T.
  ENDIF
  lnCurRecNo = RECNO(lcTmpBom)          
  lcCurItem = ALLTRIM(Item)
  lcIclr    = Iclr
  lnCntRec = 0
  lnTotQty = 0
  lnCurWastg = 0
  lnCount  = 1
  SELECT (lcTmpBom)
  lcItmMask = &lcTmpBom..citmmask        
  DIMENSION laFabric[1,2]
  SCAN 
    IF &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND lcIclr  = Iclr
      IF  ASCAN(laFabric,ALLTRIM(Item))=0
        DIMENSION laFabric[lnCount,2]
        laFabric[lnCount,1]   = ALLTRIM(Item)
        laFabric[lnCount,2]   = 1
        lnCount = 1 +lnCount
        lnTotQty = IIF(!EMPTY(&lcTmpBom..nPrUntQty),&lcTmpBom..nPrUntQty,lnTotQty)
        lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
      ELSE
        laFabric[lnCount-1,2]   = laFabric[lnCount-1,2] + 1
        lnTotQty = IIF(!EMPTY(&lcTmpBom..nPrUntQty),&lcTmpBom..nPrUntQty,lnTotQty)
        lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
      ENDIF  
    ENDIF
  ENDSCAN
  *-- divid by No of primary fabrics
  lnRecNo = RECNO(lcTmpBom)           
  lnAscn = ASCAN(laFabric,ALLTRIM(lcCurItem))
  lnPos = IIF(lnAscn>1,CEILING(lnAscn/2),1)
  IF laFabric[lnPos,2] >1
    lnCntRec = ALEN(laFabric,1)
  ELSE
    lnCntRec = ALEN(laFabric,1)-1
  ENDIF  
  
  SELECT (lcTmpBom)
  IF lnCurRecNo > 0 .AND. lnCurRecNo <= RECCOUNT()
    GOTO lnCurRecNo
  ENDIF

  REPLACE &lcTmpBom..lprimary    WITH .F.,;
          &lcTmpBom..lFrstprm    WITH .F.,;
          &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)

  LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask AND &lcTmpBom..lFrstprm
  IF !FOUND()
    LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask 
    IF FOUND()
      REPLACE &lcTmpBom..lFrstprm WITH .T.,;
              &lcTmpBom..npruntqty WITH lnTotalFab
    ENDIF          
  ENDIF
          
  SELECT (lcTmpBom)
  LOCATE
  SCAN FOR ccatgtyp = 'F'
    IF &lcTmpBom..lprimary AND lcIclr    = Iclr
      *--add totcost to our calculation
      lnOld = &lcTmpBom..TotCost
      REPLACE &lcTmpBom..nEstBomQty WITH (lnTotQty*100/(lnCurWastg+100))/IIF(lnCntRec=0,1,lnCntRec),;
              &lcTmpBom..nbomtotqty WITH (lnTotQty/IIF(lnCntRec=0,1,lnCntRec)),;
              &lcTmpBom..TotCost    WITH &lcTmpBom..untcost*&lcTmpBom..nbomtotqty,;
              &lcTmpBom..nbomwastge WITH lnCurWastg,;
              &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
      =lfUpdCost(1 , lnOld)              
    ELSE
      REPLACE &lcTmpBom..npruntqty WITH 0
    ENDIF  
  ENDSCAN
  SELECT (lcTmpBom)
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF
  IF lnMsgNo = 1         &&DELETE
      REPLACE &lcTmpBom..cStatus WITH "D"
      DELETE
      =lfBrwCstSh()
  ENDIF
  
  IF lnMsgNo = 2         &&Modify
    lnEstQtyB = 0
    lnUnitQtyB = 0
    SHOW GET lnEstQtyB ENABLE
    _CUROBJ = OBJNUM(lnEstQtyB)
  ENDIF
  IF lnMsgNo = 3         &&Keep
    lnTotalFab = 0
    SHOW GET lnTotalFab DISABLE
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfvFabTot
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid Fabric total
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*
FUNCTION lfvFabTot
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcTmpBom)
lnRecNo = RECNO(lcTmpBom)           

SCAN FOR &lcTmpBom..lprimary
  REPLACE &lcTmpBom..nPrUntQty    WITH lnTotalFab,;
          &lcTmpBom..cStatus      WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
         
ENDSCAN
IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
  GOTO lnRecNo
ENDIF
=lfvPrmFab()
=lfBrwCstSh()

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvSavDty
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Save Duty type
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*
FUNCTION lfvSavDty

lnAlias = SELECT()
SELECT (lcTmpBom)
lnRecNo = RECNO(lcTmpBom)           

GOTO TOP IN BOMDEF
DO CASE 
    CASE lntype = 1
          lnCstPerc= BOMDEF.nIPRRate
    CASE lntype = 2
          lnCstPerc =BOMDEF.nOprrate
    CASE lntype = 3
          lnCstPerc =BOMDEF.nEURRate
ENDCASE

 
SELECT (lcTmpBom)
LOCATE
SCAN
  IF &lcTmpBom..cCatgTyp='D'
    lnTotEquCst =  (&lcTmpBom..UntCost*lnCstPerc)/IIF(npercent=0,1,npercent)
    REPLACE UntCost  WITH lnTotEquCst,;
            TotCost  WITH lnTotEquCst,;
            npercent WITH lnCstPerc 
  ELSE
    IF  &lcTmpBom..ccoststat $ '13'
      REPLACE &lcTmpBom..nDutyRate WITH lnCstPerc 
    ENDIF  
  ENDIF
ENDSCAN

IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
  GOTO lnRecNo
ENDIF
SCAN
  IF &lcTmpBom..cCatgTyp $ "D"
    =lfvCstPrc()
    IF &lcTmpBom..cCatgTyp $ "D"
      lnCurrDuty = &lcTmpBom..nPercent
    ENDIF
  ENDIF
ENDSCAN

=lfBrwCstSh()

CLEAR READ
*!*************************************************************
*! Name      : lfGetBomrc
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Get BOM records
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfGetBomrc(lcPadName)
*!*************************************************************
*
FUNCTION lfGetBomrc
PRIVATE llChkPR,llCHkDuty,llCHkMat,lcLocSty,llFrstPrm

STORE '' TO lcLocSty
STORE .F. TO llFrstPrm
SELECT BOMDEF
LOCATE
SELECT (lcBomTemp)
SCAN  
  IF lcLocSty <> PADR(Style,lnMajorLen)
    lcLocSty = PADR(Style,lnMajorLen)
    STORE .F. TO llChkPR,llCHkDuty,llCHkMat
  ENDIF

  SCATT MEMVAR MEMO
      
  IF cCatgTyp='F'
    IF EMPTY(&lcBomTemp..Width)
      =SEEK(ALLTRIM(Item),'FABRIC')
      m.Width = Fabric.Width
    ENDIF
    SELECT STYLE
    lcStyOrd = ORDER()
    SET ORDER TO STYLE
    IF SEEK(ALLTRIM(&lcBomTemp..Style),'STYLE') AND !EMPTY(STYLE.Fabric) AND ALLTRIM(STYLE.Fabric) = ALLTRIM(Item)
      IF !lprimary
        IF llFrstPrm
          m.lFrstPrm  = .F.
        ELSE
          m.lFrstPrm  = .T.
          llFrstPrm   = .T.
        ENDIF
        
        m.lprimary  = .T.
        m.nPrUntQty =nEstBomQty
      ENDIF  
    ENDIF
    SET ORDER TO &lcStyOrd
  ENDIF  
  INSERT INTO (lcTmpBom) FROM MEMVAR
ENDSCAN  
*!*************************************************************
*! Name      : lfCreatBom
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Create BOM records
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfCreatBom()
*!*************************************************************
*
FUNCTION lfCreatBom

IF EOF(lcTmpBom)
  llNewEntry  = .T.
      
  IF .F. AND 'SP' $ gcComp_Mdl .AND. lcStyleTyp $ 'MI' AND llOpnPdm
    *** Cost sheet not found.  Do you wish to Add , ***
    *** copy from another {lcItemFile} Cost Sheet or import ? ***
    *** < Add > - < Copy > - < Import > - < Cancel > ***

  ELSE
    STORE .F. TO cbOnSize , cbSelSize , llEdit
    lcMsg2 = 'You have to Create cost sheet for style ' +lcstyle
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    lnChoice = gfModalGen("QRM38002B38000" , "DIALOG" , lcItemFile)
  ENDIF

  DO CASE
    *-- If press copy.
    CASE lnChoice = 2
      *-- If there is something was copied.
      IF lfCpyCstSht()
        IF lnBomLnNo > 0
          *-- Get the available mfg operations from the bom file.
          DECLARE laOprCode[1,2]
          laOprCode = ""
         
          SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
           WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
                 cDefCode+cCode_No+cRltField+cFld_Name="N"+MfgCode+"N"+"MFGCODE" AND;
                 !EMPTY(Codes.cDiscrep) ;
            FROM (lcTmpBom) , Codes ;
            INTO ARRAY laOprCode 
           IF !EMPTY(laOprCode[1,1])
            DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
            =AINS(laOprCode,1)
          ENDIF
          laOprCode[1,1] = "Select Operation"
          laOprCode[1,2] = SPACE(6)
          SELECT (lcTmpBom)
       ENDIF
     ENDIF

    CASE lnChoice = 3
       SELECT ORDHDR
       =gfObj_lock(.F.)
       STORE .F. TO laScrMode
       STORE .T. TO laScrMode[2]
       SHOW GETS
       RETURN
  ENDCASE
  IF !EOF(lcTmpBom)
    SELECT (lcTmpBom)
    LOCATE FOR &lcTmpBom..lBasOnSiz
    cbOnSize  = IIF(FOUND() , .T. , .F.)
      
    LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
    llOnSize  = IIF(FOUND() , .T. , .F. )
    lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
    GO TOP
  ELSE
    lcBasStat = "ENABLE"
  ENDIF
ELSE
  llEdit      = .T.
  llNewEntry  = .F.
  SELECT (lcTmpBom)
  LOCATE FOR &lcTmpBom..lBasOnSiz
  cbOnSize  = IIF(FOUND() , .T. , .F.)
  LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
  llOnSize  = IIF(FOUND() , .T. , .F. )
  lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
  GO TOP IN (lcTmpBom)
ENDIF

*!*************************************************************
*! Name      : lfCpyCstSht
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Copy from another cost sheet
*!*************************************************************
*! Calls     : MFSTYCP.PRG, lfBrwCstSh, lfShowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .F. if failed to copy
*!*************************************************************
*! Example   : lfCpyCstSht()
*!*************************************************************
*
FUNCTION lfCpyCstSht
PRIVATE lnItmRec , lcCopFmSty, llCopy , lcStyDes

*-- Set the index to the main tag to be able to see all the non-majors 
*-- for the style and all the colors for the fabric.
SELECT STYLE
SET ORDER TO TAG STYLE
SELECT FABRIC
SET ORDER TO TAG FABRIC

*-- Save the record pointer in the main item file.
SELECT (lcItemFile)
lnItmRec = RECNO()
SET FILTER TO 

*-- Var. hold the item to copy from.
lcCopFmSty  = SPACE(lnItemLen)
lcSelTxt    = '\<Select'
lcStyDes    = ""
llCopy      = .F.

*-- Copy browse title.
lcBrCpItmT  = 'Copy Cost Items'

lcCopPrFab = ""

PUSH KEY
ON KEY

*-- Select the temp. file & call the copy from screen.
SELECT (lcTmpBom)
DO (gcScrDir+"MFSTYCP.SPX")

POP KEY

SELECT (lcTmpCopy)
ZAP

*-- Restore the record pointer.
GO TOP IN (lcTmpBom)

*-- If the browse was released , call the browse function.
IF !WEXIST(lcBrCstShT)
  =lfBrwCstSh()
ELSE
  =lfShowDet()
ENDIF

*-- Set the index to the item tag to be able to see only the major record
*-- for the style and only the main fabric.
SELECT STYLE
SET ORDER TO TAG CSTYLE
SELECT FABRIC
SET ORDER TO TAG CFABRIC

*-- Restore the record pointer for the main file & its filter.
SELECT (lcItemFile)
DO CASE
  CASE lcStyleTyp = 'I'
    SET FILTER TO !Style.Make
  CASE lcStyleTyp = 'M'
    SET FILTER TO Style.Make
ENDCASE
IF lnItmRec > 0 .AND. lnItmRec <= RECCOUNT()
  GOTO lnItmRec
ENDIF

llCopy = (lnBomLnNo > 0)
RETURN (llCopy)

*!*************************************************************
**************    Functions for MFSTYCP Screen    *************
************** Copy from another style cost sheet *************
*!*************************************************************

*!*************************************************************
*! Name      : lfvCpyItem
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Validate Style/Fabric to copy Cost Sheet from
*!*************************************************************
*! Calls     : FaBrow, gfStyBrw, lfBrwCopy
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCpyItem()
*!*************************************************************
*
FUNCTION lfvCpyItem

*-- there is item selected or press the browse icon.
IF (!EMPTY(lcCopFmSty) .OR. llBrowse) .AND. LASTKEY() =13
  *-- Validate from the fabric file if copy material cost sheet.
  SELECT (lcItemFile)
  IF lcStyleTyp = 'T' .AND. (llBrowse .OR. !SEEK(PADR(lcCopFmSty,7)))
    =FaBrow(@lcCopFmSty,'*')
  ENDIF
    
  *-- Validate from the style file if copy style cost sheet.
  IF lcStyleTyp <> 'T' .AND. (llBrowse .OR. !SEEK(PADR(lcCopFmSty,lnItemLen)))
    *-- If the item is a style.
    CLEAR TYPEAHEAD
    
    IF '?' $ lcCopFmSty
      lcCopFmSty = gfStyBrw("M" , "" , "" , .F.)
    ELSE
      lcCopFmSty = gfStyBrw("M" , lcCopFmSty , "" , .F.)
    ENDIF  
    
    *-- If the browse was released, call it again.
    IF !WEXIST(lcBrCpItmT)
      =lfBrwCopy()
    ENDIF
  ENDIF
  *-- Check if the selected item was dyelot no or yes.
  lcStyDes = IIF(!EMPTY(lcCopFmSty) , IIF(lcStyleTyp = 'T' , FABRIC.Desc , STYLE.Desc1) , "")
  llBrowse = .F.
  llSkipIt = .F.
  IF !EMPTY(lcCopFmSty)
    DO CASE
      CASE lcStyleTyp = 'I' .AND. Style.Make
        *** Copy cost sheet from imported styles only.        ***
        *** <  Ok  > ***
        =gfModalGen("INM38017B00000" , "DIALOG" , "imported styles")
        llSkipIt = .T.
      CASE lcStyleTyp = 'M' .AND. !Style.Make
        *** Copy cost sheet from manufactured styles only.    ***
        *** <  Ok  > ***
        =gfModalGen("INM38017B00000" , "DIALOG" , "manufactured styles")
        llSkipIt = .T.
      CASE !SEEK(PADR(lcCopFmSty,19) , 'Bom')
        *** Cost Sheet for this {lcItemFile} not found.       ***
        *** <  Ok  > ***
        =gfModalGen("INM38018B00000" , "DIALOG" , lcItemFile)
        llSkipIt = .T.
    ENDCASE
  ENDIF
  *-- If nothing was selected, blank the variable.
  IF EMPTY(lcCopFmSty) .OR. llSkipIt
    lcCopFmSty = SPACE(lnItemLen)
    lcStyDes   = ""
    
    lcCopPrFab = ""
  ELSE
    lcCopPrFab = IIF(lcStyleTyp <> 'T' , Style.Fabric , "")
  ENDIF
  
  
  SHOW GET lcStyDes   && Refersh the style description.
  
  *-- Copy Cost Items.
  *-- Get all the item cost sheet lines from BOM file.
  IF !EMPTY(lcCopFmSty)
    
    =lfGetCopy()
    
    *-- If nothing was selected, replace with the current style "laData[1]".
    IF lnBomLnNo > 0
      *-- Replace all the item with laData[1] & set the proper index.
      REPLACE ALL cItmMajor WITH PADR(&lcOrdSty..Style,19) ;
                  cItmMask  WITH IIF(lcStyleTyp<>"T" , PADR(STUFF(cItmMask,1,lnMajorLen,PADR(&lcOrdSty..Style,lnMajorLen)),19) , cItmMask)
      *-- Define array to hold all the available cost items in the copy file.
      DECLARE laCatgory[1]
      laCatgory = ""
      SELECT DISTINCT &lcTmpCopy..ccatgtyp ;
             FROM (gcWorkDir+lcTmpCopy) ;
             INTO ARRAY laCatgory
      
      *-- Define description for all the cost types.
      IF !EMPTY(laCatgory[1])
        FOR lnCnt = 1 TO ALEN(laCatgory,1)
          DO CASE
            CASE laCatgory[lnCnt] = 'F'
              lcMskStr = '====== FABRIC ====='
            CASE laCatgory[lnCnt] = 'T'
              lcMskStr = '======  TRIM  ====='
            CASE laCatgory[lnCnt] = 'S'
              lcMskStr = '====== STYLE  ====='
            CASE laCatgory[lnCnt] = 'M'
              lcMskStr = '======  MFG   ====='
            CASE laCatgory[lnCnt] = 'P'
              lcMskStr = '======  PRICE ====='
            CASE laCatgory[lnCnt] = 'D'
              lcMskStr = '======  DUTY  ====='
          ENDCASE
          INSERT INTO (lcTmpCopy) (ccatgtyp , citmmask , Typ) ;
                 VALUES (laCatgory[lnCnt] , lcMskStr , "*")
        ENDFOR
      ENDIF
      
      INDEX ON ccatgtyp + citmmajor TAG (lcTmpCopy) OF (lcTmpCopy)
      
      *-- Refresh the browse.
      =lfBrwCopy()
      *-- Enable all the processes buttons.
      SHOW GET pbSel    ENABLE
      SHOW GET pbNone   ENABLE
      SHOW GET pbInvert ENABLE
      SHOW GET pbAll    ENABLE
      SHOW GET pbOkCop  ENABLE
    ELSE
      *-- blank the varaiables.
      lcCopFmSty = SPACE(lnItemLen)
      lcStyDes   = ""
      *-- Disable all the processes buttons.
      SHOW GET pbSel    DISABLE
      SHOW GET pbNone   DISABLE
      SHOW GET pbInvert DISABLE
      SHOW GET pbAll    DISABLE
      SHOW GET pbOkCop  DISABLE
      *-- Blank the copy file.
      SELECT (lcTmpCopy)
      ZAP
      *-- Refresh the copy browse.
      =lfBrwCopy()
      lnBomLnNo = 0
      _CUROBJ   = OBJNUM(lcCopFmSty)
    ENDIF
  ELSE
    lcCopFmSty = SPACE(lnItemLen)
    lcStyDes   = ""
    *-- Disable all the processes buttons.
    SHOW GET pbSel    DISABLE
    SHOW GET pbNone   DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbAll    DISABLE
    SHOW GET pbOkCop  DISABLE
    SELECT (lcTmpCopy)
    ZAP
    =lfBrwCopy()
    lnBomLnNo = 0
    _CUROBJ   = OBJNUM(lcCopFmSty)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvCopy
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Copy from another Material/Style cost sheet
*!*************************************************************
*! Calls     : lfUpdPerc
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvCopy()
*!*************************************************************
*! Modifications : 
*!*************************************************************
*
FUNCTION lfvCopy
PRIVATE llDel , lnSavRecNo , lcScanExp

PRIVATE llChkRclct
llChkRclct = .F. 
*-- Return if there is nothing selected to be copy.
SELECT (lcTmpCopy)
SCAN
 IF !(CINCL = "»")
    llChkRclct = .T. 
 ENDIF
ENDSCAN
LOCATE

PRIVATE lcOrder
lcOrder = SET('ORDER')
SET ORDER TO TAG (lcTmpCopy1)
SEEK "»"

IF !FOUND()
  *** Nothing selected to be copy. ***
  *** <  Ok  > ***
  =gfModalGen("INM38005B00000" , "DIALOG")
  _CUROBJ = OBJNUM(pbSel)
  
  SELECT (lcTmpCopy)
  SET ORDER TO &lcOrder.
  
  RETURN
ENDIF

WAIT 'Copying Cost Sheet ......' WINDOW NOWAIT

*-- Get all the item cost sheet lines from BOM file.

*B605400,1 AMH optimize speed [Start]
*SELECT * FROM (lcTmpCopy) ;
 WHERE !EMPTY(cIncl) .AND. Typ <> "*" ;
  INTO DBF (gcWorkDir+lcTmpBom)
lnBomLnNo = 0

SCAN REST WHILE CINCL = "»" FOR Typ <> "*"
  lnBomLnNo = lnBomLnNo + 1
  SCATTER MEMO MEMVAR

  m.style       = &lcOrdSty..Style  
  m.citmmajor   = &lcOrdSty..Style  
  SELECT (lcTmpBom)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

*-- Variable hold the no. of records copied.
SELECT (lcTmpBom)

INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)
IF lnBomLnNo <> 0 .AND. llPwInst .AND. lcStyleTyp = 'M'
  =lfCopyDet()
ENDIF

STORE "" TO lcCopyScl , lcMainScl
SELECT STYLE
SET ORDER TO TAG CSTYLE
*-- Get the copy from style scale.
IF SEEK(SUBSTR(lcCopFmSty,1,lnMajorLen))
  lcCopyScl = Style.Scale
ENDIF
*-- Get the parent style scale.
IF SEEK(SUBSTR(&lcOrdLine..Style,1,lnMajorLen))
  lcMainScl = Style.Scale
ENDIF
SET ORDER TO TAG STYLE

*-- True if the parent style & component style have different scales.
llMapSizes = IIF((lcCopyScl == lcMainScl) , .F. , .T.)

*-- If there is records copied.
IF lnBomLnNo > 0
  SELECT (lcTmpBom)
  LOCATE FOR lBasOnSiz
  cbOnSize  = IIF(FOUND() , .T. , .F.)
  LOCATE
  *-- Scan through the copied records to validate all the records.
  SCAN
    *-- If manufactered style or material and the cost item is mfg operation
    *-- or the cost item was trim & consedered as non inventory, accept the record.

    IF (cCatGTyp $ 'MPD') .OR. (cCatGTyp = 'T'.AND. !Trim_Invt)
      LOOP
    ENDIF
    
    *-- Check that the copy to fabric has all copy from fabric colors.
      *SET ORDER TO STYLE IN STYLE
      *-- Flag to delete the current record in the BOM temp file.
      llDel = .F.
      *-- Check that the copy to style has all copy from style colors.
      IF !('*' $ cItmMask)
        IF !SEEK(cItmMask , "STYLE")
          *-- Set flag to delete the current record in the BOM temp file.
          llDel = .T.
        ENDIF
      ELSE
        *-- Prepare the locate condition to use the like().
        lcLikeMask = STRTRAN(cItmMask , "*" , "?")
        IF SEEK(SUBSTR(cItmMajor,1,lnMajorLen) , "STYLE")
          *-- Seek for the available style masks in the file.
          SELECT STYLE
          LOCATE REST WHILE SUBSTR(Style,1,lnMajorLen) = SUBSTR(&lcTmpBom..cItmMajor,1,lnMajorLen) FOR LIKE(lcLikeMask , STYLE.Style)
          IF !FOUND()
            *-- Set flag to delete the current record in the BOM temp file.
            llDel = .T.
          ENDIF
        ELSE
          llDel = .T.
        ENDIF
      ENDIF
      *SET ORDER TO CSTYLE IN STYLE
      SELECT (lcTmpBom)
      IF llDel
        *-- Delete the current record in the BOM temp file.
        lnBomLnNo = lnBomLnNo - 1
        REPLACE cStatus WITH "S"
        LOOP
      ENDIF
    
    *-- Check that the Copy To item has all colors belongs to the copy from
    *-- cost items only if there is non-major for the style structure.
    *IF llNonMjExt .AND. llColorExt .AND. ((cCatGTyp $ 'FT' .AND. &lcTmpBom..IClr = '******') .OR. ;
       (cCatGTyp = 'S' .AND. '*' $ &lcTmpBom..Item))
    IF llNonMjExt .AND. llColorExt .AND. ;
       ((cCatGTyp $ 'FT' .AND. &lcTmpBom..IClr = '******') .OR. (cCatGTyp = 'S'))
      llDelRec = .F.
      IF cCatGTyp $ 'FT'
        WAIT IIF(cCatGTyp='F' , 'Cross referencing fabric colors' ,;
                                'Cross referencing trim colors')  WINDOW NOWAIT
        *-- Get the first record that its data matching the item in BOM file.
        SELECT Fabric
        IF !SEEK(SUBSTR(&lcTmpBom..Item,1,7))
          *** {Item} : {&lcTmpBom..Item} does not exist in the ***
          *** {fabric} file.  Therefor, it will not be copied. ***
          *** < OK > ***
          =gfModalGen("INM38126B00000" , "DIALOG" , "Item|"+ALLTRIM(&lcTmpBom..Item)+"|fabric")
          *-- If the fabric was not found, delete the record from the temp. bom.
          lnBomLnNo = lnBomLnNo - 1
          SELECT (lcTmpBom)
          REPLACE cStatus WITH "S"
          LOOP
        ENDIF
      ELSE
        WAIT 'Cross referencing Component style/'+lcNonMjHdr WINDOW NOWAIT
        *-- Get the first record that its data matching the item major in BOM file.
        SELECT Style
        IF !SEEK(SUBSTR(&lcTmpBom..Item,1,lnMajorLen))
          *** {Style} : {&lcTmpBom..Item} does not exist in the ***
          *** {style} file.  Therefor, it will not be copied.   ***
          *** < OK > ***
          =gfModalGen("INM38126B00000" , "DIALOG" , "Style|"+ALLTRIM(&lcTmpBom..Item)+"|style")
          *-- If the style was not found, delete the record from the temp. bom.
          lnBomLnNo = lnBomLnNo - 1
          SELECT (lcTmpBom)
          REPLACE cStatus WITH "S"
          LOOP
        ENDIF
        
        lcCurScale = SUBSTR(Style.Scale,1,1)
        DECLARE laTmpScale[1]
        laTmpScale = ""
        
        *-- Get the available sizes for the current scale.
        SELECT SCALE
        SELECT Scale FROM (gcDataDir+"Scale") ;
         WHERE Type + SUBSTR(Scale,1,1) = "S" + lcCurScale ;
          INTO ARRAY laTmpScale
        SELECT STYLE
      ENDIF
      *-- Save the record in the following array.
      SCATTER TO laItemRec
      
      IF &lcTmpBom..cCatGTyp $ 'FT'
        lcParent = IIF(lcStyleTyp = "T" , SUBSTR(&lcTmpBom..cItmMask,1,6) , ;
                   SUBSTR(&lcTmpBom..cItmMask , lnMajorLen + IIF(!EMPTY(lcSeprator) , 2 , 1) , lnNonMjLen))
        lcCmpont = SUBSTR(&lcTmpBom..IClr,1,6)
        DO CASE
          *** Case the current component is same as the style parent.
          CASE lcParent = "******" .AND. lcCmpont = "******"
             *-- If the category is Fabric Or Trim.
             lcScanExp = IIF(lcStyleTyp = "T" , " FOR FABRIC.Make" , "")
             *-- Search for the current fabric or trim.
             SELECT (lcItemFile)
             =SEEK(PADR(lcStyMaj,lnItemLen))
             SCAN REST WHILE (PADR(&lcItemFile,lnItemLen) = ;
                              PADR(lcStyMaj,lnItemLen)) &lcScanExp
               *-- Save record no. in the item file.
               lnSavRecNo = RECNO()
               *-- If the item file is fabric, use the color field.
               *-- If the item file is style, cut the color according to 
               *-- the color start position & color segment lenght.
               lcColor    = IIF(lcStyleTyp ='T' , &lcItemFile..Color , ;
                            SUBSTR(&lcItemFile..Style , lnColorStr , lnColorLen))
               SELECT FABRIC
               IF !SEEK(SUBSTR(&lcTmpBom..Item,1,7) + PADR(lcColor,6) , "FABRIC") .AND. ;
                  !SEEK(SUBSTR(&lcTmpBom..Item,1,7) + PADR(lcColor,6) , lcTmpFbric)
                 lcFilVar = IIF(&lcTmpBom..cCatGTyp = 'F' , 'Fabric' , 'Trim')
                 *** {lcItemFile/lcNonMjHdr} : lcColor not found for the   ***
                 *** {lcFilVar}: {SUBSTR(&lcTmpBom..Item,1,7)}.  Do you wish to add it to the {lcFilVar}? ***
                 *** <  Yes  > - <  No  > ***
                 lcTmpStr = IIF(lcStyleTyp="T",lcItemFile+"/color",lcNonMjHdr)+"|"+lcColor+"|"+lcFilVar+"|"+SUBSTR(&lcTmpBom..Item,1,7)+"|"+lcFilVar
                 IF gfModalGen("QRM38006B00006" , "DIALOG" , lcTmpStr) = 2
                   llDelRec = .T.
                   EXIT
                 ENDIF
                 
                 *-- Add the trim or fabric to the temp fabric file.
                 SELECT (lcTmpFbric)
                 APPEND BLANK
                 GATHER FROM laItemRec
                 
                 REPLACE Color   WITH lcColor ;
                         OnHand  WITH 0       ;
                         OnOrder WITH 0       ;
                         Usage   WITH 0       ;
                         nStkVal WITH 0       ;
                         nMatWip WITH 0
                 
                 *-- Call global function to add audit fields info.
                 =gfAdd_Info(lcTmpFbric)
               ENDIF
               *-- Go to the previous record.
               SELECT (lcItemFile)
               IF lnSavRecNo > 0 .AND. lnSavRecNo <= RECCOUNT(lcItemFile)
                 GOTO lnSavRecNo
               ENDIF
             ENDSCAN
          *** Case the current component uses different colors for the parent.
          CASE lcParent <> "******" .AND. lcCmpont <> "******"
            IF !SEEK(SUBSTR(&lcOrdLine..Style,1,lnMajorLen) + IIF(lcStyleTyp = "T" , "" , lcSeprator) + lcParent , lcItemFile)
              lnBomLnNo = lnBomLnNo - 1
              SELECT (lcTmpBom)
              REPLACE cStatus WITH "S"
              LOOP
            ENDIF
        ENDCASE
      ELSE
        *-- If the category is Style, for sure you will use the style file.
        *-- because the style component can be part only from style cost sheet.
        SELECT STYLE
        lcParent = SUBSTR(&lcTmpBom..cItmMask , lnMajorLen + IIF(!EMPTY(lcSeprator) , 2 , 1) , lnNonMjLen)
        lcCmpont = SUBSTR(&lcTmpBom..Item , lnMajorLen + IIF(!EMPTY(lcSeprator) , 2 , 1) , lnNonMjLen)
        DO CASE
          *** Case the current component is same as the style parent.
          CASE lcParent = "******" .AND. lcCmpont = "******"
            =SEEK(SUBSTR(&lcTmpBom..cItmMask , 1 , lnMajorLen) , "STYLE")
            SCAN REST WHILE SUBSTR(Style,1,lnMajorLen) = SUBSTR(&lcTmpBom..cItmMask,1,lnMajorLen) ;
                      FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
              lnSavRecNo = RECNO()
              lcNonMajor = SUBSTR(&lcItemFile..Style , lnMajorLen+1 , lnNonMjLen + IIF(!EMPTY(lcSeprator) , 1 , 0))
              lcString   = SUBSTR(&lcItemFile..Style , lnMajorLen+IIF(!EMPTY(lcSeprator) , 2 , 1) , lnNonMjLen)
              
              IF !SEEK(SUBSTR(&lcTmpBom..Item,1,lnMajorLen) + lcNonMajor , "STYLE") .AND. ;
                 !SEEK(SUBSTR(&lcTmpBom..Item,1,lnMajorLen) + lcNonMajor , lcTmpStyle)
                *** {lcItemFile/lcNonMjHdr} : {lcNonMajor} not found ***
                *** for the component style {SUBSTR(&lcTmpBom..Item,1,lnMajorLen)}. 
                *** Do you wish to add it? ***
                *** <  Yes  > - <  No  > ***
                lcTmpStr = IIF(lcStyleTyp="T",lcItemFile+"/color",lcNonMjHdr)+ "|" + lcString + "|" + SUBSTR(&lcTmpBom..Item,1,lnMajorLen)
                IF gfModalGen("QRM38007B00006" , "DIALOG" , lcTmpStr) = 2
                  llDelRec = .T.
                  EXIT
                ENDIF
                *-- If there is extended size scale.
                IF llExtSizSc
                  *-- Add styles for all the available scales with zero values
                  *-- in all the fields related to stock or orders or ras.....
                  IF !EMPTY(laTmpScale[1])
                    FOR lnCnt = 1 TO ALEN(laTmpScale,1)
                      SELECT (lcTmpStyle)
                      APPEND BLANK
                      GATHER FROM laItemRec
                      REPLACE STYLE     WITH SUBSTR(&lcTmpBom..Item,1,lnMajorLen) + lcNonMajor + lcSizeSep + laTmpScale[lnCnt] ;
                              cStyMajor WITH SUBSTR(&lcTmpBom..Item,1,lnMajorLen) ;
                              Ord1 WITH 0 Ord2 WITH 0 Ord3 WITH 0 Ord4 WITH 0 ;
                              Ord5 WITH 0 Ord6 WITH 0 Ord7 WITH 0 Ord8 WITH 0 TotOrd WITH 0 ;
                              Wip1 WITH 0 Wip2 WITH 0 Wip3 WITH 0 Wip4 WITH 0 ;
                              Wip5 WITH 0 Wip6 WITH 0 Wip7 WITH 0 Wip8 WITH 0 TotWip WITH 0 ;
                              Stk1 WITH 0 Stk2 WITH 0 Stk3 WITH 0 Stk4 WITH 0 ;
                              Stk5 WITH 0 Stk6 WITH 0 Stk7 WITH 0 Stk8 WITH 0 TotStk WITH 0 ;
                              Alo1 WITH 0 Alo2 WITH 0 Alo3 WITH 0 Alo4 WITH 0 ;
                              Alo5 WITH 0 Alo6 WITH 0 Alo7 WITH 0 Alo8 WITH 0 TotAlo WITH 0 ;
                              Shp1 WITH 0 Shp2 WITH 0 Shp3 WITH 0 Shp4 WITH 0 ;
                              Shp5 WITH 0 Shp6 WITH 0 Shp7 WITH 0 Shp8 WITH 0 TOTShp WITH 0
                      REPLACE Ret1  WITH 0 Ret2  WITH 0 Ret3  WITH 0 Ret4  WITH 0 ;
                              Ret5  WITH 0 Ret6  WITH 0 Ret7  WITH 0 Ret8  WITH 0 TOTRet WITH 0 ;
                              Plan1 WITH 0 Plan2 WITH 0 Plan3 WITH 0 Plan4 WITH 0 ;
                              Plan5 WITH 0 Plan6 WITH 0 Plan7 WITH 0 Plan8 WITH 0 TotPlan WITH 0;
                              RA1   WITH 0 RA2   WITH 0 RA3   WITH 0 RA4   WITH 0 ;
                              RA5   WITH 0 RA6   WITH 0 RA7   WITH 0 RA8   WITH 0 TotRa   WITH 0 ;
                              Intrans1 WITH 0 Intrans2 WITH 0 Intrans3 WITH 0 Intrans4 WITH 0 ;
                              Intrans5 WITH 0 Intrans6 WITH 0 Intrans7 WITH 0 Intrans8 WITH 0 TotIntrn WITH 0 ;
                              Nwo1  WITH 0 Nwo2  WITH 0 Nwo3  WITH 0 Nwo4  WITH 0 ;
                              Nwo5  WITH 0 Nwo6  WITH 0 Nwo7  WITH 0 Nwo8  WITH 0 nTotWo WITH 0
                      *-- Call global function to add audit fields info.
                      =gfAdd_Info(lcTmpStyle)
                    ENDFOR
                  ENDIF
                ELSE
                  SELECT (lcTmpStyle)
                  APPEND BLANK
                  GATHER FROM laItemRec
                  *-- If there is no extended size scale, add only one style.
                  REPLACE STYLE     WITH SUBSTR(&lcTmpBom..Item,1,lnMajorLen) + lcNonMajor ;
                          cStyMajor WITH SUBSTR(&lcTmpBom..Item,1,lnMajorLen) ;
                          Ord1 WITH 0 Ord2 WITH 0 Ord3 WITH 0 Ord4 WITH 0 ;
                          Ord5 WITH 0 Ord6 WITH 0 Ord7 WITH 0 Ord8 WITH 0 TotOrd WITH 0 ;
                          Wip1 WITH 0 Wip2 WITH 0 Wip3 WITH 0 Wip4 WITH 0 ;
                          Wip5 WITH 0 Wip6 WITH 0 Wip7 WITH 0 Wip8 WITH 0 TotWip WITH 0 ;
                          Stk1 WITH 0 Stk2 WITH 0 Stk3 WITH 0 Stk4 WITH 0 ;
                          Stk5 WITH 0 Stk6 WITH 0 Stk7 WITH 0 Stk8 WITH 0 TotStk WITH 0 ;
                          Alo1 WITH 0 Alo2 WITH 0 Alo3 WITH 0 Alo4 WITH 0 ;
                          Alo5 WITH 0 Alo6 WITH 0 Alo7 WITH 0 Alo8 WITH 0 TotAlo WITH 0 ;
                          Shp1 WITH 0 Shp2 WITH 0 Shp3 WITH 0 Shp4 WITH 0 ;
                          Shp5 WITH 0 Shp6 WITH 0 Shp7 WITH 0 Shp8 WITH 0 TOTShp WITH 0
                  REPLACE Ret1  WITH 0 Ret2  WITH 0 Ret3  WITH 0 Ret4  WITH 0 ;
                          Ret5  WITH 0 Ret6  WITH 0 Ret7  WITH 0 Ret8  WITH 0 TOTRet WITH 0 ;
                          Plan1 WITH 0 Plan2 WITH 0 Plan3 WITH 0 Plan4 WITH 0 ;
                          Plan5 WITH 0 Plan6 WITH 0 Plan7 WITH 0 Plan8 WITH 0 TotPlan WITH 0;
                          RA1   WITH 0 RA2   WITH 0 RA3   WITH 0 RA4   WITH 0 ;
                          RA5   WITH 0 RA6   WITH 0 RA7   WITH 0 RA8   WITH 0 TotRa   WITH 0 ;
                          Intrans1 WITH 0 Intrans2 WITH 0 Intrans3 WITH 0 Intrans4 WITH 0 ;
                          Intrans5 WITH 0 Intrans6 WITH 0 Intrans7 WITH 0 Intrans8 WITH 0 TotIntrn WITH 0 ;
                          Nwo1  WITH 0 Nwo2  WITH 0 Nwo3  WITH 0 Nwo4  WITH 0 ;
                          Nwo5  WITH 0 Nwo6  WITH 0 Nwo7  WITH 0 Nwo8  WITH 0 nTotWo WITH 0
                  *-- Call global function to add audit fields info.
                  =gfAdd_Info(lcTmpStyle)
                ENDIF
              ENDIF
              *-- Go to previous record.
              SELECT STYLE
              IF lnSavRecNo > 0 .AND. lnSavRecNo <= RECCOUNT(lcItemFile)
                GOTO lnSavRecNo
              ENDIF
            ENDSCAN
          *** Case the current component uses different colors for the parent.
          CASE lcParent <> "******" .AND. lcCmpont <> "******"
            IF !SEEK(SUBSTR(&lcOrdLine..Style,1, lnMajorLen) + lcSeprator + lcParent , "STYLE")
              lnBomLnNo = lnBomLnNo - 1
              SELECT (lcTmpBom)
              REPLACE cStatus WITH "S"
              LOOP
            ENDIF
        ENDCASE
      ENDIF
      
      *-- If the flag was set to be deleted, delete the current cost item.
      SELECT (lcTmpBom)
      IF llDelRec
        lnBomLnNo = lnBomLnNo - 1
        REPLACE cStatus WITH "S"
      ENDIF
    ENDIF
    IF &lcTmpBom..cCatGTyp = "S" .AND. llMapSizes
      =lfMapSizes(&lcTmpBom..cItmMask , &lcTmpBom..Item)
    ENDIF
  ENDSCAN
  lcPrimFbr = lcCopPrFab
ENDIF
*-- Delete all the records was "S-> Same" status.
SELECT (lcTmpBom)
DELETE ALL FOR cStatus = "S"

IF llChkRclct
  SCAN
    *-- Calculate the total cost for the Mfg cost item or Duty 
    *-- cost item if it has percent.
    IF &lcTmpBom..cCatGTyp $ "MD" .AND. &lcTmpBom..nPercent <> 0
      *DO lfUpdPerc IN gcAppHome+"MFCstSc.Fxp" WITH &lcTmpBom..cCatgTyp
      =lfUpdPerc(&lcTmpBom..cCatgTyp)
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcTmpBom)
lnBomLnNo  = RECCOUNT()

WAIT CLEAR
CLEAR READ

*!*************************************************************
*! Name      : lfvSel
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : valid function for <Select> button in the copy 
*!           : from another style screen to select 1 item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSel()
*!*************************************************************
*
FUNCTION lfvSel
PRIVATE lnCopyRec , lcCatgory

SELECT (lcTmpCopy)
*-- Save record pointer in the temp. file.
lnCopyRec = RECNO(lcTmpCopy)

*-- If pointing to the header of any of the catgories, process all
*-- the current category records.
IF &lcTmpCopy..Typ = "*"
  lcCatgory = &lcTmpCopy..ccatgtyp
  *B603726,1 ABD Cheak if Catgory Price or Duty and style imported. [Begin]
  IF lcCatgory $ "PD" .AND. lcStyleTyp = 'I'
    = lfvSelDPCu ()
  ELSE
    lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
    REPLACE ALL cIncl WITH lcIncl ;
            FOR ccatgtyp = lcCatgory
  ENDIF
          
ELSE
  *-- Select or unselect the current cost item.
  REPLACE cIncl WITH IIF(EMPTY(cIncl) , "»" ," ")
  IF EMPTY(cIncl)
    lcCatgory = &lcTmpCopy..ccatgtyp
    LOCATE FOR Typ = "*" .AND. ccatgtyp = lcCatgory
    IF FOUND()
      REPLACE cIncl WITH " "
    ENDIF
  ENDIF
ENDIF

*-- Restore record no. in the temp file.
IF lnCopyRec > 0 .AND. lnCopyRec <= RECCOUNT(lcTmpCopy)
  GOTO lnCopyRec IN (lcTmpCopy)
ENDIF

*-- Refresh the copy browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!*************************************************************
*! Name      : lfvNone
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
*! Purpose   : valid function for <None> button in the copy 
*!           : from another style screen to unselect all items.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNone()
*!*************************************************************
*
FUNCTION lfvNone
PRIVATE lnCopyRec

SELECT (lcTmpCopy)
*-- Save record pointer in the temp. file.
lnCopyRec = RECNO(lcTmpCopy)

*-- Unselect all the records.
REPLACE ALL cIncl WITH " "

*-- Restore record no. in the temp file.
IF lnCopyRec > 0 .AND. lnCopyRec <= RECCOUNT(lcTmpCopy)
  GOTO lnCopyRec IN (lcTmpCopy)
ENDIF

*-- Refresh the copy browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : valid function for <Invert> button in the copy 
*!           : from another style screen to invert selected &
*!           : unseletced item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvInvert()
*!*************************************************************
*
FUNCTION lfvInvert
PRIVATE lnCopyRec

SELECT (lcTmpCopy)
*-- Save record pointer in the temp. file.
lnCopyRec = RECNO(lcTmpCopy)

*-- Invert the selected records.
REPLACE ALL cIncl WITH IIF(EMPTY(cIncl) , "»" ," ")

*-- Restore record no. in the temp file.
IF lnCopyRec > 0 .AND. lnCopyRec <= RECCOUNT(lcTmpCopy)
  GOTO lnCopyRec IN (lcTmpCopy)
ENDIF

*-- Refresh the copy browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!*************************************************************
*! Name      : lfvAll
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : valid function for <Select All> button in the copy 
*!           : from another style screen to select All item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAll()
*!*************************************************************
*
FUNCTION lfvAll
PRIVATE lnCopyRec

SELECT (lcTmpCopy)

*-- Save record pointer in the temp. file.
lnCopyRec = RECNO(lcTmpCopy)

IF lcStyleTyp = 'I'
  = lfvAelDPCu()
ELSE
  *-- Select all the cost items.
  REPLACE ALL cIncl WITH "»"
ENDIF
*-- Restore record no. in the temp file.
IF lnCopyRec > 0 .AND. lnCopyRec <= RECCOUNT(lcTmpCopy)
  GOTO lnCopyRec IN (lcTmpCopy)
ENDIF

*-- Refresh the copy browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!*************************************************************
*! Name      : lfvCanCop
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to cancel the copying from another item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCanCop()
*!*************************************************************
*
FUNCTION lfvCanCop

*-- Set flag to know that the cancel was pressed & nothing was copied.
llCopy    = .F.
lnBomLnNo = 0

*!*************************************************************
*! Name      : lfBrwCopy
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Browse Cost items for the selected style/fabric
*!           : to copy from.
*!*************************************************************
*! Calls     : lfWhenCopy
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrwCopy()
*!*************************************************************
*
FUNCTION lfBrwCopy

SELECT (lcTmpCopy)
lnMarkC = RECNO()

*-- Brow the copied items in the copy browse in the copy screen.
BROWSE FIELDS cMarker = IIF(RECNO()=lnMarkC,'>',' '):H=' ':R:1,;
       cIncl      :H=" " :R:1 ,;
       cItmMask   :H= lcNonMjHdr :R:23,;
       Item       :H= 'Item'     :R:23  ,;
       IClr       :H= 'I. Clr'   :R:9   ,;
       lcMFGDesc = IIF(Typ="*" ,"" ,gfCodDes(MFGCODE , 'MFGCODE')):H= 'MFG Code' :R:12 ,;
       Desc       :H= 'Description' :R:12 ;
       WHEN lfWhenCopy() ;
       VALID :F lfvCopBrow();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW MFStyCp0   ;
       IN WINDOW AWDMFStyCp ;
       TITLE lcBrCpItmT

SHOW WINDOW (lcBrCpItmT) REFRESH

*!*************************************************************
*! Name      : lfWhenCopy
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : When function for browse copy style cost sheet.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfWhenCopy()
*!*************************************************************
*
FUNCTION lfWhenCopy

*-- Set the browse pointer.
lnMarkC    = RECNO(lcTmpCopy)

glFromBrow = .T.

*-- Refresh the browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!*************************************************************
*! Name      : lfMapSizes
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcPrntSty -> Parent Style
*!           : lcChldSty -> Component Style
*!*************************************************************
*! Returns   : lcMapSize -> String hold the sizes cross reference.
*!*************************************************************
*! Example   : =lfMapSizes(lcPrntSty , lcChldSty)
*!*************************************************************
*! Modifications :
*!*************************************************************
*
FUNCTION lfMapSizes
PARAMETERS lcPrntSty , lcChldSty
PRIVATE lcPrntSty , lcChldSty , laPrntScal , lcPrntScal , ;
        lcCurSize , lnCnt , lcCnt , lcCurSize , laChldScal , ;
        lcCrosRef , lnChldCnt , lnCount , lcCrosRef , lnSavSty

*-- Array hold the Parent style scale.
DECLARE laPrntScal[1]
laPrntScal = ""
lcPrntScal = ""

*-- Save record pointer for the style file.
lnSavSty = RECNO("STYLE")

*-- Collect all the sizes for the parent scales..
*-- Get all the available scale for the current mask.
SELECT STYLE
SET ORDER TO TAG STYLE
=SEEK(PADR(lcPrntSty,lnMajorLen))
SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(lcPrntSty,lnMajorLen) ;
            FOR LIKE(STRTRAN(lcPrntSty , "*" , "?") , STYLE.Style)
  *-- Get the scale info.
  IF SEEK("S" + Style.Scale , "SCALE")
    *-- Get the sizes for the current scale.
    lcCurSize = ""
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt     = STR(lnCnt,1)
      lcCurSize = lcCurSize + IIF(EMPTY(lcCurSize) , "" , ",") + lcCnt
      
      *-- Insert the current scale to the scale array.
      IF ASCAN(laPrntScal , PADR(Scale.Scale,3) + "," + lcCnt) = 0
        IF !EMPTY(laPrntScal[1])
          DECLARE laPrntScal[ALEN(laPrntScal,1)+1]
        ENDIF
        laPrntScal[ALEN(laPrntScal,1)] = PADR(Scale.Scale,3) + "," + lcCnt
      ENDIF
    ENDFOR
    
    *-- Get the string to be fill in the mSizes field.
    IF !(PADR(Scale.Scale,3) $ lcPrntScal)
      lcPrntScal = lcPrntScal + PADR(Scale.Scale,3) + "~" + lcCurSize +CHR(13)
    ENDIF
  ENDIF
ENDSCAN

*-- Fill the array that hold the scales of the style component.
DECLARE laChldScal[1]
laChldScal[1] = ""

*-- Scan to collect the different scales of the style component.
SELECT STYLE
=SEEK(PADR(lcChldSty , lnMajorLen) , "STYLE")
SCAN REST WHILE PADR(Style , lnMajorLen) = PADR(lcChldSty , lnMajorLen) ;
            FOR LIKE(STRTRAN(lcChldSty , "*" , "?") , STYLE.Style)
  
  *-- Search for the current style scale.
  IF SEEK("S" + Style.Scale , "SCALE")
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt   = STR(lnCnt,1)
      IF ASCAN(laChldScal , PADR(Style.Scale,3) + "," +lcCnt) = 0
        IF !EMPTY(laChldScal[1])
          DECLARE laChldScal[ALEN(laChldScal,1)+1]
        ENDIF
        *-- Hold the current scale + its size.
        laChldScal[ALEN(laChldScal,1)] = PADR(Style.Scale,3) + "," +lcCnt
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN

lcCrosRef = ""
lnChldCnt = 1
FOR lnCount = 1 TO ALEN(laPrntScal,1)
  lcCrosRef = lcCrosRef + laPrntScal[lnCount] + "~" + laChldScal[lnChldCnt] + CHR(13)
  
  *-- Adjust the child array counter.
  lnChldCnt = lnChldCnt + IIF(lnChldCnt < ALEN(laChldScal,1) , 1 , 0)
ENDFOR

*-- Restore the record pointer in the style file.
SELECT STYLE
IF lnSavSty > 0 .AND. lnSavSty <= RECCOUNT()
  GOTO lnSavSty
ENDIF

*-- Update the sizes in the bom file.
SELECT (lcTmpBom)
REPLACE mSizes     WITH lcPrntScal ;
        mSzCrosRef WITH lcCrosRef

*!*************************************************************
*! Name      : lfCopyAct
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Activate function for the copy screen..
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCopyAct()
*!*************************************************************
*! Modifications :
*!*************************************************************
*
FUNCTION lfCopyAct

*-- Clear browse as you are coming out of a browse window
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP() , lcBrCpItmT)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name      : lfCopyDAct
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Deactivate function for the copy screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCopyDAct()
*!*************************************************************
*! Modifications :
*!*************************************************************
*
FUNCTION lfCopyDAct

glFromBrow = WONTOP() = lcBrCpItmT

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB        DO lpCopyTab
  ON KEY LABEL BACKTAB    DO lpCopyBTab
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpCopyTab
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Trap of tab key for the copy screens...
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpCopyTab
*!*************************************************************
*
PROCEDURE lpCopyTab

ON KEY LABEL TAB
*-- Point to the current object from the browse if press tab from the browse.
ACTIVATE WINDOW MfStyCp1
_CUROBJ = OBJNUM(pbSel)
ON KEY LABEL TAB DO lpCopyTab

*!*************************************************************
*! Name      : lpCopyBTab
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Trap of backtab key for the scopy screen...
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpCopyBTab
*!*************************************************************
*
PROCEDURE lpCopyBTab

ON KEY LABEL BACKTAB

*-- Point to the current object from the browse if press 
*-- Shift + tab from the browse.
ACTIVATE WINDOW MfStyCp2
_CUROBJ = OBJNUM(ibTabCop)
    
ON KEY LABEL BACKTAB DO lpCopyBTab

*!*************************************************************
*! Name      : lfvCopBrow
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Valid function for the copy browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCopBrow()
*!*************************************************************
*
FUNCTION lfvCopBrow

IF !WONTOP(lcBrCpItmT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*!*************************************************************
*! Name      : lfvSelDPCu
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to get the price for the copy style.
*!           : In case of sellect one cost item
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None.
*!*************************************************************
FUNCTION lfvSelDPCu

STORE '' To lcPricecu1 , lcPricecu2 , lcDutyCur1 , lcDutyCur2

lnAlias = SELECT(0) 
IF EMPTY(Citmmajor)
  LOCATE FOR !EMPTY(Citmmajor) .AND. ccatgtyp = lcCatgory
ENDIF
SELECT STYLE
lcSaveOrd = Order()
SET ORDER TO TAG Cstyle 

=SEEK(lcCopFmSty,'STYLE')
lcPricecu1 = Style.cPricecur
lcDutyCur1 = Style.cdutycur

=SEEK(&lcTmpCopy..Citmmajor,'STYLE')
lcPricecu2 = Style.cPricecur
lcDutyCur2 = Style.cdutycur

IF lcCatgory = 'D' 
  DO CASE
    CASE lcDutyCur1 = lcDutyCur2  .AND. lcPricecu1 = lcPricecu2
      SELECT (lcTmpCopy)
      lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
      REPLACE ALL cIncl WITH lcIncl ;
              FOR ccatgtyp = lcCatgory

    CASE lcDutyCur1 <> lcDutyCur2
      = gfModalGen('INM34174B00000','DIALOG',IIF(lcCatgory = 'P','Price currency','Duty currency'))

    CASE lcDutyCur1 =  lcDutyCur2 .AND. lcPricecu1 <> lcPricecu2
      IF lcDutyCur1 = lcPricecu1
        = gfModalGen('INM34174B00000','DIALOG',IIF(lcCatgory = 'P','Price currency','Duty currency'))
      ELSE
        SELECT (lcTmpCopy)
        lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
        REPLACE ALL cIncl WITH lcIncl ;
                FOR ccatgtyp = lcCatgory
      ENDIF
  ENDCASE
ELSE
  *-- Cheak if duty currency and price currency different.
  IF lcPricecu1 = lcPricecu2
    SELECT (lcTmpCopy)
    lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
    REPLACE ALL cIncl WITH lcIncl ;
            FOR ccatgtyp = lcCatgory
  ELSE
    = gfModalGen('INM34174B00000','DIALOG',IIF(lcCatgory = 'P','Price currency','Duty currency'))
  ENDIF
ENDIF
SELECT STYLE
SET ORDER TO TAG &lcSaveOrd
SELECT(lnAlias)
*-- End Of lfvSelDPCu.
*!*************************************************************
*! Name      : lfvAelDPCu
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to get the price for the copy style.
*!           : In case of select all cost item
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None.
*!*************************************************************
FUNCTION lfvAelDPCu
PRIVATE lcCatgory
STORE .F. To llDifPcurr ,llDifDCurr
STORE '' To lcPricecu1 , lcPricecu2 ,lcCurrMesg ,lcCatgory,;
            lcDutyCur1 , lcDutyCur2

lnAlias = SELECT(0) 
SELECT STYLE
lcSaveOrd = Order()
SET ORDER TO TAG Cstyle 

SELECT (lcTmpCopy)
SCAN FOR cStatus = 'A'
  lcCatgory = &lcTmpCopy..cCatgtyp
  
  =SEEK(lcCopFmSty,'STYLE')
  lcPricecu1 = Style.cPricecur
  lcDutyCur1 = Style.cdutycur

  =SEEK(&lcTmpCopy..Citmmajor,'STYLE')
  lcPricecu2 = Style.cPricecur
  lcDutyCur2 = Style.cdutycur
  DO CASE 
    CASE lcCatgory = "D"
      DO CASE
        CASE lcDutyCur1 = lcDutyCur2  .AND. lcPricecu1 = lcPricecu2
          SELECT (lcTmpCopy)
          lnSaveRec = RECNO()
          REPLACE ALL cIncl WITH '»' ;
                  FOR ccatgtyp = lcCatgory
          GOTO lnSaveRec
        CASE lcDutyCur1 <> lcDutyCur2
          llDifDCurr = .T.  
        CASE lcDutyCur1 =  lcDutyCur2 .AND. lcPricecu1 <> lcPricecu2
          IF  lcDutyCur1 = lcPricecu1
            llDifDCurr = .T.
          ELSE
            SELECT (lcTmpCopy)
            lnSaveRec = RECNO()
            REPLACE ALL cIncl WITH '»' ;
                    FOR ccatgtyp = lcCatgory
            GOTO lnSaveRec
          ENDIF
      ENDCASE
    CASE lcCatgory = "P"  
      IF lcPricecu1 = lcPricecu2
        SELECT (lcTmpCopy)
        lnSaveRec = RECNO()
        REPLACE ALL cIncl WITH '»' ;
                FOR ccatgtyp = lcCatgory
        GOTO lnSaveRec
      ELSE    
        llDifPcurr = .T.
      ENDIF
    OTHERWISE
      SELECT (lcTmpCopy)
      lnSaveRec = RECNO()
      REPLACE ALL cIncl WITH '»' ;
              FOR ccatgtyp = lcCatgory
      GOTO lnSaveRec
  ENDCASE  
ENDSCAN  

DO CASE
  CASE llDifPcurr .AND. !llDifDCurr 
    lcCurrMesg  = 'Price currency'
  CASE !llDifPcurr .AND. llDifDCurr 
    lcCurrMesg  = 'Duty currency'
  CASE llDifPcurr .AND. llDifDCurr 
    lcCurrMesg  = 'Price currency and Duty currency'
ENDCASE

IF !EMPTY(lcCurrMesg)
  *B603726,1 Message : 34174.
  *B603726,1 There is a mismatch in XXX , Unable to copy this cost item.
  *B603726,1 Button  : 00000
  *B603726,1 Ok
  = gfModalGen('INM34174B00000','DIALOG',lcCurrMesg)
ENDIF     

SELECT STYLE
SET ORDER TO TAG &lcSaveOrd
SELECT(lnAlias)
*-- End Of lfvAelDPCu.

*!*************************************************************
*! Name      : lfGetCopy
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : collecting the copy records
*!*************************************************************
*! Example   : = lfGetCopy()
*!*************************************************************
FUNCTION lfGetCopy
PRIVATE lcOrder, lcStyKey, lcStyScale, llDifScale, llExit, lnMsg
STORE .F. TO llDifScale, llExit
IF lcStyleTyp <> 'T'
  SELECT STYLE
  lcStyKey = EVALUATE(KEY())
  =SEEK(PADR(lcStyMaj,lnItemLen))
  lcStyScale = SCALE
  =SEEK(PADR(lcCopFmSty,lnItemLen))
  llDifScale = (lcStyScale#SCALE)
  lcCpyScale = SCALE
  =SEEK(lcStyKey)
ENDIF
SELECT (lcOrdLine)
lcOrdlnOrd = ORDER()
SET ORDER TO Ordlines
SELECT BOM
lcOrder = SET('ORDER')
SET ORDER TO TAG BOM
lnBomLnNo = 0
IF SEEK(PADR(lcCopFmSty,lnItemLen))
  SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = PADR(lcCopFmSty,lnItemLen);
            FOR PADR(Item,lnItemLen) <> PADR(lcStyMaj,lnItemLen)
    IF llDifScale .AND. LBASONSIZ .AND. !llExit
      lnMsg = gfModalGen("QRM00000B00006" , "DIALOG" , '' , '' , 'This style ';
                         +'has a style cost sheet by size and uses a different scale. '+;
                         'Do you want to copy the cost sheet without sizes?')
      IF lnMsg = 1
        llExit = .T.
      ELSE
        lnBomLnNo = 0
        EXIT
      ENDIF
    ENDIF
    IF SUBSTR(cItmMask,lnMajorLen+2) <>'******'
      lcStySek = PADR(STUFF(cItmMask,1,lnMajorLen,PADR(&lcOrdSty..Style,lnMajorLen)),19) 
      IF !SEEK("O"+lcOrderNo+lcStySek, lcOrdLine)      
        LOOP
      ENDIF
    ENDIF
    
    lnBomLnNo = lnBomLnNo + 1
    SCATTER MEMO MEMVAR
    m.cStatus = 'A'
    m.nRecNo  = RECNO()
    IF llExit
      lnWavePos = AT('~',m.mSizes)
      lcSizes = SUBSTR(m.mSizes,lnWavePos+1)
      llAllSizes = .T.
      IF SEEK('S'+lcCpyScale,'SCALE')
        FOR lnI = 1 TO SCALE.CNT
          lcI = STR(lnI,1)
          IF !(lcI $ lcSizes)
            llAllSizes = .F.
            EXIT
          ENDIF
        ENDFOR
        IF llAllSizes
          IF SEEK('S'+lcStyScale,'SCALE')
            m.mSizes = lcStyScale + '~'
            FOR lnI = 1 TO SCALE.CNT
              lcI = STR(lnI,1)
              m.mSizes = m.mSizes + lcI + IIF(lnI=SCALE.CNT,'',',')
            ENDFOR
          ENDIF
        ELSE
          m.lBasOnSiz = .F.
          m.mSizes = SPACE(0)
        ENDIF
      ENDIF
      
    ENDIF
    SELECT (lcTmpCopy)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDSCAN
ENDIF
SELECT BOM
SET ORDER TO &lcOrder.
SELECT (lcTmpCopy)
*--end of lfGetCopy.
*!*************************************************************
*! Name      : lfUpdExRat
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Update Exchange Rate With latest EX rate for Curr
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfUpdExRat()
*!*************************************************************
*
FUNCTION lfUpdExRat

SELECT (lcTmpBom)
LOCATE
SCAN
  IF cCostStat $ "123"
    =lfUpdCost(-1 , 0)
  ENDIF
  *-- Get purchase price currency, duty currency or base currency
  
  lcCurrency =  IIF(&lcTmpBom..cCatgTyp='P' , Style.cPriceCur , ;
                   IIF(&lcTmpBom..cCatgTyp $ 'MD' , Style.cDutyCur , ;
                   IIF(&lcTmpBom..cCatgTyp $ 'FT',cCurrCode,gcBaseCurr)))
  lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  *-- Get currency unit and exchange rate. 
  llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
  lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  *-- Get currency symbol.
  llFound    = SEEK(lcCurrency , 'SycCurr')
  lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
  STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
  lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
  lnTotEquCst =  ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2)

  IF &lcTmpBom..cCatgTyp='D'
    lnTotEquCst =  (&lcTmpBom..UntCost * lnExRate) /nExRate
    REPLACE UntCost WITH lnTotEquCst,;
            TotCost WITH lnTotEquCst

    REPLACE nExRate WITH lnExRate,;
            nExCost WITH lnTotEquCst,;
            cStatus WITH IIF(cStatus = "S" , "M" , cStatus)
  ENDIF
  
  IF &lcTmpBom..cCatgTyp $ 'FT'
      REPLACE cfabcurr   WITH lcCurrency,;
              nFabExRate WITH lnExRate
  ENDIF

  IF cCostStat $ "123"
    =lfUpdCost(1 , 0) 
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfUpdPerc
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to update the cost for dutyable or MFG
*!           : cost item & has a percent.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcCatgType-> Category type. (F,S,T,P,D,M)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdPerc()
*!*************************************************************
*
FUNCTION lfUpdPerc
PARAMETERS lcCatgType
PRIVATE lnCurRec , lnStyRec , lnPercent , lnCurCst , lnCrTotCst , ;
        lnTotSizes , laCurSize , lnShardSiz , lcCurScal , lcCurSize , ;
        lcLinFound , lnSizPos , lcScanExp , lcMSize , lcCSize , ;
        lcCScale , lcChldScal , lcCatgType
*-- If there is percent in any MFG or Duty cost item, calculate the
*-- cost from the participated items.

IF lcCatgType $ 'DM'
  SELECT STYLE
  SET ORDER TO TAG STYLE
  lnStyRec   = RECNO()     && Save the style record pointer.
  SELECT (lcTmpBom)
  lnCurRec   = RECNO()     && Save the BOM record pointer.
  lcItmMask  = cItmMask    && Save the current item mask.
  lnPercent  = nPercent    && Save the percent of the current MFG or current Duty.
  lnCurCst   = 0           && Default the total cost of the current MFG or current Duty.
  lnCrTotCst = 0           && Var. to increment the total cost of current MFG or current Duty.
  lnTotSizes = 0           && Var. hold the total sizes of the current MFG or current Duty.
  DECLARE laCurSize[1]     && Array hold the scale sizes' of the current MFG or current Duty.
  laCurSize  = ""
  
  *-- If style cost sheet based on sizes, based on sizes.
  IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
    *-- If there is selected sizes for the current cost item.
    FOR lnCount = 1 TO MEMLINES(ALLTRIM(&lcTmpBom..mSizes))
      *-- Fill the current cost item scale sizes array.
      IF !EMPTY(laCurSize[1])
        DIMENSION laCurSize[ALEN(laCurSize,1)+1]
      ENDIF
      laCurSize[ALEN(laCurSize,1)] = MLINE(ALLTRIM(&lcTmpBom..mSizes) , lnCount)
      lnTotSizes = lnTotSizes + OCCURS("," , laCurSize[ALEN(laCurSize,1)]) +1
    ENDFOR
  ELSE
    *-- If there is no selected sizes, fill the scale sizes array 
    *-- for the current cost item with the right scales.
    STORE "" TO lcMSize
    SELECT STYLE
    =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
              FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
      IF SEEK("S" + Style.Scale , "SCALE")
        STORE "" TO lcMSize
        FOR lnCnt  = 1 TO Scale.Cnt
          lcCnt    = STR(lnCnt,1)
          lcMSize  = lcMSize + IIF(EMPTY(lcMSize) , "" , ",") + lcCnt
        ENDFOR
        
        IF ASCAN(laCurSize, PADR(Scale.Scale,3)) = 0
          IF !EMPTY(laCurSize[1])
            DIMENSION laCurSize[ALEN(laCurSize,1)+1]
          ENDIF
          laCurSize[ALEN(laCurSize,1)] = PADR(Scale.Scale,3) + "~" + lcMSize
          lnTotSizes = lnTotSizes + OCCURS("," , laCurSize[ALEN(laCurSize,1)]) +1
        ENDIF
      ENDIF
    ENDSCAN
    SELECT (lcTmpBom)
  ENDIF
  
  *-- Prepare the scan expression.
  *lcScanExp = IIF(lcCatgType = "M" , "&lcTmpBom..cCatGTyp = 'P'" , "&lcTmpBom..cCostStat = '1'")
  lcScanExp = IIF(lcCatgType = "M" , "&lcTmpBom..cCostStat $ '23'" , "&lcTmpBom..cCostStat $ '13'")

  *-- Scan in the BOM for all the price cost items record to 
  *-- calculate the current Mfg cost item or current Duty cost item.
  SELECT (lcTmpBom)
  SCAN FOR &lcScanExp .AND. ;
           (LIKE(STRTRAN(lcItmMask , "*" , "?") , &lcTmpBom..cItmMask) .OR. ;
            LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , lcItmMask))
    IF EMPTY(ALLTRIM(&lcTmpBom..mSizes))
      STORE "" TO lcCSize , lcCScale , lcChldScal
      SELECT STYLE
      =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
      SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
                FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
        IF SEEK("S" + Style.Scale , "SCALE")
          STORE "" TO lcCSize , lcCScale
          FOR lnCnt  = 1 TO Scale.Cnt
            lcCnt    = STR(lnCnt,1)
            lcCSize  = lcCSize + IIF(EMPTY(lcCSize) , "" , ",") + lcCnt
          ENDFOR
          lcCScale   = PADR(Scale.Scale,3) + "~" + lcCSize + CHR(13)
          lcChldScal = lcChldScal + lcCScale
        ENDIF
      ENDSCAN
      SELECT (lcTmpBom)
    ELSE
      lcChldScal = ALLTRIM(&lcTmpBom..mSizes)
    ENDIF
    
    *-- Var. hold the no. of sizes shared between the current 
    *-- cost items (Mfg - Price) or (Duty & Dutyable records). 
    lnShardSiz = 0
    
    *-- Get the shared sizes between the current Mfg cost item &
    *-- the current price cost item <OR> current Duty cost item &
    *-- the current dutyable record.
    FOR lnCount = 1 TO ALEN(laCurSize,1)
      
      *-- Get the scale of the current Mfg or current Duty from the current 
      *-- MFG scale array row or current Duty scale array row.
      lcCurScal = SUBSTR(laCurSize[lnCount],1,3)
      lcCurSize = SUBSTR(laCurSize[lnCount],5)
      *-- Check if this scale exists in the current price cost item sizes.
      IF ATCLINE(lcCurScal , lcChldScal) > 0
        
        *-- If the scale was found, get the contents of this line.
        lcLinFound = MLINE(ALLTRIM(lcChldScal) , ATCLINE(lcCurScal , lcChldScal))
        
        *-- Check if there is any shared sizes.
        lnSizPos   = 1
        FOR lnCnt2 = 1 TO OCCURS("," , lcCurSize) + 1
          IF ("~"+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound) .OR. (","+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound)
            lnShardSiz = lnShardSiz + 1
          ENDIF
          *-- Increment the size position with 2.
          lnSizPos = lnSizPos + 2
        ENDFOR
      ENDIF
    ENDFOR
    
    *-- Get the percentage of the current line cost multiple by shared sizes.
    lnCurCst   = ((&lcTmpBom..UntCost * &lcTmpBom..nBomTotQty * lnPercent)/100) * lnShardSiz
    *--lnCrOrgCst Current cost for spacific item
    
    
    *-- Change the cost of the current item to the equevelant currency.
    IF lcStyleTyp = "I" .AND. llMulCurr .AND. !(&lcTmpBom..cCurrCode = gcBaseCurr);
      AND (&lcTmpBom..cCatGTyp $ "FTP")
      *llFound    = SEEK(gcBaseCurr + &lcTmpBom..cCurrCode , "SYCEXCH")
      *lnPExRate  = IIF(llFound , SYCEXCH.nExRate , 1)
      lnPExRate  =  IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
      
      llFound    = SEEK(&lcTmpBom..cCurrCode , "SYCCURR")
      lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
      lcPRatSign = "*"
      lcPUntSign = "/"
      lcPRatSign = gfGetExSin(@lcPUntSign , &lcTmpBom..cCurrCode)
      
      lnCurCst   = lnCurCst &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
    ENDIF
    
    *-- Increment the var that hold all the lines cost.
    lnCrTotCst = lnCrTotCst + lnCurCst
  ENDSCAN
  
  *-- Divide the calculated total cost by the no. of all sizes in the
  *-- current MFG or current Duty.
  lnCrTotCst = lnCrTotCst / lnTotSizes
  
  *-- Restore record no.
  IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
    GOTO lnCurRec IN (lcTmpBom)
  ENDIF
  *-- Update the current MFG or current Duty with the right total cost.
  REPLACE UntCost WITH lnCrTotCst ;
          TOTCOST WITH UNTCOST * nBomTotQty
  
  *-- If adding style cost sheet only.
  IF lcStyleTyp <> "T"
    *-- Call local function to calculate the current parent cost in its
    *-- parent records...
    =lfPrntCost(&lcTmpBom..cItmMask)
  ENDIF
  
  *-- Restore record no.
  IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
    GOTO lnCurRec IN (lcTmpBom)
  ENDIF
  
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF
  SET ORDER TO TAG CSTYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfUpdCost
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to update the cost for duty or MFG
*!           : cost item & has a percent if there is a new 
*!           : added price cost item or dutyable cost item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnSign -> (-1 or +1)
*!             lnOld  -> old value to be subtracted.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdCost()
*!*************************************************************
*
FUNCTION lfUpdCost
PARAMETERS lnSign , lnOld
PRIVATE lnCurRec , lnStyRec , lnCurCst , lnTotSizes , laCurSize , ;
        lnShardSiz , lcCurScal , lcCurSize , lcLinFound , lnSizPos , ;
        lcScanExp , lcMSize , lcCSize , lcCScale , lcChldScal , ;
        lcCurType , lnSign , lnOld , laPrntCost , lnCnt1
lnStyRec   = RECNO("STYLE")         && Save the style record pointer.
SET ORDER TO TAG STYLE IN STYLE     && Change the style index.
lnCurRec   = RECNO(lcTmpBom)        && Save the BOM record pointer.
lcCurType  = &lcTmpBom..cCatGTyp    && Save the current cost type.
lcItmMask  = &lcTmpBom..cItmMask    && Save the current item mask.
lnCurCst   = &lcTmpBom..TotCost     && Default the total cost of the current MFG or current Duty.
*-- MHM Get Total cost in base Currancy
IF lcStyleTyp = "I" .AND. llMulCurr .AND. !(&lcTmpBom..cCurrCode = gcBaseCurr)
  
  lnPExRate  = IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
  
  llFound    = SEEK(&lcTmpBom..cCurrCode , "SYCCURR")
  lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
  lcPRatSign = "*"
  lcPUntSign = "/"
  lcPRatSign = gfGetExSin(@lcPUntSign , &lcTmpBom..cCurrCode)
  lnPExRate = IIF(&lcTmpBom..nExRate<>0,&lcTmpBom..nExRate,1)
  lnCurCst = lnCurCst &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
ENDIF

lnTotSizes = 0                      && Var. hold the total sizes of the current MFG or current Duty.
DECLARE laCurSize[1]                && Array hold the scale sizes' of the current MFG or current Duty.
laCurSize  = ""

*-- Define array to hold all the updated parent cost to update its parents cost.
DECLARE laPrntCost[1]
laPrntCost = ""

*-- If style cost sheet based on sizes, based on sizes.
IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
  *-- If there is selected sizes for the current cost item.
  FOR lnCount = 1 TO MEMLINES(ALLTRIM(&lcTmpBom..mSizes))
    *-- Fill the current cost item scale sizes array.
    IF !EMPTY(laCurSize[1])
      DIMENSION laCurSize[ALEN(laCurSize,1)+1]
    ENDIF
    laCurSize[ALEN(laCurSize,1)] = MLINE(ALLTRIM(&lcTmpBom..mSizes) , lnCount)
  ENDFOR
ELSE
  *-- If there is no selected sizes, fill the scale sizes array 
  *-- for the current cost item with the right scales.
  STORE "" TO lcMSize
  SELECT STYLE
  =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
            FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
    IF SEEK("S" + Style.Scale , "SCALE")
      STORE "" TO lcMSize
      FOR lnCnt  = 1 TO Scale.Cnt
        lcCnt    = STR(lnCnt,1)
        lcMSize  = lcMSize + IIF(EMPTY(lcMSize) , "" , ",") + lcCnt
      ENDFOR
      
      IF ASCAN(laCurSize, PADR(Scale.Scale,3)) = 0
        IF !EMPTY(laCurSize[1])
          DIMENSION laCurSize[ALEN(laCurSize,1)+1]
        ENDIF
        laCurSize[ALEN(laCurSize,1)] = PADR(Scale.Scale,3) + "~" + lcMSize
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcTmpBom)
ENDIF

*-- Prepare the scan expression.
DO CASE
  CASE  &lcTmpBom..cCostStat = "1"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'D'"

  CASE  &lcTmpBom..cCostStat = "2"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'M'"
    
  CASE  &lcTmpBom..cCostStat = "3"
    
    IF &lcTmpBom..cCatGTyp = "D"
      lcScanExp = "&lcTmpBom..cCatGTyp = 'M'"
    ENDIF  
    
    IF &lcTmpBom..cCatGTyp = "M"
      lcScanExp = "&lcTmpBom..cCatGTyp = 'D'"
    ENDIF
      
    IF !(&lcTmpBom..cCatGTyp $ "DM")
      lcScanExp = "&lcTmpBom..cCatGTyp = 'M' OR &lcTmpBom..cCatGTyp = 'D'"
    ENDIF    
    
  OTHERWISE
    lcScanExp = "&lcTmpBom..cCatGTyp = 'D'"
ENDCASE

*-- Scan in the BOM for all the price cost items record to 
*-- calculate the current Mfg cost item or current Duty cost item.
SELECT (lcTmpBom)
SCAN FOR &lcScanExp .AND. &lcTmpBom..nPercent <> 0 .AND. ;
         (LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , lcItmMask) .OR. ;
          LIKE(STRTRAN(lcItmMask , "*" , "?") , &lcTmpBom..cItmMask))
   *-- Var. hold the no. of sizes shared between the current 
  *-- cost items (Mfg - Price) or (Duty & Dutyable records). 
  lnShardSiz = 0
  lnTotSizes = 0
  IF EMPTY(ALLTRIM(&lcTmpBom..mSizes))
    STORE "" TO lcCSize , lcCScale , lcChldScal
    SELECT STYLE
    =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
              FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
      IF SEEK("S" + Style.Scale , "SCALE")
        STORE "" TO lcCSize , lcCScale
        FOR lnCnt  = 1 TO Scale.Cnt
          lcCnt    = STR(lnCnt,1)
          lcCSize  = lcCSize + IIF(EMPTY(lcCSize) , "" , ",") + lcCnt
        ENDFOR
        lcCScale   = PADR(Scale.Scale,3) + "~" + lcCSize + CHR(13)
        IF !(lcCScale $ lcChldScal)
          lcChldScal = lcChldScal + lcCScale
        ENDIF
      ENDIF
    ENDSCAN
    SELECT (lcTmpBom)
  ELSE
    lcChldScal = ALLTRIM(&lcTmpBom..mSizes)
  ENDIF
  
  *-- Get the shared sizes between the current Mfg cost item &
  *-- the current price cost item <OR> current Duty cost item &
  *-- the current dutyable record.
  FOR lnCount = 1 TO ALEN(laCurSize,1)
    
    *-- Get the scale of the current Mfg or current Duty from the current 
    *-- MFG scale array row or current Duty scale array row.
    lcCurScal = SUBSTR(laCurSize[lnCount],1,3)
    lcCurSize = SUBSTR(laCurSize[lnCount],5)
    *-- Check if this scale exists in the current price cost item sizes.
    IF ATCLINE(lcCurScal , lcChldScal) > 0
      
      *-- If the scale was found, get the contents of this line.
      lcLinFound = MLINE(ALLTRIM(lcChldScal) , ATCLINE(lcCurScal , lcChldScal))
      
      *-- Check if there is any shared sizes.
      lnSizPos   = 1
      FOR lnCnt2 = 1 TO OCCURS("," , lcCurSize) + 1
        IF ("~"+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound) .OR. (","+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound)
          lnShardSiz = lnShardSiz + 1
        ENDIF
        *-- Increment the size position with 2.
        lnSizPos = lnSizPos + 2
      ENDFOR
    ENDIF
  ENDFOR
  
  FOR lnCount = 1 TO MEMLINES(lcChldScal)
    lnTotSizes = lnTotSizes + OCCURS("," , MLINE(lcChldScal,lnCount)) +1
  ENDFOR
  
  *-- Update the current MFG or current Duty with the right total cost.
  *-- Get the percentage of the current line cost multiple by shared sizes.
  *-- & divide the calculated total cost by the no. of all sizes in the
  *-- current MFG or current Duty.
  lnUpdCost = (((((lnCurCst-lnOld) * &lcTmpBom..nPercent)/100) * lnShardSiz) / lnTotSizes)  
  
  *-- Change the cost of the current item to the equevelant currency.
  lnUpdCost = UntCost + (lnSign * lnUpdCost)
  
  *-- Save the updated cost.
  
  REPLACE UntCost WITH lnUpdCost ;
          TOTCOST WITH UNTCOST * nBomTotQty;
          CSTATUS WITH IIF(CSTATUS = "S" , "M" , CSTATUS)
  
  *-- Get all the parent records that updated their cost in one array.
  IF ASCAN(laPrntCost , &lcTmpBom..cItmMask) = 0
    IF !EMPTY(laPrntCost[1])
      DIMENSION laPrntCost[ALEN(laPrntCost,1)+1]
      =AINS(laPrntCost,1)
    ENDIF
    *-- Put the current mask in the array.
    laPrntCost[1] = &lcTmpBom..cItmMask
  ENDIF
ENDSCAN

IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
SET ORDER TO TAG CSTYLE IN STYLE

*-- If style cost sheet only...
IF lcStyleTyp <> "T"
  *-- If there is records their cost has been updated, loop to update their
  *-- Related parents records.
  IF !EMPTY(laPrntCost[1])
    FOR lnCnt1 = 1 TO ALEN(laPrntCost,1)
      *-- Call local function to calculate the current parent cost in its
      *-- parent records...
      =lfPrntCost(laPrntCost[lnCnt1])
    ENDFOR
  ENDIF
ENDIF

*-- Restore record no.
IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
  GOTO lnCurRec IN (lcTmpBom)
ENDIF
*!*************************************************************
*! Name      : lfPrntCost
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to know if the selected Component is 
*!           : already selected before as a component.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcPrnt -> The parent non-major color.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfGetClrCm(lcPrnt)
*!*************************************************************
*
FUNCTION lfPrntCost
PARAMETERS lcPrnt
PRIVATE lcPrnt , lcCurMajor , lcCurNonMj , lnCurCost , ;
        llFndComp , lnCurRec

llFndComp = .T.   && Flag to know if something found or not.
llCalCost = .F.   && Flag to know if calculated the parent cost.
lnCurCost = 0     && Variable hold the calculated cost value.
SELECT (lcTmpBom)
GO TOP

*-- Loop till there is no parent records found in the bom file.
DO WHILE llFndComp
  
  *-- Find if the sent parent was a component to any cost item in the
  *-- Bom file & get the its parents.
  LOCATE REST FOR (LIKE(STRTRAN(Item , "*" , "?") , lcPrnt) .OR. ;
                   LIKE(STRTRAN(lcPrnt , "*" , "?") , Item))
  
  IF FOUND()
    llFndComp = .T.   && Set flag that a parent was found for the parent sent.
    
    IF !llCalCost
      *-- Get the major part of the parent sent.
      *lcCurMajor = PADR(lcPrnt,lnMajorLen)
      *-- Get the non- major part of the parent sent.
      *lcCurNonMj = SUBSTR(lcPrnt,lnMajorLen + IIF(EMPTY(ALLTRIM(lcSeprator)) ,1 , 2), lnNonMjLen)

      *-- Get the major part of the parent sent.
      lcCurMajor = PADR(Item,lnMajorLen)
      *-- Get the non- major part of the parent sent.
      lcCurNonMj = SUBSTR(Item,lnMajorLen + IIF(EMPTY(ALLTRIM(lcSeprator)) ,1 , 2), lnNonMjLen)
      *-- Define the flag of parent style same as component style to void
      *-- the error if calling this function from main prog "MfCstSh"
      llSame     = IIF(TYPE("llSame") = "U" , .F. , llSame)
      *-- Call local function to calculate the parent for the parent sent.
      lnCurCost  = lfTempCost(lcCurMajor , lcCurNonMj)
      *-- Set the flag to true to know that the parent cost calculated.
      llCalCost  = .T.
    ENDIF
    
    *-- Update the cost of the parent found for the parent sent.
    REPLACE UNTCOST WITH lnCurCost ;
            TOTCOST WITH UNTCOST * nBomTotQty ;
            cStatus WITH IIF(cStatus = "S" , "M" , cStatus)
    
    *-- Save current record pointer.
    lnCurRec = RECNO()
    
    *-- Call the current function to know calculate the cost for the current 
    *-- parent sent if there is parent records for it.
    =lfPrntCost(cItmMask)
    
    *-- Restore the record pointer in the temp. Bom file.
    IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
      SKIP
    ENDIF
  ELSE
    llFndComp = .F.   && Set flag that no parent was found for the parent sent.
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Delete Cost Sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

lnAlias=SELECT()
SELECT ORDBOM
=gfObj_lock(.F.)
SELECT ORDHDR
=gfObj_lock(.F.)

SELECT (lcTmpBom)
LOCATE 
IF EOF()
  lcMsg2 = 'No record to delete'
  lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  SELECT(lnAlias)
  RETURN
ELSE
  SELECT (lcOrdSty)
  SCAN FOR CCOSTSHT = "YES"
    SELECT (lcTmpBom)
    SET DELE OFF
    SELECT (lcTmpBom)
    SET FILTER TO 
    SCAN FOR ALLTRIM(cItmMajor) = ALLTRIM(&lcOrdSty..Style)
      SCATTER MEMVAR MEMO
      m.cstatus = "D"
      INSERT INTO (lcAllBom) FROM MEMVAR
    ENDSCAN
    SELECT (lcAllBom)
    DELETE ALL FOR cstatus = "D"
    SET DELE ON
    =gfTmp2Mast('ORDBOM' , lcAllBom, 'Delete cost sheet items for : ' + &lcOrdLine..Order+ ' ...')
  ENDSCAN
  SELECT (lcOrdLine)
  DELE ALL 
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  
SELECT (lcTmpBom)
PACK
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwAmdPrc
*! Developer : Mohamed Shokry
*! Date      : 06/30/2004
*! Purpose   : When function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lfwAmdPrc

IF &lcTmpBom..cCatgTyp $ "FT"
  IF lcDutyable $ '13'
    SHOW GET m.DutyRate ENABLE
  ELSE
    SHOW GET m.DutyRate DISABLE
  ENDIF
  SHOW GET m.ExchRate ENABLE
  SHOW GET m.UsdPrice ENABLE
ELSE
  IF lcDutyable $ '13'
    SHOW GET m.DutyRate ENABLE
    SHOW GET pbSave ENABLE
  ELSE
    SHOW GET m.DutyRate DISABLE
    SHOW GET pbSave DISABLE
  ENDIF  
  IF &lcTmpBom..cCatgTyp $ "P"
    SHOW GET m.ExchRate ENABLE
    SHOW GET m.UsdPrice ENABLE
  ELSE
    SHOW GET m.ExchRate DISABLE
    SHOW GET m.UsdPrice DISABLE
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfUpdDCost
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to update the cost for duty 
*!           : cost item & has a percent if there is a new 
*!           : added price cost item or dutyable cost item.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnSign -> (-1 or +1)
*!             lnOld  -> old value to be subtracted.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdDCost()
*!*************************************************************
*
FUNCTION lfUpdDCost
PARAMETERS lnSign , lnOld
PRIVATE lnCurRec , lnStyRec , lnCurCst , lnTotSizes , laCurSize , ;
        lnShardSiz , lcCurScal , lcCurSize , lcLinFound , lnSizPos , ;
        lcScanExp , lcMSize , lcCSize , lcCScale , lcChldScal , ;
        lcCurType , lnSign , lnOld , laPrntCost , lnCnt1,lnDutyPer
lnStyRec   = RECNO("STYLE")         && Save the style record pointer.
SET ORDER TO TAG STYLE IN STYLE     && Change the style index.
lnCurRec   = RECNO(lcTmpBom)        && Save the BOM record pointer.
lcCurType  = &lcTmpBom..cCatGTyp    && Save the current cost type.
lcItmMask  = &lcTmpBom..cItmMask    && Save the current item mask.
lnCurCst   = &lcTmpBom..TotCost     && Default the total cost of the current MFG or current Duty.
lnTotSizes = 0                      && Var. hold the total sizes of the current MFG or current Duty.
DECLARE laCurSize[1]                && Array hold the scale sizes' of the current MFG or current Duty.

lnDutyPer   = &lcTmpBom..nDutyRate   && Default the total cost of the current MFG or current Duty.
laCurSize  = ""

*-- Define array to hold all the updated parent cost to update its parents cost.
DECLARE laPrntCost[1]
laPrntCost = ""

*-- If style cost sheet based on sizes, based on sizes.
IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
  *-- If there is selected sizes for the current cost item.
  FOR lnCount = 1 TO MEMLINES(ALLTRIM(&lcTmpBom..mSizes))
    *-- Fill the current cost item scale sizes array.
    IF !EMPTY(laCurSize[1])
      DIMENSION laCurSize[ALEN(laCurSize,1)+1]
    ENDIF
    laCurSize[ALEN(laCurSize,1)] = MLINE(ALLTRIM(&lcTmpBom..mSizes) , lnCount)
  ENDFOR
ELSE
  *-- If there is no selected sizes, fill the scale sizes array 
  *-- for the current cost item with the right scales.
  STORE "" TO lcMSize
  SELECT STYLE
  =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
            FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
    IF SEEK("S" + Style.Scale , "SCALE")
      STORE "" TO lcMSize
      FOR lnCnt  = 1 TO Scale.Cnt
        lcCnt    = STR(lnCnt,1)
        lcMSize  = lcMSize + IIF(EMPTY(lcMSize) , "" , ",") + lcCnt
      ENDFOR
      
      IF ASCAN(laCurSize, PADR(Scale.Scale,3)) = 0
        IF !EMPTY(laCurSize[1])
          DIMENSION laCurSize[ALEN(laCurSize,1)+1]
        ENDIF
        laCurSize[ALEN(laCurSize,1)] = PADR(Scale.Scale,3) + "~" + lcMSize
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcTmpBom)
ENDIF

*-- Change the cost of the current item to the equevelant currency.
IF lcStyleTyp = "I" .AND. llMulCurr .AND. ;
   !(&lcTmpBom..cCurrCode = gcBaseCurr)
  llFound    = SEEK(gcBaseCurr + &lcTmpBom..cCurrCode , "SYCEXCH")
  lnPExRate  = IIF(llFound , SYCEXCH.nExRate , 1)
  llFound    = SEEK(&lcTmpBom..cCurrCode , "SYCCURR")
  lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
  lcPRatSign = "*"
  lcPUntSign = "/"
  lcPRatSign = gfGetExSin(@lcPUntSign , &lcTmpBom..cCurrCode)
ENDIF

*-- Prepare the scan expression.
DO CASE
  CASE  &lcTmpBom..cCostStat = "1"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'D'"
  CASE  &lcTmpBom..cCostStat = "2"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'M'"
    
  CASE  &lcTmpBom..cCostStat = "3"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'M' OR &lcTmpBom..cCatGTyp = 'D'"

  OTHERWISE
    lcScanExp = "&lcTmpBom..cCatGTyp = 'D'"
ENDCASE

*-- Scan in the BOM for all the price cost items record to 
*-- calculate the current Mfg cost item or current Duty cost item.
SELECT (lcTmpBom)
SCAN FOR &lcScanExp .AND. &lcTmpBom..nPercent <> 0 .AND. ;
         (LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , lcItmMask) .OR. ;
          LIKE(STRTRAN(lcItmMask , "*" , "?") , &lcTmpBom..cItmMask))
   *-- Var. hold the no. of sizes shared between the current 
  *-- cost items (Mfg - Price) or (Duty & Dutyable records). 
  lnShardSiz = 0
  lnTotSizes = 0
  IF EMPTY(ALLTRIM(&lcTmpBom..mSizes))
    STORE "" TO lcCSize , lcCScale , lcChldScal
    SELECT STYLE
    =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
              FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
      IF SEEK("S" + Style.Scale , "SCALE")
        STORE "" TO lcCSize , lcCScale
        FOR lnCnt  = 1 TO Scale.Cnt
          lcCnt    = STR(lnCnt,1)
          lcCSize  = lcCSize + IIF(EMPTY(lcCSize) , "" , ",") + lcCnt
        ENDFOR
        lcCScale   = PADR(Scale.Scale,3) + "~" + lcCSize + CHR(13)
        IF !(lcCScale $ lcChldScal)
          lcChldScal = lcChldScal + lcCScale
        ENDIF
      ENDIF
    ENDSCAN
    SELECT (lcTmpBom)
  ELSE
    lcChldScal = ALLTRIM(&lcTmpBom..mSizes)
  ENDIF
  
  *-- Get the shared sizes between the current Mfg cost item &
  *-- the current price cost item <OR> current Duty cost item &
  *-- the current dutyable record.
  FOR lnCount = 1 TO ALEN(laCurSize,1)
    
    *-- Get the scale of the current Mfg or current Duty from the current 
    *-- MFG scale array row or current Duty scale array row.
    lcCurScal = SUBSTR(laCurSize[lnCount],1,3)
    lcCurSize = SUBSTR(laCurSize[lnCount],5)
    *-- Check if this scale exists in the current price cost item sizes.
    IF ATCLINE(lcCurScal , lcChldScal) > 0
      
      *-- If the scale was found, get the contents of this line.
      lcLinFound = MLINE(ALLTRIM(lcChldScal) , ATCLINE(lcCurScal , lcChldScal))
      
      *-- Check if there is any shared sizes.
      lnSizPos   = 1
      FOR lnCnt2 = 1 TO OCCURS("," , lcCurSize) + 1
        IF ("~"+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound) .OR. (","+SUBSTR(lcCurSize , lnSizPos , 1) $ lcLinFound)
          lnShardSiz = lnShardSiz + 1
        ENDIF
        *-- Increment the size position with 2.
        lnSizPos = lnSizPos + 2
      ENDFOR
    ENDIF
  ENDFOR
  
  FOR lnCount = 1 TO MEMLINES(lcChldScal)
    lnTotSizes = lnTotSizes + OCCURS("," , MLINE(lcChldScal,lnCount)) +1
  ENDFOR
  
  *-- Update the current MFG or current Duty with the right total cost.
  *-- Get the percentage of the current line cost multiple by shared sizes.
  *-- & divide the calculated total cost by the no. of all sizes in the
  *-- current MFG or current Duty.

  *--mhm the only change is the percentage to be variant
  lnUpdCost = (((((lnCurCst-lnOld) * lnDutyPer)/100) * lnShardSiz) / lnTotSizes)  
  
  *-- Change the cost of the current item to the equevelant currency.
  IF lcStyleTyp = "I" .AND. llMulCurr .AND. (lcCurType $ "FTS") ;
     .AND. !(&lcTmpBom..cCurrCode = gcBaseCurr)
    lnUpdCost = lnUpdCost &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
  ENDIF
  lnUpdCost = UntCost + (lnSign * lnUpdCost)
  
  *-- Save the updated cost.
  
  REPLACE UntCost WITH lnUpdCost ;
          TOTCOST WITH UNTCOST * nBomTotQty;
          CSTATUS WITH IIF(CSTATUS = "S" , "M" , CSTATUS)
  
  *-- Get all the parent records that updated their cost in one array.
  IF ASCAN(laPrntCost , &lcTmpBom..cItmMask) = 0
    IF !EMPTY(laPrntCost[1])
      DIMENSION laPrntCost[ALEN(laPrntCost,1)+1]
      =AINS(laPrntCost,1)
    ENDIF
    *-- Put the current mask in the array.
    laPrntCost[1] = &lcTmpBom..cItmMask
  ENDIF
ENDSCAN

IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
SET ORDER TO TAG CSTYLE IN STYLE

*-- If style cost sheet only...
IF lcStyleTyp <> "T"
  *-- If there is records their cost has been updated, loop to update their
  *-- Related parents records.
  IF !EMPTY(laPrntCost[1])
    FOR lnCnt1 = 1 TO ALEN(laPrntCost,1)
      *-- Call local function to calculate the current parent cost in its
      *-- parent records...
      =lfPrntCost(laPrntCost[lnCnt1])
    ENDFOR
  ENDIF
ENDIF

*-- Restore record no.
IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT(lcTmpBom)
  GOTO lnCurRec IN (lcTmpBom)
ENDIF


*!**************************************************************************
*! Name      : gfCPBrows
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/17/2004
*! Purpose   : Browse the Order Header file.
*!**************************************************************************
*! Reference : 
*!**************************************************************************
*! Calls     : lfvOrder()
*!**************************************************************************
*! Example   : =gfCPBrows()
*!**************************************************************************
*
FUNCTION gfCPBrows
PRIVATE lnAlias 
lnAlias = SELECT()
lcForExpr = "'O' FOR cOrdType ='O' AND !(STATUS$'XC')"

SELECT ORDHDR
lcOldOrd = ORDER()
SET ORDER TO TAG ORDHDR
LOCATE

lcTitle = "Sales Order"
DIME laTempData[1]
STORE '' TO laTempData
llGetOrd =ARIABROW(lcForExpr ,"Sales Order",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laTempData')

IF llGetOrd
  SELECT ORDHDR
  SET ORDER TO (lcOldOrd)
  laData[1]  = laTempData[1]
  =lfvOrder()
ELSE
  IF !EMPTY(laData[1])
    =SEEK('O'+laData[1],'ORDHDR')
  ENDIF
  SELECT ORDHDR
  SET ORDER TO (lcOldOrd)
ENDIF

SELECT (lnAlias)
*-- End of gfCPBrows.

*!*************************************************************
*! Name      : lfCostInfo
*! Developer : Mohamed Shokry
*! Date      : 06/10/2004
*! Purpose   : Function to update the item costing info
*!           : in the style or fabric files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCostInfo()
*!*************************************************************
*
FUNCTION lfCostInfo
PRIVATE lnItemRec , lnStyRec , lnFbrRec , lnTmpRec

*-- Save the record pointer in th style file.
SET ORDER TO TAG STYLE  IN STYLE
lnStyRec = RECNO("STYLE")

*-- Save the record pointer in the fabric file.
SET ORDER TO TAG FABRIC IN FABRIC
lnFbrRec = RECNO("FABRIC")

*-- Compute cost item elements.
*-- If copute for material cost sheet.

  *-- Define variables for the cost fields in the style file.
  FOR lnCntFld = 1 TO 5
    lcCntFld = STR(lnCntFld,1)
    lcRepFld&lcCntFld = ('n'+lcStyleTyp+'Cost'+lcCntFld)
  ENDFOR
  *-- Scan for the major style "&lcOrdSty..Style".
  SELECT STYLE
  =SEEK(PADR(&lcOrdSty..Style,lnMajorLen))
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcOrdSty..Style,lnMajorLen)
    *-- Save the record pointer in the style file.
    lnItemRec = RECNO()
    *-- Save the current style color.
    lcStyClr  = SUBSTR(Style.Style , lnColorStr , lnColorLen)
    *-- Blank the current cost variables.
    STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5

    lnAllScSiz = 0
    SELECT SCALE
    SUM CNT FOR Type + Scale = "S" + STYLE.Scale to lnAllScSiz
    
    *-- Scan in the temp. bom file to calculate the cost for each cost item.
    SELECT (lcTmpBom)
    SCAN FOR LIKE(STRTRAN(&lcTmpBom..cItmMask,'*','?') , STYLE.Style) .AND. ;
             (EMPTY(&lcTmpBom..mSizes) .OR. STYLE.Scale $ &lcTmpBom..mSizes)
      lnNoOfSize = 1   && Var. hold the no. of sizes selected.
      lnWholSclN = 1   && Var. hold no. of all sizes in current style scale.
      *-- If there is sizes selected for the current cost item.
      IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
        *-- Loop for the memo lines count.
        *-- Get the scale count from the scale file.
        lnWholSclN = IIF(SEEK("S"+STYLE.Scale , "SCALE") , SCALE.Cnt , 1)
        *-- Get the no. of lines in the current sizes memo field.
        lnLineNo   = ATCLINE(STYLE.Scale , ALLTRIM(&lcTmpBom..mSizes))
        *-- Get the string that hold the sizes.
        lcGetSize  = IIF(lnLineNo > 0 , SUBSTR(ALLTRIM(MLINE(&lcTmpBom..mSizes,lnLineNo)),5) , "")
        
        *-- Get the no. of sizes selected for the current scale.
        lnNoOfSize = OCCURS("," , lcGetSize) + 1
      ENDIF
      
      *-- Define the current cost field in the style file.
      lcCurVar = 'lnCost'+&lcTmpBom..Typ
      DO CASE
        *-- If the cost item is fabric or trim & trim inventory.
        CASE (cCatGTyp='F') .OR. (cCatGTyp='T' .AND. Trim_Invt)
          IF IClr = '******'
            lcFabClr = SUBSTR(STYLE.Style , lnColorStr , lnColorLen)
          ELSE
            lcFabClr = PADR(IClr,6)
          ENDIF
          *-- Save the current record pointer in the fabric file.
          lnTmpRec = RECNO("Fabric")
          IF SEEK(SUBSTR(Item,1,7)+lcFabClr , 'Fabric')
            *-- Calculate the cost for the current cost item.
            
            IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0 AND lcStyleTyp = 'I'
              &lcCurVar = &lcCurVar + ROUND((((&lcTmpBom..nBomTotQty*&lcTmpBom..UntCost)*lnNoOfSize)/lnWholSclN),2)
            ELSE
              &lcCurVar = &lcCurVar + ROUND((((&lcTmpBom..nBomTotQty*Fabric.CostBuy/Fabric.Conv)*lnNoOfSize)/lnWholSclN),2)
            ENDIF  
            
          ENDIF
          IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("FABRIC")
            GOTO lnTmpRec IN FABRIC
          ENDIF
        *-- If the cost item is trim & not trim inventory or 
        *-- Price, Duty or Mfg operation.
        CASE (cCatGTyp = 'T' .AND. !Trim_Invt) .OR. cCatGTyp $ 'MDP'
          *-- Calculate the cost for the current cost item.
          
          &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN),2)
          
        *-- If the cost item is style component.
        CASE cCatGTyp = 'S'
          lcStySeek = &lcTmpBom..Item
          *-- Save the record pointer in the style file.
          lnTmpRec  = RECNO("STYLE")
          *-- If the style component is same as style parent.
          IF &lcOrdSty..Style = SUBSTR(lcStySeek , 1 , lnMajorLen)
            *-- Get the cost from the temp. Bom file.
            
            &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN),2)
            
          ELSE
            IF '*' $ lcStySeek
              *-- If there is no extended size scale, concatenate a style 
              *-- mask to search with it in the style file.
              IF !llExtSizSc .OR. EMPTY(&lcTmpBom..mSzCrosRef)
                FOR lnCnt = 1 TO ALEN(laStyCseg,1)
                  lnSegPos = laStyCSeg[lnCnt,4]
                  lnSegLen = LEN(laStyCSeg[lnCnt,3])
                  IF '*' $ SUBSTR(lcStySeek , lnSegPos , lnSegLen)
                    lcStySeek = STRTRAN(lcStySeek , ;
                                 SUBSTR(lcStySeek   , lnSegPos , lnSegLen) , ;
                                 SUBSTR(STYLE.Style , lnSegPos , lnSegLen) , 1 , 1)
                  ENDIF
                ENDFOR
                *-- Save the record pointer in the style file.
                lnTmpRec = RECNO("STYLE")
                IF SEEK(lcStySeek , 'Style')
                  *-- Calculate the cost for the current cost item.
                  
                  &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN),2)
                 
                ENDIF
              ELSE
                *-- Get the cross reference memo field lines no.
                lnLineCnt = MEMLINES(&lcTmpBom..mSzCrosRef)
                *-- Define an array to hold the style component scales & its sizes no.
                DECLARE laTmpChldS[1,2]
                laTmpChldS[1,1] = ""
                laTmpChldS[1,2] = 0
                *-- Variable hold all sizes count.
                lnTotSize = 0
                *-- Loop in all the lines to get the style component scales.
                FOR lnCnt = 1 TO lnLineCnt
                  *-- Get the current line in the memo field.
                  lcCurCrRef = ALLTRIM(MLINE(&lcTmpBom..mSzCrosRef,lnCnt))
                  *-- If the parent style scale included in the cross ref. field.
                  IF STYLE.Scale = SUBSTR(lcCurCrRef,1,3)
                    lnWherScal = ASCAN(laTmpChldS , SUBSTR(lcCurCrRef,7,3))
                    *-- Add the current child scale if it was not added in the temp. array.
                    IF lnWherScal = 0
                      IF !EMPTY(laTmpChldS[1,1])
                        DIMENSION laTmpChldS[ALEN(laTmpChldS,1)+1 , 2]
                      ENDIF
                      laTmpChldS[ALEN(laTmpChldS,1) , 1] = SUBSTR(lcCurCrRef,7,3)
                      laTmpChldS[ALEN(laTmpChldS,1) , 2] = 1
                    ELSE
                      *-- Add 1 to the sizes no. for the current child scale.
                      laTmpChldS[ASUBSCRIPT(laTmpChldS , lnWherScal , 1) , 2] = laTmpChldS[ASUBSCRIPT(laTmpChldS , lnWherScal , 1) , 2] + 1
                    ENDIF
                    *-- Add 1 to the whole sizes count.
                    lnTotSize = lnTotSize + 1
                  ENDIF
                ENDFOR
                *-- concatenate a style mask to search with it in the style file.
                FOR lnCnt = 1 TO ALEN(laStyCseg,1)
                  lnSegPos = laStyCSeg[lnCnt,4]
                  lnSegLen = LEN(laStyCSeg[lnCnt,3])
                  IF laStyCSeg[lnCnt,2] <> "S" .AND. ;
                     '*' $ SUBSTR(lcStySeek , lnSegPos , lnSegLen)
                    lcStySeek = STRTRAN(lcStySeek , ;
                                SUBSTR(lcStySeek   , lnSegPos , lnSegLen) , ;
                                SUBSTR(STYLE.Style , lnSegPos , lnSegLen) , 1 , 1)
                  ENDIF
                ENDFOR
                *-- Loop in all the temp. array elements that hold child scales.
                FOR lnCnt = 1 To ALEN(laTmpChldS,1)
                  *-- If there is "*" in the position of scale segment.
                  IF '*' $ SUBSTR(lcStySeek , IIF(!EMPTY(lcSizeSep) , lnSizePos+1 , lnSizePos) , IIF(!EMPTY(lcSizeSep) , lnSizeLen-1 , lnSizeLen))
                    *-- Put the child scale in the current array element
                    *-- instead of the stars in the position of the scale
                    *-- segment in the style component mask.
                    lcStySkExp = STRTRAN(lcStySeek , ;
                                 SUBSTR(lcStySeek , IIF(!EMPTY(lcSizeSep) , lnSizePos+1 , lnSizePos) , IIF(!EMPTY(lcSizeSep) , lnSizeLen-1 , lnSizeLen)) , ;
                                 laTmpChldS[lnCnt,1] , 1 , 1)
                  ENDIF
                  *-- Seek with the style component mask in the style file.
                  IF SEEK(lcStySkExp , 'Style')
                    *B602134,1 Reham On 11/22/98   *** Begin ***
                    *B602134,1 Divide the current style component cost by all the parent 
                    *B602134,1 style sizes before adding to the style current cost.
                    *-- Get the total cost of the style component.
                    *&lcCurVar = &lcCurVar + (&lcTmpBom..nBomTotQty * Style.TotCost * laTmpChldS[lnCnt,2])
                    
                    *B604813,1 AMH Round before add [Start]
                    *&lcCurVar = &lcCurVar + ((&lcTmpBom..nBomTotQty * Style.TotCost * laTmpChldS[lnCnt,2])/ lnAllScSiz)
                    &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..nBomTotQty * Style.TotCost * laTmpChldS[lnCnt,2])/ lnAllScSiz),2)
                    *B604813,1 AMH [End]
                    
                    *B602134,1 Reham On 11/22/98   *** End   ***
                  ENDIF
                ENDFOR
                *B602134,1 Reham On 11/22/98   *** Begin ***
                *B602134,1 Remark dividing the total cost by sizes in this step.
                *-- Divide the cost by the whole total size.
                *&lcCurVar = &lcCurVar / lnTotSize
                *B602134,1 Reham On 11/22/98   *** End   ***
              ENDIF
            ELSE
              IF SEEK(lcStySeek , 'Style')
                
                *B604813,1 AMH Round before add [Start]
                *&lcCurVar = &lcCurVar + ((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN)
                &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN),2)
                *B604813,1 AMH [End]
                
              ENDIF
            ENDIF
          ENDIF
          *-- Restore the record pointer in the style file.
          IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("STYLE")
            GOTO lnTmpRec IN STYLE
          ENDIF
      ENDCASE
    ENDSCAN
    *-- Restore the record pointer in the style file.
    SELECT STYLE
    IF lnItemRec > 0 .AND. lnItemRec <= RECCOUNT("STYLE")
      GOTO lnItemRec
    ENDIF
    
    *-- Round The cost updated in the style file.
    *-- Update Style costing elements.
    *REPLACE &lcRepFld1 WITH lnCost1 ,;
            &lcRepFld2 WITH lnCost2 ,;
            &lcRepFld3 WITH lnCost3 ,;
            &lcRepFld4 WITH lnCost4 ,;
            &lcRepFld5 WITH lnCost5
    
    *-- Update Style costing elements.
    *B603589,1 AMH 09/12/2000 Updating Gros_Price in Style File [Start]
    *REPLACE &lcRepFld1 WITH ROUND(lnCost1 , 2) ;
            &lcRepFld2 WITH ROUND(lnCost2 , 2) ;
            &lcRepFld3 WITH ROUND(lnCost3 , 2) ;
            &lcRepFld4 WITH ROUND(lnCost4 , 2) ;
            &lcRepFld5 WITH ROUND(lnCost5 , 2)
    REPLACE &lcRepFld1 WITH ROUND(lnCost1 , 2) ;
            &lcRepFld2 WITH ROUND(lnCost2 , 2) ;
            &lcRepFld3 WITH ROUND(lnCost3 , 2) ;
            &lcRepFld4 WITH ROUND(lnCost4 , 2) ;
            &lcRepFld5 WITH ROUND(lnCost5 , 2) ;
            Gros_Price WITH IIF(lcStyleTyp='I',ROUND(lnCost1 , 2),0)
    *B603589,1 AMH 09/12/2000 Updating Gros_Price in Style File [End  ]
    
    *-- If there was a fabric set as a primary fabric.
    IF !EMPTY(lcPrimFbr)
      REPLACE STYLE.Fabric WITH lcPrimFbr
    ENDIF
    
    *-- Added to check the currency of the PPrice and duty components 
    *-- and take it in consideration when calculate the total cost to 
    *-- avoid the sum of different currencys.
    lnTCost1 = lnCost1
    lnTCost2 = lnCost2
    lnTCost3 = lnCost3
    lnTCost4 = lnCost4
    lnTCost5 = lnCost5
    IF llMulCurr AND lcStyleTyp = 'I'
      *-- Save the record pointer in the style file.
      lnStRecd = RECNO("STYLE")
      *-- Get purchase price currency, duty currency or base currency.
      lcPPrCurr = IIF(SEEK(PADR(&lcOrdSty..Style,lnItemLen),"STYLE") , ;
                      IIF(!EMPTY(Style.cPriceCur) , Style.cPriceCur , ;
                          gcBaseCurr) , gcBaseCurr)
      lcDutyCur = IIF(SEEK(PADR(&lcOrdSty..Style,lnItemLen),"STYLE") , ;
                      IIF(!EMPTY(Style.cDutyCur) , Style.cDutyCur , ;
                          gcBaseCurr) , gcBaseCurr)
      
      *-- Restore the record pointer in the style file.
      IF lnStRecd > 0 .AND. lnStRecd <= RECCOUNT("STYLE")
        GOTO lnStRecd
      ENDIF
      
      *-- If the price currency is different from the base currency, get 
      *-- the exchange rate , current unit & the rate sign.
      IF lcPPrCurr <> gcBaseCurr
        llFound    = SEEK(gcBaseCurr + lcPPrCurr , 'SycExch')
        lnExRate   = IIF(llFound , sycexch.nExRate , 1)
        llFound    = SEEK(lcPPrCurr  , 'SycCurr')
        lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
        lcRateSign = gfGetExSin(@lcUnitSign , lcPPrCurr)
        
        *-- Calculate the price cost according to the price currency.
        lnTCost1   = lnCost1 &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      ENDIF
      *-- If the duty currency is different from the base currency, get 
      *-- the exchange rate , current unit & the rate sign.
      IF lcDutyCur <> gcBaseCurr
        *-- Get the duty & Mfg operation no. 
        *B603765,1 ABD Remark the next variable that we don't need them now.[Begin]
        *STORE "" To lcDuty , lcMfg
        *- B603765,1 ABD define new array that will hold the types.
        DIMENSION laTemptype [5,2]
        laTemptype = ''
        *B603765,1 ABD [End]
        FOR lnCnt1 = 1 TO 5
          lcCnt1 = STR(lnCnt1,1)
          *B603765,1 ABD Remark the next two lines that we don't need them. [Begin]
          *lcDuty = IIF(lc&lcStyleTyp.TYPE&lcCnt1 = "D" , lcCnt1 , lcDuty)
          *lcMfg  = IIF(lc&lcStyleTyp.TYPE&lcCnt1 = "M" , lcCnt1 , lcMfg)
          *-- B603765,1 ABD Do case to fill the array with type of opration.
          DO CASE
            *-- Case type of opration is duty
            CASE lc&lcStyleTyp.TYPE&lcCnt1 = "D"
              laTemptype [lnCnt1,1] = lcCnt1
              laTemptype [lnCnt1,2] = "D"
              
            *-- Case type of opration is Mfg.
            CASE lc&lcStyleTyp.TYPE&lcCnt1 = "M" 
              laTemptype [lnCnt1,1] = lcCnt1
              laTemptype [lnCnt1,2] = "M"
             
          ENDCASE
          *B603765,1 ABD End case [end]
        ENDFOR
      
        llFound    = SEEK(gcBaseCurr + lcDutyCur ,'SycExch')
        lnExRate   = IIF(llFound , sycexch.nExRate , 1)
        llFound    = SEEK(lcDutyCur , 'SycCurr')
        lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
        lcRateSign = gfGetExSin(@lcUnitSign , lcDutyCur)
        
        *B603016,1 Reham On 06/27/1999   *** Begin ***
        *B603016,1 Check if there is duty cost item before updating.
        *B603765,1 ABD Remark the Following lines. [Begin]
        *IF BETWEEN(VAL(lcDuty) , 1 , 5)
        *B603765,1 ABD [End]
        *B603016,1 Reham On 06/27/1999   *** End   ***
          *-- Calculate the duty cost & mfg operation cost according to 
          *-- the duty currency.
          *B603765,1 ABD Remark the Following lines. [Begin]
          *lnTCost&lcDuty = lnCost&lcDuty &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          *B603765,1 ABD [End]
        *B603016,1 Reham On 06/27/1999   *** Begin ***
        *B603765,1 ABD Remark the Following line. [Begin]
        *ENDIF
        *B603765,1 ABD [End]
        *B603016,1 Check if there is Mfg. cost item before updating.
        *B603765,1 ABD Remark the Following lines. [Begin]
        *IF BETWEEN(VAL(lcMfg) , 1 , 5)
        *B603765,1 ABD [End]
        *B603016,1 Reham On 06/27/1999   *** End   ***
          *-- Calculate the duty cost & mfg operation cost according to 
          *-- the duty currency.
          *B603765,1 ABD Remark the Following lines. [Begin]
          *lnTCost&lcMfg  = lnCost&lcMfg  &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          *B603765,1 ABD [End]
        *B603016,1 Reham On 06/27/1999   *** Begin ***
        *B603765,1 ABD Remark the Following line. [Begin]
        *ENDIF
        *B603765,1 ABD [End]
        *B603016,1 Reham On 06/27/1999   *** End   ***

        *B603765,1 ABD For statement to calculate the duty cost & mfg operation cost 
        *B603765,1 ABD According to the duty currency [Begin]
        FOR lnCnt1 = 1 TO 5
          lcCnt1 = STR(lnCnt1,1)
          *-- Check if there is duty cost item before updating.
          IF laTempType [lnCnt1,2] = "D" OR laTempType [lnCnt1,2] = "M"
            *-- Calculate the duty cost & according to the duty currency.
            *-- Calculate the mfg operation cost according to the duty currency.
            lnTCost&lcCnt1 = lnCost&lcCnt1 &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          ENDIF
        ENDFOR   
        *B603765,1 ABD [End]
      ENDIF
    ENDIF
    *-- Update the total cost in the style file.
    SELECT STYLE

    *B602895,1 Reham On 05/19/1999   *** Begin ***
    *B602895,1 Update the total cost with rounded cost items instead of rounding their totals.
    *-- Round The cost updated in the style file.
    *REPLACE TotCost WITH lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5
    *REPLACE TotCost WITH ROUND((lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5) , 2)
    REPLACE TotCost WITH ROUND(lnTCost1,2)+ROUND(lnTCost2,2)+ROUND(lnTCost3,2)+ROUND(lnTCost4,2)+ROUND(lnTCost5,2)
    *B602895,1 Reham On 05/19/1999   *** End   ***
    
    *E301077,16 Reham On 12/30/98   *** Begin ***
    *E301077,16 Open the company file.
    =gfOpenFile(gcDataDir+'StyInvJl',gcDataDir+'StyInvJl','SH')
    SELECT STYLE
    *E301077,16 Reham On 12/30/98   *** End   ***
    
    *-- Update style average cost if no stock or returns.
    IF !SEEK(STYLE.Style , 'StyInvJl')
      REPLACE Style.Ave_Cost WITH Style.TotCost
      
      *E301077,16 Reham On 12/30/98   *** Begin ***
      *E301077,16 Open the company file.
      =gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
      *E301077,16 Reham On 12/30/98   *** End   ***
      
      *-- Update average cost in the style dyelot file.
      IF SEEK(STYLE.Style , 'StyDye')
        SELECT StyDye
        REPLACE REST Ave_Cost WITH Style.Ave_Cost ;
               WHILE Style = STYLE.Style ;
                 FOR EMPTY(Dyelot)
      ENDIF
      
      *E301077,16 Reham On 12/30/98   *** Begin ***
      =gfCloseFile('StyDye')
      *E301077,16 Reham On 12/30/98   *** End   ***
    ENDIF
    *E301077,16 Reham On 12/30/98   *** Begin ***
    =gfCloseFile('StyInvJl')
    *E301077,16 Reham On 12/30/98   *** End   ***
  ENDSCAN

*-- Restore the record pointer in the style file.
SELECT STYLE
SET ORDER TO TAG CSTYLE IN STYLE
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT()
  GOTO lnStyRec
ENDIF

*-- Restore the record pointer in the fabric file.
SELECT FABRIC
SET ORDER TO TAG CFABRIC IN FABRIC
IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT()
  GOTO lnFbrRec
ENDIF


*!**************************************************************************
*! Name      : lfFrnFab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/17/2004
*! Purpose   : To Control showing Fabric in case of forign Curr.
*!**************************************************************************
*! Reference : 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Example   : =lfFrnFab()
*!**************************************************************************
*
FUNCTION lfFrnFab

*lnUnitCst  				&& Unit Cost  (Forign )
*lcCurrSmbl                 && Curr For unit cost
*lnTotFCst                  && TOtal forign Cost
*lcCurSmbl1                 && Simbol Curr forign
*lnExRate                   && Exchange Rate  

lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , &lcTmpBom..cFabCurr)
lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  *-- Get currency symbol.
llFound    = SEEK(lcCurrency , 'SycCurr')
lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
lcRateSg   = gfGetExSin(@lcUnitSign , lcCurrency)
lcRateSg   = IIF(lcRateSg   = "/" , "*" , "/")
lcUnitSign = IIF(lcUnitSign = "/" , "*" , "/")

lnUnitCst    = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..UntCost &lcRateSg &lcTmpBom..nFabExRate &lcUnitSign lnCurrUnit,2))
lnTotFCst    = lnUnitCst * &lcTmpBom..nBomTotQty  
lnExRate     = &lcTmpBom..nFabExRate


*!**************************************************************************
*! Name      : lfFrnFab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/17/2004
*! Purpose   : To Control showing Fabric in case of forign Curr.
*!**************************************************************************
*! Reference : 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Example   : =lfFrnFab()
*!**************************************************************************
*
FUNCTION lfChkColor
PARA lcCurColor

SELECT (lcOrdLine)
lnCount = 1
SCAN FOR ALLTRIM(OrdBom.cItmMajor) = ALLTRIM(PADR(&lcOrdLine..Style,lnMajorLen)) 
  DIMENSION laColor[lnCount,1]
  laColor[lnCount,1]   = SUBSTR(&lcOrdLine..Style , lnColorStr , lnColorLen)
  lnCount = 1 +lnCount
ENDSCAN
SELECT ORDBOM

IF ASCAN(laColor,lcCurColor)=0
  RETURN .F.
ELSE
  RETURN .T.
ENDIF  