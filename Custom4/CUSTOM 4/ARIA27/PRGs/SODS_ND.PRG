*:************************************************************************
*: Program File        : SoDs_ND.Prg
*: Program Description : Design & Name Drop Screen
*: For Screen          : SoDs_ND.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Shared (SO, PO)
*: Developer Name      : Reham Al-Allamy - (RAM)
*: Tracking Job Number : E301288,1
*: Date                : July/1999
*:************************************************************************
*: Called From         : 1. Sales Orders Screen (Detail folder)
*:************************************************************************
*: Calls               : SoDs_ND.Spr
*:************************************************************************
*: Passed Parameters   : lcType     -> "SO" if from order screen.
*:                                  -> "AD" if from PO screen.
*:                     : lcOrdNo    -> Current sales order #.
*:                     : lcT_BomVar -> Temp file copied from BOMVAR.
*:                     : lcStyle    -> Current style in the line.
*:                     : lcOrdLinNo -> Current line no.
*:                     : lnCurPrice -> Current price of the current style.
*:                     : lcCust     -> Current customer.
*:                     : lcStore    -> Current store for this customer.
*:                     : lnRetPrice -> The price that will return to the order program.
*:************************************************************************
*: Example             : 
*: DO (gcAppHome+'SODS_ND.FXP') WITH "SO" , laData[1] , lcT_BomVar , m.Style , m.LineNo , 
*:                                   m.Gros_Price , laData[2] , m.Store
*:************************************************************************
*: Modifications       : 
*:B802594,1 WAB 09/15/1999 The Popup design get the elemnt depends on Desgin Sizes.Also,
*:B802594,1                in case of No Name Drop.
*:B603211,1 WAB 10/14/1999 Fix The Bug of adding record in BomVar file with empty
*:B603211,1 			   No Name Drop ID. and empty Design Code.
*:B603255,1 WAB 10/31/1999 Fix the bug of when enters multiple adorned line , the adornment cost
*:B603255,1 WAB   		   adder is not being brought forward & the price adder coming from
*:B603255,1 WAB 		   The design file is not being converted in case multi Currency 
*:B603391,1 TAK 01/23/2000 Display designs ordered in configure screen popup.
*:B603458,1 KHM 02/16/2000 Fix the bug of not saving a record in the 
*:B603458,1                audiTrail when changing in the configure screen 
*:B603458,1                and pressing ESC.
*:B803099,1 ABD 06/18/2000 Change the designs popup to be a get field same as name drop.
*:B803099,1 ABD            Fix the following bug on this bug :-
*:B803099,1 ABD            1 - Fix Bug that you don't made any change and press ESC
*:B803099,1 ABD                You must change the gross price.
*:B803099,1 ABD            2 - Fixed Bug that after you call Program icnmdrp that add new name 
*:B803099,1 ABD                Drop id the browse didn't get any thing in the browse.
*:B605082,1 AMH 11/12/2001 Fix the bug cost of name drop does not display in case of Adornment order.
*:B605052,1 TMI 07/25/2002 Fix the bug that when open then closing the screen SoDs_ND the Gross 
*:B605052,1                Price is increased by Price Adder
*:C102797,1 AMH 02/05/2003 Change the cDsgnCode field to be 8 Chr.
*:************************************************************************
*
PARAMETERS lcType , lcOrdNo , lcT_BomVar , lcStyle , lcOrdLinNo , lnCurPrice , lcCust , lcStore , lnRetPrice
*-- Array hold the folders info.

PRIVATE laFoldWinds , lcfoldprnt , lnActfolder , lcfolder , lcWFoldChng ,;
        lcfoldpush , lnFolderCen , lnFolderRen , lnFoldSize , lnLastFold ,;
        ibFolder , lcType , lcOrdNo , lcT_BomVar , lcStyle , lcOrdLinNo ,;
        lcStylePos , lcNamDrpId , lcDsgnCode , lnPriceAdd , lcNewPos ,;
        lcRemPos , lcClsPos , lcBrwVarT ,lcCurStat , lcOldValue , lcCust ,;
        lcStore , lnDsgnPop , laDsgnCod ,lnNamPrice , lnDsnPrice , lcNDrpTT ,;
        lcNDrpTB , lnOldValue , llOpnSPos , llOpnBHdr , llOpnCtPik
*-- Variable hold the name drop.
lcNDrpId = ""

*-- Save current alias.
lcCurAlias = ALIAS()

llOpnSPos = .F.
IF !USED("IcStyPos")
  *-- Check if there is positions for the selected style.
  =gfOpenFile(gcDataDir+'IcStyPos',gcDataDir+'IcStyPos','SH')
  llOpnSPos = .T.
ELSE
  SELECT IcStyPos
  SET ORDER TO TAG IcStyPos IN IcStyPos
ENDIF

*-- Flag to know if there is positions for the currently selected style or not.
llPosition = IIF(SEEK(lcStyle) , .T. , .F.)

llOpnBHdr = .F.
IF !USED("BomHdr")
  *-- Check if there is variant cost sheet for the selected style major.
  =gfOpenFile(gcDataDir+'BomHdr',gcDataDir+'BomHdr','SH')
  llOpnBHdr = .T.
ELSE
  SET ORDER TO TAG BomHdr IN BomHdr
ENDIF

*-- Get the lenght of the the style major.
lnMajorLen = LEN(gfItemMask("PM"))

*-- Seek for the current style major.
SELECT BomHdr
=SEEK(PADR(lcStyle , lnMajorLen))
*-- Search for a variant cost sheet for the selected style.
LOCATE REST FOR BomHdr.cItmMajor = lcStyle .AND. BomHdr.lVariant
*-- Flag to know if there is variant cost sheet or not.
llVariant  = IIF(FOUND() , .T. , .F.)

*-- If there is no variant cost sheet or there is no styles' positions 
*-- for the currently selected style, do not display the dialog screen.
IF !llPosition .AND. !llVariant
  *** There are neither variant cost sheet nor style positions for style: {lcStyle}. ***
  *** <  OK  > ***
  =gfModalGen("TRM32069B00000" , "DIALOG" , lcStyle)
  *-- Restore the previous alias.
  SELECT (lcCurAlias)
  RETURN
ENDIF

*-- Variable to determine if validate cost or price.
lcCurType = IIF(lcType = "SO" , "Price" , "Cost")

*-- Define array to hold the design codes for the selected style position.
DECLARE laDsgnCod[1]
laDsgnCod[1] = "Select Design Code"

*-- Define flag to know if define name drops by account+store or all available name-drops.
llByCstStr  = .T.

*-- Define flag to know if open name drop file or not.
llOpnCtPik  = .F.
IF lcType = "AD"
  *-- Open the order line file.
  llOpnOrLin = .F.
  IF !USED("OrdLine")
    *-- Open the name drop file.
    =gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLine','SH')
    *-- Set flag to true to know that name drop file has opened.
    llOpnOrLin = .T.
  ELSE
    SET ORDER TO TAG OrdLine IN OrdLine
  ENDIF
  
  *-- Open the cutpick file.
  IF !USED("CutPick")
    *-- Open the name drop file.
    =gfOpenFile(gcDataDir+'CutPick',gcDataDir+'CutPkOrd','SH')
    *-- Set flag to true to know that name drop file has opened.
    llOpnCtPik = .T.
  ELSE
    SELECT CutPick
    SET ORDER TO TAG CutPkOrd
  ENDIF
  
  *-- Check if there is record for the current Adorned order line in the cutpick file
  *-- related to an order line. this is in order to get the account + store for this 
  *-- order line.
  IF !laScrMode[4] .AND. SEEK("3" + lcOrdNo + STR(lcOrdLinNo,6) , "CutPick")
    SELECT OrdLine
    IF SEEK("O" + Cutpick.Order + Cutpick.cOrdLine , "OrdLine")
      llByCstStr = .T.
      lcCust     = Ordline.Account
      lcStore    = Ordline.Store
    ELSE
      llByCstStr = .F.
    ENDIF
  ELSE
    llByCstStr = .F.
  ENDIF
  
  *-- If open the order line file opened in this program, close it.
  IF llOpnOrLin
    =gfCloseFile("OrdLine")
  ENDIF
ENDIF

*-- Define flag to know if open name drop file or not.
llOpnNmDrp  = .F.
IF !USED("IcNamDrp")
  *-- Open the name drop file.
  =gfOpenFile(gcDataDir+'IcNamDrp',gcDataDir+'IcNamDrp','SH')
  *-- Set flag to true to know that name drop file has opened.
  llOpnNmDrp = .T.
*RAM
*ELSE
*  SET ORDER TO TAG IcNamDrp IN IcNamDrp
ENDIF
IF llByCstStr
  SET ORDER TO TAG IcNamDrp IN IcNamDrp
ELSE
  SET ORDER TO TAG IcNamDrpID IN IcNamDrp
ENDIF
lcNamSeek = ""
*RAM

*-- Define flag to know if open design code file or not.
llOpnDsCd = .F.
IF !USED("IcDesign")
  *-- Open the name drop file.
  =gfOpenFile(gcDataDir+'IcDesign',gcDataDir+'IcDesign','SH')
  *-- Set flag to true to know that name drop file has opened.
  llOpnDsCd  = .T.
ELSE
  SET ORDER TO TAG IcDesign IN IcDesign
ENDIF

*-- Array hold the folders info.
DECLARE laFoldWinds[2,2]
*-- Adjust & fill the folders array.
laFoldWinds[1,1] = "Positions"
laFoldWinds[1,2] = "SoDs_Nd1"
laFoldWinds[2,1] = "BOM Configure"
laFoldWinds[2,2] = "SoDs_Nd2"
  
*-- Window parent name for the folder.
lcfoldprnt  = "SoDs_ND"
*-- Current active folder.
lnActfolder = 1
lcfolder    = gfTempName()
*-- Function to control shows after change the folder.
lcWFoldChng = '=lfActFolder()'
*-- Push button name for the next folder.
lcfoldpush  = 'pbFolders'

*-- Position of the folders on the base window.
lnFolderCen = 82.00
lnFolderRen = 2

*-- Variable hold the browse record pointer.
lnMark    = 0

*-- Flag hold the browse flag.
llBrowse  = .F.

*-- Variables hold the prices objects.
STORE 0 TO lnAddPrice , lnNewPrice

*-- Variable hold old value in all string objects.
lcOldValue = ""
*-- Variable hold the style position code.
lcStylePos = ""
*-- Variable hold the name drop ID.
lcNamDrpId   = ""
*-- Variable hold the name drop price.
lnNamPrice = 0
*-- Variable hold the name drop text top.
lcNDrpTT   = ""
*-- Variable hold the name drop text bottom.
lcNDrpTB   = ""
*-- Variable hold the Design code.
lcDsgnCode = ""
*-- Variable hold the design code price.
lnDsnPrice = 0
*-- Variable hold the Price Adder.
lnPriceAdd = 0

*-- Variable hold the poup name.
lnDsgnPop  = 1

*-- Variable hold the old added price.
lnOldValue = 0

*-- Define variable to hold the new button bitmap.
lcNewPos = gcBmpHome + "new1.bmp"
*-- Define variable to hold the remove button bitmap.
lcRemPos = gcBmpHome + "rem1.bmp"
*-- Define variable to hold the close button bitmap.
lcClsPos = gcBmpHome + "Cls.bmp"
*-- Define variable to hold the external key bitmap.
lcKeyBmp = gcBmpHome + "ExtKey.BMP"

*-- Variable hold all the objects status.
lcCurStat = "DISABLE"

*-- Variable hold the browse title.
lcBrwVarT = "Variant Options & Positions"

*-- Select the variant cost sheet temp. file.
SELECT (lcT_BomVar)
IF lcType = "SO"
  SUM npriceadd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + lcOrdNo + STR(lcOrdLinNo,6) TO lnAddPrice
ELSE
  SUM TotCost   FOR cIdType+cCost_Id+STR(LineNo,6) = "AD" + lcOrdNo + STR(lcOrdLinNo,6) TO lnAddPrice
ENDIF

*B803099,1 ABD Comment the next line that you dont made any change to the gross price.[Begin]
*lnCurPrice = MAX((lnCurPrice - lnAddPrice) , 0)
*B803099,1 ABD [End]

*B605052,1 TMI [Start] Increase the new price only by the diffrence
*lnNewPrice = lnCurPrice + lnAddPrice
lnOldAdPr = lnAddPrice
lnNewPrice = lnCurPrice
*B605052,1 TMI [End  ]
SET FILTER TO cIdType+cCost_Id+STR(LineNo,6) = lcType + lcOrdNo + STR(lcOrdLinNo,6)
GO TOP

*B603458,1 KHM 02/16/2000 (Begin) Saving the ESC Trap key and assigning
*B603458,1                the ESC key to do lfvConCls when press it.
lcEscTrap  = ON("KEY","ESC")
ON KEY LABEL ESC DO lfvConCls
*B603458,1 KHM 02/16/2000 (End)


*B803099,1 ABD Fix Bug that you don't made any change and press ESC
*B803099,1 ABD you must change the gross price. [Begin]
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
DO (gcScrDir+"SoDs_ND.SPX")
ON KEY LABEL ESC &lcHdEsc1
*B803099,1 ABD [END]


*B603458,1 KHM 02/16/2000 (Begin) Restoring the assignment of ESC key
ON KEY LABEL ESC &lcEscTrap
*B603458,1 KHM 02/16/2000 (End)

*-- Select the variant cost sheet temp. file.
SELECT (lcT_BomVar)
SET FILTER TO

*-- If open name drop file in this program, close it.
IF llOpnNmDrp
  =gfCloseFile("IcNamDrp")
ENDIF

*-- If Design code file in this program, close it.
IF llOpnDsCd
  =gfCloseFile("IcDesign")
ENDIF

*--If open the style position file in this program, close it.
IF llOpnSPos
  =gfCloseFile("IcStyPos")
ENDIF

*-- If open the Bom Header in this program, close it.
IF llOpnBHdr
  =gfCloseFile("BomHdr")
ENDIF

*-- If open the cutpick file in theis program, close it.
IF llOpnCtPik
  =gfCloseFile("CutPick")
ENDIF

*-- Restore the current alias.
SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfScrShw
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/04/1999
*! Purpose   : Function to execute the show of the current screen.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfScrShw()
*!*************************************************************
*! Modifications     :
*!*************************************************************
*
FUNCTION lfScrShw

DO CASE
  *-- If there is no style positions for the selected style, disable 
  *-- the position folder
  CASE !llPosition
    SHOW GET ibFolder(1) DISABLE
  *-- If there is no variant cost sheet for the selected style, disable 
  *-- the configure BOM folder.
  CASE !llVariant
    SHOW GET ibFolder(2) DISABLE
ENDCASE

IF lnActFolder = 1
  =lfWhnBrow()
ENDIF

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Function to validate the folder changing.
*!*************************************************************
*! Calls     : lfGetLines, lfBrowLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
  CASE lnActFolder = 2
ENDCASE

*!*************************************************************
*! Name      : lfBrwPos
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/04/1999
*! Purpose   : Function to display the position records for the 
*!           : current order line.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfBrwPos()
*!*************************************************************
*
FUNCTION lfBrwPos

SELECT (lcT_BomVar)
lnMark = RECNO()

IF llByCstStr 
  *-- Brow the copied items in the copy browse in the copy screen.
  BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
         cStylePos :H='Position' :R:10, ;
         cNDrpId   :H='Name Drop' :R:12, ;
         lcText1=LOOKUP(Icnamdrp.cndrptt,lcCust+lcStore+cNDrpId,Icnamdrp.cNDrpId,'Icnamdrp') :H='Text Top':R:15, ;
         lcText2=LOOKUP(Icnamdrp.cndrptb,lcCust+lcStore+cNDrpId,Icnamdrp.cNDrpId,'Icnamdrp') :H='Text Bottom':R:15, ;
         cDsgnCode :H='Dsgn Code' :R:10, ;
         lcDsnNam=LOOKUP(IcDesign.cDsgnName,cDsgnCode,IcDesign.cDsgnCode,'IcDesign') :H='Dsgn Name':R:15, ;
         nPriceAdd :H=IIF(lcType='SO','Price Adder','Cost adder') :R:15 ;
         WHEN lfWhnBrow() ;
         VALID :F lfVldBrow();
         NOEDIT            ;
         LOCK 0            ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
         WINDOW SoDsND1a   ;
         IN WINDOW SoDs_ND1 ;
         TITLE lcBrwVarT
ELSE
  *-- Brow the copied items in the copy browse in the copy screen.
  BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
         cStylePos :H='Position' :R:10, ;
         cNDrpId   :H='Name Drop' :R:12, ;
         lcText1=LOOKUP(Icnamdrp.cndrptt,cNDrpId,Icnamdrp.cNDrpId,'IcNamDrpID') :H='Text Top':R:15, ;
         lcText2=LOOKUP(Icnamdrp.cndrptb,cNDrpId,Icnamdrp.cNDrpId,'IcNamDrpID') :H='Text Bottom':R:15, ;
         cDsgnCode :H='Dsgn Code' :R:10, ;
         lcDsnNam=LOOKUP(IcDesign.cDsgnName,cDsgnCode,IcDesign.cDsgnCode,'IcDesign') :H='Dsgn Name':R:15, ;
         nPriceAdd :H=IIF(lcType='SO','Price Adder','Cost adder') :R:15 ;
         WHEN lfWhnBrow() ;
         VALID :F lfVldBrow();
         NOEDIT            ;
         LOCK 0            ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
         WINDOW SoDsND1a   ;
         IN WINDOW SoDs_ND1 ;
         TITLE lcBrwVarT
  
ENDIF

SHOW WINDOW (lcBrwVarT) REFRESH

*!*************************************************************
*! Name      : lfWhnBrow
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/05/1999
*! Purpose   : When function for browse of the style positions.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfWhnBrow()
*!*************************************************************
*
FUNCTION lfWhnBrow

*-- Set the browse pointer.
lnMark   = RECNO(lcT_BomVar)

*-- Flag to know that the active window is the browse.
glFromBrow = .T.

IF BOF() .OR. EOF()
  *-- Variable hold the style position code, name drop & design code.
  STORE "" TO lcStylePos , lcNamDrpId , lcDsgnCode , lcNDrpTT , lcNDrpTB
  *-- Variable hold the Price Adder.
  STORE 0 TO lnPriceAdd , lnNamPrice , lnDsnPrice
  *-- Define the design codes array.
  DECLARE laDsgnCod[1]
  laDsgnCod[1] = "Select Design Code"
  *-- Variable hold the popup value.
  lnDsgnPop  = 1
  *-- Variable hold the current fields' status.
  lcCurStat = "DISABLE"
ELSE
  *-- Variable hold the style position code.
  lcStylePos = &lcT_BomVar..cStylePos
  *-- Variable hold the name drop ID.
  lcNamDrpId   = &lcT_BomVar..cNDrpId
  
  *RAM
  lcNamSeek = IIF(llByCstStr , lcCust + lcStore + lcNamDrpId , lcNamDrpId)
  *RAM
  IF !EMPTY(lcNamDrpId) .AND. SEEK(lcNamSeek , "IcNamDrp")
    *-- Variable hold the name drop price.
    lnNamPrice = IIF(lcType = "SO" , ICNamDrp.nNDrpPric , ICNamDrp.nNDrpCost)
    lcNDrpTT   = ICNamDrp.cndrptt
    lcNDrpTB   = ICNamDrp.cndrptb
    *B603255,1 WAB - in case of calling  from So and it is multi Currency  convert 
    *B603255,1 WAB - the name drop price to the current currency
    *B603255,1 WAB - START
    IF lcType = 'SO'
      IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
        lnNamPrice = lnNamPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
      ENDIF
    ENDIF
    *B603255,1 WAB - END
  ELSE
    lnNamPrice = 0
    lcNDrpTT   = ""
    lcNDrpTB   = ""
  ENDIF
  *-- Variable hold the Design code.
  lcDsgnCode = &lcT_BomVar..cDsgnCode
  *-- Variable hold the Price Adder.
  lnPriceAdd = IIF(lcType = "SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost)
  *-- Variable hold the current fields' status.
  lcCurStat = IIF(laScrMode[2] , "DISABLE" , "ENABLE")
  
  *-- Call local function to fill the design codes popup.
  *B803099,1 ABD- Change the designs popup to be a get field same as name drop.
  *B803099,1 ABD- And change the valid function & don't need to fill the array. [Begin]
  *=lfFilDsnCd()
  *B803099,1 ABD- [End]
  *-- Get the design code popup value.
  IF !EMPTY(lcDsgnCode) .AND. SEEK(lcDsgnCode , "IcDesign")
    lcCodSrch  = lcDsgnCode + " - " + IcDesign.cDsgnName
    lnDsgnPop  = IIF(ASCAN(laDsgnCod , lcCodSrch) > 0 , ASCAN(laDsgnCod , lcCodSrch) , 1)
    *-- Variable hold the design code price.
    lnDsnPrice = IIF(lcType="SO" , ICDesign.nDsgnPrc , ICDesign.nDsgnCst)
    *B603255,1 WAB - in case of calling  from So and it is multi Currency  convert 
    *B603255,1 WAB - the design price to the current currency
    *B603255,1 WAB - START
    IF lcType = 'SO'
      IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
        lnDsnPrice = lnDsnPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
      ENDIF
    ENDIF
    *B603255,1 WAB - END
  ELSE
    lnDsgnPop  = 1
    *-- Variable hold the design code price.
    lnDsnPrice = 0
  ENDIF
ENDIF

*-- Refresh the objects on the screen.
SHOW GET lcStylePos &lcCurStat
SHOW GET lcNamDrpId &lcCurStat
SHOW GET pbAddNmDp  &lcCurStat
SHOW GET lnNamPrice &lcCurStat
SHOW GET lcNDrpTT   &lcCurStat
SHOW GET lcNDrpTB   &lcCurStat
SHOW GET lnDsgnPop  &lcCurStat
SHOW GET lnDsnPrice &lcCurStat
SHOW GET lnPriceAdd &lcCurStat
SHOW GET pbRemPos   &lcCurStat

*B803099,1 ABD- Change the designs popup to be a get field same as name drop. [Begin]
SHOW GET ibDsgnCode &lcCurStat
SHOW GET lcDsgnCode &lcCurStat
*B803099,1 ABD- [End]

*B603255,1 WAB - enable or disable browse butt. depend on lcCurstat
*B603255,1 WAB - START
SHOW GET ibStyPos &lcCurStat
SHOW GET IbNamDrp &lcCurStat
*B603255,1 WAB - END

IF laScrMode[2]
  SHOW GET pbNewPos DISABLE
ELSE
  SHOW GET pbNewPos ENABLE
ENDIF

*-- Refresh the browse.
SHOW WINDOW (lcBrwVarT) REFRESH SAME

*!*************************************************************
*! Name      : lfVldBrow
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/05/1999
*! Purpose   : Valid function for browse of the style positions.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVldBrow()
*!*************************************************************
*
FUNCTION lfVldBrow

*-- If exist from browse window.
IF !WONTOP(lcBrwVarT)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfActPos
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Activate function for the style positions screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActPos()
*!*************************************************************
*
FUNCTION lfActPos

*-- Clear browse as you are coming out of a browse window.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the trapped keys.
IF !INLIST(WONTOP() , lcBrwVarT)
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
*! Name      : lfDActPos
*! Developer : RAM - Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Deactivate function for the style positions screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDActPos()
*!*************************************************************
*
FUNCTION lfDActPos

glFromBrow = WONTOP() = lcBrwVarT

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB        DO lpPosTab
  ON KEY LABEL BACKTAB    DO lpPosBTab
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpPosTab
*! Developer : Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Trap of tab key for the style position screens...
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpPosTab
*!*************************************************************
*
PROCEDURE lpPosTab

ON KEY LABEL TAB

*-- Point to the current object from the browse if press tab from the browse.
ACTIVATE WINDOW SoDsND1b
_CUROBJ = OBJNUM(pbNewPos)

ON KEY LABEL TAB DO lpPosTab

*!*************************************************************
*! Name      : lpPosBTab
*! Developer : Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Trap of backtab key for the style position screen...
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpPosBTab
*!*************************************************************
*
PROCEDURE lpPosBTab

ON KEY LABEL BACKTAB

*-- Point to the current object from the browse if press 
*-- Shift + tab from the browse.
ACTIVATE WINDOW SoDs_Nd3
_CUROBJ = OBJNUM(pbClsPos)
    
ON KEY LABEL BACKTAB DO lpPosBTab

*!*************************************************************
*! Name      : lfvNewPos
*! Developer : Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Valid function for the new button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewPos()
*!*************************************************************
*
FUNCTION lfvNewPos

*-- Add new line in the temp. R/A lines file.
SELECT (lcT_BomVar)
LOCATE FOR EMPTY(cStylePos)
IF !FOUND()
  APPEND BLANK
ENDIF
BLANK
REPLACE cIdType  WITH lcType ;
        cCost_Id WITH lcOrdNo ;
        LineNo   WITH lcOrdLinNo ;
        cStatus  WITH "A"

*-- Call global function to add the audit fields to the newly added record.
=gfAdd_Info(lcT_BomVar)

*-- Variable hold the style position code.
lcStylePos = ""
*-- Variable hold the name drop ID.
lcNamDrpId   = ""
*-- Variable hold the name drop price.
lnNamPrice = 0
*-- Variable hold the name drop text top.
lcNDrpTT   = ""
*-- Variable hold the name drop text bottom.
lcNDrpTB   = ""
*-- Variable hold the Design code.
lcDsgnCode = ""
*-- Variable hold the design code price.
lnDsnPrice = 0
*-- Variable hold the Price Adder.
lnPriceAdd = 0

*-- Refresh the detail folder.
=lfWhnBrow()
_CUROBJ = OBJNUM(lcStylePos)

*!*************************************************************
*! Name      : lfvRemPos
*! Developer : Reham Al-Allamy
*! Date      : 07/06/1999
*! Purpose   : Valid function for the remove button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemPos()
*!*************************************************************
*
FUNCTION lfvRemPos

*-- Confirm Removing current line.
*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM00002B00006","ALERT") = 1
  *-- Calculate the new price for the order.
  lnAddPrice = lnAddPrice - IIF(lcType="SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost)
  *B605052,1 TMI [Start]  Increase the new price only by the diffrence
  *lnNewPrice = lnCurPrice + lnAddPrice
  lnNewPrice = lnCurPrice + lnAddPrice - lnOldAdPr
  *B605052,1 TMI [End  ] 
  *-- Refresh the changed prices.
  SHOW GET lnAddPrice
  SHOW GET lnNewPrice
  
  *-- Delete the record & change the status according to the current.
  SELECT (lcT_BomVar)
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  GO TOP
  
  *-- Call the when browse function.
  =lfWhnBrow()
ENDIF

*!*************************************************************
*! Name      : lfwOldStr
*! Developer : Reham Al-Allamy
*! Date      : 07/07/1999
*! Purpose   : When function for all string values.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwOldStr()
*!*************************************************************
*
FUNCTION lfwOldStr

lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvStyPos
*! Developer : Reham Al-Allamy
*! Date      : 07/07/1999
*! Purpose   : Valid function for the style position.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyPos()
*!*************************************************************
*
FUNCTION lfvStyPos
IF llBrowse .OR. (!EMPTY(lcStylePos) .AND. lcOldValue <> lcStylePos .AND. LASTKEY() = 13)
  SELECT ICStyPos
  IF llBrowse .OR. !SEEK(lcStyle + lcStylePos)
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    lcSaveBrow = lcBrFields
    *-- Set the browse fields.
    lcBrFields = [Style:H="Style":25,cStylePos:20:H="Style Position"]
    lcOld_ttl  = lcFile_ttl
    lcFile_ttl = "Style Positions for style: "+ALLTRIM(lcStyle)
    DECLARE laPos[1]
    laPos[1]   = lcStylePos
    *-- call the global browse to browse the available style positions for 
    *-- the current style in the order line.
    =gfBrows("lcStyle" , "cStylePos" , "laPos")
    lcBrFields  = lcSaveBrow
    lcFile_ttl  = lcOld_ttl
    lcStylePos  = IIF(lcStylePos = laPos[1] , lcOldValue , laPos[1])
  ENDIF
  
  llBrowse = .F.
  
  *-- If the style position was changed from old value.
  IF lcOldValue <> lcStylePos
    *-- Update the temp. file.
    SELECT (lcT_BomVar)
    REPLACE cStylePos WITH lcStylePos ;
            cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
    
    *-- Call local function to fill the design codes popup.
    *B803099,1 ABD- Change the designs popup to be a get field same as name drop.
    *B803099,1 ABD- And change the valid function & don't need to fill the array. [Begin]
    *=lfFilDsnCd()
    *B803099,1 ABD- [End]
    *-- Refresh the browse window.
    SHOW WINDOW (lcBrwVarT) REFRESH
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvNDrpId
*! Developer : Reham Al-Allamy
*! Date      : 07/07/1999
*! Purpose   : Valid function for the name drop.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNDrpId()
*!*************************************************************
*
FUNCTION lfvNDrpId

IF llBrowse .OR. (lcOldValue <> lcNamDrpId .AND. LASTKEY() = 13)
  SELECT IcNamDrp
  
  *B803099 ABD Fixed Bug that after you call Program icnmdrp that add new name 
  *B803099 ABD Drop id the browse didn't get any thing taht because the order
  *B803099 ABD Of the file is change from ICNAMDRP To ICNAMDRPID and I set to the old order 
  *B803099 ABD If Change to ICNAMDRPID. [Begin]
  
  *B605082,1 AMH Fix the bug cost of name drop does not display in case of Adornment order [Start]
  *IF ORDER() = "ICNAMDRPID"
  IF ORDER() = "ICNAMDRPID" .AND. llByCstStr
  *B605082,1 AMH [End]
  
    SET ORDER TO TAG Icnamdrp
  ENDIF
  *B803099 ABD [End]
  *RAM
  lcNamSeek = IIF(llByCstStr , lcCust + lcStore + lcNamDrpId , lcNamDrpId)
  *IF llBrowse .OR. (!SEEK(lcCust + lcStore + lcNamDrpId))
  IF llBrowse .OR. (!SEEK(lcNamSeek))
  *RAM
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    
    lcSaveBrow  = lcBrFields
    *-- Set the browse fields.
    lcBrFields  = [cNDrpTT:60:H="Text Top",cNDrpTB:60:H="Text Bottom",]+;
                  [cNDrpId:8:H="N-D Id",Account:8,Store:8,]+;
                  [cSpecIns:15:H="Instructions",]+;
                  [nNDrpPric:12:H="Price adder",nNDrpCost:12:H="Cost adder",]+;
                  [cNDrpClr:10:H="Color",cNDrpFnt:10:H="Font",nNDrppSize:10:H="Size",]+;
                  [nNDrpMaxLn:15:H="Max. Length"]
    
    lcOld_ttl   = lcFile_ttl
    DECLARE laNamDrp[1]
    laNamDrp[1] = lcNamDrpId
    
    IF llByCstStr
      lcFile_ttl  = "Name-Drops for customer/store: "+ALLTRIM(lcCust)+"/"+ALLTRIM(lcStore)
      *-- call the global browse to browse the available name drops for the current account+store.
      =gfBrows("lcCust+lcStore" , "cNDrpId" , "laNamDrp")
    ELSE
      lcFile_ttl  = "Name-Drops"
      *-- call the global browse to browse the available name drops for the current account+store.
      =gfBrows(.F. , "cNDrpId" , "laNamDrp")
    ENDIF
    
    lcBrFields = lcSaveBrow
    lcFile_ttl = lcOld_ttl
    lcNamDrpId   = IIF(lcNamDrpId = laNamDrp[1] , lcOldValue , laNamDrp[1])
    *RAM
    lcNamSeek = IIF(llByCstStr , lcCust + lcStore + lcNamDrpId , lcNamDrpId)
    *=SEEK(lcCust + lcStore + lcNamDrpId , "IcNamDrp")
    =SEEK(lcNamSeek , "IcNamDrp")
    *RAM
  ENDIF
  
  llBrowse = .F.
  
  *-- If the name drop was changed was changed from old value.
  IF lcOldValue <> lcNamDrpId
    *-- Variable hold the name drop price.
    lnNamPrice = IIF(EMPTY(lcNamDrpId) , 0 , IIF(lcType="SO", ICNamDrp.nNDrpPric , ICNamDrp.nNDrpCost))
    lcNDrpTT   = IIF(EMPTY(lcNamDrpId) , "" , ICNamDrp.cndrptt)
    lcNDrpTB   = IIF(EMPTY(lcNamDrpId) , "" , ICNamDrp.cndrptb)
    
    *-- Calculate the price adder.
    *B603255,1 WAB - in case of calling  from So and it is multi Currency  convert 
    *B603255,1 WAB - the name drop price to the current currency
    *B603255,1 WAB - START
    IF lcType = 'SO'
      IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
        lnNamPrice = lnNamPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
      ENDIF
    ENDIF
    *B603255,1 WAB - END
    lnPriceAdd = lnNamPrice + lnDsnPrice
    
    *-- Calculate the new price for the order.
    lnAddPrice = lnAddPrice - IIF(lcType="SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost) + lnPriceAdd
    *B605052,1 TMI [Start]  Increase the new price only by the diffrence
    *lnNewPrice = lnCurPrice + lnAddPrice
    lnNewPrice = lnCurPrice + lnAddPrice - lnOldAdPr
    *B605052,1 TMI [End  ] 
    
    *-- Update the temp file.
    SELECT (lcT_BomVar)
    REPLACE cNDrpId   WITH lcNamDrpId ;
            cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
    IF lcType = "SO"
      REPLACE nPriceAdd WITH lnPriceAdd
    ELSE
      REPLACE TotCost     WITH lnPriceAdd ;
              UntCost     WITH lnPriceAdd ;
              nBomTotQty  WITH 1
    ENDIF
    
    *-- Refresh the changed objects.
    SHOW GET lnNamPrice
    SHOW GET lcNDrpTT
    SHOW GET lcNDrpTB
    SHOW GET lnPriceAdd
    SHOW GET lnAddPrice
    SHOW GET lnNewPrice
    
    *-- Call local function to fill the design codes popup.
    *B803099,1 ABD- Change the designs popup to be a get field same as name drop.
    *B803099,1 ABD- And change the valid function & don't need to fill the array. [Begin]
    *=lfFilDsnCd()
    *B803099,1 ABD- [End]
    *-- Refresh the browse window.
    SHOW WINDOW (lcBrwVarT) REFRESH
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvDsgnCod
*! Developer : Reham Al-Allamy
*! Date      : 07/07/1999
*! Purpose   : Valid function for the design code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDsgnCod()
*!*************************************************************
*
*B803099,1 ABD- Change the designs popup to be a get field same as name drop.
*B803099,1 ABD- And change the valid function. [Begin]

FUNCTION lfvDsgnCod

*-- Get the mfg operation value.

*C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
*lcDsgnCode = IIF(lnDsgnPop = 1 , SPACE(6) , SUBSTR(laDsgnCod[lnDsgnPop] ,1 ,6))
lcDsgnCode = IIF(lnDsgnPop = 1 , SPACE(8) , SUBSTR(laDsgnCod[lnDsgnPop] ,1 ,8))
*C102797,1 AMH [End]

*-- If the design code was changed, update the temp. file.
IF lcDsgnCode <> &lcT_BomVar..cDsgnCode
  *-- Get the design code price.
  lnDsnPrice = IIF(!EMPTY(lcDsgnCode) .AND. SEEK(lcDsgnCode , "IcDesign") , IIF(lcType="SO" , ICDesign.nDsgnPrc , ICDesign.nDsgnCst) , 0)
  
  *-- Calculate the price adder.
  *B603255,1 WAB - in case of calling  from So and it is multi Currency  convert 
  *B603255,1 WAB - the design price to the current currency
  *B603255,1 WAB - START
  IF lcType = 'SO'
    IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
      lnDsnPrice = lnDsnPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
    ENDIF
  ENDIF
  *B603255,1 WAB - END

  lnPriceAdd = lnNamPrice + lnDsnPrice
  
  *-- Calculate the new price for the order.
  lnAddPrice = lnAddPrice - IIF(lcType="SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost) + lnPriceAdd
  *B605052,1 TMI [Start]  Increase the new price only by the diffrence
  *lnNewPrice = lnCurPrice + lnAddPrice
  lnNewPrice = lnCurPrice + lnAddPrice - lnOldAdPr
  *B605052,1 TMI [End  ] 
  
  *-- Update the temp file.
  SELECT (lcT_BomVar)
  REPLACE cDsgnCode WITH lcDsgnCode ;
          cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
  IF lcType = "SO"
    REPLACE nPriceAdd   WITH lnPriceAdd
  ELSE
    REPLACE TotCost     WITH lnPriceAdd ;
            UntCost     WITH lnPriceAdd ;
            nBomTotQty  WITH 1
  ENDIF
  
  *-- Refresh the changed objects.
  SHOW GET lnDsnPrice
  SHOW GET lnPriceAdd
  SHOW GET lnAddPrice
  SHOW GET lnNewPrice
  
  *-- Refresh the browse window.
  SHOW WINDOW (lcBrwVarT) REFRESH
ENDIF
*B803099,1 ABD- [End]
*!*************************************************************
*! Name      : lfvPricAdd
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1999
*! Purpose   : Function to validate the price adder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPricAdd()
*!*************************************************************
*
FUNCTION lfvPricAdd

IF !(lnOldValue == lnPriceAdd)
  *-- Calculate the new price for the order.
  lnAddPrice = lnAddPrice - IIF(lcType="SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost) + lnPriceAdd
  *B605052,1 TMI [Start]  Increase the new price only by the diffrence
  *lnNewPrice = lnCurPrice + lnAddPrice
  lnNewPrice = lnCurPrice + lnAddPrice - lnOldAdPr
  *B605052,1 TMI [End  ]  
  
  *-- Update the temp file.
  SELECT (lcT_BomVar)
  IF lcType = "SO"
    REPLACE nPriceAdd WITH lnPriceAdd ;
            cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
  ELSE
    REPLACE TotCost     WITH lnPriceAdd ;
            UntCost     WITH lnPriceAdd ;
            nBomTotQty  WITH 1 ;
            cStatus     WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
  ENDIF
  
  *-- Refresh the changed prices.
  SHOW GET lnAddPrice
  SHOW GET lnNewPrice
  
  *-- Refresh the browse window.
  SHOW WINDOW (lcBrwVarT) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfFilDsnCd
*! Developer : Reham Al-Allamy
*! Date      : 07/07/1999
*! Purpose   : Function to fill the array of the design codes.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfFilDsnCd()
*!*************************************************************
*
*B803099,1 ABD- Change the designs popup to be a get field same as name drop.
*B803099,1 ABD- And change the valid function & don't need to fill the array. [Begin]
FUNCTION lfFilDsnCd

*-- If the style position is not empty.
IF !EMPTY(lcStylePos)
  *-- Seek with the style position.
  =SEEK(lcStyle + lcStylePos , "IcStyPos")
  *-- If there is no name drop selected.
  IF EMPTY(lcNamDrpId)
    *-- Get the design codes of the "No Name-Drop" memo field into array.
    DECLARE laDsgnTmp[1]
    =gfSubStr(ALLTRIM(Icstypos.m0nd_dsgn) , @laDsgnTmp , ',')
    *-- Select all the design codes concatinating with its names.
    DECLARE laDsgnCod[1]
    laDsgnCod = " "

    *B802594,1 WAB - get the design code into array depends on icdsgnsize (desgin size code)
    *B802594,1 WAB - START
    *SELECT cDsgnCode + " - " + cDsgnName;
        FROM (gcDataDir+"IcDesign") ;
        WHERE ASCAN(laDsgnTmp , cDsgnCode) > 0 ;
        INTO ARRAY laDsgnCod

    *B603391,1 Start, Added order by to select.
    *SELECT cDsgnCode + " - " + cDsgnName;
        FROM (gcDataDir+"IcDesign") ;
        WHERE ASCAN(laDsgnTmp , Icdesign.cDsgnSize) > 0 ;
        INTO ARRAY laDsgnCod
    SELECT cDsgnCode + " - " + cDsgnName;
        FROM (gcDataDir+"IcDesign") ;
        WHERE ASCAN(laDsgnTmp , Icdesign.cDsgnSize) > 0 ;
        INTO ARRAY laDsgnCod ORDER BY 1
    *B603391,1 End.
    *B802594,1 WAB - END

  ELSE
    *RAM
    lcNamSeek = IIF(llByCstStr , lcCust + lcStore + lcNamDrpId , lcNamDrpId)
    *-- If there is name-drop was entered, check if it is 1 line or 2 lines.
    *=SEEK(lcCust + lcStore + lcNamDrpId , "IcNamDrp")
    =SEEK(lcNamSeek , "IcNamDrp")
    *RAM
    
    *-- Define array hold the design sizes.
    DECLARE laDsgnSiz[1]
    *-- If the name-drop is 2 text lines.
    IF !EMPTY(Icnamdrp.cndrptt) .AND. !EMPTY(Icnamdrp.cndrptb)
      *-- Get the design sizes of the 2 text name-drop memo field.
      =gfSubStr(ALLTRIM(Icstypos.m2nd_dsgn) , @laDsgnSiz , ',')
    ELSE
      *-- If the name-drop is 1 text lines.
      *-- Get the design sizes of the 1 text name-drop memo field.
      =gfSubStr(ALLTRIM(Icstypos.m1nd_dsgn) , @laDsgnSiz , ',')
    ENDIF
    *-- Select all the design codes related to the selected sizes.
    DECLARE laDsgnCod[1]
    laDsgnCod = " "
    *B603391,1 Start, Added order by to select.
    *SELECT cdsgnCode + " - " + cDsgnName;
       FROM (gcDataDir+"IcDesign") ;
       WHERE ASCAN(laDsgnSiz , Icdesign.cDsgnSize) > 0 ;
       INTO ARRAY laDsgnCod 
    SELECT cdsgnCode + " - " + cDsgnName;
       FROM (gcDataDir+"IcDesign") ;
       WHERE ASCAN(laDsgnSiz , Icdesign.cDsgnSize) > 0 ;
       INTO ARRAY laDsgnCod ORDER BY 1
    *B603391,1 End.
  ENDIF

  IF _TALLY <> 0
    *-- Add title bar in the design code poup.
    DIMENSION laDsgnCod[ALEN(laDsgnCod,1)+1]
    =AINS(laDsgnCod,1)
  ENDIF

  laDsgnCod[1] = "Select Design Code"
  *B603255,1 WAB - default the popup to the first elment
  
  IF ALEN(laDsgnCod,1) = 1
    lnDsgnPop = 1 
    =lfvDsgnCod()    
  ENDIF
  *B603255,1 WAB - START
  SHOW GET lnDsgnPop
ENDIF
*B803099,1 ABD- [End]
*!*************************************************************
*! Name      : lfvAddNmDp
*! Developer : Reham Al-Allamy
*! Date      : 07/22/1999
*! Purpose   : Valid function for push button <Add Name Drop>
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAddNmDp()
*!*************************************************************
*
FUNCTION lfvAddNmDp
PRIVATE lcAlias

*-- Save current alias.
lcAlias = ALIAS()

*-- Clear the trapped keys.
PUSH KEY
ON KEY

DECLARE laTempUsr[ALEN(laUsrFields,1) , ALEN(laUsrFields,2)]
=ACOPY(laUsrFields , laTempUsr)
laUsrFields = ""

*-- Variables needed to be difined before calling the new program.
lcCurrWind = gcBaseWind
=gfStatic()
lcWindow   = UPPER(lcBaseWind)
glFirsTime = .T.
gcBaseWind = 'AWRICNMDRP'
gcWinAppl  = "IC"

*-- Variable hold the name drop.
lcNDrpId   = ""
QcOldValue = ""
QcNamDrpId = ""
*-- Call the name drop screen with current account + store & variable 
*-- to hold the returned name drop ID.
DO (gcAppHome+'IC'+'\ICNMDRP') WITH lcCust , lcStore , lcNDrpId

IF !EMPTY(lcNDrpId)
  QcOldValue = lcNamDrpId
  QcNamDrpId = lcNDrpId
ENDIF

*-- Release the base window of the name drop screen.
RELEASE WINDOW (gcBaseWind)

*-- Refresh the control pannel.
SHOW GETS WINDOW gwccontrl1 LEVEL RDLEVEL()-1 ONLY

*-- Restore the trapped keys.
POP KEY

*-- Variables needed to be restored after calling the program.
gcBaseWind = lcCurrWind
glFirsTime = .F.
gcWinAppl  = "SO"
=gfsetup()

=ACOPY(laTempUsr , laUsrFields)

lcOldValue = IIF(!EMPTY(QcOldValue) , QcOldValue , lcOldValue)
lcNamDrpId = IIF(!EMPTY(QcNamDrpId) , QcNamDrpId , lcNamDrpId)
SHOW GET lcNamDrpId

*-- Restore the selected alias.
SELECT (lcAlias)

KEYBOARD "ENTER"
=lfvNDrpId()

*!*************************************************************
*! Name      : lfvConCls
*! Developer : Reham Al-Allamy
*! Date      : 07/19/1999
*! Purpose   : Function executes when closing the configure screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvConCls()
*!*************************************************************
*
FUNCTION lfvConCls
*B603211,1 WAB - Add Variable to determin at any case we push the butt. 'CLOSE'
*B603211,1 WAB -START
PRIVATE llClosScr
llClosScr = .T.			&& case there a record in temp file with name drop or design code
*B603211,1 WAB -END

IF !laScrMode[2] .AND. llPosition
  *-- Select the temp. file that hold the style positions.
  SELECT (lcT_BomVar)
  GO TOP
  
  *B603211,1 WAB - Commit this line cause no need for that message when close screen
  **-- Check if there is no style positions entered & inform the user.
  *IF EOF()
  *  *** You have to enter style position for Style: {lcStyle}. ***
  *  *** <  OK  > ***
  *  =gfModalGen("TRM32070B00000" , "DIALOG" , lcStyle)
  *ENDIF
  *WAB
  LOCATE FOR EMPTY(lcStylePos)
  IF FOUND()
    *  *** You have to enter style position for Style: {lcStyle}. ***
    *  *** <  OK  > ***
    =gfModalGen("TRM32070B00000" , "DIALOG" , lcStyle)
     llClosScr = .F.				
  ENDIF
  *WAB

  
  *-- Check if there is any entered lines have empty name drop and empty design code.
  *-- And inform the user that these fields has to be entered.
  LOCATE FOR EMPTY(cNDrpId) .AND. EMPTY(cDsgnCode)
  IF FOUND()
    *** You cannot leave both name-drop and design code empty for style: {lcStyle}. ***
    *** <  OK  > ***
    =gfModalGen("TRM32071B00000" , "DIALOG" , lcStyle)
    *B603211,1 WAB -START
    llClosScr = .F.				&& case empty name drop id and empty design code
    *B603211,1 WAB -END
  ENDIF
  *B603211,1 WAB - add IF statment to replace data only in case of there are an record
  *B603211,1 WAB - in temp. file with 'no name drop id' or design code
  *B603211,1 WAB -START
  IF llClosScr 
  *B603211,1 WAB -END
    *-- Update the cost adder in all the bomvar records.
    SCAN FOR cStatus $ "AM"
      *RAM
      lcNamSeek = IIF(llByCstStr , lcCust + lcStore + &lcT_BomVar..cndrpid , &lcT_BomVar..cndrpid)
      *RAM
      IF lcType = "SO"
        lnNameCost = IIF(SEEK(lcNamSeek , "ICNamDrp") , ICNamDrp.nNDrpCost , 0)
        lnDsgnCost = IIF(SEEK(&lcT_Bomvar..cDsgnCode , "ICDesign") , Icdesign.ndsgncst , 0)
        REPLACE nBomTotQty WITH 1 ;
                UntCost    WITH lnNameCost + lnDsgnCost ;
                TotCost    WITH lnNameCost + lnDsgnCost
      ELSE
        lnNameP = IIF(SEEK(lcNamSeek , "ICNamDrp") , ICNamDrp.nNDrpPric , 0)
        lnDsgnP = IIF(SEEK(&lcT_Bomvar..cDsgnCode , "ICDesign") , Icdesign.nDsgnPrc , 0)
        REPLACE nPriceAdd WITH lnNameP + lnDsgnP
      ENDIF
    ENDSCAN
  
    *-- Fill the price that will return to the sales order.
    lnRetPrice = lnNewPrice

  *B603211,1 WAB - END OF IF STATMENT
  *B603211,1 WAB -START
  ENDIF
  *B603211,1 WAB -END
ENDIF

*B603211,1 WAB - in case of there are an record with empty 'no name drop id' and 
*B603211,1 WAB - empty design code can not close the screen
*B603211,1 WAB -START
IF llClosScr
  CLEAR READ
ENDIF
*B603211,1 WAB -END
*!*************************************************************
*! Name      : lfvDesCode
*! Developer : Abdou Elgendi
*! Date      : 06/14/2000
*! Purpose   : Valid function for the Design Code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDesCode()
*!*************************************************************
*B803099,1 ABD- Change the designs popup to be a get field same as name drop.
*B803099,1 ABD- And change the valid function. [Begin]
FUNCTION lfvDesCode

IF llBrowse .OR. lcOldValue <> lcDsgnCode
  IF llBrowse .OR. (!EMPTY(lcDsgnCode) .AND. !SEEK(lcDsgnCode , "IcDesign"))
    SELECT IcDesign
    lcSaveBrow  = lcBrFields
    *-- Set the browse fields.
    lcBrFields = [cdsgncode:13:H="Design Code",cdsgnname:30:H="Design Name",]+;
                 [cdsgnclr:15:H="Design Color",cdsgnsize:15:H="Design Size",]+;
                 [season:15:H="season",]+;
                 [cvendcode:16:H="Vendor Code",nvenadmin:26:H="Adornment Contr. Minimum",] +;
                 [cdsgnlink:15:H="Design Link",royalty:30:H="Royalty Amount",nroyltamt:20:H="Royalty Amount",] +;
                 [ndsgncst:25:H="Design Adornment Cost",ndsgnprc:30:H="Design Adornment Price"]

    lcOld_ttl   = lcFile_ttl
    DECLARE laDsgnCode[1]
    laDsgnCode[1] = lcDsgnCode
    lcFile_ttl  = "Design Code"
    *-- call the global browse to browse the available name drops for the current account+store.
    =gfBrows(.F.,"cdsgncode","laDsgnCode")
    
    lcBrFields = lcSaveBrow
    lcFile_ttl = lcOld_ttl
    
    lcDsgnCode = IIF(lcDsgnCode = laDsgnCode[1] , lcOldValue , laDsgnCode[1])
    =SEEK(lcDsgnCode , "IcDesign")
  ENDIF
  
  llBrowse = .F.
ENDIF

*-- If the design code was changed, update the temp. file.
IF lcDsgnCode <> &lcT_BomVar..cDsgnCode
  *-- Get the design code price.
  lnDsnPrice = IIF(!EMPTY(lcDsgnCode) .AND. SEEK(lcDsgnCode , "IcDesign") , IIF(lcType="SO" , ICDesign.nDsgnPrc , ICDesign.nDsgnCst) , 0)
  
  *-- Calculate the price adder.
  IF lcType = 'SO'
    IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
      lnDsnPrice = lnDsnPrice &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
    ENDIF
  ENDIF

  lnPriceAdd = lnNamPrice + lnDsnPrice
 
  *-- Calculate the new price for the order.
  lnAddPrice = lnAddPrice - IIF(lcType="SO" , &lcT_BomVar..nPriceAdd , &lcT_BomVar..TotCost) + lnPriceAdd
  *B605052,1 TMI [Start]  Increase the new price only by the diffrence
  *lnNewPrice = lnCurPrice + lnAddPrice
  lnNewPrice = lnCurPrice + lnAddPrice - lnOldAdPr
  *B605052,1 TMI [End  ] 
  
  *-- Update the temp file.
  SELECT (lcT_BomVar)
  REPLACE cDsgnCode WITH lcDsgnCode ;
          cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
  IF lcType = "SO"
    REPLACE nPriceAdd   WITH lnPriceAdd
  ELSE
    REPLACE TotCost     WITH lnPriceAdd ;
            UntCost     WITH lnPriceAdd ;
            nBomTotQty  WITH 1
  ENDIF
  
  *-- Refresh the changed objects.
  SHOW GET lnDsnPrice
  SHOW GET lnPriceAdd
  SHOW GET lnAddPrice
  SHOW GET lnNewPrice
  
  *-- Refresh the browse window.
  SHOW WINDOW (lcBrwVarT) REFRESH
ENDIF

*-- End Of lfvDesCode.
*!*************************************************************
