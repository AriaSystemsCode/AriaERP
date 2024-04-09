*:************************************************************************
*: Program file  : MFCSTSH.PRG
*: Program desc. : Style Cost Sheet
*: For screen    : MFCSTSH.SCX (0,1,2,3,4,5,6)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufactering Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300722,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : MFCstSc.Fxp (New Cost Item Program)
*:                      MFCSTSZ.Fxp (Select sizes screen)
*:                      Costing.FXP (Costing Program)
*:                      NOTEPAD.Fxp (Note Pad Program)
*:         Screens    : MFCSTSH.SPX (Main Style Cost Sheet screen)
*:                      MFSTYCP.SPX (Copy from another cost sheet screen)
*:                      MFSerch.SPX (Search screen related to the main browse)
*:         Functions  : lpShow
*:                      lfvData_1
*:                      lfvOnSize
*:                      lfvSelSize
*:                      lfCpyCstSht
*:                      lfBrwCstSh
*:                      lfShowDet
*:                      lfRefLin
*:                      lfvBrows
*:                      lfReadAct
*:                      lfDactMain
*:                      lfvNewEle
*:                      lfvRemEle
*:                      lfvSearch
*:                      lfvCstItmDs
*:                      lfvOprCode
*:                      lfvDuty
*:                      lfvUnitQty
*:                      lfvUom
*:                      lfvUnitCst
*:                      lfvCstPrc
*:                      lpDelScr
*:                      lpSavScr
*:                      lfCostInfo
*:                      lfvNotes
*:                      lfvCosting
*:                      lfvCpyItem
*:                      lfvCopy
*:                      lfvSel
*:                      lfvNone
*:                      lfvInvert
*:                      lfvAll
*:                      lfvCanCop
*:                      lfBrwCopy
*:                      lfWhenCopy
*:                      lfvSCstTyp
*:                      lfvSItmMsk
*:                      lfvSItems
*:                      lfvSItmClr
*:                      lfvSrOk
*:*************************************************************
*: Passed Parameters  : lcStyleTyp : 'M' Manufacturing Styles
*:                                   'I' Imported Styles
*:                                   'T' Material
*:*************************************************************
*: Example            :
*:  Call the program with "M" parameter for Style Cost Sheet.
*:  DO (gcAppHome+'\MFCSTSH') WITH 'M'
*:*************************************************************
*:  Modifications :
*E301030,1  Reham On 10/10/98
*E301030,1  Change the style detail costing to be on the style level 
*E301030,1  instead of style purchase orders module level.
*B602102,1  Reham On 10/25/98
*B602102,1  Fix the problem of cross reference the colors in: lfvCopy()
*E301045,1  Reham On 10/26/98
*E301045,1  Map the sizes between style parent & style component upon copy
*E301045,1  cost sheet from another style.
*B801813,1  Reham On 11/16/98
*B801813,1  1- adjust the total cost decimals in the screen to be 2 digits 
*B801813,1     instead of 3 digits.
*B602202,1  Reham On 11/16/98
*B602202,1  1- Disable check box [ ] Cost sheet by size if at least add 
*B602202,1     1 size in any cost sheet.
*B602213,1  Reham On 11/17/98
*B602213,1  1- Fix the problem of the popup that hold (All Values - Specific values)
*B602213,1     For style code structure that include segments saved in the
*B602213,1     ICSegVal file in the Major & the non-major.
*B602134,1  Reham On 11/22/98
*B602134,1  Fix the wrong calculations of the style cost fields that updated
*B602134,1  wrong in the style file.
*B602132,1  Reham On 11/29/98
*B602132,1  Add new Pad in the menu contain a bar for costing screen.
*B602293,1  Reham On 12/01/98
*B602293,1  Point the record poistion in the temp. BOM file after return from the search screen.
*B602298,1  Reham On 13/12/98
*B602298,1  Call the browse function to redisplay the style cost sheet lines again 
*B602298,1  if blanked from the costing program.
*B602132,1  Reham On 12/17/98
*B602132,1  If the long description for the segment was empty, display the short description.
*E301077,16 Reham On 12/30/98
*E301077,16 Reduce the opened file on the program level:
*E301077,16 1- For the main record "MFCSTSH", change the files in SydObjct:
*E301077,16    From : STYLE,STYDYE,STYINVJL,FABRIC,FABDYE,MATINVJL,BOM,CODES,SCALE,ICSEGVAL|CSTYLE,STYDYE,STYINVJL,CFABRIC,FABDYE,MATINVJL,BOM,CCODE_NO,SCALE,SEGVAL
*E301077,16    To   : STYLE,FABRIC,BOM,CODES,SCALE|CSTYLE,CFABRIC,BOM,CCODE_NO,SCALE
*E301077,16 2- For imported style cost sheet program, change the files in SydObjct:
*E301077,16    From : STYLE,STYDYE,STYINVJL,FABRIC,FABDYE,MATINVJL,BOM,CODES,SCALE,ICSEGVAL,SYCCURR,SYCEXCH|CSTYLE,STYDYE,STYINVJL,CFABRIC,FABDYE,MATINVJL,BOM,CCODE_NO,SCALE,SEGVAL,CCURRCODE,CURRENCY
*E301077,16    To   : STYLE,FABRIC,BOM,CODES,SCALE|CSTYLE,CFABRIC,BOM,CCODE_NO,SCALE
*E301077,16 3- For manufacturing style cost sheet program, change the files in SydObjct:
*E301077,16    From : STYLE,STYDYE,STYINVJL,FABRIC,FABDYE,MATINVJL,BOM,CODES,SCALE,ICSEGVAL|CSTYLE,STYDYE,STYINVJL,CFABRIC,FABDYE,MATINVJL,BOM,CCODE_NO,SCALE,SEGVAL
*E301077,16    To   : STYLE,FABRIC,BOM,CODES,SCALE|CSTYLE,CFABRIC,BOM,CCODE_NO,SCALE
*E301077,16 4- For material cost sheet program, change the files in SydObjct:
*E301077,16    From : STYLE,STYDYE,STYINVJL,FABRIC,FABDYE,MATINVJL,BOM,CODES|CSTYLE,STYDYE,STYINVJL,CFABRIC,FABDYE,MATINVJL,BOM,CCODE_NO
*E301077,16    To   : STYLE,FABRIC,BOM,CODES|CSTYLE,CFABRIC,BOM,CCODE_NO
*E301077,16 5- Open the removed files from SydObjct in the current program.
*B602395,1  Reham On 01/04/99
*B602395,1  Disable the unit cost & Uom fields if the trim from file.
*B801908,1  Reham On 01/28/99
*B801908,1  Get the style primary fabric for the "copy from style" 
*B801908,1  to be saved for the current style cost sheet.
*B801924,1  Reham On 02/01/99
*B801924,1  Enable the costing button in add mode.
*B602539,1  Reham On 02/21/99
*B602539,1  Fix error "Variable laSeg not found" by not calling function 
*B602539,1  "gfItemMask" if the program is material cost sheet.
*E301176,1  HDM 03/22/1999 Prevent programs from displaying notepad icon
*E301176,1  as it's now controlled globally
*B602895,1  Reham On 05/19/1999
*B602895,1  Update the total cost with rounded cost items instead of rounding their totals.
*B603016,1  Reham On 06/27/1999
*B603016,1  1- Fix the bug of retrieving the "Trim Inventory maintained"
*B603016,1     value from setup file.
*B603016,1  2- Check if there is duty or Mfg cost items before updating 
*B603016,1     the cost in case of Imported style cost sheet to void errors.
*B802394,1  Reham On 08/03/1999
*B802394,1  Check if there is any changed cost for the style, fabric & trims cost 
*B802394,1  item in their files.
*C200080,6  Reham On 08/11/1999
*C200080,6  Enable the unit qty. & wastage if the the cost item is MFG.
*B603309,1  Reham On 12/04/1999
*B603309,1  Fix the bugs upon updating the material cost in the save procedure.
*B603348,1  Abdou on 02/02/2000
*B603348,1  Increase The size of Purchase price per item (unit cost field) to 9999999.999 
*E301411,1 SSH 31/05/20 Enhancement this program to support Piece Work
*E301411,1              Module by adding new button to assigne detail Oper.
*E301411,1              to the mfg. operation.
*B603765,1 ABD 07/25/2000 Fix bug that the calculation of TOTCOST and AVE_COST in the 
*B603765,1 ABD            STYLE file is  wrong.
*B603726,1 ABD 07/25/2000 Fix bug that when copy Style cost sheet from another style
*B603726,1 ABD            Cost sheet in different currency the program accumlate wrong cost.
*B803553,1 ABD 08/08/2000 Fix bug that miscalculate the duty charges on the style BOM.
*B603872,1 SSH 03/09/2000 Fix the bug of  "file pwbom.dbf is missing"
*B603872,1 SSH            when delete style cost sheet and PW module not installed
*B603747,1 ABD 09/06/2000 Add Cheak if make cost sheet for Manufacturing Styles.
*B603589,1 AMH 09/12/2000 Updating Gros_Price in Style File.
*E500402,1 AMH 01/10/2001 Adding the marker field in screen MFCSTSH2.
*E500402,1                The marker field will be shown in case 
*E500402,1                of Fabric only and "SP" module is installed.
*E301583,1 AMH 03/15/2001 Adding new festure to import Style Cost Sheets.
*B604328,1 AMH 04/03/2001 Adding the imported style the feature of import 
*B604328,1                style cost sheets
*B604408,1 WAB 04/18/2001 fix the bug of file does not exist while inporting from PDM Module
*B604433,1 AMH 01/10/2001 Using PDMPATRN file insted of SPPITEM file to validate marker field.
*B604508,1 AMH 05/31/2001 Fix the bug of importing the style component.
*B604569,1 AMH 06/20/2001 Using PDMMARK file instead of PDMPATRN file to validate marker field.
*B604632,1 SSH Use the module variable instead of installed modules variable.
*B604632,1 AMH 08/06/2001 Fix the bug of not copying some cost elements correctly.
*B604813,1 AMH 08/19/2001 Fix the bug of not replacing the values of the 
*B604813,1                cost elements in the style.
*B604802,1 AMH 08/19/2001 Fix the syntax error bug in the edit mode when move
*B604802,1                the mouse from the % field to the unit cost field.
*B604861,1 AMH 09/03/2001 Don't Update Wastage field form PDM data.
*B604899,1 AMH 09/11/2001 Update custom fields for Cathy Daniels from PDMMARK file.
*B605400,1 AMH 01/30/2002 Optimizing the speed of copy cost sheet.
*B605444,1 AMH 01/30/2002 Fix the bug of copy style cost sheet from style has different scale.
*C200303,1 HBG 13/03/2002 Add a new option to Amend materials costs on trigger for "Collage Ltd"
*B605714,1 AMH 03/20/2002 Fix the bug of not copy all sizes of style cost sheet based on size
*B605714,1                when copy it from style has different scale.
*B605726,1 KHM 03/27/2002 Fix the bug of not displaying the operations according
*B605726,1                there sequences
*E301842,1 AMH 04/15/2002 Change the fabric/trim in the edit mode.
*C200392,1 AMH 08/19/2002 Add new par in option menu 'Item Description' for customer DAR12.
*C102742,1 AMH 10/10/2002 Display the cost status popup in the MF style cost sheet
*B606979,1 AMH 02/25/2002 Fix the bug of scale code error in case of style componant
*B606979,1 AMH            for extended size scale style.
*B607469,1 SSE 08/21/2003 Fix bug of not pointing to the nearest record when browsing Styles.
*B123630,1 MHM 08/26/2004 Fix bug of Primary fabric replaced in case of copy cost sheet.
*B038753,1 WSH 12/20/2004 Update Inventory Jounal file with correct value when cost changes.
*B127591,1 MMR 05/18/2005 Fix Bug of Variable not found.
*B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy 
*B607902,1 TMI 12/24/2006 Increase the array "laCostFld" according to the number of cost lines
*:************************************************************************************
*
PARAMETERS lcStyleTyp
EXTERNAL ARRAY laData , laKeyField , laDefProc

*E301411,1 SSH [Begin] Documentation.
*--- This Enhancemet is available only if : -
*--- 1) The piece work module instaled.
*--- 2) Running the program throwgth the MF module.
*--- 3) The currernt Cost Item is MFG Operation , Consider as Operation
*---    And has Detail operation in PWOPERAT.DBF
*---    
*--- It required the following DBF's : -
*--- 1) PWBOM
*--- 2) PWBOMTOL
*--- 3) PWOPERAT
*--- 4) PWOPTOOL
*--- General Notes :
*--- If the same MfgCode assigned on the same style more than once
*--- Then it is not nessesary for these MfgCode to have the same 
*--- detail operation or the same costs in PWBOM.DBF.
*--- and the detail operation maybe has different tools in PWBOMTOL.DBF.
*E301411,1 SSH [End]
DECLARE laKeyField [1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = IIF(lcStyleTyp <> 'T' , 'CStyle' , 'CFabric')
laKeyField[1,4] = 1
*B128128,1 HBG 11/20/2005 Add array to calculate the cost according to the base currancy [Begin]
DECLARE laCostFld[5,2]
STORE 0 To laCostFld
*B128128,1 [End]

*-- Add more buttons to the control pannel.
*E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*E301176,1 as it's now controlled globally

*DECLARE laPanelObj[2,3]
DECLARE laPanelObj[1,3]

*laPanelObj[1,1] = "pbNotes"
*laPanelObj[1,2] = gcBmpHome + "NOTES2.BMP"
*laPanelObj[1,3] = "VALID lfvNotes() MESSAGE 'Notes'"

laPanelObj[1,1] = "pbCosting"
laPanelObj[1,2] = gcBmpHome + "COSTING1.BMP"
laPanelObj[1,3] = "VALID lfvCosting() MESSAGE 'Costing'"

*E301176,1 HDM 03/22/1999[End]

*-- Define the base file.
lcItemFile = IIF(lcStyleTyp = 'T' , 'Fabric' , 'Style')

laDefProc[7]  = .F.     && Force to local void procedure  (lDelScr)
laDefProc[9]  = .F.     && Force to local Save procedure  (lpSavScr)

*B602213,1 Reham On 11/17/98    *** Begin ***
*B602213,1 Increase the style segment array columns to be 9 instead of 8
*-- Define the needed arrays in this file.
*DECLARE laStySeg[1,1] , laStyCSeg[1,8] , laDutyable[1,1] , ;
        laOprCode[1,2] , laMfgCode[1] , laCodInfo[1,10] , ;
        laCostType[IIF(lcStyleTyp='T',4,5),2]
DECLARE laStySeg[1,1] , laStyCSeg[1,9] , laDutyable[1,1] , ;
        laOprCode[1,2] , laMfgCode[1] , laCodInfo[1,10] , ;
        laCostType[IIF(lcStyleTyp='T',4,5),2]
*B602213,1 Reham On 11/17/98    *** End   ***

*E301077,16 Reham On 02/22/99   *** Begin ***
*E301077,16 Array hold the memory variables that need to be load 
*E301077,16 from the company setup file.
DECLARE laMainSetp[IIF(lcStyleTyp = 'T' , 15 , 18) ,2]
*laMainSetp = ""
*E301077,16 Reham On 02/22/99   *** End   ***

*B602539,1 Reham On 02/21/99  *** Begin ***
*-- Get the style header.
*lcStyleHdr = gfItemMask("HI")
lcStyleHdr = IIF(lcStyleTyp='T' , "Fabric" , gfItemMask("HI"))
*B602539,1 Reham On 02/21/99  *** End   ***

*-- Get the prompt of the available pictures on the screen.
lcSBrowBmp = gcBmpHome + "ExtKey.Bmp"
lcSNewBmp  = gcBmpHome + "New1.Bmp"
lcSRemBmp  = gcBmpHome + "Rem1.Bmp"
lcSSerBmp  = gcBmpHome + "Search.Bmp"
lcSCpyBmp  = gcBmpHome + "Copy1.Bmp"
lcSOkBmp   = gcBmpHome + "Ok.Bmp"
lcSCanBmp  = gcBmpHome + "Can.Bmp"

*-- Define the variables that hold the screen names.
STORE ' ' To lcCstSh0 , lcCstSh1 , lcCstSh2 , lcCstSh3 , ;
             lcCstSh4 , lcCstSh5 , lcCstSh6 , lcTmpFbric , ;
             lcTmpStyle , lcTmpBom , lcTmpCopy , lcBomTemp

*B605400,1 AMH Add variable to index of lcTmpCopy file [Start]
STORE ' ' TO lcTmpCopy1
*B605400,1 AMH [End]

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
             lnOldPerc

STORE 1   TO lnExRate , lnCurrUnit

*E301030,1 Reham On 10/01/98  *** Begin ***
*STORE .F. TO llImpCost
*E301030,1 Reham On 10/01/98  *** End   ***

*B602202,1 Reham On 11/16/98   *** Begin ***
*B602202,1 Define flag to know if at least one cost item include 1 size.
llOnSize  = .F.
*B602202,1 Reham On 11/16/98   *** End   ***

STORE .F. TO llBrowse , llStyDye , llTrimInv , ;
             llMfgOpr , cbOnSize , cbSelSize , llSelSize , ;
             llNewEntry , llMulCurr , llEdit
*llDyelot = .F.

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

DECLARE laData[1]
laData     = ""

*-- None of the screen's objects has been updated.
llCUpDate  = .F.
*E301411,1 SSH [Begin] PW module Variable.
llPwInst = .T.
IF lcStyleTyp = 'M'
  lcDetLin  = ''
  lcPWBom   = ''
  lcPWTol   = ''
  llNewSess = .T.
  llFromCtk = .F.
ENDIF
*E301411,1 SSH [End]

*E301583,1 AMH Flage to check if you are using the importing feature or not.[Start]
llImport = .F.
*B604408,1 - WAB (Start) check first if the files is exist becuase in case of PDM module is installed 
*B604408,1 - WAB (Start)  the needed files for inmprtoing still not prepared by PDM teamllOpnPdm = .F.
IF 'SP' $ gcComp_Mdl
  llOpnPdm   = FILE(gcDataDir+'PDMBOM.DBF')
  llOpnPdm   = FILE(gcDataDir+'PDMLOG.DBF')
ENDIF
*B604408,1 - WAB (End)
*E301583,1 AMH [End]

*B604433,1 AMH Define lcOldMrker to hold the marker old value [Start]
STORE SPACE(10) TO lcOldMrker
*B604433,1 AMH [End]

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.

IF !gfSetup()
  RETURN
ENDIF

*-- Array for the header data, & variable hold the header fields.
lcScFields = IIF(lcStyleTyp = 'T' , 'Fabric' , 'cStyMajor')

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*E301077,16 Reham On 12/30/98   *** Begin ***
*E301077,16 Open the company file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
IF llMulCurr
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'CCURRCODE','SH')
  =gfOpenFile(gcSysHome+'SYCEXCH',gcSysHome+'CURRENCY','SH')
ENDIF
*E301077,16 Reham On 12/30/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  *E301030,1 Reham On 10/01/98   *** Begin ***
  *E301030,1 Remove and comment checking using if using detail costing 
  *E301030,1 method on the style purchase order module.
  *llImpCost = gfGetMemVar('M_lImpCost',gcAct_Comp)
  *IF lcStyleTyp = 'I' .AND. !llImpCost
  *  *** The system has not been setup to use detailed ***
  *  *** costing for imported styles.  Cannot proceed. ***
  *  *** < OK > ***
  *  =gfModalGen("TRM38000B00000" , "DIALOG")
  *  glQuitting = .T.
  *  RETURN
  *ENDIF
  *E301030,1 Reham On 10/01/98   *** End   ***
  
  *-- Get the needed information from the company setup.
  *E301077,16 Reham On 02/22/99   *** Begin ***
  *llDyelot   = IIF(lcStyleTyp <> "T" , ;
                   ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp)) = 'Y' , ;
                   ALLTRIM(gfGetMemVar('M_MATDYE',gcAct_Comp)) = 'Y')
  
  *E301077,16 Reduce the calling of global function gfGetMemVar()
  *FOR lnCount = 1 TO IIF(lcStyleTyp = 'T' , 4 , 5)
  *  lcCount  = STR(lnCount,1)
  *  lc&lcStyleTyp.SLBL&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'SLBL'+lcCount , gcAct_Comp)
  *  lc&lcStyleTyp.COST&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'COST'+lcCount , gcAct_Comp)
  *  lc&lcStyleTyp.TYPE&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'TYPE'+lcCount , gcAct_Comp)
  *  laCostType[lnCount,1]      = lc&lcStyleTyp.SLBL&lcCount
  *  laCostType[lnCount,2]      = lnCount
  *ENDFOR 
  
  *llTrimInv  = ALLTRIM(gfGetMemVar('M_TINVT',gcAct_Comp)) = 'Y'
  *llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
  *llMulCurr = (lcStyleTyp='I') .AND. gfGetMemVar('llMulCurr' , gcAct_Comp)
  
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
  *B603016,1  Reham On 06/27/1999   *** Begin ***
  *B603016,1  Fix the bug of retrieving the "Trim Inventory maintained"
  *B603016,1  value from setup file.
  *llTrimInv  = ALLTRIM(laMainSetp[1,2]) = 'Y'
  llTrimInv  = ALLTRIM(laMainSetp[1,2]) = 'N'
  *B603016,1  Reham On 06/27/1999   *** End   ***
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
  *E301077,16 Reham On 02/22/99   *** End   ***
  
  *B602539,1 Reham On 02/21/99  *** Begin ***
  *-- Get the picture of the style.
  *lcStyPic   = gfItemMask("PI")
  lcStyPic   = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PI"))
  *B602539,1 Reham On 02/21/99  *** End   ***
  
  *-- Get the major picture or material picture.
  lcItemPic  = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM"))
  *-- Get the non major picture.
  lcNMjrPic  = IIF(lcStyleTyp='T' , "XXXXXX" , gfItemMask("PN"))
  
  *-- Get the style lenght.
  lnStyLen   = LEN(lcStyPic)
  *-- Get the major length or the material legnth.
  lnItemLen  = LEN(lcItemPic)
  
  *B602539,1 Reham On 02/21/99  *** Begin ***
  *-- Picture of the style major.
  *lcMajorPic = gfItemMask("PM")
  lcMajorPic = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM"))
  *B602539,1 Reham On 02/21/99  *** End   ***
  
  *-- Lenght of the the style major.
  lnMajorLen = LEN(lcMajorPic)
  
  *B602539,1 Reham On 02/21/99  *** Begin ***
  *-- Get the non-major lenght.
  *lnNonMjLen = LEN(gfItemMask("PN"))
  lnNonMjLen = IIF(lcStyleTyp='T' , 6 , LEN(gfItemMask("PN")))
  
  *-- Get the whole style picture.
  *lcStyMask  = STRTRAN(gfItemMask("PI") , "X" , "*")
  lcStyMask  = IIF(lcStyleTyp='T' , "*******" , STRTRAN(gfItemMask("PI") , "X" , "*"))
  
  *-- Get the style mask separator.
  *lcSeprator = SUBSTR(lcStyleHdr , lnMajorLen+1 , 1)
  lcSeprator = IIF(lcStyleTyp='T' , "" , SUBSTR(lcStyleHdr , lnMajorLen+1 , 1))
  *B602539,1 Reham On 02/21/99  *** End   ***
  
  *-- Get the non-major header.
  lcNonMjHdr = IIF(lcStyleTyp='T' , 'Color' , gfItemMask("HN"))
  *-- Get the major mask (header).
  lcItemExp  = IIF(lcStyleTyp='T' , 'Material' , gfItemMask("HM"))
  
  *-- See if there is non-major segments or not.
  llNonMjExt = !EMPTY(lcNonMjHdr)
  
  *B602539,1 Reham On 02/21/99  *** Begin ***
  IF lcStyleTyp = 'T'
    STORE "" TO laStyCSeg , laStyNSeg , laStySeg
  ELSE
  *B602539,1 Reham On 02/21/99  *** End   ***
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
        *B602213,1 Reham On 11/17/98    *** Begin ***
        *B602213,1 Increase the style segment array columns to be 9 instead of 8
        *DECLARE laStyCSeg[ALEN(laStyCSeg,1)+1,8]
        DECLARE laStyCSeg[ALEN(laStyCSeg,1)+1,9]
        *B602213,1 Reham On 11/17/98    *** End   ***
      ENDIF
      
      *B602132,1 Reham On 12/17/98    *** Begin ***
      *B602132,1 If the long description for the segment was empty, display the short description.
      *laStyCSeg[ALEN(laStyCSeg,1),1] = IIF(!EMPTY(laStySeg[lnCnt , 5]) , laStySeg[lnCnt , 5] , "Segment # "+STR(lnCnt,1)) && Long Description
      laStyCSeg[ALEN(laStyCSeg,1),1] = IIF(!EMPTY(laStySeg[lnCnt , 5]) , laStySeg[lnCnt , 5] , laStySeg[lnCnt , 2])
      *B602132,1 Reham On 12/17/98    *** End   ***
      
      laStyCSeg[ALEN(laStyCSeg,1),2] = laStySeg[lnCnt , 1] && Type
      laStyCSeg[ALEN(laStyCSeg,1),3] = laStySeg[lnCnt , 3] && Picture
      laStyCSeg[ALEN(laStyCSeg,1),4] = laStySeg[lnCnt , 4] && Start Position
      laStyCSeg[ALEN(laStyCSeg,1),5] = laStySeg[lnCnt , 2] && Short Desc.
      laStyCSeg[ALEN(laStyCSeg,1),6] = laStySeg[lnCnt , 6] && Separator
      laStyCSeg[ALEN(laStyCSeg,1),7] = laStySeg[lnCnt , 7] 
      laStyCSeg[ALEN(laStyCSeg,1),8] = "A"      && All Values or Selected values.
      *B602213,1 Reham On 11/17/98    *** Begin ***
      *B602213,1 define array column to hold the segment position # in 
      *B602213,1 the style code structure.
      laStyCSeg[ALEN(laStyCSeg,1),9] = lnCnt
      *B602213,1 Reham On 11/17/98    *** End   ***
    ENDFOR
  *B602539,1 Reham On 02/21/99  *** Begin ***
  ENDIF
  *B602539,1 Reham On 02/21/99  *** End   ***
  
  *-- Adjust the non major info if there is extended size scale.
  IF llExtSizSc .AND. lcStyleTyp <> "T"
    lnNonMjLen = lnNonMjLen - lnSizeLen
    *B602385,1 Reham On 01/05/99   *** Begin ***
    *B602385,1 If there is no non major except the scale, consider there is no non major.
    IF lnNonMjLen < 0
      lnNonMjLen = 0
      llNonMjExt = .F.
    ENDIF
    *B602385,1 Reham On 01/05/99   *** End   ***
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
    *E301077,16 Reham On 12/30/98   *** Begin ***
    *E301077,16 Open the currency & exchange files.
    =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'CCURRCODE','SH')
    =gfOpenFile(gcSysHome+'SYCEXCH',gcSysHome+'CURRENCY','SH')
    *E301077,16 Reham On 12/30/98   *** End   ***
    =SEEK(gcBaseCurr,'SycCurr')  
    lcBsCrSmbl = SycCurr.cCurrSmbl
  ENDIF
  
  *-- Get the cost different status data in the laDutyable array from the dictionary.
  =gfGetVld("CCOSTSTAT" , @laDutyable)
  
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
  
  *B605400,1 AMH Add variable to index of lcTmpCopy file [Start]
  lcTmpCopy1 = gfTempName()
  *B605400,1 AMH [End]
  
  lcBomTemp  = gfTempName()
  *E301411,1 SSH [Begin] Check If the PW module Instaled.
  *E301411,1 SSH         IF installed and the current style type is MF.
  *E301411,1 SSH         Start Get Temp Files Name.
  *E301411,1 SSH         And Define new variable llNewSess to indicate
  *E301411,1 SSH         if we are modifing in the same session.
  *B604641,1 SSH Use the module variable instead of installed modules variable.
  *llPwInst = (OCCURS('PW',gcCmpModules)<>0)
  llPwInst = (OCCURS('PW',gcComp_Mdl)<>0)
  *B604641,1 SSH [END]
  *B603872,1 SSH 03/09/2000 [Start] Commented out.
  *llPwInst = .T.
  *B603872,1 SSH [END]
  llFromCtk = .F.
  IF llPwInst .AND. lcStyleTyp = 'M'
    lcDetLin  = gfTempName()
    lcPWBom   = gfTempName()
    lcPWTol   = gfTempName()
    llNewSess = .T.
  ENDIF
  *E301411,1 SSH [End]
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
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  CREATE TABLE (gcWorkDir+lcTmpBom) FROM ARRAY laFileStru   && Temp Bom.
  INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)
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
  
  *B605400,1 AMH Add index of lcTmpCopy file [Start]
  INDEX ON cIncl TAG (lcTmpCopy1) OF (lcTmpCopy)
  *B605400,1 AMH [End]
  
  lcCstSLbl  = 'Cost Item'   && Var. hold the cost label in the main screen.
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
  *B603747,1 ABD Filter to Browse only for Manufacturing Styles. [Begin]
  CASE lcStyleTyp = 'T'
    SET FILTER TO Fabric.Make
  *B603747,1 ABD [End]
ENDCASE

*-- Define the browse bar.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcBrCstShT KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcBrCstShT)

*-- Call the screen.
SELECT (lcItemFile)

*E500402,1 AMH Initial color values of Marker field and lable [Start]
lcMarkrLbl = "RGB(192,192,192,192,192,192)"
lcMarkrFld = ",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl"
lcMarkr3DG = lcMarkrLbl
lcMarkr3DW = lcMarkrLbl
STORE SPACE(0) TO lcMarker
*E500402,1 AMH [End]

DO (gcScrDir+"MFCSTSH.SPX")

*-- Release the browse window.
RELEASE WINDOW (lcBrCstShT)
POP MENU _MSYSMENU

*-- Set the excahnge file index ascending again.
IF llMulCurr .AND. lcStyleTyp = 'I'
  SELECT SycExch
  SET ORDER TO TAG Currency ASCENDING
ENDIF

*-- Clear the available filters.
SELECT BOM
SET FILTER TO

SELECT (lcItemFile)
SET FILTER TO

*-- Release the browse window.
RELEASE WINDOW (lcBrCstShT)

*-- If quitting the program erase the temp. files.
IF glQuitting
  *E301411,1 SSH [Begin] Function to clear temp files with parameter
  *E301411,1 SSH         if .T. So, we are going to ZAP all temp files
  *E301411,1 SSH         created for PW modifications Else we will delete.
  IF llPwInst  .AND. lcStyleTyp = 'M'
    =lfPWClear(.T.)
  ENDIF
  *E301411,1 SSH [End]
  IF USED(lcTmpFbric)
    USE IN (lcTmpFbric)
  ENDIF
  ERASE &gcWorkDir.&lcTmpFbric..DBF
  ERASE &gcWorkDir.&lcTmpFbric..CDX
  
  IF USED(lcTmpStyle)
    USE IN (lcTmpStyle)
  ENDIF
  ERASE &gcWorkDir.&lcTmpStyle..DBF
  ERASE &gcWorkDir.&lcTmpStyle..CDX
  
  IF USED(lcTmpCopy)
    USE IN (lcTmpCopy)
  ENDIF
  ERASE &gcWorkDir.&lcTmpCopy..DBF
  ERASE &gcWorkDir.&lcTmpCopy..CDX
  ERASE &gcWorkDir.&lcTmpCopy..FPT
  
  IF USED(lcTmpBom)
    USE IN (lcTmpBom)
  ENDIF
  ERASE &gcWorkDir.&lcTmpBom..DBF
  ERASE &gcWorkDir.&lcTmpBom..CDX
  ERASE &gcWorkDir.&lcTmpBom..FPT
ENDIF

*B602132,1 Reham On 11/29/98  ** Begin **
*B602132,1 Release the Costing pad from the system menu.
RELEASE PAD _OPTION OF _MSYSMENU
*B602132,1 Reham On 11/29/98  ** End **

*!*************************************************************
*! Name      : lfActPad
*! Developer : Reham Al-Allamy
*! Date      : 11/30/1998
*! Purpose   : Bulid a new menu pad [Option] in the when of the
*!           : main screen to call the costing program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*B602132,1 Reham On 11/30/98
*!*************************************************************
*
FUNCTION lfActPad

*-- Define a pad to call the costing program.
DEFINE PAD _OPTION OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _OPTION OF _MSYSMENU (laScrMode[1])
ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP    _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT "\<Costing" SKIP FOR laScrMode[1]
*-- If select the costing bar from the menu, call the costing validation.
ON SELECTION BAR 1 OF _OPTIONPOP DO lfvActCost

*C200303,1 HBG 13/03/2002 call Triger to add new par in option menu 'Amend material costs' [Begin]
IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0 AND lcStyleTyp = 'I'
  =gfDoTriger('POSCTSH',PADR('ADDOPTN',10))
ENDIF     
*C200303,1 [End]

*C200392,1 AMH call Triger to add new par in option menu 'Item Description' [Start]
IF ASCAN(laEvntTrig , PADR('ADOPTDAR',10)) <> 0
  =gfDoTriger('POSCTSH',PADR('ADOPTDAR',10))
ENDIF     
*C200392,1 [End]

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
    laData[1]     = SPACE(lnItemLen)
    lcItemDesc    = " "
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
    *E301411,1 SSH [Begin] .F. ==> Only Delete.
    IF llPwInst  .AND. lcStyleTyp = 'M'
      =lfPWClear(.F.)
    ENDIF
    *E301411,1 SSH [Begin] 
    *-- Call the browse function to blank the browse.
    =lfBrwCstSh()
    *-- Disable the notes & the cost buttons if select mode.
    SHOW GET pbNotes   DISABLE
    SHOW GET pbCosting DISABLE
    
    *B602132,1 Reham 11/29/98  ** Begin **
    *B602132,1 Disable the costing pad in the select mode.
    SET SKIP OF PAD _OPTION OF _MSYSMENU (.T.)
    *B602132,1 Reham 11/29/98  ** End   **
  CASE laScrMode[2]
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
    
    *MAN NY Start Optimize The SQL
    *SELECT *,RECNO() AS nRecNo , "S" AS cStatus ;
      FROM BOM ;
     WHERE cItmMajor = laData[1] .AND. IIF(lcStyleTyp='T' , lMaterial , !lMaterial) ;
      INTO DBF (gcWorkDir+lcTmpBom)
    SELECT *,RECNO() AS nRecNo , "S" AS cStatus ;
      FROM BOM ;
     WHERE citmmajor+typ+citmmask+mfgcode+item+iclr= laData[1] .AND. IIF(lcStyleTyp='T' , lMaterial , !lMaterial) ;
      INTO DBF (gcWorkDir+lcTmpBom)  
    *MAN NY End  
    lnBomLnNo  = _TALLY
    *-- Call the browse function to refresh the browse with right records.
    =lfBrwCstSh()
    SELECT (lcTmpBom)
    INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)
    
    *-- If there is lines for the current cost sheet, get the available 
    *-- Mfg operations from the lines.
    IF lnBomLnNo > 0
      DECLARE laOprCode[1,2]
      laOprCode = ""
      *MAN NY Start Optimize The SQL
      *SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
       WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
             !EMPTY(Codes.cDiscrep) .AND. ;
             Codes.cFld_Name = "MFGCODE" .AND. ;
             Codes.cCode_No  = &lcTmpBom..MfgCode ;
        FROM (lcTmpBom) , Codes ;
        INTO ARRAY laOprCode
        
       SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
       WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
             cdefcode+ccode_no+crltfield+cfld_name="N"+MfgCode+"N"+"MFGCODE" AND;
             !EMPTY(Codes.cDiscrep) ;
        FROM (lcTmpBom) , Codes ;
        INTO ARRAY laOprCode 
      *MAN NY End  
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
      
      *B602132,1 Reham 11/29/98  ** Begin **
      *B602132,1 Disable the costing pad in the select mode.
      SET SKIP OF PAD _OPTION OF _MSYSMENU (.F.)
      *B602132,1 Reham 11/29/98  ** End   **
    ELSE
      cbOnSize   = .F.
      SHOW GET pbDlt DISABLE
      laCtrStat[8]  = "DISABLE"    && Delete button
      *-- Disable the notes & the costing buttons if there is no lines in the cost sheet.
      SHOW GET pbNotes   DISABLE
      SHOW GET pbCosting DISABLE
      lcSrcStat = "DISABLE"
      
      *B602132,1 Reham 11/29/98  ** Begin **
      *B602132,1 Disable the costing pad in the select mode.
      SET SKIP OF PAD _OPTION OF _MSYSMENU (.T.)
      *B602132,1 Reham 11/29/98  ** End   **
    
      *E301411,1 SSH [Begin] .T. ==> ZAP PW Temp Files.
      IF llPwInst  .AND. lcStyleTyp = 'M'
        =lfPwClear(.T.)
      ENDIF
      *E301411,1 SSH [End] 

    ENDIF
    
    *C200392,1 AMH call Triger to Enable the costing bar [Start]
    IF ASCAN(laEvntTrig , PADR('ENBCST',10)) <> 0
      =gfDoTriger('POSCTSH',PADR('ENBCST',10))
    ENDIF     
    *C200392,1 [End]
    
    SHOW GET pbSearch &lcSrcStat
    WAIT CLEAR
    
    *-- Set the select mode flag to false.
    llFrmSelct = .F.
    =lfShowDet()
       
  CASE laScrMode[3]
    IF lcStyleTyp = "T"
      SELECT FABRIC
      lnMainRec = RECNO()
      SET ORDER TO TAG FABRIC   && Select the fabric + color tag.
      llFind = .F.              && Flag to know if there is colors domestic No.
      *-- Search to find if there is at least 1 color for this 
      *-- fabric with domestic NO.
      =SEEK(PADR(laData[1],7))
      LOCATE REST WHILE FABRIC = PADR(laData[1],7) FOR Fabric.Make
      llFind = FOUND()
      SET ORDER TO TAG CFABRIC
      IF lnMainRec > 0 .AND. lnMainRec <= RECCOUNT()
        GOTO lnMainRec
      ENDIF
      
      IF !llFind
        *** Only {manufactured Materials} are allowed here.  ***
        *** <  OK  > ***
        =gfModalGen("INM38015B00000" , "DIALOG" , "manufactured Materials")
        =gfObj_lock(.F.)
        laScrMode    = .F.
        laScrMode[2] = .T.
        SHOW GETS
        RETURN
      ENDIF
    *E301030,1 Reham On 10/01/98  *** Begin ***
    *E301030,1 Change using of the detail costing method to be on the style level.
    ELSE
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
      *E301030,1 Reham On 10/01/98  *** End ***  
    ENDIF
    *-- None of the screen's objects has been updated.
    llCUpDate = .T.
    lcObjStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE" )
    IF lnBomLnNo  = 0 .AND. !llEdit
      llEdit      = .T.
      llNewEntry  = .T.
      
      *** Cost sheet not found.  Do you wish to Add or ***
      *** copy from another {lcItemFile} Cost Sheet ? ***
      *** < Add > - < Copy > - < Cancel > ***

      *E301583,1 AMH Displaying a new message in case of SP module is 
      *E301583,1 AMH installed and manufactured style [Start]
      *lnChoice = gfModalGen("QRM38002B38000" , "DIALOG" , lcItemFile)

      *B604328,1 AMH Adding the imported style.[Start]
      *IF 'SP' $ gcComp_Mdl .AND. lcStyleTyp = 'M'
      *B604408,1 - WAB (Start)- check if the files exist 
      *IF 'SP' $ gcComp_Mdl .AND. lcStyleTyp $ 'MI'
      IF 'SP' $ gcComp_Mdl .AND. lcStyleTyp $ 'MI' AND llOpnPdm
      *B604408,1 - WAB (End)
      *B604328,1 AMH [End]
        *** Cost sheet not found.  Do you wish to Add , ***
        *** copy from another {lcItemFile} Cost Sheet or import ? ***
        *** < Add > - < Copy > - < Import > - < Cancel > ***
        lnChoice = gfModalGen("QRM38205B38027" , "DIALOG" , lcItemFile)
        DO CASE
          *-- If press Import.
          CASE lnChoice = 3
            llImport = lfImportSt()
            lnChoice = 5
          CASE lnChoice = 4
            lnChoice = 3
        ENDCASE
      ELSE
        *** Cost sheet not found.  Do you wish to Add or ***
        *** copy from another {lcItemFile} Cost Sheet ? ***
        *** < Add > - < Copy > - < Cancel > ***
        lnChoice = gfModalGen("QRM38002B38000" , "DIALOG" , lcItemFile)
      ENDIF
      *E301583,1 AMH [End]
      DO CASE
        *-- If press copy.
        CASE lnChoice = 2
          *-- If there is something was copied.
          IF lfCpyCstSht()
            IF lnBomLnNo > 0
              *-- Get the available mfg operations from the bom file.
              DECLARE laOprCode[1,2]
              laOprCode = ""
              
              *MAN NY Start Optimize The SQL
              *SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
               WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
                     !EMPTY(Codes.cDiscrep) .AND. ;
                     Codes.cFld_Name = "MFGCODE" .AND. ;
                     Codes.cCode_No  = &lcTmpBom..MfgCode ;
                FROM (lcTmpBom) , Codes ;
                INTO ARRAY laOprCode
              
              SELECT DISTINCT &lcTmpBom..MfgCode+" - "+Codes.cDiscrep , &lcTmpBom..MfgCode ;
               WHERE !EMPTY(&lcTmpBom..MfgCode) .AND. ;
                     cDefCode+cCode_No+cRltField+cFld_Name="N"+MfgCode+"N"+"MFGCODE" AND;
                     !EMPTY(Codes.cDiscrep) ;
                FROM (lcTmpBom) , Codes ;
                INTO ARRAY laOprCode 
              *MAN NY End

              IF !EMPTY(laOprCode[1,1])
                DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
                =AINS(laOprCode,1)
              ENDIF
              laOprCode[1,1] = "Select Operation"
              laOprCode[1,2] = SPACE(6)
            ENDIF
          ENDIF
        CASE lnChoice = 3
          *-- Unlock the current record in the style file.
          SELECT STYLE
          =gfObj_Lock(.F.)
          laScrMode    = .F.
          laScrMode[2] = .T.
          SHOW GETS
          RETURN
      ENDCASE
      IF lnBomLnNo > 0
        SELECT (lcTmpBom)
        LOCATE FOR &lcTmpBom..lBasOnSiz
        cbOnSize  = IIF(FOUND() , .T. , .F.)
        
        *B602202,1 Reham On 11/16/98   *** Begin ***
        *B602202,1 Define the enabling & disabling of the check box
        *B602202,1 cost sheet by size on the existing of the sizes.
        *B602202,1 If there is at least one cost item (Not style component)
        *B602202,1 have minimum 1 size.
        *lcBasStat = IIF(cbOnSize , "DISABLE" , "ENABLE")
        LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
        llOnSize  = IIF(FOUND() , .T. , .F. )
        lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
        *B602202,1 Reham On 11/16/98   *** End   ***
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

      *B602202,1 Reham On 11/16/98   *** Begin ***
      *B602202,1 Define the enabling & disabling of the check box
      *B602202,1 cost sheet by size on the existing of the sizes.
      *B602202,1 If there is at least one cost item (Not style component)
      *B602202,1 have minimum 1 size.
      *lcBasStat = IIF(cbOnSize , "DISABLE" , "ENABLE")
      LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
      llOnSize  = IIF(FOUND() , .T. , .F. )
      lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
      *B602202,1 Reham On 11/16/98   *** End   ***
      
      GO TOP IN (lcTmpBom)
    ENDIF
    IF lnBomLnNo > 0
      *-- Enable the costing button in the edit or add mode.
      SHOW GET pbCosting ENABLE
      SHOW GET pbNotes   ENABLE
      
      *B602132,1 Reham 11/29/98  ** Begin **
      *B602132,1 Disable the costing pad in the select mode.
      SET SKIP OF PAD _OPTION OF _MSYSMENU (.F.)
      *B602132,1 Reham 11/29/98  ** End   **
    ELSE
      *-- Disable the costing button in the edit or add mode.
      SHOW GET pbCosting DISABLE
      SHOW GET pbNotes   DISABLE
      
      *B602132,1 Reham 11/29/98  ** Begin **
      *B602132,1 Disable the costing pad in the select mode.
      SET SKIP OF PAD _OPTION OF _MSYSMENU (.T.)
      *B602132,1 Reham 11/29/98  ** End   **

      *C200303,1 HBG 13/03/2002 call Triger to add new par in option menu 'Amend material costs' [Begin]
      IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0 AND lcStyleTyp = 'I'
        =gfDoTriger('POSCTSH',PADR('ADDOPTN',10))
      ENDIF     
      *C200303,1 [End]

      *C200392,1 AMH call Triger to enable the option pad [Start]
      IF ASCAN(laEvntTrig , PADR('SKOPTDAR',10)) <> 0
        =gfDoTriger('POSCTSH',PADR('SKOPTDAR',10))
      ENDIF     
      *C200392,1 [End]
      
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
    
    *B802394,1 Reham On 08/03/1999   *** Begin ***
    *B802394,1 Check if there is any changed cost for the style, fabric & trims cost 
    *B802394,1 item in their files.
    IF lnBomLnNo > 0
      =lfChckCost()
    ENDIF
    *B802394,1 Reham On 08/03/1999   *** End   ***
    
    *-- Function to refresh the line info, called from :_
    *-- (show procedure - browse when function)
    =lfRefLin()

    *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [Begin]
    SELECT (lcTmpBom)
    lnI = 1
    SCAN
  	  IF llMulCurr
	    *-- Get purchase price currency, duty currency or base currency
	    lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' , Style.cPriceCur , ;
	                   IIF(&lcTmpBom..cCatgTyp $ 'MD' , Style.cDutyCur , gcBaseCurr)))
	    lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  
	    *-- Get currency unit and exchange rate. 
	    llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
	    lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  
	    *-- Get currency symbol.
	    llFound    = SEEK(lcCurrency , 'SycCurr')
	    lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
	    STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
	    lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
        IF &lcTmpBom..cCatgTyp='P'
          laCostFld[1,1] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))
        ELSE
          lnI = lnI + 1
          *B607902,1 TMI [Start] reset the array dimension
          IF lnI>ALEN(laCostFld,1)
            DIMENSION laCostFld[lnI,2]
          ENDIF
          *B607902,1 TMI [End  ] 
          laCostFld[lnI,1] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))  
        ENDIF
	  ENDIF
    ENDSCAN
    *B128128,1 [End]
        
ENDCASE

SHOW GET cbOnSize   &lcBasStat
SHOW GET lcItemDesc &lcObjStat
SHOW GET pbNewEle   &lcObjStat

SELECT (lcItemFile)

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate Style or Fabric
*!*************************************************************
*! Calls     : FaBrow, gfStyBrow, lfBrwCstSh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvData_1()
*!*************************************************************
*
FUNCTION lfvData_1
PRIVATE llFind

IF (!EMPTY(laData[1]) .OR. llBrowse) .AND. LASTKEY() =13
  SELECT (lcItemFile)
  IF lcStyleTyp = 'T' .AND. (llBrowse .OR. !SEEK(PADR(laData[1],7)))
    *-- If the item is a fabric.
    lcItem = laData[1]
    *B603747,1 ABD Add Browse only for Manufacturing Styles. [Begin]
    *=FaBrow(@lcItem,'*')
    =FaBrow(@lcItem,'*',.T.,[FOR FABRIC.MAKE = .T.])
    *B603747,1 ABD [End]
    laData[1] = lcItem
  ENDIF
  IF lcStyleTyp <> 'T' .AND. (llBrowse .OR. !SEEK(PADR(laData[1],lnItemLen)))
    *-- If the item is a style.
    
    *B607469,1 SSE 08/21/2003 Point record to the nearest style. [Begin]
    *laData[1] = gfStyBrw("M" , "" , "" , .F.)
    IF '?' $ laData[1]
      laData[1] = gfStyBrw("M" , "" , "" , .F.)
    ELSE
      laData[1] = gfStyBrw("M" , laData[1] , "" , .F.)
    ENDIF  
    *B607469,1 SSE 08/21/2003 Point record to the nearest style. [End]
    
    IF !WEXIST(lcBrCstShT)
      =lfBrwCstSh()
    ENDIF
  ENDIF
  
  *-- See if the item with dyelots yes or no.
  llStyDye   = (&lcItemFile..cDye_Flg ='Y')
  
  llBrowse   = .F.

  IF !EMPTY(laData[1]) 
    DO CASE
      CASE lcStyleTyp = 'I' .AND. Style.Make
        *** Only {imported styles} are allowed here.         ***
        *** <  OK  > ***
        =gfModalGen("INM38015B00000" , "DIALOG" , "imported styles")
        *-- Blank the key & its description.
        laData[1]  = SPACE(lnItemLen)
        _CUROBJ    = _CUROBJ
      CASE lcStyleTyp = 'M' .AND. !Style.Make
        *** Only {manufactured styles} are allowed here.     ***
        *** <  OK  > ***
        =gfModalGen("INM38015B00000" , "DIALOG" , "manufactured styles")
        *-- Blank the key & its description.
        laData[1]  = SPACE(lnItemLen)
        _CUROBJ    = _CUROBJ
      *CASE lcStyleTyp = 'I' .AND. llDyelot .AND. llStyDye
      *  *** You cannot create Cost Sheet for imported styles ***
      *  *** which work with dyelots.  Cannot proceed.        ***
      *  *** <  OK  > ***
      *  =gfModalGen("INM38016B00000" , "DIALOG")
      *  *-- Blank the key & its description.
      *  laData[1]  = SPACE(lnItemLen)
      *  _CUROBJ    = _CUROBJ
      *B603747,1 ABD add cheak if make cost sheet for Manufacturing Styles. [Begin]
      CASE lcStyleTyp = 'T'
        IF !lfvData_2()
          _CUROBJ    = _CUROBJ
        ENDIF
      *B603747,1 ABD [End]
      OTHERWISE
        laScrMode    = .F.
        laScrMode[2] = .T.
        SHOW GETS
    ENDCASE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvOnSize
*! Developer : Reham Al-Allamy
*! Date      : 08/28/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 08/28/1997
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
=SEEK(PADR(laData[1],lnMajorLen))
SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(laData[1],lnMajorLen) ;
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
    
    *B606979,1 AMH Get the extended size scale width [Start]
    *lcScale = PADR(Style.Scale,1) + " * " + LOOKUP(Scale.cscl_desc , "S"+Style.Scale , Scale.Scale , "SCALE")
    lcScale = PADR(Style.Scale,3) + " * " + LOOKUP(Scale.cscl_desc , "S"+Style.Scale , Scale.Scale , "SCALE")
    *B606979,1 AMH [End]
    
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
        
        *B606979,1 AMH Get the extended size scale width [Start]
        *lcSize  = PADR(Scale.Scale,1)+" * "+PADR(Scale.cScl_Desc,10)+" * "+Scale.Sz&lcCnt
        lcSize  = PADR(Scale.Scale,3)+" * "+PADR(Scale.cScl_Desc,10)+" * "+Scale.Sz&lcCnt
        *B606979,1 AMH [End]
        
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
        *laMainScal[ALEN(laMainScal,1),1] = SUBSTR(laScale[lnCnt1,3],1,3)+","+Scale.SZ&lcCnt2
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
      DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH -1 , 0
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
      DO lfUpdPerc IN gcAppHome+"MFCstSc.Fxp" WITH &lcTmpBom..cCatgTyp
    ENDIF
    
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to add the current cost to all the manufacutered or duty records 
    *-- that has percentage after updating with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , 0
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
      DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH -1 , 0
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
      DO lfUpdPerc IN gcAppHome+"MFCstSc.Fxp" WITH &lcTmpBom..cCatgTyp
    ENDIF
    
    *-- If the current cost item is price or dutyable cost item, call func.
    *-- to add the current cost to all the manufacutered or duty records 
    *-- that has percentage after updating with the new sizes.
    IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
      DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , 0
    ENDIF
  ENDIF
  
  cbSelSize = IIF(!EMPTY(ALLTRIM(&lcTmpBom..mSizes)) , .T. , .F.)
ENDIF

SHOW GET cbSelSize

*B602202,1 Reham On 11/26/98  ** Begin **
*lcBasStat = IIF(lcBasStat = "ENABLE" , IIF(cbSelSize , "DISABLE" , "ENABLE") , "DISABLE")
*B602202,1 If all the sizes was removed & the current cost item is not style component.
*B602202,1 Check in the BOM file if this was the last item to allow the enabling 
*B602202,1 of the cost sheet by size.
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
*B602202,1 Reham On 11/26/98  ** End   **

SHOW GET cbOnSize  &lcBasStat

SELECT (lcTmpBom)

*!*************************************************************
*! Name      : lfCpyCstSht
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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

*B801908,1 Reham On 01/28/99   *** Begin ***
*B801908,1 Variable hold the style primary fabric for the "copy from style" 
*B801908,1 to be saved for the current style cost sheet.
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
*! Name      : lfBrwCstSh
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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

IF llNonMjExt .OR. lcStyleTyp = "T"
  *-- If there is non-major exist.
  
  

  *B603348,1  ABD Increase the UntCost Filed to 9999999.999  & TotCost 9999999999.99. [ Begin ]
  *lcCstShFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
               "lcMask  = IIF(lcStyleTyp='T',cItmMask,SUBSTR(cItmMask,lnMajorLen+LEN(ALLTRIM(lcSeprator))+1)) :H= lcNonMjHdr :R:18 ,"+;
               "lcDesc  = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
               "Item       :H= 'Item'     :R:20 ,"+;
               "IClr       :H= 'I. Clr'   :R:9  ,"+;
               "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
               "UOM        :H= 'UOM' :R:6 ,"+;
               "nBomTotQty :H= 'Unt Qty' :P='999.999' :R:7 ,"+;
               "UntCost    :H= 'Unt Cst' :P='999.999' :R:7 ,"+;
               "TotCost    :H= 'Tot. Cst' :P='999999.99':R:9"
  *E500402,1 AMH Add marker field [Start]
  *lcCstShFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
               "lcMask  = IIF(lcStyleTyp='T',cItmMask,SUBSTR(cItmMask,lnMajorLen+LEN(ALLTRIM(lcSeprator))+1)) :H= lcNonMjHdr :R:18 ,"+;
               "lcDesc  = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
               "Item       :H= 'Item'     :R:20 ,"+;
               "IClr       :H= 'I. Clr'   :R:9  ,"+;
               "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
               "UOM        :H= 'UOM' :R:6 ,"+;
               "nBomTotQty :H= 'Unt Qty'  :P='999.999' :R:7 ,"+;
               "UntCost    :H= 'Unt Cst'  :P='999999999.999' :R:13 ,"+;
               "TotCost    :H= 'Tot. Cst' :P='9999999999.99':R:13"
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
  *E500402,1 AMH [End]
  *B603348,1  ABD [ END ]
ELSE
  *-- If there is no non-major exist in the style.
  *B603348,1  ABD Increase the untCost Filed to 9999999.999  & TotCost 9999999999.99. [ Begin ]
  *lcCstShFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
             "lcDesc    = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
             "Item       :H= 'Item'   :R:20  ,"+;
             "IClr       :H= 'I. Clr' :R:9   ,"+;
             "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
             "Desc       :H= 'Description' :R:16 ,"+;
             "UOM        :H= 'UOM' :R:6 ,"+;
             "nBomTotQty :H= 'Unt Qty' :P='999.999' :R:7 ,"+;
             "UntCost :H= 'Unt Cst' :P='999.999' :R:7 ,"+;
             "TotCost :H= 'Tot. Cst':P='999999.99':R:9"
  *E500402,1 AMH Add marker field [Start]
  lcCstShFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
             "lcDesc    = IIF(!EMPTY(Typ),EVALUATE('lc'+lcStyleTyp+'SLbl'+Typ),''):H='Type':R:12,"+;
             "Item       :H= 'Item'   :R:20  ,"+;
             "IClr       :H= 'I. Clr' :R:9   ,"+;
             "lcMFGDesc = gfCodDes(MFGCODE , 'MFGCODE'):H= 'MFG Code' :R:13 ,"+;
             "Desc       :H= 'Description' :R:16 ,"+;
             "UOM        :H= 'UOM' :R:6 ,"+;
             "nBomTotQty :H= 'Unt Qty' :P='999.999' :R:7 ,"+;
             "UntCost :H= 'Unt Cst' :P='999999999.999':R:13 ,"+;
             "TotCost :H= 'Tot. Cst':P='9999999999.99':R:13"
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
  *E500402,1 AMH [End]
  *B603348,1  ABD [ END ]             
  
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
       IN WINDOW (gcBaseWind) ;
       TITLE lcBrCstShT

=lfShowDet()

SELECT (lcItemFile)

*!*************************************************************
*! Name      : lfShowDet
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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

lnMarkB    = RECNO(lcTmpBom)
glFromBrow = .T.

lcCstSLbl  = IIF(EMPTY(&lcTmpBom..Typ) .OR. lnBomLnNo = 0 , 'Cost Item' , EVALUATE('lc'+lcStyleTyp+'SLbl'+&lcTmpBom..Typ))

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

*B801924,1 Reham On 02/01/99   *** Begin ***
*B801924,1 Enable the costing button in add mode.
IF laScrMode[1]
  SHOW GET pbCosting DISABLE
ELSE
  IF lnBomLnNo = 0
    SHOW GET pbCosting DISABLE
  ELSE
    SHOW GET pbCosting ENABLE
  ENDIF
ENDIF
*B801924,1 Reham On 02/01/99   *** End   ***

*-- Define all the needed objects with the right values according to the current record.
lcUom       = IIF(EMPTY(&lcTmpBom..UOM) .OR. lnBomLnNo = 0 , "" , &lcTmpBom..UOM)
lnEstQtyA   = IIF(EMPTY(&lcTmpBom..nEstBomQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nEstBomQty)
lnEstQtyB   = IIF(EMPTY(&lcTmpBom..nEstBomQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nEstBomQty)
lnUnitQtyA  = IIF(EMPTY(&lcTmpBom..nBomTotQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomTotQty)
lnUnitQtyB  = IIF(EMPTY(&lcTmpBom..nBomTotQty) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomTotQty)
lnWstg      = IIF(EMPTY(&lcTmpBom..nBomWastge) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nBomWastge)
lnUnitCst   = IIF(EMPTY(&lcTmpBom..UntCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..UntCost)
lnTotFCst   = IIF(EMPTY(&lcTmpBom..TotCost) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..TotCost)
lnCstPerc   = IIF(EMPTY(&lcTmpBom..nPercent) .OR. lnBomLnNo = 0 , 0 , &lcTmpBom..nPercent)
lcOprCode   = IIF(EMPTY(&lcTmpBom..cOprCode) .OR. lnBomLnNo = 0 , "" , &lcTmpBom..cOprCode)
puOprCode   = IIF(EMPTY(lcOprCode) .OR. lnBomLnNo = 0 , 1 , IIF(ASCAN('laOprCode',lcOprCode) > 0 , ;
                 ASUBSCRIPT('laOprCode' , ASCAN('laOprCode',lcOprCode) , 1) , 1))
cbSelSize   = IIF(EMPTY(ALLTRIM(&lcTmpBom..mSizes)) .OR. lnBomLnNo = 0 , .F. , .T.)
lcDutyable  = IIF(EMPTY(&lcTmpBom..cCostStat) .OR. lnBomLnNo = 0 , " " , &lcTmpBom..cCostStat)
ibDutyabl   = IIF(EMPTY(&lcTmpBom..cCostStat) .OR. lnBomLnNo = 0 , 1 , VAL(&lcTmpBom..cCostStat)+1)
*E500402,1 AMH Updating the marker field data according to the current record [Start]
lcMarker    = &lcTmpBom..cMarker
*E500402,1 AMH [End]
IF llMulCurr
  *-- Get purchase price currency, duty currency or base currency
  lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' , Style.cPriceCur , ;
                   IIF(&lcTmpBom..cCatgTyp $ 'MD' , Style.cDutyCur , gcBaseCurr)))
  lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  
  *-- Get currency unit and exchange rate. 
  llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
  lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  
  *-- Get currency symbol.
  llFound    = SEEK(lcCurrency , 'SycCurr')
  lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
  STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
  lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
  
  lnTotEquCst = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))
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

*E500402,1 AMH Refreshing the color values of Marker Field and lable [Start]
lcMarkrLbl = IIF(&lcTmpBom..cCatgTyp='F',"RGB(0,0,,,,,)","RGB(192,192,192,192,192,192)")
lcMarkrFld = IIF(&lcTmpBom..cCatgTyp='F',gcObjColor,",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl")
lcMarkr3DG = IIF(&lcTmpBom..cCatgTyp='F',"RGB(128,128,128,128,128,128)",lcMarkrLbl)
lcMarkr3DW = IIF(&lcTmpBom..cCatgTyp='F',"RGB(255,255,255,192,192,192)",lcMarkrLbl)
*E500402,1 AMH [End]

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
*! Developer : Reham Al-Allamy
*! Date      : 09/07/1997
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
    *C200080,6  Reham On 08/11/1999   *** Begin ***
    *C200080,6  Enable the unit qty. & wastage if the the cost item is MFG.
    *SHOW GET lnEstQtyA  DISABLE
    *SHOW GET lnUnitQtyA DISABLE
    *SHOW GET lnWstg     DISABLE
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    *C200080,6  Reham On 08/11/1999   *** End   ***
    SHOW GET lnUnitCst  &lcLinStat
    SHOW GET lcUom      &lcLinStat
    *E301411,1 SSH [Begin] PW Function to Enabling and Desabling
    *E301411,1 SSH         Detail Operaiton Button.
    IF llPwInst  .AND. lcStyleTyp = 'M'
      =lfPwEnDes()
    ENDIF
    *E301411,1 SSH [Begin] 
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
    
    *E301842,1 AMH Enable all objects [Start]
    SHOW GET lcCostItmF &lcLinStat
    SHOW GET lcIClr     &lcLinStat
    SHOW GET ibFabric   &lcLinStat
    SHOW GET ibColor    &lcLinStat
    *E301842,1 AMH [End]
    
    SHOW GET lcCstItmDF &lcLinStat
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    SHOW GET lnUnitCst  DISABLE
    SHOW GET lcUom      DISABLE
  *-- If the cost item is trim.
  CASE &lcTmpBom..cCatgTyp = 'T'
    SHOW WINDOW (lcCstSh3) TOP
    SHOW GET lcCstItmDF &lcLinStat
    SHOW GETS WINDOW (lcCstSh4) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh5) DISABLE ONLY
    SHOW GETS WINDOW (lcCstSh6) DISABLE ONLY
    
    *E301842,1 AMH Enable all objects [Start]
    SHOW GET lcCostItmF &lcLinStat
    SHOW GET lcIClr     &lcLinStat
    SHOW GET ibFabric   &lcLinStat
    SHOW GET ibColor    &lcLinStat
    *E301842,1 AMH [End]
    
    SHOW GET lnEstQtyB  DISABLE
    SHOW GET lnEstQtyA  &lcLinStat
    SHOW GET lnUnitQtyB DISABLE
    SHOW GET lnUnitQtyA &lcLinStat
    SHOW GET lnWstg     &lcLinStat
    *B602395,1 Reham On 01/04/99   *** Begin ***
    *B602395,1 Disable the unit cost & Uom fields if the trim from file.
    IF &lcTmpBom..trim_invt
      SHOW GET lnUnitCst  DISABLE
      SHOW GET lcUom      DISABLE
    ELSE
      SHOW GET lnUnitCst  &lcLinStat
      SHOW GET lcUom      &lcLinStat
    ENDIF
    *B602395,1 Reham On 01/04/99   *** Begin ***
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
IF lcStyleTyp = 'I' .AND. lnCstPerc > 0 .AND. ;
  (lcPricCur = lcDutyCur) .AND. &lcTmpBom..cCatgTyp $ 'MD'
  SHOW GET lnCstPerc &lcLinStat
ELSE
  SHOW GET lnCstPerc DISABLE
ENDIF

*-- Disable the duty popup in case of duty cost item.
IF &lcTmpBom..cCatgTyp = 'D' .OR. ;
  (&lcTmpBom..cCatgTyp = 'S' .AND. ;
   PADR(&lcTmpBom..Item,lnMajorLen) = PADR(laData[1],lnMajorLen))
  SHOW GET ibDutyabl  DISABLE
ELSE
  SHOW GET ibDutyabl  &lcLinStat
ENDIF

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Reham Al-Allamy
*! Date      : 08/14/1997
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
*! Name      : lfReadAct
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : READ Activate function of the main screen.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfReadAct()
*!*************************************************************
*
FUNCTION lfReadAct

*-- Clear browse as you are coming out of a browse window
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP() , lcBrCstShT) 
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
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 07/20/1997
*! Purpose   : Deactivate function for the base screen.
*!*************************************************************
*! Calls     : lpTab, lpShftTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

glFromBrow = WONTOP() = lcBrCstShT 

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShftTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 02/04/1998
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab

ON KEY LABEL TAB
*-- Point to the current object from the browse if press tab from the browse.
ACTIVATE WINDOW (lcCstSh2)
_CUROBJ = OBJNUM(pbNewEle)
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShftTab
*! Developer : Reham Al-Allamy
*! Date      : 01/08/1998
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShftTab
*!*************************************************************
*
PROCEDURE lpShftTab

ON KEY LABEL BACKTAB

*-- Point to the current object from the browse if press 
*-- Shift + tab from the browse.
ACTIVATE WINDOW (lcCstSh0)
_CUROBJ = OBJNUM(ibTab1)
    
ON KEY LABEL BACKTAB DO lpShftTab

*!*************************************************************
*! Name      : lfvNewEle
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
DO (gcAppHome+"MFCSTSC") WITH lcStyleTyp , laData[1] , lcTmpBom , ;
                              lcTmpFbric , lcTmpStyle , IIF(cbOnSize , "Y" , "N")

POP KEY

*B602202,1 Reham On 11/16/98   *** Begin ***
*B602202,1 Define the enabling & disabling of the check box
*B602202,1 cost sheet by size on the existing of the sizes.
*B602202,1 If there is at least one cost item (Not style component)
*B602202,1 have minimum 1 size.
SELECT (lcTmpBom)
LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
llOnSize  = IIF(FOUND() , .T. , .F. )
lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE" )
SHOW GET cbOnSize &lcBasStat
*B602202,1 Reham On 11/16/98   *** End   ***

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

_CUROBJ = OBJNUM(ibTab2)

*!*************************************************************
*! Name      : lfvRemEle
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
    DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH -1 , 0
  ENDIF
  
  *-- Save the current parent mask of the deleted record.
  lcDelMask = &lcTmpBom..cItmMask
  
  *B602202,1 Reham On 11/16/98  ** Begin **
  *B602202,1 If the current removed cost item (not a style component type)
  *B602202,1 and has sizes, set flag to find if it was the last cost item
  *B602202,1 that has sizes or not.
  llEnblSize = IIF(!EMPTY(mSizes) OR cCatgTyp <> "S" , .T. , .F.)
  *B602202,1 Reham On 11/16/98  ** End   **
  
  *E301411,1 SSH [Begin] PW Function to Remove all Detail operation
  *E301411,1 SSH         Assigned to the Mfg Code That you are removing.
  IF llPwInst .AND. lcStyleTyp = 'M'
    =lfPwRem()
  ENDIF
  *E301411,1 SSH [End] 
  *-- Change the status to be deleted & delete the record.
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  *-- If style cost sheet only...
  IF lcStyleTyp <> "T"
    *-- Call local function to calculate the current parent cost in its
    *-- parent records...
    DO lfPrntCost IN gcAppHome+"MFCstSc.FXP" WITH lcDelMask
  ENDIF
  
  *B602202,1 Reham On 11/16/98  ** Begin **
  *B602202,1 If the current removed cost item is not a style component type
  *B602202,1 and has sizes, Find if it was the last cost item that has sizes
  *B602202,1 or not, if it was disable the check box "Cost sheet by size".
  IF llEnblSize
    *lnCurRec  = RECNO(lcTmpBom)
    LOCATE FOR !EMPTY(mSizes) AND cCatgTyp <> "S"
    llOnSize  = IIF(FOUND() , .T. , .F. )
    lcBasStat = IIF(llOnSize , "DISABLE" , "ENABLE")
    SHOW GET cbOnSize &lcBasStat
    *-- Restore the record pointer in the bom file.
    *IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
    *  GOTO lnCurRec
    *ENDIF
  ENDIF
  GO TOP
  *B602202,1 Reham On 11/16/98  ** End   **
  
  *-- Subtract 1 from the bom records no.
  lnBomLnNo = lnBomLnNo - 1
  *-- Update the enabling & disabling variables in case there is 
  *-- no lines in the browse at all.
  lcLinStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
  lcSrcStat = IIF(!laScrMode[1] .AND. lnBomLnNo > 0 , "ENABLE" , "DISABLE" )
  *-- Call local function to refresh the objects in the main screen 
  *-- according to the current cost items.
  =lfShowDet()
ENDIF
_CUROBJ = OBJNUM(ibTab2)

*!*************************************************************
*! Name      : lfvSearch
*! Developer : Reham Al-Allamy
*! Date      : 01/21/1998
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
    *B602293,1 Reham On 12/01/98   *** Begin ***
    *B602293,1 Point the record poistion in the temp. BOM file.
    *GOTO lnSrchRec
    GOTO lnSrchRec IN (lcTmpBom)
    *B602293,1 Reham On 12/01/98   *** End   ***
  ENDIF
ENDIF

*-- Refresh the browse & its objects in the main screen.
=lfShowDet()
_CUROBJ = OBJNUM(ibTab2)

*!*************************************************************
*! Name      : lfvCstItmDs
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 08/20/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 11/12/1997
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

lcDutyable = laDutyable[ibDutyabl,2]

*-- Update the temp. bom file.
REPLACE &lcTmpBom..cCostStat WITH lcDutyable ;
        &lcTmpBom..cStatus   WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- Update the Duty cost items with all the dutyable cost items.
*B803553,1 ABD- Fix bug that miscalculate the duty charges on the style BOM
*B803553,1 ABD- & Fix bug in the lfUpdcost also. [Begin]
*IF lcDutyable = "1" .OR. lcOldDuty = "1" .AND. !(lcDutyable == lcOldDuty)
IF (lcDutyable = "1" .OR. lcOldDuty = "1") .AND. !(lcDutyable == lcOldDuty)
  *B803553,1 ABD [End]
  DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH IIF(lcDutyable = "1",+1 ,-1) , 0
ENDIF

*!*************************************************************
*! Name      : lfvUnitQty
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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

*RAM
*lnTotEquCst = lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit
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
  DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , lnOld
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
*! Developer : Reham Al-Allamy
*! Date      : 08/20/1998
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

*RAM
*lnTotEquCst = lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit
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
  DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , lnOld
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
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
  IF lcStyleTyp = 'I' .AND. &lcTmpBom..cCatgTyp $ 'MD' .AND. (lcPricCur = lcDutyCur)
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

*RAM
*lnTotEquCst = lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit  
lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,2)

*-- Save the old total cost before updating with the new one.
lnOld = &lcTmpBom..TotCost

*-- Update unit cost and total cost.
REPLACE &lcTmpBom..nPercent   WITH lnCstPerc ;
        &lcTmpBom..UntCost    WITH lnUnitCst ;
        &lcTmpBom..TotCost    WITH lnTotFCst ;
        &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" , "M" , &lcTmpBom..cStatus)

*-- If the current cost item is price call function to update
*-- all the manufacutered records that has percentage.
IF (&lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1') ;
   .AND. !(lnOld == &lcTmpBom..TotCost)
  DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , lnOld
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
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
  DO lfUpdPerc IN gcAppHome+"MFCstSc.Fxp" WITH lcType
ENDIF

*-- Recalculate total and equevlent cost
lnUnitCst   = &lcTmpBom..UntCost
lnTotFCst   = &lcTmpBom..TotCost

*RAM
*lnTotEquCst = lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit
lnTotEquCst = ROUND(lnTotFCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2)

*-- Refresh the related objects.
*B604802,1 AMH 08/19/2001 (Begin) Changing the show get command because it .
*B604802,1                causes a syntax error.
*SHOW GET lnUnitCst DISABLE
SHOW GET lnUnitCst 
*B604813,1 AMH 08/19/2001 (End)

SHOW GET lnTotFCst
SHOW GET lnTotEquCst
*-- Refresh the main browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Delete Cost Sheet
*!*************************************************************
*! Calls     : gfTmp2Mast
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
*
FUNCTION lpDelScr

*-- Change all the records status with "D-> Deleted"
SELECT (lcTmpBom)
REPLACE ALL &lcTmpBom..cStatus WITH "D"
*E301411,1 SSH [Begin] PW Function to Delete all Detail Operation assigend
*E301411,1 SSH         to Mfg Code in the currernt style cost sheet.
*B603872,1 SSH 03/09/2000 [Start] Commented out.
*= lfPwDel()
llDummy = llPwInst .AND. lcStyleTyp = 'M' .AND. lfPwDel()
*E301411,1 SSH [End] 
*-- Delete all records.
DELETE ALL

*-- Call global function to delete the style/fabric cost sheet items.
=gfTmp2Mast('BOM' , lcTmpBom , 'Deleting cost sheet items for : ' + laData[1] + ' ...')

*-- Refresh the browse.
SHOW WINDOW (lcBrCstShT) REFRESH SAME

*-- Blank the mfg operations array.
DECLARE laOprCode[1,2]
laOprCode[1,1] = "Select Operation"
laOprCode[1,2] = SPACE(6)

*-- If material cost sheet.
IF lcStyleTyp = 'T'
  WAIT 'Updating the costing information to the material file...' WINDOW NOWAIT
  SELECT FABRIC
  *-- Save the current record pointer in the fabric file.
  lnFbrRec = RECNO("FABRIC")
  SET ORDER TO TAG FABRIC IN FABRIC
  =SEEK(PADR(laData[1],7))
  *-- Zero out fabric cost item fields 
  SCAN REST WHILE Fabric = laData[1] FOR Fabric.Make
    REPLACE nMCost1 WITH 0 , nMCost2 WITH 0 ,;
            nMCost3 WITH 0 , nMCost4 WITH 0 ,;
            CostBuy WITH 0 , CostUse WITH 0
  ENDSCAN
  *-- Restore fabric file index.
  SET ORDER TO TAG CFABRIC IN FABRIC
  *-- Restore the record pointer in the fabric file.
  IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
    GOTO lnFbrRec IN FABRIC
  ENDIF
ELSE
  WAIT 'Updating the costing information to the style file...' WINDOW NOWAIT
  SELECT STYLE
  *-- Save the record pointer in the style file.
  lnStyRec = RECNO("STYLE")
  SET ORDER TO TAG STYLE  IN STYLE

  =SEEK(PADR(laData[1],lnMajorLen))
  *-- Zero out Style cost item fields.
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(laData[1],lnMajorLen)
    REPLACE ('n'+lcStyleTyp+'Cost1')  WITH 0 ;
            ('n'+lcStyleTyp+'Cost2')  WITH 0 ;
            ('n'+lcStyleTyp+'Cost3')  WITH 0 ;
            ('n'+lcStyleTyp+'Cost4')  WITH 0 ;
            ('n'+lcStyleTyp+'Cost5')  WITH 0 ;
            TotCost                   WITH 0
  ENDSCAN
  *-- Restore the style index.
  SET ORDER TO TAG CSTYLE IN STYLE
  *-- Restore the style record pointer.
  IF lnStyRec > 0 .AND. lnStyRec < RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ELSE
    =SEEK(ALLTRIM(laData[1]),'STYLE')   
  ENDIF
ENDIF  
WAIT CLEAR
SELECT (lcItemFile)

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
GOTO TOP
*-- If the BOM file is empty.
IF EOF()
  IF llNewEntry
    *** Nothing updated to the Cost Sheet. ***
    *** <  Ok  > ***
    =gfModalGen("INM38053B00000" , "DIALOG")
  ELSE
    *** This {lcItemFile} cost Sheet will be deleted. ***
    *** <  Ok  > ***
    =gfModalGen("INM38008B00000" , "DIALOG" , lcItemFile)
    DO lpDelScr
    *-- Call global function to delete the style/fabric cost sheet items.
    *=gfTmp2Mast('BOM' , lcTmpBom , 'Deleting cost sheet items for : ' + laData[1] + ' ...')
  ENDIF
ELSE
  WAIT 'Updating the costing information to the style file' WINDOW NOWAIT
  lnFbrRec = RECNO("FABRIC")     && Save record pointer in the fabric file.
  lnStyRec = RECNO("STYLE")      && Save record pointer in the style file.
  
  *-- Clear the filter in the current item file (Style or fabric).
  SELECT (lcItemFile)
  SET FILTER TO
  
  *B038753,1 WSH If Cost Method is Standard, Update Inventory Journal with new Value {Start]
  IF IIF(lcStyleTyp = 'T', ALLTRIM(gfGetMemVar('M_MATCSTMT')), ALLTRIM(gfGetMemVar('M_Cost_Met'))) = 'S'
    IF !lfCostInfo(.T.)
      llCSave = .F.
      RETURN
    ENDIF
  ENDIF
  *B038753,1 WSH [End]
  
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
  
  *-- Check if any styles to be add to the master style file.
  SET ORDER TO STYLE IN STYLE
  *-- Save the record pointer in the style file.
  lnStyRec = RECNO("STYLE")
  *-- If the style was not found, add it to the style.
  SELECT (lcTmpStyle)
  SCAN
    IF !SEEK(Style , 'Style')
      SCATTER MEMVAR
      SELECT Style
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
  ENDSCAN
  *-- Blank the temp. style file.
  ZAP
  *-- Restore the index in the style file.
  SET ORDER TO CSTYLE IN STYLE
  *-- Restore the record pointer in the style file.
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF

  *E301583,1 AMH Delete the imported records from PDMBOM file 
  *E301583,1 AMH and update PDMLOG file [Start]
  IF llImport
    =gfOpenFile(gcDataDir+"PDMBOM" , gcDataDir+"PDMBOM" , "SH")
    SELECT PDMBOM
    IF SEEK(laData[1])
      DELETE REST WHILE cItmMajor = laData[1]
    ENDIF
    =gfCloseFile('PDMBOM')
    =gfOpenFile(gcDataDir+"PDMLOG" , gcDataDir+"PDMITEMCD" , "SH")
    SELECT PDMLOG
    IF SEEK('PROTOTYPE'+laData[1])
      REPLACE CBOMSTATUS WITH 'C'
    ENDIF
    =gfCloseFile('PDMLOG')
    llImport = .F.
  ENDIF
  *E301583,1 AMH [End]
  
  *-- Update BOM master file
  *-- Call global function to delete the style/fabric cost sheet items.
  *E301411,1 SSH [Begin] PW Function To Save Detail Operation.
  IF llPwInst .AND. lcStyleTyp = 'M'
    =lfPwSav()
  ENDIF
  *E301411,1 SSH [End]
  =gfTmp2Mast('BOM' , lcTmpBom , 'Saving cost sheet items for : ' + laData[1] + ' ...')

  SELECT (lcItemFile)
  DO CASE
    CASE lcStyleTyp = 'I'
      SET FILTER TO !Style.Make
    CASE lcStyleTyp = 'M'
      SET FILTER TO Style.Make
  ENDCASE
ENDIF

SELECT (lcItemFile)

*!*************************************************************
*! Name      : lfCostInfo
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
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

*B038753,1 WSH Add a parameter to check if update in Master or Temp Item file [Start]
PARAMETERS llUpdInTmp
*B038753,1 WSH End [Start]

PRIVATE lnItemRec , lnStyRec , lnFbrRec , lnTmpRec

*B038753,1 WSH Create a temp file to update in [Start]
IF llUpdInTmp
  PRIVATE lcColorFil
  lcColorFil   = gfTempName()
  llContinSave = .T.
ENDIF
*B038753,1 WSH [End]

*-- Save the record pointer in th style file.
SET ORDER TO TAG STYLE  IN STYLE
lnStyRec = RECNO("STYLE")

*-- Save the record pointer in the fabric file.
SET ORDER TO TAG FABRIC IN FABRIC
lnFbrRec = RECNO("FABRIC")

*-- Compute cost item elements.
*-- If copute for material cost sheet.
IF lcStyleTyp = "T"

  *B038753,1 WSH Fill temp file with needed Data [Start]
  IF llUpdInTmp
    SELECT *, Fabric+Color AS Style ;
     FROM  Fabric ;
     WHERE Fabric+Color = PADR(laData[1],7) AND Fabric.Make ;
     INTO  TABLE (gcWorkDir+lcColorfil)

    SELECT (lcColorFil)
    INDEX ON Fabric+Color TAG (lcColorFil)
    SET ORDER TO (lcColorFil)
  ENDIF
  *B038753,1 WSH [End]

  *-- Search for all the manufactered fabric colors to calculate its cost.
  SELECT FABRIC
  =SEEK(laData[1],7)
  SCAN REST WHILE PADR(FABRIC,7) = PADR(laData[1],7) FOR FABRIC.Make
    lnItemRec = RECNO()        && Save the record pointer.
    lcFbrClr  = FABRIC.Color   && Save the current color.
    
    *-- Blank all the cost variables.
    STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4
    
    *-- Scan in the temp. bom file to calculate the current fabric color cost.
    SELECT (lcTmpBom)
    *B603309,1  Reham On 12/04/1999   *** Begin ***
    *B603309,1  Fix the bugs upon updating the material cost in the save procedure.
    *SCAN
    SCAN FOR LIKE(STRTRAN(SUBSTR(&lcTmpBom..cItmMask,1,6),'*','?') , FABRIC.Color)
    *B603309,1  Reham On 12/04/1999   *** End   ***
      lcCurVar = 'lnCost'+&lcTmpBom..Typ
      DO CASE
        *-- If the cost item is fabric or trim & trim inventory.
        CASE (cCatGTyp='F') .OR. (cCatGTyp='T' .AND. Trim_Invt)
          *-- Get the current fabric color from the temp. bom file.
          lcFbrClr = IIF("*" $ IClr , FABRIC.Color , IClr)
          *-- Save the current record pointer.
          lnTmpRec = RECNO("FABRIC")
          *-- Seek for the current fabric + color in the fabric file.
          IF SEEK(SUBSTR(Item,1,7) + lcFbrClr , 'Fabric')
            *-- Calculate the cost for the current cost item.
            
            *B604813,1 AMH Round before add [Start]
            *&lcCurVar = &lcCurVar + (nBomTotQty * Fabric.CostBuy/Fabric.Conv)
            &lcCurVar = &lcCurVar + ROUND((nBomTotQty * Fabric.CostBuy/Fabric.Conv),2)
            *B604813,1 AMH [End]
            
          *B603309,1 Reham On 12/06/1999     *** Begin ***
          ELSE
            IF SEEK(SUBSTR(Item,1,7) + lcFbrClr , lcTmpFbric)
              *-- Calculate the cost for the current cost item.
              
              *B604813,1 AMH Round before add [Start]
              *&lcCurVar = &lcCurVar + (nBomTotQty * &lcTmpFbric..CostBuy/&lcTmpFbric..Conv)
              &lcCurVar = &lcCurVar + ROUND((nBomTotQty * &lcTmpFbric..CostBuy/&lcTmpFbric..Conv),2)
              *B604813,1 AMH [End]
              
            ENDIF
            *B603309,1 Reham On 12/06/1999     *** End   ***
          ENDIF
          *-- Restore the record pointer.
          IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("FABRIC")
            GOTO lnTmpRec IN FABRIC
          ENDIF
        *-- If the cost item is trim & not trim inventory or 
        *-- Price, Duty or Mfg operation.
        CASE (cCatGTyp = 'T' .AND. !Trim_Invt) .OR. cCatGTyp $ 'MDP'
          *-- Calculate the cost for the current cost item.
          
          *B604813,1 AMH Round before add [Start]
          *&lcCurVar = &lcCurVar + TotCost
          &lcCurVar = &lcCurVar + ROUND(TotCost,2)
          *B604813,1 AMH [End]
          
        *-- If the cost item is style component.
        CASE cCatGTyp = 'S' .AND. llColorExt
          IF '*' $ SUBSTR(Item , lnColorStr , lnColorLen)
            lcItmFbr  = PADR(STUFF(Item , lnColorStr , lnColorLen , lcFbrClr),19)
          ELSE
            lcItmFbr  = &lcTmpBom..Item
          ENDIF
          *-- Save the current record pointer in the style file.
          lnTmpRec = RECNO("STYLE")
          *-- Seek for the current style + color in the style file.
          SELECT STYLE
          =SEEK(PADR(lcItmFbr,lnMajorLen) , "STYLE")
          LOCATE FOR LIKE(STRTRAN(lcItmFbr , "*" , "?") , STYLE.Style)
          *-- Calculate the cost for the current cost item.
          IF FOUND()
            
            *B604813,1 AMH Round before add [Start]
            *&lcCurVar = &lcCurVar + &lcTmpBom..nBomTotQty * Style.TotCost
            &lcCurVar = &lcCurVar + ROUND(&lcTmpBom..nBomTotQty * Style.TotCost,2)
            *B604813,1 AMH [End]
            
          ENDIF
          *-- Restore the record pointer in the style file.
          IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("STYLE")
            GOTO lnTmpRec IN STYLE
          ENDIF
          SELECT (lcTmpBom)
      ENDCASE
      SELECT (lcTmpBom)
    ENDSCAN
    *-- Restore record pointer in the fabric file.
    SELECT FABRIC
    IF lnItemRec > 0 .and. lnItemRec <= RECCOUNT("FABRIC")
      GOTO lnItemRec
    ENDIF
    
    *-- Round The cost updated in the style file.
    *-- Update Fabric cost elements
    *REPLACE nMCost1 WITH lnCost1 ;
            nMCost2 WITH lnCost2 ;
            nMCost3 WITH lnCost3 ;
            nMCost4 WITH lnCost4 ;
            CostBuy WITH nMCost1 + nMCost2 + nMCost3 + nMCost4 ;
            CostUse WITH CostBuy/Conv

    *B602895,1 Reham On 05/19/1999   *** Begin ***
    *B602895,1 Update the total cost with rounded cost items instead of rounding their totals.
    *-- Update Fabric cost elements
    *REPLACE nMCost1 WITH ROUND(lnCost1 , 2) ;
            nMCost2 WITH ROUND(lnCost2 , 2) ;
            nMCost3 WITH ROUND(lnCost3 , 2) ;
            nMCost4 WITH ROUND(lnCost4 , 2) ;
            CostBuy WITH ROUND((nMCost1 + nMCost2 + nMCost3 + nMCost4) , 2) ;
            CostUse WITH CostBuy/Conv

    *B038753,1 WSH Select temp file to update in [Start]
    IF llUpdInTmp
      SELECT (lcCOlorFil)
      =SEEK(FABRIC.Fabric+FABRIC.Color)
    ENDIF
    *B038753,1 WSH [End]

    REPLACE nMCost1 WITH ROUND(lnCost1 , 2) ;
            nMCost2 WITH ROUND(lnCost2 , 2) ;
            nMCost3 WITH ROUND(lnCost3 , 2) ;
            nMCost4 WITH ROUND(lnCost4 , 2) ;
            CostBuy WITH ROUND(lnCost1,2) + ROUND(lnCost2,2) + ROUND(lnCost3,2) + ROUND(lnCost4,2) ;
            CostUse WITH CostBuy/Conv
    *B602895,1 Reham On 05/19/1999   *** End   ***
    
    *B038753,1 WSH If we update temp, Don't update working files before updating Stock value [Start]
    IF !llUpdInTmp
    *B038753,1 WSH [End]
    
    *E301077,16 Reham On 12/30/98   *** Begin ***
    *E301077,16 Open the company file.
    =gfOpenFile(gcDataDir+'MatInvJl',gcDataDir+'MatInvJl','SH')
    SELECT FABRIC
    *E301077,16 Reham On 12/30/98   *** End   ***
        
    *-- Update Fabric Average cost in the material inventory journal file.
    IF !SEEK(FABRIC.Fabric+FABRIC.Color , 'MatInvJl')
      REPLACE nAveCstBuy WITH CostBuy ;
              nFAve_Cost WITH CostUse
      
      *E301077,16 Reham On 12/30/98   *** Begin ***
      *E301077,16 Open the company file.
      =gfOpenFile(gcDataDir+'FabDye',gcDataDir+'FabDye','SH')
      *E301077,16 Reham On 12/30/98   *** End   ***
      
      *-- Update fabric Average cost in the fabric dyelot file.
      IF SEEK(FABRIC.Fabric+ FABRIC.Color , 'FabDye')
        SELECT FabDye
        REPLACE REST nAveCstBuy WITH Fabric.CostBuy ;
                     nFAve_cost WITH Fabric.CostUse  ;
               WHILE Fabric + Color = Fabric.Fabric + Fabric.Color ;
                 FOR EMPTY(Dyelot)     
      ENDIF
      *E301077,16 Reham On 12/30/98   *** Begin ***
      =gfCloseFile('FabDye')
      *E301077,16 Reham On 12/30/98   *** End   ***
    ENDIF
    *E301077,16 Reham On 12/30/98   *** Begin ***
    =gfCloseFile('MatInvJl')
    *E301077,16 Reham On 12/30/98   *** End   ***
    SELECT FABRIC

    *B038753,1 WSH If we update temp, Don't update working files before updating Stock value [Start]
    ENDIF
    SELECT FABRIC
    *B038753,1 WSH [End]

  ENDSCAN

  *B038753,1 Call Program to update Stock value [Start]
  IF llUpdInTmp
    *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [Begin]
    *DO (gcAppHome+'ICUPDSV') WITH '0002', laData[1], lcColorFil 
    *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [Begin]
    SELECT (lcColorFil)
    LOCATE
    SELECT (lcTmpBom)
    lnI = 1
    SCAN
  	  IF llMulCurr
	    *-- Get purchase price currency, duty currency or base currency
	    lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' , &lcColorFil..cPriceCur , ;
	                   IIF(&lcTmpBom..cCatgTyp $ 'MD' ,  &lcColorFil..cDutyCur , gcBaseCurr)))
	    lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  
	    *-- Get currency unit and exchange rate. 
	    llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
	    lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  
	    *-- Get currency symbol.
	    llFound    = SEEK(lcCurrency , 'SycCurr')
	    lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
	    STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
	    lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
        IF &lcTmpBom..cCatgTyp='P'
          laCostFld[1,2] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))
        ELSE
          lnI = lnI + 1
          *B607902,1 TMI [Start] reset the array dimension
          IF lnI>ALEN(laCostFld,1)
            DIMENSION laCostFld[lnI,2]
          ENDIF
          *B607902,1 TMI [End  ] 
          laCostFld[lnI,2] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))  
        ENDIF
	  ENDIF
    ENDSCAN
    DO (gcAppHome+'ICUPDSV') WITH '0002', laData[1], lcColorFil  , .F. , laCostFld
    *B128128,1 [End]
  ENDIF
  *B038753,1 [End]

ELSE
  *-- Define variables for the cost fields in the style file.
  FOR lnCntFld = 1 TO 5
    lcCntFld = STR(lnCntFld,1)
    lcRepFld&lcCntFld = ('n'+lcStyleTyp+'Cost'+lcCntFld)
  ENDFOR

  *B038753,1 WSH Get needed lines from Style file to temp file [Start]
  IF llUpdInTmp
    SELECT * ;
     FROM  Style ;
     WHERE Style = SUBSTR(laData[1],1,lnMajorLen) ;
     INTO  TABLE (gcWorkDir+lcColorfil)

    SELECT (lcColorFile)
    INDEX ON Style TAG (lcColorFil)
    SET ORDER TO (lcColorFil)
  ENDIF
  *B038753,1 WSH [End]

  *-- Scan for the major style "laData[1]".
  SELECT STYLE
  =SEEK(PADR(laData[1],lnMajorLen))
  SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(laData[1],lnMajorLen)
    *-- Save the record pointer in the style file.
    lnItemRec = RECNO()
    *-- Save the current style color.
    lcStyClr  = SUBSTR(Style.Style , lnColorStr , lnColorLen)
    *-- Blank the current cost variables.
    STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5
    
    *B602134,1 Reham On 11/16/98   *** Begin ***
    *B602134,1 Get the total sizes for the scale of the current style parent.
    lnAllScSiz = 0
    SELECT SCALE
    SUM CNT FOR Type + Scale = "S" + STYLE.Scale to lnAllScSiz
    *B602134,1 Reham On 11/16/98   *** End   ***
    
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
            
            *B604813,1 AMH Round before add [Start]
            *&lcCurVar = &lcCurVar + (((&lcTmpBom..nBomTotQty*Fabric.CostBuy/Fabric.Conv)*lnNoOfSize)/lnWholSclN)
            
            *C200303,1 HBG 13/03/2002 Apdate Item costs in style file on trigger for "Collage Ltd" from Bom file [Begin]
            IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0 AND lcStyleTyp = 'I'
              &lcCurVar = &lcCurVar + ROUND((((&lcTmpBom..nBomTotQty*&lcTmpBom..UntCost)*lnNoOfSize)/lnWholSclN),2)
            ELSE
            *C200303,1 [End]
              &lcCurVar = &lcCurVar + ROUND((((&lcTmpBom..nBomTotQty*Fabric.CostBuy/Fabric.Conv)*lnNoOfSize)/lnWholSclN),2)
            *C200303,1 HBG 13/03/2002 End if trigger for "Collage Ltd" [Begin]
            ENDIF  
            *C200303,1 [End] 
            
            *B604813,1 AMH [End]
            
          ENDIF
          IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("FABRIC")
            GOTO lnTmpRec IN FABRIC
          ENDIF
        *-- If the cost item is trim & not trim inventory or 
        *-- Price, Duty or Mfg operation.
        CASE (cCatGTyp = 'T' .AND. !Trim_Invt) .OR. cCatGTyp $ 'MDP'
          *-- Calculate the cost for the current cost item.
          
          *B604813,1 AMH Round before add [Start]
          *&lcCurVar = &lcCurVar + ((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN)
          &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN),2)
          *B604813,1 AMH [End]
          
        *-- If the cost item is style component.
        CASE cCatGTyp = 'S'
          lcStySeek = &lcTmpBom..Item
          *-- Save the record pointer in the style file.
          lnTmpRec  = RECNO("STYLE")
          *-- If the style component is same as style parent.
          IF laData[1] = SUBSTR(lcStySeek , 1 , lnMajorLen)
            *-- Get the cost from the temp. Bom file.
            
            *B604813,1 AMH Round before add [Start]
            *&lcCurVar = &lcCurVar + ((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN)
            &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN),2)
            *B604813,1 AMH [End]
            
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
                IF SEEK(lcStySeek , 'Style'))
                  *-- Calculate the cost for the current cost item.
                  
                  *B604813,1 AMH Round before add [Start]
                  *&lcCurVar = &lcCurVar + ((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN)
                  &lcCurVar = &lcCurVar + ROUND(((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN),2)
                  *B604813,1 AMH [End]
                  
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
                  IF SEEK(lcStySkExp , 'Style'))
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
              IF SEEK(lcStySeek , 'Style'))
                
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

    *B038753,1 WSH Update in Temp file not master file [Start]
    IF llUpdInTmp
      SELECT (lcCOlorFil)
      SEEK(STYLE.Style)
    ENDIF
    *B038753,1 WSH [End]

    REPLACE &lcRepFld1 WITH ROUND(lnCost1 , 2) ;
            &lcRepFld2 WITH ROUND(lnCost2 , 2) ;
            &lcRepFld3 WITH ROUND(lnCost3 , 2) ;
            &lcRepFld4 WITH ROUND(lnCost4 , 2) ;
            &lcRepFld5 WITH ROUND(lnCost5 , 2) ;
            Gros_Price WITH IIF(lcStyleTyp='I',ROUND(lnCost1 , 2),0)
    *B603589,1 AMH 09/12/2000 Updating Gros_Price in Style File [End  ]
    
    *-- If there was a fabric set as a primary fabric.
    IF !EMPTY(lcPrimFbr)
      REPLACE Fabric WITH lcPrimFbr
    ENDIF
    
    *-- Added to check the currency of the PPrice and duty components 
    *-- and take it in consideration when calculate the total cost to 
    *-- avoid the sum of different currencys.
    lnTCost1 = lnCost1
    lnTCost2 = lnCost2
    lnTCost3 = lnCost3
    lnTCost4 = lnCost4
    lnTCost5 = lnCost5
    
    SELECT STYLE
    IF llMulCurr AND lcStyleTyp = 'I'
      *-- Save the record pointer in the style file.
      lnStRecd = RECNO("STYLE")
      *-- Get purchase price currency, duty currency or base currency.
      lcPPrCurr = IIF(SEEK(PADR(laData[1],lnItemLen),"STYLE") , ;
                      IIF(!EMPTY(Style.cPriceCur) , Style.cPriceCur , ;
                          gcBaseCurr) , gcBaseCurr)
      lcDutyCur = IIF(SEEK(PADR(laData[1],lnItemLen),"STYLE") , ;
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

    *B038753,1 WSH Select temp file to upate in [Start]
    *SELECT STYLE
    IF llUpdInTmp
      SELECT (lcCOlorFil)
    ELSE
      SELECT STYLE
    ENDIF
    *B038753,1 WSH {End]

    *-- Update the total cost in the style file.
    *B602895,1 Reham On 05/19/1999   *** Begin ***
    *B602895,1 Update the total cost with rounded cost items instead of rounding their totals.
    *-- Round The cost updated in the style file.
    *REPLACE TotCost WITH lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5
    *REPLACE TotCost WITH ROUND((lnTCost1+lnTCost2+lnTCost3+lnTCost4+lnTCost5) , 2)
    REPLACE TotCost WITH ROUND(lnTCost1,2)+ROUND(lnTCost2,2)+ROUND(lnTCost3,2)+ROUND(lnTCost4,2)+ROUND(lnTCost5,2)
    *B602895,1 Reham On 05/19/1999   *** End   ***
    
    *B038753,1 WSH if we update in temp file, don't update master files [Start]
    IF !llUpdInTmp
    *B038753,1 WSH [End]

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
    
    *B038753,1 WSH if we update in temp file, don't update master files [Start]
    ENDIF
    SELECT STYLE
    *B038753,1 WSH [End]
    
  ENDSCAN

  *B038753,1 WSH Call Program to update new stock value [Start]
  IF llUpdInTmp
    *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [Begin]
    *DO (gcAppHome+'ICUPDSV') WITH '0001', PADR(laData[1],lnMajorLen), lcColorFil
    SELECT (lcColorFil)
    LOCATE
    SELECT (lcTmpBom)
    lnI = 1
    SCAN
  	  IF llMulCurr
	    *-- Get purchase price currency, duty currency or base currency
	    lcCurrency = IIF(lnBomLnNo = 0 , gcBaseCurr , IIF(&lcTmpBom..cCatgTyp='P' ,  &lcColorFil..cPriceCur , ;
	                   IIF(&lcTmpBom..cCatgTyp $ 'MD' ,  &lcColorFil..cDutyCur , gcBaseCurr)))
	    lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
  
	    *-- Get currency unit and exchange rate. 
	    llFound   = SEEK(gcBaseCurr + lcCurrency , 'SycExch')
	    lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  
	    *-- Get currency symbol.
	    llFound    = SEEK(lcCurrency , 'SycCurr')
	    lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
	    STORE SycCurr.cCurrSmbl TO lcCurrSmbl , lcCurSmbl1 
	    lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
        IF &lcTmpBom..cCatgTyp='P'
          laCostFld[1,2] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))
        ELSE
          lnI = lnI + 1
          *B607902,1 TMI [Start] reset the array dimension
          IF lnI>ALEN(laCostFld,1)
            DIMENSION laCostFld[lnI,2]
          ENDIF
          *B607902,1 TMI [End  ] 
          laCostFld[lnI,2] = IIF(lnBomLnNo = 0 , 0 , ROUND(&lcTmpBom..TotCost &lcRateSign lnExRate &lcUnitSign lnCurrUnit,2))  
        ENDIF
	  ENDIF
    ENDSCAN
    DO (gcAppHome+'ICUPDSV') WITH '0001', PADR(laData[1],lnMajorLen), lcColorFil,.F.,laCostFld
    *B128128,1 [End]
  ENDIF
  *B038753,1 WSH [End]

ENDIF

*B038753,1 WSH Close temp file [Start]
IF llUpdInTmp
  SELECT (lcColorfil)
  USE
  ERASE (gcWorkDir+lcColorfil+'.DBF')
  ERASE (gcWorkDir+lcColorfil+'.CDX')
ENDIF
*B038753,1 WSH [End]

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

*B038753,1 WSH Return if stock updated or update canceled [Start]
IF llUpdInTmp
  RETURN llContinSave
ENDIF
*B038753,1 WSH [End]

*!*************************************************************
*! Name      : lfvNotes
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Cost Sheet Notes.
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNotes()
*!*************************************************************
*
FUNCTION lfvNotes

*-- Clear all the trapped keys.
PUSH KEY
ON KEY

*-- Call the notepad program.
=NOTEPAD('F',laData[1])

POP KEY

*!*************************************************************
*! Name      : lfvActCost
*! Developer : Reham Al-Allamy
*! Date      : 12/17/1997
*! Purpose   : Function to activate the costing function.
*!*************************************************************
*! Calls     : COSTING.PRG
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvActCost()
*!*************************************************************
*
FUNCTION lfvActCost

ACTIVATE WINDOW gwcContrl1 TOP
_CUROBJ = OBJNUM(pbCosting)
KEYBOARD "{ENTER}"

*!*************************************************************
*! Name      : lfvCosting
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Display Costing information.
*!*************************************************************
*! Calls     : COSTING.PRG
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCosting()
*!*************************************************************
*
FUNCTION lfvCosting
PRIVATE lnBomRec , lnItmRec

*-- Save the current record pointer in the bom file.
lnBomRec  = RECNO(lcTmpBom)

*-- Open the temp. bom file in another alias.
USE (gcWorkDir + lcTmpBom) IN 0 AGAIN ALIAS (lcBomTemp)
SELECT (lcBomTemp)
INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcBomTemp)

*-- Clear the current filter in the style or fabric file.
SELECT (lcItemFile)
SET FILTER TO
*-- Save the current record pointer in the style or fabric file.
lnItmRec = RECNO(lcItemFile)

*-- Clear all the trapped keys.
PUSH KEY
ON KEY
*-- Define the mask to be send as a parameter.
lcItmSnd = IIF(lcStyleTyp = "T" , PADR(laData[1],7)+"******" , ;
   PADR(STUFF(lcStyMask , 1 , lnMajorLen , PADR(laData[1],lnMajorLen)),19))

*-- Call the costing program.
*DO (gcAppHome+"Costing.FXP") WITH lcItmSnd , lcTmpBom , lcStyleTyp
DO (gcAppHome+"Costing.FXP") WITH lcItmSnd , lcBomTemp , lcStyleTyp

POP KEY

*-- Close the temp. bom after coming from costing program.
USE IN (lcBomTemp)

*-- Restore saved record in bom file.
SELECT (lcTmpBom)

*B602298,1 Reham On 13/12/98   *** Begin ***
*B602298,1 Call the browse function to redisplay the style cost sheet lines again 
*B602298,1 if blanked from the costing program.
=lfBrwCstSh()
*B602298,1 Reham On 13/12/98   *** End   ***

IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT(lcTmpBom)
  GOTO lnBomRec
ENDIF

*-- Restore the filter in the style or fabric file.
SELECT (lcItemFile)
DO CASE
  CASE lcStyleTyp = 'I'
    SET FILTER TO !Style.Make
  CASE lcStyleTyp = 'M'
    SET FILTER TO Style.Make
ENDCASE

*-- Restore the saved record in the style or fabric file.
IF lnItmRec > 0 .AND. lnItmRec <= RECCOUNT(lcItemFile)
  GOTO lnItmRec
ENDIF


*!*************************************************************
**************    Functions for MFSTYCP Screen    *************
************** Copy from another style cost sheet *************
*!*************************************************************

*!*************************************************************
*! Name      : lfvCpyItem
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
    
    *B607469,1 SSE 08/21/2003 Point record to the nearest style. [Begin]
    *lcCopFmSty = gfStyBrw("M" , "" , "" , .F.)
    IF '?' $ lcCopFmSty
      lcCopFmSty = gfStyBrw("M" , "" , "" , .F.)
    ELSE
      lcCopFmSty = gfStyBrw("M" , lcCopFmSty , "" , .F.)
    ENDIF  
    *B607469,1 SSE 08/21/2003 Point record to the nearest style. [End]
    
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
      CASE lcStyleTyp = 'T'
        *-- Check to see if there is at least 1 color is domestic.
        SELECT FABRIC
        SET ORDER TO FABRIC   && Select the fabric + color tag.
        llFind = .F.          && Flag to know if there is colors domestic No.
        *-- Search to find if there is at least 1 color for this 
        *-- fabric with manufactered.
        =SEEK(PADR(laData[1],7))
        LOCATE REST WHILE FABRIC = PADR(laData[1],7) FOR Fabric.Make
        llFind = FOUND()
        SET ORDER TO CFABRIC
        =SEEK(PADR(laData[1],7))
        IF !llFind
          *** Copy cost sheet from manufactured materials only. ***
          *** <  Ok  > ***
          =gfModalGen("INM38017B00000" , "DIALOG" , "manufactured materials")
          llSkipIt = .T.
        ELSE
          llSkipIt = .F.
        ENDIF
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
    
  *B801908,1 Reham On 01/28/99   *** Begin ***
  *B801908,1 Get the style primary fabric for the "copy from style" 
  *B801908,1 to be saved for the current style cost sheet.
    lcCopPrFab = ""
  ELSE
    lcCopPrFab = IIF(lcStyleTyp <> 'T' , Style.Fabric , "")
  *B801908,1 Reham On 01/28/99   *** End   ***
  ENDIF
  
  *IF !llStyDye
  *  *-- Scan to validate the cost items for the selected item.
  *  SELECT BOM
  *  SCAN REST WHILE cItmMajor = lcCopFmSty ;
  *              FOR cCatGTyp $ 'FS' .OR. (cCatGTyp = 'T' .AND. Trim_Invt)
  *    IF SEEK(ALLTRIM(Item),lcItemFile) .AND. &lcItemFile..cDye_Flg = 'Y' .AND. llDyelot
  *      IF lcStyleTyp = 'T'
  *        *** Cannot copy Cost Sheet from material using ***
  *        *** components comes in dyelots to a material  ***
  *        *** which had not been setup to use dyelots.   ***
  *        *** <  Ok  > ***
  *        =gfModalGen("INM38003B00000" , "DIALOG")
  *      ELSE
  *        *** Cannot copy Cost Sheet from style using components ***
  *        *** comes in dyelots to a style which had not been     *** 
  *        *** setup to use a primary fabric with dyelots.        ***
  *        *** <  Ok  > ***
  *        =gfModalGen("INM38004B00000" , "DIALOG")
  *      ENDIF
  *      lcCopFmSty = SPACE(lnItemLen)
  *      lcStyDes   = ""
  *    ENDIF
  *  ENDSCAN
  *ENDIF
  
  SHOW GET lcStyDes   && Refersh the style description.
  
  *-- Copy Cost Items.
  *-- Get all the item cost sheet lines from BOM file.
  IF !EMPTY(lcCopFmSty)
    
    *B605400,1 AMH Optimizing the speed of copy cost sheet [Start]
    *SELECT " " AS cIncl, *,RECNO() AS nRecNo , "A" AS cStatus FROM BOM ;
     WHERE PADR(cItmMajor,lnItemLen) = PADR(lcCopFmSty,lnItemLen) .AND. ;
           PADR(Item,lnItemLen) <> PADR(laData[1],lnItemLen) ;
      INTO DBF (gcWorkDir+lcTmpCopy)
    *lnBomLnNo = _TALLY
    =lfGetCopy()
    *B605400,1 AMH [End]
    
    *-- If nothing was selected, replace with the current style "laData[1]".
    IF lnBomLnNo > 0
      *-- Replace all the item with laData[1] & set the proper index.
      REPLACE ALL cItmMajor WITH PADR(laData[1],19) ;
                  cItmMask  WITH IIF(lcStyleTyp<>"T" , PADR(STUFF(cItmMask,1,lnMajorLen,PADR(laData[1],lnMajorLen)),19) , cItmMask)
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
      
      *B605400,1 AMH Don't overwrite the current index [Start]
      *INDEX ON ccatgtyp + citmmajor TAG (lcTmpCopy)
      INDEX ON ccatgtyp + citmmajor TAG (lcTmpCopy) OF (lcTmpCopy)
      *B605400,1 AMH Don't overwrite the current index [End]
      
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
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
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
*B602102,1 Reham On 10/25/98
*B602102,1 Fix the problem of cross reference the colors in: lfvCopy()
*B602102,1 Change the way of adding the colors in case of same as
*B602102,1 and different colors.
*!*************************************************************
*
FUNCTION lfvCopy
PRIVATE llDel , lnSavRecNo , lcScanExp

*-- Return if there is nothing selected to be copy.
SELECT (lcTmpCopy)

*B605400,1 AMH Use lcTmpCopy1 index to optimize speed [Start]
*LOCATE FOR !EMPTY(cIncl)
PRIVATE lcOrder
lcOrder = SET('ORDER')
SET ORDER TO TAG (lcTmpCopy1)
SEEK "»"
*B605400,1 AMH [End]

IF !FOUND()
  *** Nothing selected to be copy. ***
  *** <  Ok  > ***
  =gfModalGen("INM38005B00000" , "DIALOG")
  _CUROBJ = OBJNUM(pbSel)
  
  *B605400,1 AMH Restore the order tag [Start]
  SELECT (lcTmpCopy)
  SET ORDER TO &lcOrder.
  *B605400,1 AMH [End]
  
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
  SELECT (lcTmpBom)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

*-- Variable hold the no. of records copied.
*lnBomLnNo = _TALLY
SELECT (lcTmpBom)
*B605400,1 AMH [End]

INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBom)
*E301411,1 SSH 31/05/20 Case copy from another copy detaiol operation to.
IF lnBomLnNo <> 0 .AND. llPwInst .AND. lcStyleTyp = 'M'
  =lfCopyDet()
ENDIF
*E301411,1 SSH [END]
STORE "" TO lcCopyScl , lcMainScl
SELECT STYLE
SET ORDER TO TAG CSTYLE
*-- Get the copy from style scale.
IF SEEK(SUBSTR(lcCopFmSty,1,lnMajorLen))
  lcCopyScl = Style.Scale
ENDIF
*-- Get the parent style scale.
IF SEEK(SUBSTR(laData[1],1,lnMajorLen))
  lcMainScl = Style.Scale
ENDIF
SET ORDER TO TAG STYLE

*-- True if the parent style & component style have different scales.
llMapSizes = IIF((lcCopyScl == lcMainScl) , .F. , .T.)

*-- If there is records copied.
IF lnBomLnNo > 0
  SELECT (lcTmpBom)
  *-- Scan through the copied records to validate all the records.
  SCAN
    *-- If manufactered style or material and the cost item is mfg operation
    *-- or the cost item was trim & consedered as non inventory, accept the record.
    *IF (lcStyleTyp $ 'MT' .AND. cCatGTyp='M') .OR. (cCatGTyp='T'.AND. !Trim_Invt)
    IF (cCatGTyp $ 'MPD') .OR. (cCatGTyp = 'T'.AND. !Trim_Invt)
      LOOP
    ENDIF
    
    *-- Check that the copy to fabric has all copy from fabric colors.
    IF lcStyleTyp = 'T'
      *-- If there is no stars in the mask, seek material + color in the fabric file.
      IF !('*' $ cItmMask) .AND. !SEEK(laData[1] + SUBSTR(cItmMask,1,6) , lcItemFile)
        *-- If the current color for the current fabric was not found, delete
        *-- the current cost item.
        lnBomLnNo = lnBomLnNo - 1
        REPLACE cStatus WITH "S"
        LOOP
      ENDIF
    ELSE
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
             =SEEK(PADR(laData[1],lnItemLen))
             SCAN REST WHILE (PADR(&lcItemFile,lnItemLen) = ;
                              PADR(laData[1],lnItemLen)) &lcScanExp
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
                 
                 *B602675,1 Reham On 03/18/99   *** Begin ***
                 *B602675,1 Add the new fabric+color with WIP value equal zero.
                 *B602625,1 Reham On 03/03/99   *** Begin ***
                 *B602625,1 Add the new fabric+color with stock value equal zero.
                 *REPLACE Color   WITH lcColor ;
                         OnHand  WITH 0       ;
                         OnOrder WITH 0       ;
                         Usage   WITH 0
                 REPLACE Color   WITH lcColor ;
                         OnHand  WITH 0       ;
                         OnOrder WITH 0       ;
                         Usage   WITH 0       ;
                         nStkVal WITH 0       ;
                         nMatWip WITH 0
                 *B602625,1 Reham On 03/03/99   *** End   ***
                 *B602675,1 Reham On 03/18/99   *** End   ***
                 
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
            IF !SEEK(SUBSTR(laData[1],1,lnMajorLen) + IIF(lcStyleTyp = "T" , "" , lcSeprator) + lcParent , lcItemFile)
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
            IF !SEEK(SUBSTR(laData[1],1, lnMajorLen) + lcSeprator + lcParent , "STYLE")
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
  *B801908,1 Reham On 01/28/99   *** Begin ***
  *B801908,1 Get the style primary fabric for the "copy from style" 
  *B801908,1 to be saved for the current style cost sheet.
  lcPrimFbr = lcCopPrFab
  *B123630,1 MHM 08/26/2004 Fix bug of Primary fabric replaced in case of copy cost sheet.[Start]
  *-- Scan the styles to get the primary fabric from the style file if :
  *-- 1- The current added cost item is fabric.
  *-- 2- Set the current fabric as primary fabric.
  *IF &lcTmpBom..cCatGTyp = "F" .AND. !EMPTY(lcNFabItem) .AND. cbPrim
  IF !EMPTY(lcPrimFbr)
    llExitPrim = .F.   && Flag to exit from the 2 scan.
    llSetPrim  = .F.   && Flag to know if there is a primary fabric or not.
    SELECT (lcTmpBom)
    SCAN
      *-- Scan in the style file to check the primary fabric.
      SELECT STYLE
      =SEEK(PADR(laData[1],lnMajorLen))
      SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(laData[1],lnMajorLen) ;
                  FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
        *-- If there is at least 1 primary fabric not empty.
        IF !EMPTY(STYLE.Fabric)
          *-- Set the flag to true.
          llSetPrim = .T.
        ENDIF
        
        *-- If the primary fabric is different from what is saved in style file.
        IF !EMPTY(STYLE.Fabric) .AND. !(PADR(lcPrimFbr,7) == STYLE.Fabric)
          llExitPrim = .T.
          EXIT
        ENDIF
      ENDSCAN
      IF llExitPrim
        EXIT
      ENDIF
      SELECT (lcTmpBom)
    ENDSCAN
  
    *-- If exit from the 2 scan because the primary fabric is different, warn 
    *-- the user from overriding the primary fabric.
    IF llSetPrim .AND. llExitPrim
      *** One or more style(s) have different primary fabric. ***
      *** Overwrite with the current selected fabric?         ***
      *** < YES > - < NO > ***
      *IF gfModalGen("QRM38125B00006" , "DIALOG") = 1
      IF !(gfModalGen("QRM38125B00006" , "DIALOG", Style.Fabric+"|"+lcPrimFbr) = 1 )
      *  lcPrimFbr = lcNFabItem
      *ELSE
        lcPrimFbr = " "
      ENDIF
    ELSE
      *B127591,1 MMR 05/18/2005 Fix Bug of Variable not found. [Start]
      lcNFabItem = IIF(TYPE('lcNFabItem')='U',lcPrimFbr,lcNFabItem)
      *B127591,1 MMR [End]
      lcPrimFbr = lcNFabItem
    ENDIF
  ENDIF
  *B123630,1 MHM [End]
  *B801908,1 Reham On 01/28/99   *** Begin ***
ENDIF
*-- Delete all the records was "S-> Same" status.
SELECT (lcTmpBom)
DELETE ALL FOR cStatus = "S"

SCAN
  *-- Calculate the total cost for the Mfg cost item or Duty 
  *-- cost item if it has percent.
  IF &lcTmpBom..cCatGTyp $ "MD" .AND. &lcTmpBom..nPercent <> 0
    DO lfUpdPerc IN gcAppHome+"MFCstSc.Fxp" WITH &lcTmpBom..cCatgTyp
  ENDIF
ENDSCAN

WAIT CLEAR
CLEAR READ

*!*************************************************************
*! Name      : lfvSel
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
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
    *B603726,1 ABD [End]
    lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
    REPLACE ALL cIncl WITH lcIncl ;
            FOR ccatgtyp = lcCatgory
    *B603726,1 ABD End for If statement. [Begin]
  ENDIF
  *B603726,1 ABD [End]
          
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
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
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
*B603726,1 ABD Cheak if style imported make cheak on currency. [Begin]
IF lcStyleTyp = 'I'
  = lfvAelDPCu()
ELSE
  *-- Select all the cost items.
  REPLACE ALL cIncl WITH "»"
  *B603726,1 ABD End if for if statement. [Begin]  
ENDIF
*B603726,1 ABD [End] 
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
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
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
*B801908,1 Reham On 01/28/99   *** Begin ***
lnBomLnNo = 0
*B801908,1 Reham On 01/28/99   *** End   ***

*!*************************************************************
*! Name      : lfBrwCopy
*! Developer : Reham Al-Allamy
*! Date      : 08/23/1997
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
*! Developer : Reham Al-Allamy
*! Date      : 08/23/1997
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

*B801908,1 Reham On 01/28/99   *** Begin ***
glFromBrow = .T.
*B801908,1 Reham On 01/28/99   *** End   ***

*-- Refresh the browse.
SHOW WINDOW (lcBrCpItmT) REFRESH SAME

*-- Define the select button prompt according to the current record status.
lcSelTxt = IIF(EMPTY(cIncl) , "\<Select" , "\<Unselect")

*-- Refresh the select button with the right prompt.
SHOW GET pbSel,1 PROMPT lcSelTxt

*!***************************************************************
***                Functions for MFSERCH Screen               ***
***         Screen to view all the available items to         ***
***         search for specific one in the main browse        ***
*!***************************************************************

*!*************************************************************
*! Name      : lfvSCstTyp
*! Developer : Reham Al-Allamy
*! Date      : 01/21/1998
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
*! Developer : Reham Al-Allamy
*! Date      : 01/21/1998
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
*! Developer : Reham Al-Allamy
*! Date      : 01/21/1998
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
*! Developer : Reham Al-Allamy
*! Date      : 01/21/1998
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
*! Developer : Reham Al-Allamy
*! Date      : 01/25/1998
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
IF !SEEK(PADR(laData[1],19) + lcSrType + PADR(lcSrItmMsk,19) + ;
         IIF(EVALUATE("lc"+lcStyleTyp+"TYPE"+lcSrType) $ "MPD" , ;
             PADR(lcSrItem,6) , SPACE(6) + PADR(lcSrItem,19) + lcSrItmClr))
  *-- If the record not found, go back to the previous saved record.
  IF lnSrchRec > 0 .AND. lnSrchRec <= RECCOUNT(lcTmpBom)
    GOTO lnSrchRec
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfMapSizes
*! Developer : Reham Al-Allamy
*! Date      : 10/25/1998
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
*E301045,1 Reham On 10/26/98
*E301045,1 Map the sizes between style parent & style component upon copy
*E301045,1 cost sheet from another style.
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
*! Developer : Reham Al-Allamy
*! Date      : 01/28/99
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
*B801908,1 Reham On 01/28/99
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
*! Developer : Reham Al-Allamy
*! Date      : 01/28/99
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
*B801908,1 Reham On 01/28/99
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
*! Developer : Reham Al-Allamy
*! Date      : 01/28/1999
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
*! Developer : Reham Al-Allamy
*! Date      : 01/28/1999
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
*! Developer : Reham Al-Allamy
*! Date      : 01/28/1998
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
*! Name      : lfChckCost
*! Developer : Reham Al-Allamy
*! Date      : 08/03/1998
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
        DO lfUpdCost IN gcAppHome+"MFCstSc.Fxp" WITH 1 , lnOld
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
*! Name      : lfGetPWBOM
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to get data from PWBOM .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetPWBOM()
*!*************************************************************
*!*E301411,1 SSH [Begin] 
*!*************************************************************
FUNCTION lfGetPWBOM

PRIVATE lnOldAls
lnOldAls = SELECT(0)
=gfOpenFile(gcDataDir+"pwbom" , gcDataDir+"pwbom" , "SH")
IF USED(lcPWBom)
  SELECT PWBOM
  *--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
  IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode)
    SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                    PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode
      SCAT MEMVAR MEMO
      IF !SEEK(m.citmmajor+m.mfgcode+m.coprcode+STR(m.nlineno,6),lcPWBom)
        INSERT INTO (lcPWBom) FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfvDetOpr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/05/2000
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
*!*E301411,1 SSH.
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
*! Name      : lfCreatTemp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Create temp files....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCreatTemp()
*!*************************************************************
*!*E301411,1 SSH.
*!*************************************************************
*!
FUNCTION lfCreatTemp
PRIVATE lnOldAls
*--- Here we dont need to check the PW module since this function
*--- is called from lfGetData which run under the our condition.
lnOldAls = SELECT(0)

*B604433,4 AMH open PWOPERAT file [Start]
IF !USED('PWOPERAT')
  =gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
ENDIF
*B604433,4 AMH [End]

=gfOpenFile(gcDataDir+"pwbom" , gcDataDir+"pwbom" , "SH")
=gfOpenFile(gcDataDir+"pwbomtol" , gcDataDir+"pwbomtol" , "SH")
=gfOpenFile(gcDataDir+'PWOPTOOL',gcDataDir+'PWOPTOOL','SH')
IF !USED(lcDetLin)
  CREATE TABLE &gcWorkDir.&lcDetLin (ChildOpr C(6),Desc C(30),WorkCent C(02),;
                                     SetUpTim N(6),PersTim N(6),MatchTim N(6),;
                                     Unit N(6),Seq C(2),RatType C(6),;
                                     RatePer N(3), UnitCost N(9,2),Tool C(2),MfgCode C(6),cStatus C(1))
ELSE
  *--- Select and ZAP if used because we call this function
  *--- Every time the mfgcode changed and because we are browsing from.
  *--- this temp file.
  SELECT(lcDetLin)
  ZAP
ENDIF
*--- But the following temps we dont need to delete unless we enter into
*--- new session, by undo or select new style. 

*B605726,1 KHM 03/27/2002 (Begin) Changing the index to sort the operations
*B605726,1                according to there sequence.
*INDEX ON MfgCode+ChildOpr TAG &lcDetLin
INDEX ON MfgCode+Seq+ChildOpr TAG &lcDetLin
*B605726,1 KHM 03/27/2002 (End)

*--- 2 PWBOM
IF !USED(lcPWBom)
  SELECT PWBOM
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcPWBom) FROM ARRAY laFileStru
  INDEX ON citmmajor+mfgcode+coprcode+STR(nlineno,6) TAG (lcPWBom)
ENDIF
*--- 3 pwbomtol
IF !USED(lcPWTol)
  SELECT pwbomtol
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcPWTol) FROM ARRAY laFileStru
  INDEX ON coprcode+citmmajor+ctolgrpid+STR(nlineno,6) TAG (lcPWTol)
ENDIF
SELECT(lnOldAls)

*!******************************************************************
*! Name      : lfGtDetOpr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Get detail operation....
*!******************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!******************************************************************
*! Passed Parameters  : lcMfgCode : Mfg Code.
*!                      llFrmTemp : Get Data from temp File.
*!                      llFrmCost : Called From New Cost Item Screen
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfGtDetOpr()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfGtDetOpr
PARAMETER lcMfgCode , llFrmTemp , llFrmMain

PRIVATE lnOldAls , lcKey , lcExp

lnOldAls = SELECT(0)
*--- If the Mfg Code exist in the temp file so,We will collect data from
*--- The Temp Files because if some things changed in the same session.
*--- i.e. to be Sure the we get the last updated data.
IF llFrmTemp
  SELECT (lcPWBOM)
  lcKey = "citmmajor+mfgcode+coprcode+STR(nlineno,6)"
  lcExp = PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode
ELSE
  SELECT PwOperat
  lcKey = "mfgcode+coprcode"
  lcExp = lcMfgCode
ENDIF
IF SEEK(lcExp)
  SCAN REST WHILE &lcKey = lcExp;
            FOR IIF(llFrmTemp,!(cStatus$'RD'),.T.)
    IF llFrmTemp
      =SEEK(lcMfgCode+cOprCode,'PwOperat')
    ENDIF
    m.MfgCode  = lcMfgCode
    m.ChildOpr = cOprCode
    m.Desc     = PwOperat.cDescRip
    m.WorkCent = PwOperat.cWorkCent
    m.SetUpTim = PwOperat.NSetup_Tim
    m.PersTim  = PwOperat.NEmp_Time
    m.MatchTim = PwOperat.NMach_Time
    m.Unit     = PwOperat.nUnit
    m.Seq      = cOperSeq
    m.RatType  = cOprAtType
    m.RatePer  = nOperatPer
    m.UnitCost = nOperCost
    m.Tool     = IIF(SEEK(cOprCode,'pwbomtol'),'Y','N')
    *--- MfgCode+ChildOpr
    *--- Check for the Child operaiton if it selected before
    *--- for the same line no.
    *--- LineNo : If the Mfg Code assigned more than once for the same
    *---          style so we will distinguish between them using
    *---          line no.
    *---          we need this distinguish because each group may have
    *---          different costs or diff. detail group.
    
    *B605726,1 KHM 03/27/2002 (Begin) Chaning the index by adding the sequence
    *IF !SEEK(lcMfgCode+m.ChildOpr,lcDetLin)
    IF !SEEK(lcMfgCode+m.Seq+m.ChildOpr,lcDetLin)
    *B605726,1 KHM 03/27/2002 (End)
    
      *--- IF we goinig to get from temp. and not from the new cost item
      *--- screen and the main Mfg Code lineno <> 0 because if the 
      *--- nLineNo = 0 this mean we did not save this session so 
      *--- we dont need to recollect data.
      IF llFrmMain
        IF (STR(&lcPWBOM..nLineNo,6)=STR(&lcTmpBom..nLineNo,6))
          INSERT INTO (lcDetLin) FROM MEMVAR
        ENDIF
      ELSE
        IF llNewSess .OR. llFrmTemp
          INSERT INTO (lcDetLin) FROM MEMVAR
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnOldAls)

*!******************************************************************
*! Name      : lfPwSav
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Save detail operation....
*!******************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!******************************************************************
*! Passed Parameters  : None.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfPwSav()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfPwSav

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- lcPWBom
IF USED(lcPWBom)
  SELECT (lcPWBom)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    IF SEEK(m.citmmajor+m.mfgcode+m.cOprCode+STR(m.nlineno,6),'PWBom')
      IF cStatus = 'D'
        SELECT PWBom
        =lfRemTol(cOprCode)
        DELETE
      ELSE
        SELECT PWBom
        GATH MEMVAR MEMO
      ENDIF  
    ELSE
      IF cStatus <> 'D'
        INSERT INTO PWBom FROM MEMVAR
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
*--- lcPWTol
IF USED(lcPWTol)
  SELECT(lcPWTol)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    *--- coprcode+citmmajor+ctolgrpid+STR(nlineno,6)
    IF SEEK(m.coprcode+m.citmmajor+m.ctolgrpid+STR(m.nlineno,6),'pwbomtol')
      IF m.cStatus = 'D' 
        SELECT pwbomtol
        DELETE
      ELSE
        SELECT pwbomtol
        GATH MEMVAR MEMO
      ENDIF
    ELSE
      IF cStatus <> 'D'
        INSERT INTO pwbomtol FROM MEMVAR
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnOldAls)


*!******************************************************************
*! Name      : lfPwEnDes
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Enable/Disabling detail operation Butt.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : None.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfPwEnDes()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfPwEnDes

PRIVATE lnOldAls
lnOldAls = SELECT(0)
=gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
*--- Check if the current Mfg Code Selected From PopUp
*--- Has detail operation.
llFondDet = SEEK(laMfgCode[lnNMfgCode,2],'pwoperat')
llBMfgOpr = .F.
DECLARE laMfg[1,2]
laMfg[1,1] = 'LMFGOPR'
laMfg[1,2] = 'llBMfgOpr'
=gfRltFld(laMfgCode[lnNMfgCode,2] , @laMfg , "MFGCODE")
*--- We will Enable Detail operation Button if and only if 
*--- this Mfg Code is consider as operaion YES and has detail operation.
IF llBMfgOpr .AND. llFondDet
*  IF laScrMode[3]
    SHOW GET pbDetOpr   ENABLE
*  ELSE
*    SHOW GET pbDetOpr   DISABLE
*  ENDIF      
  SHOW GET lnEstQtyB  DISABLE
  SHOW GET lnEstQtyA  DISABLE
  SHOW GET lnUnitCst  DISABLE
ELSE
  SHOW GET pbDetOpr   DISABLE
  SHOW GET lnUnitQtyB DISABLE
  SHOW GET lnUnitQtyA &lcLinStat
  SHOW GET lnUnitCst  &lcLinStat
ENDIF
SELECT(lnOldAls)

*!******************************************************************
*! Name      : lfPwRem
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Remove detail Operation.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : None.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfPwRem()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfPwRem

PRIVATE lnOldAls
lnOldAls = SELECT(0)
lnToDel  = nLineNo
lcOprToDel = mfgcode
*--- We have to call this function to check if the data collected before or
*--- Collect it.
*--- Since we maybe remove with out pressing detail operation button.
=lfGetData(lcOprToDel)
SELECT (lcPWBOM)
IF SEEK(IIF(llFromCtk , laData[2],laData[1] ))
  SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                  PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcOprToDel;
            FOR nLineNo = lnToDel
    REPLACE cStatus WITH  'D'
   *--- Remove the tool related to this operation.
   IF SEEK(cOprCode+PADR(IIF(llFromCtk , laData[2],laData[1] ),19),'pwbomtol') 
     =lfRemTol(cOprCode)
   ENDIF  
  ENDSCAN
ENDIF
SELECT (lnOldAls)

*!******************************************************************
*! Name      : lfPwDel
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Delete detail Operation.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : None.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfPwDel()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfPwDel

PRIVATE lnOldAls
lnOldAls = SELECT(0)

IF !USED('PWBOM')
  =gfOpenFile(gcDataDir+"pwbom" , gcDataDir+"pwbom" , "SH")
ENDIF
IF !USED('PWBOMTOL')
  =gfOpenFile(gcDataDir+"PWBOMTOL" , gcDataDir+"PWBOMTOL" , "SH")
ENDIF
SELECT PWBOM
IF SEEK(IIF(llFromCtk , laData[2],laData[1] ))
  SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                    PADR(IIF(llFromCtk , laData[2],laData[1] ),19)
    SELECT PWBOMTOL
    IF SEEK(PWBOM.coprcode+PADR(IIF(llFromCtk , laData[2],laData[1] ),19))
      DELETE REST WHILE coprcode+citmmajor+ctolgrpid+STR(nlineno,6)=;
                        PWBOM.coprcode+PADR(IIF(llFromCtk , laData[2],laData[1] ),19)
    ENDIF
    SELECT PWBOM
    DELETE
  ENDSCAN
ENDIF
SELECT (lnOldAls)


*!******************************************************************
*! Name      : lfPWClear
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Clear Temp Files.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : llErase : Erase Temp Files Or just Zap.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfPWClear()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfPWClear
PARAMETER llErase

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- lcDetLin
IF USED(lcDetLin)
  IF llErase
    USE IN (lcDetLin)
  ELSE
    SELECT (lcDetLin)
  ENDIF
ENDIF
IF llErase
  ERASE &gcWorkDir.&lcDetLin..DBF
  ERASE &gcWorkDir.&lcDetLin..CDX
ELSE
  ZAP
ENDIF
*--- lcPWBom
IF USED(lcPWBom)
  IF llErase
    USE IN (lcPWBom)
  ELSE
    SELECT (lcPWBom)
  ENDIF
ENDIF
IF llErase
  ERASE &gcWorkDir.&lcPWBom..DBF
  ERASE &gcWorkDir.&lcPWBom..CDX
ELSE
  ZAP
ENDIF
*--- lcPWTol
IF USED(lcPWTol)
  IF llErase
    USE IN (lcPWTol)
  ELSE
    SELECT (lcPWTol)
  ENDIF
ENDIF
IF llErase
  ERASE &gcWorkDir.&lcPWTol..DBF
  ERASE &gcWorkDir.&lcPWTol..CDX
ELSE
  ZAP
ENDIF
SELECT(lnOldAls)

*!******************************************************************
*! Name      : lfRemTol
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Remove Tools.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : lcOprToDl : Operaiotn to delete it tools.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfRemTol()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfRemTol
PARAMETER lcOprToDl

PRIVATE lnOld
lnOld = SELECT(0)
SELECT pwbomtol
*--- Here we are Working with Line No. since eatch detail operation may
*--- has different tools.
*--- coprcode+citmmajor+ctolgrpid+STR(nlineno,6)
SCAN REST WHILE coprcode+citmmajor+ctolgrpid+STR(nlineno,6)=;
                lcOprToDl+PADR(IIF(llFromCtk , laData[2],laData[1] ),19);
          FOR   STR(nlineno,6) = STR(&lcTmpBom..nLineNo,6)
  SCAT MEMVAR MEMO
  *--- If this tool not in the tmep file then insert it and replace
  *--- its status with "D" because you can cancel after remove.
  *--- So, We can not deal with the DBF it self.
  IF !SEEK(m.coprcode+m.citmmajor+m.ctolgrpid+STR(m.nlineno,6),lcPWTol)
    INSERT INTO (lcPWTol) FROM MEMVAR
  ENDIF
  SELECT (lcPWTol)
  REPLACE cStatus WITH 'D'
ENDSCAN
SELECT (lnOld)

*!******************************************************************
*! Name      : lfGetData
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : PW Function to Remove Tools.
*!******************************************************************
*! Calls     : None
*!******************************************************************
*! Passed Parameters  : lcMfgCode : Operaiotn to delete it tools.
*!******************************************************************
*! Returns            : None
*!******************************************************************
*! Example   : = lfGetData()
*!******************************************************************
*!*E301411,1 SSH.
*!******************************************************************
*!
FUNCTION lfGetData
PARAMETER lcMfgCode

*--- Start Create Temp Files if not created before.
*--- and this check from the function it self.
=lfCreatTemp()
*--- Start Get Data From the following
*--- 1) PWBOM
*--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode,lcPWBOM)
  SELECT (lcPWBOM)
  *--- Here we need the Lineno because the same detial operation
  *--- may exist with different lineNo.
  LOCATE REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                    PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode;
              FOR   STR(nLineNo,6)=STR(&lcTmpBom..nLineNo,6)
  IF !FOUND()
    =lfGetPWBOM()
  ENDIF
ELSE
  =lfGetPWBOM()
ENDIF

*--- 3) PWOPERAT
IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcMfgCode,lcPWBOM)
  =lfGtDetOpr(lcMfgCode,.T.,.T.)
ELSE
  =lfGtDetOpr(lcMfgCode)
ENDIF

FUNCTION lfCopyDet
*--- lcCopFmSty
PRIVATE lnOld , lnCntLn
=lfCreatTemp()
lnCntLn = 0
lnOld = SELECT(0)
SELECT (lcTmpBom)
GO TOP
SCAN FOR cCatgTyp = "M"
  IF SEEK(PADR(lcCopFmSty,19)+MfgCode,"PWBOM")
    lnCntLn = lnCntLn+1
    *REPLACE nLineNo WITH lnCntLn
    SELECT PWBOM
    SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                    PADR(lcCopFmSty,19)+&lcTmpBom..MfgCode
      SCAT MEMVAR MEMO
      m.citmmajor = IIF(llFromCtk , laData[2],laData[1] )
      *m.nLineNo   = lnCntLn
      *--- 
      IF !SEEK(m.citmmajor+m.mfgcode+m.coprcode+STR(m.nlineno,6),lcPwBom)
        INSERT INTO (lcPwBom) FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
SELECT(lnOld)
*!*************************************************************
*! Name      : lfvSelDPCu
*! Developer : Abdou Elgendi [ABD]
*! Date      : 07/25/2000
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
*B603726,1
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
      *B603726,1 Message : 34174.
      *B603726,1 There is a mismatch in XXX , Unable to copy this cost item.
      *B603726,1 Button  : 00000
      *B603726,1 Ok
      = gfModalGen('INM34174B00000','DIALOG',IIF(lcCatgory = 'P','Price currency','Duty currency'))

    CASE lcDutyCur1 =  lcDutyCur2 .AND. lcPricecu1 <> lcPricecu2
      IF lcDutyCur1 = lcPricecu1
        *B603726,1 Message : 34174.
        *B603726,1 There is a mismatch in XXX , Unable to copy this cost item.
        *B603726,1 Button  : 00000
        *B603726,1 Ok
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
    *B603726,1 Message : 34174.
    *B603726,1 There is a mismatch in XXX , Unable to copy this cost item.
    *B603726,1 Button  : 00000
    *B603726,1 Ok
    = gfModalGen('INM34174B00000','DIALOG',IIF(lcCatgory = 'P','Price currency','Duty currency'))
  ENDIF
ENDIF
SELECT STYLE
SET ORDER TO TAG &lcSaveOrd
SELECT(lnAlias)
*-- End Of lfvSelDPCu.
*!*************************************************************
*! Name      : lfvAelDPCu
*! Developer : Abdou Elgendi [ABD]
*! Date      : 07/25/2000
*! Purpose   : Function to get the price for the copy style.
*!           : In case of sellect all cost item
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None.
*!*************************************************************
*B603726,1
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
          *B604632,1 AMH fill clncl field with '»' in all cases [Start]
          *lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
          *REPLACE ALL cIncl WITH lcIncl ;
                  FOR ccatgtyp = lcCatgory
          REPLACE ALL cIncl WITH '»' ;
                  FOR ccatgtyp = lcCatgory
          *B604632,1 AMH [End]
          GOTO lnSaveRec
        CASE lcDutyCur1 <> lcDutyCur2
          llDifDCurr = .T.  
        CASE lcDutyCur1 =  lcDutyCur2 .AND. lcPricecu1 <> lcPricecu2
          IF  lcDutyCur1 = lcPricecu1
            llDifDCurr = .T.
          ELSE
            SELECT (lcTmpCopy)
            lnSaveRec = RECNO()
            *B604632,1 AMH fill clncl field with '»' in all cases [Start]
            *lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
            *REPLACE ALL cIncl WITH lcIncl ;
                    FOR ccatgtyp = lcCatgory
            REPLACE ALL cIncl WITH '»' ;
                    FOR ccatgtyp = lcCatgory
            *B604632,1 AMH [End]
            GOTO lnSaveRec
          ENDIF
      ENDCASE
    CASE lcCatgory = "P"  
      IF lcPricecu1 = lcPricecu2
        SELECT (lcTmpCopy)
        lnSaveRec = RECNO()
        *B604632,1 AMH fill clncl field with '»' in all cases [Start]
        *lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
        *REPLACE ALL cIncl WITH lcIncl ;
                FOR ccatgtyp = lcCatgory
        REPLACE ALL cIncl WITH '»' ;
                FOR ccatgtyp = lcCatgory
        *B604632,1 AMH [End]
        GOTO lnSaveRec
      ELSE    
        llDifPcurr = .T.
      ENDIF
    OTHERWISE
      SELECT (lcTmpCopy)
      lnSaveRec = RECNO()
      *B604632,1 AMH fill clncl field with '»' in all cases [Start]
      *lcIncl    = IIF(EMPTY(&lcTmpCopy..cIncl) , "»" ," ")
      *REPLACE ALL cIncl WITH lcIncl ;
              FOR ccatgtyp = lcCatgory
      REPLACE ALL cIncl WITH '»' ;
              FOR ccatgtyp = lcCatgory
      *B604632,1 AMH [End]
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
*! Name      : lfvData_2
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 09/06/2000
*! Purpose   : Function to find if there is at least 1 color for this.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvData_2()
*!*************************************************************
*B603747,1
FUNCTION lfvData_2

SELECT FABRIC
lnMainRec = RECNO()
SET ORDER TO TAG FABRIC   && Select the fabric + color tag.
llFind = .F.              && Flag to know if there is colors domestic No.
*-- Search to find if there is at least 1 color for this.
*-- fabric with domestic NO.
=SEEK(PADR(laData[1],7))
LOCATE REST WHILE FABRIC = PADR(laData[1],7) FOR Fabric.Make
llFind = FOUND()
SET ORDER TO TAG CFABRIC
IF lnMainRec > 0 .AND. lnMainRec <= RECCOUNT()
  GOTO lnMainRec
ENDIF
IF !llFind
  *B603747,1 Message No. :- 38015.
  *B603747,1 Message Text:- Only {manufactured Materials} are allowed here.
  *B603747,1 Button No.  :- 00000
  *B603747,1 Button Text :-  OK
  =gfModalGen("INM38015B00000" , "DIALOG" , "Manufactured materials")
  laScrMode    = .F.
  laScrMode[1] = .T.
  laData[1]  = SPACE(lnItemLen)  
  SHOW GET laData[1] ENABLE
ELSE
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
ENDIF
RETURN llFind
*- End Of lfvData_2.
*!*************************************************************

*!*************************************************************
*! Name      : lfvMarker
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/10/2001
*! Purpose   : Valid function of Marker field.
*!*************************************************************
*! Example   : = lfvMarker()
*!*************************************************************
*! E500402,1 AMH 01/10/2001.
*!*************************************************************
*!*E500402,1 AMH
FUNCTION lfvMarker

PRIVATE lnAlias, lcFile_Ttl, lcOrder, lcOldBrwFl, lcPattern
DIMENSION laTempData[1]
STORE '' TO laTempData, lcPattern

lnAlias = SELECT(0)
*B604433,1 AMH Using PDMPATRN file insted of SPPITEM file to validate marker field [Start]
IF TYPE('laAStySeg') # 'U'
  IF !EMPTY(laAStySeg[1,1])
    llReturn = (lcOldMrker == lcMarker7)
  ELSE
    llReturn = (lcOldMrker == lcMarker6)
  ENDIF
ELSE
  llReturn = (lcOldMrker == lcMarker)
ENDIF
*IF FILE(gcDataDir+"SPPITEM"+".DBF")

*B604569,1 AMH Using PDMMARK file instead of PDMPATRN file [Start]
*IF FILE(gcDataDir+"PDMPATRN"+".DBF") .AND. !llReturn
IF FILE(gcDataDir+"PDMMARK"+".DBF") .AND. !llReturn
  *=gfOpenFile(gcDataDir+"SPPITEM" , gcDataDir+"CPATERNNO" , "SH")
  *=gfOpenFile(gcDataDir+"PDMPATRN" , gcDataDir+"CFABRIC" , "SH")
  =gfOpenFile(gcDataDir+"PDMMARK" , gcDataDir+"CFABRIC" , "SH")
*B604569,1 AMH [End]

*B604433,1 AMH [End]
ELSE
  RETURN
ENDIF

SELECT STYLE
lcOrder = SET('ORDER')
SET ORDER TO TAG CSTYLE
SEEK(PADR(laData[1],19))
SET ORDER TO &lcOrder

*B604433,1 AMH Using PDMPATRN file insted of SPPITEM file to validate marker field [Start]
*SELECT SPPITEM

*B604569,1 AMH Using PDMMARK file insted of PDMPATRN file [Start]
*SELECT PDMPATRN
SELECT PDMMARK
*=SEEK(STYLE.PATTERN+'M')
IF TYPE('laAStySeg') # 'U'
  =SEEK(PADR(lcNFabItem,7))
ELSE
  =SEEK(PADR(lcCostItmF,7))
ENDIF

lcOldBrwFl = lcBrFields 

lcFile_Ttl = 'Markers'
*lcBrFields = "CPATERN_NO :H='Pattern no' ,"+;
             "CKEY_FLD   :H='Marker'     ,"+;
             "CDESC      :H='Description',"+;
             "DDATE      :H='Marker date'"
*lcBrFields = "CFABRIC    :H='Fabric'     ,"+;
             "CPATT_NO   :H='Pattern no' ,"+;
             "CMARK_ID   :H='Marker'     ,"+;
             "CMARK_DESC :H='Description',"+;
             "NESTBOMQTY :H='Net Unit'   ,"+;
             "NBOMWASTAG :H='Wastage %'  ,"+;
             "NBOMTOTQTY :H='Gross Unit'"
*B604861,1 AMH Don't browse net unit & wastage % [Start]
*lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
             "CMARK_DESC                  :H='Description',"+;
             "CFABRIC                     :H='Fabric'     ,"+;
             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
             "NTOTGOODS                   :H='Net Unit'   ,"+;
             "NLOSS                       :H='Wastage %'  ,"+;
             "NTOTQTY                     :H='Gross Unit'"
lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
             "CMARK_DESC                  :H='Description',"+;
             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
             "NTOTQTY                     :H='Gross Unit'"
*B604861,1 AMH [End]
*B604569,1 AMH [End]

*B604433,1 AMH [End]
IF TYPE('laAStySeg') # 'U'
  IF !EMPTY(laAStySeg[1,1])
    lcMarker = lcMarker7
  ELSE
    lcMarker = lcMarker6
  ENDIF
ENDIF

IF !EMPTY(lcMarker)
  IF EOF()
    =gfModalGen("INM000000B00000","DIALOG",'','',"No markers have been defined for this fabric.")
   
    *B604569,1 AMH Blank marker field [Start]
    IF TYPE('laAStySeg') # 'U'
      IF !EMPTY(laAStySeg[1,1])
        lcMarker7 = ''
        SHOW GET lcMarker7
      ELSE
        lcMarker6 = ''
        SHOW GET lcMarker6
      ENDIF
    ENDIF
    *B604569,1 AMH [End]
   
  ELSE
    *B604433,1 AMH Using PDMPATRN file insted of SPPITEM file to validate marker field [Start]
    *IF !SEEK(STYLE.PATTERN+'M'+lcMarker,'SPPITEM')
    *B604569,1 AMH Seek with correct pattern no. and rev. no. [Start]
    *=SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+STYLE.PATTERN)
    *B604861,1 AMH Seek in pattern field with padr of 10 [Start]
    *=SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2))
    =SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2))
    *B604861,1 AMH [End]
    *B604569,1 AMH [End]
    IF EOF()
      =AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))],;
                lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
      lcMarker = IIF(TYPE('laAStySeg') # 'U',laTempData[1],;
                 IIF(EMPTY(laTempData[1]),&lcTmpBom..cMarker,laTempData[1]))

      *B604569,1 AMH Using PDMMARK file insted of PDMPATRN file [Start]
      *lcPattern = PdmPatrn.cPatt_No
      lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
      *B604569,1 AMH [End]

    ELSE
      *B604569,1 AMH Seek with correct pattern no. and rev. no. [Start]
      *IF !SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+STYLE.PATTERN+lcMarker)
      *B604861,1 AMH Seek in pattern field with padr of 10 [Start]
      *IF !SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2)+lcMarker)
      IF !SEEK(IIF(TYPE('laAStySeg')#'U',PADR(lcNFabItem,7),PADR(lcCostItmF,7))+;
          PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
          RIGHT(ALLTRIM(STYLE.PATTERN),2)+lcMarker)
        *=AriaBrow([STYLE.PATTERN+'M'],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CKEY_FLD','laTempData')
        *=AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))+STYLE.PATTERN],;
                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
        *=AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))+;
                  PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),8)+;
                  RIGHT(ALLTRIM(STYLE.PATTERN),2)],;
                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
        =AriaBrow([IIF(TYPE('laAStySeg')#'U',lcNFabItem,PADR(lcCostItmF,7))+;
                  PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
                  RIGHT(ALLTRIM(STYLE.PATTERN),2)],;
                  lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
      *B604861,1 AMH [End]
        *B604569,1 AMH [End]
        lcMarker = IIF(TYPE('laAStySeg') # 'U',laTempData[1],;
                   IIF(EMPTY(laTempData[1]),&lcTmpBom..cMarker,laTempData[1]))

        *B604569,1 AMH Using PDMMARK file insted of PDMPATRN file [Start]
        *lcPattern = PdmPatrn.cPatt_No
        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
        *B604569,1 AMH [End]

      *B604569,1 AMH Using PDMMARK file instead of PDMPATRN file [Start]
      ELSE
        lcPattern = PdmMark.cPatt_No+PdmMark.cRev_No
      *B604569,1 [End]
      ENDIF
    ENDIF
    *B604433,1 AMH [End]
  ENDIF
ENDIF

lcBrFields = lcOldBrwFl 

IF TYPE('laAStySeg') # 'U'
  IF !EMPTY(laAStySeg[1,1])
    lcMarker7 = lcMarker
  ELSE
    lcMarker6 = lcMarker
  ENDIF
ELSE
  SELECT (lcTmpBom)
  REPLACE cMarker WITH lcMarker,;
          cStatus WITH IIF(cStatus = "S" , "M" , cStatus)
ENDIF

*B604433,1 AMH Using PDMPATRN file insted of SPPITEM file to validate marker field [Start]
*B604433,4 AMH don't update in case of no markers found [Start]
*IF !EMPTY(lcMarker) .AND. gfModalGen("QRM00000B00006","DIALOG",'','',;
                                     "Do you want to update the units from marker data ?")=1

*B604569,1 AMH Using PDMMARK file insted of PDMPATRN file [Start]
*IF !EMPTY(lcMarker) .AND. !EOF('PDMPATRN') .AND. gfModalGen("QRM00000B00006","DIALOG",'','',;
                                     "Do you want to update the units from marker data ?")=1
IF !EMPTY(lcMarker) .AND. !EOF('PDMMARK') .AND.;
   (PDMMARK.NTOTGOODS<>0 .OR. PDMMARK.NLOSS<>0 .OR. PDMMARK.NTOTQTY<>0) .AND.;
   gfModalGen("QRM00000B00006","DIALOG",'','',"Do you want to update the units from marker data ?")=1

*B604433,4 AMH [End]
  IF TYPE('laAStySeg') # 'U'
    IF !EMPTY(laAStySeg[1,1])
      *lnNEstUnt7 = PDMPATRN.NESTBOMQTY
      *lnNWstg7   = PDMPATRN.NBOMWASTAG
      *lnNUntQty7 = PDMPATRN.NBOMTOTQTY
      *B604861,1 AMH Update wastage with zero [Start]
      *lnNEstUnt7 = PDMMARK.NTOTGOODS
      *lnNWstg7   = PDMMARK.NLOSS
      lnNEstUnt7 = PDMMARK.NTOTQTY
      lnNWstg7   = 0
      *B604861,1 AMH [End]
      lnNUntQty7 = PDMMARK.NTOTQTY
      SHOW GET lnNEstUnt7
      SHOW GET lnNWstg7
      SHOW GET lnNUntQty7
      =lfvNUntQty('7')
    ELSE
      *STORE PDMPATRN.NESTBOMQTY TO lnNEsUnt6B, lnNEsUnt6A
      *STORE PDMPATRN.NBOMWASTAG TO lnNWstg6
      *STORE PDMPATRN.NBOMTOTQTY TO lnNUntQt6B, lnNUntQt6A
      *B604861,1 AMH Update wastage with zero [Start]
      *STORE PDMMARK.NTOTGOODS TO lnNEsUnt6B, lnNEsUnt6A
      *STORE PDMMARK.NLOSS     TO lnNWstg6
      STORE PDMMARK.NTOTQTY   TO lnNEsUnt6B, lnNEsUnt6A
      STORE 0                 TO lnNWstg6
      *B604861,1 AMH [End]
      STORE PDMMARK.NTOTQTY   TO lnNUntQt6B, lnNUntQt6A
      SHOW GET lnNEsUnt6B
      SHOW GET lnNEsUnt6A
      SHOW GET lnNWstg6
      SHOW GET lnNUntQt6B
      SHOW GET lnNUntQt6A
      =lfvNUntQty('6')
    ENDIF
  ELSE
    *=SEEK(PADR(lcCostItmF,7)+lcPattern+lcMarker,'PDMPATRN')
    =SEEK(PADR(lcCostItmF,7)+lcPattern+lcMarker,'PDMMARK')
    *STORE PDMPATRN.NESTBOMQTY TO lnEstQtyB, lnEstQtyA
    *STORE PDMPATRN.NBOMWASTAG TO lnWstg
    *STORE PDMPATRN.NBOMTOTQTY TO lnUnitQtyB, lnUnitQtyA
    *B604861,1 AMH Update wastage with zero [Start]
    *STORE PDMMARK.NTOTGOODS TO lnEstQtyB, lnEstQtyA
    *STORE PDMMARK.NLOSS     TO lnWstg
    STORE PDMMARK.NTOTQTY   TO lnEstQtyB, lnEstQtyA
    STORE 0                 TO lnWstg
    *B604861,1 AMH [End]
    STORE PDMMARK.NTOTQTY   TO lnUnitQtyB, lnUnitQtyA
    SHOW GET lnEstQtyB
    SHOW GET lnEstQtyA
    SHOW GET lnWstg
    SHOW GET lnUnitQtyB
    SHOW GET lnUnitQtyA
    =lfvUnitQty('A')
  ENDIF
  *B604899,1 AMH Update custom fields for Cathy Daniels from PDMMARK file [Start]
  IF ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
    =gfDoTriger('MFSCTSH',PADR('UPDBOMCA',10))
  ENDIF
  *B604899,1 AMH [End]
ENDIF
*B604899,1 AMH Update custom fields for Cathy Daniels from PDMMARK file [Start]
IF EMPTY(lcMarker) .AND. ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
  =SEEK(SPACE(27),'PDMMARK')
  =gfDoTriger('MFSCTSH',PADR('UPDBOMCA',10))
ENDIF
*B604899,1 AMH [End]

*=gfCloseFile('SPPITEM')
*=gfCloseFile('PDMPATRN')
=gfCloseFile('PDMMARK')
*B604569,1 AMH [End]

*B604433,1 AMH [End]
SELECT (lnAlias)
*-- end of lfvMarker.

*!*************************************************************
*! Name      : lfImportSt
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/15/2001
*! Purpose   : Importing style cost sheets.
*!*************************************************************
*! Example   : = lfImportSt()
*!*************************************************************
*! E301583,1 AMH 03/15/2001.
*!*************************************************************
*!*E301583,1 AMH
FUNCTION lfImportSt

PRIVATE llRet, llReject, lcMsg, lcFabOrd, lcStyOrd, lnFabRecNo, lnStyRecNo
PRIVATE lcFilter
SELECT STYLE
lcFilter = SET('FILTER')
SET FILTER TO
llRet = .T.
llReject = .F.
lcMsg = ''

*-- Open PDMLOG file.
=gfOpenFile(gcDataDir+"PDMLOG" , gcDataDir+"PDMITEMCD" , "SH")
SELECT PDMLOG
IF SEEK('PROTOTYPE'+laData[1])
  IF CBOMSTATUS <> 'O'
    llRet = .F.
    lcMsg = 'Cost sheet has already been imported.'
  ENDIF
ELSE
  llRet = .F.
  lcMsg = "Imported style "+ALLTRIM(laData[1])+" doesn't exist."
ENDIF

*-- Do nothing in this case.
IF !llRet
  =gfCloseFile('PDMLOG')
  *-- Unlock the current record in the style file.
  =gfModalGen("TRM00000B00000","DIALOG","",.F.,lcMsg)
  SELECT STYLE
  SET FILTER TO &lcFilter.
  =gfObj_Lock(.F.)
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
  RETURN .F.
ENDIF

*-- Open PDMBOM file.
=gfOpenFile(gcDataDir+"PDMBOM" , gcDataDir+"PDMBOM" , "SH")

lnStyRecNo = RECNO('STYLE')
lnFabRecNo = RECNO('FABRIC')
*-- Importing Cost Sheet
SELECT PDMBOM
IF SEEK(laData[1])
  SCAN REST WHILE cItmMajor = laData[1]
    DO CASE
      CASE TYPE = 'F' .OR. (TYPE = 'T' .AND. !llTrimInv)
        IF !SEEK(PADR(ITEM,7),'FABRIC')
          llReject = .T.
          lcMsg = 'Fabric ' + ALLTRIM(Item) + ' does not exist in the fabric file. Cannot proceed.'
        ELSE
          
          *-- In using constant or different.
          IF ICLR <> '******'
            SELECT FABRIC
            lcFabOrd = SET('ORDER')
            SET ORDER TO TAG FABRIC
            IF !SEEK(PADR(PDMBOM.ITEM,7)+PDMBOM.ICLR)
              llReject = .T.
              lcMsg = 'Fabric/color ' + ALLTRIM(PDMBOM.ITEM) + '/' + ALLTRIM(PDMBOM.ICLR) + ' does not exist in the fabric file. Cannot proceed.'
            ELSE
              IF llColorExt .AND. SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen) <> '******'
                SELECT STYLE
                lcStyOrd = SET('ORDER')
                SET ORDER TO TAG STYLE
                IF SEEK(ALLTRIM(PDMBOM.CITMMAJOR))
                  LOCATE REST WHILE CSTYMAJOR = PDMBOM.CITMMAJOR;
                         FOR SUBSTR(STYLE,lnColorStr,lnColorLen) =;
                             SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen)
                  IF !FOUND()
                    llReject = .T.
                    lcMsg = 'Style/color ' + ALLTRIM(PDMBOM.CITMMAJOR) + '/' +;
                             SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen) +;
                            ' does not exist in the style file. Cannot proceed.'
                  ENDIF
                ENDIF
                SET ORDER TO &lcStyOrd.
              ENDIF
            ENDIF
            SELECT FABRIC
            SET ORDER TO &lcFabOrd.
            SELECT PDMBOM
          ELSE

            *-- Case same as
            IF llColorExt .AND. SEEK(CITMMAJOR,'STYLE')
              SELECT FABRIC
              lcFabOrd = SET('ORDER')
              SET ORDER TO TAG FABRIC
              =SEEK(PADR(PDMBOM.ITEM,7))
              SCATTER TO laItemRec
              SELECT STYLE
              lcStyOrd = SET('ORDER')
              SET ORDER TO TAG STYLE
              SCAN REST WHILE CSTYMAJOR = PDMBOM.CITMMAJOR
                SELECT (lcTmpFbric)
                APPEND BLANK
                GATHER FROM laItemRec
                REPLACE COLOR      WITH SUBSTR(STYLE.STYLE,lnColorStr,lnColorLen) ;
                        ONHAND     WITH 0 ;
                        ONORDER    WITH 0 ;
                        USAGE      WITH 0 ;
                        nStkVal    WITH 0 ;
                        nMatWip    WITH 0
                *-- Call global function to add audit fields info.
                =gfAdd_Info(lcTmpFbric)
              ENDSCAN
              SELECT STYLE
              SET ORDER TO &lcStyOrd.
              SELECT FABRIC
              SET ORDER TO &lcFabOrd.
              SELECT PDMBOM
            ENDIF
          ENDIF
        ENDIF

      *-- Style Component
      CASE TYPE = 'S'
        lnRecNo = RECNO('STYLE')
        IF !SEEK(SUBSTR(ITEM,1,lnMajorLen),'STYLE')
          llReject = .T.
          lcMsg = 'Style ' + ALLTRIM(ITEM) + ' does not exist in the style file. Cannot proceed.'
        ELSE
          *B604508,1 AMH Chech color in iclr field instaed of item field [Start]
          *IF llColorExt .AND. SUBSTR(ITEM,lnColorStr,lnColorLen) <> '******'
          IF llColorExt .AND. ICLR  <> '******'
          *B604508,1 AMH [End]
            SELECT STYLE
            lcStyOrd = SET('ORDER')
            SET ORDER TO TAG STYLE
            *B604508,1 AMH Chech color in iclr field instaed of item field [Start]
            *IF !SEEK(PDMBOM.ITEM)
            LOCATE REST WHILE CSTYMAJOR = PADR(SUBSTR(PDMBOM.ITEM,1,lnMajorLen),19);
                        FOR SUBSTR(STYLE,lnColorStr,lnColorLen) = PDMBOM.ICLR
            IF !FOUND()
            *B604508,1 AMH [End]
              llReject = .T.
              *B604508,1 AMH Get color from iclr field instaed of item field [Start]
              *lcMsg = 'Style/color ' + ALLTRIM(PDMBOM.ITEM) + ' does not exist in the style file. Cannot proceed.'
              lcMsg = 'Style/color ' + ALLTRIM(PDMBOM.ITEM) + '/' + PDMBOM.ICLR + ' does not exist in the style file. Cannot proceed.'
              *B604508,1 AMH [End]
            ELSE
              IF llColorExt .AND. SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen) <> '******'
                IF SEEK(ALLTRIM(PDMBOM.CITMMAJOR))
                  LOCATE REST WHILE CSTYMAJOR = PDMBOM.CITMMAJOR;
                         FOR SUBSTR(STYLE,lnColorStr,lnColorLen) =;
                             SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen)
                  IF !FOUND()
                    llReject = .T.
                    lcMsg = 'Style/color ' + ALLTRIM(PDMBOM.ITEM) + '/' +;
                            SUBSTR(PDMBOM.CITMMASK,lnColorStr,lnColorLen) +;
                            ' does not exist in the style file. Cannot proceed.'
                  ENDIF
                ENDIF
                SET ORDER TO &lcStyOrd.
              ENDIF
            ENDIF
            IF BETWEEN(lnRecNo,1,RECCOUNT())
              GOTO lnRecNo
            ENDIF
            SELECT PDMBOM
          ELSE
            IF llColorExt .AND. SEEK(SUBSTR(ITEM,1,lnMajorLen),'STYLE')
              lcCurScale = SUBSTR(Style.Scale,1,1)
              DECLARE laTmpScale[1]
              laTmpScale = ""
        
              *-- Get the available sizes for the current scale.
              SELECT SCALE
              SELECT Scale FROM (gcDataDir+"Scale") ;
               WHERE Type + SUBSTR(Scale,1,1) = "S" + lcCurScale ;
                INTO ARRAY laTmpScale
              SELECT STYLE
              SCATTER TO laItemRec
              lcStyOrd = SET('ORDER')
              SET ORDER TO TAG STYLE
              =SEEK(ALLTRIM(PDMBOM.CITMMAJOR))
              SCAN REST WHILE CSTYMAJOR = PDMBOM.CITMMAJOR
                lcNonMajor = SUBSTR(STYLE.Style , lnMajorLen+1 , lnNonMjLen + IIF(!EMPTY(lcSeprator) , 1 , 0))
                SELECT (lcTmpStyle)
                APPEND BLANK
                GATHER FROM laItemRec
                IF llExtSizSc
                  IF !EMPTY(laTmpScale[1])
                    FOR lnCnt = 1 TO ALEN(laTmpScale,1)
                      REPLACE STYLE     WITH SUBSTR(PDMBOM.Item,1,lnMajorLen) + lcNonMajor + lcSizeSep + laTmpScale[lnCnt] ;
                              cStyMajor WITH SUBSTR(PDMBOM.Item,1,lnMajorLen) ;
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
                    ENDFOR
                  ENDIF
                ELSE
                  REPLACE STYLE     WITH SUBSTR(PDMBOM.Item,1,lnMajorLen) + lcNonMajor;
                          cStyMajor WITH SUBSTR(PDMBOM.Item,1,lnMajorLen) ;
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
                ENDIF
                *-- Call global function to add audit fields info.
                =gfAdd_Info(lcTmpStyle)
              ENDSCAN
              SELECT STYLE
              SET ORDER TO &lcStyOrd.
              SELECT PDMBOM
            ENDIF
          ENDIF
        ENDIF

      *-- MFG Operation.
      *B604328,1 AMH Case for manufactured styles [Start]
      *CASE TYPE = 'M' .AND. (MFGCODE='DUTY' .OR. MFGCODE='PRICE' .OR. MFGCODE='SERV')
      CASE TYPE = 'M' .AND. lcStyleTyp = 'M' .AND. ;
           (MFGCODE='DUTY' .OR. MFGCODE='PRICE' .OR. MFGCODE='SERV')
      *B604328,1 AMH (End)     
      
        IF !SEEK('DMFGCODE   '+MFGCODE,'CODES')
          IF !SEEK('NMFGCODE   '+MFGCODE,'CODES')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,CDISCREP,LRLTFIELDS,CRLTFIELD);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,PDMBOM.DESC,.F.,'N')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','CCONTCODE','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','CCONTNAME','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','COPERSEQ','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','GLACCOUNT','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LEADTIME','N','1')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LINHOUSE','L','F')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LMFGOPR','L','F')
          ENDIF
        ENDIF

      *B604328,1 AMH Case for imported styles [Start]
      CASE TYPE = 'M' .AND. lcStyleTyp = 'I' .AND. MFGCODE='SERV'
        IF !SEEK('DMFGCODE   '+MFGCODE,'CODES')
          IF !SEEK('NMFGCODE   '+MFGCODE,'CODES')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,CDISCREP,LRLTFIELDS,CRLTFIELD);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,PDMBOM.DESC,.F.,'N')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','CCONTCODE','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','CCONTNAME','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','COPERSEQ','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','GLACCOUNT','C')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LEADTIME','N','1')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LINHOUSE','L','F')
            INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,LRLTFIELDS,CRLTFIELD,CRLTD_NAM,;
                   CRLTD_TYP,CRLTD_VLU);
                   VALUES;
                   ('N','MFGCODE',PDMBOM.MFGCODE,.F.,'Y','LMFGOPR','L','F')
          ENDIF
        ENDIF
      *B604328,1 AMH [End]
    ENDCASE
    lnI = 0
    lcTyp = ''
    FOR lnI = 6 TO 18 STEP 3
      *B604328,1 AMH in case of imported style with MFG price use 'P' or duty use 'D' [Start]
      *IF laMainSetp[lnI,2] = PDMBOM.TYPE
      IF laMainSetp[lnI,2] = IIF(PDMBOM.TYPE='M' .AND. lcStyleTyp='I' .AND. ;
                                (PDMBOM.MFGCODE='DUTY' .OR. PDMBOM.MFGCODE='PRICE'),;
                                IIF(PDMBOM.MFGCODE='PRICE','P','D'),PDMBOM.TYPE)
      *B604328,1 AMH [End]
        lcTyp = STR((lnI/3)-1,1)
        EXIT
      ENDIF
    ENDFOR
    IF EMPTY(lcTyp)
      llReject = .T.
      lcMsg = 'One or more Cost elements does not exist. Cannot proceed.'
    ENDIF
    IF llReject
      SELECT (lcTmpBom)
      ZAP
      llRet = .F.
      EXIT
    ENDIF
    IF lPrimFab
      SELECT STYLE
      IF SEEK(laData[1])
        =gfObj_Lock(.T.)
        REPLACE FABRIC WITH PDMBOM.ITEM
        =gfObj_Lock(.F.)
      ENDIF
    ENDIF
    SELECT PDMBOM
    SCATTER MEMO MEMVAR
    m.nBomTotQty = m.nEstBomQty

    *B604328,1 AMH in case of imported style with MFG price use 'P' or duty use 'D' [Start]
    m.cCatgTyp = IIF(PDMBOM.TYPE='M' .AND. lcStyleTyp='I' .AND. (PDMBOM.MFGCODE='DUTY' .OR. PDMBOM.MFGCODE='PRICE'),;
                    IIF(PDMBOM.MFGCODE='PRICE','P','D'),PDMBOM.TYPE)
    m.mfgcode = IIF(PDMBOM.TYPE='M' .AND. lcStyleTyp='I' .AND. (PDMBOM.MFGCODE='DUTY' .OR. PDMBOM.MFGCODE='PRICE'),;
                    IIF(PDMBOM.MFGCODE='PRICE','******','######'),PDMBOM.MFGCODE)
    *B604328,1 AMH [End]

    SELECT (lcTmpBom)
    APPEND BLANK
    GATHER MEMO MEMVAR

    *B604328,1 AMH Changing the replacement command by removing the[Start]
    *B604328,1 AMH replacement of cCatgTyp.
    *REPLACE CCATGTYP   WITH PDMBOM.TYPE;
            TYP        WITH lcTyp;
            UNTCOST    WITH MIN(ROUND(IIF(NBOMTOTQTY<>0,TOTCOST/NBOMTOTQTY,0),3),9999999.999);
            CSTATUS    WITH 'A';
            NRECNO     WITH RECNO()
    REPLACE TYP        WITH lcTyp;
            UNTCOST    WITH MIN(ROUND(IIF(NBOMTOTQTY<>0,TOTCOST/NBOMTOTQTY,0),3),9999999.999);
            CSTATUS    WITH 'A';
            NRECNO     WITH RECNO()
    *B604328,1 AMH [End]
    *B604508,1 AMH Fix saving style component [Start]
    IF CCATGTYP = 'S'
      REPLACE ITEM WITH STUFF(ITEM,lnColorStr-1,lnColorLen,SUBSTR(CITMMASK,lnColorStr-1,1)+ICLR),;
              ICLR WITH ''
    ENDIF
    *B604508,1 AMH [End]

  ENDSCAN
ELSE
  llRet = .F.
  lcMsg = IIF(EMPTY(lcMsg),"Cost sheet doesn't exist.",lcMsg)
ENDIF

IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
  GOTO lnStyRecNo IN STYLE
ENDIF

IF BETWEEN(lnFabRecNo,1,RECCOUNT('FABRIC'))
  GOTO lnFabRecNo IN FABRIC
ENDIF

=gfCloseFile('PDMBOM')
IF !llRet
  *-- Unlock the current record in the style file.
  =gfModalGen("TRM00000B00000","DIALOG","",.F.,lcMsg)
  SELECT STYLE
  SET FILTER TO &lcFilter.
  =gfObj_Lock(.F.)
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
  RETURN .F.
ENDIF
lnBomLnNo = RECCOUNT(lcTmpBom)
=lfBrwCstSh()
RETURN .T.
*-- end of lfImportSt.

*!*************************************************************
*! Name      : lfwMarker
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/07/2001
*! Purpose   : When function of Marker field.
*!*************************************************************
*! Example   : = lfwMarker()
*!*************************************************************
*! B604433,1 AMH 05/07/2001.
*!*************************************************************
*!*B604433,1 AMH
FUNCTION lfwMarker

IF TYPE('laAStySeg') # 'U'
  IF !EMPTY(laAStySeg[1,1])
    lcOldMrker = lcMarker7
  ELSE
    lcOldMrker= lcMarker6
  ENDIF
ELSE
  lcOldMrker = lcMarker
ENDIF
*--end of lfwMarker.

*!*************************************************************
*! Name      : lfGetCopy
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/30/2002
*! Purpose   : collecting the copy records
*!*************************************************************
*! Example   : = lfGetCopy()
*!*************************************************************
*! B605400,1 AMH 01/30/2002.
*!*************************************************************
*!*B605400,1 AMH
FUNCTION lfGetCopy

PRIVATE lcOrder, lcStyKey, lcStyScale, llDifScale, llExit, lnMsg
*B605444,1 AMH Check if scale of the current style different to scale of copied style [Start]
STORE .F. TO llDifScale, llExit
IF lcStyleTyp <> 'T'
  SELECT STYLE
  lcStyKey = EVALUATE(KEY())
  =SEEK(PADR(laData[1],lnItemLen))
  lcStyScale = SCALE
  =SEEK(PADR(lcCopFmSty,lnItemLen))
  llDifScale = (lcStyScale#SCALE)
  
  *B605714,1 AMH Save the scale of style which we copy from [Start]
  lcCpyScale = SCALE
  *B605714,1 AMH [End]
  
  =SEEK(lcStyKey)
ENDIF
*B605444,1 AMH [End]
SELECT BOM
lcOrder = SET('ORDER')
SET ORDER TO TAG BOM
lnBomLnNo = 0
IF SEEK(PADR(lcCopFmSty,lnItemLen))
  SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = PADR(lcCopFmSty,lnItemLen);
            FOR PADR(Item,lnItemLen) <> PADR(laData[1],lnItemLen)
    *B605444,1 AMH Check if cost sheet base on size [Start]
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
    *B605444,1 AMH [End]
    lnBomLnNo = lnBomLnNo + 1
    SCATTER MEMO MEMVAR
    m.cStatus = 'A'
    m.nRecNo  = RECNO()
    IF llExit
      
      *B605714,1 AMH Copy all sizes only [Start]
      *m.lBasOnSiz = .F.
      *m.mSizes = SPACE(0)
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
      *B605714,1 AMH [End]
      
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

*E301842,1 AMH Validate Fabric/trim field & Validate Color [Start]
*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/15/2002
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
*E301842,1 AMH Validate Fabric/trim field & Validate Color [Start]

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
      IF lcStyleTyp='T' .AND. lcCostItmF = laData[1]
        *** Component Fabric and Cost Sheet Material Cannot be the same. ***
        *** <  Ok  > ***
        =gfModalGen("INM38020B00000" , "DIALOG")
        lcCostItmF = SPACE(7)
        _CUROBJ    = OBJNUM(lcCostItmF)
        SELECT (lnAlias)
        RETURN
      ENDIF
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
      IF lcStyleTyp='T' .AND. lcCostItmF = laData[1]
        *** Component Trim and Cost Sheet Material Cannot be the same. ***
        *** <  OK  > ***
        =gfModalGen("INM38024B00000" , "DIALOG")
        lcCostItmF = SPACE(7)
        _CUROBJ    = OBJNUM(lcCostItmF)
        SELECT (lnAlias)
        RETURN
      ENDIF
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/16/2002
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/16/2002
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
  =SEEK(PADR(laData[1],lnItemLen))
  SCAN REST WHILE PADR(&lcItemFile,lnItemLen) = PADR(laData[1],lnItemLen) &lcScanExp
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/15/2002
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
*E301842,1 AMH [End]
