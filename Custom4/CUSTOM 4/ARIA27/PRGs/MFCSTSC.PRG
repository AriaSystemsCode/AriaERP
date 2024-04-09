*:************************************************************************
*: Program file  : MFCSTSC.PRG
*: Program desc. : New Cost Sheet Item
*: For screen    : MFCSTSC.SCX (0,1,2,3,4,5,6,7,8,9,10)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufactering Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300722,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Screens    : MFCstSC.SPX (New cost item screen)
*:         Programs   : MFCSTSZ.Fxp (Select sizes program)
*:         Functions  : lfFillArry
*:                      lfGetSetup
*:                      lfStylInfo
*:                      lfShwCSh_C
*:                      lfvCostType
*:                      lfvMfgCode
*:                      lfwClrClass
*:                      lfvClrClass
*:                      lfvCScheme
*:                      lfvItmClrs
*:                      lfvCsItmClr
*:                      lfvNUntQty
*:                      lfvNUntCst
*:                      lfvDutyable
*:                      lfvNCstPrc
*:                      lfvNCstItm
*:                      lfvNMfgOpr
*:                      lfwStyNSeg
*:                      lfvStyNSeg
*:                      lfvSelVal
*:                      lfvAcptNew
*:                      lfvClsNew
*:                      lfIntNewCS
*:                      lfCreatMsk
*:                      lfUpdPerc
*:                      lfUpdCost
*:************************************************************************
*: Passed Parameters  : lcStyleTyp : 'M' Manufacturing Styles
*:                                   'I' Imported Styles
*:                                   'T' Material
*:                    : lcMajor    : laData[1]
*:                    : lcTmpBom   : Bom file temp. name.
*:                    : lcTmpFbric : Fabric file temp. name.
*:                    : lcTmpStyle : Style file temp. name.
*:************************************************************************
*: Example            :
*:  Call the program that add a new cost item.
*:  DO (gcAppHome+"MFCSTSC") WITH lcStyleTyp , laData[1] , lcTmpBom ,
*:                lcTmpFbric , lcTmpStyle , IIF(cbOnSize , "Y" , "N")
*:************************************************************************
*:  Modifications :
*B602121,1  Reham On 10/28/98
*B602121,1  1- Disable the cost item component (F,T,S) after selecting 
*B602121,1     a way of matching colors.
*B602121,1  2- Upon accepting new cost item, give the user a warning 
*B602121,1     message if the color field was empty.
*B602121,1  3- Fix the problem of getting stuck in an infinite loop upon 
*B602121,1     entering a component style exactly like the parent style.
*B801795,1  Reham On 11/17/98
*B801795,1  Fix a bug in function lfCreatMsk() to void the displaying of 
*B801795,1  message "This Cost Item does not apply to any style."
*B602213,1  Reham On 11/17/98
*B602213,1  1- Fix the problem of the popup that hold (All Values - Specific values)
*B602213,1     For style code structure that include segments saved in the
*B602213,1     ICSegVal file in the Major & the non-major.
*B602202,1  Reham On 11/26/98
*B602202,1  Disable check box [ ] Set as primary fabric if cost item is Trim.
*B602339,1  Reham On 14/12/98
*B602339,1  Fix the error of getting zero cost for trim if not 
*B602339,1  Trim Inventory Maintained.
*B602132,1  Reham On 12/17/98
*B602132,1  If the long description for the segment was empty, display the short description.
*E301077,16 Reham On 01/03/99
*E301077,16 Open ICSegVal in this program level.
*B602539,1  Reham On 02/21/99
*B602539,1  Fix error "Variable laSeg not found" by not calling function 
*B602539,1  "gfItemMask" if the program is material cost sheet.
*B602624,1  Reham On 03/03/99
*B602624,1  Round the gross units to 3 dicimals.
*B602625,1  Reham On 03/03/99
*B602625,1  Add the new fabric+color with stock value equal zero.
*B602638,1  Reham On 03/04/99
*B602638,1  Update the UOM field.
*E301077,16 Reham On 03/08/99
*E301077,16 Use field cDefCode instead of field cComp_Id if using file Codes.
*B802097,1  Reham 04/05/99
*B802097,1  Refresh the UOM field if select trim from the material file.
*B603016,1  Reham On 06/27/1999
*B603016,1  Fix the bug of retrieving the "Trim Inventory maintained"
*B603016,1  value from setup file.
*C200080,6  Reham On 08/11/1999
*C200080,6  Enable the unit qty. & wastage if the the cost item is MFG.
*B603348,1  Abdou on 02/02/2000
*B603348,1  Increase The Size of purchase price per (unit cost field) item to 9999999.999 
*B603348,1  Fixed In Screens [ mfcstsc6.scx , mfcstsc7.scx ]
*B603351,1 SSH Enhancement performance.
*E301411,1 SSH 31/05/20 Enhancement this program to support Piece Work
*E301411,1              Module by adding new button to assigne detail Oper.
*E301411,1              to the mfg. operation.
*E500348,1 KHM 06/04/2000 Adding the popup option which allows the user to 
*E500348,1                select a value (Color) in case of Material Cost 
*E500348,1                Sheet.
*B803553,1 ABD 08/08/2000 Fix bug that miscalculate the duty charges on the style BOM.
*B802958,1 ABD 09/05/2000 fix bug thet the color filed should be blank when
*B802958,1               Cost Type  "XXXX color same as fabric color".
*E500402,1 AMH 01/10/2001 Adding the marker field in screen MFCSTSC6 and.
*E500402,1                MFCSTSC7. The marker field will be shown in case 
*E500402,1                of Fabric only and "SP" module is installed.
*B604641,1 SSH Use the module variable instead of installed modules variable.
*B604802,1 AMH 08/19/2001 Fix bug that the style cost sheet dosen't calculate 
*B604802,1                duty % in the edit mode.
*B604899,1 AMH 09/12/2001 Update custom fields for Cathy Daniels from PDMMARK file.
*C102742,1 AMH 10/10/2002 Display the cost status popup in the MF style cost sheet
*B606979,1 AMH 02/25/2002 Fix the bug of scale code error in case of style componant
*B606979,1 AMH            for extended size scale style.
*C037892,1 MHM 06/15/2004 Validate for Custom sales order cost sheet
*B127371,1 EIH 05/31/2005 Fix the bug of display valid description in style cost sheet screen 
*B127371,1 EIH            When user select cost type error hapen when ther are same cost type 
*B127371,1 EIH            In setup of IC .
*T20060906.0003 (B607793 ) fix a problem related to B127371, that the array 1st column must be a numeric value for all lines
*:************************************************************************
*
PARAMETERS lcStyleTyp , lcMajor , lcTmpBom , lcTmpFbric , lcTmpStyle , ;
           llOnSize

PRIVATE lcItemFile , lcCurAlias , lcStyTag , lcFbrTag , ;
        lcBsCrSmb6 , lcBsCrSmb7 , lcCrSmbA6 , lcCrSmbA7 , ;
        lcCrSmbB6 , lcCrSmbB7 , lcRateSign , lnExRate6 , ;
        lnExRate7 , lnTotEqu6 , lnTotEqu7 , lcUnitSign , ;
        lcStyleHdr , lcSeprator , lcNMjrPic , lcMajorPic , ;
        lcStyMask , lcNonMjHdr , lcItemExp , lcSizeSep , ;
        lcPricCur , lcDutyCur , lnCurRecNo , lnItemLen , ;
        lnMajorLen , lnNonMjLen , lnSizePos , lnSizeLen , ;
        lnColorStr , lnColorLen , llNonMjExt , llColorExt , ;
        lnCurrUnit , lnNMfgCode , lnBomRec , llTrimInv , ;
        llExtSizSc , llMulCurr , llStyDye , llMfgOpr , ;
        cbPrim , llPrimFbr , llSame

PRIVATE lc&lcStyleTyp.SLBL1 , lc&lcStyleTyp.SLBL2 , lc&lcStyleTyp.SLBL3 , ;
        lc&lcStyleTyp.SLBL4 , lc&lcStyleTyp.SLBL5 , lc&lcStyleTyp.COST1 , ;
        lc&lcStyleTyp.COST2 , lc&lcStyleTyp.COST3 , lc&lcStyleTyp.COST4 , ;
        lc&lcStyleTyp.COST5 , lc&lcStyleTyp.TYPE1 , lc&lcStyleTyp.TYPE2 , ;
        lc&lcStyleTyp.TYPE3 , lc&lcStyleTyp.TYPE4 , lc&lcStyleTyp.TYPE5 , ;
        lcSBrowBmp , lcSOkBmp , lcSCanBmp , lcSAcptBmp , lcSClosBmp
        
PRIVATE laCostType , laClrClass , laColors9 , laColors10 , laItmClrs , ;
        laCodInfo  , laStySeg   , laAStySeg , laStyNSeg  , laStyCSeg , ;
        laDutyable , laMfgCode , laScale

PRIVATE lcStyTag , lcFbrTag , lcSclTag , lcCodTag , ;
        llOpnStyle , llOpnFabrc , llOpnScale , llOpnCodes , ;
        lnStyRec , lnFbrRec , lnSclRec , lnCodRec


***********-- Save current alias & its record pointer. --***********
lcCurAlias = ALIAS()           && Save the current alias.
lnCurRecNo = RECNO(lcCurAlias) && Save the current alias record pointer.

***********-- Prepare the needed tags & save current alias. --*********
**************-- Check if the needed files open or not. --*************
********-- Restore the records pointers in the opened files. --********

*-- Variables to handle saving the previous tags.
STORE  "" TO lcStyTag , lcFbrTag , lcSclTag , lcCodTag

*-- Flags to handle saving the checking the openning of the files on this session.
STORE .F. TO llOpnStyle , llOpnFabrc , llOpnScale , llOpnCodes , ;
             llOpenCur , llOpenExRt

*-- Variables hold the record pointer in the opened files.
STORE  0  TO lnStyRec , lnFbrRec , lnSclRec , lnCodRec

*B127371,1 EIH 05/31/2005 Declare array holding descriptions of same type cost [Begin]
DECLARE laCstDesc[6,2]
*B127371,1 EIH 05/31/2005 [End]


*E301077,16 Reham On 01/03/99   *** Begin ***
*E301077,16 Open the style segments file.
=gfOpenFile(gcDataDir+"ICSEGVAL" , gcDataDir+"SEGVAL" , "SH")
*E301077,16 Reham On 01/03/99   *** End   ***

IF !USED("STYLE")
  *-- Open the style file if was not open & set flag to close it at the end.
  =gfOpenFile(gcDataDir+"STYLE" , gcDataDir+"CSTYLE" , "SH")
  llOpnStyle = .T.
ELSE
  lcStyTag   = ORDER("STYLE")    && Save the style tag.
  lnStyRec   = RECNO("STYLE")    && save the style record pointer.
  SET ORDER TO CSTYLE IN STYLE
  llOpnStyle = .F.
ENDIF

IF !USED("FABRIC")
  *-- Open the fabric file if was not open & set flag to close it at the end.
  =gfOpenFile(gcDataDir+"FABRIC" , gcDataDir+"CFABRIC" , "SH")
  llOpnFabrc = .T.
ELSE
  lcFbrTag   = ORDER("FABRIC")    && Save the fabric tag.
  lnFbrRec   = RECNO("FABRIC")    && save the fabric record pointer.
  SET ORDER TO CFABRIC IN FABRIC
  llOpnFabrc = .F.
ENDIF

IF !USED("SCALE")
  *-- Open the scale file if was not open & set flag to close it at the end.
  =gfOpenFile(gcDataDir+"SCALE" , gcDataDir+"SCALE" , "SH")
  llOpnScale = .T.
ELSE
  lcSclTag   = ORDER("SCALE")    && Save the scale tag.
  lnSclRec   = RECNO("SCALE")    && save the scale record pointer.
  SET ORDER TO SCALE IN SCALE
  llOpnScale = .F.
ENDIF

IF !USED("CODES")
  *-- Open the code file if was not open & set flag to close it at the end.
  =gfOpenFile(gcDataDir+"CODES" , gcDataDir+"cCode_No" , "SH")
  llOpnCodes = .T.
ELSE
  lcCodTag   = ORDER("CODES")    && Save the codes tag.
  lnCodRec   = RECNO("CODES")    && save the code record pointer.
  SET ORDER TO cCode_No IN CODES
  llOpnCodes = .F.
ENDIF

IF lcStyleTyp = 'I'
  IF !USED("SycCurr")
    *-- Open the currency file if was not open & set flag to close it at the end.
    =gfOpenFile(gcSysHome+'SycCurr' ,'cCurrCode','SH')
    llOpenCur = .T.
  ELSE
    SET ORDER TO cCurrCode IN SYCCURR
    llOpenCur = .F.
  ENDIF
  
  IF !USED("SycExch")
    *-- Open the exchange file if was not open & set flag to close it at the end.
    =gfOpenFile(gcSysHome+'SycExch' ,'Currency','SH')
    llOpenExRt = .T.
  ELSE
    SELECT SycExch
    llOpenExRt = .F.
  ENDIF
  SET ORDER TO TAG Currency DESCENDING
ENDIF

**********-- Define variables related to the style structure --**********
STORE '' TO  lcStyleHdr , lcSeprator , lcNMjrPic , lcMajorPic , ;
             lcStyMask , lcNonMjHdr , lcItemExp , lcSizeSep
STORE  0  TO lnItemLen , lnMajorLen , lnNonMjLen , lnColorStr , ;
             lnColorLen , lnSizePos , lnSizeLen
STORE .F. TO llNonMjExt , llColorExt , llExtSizSc
*E301411,1 SSH 31/05/20 [Begin] New Variable to indicate if we save the 
*E301411,1 SSH                  current selected Mfg Code or not.
llNewSess = .T.
*E301411,1 SSH [Begin] Check If the PW module Instaled.
*E301411,1 SSH         IF installed and the current style type is MF.
*E301411,1 SSH         Start Get Temp Files Name.
*E301411,1 SSH         And Define new variable llNewSess to indicate
*E301411,1 SSH         if we are modifing in the same session.
*B604641,1 SSH Use the module variable instead of installed modules variable.
*llPwInst = (OCCURS('PW',gcCmpModules)<>0)
llPwInst = (OCCURS('PW',gcComp_Mdl)<>0)
*B604641,1 SSH [END]
IF llPwInst .AND. lcStyleTyp = 'M'
  *--- Check the type if <> 'C' ==> We call this program from C/T Cost Shet
  IF TYPE('lcDetLin') <> 'C' .OR. llFromCtk
    lcDetLin  = gfTempName()
    lcPWBom   = gfTempName()
    lcPWTol   = gfTempName()
    llNewSess = .T.
  ENDIF
ENDIF
*E301411,1 SSH [End]
*E301411,1 SSH 31/05/20 [End]
*******************-- Define the setup variables. --*********************
STORE .F. TO llTrimInv , llMulCurr
*llDyelot = .F.
**************-- Define the screen objects & variables. --***************
STORE '' TO  lcNStyItem, lcNFabItem, lcMCsItmDs, lcFCsItmDs, lcSCsItmDs, ;
             lcMfgCode, lcCsItmC9, lcCsItmC10, lcItmClrs, lcNUom6, lcNUom7, ;
             lcNMfgOpr, lcBsCrSmb6 , lcBsCrSmb7 , lcCrSmbA6 , lcCrSmbA7 , ;
             lcCrSmbB6 , lcCrSmbB7 , lcNDutyable , lcDutyDesc , lcPricDesc

STORE  0 TO  lnNEsUnt6A, lnNEsUnt6B, lnNUntQt6A, lnNUntQt6B, ;
             lnNEstUnt7, lnNUntQty7, lnNWstg6, lnNWstg7, ;
             lnNUntCst6, lnNUntCst7, lnNCstPrc6, lnNCstPrc7, ;
             lnTotFCst6, lnTotFCst7, lnTotEqu6 , lnTotEqu7 , ;
             lnOldClrCls

STORE  1  TO lnItmClrs, lnClrClass, lnNMfgCode
STORE  1  TO lnCostType , lnCurrUnit , ibCostType , ibClrCls9 , ;
             ibClrCls10 , ibItmClr9 , ibItmClr10 , puSelVal , ;
             puStyNSeg , puNMfgOpr , lnExRate6 , lnExRate7
STORE .F. TO llBrowse , llStyDye , llItmTrInv , llBMfgOpr, ;
             llMfgOpr , llCancel , cbPrim , llDifColor , llSame

STORE '*' TO lcRateSign  && Default the rate sign to multiply.
STORE '/' TO lcUnitSign  && Default the unit sign to divide.
STORE  1  TO ibNDutyabl

lcCstType  = 'F'
lcIClrText = IIF(lcCstType = 'S' , lcNonMjHdr , 'Item Color')
lcStColPop = IIF(lcStyleTyp <> "T" , IIF(llNonMjExt , lcNonMjHdr , 'Style') , "COLOR")

*-- Define the cost items labels, types & cost variables
FOR lnCount = 1 TO IIF(lcStyleTyp = 'T' , 4 , 5)
  lcCount  = STR(lnCount,1)
  STORE "" TO lc&lcStyleTyp.SLBL&lcCount , lc&lcStyleTyp.COST&lcCount , ;
              lc&lcStyleTyp.TYPE&lcCount
ENDFOR

******************-- Adjust the unsent parameters --*******************

*-- Define the cost sheet by size parameter.
IF TYPE("llOnSize") = "L"
  llOnSize = .T.
ELSE
  llOnSize = IIF(llOnSize="Y" , .T. , .F.)
ENDIF

*-- Define the no. of lines in the temp. bom file.
IF TYPE('lnBomLnNo') = 'U'
  lnBomLnNo = 0
ENDIF

*-- Define the primary fabric variable & flag in case not material cost sheet.
IF lcStyleTyp <> "T"
  IF TYPE("lcPrimFbr") = "L" .OR. TYPE("lcPrimFbr") = "U"
    lcPrimFbr = " "
    llPrimFbr = .F.
  ELSE
    llPrimFbr = .T.
  ENDIF
ELSE
  lcPrimFbr = " "
  llPrimFbr = .F.
ENDIF

*-- Define the MFG operation array if not defined.
IF TYPE('laOprCode') = 'U'
  *-- Collect the mfg operation from the temp bom file.
  DECLARE laOprCode[1,2]
  laOprCode = ""
  *B603351,1 SSH Enhancement performance. *** Begin ***
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
  *B603351,1 SSH Enhancement performance. *** End ***

  *-- If there is mfg operation found, add a row for "Select Operation" text.
  IF !EMPTY(laOprCode[1,1])
    DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
    =AINS(laOprCode,1)
  ENDIF
  laOprCode[1,1] = "Select Operation"
  laOprCode[1,2] = SPACE(6)
  
  *-- Delete all the mfg operation that has the related field not 
  *-- mfg operation = false.
  IF !EMPTY(laOprCode[1,1])
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
            DECLARE laOprCode[ALEN(laOprCode,1)-1,2]
          ELSE
            DECLARE laOprCode[1,2]
          ENDIF
        ELSE
          lnHidnCnt = lnCnt
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
ENDIF

**************************-- Define Arrays --****************************
*-- Initialize the needed variables for the new cost sheet item.
DECLARE laSegSorc1[1] , laSegTrgt1[1] , laSegSorc2[1] , laSegTrgt2[1] , ;
        laSegSorc3[1] , laSegTrgt3[1] , laSegSorc4[1] , laSegTrgt4[1] , ;
        laSegSorc5[1] , laSegTrgt5[1]

STORE "" TO laSegSorc1 , laSegTrgt1 , laSegSorc2 , laSegTrgt2 , ;
            laSegSorc3 , laSegTrgt3 , laSegSorc4 , laSegTrgt4 , ;
            laSegSorc5 , laSegTrgt5

*B602213,1 Reham On 11/17/98   *** Begin ***
*B602213,1 Increase the style segment array columns to be 9 instead of 8
*DECLARE laCostType[IIF(lcStyleTyp='T',4,5),2] , laClrClass[3,2] , ;
        laColors9[1,4] , laColors10[1,4] , laItmClrs[1,4] , ;
        laCodInfo[1,10] , laStySeg[1,1] , laAStySeg[1,8] , ;
        laStyNSeg[1,8] , laStyCSeg[1,8] , laDutyable[1,1] , ;
        laMfgCode[1]
DECLARE laCostType[IIF(lcStyleTyp='T',4,5),2] , laClrClass[3,2] , ;
        laColors9[1,4] , laColors10[1,4] , laItmClrs[1,4] , ;
        laCodInfo[1,10] , laStySeg[1,1] , laAStySeg[1,9] , ;
        laStyNSeg[1,9] , laStyCSeg[1,9] , laDutyable[1,1] , ;
        laMfgCode[1]
*B602213,1 Reham On 11/17/98   *** End   ***

************************-- Define bitmaps path --************************
*-- Get the prompt of the available pictures on the screen.
lcSBrowBmp = gcBmpHome + "ExtKey.Bmp"
lcSOkBmp   = gcBmpHome + "Ok.Bmp"
lcSCanBmp  = gcBmpHome + "Can.Bmp"
lcSAcptBmp = gcBmpHome + "Accept.Bmp"
lcSClosBmp = gcBmpHome + "Cls.Bmp"

*********************-- Define the base file. --*************************
lcItemFile = IIF(lcStyleTyp = 'T' , 'Fabric' , 'Style')

************************-- Define temp. file. --*************************
*-- Create temp. name for the mask cursor.
lcTmpMask  = gfTempName()
*-- Create temp. name for file to view items.
lcTmpItmNw = gfTempName()

*-- Cursor hold all the masks.
CREATE CURSOR (lcTmpMASK) (cItmMASK C(19) , Item C(19) , IClr C(6) , nCost N(7,3), mSizes M , mSzCrosRef M , lDelete L)

CREATE DBF (gcWorkDir+lcTmpItmNw) (cItem C(19) , cInclud C(1) , cDesc C(60) , cCstItm C(19))
INDEX ON cItem TAG (lcTmpItmNw)

***********************-- Define needed setups. --***********************
*-- Call function to get the default setups.
=lfGetSetup()

***********************-- Define the style info. --**********************
*-- Call function to get the style code strucure.
=lfStylInfo()

*************-- Check if current item is dyelot yes or no. --************
IF SEEK(PADR(lcMajor,lnItemLen),lcItemFile)
  llStyDye  = (&lcItemFile..cDye_Flg ='Y')
  lcPricCur = IIF(lcStyleTyp = "I" , Style.cPriceCur , gcBaseCurr)
  lcDutyCur = IIF(lcStyleTyp = "I" , Style.cDutyCur , gcBaseCurr)
ELSE
  llStyDye  = .F.
  STORE gcBaseCurr TO lcPricCur , lcDutyCur
ENDIF

***********-- Define array to be used in the segments popup. --**********
DECLARE laAStySeg [ALEN(laStyNSeg,1) , 9]
=ACOPY(laStyNSeg , laAStySeg)

***********************-- Fill the colors array. --**********************
*-- Call function to fill the colors arrays.
=lfFillArry()

***************-- Adjust the needed variables & arrays. --***************
STORE REPLICATE('*' , lnNonMjLen) TO lcItmClrs , lcCsItmC9
lcCsItmC10 = IIF(lcStyleTyp<>"T" AND !llColorExt , SPACE(6) , "******")
lcCostType = laCostType[1,1]
lnCostType = laCostType[1,2]

*-- Fill the dutyable array.
=gfGetVld("CCOSTSTAT" , @laDutyable)

*C037892,1 MHM 06/15/2004 Validate PopUp For  sales order cost sheet [Start]
IF ASCAN(laEvntTrig,PADR('SODTYPOP',10))<>0
  =gfDoTriger('SOALCST',PADR('SODTYPOP',10))
ENDIF
*C037892,1 MHM [End]

*-- Fill the class popup array.
IF lcStyleTyp = "T"
  laClrClass[1,1] = 'Item Color Same As '+lcItemFile+' Color'
  laClrClass[2,1] = 'Item Color Always Constant'
  laClrClass[3,1] = lcItemFile +' Color Uses Different Item Colors'
ELSE
  laClrClass[1,1] = 'Item Color Same As Style ' + lcNonMjHdr
  laClrClass[2,1] = 'Item Color Always Constant'
  laClrClass[3,1] = 'Style ' + lcNonMjHdr + ' Uses Different Item Colors'
ENDIF
laClrClass[1,2] = 1
laClrClass[2,2] = 2
laClrClass[3,2] = 3

*E500402,1 AMH Initial color values of Marker field and lable [Start]
lcMarkrLbl = "RGB(192,192,192,192,192,192)"
lcMarkrFld = ",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl"
lcMarkr3DG = lcMarkrLbl
lcMarkr3DW = lcMarkrLbl
STORE SPACE(0) TO lcMarker6, lcMarker7

*B604899,1 AMH Define variable need to update Cathy Daniels custom fields [Start]
IF ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
  STORE 0 TO lnMark_Len, lnOut_Goods, lnTot_Units, lnLoss
ENDIF
*B604899,1 AMH [End]

*E500402,1 AMH [End]

*******************-- Call the new cost item screen. --******************
DO (gcScrDir+"MFCstSC.SPX")

*****************-- Close the cursor & the temp. file. --****************
CLEAR TYPEAHEAD             && Clear the fox buffer.

USE IN (lcTmpMask)          && Close the cursor.

IF USED(lcTmpItmNw)         && Close the temp. file & erase it.
  USE IN (lcTmpItmNw)
ENDIF
ERASE &gcWorkDir.&lcTmpItmNw..DBF
ERASE &gcWorkDir.&lcTmpItmNw..CDX

*********-- Restore the previous tags & the record pointers. --**********
*******-- And close the files that was opened in current session --******
IF llOpnStyle
  =gfCloseFile('STYLE')
ELSE
  SET ORDER TO &lcStyTag IN STYLE
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOTO lnStyRec IN STYLE
  ENDIF
ENDIF

IF llOpnFabrc
  =gfCloseFile('FABRIC')
ELSE
  SET ORDER TO &lcFbrTag IN FABRIC
  IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
    GOTO lnFbrRec IN FABRIC
  ENDIF
ENDIF

IF llOpnScale
  =gfCloseFile('SCALE')
ELSE
  SET ORDER TO &lcSclTag IN SCALE
  IF lnSclRec > 0 .AND. lnSclRec <= RECCOUNT("SCALE")
    GOTO lnSclRec IN SCALE
  ENDIF
ENDIF

IF llOpnCodes
  =gfCloseFile('CODES')
ELSE
  SET ORDER TO &lcCodTag IN CODES
  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT("CODES")
    GOTO lnCodRec IN CODES
  ENDIF
ENDIF

IF lcStyleTyp = 'I'
  IF llOpenCur
    =gfCloseFile('SYCCURR')
  ENDIF
  
  IF llOpenExRt
    =gfCloseFile('SycExch')
  ELSE
    SELECT SycExch
    SET ORDER TO TAG Currency DESCENDING
  ENDIF
ENDIF

*E301077,16 Reham On 01/03/99   *** Begin ***
*E301077,16 Close the style segments file.
=gfCloseFile('ICSEGVAL')
*E301077,16 Reham On 01/03/99   *** End   ***

*********************-- Restore the previous alias. --*******************
SELECT (lcCurAlias)
IF lnCurRecNo > 0 .AND. lnCurRecNo <= RECCOUNT(lcCurAlias)
  GOTO lnCurRecNo
ENDIF

*!***************************************************************
************* Functions to prepare needed variables *************
*************** Screen to add New cost sheet item ***************
*!***************************************************************

*!*************************************************************
*! Name      : lfFillArry
*! Developer : Reham Al-Allamy
*! Date      : 08/14/1997
*! Purpose   : Fill the colors' arrays.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfFillArry()
*!*************************************************************
*
FUNCTION lfFillArry

*B603351,1 SSH Private variables .
PRIVATE lnOldOrd , lnDelSet , lnOldAls
*B603351,1 SSH Enhancement performance. *** End ***

*-- Create colors arrays.
IF lcStyleTyp = 'T'
  SELECT FABRIC
*B603351,1 SSH Activate rushmore. *** Begin ***
  lnDelSet = SET('DELETE')
  SET ORDER TO 
  SET DELETE OFF
*  SELECT DISTINCT Color , 1 , SPACE(6) , 0 ;
    FROM FABRIC ;
   WHERE FABRIC.Fabric = lcMajor .AND. ;
         !EMPTY(FABRIC.Color) .AND. FABRIC.MAKE ;
    INTO ARRAY laItmClrs

  SELECT DISTINCT Color , 1 , SPACE(6) , 0 ;
    FROM FABRIC ;
   WHERE FABRIC.Fabric+FABRIC.COLOR = lcMajor .AND. ;
         !EMPTY(FABRIC.Color) .AND. FABRIC.MAKE .AND. !DELETED();
    INTO ARRAY laItmClrs
   SET DELETE &lnDelSet
*B603351,1 SSH Activate rushmore. *** End ***
  SET ORDER TO CFABRIC
ELSE
  *-- If there is non major for the style structure.
  IF llNonMjExt
    *-- If there is extended color scale, do not get the scales in the array.
    IF llExtSizSc
      *B603351,1 SSH Activate rushmore. *** Begin ***
      *SELECT DISTINCT SUBSTR(Style , lnMajorLen+2, lnNonMjLen) , 1 , SPACE(lnNonMjLen) , 0 ;
        FROM STYLE ;
       WHERE cStyMajor = lcMajor .AND. !EMPTY(SUBSTR(Style,lnMajorLen+2));
        INTO ARRAY laItmClrs
      
      lnOldAls = SELECT(0)
      SELECT Style
      lnOldOrd = ORDER()
      lnDelSet = SET('DELETE')
      SET ORDER TO 
      SET DELETE OFF
      SELECT DISTINCT SUBSTR(Style , lnMajorLen+2, lnNonMjLen) , 1 , SPACE(lnNonMjLen) , 0 ;
        FROM STYLE ;
       WHERE STYLE = PADR(lcMajor,lnMajorLen) .AND. !EMPTY(SUBSTR(Style,lnMajorLen+2))  .AND. !DELETED();
        INTO ARRAY laItmClrs
      SELECT Style
      SET ORDER TO lnOldOrd
      SET DELETE &lnDelSet
      SELECT(lnOldAls)
      *B603351,1 SSH Activate rushmore. ***End***

    ELSE
      *-- If there is no extended color scale, get the whole non-major.
      *B603351,1 SSH Activate rushmore. *** Begin ***
      *SELECT DISTINCT SUBSTR(Style,lnMajorLen+2) , 1 , SPACE(lnNonMjLen) , 0 ;
        FROM STYLE ;
       WHERE cStyMajor = lcMajor .AND. !EMPTY(SUBSTR(Style,lnMajorLen+2));
        INTO ARRAY laItmClrs
      lnOldAls = SELECT(0)
      SELECT Style
      lnOldOrd = ORDER()
      lnDelSet = SET('DELETE')
      SET ORDER TO 
      SET DELETE OFF
      SELECT DISTINCT SUBSTR(Style,lnMajorLen+2) , 1 , SPACE(lnNonMjLen) , 0 ;
        FROM STYLE ;
       WHERE STYLE = PADR(lcMajor,lnMajorLen) .AND. !EMPTY(SUBSTR(Style,lnMajorLen+2))  .AND. !DELETED();
        INTO ARRAY laItmClrs
      SELECT Style
      SET ORDER TO lnOldOrd
      SET DELETE &lnDelSet
      SELECT(lnOldAls)
      *B603351,1 SSH Activate rushmore. ***End***
    ENDIF
  ELSE
    DECLARE laItmClrs[1,4]
    laItmClrs = ""
  ENDIF
ENDIF

*-- If entering material cost sheet or style cost sheet with 
*-- non major in its structure, prepare the non majors array.
IF lcStyleTyp = 'T' .OR. llNonMjExt
  *-- Create colors Arrays.
  DIMENSION laColors9[ALEN(laItmClrs,1),4]
  IF lcStyleTyp <> "T"
    DIMENSION laColors10[1,4]
  ELSE
    DIMENSION laColors10[ALEN(laItmClrs,1),4]
  ENDIF
  STORE "" TO laColors9 , laColors10
  
  *-- Fill the colors' arrays with the right values.
  FOR lnCount = 1 TO ALEN(laItmClrs,1)
    laItmClrs[lnCount,2]   = lnCount
    
    laColors9[lnCount,1]   = laItmClrs[lnCount,1]
    laColors9[lnCount,2]   = lnCount
    laColors9[lnCount,3]   = SPACE(6)
    laColors9[lnCount,4]   = 0
    
    IF lcStyleTyp = "T"
      laColors10[lnCount,1]  = laItmClrs[lnCount,1]
      laColors10[lnCount,2]  = lnCount
      laColors10[lnCount,3]  = SPACE(6)
      laColors10[lnCount,4]  = 0
    ELSE
      lcTmpClr = SUBSTR(SPACE(lnMajorLen+1) + laItmClrs[lnCount,1] , lnColorStr , lnColorLen)
      IF ASCAN(laColors10 , lcTmpClr) = 0
        IF !EMPTY(laColors10[1,1])
          DIMENSION laColors10[ALEN(laColors10,1)+1,4]
        ENDIF
        laColors10[ALEN(laColors10,1),1]  = lcTmpClr
        laColors10[ALEN(laColors10,1),2]  = ALEN(laColors10,1)
        laColors10[ALEN(laColors10,1),3]  = SPACE(6)
        laColors10[ALEN(laColors10,1),4]  = 0
      ENDIF
    ENDIF
  ENDFOR
ELSE
  DIMENSION laColors10[1,4] , laColors9[1,4]
  STORE "" TO laColors9 , laColors10
ENDIF

*!*************************************************************
*! Name      : lfGetSetup
*! Developer : Reham Al-Allamy
*! Date      : 10/20/1997
*! Purpose   : Get the default setups from the setup file.
*!*************************************************************
*! Calls     : gfGetMemVar
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetSetup()
*!*************************************************************
*
FUNCTION lfGetSetup

*** Get the needed information from the company setup. ***

*E301077,16 Reham On 02/22/99   *** Begin ***
*E301077,16 Reduce the calling of gfGetMemVar()
*-- System dyelot or not.
*llDyelot   = IIF(lcStyleTyp <> "T" , ;
                 ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp)) = 'Y' , ;
                 ALLTRIM(gfGetMemVar('M_MATDYE',gcAct_Comp)) = 'Y')

*-- Use trim items as non trim inventory.
*llTrimInv  = ALLTRIM(gfGetMemVar('M_TINVT',gcAct_Comp)) = 'Y'
*-- If the style strucure is extended size scale or not.
*llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
*-- If the system is multi currency or not.
*llMulCurr = (lcStyleTyp='I') .AND. gfGetMemVar('llMulCurr' , gcAct_Comp)

*-- Define variables for labels, cost, type.
*FOR lnCount = 1 TO IIF(lcStyleTyp = 'T' , 4 , 5)
*  lcCount  = STR(lnCount,1)
*  lc&lcStyleTyp.SLBL&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'SLBL'+lcCount , gcAct_Comp)
*  lc&lcStyleTyp.COST&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'COST'+lcCount , gcAct_Comp)
*  lc&lcStyleTyp.TYPE&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'TYPE'+lcCount , gcAct_Comp)
*  laCostType[lnCount,1] = lc&lcStyleTyp.SLBL&lcCount
*  laCostType[lnCount,2] = lnCount
*  IF lc&lcStyleTyp.TYPE&lcCount = "D"
*    lcDutyDesc = lc&lcStyleTyp.COST&lcCount
*  ENDIF
*  IF lc&lcStyleTyp.TYPE&lcCount = "P"
*    lcPricDesc = lc&lcStyleTyp.COST&lcCount
*  ENDIF
*ENDFOR 

DECLARE laMainSetp[IIF(lcStyleTyp = 'T' , 15 , 18) ,2]
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

*B127371,1 EIH 05/31/2005 initialize array holding descriptions of same type cost [Begin]
STORE .F. TO laCstDesc
*B127371,1 EIH 05/31/2005 [End]

FOR lnCount = 4 TO IIF(lcStyleTyp = 'T' , 15 , 18) STEP 3
  lcNo = STR(lnNo,1)
  lc&lcStyleTyp.SLBL&lcNo = laMainSetp[lnCount  ,2]
  lc&lcStyleTyp.COST&lcNo = laMainSetp[lnCount+1,2]
  lc&lcStyleTyp.TYPE&lcNo = laMainSetp[lnCount+2,2]
  laCostType[lnNo,1] = lc&lcStyleTyp.SLBL&lcNo
  laCostType[lnNo,2] = lnNo
  
  *B127371,1 EIH 05/31/2005 Filling the array holding descriptions of same type cost [Begin]
  *IF lc&lcStyleTyp.TYPE&lcNo = "D"
  *  lcDutyDesc = lc&lcStyleTyp.COST&lcNo
  *ENDIF
  *IF lc&lcStyleTyp.TYPE&lcNo = "P"
  *  lcPricDesc = lc&lcStyleTyp.COST&lcNo
  *ENDIF

  *T20060906.0003 (B607793 ) TMI [Start] update the 1st column of the array with a numeric value such that the code does not crash 
  laCstDesc[lnNo,1] = lnNo
  laCstDesc[lnNo,2] = ''
  *T20060906.0003 (B607793 ) TMI [End  ] 
  
  IF lc&lcStyleTyp.TYPE&lcNo $ "DP"
    laCstDesc[lnNo,1] = lnNo
    laCstDesc[lnNo,2] = lc&lcStyleTyp.COST&lcNo
  ENDIF
  *B127371,1 EIH 05/31/2005  [End]
  
  lnNo = lnNo + 1
ENDFOR
*E301077,16 Reham On 02/22/99   *** End   ***

*-- Get the base currency symbole.
IF llMulCurr
  =SEEK(gcBaseCurr,'SycCurr')  
  STORE SycCurr.cCurrSmbl TO lcBsCrSmb6 , lcBsCrSmb7 , lcCrSmbA6 , ;
                             lcCrSmbA7 , lcCrSmbB6 , lcCrSmbB7
ENDIF

*-- Array hold the codes info. (MFG Code)
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

*!*************************************************************
*! Name      : lfStylInfo
*! Developer : Reham Al-Allamy
*! Date      : 10/20/1997
*! Purpose   : Prepare the needed style information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfStylInfo()
*!*************************************************************
*
FUNCTION lfStylInfo

*B602539,1 Reham On 02/21/99  *** Begin ***
*-- Get the style header.
*lcStyleHdr = gfItemMask("HI")
lcStyleHdr = IIF(lcStyleTyp='T' , "Material" , gfItemMask("HI"))
*B602539,1 Reham On 02/21/99  *** End   ***

*-- Get the non major picture.
lcNMjrPic  = IIF(lcStyleTyp='T' , "XXXXXX" , gfItemMask("PN"))

*-- Get the major length or the material legnth.
lnItemLen  = LEN(IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM")))

*B602539,1 Reham On 02/21/99  *** Begin ***
*-- Picture of the style major.
*lcMajorPic = gfItemMask("PM")
lcMajorPic = IIF(lcStyleTyp='T' , "XXXXXXX" , gfItemMask("PM"))
*B602539,1 Reham On 02/21/99  *** End   ***

*-- Lenght of the the style major.
lnMajorLen = LEN(lcMajorPic)

*B602539,1 Reham On 02/21/99  *** Begin ***
*-- Get the style mask separator.
*lcSeprator = SUBSTR(lcStyleHdr , lnMajorLen+1 , 1)
lcSeprator = IIF(lcStyleTyp='T' , "" , SUBSTR(lcStyleHdr , lnMajorLen+1 , 1))

*-- Get the non-major lenght.
*lnNonMjLen = LEN(gfItemMask("PN"))
lnNonMjLen = IIF(lcStyleTyp='T' , 6 , LEN(gfItemMask("PN")))

*-- Get the whole style picture.
*lcStyMask  = STRTRAN(gfItemMask("PI") , "X" , "*")
lcStyMask  = IIF(lcStyleTyp='T' , "*******" , STRTRAN(gfItemMask("PI") , "X" , "*"))
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
  STORE "" TO laStyCSeg , laStyNSeg
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
      
      IF lnMajorLen <= laStySeg[lnCnt,4]
        IF !EMPTY(laStyNSeg[1,1])
          *B602213,1 Reham On 11/17/98    *** Begin ***
          *B602213,1 Increase the style segment array columns to be 9 instead of 8
          *DECLARE laStyNSeg[ALEN(laStyNSeg,1)+1,8]
          DECLARE laStyNSeg[ALEN(laStyNSeg,1)+1,9]
          *B602213,1 Reham On 11/17/98    *** End   ***
        ENDIF
        
        *B602132,1 Reham On 12/17/98    *** Begin ***
        *B602132,1 If the long description for the segment was empty, display the short description.
        *laStyNSeg[ALEN(laStyNSeg,1),1] = IIF(!EMPTY(laStySeg[lnCnt , 5]) , laStySeg[lnCnt , 5] , "Segment # "+STR(lnCnt,1)) && Long Description
        laStyNSeg[ALEN(laStyNSeg,1),1] = IIF(!EMPTY(laStySeg[lnCnt , 5]) , laStySeg[lnCnt , 5] , laStySeg[lnCnt , 2])
        *B602132,1 Reham On 12/17/98    *** End   ***
        
        laStyNSeg[ALEN(laStyNSeg,1),2] = laStySeg[lnCnt , 1] && Type
        laStyNSeg[ALEN(laStyNSeg,1),3] = laStySeg[lnCnt , 3] && Picture
        laStyNSeg[ALEN(laStyNSeg,1),4] = laStySeg[lnCnt , 4] && Start Position
        laStyNSeg[ALEN(laStyNSeg,1),5] = laStySeg[lnCnt , 2] && Short desc.
        laStyNSeg[ALEN(laStyNSeg,1),6] = laStySeg[lnCnt , 6] && Separator
        laStyNSeg[ALEN(laStyNSeg,1),7] = laStySeg[lnCnt , 7] 
        laStyNSeg[ALEN(laStyNSeg,1),8] = "A"      && All Values or Selected values.
        *B602213,1 Reham On 11/17/98    *** Begin ***
        *B602213,1 define array column to hold the segment position # in 
        *B602213,1 the style code structure.
        laStyNSeg[ALEN(laStyNSeg,1),9] = lnCnt    && Hold the segment position in the style code structure.
        *B602213,1 Reham On 11/17/98    *** End   ***
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
    laStyCSeg[ALEN(laStyCSeg,1),9] = lnCnt    && Hold the segment position in the style code structure.
    *B602213,1 Reham On 11/17/98    *** End   ***
  ENDFOR
*B602539,1 Reham On 02/21/99  *** Begin ***
ENDIF
*B602539,1 Reham On 02/21/99  *** End   ***

*-- Adjust the non major info if there is extended size scale.
IF llExtSizSc .AND. lcStyleTyp <> "T"
  lnNonMjLen = lnNonMjLen - lnSizeLen
  *B602385,1 Reham On 01/05/99   *** Begin ***
  *B602385,1 If there is no major except the scale, consider there is no non-major.
  IF lnNonMjLen < 0
    lnNonMjLen = 0
    llNonMjExt = .F.
  ENDIF
  *B602385,1 Reham On 01/05/99   *** End   ***
  lcNMjrPic  = SUBSTR(lcNMjrPic  , 1 , (LEN(lcNMjrPic )-lnSizeLen))
  lcNonMjHdr = SUBSTR(lcNonMjHdr , 1 , lnNonMjLen)
ENDIF

*!***************************************************************
***************    Functions for MFCSTSC Screen   ***************
*************** Screen to add New cost sheet item ***************
*!***************************************************************

*!*************************************************************
*! Name      : lfShwCSh_C
*! Developer : Reham Al-Allamy
*! Date      : 08/20/1997
*! Purpose   : Show function for screen : MFCSTSHC
*!           :                          : (New cost sheet item)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfShwCSh_C()
*!*************************************************************
*
FUNCTION lfShwCSh_C

*-- Screen to validate cost item type.
SHOW GETS WINDOW MFCstSC0 ENABLE ONLY

*-- Screen hold the whole objects.
SHOW GETS WINDOW MFCstSC1 DISABLE ONLY
SHOW GET lcStColPop ENABLE
SHOW GET lcIClrText ENABLE

*-- Screen to validate fabric or trim items.
SHOW GETS WINDOW MFCstSC3 ENABLE ONLY
SHOW WINDOW MFCstSC3 TOP

*-- Screen to validate Mfg. Operations codes.
SHOW GETS WINDOW MFCstSC2 DISABLE ONLY
*-- Screen to validate Style component items.
SHOW GETS WINDOW MFCstSC4 DISABLE ONLY
*-- Screen to validate the duty description or display price description.
SHOW GETS WINDOW MFCstSC5 DISABLE ONLY

SHOW GETS WINDOW MFCstSC6 DISABLE ONLY
SHOW GETS WINDOW MFCstSC7 DISABLE ONLY
*E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of lcStyleTyp <> "T" 
*E500348,1                (Material cost sheet).
*IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
IF !EMPTY(laAStySeg[1,1])
*E500348,1 KHM 06/04/2000 (End)
  SHOW WINDOW MFCstSC7 TOP
ELSE
  SHOW WINDOW MFCstSC6 TOP
ENDIF

SHOW GETS WINDOW MFCstC10 DISABLE ONLY
SHOW WINDOW MFCstC10 TOP
SHOW GETS WINDOW MFCSTSC9 DISABLE ONLY

SHOW GET pbClsNew,1  PROMPT lcSClosBmp ENABLE
SHOW GET pbAcptNew DISABLE

*E500402,1 AMH Refreshing the color values of Marker Field and lable [Start]
lcMarkrLbl = "RGB(192,192,192,192,192,192)"
lcMarkrFld = ",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl"
lcMarkr3DG = lcMarkrLbl
lcMarkr3DW = lcMarkrLbl
STORE SPACE(0) TO lcMarker6, lcMarker7
=lfrefresh()
IF !EMPTY(laAStySeg[1,1])
  SHOW GET lcMarker7   ENABLE
ELSE
  SHOW GET lcMarker6   ENABLE
ENDIF
*E500402,1 AMH [End]

*!*************************************************************
*! Name      : lfvCostType
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate Cost type popup.
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvCostType()
*!*************************************************************
*
FUNCTION lfvCostType
PRIVATE lnStyRec

*-- Get current cost type.
lnCostType = laCostType[ibCostType,2]

IF EMPTY(lnCostType) .OR. LASTKEY() = 27
  RETURN
ENDIF

SHOW GET ibBegin DISABLE

STORE 1 TO ibClrCls9 , ibClrCls10
lcCstType = EVAL('lc'+lcStyleTyp+'Type'+STR(lnCostType,1))

*-- If the cost item type is (Mfg, Price or Duty), use the array 
*-- that has the color segment.
IF lcCstType $ "MPD"
  *E500348,1 KHM 06/04/2000 (Begin) Initializing laAStySeg array in case of
  *E500348,1                Material Cost sheet.     
  *DECLARE laAStySeg [ALEN(laStyCSeg,1) , ALEN(laStyCSeg,2)]
  *=ACOPY(laStyCSeg , laAStySeg)
  IF lcStyleTyp = "T"
    DECLARE laAStySeg [1, 9]
    laAStySeg [1, 1] = "Color"
    laAStySeg [1, 2] = "C"
    laAStySeg [1, 8] = "A"
  *E500348,1 KHM 06/04/2000 (End)
  ELSE  
    DECLARE laAStySeg [ALEN(laStyCSeg,1) , ALEN(laStyCSeg,2)]
    =ACOPY(laStyCSeg , laAStySeg)
  ENDIF
ELSE
  *-- If the cost item type is (Style, Fabric or Trim), use the array 
  *-- that does not have the color segment.
  DECLARE laAStySeg [ALEN(laStyNSeg,1) , ALEN(laStyNSeg,2)]
  =ACOPY(laStyNSeg , laAStySeg)
ENDIF
SHOW GET puStyNSeg

lcCostType = IIF(lcCstType = "S" , lcItemExp , laCostType[ibCostType,1])

DO CASE
  *-- If the cost type was purchase price or duty.
  CASE lcCstType $ 'PD'
    lcMfgCode  = IIF(lcCstType = 'P' , "******" , '######')
    *B127371,1 EIH 05/31/2005 Set variable lcMCsItmDs with valid value of description [Begin]
    *lcMCsItmDs = IIF(lcCstType = 'P' , lcPricDesc , lcDutyDesc)
    STORE 1 TO liCounter
    FOR liCounter = 1 TO  6 
      IF  lnCostType = laCstDesc[liCounter,1] 
        lcMCsItmDs = laCstDesc[liCounter,2] 
        EXIT
      ENDIF
    ENDFOR
    *B127371,1 EIH 05/31/2005 [End]
    lcIClrText = 'Item Color'
    SHOW GETS WINDOW MFCstSC5 ENABLE ONLY
    SHOW WINDOW MFCstSC5 TOP
    SHOW GETS WINDOW MFCstSC2 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC3 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC4 DISABLE ONLY
    
    SHOW WINDOW MFCstC10 TOP
    SHOW GETS WINDOW MFCSTSC9 DISABLE ONLY
    SHOW GETS WINDOW MFCSTC10 DISABLE ONLY
    
    STORE   1   TO lnNEsUnt6A, lnNEsUnt6B, lnNUntQt6A, lnNUntQt6B, lnNEstUnt7, lnNUntQty7
    STORE   0   TO lnNWstg6 , lnNWstg7
    STORE 'EAC' TO lcNUom6    , lcNUom7
    IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
      puStyNSeg = 1
      SHOW GET puStyNSeg  ENABLE
      SHOW GET puSelVal   ENABLE
      SHOW GET lnNEstUnt7 DISABLE
      SHOW GET lnNUntQty7 DISABLE
      SHOW GET lnNWstg7   DISABLE
      SHOW GET lcNUom7    ENABLE
      SHOW GET lnNUntCst7 ENABLE
      SHOW WINDOW MFCstSC7 TOP
      SHOW GETS WINDOW MFCstSC6 DISABLE ONLY
    ELSE
      SHOW GET lnNEsUnt6B DISABLE
      SHOW GET lnNEsUnt6A ENABLE
      SHOW GET lnNUntQt6A ENABLE
      SHOW GET lnNWstg6   DISABLE
      SHOW GET lcNUom6    ENABLE
      SHOW GET lnNUntCst6 ENABLE
      SHOW WINDOW MFCstSC6 TOP
      SHOW GETS WINDOW MFCstSC7 DISABLE ONLY
    ENDIF
    
    IF lcCstType = 'D'
      ibNDutyabl = 1
      lcNDutyable = laDutyable[ibNDutyabl,2]
      SHOW GET ibNDutyabl DISABLE
      SHOW GET lcMCsItmDs ENABLE
      _CUROBJ = OBJNUM(lcMCsItmDs)
    ELSE
      SHOW GET ibNDutyabl ENABLE
      SHOW GET lcMCsItmDs DISABLE
    ENDIF
    
    SHOW GET puNMfgOpr  ENABLE
  *-- If the current cost item was mfg operation.
  CASE lcCstType = 'M'
    =gfwCodePop(@laCodInfo, "MFGCODE", "D")
    lcMfgCode  = laMfgCode[lnNMfgCode,2]
    lcMCsItmDs = laMfgCode[lnNMfgCode,1]
    lcIClrText = 'Item Color'
    SHOW GETS WINDOW MFCstSC2 ENABLE ONLY
    SHOW WINDOW MFCstSC2 TOP
    SHOW GETS WINDOW MFCstSC3 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC4 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC5 DISABLE ONLY
    
    SHOW WINDOW MFCstC10 TOP
    SHOW GETS WINDOW MFCSTSC9 DISABLE ONLY
    SHOW GETS WINDOW MFCSTC10 DISABLE ONLY
    
    STORE   1   TO lnNEsUnt6A, lnNEsUnt6B, lnNUntQt6A, lnNUntQt6B, lnNEstUnt7, lnNUntQty7
    STORE   0   TO lnNWstg6 , lnNWstg7
    STORE 'EAC' TO lcNUom6 , lcNUom7
  
    *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
    *E500348,1                lcStyleTyp <> "T" from the IF command to do 
    *E500348,1                it in all cost sheet types.
    *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
    IF !EMPTY(laAStySeg[1,1])
    *E500348,1 KHM 06/04/2000 (End)
      puStyNSeg = 1
      SHOW GET puStyNSeg   ENABLE
      SHOW GET puSelVal    ENABLE
      *C200080,6  Reham On 08/11/1999   *** Begin ***
      *C200080,6  Enable the unit qty. & wastage if the the cost item is MFG.
      *SHOW GET lnNEstUnt7  DISABLE
      *SHOW GET lnNWstg7    DISABLE
      SHOW GET lnNEstUnt7  ENABLE
      SHOW GET lnNWstg7    ENABLE
      *C200080,6  Reham On 08/11/1999   *** End   ***
      SHOW GET lnNUntQty7  DISABLE
      SHOW GET lcNUom7     ENABLE
      SHOW GET lnNUntCst7  ENABLE
      SHOW WINDOW MFCstSC7 TOP
      SHOW GETS WINDOW MFCstSC6 DISABLE ONLY
      *E301411,1 SSH 31/05/20 [Begin] If PW module installed then
      *E301411,1 SSH          check to Enable/Desable Detail Operation Butt.
      IF llPwInst .AND. lcStyleTyp = 'M'
        =lfCheckPW()
      ENDIF
      *E301411,1 SSH 31/05/20 [End]
    ELSE
      *C200080,6  Reham On 08/11/1999   *** Begin ***
      *C200080,6  Enable the unit qty. & wastage if the the cost item is MFG.
      *SHOW GET lnNEsUnt6B  DISABLE
      *SHOW GET lnNWstg6    DISABLE
      SHOW GET lnNEsUnt6B  ENABLE
      SHOW GET lnNWstg6    ENABLE
      *C200080,6  Reham On 08/11/1999   *** End   ***
      SHOW GET lnNEsUnt6A  DISABLE
      SHOW GET lnNUntQt6A  DISABLE
      SHOW GET lcNUom6     ENABLE
      SHOW GET lnNUntCst6  ENABLE
      SHOW WINDOW MFCstSC6 TOP
      SHOW GETS WINDOW MFCstSC7 DISABLE ONLY
    ENDIF
    
    SHOW GET  ibNDutyabl ENABLE
    SHOW GET  puNMfgOpr  DISABLE
    _CUROBJ = OBJNUM(lnNMfgCode)
  *-- If the current cost item was style.
  CASE lcCstType = 'S'
    STORE 0 TO lnNWstg6 , lnNWstg7
    SHOW GETS WINDOW MFCstSC4 ENABLE ONLY
    SHOW WINDOW MFCstSC4 TOP
    SHOW GETS WINDOW MFCstSC2 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC3 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC5 DISABLE ONLY
    
    SHOW GET lnNEsUnt6A DISABLE
    SHOW GET lnNEsUnt6B ENABLE
    SHOW GET lnNUntQt6B ENABLE
    SHOW GET lnNWstg6   ENABLE
    SHOW GET lcNUom6    ENABLE
    SHOW GET lnNUntCst6 DISABLE
    SHOW WINDOW MFCstSC6 TOP
    SHOW GETS WINDOW MFCstSC7 DISABLE ONLY
    
    lcIClrText      = lcNonMjHdr
    laClrClass[1,1] = STRTRAN(laClrClass[1,1] , 'Item Color' , 'Style '+TRIM(lcNonMjHdr))
    laClrClass[2,1] = STRTRAN(laClrClass[2,1] , 'Item Color' , 'Style '+TRIM(lcNonMjHdr))
    laClrClass[3,1] = STRTRAN(laClrClass[3,1] , 'Item Color' , 'Style '+TRIM(lcNonMjHdr))
    lnClrClass      = laClrClass[1,2]
    *-- If style cost sheet.
    IF llNonMjExt
      *-- If there is non major.
      SHOW GET ibClrCls9 ENABLE
    ELSE
      SHOW GETS WINDOW MFCstSC9 DISABLE ONLY
    ENDIF
    SHOW WINDOW MFCSTSC9 TOP
    SHOW GETS WINDOW MFCstC10 DISABLE ONLY
    
    SHOW GET ibNDutyabl ENABLE
    SHOW GET puNMfgOpr  ENABLE
    
    _CUROBJ = OBJNUM(lcNStyItem)
  *-- If the current cost fabric or trim.
  CASE lcCstType $ 'FT'
    STORE 0 TO lnNWstg6 , lnNWstg7
    SHOW GETS WINDOW MFCstSC3 ENABLE ONLY
    *B602202,1 Reham On 11/26/98  ** Begin **
    *B602202,1 Disable set as primary fabric if cost item is trim.
    IF lcCstType = 'T'
      SHOW GET cbPrim DISABLE
    ELSE
      SHOW GET cbPrim ENABLE
    ENDIF
    *B602202,1 Reham On 11/26/98  ** End   **
    SHOW WINDOW MFCstSC3 TOP
    SHOW GETS WINDOW MFCstSC2 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC4 DISABLE ONLY
    SHOW GETS WINDOW MFCstSC5 DISABLE ONLY
    
    lcIClrText      = IIF(lcCstType='F','Fabric Color','Trim Color  ')
    laClrClass[1,1] = STRTRAN(laClrClass[1,1],'Item',TRIM(lcCostType))
    laClrClass[2,1] = STRTRAN(laClrClass[2,1],'Item',TRIM(lcCostType))
    laClrClass[3,1] = STRTRAN(laClrClass[3,1],'Item',TRIM(lcCostType))
    *-- If entering style cost sheet with no non-major , consider as constant.
    lnClrClass      = laClrClass[1,2]
    
    IF llColorExt .OR. lcStyleTyp = "T"
      SHOW GET ibClrCls10 ENABLE
    ELSE
      SHOW GET ibClrCls10 DISABLE
      SHOW GET ibItmClr10 DISABLE
      SHOW GET ibCsItmC10 ENABLE
      SHOW GET lcCsItmC10 ENABLE
    ENDIF
    SHOW WINDOW MFCstC10 TOP
    SHOW GETS WINDOW MFCSTSC9 DISABLE ONLY
    
    *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
    *E500348,1                lcStyleTyp <> "T" from the IF command to do 
    *E500348,1                it in all cost sheet types.    
    *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
    IF !EMPTY(laAStySeg[1,1])
    *E500348,1 KHM 06/04/2000 (End)
      puStyNSeg = 1
      SHOW GET puStyNSeg  ENABLE
      SHOW GET puSelVal   ENABLE
      SHOW GET lnNEstUnt7 ENABLE
      SHOW GET lnNUntQty7 ENABLE
      SHOW GET lnNWstg7   ENABLE
      IF lcCstType = 'T' .AND. !llItmTrInv
        SHOW GET lcNUom7    ENABLE
        SHOW GET lnNUntCst7 ENABLE
      ELSE
        SHOW GET lcNUom7    DISABLE
        SHOW GET lnNUntCst7 DISABLE
      ENDIF
      SHOW WINDOW MFCstSC7 TOP
      SHOW GETS WINDOW MFCstSC6 DISABLE ONLY
    ELSE
      SHOW GET lnNEsUnt6B DISABLE
      SHOW GET lnNEsUnt6A ENABLE
      SHOW GET lnNUntQt6A ENABLE
      SHOW GET lnNWstg6   ENABLE
      IF lcCstType = 'T' .AND. !llItmTrInv
        SHOW GET lcNUom6    ENABLE
        SHOW GET lnNUntCst6 ENABLE
      ELSE
        SHOW GET lcNUom6    DISABLE
        SHOW GET lnNUntCst6 DISABLE
      ENDIF
      SHOW WINDOW MFCstSC6 TOP
      SHOW GETS WINDOW MFCstSC7 DISABLE ONLY
    ENDIF
    
    SHOW GET ibNDutyabl ENABLE
    SHOW GET puNMfgOpr  ENABLE
    _CUROBJ = OBJNUM(lcNFabItem)
ENDCASE

*C037892,1 MHM 06/15/2004 Validate PopUp For  sales order cost sheet [Start]
IF ASCAN(laEvntTrig,PADR('SODTYPOP',10))<>0
  =gfDoTriger('SOALCST',PADR('SODTYPOP',10))
ENDIF
*C037892,1 MHM [End]

SHOW GET lcIClrText

*-- When the system is setup to use multiple curencies, Get cost
*-- item currency, Currency symbol, Exchange rate and unit.
IF llMulCurr
  lnStyRec = RECNO("STYLE")
  IF SEEK(SUBSTR(lcMajor,1,lnMajorLen),'Style')
    *-- Get the price currency & duty currency for the current style.
    lcCurrency = IIF(lcCstType='P' , Style.cPriceCur , ;
                     IIF(lcCstType $ 'MD' , Style.cDutyCur , gcBaseCurr))
    lcCurrency = IIF(EMPTY(lcCurrency) , gcBaseCurr , lcCurrency)
    
    *-- Get the exchange rate & rate sign & current unit.
    llFound   = SEEK(gcBaseCurr+lcCurrency,'SycExch')
    lnExRate6 = IIF(llFound,sycexch.nExRate,1)
    lnExRate7 = lnExRate6
    
    llFound    = SEEK(lcCurrency , 'SycCurr')
    lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
    STORE SycCurr.cCurrSmbl TO lcCrSmbA6 , lcCrSmbA7 , lcCrSmbB6 , lcCrSmbB7
    
    lcRateSign = gfGetExSin(@lcUnitSign , lcCurrency)
    
    *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
    *E500348,1                lcStyleTyp <> "T" from the IF command to do 
    *E500348,1                it in all cost sheet types.        
    *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
    IF !EMPTY(laAStySeg[1,1])
    *E500348,1 KHM 06/04/2000 (End)
      SHOW GET lcCrSmbA7 DISABLE
      SHOW GET lcCrSmbB7 DISABLE
      SHOW GET lnExRate7 DISABLE
    ELSE
      SHOW GET lcCrSmbA6 DISABLE
      SHOW GET lcCrSmbB6 DISABLE
      SHOW GET lnExRate6 DISABLE
    ENDIF
  ENDIF
  *-- Restore the record pointer in the style file.
  IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
    GOtO lnStyRec IN STYLE
  ENDIF
ENDIF

llCancel = .T.

*-- Disable the cost type popup.
SHOW GET ibCostType DISABLE

*-- Change the close prompt to be cancel.
SHOW GET pbClsNew,1 PROMPT lcSCanBmp ENABLE

*-- Enable the accept button.
SHOW GET pbAcptNew  ENABLE
*E500402,1 AMH Refreshing the color values of Marker Field and lable [Start]
lcMarkrLbl = IIF(lcCstType='F',"RGB(0,0,,,,,)","RGB(192,192,192,192,192,192)")
lcMarkrFld = IIF(lcCstType='F',gcObjColor,",&lcMarkrLbl,,,,&lcMarkrLbl,,,&lcMarkrLbl,&lcMarkrLbl")
lcMarkr3DG = IIF(lcCstType='F',"RGB(128,128,128,128,128,128)",lcMarkrLbl)
lcMarkr3DW = IIF(lcCstType='F',"RGB(255,255,255,192,192,192)",lcMarkrLbl)
=lfrefresh()
IF !EMPTY(laAStySeg[1,1])
  SHOW GET lcMarker7   ENABLE
ELSE
  SHOW GET lcMarker6   ENABLE
ENDIF
*E500402,1 AMH [End]

*!*************************************************************
*! Name      : lfvMfgCode
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate MFG codes.
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : lfvMfgCode()
*!*************************************************************
*!E301411,1 SSH 31/05/20
*!*************************************************************
FUNCTION lfvMfgCode
PRIVATE lnAlias

lcMfgCode  = laMfgCode[lnNMfgCode,2]
lcMCsItmDs = laMfgCode[lnNMfgCode,1]

*-- See if there is Mfg Operation have flag considered as operation yes .or. no.
DECLARE laMfgRltFd[1,2]
laMfgRltFd[1,1] = 'LMFGOPR'
laMfgRltFd[1,2] = 'llBMfgOpr'
=gfRltFld(lcMfgCode , @laMfgRltFd , "MFGCODE")

*E301411,1 SSH 31/05/20 [Begin] If PW module installed then
*E301411,1 SSH          check to Enable/Desable Detail Operation Butt.
IF llPwInst .AND. lcStyleTyp = 'M'
  =lfCheckPW()
ENDIF
*E301411,1 SSH 31/05/20 [End]
*C037892,1 MHM 06/15/2004 Validate PopUp For  sales order cost sheet [Start]
IF ASCAN(laEvntTrig,PADR('SOMFGPOP',10))<>0
  =gfDoTriger('SOALCST',PADR('SOMFGPOP',10))
ENDIF
*C037892,1 MHM [End]

IF llBMfgOpr
  SHOW GET puNMfgOpr DISABLE
ELSE
  SHOW GET puNMfgOpr ENABLE
ENDIF

*!*************************************************************
*! Name      : lfwClrClass
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Wehn function for Color class.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfwClrClass()
*!*************************************************************
*
FUNCTION lfwClrClass

*-- Save old method.
lnOldClrCls = lnClrClass

*!*************************************************************
*! Name      : lfvClrClass
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate Color class.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcNo-> Color class (1-Same as, 2-Constant, 3-Different)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvClrClass(lcNo)
*!*************************************************************
*
FUNCTION lfvClrClass
PARAMETERS lcNo
PRIVATE lcNo

lnibClrVal = EVAL(VARREAD())
ibClrClass = VARREAD()

lnClrClass = laClrClass[lnibClrVal,2]

SHOW GET &ibClrClass DISABLE

*B602121,1 Reham On 10/28/98   *** Begin ***
*B602121,1 Disable the cost item component to prevent the user from changing it
DO CASE
  CASE lcCstType = "S" AND !EMPTY(lcNStyItem)
    SHOW GET lcNStyItem DISABLE
  CASE lcCstType $ "FT" AND !EMPTY(lcNFabItem)
    SHOW GET lcNFabItem DISABLE
ENDCASE
*B602121,1 Reham On 10/28/98   *** End   ***

DO CASE
  *-- Same As color.
  CASE lnClrClass = 1
    STORE REPLICATE('*' , lnNonMjLen) TO lcItmClrs , lcCsItmC9
    lcCsItmC10 = REPLICATE('*' , 6)
    *B802958,1 ABD Remark the next line. [Begin]
    *SHOW GET ibItmClr&lcNo  DISABLE
    *B802958,1 ABD [End]
    SHOW GET ibCsItmC&lcNo  DISABLE
    SHOW GET lcCsItmC&lcNo  DISABLE
  *-- Constant color.
  CASE lnClrClass = 2
    STORE REPLICATE('*' , lnNonMjLen) TO lcItmClrs , lcCsItmC9
    lcCsItmC10 = SPACE(6)
    *B802958,1 ABD Remark the next line. [Begin]
    *SHOW GET ibItmClr&lcNo  DISABLE
    *B802958,1 ABD [End]
    IF llNonMjExt
      SHOW GET ibCsItmC&lcNo  ENABLE
      SHOW GET lcCsItmC&lcNo  ENABLE
    ELSE
      SHOW GET ibCsItmC&lcNo  DISABLE
      SHOW GET lcCsItmC&lcNo  DISABLE
    ENDIF
  *-- Different color.
  CASE lnClrClass = 3
    llDifColor = .F.  && Flag to know that there is no color entered yet.
    IF lcStyleTyp = 'T'
      lcItmClrs   = IIF(lcCstType = "S" , laColors9[1,1] , laColors10[1,1])
      FOR lnCount = 1 TO ALEN(laColors9,1)
        laColors9[lnCount,3] = SPACE(6)
        laColors9[lnCount,4] = 0
      ENDFOR
      FOR lnCount = 1 TO ALEN(laColors10,1)
        laColors10[lnCount,3] = SPACE(6)
        laColors10[lnCount,4] = 0
      ENDFOR
      
      lcCsItmC9  = laColors9[1,3]
      lcCsItmC10 = laColors10[1,3]
    ELSE
      lcCsItmC9  = SPACE(lnNonMjlen)
      lcCsItmC10 = SPACE(6)
    ENDIF
    IF llNonMjExt
      SHOW GET ibItmClr&lcNo  ENABLE
      SHOW GET ibCsItmC&lcNo  ENABLE
      SHOW GET lcCsItmC&lcNo  ENABLE
      _CUROBJ = OBJNUM(ibItmClr&lcNo)
    ELSE
      SHOW GET ibItmClr&lcNo  DISABLE
      SHOW GET ibCsItmC&lcNo  DISABLE
      SHOW GET lcCsItmC&lcNo  DISABLE
    ENDIF
ENDCASE

*C037892,1 MHM 06/15/2004 Validate PopUp For  sales order cost sheet [Start]
IF ASCAN(laEvntTrig,PADR('SOCLSCHK',10))<>0
  =gfDoTriger('SOALCST',PADR('SOCLSCHK',10))
ENDIF
*C037892,1

*!*************************************************************
*! Name      : lfvCScheme
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate Selected Color Scheme.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvCScheme()
*!*************************************************************
*
FUNCTION lfvCScheme
PRIVATE laItemRec , lnCurRec , lnFbrRec , lnStyRec

*-- If entering style cost sheet & the style structure has no 
*-- non major, No need for cross refrencing the colors.
IF lcStyleTyp <> "T" .AND. (!llNonMjExt .OR. !llColorExt)
  RETURN .T.
ENDIF

*-- If "same as color" , cross refrencing the colors or the majors.
IF lnClrClass=1 .AND. (lcCstType $ 'FS' .OR. (lcCstType='T' .AND. llItmTrInv))
  lnFbrRec = RECNO("FABRIC")
  lnStyRec = RECNO("STYLE")
  IF lcCstType $ 'FT'
    WAIT IIF(lcCstType='F','Cross referencing Fabric colors',;
                           'Cross referencing Trim colors') WINDOW NOWAIT
    SELECT FABRIC
    =SEEK(PADR(lcNFabItem,7))
  ELSE
    WAIT 'Cross referencing Component style colors' WINDOW NOWAIT
    SELECT STYLE
    =SEEK(PADR(lcNStyItem,lnMajorLen))
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
  SCATTER TO laItemRec
  
  *-- If the cost item was fabric or trim.
  IF lcCstType $ 'FT'
    SET ORDER TO FABRIC IN FABRIC
    SET ORDER TO STYLE  IN STYLE
    lcScanExp = IIF(lcStyleTyp = "T" , " FOR Fabric.Make" , "")
    SELECT (lcItemFile)
    =SEEK(PADR(lcMajor,lnItemLen))
    SCAN REST WHILE PADR(&lcItemFile,lnItemLen) = PADR(lcMajor,lnItemLen) &lcScanExp
      lnCurRec = RECNO()
      *-- If the item file is fabric, use the color field.
      *-- If the item file is style, cut the color according to 
      *-- the color start position & color segment lenght.
      lcColor  = IIF(lcStyleTyp ='T' , &lcItemFile..Color , ;
                 SUBSTR(&lcItemFile..Style , lnColorStr , lnColorLen))
      IF !SEEK(SUBSTR(lcNFabItem,1,7)+PADR(lcColor,6) , 'Fabric') .AND. ;
         !SEEK(SUBSTR(lcNFabItem,1,7)+PADR(lcColor,6) , lcTmpFbric) 
        
        *** {lcNonMjHdr} : {lcColor} not found for ***
        *** {IIF(lcCstType= 'F','Fabric','Trim')}: ***
        *** SUBSTR(lcNFabItem,1,7). Do you wish to add it? ***
        *** < Yes > - < No > ***
        lcTmpStr = IIF(lcStyleTyp="T",lcItemFile+"/"+"color",lcNonMjHdr)+"|"+lcColor+"|"+IIF(lcCstType= 'F','Fabric','Trim')+"|"+SUBSTR(lcNFabItem,1,7)
        IF gfModalGen("QRM38012B00006" , "DIALOG" , lcTmpStr) = 2
          RETURN(.F.)
        ENDIF
        
        *-- If the fabric + color was not found, add it in the temp. fabric file.
        SELECT (lcTmpFbric)
        APPEND BLANK
        GATHER FROM laItemRec
        *B602675,1 Reham On 03/18/99   *** Begin ***
        *B602675,1 Add the new fabric+color with WIP value equal zero.
        *B602625,1 Reham On 03/03/99   *** Begin ***
        *B602625,1 Add the new fabric+color with stock value equal zero.
        *REPLACE COLOR      WITH lcColor ;
                ONHAND     WITH 0 ;
                ONORDER    WITH 0 ;
                USAGE      WITH 0
        REPLACE COLOR      WITH lcColor ;
                ONHAND     WITH 0 ;
                ONORDER    WITH 0 ;
                USAGE      WITH 0 ;
                nStkVal    WITH 0 ;
                nMatWip    WITH 0
        *B602625,1 Reham On 03/03/99   *** End   ***
        *B602675,1 Reham On 03/18/99   *** End   ***
        
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
  ELSE
    SELECT STYLE
    SET ORDER TO STYLE IN STYLE
    *-- Scan for all the available majors , if not found add it.
    =SEEK(PADR(lcMajor,lnMajorLen))
    SCAN REST WHILE PADR(STYLE,lnMajorLen) = PADR(lcMajor,lnMajorLen) ;
          FOR LIKE(PADR(PADR(lcMajor,lnMajorLen)+ALLTRIM(lcSeprator)+REPLICATE("?" , lnNonMjLen+lnSizeLen),19) , STYLE.Style)
      lnCurRec   = RECNO()
      lcNonMajor = SUBSTR(Style.Style , lnMajorLen+2 , lnNonMjLen)
      
      IF !SEEK(lcNStyItem + lcSeprator + lcNonMajor , 'Style') .AND. ;
         !SEEK(lcNStyItem + lcSeprator + lcNonMajor , lcTmpStyle)
        
        *** {lcItemFile} : {&lcItemFile..Color} not found for ***
        *** component style: {lcNStyItem}.  Do you wish to add it? ***
        *** < Yes > - < No > ***
        lcTmpStr = IIF(lcStyleTyp="T" , lcItemFile+"/color" , lcNonMjHdr) + "|" + lcNonMajor + "|" + lcNStyItem
        IF gfModalGen("QRM38013B00006" , "DIALOG" , lcTmpStr) = 2
          RETURN(.F.)
        ENDIF
        
        *-- If there is extended size scale.
        IF llExtSizSc
          IF !EMPTY(laTmpScale[1])
            *-- Add record for every scale.
            FOR lnCnt = 1 TO ALEN(laTmpScale,1)
              SELECT (lcTmpStyle)
              APPEND BLANK
              GATHER FROM laItemRec
              REPLACE STYLE     WITH lcNStyItem + lcSeprator + lcNonMajor + lcSizeSep + laTmpScale[lnCnt] ;
                      cStyMajor WITH lcNStyItem ;
                      ORD1 WITH 0 ORD2 WITH 0 ORD3 WITH 0 ORD4 WITH 0 ;
                      ORD5 WITH 0 ORD6 WITH 0 ORD7 WITH 0 ORD8 WITH 0 TOTORD WITH 0 ;
                      Wip1 WITH 0 Wip2 WITH 0 Wip3 WITH 0 Wip4 WITH 0 ;
                      Wip5 WITH 0 Wip6 WITH 0 Wip7 WITH 0 Wip8 WITH 0 TOTWip WITH 0 ;
                      Stk1 WITH 0 Stk2 WITH 0 Stk3 WITH 0 Stk4 WITH 0 ;
                      Stk5 WITH 0 Stk6 WITH 0 Stk7 WITH 0 Stk8 WITH 0 TOTStk WITH 0 ;
                      Alo1 WITH 0 Alo2 WITH 0 Alo3 WITH 0 Alo4 WITH 0 ;
                      Alo5 WITH 0 Alo6 WITH 0 Alo7 WITH 0 Alo8 WITH 0 TOTAlo WITH 0 ;
                      Shp1 WITH 0 Shp2 WITH 0 Shp3 WITH 0 Shp4 WITH 0 ;
                      Shp5 WITH 0 Shp6 WITH 0 Shp7 WITH 0 Shp8 WITH 0 TOTShp WITH 0
              REPLACE Ret1 WITH 0 Ret2 WITH 0 Ret3 WITH 0 Ret4 WITH 0 ;
                      Ret5 WITH 0 Ret6 WITH 0 Ret7 WITH 0 Ret8 WITH 0 TOTRet WITH 0 ;
                      Plan1 WITH 0 Plan2 WITH 0 Plan3 WITH 0 Plan4 WITH 0 ;
                      Plan5 WITH 0 Plan6 WITH 0 Plan7 WITH 0 Plan8 WITH 0 TotPlan WITH 0;
                      RA1   WITH 0 RA2   WITH 0 RA3   WITH 0 RA4   WITH 0 ;
                      RA5   WITH 0 RA6   WITH 0 RA7   WITH 0 RA8   WITH 0 TotRa   WITH 0 ;
                      Intrans1 WITH 0 Intrans2 WITH 0 Intrans3 WITH 0 Intrans4 WITH 0 ;
                      Intrans5 WITH 0 Intrans6 WITH 0 Intrans7 WITH 0 Intrans8 WITH 0 TotIntrn WITH 0 ;
                      Nwo1 WITH 0 Nwo2 WITH 0 Nwo3 WITH 0 Nwo4 WITH 0 ;
                      Nwo5 WITH 0 Nwo6 WITH 0 Nwo7 WITH 0 Nwo8 WITH 0 nTotWo WITH 0
              *-- Call global function to add audit fields info.
              =gfAdd_Info(lcTmpStyle)
            ENDFOR
          ENDIF
        ELSE
          *-- If there is no extended size scale, add the style.
          SELECT (lcTmpStyle)
          APPEND BLANK
          GATHER FROM laItemRec
          REPLACE STYLE     WITH lcNStyItem + lcSeprator + lcNonMajor ;
                  cStyMajor WITH lcNStyItem ;
                  ORD1 WITH 0 ORD2 WITH 0 ORD3 WITH 0 ORD4 WITH 0 ;
                  ORD5 WITH 0 ORD6 WITH 0 ORD7 WITH 0 ORD8 WITH 0 TOTORD WITH 0 ;
                  Wip1 WITH 0 Wip2 WITH 0 Wip3 WITH 0 Wip4 WITH 0 ;
                  Wip5 WITH 0 Wip6 WITH 0 Wip7 WITH 0 Wip8 WITH 0 TOTWip WITH 0 ;
                  Stk1 WITH 0 Stk2 WITH 0 Stk3 WITH 0 Stk4 WITH 0 ;
                  Stk5 WITH 0 Stk6 WITH 0 Stk7 WITH 0 Stk8 WITH 0 TOTStk WITH 0 ;
                  Alo1 WITH 0 Alo2 WITH 0 Alo3 WITH 0 Alo4 WITH 0 ;
                  Alo5 WITH 0 Alo6 WITH 0 Alo7 WITH 0 Alo8 WITH 0 TOTAlo WITH 0 ;
                  Shp1 WITH 0 Shp2 WITH 0 Shp3 WITH 0 Shp4 WITH 0 ;
                  Shp5 WITH 0 Shp6 WITH 0 Shp7 WITH 0 Shp8 WITH 0 TOTShp WITH 0
          REPLACE Ret1 WITH 0 Ret2 WITH 0 Ret3 WITH 0 Ret4 WITH 0 ;
                  Ret5 WITH 0 Ret6 WITH 0 Ret7 WITH 0 Ret8 WITH 0 TOTRet WITH 0 ;
                  Plan1 WITH 0 Plan2 WITH 0 Plan3 WITH 0 Plan4 WITH 0 ;
                  Plan5 WITH 0 Plan6 WITH 0 Plan7 WITH 0 Plan8 WITH 0 TotPlan WITH 0;
                  RA1   WITH 0 RA2   WITH 0 RA3   WITH 0 RA4   WITH 0 ;
                  RA5   WITH 0 RA6   WITH 0 RA7   WITH 0 RA8   WITH 0 TotRa   WITH 0 ;
                  Intrans1 WITH 0 Intrans2 WITH 0 Intrans3 WITH 0 Intrans4 WITH 0 ;
                  Intrans5 WITH 0 Intrans6 WITH 0 Intrans7 WITH 0 Intrans8 WITH 0 TotIntrn WITH 0 ;
                  Nwo1 WITH 0 Nwo2 WITH 0 Nwo3 WITH 0 Nwo4 WITH 0 ;
                  Nwo5 WITH 0 Nwo6 WITH 0 Nwo7 WITH 0 Nwo8 WITH 0 nTotWo WITH 0
          *-- Call global function to add audit fields info.
          =gfAdd_Info(lcTmpStyle)
        ENDIF
      ENDIF
      SELECT STYLE
      *-- Restore the record pointer in the item file.
      IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT("STYLE")
        GOTO lnCurRec
      ENDIF
    ENDSCAN
    SET ORDER TO CSTYLE IN STYLE
  ENDIF
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
*! Name      : lfvItmClrs
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Select Style/Material color.
*!*************************************************************
*! Calls     : gfActPop, lfRefresh
*!*************************************************************
*! Passed Parameters  : lcNo-> Define if the color for style or fabric
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvItmClrs(lcNo)
*!*************************************************************
*
FUNCTION lfvItmClrs
PARAMETERS lcNo
PRIVATE lcNo
*-- Save the color selected from the item colors popup.
lcItmClrs      = laColors&lcNo[ibItmClr&lcNo,1]
lnItmClrs      = laColors&lcNo[ibItmClr&lcNo,2]
lcCsItmC&lcNo  = laColors&lcNo[lnItmClrs,3]
*-- Refresh the object to select a color.
SHOW GET lcCsItmC&lcNo
*-- Clear fox buffer.
CLEAR TYPEAHEAD
_CUROBJ = OBJNUM(lcCsItmC&lcNo)

*!*************************************************************
*! Name      : lfvCsItmClr
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate cost item color.
*!*************************************************************
*! Calls     : gfStyBrw, FABrow, gfBrows
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCsItmClr()
*!*************************************************************
*
FUNCTION lfvCsItmClr
PRIVATE lcAlias , lnBomRec

IF !llBrowse AND EMPTY(EVAL(VARREAD()))
  RETURN
ENDIF

DO CASE
  *-- If cost type is style component, Browse the non-major 
  *-- for the selected style.
  CASE lcCstType = 'S'
    lnStyRec = RECNO("STYLE")
    SET ORDER TO STYLE IN STYLE
    IF llBrowse .OR. ;
       !SEEK(PADR(lcNStyItem,lnMajorLen)+lcSeprator+PADR(lcCsItmC9,lnNonMjLen) , 'Style')
      lcCsItmC9  = IIF(llBrowse , "?" , lcCsItmC9)
      llBrowse   = .F.
      lcCsItmC9  = gfStyBrw("N" , ALLTRIM(lcNStyItem) , lcCsItmC9 , .F.)
      lcCsItmC9  = IIF(!EMPTY(lcCsItmC9) , PADR(lcCsItmC9,lnNonMjLen) , lcCsItmC9)
      SET ORDER TO CSTYLE IN STYLE
      IF EMPTY(lcCsItmC9)
        lcCsItmC9 = SPACE(lnNonMjLen)
        _CUROBJ   = OBJNUM(lcCsItmC9)
        RETURN
      ENDIF
    ENDIF
    SET ORDER TO CSTYLE IN STYLE
    
    *-- If the style component is the same like the main style.
    IF llSame .AND. !EMPTY(lcCsItmC9)
      *-- Main style + no-major cannot be the same as the style 
      *-- component + non-major.
      IF PADR(laColors9[ibItmClr9,1] , lnNonMjLen) = PADR(lcCsItmC9 , lnNonMjLen)
        *** Cannot use item component same as the parent item. ***
        *** <  OK  > ***
        =gfModalGen("INM38144B00000" , "DIALOG")
        lcCsItmC9 = SPACE(lnNonMjLen)
        _CUROBJ   = OBJNUM(lcCsItmC9)
        RETURN
      ELSE
        llFoundClr = .F.      && Flag to know if cost item entered before or not.
        lcAlias    = ALIAS()  && Save the current alias.
        SELECT (lcTmpBom)
        lnBomRec   = RECNO()  && Save the current record pointer in the bom file.
        
        *-- Find if the item component for the current parent item 
        *-- has used before in the opposite way or not in the file.
        LOCATE FOR PADR(cItmMajor , lnMajorLen) = lcMajor .AND. ;
                   PADR(Item , lnMajorLen) = lcMajor .AND. ;
                   PADR(lcCsItmC9 , lnNonMjLen) $ cItmMask .AND. ;
                   PADR(laColors9[ibItmClr9,1] , lnNonMjLen) $ Item
        IF FOUND()
          llFoundClr = .T.
        ELSE
          *-- Find if the item component for the current parent item 
          *-- has used before in the opposite way or not in the array.
          FOR lnCount = 1 TO ALEN(laColors9,1)
            IF PADR(laColors9[lnCount,1] , lnNonMjLen) = PADR(lcCsItmC9 , lnNonMjLen) .AND. ;
               PADR(laColors9[ibItmClr9,1] , lnNonMjLen) = PADR(laColors9[lnCount,3] , lnNonMjLen)
              llFoundClr = .T.
              EXIT
            ENDIF
          ENDFOR
        ENDIF
        *-- Restore the record pointer in the bom file.
        IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT()
          GOTO lnBomRec
        ENDIF
        
        IF !llFoundClr
          lcCurAlis = ALIAS() && Save the current alias
          SELECT (lcTmpBom)   && Select the temp. Bom file.
          lnCurRec = RECNO()  && Save the current record pointer.
          
          *-- Call local function to know if the selected component is
          *-- a parent of a component to the selected parent.
          lcClrComp = lfGetClrCm(PADR(lcCsItmC9 , lnNonMjLen))
          
          *-- Restore record pointer in the temp. Bom file.
          IF lnCurRec > 0 AND lnCurRec <= RECCOUNT()
            GOTO lnCurRec
          ENDIF
          
          *-- Restore the previous alias.
          SELECT (lcCurAlis)
          
          *-- Check if the selected parent exist in the returnd string.
          IF PADR(laColors9[ibItmClr9,1] , lnNonMjLen) $ lcClrComp
            llFoundClr = .T.
          ENDIF
        ENDIF
        
        *-- Restore the previous alias.
        SELECT (lcAlias)
        IF llFoundClr
          *** Item {ALLTRIM(laColors9[lnCount,1])} is already component for item {ALLTRIM(lcCsItmC9)}. ***
          *** <  OK  > ***
          lcTmpStr = ALLTRIM(laColors9[ibItmClr9,1]) + "|" + ALLTRIM(lcCsItmC9)
          =gfModalGen("INM38143B00000" , "DIALOG" , lcTmpStr)
          
          *-- Blank the entered color & back to the same object.
          lcCsItmC9 = SPACE(lnNonMjLen)
          _CUROBJ   = OBJNUM(lcCsItmC9)
          RETURN
        ENDIF
      ENDIF
    ENDIF    
  
  *-- If the cost type is fabric, browse the available colors 
  *-- for the selected fabric.
  CASE lcCstType = 'F'
    SET ORDER TO FABRIC IN FABRIC
    IF llBrowse .OR. !SEEK(PADR(lcNFabItem,7)+PADR(lcCsItmC10,6) , 'Fabric')
      llBrowse   = .F.
      lcCsItmC10 = IIF(EMPTY(lcNFabItem), PADR(lcCsItmC10, 6), CHR(240))
      =FaBrow(lcNFabItem,@lcCsItmC10))
      SET ORDER TO CFABRIC IN FABRIC
      IF EMPTY(lcCsItmC10)
        lcCsItmC10 = SPACE(6)
        _CUROBJ    = OBJNUM(lcCsItmC10)
        RETURN
      ELSE
        lcCsItmC10 = PADR(lcCsItmC10 , 6)
      ENDIF
    ENDIF
    SET ORDER TO CFABRIC IN FABRIC
  *-- If the cost type is Trim.
  CASE lcCstType = 'T'
    IF llItmTrInv
      *-- Browse from the fabric file.
      SET ORDER TO FABRIC IN FABRIC
      IF llBrowse .OR. !SEEK(PADR(lcNFabItem,7)+PADR(lcCsItmC10,6) , 'Fabric')
        llBrowse   = .F.
        lcCsItmC10 = IIF(EMPTY(lcNFabItem), PADR(lcCsItmC10, 6), CHR(240))        
        =FaBrow(lcNFabItem,@lcCsItmC10))
        SET ORDER TO CFABRIC IN FABRIC
        IF EMPTY(lcCsItmC10)
          lcCsItmC10 = SPACE(6)
          _CUROBJ    = OBJNUM(lcCsItmC10)
          RETURN
        ELSE
          lcCsItmC10 = PADR(lcCsItmC10 , 6)
        ENDIF
      ENDIF
      SET ORDER TO CFABRIC IN FABRIC
    ELSE
      *-- Browse colors from the codes file.
      lcOldAlias = ALIAS()
      lcOldTag   = ORDER("CODES")
      SELECT CODES
      SET ORDER TO cCode_No IN CODES
      *E301077,16 Reham On 03/08/99  *** Begin ***
      *E301077,16 Use cDefCode field instead of cComp_Id.
      *IF llBrowse .OR. !SEEK(gcAct_Comp + PADR("COLOR",10) + PADR(lcCsItmC10,6) , "CODES")
      IF llBrowse .OR. !SEEK("N" + PADR("COLOR",10) + PADR(lcCsItmC10,6) , "CODES")
      *E301077,16 Reham On 03/08/99  *** End   ***
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
        laClr[1]   = lcCsItmC10
        
        *E301077,16 Reham On 03/08/99  *** Begin ***
        *E301077,16 Browse the main colors records without its related fields.
        SET FILTER TO cRltField = "N"
        *E301077,16 Use cDefCode field instead of cComp_Id.
        *-- Call the global browse with the colors file.
        *=gfBrows([gcAct_Comp + "COLOR"],"cCode_No","laClr")
        =gfBrows(["N" + "COLOR"],"cCode_No","laClr")
        SET FILTER TO
        *E301077,16 Reham On 03/08/99  *** End   ***
        
        lcBrFields = lcSaveBrow
        lcFile_ttl = lcOld_ttl
        
        lcCsItmC10 = IIF(lcCsItmC10 = laClr[1] , SPACE(6) , laClr[1])
      ENDIF
      SET ORDER TO &lcOldTag IN CODES
      SELECT (lcOldAlias)
    ENDIF
ENDCASE

llBrowse = .F.

*-- Refresh the cost objects in the screen.
IF (lcCstType $ 'SF' .OR. (lcCstType = 'T' .AND. llItmTrInv)) ;
   .AND. !EMPTY(EVALUATE(VARREAD()))
  lcSCsItmDs  = IIF(lcCstType = 'S' , Style.Desc1  , '')
  *-- If the style component is same as parent style, call function to calculate cost.
  lnNUntCst6  = IIF(lcCstType = 'S' , ;
                IIF(llSame , lfTempCost(lcNStyItem , lcCsItmC9) , Style.TotCost) , ;
                Fabric.CostBuy/Fabric.Conv)
  
  *B602624,1 Reham On 03/03/99   *** Begin ***
  *B602624,1 Round the gross units to 3 dicimals.
  *lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
  lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))),3) * lnNUntCst6 , 2))
  *B602624,1 Reham On 03/03/99   *** End   ***
  
  *-- If the style component is same as parent style, call function to calculate cost.
  lnNUntCst7  = IIF(lcCstType = 'S'  , ;
                IIF(llSame , lfTempCost(lcNStyItem , lcCsItmC9) , Style.TotCost) , ;
                Fabric.CostBuy/Fabric.Conv)
  
  *B602624,1 Reham On 03/03/99   *** Begin ***
  *B602624,1 Round the gross units to 3 dicimals.
  *lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
  lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
  
  *lnNUntQt6A = lnNEsUnt6A*(1+(lnNWstg6/100))
  lnNUntQt6A = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)
  
  *lnNUntQt6B = ROUND(lnNEsUnt6B*(1+(lnNWstg6/100)),0)
  lnNUntQt6B = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))
  
  *lnNUntQty7 = lnNEstUnt7*(1+(lnNWstg7/100))
  lnNUntQty7 = ROUND(lnNEstUnt7*(1+(lnNWstg7/100)) , 3)
  *B602624,1 Reham On 03/03/99   *** End   ***
ENDIF

IF lcCstType = "S"
  SHOW GET lcSCsItmDs
  SHOW GET lcCsItmC9
ELSE
  SHOW GET lcFCsItmDs
  SHOW GET lcCsItmC10
ENDIF
*E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
*E500348,1                lcStyleTyp <> "T" from the IF command to do 
*E500348,1                it in all cost sheet types.
*IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1]) .AND. lcCstType <> "S"
IF !EMPTY(laAStySeg[1,1]) .AND. lcCstType <> "S"
*E500348,1 KHM 06/04/2000 (End)
  SHOW GET lnNUntCst7
  SHOW GET lnNUntQty7
  SHOW GET lnTotFCst7 DISABLE
ELSE
  SHOW GET lnNUntCst6
  IF lcCstType = "S"
    SHOW GET lnNUntQt6B
  ELSE
    SHOW GET lnNUntQt6A
  ENDIF
  SHOW GET lnTotFCst6 DISABLE
ENDIF

*-- Fill the colors array if the method was different color.
IF lnClrClass=3
  DO CASE
    CASE lcCstType = "S" .AND. !EMPTY(lcCsItmC9)
      *-- Flag to know that there is at least one color entered.
      llDifColor = .T. 
      laColors9[lnItmClrs,3] = lcCsItmC9
      laColors9[lnItmClrs,4] = lnNUntCst6
      FOR lnCnt = 1 TO ALEN(laColors9,1)
        IF EMPTY(laColors9[lnCnt,3])
          lcItmClrs  = laColors9[lnCnt,1]
          lnItmClrs  = laColors9[lnCnt,2]
          ibItmClr9  = laColors9[lnCnt,2]
          SHOW GET ibItmClr9
          lcItmClrs = laColors9[ibItmClr9,1]
          lnItmClrs = laColors9[ibItmClr9,2]
          lcCsItmC9 = laColors9[lnItmClrs,3]
          SHOW GET lcCsItmC9
          _CUROBJ    = _CUROBJ
          EXIT
        ENDIF
      ENDFOR
    CASE lcCstType <> "S" .AND. !EMPTY(lcCsItmC10)
      *-- Flag to know that there is at least one color entered.
      llDifColor = .T. 
      laColors10[lnItmClrs,3] = PADR(lcCsItmC10,6)
      laColors10[lnItmClrs,4] = lnNUntCst6
      FOR lnCnt = 1 TO ALEN(laColors10,1)
        IF EMPTY(laColors10[lnCnt,3])
          lcItmClrs  = laColors10[lnCnt,1]
          lnItmClrs  = laColors10[lnCnt,2]
          ibItmClr10 = laColors10[lnCnt,2]
          SHOW GET ibItmClr10
          lcItmClrs  = laColors10[ibItmClr10,1]
          lnItmClrs  = laColors10[ibItmClr10,2]
          lcCsItmC10 = laColors10[lnItmClrs,3]
          SHOW GET lcCsItmC10
          _CUROBJ    = _CUROBJ
          EXIT
        ENDIF
      ENDFOR
  ENDCASE
ENDIF

*!*************************************************************
*! Name      : lfvNUntQty
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validation function for Unit quantity for new item.
*!*************************************************************
*! Calls     :lfRefresh
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which unit qty.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNUntQty(lcNo)
*!*************************************************************
*
FUNCTION lfvNUntQty
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  IF lcCstType = 'S'
    lnNEstUnt7 = lnNEsUnt6B
    lnNEsUnt6A = lnNEsUnt6B
  ELSE
    lnNEstUnt7 = lnNEsUnt6A
    lnNEsUnt6B = lnNEsUnt6A
  ENDIF
ELSE
  lnNEsUnt6B = lnNEstUnt7
  lnNEsUnt6A = lnNEstUnt7
ENDIF

*-- Update the cost objects.
lnNUntQt6B = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))

*B602624,1 Reham On 03/03/99   *** Begin ***
*B602624,1 Round the gross units to 3 dicimals.
*lnNUntQt6A = lnNEsUnt6A*(1+(lnNWstg6/100))
lnNUntQt6A = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)

*lnNUntQty7 = lnNEstUnt7 * (1+(lnNWstg7/100))
lnNUntQty7 = ROUND(lnNEstUnt7 * (1+(lnNWstg7/100)) , 3)

*lnTotFCst6 = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
lnTotFCst6 = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))

*lnTotFCst7 = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
lnTotFCst7 = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
*B602624,1 Reham On 03/03/99   *** End   ***

*RAM
*lnTotEqu6  = lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit
*lnTotEqu7  = lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit
lnTotEqu6  = ROUND(lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit , 2)
lnTotEqu7  = ROUND(lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit , 2)


*-- Refresh the cost object
IF lcNo = "6"
  IF lcCstType = 'S'
    SHOW GET lnNUntQt6B
  ELSE
    SHOW GET lnNUntQt6A
  ENDIF
ELSE
  SHOW GET lnNUntQty7
ENDIF
SHOW GET lnTotFCst&lcNo
SHOW GET lnTotEqu&lcNo

*!*************************************************************
*! Name      : lfvNWstg
*! Developer : Reham Al-Allamy
*! Date      : 08/20/1998
*! Purpose   : Validation function for Unit quantity wastage.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which unit qty.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNWstg(lcNo)
*!*************************************************************
*
FUNCTION lfvNWstg
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  lnNWstg7 = lnNWstg6
ELSE
  lnNWstg6 = lnNWstg7
ENDIF

lnNUntQt6B = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))

*B602624,1 Reham On 03/03/99   *** Begin ***
*B602624,1 Round the gross units to 3 dicimals.
*lnNUntQt6A = lnNEsUnt6A*(1+(lnNWstg6/100))
lnNUntQt6A = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)

*lnNUntQty7 = lnNEstUnt7 * (1+(lnNWstg7/100))
lnNUntQty7 = ROUND(lnNEstUnt7 * (1+(lnNWstg7/100)) , 3)

*lnTotFCst6 = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
lnTotFCst6 = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))

*lnTotFCst7 = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
lnTotFCst7 = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
*B602624,1 Reham On 03/03/99   *** End   ***

*RAM
*lnTotEqu6  = lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit
*lnTotEqu7  = lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit
lnTotEqu6  = ROUND(lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit,2)
lnTotEqu7  = ROUND(lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit,2)

*-- Refresh the cost object
IF lcNo = "6"
  IF lcCstType = 'S'
    SHOW GET lnNUntQt6B
  ELSE
    SHOW GET lnNUntQt6A
  ENDIF
ELSE
  SHOW GET lnNUntQty7
ENDIF
SHOW GET lnTotFCst&lcNo
SHOW GET lnTotEqu&lcNo


*!*************************************************************
*! Name      : lfvNUom
*! Developer : Reham Al-Allamy
*! Date      : 03/04/1999
*! Purpose   : Validation function for UOM field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which unit qty.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNUom(lcNo)
*!*************************************************************
*
FUNCTION lfvNUom
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  lcNUom7 = lcNUom6
ELSE
  lcNUom6 = lcNUom7
ENDIF

*!*************************************************************
*! Name      : lfvNUntCst
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validation function for Unit cost for a new item.
*!*************************************************************
*! Calls     :lfRefresh
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which unit cost.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNUntCst(lcNo)
*!*************************************************************
*
FUNCTION lfvNUntCst
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  lnNUntCst7 = lnNUntCst6
ELSE
  lnNUntCst6 = lnNUntCst7
ENDIF

IF lcStyleTyp = 'I' .AND. lnNUntCst&lcNo = 0 
  IF !(lcCstType $ 'MD')
    *** Cost must be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("INM38027B00000" , "DIALOG")
    SHOW GET lnNCstPrc&lcNo DISABLE
    _CUROBJ = _CUROBJ
  ELSE
    IF lcPricCur <> lcDutyCur
      SHOW GET lnNCstPrc&lcNo DISABLE
      _CUROBJ = _CUROBJ
    ELSE
      SHOW GET lnNCstPrc&lcNo ENABLE
      _CUROBJ = OBJNUM(lnNCstPrc&lcNo)
    ENDIF
    *C037892,1 MHM 06/15/2004 Enable Price[Start]
    IF ASCAN(laEvntTrig,PADR('SOENBPRC',10))<>0
      =gfDoTriger('SOALCST',PADR('SOENBPRC',10))
    ENDIF
    *C037892,1 MHM [End]
    
  ENDIF
  RETURN
ELSE
  *-- Blank the percentage objects.
  STORE 0 TO lnNCstPrc6 , lnNCstPrc7
  SHOW GET lnNCstPrc&lcNo DISABLE
ENDIF

*-- Recalculate total and equevlent cost

*B602624,1 Reham On 03/03/99   *** Begin ***
*B602624,1 Round the gross units to 3 dicimals.
*lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))

*lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
*B602624,1 Reham On 03/03/99   *** End   ***

*RAM
*lnTotEqu6   = lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit
*lnTotEqu7   = lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit
lnTotEqu6   = ROUND(lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit,2)
lnTotEqu7   = ROUND(lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit,2)


SHOW GET lnTotFCst&lcNo
SHOW GET lnTotEqu&lcNo

*!*************************************************************
*! Name      : lfvDutyable
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Decide if this item is a dutyable item.
*!*************************************************************
*! Calls     :None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvDutyable()
*!*************************************************************
*
FUNCTION lfvDutyable

*-- Update the duty variable.
*C037892,1 MHM 06/15/2004 Validate on Duty for Custom sales order cost sheet
IF ASCAN(laEvntTrig,PADR('SOVDTYPO',10))<>0
  =gfDoTriger('SOALCST',PADR('SOVDTYPO',10))
  RETURN
ENDIF
*C037892,1 [End]

lcNDutyable = laDutyable[ibNDutyabl,2]

*!*************************************************************
*! Name      : lfvNCstPrc
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Valide function for cost percent for Duty and 
*!           : Manufactuing operations while adding new cost item.
*!*************************************************************
*! Calls     :None
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which percentage
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNCstPrc(lcNo)
*!*************************************************************
*
FUNCTION lfvNCstPrc
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  lnNCstPrc7 = lnNCstPrc6
ELSE
  lnNCstPrc6 = lnNCstPrc7
ENDIF

lnNUntCst6 = 0
lnNUntCst7 = 0

*-- Recalculate total and equevlent cost

*B602624,1 Reham On 03/03/99   *** Begin ***
*B602624,1 Round the gross units to 3 dicimals.
*lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))

*lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
*B602624,1 Reham On 03/03/99   *** End   ***

*RAM
*lnTotEqu6   = lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit
*lnTotEqu7   = lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit
lnTotEqu6   = ROUND(lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit,2)
lnTotEqu7   = ROUND(lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit,2)

*-- Refresh the cost objects.
SHOW GET lnNUntCst&lcNo  ENABLE
SHOW GET lnTotFCst&lcNo
SHOW GET lnTotEqu&lcNo

*!*************************************************************
*! Name      : lfvNCstItm
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Validate Cost Items ( Fabric, Trims and Styles )
*!*************************************************************
*! Calls     : FaBrow, gfStyBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvNCstItm()
*!*************************************************************
*
FUNCTION lfvNCstItm
PRIVATE lnAlias

IF !llBrowse .AND. IIF(lcCstType='S' , EMPTY(lcNStyItem) , EMPTY(lcNFabItem))
  RETURN
ENDIF

lnAlias = SELECT()
DO CASE
  *-- If the cost type is fabric.
  CASE lcCstType = 'F'
    IF (llBrowse .OR. !SEEK(lcNFabItem , 'Fabric'))
      *-- Call the fabric global browse.
      =FaBrow(@lcNFabItem,'*')
    ENDIF
    llBrowse = .F.   && Browse flag to false.
    llSame   = .F.   && Cost item is not same style component.
    IF !EMPTY(lcNFabItem)
      IF lcStyleTyp='T' .AND. lcNFabItem = lcMajor
        *** Component Fabric and Cost Sheet Material Cannot be the same. ***
        *** <  Ok  > ***
        =gfModalGen("INM38020B00000" , "DIALOG")
        lcNFabItem = SPACE(7)
        _CUROBJ    = OBJNUM(lcNFabItem)
        SELECT (lnAlias)
        RETURN
      ENDIF
      *-- Default from the fabric file according to the selected fabric.
      lcNUom6     = Fabric.UomUse
      lcNUom7     = Fabric.UomUse
      lnNUntCst6  = Fabric.CostBuy/Fabric.Conv
      lnNUntCst7  = Fabric.CostBuy/Fabric.Conv
      
      lnNUntQt6B  = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))
      
      *B602624,1 Reham On 03/03/99   *** Begin ***
      *B602624,1 Round the gross units to 3 dicimals.
      *lnNUntQt6A  = lnNEsUnt6A*(1+(lnNWstg6/100))
      lnNUntQt6A  = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)
      
      *lnNUntQty7  = lnNEstUnt7*(1+(lnNWstg7/100))
      lnNUntQty7  = ROUND(lnNEstUnt7*(1+(lnNWstg7/100)) , 3)
      
      *lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
      lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))
      
      *lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
      lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
      *B602624,1 Reham On 03/03/99   *** End   ***
      
      lcFCsItmDs  = Fabric.Desc
      *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
      *E500348,1                lcStyleTyp <> "T" from the IF command to do 
      *E500348,1                it in all cost sheet types.
      *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
      IF !EMPTY(laAStySeg[1,1])
      *E500348,1 KHM 06/04/2000 (End)
        SHOW GET lcNUom7  DISABLE
      ELSE
        SHOW GET lcNUom6  DISABLE
      ENDIF
      SHOW GET lcFCsItmDs ENABLE
    ELSE
      _CUROBJ = OBJNUM(lcNFabItem)
    ENDIF
  *-- If the cost item is trim.
  CASE lcCstType = 'T'
    llSame   = .F.   && Cost item is not same style component.
    lcNUom6  = 'EAC'
    lcNUom7  = 'EAC'
    llBrowse = IIF("?" $ lcNFabItem , .T. , llBrowse)
    *-- If the system was setup to use non inventory trims.
    IF llTrimInv
      llItmTrInv = IIF(SEEK(lcNFabItem,'FABRIC') , .T. , .F.)
      
      *** Trim not found in the file.  Keep as non-inventory item? ***
      *** < Yes > - < No > ***
      IF llBrowse .OR. (!llItmTrInv .AND. gfModalGen("INM38022B00006" , "DIALOG") = 2)
        SELECT Fabric
        IF !FaBrow(@lcNFabItem , '*')
          llBrowse   = .F.
          lcNFabItem = SPACE(7)
          _CUROBJ    = OBJNUM(lcNFabItem)
          SELECT (lnAlias)
          RETURN
        ENDIF
        llItmTrInv = .T.
      ENDIF
    ELSE
      *-- If the system was setup to not use non inventory trims.
      IF llBrowse .OR. !SEEK(lcNFabItem,'FABRIC')
        SELECT Fabric
        IF !FaBrow(@lcNFabItem , '*')
          llBrowse   = .F.
          lcNFabItem = SPACE(7)
          _CUROBJ    = OBJNUM(lcNFabItem)
          SELECT (lnAlias)
          RETURN
        ENDIF
        *llItmTrInv = .T.
      ENDIF
      *B602339,1 Reham On 12/14/98   *** Begin ***
      *B602339,1 Fix the error of getting zero cost for trim if not 
      *B602339,1 Trim Inventory Maintained
      llItmTrInv = IIF(SEEK(lcNFabItem,'FABRIC') , .T. , .F.)
      *B602339,1 Reham On 12/14/98   *** End   ***
    ENDIF
    IF llItmTrInv .AND. !EMPTY(lcNFabItem)
      *-- If entering material cost sheet , trim cannot be same as main item.
      IF lcStyleTyp='T' .AND. lcNFabItem = lcMajor
        llBrowse = .F.
        *** Component Trim and Cost Sheet Material Cannot be the same. ***
        *** <  OK  > ***
        =gfModalGen("INM38024B00000" , "DIALOG")
        lcNFabItem = SPACE(7)
        _CUROBJ    = OBJNUM(lcNFabItem)
        SELECT (lnAlias)
        RETURN
      ENDIF
      *-- Default the variables from the fabric file for current selected trim.
      lnNUntCst6  = Fabric.CostBuy/Fabric.Conv
      lnNUntCst7  = Fabric.CostBuy/Fabric.Conv
      
      lnNUntQt6B  = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))
      
      *B602624,1 Reham On 03/03/99   *** Begin ***
      *B602624,1 Round the gross units to 3 dicimals.
      *lnNUntQt6A  = lnNEsUnt6A*(1+(lnNWstg6/100))
      lnNUntQt6A  = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)
      
      *lnNUntQty7  = lnNEstUnt7*(1+(lnNWstg7/100))
      lnNUntQty7  = ROUND(lnNEstUnt7*(1+(lnNWstg7/100)) , 3)
      
      *lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
      lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))
      
      *lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
      lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
      *B602624,1 Reham On 03/03/99   *** End   ***      
      
      lcNUom6     = Fabric.UomUse
      lcNUom7     = Fabric.UomUse
      lcFCsItmDs  = Fabric.Desc
      SHOW GET lcFCsItmDs ENABLE
    ENDIF
    llBrowse = .F.
  *-- If the cost type was style.
  CASE lcCstType = 'S'
    IF (llBrowse .OR. !SEEK(PADR(lcNStyItem,lnMajorLen),'Style'))
      CLEAR TYPEAHEAD
      *-- Browse from the available styles.
      lcNStyItem = gfStyBrw("M" , "" , "" , .F.)
    ENDIF
    
    llBrowse = .F.
    *-- If entering style cost sheet, style cost item cannot be the same 
    *-- as the main style major if there is no non-major exist.
    IF !EMPTY(lcNStyItem)
      IF lcStyleTyp <> 'T' .AND. ;
         PADR(lcNStyItem,lnMajorLen) = PADR(lcMajor,lnMajorLen)
        IF llNonMjExt
          llSame   = .T.   && Cost item is the same style component.
        ELSE
          llSame   = .F.   && Cost item is not same style component.
          *** Component style and Cost Sheet style cannot be the same    ***
          *** while there is no non-major exist for the style structure. ***
          *** <  Ok  > ***
          =gfModalGen("INM38025B00000" , "DIALOG")
          lcNStyItem = SPACE(lnMajorLen)
          _CUROBJ    = _CUROBJ
          SELECT (lnAlias)
          RETURN
        ENDIF
      ELSE
        llSame    = .F.   && Cost item is not the same style component.
      ENDIF
      
      *-- If style component same as style parent, disable the dutyable popup.
      IF llSame
        ibNDutyabl = 1
        lcNDutyable = laDutyable[ibNDutyabl,2]
        SHOW GET ibNDutyabl DISABLE
      ELSE
        SHOW GET ibNDutyabl ENABLE
      ENDIF
      
      *-- Default the variables values from the style file for the selected style.
      lnNUntCst6  = Style.TotCost
      lnNUntCst7  = Style.TotCost
      
      lnNUntQt6B  = CEILING(lnNEsUnt6B*(1+(lnNWstg6/100)))
      
      *B602624,1 Reham On 03/03/99   *** Begin ***
      *B602624,1 Round the gross units to 3 dicimals.
      *lnNUntQt6A  = lnNEsUnt6A*(1+(lnNWstg6/100))
      lnNUntQt6A  = ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)
      
      *lnNUntQty7  = lnNEstUnt7*(1+(lnNWstg7/100))
      lnNUntQty7  = ROUND(lnNEstUnt7*(1+(lnNWstg7/100)) , 3)
      
      *lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) * lnNUntCst6 , 2))
      lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))
      
      *lnTotFCst7  = ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) * lnNUntCst7 , 2)
      lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)
      *B602624,1 Reham On 03/03/99   *** End   ***
      
      lcNUom6     = 'EAC'
      lcNUom7     = 'EAC'
      lcSCsItmDs  = Style.Desc1
      SHOW GET lcNUom6    ENABLE
      SHOW GET lcSCsItmDs ENABLE
    ELSE
      _CUROBJ = OBJNUM(lcNStyItem)
    ENDIF
ENDCASE

*-- If cost type is fabric or style or trim exist in the file.
IF lcCstType $ 'FS' .OR. (lcCstType = 'T' .AND. llItmTrInv)
  *-- Refresh the cost & unit objects in the screen.
  *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
  *E500348,1                lcStyleTyp <> "T" from the IF command to do 
  *E500348,1                it in all cost sheet types.
  *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1]) .AND. lcCstType <> "S"
  IF !EMPTY(laAStySeg[1,1]) .AND. lcCstType <> "S"
  *E500348,1 KHM 06/04/2000 (End)
    SHOW GET lnNUntCst7 DISABLE
    SHOW GET lnNEstUnt7 ENABLE
    SHOW GET lnNUntQty7
    SHOW GET lnNWstg7   ENABLE
    SHOW GET lnTotFCst7 DISABLE
    *B802097,1 Reham 04/05/99   *** Begin ***
    *B802097,1 Refresh the UOM field.
    SHOW GET lcNUOM7    DISABLE
    *B802097,1 Reham 04/05/99   *** End   ***
  ELSE
    SHOW GET lnNUntCst6 DISABLE
    SHOW GET lnNWstg6   ENABLE
    IF lcCstType = 'S'
      SHOW GET lnNEsUnt6A DISABLE
      SHOW GET lnNEsUnt6B ENABLE
      SHOW GET lnNUntQt6B ENABLE
    ELSE
      SHOW GET lnNEsUnt6B DISABLE
      SHOW GET lnNEsUnt6A ENABLE
      SHOW GET lnNUntQt6A ENABLE
    ENDIF
    SHOW GET lnTotFCst6 DISABLE
    *B802097,1 Reham 04/05/99   *** Begin ***
    *B802097,1 Refresh the UOM field.
    SHOW GET lcNUOM6    ENABLE
    *B802097,1 Reham 04/05/99   *** End   ***
  ENDIF
ENDIF

*-- If trim & does not exist in the file, open the unit cost object.
IF lcCstType = 'T' .AND. !llItmTrInv
  *E500348,1 KHM 06/04/2000 (Begin) Removing the checkig of 
  *E500348,1                lcStyleTyp <> "T" from the IF command to do 
  *E500348,1                it in all cost sheet types.
  *IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1])
  IF !EMPTY(laAStySeg[1,1])
  *E500348,1 KHM 06/04/2000 (End)
    SHOW GET lnNUntCst7 ENABLE
    SHOW GET lnNEstUnt7 ENABLE
    SHOW GET lnNUntQty7
    SHOW GET lnNWstg7   ENABLE
    SHOW GET lcNUom7    ENABLE
    SHOW GET lnTotFCst7 DISABLE
  ELSE
    SHOW GET lnNUntCst6 ENABLE
    SHOW GET lnNWstg6   ENABLE
    IF lcCstType = 'S'
      SHOW GET lnNEsUnt6A DISABLE
      SHOW GET lnNEsUnt6B ENABLE
      SHOW GET lnNUntQt6B ENABLE
    ELSE
      SHOW GET lnNEsUnt6B DISABLE
      SHOW GET lnNEsUnt6A ENABLE
      SHOW GET lnNUntQt6A ENABLE
    ENDIF
    SHOW GET lcNUom6    ENABLE
    SHOW GET lnTotFCst6 DISABLE
  ENDIF
  SHOW GET lcFCsItmDs ENABLE
ENDIF

*-- If style cost item & there is non major exist, enable the major popup.
IF llNonMjExt .AND. lcCstType = 'S'
  *-- If the style component is the same like the main style, force to
  *-- enter different colors & disable the color class popup.
  IF llSame
    ibClrCls9  = 3
    lnClrClass = 3
    SHOW GET ibClrCls9  DISABLE
    
    llDifColor = .F.  && Flag to know that there is no color entered yet.
    lcCsItmC9  = SPACE(lnNonMjlen)
    lcCsItmC10 = SPACE(6)
    SHOW GET ibItmClr9  ENABLE
    SHOW GET ibCsItmC9  ENABLE
    SHOW GET lcCsItmC9  ENABLE
    _CUROBJ = OBJNUM(ibItmClr9)
  ELSE
    ibClrCls9  = 1
    lnClrClass = 1
    SHOW GET ibClrCls9  ENABLE
  ENDIF
  SHOW GET ibClrCls10 DISABLE
ELSE
  *-- If entering material cost sheet & there is color exist.
  IF llColorExt .OR. lcStyleTyp = "T"
    IF lcCstType = 'S'
      SHOW GET ibClrCls9  ENABLE
      SHOW GET ibClrCls10 DISABLE
    ELSE
      SHOW GET ibClrCls9  DISABLE
      SHOW GET ibClrCls10 ENABLE
    ENDIF
  ELSE
    SHOW GET ibClrCls9  DISABLE
    SHOW GET ibClrCls10 DISABLE
  ENDIF
ENDIF

llBrowse = .F.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvNMfgOpr
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
*! Example   : lfvNMfgOpr()
*!*************************************************************
*
FUNCTION lfvNMfgOpr

*-- Get the mfg operation value.
lcNMfgOpr = laOprCode[puNMfgOpr,2]

*!*************************************************************
*! Name      : lfwStyNSeg
*! Developer : Reham Al-Allamy
*! Date      : 08/31/1997
*! Purpose   : Valid function for popup select segment.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : lfwStyNSeg()
*!*************************************************************
*
FUNCTION lfwStyNSeg

*-- If the cost type is : mfg operation , purchase price or duty, use the 
*-- array that has colors.
IF lcCstType $ "MPD"
  *E500348,1 KHM 06/04/2000 (Begin) Initializing laAStySeg array in case of
  *E500348,1                Material Cost sheet.     
  *DECLARE laAStySeg [ALEN(laStyCSeg,1) , ALEN(laStyCSeg,2)]
  *=ACOPY(laStyCSeg , laAStySeg)
  IF lcStyleTyp = "T"
    DECLARE laAStySeg [1, 9]
    laAStySeg [1, 1] = "Color"
    laAStySeg [1, 2] = "C"
    laAStySeg [1, 8] = "A"
  *E500348,1 KHM 06/04/2000 (End)
  ELSE
    DECLARE laAStySeg [ALEN(laStyCSeg,1) , ALEN(laStyCSeg,2)]
    =ACOPY(laStyCSeg , laAStySeg)
  ENDIF
ELSE
  *-- If the cost type is : style , trim or fabric, use the array that
  *-- has no colors.
  DECLARE laAStySeg [ALEN(laStyNSeg,1) , ALEN(laStyNSeg,2)]
  =ACOPY(laStyNSeg , laAStySeg)
ENDIF

*-- Refresh the non-major or colors popup with the first elements in the array.
SHOW GET puStyNSeg

*!*************************************************************
*! Name      : lfvStyNSeg
*! Developer : Reham Al-Allamy
*! Date      : 08/26/1997
*! Purpose   : Valid function for popup select segment.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : lfvStyNSeg()
*!*************************************************************
*
FUNCTION lfvStyNSeg

*-- Get the selected popup value (All -or- Selected values)
puSelVal = IIF(laAStySeg[puStyNSeg , 8] = "A" , 1 , 2)
SHOW GET puSelVal

*!*************************************************************
*! Name      : lfvSelVal
*! Developer : Reham Al-Allamy
*! Date      : 08/26/1997
*! Purpose   : Valid function for popup Select value.
*!*************************************************************
*! Calls     : gfMover
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : lfvSelVal()
*!*************************************************************
*
FUNCTION lfvSelVal
PRIVATE lnCurRec

DO CASE
  CASE puSelVal = 1
    *-- All Values.
    laAStySeg[puStyNSeg , 8] = "A"
  CASE puSelVal = 2
    *-- Selected values.
    laAStySeg[puStyNSeg , 8] = "S"
    *-- Array no.
    lcNo      = STR(puStyNSeg,1)
    *-- The start position of the segment.
    lnTmpStrt = laAStySeg[puStyNSeg,4]
    *-- The lenght of the current segment.
    lnTmpLen  = LEN(laAStySeg[puStyNSeg,3])
    *-- Get the file name.
    DO CASE
      *-- Free , Other , Domestic , Quality.
      CASE laAStySeg[puStyNSeg , 2] $ "FOTQ"
        lcActFile = "ICSEGVAL"
        lcSelFeld = "Icsegval.cisgvalld"
        *B602213,1 Reham On 11/17/98    *** Begin ***
        *B602213,1 Use the segment # in the style code structure instead of
        *B602213,1 the popup variable.
        *lcActExpr = "Icsegval.cisegno + Icsegval.cisegval = STR(puStyNSeg ,1) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
        lcActExpr = "Icsegval.cisegno + Icsegval.cisegval = STR(laAStySeg[puStyNSeg,9],1) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
        *B602213,1 Reham On 11/17/98    *** End   ***
      *-- Color , Division , Seazon , Group.
      CASE laAStySeg[puStyNSeg , 2] $ "CDZG"
        lcActFile = "CODES"
        lcSelFeld = "Codes.cDiscrep"
        DO CASE
          CASE laAStySeg[puStyNSeg , 2] = "C"
            *E301077,16 Reham On 03/08/99  *** Begin ***
            *lcActExpr = "cComp_Id + cFld_Name + cCode_No = gcAct_Comp + PADR('COLOR',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            *E500348,1 KHM 06/04/2000 (Begin) If Material cost sheet then
            *E500348,1                assing the appropriate values 
            *E500348,1                according to Cost sheet type to lcActExpr.
            *lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('COLOR',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            IF lcStyleTyp = "T"
              lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('COLOR',10) + Fabric.Color"
            *E500348,1 KHM 06/04/2000 (End)
            ELSE
              lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('COLOR',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            ENDIF
            *E301077,16 Reham On 03/08/99  *** End   ***
          CASE laAStySeg[puStyNSeg , 2] = "D"
            *E301077,16 Reham On 03/08/99  *** Begin ***
            *lcActExpr = "cComp_Id + cFld_Name + cCode_No = gcAct_Comp + PADR('CDIVISION',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('CDIVISION',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            *E301077,16 Reham On 03/08/99  *** End   ***
          CASE laAStySeg[puStyNSeg , 2] $ "Z"
            *E301077,16 Reham On 03/08/99  *** Begin ***
            *lcActExpr = "cComp_Id + cFld_Name + cCode_No = gcAct_Comp + PADR('SEASON',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('SEASON',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            *E301077,16 Reham On 03/08/99  *** End   ***
          CASE laAStySeg[puStyNSeg , 2] $ "G"
            *E301077,16 Reham On 03/08/99  *** Begin ***
            *lcActExpr = "cComp_Id + cFld_Name + cCode_No = gcAct_Comp + PADR('CSTYGROUP',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            lcActExpr = "cDefCode + cFld_Name + cCode_No = 'N' + PADR('CSTYGROUP',10) + SUBSTR(Style , laAStySeg[puStyNSeg,4] , LEN(laAStySeg[puStyNSeg,3]))"
            *E301077,16 Reham On 03/08/99  *** End   ***
        ENDCASE
        lcActExpr = lcActExpr + ".AND. Codes.crltfield = 'N'"
      *-- Size Scale.
      CASE laAStySeg[puStyNSeg , 2] = "S"
        lcActFile = "SCALE"
        lcSelFeld = "Scale.cscl_desc"
        lcActExpr = "Scale.Type + Scale.Scale = 'S' + Style.Scale"
    ENDCASE
    
    *-- Select the available majors for the main style.
    *E500348,1 KHM 06/04/2000 (Begin) Check if Material cost sheet then
    *E500348,1                select from fabric file.
    IF lcStyleTyp <> "T"
    *E500348,1 KHM 06/04/2000 (End)    
      SELECT STYLE
      lnCurRec = RECNO()
      SELECT DISTINCT SUBSTR(Style , lnTmpStrt , lnTmpLen) + " * " + &lcSelFeld ;
        FROM STYLE , &lcActFile ;
        WHERE STYLE = PADR(lcMajor,lnMajorLen) .AND. &lcActExpr;
        INTO ARRAY laSegSorc&lcNo
    
      lnCountRec = _TALLY
      *-- Restore the record pointer in the style file.
      IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT("STYLE")
        GOTO lnCurRec IN STYLE
      ENDIF
    *E500348,1 KHM 06/04/2000 (Begin) Adding the selec sql for Material cost
    *E500348,1                sheet to select from fabric file.
    ELSE
      SELECT Fabric
      lnCurRec = RECNO()
      SELECT DISTINCT Color+ " * " + &lcSelFeld ;
        FROM Fabric, &lcActFile;
       WHERE Fabric+Color = lcMajor .AND. &lcActExpr;
       INTO ARRAY laSegSorc&lcNo
    
      lnCountRec = _TALLY
      *-- Restore the record pointer in the fabric file.
      IF BETWEEN(lnCurRec, 1, RECCOUNT("Fabric"))
        GOTO lnCurRec IN Fabric
      ENDIF
      *E500348,1 KHM 06/04/2000 (End)
    ENDIF    
    *-- If there is something selected, use the mover to select the non-majors.
    IF lnCountRec > 0
      *C037892,1 MHM 06/15/2004 Get Color for Custom sales order cost sheet[Start]
      IF ASCAN(laEvntTrig,PADR('SOGTCOLR',10))<>0
        =gfDoTriger('SOALCST',PADR('SOGTCOLR',10))
      ENDIF
      *C037892,1 MHM [End]
      =gfMover(@laSegSorc&lcNo , @laSegTrgt&lcNo , "Select "+laAStySeg[puStyNSeg , 1])
      laAStySeg[puStyNSeg , 8] = IIF(!EMPTY(laSegTrgt&lcNo[1]) , "S" , "A")
    ELSE
      laAStySeg[puStyNSeg , 8] = "A"
    ENDIF
ENDCASE

puSelVal = IIF(laAStySeg[puStyNSeg , 8] = "A" , 1 , 2)
SHOW GET puSelVal

IF lcCstType $ "MPD"
  laStyCSeg[puStyNSeg , 8] = laAStySeg[puStyNSeg , 8]
ELSE
  laStyNSeg[puStyNSeg , 8] = laAStySeg[puStyNSeg , 8]
ENDIF

*!*************************************************************
*! Name      : lfvAcptNew
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Add new item to the Style/Material Cost Sheet.
*!*************************************************************
*! Calls     : lfCreatMsk, MFCSTSZ.PRG, lfUpdPerc, lfUpdCost
*!           : lfIntNewCS, gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvAcptNew()
*!*************************************************************
*
FUNCTION lfvAcptNew
PRIVATE lnCnt
*E301411,1 SSH 31/05/20 [Begin] Variable to indicate if we are in new
*E301411,1 SSH          session if .T.
llNewSess = .T.
*E301411,1 SSH 31/05/20 [End]
*B602385,1 Reham On 01/06/99   *** Begin ***
*B602385,1 Do not accept trim or fabric component without 
*B602385,1 a color if there is no non major.
*-- Check if entered a constant color .Or. atleast one different color
*-- in case of style component, trim, fabric & there is non-major.
*IF llNonMjExt .AND. ((lnClrClass = 2 .AND. ;
                      ((lcCstType = "S"  .AND. EMPTY(lcCsItmC9)) .OR. ;
                       (lcCstType $ "FT" .AND. EMPTY(lcCsItmC10)))) .OR. ;
                     (lnClrClass = 3 .AND. !llDifColor)))
IF (llNonMjExt .AND. ((lnClrClass = 2 .AND. ;
                      ((lcCstType = "S"  .AND. EMPTY(lcCsItmC9)) .OR. ;
                       (lcCstType $ "FT" .AND. EMPTY(lcCsItmC10)))) .OR. ;
                     (lnClrClass = 3 .AND. !llDifColor))) ;
   .OR. (!llNonMjExt .AND. lcCstType $ "FT" .AND. EMPTY(lcCsItmC10))
*B602385,1 Reham On 01/06/99   *** End   ***

  *B602121,1 Reham On 10/28/98   *** Begin ***
  *B602121,1 Remove the commenting of the warning message.
  *** You have to enter a color. ***
  *** < Ok > ***
  =gfModalGen("INM38128B00000" , "DIALOG")
  *B602121,1 Reham On 10/28/98   *** End   ***
  _CUROBJ  = IIF(lcCstType='S',OBJNUM(lcCsItmC9),OBJNUM(lcCsItmC10))
  RETURN
ENDIF

*-- Check that cost item is not empty
IF lcCstType $ 'FTS' .AND. IIF(lcCstType='S',EMPTY(lcNStyItem),EMPTY(lcNFabItem))
  *** You have to select a cost item. ***
  *** < Ok > ***
  =gfModalGen("INM38001B00000" , "DIALOG")
  _CUROBJ  = IIF(lcCstType='S',OBJNUM(lcNStyItem),OBJNUM(lcNFabItem))
  RETURN
ENDIF

*-- Check that MFG code is not empty
IF lcCstType = 'M' .AND. EMPTY(lcMfgCode)
  *** You have to select a cost item. ***
  *** < Ok > ***
  =gfModalGen("INM38001B00000" , "DIALOG")
  _CUROBJ  = OBJNUM(lnNMfgCode)   
  RETURN
ENDIF

*-- Check that inventory maintained trim exists in the fabric file
*-- when the system is setup to link with AP.
IF lcCstType = 'T' .AND. !llTrimInv .AND. !SEEK(lcNFabItem,'FABRIC')
  *** Trim not found in the file. ***
  *** <  Ok  > ***
  =gfModalGen("INM38014B00000" , "DIALOG")
  _CUROBJ  = OBJNUM(lcNFabItem)   
  RETURN
ENDIF

*-- Check that unit quantity is greater than zero.
IF lnNEsUnt6A <= 0 
  *** Unit {quantity} must be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM38019B00000" , "DIALOG" , "quantity")
  IF lcStyleTyp <> "T" .AND. !EMPTY(laAStySeg[1,1]) .AND. lcCstType <> "S"
    _CUROBJ  = OBJNUM(lnNEstUnt7)
  ELSE
    IF lcCstType = 'S'
      _CUROBJ  = OBJNUM(lnNEsUnt6B)
    ELSE
      _CUROBJ  = OBJNUM(lnNEsUnt6A)
    ENDIF
  ENDIF
  RETURN
ENDIF

*-- If the cost type was : style , fabric or inventory trim & the colors 
*-- method was " Same as Color" and return false from the cross refrencing
*-- function, do not accept anything.
IF (lcCstType $ 'SF' .OR. (lcCstType = 'T' .AND. llItmTrInv)) .AND. !lfvCScheme()
  RETURN
ENDIF

DO CASE
  *-- If cost type is style component.
  CASE lcCstType = "S"
    DO CASE
      *-- Color same as.
      CASE lnClrClass = 1
        INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
             VALUES (STUFF(lcStyMask , 1 , lnMajorLen , PADR(lcMajor,lnMajorLen)) , ;
                     STUFF(lcStyMask , 1 , lnMajorLen , PADR(lcNStyItem,lnMajorLen)) , "")
      *-- Color is constant.
      CASE lnClrClass = 2
        INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
             VALUES (STUFF(lcStyMask , 1 , lnMajorLen , PADR(lcMajor,lnMajorLen)) , ;
                     PADR(lcNStyItem,lnMajorLen)+ lcSeprator+ PADR(lcCsItmC9,lnNonMjLen)+ ;
                          IIF(llExtSizSc,lcSizeSep+REPLICATE("*",IIF(!EMPTY(lcSizeSep) , lnSizeLen-1 , lnSizeLen)),"") , "")
      *-- Color is different.
      CASE lnClrClass = 3
        FOR lnCnt = 1 TO ALEN(laColors9,1)
          IF EMPTY(laColors9[lnCnt,3])
            LOOP
          ENDIF
          *-- If there is extended size scale.
          IF llExtSizSc
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                 VALUES (PADR(lcMajor,lnMajorLen) + lcSeprator + PADR(laColors9[lnCnt,1],lnNonMjLen) + ;
                         lcSizeSep+REPLICATE("*",IIF(!EMPTY(lcSizeSep) , lnSizeLen-1 , lnSizeLen)) , ;
                         PADR(lcNStyItem,lnMajorLen)+lcSeprator+PADR(laColors9[lnCnt,3],lnNonMjLen) + ;
                         lcSizeSep+REPLICATE("*",IIF(!EMPTY(lcSizeSep) , lnSizeLen-1 , lnSizeLen)) , ;
                         "" , laColors9[lnCnt,4])
          ELSE
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                 VALUES (PADR(lcMajor,lnMajorLen) + lcSeprator + PADR(laColors9[lnCnt,1],lnNonMjLen) , ;
                         PADR(lcNStyItem,lnMajorLen)+lcSeprator+PADR(laColors9[lnCnt,3],lnNonMjLen) , ;
                         "" , laColors9[lnCnt,4])
          ENDIF
        ENDFOR
    ENDCASE
  *-- If the cost type was : fabric, trim, purchase price, duty or mfg operation.
  CASE lcCstType $ "FTPDM"
    *-- In case of style cost sheet (Imorted or Manufactered)
    IF lcStyleTyp <> "T"
      *-- If there is no non-major for the style structure & the cost type
      *-- is fabric or trim no need to create maks.
      IF !llNonMjExt .AND. lcCstType $ "FT"
        *B602385,1 Reham On 01/05/99   *** Begin ***
        *B602385,1 If there is no non major And the component type is fabric or trim.
        IF llExtSizSc
          *B602385,1 If there is scale in the non-major.
          *-- If select all majors.
          IF laAStySeg[1,8] = "A"
            lcCurMask = PADR(lcMajor,lnMajorLen) + lcSeprator + ;
                        REPL('*',LEN(laAStySeg[1,3])) + ALLTRIM(laAStySeg[1,6])
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
                 VALUES (lcCurMask , lcNFabItem , lcCsItmC10)
          ELSE
            *-- If select specific majors.
            FOR lnCount = 1 TO ALEN(laSegTrgt1,1)
              lcCurMask = PADR(lcMajor,lnMajorLen) + lcSeprator + ;
                          PADR(laSegTrgt1[lnCount],LEN(laAStySeg[1,3])) + ;
                          ALLTRIM(laAStySeg[1,6])
              INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
                   VALUES (lcCurMask , lcNFabItem , lcCsItmC10)
            ENDFOR
          ENDIF
        ELSE
        *B602385,1 Reham On 01/05/99   *** End   ***
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
               VALUES (PADR(lcMajor,lnMajorLen) , lcNFabItem , lcCsItmC10)
        ENDIF
      ELSE
        *-- If there is non-major for the style structure.
        *-- Call function to prepare the Mask, item, item color.
        =lfCreatMsk(PADR(lcMajor,lnMajorLen) + lcSeprator)
      ENDIF
    ELSE
      *-- In case of Material cost sheet & the cost item is fabric or trim.
      IF lcCstType $ "FT"
        DO CASE
          *-- Color same as.
          CASE lnClrClass = 1
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
                 VALUES ("******" , PADR(lcNFabItem,7) , "******")
          *-- Color is constant.
          CASE lnClrClass = 2
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
                 VALUES ("******" , PADR(lcNFabItem,7) , lcCsItmC10)
          *-- Color is different.
          CASE lnClrClass = 3
            FOR lnCnt = 1 TO ALEN(laColors10,1)
              IF EMPTY(laColors10[lnCnt,3])
                LOOP
              ENDIF
              INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                   VALUES (laColors10[lnCnt,1] , PADR(lcNFabItem,7) , ;
                   laColors10[lnCnt,3] , laColors10[lnCnt,4])
            ENDFOR
        ENDCASE
      ELSE
        *-- In case of Material cost sheet & the cost item is mfg operation.
        *E500348,1 KHM 06/04/2000 (Begin) Check If choice of popup was 
        *E500348,1                selected values then insert into lcTmpMask
        *E500348,1                file the selected colors.
        *INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
             VALUES ("******" , "" , "")
        IF laAStySeg[puStyNSeg , 8] = "S"
          lcArryNo = STR(puStyNSeg,1)
          FOR lnCntTArr = 1 TO ALEN(laSegTrgt&lcArryNo)
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
                  VALUES (ALLTRIM(LEFT(laSegTrgt&lcArryNo[lnCntTArr,1],6)),;
                         "" , "")
          ENDFOR     
        ELSE
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr) ;
             VALUES ("******" , "" , "")
        ENDIF
        *E500348,1 KHM 06/04/2000 (End)
      ENDIF
    ENDIF
ENDCASE

*-- Collect all the sizes for the existed scales cursor (lcTmpMask)
lnMaskCnt = 0

IF lcStyleTyp = "T"
  SELECT FABRIC
  SET ORDER TO TAG FABRIC
  
  SELECT (lcTmpMask)
  LOCATE FOR "*" $ &lcTmpMask..cItmMask
  IF FOUND()
    lnMaskCnt = 1
  ENDIF
  GO TOP
  *-- Check if all existed masks are vaild.
  SCAN FOR !("*" $ cItmMask)
    REPLACE &lcTmpMask..lDelete WITH .T.
    SELECT FABRIC
    =SEEK(PADR(lcMajor,7))
    SCAN REST WHILE PADR(Fabric,7) = PADR(lcMajor,7) ;
                FOR FABRIC.Color   = PADR(&lcTmpMask..cItmMask,6)
      REPLACE &lcTmpMask..lDelete WITH .F.
      lnMaskCnt = lnMaskCnt + 1
    ENDSCAN
    SELECT (lcTmpMask)
  ENDSCAN
  SET ORDER TO TAG CFABRIC IN FABRIC
ELSE
  *-- Define an array to hold the available scales.
  DECLARE laScale[1,3]
  laScale   = ""
  SELECT STYLE
  SET ORDER TO TAG STYLE
  
  *-- Scan to check that all the entered masks are vaild.
  SELECT (lcTmpMask)
  SCAN
    REPLACE &lcTmpMask..lDelete WITH .T.
    SELECT STYLE
    =SEEK(PADR(lcMajor,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(lcMajor,lnMajorLen) ;
                  FOR LIKE(STRTRAN(&lcTmpMask..cItmMask , "*" , "?") , STYLE.Style)
      REPLACE &lcTmpMask..lDelete WITH .F.
      *-- If cost sheet items based on sizes, select the available sizes.
      IF lcCstType = "S" .OR. (llOnSize .AND. lcStyleTyp <> "T")
        IF SEEK("S" + Style.Scale , "SCALE")
          lcCurSize = ""
          FOR lnCnt = 1 TO Scale.Cnt
            lcCnt     = STR(lnCnt,1)
            lcCurSize = lcCurSize + IIF(EMPTY(lcCurSize) , "" , ",") + lcCnt
          ENDFOR
          
          *-- Only if not style component.
          IF lcCstType <> "S" .AND. !(PADR(Scale.Scale,3) $ ALLTRIM(&lcTmpMask..mSizes))
            SELECT (lcTmpMask)
            REPLACE mSizes WITH ALLTRIM(mSizes) + PADR(Scale.Scale,3) + ","
          ENDIF
          
          *-- Collect the scales for the current mask.
          
          *B606979,1 AMH Get the extended size scale width [Start]
          *lcScale = PADR(Style.Scale,1) + " * " + LOOKUP(Scale.cscl_desc , "S"+Style.Scale , Scale.Scale , "SCALE")
          lcScale = PADR(Style.Scale,3) + " * " + LOOKUP(Scale.cscl_desc , "S"+Style.Scale , Scale.Scale , "SCALE")
          *B606979,1 AMH [End]
          
          IF ASCAN(laScale , PADR(Scale.Scale,3)) = 0
            IF !EMPTY(laScale[1,1])
              DECLARE laScale[ALEN(laScale,1)+1,3]
            ENDIF
            laScale[ALEN(laScale,1),1] = lcScale
            laScale[ALEN(laScale,1),2] = lcCurSize
            laScale[ALEN(laScale,1),3] = PADR(Scale.Scale,3)
          ENDIF
        ENDIF
      ENDIF
      lnMaskCnt = lnMaskCnt + 1
    ENDSCAN
    SELECT (lcTmpMask)
  ENDSCAN
  SET ORDER TO TAG CSTYLE IN STYLE
ENDIF

*-- If none of the prepared masks exist in the style file, do not save.
IF lnMaskCnt = 0
  *** This Cost Item does not apply to any style. ***
  *** <  OK  > ***
  =gfModalGen("TRM38046B00000" , "DIALOG")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*-- Delete the non valid masks.
SELECT (lcTmpMask)
DELETE ALL FOR lDelete

*-- If cost sheet items based on sizes, select the available sizes.
IF lcCstType = "S" .OR. (llOnSize .AND. lcStyleTyp <> "T")
  IF lcCstType = "S"
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
    SET ORDER TO TAG STYLE IN STYLE
    SELECT (lcTmpMask)
    SCAN
      SELECT STYLE
      =SEEK(PADR(&lcTmpMask..Item,lnMajorLen) , "STYLE")
      SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpMask..Item,lnMajorLen) ;
                  FOR LIKE(STRTRAN(&lcTmpMask..Item , "*" , "?") , STYLE.Style)
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
      SELECT (lcTmpMask)
    ENDSCAN
    SET ORDER TO TAG CSTYLE IN STYLE
    
    *-- Adjust the assigning of the style component sizes to the
    *-- style parent sizes.
    DECLARE laMainScal[1,2]
    laMainScal = ""
    lnChldSz   = 2
    FOR lnCnt1 = 1 TO ALEN(laScale,1)
      DECLARE laPrntSize[1]
      laPrntSize = ""
      =gfSubstr(ALLTRIM(laScale[lnCnt1,2]),@laPrntSize,",")
      FOR lnCnt2 = 1 TO ALEN(laPrntSize,1)
        IF !EMPTY(laMainScal[1,1])
          DIMENSION laMainScal[ALEN(laMainScal,1)+1 , 2]
        ENDIF
        laMainScal[ALEN(laMainScal,1),1] = "*"+SUBSTR(laScale[lnCnt1,3],1,3)+","+laPrntSize[lnCnt2]
        laMainScal[ALEN(laMainScal,1),2] = "#"+SUBSTR(laChldSz[lnChldSz,3],1,3)+","+STR(laChldSz[lnChldSz,2],1)
        lnChldSz   = lnChldSz + IIF(lnChldSz < ALEN(laChldSz,1) , 1 , 0)
      ENDFOR
    ENDFOR
    
    llSaveSize = .F.  && Flag to know if save the sizes or not.
    
    *-- Call the size program & screen.
    DO (gcAppHome+"MFCSTSZ") WITH lcCstType
    
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
      
      lcPrntSiz = lcPrntSiz + lcOldScal + "~" + lcSizes + CHR(13)
      *-- Update the sizes in the mask file.
      SELECT (lcTmpMask)
      SCAN
        REPLACE &lcTmpMask..mSizes     WITH lcPrntSiz ;
                &lcTmpMask..mSzCrosRef WITH lcCrosRef 
      ENDSCAN
    ELSE
      *-- Update the sizes with null in the mask file.
      SELECT (lcTmpMask)
      SCAN
        REPLACE &lcTmpMask..mSizes     WITH "" ;
                &lcTmpMask..mSzCrosRef WITH ""
      ENDSCAN
    ENDIF
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
      ENDFOR
      *-- Copy the source array to the target array.
      DECLARE laSelSiz[ALEN(laAllSiz,1) , ALEN(laAllSiz,2)]
      =ACOPY(laAllSiz , laSelSiz)
    ENDIF
    
    *-- Define a popup from array laAllSiz that hold all the sizes 
    *-- for the first scale in the popup.
    DEFINE POPUP puAllSiz MARGIN RELATIVE SCROLL MARK CHR(16)
    FOR lnCount = 1 TO ALEN('laAllSiz',1)
      DEFINE BAR lnCount OF puAllSiz PROMPT (ALLTRIM(laAllSiz[lnCount,1]))
      SET SKIP OF BAR lnCount OF puAllSiz .T.
    ENDFOR
    
    llSaveSize = .F.
    
    *-- Call the size program & screen.
    DO (gcAppHome+"MFCSTSZ") WITH lcCstType
    
    *-- Update the sizes in the mask file.
    SELECT (lcTmpMask)
    SCAN
      *-- If there was scales selected.
      IF llSaveSize
        lcScale = ALLTRIM(&lcTmpMask..mSizes)
        IF !EMPTY(lcScale)
          lcSizes = ""
          FOR lnCnt = 1 TO OCCURS("," , lcScale)
            lcCurScale = SUBSTR(lcScale,1,3)
            lnArrayNo  = ASCAN(laScale , lcCurScale)
            IF lnArrayNo > 0
              lnArrayNo  = ASUBSCRIPT(laScale , lnArrayNo , 1)
              IF !EMPTY(laScale[lnArrayNo , 2])
                lcSizes   = lcSizes + lcCurScale + "~" + laScale[lnArrayNo , 2] + CHR(13)
              ENDIF
            ENDIF
            IF LEN(lcScale) > 5
              lcScale = SUBSTR(lcScale,5)
            ELSE
              EXIT
            ENDIF
          ENDFOR
          REPLACE &lcTmpMask..mSizes WITH lcSizes
        ENDIF
      ELSE
        REPLACE &lcTmpMask..mSizes WITH ""
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

*-- Scan the styles to get the primary fabric from the style file if :
*-- 1- The current added cost item is fabric.
*-- 2- Set the current fabric as primary fabric.
IF lcCstType = "F" .AND. !EMPTY(lcNFabItem) .AND. cbPrim
  llExitPrim = .F.   && Flag to exit from the 2 scan.
  llSetPrim  = .F.   && Flag to know if there is a primary fabric or not.
  SELECT (lcTmpMask)
  SCAN
    *-- Scan in the style file to check the primary fabric.
    SELECT STYLE
    =SEEK(PADR(lcMajor,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(lcMajor,lnMajorLen) ;
                FOR LIKE(STRTRAN(&lcTmpMask..cItmMask , "*" , "?") , STYLE.Style)
      *-- If there is at least 1 primary fabric not empty.
      IF !EMPTY(STYLE.Fabric)
        *-- Set the flag to true.
        llSetPrim = .T.
      ENDIF
      
      *-- If the primary fabric is different from what is saved in style file.
      IF !EMPTY(STYLE.Fabric) .AND. !(PADR(lcNFabItem,7) == STYLE.Fabric)
        llExitPrim = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF llExitPrim
      EXIT
    ENDIF
    SELECT (lcTmpMask)
  ENDSCAN
  
  *-- If exit from the 2 scan because the primary fabric is different, warn 
  *-- the user from overriding the primary fabric.
  IF llSetPrim .AND. llExitPrim
    *** One or more style(s) have different primary fabric. ***
    *** Overwrite with the current selected fabric?         ***
    *** < YES > - < NO > ***
    *IF gfModalGen("QRM38125B00006" , "DIALOG") = 1
    IF gfModalGen("QRM38125B00006" , "DIALOG", Style.Fabric+"|"+lcNFabItem) = 1    
      lcPrimFbr = lcNFabItem
    ELSE
      lcPrimFbr = " "
    ENDIF
  ELSE
    lcPrimFbr = lcNFabItem
  ENDIF
ENDIF

*-- Update the temp. BOM file with the valid masks.
*E301411,1 SSH 31/05/20 variable to use in update non major detail operation
llUpdNMDet = .T.
*E301411,1 SSH 31/05/20 [END]
SELECT (lcTmpMask)
SCAN
  SELECT (lcTmpBom)
  *E301411,1 SSH 31/05/20 [Begin] Get the new mfgCode LineNo.
  IF llPwInst .AND. lcCstType='M' .AND. lcStyleTyp = 'M'
    DIMENSION lnToCnt[1,1]
    lnToCnt[1,1] = 0
    lcTempMfg = mfgcode
    lcTempMask = &lcTmpMask..cItmMask
    lcTempSize = &lcTmpMask..mSizes
    SELECT MAX(nLineNo) FROM (lcTmpBom) WHERE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                                              PADR(IIF(llFromCtk , laData[2],laData[1] ),19);
                                     .AND. mfgcode == laMfgCode[lnNMfgCode,2];
                         INTO ARRAY lnToCnt
                                    
    lnToCnt[1,1] = lnToCnt[1,1] + 1
    IF llUpdNMDet
      =lfAddNMDet()
      llUpdNMDet = .F.
    ENDIF  
    SELECT (lcTmpBom)
  ENDIF
  *E301411,1 SSH 31/05/20 [End] 
  APPEND BLANK
  IF llPwInst .AND. lcCstType='M' .AND. lcStyleTyp = 'M'
    REPLACE nLineNo WITH MAX(lnToCnt[1,1],1)
  ENDIF
  REPLACE cItmMajor  WITH PADR(lcMajor,19)   ;
          TYP        WITH STR(lnCostType,1)    ;
          cItmMask   WITH &lcTmpMask..cItmMask ;
          ITEM       WITH &lcTmpMask..Item     ;
          ICLR       WITH &lcTmpMask..IClr     ;
          mSizes     WITH &lcTmpMask..mSizes   ;
          mSzCrosRef WITH &lcTmpMask..mSzCrosRef ;
          DESC       WITH IIF(lcCstType $ 'PMD' , lcMCsItmDs , IIF(lcCstType='S' , lcSCsItmDs , lcFCsItmDs)) ;
          MfgCode    WITH IIF(lcCstType $ 'PMD' , lcMfgCode , "") ;
          cOprCode   WITH lcNMfgOpr   ;
          UOM        WITH lcNUom6     ;
          UNTCOST    WITH IIF((lcCstType $ "FS" .OR. ;
                          (lcCstType ="T" .AND. llItmTrInv)) .AND. ;
                          (ibClrCls9 = 3 .OR. ibClrCls10 = 3) , ;
                          &lcTmpMask..nCost , lnNUntCst6)
  
  *B602624,1 Reham On 03/03/99   *** Begin ***
  *B602624,1 Round the gross units to 3 dicimals.
  *REPLACE nBomTotQty WITH IIF(lcCstType = "S" , ;
                          CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) , ;
                          lnNEsUnt6A*(1+(lnNWstg6/100)))
  REPLACE nBomTotQty WITH IIF(lcCstType = "S" , ;
                          CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) , ;
                          ROUND(lnNEsUnt6A*(1+(lnNWstg6/100)) , 3)) ;
          nEstBomQty WITH IIF(lcCstType = "S"  , lnNEsUnt6B , lnNEsUnt6A) ;
          nBomWastge WITH lnNWstg6 ;
          TOTCOST    WITH UNTCOST * nBomTotQty ;
          nPercent   WITH lnNCstPrc6  ;
          TRIM_INVT  WITH IIF(lcCstType $ 'PDM' , .F. , llItmTrInv) ;
          lMaterial  WITH (lcStyleTyp = 'T') ;
          cCatGTyp   WITH lcCstType   ;
          lBasOnSiz  WITH IIF(lcStyleTyp = "T" , .F. , llOnSize) ;
          cCostStat  WITH IIF(lcCstType = "D" , " " , lcNDutyable) ;
          cStatus    WITH "A" ;
          cAdd_User  WITH gcUser_Id ;
          cAdd_Time  WITH gfGetTime() ;
          dAdd_Date  WITH gdSysDate
  *B602624,1 Reham On 03/03/99   *** End   ***
 
  *C037892,1 MHM 06/15/2004 Validate PopUp For  sales order cost sheet [Start]
  IF ASCAN(laEvntTrig,PADR('SODTYPOP',10))<>0
    =gfDoTriger('SOALCST',PADR('SOUPDUTY',10))
  ENDIF
  *C037892,1 MHM [End]
  
  *E500402,1 AMH Saving the marker field [Start]
  IF !EMPTY(laAStySeg[1,1])
    REPLACE CMARKER WITH lcMarker7
  ELSE
    REPLACE CMARKER WITH lcMarker6
  ENDIF

  *B604899,1 AMH Update custom fields of Cathy Daniels [Start]
  IF ASCAN(laEvntTrig , PADR('UPDBOMCA',10)) <> 0
    REPLACE MARKER_LEN WITH lnMark_Len,;
            OUT_GOODS  WITH lnOut_Goods,;
            TOT_UNITS  WITH lnTot_Units,;
            LOSS       WITH lnLoss
    STORE 0 TO lnMark_Len, lnOut_Goods, lnTot_Units, lnLoss
  ENDIF
  *B604899,1 AMH [End]

  *E500402,1 AMH [End]
  
  lnBomLnNo = lnBomLnNo + 1
  *-- Calculate the total cost for the Mfg cost item or Duty 
  *-- cost item if it has percent.
  IF lcCstType $ "MD" .AND. &lcTmpBom..nPercent <> 0
    =lfUpdPerc(lcCstType)
  ENDIF
  *-- If there is percent in any MFG or Duty cost item, calculate the
  *-- cost from the participated items.
  IF &lcTmpBom..cCatGTyp = 'P' .OR. &lcTmpBom..cCostStat = '1'
    =lfUpdCost(+1 , 0)
  ENDIF
  
  SELECT (lcTmpMask)
ENDSCAN

*-- Scan all the added cost items to update its cost if style cost sheet only.
IF lcStyleTyp <> "T"
  SELECT (lcTmpMask)
  SCAN
    *-- Call local function to calculate the current parent cost in its
    *-- parent records...
    =lfPrntCost(&lcTmpMask..cItmMask)
    SELECT (lcTmpMask)
  ENDSCAN
ENDIF

SELECT (lcTmpBom)

*-- If the cost item was mfg operation set to be considered as parent 

*-- operation, add it to the mfg operations array.
IF lcCstType = "M" .AND. !EMPTY(lcMfgCode) .AND. lnNMfgCode >= 1
  DECLARE laOprRltFd[1,2]
  laOprRltFd[1,1] = 'LMFGOPR'
  laOprRltFd[1,2] = 'llMfgOpr'
  =gfRltFld(lcMfgCode , @laOprRltFd , "MFGCODE")
  IF llMfgOpr .AND. ASCAN(laOprCode , lcMCsItmDs) = 0 
    IF !EMPTY(laOprCode[1,1])
      DIMENSION laOprCode[ALEN(laOprCode,1)+1 , 2]
    ENDIF
    laOprCode[ALEN(laOprCode,1) , 1] = lcMCsItmDs
    laOprCode[ALEN(laOprCode,1) , 2] = lcMfgCode
  ENDIF
ENDIF

*-- Call function to go to the add mode again to enter new cost item.
=lfIntNewCS()

*!*************************************************************
*! Name      : lfvClsNew
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Valid function for get field Close.
*!*************************************************************
*! Calls     : lfIntNewCS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvClsNew()
*!*************************************************************
*
FUNCTION lfvClsNew

*-- If cancel the current session.
IF llCancel
  *** Are you sure? You will lose your changes! ***
  *** <  Yes  > - <  No  > ***
  IF gfModalGen("QRM38028B00006" , "DIALOG") = 2
    RETURN
  ENDIF
  *-- Call local function to initialize the needed var.
  =lfIntNewCS()
ELSE
  *-- Quit the program.
  CLEAR READ
ENDIF
RETURN

*!*************************************************************
*! Name      : lfIntNewCS
*! Developer : Reham Al-Allamy
*! Date      : 08/13/1997
*! Purpose   : Reset Variables for New entry screen.
*!*************************************************************
*! Calls     : lfShwCSh_C
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfIntNewCS()
*!*************************************************************
*
FUNCTION lfIntNewCS

*-- Initialize the needed variables & arrays in the new cost item screen.
DECLARE laSegSorc1[1] , laSegTrgt1[1] , laSegSorc2[1] , laSegTrgt2[1] , ;
        laSegSorc3[1] , laSegTrgt3[1] , laSegSorc4[1] , laSegTrgt4[1] , ;
        laSegSorc5[1] , laSegTrgt5[1]
STORE "" TO laSegSorc1 , laSegTrgt1 , laSegSorc2 , laSegTrgt2 , ;
            laSegSorc3 , laSegTrgt3 , laSegSorc4 , laSegTrgt4 , ;
            laSegSorc5 , laSegTrgt5

STORE ''  TO lcNStyItem , lcNFabItem , lcMfgCode , lcMCsItmDs , ;
             lcFCsItmDs , lcSCsItmDs , lcNUom6 , lcNUom7 , lcNMfgOpr
STORE 0   TO lnNEsUnt6A , lnNEsUnt6B , lnNUntQt6A , lnNUntQt6B, ;
             lnNWstg6 , lnNEstUnt7 , lnNUntQty7 , lnNWstg7 , ;
             lnNUntCst6 , lnNUntCst7 , lnNCstPrc6 , lnNCstPrc7 , ;
             lnTotFCst6 , lnTotFCst7 , lnTotEqu6 , lnTotEqu7
STORE 1   TO lnItmClrs , lnCostType , lnExRate6 , lnExRate7 , ;
             ibCostType , ibClrCls9 , ibClrCls10 , ibItmClr9 , ;
             ibItmClr10 , puSelVal , puStyNSeg , puNMfgOpr
STORE .F. TO llDifColor , llSame
*E301411,1 SSH 31/05/20 New session variable.
llNewSess = .T.
*E301411,1 SSH 31/05/20 [END]
*B801813,1 Reham On 11/15/98  *** Begin ***
*B801813,1 Refresh the check box : "[ ] Set As Primary Fabric".
cbPrim      = .F.
*B801813,1 Reham On 11/15/98  *** End   ***

ibNDutyabl  = 1
lcNDutyable = laDutyable[ibNDutyabl,2]

STORE REPLICATE('*' , lnNonMjLen) TO lcItmClrs , lcCsItmC9
lcCsItmC10 = IIF(lcStyleTyp<>"T" AND !llColorExt , SPACE(6) , "******")
STORE lcBsCrSmb6 TO lcCrSmbA6 , lcCrSmbA7 , lcCrSmbB6 , lcCrSmbB7

lcCostType = laCostType[1,1]
lnCostType = laCostType[1,2]
lcCstType  = 'F'
STORE 1 TO ibClrCls9 , ibClrCls10
lnClrClass = laClrClass[1,2]

FOR lnCount = 1 TO ALEN(laStyNSeg,1)
  laStyNSeg[lnCount,8] = "A"
ENDFOR
FOR lnCount = 1 TO ALEN(laStyCSeg,1)
  laStyCSeg[lnCount,8] = "A"
ENDFOR

DECLARE laAStySeg [ALEN(laStyNSeg,1) , ALEN(laStyNSeg,2)]
=ACOPY(laStyNSeg , laAStySeg)

*-- Blank the elements related to the method "Different Colors" in 
*-- the colors array.
FOR lnCount = 1 TO ALEN(laColors9,1)
  laColors9[lnCount,3] = SPACE(6)
  laColors9[lnCount,4] = 0
ENDFOR
FOR lnCount = 1 TO ALEN(laColors10,1)
  laColors10[lnCount,3] = SPACE(6)
  laColors10[lnCount,4] = 0
ENDFOR

lcIClrText = IIF(lcCstType = 'S' , lcNonMjHdr , 'Item Color')
lcStColPop = IIF(lcStyleTyp <> "T" , IIF(llNonMjExt , lcNonMjHdr , 'Style') , "COLOR")
llCancel   = .F.

IF lcStyleTyp = "T"
  laClrClass[1,1] = 'Item Color Same As '+lcItemFile+' Color'
  laClrClass[2,1] = 'Item Color Always Constant'
  laClrClass[3,1] = lcItemFile +' Color Uses Different Item Colors'
ELSE
  laClrClass[1,1] = 'Item Color Same As Style ' + lcNonMjHdr
  laClrClass[2,1] = 'Item Color Always Constant'
  laClrClass[3,1] = 'Style ' + lcNonMjHdr + ' Uses Different Item Colors'
ENDIF

*-- Delete all the masks to get a clear file for new data.
SELECT (lcTmpMask)
ZAP

*-- Call show function for new cost item.
=lfShwCSh_C()

SHOW GET ibBegin ENABLE
_CUROBJ = OBJNUM(ibBegin)

*!*************************************************************
*! Name      : lfCreatMsk
*! Developer : Reham Al-Allamy
*! Date      : 09/01/1997
*! Purpose   : Function to create mask.
*!*************************************************************
*! Calls     : lfCreatMsk
*!*************************************************************
*! Parameters: lcMask      -> Mask to be completed
*!             lnSegNo     -> The current segment no.
*!             llColorDone -> If the color is added in the mask.
*!             lcIclr      -> The current color.
*!             lnCost      -> The current cost.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCreatMsk()
*!*************************************************************
*
FUNCTION lfCreatMsk
PARAMETERS lcMask , lnSegNo , llColorDone , lcIclr , lnCost
PRIVATE lcMask , lcSegType , lnCount , lcMaskTosend , lnSegNo, ;
        lnSegToSend , llColorDone , lcSegNo , lcIclr , lnCost

*-- If the segment no was not send as a parameter, set it as 1.
IF TYPE('lnSegNo') <> 'N'
  lnSegNo = 1
ENDIF

IF TYPE('lcIClr') <> 'C'
  lcIClr = IIF(lcStyleTyp <> "T" .AND. lcCstType $ 'FT' .AND. !llColorExt , lcCsItmC10 , "")
ENDIF

IF TYPE('lnCost') <> 'N'
  lnCost = 0
ENDIF

*-- Define the segment type.
lcSegType = IIF(llColorDone,laStyCSeg[lnSegNo+1,2],laStyCSeg[lnSegNo,2])

DO CASE
  CASE lcSegType='C' .AND. lcCstType $ 'FT' .AND. !llColorDone
    DO CASE
      *-- If the color method is : "Same as color".
      CASE lnClrClass = 1
        lcMaskTosend  = lcMask + REPL('*',LEN(laStyCSeg[lnSegNo,3])) + ;
                        ALLTRIM(laStyCSeg[lnSegNo,6])
        lnSegTosend   = lnSegNo
        llColorDone   = .T.
        IF (lcCstType $ 'FT' .AND. lnSegTosend = ALEN(laStyCSeg,1)) .OR. ;
           (!(lcCstType $ 'FT') .AND. lnSegTosend = ALEN(laStyNSeg,1))
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
               VALUES (lcMaskTosend , lcNFabItem , "******" , lnNUntCst6)
        ELSE
          =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,"******",lnNUntCst6)
        ENDIF
      *-- If the color method is : "Constant color".
      CASE lnClrClass = 2
        lcMaskTosend  = lcMask + REPL('*',LEN(laStyCSeg[lnSegNo,3])) + ;
                        ALLTRIM(laStyCSeg[lnSegNo,6])
        lnSegTosend   = lnSegNo
        llColorDone   = .T.
        IF (lcCstType $ 'FT' .AND. lnSegTosend = ALEN(laStyCSeg,1)) .OR. ;
           (!(lcCstType $ 'FT') .AND. lnSegTosend = ALEN(laStyNSeg,1))
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
               VALUES (lcMaskTosend , lcNFabItem , lcCsItmC10 , lnNUntCst6)
        ELSE
          =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,lcCsItmC10 , lnNUntCst6)
        ENDIF
      *-- If the color method is : "Different color".
      CASE lnClrClass = 3
        FOR lnCount = 1 TO ALEN(laColors10,1)
          IF !EMPTY(laColors10[lnCount,3])
            lcMaskTosend = lcMask + laColors10[lnCount,1] + ALLTRIM(laStyCSeg[lnSegNo,6])
            lnSegTosend  = lnSegNo
            llColorDone  = .T.
            IF (lcCstType $ 'FT' .AND. lnSegTosend = ALEN(laStyCSeg,1)) .OR. ;
               (!(lcCstType $ 'FT') .AND. lnSegTosend = ALEN(laStyNSeg,1))
              INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                   VALUES (lcMaskTosend , lcNFabItem , PADR(laColors10[lnCount,3],lnNonMjLen) , laColors10[lnCount,4])
            ELSE
              =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,PADR(laColors10[lnCount,3],lnNonMjLen) , laColors10[lnCount,4])
            ENDIF
          ENDIF
        ENDFOR
    ENDCASE
  OTHERWISE
    lcSegNo = STR(lnSegNo,1)
    *-- If the cost type was fabric or trim.
    IF lcCstType $ 'FT'
      *-- If select all majors.
      IF  laStyNSeg[lnSegNo,8] = "A"
        lcMaskTosend = lcMask + REPL('*',LEN(laStyNSeg[lnSegNo,3])) + ;
                       ALLTRIM(laStyNSeg[lnSegNo,6])
        lnSegTosend  = lnSegNo + 1
        *B801795,1 Reham On 11/17/98  *** Begin ***
        *B801795,1 If the color exist in the style code structure & it 
        *B801795,1 was not added in the mask, do not save the current mask.
        *IF lnSegNo = ALEN(laStyNSeg,1) 
        IF lnSegNo = ALEN(laStyNSeg,1) AND (llColorDone OR !llColorExt)
        *B801795,1 Reham On 11/17/98  *** End   ***
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
               VALUES (lcMaskTosend , IIF(lcCstType $ 'FT' , lcNFabItem , "") , lcIClr , lnCost)
        ELSE
          =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,lcIClr,lnCost)
        ENDIF
      ELSE
        *-- If select specific majors.
        FOR lnCount = 1 TO ALEN(laSegTrgt&lcSegNo.,1)
          lcMaskTosend = lcMask + PADR(laSegTrgt&lcSegNo.[lnCount],LEN(laStyNSeg[lnSegNo,3])) + ;
                         ALLTRIM(laStyNSeg[lnSegNo,6])
          lnSegTosend  = lnSegNo + 1
          *B801795,1 Reham On 11/17/98  *** Begin ***
          *B801795,1 If the color exist in the style code structure & it 
          *B801795,1 was not added in the mask, do not save the current mask.
          *IF lnSegNo = ALEN(laStyNSeg,1) 
          IF lnSegNo = ALEN(laStyNSeg,1) AND (llColorDone OR !llColorExt)
          *B801795,1 Reham On 11/17/98  *** End   ***
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                 VALUES (lcMaskTosend , IIF(lcCstType $ 'FT' , lcNFabItem , "") , lcIClr , lnCost)
          ELSE
            =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,lcIClr,lnCost)
          ENDIF
        ENDFOR
      ENDIF
    ELSE
      *-- If select all majors.
      IF laStyCSeg[lnSegNo,8] = "A"
        lcMaskTosend = lcMask + REPL('*',LEN(laStyCSeg[lnSegNo,3])) + ;
                       ALLTRIM(laStyCSeg[lnSegNo,6])
        lnSegTosend  = lnSegNo + 1
        IF lnSegNo = ALEN(laStyCSeg,1)
          INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
               VALUES (lcMaskTosend , lcNFabItem , lcIClr , lnCost)
        ELSE
          =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,lcIClr,lnCost)
        ENDIF
      ELSE
        *-- If select specific majors.
        FOR lnCount = 1 TO ALEN(laSegTrgt&lcSegNo.,1)
          lcMaskTosend = lcMask + PADR(laSegTrgt&lcSegNo.[lnCount],LEN(laStyCSeg[lnSegNo,3])) + ;
                         ALLTRIM(laStyCSeg[lnSegNo,6])
          lnSegTosend  = lnSegNo + 1 
          IF lnSegNo = ALEN(laStyCSeg,1)
            INSERT INTO (lcTmpMask) (cItmMask , Item , IClr , nCost) ;
                 VALUES (lcMaskTosend , lcNFabItem , lcIClr , lnCost)
          ELSE
            =lfCreatMsk(lcMaskTosend,lnSegTosend,llColorDone,lcIClr,lnCost)
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfUpdPerc
*! Developer : Reham Al-Allamy
*! Date      : 11/16/1997
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
  
  *-- Change the cost of the current item to the equevelant currency.
  IF lcStyleTyp = "I" .AND. llMulCurr ;
    .AND. (lcPricCur = lcDutyCur) .AND. !(lcPricCur = gcBaseCurr)
    llFound    = SEEK(gcBaseCurr + lcPricCur , "SYCEXCH")
    lnPExRate  = IIF(llFound , SYCEXCH.nExRate , 1)
    llFound    = SEEK(lcPricCur , "SYCCURR")
    lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
    lcPRatSign = "*"
    lcPUntSign = "/"
    lcPRatSign = gfGetExSin(@lcPUntSign , lcPricCur)
    lcPRatSign = IIF(lcPRatSign = "/" , "*" , "/")
    lcPUntSign = IIF(lcPUntSign = "/" , "*" , "/")
  ENDIF
  
  *-- Prepare the scan expression.
  lcScanExp = IIF(lcCatgType = "M" , "&lcTmpBom..cCatGTyp = 'P'" , "&lcTmpBom..cCostStat = '1'")
  
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
    
    *-- Change the cost of the current item to the equevelant currency.
    IF lcStyleTyp = "I" .AND. llMulCurr .AND. (&lcTmpBom..cCatGTyp $ "FTS") ;
      .AND. (lcPricCur = lcDutyCur) .AND. !(lcPricCur = gcBaseCurr)
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
*! Developer : Reham Al-Allamy
*! Date      : 11/16/1997
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

*-- Change the cost of the current item to the equevelant currency.
IF lcStyleTyp = "I" .AND. llMulCurr .AND. ;
   (lcPricCur = lcDutyCur) .AND. !(lcDutyCur = gcBaseCurr)
  llFound    = SEEK(gcBaseCurr + lcPricCur , "SYCEXCH")
  lnPExRate  = IIF(llFound , SYCEXCH.nExRate , 1)
  llFound    = SEEK(lcPricCur , "SYCCURR")
  lnPCurUnit = IIF(llFound , SYCCURR.nCurrUnit , 1)
  lcPRatSign = "*"
  lcPUntSign = "/"
  lcPRatSign = gfGetExSin(@lcPUntSign , lcPricCur)
  lcPRatSign = IIF(lcPRatSign = "/" , "*" , "/")
  lcPUntSign = IIF(lcPUntSign = "/" , "*" , "/")
ENDIF

*-- Prepare the scan expression.
DO CASE
  CASE &lcTmpBom..cCatGTyp = "P" .AND. &lcTmpBom..cCostStat = "1"
    lcScanExp = "&lcTmpBom..cCatGTyp = 'M' .OR. &lcTmpBom..cCatGTyp = 'D'"
  *B803553,1 ABD- Remark the next line to Fix bug that miscalculate the 
  *B803553,1 ABD- duty charges on the style BOM. [Begin]
  *CASE &lcTmpBom..cCatGTyp = "P"
  *  lcScanExp = "&lcTmpBom..cCatGTyp = 'M'"
  *B803553,1 ABD- [End]
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
  IF lcStyleTyp = "I" .AND. llMulCurr .AND. (lcCurType $ "FTS") ;
     .AND. (lcPricCur = lcDutyCur) .AND. !(lcDutyCur = gcBaseCurr)
    lnUpdCost = lnUpdCost &lcPRatSign lnPExRate &lcPUntSign lnPCurUnit
  ENDIF
  lnUpdCost = UntCost + (lnSign * lnUpdCost)
  
  *-- Save the updated cost.
  
  *B604802,1 AMH Update the cstatus field to save this modification [Start]
  *REPLACE UntCost WITH lnUpdCost ;
          TOTCOST WITH UNTCOST * nBomTotQty
  REPLACE UntCost WITH lnUpdCost ;
          TOTCOST WITH UNTCOST * nBomTotQty;
          CSTATUS WITH IIF(CSTATUS = "S" , "M" , CSTATUS)
  *B604802,1 AMH [End]
  
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
*! Name      : lfTempCost
*! Developer : Reham Al-Allamy
*! Date      : 05/04/1998
*! Purpose   : Function to calculate the style component cost
*!           : if the style component same as style parent.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcMjrComp -> The style component major.
*!             lcNonMjr  -> The style component non-major.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfTempCost(lcMjrComp , lcNonMjr)
*!*************************************************************
*
FUNCTION lfTempCost
PARAMETERS lcMjrComp , lcNonMjr
PRIVATE lcMjrComp , lcNonMjr , lcCurAlias , lnStyRec , ;
        lnBomRec , lnCost1 , lnCost2 , lnCost3 , lnCost4 , ;
        lnCost5 , lnTotCost , lcScale , lnFbrRec

*-- Save the current alias.
lcCurAlias = ALIAS()

*-- Save the style record pointer.
lnStyRec = RECNO("STYLE")

*-- Change the style file index.
SET ORDER TO TAG STYLE  IN STYLE

*-- Save the fabric record pointer.
lnFbrRec = RECNO("FABRIC")

*-- Change the fabric file index.
SET ORDER TO TAG FABRIC IN FABRIC

*-- Save the temp. Bom record pointer.
lnBomRec = RECNO(lcTmpBom)

*-- Blank the current cost variable.
STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5 , lnTotCost

*-- Concatinate the style component structure.
lcStyle   = PADR(lcMjrComp,lnMajorLen) + ALLTRIM(lcSeprator) + PADR(lcNonMjr,lnNonMjLen)
lcStySeek = IIF("*" $ lcStyle , SUBSTR(lcStyle,1,AT("*",lcStyle)-1) , lcStyle)
lcScale = IIF(SEEK(lcStySeek , "STYLE") , STYLE.Scale , "")

SELECT STYLE
IF llExtSizSc .AND. SEEK(lcStySeek)
  lcStyle = STYLE.Style
  lcScale = STYLE.Scale
ENDIF

*-- Blank the current cost variables.
STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5

*-- Scan in the temp. bom file to calculate the cost for each cost item.
SELECT (lcTmpBom)
SCAN FOR (LIKE(STRTRAN(&lcTmpBom..cItmMask,'*','?') , lcStyle) .OR. ;
          LIKE(STRTRAN(lcStyle,'*','?') , &lcTmpBom..cItmMask)) .AND. ;
         (EMPTY(&lcTmpBom..mSizes) .OR. lcScale $ &lcTmpBom..mSizes)
  lnNoOfSize = 1   && Var. hold the no. of sizes selected.
  lnWholSclN = 1   && Var. hold no. of all sizes in current style scale.
  *-- If there is sizes selected for the current cost item.
  IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
    *-- Loop for the memo lines count.
    *-- Get the scale count from the scale file.
    lnWholSclN = IIF(SEEK("S"+lcScale , "SCALE") , SCALE.Cnt , 1)
    *-- Get the no. of lines in the current sizes memo field.
    lnLineNo   = ATCLINE(lcScale , ALLTRIM(&lcTmpBom..mSizes))
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
      IF IClr = "******"
        lcFabClr = SUBSTR(lcStyle , lnColorStr , lnColorLen)
      ELSE
        lcFabClr = PADR(IClr,6)
      ENDIF
      *-- Save the current record pointer in the fabric file.
      lnTmpRec = RECNO("Fabric")
      IF SEEK(SUBSTR(Item,1,7)+lcFabClr , 'Fabric')
        *-- Calculate the cost for the current cost item.
        &lcCurVar = &lcCurVar + (((&lcTmpBom..nBomTotQty*Fabric.CostBuy/Fabric.Conv)*lnNoOfSize)/lnWholSclN)
      ENDIF
      IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("FABRIC")
        GOTO lnTmpRec IN FABRIC
      ENDIF
    *-- If the cost item is trim & not trim inventory or 
    *-- Price, Duty or Mfg operation.
    CASE (cCatGTyp = 'T' .AND. !Trim_Invt) .OR. cCatGTyp $ 'MDP'
      *-- Calculate the cost for the current cost item.
      &lcCurVar = &lcCurVar + ((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN)
    *-- If the cost item is style component.
    CASE cCatGTyp = 'S'
      lcStySeek = &lcTmpBom..Item
      *-- Save the record pointer in the style file.
      lnTmpRec  = RECNO("STYLE")
      *-- If the style component is same as style parent.
      IF llSame .OR. PADR(&lcTmpBom..cItmMask,lnMajorLen) = PADR(&lcTmpBom..Item,lnMajorLen)
        *-- Get the cost from the temp. Bom file.
        &lcCurVar = &lcCurVar + ((&lcTmpBom..TotCost * lnNoOfSize)/lnWholSclN)
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
                             SUBSTR(lcStyle , lnSegPos , lnSegLen) , 1 , 1)
              ENDIF
            ENDFOR
            *-- Save the record pointer in the style file.
            lnTmpRec = RECNO("STYLE")
            IF SEEK(lcStySeek , 'Style'))
              *-- Calculate the cost for the current cost item.
              &lcCurVar = &lcCurVar + ((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN)
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
              IF lcScale = SUBSTR(lcCurCrRef,1,3)
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
                            SUBSTR(lcStyle , lnSegPos , lnSegLen) , 1 , 1)
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
                *-- Get the total cost of the style component.
                &lcCurVar = &lcCurVar + (&lcTmpBom..nBomTotQty * Style.TotCost * laTmpChldS[lnCnt,2])
              ENDIF
            ENDFOR
            *-- Divide the cost by the whole total size.
            &lcCurVar = &lcCurVar / lnTotSize
          ENDIF
        ELSE
          IF SEEK(lcStySeek , 'Style'))
            &lcCurVar = &lcCurVar + ((&lcTmpBom..nBomTotQty * Style.TotCost * lnNoOfSize)/lnWholSclN)
          ENDIF
        ENDIF
      ENDIF  
      *-- Restore the record pointer in the style file.
      IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("STYLE")
        GOTO lnTmpRec IN STYLE
      ENDIF
  ENDCASE
ENDSCAN

*-- Restore the style record pointer.
IF lnFbrRec > 0 .AND. lnFbrRec <= RECCOUNT("FABRIC")
  GOTO lnFbrRec IN STYLE
ENDIF
*-- Restore the old style index.
SET ORDER TO TAG CFABRIC IN FABRIC

*-- Restore the style record pointer.
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
*-- Restore the old style index.
SET ORDER TO TAG CSTYLE IN STYLE

*-- Restore the temp. BOM record pointer.
IF lnBomRec > 0 .AND. lnBomRec <= RECCOUNT(lcTmpBom)
  GOTO lnBomRec IN (lcTmpBom)
ENDIF

*-- Restore the previous alias.
SELECT (lcCurAlias)

*-- Calculate the total cost.
lnTotCost = lnCost1 + lnCost2 + lnCost3 + lnCost4 + lnCost5

*-- Return with the total cost of the current cost item.
RETURN lnTotCost

*!*************************************************************
*! Name      : lfGetClrCm
*! Developer : Reham Al-Allamy
*! Date      : 06/16/1998
*! Purpose   : Function to know if the selected Component is 
*!           : already selected before as a component.
*!*************************************************************
*! Calls     : lfGetClrCm()
*!*************************************************************
*! Parameters: lcComp_NMj -> The component non-major.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lcClrCmp = lfGetClrCm(lcMainCmp , lcComp_NMj)
*!*************************************************************
*
FUNCTION lfGetClrCm
PARAMETERS lcComp_NMj
PRIVATE lcCurAlis , lnCurRec , lcMainCmp , lcComp_NMj , ;
        lcCurClr , llFoundClr , lnCount , lcCount , lnCnt1 , ;
        lnStart

*B602121,1 Reham On 10/28/98   *** Begin ***
*B602121,1 Variable to determine the start row in the for loop in 
*B602121,1 the array that hold the different colors.
lnStart    = 1
*B602121,1 Reham On 10/28/98   *** End   ***

lcCurClr   = ''    && Var. hold the founded color.
llFoundClr = .T.   && Flag to know if something found or not.
lnCount    = 1     && Incremental variable hold the looping count.
GO TOP IN (lcTmpBom)

DO WHILE llFoundClr
  lcCount = ALLTRIM(STR(lnCount))
  PRIVATE lcCurClr&lcCount.
  lcCurClr&lcCount. = ""
  
  *-- Find if the item component for the current parent item 
  *-- has used before as a component of a component in the file.
  LOCATE REST FOR cCatgTyp = "S" .AND. ;
              PADR(Item , lnMajorLen) = PADR(lcMajor , lnMajorLen) .AND. ;
              SUBSTR(cItmMask , lnMajorLen + IIF(Empty(ALLTRIM(lcSeprator)) , 1 , 2) , lnNonMjLen) = PADR(lcComp_NMj , lnNonMjLen)
  
  IF FOUND()
    *-- If the componenet found in the file, Save it in a variable 
    *-- and set the found flag to true.
    lcCurClr&lcCount. = SUBSTR(Item , lnMajorLen + IIF(Empty(ALLTRIM(lcSeprator)) , 1 , 2) , lnNonMjLen)
    llFoundClr = .T.
  ELSE
    *-- If the component was not used before set the found flag to false.
    llFoundClr = .F.
    
    *B602121,1 Reham On 10/28/98   *** Begin ***
    *B602121,1 Use the start position value to start the for loop to avoid
    *B602121,1 getting stuck in inifinte loop.
    IF lnStart <= ALEN(laColors9,1)
      *-- Find if the item component for the current parent item 
      *-- has used before as a component of a component in the array.
      *FOR lnCnt1 = 1 TO ALEN(laColors9,1)
      FOR lnCnt1 = lnStart TO ALEN(laColors9,1)
      *B602121,1 Reham On 10/28/98   *** End ***
        IF PADR(laColors9[lnCnt1,1] , lnNonMjLen) = PADR(lcComp_NMj , lnNonMjLen) ;
           .AND. !EMPTY(PADR(laColors9[lnCnt1,3] , lnNonMjLen))
          
          *-- If the componenet found in the array, Save it in a variable 
          *-- and set the found flag to true.
          lcCurClr&lcCount. = PADR(laColors9[lnCnt1,3] , lnNonMjLen)
          llFoundClr = .T.
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  
  *-- If the found flag was true, call the function again was the founded color.
  IF llFoundClr
    *-- Save the bom file record pointer.
    lnCurRec = RECNO()
    *-- Call local function to get the components for the founded non-major.
    lcCurClr&lcCount. = lcCurClr&lcCount. + lfGetClrCm(lcCurClr&lcCount.)
    *-- Restore the bom record pointer.
    IF lnCurRec > 0 .AND. lnCurRec <= RECCOUNT()
      GOTO lnCurRec
      SKIP
    ENDIF
    *B602121,1 Reham On 10/28/98   *** Begin ***
    *B602121,1 Increment the for loop starting value.
    lnStart = lnStart + 1
    *B602121,1 Reham On 10/28/98   *** End   ***
  ENDIF
  lnCount = lnCount + 1
ENDDO

*-- Concatinate all the found colors.
FOR I = 1 to lnCount - 1
  *lcCurClr = lcCurClr + lcCurClr&lcCount.
  lcI = ALLTRIM(STR(I))
  lcCurClr = lcCurClr + IIF(lcCurClr&lcI. $ lcCurClr , "" , lcCurClr&lcI.)
ENDFOR 

RETURN lcCurClr

*!*************************************************************
*! Name      : lfPrntCost
*! Developer : Reham Al-Allamy
*! Date      : 06/16/1998
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
      GOTO lnCurRec
      SKIP
    ENDIF
  ELSE
    llFndComp = .F.   && Set flag that no parent was found for the parent sent.
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfvSizPrct
*! Developer : Reham Al-Allamy
*! Date      : 06/28/1998
*! Purpose   : Function to calculate the percentage of the 
*!           : selected sizes to all sizes...
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvSizPrct()
*!*************************************************************
*
FUNCTION lfvSizPrct
PRIVATE lcCurAlias , lnStyRec , lnUsdSizes , lnAllSizes , laCurSize

IF lcStyleTyp <> "T"
  lcCurAlias = ALIAS()        && Var. Hold the current alias.
  lnStyRec   = RECNO("STYLE") && Var. hold the record pointer of the style file.
  lnUsdSizes = 0              && Var. hold the used sizes for the current style.
  lnAllSizes = 0              && Var. hold the all sizes for the current style.
  DECLARE laCurSize[1]        && Array hold the scale sizes' of the current style.
  laCurSize  = ""
  
  *-- If style cost sheet based on sizes, Collect all sizes & used sizes.
  IF !EMPTY(ALLTRIM(&lcTmpBom..mSizes))
    *-- If there is selected sizes for the current cost item.
    FOR lnCount = 1 TO MEMLINES(ALLTRIM(&lcTmpBom..mSizes))
      *-- Fill the current cost item scale sizes array.
      IF !EMPTY(laCurSize[1])
        DIMENSION laCurSize[ALEN(laCurSize,1)+1]
      ENDIF
      laCurSize[ALEN(laCurSize,1)] = MLINE(ALLTRIM(&lcTmpBom..mSizes) , lnCount)
      lnUsdSizes = lnUsdSizes + OCCURS("," , laCurSize[ALEN(laCurSize,1)]) +1
    ENDFOR
    
    *-- Collect all the sizes for the current mask.
    STORE "" TO lcMSize
    SELECT STYLE
    *-- Change the style file index.
    SET ORDER TO TAG STYLE
    *-- Clear the style file filter.
    SET FILTER TO
    *-- Seek for the current mask.
    =SEEK(PADR(&lcTmpBom..cItmMask,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(&lcTmpBom..cItmMask,lnMajorLen) ;
              FOR LIKE(STRTRAN(&lcTmpBom..cItmMask , "*" , "?") , STYLE.Style)
      *-- Seek for the founded scale to get its sizes.
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
          lnAllSizes = lnAllSizes + OCCURS("," , laCurSize[ALEN(laCurSize,1)]) +1
        ENDIF
      ENDIF
    ENDSCAN
    *-- Restore the style file index.
    SET ORDER TO TAG CSTYLE IN STYLE
    *-- Restore the style file filter.
    DO CASE
      CASE lcStyleTyp = 'I'
        SET FILTER TO !Style.Make
      CASE lcStyleTyp = 'M'
        SET FILTER TO Style.Make
    ENDCASE
    *-- Restore the record pointer in the style file.
    IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
      GOTO lnStyRec IN STYLE
    ENDIF
  ELSE
    lnUsdSizes = 1   && Var. hold the used sizes for the current style.
    lnAllSizes = 1   && Var. hold the all sizes for the current style.
  ENDIF
  
  *-- Restore the previous alias.
  SELECT (lcCurAlias)
ELSE
  lnUsdSizes = 1   && Var. hold the used sizes for the current style.
  lnAllSizes = 1   && Var. hold the all sizes for the current style.
ENDIF

*-- Return with the selected sizes divided by all sizes.
RETURN (lnUsdSizes/lnAllSizes)


*!*************************************************************
*! Name      : lfvDetOpr
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Detail Operation Screens.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvDetOpr()
*!*************************************************************
*! E301411,1 SSH 31/05/20 [Begin] 
*!*************************************************************
*!
FUNCTION lfvDetOpr
PARAMETER llHide
PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF llPwInst  .AND. lcStyleTyp = 'M'
  lcMfgCode  = laMfgCode[lnNMfgCode,2]
  *--- Strt Create Temp Files if not created before.
  *--- This Function Exist in the mfcstsh.prg
  DO lfCreatTemp IN gcAppHome+"mfcstsh.prg"
  *=lfCreatTemp()
  *--- Start Get Data From the following
  *--- 1) PWBOM
  *--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
  *IF SEEK(PADR(laData[1],19)+lcMfgCode,lcPWBOM)
  *  SELECT (lcPWBOM)
  *  LOCATE REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
  *                    PADR(laData[1],19)+lcMfgCode;
  *              FOR   STR(nLineNo,6)=STR(&lcTmpBom..nLineNo,6)

  * IF !FOUND()
  *    DO lfGetPWBOM IN gcAppHome+"mfcstsh.prg"
  *    *=lfGetPWBOM()
  *  ENDIF
  *ELSE
    *=lfGetPWBOM()
  DO lfGetPWBOM IN gcAppHome+"mfcstsh.prg"
  *ENDIF
  *--- 3) PWOPERAT
  DO lfGtDetOpr IN gcAppHome+"mfcstsh.prg" WITH lcMfgCode,!llNewSess
  DO (gcAppHome+"MFDetOpr") WITH lcMfgCode , lcDetLin , 'lnNUntCst7',.F. , llHide
  SELECT(lnOldAls)
  *--- This function is one of New Cost Item screen functions that valid 
  *--- the unit cost .
  *--- so after runnig detail operation program which will calculate
  *--- MfgCode unit Cost we will call this function to valid the calculate
  *--- unit cost the same standard calculation. i.e. we did not change
  *--- any thing in this function.
  =lfvNUntCst('7')
  SHOW GET lnNUntCst7 DISABLE
ENDIF

*!*************************************************************
*! Name      : lfCheckPW
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to check if Pw module installed and if we.
*!           : going to enable/disable detail operation buttons.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfCheckPW()
*!*************************************************************
*! E301411,1 SSH 31/05/2000.
*!*************************************************************
*!*E301411,1 SSH
FUNCTION lfCheckPW

PRIVATE lcMCsItmDs , lnOldAls
lnOldAls = SELECT(0)
llNewSess = .T.
IF USED(lcPwBom)
  SELECT (lcPwBom)
  REPLACE ALL cStatus WITH 'D' FOR cStatus = 'T'
ENDIF
=gfOpenFile(gcDataDir+"pwoperat" , gcDataDir+"pwoperat" , "SH")
llFondDet = SEEK(lcMfgCode,'pwoperat')
*-- See if there is Mfg Operation have flag considered as operation yes .or. no.
DECLARE laMfgRltFd[1,2]
laMfgRltFd[1,1] = 'LMFGOPR'
laMfgRltFd[1,2] = 'llBMfgOpr'
=gfRltFld(lcMfgCode , @laMfgRltFd , "MFGCODE")
IF llBMfgOpr .AND. lcCstType='M' .AND. llFondDet   .AND. lcStyleTyp = 'M'
  SHOW GET pbDetOpr   ENABLE
  =lfvDetOpr(.T.)
  SHOW GET lnNEstUnt7 DISABLE
  SHOW GET lnNUntCst7 DISABLE
  SHOW GET lnNWstg7   DISABLE
ELSE
  SHOW GET pbDetOpr   DISABLE
  SHOW GET lnNEstUnt7 ENABLE
  SHOW GET lnNUntCst7 ENABLE
  SHOW GET lnNWstg7   ENABLE
ENDIF
SELECT(lnOldAls)

FUNCTION lfAddNMDet
PRIVATE lnOldAls

lnOldAls = SELECT(0)
IF USED(lcPwBom)
  SELECT (lcPwBom)
  REPLACE ALL cStatus WITH ' ' FOR cStatus = 'T'
  IF RECCOUNT(lcTmpMask)>1
    lcNMajTemp = gfTempName()
    SELECT * FROM (lcPwBom) WHERE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                                  PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+laMfgCode[lnNMfgCode,2];
                           .AND. nLineNo = lnToCnt[1,1];
                           INTO DBF (lcNMajTemp)
    IF _TALLY > 0
      FOR lnRecNo = 1 TO (RECCOUNT(lcTmpMask)-1)
        *--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
        SELECT(lcNMajTemp)
        REPLACE ALL nLineNo WITH nLineNo+1
        SELECT (lcPwBom)
        APPEND FROM (lcNMajTemp)
      ENDFOR
    ENDIF
  ENDIF
ENDIF
ERASE &gcWorkDir.&lcNMajTemp..DBF
ERASE &gcWorkDir.&lcNMajTemp..CDX
SELECT(lnOldAls)