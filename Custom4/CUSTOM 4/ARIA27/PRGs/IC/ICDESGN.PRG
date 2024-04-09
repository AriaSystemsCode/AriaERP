*:************************************************************************
*:
*: Procedure file: icdesign.PRG 
*:                 (Design Size)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: Inventory Contol
*:         Author: WAB - Walid A. Wahab
*:		   Date  : 05/13/1999
*:      Copyright (c) 
*C200074,1 
*:************************************************************************
*: Last modified :
*E301268,1 WAB 06/22/1999 Add Parameter (.F.,'C') When Calling gfApVnBrow()
*E301311,1 WAB 08/25/1999 add  "Object link" button in the control pannel.
*B603745,1 KAM 11/05/2000 to handle case of record out of range
*:************************************************************************
EXTERNAL ARRAY laData,laKeyField,laDefProc
*--Define key FIELD array, ,Table Field Array 
*Ren laCountry ? 
DECLARE laKeyField [1,4],laCountry[1],laData[12]
*--Define Code information Aray for global Function gfwCodePop,
*--       Size Popup array, Season Popup array, Royality Popup array, 
DECLARE laCodeInfo[3,10],laDesign[1],laSeason[1],laRoyalty[1]


*--For Design Size
laCodeInfo[1,1]  = 'cDsgnSize'
laCodeInfo[1,2]  = 'laDesign'
laCodeInfo[1,3]  = 'puDesign'
laCodeInfo[1,4]  = ''
laCodeInfo[1,5]  = .F.
laCodeInfo[1,6]  = .T.
laCodeInfo[1,7]  = ''
laCodeInfo[1,8]  = ''
laCodeInfo[1,9]  = ''
laCodeInfo[1,10] = ''

*--For Season
laCodeInfo[2,1]  = 'Season'
laCodeInfo[2,2]  = 'laSeason'
laCodeInfo[2,3]  = 'puSeason'
laCodeInfo[2,4]  = ''
laCodeInfo[2,5]  = .F.
laCodeInfo[2,6]  = .T.
laCodeInfo[2,7]  = ''
laCodeInfo[2,8]  = ''
laCodeInfo[2,9]  = ''
laCodeInfo[2,10] = ''

*--For Royalty
laCodeInfo[3,1]  = 'Royalty'
laCodeInfo[3,2]  = 'laRoyalty'
laCodeInfo[3,3]  = 'puRoyalty'
laCodeInfo[3,4]  = ''
laCodeInfo[3,5]  = .F.
laCodeInfo[3,6]  = .T.
laCodeInfo[3,7]  = ''
laCodeInfo[3,8]  = ''
laCodeInfo[3,9]  = ''
laCodeInfo[3,10] = ''

*--The Tag Expression of the default Selected Database File
*Ren changed lakeyfield[1,3] to 'icdesign' instead of 'design'
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'icdesign'
laKeyField[1,4] = 1

*--Initilize screen and program variables .
lcVendNam  = SPACE(30)  && Display Field (Vendor Name)
llOpnVend = .F.   		&& flag for opennig the vendor table
lnArryPos = 0			&& Variable for array position (popup field)
llBrowse   = .F.		&& Browse Window Is Not Active
*Ren what is 
llButPress = .T.		&& bROWSE Boottom Is Pressed

*-- open Tables, define variables like:
*-- lcWindTitle : window title
*-- lcBaseFile  : base file of the screen
*-- lcFileTtl   : File title of the base file	&&used by the global browse
*-- lcBrFields  : browse fields string of the base file &&used by the global browse
*-- lcScFields  : ''
IF !gfSetup()    
  RETURN
ENDIF  

*Ren : there's no need to close the vendor file.
*-- Open Table Vendor 
IF FILE(gcDataDir+'APVENDOR.dbf')
  =gfOpenFile(gcDataDir+'APVENDOR','VenCode','SH')
  llOpnVend = .T.  				&& table apvendor is found
ENDIF

*--Fields NAME
*--Change fileds name ( RENEE)
*lcScFields ='cDsgnCode,cDsgnName,cDsgnClr,cDsgnSize,season,cVendCode,;
             nVenAdMin,cDsgnLink,royalty,nDsgnAPrc ,nDsgnACst,nRoyltAmt'
lcScFields ='cDsgnCode,cDsgnName,cDsgnClr,cDsgnSize,season,cVendCode,;
             nVenAdMin,cDsgnLink,royalty,nDsgnPrc ,nDsgnCst,nRoyltAmt'


*E301311,1 WAB  - To add the "Object link" button in the control pannel.
*E301311,1 WAB - START
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
*--Object link button.
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvDsgnOLnk() MESSAGE 'Object link' DISABLE]
*E301311,1 WAB - END


*-- Global Function For Reintialised User Define Fields
*Ren : there's no need for this function anymore
*gfReInUsrF(lcScFields,@laUsrFields)

SELECT (lcBaseFile)
*-- if it is time to activate the window scatter an empty record
IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

*-- call the gf. win code poppup with N/A Display
=gfwCodePop(@laCodeInfo,'cDsgnSize','N')
=gfwCodePop(@laCodeInfo,'Royalty','N')
=gfwCodePop(@laCodeInfo,'Season','N')

 
*-- call the screen file ( icdesign.SPR  )
DO (gcScrDir + gcWinAppl + '\icdesgn.SPR')

*!**************************************************************************
*! Name      : lpShow
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/13/1999
*! Purpose   : Show objects function.
*!**************************************************************************
*! Calls     : gfwCodePop()
*!			 : lfGetArry()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lpShow()
*!**************************************************************************
*-
PROCEDURE lpShow
DO CASE
  CASE laScrMode[1]
    lcVendNam = ''
    =gfwCodePop(@laCodeInfo,'cDsgnSize','N')
    =gfwCodePop(@laCodeInfo,'Royalty','N')
    =gfwCodePop(@laCodeInfo,'Season','N')
    lcVendNam  = SPACE(30)  && reintial. then vendor Name
    SHOW GET laDesign   DISABLE
    SHOW GET laRoyalty  DISABLE
    SHOW GET laseason   DISABLE
    SHOW GET ibBrowKey2 DISABLE    
    SHOW GET ibBrowKey3 DISABLE    

    *E301311,1 WAB  - DISABLE button "Object link"  (Select mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk DISABLE
    *E301311,1 WAB - END
    
  CASE laScrMode[2]
    =lfGetArry()
    IF llOpnVend THEN 
      lcVendNam = LOOKUP(APVENDOR.cVenComp, laData[6], APVENDOR.cVendCode, 'VENCODE')
    ENDIF 
    SHOW GET ibBrowKey2 DISABLE    
    SHOW GET ibBrowKey3 DISABLE    
    
    *E301311,1 WAB  - Enable button "Object link"  (view mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk ENABLE
    *E301311,1 WAB - END
   
  CASE laScrMode[3]
    IF llOpnVend THEN 
      SHOW GET ibBrowKey3 ENABLE
      SHOW GET laData[6]  ENABLE       
    ELSE
      SHOW GET ibBrowKey3 DISABLE
      SHOW GET laData[6]  DISABLE
    ENDIF
    SHOW GET ibBrowKey2 ENABLE

    *E301311,1 WAB  - Enable button "Object link"  (edit mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk ENABLE
    *E301311,1 WAB - END
    
  CASE laScrMode[4]
    IF llOpnVend THEN 
      SHOW GET ibBrowKey3 ENABLE
      SHOW GET laData[6]  ENABLE       
    ELSE
      SHOW GET ibBrowKey3 DISABLE    
      SHOW GET laData[6]  DISABLE
    ENDIF
    SHOW GET ibBrowKey2 ENABLE

    *E301311,1 WAB  - DISABLE button "Object link"  (ADD mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk DISABLE
    *E301311,1 WAB - END
    
ENDCASE 
=lfRefresh()

*!**************************************************************************
*! Func. Name: lfvData_1
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/13/1999
*! Purpose   : valid the design code and browse from table icdesign 
*!**************************************************************************
*! Calls     : gfSeekRec() 
*!			 : lfRefresh()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvData_1()
*!**************************************************************************
*-
FUNCTION lfvData_1
IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13) 
  =gfSeekRec() 
  =lfRefresh()
  llBrowse = .F.
ENDIF
*-
*!**************************************************************************
*! Func. Name: lfvData_6
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/14/1999
*! Purpose   : valid the Vendor Code and browse from table Apvendor
*!**************************************************************************
*! Calls     : gfAPVnBrow() 
*!			 : lfRefresh()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvData_6()
*!**************************************************************************
*-
FUNCTION lfvData_6
PRIVATE lcOldArea, lcVendCode, lnNearRec
lnOldArea = SELECT(0)
lcVendCode = laData[6]
IF !llBrowse
  SELECT APVENDOR
  IF !EMPTY(laData[6]) and SEEK(laData[6])
    lcVendNam = cVenComp
    ladata[7] = nVenAdMin
  ELSE
    lnNearRec = RECNO(0)
    IF BETWEEN(lnNearRec, 1, RECCOUNT())
      GO lnNearRec
    ENDIF  
    llBrowse = .T.
  ENDIF
ENDIF
IF llBrowse  
  *-E301268 WAB  Add Parameter When Calling gfApVnBrow() 
  *-E301268 WAB  Begin
  *  =gfAPVnBrow(@lcVendCode)
  =gfAPVnBrow(@lcVendCode,.F.,'C')
  *-E301268  WAB End
  laData[6] = cvendCode
  lcVendNam = cVenComp
ENDIF
llBrowse = .F.
SELECT (lnOldArea)
=lfRefresh()
RETURN

*-
*!**************************************************************************
*! Func. Name: lfvbrwsD_6()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/30/1999
*! Purpose   : calling the validation of the vendor code
*!**************************************************************************
*! Calls     : lfvData_6() 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvbrwsD_6()
*!**************************************************************************
*-
FUNCTION  lfvbrwsD_6
llBrowse = .T.
  =lfvData_6()
  _CUROBJ = OBJNUM(laData[6])
return .t.
*-
*!**************************************************************************
*! Func. Name: lfvalDesign()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : assign value from popup to laData[4] &design size
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvalDesign()
*!**************************************************************************
FUNCTION lfvalDesign
 laData[4] = laDesign[pudesign,2]
RETURN .T.

*!**************************************************************************
*! Func. Name: lfvalSeason()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : assign value from popup to laData[5] &Season
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvalSeason()
*!**************************************************************************
FUNCTION lfvalSeason
  laData[5] = laSeason[puSeason,2]
RETURN .T.

*-
*!**************************************************************************
*! Func. Name: lfvalRoyalty()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : assign value from popup to laData[9 &Royalty
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvalRoyalty()
*!**************************************************************************
FUNCTION lfvalRoyalty
  laData[9] = laRoyalty[puRoyalty,2]
RETURN .T.
*-

*!**************************************************************************
*! Func. Name: lfwnDesign()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : before focus to fill the popup design Code
*!**************************************************************************
*! Calls     : gfwCodePop()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfwnDesign()
*!**************************************************************************
FUNCTION lfwnDesign
  =gfwCodePop(@laCodeInfo,'cDsgnSize','L')
  laData[4] = laDesign[pudesign,2]
RETURN .T.
*-

*!**************************************************************************
*! Func. Name: lfwnSeason()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : before focus to fill the popup ( Season )
*!**************************************************************************
*! Calls     : gfwCodePop()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfwnSeason()
*!**************************************************************************
FUNCTION lfwnSeason
  =gfwCodePop(@laCodeInfo,'Season','L')
  laData[5] = laSeason[puSeason,2]
RETURN .T.
*-

*!**************************************************************************
*! Func. Name: lfwnRoyalty()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : before focus to fill the popup ( Royalty )
*!**************************************************************************
*! Calls     : gfwCodePop()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfwnRoyalty()
*!**************************************************************************
FUNCTION lfwnRoyalty
  =gfwCodePop(@laCodeInfo,'Royalty','L')
  laData[9] = laRoyalty[puRoyalty,2]
RETURN .T.

*!**************************************************************************
*! Func. Name: lfGetArry()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/25/1999
*! Purpose   : get Value From Table Fields to the Popup
*!**************************************************************************
*! Calls     : gfwCodePop()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfGetArry()
*!**************************************************************************
FUNCTION lfGetArry
PRIVATE lnOldArea,lnArryPos
lnOldArea=select(0)
  =gfwCodePop(@laCodeInfo,'cDsgnSize','L')
  =gfwCodePop(@laCodeInfo,'Royalty','L')
  =gfwCodePop(@laCodeInfo,'Season','L')
  SELECT icdesign 
  set exact on
  lnArryPos  = ASCAN(laSeason, ICDESIGN.Season)	
  IF  lnArryPos = 0 then
    puSeason = 1
  ELSE
    puSeason = lnArryPos / 2
  ENDIF
  lnArryPos  = ASCAN(laDesign, ICDESIGN.cDsgnSize)	
  IF  lnArryPos = 0 then
    pudesign = 1
  ELSE
    puDesign = lnArryPos /2
  ENDIF
  lnArryPos  = ASCAN(laRoyalty, ICDESIGN.Royalty)	
  IF  lnArryPos = 0 then
    puRoyalty = 1
  ELSE
    puRoyalty  = lnArryPos /2
  ENDIF
  SHOW GET puDesign  DISABLE
  SHOW GET puRoyalty DISABLE
  SHOW GET puseason  DISABLE
  set exact off
  select (lnOldArea)
return .t.

*!**************************************************************************
*! Func. Name: lfDsgnLnk()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/30/1999
*! Purpose   : to browse and select  lnk codes 
*!**************************************************************************
*! Calls     : gfBrows(()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfDsgnLnk()
*!**************************************************************************
FUNCTION lfDsgnLnk
PRIVATE lcbrfields,lcOldValue,lnMessAns,lcFields,lnRecNo
lcOldArea = SELECT()
lnRecNo   = RECNO()
lnMessAns = 0
DECLARE laTmpDat[1]
laTmpDat[1] = ' '
IF (!llBrowse .AND. EMPTY(laData[8])) .OR. MDOWN()
  RETURN .T.
ENDIF
SELECT ICDESIGN
*CHANGE TAG Name ( RENEE )
SET ORDER TO TAG DSGNLINK    
IF !llBrowse .AND. laData[8] <> '?'
  seek laData[8] 
  IF !FOUND() THEN
    lnMessAns = gfModalGen("TRM00001B00001","DIALOG","LINK CODE ")
    IF lnMessAns = 1 THEN
      llBrowse = .T.
    ENDIF
  ENDIF
ENDIF
IF llBrowse .OR. laData[8] = '?'
 lcFields    = 'cDsgnLink'
 lcbrfields  = "cDsgnLink :H='Design Link',"+;
               "cDsgnCode :H='Design Code',"+;
               "cDsgnName :H='Color'"
  =gfBrows('',lcfields,"laTmpDat",'LINK CODES','','',.F.)             
  laData[8]=laTmpDat[1]
ENDIF
llBrowse   = .F.
SELECT(lcOldArea)
set order to TAG icDesign
*B603745,1 KAM 11/05/2000 add condition to sure the lnrecno between 
*B603745,1 one and last record[start]
IF BETWEEN(lnRecNo,1,RECCOUNT(lcOldArea))
  GO lnRecNo
ENDIF
*B603745,1 KAM[end] 
IF lnMessAns = 3 then
  laData[8] = ' ' 
  _CUROBJ = OBJNUM(laData[8])
ENDIF
RETURN
*-
*!**************************************************************************
*! Func. Name: lfvbrwsD_8()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/31/1999
*! Purpose   : BROWSE  Lnk Code
*!**************************************************************************
*! Calls     : =lfvData_8() 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvbrwsD_8()
*!**************************************************************************
FUNCTION  lfvbrwsD_8
IF llBrowse 
  RETURN
ENDIF
llBrowse   = .T.
=lfDsgnLnk()
_CUROBJ = OBJNUM(laData[8])
return .t.
*!**************************************************************************
*! Func. Name: lfChkNgtv()
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/31/1999
*! Purpose   : chk. Negative Values In Fields 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfChkNgtv()
*!**************************************************************************
FUNCTION lfChkNgtv
IF EVAL(SYS(18)) < 0 THEN
  = gfModalGen("TRM42000B00000","DIALOG",'')
  _CUROBJ = _CUROBJ
ENDIF


*!**************************************************************************
*! Func. Name: lfvDsgnOLnk()
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/25/1999
*! Purpose   : Call Get Object link Prog. with type 'N' and ladata[1]
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvDsgnOLnk()
*E301311,1 WAB 
*!**************************************************************************
FUNCTION lfvDsgnOLnk
DO GETOBJ WITH 'G',laData[1]

