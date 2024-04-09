*!*******************************************************************************
*! Program file  : SoJLDsn.PRG
*! Program desc. : Custom Design Screen
*! Screen        : SoJLDsn.SPR
*! System        : Aria Apparel System
*! Module        : Sales Order (SO)
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 04/23/2001
*! Reference     : C102230
*!*******************************************************************************
*! Example       : DO SoJLDsn
*!*******************************************************************************
*B804410,1 SSE 09/11/2001 Fix bug of adding and deleting some fields in browse screen.
*B605989,4 AMH 06/16/2002 Rename the account field in ICDESIGN file to cdsgnacct.

EXTERNAL ARRAY laData , laKeyField

DECLARE laKeyField [3,4] , laStatus[2] , laType[3]
DECLARE laCodeInfo[2,10] , laDesign[1] , laRoyalty[1]
DECLARE laSource[1] , laTarget[1] , laCodesVal[1]     && Used in Design Location 

PRIVATE llBrowse , lcDsgPos1 , lcDsgPos2 , lcDsgPos3 , lcDsgPos4 , lcDsgPos5 , ;
        lcDsgPos6 , lnStatus , lnType , lnColorChg , lcStName , lcStockDsc
STORE .F. TO llBrowse , llCUpdate
STORE 1 TO lnStatus , lnType , lnColorChg
STORE '' TO lcDsgPos1 , lcDsgPos2 , lcDsgPos3 , lcDsgPos4 , lcDsgPos5 , lcDsgPos6 , ;
            lcStName , laSource , laTarget

laDefProc[9]  = .F.     && Save procedure(lpSavScr)

*-- For Design Status. 
laStatus[1] = 'Approved'
laStatus[2] = 'Pending'

*-- For Design Type.
laType[1] = 'Stock Design'
laType[2] = 'Custom Design'
laType[3] = 'Customer Design'

*-- For Design Size
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

*--For Royalty
laCodeInfo[2,1]  = 'Royalty'
laCodeInfo[2,2]  = 'laRoyalty'
laCodeInfo[2,3]  = 'puRoyalty'
laCodeInfo[2,4]  = ''
laCodeInfo[2,5]  = .F.
laCodeInfo[2,6]  = .T.
laCodeInfo[2,7]  = ''
laCodeInfo[2,8]  = ''
laCodeInfo[2,9]  = ''
laCodeInfo[2,10] = ''

*--The Tag Expression of the default Selected Database File
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'ACCTDSGN'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .F.
laKeyField[2,3] = 'ACCTDSGN'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[3]'
laKeyField[3,2] = .F.
laKeyField[3,3] = 'ACCTDSGN'
laKeyField[3,4] = 3

*--Initilize screen and program variables .
lnArryPos = 0			&& Variable for array position (popup field)

IF !gfSetup()    
  RETURN
ENDIF  

*-- Collect all the Style Positions from Codes file
SELECT cDiscrep FROM Codes WHERE cDefCode = "N" AND cFld_Name = "CSTYLEPOS" INTO ARRAY laSource
SELECT cCode_No FROM Codes WHERE cDefCode = "N" AND cFld_Name = "CSTYLEPOS" INTO ARRAY laCodesVal

*--Fields Name.

*B605989,4 AMH Rename the account field in ICDESIGN file to cdsgnacct [Start]
*lcScFields ='Account,Store,cDsgnCode,cDsnName,Status,Type,cStockDsn,cDsgnName,nStitches,cDsgnSize,;
             cNameDrp1,cNameDrp2,cNameDrp3,Royalty,nRoyltAmt,nDsgnPrc,nDsgnCst,cColorChg,;
             cStylePos1,cstylepos2,cstylepos3,cstylepos4,cstylepos5,cstylepos6' 
lcScFields ='cDsgnAcct,SkuStore,cDsgnCode,cDsnName,cDsgStatus,cDsgnType,cStockDsn,cDsgnName,nStitches,cDsgnSize,'+;
            'cNameDrp1,cNameDrp2,cNameDrp3,Royalty,nRoyltAmt,nDsgnPrc,nDsgnCst,cColorChg,'+;
            'cStylePos1,cstylepos2,cstylepos3,cstylepos4,cstylepos5,cstylepos6'
*B605989,4 AMH [End]

DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
*--Object link button.
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvDsgnOLnk() MESSAGE 'Object link' DISABLE]

SELECT (lcBaseFile)
*-- if it is time to activate the window scatter an empty record
IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

*-- call the gfwCodePop with "N/A" Display.
=gfwCodePop(@laCodeInfo,'cDsgnSize','N')
=gfwCodePop(@laCodeInfo,'Royalty','N')

*-- call the screen file ( SOJLDSN.SPX  )
PUSH KEY
ON KEY LABEL ESC DO lpEscap
DO (gcScrDir + gcWinAppl + '\SOJLDSN.SPX')
POP KEY
*-- End of Program.

*!*******************************************************************************
*! Name      : lfGetArry()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : get Value From Table Fields to the Popup.
*!*******************************************************************************
*! Calls     : gfwCodePop()
*!*******************************************************************************
*! Example   : =lfGetArry()
*!*******************************************************************************
*
FUNCTION lfGetArry
PRIVATE lcAlias , lnArryPos
lcAlias = ALIAS()

=gfwCodePop(@laCodeInfo,'cDsgnSize','L')
=gfwCodePop(@laCodeInfo,'Royalty','L')
SELECT IcDesign 
lnArryPos  = ASCAN(laDesign, ICDESIGN.cDsgnSize)	
IF  lnArryPos = 0 then
  pudesign = 1
ELSE
  puDesign = (lnArryPos+1)/2
ENDIF
lnArryPos  = ASCAN(laRoyalty, ICDESIGN.Royalty)	
IF  lnArryPos = 0 then
  puRoyalty = 1
ELSE
  puRoyalty  = lnArryPos /2
ENDIF
SHOW GET puDesign  DISABLE
SHOW GET puRoyalty DISABLE
SELECT (lcAlias)
RETURN
*-- End of lfGetArry.

*!*******************************************************************************
*! Name      : lfvDsgnOLnk()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Call Get Object link Prog. with type 'N' and ladata[1].
*!*******************************************************************************
*! Example   :  =lfvDsgnOLnk()
*!*******************************************************************************
*
FUNCTION lfvDsgnOLnk
PRIVATE lcKeyValue
lcKeyValue = laData[1]+laData[2]+laData[3]
DO GETOBJ WITH 'G' , lcKeyValue
*-- End of lfvDsgnOLnk.

*!*******************************************************************************
*! Name      : lfvDsgnLoc
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validation for Design Location push button.
*!*******************************************************************************
*! Example   : =lfvDsgnLoc()
*!*******************************************************************************
*
FUNCTION lfvDsgnLoc
PRIVATE lnLoop , lcLoop , lnInSource
= gfMover(@laSource,@laTarget,'Design Locations ',.T.,'lfvNoOfPos()')
STORE '' TO lcDsgPos1 , lcDsgPos2 , lcDsgPos3 , lcDsgPos4 , lcDsgPos5 , lcDsgPos6
STORE '' TO laData[19] , laData[20] , laData[21] , laData[22] , laData[23] , laData[24]
IF !EMPTY(laTarget[1])
  FOR lnLoop = 1 TO ALEN(laTarget,1)
    lnInSource  = ASCAN(laSource,"\"+laTarget[lnLoop],1)
    lcLoop = ALLTRIM(STR(lnLoop,6))
    laData[18+lnLoop] = laCodesVal[lnInSource]
    lcDsgPos&lcLoop = laTarget[lnLoop] 
  ENDFOR
ENDIF
=lfRefresh()
*-- End of lfvDsgnLoc.

*!*******************************************************************************
*! Name      : lfvNoOfPos
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Check the Maximum number of style positions.
*!*******************************************************************************
*! Example   : =lfvNoOfPos()
*!*******************************************************************************
*
FUNCTION lfvNoOfPos
IF ALEN(latarget) > 5 then
  = gfModalGen('TRB00000','F',' ',' ','You can not select more than 6 style positions.') 
 RETURN .F.
ENDIF
*-- End of lfvNoOfPos.

*!*******************************************************************************
*! Name      : lpShow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Show objects function.
*!*******************************************************************************
*! Calls     : gfwCodePop()
*!			 : lfGetArry()
*!*******************************************************************************
*! Example   :  =lpShow()
*!*******************************************************************************
*
PROCEDURE lpShow
DO CASE
  CASE laScrMode[1]
    SHOW GET pbUsrFields DISABLE
    SHOW GET pbObjlnk DISABLE
    lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')
    STORE 1 TO lnStatus , lnType , lnColorChg
    STORE '' TO lcDsgPos1 , lcDsgPos2 , lcDsgPos3 , lcDsgPos4 , lcDsgPos5 , lcDsgPos6 , ;
                lcStockDsc
    
    *-- call the gfwCodePop with "N/A" Display.
    =gfwCodePop(@laCodeInfo,'cDsgnSize','N')
    =gfwCodePop(@laCodeInfo,'Royalty','N')
    
    SHOW GET lcStockDsc DISABLE
    SHOW GET ibAccount ENABLE
    SHOW GET ibStore DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET ibBrowKey DISABLE
    SHOW GET laData[3] DISABLE            
    =lfRefresh()
    
    DECLARE laTarget[1]
    laTarget = ''
    PRIVATE lnLoop
    FOR lnLoop = 1 TO ALEN(laSource,1)
      laSource[lnLoop] = IIF(SUBSTR(laSource[lnLoop],1,1)="\",SUBSTR(laSource[lnLoop],2),laSource[lnLoop])
    ENDFOR                  
  CASE laScrMode[2]
    =lfGetArry()
    IF EMPTY(laData[2])
      lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')
    ELSE
      lcStName = LOOKUP(Customer.StName,'S'+laData[1]+laData[2],Customer.Type,'Customer')    
    ENDIF
    
    IF EMPTY(laData[7])
      lcStockDsc = ''
    ELSE
      lcStockDsc = IIF(SEEK('NCSTOCKDSN '+laData[7],'Codes'),Codes.cDiscrep,'')
    ENDIF
    
    SHOW GET lcStockDsc DISABLE
    SHOW GET pbUsrFields DISABLE
    SHOW GET ibAccount DISABLE
    SHOW GET pbObjlnk ENABLE
    lnStatus   = IIF(laData[5] = "A",1,2)
    lnType     = IIF(laData[6] = "S",1,IIF(laData[6]="D",2,3))
    lnColorChg = VAL(laData[18])
    
    PRIVATE lnLoop , lcLoop , lnInSource , lnPosNum
    lnPosNum = 0
    FOR lnLoop = 6 TO 1 STEP -1
      IF !EMPTY(laData[18+lnLoop])
        lnPosNum = lnLoop
        EXIT
      ENDIF  
    ENDFOR
    
    IF lnPosNum > 0
      DECLARE laTarget[lnPosNum]
      laTarget = ''
    ENDIF
    
    FOR lnLoop = 1 TO lnPosNum
      lnInSource  = ASCAN(laCodesVal,laData[18+lnLoop],1)
      lcLoop = ALLTRIM(STR(lnLoop,6))
      laTarget[lnLoop] = IIF(SUBSTR(laSource[lnInSource],1,1)="\",SUBSTR(laSource[lnInSource],2),laSource[lnInSource])
      laSource[lnInSource] = IIF(SUBSTR(laSource[lnInSource],1,1)="\","","\") + laSource[lnInSource]
      lcDsgPos&lcLoop = laTarget[lnLoop] 
    ENDFOR
    =lfRefresh()
   
  CASE laScrMode[3]
    SELECT IcDesign
    =gfObj_lock(.F.)
    
    SHOW GET pbUsrFields DISABLE
    SHOW GET ibAccount DISABLE
    SHOW GET pbObjlnk ENABLE
    IF lnType = 1
      SHOW GET laData[7] ENABLE
      SHOW GET lcStockDsc DISABLE
    ELSE
      STORE '' TO laData[7] , lcStockDsc
      SHOW GET laData[7] DISABLE
      SHOW GET lcStockDsc DISABLE
    ENDIF
    
  CASE laScrMode[4]
    =gfwCodePop(@laCodeInfo,'cDsgnSize','D')
    =gfwCodePop(@laCodeInfo,'Royalty','D')

    laData[10] = laDesign[pudesign,2]
    laData[14] = laRoyalty[puRoyalty,2]

    SHOW GET lcStockDsc ENABLE
    SHOW GET pbUsrFields DISABLE
    SHOW GET ibAccount DISABLE
    laData[5] = IIF(lnStatus=1,"A","P")
    laData[6] = IIF(lnType=1,"S",IIF(lnType=2,"D","C"))  
    SHOW GET pbObjlnk ENABLE
ENDCASE 
=lfRefresh()
*-- End of lpShow.

*!*******************************************************************************
*! Name      : lfwRoyalty()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : before focus to fill the popup Royalty .
*!*******************************************************************************
*! Calls     : gfwCodePop()
*!*******************************************************************************
*! Example   : =lfwRoyalty()
*!*******************************************************************************
*
FUNCTION lfwRoyalty
=gfwCodePop(@laCodeInfo,'Royalty','L')
laData[14] = laRoyalty[puRoyalty,2]
RETURN
*-- End of lfwRoyalty.

*!*******************************************************************************
*! Name      : lfvRoyalty()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : assign value from popup to Royalty.
*!*******************************************************************************
*! Example   : =lfvRoyalty()
*!*******************************************************************************
*
FUNCTION lfvRoyalty
laData[14] = laRoyalty[puRoyalty,2]
RETURN
*-- End of lfvRoyalty.

*!*******************************************************************************
*! Name      : lfwDesign()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : before focus to fill the popup design Code.
*!*******************************************************************************
*! Calls     : gfwCodePop()
*!*******************************************************************************
*! Example   : =lfwDesign()
*!*******************************************************************************
*
FUNCTION lfwDesign
=gfwCodePop(@laCodeInfo,'cDsgnSize','L')
laData[10] = laDesign[pudesign,2]
RETURN
*-- End of lfwDesign.

*!*******************************************************************************
*! Name      : lfvDesign()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : assign value from popup to design size.
*!*******************************************************************************
*! Example   : =lfvDesign()
*!*******************************************************************************
*
FUNCTION lfvDesign
laData[10] = laDesign[pudesign,2]
RETURN
*-- End of lfvDesign.

*!*******************************************************************************
*! Name      : lfvStatus
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validation for Status Popup.
*!*******************************************************************************
*! Example   : =lfvStatus()
*!*******************************************************************************
*
FUNCTION lfvStatus
laData[5] = IIF(lnStatus=1,"A","P")
*-- End of lfvStatus.

*!*******************************************************************************
*! Name      : lfvType
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validation for Design type Popup.
*!*******************************************************************************
*! Example   : =lfvType()
*!*******************************************************************************
*
FUNCTION lfvType
IF lnType = 1
  SHOW GET ibStock   ENABLE
  SHOW GET laData[7] ENABLE
  SHOW GET lcStockDsc DISABLE
  IF laScrMode[4] AND !EMPTY(laData[7]) AND !SEEK("NCSTOCKDSN "+laData[7],"Codes")
    SHOW GET lcStockDsc ENABLE
  ENDIF  
ELSE
  STORE '' TO laData[7] , lcStockDsc
  SHOW GET ibStock   DISABLE
  SHOW GET laData[7] DISABLE
  SHOW GET lcStockDsc DISABLE
ENDIF
laData[6] = IIF(lnType=1,"S",IIF(lnType=2,"D","C"))
*-- End of lfvType.

*!*******************************************************************************
*! Name      : lfChkNgtv()
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : chk. Negative Values In Fields.
*!*******************************************************************************
*! Example   : =lfChkNgtv()
*!*******************************************************************************
*
FUNCTION lfChkNgtv
IF EVAL(SYS(18)) < 0 THEN
  = gfModalGen("TRM42000B00000","DIALOG",'')
  _CUROBJ = _CUROBJ
ENDIF
*-- End of lfChkNgtv.

*!*******************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid Customer Account Code.
*!*******************************************************************************
*! Reference : C102230,1
*!*******************************************************************************
*! Example   : =lfvAccount()
*!*******************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcAccount

IF (!llBrowse AND EMPTY(laData[1])) OR MDOWN()
  RETURN .T.
ENDIF

IF !EMPTY(laData[1]) AND LEN(ALLTRIM(laData[1])) < 5 AND !('?' $ laData[1])
  *-- Message : 40057
  *-- Account code should not be less than 5 characters.
  *-- Button : 00000
  *-- <Browse> <Reenter>
  IF gfModalGen('TRM40057B40007','ALERT') = 2
    _CUROBJ  = OBJNUM(ladata[1])
    lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')  
    =lfRefresh()
    RETURN
  ENDIF
  llBrowse = .T.    
ENDIF
IF llBrowse OR '?' $ laData[1] OR (!EMPTY(laData[1]) AND !SEEK('M'+laData[1],'Customer'))
  lcAccount = laData[1]
  DO CUSBROWM WITH lcAccount
  laData[1] = lcAccount
ENDIF
llBrowse = .F.
IF EMPTY(laData[1])
  laData[2] = ''
  laData[3] = ''
  SHOW GET ibStore   DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET laData[3] DISABLE
  lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')  
  =lfRefresh()
  _CUROBJ = OBJNUM(laData[1])
  RETURN
ENDIF
IF LEN(ALLTRIM(laData[1])) < 5
  _CUROBJ = _CUROBJ
  lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')  
  =lfRefresh()
  RETURN
ENDIF
IF SEEK('M'+laData[1],'Customer')
  SHOW GET ibStore   ENABLE
  SHOW GET laData[2] ENABLE
  lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')  
  =lfRefresh()
ELSE
ENDIF
*-- End of lfvAccount.

*!*******************************************************************************
*! Name      : lfvStore
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validate customer store code.
*!*******************************************************************************
*! Calls     : CUSBROWS,gfSeekRec
*!*******************************************************************************
*! Reference : C102230,1
*!*******************************************************************************
*! Example   : =lfvStore()
*!*******************************************************************************
*
FUNCTION lfvStore
PRIVATE xStore , lcOldAcc

IF MDOWN() AND !llBrowse
  RETURN
ENDIF
IF llBrowse OR '?' $ laData[2] OR (!EMPTY(laData[2]) AND !SEEK('S'+laData[1]+laData[2],'Customer'))
  xStore   = laData[2]
  IF !CUSBROWS(laData[1],.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  laData[2] = xStore
ENDIF
IF EMPTY(laData[2])
  lcStName = LOOKUP(Customer.StName,'M'+laData[1],Customer.Type,'Customer')
  =lfRefresh()
ELSE
  IF SEEK('S'+laData[1]+laData[2],'Customer')
    lcStName = LOOKUP(Customer.StName,'S'+laData[1]+laData[2],Customer.Type,'Customer')
    =lfRefresh()
  ENDIF    
ENDIF

SHOW GET ibBrowKey ENABLE
SHOW GET laData[3] ENABLE
llBrowse = .F.
*-- End of lfvStore.

*!*******************************************************************************
*! Name      : lfvDsgnCod
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid the design code and browse from table icdesign.
*!*******************************************************************************
*! Example   : =lfvDsgnCod()
*!*******************************************************************************
*
FUNCTION lfvDsgnCod
PRIVATE lcBrFields , lcAlias , lnMessAns , lcField

lcAlias = ALIAS()
lnMessAns = 0

DECLARE laTmpDat[1]
laTmpDat[1] = ' '

IF (!llBrowse AND EMPTY(laData[3])) OR MDOWN()
  RETURN .T.
ENDIF
SELECT ICDESIGN

IF !llBrowse AND laData[3] <> '?'
  SEEK laData[1]+laData[2]+laData[3] 
  IF FOUND()
    laScrMode = .F.
    laScrMode[2] = .T.
    SCATTER FIELDS &lcScFields MEMO TO laData
    SHOW GETS    
  ELSE
    lnMessAns = gfModalGen("TRM00001B00001","DIALOG","Design Code")
    DO CASE
      CASE lnMessAns = 1
        llBrowse = .T.
      CASE lnMessAns = 2
        laScrMode = .F.
        laScrMode[4] = .T.
        SHOW GETS
    ENDCASE
  ENDIF
ENDIF
IF llBrowse OR laData[3] = '?'
  IF SEEK(laData[1]+laData[2])
    lcField     = 'cDsgnCode'
    
    *B804410,1 Modify the browse screen. [Begin]
    *lcBrFields  = "cDsgnCode :12 :H='Design Code',"+;
    *              "cDsgnName :40 :H='Design Name',"+;
    *              "Royalty   :12 :H='Royality',"+;
    *              "nRoyltAmt :20 :H='Royality Amount',"+;
    *              "nDsgnCst  :20 :H='Design Adornment Cost',"+;
    *              "nDsgnPrc  :20 :H='Design Adornment Price',"+;
    *              "cStockDsn :12 :H='Stock Design Code',"+;
    *              "nStitches :20 :H='Number of Stitches',"+;
    *              "cNameDrp1 :40 :H='Name Drop One',"+;
    *              "cNameDrp2 :40 :H='Name Drop Two',"+;
    *              "cNameDrp3 :40 :H='Name Drop Three'"
    lcBrFields  = "cDsgnCode :12 :H='Design Code',"+;
                  "cDsnName :40 :H='Design Name',"+;
                  "Royalty   :12 :H='Royality',"+;
                  "cStockDsn :12 :H='Stock Design Code',"+;
                  "nStitches :20 :H='Number of Stitches',"+;
                  "cNameDrp1 :40 :H='Name Drop One',"+;
                  "cNameDrp2 :40 :H='Name Drop Two',"+;
                  "cNameDrp3 :40 :H='Name Drop Three'"
    *B804410,1 Modify the browse screen. [End]

    *B605989,4 AMH Rename the account field in ICDESIGN file to cdsgnacct [Start]
    *=gfBrows('FOR Account+Store+cDsgnCode = laData[1]+laData[2]',lcField,"laTmpDat",'Design Code','','',.F.)
    =gfBrows('FOR cDsgnAcct+SkuStore+cDsgnCode = laData[1]+laData[2]',lcField,"laTmpDat",'Design Code','','',.F.)
    *B605989,4 AMH [End]

    IF EMPTY(laTmpDat[1])
      laData[3] = ''
      _CUROBJ = OBJNUM(laData[3])    
    ELSE
      laData[3] = laTmpDat[1]
      laScrMode = .F.
      laScrMode[2] = .T.
      SCATTER FIELDS &lcScFields MEMO TO laData
      SHOW GETS
    ENDIF  
  ELSE
    = gfModalGen("TRM00000B00000","DIALOG",'','','No Design Code found for this Account , Store.')
    laData[3] = ''
    _CUROBJ = OBJNUM(laData[3])
  ENDIF  
ENDIF
llBrowse = .F.

IF lnMessAns = 3 then
  laData[3] = ' ' 
  _CUROBJ = OBJNUM(laData[3])
ENDIF

SELECT(lcAlias)
RETURN
*-- End of lfvDsgnCod.

*!*******************************************************************************
*! Name      : lfvStckDsn
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid the stock design and browse from table icdesign.
*!*******************************************************************************
*! Example   : =lfvStckDsn()
*!*******************************************************************************
*
FUNCTION lfvStckDsn
PRIVATE lcBrFields , lcAlias , lnMessAns , lcField , lnRecNo

lcAlias = ALIAS()
lnRecNo = RECNO()
lnMessAns = 0

DECLARE laTmpDat[1]
laTmpDat[1] = ' '

IF (!llBrowse AND EMPTY(laData[7])) OR MDOWN()
  lcStockDsc = ''
  SHOW GET lcStockDsc ENABLE    
  RETURN .T.
ENDIF
SELECT Codes

IF !llBrowse AND laData[7] <> '?'
  SEEK "NCSTOCKDSN " + laData[7] 
  IF FOUND()
    lcStockDsc = Codes.cDiscrep
    SHOW GET lcStockDsc DISABLE    
  ELSE
    lnMessAns = gfModalGen("TRM00001B00001","DIALOG","Codes")    
    IF lnMessAns = 1 THEN
      llBrowse = .T.    
    ENDIF
    IF lnMessAns = 2 THEN
      lcStockDsc = ''
      SHOW GET lcStockDsc ENABLE
    ENDIF
  ENDIF
ENDIF
IF llBrowse OR laData[7] = '?'
  lcField     = 'cCode_No'
  lcBrFields  = "cCode_No :20 :H='Stock Design Code',"+;
                "cDiscRep :40 :H='Stock Design Description'"
  =gfBrows([FOR cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'NCSTOCKDSN '],lcField,"laTmpDat",'Stock Design Codes','','',.F.)
  laData[7] = laTmpDat[1]
ENDIF
IF EMPTY(laData[7])
  lcStockDsc = ''
  SHOW GET lcStockDsc DISABLE
ELSE
  IF SEEK("NCSTOCKDSN " + laData[7],'Codes')
    lcStockDsc = Codes.cDiscrep
    SHOW GET lcStockDsc DISABLE
  ENDIF  
ENDIF
llBrowse   = .F.

SELECT(lcAlias)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcAlias))
  GO lnRecNo
ENDIF

IF lnMessAns = 3 then
  laData[7] = ' ' 
  _CUROBJ = OBJNUM(laData[7])
ENDIF
RETURN
*-- End of lfvStckDsn.

*!*******************************************************************************
*! Name      : lfvColorChg
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validation for the color change spinner. 
*!*******************************************************************************
*! Example   : =lfvColorChg()
*!*******************************************************************************
*
FUNCTION lfvColorChg
laData[18] = ALLTRIM(STR(lnColorChg))
*-- End of lfvColorChg.

*!*******************************************************************************
*! Name      : gfCPBrows
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Override the global function Select in the ToolBar.
*!*******************************************************************************
*! Example   : =gfCPBrows()
*!*******************************************************************************
*
FUNCTION gfCPBrows
IF !EMPTY(laData[1]) AND SEEK('M'+laData[1],'Customer')
  llBrowse = .T.
  =lfvDsgnCod()  
ELSE
  llBrowse = .F.
  = gfModalGen("TRM00000B00000","DIALOG",'','','You have to select Account code first.')
  _CUROBJ = OBJNUM(laData[1])
ENDIF
*-- End of gfCPBrows.

*!*******************************************************************************
*! Name      : lpSavScr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Validation done before saving the screen.
*!*******************************************************************************
*! Example   : DO lpSavScr
*!*******************************************************************************
*
FUNCTION lpSavScr
PRIVATE lnLRecNo , llSaveIt

IF (lnType = 1 AND EMPTY(laData[7])) OR EMPTY(laTarget[1])
  IF lnType = 1 AND EMPTY(laData[7])
    *-- Message <You have to enter a Stock Design, Cannot save.>
    *-- Buttons <                      OK                      >
    = gfModalGen("TRM00000B00000","DIALOG",'','','You have to enter a Stock Design, Cannot save.')
    _CUROBJ = OBJNUM(laData[7])
    llcSave = .F.
  ELSE
    *-- Message <You have to enter a Design Location, Cannot save.>
    *-- Buttons <                       OK                        >
    = gfModalGen("TRM00000B00000","DIALOG",'','','You have to enter a Design Location, Cannot save.')
    _CUROBJ = OBJNUM(PbDsgnLoc)
    llcSave = .F.  
  ENDIF  
ELSE
  IF TYPE('laCtrStat[14]')#'U' AND TYPE('lcBaseFile')='C' AND ;
     TYPE('laUsrFields') # "U" AND !EMPTY(laUsrFields)    AND ;
     !EMPTY(lcBaseFile)
     IF !gfVldUsrFld(@laUsrFields)
       RETURN 
     ENDIF
  ENDIF   
  IF laScrMode[4]
    APPEND BLANK
    llSaveIt = .T.
  ELSE
    *-- Check if the record has been modified after user start editing
    lnLRecNo = IIF(RECNO()>RECCOUNT(),0,RECNO())
    IF lnLRecNo <> 0
      GO lnLRecNo
    ENDIF  
    IF !EMPTY(lcBaseFile)
      lcNStamp =  cAdd_User+IIF(EMPTY(dAdd_Date),'',DTOC(dAdd_Date))+cAdd_Time
      IF ALLTRIM(lcStamp) = ALLTRIM(lcNStamp)
        llSaveIt = .T.
      ELSE
        IF gfModalGen("QRM00030B00012","DIALOG",cLok_User) = 1
          llSaveIt = .T.
        ELSE
          llSaveIt = .F.
        ENDIF 
      ENDIF
    ELSE
      llSaveIt = .T.       
    ENDIF 
  ENDIF
  
  IF llSaveIt 
    GATHER FROM laData FIELDS &lcScFields MEMO 
    =gfAdd_Info()                     && Add the add user,time and date
    =gfObj_Lock(.F.)                  && Unlock the record after saving
    llCUpDate = .F.
  ENDIF  
  
  IF laScrMode[3]                     && If comming from add mode 
    SELECT Codes
    IF !SEEK("DCSTOCKDSN ","Codes")
      APPEND BLANK
      =gfAdd_Info()                     && Add the add user,time and date
      =gfObj_Lock(.F.)                  && Unlock the record after saving
      REPLACE cDefCode WITH "D" , cFld_Name WITH "CSTOCKDSN" , cCode_No WITH laData[7] ,;
              cDiscrep WITH lcStockDsc , lRltFields WITH .F. , cRltField WITH "N"             
    ENDIF         

    IF !SEEK("NCSTOCKDSN "+laData[7],"Codes")
      APPEND BLANK
      =gfAdd_Info()                     && Add the add user,time and date
      =gfObj_Lock(.F.)                  && Unlock the record after saving
      REPLACE cDefCode WITH "N" , cFld_Name WITH "CSTOCKDSN" , cCode_No WITH laData[7] ,;
              cDiscrep WITH lcStockDsc , lRltFields WITH .F. , cRltField WITH "N"             
    ENDIF         
  ENDIF

  IF laScrMode[4]                     && If comming from add mode 
    SELECT Codes
    IF !SEEK("DCSTOCKDSN ","Codes")
      APPEND BLANK
      =gfAdd_Info()                     && Add the add user,time and date
      =gfObj_Lock(.F.)                  && Unlock the record after saving
      REPLACE cDefCode WITH "D" , cFld_Name WITH "CSTOCKDSN" , cCode_No WITH laData[7] ,;
              cDiscrep WITH lcStockDsc , lRltFields WITH .F. , cRltField WITH "N"             
    ENDIF         

    IF !SEEK("NCSTOCKDSN "+laData[7],"Codes")
      APPEND BLANK
      =gfAdd_Info()                     && Add the add user,time and date
      =gfObj_Lock(.F.)                  && Unlock the record after saving
      REPLACE cDefCode WITH "N" , cFld_Name WITH "CSTOCKDSN" , cCode_No WITH laData[7] ,;
              cDiscrep WITH lcStockDsc , lRltFields WITH .F. , cRltField WITH "N"             
    ENDIF         
    laScrMode    = .F.                && Return all screen mode to normal
    laScrMode[1] = .T.                && Back to select mode
    
  ELSE
    laScrMode    = .F.                && Return all screen mode to normal
    laScrMode[2] = .T.                && Go to view mode 
  ENDIF

  llcSave = .F.
  SHOW GETS
ENDIF
*-- End of lpSavScr.

*!*******************************************************************************
*! Name      : gfMover
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Display the mover screen.
*!*******************************************************************************
*! Example   : =gfMover()
*!*******************************************************************************
*
FUNCTION gfMover
PARAMETERS laSource,laTarget,lcMovTitle,llOpenAll,lcVldFunc,llMV1By1,llReMV1By1

DIMENSION laSource[ALEN(laSource,1),1],laTarget[ALEN(laTarget,1),1]
lnLoop    = 0

*** This for the mover title. ***
lcMovTitle = IIF(TYPE("lcMovTitle") $ "UL" , "Mover" , lcMovTitle)
lcMovTitle = IIF(EMPTY(lcMovTitle),"Mover" ,lcMovTitle)

lnWidTit  = IIF(LEN(ALLTRIM(lcMovTitle)) > 74,74,LEN(ALLTRIM(lcMovTitle)))
lnWidTit  = IIF(lnWidTit = 0 , 1 , lnWidTit )
llOpenAll = IIF(TYPE("llOpenAll")="U",.F.,llOpenAll)

IF llOpenAll
  llCanVald = .T.
ELSE
  llCanVald = (laScrMode[3] .OR. laScrMode[4]) 
ENDIF  

lnMaxList = 1

FOR lnCount = 1 TO ALEN(laSource,1)
  IF TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N') > lnMaxList
    lnMaxList = TXTWIDTH(laSource[lnCount],'MS Sans Serif',9,'N')
  ENDIF
ENDFOR

lnMaxList = IIF(_DOS,IIF(lnMaxList+4 > 31 , 31 , lnMaxList+4 ),;
                     IIF(lnMaxList+7 > 27 , 27 , lnMaxList+7 ))
lnMaxScrn = IIF(_DOS,MAX(lnWidTit,(lnMaxList * 2) + 16),(lnMaxList*2)+31)

lnColPos = IIF(LEN(ALLTRIM(lcMovTitle)) > lnMaxScrn,0,;
           INT((lnMaxScrn - LEN(ALLTRIM(lcMovTitle)))/2))

EXTERNAL ARRAY laSource,laTarget
lnOldDim =ALEN(laTarget,1)
DECLARE laOldTarg[lnOldDim]

=ACOPY(laTarget,laOldTarg)

lnOldSour =ALEN(laSource,1)
DECLARE laOldSour[lnOldSour]
=ACOPY(laSource,laOldSour)

IF ALEN(laTarget,1) = 1 .AND. TYPE('laTarget[1]')="L"
  laTarget =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSource',1)
  IF ASCAN('laTarget',ALLTRIM(laSource[lnCount])) > 0
    laSource[lnCount,1] = '\'+laSource[lnCount,1]
  ENDIF
ENDFOR

lsSource = IIF(LEFT(laSource[1,1] ,1) = "\",0,1)
lsTarget = 1

PUSH KEY
ON KEY
ON KEY LABEL ESC DO lfCancel
DO SYMover.SPR
POP KEY
*-- End of gfMover.

*!*******************************************************************************
*! Name      : lfShow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Show the mover screen.
*!*******************************************************************************
*! Example   : =lfShow()
*!*******************************************************************************
*
FUNCTION lfShow
IF llCanVald 
  IF ALEN('laTarget',1) = ALEN('laSource',1)  AND !EMPTY(laTarget[1]);
    OR EMPTY(laSource[1])
    SHOW GET lsSource     DISABLE
    SHOW GET pbMove       DISABLE
    SHOW GET pbAll        DISABLE
  ELSE
    SHOW GET lsSource     ENABLE
    SHOW GET pbMove       ENABLE
    SHOW GET pbAll        ENABLE
  ENDIF  
  
  IF EMPTY(laTarget[1])
    SHOW GET lsTarget    DISABLE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE
  ELSE
    SHOW GET lsTarget    ENABLE
    SHOW GET pbRemove    ENABLE
    SHOW GET pbRAll      ENABLE
  ENDIF  
ELSE
    SHOW GET pbRemove    DISABLE
    SHOW GET pbRAll      DISABLE    
    SHOW GET pbMove      DISABLE
    SHOW GET pbAll       DISABLE    
    SHOW GET pbCancel    DISABLE
ENDIF  
SHOW GET pbAll        DISABLE
*-- End of lfShow.

*!*******************************************************************************
*! Name      : lfvSource
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid for the source Popup.
*!*******************************************************************************
*! Example   : =lfvSource()
*!*******************************************************************************
*
FUNCTION lfvSource

IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
  IF !&lcVldFunc.(1)
    RETURN .F.
  ENDIF  
ENDIF  

IF lsSource = 0
  RETURN .F.
ENDIF

IF lsSource <= ALEN('laSource',1) AND lsSource <> 0
  IF !EMPTY(laTarget[1]) 
    DIMENSION laTarget[ALEN(laTarget)+1]
  ENDIF
  laTarget[ALEN(laTarget)]= ALLTRIM(laSource[lsSource])
    laSource[lsSource] = "\"+laSource[lsSource]
    IF lsSource < ALEN(laSource)
      DO WHILE LEFT(laSource[lsSource],1) = "\" 
        lsSource = lsSource + 1
        IF lsSource = ALEN(laSource)
          EXIT
        ENDIF
      ENDDO  
    ELSE
      lsSource = 1  
    ENDIF
ENDIF  

IF lsSource > ALEN(laSource,1)
  lsSource = ALEN(laSource,1)
ENDIF

_CUROBJ = OBJNUM(lsSource)
SHOW GETS
*-- End of lfvSource.

*!*******************************************************************************
*! Name      : lfvTarget
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid function for the Target Popup.
*!*******************************************************************************
*! Example   : =lfvTarget()
*!*******************************************************************************
*
FUNCTION lfvTarget
*IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) 
*  IF !&lcVldFunc.(3)
*    IF lnLoop > 0
*      lnLoop = lnLoop + 1
*    ENDIF  
*    RETURN .F.
*  ENDIF  
*ENDIF  

lcExactSav = SET("EXACT")
SET EXACT ON

IF lsTarget <= ALEN('laTarget',1) .AND.;
   lsTarget <> 0 .AND.;
   ASCAN('laSource',"\"+laTarget[lsTarget]) > 0

  lsSource  = ASUBSCRIPT('laSource',;
                   ASCAN('laSource',"\"+laTarget[lsTarget]),1)
  *-- Adding this line in order to enable the element in the source list 
  *-- when removing it from the target one.
  laSource[lsSource] = SUBSTR(laSource[lsSource],2,LEN(laSource[lsSource]))

  =ADEL(laTarget,lsTarget)
  IF ALEN(laTarget,1) > 1
    DIMENSION laTarget[ALEN(laTarget)-1]
  ELSE
    laTarget[1] =' '
  ENDIF  
ENDIF


IF lsTarget = 0
  lsTarget = 1
ENDIF  

IF lsTarget > ALEN(laTarget,1)
  lsTarget = ALEN(laTarget,1)
ENDIF

_CUROBJ = OBJNUM(lsTarget)
SHOW GETS
SET EXACT &lcExactSav
*-- End of lfvTarget.

*!*******************************************************************************
*! Name      : lfvMovmnts
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid function for moving to or from Target array.
*!*******************************************************************************
*! Example   : =lfvMovmnts()
*!*******************************************************************************
*
FUNCTION lfvMovmnts
PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    =lfvSource()
  CASE lnMovmnts = 2
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llMV1By1
      FOR lnI = 1 TO ALEN(laSource,1)
        lsSource = lnI 
        =lfvSource()
      ENDFOR
    ELSE  
      IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
        IF !&lcVldFunc.(lnMovmnts)
          RETURN
        ENDIF  
      ENDIF
      DECLARE laTarget[ALEN('laSource',1)]
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laTarget[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
        laSource[lnCounter] = IIF(OCCURS("\",laSource[lnCounter])>0,"","\") + laSource[lnCounter]
      ENDFOR
      SHOW GETS
    ENDIF
  CASE lnMovmnts = 3
    =lfvTarget()
  CASE lnMovmnts = 4
    IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc) .AND. llReMV1By1
      lnLoop    = 1
      DO WHILE lnLoop <= ALEN(laTarget) AND ;
               ! (ALEN(laTarget,1) = 1 AND EMPTY(laTarget[1]))   
        lsTarget = lnLoop 
        =lfvTarget()
      ENDDO 				&& Ending of DO loop
      lnLoop    = 0
    ELSE
      *IF TYPE('lcVldFunc') = 'C' .AND. !EMPTY(lcVldFunc)
      *  IF !&lcVldFunc.(lnMovmnts)
      *    RETURN
      *  ENDIF  
      *ENDIF
      FOR lnCounter = 1 TO ALEN(laSource,1)
        lnPos = IIF(OCCURS("\",laSource[lnCounter])>0,2,1)
        laSource[lnCounter] = SUBSTR(laSource[lnCounter],lnPos,LEN(laSource[lnCounter]))
      ENDFOR
      DECLARE laTarget[1]
      laTarget =' '
      SHOW GETS
    ENDIF
ENDCASE
*-- End of lfvMovmnts.

*!*******************************************************************************
*! Name      : lfvCancel
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Reinitialize the source & target array.
*!*******************************************************************************
*! Example   : =lfvCancel()
*!*******************************************************************************
*
FUNCTION lfvCancel
DECLARE laTarget[lnOldDim]
=ACOPY(laOldTarg,laTarget)
DECLARE laSource[lnOldSour]
=ACOPY(laOldSour,laSource)
*-- End of lfvCancel.

*!*******************************************************************************
*! Name      : lfCancel
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid function for Cancel Push Button.
*!*******************************************************************************
*! Example   : =lfCancel()
*!*******************************************************************************
*
FUNCTION lfCancel
=lfvCancel()
CLEAR READ
*-- End of lfCancel.

*!*******************************************************************************
*! Name      : lpEscap
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 04/23/2001
*! Purpose   : Valid function when pressing escape.
*!*******************************************************************************
*! Example   : =lpEscap()
*!*******************************************************************************
*
FUNCTION lpEscap
=gfUpDate()
DO gfEscap
*-- End of lpEscap.
