*!*************************************************************
*! Program File : MADOR30.PRG
*! Program Desc : Custom Fabric Screen (For Dorby)
*! For Screen   : MADOR30.SPR
*!       System : Aria Apparel Series(A27) 
*!       Module : MATERIAL Module
*! Developer    : IHB
*!*************************************************************
*! Calls        :
*!        Procedures : lpShow 
*!        Functions  : lfActFolder(), lfFillFold(), lfvData1(),
*!                     lfvData2(), lfvkColor(), lfvVendor(),
*!                     lfwVendor(), lfvkVendor(), lfInit(),
*!                     lfvWare(), lfvWCancel(), lfvWarOk(),
*!                     lfwWareBrs(), lfKeyTrap(), lfWarActiv(),
*!                     lfWarDeact(), lfActBrow(), lfvGLWareH(),
*!                     lfvGLk(), lfwField(), lfvField()
*!*************************************************************
*! Passed :
*!        Parameters : NONE
*!*************************************************************
*! Example :
*!        MADOR30
*!*************************************************************
*! Modifications : -
*! C101485,1 IHB 05/17/1999
*! B802581,1 SSH 07/09/1999 1-Display POSHDR.Available Insted of Complete date
*! B802581,1 SSH            2-Make Customer field in the detail folder not
*! B802581,1 SSH              editable in case of style PO. 
*! B802734,1 SSH 28/10/1999 Increase Balance field in detail screen by 1
*! B802885,1 SSH 17/12/99   Save comments by PO/ITEM/COLOR not Po.
*:*************************************************************
EXTERNAL ARRAY laData, laKeyField, laScrMode, laDefProc
*-- lafoldwinds array that hold the count of used folders and it's titles 
*-- lcfolder    variable that hold the temp name of the folder window
*-- lcfoldprnt  variable that hold the name of the parent window of the folder
*               window which is the base screen 
*-- lnLastFold  variable that hold the last activated folder befor changing
*               the folder
*-- lnActFolder variable that hold the number of activated folder
*-- laKeyField will be equal the no. of get fields
*-- we have 5 get fields (pattern ,trans#, color, location and vendor)
*-- the user can select whatever he wants then generate records according
*-- to the selected criteria
*-- laDefProc is used to set program to save locally
*-- for example for local save we set laDefProc[9]=.F. that make the program
*-- executes local procedure for saving (lpSavScr)

*-- lcWinCh7   : window name for the header screen
*-- lcWinCh3   : base window name of the detail folder
*-- lcWinC31   : browse window name of the detail folder
*-- lcWinC32   : edit region window name of the detail folder
*-- lcWinCh4   : base window name of the summary folder
*-- lcWinCC1   : browse window name of the summary folder
*-- lcDetTemp  : detail temp. file
*-- lcTempSum  : summary temp. file
*-- lcCursName : cursor name of file that contain distinct
*--              and non empty(pattern) from FABRIC
*-- lcTempV    : temp. vendor file
*-- lnBalance,lnAdded,lnPulled : are computed figures
DIMENSION lafoldwinds[2,2]   && we have two folders : header and detail
DECLARE ladata[3], laKeyField[5,4], laDefProc[10]
STORE .T. TO laDefProc  && no local default procedure is used
STORE ''  TO lcFolder, laData, laKeyField, laFoldWinds
STORE ''  TO lcClrDesc, lcOldVen, lcVenName, lcModal, lcWinCh3, lcWinCh7, ;
             lcWinCh4, lcWinC31, lcWinC32, lcWinCC1, lcOldLink, lcOldData, ;
             lcDetTemp, lcTempSum, lcCursName, lcTempV, lcSFields
STORE 0   TO lnBalance,lnAdded,lnPulled
STORE .F. TO llUpdatWar
lcWare  = 'All'   && warehouse popup initial value
*-- llCalled is .f. since there's no calling to that screen
STORE .F. TO llCalled
lcVisClr    = RGBSCHEME(1)
lcfolder    = SYS(2015)        && Folder Window Name
lcfoldprnt  = gcBaseWind       && window parent name for the folder
lcwfoldchng = '=lfActFolder()' && function to control shows after change the folder
lcfoldpush  = 'pbFolders'      && push button name for the next folder
*-- lnFolderCEnd is the end col. of the folder
*-- lnFolderREnd is the end row of the folder
*-- lnNoFld is the no. of folders
*-- lcwfoldchng stores the name of a function to control shows after change the folder 
lnFolderCEnd = 103.00
lnFolderREnd = 2.00

IF !gfSetup()
  RETURN
ENDIF  

*-- Retsore some global setting from the setup file 
*-- and load them to local variables 
DIMENSION laSetUp[6,2]
laSetUp[1,1] = 'M_WareHouse'
laSetUp[2,1] = 'M_cIType1'
laSetUp[3,1] = 'M_cIType2'
laSetUp[4,1] = 'M_cIType3'
laSetUp[5,1] = 'M_cIType4'
laSetUp[6,1] = 'M_cIType5'
=gfGetMemVar(@laSetUp)
llMultiWH  = ALLTRIM(laSetUp[1,2])= 'Y'
llApIstall = (OCCURS('AP',gcCmpModules)<>0)
llMFIstall = (OCCURS('MF',gcCmpModules)<>0)
DIMENSION laSetUp[9,2]
laSetUp[1,1] = 'M_LINK_GL'
laSetUp[2,1] = 'M_MATDYE'
laSetUp[3,1] = 'M_TRKROLLS'
laSetUp[4,1] = 'M_WareLoc'
laSetUp[5,1] = 'M_cTSLbl1'
laSetUp[6,1] = 'M_cTSLbl2'
laSetUp[7,1] = 'M_cTSLbl3'
laSetUp[8,1] = 'M_cTSLbl4'
laSetUp[9,1] = 'llMulCurr'
=gfGetMemVar(@laSetUp)
llGl_Link  = ALLTRIM(laSetUp[1,2]) = 'Y' 
llMultCur  = laSetUp[9,2]

laKeyField[1,1] = 'laData[1]'  && pattern
laKeyField[1,2] = .F.
laKeyField[1,3] = 'PATTERN'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'  && color
laKeyField[2,2] = .T.
laKeyField[2,3] = 'COLOR'
laKeyField[2,4] = 2
lcScFields   = 'Pattern    , Color      , Vendor    '
llProceed    = .T.
lcOldField   = SPACE(1)
lcUnMark     = SPACE(1)
lcMark       = ">"
lcWare_Ttl   = "Location"
lnNomOfFo    = 2
DIMENSION laFoldWinds[lnNomOfFo,2]
llNothing    = lfFillFold()
lcSWinTtl    = 'Detail Information' 
lcClrTtl     = 'Summary Information'
llNoShow     = .F.
STORE SPACE(01) TO lcoldFold
*-- if the system is linked to GL
IF llGl_Link
  =gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK1','SH')
  LOCATE
  IF !FOUND()
    *-- There is no General Ledger link codes have been setup
    =gfDialog("I","No General Ledger link codes have been setup ...! Cannot proceed...")
    llProceed = .F.
  ENDIF
ENDIF
*-- setting up the relation(s)
IF !USED('POFHDR')
  =gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH') &&cmattype+pomat
ENDIF
SELECT POFHDR
SET ORDER TO TAG POFHDR
IF !USED('POFLN')
  =gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFLN','SH') &&cmattype+pomat+fabric+color+trancd
ENDIF
SELECT POFLN
SET ORDER TO TAG POFLN
SELECT POFHDR
SET RELATION TO Pofhdr.cmattype + Pofhdr.pomat INTO Pofln ADDITIVE
SELECT APVENDOR
SET ORDER TO TAG VENCODE  && cvendcode
SELECT POFHDR
SET RELATION TO Pofhdr.vendor INTO Apvendor ADDITIVE
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH') &&cmattype+pomat
ENDIF
SELECT WAREHOUS
SET ORDER TO TAG WAREHOUS
GOTO TOP
IF EOF()
  =gfModalGen('QRM42080B42001','DIALOG','Locations file is empty.')
  RETURN
ELSE
  LOCATE FOR LMATINV
  IF !FOUND()
    =gfModalGen('QRM42080B42001','DIALOG','No Material Inventory Locations Found')
    RETURN
  ENDIF
ENDIF
SELECT CODES
SET ORDER TO TAG CCODE_NO
IF !SEEK('N' + PADR('COLOR',6))
  *--No colors found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','No colors found')
  glQuitting = .T.
  RETURN
ENDIF
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvObjLnk()]

IF !WEXIST(gcBaseWind)
  lcDetTemp  = gfTempName()
  lcTempSum  = gfTempName()
  lcCursName = gfTempName()
  lcTempV    = gfTempName()
  lcWinCh7   = gfTempName()
  lcWinCh4   = gfTempName()
  lcWinCC1   = gfTempName()
  lcWinCh3   = gfTempName()
  lcWinC31   = gfTempName()
  lcWinC32   = gfTempName()
  lcfolder   = gfTempName()
  *-- creating temp summary folder file structure
  lnNewFld = 1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Balance'
  laFileStru[lnNewFld,2] = 'N'
*! B802734,1 SSH 28/10/1999 Increase Balance field in detail screen by 1
  *laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,3] = 7
*! B802734,1 SSH(END)
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1

  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TotAdded'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TotPulled'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Cost'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 9
  laFileStru[lnNewFld,4] = 3
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Vendor'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 8
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'WareW'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Pattern'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Item'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 7
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'ClrDesc'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 30
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Color'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Width'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Content'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 60
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Desc'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 20
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Fabric'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 7
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TransNo'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Nfave_cost'  && ave. cost/unit
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 3
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Idate'       && introduced date 
  laFileStru[lnNewFld,2] = 'D'
  laFileStru[lnNewFld,3] = 8
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'NAdded'      && added , computed value
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'NPulled'     && pulled , computed value
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  
  CREATE TABLE  (gcWorkDir+lcTempSum) FROM ARRAY laFileStru
  *INDEX ON  COLOR TAG COLOR OF (gcWorkDir+lcTempSum)

  SELECT FABDYE
  SET ORDER TO TAG Fabdye
  
  *-- creating temp detail folder file structure
  lnNewFld = 1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TransNo'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Status'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 20
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cDate'
  laFileStru[lnNewFld,2] = 'D'
  laFileStru[lnNewFld,3] = 8
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Added'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 11
  laFileStru[lnNewFld,4] = 2
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Pulled'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 11
  laFileStru[lnNewFld,4] = 2
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Cost'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 13
  laFileStru[lnNewFld,4] = 2
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Customer'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 30
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'StyComm'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 30
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Units'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 7
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TrType'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'WareW'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Item'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 7
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Color'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cStatus'  && to indicate whether the record is modified or not 
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  CREATE CURSOR (lcDetTemp) FROM ARRAY laFileStru
  
  *-- collecting distinct patterns from fabric to allow browse pattern(s)
  lnNewFld = 1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Pattern'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 0
  CREATE TABLE (gcWorkDir+lcCursName) FROM ARRAY laFileStru
  SELECT DISTINCT Pattern ;
         FROM FABRIC ;
         WHERE !EMPTY(Pattern) ;
         INTO DBF (gcWorkDir+lcCursName)
  INDEX ON Pattern TAG (lcCursName) OF (gcWorkDir+lcCursName)

  *-- collecting distinct vendors from fabric and apvendor
  *-- to allow browse vendor in case of entered pattern
  lnNewFld = 1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Vendor'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 8
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cVenComp'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 30
  laFileStru[lnNewFld,4] = 0
  CREATE TABLE (gcWorkDir+lcTempV) FROM ARRAY laFileStru
  
  *SELECT DISTINCT a.Vendor , b.cVenComp FROM FABRIC a , APVENDOR b  INTO DBF (gcWorkDir+lcCursName) ,;
  *WHERE a.Vendor = b.Vendor AND a.Pattern = laData[1]
  *INDEX ON Vendor TAG (lcTempV) OF (gcWorkDir+lcTempV)


  SELECT(lcBaseFile)  && FABRIC file
  SET ORDER TO TAG Pattern
  SELECT (lcCursName)
  SET RELATION TO Pattern INTO FABRIC ADDITIVE
  
  SELECT FABRIC
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK  && ihb 1
ENDIF
SELECT(lcBaseFile)
PUSH KEY
llClrFound = .F.  && indicate that color is found used in color validation
*-- initializing header screen values
STORE '' TO lcClrStr, lcTransStr, lcPatStr, lcItem, lcDescr, lcContent, lcWidth
STORE 0  TO lnCostuse, lnAvCost
STORE {} TO ldDate
llPatChg   = .F.  && indicate that the pattern field is changed
*-- setting relation(s)
SELECT (lcTempSum)
SET RELATION TO
SET RELATION TO Vendor INTO Apvendor ADDITIVE
*-- initializing warehouse popup
SELECT WAREHOUS
SELECT cWareCode FROM WAREHOUS   ;
    WHERE lMatInv         ;
    INTO ARRAY laWare     ;
    ORDER BY cWareCode
lnCount = _TALLY
lcAll   = 'All'
DIMENSION laWare[lnCount+1]
=AINS(laWare, lnCount+1)
laWare[lnCount+1] = lcAll
SHOW GET lcWare
DECLARE laScDum[4]       && dummy array used to keep lascrmode values
STORE .F. TO laScDum[4]
*-- initializing object state, button state and key state
lcObjStat  = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE")
lcButStat  = IIF(laScrMode[1] ,"DISABLE","ENABLE")
lcKeyStat  = IIF(laScrMode[1] ,"ENABLE" ,"DISABLE")

*-- allow ESC whenever in select mode
ON KEY LABEL ESC llDummy = lfEscp()

*-- Calling the screen
DO (gcScrDir+gcWinAppl+'\MADOR30.SPX')

*-- remove any previously initialized trap key
ON KEY LABEL ESC

*-- Release the browse window
RELEASE WINDOWS (lcSWinTtl)
RELEASE WINDOWS (lcClrTtl)
*-- Reset filters and relations , for usage(s) of dbfs. elsewhere .
*
*-- Release defined popups and pads (if exist)
POP KEY
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE PAD _Option OF _MSYSMENU
*-- Close and erase temporary files if quitting
IF glQuitting
  SELECT (lcDetTemp)
  USE
  ERASE (gcWorkDir+lcDetTemp+'.DBF')
  ERASE (gcWorkDir+lcDetTemp+'.CDX') 
  SELECT (lcTempSum)
  SET RELATION TO
  USE
  ERASE (gcWorkDir+lcTempSum+'.DBF')
  ERASE (gcWorkDir+lcTempSum+'.CDX')
  SELECT (lcCursName)
  SET RELATION TO
  USE
  ERASE (gcWorkDir+lcCursName+'.DBF')
  ERASE (gcWorkDir+lcCursName+'.CDX')
  SELECT (lcTempV)
  SET RELATION TO
  USE
  ERASE (gcWorkDir+lcTempV+'.DBF')
  ERASE (gcWorkDir+lcTempV+'.CDX')
ENDIF
*-- end of program code...
*--------------------- Functions' And Procedures' Section -----
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfActFolder
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Activate the selected folder
*!*************************************************************
*! Calls     : lfActClr , lfStyBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActFolder
PRIVATE lnAlias
lnAlias = SELECT()
*! B802885,1 SSH 17/12/99   Save comments by PO/ITEM/COLOR not Po.
STORE '' TO lcCustomer , lcStyComm
*! B802885,1 SSH (END)
DO CASE
  CASE lnactfolder = 1
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY   	  	  	
    =lfActClr()
    SELECT(lnAlias)
    SHOW GET ibToColor ENABLE
    SHOW GET ibTabs ENABLE
  CASE lnactfolder = 2
  	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
  	SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY  
  	SHOW GETS WINDOW (lcWinCC1) DISABLE ONLY   	  	  	
    SHOW WINDOW (lcWinCh4) TOP
    *-- was fn. called to zap and recollct lcdettemp (if needed)
    =lfStyBrow()
    SHOW GET ibToSty ENABLE
    SHOW GET ibDet ENABLE
    *! B802581,1 SSH Make Customer field in the detail folder not
    *! B802581,1 SSH editable in case of style PO. 
    *IF laScrMode[4] AND &lcDetTemp..TrType $ 'MSA'
    IF laScrMode[4] AND &lcDetTemp..TrType $ 'MA'
      SHOW GET lcCustomer ENABLE
    ENDIF  
    *! B802581,1 SSH(END).
    IF laScrMode[4] AND  &lcDetTemp..TrType = 'M'
      SHOW GET lcStyComm ENABLE
    ENDIF  
ENDCASE    

*!*************************************************************
*! Name      : lfFillFold
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : fill the array with screen names of folders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFillFold
laFoldWinds[1,1] = "Summary"
laFoldWinds[1,2] = "lcWinCh4"
laFoldWinds[2,1] = "Details"
laFoldWinds[2,2] = "lcWinCh3"

*!*************************************************************
*! Name      : lpShow
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Show the objects due the screen mode
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpShow
DO CASE
  CASE laScrMode[1]    && Select Mode, all keys are enabled and temp is zapped
    SELECT(lcTempSum)
    ZAP
    SELECT (lcDetTemp)
    ZAP
    SELECT(lcBaseFile)
    =lfInit(.F.)
    STORE '' TO lcCustomer , lcStyComm
    SHOW GET lcCustomer DISABLE
    SHOW GET lcStyComm DISABLE
    SHOW GET laData[1]   ENABLE  && pattern
    SHOW GET ibPattern   ENABLE  && pb pattern
    SHOW GET laData[2]   ENABLE  && color
    SHOW GET ibColor     ENABLE  && pb color
    SHOW GET pbWare      ENABLE  && pb warehouse, enabled if no selection
    SHOW GET laData[3]   ENABLE  && vendor
    SHOW GET ibVendor    ENABLE  && pb vendor
    SHOW GET ibFolders   DISABLE
    SHOW GET lcWare      ENABLE  && popup warehouses
    SHOW GET ibTrans     ENABLE
    SHOW GET lcTrans     ENABLE
    _CUROBJ = OBJNUM(laData[1])

  CASE laScrMode[4]
    *-- Edit Mode , top screen is disabled
    *-- only customer and comment filed is to be edited 
    *-- in the detail folder in specific cases only,
    *-- and records bounded with summary folder
    SELECT (lcTempSum)
    IF RECCOUNT(lcTempSum) <> 0
      GO TOP
      laData[1] = Pattern
      ladata[2] = Color
      laData[3] = Vendor
      lcDescr   = Desc
      lcItem    = Item
      *lcTrans   =
      lcContent = Content
      lcWare    = WareW
      lcWidth   = Width
      lcClrDesc = ClrDesc
      lcVenname = APVENDOR.cVencomp
      lnCostuse = Cost
      lnAvcost  = nfave_cost
      *lnBalance =
      *lnAdded   =
      *lnPulled  =
      ldDate    = IDate
      IF !EMPTY(TransNo)
        lcTrans = TransNo
      ENDIF
      =lfRefresh()
    ENDIF
    *
    SHOW GET laData[1]   DISABLE  && pattern
    SHOW GET ibPattern   DISABLE  && pb pattern
    SHOW GET laData[2]   DISABLE  && color
    SHOW GET ibColor     DISABLE  && pb color
    SHOW GET pbWare      DISABLE  && pb warehouse
    SHOW GET laData[3]   DISABLE  && vendor
    SHOW GET ibVendor    DISABLE  && pb vendor
    SHOW GET ibFolders   DISABLE
    SHOW GET lcWare      DISABLE  && popup warehouses
    SHOW GET ibTrans     DISABLE
    SHOW GET lcTrans     DISABLE

   IF lnactfolder = 1
      SHOW GET ibTabs ENABLE
   ENDIF 
    *
  CASE lnactfolder = 2
   
ENDCASE  
=lfchngfolder(lnactfolder)

*!*************************************************************
*! Name      : lfvdata1
*! Developer : IHB
*! Date      : 05/11/1996
*! Purpose   : Validatin of the pattern field (laData[1])
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvdata1
PRIVATE lnOption, lnAlias, lnCurTag
lnAlias   = SELECT()
lnCurTag  = TAG()
lnOption  = 1

*-- use (lcCursName) to browse distinct values of pattern
*! B802734,1 SSH 28/10/1999 Set the correct order.
SELECT FABRIC
SET ORDER TO TAG PATTERN
*! B802734,1 SSH (END)
SELECT (lcCursName)
SET ORDER TO TAG (lcCursName)
IF MDOWN()
  RETURN
ENDIF  
IF lcPatStr  <> laData[1]
  llPatChg   = .T.
ELSE
  llPatChg   = .F.
ENDIF  
IF  !EMPTY(laData[1]) 
  IF SEEK(laData[1],"Fabric")
    _CUROBJ  = OBJNUM(lcTrans)
  ELSE  
    IF '?' $ ALLTRIM(laData[1]) OR !SEEK(laData[1],"Fabric")
      IF RECCOUNT(lcCursName) = 0 OR EMPTY(&lcCursName..Pattern)
        WAIT WINDOW "No Patterns Found !"
        STORE SPACE(10) TO laData[1]
        _CUROBJ = OBJNUM(laData[1])
        lnOption = 3  && means reenter
      ELSE
        lnOption = 2
      ENDIF
    *ELSE
    *  WAIT WINDOW "Pattern Does Not Exist !"
    *  STORE SPACE(10) TO laData[1]
    *  _CUROBJ = OBJNUM(laData[1])
    *  lnOption = 3  && means reenter
    ENDIF  
    DO CASE
      CASE lnOption = 2
         *lcBrFields = [Fabric.Pattern]
         lcBrFields = [&lcCursName..Pattern]
         *laData[1]= IIF(ARIABROW('','Patterns',gnBrFSRow1, gnBrFSCol1,;
                     gnBrFSRow2, gnBrFSCol2,'','','Pattern','ladata'),;
                     FABRIC.Pattern,SPACE(10))
         *-- prevent browsing empty values of pattern
         lcDumF      = 'lfDumFun()'
         laData[1]= IIF(ARIABROW([FOR EVAL(lcDumF)],'Patterns',gnBrFSRow1, gnBrFSCol1,;
                     gnBrFSRow2, gnBrFSCol2,'','','Pattern','ladata'),;
                     &lcCursName..Pattern,SPACE(10))
         
         IF !EMPTY(laData[1])        
           SHOW GET laData[1]  ENABLE
           _CUROBJ = OBJNUM(lcTrans)
         ELSE
           laData[1]  = SPACE(10)
           _CUROBJ = OBJNUM(laData[1])
           laScrMode[1] = .T.
           lnlastfold = lnactfolder
           lnactfolder  = 1
           =lfchngfolder(lnactfolder)
           =lfInit(.F.)
           SHOW GETS
        ENDIF        
      CASE lnOption = 3
          laScrmode[1]= .T.
          lnlastfold = lnactfolder
          =lfchngfolder(lnactfolder)
          = lfInit(.F.)
          SHOW GETS
          _CUROBJ = OBJNUM(laData[1])
      ENDCASE
  ENDIF 
ELSE
  laScrMode[1] = .T.
ENDIF
SHOW GET laData[1]  ENABLE
lcPatStr = laData[1]
SELECT (lnAlias)
SET ORDER TO TAG (lnCurTag)

*!*************************************************************
*! Name      : lfDumFun
*! Developer : IHB
*! Date      : 05/11/1999
*! Purpose   : Prevent empty pattern to be displayed
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfDumFun
RETURN IIF(EMPTY(Pattern),.F.,.T.)

*!*************************************************************
*! Name      : lfvkPat
*! Developer : IHB
*! Date      : 05/11/1999
*! Purpose   : Validation of the browse key for the pattern field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkPat
PARAMETER lcVariable
IF lcVariable = "laData[1]"
  laData[1]    = PADR("?",10)
  llNothing    = lfvdata1()
ENDIF  

*!*************************************************************
*! Name      : lfvData2
*! Developer : IHB
*! Date      : 05/12/1999
*! Purpose   : Validation color field (laData[2])
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvData2
PRIVATE lnAlias, lnCurTag
lnAlias   = SELECT()
lnCurTag  = TAG()
llMdown   = MDOWN()
IF llMdown
  RETURN
ENDIF
*-- check whether color found or not
=lfChkClr()

*
*if llClrFound = .F.
*  wait window '.f.'
*else
*  wait window '.t.'
*endif
*  

*IF llClrFound = .F. OR lcClrStr <> laData[2] OR (llPatChg AND !EMPTY(laData[1])) OR (lcTransStr <> lcTrans AND !EMPTY(lcTrans) AND !EMPTY(lcTransStr))
IF llClrFound = .F.
  IF !EMPTY(lcTrans)  && browse from POFLN
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    SELECT POFLN
    SET RELATION TO
    SET RELATION TO Pofln.fabric+ Pofln.color INTO Fabric ADDITIVE
    IF !EMPTY(laData[2])
      lcPattern  = laData[1]
      lcFunct    = 'lfClrEx2()'
      lcBrFields = [POFLN.Fabric,POFLN.Color,]     +;
                   [FABRIC.Desc:h="Description":19]
      laData[2]  = IIF(ARIABROW([FOR EVAL(lcFunct)],'FABRIC/Colors',gnBrFSRow1, gnBrFSCol1,;
                       gnBrFSRow2, gnBrFSCol2,'','','Color','ladata'),;
                       POFLN.Color,SPACE(6))
      lcClrStr  = laData[2]
      laData[1] = lcPattern
      SHOW GET laData[1] ENABLE
    ENDIF
  ELSE  && browse from FABRIC
    SELECT FABRIC
    SET ORDER TO TAG FABRIC &&fabric+color
    IF !EMPTY(laData[2])
      lcPattern  = laData[1]
      lcFunc     = 'lfClrEx()'
      lcBrFields = [FABRIC.Fabric, FABRIC.Color,]+;
                   [Desc:h="Description":19]
      laData[2]  = IIF(ARIABROW([FOR EVAL(lcFunc)],'FABRIC/Colors',gnBrFSRow1, gnBrFSCol1,;
                       gnBrFSRow2, gnBrFSCol2,'','','Color','ladata'),;
                       FABRIC.Color,SPACE(6))
      lcClrStr  = laData[2]
      laData[1] = lcPattern
      SHOW GET laData[1] ENABLE
    ENDIF  
  ENDIF
ENDIF
IF !EMPTY(laData[2])
  lcClrDesc  = gfCodDes(laData[2], 'COLOR')
  =lfRefresh()
  llClrFound = .T.
ELSE
  laData[2] = SPACE(6)
  lcClrDesc = SPACE(15)
  =lfRefresh()
  llClrFound = .F.
ENDIF
SHOW GET laData[2] ENABLE
SELECT (lnAlias)
SET ORDER TO TAG (lnCurTag)

*!*************************************************************
*! Name        : lfChkClr
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : set / reset color found flag
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfChkClr
*-- check whether entered color exist in POFLN or FABRIC
llClrFound = .T.
IF !EMPTY(laData[2])
  IF !EMPTY(lcTrans)
    *SELECT POFLN
    *LOCATE FOR Color = laData[2] AND IIF(!EMPTY(laData[1]),Pattern = laData[1],.T.);
    *      AND 'P'+lcTrans = 'P'+Pomat
    *IF FOUND()
    *  llClrFound = .T.
    *ELSE
    *  llClrFound = .F.    
    *ENDIF
    *-- optomized
    SELECT POFLN
    SEEK('P'+lcTrans)
    LOCATE REST WHILE cMatType+Pomat = 'P'+lcTrans FOR Color = laData[2] ;
                AND IIF(!EMPTY(laData[1]),Pattern = laData[1],.T.)
    llClrFound = FOUND()
    *
  ELSE
    *SELECT FABRIC
    *LOCATE FOR Color = laData[2] AND IIF(!EMPTY(laData[1]),Pattern = laData[1],.T.)
    *IF FOUND()
    *  llClrFound = .T.
    *ELSE
    *  llClrFound = .F.    
    *ENDIF
    *-- optomized
    SELECT FABRIC
    IF !EMPTY(laData[1])
      SET ORDER TO TAG Pattern
      SEEK(laData[1])
      LOCATE REST WHILE Pattern = laData[1] FOR Color = laData[2]
      llClrFound = FOUND()
    ELSE
      LOCATE FOR Color = laData[2]
      llClrFound = FOUND()
    ENDIF
    *
  ENDIF
ENDIF

*!*************************************************************
*! Name        : lfClrEx
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate existence of Color in FABRIC
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfClrEx
SELECT FABRIC
DO CASE
  CASE EMPTY(laData[1])
    RETURN .T.
  CASE !EMPTY(laData[1])
    IF FABRIC.Pattern = laData[1]
      RETURN .T.
    ELSE
      RETURN .F.
    ENDIF
ENDCASE


*!*************************************************************
*! Name        : lfClrEx2
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate existence of Color in POFLN
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfClrEx2
SELECT POFLN

DO CASE
  CASE EMPTY(laData[1]) AND EMPTY(lcTrans)
    RETURN .T.
  CASE EMPTY(laData[1]) AND !EMPTY(lcTrans)
    IF cMatType+POFLN.Pomat = 'P'+lcTrans ;
       AND POFLN.Trancd = '1'
      RETURN .T.
    ELSE
      RETURN .F.
    ENDIF   
  CASE !EMPTY(laData[1]) AND !EMPTY(lcTrans)
    IF cMatType+POFLN.Pomat = 'P'+lcTrans ;
       AND POFLN.Pattern = laData[1] ;
       AND POFLN.Trancd = '1'
      RETURN .T.
    ELSE
      RETURN .F.
    ENDIF   
ENDCASE

*!*************************************************************
*! Name      : lfvkColor
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Validation browse key color field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkColor
laData[2]   = PADR("?",6)
llNothing   = lfvData2()

*!*************************************************************
*! Name      : lfvVendor
*! Developer : IHB
*! Date      : 05/13/1999
*! Purpose   : Validation for the vendor field (laData[3])
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lnAlias, lnCurTag
lnAlias   = SELECT()
lnCurTag  = TAG()
*-- if trans# is not empty then vendor is known
*-- if pattern and / or color not empty then filter fabric 
*-- for these pattern and or color
*-- elsewise , browse from vendor file.
IF EMPTY(laData[1]) AND EMPTY(laData[2]) AND EMPTY(lcTrans)
  IF EMPTY(laData[3]) 
   lcVenName = " "
  ELSE  
    SELECT APVENDOR
    IF !SEEK (laData[3])
      lcVendor = laData[3]
      = gfApVnBrow (@lcVendor)
      IF !EMPTY(lcVendor)
        laData[3] = lcVendor
      ELSE
        laData[3] = lcOldVen  
      ENDIF  
      IF !EMPTY(laData[3])
        lcVenName  = cVenComp
      ELSE
        laData[3] = lcOldField
        SHOW GET laData[3]
      ENDIF  
    ELSE
      lcVenName = cVenComp
    ENDIF
  ENDIF
  =lfvField('laData[3]')
  =lfRefresh()
ELSE
  IF (!EMPTY(laData[1]) OR !EMPTY(laData[2])) AND (EMPTY(lctrans))
    
    *-- browse from fabric for entered pattern and or color
    SELECT FABRIC
    SET ORDER TO TAG FABRIC &&fabric+color
    SET RELATION TO
    SET RELATION TO FABRIC.vendor INTO Apvendor ADDITIVE
    IF !EMPTY(laData[3]) AND !SEEK(laData[3],'APVENDOR')
      lcPattern  = laData[1]
      lcColor    = laData[2]
      
      *-- browse distinct values from lctempv
      SELECT (lcTempV)
      ZAP
      SELECT DISTINCT a.Vendor , b.cVenComp ;
      FROM FABRIC a , APVENDOR b  INTO DBF (gcWorkDir+lcTempV) ;
      WHERE a.Vendor = b.cVendCode AND a.Pattern = laData[1]
      INDEX ON Vendor TAG (lcTempV) OF (gcWorkDir+lcTempV)
      SELECT (lcTempV)
      * 
      
      
      *lcFun      = 'lfPCEx()'  && Pattern Color exist
      *lcBrFields = [FABRIC.Vendor, APVENDOR.CVENCOMP:h="Name":30]
      
      lcBrFields = [&lcTempV..Vendor, &lcTempV..CVENCOMP:h="Name":30]
      
      
      *laData[3]  = IIF(ARIABROW([FOR EVAL(lcFun)],'VENDORS',gnBrFSRow1, gnBrFSCol1,;
                       gnBrFSRow2, gnBrFSCol2,'','','Vendor','ladata'),;
                       FABRIC.Vendor,SPACE(8))
      
      laData[3]  = IIF(ARIABROW('','VENDORS',gnBrFSRow1, gnBrFSCol1,;
                       gnBrFSRow2, gnBrFSCol2,'','','Vendor','ladata'),;
                       &lcTempV..Vendor,SPACE(8))
      
      laData[1] = lcPattern
      laData[2] = lcColor
      SHOW GET laData[1] ENABLE
      SHOW GET laData[2] ENABLE
    ELSE
      IF SEEK(laData[3],'APVENDOR')
        lcVenName = APVENDOR.cVenComp
        =lfvField('laData[3]')
        =lfRefresh()
      ENDIF
    ENDIF  
  ENDIF
  IF !EMPTY(laData[3]) AND SEEK(laData[3],'APVENDOR')
    lcVenName = APVENDOR.cVenComp
    =lfvField('laData[3]')
    =lfRefresh()
  ENDIF
ENDIF
*-- if vendor is deleted
IF EMPTY(laData[3]) 
  lcVenName = " "
  =lfRefresh()
ENDIF  
*

SELECT(lnAlias)
SET ORDER TO TAG (lnCurTag)

*!*************************************************************
*! Name        : lfPCEx
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate existence of Color in POFLN
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfPCEx
IF (!EMPTY(laData[1]) AND FABRIC.Pattern <> laData[1]) OR (!EMPTY(laData[2]) AND FABRIC.Color <> laData[2]) OR EMPTY(FABRIC.Vendor)
  RETURN .F.
ELSE
  RETURN .T.
ENDIF

*!*************************************************************
*! Name      : lfwVendor
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : when function for the vendor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwVendor
lcOldVen = laData[3]

*!*************************************************************
*! Name      : lfvkVendor
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Validation for the browse key vendor field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvkVendor
laData[3]  = PADR("?", 8)
lcOldData  = laData[3]
llNothing  = lfvVendor() 
_CUROBJ    = OBJNUM(laData[3])

*!*************************************************************
*! Name      : lfInit
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Initialize the variables form file or 
*!             with initial values
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
FUNCTION lfInit
PARAMETER llFromFile, lnAlias,lnOldTag
lnAlias = SELECT()
SELECT FABRIC
IF llFromFile

ELSE
  STORE '' TO laData[1], ladata[2], laData[3], lcDescr, lcItem, ;
              lcContent, lcWare, lcWidth, lcClrDesc, lcVenname, lcTrans
  STORE 0 TO  lnCostuse, lnAvcost, lnBalance, lnAdded, lnPulled
  STORE {} TO ldDate
  lcWare  = 'All'
  =lfRefresh()
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvWare
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : calls the screen to add new warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWare
PRIVATE lnAlias
lnAlias = SELECT()

*-- in the standard fabric screen we can view / add / remove the warehouse(s)
*-- for a specific fabric/color according to laScrMode. Here we need only
*-- to view all warehouses' information before generation, and if generate
*-- is pressed , then warehouse push button is disabled.
*-- to keep the original setting for lascrmode , we store them into lascdum
laScDum[1] = laScrMode[1]
laScDum[2] = laScrMode[2]
laScDum[3] = laScrMode[3]
laScDum[4] = laScrMode[4]
STORE .F. TO laScrMode
STORE .T. TO laScrMode[2]

*-- browse directly from warehouse file
SELECT WAREHOUS
GO TOP
llWarFirst = !EOF()
lcDispMode = IIF((laScrMode[3] OR laScrMode[4]) .AND. !EOF(),'ENABLE','DISABLE')
lnRecNo    = RECNO()
lcGL_Link  = GL_Link
lcFObject  = IIF(laScrMode[2],"PBOKWAR" , "PBINSWAR"  )
lcLObject  = IIF(laScrMode[2],"PBOKWAR" , "PBCLOSEWAR")
PUSH KEY
=gfClearKey()
ON KEY LABEL TAB        llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL RIGHTARROW llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL DNARROW    llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL BACKTAB    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL LEFTARROW  llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL UPARROW    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL CTRL+W     llDummi=1
ON KEY LABEL CTRL+Q     llDummi=1
DO (gcScrDir+gcWinAppl+'\mamatrd.SPX')
POP KEY
SET RELATION TO
laScrMode[1] = laScDum[1]
laScrMode[2] = laScDum[2]
laScrMode[3] = laScDum[3]
laScrMode[4] = laScDum[4]
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvWCancel
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : clear reads of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWCancel
ACTIVATE WINDOW (WOUTPUT())
CLEAR READ

*!*************************************************************
*! Name      : lfvWarOk
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : clear reads of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWarOk
ACTIVATE WINDOW (WOUTPUT())
CLEAR READ

*!*************************************************************
*! Name      : lfwWareBrs
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : browse for avialable warehouses
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwWareBrs
lnRecNo   = RECNO()
SHOW WINDOW (lcWare_Ttl) REFRESH
lcGL_Link = gl_link
=lfWRefresh()
SHOW GET lcGL_Link

*!*************************************************************
*! Name      : lfGo2Brows
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : go to browse function in mamatrd screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfGo2Brows
IF llWarFirst
  llWarFirst = .F.
  KEYBOARD "{BACKTAB}" CLEAR
ENDIF  

*!*************************************************************
*! Name      : lfKeyTrap
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : trap keys for the warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfKeyTrap
PARAMETERS lcDirection,lcFirstObj,lcLastObj
lcObjChek = IIF(lcDirection = "FORWORD",UPPER(lcLastObj) ,UPPER(lcFirstObj))
lcObjJump = IIF(lcDirection = "FORWORD",UPPER(lcFirstObj),UPPER(lcLastObj) )
lnFactor  = IIF(lcDirection = "FORWORD",1,-1)
IF WONTOP(WOUTPUT()) .AND. VARREAD()=lcObjChek
  ACTIVATE WINDOW (lcWare_Ttl)
ELSE
  _CUROBJ =IIF(WONTOP(WOUTPUT()),_CUROBJ+(1*lnFactor),OBJNUM(&lcObjJump))
  ACTIVATE WINDOW (WOUTPUT())
ENDIF  

*!*************************************************************
*! Name      : lfWarActiv
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : activate of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfWarActiv
ON KEY LABEL RIGHTARROW llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL DNARROW    llDummi=lfKeyTrap("FORWORD" ,lcFObject,lcLObject)
ON KEY LABEL LEFTARROW  llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL UPARROW    llDummi=lfKeyTrap("BACKWORD",lcFObject,lcLObject)
ON KEY LABEL ENTER
ON KEY LABEL Ctrl+ENTER
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfWarDeact
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : deactivate of warehouse screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfWarDeact
ON KEY LABEL RIGHTARROW 
ON KEY LABEL DNARROW    
ON KEY LABEL LEFTARROW  
ON KEY LABEL UPARROW    
ON KEY LABEL Ctrl+ENTER llDummi=lfvWarOk()
ON KEY LABEL ESC        llDummi=lfvWCancel()
RETURN .F.

*!*************************************************************
*! Name      : lfActBrow
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : browse the assigned warehouses for the selected
*!             fabric/color
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActBrow
PRIVATE lcBrowfields
*-- browse directly from warehouse file
SELECT WAREHOUS 
lcBrowfields = "dummi=IIF(RECNO()=lnRecNo,'>',' '):H='',cWareCode :H= 'Location'," +;
             "lcWareDesc=Warehous.cDesc:H= 'Description' " +;
             IIF(llGl_Link,',GL_Link:H="GL Link":R','')
BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwWareBrs()   ;
       TITLE lcWare_Ttl    ;
       WINDOW MAT100B1 IN WINDOW MAT100_B

*!*************************************************************
*! Name      : lfvGLWareH
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : validate the Gl_Link for the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION  lfvGLWareH
PRIVATE llDown
llDown = MDOWN()
IF llDown
  RETURN
ENDIF 
IF EMPTY(lcGL_Link)
    REPLACE GL_Link    WITH lcGL_Link ;
            cStatus    WITH IIF (cStatus='A','A','M')
ELSE
  =gfGLBrowse('04',@lcGL_Link)
  IF !EMPTY(lcGL_Link)
    REPLACE GL_Link WITH lcGL_Link ;
            cStatus WITH IIF (cStatus='A','A','M')
  ELSE
    lcGL_Link = lcOldLink
  ENDIF
ENDIF  
SHOW GET lcGL_Link
SHOW WINDOW (lcWare_Ttl) REFRESH

*!*************************************************************
*! Name      : lfvGLk
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : validate the Gl_Link for the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvGLk
lcGL_Link = PADR("?",3)
llNothing = lfvGLWareH()

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : browse the Detail Folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfStyBrow
PRIVATE lnAlias
lnAlias = SELECT()
SELECT &lcDetTemp

*GOTO TOP
lcSWinTtl    = 'Detail Information' 
lcBrowfilds  = [cMarker  :H=' ':W=.F.,]      +;
               [TransNo  :8 :H='Trans#',]    +;
               [Status   :10:H='Status',]    +;
               [cDate    :10:H='Date',]      +;
               [Added    :H='Added':P='999999',]     +;
               [Pulled   :H='Pulled':P='999999',]    +;
               [Cost     :H='Cost':P='99999999.99',]      +;
               [StyComm  :25:H='Style/com',] +;
               [Customer :25:H='Customer',]  +;
               [Units    :7 :H='Units']
BROWSE FIELDS &lcBrowfilds;
       LOCK 0            ;   
       SAVE              ;
       NOMENU            ;
       NODELETE          ;
       NOAPPEND          ;
       NOEDIT            ;
       NOWAIT            ;
       TITLE lcSWinTtl   ;
       WHEN lfwBrowse()  ;
       FOR WareW=&lcTempSum..WareW AND Item=&lcTempSum..Item AND Color=&lcTempSum..Color;
       WINDOW (lcWinC31) IN WINDOW (lcWinCh3)

IF lnActFolder = 2 
  SHOW WINDOW (lcWinCh3)    REFRESH  
  SHOW WINDOW (lcWinC32)    REFRESH  
  *SHOW GET lcCustomer DISABLE  && ihb 1
  *SHOW GET lcStyComm DISABLE  
ENDIF
SELECT(lnAlias)
=lfRefresh()

*!*************************************************************
*! Name      : lfActClr
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Browse Active Collected Records in the Summary
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActClr
PRIVATE lnAlias

lnAlias      = SELECT(0)
*! B802734,1 SSH 28/10/1999 Increase Balance field in detail screen by 1
*lcBrowfields = "cMarker  :H=' ':W=.F.,"           +;
             "Balance  :H ='Balance' :R:P='999999',"       +;
             "Cost     :H ='Cost'    :R:P='99999.99',"      +;
             "Vendor   :H ='Vendor'  :R:11 ,"     +;
             "WareW    :H ='Location':R:10,"      +;
             "Pattern  :H ='Pattern ':R:10,"      +;
             "Item     :H ='Item'    :R:9,"       +;
             "Color    :H ='Color'   :R:9,"       +;
             "Width    :H ='Width'   :R:6,"       +;
             "Content  :H ='Contents':R:60" 

lcBrowfields = "cMarker  :H=' ':W=.F.,"           +;
             "Balance  :H ='Balance' :R:P='9999999',"       +;
             "Cost     :H ='Cost'    :R:P='99999.99',"      +;
             "Vendor   :H ='Vendor'  :R:11 ,"     +;
             "WareW    :H ='Location':R:10,"      +;
             "Pattern  :H ='Pattern ':R:10,"      +;
             "Item     :H ='Item'    :R:9,"       +;
             "Color    :H ='Color'   :R:9,"       +;
             "Width    :H ='Width'   :R:6,"       +;
             "Content  :H ='Contents':R:60" 
*! B802734,1 SSH(END)
SELECT (lcTempSum)
*GOTO TOP
BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOEDIT   ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfChMarker() ;
       TITLE lcClrTtl    ;
       WINDOW (lcWinCC1) IN WINDOW (lcWinCh4)

*
*   VALID lfvSumBr();

*
       
IF lnActFolder = 1
  SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCC1)  DISABLE ONLY
ENDIF
lnAlias = SELECT()

*!*************************************************************
*! Name      : lfChMarker
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : mark the selected record in the summary folder
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfChMarker
IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
  *-- Refresh header block
  laData[1] = Pattern
  ladata[2] = Color
  laData[3] = Vendor
  lcDescr   = Desc
  lcItem    = Item
  *lcTrans   =
  lcContent = Content
  lcWare    = WareW
  lcWidth   = Width
  lcClrDesc = ClrDesc
  lcVenname = APVENDOR.cVencomp
  lnCostuse = Cost
  lnAvcost  = Nfave_cost
  lnBalance = Balance
  lnAdded   = TotAdded
  lnPulled  = TotPulled
  ldDate    = IDate
  IF !EMPTY(TransNo)
    lcTrans = TransNo
  ENDIF
  =lfRefresh()
  SHOW GET laData[1]   DISABLE  && pattern
  SHOW GET ibPattern   DISABLE  && pb pattern
  SHOW GET laData[2]   DISABLE  && color
  SHOW GET ibColor     DISABLE  && pb color
  SHOW GET pbWare      DISABLE   && pb warehouse
  SHOW GET laData[3]   DISABLE  && vendor
  SHOW GET ibVendor    DISABLE  && pb vendor
  SHOW GET ibFolders   DISABLE
  SHOW GET lcWare      DISABLE  && popup warehouses
  SHOW GET ibTrans     DISABLE
  SHOW GET lcTrans     DISABLE
ENDIF  

*!*************************************************************
*! Name      : lfwBrowse
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : When browse detail folder.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwBrowse

*-- matching edit region with browse record

IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
  *-- Refresh header block
  lcCustomer = Customer
  lcStyComm  = StyComm
  =lfRefresh()
  *-- enable or disable lcCustomer / lcStyComm
  *-- according to type of transaction.
  *! B802581,1 SSH Make Customer field in the detail folder not
  *! B802581,1 SSH editable in case of style PO. 
  *IF laScrMode[4] AND TrType $ 'MSA'
  IF laScrMode[4] AND TrType $ 'MA'
    SHOW GET lcCustomer  ENABLE
  ELSE
    SHOW GET lcCustomer  DISABLE
  ENDIF
  *! B802581,1 SSH(END)
  *-- enable style/comments in case of material po
  IF laScrMode[4] AND TrType = 'M'
    SHOW GET lcStyComm   ENABLE
  ELSE
    SHOW GET lcStyComm   DISABLE
  ENDIF  
ENDIF  
*glFromBrow = .T.


*!*************************************************************
*! Name      : lfTrapKeys
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Trap keys
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfTrapKeys
RETURN

IF WONTOP() = lcClrTtl .OR. WONTOP() = lcSWinTtl
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
  *-- make the same message appearing
  *ON KEY LABEL ESC       DO gfCPClose  && original
  *ON KEY LABEL ESC DO gfEscap
  ON KEY LABEL ESC
ENDIF  
*ON KEY LABEL CTRL+B DO lfActBr()

*!*************************************************************
*! Name      : lpTab
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Trap key "Tab"
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpTab

DO CASE
  CASE WONTOP(lcClrTtl) OR WONTOP(lcSWinTtl)
     IF lnActFolder = 2 AND &lcDetTemp..TrType $ 'MSA'
       ACTIVATE WINDOW (lcWinC32)
       _CUROBJ = OBJNUM(lcCustomer)
     ELSE
       ACTIVATE WINDOW (lcWinCh7)
       _CUROBJ = OBJNUM(pbGen)
     ENDIF
  CASE _CUROBJ = OBJNUM(IBFOLDER[1])
    ACTIVATE WINDOW (lcClrTtl)
  CASE _CUROBJ = OBJNUM(IBFOLDER[2])
    ACTIVATE WINDOW (lcSWinTtl)
  
  CASE _CUROBJ = OBJNUM(lcCustomer)
    IF &lcDetTemp..TrType = 'M'
       ACTIVATE WINDOW (lcWinC32)
      _CUROBJ = OBJNUM(lcStyComm)
    ELSE
       ACTIVATE WINDOW (lcWinCh7)
      _CUROBJ = OBJNUM(pbGen)
    ENDIF

  OTHERWISE  
    _CUROBJ = _CUROBJ + 1
ENDCASE
RETURN

*!*************************************************************
*! Name      : lpShiftTab
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Trap key "Shift+Tab"
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpShiftTab

DO CASE
  CASE WONTOP(lcClrTtl) OR WONTOP(lcSWinTtl)
     ACTIVATE WINDOW (lcFolder)    
    _CUROBJ = OBJNUM(IBFOLDER[lnActFolder])
  CASE _CUROBJ = OBJNUM(IBFOLDER[2])
    _CUROBJ = OBJNUM(PBCLO)
  CASE _CUROBJ = OBJNUM(PBGEN)
    IF lnActFolder = 2
      IF &lcDetTemp..TrType $ 'MSA'
         ACTIVATE WINDOW (lcWinC32)
        IF &lcDetTemp..TrType = 'M'
          _CUROBJ = OBJNUM(lcStyComm)
        ELSE
          _CUROBJ = OBJNUM(lcCustomer)
        ENDIF
      ELSE
        ACTIVATE WINDOW (lcSWinTtl)
      ENDIF
    ELSE
      ACTIVATE WINDOW (lcClrTtl)    
    ENDIF
  CASE _CUROBJ = OBJNUM(lcCustomer)
    ACTIVATE WINDOW (lcSWinTtl)
  OTHERWISE  
    _CUROBJ = _CUROBJ - 1
ENDCASE
RETURN

*!*************************************************************
*! Name      : lfClearTrap
*! Developer : IHB
*! Date      : 05/17/1999
*! Purpose   : Clear Trap.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfClearTrap
RETURN
IF WONTOP() <> lcClrTtl .AND. WONTOP() <> lcSWinTtl
   *ON KEY LABEL ESC &lcOldEscTrap  && original
   ON KEY LABEL ESC
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   
*ON KEY LABEL CTRL+B


*!*************************************************************
*! Name      : lfwField
*! Developer : IHB
*! Date      : 02/11/1997
*! Purpose   : called from the when of every get filed to get 
*!             the old value in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwField
lcOldData = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvField
*! Developer : IHB
*! Date      : 02/11/1997
*! Purpose   : called from the valid of every get filed the show 
*!             if there is any changes or not
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvField
PARAMETER lcField
*! B802734,1 SSH 28/10/1999 Fix inequqlity.
*IF &lcField) <> lcOldData
IF ALLTRIM(lcOldData) == ALLTRIM(&lcField)
ELSE
*! B802734,1 SSH(END)
  lcSFields = lcSFields +','+lcField
ENDIF

*!*************************************************************
*! Function  : lfvObjLnk
*! Developer : IHB
*! Date      : 02/11/1997
*! Purpose   : Object link ,Material picture.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvObjLnk
lnAlias=SELECT()
DO GetObj WITH 'M',laData[1]
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name        : lfvITrans
*! Developer   : IHB
*! Date        : 10/05/1999
*!*************************************************************
*! Purpose     : Validate the P/O field from the key field.
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvITrans
lcTrans     = PADR("?",6)
llNothing   = lfvTrans()

*!*************************************************************
*! Name        : lfvTrans
*! Developer   : IHB
*! Date        : 10/05/1999
*!*************************************************************
*! Purpose     : Validate the P/O field and if 
*!               found switch the vendor view mode.
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvTrans
PRIVATE lnAlias, lnCurTag
lnAlias   = SELECT()
lnCurTag  = TAG()
llMdown   = MDOWN()
IF llMdown
  RETURN
ENDIF

*-- moved above to initialize relations alltogether
*-- and one time only
*SELECT APVENDOR
*SET ORDER TO TAG VENCODE  && cvendcode
*SELECT POFHDR
*SET RELATION TO
*SET RELATION TO Pofhdr.vendor INTO Apvendor ADDITIVE

*
SELECT POFHDR
SET ORDER TO TAG POFHDR
SELECT POFLN
SET ORDER TO TAG POFLN
SELECT POFHDR
SET RELATION TO
*SET RELATION TO Pofhdr.cmattype + Pofhdr.pomat INTO Pofln ADDITIVE
SELECT APVENDOR
SET ORDER TO TAG VENCODE  && cvendcode
SELECT POFHDR
SET RELATION TO Pofhdr.vendor INTO Apvendor ADDITIVE
*
SELECT POFHDR
IF !EMPTY(lcTrans)
  IF !SEEK('P'+lcTrans,"POFHDR")
    lcPattern  = laData[1]
    lcTransStr = lcTrans
    *-- to prevent browse zero records
    IF lfPatChk()
      *-- 
      SELECT POFHDR
      GO TOP

      *
      lcFun      = 'lfPatExist()'
      lcBrFields = [POFHDR.Pomat]

  	  DIMENSION latemp[1]
   	  latemp = ''

      = ARIABROW([FOR lfPatExist()],'PO Numbers',gnBrFSRow1, gnBrFSCol1,;
                     gnBrFSRow2, gnBrFSCol2,'','','Pomat','laTemp')

      lcTrans = IIF(EMPTY(laTemp[1]),SPACE(6),laTemp[1])

    ELSE
      WAIT WINDOW "No PO Found For The Selected Pattern ! "
      lcTrans  = SPACE(6)
      _CUROBJ = OBJNUM(lcTrans)
    ENDIF
    
    IF !EMPTY(lcTrans)        
      _CUROBJ = OBJNUM(laData[2])
    ELSE
      lcTrans  = SPACE(6)
      _CUROBJ = OBJNUM(lcTrans)
    ENDIF        
    laData[1] = lcPattern
    SHOW GET lcPattern ENABLE
  ELSE  && chk. if entered pattern then the pomat must has that pattern
    IF !(lfPatExist())
      WAIT WINDOW "Transaction Doesn't Contain Entered Pattern ! "
      STORE SPACE(6) TO lcTrans
      _CUROBJ = OBJNUM(lcTrans)
      SHOW GET lcTrans ENABLE
    ELSE
        lcTransStr = lcTrans
    ENDIF
  ENDIF
ENDIF
*IF !EMPTY(lcTrans)
*  _CUROBJ = OBJNUM(laData[2])
*ELSE
*  lcTrans  = SPACE(6)
*  _CUROBJ = OBJNUM(lcTrans)
*  *laScrMode[1] = .T.
*  *lnlastfold = lnactfolder
*  *lnactfolder  = 1
*  *=lfchngfolder(lnactfolder)
*  *=lfInit(.F.)
*  *SHOW GETS
*ENDIF

IF !EMPTY(lcTrans)
  laData[3] = POFHDR.Vendor
  *IF !EMPTY(laData[3])
    lcVenName = APVENDOR.cVenComp
    =lfRefresh()
    SHOW GET laData[3] DISABLE
    SHOW GET ibVendor DISABLE
  *ELSE
  *  laData[3] = SPACE(8)
  *  lcVenName = ' '
  *  =lfRefresh()
  *  SHOW GET laData[3] ENABLE
  *ENDIF  
ELSE
*  laData[3] = SPACE(8)
*  lcVenName = ' '
*  =lfRefresh()
  SHOW GET laData[3] ENABLE
  SHOW GET ibVendor ENABLE
ENDIF

*-- disable the relation for the generate
SELECT POFHDR
SET RELATION TO
*
SHOW GET lcTrans ENABLE
SELECT (lnAlias)
SET ORDER TO TAG (lnCurTag)

*!*************************************************************
*! Name        : lfPatExist
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate existence of pattern in POFLN.
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfPatExist

llFound = .T.
IF !EMPTY(laData[1]) AND SEEK('P'+POFHDR.Pomat,'POFLN')
  SELECT POFLN
  LOCATE REST WHILE cMatType+Pomat = 'P'+POFHDR.Pomat FOR Pattern = laData[1]
  llFound = FOUND()
  SELECT POFHDR
ENDIF
RETURN llFound

*!*************************************************************
*! Name        : lfPatChk
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Prevent browse no records if selected pattern.
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfPatChk
IF !EMPTY(laData[1])
  SELECT POFLN
  LOCATE FOR Pattern = laData[1]
  IF !FOUND()
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*!*************************************************************
*! Name        : lfvGen
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate generate push button,
*!               Data collection.
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvGen
PRIVATE lnAlias, lnCurTag
lnAlias   = SELECT()
lnCurTag  = TAG()

*-- temp files are zapped above whenever cancel is pressed
*-- and the screen is returned to select mode
*SELECT (lcTempSum)  && ihb 1
*ZAP

IF laScrMode[1]
***

*-- if entered PO# then data collection will be from POFLN
*-- if no PO# Then collecting from FABRIC  
IF !EMPTY(lcTrans)
  *SELECT FABDYE
  *SET ORDER TO TAG FABDYE
  *-- all initial and nonchangeable relations must be moved above
  SELECT POFHDR
  SET RELATION TO
  SET ORDER TO TAG POFHDR && mattype+pomat
  SELECT FABRIC
  SET RELATION TO
  SET ORDER TO TAG FABRIC
  *SET RELATION TO
  *SET RELATION TO Fabric+Color INTO FABDYE ADDITIVE
  SELECT POFLN
  SET RELATION TO
  SET RELATION TO POFLN.Fabric+POFLN.Color INTO FABRIC ADDITIVE
  SET RELATION TO 'P'+Pomat into POFHDR ADDITIVE  && to obtain warehouse of trans.
  
  IF lcWare = 'All'
    *-- added condition trancd = '1' to avoid repeated values
    SCAN FOR IIF(!EMPTY(laData[1]),Pattern = laData[1] ,.T.) ;
                  AND IIF(!EMPTY(laData[2]),Color = laData[2],.T.) ;
                  AND Trancd = '1' ;
                  AND 'P'+Pomat = 'P'+lcTrans
       SELECT (lcTempSum)
       APPEND BLANK
       REPLACE Balance      WITH   0.0              ,;
               Cost         WITH  FABRIC.COSTUSE    ,;
               Nfave_cost   WITH  FABRIC.Nfave_cost ,;
               IDate        WITH  FABRIC.Introduced ,;
               Vendor       WITH  POFHDR.Vendor     ,;
               WareW        WITH  POFHDR.cWarecode  ,;
               Pattern      WITH  FABRIC.Pattern    ,;
               Item         WITH  FABRIC.Fabric     ,;
               Color        WITH  FABRIC.Color      ,;
               ClrDesc      WITH  gfCodDes(FABRIC.COLOR, 'COLOR'),; 
               Width        WITH  FABRIC.Width      ,;
               Desc         WITH  FABRIC.Desc       ,;
               TransNo      WITH  lcTrans           ,;
               Content      WITH  FABRIC.CONTENT
               
*               Vendor       WITH  FABRIC.Vendor     ,;
               
               
    ENDSCAN
  ELSE
    *-- added condition trancd = '1' to avoid repeated values
    SCAN FOR IIF(!EMPTY(laData[1]),Pattern = laData[1] ,.T.) ;
                  AND IIF(!EMPTY(laData[2]),Color = laData[2],.T.) ;
                  AND Trancd = '1' ;
                  AND 'P'+Pomat = 'P'+lcTrans
      IF 'P'+Pomat = 'P'+lcTrans AND POFHDR.cWarecode = lcWare
        SELECT (lcTempSum)
        APPEND BLANK
        REPLACE Balance      WITH   0.0              ,;
                Cost         WITH  FABRIC.COSTUSE    ,;
                Nfave_cost   WITH  FABRIC.Nfave_cost ,;
                IDate        WITH  FABRIC.Introduced ,;
                Vendor       WITH  POFHDR.Vendor     ,;
                WareW        WITH  POFHDR.cWarecode  ,;
                Pattern      WITH  FABRIC.Pattern    ,;
                Item         WITH  FABRIC.Fabric     ,;
                Color        WITH  FABRIC.Color      ,;
                ClrDesc      WITH  gfCodDes(FABRIC.COLOR, 'COLOR'),; 
                Width        WITH  FABRIC.Width      ,;
                Desc         WITH  FABRIC.Desc       ,;
                TransNo      WITH  lcTrans           ,;
                Content      WITH  FABRIC.CONTENT
                
*                Vendor       WITH  FABRIC.Vendor     ,;
                
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  SELECT FABRIC
  SET RELATION TO
  SET ORDER TO TAG FABRIC
  SET RELATION TO Fabric.fabric + Fabric.color INTO Fabdye ADDITIVE
  IF lcWare = 'All'
    SELECT FABRIC
    SCAN FOR IIF(!EMPTY(laData[1]),Pattern = laData[1] ,.T.) ;
                  AND IIF(!EMPTY(laData[2]),Color = laData[2],.T.) ;
                  AND IIF(!EMPTY(laData[3]),Vendor = laData[3],.T.)
       
      *-- scan fabdye while the same fabric/color for all warehouses
      SELECT FABDYE
      SCAN WHILE Fabric = FABRIC.Fabric AND Color = FABRIC.Color
        SELECT (lcTempSum)
        APPEND BLANK
        REPLACE Balance      WITH   0.0              ,;
                Cost         WITH  FABRIC.COSTUSE    ,;
                Nfave_cost   WITH  FABRIC.Nfave_cost ,;
                IDate        WITH  FABRIC.Introduced ,;
                Vendor       WITH  FABRIC.Vendor     ,;
                WareW        WITH  FABDYE.cWarecode  ,;
                Pattern      WITH  FABRIC.Pattern    ,;
                Item         WITH  FABRIC.Fabric     ,;
                Color        WITH  FABRIC.Color      ,;
                ClrDesc      WITH  gfCodDes(FABRIC.COLOR, 'COLOR'),; 
                Width        WITH  FABRIC.Width      ,;
                Desc         WITH  FABRIC.Desc       ,;
                Content      WITH  FABRIC.CONTENT
      ENDSCAN
    ENDSCAN
  ELSE
    SELECT FABRIC
    SCAN FOR IIF(!EMPTY(laData[1]),Pattern = laData[1] ,.T.) ;
                  AND IIF(!EMPTY(laData[2]),Color = laData[2],.T.) ;
                  AND IIF(!EMPTY(laData[3]),Vendor = laData[3],.T.)
      *-- scan fabdye while the same fabric/color for the same warehouse
      SELECT FABDYE
      SCAN WHILE Fabric = FABRIC.Fabric AND Color = FABRIC.Color ;
           FOR cWareCode = lcWare
        SELECT (lcTempSum)
        APPEND BLANK
        REPLACE Balance      WITH   0.0              ,;
                Cost         WITH  FABRIC.COSTUSE    ,;
                Nfave_cost   WITH  FABRIC.Nfave_cost ,;
                IDate        WITH  FABRIC.Introduced ,;
                Vendor       WITH  FABRIC.Vendor     ,;
                WareW        WITH  FABDYE.cWarecode  ,;
                Pattern      WITH  FABRIC.Pattern    ,;
                Item         WITH  FABRIC.Fabric     ,;
                Color        WITH  FABRIC.Color      ,;
                ClrDesc      WITH  gfCodDes(FABRIC.COLOR, 'COLOR'),; 
                Width        WITH  FABRIC.Width      ,;
                Desc         WITH  FABRIC.Desc       ,;
                Content      WITH  FABRIC.CONTENT

      ENDSCAN      
      *-- 
      *LOCATE FOR FABDYE.cwarecode = lcWare
      *IF FOUND()
      *  SELECT (lcTempSum)
      *  APPEND BLANK
      *  REPLACE Balance      WITH   0.0              ,;
                Cost         WITH  FABRIC.COSTUSE    ,;
                Nfave_cost   WITH  FABRIC.Nfave_cost ,;
                IDate        WITH  FABRIC.Introduced ,;
                Vendor       WITH  FABRIC.Vendor     ,;
                WareW        WITH  FABDYE.cWarecode  ,;
                Pattern      WITH  FABRIC.Pattern    ,;
                Item         WITH  FABRIC.Fabric     ,;
                Color        WITH  FABRIC.Color      ,;
                ClrDesc      WITH  gfCodDes(FABRIC.COLOR, 'COLOR'),; 
                Width        WITH  FABRIC.Width      ,;
                Desc         WITH  FABRIC.Desc       ,;
                Content      WITH  FABRIC.CONTENT
      *ENDIF
    ENDSCAN
  ENDIF
ENDIF
*-- scan through (lcTempSum) to collect detail


SELECT (lcTempSum)
IF !EOF()
  *lcItem  = Fabric
  *lcware  = WareW
  *lcColor = Color
  IF !USED('DFABRIC')
    =gfOpenFile(gcDataDir+'DFABRIC',gcDataDir+'DFABRIC','SH')
  ENDIF
  SELECT DFABRIC
  SET ORDER TO TAG Dfabric
  SELECT POFHDR
  SET RELATION TO
  SELECT POFLN
  SET RELATION TO
  SET RELATION TO 'P'+Pomat INTO POFHDR ADDITIVE
  SELECT POFHDR
  SET RELATION TO
  SET RELATION TO 'M'+Pomat INTO DFABRIC ADDITIVE
  IF !USED('CTKTBOM')
    =gfOpenFile(gcDataDir+'CTKTBOM',gcDataDir+'CTKTBOM','SH')
  ENDIF
  IF !USED('POSHDR')
    =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
  ENDIF
  IF !USED('POSLN')
    =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
  ENDIF
  SELECT POSLN
  SET RELATION TO
  SET ORDER TO TAG POSLN
  SELECT POSHDR
  SET ORDER TO TAG POSHDR
  SET RELATION TO
  SET RELATION TO 'P'+ Poshdr.po INTO Posln ADDITIVE
  SELECT CTKTBOM
  SET RELATION TO
  SET RELATION TO 'P'+ Ctktbom.cuttkt INTO Poshdr ADDITIVE
  SET RELATION TO 'S'+CutTkt INTO DFABRIC ADDITIVE
  IF !USED('FINVTADJ')
    =gfOpenFile(gcDataDir+'FINVTADJ',gcDataDir+'FINVTADJ','SH')
  ENDIF
  IF !USED('DFINVADJ')
    =gfOpenFile(gcDataDir+'DFINVADJ',gcDataDir+'DFINVADJ','SH')
  ENDIF
  SELECT DFINVADJ
  SET RELATION TO
  SET RELATION TO 'A'+CTrn_Seq INTO DFABRIC ADDITIVE
  SELECT FINVTADJ
  SET RELATION TO
  SET RELATION TO IIF(FINVTADJ.Type='P','P',IIF(FINVTADJ.Type='T','T',''))+CTrn_Seq INTO DFABRIC ADDITIVE
  SELECT (lcTempSum)
  SCAN
  
    SELECT POFLN
    SCAN FOR POFLN.Pattern = &lcTempSum..Pattern ;
             AND IIF(!EMPTY(lcTrans),'P'+POFHDR.Pomat='P'+&lcTempSum..TransNo ,.T.);
             AND POFLN.Color = &lcTempSum..Color ;
             AND POFHDR.cWarecode = &lcTempSum..WareW ;
             AND IIF(EMPTY(&lcTempSum..Vendor),.T.,POFHDR.Vendor = &lcTempSum..Vendor) ;
             AND POFLN.Fabric = &lcTempSum..Item ;
             AND POFLN.Trancd = '1'
             
        SELECT (lcDetTemp)
        APPEND BLANK
        *! B802885,1 SSH 17/12/99   Save comments by PO/ITEM/COLOR not Po.
        =SEEK('M'+POFHDR.Pomat+&lcTempSum..Item+&lcTempSum..Color,'DFABRIC')
        *! B802885,1 SSH (End)
        REPLACE TransNo  WITH POFHDR.Pomat     ,;
                Status   WITH IIF(POFHDR.Status = 'O','Open',IIF(POFHDR.Status = 'C','Complete',IIF(POFHDR.Status = 'B','Bid','Cancelled'))),;
                cDate    WITH POFHDR.Complete  ,;
                Cost     WITH POFLN.NCost1     ,;
                Customer WITH DFABRIC.Customer ,;
                StyComm  WITH DFABRIC.Comments ,;
                WareW    WITH &lcTempSum..WareW,;
                Item     WITH &lcTempSum..Item ,;
                Color    WITH &lcTempSum..Color,;
                TrType   WITH 'M'  && material po
        DO CASE
          CASE POFHDR.Status $ 'XB'
            REPLACE Added WITH 0
          CASE POFHDR.Status = 'O' AND POFLN.Trancd = '1'
            REPLACE Added WITH POFLN.nFabTotQty
          CASE POFHDR.Status = 'C'
            lnRec   = RECNO('POFLN')
            lcPomat = POFLN.Pomat
            SELECT POFLN
            SUM REST POFLN.NFabTotQty TO lnAdded FOR &lcTempSum..Item   = POFLN.Fabric ;
                        AND &lcTempSum..Color  = POFLN.Color ;
                        AND lcPomat = POFLN.Pomat ;
                        AND POFLN.cFabGrade = '1' ;
                        AND POFLN.Trancd = '2'
            SELECT POFLN
            GOTO lnRec
            SELECT (lcDetTemp)
            REPLACE Added WITH lnAdded
        ENDCASE        
        *-- accumulating totadded and totpulled in lctempsum
        SELECT (lcTempSum)
        REPLACE TotAdded   WITH TotAdded  + &lcDetTemp..Added ,;
                TotPulled  WITH TotPulled + &lcDetTemp..Pulled
    ENDSCAN         
    IF EMPTY(lcTrans)
      SELECT CTKTBOM
      
      SCAN FOR CTKTBOM.Iclr = &lcTempSum..Color ;
               AND CTKTBOM.cWarecode = &lcTempSum..WareW ;
               AND CTKTBOM.Item = &lcTempSum..Item
          SELECT (lcDetTemp)
          APPEND BLANK
          *! B802581,1 SSH 07/09/1999 Display POSHDR.Available Insted of Complete date.
          *! B802581,1 SSH            and replace customer with poshdr.buyer.
          *REPLACE TransNo  WITH CTKTBOM.Cuttkt  ,;
                  Status   WITH POSHDR.Buyer     ,; 
                  cDate    WITH POSHDR.Complete  ,;
                  Cost     WITH CTKTBOM.UntCost  ,;
                  Customer WITH DFABRIC.Customer ,;
                  StyComm  WITH POSLN.Style      ,;                   
                  WareW    WITH &lcTempSum..WareW,;
                  Item     WITH &lcTempSum..Item ,;
                  Color    WITH &lcTempSum..Color,;
                  TrType   WITH 'S'  && style po
          REPLACE TransNo  WITH CTKTBOM.Cuttkt   ,;
                  Status   WITH IIF(POSHDR.LREQED,'R',''),; 
                  cDate    WITH POSHDR.Available ,;
                  Cost     WITH CTKTBOM.UntCost  ,;
                  Customer WITH POSHDR.BUYER     ,;
                  cStatus  WITH 'M'              ,;
                  StyComm  WITH POSLN.Style      ,;                   
                  WareW    WITH &lcTempSum..WareW,;
                  Item     WITH &lcTempSum..Item ,;
                  Color    WITH &lcTempSum..Color,;
                  TrType   WITH 'S'  && style po
          *! B802581,1 SSH (END).
          IF POSHDR.Status = 'O'
            DO CASE
              CASE CTKTBOM.cCatgTyp $ 'FT'
                SELECT CTKTBOM
                lnRec = RECNO()
                lcCutTkt = CTKTBOM.CutTkt
                
                SUM REST CTKTBOM.Req_Qty TO lnPulled FOR CTKTBOM.Iclr = &lcTempSum..Color ;
                         AND CTKTBOM.Item  = &lcTempSum..Item ;
                         AND 'I'+CTKTBOM.CutTkt = 'I'+lcCutTkt ;
                         AND CTKTBOM.cCatgTyp $ 'FT'
                
                SELECT CTKTBOM
                GOTO lnRec
                SELECT (lcDetTemp)
                REPLACE Pulled WITH lnPulled
              OTHERWISE
                REPLACE Pulled WITH CTKTBOM.Req_Qty
            ENDCASE
          ENDIF
          
          DO CASE
            CASE POSHDR.Status $ 'OB' AND POSLN.Trancd = '1'
              REPLACE Units WITH POSLN.TotQty
            CASE POSHDR.Status = 'C'
              REPLACE Units WITH POSLN.TotQty
          ENDCASE
          *-- accumulating totadded and totpulled in lctempsum
          SELECT (lcTempSum)
          REPLACE TotAdded   WITH TotAdded  + &lcDetTemp..Added ,;
                  TotPulled  WITH TotPulled + &lcDetTemp..Pulled
          
      ENDSCAN
      SELECT FINVTADJ
      SCAN FOR &lcTempSum..Color  = FINVTADJ.Color ;
               AND IIF(FINVTADJ.Type = 'T',&lcTempSum..WareW=FINVTADJ.cFromware OR &lcTempSum..WareW  = FINVTADJ.cToware,&lcTempSum..WareW  = FINVTADJ.cFromware) ;
               AND &lcTempSum..Item   = FINVTADJ.Fabric    ;
               AND FINVTADJ.Type $ 'PT' 
          SELECT (lcDetTemp)
          APPEND BLANK
          REPLACE TransNo  WITH FINVTADJ.cTrn_Seq   ,;
                  cDate    WITH FINVTADJ.Date       ,;
                  Cost     WITH FINVTADJ.NFUnitCost ,;
                  WareW    WITH &lcTempSum..WareW,;
                  Item     WITH &lcTempSum..Item ,;
                  Customer WITH DFABRIC.Customer ,;
                  Color    WITH &lcTempSum..Color,;
                  TrType   WITH FINVTADJ.Type        && 'P' or 'T'
          DO CASE
            CASE FINVTADJ.Type = 'P'
              REPLACE Added  WITH IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)>0,(FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)
              REPLACE Pulled WITH IIF((FINVTADJ.OldQty - FINVTADJ.NmTotAdj)<0,(FINVTADJ.OldQty - FINVTADJ.NmTotAdj),0)
              REPLACE StyComm  WITH FINVTADJ.cReason
              
            CASE FINVTADJ.Type = 'T'
              REPLACE StyComm  WITH FINVTADJ.cReason
              IF &lcTempSum..WareW = FINVTADJ.cFromware
                *REPLACE Added  WITH -FINVTADJ.NmTotAdj
                REPLACE Added  WITH  0
                REPLACE Pulled WITH  FINVTADJ.NmTotAdj
              ELSE
                IF &lcTempSum..WareW = FINVTADJ.cToware
                  REPLACE Added  WITH  FINVTADJ.NmTotAdj
                  *REPLACE Pulled WITH -FINVTADJ.NmTotAdj
                  REPLACE Pulled WITH  0
                ENDIF
              ENDIF
          ENDCASE
          *-- accumulating totadded and totpulled in lctempsum
          SELECT (lcTempSum)
          REPLACE TotAdded   WITH TotAdded  + &lcDetTemp..Added ,;
                  TotPulled  WITH TotPulled + &lcDetTemp..Pulled
          
      ENDSCAN
      SELECT DFINVADJ
      SCAN FOR &lcTempSum..Color  = DFINVADJ.Color ;
               AND &lcTempSum..WareW  = DFINVADJ.cFromware ;
               AND &lcTempSum..Item   = DFINVADJ.Fabric    ;
               AND DFINVADJ.Type $ 'A' 
          SELECT (lcDetTemp)
          APPEND BLANK
          REPLACE TransNo  WITH DFINVADJ.cTrn_Seq   ,;
                  cDate    WITH DFINVADJ.Date       ,;
                  Cost     WITH DFINVADJ.NFUnitCost ,;
                  Customer WITH DFABRIC.Customer    ,;
                  StyComm  WITH DFINVADJ.cReason    ,;
                  WareW    WITH &lcTempSum..WareW,;
                  Item     WITH &lcTempSum..Item ,;
                  Color    WITH &lcTempSum..Color,;
                  TrType   WITH DFINVADJ.Type        && 'A'
          IF DFINVADJ.Type = 'A'
            REPLACE Added  WITH IIF(DFINVADJ.NmTotAdj > 0 ,DFINVADJ.NmTotAdj ,0)
            REPLACE Pulled WITH IIF(DFINVADJ.NmTotAdj < 0 ,DFINVADJ.NmTotAdj ,0)
          ENDIF
          *-- accumulating totadded and totpulled in lctempsum
          SELECT (lcTempSum)
          REPLACE TotAdded   WITH TotAdded  + &lcDetTemp..Added ,;
                  TotPulled  WITH TotPulled + &lcDetTemp..Pulled
          
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
*--
SELECT (lcTempSum)
REPLACE ALL Balance WITH (TotAdded-TotPulled)
*
SELECT (lcDetTemp)
*lcBrowfilds  = [TransNo:6:H='Trans#',]+;
               [Status:7:H='Status',]+;
               [cDate:10:H='Date',]+;
               [Added:15:H='Added',]+;
               [Pulled:15:H='Pulled',]+;
               [Cost:15:H='Cost',]+;
               [StyComm:30:H='Style/com',]+;
               [Customer:30:H='Customer',]+;
               [Units:7:H='Units']
SHOW GET pbClo,1 PROMPT 'C\<ancel' ENABLE
*SHOW GET pbGen,1 PROMPT 'S\<ave'   ENABLE
SHOW GET pbGen,1 PROMPT 'Sa\<ve'   ENABLE
STORE .F. TO laScrMode
STORE .T. TO laScrMode[4]
=lpShow()
=lfchngfolder(lnactfolder)
*TTTT
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
***
ELSE
  =lfvSave()
  *
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GET pbClo,1 PROMPT 'C\<lose'     ENABLE
  SHOW GET pbGen,1 PROMPT 'G\<enerate'  ENABLE
  =lpShow()
  *
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB

ENDIF

*-- disable all relations for another validation
IF USED('POFLN')
  SELECT POFLN
  SET RELATION TO
ENDIF
IF USED('FABRIC')
  SELECT FABRIC
  SET RELATION TO
ENDIF
IF USED('POFHDR')
  SELECT POFHDR
  SET RELATION TO
ENDIF
IF USED('POSLN')  
  SELECT POSLN
  SET RELATION TO
ENDIF  
IF USED('POSHDR')  
  SELECT POSHDR
  SET RELATION TO
ENDIF
IF USED('CTKTBOM')  
  SELECT CTKTBOM
  SET RELATION TO
ENDIF
IF USED('FINVTADJ')  
  SELECT FINVTADJ
  SET RELATION TO
ENDIF
IF USED('DFINVADJ')  
  SELECT DFINVADJ
  SET RELATION TO
ENDIF  
*

SELECT (lnAlias)
SET ORDER TO TAG (lnCurTag)

*!*************************************************************
*! Name        : lfvClose
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate close push button
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvClose
IF laScrMode[1]
  glQuitting = .T.
  CLEAR READ
  RETURN
ELSE  && edit mode and the prompt is cancel
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GET pbClo,1 PROMPT 'C\<lose'     ENABLE
  SHOW GET pbGen,1 PROMPT 'G\<enerate'  ENABLE
  =lpShow()
*tttt
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB

ENDIF  

*-- IHB 1
*-- called locally to avoid any conflict regarding control screen
*!*************************************************************
*! Name      : gfEscap
*! Developer : IHB
*! Date      : 12/05/1999
*! Purpose   : To trape the escap button under windows
*!*************************************************************
*! Calls     : 
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFCPSAVE()               (function  in ARIA3.PRG)
*!          Calls: GFCHCLOSE()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Names of base window
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfEscap
PARAMETERS lcWin2Close

lcWin2Close = IIF(TYPE('lcWin2Close')="C",lcWin2Close,WONTOP())
lcWindPar=WPARENT(WONTOP())
lcParWinds = ''
DO WHILE !EMPTY(lcWindPar)
  lcParWinds = lcParWinds+IIF(EMPTY(lcParWinds),'',',')+lcWindPar
  lcWindPar=WPARENT(lcWindPar)
ENDDO

DO CASE
*WPARENT(WONTOP()) = gcBaseWind
  

  
  CASE WONTOP(gcBaseWind) .OR. UPPER(gcBaseWind) $ lcParWinds .OR. WONTOP('gwcContrl1')
    DEACTIVATE POPUP ALL
    DO CASE
      CASE (laScrMode[3] OR laScrMode[4]) AND WREAD('gwcContrl1')
          IF ALIAS()<>lcBaseFile
            SELECT (lcBaseFile)
          ENDIF
          ACTIVATE WINDOW gwcContrl1
          _CUROBJ=OBJNUM(pbCls)
          KEYBOARD "{SPACEBAR}"
      *CASE (laScrMode[3] .OR. laScrMode[4]) .AND. llCUpDate
      *CASE laScrMode[3] .AND. llCUpDate
      CASE laScrMode[4]
          *** Do you want to save chanes befor closing this progrm
          *** Yes, No , Cancel
          lnOption = gfModalGen("QRM00140B00025","ALERT")
          DO CASE
            CASE lnOption = 1
             *IF gfCpSave()  && will use local save ihb 1
             *-- calling lfvsave to save all records of lcdettemp
             *-- that marked cstatus = 'M' (if any)
             IF lfvSave()
               glQuitting = .T.
               CLEAR READ                                    
             ENDIF  
            CASE lnOption = 2
              *
              *-- if WONTOP() is either summary or detail,
              *-- then send key to quit that window.
              IF WONTOP(lcSWinTtl) OR WONTOP(lcClrTtl)
                KEYBOARD "{BACKTAB}"
                KEYBOARD "{BACKTAB}"
              ENDIF  
              *
              glQuitting = .T.
              CLEAR READ
          ENDCASE

        CASE (laScrMode[4] OR laScrMode[2] OR laScrMode[1]) AND WREAD('gwcContrl1')
          IF ALIAS()<>lcBaseFile
            SELECT (lcBaseFile)
          ENDIF
          ACTIVATE WINDOW gwcContrl1          
          _CUROBJ=OBJNUM(pbCls)
          KEYBOARD "{SPACEBAR}"
        *CASE laScrMode[3] OR laScrMode[2] OR laScrMode[1]
        CASE laScrMode[1]
          *-- the following is escape without msg.
          *IF laScrMode[3]
          *  IF ALIAS()<>lcBaseFile
          *    SELECT (lcBaseFile)
          *  ENDIF
          *  =gfObj_Lock(.F.)
          *ENDIF  
          glQuitting = .T.   
          CLEAR READ
          IF .F.
          ***
          *-- the following is escape with msg.
          *** Do you want to save chanes befor closing this progrm
          *** Yes, No , Cancel
          lnOption = gfModalGen("QRM00140B00025","ALERT")
          DO CASE
            CASE lnOption = 1
             *IF gfCpSave()  && will use local save ihb 1
               glQuitting = .T.
               CLEAR READ                                    
             *ENDIF  
            CASE lnOption = 2
              *
              *-- if WONTOP() is either summary or detail,
              *-- then send key to quit that window.
              IF WONTOP(lcSWinTtl) OR WONTOP(lcClrTtl)
                KEYBOARD "{BACKTAB}"
                KEYBOARD "{BACKTAB}"
              ENDIF  
              *
              glQuitting = .T.
              CLEAR READ
          ENDCASE
          
          ENDIF
          *** 
          
          
          ***
      ENDCASE
  CASE SUBSTR(WONTOP(),1,2) = "CW" .OR. SUBSTR(WPARENT(WONTOP()),1,2) = "CW"
          *WAIT '5' WINDOW
    DEACTIVATE POPUP ALL
    =gfChClose(lcWin2Close)
ENDCASE

*!*************************************************************
*! Name        : lfvCust
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate Customer field in detail folder
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvCust

*IF &lcDetTemp..Customer <> lcCustomer
*! B802734,1 SSH 28/10/1999 Fix inequqlity.
*IF lcOldData <> lcCustomer
IF ALLTRIM(lcOldData) == ALLTRIM(lcCustomer)
ELSE
*! B802734,1 SSH (END)
  SELECT (lcDetTemp)
  REPLACE Customer WITH lcCustomer ,;
          cStatus WITH 'M'
  SHOW WINDOW (lcWinCC1) REFRESH SAME
  SHOW WINDOW (lcSWinTtl) REFRESH
ENDIF


*!*************************************************************
*! Name        : lfvStyCom
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Validate style comment field in detail folder
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvStyCom

*IF &lcDetTemp..StyComm <> lcStyComm

*! B802734,1 SSH 28/10/1999 Fix inequqlity.
*IF lcOldData <> lcStyComm
IF ALLTRIM(lcOldData) == ALLTRIM(lcStyComm)
ELSE
*! B802734,1 SSH (END)
  SELECT (lcDetTemp)
  REPLACE StyComm WITH lcStyComm ,;
          cStatus WITH 'M'
  SHOW WINDOW (lcWinCC1) REFRESH SAME
  SHOW WINDOW (lcSWinTtl) REFRESH
ENDIF


*!*************************************************************
*! Name        : lfvSave
*! Developer   : IHB
*! Date        : 12/05/1999
*!*************************************************************
*! Purpose     : Save customer and style/comments
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvSave
SELECT (lcDetTemp)
SCAN FOR cStatus = 'M'
  SELECT DFABRIC
  SET ORDER TO TAG Dfabric   &&ctrantype+ctran + cfabric + ccolor + cwarecode
  IF !SEEK(&lcDetTemp..TrType + &lcDetTemp..TransNo + ;
        &lcDetTemp..Item + &lcDetTemp..Color + ;
        &lcDetTemp..WareW)
     SELECT DFABRIC
     APPEND BLANK
     REPLACE ctrantype  WITH &lcDetTemp..TrType   ,;
             ctran      WITH &lcDetTemp..TransNo  ,;
             cfabric    WITH &lcDetTemp..Item     ,;
             ccolor     WITH &lcDetTemp..Color    ,;
             cwarecode  WITH &lcDetTemp..WareW    ,;
             ctrandate  WITH &lcDetTemp..cDate    ,;
             comments   WITH &lcDetTemp..StyComm  ,;
             customer   WITH &lcDetTemp..Customer
  ELSE
    REPLACE comments   WITH &lcDetTemp..StyComm  ,;
            customer   WITH &lcDetTemp..Customer
  ENDIF      
  *-- temp disable untill update file stru.
  *=gfAdd_Info()
  *
ENDSCAN


FUNCTION LFACTBR
*IF lnActFolder=1
*  ACTIVATE WINDOW (lcClrTtl)
*ELSE
*  ACTIVATE WINDOW (lcSWinTtl)
*ENDIF
  
  
*FUNCTION lfvSumBr
*IF WONTOP() = lcClrTtl
*  =gfStopBrow() 
*ENDIF


*!*************************************************************
*! Name        : lfEscp
*! Developer   : IHB
*! Date        : 31/05/1999
*!*************************************************************
*! Purpose     : exit screen whenever in select mode only
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfEscp
IF laScrMode[1]
  KEYBOARD '{ALT+L}'
ENDIF
RETURN .T.