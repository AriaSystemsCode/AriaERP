*:***************************************************************************
*: Name      : CnvMain.prg						
*: Developer : Timour Abdalla Khalil (TAK).
*: Date      : 09/10/98
*: Purpose   : Convert Data form Aria ver 2.6 into ver 2.7  .
*:***************************************************************
*: Called From  : Window (05). lfvNext()
*:***************************************************************
*B801914,1 TAK 01/25/99 Fixed wrong convert in FISHD and KEY_CHG.
*E300630,1 SSH 01/01/99 Add the ability to convert the custom DBF's too.
*B801919,1 TAK 01/28/99 Fix wrong update of def ware in single warehouse.
*B801933,1 TAK 02/01/99 Fix wrong update of style long discription.
*B801940,1 TAK 02/04/99 Dont clear the fisical calendar if no F.C. in 2.6.
*B801981,1 TAK 02/18/99 Fix some converting issues in CODES,BOM,INVHDR
*B801981,1              and FABRIC files (Fabric records in SYCNVLLN).
*B802012,1 TAK 03/02/99 Fixed the wrong updating POFHDR,POFLN due to 2.6 bug.
*B802021,1 TAK 03/04/99 Fixed bug in updating the cost for trancd='1' in POSLN,POSHDR
*B802021,1              Fix of does not convering currency proporly from SYCCURR file. 
*B802033,1 TAK 03/08/99 Fixed the wrong updating in MFGOPRHD also update the
*B802033,1              SYCNVLIB record to get from CUTTKTH insted of CONMGMT.
*B802040,1 TAK 03/10/99 Check if the style exist in style file when converting
*B802040,1              the style cost sheet, also check existing of C/T and P/O 
*B802040,1              when converting CTKTBOM file.
*B802040,1              Create MFGCODE for defaults costing elements for 
*B802040,1              imported styles in CODES file if no detail costing used.
*E500257,1 TAK 03/15/99 Added the ability to convert the Extended Size Scale Company.
*B802076,1 TAK 02/20/99 Changes due to new added and removed fields in new release.
*B802078,1 TAK 02/23/99 Fixed the files opens function to check the file module install.
*B802126,1 TAK 04/06/99 Changes due to a new added field LHASNOTES in the system 
*B802126,1              and make a coresponding changes in the convertion program.
*B802126,1              Added new field in CUTPICK called CTKTLINENO.
*B802126,1              Added new field CARPTYPE in ARHIST and CREDIT and remove from DEBIT.
*E500259,1 TAK 04/09/99 Added to convert an EDI files if module was installed.
*B802288,1 WAM 05/19/99 Change invoice status 'O' to 'C'.
*B802315,1 TAK 06/03/99 Added to update DESC1 field in OrdLine and InvLine files
*B802315,1              by the Desc1 in style if this field was empty.
*B802310,1 TAK 06/06/99 Fixed the wrong sequence group if not base on division.
*B802324,1 TAK 06/11/99 Added a new field for sales link code in OrdHdr.
*B802700,1  WA 10/18/99 Fix the wrong updating in MFGOPRHD also update the
*B802700,1              Fix the wrong updating in MFGOPRHD & MFGOPRDT .
*B802817,1 KHM 11/23/99 Fix the bug of not replacing cIMType in the
*B802817,1              MatInvJl. Fix the bug of variable cConUCode
*B802817,1              not found. Fix the bug of missing warehouse record
*B802817,1              in the fabdye file in case of singl warehouse.
*B802817,1              Fix the bug of creating one record in the bomline
*B802817,1              file when there are more than dyelot.
*E301337,1 ASH 12/07/99 Add 'MH' to the parameter send to rebalance program to rebalance Material On Hand.
*B802861,1 TAK 12/30/99 Fix of wrong updating in PACK_LIN table.
*B802901,1 TAK 01/03/00 Fix of wrong updating RetHdr.disc_amt filed (change in SYCNVLLN.dbf).
*B603300,1 TAK 12/24/99 Fix of wrong sequence update for Batch and Batch_Pymt fields.
*B603386,1 TAK 01/11/00 Optinal convert inventory journal files, or default it
*B603386,1              by physical inventory of the current stock.
*E301359,1 TAK 01/12/00 Added to convert a customer history.
*B802998,1 TAK 01/30/00 Don't hide releted field CADJACCT when converting codes.
*B603435,1 TAK 02/03/00 Don't zap ARCUSHST file , acumulate the figures only.
*B803222,1 TAK 04/27/00 Fix wrong updating the lineno and csize in BomLine file.
*B603674,1 KHM 06/04/00 Fix the bug of not replacing the related field 
*B603674,1              'CSLSGLLINK' in the codes file.
*B603674,1 TAK 06/04/00 Fix an empty link code conversion, put 'DEFDEF' instead.
*B803644,1 SSE 09/24/00 1) Convert the Add_Date Correctly for each record (Add record in Sycnvlln)
*B803644,1              2) Correct Spelling of word "Preparing" , it was written "Prepering"
*B803695,1 AMM 09/25/00 Fix some bugs in conversion of EDI files .
*B603926,1 SSE 09/27/00 Add a record in Codes file for each DIVISION CODE to hold U.C.C type (EUROPE/AMERICAN)
*B803706,1 NAD 10/03/00 Change default network income and outcome file name .    
*B803707,1 NAD 10/03/00 Field lJcpStDash is missing during the conversion of EDIACPRT .   
*E500382,1 AMH 10/24/00 Add more details to log file about the record number and field name for 
*E500382,1              numeric overflow error.
*B803770,1 AMH 10/31/00 Add new record to SYCNVLLN (EDINET CNETNAME).
*B803772,1 AMH 10/31/00 Converting Mapping Files for upper case of function lfSkuDesc.
*B803808,1 WAM 11/07/00 Update ship to duns number in partner setup file.
*B803819,1 NAD 11/08/00 Flag unsent EDI invoices upon converting them.          
*B803805,1 HS  11/08/00 Add new records in SYCNVLLN to convert PRODACTI 
*B803805,1 HS           Quantity fields to new bigger fields.
*B803810,1 HBG 11/14/00 Fix bug of Wrong line_no when converting Pack_lin
*B803833,1 TAK 11/15/00 Added to update the new ShipVia field in BOL_hdr file.
*C102076,1 SSH 01/22/01 Modify the program to convert Cathy Daniel custom DBF.
*B803981,1 SSH 01/24/01 Fix bug of data type mismatch when the PO module not 
*B803981,1 SSH 01/24/01 installed and converting CODES file.
*B604617,1 TAK 07/01/01 Added to check usage of the 26 file before open, and close it if used.
*B804309,1 AME 07/15/01 fix the bug of some inventory transactions get updated with wrong quantities
*B605511,1 ABD 02/25/2002 Fix error Insufficient memory error message in the conversion program
*B605511,1 ABD 02/25/2002 only when convert CTKTBOM file.
*B804459,1 TMI 03/11/2002 Add a line for each partner for 997
*B605949,1 KHM 05/29/2002 Fix the problem of replacing the nissued in the
*B605949,1                MatinvJl with -ve.
*B606691,1 SSE 11/26/2002 Fix bug of insufficient memory when converting BomLine.
*B607236,1 ABD 06/23/2003 Convert the pattern from the style file At26 to PDM pattern file at 27, in case SP install.
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char 
*:***************************************************************************

*--Initialize screen variables.
STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc,lcD3Thrm,lcD3ThPrc
STORE 0   TO lnMax1,lnMax2,lnMax3
STORE 1   TO lnCurNm1,lnCurNm2,lnCurNm3
DIME laModNum[1,2]
laModNum=''

*--Needed setups.

*--Company Setup.
llGlLnkStp = (gfGetMemVar("M_LINK_GL",gcAct_Comp)='Y' AND gfGetMemVar("M_DIV_LINK",gcAct_Comp)='Y')
*--P/O Setup.
llImpUse   = gfGetMemVar('M_LImpCost')
*B803981,1 SSH 24/01/2001 Check if the type is not logic replace with .F.
llImpUse = IIF(TYPE("llImpUse")<>"L",.F.,llImpUse)
*B803981,1 SSH [END]
*--IC Setup.
llMarkTop  = (gfGetMemVar('M_stymark')='T')
*--Multi warehouse setup.
llMultiwh  = (gfGetMemVar('M_WareHouse')='Y')
*--Dyelot system.
llDyeSysm  = (gfGetMemVar('M_Dyelot')='Y')

*E500257,1 Start.
*--Use Extended size scale.
llExtSzScl = .F.
*--Extended size scale conversion flag.
IF FILE(gcSysHome+'SYCNVSCL.DBF')
  *--Open system conversion of ext size scale.
  =gfOpenFile(gcSysHome+'SYCNVSCL','CExtScale','SH')   && Custom Extended scale library.
  GO TOP
  IF !EOF() AND RECCOUNT() > 0
    llExtSzScl = .T.
  ENDIF
  USE IN SYCNVSCL
ENDIF
*E500257,1 End.


*--Read the aria version to convert.
lcSelVer = ''
DO CASE
  CASE !EMPTY(lcSource) AND !EMPTY(lcASource)
    lcSelVer = 'A26ADV1'
  CASE  EMPTY(lcSource) AND !EMPTY(lcASource)
    lcSelVer = 'ADV1'
  CASE !EMPTY(lcSource) AND  EMPTY(lcASource)
    lcSelVer = 'A26'
ENDCASE

*--Proceed with conversion.
*--Converting files temp file.
lcTmpConv = gfTempName()

SELECT SYCNVLIB
COPY STRU TO (gcWorkDir+lcTmpConv)
=gfOpenFile(gcWorkDir+lcTmpConv,'','EX')
INDEX ON cApp_id+cFile TAG lcTmpCnv1 DESCENDING
INDEX ON cFile TAG lcTmpCnv2 ADDI
SET ORDER TO TAG lcTmpCnv2

SELECT SYCNVMDL
SCAN FOR lFlag
  lcfileMod = IIF(SYCNVMDL.cApp_Id='00','SY',SYCNVMDL.cApp_Id)

  WAIT WINDOW 'Preparing data for the conversion... module :'+lcfileMod NOWAIT
  llInWork  = (SYCNVMDL.cAutRef = 'Converting...' OR SYCNVMDL.cAutRef = 'Reconverting...')
  REPLACE cAutRef WITH IIF(EMPTY(cAutRef),'Converting...','Reconverting...')
  lnMax3 = lnMax3 + 1

  *--Define target array.
  DIME laSource[1],laTarget[1]
  =lfDefneFls()

  DIME laModNum[lnMax3,2]
  laModNum[lnMax3,1] = lcfileMod
  laModNum[lnMax3,2] = 0
  FOR lnI=1 TO ALEN(laTarget,1)
    IF ('(In-Work)' $ laTarget[lnI])
      LOOP
    ENDIF
    *--Selected module selected file.
    lcFleToLk= PADR(laTarget[lnI],10)

    IF !SEEK(ALLT(lcFleToLk),lcTmpConv)
      IF SEEK(lcFleToLk,'SYCNVLIB') AND ;
         ( ALLT(SYCNVLIB.CCNVVER) $ lcSelVer OR ;
           ALLT(SYCNVLIB.CCNVVER) = 'A26ADV1' OR ALLT(SYCNVLIB.CCNVVER) = 'NEW')
        SELECT SYCNVLIB
        SCATTER MEMVAR
        *--Lock selected files record.
        llCanLock = RLOCK()
        
        SELECT (lcTmpConv)
        APPEND BLANK
        GATHER MEMVAR
        REPLACE cApp_Id WITH lcfileMod
        laModNum[lnMax3,2] = laModNum[lnMax3,2] + 1
      ENDIF
    ENDIF   

  ENDFOR
   
  SELECT SYCNVMDL
ENDSCAN
WAIT CLEAR 

SELECT (lcTmpConv)
GO TOP
IF EOF()
  *--Nothing to convert, Cannot continue.
  =gfModalGen('TRM00324B00000','DIALOG','to convert')
  USE IN (lcTmpConv)
  ERASE (gcWorkDir+lcTmpConv+'.DBF')
  ERASE (gcWorkDir+lcTmpConv+'.CDX')
  RETURN .F.
ENDIF



lnMax3 = RECCOUNT()
SET ORDER TO TAG lcTmpCnv1

*--Prepere work areas in which the files will work.
SELECT 0
lnWaSel1 = SELECT()
USE (gcWorkDir+lcTmpConv) SHARED AGAIN ALIAS 'TEMPFL1'
IF 'A26' $ lcSelVer
  SELECT 0
  lnWaSel2 = SELECT()
  USE (gcWorkDir+lcTmpConv) SHARED AGAIN ALIAS 'TEMPFL2'
ENDIF
IF 'ADV1' $ lcSelVer
  SELECT 0
  lnWaSel3 = SELECT()
  USE (gcWorkDir+lcTmpConv) SHARED AGAIN ALIAS 'TEMPFL3'
ENDIF



*--Start scan for selected modules.
SELECT (lcTmpConv)

*B803644,1 Correct word "Prepering" to be "Preparing" [Begin]
*GO TOP
LOCATE
*lcD1Thrm = 'Prepering...' 
lcD1Thrm = 'Preparing...' 
*B803644,1 Correct word "Prepering" to be "Preparing" [End]

SHOW GET lcD1Thrm

*--Pick the first module and Lock the status for this module.
=SEEK(gcAct_Comp+IIF(cApp_id='SY','00',cApp_id),'SYCNVMDL')
SELECT SYCNVMDL
lcMod_Id = ALLT(cApp_Name)      && module description 
SHOW GET lcMod_Id
=RLOCK() 
REPLACE cAutRef  WITH IIF(EMPTY(cAutRef),'Converting...','Reconverting...'),;
        dDate    WITH gdSysDate,;
        cCnvTime WITH TIME()
UNLOCK

SELECT (lcTmpConv)
lcOldAppID = cApp_id     && old module id.

*--number of files in the module.
lnCurNm2 = 1
FOR lnI=1 TO ALEN(laModNum,1) 
  IF laModNum[lnI,1] = cApp_id
    lnMax2 = laModNum[lnI,2]
    EXIT
  ENDIF  
ENDFOR


SCAN
  IF llTerminat
     RETURN .F.
  ENDIF
  
  *--If module changed (next module).
  IF cApp_id <> lcOldAppID
    lcOldAppID = cApp_id     && old module id.

    *--Previus module finished.
    SELECT SYCNVMDL
    =RLOCK()
    REPLACE cAutRef WITH IIF(cAutRef='Converting...','Converted','Reconverted'),;
            dDate   WITH gdSysDate,;
            cCnvTime WITH TIME()
    UNLOCK

    SELECT (lcTmpConv)

    *B803644,1 Correct word "Prepering" to be "Preparing" [Begin]
    *lcD1Thrm = 'Prepering...' 
    lcD1Thrm = 'Preparing...' 
    *B803644,1 Correct word "Prepering" to be "Preparing" [End]

    SHOW GET lcD1Thrm

    *--Pick the first module and Lock the status for this module.
    =SEEK(gcAct_Comp+IIF(cApp_id='SY','00',cApp_id),'SYCNVMDL')
    SELECT SYCNVMDL
    lcMod_Id = ALLT(cApp_Name)      && module description 
    SHOW GET lcMod_Id
    =RLOCK() 
    REPLACE cAutRef  WITH IIF(EMPTY(cAutRef),'Converting...','Reconverting...'),;
            dDate    WITH gdSysDate,;
            cCnvTime WITH TIME()
    UNLOCK

    SELECT (lcTmpConv)
    *--number of files in the module.
    lnCurNm2 = 1
    FOR lnI=1 TO ALEN(laModNum,1) 
      IF laModNum[lnI,1] = cApp_id
        lnMax2 = laModNum[lnI,2]
        EXIT
      ENDIF  
    ENDFOR

  ENDIF

  lcMFile   = ALLT(cFile)             && file
  lcMFleDesc= ALLT(cFile_ttl)         && file description 
  SHOW GET lcMFleDesc

  *--Start convert.
  =lfConvData()

  *--Thermometers.
  =lfThrmo(lnCurNm2,lnMax2,'2')
  =lfThrmo(lnCurNm3,lnMax3,'3')
  
ENDSCAN

*--Update for last module.
SELECT SYCNVMDL
=RLOCK()
REPLACE cAutRef WITH IIF(cAutRef='Converting...','Converted','Reconverted'),;
        dDate   WITH gdSysDate,;
        cCnvTime WITH TIME()
UNLOCK



*--All converted.
SELECT(lnWaSel1)
USE
IF 'A26' $ lcSelVer
  USE IN (lnWaSel2)
ENDIF
IF 'ADV1' $ lcSelVer
  USE IN (lnWaSel3)
ENDIF

SELECT SYCNVMDL
REPLACE ALL cAutRef WITH IIF(cAutRef='Converting...','Converted','Reconverted'),;
            dDate   WITH gdSysDate,;
            cCnvTime WITH TIME() FOR lFlag AND '...' $ cAutRef



*--Run special programs update after the standerd conversion.

*--Check if STYLE or MATERIAL files are converting at this conversion session.
SELECT (lcTmpConv)
SET ORDER TO TAG lcTmpCnv2
llStyCnvrd = ( SEEK('STYLE ') OR SEEK('STYDYE ') )
llFabCnvrd = ( SEEK('FABRIC') OR SEEK('FABDYE ') )


*-1) Run Rebalance program by calling SM\SMREBAL.FXP.
IF llStyCnvrd OR llFabCnvrd
  =lfRebalSM()
ENDIF


*-2) Update Special Fields NMATWIP for material Wip.
IF llFabCnvrd
  =lfSpclFld()
ENDIF



USE IN (lcTmpConv)
ERASE (gcWorkDir+lcTmpConv+'.DBF')
ERASE (gcWorkDir+lcTmpConv+'.CDX')


*E500257,1 Start.
*-3) Run the Extended size scale conversion.
IF llExtSzScl
  *--Extended size scale conversion program.
  DO (gcAppHome+"SM\SMCVEXS.FXP")
ENDIF
*E500257,1 End.
RETURN




*:*************************************************************
*: Name      : lfConvData
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Convert Data.
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
FUNCTION lfConvData

SELECT (lcTmpConv)
lcFileVer = ALLT(CCNVVER)

lcD1Thrm = ' '
SHOW GET lcD1Thrm

*--Open Aria 27 file.
lcPath27 = IIF(cPath='S',gcSysHome,gcDataDir)
IF FILE(lcPath27+lcMFile+'.DBF') 
  IF USED(lcMFile)
    USE IN (lcMFile)  
  ENDIF
  SELECT (lnWaSel1) 
  lcErr=ON('ERROR')
  ON ERROR llDumy = lfUpdError('Module :'+lcMod_Id+' ( File '+lcMFile+' '+MESSAGE()+')')
  USE (lcPath27+lcMFile) EXCL 
  ON ERROR &lcErr
  IF EMPTY(ALIAS())
    RETURN
  ENDIF
  *--Do not zap this files .
*B801940,1 Start. 
* lcNoZapFl = 'ICISTRU ,SYCCURR ,FORMCDHD'
*B603435,1 Start.
* lcNoZapFl = 'ICISTRU ,SYCCURR ,FORMCDHD,FISHD   ,FSPRD   '
  lcNoZapFl = 'ICISTRU ,SYCCURR ,FORMCDHD,FISHD   ,FSPRD   ,ARCUSHST'
*B603435,1 End.
*B801940,1 End.
  *--Zap the distenation file.
  IF !(lcMFile $ lcNoZapFl)
    ZAP
  ENDIF
ELSE
  =lfUpdError('Module :'+lcMod_Id+' ( File '+lcMFile+' does not exist in Aria 2.7 directory.)')
  RETURN
ENDIF

*--Open equevelent apparel file.
IF !EMPTY(&lcTmpConv..cFile_a26) AND ('A26' $ lcFileVer OR 'NEW' $ lcFileVer) AND !EMPTY(lcSource)
  lcPathA26 = IIF(&lcTmpConv..cPath_a26='D',lcA26Data,;
              IIF(&lcTmpConv..cPath_a26='L',lcA26lib,;
              IIF(&lcTmpConv..cPath_a26='I',lcA26Img,lcSource)))
  lcA26File = IIF(!EMPTY(&lcTmpConv..cFile_a26),ALLT(&lcTmpConv..cFile_a26),lcMFile)
  IF FILE(lcPathA26+lcA26File+'.DBF') 
    *B604617,1 Added to check usage of the 26 file before open, and close it if used.
    IF USED('A'+lcA26File)
      USE IN ('A'+lcA26File)
    ENDIF
    *B604617,1 End.

    SELECT (lnWaSel2)
    USE (lcPathA26+lcA26File) SHARED
  ELSE
    =lfUpdError('Module :'+lcMod_Id+' ( File '+lcA26File+' does not exist in Aria 2.6 directory.)')
    RETURN
  ENDIF
ENDIF
  
*--Open equevelent advantage file.
IF !EMPTY(&lcTmpConv..cFile_adv1) AND 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  lcPathAdv = IIF(&lcTmpConv..cPath_Adv1='S',lcAdvSys,lcAdvData)
  lcAdvFile = IIF(!EMPTY(&lcTmpConv..cFile_adv1),ALLT(&lcTmpConv..cFile_adv1),lcMFile)
  IF FILE(lcPathAdv+lcAdvFile+'.DBF') 

    SELECT (lnWaSel3)
    USE (lcPathAdv+lcAdvFile) SHARED
  ELSE
    =lfUpdError('Module :'+lcMod_Id+' ( File '+lcAdvFile+' does not exist in Aria Adv. directory.)')
    RETURN
  ENDIF
ENDIF

*--Save any errors ocurse while converting files.
lcErr=ON('ERROR')
*E500382,1 AMH Add function to get fields witch creating numeric overflew error.  [Start]
*ON ERROR llDumy =lfUpdError('Module :'+lcMod_Id+' ( File '+lcMFile+' '+MESSAGE()+')')
ON ERROR llDumy =lfUpdError('Module :'+lcMod_Id+' ( File '+lcMFile+' '+'Rec No '+;
                 ALLTRIM(STR(RECNO()))+' '+lfGtFldErr()+' '+MESSAGE()+')')
*E500382,1 AMH [End]
*--All files was sucsesfuly opened start convert.
*-1) Case of normal update.
lnCurNm1  = 1
IF &lcTmpConv..lExt_call
  lnTgtWara = IIF('A26' $ lcFileVer, lnWaSel2 ,lnWaSel3 )
  lcFlFlter = IIF(!EMPTY(&lcTmpConv..f_cond),ALLT(&lcTmpConv..f_cond),".T.")

  *B801933,1 Start.
  *--Open Needed files for the Records.
  *--HelpFiles
  STORE .F. TO llNoUField
  DO CASE
    *B803695,1 AMM Open the spck_hdr file to get the account of each pack ID, to update STYLEUPC table
    CASE UPPER(lcMFile)='STYLEUPC'
      lcTmpPIdx = gfTempName()
      IF FILE(lcPathA26+'SPCK_HDR'+'.DBF') .AND. !USED('ASPCK_HDR')
        SELECT 0 
        USE lcPathA26+'SPCK_HDR' ALIAS ASPCK_HDR EXCL
        INDEX ON TYPE+PACK_ID+ACCOUNT TAG (lcTmpPIdx) OF  (gcWorkDir +  lcTmpPIdx + '.CDX')
      ENDIF
    *B803695,1 AMM end

    CASE UPPER(lcMFile)='STYLE'
      IF !lfOpn26Fl('CODE','CODE')
      ENDIF
      *B802126,1 Added to open a Notepad.
      IF !lfOpn26Fl('Notepad','Notepad')
        llNoUField = .T.
      ENDIF
  
    CASE UPPER(lcMFile)='SPCK_HDR'
      IF !lfOpn26Fl('SPCK_LIN','SPCK_LIN')
        llNoUField = .T.
      ENDIF

    CASE UPPER(lcMFile)='WIPADJ'
      IF !lfOpn26Fl('WAREHOUS','WAREHOUS')
        llNoUField = .T.
      ENDIF

    *B802126,1 Added to open a Notepad.
    CASE UPPER(lcMFile) $ 'CUSTOMER,FABRIC,RETHDR,SALESREP,LC '
      IF !lfOpn26Fl('Notepad','Notepad')
        llNoUField = .T.
      ENDIF
    *B802126,1 End.
    CASE UPPER(lcMFile)='CUTPICK'
      =!lfOpn26Fl('CUTTKTL','CUTTKTL',.T.)
      =!lfOpn26Fl('POSLN','POSLN',.T.)

    *E500259,1 Added to open some files used in EDISTORE convert.
    CASE UPPER(lcMFile)='EDISTORE'
      =!lfOpn26Fl('EDIACC','EDIACC')
      =gfOpenFile(gcSysHome+'SYCEDIPD','Partid' ,'SH')
    *E500259,1 End.

    *B803833,1 (Start) Added to open CODE file used in BOL_HDR convert.
    CASE UPPER(lcMFile)='BOL_HDR'
      =lfOpn26Fl('CODE','CODE')
    *B803833,1 (End).

  ENDCASE
  *B801933,1 End 

  *--C o v e r s i o n  e n j g e n.
  *--Scan on source file.
  SELECT (lnTgtWara)
  lnMax1 = RECCOUNT()
  SCAN FOR &lcFlFlter
    SCATTER MEMVAR MEMO
    =lfUpdEngn()
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN


  *B801933,1 Start.
  *--Close help file.
  IF UPPER(lcMFile)='STYLE' AND !llNoUField
    USE IN ACODE
    *B802126,1 Close a notepad.
    USE IN ANOTEPAD
  ENDIF
  IF UPPER(lcMFile)='SPCK_HDR' AND !llNoUField
    USE IN ASPCK_LIN
  ENDIF
  IF UPPER(lcMFile)='WIPADJ' AND !llNoUField
    USE IN AWAREHOUS
  ENDIF
  *B801933,1 End.
  *B802126,1 Close a notepad.
  IF UPPER(lcMFile) $ 'CUSTOMER,FABRIC,RETHDR,SALESREP,LC ' AND !llNoUField
    USE IN ANOTEPAD
  ENDIF
  IF UPPER(lcMFile)='CUTPICK'
    IF USED('ACUTTKTL')
      USE IN ACUTTKTL
    ENDIF
    IF USED('APOSLN')
      USE IN APOSLN
    ENDIF
  ENDIF
  *B802126,1 end.

  *B803833,1 (Start) Close CODE file after BOL_HDR convert.
  IF UPPER(lcMFile)='BOL_HDR' AND USED('ACODE')
    USE IN ACODE
  ENDIF
  *B803833,1 (End).

  *E500259,1 Added to close opend files.
  IF UPPER(lcMFile)='EDISTORE'
    IF USED('AEDIACC')
      USE IN AEDIACC
    ENDIF  
    IF USED('SYCEDIPD')
      USE IN SYCEDIPD
    ENDIF
  ENDIF
  *E500259,1 End.
  *B803695,1 AMM Close Spck_hdr table and erase the temporary index
  IF USED('ASPCK_HDR')
    SELECT ASPCK_HDR
    CLOSE INDEXES
    USE IN ASPCK_HDR
    ERASE (gcWorkDir +  lcTmpPIdx + '.CDX')
  ENDIF
  *B803695,1 AMM end
*-2)Case of UpNormal update.
ELSE
  lcRunUpd = &lcTmpConv..f_name
  IF !EMPTY(lcRunUpd)
    llDumy = &lcRunUpd
  ELSE
    =lfUpdError('[lcMod_Id]'+' File '+lcMFile+' was not Updated.')
  ENDIF
ENDIF
ON ERROR &lcErr
SELECT (lcTmpConv)
RETURN


*:*************************************************************
*: Name      : lfUpdEngn
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Engin to update.
*:*************************************************************
*: Called From  : lfConvData
*:*************************************************************
FUNCTION lfUpdEngn

*--Select Target file.
SELECT (lnWaSel1)
APPEND BLANK
GATHER MEMVAR MEMO
*--Seek for UpNormal fields update.
IF SEEK(PADR(lcMFile,10),'SYCNVLLN')
  SELECT SYCNVLLN
  SCAN WHILE cFile=PADR(lcMFile,10) FOR !EMPTY(SYCNVLLN.Content)
    lcAField = ALLT(cFld_name)
    lcSField = ALLT(Content)
    SELECT (lnWaSel1)
    DO CASE
      CASE SYCNVLLN.cType='A'
        REPLACE &lcAField WITH &lcSField
      CASE SYCNVLLN.cType='V'            
        lcStorVal = IIF(TYPE(lcAField)='N',VAL(SYCNVLLN.Content),IIF(TYPE(lcAField)='L',IIF(SYCNVLLN.Content='.T.',.T.,.F.),SYCNVLLN.Content))
        REPLACE &lcAField WITH lcStorVal
      CASE SYCNVLLN.cType='E'
        lcUpdFld = &lcSfield
        REPLACE &lcAField WITH lcUpdFld
    ENDCASE
  ENDSCAN
ENDIF
RETURN

*************************************************************************
FUNCTION lfOpn26Fl
*************************************************************************
*PARA lcPrFil,lcPrFlIdx
*B802078,1 Added a new parameters for existance of the file message.
PARA lcPrFil,lcPrFlIdx,llNoErMsge


IF FILE(lcPathA26+lcPrFil+'.DBF') 
  lcPflAls = 'A'+lcPrFil
  IF USED(lcPflAls)
    USE IN (lcPflAls)
  ENDIF
  SELECT 0
  USE (lcPathA26+lcPrFil) SHARED ALIAS (lcPflAls)
  *E:300630 SSH (Begin) To Check if ther is an index or not.
  IF !EMPTY(lcPrFlIdx)
   SET ORDER TO TAG (lcPrFlIdx)
  ENDIF
  *E:300630 SSH (END)
  RETURN .T.
ELSE
  *B802078,1 Added condition.
  IF ! llNoErMsge
    =lfUpdError('Module :'+lcMod_Id+' ( File '+lcPrFil+' does not exist in Aria 2.6 directory.)')
  ENDIF
  RETURN .F.
ENDIF

*********************************************************************
* lfRebalSM rebalance styles or marerials.
*********************************************************************
FUNCTION lfRebalSM

lcMFleDesc = 'Rebalance files...'
lcMod_Id   = ' '
STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc,lcD3Thrm,lcD3ThPrc
STORE 1  TO lnCurNm3,lnCurNm2,lnCurNm1
SHOW GET lcMFleDesc
SHOW GET lcMod_Id
SHOW GET lcD1Thrm
SHOW GET lcD2Thrm
SHOW GET lcD3Thrm  

DO CASE
  CASE llStyCnvrd AND llFabCnvrd
    *E301337,1 ASH 12/07/99 (Begin) Add MH to the parameter to rebalance material on hand.
    lcNeedRebal = 'WO,IN,RA,WI,ONO,MH'
    *E301337,1 ASH 12/07/99 (End)
  CASE llStyCnvrd  OR !llFabCnvrd
    lcNeedRebal = 'WO,IN,RA,WI'
  CASE !llStyCnvrd OR llFabCnvrd
    *E301337,1 ASH 12/07/99 (Begin) Add MH to the parameter to rebalance material on hand.
    lcNeedRebal = 'ONO,MH'
    *E301337,1 ASH 12/07/99 (End)
ENDCASE

*--Run Styles Rebalance for Work Orders,In-Transit,Retern Auth. and WIP.
*--Run Fabric Rebalance for onorder in location.
DO (gcAppHome+"SM\SMREBAL.FXP") WITH lcNeedRebal

RETURN


*********************************************************************
* Rebalance Special Fields. (NMATWIP)
*********************************************************************
FUNCTION lfSpclFld

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc,lcD3Thrm,lcD3ThPrc
STORE 1  TO lnCurNm3,lnCurNm2,lnCurNm1
lcMFleDesc = 'Rebalance Material WIP...'
SHOW GET lcMFleDesc
SHOW GET lcMod_Id
SHOW GET lcD1Thrm
SHOW GET lcD2Thrm
SHOW GET lcD3Thrm  

llOndFab=gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
llOndFbd=gfOpenFile(gcDataDir+'FABDYE','FABDYE','SH')
lcPathA26=lcA26Data
IF !lfOpn26Fl('CTKTBOM','')
  RETURN
ENDIF

SELECT ACTKTBOM
STORE RECCOUNT() TO lnMax1,lnMax2,lnMax3
SCAN FOR CCATGTYP $ 'FT'
  IF SEEK(PADR(ACTKTBOM.Item,7)+ACTKTBOM.IClr,'FABRIC')
    SELECT FABRIC
    =RLOCK()
    REPLACE nMatWIP WITH nMatWIP + ACTKTBOM.Used_Qty
    UNLOCK
  ENDIF
  IF SEEK(PADR(ACTKTBOM.Item,7)+ACTKTBOM.IClr+ACTKTBOM.cWareCode,'FABDYE')
    SELECT FABDYE
    =RLOCK()
    REPLACE nMatWIP WITH nMatWIP + ACTKTBOM.Used_Qty
    UNLOCK
  ENDIF
  =lfThrmo(lnCurNm1,lnMax1,'1')
  =lfThrmo(lnCurNm2,lnMax2,'2')
  =lfThrmo(lnCurNm3,lnMax3,'3')
ENDSCAN
USE IN ACTKTBOM

IF llOndFab AND USED('FABRIC')
  USE IN FABRIC
ENDIF
IF llOndFbd AND USED('FABDYE')
  USE IN FABDYE
ENDIF
RETURN


*B802126,1 Added.
*********************************************************************
* Update notepad field. (LHASNOTES)
*********************************************************************
FUNCTION lfVHsNpd
PARA lcNoPadKy

IF USED('ANOTEPAD')
  RETURN SEEK(lcNoPadKy,'ANOTEPAD')
ELSE
  RETURN .F.
ENDIF
*B802126,1 End.


*802126,1 Added.
*********************************************************************
* Update (CTKTLINENO) field in CUTPICK file.
*********************************************************************
FUNCTION lfvCTlin

lcTktLnNo='     1'
lnAlias = SELECT()
IF m.TranCd = '1'
  IF SEEK(m.Ctktno,'ACUTTKTL')
    lnLin = 0
    SELECT ACUTTKTL
    SCAN WHILE Cuttkt=m.Ctktno
      lnLin = lnLin + 1
      IF Cuttkt+Style+Color = m.Ctktno+m.Style+m.Color 
        lcTktLnNo = STR(lnLin,6)
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(m.Ctktno+m.Style+m.Color,'APOSLN')
    lcTktLnNo = STR(APOSLN.LineNo,6)
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN lcTktLnNo



*-------------- U P D A T E S     F U N C T I O N  -----------------* 
*-For comman files.
*********************************************************************
* Update SYCFACT.dbf
*********************************************************************
FUNCTION lfUpdFact

*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  SELECT (lnWaSel3)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR MEMO
    =lfUpdEngn()
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDIF

*--Convert equevelent apparel file.
IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
  *--Open aria27 file with index.
  SELECT (lnWaSel1)
  SET ORDER TO TAG Cfaccode
  SELECT (lnWaSel2)
  SET ORDER TO TAG Customer
  SEEK 'F'
  SCAN WHILE Type = 'F'
    SCATTER MEMVAR MEMO
    SELECT (lnWaSel1)
    IF !SEEK(m.Account)
    *--Does not exist in adv1.  
      APPEND BLANK
      REPLACE CFACCODE  WITH m.Account,;
              CFACCOMP  WITH m.BtName,;
              CPHONENO  WITH m.Phone1,;
              CFACCONT  WITH m.BUYER ,;     
              CFACCOMNT WITH m.NOTE,;
              CCONT_CODE WITH gccontcode,;
              CADDRESS1 WITH m.BTADDR1,;
              CADDRESS2 WITH m.BTADDR2,;
              CADDRESS3 WITH m.BTCITY,;
              CADDRESS4 WITH m.BTSTATE,;
              CADDRESS5 WITH m.BTZIP
    ENDIF
    *--Fields only in apparel.
    REPLACE LELECTRAN WITH m.ELECTRAN,;
            CCLIENTID WITH m.CLIENTID,;
            CCLIENTNO WITH m.CLIENTNO,;
            CCONFIRMID WITH m.CONFIRMID,;
            CPASSWORD  WITH m.PASSWORD
  ENDSCAN  
ENDIF
RETURN



*********************************************************************
* Update APVENDOR.dbf
*********************************************************************
FUNCTION lfUpdVend

*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  SELECT (lnWaSel3)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR MEMO
    =lfUpdEngn()
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDIF

*--Convert equevelent apparel file.
IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
  
  *B802126,1 Open.
  =lfOpn26Fl('Notepad','Notepad')


  *--Open aria27 file with index.
  SELECT (lnWaSel1)
  SET ORDER TO TAG Vencode
  SELECT (lnWaSel2)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lnWaSel1)
    *B802126,1 Added to update lHasNotes.
    IF !SEEK(m.Vendor)
      APPEND BLANK
      REPLACE CVENDCODE WITH m.Vendor,;
              CVENCOMP  WITH m.NAME,;
              CADDRESS1 WITH m.ADDR1,;
              CADDRESS2 WITH m.ADDR2,;
              CADDRESS3 WITH m.CITY,;
              CADDRESS4 WITH m.STATE,;
              CADDRESS5 WITH m.ZIP,;
              CADDRESS6 WITH m.COUNTRY,;
              CCONT_CODE WITH gccontcode,;
              CPHONENO  WITH m.PHONE,;
              CFAXNO    WITH m.FAX,;
              CVENCONT  WITH m.CONTACT,;
              CVENTITLE WITH m.VENDOR_TYP,;
              CVENOURAC WITH m.ACCOUNT,;
              CVENCOMNT WITH m.COMMENT1,;
              CTERMCODE WITH m.TERMS,;
              MVENNOTES WITH m.COMMENT2,;
              CCURRCODE WITH gcBaseCurr,;
              lHasNotes WITH lfVHsNpd('H'+m.Vendor)
      *C102076,1 SSH 22/01/01 Increase the color lenth to 6 char.
      IF FILE(gcSysHome+"SYCONFIL.DBF")
        REPLACE cLog_No  WITH m.Log_No,;
                CShrtNam WITH m.CShrtNam
      ENDIF
    ENDIF
  ENDSCAN  

  *B802126,1 Close a notepad.
  IF USED('ANOTEPAD')
    USE IN ANOTEPAD
  ENDIF
ENDIF
RETURN



*********************************************************************
* Update FISHD.dbf
*********************************************************************
FUNCTION lfUpdFHD

*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  SELECT (lnWaSel3)
*B801940,1 Start.
* lnMax1 = RECCOUNT()
*  SCAN FOR cComp_id = laASComp[lnASComp]
*    SCATTER MEMVAR MEMO
*    =lfUpdEngn()
*    =lfThrmo(lnCurNm1,lnMax1,'1')
*  ENDSCAN

  COUNT ALL TO lnMax1 FOR cComp_id = laASComp[lnASComp]
  IF lnMax1 <> 0
    SELECT (lnWaSel1)  
    ZAP
    SELECT (lnWaSel3)
    SCAN FOR cComp_id = laASComp[lnASComp]
      SCATTER MEMVAR MEMO
      =lfUpdEngn()
      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDSCAN

    *--Update the CURRENT fis year and period in SYCCOMP.
    SELECT 0
    *B801914,1 Start.
    *USE (lcASource+'SYCCOMP') ALIAS AVSYSCMP ORDER TAG CCOMP_ID SHARED
    USE (lcASource+'SYSFILES\SYCCOMP') ALIAS AVSYSCMP ORDER TAG CCOMP_ID SHARED
    *B801914,1 End.
    SEEK laASComp[lnASComp]
    IF !EMPTY(cCurr_yer+cCurr_prd)
      SELECT SYCCOMP
      =SEEK(gcAct_Comp)
      =RLOCK()
      REPLACE cCurr_yer WITH AVSYSCMP.cCurr_yer,;
              cCurr_prd WITH AVSYSCMP.cCurr_prd
      UNLOCK
    ENDIF
    USE IN AVSYSCMP
  ENDIF
ELSE
  *--Convert equevelent apparel file.
  IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
    SELECT (lnWaSel2)
    COUNT ALL TO lnMax1
    IF lnMax1 <> 0
      SELECT (lnWaSel1)  
      ZAP
      SELECT (lnWaSel2)
      SCAN
        SCATTER MEMVAR MEMO
        SELECT (lnWaSel1)
        APPEND BLANK
        *B802076,1 Remove the comp_id field.
        *REPLACE CCOMP_ID  WITH gcAct_Comp
        REPLACE CFISFYEAR WITH m.CFISYEAR  ,;
                CFISYSTAT WITH m.CYEARSTAT ,;
                CFISNOPRD WITH IIF(LEN(ALLT(STR(m.CNOPRD,2)))=1,'0','')+ALLT(STR(m.CNOPRD,2)),;
                CFISLHEAD WITH m.CDESCRIP  ,;
                CFISSHEAD WITH m.CDESCRIP  ,;
                cFisNonWd WITH '67',;
                DFISBGDAT WITH m.DBEGDATE  ,;
                DFISENDAT WITH m.DENDATE   ,;
                MFISCOMNT WITH m.MCOMENT

        =lfThrmo(lnCurNm1,lnMax1,'1')
      ENDSCAN

      *--Update the CURRENT fis year and period in SYCCOMP.
      llCntrOpn = gfOpenFile(lcSource+'CONTROL','CONTROL','SH')
      SEEK laSComp[lnSComp]
      IF !EMPTY(CurrYear+CurrPrd)
        SELECT SYCCOMP
        =SEEK(gcAct_Comp)
        =RLOCK()
        REPLACE cCurr_yer WITH CONTROL.CurrYear,;
                cCurr_prd WITH CONTROL.CurrPrd
        UNLOCK
      ENDIF
      IF llCntrOpn AND USED('CONTROL')
        USE IN CONTROL
      ENDIF

    ENDIF
  ENDIF
ENDIF
*B801940,1 End.
RETURN



*********************************************************************
* Update FSPRD.dbf
*********************************************************************
FUNCTION lfUpdFPD

*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
*B801940,1 Start.
*  SELECT (lnWaSel3)
*  lnMax1 = RECCOUNT()
*  SCAN FOR cComp_id = laASComp[lnASComp]
*    SCATTER MEMVAR MEMO
*    =lfUpdEngn()
*    =lfThrmo(lnCurNm1,lnMax1,'1')
*  ENDSCAN

  SELECT (lnWaSel3)
  COUNT ALL TO lnMax1 FOR cComp_id = laASComp[lnASComp]
  IF lnMax1 <> 0
    SELECT (lnWaSel1)  
    ZAP
    SELECT (lnWaSel3)
    SCAN FOR cComp_id = laASComp[lnASComp]
      SCATTER MEMVAR MEMO
      =lfUpdEngn()
      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDSCAN
  ENDIF

ELSE
  *--Convert equevelent apparel file.
  IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
    SELECT (lnWaSel2)
    COUNT ALL TO lnMax1
    IF lnMax1 <> 0
      SELECT (lnWaSel1)  
      ZAP
      SELECT (lnWaSel2)
      SCAN
        SCATTER MEMVAR MEMO
        SELECT (lnWaSel1)
        APPEND BLANK
        *B802076,1 Remove the comp_id field.
        *REPLACE CCOMP_ID WITH gcAct_Comp
        REPLACE CFISFYEAR WITH m.CFISYEAR,;
                CFSPPRDID WITH m.CPRDID,;
                CFSPPDESC WITH m.CPRDDESC,;
                DFSPPBGDT WITH m.DPRDBGDAT,;
                DFSPPENDT WITH m.DPRDENDAT,;
                NFSPPARTN WITH m.NPRDPARTN,;
                LFSPLOCKS WITH m.LPRDCLOST,;
                LFSPCLSDS WITH m.LPRDLCKST
      ENDSCAN  
    ENDIF
  ENDIF
ENDIF
*B801940,1 End.
RETURN



*********************************************************************
* Update CODES.dbf
*********************************************************************
FUNCTION lfUpdCods

*--Setups effect in releted fields for the following codes
*--(Credit and debit adj. ,Division ,ShipVia ,State).
llApInst  = ('AP' $ gcComp_mdl)
lcSCountry= UPPER(ALLTRIM(gcContCode))

*--Convert equevelent apparel file.
IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
  SELECT (lnWaSel2)
  SET ORDER TO TAG Code
  lcOldCdTyp = ' '
  lnMax1 = RECCOUNT()
  *--Skip Currency code records.
  SCAN FOR SUBSTR(Code,1,1)<>'F'
    SCATTER MEMVAR    
    SELECT (lnWaSel1)
    *B801981,1 replace desciption with code if empty.
    *CDISCREP  WITH m.cData
    APPEND BLANK
    *B802076,1 Remove the comp_id field and put cdefcode insted.
    *REPLACE CCOMP_ID  WITH gcAct_Comp
    REPLACE CDEFCODE  WITH 'N',;
            CCODE_NO  WITH SUBSTR(m.Code,2,6),;
            CDISCREP  WITH IIF(EMPTY(m.cData),SUBSTR(m.Code,2,6),m.cData),;
            CRLTFIELD WITH 'N',;
            CRLTD_NAM WITH '',;
            CRLTD_TYP WITH '',;
            CRLTD_VLU WITH '',;
            CADD_USER WITH m.Add_userid,;
            DADD_DATE WITH m.Add_date,;
            CADD_TIME WITH m.Add_Time
    *C102076,1 SSH 22/01/01 Increase the color lenth to 6 char.
    IF SUBSTR(m.Code,1,1) = "C" .AND. FILE(gcSysHome+"SYCONFIL.DBF")
      REPLACE cCode_No WITH PADR(ALLTRIM(cCode_No),6,"0")
    ENDIF
    *C102076,1 SSH [END]
    *--Update field name and releted fields.
    IF lfRetCode(SUBSTR(m.Code,1,1))
      *--If this is the first code it will be a default.
      SCATTER MEMVAR
      APPEND BLANK
      GATHER MEMVAR
      *B802076,1 Remove the comp_id field and put cdefcode insted.
      *REPLACE CCOMP_ID  WITH SPACE(2)
      REPLACE CDEFCODE  WITH 'D',;
              CRLTFIELD WITH 'N',;
              CRLTD_NAM WITH '',;
              CRLTD_TYP WITH '',;
              CRLTD_VLU WITH ''
    ENDIF    
    SELECT (lnWaSel2)
    lcOldCdTyp = SUBSTR(Code,1,1)
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN 

  *--Update new code (CUPS)
  DIME laUps[13,2]
  laUps = '' 
  laUps[1,1]  ='USUPSG' 
  laUps[1,2]  ='U.S.A. UPS Ground            '
  laUps[2,1]  ='USUPST' 
  laUps[2,2]  ='U.S.A. UPS Next Day Air saver'
  laUps[3,1]  ='USUPS3' 
  laUps[3,2]  ='U.S.A. UPS 3 Day Select      '
  laUps[4,1]  ='USUPS2' 
  laUps[4,2]  ='U.S.A. UPS 2nd Day Air       '
  laUps[5,1]  ='USUPSN' 
  laUps[5,2]  ='U.S.A. UPS Next Day Air      '
  laUps[6,1]  ='USUPSE' 
  laUps[6,2]  ='U.S.A. UPS 2nd Day Air A.M.  '
  laUps[7,1]  ='CAXPR '
  laUps[7,2]  ='Canada Xpresspost Service    '
  laUps[8,1]  ='CACOM '
  laUps[8,2]  ='Canada Commercial Parcel     '
  laUps[9,1]  ='CAEXPR' 
  laUps[9,2]  ='Express Service Within Canada  '
  laUps[10,1] ='CAEXPD'
  laUps[10,2] ='Expedited Service Within Canada'
  laUps[11,1] ='CASTAN'
  laUps[11,2] ='Standard Service Within Canada '
  laUps[12,1] ='CAECON'
  laUps[12,2] ='Economy Service Within Canada  '
  laUps[13,1] ='OTHER '
  laUps[13,2] ='Other'

  FOR lnI=1 TO 14
    SELECT (lnWaSel1)
    APPEND BLANK
    *B802076,1 Remove the comp_id field and put cdefcode insted.
    *REPLACE CCOMP_ID  WITH IIF(lnI=14,'',gcAct_Comp)
    REPLACE CDEFCODE  WITH IIF(lnI=14,'D','N'),;
            CFLD_NAME WITH 'CUPS      ',;
            CCODE_NO  WITH IIF(lnI=14,laUps[1,1],laUps[lnI,1]),;
            CDISCREP  WITH IIF(lnI=14,'',laUps[lnI,2]),;
            CRLTFIELD WITH 'N'
  ENDFOR
  

  *B802040,1 Start.
  *--Update code file for default manufacturing operation use for imported styles
  *--if no detail costing setup.
  IF ! llImpUse
    lcILlbl1 = gfGetMemVar('M_CICOST1')
    lcILlbl2 = gfGetMemVar('M_CICOST2')
    lcILlbl3 = gfGetMemVar('M_CICOST3')
    lcILlbl4 = gfGetMemVar('M_CICOST4')
    lcILlbl5 = gfGetMemVar('M_CICOST5')

    SELECT (lnWaSel1)
    FOR lnI=1 TO 5
      lcZ=STR(lnI,1)
      APPEND BLANK
      *B802076,1 Remove the comp_id field and put cdefcode insted.
      *REPLACE CCOMP_ID  WITH gcAct_Comp
      REPLACE CDEFCODE  WITH 'N',;
              CFLD_NAME WITH 'MFGCODE',;
              CCODE_NO  WITH '*'+lcZ  ,;
              CDISCREP  WITH lcILlbl&lcZ,;
              CRLTFIELD WITH 'N'
    ENDFOR 
  ENDIF
  *B802040,1 End.
ENDIF

*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  *--Open aria27 file with index.
  SELECT (lnWaSel1)
  SET ORDER TO TAG Codes
  SELECT (lnWaSel3)
  SET ORDER TO TAG Ccode_no
  lcOldFldNm=' '
  SEEK laASComp[lnASComp]
  SCAN WHILE cComp_id = laASComp[lnASComp] FOR ;
     cFld_Name $ 'CDIVISION ,CTAXCODE  ,CTERMCODE ,CVEN1099T ' 
    SCATTER MEMVAR
    SELECT (lnWaSel1)
    *B802076,1 Remove the comp_id field and put cdefcode insted.
    *IF SEEK(gcAct_Comp+m.cCode_no+'N'+m.cFld_name)
    IF SEEK('N'+m.cCode_no+'N'+m.cFld_name)
       SELECT (lnWaSel3)
       LOOP
    ENDIF
    *--Default record.
    IF m.cFld_Name<>lcOldFldNm AND (m.cFld_Name $ 'CTAXCODE  ,CVEN1099T ' OR EMPTY(lcSource))
      APPEND BLANK
      GATHER MEMVAR 
      *B802076,1 Remove the comp_id field and put cdefcode insted.
      *REPLACE CCOMP_ID  WITH SPACE(2)
      REPLACE CDEFCODE  WITH 'D',;
              CRLTFIELD WITH 'N',;
              CRLTD_NAM WITH '',;
              CRLTD_TYP WITH '',;
              CRLTD_VLU WITH ''
      lcOldFldNm = cFld_Name
    ENDIF
    APPEND BLANK
    GATHER MEMVAR 
    *B802076,1 Remove the comp_id field and put cdefcode insted.
    *REPLACE CCOMP_ID WITH gcAct_Comp
    REPLACE CDEFCODE WITH 'N'
    SCATTER MEMVAR
    *--Update field name and releted fields for terms and divisions only.
    IF m.cFld_Name = 'CDIVISION ' AND m.CRLTFIELD='N'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'CUPCMAN',CRLTD_TYP WITH 'C'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'DIVGROUP',CRLTD_TYP WITH 'C'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'DIVLNAME',CRLTD_TYP WITH 'C'
      
      *B603926,1 Add a new record for the UPCMethod [Begin]
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'CUPCGENTYP',CRLTD_TYP WITH 'C'            
      *B603926,1 Add a new record for the UPCMethod [End]
      
      IF llGlLnkStp
        APPEND BLANK
        GATHER MEMVAR 
        REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'CSLSGLLINK',CRLTD_TYP WITH 'C'
        APPEND BLANK
        GATHER MEMVAR 
        REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'LINK_CODE',CRLTD_TYP WITH 'C'
      ENDIF
    ENDIF
    IF m.cFld_Name = 'CTERMCODE ' AND m.CRLTFIELD='N'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'CODYN',CRLTD_TYP WITH 'C'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'EOM',CRLTD_TYP WITH 'C'
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE CRLTFIELD WITH 'Y',CRLTD_NAM WITH 'LINSTALLM',CRLTD_TYP WITH 'L'
    ENDIF
  ENDSCAN
ENDIF

******************
FUNCTION lfRetCode
******************
PARA lcCodeTyp

DIME laCodArry[1,3]
laCodArry=''
DO CASE
  CASE lcCodeTyp='A'
    laCodArry[1,1]='REASON'

  CASE lcCodeTyp='B' OR lcCodeTyp='E' 
    *B802998,1 Start, increase the array.
    *DIME laCodArry[IIF(llApInst,4,3),3]
    DIME laCodArry[IIF(llApInst,5,3),3]
    *B802998,1 End.
    laCodArry=''
    laCodArry[1,1]=IIF(lcCodeTyp='B','TRANCODE','CCREDITCOD')
    laCodArry[2,1]='ALLOW_TYPE'
    laCodArry[2,2]='C'
    laCodArry[2,3]='m.ALLOW_TYPE'
    *B802998,1 Start, make this releated field outside any conditions.
    laCodArry[3,1]='CADJACCT'
    laCodArry[3,2]='C'
    *B802998,1 End.
    *--Read default link code for Credit and Debit Adjustments.
    *--HelpFiles
    IF FILE(lcPathA26+'GL_LINK.DBF') 
      lnWAlis = SELECT()
      SELECT 0
      USE (lcA26Data+'GL_LINK') SHARED ALIAS 'AGL_LINK'
      SET ORDER TO TAG Gl_link
      =SEEK('DEF'+IIF(lcCodeTyp='B','010','009'))
      lcDefAdjAct = GLACNT
      USE IN AGL_LINK
      SELECT(lnWAlis)
      laCodArry[3,3]='lcDefAdjAct'
    ELSE
      =lfUpdError('Module :'+lcMod_Id+' ( File GL_LINK does not exist in Aria 2.6 directory.)')
    ENDIF

    *B802998,1 Start, change the array elements and comment the rest.
    IF llApInst
      laCodArry[4,1]='CBNKCODE'
      laCodArry[5,1]='CCHKACCT'
      laCodArry[4,2]='C'
      laCodArry[5,2]='C'
    ENDIF
    *IF llApInst
      *laCodArry[3,1]='CBNKCODE'
      *laCodArry[4,1]='CCHKACCT'
      *laCodArry[3,2]='C'
      *laCodArry[4,2]='C'
    *ELSE
      *laCodArry[3,1]='CADJACCT'    
      *laCodArry[3,2]='C'
      *--Read default link code for Credit and Debit Adjustments.
      *--HelpFiles
      *IF FILE(lcPathA26+'GL_LINK.DBF') 
      *  lnWAlis = SELECT()
      *  SELECT 0
      *  USE (lcA26Data+'GL_LINK') SHARED ALIAS 'AGL_LINK'
      *  SET ORDER TO TAG Gl_link
      *  =SEEK('DEF'+IIF(lcCodeTyp='B','010','009'))
      *  lcDefAdjAct = GLACNT
      *  USE IN AGL_LINK
      *  SELECT(lnWAlis)
      *  laCodArry[3,3]='lcDefAdjAct'
      *ELSE
      *  =lfUpdError('Module :'+lcMod_Id+' ( File GL_LINK does not exist in Aria 2.6 directory.)')
      *ENDIF
    *ENDIF
    *B802998,1 End.
        
  CASE lcCodeTyp='C'
    DIME laCodArry[3,3]
    laCodArry=''
    laCodArry[1,1]='COLOR'
    laCodArry[2,1]='CLRLNAME'
    laCodArry[3,1]='CNRFCODE'
    laCodArry[2,2]='C'
    laCodArry[3,2]='C'
    laCodArry[2,3]='m.CLRLNAME'
    laCodArry[3,3]='m.CNRFCODE'

  CASE lcCodeTyp='D'

    *B603926,1 Include the UPC method in the Array [Begin]
    *DIME laCodArry[IIF(llGlLnkStp,6,4),3]
    DIMENSION laCodArry[IIF(llGlLnkStp,7,5),3]
    *B603926,1 Include the UPC method in the Array [End]

    laCodArry=''
    laCodArry[1,1]='CDIVISION'
    laCodArry[2,1]='CUPCMAN'
    laCodArry[3,1]='DIVGROUP'
    laCodArry[4,1]='DIVLNAME'                
    laCodArry[2,2]='C'
    laCodArry[3,2]='C'
    laCodArry[4,2]='C'
    laCodArry[2,3]='m.CUPCMAN'
    laCodArry[3,3]='m.DIVGROUP'
    laCodArry[4,3]='m.DIVLNAME'

    IF llGlLnkStp
      laCodArry[5,1]='CSLSGLLINK'
      laCodArry[6,1]='LINK_CODE'
      laCodArry[5,2]='C'
      *B603674,1 KHM 06/04/2000 (Begin) Assigning the m.Link_code to 
      *B603674,1                laCodArry[5,3] in order to replace the 
      *B603674,1                related field 'CSLSGLLINK' in the codes file.
      laCodArry[5,3]='m.Link_Code'
      *B603674,1 KHM 06/04/2000 (End)
      laCodArry[6,2]='C'
      *B603674,1 KHM 06/04/2000 (Begin) Check if m.Link_Code ='DEF' then replace it with 'DEFDEF'.
      *laCodArry[6,3]='m.LINK_CODE'
      laCodArry[6,3]=IIF(EMPTY(m.Link_code) OR m.Link_code='DEF',"'DEFDEF'",m.Link_code)
      *B603674,1 KHM 06/04/2000 (End)
    ENDIF

    *B603926,1 Add the UPC method to the Array [Begin]
    PRIVATE lnLastRow
    lnLastRow = ALEN(laCodArry,1)
    laCodArry[lnLastRow,1]='CUPCGENTYP'
    laCodArry[lnLastRow,2]='C'
    laCodArry[lnLastRow,3]='m.cUPCMETHOD'
    *B603926,1 Add the UPC method to the Array [End]

  CASE lcCodeTyp='G'
    laCodArry[1,1]='CSTYGROUP'
    
  CASE lcCodeTyp='H'
    laCodArry[1,1]='BANK'
    
  CASE lcCodeTyp='I'
    laCodArry[1,1]='DECL_CODE'
    
  CASE lcCodeTyp='J'  
    laCodArry[1,1]='ITEM_TYPE'
    
  CASE lcCodeTyp='K'  
    DIME laCodArry[IIF(lcSCountry="CANADA",3,2),3]
    laCodArry=''
    laCodArry[1,1]='STATE'
    laCodArry[2,1]='NTAXRATE'
    laCodArry[2,2]='N'
    laCodArry[2,3]='m.NTAXRATE'
    IF lcSCountry="CANADA"
      laCodArry[3,1]='CTAXRULE'
      laCodArry[3,2]='C'
      laCodArry[3,3]='m.CTAXRULE'
    ENDIF

  CASE lcCodeTyp='L'    
    laCodArry[1,1]='CLASS'

  CASE lcCodeTyp='M'    
    DIME laCodArry[8,3]
    laCodArry=''
    laCodArry[1,1]='MFGCODE'
    laCodArry[2,1]='CCONTCODE'
    laCodArry[3,1]='CCONTNAME'
    laCodArry[4,1]='COPERSEQ'        
    laCodArry[5,1]='GLACCOUNT'
    laCodArry[6,1]='LEADTIME'    
    laCodArry[7,1]='LINHOUSE'
    laCodArry[8,1]='LMFGOPR'    
    laCodArry[2,2]='C'
    laCodArry[3,2]='C'
    laCodArry[4,2]='C'
    laCodArry[5,2]='C'
    laCodArry[6,2]='N'
    laCodArry[7,2]='L'
    laCodArry[8,2]='L'
    
  CASE lcCodeTyp='N'    
    DIME laCodArry[2,3]
    laCodArry=''
    laCodArry[1,1]='CCHRGCODE'
    laCodArry[2,1]='CTAXCODE'    
    laCodArry[2,2]='C'

  CASE lcCodeTyp='O'    
    laCodArry[1,1]='CCANCRESON'

  CASE lcCodeTyp='R'    
    laCodArry[1,1]='REGION'
    
  CASE lcCodeTyp='S'    
    laCodArry[1,1]='SPCINST'

  CASE lcCodeTyp='T'    
    DIME laCodArry[7,3]
    laCodArry=''
    laCodArry[1,1]='CTERMCODE'
    laCodArry[2,1]='CODYN'
    laCodArry[3,1]='EOM'
    laCodArry[4,1]='LINSTALLM'
    laCodArry[5,1]='NTERDISCD'
    laCodArry[6,1]='NTERDISCR'                    
    laCodArry[7,1]='NTERDUED'
    laCodArry[2,2]='C'
    laCodArry[3,2]='C'
    laCodArry[4,2]='L'
    laCodArry[5,2]='N'
    laCodArry[6,2]='N'
    laCodArry[7,2]='N'
    laCodArry[2,3]='m.CODYN'
    laCodArry[3,3]='m.EOM'
    laCodArry[5,3]='m.DISC_DAYS'
    laCodArry[6,3]='m.TRDE_DISC'        
    laCodArry[7,3]='m.DAYS_DUE'

  CASE lcCodeTyp='V'    
    *C102076,1 SSH 22/01/01 Add the custom ShipVia related field.
    IF FILE(gcSysHome+"SYCONFIL.DBF")
      DIME laCodArry[IIF(lcSCountry = "ENG",3,6),3]
    ELSE
    *C102076,1 SSH [END]
      DIME laCodArry[IIF(lcSCountry = "ENG",2,5),3]
    ENDIF
    laCodArry=''
    laCodArry[1,1]='SHIPVIA'
    laCodArry[2,1]='CUPS'
    laCodArry[2,2]='C'
    laCodArry[2,3]='m.CUPS'

    IF lcSCountry <> "ENG"
      laCodArry[3,1]='NCODCHARGE'
      laCodArry[4,1]='NFXDPRCNT'
      laCodArry[5,1]='NINSCHARGE'            
      laCodArry[3,2]='N'
      laCodArry[4,2]='N'
      laCodArry[5,2]='N'
      laCodArry[3,3]='m.NCODCHARGE'
      laCodArry[4,3]='m.NFXDPRCNT'
      laCodArry[5,3]='m.NINSCHARGE'
      *C102076,1 SSH 22/01/01 Add the Shipment Long name related field
      IF FILE(gcSysHome+"SYCONFIL.DBF")
        laCodArry[6,1] = "CSHIPLNAME"
        laCodArry[6,2] = "C"
        laCodArry[6,3] = "m.SHIPLNAMES"
      ENDIF
      *C102076,1 SSH [END]
    ELSE
       *C102076,1 SSH 22/01/01 Add the Shipment Long name related field
      IF FILE(gcSysHome+"SYCONFIL.DBF")
        laCodArry[3,1] = "CSHIPLNAME"
        laCodArry[3,2] = "C"
        laCodArry[3,3] = "m.SHIPLNAMES"
      ENDIF
      *C102076,1 SSH [END]
    ENDIF 

  CASE lcCodeTyp='Z'    
    *C102076,1 SSH 22/01/01 Add the season Long name related field
    IF FILE(gcSysHome+"SYCONFIL.DBF")
      DIME laCodArry[3,3]
      laCodArry[1,1]='SEASON'
      
      laCodArry[2,1] = "CSEALNAME"
      laCodArry[2,2] = "C"
      laCodArry[2,3] = "m.SEALNAME"
      
      laCodArry[3,1] = "CSEA2NAME"
      laCodArry[3,2] = "C"
      laCodArry[3,3] = "m.SEA2NAME"
    ELSE
      laCodArry[1,1]='SEASON'  
    ENDIF
   *C102076,1 SSH [END]
         
  CASE lcCodeTyp='P'    
    laCodArry[1,1]='CPRO_CODE'

  CASE lcCodeTyp='Y'    
    laCodArry[1,1]='CRESO_CODE'

  *C102076,1 SSH 22/01/01 Convert the care lable code
  CASE lcCodeTyp="Q"
    DIME laCodArry[6,3]
    PRIVATE lnCathInd , lcCathInd
    laCodArry[1,1]='CCARLBL'
    FOR lnCathInd = 2 TO 6
      lcCathInd = STR(lnCathInd-1,1)
      laCodArry[lnCathInd,1]='CDESC'+lcCathInd
      *B604246,1 SSH Change the type to char.
      *laCodArry[lnCathInd,2]='N'
      laCodArry[lnCathInd,2]='C'
      laCodArry[lnCathInd,3]='m.DESC'+lcCathInd
    ENDFOR
    *C102076,1 SSH [END]
ENDCASE

SELECT (lnWaSel1)
FOR lnI=1 TO ALEN(laCodArry,1)
  IF lnI=1
    REPLACE CFLD_NAME WITH laCodArry[1,1]
  ELSE
    APPEND BLANK  
    *B802076,1 Remove the comp_id field and put cdefcode insted.
    *REPLACE CCOMP_ID  WITH gcAct_Comp
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH laCodArry[1,1],;
            CCODE_NO  WITH SUBSTR(m.Code,2,6),;
            CRLTFIELD WITH 'Y',;
            CRLTD_NAM WITH laCodArry[lnI,1],;
            CRLTD_TYP WITH laCodArry[lnI,2],;
            CADD_USER WITH m.Add_userid,;
            DADD_DATE WITH m.Add_date,;
            CADD_TIME WITH m.Add_Time
    IF !EMPTY(laCodArry[lnI,3])
      lcRlValStr = laCodArry[lnI,3]
      lcRlValue  = &lcRlValStr

      *B603926,1 Add the Return Value for CUPCMETHOD in DIVISION code [Begin] 
      *REPLACE CRLTD_VLU WITH IIF(TYPE('lcRlValue')='N',ALLT(STR(lcRlValue,10,2)),lcRlValue)      
      IF lcCodeTyp = "D" AND laCodArry[lnI,1]=="CUPCGENTYP"
        REPLACE CRLTD_VLU WITH IIF(lcRlValue="U","S","E")
      ELSE
        REPLACE CRLTD_VLU WITH IIF(TYPE('lcRlValue')='N',ALLT(STR(lcRlValue,10,2)),lcRlValue)
      ENDIF  
      *B603926,1 Add the Return Value for CUPCMETHOD in DIVISION code [Begin] 

    ENDIF
  ENDIF
ENDFOR
RETURN (lcCodeTyp<>lcOldCdTyp)



*********************************************************************
* Update SEQUENCE.dbf
*********************************************************************
FUNCTION lfUpdSeq
ON ERROR
llOpnSdf=gfOpenFile(gcSysHome+'SYDFIELD','Cfld_name','SH')

STORE ' ' TO lcSeqFile,lcSeqTag
*--Convert equevelent advantage file.
IF 'ADV1' $ lcFileVer AND !EMPTY(lcASource)
  *--Available sequence in adv1.
  lcSeqAval = 'APSESS    ,BATCH     ,TRANSACT  ,BNKADJNO  ,PSESSION  ,CLOSENT   ,CODE      '
  lcSeqEquv = 'CAPSESSNO ,CBATCHNO  ,CTRANNO   ,CPAYDOCNO ,CPOSTSESS ,CCLOSENT  ,CCODE_NO  '
  SELECT (lnWaSel3)
  SCAN FOR cComp_id = laASComp[lnASComp] AND ALLT(cSeq_Type) $ lcSeqAval
    lcBseSeq = ALLT(cSeq_Type)
    lcEquSeq = SUBSTR(lcSeqEquv,AT(lcBseSeq,lcSeqAval),10) 
    lnSeq_NO = nSeq_NO
    SELECT (lnWaSel1)
    APPEND BLANK
    REPLACE CSEQ_TYPE WITH lcEquSeq,;
            NSEQ_NO   WITH lnSeq_NO + 1

    IF SEEK(lcEquSeq,'SYDFIELD')
      SELECT (lnWaSel1)
      =RLOCK()
      REPLACE NFLD_WDTH WITH SYDFIELD.nFld_wdth,;
              CDATA_TYP WITH SYDFIELD.cData_Typ
      UNLOCK
    ELSE
      =RLOCK()
      REPLACE NFLD_WDTH WITH 8,;
              CDATA_TYP WITH 'C'
      UNLOCK
    ENDIF
    =lfRedEqFil(CSEQ_TYPE)
    =RLOCK()
    REPLACE CFILE_NAM WITH lcSeqFile,;
            CFILE_TAG WITH lcSeqTag
    UNLOCK

  ENDSCAN
ENDIF

*--Convert equevelent apparel file.
IF 'A26' $ lcFileVer AND !EMPTY(lcSource)
  SELECT (lnWaSel2)  
  COUNT ALL TO lnBaseDiv
  lnDivSeq=1
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  lnMax1 = lnFileStru
  SCAN
    lnCurNm1  = 1
    *B802310,1 Start default the sequence by '01'.
    *lcBaseDiv = IIF(lnBaseDiv>1,IIF(LEN(ALLT(STR(lnDivSeq)))=1,'0','')+ALLT(STR(lnDivSeq)),' ')
    lcBaseDiv = IIF(lnBaseDiv>1,IIF(LEN(ALLT(STR(lnDivSeq)))=1,'0','')+ALLT(STR(lnDivSeq)),'01')
    *B802310,1 End.
    FOR lnI=1 TO lnFileStru  
      SELECT (lnWaSel2)  
      lcSeq_Type = UPPER(ALLT(laFileStru[lnI,1]))
      *--Skip this sequences.
      *B603300,1 Added to convert batch_pymt.
      *IF lcSeq_Type $ 'CHARGEBACK,BATCH_PYMT'
      IF lcSeq_Type $ 'CHARGEBACK'
        LOOP
      ENDIF  
      *B603300,1 End.
      lnSeq_no   = &lcSeq_Type
      lnPrTpLen  = IIF(lcSeq_Type='PURCHASE',2,1)
      FOR lnPrCnt = 1 TO lnPrTpLen 
        lcSeq_Type = IIF(lcSeq_Type='PURCHASE','PO ',lcSeq_Type)
        IF lnPrCnt = 2
          lcSeq_Type ='POMAT'
        ENDIF
        *--Fields with different names.
        IF lcSeq_Type $ 'NFINVOICE ,FINVOICE  '
          lcSeq_Type = IIF(lcSeq_Type='NFINVOICE','INVOICE','CFINVOICE')
        ENDIF
        *B603300,1 Start.
        IF lcSeq_Type $ 'BATCH,BATCH_PYMT'
          lcSeq_Type = IIF(lcSeq_Type='BATCH_PYMT','BATCH','REPBATCH')
        ENDIF

        *B603300,1 End.
        lcUdSeqGrp = lcBaseDiv
        lnFld_widt = 6
        lcData_typ = 'C'
        IF SEEK(lcSeq_Type,'SYDFIELD')
          lcUdSeqGrp = IIF('SEQPERCOMP' $ SYDFIELD.mCodeInfo,SPACE(3),lcBaseDiv)
          lnFld_widt = SYDFIELD.nFld_wdth
          lcData_typ = SYDFIELD.cData_Typ
        ENDIF
        SELECT (lnWaSel1)
        *C102076,1 SSH 22/01/01 [Begin] Modify the program to convert Cathy Daniel custom DBF.
        *--- We do not need to convert it.
        IF ALLTRIM(lcSeq_Type) <> "USE_CHR"
        *C102076,1 SSH 22/01/01 [End..]
          APPEND BLANK
          REPLACE CSEQ_TYPE  WITH lcSeq_Type,;
                  NSEQ_NO    WITH lnSeq_no  ,;
                  CSEQ_GROUP WITH lcUdSeqGrp,;
                  NFLD_WDTH  WITH lnFld_widt,;
                  CDATA_TYP  WITH lcData_typ
          =lfRedEqFil(CSEQ_TYPE)
          =RLOCK()
          REPLACE CFILE_NAM WITH lcSeqFile,;
                  CFILE_TAG WITH lcSeqTag
        *C102076,1 SSH 22/01/01 [BEGIN]
        ENDIF
        *C102076,1 SSH 22/01/01 [End..]
        UNLOCK
      ENDFOR
      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDFOR
    lnDivSeq=lnDivSeq+1
  ENDSCAN
ENDIF
IF llOpnSdf AND USED('SYDFIELD')
  USE IN SYDFIELD
ENDIF
RETURN


*******************
FUNCTION lfRedEqFil
PARA lcSqFldTyp
STORE ' ' TO lcSeqFile,lcSeqTag
DO CASE
  CASE lcSqFldTyp='CBATCHNO' OR lcSqFldTyp='CPOSTSESS'
    lcSeqFile='GLBATCH'
    lcSeqTag ='BATCHNO'
  CASE lcSqFldTyp='CTRANNO' OR  lcSqFldTyp='CCLOSENT'
    lcSeqFile='GLTRNSHD'
    lcSeqTag ='BATCHTRN'
  CASE lcSqFldTyp='ORDER'
    STORE 'ORDHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='ORDER'
    STORE 'ORDHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='CFINVOICE ' OR  lcSqFldTyp='INVOICE '
    STORE 'INVHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='PO '
    STORE 'POSHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='POMAT'
    STORE 'POFHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='PIKTKT'
    STORE 'PIKTKT' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='RANO'
    STORE 'RETAUTH' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='CUTTKT'
    STORE 'CUTTKTH' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='CRMRMO'
    STORE 'RETHDR' TO lcSeqFile,lcSeqTag
  CASE lcSqFldTyp='NMMFGORD'
    STORE 'MMFGORDH' TO lcSeqFile,lcSeqTag
  *B802310,1 Added case for GLsession no.
  CASE lcSqFldTyp='GLSESSION'
    lcSeqFile='GLDIST'
    lcSeqTag ='GLDISTNO'
  *B802310,1 End.
ENDCASE
RETURN


*********************************************************************
* Update SYCCURR.dbf
*********************************************************************
FUNCTION lfUpdCurcy


*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*B802021,1 (Start) Added.
lcOldBasCur = laCurrncy[lnCurrncy]
*B802021,1 (End).

*--Open aria27 file with index.
SELECT (lnWaSel1)
SET ORDER TO TAG Ccurrcode
SELECT (lnWaSel2)
SET ORDER TO TAG Code
SEEK 'F'
*--Scan for Currency code records.
SCAN WHILE SUBSTR(Code,1,1)='F' FOR SUBSTR(Code,2,3)<>lcOldBasCur
  SCATTER MEMVAR MEMO
  SELECT (lnWaSel1)
  IF !SEEK(PADR(SUBSTR(m.Code,2,3),5))
    APPEND BLANK
    REPLACE CCURRCODE WITH SUBSTR(m.Code,2,3),;
            CCURRDESC WITH m.cData,;
            NCURRUNIT WITH 1,;
            CCURRSMBL WITH SUBSTR(m.Code,2,3),;
            CCURMETH  WITH 'M'
  ENDIF
ENDSCAN  
RETURN


*********************************************************************
* Update ICISTRU.dbf
*********************************************************************
FUNCTION lfUpdSStu

*--Open aria27 file.
SELECT (lnWaSel1)
SET ORDER TO TAG Segno
IF !SEEK('A1')
  =lfUpdError('Module :'+lcMod_Id+' ( File '+lcMFile+' No default structure)')
  RETURN
ENDIF
SCATTER TO laStyRec
IF !SEEK('U1')
  APPEND BLANK
ENDIF
=RLOCK()
GATHER FROM laStyRec
*C102076,1 SSH 22/01/01 Convert Cathy Daniel style code sructure
IF FILE(gcSysHome+"SYCONFIL.DBF")
  =lfCathStr("1")
ELSE
*C102076,1 SSH [END]
  REPLACE cItemRecTy WITH 'U',;
          cISegLDes  WITH 'Style  ',;
          cAdd_user  WITH 'CONVERSION'
ENDIF
UNLOCK

IF SEEK('A2')
  SCATTER TO laStyRec
  IF !SEEK('U2')
    APPEND BLANK
  ENDIF
  =RLOCK()
  GATHER FROM laStyRec
  *C102076,1 SSH 22/01/01 Convert Cathy Daniel style code sructure
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    =lfCathStr("2")
  ELSE
  *C102076,1 SSH [END]
    REPLACE cItemRecTy WITH 'U',;
            cISegLDes  WITH 'Color  ',;
            cAdd_user  WITH 'CONVERSION'
  ENDIF
  UNLOCK
ENDIF
*C102076,1 SSH 22/01/01 Convert Cathy Daniel style code sructure
=lfCathStr("3")
*C102076,1 SSH [END]
RETURN



*********************************************************************
* Update ICSEGVAL.dbf
*********************************************************************
FUNCTION lfUpdSgVl

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open aria27 file with index.
SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SET ORDER TO TAG Cstyle
SCAN
  SCATTER MEMVAR
  SELECT (lnWaSel1)
  APPEND BLANK
  REPLACE CISEGNO   WITH '1',;
          CISEGVAL  WITH m.Style,;
          CISGVALSD WITH m.Desc,;
          CISGVALLD WITH m.Desc1
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN  
RETURN

*********************************************************************
* Update SCALEHD.dbf
*********************************************************************
FUNCTION lfUpdSlHd

*E500257,1 Start.
*--Don't update scale header since it will be updated in extended scale conversion program.
IF llExtSzScl
  RETURN
ENDIF  
*E500257,1 End.

*--Open aria27 file PUT A DUMMY RECORD.
SELECT (lnWaSel1)
APPEND BLANK
REPLACE DADD_DATE WITH gdSysDate,;
        CADD_TIME WITH TIME()
RETURN

*********************************************************************
* Update DYE_REL.dbf
*********************************************************************
FUNCTION lfUpdDyRl

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open aria27 file with index.
SELECT (lnWaSel1)
SET ORDER TO TAG Dye_rel
SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SET ORDER TO TAG Fabdye
SCAN FOR !EMPTY(Dyelot)
  SCATTER MEMVAR
  SELECT (lnWaSel1)
  IF !SEEK(m.Fabric+m.Color+m.Dyelot)
    APPEND BLANK
    REPLACE FABRIC WITH m.Fabric,;
            COLOR  WITH m.Color ,;
            DYELOT WITH m.Dyelot
  ENDIF  
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN  

*--Update dyelot sequence.
SELECT (lnWaSel1)
lcOldFbClr = ' * '
lnDyeSeq = 1
SCAN
  IF Fabric+Color <> lcOldFbClr
    lcOldFbClr = Fabric+Color
    lnDyeSeq = 1
  ENDIF
  REPLACE cDye_Seq WITH REPLICATE('0',4-LEN(ALLT(STR(lnDyeSeq)))) +ALLT(STR(lnDyeSeq))
  lnDyeSeq = lnDyeSeq + 1
ENDSCAN
RETURN


*********************************************************************
* Update GL_LINK.dbf
*********************************************************************
FUNCTION lfUGLLink

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

llOpnGlCtg=gfOpenFile(gcSysHome+'SYGLCATG','','SH')  
*--Note :The type in 26 is type+1 in 27. 
*--Ignore 26 categoties 002,009,010,011,017,027.
*--Open aria27 file with index.
SELECT (lnWaSel1)  && 27
SET ORDER TO TAG Gl_link
SELECT (lnWaSel2)  && 26
SET ORDER TO TAG Gl_link

*--Create for 'DEFDEF' default link code.
lcCLnkCode = 'DEF'
SEEK lcCLnkCode
SCAN WHILE Link_Code=lcCLnkCode FOR !( CatgKey $ '002,009,010,011,017,027')
  SCATTER MEMVAR
  SELECT SYGLCATG
  IF m.LinkType = '00'
    lcCg27Typ = m.LinkType
    llFound = .T.
  ELSE
    lcCg27Typ = '0'+ALLT(STR(VAL(m.LinkType)+1))
    LOCATE FOR CatgKey=m.CatgKey AND CatgType=lcCg27Typ
    llFound = FOUND()
  ENDIF
  IF llFound
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR      
    REPLACE LinkType  WITH lcCg27Typ,;
            Link_Code WITH 'DEFDEF'
  ENDIF
ENDSCAN  

SELECT SYGLCATG
SCAN
  SELECT (lnWaSel1)
  IF !SEEK('DEFDEF'+SYGLCATG.CATGKEY)
    APPEND BLANK
    REPLACE LINK_CODE WITH 'DEFDEF',;
            CATGKEY   WITH SYGLCATG.CATGKEY,;
            LINKTYPE  WITH SYGLCATG.CATGTYPE,;
            CATGDESC  WITH SYGLCATG.CATGDESC
    SELECT (lnWaSel2)
    =SEEK('DEF'+SYGLCATG.CATGKEY) 
    lcGlAcnt  = GLACNT
    lcGlAccDs = GLACCDESC
    SELECT (lnWaSel1)
    =RLOCK()
    REPLACE GLACNT WITH lcGlAcnt , GLACCDESC WITH lcGlAccDs
    UNLOCK
  ENDIF
ENDSCAN  

*--Create for Other link codes.
lcTmpGLL = gfTempName()
SELECT (lnWaSel2)
SET ORDER TO TAG Gl_link1
COPY ALL TO (gcWorkDir+lcTmpGLL) FOR LINK_CODE<>'DEF'
=gfOpenFile(gcWorkDir+lcTmpGLL,'','EX')
INDEX ON LINK_CODE+LINKTYPE TAG (lcTmpGLL)
SELECT (lnWaSel2)
SET ORDER TO TAG Gl_link
SELECT (lcTmpGLL)
lnMax1 = RECCOUNT()
SCAN
  lcCLnkCode = &lcTmpGLL..Link_Code
  lcCLnkType = &lcTmpGLL..LinkType
  SELECT (lnWaSel2)
  SEEK lcCLnkCode
  SCAN WHILE Link_Code=lcCLnkCode FOR LinkType =lcCLnkType AND !( CatgKey $ '002,009,010,011,017,027')
    SCATTER MEMVAR
    SELECT SYGLCATG
    IF m.LinkType = '00'
      lcCg27Typ = m.LinkType
      llFound = .T.
    ELSE
      lcCg27Typ = '0'+ALLT(STR(VAL(m.LinkType)+1))
      LOCATE FOR CatgKey=m.CatgKey AND CatgType=lcCg27Typ
      llFound = FOUND()
    ENDIF
    IF llFound
      SELECT (lnWaSel1)
      APPEND BLANK
      GATHER MEMVAR      
      REPLACE LinkType WITH lcCg27Typ
      *B603674,1.
      REPLACE LINK_CODE WITH PADR(LINK_CODE,3)+IIF(lcCg27Typ='02','DEF','')
    ENDIF
  ENDSCAN  

  *--Create new moved categories.
  IF lcCLnkType = '01'  &&26 Sales -> create also customer.
    SELECT SYGLCATG
    SCAN FOR CATGKEY $ '001,004,014'
      SELECT (lnWaSel1)
      IF !SEEK(lcCLnkCode+'   '+SYGLCATG.CATGKEY)
        APPEND BLANK
        REPLACE LINK_CODE WITH lcCLnkCode,;
                CATGKEY   WITH SYGLCATG.CATGKEY,;
                LINKTYPE  WITH SYGLCATG.CATGTYPE,;
                CATGDESC  WITH SYGLCATG.CATGDESC
        SELECT (lnWaSel2)
        IF !SEEK(lcCLnkCode+SYGLCATG.CATGKEY) 
          =SEEK('DEF'+SYGLCATG.CATGKEY) 
        ENDIF
        lcGlAcnt= GLACNT
        lcGlAcDs= GLACCDESC
        SELECT (lnWaSel1)
        =RLOCK()
        REPL GLACNT WITH lcGlAcnt, GLACCDESC WITH lcGlAcDs
        UNLOCK
      ENDIF
    ENDSCAN  
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

USE IN (lcTmpGLL)
ERASE (gcWorkDir+lcTmpGLL+'.DBF')
ERASE (gcWorkDir+lcTmpGLL+'.CDX')

IF llOpnGlCtg AND USED('SYGLCATG')
  USE IN SYGLCATG
ENDIF
RETURN


*********************************************************************
* Update WHSLOC.dbf
*********************************************************************
FUNCTION lfUWhLoc

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 style file to check if it style of fabric record.
*--HelpFiles
IF !lfOpn26Fl('STYLE','STYLE')
  RETURN
ENDIF

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR
  IF !EMPTY(m.Style) AND SEEK(m.Style,'ASTYLE')
    lcStyle = PADR(m.Style,12)+'-'+m.Color
    lcColor = SPACE(6)
  ELSE
    lcStyle = PADR(m.Style,12)
    lcColor = m.Color
  ENDIF
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR
  REPLACE Style WITH lcStyle,;
          Color WITH lcColor
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN  
USE IN ASTYLE
RETURN


*********************************************************
* S P E C I A L  FIELDS  U P D A T E   F U N C T I O N S.
*********************************************************


*B801933,1 Start.
*********************************************************************
* Update STYLE.dbf FIELD Desc1
*********************************************************************
FUNCTION lfvGetLDs

IF !llNoUField AND EMPTY(m.Desc1)
  lcLngDesc = ALLT(m.Desc) +' - '+IIF(SEEK('C'+m.Color,'ACODE'),ACODE.CDATA,'')
  RETURN lcLngDesc
ELSE
  RETURN m.Desc1
ENDIF


*********************************************************************
* Update STYLE.dbf FIELD nMarkRet
*********************************************************************
FUNCTION lfvRetMark

lnMarkPr = 0 
IF m.nSugRetPri<>0 AND m.TotCost<>0
  lnRotSub = IIF(llMarkTop,ROUND(m.TotCost,2),ROUND(m.nSugRetPri,2))
  lnMarkPr = ( ( ROUND(m.nSugRetPri,2)-ROUND(m.TotCost,2) ) / lnRotSub ) *100
ENDIF
RETURN lnMarkPr



*********************************************************************
* Update KEY_CHG.dbf FIELD cKey_Chg  
*********************************************************************
FUNCTION LFUKYCH

*B801914,1 Start.
lcKey_Chg = UPPER(ALLT(m.Key))
lcKey_Chg = IIF(lcKey_Chg='STYLE','CSTYMAJOR',;
            IIF(lcKey_Chg='VENDOR','CVENDCODE',;
            IIF(lcKey_Chg='CODE','STYLE',lcKey_Chg)))
RETURN lcKey_Chg
*B801914,1 End.

*********************************************************************
* Update SPCK_HDR.dbf FIELD Style.
*********************************************************************
FUNCTION lfvUdPkSty

*B801933,1 Start.
lcRtStyle = ' '
IF m.Type = 'S' AND !llNoUField
  *--Take the first style/color in sku lines.
  =SEEK(m.Type+m.Account+m.Pack_Id,'ASPCK_LIN')
  lcRtStyle = ASPCK_LIN.Style+'-'+ASPCK_LIN.Color
ENDIF
RETURN lcRtStyle




*********************************************************************
* Update WIPADJ.dbf FIELD cWareCode.
*********************************************************************
FUNCTION lfvGtDefWh

IF EMPTY(lcDefWipWH) AND !llNoUField
  *--Take the first Warehouse as a default.
  GO TOP IN AWAREHOUS
  lcDefWipWH = IIF(!EMPTY(AWAREHOUS.cWareCode),AWAREHOUS.cWareCode,'DEFWHE')
ENDIF
RETURN lcDefWipWH


*********************************************************************
* Update CTKTBOM.dbf
*********************************************************************
FUNCTION lfUCKBom
*B802040,1 Start.

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

SELECT (lnWaSel2)
*B605511,1 ABD - Fix error Insufficient memory error message in the conversion program
*B605511,1 ABD - only when convert CTKTBOM file , that because this file has a custoum 
*B605511,1 ABD - index and the program didn't open the files. [Begin]
*SET ORDER TO 1
SET ORDER TO Ctktbom
*B605511,1 ABD - [End]

STORE .F. TO llPOModEx,llMFModEx
*--Open 26 needed files.

IF SEEK('I')
  IF !lfOpn26Fl('POSHDR','POSHDR')
    RETURN
  ENDIF
  IF !lfOpn26Fl('POSLN','POSLNS')
    RETURN
  ENDIF
  llPOModEx = .T.
ENDIF

SELECT (lnWaSel2)
IF SEEK('M')
  IF !lfOpn26Fl('CUTTKTH','CUTTKTH')
    RETURN
  ENDIF
  IF !lfOpn26Fl('CUTTKTL','CUTTKTLS')
    RETURN
  ENDIF
  llMFModEx = .T.
ENDIF


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  lcWAHDR = IIF(m.cIMTyp = 'I','APOSHDR','ACUTTKTH')
  lcWALIN = IIF(m.cIMTyp = 'I','APOSLN' ,'ACUTTKTL')

  =lfThrmo(lnCurNm1,lnMax1,'1')
    
  IF !SEEK(m.Cuttkt,lcWAHDR)
    LOOP
  ENDIF

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE ICLR      WITH IIF(cCatgTyp='S',' ',m.IClr),;
          ITEM      WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
          LINK_CODE WITH IIF(EMPTY(m.Link_code) OR m.Link_code='DEF','DEFDEF',m.Link_code),;
          MFGCODE   WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode)

  *C102076,1 SSH 22/01/01 Convert custom cathy daniel fields.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    REPLACE CMARKER WITH MARKER,;
            NLOT_NO WITH LOT_NO
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK
  
  IF m.cCatgTyp='S'
    SELECT (lcWALIN)
    =SEEK(m.Item+m.IClr+m.Cuttkt)
    lcUsdSzs = ''
    FOR lnI=1 TO 8
      lnZ = STR(lnI,1)
      IF Qty&lnZ <> 0
        lcUsdSzs = lcUsdSzs + lnZ
      ENDIF
    ENDFOR
    SELECT (lnWaSel1)
    =RLOCK() 
    REPLACE CCOMPSIZES WITH lcUsdSzs
    UNLOCK
  ENDIF
  
ENDSCAN
IF llPOModEx
  USE IN APOSHDR
  USE IN APOSLN
ENDIF
IF llMFModEx 
  USE IN ACUTTKTH
  USE IN ACUTTKTL
ENDIF
RETURN
*B802040,1 End.


*********************************************************************
* Update BOM.dbf
*********************************************************************
FUNCTION lfUpdBom

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('STYLE','STYLE')
  RETURN
ENDIF
IF !lfOpn26Fl('SCALE','SCALE')
  RETURN
ENDIF

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  *B802040,1 Start.
  IF !SEEK(m.Style,'ASTYLE')
    LOOP
  ENDIF
  *B802040,1 End.

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B801981,1 replace style color with '*' if type is mgf operation.
  *CITMMASK WITH m.Style+'-'+m.SClr
  REPLACE CITMMAJOR  WITH m.Style,;
          NBOMTOTQTY WITH m.TotQty,;
          NESTBOMQTY WITH m.TotQty,;
          CITMMASK   WITH m.Style+'-'+IIF(cCatgTyp='M','******',m.SClr),;
          MFGCODE    WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
          ITEM       WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
          ICLR       WITH IIF(cCatgTyp='S',' ',m.IClr)
  *C102076,1 SSH 22/01/01 Convert custom cathy daniel fields.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    REPLACE CMARKER WITH m.Marker
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK
  
  *--Update MSIZES and MSZCROSREF if style componant.
  IF cCatgTyp='S'
    =lfUBomSCSz()
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Create cost sheet for imported styles if not used detail costing.
IF ! llImpUse
  *--Restore costing types.
  lcIType1   = gfGetMemVar('M_cIType1')
  lcIType2   = gfGetMemVar('M_cIType2')
  lcIType3   = gfGetMemVar('M_cIType3')
  lcIType4   = gfGetMemVar('M_cIType4')
  lcIType5   = gfGetMemVar('M_cIType5')      
  lcILlbl1   = gfGetMemVar('M_CICOST1')
  lcILlbl2   = gfGetMemVar('M_CICOST2')
  lcILlbl3   = gfGetMemVar('M_CICOST3')
  lcILlbl4   = gfGetMemVar('M_CICOST4')
  lcILlbl5   = gfGetMemVar('M_CICOST5')

  SELECT ASTYLE
  lnCurNm1 = 1
  lnMax1 = RECCOUNT()
  SCAN FOR !Make 
    SELECT BOM
    FOR lnI=1 TO 5
      lcZ=STR(lnI,1)
      IF ASTYLE.nICost&lcZ <> 0
        APPEND BLANK
        REPLACE CITMMAJOR  WITH ASTYLE.Style,;
                TYP        WITH lcZ,;
                CCATGTYP   WITH lcIType&lcZ,;
                CITMMASK   WITH ASTYLE.Style+'-'+ASTYLE.Color,;
                MFGCODE    WITH '*'+lcZ,;
                DESC       WITH lcILlbl&lcZ,;
                UOM        WITH 'EAC',;
                UNTCOST    WITH ASTYLE.nICost&lcZ,;
                NBOMTOTQTY WITH 1,;
                TOTCOST    WITH UNTCOST
      ENDIF        
    ENDFOR
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN

ENDIF
USE IN ASCALE
USE IN ASTYLE
RETURN


*******************
FUNCTION lfUBomSCSz
*******************
*-- Array hold the Parent style scale.
DECLARE laPrntScal[1]
laPrntScal = ""
lcPrntScal = ""

*-- Collect all the sizes for the parent scales..
*-- Get all the available scale for the current mask.
SELECT ASTYLE
=SEEK(m.Style)
SCAN REST WHILE Style = m.Style ;
            FOR LIKE(STRTRAN(m.Sclr,"*","?"),ASTYLE.Color)
  *-- Get the scale info.
  IF SEEK("S" + ASTYLE.Scale , "ASCALE")
    *-- Get the sizes for the current scale.
    lcCurSize = ""
    FOR lnCnt = 1 TO ASCALE.Cnt
      lcCnt     = STR(lnCnt,1)
      lcCurSize = lcCurSize + IIF(EMPTY(lcCurSize) , "" , ",") + lcCnt
      
      *-- Insert the current scale to the scale array.
      IF ASCAN(laPrntScal , PADR(ASCALE.Scale,3) + "," + lcCnt) = 0
        IF !EMPTY(laPrntScal[1])
          DECLARE laPrntScal[ALEN(laPrntScal,1)+1]
        ENDIF
        laPrntScal[ALEN(laPrntScal,1)] = PADR(ASCALE.Scale,3) + "," + lcCnt
      ENDIF
    ENDFOR
    *-- Get the string to be fill in the mSizes field.
    IF !(PADR(ASCALE.Scale,3) $ lcPrntScal)
      lcPrntScal = lcPrntScal + PADR(ASCALE.Scale,3) + "~" + lcCurSize +CHR(13)
    ENDIF
  ENDIF
ENDSCAN

*-- Fill the array that hold the scales of the style component.
DECLARE laChldScal[1]
laChldScal[1] = ""

*-- Scan to collect the different scales of the style component.
SELECT ASTYLE
=SEEK(m.Item)
SCAN REST WHILE Style = m.Item ;
            FOR LIKE(STRTRAN(m.Iclr,"*","?"),ASTYLE.Color)
  *-- Search for the current style scale.
  IF SEEK("S" + ASTYLE.Scale , "ASCALE")
    FOR lnCnt = 1 TO ASCALE.Cnt
      lcCnt   = STR(lnCnt,1)
      IF ASCAN(laChldScal , PADR(ASTYLE.Scale,3) + "," +lcCnt) = 0
        IF !EMPTY(laChldScal[1])
          DECLARE laChldScal[ALEN(laChldScal,1)+1]
        ENDIF
        *-- Hold the current scale + its size.
        laChldScal[ALEN(laChldScal,1)] = PADR(ASTYLE.Scale,3) + "," +lcCnt
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

*-- Update the sizes in the bom file.
SELECT (lnWaSel1)
=RLOCK() 
REPLACE mSizes     WITH lcPrntScal ;
        mSzCrosRef WITH lcCrosRef
UNLOCK
RETURN



*********************************************************************
* Update BOMLINE.dbf
*********************************************************************
FUNCTION lfUpdBLn

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('STYLE','STYLE')
  RETURN
ENDIF
IF !lfOpn26Fl('SCALE','SCALE')
  RETURN
ENDIF

*B802078,1 Continue conversion if the following files was not opened.
IF !lfOpn26Fl('CUTTKTL','CUTTKTLS',.T.)
  *RETURN
ENDIF
IF !lfOpn26Fl('POSLN','POSLNS',.T.)
  *RETURN
ENDIF
*B802040,1 Start.
IF !lfOpn26Fl('CUTTKTH','CUTTKTH',.T.)
  *RETURN
ENDIF
IF !lfOpn26Fl('POSHDR','POSHDR',.T.)
  *RETURN
ENDIF

*B802817,1 KHM 11/23/99 (Begin) Opening the cTktBom file to check if the
*B802817,1              record in the BomLine has a dyelot or not.
=!lfOpn26Fl('CTKTBOM','CTKTBOM',.T.)
*B802817,1 KHM 11/23/99 (End)

*B802817,1 KHM 11/23/99 (Begin) Creating lcTempCur cursor to hold the
*B802817,1              number of lines in each CutTkt or Po
lcTempCur = gfTempName()

*B606691,1 Increase the size of the numeric field. [Begin]
*CREATE CURSOR (lcTempCur) (cType C(1),cTktNo C(6),nLineNo N(3))
CREATE CURSOR (lcTempCur) (cType C(1),cTktNo C(6),nLineNo N(6))
*B606691,1 Increase the size of the numeric field. [End]

INDEX ON cType+cTktNo TAG &lcTempCur
*B802817,1 KHM 11/23/99 (End)

*B802078,1 End.
*B802040,1 End.

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()


SCAN
  SCATTER MEMVAR MEMO

  *B802040,1 Start.
  lcWAActv = IIF(m.cIMTyp = 'I','APOSHDR','ACUTTKTH')
  IF !SEEK(m.CtktNo,lcWAActv)
    LOOP
  ENDIF
  *B802040,1 End.

  *B802817,1 KHM 11/23/99 (Begin) Check if the cTktNo exists then add the
  *B802817,1              line number by one otherwise add a new record.
  IF !SEEK(m.cIMTyp+m.CtktNo,lcTempCur)
    INSERT INTO (lcTempCur) (cType,cTktNo,nLineNo) VALUES(m.cIMTyp,m.CtktNo,1)
  ELSE
    REPLACE &lcTempCur..nLineNo WITH &lcTempCur..nLineNo + 1
  ENDIF  
  *B802817,1 KHM 11/23/99 (End)
    
  *B802817,1 KHM 11/23/99 (Begin) Checking if the system is dyelot 'Y' and
  *B802817,1              its a manufactured record 'M' and found the 
  *B802817,1              style+color+cTktNo in the CutTktL then find if the
  *B802817,1              record has a dyelot or not in the CutTktL file. 
  *B802817,1              Then check if the record is found in th cTktBom 
  *B802817,1              file other wise it will work as it was.
  llFound = .F.
  IF llDyeSysm AND m.cIMTyp = 'M' AND ;
    SEEK(m.Style+m.SClr+m.CtktNo+'1','ACutTktL')
    SELECT ACutTktL
    LOCATE REST WHILE Style+Color+CutTkt+TranCd = m.Style+m.SClr+m.CtktNo+'1' ;
              FOR !EMPTY(Dyelot)
    IF FOUND()
      llFound = SEEK('M'+m.CtktNo+m.cType+IIF(.NOT.(m.ccatgtyp$"PMD"),;
                m.item,PADR(m.mfgcode,12))+m.IClr+ACutTktL.dyelot,'ACTKTBOM')
               
    ENDIF
  ENDIF
  IF llFound    
    SCAN REST WHILE Style+Color+CutTkt+TranCd = m.Style+m.SClr+m.CtktNo+'1' ;
              FOR !EMPTY(Dyelot)
      SELECT (lnWaSel1)
      APPEND BLANK
      GATHER MEMVAR MEMO
      *--Update Differencies
      =RLOCK() 
      REPLACE STYLE     WITH m.Style+'-'+m.SClr,;
              ITEM      WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
              ICLR      WITH IIF(cCatgTyp='S',' ',m.IClr),;
              MFGCODE   WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
              SCLR      WITH ' ',;
              CSTYGRADE WITH '1',;
              LINENO    WITH &lcTempCur..nLineNo,;
              Dyelot    WITH ACutTktL.Dyelot,;
              StyQty    WITH ACutTktL.TotQty,;
              ItemQty   WITH StyQty * UnitQty,;
              ItemAmt   WITH ItemQty * UnitCost
      UNLOCK        
      *--Update MSIZES and MSZCROSREF if style componant.
      IF cCatgTyp='S'
        =SEEK(m.Style+IIF(m.SClr='*','',m.SClr),'ASTYLE')
        lcBomClr = IIF(m.SClr='*',ASTYLE.Color,m.SClr)
        =SEEK('S'+ASTYLE.Scale,'ASCALE')
        STORE '' TO lcSzsStr,lcUsdSzs
        SELECT ACUTTKTL
  
        FOR lnI=1 TO ASCALE.Cnt
          lnZ = STR(lnI,1)
          lcSzsStr = lcSzsStr + lnZ
          IF Qty&lnZ <> 0
            lcUsdSzs = lcUsdSzs + lnZ
          ENDIF
        ENDFOR
  
        SELECT (lnWaSel1)
        =RLOCK() 
        REPLACE CSIZES     WITH lcSzsStr,; 
                CCOMPSIZES WITH lcUsdSzs
        UNLOCK
      ENDIF
      *B802817,1 KHM 11/23/99 (Begin) Check if the cTktNo exists then add the
      *B802817,1              line number by one otherwise add a new record.
      IF !SEEK(m.cIMTyp+m.CtktNo,lcTempCur)
        INSERT INTO (lcTempCur) (cType,cTktNo,nLineNo) VALUES(m.cIMTyp,m.CtktNo,1)
      ELSE
        REPLACE &lcTempCur..nLineNo WITH &lcTempCur..nLineNo + 1
      ENDIF  
      *B802817,1 KHM 11/23/99 (End)
    ENDSCAN
    *B802817,1 KHM 11/23/99 (Begin) Reducing the line number by one when
    *B802817,1              getting out of the Scan.
    =SEEK(m.cIMTyp+m.CtktNo,lcTempCur)
    REPLACE &lcTempCur..nLineNo WITH &lcTempCur..nLineNo - 1
    *B802817,1 KHM 11/23/99 (End)
  ELSE
  *B802817,1 KHM 11/23/99 (End)
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Update Differencies
    =RLOCK() 
    *B802817,1 KHM 11/23/99 (Begin) Changing the replacement of line no
    *REPLACE STYLE     WITH m.Style+'-'+m.SClr,;
            ITEM      WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
            ICLR      WITH IIF(cCatgTyp='S',' ',m.IClr),;
            MFGCODE   WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
            SCLR      WITH ' ',;
            CSTYGRADE WITH '1',;
            LINENO    WITH INT(VAL(m.LineNo))
    *B803222,1 Start, Update line no from count in C/T case only.
    *REPLACE STYLE     WITH m.Style+'-'+m.SClr,;
            ITEM      WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
            ICLR      WITH IIF(cCatgTyp='S',' ',m.IClr),;
            MFGCODE   WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
            SCLR      WITH ' ',;
            CSTYGRADE WITH '1',;
            LINENO    WITH &lcTempCur..nLineNo
    REPLACE STYLE     WITH m.Style+'-'+m.SClr,;
            ITEM      WITH IIF(cCatgTyp='S',m.Item+'-'+m.IClr,m.Item),;
            ICLR      WITH IIF(cCatgTyp='S',' ',m.IClr),;
            MFGCODE   WITH IIF(cCatgTyp$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
            SCLR      WITH ' ',;
            CSTYGRADE WITH '1',;
            LINENO    WITH IIF(m.cIMTyp='I',INT(VAL(m.LineNo)),&lcTempCur..nLineNo)  
    *B803222,1 End.
    *B802817,1 KHM 11/23/99 (End)
    UNLOCK
  
    *--Update MSIZES and MSZCROSREF if style componant.
    IF cCatgTyp='S'
      =SEEK(m.Style+IIF(m.SClr='*','',m.SClr),'ASTYLE')
      lcBomClr = IIF(m.SClr='*',ASTYLE.Color,m.SClr)
      =SEEK('S'+ASTYLE.Scale,'ASCALE')
      STORE '' TO lcSzsStr,lcUsdSzs
      IF m.cIMTyp = 'I'
        SELECT APOSLN
      ELSE
        SELECT ACUTTKTL
      ENDIF
      =SEEK(m.Style+lcBomClr+m.CtktNo+IIF(m.cIMTyp='I',STR(INT(VAL(m.Lineno)),4),'')+'1')
  
      FOR lnI=1 TO ASCALE.Cnt
        lnZ = STR(lnI,1)
        lcSzsStr = lcSzsStr + lnZ
        IF Qty&lnZ <> 0
          lcUsdSzs = lcUsdSzs + lnZ
        ENDIF
      ENDFOR
  
      SELECT (lnWaSel1)
      =RLOCK() 
      REPLACE CSIZES     WITH lcSzsStr,; 
              CCOMPSIZES WITH lcUsdSzs
      UNLOCK
    *B803222,1 Start, update cSizes with default.
    ELSE
      SELECT (lnWaSel1)
      =RLOCK() 
      REPLACE CSIZES WITH "12345678"
      UNLOCK
    ENDIF
    *B803222,1 End.
  *B802817,1 KHM 11/23/99 (Begin) 
  ENDIF
  *B802817,1 KHM 11/23/99 (End)
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Close 26 files.
IF USED('APOSLN')
  USE IN APOSLN
ENDIF
IF USED('APOSHDR')
  USE IN APOSHDR
ENDIF
IF USED('ACUTTKTL')
  USE IN ACUTTKTL
ENDIF
IF USED('ACUTTKTH')
  USE IN ACUTTKTH
ENDIF

USE IN ASCALE
USE IN ASTYLE
RETURN


*********************************************************************
* Update WAREHOUS.dbf
*********************************************************************
FUNCTION lfUpdWH


*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Case of single warehouse.
IF ! llMultiwh
  llCntrOpn = gfOpenFile(lcSource+'CONTROL','CONTROL','SH')
  SEEK laSComp[lnSComp]

  SELECT (lnWaSel1)
  APPEND BLANK
  REPLACE CWARECODE  WITH 'DEFWHE',;
          CDESC      WITH CONTROL.WHCOMPANY,;
          CPHONE     WITH CONTROL.WHPHONE,;
          UPS        WITH CONTROL.UPS,;
          CFAX       WITH CONTROL.FAX,;
          CADDRESS1  WITH CONTROL.WHADDR1,;
          CADDRESS2  WITH CONTROL.WHADDR2,;
          CADDRESS3  WITH CONTROL.WHCITY,;
          CADDRESS4  WITH CONTROL.WHSTATE,;
          CADDRESS5  WITH CONTROL.WHZIP,;
          CADDRESS6  WITH CONTROL.COUNTRY,;
          CCONT_CODE WITH gcContCode,;
          LMATINV    WITH .T.,;
          LSTYINV    WITH .T.,;
          GL_LINK    WITH 'DEFDEF'

  IF llCntrOpn AND USED('CONTROL')
    USE IN CONTROL
  ENDIF
 
*--Case of Multi Warehouse setup.
ELSE  
  SELECT (lnWaSel2)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR
    SELECT (lnWaSel1)
    APPEND BLANK
    *--Update Differencies
    =RLOCK() 
    *B603674,1 Add !EMPTY() condition in Link code update.
    REPLACE CWARECODE  WITH m.cWareCode,;
            CDESC      WITH m.cDesc ,;
            CPHONE     WITH m.cPhone,;
            UPS        WITH m.Ups,;
            CADDRESS1  WITH m.cAddr1  ,;
            CADDRESS2  WITH m.cAddr2  ,;
            CADDRESS3  WITH m.cCity   ,;
            CADDRESS4  WITH m.cState  ,;
            CADDRESS5  WITH m.cZip    ,;
            CADDRESS6  WITH m.cCountry,;
            CCONT_CODE WITH gcContCode,;
            LMATINV    WITH .T.,;
            LSTYINV    WITH .T.,;
            GL_LINK    WITH IIF(EMPTY(m.Gl_Link),'DEFDEF',m.Gl_link+IIF(m.Gl_link='DEF','DEF',''))
    UNLOCK
    
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  
ENDIF
RETURN



*********************************************************************
* Update STYDYE.dbf
*********************************************************************
FUNCTION lfUStyDye


*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Case of single warehouse.
IF ! llMultiwh
  *--Open 26 style file to fill the stydye file.
  *--HelpFiles
  IF !lfOpn26Fl('STYLE','STYLE')
    RETURN
  ENDIF

  SELECT (lnWaSel2)
  GO TOP
  llDyeExst = !EOF()
  IF llDyeExst
    SET ORDER TO TAG Stydye
  ENDIF

  SELECT ASTYLE
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR
    *--Update Differencies
    =RLOCK() 
    *B603674,1 Add !EMPTY() condition in Link code update.

    *B803644,1 Add the Audit fields [Begin]
    *REPLACE STYLE     WITH m.Style+'-'+m.Color,;
    *        CWARECODE WITH 'DEFWHE',;
    *        GL_LINK   WITH IIF(EMPTY(m.Link_code) OR m.Link_code='DEF','DEFDEF',m.Link_code),;
    *        NSTKVAL   WITH m.TotStk*m.Ave_Cost,;
    *        Dyelot    WITH SPACE(10)
    REPLACE STYLE     WITH m.Style+'-'+m.Color,;
            CWARECODE WITH 'DEFWHE',;
            GL_LINK   WITH IIF(EMPTY(m.Link_code) OR m.Link_code='DEF','DEFDEF',m.Link_code),;
            NSTKVAL   WITH m.TotStk*m.Ave_Cost,;
            Dyelot    WITH SPACE(10),;
            CAdd_Time WITH IIF(EMPTY(m.Mod_Time),m.Add_Time,m.Mod_Time),;
            Dadd_Date WITH IIF(EMPTY(m.Mod_Date),m.Add_Date,m.Mod_Date),;
            CAdd_User WITH IIF(EMPTY(m.Mod_Userid),m.Add_Userid,m.Mod_Userid)
    *B803644,1 Add the Audit fields [End]

    UNLOCK

    IF llDyeExst
      SELECT (lnWaSel2)
      IF SEEK(m.Style+m.Color)
        SCAN WHILE Style+Color=m.Style+m.Color FOR !EMPTY(Dyelot)
          SCATTER MEMVAR
          SELECT (lnWaSel1)
          APPEND BLANK
          GATHER MEMVAR
          *--Update Differencies
          =RLOCK() 

          *B803644,1 Add the Audit fields [Begin]
          *REPLACE STYLE WITH m.Style+'-'+m.Color
          REPLACE STYLE WITH m.Style+'-'+m.Color,;
          CAdd_Time WITH IIF(EMPTY(m.Mod_Time),m.Add_Time,m.Mod_Time),;
          Dadd_Date WITH IIF(EMPTY(m.Mod_Date),m.Add_Date,m.Mod_Date),;
          CAdd_User WITH IIF(EMPTY(m.Mod_Userid),m.Add_Userid,m.Mod_Userid)
          *B803644,1 Add the Audit fields [End]

          UNLOCK
        
        ENDSCAN
      ENDIF
    ENDIF    

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  
  USE IN ASTYLE


*--Case of Multi Warehouse setup.
ELSE  

  SELECT (lnWaSel2)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR
    *--Update Differencies
    =RLOCK() 
    *B603674,1 Add !EMPTY() condition in Link code update.

    *B803644,1 Add the Audit fields [Begin]
    *REPLACE STYLE   WITH m.Style+'-'+m.Color,;
    *        GL_LINK WITH IIF(EMPTY(m.GL_Link) OR m.GL_Link='DEF','DEFDEF',m.GL_Link),;
    *        NSTKVAL WITH m.TotStk*m.Ave_Cost
    REPLACE STYLE   WITH m.Style+'-'+m.Color,;
            GL_LINK WITH IIF(EMPTY(m.GL_Link) OR m.GL_Link='DEF','DEFDEF',m.GL_Link),;
            NSTKVAL WITH m.TotStk*m.Ave_Cost,;
            CAdd_Time WITH IIF(EMPTY(m.Mod_Time),m.Add_Time,m.Mod_Time),;
            Dadd_Date WITH IIF(EMPTY(m.Mod_Date),m.Add_Date,m.Mod_Date),;
            CAdd_User WITH IIF(EMPTY(m.Mod_Userid),m.Add_Userid,m.Mod_Userid)            
    *B803644,1 Add the Audit fields [End]

    UNLOCK

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  

ENDIF
RETURN



*B802817,1 KHM 11/23/99 Added To update the Fabdye file.
**********************************************************************
* Update FABDYE.dbf
**********************************************************************
FUNCTION lfUpdFabDy

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Case of single warehouse.
IF !llMultiwh
  *--Open 26 fabric file to fill the fabdye file.
  IF !lfOpn26Fl('Fabric','Fabric')
    RETURN
  ENDIF

  SELECT (lnWaSel2)
  GO TOP
  llDyeExst = !EOF()
  IF llDyeExst
    SET ORDER TO TAG FabDye
  ENDIF

  SELECT AFabric
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Update Differencies
    =RLOCK() 
    *B603674,1 Add !EMPTY() condition in Link code update.
    REPLACE CWARECODE  WITH 'DEFWHE',;
            GL_LINK    WITH IIF(EMPTY(m.Link_code) OR m.Link_code='DEF','DEFDEF',m.Link_code),;
            nFave_Cost WITH m.Ave_Cost,;
            NSTKVAL    WITH m.OnHand*m.Ave_Cost,;
            Dyelot     WITH SPACE(10)
    UNLOCK

    IF llDyeExst
      SELECT (lnWaSel2)
      IF SEEK(m.Fabric+m.Color)
        SCAN WHILE Fabric+Color=m.Fabric+m.Color FOR !EMPTY(Dyelot)
          SCATTER MEMVAR MEMO
          SELECT (lnWaSel1)
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDSCAN
      ENDIF
    ENDIF    

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  
  USE IN AFabric

*--Case of Multi Warehouse setup.
ELSE  

  SELECT (lnWaSel2)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Update Differencies
    =RLOCK() 
    *B603674,1 Add !EMPTY() condition in Link code update.
    *B803695,1 Read from m.GL_Link instead of m.Link_code.
    *REPLACE GL_LINK    WITH IIF(EMPTY(m.Link_code) OR m.Link_code='DEF','DEFDEF',m.Link_code)
    REPLACE GL_LINK    WITH IIF(EMPTY(m.GL_Link) OR m.GL_Link='DEF','DEFDEF',m.GL_Link),;
            nFave_Cost WITH m.Ave_Cost,;
            NSTKVAL    WITH m.OnHand*m.Ave_Cost
    *B803695,1 End.
    UNLOCK
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  
ENDIF
RETURN
*B802817,1 End.



*********************************************************************
* Update CUTTKTH.dbf
*********************************************************************
FUNCTION lfUCtkHdr

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('CTKTBOM','CTKTBOM')
  RETURN
ENDIF

*B802076,1 Open Gl file to read the account.
*B802078,1 Continue conversion if the following files was not opened.
llNoGlInfo = .F.
IF !lfOpn26Fl('GL_LINK','GL_LINK',.T.)
  llNoGlInfo = .T.
ENDIF

*B802126,1 Open.
=!lfOpn26Fl('Notepad','Notepad')


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B802126,1 Added to update lHasNotes.
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE cDivision  WITH m.Division,;
          Link_Code  WITH IIF(EMPTY(m.Link_Code) OR m.Link_Code='DEF','DEFDEF',m.Link_Code),;
          cTktType   WITH m.Type,;
          cMultiLot  WITH 'S',;
          lMultiWare WITH .F.,;
          lHasNotes  WITH lfVHsNpd('I'+m.Cuttkt)
  *C102076,1 SSH 22/01/01 Convert custom cathy daniel fields.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    =lfCathCtkH()
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK
  
  *--Update CITEMWARE , CMATWARE 
  IF Status<>'H' AND SEEK('M'+m.Cuttkt,'ACTKTBOM')
    SELECT ACTKTBOM  
    LOCATE REST WHILE cIMTyp+Cuttkt='M'+m.Cuttkt FOR (cCatgTyp='F' OR (Trim_invt AND cCatgTyp='T'))
    lcMatWare = IIF(FOUND(),cWareCode,m.cWareCode)
    =SEEK('M'+m.Cuttkt)
    LOCATE REST WHILE cIMTyp+Cuttkt='M'+m.Cuttkt FOR cCatgTyp='S'      
    lcItmWare = IIF(FOUND(),cWareCode,m.cWareCode)
    SELECT (lnWaSel1)
    =RLOCK() 
    REPLACE CMATWARE  WITH lcMatWare,;
            CITEMWARE WITH lcItmWare
    UNLOCK
  ENDIF
  SELECT (lnWaSel1)

  *B802076,1 Read the GL accounts if file was opened.
  IF !llNoGlInfo AND !EMPTY(m.Link_Code)
    IF SEEK(m.Link_Code+'013','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cWipAcnt  WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'022','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt1 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'023','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt2 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'024','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt3 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'025','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt4 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'026','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt5 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
  ENDIF
  
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B802126,1 Close a notepad.
IF USED('ANOTEPAD')
  USE IN ANOTEPAD
ENDIF

IF !llNoGlInfo AND USED('AGL_LINK')
  USE IN AGL_LINK
ENDIF

USE IN ACTKTBOM

RETURN


*********************************************************************
* Update CUTTKTL.dbf
*********************************************************************
FUNCTION lfUCtkLin

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

SELECT (lnWaSel2)
SET ORDER TO TAG CUTTKTL
lnMax1 = RECCOUNT()
lnTLinNo  = 0
lcOldCtkt = Cuttkt
SCAN
  SCATTER MEMVAR MEMO

  IF m.Cuttkt <> lcOldCtkt
    lnTLinNo = 1
    lcOldCtkt= m.Cuttkt
  ELSE
    *B801919,1 
    lnTLinNo = lnTLinNo + IIF(TranCd='1',1,0)
  ENDIF

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE Style     WITH m.Style+'-'+m.Color,;
          cStyGrade WITH '1',;
          LineNo    WITH lnTLinNo,;
          dPostDate WITH m.Date  ,;
          nAct_Cst1 WITH m.nAct_Cost1,;
          nAct_Cst2 WITH m.nAct_Cost2,;
          nAct_Cst3 WITH m.nAct_Cost3,;
          nAct_Cst4 WITH m.nAct_Cost4,;
          nAct_Cst5 WITH m.nAct_Cost5,;
          nLan_Cst1 WITH m.nLan_Cost1,;  
          nLan_Cst2 WITH m.nLan_Cost2,;  
          nLan_Cst3 WITH m.nLan_Cost3,;              
          nLan_Cst4 WITH m.nLan_Cost4,;  
          nLan_Cst5 WITH m.nLan_Cost5
  
  *C102076,1 SSH 22/01/01 Convert custom cathy daniel fields.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    REPLACE NLOT_NO WITH m.LOT_NO,;
            CCUTVEN WITH m.Vendor,;
            CCUTINV WITH m.Invoice,;
            cShip_Type WITH m.Ship_Type
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
RETURN



*********************************************************************
* Update POSHDR.dbf
*********************************************************************
FUNCTION lfUPoHdr

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('CTKTBOM','CTKTBOM')
  RETURN
ENDIF
*B802076,1 Open Gl file to read the account.
llNoGlInfo = .F.
IF !lfOpn26Fl('GL_LINK','GL_LINK',.T.)
  llNoGlInfo = .T.
ENDIF


*B802126,1 Open.
=!lfOpn26Fl('Notepad','Notepad')


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO

  m.Rate = IIF(m.Rate<=1,1,m.Rate)
  *B801919,1 added case in cwarecode update.
  *--Update Differencies
  =RLOCK() 
  REPLACE CSTYTYPE   WITH 'P',;
          CALLO_FLAG WITH m.Allocated,;
          CDIVISION  WITH m.Division,;
          CPURCODE   WITH lcGVr_PrGp,;
          CTERMCODE  WITH m.Terms,;
          CFOB       WITH m.Fob,;
          CLCCSTITM  WITH '12345',;
          CLCNO      WITH m.Lc,;
          CMULTILOT  WITH 'S',;
          COUTADDR1  WITH m.ShpAddr1,;
          COUTADDR2  WITH m.ShpAddr2,;
          COUTADDR3  WITH m.ShpCity,;
          COUTADDR4  WITH m.ShpState,;
          COUTADDR5  WITH m.ShpZip,;
          CPRICECUR  WITH IIF(m.Currency=laCurrncy[lnCurrncy],gcBaseCurr,m.Currency),;
          CDUTYCUR   WITH CPRICECUR,;
          NPRICERAT  WITH m.Rate,;
          NDUTYRAT   WITH m.Rate,;
          NCURRUNIT  WITH 1,;
          NDCURUNIT  WITH 1,;
          NSTYORDER  WITH m.Order,;
          NTOT_COST  WITH m.Tot_Cost,;
          CWARECODE  WITH IIF(!llMultiwh AND EMPTY(m.cWareCode),'DEFWHE',m.cWareCode),;
          LINK_CODE  WITH IIF(EMPTY(m.Link_Code) OR m.Link_Code='DEF','DEFDEF',m.Link_Code)

  REPLACE NFACTCOST1 WITH ROUND(m.nAct_Cost1*m.Rate,2),;
          NFACTCOST2 WITH ROUND(m.nAct_Cost2*m.Rate,2),;  
          NFACTCOST3 WITH ROUND(m.nAct_Cost3*m.Rate,2),;  
          NFACTCOST4 WITH ROUND(m.nAct_Cost4*m.Rate,2),;  
          NFACTCOST5 WITH ROUND(m.nAct_Cost5*m.Rate,2),;                        
          NFLANCOST1 WITH ROUND(m.nLAN_Cost1*m.Rate,2),;
          NFLANCOST2 WITH ROUND(m.nLAN_Cost2*m.Rate,2),;  
          NFLANCOST3 WITH ROUND(m.nLAN_Cost3*m.Rate,2),;  
          NFLANCOST4 WITH ROUND(m.nLAN_Cost4*m.Rate,2),;  
          NFLANCOST5 WITH ROUND(m.nLAN_Cost5*m.Rate,2),;
          NFCOST1    WITH ROUND(m.nICost1   *m.Rate,2),;
          NFCOST2    WITH ROUND(m.nICost2   *m.Rate,2),;
          NFCOST3    WITH ROUND(m.nICost3   *m.Rate,2),;
          NFCOST4    WITH ROUND(m.nICost4   *m.Rate,2),;
          NFCOST5    WITH ROUND(m.nICost5   *m.Rate,2)

  *B802126,1 Added to update lHasNotes.
  REPLACE lHasNotes WITH lfVHsNpd('P'+m.PO)
  UNLOCK

  *--Update CITEMWARE , CMATWARE 
  IF Status<>'H' AND SEEK('I'+m.PO,'ACTKTBOM')
    SELECT ACTKTBOM  
    LOCATE REST WHILE cIMTyp+Cuttkt='I'+m.PO FOR cCatgTyp='F'
    lcMatWare = IIF(FOUND(),cWareCode,m.cWareCode)
    =SEEK('I'+m.PO)
    LOCATE REST WHILE cIMTyp+Cuttkt='I'+m.PO FOR cCatgTyp='S'
    lcItmWare = IIF(FOUND(),cWareCode,m.cWareCode)
    SELECT (lnWaSel1)
    =RLOCK() 
    REPLACE CMATWARE  WITH lcMatWare,;
            CITEMWARE WITH lcItmWare
    UNLOCK
  ENDIF
  SELECT (lnWaSel1)

  *B802076,1 Read the GL accounts if file was opened.
  IF !llNoGlInfo AND !EMPTY(m.Link_Code)
    IF SEEK(m.Link_Code+'013','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cWipAcnt  WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'022','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt1 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'023','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt2 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'024','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt3 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'025','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt4 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
    IF SEEK(m.Link_Code+'026','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cVarAcnt5 WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
  ENDIF


  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B802126,1 Close a notepad.
IF USED('ANOTEPAD')
  USE IN ANOTEPAD
ENDIF

IF !llNoGlInfo AND USED('AGL_LINK')
  USE IN AGL_LINK
ENDIF

USE IN ACTKTBOM

RETURN


*********************************************************************
* Update POSLN.dbf
*********************************************************************
FUNCTION lfUPoLin

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('POSHDR','POSHDR')
  RETURN
ENDIF

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  lnRate=IIF(SEEK(m.PO,'APOSHDR') AND APOSHDR.Rate>0 ,APOSHDR.Rate,1)
  lnICost2 = APOSHDR.nICost2 
  lnICost3 = APOSHDR.nICost3 
  lnICost4 = APOSHDR.nICost4 
  lnICost5 = APOSHDR.nICost5 
  lnLstLine= IIF(APOSHDR.LastLine<=1,1,APOSHDR.LastLine)
  
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  *B801919,1 added case in cwarecode update.
  =RLOCK() 
  REPLACE Style     WITH m.Style+'-'+m.Color,;
          cStyType  WITH 'P',;
          cStyGrade WITH '1',;
          dPostDate WITH m.Date,;
          nLanDuRat WITH 1,;
          nLanPrRat WITH 1,;
          nActPrRat WITH 1,;
          cWareCode WITH IIF(!llMultiwh,'DEFWHE',IIF(EMPTY(cWareCode),APOSHDR.cWareCode,cWareCode))
  *B802021,1 Start.
  IF TranCd = '1' AND m.TotQty <> 0
     REPLACE nCost2 WITH ROUND(((lnICost2/lnLstLine)*lnRate)/m.TotQty,2),;
             nCost3 WITH ROUND(((lnICost3/lnLstLine)*lnRate)/m.TotQty,2),;
             nCost4 WITH ROUND(((lnICost4/lnLstLine)*lnRate)/m.TotQty,2),;
             nCost5 WITH ROUND(((lnICost5/lnLstLine)*lnRate)/m.TotQty,2)
  ENDIF
  *B802021,1 End.  
  *-Update forign costs.
  REPLACE nAct_Cst1 WITH m.nAct_Cost1,;
          nAct_Cst2 WITH m.nAct_Cost2,;
          nAct_Cst3 WITH m.nAct_Cost3,;
          nAct_Cst4 WITH m.nAct_Cost4,;
          nAct_Cst5 WITH m.nAct_Cost5,;
          nLan_Cst1 WITH m.nLan_Cost1,;  
          nLan_Cst2 WITH m.nLan_Cost2,;  
          nLan_Cst3 WITH m.nLan_Cost3,;              
          nLan_Cst4 WITH m.nLan_Cost4,;  
          nLan_Cst5 WITH m.nLan_Cost5

    *B802021,1 Start.
    *--Update in base curr.
    REPLACE NEACTCOST1 WITH ROUND(nAct_Cst1/lnRate,2),;
            NEACTCOST2 WITH ROUND(nAct_Cst2/lnRate,2),;  
            NEACTCOST3 WITH ROUND(nAct_Cst3/lnRate,2),;  
            NEACTCOST4 WITH ROUND(nAct_Cst4/lnRate,2),;  
            NEACTCOST5 WITH ROUND(nAct_Cst5/lnRate,2),;                        
            NELANCOST1 WITH ROUND(nLan_Cst1/lnRate,2),;
            NELANCOST2 WITH ROUND(nLan_Cst2/lnRate,2),;  
            NELANCOST3 WITH ROUND(nLan_Cst3/lnRate,2),;  
            NELANCOST4 WITH ROUND(nLan_Cst4/lnRate,2),;  
            NELANCOST5 WITH ROUND(nLan_Cst5/lnRate,2),;
            NECOST1    WITH ROUND(nCost1/lnRate,2),;
            NECOST2    WITH ROUND(nCost2/lnRate,2),;
            NECOST3    WITH ROUND(nCost3/lnRate,2),;
            NECOST4    WITH ROUND(nCost4/lnRate,2),;
            NECOST5    WITH ROUND(nCost5/lnRate,2)
  UNLOCK
  *B802021,1 End.  
  
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN APOSHDR
RETURN


*********************************************************************
* Update INVHDR.dbf
*********************************************************************
FUNCTION lfUdInvHd

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
*B801981,1 Don't open this file with this function since this function terminete the process,
*B801981,1 but what we need is if the file does not exist then continue the process and only
*B801981,1 does not update the BOL_No field, by adding a flag 'llUpdBOL'.
*IF !lfOpn26Fl('PACK_HDR','Orderpck')
*  RETURN
*ENDIF
IF FILE(lcPathA26+'PACK_HDR.DBF')
  SELECT 0
  USE (lcA26Data+'PACK_HDR') SHARED ALIAS 'APACK_HDR'
  SET ORDER TO TAG Orderpck
  llUpdBOL = .T.
ELSE
  llUpdBOL = .F.
ENDIF
*B801981,1 End.

*B802126,1 Open.
=!lfOpn26Fl('Notepad','Notepad')

*B803819,1 NAD (Start) Open the EDIACC file.
PRIVATE llClsEDI
llClsEDI=.F.
IF !USED('AEDIACC')
  llClsEDI=.T.
  =lfOpn26Fl('EDIACC','EDIACC')
ENDIF 
*B803819,1 NAD (End)

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()

SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK

  *B802288,1 Change invoice status 'O' to 'C'
  m.Status = IIF(m.Status='V',m.Status,'C')
  *B802288,1 (End)
  
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B802126,1 Added to update lHasNotes.
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE CDIVISION WITH m.Division,;
          CTERMCODE WITH m.Terms,;
          CFACCODE  WITH m.Factor,;
          CCURRcODE WITH gcBaseCurr,;
          NCURRUNIT WITH 1,;
          NEXRATE   WITH 1,;
          DPOSTDATE WITH m.InvDate,;
          LINK_CODE WITH IIF(EMPTY(m.Link_Code) OR m.Link_Code='DEF','DEFDEF',m.Link_Code),;
          lHasNotes WITH lfVHsNpd('C'+m.Invoice)
  UNLOCK  
*B803819,1 NAD (Start) Update the EDITrans file.
IF SEEK (m.ACCOUNT,'AEDIACC') AND !EMPTY(AEDIACC.INV_VER) AND EMPTY(m.dSentDate)
  *C102076,1 SSH 22/01/01 Add conditon that EDI module installed
  *IF !USED('EDITRANS')
  IF llEDI_Ins .AND. !USED('EDITRANS')
  *C102076,1 SSH [END]
    =gfOpenFile(gcDataDir+'EDITrans',gcDataDir+'Typekey')    
  ENDIF            
  *C102076,1 SSH 22/01/01 Add conditon that EDI module installed
  *IF !SEEK('810'+PADR(m.Invoice,20)+'A'+m.Account,'EDITrans')
  *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
  *IF llEDI_Ins .AND. !SEEK('810'+PADR(m.Invoice,20)+'A'+m.Account,'EDITrans')
  IF llEDI_Ins .AND. !SEEK('810'+PADR(m.Invoice,40)+'A'+m.Account,'EDITrans')
  *E037853,1 [End]
  *C102076,1 SSH [END]
    INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key      ,cPartner  ,Type,cStatus) VALUES;
                           ('810'     ,m.Invoice,m.Account ,'A' ,'N'    ) 
  ENDIF
ENDIF
IF !EMPTY(m.Factor) AND SEEK (m.Factor,'AEDIACC') AND !EMPTY(AEDIACC.INV_VER) AND EMPTY(m.dSentDate)
  *C102076,1 SSH 22/01/01 Add conditon that EDI module installed
  *IF !USED('EDITRANS')
  IF llEDI_Ins .AND. !USED('EDITRANS')
  *C102076,1 SSH [END]
    =gfOpenFile(gcDataDir+'EDITrans',gcDataDir+'Typekey')    
  ENDIF            
  *C102076,1 SSH 22/01/01 Add conditon that EDI module installed
  *IF !SEEK('810'+PADR(m.Invoice,20)+'A'+m.Factor,'EDITrans')
  *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
  *IF llEDI_Ins .AND. !SEEK('810'+PADR(m.Invoice,20)+'A'+m.Factor,'EDITrans')
  IF llEDI_Ins .AND. !SEEK('810'+PADR(m.Invoice,40)+'A'+m.Factor,'EDITrans')
  *E037853,1 [End]
  *C102076,1 SSH [END]
    INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key      ,cPartner  ,Type,cStatus) VALUES;
                           ('810'     ,m.Invoice,m.Factor ,'F' ,'N'    ) 
  ENDIF
ENDIF
*B803819,1 NAD (End) 

*B801981,1 Added a flag.
  IF llUpdBOL AND SEEK(m.Order+m.Store,'APACK_HDR')
*B801981,1 End.
    lcBol_no=''
    SELECT APACK_HDR
    SCAN REST WHILE Order+Store = m.Order+m.Store
      lcBol_no = Bill_ladg
    ENDSCAN 
    SELECT (lnWaSel1)
    =RLOCK() 
    REPLACE BOL_NO WITH lcBol_no
    UNLOCK
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B803819,1 NAD (Start) Close the EDIACC file.
IF llClsEdi AND USED('AEDIACC')
  USE IN AEDIACC
ENDIF
*B803819,1 NAD (END)

*B802126,1 Close a notepad.
IF USED('ANOTEPAD')
  USE IN ANOTEPAD
ENDIF

*--Close if opended.
*B801981,1 Added a flag.
IF llUpdBOL
*B801981,1 End.
  USE IN APACK_HDR
ENDIF
RETURN


*********************************************************************
* Update INVLINE.dbf
*********************************************************************
FUNCTION lfUdInvLn

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('ORDLINE','Ordlinst')
  RETURN
ENDIF
IF !lfOpn26Fl('ORDHDR','ORDHDR')
  RETURN
ENDIF
*B802315,1 added to open style and code to get DESC1 field.
IF !lfOpn26Fl('STYLE','STYLE')
  RETURN
ENDIF
IF !lfOpn26Fl('CODE','CODE')
  RETURN
ENDIF
*B802315,1 End.

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  =SEEK(m.Order,'AORDHDR')
  =SEEK(m.Order+m.Store+m.Style+m.Color+STR(m.Lineno,6),'AORDLINE')

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE STYLE      WITH m.Style+'-'+m.Color,;
          GL_COST    WITH IIF(EMPTY(m.GL_Cost) OR m.GL_Cost='DEF','DEFDEF',m.GL_Cost),;
          GL_SALES   WITH IIF(EMPTY(m.GL_Sales) OR m.GL_Sales='DEF','DEFDEF',PADR(m.GL_Sales,3)+'DEF'),;
          ALTSTYLE   WITH IIF(!EMPTY(m.AltStyle),m.AltStyle+'-'+m.AltColor,''),;
          GROS_PRICE WITH m.Price,;
          NEQVAMNT   WITH ROUND(m.Price*m.TotQty,2),;
          DESC1      WITH AORDLINE.DESC1,;
          GROUP      WITH AORDLINE.GROUP,;
          PPQTY      WITH AORDLINE.PPQTY,;
          PREPAK     WITH AORDLINE.PREPAK,;
          SEASON     WITH AORDHDR.SEASON
  *B802315,1 Read Desc1 if empty from style and code files.
  IF EMPTY(DESC1)
    =SEEK(m.Style+m.Color,'ASTYLE')
    REPLACE DESC1 WITH ALLT(ASTYLE.Desc)+' - '+IIF(SEEK('C'+m.Color,'ACODE'),ACODE.CDATA,'')
  ENDIF
  *B802315,1 End.
  UNLOCK  

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN AORDLINE
USE IN AORDHDR
*B802315,1 close style and code.
USE IN ASTYLE
USE IN ACODE
*B802315,1 End.
RETURN



*********************************************************************
* Update POFHDR.dbf
*********************************************************************
FUNCTION lfUMPoHd
*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*B802126,1 Open.
=!lfOpn26Fl('Notepad','Notepad')

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE CMATTYPE   WITH 'P' ,;
          POMAT      WITH m.PO,;
          NFABCANCEL WITH m.Cancel,;
          NFABDAMAGE WITH m.Damage,;
          NPO_OPEN   WITH m.Open,;
          NFABORDER  WITH m.Order,;
          NFBRECEIVE WITH m.Receive,;
          CDIVISION  WITH m.Division,;
          CTERMCODE  WITH m.Terms,;
          CFOB       WITH m.Fob,;
          CFAB_LC    WITH m.LC,;
          LINK_CODE  WITH 'DEFDEF',; 
          CFABGRADE  WITH '1',; 
          CPRICECUR  WITH IIF(m.Currency=laCurrncy[lnCurrncy],gcBaseCurr,m.Currency),;
          CDUTYCUR   WITH CPRICECUR,;
          CCURRCODE  WITH CPRICECUR,;
          NPRICERAT  WITH m.Rate,;
          NDUTYRAT   WITH m.Rate,;
          NCURRUNIT  WITH 1,;
          COUTADDR1  WITH m.ShpAddr1,;
          COUTADDR2  WITH m.ShpAddr2,;
          COUTADDR3  WITH m.ShpCity,;
          COUTADDR4  WITH m.ShpState,;
          COUTADDR5  WITH m.ShpZip,;
          CCONT_CODE WITH gcContCode
  *B802012,1 Added (Start).
  REPLACE NECOST1    WITH m.nCost1,;
          NECOST2    WITH m.nCost3,;
          NECOST3    WITH m.nCost2,;
          NECOST4    WITH m.nCost4,;
          NEACTCOST1 WITH m.nAct_Cost1,; 
          NEACTCOST2 WITH m.nAct_Cost3,; 
          NEACTCOST3 WITH m.nAct_Cost2,; 
          NEACTCOST4 WITH m.nAct_Cost4,; 
          NELANCOST1 WITH m.nLan_Cost1,; 
          NELANCOST2 WITH m.nLan_Cost3,; 
          NELANCOST3 WITH m.nLan_Cost2,; 
          NELANCOST4 WITH m.nLan_Cost4
  *B802012,1 (End).

  *B802126,1 Added to update lHasNotes.
  REPLACE lHasNotes WITH lfVHsNpd('M'+m.PO)
  
  REPLACE nCost1     WITH ROUND(NECOST1*m.Rate,2),;
          nCost2     WITH ROUND(NECOST2*m.Rate,2),;
          nCost3     WITH ROUND(NECOST3*m.Rate,2),;
          nCost4     WITH ROUND(NECOST4*m.Rate,2),;
          nAct_Cost1 WITH ROUND(NEACTCOST1*m.Rate,2),;
          nAct_Cost2 WITH ROUND(NEACTCOST2*m.Rate,2),;
          nAct_Cost3 WITH ROUND(NEACTCOST3*m.Rate,2),;
          nAct_Cost4 WITH ROUND(NEACTCOST4*m.Rate,2),;
          nLan_Cost1 WITH ROUND(NELANCOST1*m.Rate,2),;
          nLan_Cost2 WITH ROUND(NELANCOST2*m.Rate,2),;
          nLan_Cost3 WITH ROUND(NELANCOST3*m.Rate,2),;
          nLan_Cost4 WITH ROUND(NELANCOST4*m.Rate,2)


*    REPLACE NEACTCOST1 WITH ROUND(nAct_Cost1*m.Rate,1),; 
            NEACTCOST2 WITH ROUND(nAct_Cost2*m.Rate,1),; 
            NEACTCOST3 WITH ROUND(nAct_Cost3*m.Rate,1),; 
            NEACTCOST4 WITH ROUND(nAct_Cost4*m.Rate,1),; 
            NECOST1    WITH ROUND(nCost1*m.Rate,1),;
            NECOST2    WITH ROUND(nCost2*m.Rate,1),;
            NECOST3    WITH ROUND(nCost3*m.Rate,1),;
            NECOST4    WITH ROUND(nCost4*m.Rate,1),;
            NELANCOST1 WITH ROUND(nLan_Cost1*m.Rate,1),; 
            NELANCOST2 WITH ROUND(nLan_Cost2*m.Rate,1),; 
            NELANCOST3 WITH ROUND(nLan_Cost3*m.Rate,1),; 
            NELANCOST4 WITH ROUND(nLan_Cost4*m.Rate,1)

  UNLOCK
  
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B802126,1 Close a notepad.
IF USED('ANOTEPAD')
  USE IN ANOTEPAD
ENDIF
RETURN


*********************************************************************
* Update POFLN.dbf
*********************************************************************
FUNCTION lfUMPoLn

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('POFHDR','POFHDR')
  RETURN
ENDIF


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  lnRate=IIF(SEEK(m.PO,'APOFHDR') AND APOFHDR.Rate>0 ,APOFHDR.Rate,1)
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE CMATTYPE  WITH 'P' ,;
          POMAT     WITH m.PO,;
          CFABGRADE WITH '1',;
          DPOSTDATE WITH m.Date,;
          cWareCode WITH IIF(EMPTY(cWareCode),APOFHDR.cWareCode,cWareCode),;
          nFabTotQty WITH m.TotQty
  *B802021,1 Start.
  REPLACE NEACTCOST1 WITH m.nAct_Cost1,;
          NEACTCOST2 WITH m.nAct_Cost3,;  
          NEACTCOST3 WITH m.nAct_Cost2,;  
          NEACTCOST4 WITH m.nAct_Cost4,;  
          NELANCOST1 WITH m.nLAN_Cost1,;
          NELANCOST2 WITH m.nLAN_Cost3,;  
          NELANCOST3 WITH m.nLAN_Cost2,;  
          NELANCOST4 WITH m.nLAN_Cost4,;  
          NECOST1    WITH m.nCost1,;
          NECOST2    WITH m.nCost3,;
          NECOST3    WITH m.nCost2,;
          NECOST4    WITH m.nCost4

  REPLACE NCOST1 WITH nECost1*lnRate,;
          NCOST2 WITH nECost2*lnRate,;
          NCOST3 WITH nECost3*lnRate,;
          NCOST4 WITH nECost4*lnRate,;
          nAct_Cost1 WITH NEACTCOST1*lnRate,;
          nAct_Cost2 WITH NEACTCOST2*lnRate,;
          nAct_Cost3 WITH NEACTCOST3*lnRate,;
          nAct_Cost4 WITH NEACTCOST4*lnRate,;
          nLan_Cost1 WITH NELANCOST1*lnRate,;  
          nLan_Cost2 WITH NELANCOST2*lnRate,;  
          nLan_Cost3 WITH NELANCOST3*lnRate,;
          nLan_Cost4 WITH NELANCOST4*lnRate

*  REPLACE NEACTCOST1 WITH nAct_Cost1*lnRate,;
          NEACTCOST2 WITH nAct_Cost2*lnRate,;  
          NEACTCOST3 WITH nAct_Cost3*lnRate,;  
          NEACTCOST4 WITH nAct_Cost4*lnRate,;  
          NELANCOST1 WITH nLAN_Cost1*lnRate,;
          NELANCOST2 WITH nLAN_Cost2*lnRate,;  
          NELANCOST3 WITH nLAN_Cost3*lnRate,;  
          NELANCOST4 WITH nLAN_Cost4*lnRate,;  
          NECOST1    WITH nCost1*lnRate,;
          NECOST2    WITH nCost2*lnRate,;
          NECOST3    WITH nCost3*lnRate,;
          NECOST4    WITH nCost4*lnRate
  UNLOCK
  *B802021,1 End.
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN APOFHDR
RETURN


*********************************************************************
* Update CTKTRCVL.dbf
*********************************************************************
FUNCTION lfURcvBth

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--HelpFiles
IF !lfOpn26Fl('CUTTKTL','CUTTKTL')
  RETURN
ENDIF


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  lnTktLNo = 0
  SELECT ACUTTKTL
  =SEEK(m.Cuttkt)
  SCAN WHILE Cuttkt=m.Cuttkt FOR TranCd = '1'
     lnTktLNo = lnTktLNo + 1
     IF Style+Color = m.Style+m.Color
       EXIT
     ENDIF
  ENDSCAN

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE STYLE      WITH m.Style+'-'+m.Color,;
          CCARTON    WITH m.Carton,;
          CSTYGRADE  WITH '1',;
          CTYPE      WITH 'M',;
          LINENO     WITH VAL(m.LineNo),;
          NLINENO    WITH lnTktLNo
 
  UNLOCK
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN ACUTTKTL
RETURN

*********************************************************************
* Update NOTEPAD.dbf
*********************************************************************
FUNCTION lfUpdNPd

*E500259,1 (Start) Added to append the EDINOTEs for EDI orders in Standard Notepad.
SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

IF llEDI_Ins    && EDI installed.
  IF !lfOpn26Fl('EDINOTE','EDINOTE')
  ELSE
    SELECT AEDINOTE
    IF SEEK('B')
      SCAN WHILE Type = 'B' AND Key <> 'TEMP'
        SCATTER MEMVAR MEMO

        SELECT (lnWaSel1)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =RLOCK()
        REPLACE Key WITH 'T'+m.Key
        UNLOCK
      ENDSCAN
    ENDIF
    USE IN AEDINOTE
  ENDIF
ENDIF    
*E500259,1 (End).
RETURN



*********************************************************************
* Update ORDHDR.dbf
*********************************************************************
FUNCTION lfUOrdHdr

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('INVLINE','Invlineo')
  RETURN
ENDIF
IF !lfOpn26Fl('INVHDR','INVHDR')
  RETURN
ENDIF

*B802126,1 Open.
=!lfOpn26Fl('Notepad','Notepad')


SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B802126,1 Added to update lHasNotes.
  *B802324,1 Added new field GL_Sales.
  REPLACE CORDTYPE   WITH 'O',;
          CCLASS     WITH m.Class,;
          CTERMCODE  WITH m.Terms,;
          CDIVISION  WITH m.Division,;
          CFACCODE   WITH m.Factor,;
          DECL_CODE  WITH lcGVr_DclR,;
          CINSUR     WITH m.Insur,;
          CREORDER   WITH m.Reorder,;
          CCURRCODE  WITH gcBaseCurr,;
          NCURRUNIT  WITH 1,;
          NEXRATE    WITH 1,;
          CADDRESS1  WITH m.StAddr1,;
          CADDRESS2  WITH m.StAddr2,;
          CADDRESS3  WITH m.StCity+' , '+m.StState+' , '+m.StZip ,;
          CADDRESS4  WITH gcContCode,;
          lHasNotes  WITH lfVHsNpd('B'+m.Order),;
          GL_Sales   WITH 'DEF'
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE LINK_CODE  WITH IIF(EMPTY(m.Link_Code) OR m.Link_Code='DEF','DEFDEF',m.Link_Code) 
  *C102076,1 SSH 22/01/01 Convert Cathy Daniel Custom field.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    REPLACE CPACK WITH PACK
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK
  *--Check if it direct invoice order.
  IF m.Ship > 0 AND SEEK(m.Order,'AINVLINE')
    IF SEEK(AINVLINE.Invoice,'AINVHDR') AND AINVHDR.DIRECT_INV
      SELECT (lnWaSel1)
      =RLOCK()     
      REPLACE DIRECT_INV WITH .T.
      UNLOCK
    ENDIF
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*E500259,1 (Start) Append the EDIORDHD file records to a standatd ORDHDR file. 
IF llEDI_Ins  && EDI installed.
  IF !lfOpn26Fl('EDIORDHD','')
  ELSE
    =!lfOpn26Fl('EDINOTE','EDINOTE')
    SELECT AEDIORDHD
    lnCurNm1 = 1
    lnMax1   = RECCOUNT()
    SCAN
      SCATTER MEMVAR MEMO

      SELECT (lnWaSel1)
      APPEND BLANK
      GATHER MEMVAR MEMO
      *--Update Differencies
      =RLOCK() 
      REPLACE CORDTYPE   WITH 'T',;
              CCLASS     WITH m.Class,;
              CTERMCODE  WITH m.Terms,;
              CDIVISION  WITH m.Division,;
              CFACCODE   WITH m.Factor,;
              LINK_CODE  WITH IIF(EMPTY(m.Link_Code) OR m.Link_Code='DEF','DEFDEF',m.Link_Code),; 
              CINSUR     WITH m.Insur,;
              CREORDER   WITH m.Reorder,;
              CCURRCODE  WITH gcBaseCurr,;
              NCURRUNIT  WITH 1,;
              NEXRATE    WITH 1,;
              CADD_USER  WITH m.Add_UserId,;
              CADD_TIME  WITH m.Add_Time,;
              DADD_DATE  WITH m.Add_Date
      UNLOCK    

      IF USED('AEDINOTE') AND SEEK('B'+m.Order,'AEDINOTE')
        =RLOCK()
        REPLACE lHasNotes WITH .T.
        UNLOCK
      ENDIF

      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDSCAN
    IF USED('AEDINOTE')
      USE IN AEDINOTE
    ENDIF
    USE IN AEDIORDHD

  ENDIF
ENDIF
*E500259,1 End.

*B802126,1 Close a notepad.
IF USED('ANOTEPAD')
  USE IN ANOTEPAD
ENDIF

USE IN AINVLINE
USE IN AINVHDR
RETURN


*********************************************************************
* Update ORDLINE.dbf
*********************************************************************
FUNCTION lfUOrdLin

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open 26 needed files.
*--HelpFiles
IF !lfOpn26Fl('ORDHDR','ORDHDR')
  RETURN
ENDIF

*B802078,1 Continue conversion if the following files was not opened.
STORE .F. TO llNoPacks,llNoPiktk
IF !lfOpn26Fl('PACK_LIN','PACK_LIN',.T.)
  llNoPacks = .T.
  *RETURN
ENDIF
IF !lfOpn26Fl('PACK_HDR','Orderpck',.T.)
  llNoPacks = .T.
  *RETURN
ENDIF
IF !lfOpn26Fl('PIKTKT','Ordpik',.T.)
  llNoPiktk = .T.
  *RETURN
ENDIF
*B802315,1 added to open style and code to get DESC1 field.
IF !lfOpn26Fl('STYLE','STYLE')
  RETURN
ENDIF
IF !lfOpn26Fl('CODE','CODE')
  RETURN
ENDIF
*B802315,1 End.

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO
  
  =SEEK(m.Order,'AORDHDR')

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B603674,1 Add !EMPTY() condition in Link code update.
  *B603674,1 Update store field in ordline by the one in header.
  REPLACE CORDTYPE   WITH 'O',;
          STYLE      WITH m.Style+'-'+m.Color,;
          STORE      WITH IIF(AORDHDR.Multi='N' AND !EMPTY(AORDHDR.STORE),AORDHDR.STORE,m.Store),;
          GROS_PRICE WITH m.Price,;
          GL_COST    WITH IIF(EMPTY(m.GL_Cost) OR m.GL_Cost='DEF','DEFDEF',m.GL_Cost),;
          GL_SALES   WITH IIF(EMPTY(m.GL_Sales) OR m.GL_Sales='DEF','DEFDEF',PADR(ALLT(m.GL_Sales),3)+'DEF'),;
          ALTSTYLE   WITH IIF(!EMPTY(m.AltStyle),m.AltStyle+'-'+m.AltColor,''),;
          SEASON     WITH AORDHDR.Season  

  *-We only can calculate the book if no cancel or ship.
  IF AORDHDR.Cancel = 0 AND AORDHDR.Ship = 0
    REPLACE BOOK1    WITH m.QTY1,;
            BOOK2    WITH m.QTY2,;
            BOOK3    WITH m.QTY3,;
            BOOK4    WITH m.QTY4,;
            BOOK5    WITH m.QTY5,;
            BOOK6    WITH m.QTY6,;
            BOOK7    WITH m.QTY7,;
            BOOK8    WITH m.QTY8,;                                    
            TOTBOOK  WITH m.TOTQTY
  ENDIF

  *B802078,1 Read only if the file was opened.
  IF !llNoPiktk AND !EMPTY(m.PikTkt) AND SEEK(m.Order+m.PikTkt,'APIKTKT')
    REPLACE CWARECODE WITH APIKTKT.CWARECODE
  ELSE
    *B801919,1 Take the warehouse from header.
    REPLACE CWARECODE WITH AORDHDR.CWARECODE
    *B801919,1 End.
  ENDIF

  *B802315,1 Read Desc1 if empty from style and code files.
  IF EMPTY(DESC1)
    =SEEK(m.Style+m.Color,'ASTYLE')
    REPLACE DESC1 WITH ALLT(ASTYLE.Desc)+' - '+IIF(SEEK('C'+m.Color,'ACODE'),ACODE.CDATA,'')
  ENDIF
  *B802315,1 End.
  *C102076,1 SSH 22/01/01 Convert Cathy Daniel Custom field.
  IF FILE(gcSysHome+"SYCONFIL.DBF")
    REPLACE CPACK WITH PACK
  ENDIF
  *C102076,1 SSH [END]
  UNLOCK

  *B802078,1 Read only if the file was opened.
  IF !llNoPacks AND SEEK(m.Order+m.Store,'APACK_HDR') AND SEEK(APACK_HDR.Pack_No,'APACK_LIN')
    SELECT APACK_LIN
    LOCATE REST WHILE Pack_No=APACK_HDR.Pack_No FOR OrdLineNo = m.LineNo
    IF FOUND()
      SELECT (lnWaSel1)
      =RLOCK() 
      REPLACE NPCK1  WITH APACK_LIN.Qty1,;
              NPCK2  WITH APACK_LIN.Qty2,;
              NPCK3  WITH APACK_LIN.Qty3,;
              NPCK4  WITH APACK_LIN.Qty4,;
              NPCK5  WITH APACK_LIN.Qty5,;
              NPCK6  WITH APACK_LIN.Qty6,;
              NPCK7  WITH APACK_LIN.Qty7,;
              NPCK8  WITH APACK_LIN.Qty8,;
              NPWGHT WITH APACK_LIN.WEIGHT
      UNLOCK
    ENDIF
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*E500259,1 (Start) Append the EDIORDLN file records to a standatd ORDLINE file. 
IF llEDI_Ins  && EDI installed.
  IF !lfOpn26Fl('EDIORDLN','')
  ELSE
    =!lfOpn26Fl('EDIORDHD','EDIORDHD')
    =!lfOpn26Fl('CUSTOMER','CUSTOMER')
    SELECT AEDIORDLN
    lnCurNm1 = 1
    lnMax1   = RECCOUNT()
    SCAN
      SCATTER MEMVAR MEMO
      =SEEK(m.Order,'AEDIORDHD')

      SELECT (lnWaSel1)
      APPEND BLANK
      GATHER MEMVAR MEMO
      *--Update Differencies
      =RLOCK() 
      REPLACE CORDTYPE   WITH 'T',;
              STYLE      WITH m.Style+'-'+m.Color,;
              CWARECODE  WITH AEDIORDHD.cWareCode,;
              GL_COST    WITH 'DEFDEF',;
              GL_SALES   WITH 'DEFDEF',;
              SEASON     WITH AEDIORDHD.Season,;  
              GROS_PRICE WITH m.Price,;
              BOOK1     WITH m.QTY1,;
              BOOK2     WITH m.QTY2,;
              BOOK3     WITH m.QTY3,;
              BOOK4     WITH m.QTY4,;
              BOOK5     WITH m.QTY5,;
              BOOK6     WITH m.QTY6,;
              BOOK7     WITH m.QTY7,;
              BOOK8     WITH m.QTY8,;                       
              TOTBOOK   WITH m.TOTQTY
      UNLOCK    
    
      IF SEEK('M'+m.Account,'ACUSTOMER') AND SEEK(m.Style+m.Color,'ASTYLE')
        lcPRLvL = IIF(!EMPTY(ACUSTOMER.PriceLvl),ACUSTOMER.PriceLvl,'A')
        lnGPRice= ASTYLE.Price&lcPRLvL
        =RLOCK()
        REPLACE GROS_PRICE WITH lnGPRice,;
                DISC_PCNT  WITH IIF(Gros_Price<Price,0,( (1-(Price/Gros_Price)) * 100) ),;
                COST       WITH ASTYLE.Ave_Cost
        UNLOCK
      ENDIF

      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDSCAN
    USE IN ACUSTOMER
    USE IN AEDIORDHD
    USE IN AEDIORDLN
  ENDIF
ENDIF
*E500259,1 End.


IF USED('APIKTKT')
  USE IN APIKTKT
ENDIF
IF USED('APACK_LIN')
  USE IN APACK_LIN
ENDIF
IF USED('APACK_HDR')
  USE IN APACK_HDR
ENDIF
USE IN AORDHDR
*B802315,1 close style and code.
USE IN ASTYLE
USE IN ACODE
*B802315,1 End.
RETURN



*********************************************************************
* Update RETLINE.dbf
*********************************************************************
FUNCTION lfURetLin

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF


SELECT (lnWaSel2)
SET ORDER TO TAG RETLINE
lnTLinNo= 0
lcOldRT = CrMemo
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  IF m.CrMemo <> lcOldRT 
    lnTLinNo = 1
    lcOldRT = m.CrMemo
  ELSE
    lnTLinNo = lnTLinNo + 1
  ENDIF

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  *B603674,1 Add !EMPTY() condition in Link code update.
  REPLACE STYLE      WITH m.Style+'-'+m.Color,;
          GL_COST    WITH IIF(EMPTY(m.GL_Cost) OR m.GL_Cost='DEF','DEFDEF',m.GL_Cost),;
          GL_SALES   WITH IIF(EMPTY(m.GL_Sales) OR m.GL_Sales='DEF','DEFDEF',PADR(ALLT(m.GL_Sales),3)+'DEF'),;
          CSTYGRADE  WITH '1',;
          CRET_LINNO WITH padr(ALLT(STR(lnTLinNo)),4),;
          CRET_TRNCD WITH '2'

  UNLOCK

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
RETURN


*********************************************************************
* Update FORMCDHD.dbf
*********************************************************************
FUNCTION lfUFCdHdr

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open form code detail.
llOndFCD=gfOpenFile(gcDataDir+'FORMCDDT','Formcddt','SH')

*--Open 27 file with index.
SELECT (lnWaSel1)   && FORMCDhd file.
SET ORDER TO TAG Formcdhd

*--Aria 26 Form codes.
*-Fields ???Format or ???Name and InvFactor 
DIME laAForm[11,2]
laAForm=SPACE(1)
laAForm[1,1]  = 'Ord'    &&1)  Order
laAForm[1,2]  = 'SOORCN'
laAForm[2,1]  = 'Inv'    &&2)  Invoice
laAForm[2,2]  = 'ARPINV'
laAForm[3,1]  = 'Pik'    &&3)  PickTkt
laAForm[3,2]  = 'ALPKTK'
laAForm[4,1]  = 'Crm'    &&4)  Credit Memo
laAForm[4,2]  = 'RMCMEM'
laAForm[5,1]  = 'Pur'    &&5)  Style P/o
laAForm[5,2]  = 'POSTYP'
laAForm[6,1]  = 'MPur'   &&6)  Mat P/o
laAForm[6,2]  = 'MAMATP'
laAForm[7,1]  = 'Lbl'    &&7)  Shipping Label
laAForm[7,2]  = 'ARSHLB'
laAForm[8,1]  = 'Ret'    &&8)  R/A label
laAForm[8,2]  = 'RMRAUT'
laAForm[9,1]  = 'Stm'    &&9)  Statment
laAForm[9,2]  = 'ARCSTM'
laAForm[10,1] = 'Mai'    &&10) Mailing Label
laAForm[10,2]  = 'ARMLLB'
laAForm[11,1] = 'Cut'    &&11) Cutting Tiket
laAForm[11,2]  = 'MFPRCT'

FOR lnI=1 to 11
   SELECT (lnWaSel2)   && Control file.
   lc26FForm = laAForm[lnI,1]+'Format'
   lc26FFNam = laAForm[lnI,1]+'Name'
   lc26Form = &lc26FForm 
   lc26FNam = &lc26FFNam

   *--Note invoce form code 'A' becomes 'B','B' becomes 'C','C' becomes 'D',
   *--'D' becomes 'E','E' becomes 'F','F' becomes 'G','G' becomes 'H',
   *--'H' becomes 'I','I' becomes 'J','J' becomes 'K',
   *--'K' becomes 'A','L' becomes 'L'
   IF lnI=2 
     lc26Form = lfInvGtFrm(lc26Form)
   ENDIF

   *--Note piktkt form code 'B' becomes 'A'
   *--Note piktkt form code 'A' becomes 'B'
   IF lnI=3
     lc26Form = IIF(lc26Form='A','B',IIF(lc26Form='B','A',lc26Form))
   ENDIF


   lc27FmFld = laAForm[lnI,2]
   SELECT (lnWaSel1)   && FORMCDhd file.
   IF SEEK(lc27FmFld)   
     IF SEEK(lc27FmFld+lc26Form,'FORMCDDT')
       =RLOCK()
       REPLACE cCurForm  WITH lc26Form,;
               CADD_USER WITH 'CONVERSION'
       UNLOCK
     ENDIF
   ENDIF
ENDFOR
IF llOndFCD AND USED('FORMCDDT')
  USE IN FORMCDDT
ENDIF
RETURN

*******************
FUNCTION lfInvGtFrm
PARA lcInv26Frm
lcInvFrm = lcInv26Frm
DO CASE
  CASE lcInv26Frm = 'A'
    lcInvFrm = 'B'
  CASE lcInv26Frm = 'B'
    lcInvFrm = 'C'
  CASE lcInv26Frm = 'C'
    lcInvFrm = 'D'
  CASE lcInv26Frm = 'D'
    lcInvFrm = 'E'
  CASE lcInv26Frm = 'E'
    lcInvFrm = 'F'
  CASE lcInv26Frm = 'F'
    lcInvFrm = 'G'
  CASE lcInv26Frm = 'G'
    lcInvFrm = 'H'
  CASE lcInv26Frm = 'H'
    lcInvFrm = 'I'
  CASE lcInv26Frm = 'I'
    lcInvFrm = 'J'
  CASE lcInv26Frm = 'J'
    lcInvFrm = 'K'
  CASE lcInv26Frm = 'K'
    lcInvFrm = 'A'
  CASE lcInv26Frm = 'L'
    lcInvFrm = 'L'
ENDCASE
RETURN lcInvFrm

*********************************************************************
* Update RETAUTH.dbf
*********************************************************************
FUNCTION lfUpdRAt

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE CDIVISION WITH m.Division,;
          CINTR_INV WITH m.Complete,;
          CCURRCODE WITH gcBaseCurr,;
          NEXRATE   WITH 1,;
          NCURRUNIT WITH 1,;
          CRETNOTE1 WITH m.Note1,;
          CRETNOTE2 WITH m.Note2,;
          CRETNOTE3 WITH m.Note3,;
          CRETNOTE4 WITH m.Note4,;
          NRETA_BUD WITH m.Auth,;
          NRETA_REC WITH m.Return,;
          NRETA_OPN WITH MAX(m.Auth-m.Return,0),;
          NRTOPNAMT WITH MAX(m.AuthAmt-m.ReturnAmt,0)
  UNLOCK
 
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
RETURN


*********************************************************************
* Update RALINE.dbf
*********************************************************************
FUNCTION lfUpdRal

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

IF !lfOpn26Fl('RETAUTH','RETAUTH')
  RETURN
ENDIF
IF !lfOpn26Fl('RETLINE','RETLINE')
  RETURN
ENDIF


SELECT (lnWaSel2)
SET ORDER TO TAG Raline
lnTLinNo= 0
lcOldRT = RaNo
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  IF m.RaNo <> lcOldRT 
    lnTLinNo = 1
    lcOldRT = m.RaNo
  ELSE
    lnTLinNo = lnTLinNo + 1
  ENDIF

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE STYLE     WITH m.Style+'-'+m.Color,;
          CRA_LINNO WITH padr(ALLT(STR(lnTLinNo)),4)
  UNLOCK
  DIME laRaRcv[8]
  laRaRcv = 0
  IF SEEK(m.RaNo,'ARETAUTH') AND !EMPTY(ARETAUTH.Tran)
    IF SEEK(ARETAUTH.Tran+m.Style+m.Color,'ARETLINE')
      SELECT ARETLINE
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laRaRcv
    ENDIF
  ENDIF
  SELECT (lnWaSel1)
  =RLOCK() 
  REPLACE NOPNQTY1   WITH MAX(m.Qty1 - laRaRcv[1],0),;
          NOPNQTY2   WITH MAX(m.Qty2 - laRaRcv[2],0),;
          NOPNQTY3   WITH MAX(m.Qty3 - laRaRcv[3],0),;
          NOPNQTY4   WITH MAX(m.Qty4 - laRaRcv[4],0),;
          NOPNQTY5   WITH MAX(m.Qty5 - laRaRcv[5],0),;
          NOPNQTY6   WITH MAX(m.Qty6 - laRaRcv[6],0),;
          NOPNQTY7   WITH MAX(m.Qty7 - laRaRcv[7],0),;
          NOPNQTY8   WITH MAX(m.Qty8 - laRaRcv[8],0),;                                                  
          NTOTOPNQTY WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
  UNLOCK

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN ARETLINE
USE IN ARETAUTH
RETURN


*********************************************************************
* Update MFGOPRHD.dbf
*********************************************************************
FUNCTION lfUdMOph

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*B802033,1 (Start).
llOndVen=gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()


*B802700,1 We should not convert operations for Hold tickets because
*B802700,1 Generating cost sheet for this C/T will update the manufacturing
*B802700,1 Operation again.[Start]
*SCAN
SCAN FOR UPPER(STATUS) <> "H"
*B802700,1 Operation again.[End..]

  SCATTER MEMVAR
  SELECT (lnWaSel1)
  lnCntSeq = 1
  IF !EMPTY(m.Contr1)
    APPEND BLANK  
    =RLOCK() 

*B802700,1 Fix the wrong updating for the coperseq field [Start]
*B802700,1 just change COPERSEQ  WITH '0'+ALLT(STR(INT(lnCntSeq)))
*B802700,1 To          COPERSEQ  WITH     PADL(ALLT(STR(INT(lnCntSeq))),2)
*B802700,1 Fix the wrong updating coperseq field [End..]

    REPLACE CIMTYP    WITH 'M',;
            CTKTNO    WITH m.Cuttkt,;
            COPRCODE  WITH m.Mfg_opr1,;
            CITMMAJOR WITH m.Style,;
            COPERSEQ  WITH PADL(ALLT(STR(INT(lnCntSeq))),2),;
            COPRCOMNT WITH m.Note1,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.Contr1,;
            NNXTLOTNO WITH 1,;
            CCONTNAME WITH IIF(SEEK(m.Contr1,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
    lnCntSeq = lnCntSeq + 1
  ENDIF
  IF !EMPTY(m.Contr2)
    APPEND BLANK  
    =RLOCK() 

    REPLACE CIMTYP    WITH 'M',;
            CTKTNO    WITH m.Cuttkt,;
            COPRCODE  WITH m.Mfg_opr2,;
            CITMMAJOR WITH m.Style,;
            COPERSEQ  WITH PADL(ALLT(STR(INT(lnCntSeq))),2),;
            COPRCOMNT WITH m.Note2,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.Contr2,;
            NNXTLOTNO WITH 1,;
            CCONTNAME WITH IIF(SEEK(m.Contr2,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
    lnCntSeq = lnCntSeq + 1
  ENDIF
  IF !EMPTY(m.Contr3)
    APPEND BLANK  
    =RLOCK() 

    REPLACE CIMTYP    WITH 'M',;
            CTKTNO    WITH m.Cuttkt,;
            COPRCODE  WITH m.Mfg_opr3,;
            CITMMAJOR WITH m.Style,;
            COPERSEQ  WITH PADL(ALLT(STR(INT(lnCntSeq))),2),;
            COPRCOMNT WITH m.Note3,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.Contr3,;
            NNXTLOTNO WITH 1,;
            CCONTNAME WITH IIF(SEEK(m.Contr3,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
  ENDIF

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Close the vendor file if opened.
IF llOndVen
  USE IN APVENDOR
ENDIF
*B802033,1 (End).
RETURN


*********************************************************************
* Update MFGOPRDT.dbf
*********************************************************************
FUNCTION lfUdMOpd

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

IF !lfOpn26Fl('CUTTKTH','CUTTKTH')
  RETURN
ENDIF
IF !lfOpn26Fl('CUTTKTL','CUTTKTL')
  RETURN
ENDIF
*B802033,1 (Start) Open the vendor file.
llOndVen=gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
*B802033,1 (End).

SELECT (lnWaSel2)
SET ORDER TO TAG Conmgmt
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO
  lcTktSty=IIF(SEEK(m.Cuttkt,'ACUTTKTH'),ACUTTKTH.Style,'') 

  *B802700,1 We should not convert operations for Hold tickets because
  *B802700,1 Generating cost sheet for this C/T will update the manufacturing
  *B802700,1 Operation again.[Start]
  IF SEEK(m.Cuttkt,'ACUTTKTH') AND UPPER(ACUTTKTH.STATUS)="H"
    LOOP
  ENDIF
  *B802700,1 Operation again.[End..]

  DIME laCutQty[9]
  laCutQty=0
  SELECT ACUTTKTL
  IF SEEK(m.Cuttkt)
    SCAN WHILE Cuttkt=m.Cuttkt FOR TranCd='1'
      FOR lnI=1 TO 8
        lcZ=STR(lnI,1)
        laCutQty[lnI] = laCutQty[lnI] + Qty&lcZ
      ENDFOR
      laCutQty[9] = laCutQty[9] + TotQty
    ENDSCAN
    lnTmpTotal=0
    FOR lnI=1 TO 8
      laCutQty[lnI] = ROUND((m.TotQTY/laCutQty[9]) * laCutQty[lnI],0)
      lnTmpTotal = lnTmpTotal+laCutQty[lnI]
    ENDFOR
    IF lnTmpTotal <> m.TotQTY
       laCutQty[1] = laCutQty[1] + (m.TotQTY-lnTmpTotal)
    ENDIF
  ENDIF
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *B802033,1 Update the contractor name by vendor name.
  *--Update Differencies
  =RLOCK() 

  *B802817,1 KHM 11/23/99 (Begin) Removing cConUCode field from the replace.
  *REPLACE CIMTYP    WITH 'M',;
          CTKTNO    WITH m.Cuttkt,;
          COPRCODE  WITH m.Mfg_opr,;
          CLOTNO    WITH '01',;
          ITEM      WITH lcTktSty,;
          LINHOUSE  WITH .F.,;
          CCONUCODE WITH m.Contr,;
          CCONTNAME WITH IIF(SEEK(m.Contr,'APVENDOR'),APVENDOR.cVenComp,''),;
          TRANCD    WITH m.TranCd,;
          DTRANDATE WITH m.Date,;
          DUEDATE   WITH m.Date,;
          CTRGOPR   WITH '',;
          CTRGLOT   WITH IIF(m.Trancd <> '1','01',''),;
          NLOTQTY1  WITH laCutQty[1],;
          NLOTQTY2  WITH laCutQty[2],;
          NLOTQTY3  WITH laCutQty[3],;
          NLOTQTY4  WITH laCutQty[4],;
          NLOTQTY5  WITH laCutQty[5],;
          NLOTQTY6  WITH laCutQty[6],;
          NLOTQTY7  WITH laCutQty[7],;
          NLOTQTY8  WITH laCutQty[8],;
          NLOTTOTQTY WITH m.TotQTY 

  REPLACE CIMTYP    WITH 'M',;
          CTKTNO    WITH m.Cuttkt,;
          COPRCODE  WITH m.Mfg_opr,;
          CLOTNO    WITH '01',;
          ITEM      WITH lcTktSty,;
          LINHOUSE  WITH .F.,;
          CCONTNAME WITH IIF(SEEK(m.Contr,'APVENDOR'),APVENDOR.cVenComp,''),;
          TRANCD    WITH m.TranCd,;
          DTRANDATE WITH m.Date,;
          DUEDATE   WITH m.Date,;
          CTRGOPR   WITH '',;
          CTRGLOT   WITH IIF(m.Trancd <> '1','01',''),;
          NLOTQTY1  WITH laCutQty[1],;
          NLOTQTY2  WITH laCutQty[2],;
          NLOTQTY3  WITH laCutQty[3],;
          NLOTQTY4  WITH laCutQty[4],;
          NLOTQTY5  WITH laCutQty[5],;
          NLOTQTY6  WITH laCutQty[6],;
          NLOTQTY7  WITH laCutQty[7],;
          NLOTQTY8  WITH laCutQty[8],;
          NLOTTOTQTY WITH m.TotQTY 
  *B802817,1 KHM 11/23/99 (End)
  UNLOCK
 
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B802033,1 Close the vendor file if opened.
IF llOndVen
  USE IN APVENDOR
ENDIF
*B802033,1 (End).
USE IN ACUTTKTL
USE IN ACUTTKTH
RETURN


*B802076,1 Change the updating the BOMCOST from engen to function.
*********************************************************************
* Update BOMCOST.dbf
*********************************************************************
FUNCTION LFUPBMCT

*B802078,1 Continue conversion if the following files was not opened.
IF !lfOpn26Fl('CUTTKTH','CUTTKTH',.T.)
  *RETURN
ENDIF
IF !lfOpn26Fl('POSHDR','POSHDR',.T.)
  *RETURN
ENDIF
*B802076,1 Open Gl file to read the account.
llNoGlInfo = .F.
IF !lfOpn26Fl('GL_LINK','GL_LINK',.T.)
  llNoGlInfo = .T.
  *RETURN
ENDIF

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE ITEM      WITH IIF(cCostType='S',m.Item+'-'+m.IClr,m.Item),;
          ICLR      WITH IIF(cCostType='S',' ',m.IClr),;
          MFGCODE   WITH IIF(cCostType$'PD',REPL(SUBSTR(m.MfgCode,1,1),6),m.MfgCode),;
          ACTUALIZE WITH 'Y'

  IF cIMTyp = 'M' AND SEEK(m.ctktno,'ACUTTKTH')
    IF SEEK(ACUTTKTH.Link_Code+'013','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      =RLOCK() 
      REPLACE cWipAcnt  WITH AGL_LINK.GLAcnt
      UNLOCK
    ENDIF
  ENDIF

  IF cIMTyp = 'I' AND SEEK(m.ctktno,'APOSHDR')
    IF SEEK(APOSHDR.Link_Code+'013','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
      REPLACE cWipAcnt  WITH AGL_LINK.GLAcnt
    ENDIF
  ENDIF

  *B802076,1 Read the GL accounts if file was opened.
  IF llNoGlInfo AND SEEK('DEF'+'018','AGL_LINK') AND !EMPTY(AGL_LINK.GLAcnt)
    REPLACE CLBLTYACNT WITH AGL_LINK.GLAcnt
  ENDIF
  UNLOCK

  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

IF USED('AGL_LINK')
  USE IN AGL_LINK
ENDIF
IF USED('ACUTTKTH')
  USE IN ACUTTKTH
ENDIF
IF USED('APOSHDR')
  USE IN APOSHDR
ENDIF
RETURN 


*E500259,1 Added.
*********************************************************************
* Update EDINET  fields COUTFILE and CINFILE  -> Parameter 'OUT','IN'
*********************************************************************
FUNCTION lfvEdiNt
PARA lcIn_Out
lcIOValue = ''
IF lcIn_Out='IN'
  DO CASE
     CASE EMPTY(m.Income)
       *B803706,1 NAD 10/03/00 (START) Change the default incoming file name.
       *lcIOValue = 'income.edi'
       lcIOValue = ALLTRIM(m.Net_Name)+'in.edi'
       *B803706,1 NAD (End)
     *B803706,1 NAD 10/03/00 (START) Change the default incoming file name.
     *CASE 'GETFILE' $ m.Income
       *lcIOValue = 'income.edi'
     CASE 'GETFILE' $ UPPER(m.Income)      
       lcIOValue = ALLTRIM(m.Net_Name)+'in.edi'
       *B803706,1 NAD 10/03/00 (End)
     CASE OCCURS('\',m.Income) <> 0
       lcIOValue = ALLT(SUBSTR(m.Income,ATC('\',m.Income,OCCURS('\',m.Income))+1))
     OTHE
       lcIOValue = ALLT(m.Income)
  ENDCASE
ELSE   && 'OUT'
  DO CASE
     CASE EMPTY(m.outgouing)
       *B803706,1 NAD 10/03/00 (START)  Change the default outgoing file name.      
       *lcIOValue = 'out.edi'
       lcIOValue = ALLTRIM(m.Net_Name)+'out.edi'
       *B803706,1 NAD 10/03/00 (End)
     *B803706,1 NAD 10/03/00 (START)  Change the default outgoing file name.       
     *CASE 'GETFILE' $ m.outgouing            
       *lcIOValue = 'out.edi'
     CASE 'GETFILE' $ UPPER(m.outgouing)      
       lcIOValue = ALLTRIM(m.Net_Name)+'out.edi'
     *B803706,1 NAD 10/03/00 (End)
     CASE OCCURS('\',m.outgouing) <> 0
       lcIOValue = ALLT(SUBSTR(m.outgouing,ATC('\',m.outgouing,OCCURS('\',m.outgouing))+1))
     OTHE
       lcIOValue = ALLT(m.outgouing)
  ENDCASE
ENDIF
RETURN lcIOValue

*********************************************************************
* Update EDISTORE   fields '1'-> CEDITRNTYP or '2'-> CPARTCODE 
*********************************************************************
FUNCTION lfvUEdiSt
PARA lcWichFld

lcRValue = ''
IF SEEK(m.Account,'AEDIACC')
  IF !EMPTY(AEDIACC.Txt_P_Id)
    IF lcWichFld = '1'
      lcRValue = '864'
    ELSE
      lcRValue = IIF(SEEK(AEDIACC.Txt_P_Id,'SYCEDIPD'),SYCEDIPD.cPartCode,'')
    ENDIF
  ELSE
    IF !EMPTY(AEDIACC.Org_Id)
      IF lcWichFld = '1'
        lcRValue = '816'
      ELSE
        lcRValue = IIF(SEEK(AEDIACC.Org_Id,'SYCEDIPD'),SYCEDIPD.cPartCode,'')
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN lcRValue


****************************************************************************
* Update EDIACPRT.DBF , EDIPH.DBF , EDIPD.DBF files releated to EDIACC file.
****************************************************************************
FUNCTION lfEDIAcc

*--Zap EDI PH,PD files before conversion.
=gfOpenFile(gcDataDir+'EDIPH','','EX')
SELECT EDIPH
ZAP
=gfOpenFile(gcDataDir+'EDIPD','','EX')
SELECT EDIPD
ZAP
*--Open help system files needed in conversion.
=gfOpenFile(gcSysHome+'SYCEDIPH','Partner','SH')
=gfOpenFile(gcSysHome+'SYCEDIPD','Partid' ,'SH')
*B803695,1 AMM Open the customer table to update the Int_vend field
llOpCust = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER' ,'SH')
*B803695,1 AMM end
*--Open 2.6 help file.
=lfOpn26Fl('CUSTOMER','CUSTOMER')
lcPathA26 = lcA26lib
=lfOpn26Fl('FILD2SEG','FILD2SEG')
=lfOpn26Fl('EDI_ASN','EDI_ASN')
=lfOpn26Fl('SEG2FILD','SEG2FILD')
=lfOpn26Fl('EDINET'  ,'EDINET'  )
lcPathA26 = lcA26Data

*--Define Edi Accounts array. 
DECLARE laEdiAcc[10,3]
laEdiAcc = ' '
=lfDefEDIA()

SELECT (lnWaSel2)   && 2.6 EDIACC.dbf
*--Start updating , it will update 3 files.
lnMax1 = RECCOUNT() 
SCAN
  SCATTER MEMVAR MEMO
  
  *--Get needed information.
  lcType     = IIF(SEEK('M'+m.Account,'ACUSTOMER'),'A',IIF(SEEK('F'+m.Account,'ACUSTOMER'),'F','N'))
  *B803695,1 AMM 08/25/2000 If the three Id's empty, update the error report then continue next account
  *lcPrt_ACod = IIF(!EMPTY(m.PO_P_Id),m.PO_P_Id,IIF(!EMPTY(m.Inv_P_Id),m.Inv_P_Id,m.Asn_P_Id))
  *lcPartCode = IIF(SEEK(lcPrt_ACod,'SYCEDIPD'),SYCEDIPD.cPartCode,m.Accout)
  lcPrt_ACod = IIF(!EMPTY(m.PO_P_Id),m.PO_P_Id,IIF(!EMPTY(m.Inv_P_Id),m.Inv_P_Id, IIF(!EMPTY(m.Asn_P_Id),m.Asn_P_Id,m.Ucc_recvid) ) )
  IF !SEEK(m.PO_P_Id,'SYCEDIPD') .AND. !SEEK(m.Inv_P_Id,'SYCEDIPD') .AND. !SEEK(m.Asn_P_Id,'SYCEDIPD') .AND. !SEEK(m.Ucc_recvid,'SYCEDIPD') 
    =lfUpdError('Account '+m.Account+" was not updated in EDI files. Invalid transaction's ID# "+lcPrt_ACod+".")
    LOOP
  ELSE
    lcPartCode = SYCEDIPD.cPartCode
  ENDIF
  *B803695,1 AMM end
  
  lcAC_Ver   = IIF(!EMPTY(m.Inv_Ver),m.Inv_Ver,IIF(!EMPTY(m.Asn_Ver),m.Asn_Ver,m.Po_Ver)) 
  *B803695,1 AMM Default the qualifier with 12
  *lcCmpIsaql = ''  
  lcCmpIsaql = '12'  
  *B803695,1 AMM end
  
  lcIntChgVer= ''
  lcVersion  = ''
  =SEEK(lcPartCode,'SYCEDIPH')

  *B803695,1 AMM (start) get the network
  *m.NetWork  = IIF(!EMPTY(m.Network),m.Network,SYCEDIPH.cNetWork)
  *=SEEK(m.NetWork,'AEDINET')
  *SELECT AFILD2SEG
  *B803695,1 AMM get the network that this account uses.
  SELECT AFILD2SEG
  IF EMPTY(m.NetWork)
    LOCATE FOR network+ver+loop_id+STR(f_order,2)+STR(RECNO(),7) = network+lcAC_Ver
    m.Network = IIF(FOUND(), network ,SYCEDIPH.cNetWork)
  ENDIF
  =SEEK(m.NetWork,'AEDINET')
  *B803695,1 AMM end

  IF SEEK(m.Network+lcAC_Ver+'01 '+' 1')
    LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'01 '+' 1' ;
           FOR ALLT(F_Name) = "XFIELD(5)"
    lcCmpIsaql = EVAL(F_Value)
    =SEEK(m.Network+lcAC_Ver+'01 '+' 1')
    LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'01 '+' 1' ;
           FOR ALLT(F_Name) = "XFIELD(12)"
    lcIntChgVer = EVAL(F_Value)

    =SEEK(m.Network+lcAC_Ver+'02 '+' 1')
    LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'02 '+' 1' ;
           FOR ALLT(F_Name) = "XFIELD(8)"
    lcVersion  = EVAL(F_Value)
  ELSE
    SELECT AEDI_ASN
    IF SEEK(m.Network+lcAC_Ver+'01 '+' 1')
      LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'01 '+' 1' ;
             FOR ALLT(F_Name) = "XFIELD(5)"
      lcCmpIsaql = EVAL(F_Value)
      =SEEK(m.Network+lcAC_Ver+'01 '+' 1')
      LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'01 '+' 1' ;
             FOR ALLT(F_Name) = "XFIELD(12)"
      lcIntChgVer = EVAL(F_Value)
      =SEEK(m.Network+lcAC_Ver+'01 '+' 2')
      LOCATE REST WHILE network+ver+loop_id+STR(f_order,2)=m.Network+lcAC_Ver+'01 '+' 2' ;
             FOR ALLT(F_Name) = "XFIELD(8)"
      lcVersion  = EVAL(F_Value)
    ENDIF  
  ENDIF
  lcIntChgVer = IIF(EMPTY(lcIntChgVer),SYCEDIPH.cIntChgVer,lcIntChgVer)
  lcVersion   = IIF(EMPTY(lcVersion),SYCEDIPH.cVersion,lcVersion)
  
  *- 1st file update.
  SELECT (lnWaSel1)    && EDIACPRT.dbf
  APPEND BLANK
  *B803695,1 AMM fix the bug in updating the segment ID field
  *REPLACE Type       WITH lcType,;
          cPartner   WITH m.Account,;
          cPartCode  WITH lcPartCode,;
          cCmpIsaql  WITH lcCmpIsaql,;
          cCmpIsaId  WITH m.User_Id,;
          cCmpGsId   WITH m.User_Id_Gs,;
          cRemit     WITH m.RemtNo,;
          cEdiFob    WITH m.FOB,;
          lDetLabel  WITH m.lDetLabel
  *B803707,1 NAD 10/03/00 (Start) Field lJcpStDash is missing during the conversion of EDIACPRT .        
  *REPLACE Type       WITH lcType,;
          cPartner   WITH m.Account,;
          cPartCode  WITH lcPartCode,;
          cCmpIsaql  WITH lcCmpIsaql,;
          cCmpIsaId  WITH m.User_Id,;
          cCmpGsId   WITH IIF(EMPTY(m.User_Id_Gs),m.User_Id,m.User_Id_Gs)  ,;
          cRemit     WITH m.RemtNo,;
          cEdiFob    WITH m.FOB,;
          lDetLabel  WITH m.lDetLabel
          
  REPLACE Type       WITH lcType,;
          cPartner   WITH m.Account,;
          cPartCode  WITH lcPartCode,;
          cCmpIsaql  WITH lcCmpIsaql,;
          cCmpIsaId  WITH m.User_Id,;
          cCmpGsId   WITH IIF(EMPTY(m.User_Id_Gs),m.User_Id,m.User_Id_Gs)  ,;
          cRemit     WITH m.RemtNo,;
          cEdiFob    WITH m.FOB,;
          lDetLabel  WITH m.lDetLabel,;
          lJcpStDash WITH m.Store_Dash       
  *B803808,1 WAM Update ship to duns number
  REPLACE duns WITH m.ShipToDuns
  *B803808,1 (End)
  
  *B803707,1 NAD 10/03/00 (END)       
  *B803695,1 AMM end
  *B803695,1 AMM Update the vendor no in the customer file
  IF SEEK('M'+m.Account,'CUSTOMER')
    SELECT CUSTOMER
    REPLACE cCusVend  WITH m.Vend
  ENDIF
  *B803695,1 AMM end
  *- 2nd file update.
  SELECT EDIPH
  APPEND BLANK
  *B803695,1 AMM put default values to field and line separator
  *REPLACE cPartCode  WITH lcPartCode,;
          cPartName  WITH SYCEDIPH.cPartName,;
          cIntChgVer WITH lcIntChgVer,;
          cVersion   WITH lcVersion ,;
          cNetWork   WITH m.NetWork,;
          cFieldSep  WITH AEDINET.Field_Sep,;
          cLineSep   WITH AEDINET.Line_Sep,;
          nIntChgSeq WITH m.Edi_Isa,;
          nGroupSeq  WITH m.Edi_Gs,;
          nAckSeq    WITH m.Ack_Seq,;
          cAsnLbl1   WITH m.Asn_Ver,;
          cAsnLbl2   WITH m.DtcLblVer,;
          cCrtnType  WITH IIF(m.StandCrtPk='Y','S','P'),;
          lPltShp    WITH m.lShipInplt,;
          cPltLbl    WITH m.cPltLblVer,;
          lSendAck   WITH IIF(m.NetWork='EXP',.T.,.F.)

  REPLACE cPartCode  WITH lcPartCode,;
          cPartName  WITH SYCEDIPH.cPartName,;
          cIntChgVer WITH lcIntChgVer,;
          cVersion   WITH lcVersion ,;
          cNetWork   WITH m.NetWork,;
          cFieldSep  WITH IIF(EMPTY(AEDINET.Field_Sep),"CHR(42)",AEDINET.Field_Sep) ,;
          cLineSep   WITH IIF(EMPTY(AEDINET.Line_Sep),"''",AEDINET.Line_Sep),;
          nIntChgSeq WITH m.Edi_Isa,;
          nGroupSeq  WITH m.Edi_Gs,;
          nAckSeq    WITH m.Ack_Seq,;
          cAsnLbl1   WITH m.Asn_Ver,;
          cAsnLbl2   WITH m.DtcLblVer,;
          cCrtnType  WITH IIF(m.StandCrtPk='Y','S','P'),;
          lPltShp    WITH m.lShipInplt,;
          cPltLbl    WITH m.cPltLblVer,;
          lSendAck   WITH IIF(m.NetWork='EXP',.T.,.F.)
  *B803695,1 AMM end

  *- 3nd file update.
  SELECT EDIPD
  FOR lnI=1 TO 10  
    *-Coresponding field.
    lcEdiAtyp = 'm.'+ALLT(laEdiAcc[lnI,1])
    lcPartId  = EVAL(lcEdiAtyp)
    *--If empty no part id line will be created.
    IF EMPTY(lcPartId)
      LOOP
    ENDIF
    *-Coresponding field.
    lcTrPSeq = IIF(!EMPTY(laEdiAcc[lnI,3]) , 'm.'+ALLT(laEdiAcc[lnI,3]) , '')    
    lcMapSet = 'm.'+SUBSTR(laEdiAcc[lnI,1],1,ATC('_',laEdiAcc[lnI,1]))+'VER'

    APPEND BLANK
    REPLACE cPartCode  WITH lcPartCode,;
            cEDITrnTyp WITH laEdiAcc[lnI,2],;
            cTranActv  WITH m.Testind,;
            lTrade     WITH .T.,;
            cVersion   WITH lcVersion ,;
            cPartQual  WITH SYCEDIPD.cPartQual,;
            cPartId    WITH lcPartId,;
            cPartGsId  WITH lcPartId ,;
            nTranSeq   WITH IIF(!EMPTY(lcTrPSeq),EVAL(lcTrPSeq),0),;
            cMapSet    WITH EVAL(lcMapSet)
    IF laEdiAcc[lnI,2] = '850' AND SEEK(m.NetWork+EVAL(lcMapSet)+'BCH','ASEG2FILD')
      APPEND BLANK
      REPLACE cPartCode  WITH lcPartCode,;
              cEDITrnTyp WITH '860' ,;
              cTranActv  WITH m.Testind,;
              lTrade     WITH .T.,;
              cVersion   WITH lcVersion ,;
              cPartQual  WITH SYCEDIPD.cPartQual,;
              cPartId    WITH lcPartId,;
              cPartGsId  WITH lcPartId,;
              nTranSeq   WITH IIF(!EMPTY(lcTrPSeq),EVAL(lcTrPSeq),0),;
              cMapSet    WITH EVAL(lcMapSet)
    ENDIF
  ENDFOR

  *B804459,1 TMI [Start] Add a line for 997 for the current partner
  lcPartId = SPACE(0)
  FOR lnI = 1 TO 10
    lcPartId  = EVAL('m.'+ALLT(laEdiAcc[lnI,1]))
    IF !EMPTY(lcPartId)
      EXIT
    ENDIF
  ENDFOR
  lcOrder = ORDER()
  SET ORDER TO TAG PARTTRANS
  IF !SEEK(lcPartCode+'997')
    APPEND BLANK 
    REPLACE cPartCode  WITH lcPartCode,;
            cEDITrnTyp WITH '997',;
            cTranActv  WITH m.Testind,;
            lTrade     WITH .T.,;
            cVersion   WITH lcVersion ,;
            cPartQual  WITH SYCEDIPD.cPartQual,;
            cPartId    WITH lcPartId,;
            cPartGsId  WITH lcPartId ,;
            nTranSeq   WITH m.ACK_SEQ,;
            cMapSet    WITH EVAL(lcMapSet)
  ENDIF
  SET ORDER TO TAG &lcOrder
  *B804459,1 TMI [End  ] Add a line for 997 for the current partner

  SELECT (lnWaSel2)
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Close opened files.
IF USED('EDIPH')
  USE IN EDIPH
ENDIF
IF USED('EDIPD')
  USE IN EDIPD
ENDIF
IF USED('SYCEDIPH')
  USE IN SYCEDIPH
ENDIF
IF USED('SYCEDIPD')
  USE IN SYCEDIPD
ENDIF
IF USED('ACUSTOMER')
  USE IN ACUSTOMER
ENDIF
IF USED('AFILD2SEG')
  USE IN AFILD2SEG
ENDIF
IF USED('AEDI_ASN')
  USE IN AEDI_ASN
ENDIF
IF USED('ASEG2FILD')
  USE IN ASEG2FILD
ENDIF
IF USED('AEDINET')
  USE IN AEDINET
ENDIF
*B803695,1 AMM close the file
IF llOpCust .AND. USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
*B803695,1 AMM end
RETURN

****************************************************************************
* Define EDIACC Array -> laEdiAcc
****************************************************************************
FUNCTION lfDefEDIA

laEdiAcc[1,1]  = 'PO_P_ID'
laEdiAcc[1,2]  = '850'
laEdiAcc[1,3]  = ''

laEdiAcc[2,1]  = 'PD_P_ID'
laEdiAcc[2,2]  = '852'
laEdiAcc[2,3]  = ''

laEdiAcc[3,1]  = 'INV_P_ID'
laEdiAcc[3,2]  = '810'
laEdiAcc[3,3]  = 'INV_SEQ'

laEdiAcc[4,1]  = 'ASN_P_ID'
laEdiAcc[4,2]  = '856'
laEdiAcc[4,3]  = 'ASN_SEQ'

laEdiAcc[5,1]  = 'BOL_P_ID'
laEdiAcc[5,2]  = '204'
laEdiAcc[5,3]  = 'BOL_SEQ'

laEdiAcc[6,1]  = 'TXT_P_ID'
laEdiAcc[6,2]  = '864'
laEdiAcc[6,3]  = 'TXT_SEQ'

laEdiAcc[7,1]  = 'AA_P_ID'
laEdiAcc[7,2]  = '822'
laEdiAcc[7,3]  = ''

laEdiAcc[8,1]  = 'RO_ID'
laEdiAcc[8,2]  = '855' 
laEdiAcc[8,3]  = 'RO_SEQ'

laEdiAcc[9,1]  = 'ORG_ID'
laEdiAcc[9,2]  = '816'
laEdiAcc[9,3]  = ''

laEdiAcc[10,1] = 'UCC_RECVID'
laEdiAcc[10,2] = '832'
laEdiAcc[10,3] = 'UCC_SEQ'
RETURN


****************************************************************************
* Update EDILIBHD.DBF and EDILIBDT.DBF files  releated to EDIOUT file.
* parameters   'H' to update EDILIBHD  ,and  'D' to update EDILIBDT .
****************************************************************************
FUNCTION lfEDIOut
PARA lcFlToUpd

return
***

DO CASE
  CASE lcFlToUpd = 'H'
    SELECT (lnWaSel2)   && 2.6 EDIOUT.dbf
    *--Start updating , it will update 3 files.
    lnMax1 = RECCOUNT() 
    SCAN
      SCATTER MEMVAR MEMO

      SELECT (lnWaSel1)    && 2.7 file.
*      APPEND BLANK
*     REPLACE CFILECODE   Character      6       
*              CEDIFILNAM  Character     12       
*              CFILEPATH   Character     60       
*              CEDIFILTYP  Character      1       
*              DDATE       Date           8       
*              CTIME       Character     11       
*              CNETWORK    Character      3       
*              CSTATUS     Character      1       
*              NTRANNUMB   Numeric        6       
*              NREJTRAN    Numeric        6       
*              NPROCTRAN   Numeric        6       
*              MACKTEXT    Memo          10       
*              CPROCESSBY  Character     10       

      
      =lfThrmo(lnCurNm1,lnMax1,'1')
    ENDSCAN

  CASE lcFlToUpd = 'D'

*          CEDIFILTYP  Character      1       
*          CFILECODE   Character      6       
*          CPARTCODE   Character      6       
*          CINTCHGSEQ  Character      9       
*          CGROUPSEQ   Character      9       
*          CTRANSEQ    Character      9       
*          CEDITRNTYP  Character      3       
*          CEDITRANNO  Character      6       
*          CEDIREF     Character     30       
*          CRECFLDSEP  Character      1       
*          CACKSTATUS  Character      1       
*          DDATE       Date           8       
*          CTIME       Character     11       
*          CREJREASON  Character      5       
*          CGSREJREAS  Character      5       
*          CEDITRNST   Character      1       
*          CSTATUS     Character      1       
*          MTRANTEXT   Memo          10       
*          MSTATUS     Memo          10       
*          DACKDATE    Date           8       


ENDCASE
RETURN
*E500259,1 End.









******************************************************
* C U S T O M  F I L E S  U P D A T E S.
******************************************************


*E:300630 SSH (BEGIN) Add function to convert robpofhdr
*********************************************************************
* Update CUSTOMER POFHDR.dbf For ROB100
*********************************************************************
FUNCTION lfURMPoH

*--Call standard pofhdr update.
=lfUMPoHd()

IF !lfOpn26Fl('ROBPOFHD','POFHDR')
  RETURN
ENDIF

SELECT AROBPOFHD
lnMax1 = RECCOUNT()
lnCurNm1 =1
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 
  REPLACE CMATTYPE   WITH 'R' ,;
          POMAT      WITH m.PO,;
          NFABCANCEL WITH m.Cancel,;
          NFABDAMAGE WITH m.Damage,;
          NPO_OPEN   WITH m.Open,;
          NFABORDER  WITH m.Order,;
          NFBRECEIVE WITH m.Receive,;
          CDIVISION  WITH m.Division,;
          CTERMCODE  WITH m.Terms,;
          CFOB       WITH m.Fob,;
          CFAB_LC    WITH m.LC,;
          LINK_CODE  WITH 'DEFDEF',; 
          CFABGRADE  WITH '1',; 
          CPRICECUR  WITH IIF(m.Currency=laCurrncy[lnCurrncy],gcBaseCurr,m.Currency),;
          CDUTYCUR   WITH CPRICECUR,;
          CCURRCODE  WITH CPRICECUR,;
          NPRICERAT  WITH m.Rate,;
          NDUTYRAT   WITH m.Rate,;
          NCURRUNIT  WITH 1

  REPLACE COUTADDR1  WITH m.ShpAddr1,;
          COUTADDR2  WITH m.ShpAddr2,;
          COUTADDR3  WITH m.ShpCity,;
          COUTADDR4  WITH m.ShpState,;
          COUTADDR5  WITH m.ShpZip,;
          CCONT_CODE WITH gcContCode,;
          CWARECODE  WITH IIF(!llMultiwh AND EMPTY(m.cWareCode),'DEFWHE',m.cWareCode)

  IF m.Rate = 1
    REPLACE NEACTCOST1 WITH nAct_Cost1,; 
            NEACTCOST2 WITH nAct_Cost2,; 
            NEACTCOST3 WITH nAct_Cost3,; 
            NEACTCOST4 WITH nAct_Cost4,; 
            NECOST1    WITH nCost1,;
            NECOST2    WITH nCost2,;
            NECOST3    WITH nCost3,;
            NECOST4    WITH nCost4,;
            NELANCOST1 WITH nLan_Cost1,; 
            NELANCOST2 WITH nLan_Cost2,; 
            NELANCOST3 WITH nLan_Cost3,; 
            NELANCOST4 WITH nLan_Cost4
  ELSE
    REPLACE NEACTCOST1 WITH ROUND(nAct_Cost1*m.Rate,1),; 
            NEACTCOST2 WITH ROUND(nAct_Cost2*m.Rate,1),; 
            NEACTCOST3 WITH ROUND(nAct_Cost3*m.Rate,1),; 
            NEACTCOST4 WITH ROUND(nAct_Cost4*m.Rate,1),; 
            NECOST1    WITH ROUND(nCost1*m.Rate,1),;
            NECOST2    WITH ROUND(nCost2*m.Rate,1),;
            NECOST3    WITH ROUND(nCost3*m.Rate,1),;
            NECOST4    WITH ROUND(nCost4*m.Rate,1),;
            NELANCOST1 WITH ROUND(nLan_Cost1*m.Rate,1),; 
            NELANCOST2 WITH ROUND(nLan_Cost2*m.Rate,1),; 
            NELANCOST3 WITH ROUND(nLan_Cost3*m.Rate,1),; 
            NELANCOST4 WITH ROUND(nLan_Cost4*m.Rate,1)
  ENDIF
  UNLOCK
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN AROBPOFHD
RETURN
*E:300630 SSH (END)

*E:300630 SSH (BEGIN) Add function to convert robpofline
*********************************************************************
* Update CUSTOMER POFLN.dbf For ROB100.
*********************************************************************
FUNCTION lfURMPoL

*--Call standard pofln update.
=lfUMPoLn()

IF !lfOpn26Fl('ROBPOFLN','POFLN') OR !lfOpn26Fl('ROBPOFHD','POFHDR')
  RETURN
ENDIF

SELECT AROBPOFLN
lnMax1 = RECCOUNT()
lnCurNm1 =1

lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO
  lnRate=IIF(SEEK(m.PO,'AROBPOFHD') AND AROBPOFHD.Rate>0 ,AROBPOFHD.Rate,1)
  SELECT (lnWaSel1)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *--Update Differencies
  =RLOCK() 

  REPLACE CMATTYPE  WITH 'R' ,;
          POMAT     WITH m.PO,;
          CFABGRADE WITH '1',;
          DPOSTDATE WITH m.Date,;
          cWareCode WITH IIF(!llMultiwh,'DEFWHE',IIF(EMPTY(cWareCode),AROBPOFHD.cWareCode,cWareCode)),;
          nAct_Cost1 WITH m.nAct_Cost1,;
          nAct_Cost2 WITH m.nAct_Cost2,;
          nAct_Cost3 WITH m.nAct_Cost3,;
          nAct_Cost4 WITH m.nAct_Cost4,;
          nLan_Cost1 WITH m.nLan_Cost1,;  
          nLan_Cost2 WITH m.nLan_Cost2,;  
          nLan_Cost3 WITH m.nLan_Cost3,;              
          nLan_Cost4 WITH m.nLan_Cost4,;
          nFabTotQty WITH m.TotQty
  IF lnRate=1
    REPLACE NEACTCOST1 WITH m.nAct_Cost1,;
            NEACTCOST2 WITH m.nAct_Cost2,;  
            NEACTCOST3 WITH m.nAct_Cost3,;  
            NEACTCOST4 WITH m.nAct_Cost4,;  
            NELANCOST1 WITH m.nLAN_Cost1,;
            NELANCOST2 WITH m.nLAN_Cost2,;  
            NELANCOST3 WITH m.nLAN_Cost3,;  
            NELANCOST4 WITH m.nLAN_Cost4,;  
            NECOST1    WITH m.nCost1,;
            NECOST2    WITH m.nCost2,;
            NECOST3    WITH m.nCost3,;
            NECOST4    WITH m.nCost4
  ELSE
    REPLACE NEACTCOST1 WITH ROUND(m.nAct_Cost1*lnRate,1),;
            NEACTCOST2 WITH ROUND(m.nAct_Cost2*lnRate,1),;  
            NEACTCOST3 WITH ROUND(m.nAct_Cost3*lnRate,1),;  
            NEACTCOST4 WITH ROUND(m.nAct_Cost4*lnRate,1),;  
            NELANCOST1 WITH ROUND(m.nLAN_Cost1*lnRate,1),;
            NELANCOST2 WITH ROUND(m.nLAN_Cost2*lnRate,1),;  
            NELANCOST3 WITH ROUND(m.nLAN_Cost3*lnRate,1),;  
            NELANCOST4 WITH ROUND(m.nLAN_Cost4*lnRate,1),;  
            NECOST1    WITH ROUND(m.nCost1*lnRate,1),;
            NECOST2    WITH ROUND(m.nCost2*lnRate,1),;
            NECOST3    WITH ROUND(m.nCost3*lnRate,1),;
            NECOST4    WITH ROUND(m.nCost4*lnRate,1)
  ENDIF 
  UNLOCK
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
USE IN AROBPOFHD
USE IN AROBPOFLN
RETURN


*E:300630 SSH(BEGIN)Add function to convert contractor manegment Detail file.
*********************************************************************
* Update CUSTOMER poconmgm.dbf For ROB100.
*********************************************************************
FUNCTION lfURMopD

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

*--Call standard POConMgM update.
=lfUdMOpd()

IF !lfOpn26Fl('poshdr','poshdr')
  RETURN
ENDIF
IF !lfOpn26Fl('posln','posln')
  RETURN
ENDIF
IF !lfOpn26Fl('PoConMgm','PoConMgm')
  RETURN
ENDIF

lcTmpCur = gfTempName()
SELECT APoConMgm
lnMax1 = RECCOUNT()
lnCurNm1 =1
SCAN
  CREATE CURSOR &lcTmpCur ;
  (Cpo C(6),cSTYLE C(12),QTY1 N(10) ,QTY2 N(10) ,QTY3 N(10) ,QTY4 N(10),;
   QTY5 N(10) , QTY6 N(10) , QTY7 N(10) , QTY8 N(10),TOTQTY N(12))
  INDEX ON cPo+cSTYLE TAG (lcTmpCur)
  *--- Start Select Po's Lines
  SELECT APosLn
  IF SEEK(APoConMgm.Po)
    SCAN WHILE APosLn.Po = APoConMgm.Po FOR TranCd = '1'
      SELECT(lcTmpCur)
      IF !SEEK(APosLn.PO+APosLn.Style,lcTmpCur)
        APPEND BLANK
        REPLACE cSTYLE WITH APosLn.Style,;
                cPo    WITH APosLn.PO
      ENDIF
      REPLACE Qty1 WITH Qty1 + APosLn.Qty1 ,;
              Qty2 WITH Qty2 + APosLn.Qty2 ,;
              Qty3 WITH Qty3 + APosLn.Qty3 ,;
              Qty4 WITH Qty4 + APosLn.Qty4 ,;
              Qty5 WITH Qty5 + APosLn.Qty5 ,;
              Qty6 WITH Qty6 + APosLn.Qty6 ,;
              Qty7 WITH Qty7 + APosLn.Qty7 ,;
              Qty8 WITH Qty8 + APosLn.Qty8 ,;
              TotQty WITH TotQty+APosLn.TotQty
    ENDSCAN
  ELSE
    LOOP
  ENDIF

  SELECT (lcTmpCur)
  SUM ALL TOTQTY TO lnTotPo
  SCAN
    SELECT APoConMgm
    SCATTER MEMVAR MEMO
    SELECT (lnWaSel1)
    APPEND BLANK
    *--Update Differencies
    =RLOCK() 
    REPLACE CIMTYP    WITH 'I',;
            CTKTNO    WITH m.PO,;
            COPRCODE  WITH m.Mfg_opr,;
            CLOTNO    WITH '01',;
            ITEM      WITH &lcTmpCur..cStyle,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.Contr,;
            CCONTNAME WITH '',;
            TRANCD    WITH m.TranCd,;
            DTRANDATE WITH m.Date,;
            DUEDATE   WITH m.Date,;
            CTRGOPR   WITH '',;
            CTRGLOT   WITH IIF(m.Trancd<>'1','01',''),;
            NLOTQTY1  WITH &lcTmpCur..Qty1*(m.TotQty/lnTotPo),;
            NLOTQTY2  WITH &lcTmpCur..Qty2*(m.TotQty/lnTotPo),;
            NLOTQTY3  WITH &lcTmpCur..Qty3*(m.TotQty/lnTotPo),;
            NLOTQTY4  WITH &lcTmpCur..Qty4*(m.TotQty/lnTotPo),;
            NLOTQTY5  WITH &lcTmpCur..Qty5*(m.TotQty/lnTotPo),;
            NLOTQTY6  WITH &lcTmpCur..Qty6*(m.TotQty/lnTotPo),;
            NLOTQTY7  WITH &lcTmpCur..Qty7*(m.TotQty/lnTotPo),;
            NLOTQTY8  WITH &lcTmpCur..Qty8*(m.TotQty/lnTotPo)
            lnTotLn = NLOTQTY1+NLOTQTY2+NLOTQTY3+NLOTQTY4+;
                      NLOTQTY5+NLOTQTY6+NLOTQTY7+NLOTQTY8
            REPLACE NLOTTOTQTY WITH lnTotLn
    UNLOCK
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDSCAN
USE IN APOSHDr
USE IN APOSLN
USE IN APoConMgm
IF USED(lcTmpCur)
  USE IN (lcTmpCur)
ENDIF
RETURN

*E:300630 SSH (BEGIN)Add function to convert contractor manegment Headerfile
*********************************************************************
* Update CUSTOMER poconmgm.dbf For ROB100
*********************************************************************
FUNCTION lfURMopH

*--Call standard poconmgm update.
=lfUdMOph()

IF EMPTY(lcSource)
  RETURN
ENDIF

IF !lfOpn26Fl('poshdr','poshdr')
  RETURN
ENDIF
IF !lfOpn26Fl('CONTRACT','CONTRACT')
  RETURN
ENDIF

*B802033,1 (Start) Open the vendor file.
llOndVen=gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
*B802033,1 (End).

SELECT ACONTRACT
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR MEMO
  IF !EMPTY(cContr1)
    SELECT (lnWaSel1)
    APPEND BLANK
    =RLOCK() 
    REPLACE CIMTYP    WITH 'I',;
            CTKTNO    WITH m.cPO,;
            COPRCODE  WITH m.CManfCode1,;
            COPERSEQ  WITH '1' ,;
            COPRCOMNT WITH m.cNote1 ,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.cContr1 ,;
            NNXTLOTNO WITH 1 ,;
            CCONTNAME WITH IIF(SEEK(m.cContr1,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
  ENDIF
  IF !EMPTY(cContr2)
    SELECT (lnWaSel1)
    APPEND BLANK
    =RLOCK() 
    REPLACE CIMTYP    WITH 'I',;
            CTKTNO    WITH m.cPO,;
            COPRCODE  WITH m.CManfCode2,;
            COPERSEQ  WITH '2' ,;
            COPRCOMNT WITH m.cNote2 ,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.cContr2 ,;
            NNXTLOTNO WITH 1 ,;
            CCONTNAME WITH IIF(SEEK(m.cContr2,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
  ENDIF
  IF !EMPTY(cContr3)
    SELECT (lnWaSel1)
    APPEND BLANK
    =RLOCK() 
    REPLACE CIMTYP    WITH 'I',;
            CTKTNO    WITH m.cPO,;
            COPRCODE  WITH m.CManfCode3,;
            COPERSEQ  WITH '3' ,;
            COPRCOMNT WITH m.cNote3 ,;
            LINHOUSE  WITH .F.,;
            CCONTCODE WITH m.cContr3,;
            NNXTLOTNO WITH 1 ,;
            CCONTNAME WITH IIF(SEEK(m.cContr3,'APVENDOR'),APVENDOR.cVenComp,'')
    UNLOCK
  ENDIF
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Close the vendor file if opened.
IF llOndVen
  USE IN APVENDOR
ENDIF
*B802033,1 (End).
USE IN aposhdr
USE IN ACONTRACT
RETURN
*E:300630 SSH (END)


*WAM
****************************************************************************
* Update EDICATGH.DBF , EDICATGD.DBF files releated to UCC_SELC and UCC_CATG file.
****************************************************************************
FUNCTION ldEdiCatg

=gfOpenFile(gcDataDir+'EDICATGD','','EX')
SELECT EDICATGD
ZAP
*--Open 2.6 help file.
=lfOpn26Fl('UCC_CATG','UCC_CATG')
=lfOpn26Fl('CUSTOMER','CUSTOMER')
=lfOpn26Fl('EDIACC','EDIACC')

SELECT (lnWaSel2)   && 2.6 UCC_SELC.dbf
*--Start updating , it will update 3 files.
lnMax1 = RECCOUNT() 
SCAN
  SCATTER MEMVAR MEMO
  SELECT AEDIACC
  LOCATE FOR UCC_VER = m.Ver
  IF FOUND()
    m.Type = IIF(SEEK('M'+AEDIACC.Account,'ACUSTOMER'),'A','N')
    SELECT (lnWaSel1)    && EDICATGH.dbf
    APPEND BLANK
    *B803695,1 AMM the default value of cEdiStatus should be 'A'
    *REPLACE Type       WITH m.Type ,;
            cPartner   WITH AEDIACC.Account,;
            cSelcCode  WITH m.SelUCode ,;
            cSelcDesc  WITH m.SelDesc ,;
            cSelcType  WITH m.SelType ,;
            cCode      WITH m.SelACode ,;
            cEdiStatus WITH m.Status
    REPLACE Type       WITH m.Type ,;
            cPartner   WITH AEDIACC.Account,;
            cSelcCode  WITH m.SelUCode ,;
            cSelcDesc  WITH m.SelDesc ,;
            cSelcType  WITH m.SelType ,;
            cCode      WITH m.SelACode ,;
            cEdiStatus WITH IIF(EMPTY(m.Status),'A',m.Status)
    *B803695,1 AMM end
    SELECT AUCC_CATG
    =SEEK(m.Ver+m.SelUCode)
    SCAN REST WHILE ver+selucode+upc = UCC_SELC.Ver+UCC_SELC.SelUCode
      SCATTER MEMVAR
      SELECT EDICATGD
      APPEND BLANK
      REPLACE Type      WITH m.Type ,;
              cPartner  WITH AEDIACC.Account,;
              cSelcCode WITH m.SelUCode ,;
              Style     WITH m.Style+'-'+m.Color,;
              Size      WITH m.Size ,;
              cUpc      WITH m.UPC ,;
              cEdiDStat WITH IIF(!EMPTY(m.DelDate),'D',IIF(!EMPTY(m.DiscDate),'C','A')) ,;
              Date      WITH {} ,;
              DiscDate  WITH m.DiscDate ,;
              DelDate   WITH m.DelDate ,;
              SendDate  WITH IIF(!EMPTY(m.DelDate),m.DelDate,IIF(!EMPTY(m.DiscDate),m.DiscDate,DATE())) ,;
              lPackUpc  WITH m.lPack
    ENDSCAN
  ENDIF  
  SELECT (lnWaSel2)
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*--Close opened files.
IF USED('EDICATGD')
  USE IN EDICATGD
ENDIF
IF USED('ACUSTOMER')
  USE IN ACUSTOMER
ENDIF
IF USED('AUCC_CATG')
  USE IN AUCC_CATG
ENDIF
IF USED('AEDIACC')
  USE IN AEDIACC
ENDIF
RETURN



*E301359,1 Added function to update customer history file.
**********************************************************************
* Update ARCUSHST.dbf
**********************************************************************
FUNCTION lfUCstHist

IF EMPTY(lcSource)
  RETURN
ENDIF

*--Open perids file.
llOFsPrOp = .F.
llFsPrOp  = gfOpenFile(gcDataDir+'FSPRD','','SH')
SELECT FSPRD
GO TOP
IF EOF()
  *--It does not converted yet.
  IF !EMPTY(lcASource)
    IF FILE(lcAdvSys+'SYCFSPRD.DBF')
      SELECT 0
      USE (lcAdvSys+'SYCFSPRD') SHARED
      llOFsPrOp = .T.
    ENDIF
  ELSE
    IF FILE(lcA26Data+'FISCALLN.DBF')
      SELECT 0
      USE (lcA26Data+'FISCALLN') SHARED
      llOFsPrOp = .T.
    ENDIF
  ENDIF
  GO TOP
  IF EOF()
    =lfUpdError('Fiscal Period not Defined, Unable to convert Customer History.')
    IF llOFsPrOp
      USE
    ENDIF
    IF llFsPrOp AND USED('FSPRD')
      USE IN FSPRD
    ENDIF
    RETURN
  ENDIF
ENDIF
lnFpdWa = SELECT()

llEngDate = (ALLTRIM(gcContCode) = 'ENG' .OR. SET('DATE')='BRITISH' )
*B603435,1 Start.
*DIME laFpirods[12]
DIME laFpirods[12,2],laPrdAmt[6,13]

lcCentry  = SET('CENT')
SET CENT ON

SELECT (lnWaSel1)
SET ORDER TO TAG Acthst    &&account+cfisfyear
*B603435,1 End.

*B603435,1 Work on arraies insted of fileds, in main scan loop.
SELECT (lnWaSel2)
lnMax1 = RECCOUNT()
SCAN
  SCATTER MEMVAR

  *--Clear array
  laPrdAmt  = 0
  laFpirods = ' '
  *--Fill laFpirods array by year monthes.
  =lfGetFPMnh()
  
  
  lnPd = 1
  DO WHILE lnPd <= 12
   
    *--Read period.
    lcPrd = lfGetFProd(m.Year,lnPd)
    IF EMPTY(lcPrd)
      lnPd = lnPd + 1
      LOOP
    ENDIF

    *--Save 2.7 period and restore month.
    lcMonth = ALLT(laFpirods[lnPd,1])
    laFpirods[lnPd,2] = lcPrd

    *--Get amounts. 
    lnBookAmt = EVAL('m.'+lcMonth+'_BOOK')
    lnShipAmt = EVAL('m.'+lcMonth+'_SHIP')
    lnRetnAmt = EVAL('m.'+lcMonth+'_RETN')
    lnAlowAmt = EVAL('m.'+lcMonth+'_ALLOW')
    lnPtmtAmt = EVAL('m.'+lcMonth+'_PYMT')
    lnCgBkAmt = EVAL('m.'+lcMonth+'_CHARGE')

    laPrdAmt[1,lnPd] = lnBookAmt 
    laPrdAmt[2,lnPd] = lnShipAmt 
    laPrdAmt[3,lnPd] = lnRetnAmt
    laPrdAmt[4,lnPd] = lnAlowAmt 
    laPrdAmt[5,lnPd] = lnPtmtAmt 
    laPrdAmt[6,lnPd] = lnCgBkAmt 
 
    *--Total of 12 periods.
    laPrdAmt[1,13] = laPrdAmt[1,13] + lnBookAmt 
    laPrdAmt[2,13] = laPrdAmt[2,13] + lnShipAmt 
    laPrdAmt[3,13] = laPrdAmt[3,13] + lnRetnAmt
    laPrdAmt[4,13] = laPrdAmt[4,13] + lnAlowAmt 
    laPrdAmt[5,13] = laPrdAmt[5,13] + lnPtmtAmt 
    laPrdAmt[6,13] = laPrdAmt[6,13] + lnCgBkAmt 

    lnPd = lnPd + 1
  ENDDO

  *--If no informaition all periods are zero.
  IF laPrdAmt[1,13]=0 AND laPrdAmt[2,13]=0 AND laPrdAmt[3,13]=0 AND;
     laPrdAmt[4,13]=0 AND laPrdAmt[5,13]=0 AND laPrdAmt[6,13]=0
    LOOP
  ENDIF


  SELECT (lnWaSel1)
  IF !SEEK(m.Account+m.Year)
    APPEND BLANK
    REPLACE Account   WITH m.Account,;
            cFisFYear WITH m.Year
  ENDIF
  
  lnPd = 1
  DO WHILE lnPd <= 12
    lcPrd = laFpirods[lnPd,2]
    IF EMPTY(lcPrd)
      lnPd = lnPd + 1
      LOOP
    ENDIF
     
    REPLACE NORDAMT&lcPrd  WITH NORDAMT&lcPrd + laPrdAmt[1,lnPd],;
            NSLSAMT&lcPrd  WITH NSLSAMT&lcPrd + laPrdAmt[2,lnPd],;
            NRETAMT&lcPrd  WITH NRETAMT&lcPrd + laPrdAmt[3,lnPd],;
            NALLOW&lcPrd   WITH NALLOW&lcPrd  + laPrdAmt[4,lnPd],;
            NPAYMENT&lcPrd WITH NPAYMENT&lcPrd+ laPrdAmt[5,lnPd],;
            NDRADJ&lcPrd   WITH NDRADJ&lcPrd  + laPrdAmt[6,lnPd]
    lnPd = lnPd + 1
  ENDDO

  REPLACE NORDAMT  WITH NORDAMT + laPrdAmt[1,13],;
          NSLSAMT  WITH NSLSAMT + laPrdAmt[2,13],;
          NRETAMT  WITH NRETAMT + laPrdAmt[3,13],;
          NALLOW   WITH NALLOW  + laPrdAmt[4,13],;
          NPAYMENT WITH NPAYMENT+ laPrdAmt[5,13],;
          NDRADJ   WITH NDRADJ  + laPrdAmt[6,13]


  SELECT (lnWaSel2)
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
*B603435,1 End.

*-Create current perieds in not created before.
IF lfOpn26Fl('CUSTOMER','CUSTOMER') AND !EMPTY(gcCurrYear)
  SELECT ACUSTOMER
  lnMax1   = RECCOUNT()
  lnCurNm1 = 1
  STORE ' ' TO lcD1Thrm,lcD1ThPrc
  SELECT (lnWaSel1)   
  SET ORDER TO TAG Acthst
  SELECT ACUSTOMER
  SCAN FOR Type = 'M' 
    lcCustomer = Account

    SELECT (lnWaSel1)   
    IF !SEEK(lcCustomer+STR(VAL(gcCurrYear)-1,4))
      INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcCustomer,STR(VAL(gcCurrYear)-1,4))
    ENDIF
    IF !SEEK(lcCustomer+gcCurrYear)
      INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcCustomer,gcCurrYear)
    ENDIF
    IF !SEEK(lcCustomer+STR(VAL(gcCurrYear)+1,4))    
      INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcCustomer,STR(VAL(gcCurrYear)+1,4))
    ENDIF    

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN

  USE IN ACUSTOMER
ENDIF

*-Sent century setting back.
SET CENT &lcCentry

*--Close perioed file.
IF llFsPrOp AND USED('FSPRD')
  USE IN FSPRD
ENDIF
IF llOFsPrOp
  SELECT(lnFpdWa)
  USE
ENDIF
RETURN

*******************
FUNCTION lfGetFProd
*******************
PARA lcFYear,lnFYPd

lcRetPrd = ''
lcStPrDt = IIF(llEngDate,;
  CTOD('01/'+STR(lnFYPd,2)+'/'+lcFYear),CTOD(STR(lnFYPd,2)+'/01/'+lcFYear))

SELECT(lnFpdWa)
IF llOFsPrOp
  IF !EMPTY(lcASource)
    *--Adv1 file.
    LOCATE FOR cComp_id = laASComp[lnASComp] AND cFisFYear=lcFYear AND BETWEEN(lcStPrDt,dFsPpBgDt,dFsPpEnDt)
    lcRetPrd = IIF(FOUND(),cFsPPrdId,'')
  ELSE
    *--2.6 file.
    LOCATE FOR cFisYear=lcFYear AND BETWEEN(lcStPrDt,dPrdBgDat,dPrdEnDat)
    lcRetPrd = IIF(FOUND(),cPrdId,'')
  ENDIF
ELSE
  *--2.7 file.
  LOCATE FOR cFisFYear=lcFYear AND BETWEEN(lcStPrDt,dFsPpBgDt,dFsPpEnDt)
  lcRetPrd = IIF(FOUND(),cFsPPrdId,'')
ENDIF
SELECT (lnWaSel1)
RETURN lcRetPrd

*******************
FUNCTION lfGetFPMnh
*******************
laFpirods[1,1]  = 'JAN'
laFpirods[2,1]  = 'FEB'
laFpirods[3,1]  = 'MAR'
laFpirods[4,1]  = 'APR'
laFpirods[5,1]  = 'MAY'
laFpirods[6,1]  = 'JUN'
laFpirods[7,1]  = 'JUL'
laFpirods[8,1]  = 'AUG'
laFpirods[9,1]  = 'SEP'
laFpirods[10,1] = 'OCT'
laFpirods[11,1] = 'NOV'
laFpirods[12,1] = 'DEC'
RETURN
*E301359,1 End.

*!*************************************************************
*! Name      : lfUpdMatJl
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/23/1999
*! Purpose   : To update the MatInvJl file.
*!*************************************************************
*! B802817,1 KHM 11/23/99 
*!*************************************************************
FUNCTION lfUpdMatJl

IF EMPTY(lcSource)
  RETURN
ENDIF

*B603386,1 Start.
IF llBalJurl   && Don't convert , rebalance only.

  *--Open 26 style file to fill the styinvjl file.
  IF !lfOpn26Fl('FABRIC','FABRIC')
    RETURN
  ENDIF
  IF !lfOpn26Fl('FABDYE','FABDYE')
    RETURN
  ENDIF
  SELECT 0
  USE (lcPathA26+'FABDYE') SHARED ALIAS 'XFABDYE' AGAIN
  SET ORDER TO TAG FABDYE


  SELECT AFABRIC
  lnMax1 = RECCOUNT()
  SCAN

    IF llDyeSysm AND AFABRIC.Dyelot='Y'
      *--Convert from Fabdye for !empty(dyelot) , dyelot record.
      llFabrUpd = .F.
      lcForCndn = '!EMPTY(Dyelot)'
    ELSE
      llFabrUpd = IIF(llMultiwh , .F. , .T. )
      lcForCndn = IIF(llMultiwh,'EMPTY(Dyelot)','.T.')
    ENDIF

    *--Create inventory adjustment. 
    IF llFabrUpd   && Single warehouse , Dyelot No.

      *--Create adjustment only for fabrics that have stock.
      IF OnHand = 0
        LOOP
      ENDIF  

      *--Create inventory adjustment. 
      SELECT (lnWaSel1)
      APPEND BLANK
      *B605949,1 KHM 05/29/2002 (Begin) Replacing the nIssued with +ve 
      *REPLACE CTRN_SEQ   WITH '000000',;
              CFABRIC    WITH AFABRIC.Fabric,;
              CCOLOR     WITH AFABRIC.Color,;
              CWARECODE  WITH 'DEFWHE',;
              CDYELOT    WITH '',;
              DTRANDATE  WITH gdSysDate,;
              CTRANTYPE  WITH '2',;
              CTRAN      WITH '000000',;
              CRSESSION  WITH IIF(AFABRIC.OnHand<0,'','000000'),;
              CISESSION  WITH IIF(AFABRIC.OnHand<0,'000000',''),;
              NUNITCOST  WITH AFABRIC.Ave_Cost,;
              NRECEIVED  WITH IIF(AFABRIC.OnHand<0,0,AFABRIC.OnHand),;
              NISSUED    WITH IIF(AFABRIC.OnHand<0,AFABRIC.OnHand,0),;
              NUNTCSTBUY WITH AFABRIC.Ave_Cost,;
              REFERENCE  WITH 'Starting Balance',;
              CADJREASON WITH lcGVr_AdjR,;
              CGLMATADJ  WITH lcGVr_ARRf,;
              DPOSTDATE  WITH gdSysDate,;
              NSTKVAL    WITH (AFABRIC.Ave_Cost*AFABRIC.OnHand)
      REPLACE CTRN_SEQ   WITH '000000',;
              CFABRIC    WITH AFABRIC.Fabric,;
              CCOLOR     WITH AFABRIC.Color,;
              CWARECODE  WITH 'DEFWHE',;
              CDYELOT    WITH '',;
              DTRANDATE  WITH gdSysDate,;
              CTRANTYPE  WITH '2',;
              CTRAN      WITH '000000',;
              CRSESSION  WITH IIF(AFABRIC.OnHand<0,'','000000'),;
              CISESSION  WITH IIF(AFABRIC.OnHand<0,'000000',''),;
              NUNITCOST  WITH AFABRIC.Ave_Cost,;
              NRECEIVED  WITH IIF(AFABRIC.OnHand<0,0,AFABRIC.OnHand),;
              NISSUED    WITH IIF(AFABRIC.OnHand<0,ABS(AFABRIC.OnHand),0),;
              NUNTCSTBUY WITH AFABRIC.Ave_Cost,;
              REFERENCE  WITH 'Starting Balance',;
              CADJREASON WITH lcGVr_AdjR,;
              CGLMATADJ  WITH lcGVr_ARRf,;
              DPOSTDATE  WITH gdSysDate,;
              NSTKVAL    WITH (AFABRIC.Ave_Cost*AFABRIC.OnHand)
      *B605949,1 KHM 05/29/2002 (End)
    ELSE  && CONVERT FROM FABDYE.

      IF !SEEK(AFABRIC.Fabric+AFABRIC.Color,'AFABDYE')
        LOOP
      ENDIF

      SELECT AFABDYE
      SCAN WHILE Fabric+Color = AFABRIC.Fabric+AFABRIC.Color FOR &lcForCndn

        *--Create adjustment only for fabrics that have stock.
        IF OnHand = 0
          LOOP
        ENDIF  

        lnFSBCost = AFABDYE.Ave_Cost
        *--Read ave cost from warehouse record.
        IF !EMPTY(AFABDYE.Dyelot)
          lnFSBCost = IIF(SEEK(AFABDYE.Fabric+AFABDYE.Color+AFABDYE.cWareCode+SPACE(10),'XFABDYE'),XFABDYE.Ave_Cost,0)
        ENDIF

        *--Create inventory adjustment. 
        SELECT (lnWaSel1)
        APPEND BLANK
        *B605949,1 KHM 05/29/2002 (Begin) Replacing the nIssued with +ve 
        *REPLACE CTRN_SEQ   WITH '000000',;
                CFABRIC    WITH AFABDYE.Fabric,;
                CCOLOR     WITH AFABDYE.Color,;
                CWARECODE  WITH AFABDYE.cWareCode,;
                CDYELOT    WITH AFABDYE.Dyelot,;
                DTRANDATE  WITH gdSysDate,;
                CTRANTYPE  WITH '2',;
                CTRAN      WITH '000000',;
                CRSESSION  WITH IIF(AFABDYE.OnHand<0,'','000000'),;
                CISESSION  WITH IIF(AFABDYE.OnHand<0,'000000',''),;
                NUNITCOST  WITH lnFSBCost,;
                NRECEIVED  WITH IIF(AFABDYE.OnHand<0,0,AFABDYE.OnHand),;
                NISSUED    WITH IIF(AFABDYE.OnHand<0,AFABDYE.OnHand,0),;
                NUNTCSTBUY WITH lnFSBCost,;
                REFERENCE  WITH 'Starting Balance',;
                CADJREASON WITH lcGVr_AdjR,;
                CGLMATADJ  WITH lcGVr_ARRf,;
                DPOSTDATE  WITH gdSysDate,;
                NSTKVAL    WITH (lnFSBCost*AFABDYE.OnHand)
        REPLACE CTRN_SEQ   WITH '000000',;
                CFABRIC    WITH AFABDYE.Fabric,;
                CCOLOR     WITH AFABDYE.Color,;
                CWARECODE  WITH AFABDYE.cWareCode,;
                CDYELOT    WITH AFABDYE.Dyelot,;
                DTRANDATE  WITH gdSysDate,;
                CTRANTYPE  WITH '2',;
                CTRAN      WITH '000000',;
                CRSESSION  WITH IIF(AFABDYE.OnHand<0,'','000000'),;
                CISESSION  WITH IIF(AFABDYE.OnHand<0,'000000',''),;
                NUNITCOST  WITH lnFSBCost,;
                NRECEIVED  WITH IIF(AFABDYE.OnHand<0,0,AFABDYE.OnHand),;
                NISSUED    WITH IIF(AFABDYE.OnHand<0,ABS(AFABDYE.OnHand),0),;
                NUNTCSTBUY WITH lnFSBCost,;
                REFERENCE  WITH 'Starting Balance',;
                CADJREASON WITH lcGVr_AdjR,;
                CGLMATADJ  WITH lcGVr_ARRf,;
                DPOSTDATE  WITH gdSysDate,;
                NSTKVAL    WITH (lnFSBCost*AFABDYE.OnHand)
         *B605949,1 KHM 05/29/2002 (End)
      ENDSCAN
    ENDIF

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
 
  USE IN AFABRIC
  USE IN AFABDYE
  USE IN XFABDYE

ELSE   && Convert the hole file.
*B603386,1 End.

  *-- Opening the following files in order to determine the type of the
  *-- transaction whether its Imported 'I' or Manufactured 'M'.
  llOpenPo   = lfOpn26Fl('PosHdr','PosHdr',.T.)
  llOpenCT   = lfOpn26Fl('CutTktH','CutTktH',.T.)
  llOpenBomC = lfOpn26Fl('BomCost','BomCost')

  SELECT (lnWaSel2)
  lnMax1 = RECCOUNT() 
  SCAN
    SCATTER MEMVAR MEMO

    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cAdJReason  WITH lcGVr_AdjR ,;
            cGlMatAdj   WITH lcGVr_ARRf,;
            cLotNo      WITH m.cBatchNo ,;
            dPostDate   WITH dTranDate ,;
            nIssued     WITH IIF(m.cTranType='4' AND m.nIssued<0,0,m.nIssued),;
            nReceived   WITH IIF(cTranType='4' AND m.nIssued<0,ABS(m.nIssued),nReceived),;
            cIMTyp      WITH IIF(!EMPTY(cTktNo),lfGetType(),cIMTyp)

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN

ENDIF
RETURN


*!*************************************************************
*! Name      : lfGetType
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/23/1999
*! Purpose   : To get the type of the transaction 
*!*************************************************************
*! Return    : lcRetValue : 'I' : Imported
*!                          'M' : Manufactured
*!*************************************************************
*! B802817,1 KHM 11/23/99 
*!*************************************************************
FUNCTION lfGetType
PRIVATE lnAlias

IF !llOpenPo AND !llOpenCT
  RETURN ' '
ENDIF

lnAlias    = SELECT(0)
lcCompVal = ''
lcExp      = cFabric+cColor+cWareCode+cDyelot
lcTktNo    = cTktNo

IF llOpenCT AND SEEK(lcTktNo,'ACutTktH')
  lcCompVal = 'M'
  IF llOpenBomC AND SEEK(lcExp,'ABomCost')
    SELECT ABomCost
    LOCATE REST WHILE Item+IClr+cWareCode+cDyelot+cRSession+cISession = lcExp;
                FOR cTktNo = lcTktNo
    IF FOUND()
      IF lcCompVal = ABomCost.cIMTyp
        lcCompVal = 'M'
      ELSE
        IF llOpenPo AND SEEK('P'+lcTktNo,'APosHdr')
          lcCompVal = 'I'
        ENDIF
      ENDIF
    ENDIF  
  ENDIF
ENDIF

IF llOpenPo AND SEEK('P'+lcTktNo,'APosHdr')
  lcCompVal = 'I'
  IF llOpenBomC AND SEEK(lcExp,'ABomCost')
    SELECT ABomCost
    LOCATE REST WHILE Item+IClr+cWareCode+cDyelot+cRSession+cISession = lcExp;
                FOR cTktNo = lcTktNo
    IF FOUND()
      IF lcCompVal = ABomCost.cIMTyp
        lcCompVal = 'I'
      ELSE
        IF llOpenCT AND SEEK(lcTktNo,'ACutTktH')
          lcCompVal = 'M'
        ENDIF
      ENDIF
    ENDIF  
  ENDIF
ENDIF

RETURN lcCompVal



*B802861,1 Added insted of SYCNVLLN.dbf records update.
*********************************************************************
* Update PACK_LIN.dbf
*********************************************************************
FUNCTION lfUpdPkLn

*B803810,1 HBG 11/14/2000 Create temp index to PACK_LIN file[Begin]
lcTempIndx = gfTempName()
SELECT (lnWaSel1)
INDEX ON pack_no+STR(line_no,6) DESCENDING TAG (lcTempIndx) OF (gcWorkDir+lcTempIndx+".CDX")
*B803810,1 [End]

SELECT (lnWaSel2)
lnMax1 = RECCOUNT()

SCAN
  SCATTER MEMVAR MEMO

  SELECT (lnWaSel1)
  IF m.from_crt<>0 AND m.from_crt <= m.to_crt
    FOR lnI = m.from_crt TO m.to_crt
     
      *B803810,1 HBG 11/14/2000 Make the line # sequential [Begin]
      =SEEK(m.pack_no)
      m.Line_no = Line_no + 1
      *B803810,1 [End]
      
      APPEND BLANK
      GATHER MEMVAR MEMO

      *B803810,1 HBG 11/14/2000 Save 0 to 'from_crt' & 'to_crt' fileds [Begin]
      *REPLACE nOrdLineNo WITH m.OrdLineNo,;
              STYLE      WITH m.Style+'-'+m.Color,;
              from_crt   WITH lnI ,;
              to_crt     WITH lnI ,;
              no_cart    WITH lnI ,;
              Line_no    WITH lnLine_no
      REPLACE nOrdLineNo WITH m.OrdLineNo,;
              STYLE      WITH m.Style+'-'+m.Color,;
              from_crt   WITH 0 ,;
              to_crt     WITH 0 ,;
              no_cart    WITH lnI
      *B803810,1 [End]        
    ENDFOR

  ELSE

    APPEND BLANK
    GATHER MEMVAR MEMO
    *B803810,1 HBG 11/14/2000 Save '0' to 'from_crt' & 'to_crt' Fields [Begin]
    *REPLACE NORDLINENO WITH m.OrdLineNo,;
            STYLE      WITH m.Style+'-'+m.Color
    REPLACE nOrdLineNo WITH m.OrdLineNo,;
                STYLE      WITH m.Style+'-'+m.Color,;
                from_crt   WITH 0   ,;
                to_crt     WITH 0 
    *B803810,1 [End]                        
  ENDIF
   =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN

*B803810,1 HBG 11/14/2000 Clear the temp index [Begin]
SELECT (lnWaSel1)
CLOSE INDEX
ERASE (gcWorkDir+lcTempIndx+".CDX")
*B803810,1 [End] 
*B802861,1 End.

*B603386,1 Added function to update STYINVJL file instead of Sycnvlln.
**********************************************************************
* Update STYINVJL.dbf
**********************************************************************
FUNCTION lfUSIvJrnl

*--Convert equevelent apparel file.
IF EMPTY(lcSource)
  RETURN
ENDIF

IF llBalJurl   && Don't convert , rebalance only.

  *--Open 26 style file to fill the styinvjl file.
  IF !lfOpn26Fl('STYLE','STYLE')
    RETURN
  ENDIF
  IF !lfOpn26Fl('STYDYE','STYDYE')
    RETURN
  ENDIF
  SELECT 0
  USE (lcPathA26+'STYDYE') SHARED ALIAS 'XSTYDYE' AGAIN
  SET ORDER TO TAG STYDYE
 

  SELECT ASTYLE
  lnMax1 = RECCOUNT()
  SCAN

    lcStyle = ASTYLE.Style+'-'+ASTYLE.Color
    
    
    IF llDyeSysm AND ASTYLE.Dyelot='Y'
      *--Convert from stydye for !empty(dyelot) , dyelot record.
      llStylUpd = .F.
      lcForCndn = '!EMPTY(Dyelot)'
    ELSE
      llStylUpd = IIF(llMultiwh , .F. , .T. )
      lcForCndn = IIF(llMultiwh,'EMPTY(Dyelot)','.T.')
    ENDIF

    *--Create inventory adjustment. 
    IF llStylUpd   && Single warehouse , Dyelot No.

      *--Create adjustment only for style that have stock.
      IF Stk1 = 0 AND Stk2 = 0 AND Stk3 = 0 AND Stk4 = 0 AND ;
         Stk5 = 0 AND Stk6 = 0 AND Stk7 = 0 AND Stk8 = 0
        LOOP
      ENDIF  

      *--Create inventory adjustment. 
      SELECT (lnWaSel1)
      APPEND BLANK
      REPLACE CSESSION   WITH '000000',;
              STYLE      WITH lcStyle,;
              CWARECODE  WITH 'DEFWHE',;
              CDYELOT    WITH '',;
              DTRDATE    WITH gdSysDate,;
              CTRTYPE    WITH '1',; 
              CTRCODE    WITH '000000',;
              NCOST      WITH ASTYLE.Ave_Cost,;
              CIRTYPE    WITH IIF(ASTYLE.TotStk<0,'I','R'),;
              CRSESSION  WITH IIF(ASTYLE.TotStk<0,'','000000'),;
              CISESSION  WITH IIF(ASTYLE.TotStk<0,'000000',''),;
              NSTK1      WITH ASTYLE.Stk1,;
              NSTK2      WITH ASTYLE.Stk2,;
              NSTK3      WITH ASTYLE.Stk3,;
              NSTK4      WITH ASTYLE.Stk4,;
              NSTK5      WITH ASTYLE.Stk5,;
              NSTK6      WITH ASTYLE.Stk6,;
              NSTK7      WITH ASTYLE.Stk7,;
              NSTK8      WITH ASTYLE.Stk8,;
              NTOTSTK    WITH ASTYLE.TotStk,;
              NSTKVAL    WITH (ASTYLE.TotStk*ASTYLE.Ave_Cost),;
              REFERENCE  WITH 'Starting Balance',;
              CADJACCT   WITH lcGVr_ARRf,;
              CADJREASON WITH lcGVr_AdjR

    ELSE  && CONVERT FROM STYDYE.

      IF !SEEK(ASTYLE.Style+ASTYLE.Color,'ASTYDYE')
        LOOP
      ENDIF

      SELECT ASTYDYE
      SCAN WHILE Style+Color = ASTYLE.Style+ASTYLE.Color FOR &lcForCndn

        *--Create adjustment only for style that have stock.
        IF Stk1 = 0 AND Stk2 = 0 AND Stk3 = 0 AND Stk4 = 0 AND ;
           Stk5 = 0 AND Stk6 = 0 AND Stk7 = 0 AND Stk8 = 0
          LOOP
        ENDIF  

        lnSSBCost = ASTYDYE.Ave_Cost
        *--Read ave cost from warehouse record.
        IF !EMPTY(ASTYDYE.Dyelot)
          lnSSBCost = IIF(SEEK(ASTYDYE.Style+ASTYDYE.Color+ASTYDYE.cWareCode+SPACE(10),'XSTYDYE'),XSTYDYE.Ave_Cost,0)
        ENDIF


        *--Create inventory adjustment. 
        SELECT (lnWaSel1)
        APPEND BLANK
        REPLACE CSESSION   WITH '000000',;
                STYLE      WITH lcStyle,;
                CWARECODE  WITH ASTYDYE.cWareCode,;
                CDYELOT    WITH ASTYDYE.Dyelot;
                DTRDATE    WITH gdSysDate,;
                CTRTYPE    WITH '1',; 
                CTRCODE    WITH '000000',;
                NCOST      WITH lnSSBCost,;
                CIRTYPE    WITH IIF(ASTYDYE.TotStk<0,'I','R'),;
                CRSESSION  WITH IIF(ASTYDYE.TotStk<0,'','000000'),;
                CISESSION  WITH IIF(ASTYDYE.TotStk<0,'000000',''),;
                NSTK1      WITH ASTYDYE.Stk1,;
                NSTK2      WITH ASTYDYE.Stk2,;
                NSTK3      WITH ASTYDYE.Stk3,;
                NSTK4      WITH ASTYDYE.Stk4,;
                NSTK5      WITH ASTYDYE.Stk5,;
                NSTK6      WITH ASTYDYE.Stk6,;
                NSTK7      WITH ASTYDYE.Stk7,;
                NSTK8      WITH ASTYDYE.Stk8,;
                NTOTSTK    WITH ASTYDYE.TotStk,;
                NSTKVAL    WITH (ASTYDYE.TotStk*lnSSBCost),;
                REFERENCE  WITH 'Starting Balance',;
                CADJACCT   WITH lcGVr_ARRf,;
                CADJREASON WITH lcGVr_AdjR

      ENDSCAN
    ENDIF

    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
 
  USE IN ASTYLE
  USE IN ASTYDYE
  USE IN XSTYDYE

ELSE   && Convert the Whole file.

  SELECT (lnWaSel2)
  lnMax1 = RECCOUNT()
  SCAN
    SCATTER MEMVAR
    SELECT (lnWaSel1)
    APPEND BLANK
    GATHER MEMVAR
    *--Update Differencies
    =RLOCK() 
    *B804309,1 AME [Start] it replaced the data with nstk3 for all nstk fields  after nstk3
    *REPLACE STYLE     WITH m.Style+'-'+m.Color ,;
            CISESSION WITH IIF(m.cIRType='I',m.cSession,''),;
            CRSESSION WITH IIF(m.cIRType='R',m.cSession,''),;
            NSTK1     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk1,m.nStk1),;
            NSTK2     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk2,m.nStk2),;
            NSTK3     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk3),;
            NSTK4     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk4),;
            NSTK5     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk5),;
            NSTK6     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk6),;
            NSTK7     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk7),;
            NSTK8     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk8),;
            NTOTSTK   WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,; 
            NSTKVAL   WITH (nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8) * nCost,;
            CADJACCT  WITH lcGVr_ARRf,;
            CADJREASON WITH lcGVr_AdjR
    REPLACE STYLE     WITH m.Style+'-'+m.Color ,;
            CISESSION WITH IIF(m.cIRType='I',m.cSession,''),;
            CRSESSION WITH IIF(m.cIRType='R',m.cSession,''),;
            NSTK1     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk1,m.nStk1),;
            NSTK2     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk2,m.nStk2),;
            NSTK3     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk3,m.nStk3),;
            NSTK4     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk4,m.nStk4),;
            NSTK5     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk5,m.nStk5),;
            NSTK6     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk6,m.nStk6),;
            NSTK7     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk7,m.nStk7),;
            NSTK8     WITH IIF(m.cIRType='I' AND m.cTrType $ '238',-m.nStk8,m.nStk8),;
            NTOTSTK   WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,; 
            NSTKVAL   WITH (nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8) * nCost,;
            CADJACCT  WITH lcGVr_ARRf,;
            CADJREASON WITH lcGVr_AdjR
    *B804309,1 AME [End]
    UNLOCK
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN  
ENDIF

RETURN
*B603386,1 End.

*WAEL
****************************************************************************
* Update SYCASNLB Shipping Labels
****************************************************************************
FUNCTION lfASNLBL

SELECT (lnWaSel2)   && 2.6 ASN_LBL.DBF
*--Start updating
lnMax1 = RECCOUNT() 
SCAN
  SCATTER MEMVAR
  m.cVer = m.Ver
  m.cEdiType = m.Type
  m.nSeq = m.Seq
  m.Data = STRTRAN(UPPER(m.Data),'NTOTQTY','TOTQTY')
  m.Data = STRTRAN(UPPER(m.Data),'NCARTONS','CARTONS')
  m.Data = STRTRAN(UPPER(m.Data),'CPACKID','PACK_ID')
  m.Data = STRTRAN(UPPER(m.Data),'ORDNOTE1','NOTE1')
  m.Data = STRTRAN(UPPER(m.Data),'ORDNOTE2','NOTE2')
  INSERT INTO (ALIAS(lnWasel1))  FROM MEMVAR  && SYCASNLBL
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
RETURN

****************************************************************************
* Update SYCUPCLBL Shipping Labels
****************************************************************************
FUNCTION lfUPCLBL

SELECT (lnWaSel2)   && 2.6 UPC_LBL.DBF
*--Start updating
lnMax1 = RECCOUNT() 
SCAN
  SCATTER MEMVAR
  IF cType='L' AND nSeq = 1 AND STRTRAN(ALLTRIM(m.cData),' ','') = '"{B,1,N,1|"'
    m.cData = '"{B,1,N,"+ALLTRIM(STR(lnQuantity))+"|"'
  ENDIF
  m.cEdiType = m.cType
  m.Data = m.cData
  INSERT INTO (ALIAS(lnwasel1)) FROM MEMVAR  && SYCUPCLBL
  =lfThrmo(lnCurNm1,lnMax1,'1')
ENDSCAN
RETURN

****************************************************************************
* Update SYCEDIFS 810 EDI Transaction
****************************************************************************
FUNCTION lfEDIFS

SELECT (lnWaSel2)   && 2.6 FILD2SEG
SET ORDER TO TAG FILD2SEG
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'02  1')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'02  1' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','02','09') AND ;
       !(INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion AND ;
         LOOP_ID+STR(F_ORDER,2)='03  2' AND ALLTRIM(F_NAME)='XFIELD(4)')

      SCATTER MEMVAR
      *B803772,1 AMH Converting Mapping Files for upper case of function lfSkuDesc [Start]
      *IF 'lfSkuDesc(' $ m.F_VALUE
      IF 'LFSKUDESC(' $ UPPER(m.F_VALUE)
        *m.F_VALUE = STRTRAN(m.F_VALUE,'lfSkuDesc(','lfSkuDesc(MORDER,MLINENO,')
        m.F_VALUE = STRTRAN(UPPER(m.F_VALUE),'LFSKUDESC(','LFSKUDESC(MORDER,MLINENO,')
      ENDIF
      *B803772,1 AMH [End]
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDIFS
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
  IF INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'03',2,'XFIELD(4)',"SUBSTR(MCUSTPO,1,4)+'-'+SUBSTR(MCUSTPO,5,7)",".T.")
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'03',2,'XFIELD(4)',"XFIELD(4)+'-'+SUBSTR(MCUSTPO,12,4)","LEN(ALLTRIM(MCUSTPO)) > 11")
  ENDIF
ENDDO
RETURN

****************************************************************************
* Update SYCEDISH 856 EDI Transaction
****************************************************************************
FUNCTION lfEDISH

SELECT (lnWaSel2)   && 2.6 EDI_ASN
SET ORDER TO TAG EDI_ASN
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'01  2')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','09') AND ;
       !(INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion AND ;
         LOOP_ID+STR(F_ORDER,2)='04  2' AND ALLTRIM(F_NAME)='XFIELD(1)')
      SCATTER MEMVAR
      *B803772,1 AMH Converting Mapping Files for upper case of function lfSkuDesc [Start]
      *IF 'lfSkuDesc(' $ m.F_VALUE
      IF 'LFSKUDESC(' $ UPPER(m.F_VALUE)
        *m.F_VALUE = STRTRAN(m.F_VALUE,'lfSkuDesc(','lfSkuDesc(MORDER,MORDLINENO,')
        m.F_VALUE = STRTRAN(UPPER(m.F_VALUE),'LFSKUDESC(','LFSKUDESC(MORDER,MORDLINENO,')
      ENDIF
      *B803772,1 AMH [End]
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDISH
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
  IF INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'04',2,'XFIELD(1)',"SUBSTR(MCUSTPO,1,4)+'-'+SUBSTR(MCUSTPO,5,7)",".T.")
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'04',2,'XFIELD(1)',"XFIELD(1)+'-'+SUBSTR(MCUSTPO,12,4)","LEN(ALLTRIM(MCUSTPO)) > 11")
  ENDIF
ENDDO
RETURN

****************************************************************************
* Update SYCEDIPR 855 EDI Transaction
****************************************************************************
FUNCTION lfEDIPR

SELECT (lnWaSel2)   && 2.6 EDI_PREV
SET ORDER TO TAG EDI_PREV
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'01  2')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','09') AND ;
       !(INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion AND ;
         LOOP_ID+STR(F_ORDER,2)='02  2' AND ALLTRIM(F_NAME)='XFIELD(3)')
      SCATTER MEMVAR
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDIPR
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
  IF INLIST(m.cMapSet,'DAY','MER') AND '004010' $ m.cVersion
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'02',2,'XFIELD(3)',"SUBSTR(MCUSTPO,1,4)+'-'+SUBSTR(MCUSTPO,5,7)",".T.")
    INSERT INTO (ALIAS(lnWasel1)) (CMAPSET,CVERSION,LOOP_ID,F_ORDER,F_NAME,F_VALUE,F_COND) VALUES ;
    (m.cMapSet,m.cVersion,'02',2,'XFIELD(3)',"XFIELD(3)+'-'+SUBSTR(MCUSTPO,12,4)","LEN(ALLTRIM(MCUSTPO)) > 11")
  ENDIF
ENDDO
RETURN

****************************************************************************
* Update SYCEDISC 832 EDI Transaction
****************************************************************************
FUNCTION lfEDISC

SELECT (lnWaSel2)   && 2.6 EDI_UCC
SET ORDER TO TAG Edi_ucc 
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'01  2')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','09')
      SCATTER MEMVAR
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDISC
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDDO
RETURN

****************************************************************************
* Update SYCEDISM 204 EDI Transaction
****************************************************************************
FUNCTION lfEDISM

SELECT (lnWaSel2)   && 2.6 EDI_BOL
SET ORDER TO TAG EDI_BOL
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'01  2')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','09')
      SCATTER MEMVAR
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDISM
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDDO
RETURN

****************************************************************************
* Update SYCEDITX 864 EDI Transaction
****************************************************************************
FUNCTION lfEDITX

SELECT (lnWaSel2)   && 2.6 EDI_TXT
SET ORDER TO TAG EDI_TXT
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = NetWork
  m.cMapSet = Ver

  =SEEK(lcNetWork+m.cMapSet+'02  1')
  LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'02  1' ;
  FOR F_NAME = 'XFIELD(8)'
  m.cVersion = STRTRAN(ALLTRIM(F_VALUE),"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet 
    IF !INLIST(LOOP_ID,'01','02','09')
      SCATTER MEMVAR
      INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDITX
    ENDIF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
ENDDO
RETURN

****************************************************************************
* Update SYCEDISF & SYCEDISG  850 EDI Transaction
****************************************************************************
FUNCTION lfEDIPO

*--Zap EDI PH,PD files before conversion.
=gfOpenFile(gcSysHome+'SYCEDISG','','EX')
SELECT SYCEDISG
ZAP
lcPathA26 = lcA26lib
=lfOpn26Fl('FILD2SEG','FILD2SEG')
=lfOpn26Fl('EDI_ASN' ,'EDI_ASN')
=lfOpn26Fl('EDI_PREV','EDI_PREV')
=lfOpn26Fl('EDI_UCC' ,'EDI_UCC')
=lfOpn26Fl('SEGFILE' ,'SEGFILE')
lcPathA26 = lcA26Data

SELECT (lnWaSel2)   && 2.6 SEG2Fild
SET ORDER TO TAG SEG2FILD
*--Start updating
lnMax1 = RECCOUNT() 
GO TOP
DO WHILE !EOF()
  lcNetWork = Net_Name
  m.cMapSet = Ver
  m.cVersion = ''
  IF SEEK(lcNetWork+m.cMapSet+'02  1','AFILD2SEG')
    SELECT AFILD2SEG
    LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'02  1' ;
    FOR F_NAME = 'XFIELD(8)'
    m.cVersion = ALLTRIM(F_VALUE)
  ELSE
    IF SEEK(lcNetWork+m.cMapSet+'01  2','AEDI_ASN')
      SELECT AEDI_ASN
      LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
      FOR F_NAME = 'XFIELD(8)'
      m.cVersion = ALLTRIM(F_VALUE)
    ELSE
      IF SEEK(lcNetWork+m.cMapSet+'01  2','AEDI_PREV')
        SELECT AEDI_PREV
        LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
        FOR F_NAME = 'XFIELD(8)'
        m.cVersion = ALLTRIM(F_VALUE)
      ELSE
        IF SEEK(lcNetWork+m.cMapSet+'01  2','AEDI_UCC')
          SELECT AEDI_UCC
          LOCATE REST WHILE NETWORK+VER+LOOP_ID+STR(F_ORDER,2)+STR(RECNO(),7) = lcNetWork+m.cMapSet+'01  2' ;
          FOR F_NAME = 'XFIELD(8)'
          m.cVersion = ALLTRIM(F_VALUE)
        ELSE
          =SEEK(lcNetWork+m.cMapSet+'DTM',lnWaSel2)  && SEG2FILD
          IF 'CENDATE' $ SEG2Fild.F_Value
            m.cVersion = "004010"
          ENDIF 
        ENDIF
      ENDIF
    ENDIF  
  ENDIF
  m.cVersion = STRTRAN(m.cVersion,"'","")
  m.cVersion = STRTRAN(m.cVersion,'"',"")

  SELECT (lnWaSel2)   && 2.6 SEG2Fild
  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE net_name+ver+segid+STR(f_order,2)+STR(RECNO(),7) =  lcNetWork+m.cMapSet 
    SCATTER MEMVAR
    IF INLIST(cMapSet,'DAY','MER') AND '004010' $ m.cVersion AND  ;
       INLIST(SegId+STR(F_Order,2),'BCH 3','BEG 3') AND ALLTRIM(F_NAME) = 'MCUSTPO'
      m.F_VALUE = "STRTRAN(XFIELD(3),'-','')"
    ENDIF
    INSERT INTO (ALIAS(lnWasel1)) FROM MEMVAR  && SYCEDISF
    =lfThrmo(lnCurNm1,lnMax1,'1')
  ENDSCAN
  SELECT ASEGFILE
  =SEEK(lcNetWork+m.cMapSet)
  SCAN REST WHILE net_name+ver+segid = lcNetWork+m.cMapSet
    SCATTER MEMVAR
    INSERT INTO SYCEDISG FROM MEMVAR
  ENDSCAN
  SELECT (lnWaSel2)   && 2.6 SEG2Fild
ENDDO

*--Close opened files.
IF USED('AFILD2SEG')
  USE IN AFILD2SEG
ENDIF
IF USED('AEDI_ASN')
  USE IN AEDI_ASN
ENDIF
IF USED('AEDI_PREV')
  USE IN AEDI_PREV
ENDIF
IF USED('AEDI_UCC')
  USE IN AEDI_UCC
ENDIF
IF USED('ASEGFILE')
  USE IN ASEGFILE
ENDIF
RETURN

*:*************************************************************
*: Name      : lfgetAcct
*: Developer : Ahmed Ibrahim  AMM
*: Date      : 09/22/2000
*: Purpose   : Update account field in STYLE UPC file.
*: Reference : *B803695,1 AMM
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
FUNCTION lfgetAcct
IF !m.lPack                                                     
  RETURN ''
ENDIF
IF SEEK('P'+ALLTRIM(m.style),'ASPCK_HDR')
  RETURN ASPCK_HDR.ACCOUNT
ENDIF
RETURN ''

*:*************************************************************
*: Name      : lfGtFldErr
*: Developer : Ahmed Maher AMH
*: Date      : 10/24/2000
*: Purpose   : Get Field witch creating numric overflow error
*: Reference : *E500382,1 AMH
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************

FUNCTION lfGtFldErr
PRIVATE lcFields , laFields
lcFields = ''
DECLARE laFields[1,4]
IF ERROR() = 39
  =AFIELDS(laFields)
  *-- Loop All Fields in the table casued the error
  FOR lnFlds = 1 TO ALEN(laFields,1)
    *-- Compare Numeric fields with it Maximum value
    IF laFields[lnFlds,2] = 'N' .AND. ;
       EVALUATE(laFields[lnFlds,1]) > VAL(REPLICATE('9',laFields[lnFlds,3]))
       lcFields = lcFields + IIF(EMPTY(lcFields),'Field(s) ' + laFields[lnFlds,1],;
                                 ','+laFields[lnFlds,1])
    ENDIF
  ENDFOR
ENDIF
RETURN lcFields
*--- end of lfGtFldErr.



*B803833,1 (Start) New function to update ShipVia field in BOL_hdr file.
*:*************************************************************
*: Name      : lfvShpVCd
*: Developer : TAK
*: Date      : 11/15/2000
*: Purpose   : Get ship via code from passed description.
*: Para      : Ship via description (lcCarrier)
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
FUNCTION lfvShpVCd
PARA lcCarrier

lcShpVCd = ""
IF !EMPTY(lcCarrier) AND SEEK('V','ACODE')
  lnCurAlis = SELECT()
  SELECT ACODE
  LOCATE REST WHILE Code='V' FOR UPPER(CData) = PADR(UPPER(lcCarrier),15)
  lcShpVCd = IIF(FOUND(),SUBSTR(Code,2,2),"")
  SELECT(lnCurAlis)
ENDIF
RETURN lcShpVCd
*B803833,1 (End).


*C102076,1 SSH 22/01/01 Convert Cathy Daniel Custom field.
*:*************************************************************
*: Name      : lfCathCtkH
*: Developer : Ahmed Salah Shalaby - SSH
*: Date      : 22/01/2001
*: Purpose   : Convert Cathy Daniel Custom field in CUTTKTH.
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
FUNCTION lfCathCtkH

REPLACE NLOT_NO  WITH m.LOT_NO    ,;
        CPACK    WITH m.PACK      ,;
        CCT1CSTY WITH m.STYLE_ADD1,;
        CCT2CSTY WITH m.STYLE_ADD2,;
        CCT3CSTY WITH m.STYLE_ADD3,;
        CCT4CSTY WITH m.STYLE_ADD4,;
        CCT5CSTY WITH m.STYLE_ADD5,;
        NCT1NLOT WITH m.LOT_ADD1  ,;
        NCT2NLOT WITH m.LOT_ADD2  ,;
        NCT3NLOT WITH m.LOT_ADD3  ,;
        NCT4NLOT WITH m.LOT_ADD4  ,;
        NCT5NLOT WITH m.LOT_ADD5  ,;
        DBOMDATE WITH m.BOM_DATE  ,;
        CTKTTYPE WITH STR(m.LOT_NO,6)


*C102076,1 SSH 22/01/01 Convert Cathy Daniel style code structure.
*:*************************************************************
*: Name      : lfCathStr
*: Developer : Ahmed Salah Shalaby - SSH
*: Date      : 22/01/2001
*: Purpose   : Convert Cathy Daniel style code structure.
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
FUNCTION lfCathStr
PARAMETER lcStrNo

IF lcStrNo="3" .AND. !SEEK('U3')
  APPEND BLANK
  =RLOCK()
  GATHER FROM laStyRec
ENDIF
REPLACE cItemRecTy WITH 'U',;
        cISegsepr  WITH IIF(lcStrNo="3","","-"),;
        ciSegsDes  WITH IIF(lcStrNo="1","Ses",IIF(lcStrNo="2","Style","Color")),;
        cISegLDes  WITH IIF(lcStrNo="1","Season",IIF(lcStrNo="2","Style","Color")),;
        cIsegType  WITH IIF(lcStrNo="1","Z",IIF(lcStrNo="2","F","C")),;
        NIsegSize  WITH IIF(lcStrNo="1",3,IIF(lcStrNo="2",8,6)),;
        cISegHead  WITH IIF(lcStrNo="1","Ses-Style   -Color ",""),;
        lSegEndMaj WITH (lcStrNo="2"),;
        cIsegNo    WITH lcStrNo,;
        cAdd_user  WITH 'CONVERSION'
        
        
*-- End OF lfCathStr.
*:*************************************************************
*: Name      : lfUPdmPatr
*: Developer : Abdou Elgendy
*: Date      : 06/23/2003
*: Purpose   : Convert the pattern from the style file At26
*:           : to PDM pattern file at 27, in case SP install.
*:*************************************************************
*: Called From  : CnvMain
*:*************************************************************
*:B#607236,1
FUNCTION lfUPdmPatr
PRIVATE lnPrvAlias,lcOldOrdr1,lcOldOrdr2

lnPrvAlias = SELECT(0)

SELECT PDMPATRN
lcOldOrdr1 = ORDER()
SET ORDER TO Cpattern


SELECT STYLE
lcOldOrdr2= ORDER()
SET ORDER TO Stylepat
LOCATE
SCAN
  WAIT WINDOW 'Convert pattern # :- ' + Pattern NOWAIT
  *-- Scan for pattern
  STORE '' TO lcPattern , lcRev_no
  *-- get the lenght of the field that because we should add 8 chracter from the
  *-- convert pattern to cpatt_no and the last 2 charcter from the pattern should
  *--  update the cRev_no
  
  lnFldLen  = LEN(ALLTRIM(PATTERN))
  IF lnFldLen>= 3
    lcPattern = PADR(LEFT(PATTERN,lnFldLen-2),8)
    lcRev_no  = RIGHT(ALLTRIM(PATTERN),2)
  ELSE
    lcPattern = PATTERN
  ENDIF
  *-- LEFT(cpatt_no,8)+crev_no
  IF !SEEK(lcPattern+lcRev_no,'PDMPATRN')
    *-- Add pattern to pattern file.
    SELECT PDMPATRN
    APPEND BLANK
    REPLACE cpatt_no WITH  lcPattern,;
            crev_no  WITH lcRev_no
  ENDIF
  SELECT STYLE
ENDSCAN

SELECT STYLE
SET ORDER TO &lcOldOrdr2

SELECT PDMPATRN
SET ORDER TO &lcOldOrdr1

SELECT(lnPrvAlias )

*-- End OF lfUPdmPatr
*-- B#607236,1
*!*************************************************************