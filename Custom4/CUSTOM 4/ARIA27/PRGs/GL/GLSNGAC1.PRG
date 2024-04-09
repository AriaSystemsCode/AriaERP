*:************************************************************************
*:
*: Procedure file: GLSNGAC.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow     *** Show proc. for home screen ***
*:                 lfvData_1  *** The laData[1] valid function ***
*:                 lfvData_5  *** Valid func. for term popup ***
*:                 lfvData_6  *** Valid func. for categry popup ***
*:                 lfvData_7  *** Valid func. for status popup ***
*:                 lfvData_8  *** Valid func. for allow posting check box ***
*:                 lfvData_9  *** Valid func. for ratio group popup ***
*:                 lfvData_10 *** Valid func. for cash flow popup ***
*:                 lfwData_11 *** When func. for automatic distribution ***
*:                 lfvData_11 *** Valid func. for automatic distribution ***
*:                 lfvData_14 *** Valid func.for consolidation % ***
*:                 lpSavScr   *** Save the new record ***
*:                 lpDelScr   *** Delete record ***
*:                 lfvEntry   *** Valid func. that call entry screen ***
*:                 lfvBalance *** Valid func. that call balance screen ***
*:                 lfwComYrsB *** When func. for year popup in balance screen ***
*:                 lfvComYrsB *** Valid func. for year popup in balance screen ***
*:                 lfvBalLst  *** Valid func. for balances list in balance screen ***
*:                 lfBalQuery *** func. that collect balances data ***
*:                 lfvBalEntr *** Valid func. that call entry screen from balance screen ***
*:                 lfvSegOK   *** Save record if calling from account validation ***
*:                 lfAddRetMj *** func. to add retained earnings account major ***
*:                 
*:      Documented 12/22/1993
*:
*:************************************************************************
*B600372,1 Reham On 06/01/95 
*B600372,1 1- Restore the right value if back to the from another screen.
*B600372,1 2- Change the hot key for <Entries...> button on the screen.
*B600372,1 3- Set filter again into ratio group file if left the screen 
*B600372,1    and back again
*B600919,1 Hesham El-Sheltawi 04/01/96
*B600919,1 under windows if changing the (CASH FLOW AND RATIO GROUP) FIELDS 
*B600919,1 call the gfUpdate() so the system can feel that there is modifications
*B600919,1 Happend
*B600926,1 Hesham El-Sheltawi 06/30/96
*B600926,1 set the default source journal as All instead of the default value
*B600926,1 in the setup file
*B601432,1 M.H 11/26/96 When you click on a year-period (or press Entries button),
*B601432,1              such that there is a debit or credit value for that period,
*B601432,1              you should be able to view the entries causing the activity 
*B601432,1              on this period. Sometimes, instead, we get a 'No activity for this period'
*B601432,1              message and the Entries screen opens to an empty browse.
*B601495,1 RENEE 12/22/96. Balances screen: go to another session an back :
*B601495,1                 a. Do not reinitialize the year popup.
*B601495,1                 b. Proper refreshing of the entries button
*B601570,1   Hesham El-Sheltawi 03/17/97
*B601570,1   Add the system manager module to the Gl Link modules for the company
*B601570,1   so when getting the entries we can get the close year & close per.
*B601570,1   account enteries
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*B601847,1 RENEE 08/20/97. Add a confirmation if adding a new account,
*B601847,1 				   then cancelling
*B602066,1 AMM 08/05/1998 Adjust the default short description of single account.
*E301024,1 AAMER 10/15/98 Collect the source modules from SyGLTran File
*B602218,1 AAMER 12/03/98 'Invalid function argument ..' bug upon entry
*B602218,1 				  of the screen. 
*E300789,6 MAB 03/03/1999 Remove company Id from all indecies and SQLs. 
*B602645,1 WALID 03/08/99 Proper deleting of elements from the source module 
*B602645,1 				  array.
*B602221,1 AAN 14/09/2000 At the screen change the format of the short and long Desc. by delete the table name
*B804399,1 SSE 09/23/2001 Fix bug of Updating Long description with short description, and not
*B804399,1                Updating Ratio group and cash flow.
*:************************************************************************
*
PARAMETERS pcAcontCd       && If calling from the account validation.
*** To know if the prog. calling from the menu or account validation. ***
*** to know which screen to call the modal screen or the nonmodal one ***
llSngMd = IIF(TYPE("pcAcontCd") $ "UL",.F.,.T.)

EXTERNAL ARRAY laData,laKeyField,laScrMode

DECLARE laKeyField [1,4]        && Have the tag information.

laKeyField[1,1] = 'laData[1]'   && The 1st var that has the index expr.
laKeyField[1,2] =.T.            && If this field terminatig for index or not.
laKeyField[1,3] = 'ACCTCODE'    && Tag name.
laKeyField[1,4] = 1             && Position of field inside index.

**** Single Account Arrays ****
DECLARE laRecNo   [1,1]    && Array to hold the record no to delete.

DECLARE laWndObj  [3,3]    && Have screen name & first & last obj. for each screen.

*** The status popup array ***
DECLARE laTStatus [2,2]
laTStatus [1,1] = "Active"
laTStatus [1,2] = "A"
laTStatus [2,1] = "Inactive"
laTStatus [2,2] = "I"

*** The categry popup array ***
DECLARE laTCatgry [4,2]
laTCatgry [1,1] = "Cash"
laTCatgry [1,2] = "C"
laTCatgry [2,1] = "Non cash"
laTCatgry [2,2] = "N"
laTCatgry [3,1] = "Depreciation"
laTCatgry [3,2] = "D"
laTCatgry [4,1] = "Amortization"
laTCatgry [4,2] = "A"

*** The term popup array ***
DECLARE laTTerm   [2,2]
laTTerm   [1,1] = "Long term"
laTTerm   [1,2] = "L"
laTTerm   [2,1] = "Current"
laTTerm   [2,2] = "C"

****  Entries Arrays ****
DECLARE laCompYrs [1,1]    && Array to hold years for this company.
DECLARE laYerPer  [1,2]    && array to hold all the periods for these years.
DECLARE laPrdNo   [1,1]    && array to hold all the periods for these years.
DECLARE laAppDesc [1,2]    && Collect Applications descriptions.
DECLARE laAppID   [1,1]    && Hold Applications ID.
DECLARE laSJDesc  [1,2]    && Collect source journal descriptions.
DECLARE laSorcJor [1,1]    && Hold source journals ID.

DECLARE laAppl    [1,1]    && Hold Applications ID
DECLARE laSorcID  [1,1]    && Hold source journals ID.

**** Balances Arrays ****
DECLARE laYrPrBal [1,2]    && Array to hold all the periods for these years.
DECLARE laBalance [1,1]    && Array to hold all the balance records for this acc.

*** Varibles related to the main screen (Single account) ***
laDefProc    = .T.     && Default value for this array elements is .T.
laDefProc[7] = .F.     && Use local Delete procedure.
laDefProc[9] = .F.     && Use local Save   procedure.

lcTypCode  = ""        && Var to hold code type.
lcTypDesc  = ""        && Var to hold code description.
lcAccDes   = ""        && To combine account desc.
lcOldAuto  = ""        && Hold the old automatic distribution.
lcOldAcc   = ""        && Hold the old account.
lcModName  = "General ledger"   && String to store general ladger module name
llChild    = .F.       && Var use in the child screen.

lcRatioDes = ""        && Ratio group say var.
puRatio    = ""
puRatioGrp = ""
lcCashFDes = ""        && Cash flow say var.
puCash     = ""
puCashFlow = ""
lcTerm     = ""        && Term say var.
lcCatgry   = ""        && Categry say var.
lcStatus   = ""        && Status say var.
cbPost     = 1         && Default value for allow posting check box.

*** Varibles related to the Entry screen ***
lcOldYear  = ""        && Var to hold old year.
lcOldPer   = ""        && Var to hold old period.
lnFrstElm  = 1         && Var to hold the first element in the period popup.
lnElmNo    = 1         && Var to hold no. of elements in the period popup.
lcAppIdStr = ""        && Var to hold the App. ID to select the data.
lcSJ_IdStr = ""        && Var to hold the source journal to select the data.
lcBrowEntr = ""        && Var to hold the fields name for the browse.
lcCursor1  = ""        && Var hold the name of the temp. file used in entries.
lcWid1     = ""        && Width of batch no. field.
lcWid2     = ""        && Width of transaction # field.
lcWid3     = ""        && Width of source modul field.
lcWid4     = ""        && Width of date field.
lcWid5     = ""        && Width of debit or credit amount.
lcWid6     = ""        && Width of source journal field.

*** Varibles related to the Detail screen ***
lcTypCode2 = ""        && Var to hold the code type.
lcBrowDet  = ""        && Var to hold the fields name for the browse.
lcCursor2  = ""        && Var to hold name of the temp. file used in details.
lcWidD1    = ""        && Width of debit amount.
lcWidD2    = ""        && Width of credit amount.

*** Varibles related to the Balance screen ***
puCompYrs2 = ""        && Var of the popup hold the year.
lcCompYrs2 = ""        && Var of the popup hold the year.
lcOldYear2 = ""        && Var to hold old year.
lcExp      = ""        && To save index expr. of glbalance.
laBalance  = ""        && Put in all the array element none value.
lsBalance  = 1         && default var for the list.
llEndBal   = .F.       && To display specific string in balance screen.
*lcTbalance = ""        && Hold the balances screen title.
lcTbalance = "Balances"
lnWid_Fld  = 16
cbSummary  = .F.       && Variable of the summary check box.
llBrowse   = .F.
lcAccount  = ''        && Variable to hold the account to compair with.

lcPopTerm = " "
lcPopCatg = " "
lcPopStat = " "
lcPopRato = " "
lcPopCash = " "

*** Var to hold the child window name. ***
lcChldWind = " "

*** Function in the main program ***
*** Intialise all the variables & open all the files needed   ***
*** in this session and controling disabling and enabling of  ***
*** the menu bars and writting the screen names in the window ***
*** bars ....

*B601495,1 Add a variable for the Entries button state
lcBalStat  = ''
*B601495,1 end

IF !gfSetup()    
  RETURN
ENDIF  

llSngMd    = IIF(TYPE("pcAcontCd") $ "UL",.F.,.T.)

SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

IF !WEXIST(gcBaseWind)
  *** Get random name for the balances window. ***
  lcChldWind = "CWR"+SUBSTR(ALLTRIM(gfTempName()),1,7)

  lcCursor1  = gfTempName()     && Get a random name for the 1st temp. file.
  lcCursor2  = gfTempName()     && Get a random name for the 2nd temp. file.
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[1]  = REPLICATE("0",lnAcsSegSz)   
  
  *** Get the compny years that available in the fiscal year header file. ***
*  SELECT cFisfyear FROM &gcSysHome.SYCFISHD;
         WHERE CCOMP_ID+CFISFYEAR = gcAct_Comp;
         INTO ARRAY laCompYrs;
         ORDER BY cFisfyear ;
         DISTINCT

 *E300692,1 Change file name and path from SYCFISHD to FISHD  
   *SELECT cFisfyear FROM &gcSysHome.SYCFISHD;
         WHERE CCOMP_ID+CFISFYEAR = gcPrnt_Cmp;
         INTO ARRAY laCompYrs;
         ORDER BY cFisfyear ;
         DISTINCT
         
  *E300789,6 FISHD now does have company Id in its index [begin
   *SELECT cFisfyear FROM &gcDataDir.FISHD;
   *      WHERE CCOMP_ID+CFISFYEAR = gcPrnt_Cmp;
   *      INTO ARRAY laCompYrs;
   *      ORDER BY cFisfyear ;
   *      DISTINCT
  SELECT cFisfyear FROM &gcDataDir.FISHD;
         INTO ARRAY laCompYrs;
         ORDER BY cFisfyear ;
         DISTINCT
  *E300789,6 FISHD now does have company Id in its index [end


 *E300692,1 end        
  
  *** Get the current company years {previous-current-next } and ***
  *** the  historical years  and its  periods to use them in the ***
  *** balances screen popups in a two dimention array.
*  SELECT cFisfyear,cFspprdid ;
         FROM &gcSysHome.SYCFSPRD;
         WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = gcAct_Comp .AND. ;
         ASCAN(laCompYrs,cFisfyear) > 0 ;
         INTO ARRAY laYerPer;
         ORDER BY cFisfyear,cFspprdid ;
         DISTINCT

*E300692,1 Change file name and path from SYCFSPRD to FSPRD    
  *SELECT cFisfyear,cFspprdid ;
         FROM &gcSysHome.SYCFSPRD;
         WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = gcPrnt_Cmp .AND. ;
         ASCAN(laCompYrs,cFisfyear) > 0 ;
         INTO ARRAY laYerPer;
         ORDER BY cFisfyear,cFspprdid ;
         DISTINCT
         
  *E300789,6 FSPRD now does not have company Id in its index[Begin
  *SELECT cFisfyear,cFspprdid ;
  *       FROM &gcDataDir.FSPRD;
  *       WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = gcPrnt_Cmp .AND. ;
  *       ASCAN(laCompYrs,cFisfyear) > 0 ;
  *       INTO ARRAY laYerPer;
  *       ORDER BY cFisfyear,cFspprdid ;
  *       DISTINCT

  SELECT cFisfyear,cFspprdid ;
         FROM &gcDataDir.FSPRD;
         WHERE ASCAN(laCompYrs,cFisfyear) > 0 ;
         INTO ARRAY laYerPer;
         ORDER BY cFisfyear,cFspprdid ;
         DISTINCT

  *E300789,6 FSPRD now does not have company Id in its index[end
  *E300692,1 end       
  
  *** Delete the years column.
  IF !EMPTY(laYerPer[1,1])
    =gfADel(@laYerPer,1,2)
  ELSE
    DECLARE laYerPer[1,1]
    laYerPer = " "
  ENDIF

  *** Get the current company years {previous-current-next } and ***
  *** its periods to use them in the balances screen popups in a ***
  *** two dimention array.
*  SELECT DISTINCT CFISFYEAR,CFSPPRDID;
         FROM &gcSysHome.SYCFSPRD;
         WHERE ccomp_id=gcAct_Comp .AND.;
         VAL(CFISFYEAR) >= lnCurr_yer - 1;
         ORDER BY CFISFYEAR,CFSPPRDID;
         INTO ARRAY laYrPrBal

*E300692,1 Change file name and path from SYCFSPRD to FSPRD      
  *SELECT DISTINCT CFISFYEAR,CFSPPRDID;
         FROM &gcSysHome.SYCFSPRD;
         WHERE ccomp_id=gcPrnt_Cmp .AND.;
         VAL(CFISFYEAR) >= lnCurr_yer - 1;
         ORDER BY CFISFYEAR,CFSPPRDID;
         INTO ARRAY laYrPrBal
         
  *E300789,6 FSPRD now does not have company Id in its index[Begin
  *SELECT DISTINCT CFISFYEAR,CFSPPRDID;
  *       FROM &gcDataDir.FSPRD;
  *       WHERE ccomp_id=gcPrnt_Cmp .AND.;
  *       VAL(CFISFYEAR) >= lnCurr_yer - 1;
  *       ORDER BY CFISFYEAR,CFSPPRDID;
  *       INTO ARRAY laYrPrBal

  SELECT DISTINCT CFISFYEAR,CFSPPRDID;
         FROM &gcDataDir.FSPRD;
         WHERE VAL(CFISFYEAR) >= lnCurr_yer - 1;
         ORDER BY CFISFYEAR,CFSPPRDID;
         INTO ARRAY laYrPrBal

  *E300789,6 FSPRD now does not have company Id in its index[end
 *E300692,1 end        
         

  *** collect the no. of periods for each year. ***
*  SELECT cFisNoPrd,cFisfyear FROM &gcSysHome.SYCFISHD;
         WHERE cComp_ID+cFisfyear = gcAct_Comp;
         INTO ARRAY laPrdNo;
         ORDER BY cFisfyear

*E300692,1 Change file name and path from SYCFISHD to FISHD        
  *SELECT cFisNoPrd,cFisfyear FROM &gcSysHome.SYCFISHD;
         WHERE cComp_ID+cFisfyear = gcPrnt_Cmp;
         INTO ARRAY laPrdNo;
         ORDER BY cFisfyear
  
  *E300789,6 FISHD now does have company Id in its index [begin
  *SELECT cFisNoPrd,cFisfyear FROM &gcDataDir.FISHD;
  *       WHERE cComp_ID+cFisfyear = gcPrnt_Cmp;
  *       INTO ARRAY laPrdNo;
  *       ORDER BY cFisfyear

  SELECT cFisNoPrd,cFisfyear FROM &gcDataDir.FISHD;
         INTO ARRAY laPrdNo;
         ORDER BY cFisfyear
  
  *E300789,6 FISHD now does have company Id in its index [end
 *E300692,1 end        
         

  IF !EMPTY(laPrdNo[1,1])
    =gfADel(@laPrdNo,2,2)
  ELSE
    DECLARE laPrdNo[1,1]
    laPrdNo = " "
  ENDIF

  *** Get the available source journals ID and its descriptions. ***
  SELECT GLSUBJOR.CSRCJRNL,;
         IIF(EMPTY(cjorshdes),;
         PADR(csrcjrnl,FSIZE('cjorshdes'),' '),;
         cjorshdes) AS 'cjorshdes';
    FROM &gcDataDir.GLSUBJOR ;
    INTO ARRAY laSJDesc

  *** Separate the two dimentional arrayinto two arrays each one ***
  *** is one dimentional array, one for ID and the other for the ***
  *** descriptions.
  IF !EMPTY(laSJDesc[1,1])
    DECLARE laSorcID[ALEN(laSJDesc,1),2]
    =ACOPY(laSJDesc,laSorcID)
    =gfADel(@laSorcID,2,2)riptions in a two ***
    =gfADel(@laSJDesc,1,2)
  ELSE
    DECLARE laSorcID[1,1],laSJDesc[1,1]
    STORE " " TO laSorcID,laSJDesc
  ENDIF

  *** Collect the available module and its desc    
  *** dimentional array.
  *SELECT cApp_id,cApp_name FROM &gcSysHome.SYDAPPL;
         WHERE cUpgrdlvl = 'A';
         INTO ARRAY laAppDesc

  
  *E301024,1 (AAMER 10/15/98)
  *E301024,1 Collect the source modules from SyGLTran File (Start)

  *-- Comment this part of code and collect the source modules from
  *-- G/L Transaction types file SYGLTRAN
  **** Added by Yasser on 3-11-95
  *SELECT GLSETUP
  *DECLARE laAppDesc[1,2]
  *laAppDesc = ""
  *=gfSUBSTR(glsetup.mlinkapp,@laAppDesc,"|~")

  *IF !EMPTY(laAppDesc[1,1])
  *  DECLARE laAppDesc[ALEN(laAppDesc,1)+1,2]
  *ENDIF

  *laAppDesc[ALEN(laAppDesc,1),1] = "GL"
  *laAppDesc[ALEN(laAppDesc,1),2] = lcModName

  **B601570,1  Hesham El-Sheltawi (Start)
  **B601570,1  add the SM module to the installed modules to the
  **B601570,1  company
  *IF !EMPTY(laAppDesc[1,1])
  *  DECLARE laAppDesc[ALEN(laAppDesc,1)+1,2]
  *ENDIF

  *laAppDesc[ALEN(laAppDesc,1),1] = "SM"
  *laAppDesc[ALEN(laAppDesc,1),2] = "System Manager"
  **B601570,1  Hesham El-Sheltawi (End)

  SELECT DISTINCT SyGLTran.cSrcModul,SydAppl.cApp_Name ;
    FROM SyGLTran,SydAppl ;
    INTO ARRAY laAppDesc  ;
    WHERE SyGLTran.cSrcModul = SydAppl.cApp_ID

  IF !EMPTY(laAppDesc[1,1])
    SELECT SycComp
    PRIVATE lcCurTag
    lcCurTag = ORDER()
    SET ORDER TO Ccomp_id
    IF SEEK(gcAct_Comp)
      PRIVATE lnI,lnDelCnt,lnArLen
      STORE 0 TO lnDelCnt,lnArLen

      *B602218,1 Exchange the 'FOR' loop with a 'DO WHILE' so as to have 
      *B602218,1 a flexible limit to the loop
      *FOR lnI = 1 TO ALEN(laAppDesc,1)
      *  IF !(laAppDesc[lnI,1] $ SycComp.mModlSet)
      *    lnDelCnt = lnDelCnt + 1
      *    =ADEL(laAppDesc,lnI)
      *  ENDIF
      *ENDFOR
      
      *B602645,1 WALID [BEGIN]
      * The following code will not fix the problem of deleting an 
      * elements from the source module array as follow
      * when lnI=8 the condition !(laAppDesc[lnI,1] $ SycComp.mModlSet)
      * is .T. so laAppDesc[8,1] will be deleted and laAppDesc[9,1]
      * laAppDesc[9,1] will automatically be shifted to be laAppDesc[8,1]
      * Now lnI=9 but lnArLen is modified to be 8 so DO WHILE loop
      * will termenated without check laAppDesc[9,1] .
      * 
*      lnI = 1
*      lnArLen = ALEN(laAppDesc,1)
*      DO WHILE lnI <= lnArLen
*        IF !(laAppDesc[lnI,1] $ SycComp.mModlSet)
*          lnDelCnt = lnDelCnt + 1
*          =ADEL(laAppDesc,lnI)
*          lnArLen = lnArLen - 1
*        ENDIF
*        lnI = lnI + 1
*      ENDDO
*      *B602218,1 end
      
*      IF lnDelCnt > 0
*        DIMENSION laAppDesc[ALEN(laAppDesc,1)-lnDelCnt,2]
*      ENDIF

      DIMENSION laDumm(ALEN(laAppDesc,1),1)
      FOR lnI = 1 TO ALEN(laAppDesc,1)
        IF !(laAppDesc[lnI,1] $ SycComp.mModlSet)
          laDumm[LNI,1] = .T.
          lnDelCnt = lnDelCnt + 1
        ENDIF
      ENDFOR
      IF lnDelCnt > 0 
        lnDelCnt =0
        FOR lnI = 1 TO ALEN(laDumm,1)
          IF laDumm[LNI,1]
          =ADEL(laAppDesc,lnI)
          lnDelCnt = lnDelCnt+1
          ENDIF
        ENDFOR
        DIMENSION laAppDesc[ALEN(laAppDesc,1)-lnDelCnt,2]
      ENDIF
    *B602645,1 [END..]
    
  ELSE
      STORE " " TO laAppDesc
    ENDIF
    SET ORDER TO &lcCurTag
  ENDIF  
  *E301024,1 (AAMER 10/15/98) 
  *E301024,1 Collect the source modules from SyGLTran File (End)


  *** Separate the two dimentional array into two arrays.  each one ***
  *** is one dimentional array, one for applications ID & the other ***
  *** one for the applications descriptions.
  IF !EMPTY(laAppDesc[1,1])
    DECLARE laAppID[ALEN(laAppDesc,1),2]
    =ACOPY(laAppDesc,laAppID)
    =gfADel(@laAppID,2,2)
    =gfADel(@laAppDesc,1,2)
  ELSE
    DECLARE laAppID[1,1],laAppDesc[1,1]
    STORE " " TO laAppID,laAppDesc
  ENDIF

  *** Put all the applications as defaulted selected in the mover ***
  *** and in the string that hold the moduls id in the SQL command ***
  DECLARE laAppl[ALEN(laAppID,1),1]  
  FOR lnCount = 1 TO ALEN(laAppID,1)
    lcAppIdStr = IIF(EMPTY(lcAppIdStr),lcAppIdStr,lcAppIdStr+",") + laAppID[lnCount]
   * laAppl[lnCount,1]  = ALLTRIM(LOOKUP(SYDAPPL.cApp_name,laAppID[lnCount,1],SYDAPPL.cApp_id,'cApp_id'))
    laAppl[lnCount,1]  = ALLTRIM(laAppDesc[lnCount,1])
  ENDFOR

*B600926,1 Hesham El-Sheltawi (Start)
*B600926,1 set the default source journal all instead of the default value
*B600926,1 in the setup file
  *** Put the {GJ} as default selected in the mover and the SQL command. ***
*  IF ASCAN(laSorcID,lcSJ_Def) <> 0
*    laSorcJor[1,1] = laSJDesc[ASCAN(laSorcID,lcSJ_Def),1]
*  ENDIF

*  lcSJ_IdStr   = lcSJ_Def
  lcSJ_IDStr   = ""
  DIMEN laSorcJor[ALEN(laSJDesc,1),1]
  FOR lnCount  = 1 TO ALEN(laSJDesc,1)
      laSorcJor[lnCount,1] = laSJDesc[lnCount,1]
      lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
                   + ALLTRIM(laSorcJor[lnCount,1])
  ENDFOR
*B600926,1 Hesham El-Sheltawi (End)

  *** Get the GLACBALS tag. ***
  SELECT GLACBALS
  SET ORDER TO TAG ACCYRPRD
  lcExp = SYS(14,VAL(SYS(21)))
  
  IF _WINDOWS
    IF llSngMd
      puRatio = "N/A"
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 16.333+1.538,17.875 TO 23,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_9
      
      puCash = "N/A"
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 18.583+1.538,17.875 TO 25,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_10
    ELSE
      puRatio = "N/A"
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 16.833+1.538,18.625 TO 23,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_9
      
      puCash = "N/A"
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 19.167+1.538,18.625 TO 25,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_10
    ENDIF
  ELSE
    lcRatioDes = "N/A"
    lcCashFDes = "N/A"
  ENDIF
  *B601495,1 Initialize popup variables if the window does not exist only
  puCompYrs2 = lcCurr_yer
  lcCompYrs2 = lcCurr_yer
*B601495,1 Add an ELSE
ELSE
  puCompYrs2 = lcCompYrs2  
  *B601495,1 end
ENDIF

SELECT GLACCHAR

*B601495,1 Move those lines if !WEXIST() (above)
*puCompYrs2 = lcCurr_yer
*lcCompYrs2 = lcCurr_yer
*B601495,1 end

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "IBBROWKEY"
laWndObj [1,3] = "PBENTRY"

laWndObj [2,1] = "GWCCONTRL1"
laWndObj [2,2] = "PBTOP"
laWndObj [2,3] = "PBCLS"

laWndObj [3,1] = lcChldWind
laWndObj [3,2] = IIF(_DOS,"IBCOMPYRS2","PUCOMPYRS2")
laWndObj [3,3] = "PBBLCLOSE"

IF llSngMd
  *** If coming from the account validation func., call the modal screen. ***

  *** Put the account code segments in an array to scan in this ***
  *** array to get the  descriptions for each segment  for this ***
  *** account code to make its long description.
  DECLARE laAccSeg [1]
  =gfSubStr(ALLTRIM(pcAcontCd),@laAccSeg,"-")

  *** Get the account long description from the segment value file by ***
  *** collecting the short descriptions for the different segments.   ***
  laData[4] = ""
  FOR lnCount = 1 TO ALEN(laAccSeg,1)
    IF SEEK(STR(lnCount,1)+laAccSeg[lnCount],"GLSEGVAL")
      laData[4] = laData[4] + ;
                  IIF(EMPTY(laData[4]) .OR. RIGHT(laData[4],1)='-','','-');
                  + ALLTRIM(GLSEGVAL.cSegshdes)
      IF lnCount = 1
        *** Get the type of the first segment. ***
        lcTypCode = GLSEGVAL.ctypecode
        laData[3] = lcTypCode
        *** Get the type description. ***
        lcTypDesc = LOOKUP(GLTYPES.cTypedesc,lcTypCode,GLTYPES.cTypecode,'Typecode')

        *** Get the default data from the first segment. ***
        laData[2]  = GLSEGVAL.cSegshdes + SPACE(5)
        
        *B804399,1 Get the long description. [Begin]
        laData[4] = GLSEGVAL.cSegLndes + SPACE(25)
        *B804399,1 Get the long description. [End]
                
        laData[5]  = GLSEGVAL.cSegterm
        laData[6]  = GLSEGVAL.cSegaccat
        laData[7]  = GLSEGVAL.cSegactiv
        laData[8]  = GLSEGVAL.cSegalpos
        laData[9]  = GLSEGVAL.cSegratio
        laData[10] = GLSEGVAL.cSegcaflo
        laData[11] = GLSEGVAL.cSegautds
        laData[12] = GLSEGVAL.cSegcocac
        laData[13] = GLSEGVAL.nSegseqn
        laData[14] = GLSEGVAL.nSegconpr
      ENDIF
    ENDIF
  ENDFOR
  laData[4] = ALLTRIM(laData[4]) + SPACE(65 - LEN(laData[4]))

  lcPopTerm = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL',;
              "ENABLE","DISABLE")
  lcTerm    = IIF(AT(laData[5],"LC") > 0,;
              laTTerm[AT(laData[5],"LC"),1],"N/A")
  ibPopTerm = IIF(AT(laData[5],"LC") > 0,;
              AT(laData[5],"LC"),IIF(_DOS,0,"N/A"))

  lcPopCatg = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A',;
              "ENABLE","DISABLE")
  lcCatgry  = IIF(AT(laData[6],"CNDA") > 0 ,;
              laTCatgry[AT(laData[6],"CNDA"),1],"N/A" )
  ibPopCatg = IIF(AT(laData[6],"CNDA") > 0 ,;
              AT(laData[6],"CNDA"),IIF(_DOS,0,"N/A"))

  lcPopStat = IIF(GLSETUP.lSetacate,"ENABLE","DISABLE")
  lcStatus  = IIF(AT(laData[7],"AI") > 0 ,;
              laTStatus[AT(laData[7],"AI"),1],"N/A" )
  ibPopStat = IIF(AT(laData[7],"AI") > 0 ,;
              AT(laData[7],"AI"),IIF(_DOS,0,"N/A"))

  lcPopRato = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE")
  lcPopCash = IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE")
    
  cbPost = IIF(laData[8] = "Y",1,0)

  SELECT GLRACOD
  SET FILTER TO CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " "
  *** Get the ratio group desc. according to ratio group code. ***
  IF !EMPTY(laData[9])
    IF SEEK(LEFT(laData[3],1)+laData[9],"GLRACOD")
      lcRatioDes = GLRACOD.cRatDesc
      puRatio    = GLRACOD.cRatDesc
    ELSE
      lcRatioDes = "N/A"
      puRatio    = "N/A"
    ENDIF
  ELSE
    lcRatioDes = "N/A"
    puRatio    = "N/A"
  ENDIF

  *** Get the cash flow desc. according to cash flow code. ***
  IF _DOS
    lcCashFDes = LOOKUP(GLCFITEM.cCfidesc,laData[10],GLCFITEM.cSegcaflo,'CFICODE')
    lcCashFDes = IIF(EMPTY(lcCashFDes) , "N/A" , lcCashFDes)
  ELSE
    puCash = LOOKUP(GLCFITEM.cCfidesc,laData[10],GLCFITEM.cSegcaflo,'CFICODE')
    puCash = IIF(EMPTY(puCash) , "N/A" , puCash)
  ENDIF

  SELECT GLACCHAR

  *** Go directly to the add mode if coming from the account ***
  *** validation func.
  laData[1]    = pcAcontCd
  lcAccStat    = "DISABLE"
  
  laScrMode    = .F.
  laScrMode[4] = .T.
  *E300683,5 Call *.SPR from screens directory
  * DO GLSNGMD.SPR 
  DO (gcScrDir + gcWinAppl + '\GLSNGMD.SPR')
  *E300683,5 end   
ELSE
  *** If branched from the menu call the nonmodal screen. ***

  lcPopTerm = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL',;
              "ENABLE","DISABLE"))
  lcPopCatg = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A',;
              "ENABLE","DISABLE"))
  lcPopRato = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE"))
  lcPopCash = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
              IIF(GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y',;
              "ENABLE","DISABLE"))
  lcPopStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",;
              IIF(GLSETUP.lSetacate,"ENABLE","DISABLE"))
  
  *B600372,1 Reham On 06/01/95 Restore the right value if back to 
  *B600372,1 the from another screen.
  cbPost = IIF(laData[8] = "Y",1,0)

  *B600372,1 Reham On 06/03/95 Set filter in the GLRACOD according to 
  *B600372,1 current type code.
  SELECT GLRACOD
  SET FILTER TO 
  SET FILTER TO CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " "

  SELECT GLACCHAR
  *E300683,5 Call *.SPR from screens directory
  * DO GLSNGAC.SPR 
  DO (gcScrDir + gcWinAppl + '\GLSNGAC.SPR')
  *E300683,5 end   
ENDIF

SELECT GLRACOD
SET FILTER TO


IF glQuitting 
  *** If quit from this screen close its child screen. ***
  RELEASE POPUP puRatioGrp
  RELEASE POPUP puCashFlow

  IF WEXIST(lcChldWind)
    RELEASE WINDOW &lcChldWind
  ENDIF
  
  *** close all the curors that opened in this program. ***
  IF USED(lcCursor1)
    USE IN ALIAS(lcCursor1)
  ENDIF

  IF USED(lcCursor2)
    USE IN ALIAS(lcCursor2)
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    *** Select mode ***
    *** Put default for var. & popups & check boxes. ***
    STORE "N/A" TO lcRatioDes,puRatio,lcCashFDes,puCash
    lcTerm     = "N/A"      &&  laTTerm[1,1]
    ibPopTerm  = 1
    lcCatgry   = "N/A"      &&  laTCatgry[1,1]
    ibPopCatg  = 1
    lcStatus   = "N/A"      &&  laTStatus[1,1]
    ibPopStat  = 1
    cbPost     = 1
    cbSummary  = .F.
    
    *** Redeclare the balance array to put new data in it. ***
    DIMENSION laBalance [1,1]
    laBalance  = ""
    
    *** Put zeros in the account mask. ***
    laData[1]  = REPLICATE("0",lnAcsSegSz)   
    lcTypCode  = ""
    lcTypDesc  = ""

    SHOW GET ibPopTerm  DISABLE
    SHOW GET ibPopCatg  DISABLE
    SHOW GET ibPopStat  DISABLE
    IF _DOS
      SHOW GET ibPopRatio DISABLE
      SHOW GET ibPopCashF DISABLE
    ELSE
      SHOW GET ibRatio DISABLE
      SHOW GET ibCash  DISABLE
      SHOW GET puRatio DISABLE
      SHOW GET puCash  DISABLE
    ENDIF
    
  CASE laScrMode[2]
    *** View mode ***
    SHOW GET ibPopTerm  DISABLE
    SHOW GET ibPopCatg  DISABLE
    SHOW GET ibPopStat  DISABLE
    IF _DOS
      SHOW GET ibPopRatio DISABLE
      SHOW GET ibPopCashF DISABLE
    ELSE
      SHOW GET ibRatio DISABLE
      SHOW GET ibCash  DISABLE
      SHOW GET puRatio DISABLE
      SHOW GET puCash  DISABLE
    ENDIF
    *** Put the var. & popups values according ***
    *** to its values in the file & ladata...
    lcTerm     = IIF(AT(laData[5] , "LC") > 0 , laTTerm[AT(laData[5] , "LC"),1] , "N/A" )
    ibPopTerm  = AT(laData[5] , "LC")
    
    lcStatus   = IIF(AT(laData[7] , "AI") > 0 , laTStatus[AT(laData[7] , "AI"),1] , "N/A" )
    ibPopStat  = AT(laData[7] , "AI")
    
    lcCatgry   = IIF(AT(laData[6] , "CNDA") > 0 , laTCatgry[AT(laData[6] , "CNDA"),1] , "N/A" )
    ibPopCatg  = AT(laData[6] , "CNDA")
    
    cbPost = IIF(laData[8] = "Y",1,0)
  
    *** Get the ratio group desc. according to ratio group code. ***
    IF !EMPTY(laData[9])
      IF SEEK(LEFT(laData[3],1)+laData[9],"GLRACOD")
        puRatio    = GLRACOD.cRatDesc
        lcRatioDes = GLRACOD.cRatDesc
      ELSE
        puRatio    = "N/A"
        lcRatioDes = "N/A"
      ENDIF
    ELSE
      puRatio    = "N/A"
      lcRatioDes = "N/A"
    ENDIF
      
    *** Get the cash flow desc. according to cash flow code. ***
    IF _DOS
      lcCashFDes = LOOKUP(GLCFITEM.cCfidesc,laData[10],GLCFITEM.cSegcaflo,'CFICODE')
      lcCashFDes = IIF(EMPTY(lcCashFDes) , "N/A" , lcCashFDes)
    ELSE
      puCash = LOOKUP(GLCFITEM.cCfidesc,laData[10],GLCFITEM.cSegcaflo,'CFICODE')
      puCash = IIF(EMPTY(puCash) , "N/A" , puCash)
    ENDIF

    SHOW GET pbBalance  ENABLE
    SHOW GET pbEntry    ENABLE

    *** Get the type code & desc. ***
    lcTypCode  = laData[3]
    lcTypDesc  = LOOKUP(GLTYPES.cTypedesc,laData[3],GLTYPES.cTypecode,'Typecode')

    *** Gather data from Balance file ***
    cbSummary  = .F.
    puCompYrs2 = lcCurr_yer
    lcCompYrs2 = lcCurr_yer
    lcAccPic   = STRTRAN(lcAcsMask,'9','0')
    *B602066,1 AMM Adjust in case of single segment companys.
    *lcFrstSeg  = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    IF AT("-",laData[1]) = 0
      lcFrstSeg  = laData[1]
    ELSE
      lcFrstSeg  = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    ENDIF
    *B602066,1 AMM end    
    lcAccount  = STUFF(lcAccPic,0,AT("-",laData[1])-1,lcFrstSeg)
    =lfBalQuery()

    SELECT GLACCHAR

  CASE laScrMode[3]
    *** Edit mode ***
    *** Set filter to account type in the ratio group file. ***
    SELECT GLRACOD
    SET FILTER TO 
    SET FILTER TO CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " "
    SELECT GLACCHAR

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL'
      SHOW GET ibPopTerm ENABLE
    ELSE
      SHOW GET ibPopTerm DISABLE
    ENDIF

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A'
      SHOW GET ibPopCatg ENABLE
    ELSE
      SHOW GET ibPopCatg DISABLE
    ENDIF
    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y'
      IF _DOS
        SHOW GET ibPopRatio  ENABLE
        SHOW GET ibPopCashF  ENABLE
      ELSE
        SHOW GET ibRatio ENABLE
        SHOW GET ibCash  ENABLE
        SHOW GET puRatio ENABLE
        SHOW GET puCash  EnABLE
      ENDIF
    ELSE
      IF _DOS
        SHOW GET ibPopRatio  DISABLE
        SHOW GET ibPopCashF  DISABLE
      ELSE
        SHOW GET ibRatio DISABLE
        SHOW GET ibCash  DISABLE
        SHOW GET puRatio DISABLE
        SHOW GET puCash  DISABLE
      ENDIF
    ENDIF
    IF GLSETUP.lSetacate
      SHOW GET ibPopStat ENABLE
    ELSE
      SHOW GET ibPopStat DISABLE
    ENDIF

    SHOW GET  pbBalance  ENABLE
    SHOW GET  pbEntry    ENABLE

  CASE laScrMode[4]
    *** Add mode ***
    *B601847,1 Set llCUpdate, to be checked in global cancel
    llCUpdate = .T.
    *B601847,1 end

    *** Copy the default account value from segment ***
    *** file according to the first segment data...
    *B602066,1 AMM Adjust in case of single segment companys.
    *lcFrstSeg = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    IF AT("-",laData[1]) = 0
      lcFrstSeg  = laData[1]
    ELSE
      lcFrstSeg = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    ENDIF
    *B602066,1 AMM end
    IF SEEK("1"+lcFrstSeg,"GLSEGVAL")
      laData[2]  = GLSEGVAL.cSegshdes + SPACE(5)

      *B804399,1 Get the long description. [Begin]
      laData[4] = GLSEGVAL.cSegLndes + SPACE(25)
      *B804399,1 Get the long description. [End]
      
      laData[5]  = GLSEGVAL.cSegterm
      laData[6]  = GLSEGVAL.cSegaccat
      laData[7]  = GLSEGVAL.cSegactiv
      laData[8]  = GLSEGVAL.cSegalpos
      laData[9]  = GLSEGVAL.cSegratio
      laData[10] = GLSEGVAL.cSegcaflo
      laData[11] = GLSEGVAL.cSegautds
      laData[12] = GLSEGVAL.cSegcocac
      laData[13] = GLSEGVAL.nSegseqn
      laData[14] = GLSEGVAL.nSegconpr
   ENDIF

    laData[3]  = lcTypCode
    
    laData[15] = IIF(SUBSTR(laData[3],1,1) = 'Y' , 'N' , 'Y')
    
    lcTerm     = IIF(AT(laData[5] , "LC") > 0 , laTTerm[AT(laData[5] , "LC"),1] , "N/A" )
    ibPopTerm  = AT(laData[5] , "LC")
    
    lcStatus   = IIF(AT(laData[7] , "AI") > 0 , laTStatus[AT(laData[7] , "AI"),1] , "N/A" )
    ibPopStat  = AT(laData[7] , "AI")
    
    lcCatgry   = IIF(AT(laData[6] , "CNDA") > 0 , laTCatgry[AT(laData[6] , "CNDA"),1] , "N/A" )
    ibPopCatg  = AT(laData[6] , "CNDA")
    
    cbPost     = IIF(laData[8] = "Y",1,0)
  
    *** Set filter to account type in ratio groups file. ***
    SELECT GLRACOD
    SET FILTER TO
    SET FILTER TO CRATTYPE = LEFT(laData[3],1) .OR. CRATTYPE = " "
    SELECT GLACCHAR

    *** Get the ratio group desc. according to ratio group code. ***
    IF !EMPTY(laData[9])
      IF SEEK(LEFT(laData[3],1)+laData[9],"GLRACOD")
        puRatio    = GLRACOD.cRatDesc
        lcRatioDes = GLRACOD.cRatDesc
      ELSE
        puRatio    = "N/A"
        lcRatioDes = "N/A"
      ENDIF
    ELSE
      puRatio    = "N/A"
      lcRatioDes = "N/A"
    ENDIF
    
    *** Get cash flow desc. ***
    IF _DOS
      lcCashFDes = IIF(EMPTY(laData[10]) , "N/A" , ;
                   LOOKUP(GLCFITEM.cCfidesc,laData[10],;
                   GLCFITEM.cSegcaflo,'CFICODE'))
    ELSE
      puCash     = IIF(EMPTY(laData[10]) , "N/A" , ;
                   LOOKUP(GLCFITEM.cCfidesc,laData[10],;
                   GLCFITEM.cSegcaflo,'CFICODE'))
    ENDIF

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) $ 'AL'
      SHOW GET ibPopTerm ENABLE
    ELSE
      SHOW GET ibPopTerm DISABLE
    ENDIF

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) = 'A'
      SHOW GET ibPopCatg ENABLE
    ELSE
      SHOW GET ibPopCatg DISABLE
    ENDIF

    IF GLSETUP.lSetacate .AND. LEFT(laData[3],1) <> 'Y'
      IF _DOS
        SHOW GET ibPopRatio ENABLE
        SHOW GET ibPopCashF ENABLE
      ELSE
        SHOW GET ibRatio ENABLE
        SHOW GET ibCash  ENABLE
        SHOW GET puRatio ENABLE
        SHOW GET puCash  ENABLE
      ENDIF
    ELSE
      IF _DOS
        SHOW GET ibPopRatio DISABLE
        SHOW GET ibPopCashF DISABLE
      ELSE
        SHOW GET ibRatio DISABLE
        SHOW GET ibCash  DISABLE
        SHOW GET puRatio DISABLE
        SHOW GET puCash  DISABLE
      ENDIF
    ENDIF
    IF GLSETUP.lSetacate
      SHOW GET ibPopStat  ENABLE
    ELSE
      SHOW GET ibPopStat  DISABLE
    ENDIF
    
    SHOW GET pbEntry    DISABLE
    SHOW GET pbBalance  DISABLE

ENDCASE

**** Refresh the child window objects ***

lsBalance = 1

*** If calling this program from the menu, balance screen ***
*** should  be exist, so enable all  its objects.  But if ***
*** calling from account validation, we cannot do this...
IF WEXIST(lcChldWind)
  SHOW GETS WINDOW &lcChldWind ENABLE ONLY
ENDIF

*** If there is no balances for any record disable the balances button. ***
IF ALEN(laBalance,1) = 1 .AND. EMPTY(laBalance[1,1])
  SHOW GET pbBalEntry DISABLE
  *B601495,1 Entries button 'Disable'
  lcBalStat        = 'DISABLE'
  *B601495,1 end
ELSE
  SHOW GET pbBalEntry ENABLE
  *B601495,1 Entries button 'Enable'
  lcBalStat        =  'ENABLE'
  *B601495,1 end  
ENDIF

IF laData[1] <> lcAccount
  cbSummary = .F.
*  =lfBalQuery()
  SHOW GET cbSummary DISABLE
ELSE
  SHOW GET cbSummary ENABLE
ENDIF

SELECT GLACCHAR

*!**************************************************************************
*!
*!      Function: lfwData_1
*!
*!**************************************************************************
*
*** laData[1] when function. {The account code}

FUNCTION lfwData_1

lcOldAcc = laData[1]

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
*** laData[1] valid function. {The account code}

FUNCTION lfvData_1
IF llBrowse
  laData[1] = STUFF(laData[1],1,1,'?')
ENDIF

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. lcOldAcc <> laData[1] .AND. LASTKEY() = 13)

  llFoundAcn = .F.     && To know if the acc. found in its file or not. ***
  
  *------------------------------------------*
  *** Call the account validation function ***
  *------------------------------------------*
  
  *** To add new account to the chart of account file. ***
  IF lfVldAccnt('A','A','A',.T.,@lcAccDes,@lcTypCode,@lcTypDesc,.T.)

    SELECT GLACCHAR
    IF llFoundAcn
      *** If the field not empty and found in the main file ***
      *** chart of account , it should skatter all the data ***
      *** from the main file and back to the view mode .... ***
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ELSE
      *** If the field not empty and not found also in the ***
      *** main file , it means adding new record , so it   ***
      *** should back to the add mode ...                  ***
 
      *B804399,1 Don't update Long with short description. [Begin]
      *laData[4]    = lcAccDes
      *B804399,1 Don't update Long with short description. [End]

      laScrMode    = .F.
      laScrMode[4] = .T.
      SHOW GETS
    ENDIF
    lcAccPic  = STRTRAN(lcAcsMask,'9','0')
    *B602066,1 AMM Adjust in case of single segment companys.
    *lcFrstSeg = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    IF AT("-",laData[1]) = 0
      lcFrstSeg  = laData[1]
    ELSE
      lcFrstSeg  = SUBSTR(laData[1],1,AT("-",laData[1])-1)
    ENDIF
    *B602066,1 AMM end
    lcAccount = STUFF(lcAccPic,0,AT("-",laData[1])-1,lcFrstSeg)
    cbSummary = .F.
    IF laData[1] = lcAccount
      SHOW GET cbSummary ENABLE
    ELSE
      SHOW GET cbSummary DISABLE
    ENDIF
    
    *B804399,1 ReInitialize the Ratio group and cash flow popup. [Begin]
    IF llSngMd
      puRatio = IIF(EMPTY(puRatio),"N/A",puRatio)
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 16.333+1.538,17.875 TO 23,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_9
      
      puCash = IIF(EMPTY(puCash),"N/A",puCash)
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 18.583+1.538,17.875 TO 25,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_10
    ELSE
      puRatio = IIF(EMPTY(puRatio),"N/A",puRatio)
      DEFINE POPUP puRatioGrp prompt field GLRACOD.cRatDesc scroll;
      FROM 16.833+1.538,18.625 TO 23,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puRatioGrp DO lfvData_9
      
      puCash = IIF(EMPTY(puCash),"N/A",puCash)
      DEFINE POPUP puCashFlow prompt field GLCFITEM.cCfidesc scroll;
      FROM 19.167+1.538,18.625 TO 25,60;
      MESSAGE gfObj_msg()
      ON SELECTION POPUP puCashFlow DO lfvData_10
    ENDIF
    *B804399,1 ReInitialize the Ratio group and cash flow popup. [End]
    
  ELSE
    *** Return with none, So back to select mode ***
    *** again to enter another account...
    SELECT GLACCHAR
    SHOW GET laData[1] ENABLE
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*
*** The term popup valid function. ***

FUNCTION lfvData_5

DO CASE
  CASE _DOS
    IF llSngMd
      laData[5] = gfActPop(10,2,14,15,'laTTerm',2,1,@lcTerm)
    ELSE
      laData[5] = gfActPop(8,2,12,15,'laTTerm',2,1,@lcTerm)
    ENDIF
    =lfRefresh()
  CASE _WINDOWS
    laData[5] = SUBSTR("LC" , ibPopTerm , 1)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
*** The category popup valid function. ***

FUNCTION lfvData_6

DO CASE
  CASE _DOS
    IF llSngMd
      laData[6] = gfActPop(10,30,16,45,'laTCatgry',2,1,@lcCatgry)
    ELSE
      laData[6] = gfActPop(8,30,14,45,'laTCatgry',2,1,@lcCatgry)
    ENDIF
    =lfRefresh()
  CASE _WINDOWS
    laData[6] = SUBSTR("CNDA" , ibPopCatg , 1)
ENDCASE 
 
*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
*** The status popup valid function. ***

FUNCTION lfvData_7

DO CASE
  CASE _DOS
    IF llSngMd
      laData[7] = gfActPop(10,59,14,72,'laTStatus',2,1,@lcStatus)
    ELSE
      laData[7] = gfActPop(8,59,12,72,'laTStatus',2,1,@lcStatus)
    ENDIF
    =lfRefresh()
  CASE _WINDOWS
    laData[7] = SUBSTR("AI" , ibPopStat , 1)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_8
*!
*!**************************************************************************
*
*** The allow posting for G/L valid function. ***

FUNCTION lfvData_8

laData[8] = IIF(cbPost = 0 , 'N' , 'Y')

*!**************************************************************************
*!
*!      Function: lfvData_9
*!
*!**************************************************************************
*
*** The ratio group code valid function. ***

FUNCTION lfvData_9

DO CASE
  CASE _DOS
    IF llSngMd
      laData[9] = gfActPop(12,22,19,62,'GLRACOD','CSEGRATIO','CRATDESC',@lcRatioDes)
    ELSE
      laData[9] = gfActPop(10,22,17,62,'GLRACOD','CSEGRATIO','CRATDESC',@lcRatioDes)
    ENDIF
    =lfRefresh()
  CASE _WINDOWS
    puRatio   = GLRACOD.CRATDESC
    laData[9] = GLRACOD.CSEGRATIO
    SHOW GET puRatio
    *B600919,1 Hesham (Start)
    *B600919,1 under windows if changing the (CASH FLOW AND RATIO GROUP) FIELDS 
    *B600919,1 call the gfUpdate() so the system can feel that there is modifications
    *B600919,1 Happend    
    =gfUpdate()
    *B600919,1 Hesham (End)    
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puRatioGrp
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
*
*** The cash flow code valid function. ***

FUNCTION lfvData_10

DO CASE
  CASE _DOS
    IF llSngMd
      laData[10] = gfActPop(11,24,17,71,'GLCFITEM','CSEGCAFLO','CCFIDESC',@lcCashFDes)
    ELSE
      laData[10] = gfActPop(11,22,17,69,'GLCFITEM','CSEGCAFLO','CCFIDESC',@lcCashFDes)
    ENDIF
    =lfRefresh()
  CASE _WINDOWS
    puCash     = GLCFITEM.CCFIDESC
    laData[10] = GLCFITEM.CSEGCAFLO
    SHOW GET puCash
    *B600919,1 Hesham (Start)
    *B600919,1 under windows if changing the (CASH FLOW AND RATIO GROUP) FIELDS 
    *B600919,1 call the gfUpdate() so the system can feel that there is modifications
    *B600919,1 Happend    
    =gfUpdate()
    *B600919,1 Hesham (End)    
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puCashFlow
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_11
*!
*!**************************************************************************
*
*** The automatic distribution code when function. ***

FUNCTION lfwData_11

lcOldAuto = laData[11]

*!**************************************************************************
*!
*!      Function: lfvData_11
*!
*!**************************************************************************
*
*** The automatic distribution valid function. ***

FUNCTION lfvData_11

SELECT GLACCHAR
lnRecordNo = RECNO()

DECLARE laAutoInfo[1,1]

laData[11] = PADR(ALLTRIM(laData[11]),8)
IF llBrowse .OR. !EMPTY(laData[11]) 

  IF llBrowse .OR. lcOldAuto <> laData[11]
    IF SEEK("D","GLAUTHD") .OR. llBrowse
  
      lcCurFile = ALIAS()
      SELECT GLAUTHD
      *** If found update the user name from automatic dist. file

      IF !SEEK("D"+laData[11],"GLAUTHD") .OR. llBrowse
    
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
          GO RECNO(0)
        ELSE
          GO TOP
        ENDIF

        lcSaveBrow     =  lcBrFields  && Save old valu of the variable
        lcBrFields     =  "cautcode :H='Distribution code',cautref :H='Distribution reference',"+;
                          "cautdes  :H='Distribution description'"
        lcSav_ttl      = lcFile_Ttl
        lcFile_Ttl     = "Automatic Distribution"
  
        laAutoInfo [1] = laData[11]

        =gfBrows("'D'","cautcode" ,"laAutoInfo")
    
        lcBrFields = lcSaveBrow 
        lcFile_Ttl = lcSav_ttl

        *** If cancel from the brow you will come back  with the same id
        IF laData[11] = laAutoInfo[1] 
          laData[11]  = lcOldAuto
        ELSE
          laData[11]  = laAutoInfo[1]

          SELECT GLAUTDT
          SET ORDER TO TAG TYPCODACC
          SELECT GLACCHAR
          SET ORDER TO TAG ACCTCODE
          SELECT GLAUTDT
          SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

          SELECT GLAUTDT
          IF SEEK("D"+laData[11],"GLAUTDT")
            llAutcd_Ok = .T.
            SCAN REST WHILE GLAUTDT.cAutcode = laData[11]
              IF GLACCHAR.cSegactiv = "I"
                *** This  automatic  distribution has an ***
                *** inactive account.  You cannot select ***
                *** this automatic distribution code. ***
                *** <  Ok  > ***
                =gfModalGen("TRM02194B00000","DIALOG")
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              IF GLACCHAR.cStandard <> laData[15]
                *** This automatic distribution code ***
                *** has a { standard : statistical } ***
                *** account.  You cannot select this ***
                *** automatic distribution code.
                *** <  Ok  > ***
                lcStandStr = IIF(GLACCHAR.cStandard = "Y","standard","statistical")
                =gfModalGen("TRM02195B00000","DIALOG",lcStandStr)
                llAutcd_Ok = .F.
                EXIT
              ENDIF
              
              SELECT GLAUTDT
            ENDSCAN
            IF !llAutcd_Ok
              laData[11]  = lcOldAuto
            ENDIF 
          ENDIF
          
          SELECT GLAUTDT
          SET RELATION TO

        ENDIF
      ENDIF  
      SELECT (lcCurFile)
    ELSE
      *** There is no records to display. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00052B00000","DIALOG")
      laData[11] = SPACE(8)
    ENDIF
  ENDIF
ENDIF

SELECT GLACCHAR
IF lnRecordNo > 0 .AND. lnRecordNo <= RECCOUNT()
  GO lnRecordNo
ENDIF

llBrowse = .F.

SHOW GET laData[11]

*!**************************************************************************
*!
*!      Function: lfvData_13
*!
*!**************************************************************************
*
*** The report seq. no. ***

FUNCTION lfvData_13

IF laData[13] < 0
  *** Negative values are not allowed. ***
  =gfModalGen("INM02036B00000","DIALOG")
  laData[13] = 0
  SHOW GET laData[13]
  _CUROBJ    = OBJNUM(laData[13])
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
*** The consolidation % valid function. ***

FUNCTION lfvData_14

DO CASE
  CASE laData[14] > 100
    *** Total percentage cannot exceed 100 %. ***
    =gfModalGen("TRM02017B00000","DIALOG","Percentage")
    laData[14] = 0
    SHOW GET laData[14]
    _CUROBJ    = OBJNUM(laData[14])
  CASE laData[14] < 0
    *** Negative values are not allowed. ***
    =gfModalGen("TRM02036B00000","DIALOG")
    laData[14] = 0
    SHOW GET laData[14]
    _CUROBJ    = OBJNUM(laData[14])
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
*** This procedure replaces the default Save procedure ***
*** for the push button "SAVE".

PROCEDURE lpSavScr
EXTERNAL ARRAY laScrMode    

SELECT GLACCHAR
*** This array element hold the field cStandard and it put in it ***
*** char. : "Y" if this account has standard type, 
*** and it put char. : "N" if this account has statistical type.
laData[15] = IIF(SUBSTR(lcTypCode,1,1) = 'Y' , 'N' , 'Y')

*** Save the record in the chart of accounts file. ***
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

*** Save this account balances in the balances file. ***
IF laScrMode[4]
  SELECT GLACBALS
  FOR lnCount = 1 TO ALEN(laYrPrBal,1)
    INSERT INTO &gcDataDir.GLACBALS ;
           (cAcctCode,cFisfYear,cFspPrdid);
           VALUES (laData[1],laYrPrBal[lnCount,1],laYrPrBal[lnCount,2])
    =gfAdd_Info()
  ENDFOR
  *** Call the retained earnings for account major func. ***
  =lfAddRetMj()
  SELECT GLACCHAR
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
* 
*** This procedure replaces the default delete procedure ***
*** for the push button DELETE in the control pannel.

PROCEDURE lpDelScr

*** Call the global delete function that in the GL.PRG to ***
*** delete this account and its related data.
IF lfDelAcnt(laData[1])
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF

*!**************************************************************************
*!
*!      Function: lfvEntry
*!
*!**************************************************************************
*
*** This is the entry push button valid func. that call the ***
*** entry screen from the main screen.

FUNCTION lfvEntry

*** Put defaults as current year & current period ***
*** before enter the screen. ***

puCompYrs  = lcCurr_yer
puYerPer   = RIGHT('0'+ALLTRIM(STR(lnCurr_Prd)),2)
lnFrstElm  = VAL(laPrdNo[ALEN(laPrdNo,1)-1])+1
lnElmNo    = VAL(laPrdNo[ALEN(laPrdNo,1)-1])

*** This call the screen with 5 parameters : { 1.current year - ***
*** 2.current period - 3.first element - 4.number of elements - ***
*** 5.flag to say that coming from the main screen}.
*E300683,5 Call programs from PRGS directory
*DO GLQENTR.PRG WITH puCompYrs , puYerPer , lnFrstElm , lnElmNo , .T.
DO (gcAppHome + gcWinAppl + '\GLQENTR.PRG ') WITH puCompYrs , puYerPer , lnFrstElm , lnElmNo , .T.
*E300683,5 end

SELECT GLACCHAR

*!**************************************************************************
*!
*!      Function: lfvBalance
*!
*!**************************************************************************
*
*** This the balance push button valid function that call ***
*** the child screen balances.

FUNCTION lfvBalance

*** This is the balances screen title. ***
lcEntTitl  = "Account Balances"

*** Put the current company as a default in the popup ***
*** that exist in the balances screen.

*B601432,1 M.H Begin.
*puCompYrs2 = lcCurr_Yer
*lcCompYrs2 = lcCurr_yer
puCompYrs2 = lcCompYrs2
*B601432,1 M.H End.

*** Activate the balances child screen. ***
=gfActWind(lcChldWind,lcTbalance)

SELECT GLACCHAR

*!**************************************************************************
*!
*!       Function: lfvComYrsB
*!
*!**************************************************************************
*
*** The year popup valid function that exist in the balances screen. ***

FUNCTION lfvComYrsB

lcOldYear2 = lcCompYrs2

DO CASE
  CASE _DOS
    lcCompYrs2 = gfActPop(0,32,6,40,'laCompYrs',1,1,@lcCompYrs2)
    =lfRefresh()
  CASE _WINDOWS
    lcCompYrs2 = puCompYrs2
ENDCASE

IF lcOldYear2 <> lcCompYrs2
  *** If change the year call the balances query function ***
  *** to collect the new year data in the laBalance array ***
  =lfBalQuery()
  IF ALEN(laBalance,1) = 1 .AND. EMPTY(laBalance[1,1])
    SHOW GET pbBalEntry DISABLE
    *B601495,1 Entries button 'Disable'
    lcBalStat        = 'DISABLE'
    *B601495,1 end
  ELSE
    SHOW GET pbBalEntry ENABLE
    *B601495,1 Entries button 'Enable'
    lcBalStat        = 'ENABLE'
    *B601495,1 end
  ENDIF
  *** Refresh the list exist in the balance screen. ***
  SHOW GET lsBalance ENABLE
ENDIF

*!**************************************************************************
*!
*!       Function: lfvBalLst
*!
*!**************************************************************************
*
*** The balances list valid function. ***

FUNCTION lfvBalLst

*** If double click or enter inside the  list call ***
*** the entry screen from the balances screen with ***
*** the active period at this time in the list.    ***
_CUROBJ = OBJNUM(pbBalEntry)
KEYBOARD "{ENTER}"

*!**************************************************************************
*!
*!       Function: lfBalQuery
*!
*!**************************************************************************
*
*** Balances query function that create the list array that hold ***
*** the balances information for the current account code. 

FUNCTION lfBalQuery

DIMENSION laBalance [1,1]

laBalance = ""

DO CASE
  CASE _DOS
    *** Display 16 char. from the numeric fields if _DOS. ***
    lnWid_Fld = 16
  CASE _WINDOWS
    *** Display 14 char. from the numeric fields if _WINDOWS. ***
    lnWid_Fld = 14
ENDCASE

IF SUBSTR(lcTypCode,1,1) $ ("AQLY")
  IF cbSummary

    lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
               STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
               LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')
               
    SELECT CFSPPRDID+'³'+;
           IIF(SUM(NACBOPBAL) < 0,;
              '('+STR(ABS(SUM(NACBOPBAL)),lnWid_FLD,2)+')',;
              ' '+STR(SUM(NACBOPBAL),lnWid_Fld,2)+' ')+'³'+;
           STR(SUM(NACBPTDDR),lnWid_Fld,2)+'³'+;
           STR(SUM(NACBPTDCR),lnWid_FLD,2)+'³'+;
           IIF(SUM(NACBCLBAL) < 0,;
              '('+STR(ABS(SUM(NACBCLBAL)),lnWid_FLD,2)+')',;
              ' '+STR(SUM(NACBCLBAL),lnWid_FLD,2)+' ')+' ';
      FROM &gcDataDir.GLACBALS;
      WHERE GLACBALS.CACCTCODE LIKE lcAccMaj;
            AND GLACBALS.CFISFYEAR = lcCompYrs2;
      GROUP BY GLACBALS.CFSPPRDID;
      ORDER BY GLACBALS.CFSPPRDID;
      INTO ARRAY laBalance
  ELSE
    SELECT GLACBALS.CFSPPRDID+"³"+;
           IIF(GLACBALS.NACBOPBAL<0,'('+STR(ABS(GLACBALS.NACBOPBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBOPBAL,lnWid_Fld,2)+' ' ) +"³"+;
           STR(GLACBALS.NACBPTDDR,lnWid_Fld,2)+"³"+STR(GLACBALS.NACBPTDCR,lnWid_Fld,2)+"³"+;
           IIF (GLACBALS.NACBCLBAL<0 ,'('+STR(ABS(GLACBALS.NACBCLBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBCLBAL,lnWid_Fld,2)+' ');
      FROM &gcDataDir.GLACBALS ;
      INTO ARRAY laBalance;
      WHERE &lcExp = laData[1] + lcCompYrs2;
      ORDER BY GLACBALS.CFSPPRDID
  ENDIF 
  llEndBal  = .T.
ELSE
  IF cbSummary

    lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
               STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
               LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')
               
    SELECT CFSPPRDID+'³'+;
           IIF(SUM(NACBOPBAL) < 0,;
               '('+STR(ABS(SUM(NACBOPBAL)),lnWid_FLD,2)+')',;
               ' '+STR(SUM(NACBOPBAL),lnWid_Fld,2)+' ')+'³'+;
           STR(SUM(NACBPTDDR),lnWid_Fld,2)+'³'+;
           STR(SUM(NACBPTDCR),lnWid_Fld,2)+'³'+;
           IIF(SUM(NACBYTDDR-NACBYTDCR) < 0,;
               '('+STR(ABS(SUM(NACBYTDDR-NACBYTDCR)),lnWid_FLD,2)+')',;
               ' '+STR(SUM(NACBYTDDR-NACBYTDCR),lnWid_Fld,2)+' ')+' ';
      FROM &gcDataDir.GLACBALS;
      WHERE GLACBALS.CACCTCODE LIKE lcAccMaj;
            AND GLACBALS.CFISFYEAR = lcCompYrs2;
      GROUP BY GLACBALS.CFSPPRDID;
      ORDER BY GLACBALS.CFSPPRDID;
      INTO ARRAY laBalance
  ELSE
    SELECT GLACBALS.CFSPPRDID+"³"+;
           IIF(GLACBALS.NACBOPBAL<0,'('+STR(ABS(GLACBALS.NACBOPBAL),lnWid_Fld,2)+')',;
           ' '+STR(GLACBALS.NACBOPBAL,lnWid_Fld,2)+' ' ) +"³"+;
           STR(GLACBALS.NACBPTDDR,lnWid_Fld,2)+"³"+STR(GLACBALS.NACBPTDCR,lnWid_Fld,2)+"³"+;
           IIF(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR>0,;
              ' '+STR(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR,lnWid_Fld,2)+' ',;
              '('+STR(ABS(GLACBALS.NACBYTDDR-GLACBALS.NACBYTDCR),lnWid_Fld,2)+')'); 
      FROM &gcDataDir.GLACBALS ;
      INTO ARRAY laBalance;
      WHERE &lcExp = laData[1] + lcCompYrs2;
      ORDER BY GLACBALS.CFSPPRDID
  ENDIF
  llEndBal  = .F.
ENDIF

IF WVISIBLE(lcChldWind)
  SHOW GET lsBalance ENABLE
ENDIF

*!**************************************************************************
*!
*!       Function: lfvBalEntr
*!
*!**************************************************************************
*
*** This is the entry push button valid function from the ***
*** the balances screen the call the entry screen.

FUNCTION lfvBalEntr

lcSavAls = ALIAS()

*** Put the year in the balances screen popup as default ***
*** in the entry screen popup and in the SQL command. 
puCompYrs  = lcCompYrs2

*** Put the current period in the list in the balances screen ***
*** as default period in the period popup in the entry screen ***
*** and in the SQL command.
puYerPer   = SUBSTR(laBalance[lsBalance,1],1,2)

*** Define the  first element no. of elements to send as ***
*** parameters to the entry screen according to the year ***
*** if it is previous or current or next.

lnYearNo = ASCAN(laCompYrs,puCompYrs)

IF lnYearNo > 0
  IF lnYearNo = 1
    lnFrstElm = 1
  ELSE
    lnFrstElm = 1
    FOR lnCount = 1 TO lnYearNo - 1
      lnFrstElm = lnFrstElm + VAL(laPrdNo[lnCount])
    ENDFOR
  ENDIF
  lnElmNo = VAL(laPrdNo[lnYearNo])
ELSE
  lnFrstElm = 1
  lnElmNo   = VAL(laPrdNo[1])
ENDIF

*** This call the screen with 5 parameters : { 1.current year - ***
*** 2.current period - 3.first element - 4.number of elements - ***
*** 5.flag to say that coming from the main screen}.
*E300683,5 Call programs from PRGS directory
*DO GLQENTR.PRG  WITH puCompYrs , puYerPer , lnFrstElm , lnElmNo , .F.
DO (gcAppHome + gcWinAppl + '\GLQENTR.PRG ') WITH puCompYrs , puYerPer , lnFrstElm , lnElmNo , .F.
*E300683,5 end
_CUROBJ = OBJNUM(lsBalance)

SELECT (lcSavAls)

*!**************************************************************************
*!
*!      FUNCTION: lfvSngOk
*!
*!**************************************************************************
* 
*** This is the OK push button valid function in the single account ***
*** screen if it is modal screen called from the account validation ***
*** function.  { Its work is like the save procedure in the case of ***
*** calling the screen as nonmodal from the menu. } 

FUNCTION lfvSngOK

*** This is to quit from this program directly ***
*** after finishing the save process.
glQuitting  = .T.
llSavSngAc  = .T.

SELECT GLACCHAR

*** This array element hold the field cStandard and it put in it ***
*** char. : "Y" if this account has standard type, 
*** and it put char. : "N" if this account has statistical type.
laData[15] = IIF(SUBSTR(laData[3],1,1) = 'Y' , 'N' , 'Y')

*** Add record in the chart of account file to save this account code. ***
APPEND BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

*** Add the balances record for this account code in the balances file. ***
*** Recrod for each period from the previous, current and next years.   ***
SELECT GLACBALS
FOR lnCount = 1 TO ALEN(laYrPrBal,1)
  INSERT INTO &gcDataDir.GLACBALS ;
         (cAcctCode,cFisfYear,cFspPrdid);
         VALUES (laData[1],laYrPrBal[lnCount,1],laYrPrBal[lnCount,2])
  =gfAdd_Info()
ENDFOR

*** Message to say that an account is added. ***
lcTAccCode = lcTAccCode+" "+ALLTRIM(laData[1])
WAIT lcTAccCode WINDOW TIMEOUT 2

*** Call the retained earnings for account major func. ***
=lfAddRetMj()

SELECT GLACCHAR

*!**************************************************************************
*!
*!      FUNCTION: lfAddRetMj
*!
*!**************************************************************************
* 
*** This is the retained earning for the current account ***
*** major in the current account.

FUNCTION lfAddRetMj

*** This function will be executed if three condition are true :_
***     1- The account type is one from these types :_
***               { C } Cost of sales.
***               { I } other Income.
***               { T } Taxes.
***               { E } Expenses.
***               { S } Sales.
***     2- The cost center field in the GLSETUP file is greater than 1
***     3- The Retained earning account major in the GLSETUP file is .T.

IF LEFT(lcTypCode,1) $ 'CITES' .AND. GLSETUP.NSETCOSTC > 1 .AND. !EMPTY(GLSETUP.CSETRETMJ)

  *** Scan in the account code strucure file to get ***
  *** the account segments no. and position. 
*E300692,1 Change file name SYCACCOD to ACCOD          
  *SELECT SYCACCOD
  SELECT ACCOD
*E300692,1 end  
  
  *E300789,6 ACCOD now does have company Id in its index [Begin
  *E300789,6 SET ORDER TO TAG COMPID
  *E300789,6
  GO TOP
*E300692,1 Change file name SYCACCOD to ACCOD            
    *IF SEEK(gcAct_comp,"SYCACCOD")
      *lnAryLen = SYCACCOD.NACSNOSEG

  *IF SEEK(gcAct_comp,"ACCOD")
  IF !EOF()
  *E300789,6 ACCOD now does have company Id in its index [End
    lnAryLen = ACCOD.NACSNOSEG
*E300692,1 end   
    
    DECLARE laSegment[lnAryLen,2]

    lnCont    = 1
    lnStrtPos = 1
    
    SKIP
  
    *E300789,6 ACCOD now does have company Id in its index [begin
    *SCAN WHILE cComp_Id = gcAct_Comp
    SCAN REST
    *E300789,6 ACCOD now does have company Id in its index [end
      laSegment [lnCont,1] = lnStrtPos
    *E300692,1 Change file name SYCACCOD to ACCOD              
      *laSegment [lnCont,2] = SYCACCOD.NACSSIZE
      *lnStrtPos            = lnStrtPos + SYCACCOD.NACSSIZE + 1
      laSegment [lnCont,2] = ACCOD.NACSSIZE
      lnStrtPos            = lnStrtPos + ACCOD.NACSSIZE + 1
    *E300692,1 end  
      
      lnCont               = lnCont + 1 
    ENDSCAN
  ENDIF

  *** Create the account code to be added in the chart of accounts file. ***
  lcTmpAcCod   = lcAcsMask

  lcTmpAcCod   = stuff(lcTmpAcCod,laSegment[1,1],laSegment[1,2],;
                 ALLTRIM(GLSETUP.CSETRETMJ))

  FOR lnCont = 2 TO GLSETUP.NSETCOSTC
    lcTmpAcCod   = stuff(lcTmpAcCod,laSegment[lnCont,1],laSegment[lnCont,2],;
                   SUBSTR(laData[1],laSegment[lnCont,1],laSegment[lnCont,2]))

  ENDFOR

  *** Put the rest of the account code as zeros. ***
  IF GLSETUP.NSETCOSTC < ALEN(laSegment,1)
    FOR lnCount  =  GLSETUP.NSETCOSTC+1 TO ALEN(laSegment,1)
      lcTmpAcCod = stuff(lcTmpAcCod,laSegment[lnCount,1],laSegment[lnCount,2],REPLICATE('0',laSegment[lnCount,2]))
    ENDFOR
  ENDIF

  *** If the created account is not exist in the chart of account file. ***
  IF !SEEK(lcTmpAcCod,"GLACCHAR")

    IF gfModalGen("QRM02060B00006","DIALOG",SUBSTR(laData[1],laSegment[2,1],laSegment[2,2])) = 1

      *** Get the first segment data from the segment  file as ***
      *** default for the new account code record that will be ***
      *** added in the chart of accounts file.
      SELECT GLSEGVAL
      IF SEEK("1"+ALLTRIM(GLSETUP.CSETRETMJ))
        SCATTER MEMVAR
        
        *** Add the new retained earnings for account major ***
        *** in the chart of account.
        SELECT GLACCHAR
        APPEND BLANK
        REPLACE cAcctcode WITH lcTmpAcCod
        GATHER MEMVAR
        =gfAdd_Info()
      
        *** Add the balances record for this account code in the ***
        *** balances file,  record for  each period according to ***
        *** the {previous - current - next} years.
        SELECT GLACBALS
        FOR lnCount = 1 TO ALEN(laYrPrBal,1)
          INSERT INTO &gcDataDir.GLACBALS ;
                 (cAcctCode,cFisfYear,cFspPrdid);
                 VALUES (lcTmpAcCod,laYrPrBal[lnCount,1],laYrPrBal[lnCount,2])
          =gfAdd_Info()
        ENDFOR
      ENDIF
    ENDIF    
  ENDIF
ENDIF
