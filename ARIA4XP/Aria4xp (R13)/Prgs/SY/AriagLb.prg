*:************************************************************************
*: Modifications :
 
* B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin
* B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008]
* E303111,1 HIA 04/22/2012 Add Next month day related field, to trems code [T20120301.0038]
* B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[T20120301.0038]
* B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041]
* B610014,1 HIA 07/18/2012 Error Popsup when open Aria Application [T20120711.0030]
* E303256,1 TMI 09/25/2012  add the GETRPFIELD,lfGetOrder function that is used in the GL reports [T20120917.0025]
* B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002]
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008]
* E303294,1 HIA 11/11/2012 Add new support login[T20120726.0060]
* B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012]
* B610203,1 HIA 01/17/2013 Aria4xp - IC - Style- Cut & Sold - OTS does not allow filters to be saved [T20130103.0004]
* N000682,1 SAB 04/03/2013 Change Get User Pervilege to use A5 Security [Start]
* B610314,1 HIA 04/21/2013 T20130218.0012 - PO - automatic issue on PO costs sheet not issuing all components
* B610447,1 TMI 07/24/2013 [Start] fix problems in maintain dictionary screen
* B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice
* B610451,1 HIA 07/28/13 T20130723.0006 - Error in editing company information
* E303425,1 TMI 10/30/2013 add a new parameter to the gfSysLock screen to enable it to be called from screens [ T20130716.0014 ]
* B610604,1 MMT 11/27/2013 Aria4XP Login screen takes too long to appear[T20130726.0001]
* B610656,1 TMI 01/14/2014 Modify function GFSTYPRICE to add new parameter called 'lReadOnly', 
* B610656,1                if this parameter is true, don't call the form called STYPRICE.scx and set the return price to 0 [T20140102.0574 ]
* B610732,1 TMI 05/25/2014 GMA  Fix a performance issue accessing a5 privelages             [T20140527.0005 - GMA Security issue]
* B610809,1 MMT 08/18/2014 Cutting ticket cost sheet reads trim cost incorrectly from style cost sheet[T20140804.0032]
* B611194,1 MMT 10/09/2016 Error while previewing AR Aging report[T20160822.0163]
* B611219,1 MMT 10/24/2016 Email Validation function considers .info email is invalid[T20161019.0039]
* E303736,1 MMT 12/29/2016 Aria5 changes - Facelift [Aria5 ERP - Main System - Iteration 2016-12] 
* E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005]
* E303855,1 MMT 07/31/2017 Applying Aria5 new Framework - Decorator[T20170706.0015][Start]
* E303920,1 MMT 01/30/2018 Call Aria5 Messaging function[T20180124.0005]
* B611566,1 SAH 04/30/2018 Modify function GFGETOBJ to add lines of code  to check if the type of '_SCREEN.ACTIVEFORM.PARENT' is Object it is not Null [T20180424.0013]
* B611588,1 Es 05/27/2018 Modify function GFCODDES to add some lines[T20180405.0008]
* B611601,1 MMT 06/12/2018 Error while adding new colors to style in Style screen [T20180611.0017]
* E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL
* E304037,1 MMT 08/15/2018 Create new function to read the Identifier code structure [P20171120.0011]
* B611686,1 SAH 10/28/2018 modify function gfSheetItem as the Cutting cost sheet calculated the Qty incorrectly When item is repeated more than once in Style cost sheet [T20181010.0018]
* B611748,1 Es 3/20/2019 The Sales Order screen does not show the customer billing address if customer.ccont_cod2 is empty in customer table. [T20190319.0006]
* E611881,1 Es 08/18/2019 When aria4XP screens is opened in Aria5, messages are displayed in the old format [T20180724.0004]
* B611925,1 ES 10/14/2019 Random issue in Manual packing list screen that Packing list is saved without carton detail [T20190610.0006]
* E611820,1 MMT 11/13/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
* B612010,1 MMT 02/03/2020 Add new Global function to check if same windows user is running aria4xp or not[T20180807.0005]
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004]
* B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003]
* E612596,1 MMT 07/26/2022 Add memo field to the profiles list and profiles screens [T20220407.0001]
* B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002]
*:************************************************************************
*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ARIA.H

*!*************************************************************
*! Name      : gfSubStr
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : To extract element from string or to convert string to array
*!*************************************************************
*! Calls     :
*!      Called by: ARIA3.PRG
*!      Called by: GFSETUP()                (function  in ARIA3.PRG)
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!      Called by: GFMODALGEN()             (function  in ARIA3.PRG)
*!      Called by: GFSEEKREC()              (function  in ARIA3.PRG)
*!      Called by: GFDBFFIELD()             (function  in ARIA3.PRG)
*!      Called by: GFFLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFRLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFWAIT()                 (function  in ARIA3.PRG)
*!      Called by: GFGETVLD()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : String to be used
*!                      poiter to array or element position
*!                      sparators used in the string
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
* This function will return eather a string part # OR an array of all
* the string parts according to the type of the second parameter. The
* firest parameter will be the string or string variable. If the
* second parameter have a numeric type, the function will return the
* but if it is an array the function will return the array with each
*  element having a part from the string.
*
*:->
FUNCTION GFSUBSTR
PARAMETERS LCSTRING,LNARYORPOS,LCSEPTA

LCSUBSTR  =' '
LNARYDIM  = 1
LNARYROWS = 1
LNARYCOLS = 1
LCSEPTA   = IIF(TYPE('lcSepta')='C',LCSEPTA,',')

IF LEN(ALLTRIM(LCSEPTA))>1
  LCCOLSEP  = SUBSTR(LCSEPTA,2,1)
  LCSEPTA   = LEFT(LCSEPTA,1)
  LNARYDIM  = IIF(OCCURS(LCSEPTA,LCSTRING)>0,;
    OCCURS(LCSEPTA,LCSTRING)+;
    IIF(RIGHT(LCSTRING,1)<>LCSEPTA,1,0),;
    LNARYDIM)
  LNARYCOLS = IIF(OCCURS(LCCOLSEP,LCSTRING)>0,;
    OCCURS(LCCOLSEP,LCSTRING)+;
    IIF(RIGHT(LCSTRING,1)<>LCCOLSEP,1,0),;
    LNARYDIM)
  LNARYROWS = (LNARYDIM+(LNARYCOLS-1)) / LNARYCOLS
  LNARYDIM  = LNARYDIM +(LNARYCOLS-1)
  LCSTRING  = STRTRAN(LCSTRING,LCCOLSEP,LCSEPTA)
ELSE
  LNARYDIM = IIF(OCCURS(LCSEPTA,LCSTRING)>0,;
    OCCURS(LCSEPTA,LCSTRING)+;
    IIF(RIGHT(LCSTRING,1)<>LCSEPTA,1,0),;
    LNARYDIM)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
CASE TYPE ('lnAryOrPos')='U'
  LNARYORPOS = 1

  *** If array strich it to hold all string parts
CASE TYPE ('lnAryOrPos') $ 'C,L'
  IF LNARYCOLS > 1
    DIMENSION LNARYORPOS[lnAryRows,lnAryCols]
  ELSE
    IF ALEN(LNARYORPOS,2) > 0
      DIMENSION LNARYORPOS[lnAryDim,ALEN(lnAryOrPos,2)]
    ELSE
      DIMENSION LNARYORPOS[lnAryDim]
    ENDIF

  ENDIF
  LNARYORPOS  = ' '

ENDCASE

FOR LNARELEM  = 1 TO LNARYDIM
  IF TYPE ('lnAryOrPos')='N'
    LNARELEM = LNARYORPOS
  ENDIF

  DO CASE
    *** In case of firest string part
  CASE LNARELEM = 1
    LCSUBSTR = SUBSTR(LCSTRING,1,;
      IIF(LCSEPTA $ LCSTRING,AT(LCSEPTA,LCSTRING)-1,LEN(LCSTRING)))

    *** In case of last string part
  CASE LNARELEM = LNARYDIM
    LCSUBSTR = SUBSTR(LCSTRING,AT(LCSEPTA,LCSTRING,LNARELEM-1)+1)
    LCSUBSTR = IIF(RIGHT(LCSUBSTR,1)=LCSEPTA,;
      SUBSTR(LCSUBSTR,1,LEN(LCSUBSTR)-1),LCSUBSTR)
    *** In case of any string part from the meddel
  CASE LNARELEM > 1
    LCSUBSTR = SUBSTR(LCSTRING,AT(LCSEPTA,LCSTRING,LNARELEM-1)+1,;
      AT(LCSEPTA,LCSTRING,LNARELEM)-;
      AT(LCSEPTA,LCSTRING,LNARELEM-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN LCSUBSTR
  ENDIF

  IF LNARYCOLS > 1
    LNARYORPOS[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = LCSUBSTR
  ELSE
    LNARYORPOS[lnArElem] = LCSUBSTR
  ENDIF
ENDFOR




*!*************************************************************
*! Name      : gfGetTime
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : get the current time with am,pm format
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : current time
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFGETTIME

LCCURRHOUR = IIF(VAL(SUBSTR(TIME(),1,2))=12 OR VAL(SUBSTR(TIME(),1,2))=0,;
  '12',ALLTRIM(STR(VAL(SUBSTR(TIME(),1,2))%12)))

LCCURRTIME = IIF(VAL(LCCURRHOUR)<10,'0','')+LCCURRHOUR+;
  SUBSTR(TIME(),3)+IIF(VAL(SUBSTR(TIME(),1,2))>=12,' pm',' am')

RETURN (LCCURRTIME)


*!*************************************************************
*! Name      : gfMenuBar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Run a Menu option
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : lcPop_Name,lnBar_Pos
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFMENUBAR
PARAMETERS LCPOP_NAME,LNBAR_POS
=OARIAAPPLICATION.MENUBAR(LCPOP_NAME,LNBAR_POS)

*!*************************************************************
*! Name      : gfChngComp
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Change the active company
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : lcComp_ID Company ID to be active
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFCHNGCOMP
PARAMETERS LCCOMP_ID
=OARIAAPPLICATION.CHANGECOMPANY(LCCOMP_ID)


*!*************************************************************
*! Name      : gfDoHelp
*! Developer : Hesham El-Sheltawi
*! Date      : 10/22/96
*! Purpose   : Activate the help screen for aria systems
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFDOHELP
HELP



*:************************************************************************
*: Modifications :
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! E037885,4 MAH 02/16/2005 Remove the host form.
*! E037885,4 MAH 02/17/2005 Remove the host form.
*! B039071,1 MAH 02/24/2005 Close Group Problem.
*:************************************************************************

FUNCTION GPEXIT

*! B039071,1 MAH 02/24/2005 [BEGIN]
IF TYPE('oAriaApplication.oMainForm') = 'O' .AND. !ISNULL(OARIAAPPLICATION.OMAINFORM)
  OARIAAPPLICATION.OMAINFORM.QUERYUNLOAD()
  *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
  RETURN
  *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
ENDIF

*! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
*RETURN
*! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}

*! B039071,1 MAH 02/24/2005 [END]

*! E037885,4 MAH 02/17/2005 Maximize the main screen  [BEGIN]
*-- _Screen.Visible = .T.
_SCREEN.TOP         = 100
_SCREEN.WINDOWSTATE = 2
* MAH
*_SCREEN.Visible     = .T.
* MAH
*! E037885,4 MAH 02/17/2005 [END]

*! E037885,4 MAH 02/16/2005 No need [BEGIN]
*-- *! E037885,2 MAH 12/03/2004 [BEGIN]
*-- IF TYPE('oAriaApplication.oMainForm') = 'O' .AND. !ISNULL(oAriaApplication.oMainForm)
*--   oAriaApplication.oMainForm.Visible = .F.
*-- ENDIF
*-- *! E037885,2 MAH 12/03/2004 [END]
*! E037885,4 MAH 02/16/2005 [END]

CLEAR WINDOWS
LOCAL LNFORMS,LNCOUNT
LNFORMS = 0
IF _SCREEN.FORMCOUNT > 0
  FOR LNCOUNT = 1 TO _SCREEN.FORMCOUNT
    IF _SCREEN.FORMS(LNCOUNT).VISIBLE AND _SCREEN.FORMS(LNCOUNT).BASECLASS<>'Toolbar'
      LNFORMS = 1
      EXIT
    ENDIF
  ENDFOR
ENDIF
IF LNFORMS < 1
  *! E037885,2 MAH 12/03/2004 [BEGIN]
  ON SHUTDOWN
  *! E037885,2 MAH 12/03/2004 [END]

  *! E037885,4 MAH 02/16/2005 [BEGIN]
  *-- oAriaApplication.Sysexit()
  IF TYPE('oAriaApplication') = 'O' .AND. !ISNULL(OARIAAPPLICATION)
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
    TRY
      *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
      OARIAAPPLICATION.SYSEXIT()
      *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{Start}
    CATCH
    ENDTRY
    *! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{End}
  ENDIF
  *! E037885,4 MAH 02/16/2005 [END]
ENDIF

*!*************************************************************
FUNCTION GFUSERLIST
PARAMETERS LLGETCOUNT
PRIVATE ALL LIKE  L*
*SET DATASESSION TO 1
LCOLDREP = SET('REPROCESS')
LNDATASESSION = SET('datas')
SET DATASESSION TO 1
SET REPROCESS TO 1
* B610604,1 MMT 11/27/2013 Aria4XP Login screen takes too long to appear[T20130726.0001][Start]
LOCAL lnOldInterval
lnOldInterval = SYS(3051) 
= SYS(3051,100) 
* B610604,1 MMT 11/27/2013 Aria4XP Login screen takes too long to appear[T20130726.0001][End]
*MEDIA MMT 05-19-2011[Start]
LOCAL LCSQLDICPATH
LCSQLDICPATH = OARIAAPPLICATION.CARIA4SYSPATH
*MEDIA MMT 05-19-2011[End]
LLGETCOUNT = IIF(TYPE('llGetCount')="U",.F.,LLGETCOUNT)

DECLARE LAUSERLIST[1]
LLUSERUSED = USED('SYUUSER')
LCCURRFILE = ALIAS()
LAUSERLIST = " "
LNUSRREC   = 0
IF !USED("SYUSTATC")
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *USE (oAriaApplication.SysPath+"SYUSTATC") IN 0 SHARED
  *MEDIA MMT 05-19-2011[Start]
  *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  *IF oAriaApplication.multiinst
  *! E302857,1 HES 02/10/2011 Avoid 'X:\' Fixed Path [BEGIN]
  *!*	    lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
  * lcSQLDICPATH = oAriaApplication.cAria4SysPath
  *! E302857,1 HES 02/10/2011 Avoid 'X:\' Fixed Path [END  ]
  *ENDIF
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
  *MEDIA MMT 05-19-2011[ENd]

  USE (LCSQLDICPATH +"SYUSTATC") IN 0 SHARED
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
ENDIF
SELECT SYUSTATC

*B128052,1 AMH Set order to cuser_id to be sure the order tag is used [Start]
SET ORDER TO CUSER_ID
*B128052,1 AMH [End]

*-- Hesham (Start)
*!*	IF !llUserUsed
*!*	  USE (oAriaApplication.SysPath+'SYUUSER') IN 0 ORDER TAG cuser_id
*!*	ENDIF
*!*	IF USED("SYUUSER")
*!*	  lnUsrRec = RECNO("SYUUSER")
*!*	ENDIF

*IF lnRemResult>=1

*-- Hesham (End)
*MAN Speed Optimization, use the curr user as a variable instead of obj. ref.
*!*	SELECT IIF(SYUSTATC.CUSER_ID = oAriaApplication.User_ID AND;
*!*	           SYUSTATC.cstation = oAriaApplication.Station,;
*!*	           "» ","  ")+;
*!*	           PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
*!*	     FROM (oAriaApplication.SysPath+"SYUSTATC");
*!*	     WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
*!*	            'INI'+'OLDVARS' ;
*!*	       .AND.;
*!*	             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
*!*	       INTO ARRAY  laUserList
LCCURUSER= OARIAAPPLICATION.USER_ID
LCCURSTAT= OARIAAPPLICATION.STATION
IF !LLGETCOUNT
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syuuser",'',"syuuser","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",1)

  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *SELECT IIF(SYUSTATC.CUSER_ID = lcCurUser AND;
  SYUSTATC.cstation = lcCurStat,;
  "» ","  ")+;
  PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
  FROM (oAriaApplication.SysPath+"SYUSTATC");
  WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
  'INI'+'OLDVARS' ;
  .AND.;
  gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
  INTO ARRAY  laUserList
  *MEDIA MMT 05-19-2011[Start]
  *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'


  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  *IF oAriaApplication.multiinst
  *lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
  *ENDIF
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
  *MEDIA MMT 05-19-2011[End]

  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [Begin]
  *!*	  Select Iif(SYUSTATC.cUser_ID = lcCurUser And;
  *!*	    SYUSTATC.cstation = lcCurStat,;
  *!*	    "» ","  ")+;
  *!*	    PADR(LFGETUSRNM(SYUSTATC.cUser_ID),35) ;
  *!*	    FROM (lcSQLDICPATH+"SYUSTATC");
  *!*	    WHERE COBJ_TYP+Alltrim(COBJ_NAME)+SYUSTATC.cUser_ID+cstation=;
  *!*	    'INI'+'OLDVARS' ;
  *!*	    .And.;
  *!*	    gfCheckUser(SYUSTATC.cUser_ID,cstation) ;
  *!*	    INTO Array  laUserList

  SELECT IIF(SYUSTATC.CUSER_ID = LCCURUSER AND;
    SYUSTATC.CSTATION = LCCURSTAT,;
    "» ","  ")+;
    PADR(LFGETUSRNM(SYUSTATC.CUSER_ID),35) ;
    FROM (LCSQLDICPATH+"SYUSTATC");
    WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
    'INI'+'OLDVARS' ;
    .AND.;
    GFCHECKUSER(SYUSTATC.CUSER_ID,CSTATION)  AND UPPER(ALLTRIM(SYUSTATC.CADD_VER))= "A40" ;
    INTO ARRAY  LAUSERLIST

  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [End]

  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
ELSE
  IF !USED("SYUSTATC")

    *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
    *USE (oAriaApplication.SysPath+"SYUSTATC") IN 0 SHARED
    *MEDIA MMT 05-19-2011[Start]
    *lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'

    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
    *IF oAriaApplication.multiinst
    *lcSQLDICPATH = 'X:\Aria4xp\SQLDictionary\'
    *ENDIF
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
    *MEDIA MMT 05-19-2011[End]

    USE (LCSQLDICPATH+"SYUSTATC") IN 0 SHARED
    *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]

  ENDIF
  LNUSERCOUNT = 0
  SELECT SYUSTATC

  *! E303294,1 HIA 11/11/2012 Add new support login[T20120726.0060][Start]
  LNSUPPORTUSERCOUNT = 0

  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [Begin]
  *Count For COBJ_TYP+Alltrim(COBJ_NAME)+SYUSTATC.cUser_ID+cstation=;
  *	'INI'+'OLDVARS' And gfCheckUser(SYUSTATC.cUser_ID,cstation) And cUser_ID == Padr("SUPPORT",10,Space(1)) To lnSupportUserCount

  COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
    'INI'+'OLDVARS' AND GFCHECKUSER(SYUSTATC.CUSER_ID,CSTATION) AND CUSER_ID == PADR("SUPPORT",10,SPACE(1)) ;
    AND UPPER(ALLTRIM(SYUSTATC.CADD_VER))= "A40" TO LNSUPPORTUSERCOUNT
  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [End]

  NSUPPORTUSERCOUNT = LNSUPPORTUSERCOUNT
  LOCATE
  *! E303294,1 HIA 11/11/2012 Add new support login[T20120726.0060][End]



  *B128052,1 AMH Use gfCheckUser function to privent lock records with out unlock them [Start]
  *COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
  'INI'+'OLDVARS' AND !RLOCK() TO lnUserCount
  *UNLOCK

  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
  *COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
  'INI'+'OLDVARS' AND !gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) TO lnUserCount

  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [Begin]

  *Count For COBJ_TYP+Alltrim(COBJ_NAME)+SYUSTATC.cUser_ID+cstation=;
  *	'INI'+'OLDVARS' And gfCheckUser(SYUSTATC.cUser_ID,cstation) To lnUserCount

  COUNT FOR COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
    'INI'+'OLDVARS' AND GFCHECKUSER(SYUSTATC.CUSER_ID,CSTATION) ;
    AND UPPER(ALLTRIM(SYUSTATC.CADD_VER))= "A40" TO LNUSERCOUNT

  *B610450,1 HIA 07/28/13 T20130711.0032 - The system shows that some users are logged in twice [End]

  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]
  *B128052,1 AMH [End]

  *! E303294,1 HIA 11/11/2012 Add new support login[T20120726.0060][Start]
  IF LNSUPPORTUSERCOUNT > 0
    LNUSERCOUNT = LNUSERCOUNT -1
  ENDIF
  *! E303294,1 HIA 11/11/2012 Add new support login[T20120726.0060][End]

ENDIF

*! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[Start]
*IF oAriaApplication.UserStaticRecord < RECCOUNT('syuStatc')
IF OARIAAPPLICATION.USERSTATICRECORD <= RECCOUNT('syuStatc')
  *! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users[End]

  GO OARIAAPPLICATION.USERSTATICRECORD  IN SYUSTATC
  SET REPROCESS TO 1
  =RLOCK('syuStatc')
ENDIF
SET REPROCESS TO LCOLDREP
IF !LLGETCOUNT
  OARIAAPPLICATION.DOFORMRETVAL(OARIAAPPLICATION.SCREENHOME+"sy\syusrlst")
ENDIF


IF !EMPTY(LCCURRFILE)
  SELECT (LCCURRFILE)
ENDIF

*-- Hesham (Start)
*!*	IF lnUsrRec > 0 .AND. USED("SYUUSER")
*!*	  IF lnUsrRec <= RECCOUNT("SYUUSER")
*!*	    GO lnUsrRec IN SYUUSER
*!*	  ENDIF
*!*	ENDIF
*-- Hesham (End)
IF !LLGETCOUNT AND !LLUSERUSED
  USE IN SYUUSER
ENDIF
* B610604,1 MMT 11/27/2013 Aria4XP Login screen takes too long to appear[T20130726.0001][Start]
= SYS(3051,VAL(lnOldInterval)) 
* B610604,1 MMT 11/27/2013 Aria4XP Login screen takes too long to appear[T20130726.0001][End]
SET DATASESSION TO LNDATASESSION
IF LLGETCOUNT
  *RETURN _TALLY
  RETURN LNUSERCOUNT
ENDIF



*!*************************************************************
FUNCTION GFCHECKUSER
PARAMETERS LCUSERID, LCSTATION
LCSTATION=IIF(TYPE('lcStation')<>"C","",LCSTATION)
LLRETFLAG = .T.
IF SEEK('INI'+'OLDVARS'+LCUSERID+LCSTATION,'SYUSTATC')
  IF RLOCK('SYUSTATC')
    UNLOCK IN SYUSTATC
    LLRETFLAG = .F.
  ENDIF
ELSE
  LLRETFLAG = .F.
ENDIF
RETURN LLRETFLAG .OR. (LCUSERID+LCSTATION = LCCURUSER+LCCURSTAT)



*!*************************************************************
FUNCTION LFGETUSRNM
PARAMETER LCUSER_ID
SELECT SYUUSER
*-- Hesham (Start)
*IF SEEK(lcUser_ID)
LOCATE FOR CUSER_ID = LCUSER_ID
IF FOUND()
  *-- Hesham (End)
  RETURN CUSR_NAME
ELSE
  RETURN LCUSER_ID
ENDIF


*!*************************************************************
FUNCTION GPRELOGIN
IF _SCREEN.FORMCOUNT <= 1
  OARIAAPPLICATION.LOGIN()
  OARIAAPPLICATION.LOGUSER(.T.)
  OARIAAPPLICATION.SETMENU(OARIAAPPLICATION.ACTIVEMODULEID,IIF(OARIAAPPLICATION.ACTIVEMODULEID='SY','S','A'))
ELSE
  =MESSAGEBOX("You have to close all programs before login in with new use id",16)
ENDIF


*!*************************************************************
FUNCTION GFPRINTSET
=SYS(1037)

*!*************************************************************
FUNCTION GFTRACEKEY
LPARAMETERS LCFILENAME,LCKEYEXPR,LCEVENTOCCR,LCUPDTDIR,LCUPDTMODL
*--Not Needed
RETURN

*!*************************************************************
*! Name      : gfBrowse
*! Developer : Hesham El-Sheltawi
*! Date      : 11/17/96
*! Purpose   : Browse a File and return .t. if the user select record
*!*************************************************************
*! Parameters: tcBrowseFields   && variable Hold the browse fields to
*!                              && be displayed with the headers if needed
*!             tcBrowseTitle    && browse title
*!             tcAlias          && alias to be browsed if not the default alias
*!             tcKey            && key to be filter in the browse
*!             tcFor            && FOR condition or FOR condition REST
*!             tcOptions        && Options for the shortcut to be displayed
*!*************************************************************
*! Called by :
*!*************************************************************
*! Returns            : .t. if selected .f. if not
*!*************************************************************
*! Example   : llBrowseSelected=gfBrowse()
*!*************************************************************
*: Modifications :
*! E038142,2 MAH 09/15/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL
*:************************************************************************

FUNCTION GFBROWSE
*! E038142,2 MAH 09/15/2004 Add additonal parameters [BEGIN]
*-- lParameters tcBrowseFields,tcBrowseTitle,tcAlias,tcKey,tcFor,tcOptions,tlSelect,;
*--             toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--             llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- lPARAMETERS tcBrowseFields, tcBrowseTitle, tcAlias, tcKey, tcFor, tcOptions, tlSelect, ;
*--             toSelectObj , tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--             llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--             lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey

*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][Begin]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL
*!*	LPARAMETERS tcBrowseFields, tcBrowseTitle, tcAlias, tcKey, tcFor, tcOptions, tlSelect, ;
*!*	            toSelectObj , tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*!*	            llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*!*	            lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey, ;
*!*	            lcPreferenceKey, lcPreferenceSubKey

LPARAMETERS TCBROWSEFIELDS, TCBROWSETITLE, TCALIAS, TCKEY, TCFOR, TCOPTIONS, TLSELECT, ;
  TOSELECTOBJ , TCSELECTMETHOD, TCUSERSHORTCUT, TCTEMPFILE, TCSELFIELD, ;
  LLHALFHEIGHT, LCFIELDSNAM, LCARRNAME, LLCHCKKEY, LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCCURRENTRECORDKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY,LCREALTBNAME
*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][End]

*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/15/2004 [END]

LOCAL LLRETURNVALUE,LCALIAS
LCALIAS = SELECT()
IF !EMPTY(TCALIAS)
  SELECT (TCALIAS)
ENDIF
IF EMPTY(TCBROWSEFIELDS)
  TCBROWSEFIELDS=GFDATABASEPROP('Get',ALIAS(),'Table','BrowseFields')
ENDIF
PRIVATE OBROWSE
OBROWSE = .NULL.

*! E038142,2 MAH 09/15/2004 Add additonal parameters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
*--            toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--            llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree;
*--     TO llReturnValue
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*--         tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*--         toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--         llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--         lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey ;
*--     TO llReturnValue

*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][Begin]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL
*!*	DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*!*	        tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*!*	        toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*!*	        llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*!*	        lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcCurrentRecordKey, ;
*!*	        lcPreferenceKey, lcPreferenceSubKey ;
*!*	    TO llReturnValue

DO FORM (OARIAAPPLICATION.SCREENHOME+"sy\BROWSE") WITH ;
  TCBROWSEFIELDS, TCBROWSETITLE, TCKEY, TCFOR, TCOPTIONS, TLSELECT, ;
  TOSELECTOBJ, TCSELECTMETHOD, TCUSERSHORTCUT, TCTEMPFILE, TCSELFIELD, ;
  LLHALFHEIGHT, LCFIELDSNAM, LCARRNAME, LLCHCKKEY, LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCCURRENTRECORDKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY, LCREALTBNAME ;
  TO LLRETURNVALUE

*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][End]
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/15/2004 [END]

SELECT (LCALIAS)
RETURN LLRETURNVALUE


*!*************************************************************
*: Modifications :
*! E038142,2 MAH 09/16/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL
*:************************************************************************
FUNCTION ARIABROW

*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- PARAMETER tcKey,tcBrowseTitle,lnY1,lnX1,lnY2,lnX2,lcOnSelect,lcBrPushB,lcFieldsNam,;
*--           lcArrName,llChckKey,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
*--           lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--           lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
*PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
lcArrName, llChckKey, lcAliasName, llGetTree, ;
lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
lcPreferenceKey, lcPreferenceSubKey
*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][Begin]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL
*!*	PARAMETER tcKey, tcBrowseTitle, lnY1, lnX1, lnY2, lnX2, lcOnSelect, lcBrPushB, lcFieldsNam, ;
*!*	          lcArrName, llChckKey, lcAliasName, llGetTree, ;
*!*	          lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
*!*	          lcPreferenceKey, lcPreferenceSubKey,lcTmpFile,lcFldtoRet

PARAMETER TCKEY, TCBROWSETITLE, LNY1, LNX1, LNY2, LNX2, LCONSELECT, LCBRPUSHB, LCFIELDSNAM, ;
  LCARRNAME, LLCHCKKEY, LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCSEEKTOKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY,LCTMPFILE,LCFLDTORET, LCREALTBNAME

*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][End]

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 [END]

LOCAL LLRETURNVALUE,LCALIAS
LCALIAS = SELECT()
TCBROWSEFIELDS = IIF(TYPE('lcBrFields')='C',LCBRFIELDS,'')
LNY1 = IIF(TYPE('lnY1')='N',LNY1,0)
LLHALFHEIGHT = LNY1=GNBRHSROW1
TCFOR = .F.
*B608546,1 TMI [START] assign the value of the optin lcBrPushB to tcOptions and then use the last in the browse.scx screen to not trim the sent key
*                      in case of fox tables as this causes problems when keying in the browse screen is the keyed field in the second key in the
*                      current order and the first key has spece in its right
*tcOptions=''
TCOPTIONS = LCBRPUSHB
*B608546,1 tmi [end  ]

TLSELECT=.T.
TCSELECTMETHOD = LCONSELECT
IF !EMPTY(LCONSELECT) AND LEFT(LCONSELECT,1) <> "=" AND TYPE("_SCREEN.ActiveForm") = "O" AND  TYPE("_SCREEN.ActiveForm.PARENT") = "O"
  TOSELECTOBJ=_SCREEN.ACTIVEFORM.PARENT
ELSE
  TOSELECTOBJ=.F.

  *N037635,1 WSH 07/04/2005 Don't remove the passed OnSelect function if it is not a methiod in a FormSet Object. [Start]
  *tcSelectMethod=.F.

  *B128950,1 AMH Fix bug of variable not found when browse [Start]
  *IF !(LEFT(tcSelectMethod,1)="=")
  IF (TYPE('tcSelectMethod')#"C") OR !(LEFT(TCSELECTMETHOD,1)="=")
    *B128950,1 AMH [End]

    TCSELECTMETHOD=.F.
  ENDIF
  *N037635,1 WSH 07/04/2005 [End]

ENDIF
TCUSERSHORTCUT=.F.
TCTEMPFILE=.F.
TCSELFIELD=.F.

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
TCTEMPFILE= IIF(TYPE('lcTmpFile')='C' AND !EMPTY(LCTMPFILE),LCTMPFILE, .F.)
TCSELFIELD =IIF(TYPE('lcFldtoRet')='C' AND !EMPTY(LCFLDTORET),LCFLDTORET,.F.)
IF (TYPE('lcFldtoRet')='C' AND !EMPTY(LCFLDTORET)) AND (TYPE('lcTmpFile')='C' AND !EMPTY(LCTMPFILE))
  TOSELECTOBJ=.F.
ENDIF
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]


PRIVATE OBROWSE
OBROWSE = .NULL.
IF TYPE('tcKey')='C'
  TCFOR=IIF(ATC('FOR ',TCKEY)>0,SUBSTR(TCKEY,ATC('FOR ',TCKEY)),'')
ENDIF
TCKEY=IIF(!EMPTY(TCFOR),SUBSTR(TCKEY,1,ATC(TCFOR,TCKEY)-1),TCKEY)
TCKEY = IIF(TYPE('tcKey')='C' AND !EMPTY(TCKEY),TCKEY,'')


*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH tcBrowseFields,tcBrowseTitle,tcKey,tcFor,tcOptions,tlSelect,;
*--            toSelectObj   ,tcSelectMethod,tcUserShortCut,tcTempFile,tcSelField,;
*--            llHalfHeight,lcFieldsNam,lcArrName,llChckKey,lcAliasName,llGetTree;
*--     TO llReturnValue
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*--         tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*--         toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*--         llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*--         lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey ;
*--         TO llReturnValue
*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][Begin]
*! Add new parameter contains the name of the master table, in case we browse from temp table for SQL

*!*	DO FORM (oAriaApplication.ScreenHome+"sy\BROWSE") WITH ;
*!*	        tcBrowseFields, tcBrowseTitle, tcKey, tcFor, tcOptions, tlSelect, ;
*!*	        toSelectObj, tcSelectMethod, tcUserShortCut, tcTempFile, tcSelField, ;
*!*	        llHalfHeight, lcFieldsNam, lcArrName, llChckKey, lcAliasName, llGetTree, ;
*!*	        lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey, ;
*!*	        lcPreferenceKey, lcPreferenceSubKey ;
*!*	        TO llReturnValue
DO FORM (OARIAAPPLICATION.SCREENHOME+"sy\BROWSE") WITH ;
  TCBROWSEFIELDS, TCBROWSETITLE, TCKEY, TCFOR, TCOPTIONS, TLSELECT, ;
  TOSELECTOBJ, TCSELECTMETHOD, TCUSERSHORTCUT, TCTEMPFILE, TCSELFIELD, ;
  LLHALFHEIGHT, LCFIELDSNAM, LCARRNAME, LLCHCKKEY, LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCSEEKTOKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY, LCREALTBNAME ;
  TO LLRETURNVALUE

*! B610127,1 HIA 10/18/2012 Cannot add fields to total inventory browse [T20120927.0002][End]
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 Add addtional parameters [END]

SELECT (LCALIAS)
RETURN LLRETURNVALUE

*!*************************************************************
*! Name      : gfDataBaseProp
*! Developer : Hesham El-Sheltawi
*! Date      : 11/20/96
*! Purpose   : Return Or Set or Remove User defined property value
*!             from database
*!*************************************************************
*! Parameters: tcName          && object name in database
*!             tcType          && type of object in database
*!                             && Table,Field,Index,Relation
*!             tcProperty      && User Defined Property name
*!             tcPropertyValue && value of property to be saved
*!*************************************************************
*! Call      :
*!*************************************************************
*! Returns            : VALUE OF Property value "Different types"
*!*************************************************************
*! Example   : lcVarName=gfDataBaseProp('Get',"syccomp.ccomp_id",'Field','BrowseField')
*!             WILL return from the database the value of the property
*!             called browsefield for the field ccomp_id in the table
*!             syccomp
*!*************************************************************
FUNCTION GFDATABASEPROP
LPARAMETERS TCDATABASEFUNCTION,TCNAME,TCTYPE,TCPROPERTY,TCPROPERTYVALUE

LOCAL LNCOUNT,LCALIAS,LCFIELDNAME,LCTABLENAME,LCDATABASE,LCPATH,LCRETRUNVALUE
TCNAME = UPPER(TCNAME)
TCTYPE = UPPER(TCTYPE)
TCPROPERTY = UPPER(TCPROPERTY)
TCDATABASEFUNCTION = PROP(ALLTRIM(TCDATABASEFUNCTION))

IF TCTYPE='FIELD'
  IF ATC('.',TCNAME)>0
    LCALIAS = SUBSTR(TCNAME,1,ATC('.',TCNAME)-1)
    LCFIELDNAME = SUBSTR(TCNAME,ATC('.',TCNAME)+1)

    *-- MAB 12/08/2002 Detect database errors.
    LOCAL LCOLDFERR, LLCURRERR
    LCOLDFERR = ON("ERROR")
    ON ERROR LLCURRERR = .T.
    LCTABLENAME = CURSORGETPROP('Sourcename',LCALIAS)
    ON ERROR &LCOLDFERR.
    IF LLCURRERR
      RETURN .F.
    ENDIF

    LCTABLENAME = STRTRAN(LCTABLENAME,'.DBF','')
    TCNAME = LCTABLENAME+'.'+LCFIELDNAME
  ENDIF
ENDIF

DO CASE
CASE TCTYPE = 'DATABASE'
  IF DBUSED(TCNAME)
    SET DATABASE TO (TCNAME)
  ELSE
    RETURN TCNAME
  ENDIF
CASE TCTYPE $ "FIELD,TABLE,VIEW"
  IF TCTYPE = 'FIELD'
    LCDATABASE = CURSORGETPROP("DATABASE",LCALIAS)
  ELSE
    LCDATABASE = CURSORGETPROP("DATABASE",TCNAME)
  ENDIF
  IF !EMPTY(LCDATABASE)
    LCPATH =''
    IF ATC('\',LCDATABASE)>0
      LCPATH = SUBSTR(LCDATABASE,1,RAT('\',LCDATABASE))
      LCDATABASE = STRTRAN(LCDATABASE,LCPATH,'')
      LCDATABASE = STRTRAN(LCDATABASE,'.DBC','')
    ENDIF
    IF !DBUSED(LCDATABASE)
      OPEN DATABASE (LCPATH+LCDATABASE)
    ENDIF
    SET DATABASE TO (LCDATABASE)
  ELSE
    RETURN .F.
  ENDIF
ENDCASE
DO CASE
CASE TCDATABASEFUNCTION == 'Dbgetprop'
  LCRETRUNVALUE = DBGETPROP(TCNAME,TCTYPE,TCPROPERTY)
CASE TCDATABASEFUNCTION == 'Dbsetprop'
  LCRETRUNVALUE = DBSETPROP(TCNAME,TCTYPE,TCPROPERTY,TCPROPERTYVALUE)
CASE TCDATABASEFUNCTION == 'Get'
  *    lcRetrunValue = sfsGETProp(tcName,tcType,tcProperty)
CASE TCDATABASEFUNCTION == 'Set'
  *    lcRetrunValue = sfsSetProp(tcName,tcType,tcProperty,tcPropertyValue)
CASE TCDATABASEFUNCTION == 'Remove'
  *    lcRetrunValue = sfsRemoveProp(tcName,tcType,tcProperty)
ENDCASE
RETURN LCRETRUNVALUE


*:************************************************************************
*: Program file  : GFGENFLT.PRG
*: Program desc. :
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system
*:      Developer:
*:************************************************************************
*: Calls :
*:         Procedures :
*:         Functions  :
*:************************************************************************
*: Passed Parameters  :
*:************************************************************************
FUNCTION OLDGFGENFLT
PARAMETERS LCARRAY,LLFILTER
LCQUERY=''
LCELMSEP=','


LCLINEFEED=' ' &&+CHR(10)+CHR(13)
LNFLTSTART=1
DO WHILE (&LCARRAY[lnFltStart,1]='.OR.' OR EMPTY(&LCARRAY[lnFltStart,1]));
    AND !LNFLTSTART=ALEN(&LCARRAY,1)
  LNFLTSTART=LNFLTSTART+1
ENDDO
LNFLTEND=ALEN(&LCARRAY,1)
DO WHILE (&LCARRAY[lnFltEnd,1]='.OR.' OR EMPTY(&LCARRAY[lnFltEnd,1]));
    AND LNFLTEND>1
  LNFLTEND=LNFLTEND-1
ENDDO

LNOR=0
IF LNFLTSTART>ALEN(&LCARRAY,1)
  RETURN ''
ENDIF
IF LNFLTEND=ALEN(&LCARRAY,1)
  LCWHICHELM=LCARRAY+'['+ALLTRIM(STR(ALEN(&LCARRAY,1)))+',1]'
  IF TYPE(LCWHICHELM)<>'C'
    RETURN ''
  ENDIF
  IF &LCARRAY[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
LNCOUNT=LNFLTSTART

DO WHILE  LNCOUNT<=LNFLTEND
  IF  &LCARRAY[lnCount,3]='N' AND EMPTY(VAL(&LCARRAY[lnCount,6])) ;
      AND &LCARRAY[lnCount,7]='V'
    LNCOUNT=LNCOUNT+1
    LOOP
  ENDIF
  IF !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount,6],LCELMSEP,'');
      ,IIF(&LCARRAY[lnCount,3]='D','/',''),''))) OR &LCARRAY[lnCount,1]='.OR.'
    IF !EMPTY(&LCARRAY[lnCount,1])
      IF &LCARRAY[lnCount,1]<>'.OR.'
        LCQUERY=LCQUERY+LFGETQCOND(LNCOUNT,LCARRAY,LLFILTER)
      ELSE
        LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
        IF LNOR>0
          LCQUERY=LCQUERY+' ) '
          LNOR=0
        ENDIF
        ** THIS CONDITION IS ADDED BY HESHAM 3 AUG.
        DO WHILE LNCOUNT<LNFLTEND-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,'');
            ,IIF(&LCARRAY[lnCount+1,3]='D','/  /',''),''))) OR &LCARRAY[lnCount+1,1]='.OR.')
          LNCOUNT=LNCOUNT+1
        ENDDO
        IF !EMPTY(ALLTRIM(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,''))) AND !EMPTY(ALLTRIM(LCQUERY))
          LCQUERY=ALLTRIM(LCQUERY)+' '+LCELMSEP+' OR '+LCELMSEP+'( '
          LNOR=1
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  LNCOUNT=LNCOUNT+1
ENDDO
LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
IF LNOR>0
  LCQUERY=LCQUERY+' ) '
ENDIF
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' .AND. '+LCELMSEP,LCLINEFEED+' AND '+LCLINEFEED)
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' OR '+LCELMSEP,LCLINEFEED+' OR '+LCLINEFEED)
RETURN LCQUERY


FUNCTION OLDLFGETQCOND
PARAMETERS LNCOUNT,LCARRAY,LLFILTER
LCFILTEXP=''
DO CASE
CASE &LCARRAY[lnCount,5] = 'Contains'

  LCFILTEXP=IIF(!&LCARRAY[lnCount,4],'','!(')+;
    LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
    &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
    ' $ '+ALLTRIM(&LCARRAY[lnCount,1])+' '+;
    IIF(!&LCARRAY[lnCount,4],'',' ) ')+LCELMSEP+' .AND. '+LCELMSEP

CASE &LCARRAY[lnCount,5] = 'Like' OR &LCARRAY[lnCount,5] = 'Exactly Like'

  LCFILTEXP=IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
    +IIF(&LCARRAY[lnCount,3]='D',")",'')+' '+IIF(!&LCARRAY[lnCount,4],;
    IIF(&LCARRAY[lnCount,5] = 'Like','=','=='),'<>')+' '+;
    LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
    &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+LCELMSEP+' .AND. '+LCELMSEP

CASE INLIST(&LCARRAY[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
    'Less Or Equal')
  LCOPERATOR=LFGETOPER(ALLTRIM(&LCARRAY[lnCount,5]),!&LCARRAY[lnCount,4])
  LCFILTEXP=IIF(&LCARRAY[lnCount,4],'','!(')+;
    IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
    +IIF(&LCARRAY[lnCount,3]='D',")",'')+' '+LCOPERATOR+' '+;
    LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
    &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+IIF(!&LCARRAY[lnCount,4],'',' ) ')+;
    LCELMSEP+' .AND. '+LCELMSEP
CASE &LCARRAY[lnCount,5] = 'Between'
  IF LLFILTER
    LCFILTEXP=IIF(!&LCARRAY[lnCount,4],'BETWEEN(','!BETWEEN(')+;
      IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
      +IIF(&LCARRAY[lnCount,3]='D',")",'')+','+;
      LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
      &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
      ')'+LCELMSEP+' .AND. '+LCELMSEP
  ELSE
    LCFILTEXP= IIF(!&LCARRAY[lnCount,4],'','!(')+;
      IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
      +IIF(&LCARRAY[lnCount,3]='D',")",'')+' BETWEEN '+;
      LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
      &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
      IIF(!&LCARRAY[lnCount,4],'',')')+LCELMSEP+' .AND. '+LCELMSEP
  ENDIF
CASE &LCARRAY[lnCount,5] = 'In List'
  IF LLFILTER
    LCFILTEXP=IIF(!&LCARRAY[lnCount,4],'INLIST(','!INLIST(')+;
      IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
      +IIF(&LCARRAY[lnCount,3]='D',")",'')+','+;
      LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
      &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
      ')'+LCELMSEP+' .AND. '+LCELMSEP
  ELSE
    LCFILTEXP= IIF(!&LCARRAY[lnCount,4],'','!(')+;
      IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
      +IIF(&LCARRAY[lnCount,3]='D',")",'')+' IN('+;
      LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
      &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+')'+;
      IIF(!&LCARRAY[lnCount,4],'',')')+LCELMSEP+' .AND. '+LCELMSEP
  ENDIF
ENDCASE

RETURN LCFILTEXP


FUNCTION OLDLFGETOPER
PARAMETER LCOPERATOR,LLISNOT
DO CASE
CASE LCOPERATOR = 'Greater Than'
  RETURN '>'
CASE LCOPERATOR = 'Less Than'
  RETURN '<'
CASE LCOPERATOR = 'Greater Or Equal'
  RETURN '>='
CASE LCOPERATOR = 'Less Or Equal'
  RETURN '<='
ENDCASE


FUNCTION OLDLFRIGHTGET
PARAMETERS MRIGHTHEAD,CLEFTTYPE,COPERATOR,LCELMSEP,CRIGHTTYPE
LCRETVAL=MRIGHTHEAD
DO CASE
CASE CRIGHTTYPE='V'
  DO CASE
  CASE CLEFTTYPE $ 'CM'
    IF INLIST(COPERATOR,'Between','In List')
      LCSEPER=IIF(!LLFILTER AND COPERATOR='Between',' AND ',',')
      LCRETVAL='"'+STRTRAN(ALLTRIM(MRIGHTHEAD),LCELMSEP,'"'+LCSEPER+'"')+'"'
    ELSE
      RETURN '"'+MRIGHTHEAD+'"'   &&'"'+ALLTRIM(mrightHead)+'"'
    ENDIF
  CASE CLEFTTYPE = 'N'
    LCSEPER=IIF(COPERATOR='Between' AND !LLFILTER,' AND ',',')
    LCRETVAL=STRTRAN(MRIGHTHEAD,LCELMSEP,LCSEPER)
    IF EMPTY(LCRETVAL)
      LCRETVAL='0'
    ENDIF
  CASE CLEFTTYPE = 'D'
    IF INLIST(COPERATOR,'Between','In List')
      LCSEPER=IIF(!LLFILTER AND COPERATOR='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')
      LCRETVAL='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(MRIGHTHEAD),LCELMSEP,'  }))'+LCSEPER+'{  ')+'  }))'
    ELSE
      LCRETVAL='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
    ENDIF
  CASE CLEFTTYPE = 'L'
    RETURN ' '+LCRETVAL+' '
  ENDCASE
CASE CRIGHTTYPE='F'
  LCRETVAL=STRTRAN(ALLTRIM(MRIGHTHEAD),LCELMSEP,',')
ENDCASE
IF INLIST(COPERATOR,'Between','In List') AND EMPTY(ALLTRIM(MRIGHTHEAD))
  LCSEPER=IIF(!LLFILTER AND COPERATOR='Between',' AND ',',')
  LCRETVAL=LCRETVAL+LCSEPER+LCRETVAL
ENDIF
RETURN LCRETVAL



*!*************************************************************
*! Name      : GFCPTOP
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR FIRST
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPTOP
OARIAAPPLICATION.OTOOLBAR.CMDTOP.CLICK

*!*************************************************************
*! Name      : GFCPBTTM
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Bottom
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPBTTM
OARIAAPPLICATION.OTOOLBAR.CMDEND.CLICK

*!*************************************************************
*! Name      : GFCPNEXT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR next
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPNEXT
OARIAAPPLICATION.OTOOLBAR.CMDNEXT.CLICK

*!*************************************************************
*! Name      : GFCPPRVIS
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR previous
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPPRVIS
OARIAAPPLICATION.OTOOLBAR.CMDPREV.CLICK

*!*************************************************************
*! Name      : GFVCPNEW
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR New
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFVCPNEW
OARIAAPPLICATION.OTOOLBAR.CMDADD.CLICK


*!*************************************************************
*! Name      : GFVCPPRINT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Print
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFVCPPRINT
*oAriaApplication.oToolBar.cmdprint.Click
OARIAAPPLICATION.OTOOLBAR.CMDPRINT.CLICK("M")    && MAB 05/01/2003
*--

*!*************************************************************
*! Name      : GFCPEDIT
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Edit
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPEDIT
OARIAAPPLICATION.OTOOLBAR.CMDEDIT.CLICK

*!*************************************************************
*! Name      : GFCPDELETE
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Delete
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPDELETE
OARIAAPPLICATION.OTOOLBAR.CMDDELETE.CLICK

*!*************************************************************
*! Name      : GFCPBROWS
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Browse
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPBROWS
OARIAAPPLICATION.OTOOLBAR.CMDFIND.CLICK

*!*************************************************************
*! Name      : GFCHNGORDR
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Order
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCHNGORDR

*!*************************************************************
*! Name      : GFSETFILTR
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Filter
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFSETFILTR


*!*************************************************************
*! Name      : GPRECHIST
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Record History
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*! B608156,1 SSH 07/29/2007 Allow the user to unlock record from history options
FUNCTION GPRECHIST
PRIVATE LNOLDALS,LNBASETABEL,LNOLDDATASESSION,LLRETURN

LLRETURN=.F.
LNOLDALS=SELECT(0)
LNBASETABEL = _SCREEN.ACTIVEFORM.PARENT.NWORKAREA
SET DATASESSION TO _SCREEN.ACTIVEFORM.PARENT.DATASESSIONID

SELECT(LNBASETABEL)
IF TYPE('Cedit_user') = 'U' .AND. TYPE('dedit_date') = 'U' .AND. TYPE('cedit_time') = 'U' AND TYPE('lLok_Stat') = 'U'
  =GFMODALGEN("INM00050B00000","DIALOG")
  LLRETURN = .T.
ELSE

  LCUSER = IIF(EMPTY(CEDIT_USER),CADD_USER,CEDIT_USER)
  LDDATE = IIF(EMPTY(DEDIT_DATE),DADD_DATE,DEDIT_DATE)
  LDTIME = IIF(EMPTY(CEDIT_TIME),CADD_TIME,CEDIT_TIME)
  *lcUser = IIF(EMPTY(cLok_User),lcUser,cLok_User)
  IF EMPTY(CADD_USER) .OR. EMPTY(DADD_DATE) .OR. EMPTY(CADD_TIME)
    =GFMODALGEN("INM00051B00000","DIALOG")
    LLRETURN = .T.
  ENDIF
ENDIF
IF !LLRETURN
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syuuser where cUser_ID='"+LCUSER+"'",'',"syuuser","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",_SCREEN.ACTIVEFORM.PARENT.DATASESSIONID)
  IF LNREMRESULT>=1
    LOCATE
  ENDIF
  SELECT(LNBASETABEL)
  DO FORM (OARIAAPPLICATION.SCREENHOME+"sm\smrechist.scx") WITH SYUUSER.CUSR_NAME,LDTIME,LDDATE,LLOK_STAT,LNBASETABEL
  IF USED('SYUUSER')
    USE IN SYUUSER
  ENDIF
ENDIF
SET DATASESSION TO
SELECT(LNOLDALS)


*!*************************************************************
*! Name      : gfCpSave
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Save
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPSAVE
OARIAAPPLICATION.OTOOLBAR.CMDADD.CLICK

*!*************************************************************
*! Name      : gfCpClose
*! Developer : Hesham El-Sheltawi
*! Date      : 07/08/98
*! Purpose   :
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Called by : MENU BAR Close  / Cancel
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFCPCLOSE
IF OARIAAPPLICATION.OTOOLBAR.EDITMODE
  OARIAAPPLICATION.OTOOLBAR.CMDEDIT.CLICK
ELSE
  OARIAAPPLICATION.OTOOLBAR.CMDEXIT.CLICK
ENDIF

*!*************************************************************

*!*****************************************************************************************
*! Modification:
*! E037885,4 MAH 02/17/2005 Remove the host form.
*!*****************************************************************************************
FUNCTION GFTBARONOF
LNBAR = BAR()
LCPOP = POPUP()
OARIAAPPLICATION.USERUSETOOLBAR = !OARIAAPPLICATION.USERUSETOOLBAR
OARIAAPPLICATION.OTOOLBAR.VISIBLE = OARIAAPPLICATION.USERUSETOOLBAR

*! E037885,4 MAH 02/17/2005 No need [BEGIN]
*-- oAriaApplication.oListView.ArrangeHeight()
*! E037885,4 MAH 02/17/2005 [END]

SET MARK OF BAR LNBAR OF (LCPOP) OARIAAPPLICATION.USERUSETOOLBAR
*!*************************************************************

FUNCTION GFABOTARIA
DO FORM SYABOUT
*oAriaApplication.RunProg("SYABOUT")


*!*************************************************************
FUNCTION GFGETNAME
LPARAMETERS LCTAGUSER
LOCAL LCNAMES,LATO,LAVAL,LNCOUNT
DIME LATO[1,1]
STORE '' TO LATO,LCNAMES
IF !EMPTY(LCTAGUSER)
  =GFSUBSTR(LCTAGUSER,@LATO,'|')
  FOR LNCOUNT = 1 TO ALEN(LATO,1)
    DIMEN LAVAL[1,1]
    IF !EMPTY(LATO[lnCount,1])
      =GFSUBSTR(LATO[lnCount,1],@LAVAL,'~')
      LCNAMES = LCNAMES + IIF(EMPTY(LCNAMES),'',';')+ALLT(LAVAL[1,1])+'['+ALLT(LAVAL[1,3])+'] '
    ENDIF
  ENDFOR
ENDIF
RETURN LCNAMES

*!*************************************************************
FUNCTION GFGETATTCH
LPARAMETERS LCMSGID
PRIVATE LNALIAS,LCATTACH
LNALIAS = SELECT()
SELECT MTMSATCH
LCATTACH = ""
IF SEEK(LCMSGID,'MTMSATCH')
  SCAN REST WHILE CMSGID = LCMSGID
    LCATTACH = LCATTACH + CATTCHFILE + CHR(13)+CHR(10)
  ENDSCAN
ENDIF
SELECT (LNALIAS)
RETURN LCATTACH

*!*************************************************************
FUNCTION UPDDATE
DO FORM SYUPDATE

*!*************************************************************
*! Name      : gfAdd_Info
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : To add audit information to any file
*!*************************************************************
*! Calls     :
*!      Called by: ARIA3.PRG
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!      Called by: GFSTATIC()               (function  in ARIA3.PRG)
*!      Called by: GFSEQUENCE()             (function  in ARIA3.PRG)
*!      Called by: GFCPSAVE()               (function  in ARIA3.PRG)
*!      Called by: GFSUSRPRG()              (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : File name to add to
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
* Update add user,date and time
*:->
FUNCTION GFADD_INFO
LPARAMETERS LCFILENAME, OFORM

*! B608644,1 MMT 08/05/2008 Fix bug of error in gfAddUserInfo due data session change[Start]
*RETURN oAriaApplication.AddUserInformation(lcFileName, oForm)
LNCURRDATA  = SET("Datasession")
RETURN OARIAAPPLICATION.ADDUSERINFORMATION(LCFILENAME, OFORM,LNCURRDATA)
*! B608644,1 MMT 08/05/2008 Fix bug of error in gfAddUserInfo due data session change[End]


FUNCTION GFERRORTRAP
LPARAMETERS NERROR, CMETHOD, NLINE

LCERROR   = IIF(TYPE("nError")  = "N", ALLTRIM(STR(NERROR)), ALLTRIM(STR(ERROR())))
LCMETHOD  = IIF(TYPE("cMethod") = "C", CMETHOD, "")
LCLINE    = IIF(TYPE("nLine")   = "N", ALLTRIM(STR(NLINE)), "")

LCNEWLINE = CHR(13)+CHR(10)
MESSAGEBOX("An error has occuerd..."         + LCNEWLINE + LCNEWLINE +;
  "Error Number : " + LCERROR    + LCNEWLINE +;
  "Error Message: " + MESSAGE()  + LCNEWLINE +;
  "Method 		: " + LCMETHOD   + LCNEWLINE +;
  "Line Number	: " + LCLINE     + LCNEWLINE +;
  "Line Code	: " + MESSAGE(1) + LCNEWLINE )


CLEAR ALL
CLOSE ALL
QUIT

*!************************************************************
*! Name      : gfDoTriger
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/22/2002
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301903,1 WAB
*!*************************************************************
*
FUNCTION GFDOTRIGER
PARAMETERS LCPROGNAME , LCEVENT

PRIVATE LNOLDALIAS , LCPROGTODO , LAPARAMEXP , LAPARAM , LCPARMSTR ,;
  LNCOUNT    , LLRETURN , LLISOPEN
LLRETURN = .T.
*-- If any of the parameters is not passed or passed incorrectly
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(LCPROGNAME) .OR.;
    TYPE('lcEvent') <> 'C' .OR. EMPTY(LCEVENT)
  RETURN
ENDIF

*-- Save the old alias
LNOLDALIAS = SELECT(0)

*-- Open the Trigger file if it was not opened
*-- Hesham (Start)
*-- Use trigger file remotely in temprory alias
*!*	llIsOpen = .F.
*!*	IF !USED('SYCTRIGG')
*!*	    SELECT 0
*!*	    *** Open the
*!*	    USE (oARiaApplication.SysPath+"SYCTRIGG")
*!*	    SET ORDER TO 1
*!*	ENDIF

*!*	SELECT SYCTRIGG
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCTRIGG where cAPObjNam='"+PADR(LCPROGNAME , 10)+"' AND cEvent_ID ='"+PADR(LCEVENT , 10)+"'",'',"SYCTRIGGTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,'',SET("DATASESSION"))
IF LNREMRESULT>=1
  LOCATE
  *-- Hesham (End)

  *-- If there is triggers for this Object/Event
  *IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  IF FOUND()
    *-- Scan loop to scan the Object/Event triggers
    SCAN REST;
        WHILE CAPOBJNAM + CEVENT_ID = PADR(LCPROGNAME , 10) +;
        PADR(LCEVENT , 10)

      *-- Get the name of the program that should be executed
      LCPROGTODO = CTRIG_ID
      *-- Initialize the parameter string variable
      LCPARMSTR  = ''

      *-- Restore the old alias to be able to evaluate the parameter
      *-- expressions properly
      SELECT (LNOLDALIAS)

      *-- If there is one or more parameters that should be passed to the
      *-- program
      IF !EMPTY(SYCTRIGGTMP.MPARMEXPR)

        *-- Get the parameter expressions
        DIMENSION LAPARAMEXP[OCCURS('~' , SYCTRIGGTMP.mParmExpr) + 1]
        =GFSUBSTR(SYCTRIGGTMP.MPARMEXPR , @LAPARAMEXP , '~')

        *-- Initialize the parameters array
        DIMENSION LAPARAM[ALEN(laParamExp , 1)]
        LAPARAM = ""

        *-- Get the parameters values that will be passed to the program
        FOR LNCOUNT = 1 TO ALEN(LAPARAMEXP , 1)
          LAPARAM[lnCount] = EVALUATE(LAPARAMEXP[lnCount])
          LCPARMSTR = LCPARMSTR + IIF(LNCOUNT = 1 , '' , ' , ') +;
            'laParam[' + ALLTRIM(STR(LNCOUNT)) + ']'

        ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
      ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)
      LCPARMSTR = "''"+IIF(!EMPTY(LCPARMSTR),",","")+LCPARMSTR && Wael
      *-- If custom process
      *Hassan [Begin]
      LCOLDPATH = FULLPATH('')
      LCNEWPATH = SUBSTR(OARIAAPPLICATION.APPLICATIONHOME,1,RAT("\",OARIAAPPLICATION.APPLICATIONHOME,2))
      CD (LCNEWPATH)
      *Hassan [End]
      IF SYCTRIGGTMP.CACTVTYP = 'C'
        *-- Call the program and get the returned value
        LLRETURN = &LCPROGTODO(&LCPARMSTR)
      ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
      *Hassan [Begin]
      CD (LCOLDPATH)
      *Hassan [End]
      SELECT SYCTRIGGTMP
    ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  ENDIF

ELSE    &&  *In case the process doesn't exist.[START]
  LLRETURN = .F.

ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
IF USED("SYCTRIGGTMP")
  USE IN SYCTRIGGTMP
ENDIF
*-- Restore the old alias
SELECT (LNOLDALIAS)

RETURN (LLRETURN)


*****************************************************************************
* PROC: GLDIST.PRG
* DESC: A GLOBAL PROCEDURE  THAT WILL BE  CALLED FROM  SEVERAL PLACES WITH
*       DIFFERENT PARAMETERS, DEPENDING ON THE  PARAMETERS THE PROGRAM WILL
*       UPDATE THE G/L DISTRIBUTION FILE WITH THE CORRESPONDING G/L ACCOUNT
*       AND AMOUNT.
* DATE: 12/24/93
* AUTH: WAEL ALY MOHAMED
* NOTE: PARAMETERS USED:-
*       - _GLLINK  : GL LINK CODE -> THE LINK CODE USED FOR POSTING.
*       - _CATGKEY : CATEGORY KEY
*       - _AMOUNT  : AMOUNT
*                    DEBITS ARE POSITIVE, CREDITS ARE NEGATIVE.
*       - _TRANTYP : TRANSACTION TYPE :-
*                    'IN' -> INVOICE
*                    'VI' -> VOID INVOICE
*                    'CR' -> CASH RECEIPT
*                    'CA' -> CREDIT ADJUSTMENT
*                    'DA' -> DEBIT ADJUSTMENT
*                    'RM' -> RETURN MERCHANDISE
*                    'VR' -> VOID RETURN
*                    'IP' -> INVENTORY PHYSICAL
*                    'MA' -> MATERIAL INVENTORY ADJUSTMENT
*                    'MP' -> MATERIAL INVENTORY PHYSICAL
*                    'IA' -> INVENTORY ADJUSTMENT
*                    'PO' -> P/O RECEIVING
*                    'MO' -> MATERIAL P/O RECEIVING
*                    'CT' -> C/T RECEIVING
*                    'ZE' -> ZERO OUT STOCK
*                    'NL' -> NON MAT. LIABILITY
*                    'JC' -> JOB COST CLOSING ADJ
*                    'RO' -> MATERIAL P/O RECEIVING
*                    'RS' -> C/T RECEIVING
*                    'MM' -> ZERO OUT STOCK
*                    'EX' -> EXCHANGE RATE DIFFERENCE
*                    'KO' -> KEY OFF
*      - _TRANNO   : TRANSACTION NUMBER.
*      - _TRANDAT  : TRANSACTION DATE.
*      - _NFILE    : NAME OF FILE WILL BE USED.
*      - _FYEAR    : TRANSACTION FISCAL YEAR.
*      - _PRDID    : PERIOD ID.
*****************************************************************************
PROCEDURE GLDIST
PARAMETERS _GLLINK,_CATGKEY,_AMOUNT,_TRANTYP,_TRANNO,_TRANDAT,_FYEAR,_PRDID,;
  _NFILE,LCGLACT, LCCURRCODE, LNCURRUNIT, LNEXRATE

PRIVATE XGLACNT, XTRANDESC, LCACNTTYPE, LCEXRSIN, LCUNTSIN, LNEQVAMNT

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

*B803032,1 BWA 14/02/2000 [START]
* Fix the bug of Period & year fields in GL distribution file are empty,So there is no way
* to release those entries unless we are replace in those fields manually.
IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
  =CHECKPRD(_TRANDAT,'_FYEAR','_PRDID','_TRANTYP' , .T.)
  IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
    LCYEARPERIOD = "YYYY,PP"
    *--This Form return valid year and period.
    DO FORM (OARIAAPPLICATION.SCREENHOME + 'GL\GLDDATE') TO LCYEARPERIOD
    _FYEAR = ALLTRIM(SUBSTR(LCYEARPERIOD,1,ATC(",",LCYEARPERIOD)-1))
    _PRDID = ALLTRIM(SUBSTR(LCYEARPERIOD,ATC(",",LCYEARPERIOD)+1))
  ENDIF
ENDIF
*B803032,1 BWA 14/02/2000 [END]

*E301210,1 ASH 04/27/99 (Begin) Don't create GL entries for some transactions
*E301210,1                      due to the new parameter M_GL_COST.
LLGLCOST = ALLTRIM(GFGETMEMVAR('M_GL_COST',OARIAAPPLICATION.ACTIVECOMPANYID))='Y'

IF !LLGLCOST AND _CATGKEY $ '006,007,008,011,012,013,015,016,017,018,019,021,022,023,024,025,026,027'
  RETURN
ENDIF
*E301210,1 ASH 04/27/99 (End)
*** OPEN GL_LINK FILE TO GET G/L ACCOUNT FOR THIS CATEGORY/GL LINK CODE

IF _AMOUNT = 0
  RETURN
ENDIF

*E300325,1 If the currency code is not passed, or
*E300325,1 If the currency code is the base currency, or
*E300325,1 If the category key of the transaction is either,
*E300325,1   '006' : 'FINISHED GOODS INVENTORY'
*E300325,1    or
*E300325,1   '008' : 'COST OF GOODS'
*E300325,1 Default the currency fields to the base currency values.
IF EMPTY(LCCURRCODE) .OR. INLIST(_CATGKEY, '006', '008');
    .OR. LCCURRCODE = OARIAAPPLICATION.BASECURRENCY
  LCCURRCODE = OARIAAPPLICATION.BASECURRENCY
  LNCURRUNIT = 1
  LNEXRATE   = 1
  LNEQVAMNT  = _AMOUNT
ELSE
  *E300325,1 If either of the exchange rate or the currency unit
  *E300325,1 is not greater than 0, return .F., otherwise, calculate
  *E300325,1 as follows.
  IF LNEXRATE > 0 .AND. LNCURRUNIT > 0
    *E300325,1 Get the exchange rate sign for the curreny code
    LCUNTSIN = ''
    LCEXRSIN = GFGETEXSIN(@LCUNTSIN, LCCURRCODE)
    *E300325,1 Get the currency unit sign for the curreny code
    LNEQVAMNT  = ROUND(_AMOUNT &LCEXRSIN LNEXRATE &LCUNTSIN LNCURRUNIT, 2)
  ELSE
    RETURN .F.
  ENDIF
ENDIF
*E300325,1 end.

=GFOPENFILE(OARIAAPPLICATION.DATADIR+'GL_LINK','GL_LINK','SH')

SELE GL_LINK
*-- 03/29/94 WAM
*-- If the link code not found (Zap the file for example), default to 'DEF'

*E300592,1 Increase the link_Code field to be 6 characters
*IF !SEEK(_GLLINK+_CATGKEY)
*  SEEK('DEF'+_CATGKEY)
*ENDIF
IF !SEEK(PADR(_GLLINK,6)+_CATGKEY)
  SEEK('DEFDEF'+_CATGKEY)
ENDIF
*E300592,1 (End)

XGLACNT = IIF(EMPTY(LCGLACT),GLACNT,LCGLACT)

*-- WAM 03/29/94
*-- Get Account Type

DO CASE
CASE _CATGKEY = '001'   && Account Receivable
  *-- Control Account
  LCACNTTYPE = 'C'
CASE _CATGKEY = '002'   && Cash Receipts
  *-- Distribition Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '003'   && Sales Revenue
  *-- Control Account
  LCACNTTYPE = 'C'
CASE _CATGKEY = '004'   && Freight
  *-- Control Account
  LCACNTTYPE = 'C'
CASE _CATGKEY = '005'   && Discount
  *-- Distribition Account
  LCACNTTYPE = 'D'
  *-- WAM 04/20/94
  *-- Add category key for material inventory control
CASE _CATGKEY = '006' .OR. _CATGKEY = '015'  && Inventory Control
  *-- It is a Control account if the inventory decrease,
  *-- and a distribution account if the inventory increase.
  LCACNTTYPE = IIF(_AMOUNT < 0 , 'C', 'D')
  *-- WAM 04/20/94
  *-- Add category key for material inventory adjustment
CASE _CATGKEY = '007' .OR. _CATGKEY = '016'  && Inventory Adjustments
  *-- It is a distribution account if the inventory decrease,
  *-- and a Control account if the inventory increase.
  LCACNTTYPE = IIF(_AMOUNT < 0 , 'D', 'C')
CASE _CATGKEY = '008'   && Cost of Goods
  *-- It is a distribution account if the inventory decrease,
  *-- and a Control account if the inventory increase.
  LCACNTTYPE = IIF(_AMOUNT < 0 , 'D', 'C')
CASE _CATGKEY = '009'   && Credit Adjustments
  *-- Distribition Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '010'   && Debit Adjustments
  *-- Distribition Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '011'   && Return Merchandise
  *-- Distribition Account
  LCACNTTYPE = 'D'
  *-- WAM 04/20/94
  *-- Add category key for material P/O clearing
CASE _CATGKEY = '012' .OR. _CATGKEY = '017'  && P/O Clearing
  *-- It is a distribution account if the inventory decrease,
  *-- and a Control account if the inventory increase.
  LCACNTTYPE = IIF(_AMOUNT < 0 , 'D', 'C')
CASE _CATGKEY = '013'   && C/T Clearing
  *-- It is a distribution account if the inventory decrease,
  *-- and a Control account if the inventory increase.
  LCACNTTYPE = 'C'
CASE _CATGKEY = '014'   && Sales Tax Liability
  *-- Control Account
  LCACNTTYPE = 'C'
CASE _CATGKEY = '018'   && Non material cost liability
  *-- Control Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '019'   && Cost of goods variance
  *-- Distribution Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '020'   && Return Merchandise
  *-- Distribution Account
  LCACNTTYPE = 'C'
  *E100219,9 WAM 07/04/95 Add new category key '021' in the GL_CATG for
  *E100219,9              Cost of Material Variance.
CASE _CATGKEY = '021'   && Cost of material variance
  *-- Distribution Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '022'   && Cost of goods variance 1
  *-- Distribution Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '023'   && Cost of goods variance 2
  *-- Distribution Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '024'   && Cost of goods variance 3
  *-- Distribution Account
  LCACNTTYPE = 'D'
CASE _CATGKEY = '025'   && Cost of goods variance 4
  *-- Distribution Account
  LCACNTTYPE = 'D'
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [Begin]
CASE _CATGKEY = '026'   && Cost of goods variance 5
  LCACNTTYPE = 'D'
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]

  *B608402,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [Begin]
CASE _CATGKEY = '027'   && Cost of goods variance 6
  LCACNTTYPE = 'D'
CASE _CATGKEY = '028'   && Cost of goods variance 7
  LCACNTTYPE = 'D'
  *B608402,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]

  *: E302618,1 MMT 06/17/2009 Add New GL Categories 029,030 For HST,PST Taxes[Start]
CASE _CATGKEY = '029'   && PST TAX LIABILITIY
  LCACNTTYPE = 'C'
CASE _CATGKEY = '030'   && HST TAX LIABILITIY
  LCACNTTYPE = 'C'
  *: E302618,1 MMT 06/17/2009 Add New GL Categories 029,030 For HST,PST Taxes[End]


ENDCASE

*** GET DESCRIBTION OF THIS TRANSACTION TYPE

*-- WAM 04/20/94
*-- Add three transaction types
*-- 'MP', 'MA' and 'MO' for material inventory physical, inventory adjustment
*-- and P/O receiving.
*N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
*!*	DO CASE
*!*	CASE _TRANTYP = 'IN'
*!*	  XTRANDESC = 'INVOICE             '
*!*	CASE _TRANTYP = 'VI'
*!*	  XTRANDESC = 'VOID INVOICE        '
*!*	CASE _TRANTYP = 'CR'
*!*	  XTRANDESC = 'CASH RECEIPT        '
*!*	CASE _TRANTYP = 'CA'
*!*	  XTRANDESC = 'CREDIT ADJUSTMENT   '
*!*	CASE _TRANTYP = 'DA'
*!*	  XTRANDESC = 'DEBIT ADJUSTMENT    '
*!*	CASE _TRANTYP = 'RM'
*!*	  XTRANDESC = 'RETURN MERCHANDISE  '
*!*	CASE _TRANTYP = 'VR'
*!*	  XTRANDESC = 'VOID RETURN         '
*!*	CASE _TRANTYP = 'IP'
*!*	  XTRANDESC = 'INVENTORY PHYSICAL  '
*!*	CASE _TRANTYP = 'IA'
*!*	  XTRANDESC = 'INVENTORY ADJUSTMENT'
*!*	CASE _TRANTYP = 'MP'
*!*	  XTRANDESC = 'MATERIAL INV. PHYSI.'
*!*	CASE _TRANTYP = 'MA'
*!*	  XTRANDESC = 'MATERIAL INV. ADJUS.'
*!*	CASE _TRANTYP = 'PO'
*!*	  XTRANDESC = 'P/O RECEIVING       '
*!*	CASE _TRANTYP = 'MO'
*!*	  XTRANDESC = 'MATERIAL P/O RECEIV.'
*!*	CASE _TRANTYP = 'CT'
*!*	  XTRANDESC = 'C/T RECEIVING       '
*!*	CASE _TRANTYP = 'ZE'
*!*	  XTRANDESC = 'ZERO OUT STOCK      '
*!*	  *-- ARH 11/10/94
*!*	CASE _TRANTYP = 'NL'
*!*	  XTRANDESC = 'NON MAT. LIABILITY  '
*!*	  *B603862,1 Modify 'JC' type description [Begin]
*!*	CASE _TRANTYP = 'JC'
*!*	  *XTRANDESC = 'JOB COST CLOSING ADJ'
*!*	  XTRANDESC = 'P/O JOB COST CLOSING'
*!*	  *B603862,1 Modify 'JC' type description [End]
*!*	  *-- END  ARH  11/10/94
*!*	  *E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
*!*	  *N100219,4              from operation
*!*	  *N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
*!*	CASE _TRANTYP = 'RO'
*!*	  XTRANDESC = 'MATERIAL OP. RECEIVE'
*!*	CASE _TRANTYP = 'RS'
*!*	  XTRANDESC = 'STYLE OP. RECEIVE   '
*!*	CASE _TRANTYP = 'MM'
*!*	  XTRANDESC = 'RECEIVE M.F.G. ORDER'
*!*	  *E300344,2 Add new transaction type for Differences in Exchange rate
*!*	CASE _TRANTYP = 'EX'
*!*	  XTRANDESC = 'EX. RATE DIFFERENCES'
*!*	CASE _TRANTYP = 'KO'
*!*	  XTRANDESC = 'KEY OFF'
*!*	  *B603504,1 (Begin) Add a new transaction type for Closing material MFG and Closing Material PO.
*!*	CASE _TRANTYP = 'MC'
*!*	  XTRANDESC = 'MATERIAL JOB ClOSING'
*!*	  *B603504,1 (End)
*!*	  *B603862,1 Add new type for the Manufacturing closing cost sheet [Begin]
*!*	CASE _TRANTYP = 'JP'
*!*	  XTRANDESC = 'C/T JOB COST CLOSING'
*!*	  *B603862,1 Add new type for the Manufacturing closing cost sheet [End]
*!*	  *B603983,1 Add new type for Inventory Locking [Begin]
*!*	CASE _TRANTYP = 'LK'
*!*	  XTRANDESC = 'INVENTORY LOCKING'
*!*	  *B603983,1 Add new type for Inventory Locking [End]
*!*	ENDCASE
DO CASE
CASE _TRANTYP = 'IN'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVOICE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_INVOICE",LCARIAHFILE))
CASE _TRANTYP = 'VI'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_VOIDINVOICE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_VOIDINVOICE",LCARIAHFILE))
CASE _TRANTYP = 'CR'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CASHR,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_CASHR",LCARIAHFILE))
CASE _TRANTYP = 'CA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CREDITADJ,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_CREDITADJ",LCARIAHFILE))
CASE _TRANTYP = 'DA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_DEBITADJ,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_DEBITADJ",LCARIAHFILE))
CASE _TRANTYP = 'RM'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_RM,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_RM",LCARIAHFILE))
CASE _TRANTYP = 'VR'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_VOIDRM,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_VOIDRM",LCARIAHFILE))
CASE _TRANTYP = 'IP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVPHY,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_INVPHY",LCARIAHFILE))
CASE _TRANTYP = 'IA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVADJ,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_INVADJ",LCARIAHFILE))
CASE _TRANTYP = 'MP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATINVPHY,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_MATINVPHY",LCARIAHFILE))
CASE _TRANTYP = 'MA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATINVDJ,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_MATINVDJ",LCARIAHFILE))
CASE _TRANTYP = 'PO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_POREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_POREC",LCARIAHFILE))
CASE _TRANTYP = 'MO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MAPOREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_MAPOREC",LCARIAHFILE))
CASE _TRANTYP = 'CT'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CTREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_CTREC",LCARIAHFILE))
CASE _TRANTYP = 'ZE'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_ZEROOUT,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_ZEROOUT",LCARIAHFILE))
CASE _TRANTYP = 'NL'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_NONMATLIB,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_NONMATLIB",LCARIAHFILE))
CASE _TRANTYP = 'JC'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_POJOBCLOSE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_POJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'RO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATOPREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_MATOPREC",LCARIAHFILE))
CASE _TRANTYP = 'RS'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_STYOPREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_STYOPREC",LCARIAHFILE))
CASE _TRANTYP = 'MM'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_RECMFGORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_RECMFGORDER",LCARIAHFILE))
CASE _TRANTYP = 'EX'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_EXRATEDIFF,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_EXRATEDIFF",LCARIAHFILE))
CASE _TRANTYP = 'KO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_KEYOFF,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_KEYOFF",LCARIAHFILE))
CASE _TRANTYP = 'MC'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MAJOBCLOSE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_MAJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'JP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CTJOBCLOSE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_CTJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'LK'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVLOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLDIST_INVLOCK",LCARIAHFILE))
ENDCASE
*N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
***
SELECT &_NFILE
APPEND BLANK

REPLACE CATG_KEY   WITH _CATGKEY  ,;
  TRAN_TYPE  WITH _TRANTYP  ,;
  TRAN_NO    WITH _TRANNO   ,;
  NGLAMOUNT  WITH _AMOUNT   ,;
  TRAN_DATE  WITH _TRANDAT  ,;
  GLACCOUNT  WITH XGLACNT   ,;
  TRAN_DESC  WITH XTRANDESC ,;
  GLPERIOD   WITH _PRDID    ,;
  GLFYEAR    WITH _FYEAR    ,;
  GLACNTTYPE WITH LCACNTTYPE,;
  CCURRCODE  WITH LCCURRCODE,;
  NCURRUNIT  WITH LNCURRUNIT,;
  NEXRATE    WITH LNEXRATE  ,;
  NEQVAMNT   WITH LNEQVAMNT

=GFADD_INFO(_NFILE)
RETURN
********************
*** EOF of GLDIST
********************

*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/05/95
*! Purpose   : Return Variable(s) for company settings from SETUP
*!*************************************************************
*! Parameters: lcArray   && variable to restore
*!                       && OR one dimension array to restore the variable(s)
*!                       name with the same variable name
*!                       && OR two dimension array to restore the variable(2)
*!                       in column 1 into variable names in column 2
*!             lcCompID  &&company id to get its settings
*!*************************************************************
*! Called by :
*!*************************************************************
*! Returns            : VALUE OF VARIABLE OR no of variables restored
*!*************************************************************
*! Example   : lcVarName=gfGetMemVar('LLMULCURR ','01')
*!             WILL return from the sycsetup file the setting
*!             value for company 01 the variable called "LLMULCURR "
*!*************************************************************
*
FUNCTION GFGETMEMVAR
PARAMETERS LCARRAY,LCCOMPID
*WAIT '1 '+lcArray WIND
*WAIT '2 '+lcCompID WIND
PRIVATE LNALIASNO,LLCUSEDBY,LLARRAYORVAR,LLTWODIMEN,LCSETUPTAG,;
  LCCONFGTAG,LNRETCOUNT,LCONERR,LLERROR,LAVARARR,LLUSESYCC
*B601818,1  Get company path
PRIVATE LCCOMPDIR, LCSETPATH, LLREUSEDBY, LNCURTAG, LNCURREC, LLUSECOMP
*B601818,1  end

LNALIASNO=SELECT()
LLUSEDBY  = .F.
LLCUSEDBY = .F.
LLSUSEDBY = .F.
LLUSESYCC = .F.
LCCOMPID  = IIF(TYPE('lcCompId')<>'C',OARIAAPPLICATION.ACTIVECOMPANYID,LCCOMPID)
*B601818,1  Get company path
LCCOMPDIR  = OARIAAPPLICATION.DATADIR
IF OARIAAPPLICATION.ACTIVECOMPANYID <> LCCOMPID
  *-- Hesham (Start)
  *!*		llUseComp = gfOpenFile(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID')
  *!*		lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir)), oAriaApplication.DataDir)
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syccomp where cComp_ID='"+LCCOMPID+"'",'',"CompFile","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATASESSION"))
  IF LNREMRESULT>=1
    LOCATE
  ENDIF
  IF LNREMRESULT>=1 AND FOUND()
    *-- Hesham (End)
    *B131801,1 MMT 26/04/2006 fix bug of error in Auto Alloc if Co. Changed[Start]
    *lcCompDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
    LCCOMPDIR  = GFGETDATADIR(ALLTRIM(COMPFILE.CCOM_DDIR))
    *B131801,1 MMT 26/04/2006 fix bug of error in Auto Alloc if Co. Changed[End]
  ENDIF
  USE IN COMPFILE
ENDIF
LLREUSEDBY = .F.
*B601818,1  end
LLARRAYORVAR = TYPE('lcArray[1]')='C'
LLTWODIMEN = IIF(LLARRAYORVAR AND ALEN(LCARRAY,2)=2,IIF(TYPE('lcArray[1,2]')='L' OR TYPE(LCARRAY[1,1])='U','A','N'),'V' )
IF !LLARRAYORVAR AND ',' $ LCARRAY
  DIMENSION LAVARARR[1]
  =GFSUBSTR(LCARRAY,@LAVARARR)
  DIMENSION LCARRAY[ALEN(laVarArr)]
  =ACOPY(LAVARARR,LCARRAY)
  LLARRAYORVAR = .T.
  LLTWODIMEN = 'V'
  *ELSE
  *  lcArray = UPPER(PADR(LCARRAY,10))
ENDIF
IF !USED('SETUPS')
  SELECT 0
  *B601818,1 use SETUPS from the company directory
  *USE (oAriaApplication.DataDir+'SETUPS')
  USE (LCCOMPDIR+'SETUPS') AGAIN
  *B601818,1 end
  LLSUSEDBY = .T.
ELSE
  SELECT SETUPS
  *B601818,1 Check if the file is opened from the company path
  LCSETPATH = SET('FULLPATH')
  SET FULLPATH ON
  LCSETUPSDIR = DBF()
  SET FULLPATH &LCSETPATH
  IF !(LCCOMPDIR) $ LCSETUPSDIR
    LNCURTAG  = VAL(SYS(21))
    LNCURREC  = RECNO()
    USE (LCCOMPDIR+'SETUPS') AGAIN
    LLREUSEDBY = .T.
  ENDIF
  *B601818,1 end
ENDIF
LCSETUPTAG =TAG()
SET ORDER TO TAG VARNAME
LCRETVAL=''
LNRETCOUNT=0
LCONERR=ON('ERROR')
ON ERROR LLERROR = .T.
IF !LLARRAYORVAR
  IF SEEK(PADR(UPPER(LCARRAY),10))
    DO CASE
    CASE CDEFA_TYP='V'
      LCRETVAL = LFTRNSSTR(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''),CDATA_TYP)
    CASE CDEFA_TYP='E'
      LCRETVAL = EVAL(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''))
    ENDCASE
  ELSE
    *-- Hesham (Start)
    *!*	    llUseSycC = .T.
    *!*	    IF !USED('SYCCONFG')
    *!*	      SELECT 0
    *!*	      USE (oAriaApplication.Syspath+'SYCCONFG')
    *!*	      llCUsedBy = .T.
    *!*	    ELSE
    *!*	      SELECT SYCCONFG
    *!*	    ENDIF
    *!*	    lcConfgTag=TAG()
    *!*	    SET ORDER TO TAG VARNAME

    *!*	    IF SEEK(PADR(UPPER(lcArray),10))
    *!*	      DO CASE
    *!*	        CASE cDefa_Typ='V'
    *!*	          lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
    *!*	        CASE cDefa_Typ='E'
    *!*	         lcRetVal = EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
    *!*	      ENDCASE
    *!*	    ENDIF
    LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCCONFG where cfld_name='"+PADR(UPPER(LCARRAY),10)+"'",'',"SYCCONFGTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATASESSION"))
    IF LNREMRESULT>=1
      LOCATE
    ENDIF
    IF LNREMRESULT>=1 AND FOUND()
      DO CASE
      CASE CDEFA_TYP='V'
        LCRETVAL = LFTRNSSTR(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''),CDATA_TYP)
      CASE CDEFA_TYP='E'
        LCRETVAL = EVAL(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''))
      ENDCASE
      USE IN SYCCONFGTMP
    ENDIF
    *-- Hesham (Start)

  ENDIF
ELSE
  *-- Hesham (Start)
  *!*	  llUseSycC = .T.
  *!*	  IF !USED('SYCCONFG')
  *!*	    SELECT 0
  *!*	    USE (oAriaApplication.Syspath+'SYCCONFG')
  *!*	    llCUsedBy = .T.
  *!*	  ELSE
  *!*	    SELECT SYCCONFG
  *!*	  ENDIF
  *!*	  lcConfgTag=TAG()
  *!*	  SET ORDER TO TAG VARNAME
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCCONFG ",'',"SYCCONFGTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATASESSION"))
  *-- Hesham (End)

  FOR LNCOUNT = 1 TO ALEN(LCARRAY,1)
    LLERROR = .F.
    LCRETVAL=''
    SELECT SETUPS
    IF SEEK(PADR(UPPER(IIF(LLTWODIMEN = 'V',LCARRAY[lnCount],LCARRAY[lnCount,1])),10))
      DO CASE
      CASE CDEFA_TYP='V'
        LCRETVAL=LFTRNSSTR(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''),CDATA_TYP)
      CASE CDEFA_TYP='E'
        LCRETVAL=EVAL(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''))
      ENDCASE
    ELSE
      *-- Hesham (Start)
      *SELECT SYCCONFG
      *IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      SELECT SYCCONFGTMP
      LOCATE FOR CFLD_NAME = PADR(UPPER(IIF(LLTWODIMEN = 'V',LCARRAY[lnCount],LCARRAY[lnCount,1])),10)
      IF FOUND()
        *-- Hesham (End)
        DO CASE
        CASE CDEFA_TYP='V'
          LCRETVAL = LFTRNSSTR(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''),CDATA_TYP)
        CASE CDEFA_TYP='E'
          LCRETVAL = EVAL(STRTRAN(MDATA_DEF,CHR(13)+CHR(10),''))
        ENDCASE
      ENDIF
    ENDIF
    DO CASE
    CASE LLTWODIMEN = 'N'
      &LCARRAY[lnCount,2] = LCRETVAL
    CASE LLTWODIMEN = 'V'
      &LCARRAY[lnCount] = LCRETVAL
    CASE LLTWODIMEN = 'A'
      LCARRAY[lnCount,2] = LCRETVAL
    ENDCASE
    LNRETCOUNT=LNRETCOUNT+IIF(!LLERROR,1,0)
  ENDFOR
  IF USED("SYCCONFGTMP")
    USE IN SYCCONFGTMP
  ENDIF
ENDIF
ON ERROR &LCONERR
IF LLUSESYCC
  SELECT SYCCONFG
  IF !EMPTY(LCCONFGTAG)
    SET ORDER TO TAG (LCCONFGTAG)
  ELSE
    SET ORDER TO
  ENDIF
  IF LLCUSEDBY
    USE IN SYCCONFG
  ENDIF
ENDIF

SELECT SETUPS
IF !EMPTY(LCSETUPTAG)
  SET ORDER TO TAG (LCSETUPTAG)
ELSE
  SET ORDER TO
ENDIF
IF LLSUSEDBY
  USE IN SETUPS
ENDIF

*B601818,1 ReUse SETUPS file
IF LLREUSEDBY .AND. !EMPTY(LCSETUPSDIR)
  SELECT SETUPS
  USE (LCSETUPSDIR) ORDER LNCURTAG
  IF BETWEEN(LNCURREC, 1, RECCOUNT())
    GO LNCURREC
  ELSE
    GO TOP
  ENDIF
ENDIF
*B601818,1 end

SELECT (LNALIASNO)
RETURN IIF(!LLARRAYORVAR,LCRETVAL,LNRETCOUNT)

*!*************************************************************
*! Name      : gfOpenFile
*! Developer : MALAK - Malak Hanna
*! Date      : 04/18/1995
*! Purpose   : To open database needed by the program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name, Index Tag,
*!                          Open Mode "EX" ----> "EXCLUSIVE"
*!                                    "SH" ----> "SHARED"
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
*! Example            : =gfOpenFile(QDD+'ORDHDR',QDD+'ORDHDR','SH')
*!*************************************************************
*E300247,1 YMA 06/13/95 Changed the displaying of the file
*E300247,1 name to be in the status message.
*B602015,1 AHM 05/07/98 using alias name to use the file by this alias
*!*************************************************************

FUNCTION GFOPENFILE
PARAMETERS NFILE,LCINDEX,MODE,LCALIASNAM,LLFORCEOP
PRIVATE MODE,LCFILENAME,LCPATH,LLRETURNVAL,LCMSG,LCSETEXACT

PRIVATE LCMACROSUB
LCMACROSUB=""
LCFILENAME = IIF(ATC('\',NFILE)<>0,SUBSTR(NFILE,RAT('\',NFILE)+1),NFILE)
LCOPENMODE = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
LCORDERTAG = IIF(TYPE('lcIndex')='C',SUBSTR(LCINDEX,IIF('\' $ LCINDEX,ATC('\',LCINDEX,OCCURS('\',LCINDEX)),0) +1),'')

PRIVATE LLOPEN
LLOPEN = .F.

LCALIASNAM = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(LCALIASNAM),ALLTRIM(STRTRAN(UPPER(LCFILENAME),".DBF")),LCALIASNAM)

LCMSG = 'Opening '+NFILE+IIF(EMPTY(LCINDEX),'', ' Index Tag '+LCORDERTAG)+'....'
LCMSG = PROPER(LCMSG)
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO LCMSG
ENDIF
LLRETURNVAL = .T.
LCFPATHST   = SET('FULLPATH')
SET FULLPATH ON
IF USED(LCALIASNAM)
  LCOPENMODE = "SHARED"
  *-- if the file is used and it is from the same data directory
  IF DBF(LCALIASNAM) == ALLTRIM(STRTRAN(UPPER(NFILE), ".DBF") + ".DBF")
    *-- if forced open is desired
    IF LLFORCEOP
      LCALIASNAM  = GFTEMPNAME()
      LCMACROSUB="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &LCMACROSUB
      LLOPEN = .T.
      IF !EMPTY(LCORDERTAG)
        SET ORDER TO TAG LCORDERTAG IN (LCALIASNAM)
      ENDIF    &&IF !EMPTY(lcOrderTag)
    ELSE
      *-- if forced open is not desired
      LLRETURNVAL = .F.
      *-- if there is no tag is desired to set order to
      IF EMPTY(LCORDERTAG)
        SET ORDER TO 0 IN (LCALIASNAM)
      ELSE
        SET ORDER TO TAG LCORDERTAG IN (LCALIASNAM)
      ENDIF   &&IF EMPTY(lcOrderTag)
    ENDIF     &&IF llForceOp
  ELSE
    *-- if the file is used but not from the same data directory
    LCALIASNAM  = GFTEMPNAME()
    LCMACROSUB="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
    &LCMACROSUB
    LLOPEN = .T.
    IF !EMPTY(LCORDERTAG)
      SET ORDER TO TAG LCORDERTAG IN (LCALIASNAM)
    ENDIF  &&IF !EMPTY(lcOrderTag)
  ENDIF   &&IF DBF(lcFilename) == .......
ELSE
  *-- if the file is not used

  LCMACROSUB = "USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
  &LCMACROSUB
  LLOPEN = .T.
  IF !EMPTY(LCORDERTAG)
    SET ORDER TO TAG LCORDERTAG IN (LCALIASNAM)
  ENDIF    &&IF !EMPTY(lcOrderTag)
ENDIF    &&IF IF USED(lcFilename)
SELECT (LCALIASNAM)
SET FULLPATH &LCFPATHST
*! B609424,1 MMT 10/11/2010 Fix bug of Error in Rebalance program (PO module)[Start]
*!*	IF !UPPER(oAriaApplication.WorkDir) $ UPPER(NFILE)
*!*	  lcErrOn = ON('ERROR')
*!*	  llError = .F.
*!*	  ON ERROR llError = .T.
*!*	  IF TYPE('laFileName')<>'U'
*!*	    lcSetExact = SET('Exact')
*!*	    SET EXACT ON
*!*	    FOR lnFilePos = 1 TO ALEN(laFileName,1)
*!*	      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
*!*	        EXIT
*!*	      ENDIF
*!*	    ENDFOR
*!*	    IF lnFilePos > ALEN(laFileName,1)
*!*	      lnFilePos = 0
*!*	    ENDIF
*!*	    IF lnFilePos = 0
*!*	      IF !EMPTY(laFileName[1,1])
*!*	        DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
*!*	      *-- MAN Added ELSE Cond.
*!*	      ELSE
*!*	        DIME laFileName[1,4]
*!*	      ENDIF
*!*	      laFileName[ALEN(laFileName,1),1] = lcAliasNam
*!*	      laFileName[ALEN(laFileName,1),2] = lcOrderTag
*!*	      laFileName[ALEN(laFileName,1),3] = NFILE
*!*	      laFileName[ALEN(laFileName,1),4] = lcFilename
*!*	      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
*!*	        IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
*!*	          EXIT
*!*	        ENDIF
*!*	      ENDFOR
*!*	      IF lnFileElm > ALEN(gaMnu_Fl,1)
*!*	        lnFileElm = 0
*!*	      ENDIF
*!*	      IF lnFileElm > 0
*!*	        lnRowNo = lnFileElm
*!*	        gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
*!*	      ELSE
*!*	        DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
*!*	        =AINS(gaMnu_Fl,1)
*!*	        gaMnu_Fl[1,1] = lcAliasNam
*!*	        gaMnu_Fl[1,2] = lcOrderTag
*!*	        gaMnu_Fl[1,3] = SELECT(0)
*!*	        gaMnu_Fl[1,4] = 1
*!*	        gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
*!*	        gaMnu_Fl[1,6] = " "
*!*	      ENDIF
*!*	    ENDIF
*!*	    SET EXACT &lcSetExact
*!*	  ENDIF
*!*	  ON ERROR &lcErrOn
*!*	ENDIF
*! B609424,1 MMT 10/11/2010 Fix bug of Error in Rebalance program (PO module)[End]
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO ""
ENDIF
RETURN LLRETURNVAL

FUNCTION GFSYSCLOSE
PARAMETERS LCFILE
PRIVATE LNFILEPOS

LNFILEPOS = 0
IF (TYPE('laFileName[1,1]')='C' AND !EMPTY(LAFILENAME[1,1]))
  FOR LNFILEPOS = 1 TO ALEN(LAFILENAME,1)
    IF ALLTRIM(LAFILENAME[lnFilePos,1]) == ALLTRIM(LCFILE)
      EXIT
    ENDIF
  ENDFOR
  IF LNFILEPOS > ALEN(LAFILENAME,1)
    LNFILEPOS = 0
  ENDIF
ENDIF
IF LNFILEPOS = 0 AND USED(LCFILE)
  USE IN (LCFILE)
ENDIF

*!*************************************************************
*! Name      : gfGetDataDir
*! Developer : Hesham El-Sheltawi
*! Date      : 12/16/1998
*! Purpose   : Function to return company data directory
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) company data dir
*!*************************************************************
*! Example	      	 : lcDataDir=gfGetDataDir(syccomp.ccom_ddir)
*!*************************************************************
*E301078,1 HSS 11/29/98 Add this function.
*!*************************************************************
*
FUNCTION GFGETDATADIR
PARAMETERS LCDATADIR

RETURN (OARIAAPPLICATION.GETDATADIR(LCDATADIR))

*************************************************************************
*FUNCTION CheckPrd
*DESC: Function to validate transaction date
*NOTE: This function is called from evry transaction program.
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed
*PARA: ldDate   : Transaction date to be check
*    : lcPeriod : Transaction Period
*    : lcFYear  : Transaction Fiscal Year
*    : lcTranTyp: Type of transaction calls this function
*! MODI:  WAM 09/19/94
*!        1) Modified to call the function 'gfDialog' instead of the
*!           function 'MsgCenter' to display messages when validate
*!           transactions dates. Function'MsgCenter' has been deleted also.
*!B602317,1 WAM 12/06/98 Open SBT system company file with another name
*!B802236,1 AHM 08/05/1999 Allow voiding invoice in prior period
*************************************************************************
FUNCTION CHECKPRD
PARAMETERS LDDATE,LCFYEAR,LCPERIOD,LCTRANTYP,LLHIDEMSG
PRIVATE LCDTYPE,LCADDMES1,LCADDMES2,LCSYSDIR,LCGLVERS,LCGLCOMP, ;
  LCDATE,LLCONTINUE,LCERRORM1,LCERRORM2, LNALIAS

LNALIAS = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=GFGETMEMVAR('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',OARIAAPPLICATION.ACTIVECOMPANYID)

LCSYSDIR   = ALLTRIM(M_SYS_DIR)
LCGLVERS   = ALLTRIM(M_GL_VERS)
LCGLCOMP   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO LCDTYPE,LCADDMES1,LCADDMES2
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

LCDATE = DTOC(LDDATE)      && Transaction date as a string used in messages
IF LCGLVERS = 'S'            &&   <<<... SBT 2.5 ... >>>
  *B602317,1 Open SBT system company file with another name
  *=gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  *=SEEK(lcGlComp,'SYCCOMP')

  USE (LCSYSDIR+'SYCCOMP') ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(LCGLCOMP,'SBTCOMP')
  *B602317,1 (End)

  =GFOPENFILE(LCSYSDIR+'SYCHFIS',LCSYSDIR+'COMPID1','SH')
  =GFOPENFILE(LCSYSDIR+'SYCDFIS',LCSYSDIR+'COMPID1','SH')

  LLCONTINUE = .T.
  IF SEEK(LCGLCOMP)
    LOCATE REST FOR BETWEEN(LDDATE,BDATE,EDATE) ;
      WHILE (LDDATE >= BDATE) .AND. (COMPID = LCGLCOMP)
  ENDIF
  IF !FOUND()                && No period match checked date
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' does not fall within any period. '
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG1,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG1",LCARIAHFILE))+' '
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    LCERRORM2 = ''
  ELSE
    &LCFYEAR  = SUBSTR(YEARPRD,1,4)      && Transaction date year
    &LCPERIOD = SUBSTR(YEARPRD,5,2)      && Transaction date period
  ENDIF
  IF LLCONTINUE .AND. PERMLCK         && Permanently locked period
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' falls in a permanently locked period.'
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG2,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG2",LCARIAHFILE))
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    LCERRORM2 = ''
  ENDIF
  IF LLCONTINUE .AND. PLOCKED         && Locked period
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' falls in a locked period.'
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG3",LCARIAHFILE))
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    LCERRORM2 = ''
  ENDIF
  IF LLCONTINUE              && So far so good
    IF PCLOSED               && Closed period
      IF !(LCTRANTYP $ 'VI2VR2')  && Transaction is neither
        && 'Void invoice' nor 'void return'.
        LLDUMMY =  FERRINFO(LCTRANTYP,'lcDType','lcAddMes1','lcAddMes2')
        *N000682,1 MMT 11/21/2012 globalization Changes [Start]
        *lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        LCERRORM1 = '&lcDType&lcDate '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG4,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG4",LCARIAHFILE))
        *N000682,1 MMT 11/21/2012 globalization Changes [Start]
        LCERRORM2 = ''
        =OARIAAPPLICATION.MESSAGEBOX('INM00274B00000','ALERT',LCERRORM1+LCERRORM2)
      ELSE
        LLCONTINUE = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      *B602317,1 Open SBT system company file with another name
      *IF Yearprd <>  SYCCOMP.CURYR+SYCCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      IF YEARPRD <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(LCTRANTYP $ 'VI2VR2')
        *B602317,1 (End)

        LLDUMMY   =  FERRINFO(LCTRANTYP,'lcDType','lcAddMes1','lcAddMes2')
        *N000682,1 MMT 11/21/2012 globalization Changes [Start]
        *lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        LCERRORM1 = '&lcDType&lcDate '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG5",LCARIAHFILE))
        *N000682,1 MMT 11/21/2012 globalization Changes [END]
        LCERRORM2 = ''
        =OARIAAPPLICATION.MESSAGEBOX('INM00274B00000','ALERT',LCERRORM1+LCERRORM2)
      ENDIF
    ENDIF
  ENDIF

  *B602317,1 Open SBT system company file with another name
  USE IN SBTCOMP
  *B602317,1 (End)
ELSE

  *  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'CCOMP_ID','SH')
  *  =SEEK(oAriaApplication.PrntCompanyID,'SYCCOMP')
  LCCOMPALIAS = GFTEMPNAME()
  LCSELECT = "Select * from SYCCOMP where ccomp_id='"+OARIAAPPLICATION.PRNTCOMPANYID+"'"
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE(LCSELECT,'',LCCOMPALIAS,"",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))

  IF 'GL' $ &LCCOMPALIAS..MMODLSET
    *=gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP','','SH')
    USE (GFGETDATADIR(ALLTRIM(&LCCOMPALIAS..CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    LDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    LLALLPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP
  ELSE
    LDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    LLALLPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF
  *E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
  *=gfOpenFile(gcSysHome+'SYCFISHD',gcSysHome+'COMPFYEAR','SH')
  *=gfOpenFile(gcSysHome+'SYCFSPRD',gcSysHome+'COMFYRPRDI','SH')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'FISHD',OARIAAPPLICATION.DATADIR+'COMPFYEAR','SH')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'FSPRD',OARIAAPPLICATION.DATADIR+'COMFYRPRDI','SH')

  *E300692,5 end
  LLCONTINUE = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(LDDATE,DFSPPBGDT,DFSPPENDT) ;
      WHILE (LDDATE >= DFSPPBGDT)
  ENDIF
  IF !FOUND()                  && No period match checked date
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' does not fall within any period. '
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG1,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG1",LCARIAHFILE))+' '
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    LCERRORM2 = ''
  ELSE
    &LCFYEAR  = CFISFYEAR      && Transaction date year
    &LCPERIOD = CFSPPRDID      && Transaction date period
  ENDIF
  IF LLHIDEMSG
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
    USE IN (LCCOMPALIAS)
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]
    SELECT (LNALIAS)
    RETURN(LLCONTINUE)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF LLCONTINUE .AND. CFISFYEAR < STR(VAL(&LCCOMPALIAS..CCURR_YER)-1)
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' belongs to a history fiscal year.'
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG6,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG6",LCARIAHFILE))
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    LCERRORM2 = ''
  ENDIF
  IF LLCONTINUE
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    *-- AAMER 11/12/98
    *IF !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    IF LCGLVERS='A' AND !LLALLPBB AND !EMPTY(LDSETBBDAT) .AND. LDDATE < LDSETBBDAT
      *-- Check if the system is linked to GL And Allow posting before beginning Balance (End)
      LLCONTINUE = .F.
      *N000682,1 MMT 11/21/2012 globalization Changes [Start]
      *!*	      lcErrorM1 = ' falls before the begining balance date.'
      *!*	      lcErrorM2 = ' No posting allowed before the begining balance date. '
      LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG7,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG7",LCARIAHFILE))
      LCERRORM2 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG8,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG8",LCARIAHFILE))+' '
      *N000682,1 MMT 11/21/2012 globalization Changes [END]
    ENDIF
  ENDIF
  IF LLCONTINUE .AND. LFSPLOCKS         && Locked period
    LLCONTINUE = .F.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1 = ' falls in a locked period.'
    LCERRORM1 = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG3",LCARIAHFILE))
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    LCERRORM2 = ''
  ENDIF
  IF LLCONTINUE
    IF LFSPCLSDS               && Closed period
      IF !(LCTRANTYP $ 'VI2VR2')
        LLDUMMY =  FERRINFO(LCTRANTYP,'lcDType','lcAddMes1','lcAddMes2')
        *N000682,1 MMT 11/21/2012 globalization Changes [Start]
        *lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        LCERRORM1 = '&lcDType&lcDate '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG4,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG4",LCARIAHFILE))
        *N000682,1 MMT 11/21/2012 globalization Changes [END]
        LCERRORM2 = ''
        *E300420,1 Message : 00274
        *E300420,1
        *E300420,1 Button : 00000
        *E300420,1 Ok
        IF LCTRANTYP # 'VI1'
          =OARIAAPPLICATION.MESSAGEBOX('INM00274B00000','ALERT',LCERRORM1+LCERRORM2)
        ENDIF
      ELSE
        IF LCTRANTYP # 'VI2'
          LLCONTINUE = .F.
        ENDIF
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF CFISFYEAR+CFSPPRDID <> &LCCOMPALIAS..CCURR_YER+&LCCOMPALIAS..CCURR_PRD .AND. !(LCTRANTYP $ 'VI2VR2')
        LLDUMMY =  FERRINFO(LCTRANTYP,'lcDType','lcAddMes1','lcAddMes2')
        *N000682,1 MMT 11/21/2012 globalization Changes [Start]
        *lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        LCERRORM1 = '&lcDType&lcDate '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG5",LCARIAHFILE))
        *N000682,1 MMT 11/21/2012 globalization Changes [ENd]
        LCERRORM2 = ''
        *E300420,1 Message : 00274
        *E300420,1
        *E300420,1 Button : 00000
        *E300420,1 Ok
        =OARIAAPPLICATION.MESSAGEBOX('INM00274B00000','ALERT',LCERRORM1+LCERRORM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF
  ENDIF
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
  USE IN (LCCOMPALIAS)
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]
ENDIF
IF !LLCONTINUE             && There is an error.
  IF LCTRANTYP $ 'VI2VR2'       && Transaction is either 'Void invoice'
    && or 'Void return'
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcErrorM1  = ' not in the current period. '
    LCERRORM1  = ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CHKPRD_MSG9,OARIAAPPLICATION.GETHEADERTEXT("LANG_CHKPRD_MSG9",LCARIAHFILE))+' '
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    LCERRORM2 = ''
  ENDIF
  LLDUMMY =  FERRINFO(LCTRANTYP,'lcDType','lcAddMes1','lcAddMes2')
  LCERRORM1= LCDTYPE + LCDATE + LCERRORM1
  *E300420,1 Message : 00274
  *E300420,1
  *E300420,1 Button : 00000
  *E300420,1 Ok
  IF LCTRANTYP # 'VI2'
    =OARIAAPPLICATION.MESSAGEBOX('INM00274B00000','ALERT',LCERRORM1+LCERRORM2+LCADDMES1+LCADDMES2)
  ENDIF
  *=gfDialog( 'I',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  SELECT (LNALIAS)
  RETURN(.F.)
ENDIF
SELECT (LNALIAS)
RETURN(.T.)



*!*************************************************************
*! Name      : gfModalGen
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/13/2002
*! Purpose   : To display any data driven dialog
*!*************************************************************
*! Passed Parameters  :
*!                1-lcDlgID   (Dialog ID)
*!                          1st 2 characters are TR for Terminat icon
*!                                               QR for Quiry    icon
*!                                               IN for Inform   icon
*!                          2nd 4 characters are the messag ID
*!                          3rd 4 characters are the button ID
*!                2-lcDlgTyp  (Dialog type)
*!                          'D' --> Dialog colors
*!                          'A' --> Alert  colors
*!                3-lcVarsStr  (variable(s) to be replased in the messag
*!                4-lcDlgValid (Validation function name to be used in
*!                              the valid of the dialog buttons)
*!                5-lcDlgMessg if you want to display a specific message
*!                            send the message string to this parameter
*!
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*: Modification:
*! B037981,1 MAH 04/17/2004 Toolbar is active while aria message displayed.
*! N119813,FW1 MAH Enable toolbar before return.
*:***********************************************************************
* Function to display any messag with any button from the
* of dialog object file.  The width and hight of the dialog window
* will be calculated according to the messag width and No. of buttons.
* Parameters are:
*:->
FUNCTION GFMODALGEN
LPARAMETER LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG
*RETURN oAriaApplication.MessageBox(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)

*E303920,1 MMT 01/30/2018 Call Aria5 Messaging function[T20180124.0005][Start]

*E611881,1 Es 08/18/2019 When aria4XP screens is opened in Aria5, messages are displayed in the old format [Start]
*!*	IF oAriaApplication.Context == 5
*!*	  RETURN GFMODALGEN5(LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG)
*!*	ENDIF 
IF oAriaApplication.Context == 5 or oAriaApplication.llNewInterface 
  lnOldContextVal = oAriaApplication.Context 
  oAriaApplication.Context = 5
  lnRetVal5 = GFMODALGEN5(LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG)
  oAriaApplication.Context = lnOldContextVal 
  RETURN lnRetVal5 
ENDIF 
*E611881,1 Es 08/18/2019 When aria4XP screens is opened in Aria5, messages are displayed in the old format [End]

*E303920,1 MMT 01/30/2018 Call Aria5 Messaging function[T20180124.0005][End]

* B037981,1 MAH Disable toolbar before show message
IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
  OARIAAPPLICATION.OTOOLBAR.ENABLED = .F.
ENDIF
* B037981,1 MAH End

LOCAL OMESSAGEBOX, LLACTIVEFORMLOCKED, LLFORMEXIST
OMESSAGEBOX = NEWOBJECT("AriaMessageBox",ADDBS(OARIAAPPLICATION.CLASSDIR)+"Utility.vcx")
IF VARTYPE(OMESSAGEBOX) != "O"
  * N119813,FW1 MAH Enable toolbar before return
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
    OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
  ENDIF
  * N119813,FW1 MAH End

  RETURN 0
ENDIF

*-- Get the dialog and buttons from the dictionary.
IF !OMESSAGEBOX.GETMESSAGE(LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG)
  * N119813,FW1 MAH Enable toolbar before return
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
    OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
  ENDIF
  * N119813,FW1 MAH End

  RETURN 0
ENDIF

IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM)
  LLFORMEXIST = .T.
  LLACTIVEFORMLOCKED = _SCREEN.ACTIVEFORM.LOCKSCREEN
  _SCREEN.ACTIVEFORM.LOCKSCREEN = .F.
ENDIF

PRIVATE LNMESSAGECHOICE
LNMESSAGECHOICE = 1
OMESSAGEBOX.SETMESSAGEBOX()    && Set message parameters.

PUSH KEY
ON KEY
OMESSAGEBOX.SHOW()  && Show the message.
POP KEY

OMESSAGEBOX = .NULL.
RELEASE OMESSAGEBOX

IF LLFORMEXIST AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM)
  _SCREEN.ACTIVEFORM.LOCKSCREEN = LLACTIVEFORMLOCKED
ENDIF

* B037981,1 MAH Enable toolbar before show message
IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
  OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
ENDIF
* B037981,1 MAH End

RETURN LNMESSAGECHOICE  && Return the response.
ENDFUNC
*-- end of gfModalGen.

*!*************************************************************
*! Name      : gfSequence                    E:300632
*! Developer : Wael Aly Mohamed
*! Date      : 03/04/1997
*! Purpose   : To get new sequance number for any item
*!*************************************************************
*! Calls     :  GFADD_INFO()
*!              gfRltFld()
*!*************************************************************
*! Passed Parameters  : Sequance type
*!                      Company ID
*!                      Group ID
*!                      Division Code
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :  lcData[1] = gfSequence('CINVOICE')
*!*************************************************************
*! Modifications
*! E300888 06/04/98 YMA Added to generate the required code
*!                      prefixed with a unique 2 characters
*!                      code that representthe current site
*!                      in case if the "CM" Communication
*!                      module is installed.
*!B802982,1 02/01/2000 HDM Don't GET the GroupID if the system is not set to
*!                     generate seq.# based on division
*!*************************************************************
FUNCTION GFSEQUENCE
*B608122,1  TMI [Start] Add three parameters for Document#,Sql Table, sql Tag
*PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
PARAMETERS LCSEQTYPE,LCCOMPANYID,LCGROUPID,LCDIVISION,LCFIELD,LCTRANTYPE,LCTABLE,LCTAG
*B608122,1  TMI [End  ]
PRIVATE LNRETVAL,LCSAVALIAS,LCDATADIR,LNOLDGENNM,LCEXTRASTR,LCTOFIND,LCKEYEXP,;
  GCDATADIR,GCCOMP_MDL,GCSYSHOME,GCCURSITE,GCACT_COMP,GCORGPATH

*!*	gcDataDir  = oAriaApplication.DataDir
*!*	gcComp_Mdl = oAriaApplication.CompanyInstalledModules
*!*	gcSysHome  = oAriaApplication.SysPath
*!*	gcCurSite  = oAriaApplication.CurrentSite
*!*	gcAct_Comp = oAriaApplication.ActiveCompanyId
*!*	gcOrgPath  = oAriaApplication.DefaultPath
*!*	*TAK E300973,1 end.

*!*	*E300894,1 06/18/98 YMA Validate the optional passed parameter.
*!*	lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*!*	*E300894,1 06/18/98 YMA End.

*!*	lcSavAlias = SELECT(0)
*!*	lcSeqType  = UPPER(lcSeqType)
*!*	lcDataDir = gcDataDir

*!*	*E300888 06/04/98 YMA If the communication module is installed, then
*!*	*E300888              get the unique site prefix for the active site
*!*	*E300888              from the sites file.
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*!*	*lcUnqPreFx = SPACE(0)
*!*	*IF "CM" $ gcComp_Mdl
*!*	*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*!*	*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*!*	*  USE IN Sites
*!*	*ENDIF
*!*	PRIVATE lcCmpCode
*!*	PRIVATE oGetMemVar
*!*	oGetMemVar = CREATEObject("GetMemVar", This.oForm)

*!*	lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
*!*	lcUnqPreFx = oGetMemVar.Do('M_UNQSTPRX' , lcCmpCode)
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*!*	*E300888 06/04/98 YMA End.

*!*	IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
*!*	  IF USED("sycComp")
*!*	    SELECT sycComp
*!*	    luSycComp = .F.
*!*	    ltSycComp = VAL(SYS(21))
*!*	    leSycComp = RECNO()
*!*	    SET ORDER TO TAG cComp_Id IN syccomp
*!*	  ELSE
*!*	    luSycComp = .T.
*!*	    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
*!*	  ENDIF

*!*	  IF SEEK(lcCompanyId,'syccomp')
*!*	    *E301098,1 Hesham (Start)
*!*	    *lcDataDir = ALLTRIM(syccomp.cCom_dDir)
*!*	    *IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
*!*	    *   UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
*!*	    *   UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
*!*	    *   UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
*!*	    *  lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
*!*	    *             SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
*!*	    *ENDIF
*!*	    lcDataDir = gfGetDataDir(ALLTRIM(syccomp.cCom_dDir))
*!*	  ENDIF
*!*	  IF luSycComp
*!*	    USE IN syccomp
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSycComp IN syccomp
*!*	    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
*!*	      GOTO leSycComp IN 'syccomp'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*E301046,4 Assure that lcGroupId is 3 Char. only
*!*	*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
*!*	lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
*!*	*E301046,4 end
*!*	lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
*!*	lnRetVal   = 0

*!*	*300632,1 Get division sequence group
*!*	*B802982,1 [start] Don't GET the GroupID if the system is not set to
*!*	*                  generate seq.# based on division
*!*	llDivOnSeq = oGetMemVar.Do('M_DIV_SEQ' , lcCompanyId) = 'Y'
*!*	RELEASE oGetMemVar
*!*	*Change this line to check the llDivOnSeq
*!*	*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	*B802982,1 [End]

*!*	  DECLARE laDivDlt[1,2]
*!*	  laDivDlt[1,1] = 'DIVGROUP'
*!*	  laDivDlt[1,2] = 'lcGroupId'
*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  *=gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  PRIVATE oRlatdFields
*!*	  oRlatdFields = CREATEObject("GetRelatedFields", This.oForm)
*!*	  oRlatdFields.Do(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  RELEASE oRlatdFields
*!*	  *TAK E300973,1 End.

*!*	  *E301046,4 Change lcGroupId to be 3 Char. only
*!*	  *lcGroupId = SUBSTR(lcGroupId,1,10)
*!*	  lcGroupId = SUBSTR(lcGroupId,1,3)
*!*	  *E301046,4 end
*!*	ENDIF
*!*	*B802982,1 [start] make sure the group id is empty if the system
*!*	*                  is not set to generate seq.# based on division
*!*	*                  This case will BE FEASABLE ONLY
*!*	*                  IF llDivOnSeq = .F.
*!*	*                  AND !EMPTY(lcGroupId)
*!*	lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*!*	*B802982,1 [End]

*!*	IF !USED('SEQUENCE')
*!*	  luSequence = .T.
*!*	  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
*!*	ELSE
*!*	  SELECT SEQUENCE
*!*	  luSequence = .F.
*!*	  ltSequence = VAL(SYS(21))
*!*	  leSequence = RECNO()
*!*	  SET ORDER TO TAG Cseq_type IN SEQUENCE
*!*	ENDIF

*!*	IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
*!*	  IF !USED('sydflfld')
*!*	    luSydflfld = .T.
*!*	    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydflfld
*!*	    luSydflfld = .F.
*!*	    ltSydflfld = VAL(SYS(21))
*!*	    leSydflfld = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydflfld'
*!*	  ENDIF
*!*	  IF !USED('sydfield')
*!*	    luSydfield = .T.
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydfield
*!*	    luSydfield = .F.
*!*	    ltSydfield = VAL(SYS(21))
*!*	    leSydfield  = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydfield'
*!*	  ENDIF
*!*
*!*	  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
*!*	  *E300894,1              proprities instead of the sequence field
*!*	  *E300894,1              if any.
*!*	  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
*!*	  = SEEK(PADR(lcPropFld,10),'sydfield')
*!*	  SELECT sydflfld
*!*	  = SEEK(PADR(lcPropFld,10))
*!*	  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
*!*
*!*	  *=SEEK(PADR(lcSeqType,10),'sydfield')
*!*	  *SELECT sydflfld
*!*	  *=SEEK(PADR(lcSeqType,10))
*!*	  *LOCATE REST WHILE cFld_Name=PADR(lcSeqType,10) FOR lEnumerate
*!*	  *E300894,1 06/18/98 YMA End.

*!*	  lnDefSeq = sydflfld.nDef_Seq
*!*	  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
*!*	    SELECT SEQUENCE
*!*	    lnDefSeq = 0
*!*	    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
*!*	      lnDefSeq = MAX(lnDefSeq,nSeq_No)
*!*	    ENDSCAN
*!*	    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
*!*	  ENDIF
*!*
*!*	  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
*!*	       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
*!*	       sydfield.nFld_Wdth)
*!*	  IF sydflfld.lEnumerate
*!*	    IF !USED('sydfiles')
*!*	      luSydfiles = .T.
*!*	      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
*!*	    ELSE
*!*	      SELECT Sydfiles
*!*	      luSydfiles = .F.
*!*	      ltSydfiles = VAL(SYS(21))
*!*	      leSydfiles = RECNO()
*!*	      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
*!*	    ENDIF
*!*	    =SEEK(sydflfld.cFile_Nam,'sydfiles')
*!*	    SELECT SEQUENCE
*!*	    REPLACE cFile_Nam WITH sydfiles.cFile_Nam ,;
*!*	            cFile_Tag WITH sydfiles.cFile_Tag
*!*	    IF luSydfiles
*!*	      USE IN Sydfiles
*!*	    ELSE
*!*	      SET ORDER TO TAG ltSydfiles IN Sydfiles
*!*	      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
*!*	        GOTO leSydfiles IN 'Sydfiles'
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydflfld
*!*	    USE IN Sydflfld
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydflfld IN Sydflfld
*!*	    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
*!*	      GOTO leSydflfld IN 'Sydflfld'
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydfield
*!*	    USE IN Sydfield
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydfield IN Sydfield
*!*	    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
*!*	      GOTO leSydfield IN 'Sydfield'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*--MAN Added RLOCK Condition[Start]
*!*	   DO WHILE !RLOCK("SEQUENCE")
*!*	   ENDDO
*!*	  lnRetVal = SEQUENCE.nSeq_No
*!*	*--MAN Added RLOCK Condition[End]

*!*	*E300888 06/04/98 YMA Compute the required code width assuming that
*!*	*E300888              the minemum code field width = 6.
*!*	lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*!*	*E300888 06/04/98 YMA End.
*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	lnOldGenNm = SEQUENCE.nSeq_No
*!*	lcExtraStr = ''
*!*	IF !EMPTY(SEQUENCE.cSeq_Chr)
*!*	  lcExtraStr = SEQUENCE.cSeq_Chr
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	IF !EMPTY(SEQUENCE.cFile_Nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
*!*	  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
*!*	  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
*!*	  IF !USED(lcSeqFile)
*!*	    luSeqFile = .T.
*!*	    *B601946,1 Use the file again to prevent 'File is in use' message
*!*	    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    *B601946,1 end
*!*	  ELSE
*!*	    SELECT (lcSeqFile)
*!*	    luSeqFile = .F.
*!*	    ltSeqFile = VAL(SYS(21))
*!*	    leSeqFile = RECNO()
*!*	    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
*!*	  ENDIF
*!*	  SELECT (lcSeqFile)
*!*	  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
*!*	  DECLARE laVldEnt[1]

*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  PRIVATE oGetValid
*!*	  oGetValid = CREATEObject("GetValidEntries")
*!*	* IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
*!*	  IF !EMPTY(lcKeyField) .AND. oGetValid.Do(lcKeyField,@laVldEnt) > 0
*!*	    FOR lnCount = 1 TO ALEN(laVldEnt,1)
*!*	      *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	      *E300888              the unique site prefix.
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
*!*	      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	      *E300888 06/04/98 YMA End.
*!*	        lnRetVal = lnRetVal + 1
*!*	        *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	        IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	          lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	        ENDIF
*!*	        lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                         ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	        *B603586,1 SSH 29/02/00  (End)

*!*	      ENDDO
*!*	    ENDFOR
*!*	  ELSE
*!*	    *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	    *E300888              the unique site prefix.
*!*	    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	    lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                     ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
*!*	    DO WHILE SEEK(lcKeyExp,lcSeqFile)
*!*	    *E300888 06/04/98 YMA End.
*!*	      lnRetVal = lnRetVal + 1
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	        lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	      ENDIF
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	    ENDDO
*!*	  ENDIF
*!*	  RELEASE oGetValid
*!*
*!*	  IF luSeqFile
*!*	    USE IN (lcSeqFile)
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
*!*	    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
*!*	      GOTO leSeqFile IN (lcSeqFile)
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT SEQUENCE
*!*	*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*!*	*REPLACE nSeq_No WITH lnRetVal+1
*!*	REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	IF nSeq_No = 0 .AND. lnOldGenNm <> 0
*!*	  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*!*	ENDIF
*!*	IF !EMPTY(lcExtraStr)
*!*	  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*--MAN Added RLOCK Condition[Start]
*!*	UNLOCK
*!*	*--MAN Added RLOCK Condition[End]

*!*	*TAK E300973,1 Changed to work under visual.
*!*	*=gfAdd_info('SEQUENCE')
*!*	*oAriaApplication.AddUserInformation('SEQUENCE')

*!*
*!*	*E300888 06/04/98 YMA Never return a numeric code, and return the code
*!*	*E300888              prefixed with the active site unique prefix code
*!*	*E300888              if any.
*!*	*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*!*	*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))
*!*	 lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
*!*	*E300888 06/04/98 YMA End.

*!*	IF luSequence
*!*	  USE IN Sequence
*!*	ELSE
*!*	  SET ORDER TO TAG ltSequence IN Sequence
*!*	  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
*!*	    GOTO leSequence IN 'Sequence'
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT (lcSavAlias)
*!*	RETURN(lnRetVal)

GCDATADIR  = OARIAAPPLICATION.DATADIR
GCCOMP_MDL = OARIAAPPLICATION.COMPANYINSTALLEDMODULES
GCSYSHOME  = OARIAAPPLICATION.SYSPATH
GCCURSITE  = OARIAAPPLICATION.CURRENTSITE
GCACT_COMP = OARIAAPPLICATION.ACTIVECOMPANYID
GCORGPATH  = OARIAAPPLICATION.DEFAULTPATH
*TAK E300973,1 end.

*E300894,1 06/18/98 YMA Validate the optional passed parameter.
LCFIELD    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(LCFIELD)), SPACE(0))
*E300894,1 06/18/98 YMA End.

LCSAVALIAS = SELECT(0)
LCSEQTYPE  = UPPER(LCSEQTYPE)
LCDATADIR = GCDATADIR

*E300888 06/04/98 YMA If the communication module is installed, then
*E300888              get the unique site prefix for the active site
*E300888              from the sites file.
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*lcUnqPreFx = SPACE(0)
*IF "CM" $ gcComp_Mdl
*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*  USE IN Sites
*ENDIF
PRIVATE LCCMPCODE

*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [Begin]
PRIVATE LCCHRTOUPD
LCCHRTOUPD = CHR(0)
*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [End]


LCCMPCODE = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(LCCOMPANYID), LCCOMPANYID , GCACT_COMP)
LCUNQPREFX = GFGETMEMVAR('M_UNQSTPRX' , LCCMPCODE)
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*E300888 06/04/98 YMA End.

IF TYPE('lcCompanyId')='C' AND !EMPTY(LCCOMPANYID) AND LCCOMPANYID <> GCACT_COMP
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syccomp where cComp_ID='"+LCCOMPANYID+"'",'',"syccomptmp","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3)
  IF LNREMRESULT>=1
    LOCATE
    LCDATADIR = GFGETDATADIR(ALLTRIM(SYCCOMPTMP.CCOM_DDIR))
  ENDIF
  IF USED("syccomptmp")
    USE IN SYCCOMPTMP
  ENDIF
ENDIF
*E301046,4 Assure that lcGroupId is 3 Char. only
*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
LCGROUPID  = IIF(TYPE('lcGroupId') ='C',PADR(LCGROUPID,3),SPACE(3))
*E301046,4 end
LCDIVISION = IIF(TYPE('lcDivision')='C',ALLTRIM(LCDIVISION),SPACE(10))
LNRETVAL   = 0

*300632,1 Get division sequence group
*B802982,1 [start] Don't GET the GroupID if the system is not set to
*                  generate seq.# based on division
LLDIVONSEQ = GFGETMEMVAR('M_DIV_SEQ' , LCCOMPANYID) = 'Y'
*Change this line to check the llDivOnSeq
*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
IF LLDIVONSEQ AND EMPTY(LCGROUPID) .AND. !EMPTY(LCDIVISION)
  *B802982,1 [End]

  DECLARE LADIVDLT[1,2]
  LADIVDLT[1,1] = 'DIVGROUP'
  LADIVDLT[1,2] = 'lcGroupId'
  =GFRLTFLD(PADR(LCDIVISION,6),@LADIVDLT,'CDIVISION')

  *E301046,4 Change lcGroupId to be 3 Char. only
  *lcGroupId = SUBSTR(lcGroupId,1,10)
  LCGROUPID = SUBSTR(LCGROUPID,1,3)
  *E301046,4 end
ENDIF
*B802982,1 [start] make sure the group id is empty if the system
*                  is not set to generate seq.# based on division
*                  This case will BE FEASABLE ONLY
*                  IF llDivOnSeq = .F.
*                  AND !EMPTY(lcGroupId)
LCGROUPID = IIF(LLDIVONSEQ , SUBSTR(LCGROUPID,1,3) , SPACE(3))
*B802982,1 [End]

IF !USED('SEQUENCE')
  LUSEQUENCE = .T.
  USE (LCDATADIR+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  LUSEQUENCE = .F.
  LTSEQUENCE = VAL(SYS(21))
  LESEQUENCE = RECNO()
  SET ORDER TO TAG CSEQ_TYPE IN SEQUENCE
ENDIF

IF !SEEK(PADR(LCSEQTYPE,10)+LCGROUPID,'SEQUENCE')
  *-- Hesham Start
  *-- B606591,1 Get the active datasession and send it to the RemoteDataAccess Object
  LOCAL LNDATASESS
  LNDATASESS = SET("Datasession")
  *-- Hesham End
  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
  *E300894,1              proprities instead of the sequence field
  *E300894,1              if any.
  LCPROPFLD = IIF(EMPTY(LCFIELD), LCSEQTYPE, LCFIELD)
  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
  *E303030,1 BEGIN
  *lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,10)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataS
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
  *lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,oAriaApplication.FieldW)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  LNREMFLDRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from sydfield where Cfld_name='"+PADR(LCPROPFLD,OARIAAPPLICATION.FIELDW)+"' AND (EMPTY(CVER) OR CVER='A27')",'',"sydfieldtmp","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",LNDATASESS)
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
  *E303030,1 END
  *-- Hesham End
  IF LNREMFLDRESULT=1
    LOCATE
  ENDIF

  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
  *E303030,1 BEGIN
  *lnRemFlFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where Cfld_name='"+PADR(lcPropFld,10)+"' AND lEnumerate=1",'',"sydflfldtmp","",oAriaApplication.SystemConnection
  LNREMFLFLDRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from sydflfld where Cfld_name='"+PADR(LCPROPFLD,OARIAAPPLICATION.FIELDW)+"' AND lEnumerate=1",'',"sydflfldtmp","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",LNDATASESS)
  *E303030,1 END
  *-- Hesham End
  IF LNREMFLFLDRESULT=1
    LOCATE
  ENDIF

  *E303030,1 BEGIN
  *LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
  LOCATE REST WHILE CFLD_NAME=PADR(LCPROPFLD,OARIAAPPLICATION.FIELDW) FOR LENUMERATE
  *E303030,1 END

  LNDEFSEQ = SYDFLFLDTMP.NDEF_SEQ
  IF !EMPTY(LCGROUPID) AND SEEK(PADR(LCSEQTYPE,10),'SEQUENCE')
    SELECT SEQUENCE
    LNDEFSEQ = 0
    SCAN REST WHILE CSEQ_TYPE+CSEQ_GROUP = PADR(LCSEQTYPE,10)
      LNDEFSEQ = MAX(LNDEFSEQ,NSEQ_NO)
    ENDSCAN
    LNDEFSEQ = (INT(LNDEFSEQ/50000)+1)*50000
  ENDIF
  *B606902,4 KHM 02/09/2003 (Begin) Replacing the cSeq_Chr with CHR(0)
  *INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
  VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
  sydfieldtmp.nFld_Wdth)

  INSERT INTO SEQUENCE (CSEQ_TYPE,NSEQ_NO,CSEQ_GROUP,CDATA_TYP,NFLD_WDTH,CSEQ_CHR) ;
    VALUES (LCSEQTYPE,LNDEFSEQ,LCGROUPID,SYDFIELDTMP.CDATA_TYP,;
    SYDFIELDTMP.NFLD_WDTH,CHR(0))
  *B606902,4 KHM 02/09/2003 (End)

  IF SYDFLFLDTMP.LENUMERATE
    *-- Hesham Start
    *-- B606591,1  send the active datasession to the RemoteDataAccess Object
    *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
    *lnRemFlResult = oAriaApplication.remotesystemdata.execute("Select * from sydfiles where Cfile_nam='"+sydflfldtmp.cFile_Nam+"'",'',"sydfilestmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
    LNREMFLRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from sydfiles where Cfile_nam='"+SYDFLFLDTMP.CFILE_NAM+;
      "' AND CVER='A27'",'',"sydfilestmp","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",LNDATASESS)
    *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]
    *-- Hesham End
    IF LNREMFLRESULT=1
      LOCATE
    ENDIF
    SELECT SEQUENCE
    REPLACE CFILE_NAM WITH SYDFILESTMP.CFILE_NAM ,;
      CFILE_TAG WITH SYDFILESTMP.CFILE_TAG
    IF USED("SYDFILESTMP")
      USE IN SYDFILESTMP
    ENDIF
  ENDIF
  IF USED("SYDFLFLDTMP")
    USE IN SYDFLFLDTMP
  ENDIF
  IF USED("sydfieldtmp")
    USE IN SYDFIELDTMP
  ENDIF
ENDIF
*--MAN Added RLOCK Condition[Start]
DO WHILE !RLOCK("SEQUENCE")
ENDDO
LNRETVAL = SEQUENCE.NSEQ_NO

*B606902,1 Get the character expression. [Begin]
LCCHRTOUPD = SEQUENCE.CSEQ_CHR
*B606902,1 Get the character expression. [End]

*--MAN Added RLOCK Condition[End]

*E300888 06/04/98 YMA Compute the required code width assuming that
*E300888              the minemum code field width = 6.
LNRETLEN = SEQUENCE.NFLD_WDTH - LEN(LCUNQPREFX)
*E300888 06/04/98 YMA End.
*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
LNOLDGENNM = SEQUENCE.NSEQ_NO
LCEXTRASTR = ''
IF !EMPTY(SEQUENCE.CSEQ_CHR)
  *B606902,1 Get the Extra string added to the file. [Begin]
  *lcExtraStr = SEQUENCE.cSeq_Chr
  PRIVATE LCCHAR , LNCHARPOS , LNI
  IF !(SEQUENCE.CSEQ_CHR = CHR(0))
    IF MOD(ASC(SEQUENCE.CSEQ_CHR),26) = 0
      LCCHAR = "Z"
      LNCHARPOS = ASC(SEQUENCE.CSEQ_CHR)/26
    ELSE
      LCCHAR =  CHR(MOD(ASC(SEQUENCE.CSEQ_CHR),26)+64)
      LNCHARPOS = INT(ASC(SEQUENCE.CSEQ_CHR)/26)+1
    ENDIF
    FOR LNI = 1 TO LNCHARPOS - 1
      LCEXTRASTR = LCEXTRASTR + "Z"
    ENDFOR
    LCEXTRASTR = LCEXTRASTR + LCCHAR
  ELSE
    LCCHAR=""
  ENDIF
  *B606902,1 Get the Extra string added to the file. [End]
ENDIF
*B603586,1 SSH 29/02/00 (End)
*IF !EMPTY(SEQUENCE.cFile_Nam) AND !EMPTY(SEQUENCE.cFile_Tag)
IF !EMPTY(SEQUENCE.CFILE_NAM) .AND. UPPER(LEFT(SEQUENCE.CFILE_NAM,2))<> 'SY' AND !EMPTY(SEQUENCE.CFILE_TAG)
  LCSEQFILE = ALLTRIM(SEQUENCE.CFILE_NAM)
  LCSEQTAG  = ALLTRIM(SEQUENCE.CFILE_TAG)

  *B608122,1  TMI [Start] check if the parameter is empty
  LCTRANTYPE = IIF(EMPTY(LCTRANTYPE),'',LCTRANTYPE)
  LCSEQFILE = IIF(EMPTY(LCTABLE),LCSEQFILE,LCTABLE)
  LCSEQTAG  = IIF(EMPTY(LCTAG)  ,LCSEQTAG ,LCTAG)
  *B608122,1  TMI [End  ]

  *B132218,1 KHM 05/25/2006 [Start]
  *IF !USED(lcSeqFile)
  *  luSeqFile = .T.
  *B601946,1 Use the file again to prevent 'File is in use' message
  *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
  *  USE (gcDataDir+lcSeqFile) AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
  *B601946,1 end
  *ELSE
  *  SELECT (lcSeqFile)
  *  luSeqFile = .F.
  *  ltSeqFile = VAL(SYS(21))
  *  leSeqFile = RECNO()
  *  SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
  *ENDIF
  PRIVATE LCNEWSEQFILE
  LCNEWSEQFILE = GFTEMPNAME()
  =GFOPENTABLE(GCDATADIR+LCSEQFILE,LCSEQTAG,'SH',LCNEWSEQFILE)
  *B132218,1 KHM 05/25/2006 [End]


  *B132218,1 KHM 05/25/2006 [Start]
  *SELECT (lcSeqFile)
  SELECT (LCNEWSEQFILE)
  *B132218,1 KHM 05/25/2006 [End]
  LCKEYFIELD = SUBSTR(KEY(),1,AT('+'+LCSEQTYPE,KEY())-1)
  DECLARE LAVLDENT[1]

  *B608122,1  TMI [Start] Define this variable as local so it will not be affected with defining variables with same name in any called function.
  LOCAL LNCOUNT
  *B608122,1  TMI [End  ]

  *TAK E300973,1 Changed to work under visual.
  IF !EMPTY(LCKEYFIELD) .AND. GFGETVLD(LCKEYFIELD,@LAVLDENT) > 0
    FOR LNCOUNT = 1 TO ALEN(LAVLDENT,1)
      *E300888 06/04/98 YMA Search for the generated code prefixed with
      *E300888              the unique site prefix.
      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
      LCKEYEXP = LCEXTRASTR+PADL(LNRETVAL,LNRETLEN-LEN(LCEXTRASTR),"0")
      *B606902,1 Check if next sequence number is valid. [End]

      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)

      *B132218,1 KHM 05/25/2006 [Start]
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
      *B608122,1  TMI [Start] call the global funciton gfSeek instead
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)
      DO WHILE GFSEEK(LAVLDENT[lnCount,2]+LCUNQPREFX+LCKEYEXP,LCNEWSEQFILE)
        *B608122,1  TMI [End  ]

        *B132218,1 KHM 05/25/2006 [End]

        *B603586,1 SSH 29/02/00  (End)
        *E300888 06/04/98 YMA End.

        *B606902,1 Check if next sequence number is valid. [Begin]
        *lnRetVal = lnRetVal + 1
        =GFGETSEQ(LNRETVAL,LCCHRTOUPD)
        *B606902,1 Check if next sequence number is valid. [End]

        *B606902,1 SSE Commented out. [Begin]
        *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
        *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
        *ENDIF
        *B606902,1 SSE Commented out. [End]

        *B606902,1 Get the new key expression. [Begin]
        *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
        *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
        LCKEYEXP = LCEXTRASTR+PADL(LNRETVAL,LNRETLEN-LEN(LCEXTRASTR),"0")
        *B606902,1 Get the new key expression. [end]

      ENDDO
    ENDFOR
  ELSE
    *E300888 06/04/98 YMA Search for the generated code prefixed with
    *E300888              the unique site prefix.
    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

    *B606902,1 Check if next sequence number is valid. [Begin]
    *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
    *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
    LCKEYEXP = LCEXTRASTR+PADL(LNRETVAL,LNRETLEN-LEN(LCEXTRASTR),"0")
    *B606902,1 Check if next sequence number is valid. [End]


    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)

    *B132218,1 KHM 05/25/2006 [Start]
    *B608122,1  TMI [Start] call the global function gfSeek instead with the lcTranType passed
    *DO WHILE SEEK(lcKeyExp,lcSeqFile)
    DO WHILE GFSEEK(LCTRANTYPE+LCKEYEXP,LCNEWSEQFILE)
      *B608122,1  TMI [End  ]
      *B132218,1 KHM 05/25/2006 [End]
      *E300888 06/04/98 YMA End.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lnRetVal = lnRetVal + 1
      =GFGETSEQ(LNRETVAL,LCCHRTOUPD)
      *B606902,1 Check if next sequence number is valid. [End]

      *B606902,1 SSE Commented out. [Begin]
      *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
      *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
      *ENDIF
      *B606902,1 SSE Commented out. [End]

      *B606902,1 Get the new key expression. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
      LCKEYEXP = LCEXTRASTR+PADL(LNRETVAL,LNRETLEN-LEN(LCEXTRASTR),"0")
      *B606902,1 Get the new key expression. [end]

      *B603586,1 SSH 29/02/00  (End)
    ENDDO
  ENDIF

  *B132218,1 KHM 05/25/2006 [Start]
  *IF luSeqFile
  *  USE IN (lcSeqFile)
  *ELSE
  *  SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
  *IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *  IF leSeqFile < 0 OR BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *    GOTO leSeqFile IN (lcSeqFile)
  *  ENDIF
  *ENDIF
  =GFCLOSETABLE(LCNEWSEQFILE)
  *B132218,1 KHM 05/25/2006 [End]
ENDIF
SELECT SEQUENCE
*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*REPLACE nSeq_No WITH lnRetVal+1
REPLACE NSEQ_NO WITH IIF(LNRETVAL + 1 > 999999,0,LNRETVAL + 1)
*B603586,1 SSH 29/02/00 (End)

*B606902,1 Always check if we used Characters before. [Begin]
*REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*lnRetVal = lnRetVal + 1
*lnRetVal = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
LNOLDGENNM = LCEXTRASTR+PADL(LNRETVAL,LNRETLEN-LEN(LCEXTRASTR),"0")
=GFGETSEQ(LNRETVAL,LCCHRTOUPD)
REPLACE NSEQ_NO WITH LNRETVAL , CSEQ_CHR WITH LCCHRTOUPD
*B606902,1 Always check if we used Characters before. [End]

*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*B608122,1  TMI [Start] comment out, no need for this code segment
*IF nSeq_No = 0 .AND. VAL(lnOldGenNm) <> 0
*  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*ENDIF
*B608122,1  TMI [End  ]
IF !EMPTY(LCEXTRASTR)
  LNRETVAL = ALLTRIM(LCEXTRASTR) + PADL(LNRETVAL,LNRETLEN-1,"0")
ENDIF
*B603586,1 SSH 29/02/00 (End)

*--MAN Added RLOCK Condition[Start]
UNLOCK
*--MAN Added RLOCK Condition[End]

*TAK E300973,1 Changed to work under visual.
*=gfAdd_info('SEQUENCE')
*oAriaApplication.AddUserInformation('SEQUENCE')


*E300888 06/04/98 YMA Never return a numeric code, and return the code
*E300888              prefixed with the active site unique prefix code
*E300888              if any.
*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))

*B606902,1 Get the value that will be displayed in message box. [Begin]
*lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
LNRETVAL = LCUNQPREFX + PADL(LNOLDGENNM, LNRETLEN, "0")
*B606902,1 Get the value that will be displayed in message box. [End]

*E300888 06/04/98 YMA End.

IF LUSEQUENCE
  USE IN SEQUENCE
ELSE
  SET ORDER TO TAG LTSEQUENCE IN SEQUENCE
  IF BETWEEN(LESEQUENCE,1,RECCOUNT('Sequence'))
    GOTO LESEQUENCE IN 'Sequence'
  ENDIF
ENDIF
SELECT (LCSAVALIAS)
RETURN(LNRETVAL)
*PADR


*!*************************************************************
FUNCTION NOTEPAD
*E302656 Hassan.I [Start]
*PARAMETERS lcNoteType, lcNoteKey, llTempltOk, lcDataDir
PARAMETERS LCNOTETYPE, LCNOTEKEY, LLTEMPLTOK, LCDATADIR, LOFORMSET
*E302656 Hassan.I [End]
LOCAL ONOTEPAD

IF LLTEMPLTOK
ELSE
  ONOTEPAD = CREATEOBJECT('NotePad')
  *E302656 Hassan.I [Start]
  *RETURN oNotePad.Do(lcNoteType, lcNoteKey)
  RETURN ONOTEPAD.DO(LCNOTETYPE, LCNOTEKEY, LOFORMSET)
  *E302656 Hassan.I [End]
ENDIF



*:---------------------------------------------------------------------
*! Name      : gfwCodePop
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to fill any code array with on of the
*:             following values :
*:               1) A list of the available codes from the codes file.
*:               2) The default code value.
*:               3) "ALL"
*:               4) "N/A"
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : gfIsEdtble()
*:---------------------------------------------------------------------
*: Passed Parameters  : laInfArray : Pointer to the code information array.
*:                      lcField    : The code to be displayed.
*:                      lcFillWith : Fill the array with ...
*:                      "L" : List of the available codes.
*:                      "D" : Default value.
*:                      "A" : "All"
*:                      "N" : "N/A"
*:---------------------------------------------------------------------
*: Example            : = gfwCodePop(@laCodInfo, "CTERMCODE", "A")
*:---------------------------------------------------------------------

FUNCTION GFWCODEPOP
PARAMETERS LAINFARRAY, LCFIELD, LCFILLWITH, LCACTCOMP
LOCAL OCODEPOP

OCODEPOP = CREATEOBJECT('CodePop')
RETURN OCODEPOP.DO(LAINFARRAY, LCFIELD, LCFILLWITH, LCACTCOMP)


*!*************************************************************
*! Name      : gfRltFld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995
*! Purpose   :
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*****************************************************************************
* Modifications:
*!*B603246,1 Fix the bug of the 'Variable lcObjValue not found' error message.
*!*************************************************************
*
FUNCTION GFRLTFLD
PARAMETERS LCCODEVAL,LAARRAYNAM, LCFLDNAME
PRIVATE LATEMPCODES,LCOBJVALUE,LLFILEUSD,LCOLDORDR
*-- Hesham (Start)
*lcSavSelct  = ALIAS()   && Variable to save the currently selected file.

*!*	*B603246,1 (Begin) Open sydfield file with Cfld_name tag .
*!*	*--Is 'sydfield ' file used?
*!*	llFileUsd = .F.
*!*	IF !USED('sydfield')
*!*	  USE oAriaApplication.SysPath+'sydfield' ORDER TAG Cfld_name IN 0
*!*	  llFileUsd = .T.
*!*	ELSE
*!*	  *-- Save old order OF sydfield.
*!*	  lcOldOrdr = ORDER('sydfield')
*!*	  SET ORDER TO TAG Cfld_name IN sydfield
*!*	ENDIF
*!*	*B603246,1 (End)

*!*	IF EMPTY(lcCodeVal)   && Case N/A
*!*	  FOR lnCount  = 1 TO ALEN(laArrayNam,1)
*!*	    *B603246,1 (Begin) Remark the following lines and get the field type from sydfield file.
*!*	    *DO CASE
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Cc'
*!*	    *    lcObjValue = ''
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Nn'
*!*	    *    lcObjValue = 0
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Ll'
*!*	    *    lcObjValue = .F.
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Dd'
*!*	    *    lcObjValue = {}
*!*	    *ENDCASE
*!*	    *--Get field type from sydfield instead of any guess cases like naming convensions errors..
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- Case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- Case Numeric
*!*	      CASE lcObjValue = "N"
*!*	        lcObjValue = 0
*!*	      *-- Case Logical
*!*	      CASE lcObjValue = "L"
*!*	        lcObjValue = .F.
*!*	      *-- Case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    *B603246,1 (End)
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDFOR
*!*	  RETURN
*!*	ENDIF

*!*	*E300631,1 YMA 04/06/97 Select the codes file instead of SYCCodes.
*!*	*SELECT SYCCODES         && Select CODES file
*!*	llUseCodes = .F.
*!*	IF !USED("CODES")
*!*	  USE (oAriaApplication.DataDir+"Codes") IN 0
*!*	  llUseCodes = .T.
*!*	ENDIF
*!*	SELECT CODES         && Select CODES file
*!*	*E300631,1 YMA 04/06/97 End.
*!*	lcSavOrder = SYS(22)    && Save the file order
*!*	SET ORDER TO 0          && To activate rushmore

*!*	DECLARE laTempCodes[1]
*!*	laTempCodes = ' '

*!*	*E300631,1 YMA 04/06/97 Changed the file name to be "Codes" instead of "SYCCodes".
*!*	*E300631,1 YMA 04/06/97 And include the lcFldName in the "where" clause.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM SYCCODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' ;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 Changed to work under visual.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM CODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' + lcFldName;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	  FROM CODES;
*!*	  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
*!*	  AND   CCODE_NO = lcCodeVal ;
*!*	  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 End.
*!*	*E300631,1 YMA 04/06/97 End.

*!*	FOR lnCount  = 1 TO ALEN(laArrayNam,1)
*!*	  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

*!*	  IF lnPosition = 0     && not found
*!*	    *B603246,1 (Begin) Get the field type from sydfield file and if
*!*	    *B603246,1         the related field was not saved in the codes file.
*!*	    *B603246,1         then inisialize it's value.
*!*	    *--Get field type.
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- case Numeric
*!*	      CASE lcObjValue = "N"
*!*	        lcObjValue = 0
*!*	      *-- case Logical
*!*	      CASE lcObjValue = "L"
*!*	        lcObjValue = .F.
*!*	      *-- case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	    *B603246,1  (End)
*!*	  ELSE
*!*	    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
*!*	    DO CASE
*!*	      CASE laTempCodes[lnPosition,2] = 'C'
*!*	        lcObjValue = laTempCodes[lnPosition,3]
*!*	      CASE laTempCodes[lnPosition,2] = 'N'
*!*	        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
*!*	        IF lnDecimPos > 0
*!*	          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
*!*	          SET DECIMALS TO lnDecimPos
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	          SET DECIMALS TO &lcSavDecim
*!*	        ELSE
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	        ENDIF
*!*	      CASE laTempCodes[lnPosition,2] = 'L'
*!*	        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

*!*	      CASE laTempCodes[lnPosition,2] = 'D'
*!*	        lcObjValue = CTOD(laTempCodes[lnPosition,3])
*!*	    ENDCASE

*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDIF
*!*	ENDFOR

*!*	SET ORDER TO &lcSavOrder
*!*	IF llUseCodes
*!*	  USE IN Codes
*!*	ENDIF
*!*	*B603246,1 (Begin) If the sydfield file was not used close it, else restore the old order.
*!*	IF llFileUsd
*!*	  USE IN sydfield
*!*	ELSE
*!*	  SET ORDER TO TAG lcOldOrdr IN sydfield
*!*	ENDIF
*!*	*B603246,1 (End)

*B039255,1 KHM 04/27/2005 Initialize a temporary name for the sydfield file [Begin]
PRIVATE LCTMPFILE
LCTMPFILE   = GFTEMPNAME()
*B039255,1 KHM 04/27/2005 [End]

LNSESSIONID = SET("DATASESSION")
LCSAVSELCT  = ALIAS()  && Variable to save the currently selected file.

* Open sydfield file Remotely.
LCWHERECOND = ""
FOR LNCOUNT = 1 TO ALEN(LAARRAYNAM,1)
  *E303030,1 BEGIN
  *lcWhereCond = lcWhereCond + IIF(lnCount>1," OR ","")+[CFLD_NAME=']+PADR(laArrayNam[lnCount,1],10)+[']
  LCWHERECOND = LCWHERECOND + IIF(LNCOUNT>1," OR ","")+[CFLD_NAME=']+PADR(LAARRAYNAM[lnCount,1],OARIAAPPLICATION.FIELDW)+[']
  *E303030,1 END
NEXT
LCSQLCMD = "Select * from sydfield " + IIF(!EMPTY(LCWHERECOND),"WHERE ","") + LCWHERECOND
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
LCSQLCMD = LCSQLCMD + " AND (CVER = 'A27' OR EMPTY(CVER))"
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*lnRemResult = oAriaApplication.remotesystemdata.execute(lcSqlCmd,'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnSessionID)
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE(LCSQLCMD,'',LCTMPFILE,"",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",LNSESSIONID)
*B039255,1 KHM 04/27/2005 [End]

IF EMPTY(LCCODEVAL)   && Case N/A
  FOR LNCOUNT  = 1 TO ALEN(LAARRAYNAM,1)
    *E303030,1 BEGIN
    *LOCATE FOR cfld_name = PADR(laArrayNam[lnCount,1],10)
    * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[Start]
    *LOCATE FOR cfld_name = PADR(laArrayNam[lnCount,1],oAriaApplication.FieldW)
    LOCATE FOR CFLD_NAME = PADR(LAARRAYNAM[lnCount,1],10)
    * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[END]
    *E303030,1 END

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    LCOBJVALUE = UPPER(IIF(FOUND(),EVALUATE(LCTMPFILE+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- Case char
    CASE LCOBJVALUE = "C"
      * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[Start]
      *lcObjValue = ''
      LCOBJVALUE = SPACE(EVALUATE(LCTMPFILE+'.nfld_wdth'))
      * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[END]
      *-- Case Numeric
    CASE LCOBJVALUE = "N"
      LCOBJVALUE = 0
      *-- Case Logical
    CASE LCOBJVALUE = "L"
      LCOBJVALUE = .F.
      *-- Case date
    CASE LCOBJVALUE = "D"
      LCOBJVALUE = {}
    ENDCASE
    LCFIELDNAM  = LAARRAYNAM[lnCount,2]
    &LCFIELDNAM = LCOBJVALUE
  ENDFOR
  *-- Amin
  SET DATASESSION TO LNSESSIONID
  SELECT IIF(EMPTY(LCSAVSELCT),0,LCSAVSELCT)
  *-- Amin
  RETURN
ENDIF

LLUSECODES = .F.
IF !USED("CODES")
  USE (OARIAAPPLICATION.DATADIR+"Codes") IN 0
  LLUSECODES = .T.
ENDIF
SELECT CODES         && Select CODES file
*E300631,1 YMA 04/06/97 End.
LCSAVORDER = SYS(22)    && Save the file order
SET ORDER TO 0          && To activate rushmore

DECLARE LATEMPCODES[1]
LATEMPCODES = ' '

SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
  FROM CODES;
  WHERE CDEFCODE + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + LCFLDNAME;
  AND   CCODE_NO = LCCODEVAL ;
  INTO ARRAY LATEMPCODES
*TAK E300973,1 End.
*E300631,1 YMA 04/06/97 End.

*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*SELECT sydfieldtmp
SELECT (LCTMPFILE)
*B039255,1 KHM 04/27/2005 [End]

FOR LNCOUNT  = 1 TO ALEN(LAARRAYNAM,1)
  LNPOSITION = ASCAN(LATEMPCODES,LAARRAYNAM[lnCount,1])

  IF LNPOSITION = 0     && not found
    *--Get field type.
    *E303030,1 BEGIN
    *LOCATE FOR CFLD_NAME = PADR(laArrayNam[lnCount,1],10)
    * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[Start]
    *LOCATE FOR CFLD_NAME = PADR(laArrayNam[lnCount,1],oAriaApplication.FieldW)
    LOCATE FOR CFLD_NAME = PADR(LAARRAYNAM[lnCount,1],10)
    * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[END]
    *E303030,1 END

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    LCOBJVALUE = UPPER(IIF(FOUND(),EVALUATE(LCTMPFILE+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- case char
    CASE LCOBJVALUE = "C"
      * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[Start]
      *lcObjValue = ''
      LCOBJVALUE = SPACE(EVALUATE(LCTMPFILE+'.nfld_wdth'))
      * B609933,1 MMT 06/27/2012 Fix bug of error in Payable invoice screen if term code is empty[END]
      *-- case Numeric
    CASE LCOBJVALUE = "N"
      LCOBJVALUE = 0
      *-- case Logical
    CASE LCOBJVALUE = "L"
      LCOBJVALUE = .F.
      *-- case date
    CASE LCOBJVALUE = "D"
      LCOBJVALUE = {}
    ENDCASE
    LCFIELDNAM  = LAARRAYNAM[lnCount,2]
    &LCFIELDNAM = LCOBJVALUE
  ELSE
    LNPOSITION = ASUBSCRIPT(LATEMPCODES,LNPOSITION,1)
    DO CASE
    CASE LATEMPCODES[lnPosition,2] = 'C'
      LCOBJVALUE = LATEMPCODES[lnPosition,3]
    CASE LATEMPCODES[lnPosition,2] = 'N'
      LNDECIMPOS = AT('.',LATEMPCODES[lnPosition,3])
      IF LNDECIMPOS > 0
        LCSAVDECIM = SET('DECIMALS')  && Save old decimals setting
        SET DECIMALS TO LNDECIMPOS
        LCOBJVALUE = VAL(LATEMPCODES[lnPosition,3])
        SET DECIMALS TO &LCSAVDECIM
      ELSE
        LCOBJVALUE = VAL(LATEMPCODES[lnPosition,3])
      ENDIF
    CASE LATEMPCODES[lnPosition,2] = 'L'
      LCOBJVALUE = IIF(ALLTRIM(LATEMPCODES[lnPosition,3]) $ 'YT',.T.,.F.)

    CASE LATEMPCODES[lnPosition,2] = 'D'
      LCOBJVALUE = CTOD(LATEMPCODES[lnPosition,3])
    ENDCASE

    LCFIELDNAM  = LAARRAYNAM[lnCount,2]
    &LCFIELDNAM = LCOBJVALUE
  ENDIF
ENDFOR
SELECT CODES
SET ORDER TO &LCSAVORDER

IF LLUSECODES
  USE IN CODES
ENDIF
*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*!*	IF USED("sydfieldtmp")
*!*	  USE IN sydfieldtmp
*!*	ENDIF

IF USED(LCTMPFILE)
  USE IN (LCTMPFILE)
ENDIF
*B039255,1 KHM 04/27/2005 [End]

SET DATASESSION TO LNSESSIONID
*-- Hesham (End)

SELECT IIF(EMPTY(LCSAVSELCT),0,LCSAVSELCT)
*PADR


*!*************************************************************
*! Name     : gfItemMask
*! Auth     : MAN - Mohamed Abdel Salam
*! Date     : 07/12/97
*! Task ID  : E300705,1
*!*************************************************************
*! Synopsis : function to return the item code mask or the item
*!            code header
*!*************************************************************
FUNCTION GFITEMMASK
*N037782,1 AMH Add new parameter to get the material code structure [Start]
*PARAMETERS lcMaskOrHead, lcDataDr
PARAMETERS LCMASKORHEAD, LCDATADR, LCINVTYPE
*N037782,1 AMH [End]

PRIVATE LCRETURN,LLSTRUCTUSE,LNRECNO,LCSTRUCTORD,LNCURALIAS,LLARRAY,;
  LASEG,LCITEMDIM,LCHEADER,LNSTARTNONM,LNNOSEG,LNPOSISTION

IF TYPE('lcDataDr') # 'C' .OR. EMPTY(LCDATADR)
  LCDATADR = OARIAAPPLICATION.DATADIR
ENDIF

*N037782,1 AMH Suport Old calling [Start]
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
*IF TYPE('lcInvType') # 'C'
IF TYPE('lcInvType') # 'C' OR (TYPE('lcInvType') = 'C' AND EMPTY(LCINVTYPE))
  *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
  LCINVTYPE = '0001'
ENDIF
*N037782,1 AMH [End]

STORE '' TO LCRETURN
LLARRAY = TYPE('lcMaskOrHead[1]') # 'U'
LCITEMDIM = 'I'
IF !LLARRAY
  IF TYPE('lcMaskOrHead')<>'C'

    *N037782,1 AMH Fix bug of Data type missmatch [Start]
    *RETURN .F.
    RETURN LCRETURN
    *N037782,1 AMH [End]

  ENDIF
  LCMASKORHEAD = UPPER(LCMASKORHEAD)
  LCITEMDIM = IIF(LEN(LCMASKORHEAD)>1,RIGHT(LCMASKORHEAD,1),'I')
  LCMASKORHEAD = LEFT(LCMASKORHEAD,1)
ENDIF

LCLOOPEXT = LCITEMDIM
LNCURALIAS = SELECT()

*N037782,1 AMH Get the inventory type [Start]
LOCAL LCRMTINVTYPE,LNCONNECTIONHANDLAR,LCSQLSTATMENT,LNDATASESSION,LCITEMSTRU
LCRMTINVTYPE  = GFTEMPNAME()
LCSQLSTATMENT = "SELECT * FROM INVTYPE (INDEX=CINVTYPE) WHERE CINVTYPE='"+LCINVTYPE+"'"
LNDATASESSION = SET("Datasession")

LNCONNECTIONHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSQLSTATMENT,LCRMTINVTYPE,'INVTYPE',;
  OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'SAVE',LNDATASESSION)

IF LNCONNECTIONHANDLAR # 1
  *RETURN lcReturn
  LCITEMSTRU = 'U'
ELSE
  SELECT (LCRMTINVTYPE)
  LCITEMSTRU = CITEMSTRU
ENDIF
*N037782,1 AMH [End]

LLSTRUCTUSE = .F.
IF !USED('ICISTRU')
  USE (LCDATADR+'ICISTRU') IN 0
  LLSTRUCTUSE = .T.
ELSE
  SELECT ICISTRU
  LCSTRUCTORD = ORDER()
  LNRECNO = RECNO()
ENDIF
SELECT ICISTRU
SET ORDER TO TAG SEGNO

*N037782,1 AMH Get the code structure of inventory type [Start]
*=SEEK('U1')
=SEEK(LCITEMSTRU+'1')
*N037782,1 AMH [End]

*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
DIMENSION LASEG[1,7]
LASEG = ''
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]

LCHEADER = CISEGHEAD
LNNOSEG = 0
LNPOSISTION = 1
*N037782,1 AMH Get the code structure of inventory type [Start]
*SCAN REST WHILE citemrecty+cisegno = 'U'
SCAN REST WHILE CITEMRECTY+CISEGNO = LCITEMSTRU
  *N037782,1 AMH [End]

  IF LCLOOPEXT <> 'N'
    LNNOSEG = LNNOSEG + 1
    DIMEN LASEG[lnNoSeg,7]
    LASEG[lnNoSeg,1] = CISEGTYPE
    LASEG[lnNoSeg,2] = ALLT(CISEGSDES)
    LASEG[lnNoSeg,3] = REPL('X',NISEGSIZE)
    LASEG[lnNoSeg,4] = LNPOSISTION
    LASEG[lnNoSeg,5] = ALLT(CISEGLDES)
    LASEG[lnNoSeg,6] = CISEGSEPR
    LASEG[lnNoSeg,7] = LSEGENDMAJ
    LCRETURN = LCRETURN+REPL('X',NISEGSIZE)+ALLT(CISEGSEPR)
  ENDIF
  LNPOSISTION = LNPOSISTION + NISEGSIZE + LEN(ALLT(CISEGSEPR))
  IF LCLOOPEXT = 'N' AND LSEGENDMAJ
    LCLOOPEXT = 'I'
  ENDIF
  IF LCITEMDIM = 'M' AND LSEGENDMAJ
    EXIT
  ENDIF
ENDSCAN
IF LLARRAY
  DIMEN LCMASKORHEAD[ALEN(laSeg,1),ALEN(laSeg,2)]
  LCRETURN=ACOPY(LASEG,LCMASKORHEAD)
ELSE
  DO CASE
  CASE  LCMASKORHEAD = 'S'
    LCRETURN = LNNOSEG
  CASE  LCMASKORHEAD = 'P' AND  LCITEMDIM='M'
    IF GFITEMMASK('SN') > 0
      LCRETURN = SUBSTR(LCRETURN,1,LEN(LCRETURN)-1)
    ENDIF
  CASE LCMASKORHEAD = 'H' AND LCITEMDIM='M'
    IF GFITEMMASK('SN') > 0
      LCRETURN = SUBSTR(LCHEADER,1,LASEG[lnNoSeg,4]+LEN(LASEG[lnNoSeg,2])-1)
    ELSE
      LCRETURN = LCHEADER
    ENDIF
  CASE LCMASKORHEAD = 'H' AND LCITEMDIM='N'  AND LNNOSEG>0
    LCRETURN = SUBSTR(LCHEADER,LASEG[1,4])
  CASE LCMASKORHEAD = 'H' AND LCITEMDIM='I'
    LCRETURN = LCHEADER
  ENDCASE
ENDIF
IF LLSTRUCTUSE
  USE IN ICISTRU
ELSE
  SELECT ICISTRU
  SET ORDER TO TAG (LCSTRUCTORD)
  IF LNRECNO>0 AND LNRECNO<=RECCOUNT()
    GO LNRECNO
  ENDIF
ENDIF

*N037782,1 AMH Close inventory type [Start]
IF USED(LCRMTINVTYPE)
  USE IN (LCRMTINVTYPE)
ENDIF
*N037782,1 AMH [End]

SELECT (LNCURALIAS)
RETURN LCRETURN

*!**************************************************************************
*!
*!      Function:  gfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
FUNCTION GFGETADR
PARAMETERS LCALIAS, LCTAG, LCKEYCODE, LCADRCODE,LNLINENO,LCADDGRP,LCCURRCODE

*** lcAlias   : source file name
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)
*** lnLineNo  : The Address line number to return

PRIVATE LNSAVINTTG, LNSAVCMPTG, LCCURALIAS, LNOLDTAG,;
  LLOPENINT, LLOPENCMP, LLCONTINUE, LNCOMPREC,LCCURRCODE,LNCOUNTER

LNCOUNTER = 0
* You have to send the source file and 1 or more from the following parameters
* 1 - The alias name for the source file or you have it the currently selected
* 2 - Address code to be used in getting the address line  OR
* 3 - Tag name and Seek Expression to get the  Address code
* 4 - You can have the source file opened with the proper tag and just send
*     the seek expr. (In case of not sending Tag ID there must be an active one)

IF EMPTY(LCALIAS) .OR. TYPE('lcTag') <> 'C'
  IF EMPTY(ALIAS())
    RETURN .F.
  ELSE
    LCALIAS = ALIAS()
  ENDIF
ENDIF
IF EMPTY(LCADDGRP) OR TYPE('lcAddGrp') <> 'C'
  LCADDGRP  = ''
  LCGRPCODE = 'E'
ELSE
  LCADDGRP  = ALLTRIM(LCADDGRP)
  LCGRPCODE = LCADDGRP
ENDIF
LCCURALIAS = ALIAS()
SELECT (LCALIAS)
LNOLDTAG = VAL(SYS(21))

*-- No Address code has been sent
IF EMPTY(LCADRCODE)
  IF !EMPTY(LCKEYCODE) .AND. TYPE('lcKeyCode') <> 'C'
    DO CASE

      *-- A Search Expr has been sent and no Tag Has been Sent and no active tag
    CASE (EMPTY(LCTAG) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
      SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)
      RETURN .F.

      *-- A Search Expr and a Tag ID have been sent
    CASE !EMPTY(LCTAG)
      *lnOldTag = VAL(SYS(21))
      SET ORDER TO TAG (LCTAG)
      *-- The Search expr is not found
      IF !SEEK(LCKEYCODE)
        SET ORDER TO LNOLDTAG
        SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)
        RETURN .F.
      ENDIF

      *-- A search expr has been sent without a Tag
    OTHERWISE
      *-- There is no active tag
      IF EMPTY(SYS(22))
        SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)
        RETURN .F.
      ENDIF
      *-- The Search Expr. is not found
      IF !SEEK(LCKEYCODE)
        SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)
        RETURN .F.
      ENDIF
    ENDCASE
  ENDIF

  *-- Just to be able to set the old tag even if it has not been
  *-- changed in the above DO CASE
  *lnOldTag = VAL(SYS(21))

  LCADRCODE = CCONT_COD&LCGRPCODE
ENDIF

DECLARE LAADDRESS[6,3]
LAADDRESS = " "
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
LLRETARRAY = (TYPE("lnLineNo[1]") = "C")
IF LLRETARRAY
  LNLINENO = ""
ELSE
  LNLINENO  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(LNLINENO,1,5),INT(LNLINENO),1)
ENDIF
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly

*!*	STORE .F. TO llOpenInt, llOpenCmp
*!*	*** Check being on a correct alias
*!*
*!*	IF !USED('SYCINT')  && Check if the internationals file is open or not.
*!*	  llOpenInt  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaApplication.SysPath + 'SYCINT') ORDER TAG cContCode IN 0
*!*	ELSE
*!*	  SELECT SYCINT
*!*	  lnSavIntTg = VAL(SYS(21))
*!*	  SET ORDER TO TAG cContCode   && Change the order
*!*	ENDIF

*!*	IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
*!*	  llOpenCmp  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaApplication.SysPath + 'SYCCOMP') ORDER TAG cComp_ID IN 0
*!*	ELSE
*!*	  SELECT SYCCOMP
*!*	  lnSavCmpTg = VAL(SYS(21))
*!*	  lnCompRec  = RECNO()
*!*	  SET ORDER TO TAG cComp_ID   && Change the order
*!*	ENDIF

*!*	IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP') ;
*!*	   .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
*!*	  laAddress[1,1] = SYCINT.nPart1Ord
*!*	  laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
*!*	  laAddress[1,3] = SYCINT.nPart1LEN
*!*	  laAddress[2,1] = SYCINT.nPart2Ord
*!*	  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp)
*!*	  laAddress[2,3] = SYCINT.nPart2LEN
*!*	  laAddress[3,1] = SYCINT.nPart3Ord
*!*	  laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
*!*	  laAddress[3,3] = SYCINT.nPart3LEN
*!*	  laAddress[4,1] = SYCINT.nPart4Ord
*!*	  laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
*!*	  laAddress[4,3] = SYCINT.nPart4LEN
*!*	  laAddress[5,1] = SYCINT.nPart5Ord
*!*	  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
*!*	  laAddress[5,3] = SYCINT.nPart5LEN
*!*	  laAddress[6,1] = SYCINT.nPart6Ord
*!*	  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
*!*	  laAddress[6,3] = SYCINT.nPart6LEN
*!*	  IF TYPE("lcCurrCode") = 'C'
*!*	    &lcCurrCode = SYCINT.cCurrCode
*!*	    *SHOW GET (lcCurrCode)
*!*	  ENDIF
*!*	  =ASORT(laAddress,1)
*!*	  lcRetVal=''
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	  *  IF laAddress[lnCount,1] = lnLineNo
*!*	  *    *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
*!*	  *    lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	  *    lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
*!*	  *   ENDIF
*!*	  *ENDFOR
*!*	  *-- if it is array
*!*	  IF llRetArray 
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	        IF laAddress[lnCount,1] = lnArrLen
*!*	          lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	          lnLineNo[lnArrLen] = lnLineNo[lnArrLen]+;
*!*	                               IIF(EMPTY(lnLineNo[lnArrLen]) .OR. ;
*!*	                               RIGHT(lnLineNo[lnArrLen],1) = ',' ,'',', ') + lcAddPart
*!*	         ENDIF
*!*	      ENDFOR
*!*	    ENDFOR
*!*	  ELSE  && else numeric value.

*!*	    FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	      IF laAddress[lnCount,1] = lnLineNo
*!*	        lnCounter = lnCounter + 1
*!*	        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	        lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',;
*!*	                             IIF(ALLTRIM(gcContCode) = "USA" AND lnCounter > 2 ,'  ' ,', ')) + lcAddPart
*!*	       ENDIF
*!*	    ENDFOR
*!*	  ENDIF
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*
*!*	ELSE
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  *-- if it is an array
*!*	  IF llRetArray
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      lnLineNo[lnArrLen]= EVAL(lcAlias+'.cAddress'+STR(lnArrLen,1)+lcAddGrp)
*!*	    ENDFOR
*!*	  ELSE  && else it is numeric value.
*!*	    lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  ENDIF
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*	ENDIF

*!*	IF USED('SYCCOMP')
*!*	  IF llOpenCmp
*!*	    USE IN SYCCOMP
*!*	  ELSE
*!*	    SET ORDER TO lnSavCmpTg IN SYCCOMP
*!*	    IF BETWEEN(lnCompRec,1,RECCOUNT("SYCCOMP"))
*!*	      GOTO lnCompRec IN SYCCOMP
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*
*!*	IF USED('SYCINT')
*!*	  IF llOpenInt
*!*	    USE IN SYCINT
*!*	  ELSE
*!*	    SET ORDER TO lnSavIntTg IN SYCINT
*!*	  ENDIF
*!*	ENDIF
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCINT where ccont_code='"+LCADRCODE+"'",'',"SYCINTTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
*B611194,1 MMT 10/09/2016 Error while previewing AR Aging report[T20160822.0163][Start]
IF LNREMRESULT<1
  IF USED("SYCINTTMP")
    USE IN SYCINTTMP
  ENDIF
  IF USED('SYCINT')
    USE IN SYCINT 
  ENDIF
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCINT where ccont_code='"+LCADRCODE+"'",'',"SYCINTTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
ENDIF
*B611194,1 MMT 10/09/2016 Error while previewing AR Aging report[T20160822.0163][End]
IF LNREMRESULT=1
  LOCATE
   *! B611748,1 Es 3/20/2019 The Sales Order screen does not show the customer billing address if customer.ccont_cod2 is empty in customer table. [Start]
 * IF !FOUND()
   IF !FOUND() Or Empty(LCADRCODE)
   *! B611748,1 Es 3/20/2019 The Sales Order screen does not show the customer billing address if customer.ccont_cod2 is empty in customer table. [End]

    LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCINT where ccont_code='"+OARIAAPPLICATION.DEFAULTCOUNTRY+"'",'',"SYCINTTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
    *N000300,1 HBG 05/17/2004 Locate to fix bug of EOF()
    LOCATE
    *N000300,1 [End]
  ENDIF
  IF LNREMRESULT>=1 AND FOUND()
    LAADDRESS[1,1] = SYCINTTMP.NPART1ORD
    LAADDRESS[1,2] = EVAL(LCALIAS+'.cAddress1'+LCADDGRP)
    LAADDRESS[1,3] = SYCINTTMP.NPART1LEN
    LAADDRESS[2,1] = SYCINTTMP.NPART2ORD
    LAADDRESS[2,2] = EVAL(LCALIAS+'.cAddress2'+LCADDGRP)
    LAADDRESS[2,3] = SYCINTTMP.NPART2LEN
    LAADDRESS[3,1] = SYCINTTMP.NPART3ORD
    LAADDRESS[3,2] = EVAL(LCALIAS+'.cAddress3'+LCADDGRP)
    LAADDRESS[3,3] = SYCINTTMP.NPART3LEN
    LAADDRESS[4,1] = SYCINTTMP.NPART4ORD
    LAADDRESS[4,2] = EVAL(LCALIAS+'.cAddress4'+LCADDGRP)
    LAADDRESS[4,3] = SYCINTTMP.NPART4LEN
    LAADDRESS[5,1] = SYCINTTMP.NPART5ORD
    LAADDRESS[5,2] = EVAL(LCALIAS+'.cAddress5'+LCADDGRP)
    LAADDRESS[5,3] = SYCINTTMP.NPART5LEN
    LAADDRESS[6,1] = SYCINTTMP.NPART6ORD
    LAADDRESS[6,2] = EVAL(LCALIAS+'.cAddress6'+LCADDGRP)
    LAADDRESS[6,3] = SYCINTTMP.NPART6LEN
    IF TYPE("lcCurrCode") = 'C'
      &LCCURRCODE = SYCINTTMP.CCURRCODE
    ENDIF
    =ASORT(LAADDRESS,1)
    LCRETVAL=''
    *-- if it is array
    IF LLRETARRAY
      PRIVATE LNARRLEN
      LNARRLEN = 0
      FOR LNARRLEN = 1 TO ALEN(LNLINENO,1)
        FOR LNCOUNT = 1 TO ALEN(LAADDRESS,1)
          IF LAADDRESS[lnCount,1] = LNARRLEN
            LCADDPART = ALLTRIM(SUBSTR(LAADDRESS[lnCount,2],1,LAADDRESS[lnCount,3]))
            LNLINENO[lnArrLen] = LNLINENO[lnArrLen]+;
              IIF(EMPTY(LNLINENO[lnArrLen]) .OR. ;
              RIGHT(LNLINENO[lnArrLen],1) = ',' ,'',', ') + LCADDPART
          ENDIF
        ENDFOR
      ENDFOR
    ELSE  && else numeric value.

      FOR LNCOUNT = 1 TO ALEN(LAADDRESS,1)
        IF LAADDRESS[lnCount,1] = LNLINENO
          LNCOUNTER = LNCOUNTER + 1
          LCADDPART = ALLTRIM(SUBSTR(LAADDRESS[lnCount,2],1,LAADDRESS[lnCount,3]))
          LCRETVAL  = LCRETVAL+IIF(EMPTY(LCRETVAL) .OR. RIGHT(LCRETVAL,1) = ',' ,'',;
            IIF(ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY) = "USA" AND LNCOUNTER > 2 ,'  ' ,', ')) + LCADDPART
        ENDIF
      ENDFOR
    ENDIF
    *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]

  ELSE
    *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
    *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
    *-- if it is an array
    IF LLRETARRAY
      PRIVATE LNARRLEN
      LNARRLEN = 0
      FOR LNARRLEN = 1 TO ALEN(LNLINENO,1)
        LNLINENO[lnArrLen]= EVAL(LCALIAS+'.cAddress'+STR(LNARRLEN,1)+LCADDGRP)
      ENDFOR
    ELSE  && else it is numeric value.
      LCRETVAL= EVAL(LCALIAS+'.cAddress'+STR(LNLINENO,1)+LCADDGRP)
    ENDIF
    *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
  ENDIF
ENDIF

IF USED("SYCINTTMP")
  USE IN SYCINTTMP
ENDIF
*-- Hesham (End)
SET ORDER TO LNOLDTAG IN (LCALIAS)
SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)

RETURN LCRETVAL

*!*************************************************************
*! Name      : gfGetVld
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995
*! Purpose   :
*!*************************************************************
*! Calls     :
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION GFGETVLD
PARAMETERS LCFIELDNAM, LAARRNAME,LLADDALL
PRIVATE LCCURRFILE,LCOLDTAG,LCSETEXCT,LLSYDFIELD,LNMAXLEN

*** Save current environment
LCCURRFILE    = SELECT()
LCSETEXACT    = SET('EXACT')
LLSYDFIELD    = .F.
LNMAXLEN      = 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

*-- Hesham (Start)
*!*	*** Check if fields system file is opened, if it is
*!*	*** set the appropriate tag, if not open use it with
*!*	*** the appropriate tag.
*!*	*** Tag to be used is 'cFld_Name'
*!*	*** Its expression is
*!*	IF !USED('SYDFIELD')
*!*	  SELECT 0
*!*	  USE (oAriaApplication.SysPath+"SYDFIELD") ORDER TAG cFld_Name
*!*	  llSydField       = .T.
*!*	ELSE
*!*	  SELECT SYDFIELD
*!*	  lcOldTag         = SYS(22)
*!*	  SET ORDER TO TAG cFld_Name
*!*	ENDIF

*!*	*** Search for field name
*!*	IF SEEK(UPPER(lcFieldNam), 'SYDFIELD')
*!*	  PRIVATE oSubString
*!*	  oSubString = CREATEObject("SubString", This.oForm)
*!*	  oSubString.Do(SYDFIELD.mVEntries, @laArrName, "|~")
*!*	  RELEASE oSubString
*!*	  lnMaxLen = LEN(laArrName[1,1])
*!*	  FOR lnCount = 2 TO ALEN(laArrName,1)
*!*	    lnMaxLen = MAX(lnMaxLen,LEN(laArrName[lnCount,1]))
*!*	  ENDFOR
*!*	ELSE
*!*	  laArrName = " "
*!*	ENDIF

*!*	IF llAddAll
*!*	  *** Add one element on top to be used for 'All' option.
*!*	  *** Assign a value to this element later.
*!*	  IF !EMPTY(laArrName[1,1])
*!*	    DIMENSION laArrName[ALEN(laArrName,1)+1,2]
*!*	    =AINS(laArrName, 1, 1)
*!*	  ENDIF
*!*	  laArrName[1,1] = "All"
*!*	  laArrName[1,2] = " "
*!*	ENDIF

*!*	*** Restore environment
*!*	IF llSydField .AND. USED('SYDFIELD')
*!*	  USE IN 'SYDFIELD'
*!*	ELSE
*!*	  IF !EMPTY(lcOldTag)
*!*	    SET ORDER TO TAG (lcOldTag)
*!*	  ENDIF
*!*	ENDIF
*-- select the field information from sydfield remotely
LNDATAS = SET("DATASESSION")
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where cFld_Name='"+UPPER(lcFieldNam)+"'",'',"SYDFIELDTMP","",oAriaApplication.SystemConnectionString,3,"",lnDataS)
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFIELD where cFld_Name='"+UPPER(LCFIELDNAM)+;
  "' AND (CVER='A27' OR EMPTY(CVER))",'',"SYDFIELDTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",LNDATAS)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]
*-- if the connection succeded
IF LNREMRESULT=1
  LOCATE
  ** Check if the field was found then get the field name
  IF FOUND()
    * 	PRIVATE oSubString
    * oSubString = CREATEObject("SubString", This.oForm)
    *  	oSubString.Do(SYDFIELD.mVEntries, @laArrName, "|~")
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID ="EN"
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
      =GFSUBSTR(SYDFIELDTMP.MVENTRIES, @LAARRNAME, "|~")
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    ELSE
      LCVALIDENT = OARIAAPPLICATION.GETA27FLDVENTRY (ALLTRIM(LCFIELDNAM))&&,SET("Datasession"),ALIAS())
      LCVALIDENT = IIF(EMPTY(LCVALIDENT),SYDFIELDTMP.MVENTRIES,LCVALIDENT)
      =GFSUBSTR(LCVALIDENT, @LAARRNAME, "|~")
    ENDIF
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    *	  RELEASE oSubString
    LNMAXLEN = LEN(LAARRNAME[1,1])
    FOR LNCOUNT = 2 TO ALEN(LAARRNAME,1)
      LNMAXLEN = MAX(LNMAXLEN,LEN(LAARRNAME[lnCount,1]))
    ENDFOR
  ELSE
    LAARRNAME = " "
  ENDIF
ENDIF
IF LLADDALL
  *** Add one element on top to be used for 'All' option.
  *** Assign a value to this element later.
  IF !EMPTY(LAARRNAME[1,1])
    DIMENSION LAARRNAME[ALEN(laArrName,1)+1,2]
    =AINS(LAARRNAME, 1, 1)
  ENDIF
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *laArrName[1,1] = "All"
  LAARRNAME[1,1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_VLADFLD_ALL,OARIAAPPLICATION.GETHEADERTEXT("LANG_VLADFLD_ALL",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[End]
  LAARRNAME[1,2] = " "
ENDIF

*** Restore environment
** if the connection cursor was created then close it
IF USED('SYDFIELDTMP')
  USE IN 'SYDFIELDTMP'
ENDIF
SET DATASESSION TO (LNDATAS)
*-- Hesham (End)
SELECT (LCCURRFILE)
SET EXACT &LCSETEXACT
RETURN LNMAXLEN
*PADR

*!*************************************************************
*! Name      : gfStyPrice
*! Developer : Renee Ezzat
*! Date      : 12/20/95
*! Purpose   : Return the style/color price in a given currency.
*!*************************************************************
*! Parameters: lcStyle    : Style
*!             lcColor    : color
*!             lcStyCur   : Style currency code
*!             lcPriceLvl : price level ('A', 'B', or 'C')
*!             llNoAdd    : .T. if adding prices on the fly
*!                          is not to be allowed despite the company
*!                          setup.
*!*************************************************************
*! Calls     : gfGetMemVar
*!             STYPRICE.SPR
*!*************************************************************
*! Returns                : Price level in passed currncy for
*!                          the passed style/color, or
*!                          -1 if it is not found and/or not
*!                          added.
*!*************************************************************
*! Example   : lnPrice = gfStyPrice(lcStyle, lcColor, lcCurrCode, 'A')
*!*************************************************************
*E300328,1 RENEE 12/21/95.
*E300620,4 Reham On 08/05/97
*E300620,4 Remove color from the function.
*B602290,1 HSS 11/29/98 Get the price from STYLE file in the case of base
*B602290,1              currency.
*:*************************************************************************
FUNCTION GFSTYPRICE
*B610656,1 TMI 01/14/2014 15:59 [Start] add a new parameter called lReadOnly
*PARAMETERS LCSTYLE, LCPRICELVL, LCSTYCUR, LLNOADD
PARAMETERS LCSTYLE, LCPRICELVL, LCSTYCUR, LLNOADD,lReadOnly
*B610656,1 TMI 01/14/2014 15:59 [End  ] 
PRIVATE LCFILTER, LNOLDVAL, LNCURALIAS, LNCURTAG, LNCURREC, LLOPENFILE, LNRETPRICE
SET STEP ON 
IF !GFGETMEMVAR('LLMULCURR',OARIAAPPLICATION.ACTIVECOMPANYID)
  *-- Change this line to use oGetMemVar [End]
  RETURN -1
ENDIF

IF EMPTY(LCSTYLE) .OR. EMPTY(LCPRICELVL) .OR. ATC(LCPRICELVL, 'ABC') = 0
  RETURN -1
ENDIF

*E300328,1 If there is no currency code parameter, default with
*E300328,1 the base currency.
IF EMPTY(LCSTYCUR)
  *-- Change this line to use oAriaApplication.BaseCurrency
  *lcStyCur = gcBaseCurr
  LCSTYCUR = OARIAAPPLICATION.BASECURRENCY
ENDIF

*E300328,1 Check if STYPRICE is open,
*E300328,1 If the file is used, store current environment
LNCURALIAS = SELECT()
LNOLDVAL   = 0
IF USED('STYPRICE')
  LLOPENFILE = .F.
  SELECT STYPRICE
  *E300328,1 Get current tag
  LNCURTAG   = VAL(SYS(21))
  *E300328,1 Get current record
  LNCURREC   = IIF(!EOF(), RECNO(), 0)
  *E300328,1 Get current filter
  LCFILTER   = FILTER()
  SET FILTER TO
  SET ORDER TO TAG STYPRICE
ELSE
  LLOPENFILE = .T.
  SELECT 0
  USE (OARIAAPPLICATION.DATADIR + 'STYPRICE') ORDER TAG STYPRICE
  LNCURALIAS = 0
ENDIF

*E300328,1 Adjust Customer level parameter.
LCPRICELVL = LEFT(ALLTRIM(LCPRICELVL), 1)
IF ALLTRIM(LCSTYCUR) == ALLTRIM(OARIAAPPLICATION.BASECURRENCY)
  PRIVATE LLCLOSESTY , LCSTYORDER , LNSTYRECNO

  IF !USED('STYLE')
    LLCLOSESTY = .T.
    =GFOPENFILE(OARIAAPPLICATION.DATADIR+ 'STYLE' , OARIAAPPLICATION.DATADIR+ 'STYLE','SH')
    SELECT STYLE
  ELSE
    LLCLOSESTY = .F.
    SELECT STYLE
    LNSTYRECNO = IIF(EOF() , 0 , RECNO())
    LCSTYORDER = ORDER()
    SET ORDER TO TAG STYLE
  ENDIF

  LCSTYLE = PADR(LCSTYLE , LEN(STYLE))
  IF SEEK(LCSTYLE)
    *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
    *lnRETPRICE = PRICE&LCPRICELVL
    lnRETPRICE = IIF(LCPRICELVL='R', nsugretpri ,  PRICE&LCPRICELVL)
    *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
  ELSE
    LNRETPRICE = -1
  ENDIF

  IF LLCLOSESTY
    USE IN STYLE
  ELSE
    SELECT STYLE
    SET ORDER TO (LCSTYORDER) IN STYLE
    IF LNSTYRECNO <> 0
      GO LNSTYRECNO IN STYLE
    ELSE
      GO BOTTOM IN STYLE
      SKIP IN STYLE
    ENDIF
  ENDIF
ELSE    && If not base currency
  *B602290,1 Add these lines to get the style price from the STYLE file [End]

  IF SEEK(LCSTYLE + LCSTYCUR, 'STYPRICE')
    
    *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
    *LNRETPRICE = STYPRICE.PRICE&LCPRICELVL
    LNRETPRICE = IIF(LCPRICELVL='R',STYPRICE.nsugretpri ,STYPRICE.PRICE&LCPRICELVL)
    *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
  ELSE
    LNRETPRICE = -1
    IF !LLNOADD .AND. GFGETMEMVAR('LLSTYPRICE',OARIAAPPLICATION.ACTIVECOMPANYID)
      STORE 0 TO LNPRICEA, LNPRICEB, LNPRICEC
      *B610656,1 TMI 01/14/2014 16:02 [Start] do not call the styprice screen when lReadOnly is .T.
      IF lReadOnly
        lnRetPrice = 0
      ELSE 
        *B610656,1 TMI 01/14/2014 16:02 [End  ] 
        DO FORM (OARIAAPPLICATION.SCREENHOME + 'SY\STYPRICE');
          WITH LCSTYLE , LCSTYCUR , LCPRICELVL TO LNRETPRICE
        *B610656,1 TMI 01/14/2014 16:03 [Start] 
      ENDIF 
      *B610656,1 TMI 01/14/2014 16:03 [End  ] 
    ELSE
      LOCAL LCSTYLETIT
      LCSTYLETIT = ALLTRIM(GFITEMMASK("HI"))
      =OARIAAPPLICATION.MESSAGEBOX("TRM00344B00000" , "DIALOG" ,;
        LCSTYLETIT + " " + ALLTRIM(LCSTYLE) +;
        "|" + ALLTRIM(LCSTYCUR))
    ENDIF
  ENDIF
ENDIF    && End of IF ALLTRIM(lcStyCur) == ALLTRIM(gcBaseCurr)

*E300328,1 Restore environment
IF !LLOPENFILE
  SELECT STYPRICE
  SET ORDER TO (LNCURTAG)
  IF !EMPTY(LCFILTER)
    SET FILTER TO (LCFILTER)
  ENDIF
  IF LNCURREC > 0
    GO LNCURREC
  ENDIF
ENDIF
SELECT (LNCURALIAS)
*E300328,1 Return price
RETURN LNRETPRICE


*:---------------------------------------------------------------------
*! Name      : gfIsEdtble
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to tell if a specific code is editable by the
*!             user or not.
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : None
*:---------------------------------------------------------------------
*: Passed Parameters  : lcField  -> The code to be checked.
*:                      lnFieldW -> Pointer to a numeric variable to
*:                                  hold the field width.
*:---------------------------------------------------------------------
*: Example            : = gfIsEdtble("TERMS", @lnWidth)
*:---------------------------------------------------------------------
FUNCTION GFISEDTBLE
PARAMETERS LCPFIELD, LNFIELDW, LCACTVCOMP
LOCAL OISEDTBLE

*B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File [Start]
*!*	oIsEdtble = CREATEOBJECT('IsEdtble')
*!*	RETURN oIsEdtble.Do(lcPField, lnFieldW, lcActvComp)
PRIVATE LLRETVAL, LCDATADIR
GCACT_COMP = OARIAAPPLICATION.ACTIVECOMPANYID
GCDATADIR  = OARIAAPPLICATION.DATADIR
GCSYSHOME  = OARIAAPPLICATION.SYSPATH

LOCAL LNCURALIAS
LNCURALIAS = SELECT(0)

LCACTVCOMP = IIF(TYPE("lcActvComp")#"C", GCACT_COMP, LCACTVCOMP)
LCDATADIR  = GCDATADIR

IF !(LCACTVCOMP == GCACT_COMP)
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syccomp where cComp_ID='"+LCACTVCOMP+"'",'',"CompFile","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
  IF LNREMRESULT>=1
    LOCATE
  ENDIF
  IF LNREMRESULT>=1 AND FOUND()
    LCDATADIR = GFGETDATADIR(ALLTRIM(COMPFILE.CCOM_DDIR))
  ENDIF
  USE IN COMPFILE
ENDIF
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where CFLD_NAME='"+lcPField+"'",'',"FieldFile","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFIELD where CFLD_NAME='"+LCPFIELD+"' AND (CVER='A27' OR EMPTY(CVER))",'',"FieldFile","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
LOCAL LNFIELDWIDTH
IF LNREMRESULT>=1
  LOCATE
ENDIF
LLRETVAL = IIF(LNREMRESULT>=1 AND FOUND(),;
  ("EDITABLE" $ UPPER(ALLTRIM(FIELDFILE.MCODEINFO))), .F.)
LNFIELDWIDTH = IIF(LNREMRESULT>=1 AND FOUND(),FIELDFILE.NFLD_WDTH, 6)
USE (LCDATADIR+"CODESET" ) IN 0 ORDER FILDNAME  AGAIN ALIAS CODESETF
LNFIELDW = IIF(SEEK(LCPFIELD, "CodeSetF" ), CODESETF.NFLD_WDTH,LNFIELDWIDTH)
USE IN FIELDFILE
USE IN CODESETF
SELECT(LNCURALIAS)


RETURN LLRETVAL
*B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File [End]

*!*************************************************************
*! Name      : gfAddCurSm
*! Developer : Haytham El-Sheltawi
*! Date      : 11/29/1998
*! Purpose   : Function to add the currency symbol to a numeric value
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) Currency code.
*!                     2) Amount.
*!                     3) Number of decimal places.
*!*************************************************************
*! Example	      	 : .AddCurrencySymbol('USDLR' , 100.97 , 2)
*!*************************************************************
FUNCTION GFADDCURSM
PARAMETERS LCCURRCODE, LNAMOUNT, LNDECIMALS
LOCAL OADDCURRENCYSYMBOL

OADDCURRENCYSYMBOL = CREATEOBJECT('AddCurrencySymbol')
RETURN OADDCURRENCYSYMBOL.DO(LCCURRCODE, LNAMOUNT, LNDECIMALS)

*!********************************************************************
*! Name      : gfGetExSin
*! Developer : Mohamed Hassan
*! Date      : 11/27/95
*! Purpose   : Return Exchange Rate sign
*!********************************************************************
*! Parameters: lcCurrency  && Currency to define or return exh. rate for
*!             lcBaseCurr  && Variable to define base currency.
*!             lcUntSin    && Pointer to unit sign character.
*!********************************************************************
*! Call      :
*!********************************************************************
*! Returns   : * OR /
*!********************************************************************
*! Example   : lcExSign = gfGetExSin(@lcUntSin,'ENG')
*!             The user can pass the currency as a parametter
*!             or the function is going to use the base currency.
*!             The function is going to return the exchnage rate
*!             sign.
*!********************************************************************
*
FUNCTION GFGETEXSIN
PARAMETERS LCUNTSIN, LCCURRENCY, LCBASECURR
LOCAL LCRETURN , LLCLOSE

IF TYPE('lcUntSin') = 'C'
  LCUNTSIN = '/'
ENDIF
IF TYPE('lcBaseCurr') $ 'UL'
  LCBASECURR = OARIAAPPLICATION.BASECURRENCY
ENDIF
IF LCCURRENCY = LCBASECURR
  RETURN '*'
ENDIF

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
** This file should always be in use if the system is multi currency,
** Sometimes it is closed by Apparel programs, so, if it is
** not used,open the file.
LLCLOSE = .F.
IF !USED('SYCCURR')
  LLCLOSE = .T.
  USE (OARIAAPPLICATION.SYSPATH+"SYCCURR") IN 0 ORDER TAG CCURRCODE
ENDIF
=SEEK(LCBASECURR,'SYCCURR','CCURRCODE')
LCRETURN = IIF((SYCCURR.CCURMETH = 'M' .AND.;
  !(ALLTRIM(LCCURRENCY) $ SYCCURR.MCUREXCEPT)) .OR.;
  SYCCURR.CCURMETH = 'D' .AND.;
  (ALLTRIM(LCCURRENCY) $ SYCCURR.MCUREXCEPT) , '*' , '/')
*!*	IF llClose
*!*	  USE IN SYCCURR
*!*	ENDIF

*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCURR where ccurrcode='"+lcBaseCurr+"'",'',"SYCCURRTMP","",oAriaApplication.SystemConnectionString,3)
*!*	IF lnRemResult=1
*!*	  LOCATE
*!*		lcReturn = IIF((SYCCURRTMP.cCurMeth = 'M' .AND.;
*!*	               !(ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept)) .OR.;
*!*	               SYCCURRTMP.cCurMeth = 'D' .AND.;
*!*	               (ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept) , '*' , '/')
*!*	  USE IN SYCCURRTMP
*!*	ENDIF

RETURN LCRETURN

*!********************************************************************
*--Get Currency Symbel.
FUNCTION GFGETCURSMBL
PARAMETERS LCCURRENCY
*-- Hesham (Start)
*-- Open currency table remotely and get the currency symbol
LOCAL LCFRNSMBL,LNALIAS,LLOPENED
LLOPENED = .F.
LNALIAS = SELECT()
IF !USED('SYCCURR')
  USE (OARIAAPPLICATION.SYSPATH+"SYCCURR") IN 0 ORDER TAG CCURRCODE
  LLOPENED = .T.
ENDIF
SET ORDER TO TAG CCURRCODE IN SYCCURR
LCFRNSMBL = IIF(SEEK(LCCURRENCY,'SycCurr'),SYCCURR.CCURRSMBL,"")
IF LLOPENED
  USE IN SYCCURR
ENDIF
*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
LLCLOSEFIL = .F.
IF !USED('SYCCURR')
  LLCLOSEFIL = .T.
  USE (OARIAAPPLICATION.SYSPATH+'SYCCURR') IN 0 ORDER TAG CCURRCODE
ENDIF

*!*	LOCAL lcFrnSmbl,lnAlias

*!*	lnAlias = SELECT()

*!*	lcFrnSmbl = ""
*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCURR where ccurrcode='"+lcCurrency+"'",'',"SYCCURRTMP","",oAriaApplication.SystemConnectionString,3)
*!*	IF lnRemResult=1
*!*	  LOCATE
*!*	  lcFrnSmbl = IIF(FOUND(),SycCurrTMP.cCurrSmbl,"")
*!*		USE IN SYCCURRTMP
*!*	ENDIF
*-- Hesham (End)
SELECT(LNALIAS)
RETURN LCFRNSMBL

*!*************************************************************
*! Name      : gfChkRate
*! Developer : Hesham El-Sheltawi
*! Date      : 10/09/95
*! Purpose   : Return Exchange Rate for Currency in spec. date
*!*************************************************************
*! Parameters: lcExUnit    && hold variable name to return Currency Units
*!             lcCurrency  && Currency to define or return exh. rate for
*!             ldDate      && Date to define or return exch. rate for
*!             llDispMsg   && Display message or not
*!             lcCompID    && company id to use its settings
*!             lcBaseCurr  && The currency that you want to use as default.
*E300309,1     llNoErrMsg  && .T. if the default error message is
*E300309,1                     not to be displayed, .F. otherwise.
*!*************************************************************
*! Call      : gfGetMemVar
*!*************************************************************
*! Returns            : VALUE OF exchage rate
*!*************************************************************
*! Example   : lcVarName=gfChkRate("lcEngUnit",'ENG',DATE(),.T.)
*!             WILL return from the sycexch file the exchange rate
*!             value for the currency "ENG" at the system date
*!             and its units in variable called lcEngUnit
*!*************************************************************
FUNCTION GFCHKRATE
*E300309,1 RENEE 11/15/95. Add a parameter to control the display
*E300309,1                 of the message that is displayed if a
*E300309,1                 valid exchange rate is not found.
*E300309,1                 parameter : llNoErrMsg
*
*E300336,1 RENEE 01/08/96. Enhance performance as concerning to speed
*E300336,1                 of execution.
*E300309,1 Add parameter llNoErrMsg
*PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr
PARAMETERS LCEXUNIT,LCCURRENCY,LDDATE,LLDISPMSG,LCCOMPID,LCBASECURR, LLNOERRMSG
*E300309,1 end.

LOCAL LNCURDATASESS

PRIVATE LLEXUSEDBY,LCOLDALIAS,LCOLDTAG,LNRETRATE,LDCURRDAY,LCOLDFLT,;
  LLCURUSEDBY, LNEXRATE
LNCURDATASESS = SET("Datasession")
LNRETRATE  = 0
LCCOMPID   = IIF(TYPE('lcCompId') <> 'C' ,OARIAAPPLICATION.ACTIVECOMPANYID , LCCOMPID)
LCBASECURR = PADR(IIF(TYPE('lcBaseCurr') <> 'C',OARIAAPPLICATION.BASECURRENCY,LCBASECURR),3)
LCOLDALIAS  = SELECT()
LLCURUSEDBY = .F.
*!*	SET DATASESSION TO 1
IF LCCURRENCY = LCBASECURR
  IF TYPE('lcExUnit') = 'C'
    &LCEXUNIT = 1
    *!*	    SET DATASESSION TO lnCurDataSess
    RETURN 1.0000
  ENDIF
ENDIF
IF !USED('SYCCURR')
  LLEXUSEDBY=.T.
  SELECT 0
  USE (OARIAAPPLICATION.SYSPATH + 'SYCCURR')
ELSE
  SELECT SYCCURR
ENDIF

LLEXUSEDBY=.F.
IF !USED('SYCEXCH')
  LLEXUSEDBY=.T.
  SELECT 0
  USE (OARIAAPPLICATION.SYSPATH + 'SYCEXCH')
ELSE
  SELECT SYCEXCH
ENDIF

LCOLDFLT=FILTER()
LCOLDTAG=TAG()
*E300336,1 Set index descendingly
*SET ORDER TO TAG CURRENCY
SET ORDER TO TAG CURRENCY DESCENDING
*E300336,1 Get current NEAR setting
LCSETNEAR = SET('NEAR')
SET NEAR ON
*E300336,1 end.
SET FILTER TO
IF SEEK(LCBASECURR+PADR(LCCURRENCY,3)+DTOS(LDDATE))
  LNRETRATE= NEXRATE
ELSE
  STORE .F. TO LLMULCURR,LLEXCHRATE
  STORE 0 TO LNEXRATDAY

  LNNOVAR    = GFGETMEMVAR('LLMULCURR,LLEXCHRATE,LNEXRATDAY' , LCCOMPID)
  *E300336,1 Using set near with a descending index places the record
  *E300336,1 pointer on the next best match. Remarked the following,
  *ldCurrDay={}
  *llFound = .F.
  *lnCount = 1
  *DO WHILE !llFound AND lnCount<=lnExratDay
  *  LOCATE FOR CBASECURR+CCURRCODE+DTOS(DRATEDATE) = lcBaseCurr+PADR(lcCurrency,5)+DTOS(ldDate-lnCount)
  *  llFound = FOUND()
  *  lnCount = lnCount + 1
  *ENDDO
  *IF llFound
  *    lnRetRate= nExRate
  *E300336,1 Check the validity of the closest matching record
  IF CBASECURR + CCURRCODE = LCBASECURR + PADR(LCCURRENCY,3) ;
      .AND. DRATEDATE >= LDDATE - LNEXRATDAY
    LNRETRATE = NEXRATE
    *E300336,1 end.
  ELSE
    IF LLEXCHRATE AND LLDISPMSG
      DO FORM SYCHRATE WITH LCCURRENCY, LCBASECURR, LDDATE,SET("Datasession") TO LNRETRATE
      *ASM, After Enabling the Close Button in SYCHRATE.SCX we must handle the case of Closing the screen without Ok or Cancel [Start]
      LNRETRATE=IIF(TYPE('lnRetRate')<>'N',0,LNRETRATE)
      *ASM, After Enabling the Close Button in SYCHRATE.SCX we must handle the case of Closing the screen without Ok or Cancel [End]
    ELSE
      *E300309,1 Display the default error message only if
      *E300309,1 llDispMsg is .T. and llNoErrMsg is .F.
      *IF llDispMsg
      IF LLDISPMSG .AND. !LLNOERRMSG
        *E300309,1 end.
        ** Message : "The last defined excahnge rate exceeds     "
        **           "the valid number of days."+CHR(13)+CHR(10)+"
        **           "The currency will be defaulted to the base "
        **           "currency.                                  "
        **           "                       ® Ok ¯              "
        =OARIAAPPLICATION.MESSAGEBOX("TRM00249B00000","DIALOG")
      ENDIF
    ENDIF
  ENDIF
ENDIF
IF TYPE('lcExUnit') = 'C'
  &LCEXUNIT = LOOKUP(SYCCURR.NCURRUNIT,LCCURRENCY,SYCCURR.CCURRCODE,"CCURRCODE")
ENDIF
SELECT SYCEXCH
IF !EMPTY(LCOLDTAG)
  SET ORDER TO TAG (LCOLDTAG)
ENDIF
SET FILTER TO &LCOLDFLT
IF LLEXUSEDBY
  USE IN SYCEXCH
ENDIF
IF LLCURUSEDBY
  USE IN SYCCURR
ENDIF

*E300336,1 Restore near settings
SET NEAR &LCSETNEAR
*E300336,1 end.

SELECT (LCOLDALIAS)
*!*	SET DATASESSION TO lnCurDataSess
RETURN LNRETRATE


*!*************************************************************************
*! Name      : gfCrtTmp
*! Developer : Hesham El-Sheltawi
*! Date      : 12/08/97
*! Purpose   : to Create uncomplete session temprory files
*!*************************************************************************
*: Calls       :
*!*************************************************************************
*: Passed parameters  : lcFileStruc
*:                      lcTagExp
*:                      lcTag
*:*************************************************************************
*! Returns   :  Temprary file name
*:*************************************************************************
*:E302130,1 Hesham El Sheltawy 03/30/2003
*:E302130,1 Make function create cursors and remove closing and then reopening the tables
FUNCTION GFCRTTMP
*-- E302130,1 Hesham (Start)
*-- Add new parameter to differnciate between creating table and cursor
*PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag
PARAMETERS LCFILE,LCFILESTRUC,LCTAGEXP,LCTAG,LLCURSOR
*-- E302130,1 (End)
PRIVATE LCFILETYPE,LCONERROR,LLERROR,LAFILESTRUC,LCFILENAME,LNWORKAREA,LCTAGTYPE,LNCOUNT
*-- E302130,1 Hesham (Start)
*-- E302130,1 if the llcursor parameter is not defined or passed with wrong value
IF TYPE('llCursor')#"L"
  LLCURSOR = .F.
ENDIF
*-- E302130,1 Hesham (End)
LNWORKAREA = SELECT()
LCFILENAME = IIF(TYPE('lcFile')='C',LCFILE,GFTEMPNAME())
LCONERROR = ON('ERROR')
LLERROR = .F.
ON ERROR LLERROR = .T.
LCFILETYPE = 'A'
LCFILETYPE = IIF(TYPE("lcFileStruc[1]")#"U",'A',;
  IIF(LEFT(ALLT(LCFILESTRUC),1)='(','S','F'))
ON ERROR &LCONERROR
DO CASE
CASE LCFILETYPE = 'F'
  SELECT (LCFILESTRUC)
  =AFIELDS(LAFILESTRUC)
  LCFILETYPE = 'A'
CASE LCFILETYPE= 'A'
  =ACOPY(LCFILESTRUC,LAFILESTRUC)
  *-- E302130,1 Hesham (Start)
  *-- E302130,1 Check for version if later than or equal 8 then add new columns to the array
  *-- E302130,1 if needed
  IF VAL(LEFT(VERSION(4),2))>=8 AND ALEN(LAFILESTRUC,2)<=16
    LOCAL LNCOUNT,LNFCOUNT
    DIMENSION LAFILESTRUC[ALEN(lcFileStruc,1),18]
    STORE "" TO LAFILESTRUC
    FOR LNCOUNT = 1 TO ALEN(LCFILESTRUC,1)
      FOR LNFCOUNT = 1 TO 4
        LAFILESTRUC[lnCount,lnFCount] = LCFILESTRUC[lnCount,lnFCount]
      ENDFOR
      LAFILESTRUC[lnCount,17] = 0
      LAFILESTRUC[lnCount,18] = 0
    ENDFOR
  ENDIF
  *-- E302130,1 Hesham (eND)
ENDCASE
LCTAGTYPE = 'A'
LCTAGTYPE = IIF(TYPE("lcTagExp[1]")#"U",'A','S')
*-- E302130,1 Hesham (Start)
*-- E302130,1 create cursor if llCursor was passed
*IF lcFileType = 'A'
*  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) FROM ARRAY laFileStruc
*ELSE
*  CREATE TABLE (oAriaApplication.WorkDir+lcFileName) &lcFileStruc
*ENDIF
IF LLCURSOR
  IF LCFILETYPE = 'A'
    CREATE CURSOR (LCFILENAME) FROM ARRAY LAFILESTRUC
  ELSE
    CREATE CURSOR (LCFILENAME) &LCFILESTRUC
  ENDIF
ELSE
  IF LCFILETYPE = 'A'
    CREATE TABLE (OARIAAPPLICATION.WORKDIR+LCFILENAME) FROM ARRAY LAFILESTRUC
  ELSE
    CREATE TABLE (OARIAAPPLICATION.WORKDIR+LCFILENAME) &LCFILESTRUC
  ENDIF
ENDIF
*-- E302130,1 Hesham (End)
SELECT (LCFILENAME)
IF LCTAGTYPE = 'A'
  FOR LNCOUNT = 1 TO ALEN(LCTAGEXP,1)
    INDEX ON &LCTAGEXP[lnCount,1] TAG &LCTAGEXP[lnCount,2]
  ENDFOR
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') <> 'C'
    LCTAG = FIELD(1)
  ENDIF
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    INDEX ON &LCTAGEXP TAG &LCTAG
  ENDIF
ENDIF
*-- E302130,1 Hesham (Start)
*USE
*USE (oAriaApplication.WorkDir+lcFileName) EXCLUSIVE
*-- E302130,1 Hesham (End)
IF LCTAGTYPE = 'A'
  SET ORDER TO TAG &LCTAGEXP[1,2]
ELSE
  IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
    SET ORDER TO TAG (LCTAG)
  ENDIF
ENDIF
SELECT (LNWORKAREA)
RETURN LCFILENAME

*!*************************************************************
*! Name      : gfUserPriv
*! Developer : Hesham El-Sheltawi
*! Date      : 04/10/97
*! Purpose   : To get user priv. on process or subprocess.
*!             function to check the user priv. on specific
*!             process & subprocess and return the users
*!             accessebility on it
*!*************************************************************
*! Calls     : gfTempName,gfSubStr
*!*************************************************************
*! Passed Parameters  : lcModule    Module ID
*!                      lcProcess   Process ID
*!                      lcSubProc   SubProcess
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : if gfUserPriv('IC','ICSTYLE','COSTING')
*!*************************************************************
FUNCTION GFUSERPRIV
PARAMETERS LCMODULE,LCPROCESS,LCSUBPROC

 
IF EMPTY(LCMODULE) OR EMPTY(LCPROCESS) OR PARAMETERS()<2
  RETURN .F.
ENDIF
IF ! OARIAAPPLICATION.LOGINREQUIRED OR OARIAAPPLICATION.USER_LEVEL='A'
  RETURN .T.
ENDIF

*B610732,1 TMI 05/27/2014 18:25 [Start] test code 
IF SYS(0) = 'SDE_TAREK # tarek'
_screen.Visible = .t.
SET STEP ON 
ON ERROR 
ENDIF 
*B610732,1 TMI 05/27/2014 18:25 [End  ] 

PRIVATE LCTMPPRIV,LCOLDALIAS,LLRETRUN,LAUSRSUBP,LCSQLSTAT
LCTMPPRIV = GFTEMPNAME()
LCOLDALIAS = SELECT()
SELECT 0
*USE (oAriaApplication.SysPath+'SYUUSRPR') AGAIN ALIAS &lcTmpPriv ORDER TAG CUSER_ID

*llRetrun = SEEK(ALLTRIM(oAriaApplication.User_ID)+UPPER(lcModule+oAriaApplication.ActiveCompanyID+lcProcess)) .OR. ;
*           SEEK(ALLTRIM(oAriaApplication.User_Group)+UPPER(lcModule+oAriaApplication.ActiveCompanyID+lcProcess))

* N000682,1 SAB 04/03/2013 Change Get User Pervilege to use A5 Security [Start]
*lcSqlStat = "Select * from SYUUSRPR WHERE ((cUser_ID = '"+oAriaApplication.User_ID+[' AND CGRPORUSER= "U")]
*IF !EMPTY(oAriaApplication.User_Group)
*  lcSqlStat = lcSqlStat +" OR (cUser_ID = '"+oAriaApplication.User_Group+[' AND CGRPORUSER= "G")]
*ENDIF
*lcSqlStat = lcSqlStat + ") AND CAPP_ID='"+lcModule+"' AND CCOMP_ID='"+oAriaApplication.ActiveCompanyID+"'"+;
*  " AND CPROSS_ID='"+lcProcess+"'"
*lnRemResult = oAriaApplication.remotesystemdata.execute(lcSqlStat,'',lcTmpPriv,"",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))

SET STEP ON   
LCSQLSTAT = "Select * from Aria4XPSecurity('" + OARIAAPPLICATION.USER_ID + "', '" + OARIAAPPLICATION.ACTIVECOMPANYID + "') WHERE ((cUser_ID = '"+OARIAAPPLICATION.USER_ID+[' AND CGRPORUSER= 'U')]
IF !EMPTY(OARIAAPPLICATION.USER_GROUP)
  LCSQLSTAT = LCSQLSTAT +" OR (cUser_ID = '"+OARIAAPPLICATION.USER_GROUP+[' AND CGRPORUSER= 'G')]
ENDIF
*B610732,1 TMI 05/26/2014 17:18 [Start] enhance the statement
*LCSQLSTAT = LCSQLSTAT + ") AND CAPP_ID='"+""+"' AND CCOMP_ID='"+OARIAAPPLICATION.ACTIVECOMPANYID+"'"+;
  " AND target_type='"+OARIAAPPLICATION.GETARIA5TYPE(LCPROCESS)+"'"
lcGETARIA5TYPE_PROCESS = OARIAAPPLICATION.GETARIA5TYPE(LCPROCESS)
LCSQLSTAT = LCSQLSTAT + ") AND CAPP_ID='"+""+"' AND ALLTRIM(UPPER(target_typ))==ALLTRIM(UPPER('"+lcGETARIA5TYPE_PROCESS+"'))"
*B610732,1 TMI 05/26/2014 17:18 [End  ]   

LOCAL LNDATASESSION
LNDATASESSION =   SET("Datasession")

SET DATASESSION TO 1
*B610732,1 TMI 05/26/2014 17:20 [Start] use lfGetAria4xpSecurity instead
*LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE(LCSQLSTAT,'',LCTMPPRIV,"",OARIAAPPLICATION.ARIA5SYSTEMMANAGERCONNECTION,3,"",1)
*OARIAAPPLICATION.REPLACEARIA5TYPES(LCTMPPRIV)
LNREMRESULT = oAriaApplication.lfGetAria4xpSecurity(LCSQLSTAT,oAriaApplication.ActiveCompanyID,LCTMPPRIV)
*B610732,1 TMI 05/26/2014 17:20 [End  ] 
* N000682,1 SAB 04/03/2013 Change Get User Pervilege to use A5 Security [End]


IF LNREMRESULT =1
  LOCATE
  *B125765 ,1 HMA  12/21/2004 assign initial value to the variable llReturn. [BEGIN]
  * llRetrun = IIF(TYPE('lcSubProc')='C',.F.,llRetrun)
  LLRETRUN = IIF(TYPE('lcSubProc')='C',.F.,!EOF())
  *B125765 ,1 HMA  12/21/2004 assign initial value to the variable llReturn. [END]


  *--Be sure that no empty string is in the memo field to have subprocess.
  IF !EMPTY(ALLTRIM(MSUBPROC)) AND TYPE('lcSubProc')='C'
    DIMENSION LAUSRSUBP[1,2]
    =GFSUBSTR(LEFT(MSUBPROC,ATC('|',MSUBPROC)-1),@LAUSRSUBP)
    LLRETRUN = ASCAN(LAUSRSUBP,UPPER(LCSUBPROC))>0
  ENDIF

  USE IN (LCTMPPRIV)
ELSE
  LLRETRUN = .F.
ENDIF
SET DATASESSION TO LNDATASESSION
SELECT (LCOLDALIAS)
RETURN LLRETRUN




*!*****************************************************************
*! Name : gfBrowWare.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 04/17/94.
*!*****************************************************************
*! Synopsis : Browse the warehouse file.
*!*****************************************************************
*! Passed :
*!        Parameters :
*!         llIsEscape: If the escape is allowed.
*!******************************************************************
*! Files      : WareHous.DBF should be opend.
*!******************************************************************
*! Returned :
*!        VARIABLES :
*!        lcWareCode : The warehouse code that will be selected
*!                     if it's empty it means that the user did
*!                     not select anything.
*!******************************************************************
*! Example :
*!        =gfBrowWare(.T.)
*!******************************************************************
FUNCTION GFBROWWARE
PARAMETERS LLISESCAPE, LCITEM, LCCOLOR, LCWARECODE, LCSRCFILE,LCSTYMATIN,LLFORCURRSITE

PRIVATE LCBRWINNAME, LCBRTTL, LCWARECODE, LCITEM, LCCOLOR, LCFORCOND;
  LNCURALIAS, LNCURTAG

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\WAREBROW.H
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCBROWWARE,LCARIAHFILE
LCBROWWARE = ""
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCBROWWARE = OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("WAREBROW")+"_"+"H" +".XML")
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
IF !USED('WareHous')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'WareHous','WareHous','SH')
ENDIF
GO TOP IN WAREHOUS
IF EOF('WareHous')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=MESSAGEBOX(LANG_NOLOC,16,_SCREEN.CAPTION)
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_NOLOC,OARIAAPPLICATION.GETHEADERTEXT("LANG_NOLOC",LCBROWWARE)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
  RETURN SPACE(6)
ENDIF

*-- Save the current alias.
LNCURALIAS = SELECT()

*!B999999,1 WSH 03/08/2005, Create a variable to hold warehouse to locate at. [Start]
LOCAL LCGOTOWARE
LCGOTOWARE = LCWARECODE
*!B999999,1 WSH 03/08/2005, [End]

SELECT WAREHOUS
LNCURTAG    = VAL(SYS(21))
SET ORDER TO TAG WAREHOUS

LCWARECODE  = IIF(EMPTY(LCWARECODE), SPACE(6), LCWARECODE)
*-- If called from browsing the warehouses of a specific item/color.
IF !EMPTY(LCSRCFILE)
  LCCOLOR     = IIF(EMPTY(LCCOLOR), SPACE(6), LCCOLOR)
  IF UPPER(ALLTRIM(LEFT(LCSRCFILE,1))) = 'S'
    LCSRCFILE = 'STYDYE'
    LCITEM    = IIF(EMPTY(LCITEM), SPACE(12), PADR(LCITEM,12))
    LCFLD     = 'Style'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcTMsg    = LANG_STYLE
    LCTMSG    = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STYLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_STYLE",LCBROWWARE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    LLFOUND   = .F.
    IF SEEK(LCITEM , LCSRCFILE)
      SELECT (LCSRCFILE)
      LOCATE REST FOR EMPTY(DYELOT) WHILE &LCFLD = LCITEM
      LLFOUND = FOUND()
    ENDIF
    IF !LLFOUND
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *=MESSAGEBOX(lcTMsg + ALLTRIM(lcItem) +' '+LANG_NOTASN,64)
      =MESSAGEBOX(LCTMSG + ALLTRIM(LCITEM) +' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_NOTASN,OARIAAPPLICATION.GETHEADERTEXT("LANG_NOTASN",LCBROWWARE)),64)
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      SET ORDER TO LNCURTAG IN WAREHOUS
      SELECT (LNCURALIAS)
      RETURN SPACE(6)
    ENDIF
    SELECT WAREHOUS
    SET RELATION TO LCITEM + CWARECODE + SPACE(10) INTO (LCSRCFILE) ADDITIVE
  ELSE
    LCSRCFILE = 'FABDYE'
    LCITEM    = IIF(EMPTY(LCITEM), SPACE(7), PADR(LCITEM,7))
    LCFLD     = 'Fabric'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcTMsg    = LANG_ITMCLR
    LCTMSG    = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ITMCLR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ITMCLR",LCBROWWARE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    LLFOUND   = .F.
    IF SEEK(LCITEM + LCCOLOR , LCSRCFILE)
      SELECT (LCSRCFILE)
      LOCATE REST FOR EMPTY(DYELOT) ;
        WHILE &LCFLD + COLOR = LCITEM + LCCOLOR
      LLFOUND = FOUND()
    ENDIF
    IF !LLFOUND
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *=MESSAGEBOX( lcTMsg + ALLTRIM(lcItem) + '/' + ALLTRIM(lcColor) +LANG_NOTASN,64)
      =MESSAGEBOX( LCTMSG + ALLTRIM(LCITEM) + '/' + ALLTRIM(LCCOLOR) +IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_NOTASN,OARIAAPPLICATION.GETHEADERTEXT("LANG_NOTASN",LCBROWWARE)),64)
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      SET ORDER TO LNCURTAG IN WAREHOUS
      SELECT (LNCURALIAS)
      RETURN SPACE(6)
    ENDIF
    SELECT WAREHOUS
    SET RELATION TO LCITEM + LCCOLOR + CWARECODE + SPACE(10) ;
      INTO (LCSRCFILE) ADDITIVE
  ENDIF
  LCFORCOND   = '!EOF(lcSrcFile)'
ELSE
  LCWARECODE = SPACE(6)
  LCFORCOND  = ''
ENDIF


IF TYPE('lcStyMatIn')='C' AND LCSTYMATIN $ 'SM'
  LCMATORSTY = IIF(UPPER(LCSTYMATIN) = 'S','lStyInv','lMatInv')
  LCFORCOND  = LCFORCOND +IIF(EMPTY(LCFORCOND),'',' AND ') + LCMATORSTY
ENDIF

IF LLFORCURRSITE
  LCFORCOND  = LCFORCOND +IIF(EMPTY(LCFORCOND),'',' AND ') + "cSiteId = oAriaApplication.CurrentSite"
ENDIF


*B123842,1 WSH Browse Locations Using Aria Global Browse Screen. [Start]
*DO FORM (oAriaApplication.ScreenHome+"IC\Warebrow.scx") WITH lcForCond TO lcWareCode

LOCAL LLWASSEL
DIMENSION LAWARDATA[1]
LAWARDATA = ''

SELECT WAREHOUS
PRIVATE LCBRFIELDS
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields = "cWareCode:H='"+LANG_ARIA_LOCATION+"':17,cDesc:H='"+LANG_ARIA_DESCRIPTION+"':30"
LCBRFIELDS = "cWareCode:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_LOCATION,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_LOCATION",LCARIAHFILE))+;
  "':17,cDesc:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DESCRIPTION,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DESCRIPTION",LCARIAHFILE))+;
  "':30"
*N000682,1 11/20/2012 MMT Globlization changes[End]


*!B999999,1 WSH 03/08/2005, Locate at the first match for lcWareCode. [Start]
IF !EMPTY(LCGOTOWARE)
  LOCAL LNSOFTSEEK
  LCGOTOWARE = RTRIM(LCGOTOWARE)
  LCGOTOWARE = IIF(RIGHT(LCGOTOWARE,1) == '?', SUBSTR(LCGOTOWARE, 1, LEN(LCGOTOWARE) - 1), LCGOTOWARE)

  IF !SEEK(LCGOTOWARE)
    LNSOFTSEEK = RECNO(0)
    IF LNSOFTSEEK > 0
      GO LNSOFTSEEK
    ELSE
      LOCATE
    ENDIF
  ENDIF
ENDIF
*!B999999,1 WSH 03/08/2005, [End]

*N120718,1 HBG 08/10/2004 FIX bug variable 'FOR' not found [Begin]
*llWasSel = ARIABROW("'' FOR "+lcForCond,LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData")
*khm
*llWasSel = ARIABROW(lcForCond,LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llWasSel = ARIABROW(IIF(!EMPTY(lcForCond),"'' FOR "+lcForCond,''),LANG_ARIA_LOCTTL,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","CWARECODE","laWarData")
LLWASSEL = ARIABROW(IIF(!EMPTY(LCFORCOND),"'' FOR "+LCFORCOND,''),IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_LOCTTL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_LOCTTL",LCARIAHFILE)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"","","CWARECODE","laWarData")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*khm
*N120718,1 [End]
IF LLWASSEL
  LCWARECODE = LAWARDATA[1]
ENDIF
*B123842,1 WSH [End]

IF !EMPTY(LCSRCFILE)
  SET RELATION OFF INTO (LCSRCFILE)
ENDIF

SET ORDER TO LNCURTAG IN WAREHOUS

SELECT (LNCURALIAS)
POP KEY
LCWARECODE = IIF(EMPTY(LCWARECODE),SPACE(6),LCWARECODE)
RETURN (LCWARECODE)




*!*************************************************************
*! Name : gpAdStyWar
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 05/03/94.
*!*************************************************************
*! Synopsis : Add a new record to the StyDye file.
*!*************************************************************
*! Passed :
*!        Parameters :
*!          lcPStyle : The style.
*!          lcPColor : The color.
*!          lcPDyelot: The Dyelot.
*!          lcPWare  : The Warehouse.
*!        Files      : The StyDye File should be opened.
*!                     The WareHouse File should be opened.
*!*************************************************************
*! Returned :
*!        Files      : The StyDye File after appending the new record.
*!*************************************************************
*! Example :
*!        DO gpAdStyWar WITH lcStyle,lcColor,SPACE(10),lcWareCode
*!*************************************************************
PROCEDURE GPADSTYWAR

PARAMETERS LCPSTYLE, LCPDYELOT, LCPWARE
PRIVATE LCDESC, LCALIAS, LNCOST , LCDISCCODS

LCALIAS = ALIAS()
*N037578,1 KHM 09/30/2004 Open the style file if it not open before, because in
*N037578,1                some programs we use fox files remotely [Begin]
PRIVATE LLOPNSTYFIL
LLOPNSTYFIL = .F.
IF !USED('STYLE')
  LLOPNSTYFIL = .T.
  = GFOPENFILE(OARIAAPPLICATION.DATADIR+'STYLE','STYLE','SH')
ENDIF
*N037578,1 KHM 09/30/2004 [End]

SELECT STYLE
IF SEEK (LCPSTYLE)
  LCDESC = DESC
  LNCOST = AVE_COST
  LCDISCCODS  = CDISCCODE
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
  LCGLCODE = LINK_CODE
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
ELSE
  LCDESC = SPACE(20)
  LNCOST = 0
  LCDISCCODS  = CDISCCODE
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
  LCGLCODE = ""
  *!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
ENDIF

STORE .F. TO LLSTYDYE,LLSTYINVJL
IF !USED('STYDYE')
  LLSTYDYE   = .T.
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'STYDYE','STYDYE','SH')
ENDIF
IF !USED('STYINVJL')
  LLSTYINVJL = .T.
  * E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[Start]
  *=GFOPENFILE(OARIAAPPLICATION.DATADIR+'STYINVJL','STYINVJL','SH')  
  =GFOPENTABLE(OARIAAPPLICATION.DATADIR+'STYINVJL','STYINVJL','SH')
  * E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[End]  
ENDIF
=GFOPENFILE(OARIAAPPLICATION.DATADIR+'WareHous','WareHous','SH')
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
LCSTYDYREL = ''
LCSTYLEREL = ''
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SELECT STYDYE
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
LCSTYDYREL = SET("Relation")
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SET RELATION TO
SET RELATION TO STYLE+CWARECODE INTO STYINVJL ADDITIVE

SELECT STYLE
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
LCSTYLEREL =SET("Relation")
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SET RELATION TO
SET RELATION TO STYLE INTO STYDYE ADDITIVE



SELECT WAREHOUS
*!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
*lcGlCode = IIF(SEEK(lcPWare),GL_LINK,'DEFDEF')
LCGLCODE = IIF(EMPTY(LCGLCODE),'DEFDEF',LCGLCODE)
*!B038241,1 AKA Update the GLLink Code with DEFDEF with the default Style Link Code [End]

SELECT STYDYE
APPEND BLANK
REPLACE STYLE      WITH LCPSTYLE  ,;
  DESC       WITH LCDESC    ,;
  CDISCCODE  WITH LCDISCCODS,;
  DYELOT     WITH LCPDYELOT ,;
  CWARECODE  WITH LCPWARE   ,;
  AVE_COST   WITH IIF(EMPTY(DYELOT),LNCOST,0) ,;
  GL_LINK    WITH LCGLCODE
=GFADD_INFO('STYDYE')

*C200171 TMI [Start] Gen Upcs in EDICATGD
*-- Run if EDI installed
IF ASCAN(OARIAAPPLICATION.LAEVNTTRIG,PADR("GNUPCWH",10)) <> 0
  IF OCCURS('NC',OARIAAPPLICATION.COMPANYINSTALLEDMODULES)<>0
    LCWHCODE   = LCPWARE
    LCSTY      = LCPSTYLE
    LLFRMADWRE = .T.
    =GFDOTRIGER('ICSTYLE','GNUPCWH   ')
  ENDIF
ENDIF
*C200171 TMI [End  ] Gen Upcs in EDICATGD
* E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[Start]
=gfSeek(STYDYE.STYLE+STYDYE.CWARECODE,'STYINVJL')
* E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[End]

IF !EOF('STYDYE') .AND. EOF('STYINVJL')
  SELECT STYDYE
  REPLACE REST WHILE STYLE = STYLE.STYLE ;
    FOR GFTRACEKEY("STYDYE",STYLE+CWARECODE+DYELOT,"M") ;
    AVE_COST WITH IIF(EMPTY(DYELOT),STYLE.AVE_COST,0)
ENDIF

SELECT STYDYE
SET RELATION TO
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
IF !EMPTY(LCSTYDYREL)
  SET RELATION TO &LCSTYDYREL
ENDIF
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

SELE STYLE
SET RELATION TO
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [Start]
IF !EMPTY(LCSTYLEREL)
  SET RELATION TO &LCSTYLEREL
ENDIF
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [END]

*-- Close files
IF LLSTYDYE
  USE IN STYDYE
ENDIF
IF LLSTYINVJL
  * E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[Start]
  *USE IN STYINVJL
  =gfCloseTable('STYINVJL')
  * E303976,1 MMT 07/24/2018 Convert STYINVJL to SQL[End]
ENDIF

*N037578,1 KHM 09/30/2004 Close the file if its opened in this session [Begin]
IF LLOPNSTYFIL
  USE IN STYLE
ENDIF
*N037578,1 KHM 09/30/2004 [End]

IF !EMPTY(LCALIAS)
  SELECT (LCALIAS)
ENDIF
RETURN


*!*************************************************************
*! Name : gpAdFabWar
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 06/28/94.
*!*************************************************************
*! Synopsis : Add a new record to the FabDye file.
*!*************************************************************
*! Passed :
*!        Parameters :
*!          lcFab  : The fabric.
*!          lcClr  : The color.
*!          lcDye  : The Dyelot.
*!          lcWare : The Warehouse.
*!        Files    : The FabDye File should be opened.
*!                   The WareHouse File should be opened.
*!*************************************************************
*! Returned :
*!        Files      : The FabDye File after appending the new record.
*!*************************************************************
*! Example :
*!        DO gpAdFabWar WITH lcfabric,lcColor,SPACE(10),lcWareCode
*!*************************************************************
PROCEDURE GPADFABWAR

PARAMETERS LCFAB, LCCLR, LCDYEL, LCWARE, LCTMPSCOPE

PRIVATE LCDESC, LNALIAS, LLGLLINK
LNALIAS = SELECT()
LCGLCODE = SPACE(3)
LLUSED = .F.
IF !USED('')
  LLUSED = GFOPENFILE(OARIAAPPLICATION.DATADIR+'FABRIC','FABRIC','SH')
ENDIF
= SEEK(LCFAB+LCCLR,'FABRIC')

SELECT FABDYE
APPEND BLANK
LLLOCK = RLOCK()
REPLACE FABRIC     WITH LCFAB    ,;
  COLOR      WITH LCCLR    ,;
  DYELOT     WITH LCDYEL   ,;
  CWARECODE  WITH LCWARE   ,;
  GL_LINK    WITH LCGLCODE ,;
  NFAVE_COST WITH IIF(EMPTY(LCDYEL),FABRIC.NFAVE_COST,0),;
  NAVECSTBUY WITH IIF(EMPTY(LCDYEL),FABRIC.NAVECSTBUY,0)
UNLOCK
=GFADD_INFO('FABDYE')

IF LLUSED
  USE IN FABRIC
ENDIF

IF TYPE('lcTmpScope') $ 'UL'
  LCTMPSCOPE = ''
ENDIF

IF !EMPTY(LCDYEL)
  LCFAB = PADR(LCFAB,7)
  LCCLR = PADR(LCCLR,6)
  LCDYEL= PADR(LCDYEL,10)

  LLDYEREL = GFOPENFILE(OARIAAPPLICATION.DATADIR+"Dye_Rel","Dye_Rel","SH")
  SELECT DYE_REL
  SET ORDER TO DYE_REL

  *-- if you did not find this fabric, color, dyelot record
  IF !SEEK(LCFAB + LCCLR + LCDYEL)
    PRIVATE LNNEAREST,LCDYE_SEQ
    LCDYE_SEQ = ''
    LNNEAREST = RECNO(0)

    *-- Add this block to adjust add records at top of file. [begin]
    IF (LNNEAREST # 0)
      GO LNNEAREST
      IF FABRIC + COLOR != LCFAB + LCCLR
        SET ORDER TO DYE_REL DESCENDING
        = SEEK(LCFAB + LCCLR + LCDYEL)
        LNNEAREST = RECNO(0)
        IF LNNEAREST # 0
          GO LNNEAREST
          IF (FABRIC + COLOR != LCFAB + LCCLR) OR (LCDYEL > DYELOT)
            LNNEAREST = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *-- Add this block to adjust add records at top of file. [end]

    *-- if it is Last dyelot code.
    IF LNNEAREST = 0
      SET ORDER TO SEQUENCE DESCENDING
      = SEEK(LCFAB + LCCLR)
      LCDYE_SEQ = PADL(ALLTRIM(STR(VAL(CDYE_SEQ) + 1)),4,'0')
    ENDIF

    SET ORDER TO
    IF LNNEAREST # 0
      GO LNNEAREST
      LCDYE_SEQ = CDYE_SEQ

      SCAN FOR FABRIC + COLOR + CDYE_SEQ = LCFAB + LCCLR AND CDYE_SEQ >= LCDYE_SEQ
        REPLACE CDYE_SEQ WITH PADL(ALLTRIM(STR(VAL(CDYE_SEQ) + 1)),4,'0')
      ENDSCAN
    ENDIF

    *-- insert new dyelot line.
    SELECT DYE_REL
    INSERT INTO ('Dye_Rel')                                 ;
      (FABRIC, COLOR, DYELOT, CDYE_SEQ , CTMPSCOPE ) ;
      VALUES (LCFAB , LCCLR, LCDYEL, LCDYE_SEQ, LCTMPSCOPE)
    =GFADD_INFO('DYE_REL')
  ENDIF

  IF LLDYEREL
    USE IN DYE_REL
  ENDIF

ENDIF

=SELECT(LNALIAS)
RETURN

*!*************************************************************
*! Name      : gfSheetItem
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Generate, Modify or Delete cost sheet items for style
*!*************************************************************
*! Calls     : gfGetMemVar,gfModalGen,gfItemMask,gpAdStyWar,gpAdFabWar,lfSelDyelot
*!*************************************************************
*! Parameters: lcTranType : Transaction type   ('M'-'I'-'T')
*!             lcTicketNo : Ticket number
*!             lcLinkCode : WIP Link code
*!             lcItem     : Style/Fabric
*!             lcColor    : Color
*!             lnLineNo   : Ticket line number
*!             lcDyelot   : Dyelot
*!             lcStyWare  : Default Style Issue Warehouse
*!             lcMatWare  : Default Fabric Issue Warehouse
*!             laQty      : Quantity array
*!             lcTmpBom   : Style Cost Sheet file name
*!             lcTktSheet : Cost sheet header file name
*!             lcDetFile  : Cost sheet detailed file name
*!             lcOprHdr   : Operation header file name
*!             lcLastOpr  : Last Operation
*!             lnPrice    : Purchase Price
*!             lnEst1     : Estimated cost 1
*!             lnEst2     : Estimated cost 2
*!             lnEst3     : Estimated cost 3
*!             lnEst4     : Estimated cost 4
*!             lnEst5     : Estimated cost 5
*!             lcTmpName  : Temp name needed to arrange new fabric dyelots
*!             llBackOnly : If .T. update all records in background.
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =gfSheetItem('M','000001','DEFDEF','ITEMD001','',0,'Dyelot',;
*!                           'WARE1',@laQty,100,'BOM','CTKTBOM','BOMLINE',;
*!                           'MFGOPRHD','000001',0,0,0,0,0)
*!:************************************************************************
*!
FUNCTION GFSHEETITEM

*N119813,1 AMH Add parameters for SQL tables and two parameters for cost element 6 and cost element 7 [Start]
*PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,;
lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lcTmpName,llBackOnly

*E038150,1 AMH Pass the inventory type instead of color [Start]
*PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,;
lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,;
lcPosHdr,lcPosLn,lcTmpName,llBackOnly
PARAMETERS LCTRANTYPE,LCTICKETNO,LCLINKCODE,LCITEM,LCINVTYPE,LNLINENO,LCDYELOT,;
  LCSTYWARE,LCMATWARE,LAQTY,LCTMPSHEET,LCTKTSHEET,LCDETFILE,LCOPRHDR,;
  LCLASTOPR,LNPRICE,LNEST1,LNEST2,LNEST3,LNEST4,LNEST5,LNEST6,LNEST7,;
  LCPOSHDR,LCPOSLN,LCTMPNAME,LLBACKONLY
*E038150,1 AMH [End]

PRIVATE LLSTYCONFG
*N119813,1 AMH [End]

*B608223,1 TMI [Start] check if the calling function is 'LFVGENERATE'
LLSHEETGEN = (LFGTCALLFN() == 'LFVGENERATE')
*B608223,1 TMI [End  ]

PRIVATE    LCMJRMSK,LCMJRHDR,LCNMJRMSK,LAITEMSEG,LCCLRMSK,LNCLRPOS,LCITEMFILE,;
  LLDYELOT,LCSCALE,LCITEMTYPE,LNTRANQTY,LCSIZES,LNCOUNT,LCCSTCLR,LNUNITPRI,;
  LCCSTITM,LLCONTINUE,LAMFGRFLD,LNLASTSEQ,LNSIZEPOS,LCTMPBOMSH,LAREQ,;
  LCCONTCODE,LCCONTNAME,LCOPERSEQ,LLINHOUSE,LLMFGOPR,LNLEADTIME
PRIVATE    LCMFGGLACNT,LCEXSIGN,LCUNTSIN,LNALIAS,LCPRICECUR,LNPRICERATE,;
  LNPRICEUNIT,LCDUTYCUR,LNDUTYRATE,LNDUTYUNIT, LNUNTQTY,LCTRANLETT


STORE 1   TO LNPRICERATE,LNPRICEUNIT,LNDUTYRATE,LNDUTYUNIT
STORE '/' TO LCEXSIGN,LCUNTSIN

*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
STORE 0 TO LNUNITPRI
*B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]


*N119813,1 AMH Consider cases of Dye order and Inter-Location PO [Start]
*lcTranLett = IIF(lcTranType $ "IMT",lcTranType ,"I")
LCTRANLETT = IIF(LCTRANTYPE $ "IMTDN",LCTRANTYPE ,"I")
*N119813,1 AMH [End]

IF LCTRANTYPE $ 'IN'

  *N119813,1 AMH Use the cursor name of the POSHDR file [Start]
  *=SEEK('P'+lcTicketNo,'POSHDR')
  *lcPriceCur  = IIF(EMPTY(POSHDR.cPriceCur),oAriaApplication.BaseCurrency,POSHDR.cPriceCur)
  *lnPriceRate = IIF(POSHDR.nPriceRat=0,1,POSHDR.nPriceRat)
  *lnPriceUnit = IIF(POSHDR.nCurrUnit=0,1,POSHDR.nCurrUnit)
  *lcDutyCur   = IIF(EMPTY(POSHDR.cDutyCur),oAriaApplication.BaseCurrency,POSHDR.cDutyCur)
  *lnDutyRate  = IIF(POSHDR.nDutyRat=0,1,POSHDR.nDutyRat)
  *lnDutyUnit  = IIF(POSHDR.nDCurUnit=0,1,POSHDR.nDCurUnit)
  LCPRICECUR  = IIF(EMPTY(EVALUATE(LCPOSHDR+'.cPriceCur')),OARIAAPPLICATION.BASECURRENCY,EVALUATE(LCPOSHDR+'.cPriceCur'))
  LNPRICERATE = IIF(EVALUATE(LCPOSHDR+'.nPriceRat')=0,1,EVALUATE(LCPOSHDR+'.nPriceRat'))
  LNPRICEUNIT = IIF(EVALUATE(LCPOSHDR+'.nCurrUnit')=0,1,EVALUATE(LCPOSHDR+'.nCurrUnit'))
  LCDUTYCUR   = IIF(EMPTY(EVALUATE(LCPOSHDR+'.cDutyCur')),OARIAAPPLICATION.BASECURRENCY,EVALUATE(LCPOSHDR+'.cDutyCur'))
  LNDUTYRATE  = IIF(EVALUATE(LCPOSHDR+'.nDutyRat')=0,1,EVALUATE(LCPOSHDR+'.nDutyRat'))
  LNDUTYUNIT  = IIF(EVALUATE(LCPOSHDR+'.nDCurUnit')=0,1,EVALUATE(LCPOSHDR+'.nDCurUnit'))
  *N119813,1 AMH [End]

ENDIF
STORE ''  TO M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC

*N119813,1 AMH Use the correct company ID [Start]
*=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',gcAct_Comp)
=GFGETMEMVAR('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',OARIAAPPLICATION.ACTIVECOMPANYID)
LLSTYCONFG = (ALLTRIM(GFGETMEMVAR('M_STYCNFG',OARIAAPPLICATION.ACTIVECOMPANYID))='Y')
*N119813,1 AMH [End]

*E038150,1 AMH Consider the inventory type [Start]
*lcMjrMsk  = gfItemMask("PM")
*lcMjrHdr  = gfItemMask("HM")
*lcNMjrMsk = gfItemMask("PN")
*lcItmMsk  = gfItemMask("PI")
LCMJRMSK  = GFITEMMASK("PM","",LCINVTYPE)
LCMJRHDR  = GFITEMMASK("HM","",LCINVTYPE)
LCNMJRMSK = GFITEMMASK("PN","",LCINVTYPE)
LCITMMSK  = GFITEMMASK("PI","",LCINVTYPE)
*E038150,1 AMH [End]

*N119813,1 AMH Use the cursor name of the BOM file [Start]
*=gfOpenFile(oAriaApplication.DataDir+'BOM',oAriaApplication.DataDir+'BOM','SH')
SELECT (LCTMPSHEET)
*N119813,1 AMH [End]

*E038150,1 AMH Consider the inventory type [Start]
*IF !SEEK(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),lcTmpSheet)
IF !SEEK(LCINVTYPE+SUBSTR(LCITEM,1,LEN(LCMJRMSK)),LCTMPSHEET)
  *E300725,1 Message : 38031
  *E300725,1 Cost sheet not found for style: xxx cannot generate cutting
  *E300725,1 ticket cost sheet
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *=gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','Fabric: ',lcMjrHdr+':')+;
  ALLTRIM(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))))+;
  '|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
  =GFMODALGEN('TRM38031B00000','ALERT',LCMJRHDR+':'+ALLTRIM(SUBSTR(LCITEM,1,LEN(LCMJRMSK)))+;
    '|'+IIF(LCTRANTYPE='M','cutting ticket',IIF(LCTRANTYPE='I','purchase order','order')))
  RETURN(.F.)
ENDIF
*E038150,1 AMH [End]

LNALIAS = SELECT()
DECLARE LAMFGRFLD[7,2],LAITEMSEG[1],LAREQ[8]
STORE '' TO LCCONTCODE,LCCONTNAME,LCOPERSEQ,LLINHOUSE,LLMFGOPR,LNLEADTIME,LCMFGGLACNT
LAMFGRFLD[1,1] = 'CCONTCODE'
LAMFGRFLD[1,2] = 'lcContCode'
LAMFGRFLD[2,1] = 'CCONTNAME'
LAMFGRFLD[2,2] = 'lcContName'
LAMFGRFLD[3,1] = 'COPERSEQ'
LAMFGRFLD[3,2] = 'lcOperSeq'
LAMFGRFLD[4,1] = 'LINHOUSE'
LAMFGRFLD[4,2] = 'llInHouse'
LAMFGRFLD[5,1] = 'LMFGOPR'
LAMFGRFLD[5,2] = 'llMfgOpr'
LAMFGRFLD[6,1] = 'LEADTIME'
LAMFGRFLD[6,2] = 'lnLeadTime'
LAMFGRFLD[7,1] = 'GLACCOUNT'
LAMFGRFLD[7,2] = 'lcMfgGlAcnt'

*E038150,1 AMH Consider the inventory type [Start]
*=gfItemMask(@laItemSeg)
=GFITEMMASK(@LAITEMSEG,"",LCINVTYPE)
*E038150,1 AMH [End]

*B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
LLMULCURR = GFGETMEMVAR('llMulCurr')
*B608542,1 WAM 08/14/2008 (End)

FOR LNCOUNT = 1 TO ALEN(LAITEMSEG,1)
  IF LAITEMSEG[lnCount,1]='C'
    LCCLRMSK = LAITEMSEG[lnCount,3]
    LNCLRPOS = LAITEMSEG[lnCount,4]
  ENDIF
  IF LAITEMSEG[lnCount,1]='S'
    LNSIZEPOS = LAITEMSEG[lnCount,4]
  ENDIF
ENDFOR
LNLASTSEQ = 0
IF !EMPTY(LCLASTOPR)
  =GFRLTFLD(LCLASTOPR,@LAMFGRFLD,'MFGCODE')
  LNLASTSEQ = VAL(LEFT(LCOPERSEQ,2))
ENDIF

*N119813,1 AMH Add variables for cost elements # 6,7 [Start]
*STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnFEst1,lnFEst2,lnFEst3,lnFEst4,lnFEst5
STORE 0 TO LNEST1,LNEST2,LNEST3,LNEST4,LNEST5,LNEST6,LNEST7,LNFEST1,LNFEST2,LNFEST3,LNFEST4,LNFEST5,LNFEST6,LNFEST7
*N119813,1 AMH [End]

LCITEMFILE = IIF(LCTRANTYPE='T','Fabric','Style')
IF !USED('Style')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'STYLE',OARIAAPPLICATION.DATADIR+'STYLE','SH')
ENDIF
IF !USED('Scale')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'Scale',OARIAAPPLICATION.DATADIR+'Scale','SH')
ENDIF

*E038150,1 AMH Remove the lcColor parameter [Start]
*llDyelot   = SEEK(lcItem+ALLTRIM(lcColor),lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
*lcScale    = IIF(lcTranType='T','',Style.Scale)
LLDYELOT   = SEEK(LCITEM,LCITEMFILE) .AND. EVALUATE(LCITEMFILE+'.cDye_Flg')='Y'
LCSCALE    = EVALUATE(LCITEMFILE+'.Scale')
PRIVATE LNSCLCNT
LNSCLCNT = IIF(SEEK('S'+LCSCALE,'SCALE'),SCALE.CNT,8)
*E038150,1 AMH [End]

LLCONTINUE = .T.

*N119813,1 AMH Don't need to open these files [Start]
*=gfOpenFile(oAriaApplication.DataDir+'Ctktbom',oAriaApplication.DataDir+'Ctktbom','SH')
*=gfOpenFile(oAriaApplication.DataDir+'BomLine',oAriaApplication.DataDir+'BomLine','SH')
*=gfOpenFile(oAriaApplication.DataDir+'MFGOPRHD',oAriaApplication.DataDir+'MFGOPRHD','SH')
*N119813,1 AMH [End]

SET ORDER TO TAG CTKTBOM  IN (LCTKTSHEET)
SET ORDER TO TAG BOMLINE  IN (LCDETFILE)
SET ORDER TO TAG MFGOPRHD IN (LCOPRHDR)

LCTMPBOMSH = GFTEMPNAME()

*N119813,1 AMH Use the cursor name of the BOM file [Start]
*SELECT BOM
SELECT (LCTMPSHEET)
*N119813,1 AMH [End]

=AFIELDS(LAFILESTRU)
CREATE TABLE (OARIAAPPLICATION.WORKDIR+LCTMPBOMSH) FROM ARRAY LAFILESTRU

*E038150,1 AMH Remove IClr from lcTmpBomSh [Start]
*INDEX ON typ+item+iclr+citmmajor+citmmask TAG (lcTmpBomSh)
INDEX ON TYP+CINVTYPC+ITEM+CINVTYPE+CITMMAJOR+CITMMASK TAG (LCTMPBOMSH)
*E038150,1 AMH [End]
*B608180,1 TMI [Start] reverse the order so the operations are met first
*B608223,1 TMI [Start] change order asscending type when called from lfvGenerate
IF LLSHEETGEN
  *B608223,1 TMI [End  ]
  SET ORDER TO TAG &LCTMPBOMSH DESCENDING
  LOCATE
  *B608223,1 TMI [Start]
ENDIF
*B608223,1 TMI [End  ]
*B608180,1 TMI [End  ]

*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
PRIVATE LCCOSTELEM
LCCOSTELEM = GFTEMPNAME()
CREATE CURSOR (LCCOSTELEM) (CTRTYPE C(1), CBOMTYP C(1), ITEM C(19), MFGCODE C(6), CDYELOT C(10), CSIZES C(8))
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON cTrType+CBOMTYP +Item+MFGCode+cDyelot TAG (lcCostElem) OF (lcCostElem)
INDEX ON CTRTYPE+CBOMTYP +ITEM+MFGCODE+CDYELOT TAG (LCCOSTELEM)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]

SELECT (LCTMPSHEET)

*E038150,1 AMH Consider the inventory type [Start]
*=SEEK(PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19))
*SCAN REST WHILE cItmMajor = PADR(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))),19)
=SEEK(LCINVTYPE+PADR(SUBSTR(LCITEM,1,LEN(LCMJRMSK)),19))
SCAN REST WHILE CINVTYPE+CITMMAJOR = LCINVTYPE+PADR(SUBSTR(LCITEM,1,LEN(LCMJRMSK)),19)
  *E038150,1 AMH [End]

  SCATTER MEMVAR MEMO
  IF CCATGTYP='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
      SUBSTR(ALLTRIM(ITEM),LNSIZEPOS) = STRTRAN(SUBSTR(LCITMMSK,LNSIZEPOS),'X','*')
    FOR LNCOUNT = 1 TO MEMLINES(MSZCROSREF)
      LCLINE = MLINE(MSZCROSREF,LNCOUNT)
      IF SUBSTR(LCLINE,1,3) <> LCSCALE
        LOOP
      ENDIF
      m.ITEM = PADR(SUBSTR(m.ITEM,1,LNSIZEPOS-1)+SUBSTR(LCLINE,AT('~',LCLINE)+1,3),19)

      *E038150,1 AMH Remove IClr from lcTmpSheet [Start]
      *IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
      IF !SEEK(m.TYP+m.CINVTYPC+m.ITEM+m.CINVTYPE+m.CITMMAJOR+m.CITMMASK,LCTMPBOMSH)
        *E038150,1 AMH [End]

        INSERT INTO (LCTMPBOMSH) FROM MEMVAR
      ENDIF
    ENDFOR
  ELSE
    INSERT INTO (LCTMPBOMSH) FROM MEMVAR
  ENDIF
ENDSCAN

IF USED('STYLE')
  SELECT STYLE
  LCSTYFILT = SET('FILTER')
  SET FILTER TO
ENDIF

LCGRADE = ''

*E038150,1 AMH Consider the inventory type [Start]
*IF lcTranType <> 'T'
*  =SEEK(lcItem,'Style')
*  lcGrade = Style.cStyGrade
*ELSE
*  =SEEK(lcItem+lcColor,'Fabric')
*  lcGrade = Fabric.cFabGrade
*ENDIF
=SEEK(LCITEM,LCITEMFILE)
LCGRADE = EVALUATE(LCITEMFILE+'.cStyGrade')
*E038150,1 AMH [End]

*N119813,1 AMH Consider case of Inter-Location PO [Start]
*IF lcTranType = 'I' OR lcTranType = 'D'
IF LCTRANTYPE $ 'IDN'
  *N119813,1 AMH [End]

  =LFUPDPOBOM()
ENDIF
LLMSGDISPD = .F.

SELECT (LCTMPBOMSH)
SCAN
  IF EMPTY(CCATGTYP) .OR. EMPTY(TYP)
    IF !LLMSGDISPD
      *--One or more cost items does not have a proper cost tyoe.
      *--Message : 38171
      =GFMODALGEN("INM38171B00000","ALERT",CITMMASK)
      LLMSGDISPD = .T.
    ENDIF
    LOOP
  ENDIF
  *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
  IF TYP = '8' OR CCATGTYP = 'N'
    LOOP
  ENDIF
  *N000587,1 WAM 12/01/2007 (End)

  *E038150,1 AMH Remove lcColor parameter [Start]
  *IF !LIKE(STRTRAN(IIF(lcTranType='T',LEFT(cItmMask,6),cItmMask),'*','?'),IIF(lcTranType='T',lcColor,lcItem)) .OR. ;
  (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
  (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)
  IF !LIKE(STRTRAN(CITMMASK,'*','?'),LCITEM) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(LCSCALE+'~',MSIZES)=0) .OR. ;
      (!EMPTY(MSZCROSREF) .AND. ATCLINE(LCSCALE+',',MSZCROSREF)=0)
    *E038150,1 AMH [End]

    LOOP
  ENDIF
  LCITEMTYPE = TYP
  IF EMPTY(MSIZES)
    LNTRANQTY = LAQTY[9]

    *E038150,1 AMH Get the correct sizes [Start]
    *lcSizes   = '1,2,3,4,5,6,7,8'
    LCSIZES = ''
    LOCAL LNI,LCI
    FOR LNI = 1 TO LNSCLCNT
      LCI = STR(LNI,1)
      LCSIZES = LCSIZES + IIF(LNI = 1,'',',') + LCI
    ENDFOR
    *E038150,1 AMH [End]

  ELSE
    LCSIZES = SUBSTR(MLINE(MSIZES,ATCLINE(LCSCALE+'~',MSIZES)),5)
    LNTRANQTY = 0
    FOR LNCOUNT = 1 TO 8
      LNTRANQTY = LNTRANQTY + IIF(STR(LNCOUNT,1) $ LCSIZES,LAQTY[lnCount],0)
    ENDFOR
  ENDIF
  STORE '' TO LCCSTCLR,LCCSTITM,LCCMSIZES
  LCCSTITMDYE = SPACE(10)
  DO CASE
  CASE CCATGTYP='S'
    LCCMSIZES='12345678'
    FOR LNCOUNT = 1 TO 8
      LAREQ[lnCount]=LAQTY[lnCount]
    ENDFOR
    LCCSTITM=''
    FOR LNCOUNT = 1 TO LEN(ITEM)
      IF SUBSTR(ITEM,LNCOUNT,1)='*'
        LCCSTITM = LCCSTITM + SUBSTR(LCITEM,LNCOUNT,1)
      ELSE
        LCCSTITM = LCCSTITM + SUBSTR(ITEM,LNCOUNT,1)
      ENDIF
    ENDFOR
    IF !SEEK(LCCSTITM,'Style')
      LOOP
    ENDIF
    LCCSITMSC = STYLE.SCALE
    =SEEK(LCITEM,'Style')
    IF !EMPTY(MSZCROSREF)
      LCCMSIZES = ''
      STORE 0 TO LAREQ
      FOR LNCOUNT = 1 TO MEMLINES(MSZCROSREF)
        LCLINE = MLINE(MSZCROSREF,LNCOUNT)
        IF SUBSTR(LCLINE,1,3) = LCSCALE .AND. SUBSTR(LCLINE,7,3)=LCCSITMSC
          LAREQ[VAL(SUBSTR(lcLine,11,1))] = LAREQ[VAL(SUBSTR(lcLine,11,1))]+;
            LAQTY[VAL(SUBSTR(lcLine,5,1))]
          LCCMSIZES = LCCMSIZES + SUBSTR(LCLINE,11,1)
        ENDIF
      ENDFOR
    ENDIF
    IF M_WAREHOUSE='Y' .AND. !EMPTY(LCSTYWARE) .AND. ;
        !SEEK(LCCSTITM+LCSTYWARE+SPACE(10),'STYDYE')
      *E300725,1 Message : 38029
      *E300725,1 Style xxxxx is not assigned to warehouse xxxx
      *E300725,1 Button : 38001
      *E300725,1 Add Cancel

      *E300935,4 adjust condition to update without message if llBackOnly is .T.
      *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
      IF LLBACKONLY OR GFMODALGEN('QRM38029B38001','ALERT','Style: '+ALLTRIM(LCCSTITM)+'|'+LCSTYWARE) = 1
        DO GPADSTYWAR WITH LCCSTITM,SPACE(10),LCSTYWARE
      ENDIF

    ENDIF
    IF !EMPTY(LCSTYWARE) AND M_DYELOT='Y' .AND. SEEK(LCCSTITM,'Style') .AND. STYLE.CDYE_FLG='Y'

      SELECT (LCDETFILE)

      *E038150,1 AMH Remove IClr from lcDetFile [Start]
      *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
      PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
      *LOCATE REST WHILE ;
      cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
      lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
      PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
      FOR cSizes = STRTRAN(lcSizes,',','')
      =SEEK(LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+EVALUATE(LCTMPBOMSH+'.Typ')+LCINVTYPE+PADR(LCITEM,19)+;
        EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+EVALUATE(LCTMPBOMSH+'.MfgCode'))
      LOCATE REST WHILE ;
        CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
        LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+EVALUATE(LCTMPBOMSH+'.Typ')+LCINVTYPE+PADR(LCITEM,19)+;
        EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+EVALUATE(LCTMPBOMSH+'.MfgCode') ;
        FOR CSIZES = STRTRAN(LCSIZES,',','')
      *E038150,1 AMH [End]

      LCCSTITMDYE = IIF(FOUND(),&LCDETFILE..DYELOT,LCDYELOT)

      IF SEEK(LCCSTITM+LCSTYWARE+SPACE(10),'STYDYE') AND ;
          (EMPTY(LCCSTITMDYE) OR !SEEK(LCCSTITM+LCSTYWARE+LCCSTITMDYE,'StyDye'))
        *E300935,4 Adjust calling with background parameter (llBackOnly)
        *E300935,4 =gfSelDyelot(lcTranType,'S',lcItem,lcColor,lcStyWare,@lcCstItmDye,lcCstItm)

        *N119813,1 AMH Consider case of configuration [Start]
        *=gfSelDyelot(lcTranType,'S',lcItem,lcColor,lcStyWare,@lcCstItmDye,lcCstItm,'','',llBackOnly)

        *E038150,1 AMH Consider the inventory type [Start]
        *=gfSelDyelot(lcTranType,'S',lcItem,lcInvType,lcStyWare,@lcCstItmDye,lcCstItm,'','',llBackOnly,llStyConfg)
        =GFSELDYELOT(LCTRANTYPE,'S',LCITEM,LCINVTYPE,LCSTYWARE,@LCCSTITMDYE,LCCSTITM,'0001','',LLBACKONLY,LLSTYCONFG)
        *E038150,1 AMH [End]

        *N119813,1 AMH [End]

      ENDIF
    ENDIF

  CASE INLIST(CCATGTYP,'F','T')

    *E038150,1 AMH Remove IClr from lcTmpBomSh [Start]
    *lcCstItm = SUBSTR(Item,1,7)
    *IF IClr = '******'
    *  lcCstClr = IIF(lcTranType='T',lcColor,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
    *ELSE
    *  lcCstClr = IClr
    *ENDIF
    *IF (cCatGTyp='F' OR Trim_Invt) AND !SEEK(lcCstItm+lcCstClr,'FABRIC')
    *  LOOP
    *ENDIF
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save (Commented out)
    *!*	      LOCAL lnItmClrPos,lnItmClrLen,lcItemWidth
    *!*	      lcItemWidth = ''
    *!*	      =gfItemMask(@laItemSeg,"",cInvTypC)
    *!*	      FOR lnCount = 1 TO ALEN(laItemSeg,1)
    *!*	        IF laItemSeg[lnCount,1]='C'
    *!*	          lnItmClrLen = LEN(laItemSeg[lnCount,3])
    *!*	          lnItmClrPos = laItemSeg[lnCount,4]
    *!*	        ENDIF
    *!*	      ENDFOR
    *!*	      lcCmSizes='12345678'
    *!*	      FOR lnCount = 1 TO 8
    *!*	        laReq[lnCount]=laQty[lnCount]
    *!*	      ENDFOR
    *!*	      IF SUBSTR(item,lnItmClrPos,lnItmClrLen) = '*'
    *!*	        lcCstItm = STUFF(item,lnItmClrPos,lnItmClrLen,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
    *!*	      ELSE
    *!*	        lcCstItm = item
    *!*	      ENDIF
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save (Commented out) End

    IF CCATGTYP='F' OR TRIM_INVT

      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save
      LOCAL LNITMCLRPOS,LNITMCLRLEN,LCITEMWIDTH
      LCITEMWIDTH = ''
      =GFITEMMASK(@LAITEMSEG,"",CINVTYPC)
      FOR LNCOUNT = 1 TO ALEN(LAITEMSEG,1)
        IF LAITEMSEG[lnCount,1]='C'
          LNITMCLRLEN = LEN(LAITEMSEG[lnCount,3])
          LNITMCLRPOS = LAITEMSEG[lnCount,4]
        ENDIF
      ENDFOR
      LCCMSIZES='12345678'
      FOR LNCOUNT = 1 TO 8
        LAREQ[lnCount]=LAQTY[lnCount]
      ENDFOR
      *B609533,1 SAB 02/22/2011 erro prevents adding material to PO cost sheet when  color starts with * [Start]
      *IF SUBSTR(item,lnItmClrPos,lnItmClrLen) = '*'
      IF SUBSTR(ITEM,LNITMCLRPOS,LNITMCLRLEN) = REPLICATE('*',LNITMCLRLEN)
        *B609533,1 SAB 02/22/2011 erro prevents adding material to PO cost sheet when  color starts with * [End]
        LCCSTITM = STUFF(ITEM,LNITMCLRPOS,LNITMCLRLEN,SUBSTR(LCITEM,LNCLRPOS,LEN(LCCLRMSK)))
      ELSE
        LCCSTITM = ITEM
      ENDIF
      *B607915,1 MMT 12/28/2006 bug of error in Po screen while save  (End)

      LOCAL LNCURALIAS
      LNCURALIAS = SELECT(0)
      IF GFOPNSQLFL('item',"TMPFABRIC","cinvtype = '"+CINVTYPC+"' and style = '"+LCCSTITM+"'","","style")
        SELECT TMPFABRIC
        LOCATE
        IF EOF()
          USE IN TMPFABRIC
          SELECT (LNCURALIAS)
          LOOP
        ENDIF
        LCCSITMSC   = SCALE
        LCITEMWIDTH = CITEMFLD1
        USE IN TMPFABRIC
        SELECT (LNCURALIAS)
        IF !EMPTY(MSZCROSREF)
          LCCMSIZES = ''
          STORE 0 TO LAREQ
          FOR LNCOUNT = 1 TO MEMLINES(MSZCROSREF)
            LCLINE = MLINE(MSZCROSREF,LNCOUNT)
            IF SUBSTR(LCLINE,1,3) = LCSCALE .AND. SUBSTR(LCLINE,7,3)=LCCSITMSC
              LAREQ[VAL(SUBSTR(lcLine,11,1))] = LAREQ[VAL(SUBSTR(lcLine,11,1))]+;
                LAQTY[VAL(SUBSTR(lcLine,5,1))]
              LCCMSIZES = LCCMSIZES + SUBSTR(LCLINE,11,1)
            ENDIF
          ENDFOR
        ENDIF
      ELSE
        LOOP
      ENDIF
    ENDIF
    *IF (cCatGTyp='F' .OR. Trim_Invt) .AND. M_WAREHOUSE='Y' .AND. ;
    !EMPTY(lcMatWare) .AND. ;
    !SEEK(lcCstItm+lcCstClr+lcMatWare+SPACE(10),'FABDYE')
    *  *E300725,1 Message : 38029
    *  *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
    *  *E300725,1 Button : 38001
    *  *E300725,1 Add Cancel
    *
    *  *E300935,4 adjust condition to update without message if llBackOnly is .T.
    *  *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
    *  IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
    *    DO gpAdFabWar WITH lcCstItm,lcCstClr,SPACE(10),lcMatWare
    *  ENDIF
    *ENDIF
    IF (CCATGTYP='F' .OR. TRIM_INVT) .AND. M_WAREHOUSE='Y' .AND. !EMPTY(LCMATWARE)
      LOCAL LNCURALIAS
      LNCURALIAS = SELECT(0)
      IF GFOPNSQLFL('itemloc',"TMPFABDYE","cinvtype = '"+CINVTYPC+"' and style = '"+LCCSTITM+;
          "' and cwarecode = '"+LCMATWARE+"' and dyelot = '"+SPACE(10)+"'","","stydye")
        SELECT TMPFABDYE
        LOCATE
        IF EOF()
          SELECT (LNCURALIAS)
          *E300725,1 Message : 38029
          *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
          *E300725,1 Button : 38001
          *E300725,1 Add Cancel

          *E300935,4 adjust condition to update without message if llBackOnly is .T.
          *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
          IF LLBACKONLY OR GFMODALGEN('QRM38029B38001','ALERT','Item: '+ALLTRIM(LCCSTITM)+'|'+LCMATWARE) = 1
            DO GFADITEMWAR WITH CINVTYPC, LCCSTITM, SPACE(10), LCMATWARE
          ENDIF
        ENDIF
        USE IN TMPFABDYE
        SELECT (LNCURALIAS)
      ELSE
        *E300725,1 Message : 38029
        *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel

        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
        IF LLBACKONLY OR GFMODALGEN('QRM38029B38001','ALERT','Item: '+ALLTRIM(LCCSTITM)+'|'+LCMATWARE) = 1
          DO GFADITEMWAR WITH CINVTYPC, LCCSTITM, SPACE(10), LCMATWARE
        ENDIF
      ENDIF
    ENDIF
    *IF !EMPTY(lcMatWare) AND (cCatGTyp='F' OR Trim_Invt) AND M_MATDYE='Y' AND ;
    *   SEEK(lcCstItm+lcCstClr,'Fabric') .AND. Fabric.cDye_Flg='Y'
    *
    *  SELECT (lcDetFile)
    *
    *  *N119813,1 AMH Consider case of style use configuration [Start]
    *  IF !llStyconfg
    *  *N119813,1 AMH [End]
    *
    *  =SEEK(lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
    *  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
    *  LOCATE REST WHILE ;
    *  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
    *  lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
    *  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
    *  FOR cSizes = STRTRAN(lcSizes,',','')
    *
    *  lcCstItmDye = IIF(FOUND(),&lcDetFile..Dyelot,lcDyelot)
    *
    *  *N119813,1 AMH Consider case of style use configuration [Start]
    *  ENDIF
    *  *N119813,1 AMH [End]
    *
    *  IF SEEK(lcCstItm+lcCstClr+lcMatWare+SPACE(10),'FABDYE') AND ;
    *    (EMPTY(lcCstItmDye) OR !SEEK(lcCstItm+lcCstClr+lcMatWare+lcCstItmDye,'FabDye'))
    *
    *    *E300935,4 Adjust calling with background parameter (llBackOnly)
    *    *E300935,4 =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName)
    *    =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName,llBackOnly)
    *  ENDIF
    *ENDIF
    IF !EMPTY(LCMATWARE) AND (CCATGTYP='F' OR TRIM_INVT) AND M_MATDYE='Y'
      LOCAL LNCURALIAS
      LNCURALIAS = SELECT(0)
      IF GFOPNSQLFL('item',"TMPFABRIC","cinvtype = '"+CINVTYPC+"' and style = '"+LCCSTITM+"'","","style")
        SELECT TMPFABRIC
        LOCATE
        IF !EOF() AND CDYE_FLG = 'Y'
          SELECT (LCDETFILE)
          IF !LLSTYCONFG
            =SEEK(LCTRANLETT +'1'+LCTICKETNO+STR(LNLINENO,6)+EVALUATE(LCTMPBOMSH+'.Typ')+LCINVTYPE+PADR(LCITEM,19)+;
              EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+EVALUATE(LCTMPBOMSH+'.MfgCode'))
            LOCATE REST WHILE CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
              LCTRANLETT +'1'+LCTICKETNO+STR(LNLINENO,6)+EVALUATE(LCTMPBOMSH+'.Typ')+LCINVTYPE+;
              PADR(LCITEM,19)+EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+;
              EVALUATE(LCTMPBOMSH+'.MfgCode') FOR CSIZES = STRTRAN(LCSIZES,',','')
            LCCSTITMDYE = IIF(FOUND(),DYELOT,LCDYELOT)
          ENDIF
          IF GFOPNSQLFL('itemloc',"TMPFABDYE","cinvtype = '"+CINVTYPC+"' and style = '"+LCCSTITM+;
              "' and cwarecode = '"+LCMATWARE+"'","","stydye")
            SELECT TMPFABDYE
            LOCATE FOR DYELOT = SPACE(10)
            IF FOUND()
              IF EMPTY(LCCSTITMDYE)
                =GFSELDYELOT(LCTRANTYPE,'F',LCITEM,LCINVTYPE,LCMATWARE,@LCCSTITMDYE,LCCSTITM,EVALUATE(LCTMPBOMSH+'.cInvTypC'),LCTMPNAME,LLBACKONLY)
              ELSE
                LOCATE FOR DYELOT = LCCSTITMDYE
                IF !FOUND()
                  =GFSELDYELOT(LCTRANTYPE,'F',LCITEM,LCINVTYPE,LCMATWARE,@LCCSTITMDYE,LCCSTITM,EVALUATE(LCTMPBOMSH+'.cInvTypC'),LCTMPNAME,LLBACKONLY)
                ENDIF
              ENDIF
            ENDIF
            USE IN TMPFABDYE
          ENDIF
        ENDIF
        USE IN TMPFABRIC
        SELECT (LNCURALIAS)
      ENDIF
    ENDIF
    *E038150,1 AMH [End]

  CASE CCATGTYP='M'
    =GFRLTFLD(&LCTMPBOMSH..MFGCODE,@LAMFGRFLD,'MFGCODE')
    *C200080,1 AMM Adjust to fit the new type 'D' for dye order
    *IF llMfgOpr .AND. !SEEK(lcTranType+lcTicketNo+&lcTmpBomSh..MfgCode,lcOprHdr)
    IF LLMFGOPR .AND. !SEEK(LCTRANLETT+LCTICKETNO+&LCTMPBOMSH..MFGCODE,LCOPRHDR)
      *C200080,1 AMM End
      LCOPERSEQ = LEFT(LCOPERSEQ,2)
      IF VAL(LCOPERSEQ) > LNLASTSEQ
        LNLASTSEQ = VAL(LCOPERSEQ)
        LCLASTOPR = &LCTMPBOMSH..MFGCODE
      ENDIF

      INSERT INTO (LCOPRHDR) (CIMTYP,CTKTNO,COPRCODE,COPERSEQ,CCONTCODE,CCONTNAME,LINHOUSE,NNXTLOTNO);
        VALUES (LCTRANLETT,LCTICKETNO,&LCTMPBOMSH..MFGCODE,LCOPERSEQ,LCCONTCODE,LCCONTNAME,LLINHOUSE,1)

    ENDIF
    IF LCTRANTYPE = 'I'
      LCEXSIGN = GFGETEXSIN(@LCUNTSIN,LCDUTYCUR)
    ENDIF
  CASE CCATGTYP='P'
    LCEXSIGN = GFGETEXSIN(@LCUNTSIN,LCPRICECUR)
  CASE !INLIST(CCATGTYP,'S','F','T') AND LCTRANTYPE = 'I'
    LCEXSIGN = GFGETEXSIN(@LCUNTSIN,LCDUTYCUR)
  ENDCASE
  SELECT (LCTKTSHEET)

  *E038150,1 AMH Remove IClr from lcTktSheet and lcDetFile [Start]
  *IF !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+;
  PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye)
  IF !SEEK(LCTRANLETT+LCTICKETNO+EVALUATE(LCTMPBOMSH+'.Typ')+EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+;
      EVALUATE(LCTMPBOMSH+'.MfgCode')+LCCSTITMDYE)
    APPEND BLANK
    *REPLACE CutTkt    WITH lcTicketNo    ,;
    cIMTyp    WITH lcTranLett    ,;
    cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
    TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
    cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
    (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
    Link_Code WITH lcLinkCode            ,;
    cOprCode  WITH &lcTmpBomSh..cOprCode ,;
    TYP       WITH &lcTmpBomSh..Typ      ,;
    ITEM      WITH lcCstItm              ,;
    ICLR      WITH lcCstClr              ,;
    MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
    cOprCode  WITH &lcTmpBomSh..cOprCode ,;
    DESC      WITH &lcTmpBomSh..Desc     ,;
    Dyelot    WITH lcCstItmDye           ,;
    UOM       WITH &lcTmpBomSh..UOM      ,;
    DATE      WITH oAriaApplication.SystemDate  ,;
    WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.WIDTH,'')

    *E038220,1 WSH Change UOM-Code to Relation Code [Start]
    *REPLACE CutTkt    WITH lcTicketNo    ,;
    cIMTyp    WITH lcTranLett    ,;
    cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
    TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
    cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
    (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
    Link_Code WITH lcLinkCode            ,;
    cOprCode  WITH &lcTmpBomSh..cOprCode ,;
    TYP       WITH &lcTmpBomSh..Typ      ,;
    cInvType  WITH EVALUATE(lcTmpBomSh+'.cInvTypC'),;
    ITEM      WITH lcCstItm              ,;
    MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
    cOprCode  WITH &lcTmpBomSh..cOprCode ,;
    DESC      WITH &lcTmpBomSh..Desc     ,;
    Dyelot    WITH lcCstItmDye           ,;
    UOM       WITH &lcTmpBomSh..UOM      ,;
    DATE      WITH oAriaApplication.SystemDate  ,;
    WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.CITEMFLD1,'')
    REPLACE CUTTKT    WITH LCTICKETNO    ,;
      CIMTYP    WITH LCTRANLETT    ,;
      CCATGTYP  WITH &LCTMPBOMSH..CCATGTYP  ,;
      TRIM_INVT WITH &LCTMPBOMSH..TRIM_INVT ,;
      CWARECODE WITH IIF(CCATGTYP='S',LCSTYWARE,IIF(CCATGTYP='F' OR ;
      (CCATGTYP='T' AND TRIM_INVT),LCMATWARE,'')) ,;
      LINK_CODE WITH LCLINKCODE            ,;
      COPRCODE  WITH &LCTMPBOMSH..COPRCODE ,;
      TYP       WITH &LCTMPBOMSH..TYP      ,;
      CINVTYPE  WITH EVALUATE(LCTMPBOMSH+'.cInvTypC'),;
      ITEM      WITH LCCSTITM              ,;
      MFGCODE   WITH &LCTMPBOMSH..MFGCODE  ,;
      COPRCODE  WITH &LCTMPBOMSH..COPRCODE ,;
      DESC      WITH &LCTMPBOMSH..DESC     ,;
      DYELOT    WITH LCCSTITMDYE           ,;
      CUOMCODE  WITH &LCTMPBOMSH..CUOMCODE ,;
      DATE      WITH OARIAAPPLICATION.SYSTEMDATE  ,;
      WIDTH     WITH IIF(CCATGTYP='F' OR (CCATGTYP='T' AND TRIM_INVT),LCITEMWIDTH,'')
    *E038220,1 WSH [End]

    IF LCTRANTYPE = 'M'
      REPLACE CMARKER WITH &LCTMPBOMSH..CMARKER
    ENDIF
    *B608180,1 TMI [Start] if the first operation in the sequence is recorded then put it in the tktSheet temp file to be issued when the cost sheet is saved
  ELSE
    *B608223,1 TMI [Start] check if called from lfvGenerate
    IF LLSHEETGEN
      *B608223,1 TMI [End  ]
      LOCAL LCSVORD
      LCSVORD = ORDER(LCOPRHDR)
      SET ORDER TO &LCOPRHDR IN &LCOPRHDR
      GO TOP IN (LCOPRHDR)
      IF !EOF(LCOPRHDR) .AND. !EMPTY(&LCTMPBOMSH..COPRCODE) .AND. &LCTMPBOMSH..COPRCODE == &LCOPRHDR..COPRCODE
        REPLACE COPRCODE WITH &LCOPRHDR..COPRCODE
      ENDIF
      SET ORDER TO &LCSVORD IN &LCOPRHDR
      *B608223,1 TMI [Start]
    ENDIF
    *B608223,1 TMI [End  ]
    *B608180,1 TMI [End  ]

  ENDIF

  SELECT (LCDETFILE)
  *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  *LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  =SEEK(LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
    EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE)
  LOCATE REST WHILE ;
    CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
    LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
    EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE ;
    FOR CSIZES = STRTRAN(LCSIZES,',','')
  *E038150,1 AMH [Start]

  LLFOUNDREC = FOUND()
  SELECT (LCTKTSHEET)

  *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
  *!*	  IF !llFoundRec
  *!*	    REPLACE Pieces  WITH Pieces  + lnTranQty ,;
  *!*	          Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
  *!*	          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
  *!*	    lnUntQty = &lcTmpBomSh..nBomTotQty
  *!*	  ELSE
  *!*	    *B608376,1 WAM 12/09/2007 Fix calculation of CT cost sheet required quantity.
  *!*	    *REPLACE Pieces  WITH Pieces  + lnTranQty ,;
  *!*	          Req_Qty WITH Req_Qty + lnTranQty * &lcDetFile..UnitQty ,;
  *!*	          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
  *!*	    *lnUntQty = &lcDetFile..UnitQty
  *!*
  *!*	    REPLACE Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
  *!*	            UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
  *!*	    lnUntQty = &lcTmpBomSh..nBomTotQty
  *!*	    *B608376,1 WAM (End)
  *!*
  *!*	  ENDIF
  SELECT (LCCOSTELEM)
  
  *B611686,1 SAH 10/28/2018 modify function gfSheetItem as the Cutting cost sheet calculated the Qty incorrectly When item is repeated more than once in Style cost sheet [BEGIN]
  
*!*	  =SEEK('A'+&LCTMPBOMSH..TYP+LCCSTITM+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE)
*!*	  LOCATE REST WHILE CTRTYPE+CBOMTYP +ITEM+MFGCODE+CDYELOT = 'A'+&LCTMPBOMSH..TYP+LCCSTITM+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE ;
*!*	    FOR CSIZES =STRTRAN(LCSIZES,',','')

=SEEK('A'+&LCTMPBOMSH..TYP+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE)
  LOCATE REST WHILE CTRTYPE+CBOMTYP +ITEM+MFGCODE+CDYELOT = 'A'+&LCTMPBOMSH..TYP+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE ;
    FOR CSIZES =STRTRAN(LCSIZES,',','')

  *B611686,1 SAH 10/28/2018 modify function gfSheetItem as the Cutting cost sheet calculated the Qty incorrectly When item is repeated more than once in Style cost sheet [END]

  IF !FOUND()
    REPLACE PIECES  WITH PIECES  + LNTRANQTY IN (LCTKTSHEET)
    INSERT INTO (LCCOSTELEM) (CTRTYPE,CBOMTYP ,ITEM, MFGCODE,CDYELOT,CSIZES);
      VALUES ('A',&LCTMPBOMSH..TYP,LCCSTITM,&LCTMPBOMSH..MFGCODE,LCCSTITMDYE,STRTRAN(LCSIZES,',',''))
  ENDIF
  SELECT (LCTKTSHEET)
  REPLACE REQ_QTY WITH REQ_QTY + LNTRANQTY * &LCTMPBOMSH..NBOMTOTQTY ,;
    UNTQTY  WITH IIF(PIECES<>0,REQ_QTY/PIECES,0)
  LNUNTQTY = &LCTMPBOMSH..NBOMTOTQTY
  *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]
  *--If tranQty was zero take it from pieces.
  *--to make sure that the cost was updated.
  *--Read lnTotCost and lnFTotCost depend on lnTTranQty insted of lnTranQty.
  LNTTRANQTY = LNTRANQTY
  IF LNTRANQTY = 0
    LNTTRANQTY = PIECES
  ENDIF

  *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[Start]
  IF LNTRANQTY = 0 AND LLFOUNDREC
    LNTTRANQTY  = EVALUATE(LCDETFILE+'.StyQty')
  ENDIF
  *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[End]

  DO CASE
  CASE CCATGTYP = 'P'
    *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
    *lnTotCost  = lnPrice * lnTTranQty &lcExSign lnPriceRate &lcUntSin lnPriceUnit
    *wael
    *lcExSign   = gfGetExSin(@lcUntSin, IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode))
    *lnExRate   = IIF(ISNULL(&lcTmpBomSh..nExRate) OR &lcTmpBomSh..nExRate=0,1,&lcTmpBomSh..nExRate)
    *lnCurrUnit = IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)

    LCEXSIGN   = GFGETEXSIN(@LCUNTSIN, LCPRICECUR)
    LNEXRATE   = LNPRICERATE
    LNCURRUNIT = LNPRICEUNIT
    *wael

    LNTOTCOST  = LNPRICE * LNTTRANQTY &LCEXSIGN LNEXRATE &LCUNTSIN LNCURRUNIT
    *N000587,1 WAM 12/01/2007 (End)
    LNFTOTCOST = LNPRICE * LNTTRANQTY
  CASE &LCTMPBOMSH..NPERCENT > 0
    LNTOTCOST  = LNPRICE*LNTTRANQTY*(&LCTMPBOMSH..NPERCENT/100) &LCEXSIGN LNPRICERATE &LCUNTSIN LNPRICEUNIT
    LNFTOTCOST = LNPRICE*LNTTRANQTY*(&LCTMPBOMSH..NPERCENT/100)
  CASE CCATGTYP = 'S'
    *! B609498,1 MMT 01/13/2010 Fix bug of wrong estimated cost for style comp. in PO Screen[Start]
    *Seek in Style file for the Comp. style record
    =SEEK(LCCSTITM,'Style')
    *! B609498,1 MMT 01/13/2010 Fix bug of wrong estimated cost for style comp. in PO Screen[End]
    LNTOTCOST  = STYLE.TOTCOST * LNTTRANQTY * &LCTKTSHEET..UNTQTY
    LNFTOTCOST = LNTOTCOST
  CASE CCATGTYP = 'F' OR (CCATGTYP='T' AND TRIM_INVT)

    *E038150,1 AMH Consider the inventory type [Start]
    *lnTotCost  = Fabric.CostBuy/Fabric.Conv * lnTTranQty * lnUntQty
    LOCAL LNCURALIAS
    LNCURALIAS = SELECT(0)
    IF GFOPNSQLFL('item',"TMPFABRIC","cinvtype = '"+EVALUATE(LCTMPBOMSH+'.cInvTypC')+"' and style = '"+LCCSTITM+"'","","style")
      SELECT TMPFABRIC
      LOCATE
      IF !EOF()

        *E038220,1 WSH Get Conversion Factor from UOM SQL file. [Start]
        *lnTotCost  = TotCost * lnTTranQty * lnUntQty
        LOCAL LNCONF
        LNCONF    = 1
        =GFGETUOMDATA(EVALUATE(LCTMPBOMSH + '.CUOMCODE'), '', '', @LNCONF)

        *B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
        *lnTotCost  = TotCost / lnConf * lnTTranQty * lnUntQty
        IF LLMULCURR
          STORE 0 TO LNIECOST1, LNIECOST2, LNIECOST3, LNIECOST4, LNIECOST5, LNIECOST6, LNIECOST7
          STORE 1 TO LNCURRUNIT, LNEXRATE
          LNEXRATE  = GFCHKRATE('lnCurrUnit',TMPFABRIC.CPRICECUR,OARIAAPPLICATION.SYSTEMDATE,.T.,OARIAAPPLICATION.ACTIVECOMPANYID,.F.,.T.)
          LCEXSIN2  = ''
          LCEXSIN1  = GFGETEXSIN(@LCEXSIN2,TMPFABRIC.CPRICECUR)
          LNIECOST1 = TMPFABRIC.NICOST1 &LCEXSIN1 LNEXRATE &LCEXSIN2 LNCURRUNIT

          STORE 1 TO LNCURRUNIT, LNEXRATE
          LNEXRATE  = GFCHKRATE('lnCurrUnit',TMPFABRIC.CDUTYCUR,OARIAAPPLICATION.SYSTEMDATE,.T.,OARIAAPPLICATION.ACTIVECOMPANYID,.F.,.T.)
          LCEXSIN2  = ''
          LCEXSIN1  = GFGETEXSIN(@LCEXSIN2,TMPFABRIC.CDUTYCUR)
          FOR LNCOUNT = 2 TO 7
            LCCOUNT = STR(LNCOUNT,1)
            LNIECOST&LCCOUNT = EVALUATE('TMPFABRIC.nICost'+LCCOUNT) &LCEXSIN1 LNEXRATE &LCEXSIN2 LNCURRUNIT
          ENDFOR
          LNTOTCOST = (LNIECOST1 + LNIECOST2 + LNIECOST3 + LNIECOST4 + LNIECOST5 + LNIECOST6 + LNIECOST7 ) / LNCONF * LNTTRANQTY * LNUNTQTY
        ELSE
          LNTOTCOST  = TOTCOST / LNCONF * LNTTRANQTY * LNUNTQTY
        ENDIF
        *B608542,1 WAM 08/14/2008 (End)

        *E038220,1 WSH [End]

      ENDIF
      USE IN TMPFABRIC
      SELECT (LNCURALIAS)
    ELSE
      LNTOTCOST = 0
    ENDIF
    *E038150,1 AMH [End]

    LNFTOTCOST = LNTOTCOST
    *N000587,1 HBG 2/22/2007 Specify the ccatgtyp to ignor non costing elements [Begin]
    *OTHERWISE
  *B610809,1 MMT 08/18/2014 Cutting ticket cost sheet reads trim cost incorrectly from style cost sheet[T20140804.0032][Start]  
  *CASE CCATGTYP = 'M' OR CCATGTYP='D'
  CASE CCATGTYP = 'M' OR CCATGTYP='D' OR (CCATGTYP='T' AND !TRIM_INVT) 
  *B610809,1 MMT 08/18/2014 Cutting ticket cost sheet reads trim cost incorrectly from style cost sheet[T20140804.0032][END]
    *N000587,1 [End]
    *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
    *lnTotCost  = &lcTmpBomSh..TotCost * lnTTranQty &lcExSign lnDutyRate &lcUntSin lnDutyUnit
    LCEXSIGN  = GFGETEXSIN(@LCUNTSIN, IIF(ISNULL(&LCTMPBOMSH..CCURRCODE) OR EMPTY(&LCTMPBOMSH..CCURRCODE),OARIAAPPLICATION.BASECURRENCY,&LCTMPBOMSH..CCURRCODE))
    LNEXRATE   = IIF(ISNULL(&LCTMPBOMSH..NEXRATE)   OR &LCTMPBOMSH..NEXRATE=0,1,&LCTMPBOMSH..NEXRATE)
    LNCURRUNIT = IIF(ISNULL(&LCTMPBOMSH..NCURRUNIT) OR &LCTMPBOMSH..NCURRUNIT=0,1,&LCTMPBOMSH..NCURRUNIT)
    LNTOTCOST  = &LCTMPBOMSH..TOTCOST * LNTTRANQTY &LCEXSIGN LNEXRATE &LCUNTSIN LNCURRUNIT
    *N000587,1 WAM 12/01/2007 (End)
    LNFTOTCOST = &LCTMPBOMSH..TOTCOST * LNTTRANQTY
  ENDCASE
  IF LNTRANQTY=0
    *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[Start]
    *REPLACE Est_Cost WITH lnTotCost
    IF LNTRANQTY = 0 AND LLFOUNDREC
      REPLACE EST_COST WITH EST_COST +LNTOTCOST -(EVALUATE(LCDETFILE+'.StyQty')*EVALUATE(LCDETFILE+'.unitcost'))
    ELSE
      REPLACE EST_COST WITH LNTOTCOST
    ENDIF
    *B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[End]
  ELSE
    REPLACE EST_COST WITH EST_COST + LNTOTCOST
  ENDIF
  REPLACE UNTCOST  WITH IIF(REQ_QTY<>0,EST_COST/REQ_QTY,0)

  LNEST&LCITEMTYPE = LNEST&LCITEMTYPE  + LNTOTCOST
  LNFEST&LCITEMTYPE= LNFEST&LCITEMTYPE + LNFTOTCOST

  *E038150,1 AMH Consider the fabric and trim since it has sizes [Start]
  *IF &lcTmpBomSh..cCatGTyp = 'S'
  IF EVALUATE(LCTMPBOMSH+'.cCatGTyp') $ 'SF' OR;
      (EVALUATE(LCTMPBOMSH+'.cCatGTyp') = 'T' AND EVALUATE(LCTMPBOMSH+'.TRIM_INVT'))
    *E038150,1 AMH [End]

    FOR LNCOUNT = 1 TO 8
      LCCOUNT = STR(LNCOUNT,1)
      *B608376,1 WAM 12/09/2007 Fix calculation of CT cost sheet required quantity.
      *REPLACE REQ_QTY&lcCount WITH REQ_QTY&lcCount+laReq[lnCount]*&lcTktSheet..UntQty
      REPLACE REQ_QTY&LCCOUNT WITH REQ_QTY&LCCOUNT+LAREQ[lnCount]*LNUNTQTY
      *B608376,1 WAM 12/09/2007 (End)
    ENDFOR
  ENDIF


  *! B610314,1 HIA 04/21/2013 T20130218.0012 - PO - automatic issue on PO costs sheet not issuing all components [Start]
  *REPLACE cCompSizes WITH lcCmSizes
  REPLACE CCOMPSIZES WITH IIF(!EMPTY(ALLTRIM(CCOMPSIZES)),ALLTRIM(CCOMPSIZES)+LCCMSIZES,LCCMSIZES)
  *! B610314,1 HIA 04/21/2013 T20130218.0012 - PO - automatic issue on PO costs sheet not issuing all components [End]



  SELECT (LCTKTSHEET)
  LOCAL LLEDTFLD,LLADDFLD
  LLEDTFLD = (TYPE(LCTKTSHEET+'.cEdit_USER') <> 'U') .AND. (TYPE(LCTKTSHEET+'.dEdit_Date') <> 'U') .AND. (TYPE(LCTKTSHEET+'.cEdit_Time') <> 'U');
    AND (TYPE(LCTKTSHEET+'.cEdt_Ver') <> 'U')
  LLADDFLD = (TYPE(LCTKTSHEET+'.cAdd_user') <> 'U') AND (TYPE(LCTKTSHEET+'.dAdd_Date') <> 'U') AND (TYPE(LCTKTSHEET+'.cAdd_Time') <> 'U') ;
    AND (TYPE(LCTKTSHEET+'.cAdd_Ver') <> 'U') AND EMPTY(CADD_USER)

  *-- New Record
  IF LLADDFLD
    *** stamp the record for this user with date and time
    REPLACE CADD_USER  WITH OARIAAPPLICATION.USER_ID ,;
      DADD_DATE  WITH DATE()    ,;
      CADD_TIME  WITH GFGETTIME(),;
      CADD_VER   WITH OARIAAPPLICATION.CSHORTVERSION
  ELSE && Modified Record
    IF LLEDTFLD
      REPLACE CEDIT_USER  WITH OARIAAPPLICATION.USER_ID ,;
        DEDIT_DATE  WITH DATE()    ,;
        CEDIT_TIME  WITH GFGETTIME(),;
        CEDT_VER    WITH OARIAAPPLICATION.CSHORTVERSION
    ENDIF
  ENDIF

  IF PIECES = 0 AND !LLBACKONLY
    DELETE
  ENDIF
  SELECT (LCTMPBOMSH)
  DO CASE
  CASE &LCTMPBOMSH..CCATGTYP = 'P'
    LNUNITPRI = LNPRICE
  CASE INLIST(&LCTMPBOMSH..CCATGTYP,'M','D')
    IF &LCTMPBOMSH..NPERCENT > 0
      LNUNITPRI  = LNPRICE*(&LCTMPBOMSH..NPERCENT/100)
    ELSE
      LNUNITPRI = &LCTMPBOMSH..UNTCOST
    ENDIF

    *E038150,1 AMH Consider the inventory type [Start]
    *CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T') .AND. SEEK(SUBSTR(lcCstItm,1,7)+lcCstClr,'Fabric')
    *lnUnitPri = Fabric.CostBuy/Fabric.Conv
  CASE INLIST(&LCTMPBOMSH..CCATGTYP,'F','T')
    LOCAL LNCURALIAS
    LNCURALIAS = SELECT(0)

    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
    LNUNITPRI = &LCTKTSHEET..UNTCOST
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]

    IF GFOPNSQLFL('item',"TMPFABRIC","cinvtype = '"+EVALUATE(LCTMPBOMSH+'.cInvTypC')+"' and style = '"+LCCSTITM+"'","","style")
      SELECT TMPFABRIC
      LOCATE
      IF !EOF()

        *E038220,1 WSH Get Conversion Factor from UOM SQL file. [Start]
        *lnUnitPri = TotCost
        LOCAL LNCONF
        LNCONF    = 1

        =GFGETUOMDATA(EVALUATE(LCTMPBOMSH + '.CUOMCODE'), '', '', @LNCONF)

        *B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates
        *lnUnitPri = TotCost / lnConf
        IF LLMULCURR
          STORE 0 TO LNIECOST1, LNIECOST2, LNIECOST3, LNIECOST4, LNIECOST5, LNIECOST6, LNIECOST7
          STORE 1 TO LNCURRUNIT, LNEXRATE
          LNEXRATE  = GFCHKRATE('lnCurrUnit',TMPFABRIC.CPRICECUR,OARIAAPPLICATION.SYSTEMDATE,.T.,OARIAAPPLICATION.ACTIVECOMPANYID,.F.,.T.)
          LCEXSIN2  = ''
          LCEXSIN1  = GFGETEXSIN(@LCEXSIN2,TMPFABRIC.CPRICECUR)
          LNIECOST1 = TMPFABRIC.NICOST1 &LCEXSIN1 LNEXRATE &LCEXSIN2 LNCURRUNIT

          STORE 1 TO LNCURRUNIT, LNEXRATE
          LNEXRATE  = GFCHKRATE('lnCurrUnit',TMPFABRIC.CDUTYCUR,OARIAAPPLICATION.SYSTEMDATE,.T.,OARIAAPPLICATION.ACTIVECOMPANYID,.F.,.T.)
          LCEXSIN2  = ''
          LCEXSIN1  = GFGETEXSIN(@LCEXSIN2,TMPFABRIC.CDUTYCUR)
          FOR LNCOUNT = 2 TO 7
            LCCOUNT = STR(LNCOUNT,1)
            LNIECOST&LCCOUNT = EVALUATE('TMPFABRIC.nICost'+LCCOUNT) &LCEXSIN1 LNEXRATE &LCEXSIN2 LNCURRUNIT
          ENDFOR
          LNUNITPRI = (LNIECOST1 + LNIECOST2 + LNIECOST3 + LNIECOST4 + LNIECOST5 + LNIECOST6 + LNIECOST7 ) / LNCONF
        ELSE
          LNUNITPRI = TOTCOST / LNCONF
        ENDIF
        *B608542,1 WAM 08/14/2008 (End)

        *E038220,1 WSH [End]

      ENDIF
      USE IN TMPFABRIC
      SELECT (LNCURALIAS)
    ENDIF
    *E038150,1 AMH [End]

    *N000587,1 HBG 2/22/2007 Specify the ccatgtyp to ignor non costing elements [Begin]
    *OTHERWISE
  CASE CCATGTYP = 'S'
    *N000587,1 [End]
    LNUNITPRI = &LCTKTSHEET..UNTCOST
  ENDCASE
  SELECT (LCDETFILE)
  *B608180,1 TMI [Start] define laNLineNo array to hold the max value of nLineno
  DIMENSION LANLINENO[1]
  LANLINENO = 0
  LLADD2DETFILE = .F.
  *B608180,1 TMI [End  ]

  *E038150,1 AMH Consider the inventory type [Start]
  *=SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  *LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  =SEEK(LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
    EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE)

  LOCATE REST WHILE ;
    CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
    LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
    EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE ;
    FOR CSIZES = STRTRAN(LCSIZES,',','')
  IF !FOUND()
    *B608180,1 TMI [Start] if the same line is found for the same fabric but with differnt operation or totcost , then add a new line
    LLADD2DETFILE = .T.
  ELSE
    *B608223,1 TMI [Start] check if called from lfvGenerate

    *B608749,1 WAM 12/02/2008 Commented out
    *IF llSheetGen
    *B608749,1 WAM 12/02/2008 (End)

    *B608223,1 TMI [End  ]

    LOCATE REST WHILE ;
      CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
      LCTRANLETT+'1'+LCTICKETNO+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
      EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE ;
      FOR CSIZES = STRTRAN(LCSIZES,',','') AND COPRCODE = &LCTMPBOMSH..COPRCODE
    IF !FOUND()
      LLADD2DETFILE = .T.
    ELSE
      *B608749,1 WAM 12/02/2008 COmpare BOM unit cost with BOM line unit cost
      *LOCATE REST WHILE ;
      cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
      lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
      EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
      FOR cSizes = STRTRAN(lcSizes,',','') AND cOprCode = &lcTmpBomSh..cOprCode AND ItemAmt = &lcTmpBomSh..TotCost

      *B608902,1 MMT 06/22/2009 Fix bugs of not updating PO Cost Sheet when edit PO[Start]
      *!*	        LOCATE REST WHILE ;
      *!*	        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
      *!*	        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+lcInvType+PADR(lcItem,19)+;
      *!*	        EVALUATE(lcTmpBomSh+'.cInvTypC')+PADR(lcCstItm,19)+&lcTmpBomSh..MfgCode ;
      *!*	        FOR cSizes = STRTRAN(lcSizes,',','') AND cOprCode = &lcTmpBomSh..cOprCode AND UnitCost = &lcTmpBomSh..UntCost
      *!*	        *B608749,1 WAM 12/02/2008 COmpare BOM unit cost with BOM line unit cost

      *!*	        IF !FOUND()
      *!*	          llAdd2DetFile = .T.
      *!*	        ENDIF
      *B608902,1 MMT 06/22/2009 Fix bugs of not updating PO Cost Sheet when edit PO[End]
    ENDIF

    *B608749,1 WAM 12/02/2008 Commented out
    *IF llAdd2DetFile
    *  SELECT MAX(nLineNo) FROM &lcDetFile INTO ARRAY laNLineNo
    *  laNLineNo[1] = laNLineNo[1] + 1
    *ENDIF
    *B608749,1 WAM 12/02/2008 Commented out

    *B608749,1 WAM 12/02/2008 Commented out
    *ENDIF
    *B608749,1 WAM 12/02/2008 (End)

    *B608223,1 TMI [Start]
  ENDIF
  *B608223,1 TMI [End  ]

  IF LLADD2DETFILE
    *B608749,1 WAM 12/02/2008 Increament the NLINENO field in BOMLINE when the value of the uneque key repeated.
    SELECT (LCDETFILE)
    LANLINENO[1] = 0
    SCAN FOR ;
        CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
        LCTRANLETT+'1'+LCTICKETNO+SPACE(6)+STR(LNLINENO,6)+&LCTMPBOMSH..TYP+LCINVTYPE+PADR(LCITEM,19)+;
        EVALUATE(LCTMPBOMSH+'.cInvTypC')+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE+SPACE(6)+LCGRADE

      LANLINENO[1] = MAX(IIF(ISNULL(NLINENO),0,NLINENO),LANLINENO[1])
    ENDSCAN
    LANLINENO[1] = LANLINENO[1] + 1
    *B608749,1 WAM 12/02/2008 (End)

    *B608180,1 TMI [End  ]
    APPEND BLANK
    *REPLACE cIMTyp     WITH lcTranLett     ,;
    cTktNo     WITH lcTicketNo     ,;
    LineNo     WITH lnLineNo       ,;
    cStyGrade  WITH lcGrade        ,;
    Style      WITH lcItem         ,;
    SClr       WITH lcColor        ,;
    cBomTyp    WITH &lcTmpBomSh..Typ,;
    cType      WITH '1'            ,;
    cCatGTyp   WITH &lcTmpBomSh..cCatGTyp   ,;
    cOprCode   WITH &lcTmpBomSh..cOprCode   ,;
    UnitQty    WITH &lcTmpBomSh..nBomTotQty ,;
    UnitCost   WITH lnUnitPri      ,;
    Item       WITH lcCstItm       ,;
    IClr       WITH lcCstClr       ,;
    MfgCode    WITH &lcTmpBomSh..MfgCode    ,;
    Dyelot     WITH lcCstItmDye ,;
    cSizes     WITH STRTRAN(lcSizes,',','') ,;
    cCompSizes WITH lcCmSizes
    REPLACE CIMTYP     WITH LCTRANLETT     ,;
      CTKTNO     WITH LCTICKETNO     ,;
      LINENO     WITH LNLINENO       ,;
      CSTYGRADE  WITH LCGRADE        ,;
      STYLE      WITH LCITEM         ,;
      CINVTYPE   WITH LCINVTYPE      ,;
      CBOMTYP    WITH &LCTMPBOMSH..TYP,;
      CTYPE      WITH '1'            ,;
      CCATGTYP   WITH &LCTMPBOMSH..CCATGTYP   ,;
      COPRCODE   WITH &LCTMPBOMSH..COPRCODE   ,;
      UNITQTY    WITH &LCTMPBOMSH..NBOMTOTQTY ,;
      UNITCOST   WITH LNUNITPRI      ,;
      ITEM       WITH LCCSTITM       ,;
      CINVTYPC   WITH EVALUATE(LCTMPBOMSH+'.cInvTypC'),;
      MFGCODE    WITH &LCTMPBOMSH..MFGCODE    ,;
      DYELOT     WITH LCCSTITMDYE ,;
      CSIZES     WITH STRTRAN(LCSIZES,',','') ,;
      CCOMPSIZES WITH LCCMSIZES
    REPLACE CCOSTSTAT WITH &LCTMPBOMSH..CCOSTSTAT,;
      NPERCENT   WITH &LCTMPBOMSH..NPERCENT
    *B608180,1 TMI [Start] update the nLineno field
    *B608223,1 TMI [Start] check if called from lfvGenerate

    *B608749,1 WAM 12/02/2008 Commented out
    *IF llSheetGen
    *B608749,1 WAM 12/02/2008 (End)

    *B608223,1 TMI [End  ]
    REPLACE NLINENO    WITH LANLINENO[1]
    *B608223,1 TMI [Start]

    *B608749,1 WAM 12/02/2008 Commented out
    *ENDIF
    *B608749,1 WAM 12/02/2008 (End)

    *B608223,1 TMI [End  ]
    *B608180,1 TMI [End  ]

  ENDIF
  *E038150,1 AMH [End]

  IF LNTRANQTY = 0
    REPLACE ITEMAMT  WITH ITEMQTY*LNUNITPRI ,;
      UNITCOST WITH IIF(ITEMQTY=0,0,ITEMAMT/ITEMQTY)

  ELSE
    *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
    *REPLACE StyQty   WITH StyQty  + lnTranQty ,;
    ItemQty  WITH ItemQty + lnTranQty*UnitQty  ,;
    UnitQty  WITH IIF(StyQty=0,0,ItemQty/StyQty) ,;
    ItemAmt  WITH ItemAmt + lnTranQty*UnitQty*lnUnitPri ,;
    UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
    SELECT (LCCOSTELEM)
    
*B611686,1 SAH 10/28/2018 modify function gfSheetItem as the Cutting cost sheet calculated the Qty incorrectly When item is repeated more than once in Style cost sheet [BEGIN]  
*!*	    =SEEK('B'+&LCTMPBOMSH..TYP+LCCSTITM+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE)
*!*	    LOCATE REST WHILE CTRTYPE+CBOMTYP +ITEM+MFGCODE+CDYELOT = 'B'+&LCTMPBOMSH..TYP+LCCSTITM+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE ;
*!*	      FOR CSIZES =STRTRAN(LCSIZES,',','')

    =SEEK('B'+&LCTMPBOMSH..TYP+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE)
    LOCATE REST WHILE CTRTYPE+CBOMTYP +ITEM+MFGCODE+CDYELOT = 'B'+&LCTMPBOMSH..TYP+PADR(LCCSTITM,19)+&LCTMPBOMSH..MFGCODE+ LCCSTITMDYE ;
      FOR CSIZES =STRTRAN(LCSIZES,',','')
      
*B611686,1 SAH 10/28/2018 modify function gfSheetItem as the Cutting cost sheet calculated the Qty incorrectly When item is repeated more than once in Style cost sheet [END]  

    IF !FOUND()
      REPLACE STYQTY WITH STYQTY  + LNTRANQTY IN (LCDETFILE)
      INSERT INTO (LCCOSTELEM) (CTRTYPE,CBOMTYP ,ITEM, MFGCODE,CDYELOT,CSIZES);
        VALUES ('B',&LCTMPBOMSH..TYP,LCCSTITM,&LCTMPBOMSH..MFGCODE,LCCSTITMDYE,STRTRAN(LCSIZES,',',''))
    ENDIF

    SELECT (LCDETFILE)
    REPLACE ITEMQTY  WITH ITEMQTY + LNTRANQTY*&LCTMPBOMSH..NBOMTOTQTY,;
      UNITQTY  WITH IIF(STYQTY=0,0,ITEMQTY/STYQTY) ,;
      ITEMAMT  WITH ITEMAMT + LNTRANQTY*&LCTMPBOMSH..NBOMTOTQTY*LNUNITPRI ,;
      UNITCOST WITH IIF(ITEMQTY=0,0,ITEMAMT/ITEMQTY)
    *! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]
  ENDIF


  *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
  *wael
  *REPLACE cCUrrCode WITH IIF(ISNULL(&lcTmpBomSh..cCurrCode) OR EMPTY(&lcTmpBomSh..cCurrCode),oAriaApplication.BaseCurrency,&lcTmpBomSh..cCurrCode) ,;
  nExRate   WITH IIF(ISNULL(&lcTmpBomSh..nExRate)   OR &lcTmpBomSh..nExRate=0  ,1,&lcTmpBomSh..nExRate) ,;
  nCurrUnit WITH IIF(ISNULL(&lcTmpBomSh..nCUrrUnit) OR &lcTmpBomSh..nCUrrUnit=0,1,&lcTmpBomSh..nCurrUnit)
  IF CCATGTYP = 'P'
    REPLACE CCURRCODE WITH LCPRICECUR ,;
      NEXRATE   WITH LNPRICERATE ,;
      NCURRUNIT WITH LNPRICEUNIT
  ELSE
    REPLACE CCURRCODE WITH IIF(ISNULL(&LCTMPBOMSH..CCURRCODE) OR EMPTY(&LCTMPBOMSH..CCURRCODE),OARIAAPPLICATION.BASECURRENCY,&LCTMPBOMSH..CCURRCODE) ,;
      NEXRATE   WITH IIF(ISNULL(&LCTMPBOMSH..NEXRATE)   OR &LCTMPBOMSH..NEXRATE=0  ,1,&LCTMPBOMSH..NEXRATE) ,;
      NCURRUNIT WITH IIF(ISNULL(&LCTMPBOMSH..NCURRUNIT) OR &LCTMPBOMSH..NCURRUNIT=0,1,&LCTMPBOMSH..NCURRUNIT)
  ENDIF
  *wael
  *N000587,1 WAM 12/01/2007 (End)

  * HES B608833,1 Update the audit fields
  =GFADD_INFO(LCDETFILE)
  * HES B608833

  IF STYQTY = 0 AND !LLBACKONLY
    DELETE
  ENDIF
ENDSCAN
IF LLCONTINUE

  *N119813,1 AMH Update all transaction in the POSHDR file [Start]
  *DO CASE
  *  CASE lcTranType='M' .AND. SEEK(lcTicketNo,'CUTTKTH')  .AND. CUTTKTH.Status # 'H'
  *    SELECT CUTTKTH
  *    =RLOCK()
  *    REPLACE CLASTOPR   WITH lcLastOpr ,;
  *            NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
  *            NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
  *            NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
  *            NEST_COST4 WITH NEST_COST4 + lnEst4 ,;
  *            NEST_COST5 WITH NEST_COST5 + lnEst5
  *    UNLOCK
  *  CASE lcTranType$'DI' .AND. SEEK(IIF(lcTranType='I','P','D')+lcTicketNo,'POSHDR') .AND. POSHDR.Status='O'
  *    SELECT POSHDR
  *    =RLOCK()
  *    REPLACE CLASTOPR WITH lcLastOpr ,;
  *            NICOST1  WITH NICOST1 + lnEst1 ,;
  *            NICOST2  WITH NICOST2 + lnEst2 ,;
  *            NICOST3  WITH NICOST3 + lnEst3 ,;
  *            NICOST4  WITH NICOST4 + lnEst4 ,;
  *            NICOST5  WITH NICOST5 + lnEst5 ,;
  *            NFCOST1  WITH NFCOST1 + lnFEst1 ,;
  *            NFCOST2  WITH NFCOST2 + lnFEst2 ,;
  *            NFCOST3  WITH NFCOST3 + lnFEst3 ,;
  *            NFCOST4  WITH NFCOST4 + lnFEst4 ,;
  *            NFCOST5  WITH NFCOST5 + lnFEst5
  *    UNLOCK
  *  CASE lcTranType='T' .AND. SEEK(lcTicketNo,'MMFGORDH') .AND. MMFGORDH.Status='O'
  *    SELECT MMFGORDH
  *    =RLOCK()
  *    REPLACE CLASTOPR   WITH lcLastOpr ,;
  *            NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
  *            NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
  *            NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
  *            NEST_COST4 WITH NEST_COST4 + lnEst4
  *    UNLOCK
  *ENDCASE
  IF EVALUATE(LCPOSHDR+'.Status')='O'
    SELECT (LCPOSHDR)
    REPLACE CLASTOPR WITH LCLASTOPR ,;
      NICOST1  WITH NICOST1 + LNEST1 ,;
      NICOST2  WITH NICOST2 + LNEST2 ,;
      NICOST3  WITH NICOST3 + LNEST3 ,;
      NICOST4  WITH NICOST4 + LNEST4 ,;
      NICOST5  WITH NICOST5 + LNEST5 ,;
      NICOST6  WITH NICOST6 + LNEST6 ,;
      NICOST7  WITH NICOST7 + LNEST7 ,;
      NFCOST1  WITH NFCOST1 + LNFEST1 ,;
      NFCOST2  WITH NFCOST2 + LNFEST2 ,;
      NFCOST3  WITH NFCOST3 + LNFEST3 ,;
      NFCOST4  WITH NFCOST4 + LNFEST4 ,;
      NFCOST5  WITH NFCOST5 + LNFEST5 ,;
      NFCOST6  WITH NFCOST6 + LNFEST6 ,;
      NFCOST7  WITH NFCOST7 + LNFEST7
  ENDIF
  *N119813,1 AMH [End]

ENDIF
IF USED('STYLE')
  SELECT STYLE
  SET FILTER TO &LCSTYFILT
ENDIF
USE IN (LCTMPBOMSH)
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[Start]
USE IN (LCCOSTELEM)
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[End]


ERASE (OARIAAPPLICATION.WORKDIR+LCTMPBOMSH+'.DBF')
ERASE (OARIAAPPLICATION.WORKDIR+LCTMPBOMSH+'.CDX')
ERASE (OARIAAPPLICATION.WORKDIR+LCTMPBOMSH+'.FPT')

SELECT (LNALIAS)

*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE LLSTYCONFG,LLSHEETGEN,LCMJRMSK,LCMJRHDR,LCNMJRMSK,LAITEMSEG,LCCLRMSK,LNCLRPOS,LCITEMFILE,;
  LLDYELOT,LCSCALE,LCITEMTYPE,LNTRANQTY,LCSIZES,LNCOUNT,LCCSTCLR,LNUNITPRI,;
  LCCSTITM,LAMFGRFLD,LNLASTSEQ,LNSIZEPOS,LCTMPBOMSH,LAREQ,;
  LCCONTCODE,LCCONTNAME,LCOPERSEQ,LLINHOUSE,LLMFGOPR,LNLEADTIME,;
  LCMFGGLACNT,LCEXSIGN,LCUNTSIN,LNALIAS,LCPRICECUR,LNPRICERATE,;
  LNPRICEUNIT,LCDUTYCUR,LNDUTYRATE,LNDUTYUNIT,LNUNTQTY,LCTRANLETT
RELEASE M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC
RELEASE LAMFGRFLD,LAITEMSEG,LAREQ,LLMULCURR,LNSCLCNT,LAFILESTRU,LCCOSTELEM,LCLINE,;
  LCSTYFILT,LCGRADE,LLMSGDISPD,LNI,LCI,LCCSTCLR,LCCSTITM,LCCMSIZES,LCCSTITMDYE,LCCSITMSC,;
  LNITMCLRPOS,LNITMCLRLEN,LCITEMWIDTH,LNCURALIAS,LCSVORD,LLFOUNDREC ,LNTTRANQTY ,LNFTOTCOST
RELEASE LNTOTCOST,LNCURRUNIT,LCEXSIGN,LNEXRATE,LNCONF,LNIECOST1, LNIECOST2, LNIECOST3, LNIECOST4,;
  LNIECOST5, LNIECOST6, LNIECOST7
RELEASE LCEXSIN2  ,LCEXSIN1  ,LCCOUNT,LNEST1,LNEST2,LNEST3,LNEST4,LNEST5,LNEST6,LNEST7,LNFEST1,;
  LNFEST2,LNFEST3,LNFEST4,LNFEST5,LNFEST6,LNFEST7,LLEDTFLD,LLADDFLD,LANLINENO,LLADD2DETFILE
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]

RETURN(LLCONTINUE)

*!*************************************************************
*! Name      : gfSelDyelot
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Check ticket sheet item have dyelots
*!*************************************************************
*! Calls     : FDYEBROW,SDYEBROW
*!*************************************************************
*! Parameters: lcTranType : Transaction type   ('M'-'I'-'T')
*!             lcType     : 'F' Fabric / 'S' Style
*!             lcItem     : Ticket Item
*!             lcColor    : Ticket Color
*!             lcWareCode : Warehouse
*!             lcDyelot   : Dyelot
*!             lcCstItem  : Cost sheet item
*!             lcCstItmClr: Cost sheet color
*!             lcTmpName  : Temp Name
*!             llBackOnly : Update files in background
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION GFSELDYELOT
*N119813,1 AMH Consider case of configuration [Start]
*PARAMETER lcTranType,lcType,lcItem,lcColor,lcWareCode,lcDyelot,lcCstItem,;
lcCstItmClr,lcTmpName,llBackOnly

*E038150,1 AMH Pass the inventory type instead of color [Start]
*LPARAMETERS lcTranType,lcType,lcItem,lcColor,lcWareCode,lcDyelot,lcCstItem,;
lcCstItmClr,lcTmpName,llBackOnly,llStyConfg
LPARAMETERS LCTRANTYPE,LCTYPE,LCITEM,LCINVTYPE,LCWARECODE,LCDYELOT,LCCSTITEM,;
  LCINVTYPC,LCTMPNAME,LLBACKONLY,LLSTYCONFG
*E038150,1 AMH [End]

*N119813,1 AMH [End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

PRIVATE LNALIAS,LNSELECT,LCMESSAGE,LCMESSAGE1,LCITMHDR

LNALIAS = SELECT()

*E038150,1 AMH Consider the inventory type [Start]
*lcItmHdr= gfItemMask("HI")
LCITMHDR= GFITEMMASK("HI","",LCINVTYPE)
LCCSTHDR= GFITEMMASK("HI","",LCINVTYPC)
*E038150,1 AMH [End]

STORE '' TO M_WAREHOUSE

*N119813,1 AMH Use the correct company ID [Start]
*=gfGetMemVar('M_WAREHOUSE',gcAct_Comp)
=GFGETMEMVAR('M_WAREHOUSE',OARIAAPPLICATION.ACTIVECOMPANYID)
*N119813,1 AMH [End]

*E300935,4 [begin]
*E300935,4 adjust condition to update without message if llBackOnly is .T.
LNSELECT = 1
IF !LLBACKONLY AND !EMPTY(LCDYELOT)

  *E038150,1 AMH Remove the lcColor parameter [Start]
  *IF lcTranType = 'T'
  *  IF EMPTY(lcItem+lcColor)
  *    lcMessage = 'One or more Fabrics'+;
  *    IIF(M_WAREHOUSE='Y','in warehouse '+lcWareCode,'')
  *  ELSE
  *    lcMessage = 'Fabric/Color'+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
  *                ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor)+;
  *                IIF(M_WAREHOUSE='Y','/'+lcWareCode,'')
  *  ENDIF
  *ELSE
  *  IF EMPTY(lcItem)
  *    lcMessage = 'One or more '+ALLTRIM(lcItmHdr)+;
  *    IIF(M_WAREHOUSE='Y','in warehouse '+lcWareCode,'')
  *  ELSE
  *    lcMessage = ALLTRIM(lcItmHdr)+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
  *                ALLTRIM(lcItem)+IIF(M_WAREHOUSE='Y','/'+lcWareCode,'')
  *  ENDIF
  *ENDIF
  IF EMPTY(LCITEM)
    LCMESSAGE = 'One or more '+ALLTRIM(LCITMHDR)+;
      IIF(M_WAREHOUSE='Y','in warehouse '+LCWARECODE,'')
  ELSE
    LCMESSAGE = ALLTRIM(LCITMHDR)+IIF(M_WAREHOUSE='Y','/Warehouse: ',': ')+;
      ALLTRIM(LCITEM)+IIF(M_WAREHOUSE='Y','/'+LCWARECODE,'')
  ENDIF
  *E038150,1 AMH [end]

  IF LCTYPE = 'F'

    *E038150,1 AMH Consider the inventory type [Start]
    *lcMessage1 = 'Fabric/Color/Dyelot: '+ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcCstItmClr)+;
    '/'+ALLTRIM(lcDyelot)
    LCMESSAGE1 = ALLTRIM(LCCSTHDR)+'/Dyelot: '+ALLTRIM(LCCSTITEM)+'/'+ALLTRIM(LCDYELOT)
    *E038150,1 AMH [End]

  ELSE

    *N119813,1 AMH Consider case of configuration [Start]
    *lcMessage1 = ALLTRIM(lcItmHdr)+'/Dyelot: '+ALLTRIM(lcCstItem)+'/'+ALLTRIM(lcDyelot)
    LCMESSAGE1 = ALLTRIM(LCITMHDR)+'/'+IIF(LLSTYCONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",;
      LANG_ARIA_CONFIGMESG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CONFIGMESG",LCARIAHFILE)),;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTMESG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DYELOTMESG",LCARIAHFILE)))+': '+;
      ALLTRIM(LCCSTITEM)+'/'+ALLTRIM(LCDYELOT)
    *N119813,1 AMH [End]

  ENDIF

  *E300725,1 Message : 38090
  *E300725,1 Fabric/Color/warehouse: xxx/xxx/xxx requires fabric/color/dyelot
  *E300725,1 xxx/xxx/xxx. This dyelot is not available for the fabric
  *E300725,1 Button : 38011
  *E300725,1 Add dyelot  Select dyelot Cancel

  *N119813,1 AMH Consider case of configuration [Start]
  *lnSelect=gfModalGen('QRM38090B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+;
  IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))

  *E038150,1 AMH Consider the inventory type [Start]
  IF LLSTYCONFG .AND. LCTYPE='S'
    *lnSelect=gfModalGen('QRM38283B38032','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_CONFIGMESG+'|'+;
    IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lnSelect=gfModalGen('QRM38283B38032','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_CONFIGMESG+'|'+ALLTRIM(lcCstHdr))
    LNSELECT=GFMODALGEN('QRM38283B38032','ALERT',LCMESSAGE+'|'+LCMESSAGE1+'|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CONFIGMESG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CONFIGMESG",LCARIAHFILE))+'|'+ALLTRIM(LCCSTHDR))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    *lnSelect=gfModalGen('QRM38283B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_DYELOTMESG+'|'+;
    IIF(lcType='F','Fabric/Color',ALLTRIM(lcItmHdr)))
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lnSelect=gfModalGen('QRM38283B38011','ALERT',lcMessage+'|'+lcMessage1+'|'+LANG_ARIA_DYELOTMESG+'|'+ALLTRIM(lcCstHdr))
    LNSELECT=GFMODALGEN('QRM38283B38011','ALERT',LCMESSAGE+'|'+LCMESSAGE1+'|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTMESG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DYELOTMESG",LCARIAHFILE))+'|'+ALLTRIM(LCCSTHDR))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  *E038150,1 AMH [End]

  *N119813,1 AMH [End]

ENDIF

IF EMPTY(LCDYELOT)
  LNSELECT = 2
ENDIF
DO CASE
CASE LNSELECT = 1

  *E038150,1 AMH Consider the inventory type [Start]
  *IF lcType = 'F' .AND. !SEEK(lcCstItem+lcCstItmClr+lcWareCode+lcDyelot,'FABDYE')
  *  DO gpAdFabWar WITH lcCstItem, lcCstItmClr, lcDyelot, lcWareCode,lcTmpName
  *ENDIF
  IF LCTYPE = 'F' .AND. !SEEK(LCCSTITEM+LCWARECODE+LCDYELOT,'FABDYE')
    DO GFADITEMWAR WITH LCINVTYPC, LCCSTITEM, LCDYELOT, LCWARECODE, LCTMPNAME
  ENDIF
  *E038150,1 AMH [End]

  IF LCTYPE = 'S' .AND. !SEEK(LCCSTITEM+LCWARECODE+LCDYELOT,'STYDYE')
    DO GPADSTYWAR WITH LCCSTITEM, LCDYELOT, LCWARECODE
  ENDIF
CASE LNSELECT = 2

  *N119813,1 AMH Consider case of configuration [Start]
  *IF !IIF(lcType='F',FDYEBROW(lcCstItem,lcCstItmClr,@lcDyelot,.T.),;
  SDYEBROW(lcCstItem,@lcDyelot,.T.)) .OR. EMPTY(lcDyelot)

  *E038150,1 AMH Consider the inventory type [Start]
  *IF !IIF(lcType='F',FDYEBROW(lcCstItem,lcCstItmClr,@lcDyelot,.T.),;
  SDYEBROW(lcCstItem,@lcDyelot,.T.,.F.,.F.,.F.,.F.,.F.,llStyConfg)) .OR. EMPTY(lcDyelot)
  IF !IIF(LCTYPE='F',GFITMDYBRW(LCINVTYPC,LCCSTITEM,@LCDYELOT,.T.),;
      SDYEBROW(LCCSTITEM,@LCDYELOT,.T.,.F.,.F.,.F.,.F.,.F.,LLSTYCONFG)) .OR. EMPTY(LCDYELOT)
    *E038150,1 AMH [End]

    *N119813,1 AMH [End]

    LCDYELOT=SPACE(10)
    RETURN(.F.)
  ENDIF
CASE LNSELECT = 3
  LCDYELOT=SPACE(10)
  RETURN(.F.)
ENDCASE
SELECT (LNALIAS)
RETURN

*!*************************************************************
*! Name      : lfUpdPOBom
*! Developer : Timour A. K.
*! Date      : 05/30/1999
*! Purpose   : Update Temp P/O cost sheet used in Creation of
*!             P/o cost sheet in gfSheetItem.
*!*************************************************************
*! Calls From: gfSheetItem()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION LFUPDPOBOM
LOCAL LNI

*--Update rate per lb cost in mfg record in dye order cost sheet.
*--If dye order cost sheet.
IF LCTRANTYPE = 'D'
  *--Overwite the cost of dyeing operation by the rate per lib * weight.
  LCDYEOPR = GFGETMEMVAR('M_DYEOPR')
  SELECT (LCTMPBOMSH)
  LOCATE FOR CCATGTYP='M' AND CITMMAJOR=STYLE.CSTYMAJOR AND MFGCODE=LCDYEOPR
  LCI = TYP

  *N119813,1 AMH Use the cursor name of the POSLN file [Start]
  *IF FOUND() AND POSLN.nCost&lcI <> 0
  *  REPLACE UntCost    WITH POSLN.nCost&lcI,;
  *          nBomTotQty WITH 1,;
  *          TotCost    WITH POSLN.nCost&lcI
  *ENDIF

  *N119813,1 KHM 08/09/2004 Use nFCost instead of nCost [Start]
  *IF FOUND() AND EVALUATE(lcPOSLN+'.nCost'+lcI) <> 0
  IF FOUND() AND EVALUATE(LCPOSLN+'.nFCost'+LCI) <> 0
    *N119813,1 KHM 08/09/2004 [End]
    REPLACE UNTCOST    WITH EVALUATE(LCPOSLN+'.nFCost'+LCI),;
      NBOMTOTQTY WITH 1,;
      TOTCOST    WITH EVALUATE(LCPOSLN+'.nFCost'+LCI)
  ENDIF
  *N119813,1 AMH [End]

ELSE

  *--If there is More than one PPrice for the same style, create the P/O cost
  *--sheet for PPrice element only one record with the cost that in P/O.
  SELECT (LCTMPBOMSH)
  =SEEK('1')

  *E038150,1 AMH Consider the inventory type [Start]
  *LOCATE REST WHILE typ+item+iclr+citmmajor+citmmask = '1' ;
  FOR cItmMajor=STYLE.cStyMajor
  LOCATE REST WHILE TYP+CINVTYPC+ITEM+CINVTYPE+CITMMAJOR+CITMMASK = '1' ;
    FOR CITMMAJOR=STYLE.CSTYMAJOR
  LCCOSTSTAT = CCOSTSTAT
  *E038150,1 AMH [End]

  IF FOUND()

    *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
    LCCURRCODE = IIF(ISNULL(CCURRCODE) OR EMPTY(CCURRCODE),OARIAAPPLICATION.BASECURRENCY,CCURRCODE)
    LNCURRUNIT = IIF(ISNULL(NCURRUNIT) OR NCURRUNIT=0,1,NCURRUNIT)
    LNEXRATE   = IIF(ISNULL(NEXRATE) OR NEXRATE=0,1,NEXRATE)
    *N000587,1 WAM 12/01/2007 (End)

    *E038150,1 AMH Consider the inventory type [Start]
    *DELETE REST WHILE typ+item+iclr+citmmajor+citmmask = '1' ;
    FOR cItmMajor=STYLE.cStyMajor
    DELETE REST WHILE TYP+CINVTYPC+ITEM+CINVTYPE+CITMMAJOR+CITMMASK = '1' ;
      FOR CITMMAJOR=STYLE.CSTYMAJOR
    *E038150,1 AMH [End]


    IF LNPRICE <> 0
      *--Delete the cost sheet created from the BOM that contail multiple
      *--record for PPrice and create a new one from P/O with P/O price.
      APPEND BLANK

      *E038150,1 AMH Consider the inventory type [Start]
      *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
      Typ        WITH '1',;
      cItmMask   WITH STYLE.Style,;
      MfgCode    WITH IIF(!STYLE.LDetCost,'*1',"******"),;
      Uom        WITH 'EAC',;
      nBomTotQty WITH 1,;
      UntCost    WITH lnPrice,;
      TotCost    WITH lnPrice,;
      CCatgTyp   WITH 'P'

      *E038220,1 WSH Change UOM-Code to Relation Code [Start]
      *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
      cInvType   WITH "0001",;
      Typ        WITH '1',;
      cItmMask   WITH STYLE.Style,;
      MfgCode    WITH IIF(!STYLE.LDetCost,'*1',"******"),;
      Uom        WITH 'EAC',;
      nBomTotQty WITH 1,;
      UntCost    WITH lnPrice,;
      TotCost    WITH lnPrice,;
      CCatgTyp   WITH 'P'
      REPLACE CITMMAJOR  WITH STYLE.CSTYMAJOR,;
        CINVTYPE   WITH "0001",;
        TYP        WITH '1',;
        CITMMASK   WITH STYLE.STYLE,;
        MFGCODE    WITH IIF(!STYLE.LDETCOST,'*1',"******"),;
        NBOMTOTQTY WITH 1,;
        UNTCOST    WITH LNPRICE,;
        TOTCOST    WITH LNPRICE,;
        CCATGTYP   WITH 'P'
      *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines

      *B608749,1 WAM 12/02/2008 Update price currency and exchange rate with PO currency and exchange rate
      *REPLACE cCurrCode WITH lcCUrrCode ,;
      nCUrrUnit WITH lnCurrUnit ,;
      nExrate   WITH lnExRate

      REPLACE CCURRCODE WITH LCPRICECUR ,;
        NCURRUNIT WITH LNPRICERATE,;
        NEXRATE   WITH LNPRICEUNIT
      *B608749,1 WAM 12/02/2008 (End)

      *N000587,1 WAM 12/01/2007 (End)

      *--Get Relation for Default UOM-Code
      LOCAL LCRELCODE
      LCRELCODE = ''

      =GFGETUOMDATA(@LCRELCODE, '', '', 1, .F.)

      REPLACE CUOMCODE WITH LCRELCODE
      *E038220,1 WSH [End]

      *E038150,1 AMH [End]

      REPLACE CCOSTSTAT WITH LCCOSTSTAT
    ENDIF
  ENDIF

  *--If no detail costing for style and there is a cost elements entered.
  *--Create or update the P/O cost sheet for this cost element with the
  *--cost that ented in P/O.
  IF !STYLE.LDETCOST
    *--Delete the cost sheet created from the BOm and create a new one from P/O.
    DELETE FOR CCATGTYP<>'P' AND CITMMAJOR=STYLE.CSTYMAJOR

    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
    LLTINVT= ALLTRIM(GFGETMEMVAR('M_TINVT',OARIAAPPLICATION.ACTIVECOMPANYID))='Y'
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]

    *E038150,1 AMH Consider the siven cost elements [Start]
    *FOR lnI=2 TO 5
    FOR LNI=2 TO 7
      *E038150,1 AMH [End]

      LCI=STR(LNI,1)

      *N119813,1 AMH Use the cursor name of the POSLN file [Start]
      *IF POSLN.nCost&lcI <> 0
      *  lcIType&lcI = gfGetMemVar('M_cIType'+lcI)
      *  APPEND BLANK
      *  GATHER MEMVAR
      *  REPLACE cItmMajor WITH STYLE.cStyMajor,;
      *          Typ       WITH lcI,;
      *          cItmMask  WITH STYLE.Style,;
      *          MfgCode   WITH '*'+lcI ,;
      *          Uom       WITH 'EAC',;
      *          UntCost   WITH POSLN.nCost&lcI,;
      *          nBomTotQty WITH 1,;
      *          TotCost   WITH POSLN.nCost&lcI,;
      *          CCatgTyp  WITH lcIType&lcI
      *ENDIF

      *N119813,1 KHM 08/09/2004 Use nFCost instead of nCost [Start]
      *IF EVALUATE(lcPOSLN+'.nCost'+lcI) <> 0
      IF EVALUATE(LCPOSLN+'.nFCost'+LCI) <> 0
        *N119813,1 KHM 08/09/2004 [End]
        LCITYPE&LCI = GFGETMEMVAR('M_cIType'+LCI)
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[Start]
        LCCISLBL&LCI = GFGETMEMVAR('M_CISLBL'+LCI)
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[End]
        APPEND BLANK
        GATHER MEMVAR

        *E038150,1 AMH Consider the inventory type [Start]
        *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
        Typ        WITH lcI,;
        cItmMask   WITH STYLE.Style,;
        MfgCode    WITH '*'+lcI ,;
        Uom        WITH 'EAC',;
        UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        nBomTotQty WITH 1,;
        TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        CCatgTyp   WITH lcIType&lcI

        *E038220,1 WSH Change UOM-Code to Relation Code [Start]
        *REPLACE cItmMajor  WITH STYLE.cStyMajor,;
        cInvType   WITH "0001",;
        Typ        WITH lcI,;
        cItmMask   WITH STYLE.Style,;
        MfgCode    WITH '*'+lcI ,;
        Uom        WITH 'EAC',;
        UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        nBomTotQty WITH 1,;
        TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        CCatgTyp   WITH lcIType&lcI
        *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
        * REPLACE cItmMajor  WITH STYLE.cStyMajor,;
        cInvType   WITH "0001",;
        Typ        WITH lcI,;
        cItmMask   WITH STYLE.Style,;
        MfgCode    WITH '*'+lcI ,;
        UntCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        nBomTotQty WITH 1,;
        TotCost    WITH EVALUATE(lcPOSLN+'.nFCost'+lcI),;
        CCatgTyp   WITH lcIType&lcI

        REPLACE CITMMAJOR  WITH STYLE.CSTYMAJOR,;
          CINVTYPE   WITH "0001",;
          TYP        WITH LCI,;
          CITMMASK   WITH STYLE.STYLE,;
          MFGCODE    WITH '*'+LCI ,;
          UNTCOST    WITH EVALUATE(LCPOSLN+'.nFCost'+LCI),;
          NBOMTOTQTY WITH 1,;
          TOTCOST    WITH EVALUATE(LCPOSLN+'.nFCost'+LCI),;
          CCATGTYP   WITH LCITYPE&LCI,;
          CINVTYPC   WITH IIF(LCITYPE&LCI='F' OR (LCITYPE&LCI='T' AND LLTINVT),'0002','') ,;
          TRIM_INVT  WITH IIF(LCITYPE&LCI='T',LLTINVT,.F.)
        *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
        *--Get Relation for Default UOM-Code
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[Start]
        REPLACE DESC WITH LCCISLBL&LCI
        *B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[End]
        LOCAL LCRELCODE
        LCRELCODE = ''

        =GFGETUOMDATA(@LCRELCODE, '', '', 1, .F.)

        REPLACE CUOMCODE WITH LCRELCODE
        *E038220,1 WSH [End]
        *E038150,1 AMH [End]
        *N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
        REPLACE CCURRCODE WITH OARIAAPPLICATION.BASECURRENCY ,;
          NCURRUNIT WITH 1 ,;
          NEXRATE   WITH 1
        *N000587,1 WAM 12/01/2007 (End)
      ENDIF
      *N119813,1 AMH [End]

    ENDFOR
  ENDIF
ENDIF
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE LNI,LCDYEOPR,LCI,LCCURRCODE,LNCURRUNIT ,LNEXRATE   ,LCCOSTSTAT,LCRELCODE,;
  LLTINVT,LCITYPE2,LCITYPE3,LCITYPE4,LCITYPE5,LCITYPE6,LCITYPE7
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]
RETURN

*****************************************************************************
* PROG: FDYEBROW
* DESC: PROC TO BROWSE THE Dyelots for specific fabric,color
* using the new browse
*****************************************************************************
PROCEDURE FDYEBROW
PARAMETER XFAB,XCLR,XDYELOT,LLRETALIAS, LCFROMWARE
PRIVATE LCBRFIELDS,LNCURALIAS,LLFOUND,LADATA

DECLARE LADATA[2]  && array to get values from browse
STORE '' TO LADATA
LLWASSEL   = .T.
LLBROWSE   = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE) && variable to determine forcing browse or not
LCBRFIELDS = [Dyelot:H="Dyelot #",Onhand:H="On Hand",Usage]

LNCURALIAS = SELECT()
IF !USED('FABDYE')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'FABDYE','FABDYE','SH')
ELSE
  SELECT FABDYE
  LCFDYORDER = TAG()
  SET ORDER TO TAG FABDYE
ENDIF

LLFOUND = .F.
IF SEEK(XFAB+XCLR+IIF( TYPE('lcFromWare') $ 'UL','',LCFROMWARE) )
  IF TYPE('lcFromWare')$'UL'
    LOCATE REST WHILE FABRIC + COLOR =  XFAB + XCLR FOR !EMPTY(DYELOT)
  ELSE
    LOCATE REST WHILE FABRIC + COLOR + CWARECODE = XFAB + XCLR + LCFROMWARE FOR !EMPTY(DYELOT)
  ENDIF
  LLFOUND = FOUND()
ENDIF

IF LLFOUND
  LNSOFTSEEK=RECNO(0)
  IF LNSOFTSEEK<>0
    GO LNSOFTSEEK
  ELSE
    GO TOP
  ENDIF

  LLWASSEL = ARIABROW([xFab+xClr+IIF(TYPE('lcFromWare')$ 'UL','',lcFromWare) FOR !EMPTY(DYELOT)],;
    "Item/Color "+ALLTRIM(XFAB)+"/"+ALLTRIM(XCLR)+" Dyelots",;
    GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,"","",;
    "FABDYE.Dyelot","laData")
  XDYELOT  = IIF(LLWASSEL, LADATA[1], SPACE(10))
ELSE
  XDYELOT  = SPACE(10)
  LOCAL LCDILMESG
  LCDILMESG = 'There are no dyelots for item/color '+ALLTRIM(XFAB)+'/'+ALLTRIM(XCLR)+;
    IIF(TYPE('lcFromWare')$'UL',' on the file.',' for warehouse '+ALLTRIM(LCFROMWARE)+'.')
  =MESSAGEBOX(LCDILMESG,64,_SCREEN.CAPTION)
ENDIF

IF !EMPTY(LCFDYORDER)
  SET ORDER TO TAG (LCFDYORDER)
ELSE
  SET ORDER TO
ENDIF
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
RETURN LLWASSEL


*****************************************************************************
*! PROG: SDYEBROW
*! DESC: PROGRAM TO BROWSE THE STYLE DYELOTS TO SELECT ONE
*! PARA: STYLE/COLOR AND DYELOT
*****************************************************************************
PROCEDURE SDYEBROW
*N119812,1 HBG 16/11/2003 Add new parameter wit .T. or .F. to check if use style configuration [Begin]
*PARAMETER XSTYLE,XDYELOT,llRetAlias,lcFromWare,llIncAvail,llTop,lcBrwAlias ,lcIndTag
PARAMETER XSTYLE,XDYELOT,LLRETALIAS,LCFROMWARE,LLINCAVAIL,LLTOP,LCBRWALIAS ,LCINDTAG , LLUSECONFG
*N119812,1 [End]
PRIVATE LCBRFIELDS,LCALIAS,LCFROMWARE,LNSTYORD,LNALIAS,LADATA,LCBRWALIAS,LCDATADIR,LCINDTAG
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
IF TYPE("lcBrwAlias") $ "UL"
  LCBRWALIAS = "STYDYE"
  LCDATADIR  = OARIAAPPLICATION.DATADIR
ELSE
  LCDATADIR  = OARIAAPPLICATION.WORKDIR
ENDIF
LCINDTAG = IIF(TYPE("lcIndTag") $ "UL","STYDYE",LCINDTAG)

DECLARE LADATA[3] && array to get values from browse
LADATA     = ' '
LLBROWSE   = IIF(TYPE('llBrowse')='U',.F.,LLBROWSE) && variable to determine forcing browse or not

PRIVATE LNBRHSROW1, LNBRHSCOL1, LNBRHSROW2, LNBRHSCOL2
IF LLTOP
  LNBRHSROW1 = GNBRFSROW1
  LNBRHSCOL1 = GNBRFSCOL1
  LNBRHSROW2 = GNBRHSROW1
  LNBRHSCOL2 = GNBRHSCOL1
ELSE
  LNBRHSROW1 = GNBRHSROW1
  LNBRHSCOL1 = GNBRHSCOL1
  LNBRHSROW2 = GNBRHSROW2
  LNBRHSCOL2 = GNBRHSCOL2
ENDIF
XSTYLE     = PADR(XSTYLE, 19)
LCFROMWARE = IIF(TYPE('lcFromWare') $ 'UL' .OR. EMPTY(LCFROMWARE),'', LCFROMWARE)

*N119812,1 HBG 16/11/2003 If use style configuration , Change Browse title and Field name [Begin]
*lcTitle    = "Style "+ALLTRIM(xStyle)+" Dyelots"
*lcBrFields = "DYELOT:R :H='Dyelot #':10,"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTitle    = "Style "+ALLTRIM(xStyle)+IIF(llUseConfg,LANG_ARIA_Config,LANG_ARIA_Dyelot)
LCTITLE    = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STYLE_TTL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_STYLE_TTL",LCARIAHFILE))+;
  " "+ALLTRIM(XSTYLE)+;
  IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CONFIG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Config",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Dyelot",LCARIAHFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcHeader = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
LCHEADER = IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CONFIGNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ConfigNum",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DyelotNum",LCARIAHFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LCBRFIELDS = "DYELOT:R :H='"+LCHEADER+"':17,"
*N119812,1 [End]


IF LLINCAVAIL
  *N119812,1 HBG 16/11/2003 Change all string s to be variabels in Header file [Begin]
  *!*	  lcBrFields = lcBrFields +;
  *!*		         "A1=STK1-ALO1  :R :H='Avl1':6,"+;
  *!*	             "A2=STK2-ALO2  :R :H='Avl2':6,"+;
  *!*		         "A3=STK3-ALO3  :R :H='Avl3':6,"+;
  *!*		         "A4=STK4-ALO4  :R :H='Avl4':6,"+;
  *!*		         "A5=STK5-ALO5  :R :H='Avl5':6,"+;
  *!*		         "A6=STK6-ALO6  :R :H='Avl6':6,"+;
  *!*		         "A7=STK7-ALO7  :R :H='Avl7':6,"+;
  *!*		         "A8=STK8-ALO8  :R :H='Avl8':6,"+;
  *!*		         "A9=TOTSTK-TOTALO :R :H='TotAvl':7,"
  *!*	  lcBrFields = lcBrFields +;
  *!*		         "ALO1  :R :H='Alo1':6,"+;
  *!*		         "ALO2  :R :H='Alo2':6,"+;
  *!*		         "ALO3  :R :H='Alo3':6,"+;
  *!*		         "ALO4  :R :H='Alo4':6,"+;
  *!*		         "ALO5  :R :H='Alo5':6,"+;
  *!*		         "ALO6  :R :H='Alo6':6,"+;
  *!*		         "ALO7  :R :H='Alo7':6,"+;
  *!*		         "ALO8  :R :H='Alo8':6,"+;
  *!*	             "TOTALO:R :H='TotAlo':7,"+;
  *!*		         "STK1  :R :H='Stk1':6,"+;
  *!*	             "STK2  :R :H='Stk2':6,"+;
  *!*		         "STK3  :R :H='Stk3':6,"+;
  *!*		         "STK4  :R :H='Stk4':6,"+;
  *!*		         "STK5  :R :H='Stk5':6,"+;
  *!*		         "STK6  :R :H='Stk6':6,"+;
  *!*		         "STK7  :R :H='Stk7':6,"+;
  *!*		         "STK8  :R :H='Stk8':6,"+;
  *!*		         "TOTSTK:R :H='TotStk':7"
  LCBRFIELDS = LCBRFIELDS +;
    "A1=STK1-ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl1",LCARIAHFILE))+"':6,"+;
    "A2=STK2-ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl2",LCARIAHFILE))+"':6,"+;
    "A3=STK3-ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl3",LCARIAHFILE))+"':6,"+;
    "A4=STK4-ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl4",LCARIAHFILE))+"':6,"+;
    "A5=STK5-ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl5",LCARIAHFILE))+"':6,"+;
    "A6=STK6-ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl6",LCARIAHFILE))+"':6,"+;
    "A7=STK7-ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl7",LCARIAHFILE))+"':6,"+;
    "A8=STK8-ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl8",LCARIAHFILE))+"':6,"+;
    "A9=TOTSTK-TOTALO :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTAVL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAvl",LCARIAHFILE))+"':7,"
  LCBRFIELDS = LCBRFIELDS +;
    "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':6,"+;
    "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':6,"+;
    "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':6,"+;
    "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':6,"+;
    "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':6,"+;
    "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':6,"+;
    "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':6,"+;
    "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':6,"+;
    "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAlo",LCARIAHFILE))+"':7,"+;
    "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':6,"+;
    "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':6,"+;
    "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':6,"+;
    "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':6,"+;
    "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':6,"+;
    "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':6,"+;
    "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':6,"+;
    "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"':6,"+;
    "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':7"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N119812,1 [End]
ELSE
  *N119812,1 HBG 16/11/2003 Change all string s to be variabels in Header file [Begin]
  *!*	  lcBrFields = lcBrFields +;
  *!*		         "STK1  :R :H='Stk1':6,"+;
  *!*	             "STK2  :R :H='Stk2':6,"+;
  *!*		         "STK3  :R :H='Stk3':6,"+;
  *!*		         "STK4  :R :H='Stk4':6,"+;
  *!*		         "STK5  :R :H='Stk5':6,"+;
  *!*		         "STK6  :R :H='Stk6':6,"+;
  *!*		         "STK7  :R :H='Stk7':6,"+;
  *!*		         "STK8  :R :H='Stk8':6,"+;
  *!*		         "TOTSTK:R :H='TotStk':7,"+;
  *!*		         "ALO1  :R :H='Alo1':6,"+;
  *!*		         "ALO2  :R :H='Alo2':6,"+;
  *!*		         "ALO3  :R :H='Alo3':6,"+;
  *!*		         "ALO4  :R :H='Alo4':6,"+;
  *!*		         "ALO5  :R :H='Alo5':6,"+;
  *!*		         "ALO6  :R :H='Alo6':6,"+;
  *!*		         "ALO7  :R :H='Alo7':6,"+;
  *!*		         "ALO8  :R :H='Alo8':6,"+;
  *!*	             "TOTALO:R :H='TotAlo':7"
  LCBRFIELDS = LCBRFIELDS +;
    "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':6,"+;
    "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':6,"+;
    "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':6,"+;
    "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':6,"+;
    "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':6,"+;
    "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':6,"+;
    "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':6,"+;
    "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"':6,"+;
    "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':7,"+;
    "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':6,"+;
    "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':6,"+;
    "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':6,"+;
    "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':6,"+;
    "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':6,"+;
    "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':6,"+;
    "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':6,"+;
    "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':6,"+;
    "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAlo",LCARIAHFILE))+"':7"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N119812,1 [End]
ENDIF

LNALIAS    = SELECT()
LLOPENFILE = GFOPENFILE(LCDATADIR+LCBRWALIAS,LCDATADIR+LCINDTAG,'SH')

SELECT (LCBRWALIAS)
LNSTYORD   = VAL(SYS(21))
SET ORDER TO TAG (LCINDTAG)
LOCATE
SEEK XSTYLE + LCFROMWARE
LOCATE REST FOR !EMPTY(DYELOT) WHILE STYLE + CWARECODE = XSTYLE + LCFROMWARE
IF !FOUND()
  *N119812,1 HBG 16/11/2003
  *-- Change all string s to be variabels in Header file [Begin]
  *!*	  lcTmpStr = ALLTRIM(xStyle) + IIF(EMPTY(lcFromWare) , ' on the file' , ' in location ' + ALLTRIM(lcFromWare))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcTmpStr = ALLTRIM(xStyle) + IIF(EMPTY(lcFromWare) , LANG_ARIA_MsgVar1 , LANG_ARIA_MsgVar2 + ALLTRIM(lcFromWare))
  LCTMPSTR = ALLTRIM(XSTYLE) + IIF(EMPTY(LCFROMWARE) ,;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGVAR1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgVar1",LCARIAHFILE)) ,;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGVAR2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgVar2",LCARIAHFILE)) + ALLTRIM(LCFROMWARE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *-- If use style configuration , Change the message [Begin]
  IF LLUSECONFG
    =GFMODALGEN("INM00407B00000" , "DIALOG" , LCTMPSTR)
  ELSE
    *N119812,1 [End]
    =GFMODALGEN("INM00277B00000" , "DIALOG" , LCTMPSTR)
    *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
  ENDIF
  *N119812,1 [End]
  LLWASSEL = .F.
ELSE
  LLWASSEL= ARIABROW('XSTYLE + lcFromWare'+[FOR !EMPTY(DYELOT)],LCTITLE,;
    LNBRHSROW1, LNBRHSCOL1, LNBRHSROW2, LNBRHSCOL2,'','',"Style,Dyelot","laData")

  IF LLWASSEL
    XSTYLE = LADATA[1]
    XDYELOT= LADATA[2]
  ELSE
    XDYELOT= SPACE(10)
  ENDIF
ENDIF

SET ORDER TO LNSTYORD
IF LLOPENFILE .AND. USED(LCBRWALIAS)
  USE IN (LCBRWALIAS)
ENDIF
SELECT (LNALIAS)

RETURN LLWASSEL

*!**************************************************************************
*! PROG: STYBROW.PRG
*! program to browse through the style file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE STYLE FILE HAS BEEN OPENED
*!       IF WE WANT TO DISPLAY NO COLORS THEN XCLR = '*'
*!       IF WE WANT TO DISPLAY ALL SEASONS XSEASON = '*'
*!**************************************************************************
*!**************************************************************************
*! Modification:
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*!*****************************************************************************************
PROCEDURE STYBROW

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
*PARAMETERS XSTYLE,XCLR,XSEASON,llRetAlias,lcMajorOrNon
PARAMETERS XSTYLE,XCLR,XSEASON,LLRETALIAS,LCMAJORORNON,LCCURSOR,LCSELFLD,LCVALIDFUN
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

PRIVATE LCBRFIELDS,LNCURALIAS, LCSTYLE,LADATA ,LCSTYORDER ,LCSTYTTL ,LCTITLE ,;
  LCXMJR ,LCXNMJR ,LCCLRTTL ,LCSTYORDER,LCMAJORORNON, LNMAJLEN,XSTYLE,XCLR,XSEASON

DECLARE LADATA[3] && array to get values from browse
STORE '' TO LADATA,LCSCOPE
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE) && variable to determine forcing browse or not

IF !USED('Codes')
  LCCURALIAS = ALIAS()
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'Codes','Codes','SH')
  SELECT(LCCURALIAS)
ENDIF

LCMAJORORNON = IIF(TYPE('lcMajorOrNon')$'UL',"M",LCMAJORORNON)
LCSTYTTL = IIF(LCMAJORORNON='M',GFITEMMASK('HM'),GFITEMMASK('HN'))
LCTITLE = IIF(LCMAJORORNON='M',GFITEMMASK('HM'),IIF(LCMAJORORNON="N",GFITEMMASK('HN'),GFITEMMASK('HI')))
LCXMJR  = GFITEMMASK('HM')
LCXNMJR = GFITEMMASK('HN')

LCCLRTTL = LCXMJR +'/'+ LCXNMJR
LNMAJLEN = LEN(GFITEMMASK('PM'))

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE,LCSTYBROWH
LCARIAHFILE = ''
LCSTYBROWH = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  LCSTYBROWH =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("STYBROW")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

IF LCMAJORORNON $ 'NI'

  *E124065,1 AMH Add color name to style and material browse [Start]
  *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
  *[lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
  LCBRFIELDS = [STYLE :30 :H=ALLTRIM(lcClrTtl),]+;
    [lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COLOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_COLOR",LCSTYBROWH))+;
    [",DESC :20 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROWH))+[",DESC1 :35 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LDESC",LCSTYBROWH))+[",]+;
    [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SEASON,OARIAAPPLICATION.GETHEADERTEXT("LANG_SEASON",LCSTYBROWH))+;
    [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DIV,OARIAAPPLICATION.GETHEADERTEXT("LANG_DIV",LCSTYBROWH))+[",]
  *E124065,1 AMH [End]

  LCBRFIELDS = LCBRFIELDS+;
    [pricea :10:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROWH))+;
    [" , PRICEB :10:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEB,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEB",LCSTYBROWH))+;
    [",PRICEC :10:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEC,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEC",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS+;
    [totWip:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROWH))+;
    [",totstk:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROWH))+;
    [",totord:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROWH))+[",]

  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][Start]
  *!*	    lcBrFields = lcBrFields+;
  *!*	                 [OTS=(TOTWIP+TOTSTK-TOTORD):12:H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]
  LCBRFIELDS = LCBRFIELDS+;
    [OTS=(TOTWIP+TOTSTK-TOTORD):12:H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROWH))+;
    [",cprifabric:15:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROWH))+[",]
  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][End]
  LCBRFIELDS = LCBRFIELDS+;
    [CSTYGRADE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GRADE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GRADE",LCSTYBROWH))+;
    [", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ROYAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ROYAL",LCSTYBROWH))+;
    [" , PATTERN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PATRN,OARIAAPPLICATION.GETHEADERTEXT("LANG_PATRN",LCSTYBROWH))+;
    [", STATUS :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_STATUS",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS + ;
    [SCALE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SCALE,OARIAAPPLICATION.GETHEADERTEXT("LANG_SCALE",LCSTYBROWH))+;
    [", PREPAK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PREPAK,OARIAAPPLICATION.GETHEADERTEXT("LANG_PREPAK",LCSTYBROWH))+;
    [", CBUYPREPK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BPREPK,OARIAAPPLICATION.GETHEADERTEXT("LANG_BPREPK",LCSTYBROWH))+;
    [", QTY_CTN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_QTYCRT,OARIAAPPLICATION.GETHEADERTEXT("LANG_QTYCRT",LCSTYBROWH))+;
    [", COMMISSION :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COMM,OARIAAPPLICATION.GETHEADERTEXT("LANG_COMM",LCSTYBROWH))+;
    [", LINK_CODE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LNKCD,OARIAAPPLICATION.GETHEADERTEXT("LANG_LNKCD",LCSTYBROWH))+[",]+;
    [lcMaked = IIF(MAKE,'Y','N') :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MAKE,OARIAAPPLICATION.GETHEADERTEXT("LANG_MAKE",LCSTYBROWH))+;
    [", NMCOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST1",LCSTYBROWH))+;
    [" , NMCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST2",LCSTYBROWH))+;
    [", NMCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST3",LCSTYBROWH))+;
    [", NMCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST4",LCSTYBROWH))+;
    [",NMCOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST5",LCSTYBROWH))+[",]


  LCBRFIELDS = LCBRFIELDS + ;
    [NICOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST1",LCSTYBROWH))+;
    [", NICOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST2",LCSTYBROWH))+;
    [", NICOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST3",LCSTYBROWH))+;
    [", NICOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST4",LCSTYBROWH))+;
    [", NICOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST5",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS+;
    [NPRCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST2",LCSTYBROWH))+;
    [",NPRCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST3",LCSTYBROWH))+;
    [",NPRCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST4",LCSTYBROWH))+;
    [",NPRCOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST5",LCSTYBROWH))+;
    [",TOTCOST :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TOTCST,OARIAAPPLICATION.GETHEADERTEXT("LANG_TOTCST",LCSTYBROWH))+[",]+;
    [AVE_COST :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_AVECST,OARIAAPPLICATION.GETHEADERTEXT("LANG_AVECST",LCSTYBROWH))+;
    [",NSTKVAL :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STKVAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_STKVAL",LCSTYBROWH))+;
    [",SOLDOUT :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SOLDDT,OARIAAPPLICATION.GETHEADERTEXT("LANG_SOLDDT",LCSTYBROWH))+;
    [",START :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STRTDT,OARIAAPPLICATION.GETHEADERTEXT("LANG_STRTDT",LCSTYBROWH))+;
    [",LOCATION :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DBIN,OARIAAPPLICATION.GETHEADERTEXT("LANG_DBIN",LCSTYBROWH))+;
    [",LINVSTY :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_INVSTY,OARIAAPPLICATION.GETHEADERTEXT("LANG_INVSTY",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS+;
    [MARKA :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKA,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKA",LCSTYBROWH))+;
    [",MARKB:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKB,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKB",LCSTYBROWH))+;
    [",MARKC :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKC,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKC",LCSTYBROWH))+[",]+;
    [CCONSINFO1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI1,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI1",LCSTYBROWH))+;
    [",CCONSINFO2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI2,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI2",LCSTYBROWH))+["]

ELSE

  LCBRFIELDS = [CSTYMAJOR:30:H=ALLTRIM(lcStyTtl),STYLE_A.DESC:20:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROWH))+;
    [",STYLE_A.DESC1:35:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LDESC",LCSTYBROWH))+[",]+;
    [lcSesDesc=gfCodDes(STYLE_A.Season,'SEASON'):20:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SEASON,OARIAAPPLICATION.GETHEADERTEXT("LANG_SEASON",LCSTYBROWH))+;
    [",lcDivDesc=gfCodDes(STYLE_A.cdivision,'CDIVISION'):20:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DIV,OARIAAPPLICATION.GETHEADERTEXT("LANG_DIV",LCSTYBROWH))+[",]


  LCBRFIELDS = LCBRFIELDS+;
    [STYLE_A.pricea:10:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROWH))+;
    [",STYLE_A.PRICEB:10:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEB,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEB",LCSTYBROWH))+;
    [",STYLE_A.PRICEC:10:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEC,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEC",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS+;
    [lnTotWIP=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'TotWip'):12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROWH))+[",]+;
    [lnTotStk=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'totstk'):12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROWH))+[",]+;
    [lnTotORD=lfSumSeasn(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen),'totord'):12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROWH))+[",]+;
    [lnOts   =lfSum_OTS(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),lnMajLen)):12:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROWH))+[",]

  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][Start]
  *!*	    lcBrFields = lcBrFields+;
  *!*	                 [Style_A.Fabric:12:h="]+LANG_FABRIC+[",Style_A.CSTYGRADE:H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(STYLE_A.ROYALTY,'ROYALTY') :15 :H="]+LANG_ROYAL+[" , STYLE_A.PATTERN :H="]+LANG_P
  LCBRFIELDS = LCBRFIELDS+;
    [Style_A.cprifabric:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROWH))+;
    [",Style_A.CSTYGRADE:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GRADE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GRADE",LCSTYBROWH))+;
    [", lcRoyDesc=gfCodDes(STYLE_A.ROYALTY,'ROYALTY') :15 :H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ROYAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ROYAL",LCSTYBROWH))+;
    [" , STYLE_A.PATTERN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PATRN,OARIAAPPLICATION.GETHEADERTEXT("LANG_PATRN",LCSTYBROWH))+;
    [", STYLE_A.STATUS :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_STATUS",LCSTYBROWH))+[",]

  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][End]
  LCBRFIELDS = LCBRFIELDS + ;
    [Style_A.SCALE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SCALE,OARIAAPPLICATION.GETHEADERTEXT("LANG_SCALE",LCSTYBROWH))+;
    [", Style_A.PREPAK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PREPAK,OARIAAPPLICATION.GETHEADERTEXT("LANG_PREPAK",LCSTYBROWH))+;
    [", Style_A.CBUYPREPK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BPREPK,OARIAAPPLICATION.GETHEADERTEXT("LANG_BPREPK",LCSTYBROWH))+;
    [", Style_A.QTY_CTN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_QTYCRT,OARIAAPPLICATION.GETHEADERTEXT("LANG_QTYCRT",LCSTYBROWH))+;
    [", Style_A.COMMISSION :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COMM,OARIAAPPLICATION.GETHEADERTEXT("LANG_COMM",LCSTYBROWH))+;
    [", Style_A.LINK_CODE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LNKCD,OARIAAPPLICATION.GETHEADERTEXT("LANG_LNKCD",LCSTYBROWH))+[",]+;
    [lcMaked = IIF(Style_A.MAKE,'Y','N') :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MAKE,OARIAAPPLICATION.GETHEADERTEXT("LANG_MAKE",LCSTYBROWH))+;
    [", Style_A.NMCOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST1",LCSTYBROWH))+;
    [" , Style_A.NMCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST2",LCSTYBROWH))+;
    [", Style_A.NMCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST3",LCSTYBROWH))+;
    [", Style_A.NMCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST4",LCSTYBROWH))+[",]


  LCBRFIELDS = LCBRFIELDS + ;
    [Style_A.NICOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST1",LCSTYBROWH))+;
    [", Style_A.NICOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST2",LCSTYBROWH))+;
    [", Style_A.NICOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST3",LCSTYBROWH))+;
    [", Style_A.NICOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST4",LCSTYBROWH))+;
    [", Style_A.NICOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST5",LCSTYBROWH))+[",]

  LCBRFIELDS = LCBRFIELDS+;
    [STYLE_A.NPRCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST2",LCSTYBROWH))+;
    [",STYLE_A.NPRCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST3",LCSTYBROWH))+;
    [",STYLE_A.NPRCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST4",LCSTYBROWH))+;
    [",STYLE_A.NPRCOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST5",LCSTYBROWH))+;
    [",STYLE_A.TOTCOST :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TOTCST,OARIAAPPLICATION.GETHEADERTEXT("LANG_TOTCST",LCSTYBROWH))+[",]+;
    [lnAvegCost=lfCalAvCst() :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_AVECST,OARIAAPPLICATION.GETHEADERTEXT("LANG_AVECST",LCSTYBROWH))+;
    [",STYLE_A.SOLDOUT :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SOLDDT,OARIAAPPLICATION.GETHEADERTEXT("LANG_SOLDDT",LCSTYBROWH))+;
    [",STYLE_A.START :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STRTDT,OARIAAPPLICATION.GETHEADERTEXT("LANG_STRTDT",LCSTYBROWH))+;
    [",STYLE_A.LOCATION :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DBIN,OARIAAPPLICATION.GETHEADERTEXT("LANG_DBIN",LCSTYBROWH))+;
    [",STYLE_A.LINVSTY :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_INVSTY,OARIAAPPLICATION.GETHEADERTEXT("LANG_INVSTY",LCSTYBROWH))+[",]


  LCBRFIELDS = LCBRFIELDS+;
    [STYLE_A.MARKA :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKA,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKA",LCSTYBROWH))+;
    [",STYLE_A.MARKB:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKB,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKB",LCSTYBROWH))+;
    [",STYLE_A.MARKC :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKC,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKC",LCSTYBROWH))+[",]+;
    [STYLE_A.CCONSINFO1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI1,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI1",LCSTYBROWH))+;
    [",STYLE_A.CCONSINFO2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI2,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI2",LCSTYBROWH))+["]

ENDIF


LNCURALIAS = SELECT()
SELECT 0

PRIVATE LLSTYLE_A
LLSTYLE_A = .F.
IF !USED('STYLE_A')
  LLSTYLE_A = .T.
  USE (OARIAAPPLICATION.DATADIR+'STYLE') AGAIN ALIAS STYLE_A ORDER TAG STYLE
ENDIF

SELECT STYLE
LCSTYORDER = TAG()
SET ORDER TO TAG CSTYLE
DO CASE
CASE '*' $ XCLR AND '*' $ XSEASON
  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][Start]
  *!*	    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
  *!*	                 [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
  *!*	                 [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
  LCBRFIELDS = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROWH))+[",]+;
    [pricea :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROWH))+;
    [",totWip :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROWH))+;
    [",totstk :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROWH))+[",]+;
    [totord :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROWH))+;
    [",cprifabric :19 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROWH))+["]

  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][End]
CASE '*' $ XCLR
  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][Start]
  *!*	    lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
  *!*	                 [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
  *!*	                 [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
  LCBRFIELDS = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROWH))+[",]+;
    [pricea :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROWH))+;
    [",totWip :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROWH))+;
    [",totstk :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROWH))+[",]+;
    [totord :14 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROWH))+;
    [",cprifabric :19 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROWH))+["]


  *B609825,1 MMT 02/09/2012 Browse cprifabric instead of fabric in style browse[T20120117.0008][End]
  *B128950,1 AMH Fix bug of variable not found when browse [Start]
  *lcScope = [=gfvSeason]
  LCSCOPE = [=gfvSeason()]
  *B128950,1 AMH [End]

OTHERWISE
  IF !('*' $ XSEASON)

    *B128950,1 AMH Fix bug of variable not found when browse [Start]
    *lcScope = [=gfvSeason]
    LCSCOPE = [=gfvSeason()]
    *B128950,1 AMH [End]

  ENDIF
ENDCASE

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
LCSCOPE = IIF(TYPE('lcValidFun')='C' AND !EMPTY(LCVALIDFUN),LCVALIDFUN ,LCSCOPE)
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

SELECT STYLE

IF LCMAJORORNON = 'M'
  SET ORDER TO CSTYLE
ELSE
  SET ORDER TO STYLE
ENDIF
SET RELATION TO PADR(STYLE.CSTYMAJOR,LNMAJLEN) INTO STYLE_A ADDI
IF LCMAJORORNON = 'M'
  LCSTYLE = [XSTYLE]
ELSE
  IF LCMAJORORNON $ 'N'
    SELECT STYLE
    LCSTYLE = [XSTYLE]
  ENDIF
ENDIF

IF !SEEK(XSTYLE)
  LNSOFTSEEK = IIF(SEEK(ALLT(XSTYLE),'STYLE'),RECNO(),RECNO(0))
  IF  LNSOFTSEEK > 0
    GO LNSOFTSEEK
  ELSE
    GO TOP
  ENDIF
ENDIF

*! B038623,1 MAH 10/12/2004 [BEGIN]
*-- IF TYPE('lcStyle')='C'
*--   IF lcMajorOrNon = 'N'
*--     llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*--   ELSE
*--     llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*--   ENDIF
*-- ELSE
*--   llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,STYLE_A.SEASON","laData")
*-- ENDIF
IF TYPE('lcStyle') = 'C'
  IF LCMAJORORNON = 'N'
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
    *llWasSel = ARIABROW(lcStyle, lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
    "STYLE,STYLE_A.SEASON", "laData", ;
    .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A1')
    LLWASSEL = ARIABROW(LCSTYLE, LCTITLE, GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2, LCSCOPE, "", ;
      "STYLE,STYLE_A.SEASON", "laData", ;
      .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A1',LCCURSOR,LCSELFLD)
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

  ELSE
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
    *llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", ;
    "STYLE,STYLE_A.SEASON", "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A2')
    LLWASSEL = ARIABROW('', LCTITLE, GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2, LCSCOPE, "", ;
      "STYLE,STYLE_A.SEASON", "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A2',LCCURSOR,LCSELFLD)
    *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[ENd]
  ENDIF
ELSE
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
  *llWasSel = ARIABROW('', lcTitle, gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2, lcScope, "", "STYLE,STYLE_A.SEASON", ;
  "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A3')
  LLWASSEL = ARIABROW('', LCTITLE, GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2, LCSCOPE, "", "STYLE,STYLE_A.SEASON", ;
    "laData", .F., .F., .F., .F., .F., .F., .F., .F., .F., 'A3',LCCURSOR,LCSELFLD)
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
ENDIF
*! B038623,1 MAH 10/12/2004 [END]

IF LCMAJORORNON = 'M'
  SET SKIP TO
  SET RELATION OFF INTO STYLE_A
ENDIF
IF LLWASSEL
  XSTYLE  = LADATA[1]

  *B128950,1 AMH Fix bug of Out side of range [Start]
  *XCLR    = laData[2]
  XCLR    = SPACE(6)
  *B128950,1 AMH [End]

  SET ORDER TO TAG STYLE
  SEEK XSTYLE
ELSE
  XSTYLE  = SPACE(12)
  XCLR    = SPACE(6)
ENDIF

IF !EMPTY(LCSTYORDER)
  SELECT STYLE
  SET ORDER TO TAG (LCSTYORDER)
ELSE
  SET ORDER TO
ENDIF

IF LLSTYLE_A
  USE IN STYLE_A
ENDIF

IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
IF  (TYPE('lcCursor')= 'C' AND !EMPTY(LCCURSOR)) AND (TYPE('lcSelFld')= 'C' AND !EMPTY(LCSELFLD))
  RETURN LLWASSEL
ELSE
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

  RETURN XSTYLE

  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
ENDIF
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]

*!**************************************************************************
*! FUNCTION gfvSeason
*! program to validate the style selected by the user in the browse
*! if it use the same season or not
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION GFVSEASON

IF STYLE_A.SEASON = XSEASON  OR ALLTRIM(STYLE_A.SEASON) = 'Y'
ELSE
  =MESSAGEBOX("You are restricted to styles from season "+XSEASON+"! RETRY")
ENDIF

*!**************************************************************************
*! FUNCTION lfSumSeasn
*! FUNCTION to sum a specific field for the current style in style file
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION LFSUMSEASN
PARAMETERS LCSTYLE,LCCOMP
LNTOTCOMP = 0
SELECT STYLE_A
SUM &LCCOMP TO LNTOTCOMP WHILE STYLE=LCSTYLE
SELECT STYLE
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
IF BETWEEN(RECNO(),1,RECCOUNT())
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
  GO RECNO()
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
ENDIF
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
RETURN INT(LNTOTCOMP)

*!**************************************************************************
*! FUNCTION lfSum_OTS
*! FUNCTION to sum the open to sell quantities.
*! written by Reham Alallamy 04/26/95
*!**************************************************************************
FUNCTION LFSUM_OTS
PARAMETERS LCSTYLE

LNTOT_OTS = 0
SELECT STYLE_A
SUM (TOTWIP+TOTSTK-TOTORD) TO LNTOT_OTS WHILE STYLE=LCSTYLE
SELECT STYLE
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
IF BETWEEN(RECNO(),1,RECCOUNT())
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
  GO RECNO()
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[Start]
ENDIF
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[End]
RETURN INT(LNTOT_OTS)


*!*************************************************************
*! Name      : lfCalAvCst
*! Developer : Hossam El Etreby [HDM]
*! Date      : 06/06/1999
*! Purpose   : Function to Calculate Average cost for all style colors
*!*************************************************************
*! Returns            : Average cost
*!*************************************************************
FUNCTION LFCALAVCST

PRIVATE LNTOTSTK, LNTOTSTVAL , LNAVCSTVAL
STORE 0 TO LNTOTSTK , LNTOTSTK , LNAVCSTVAL

LNTOTSTK   = LFSUMSEASN(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),LNMAJLEN),'TotStk')
LNTOTSTVAL = LFSUMSEASN(PADR(ALLTRIM(STYLE_A.CSTYMAJOR),LNMAJLEN),'NSTKVAL')
IF LNTOTSTK > 0
  LNAVCSTVAL = LNTOTSTVAL / LNTOTSTK
ELSE
  RETURN STYLE_A.AVE_COST
ENDIF
RETURN (LNAVCSTVAL)



*!***************************************************************
* PROG: POSBROW
* DESC: PROGRAM TO BROWSE THE STYLE PURCHASE ORDER
* PARA: THE PO AND VENDOR IS PASSED AS A PARAMETER
*PARAMETER :lcAType='P' for p/o,
*                   'R' for return po
*                   'C' for contract
*--------------------------------------------------------------
*  lcVen - >  will hold the source location insted of vendor.
*!***************************************************************
PROCEDURE POSBROW
PARAMETER LCPO,LCVEN,LCATYPE

PRIVATE LCBRFIELDS,LCALIAS,LCVENCODE,LAPDATA
DECLARE LAPDATA[2] && array to get values from browse
LAPDATA=' '
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcTitle  = IIF(lcAType='R','Return Purchase Orders',;
*!*	  IIF(lcAType='C','Contracts',IIF(lcAType='D','Dye Purchase Order',;
*!*	  IIF(lcAType='A','Adornment Purchase Order','Style Purchase Orders'))))
*!*	lcTitle  = IIF(lcAType='N','Inter Location Purchase Orders',lcTitle)
*!*	lcPOType = IIF(lcAType='R','Return P/O',IIF(lcAType='C','Contract','P/O '))
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCTITLE  = IIF(LCATYPE='R',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_RETURNPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_RETURNPO",LCARIAHFILE)),;
  IIF(LCATYPE='C',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_CONTRACTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_CONTRACTS",LCARIAHFILE)),;
  IIF(LCATYPE='D',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_DYEPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_DYEPO",LCARIAHFILE)),;
  IIF(LCATYPE='A',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_ADORPO ,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_ADORPO",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_STYLEPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_STYLEPO",LCARIAHFILE))))))
LCTITLE  = IIF(LCATYPE='N',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_INTERLOCPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_INTERLOCPO",LCARIAHFILE)),;
  LCTITLE)
LCPOTYPE = IIF(LCATYPE='R',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_RETPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_RETPO",LCARIAHFILE)),;
  IIF(LCATYPE='C',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_CONTARCT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_CONTARCT",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_PO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_PO",LCARIAHFILE))))
*N000682,1 MMT 12/03/2012 Globalization changes[End]
LCVENCODE=IIF(!EMPTY(LCVEN),PADR(LCVEN,8)+LCATYPE,LCATYPE)

LCALIAS  = ALIAS()   && Save the current alias.
IF !(LCATYPE $ 'NA')
  *--Open the vendor file if not opened.
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'APVENDOR','Vencode','SH')
  LLAPLINK = (GFGETMEMVAR('M_ApLink')='Y')
  LCAPDIR  = ALLTRIM(GFGETMEMVAR('M_ApLink'))
ENDIF
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcBrFields =   [PO        :R :H=lcPOType+' #':12,]+;
*!*	  [Status    :R :H='S':4,]+;
*!*	  [Vendor    :R :H=IIF(lcAType$'NA','Source Loc.','Vendor'):15,]+;
*!*	  [lcVnName = IIF(lcAType$'NA',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':22,]+;
*!*	  [Entered   :R :H='Entered':15,]+;
*!*	  [Complete  :R :H='Complete':15,]+;
*!*	  [nStyOrder :R :H='Tot.Qty.':10,]+;
*!*	  [POTotal   :R :H='Amount':15,]+;
*!*	  [Receive   :R :H='Receive':10,]+;
*!*	  [Open      :R :H='Open':10]
LCBRFIELDS =   [PO        :R :H=lcPOType+' #':12,]+;
  [Status    :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STATUS_CR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_STATUS_CR",LCARIAHFILE))+[':4,]+;
  [Vendor    :R :H=IIF(lcAType$'NA',']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_SOURCELOC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_SOURCELOC",LCARIAHFILE))+;
  [',']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_VENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_VENDOR",LCARIAHFILE))+['):15,]+;
  [lcVnName = IIF(lcAType$'NA',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_NAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_NAME",LCARIAHFILE))+[':22,]+;
  [Entered   :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_ENTERED,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_ENTERED",LCARIAHFILE))+[':15,]+;
  [Complete  :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_COMPLETE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_COMPLETE",LCARIAHFILE))+[':15,]+;
  [nStyOrder :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_TOTQTY,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_TOTQTY",LCARIAHFILE))+[':10,]+;
  [POTotal   :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AMOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_AMOUNT",LCARIAHFILE))+[':15,]+;
  [Receive   :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_RECEIVE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_RECEIVE",LCARIAHFILE))+[':10,]+;
  [Open      :R :H=']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_OPEN,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_OPEN",LCARIAHFILE))+[':10]
*N000682,1 MMT 12/03/2012 Globalization changes[end]
SELECT POSHDR
IF !EMPTY(LCVEN)
  SET ORDER TO TAG POSHDRV
  IF !SEEK (LCVENCODE)
    *N000682,1 MMT 12/03/2012 Globalization changes[Start]
    *=MESSAGEBOX("There are no "+lcPOType+"s for this "+IIF(lcAType='N','location','vendor')+" on the file....!",64,_SCREEN.CAPTION)
    =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_THEREARENO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_THEREARENO",LCARIAHFILE))+;
      LCPOTYPE+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_FORTHIS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_FORTHIS",LCARIAHFILE))+;
      IIF(LCATYPE='N',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_SLOCATION,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_SLOCATION",LCARIAHFILE)),;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_SVENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_SVENDOR",LCARIAHFILE)))+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_ONFILE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_ONFILE",LCARIAHFILE)))
    *N000682,1 MMT 12/03/2012 Globalization changes[END]
    LCVEN = SPACE(8)
    LCPO  = SPACE(6)
    GO TOP
    SET ORDER TO TAG POSHDR
    RETURN .F.
  ENDIF
ELSE
  SET ORDER TO TAG POSHDR
  IF !SEEK(LCATYPE)
    *N000682,1 MMT 12/03/2012 Globalization changes[Start]
    *=MESSAGEBOX("There are no "+lcPOType+"s on the file....!",64,_SCREEN.CAPTION)
    =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_THEREARENO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_THEREARENO",LCARIAHFILE))+;
      LCPOTYPE+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_ONFILE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_ONFILE",LCARIAHFILE)))
    *N000682,1 MMT 12/03/2012 Globalization changes[END]
    LCVEN = SPACE(8)
    LCPO  = SPACE(6)
    GO TOP
    RETURN .F.
  ENDIF
ENDIF

SELECT POSHDR
LCPOSREL = SET('RELATION')
IF LCATYPE $ 'NA'
  SET RELATION TO PADR(VENDOR,6) INTO WAREHOUS
ELSE
  SET RELATION TO VENDOR INTO APVENDOR
ENDIF
*--Call Global Browse.
*llWasSel=gfBrows([lcVenCode],"Po,Vendor","laPData",lcTitle)
LLWASSEL=ARIABROW([lcVenCode],LCTITLE,0,0,0,0,'','',"Po,Vendor","laPData",.T.,"POSHDR",.F.)

SET RELATION TO &LCPOSREL
IF LLWASSEL
  LCPO  = LAPDATA[1]
  LCVEN = LAPDATA[2]
ELSE
  LCVEN = SPACE(8)
  LCPO  = SPACE(6)
ENDIF

SELECT POSHDR
SET ORDER TO TAG POSHDR
RETURN LLWASSEL

***********
*!*************************************************************
*! Name      : gfBrowPref
*! Developer : Hesham El-Sheltawi
*! Date      : 1993-1995
*! Purpose   : Get the programm that called the browse to create the prefrence of the browse
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : Browse fields
*!                      Alias name
*!
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION GFBROWPREF
PARAMETERS LCBRCMDFIELDS,LCALIASNAME,LLCMTPRGTREE
PRIVATE LCPROGNAME,LCUSRFILE,LCPREF,LCFIELDS,LCSELECTALIAS

*-- User resource file
LCUSRFILE   = 'X'+SYS(2007,PADR(OARIAAPPLICATION.USER_ID,10))
*-- Browse file
LCALIASNAME = IIF(TYPE('lcAliasName')='C',LCALIASNAME,'')
LCALIASNAME = IIF(EMPTY(LCALIASNAME),IIF(INLIST(LEFT(ALIAS(),1),'X','T'),'',ALIAS()),LCALIASNAME)

IF EMPTY(LCALIASNAME) OR !(OARIAAPPLICATION.RESOURCEHOME $ SET('RESO',1))
  RETURN "Fields "+LCBRCMDFIELDS
ENDIF
*-- Get calling program name
LCPROGNAME = ''
IF !LLCMTPRGTREE
  LNCOUNT = 2
  LCPROGNAME = IIF(LEFT(PROG(1),2)='ON','',PROG(1))
  DO WHILE !('GFBROWS' $ SYS(16,LNCOUNT)) AND !('ARIABROW' $ SYS(16,LNCOUNT)) AND !EMPTY(SYS(16,LNCOUNT))
    LCPROGNAME = LCPROGNAME + IIF(LEFT(PROG(LNCOUNT),2)='ON','',PROG(LNCOUNT))
    IF !('GFBROWS' $ SYS(16,LNCOUNT)) AND !('ARIABROW' $ SYS(16,LNCOUNT))
      LNCOUNT = LNCOUNT +1
    ENDIF
  ENDDO
  LCPROGNAME = LCPROGNAME + SUBSTR(SYS(16,LNCOUNT),ATC('IN ',SYS(16,LNCOUNT)))
ENDIF
LCPREF   = 'W'+SYS(2007,PADR(LCPROGNAME+LCALIASNAME,340))
LCFIELDS = IIF(EMPTY(LCBRCMDFIELDS),'','FIELDS '+LCBRCMDFIELDS)
LCSELECTALIAS = SELECT()
IF OARIAAPPLICATION.RESOURCEHOME $ SET('RESOURCE',1) AND ;
    SET('RESOURCE')='ON' AND FILE(OARIAAPPLICATION.RESOURCEHOME+LCUSRFILE+'.DBF')
  LCFIELDS=''
  IF !USED(LCUSRFILE)
    SELECT 0
    USE (OARIAAPPLICATION.RESOURCEHOME+LCUSRFILE) ORDER TAG CUSERPREF
  ENDIF
  SELECT (LCUSRFILE)
  SET ORDER TO TAG CUSERPREF
  =SEEK(LCPREF)
  IF !FOUND() OR !(ALLTRIM(LCBRCMDFIELDS) == ALLTRIM(MBROWFIELD)) OR ;
      !(ALLTRIM(MBROWFIELD) == ALLTRIM(LCBRCMDFIELDS))
    IF !FOUND()
      APPEND BLANK
    ELSE
      SELECT 0
      USE (SYS(2005)) AGAIN ORDER TAG 1
      IF SEEK(PADR('PREF'+'W',12)+PADR('WINDBROW',12)+PADR('B'+LCPREF,24))
        REPLACE DATA WITH ''
      ENDIF
      USE
      SELECT (LCUSRFILE)
    ENDIF
    REPLACE CUSERPREF  WITH LCPREF,;
      MBROWFIELD WITH LCBRCMDFIELDS
    LCFIELDS = IIF(EMPTY(LCBRCMDFIELDS),'','FIELDS '+LCBRCMDFIELDS)
  ENDIF
ENDIF
IF USED(LCUSRFILE)
  USE IN (LCUSRFILE)
ENDIF
SELECT (LCSELECTALIAS)
LCPREF = 'PREF B'+LCPREF+' '
RETURN LCPREF+LCFIELDS

*!*************************************************************
*! Name      : gfCodDes
*! Developer : Malak Hanna Aziz
*! Date      : 1993-1995
*! Purpose   : Get Code Description
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : lcCodeVal, lcFldName , llChkEdit
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION GFCODDES
PARAMETERS LCCODEVAL, LCFLDNAME , LLCHKEDIT

*-- MAB Local variables ... BEGIN
LOCAL LNCURRENTALIAS, LCSAVORDER
*-- MAB Local variables ... END

PRIVATE LCRETURNVAL,LCSAVFLTR
LCSAVFLTR = ""

LLRETARRAY = (TYPE("lcCodeVal[1,1]") = "C")
IF !LLRETARRAY AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !LLRETARRAY
  LCCODEVAL   = PADR(UPPER(LCCODEVAL),6)
  LCFLDNAME   = PADR(UPPER(LCFLDNAME),10)
ENDIF

LCRETURNVAL = ""
LNCURRENTALIAS  = SELECT(0)      && Variable to save the currently selected file.

LLUSECODES = .F.
*B611588,1 Es 05/29/2018 Modify function GFCODDES to add some lines[Begin]
IF USED("CODES")
  * B611601,1 MMT 06/12/2018 Error while adding new colors to style in Style screen [T20180611.0017][Start]
  SELECT "CODES"
  IF TAGCOUNT() =0
  * B611601,1 MMT 06/12/2018 Error while adding new colors to style in Style screen [T20180611.0017][End]
    USE IN "CODES"
  * B611601,1 MMT 06/12/2018 Error while adding new colors to style in Style screen [T20180611.0017][Start]
  ENDIF
  * B611601,1 MMT 06/12/2018 Error while adding new colors to style in Style screen [T20180611.0017][End]
ENDIF
*B611588,1 Es 05/29/2018 Modify function GFCODDES to add some lines[End]

IF !USED("CODES")
  USE (OARIAAPPLICATION.DATADIR+"Codes") IN 0 AGAIN
  LLUSECODES = .T.
ENDIF

SELECT CODES               && Select CODES file
LCSAVORDER = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order

*-- if pass array of codes.
IF LLRETARRAY
  PRIVATE LNARRLEN , LNCODELEN
  LNCODELEN = 6
  LNARRLEN  = 0
  FOR LNARRLEN = 1 TO ALEN(LCCODEVAL,1)
    LCCODEVAL[lnArrLen,1] = PADR(UPPER(LCCODEVAL[lnArrLen,1]),6)
    LCCODEVAL[lnArrLen,2] = PADR(UPPER(LCCODEVAL[lnArrLen,2]),10)
    IF EMPTY(LCCODEVAL[lnArrLen,1]) .OR. LCCODEVAL[lnArrLen,1] = "*"
      LCCURFLT = FILTER()
      LNFLTREC = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO

      IF !SEEK(SPACE(1)+LEFT(LCCODEVAL[lnArrLen,1],1))
        APPEND BLANK
        REPLACE CFLD_NAME  WITH IIF(LCCODEVAL[lnArrLen,1] = '*','ALL','N/A') ;
          CCODE_NO   WITH IIF(LCCODEVAL[lnArrLen,1] = '*','*','')      ;
          CDISCREP   WITH IIF(LCCODEVAL[lnArrLen,1] = '*','All','N/A') ;
          CRLTFIELD  WITH 'N'
      ENDIF
      LCCODEVAL[lnArrLen,3] = CODES.CDISCREP
      SET FILTER TO &LCCURFLT.
      IF BETWEEN(LNFLTREC,1,RECCOUNT())
        GO LNFLTREC
      ENDIF

    ELSE

      IF SEEK('N' + LCCODEVAL[lnArrLen,1] + "N" + LCCODEVAL[lnArrLen,2])
        LCCODEVAL[lnArrLen,3] = CODES.CDISCREP
      ELSE
        LCCODEVAL[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF

    ENDIF

    IF !EMPTY(LCCODEVAL[lnArrLen,3]) AND GFISEDTBLE(ALLTRIM(LCCODEVAL[lnArrLen,2]) , @LNCODELEN)
      LCCODEVAL[lnArrLen,3] = PADR(LCCODEVAL[lnArrLen,1],LNCODELEN) + '-' + LCCODEVAL[lnArrLen,3]
    ENDIF

  ENDFOR

ELSE && Pass one code only

  IF EMPTY(LCCODEVAL) .OR. LCCODEVAL = "*"
    LCCURFLT = FILTER()
    LNFLTREC = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    IF !SEEK(SPACE(1)+LEFT(LCCODEVAL,1))
      APPEND BLANK
      REPLACE CFLD_NAME  WITH IIF(LCCODEVAL = '*','ALL','N/A') ;
        CCODE_NO   WITH IIF(LCCODEVAL = '*','*','')      ;
        CDISCREP   WITH IIF(LCCODEVAL = '*','All','N/A') ;
        CRLTFIELD  WITH 'N'
    ENDIF
    LCRETURNVAL = CODES.CDISCREP
    SET FILTER TO &LCCURFLT.
    IF BETWEEN(LNFLTREC,1,RECCOUNT())
      GO LNFLTREC
    ENDIF
  ELSE
    LCSAVFLTR = FILTER()
    LNFLTREC = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO

    IF SEEK('N' + LCCODEVAL + "N" + LCFLDNAME)
      LCRETURNVAL = CODES.CDISCREP
    ELSE
      LCRETURNVAL = ''       && In case of this code record is deleted
    ENDIF

    SET FILTER TO &LCSAVFLTR
    IF BETWEEN(LNFLTREC,1,RECCOUNT())
      GO LNFLTREC
    ENDIF
  ENDIF

  PRIVATE LNCODELEN
  LNCODELEN = 6
  IF LLCHKEDIT AND !EMPTY(LCRETURNVAL) AND GFISEDTBLE(ALLTRIM(LCFLDNAME) , @LNCODELEN)
    LCRETURNVAL = PADR(LCCODEVAL,LNCODELEN) + '-' + LCRETURNVAL
  ENDIF

ENDIF

*-- MAB 12/08/2002 - Codes not the selected alias... BEGIN
SELECT CODES
*-- MAB 12/08/2002 - Codes not the selected alias... END

SET ORDER TO &LCSAVORDER
*!*  IF llUseCodes
*!*    USE IN Codes
*!*  ENDIF

SELECT (LNCURRENTALIAS)
RETURN LCRETURNVAL
*-- end of gfCodDes.



*!**********************************************************************
*! Name      : lfTrnType
*! Date      : 06/02/97
*! Purpose   : Function return the transaction title descreption.
*!**********************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION LFTRNTYPE
PARAMETERS LCTRTYPE,LLSTYMAKE,LCIRTYPE
PRIVATE LCTRNDESC
LCTRNDESC = ""
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	DO CASE
*!*	CASE lcTrType $ '1I'
*!*	  lcTrnDesc='Adjustment        '
*!*	CASE lcTrType ='2'
*!*	  lcTrnDesc='Physical Inventory'
*!*	CASE lcTrType ='3'
*!*	  lcTrnDesc='Invoice           '
*!*	CASE lcTrType ='4'
*!*	  lcTrnDesc='Void Invoice      '
*!*	CASE lcTrType ='5'
*!*	  lcTrnDesc='Receive C/T       '
*!*	CASE lcTrType ='6'
*!*	  IF llStyMake
*!*	    lcTrnDesc= IIF(lcIRType='I','Issue Adornment order','Receive Mfg. Order')
*!*	  ELSE
*!*	    lcTrnDesc= IIF(lcIRType='I','Issue','Receive')+' P/O       '
*!*	  ENDIF
*!*	CASE lcTrType ='7'
*!*	  lcTrnDesc='Return Merchandise'
*!*	CASE lcTrType ='8'
*!*	  lcTrnDesc='Void Credit Memo  '
*!*	CASE lcTrType ='9'
*!*	  lcTrnDesc='Inventory Markdown'
*!*	ENDCASE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
DO CASE
CASE LCTRTYPE $ '1I'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_ADJ,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_ADJ",LCARIAHFILE))
CASE LCTRTYPE ='2'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_PHYINV,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_PHYINV",LCARIAHFILE))
CASE LCTRTYPE ='3'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_INVOICE,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_INVOICE",LCARIAHFILE))
CASE LCTRTYPE ='4'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_VINVOICE,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_VINVOICE",LCARIAHFILE))
CASE LCTRTYPE ='5'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_RECCT,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_RECCT",LCARIAHFILE))
CASE LCTRTYPE ='6'
  IF LLSTYMAKE
    LCTRNDESC= IIF(LCIRTYPE='I',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_ISSADOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_ISSADOR",LCARIAHFILE)),;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_RECMFG,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_RECMFG",LCARIAHFILE)))
  ELSE
    LCTRNDESC= IIF(LCIRTYPE='I',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_ISSUE ,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_ISSUE",LCARIAHFILE)),;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_REC,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_REC",LCARIAHFILE)))+' '+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_PO,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_PO",LCARIAHFILE))+'       '
  ENDIF
CASE LCTRTYPE ='7'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_RM,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_RM",LCARIAHFILE))
CASE LCTRTYPE ='8'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_VCM,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_VCM",LCARIAHFILE))
CASE LCTRTYPE ='9'
  LCTRNDESC=IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TRANTYPE_INVMARKDOWN,OARIAAPPLICATION.GETHEADERTEXT("LANG_TRANTYPE_INVMARKDOWN",LCARIAHFILE))
ENDCASE
*N000682,1 MMT 12/09/2012 Globalization changes[END]
RETURN LCTRNDESC


*!*************************************************************
*! Name      : gfPrePBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 05/02/1995
*! Purpose   : Function to validate entered prepack value for a
*!             specific size scale.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  Entered prepack
*!*************************************************************
*! Returns            :  .T. --> Valid scale
*!                       .F. --> Invalid scale
*!*************************************************************
*! Example            :  gfPrePBrow(m.Scale,@m.Prpak)
*!*************************************************************
FUNCTION GFPREPBROW
PARAMETER LCSCALE,LCPREPACK,LLBROWSE

IF !USED('SCALE')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'SCALE','SCALE','SH')
ENDIF
IF LLBROWSE OR (!EMPTY(LCPREPACK) AND !SEEK('P'+LCSCALE+LCPREPACK,'SCALE'))
  IF SEEK('P'+LCSCALE,'SCALE')
    LNOLDALIAS = SELECT()
    SELECT SCALE
    DECLARE LAVALUES[1]  && array to get values from browse
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *!*	    lcFile_Ttl = 'Prepacks'
    *!*	    lcBrFields = "Prepak :H='Prepack'"
    LOCAL LCARIAHFILE
    LCARIAHFILE = ''
    IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
      LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
    ENDIF
    LCFILE_TTL = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PPBROW_PREPACKS,OARIAAPPLICATION.GETHEADERTEXT("LANG_PPBROW_PREPACKS",LCARIAHFILE))
    LCBRFIELDS = "Prepak :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PPBROW_SINGLEPREPACK,OARIAAPPLICATION.GETHEADERTEXT("LANG_PPBROW_SINGLEPREPACK",LCARIAHFILE))+"'"
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    IF SEEK('S'+LCSCALE,'SCALE')
      FOR LNCOUNTER = 1 TO SCALE.CNT
        LCPOS = STR(LNCOUNTER,1)
        LCBRFIELDS =LCBRFIELDS+",PP&lcPos:H='"+SCALE.SZ&LCPOS+"' :6"
      ENDFOR
    ENDIF
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *lcBrFields = lcBrFields + ",ppTot:H='Total' :8"
    LCBRFIELDS = LCBRFIELDS + ",ppTot:H='"+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PPBROW_TOTAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_PPBROW_TOTAL",LCARIAHFILE))+;
      "' :8"
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    =SEEK("P"+LCSCALE,'Scale')
    =ARIABROW(["P"+lcScale],LCFILE_TTL,GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,'','','PREPAK','laValues')
    LCPREPACK = IIF(EMPTY(LAVALUES[1]),SPACE(1),LAVALUES[1])
    SELECT (LNOLDALIAS)
  ELSE
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=MESSAGEBOX('There is no prepak for scale '+ lcScale,64,_SCREEN.CAPTION)
    LOCAL LCARIAHFILE
    LCARIAHFILE = ''
    IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
      LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
    ENDIF
    =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PREPACK_NOPAREPACK,OARIAAPPLICATION.GETHEADERTEXT("LANG_PREPACK_NOPAREPACK",LCARIAHFILE))+' '+ LCSCALE)
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    LCPREPACK = SPACE(1)
  ENDIF
ENDIF

RETURN !EMPTY(LCPREPACK)

*!**************************************************************************
*! PROG: CUSBROWM.PRG
*! program to browse through the Customer file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*!
*!**************************************************************************
PROCEDURE CUSBROWM
PARAMETERS XACCOUNT,LLRETALIAS,LCMASTSTOR
IF EMPTY(LCMASTSTOR)
  RETURN CUSBROW(@XACCOUNT,'M',LLRETALIAS)
ELSE
  RETURN CUSBROW(@XACCOUNT,LCMASTSTOR,LLRETALIAS)
ENDIF

*!**************************************************************************
*! PROG: CUSBROWS.PRG
*! program to browse The stores for a specific Customer by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*!
*!**************************************************************************
PROCEDURE CUSBROWS
PARAMETERS XACCOUNT,LLRETALIAS
PRIVATE LCSTORE
LCSTORE=XSTORE
LLWASSEL=CUSBROW(@XACCOUNT,'S',@LCSTORE,LLRETALIAS)
XSTORE =LCSTORE
RETURN LLWASSEL

*!**************************************************************************
*! PROG: CUSBROW.PRG
*! program to browse The customer file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*! Called by CUSBROWM.PRG & CUSBROWS.PRG
*!**************************************************************************

PROCEDURE CUSBROW
PARAMETERS XACCOUNT,LCKEY,XSTORE,LLRETALIAS     && returns the account code of the customer selected
PRIVATE LCCITYHED , LCCITYWID , LCSTATHED , LCSTATWID , LNCURALIAS
PRIVATE LCBRFIELDS,LADATA

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\CUSBROW.H
*N000682,1 11/20/2012 MMT Globlization changes[Start]

LOCAL LCARIAHFILE,LCCUSTBROW
LCARIAHFILE = ''
LCCUSTBROW= ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  LCCUSTBROW =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("CUSBROW")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

LNCURALIAS = SELECT(0)
*-- Hesham (Start)
*-- select country from sycint remotely
*llSycInt = gfOpenFile(oAriaApplication.SysPath+'SycInt',oAriaApplication.SysPath+'Ccontcode','SH')
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYCINT where ccont_code='"+OARIAAPPLICATION.DEFAULTCOUNTRY+"'",'',"SYCINTTMP","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
IF LNREMRESULT=1
  LOCATE
ELSE
  =GFDIALOG("I","Error Connecting to system database.")
  RETURN .F.
ENDIF
*!*	lnSycIntRc = RECNO('SycInt')
*!*	=SEEK(oAriaApplication.DefaultCountry,'SycInt','Ccontcode')
*!*	lcCityHed = SycInt.Cpart3Lab
*!*	lcCityWid = ALLTRIM(STR(SycInt.Npart3Len))
*!*	lcStatHed = SycInt.Cpart4Lab
*!*	lcStatWid = ALLTRIM(STR(SycInt.Npart4Len))

LCCITYHED = SYCINTTMP.CPART3LAB
LCCITYWID = ALLTRIM(STR(SYCINTTMP.NPART3LEN))
LCSTATHED = SYCINTTMP.CPART4LAB
LCSTATWID = ALLTRIM(STR(SYCINTTMP.NPART4LEN))
*-- Hesham (End)

DECLARE LADATA[2]  && array to get values from browse
IF !EMPTY(XACCOUNT)
  XACCOUNT = PADR(IIF(ATC("?",XACCOUNT)<>0,STUFF(XACCOUNT,ATC("?",XACCOUNT),1,""),XACCOUNT),5)
ENDIF
STORE '' TO LADATA
LLWASSEL=.T.
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE)
LCKEY=IIF(TYPE('lcKey')='C',LCKEY,'M')
*-- Hesham (Start)
*-- Change the sycint alias to sycinttmp
*!*	IF lcKey='S'
*!*	  lcBrFields = "STORE :h='Store',stName:23:h='Name',"+;
*!*	               "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	               "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	               "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	               "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	               "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	               "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	               "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	               "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	               "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	               "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	               "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep'"
*!*	ELSE
*!*	  IF lcKey='MSP'
*!*	     lcBrFields = "Phone1 :P=GFPHONETEM() :20 :H='Phone #',Account :H='Acct#',stName:20:h='Name',"+;
*!*	                  "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                  "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                  "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                  "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                  "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                  "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                  "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                  "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                  "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                  "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                  "Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	  ELSE
*!*	    IF  lcKey='MSN'
*!*	      lcBrFields = "stName:20:h='Name',Account :H='Acct#',"+;
*!*	                   "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	    ELSE
*!*	      lcBrFields = "Account :H='Acct#', BtName :H='Name':R,"+;
*!*	                   "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
*!*	                   "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
*!*	                   "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
*!*	                   "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
*!*	                   "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
*!*	                   "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
IF LCKEY='S'
  *N000682,1 MMT 12/03/2012 Globalization changes[Start]
  *!*	  lcBrFields = "STORE :h='"+LANG_LabelStore+"',stName:23:h='"+LANG_LabelName+"',"+;
  *!*	    "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
  *!*	    "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
  *!*	    "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
  *!*	    "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
  *!*	    "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
  *!*	    "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
  *!*	    "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
  *!*	    "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
  *!*	    "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
  *!*	    "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
  *!*	   "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"'"
  LCBRFIELDS = "STORE :h='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTORE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStore",LCCUSTBROW))+;
    "',stName:23:h='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelName",LCCUSTBROW))+"',"+;
    "cAddress1 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
    "cAddress2 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
    "cAddress3 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
    "cAddress4 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
    "cAddress5 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
    "cAddress12 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
    "cAddress22 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
    "cAddress32 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
    "cAddress42 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
    "cAddress52 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
    "Phone1 :P= GFPHONETEM() :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELPHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelPhone",LCCUSTBROW))+;
    "',Buyer :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBUYER,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBuyer",LCCUSTBROW))+;
    "',salesrep :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELREP,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelRep",LCCUSTBROW))+"'"
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
ELSE
  IF LCKEY='MSP'
    *N000682,1 MMT 12/03/2012 Globalization changes[Start]
    *!*	    lcBrFields = "Phone1 :P=GFPHONETEM() :20 :H='"+LANG_LabelPhone+"',Account :H='"+LANG_LabelAcc+"',stName:20:h='"+LANG_LabelName+"',"+;
    *!*	      "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
    *!*	      "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
    *!*	      "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
    *!*	      "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
    *!*	      "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
    *!*	      "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
    *!*	      "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
    *!*	      "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
    *!*	      "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
    *!*	      "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
    *!*	       "Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
    LCBRFIELDS = "Phone1 :P=GFPHONETEM() :20 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELPHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelPhone",LCCUSTBROW))+;
      "',Account :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcc",LCCUSTBROW))+;
      "',stName:20:h='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelName",LCCUSTBROW))+"',"+;
      "cAddress1 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      "cAddress2 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      "cAddress3 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      "cAddress4 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      "cAddress5 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      "cAddress12 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      "cAddress22 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      "cAddress32 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      "cAddress42 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      "cAddress52 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      "Buyer :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBUYER,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBuyer",LCCUSTBROW))+;
      "',salesrep :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELREP,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelRep",LCCUSTBROW))+;
      "',NetBal:11:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBALANCE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBalance",LCCUSTBROW))+"'"
    *N000682,1 MMT 12/03/2012 Globalization changes[END]
  ELSE
    IF  LCKEY='MSN'
      *N000682,1 MMT 12/03/2012 Globalization changes[Start]
      *!*	      lcBrFields = "stName:20:h='"+LANG_LabelName+"',Account :H='"+LANG_LabelAcc+"',"+;
      *!*	        "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      *!*	        "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      *!*	        "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      *!*	        "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      *!*	        "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      *!*	        "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      *!*	        "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      *!*	        "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      *!*	        "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      *!*	        "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      *!*	        "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
      LCBRFIELDS = "stName:20:h='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelName",LCCUSTBROW))+;
        "',Account :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcc",LCCUSTBROW))+"',"+;
        "cAddress1 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
        "cAddress2 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
        "cAddress3 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
        "cAddress4 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
        "cAddress5 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
        "cAddress12 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
        "cAddress22 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
        "cAddress32 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
        "cAddress42 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
        "cAddress52 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
        "Phone1 :P= GFPHONETEM() :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELPHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelPhone",LCCUSTBROW))+;
        "',Buyer :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBUYER,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBuyer",LCCUSTBROW))+;
        "',salesrep :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELREP,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelRep",LCCUSTBROW))+;
        "',NetBal:11:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBALANCE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBalance",LCCUSTBROW))+"'"
      *N000682,1 MMT 12/03/2012 Globalization changes[END]
    ELSE
      *N000682,1 MMT 12/03/2012 Globalization changes[Start]
      *!*	      lcBrFields = "Account :H='"+LANG_LabelAcc+"', BtName :H='"+LANG_LabelName+"':R,"+;
      *!*	        "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      *!*	        "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      *!*	        "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      *!*	        "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      *!*	        "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      *!*	        "cAddress12 :H='BT '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
      *!*	        "cAddress22 :H='BT '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
      *!*	        "cAddress32 :H='BT '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
      *!*	        "cAddress42 :H='BT '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
      *!*	        "cAddress52 :H='BT '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
      *!*	       "Phone1 :P= GFPHONETEM() :H='"+LANG_LabelPhone+"',Buyer :H='"+LANG_LabelBuyer+"',salesrep :H='"+LANG_LabelRep+"',NetBal:11:H='"+LANG_LabelBalance+"'"
      LCBRFIELDS = "Account :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcc",LCCUSTBROW))+;
        "', BtName :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelName",LCCUSTBROW))+"':R,"+;
        "cAddress1 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
        "cAddress2 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
        "cAddress3 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
        "cAddress4 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
        "cAddress5 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ST",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
        "cAddress12 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
        "cAddress22 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
        "cAddress32 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
        "cAddress42 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
        "cAddress52 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BT,OARIAAPPLICATION.GETHEADERTEXT("LANG_BT",LCCUSTBROW))+" '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len),"+;
        "Phone1 :P= GFPHONETEM() :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELPHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelPhone",LCCUSTBROW))+;
        "',Buyer :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBUYER,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBuyer",LCCUSTBROW))+;
        "',salesrep :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELREP,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelRep",LCCUSTBROW))+;
        "',NetBal:11:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELBALANCE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelBalance",LCCUSTBROW))+"'"
      *N000682,1 MMT 12/03/2012 Globalization changes[END]
    ENDIF
  ENDIF
ENDIF
*-- Hesham (End)
IF !USED('Customer')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'Customer','Customer','SH')
ENDIF
SELECT CUSTOMER
LCCUSTORDER = TAG()
IF LCKEY='MSP'
  SET ORDER TO TAG CUSTOMPH
ELSE
  IF LCKEY='MSN'
    SET ORDER TO TAG CUSTOMNM
  ELSE
    SET ORDER TO TAG CUSTOMER
  ENDIF
ENDIF
LNCUSREC=0
IF  IIF(LCKEY<>'MSN' AND LCKEY<>'MSP',!SEEK(LCKEY+XACCOUNT+IIF(LCKEY='S',XSTORE,''),'customer'),.T.) OR LLBROWSE
  IF LCKEY<>'MSN' AND LCKEY<>'MSP'
    LNSOFTSEEK=RECNO(0)
    IF LNSOFTSEEK<>0
      GO LNSOFTSEEK
    ELSE
      GO TOP
    ENDIF
  ENDIF
  IF LCKEY='S' OR LCKEY='MSP' OR LCKEY='MSN'
    IF LCKEY='S'
      IF !SEEK('S'+XACCOUNT)
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *=gfModalgen("INM40082B00000","ALERT",LANG_LabelStores+'|'+LANG_LabelAccount+' '+xAccount)
        =GFMODALGEN("INM40082B00000","ALERT",IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTORES,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStores",LCCUSTBROW))+;
          '|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAccount",LCCUSTBROW))+' '+XACCOUNT)
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        IF !EMPTY(LCCUSTORDER)
          SET ORDER TO TAG (LCCUSTORDER)
        ELSE
          SET ORDER TO
        ENDIF
        XSTORE=SPACE(8)
        RETURN .F.
      ENDIF
      IF LCKEY='S'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *llWasSel=ARIABROW(["S"+XACCOUNT],LANG_LabelStTitle+XACCOUNT ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"ACCOUNT,STORE","laData",.F.)
        LLWASSEL=ARIABROW(["S"+XACCOUNT],IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTTITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStTitle",LCCUSTBROW))+XACCOUNT ,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',"ACCOUNT,STORE","laData",.F.)
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      ELSE
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *llWasSel=ARIABROW(["M"+XACCOUNT],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',"ACCOUNT","laData",lcKey='M')
        LLWASSEL=ARIABROW(["M"+XACCOUNT],IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACTITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcTitle",LCCUSTBROW)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',"ACCOUNT","laData",LCKEY='M')
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
      LNCUSREC = RECNO('CUSTOMER')
    ELSE
      *ASM, going to first record if no value was seeked [Start]
      IF EMPTY(XACCOUNT)
        LOCATE
      ENDIF
      *ASM, going to first record if no value was seeked [End]
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *llWasSel=ARIABROW(['M','S'],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',IIF(lcKey='S',"ACCOUNT,STORE","ACCOUNT,STORE"),"laData",lcKey='M')
      LLWASSEL=ARIABROW(['M','S'],IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACTITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcTitle",LCCUSTBROW)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',IIF(LCKEY='S',"ACCOUNT,STORE","ACCOUNT,STORE"),"laData",LCKEY='M')
      *N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *llWasSel=ARIABROW(["M"],LANG_LabelAcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',IIF(lcKey='S',"ACCOUNT,STORE","ACCOUNT"),"laData",lcKey='M')
    LLWASSEL=ARIABROW(["M"],IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACTITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAcTitle",LCCUSTBROW)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',IIF(LCKEY='S',"ACCOUNT,STORE","ACCOUNT"),"laData",LCKEY='M')
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  IF LLWASSEL
    XACCOUNT  = LADATA[1]
    IF LCKEY='S' OR LCKEY='MSP' OR LCKEY='MSN'
      XSTORE = LADATA[2]
    ENDIF
  ELSE
    XACCOUNT = SPACE(5)
  ENDIF
ENDIF
IF !EMPTY(LCCUSTORDER)
  SET ORDER TO TAG (LCCUSTORDER)
ELSE
  SET ORDER TO
ENDIF
IF LLWASSEL AND LNCUSREC>0
  GO LNCUSREC
ENDIF
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
*-- Hesham (Start)
*-- Close the temprory alias sycinttmp
*!*	IF llSycInt
*!*	  USE IN 'SycInt'
*!*	ELSE
*!*	  IF BETWEEN(lnSycIntRc,1,RECCOUNT('SycInt'))
*!*	    GO lnSycIntRc IN 'SycInt'
*!*	  ENDIF
*!*	ENDIF
IF USED("SYCINTTMP")
  USE IN SYCINTTMP
ENDIF
*-- Hesham (End)

RETURN LLWASSEL

*!*************************************************************
*! Name      : gfPhoneTem
*! Developer : Hesham Shereef
*! Date      : 1993-1995
*! Purpose   : Fix the phone template
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*  FUNCTION RETURN THE PHONE TEMPLATE FORMAT "PICTURE"
*:->
FUNCTION GFPHONETEM

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*PARAMETER lcPhoneTem
LPARAMETER LCPHONETEM, LCCONT_ID
*B132026,1 WSH 05/16/2006 [End]

*IF PARAMETERS()>0
* RETURN "@R "+STRTRAN(lcPhoneTem,'#','X')
*ELSE
* RETURN "@R "+STRTRAN(oAriaApplication.PhoneMask,'#','X')
*ENDIF

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*IF PARAMETERS() = 0
*  *If the function get called at design time, the check on oAriaApplication will prevent errors
*  *while opening a form
*  lcPhoneTem = IIF(TYPE("oAriaApplication.PhoneMask")='U','',oAriaApplication.PhoneMask)
*ENDIF
IF PARAMETERS() = 0 OR TYPE("lcPhoneTem") # 'C' OR EMPTY(LCPHONETEM)
  LCPHONETEM = IIF(TYPE("oAriaApplication.PhoneMask") = 'U', '', OARIAAPPLICATION.PHONEMASK)
ENDIF

IF TYPE("lcCont_ID") = "C" AND !EMPTY(LCCONT_ID) AND LCCONT_ID # PADR(OARIAAPPLICATION.DEFAULTCOUNTRY,6)
  LOCAL LNREMRESULT, LNALIAS
  LNALIAS     = SELECT(0)
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("SELECT * FROM SYCINT WHERE cCont_Code = '" + LCCONT_ID + "'",;
    "", "TMPSYCINT", "", OARIAAPPLICATION.SYSTEMCONNECTIONSTRING, 3, "", SET("Datasession"))

  IF LNREMRESULT >= 1
    SELECT TMPSYCINT
    LOCATE
    LCPHONETEM = IIF(FOUND() AND !EMPTY(CPHONETEMP), ALLTRIM(CPHONETEMP), LCPHONETEM)
    USE IN TMPSYCINT
  ENDIF
  SELECT (LNALIAS)
ENDIF
*B132026,1 WSH 05/16/2006 [End]

RETURN STRTRAN(LCPHONETEM,'#','X')
*!**************************************************************************
*! PROG: REPCHK.PRG
*! program to browse through the SALESREP file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE SALESREP FILE HAS BEEN OPENED
*!
*!**************************************************************************
FUNCTION REPCHK
PARAMETERS XREPCODE,LLRETALIAS

PRIVATE LCBRFIELDS,LNCURALIAS,LADATA

LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE)

IF !LLBROWSE .AND. SEEK(XREPCODE,'SALESREP','SALESREP')
  DATA = SUBSTR(SALESREP.NAME,1,20)
  RETURN
ENDIF
DECLARE LADATA[3] && array to get values from browse
STORE '' TO LADATA
SELECT SALESREP
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcBrFields = [repCode:H="Code",Name:H="Name",Phone :P= gfPhoneTem() :H="Phone"]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCBRFIELDS = [repCode:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SALESREP_CODE,OARIAAPPLICATION.GETHEADERTEXT("LANG_SALESREP_CODE",LCARIAHFILE)) +;
  [",Name:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SALESREP_NAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_SALESREP_NAME",LCARIAHFILE)) +;
  [",Phone :P= gfPhoneTem() :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SALESREP_PHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_SALESREP_PHONE",LCARIAHFILE))+["]
*N000682,1 MMT 12/09/2012 Globalization changes[END]
LNCURALIAS = SELECT()

IF LLBROWSE OR !SEEK(XREPCODE,'SALESREP','SALESREP')
  DATA = SPACE(20)
  LNSOFTSEEK=RECNO(0)
  IF LNSOFTSEEK<>0 .AND. LNSOFTSEEK <= RECCOUNT("SALESREP")
    GO LNSOFTSEEK
  ELSE
    GO TOP
  ENDIF
  IF EOF()
    =GFMODALGEN("TRM00052B00000","ALERT")
    XREPCODE = SPACE(3)
  ELSE
    *N000682,1 12/19/2012 TMI Globlization changes[Start] translate the Sales Rep. title
    *IF ARIABROW('',"Sales Representatives",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'',"","REPCODE","laData")
    IF ARIABROW('',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SALES_REPRESENTATIVES,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Sales_Representatives",LCARIAHFILE)),;
        GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'',"","REPCODE","laData")
      *N000682,1 12/19/2012 TMI Globlization changes[End  ]
      XREPCODE  = LADATA[1]
      DATA = SUBSTR(SALESREP.NAME,1,20)
    ELSE
      XREPCODE = SPACE(3)
    ENDIF
  ENDIF
ENDIF
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetprice
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*************************************************************
FUNCTION GFGETPRICE
PARAMETERS LCSTYLE, LCLEVEL, LNQUANTITY, LCCUURCODE
PRIVATE LNPRICE, LNALIAS

LNALIAS = SELECT()
LCCUURCODE = IIF(TYPE('lcCuurCode')='C' AND !EMPTY(LCCUURCODE),LCCUURCODE,OARIAAPPLICATION.BASECURRENCY)
=SEEK(LCSTYLE,'Style')
IF LCLEVEL = 'Q'
  DO CASE
    *B132211,1 KHM 05/18/2006 [Begin]
    *CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
  CASE STYLE.NATQTYC > 0 AND LNQUANTITY >= STYLE.NATQTYC
    *B132211,1 KHM 05/18/2006 [End]
    LCLEVEL = 'C'
    *B132211,1 KHM 05/18/2006 [Begin]
    *CASE Style.nAtQtyB > 0 AND lnQuantity > Style.nAtQtyB
  CASE STYLE.NATQTYB > 0 AND LNQUANTITY >= STYLE.NATQTYB
    *B132211,1 KHM 05/18/2006 [End]
    LCLEVEL = 'B'
  OTHERWISE
    LCLEVEL = 'A'
  ENDCASE
ELSE
  *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
  *LCLEVEL=IIF(INLIST(LCLEVEL,'A','B','C'),LCLEVEL,'A')
  lcLEVEL=IIF(INLIST(LCLEVEL,'A','B','C','R'),LCLEVEL,'A')
  *E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][End]
ENDIF
*E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][Begin]
*LNPRICE = IIF(LCCUURCODE=OARIAAPPLICATION.BASECURRENCY,STYLE.PRICE&LCLEVEL, GFSTYPRICE(LCSTYLE,LCLEVEL,LCCUURCODE))
LNPRICE = IIF(LCCUURCODE=OARIAAPPLICATION.BASECURRENCY,IIF(LCLEVEL='R',style.nsugretpri ,STYLE.PRICE&lcLEVEL), GFSTYPRICE(LCSTYLE,lcLEVEL,LCCUURCODE))

*E303811,1 AHH 05/11/2017 Add Triggers to Customer screen for [T20170505.0005][End]  
  
SELECT (LNALIAS)
RETURN(LNPRICE)

*!*************************************************************
*! Name      : lfCheckPri
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lcCuurCode : Currency Code
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPri('A')
*!*************************************************************
FUNCTION LFCHECKPRI
PARAMETERS LCSTYLE,LCLEVEL, LCCUURCODE
LNPRICE = 0
DO FORM (OARIAAPPLICATION.SCREENHOME+"SOSTYPRI") WITH LCSTYLE,LCLEVEL, LCCUURCODE TO LNPRICE
RETURN(LNPRICE)

*!*************************************************************
*! Name      : gfGLBrowse
*! Developer : MALAK - Malak Hanna
*! Date      : 05/18/1995
*! Purpose   : To check and browse the gl link codes from gl_Link file .
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  : lcLinkType --> '01' for 'Sales' type
*!                                 --> '02' for 'Style Invertory' type
*!                                 --> '03' for 'Material Inverntory' type
*!                                 --> '04' for 'Work in Process' type
*!                                 --> '00' for main types
*!                                 --> ''   for all types except '00'
*!                      lcLinkCode --> Variables that holds entered link code
*!                      lcLinkDesc --> Variables that holds link descrption
*!                      lnSalesPart--> 0 Browse the whole sales types
*!                                     1 Browse Customer sales
*!                                     2 Browse Style sales
*!*************************************************************
*! Returns            :  .T. or .F.
*!*************************************************************
*! Example            : =gfGLBrowse('02',@m.LinkCode,@lcLinkDesc)
*!*************************************************************
FUNCTION GFGLBROWSE
PARAMETER LCLINKTYPE, LCLINKCODE, LCLINKDESC,LNSALESPART,LLBROWSE

PRIVATE   LCLINKTYPE, LCLINKCODE, LCLINKDESC,LADATA,LCBRFIELDS

LNOLDALIAS = SELECT()
LLOPNGLLNK = .F.
IF USED('GL_LINK')
  LNOLDTAG = ORDER('GL_LINK')
ELSE
  LLOPNGLLNK = GFOPENFILE(OARIAAPPLICATION.DATADIR+'GL_LINK','','SH')
ENDIF

IF LCLINKTYPE='02' .AND. LNSALESPART=2
  SET ORDER TO TAG SALES IN GL_LINK
ELSE
  SET ORDER TO TAG GL_LINK1 IN GL_LINK
ENDIF

LLBROWSE = IIF(TYPE('llBrowse') = 'U' ,.F., LLBROWSE .OR. '?' $ LCLINKCODE)

*-- MAB 05/28/2003 Avoid browsing in View mode ..... BEGIN
LOCAL LLVALIDATE
LLVALIDATE = (TYPE("_screen.ActiveForm.Parent.ActiveMode") != "C") OR;
  _SCREEN.ACTIVEFORM.PARENT.ACTIVEMODE != "V"
*IF llBrowse OR !SEEK(lcLinkType+lcLinkCode,'GL_LINK')
IF LLVALIDATE AND (LLBROWSE OR !SEEK(LCLINKTYPE+LCLINKCODE,'GL_LINK'))
  *-- MAB 05/28/2003 Avoid browsing in View mode ..... END
  LNPOS    = IIF(LCLINKTYPE='02' .AND. LNSALESPART=2,4,1)
  LNLENGTH = IIF(LCLINKTYPE='02' .AND. INLIST(LNSALESPART,1,2),3,6)

  SELECT LINKTYPE,SUBSTR(LINK_CODE,LNPOS,LNLENGTH) AS LINKCODE FROM GL_LINK ;
    GROUP BY LINKTYPE,LINKCODE ;
    HAVING (LINKTYPE = LCLINKTYPE);
    INTO CURSOR LCCODESCURS
  SET RELATION TO LCLINKTYPE+LINKCODE INTO GL_LINK

  DECLARE LAVALUES[2]     && array to get values from browse

  DECLARE LALINKTYPE[5]
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  laLinkType[1] = 'Customer           '
  *!*	  laLinkType[2] = 'Sales              '
  *!*	  laLinkType[3] = 'Style Invertory    '
  *!*	  laLinkType[4] = 'Material Inverntory'
  *!*	  laLinkType[5] = 'Work in Process    '
  *!*	  *E300592,1 (End)

  *!*	  lcFile_Ttl    = 'GL Link Codes' + IIF(EMPTY(lcLinkType) OR lcLinkType='00' ;
  *!*	    ,'',' for '+ALLTRIM(laLinkType[VAL(lcLinkType)]))

  *!*	  lcBrFields    = "LinkCode :H='Code',"        +;
  *!*	    "GL_LINK.LinkDesc  :H='Description'"  +;
  *!*	    IIF(EMPTY(lcLinkType),",lcDummy=laLinkType[VAL(LinkType)]:H='Type'",'')
  LOCAL LCARIAHFILE
  LCARIAHFILE = ""
  IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
    LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  ENDIF
  LALINKTYPE[1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_CUSTOMER,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_CUSTOMER",LCARIAHFILE))
  LALINKTYPE[2] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_SALES,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_SALES",LCARIAHFILE))
  LALINKTYPE[3] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_STYINV,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_STYINV",LCARIAHFILE))
  LALINKTYPE[4] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_MATINV,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_MATINV",LCARIAHFILE))
  LALINKTYPE[5] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_WIP",LCARIAHFILE))
  LCFILE_TTL    = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_TITLE ,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_TITLE",LCARIAHFILE))+;
    IIF(EMPTY(LCLINKTYPE) OR LCLINKTYPE='00' ;
    ,'',' '+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_FOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_FOR",LCARIAHFILE))+' '+ALLTRIM(LALINKTYPE[VAL(lcLinkType)]))
  LCBRFIELDS    = "LinkCode :H='"+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_CODE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_CODE",LCARIAHFILE))+"',"        +;
    "GL_LINK.LinkDesc  :H='"+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_DESC",LCARIAHFILE))+"'"  +;
    IIF(EMPTY(LCLINKTYPE),",lcDummy=laLinkType[VAL(LinkType)]:H='"+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLBROW_TYPE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GLBROW_TYPE",LCARIAHFILE))+"'",'')
  *N000682,1 MMT 12/09/2012 Globalization changes[END]

  =ARIABROW('',LCFILE_TTL,GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,'','','LinkCode,GL_LINK.LinkDesc','laValues')

  IF !EMPTY(LAVALUES[1])
    LCLINKCODE = LAVALUES[1]
    LCLINKDESC = IIF(EMPTY(LAVALUES[2]),SPACE(30),LAVALUES[2])
    LCLINKTYPE = LINKTYPE
  ELSE
    LCLINKCODE = SPACE(3)
    LCLINKDESC = SPACE(30)
  ENDIF

  USE IN LCCODESCURS

ELSE
  LCLINKDESC = GL_LINK.LINKDESC
ENDIF

IF LLOPNGLLNK
  USE IN GL_LINK
ELSE
  SET ORDER TO TAG LNOLDTAG IN GL_LINK
ENDIF

SELECT (LNOLDALIAS)
LLBROWSE = .F.
RETURN !EMPTY(LCLINKCODE)  && end of gfGLBrowse.

*!**************************************************************************
*! PROG: ORDBROWA.PRG
*! program to browse The Order header file sorting by account by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*!
*!**************************************************************************
PROCEDURE ORDBROWA

PARAMETERS XACCOUNT , LLRETALIAS , LCORDRTYPE
LCORDRTYPE = IIF(TYPE('lcOrdrType') <> 'C' .OR. EMPTY(LCORDRTYPE) , 'O' ,;
  IIF(UPPER(LCORDRTYPE) = 'A' , '' , UPPER(LCORDRTYPE)))
*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ORDBROW.H

IF TYPE('XORDER')='U'
  STORE '' TO XORDER
ENDIF

IF TYPE('XPOSEARCH')='U'
  XPOSEARCH = .F.
ENDIF

IF TYPE('XSTORE') = 'U'
  XSTORE = ''
ENDIF

IF XPOSEARCH
  DECLARE LAVALUES[1]  && array to get values from browse
  STORE '' TO LAVALUES
  LNOLDWORK = SELECT()
  SELECT ORDHDR

  LNOLDORDER = SYS(21)
  SET ORDER TO TAG ORDCUST

  =SEEK(XACCOUNT+UPPER(XCUSTPO)+IIF(EMPTY(XSTORE),'',XSTORE))
  =SEEK(XACCOUNT+UPPER(XCUSTPO))

  IF FOUND()
    DO CASE
    CASE !EMPTY(XSTORE) .AND. !EMPTY(LCORDRTYPE)
      LOCATE REST;
        WHILE ACCOUNT + UPPER(CUSTPO) = XACCOUNT+UPPER(XCUSTPO);
        FOR STORE = XSTORE .AND. CORDTYPE = LCORDRTYPE

    CASE !EMPTY(XSTORE)
      LOCATE REST;
        WHILE ACCOUNT + UPPER(CUSTPO) = XACCOUNT+UPPER(XCUSTPO);
        FOR STORE = XSTORE

    CASE !EMPTY(LCORDRTYPE)
      LOCATE REST;
        WHILE ACCOUNT + UPPER(CUSTPO) = XACCOUNT+UPPER(XCUSTPO);
        FOR CORDTYPE = LCORDRTYPE

    ENDCASE
  ENDIF
  SET ORDER TO &LNOLDORDER
  LCBRFIELDS = [Order:H="]+LANG_LABELORDER+[",status:H="]+LANG_LABELSTATUS+[",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+LANG_LABELSEASON+[",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+LANG_LABELDIVISION+[",]+;
    [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="]+LANG_LABELCUSTPO+[",]+;
    [ACCOUNT:H="]+LANG_LABELACCOUNT+[",store=IIF(MULTI='Y','*Multi*',STORE):H="]+LANG_LABELSTORE+[",Customer.stname]+;
    [:H="]+LANG_LABELNAME+[",Open:H="]+LANG_LABELOPEN+[",OpenAmt:H="]+LANG_LABELOPENAMT+[",Ship:H="]+LANG_LABELSHIP+[",Shipamt:H="]+LANG_LABELSHIPAMT+[",]+;
    [start:H="]+LANG_LABELSTART+[",Complete:H="]+LANG_LABELCOMPLETE+[",Note1:H="]+LANG_LABELNOTE+["]
  IF FOUND()
    IF ARIABROW('FOR Account + UPPER(CustPO) + cOrdType + Order = xAccount + UPPER(xCustPO)' +;
        IIF(EMPTY(LCORDRTYPE) , "" , " .AND. cOrdType + Order = lcOrdrType") ,;
        IIF(EMPTY(XSTORE),""," .AND. Store = xStore") +;
        "Orders",GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','','ORDER','laValues')
      XORDER = LAVALUES[1]
    ELSE
      STORE SPACE(6)  TO XORDER
    ENDIF
  ELSE
    =GFMODALGEN("TRM32118B00000","ALERT")
    *=gfDialog('I','No orders matching the selected criteria.')
    STORE SPACE(6) TO XORDER
  ENDIF

  IF LLRETALIAS
    SELECT (LNOLDWORK)
  ENDIF

  RETURN XORDER
ELSE
  RETURN ORDBROW(@XACCOUNT , 'A' , @XORDER , LLRETALIAS ,;
    IIF(EMPTY(LCORDRTYPE) , 'A' , LCORDRTYPE))
ENDIF

*!**************************************************************************
*! PROG: ORDBROWO.PRG
*! program to browse The Order header file sorting by order  by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*!
*!**************************************************************************
PROCEDURE ORDBROWO
PARAMETERS XORDER , LLRETALIAS , LCORDRTYPE

IF TYPE('XACCOUNT')='U'
  STORE '' TO XACCOUNT
ENDIF
RETURN ORDBROW(@XACCOUNT , 'O' , @XORDER , LLRETALIAS , LCORDRTYPE)

*!**************************************************************************
*! PROG: ORDBROW.PRG
*! program to browse The Order header file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE order header FILE HAS BEEN OPENED
*!  called by CUSBROWA.PRG & CUSBROWO.PRG
*!**************************************************************************
PROCEDURE ORDBROW
PARAMETERS XACCOUNT , LCKEY , XORDER , LLRETALIAS , LCORDRTYPE    && returns the account code of the customer selected

LCORDRTYPE = IIF(TYPE('lcOrdrType') <> 'C' .OR. EMPTY(LCORDRTYPE) , 'O' ,;
  IIF(UPPER(LCORDRTYPE) = 'A' , '' , UPPER(LCORDRTYPE)))
PRIVATE LCBRFIELDS,LNCURALIAS,LADATA

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ORDBROW.H
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE,LCORDHFILE
LCARIAHFILE = ''
LCORDHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  LCORDHFILE = OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ORDBROW")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

DECLARE LADATA[2]  && array to get values from browse
DECLARE LAORDSTATE[4,2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOrdState[1,1] = LANG_LabelStatusOpen
LAORDSTATE[1,1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTATUSOPEN,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStatusOpen",LCORDHFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LAORDSTATE[1,2] = 'O'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOrdState[2,1] = LANG_LabelStatusHold
LAORDSTATE[2,1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTATUSHOLD,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStatusHold",LCORDHFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LAORDSTATE[2,2] = 'H'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOrdState[3,1] = LANG_LabelStatusCancel
LAORDSTATE[3,1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTATUSCANCEL,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStatusCancel",LCORDHFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LAORDSTATE[3,2] = 'X'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOrdState[4,1] = LANG_LabelStatusComplete
LAORDSTATE[4,1] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTATUSCOMPLETE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelStatusComplete",LCORDHFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LAORDSTATE[4,2] = 'C'
STORE '' TO LADATA
LLWASSEL=.T.
LNCURALIAS = SELECT()
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE) && variable to determine forcing browse or not
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = [Order:H="]+LANG_LabelOrder+[",status:H="]+LANG_Labelstatus+[",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+LANG_LabelSeason+[",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+LANG_LabelDivision+[",]+;
*!*	  [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="]+LANG_LabelCustPo+[",]+;
*!*	  [ACCOUNT:H="]+LANG_LabelACCOUNT+[",store=IIF(MULTI='Y','*Multi*',STORE):H="]+LANG_Labelstore+[",Customer.stname]+;
*!*	  [:H="]+LANG_Labelname+[",Open:H="]+LANG_LabelOpen+[",OpenAmt:H="]+LANG_LabelOpenAmt+[",Ship:H="]+LANG_LabelShip+[",Shipamt:H="]+LANG_LabelShipamt+[",]+;
*!*	  [start:H="]+LANG_Labelstart+[",Complete:H="]+LANG_LabelComplete+[",]+;
*!*	  [Note1:H="]+LANG_LabelNote+["]
LCBRFIELDS = [Order:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelOrder",LCORDHFILE))+;
  [",status:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_Labelstatus",LCORDHFILE))+;
  [",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSEASON,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelSeason",LCORDHFILE))+;
  [",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELDIVISION,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelDivision",LCORDHFILE))+[",]+;
  [CustPo=IIF(multipo,']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ORDERBROW_MULTIPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ORDERBROW_MULTIPO",LCORDHFILE))+[',custpo):H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELCUSTPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelCustPo",LCORDHFILE))+[",]+;
  [ACCOUNT:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelACCOUNT",LCORDHFILE))+;
  [",store=IIF(MULTI='Y',']+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ORDERBROW_MULTISTORE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ORDERBROW_MULTISTORE",LCORDHFILE))+;
  [',STORE):H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTORE,OARIAAPPLICATION.GETHEADERTEXT("LANG_Labelstore",LCORDHFILE))+;
  [",Customer.stname]+[:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_Labelname",LCORDHFILE))+;
  [",Open:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELOPEN,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelOpen",LCORDHFILE))+;
  [",OpenAmt:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELOPENAMT,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelOpenAmt",LCORDHFILE))+;
  [",Ship:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSHIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelShip",LCORDHFILE))+;
  [",Shipamt:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSHIPAMT,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelShipamt",LCORDHFILE))+[",]+;
  [start:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELSTART,OARIAAPPLICATION.GETHEADERTEXT("LANG_Labelstart",LCORDHFILE))+;
  [",Complete:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELCOMPLETE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelComplete",LCORDHFILE))+[",]+;
  [Note1:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELNOTE,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelNote",LCORDHFILE))+["]
*N000682,1 11/20/2012 MMT Globlization changes[End]

IF !USED('Customer')
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'Customer',OARIAAPPLICATION.DATADIR+'Customer','SH')
ENDIF

SELECT ORDHDR
SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER

*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]
LCOLDINDEX =  ''
IF TYPE('xSTORE') <> 'U' AND !EMPTY(XSTORE)
  LCOLDINDEX =  ORDER('ORDLINE')
  SET ORDER TO ORDLINST IN 'ORDLINE'   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
  SET RELATION TO LCORDRTYPE + ORDHDR.ORDER + XSTORE INTO ORDLINE ADDITIVE
ENDIF
*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[End]

LCORDORDER = TAG()
IF LCKEY='A'
  SET ORDER TO TAG ORDACCT
ELSE
  SET ORDER TO TAG ORDHDR
ENDIF
IF LCKEY='A'  && called from cus. prog.
  IF !SEEK(XACCOUNT)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalgen("INM40082B00000","ALERT",LANG_Labelorders+'|'+LANG_LabelAccount+' '+xAccount)
    =GFMODALGEN("INM40082B00000","ALERT",IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELORDERS,OARIAAPPLICATION.GETHEADERTEXT("LANG_Labelorders",LCORDHFILE))+;
      '|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelAccount",LCORDHFILE))+' '+XACCOUNT)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *    =gfDialog("I","No orders have been found for account# "+xAccount+".")
    IF !EMPTY(LCORDORDER)
      SET ORDER TO TAG (LCORDORDER)
    ELSE
      SET ORDER TO
    ENDIF
    SET RELATION TO
    IF LLRETALIAS
      SELECT (LNCURALIAS)
    ENDIF
    RETURN .F.
  ENDIF
ENDIF
IF LLBROWSE OR !SEEK(XACCOUNT)
  LNSOFTSEEK=RECNO(0)
  IF LNSOFTSEEK<>0 .AND. LNSOFTSEEK <= RECCOUNT("ORDHDR")
    GO LNSOFTSEEK
  ELSE
    GO TOP
  ENDIF
  LCPUSHB=IIF(LCKEY="A",'',"Fi\<nd;;\<Descending;\<Filter;\!\<Select;\?\<Cancel")
  IF LCKEY='A'
    *! B040211,1 ASM 05/10/2006 Bug in the Global Browse  [Start]
    *llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = IIF(EMPTY(xSTORE),"",xStore)',;
    "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")

    *B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]
    *llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = "'+IIF(EMPTY(xSTORE),'"',xStore+'"'),;
    "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *!*	    llWasSel=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = "'+IIF(EMPTY(xSTORE),'"',xStore+'" OR !EOF("ORDLINE")'),;
    *!*	      "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'ORDER,Account',"laData")
    LLWASSEL=ARIABROW([XACCOUNT + lcOrdrType]+' FOR STORE = "'+IIF(EMPTY(XSTORE),'"',XSTORE+'" OR !EOF("ORDLINE")'),;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELORDERS,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelOrders",LCORDHFILE)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"",LCPUSHB,'ORDER,Account',"laData")
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    *B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[END]

    *! B040211,1 ASM 05/10/2006 Bug in the Global Browse  [End]
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *llWasSel=ARIABROW('lcOrdrType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",lcPushB,'Order,ACCOUNT',"laData")
    LLWASSEL=ARIABROW('lcOrdrType',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LABELORDERS,OARIAAPPLICATION.GETHEADERTEXT("LANG_LabelOrders",LCORDHFILE)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"",LCPUSHB,'Order,ACCOUNT',"laData")
    *N000682,1 11/20/2012 MMT Globlization changes[end]
  ENDIF
  IF LLWASSEL
    XORDER    = LADATA[1]
    XACCOUNT  = LADATA[2]
  ELSE
    XORDER = SPACE(6)
    XACCOUNT = SPACE(5)
  ENDIF
ENDIF

*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[Start]
IF TYPE('xSTORE') <> 'U' AND !EMPTY(XSTORE) AND !EMPTY(LCOLDINDEX)
  SET ORDER TO (LCOLDINDEX) IN 'ORDLINE'
ENDIF
*B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[End]

IF !EMPTY(LCORDORDER)
  SET ORDER TO TAG (LCORDORDER)
ELSE
  SET ORDER TO
ENDIF
SET RELATION TO
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
RETURN LLWASSEL

*!*************************************************************************
*! Name      : gfCrdtBrow
*! Developer : Reham Alallamy
*! Date      : 30/03/97
*! Purpose   : Func. to browse all the available credit memos.
*!*************************************************************************
*! Calls     :
*!*************************************************************************
*! Returns   :
*!*************************************************************************
*
FUNCTION GFCRDTBROW
PARAMETER LCCRMEMO,LLRETALIAS,LCACCOUNT,LCSTORE
PRIVATE LCBRFIELDS,LNCURALIAS,LADATA
PRIVATE LCCRMORDR

LCACCOUNT = IIF(TYPE('lcAccount')='C',LCACCOUNT,'')
LCSTORE   = IIF(TYPE('lcStore')='C',LCSTORE,'')
DECLARE LADATA[1]  && array to get values from browse
STORE '' TO LADATA
LLWASSEL = .T.
LLBROWSE = IIF(TYPE('llBrowse') = 'U' , .T. , LLBROWSE) && variable to determine forcing browse or not
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = [CrMemo,Account:H="Acct#",Store,CrDate,RaNo,]+;
*!*	  [Status:H="S",Reference:H="Ref.",Pieces,TotCredit:H="Amount",]+;
*!*	  [cWareCode:H="WareHouse",Invoice,Order,Reason,]+;
*!*	  [cDivision,Salesrep1,Salesrep2]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCBRFIELDS = [CrMemo,Account:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ACCNO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ACCNO",LCARIAHFILE))+;
  [",Store,CrDate,RaNo,]+;
  [Status:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STATUS_CR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_STATUS_CR",LCARIAHFILE))+;
  [",Reference:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_REFERENCE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_REFERENCE",LCARIAHFILE))+;
  [",Pieces,TotCredit:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AMOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_AMOUNT",LCARIAHFILE))+[",]+;
  [cWareCode:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_WAREHOUSE_CR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_WAREHOUSE_CR",LCARIAHFILE))+[",Invoice,Order,Reason,]+;
  [cDivision,Salesrep1,Salesrep2]
*N000682,1 11/20/2012 MMT Globlization changes[END]
LNCURALIAS = SELECT()
LCCRMORDER = ''
LLOPENCRM = .F.
IF !USED("RETHDR")
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *SELECT 0
  *USE (oAriaApplication.DataDir+'RETHDR') IN 0 ORDER TAG RETHDRA
  =GFOPENTABLE(OARIAAPPLICATION.DATADIR+'RETHDR',OARIAAPPLICATION.DATADIR+'RETHDRA','SH')
  *B608139,1 WAM (End)
  LLOPENCRM = .T.
ELSE
  SELECT RETHDR
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *lcCrMOrder = TAG()
  *SET ORDER TO TAG RETHDRA
  LCCRMORDER = ORDER()
  GFSETORDER("RETHDRA")
  *B608139,1 WAM (End)
ENDIF
*B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
*=SEEK(lcAccount)
SELECT RETHDR
=GFSEEK(LCACCOUNT)
*B608139,1 WAM (End)
LOCATE REST WHILE ACCOUNT + CRMEMO = LCACCOUNT FOR STORE = IIF(EMPTY(LCSTORE) , ALLTRIM(LCSTORE) , PADR(LCSTORE,8))

IF !FOUND()
  *E300455,1 Message : 40058
  *E300455,1 No Credit Memos found
  *E300455,1 Button : 00000
  *E300455,1 Ok
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *!*	  =gfModalGen('TRM40058B00000','ALERT',IIF(EMPTY(lcAccount),'',;
  *!*	    'for account: '+lcAccount)+IIF(EMPTY(lcStore),'',' store: '+lcStore))
  =GFMODALGEN('TRM40058B00000','ALERT',IIF(EMPTY(LCACCOUNT),'',;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_FORACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_FORACCOUNT",LCARIAHFILE)) +LCACCOUNT)+;
    IIF(EMPTY(LCSTORE),'',;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_S_STORE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_S_STORE",LCARIAHFILE))+LCSTORE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
  LCCRMEMO = SPACE(6)
ELSE
  IF LLBROWSE .OR. !SEEK(LCACCOUNT+LCCRMEMO)
    LNSOFTSEEK = RECNO(0)
    IF LNSOFTSEEK > 0 .AND. LNSOFTSEEK <= RECCOUNT("RETHDR")
      GO LNSOFTSEEK
    ELSE
      GO TOP
    ENDIF
    SELECT RETHDR
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *lcCrMOrdr = TAG()
    *SET ORDER TO TAG RETHDR
    LCCRMORDR = ORDER()
    GFSETORDER("RETHDR")
    *B608139,1 WAM (End)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *llWasSel = ARIABROW("FOR ACCOUNT=lcAccount AND STORE=IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))","Credit Memos",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','CrMemo',"laData")
    LLWASSEL = ARIABROW("FOR ACCOUNT=lcAccount AND STORE=IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))",IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CRMEMO_TITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CRMEMO_TITLE",LCARIAHFILE)),GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"",'','CrMemo',"laData")
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO TAG (lcCrMOrdr) IN RETHDR
    GFSETORDER(LCCRMORDR)
    *B608139,1 WAM (End)
    LCCRMEMO = IIF(LLWASSEL , LADATA[1] , SPACE(6))
  ENDIF
ENDIF
IF LLOPENCRM
  *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
  *USE IN RETHDR
  GFCLOSETABLE('RETHDR')
  *B608139,1 WAM (End)
ELSE
  SELECT RETHDR
  IF !EMPTY(LCCRMORDER)
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO TAG (lcCrMOrder)
    GFSETORDER(LCCRMORDER)
    *B608139,1 WAM (End)
  ELSE
    *B608139,1 WAM Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
    *SET ORDER TO
    GFSETORDER()
    *B608139,1 WAM (End)
  ENDIF
ENDIF
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
RETURN (LCCRMEMO)

*****************************************************************************
* PROG: RABROW
* DESC: UDF() PROGRAM TO BROWSE THE R/A FOR A SPECIFIC ACCOUNT
* using the new browse
*****************************************************************************
PROCEDURE RABROW
PARAMETER XRANO,LLRETALIAS
PRIVATE LCBRFIELDS,LNCURALIAS,LADATA

DECLARE LADATA[2]  && array to get values from browse
STORE '' TO LADATA
LLWASSEL=.T.
LLBROWSE = IIF(TYPE('llBrowse')='U',.F.,LLBROWSE) && variable to determine forcing browse or not
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcBrFields = [RANO:H="R/A #",RADATE:H="Issued",VOID:]+;
*!*	  [H="Void",AUTH:H="Pieces",AUTHAMT:H="Amount",]+;
*!*	  [invoice:H="Invoice",order:H="Order",]+;
*!*	  [custpo:H="Cust P/O",cartons:H="Cartons"]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCBRFIELDS = [RANO:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_RANO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_RANO",LCARIAHFILE))+;
  [",RADATE:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_ISSUED,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_ISSUED",LCARIAHFILE))+[",VOID:]+;
  [H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_VOID,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_VOID",LCARIAHFILE))+[",AUTH:H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_PIECES,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_PIECES",LCARIAHFILE))+[",AUTHAMT:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AMOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_AMOUNT",LCARIAHFILE))+[",]+;
  [invoice:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_INVOICE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_INVOICE",LCARIAHFILE))+;
  [",order:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_ORDER",LCARIAHFILE))+[",]+;
  [custpo:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_CUSTPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_CUSTPO",LCARIAHFILE))+;
  [",cartons:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_CARTONS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_CARTONS",LCARIAHFILE))+["]
*N000682,1 MMT 12/03/2012 Globalization changes[END]
LNCURALIAS = SELECT()
IF TYPE('XACCOUNT')='U'
  STORE '' TO XACCOUNT
ENDIF

IF !USED('RETAUTH')

  *WSH [Start]
  *=gfOpenFile(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
  =GFOPENTABLE(OARIAAPPLICATION.DATADIR+'RETAUTH',OARIAAPPLICATION.DATADIR+'RETAUTH','SH')
  *WSH [End]

ENDIF
SELECT RETAUTH

*WSH [Start]
*lcRatOrder = TAG()
*SET ORDER TO TAG RETAUTHA
LCRATORDER = ORDER()
GFSETORDER("RETAUTHA")
*WSH [End]

IF SEEK(XACCOUNT + XRANO)
  IF STATUS <> 'O'
    *N000682,1 MMT 11/20/2012 Globalization project[START]
    *=gfDialog('I', 'This R/A is already complete!')
    =GFDIALOG('I', IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_COMPLETE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_COMPLETE",LCARIAHFILE)))
    *N000682,1 MMT 11/20/2012 Globalization project[End]
    XRANO = SPACE(6)
  ELSE
    XRANO = RANO
  ENDIF
ELSE

  *WSH [Start]
  *lnSoftSeek=RECNO(0)
  *IF SEEK(xAccount)
  IF GFSEEK(XACCOUNT)
    *WSH [End]

    IF TYPE('XSTORE') = "U"
      XSTORE=SPACE(8)
    ENDIF
    LOCATE REST FOR STATUS='O' .AND. IIF(EMPTY(XSTORE),.T.,STORE=XSTORE) ;
      WHILE ACCOUNT=XACCOUNT
    IF FOUND()

      *WSH [Start]
      *IF lnSoftSeek<>0
      *  GO lnSoftSeek
      *ELSE
      *  GO TOP
      *ENDIF
      *llWasSel=ARIABROW([xaccount FOR STATUS='O' .AND.;
      IIF(EMPTY(XSTORE),.T.,STORE=XSTORE)],;
      "Return Authorizations",;
      gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','RANO',"laData")
      *XRANO = IIF(llWasSel,laData[1],SPACE(6))
      *N000682,1 MMT 11/20/2012 Globalization project[START]
      *!*	      llWasSel=ARIABROW([xaccount FOR STATUS='O' .AND.;
      *!*	               IIF(EMPTY(XSTORE),.T.,STORE=XSTORE)],;
      *!*	        "Return Authorizations",;
      *!*	        gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','RANO',"laData",.T.,"RETAUTH")
      LLWASSEL=ARIABROW([xaccount FOR STATUS='O' .AND.;
               IIF(EMPTY(XSTORE),.T.,STORE=XSTORE)],;
        IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_TITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_TITLE",LCARIAHFILE)),;
        GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"",'','RANO',"laData",.T.,"RETAUTH")
      *N000682,1 MMT 11/20/2012 Globalization project[END]
      XRANO = IIF(LLWASSEL,LADATA[1],SPACE(6))
      *WSH [End]

    ELSE
      *N000682,1 MMT 11/20/2012 Globalization project[START]
      *=gfModalgen("INM40082B00000","ALERT","Open R/As"+'|'+"Account"+' '+xAccount)
      =GFMODALGEN("INM40082B00000","ALERT",;
        IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_OPENRAS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_OPENRAS",LCARIAHFILE))+;
        '|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_ACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_ACCOUNT",LCARIAHFILE))+' '+XACCOUNT)
      *N000682,1 MMT 11/20/2012 Globalization project[END]
      *=gfDialog("I",'There are no open R/As for this account')
      XRANO = SPACE(6)
    ENDIF
  ELSE
    *N000682,1 MMT 11/20/2012 Globalization project[START]
    *=gfModalgen("INM40082B00000","ALERT","R/As"+'|'+"Account"+' '+xAccount)
    =GFMODALGEN("INM40082B00000","ALERT",IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_RAS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_RAS",LCARIAHFILE))+;
      '|'+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_RETAUTH_ACCOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_RETAUTH_ACCOUNT",LCARIAHFILE))+' '+XACCOUNT)
    *N000682,1 MMT 11/20/2012 Globalization project[END]
    *=gfDialog('I', 'There are no R/As for this account ! ')
    XRANO = SPACE(6)
  ENDIF
ENDIF

IF !EMPTY(LCRATORDER)

  *WSH [Start]
  *SET ORDER TO TAG (lcRatOrder)
  GFSETORDER(LCRATORDER)
  *WSH [End]

ELSE

  *WSH [Start]
  *SET ORDER TO
  GFSETORDER()
  *WSH [Start]

ENDIF
IF LLRETALIAS
  SELECT(LNCURALIAS)
ENDIF
RETURN(XRANO)

*!*************************************************************
*! Name      : gfCurrBrow
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/27/1995
*! Purpose   : Global currency code fields validation
*!*************************************************************
*! Calls     : gfBrow()
*!*************************************************************
*! Passed Parameters  : lcCurrCode : pointer to currency code field
*!                      lcCurrDesc : pointer to currency description
*!                                   variable (optional)
*!*************************************************************
*! Returns            :  .T. If a valid currency code is selected,
*!                       .F. otherwise
*!*************************************************************
*! Example            :  =gfCurrBrow(@lcCurrCode, @lcCurrDesc)
*!*************************************************************
FUNCTION GFCURRBROW
PARAMETERS LCCURRCODE, LCCURRDESC
LOCAL LLWASSEL, LCALIAS
LCCURRCODE = PADR(ALLTRIM(LCCURRCODE),5)
DECLARE LATMPVAL[2]
LATMPVAL[1] = 'cCurrCode'
LATMPVAL[2] = 'cCurrDesc'
*B00000,1 ASM Apply Aria Grid in the Currency Selection [Start]
*!*  IF gfBrow('SYCCURR', 'CCURRCODE', lcCurrCode, @laTmpVal,;
*!*            [cCurrCode:8:H='Currency',cCurrDesc:30:H='Description',;
*!*             nCurrUnit:5:H='Units',cCurrSmbl:7:H='Symbol'],;
*!*             'Currencies', 56)
*!*    lcCurrCode = laTmpVal[1]
*!*    lcCurrDesc = laTmpVal[2]
*!*  ELSE
*!*    lcCurrCode = SPACE(5)
*!*    lcCurrDesc = SPACE(30)
*!*  ENDIF
LCALIAS = ALIAS()
SELECT SYCCURR
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = "cCurrCode:8:H='Currency',"+;
*!*	  "cCurrDesc:30:H='Description',"+;
*!*	  "nCurrUnit:5:H='Units',"+;
*!*	  "cCurrSmbl:7:H='Symbol'"
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCBRFIELDS = "cCurrCode:8:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURRENCY,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURRENCY",LCARIAHFILE))+"',"+;
  "cCurrDesc:30:H='"+ IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURDESC",LCARIAHFILE))+"',"+;
  "nCurrUnit:5:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURUNITS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURUNITS",LCARIAHFILE)) +"',"+;
  "cCurrSmbl:7:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURSYMBOL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURSYMBOL",LCARIAHFILE)) +"'"
*N000682,1 11/20/2012 MMT Globlization changes[END]

*B999999,1 AMH Fix bug of no records to display [Start]
*llWasSel= ARIABROW(IIF(EMPTY(lcCurrCode),.F.,[lcCurrCode]),'Currencies',gnBrFSRow1, ;
gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cCurrCode,cCurrDesc","laTmpVal")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llWasSel= ARIABROW(.F.,'Currencies',gnBrFSRow1,gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cCurrCode,cCurrDesc","laTmpVal")
LLWASSEL= ARIABROW(.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURRS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURRS",LCARIAHFILE)) ,GNBRFSROW1,GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"","","cCurrCode,cCurrDesc","laTmpVal")
*N000682,1 11/20/2012 MMT Globlization changes[END]
*B999999,1 AMH [End]

IF LLWASSEL
  LCCURRCODE = LATMPVAL[1]
  LCCURRDESC = LATMPVAL[2]
ELSE
  LCCURRCODE = SPACE(5)
  LCCURRDESC = SPACE(30)
ENDIF
IF !EMPTY(LCALIAS)
  SELECT (LCALIAS)
ENDIF

RETURN LLWASSEL
*B00000,1 [End]


*!*************************************************************
*! Name      : gfBrow
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/27/1995
*! Purpose   : No button global browse
*!*************************************************************
*! Calls     : lcCurrCode
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  IF gfBrow('SYCCURR', 'CCURRCODE',;
*!                                  lcCurrCode, @laTmpVal,;
*!                                  lcBrFlds, lcBrTtl, 15, 54)
*!*************************************************************
*: Modifications :
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*:************************************************************************
FUNCTION GFBROW
PARAMETERS LCSEEKFILE, LCSEEKTAG, LCSEEKEXP, LARETVAL, LCBRFIELDS, LCBRTITLE,LNBRCOLS
PRIVATE LCCURFLT, LCBRWINNAME, LNCURALIAS, LNCURTAG, LNCURREC, LLRETVAL,;
  LLENTERED, LCEXPTOSEEK, LAKEYEXP

LNCURALIAS = SELECT()
SELECT (LCSEEKFILE)
LCCURFLT = FILTER()
LNCURTAG = VAL(SYS(21))
IF DELETED()
  SKIP
ENDIF
LNCURREC = IIF(EOF() .OR. BOF(), 0, RECNO())

SET FILTER TO
SET ORDER TO TAG (LCSEEKTAG)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
LLBROWSE =  IIF(TYPE('llBrowse') = 'U', .F., LLBROWSE) .OR. '?' $ LCSEEKEXP
GO TOP
LLENTERED = .F.
IF EOF()
  *N000682,1 MMT 12/03/2012 Globalization changes[Start]
  *  =gfDialog('I', 'There are no records to browse.')
  =GFDIALOG('I', IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_BROWNOREC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_BROWNOREC",LCARIAHFILE)))
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
ELSE
  IF !LLBROWSE
    IF !SEEK(LCSEEKEXP)
      IF BETWEEN(RECNO(0), 1, RECCOUNT())
        GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      LLBROWSE = .T.
    ELSE
      LLENTERED = .T.
    ENDIF
  ENDIF
  IF LLBROWSE
    PUSH KEY
    =GFCLEARKEY()
    DIMENSION LAKEYEXP[1]
    STORE 0 TO LNX,LNY,LNCURR,LNTIMELIMT,LNSEEKLIMT,LNSELREC
    STORE .F. TO LLCLICK
    STORE [""] TO LAKEYEXP

    LCBRTITLE   = IIF(EMPTY(LCBRTITLE), PROPER(ALIAS()), LCBRTITLE)
    LCBRWINNAME = GFTEMPNAME()

    *! E037885,2 MAH 12/03/2004 Deine in top level window [BEGIN]
    *-- DEFINE WINDOW (lcBrWinName);
    *--        AT 0,0 SIZE 2*SROW()/3, lnBrCols;
    *--        FONT "Tahoma", 9 ;
    *--        FLOAT ;
    *--        NOCLOSE ;
    *--        SHADOW ;
    *--        NOMINIMIZE ;
    *--        SYSTEM ;
    *--        COLOR SCHEME 10
    LOCAL LOHOSTFORM
    LOHOSTFORM = GFGETTOPLEVELFROM()

    IF TYPE('loHostForm.Name') # 'C' .OR. UPPER(LOHOSTFORM.NAME) == 'SCREEN'
      DEFINE WINDOW (LCBRWINNAME);
        AT 0,0 SIZE 2*SROW()/3, LNBRCOLS ;
        FONT "Tahoma", 9 ;
        FLOAT ;
        NOCLOSE ;
        SHADOW ;
        NOMINIMIZE ;
        SYSTEM ;
        COLOR SCHEME 10
    ELSE
      DEFINE WINDOW (LCBRWINNAME);
        AT 0,0 SIZE 2*SROW()/3, LNBRCOLS ;
        IN (LOHOSTFORM.NAME) ;
        FONT "Tahoma", 9 ;
        FLOAT ;
        NOCLOSE ;
        SHADOW ;
        NOMINIMIZE ;
        SYSTEM ;
        COLOR SCHEME 10
    ENDIF
    *! E037885,2 MAH 12/03/2004 [END]

    MOVE WINDOW (LCBRWINNAME) CENTER

    ON KEY LABEL ENTER DO LPSELONENT  WITH LCBRTITLE, LLENTERED
    ON KEY LABEL LEFTMOUSE  DO LFCDCHKDCL WITH LCBRTITLE, LLENTERED
    LCEXPTOSEEK=""
    LCORDEXPR   = SYS(14, EVAL(SYS(21)))
    IF TYPE(LCORDEXPR) = 'N'
      LNSTARTTRAP = 48
      LNENDTRAP   = 57
    ELSE
      LNSTARTTRAP = 32
      LNENDTRAP   = 126
    ENDIF
    FOR LNCHRTOTRAP = LNSTARTTRAP TO LNENDTRAP
      ON KEY LABEL (CHR(LNCHRTOTRAP)) DO LFCDCHINCS
    ENDFOR

    *! E037885,2 MAH 12/03/2004 Deine in top level window [BEGIN]
    *-- BROWSE FIELDS &lcBrFields;
    *--        WINDOW (lcBrWinName);
    *--        LOCK 0;
    *--        NOMENU;
    *--        NOAPPEND;
    *--        NOEDIT;
    *--        NODELETE;
    *--        TITLE lcBrTitle
    IF TYPE('loHostForm.Name') # 'C' .OR. UPPER(LOHOSTFORM.NAME) == 'SCREEN'
      BROWSE FIELDS &LCBRFIELDS;
        WINDOW (LCBRWINNAME);
        LOCK 0;
        NOMENU;
        NOAPPEND;
        NOEDIT;
        NODELETE;
        TITLE LCBRTITLE
    ELSE
      BROWSE FIELDS &LCBRFIELDS;
        WINDOW (LCBRWINNAME);
        LOCK 0;
        NOMENU;
        NOAPPEND;
        NOEDIT;
        NODELETE;
        TITLE LCBRTITLE IN WINDOW (LOHOSTFORM.NAME)
    ENDIF
    *! E037885,2 MAH 12/03/2004 [END]
    POP KEY

    IF LLENTERED .AND. BETWEEN(LNSELREC, 1, RECCOUNT())
      GO LNSELREC
    ENDIF
    WAIT CLEAR
    RELEASE WINDOW (LCBRWINNAME)
  ENDIF
ENDIF

*E300329,1 If the browse is called from the last field before a menu
*E300329,1 created by MENU TO command, and a selection is done using
*E300329,1 a double mouse click, an extra mouse click needs to be
*E300329,1 consumed so that the cursor waits for a menu selection
=INKEY(0.001)

*E300329,1 If selected,
IF LLENTERED
  FOR LNCOUNT = 1 TO ALEN(LARETVAL)
    LARETVAL[lnCount] = EVALUATE(LCSEEKFILE + '.' + LARETVAL[lnCount])
  ENDFOR
ENDIF
LLBROWSE = .F.
*E300329,1 Restore environmet

SELECT (LCSEEKFILE)
SET ORDER TO TAG (LNCURTAG)
IF !EMPTY(LCCURFLT)
  SET FILTER TO
ENDIF
IF LNCURREC <> 0
  GO LNCURREC
ELSE
  GO TOP
ENDIF
SELECT (LNCURALIAS)
RETURN LLENTERED

*!*************************************************************
*! Name      : gfClearKey
*! Developer : RENEE - Renee Ezzat
*! Date      : 05/25/1995
*! Purpose   : Resets any keys then sets global key traps.
*!*************************************************************
*! Example            :  =gfClearKey()
*!*************************************************************
FUNCTION GFCLEARKEY
ON KEY
ON KEY LABEL F4 KEYBOARD '?'+'{ENTER}'
ON KEY LABEL F1 DO LPPRESSF1

*:************************************************************************
*: Program file  : lpSelOnEnt
*: Program desc. : Selects a code upon presssing ENTER in CodeChk browse
*: For screen    :
*:         System:
*:         Module: Aria Apparel System
*:      Developer: Renee Ezzat
*:************************************************************************
*: Example :
*:        ON KEY LABEL ENTER DO lpSelOnEnt
*:*************************************************************
PROCEDURE LPSELONENT
PARAMETERS LCBRNAME, LLEXIT

ON KEY

*B600480,1 YI on 06/20/95 save the selected record number
IF TYPE('lnSelRec')='N'
  LNSELREC = RECNO()
ENDIF

DEACTIVATE WINDOW  (LCBRNAME)
LLEXIT = .T.


*!******************************************************************
*!
*!              Function: lfCdChkDcl
*!
*!******************************************************************
*
FUNCTION LFCDCHKDCL
PARAMETERS LCBRNAME,LLEXIT
LNX =INT(MROW())
LNY =INT(MCOL())
*B600322 if the user pressed double click with the mouse in any of the
*B600322 scroll bars "Horz.,Vert." ignore it and dont select the active
*B600322 record, we added the checking if the MROW AND THE MCOL inside
*B600322 the browse window with out taking the position of the scroll
*B600322 bars in out consideration

IF  (MROW(LCBRNAME)<>-1) AND (MCOL(LCBRNAME)<>-1) AND (MROW(LCBRNAME)>IIF(_DOS OR _UNIX,2,1.77));
    AND BETWEEN(MCOL(LCBRNAME),2,WCOL(LCBRNAME)-IIF(_DOS OR _UNIX,2,1.77));
    AND BETWEEN(MROW(LCBRNAME),2,WROWS(LCBRNAME)-IIF(_DOS OR _UNIX,2,1.77))

  *    AND MCOL(xTitle)<(WCOLS(xTitle)+WLCOL(xTitle)-IIF(_DOS OR _UNIX,2,3.77));
  AND MROW(xTitle)<(WROWS(xTitle)+WLROW(xTitle)-IIF(_DOS OR _UNIX,2,1.77))

  * if leftmouse wasn't pressed  previously
  IF !LLCLICK
    LNTIMELIMT = SECONDS()
    LNCURR     = LNX
    LLCLICK    = .T.
  ELSE
    * else if leftmouse was pressed  previously
    * if the second click of the mouse is in the time limit and clicking
    * on the same row
    IF SECONDS() < LNTIMELIMT + _DBLCLICK .AND. LNCURR  = INT(MROW());
        AND  LNX=LNCURR

      LNSELREC   = RECNO()
      LLCLICK    = .F.

      ON KEY
      DEACTIVATE WINDOW  (LCBRNAME)
      LLEXIT = .T.

      *KEYBOARD lcKeyLable CLEAR
    ENDIF
    IF LNCURR    = INT(MROW())
      LNTIMELIMT = SECONDS()
    ELSE
      LLCLICK    = .F.
    ENDIF
  ENDIF
ELSE
  LLCLICK    = .F.
ENDIF

*!**************************************************************************
*!
*!              Function: lfcdchIncS
*!
*!**************************************************************************
*
*!B600363,1 function added for inc. search.
FUNCTION LFCDCHINCS
PRIVATE LNBRRECNO
* if the last key pressed time = 0 get the new time
IF LNSEEKLIMT = 0
  LNSEEKLIMT = SECONDS()
ENDIF
* if time in the limit of the last key pressed time and the dblclick time
IF SECONDS() < LNSEEKLIMT + _DBLCLICK
  LNSEEKLIMT = SECONDS()
  LNBRRECNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  * loop through the key used for the browse
  FOR LNCOUNT = 1 TO ALEN(LAKEYEXP,1)
    * if seek of key+keypressed
    IF SEEK(&LAKEYEXP[lnCount]+LCEXPTOSEEK+UPPER(CHR(LASTKEY())))
      * add the keypressed to the exptoseek
      LCEXPTOSEEK = LCEXPTOSEEK+UPPER(CHR(LASTKEY()))
      WAIT LCEXPTOSEEK WINDOW NOWAIT
      RETURN
    ELSE
      *else if !seek of key+keypressed return to the current record
      IF LNBRRECNO>0
        GO LNBRRECNO
      ENDIF
    ENDIF
  ENDFOR
ELSE
  * else if time not in the limit of the last key pressed time and the dblclick time
  LNSEEKLIMT = 0
  LCEXPTOSEEK=''
  =LFCDCHINCS()
ENDIF


*!**********************************************************************
*! Name      : gfOTSDisp
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! Purpose   : Function to show the open Qty To Sell.
*!**********************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : 1--> lcStyle	   style code
*!                      2--> lcWareHouse   ware house code
*!                      3--> llAllWareHs ----(all ware house,warehouse)
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*N000408,1 Reham 07/2002 Convert to work under Aria 4 version
*B000000,1 HBG 01/30/2005 Add parameter to Get the OTS on Configuration level if use configuration
*!************************************************************************
FUNCTION GFOTSDISP

*B038253,1 AMH Add parameter for dyelot [Start]
*PARAMETER lcStyle,lcWareHouse,llAllWareHs
*B000000,1 HBG 01/30/2005 Add parameter to Get the OTS on Configuration level if use configuration [Begin]
*PARAMETER lcStyle,lcWareHouse,llAllWareHs,lcDyelot

*B132139,1 WSH 05/18/2006 Add extended Scale parameter to get Open To Sell for it. [Start]
PARAMETER LCSTYLE,LCWAREHOUSE,LLALLWAREHS,LCDYELOT,LLUSECONFIG
*PARAMETER lcStyle,lcWareHouse,llAllWareHs,lcDyelot,llUseConfig,lcEXScale
*B132139,1 WSH 05/18/2006 [End]
IF TYPE('llUseConfig') = 'U'
  LLUSECONFIG = .F.
ENDIF
*B000000,1 [End]
LCDYELOT = IIF(TYPE('lcDyelot')#'C' OR EMPTY(LCDYELOT),SPACE(10),LCDYELOT)
*B038253,1 AMH [End]

PRIVATE LCBRFIELDS,LNALIAS,LLMFINSTLD,LLSOINSTLD,LLPOINSTLD,;
  LCTMPOTS,LCENGLAND,LLOTSINFO,LCEXKEY,LCWCONDT,LCNMJRTL,LCMJRPCT,;
  LCNMJRPT,LNSTYLEWID,LNCOLORWID,LLALLCLRS

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\GFOTSDISP.H
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE,LCGFOTSDISP
LCARIAHFILE = ''
LCGFOTSDISP = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  LCGFOTSDISP = OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("gfOTSDisp")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

*-- Get the "O.T.S. period ranges" setup if weekly, 2 weeks or monthly
LCOTSPRD   = ""
LCOTSPRD   = GFGETMEMVAR('M_OTSPRIOD')

*-- Get the setting of OTS based on exact transaction date "J&L"
LLOTSBASTR = .F.
LLOTSBASTR = GFGETMEMVAR('M_OTSBASTR',GCACT_COMP)
LLOTSBASTR = IIF(TYPE("llOTSbasTr")$"UC" , .F. , LLOTSBASTR)

*-- Get the style code info.
LCMJRPCT   = GFITEMMASK('PM')  && Major Picture
LCNMJRPT   = GFITEMMASK('PN')  && Non-Major Picture
LCNMJRTL   = GFITEMMASK('HN')  && Non-Major Title
LNSTYLEWID = LEN(LCMJRPCT)     && Major Length
LNCOLORWID = LEN(LCNMJRPT)     && Non-Major Length
LLALLCLRS  = EMPTY(SUBSTR(LCSTYLE,LNSTYLEWID+2)) && If display all colors

STORE .F. TO LLMFINSTLD , LLSOINSTLD , LLPOINSTLD
*-- Flag hold if the MF module is installed or not
LLMFINSTLD = (OCCURS('MF',OARIAAPPLICATION.COMPANYINSTALLEDMODULES)<>0)
*-- Flag hold if the SO module is installed or not
LLSOINSTLD = (OCCURS('SO',OARIAAPPLICATION.COMPANYINSTALLEDMODULES)<>0)
*-- Flag hold if the PO module is installed or not
LLPOINSTLD = (OCCURS('PO',OARIAAPPLICATION.COMPANYINSTALLEDMODULES)<>0)

*-- Assign temp names to variables
LCTMPOTS   = GFTEMPNAME()  && Temp name hold the OTS qty. cursor.
LCPOHDRTMP = GFTEMPNAME()  && Temp name hold the PO header alias.
LCPOLINTMP = GFTEMPNAME()  && Temp name hold the PO line alias.
LCSHPHDTMP = GFTEMPNAME()  && Temp name hold the shipment header alias.
LCCUTHDTMP = GFTEMPNAME()  && Temp name hold the cutting ticket header alias.
LCCUTLNTMP = GFTEMPNAME()  && Temp name hold the cutting ticket line alias.
LCORDHDTMP = GFTEMPNAME()  && Temp name hold the order header alias.
LCORDLNTMP = GFTEMPNAME()  && Temp name hold the order line alias.
LCSTYLETMP = GFTEMPNAME()  && Temp name hold the style alias.
LCSTYDYTMP = GFTEMPNAME()  && Temp name hold the style dyelot alias.
LCSCALETMP = GFTEMPNAME()  && Temp name hold the scale alias.

LCENGLAND  = 'ENG'

*-- Hold the for condition based on warehouse setup "Multiple or single"
LCFCONDT = IIF(!LLALLWAREHS,'cWareCode = lcWareHouse','.T.')

*-- Check if the  PO, MF & SO are not installed, return from this function.
IF !LLPOINSTLD AND !LLMFINSTLD AND !LLSOINSTLD
  RETURN
ENDIF

*-- Save current work area
LNALIAS=SELECT()

*-- Assign flag to know if style file opened in this function or not
LLOPNSTYLE = .F.

*B132139,1 WSH 05/18/2006 [Start]
*llOpnStyle = gfOpenFile(oAriaApplication.DataDir+"STYLE", "STYLE", "SH",@lcStyleTmp,.T.)
LLOPNSTYLE = GFOPENTABLE(OARIAAPPLICATION.DATADIR+"STYLE", "STYLE", "SH",@LCSTYLETMP,.T.)
*B132139,1 WSH 05/18/2006 [End]

*-- Checking existance of OTS information.

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle)
SELECT (LCSTYLETMP)
=GFSEEK(LCSTYLE)
*B132139,1 WSH 05/18/2006 [End]

LLOTSINFO = .F.  && Assign flag to know if there is OTS data to be displayed

*-- Check if only one color has data, stop checking.
SCAN WHILE STYLE = LCSTYLE
  FOR I=1 TO 8
    Z=STR(I,1)
    *-- Check if there is data for the current style "Stock, WIP or Order" qty.
    IF STK&Z<>0 OR WIP&Z<>0 OR ORD&Z<>0
      *-- Set the OTS info. flag to true & exit from this loop
      LLOTSINFO = .T.
      EXIT
    ENDIF
  ENDFOR
  *-- If only one color has data, stop checking.
  IF LLOTSINFO
    EXIT
  ENDIF
ENDSCAN

*-- Check the OTS info. flag, if no data to display, return from this function
IF !LLOTSINFO
  *-- No Open to sell information found.
  =GFMODALGEN('TRM42045B42001','DIALOG')
  *-- Close the style file if opened in this function.
  IF LLOPNSTYLE

    *B132139,1 WSH 05/18/2006 [Start]
    *USE IN (lcStyleTmp)
    GFCLOSETABLE(LCSTYLETMP)
    *B132139,1 WSH 05/18/2006 [End]

  ENDIF
  RETURN
ENDIF

*-- Assign flag to know if open files in this program or not
STORE .F. TO LLOPNSCALE, LLOPNSTYDYE, LLOPNPOHDR, LLOPNPOLIN, LLOPNSHPHDR, LLOPNCUTHDR, LLOPNCUTLIN, LLOPNORDHDR, LLOPNORDLIN

*-- Open the needed files & set the opened flags to true if opened in this function

*B132139,1 WSH 05/18/2006 [Start]
*llOpnScale = gfOpenFile(oAriaApplication.DataDir+"SCALE", "SCALE", "SH",@lcScaleTmp,.T.)
*llOpnStyDye = gfOpenFile(oAriaApplication.DataDir+"STYDYE", "STYDYE", "SH",@lcStyDyTmp,.T.)
*IF llPOInstld
*  llOpnPoHdr  = gfOpenFile(oAriaApplication.DataDir+'POShdr','POSHdr'  ,'SH',@lcPoHdrTmp,.T.)
*  llOpnPoLin  = gfOpenFile(oAriaApplication.DataDir+'POSLN' ,'POSLNS'  ,'SH',@lcPolinTmp,.T.)
*  llOpnShpHdr = gfOpenFile(oAriaApplication.DataDir+'Shpmthdr','Shpmthdr','SH',@lcShpHdTmp,.T.)
*ENDIF
*IF llMFInstld
*  llOpnCutHdr = gfOpenFile(oAriaApplication.DataDir+"CUTTKTH", "CUTTKTH", "SH",@lcCutHdTmp,.T.)
*  llOpnCutLin = gfOpenFile(oAriaApplication.DataDir+"CUTTKTL", "CUTTKTLS", "SH",@lcCutlnTmp,.T.)
*ENDIF
*IF llSOInstld
*  llOpnOrdHdr = gfOpenFile(oAriaApplication.DataDir+"ORDHDR", "ORDHDR", "SH",@lcOrdHdTmp,.T.)
*  llOpnOrdLin = gfOpenFile(oAriaApplication.DataDir+"ORDLINE", "ORDLINES", "SH",@lcOrdLnTmp,.T.)
*ENDIF
LLOPNSCALE  = GFOPENTABLE(OARIAAPPLICATION.DATADIR+"SCALE", "SCALE", "SH",@LCSCALETMP,.T.)
LLOPNSTYDYE = GFOPENTABLE(OARIAAPPLICATION.DATADIR+"STYDYE", "STYDYE", "SH",@LCSTYDYTMP,.T.)
IF LLPOINSTLD OR LLMFINSTLD
  LLOPNPOHDR  = GFOPENTABLE(OARIAAPPLICATION.DATADIR+'POShdr','POSHdr'  ,'SH',@LCPOHDRTMP,.T.)
  LLOPNPOLIN  = GFOPENTABLE(OARIAAPPLICATION.DATADIR+'POSLN' ,'POSLNS'  ,'SH',@LCPOLINTMP,.T.)
  IF LLPOINSTLD
    LLOPNSHPHDR = GFOPENTABLE(OARIAAPPLICATION.DATADIR+'Shpmthdr','Shpmthdr','SH',@LCSHPHDTMP,.T.)
  ENDIF
ENDIF
IF LLSOINSTLD
  LLOPNORDHDR = GFOPENTABLE(OARIAAPPLICATION.DATADIR+"ORDHDR", "ORDHDR", "SH",@LCORDHDTMP,.T.)
  LLOPNORDLIN = GFOPENTABLE(OARIAAPPLICATION.DATADIR+"ORDLINE", "ORDLINES", "SH",@LCORDLNTMP,.T.)
ENDIF
*B132139,1 WSH 05/18/2006 [End]

*-- restore the work area
SELECT (LNALIAS)
*--Save current pointers since this function will change it.
LNCRSAV1=IIF(!EOF(),RECNO(),0)
*N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
*WAIT WINDOW 'Collecting Open to Sell information...' NOWAIT
WAIT WINDOW IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTSWAITWIND,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTSWAITWIND",LCARIAHFILE)) NOWAIT
*N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
*-- Create cursor hold the OTS qty. that will be displayed.
CREATE CURSOR (LCTMPOTS) (STYLE C(19),SZCNT C(1) ,SIZE C(5) ,;
  NQTY1 N(7),NQTY2 N(7),NQTY3 N(7),NQTY4 N(7),;
  NQTY5 N(7),NQTY6 N(7),NQTY7 N(7),NQTY8 N(7),;
  NQTY9 N(7),NQTY10 N(7),NQTY11 N(7),NQTY12 N(7))

*B038253,1 AMH Create index to use it for privent dublicate records [Start]
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON Style+Size TAG (lcTmpOTS) OF (lcTmpOTS)
INDEX ON STYLE+SIZE TAG (LCTMPOTS)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
*B038253,1 AMH [End]

*-- If display all warehouses, display data from Style file, if display specific
*-- warehouse, display data from style dyelot file for this warehouse.
SELECT IIF(LLALLWAREHS,LCSTYLETMP,LCSTYDYTMP)

*-- Set while clause condition
LCWCONDT = 'Style = lcStyle'

*-- Set for clause condition
*! N119687,1 WSH [Start] 04/27/2004 Fix Bug in Open To Sell Browse gfOTSDisp()
*lcFCondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse','.T.')

*B038253,1 AMH Consider the dyelot [Start]
*lcFCondt = IIF(!llAllWareHs,'cWareCode = lcWareHouse AND EMPTY(dyelot)','.T.')
LCFCONDT = IIF(!LLALLWAREHS,'cWareCode = lcWareHouse AND dyelot = lcDyelot','.T.')
*B038253,1 AMH [End]

*! N119687,1 WSH [End] 04/27/2004 Fix Bug in Open To Sell Browse gfOTSDisp()

*-- Scan for the current style to get all the style records

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle)
=GFSEEK(LCSTYLE)
*B132139,1 WSH 05/18/2006 [End]

SCAN WHILE &LCWCONDT FOR &LCFCONDT
  IF !LLALLWAREHS
    =SEEK(&LCSTYDYTMP..STYLE,LCSTYLETMP)
  ENDIF
  *-- Get the style non-major for the current style.
  LCCOLOR = SUBSTR(STYLE,LNSTYLEWID+1,LNCOLORWID+1)
  *-- Get the scale record for the current style.

  *B132139,1 WSH 05/18/2006 [Start]
  *=SEEK('S'+&lcStyleTmp..Scale,lcScaleTmp)
  =GFSEEK('S'+&LCSTYLETMP..SCALE,LCSCALETMP)
  *B132139,1 WSH 05/18/2006 [End]

  *-- Add record for each style /size in the OTS cursor.
  SELECT (LCTMPOTS)
  FOR I = 1 TO &LCSCALETMP..CNT
    Z = STR(I,1)

    *B038253,1 AMH Check if the record exist [Start]
    IF SEEK(EVALUATE(LCSTYLETMP+'.Style')+EVALUATE(LCSCALETMP+'.sz'+Z))
      LOOP
    ENDIF
    *B038253,1 AMH [End]

    APPEND BLANK
    REPLACE STYLE WITH &LCSTYLETMP..STYLE,;
      SZCNT WITH Z,;
      SIZE  WITH &LCSCALETMP..SZ&Z
  ENDFOR
ENDSCAN
*-- Create index on style + size in the OTS cursor
SELECT (LCTMPOTS)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON Style+SZCnt TAG (lctmpots) OF (lctmpots)
INDEX ON STYLE+SZCNT TAG (LCTMPOTS)
*B609356,1 SMA 07/27/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

*-- Initialize the necessary global variables.
LDTODAY = OARIAAPPLICATION.SYSTEMDATE
LCTHISM = ALLTRIM(STR(MONTH(LDTODAY)))  && Hold the current system date month
LCTHISY = ALLTRIM(STR(YEAR(LDTODAY)))   && Hold the current system date year
LCTHISD = ALLTRIM(STR(DAY(LDTODAY)))    && Hold the current system date day

*-- Array hold no. of days in each month.
DECLARE  LANOOFDAYS[12]
LANOOFDAYS = '31'
*-- Store 30 days in the following months "April, June, September & November"
STORE '30' TO LANOOFDAYS[4],LANOOFDAYS[6],LANOOFDAYS[9],LANOOFDAYS[11]

*-- Array hold "From - To" dates up to 10 periods.
DECLARE LADTPERIOD[10,3]
LADTPERIOD = {}                 && Set value of empty date type in the period array
LCPRDMONTH = LCTHISM            && Store Current month value in the period month
LNPRDMONTH = INT(VAL(LCTHISM))  && Store no. of the current month in the period month #
LCPRDYEAR  = LCTHISY            && Store Current yesr value in the period yesr

*-- Read date format & change it to England if the country is England or date setting is British.
LLENGDATE = (ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY) = LCENGLAND .OR. SET('DATE')='BRITISH' )

*-- First default for the no. of periods will be 10
LNPRDNUM = 10
*-- IF the OTS range setting is "2 Weeks" & the current day at the first half of the month
IF LCOTSPRD='E' AND VAL(LCTHISD)<=15
  *-- Set no. of periods to 11
  LNPRDNUM = 11
ENDIF

*-- If the OTS range setting is "Weekly"
IF LCOTSPRD = 'W'
  DO CASE
    *-- If the current date day in the second week of the month.
  CASE BETWEEN(VAL(LCTHISD),8,15)
    LNPRDNUM = 11
    *-- If the current date day in the third week of the month.
  CASE BETWEEN(VAL(LCTHISD),16,22)
    LNPRDNUM = 12
    *-- If the current date day in the forth week of the month.
  CASE BETWEEN(VAL(LCTHISD),22,31)
    LNPRDNUM = 13
  ENDCASE
ENDIF

*-- Define the Period array with the final no. of periods & define type empty date to its value.
DECLARE LADTPERIOD[lnPrdNum,3]
LADTPERIOD = {}

*-- Loop from first period to the total no. of periods
FOR I = 1 TO LNPRDNUM
  *-- Variable hold the string of the current period no.
  LCPRDCNT = PADL(ALLTRIM(STR(I,2)),2,"0")
  DO CASE
    *-- Case OTS range is every 2 weeks.
  CASE LCOTSPRD = 'E'
    IF LCPRDCNT $ '01-03-05-07-09-11'  && First part of month.
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('01/'+LCPRDMONTH+'/'+LCPRDYEAR)           && Hold Period start date
        LADTPERIOD[I,2] = CTOD('15/'+LCPRDMONTH+'/'+LCPRDYEAR)           && Hold period end date
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/01/'+LCPRDYEAR)              && Hold Period start date
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/15/'+LCPRDYEAR)              && Hold Period end date
      ENDIF
      *-- Hold the title of the period "ex.: JAN. 01-15"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 01-15'
      *-- Case the value of lcPrdCnt $ '02-04-06-08-10-12'
    ELSE
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('16/'+LCPRDMONTH+'/'+LCPRDYEAR)              && Hold the period start date
        LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
        LADTPERIOD[I,2] = CTOD(ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDMONTH+'/'+LCPRDYEAR)
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/16/'+LCPRDYEAR)                 && Hold the period start date
        LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/'+ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDYEAR)
      ENDIF
      *-- Hold the title of the period "ex.: FEB. 16-31"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 16-'+ALLTRIM(LANOOFDAYS[lnPrdMonth])
      *-- Assign new date value by adding one day to the current period end date
      LDNEWDATE  = (LADTPERIOD[I,2]+1)
      LCPRDMONTH = ALLTRIM(STR(MONTH(LDNEWDATE))) && Get period month name
      LNPRDMONTH = INT(VAL(LCPRDMONTH))           && Get period month #
      LCPRDYEAR  = ALLTRIM(STR(YEAR(LDNEWDATE)))  && Get period year
    ENDIF
    *-- Case OTS range is weekly.
  CASE LCOTSPRD = 'W'
    DO CASE
    CASE LCPRDCNT $ '01-05-09-13'  && First week.
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('01/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period start date
        LADTPERIOD[I,2] = CTOD('07/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period end date
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/01/'+LCPRDYEAR)     && Hold the period start date
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/07/'+LCPRDYEAR)     && Hold the period end date
      ENDIF
      *-- Hold the title of the period "ex.: FEB. 01-07"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 01-07'
    CASE LCPRDCNT $ '02-06-10'  && Second Week
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('08/'+LCPRDMONTH+'/'+LCPRDYEAR) && Hold the period start date
        LADTPERIOD[I,2] = CTOD('15/'+LCPRDMONTH+'/'+LCPRDYEAR) && Hold the period end date
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/08/'+LCPRDYEAR)    && Hold the period start date
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/15/'+LCPRDYEAR)    && Hold the period end date
      ENDIF
      *-- Hold the title of the period "ex.: FEB. 08-15"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 08-15'
    CASE LCPRDCNT $ '03-07-11'  && Third Week
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('16/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period start date
        LADTPERIOD[I,2] = CTOD('22/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period end date
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/16/'+LCPRDYEAR)     && Hold the period start date
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/22/'+LCPRDYEAR)     && Hold the period end date
      ENDIF
      *-- Hold the title of the period "ex.: FEB. 16-22"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 16-22'
    CASE LCPRDCNT $ '04-08-12'  && Forth week
      *-- If date setting is England, use the british format
      IF LLENGDATE
        LADTPERIOD[I,1] = CTOD('23/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period start date
        LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
        LADTPERIOD[I,2] = CTOD(ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period end date
        *-- If date setting is not England, use the normal format
      ELSE
        LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/23/'+LCPRDYEAR)     && Hold the period start date
        LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )  && Adjust Feb. no. of days depend on the current year value.
        LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/'+ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDYEAR)  && Hold the period end date
      ENDIF
      *-- Hold the title of the period "ex.: FEB. 23-31"
      LADTPERIOD[I,3] = SUBSTR(CMONTH(LADTPERIOD[I,1]),1,3)+'. 23-'+ALLTRIM(LANOOFDAYS[lnPrdMonth])
      *-- Assign new date value by adding one day to the current period end date
      LDNEWDATE  = (LADTPERIOD[I,2]+1)
      LCPRDMONTH = ALLTRIM(STR(MONTH(LDNEWDATE)))   && Get period month name
      LNPRDMONTH = INT(VAL(LCPRDMONTH))             && Get period month #
      LCPRDYEAR  = ALLTRIM(STR(YEAR(LDNEWDATE)))    && Get period year
    ENDCASE
    *-- Case OTS range is monthly.
  CASE LCOTSPRD = 'M'
    *-- If date setting is England, use the british format
    IF LLENGDATE
      LADTPERIOD[I,1] = CTOD('01/'+LCPRDMONTH+'/'+LCPRDYEAR)               && Hold the period start date
      LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )   && Adjust Feb. no. of days depend on the current year value.
      LADTPERIOD[I,2] = CTOD(ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDMONTH+'/'+LCPRDYEAR)  && Hold the period end date
      *-- If date setting is not England, use the normal format
    ELSE
      LADTPERIOD[I,1] = CTOD(LCPRDMONTH+'/01/'+LCPRDYEAR)                  && Hold the period start date
      LANOOFDAYS[2]   = IIF(MOD(YEAR(LADTPERIOD[I,1]),4)=0 ,'29' ,'28' )   && Adjust Feb. no. of days depend on the current year value.
      LADTPERIOD[I,2] = CTOD(LCPRDMONTH+'/'+ALLTRIM(LANOOFDAYS[lnPrdMonth])+'/'+LCPRDYEAR)  && Hold the period end date
    ENDIF
    *-- Hold the title of the period "ex.: JANUARY"
    LADTPERIOD[I,3] = PADR(CMONTH(LADTPERIOD[I,1]),10)
    *-- Assign new date value by adding one day to the current period end date
    LDNEWDATE  = (LADTPERIOD[I,2]+1)
    LCPRDMONTH = ALLTRIM(STR(MONTH(LDNEWDATE)))      && Get period month name
    LNPRDMONTH = INT(VAL(LCPRDMONTH))                && Get period month #
    LCPRDYEAR  = ALLTRIM(STR(YEAR(LDNEWDATE)))       && Get period year
  ENDCASE
ENDFOR

*-- Copy the period array we built to another array
=ACOPY(LADTPERIOD,LADTTMPPRD)
*-- Assign variable hold the row # that has to be removed from this array
LNREMVCOL = 0
*-- Loop to check if there is any period date range is less than the system date.
FOR I = 1 TO ALEN(LADTTMPPRD,1)
  IF OARIAAPPLICATION.SYSTEMDATE > LADTTMPPRD[I,1] AND OARIAAPPLICATION.SYSTEMDATE > LADTTMPPRD[I,2]
    *-- Define the row # that need to be removed from the period array
    LNREMVCOL = I
    LOOP
  ENDIF
ENDFOR

*-- Restore the laDtPeriod array after removing the out of date range period row that is less than system date
=ACOPY(LADTTMPPRD,LADTPERIOD,(LNREMVCOL*3)+1)

FOR I = 1 TO ALEN(LADTPERIOD,1)
  LCPRDCNT = ALLTRIM(STR(I,2))
  *-- Define heading to the first period title "Current"
  IF I = 1
    *N000682,1 MMT 12/03/2012 Globalization changes[Start]
    *laDtPeriod[I,3] = 'Current'
    LADTPERIOD[I,3] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CURRENTPERIOD,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CURRENTPERIOD",LCARIAHFILE))
    *N000682,1 MMT 12/03/2012 Globalization changes[END]
  ENDIF

  *--Browse fields heading
  LCMN&LCPRDCNT = LADTPERIOD[I,3]
ENDFOR

*-- Filling the main array.
SELECT (LCSTYLETMP)

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK(lcStyle)
*lnSize_cnt = IIF(SEEK('S'+&lcStyleTmp..Scale,lcScaleTmp), &lcScaleTmp..cnt , 1)
=SEEK(LCSTYLE) && no need to gfSeek again
LNSIZE_CNT = IIF(GFSEEK('S'+&LCSTYLETMP..SCALE,LCSCALETMP), &LCSCALETMP..CNT, 1)
*B132139,1 WSH 05/18/2006 [End]

*-- Define OTS info array with dimention of its size scale
DIMENSION LAOTCINFO[lnSize_cnt,12]
LAOTCINFO = 0

*-- If display all warehouses, display data from Style file, if display specific
*-- warehouse, display data from style dyelot file for this warehouse.
SELECT IIF(LLALLWAREHS,LCSTYLETMP,LCSTYDYTMP)

*B132139,1 WSH 05/18/2006 [Start]
*=SEEK( lcStyle )
=SEEK( LCSTYLE ) && No need to gfSeek again
*B132139,1 WSH 05/18/2006 [End]

*-- Scan for the current style to get all the style records
SCAN WHILE &LCWCONDT FOR &LCFCONDT
  LCPSTYLE  = STYLE
  LAOTCINFO = 0.00
  *-- Loop with the style size count
  FOR LNSZ_NO = 1 TO LNSIZE_CNT
    LCSZ_NO = STR(LNSZ_NO,1)
    LAOTCINFO[lnSz_no,01] = STK&LCSZ_NO                && Get the stock value
    LAOTCINFO[lnSz_no,12] = ORD&LCSZ_NO + SHP&LCSZ_NO  && Get the order + shipped value
  ENDFOR

  *B132139,1 WSH 05/18/2006 [Start]
  **-- If the MF module is installed & style is MAKE "Manufactured"
  *IF llMFInstld AND &lcStyleTmp..MAKE
  *  *-- Procedure to compute the OTS quantites for the manufactured styles (MAKE : YES)
  *  DO lpOTSmYes WITH lcPStyle
  *ENDIF
  *
  **-- If the PO module is installed & style is not MAKE "Imported"
  *IF llPOInstld AND !&lcStyleTmp..MAKE
  *  *-- Procedure to compute the OTS quantites for the imported styles (MAKE : NO)
  *  DO lpOTSmNo  WITH lcPStyle
  *ENDIF

  *-- If the PO OR MF modules is installed
  IF LLPOINSTLD OR LLMFINSTLD
    *-- Procedure to compute the OTS quantites for the styles
    DO LPOTSMNO  WITH '0001' + LCPSTYLE
  ENDIF
  *B132139,1 WSH 05/18/2006 [End]

  *-- If the SO module is installed.
  IF LLSOINSTLD
    *-- Procedure to substract the order line quantities from the OTS quantities.
    DO LPORDQSUB WITH LCPSTYLE
  ENDIF

  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
  *C102567,1     the calculation will remain the same [Begin]
  IF !LLOTSBASTR
    *C102567,1 [End]
    *-- To post the negative values to the nearst positive value.
    DO LPPSTNVVLU
    *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
    *C102567,1     get the OTS for each period [Begin]
  ELSE
    *-- Update OTS qty. in each period
    DO LPUPDOTS
  ENDIF
  *C102567,1 [End]

  *-- Replace the OTS Quantities.
  DO LPOTSLINS WITH LCPSTYLE
ENDSCAN

*-- Restore old work area
SELECT(LNALIAS)
*-- Restore record pointer
IF LNCRSAV1 <> 0
  GOTO LNCRSAV1
ENDIF

*-- Calculate all the table columns to variables
SELECT (LCTMPOTS)
SUM NQTY1,NQTY2,NQTY3,NQTY4,NQTY5,NQTY6,NQTY7,NQTY8,NQTY9,NQTY10,NQTY11,NQTY12 TO ;
  LNQTY1,LNQTY2,LNQTY3,LNQTY4,LNQTY5,LNQTY6,LNQTY7,LNQTY8,LNQTY9,LNQTY10,LNQTY11,LNQTY12

*-- Add new record in the OTS cursor with for columns & rows totals

*B038253,1 AMH Add total word [Start]
*INSERT INTO (lcTmpOTS) (STYLE,SZCnt,nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12) ;
VALUES (REPL(CHR(255),LEN(ALLTRIM(SUBSTR(lcStyle,1,lnStyleWid)))),'W',lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12)
INSERT INTO (LCTMPOTS) (STYLE,SZCNT,SIZE,NQTY1,NQTY2,NQTY3,NQTY4,NQTY5,NQTY6,NQTY7,NQTY8,NQTY9,NQTY10,NQTY11,NQTY12) ;
  VALUES (REPL(CHR(255),LEN(ALLTRIM(SUBSTR(LCSTYLE,1,LNSTYLEWID)))),'W','Total',LNQTY1,LNQTY2,LNQTY3,LNQTY4,LNQTY5,LNQTY6,LNQTY7,LNQTY8,LNQTY9,LNQTY10,LNQTY11,LNQTY12)
*B038253,1 AMH [End]

*-- Change the calling of the trigger to be under condition if exist.
IF ASCAN(OARIAAPPLICATION.LAEVNTTRIG,PADR("ACCUMOTS",10)) <> 0
  *--C102237,1 TMI [START] A new line will be added right below each style sizw shown in the OTS
  *--C102237,1 TMI         screen. This line will show the accumulative OTS balance
  =GFDOTRIGER('ICSTYLE',PADR('ACCUMOTS',10))
  *--C102237,1 TMI [END  ]
ENDIF

*-- Define variable hold the browse fields.
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcBrFields = IIF(llAllClrs,"lcSty=SUBSTR(Style,lnStyleWid+2,lnColorWid) :H=lcNMjrTl,","")+;
*!*	  "Size ," +;
*!*	  "nQty1  :H=lcMn1  :P='999999',"+;
*!*	  "nQty2  :H=lcMn2  :P='999999',"+;
*!*	  "nQty3  :H=lcMn3  :P='999999',"+;
*!*	  "nQty4  :H=lcMn4  :P='999999',"+;
*!*	  "nQty5  :H=lcMn5  :P='999999',"+;
*!*	  "nQty6  :H=lcMn6  :P='999999',"+;
*!*	  "nQty7  :H=lcMn7  :P='999999',"+;
*!*	  "nQty8  :H=lcMn8  :P='999999',"+;
*!*	  "nQty9  :H=lcMn9  :P='999999',"+;
*!*	  "nQty10 :H='"+LANG_HdFuture+"' :P='999999',"+;
*!*	  "nQty11 :H='"+LANG_HdTotAvl+"' :P='999999',"+;
*!*	  "nQty12 :H='"+LANG_HdTotSld+"' :P='999999'"
LCBRFIELDS = IIF(LLALLCLRS,"lcSty=SUBSTR(Style,lnStyleWid+2,lnColorWid) :H=lcNMjrTl,","")+;
  "Size   :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SIZEBR,OARIAAPPLICATION.GETHEADERTEXT("LANG_SIZEBR",LCGFOTSDISP))+"'," +;
  "nQty1  :H=lcMn1  :P='999999',"+;
  "nQty2  :H=lcMn2  :P='999999',"+;
  "nQty3  :H=lcMn3  :P='999999',"+;
  "nQty4  :H=lcMn4  :P='999999',"+;
  "nQty5  :H=lcMn5  :P='999999',"+;
  "nQty6  :H=lcMn6  :P='999999',"+;
  "nQty7  :H=lcMn7  :P='999999',"+;
  "nQty8  :H=lcMn8  :P='999999',"+;
  "nQty9  :H=lcMn9  :P='999999',"+;
  "nQty10 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_HDFUTURE,OARIAAPPLICATION.GETHEADERTEXT("LANG_HdFuture",LCGFOTSDISP))+"' :P='999999',"+;
  "nQty11 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_HDTOTAVL,OARIAAPPLICATION.GETHEADERTEXT("LANG_HdTotAvl",LCGFOTSDISP))+"' :P='999999',"+;
  "nQty12 :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_HDTOTSLD,OARIAAPPLICATION.GETHEADERTEXT("LANG_HdTotSld",LCGFOTSDISP))+"' :P='999999'"
*N000682,1 MMT 12/03/2012 Globalization changes[END]
*-- Clear the wait message to execute the browse function
WAIT CLEAR

*-- Call global function browse to display the OTS info.
GOTO TOP
*N000682,1 MMT 12/03/2012 Globalization changes[START]
*!*	=ARIABROW('',LANG_BrowTitl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
*!*	  'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
*! B610203,1 HIA 01/17/2013 Aria4xp - IC - Style- Cut & Sold - OTS does not allow filters to be saved [T20130103.0004][start]
*!*	=ARIABROW('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BrowTitl,oAriaApplication.GetHeaderText("LANG_BrowTitl",lcGfotsdisp)),gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
*!*	  'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
=ARIABROW('',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BROWTITL,OARIAAPPLICATION.GETHEADERTEXT("LANG_BrowTitl",LCGFOTSDISP)),GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,'',;
  'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.,"" ,.F.,.F.,"OTS")
*! B610203,1 HIA 01/17/2013 Aria4xp - IC - Style- Cut & Sold - OTS does not allow filters to be saved [T20130103.0004][END]
*N000682,1 MMT 12/03/2012 Globalization changes[END]
*-- Check the opened flag to close the files that have been opened in this function

*B132139,1 WSH 05/18/2006 [Start]
*IF llOpnScale
*  USE IN (lcScaleTmp)
*ENDIF
*IF llOpnStyle
*  USE IN (lcStyleTmp)
*ENDIF
*IF llOpnPoHdr
*  USE IN (lcPoHdrTmp)
*ENDIF
*IF llOpnPoLin
*  USE IN (lcPolinTmp)
*ENDIF
*IF llOpnShpHdr
*  USE IN (lcShpHdTmp)
*ENDIF
*IF llOpnCutHdr
*  USE IN (lcCutHdTmp)
*ENDIF
*IF llOpnCutLin
*  USE IN (lcCutlnTmp)
*ENDIF
*IF llOpnOrdHdr
*  USE IN (lcOrdHdTmp)
*ENDIF
*IF llOpnOrdLin
*  USE IN (lcOrdLnTmp)
*ENDIF
IF LLOPNSCALE
  =GFCLOSETABLE(LCSCALETMP)
ENDIF
IF LLOPNSTYLE
  =GFCLOSETABLE(LCSTYLETMP)
ENDIF
IF LLOPNPOHDR
  =GFCLOSETABLE(LCPOHDRTMP)
ENDIF
IF LLOPNPOLIN
  =GFCLOSETABLE(LCPOLINTMP)
ENDIF
IF LLOPNSHPHDR
  =GFCLOSETABLE(LCSHPHDTMP)
ENDIF
IF LLOPNORDHDR
  =GFCLOSETABLE(LCORDHDTMP)
ENDIF
IF LLOPNORDLIN
  =GFCLOSETABLE(LCORDLNTMP)
ENDIF
*B132139,1 WSH 05/18/2006 [End]

*-- Restore work area
SELECT(LNALIAS)
RETURN


*:******************************************************************
*! PROG      : lpOTSmYes
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to compute the OTS quantites for the
*!             manufactured styles (MAKE : YES)
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB
*!************************************************************************
PROCEDURE LPOTSMYES
PARAMETERS LCOTSKEY

*-- Scan in the cutting ticket line with the current style & warehouse
SELECT (LCCUTLNTMP)
SEEK LCOTSKEY
SCAN WHILE STYLE = LCOTSKEY FOR (LLALLWAREHS OR CWARECODE=LCWAREHOUSE)
  *-- For the founded line, check if its Cutting ticket # is "Open, On Hold or Active" in the cutting ticket header
  =SEEK(CUTTKT,LCCUTHDTMP)
  IF &LCCUTHDTMP..STATUS $ 'OAH'
    *-- Call procedure to accumulate the cutting ticket lines quantity to the OTS info.
    *C102567,1 HBG Pass the parameter of add to current by .T. to get the OTS in the correct period [Begin]
    DO ACUM_OTS WITH LCCUTLNTMP , &LCCUTHDTMP..COMPLETE , IIF(TRANCD='1',1,-1) , .T. , .T.
    *C102567,1 [End]
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! Name      : lpOTSmNo
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! Purpose   : Procedure to compute the OTS quantites for the imported
*!             styles (MAKE : NO)
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE LPOTSMNO
PARAMETERS LCOTSKEY

PRIVATE LCPO_NO

*-- Define aray with dimention of style size count
DIMENSION LNQTY[lnSize_cnt]
LNQTY   = 0

*B132139,1 WSH 05/18/2006 [Start]
*lcPo_no = ''
LCPO_NO = SPACE(8)
*B132139,1 WSH 05/18/2006 [End]

*-- Define 2 arrays to check if there is over rescive or not[Begin]
DIMENSION LAPOVLU[lnSize_cnt],LAOVERRESV[lnSize_cnt]
LAPOVLU    = 0
LAOVERRESV = .F.

*-- Scan in the PO lines with the current style & warehouse
SELECT (LCPOLINTMP)

*B132139,1 WSH 05/18/2006 [Start]
*SEEK lcOtsKey
*SCAN WHILE Style = lcOtsKey ;
*     FOR cStyType<>'C' AND (llAllWareHs OR cWareCode=lcWareHouse) AND TranCd<>'6'
*  *-- For the founded line, check if its PO # is "Open, On Hold" in the PO header
*  =SEEK(&lcPolinTmp..cStyType+&lcPolinTmp..PO,lcPoHdrTmp)
=GFSEEK(LCOTSKEY)
SCAN WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = LCOTSKEY ;
    FOR CSTYTYPE $ "ADPUN" AND (LLALLWAREHS OR CWARECODE=LCWAREHOUSE) AND TRANCD <> '6'
  *-- For the founded line, check if its PO # is "Open, On Hold" in the PO header
  =GFSEEK(CBUSDOCU+CSTYTYPE+PO, LCPOHDRTMP)
  *B132139,1 WSH 05/18/2006 [End]

  *B608884,1 WAM 06/02/2009 Include Actualized PO in OTS calculation
  *IF !(&lcPoHdrTmp..Status $ 'OH')
  IF !(&LCPOHDRTMP..STATUS $ 'OHA')
    *B608884,1 WAM 06/02/2009 (End)

    LOOP
  ENDIF

  *-- Define the transaction sign depend on the line type & PO type.

  *B132139,1 WSH 05/18/2006 [Start]
  *lcTrSign = IIF(Trancd='1',1,-1) * IIF(cStyType $ 'ADPN',1,-1)
  LCTRSIGN = IIF(TRANCD='1',1,-1) * IIF(CBUSDOCU $ 'R', -1, 1)
  *B132139,1 WSH 05/18/2006 [End]

  *-- Get transaction date
  LDTRANDATE = IIF (EMPTY(&LCPOHDRTMP..AVAILABLE),&LCPOHDRTMP..COMPLETE ,&LCPOHDRTMP..AVAILABLE)

  LLARRQTY = .F.
  *-- If finishing looping in the PO line & the PO has changed

  *B132139,1 WSH 05/18/2006 [Start]
  *IF lcPo_no = PO
  IF LCPO_NO = CBUSDOCU+CSTYTYPE+PO
    *B132139,1 WSH 05/18/2006 [End]

    *-- Check For reciving and damaged and canceled Qty to fix bug of over reciecve
    IF TRANCD $ '245'
      LLARRQTY = .T.
      *-- Loop to check the over receive qty.
      FOR LNCOUNT = 1 TO LNSIZE_CNT
        LCCOUNT = STR(LNCOUNT,1)
        *-- If there is over receive in the current qty., add zero to the qty. array.
        IF LAOVERRESV[lnCount]
          LNQTY[lnCount] = 0
          *-- If not over receive the current qty.
        ELSE
          *-- If the current qty. greater than the budget qty. & the budget qty. greater than zero
          IF QTY&LCCOUNT. > LAPOVLU[lnCount] AND LAPOVLU[lnCount] > 0
            *-- Save budget qty.
            LNQTY[lnCount]      = LAPOVLU[lnCount]
            *-- Set the flag of over recive to .T.
            LAOVERRESV[lnCount] = .T.
          ELSE
            *-- Save current Budget qty.
            LNQTY[lnCount]      = QTY&LCCOUNT.
            LAPOVLU[lnCount]    = LAPOVLU[lnCount] - QTY&LCCOUNT.
            IF LAPOVLU[lnCount] = 0
              *-- Set the flag of over recive to .T.
              LAOVERRESV[lnCount] = .T.
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
    *-- If the PO has been changed
  ELSE
    *-- Assign the PO # to the PO variable

    *B132139,1 WSH 05/18/2006 [Start]
    *lcPo_no = PO
    LCPO_NO = CBUSDOCU+CSTYTYPE+PO
    *B132139,1 WSH 05/18/2006 [End]

    FOR LNCOUNT = 1 TO LNSIZE_CNT
      LCCOUNT = STR(LNCOUNT,1)
      *-- Save the current qty. in the qty. array.
      LNQTY[lnCount] = QTY&LCCOUNT.
      *-- Save the budget qty. of the PO line.
      LAPOVLU[lnCount] = QTY&LCCOUNT.
      *-- Set the flag of over recive to .F.
      LAOVERRESV = .F.
    ENDFOR
  ENDIF
  *-- Reham
  *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
  *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger

  *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0
  *DO acum_ots WITH lcPolinTmp , ldTranDate , lcTrSign , .T. , .F. , llArrQty
  *-- To accumulate the quantity in process.
  *-- Update Open & hold PO's if it is prior to date range in First period not the current based on JL trigger
  IF ASCAN(OARIAAPPLICATION.LAEVNTTRIG , PADR('UPDPO1', 10)) <> 0
    =GFDOTRIGER('ICSTYLE',PADR('UPDPO1',10))
    *-- Reham
  ELSE
    DO ACUM_OTS WITH LCPOLINTMP , LDTRANDATE , LCTRSIGN , .T. , .T. , LLARRQTY
  ENDIF

  *-- If there is shipped qty.

  *B132139,1 WSH 05/18/2006 [Start]
  *IF Trancd = '3'
  IF TRANCD = '3' AND LLPOINSTLD
    *B132139,1 WSH 05/18/2006 [End]

    *-- Get the shipment info.

    *B132139,1 WSH 05/18/2006 [Start]
    *=SEEK(&lcPolinTmp..ShipNo,lcShpHdTmp)
    *B609507,1 TMI 01/20/2011 [Start] replace the SEEK command with gfSeek for the shpmthdr file as it is a sql file
    *=SEEK(&lcPolinTmp..cBusDocu + &lcPolinTmp..cStyType + &lcPolinTmp..ShipNo, lcShpHdTmp)
    =GFSEEK(&LCPOLINTMP..CBUSDOCU + &LCPOLINTMP..CSTYTYPE + &LCPOLINTMP..SHIPNO, LCSHPHDTMP)
    *B609507,1 TMI 01/20/2011 [End  ]
    *B132139,1 WSH 05/18/2006 [End]

    *-- Reham
    *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
    *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger

    *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0
    *  DO acum_ots WITH lcPolinTmp , &lcShpHdTmp..eta , 1 , .T. , .F. , llArrQty
    *-- To accumulate the quantity in process.
    *-- Update Open & hold PO's if it is prior to date range in First period not the current based on JL trigger
    IF ASCAN(OARIAAPPLICATION.LAEVNTTRIG , PADR('UPDPO2', 10)) <> 0
      =GFDOTRIGER('ICSTYLE',PADR('UPDPO2',10))
      *-- Reham
    ELSE
      DO ACUM_OTS WITH LCPOLINTMP , &LCSHPHDTMP..ETA , 1 , .T. , .T. , LLARRQTY
    ENDIF
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! PROG      : lpOrdQSub
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to substract the order line quantities from the
*!             OTS quantities .
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns    : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE LPORDQSUB
PARAMETERS LCOTSKEY

*-- Scan in the order lines with the current style & warehouse
SELECT (LCORDLNTMP)

*B132139,1 WSH 05/18/2006 [Start]
*SEEK lcOtsKey
=GFSEEK(LCOTSKEY)
*B132139,1 WSH 05/18/2006 [End]

*B000000,1 HBG 01/30/2005 Get the OTS on Configuration level if use configuration [Begin]
*SCAN WHILE Style = lcOtsKey FOR cOrdType='O'
SCAN WHILE STYLE = LCOTSKEY FOR CORDTYPE='O' AND IIF(LLUSECONFIG,DYELOT=LCDYELOT,.T.)
  *B000000,1 [End]

  *B132139,1 WSH 05/18/2006 [Start]
  *=SEEK('O'+&lcOrdLnTmp..Order,lcOrdHdTmp)
  =GFSEEK('O'+&LCORDLNTMP..ORDER,LCORDHDTMP)
  *B132139,1 WSH 05/18/2006 [End]

  *-- For the founded line, check if its order # is "Open, On Hold" in the order header
  IF &LCORDHDTMP..STATUS$'OH' AND (LLALLWAREHS OR &LCORDHDTMP..CWARECODE=LCWAREHOUSE)
    *-- Call procedure to accumulate the order lines quantity to the OTS info.
    DO ACUM_OTS WITH LCORDLNTMP , START , -1 , .F. , .T.
  ENDIF
ENDSCAN
RETURN

*:******************************************************************
*! PROG      : lpPstNvVlu
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to post the negative values in any period to the
*!             nearst positive value of any next period.
*!             - We do this by looping the two dimensional array "laOTCInfo"
*!               in case of the report format is by size , otherwise we will
*!               loop just the last row of the same array (which holds the
*!               total OTS quantities for this color) ,to do the following :
*!             - If there is a negative value , we will make it 0 and subtract
*!               it from the nearst next period has a POSITIVE value.
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE LPPSTNVVLU

*-- Define on hand variable
XON_HAND = 0
*-- Loop with the no. of the style sizes
FOR LNSZ_NO = 1 TO LNSIZE_CNT
  *-- Loop in the first 10 periods
  FOR I = 1 TO 10
    *-- If the current OTS value for the current size & in the current period in negative value.
    IF LAOTCINFO[lnSz_no,I] < 0
      *-- Store this negative value to the On Hand value
      XON_HAND = LAOTCINFO [lnSz_no,I]
      *-- Set the negative value rto zero
      LAOTCINFO[lnSz_no,I] = 0
      *-- If first period
      IF I = 1
        DO LPPOSTBF WITH 2,10                  && GO_BACKWORD   ->
        *-- If any other period
      ELSE
        DO LPPOSTBF WITH I-1,1                 && GO_BACKWORD   <-
        *-- If the On Hand value is negative & the period is not the tenth period
        IF (XON_HAND < 0) AND (I < 10)
          DO LPPOSTBF WITH I+1,10              && GO_FOREWORD   ->
        ENDIF
      ENDIF
    ENDIF   &&-> -VE NUMBER
  ENDFOR
  *-- If the On hand qty. is negative
  IF XON_HAND < 0
    *-- Put the remaining negative qty. in the tenth period
    LAOTCINFO[lnSz_no,10] = XON_HAND
  ENDIF
  *-- Set on hand value to zero
  XON_HAND = 0
ENDFOR
RETURN

*:******************************************************************
*! PROG      : lpUpdOTS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/04/2002
*! DESC : Update OTS qty. in each period
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C102567,1 HBG
*!*************************************************************
PROCEDURE LPUPDOTS

*-- Loop with the no. of the style sizes.
FOR LNSZ_NO = 1 TO LNSIZE_CNT
  *-- Get the OTS value for the current size
  LNCURRENT = LAOTCINFO[lnSz_no,1]
  *-- Loop from second period to the total no. of periods
  FOR I = 2 TO LNPRDNUM
    *-- Increase the OTS value with the current size in the current period with the current value
    LAOTCINFO[lnSz_no,I] = LAOTCINFO[lnSz_no,I] + LNCURRENT
    *-- Put the the OTS value with the current size in the current period in the current value
    LNCURRENT = LAOTCINFO[lnSz_no,I]
  ENDFOR
ENDFOR

*:******************************************************************
*! PROG      : lpOTSLins
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Replace the OTS Quantities.
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!E500304,1 WAB
*!*************************************************************
PROCEDURE LPOTSLINS
PARAMETERS LCOTSKEY

*-- Loop to accumulate the the OTS lines and total to the 11th col. {Period} in the array .
FOR LNSZ_NO = 1 TO LNSIZE_CNT
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
  *C102567,1     the calculation will remain the same
  IF !LLOTSBASTR
    FOR I = 1 TO 10
      LAOTCINFO[lnSz_no,11] = LAOTCINFO[lnSz_no,11] + LAOTCINFO[lnSz_no,I]
    ENDFOR
    *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
    *C102567,1     the Total avalibale is the future qty
  ELSE
    LAOTCINFO[lnSz_no,11] = LAOTCINFO[lnSz_no,10]
  ENDIF
ENDFOR

*-- Loop with the no. of the style sizes to update the OTS cursor lines for each [color\size].
FOR LNSZ_NO = 1 TO LNSIZE_CNT
  LCSZ_NO = STR(LNSZ_NO,1)
  SELECT (LCTMPOTS)
  *-- Seek the current style & the current size
  =SEEK(LCOTSKEY+LCSZ_NO)
  *-- Loop in the first 12 periods
  FOR I = 1 TO 12
    Z = ALLTRIM(STR(I,2))
    *-- Update the style/size qty. with the current OTS value
    REPLACE NQTY&Z WITH LAOTCINFO[lnSz_no,I]
  ENDFOR
ENDFOR
RETURN

*:******************************************************************
*! PROG      : ACUM_OTS
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : Procedure to accumulate the OTS quantities .
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  :
*!        1) lcMFile    : To hold the file name in process .
*!        2) ldTrDate   : To hold the date in process .
*!        3) lnSnType   : To deside whether to add or subtract the
*!                        quantity in process to or from the balance .
*!        4) llAcumOTS  : If this funtion was called to accumalats O.T.S Qty
*!                        or Subtract the order Qty .
*!        5) llAddToCur : The qty before periods add to the current coloumn.
*!        6) llArrQty   : Flag to determine if use original transaction
*!                      : qty. or array qty.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE ACUM_OTS
PARAMETERS LCMFILE , LDTRDATE , LNSNTYPE , LLACUMOTS  , LLADDTOCUR , LLARRQTY
PRIVATE LNX

*-- Define if we are going to accumalate OTS qty. or subtract order qty
LNX = IIF(LLACUMOTS,2,1)

*-- The columns of the array laOTCInfo are initialized
*-- By the stock Qty and this function is going to accumalats the O.T.S QTY
*-- Or Subtract the order Qty form each column , But now we need to exclude
*-- The first column of this array (i.e laOTCInfo[1,x]) form this process
*-- Because this column carry the current O.T.S Qty .

*C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'No'
*C102567,1     the calculation will remain the same
SELECT (LCMFILE)
IF !LLOTSBASTR
  *-- If the current transaction date is between the defined periods date range.
  IF BETWEEN(LDTRDATE , LADTPERIOD[1,1] , LADTPERIOD[10,2])
    *-- Loop with the no. of the style sizes
    FOR  LNSZ_NO = 1 TO LNSIZE_CNT
      LCSZ_NO = STR(LNSZ_NO,1)
      *-- Determin if we will use lnQty Array or QtyX fields
      LCQTY = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
      *-- Search for the right period to process .
      FOR I = LNX TO 10
        *-- Check if the current transaction date is in the range of the current period
        IF BETWEEN(LDTRDATE , LADTPERIOD[I,1] , LADTPERIOD[I,2])
          *-- Accumulate the OTS values.
          LAOTCINFO[lnSz_no,I] = LAOTCINFO[lnSz_no,I] + ( EVALUATE(LCQTY) * LNSNTYPE )
          EXIT
        ENDIF
      ENDFOR

      *-- If the current transaction is less than 1st period or between first and second periods
      *-- and flag of accumulate the OTS qty. is true
      IF (LADTPERIOD[1,1] <= LDTRDATE).AND.(LDTRDATE < LADTPERIOD[2,1]) .AND. LLACUMOTS

        *-- Reham
        *-- Change the checking of the trigger name, Change the calling of the trigger, instead calling the standard
        *-- code, The commented line has to be moved to customer main program in a new function with the name of the trigger

        *C102567,1 Accumalate the OTS qty. in the correct period
        *IF ASCAN(oAriaApplication.laEvntTrig , PADR('ACCUMOTS', 10)) <> 0
        *  laOTCInfo[lnSz_no,2]=laOTCInfo[lnSz_no,2] + ( EVALUATE(lcQty) * lnSnType )
        IF ASCAN(OARIAAPPLICATION.LAEVNTTRIG , PADR('UPDOTS', 10)) <> 0
          =GFDOTRIGER('ICSTYLE',PADR('UPDOTS',10))
        ELSE
          LAOTCINFO[lnSz_no,1]=LAOTCINFO[lnSz_no,1] + ( EVALUATE(LCQTY) * LNSNTYPE )
        ENDIF
      ENDIF
    ENDFOR
    *-- If the current transaction date is not between the 1st period start date & the 10th period end date
  ELSE
    *-- If the current transaction date is greater than the end date of 10th period
    IF LDTRDATE > LADTPERIOD[10,2]
      FOR LNSZ_NO = 1 TO LNSIZE_CNT
        LCSZ_NO = STR(LNSZ_NO,1)
        *-- Determin if we will use lnQty Array or QtyX fields
        LCQTY = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
        *-- Accumulate the OTS values to the 10th period
        LAOTCINFO[lnSz_no,10]=LAOTCINFO[lnSz_no,10] + ( EVALUATE(LCQTY) * LNSNTYPE )
      ENDFOR
      *-- If the current transaction date is less than or equal the end date of 10th period
    ELSE
      FOR LNSZ_NO = 1 TO LNSIZE_CNT
        LCSZ_NO = STR(LNSZ_NO,1)
        *-- Determin if we will use lnQty Array or QtyX fields
        LCQTY = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
        IF LLADDTOCUR
          *-- If llAddToCur is true add the qty to current column
          LAOTCINFO[lnSz_no,1] = LAOTCINFO[lnSz_no,1] + ( EVALUATE(LCQTY)* LNSNTYPE )
        ELSE
          *-- If llAddToCur is false add the tran. qty. to the first period not the current period
          LAOTCINFO[lnSz_no,2] = LAOTCINFO[lnSz_no,2] + ( EVALUATE(LCQTY)* LNSNTYPE )
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  *C102567,1 HBG IF Get the setting of OTS based on exact transaction date = 'Yes'
  *C102567,1     Update each transaction date with the Qty of the transaction happen in it
ELSE
  *-- If the current transaction date is between the 1st period start date & the 10th period end date
  IF BETWEEN(LDTRDATE , LADTPERIOD[1,1] , LADTPERIOD[10,2])
    *-- Loop with the no. of the style sizes
    FOR LNSZ_NO = 1 TO LNSIZE_CNT
      *-- Check if the current transaction date is in date range of 1st period
      IF BETWEEN(LDTRDATE , LADTPERIOD[1,1] , LADTPERIOD[1,2])
        LCSZ_NO = STR(LNSZ_NO,1)
        *-- Determin if we will use lnQty Array or QtyX fields
        LCQTY   = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
        *-- Accumulate the OTS values to the 1st period
        LAOTCINFO[lnSz_no,1] = LAOTCINFO[lnSz_no,1] + ( EVALUATE(LCQTY) * LNSNTYPE )
        *-- If the current transaction date is not in date range of 1st period
      ELSE
        *-- Search for the right period to process .
        FOR I = LNX TO LNPRDNUM
          IF BETWEEN(LDTRDATE , LADTPERIOD[I,1] , LADTPERIOD[I,2]) AND IIF(I > LNX , LDTRDATE > LADTPERIOD[I-1,2] , .T.)
            LCSZ_NO = STR(LNSZ_NO,1)
            *-- Determin if we will use lnQty Array or QtyX fields
            LCQTY   = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
            *-- Accumulate the OTS values to the 1st period
            LAOTCINFO[lnSz_no,I] = LAOTCINFO[lnSz_no,I] + ( EVALUATE(LCQTY) * LNSNTYPE )
          ENDIF
        ENDFOR
      ENDIF
    ENDFOR
    *-- If the current transaction date is not between the 1st period start date & the 10th period end date
  ELSE
    *-- If the current transaction date is greater than the end date of 10th period
    IF LDTRDATE > LADTPERIOD[10,2]
      FOR LNSZ_NO = 1 TO LNSIZE_CNT
        LCSZ_NO = STR(LNSZ_NO,1)
        *-- Determin if we will use lnQty Array or QtyX fields
        LCQTY   = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
        *-- Accumulate the OTS values to the 10th period value
        LAOTCINFO[lnSz_no,10] = LAOTCINFO[lnSz_no,10] + ( EVALUATE(LCQTY) * LNSNTYPE )
      ENDFOR
      *-- If the current transaction date is less than or equal the end date of 10th period
    ELSE
      FOR LNSZ_NO = 1 TO LNSIZE_CNT
        LCSZ_NO = STR(LNSZ_NO,1)
        *-- Determin if we will use lnQty Array or QtyX fields
        LCQTY   = IIF(LLARRQTY,'lnQty['+LCSZ_NO+']','Qty'+LCSZ_NO)
        IF LLADDTOCUR
          *-- If llAddToCur is true add the qty to current column
          LAOTCINFO[lnSz_no,1]=LAOTCINFO[lnSz_no,1] + ( EVALUATE(LCQTY)* LNSNTYPE )
        ELSE
          *-- If llAddToCur is false add the tran. qty. to the first period not the current period
          LAOTCINFO[lnSz_no,2]=LAOTCINFO[lnSz_no,2] + ( EVALUATE(LCQTY)* LNSNTYPE )
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF
RETURN

*:******************************************************************
*! PROG      : lpPostBf
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/24/1999
*! DESC      : To do the posting either backword or forword.
*! NOTE      : Called from lpPstNvVlu
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnIniPer,lnEndPer->Initial and end periods.
*!*************************************************************
*! Returns   : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE LPPOSTBF
PARAMETERS LNINIPER ,LNENDPER

*-- Define the for direction "From , To , Direction {Forward/Backword}"
LCFLOOP = IIF(LNINIPER>LNENDPER,'lnIniPer TO lnEndPer STEP -1','lnIniPer TO lnEndPer')

FOR I = &LCFLOOP
  *-- Add the current OTS value to the on hand value
  XON_HAND =  XON_HAND + LAOTCINFO[lnSz_no,I]
  *-- If the on hand value is negative or is zero
  IF XON_HAND <= 0
    *-- Set the current OTS value to zero
    LAOTCINFO[lnSz_no,I] = 0
    *-- If the on hand value is positive
  ELSE
    *-- Save on hand value in the current period
    LAOTCINFO[lnSz_no,I] = XON_HAND
    *-- Set on hand value to zero
    XON_HAND = 0
    EXIT
  ENDIF
ENDFOR
RETURN


*:---------------------------------------------------------------------
*! Name      : POFBROW
*! Developer : Wael Aly Mohamed
*! Date      : 05/04/97
*! Purpose   : Function to browse material POS
*: Job ID    : E300637,1
*:---------------------------------------------------------------------
*: Calls              : gfDialog,AriaBrow
*:---------------------------------------------------------------------
*: Passed Parameters  : lcPOType : PO Type
*:                      lcVendor : Vendor
*:                      lcPo     : PO#
*:---------------------------------------------------------------------
*: Example            : =POFBROW('P',@lcVendor,@lcPo)
*:---------------------------------------------------------------------
FUNCTION POFBROW
PARAMETER LCPOTYPE,LCVENDOR,LCPO
PRIVATE LCBRFIELDS,LCALIAS,LADATA,LLRETVALUE,LCPOORDER


LCALIAS = ALIAS()
SELECT POFHDR
LCOLDFILT = FILTER()
SET FILT TO
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	DO CASE
*!*	CASE lcPOType= 'P'
*!*	  lcTitle  = 'Material Purchase Orders'
*!*	CASE lcPOType= 'C'
*!*	  lcTitle  = 'Material Purchase Order Contracts'
*!*	CASE lcPOType= 'R'
*!*	  lcTitle  = 'Material Purchase Order Returns'
*!*	ENDCASE
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF

DO CASE
CASE LCPOTYPE= 'P'
  LCTITLE  = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALPO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALPO",LCARIAHFILE))
CASE LCPOTYPE= 'C'
  LCTITLE  = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALPOC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALPOC",LCARIAHFILE))
CASE LCPOTYPE= 'R'
  LCTITLE  = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALPOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALPOR",LCARIAHFILE))
ENDCASE
*N000682,1 MMT 12/03/2012 Globalization changes[END]
LCPOORDER = ORDER('POFHDR')
LCVENDOR  = IIF(TYPE('lcVendor')='C' .AND. !EMPTY(LCVENDOR),LCVENDOR,'')
LCPO      = IIF(TYPE('lcPo')='C',LCPO,SPACE(6))
SET ORDER TO TAG IIF(EMPTY(LCVENDOR),'POFHDR','POFHDRV') IN POFHDR
IF !SEEK(LCVENDOR+LCPOTYPE,'POFHDR')
  *N000682,1 MMT 12/03/2012 Globalization changes[START]
  *=MESSAGEBOX('No '+lcTitle+' found'+IIF(EMPTY(lcVendor),'',' for vendor '+lcVendor)+'.')
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALNO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALNO",LCARIAHFILE))+;
    LCTITLE+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALFOUND,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALFOUND",LCARIAHFILE))+;
    IIF(EMPTY(LCVENDOR),'',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MATERIALFORVENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MATERIALFORVENDOR",LCARIAHFILE))+;
    LCVENDOR)+'.')
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
  *=gfDialog ("I","No "+lcTitle+' found'+IIF(EMPTY(lcVendor),'',' for vendor '+lcVendor)+'.')
  LCVENDOR = SPACE(8)
  LCPO     = SPACE(6)
  SELECT POFHDR
  SET FILTER TO &LCOLDFILT
  SET ORDER TO TAG LCPOORDER IN POFHDR
  IF !EMPTY(LCALIAS)
    SELECT (LCALIAS)
  ENDIF

  RETURN(.F.)
ENDIF
DECLARE LABROW[2]
LABROW=' '
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcBrFields = "POMAT   :R :H='P/O #':12,"+;
*!*	  "Status  :R :H='S':4,"+;
*!*	  "Vendor  :R :H='Vendor':15,"+;
*!*	  "ApVendor.cVenComp :R :H='Name':22,"+;
*!*	  "Complete:R :H='Complete':10,"+;
*!*	  "NFABORDER  :R :H='Tot.Qty.':10,"+;
*!*	  "POTotal :R :H='Amount':15,"+;
*!*	  "NFBRECEIVE :R :H='Receive':10,"+;
*!*	  "NPO_OPEN   :R :H='Open':10"

LCBRFIELDS = "POMAT   :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_PO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_PO",LCARIAHFILE))+"#':12,"+;
  "Status  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STATUS_CR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_STATUS_CR",LCARIAHFILE))+"':4,"+;
  "Vendor  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_VENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_VENDOR",LCARIAHFILE))+"':15,"+;
  "ApVendor.cVenComp :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_NAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_NAME",LCARIAHFILE))+"':22,"+;
  "Complete:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_COMPLETE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_COMPLETE",LCARIAHFILE))+"':10,"+;
  "NFABORDER  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_TOTQTY,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_TOTQTY",LCARIAHFILE))+"':10,"+;
  "POTotal :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AMOUNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_AMOUNT",LCARIAHFILE))+"':15,"+;
  "NFBRECEIVE :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_RECEIVE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_RECEIVE",LCARIAHFILE))+"':10,"+;
  "NPO_OPEN   :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_OPEN,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_OPEN",LCARIAHFILE))+"':10"
*N000682,1 MMT 12/03/2012 Globalization changes[END]
LLOPNAPVEN = GFOPENFILE(OARIAAPPLICATION.DATADIR+'ApVendor',OARIAAPPLICATION.DATADIR+'VenCode','SH')
CLEAR TYPEAHEAD
SELECT POFHDR
SET RELATION TO VENDOR INTO APVENDOR
=SEEK(LCVENDOR+LCPOTYPE)

*llRetValue = gfBrows('lcVendor+lcPOType',"PoMat,Vendor","laBrow",lcTitle)
LLRETVALUE =ARIABROW([lcVendor+lcPOType],LCTITLE,0,0,0,0,'','',"PoMat,Vendor","laBrow",.T.,"POSHDR",.F.)

IF LLRETVALUE
  LCPO     = LABROW[1]
  LCVENDOR = LABROW[2]
ELSE
  LCPO     = SPACE(6)
  LCVENDOR = SPACE(8)
ENDIF
IF LLOPNAPVEN
  USE IN APVENDOR
ENDIF
SET FILTER TO &LCOLDFILT
SET ORDER TO TAG LCPOORDER IN POFHDR
IF !EMPTY(LCALIAS)
  SELECT (LCALIAS)
ENDIF
RETURN LLRETVALUE
*:---------------------------------------------------------------------
*! Name      : gfGetAddLbl
*! Developer : Moamed Abdel-Salam
*! Date      : 08/13/2003
*! Purpose   : Function to the address lables
*:---------------------------------------------------------------------
*: Calls              : oAriaApplication.remotesystemdata
*:---------------------------------------------------------------------
*: Passed Parameters  : pcCountry : Country Code
*:                      pcLables : Refernce to array for return value
*:---------------------------------------------------------------------
*: Example            : =gfGetAddLbl("USA",@laAddress)
*:---------------------------------------------------------------------
FUNCTION GFGETADDLBL
PARAMETERS PCCOUNTRY, PCLABLES
*-- If no parameter sent use the default country code
IF EMPTY(PCCOUNTRY)
  PCCOUNTRY = OARIAAPPLICATION.DEFAULTCOUNTRY
ENDIF
LCTMPCURSOR = GFTEMPNAME()
LCSELECT = "Select * from SYCINT where ccont_code='" + PCCOUNTRY +"'"
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE(LCSELECT,'',LCTMPCURSOR,"",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
DECLARE PCLABLES[6,2]
FOR LNCNT = 1 TO 6
  LCLBLFLD  = "CPARt"+ STR(LNCNT,1) + "LAB"
  LCWIDFLD = "nPARt"+ STR(LNCNT,1) + "LEN"
  PCLABLES[lnCnt,1] = &LCTMPCURSOR..&LCLBLFLD.
  PCLABLES[lnCnt,2] = &LCTMPCURSOR..&LCWIDFLD.
ENDFOR
USE IN (LCTMPCURSOR)


*!*************************************************************
*! Name      : gfScalBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 05/02/1995
*! Purpose   : Function to validate entered scale size value.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  Entered Scale
*!                       Multi scale
*!*************************************************************
*! Returns            :  .T. --> Valid scale
*!                       .F. --> Invalid scale
*!*************************************************************
*! Example            :  gfScalBrow(@m.Scale)
*!*************************************************************
FUNCTION GFSCALBROW
PARAMETER LCPARASCALE,LLMLTSCLE,LLBROWSE
PRIVATE LCBRFIELDS

LNOLDALIAS = SELECT()
*-- N119687,1 WSH 04/21/2004 Chnge the title of scale browse from "Scale" to "Size Scale"  [Start]
*-- Change the variable lcFile_Ttl to  "Size Scales" and read it from ARia.H file
*lcFile_Ttl = 'Scales'
*-- N119687,1 WSH 04/21/2004 Chnge the title of scale browse from "Scale" to "Size Scale"  [End]
PRIVATE LAVALUES
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
*--If Extended size scale was used.
IF LLMLTSCLE
  *--Browse from Scale Header file.
  LLOPNDSHD = GFOPENFILE(OARIAAPPLICATION.DATADIR+'scalehd','Extscale','SH')

  IF LLBROWSE OR EMPTY(LCPARASCALE) OR !SEEK(LCPARASCALE)
    DECLARE LAVALUES[1]  && array to get values from browse
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *!*	    lcBrFields = "cExtScale :H='Scale' ,"+;
    *!*	      "cScaleDes :H='Description',"+;
    *!*	      "cDim1Desc :H='Dimention1' ,"+;
    *!*	      "cDim2Desc :H='Dimention2' ,"+;
    *!*	      "cDim3Desc :H='Dimention3' ,"+;
    *!*	      "nNoOfDim  :H='No of Dimes' "
    LCBRFIELDS = "cExtScale :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALECOLUMN,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALECOLUMN",LCARIAHFILE))+"' ,"+;
      "cScaleDes :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALEDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALEDESC",LCARIAHFILE))+"',"+;
      "cDim1Desc :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DIM1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DIM1",LCARIAHFILE))+"' ,"+;
      "cDim2Desc :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DIM2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DIM2",LCARIAHFILE))+"' ,"+;
      "cDim3Desc :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DIM3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DIM3",LCARIAHFILE))+"' ,"+;
      "nNoOfDim  :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_NODIMS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_NODIMS",LCARIAHFILE))+"' "
    *N000682,1 11/20/2012 MMT Globlization changes[end]
    *=ARIABROW('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','cExtScale','laValues')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=ARIABROW('',LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','cExtScale','laValues')
    =ARIABROW('',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALE_TITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Scale_Title",LCARIAHFILE)),GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,'','','cExtScale','laValues')
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    LCPARASCALE = IIF(EMPTY(LAVALUES[1]),SPACE(1),LAVALUES[1])
  ENDIF

  IF LLOPNDSHD
    USE IN SCALEHD
  ENDIF

  *--NO extended scales.
ELSE
  SELECT SCALE
  IF LLBROWSE OR EMPTY(LCPARASCALE) OR !SEEK('S'+LCPARASCALE)
    DECLARE LAVALUES[1]  && array to get values from browse
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *!*	    lcBrFields = "Scale:5:H='Scale' ,"+;
    *!*	      "cScl_DESC :24:H='Description',"+;
    *!*	      "CNT  :H='Cnt'  ,"+;
    *!*	      "SZ1  :15:H='Size 1',"+;
    *!*	      "SZ2  :15:H='Size 2',"+;
    *!*	      "SZ3  :15:H='Size 3',"+;
    *!*	      "SZ4  :15:H='Size 4',"+;
    *!*	      "SZ5  :15:H='Size 5',"+;
    *!*	      "SZ6  :15:H='Size 6',"+;
    *!*	      "SZ7  :15:H='Size 7',"+;
    *!*	      "SZ8  :15:H='Size 8'"
    LCBRFIELDS = "Scale:5:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALECOLUMN,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALECOLUMN",LCARIAHFILE))+"' ,"+;
      "cScl_DESC :24:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALEDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALEDESC",LCARIAHFILE))+"',"+;
      "CNT  :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALECNT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALECNT",LCARIAHFILE))+"'  ,"+;
      "SZ1  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ1",LCARIAHFILE))+"',"+;
      "SZ2  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ2",LCARIAHFILE))+"',"+;
      "SZ3  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ3",LCARIAHFILE))+"',"+;
      "SZ4  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ4",LCARIAHFILE))+"',"+;
      "SZ5  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ5",LCARIAHFILE))+"',"+;
      "SZ6  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ6",LCARIAHFILE))+"',"+;
      "SZ7  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ7",LCARIAHFILE))+"',"+;
      "SZ8  :15:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALESZ8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_SCALESZ8",LCARIAHFILE))+"'"
    *N000682,1 11/20/2012 MMT Globlization changes[eND]
    *=ARIABROW(['S'],lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
    *B128783,1 KHM 07/12/2005 Do not display the "*" because its added by the conversion [Begin]
    *=ARIABROW(['S'],LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=ARIABROW(['S' FOR SCALE <> '*'],LANG_ARIA_Scale_Title,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
    =ARIABROW(['S' FOR SCALE <> '*'],IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_SCALE_TITLE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Scale_Title",LCARIAHFILE)),GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2,'','','SCALE','laValues')
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *B128783,1 KHM 07/12/2005 [End]
    LCPARASCALE = IIF(EMPTY(LAVALUES[1]),SPACE(1),LAVALUES[1])
  ENDIF
ENDIF

SELECT (LNOLDALIAS)
RETURN !EMPTY(LCPARASCALE)



*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Hesham El_Sheltawi
*! Date      : 08/14/2002
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_Lock(.T.)
*!*************************************************************
FUNCTION GFOBJ_LOCK
PARAMETERS LLOK_SET
PRIVATE LNRECNO,LRET_FLAG
LNWORKAREA = ALIAS()
IF EMPTY(LNWORKAREA)
  RETURN
ENDIF
PRIVATE LNOLDRPST
SELECT (LNWORKAREA)
LNDATASESSION = SET("DATASESSION")
LNALIAS = SELECT()
LRET_FLAG = .F.
LLOK_IT   = .F.
LLLOCKED  = .F.
*** Go to the same record to get a fresh copy in the buffer
LNRECNO = RECNO()

DO WHILE .T.
  SELECT (LNWORKAREA)
  IF LNRECNO <= RECCOUNT()
    GO LNRECNO
    LLLOCKED = RLOCK()
    UNLOCK RECORD LNRECNO
    IF LLLOCKED
      TABLEREVERT(.F.)
    ENDIF
    IF DELETED()
      =GFMODALGEN('INM00095B00000','ALERT')
      SELECT (LNALIAS)
      *THIS.CHangemode("S")
      RETURN .F.
    ENDIF
  ENDIF

  *** Chek if the record is in use by another user
  IF LLOK_SET
    *** Chek if the field cLok_User in the structur
    IF !LLOK_STAT .AND. LLLOCKED
      *** Record is not locked you may lock it
      LLOK_IT   = .T.
    ELSE
      LCLOK_USER = CLOK_USER
      IF !EMPTY(LCLOK_USER)
        IF ALLTRIM(LCLOK_USER) = ALLTRIM(OARIAAPPLICATION.USER_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          IF GFMODALGEN("INM00240B00006","ALERT")=2
            LLOK_IT    = .F.
            LRET_FLAG  = .F.
          ELSE
            LLOK_IT    = .T.
          ENDIF
        ELSE

          *We save old value of reprocess first.[START]
          LNOLDRPST = SET('REPROCESS')
          SET REPROCESS TO 1

          SET DATASESSION TO 1
          LLLOOP = .F.
          SELECT SYUSTATC
          IF SEEK ('INI'+'OLDVARS'+LCLOK_USER,'syuStatc')
            LOCAL LNSTATCREC
            SCAN REST WHILE COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID = 'INI'+'OLDVARS'+LCLOK_USER
              LNSTATCREC = RECNO()
              IF RLOCK('syuStatc')
                UNLOCK RECORD LNSTATCREC IN  SYUSTATC
                LLOK_IT    = .T.
                *!*	  	            lnStatcRec = RECNO()
                *!*	    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc
                *!*	      	        =RLOCK('syuStatc')
                *!*	        	      GO lnStatcRec
              ELSE
                UNLOCK
                GO (OARIAAPPLICATION.USERSTATICRECORD) IN SYUSTATC
                =RLOCK('syuStatc')
                *** Display the message "Record is in use by user AAAA"
                LCLOK_USER = OARIAAPPLICATION.GETUSERNAME(LCLOK_USER)
                *** Record is in use by user ????
                SET DATASESSION TO (LNDATASESSION)
                IF  GFMODALGEN("INM00028B00015","ALERT",LCLOK_USER) = 1
                  LLLOOP = .T.
                ENDIF
                LLOK_IT    = .F.
                LRET_FLAG  = .F.
                EXIT
              ENDIF
            ENDSCAN
          ELSE
            LLOK_IT    = .T.
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  LNOLDRPST
          SET DATASESSION TO (LNDATASESSION)
          IF LLLOOP
            LOOP
          ENDIF

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (LNDATASESSION)
        IF GFMODALGEN("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        LLOK_IT    = .F.
        LRET_FLAG  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"

      *** Unlock the record
      REPLACE LLOK_STAT WITH .F. , ;
        CLOK_USER WITH ""  , ;
        DLOK_DATE WITH {}  , ;
        CLOK_TIME WITH ""
      =TABLEUPDATE(0,.T.)
      LRET_FLAG  = .T.
    ENDIF
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (LNDATASESSION)
IF LLOK_IT
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for this user with date and time
    REPLACE LLOK_STAT WITH .T.       , ;
      CLOK_USER WITH OARIAAPPLICATION.USER_ID , ;
      DLOK_DATE WITH DATE()    , ;
      CLOK_TIME WITH GFGETTIME()
    =TABLEUPDATE(0,.T.)
    LRET_FLAG  = .T.
  ENDIF
ENDIF
SELECT (LNWORKAREA)
UNLOCK
SELECT (LNALIAS)

RETURN LRET_FLAG



*!**************************************************************************
*! PROG: FABROW.PRG
*! program to browse through the fabric file by using the new browse
*! written by Hesham El_Sheltawi 15/02/1995
*! NOTE: THIS PROGRAM ASSUMES THAT THE fabric FILE HAS BEEN OPENED
*!       IF WE WANT TO DISPLAY NO COLORS THEN XCLR = '*'
*!**************************************************************************
PROCEDURE FABROW

PARAMETERS XFAB,XCLR,LLRETALIAS,LCFABFOREX
PRIVATE LCBRFIELDS,LNCURALIAS,LCSTYLE,LADATA

DECLARE LADATA[2]  && array to get values from browse
STORE '' TO LADATA
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE) && variable to determine forcing browse or not
LCTITLE = "Item"+IIF('*' $ XCLR,'',' color')  && variable to hold Browse title
LLWASSEL=.T.
LCSTYLE=IIF(XCLR=CHR(240),XFAB,'')
LNCURALIAS = SELECT()

SELECT 0
USE (OARIAAPPLICATION.DATADIR+'FABRIC') AGAIN ALIAS FABRIC_A ORDER TAG FABRIC
SELECT FABRIC
LCFABORDER = TAG()
IF '*' $ XCLR
  *--'ITEM_TYPE'
  LCITMTYPE = GFCODDES(ITEM_TYPE,'ITEM_TYPE')
  LCBRFIELDS = [Fabric:14:h="Item",Color="******" :h="Color" :12,Desc:h="Description":35,]+;
    [lcItmType = gfCodDes(item_Type,'ITEM_TYPE') :h="Type",loc:14:h="Location",Vendor:16,]+;
    [Pattern:20,Onhand=lfSumFab(Fabric,'onhand'):14:h=]+;
    ["On Hand",OnOrder=lfSumFab(Fabric,'onorder'):14:h="On Order"]
ELSE
  LCBRFIELDS = [Fabric:14:h="Item",Fabric_A.Color:12,Fabric_A.Desc:h="Description":35,]+;
    [lcItmType = gfCodDes(Fabric_A.item_Type,'ITEM_TYPE'):h="Type",Fabric_A.loc:14:h="Location",Fabric_A.Vendor:16,]+;
    [Fabric_A.Pattern:20,Fabric_A.Onhand:14:h="On Hand",Fabric_A.OnOrder:14:h="On Order"]
ENDIF
SET ORDER TO TAG CFABRIC
SET RELATION TO FABRIC.FABRIC INTO FABRIC_A
IF !('*' $ XCLR)
  SET SKIP TO FABRIC_A
ENDIF
IF LLBROWSE OR !SEEK(XFAB+XCLR,'FABRIC_A')
  LNSOFTSEEK=RECNO(0)
  IF LNSOFTSEEK<>0 AND BETWEEN(LNSOFTSEEK,1,RECCOUNT())
    GO LNSOFTSEEK
  ELSE
    GO TOP
  ENDIF
  IF EMPTY(LCFABFOREX)
    LLWASSEL= ARIABROW([lcStyle],LCTITLE,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"","","FABRIC,FABRIC_A.COLOR","laData")
  ELSE
    SELECT FABRIC
    LOCATE &LCFABFOREX
    IF FOUND()
      LLWASSEL= ARIABROW(LCFABFOREX,LCTITLE,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"","","FABRIC,FABRIC_A.COLOR","laData")
    ELSE
      =GFMODALGEN('TRM00342B00000','DIALOG')
    ENDIF
  ENDIF
  IF LLWASSEL
    XFAB  = LADATA[1]
    XCLR  = LADATA[2]
    SET ORDER TO TAG FABRIC
    SEEK XFAB + XCLR
  ELSE
    XFAB  = SPACE(7)
    XCLR  = SPACE(6)
  ENDIF
ENDIF

IF !EMPTY(LCFABORDER)
  SET ORDER TO TAG (LCFABORDER)
ELSE
  SET ORDER TO
ENDIF
SET SKIP TO
SET RELATION TO
USE IN FABRIC_A
IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF
RETURN LLWASSEL

*!**************************************************************************
*! FUNCTION lfSumFab
*! FUNCTION to sum a specific field for the current style in style file
*! written by Hesham El_Sheltawi 15/02/1995
*!**************************************************************************
FUNCTION LFSUMFAB
PARAMETERS LCFAB,LCCOMP
LNTOTCOMP = 0
SELECT FABRIC_A
SUM &LCCOMP TO LNTOTCOMP WHILE FABRIC=LCFAB
SELECT FABRIC
GO RECNO()
RETURN INT(LNTOTCOMP)


*!*************************************************************
*! Name : gfAPVnBrow.
*! Auth : Hisham Ramsis Philips (HISH).
*! Date : 13/03/95.
*!*************************************************************
*! Synopsis : Browse vendors from the AP files if system is seted to
*!            use AP link.
*!*************************************************************
*! Passed :
*!        Parameters : Vendor code.
*!*************************************************************
*! Returned :
*!        Variables  : True if the user select or false if the user
*!                     ESC. from the browse.
*!*************************************************************
*! Example :
*!        =gfAPVnBrow( @lcVenCode )
*!*************************************************************
*! Screen layout :
*!    ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*!03  ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*!04  º VENDOR    .............NAME.............  .....PHONE......  OUR ACCOUNT     º
*!05  ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*!06  º ±±±±±±±±  ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±  ±±±±±±±±±±±±±±±±  ±±±±±±±±±±±±±±± º
*!..  º 12345678  123456789012345678901234567890  1234567890123456  123456789012345 º
*!22  ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
*!*************************************************************
FUNCTION GFAPVNBROW

PARAMETERS LCVENCODE,LLRETALIAS , LCSUBTYPE
PRIVATE LCBRFIELDS,LCALIAS,LADATA , LCFOREXPR

DECLARE LADATA[1] && array to get values from browse
LADATA=' '
LLOPNAPVEN=.F.
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE) && variable to determine forcing browse or not
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	lcTitle  = 'A/P Vendors'

*!*	gcPhnFrmt  =gfPhoneTem()
*!*	lcBrFields = "cVendCode :R :H='Vendor':20,"+;
*!*	  "cVenComp  :R :H='Name':45,"+;
*!*	  "cPhoneNo  :R :P= gcPhnFrmt  :H='Phone':28,"+;
*!*	  "cVenOurAc :R :H='Our Account':28"
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
LCTITLE  = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_APVENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_APVENDOR",LCARIAHFILE))
GCPHNFRMT  =GFPHONETEM()
LCBRFIELDS = "cVendCode :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_VENDOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_VENDOR",LCARIAHFILE))+"':20,"+;
  "cVenComp  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_VEND_NAME,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_VEND_NAME",LCARIAHFILE))+"':45,"+;
  "cPhoneNo  :R :P= gcPhnFrmt  :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_APPHONE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_APPHONE",LCARIAHFILE))+"':28,"+;
  "cVenOurAc :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_OURACC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_OURACC",LCARIAHFILE))+"':28"
*N000682,1 MMT 12/03/2012 Globalization changes[END]
LCALIAS = ALIAS()   && Save the current alias.

IF !USED( 'APVendor' )
  SELECT 0
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'ApVendor','VenCode','SH')
  LLOPNAPVEN=.T.
ELSE
  LLOPNAPVEN=.F.
ENDIF
SELECT APVENDOR
GO TOP
IF EOF()
  *N000682,1 MMT 12/03/2012 Globalization changes[START]
  *= MESSAGEBOX('Vendor file is empty',16,_SCREEN.CAPTION)
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_EMPTYVENDORFILE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_EMPTYVENDORFILE",LCARIAHFILE)))
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
  LCVENCODE  = SPACE(08)
  IF LLOPNAPVEN
    USE
  ENDIF
  IF LLRETALIAS
    SELECT (LCALIAS)
  ENDIF
  RETURN
ELSE
  IF !SEEK (LCVENCODE)
    IF RECNO(0)=0
      GO TOP
    ELSE
      GO RECNO(0)
    ENDIF
  ENDIF
ENDIF
IF TYPE('lcSubType') <> 'C'
  LCSUBTYPE = ''
ENDIF
STORE '' TO LCFOREXPR
IF !EMPTY(LCSUBTYPE)
  FOR LNFORLOOP = 1 TO LEN(ALLTRIM(LCSUBTYPE))
    IF LNFORLOOP > 1
      LCFOREXPR = LCFOREXPR + ' OR '
    ENDIF
    LCFOREXPR = LCFOREXPR + "'" + SUBSTR(ALLTRIM(LCSUBTYPE),LNFORLOOP,1) +;
      "'" + ' $ ' + 'cVenSupTyp'
  ENDFOR
ENDIF
IF EMPTY(LCFOREXPR)
  LLWASSEL  =ARIABROW('',LCTITLE,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',"cVendCode","laData")
ELSE
  LLWASSEL  =ARIABROW([FOR &lcForExpr],LCTITLE,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,'','',"cVendCode","laData")
ENDIF

LCVENCODE =IIF(LLWASSEL,LADATA[1],SPACE(8))
IF  LLOPNAPVEN
  USE
ENDIF
IF LLRETALIAS
  SELECT (LCALIAS)
ENDIF
RETURN LLWASSEL
*-- EOF( gfAPVnBrow )


*!*************************************************************
*! Name : GETOBJ
*! Auth : Hesham El_Sheltawi
*! Date : 27/08/2002.
*!*************************************************************
*! Synopsis : call the object link screen
*!*!*************************************************************
*! Passed :
*!        Parameters : Object Type, Object ID
*!*************************************************************
*! Returned :
*!        Variables  :
*!*************************************************************
*! Example :
*!        =GetObj( "S","WOMCL-RED" )
*!*************************************************************
FUNCTION GFGETOBJ
PARAMETERS LCOBJTYPE,LCOBJECTID


*B611566,1 SAH Modify function GFGETOBJ to add lines of code  to check if the type of '_SCREEN.ACTIVEFORM.PARENT' is Object it is not Null [begin]
IF TYPE('_SCREEN.ACTIVEFORM.PARENT')<>'O' OR ISNULL(_SCREEN.ACTIVEFORM.PARENT)
  RETURN 
ENDIF 
*B611566,1 SAH Modify function GFGETOBJ to add lines of code  to check if the type of '_SCREEN.ACTIVEFORM.PARENT' is Object it is not Null [end]



* MAH OBJECT LINK START
*DO FORM (oAriaApplication.ScreenHome+"sy\OBJECTS") WITH lcObjType,lcObjectID
LOCAL LOOBJECT
LOOBJECT = CREATEOBJECT("AriaObjectBrowser.Loader")
LOOBJECT.LOADFORM(OARIAAPPLICATION, _SCREEN.ACTIVEFORM.PARENT, CREATEOBJECT('InterOperability'), OARIAAPPLICATION.CARIANATIVEDATAFILESCONSTR, LCOBJTYPE, LCOBJECTID)
* MAH End



RETURN
*!*************************************************************
*:************************************************************************
*: Program file  : GFBROWSE.PRG
*: Program desc. :
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system
*:      Developer:
*:************************************************************************
*: Calls :
*:         Procedures :
*:         Functions  :
*:************************************************************************
*: Passed Parameters  :
*:************************************************************************
*B600383,1 Hesham On 06/07/95
*B600383,1 Clear the key field if browse and there is no records in
*B600383,1 the file.
*E600823,1 Hesham El-Sheltawi 05/16/96
*E600823,1 Create prefrence for each browse for any file according
*E600823,1 to the name of the function that is browsing the file
*E600823,1 and the file name
*B601183,1 Hesham El-Sheltawi On 07/29/96
*B601183,1 Change the position of the trapping for then enter and
*B601183,1 escape after the checking for the emptines of the file
*B800703,1 Hesham 08/21/96
*B800703,1 the for condition is clearing after the Enhancment
*B800703,1 made for the incrementel search
*B601437,1 make the browse for command browse rest for
*B601456,1 Hesham El-Sheltawi 12/05/96
*B601456,1 Shut down the trapping of the enter and esc buttons
*B601660,1 Hesham El-Sheltawi 12/03/97
*B601660,1 Make the Global Browse Use one read Level
*B601660,1 Change the validation of the push button in the global browse
*B601660,1 window that branch to another screen to Terminate the read
*B601660,1 of the browse
*! E038142,2 MAH 09/16/2004 Full support for run forms with SQL with high Performance.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*:************************************************************************
FUNCTION GFBROWS
*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- PARAMETER lcFltrExp,lcFieldsNam,lcArrName,lcBrowTitle,llIsFilter,lcAliasName,llGetTree
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- PARAMETER lcFltrExp, lcFieldsNam, lcArrName, lcBrowTitle, llIsFilter, lcAliasName, llGetTree, ;
*--           lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey
PARAMETER LCFLTREXP, LCFIELDSNAM, LCARRNAME, LCBROWTITLE, LLISFILTER, LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCSEEKTOKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 [END]

*N000682,1 12/25/12 TMI Globlization changes[Start] update the lcBrowTitle using lcFile_Ttl if the former came empty
IF TYPE('lcFile_Ttl')='C' AND LEN(ALLT(LCFILE_TTL)) > 0
  LCBROWTITLE = IIF(EMPTY(LCBROWTITLE),LCFILE_TTL,LCBROWTITLE)
ENDIF
*N000682,1 12/25/12 TMI Globlization changes[End  ]


*! E038142,2 MAH 09/16/2004 Add addtional parameters [BEGIN]
*-- RETURN ARIABROW(lcFltrExp,lcBrowTitle,.F.,.F.,.F.,.F.,.F.,.F.,lcFieldsNam,lcArrName,.F.,lcAliasName,llGetTree)
*! B038623,1 MAH 10/12/2004 Add Addtional Parmaeters [BEGIN]
*-- RETURN ARIABROW(lcFltrExp, lcBrowTitle, .F., .F., .F., .F., .F., .F., ;
*--                 lcFieldsNam, lcArrName, .F., lcAliasName, llGetTree, ;
*--                 lcBrowseFileName, lcBrowseTableDBEngine, lcBrowsePKIndexName, lcBrowseIndexName, lcSeekToKey)
RETURN ARIABROW(LCFLTREXP, LCBROWTITLE, .F., .F., .F., .F., .F., .F., ;
  LCFIELDSNAM, LCARRNAME, .F., LCALIASNAME, LLGETTREE, ;
  LCBROWSEFILENAME, LCBROWSETABLEDBENGINE, LCBROWSEPKINDEXNAME, LCBROWSEINDEXNAME, LCSEEKTOKEY, ;
  LCPREFERENCEKEY, LCPREFERENCESUBKEY)
*! B038623,1 MAH 10/12/2004 [END]
*! E038142,2 MAH 09/16/2004 [END]

FUNCTION GFEXPONOF
LNBAR = BAR()
LCPOP = POPUP()
OARIAAPPLICATION.DISPLAYEXPLORER = !OARIAAPPLICATION.DISPLAYEXPLORER
SET MARK OF BAR LNBAR OF (LCPOP) OARIAAPPLICATION.DISPLAYEXPLORER


FUNCTION GFEXPTOP
LNBAR = BAR()
LCPOP = POPUP()
OARIAAPPLICATION.EXPLORERONTOP = !OARIAAPPLICATION.EXPLORERONTOP
SET MARK OF BAR LNBAR OF (LCPOP) OARIAAPPLICATION.EXPLORERONTOP


*!*************************************************************
*! Name      : gfADel
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : To delete a colum or row from array
*!*************************************************************
*! Calls     :
*!      Called by: GFUPDATSYS()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : array name
*!                      number of colum or row to be deleted
*!                      1 -> row  / 2 -> colum
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFADEL
PARAMETERS LADEL,LNDELWCH,LNSUBSCRP

*B601481,1 This line was added by HS [Begin]
PRIVATE LATEMP
*B601481,1 This line was added by HS [End]

LNSUBSCRP=IIF(TYPE("lnSubScrp") $ "UL",1,LNSUBSCRP)
DO CASE
CASE LNSUBSCRP=1
  =ADEL(LADEL,LNDELWCH)
  IF ALEN(LADEL,2)<>0
    IF ALEN(LADEL,1)>1
      DIMENSION LADEL[ALEN(laDel,1)-1,ALEN(laDel,2)]
    ENDIF
  ELSE
    IF ALEN(LADEL,1)>1
      DIMENSION LADEL[ALEN(laDel,1)-1,1]
    ENDIF
  ENDIF
  RETURN .T.
CASE LNSUBSCRP=2
  *B610447,1 TMI 07/24/2013 [Start] fix problems in maintain dictionary screen
  *DIMENSION laTemp[ALEN(laDel,1),ALEN(laDel,2)-1]
  DIMENSION LATEMP[ALEN(laDel,1),max(ALEN(laDel,2)-1,1)]
  *B610447,1 TMI 07/24/2013 [End  ]
  FOR I=0 TO ALEN(LADEL,1)-1
    IF LNDELWCH>1 .AND. LNDELWCH<ALEN(LADEL,2)
      =ACOPY(LADEL,LATEMP,I*ALEN(LADEL,2)+1,LNDELWCH-1,I*ALEN(LATEMP,2)+1)
      =ACOPY(LADEL,LATEMP,I*ALEN(LADEL,2)+LNDELWCH+1,ALEN(LADEL,2)-LNDELWCH,;
        I*ALEN(LATEMP,2)+LNDELWCH)
    ELSE
      IF LNDELWCH=1
        =ACOPY(LADEL,LATEMP,I*ALEN(LADEL,2)+2,ALEN(LADEL,2)-1,I*ALEN(LATEMP,2)+1)
      ELSE
        =ACOPY(LADEL,LATEMP,I*ALEN(LADEL,2)+1,ALEN(LADEL,2)-1,I*ALEN(LATEMP,2)+1)
      ENDIF
    ENDIF
  ENDFOR
  DIMENSION LADEL[ALEN(laTemp,1),ALEN(laTemp,2)]
  RETURN (ACOPY(LATEMP,LADEL)>0)
ENDCASE


*************************************************************************
*FUNCTION FErrInfo
*DESC: Function get error information, which depends on transaction type.
*NOTE: Called from Function Checkprd
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed
*PARA: lcType   : Transaction type.
*    : lcDKind  : Date kind
*    : lcErrMes1: Message line #1
*    : lcErrMes2: Message line #2
*MODI:
*E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
*E100219,9              from operation
*N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
*E300324,6 RENEE 12/28/95 function fErrInfo() : Change text of type 'IN'
*E300324,6                AND 'V1' from 'System date' to 'Invoice date',
*E300324,6                and remove the checking message.
*************************************************************************
FUNCTION FERRINFO
PARAMETERS LCTYPE,LCDKIND,LCERRMES1,LCERRMES2
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
DO CASE
CASE LCTYPE = 'IN'                 && Invoice
  *E300324,6 Change message text.
  *&lcDKind   = 'System date '
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'Invoice date '
  *!*	  *E300324,6 end.
  *!*	  &lcErrMes1 = 'Not allowed to create invoices for this date. '
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG1,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG1",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG2,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG2",LCARIAHFILE))+' '
  *N000682,1 MMT 12/09/2012 Globalization changes[End]
  *E300324,6 Remove checking message text.
  *&lcErrMes2 = 'Please check the system date.'
  &LCERRMES2 = ''
  *E300324,6 end.

CASE LCTYPE = 'VI1'                 && Void invoice 1st.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to void invoices for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG4,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG4",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]

CASE LCTYPE = 'VI2'                 && Void Invoice 2nd.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'Invoice date '
  *!*	  &lcErrMes1 = 'Not allowed to void invoices from prior periods.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG1,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG1",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG6,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG6",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[End]
  &LCERRMES2 = ''

CASE LCTYPE = 'IA'                 && Inventory Adjustment
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'Transaction date '
  *!*	  &lcErrMes1 = 'Not allowed to enter inventory adjustment for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG7,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG7",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG8,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG8",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'IP'                 && Inventory Physical
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'Transaction date '
  *!*	  &lcErrMes1 = 'Not allowed to enter physical inventory for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG7,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG7",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG9,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG9",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'ZE'                 && Zero out Stock
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to zero out stock for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG12,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG12",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
CASE LCTYPE = 'PO'                 && Receive P/Os
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to enter P/O receivings for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG13,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG13",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
CASE LCTYPE = 'CT'                 && Receive C/Ts
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to enter C/T receivings for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG14,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG14",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
CASE LCTYPE = 'RM'                 && Return merchandise
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to receive returns for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG15,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG15",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[End]
CASE LCTYPE = 'VR1'                 &&  Void Return 1st
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'System date '
  *!*	  &lcErrMes1 = 'Not allowed to void credit memo for this date. '
  *!*	  &lcErrMes2 = 'Please check the system date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG3,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG3",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG16,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG16",LCARIAHFILE))+' '
  &LCERRMES2 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG5,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG5",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
CASE LCTYPE = 'VR2'                 &&  Void Return 2nd
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Return date '
  *!*	  &lcErrMes1 = 'Not allowed to void credit memo from prior periods.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG10,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG10",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG11,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG11",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''
CASE LCTYPE = 'CR'                 &&  Customer Payment
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Batch date '
  *!*	  &lcErrMes1 = 'Not allowed to enter payments for this batch date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG17,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG17",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG18,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG18",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'AJ'                 &&  Adjustment
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Batch date '
  *!*	  &lcErrMes1 = 'Not allowed to enter adjustments for this batch date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG17,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG17",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG19,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG19",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'KO'                 &&  Key Off
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Key off date '
  *!*	  &lcErrMes1 = 'Not allowed to make key off for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG20,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG20",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG21,OARIAAPPLICATION.GETHEADERTEXT("LANG_FELANG_FERRINF_MSG21RRINF_MSG1",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[end]
  &LCERRMES2 = ''

CASE LCTYPE = 'RO'                 &&  Receive from material operation
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Material operation receiving date '
  *!*	  &lcErrMes1 = 'Not allowed to receive from material operation for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG22,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG22",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG23,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG23",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'RS'                 &&  Receive from style operation
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'Style operation receiving date '
  *!*	  &lcErrMes1 = 'Not allowed to receive from style operation for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG24,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG24",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG25,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG25",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''

CASE LCTYPE = 'MM'                 &&  Receive M.F.G. order
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *!*	  &lcDKind   = 'M.F.G. order receiving date '
  *!*	  &lcErrMes1 = 'Not allowed to receive M.F.G. order for this date.'
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG26,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG26",LCARIAHFILE))+' '
  &LCERRMES1 = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG27,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG27",LCARIAHFILE))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES2 = ''
OTHERWISE
  *N000682,1 MMT 12/09/2012 Globalization changes[START]
  *&lcDKind   = 'Transaction date '
  &LCDKIND   = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FERRINF_MSG7,OARIAAPPLICATION.GETHEADERTEXT("LANG_FERRINF_MSG7",LCARIAHFILE))+' '
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  &LCERRMES1 = ''
  &LCERRMES2 = ''
ENDCASE

RETURN(.T.)

FUNCTION LFTRNSSTR
PARAMETERS LCVALUESTR,LCDATATYPE,LCDIRECTION
DO CASE
CASE LCDATATYPE $ 'CM'
  RETURN ALLT(LCVALUESTR)
CASE LCDATATYPE = 'N'
  RETURN VAL(LCVALUESTR)
CASE LCDATATYPE='D'
  RETURN CTOD(LCVALUESTR)
CASE LCDATATYPE = 'L'
  RETURN IIF(UPPER(ALLTRIM(LCVALUESTR))='.F.',.F.,.T.)
ENDCASE


*!*************************************************************
*! Name      : gfToglErr
*! Developer : Hesham El-Sheltawi
*! Date      : 11/25/1998
*! Purpose   : function to togle error handler enabling
*!*************************************************************
*! Called from : Main Menu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : gfToglErr()
*!*************************************************************
*E300797,1 Hesham (Start)
*E300797,1 function to enable and disable error handler
FUNCTION GFTOGLERR
GLERRORHAN = !GLERRORHAN
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

IF GLERRORHAN
  *!*	    ON ERROR DO gfEHan WITH ERROR(), MESSAGE(), MESSAGE(1), ;
  *!*	        SYS(16), LINENO(), SYS(102), SYS(100), SYS(101), LASTKEY(), ;
  *!*	        ALIAS(), SYS(18), SYS(5), SYS(12), SYS(6), SYS(2003), WONTOP(), ;
  *!*	        SYS(2011), SYS(2018), SET("CURSOR")
  OARIAAPPLICATION.SETERROR()
ELSE
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
  *WAIT WINDOW "Clearing Error Handler" NOWAIT
  WAIT WINDOW IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CLEARERRORHANDLER,OARIAAPPLICATION.GETHEADERTEXT("LANG_CLEARERRORHANDLER",LCARIAHFILE)) NOWAIT
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
  ON ERROR
ENDIF
IF !EMPTY(BAR()) AND !EMPTY(POPUP())
  SET MARK OF BAR BAR() OF (POPUP()) GLERRORHAN
ENDIF
*E300797,1 Hesham (End)











*-- Added by Badran in 10/13/2002 ... BEGIN
*:************************************************************************
*: Program file  : GFGENFLT.PRG
*: Program desc. :
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system
*:      Developer:
*:************************************************************************
*: Calls :
*:         Procedures :
*:         Functions  :
*:************************************************************************
*: Passed Parameters  :
*!************************************************************************
*! Modifications :
*! E038142, 2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E037237,2 MAH 10/01/2004 Enhance the interface of color scheme.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!************************************************************************
*:
FUNCTION GFGENFLT
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- * N038038,1 SMM Add a parameter laFilterArray that will hold the expressions[Start]
*-- *--	PARAMETERS lcArray , llFilter , llUseArray
*-- PARAMETERS lcArray , llFilter , llUseArray ,laFilterArray
*-- * N038038,1 SMM [End]

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS LCARRAY, LLFILTER, LLUSEARRAY, LAFILTERARRAY, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)

*! E038142,2 MAH 08/31/2004 [END]

LLUSEARRAY = IIF(TYPE('llUseArray') <> 'L' , .F. , LLUSEARRAY)

* N038038,1 SMM To check if the parameter was send or not[Start]
LLUSEFILTERARRAY = IIF(TYPE('laFilterArray') <> 'C', .F. , .T.)
* N038038,1 SMM [End]

LCQUERY=''
LCELMSEP='|'
LCVALSEP='~'

LCLINEFEED=' ' &&+CHR(10)+CHR(13)
LNFLTSTART=1
DO WHILE (&LCARRAY[lnFltStart,1]='.OR.' OR EMPTY(&LCARRAY[lnFltStart,1]));
    AND !LNFLTSTART=ALEN(&LCARRAY,1)
  LNFLTSTART=LNFLTSTART+1
ENDDO
LNFLTEND=ALEN(&LCARRAY,1)
DO WHILE (&LCARRAY[lnFltEnd,1]='.OR.' OR EMPTY(&LCARRAY[lnFltEnd,1]));
    AND LNFLTEND>1
  LNFLTEND=LNFLTEND-1
ENDDO

LNOR=0
IF LNFLTSTART>ALEN(&LCARRAY,1)
  RETURN ''
ENDIF
IF LNFLTEND=ALEN(&LCARRAY,1)
  LCWHICHELM=LCARRAY+'['+ALLTRIM(STR(ALEN(&LCARRAY,1)))+',1]'
  IF TYPE(LCWHICHELM)<>'C'
    RETURN ''
  ENDIF
  IF &LCARRAY[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
LNCOUNT=LNFLTSTART

DO WHILE  LNCOUNT<=LNFLTEND
  *! E037237,2 MAH 10/01/2004 Remove the VAL becasue it condseder 0 is empty [BEGIN]
  *-- IF &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6])) ;
  *--     AND &lcArray[lnCount,7]='V'
  *--   lnCount=lnCount+1
  *--   LOOP
  *-- ENDIF
  IF &LCARRAY[lnCount,3]='N' AND EMPTY(&LCARRAY[lnCount,6]) ;
      AND &LCARRAY[lnCount,7]='V'
    LNCOUNT=LNCOUNT+1
    LOOP
  ENDIF
  *! E037237,2 MAH 10/01/2004 [END]

  IF IIF(TYPE(ALLTRIM(LCARRAY) + "[lnCount,6]") = 'C' ,;
      !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount,6] , LCELMSEP ,;
      '') , IIF(&LCARRAY[lnCount,3] = 'D' ,'/' , '') , ''))) ,;
      !EMPTY(&LCARRAY[lnCount,6])) .OR. &LCARRAY[lnCount,1] = '.OR.'

    IF !EMPTY(&LCARRAY[lnCount,1])
      IF &LCARRAY[lnCount,1]<>'.OR.'
        *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
        *-- lcQuery=lcQuery+lfGetQCond(lnCount,lcArray,llFilter)
        *! E038650,1 MAH 10/27/2004 apply new parameters remove case sensitive and return english filter string [BEGIN]
        *-- lcQuery = lcQuery + lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine)
        LCQUERY = LCQUERY + LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY)
        *! E038650,1 MAH 10/27/2004 [END]
        *! E038142,2 MAH 08/31/2004 [END]

        * N038038,1 SMM Filling laFilterArray with the filters one filter on the record[Start]
        IF LLUSEFILTERARRAY && If laFilterArray parameter was passed
          *-- Fill the laFilterArray column 2 with the filter expression after removing | .AND. |
          *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
          *-- laFilterArray[lnCount,2] = SUBSTR(lfGetQCond(lnCount,lcArray,llFilter),1,LEN(lfGetQCond(lnCount,lcArray,llFilter))-9)
          LAFILTERARRAY[lnCount, 2] = SUBSTR(LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE), ;
            1, ;
            LEN(LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE)) - 9)
          *! E038142,2 MAH 08/31/2004 [END]

          IF AT("SUBSTR",&LCARRAY[lnCount,1])>0
            LNTABLEFIRST = AT("(",&LCARRAY[lnCount,1])+1 && get the position of "("
            LNTABLEEND = AT(".",&LCARRAY[lnCount,1]) && get the position of "."
          ELSE
            LNTABLEFIRST = 1
            LNTABLEEND = AT(".",&LCARRAY[lnCount,1]) && get the position of "."
          ENDIF
          *-- Fill the laFilterArray column 1 with the Table Name
          LAFILTERARRAY[lnCount,1] = SUBSTR(&LCARRAY[lnCount,1],LNTABLEFIRST,LNTABLEEND-LNTABLEFIRST)
        ENDIF
        * N038038,1 SMM [END]
      ELSE
        LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
        IF LNOR>0
          LCQUERY=LCQUERY+' ) '
          LNOR=0
        ENDIF

        DO WHILE LNCOUNT<LNFLTEND-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,'');
            ,IIF(&LCARRAY[lnCount+1,3]='D','/  /',''),''))) OR &LCARRAY[lnCount+1,1]='.OR.')
          LNCOUNT=LNCOUNT+1
        ENDDO
        IF !EMPTY(ALLTRIM(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,''))) AND !EMPTY(ALLTRIM(LCQUERY))
          LCQUERY=ALLTRIM(LCQUERY)+' '+LCELMSEP+' OR '+LCELMSEP+'( '
          LNOR=1
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  LNCOUNT=LNCOUNT+1
ENDDO
LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
IF LNOR>0
  LCQUERY=LCQUERY+' ) '
ENDIF
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' .AND. '+LCELMSEP,LCLINEFEED+' AND '+LCLINEFEED)
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' OR '+LCELMSEP,LCLINEFEED+' OR '+LCLINEFEED)
RETURN LCQUERY
*-- end of gfGenFlt.

*!*****************************************************************************************
*! Name      : lfGetQCond
*! Developer :
*! Date      : 10/13/2002 10:40:09 AM
*! Purpose   : Get Filter condition
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!************************************************************************
*! Modifications :
*! E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! B999999,1 MAH 03/01/2005 Fix apply filter problem.
*!************************************************************************
FUNCTION LFGETQCOND
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS lnCount,lcArray,llFilter

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)
*! E038142,2 MAH 08/31/2004 [END]

*! E038142,2 MAH 08/31/2004 Convert left side according DB engine [BEGIN]
LOCAL LCLEFTSIDE
*! E038650,1 MAH 01/18/2005 [BEGIN]
*-- lcLeftSide = lfGetConditionLeftSide(&lcArray[lnCount, 1], lcDBEngine)
LCLEFTSIDE = LFGETCONDITIONLEFTSIDE(&LCARRAY[lnCount, 1], LCDBENGINE)
LCLEFTSIDE = LCLEFTSIDE + ' '
*! E038650,1 MAH 01/18/2005 [END]
*! E038142,2 MAH 08/31/2004 [END]

LCFILTEXP=''
DO CASE
CASE &LCARRAY[lnCount,5] = 'Contains'
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--           &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
    *--           ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
    *--           IIF(&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep
    IF LLENGLISHQUERY
      LCFILTEXP = ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
        IIF(&LCARRAY.[lnCount, 4], ' contains ', ' not contains ') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        .T.) + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ELSE
      LCFILTEXP = IIF(&LCARRAY.[lnCount, 4], '',  '!(') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        LLENGLISHQUERY) + ;
        ' $ ' + ;
        IIF(LLCASEINSENSITIVE, 'RTRIM(UPPER(', '') + ;
        ALLTRIM(&LCARRAY.[lnCount, 1]) + ' ' + ;
        IIF(LLCASEINSENSITIVE, '))', '') + ;
        IIF(&LCARRAY.[lnCount, 4], '', ' )') + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ENDIF
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    *! E038650,1 MAH 01/18/2005 [BEGIN]
    *-- lcFiltExp = lcLeftSide + ;
    IIF(&lcArray[lnCount, 4], "LIKE '" , "NOT LIKE '%") + ;
    lfRightGet(&lcArray[lnCount, 6], ;
    &lcArray[lnCount, 3], ;
    &lcArray[lnCount, 5], ;
    lcElmSep, ;
    &lcArray[lnCount, 7], ;
    lcDBEngine) + '% ' + ;
    lcElmSep + ' .AND. ' + lcElmSep
    LOCAL LCRIGHT
    LCRIGHT = LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE)
    LCRIGHT = ALLTRIM(LCRIGHT)
    LCRIGHT = SUBSTR(LCRIGHT, 2, LEN(LCRIGHT) - 2)
    *! B999999,1 MAH 03/01/2005 [BEGIN]
    *-- IF ALLTRIM(lcArray[lnCount, 7]) == 'F'
    IF ALLTRIM(&LCARRAY[lnCount, 7]) == 'F'
      *! B999999,1 MAH 03/01/2005 [END]
      LCRIGHT = SUBSTR(LCRIGHT, 2, LEN(LCRIGHT) - 2)
    ENDIF
    LCFILTEXP = LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 4], "LIKE '%" , "NOT LIKE '%") + ;
      LCRIGHT + "%' " + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 01/18/2005 [END]
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE &LCARRAY[lnCount,5] = 'Like' OR &LCARRAY[lnCount,5] = 'Exactly Like'
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(&lcArray[lnCount,4],;
    *--            IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep
    LCFILTEXP = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', IIF(&LCARRAY.[lnCount, 4], '', '!('), '') + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '')) + ;
      IIF(LLENGLISHQUERY, ;
      IIF(&LCARRAY.[lnCount, 4], ' like ', ' not like '), ;
      IIF(LLCASEINSENSITIVE  .AND. &LCARRAY.[lnCount, 3] $ 'CM', ;
      ' == ', ;
      IIF(&LCARRAY.[lnCount, 4], ;
      IIF(&LCARRAY.[lnCount, 5] = 'Like', ' = ', ' == '), ;
      ' <> '))) + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', IIF(&LCARRAY.[lnCount, 4], '', ')'), '') + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCFILTEXP = IIF(&LCARRAY[lnCount, 3] = 'D', 'CONVERT(CHAR(8), ', ' ') + ;
      LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
      IIF(&LCARRAY[lnCount, 4], '= ', '<> ') + ;
      LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE) + ' ' + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE INLIST(&LCARRAY[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
    'Less Or Equal')
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),&lcArray[lnCount,4])
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
    *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(&lcArray[lnCount,4],'',' ) ')+;
    *--          lcElmSep+' .AND. '+lcElmSep
    LCOPERATOR = LFGETOPER(ALLTRIM(&LCARRAY.[lnCount, 5]), &LCARRAY[lnCount, 4], LLENGLISHQUERY)
    LCFILTEXP  = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', '!(')) + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D',')', '')) + ;
      ' ' + LCOPERATOR + ' ' + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', ' )')) + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCOPERATOR = LFGETOPER(ALLTRIM(&LCARRAY[lnCount, 5]), &LCARRAY[lnCount, 4])
    LCFILTEXP  = IIF(&LCARRAY[lnCount, 4], ' ' , 'NOT (') + ;
      IIF(&LCARRAY[lnCount, 3]='D', 'CONVERT(CHAR(8), ' ,' ') + ;
      LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
      LCOPERATOR + ' ' + ;
      LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE) + ;
      IIF(&LCARRAY[lnCount, 4], ' ', ') ') + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE &LCARRAY[lnCount,5] = 'Between'
  IF LLFILTER
    *! E038650,1 MAH 10/27/2004 Apply case Insensitive if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+;
    *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--             +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
    *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
    *--             ')'+lcElmSep+' .AND. '+lcElmSep
    LCFILTEXP = IIF(&LCARRAY.[lnCount, 4], 'BETWEEN(', '!BETWEEN(') + ;
      IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '') + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '') + ',' + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      ')' + LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]
  ELSE
    *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
    DO CASE
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
      *! E038142,2 MAH 08/31/2004 [END]

      *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
      *-- lcFiltExp = IIF(&lcArray[lnCount,4],'','!(')+;
      *--             IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
      *--             +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+;
      *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
      *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
      *--             IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep
      LCFILTEXP = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', '!(')) + ;
        IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
        IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
        ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
        IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
        IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '')) + ;
        IIF(LLENGLISHQUERY, ;
        IIF(&LCARRAY.[lnCount, 4], ' between ', ' not between '), ;
        ' BETWEEN ') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        LLENGLISHQUERY) + ;
        IIF(LLENGLISHQUERY,  '', IIF(&LCARRAY.[lnCount, 4], '', ')')) + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
      *! E038650,1 MAH 10/27/2004 [END]

      *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
      LCFILTEXP = IIF(&LCARRAY[lnCount, 4], ' ' , 'NOT ( ') + ;
        IIF(&LCARRAY[lnCount, 3]='D', 'CONVERT(CHAR(8), ', ' ') + ;
        LCLEFTSIDE + ;
        IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
        ' BETWEEN '+ ;
        LFRIGHTGET(&LCARRAY[lnCount, 6], ;
        &LCARRAY[lnCount, 3], ;
        &LCARRAY[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY[lnCount, 7], ;
        LCDBENGINE) + ;
        IIF(&LCARRAY[lnCount, 4], ' ', ') ') + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
  ENDIF
CASE &LCARRAY[lnCount,5] = 'In List'
  IF &LCARRAY[lnCount,7] <> 'R'
    IF !LLUSEARRAY OR OCCURS("|",&LCARRAY[lnCount,6]) < 25
      IF LLFILTER
        LCFILTEXP=IIF(&LCARRAY[lnCount,4],'INLIST(','!INLIST(')+;
          IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
          +IIF(&LCARRAY[lnCount,3]='D',")",'')+','+;
          LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
          &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
          ')'+LCELMSEP+' .AND. '+LCELMSEP
      ELSE
        LCFILTEXP= IIF(&LCARRAY[lnCount,4],'','!(')+;
          IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
          +IIF(&LCARRAY[lnCount,3]='D',")",'')+' IN('+;
          LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
          &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+')'+;
          IIF(&LCARRAY[lnCount,4],'',')')+LCELMSEP+' .AND. '+LCELMSEP
      ENDIF
    ELSE    && Else [If the function can use the filter array in the filter expression]
      DO CASE

      CASE &LCARRAY[lnCount,3] = 'N'
        *-- Variable to hold the Numeric field Picture
        LCPICT    = IIF(ATC(LCELMSEP , &LCARRAY[lnCount,6]) = 0 ,;
          &LCARRAY[lnCount,6] ,;
          SUBSTR(&LCARRAY[lnCount,6] , 1 ,;
          ATC(LCELMSEP , &LCARRAY[lnCount,6]) - 1))

        LCPICT    = STUF(REPLICATE('9' , LEN(LCPICT)) ,;
          ATC('.' , LCPICT) ,;
          IIF(ATC('.' , LCPICT) = 0 , 0 , 1) ,;
          IIF(ATC('.' , LCPICT) = 0 , '' , '.'))

        *-- Variable to hold the Numeric field Length (In character
        *-- string)
        LCLENGTH  = ALLTRIM(STR(LEN(LCPICT)))

        *-- Variable to hold the Numeric field Decimal Length (In
        *-- character string)
        LCDECIMAL = ALLTRIM(STR(IIF(AT('.' , LCPICT) = 0 , 0 ,;
          LEN(LCPICT) - AT('.' , LCPICT))))

        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          'STR(' + ALLTRIM(&LCARRAY[lnCount,1]) +;
          ' , ' + LCLENGTH + ' , ' + LCDECIMAL + ')' +;
          ' $ ' + ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      CASE &LCARRAY[lnCount,3] = 'D'
        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          'DTOC(' + ALLTRIM(&LCARRAY[lnCount,1]) + ')' +;
          ' $ ' + ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      OTHERWISE
        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          ALLTRIM(&LCARRAY[lnCount,1]) +;
          ' $ '+ ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      ENDCASE
    ENDIF    && End of IF !llUseArray

    *In list operator [Begin]
  ELSE    && Else [If this filter option is using the In range screen]

    IF USED(&LCARRAY[lnCount,6]) .AND. SEEK('' , &LCARRAY[lnCount,6])
      PRIVAT LNSAVALS , LNSELCNT , LCINLSTEXP , LCOPTEXP , LCKEY
      LNSAVALS   = SELECT(0)        && Save the old alias.
      LNSELCNT   = 0                && Variable to be used to count the user selection
      LCINLSTEXP = ""               && Variable to hold the user selection
      LCKEY      = ''

      *-- Get an optimize-able expression, if applicable.
      LCOPTEXP   = LFGETOPTEX(LCARRAY , LNCOUNT , @LCKEY)

      SELECT (&LCARRAY[lnCount,6])

      *-- Scan loop to count and get the user selection as far as it
      *-- doesn't exceeds 24 selection.
      SCAN WHILE LNSELCNT <= 25
        *-- count the user selection
        LNSELCNT   = LNSELCNT   + 1
        *-- Accumulate the key - if there is one - and the user selection
        *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
        *lcInlstExp = lcInlstExp + ',"' + lcKey + EVAL(KEY()) + '"'
        * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
        *lcInlstExp = lcInlstExp + ",'" + lcKey + EVAL(KEY()) + "'"
        IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
          LCINLSTEXP = LCINLSTEXP + ",[" + LCKEY + EVAL(KEY()) + "]"
        ELSE
          LCINLSTEXP = LCINLSTEXP + ",'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "'"
        ENDIF
        * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
        *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]
      ENDSCAN    && End of SCAN WHILE lnSelCnt <= 25

      *-- If the user selected more than 24 selection
      IF LNSELCNT > 24

        *-- Use BETWEEN() function, if applicable.
        IF &LCARRAY[lnCount,4]
          GO TOP
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
          *!*	            lcFiltExp = 'BETWEEN(' + lcOptExp +;
          *!*	                        ',"' + lcKey + EVAL(KEY()) + '",'
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
          *lcFiltExp = 'BETWEEN(' + lcOptExp +;
          ",'" + lcKey + EVAL(KEY()) + "',"
          IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
            LCFILTEXP = 'BETWEEN(' + LCOPTEXP +;
              ",[" + LCKEY + EVAL(KEY()) + "],"
          ELSE
            LCFILTEXP = 'BETWEEN(' + LCOPTEXP +;
              ",'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "',"
          ENDIF
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]
          GO BOTTOM
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
          *!*	            lcFiltExp = lcFiltExp + '"' + lcKey + EVAL(KEY()) + '")' +;
          *!*	                        lcElmSep + ' .AND. ' + lcElmSep
          *lcFiltExp = lcFiltExp + "'" + lcKey + EVAL(KEY()) + "')" +;
          lcElmSep + ' .AND. ' + lcElmSep

          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
          IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
            LCFILTEXP = LCFILTEXP + "[" + LCKEY + EVAL(KEY()) + "])" +;
              LCELMSEP + ' .AND. ' + LCELMSEP
          ELSE
            LCFILTEXP = LCFILTEXP + "'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "')" +;
              LCELMSEP + ' .AND. ' + LCELMSEP
          ENDIF
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]

        ENDIF    && End of IF &lcArray[lnCount,4]

        *-- Use the SEEK() function
        LCFILTEXP = LCFILTEXP + IIF(&LCARRAY[lnCount,4] , '' , '!') + 'SEEK(' + ;
          ALLTRIM(&LCARRAY[lnCount,1]) + ' , "' + ;
          &LCARRAY[lnCount,6] + '")' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

        *-- Use INLIST when the selection less than 24
      ELSE    && Else (If the user selected 24 selection or less.)
        LCFILTEXP = 'INLIST(' + LCOPTEXP + LCINLSTEXP + ;
          ')' + LCELMSEP + ' .AND. ' + LCELMSEP
      ENDIF
      SELECT (LNSAVALS)

    ELSE    && Else [If the In Range cursor is not opened or if it has no records]
      LCFILTEXP = ''
    ENDIF    && End of IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
  ENDIF    && End of IF &lcArray[lnCount,7] <> 'R'

ENDCASE

*-- BADRAN (N000398,1 - Build Aria3 Option Grid)
*-- IF it's 8 dimension array, save the filter expression to column 8...BEGIN
IF ALEN(&LCARRAY.,2) = 8
  LOCAL LNATELMSEP
  LNATELMSEP = RAT(LCELMSEP,LCFILTEXP,2)
  &LCARRAY.[lnCount,8] = LEFT(LCFILTEXP,LNATELMSEP-1)
ENDIF
*-- IF it's 8 dimension array, save the filter expression to column 8...END
RETURN LCFILTEXP &&
*--  end of lfGetQCond.

*!*****************************************************************************************
*! Name      : lfGetOper
*! Developer :
*! Date      : 10/13/2002 10:41:02 AM
*! Purpose   : Get Filter operator
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!****************************************************************************************
FUNCTION LFGETOPER

*! E038650,1 MAH 10/27/2004 Add new parameter to return english filter string [BEGIN]
*-- PARAMETER lcOperator,llisnot
PARAMETERS LCOPERATOR, LLISNOT, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

*! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
IF LLENGLISHQUERY
  DO CASE
  CASE LCOPERATOR = 'Greater Than'
    RETURN IIF(LLISNOT, '', 'not ') + 'greater than'

  CASE LCOPERATOR = 'Less Than'
    RETURN IIF(LLISNOT, '', 'not ') + 'less than'

  CASE LCOPERATOR = 'Greater Or Equal'
    RETURN IIF(LLISNOT, '', 'not ') + 'greater or equal'

  CASE LCOPERATOR = 'Less Or Equal'
    RETURN IIF(LLISNOT, '', 'not ') + 'less or equal'
  ENDCASE
ELSE
  *! E038650,1 MAH 10/27/2004 [END]
  DO CASE
  CASE LCOPERATOR = 'Greater Than'
    RETURN '>'
  CASE LCOPERATOR = 'Less Than'
    RETURN '<'
  CASE LCOPERATOR = 'Greater Or Equal'
    RETURN '>='
  CASE LCOPERATOR = 'Less Or Equal'
    RETURN '<='
  ENDCASE
  *! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
ENDIF
*! E038650,1 MAH 10/27/2004 [END]

*-- end of lfGetOper.

*!*****************************************************************************************
*! Name      : lfRightGet
*! Developer :
*! Date      : 10/13/2002 10:41:47 AM
*! Purpose   : Get Right value
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values.
*! B039435,1 MAH Fix bug of there is a quote in the field value.
*! B129154,1 MAH 07/28/2005 Error in PO Report.
*!****************************************************************************************

FUNCTION LFRIGHTGET
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS mRightHead,cLeftType,cOperator,lcElmSep,cRightType

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS mRightHead, cLeftType, cOperator, lcElmSep, cRightType, lcDBEngine
PARAMETERS MRIGHTHEAD, CLEFTTYPE, COPERATOR, LCELMSEP, CRIGHTTYPE, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)
*! E038142,2 MAH 08/31/2004 [END]

LCRETVAL=MRIGHTHEAD
DO CASE
CASE CRIGHTTYPE='V'
  DO CASE
  CASE CLEFTTYPE $ 'CM'
    IF INLIST(COPERATOR,'Between','In List')
      *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
      *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
      LCSEPER = IIF(!LLFILTER AND COPERATOR = 'Between', IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
      *! E038142,2 MAH 08/31/2004 [END]

      *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
      *-- lcRetVal='"'+STRTRAN(mRightHead,lcElmSep,'"'+lcSeper+'"')+'"'

      *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
      *-- IF llCaseInsensitive
      *--   IF !EMPTY(mRightHead)
      *--     LOCAL lcRight
      *--     lcRight = UPPER(mRightHead)
      *--
      *--     LOCAL laRight[1]
      *--     =gfSubStr(lcRight, @laRight, lcElmSep)
      *--
      *--     lcRight = ''
      *--     LOCAL lnIndex
      *--     FOR lnIndex = 1 TO ALEN(laRight, 1)
      *--       lcRight = lcRight + laRight[lnIndex] + lcElmSep
      *--     ENDFOR
      *--     lcRight = SUBSTR(lcRight, 1, LEN(lcRight) - 1)
      *--   ENDIF
      *--
      *--   lcRetVal = '"' + STRTRAN(UPPER(lcRight), lcElmSep, '"' + lcSeper + '"') + '"'
      *-- ELSE
      *--   lcRetVal = '"' + STRTRAN(mRightHead, lcElmSep, '"' + lcSeper + '"') + '"'
      *-- ENDIF

      IF !EMPTY(MRIGHTHEAD)
        LOCAL LCRIGHT

        IF LLCASEINSENSITIVE
          LCRIGHT = UPPER(MRIGHTHEAD)
          *! B129154,1 MAH 07/28/2005 [BEGIN]
        ELSE
          LCRIGHT = MRIGHTHEAD
          *! B129154,1 MAH 07/28/2005 [END]
        ENDIF

        LOCAL LARIGHT[1]
        =GFSUBSTR(LCRIGHT, @LARIGHT, LCELMSEP)

        LCRIGHT = ''
        LOCAL LNINDEX
        FOR LNINDEX = 1 TO ALEN(LARIGHT, 1)
          LCRIGHT = LCRIGHT + GFSTRTOEXP(LARIGHT[lnIndex], LCDBENGINE, LLENGLISHQUERY) + LCELMSEP
        ENDFOR

        LCRIGHT = SUBSTR(LCRIGHT, 1, LEN(LCRIGHT) - 1)
      ENDIF

      LCRETVAL = STRTRAN(UPPER(LCRIGHT), LCELMSEP, LCSEPER)
      *! B039435,1 MAH [END]
      *! E038142,2 MAH 08/31/2004 [END]
    ELSE
      *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
      *-- RETURN '"'+IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead)+'"'

      *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
      IF COPERATOR = 'Greater Than'
        IF LLCASEINSENSITIVE
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + RTRIM(UPPER(mrightHead)) + ' "'
          RETURN GFSTRTOEXP(RTRIM(UPPER(MRIGHTHEAD)), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
        ELSE
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + RTRIM(mrightHead) + ' "'
          RETURN GFSTRTOEXP(RTRIM(MRIGHTHEAD), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
        ENDIF
      ELSE
        *! B127002,1 MAH 04/05/2005[END]
        IF LLCASEINSENSITIVE
          *! E037885,2 MAH 12/03/2004 [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), UPPER(mrightHead)) + '"'
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), RTRIM(UPPER(mrightHead))) + '"'
          RETURN GFSTRTOEXP(IIF(COPERATOR = 'Contains', UPPER(RTRIM(MRIGHTHEAD)), RTRIM(UPPER(MRIGHTHEAD))), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
          *! E037885,2 MAH 12/03/2004 [END]
        ELSE
          *! E037885,2 MAH 12/03/2004 [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), mrightHead) + '"'
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), RTRIM(mrightHead)) + '"'
          RETURN GFSTRTOEXP(IIF(COPERATOR = 'Contains', ALLTRIM(MRIGHTHEAD), RTRIM(MRIGHTHEAD)), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
          *! E037885,2 MAH 12/03/2004 [END]
        ENDIF
        *! E038142,2 MAH 08/31/2004 [END]

        *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
      ENDIF
      *! B127002,1 MAH 04/05/2005[END]

    ENDIF
  CASE CLEFTTYPE = 'N'
    *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
    *-- lcSeper=IIF(COPERATOR='Between' AND !llFilter,' AND ',',')
    LCSEPER = IIF(COPERATOR='Between' AND !LLFILTER, IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
    *! E038142,2 MAH 08/31/2004 [END]

    LCRETVAL=STRTRAN(MRIGHTHEAD,LCELMSEP,LCSEPER)
    IF EMPTY(LCRETVAL)
      LCRETVAL='0'
    ENDIF

  CASE CLEFTTYPE = 'D'
    LCOLDCEN = SET('CENT')
    SET CENTURY ON
    IF INLIST(COPERATOR,'Between','In List')
      LOCAL LNDATES, LCRIGHTHAND, LNDATE, LCDATE
      LCRIGHTHAND = LCELMSEP + ALLTRIM(MRIGHTHEAD) + LCELMSEP
      LNDATES = OCCURS(LCELMSEP,LCRIGHTHAND) - 1
      LCRETVAL = ""
      FOR LNDATE = 1 TO LNDATES
        LCDATE = STREXTRACT(LCRIGHTHAND,LCELMSEP,LCELMSEP,LNDATE)


        *! E038650,1 MAH 01/18/2005 [BEGIN]
        *-- lcRetVal = lcRetVal + ",'" + CDateToString(lcDate) + "'"
        IF !LLFILTER AND COPERATOR = 'Between'
          IF LLENGLISHQUERY
            IF EMPTY(LCRETVAL)
              LCRETVAL = LCRETVAL + " '" + CDATETOSTRING(LCDATE) + "'"
            ELSE
              LCRETVAL = LCRETVAL + " and '" + CDATETOSTRING(LCDATE) + "'"
            ENDIF
          ELSE
            IF EMPTY(LCRETVAL)
              LCRETVAL = LCRETVAL + " '" + CDATETOSTRING(LCDATE) + "'"
            ELSE
              LCRETVAL = LCRETVAL + " AND '" + CDATETOSTRING(LCDATE) + "'"
            ENDIF
          ENDIF
        ELSE
          LCRETVAL = LCRETVAL + ",'" + CDATETOSTRING(LCDATE) + "'"
        ENDIF
        *! E038650,1 MAH 01/18/2005 [END]
      ENDFOR
      LCRETVAL = SUBSTR(LCRETVAL,2)

      *--             lcSeper=IIF(!llFilter AND cOperator='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')
      *--              lcRetVal='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'  }))'+lcSeper+'{  ')+'  }))'
    ELSE
      IF BETWEEN(YEAR(CTOD(MRIGHTHEAD)) ,1900,1950)
        LCTMPYEAR  = ALLTRIM(STR(YEAR(CTOD(MRIGHTHEAD)) + 100))
        MRIGHTHEAD = SUBSTR(MRIGHTHEAD,1,6)+LCTMPYEAR
      ENDIF
      *lcRetVal='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
      *! E038142,2 MAH 08/31/2004 Correct the code [BEGIN]
      *-- lcRetVal = CDateToString(mRightHead)
      LCRETVAL = "'" + CDATETOSTRING(MRIGHTHEAD) + "'"
      *! E038142,2 MAH 08/31/2004 [END]
    ENDIF
    SET CENTURY &LCOLDCEN

  CASE CLEFTTYPE = 'L'
    *! E038142,2 MAH 08/31/2004 Return value condition according to DB Engine [BEGIN]
    *-- RETURN ' '+lcRetVal+' '
    DO CASE
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
      *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][Start]
      *RETURN ' ' + lcRetVal + ' '
      RETURN ' ' + IIF(TYPE('lcRetVal')='L', IIF(LCRETVAL = .T.,".T.",".F."),LCRETVAL)+ ' '
      *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][End]

    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
      *B608900,1 WAM 06/21/2009 Fix expression
      *RETURN ' ' + IIF(UPPER(ALLTRIM(lcRetVal)) = '.T.' .OR. UPPER(ALLTRIM(lcRetVal)) = 'T', 1, 0) + ' '
      RETURN ' ' + IIF(UPPER(ALLTRIM(LCRETVAL)) = '.T.' .OR. UPPER(ALLTRIM(LCRETVAL)) = 'T', '1','0') + ' '
      *B608900,1 WAM 06/21/2009 (End)
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
  ENDCASE

CASE CRIGHTTYPE='F'
  *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
  *-- lcRetVal=STRTRAN(IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead),lcElmSep,',')
  *! E038650,1 MAH 01/18/2005 [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    IF CLEFTTYPE $ 'MC'
      IF LLCASEINSENSITIVE
        LCRETVAL = 'UPPER(RTRIM(' + ALLTRIM(MRIGHTHEAD) + '))'
      ELSE
        LCRETVAL = STRTRAN(IIF(COPERATOR = 'Contains', ALLTRIM(MRIGHTHEAD) , MRIGHTHEAD), LCELMSEP, ',')
      ENDIF
    ENDIF

    IF CLEFTTYPE $ 'NL'
      LCRETVAL = ALLTRIM(MRIGHTHEAD)
    ENDIF

    IF CLEFTTYPE $ 'D'
      LCRETVAL = 'DTOS(' + ALLTRIM(MRIGHTHEAD) + ')'
    ENDIF

  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCRETVAL = '[' + ALLTRIM(MRIGHTHEAD) + ']'
    IF CLEFTTYPE = 'D'
      LCRETVAL = ' CONVERT(CHAR(8),' + LCRETVAL + ', 112) '
    ENDIF
  ENDCASE
  *! E038650,1 MAH 01/18/2005 [END]
  *! E038142,2 MAH 08/31/2004 [END]
ENDCASE

IF INLIST(COPERATOR,'Between','In List') AND EMPTY(ALLTRIM(MRIGHTHEAD))
  *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
  *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
  LCSEPER = IIF(!LLFILTER AND COPERATOR='Between', IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
  *! E038142,2 MAH 08/31/2004 [END]

  LCRETVAL=LCRETVAL+LCSEPER+LCRETVAL
ENDIF
RETURN LCRETVAL
*-- end of lfRightGet. DTOS

*!*************************************************************
*! Name      : lfGetOptEx
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 05/25/1999
*! Purpose   : create an optimize-able expression - if applicable -
*!             for the In Range options.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetOptEx()
*!*************************************************************
*B602931,4 this function was added by HS for the bug entry B602931,4.
*!*************************************************************
FUNCTION LFGETOPTEX

PARAMETERS LCARRAY , LNROW , LCKEY

LCKEY = ''

PRIVATE LNARRAYROW , LCFILE , LCFIELD , LCKEYEXPR , LCOPTEXP , LNSELECT ,;
  LNCOUNT , LAOPTEXP

LNARRAYROW = ASCAN(LARANGEINFO, UPPER(ALLTRIM(&LCARRAY.[lnRow,1])))

*-- If the filter option is of type expression.
IF LNARRAYROW = 0
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ENDIF
LNARRAYROW = ASUBSCRIPT(LARANGEINFO, LNARRAYROW , 1)


*-- Get the file of the filter option
LCFILE = SUBSTR(ALLTRIM(&LCARRAY[lnRow,1]) ,;
  1 , AT('.' , ALLTRIM(&LCARRAY[lnRow,1])) - 1)

*-- If the file of the filter option is not open, we will not be able to
*-- get the indexes
IF !USED(LCFILE)
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ENDIF

*-- Get the field of the filter option
LCFIELD = UPPER(SUBSTR(ALLTRIM(&LCARRAY[lnRow,1]) ,;
  AT('.' , ALLTRIM(&LCARRAY[lnRow,1])) + 1))

*-- Get the key expression that was passed to the In Range function
LCKEYEXPR = IIF(AT('FOR' , LARANGEINFO[lnArrayRow,2]) = 0 ,;
  LARANGEINFO[lnArrayRow,2] ,;
  SUBSTR(LARANGEINFO[lnArrayRow,2] , 1 ,;
  AT('FOR' , LARANGEINFO[lnArrayRow,2]) - 1))

LNSELECT = SELECT(0)        && Save the alias
SELECT (LCFILE)

LCOPTEXP = ''
LNCOUNT  = 0

*-- DO WHILE loop to search for an index that starts with the field of the
*-- filter option.
DO WHILE LCOPTEXP <> LCFIELD
  LNCOUNT  = LNCOUNT + 1
  LCOPTEXP = KEY(LNCOUNT)
  IF EMPTY(LCOPTEXP)
    EXIT
  ENDIF
ENDDO    && End of DO WHILE lcOptExp <> lcField

*-- If we didn't find an index that starts with the field of the filter
*-- option we are going to search for one that starts with the same key
*-- used with the In Range function
IF EMPTY(LCOPTEXP) .AND. TYPE(LCKEYEXPR) = 'C'

  IF LEN(&LCKEYEXPR) = 0
    LNCOUNT  = 0
    DO WHILE LCOPTEXP <> LCFIELD
      LNCOUNT  = LNCOUNT + 1
      LCOPTEXP = KEY(LNCOUNT)
      IF EMPTY(LCOPTEXP)
        EXIT
      ENDIF    && End of IF EMPTY(lcOptExp)
    ENDDO    && End of DO WHILE lcOptExp <> laBrFldFlt[lnArrayRow,4] + ...

    *-- If we found an index that can be used
    IF !EMPTY(LCOPTEXP)
      *-- Get the key that will be used with this index expression
      LCKEY = &LCKEYEXPR
    ENDIF    && End of IF !EMPTY(lcOptExp)
  ENDIF    && End of IF LEN(&lcKeyExpr) = LEN(&laBrFldFlt[lnArrayRow,4])
ENDIF    && End of EMPTY(lcOptExp) .AND. TYPE(laBrFldFlt[lnArrayRow,4]) ...

SELECT (LNSELECT)

*-- If we did not find an index that can be used
IF EMPTY(LCOPTEXP)
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ELSE    && Else (If we found an index that can be used)
  DIMENSION LAOPTEXP[1,1]
  =GFSUBSTR(LCOPTEXP , @LAOPTEXP , '+')
  LCOPTEXP = ''

  *-- FOR loop to add the alias to the index expression
  FOR LNCOUNT = 1 TO ALEN(LAOPTEXP , 1)
    LCOPTEXP = LCOPTEXP + IIF(EMPTY(LCOPTEXP) , '' , '+') +;
      IIF(OCCURS('(' , LAOPTEXP[lnCount,1]) = 0 ,;
      LCFILE + '.' + LAOPTEXP[lnCount,1] ,;
      STUFF(LAOPTEXP[lnCount,1] ,;
      RAT('(' , LAOPTEXP[lnCount,1]) + 1 , 0 ,;
      LCFILE + '.'))

  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laOptExp , 1)
  RETURN LCOPTEXP
ENDIF    && End of IF EMPTY(lcOptExp)
*-- end of lfGetOptEx.
*-- Added by Badran in 10/13/2002 ... END

*!************************************************************************************
*! Name : GetExpr.
*!************************************************************************************
*! Synopsis : Build an expression
*!************************************************************************************
*! Passed :
*!        Parameters :
*!           lcExp       : The Default Expression
*!           lcFile      : The File to use it's fields
*!           lcExpPrmpt  : The Title of the expression builder
*!           la_Field    : The name of the fields header array
*!           laField     : The phiscal fields array
*!           llAddAlias  : add alias to the expression or not
*!           lcType      : the type of expression that will return
*!           laFiles     : files is used
*!           la_Var      : array that holds variable that can be use in expression
*!************************************************************************************
*! Returned : Expression String
*!************************************************************************************
*! Example :
*!
*!************************************************************************************
FUNCTION GETEXPR
PARAMETER LCEXP,LCFILE,LCEXPPRMPT,LA_FIELD,LAFIELD,LLADDALIAS,LCTYPE,LAFILES,LA_VAR
*SET CLASSLIB TO r:\aria4\classes\utility.vcx
PRIVATE OEXPR
OEXPR=CREATEOBJECT("ariaexpression",LCEXP,LCFILE,LCEXPPRMPT,LA_FIELD,@LAFIELD,LLADDALIAS,LCTYPE,LAFILES,LA_VAR)

IF !EMPTY(OEXPR.LAFIELD) AND !('U' $ OEXPR.LCTYPE)
  FOR LNCOUNT = 1 TO ALEN(OEXPR.LAFIELD)
    LCFLDNAME = OEXPR.LAFIELD[lnCount]
    DO CASE
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="M" OR LEFT(OEXPR.LAFIELD[lnCount],1)="C"
      &LCFLDNAME = " "
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="N"
      &LCFLDNAME  = 1
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="D"
      &LCFLDNAME = DATE()
    ENDCASE
  ENDFOR
ELSE
  IF TYPE('oExpr.laFiles') $ 'UL' AND EMPTY(OEXPR.LAFIELD)
    OEXPR.LAFIELD = " "
  ENDIF
ENDIF
OEXPR.SHOW
LCRETU = OEXPR.MVEXP
RETURN LCRETU



*!************************************************************************************
*! Name : gpStDyBrow.
*!************************************************************************************
*! Synopsis : Browse the dyelots for a specific Style/Color at a specific warehouse.
*!************************************************************************************
*! Passed :
*!        Parameters :
*!           lcStyle : The active style.
*!           lcColor : The active color.
*!           lcWare  : The active warehouse.
*!           lcDyelot: The dyelot.
*!        Files      : StyDye,Style file should be opened.
*!************************************************************************************
*! Returned :
*!        Parameters : Style, Location & Selected Dyelot
*!************************************************************************************
*! Example :
*!        DO gpStDyBrow WITH lcStyle,lcFromWare,lcDyelot
*!************************************************************************************
PROCEDURE GPSTDYBROW
*N119812,1 HBG 16/11/2003 Add new parameter wit .T. or .F. to check if use style configuration [Begin]
*PARAMETER lcStyle,lcWare,lcDyelot
PARAMETER LCSTYLE,LCWARE,LCDYELOT,LLUSECONFG
*N119812,1 [End]
LNALIAS=SELECT()
LLMULTIWH  = GFGETMEMVAR('M_WareHouse')='Y'
SELECT STYLE
LNXSVREC=RECNO()
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
SELECT STYDYE
IF !SEEK(PADR(LCSTYLE,19)+LCWARE+LCDYELOT)
  IF LLMULTIWH
    *N119812,1 HBG 16/11/2003 Change all text to be variables [Begin]
    *lcmesgx = 'Style\Warehouse :'+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcmesgx = LANG_ARIA_MsgSgx1+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    LCMESGX = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGSGX1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgSgx1",LCARIAHFILE))+ALLTRIM(LCSTYLE)+'\'+ALLTRIM(LCWARE)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N119812,1 [End]
  ELSE
    *N119812,1 HBG 16/11/2003 Change all text to be variables [Begin]
    *lcmesgx = 'Style :'+ALLTRIM(lcStyle)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcmesgx = LANG_ARIA_MsgSgx2+ALLTRIM(lcStyle)
    LCMESGX = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGSGX2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgSgx2",LCARIAHFILE))+ALLTRIM(LCSTYLE)
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    *N119812,1 [End]
  ENDIF
  *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
  IF LLUSECONFG
    LNCHOICE=GFMODALGEN('TRM42254B42003','DIALOG',ALLTRIM(LCSTYLE)+'|'+ALLTRIM(LCDYELOT)+'|'+LCWARE)
  ELSE
    *N119812,1 [End]
    LNCHOICE=GFMODALGEN('TRM42062B42003','DIALOG',ALLTRIM(LCSTYLE)+'|'+ALLTRIM(LCDYELOT)+'|'+LCWARE)
    *N119812,1 HBG 16/11/2003 End if use style configuration , Change the message [Begin]
  ENDIF
  *N119812,1 [End]
  IF LNCHOICE = 1
    IF !SEEK(PADR(LCSTYLE,19)+LCWARE+SPACE(10),'STYDYE')
      DO GPADSTYWAR WITH LCSTYLE,SPACE(10),LCWARE
    ENDIF
    DO GPADSTYWAR WITH LCSTYLE,LCDYELOT,LCWARE
    RETURN

  ELSE
    IF LNCHOICE = 2
      IF SEEK(PADR(LCSTYLE,19)+LCWARE+SPACE(10),'STYDYE')
        LOCATE REST WHILE STYLE+CWARECODE = PADR(LCSTYLE,19)+LCWARE FOR !EMPTY(DYELOT)
        IF !FOUND()
          *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
          IF LLUSECONFG
            =GFMODALGEN('TRM42253B42001','DIALOG',LCMESGX)
          ELSE
            *N119812,1 [End]
            =GFMODALGEN('TRM42053B42001','DIALOG',LCMESGX)
            *N119812,1 HBG 16/11/2003 If use style configuration , Change the message [Begin]
          ENDIF
          *N119812,1 [End]
          LCDYELOT = SPACE(10)
          RETURN
        ENDIF
      ENDIF
    ELSE
      LCDYELOT = SPACE(10)
      RETURN
    ENDIF
  ENDIF
ENDIF

IF STYLE.STYLE <> STYDYE.STYLE
  =SEEK(STYDYE.STYLE,'STYLE')
ENDIF
=SEEK('S'+STYLE.SCALE,'SCALE')

*N119812,1 HBG 16/11/2003 If use style configuration , Change the field name [Begin]
*lcBrfields = "Dyelot  :H='Dyelot#' ,"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcHeader = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
LCHEADER = IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CONFIGNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ConfigNum",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DyelotNum",LCARIAHFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LCBRFIELDS = "Dyelot  :H='"+LCHEADER+"' ,"

*--- Change all text to variables

*!*	FOR I=1 TO SCALE.Cnt
*!*	  Z=STR(I,1)
*!*	  lcString = "Stk"+Z+" :H='Stock&Z' ,"
*!*	  lcBrfields = lcBrfields + lcString
*!*	ENDFOR
*!*	lcBrfields = lcBrfields + "TotStk :H='TotStk',"

*!*	FOR I=1 TO SCALE.Cnt
*!*	  Z=STR(I,1)
*!*	  lcString = "Alo"+Z+" :H='Aloc.&Z',"
*!*	  lcBrfields = lcBrfields + lcString
*!*	ENDFOR
*!*	lcBrfields = lcBrfields + "TotAlo :H='TotAlo'"
LCBRFIELDS = LCBRFIELDS +;
  "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':6,"+;
  "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':6,"+;
  "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':6,"+;
  "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':6,"+;
  "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':6,"+;
  "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':6,"+;
  "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':6,"+;
  "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"',"+;
  "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':7,"+;
  "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':6,"+;
  "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':6,"+;
  "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':6,"+;
  "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':6,"+;
  "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':6,"+;
  "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':6,"+;
  "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':6,"+;
  "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':6,"+;
  "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ToTAlo",LCARIAHFILE))+"':7"


*N119812,1 [End]

LCDYELOT = SPACE(10)
LCDYEKEY = PADR(LCSTYLE,19)+LCWARE
*N119812,1 HBG 16/11/2003 If use style configuration , Change the browse title [Begin]
*IF ARIABROW([lcDyeKey FOR !EMPTY(Dyelot) REST],' Style Dyelots ', gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ARIABROW([lcDyeKey FOR !EMPTY(Dyelot) REST],IIF(llUseConfg,LANG_ARIA_StyConfg,LANG_ARIA_StyDyelot), gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2)
IF ARIABROW([lcDyeKey FOR !EMPTY(Dyelot) REST],IIF(LLUSECONFG,;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STYCONFG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_StyConfg",LCARIAHFILE)),;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STYDYELOT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_StyDyelot",LCARIAHFILE))), GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N119812,1 [End]
  LCDYELOT = DYELOT
ENDIF

SELECT STYLE
GOTO LNXSVREC
SELECT(LNALIAS)
RETURN


*!************************************************************************************
*! Name : sygraph.
*!************************************************************************************
*! Synopsis : display graphs for specific data
*!************************************************************************************
*! Passed :
*!        Parameters :
*!           lcTempCurs  : The cursor to be used in graph
*!           lnGraphType : The Graph type to display (pie,bar,...) value from 1 to 14
*!           lcFields    : The fields that the graph will be generated for
*!           llByRow     : display graph by row if true else by column
*!************************************************************************************
*! Returned : none
*!************************************************************************************
*! Example :
*!
*!************************************************************************************
FUNCTION SYGRAPH
PARAMETERS LCTEMPCURS,LNGRAPHTYPE,LCFIELDS,LLBYROW
DO FORM (OARIAAPPLICATION.SCREENHOME+"SY\SYGRAPH") WITH LCTEMPCURS,LNGRAPHTYPE,LCFIELDS,LLBYROW
RETURN

*!*************************************************************
*! Name      : gfWait
*! Developer : Reham Al-Allamy
*! Date      : 2002
*! Purpose   : To display wait message from the dialog dectionary
*!*************************************************************
*! Calls     :
*!          Calls: GFSUBSTR()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*
FUNCTION GFWAIT
PARAMETERS LCMESSAGNO,LCNOWAIT,LCVARSSTR

LCCURRDBF   = ALIAS()
LCNOWAIT    = IIF(TYPE('lcNoWait' )='C',LCNOWAIT ,'')
LCVARSSTR   = IIF(TYPE('lcVarsStr')='C',LCVARSSTR,'')
*N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[Start]
IF (OARIAAPPLICATION.OACTIVELANG.CLANG_ID != "EN")
  LOCAL LCDIALOGALIAS
  LCDIALOGALIAS = OARIAAPPLICATION.GETLANGALIAS("SysFiles27","SYDDLOBJ_DBF")
ENDIF
*N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[End]
*-- Create cursor temp. name to hold the message info.
LCTMPCURS = GFTEMPNAME()
LNRESULT  = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * From syddlobj WHERE cdlobjtyp = 'M' AND cdlobjid = '"+LCMESSAGNO+"'", ;
  "",LCTMPCURS,"", OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3 ,"",SET("DATASESSION"))
IF LNRESULT = 1
  SELECT (LCTMPCURS)
  LOCATE
  IF FOUND() AND !EMPTY(CDLOBJTYP+CDLOBJID)
    LCMESAGTXT = ALLTRIM(MDLOBJ)
    *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[Start]
    *IF AT("ð",lcMesagtxt) > 0
    IF (OARIAAPPLICATION.OACTIVELANG.CLANG_ID != "EN") AND !EMPTY(LCDIALOGALIAS)
      LOCAL LCOTHERDLG
      LCOTHERDLG = OARIAAPPLICATION.EVALXML_LOCATE(LCDIALOGALIAS, &LCTMPCURS..CAPP_ID+&LCTMPCURS..CDLOBJTYP+&LCTMPCURS..CDLOBJID)
      IF !EMPTY(LCOTHERDLG)
        LCMESAGTXT = LCOTHERDLG
      ENDIF
    ENDIF
    IF AT(IIF((OARIAAPPLICATION.OACTIVELANG.CLANG_ID != "EN"),"<<>>","ð"),LCMESAGTXT) > 0
      *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[End]
      DECLARE LAVARSSTR [1]
      *** Collect variables to be replaced from string to array
      =GFSUBSTR(LCVARSSTR,@LAVARSSTR,'|')

      *** Replace each ð mark with variabe sent
      FOR LNVARSSTR = 1  TO ALEN(LAVARSSTR,1)
        *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[Start]
        IF (OARIAAPPLICATION.OACTIVELANG.CLANG_ID != "EN")
          LCMESAGTXT = STRTRAN(LCMESAGTXT,"<<>>",LAVARSSTR[lnVarsStr],1,1)
        ELSE
          *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[END]
          LCMESAGTXT = STRTRAN(LCMESAGTXT,'ð',LAVARSSTR[lnVarsStr],1,1)
          *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[Start]
        ENDIF
        *N000682,1 MMT 03/13/2013 Fix issues of Globalization Testing Phase#2[End]
      ENDFOR
    ENDIF
    WAIT LCMESAGTXT WINDOW &LCNOWAIT
  ENDIF
  USE IN (LCTMPCURS)
ENDIF

IF !EMPTY(LCCURRDBF)
  SELECT(LCCURRDBF)
ENDIF

*!*************************************************************
*! Name      : gfTmp2Mast
*! Developer : Reham Al-Allamy
*! Date      : 23-10-2002
*! Purpose   : To update master file from a temp one
*!*************************************************************
*! Passed Parameters  : Master file name
*!                      Temp file name
*!                      Fixed message in thermo
*!                      variable message in thermo
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
* Program to update records of master file from temp file
* Temp file has to have 2 additional fields nRecNo and cStatus to hold
* the phisical records no and the action status done to this record
*
*:->
FUNCTION GFTMP2MAST
PARAMETERS LCMASTFILE,LCTEMPFILE,LCFXDMSG,LCVARMSG
PRIVATE    LCMASTFILE,LCTEMPFILE,LCSAVALIAS,LCFXDMSG,LCVARMSG

LOTOOLBARWINDOW = OARIAAPPLICATION.OTOOLBAR.OWINDPARENT
*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\GFTMP2MAST.H
*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE,LCTMP2MAST
LCARIAHFILE = ''
LCTMP2MAST = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCTMP2MAST  =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("gfTmp2Mast")+"_"+"H" +".XML")
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
LCSAVALIAS = SELECT(0)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFxdMsg   = IIF(TYPE('lcFxdMsg')<>'C',LANG_Save,lcFxdMsg )
LCFXDMSG   = IIF(TYPE('lcFxdMsg')<>'C',IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SAVE,OARIAAPPLICATION.GETHEADERTEXT("LANG_Save",LCTMP2MAST)),LCFXDMSG )
*N000682,1 11/20/2012 MMT Globlization changes[End]

LCVARMSG   = IIF(TYPE('lcVarMsg')<>'C',' ',LCVARMSG)

LCSAVEDEL = SET ('DELETE')
SET DELETE OFF


*-- Initialize the progress bar needed variables.
OPROGRESS = NEWOBJECT('ariaprogressbar',OARIAAPPLICATION.CLASSDIR+'utility.vcx')
OPROGRESS.TOTALPROGRESS = RECCOUNT(LCTEMPFILE)
OPROGRESS.LBLFIRSTLABEL.CAPTION = LCFXDMSG
OPROGRESS.SHOW()
LNCURRECORD  = 1

SELECT (LCTEMPFILE)
GO TOP
*-- Scan through all the Added,Modified or Deleted records
SCAN FOR CSTATUS <> 'S'
  *-- Call the progress bar.
  OPROGRESS.LBLSECONDLABEL.CAPTION = LCVARMSG
  OPROGRESS.CURRENTPROGRESS(LNCURRECORD)
  LNCURRECORD = LNCURRECORD + 1

  DO CASE
    *-- New added record
  CASE CSTATUS = 'A'
    SCATTER MEMVAR MEMO
    SELECT  (LCMASTFILE)
    IF SEEK(' ')                        && Chek if there is empty
      RECALL                            && Deleted records to recall
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO &LCMASTFILE FROM MEMVAR
    ENDIF

    *-- Record was modified
  CASE CSTATUS = 'M'
    SCATTER MEMVAR MEMO                 && Collect data from temp
    SELECT  (LCMASTFILE)
    GO &LCTEMPFILE..NRECNO
    GATHER  MEMVAR MEMO                 && Replace master data

    *-- Record was deleted
  CASE CSTATUS = 'D' .AND.  DELETED()
    SELECT  (LCMASTFILE)
    GO &LCTEMPFILE..NRECNO
    SCATTER MEMVAR MEMO BLANK           && Empty the record befor
    GATHER  MEMVAR MEMO                 && delete it
    DELETE                              && Delete recored not in temp
  ENDCASE

  SELECT  (LCTEMPFILE)
  REPLACE CSTATUS WITH "S"
ENDSCAN
*-- Terminate the progress bar
OPROGRESS=NULL
OARIAAPPLICATION.OTOOLBAR.OWINDPARENT = LOTOOLBARWINDOW

SET DELETE &LCSAVEDEL
SELECT (LCSAVALIAS)


*!*************************************************************
*! Name      : gfGetBrF
*! Developer : Amin Khodary
*! Date      : 30-10-2002
*! Purpose   : To get the browse field header string and file title.
*!*************************************************************
*! Passed Parameters  : lcFileTitl  -- Reference to the file title
*!                      lcBrowFlds  -- Reference to browse fields string
*!                      lcSrchFile  -- Which file you want to get its title amd field
*!                      lcFields    -- For which fields you want to get get its header.
*!*************************************************************
*! Called from Factor field validation, called from AP programs
*!*************************************************************
*! Returns            : lcFileTitl  -- Reference to the file title
*!                    : lcBrowFlds  -- Reference to browse fields string
*!*************************************************************
*! Example   : gfGetBrF(@lcFileTitl, @lcBrowFlds, 'APVENDOR',"cVendCode, cVendComp")
*!*************************************************************
*! Note      :  This fucntion called lfGetBrF in Aria27
*!*************************************************************

FUNCTION GFGETBRF
PARAMETERS LCFILETITL, LCBROWFLDS, LCSRCHFILE, LCFIELDS
LOCAL LLMAYPROCEED , LCCURALIAS , LNFIELDNUM
LLMAYPROCEED = .F.

PRIVATE  LAFIELDS
STORE " " TO LCFILETITL, LCBROWFLDS

*-- Use the file that passed as a parameter, otherwise use the current work area
LCCURALIAS = ALIAS()
LCSRCHFILE   = IIF(TYPE('lcSrchFile') <> 'C' .OR.;
  EMPTY(LCSRCHFILE), LCCURALIAS, LCSRCHFILE)
IF !EMPTY(LCSRCHFILE)
  LLMAYPROCEED = .T.
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
  *!*	  =oAriaApplication.remotesystemdata.execute("Select * from SYDFILES WHERE cFile_Nam = '"+UPPER(lcSrchFile)+"'",'',"SYDFILES","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
  *!*	  * B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
  *!*	  *=oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD",'',"SYDFIELD","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
  *!*	  =oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD",'',"AYDFIELD","",oAriaApplication.SystemConnectionString,3,"",SET("Datasession"))
  =OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFILES WHERE cFile_Nam = '"+UPPER(LCSRCHFILE)+"' AND CVER='A27'",'',"SYDFILES","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("Datasession"))
  =OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFIELD WHERE (EMPTY(CVER) OR CVER='A27')",'',"AYDFIELD","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("Datasession"))
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]
  * B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN" AND USED("AYDFIELD")
    *!*	    lcA27FIELDXML = OARIAAPPLICATION.LangPath + "Sysfiles27\SYDFIELD_DBF.XML"
    *!*	    IF FILE(lcA27FIELDXML)
    *!*	      lcA27Fields = gfTempName()
    *!*	      lnRecordCountReturned = XMLTOCURSOR(lcA27FIELDXML,lcA27Fields,512)
    *!*	      IF lnRecordCountReturned > 0
    *!*	        SELECT(lcA27Fields)
    *!*	        INDEX on row_key TAG (lcA27Fields)
    *!*	      ENDIF
    SELECT AYDFIELD
    LOCATE
    SCAN
      LCFLDHEADTRANS = OARIAAPPLICATION.GETA27FLDHEADER (ALLTRIM(AYDFIELD.CFLD_NAME))
      SELECT AYDFIELD
      REPLACE CFLD_HEAD WITH IIF(EMPTY(LCFLDHEADTRANS),CFLD_HEAD,LCFLDHEADTRANS)
    ENDSCAN
    *!*	    ENDIF
  ENDIF
  *N000682,1 MMT 12/09/2012 Globalization changes[END]

  DECLARE LAFIELDS[1,1]
  LAFIELDS   = " "
  LCFILETITL = ALLTRIM(LOOKUP(SYDFILES.CFILE_TTL, UPPER(ALLTRIM(LCSRCHFILE)),;
    SYDFILES.CFILE_NAM, 'cFile_Nam'))
  LCFIELDS   = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(LCFIELDS),;
    STRTRAN(SYDFILES.MBROW_FLD, '|', ','), LCFIELDS)

  =GFSUBSTR(LCFIELDS, @LAFIELDS, ",")
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
    LCFILETRANSTTL = OARIAAPPLICATION.GETA27FILETTL(LCSRCHFILE)
    LCFILETITL = IIF(EMPTY(LCFILETRANSTTL),LCFILETITL,LCFILETRANSTTL)
    FOR LNFIELDNUM = 1 TO ALEN(LAFIELDS,1)
      LCFLDHEADTRANS = OARIAAPPLICATION.GETA27FLDHEADER (ALLTRIM(LAFIELDS[lnFieldNum]))
      LCBROWFLDS = LCBROWFLDS + LAFIELDS[lnFieldNum]+;
        [:H=']+IIF(EMPTY(LCFLDHEADTRANS),ALLTRIM(LOOKUP(SYDFIELD.CFLD_HEAD,;
        UPPER(ALLTRIM(LAFIELDS[lnFieldNum])),AYDFIELD.CFLD_NAME,'cFld_Name')),LCFLDHEADTRANS)+[',]
    ENDFOR
  ELSE
    *N000682,1 MMT 12/09/2012 Globalization changes[END]

    FOR LNFIELDNUM = 1 TO ALEN(LAFIELDS,1)
      * B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
      * lcBrowFlds = lcBrowFlds + laFields[lnFieldNum]+;
      [:H=']+ALLTRIM(LOOKUP(SYDFIELD.cFld_Head,;
      UPPER(ALLTRIM(laFields[lnFieldNum])),SYDFIELD.cFld_Name,'cFld_Name'))+[',]
      LCBROWFLDS = LCBROWFLDS + LAFIELDS[lnFieldNum]+;
        [:H=']+ALLTRIM(LOOKUP(AYDFIELD.CFLD_HEAD,;
        UPPER(ALLTRIM(LAFIELDS[lnFieldNum])),AYDFIELD.CFLD_NAME,'cFld_Name'))+[',]
      * B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
    ENDFOR


    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  ENDIF
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  LCBROWFLDS  = SUBSTR(LCBROWFLDS, 1, LEN(LCBROWFLDS)-1)
  SELECT IIF(!EMPTY(LCCURALIAS),(LCCURALIAS),0)
ENDIF

RETURN LLMAYPROCEED
*PADR

**************************************************************************************

*!*************************************************************
*! Name      : gpFbDyBrow
*! Developer : Samah Wilson Kirollos (SWK)
*! Date      : 09/01/96.
*! Purpose   : Browse the Dyelots for a certain Fabric/Clr
*!             in a warehouse.
*!*************************************************************
*! Calls     : Functions  : ARIABROW
*!*************************************************************
*! Passed :
*!        Parameters :
*!            lcWare   : The Warehouse.
*!            lcFab    : The Fabric.
*!            lcClr    : The color.
*!            lcDyelot : The Dyelot
*!*************************************************************
*! Returns             : Dyelot
*!***********************************************************************
FUNCTION GPFBDYBROW
PARAMETER LCFAB,LCCLR,LCDYE,LCWARE

PRIVATE LADATA,LCBRFIELDS
LNALIAS = SELECT()

*-- Array to get values from browse
DECLARE LADATA[3]
LADATA = SPACE(1)

*--Variable to check if there is any dyelot
LLFOUND = .F.

*--Check if the system setup is Multi or Single WareHouse
LLMULTIWH = ALLTRIM(GFGETMEMVAR('M_WareHouse'))= 'Y'

*-- Title for the browse
LCTITLE = IIF (LLMULTIWH, 'ITEM\WAREHOUSE DYELOTS', 'ITEM\DYELOTS')

*-- Variable to check if we select from the browse or not
LLWASSEL = .T.

LCKEY = "lcFab+lcClr+lcWare"

IF USED ('FabDye')
  SELECT FABDYE
  LCOLDTFAD = TAG()
ENDIF
LLOPENFAD =GFOPENFILE(OARIAAPPLICATION.DATADIR+'FabDye','FABDYE','SH')
SET ORDER TO TAG FABDYE IN FABDYE

IF SEEK (LCFAB+LCCLR+LCWARE)
  SCAN WHILE FABRIC+COLOR+CWARECODE = LCFAB+LCCLR+LCWARE FOR !EMPTY(DYELOT)
    LLFOUND = .T.
    EXIT
  ENDSCAN
ENDIF
IF LLFOUND
  LCBRFIELDS = [FabDye.Fabric:14:h="Item",FabDye.Color:9:H="Color",FabDye.Dyelot:h="Dyelot":15,FabDye.OnHand:h="OnHand":12]

  LLWASSEL = ARIABROW(LCKEY+[FOR !EMPTY(DYELOT)],;
    LCTITLE,GNBRFSROW1, GNBRFSCOL1, GNBRFSROW2, GNBRFSCOL2,"","","Fabric,Color,Dyelot","laData")
  LCFAB    = IIF(LLWASSEL, LADATA[1], SPACE(7))
  LCCLR    = IIF(LLWASSEL, LADATA[2], SPACE(6))
  LCDYE    = IIF(LLWASSEL, LADATA[3], SPACE(10))
ELSE
  =GFMODALGEN('TRM36045B36000','ALERT',ALLTRIM(LCFAB)+'\'+ALLTRIM(LCCLR) +;
    IIF(LLMULTIWH, ' in warehouse: '+ ALLTRIM(LCWARE)+'.'  ,;
    ' on the file.' ))
  LCDYE = SPACE(10)
ENDIF

IF LLOPENFAD
  SET ORDER TO TAG (LCOLDTFAD) IN FABDYE
  USE IN FABDYE
ENDIF
SELECT(LNALIAS)
RETURN LCDYE

*!*************************************************************
*! Name      : gfFlock
*! Developer : Yasser Saad Ibrahime
*! Convert By: Reham on 10/31/2002
*! Date      : 1993-1995
*! Purpose   : To lock entir file or files
*!*************************************************************
*! Calls     :
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Name of files
*!                      lock or unlock
*!                      Numvber of attemps
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
* This function will atempt to lock multiple files and let the user to retry
* agian till he select to cancel the operation
* parameters : Files list to lock
*              Flag to lock or unlock the files
*
*:->
FUNCTION GFFLOCK
PARAMETERS LCFLS2LOCK,LLLOKUNLOK,LNTRY

LCFLS2LOCK = IIF(TYPE('lcFls2lock')<>'C',ALIAS(),LCFLS2LOCK)
LLLOKUNLOK = IIF(TYPE('llLokUnlok')='U',.F.,LLLOKUNLOK)
LNTRY      = IIF(TYPE('lnTry')     ='N',LNTRY,-2)
LLRETFLAG  = .F.
LLRETRY    = .F.

*** If no files was sent return with .F.
IF EMPTY(LCFLS2LOCK)
  RETURN LLRETFLAG
ENDIF

*** Put files to be locked in array
DECLARE LAFLS2LOCK[1]

IF ',' $ LCFLS2LOCK
  =GFSUBSTR(LCFLS2LOCK,@LAFLS2LOCK,',')
ELSE
  LAFLS2LOCK[1] = LCFLS2LOCK
ENDIF

SET REPROCESS TO LNTRY

*** Lock one or multiple files
IF LLLOKUNLOK
  *** Keep tring till the user decide to cancle
  DO WHILE .T.
    *** Loop to lock all files to be locked
    FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
      *** If files was locked all return with .T.
      IF FLOCK(LAFLS2LOCK[lnFCount])
        LLRETFLAG = .T.
      ELSE
        *** Give the user message to retry or cancel
        *** Files is in use by another user
        IF GFMODALGEN("QRM00108B00015","ALERT",UPPER(ALLTRIM(LAFLS2LOCK[lnFCount]))) = 1
          LLRETFLAG = .F.
          LLRETRY   = .T.
          *** Exit from the for loop and retry again
          EXIT
        ELSE
          LLRETFLAG = .F.
          LLRETRY   = .F.
          *** Exit from the for loop and quit function
          EXIT
        ENDIF
      ENDIF
    ENDFOR
    *** If at least one files was not locked
    IF !LLRETFLAG
      *** If the user select to retry loop again
      IF LLRETRY
        LOOP
        *** If not quit the function with .F.
      ELSE
        *** If cancel unlock in all alias
        FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
          UNLOCK IN (LAFLS2LOCK[lnFCount])
        ENDFOR
        LLRETFLAG = .F.
        EXIT
      ENDIF
    ELSE
      *** If all lockes went ok terminat the loop
      EXIT
    ENDIF
  ENDDO

  *** Unlock multiple files
ELSE
  FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
    UNLOCK IN (LAFLS2LOCK[lnFCount])
  ENDFOR
  LLRETFLAG = .T.
ENDIF

SET REPROCESS TO -2

RETURN LLRETFLAG

*!*************************************************************
*! Name      : gfRlock
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : Phisycal lock to record in one or more files
*!*************************************************************
*! Calls     :
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Name of files
*!                      Lock or unlock
*!                      Number of attempts
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
* This function will atempt to lock multiple files and let the user to retry
* agian till he select to cancel the operation
* parameters : Files list to lock
*              Flag to lock or unlock the files
*
*:->
FUNCTION GFRLOCK
PARAMETERS LCFLS2LOCK,LLLOKUNLOK,LNTRY

LCFLS2LOCK = IIF(TYPE('lcFls2lock')<>'C',ALIAS(),LCFLS2LOCK)
LLLOKUNLOK = IIF(TYPE('llLokUnlok')='U',.F.,LLLOKUNLOK)
LNTRY      = IIF(TYPE('lnTry')     ='N',LNTRY,GNLOCKTRY)
LLRETFLAG  = .F.
LLRETRY    = .F.

*** If no files was sent return with .F.
IF EMPTY(LCFLS2LOCK)
  RETURN LLRETFLAG
ENDIF

*** Put files to be record locked in array
DECLARE LAFLS2LOCK[1]
IF ',' $ LCFLS2LOCK
  =GFSUBSTR(LCFLS2LOCK,@LAFLS2LOCK,',')
ELSE
  LAFLS2LOCK[1] = LCFLS2LOCK
ENDIF


SET REPROCESS TO LNTRY

*** Lock one or multiple files
IF LLLOKUNLOK
  *** Keep tring till the user decide to cancle
  DO WHILE .T.
    *** Loop to lock all files to be locked
    FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
      *** If files was locked all return with .T.
      IF RLOCK(LAFLS2LOCK[lnFCount])
        LLRETFLAG = .T.
      ELSE
        *** Give the user message to retry or cancel
        *** Files is in use by another user
        IF GFMODALGEN("QRM00109B00015","ALERT",UPPER(ALLTRIM(LAFLS2LOCK[lnFCount]))) = 1
          LLRETFLAG = .F.
          LLRETRY   = .T.
          *** Exit from the for loop and retry again
          EXIT
        ELSE
          LLRETFLAG = .F.
          LLRETRY   = .F.
          *** Exit from the for loop and quit function
          EXIT
        ENDIF
      ENDIF
    ENDFOR
    *** If at least one files was not locked
    IF !LLRETFLAG
      *** If the user select to retry loop again
      IF LLRETRY
        LOOP
        *** If not quit the function with .F.
      ELSE
        *** If cancel unlock in all alias
        FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
          UNLOCK IN (LAFLS2LOCK[lnFCount])
        ENDFOR
        LLRETFLAG = .F.
        EXIT
      ENDIF
    ELSE
      *** If all lockes went ok terminat the loop
      EXIT
    ENDIF
  ENDDO

  *** Unlock multiple files
ELSE
  FOR LNFCOUNT =  1 TO ALEN(LAFLS2LOCK,1)
    UNLOCK IN (LAFLS2LOCK[lnFCount])
  ENDFOR
  LLRETFLAG = .T.
ENDIF

SET REPROCESS TO GNLOCKTRY

RETURN LLRETFLAG


*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Modified  : Hesham El_Sheltawi
*! Date      : 11/12/2002
*! Purpose   : To object lock any record in any file
*!*************************************************************
*! Calls     :
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : flage to lock or unlock
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFOBJ_LOCK
PARAMETERS LLOK_SET
PRIVATE LNRECNO,LRET_FLAG,LNOLDRPST
LOCAL LCSELALIAS,LNDATASESSION,LLBUFFERMODE
LNDATASESSION = SET("DATASESSION")
SET DATASESSION TO (LNDATASESSION)
LCSELALIAS = ALIAS()
IF EMPTY(LCSELALIAS)
  RETURN
ENDIF

SELECT (LCSELALIAS)
LLBUFFERMODE = CURSORGETPROP("Buffering") >= 4

LRET_FLAG = .F.
LLOK_IT   = .F.
LLLOCKED  = .F.
*** Go to the same record to get a fresh copy in the buffer
LNRECNO = RECNO()

DO WHILE .T.
  SELECT (LCSELALIAS)
  IF LNRECNO <= RECCOUNT()
    GO LNRECNO
    LLLOCKED = RLOCK()
    IF LLLOCKED AND LLBUFFERMODE
      TABLEREVERT(.F.)
    ENDIF
    IF DELETED()
      UNLOCK
      =GFMODALGEN('INM00095B00000','ALERT')
      SELECT (LCSELALIAS)

      RETURN .F.
    ENDIF
  ENDIF

  *** Chek if the record is in use by another user
  IF LLOK_SET
    *** Chek if the field cLok_User in the structur
    IF !LLOK_STAT .AND. LLLOCKED
      *** Record is not locked you may lock it
      LLOK_IT   = .T.
    ELSE
      LCLOK_USER = CLOK_USER
      IF !EMPTY(LCLOK_USER)
        IF ALLTRIM(LCLOK_USER) = ALLTRIM(OARIAAPPLICATION.USER_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          IF GFMODALGEN("INM00240B00006","ALERT")=2
            LLOK_IT    = .F.
            LRET_FLAG  = .F.
          ELSE
            LLOK_IT    = .T.
          ENDIF
        ELSE

          *We save old value of reprocess first.[START]
          LNOLDRPST = SET('REPROCESS')
          SET REPROCESS TO 1

          SET DATASESSION TO 1
          LLLOOP = .F.
          SELECT SYUSTATC
          IF SEEK ('INI'+'OLDVARS'+LCLOK_USER,'syuStatc')
            SCAN REST WHILE COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID = 'INI'+'OLDVARS'+LCLOK_USER
              IF RLOCK('syuStatc')
                UNLOCK IN  SYUSTATC
                GO (OARIAAPPLICATION.USERSTATICRECORD) IN SYUSTATC
                =RLOCK('syuStatc')
                LLOK_IT    = .T.
              ELSE
                UNLOCK
                GO (OARIAAPPLICATION.USERSTATICRECORD) IN SYUSTATC
                =RLOCK('syuStatc')
                *** Display the message "Record is in use by user AAAA"
                LCLOK_USER = OARIAAPPLICATION.GETUSERNAME(LCLOK_USER)
                *** Record is in use by user ????
                SET DATASESSION TO (LNDATASESSION)
                IF  GFMODALGEN("INM00028B00015","ALERT",LCLOK_USER) = 1
                  LLLOOP = .T.
                ENDIF
                LLOK_IT    = .F.
                LRET_FLAG  = .F.
                EXIT
              ENDIF
            ENDSCAN
          ELSE
            LLOK_IT    = .T.
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  LNOLDRPST
          SET DATASESSION TO (LNDATASESSION)
          IF LLLOOP
            LOOP
          ENDIF

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (LNDATASESSION)
        IF GFMODALGEN("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        LLOK_IT    = .F.
        LRET_FLAG  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"

      *** Unlock the record
      REPLACE LLOK_STAT WITH .F. , ;
        CLOK_USER WITH ""  , ;
        DLOK_DATE WITH {}  , ;
        CLOK_TIME WITH ""
      IF LLBUFFERMODE
        =TABLEUPDATE(0,.T.)
      ENDIF
      LRET_FLAG  = .T.
    ENDIF
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (LNDATASESSION)
IF LLOK_IT
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for this user with date and time
    REPLACE LLOK_STAT WITH .T.       , ;
      CLOK_USER WITH OARIAAPPLICATION.USER_ID , ;
      DLOK_DATE WITH DATE()    , ;
      CLOK_TIME WITH GFGETTIME()
    IF LLBUFFERMODE
      =TABLEUPDATE(0,.T.)
    ENDIF
    LRET_FLAG  = .T.
  ENDIF
ENDIF
SELECT (LCSELALIAS)
UNLOCK


RETURN LRET_FLAG

*!*****************************************************************************************
*! Name      : CDateToString
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/26/2002 01:45:04 AM
*! Purpose   : Changes the char date to string date.
*! Entry no. :
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION CDATETOSTRING
LPARAMETERS CCHARDATE
IF (VARTYPE(CCHARDATE) != "C") OR EMPTY(CCHARDATE)
  RETURN ""
ENDIF
LOCAL LDDATE, LLERROR, LCOLDERROR
LCOLDERROR = ON("ERROR")
ON ERROR LLERROR = .T.
LDDATE = CTOD(ALLTRIM(CCHARDATE))
ON ERROR &LCOLDERROR.
IF LLERROR OR (VARTYPE(LDDATE) != "D")
  RETURN ""
ENDIF
RETURN DTOS(LDDATE)
ENDFUNC

*!*************************************************************
*! Name      : gfFillPop
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : To fill any popup from an array
*!*************************************************************
*! Calls     :
*!      Called by: GFACTPOP()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Popup name
*!                      Array name
*!                      Colums number to be used in filling
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*:->
FUNCTION GFFILLPOP
PARAMETERS LCPOPNAM,LAPOPARAY,LNCOLNUM
LNCOLNUM =IIF(TYPE('lnColNum')<>'N',1,LNCOLNUM)

RELEASE BAR ALL OF &LCPOPNAM


FOR LNCOUNT = 1 TO ALEN(&LAPOPARAY,1)
  DEFINE BAR LNCOUNT OF &LCPOPNAM ;
    PROMPT (ALLTRIM(&LAPOPARAY[lnCount,lnColNum]))
ENDFOR


*!*************************************************************
*! Name      : gfPopArang
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995
*! Purpose   : To rearrange array from any popup
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : Popup name
*!                      array name
*!                      array colum to use in sort
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
*  Collect new arrangement of popup
*
*:->
FUNCTION GFPOPARANG
PARAMETERS LCPOPNAM,LAARRAY,LNPOPUPROW

LNPOPUPROW = IIF(TYPE('lnPopUpRow')<>'N',1,LNPOPUPROW)

IF ALEN(LAARRAY,2) <= 1
  DECLARE LAARRAY[CNTBAR(lcPopNam),1]

  FOR LNCOUNT = 1 TO CNTBAR(LCPOPNAM)
    LAARRAY[lnCount]=PRMBAR(LCPOPNAM,GETBAR(LCPOPNAM,LNCOUNT))
  ENDFOR
ELSE
  DECLARE LATMPARY [ALEN(laArray,1),ALEN(laArray,2)]
  =ACOPY(LAARRAY,LATMPARY)

  FOR LNCOUNT = 1 TO CNTBAR(LCPOPNAM)

    LNSOURCROW = ASCAN(LATMPARY,PRMBAR(LCPOPNAM,GETBAR(LCPOPNAM,LNCOUNT)))

    IF LNSOURCROW > 0
      LNSOURCROW = ASUBSCRIPT(LATMPARY,LNSOURCROW,1)

      FOR  LNCOLUM = 1 TO ALEN(LATMPARY,2)
        LAARRAY[lnCount,lnColum] = LATMPARY[lnSourcRow,lnColum]
      ENDFOR

    ENDIF

  ENDFOR

ENDIF

*!*****************************************************************************************
*! Name      : DateRng
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/26/2002 01:45:04 AM
*! Purpose   : Date Range Functionality
*! Entry no. :
*!*****************************************************************************************
*!
FUNCTION DATERNG
DO FORM DATERNG
RETURN .T.
ENDFUNC
*-- end of DateRng.

*--E000000,1 Hesham (Start)
*!*****************************************************************************************
*! Name      : gfLockSys
*! Developer : Hesham - Hesham El Sheltawi
*! Date      : 04/15/2003
*! Purpose   : Lock the system to prevent any user to log into the system
*! Entry no. : E000000,1
*!*****************************************************************************************
*!
FUNCTION GFLOCKSYS
*E303425,1 TMI 10/30/2013 [Start] add a new parameter
PARAMETERS llCalledFrmScx
*E303425,1 TMI 10/30/2013 [End  ] 
LOCAL LLLOCKSYS,LCCURALIAS,LLSUCCESS
LLLOCKSYS = !GLLOCKSYS
*E303425,1 TMI 10/31/2013 [Start] use select() instead of alias()
*LCCURALIAS = ALIAS()
LCCURALIAS = SELECT(0)
*E303425,1 TMI 10/31/2013 [End  ] 
LLSUCCESS = .F.
LNBAR = BAR()
LCPOP = POPUP()

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

SELECT SYCINST
IF LLLOCKSYS
  IF RLOCK("SYCINST")
    REPLACE LLOCKSYS WITH .T.
    *E303425,1 TMI 10/30/2013 [Start] comment out
    *FLUSH()
    *E303425,1 TMI 10/30/2013 [End  ] 
    LLSUCCESS = .T.
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *MESSAGEBOX(LANG_LogMSG,16,THIS.systemname)
    MESSAGEBOX(IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LOGMSG,OARIAAPPLICATION.GETHEADERTEXT("LANG_LogMSG",LCARIAHFILE)),16,THIS.SYSTEMNAME)
    *N000682,1 11/20/2012 MMT Globlization changes[End]
  ENDIF
ELSE
  IF RLOCK("SYCINST")
    REPLACE SYCINST.LLOCKSYS WITH .F.
    UNLOCK IN SYCINST
    LLSUCCESS = .T.
  ENDIF
ENDIF
SELECT (LCCURALIAS)
*E303425,1 TMI 10/30/2013 [Start] use the new parameter
*IF LLSUCCESS
IF LLSUCCESS AND !llCalledFrmScx
*E303425,1 TMI 10/30/2013 [End  ] 
  GLLOCKSYS = SYCINST.LLOCKSYS
  SET MARK OF BAR LNBAR OF (LCPOP) GLLOCKSYS
ENDIF

*E303425,1 TMI 10/30/2013 [Start] return a value
RETURN LLSUCCESS
*E303425,1 TMI 10/30/2013 [End  ] 
*--E000000,1 Hesham (End)




*!********************************************************************
*! Name      : gfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp.
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!           : lcGetFile    detect which alias we get currency values from it.
*!           : lcGetField   detect which Field we get currency values from it.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
*E301214,1   : Transfer this function from AP Module and AR Module to be a global function.
**********
FUNCTION GFAMNTDISP
PARAMETER LNAMOUNT,LCRPDISPCUR,LDEXRATEDT,LCTMEPFILE,LLAPRVCURR,LCGETFILE,LCGETFIELD
PRIVATE LNAMOUNT,LCRPDISPCUR,LDEXRATEDT,LCTMEPFIL,LLAPRVCURR,LCEXSIN1,LCEXSIN2,LNSAVALIAS,LCGETFIELD

LNAMOUNT    = IIF(TYPE('lnAmount') = 'N',LNAMOUNT,0)
LCRPDISPCUR = IIF(TYPE('lcRpDispCur') ='C',LCRPDISPCUR,'')
LDEXRATEDT  = IIF(TYPE('ldExRateDt') = 'D',LDEXRATEDT,{})
LCTMEPFILE  = IIF(TYPE('lcTmepFile') = 'C',LCTMEPFILE,'')
LLAPRVCURR  = IIF(TYPE('llAprvCurr') = 'L',LLAPRVCURR,.F.)

LCGETFILE   = IIF(TYPE('lcGetFile')$"UL",'',LCGETFILE)
LCGETFIELD  = IIF(TYPE('lcGetField') ='C',LCGETFIELD,'')

LCEXSIN1    = ''       && Variable to hold the first sign in the equation.
LCEXSIN2    = ''       && Variable to hold the second sign in the equation.
LNSAVALIAS  = SELECT(0)  && Variable to save the alias.
DO CASE
CASE LCRPDISPCUR = 'F'

CASE LCRPDISPCUR = 'O'
  IF EMPTY(LCGETFILE)
    LCCURRCODE = IIF(LLAPRVCURR,CAPRCURCOD,CCURRCODE)
  ELSE
    *Get filed currency if send .
    LCCURRCODE = IIF(LLAPRVCURR,&LCGETFILE..CAPRCURCOD,;
      IIF(EMPTY(LCGETFIELD),&LCGETFILE..CCURRCODE,EVAL(LCGETFILE+'.'+LCGETFIELD)))
  ENDIF
  LCEXSIN2   = ' '
  LCEXSIN1   = GFGETEXSIN(@LCEXSIN2,LCCURRCODE)
  LNEXRATE = 0
  IF EMPTY(LCGETFILE)
    LNUNIT = NCURRUNIT
    LNEXRATE = IIF(LLAPRVCURR , GFCHKRATE('lnUnit' , LCCURRCODE , DINVDATE , .F.) , NEXRATE)
  ELSE
    LNUNIT = &LCGETFILE..NCURRUNIT
    LNEXRATE = IIF(LLAPRVCURR , GFCHKRATE('lnUnit' , LCCURRCODE , &LCGETFILE..DINVDATE , .F.) , &LCGETFILE..NEXRATE)
  ENDIF
  LNEXRATE = IIF(LNEXRATE <> 0 , LNEXRATE , 1)
  LNUNIT = IIF(LNUNIT <> 0 , LNUNIT , 1)
  LNAMOUNT   = ROUND(LNAMOUNT &LCEXSIN1 LNEXRATE &LCEXSIN2 LNUNIT , 2)

CASE LCRPDISPCUR = 'D'
  LNEXRATE   = 0
  LNUNIT     = 0
  IF EMPTY(LCGETFILE)
    LCCURRCODE = IIF(LLAPRVCURR,CAPRCURCOD,CCURRCODE)
  ELSE
    LCCURRCODE = IIF(LLAPRVCURR,&LCGETFILE..CAPRCURCOD,;
      IIF(EMPTY(LCGETFIELD),&LCGETFILE..CCURRCODE,EVAL(LCGETFILE+'.'+LCGETFIELD)))
  ENDIF
  IF LCCURRCODE = OARIAAPPLICATION.BASECURRENCY
    LNEXRATE = 1
    LNUNIT   = 1
  ELSE
    LNEXRATE   = GFCHKRATE('lnUnit',LCCURRCODE,LDEXRATEDT,.F.)
  ENDIF
  LNEXRATE = IIF(LNEXRATE <> 0 , LNEXRATE , 1)
  LNUNIT = IIF(LNUNIT <> 0 , LNUNIT , 1)
  LCEXSIN2   = ' '
  LCEXSIN1   = GFGETEXSIN(@LCEXSIN2,LCCURRCODE)
  LNAMOUNT   = ROUND(LNAMOUNT &LCEXSIN1 LNEXRATE &LCEXSIN2 LNUNIT,2)

CASE LCRPDISPCUR = 'N'
  LNEXRATE   = 0
  LNUNIT     = 0
  IF EMPTY(LCGETFILE)
    LCCURRCODE = IIF(LLAPRVCURR,CAPRCURCOD,CCURRCODE)
  ELSE
    LCCURRCODE = IIF(LLAPRVCURR,&LCGETFILE..CAPRCURCOD,;
      IIF(EMPTY(LCGETFIELD),&LCGETFILE..CCURRCODE,EVAL(LCGETFILE+'.'+LCGETFIELD)))
  ENDIF
  IF LCCURRCODE = OARIAAPPLICATION.BASECURRENCY
    LNEXRATE = 1
    LNUNIT   = 1
  ELSE
    IF SEEK(LCCURRCODE,LCTMEPFILE)
      LNEXRATE = &LCTMEPFILE..NEXRATE
      LNUNIT   = &LCTMEPFILE..NCURRUNIT
    ENDIF
  ENDIF
  LNEXRATE = IIF(LNEXRATE <> 0 , LNEXRATE , 1)
  LNUNIT = IIF(LNUNIT <> 0 , LNUNIT , 1)
  LCEXSIN2   = ' '
  LCEXSIN1   = GFGETEXSIN(@LCEXSIN2,LCCURRCODE)
  LNAMOUNT   = ROUND(LNAMOUNT &LCEXSIN1 LNEXRATE &LCEXSIN2 LNUNIT,2)
ENDCASE
SELECT (LNSAVALIAS)
RETURN LNAMOUNT

*-- end of gfAmntDisp.

*!***************************************************************************
*! Name      : gfGetSeq
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2003
*! Purpose   : Get the next sequence number.
*!***************************************************************************
*! Example   : =gfGetSeq()
*!***************************************************************************
*! Notes     : This function was written by MAN.
*!***************************************************************************
*B606902,1
FUNCTION GFGETSEQ
PARAMETER LNSEQ,CCHR
PRIVATE LCSPREFIX , LNCHARASCI , LNI , LCALIAS
LCALIAS = ALIAS()
SELECT SEQUENCE
LCSPREFIX = ""
IF !(CCHR = CHR(0))
  LNCHARASCI = ASC(CCHR)
  IF MOD(LNCHARASCI,26) = 0
    LCCHAR = "Z"
    LNCHARPOS = LNCHARASCI/26
  ELSE
    LNCHARPOS = INT(LNCHARASCI/26)+1
    LCCHAR =  CHR(MOD(LNCHARASCI,26)+64)
  ENDIF
  FOR LNI = 1 TO LNCHARPOS - 1
    LCSPREFIX = LCSPREFIX + "Z"
  ENDFOR
  LCSPREFIX = LCSPREFIX + LCCHAR

ELSE
  LCCHAR =""
ENDIF
IF LNSEQ = VAL(REPLICATE("9",LNRETLEN-LEN(LCSPREFIX)))
  IF EMPTY(CCHR)
    LCSPREFIX = "A"
    *REPLACE cSeq_Chr WITH CHR(1)
    LCCHRTOUPD = CHR(1)
  ELSE
    IF LCCHAR = "Z"
      LCSPREFIX = LCSPREFIX + "A"
    ELSE
      LCSPREFIX = LEFT(LCSPREFIX,LEN(LCSPREFIX)-1) + CHR(ASC(LCCHAR)+1)
    ENDIF
    *REPLACE cSeq_Chr WITH CHR(ASC(cSeq_Chr)+1)
    LCCHRTOUPD = CHR(ASC(CSEQ_CHR)+1)
  ENDIF
  LNSEQ = 0
ELSE
  LNSEQ = LNSEQ + 1
ENDIF

*lnSeq = lnSeq + 1
*REPLACE nSeq_No with lnSeq
*? lcSPrefix+PADL(lnSeq,lnRetLen-LEN(lcSPrefix),"0")

LNRETVAL = LNSEQ
LCEXTRASTR = LCSPREFIX

IF !EMPTY(LCALIAS)
  SELECT (LCALIAS)
ENDIF
*-- End of gfGetSeq.

*!*************************************************************
*! Name      : IsValidMail
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 07/10/2003
*! Purpose   : Validate an email string.
*!*************************************************************
*! Passed Parameters  : strMail   (E-Mail String)
*!*************************************************************
*! Return             : True if valid email.
*!*************************************************************
*!
FUNCTION ISVALIDMAIL
LPARAMETERS STRMAIL
IF VARTYPE(STRMAIL) != "C"
  RETURN .F.
ENDIF

STRMAIL = ALLTRIM(STRMAIL)
*-- Empty or has a space.
IF (LEN(STRMAIL) = 0) OR (ATC(" ",STRMAIL) > 0) THEN
  RETURN .F.
ENDIF

LOCAL INTAT AS INTEGER
INTAT = ATC("@", STRMAIL)

*-- If Not found or it's the first character or it's the last character.
IF (INTAT <= 1) OR (INTAT = LEN(STRMAIL)) THEN
  RETURN .F.
ENDIF

*! B127465,1 SMM 05/08/2005 Invalid Email [START]
*-- If the dot is before the @
*!*	  If ATC(".", strMail) < intAt Then
*!*	    RETURN .F.
*!*	  EndIf
*! B127465,1 SMM 05/08/2005 Invalid Email [End]
STRMAIL = SUBSTR(STRMAIL, INTAT + 1)

*-- If still have another @
IF ATC("@", STRMAIL) > 0 THEN
  RETURN .F.
ENDIF

INTAT = ATC(".", STRMAIL)
*-- If It's the first character after @ or not exists or it's the last character.
IF (INTAT <= 1) OR (INTAT = LEN(STRMAIL)) THEN
  RETURN .F.
ENDIF

*-- Validate the last characters.
STRMAIL = SUBSTR(STRMAIL,RAT(".",STRMAIL)+1)
* B611219,1 MMT 10/24/2016 Email Validation function considers .info email is invalid[T20161019.0039][Start]
*IF !BETWEEN(LEN(STRMAIL),2,3)  && (.Com, .Org, or .eg)
IF !BETWEEN(LEN(STRMAIL),2,4)  && (.Com, .Org, .info, or .eg)
* B611219,1 MMT 10/24/2016 Email Validation function considers .info email is invalid[T20161019.0039][End]
  RETURN .F.
ENDIF
RETURN .T.  && Pass the validation.
ENDFUNC     && end of IsValidMail.

*!*************************************************************
*! Name       : gfCstShtBrow
*! Developer  : AHMED MAHER (AMH)
*! Date       : 01/04/2004
*! Purpose    : Browse the cost sheet ID.
*!*************************************************************
*! Parameters : lcKeyValue = Filter Key
*!              lcFile_Ttl = Browse Title
*!              lcCstShtID = Variable to Hold the selected cost sheet ID
*!              lcBomHeadr = Alias name of the BOMHEADR cursor
*!              lnCstHeadr = 1 for Imported
*!                           2 for Manufactured
*!                           3 for Imported and Manufactured
*!                           4 for Material
*!*************************************************************
*! Return     : .F. if there is no cost sheet selected or .T. if there is a selected Cost sheet ID
*!*************************************************************
*! N037377,1 AMH
FUNCTION GFCSTSHTBROW
LPARAMETERS LCKEYVALUE,LCFILE_TTL,LCCSTSHTID,LCBOMHEADR,LNCSTHEADR

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
LOCAL LLRET,LNALIAS,LNCOUNT,LCCOUNT
LNALIAS = SELECT(0)
SELECT (LCBOMHEADR)

DIMENSION LACSTLBL[7]

IF BITAND(1,LNCSTHEADR) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE LAMAINSETP[7,2]
  FOR LNCOUNT = 1 TO 7
    LCCOUNT = STR(LNCOUNT,1)
    LAMAINSETP[lnCount,1] = 'M_CISLBL'+LCCOUNT
  ENDFOR
  =GFGETMEMVAR(@LAMAINSETP, OARIAAPPLICATION.ACTIVECOMPANYID)
  FOR LNCOUNT = 1 TO 7
    LACSTLBL[lnCount] = LAMAINSETP[lnCount,2]
  ENDFOR
ENDIF

IF BITAND(2,LNCSTHEADR) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE LAMAINSETP[7,2]
  FOR LNCOUNT = 1 TO 7
    LCCOUNT = STR(LNCOUNT,1)
    LAMAINSETP[lnCount,1] = 'M_CMSLBL'+LCCOUNT
  ENDFOR
  =GFGETMEMVAR(@LAMAINSETP, OARIAAPPLICATION.ACTIVECOMPANYID)
  FOR LNCOUNT = 1 TO 7
    LACSTLBL[lnCount] = IIF(EMPTY(LACSTLBL[lnCount]),'',LACSTLBL[lnCount]+CHR(10)) + LAMAINSETP[lnCount,2]
  ENDFOR
ENDIF

IF BITAND(4,LNCSTHEADR) # 0
  *--- Array hold the memory variables that need to be load from the company setup file.
  DECLARE LAMAINSETP[4,2]
  FOR LNCOUNT = 1 TO 4
    LCCOUNT = STR(LNCOUNT,1)
    LAMAINSETP[lnCount,1] = 'M_CTSLBL'+LCCOUNT
  ENDFOR
  =GFGETMEMVAR(@LAMAINSETP, OARIAAPPLICATION.ACTIVECOMPANYID)
  FOR LNCOUNT = 1 TO 4
    LACSTLBL[lnCount] = LAMAINSETP[lnCount,2]
  ENDFOR
ENDIF

DIMENSION LATEMPDATA[1]
STORE '' TO LATEMPDATA

LCBRFIELDS =                     "CITMMAJOR  :H='"+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CITMMAJOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CITMMAJOR",LCARIAHFILE))+"'  ," +;
  "CCSTSHT_ID :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CCSTSHT_ID,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CCSTSHT_ID",LCARIAHFILE))+"' ," +;
  "CCSTSHTDSC :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CCSTSHTDSC,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CCSTSHTDSC",LCARIAHFILE))+"' ," +;
  "LDEFCSTSHT :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_LDEFCSTSHT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_LDEFCSTSHT",LCARIAHFILE))+"' ," +;
  "CSTATUS    :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CSTATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CSTATUS",LCARIAHFILE))+"'    ," +;
  "LBASONSIZ  :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_LBASONSIZ,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_LBASONSIZ",LCARIAHFILE))+"'  ," +;
  "NCOST1     :H='"+ALLTRIM(LACSTLBL[1])+"' ," +;
  "NCOST2     :H='"+ALLTRIM(LACSTLBL[2])+"' ," +;
  "NCOST3     :H='"+ALLTRIM(LACSTLBL[3])+"' ," +;
  "NCOST4     :H='"+ALLTRIM(LACSTLBL[4])+"' ," +;
  IIF(LNCSTHEADR=4,"","NCOST5     :H='"+ALLTRIM(LACSTLBL[5])+"' ,")+;
  IIF(LNCSTHEADR=4,"","NCOST6     :H='"+ALLTRIM(LACSTLBL[6])+"' ,")+;
  IIF(LNCSTHEADR=4,"","NCOST7     :H='"+ALLTRIM(LACSTLBL[7])+"' ,")+;
  "TOTCOST    :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTCOST,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TOTCOST",LCARIAHFILE))+"'    ," +;
  "CCSTSHTTYP :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CCSTSHTTYP,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_CCSTSHTTYP",LCARIAHFILE))+"'"

LLRET = GFBROWS(LCKEYVALUE,'CCSTSHT_ID','laTempData',LCFILE_TTL,.F.)
IF LLRET
  LCCSTSHTID = LATEMPDATA[1]
ENDIF
SELECT (LNALIAS)

RETURN LLRET
*-- end of gfCstShtBrow.

*!*************************************************************
*! Name : gfOpnSqlFl (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To open the SQL files.
*!*************************************************************
*! Passed :
*!        Parameters :
*!		  lcTable     -> SQL table
*!		  lcCursor    -> Cursor to hold the SQL table
*!		  lcWhereCond -> Where condition to be used in the select statement
*!		  laIndex     -> Index on the cursor
*!		  lcTagName   -> Tag name
*!*************************************************************
*! Returned :
*!*************************************************************
*! Example :
*!        DO gfOpnSqlFl WITH "ITEM",lcTmpFile,@laIndex,"Style"
*!*************************************************************
*!*	FUNCTION gfOpnSqlFl
*!*	LPARAMETERS lcTable,lcCursor,lcWhereCond,laIndex,lcTagName

*!*	LOCAL lnConnectionHandlar, lcODBC, lcUserName, lcPassWord, lnBuffering, lcSqlStatment

*!*	lcSqlStatment = "SELECT * FROM " + lcTable + " (INDEX="+lcTagName+")" + " WHERE "+lcWhereCond
*!*	lcODBC        = 'DS'+oAriaApplication.ActiveCompanyID
*!*	lcUserName    = 'sa'
*!*	lcPassWord    = ''

*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
*!*	                                      'SAVE',SET("DATASESSION"))
*!*	IF lnConnectionHandlar = 1
*!*	  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
*!*	  =CURSORSETPROP("Buffering",3,lcCursor)
*!*	  IF !EMPTY(laIndex)
*!*	    FOR lnCntr = 1 TO ALEN(laIndex,1)
*!*	      lcIndex = laIndex[lnCntr,1]
*!*	      lcTag   = laIndex[lnCntr,2]
*!*	      lcUnique = laIndex[lnCntr,3]
*!*	      IF TYPE("lcUnique") = "C"
*!*	        INDEX ON &lcIndex. &lcUnique. TAG (lcTag) OF (lcCursor)
*!*	      ELSE
*!*	        INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
*!*	      ENDIF
*!*	    ENDFOR
*!*	    lcTag = laIndex[1,2]
*!*	    SET ORDER TO TAG (lcTag)
*!*	  ENDIF
*!*	  =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
*!*	ELSE
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
*!*	  RETURN .F.
*!*	ENDIF

FUNCTION GFOPNSQLFL

*!B999999,1 WSH 02/07/2005, Add Parameter to allow opening Native Files Remotely. [Start]
*LPARAMETERS lcTable,lcCursor,lcWhereCond,laIndex,lcTagName
LPARAMETERS LCTABLE, LCCURSOR, LCWHERECOND, LAINDEX, LCTAGNAME, LLNATIVE
*!B999999,1 WSH 02/07/2005, [End]

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*--Get Connection String Type
IF LLNATIVE
  LCCONNSTRING = OARIAAPPLICATION.CARIANATIVEDATAFILESCONSTR
ELSE
  LCCONNSTRING = OARIAAPPLICATION.ACTIVECOMPANYCONSTR
ENDIF
*!B999999,1 WSH 02/07/2005, [End]

LOCAL LNCONNECTIONHANDLAR, LCODBC, LCUSERNAME, LCPASSWORD, LNBUFFERING, LCSQLSTATMENT

*!B999999,1 WSH 02/07/2005, Don't use index if it is not passed. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + " (INDEX="+lcTagName+")" + " WHERE "+lcWhereCond

*B131608,1 WSH 05/03/2006 No "Index" if it is native. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + IIF(TYPE("lcTagName") = 'C' AND !EMPTY(lcTagName), " (INDEX="+lcTagName+")", "") + " WHERE "+ lcWhereCond
LCSQLSTATMENT = "SELECT * FROM " + LCTABLE + IIF(!LLNATIVE AND TYPE("lcTagName") = 'C' AND !EMPTY(LCTAGNAME), " (INDEX="+LCTAGNAME+")", "") + " WHERE "+ LCWHERECOND
*B131608,1 WSH 05/03/2006 [End]

*!B999999,1 WSH 02/07/2005, [End]

LCODBC        = 'DS'+OARIAAPPLICATION.ACTIVECOMPANYID
LCUSERNAME    = 'sa'
LCPASSWORD    = ''

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
'SAVE',SET("DATASESSION"))

*B131608,1 WSH 05/03/2006 Pass the Table Name to SQLRun method. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,"",lcConnString,3,;
'SAVE',SET("DATASESSION"))
LNCONNECTIONHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSQLSTATMENT,LCCURSOR,LCTABLE,LCCONNSTRING,3,;
  'SAVE',SET("DATASESSION"))
*B131608,1 WSH 05/03/2006 [Start]

*!B999999,1 WSH 02/07/2005, [End]

IF LNCONNECTIONHANDLAR = 1
  LNBUFFERING = CURSORGETPROP("Buffering",LCCURSOR)
  =CURSORSETPROP("Buffering",3,LCCURSOR)
  IF !EMPTY(LAINDEX)
    FOR LNCNTR = 1 TO ALEN(LAINDEX,1)
      LCINDEX = LAINDEX[lnCntr,1]
      LCTAG   = LAINDEX[lnCntr,2]
      LCUNIQUE = LAINDEX[lnCntr,3]
      IF TYPE("lcUnique") = "C"
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[Start]
        *INDEX ON &lcIndex. &lcUnique. TAG (lcTag) OF (lcCursor)
        INDEX ON &LCINDEX. &LCUNIQUE. TAG (LCTAG)
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[End]
      ELSE
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[Start]
        *INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
        INDEX ON &LCINDEX. TAG (LCTAG)
        *! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[ENd]
      ENDIF
    ENDFOR
    LCTAG = LAINDEX[1,2]
    SET ORDER TO TAG (LCTAG)
  ENDIF
  =CURSORSETPROP("Buffering",LNBUFFERING,LCCURSOR)
ELSE
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("sqlrun",LNCONNECTIONHANDLAR,.T.)
  RETURN .F.
ENDIF


*!*************************************************************
*! Name : gfUpdSqlFl (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To update the SQL tables
*!*************************************************************
*! Passed :
*!        Parameters :
*!        lcTable 		   -> SQL table
*!        LcPrimaryKeyList -> List of the index fields.
*!        lcSQLTable 	   -> SQL table
*!*************************************************************
*! Example :
*!        DO gfUpdSqlFl WITH "ITEM","cInvType,Style"
*!*************************************************************
FUNCTION GFUPDSQLFL
LPARAMETERS LCTABLE,LCPRIMARYKEYLIST, LCSQLTABLE

LOCAL LNCONNECTIONHANDLAR, LCTRANCODE, LCODBC, LCUSERNAME, LCPASSWORD, LLRETURN

LCODBC     = 'DS'+OARIAAPPLICATION.ACTIVECOMPANYID
LCUSERNAME = 'sa'
LCPASSWORD = ''
LLRETURN   = .T.

LCTRANCODE = OARIAAPPLICATION.REMOTECOMPANYDATA.BEGINTRAN(OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'')

IF TYPE('lcTranCode') = 'N'
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("BeginTran",LCTRANCODE,.T.)
  IF BETWEEN(LNRECNO,1,RECCOUNT(LCTABLE))
    GOTO LNRECNO IN (LCTABLE)
  ENDIF
  RETURN .F.
ENDIF

LNCONNECTIONHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLUPDATE(LCTABLE,LCTRANCODE,SET("DATASESSION"),LCPRIMARYKEYLIST,LCSQLTABLE)
IF LNCONNECTIONHANDLAR # 1 .AND. LNCONNECTIONHANDLAR # 2
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("sqlupdate",LNCONNECTIONHANDLAR,.T.)
  LLRETURN = .F.
ENDIF

LNCONNECTIONHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.COMMITTRAN(LCTRANCODE)
IF LNCONNECTIONHANDLAR # 1
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("CommitTran",LNCONNECTIONHANDLAR,.T.)
  LLRETURN = .F.
ENDIF
RETURN LLRETURN

*!*************************************************************
*! Name : gfAdItemWar  (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : This function will add a record to the new ITEMLOC
*!*************************************************************
*! Passed :
*!        Parameters :
*!        lcInvtype -> "0001" for Style, "0002" for Fabric (For example)
*!        lcItem -> Fabric/Color or Style code
*!        lcDyelot -> Dye lot
*!        lcWareH -> Warehouse
*!        lcTmpScope -> To be replaced in the DYE_REL. cTmpScope.
*!*************************************************************
*! Returned :
*!*************************************************************
*! Example :
*!        DO gfAdItemWar WITH lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION GFADITEMWAR
LPARAMETERS LCINVTYPE, LCITEM, LCDYELOT, LCWAREH, LCTMPSCOPE
PRIVATE LCDESC, LCALIAS, LNCOST , LCDISCCODS, LCGLCODE, LCITEMTRAN
LCDESC = ""
LCALIAS = ALIAS()
STORE 0 TO LNCOST , LCDISCCODS
IF TYPE('lcTmpScope') $ 'UL'
  LCTMPSCOPE = ''
ENDIF

IF TYPE("lcTmpItem") $ "UL" OR !USED(LCTMPITEM)
  PRIVATE LCTMPITEM
  LCTMPITEM = GFTEMPNAME()
ENDIF
IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(LCTMPITEMLOC)
  PRIVATE LCTMPITEMLOC
  LCTMPITEMLOC = GFTEMPNAME()
ENDIF

*-- To get the item description, average cost and discount code to replace it in the new
*-- added record in the ItemLoc file.
=GFOPNSQLFL('ITEM',LCTMPITEM,;
  "CINVTYPE = '" + LCINVTYPE + "'" + " AND Style ='" + LCITEM + "'","","Style")
SELECT (LCTMPITEM)
LOCATE
LCDESC     = DESC
LNCOST     = AVE_COST
LCDISCCODS = CDISCCODE
LCGLCODE   = LINK_CODE
*N119813,1 AMH Open the warehouse file if not opened yet [Start]
LOCAL LLOPENWARE
LLOPENWARE = .F.
IF !USED('WareHous')
  LLOPENWARE = GFOPENFILE(OARIAAPPLICATION.DATADIR+'WareHous','WareHous','SH')
ENDIF
*N119813,1 AMH [End]

*lcGlCode   = IIF(SEEK(PADR(lcWareH,6),'WareHous'),WareHous.GL_LINK,'DEFDEF')

*-- Amin, Enhance the performance of the Where Condition. [Start]
*-- To get a temporary cursor of the ItemLoc file to add the record and update the master file
*=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
"cInvType = '" + "    " + "'" + " AND Style ='" + "        " + "'","","StyDye")

*N119813,1 AMH Close the warehouse file if opened in this sission then open the itemloc file with correct SQL statment [Start]
*=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,"FALSE","","StyDye")
IF LLOPENWARE AND USED('WareHous')
  USE IN WAREHOUS
ENDIF

LOCAL LNCONNECTIONHANDLAR, LCSQLSTATMENT

LCSQLSTATMENT = "SELECT TOP 0 * FROM ITEMLOC"
LNCONNECTIONHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSQLSTATMENT,LCTMPITEMLOC,"ITEMLOC",OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,;
  'SAVE',SET("DATASESSION"))
IF LNCONNECTIONHANDLAR <> 1
  =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("sqlrun",LNCONNECTIONHANDLAR,.T.)
ENDIF
*N119813,1 AMH [End]
*-- Amin, Enhance the performance of the Where Condition. [End]


INSERT INTO (LCTMPITEMLOC) (CINVTYPE, STYLE, DESC, CDISCCODE, DYELOT, CWARECODE, AVE_COST,;
  NAVECSTBUY, GL_LINK);
  VALUES (LCINVTYPE, LCITEM, LCDESC, LCDISCCODS, LCDYELOT, LCWAREH,;
  IIF(EMPTY(LCDYELOT),LNCOST,0), IIF(EMPTY(LCDYELOT),LNCOST,0),;
  LCGLCODE)

*B131608,1 WSH 05/03/2006 [Start]
=GFADD_INFO(LCTMPITEMLOC)
*B131608,1 WSH 05/03/2006 [End]

*-- Updating the master ItemLoc file
SELECT(LCTMPITEMLOC)
IF GFUPDSQLFL(LCTMPITEMLOC,'Style,WareHous,Dyelot')
  =TABLEUPDATE(.T.,.T.)
ELSE
  =TABLEREVERT(.T.)
ENDIF

*-- [Khalid] This trigger will be called for all types of inventory that are classified for
*-- Sell (if the "S" $ cItemTran)
*C200171 TMI [Start] Gen Upcs in EDICATGD
*-- Run if EDI installed
*!*  IF ASCAN(oAriaApplication.laEvntTrig,PADR("GNUPCWH",10)) <> 0
*!*    IF OCCURS('NC',oAriaApplication.CompanyInstalledModules)<>0
*!*      lcWhCode   = lcPWare
*!*      lcSty      = lcPStyle
*!*      llFrmAdWre = .T.
*!*      =gfDoTriger('ICSTYLE','GNUPCWH   ')
*!*    ENDIF
*!*  ENDIF
*C200171 TMI [End  ] Gen Upcs in EDICATGD

*-- Arrange the dyelots for all types of inventory that are calssified to be "Used".
*-- (if the "U" $ cItemTran)
IF TYPE("lcInvTypeF") $ "UL" OR !USED(LCINVTYPEF)
  PRIVATE LCINVTYPEF
  LCINVTYPEF = GFTEMPNAME()
ENDIF
*-- To get the cItemTran
=GFOPNSQLFL('INVTYPE',LCINVTYPEF,;
  "CINVTYPE = '" + LCINVTYPE + "'" ,"","CINVTYPE")
SELECT(LCINVTYPEF)
LOCATE
LCITEMTRAN = CITEMTRAN

IF 'U' $ LCITEMTRAN AND !EMPTY(LCDYELOT)
  IF TYPE("lcTmpDyRel") $ "UL" OR !USED(LCTMPDYREL)
    LCTMPDYREL = GFTEMPNAME()
  ENDIF

  DIMENSION LAINDEX[2,3]
  LAINDEX[1,1] = "cInvType+cItem+Dyelot"
  LAINDEX[1,2] = LCTMPDYREL
  LAINDEX[2,1] = "cInvType+cItem+cDye_Seq"
  LAINDEX[2,2] = "SEQUENCE"

  =GFOPNSQLFL('Dye_Rel',LCTMPDYREL,;
    "cInvType = '" + LCINVTYPE + "'" + " AND cItem ='" + LCITEM + "'",@LAINDEX,"Dye_Rel")

  SELECT (LCTMPDYREL)
  *-- if you did not find this item, dyelot record
  IF !SEEK(LCINVTYPE+LCITEM+LCDYELOT)
    PRIVATE LNNEAREST,LCDYE_SEQ
    LCDYE_SEQ = ''
    LNNEAREST = RECNO(0)

    *-- Add this block to adjust add records at top of file.
    IF (LNNEAREST # 0)
      GO LNNEAREST
      IF CITEM <> LCITEM
        SET ORDER TO (LCTMPDYREL) DESCENDING
        = SEEK(LCINVTYPE+LCITEM+LCDYELOT)
        LNNEAREST = RECNO(0)
        IF LNNEAREST # 0
          GO LNNEAREST
          IF (CITEM <> LCITEM) OR (DYELOT <> LCDYELOT)
            LNNEAREST = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    *-- if it is the Last dyelot code.
    IF LNNEAREST = 0
      SELECT (LCTMPDYREL)
      SET ORDER TO TAG SEQUENCE DESCENDING
      = SEEK(LCINVTYPE+LCITEM)
      LCDYE_SEQ = PADL(ALLTRIM(STR(VAL(CDYE_SEQ) + 1)),4,'0')
    ENDIF

    SET ORDER TO
    IF LNNEAREST # 0
      GO LNNEAREST
      LCDYE_SEQ = CDYE_SEQ

      SCAN FOR CITEM+CDYE_SEQ = LCITEM AND CDYE_SEQ >= LCDYE_SEQ
        REPLACE CDYE_SEQ WITH PADL(ALLTRIM(STR(VAL(CDYE_SEQ) + 1)),4,'0')

        *B131608,1 WSH 05/03/2006 [Start]
        =GFADD_INFO(LCTMPDYREL)
        *B131608,1 WSH 05/03/2006 [End]

      ENDSCAN
    ENDIF

    *-- insert new dyelot line.
    INSERT INTO (LCTMPDYREL)                      ;
      (CINVTYPE, CITEM, DYELOT, CDYE_SEQ , CTMPSCOPE ) ;
      VALUES (LCINVTYPE, LCITEM, LCDYELOT, LCDYE_SEQ, LCTMPSCOPE)

    *B131608,1 WSH 05/03/2006 [Start]
    =GFADD_INFO(LCTMPDYREL)
    *B131608,1 WSH 05/03/2006 [End]

    *-- Updating the master Dye_Rel file
    SELECT(LCTMPDYREL)
    SET ORDER TO (LCTMPDYREL)
    IF GFUPDSQLFL(LCTMPDYREL,'cInvType,cItem,Dyelot')
      =TABLEUPDATE(.T.,.T.)
    ELSE
      =TABLEREVERT(.T.)
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name : gfItmDyBrw (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse the item's dyelots
*!*************************************************************
*! Passed :
*!        Parameters :
*!        lcInvtype -> "0001" for Style, "0002" for Fabric
*!        lcItem -> Item code (style or fabric-color)
*!        lcDyelot -> Dye lot code
*!        lcWareCode -> Warehouse Code
*!        llRetAlias -> To return the selected alias or not
*!        llIncAvail -> To display the browse fields according to this parameter
*!        llTop -> To determine the top of the browse.
*!        lcBrwAlias -> Alias to be browsed
*!        lcIndTag -> ITEMLOC tag.
*!        llUseConfg -> To check if using configuration or dye lot.
*!*************************************************************
*! Returned :
*!*************************************************************
*! Example :
*!        DO gfItmDyBrw WITH lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION GFITMDYBRW
LPARAMETERS LCINVTYPE, LCITEM, LCDYELOT, LCWARECODE, LLRETALIAS, LLINCAVAIL, LLTOP,;
  LCBRWALIAS, LCINDTAG, LLUSECONFG

PRIVATE LCBRFIELDS,LCALIAS,LNSTYORD,LCFROMWARE,LNALIAS,LADATA,LCBRWALIAS,LCDATADIR,LCINDTAG
LCFROMWARE = LCWARECODE
LLOPENFILE = .F.
LNALIAS    = SELECT()

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(LCTMPITEMLOC)
  PRIVATE LCTMPITEMLOC
  LCTMPITEMLOC = GFTEMPNAME()
ENDIF

IF TYPE("lcBrwAlias") $ "UL"
  DIMENSION LAINDEX [1,3]
  LAINDEX[1,1] = "Style+cWareCode+Dyelot"
  LAINDEX[1,2] = LCTMPITEMLOC
  *-- To get a temporary cursor of the ItemLoc file to add the record and update the master file

  *N119813,1 AMH Consider case of warehouse code not passed [Start]
  *=gfOpnSqlFl('ITEMLOC',lcTmpItemLoc,;
  "cInvType = '" + lcInvtype + "'" + " AND Style ='" + lcItem + "'"+;
  " AND cWareCode ='" + lcWareCode + "'" + " AND Dyelot<>'" + "          "+"'",@laIndex,"StyDye")
  =GFOPNSQLFL('ITEMLOC',LCTMPITEMLOC,;
    "cInvType = '" + LCINVTYPE + "'" + " AND Style ='" + LCITEM + "'"+;
    IIF(TYPE('lcWareCode')$'UL',""," AND cWareCode ='" + LCWARECODE + "'")+;
    " AND Dyelot<>'" + "          "+"'",@LAINDEX,"StyDye")
  *N119813,1 AMH [End]

  LCBRWALIAS = LCTMPITEMLOC
ELSE
  LCDATADIR  = OARIAAPPLICATION.WORKDIR
  LLOPENFILE = GFOPENFILE(LCDATADIR+LCBRWALIAS,LCDATADIR+LCINDTAG,'SH')
ENDIF
LCINDTAG   = IIF(TYPE("lcIndTag") $ "UL",LCTMPITEMLOC,LCINDTAG)

*-- array to get values from browse
DECLARE LADATA[3]
LADATA     = ' '
*-- variable to determine forcing browse or not
LLBROWSE   = IIF(TYPE('llBrowse')='U',.F.,LLBROWSE)

PRIVATE LNBRHSROW1, LNBRHSCOL1, LNBRHSROW2, LNBRHSCOL2
IF LLTOP
  LNBRHSROW1 = GNBRFSROW1
  LNBRHSCOL1 = GNBRFSCOL1
  LNBRHSROW2 = GNBRHSROW1
  LNBRHSCOL2 = GNBRHSCOL1
ELSE
  LNBRHSROW1 = GNBRHSROW1
  LNBRHSCOL1 = GNBRHSCOL1
  LNBRHSROW2 = GNBRHSROW2
  LNBRHSCOL2 = GNBRHSCOL2
ENDIF
LCITEM     = PADR(LCITEM, 19)
LCFROMWARE = IIF(TYPE('lcFromWare') $ 'UL' .OR. EMPTY(LCFROMWARE),'', LCFROMWARE)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTitle    = "Style "+ALLTRIM(lcItem)+IIF(llUseConfg,LANG_ARIA_Config,LANG_ARIA_Dyelot)
LCTITLE    = "Style "+ALLTRIM(LCITEM)+;
  IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_CONFIG,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Config",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Dyelot",LCARIAHFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcHeader   = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
LCHEADER   = IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",;
  LANG_ARIA_CONFIGNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ConfigNum",LCARIAHFILE)),;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DyelotNum",LCARIAHFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LCBRFIELDS = "DYELOT:R :H='"+LCHEADER+"':17,"

IF LLINCAVAIL
  LCBRFIELDS = LCBRFIELDS +;
    "A1=STK1-ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl1",LCARIAHFILE))+"':12,"+;
    "A2=STK2-ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl2",LCARIAHFILE))+"':12,"+;
    "A3=STK3-ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl3",LCARIAHFILE))+"':12,"+;
    "A4=STK4-ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl4",LCARIAHFILE))+"':12,"+;
    "A5=STK5-ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl5",LCARIAHFILE))+"':12,"+;
    "A6=STK6-ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl6",LCARIAHFILE))+"':12,"+;
    "A7=STK7-ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl7",LCARIAHFILE))+"':12,"+;
    "A8=STK8-ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_AVL8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Avl8",LCARIAHFILE))+"':12,"+;
    "A9=TOTSTK-TOTALO :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTAVL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAvl",LCARIAHFILE))+"':13,"


  LCBRFIELDS = LCBRFIELDS +;
    "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':12,"+;
    "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':12,"+;
    "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':12,"+;
    "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':12,"+;
    "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':12,"+;
    "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':12,"+;
    "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':12,"+;
    "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':12,"+;
    "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAlo",LCARIAHFILE))+"':13,"+;
    "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':12,"+;
    "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':12,"+;
    "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':12,"+;
    "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':12,"+;
    "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':12,"+;
    "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':12,"+;
    "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':12,"+;
    "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"':12,"+;
    "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':13"
ELSE
  LCBRFIELDS = LCBRFIELDS +;
    "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':12,"+;
    "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':12,"+;
    "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':12,"+;
    "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':12,"+;
    "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':12,"+;
    "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':12,"+;
    "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':12,"+;
    "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"':12,"+;
    "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':13,"+;
    "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':12,"+;
    "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':12,"+;
    "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':12,"+;
    "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':12,"+;
    "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':12,"+;
    "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':12,"+;
    "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':12,"+;
    "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':12,"+;
    "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotAlo",LCARIAHFILE))+"':13"
ENDIF


SELECT (LCBRWALIAS)
LNSTYORD   = VAL(SYS(21))
SET ORDER TO TAG (LCINDTAG)
SEEK LCITEM + LCFROMWARE
LOCATE REST WHILE STYLE+CWARECODE+DYELOT = LCITEM+LCFROMWARE;
  FOR !EMPTY(DYELOT)
IF !FOUND()
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcTmpStr = ALLTRIM(lcItem) + IIF(EMPTY(lcFromWare) , LANG_ARIA_MsgVar1 , LANG_ARIA_MsgVar2 + ALLTRIM(lcFromWare))
  LCTMPSTR = ALLTRIM(LCITEM) + IIF(EMPTY(LCFROMWARE) ,;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGVAR1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgVar1",LCARIAHFILE)) ,;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGVAR2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgVar2",LCARIAHFILE)) + ALLTRIM(LCFROMWARE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  IF LLUSECONFG
    =GFMODALGEN("INM00407B00000" , "DIALOG" , LCTMPSTR)
  ELSE
    =GFMODALGEN("INM00277B00000" , "DIALOG" , LCTMPSTR)
  ENDIF
  LLWASSEL = .F.
ELSE
  SELECT (LCBRWALIAS)
  LLWASSEL= ARIABROW("'"+LCITEM+LCFROMWARE+"' FOR !EMPTY(DYELOT)",LCTITLE,;
    LNBRHSROW1, LNBRHSCOL1, LNBRHSROW2, LNBRHSCOL2,'','',"Style,Dyelot","laData")


  IF LLWASSEL
    LCITEM  = LADATA[1]
    LCDYELOT = LADATA[2]
  ELSE
    LCDYELOT = SPACE(10)
  ENDIF
ENDIF

SET ORDER TO LNSTYORD
IF LLOPENFILE .AND. USED(LCBRWALIAS)
  USE IN (LCBRWALIAS)
ENDIF
SELECT (LNALIAS)

RETURN LLWASSEL

*!*************************************************************
*! Name : gfBrowWarH (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : This function will browse the wharehouses
*!*************************************************************
*! Passed Parameters :
*!                   llIsEscape -> If the escape is allowed
*!                   lcInvtype -> For example "0001" for Style, "0002" for Fabric
*!                   lcItem  -> Fabric or Style Code
*!                   lcWareCode -> Warehouse Code
*!                   lcStyMatIn -> "S" for finished good inventory, "M" for material inventory
*!                   llForCurrSite -> for site id in case of NC is installed.
*!*************************************************************
*! Returned : lcWareCode
*!*************************************************************
*! Example :
*!        DO gfBrowWarH WITH .F.,,,,'S',''
*!*************************************************************
FUNCTION GFBROWWARH
LPARAMETERS LLISESCAPE, LCINVTYPE, LCITEM, LCWARECODE, LCSTYMATIN, LLFORCURRSITE

PRIVATE LCWARECODE, LNCURALIAS, LNCURTAG
LCFORCOND = ''

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(LCTMPITEMLOC)
  PRIVATE LCTMPITEMLOC
  LCTMPITEMLOC = GFTEMPNAME()
ENDIF

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\WAREBROW.H


*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCWAREHFILE
LCWAREHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCWAREHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("WAREBROW")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
GO TOP IN WAREHOUS
IF EOF('WareHous')
  *N000682,1 MMT 12/03/2012 Globalization changes[Start]
  *=MESSAGEBOX("No locations found.",16,_SCREEN.CAPTION)
  =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_NOLOC,OARIAAPPLICATION.GETHEADERTEXT("LANG_NOLOC",LCWAREHFILE)))
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
  RETURN SPACE(6)
ENDIF

*-- Save the current alias.
LNCURALIAS = SELECT()

SELECT WAREHOUS
LNCURTAG    = VAL(SYS(21))
SET ORDER TO TAG WAREHOUS

LCWARECODE  = IIF(EMPTY(LCWARECODE), SPACE(6), LCWARECODE)
*-- If called from browsing the warehouses of a specific item/color.
IF !EMPTY(LCITEM)
  DIMENSION LAINDEX[1,3]
  LAINDEX[1,1] = "cWareCode"
  LAINDEX[1,2] = LCTMPITEMLOC

  =GFOPNSQLFL('ITEMLOC',LCTMPITEMLOC,;
    "cInvType = '" + LCINVTYPE + "'" +;
    " AND Style ='" + LCITEM + "'" + " AND Dyelot='"+ "          "+"'",@LAINDEX,"StyDye")
  SELECT(LCTMPITEMLOC)
  LOCATE
  LLFOUND   = !EOF()
  IF !LLFOUND
    *N000682,1 MMT 12/03/2012 Globalization changes[Start]
    *=MESSAGEBOX('Item:'+ ALLTRIM(lcItem) +  "is not assigned to any location.",64)
    =GFMODALGEN('INM00000B00000',.F.,.F.,.F.,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ITEMCOL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ITEMCOL",LCWAREHFILE)) + ALLTRIM(LCITEM) +IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_NOTASN,OARIAAPPLICATION.GETHEADERTEXT("LANG_NOTASN",LCWAREHFILE)))
    *N000682,1 MMT 12/03/2012 Globalization changes[END]
    SET ORDER TO LNCURTAG IN WAREHOUS
    SELECT (LNCURALIAS)
    RETURN SPACE(6)
  ENDIF
  SELECT WAREHOUS
  SET RELATION TO  CWARECODE ;
    INTO (LCTMPITEMLOC) ADDITIVE
  LCFORCOND   = '!EOF(lcTmpItemLoc)'
ELSE
  LCWARECODE = SPACE(6)
ENDIF


IF TYPE('lcStyMatIn')='C' AND LCSTYMATIN $ 'SM'
  LCMATORSTY = IIF(UPPER(LCSTYMATIN) = 'S','lStyInv','lMatInv')
  LCFORCOND  = LCFORCOND +IIF(EMPTY(LCFORCOND),'',' AND ') + LCMATORSTY
ENDIF

IF LLFORCURRSITE
  LCFORCOND  = LCFORCOND +IIF(EMPTY(LCFORCOND),'',' AND ') + "cSiteId = oAriaApplication.CurrentSite"
ENDIF

DO FORM (OARIAAPPLICATION.SCREENHOME+"IC\Warebrow.scx") WITH LCFORCOND TO LCWARECODE

SET ORDER TO LNCURTAG IN WAREHOUS

SELECT (LNCURALIAS)
POP KEY
LCWARECODE = IIF(EMPTY(LCWARECODE),SPACE(6),LCWARECODE)
RETURN (LCWARECODE)

*!*************************************************************
*! Name : gfAddBrwDy (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse/add dyelots for a specific item
*!*************************************************************
*! Passed :
*!        Parameters :
*!		  lcInvtype -> "0001" for Style, "0002" for Fabric
*!		  lcItem -> Item code (style or fabric-color)
*!		  lcDyelot -> Dye lot code
*!		  lcWareCode -> Warehouse Code
*!		  llUseConfg -> If the system uses configuration or dye lot
*!*************************************************************
*! Returned :
*!*************************************************************
*! Example :
*!        DO gfAddBrwDy WITH "0001",lcStyle,SPACE(10),lcWareCode
*!*************************************************************
FUNCTION GFADDBRWDY
LPARAMETERS LCINVTYPE, LCITEM, LCDYELOT, LCWARECODE, LLUSECONFG
PRIVATE LNALIAS, LLMULTIWH

LNALIAS=SELECT()
LLMULTIWH  = ALLTRIM(UPPER(GFGETMEMVAR('M_WareHouse')))='Y'

IF TYPE("lcTmpItemLoc") $ "UL" OR !USED(LCTMPITEMLOC)
  LCTMPITEMLOC = GFTEMPNAME()
ENDIF

DIMENSION LAINDEX [1,3]
LAINDEX[1,1] = "Style+cWareCode+Dyelot"
LAINDEX[1,2] = LCTMPITEMLOC
*-- To get a temporary cursor of the ItemLoc file to add the record and update the master file
=GFOPNSQLFL('ITEMLOC',LCTMPITEMLOC,;
  "cInvType = '" + LCINVTYPE + "'" + " AND Style ='" + LCITEM + "'"+;
  " AND cWareCode = '" + LCWARECODE +"'" + " AND Dyelot='" + LCDYELOT + "'",@LAINDEX,"StyDye")

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
SELECT (LCTMPITEMLOC)
LOCATE
IF EOF()
  IF LLMULTIWH
    *lcmesgx = 'Style\Warehouse :'+ALLTRIM(lcStyle)+'\'+ALLTRIM(lcWare)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcmesgx = LANG_ARIA_MsgSgx1+ALLTRIM(lcItem)+'\'+ALLTRIM(lcWareCode)
    LCMESGX = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGSGX1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgSgx1",LCARIAHFILE))+;
      ALLTRIM(LCITEM)+'\'+ALLTRIM(LCWARECODE)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    *lcmesgx = 'Item  :'+ALLTRIM(lcStyle)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcmesgx = LANG_ARIA_MsgSgx2+ALLTRIM(lcItem)
    LCMESGX = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_MSGSGX2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_MsgSgx2",LCARIAHFILE))+ALLTRIM(LCITEM)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  IF LLUSECONFG
    LNCHOICE=GFMODALGEN('TRM42254B42003','DIALOG',ALLTRIM(LCITEM)+'|'+ALLTRIM(LCDYELOT)+'|'+LCWARECODE)
  ELSE
    LNCHOICE=GFMODALGEN('TRM42062B42003','DIALOG',ALLTRIM(LCITEM)+'|'+ALLTRIM(LCDYELOT)+'|'+LCWARECODE)
  ENDIF
  IF LNCHOICE = 1
    =GFOPNSQLFL('ITEMLOC',LCTMPITEMLOC,;
      "cInvType = '" + LCINVTYPE + "'" + " AND Style ='" + LCITEM + "'"+;
      " AND cWareCode = '" + LCWARECODE + "'" ,@LAINDEX,"StyDye")
    SELECT (LCTMPITEMLOC)
    LOCATE
    IF EOF()
      =GFADITEMWAR(LCINVTYPE, LCITEM, SPACE(10), LCWARECODE )
    ENDIF
    =GFADITEMWAR(LCINVTYPE, LCITEM, LCDYELOT, LCWARECODE )
    SELECT(LNALIAS)
    RETURN
  ELSE
    IF LNCHOICE = 2
      =GFOPNSQLFL('ITEMLOC',LCTMPITEMLOC,;
        "cInvType = '" + LCINVTYPE + "'" + " AND Style ='" + LCITEM + "'"+;
        " AND cWareCode = '" + LCWARECODE + "'",@LAINDEX,"StyDye")
      SELECT(LCTMPITEMLOC)
      LOCATE
      IF !EOF()
        LOCATE REST WHILE STYLE+CWARECODE+DYELOT = PADR(LCITEM,19)+LCWARECODE;
          FOR !EMPTY(DYELOT)
        IF !FOUND()
          IF LLUSECONFG
            =GFMODALGEN('TRM42253B42001','DIALOG',LCMESGX)
          ELSE
            =GFMODALGEN('TRM42053B42001','DIALOG',LCMESGX)
          ENDIF
          LCDYELOT = SPACE(10)
          SELECT(LNALIAS)
          RETURN
        ELSE
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *lcHeader   = IIF(llUseConfg,LANG_ARIA_ConfigNum,LANG_ARIA_DyelotNum)
          LCHEADER   = IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",;
            LANG_ARIA_CONFIGNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ConfigNum",LCARIAHFILE)),;
            IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_DYELOTNUM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_DyelotNum",LCARIAHFILE)))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          LCBRFIELDS = "Dyelot  :H='"+LCHEADER+"' ,"
          LCBRFIELDS = LCBRFIELDS +;
            "STK1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk1",LCARIAHFILE))+"':12,"+;
            "STK2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk2",LCARIAHFILE))+"':12,"+;
            "STK3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk3",LCARIAHFILE))+"':12,"+;
            "STK4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk4",LCARIAHFILE))+"':12,"+;
            "STK5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk5",LCARIAHFILE))+"':12,"+;
            "STK6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk6",LCARIAHFILE))+"':12,"+;
            "STK7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk7",LCARIAHFILE))+"':12,"+;
            "STK8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STK8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Stk8",LCARIAHFILE))+"':12,"+;
            "TOTSTK:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTSTK,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_TotStk",LCARIAHFILE))+"':13,"+;
            "ALO1  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo1",LCARIAHFILE))+"':12,"+;
            "ALO2  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo2",LCARIAHFILE))+"':12,"+;
            "ALO3  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo3",LCARIAHFILE))+"':12,"+;
            "ALO4  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo4",LCARIAHFILE))+"':12,"+;
            "ALO5  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo5",LCARIAHFILE))+"':12,"+;
            "ALO6  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO6,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo6",LCARIAHFILE))+"':12,"+;
            "ALO7  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO7,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo7",LCARIAHFILE))+"':12,"+;
            "ALO8  :R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_ALO8,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_Alo8",LCARIAHFILE))+"':12,"+;
            "TOTALO:R :H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_TOTALO,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_ToTAlo",LCARIAHFILE))+"':13"
          LCDYELOT = SPACE(10)
          SELECT(LCTMPITEMLOC)
          IF ARIABROW([FOR !EMPTY(Dyelot)],;
              IIF(LLUSECONFG,IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STYCONFG,;
              OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_StyConfg",LCARIAHFILE)),;
              IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_STYDYELOT,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_StyDyelot",LCARIAHFILE))),;
              GNBRHSROW1, GNBRHSCOL1, GNBRHSROW2, GNBRHSCOL2)
            LCDYELOT = DYELOT
          ENDIF

        ENDIF
      ENDIF
    ELSE
      LCDYELOT = SPACE(10)
      SELECT(LNALIAS)
      RETURN
    ENDIF
  ENDIF
ENDIF
SELECT(LNALIAS)
RETURN

*!*************************************************************
*! Name : gfItemBrow (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To browse styles/fabrics
*!*************************************************************
*! Passed :
*!        Parameters :
*!      lcInvtype -> "0001" for Style, "0002" for Fabric
*!      lcItem  -> Fabric or Style Code
*!      lcColor -> Color Code
*!      lcSeason -> "*" for all season, otherwise a specific season.
*!      lcMajorOrNon -> For none-major segment.
*!      lcForExp -> If there is any specific expression.
*!      llRetAlias -> To return the selected alias before entering this function.
*!*************************************************************
*! Returned :
*!*************************************************************
*! Example :
*!        DO gfItemBrow WITH "0001"
*!*************************************************************
FUNCTION GFITEMBROW
LPARAMETERS LCINVTYPE, LCITEM, LCCOLOR, LCSEASON, LCMAJORORNON, LCFOREXP, LLRETALIAS

PRIVATE LCBRFIELDS,LNCURALIAS, LCSTYLE,LADATA ,LCSTYORDER ,LCSTYTTL ,LCTITLE ,;
  LCXMJR ,LCXNMJR ,LCCLRTTL ,LCSTYORDER,LCMAJORORNON, LNMAJLEN,LAINDEX, LAINDEX2


DECLARE LADATA[3] && array to get values from browse
STORE '' TO LADATA,LCSCOPE
*-- variable to determine forcing browse or not
LLBROWSE = IIF(TYPE('llBrowse')='U',.T.,LLBROWSE)

IF !USED('Codes')
  LCCURALIAS = ALIAS()
  =GFOPENFILE(OARIAAPPLICATION.DATADIR+'Codes','Codes','SH')
  SELECT(LCCURALIAS)
ENDIF
LCSEASON     = IIF(TYPE('lcSeason')$'UL',"",LCSEASON)
LCMAJORORNON = IIF(TYPE('lcMajorOrNon')$'UL',"M",LCMAJORORNON)
LCSTYTTL = IIF(LCMAJORORNON='M',GFITEMMASK('HM','',LCINVTYPE),GFITEMMASK('HN','',LCINVTYPE))
LCTITLE  = IIF(LCMAJORORNON='M',GFITEMMASK('HM','',LCINVTYPE),;
  IIF(LCMAJORORNON="N",GFITEMMASK('HN','',LCINVTYPE),GFITEMMASK('HI','',LCINVTYPE)))
LCXMJR  = GFITEMMASK('HM','',LCINVTYPE)
LCXNMJR = GFITEMMASK('HN','',LCINVTYPE)

*E124065,1 AMH Get the color position and width [Start]
LOCAL ARRAY LAITEMSEG[1,1]
=GFITEMMASK(@LAITEMSEG,'',LCINVTYPE)
PRIVATE LNCLRSTRT,LNCLRWIDTH

FOR LNI = 1 TO ALEN(LAITEMSEG,1)
  IF LAITEMSEG[lnI,1] = 'C'
    LNCLRSTRT  = LAITEMSEG[lnI,4]
    LNCLRWIDTH = LEN(LAITEMSEG[lnI,3])
    EXIT
  ENDIF
ENDFOR
*E124065,1 AMH [End]

LCCLRTTL = LCXMJR +'/'+ LCXNMJR
LNMAJLEN = LEN(GFITEMMASK('PM','',LCINVTYPE))

*!B039082,1 WSH, 02/23/2005, Enhance Item Browse... Use new browse technique. [Start]
*!*  IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem)
*!*    lcTmpItem = gfTempName()
*!*
*!*    *N119680,1 AMH Use the specific expression [Start]
*!*    LOCAL lcWhereCond
*!*    lcWhereCond = "CINVTYPE = '" + lcInvtype + "'" + IIF(EMPTY(lcForExp),""," AND ") + lcForExp
*!*    *N119680,1 AMH [End]
*!*
*!*    *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*    LOCAL lcStatement
*!*
*!*    lcStatement = "SELECT Style, cStyMajor, Status, cDefWare, [Desc], Desc1, Season, cDivision," +;
*!*                  "       PriceA, PriceB, PriceC, Fabric, cStyGrade, Royalty, Pattern, Scale, " +;
*!*                  "       Prepak, cBuyPrepk, Qty_Ctn, Commission, Link_Code, Make, nMCost1, " +;
*!*                  "       nMCost2, nMCost3, nMCost4, nMCost5, nMCost6, nMCost7, nICost1, nICost2, " +;
*!*                  "       nICost3, nicost4, nICost5, nICost6, nICost7, nPrCost2, nPrCost3, nPrCost4, nPrCost5, nPrCost6, nPrCost7, " +;
*!*                  "       TotCost, Ave_Cost, nStkVal, SoldOut, Start, Location, lInvSty, " +;
*!*                  "       MarkA, MarkB, MarkC, cConsInfo1, cConsInfo2" +;
*!*                  "  FROM Item WITH (INDEX = STYLE)" +;
*!*                  "  WHERE " + lcWhereCond
*!*
*!*    *--Run the Satatement
*!*    lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItem, "ITEM",;
*!*                                              oAriaApplication.ActiveCompanyConStr,;
*!*                                              3, "SAVE", SET("Datasession"))

*!*    *--Check Connection Result
*!*    IF lnConnectionHandler # 1
*!*      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
*!*      RETURN ""
*!*    ENDIF

*!*    LOCAL lnOldBuffMode
*!*
*!*    lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItem)
*!*    =CURSORSETPROP("Buffering", 3, lcTmpItem)

*!*    SELECT (lcTmpItem)
*!*    *E038245,1 WSH [End]

*!*    IF lcMajorOrNon = 'M'

*!*      *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*      *DIMENSION laIndex[2,3]
*!*      *laIndex = " "
*!*      *laIndex[1,1] = "Style"
*!*      *laIndex[1,2] = "Style"
*!*      *laIndex[2,1] = "cStyMajor"
*!*      *laIndex[2,2] = "CStyle"
*!*      *laIndex[2,3] = "Unique"
*!*
*!*      *N119680,1 AMH Use the specific expression [Start]
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,lcWhereCond,@laIndex,"Style")
*!*      *N119680,1 AMH [End]
*!*      INDEX ON Style TAG Style
*!*      INDEX ON cStyMajor TAG cStyle UNIQUE
*!*      *E038245,1 WSH [End]

*!*    ELSE

*!*      *E038245,1 WSH Enhance the way of openning SQL Item file [Start]
*!*      *DIMENSION laIndex[1,3]
*!*      *laIndex =" "
*!*      *laIndex[1,1] = "Style"
*!*      *laIndex[1,2] = "Style"
*!*
*!*      *N119680,1 AMH Use the specific expression [Start]
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*      *=gfOpnSqlFl('ITEM',lcTmpItem,lcWhereCond,@laIndex,"Style")
*!*      *N119680,1 AMH [End]
*!*      INDEX ON Style TAG Style
*!*      *E038245,1 WSH [End]
*!*
*!*    ENDIF

*!*    *E038245,1 WSH Open ItemLoc file to Get Quantity Fields - No Need to Open Item file again [Start]
*!*    =CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItem)
*!*
*!*    *-- Open the item file in another alias to calculate some fields.

*!*    *lcTmpItem2 = gfTempName()

*!*    *DIMENSION laIndex2[1,3]
*!*    *laIndex2 = " "
*!*    *laIndex2[1,1] = "Style"
*!*    *laIndex2[1,2] = "Style"
*!*
*!*    *N119680,1 AMH Use the specific expression [Start]
*!*    *=gfOpnSqlFl('ITEM',lcTmpItem2,"CINVTYPE = '" + lcInvtype + "'",@laIndex,"Style")
*!*    *=gfOpnSqlFl('ITEM',lcTmpItem2,lcWhereCond,@laIndex,"Style")
*!*    *N119680,1 AMH [End]

*!*    lcTmpItemLoc = gfTempName()

*!*    lcWhereCond = "cInvType = '" + lcInvtype + "' AND Dyelot = ''"
*!*
*!*    lcStatement = "SELECT  " + IIF(lcMajorOrNon == 'M', "LEFT(Style, " + STR(lnMajLen) + ") AS ", "") + "Style, " +;
*!*                  "        SUM(totord) AS totord, SUM(totstk) AS totstk, SUM(totwip) AS totwip, SUM(nStkVal) AS nStkVal" +;
*!*                  "  FROM  ItemLoc WITH (INDEX = STYDYE)" +;
*!*                  "  WHERE " + lcWhereCond +;
*!*                  "  GROUP BY " + IIF(lcMajorOrNon == 'M', "LEFT(Style," + STR(lnMajLen) + ")", "Style")

*!*    *--Run the Satatement
*!*    lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItemLoc, "ITEMLOC",;
*!*                                              oAriaApplication.ActiveCompanyConStr,;
*!*                                              3, "SAVE", SET("Datasession"))

*!*    *--Check Connection Result
*!*    IF lnConnectionHandler # 1
*!*      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
*!*      RETURN ""
*!*    ENDIF
*!*
*!*    SELECT (lcTmpItemLoc)
*!*    lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItemLoc)
*!*    =CURSORSETPROP("Buffering", 3, lcTmpItemLoc)
*!*    INDEX ON Style TAG Style
*!*    =CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItemLoc)
*!*    SET ORDER TO Style
*!*    *E038245,1 WSH [End]

*!*  ENDIF

*!*  *-- Include the .H file
*!*  #INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

*!*  *E038245,1 WSH No need for this condition as Item file is opened only once [Start]
*!*  *IF lcMajorOrNon $ 'NI'
*!*  *E038245,1 WSH [End]

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]

*!*      *E124065,1 AMH Add color name to style and material browse [Start]
*!*      *lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)],[cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) + [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
*!*      lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)]+;
*!*                   [,lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+LANG_COLOR+["],;
*!*                   [cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) +;
*!*                   [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*                   [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+;
*!*                   [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]
*!*      *E124065,1 AMH [End]

*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [pricea :10:H="]+LANG_PRICEA+[" , PRICEB :10:H="]+LANG_PRICEB+[",PRICEC :10:H="]+LANG_PRICEC+[",]
*!*
*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = lcBrFields+;
*!*                   [totWip:12:h="]+LANG_WIP+[",totstk:12:h="]+LANG_STOCK+[",totord:12:h="]+LANG_ORDER+[",]

*!*      lcBrFields = lcBrFields+;
*!*                   [&lcTmpItemLoc..TotWip: 12: H="]+LANG_WIP+[",]+;
*!*                   [&lcTmpItemLoc..TotStk: 12: H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd: 12: H="]+LANG_ORDER+[",]

*!*      *lcBrFields = lcBrFields+;
*!*                   [OTS=(TOTWIP+TOTSTK-TOTORD):12:H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]

*!*      lcBrFields = lcBrFields+;
*!*                   [OTS=&lcTmpItemLoc..totWip+&lcTmpItemLoc..totStk-&lcTmpItemLoc..totOrd: 12: H="]+LANG_OTS+[",Fabric:15:h="]+LANG_FABRIC+[",]
*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [CSTYGRADE :H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+LANG_ROYAL+[" , PATTERN :H="]+LANG_PATRN+[", STATUS :H="]+LANG_STATUS+[",]

*!*      lcBrFields = lcBrFields + ;
*!*                   [SCALE :H="]+LANG_SCALE+[", PREPAK :H="]+LANG_PREPAK+[", CBUYPREPK :H="]+LANG_BPREPK+[", QTY_CTN :H="]+LANG_QTYCRT+[", COMMISSION :H="]+LANG_COMM+[", LINK_CODE :H="]+LANG_LNKCD+[
*!*                   [lcMaked = IIF(MAKE,'Y','N') :H="]+LANG_MAKE+[", NMCOST1 :H="]+LANG_MCST1+[" , NMCOST2 :H="]+LANG_MCST2+[", NMCOST3 :H="]+LANG_MCST3+[", NMCOST4 :H="]+LANG_MCST4+[",NMCOST5 :H="]

*!*      lcBrFields = lcBrFields + ;
*!*                   [NICOST1 :H="]+LANG_ICST1+[", NICOST2 :H="]+LANG_ICST2+[", NICOST3 :H="]+LANG_ICST3+[", NICOST4 :H="]+LANG_ICST4+[", NICOST5 :H="]+LANG_ICST5+[",]

*!*      *E038245,1 WSH Calculate Average Cost from ItemLoc file not from Item file [Start]
*!*      *lcBrFields = lcBrFields+;
*!*                   [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*                   [AVE_COST :H="]+LANG_AVECST+[",NSTKVAL :H="]+LANG_STKVAL+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H="]+LANG_INVSTY+[",]
*!*      lcBrFields = lcBrFields+;
*!*                   [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*                   [lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', "", [",NSTKVAL :H="]+LANG_STKVAL)+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LA
*!*      *E038245,1 WSH [End]

*!*      lcBrFields = lcBrFields+;
*!*                   [MARKA :H="]+LANG_MARKA+[",MARKB:H="]+LANG_MARKB+[",MARKC :H="]+LANG_MARKC+[",]+;
*!*                   [CCONSINFO1 :H="]+LANG_CONSI1+[",CCONSINFO2 :H="]+LANG_CONSI2+["]

*!*  *E038245,1 WSH No need for this condition as Item file is opened only once [Start]

*!*  *!*  ELSE
*!*  *!*
*!*  *!*      lcBrFields = [CSTYMAJOR:30:H=ALLTRIM(lcStyTtl),&lcTmpItem2..DESC:20:H="]+LANG_DESC+[",&lcTmpItem2..DESC1:35:H="]+LANG_LDESC+[",]+;
*!*  *!*                   [lcSesDesc=gfCodDes(&lcTmpItem2..Season,'SEASON'):20:H="]+LANG_SEASON+[",lcDivDesc=gfCodDes(&lcTmpItem2..cdivision,'CDIVISION'):20:H="]+LANG_DIV+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..pricea:10:h="]+LANG_PRICEA+[",&lcTmpItem2..PRICEB:10:h="]+LANG_PRICEB+[",&lcTmpItem2..PRICEC:10:h="]+LANG_PRICEC+[",]
*!*  *!*
*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [lnTotWIP=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'TotWip',lcTmpItem2):12:h="]+LANG_WIP+[",]+;
*!*  *!*                   [lnTotStk=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totstk',lcTmpItem2):12:h="]+LANG_STOCK+[",]+;
*!*  *!*                   [lnTotORD=gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totord',lcTmpItem2):12:h="]+LANG_ORDER+[",]+;
*!*  *!*                   [lnOts   =gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'totWip+totStk-totOrd',lcTmpItem2):12:H="]+LANG_OTS+[",]

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..Fabric:12:h="]+LANG_FABRIC+[",&lcTmpItem2..CSTYGRADE:H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(&lcTmpItem2..ROYALTY,'ROYALTY') :15 :H="]+LANG_ROYAL+[" , &lcTmpIte

*!*  *!*      lcBrFields = lcBrFields + ;
*!*  *!*                   [&lcTmpItem2..SCALE :H="]+LANG_SCALE+[", &lcTmpItem2..PREPAK :H="]+LANG_PREPAK+[", &lcTmpItem2..CBUYPREPK :H="]+LANG_BPREPK+[", &lcTmpItem2..QTY_CTN :H="]+LANG_QTYCRT+[", &l
*!*  *!*                   [lcMaked = IIF(&lcTmpItem2..MAKE,'Y','N') :H="]+LANG_MAKE+[", &lcTmpItem2..NMCOST1 :H="]+LANG_MCST1+[" , &lcTmpItem2..NMCOST2 :H="]+LANG_MCST2+[", &lcTmpItem2..NMCOST3 :H="]

*!*  *!*      lcBrFields = lcBrFields + ;
*!*  *!*                   [&lcTmpItem2..NICOST1 :H="]+LANG_ICST1+[", &lcTmpItem2..NICOST2 :H="]+LANG_ICST2+[", &lcTmpItem2..NICOST3 :H="]+LANG_ICST3+[", &lcTmpItem2..NICOST4 :H="]+LANG_ICST4+[", &lcT

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..NPRCOST2 :H="]+LANG_PCST2+[",&lcTmpItem2..NPRCOST3 :H="]+LANG_PCST3+[",&lcTmpItem2..NPRCOST4 :H="]+LANG_PCST4+[",&lcTmpItem2..NPRCOST5 :H="]+LANG_PCST5+[",&lcT
*!*  *!*                   [lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+[",&lcTmpItem2..SOLDOUT :H="]+LANG_SOLDDT+[",&lcTmpItem2..START :H="]+LANG_STRTDT+[",&lcTmpItem2..LOCATION :H="]+LANG_DBIN+[",&lcT

*!*  *!*      lcBrFields = lcBrFields+;
*!*  *!*                   [&lcTmpItem2..MARKA :H="]+LANG_MARKA+[",&lcTmpItem2..MARKB:H="]+LANG_MARKB+[",&lcTmpItem2..MARKC :H="]+LANG_MARKC+[",]+;
*!*  *!*                   [&lcTmpItem2..CCONSINFO1 :H="]+LANG_CONSI1+[",&lcTmpItem2..CCONSINFO2 :H="]+LANG_CONSI2+["]

*!*  *!*  ENDIF

*!*  *E038245,1 WSH [End]

*!*  lnCurAlias = SELECT()
*!*  SELECT (lcTmpItem)
*!*  lcStyOrder = TAG()
*!*  *SET ORDER TO TAG CSTYLE

*!*  DO CASE
*!*    CASE '*' $ lcColor AND '*' $ lcSeason

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",&lcTmpItemLoc..TotWip :14 :H="]+LANG_WIP+[",&lcTmpItemLoc..TotStk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      *E038245,1 WSH [End]

*!*    CASE '*' $ lcColor

*!*      *E038245,1 WSH Get Quantities from ItemLoc Table not from Item Table [Start]
*!*      *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",totWip :14 :H="]+LANG_WIP+[",totstk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [totord :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
*!*                   [pricea :H="]+LANG_PRICEA+[",&lcTmpItemLoc..TotWip :14 :H="]+LANG_WIP+[",&lcTmpItemLoc..TotStk :14 :H="]+LANG_STOCK+[",]+;
*!*                   [&lcTmpItemLoc..TotOrd :14 :H="]+LANG_ORDER+[",Fabric :19 :H="]+LANG_FABRIC+["]
*!*      *E038245,1 WSH [End]

*!*      lcScope = [=gfvSeasonC]
*!*
*!*    OTHERWISE
*!*      IF !('*' $ lcSeason)
*!*        lcScope = [=gfvSeasonC]
*!*      ENDIF
*!*  ENDCASE

*!*  SELECT (lcTmpItem)

*!*  *E038245,1 WSH Set relation between Item and ItemLoc files [Start]

*!*  IF lcMajorOrNon = 'M'
*!*    SET ORDER TO TAG CSTYLE
*!*    SET RELATION TO LEFT(cStyMajor, lnMajLen) INTO (lcTmpItemLoc) ADDITIVE
*!*  ELSE
*!*    SET ORDER TO TAG STYLE
*!*    SET RELATION TO Style INTO (lcTmpItemLoc) ADDITIVE
*!*  ENDIF

*!*  *SET RELATION TO PADR(&lcTmpItem..CSTYMAJOR,lnMajLen) INTO (lcTmpItem2) ADDITIVE

*!*  *E038245,1 WSH [End]

*!*  *E121005,1 WSH Remove the '?' from the "lcItem" to locaate right [Start]
*!*  LOCAL lcBrowChr
*!*  lcItem    = RTRIM(lcItem)
*!*  lcBrowChr = RIGHT(lcItem, 1)
*!*  lcItem    = IIF(lcBrowChr == '?', SUBSTR(lcItem, 1, LEN(lcItem) - 1) , lcItem)
*!*  *E121005,1 WSH [End]

*!*  IF lcMajorOrNon = 'M'
*!*    lcStyle = [lcItem]
*!*  ELSE
*!*    IF lcMajorOrNon $ 'N'
*!*      SELECT (lcTmpItem)
*!*
*!*      *N119680,1 AMH Use the correct Expration [Start]
*!*      *lcStyle = [lcItem]
*!*      lcStyle = "'"+PADR(lcItem,lnMajLen)+"'"
*!*      *N119680,1 AMH [End]
*!*
*!*    ENDIF
*!*  ENDIF

*!*  IF !SEEK(lcItem)
*!*   lnSoftSeek = IIF(SEEK(RTRIM(lcItem),lcTmpItem),RECNO(),RECNO(0))
*!*   IF  lnSoftSeek > 0
*!*     GO lnSoftSeek
*!*   ELSE
*!*     GO TOP
*!*   ENDIF
*!*  ENDIF

*!*  IF TYPE('lcStyle')='C'
*!*    IF lcMajorOrNon = 'N'
*!*      *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*      *llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*      llWasSel=ARIABROW(lcStyle,lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*      *E038245,1 WSH [Start]
*!*    ELSE
*!*      *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*      *llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*      llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*      *E038245,1 WSH [Start]
*!*    ENDIF
*!*  ELSE
*!*    *E038245,1 WSH Remove Second Item Cursor and use the main one [Start]
*!*    *llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,&lcTmpItem2..SEASON","laData")
*!*    llWasSel=ARIABROW('',lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,lcScope,"","STYLE,SEASON","laData")
*!*    *E038245,1 WSH [Start]
*!*  ENDIF

*!*  *E038245,1 WSH Remove Second Item Cursor as it is not needed and clear ItemLoc relation [Start]
*!*  *IF lcMajorOrNon = 'M'
*!*    *SET SKIP TO

*!*    *SET RELATION OFF INTO (lcTmpItem2)
*!*  *ENDIF

*!*  SET RELATION OFF INTO (lcTmpItemLoc)
*!*  *E038245,1 WSH [Start]

*!*  IF llWasSel
*!*    lcItem  = laData[1]
*!*    lcColor = laData[2]
*!*  ELSE
*!*    lcItem  = SPACE(12)
*!*    lcColor = SPACE(6)
*!*  ENDIF

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*IF TYPE("lcTmpItem") $ "UL" OR !USED(lcTmpItem) OR TYPE("lcTmpItemLoc") $ "UL" OR !USED(lcTmpItemLoc)
IF TYPE("lcTmpItem") $ "UL" OR !USED(LCTMPITEM) OR TYPE("lcTmpItem2") $ "UL" OR !USED(LCTMPITEM2)
  *B128949,1 WSH 08/07/2005 [End]

  LCTMPITEM = GFTEMPNAME()

  *--Create Temp Item Cursor
  LNCONNECTIONHANDLER = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN("SELECT TOP 0 * FROM ITEM (INDEX = STYLE)", LCTMPITEM, "ITEM",;
    OARIAAPPLICATION.ACTIVECOMPANYCONSTR,;
    3, "SAVE", SET("Datasession"))

  *--Check Connection Result
  IF LNCONNECTIONHANDLER # 1
    =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("SQLRUN", LNCONNECTIONHANDLER, .T.)
    RETURN ""
  ENDIF

  *--Open Temp ItemLoc Cursor to Get needed Quantity Fields
  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *SELECT (lcTmpItem)
  *lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItem)
  *=CURSORSETPROP("Buffering", 3, lcTmpItem)
  *INDEX ON cInvType + Style TAG Style
  *=CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItem)
  *SET ORDER TO Style
  **--Open Temp ItemLoc Cursor to Get needed Quantity Fields
  *lcTmpItemLoc = gfTempName()
  *lcWhereCond  = "cInvType = '" + lcInvtype + "' AND Dyelot = ''"
  *
  *lcStatement  = "SELECT  Style, " +;
  *               "        totord, totstk, totwip, nStkVal" +;
  *               "  FROM  ItemLoc WITH (INDEX = STYDYE)" +;
  *               "  WHERE " + lcWhereCond
  *
  **--Run the Satatement
  *lnConnectionHandler = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTmpItemLoc, "ITEMLOC",;
  *                                          oAriaApplication.ActiveCompanyConStr,;
  *                                          3, "SAVE", SET("Datasession"))
  *
  **--Check Connection Result
  *IF lnConnectionHandler # 1
  *  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandler, .T.)
  *  RETURN ""
  *ENDIF
  *
  *SELECT (lcTmpItemLoc)
  *lnOldBuffMode = CURSORGETPROP("Buffering", lcTmpItemLoc)
  *=CURSORSETPROP("Buffering", 3, lcTmpItemLoc)
  *INDEX ON Style TAG Style
  *=CURSORSETPROP("Buffering", lnOldBuffMode, lcTmpItemLoc)
  *SET ORDER TO Style
  IF LCMAJORORNON = 'M'
    LOCAL LCSTAT
    LCTMPITEM2 = GFTEMPNAME()

    LCSTAT = "SELECT cStyMajor, SUM(TotStk) AS TotStk, SUM(TotOrd) AS TotOrd, SUM(TotWip) AS TotWip, SUM(nStkVal) AS nStkVal" +;
      "  FROM ITEM (INDEX=CSTYLE) " +;
      " WHERE cInvType = '" + LCINVTYPE + "'" +;
      " GROUP BY cStyMajor" +;
      " ORDER By cStyMajor"

    *--Create Temp Item Cursor
    LNCONNECTIONHANDLER = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSTAT, LCTMPITEM2, "ITEM",;
      OARIAAPPLICATION.ACTIVECOMPANYCONSTR,;
      3, "SAVE", SET("Datasession"))

    *--Check Connection Result
    IF LNCONNECTIONHANDLER # 1
      =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("SQLRUN", LNCONNECTIONHANDLER, .T.)
      RETURN ""
    ENDIF

    LNOLDBUFFMODE = CURSORGETPROP("Buffering", LCTMPITEM2)
    =CURSORSETPROP("Buffering", 3, LCTMPITEM2)
    INDEX ON CSTYMAJOR TAG STYLE
    =CURSORSETPROP("Buffering", LNOLDBUFFMODE, LCTMPITEM2)
    SET ORDER TO STYLE
  ENDIF
  *B128949,1 WSH 08/07/2005 [End]
ENDIF

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\STYBROW.H

PRIVATE LCTODO, LLWASSEL, LCBRFIELDS
LLWASSEL = .F.
LCBRFIELDS = ''

*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE,LCSTYBROW
LCARIAHFILE = ''
LCSTYBROW = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
  LCSTYBROW  =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("STYBROW")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = IIF(lcMajorOrNon $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)]+;
*!*	  [,lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+LANG_COLOR+["],;
*!*	  [cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) +;
*!*	  [,DESC :20 :H="]+LANG_DESC+[",DESC1 :35 :H="]+LANG_LDESC+[",]+;
*!*	  [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+LANG_SEASON+;
*!*	  [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+LANG_DIV+[",]

*!*	lcBrFields = lcBrFields+;
*!*	[pricea :10:H="]+LANG_PRICEA+[" , PRICEB :10:H="]+LANG_PRICEB+[",PRICEC :10:H="]+LANG_PRICEC+[",]
LCBRFIELDS = IIF(LCMAJORORNON $ 'NI', [STYLE :30 :H=ALLTRIM(lcClrTtl)]+;
  [,lcColor=gfCodDes(SUBSTR(STYLE,lnClrStrt,lnClrWidth),'COLOR'):20 :H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COLOR,OARIAAPPLICATION.GETHEADERTEXT("LANG_COLOR",LCSTYBROW))+["],;
  [cStyMajor :30 :H=ALLTRIM(lcStyTtl)]) +;
  [,DESC :20 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROW))+;
  [",DESC1 :35 :H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LDESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_LDESC",LCSTYBROW))+[",]+;
  [lcSesDesc=gfCodDes(Season,'SEASON'):20 :H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SEASON,OARIAAPPLICATION.GETHEADERTEXT("LANG_SEASON",LCSTYBROW))+;
  [",lcDivDesc=gfCodDes(cdivision,'CDIVISION') :20:H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DIV,OARIAAPPLICATION.GETHEADERTEXT("LANG_DIV",LCSTYBROW))+[",]

LCBRFIELDS = LCBRFIELDS+;
  [pricea :10:H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROW))+;
  [" , PRICEB :10:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEB,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEB",LCSTYBROW))+;
  [",PRICEC :10:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEC,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEC",LCSTYBROW))+[",]
*N000682,1 11/20/2012 MMT Globlization changes[End]
IF LCMAJORORNON = 'M'

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *lcBrFields = lcBrFields+;
  [lnTotWIP=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
  [lnTotStk=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
  [lnTotORD=gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
  [lnOts   =gfSumItem(PADR(ALLTRIM(CSTYMAJOR),lnMajLen),'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *!*	  lcBrFields = lcBrFields+;
  *!*	    [lnTotWIP=gfSumItem(CSTYMAJOR,'TotWip',lcTmpItem2):12:h="]+LANG_WIP+[",]+;
  *!*	    [lnTotStk=gfSumItem(CSTYMAJOR,'totstk',lcTmpItem2):12:h="]+LANG_STOCK+[",]+;
  *!*	    [lnTotORD=gfSumItem(CSTYMAJOR,'totord',lcTmpItem2):12:h="]+LANG_ORDER+[",]+;
  *!*	   [lnOts   =gfSumItem(CSTYMAJOR,'totWip+totStk-totOrd',lcTmpItem2):12:H="]+LANG_OTS+[",]
  LCBRFIELDS = LCBRFIELDS+;
    [lnTotWIP=gfSumItem(CSTYMAJOR,'TotWip',lcTmpItem2):12:h="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROW))+[",]+;
    [lnTotStk=gfSumItem(CSTYMAJOR,'totstk',lcTmpItem2):12:h="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROW))+[",]+;
    [lnTotORD=gfSumItem(CSTYMAJOR,'totord',lcTmpItem2):12:h="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROW))+[",]+;
    [lnOts   =gfSumItem(CSTYMAJOR,'totWip+totStk-totOrd',lcTmpItem2):12:H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROW))+[",]
  *N000682,1 11/20/2012 MMT Globlization changes[End]
  *B128949,1 WSH 08/07/2005 [End]

ELSE

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *lcBrFields = lcBrFields+;
  [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
  [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
  [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
  [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *!*	  lcBrFields = lcBrFields+;
  *!*	    [TotWip:12:h="]+LANG_WIP+[",]+;
  *!*	    [totstk:12:h="]+LANG_STOCK+[",]+;
  *!*	    [totord:12:h="]+LANG_ORDER+[",]+;
  *!*	[lnOts = totWip+totStk-totOrd :12:H="]+LANG_OTS+[",]
  LCBRFIELDS = LCBRFIELDS+;
    [TotWip:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROW))+[",]+;
    [totstk:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROW))+[",]+;
    [totord:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROW))+[",]+;
    [lnOts = totWip+totStk-totOrd :12:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROW))+[",]
  *N000682,1 11/20/2012 MMT Globlization changes[END]

  *B128949,1 WSH 08/07/2005 [End]

ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = lcBrFields+;
*!*	[CSTYGRADE :H="]+LANG_GRADE+[", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+LANG_ROYAL+[" , PATTERN :H="]+LANG_PATRN+[", STATUS :H="]+LANG_STATUS+[",]

*!*	lcBrFields = lcBrFields + ;
*!*	  [SCALE :H="]+LANG_SCALE+[", PREPAK :H="]+LANG_PREPAK+[", CBUYPREPK :H="]+LANG_BPREPK+[", QTY_CTN :H="]+LANG_QTYCRT+[", COMMISSION :H="]+LANG_COMM+[", LINK_CODE :H="]+LANG_LNKCD+[",]+;
*!*	[lcMaked = IIF(MAKE,'Y','N') :H="]+LANG_MAKE+[", NMCOST1 :H="]+LANG_MCST1+[" , NMCOST2 :H="]+LANG_MCST2+[", NMCOST3 :H="]+LANG_MCST3+[", NMCOST4 :H="]+LANG_MCST4+[",NMCOST5 :H="]+LANG_MCST5+[",]


*!*	lcBrFields = lcBrFields + ;
*!*	[NICOST1 :H="]+LANG_ICST1+[", NICOST2 :H="]+LANG_ICST2+[", NICOST3 :H="]+LANG_ICST3+[", NICOST4 :H="]+LANG_ICST4+[", NICOST5 :H="]+LANG_ICST5+[",]
LCBRFIELDS = LCBRFIELDS+;
  [CSTYGRADE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GRADE,OARIAAPPLICATION.GETHEADERTEXT("LANG_GRADE",LCSTYBROW))+;
  [", lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :20 :H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ROYAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_ROYAL",LCSTYBROW))+;
  [" , PATTERN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PATRN,OARIAAPPLICATION.GETHEADERTEXT("LANG_PATRN",LCSTYBROW))+;
  [", STATUS :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_STATUS",LCSTYBROW))+[",]

LCBRFIELDS = LCBRFIELDS + ;
  [SCALE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SCALE,OARIAAPPLICATION.GETHEADERTEXT("LANG_SCALE",LCSTYBROW))+;
  [", PREPAK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PREPAK,OARIAAPPLICATION.GETHEADERTEXT("LANG_PREPAK",LCSTYBROW))+;
  [", CBUYPREPK :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_BPREPK,OARIAAPPLICATION.GETHEADERTEXT("LANG_BPREPK",LCSTYBROW))+;
  [", QTY_CTN :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_QTYCRT,OARIAAPPLICATION.GETHEADERTEXT("LANG_QTYCRT",LCSTYBROW))+;
  [", COMMISSION :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COMM,OARIAAPPLICATION.GETHEADERTEXT("LANG_COMM",LCSTYBROW))+;
  [", LINK_CODE :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_LNKCD,OARIAAPPLICATION.GETHEADERTEXT("LANG_LNKCD",LCSTYBROW))+[",]+;
  [lcMaked = IIF(MAKE,'Y','N') :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MAKE,OARIAAPPLICATION.GETHEADERTEXT("LANG_MAKE",LCSTYBROW))+;
  [", NMCOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST1",LCSTYBROW))+;
  [" , NMCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST2",LCSTYBROW))+;
  [", NMCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST3",LCSTYBROW))+;
  [", NMCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST4",LCSTYBROW))+;
  [",NMCOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MCST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_MCST5",LCSTYBROW))+[",]


LCBRFIELDS = LCBRFIELDS + ;
  [NICOST1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST1,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST1",LCSTYBROW))+;
  [", NICOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST2",LCSTYBROW))+;
  [", NICOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST3",LCSTYBROW))+;
  [", NICOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST4",LCSTYBROW))+;
  [", NICOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ICST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_ICST5",LCSTYBROW))+[",]
*N000682,1 11/20/2012 MMT Globlization changes[End]


*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*lcBrFields = lcBrFields+;
[NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
[lnAvegCost=gfCalAvCst() :H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', "", [",NSTKVAL :H="]+LANG_STKVAL)+[",SOLDOUT :H="]+LANG_SOLDDT+[",START :H="]+LANG_STRTDT+[",LOCATION :H="]+LANG_DBIN+[",LINVSTY :H=
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcBrFields = lcBrFields+;
*!*	  [NPRCOST2 :H="]+LANG_PCST2+[",NPRCOST3 :H="]+LANG_PCST3+[",NPRCOST4 :H="]+LANG_PCST4+[",NPRCOST5 :H="]+LANG_PCST5+[",TOTCOST :H="]+LANG_TOTCST+[",]+;
*!*	[lnAvegCost=gfCalAvCst(']+lcMajorOrNon+['):H="]+LANG_AVECST+IIF(lcMajorOrNon $ 'NI', [",NSTKVAL], [",lnTotVal=gfSumItem(CSTYMAJOR,'NSTKVAL',lcTmpItem2)])+[ :H="]+LANG_STKVAL+[",SOLDOUT :H="]+LANG_SO

*!*	*B128949,1 WSH 08/07/2005 [End]

*!*	lcBrFields = lcBrFields+;
*!*	  [MARKA :H="]+LANG_MARKA+[",MARKB:H="]+LANG_MARKB+[",MARKC :H="]+LANG_MARKC+[",]+;
*!*	[CCONSINFO1 :H="]+LANG_CONSI1+[",CCONSINFO2 :H="]+LANG_CONSI2+["]
LCBRFIELDS = LCBRFIELDS+;
  [NPRCOST2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST2,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST2",LCSTYBROW))+;
  [",NPRCOST3 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST3,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST3",LCSTYBROW))+;
  [",NPRCOST4 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST4,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST4",LCSTYBROW))+;
  [",NPRCOST5 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PCST5,OARIAAPPLICATION.GETHEADERTEXT("LANG_PCST5",LCSTYBROW))+;
  [",TOTCOST :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_TOTCST,OARIAAPPLICATION.GETHEADERTEXT("LANG_TOTCST",LCSTYBROW))+[",]+;
  [lnAvegCost=gfCalAvCst(']+LCMAJORORNON+['):H="]+;
  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_AVECST,OARIAAPPLICATION.GETHEADERTEXT("LANG_AVECST",LCSTYBROW))+;
  IIF(LCMAJORORNON $ 'NI', [",NSTKVAL], [",lnTotVal=gfSumItem(CSTYMAJOR,'NSTKVAL',lcTmpItem2)])+;
  [ :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STKVAL,OARIAAPPLICATION.GETHEADERTEXT("LANG_STKVAL",LCSTYBROW))+;
  [",SOLDOUT :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SOLDDT,OARIAAPPLICATION.GETHEADERTEXT("LANG_SOLDDT",LCSTYBROW))+[",]

*B128949,1 WSH 08/07/2005 [End]

LCBRFIELDS = LCBRFIELDS+;
  [MARKA :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKA,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKA",LCSTYBROW))+;
  [",MARKB:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKB,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKB",LCSTYBROW))+;
  [",MARKC :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MARKC,OARIAAPPLICATION.GETHEADERTEXT("LANG_MARKC",LCSTYBROW))+[",]+;
  [CCONSINFO1 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI1,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI1",LCSTYBROW))+;
  [",CCONSINFO2 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CONSI2,OARIAAPPLICATION.GETHEADERTEXT("LANG_CONSI2",LCSTYBROW))+["]
*N000682,1 11/20/2012 MMT Globlization changes[END]


LNCURALIAS = SELECT()
SELECT (LCTMPITEM)
LCSTYORDER = TAG()

DO CASE
CASE '*' $ LCCOLOR AND '*' $ LCSEASON

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
  [pricea :H="]+LANG_PRICEA+[",]+;
  [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
  [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
  [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
  [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]+;
  [Fabric :19 :H="]+LANG_FABRIC+["]
  LCBRFIELDS = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROW))+[",]+;
    [pricea :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROW))+[",]+;
    [TotWip:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROW))+[",]+;
    [totstk:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROW))+[",]+;
    [totord:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROW))+[",]+;
    [lnOts = totWip+totStk-totOrd :12:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROW))+[",]+;
    [Fabric :19 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROW))+["]
  *N000682,1 11/20/2012 MMT Globlization changes[End]
  *B128949,1 WSH 08/07/2005 [End]

CASE '*' $ LCCOLOR

  *B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
  *lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
  [pricea :H="]+LANG_PRICEA+[",]+;
  [lnTotWIP=gfSumItem(Style,'TotWip',lcTmpItemLoc):12:h="]+LANG_WIP+[",]+;
  [lnTotStk=gfSumItem(Style,'totstk',lcTmpItemLoc):12:h="]+LANG_STOCK+[",]+;
  [lnTotORD=gfSumItem(Style,'totord',lcTmpItemLoc):12:h="]+LANG_ORDER+[",]+;
  [lnOts   =gfSumItem(Style,'totWip+totStk-totOrd',lcTmpItemLoc):12:H="]+LANG_OTS+[",]+;
  [Fabric :19 :H="]+LANG_FABRIC+["]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *!*	  lcBrFields = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+LANG_DESC+[",]+;
  *!*	    [pricea :H="]+LANG_PRICEA+[",]+;
  *!*	    [TotWip:12:h="]+LANG_WIP+[",]+;
  *!*	    [totstk:12:h="]+LANG_STOCK+[",]+;
  *!*	    [totord:12:h="]+LANG_ORDER+[",]+;
  *!*	    [lnOts = totWip+totStk-totOrd :12:H="]+LANG_OTS+[",]+;
  *!*	    [Fabric :19 :H="]+LANG_FABRIC+["]
  LCBRFIELDS = [STYLE :30 :H=ALLTRIM(lcClrTtl),DESC :45:H="]+;
    IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_DESC,OARIAAPPLICATION.GETHEADERTEXT("LANG_DESC",LCSTYBROW))+[",]+;
    [pricea :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_PRICEA,OARIAAPPLICATION.GETHEADERTEXT("LANG_PRICEA",LCSTYBROW))+[",]+;
    [TotWip:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_WIP,OARIAAPPLICATION.GETHEADERTEXT("LANG_WIP",LCSTYBROW))+[",]+;
    [totstk:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_STOCK",LCSTYBROW))+[",]+;
    [totord:12:h="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ORDER,OARIAAPPLICATION.GETHEADERTEXT("LANG_ORDER",LCSTYBROW))+[",]+;
    [lnOts = totWip+totStk-totOrd :12:H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_OTS,OARIAAPPLICATION.GETHEADERTEXT("LANG_OTS",LCSTYBROW))+[",]+;
    [Fabric :19 :H="]+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_FABRIC,OARIAAPPLICATION.GETHEADERTEXT("LANG_FABRIC",LCSTYBROW))+["]
  *N000682,1 11/20/2012 MMT Globlization changes[END]
  *B128949,1 WSH 08/07/2005 [End]

  *B128950,1 AMH Fix bug of variable not found when browse [Start]
  *lcScope = [=gfvSeasonC]
  LCSCOPE = [=gfvSeasonC()]
  *B128950,1 AMH [End]

OTHERWISE
  IF !('*' $ LCSEASON)

    *B128950,1 AMH Fix bug of variable not found when browse [Start]
    *lcScope = [=gfvSeasonC]
    LCSCOPE = [=gfvSeasonC()]
    *B128950,1 AMH [End]

  ENDIF
ENDCASE

SELECT (LCTMPITEM)

LOCAL LCBROWCHR
LCITEM    = RTRIM(LCITEM)
LCBROWCHR = RIGHT(LCITEM, 1)
LCITEM    = IIF(LCBROWCHR == '?', SUBSTR(LCITEM, 1, LEN(LCITEM) - 1) , LCITEM)

*B039435,1 AMH Fix bug of there is quat in the item value [Start]
*lcForExp  = IIF(lcMajorOrNon = "N", "Style LIKE '" + PADR(lcItem, lnMajLen) + "%'" + IIF(EMPTY(lcForExp), "", " AND " + lcForExp), lcForExp)
PUBLIC LCITEMMJR
LCITEMMJR = PADR(LCITEM, LNMAJLEN) + "%"
LCFOREXP  = IIF(LCMAJORORNON = "N", "Style LIKE ?m.lcItemMjr" + IIF(EMPTY(LCFOREXP), "", " AND " + LCFOREXP), LCFOREXP)
*B039435,1 AMH [End]

SELECT (LCTMPITEM)

*! B128044,1 WSH 05/18/2005, Fix bug of displaying only one color while browsing Fabrics from Material PO. [Start]
*lcToDo = 'DO FORM BROWSE ' +;
'WITH lcBrFields,"' + lcTitle + '",' +;
'["' + PADR(ALLTRIM(lcInvtype), 4) + '"],"' + IIF(EMPTY(lcForExp), '', lcForExp) + '",' +;
'.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
'"' + oAriaApplication.cSQLDBID + '",' +;
'"STYLE", "' + IIF(lcMajorOrNon = "N", "STYLE", "CSTYLE") + '", "' + PADR(ALLTRIM(lcInvType),4) + RTRIM(lcItem) + '"' +;
' TO llWasSel'

*B039435,1 AMH Fix bug of there is quat in the item value [Start]
*lcToDo = 'DO FORM BROWSE ' +;
'WITH lcBrFields,"' + lcTitle + '",' +;
'["' + PADR(ALLTRIM(lcInvtype), 4) + '"],"' + IIF(EMPTY(lcForExp), '', lcForExp) + '",' +;
'.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
'"' + oAriaApplication.cSQLDBID + '",' +;
'"STYLE", "' + IIF(lcMajorOrNon $ "IN", "STYLE", "CSTYLE") + '", "' + PADR(ALLTRIM(lcInvType),4) + RTRIM(lcItem) + '"' +;
' TO llWasSel'
LCTODO = 'DO FORM BROWSE ' +;
  'WITH lcBrFields,"' + LCTITLE + '",' +;
  '["' + PADR(ALLTRIM(LCINVTYPE), 4) + '"],"' + IIF(EMPTY(LCFOREXP), '', LCFOREXP) + '",' +;
  '.F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,"ITEM",' +;
  '"' + OARIAAPPLICATION.CSQLDBID + '",' +;
  '"STYLE", "' + IIF(LCMAJORORNON $ "IN", "STYLE", "CSTYLE") + '", [' + PADR(ALLTRIM(LCINVTYPE),4) + RTRIM(LCITEM) + ']' +;
  ' TO llWasSel'
*B039435,1 AMH [End]

*! B128044,1 WSH 05/18/2005, [End]


=GFEXECUTE(LCTODO)

IF LLWASSEL
  LCITEM  = EVALUATE(LCTMPITEM + '.Style')
  LCCOLOR = EVALUATE(LCTMPITEM + '.Season')
ELSE
  LCITEM  = SPACE(12)
  LCCOLOR = SPACE(6)
ENDIF
*!B039082,1 WSH, 02/23/2005, [End]

IF LLRETALIAS
  SELECT (LNCURALIAS)
ENDIF

RETURN LCITEM

*!*************************************************************
*! Name : gfSumItem (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To sum a specific field from a specific file
*!*************************************************************
*! Passed Parameters :
*!      lcItem  -> Fabric or Style Code
*!      lcFieldNam -> Field name to be summed
*!        lcFile    -> Alias to be selected
*!*************************************************************
*! Example :
*!        =gfSumItem("0001","STYLE     -BLACK",Season,lcTmpFile)
*!*************************************************************
FUNCTION GFSUMITEM
LPARAMETERS LCITEM, LCFIELDNAM, LCFILE

PRIVATE LNTOTFLD, LCALIAS
LCALIAS  = SELECT()
LNTOTFLD = 0

SELECT (LCFILE)

*B039082,1 WSH 02/21/2005, Seek as there is no relation in gfItemBrow. [Sart]
=SEEK(LCITEM)
*B039082,1 WSH 02/21/2005, [End]

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*SUM &lcFieldNam TO lnTotFld REST WHILE Style = lcItem
LNTOTFLD = EVALUATE(LCFILE + '.' + LCFIELDNAM)
*B128949,1 WSH 08/07/2005 [End]


SELECT (LCALIAS)

*B039082,1 WSH 02/21/2005, No need to locate. [Sart]
*GO RECNO()
*B039082,1 WSH 02/21/2005, [End]

RETURN INT(LNTOTFLD)

*!*************************************************************
*! Name : gfCalAvCst
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To calculate the average cost for each style
*!*************************************************************
*! Example :
*!        =gfCalAvCst()
*!*************************************************************
FUNCTION GFCALAVCST

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
LPARAMETERS LCMAJORORNON
*B128949,1 WSH 08/07/2005 [End]

PRIVATE LNTOTSTK, LNTOTSTVAL , LNAVCSTVAL
STORE 0 TO LNTOTSTK , LNTOTSTK , LNAVCSTVAL

*E038245,1 WSH Remove Second Item Cursor as it is not needed and Calculate from ItemLoc File [Start]
*lnTotStk   = gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'TotStk',lcTmpItem2)
*lnTotStVal = gfSumItem(PADR(ALLTRIM(&lcTmpItem2..CSTYMAJOR),lnMajLen),'NSTKVAL',lcTmpItem2)

*B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File. [Start]
*lnTotStk   = &lcTmpItemLoc..TotStk
*lnTotStVal = &lcTmpItemLoc..NSTKVAL
IF LCMAJORORNON = 'M'
  LNTOTSTK   = GFSUMITEM(EVALUATE(LCTMPITEM + '.CSTYMAJOR'), 'TotStk', LCTMPITEM2)
  LNTOTSTVAL = GFSUMITEM(EVALUATE(LCTMPITEM + '.CSTYMAJOR'), 'NSTKVAL', LCTMPITEM2)
ELSE
  LNTOTSTK   = EVALUATE(LCTMPITEM + '.TotStk')
  LNTOTSTVAL = EVALUATE(LCTMPITEM + '.NSTKVAL')
ENDIF
*B128949,1 WSH 08/07/2005 [End]

*E038245,1 WSH [End]

IF LNTOTSTK > 0
  LNAVCSTVAL = LNTOTSTVAL / LNTOTSTK
ELSE
  *E038245,1 WSH Remove Second Item Cursor as it is not needed and Calculate from ItemLoc File [Start]
  *RETURN &lcTmpItem2..Ave_Cost
  RETURN AVE_COST
  *E038245,1 WSH [End]
ENDIF
RETURN (LNAVCSTVAL)

*!*************************************************************
*! Name : gfvSeasonC (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To validate the style selected by the user in the browse
*!         if it use the same season or not
*!*************************************************************
*! Example :
*!        =gfvSeasonC()
*!*************************************************************
FUNCTION GFVSEASONC

*E038245,1 WSH Remove Second Item Cursor as it is not needed [Start]
*IF &lcTmpItem2..Season = lcSeason  OR ALLTRIM(&lcTmpItem2..Season) = 'Y'
IF SEASON = LCSEASON  OR ALLTRIM(SEASON) = 'Y'
  *E038245,1 WSH [End]

ELSE
  =MESSAGEBOX("You are restricted to styles from season "+LCSEASON+"! RETRY")
ENDIF

*!*************************************************************
*! Name       : gfDispRep
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 07/12/2004.
*! Purpose    : To run a report from a screen without using OG
*! Entry      : B038282,1 Missing function
*!*************************************************************
*! PARAMETERS :
*!     lcRprtNam : The report to run
*!*************************************************************
FUNCTION GFDISPREP
PARAMETERS  LCRPRTNAM

LOCAL LCREPORTHOME
LCREPORTHOME = OARIAAPPLICATION.REPORTHOME

DO CASE
  *!B038457,1 SMM 09/01/2004 Solve If there are spaces in the path an error occured [START]
  *!*	  CASE oAriaApplication.gcDevice = "SCREEN"
  *!*	     IF oAriaApplication.glHeader
  *!*	       REPORT FORM &lcReportHome.&lcRprtNam PREVIEW
  *!*	     ELSE
  *!*	       REPORT FORM &lcReportHome.&lcRprtNam PREVIEW PLAIN
  *!*	     ENDIF
  *!*	  CASE oAriaApplication.gcDevice = "PRINTER"
  *!*	    IF oAriaApplication.glHeader
  *!*	      REPORT FORM &lcReportHome.&lcRprtNam TO PRINTER NOCONSOLE NOEJECT
  *!*	    ELSE
  *!*	      REPORT FORM &lcReportHome.&lcRprtNam TO PRINTER NOEJECT NOCONSOLE PLAIN
  *!*      ENDIF

CASE OARIAAPPLICATION.GCDEVICE = "SCREEN"
  IF OARIAAPPLICATION.GLHEADER
    REPORT FORM "&lcReportHome.&lcRprtNam" PREVIEW
  ELSE
    REPORT FORM "&lcReportHome.&lcRprtNam" PREVIEW PLAIN
  ENDIF
CASE OARIAAPPLICATION.GCDEVICE = "PRINTER"
  IF OARIAAPPLICATION.GLHEADER
    REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER NOCONSOLE NOEJECT
  ELSE
    REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER NOEJECT NOCONSOLE PLAIN
  ENDIF
  *!B038457,1 SMM 09/01/2004 [END]
ENDCASE

*!*************************************************************
*! Name       : gfSoftSeeK
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 08/09/2004.
*! Purpose    : To run Soft Seek from IN Range class
*! Entry      : B123704,1
*!*************************************************************
*! PARAMETERS :
*! lnKeyCode  : The last pressed Key
*! lcField    : The field which is searched
*!*************************************************************
FUNCTION GFSOFTSEEK
PARAMETERS  LNKEYCODE, LCFIELD
LOCAL LCSEARCKEY
LCSEARCHKEY = ''
DO FORM SOFTSEEK WITH ;
  CHR(LNKEYCODE), LEN(&LCFIELD) TO LCSEARCHKEY
RETURN LCSEARCHKEY


*!*	************************************************************************
*!*	Descrption    : Configuration Browse screen
*!*	Developer     : Hend Ghanem (HBG)
*!*	Date          : 11/02/2004
*!*	Entry #       : N037401
*!*	************************************************************************
*!* P A S S E D    P A R A M E T E R S
*!* 01)-- lcStyle        : Current selected Style
*!* 02)-- lcWareCode     : Current selected Warehouse
*!* 03)-- llCloseButton  : Flag to display Close button or not.
*!* 04)-- lcValue		: Entered Configuratin value
*!* 05)-- lcMode	     : The mode of the screen
*!*	************************************************************************
*!*	Modification  :
*!*	************************************************************************
FUNCTION GFCONFGBRW
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[Start]
*LPARAMETERS lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[Start]
LPARAMETERS LCSTYLE,LCWARECODE,LLCLOSEBUTTON,LLSETFOUCS,LCVALUE,LCMODE,LLALLCLR,LLSELECTBUT,LLALLSCL
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[End]
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[End]

LOCAL LCRETURNVALUE
LCRETURNVALUE= ''
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[Start]
*DO FORM ConfgBrw WITH lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode TO lcReturnValue
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[Start]
*DO FORM ConfgBrw WITH lcStyle,lcWareCode,llCloseButton,llSetFoucs,lcValue,lcMode,llAllClr,llSelectBut TO lcReturnValue
DO FORM CONFGBRW WITH LCSTYLE,LCWARECODE,LLCLOSEBUTTON,LLSETFOUCS,LCVALUE,LCMODE,LLALLCLR,LLSELECTBUT,LLALLSCL TO LCRETURNVALUE
*! C201210,2 MMT 01/11/2010 Add New Parameters to Style Configuration screen for all Scale case[End]
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[End]
RETURN LCRETURNVALUE

*!*	************************************************************************
*!*	Descrption    : Browse screen
*!*	Developer     : Hend Ghanem (HBG)
*!*	Date          : 11/02/2004
*!*	Entry #       :
*!*	************************************************************************
*-- P A S S E D    P A R A M E T E R S
*01)-- tcBrowseFields : Browse fields
*02)-- tcBrowseTitle  : Browse title
*03)-- tcKey          : Browse key Expression
*04)-- tcFor          : Browse Filter Expression
*05)-- tcOptions      : string holds enable status of the 4 bars in the default shortcut
*   --                  Find, Filter, Quick find and Order
*06)-- tlSelect       : Show Select Button or not
*07)-- tcSelectObj    : for single select, referecance to select button method
*07)-- tcSelectMethod : for single select, name of method to call when click select button
*09)-- tcUserShortCut : address of three diminsional array that holds additional customized bars
*   --                  in the browse shortcut.
*                       1: bar prompt
*   --                  2: Reference to object
*                       3: Method name to run when select this bar
*10)-- tcTempFile     : Name of temporary file that will have multiple selections
*11)-- tcSelField     : Name of field to be returned in multiple selections
*12)-- llHalfHeight   : Browse full height or half height
*16)-- lcFieldsNam    : Returned fields
*17)-- lcArrName      : Returned Array
*!*	************************************************************************
*!*	Modification  :
*!*	************************************************************************
FUNCTION GFBROWSCR
LPARAMETERS TCBROWSEFIELDS,TCBROWSETITLE ,TCKEY,TCFOR   ,TCOPTIONS ,TLSELECT  ,;
  TOSELECTOBJ   ,TCSELECTMETHOD,TCUSERSHORTCUT,TCTEMPFILE,TCSELFIELD,;
  LLHALFHEIGHT,LCFIELDSNAM,LCARRNAME,LLCHCKKEY,LCALIASNAME,LLGETTREE, ;
  LCBROWSEFILENAME

LOCAL LCRETURNVALUE
LCRETURNVALUE= ''
DO FORM BROWSE WITH TCBROWSEFIELDS, TCBROWSETITLE , TCKEY, TCFOR , TCOPTIONS , TLSELECT , TOSELECTOBJ TO LCRETURNVALUE
RETURN LCRETURNVALUE

*!*************************************************************
*! Name : gfGetUOMData
*! Auth : Wael M. Abo-Shawareb (WSH)
*! Date : 07/28/2004.
*!*************************************************************
*! Synopsis : To Get UOM Data from UOM Table
*!*************************************************************
*! Passed Parameters :
*!      lcRelCode -> UOM-Relation Code     "By Reference"
*!      lcUOMFrom -> Convert from UOM-Code "By Reference"
*!      lcUOMTo   -> Convert to UOM-Code   "By Reference"
*!      lnConv    -> Conversion Factor     "By Reference"
*!      llDispMsg -> Flag to Display "Add, Browse, Reenter" message
*!                   or not if Relation not found. "By Value"
*!*************************************************************
*! Returned : 1 -> The User Select Add But Relation not Added.
*!            2 -> Relation Added, Found, or the User Browse and select Relation.
*!            3 -> The User Choose Reenter or Relation not found and not added
*!*************************************************************
*! Example :
*!        =gfGetUOMData(@lcRelCode, @lcUOMBuy, @lcUOMUse, @lnConv, .T.)
*!*************************************************************
*! E038220,1 WSH
FUNCTION GFGETUOMDATA
LPARAMETERS LCRELCODE, LCUOMFROM, LCUOMTO, LNCONV, LLDISPMSG

LOCAL LNRETVAL, LLCODESOPENED, LCOLDALIAS, LCWHERECOND, LCDEFUOM, LNHANDLER, LLADDREL, LLBROWSEREL
LLCODESOPENED = .F.
LNRETVAL      = 3
LCOLDALIAS    = SELECT()
LCDEFUOM      = SPACE(6)
LLADDREL      = .F.
LLBROWSEREL   = .F.


*N000682,1 11/20/2012 MMT Globlization changes[Start]
LOCAL LCARIAHFILE
LCARIAHFILE = ''
IF OARIAAPPLICATION.OACTIVELANG.CLANG_ID <> "EN"
  LCARIAHFILE =OARIAAPPLICATION.GETCLASSHEADERFILE(ADDBS(UPPER(ALLTRIM(OARIAAPPLICATION.LANGPATH))) + "PRGS\SY\" + ALLTRIM("ARIA")+"_"+"H" +".XML")
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[END]
IF EMPTY(LCRELCODE) AND (LCUOMFROM == LCUOMTO) AND !EMPTY(LNCONV) AND (LNCONV <> 1)
  *--Can not create a Relation between a UOM Code and itself.
  =GFMODALGEN('QRM00414B42001', 'DIALOG')
  SELECT (LCOLDALIAS)
  RETURN 3
ENDIF

LOCAL LCUOMCURS
LCUOMCURS = GFTEMPNAME()

DIMENSION LAUOMDATA[4]
LAUOMDATA = ''

LOCAL LCUOMCODE, LCUOM_B, LCUOM_V, LNCONF
LCUOMCODE = LCRELCODE
LCUOM_B   = LCUOMFROM
LCUOM_V   = LCUOMTO
LNCONF    = IIF(EMPTY(LCRELCODE), LNCONV, 0)
LLDISPMSG = IIF(EMPTY(LCRELCODE), LLDISPMSG, .F.)

IF !USED('Codes')
  LLCODESOPENED = GFOPENFILE(OARIAAPPLICATION.DATADIR + 'CODES', 'CCODE_NO', 'SH')
ENDIF
LCDEFUOM = IIF(SEEK("DCUNTOFMGR", "CODES", "CCODE_NO"), CODES.CCODE_NO, "EAC   ")

*--Open UOM Cursor based on passed parameters.
LOCAL LCUOMTAG

IF !EMPTY(LCUOMCODE)
  LCWHERECOND = "CUOMCODE = '" + LCUOMCODE + "'"
  LCUOMTAG = 'UOMCODE'
ELSE
  LCUOM_B = IIF(!EMPTY(LCUOM_B), LCUOM_B, LCDEFUOM)
  LCUOM_V = IIF(!EMPTY(LCUOM_V), LCUOM_V, LCDEFUOM)
  *! B609740,1 MMT 11/23/2011 Material conversion factor does not allow a value between 1.000 and 2.000[Start]
  *!*	  lcWhereCond = "CUOM_B = '" + lcUOM_B + "'" +;
  *!*	                " AND CUOM_V = '" + lcUOM_V + "'" +;
  *!*	                  IIF(!EMPTY(lnConf), " AND NCONF = " + STR(lnConf), "")
  LCWHERECOND = "CUOM_B = '" + LCUOM_B + "'" +;
    " AND CUOM_V = '" + LCUOM_V + "'" +;
    IIF(!EMPTY(LNCONF), " AND NCONF = " + STR(LNCONF,9,3), "")
  *! B609740,1 MMT 11/23/2011 Material conversion factor does not allow a value between 1.000 and 2.000[End]
  LCUOMTAG = 'UOM'
ENDIF
IF !GFOPNSQLFL("UOM", LCUOMCURS, LCWHERECOND, '', "UOMCODE")
  SELECT (LCOLDALIAS)
  RETURN 3
ENDIF

*--Case of one Relation found
IF RECCOUNT(LCUOMCURS) = 1
  LCRELCODE = EVALUATE(LCUOMCURS + '.cUOMCode')
  LCUOMFROM = EVALUATE(LCUOMCURS + '.cUOM_B')
  LCUOMTO   = EVALUATE(LCUOMCURS + '.cUOM_V')
  LNCONV    = EVALUATE(LCUOMCURS + '.nConF')
  LNRETVAL  = 2
ELSE
  IF LLDISPMSG
    *--Display Query Message based on Current Situation.
    LOCAL LNSELECT, LLWASSEL

    IF EOF(LCUOMCURS)
      IF EMPTY(LNCONF)
        *--No Relations Found Between <UOM_B> and <UOM_V>
        *--  "Add,Reenter"
        LNSELECT = GFMODALGEN('TRM00411B42006', 'DIALOG', RTRIM(LCUOM_B) + '|' + RTRIM(LCUOM_V))
        IF LNSELECT = 2   && Reenter
          LNSELECT = 3
        ENDIF
      ELSE
        *--No Relations Found Between <UOM_B> and <UOM_V> with Conversion <lnConf>
        *--  "Add,Browse,Reenter"
        *! B609740,1 MMT 11/23/2011 Material conversion factor does not allow a value between 1.000 and 2.000[Start]
        *lnSelect = gfModalGen('TRM00412B42003', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V) + '|' + ALLTRIM(STR(lnConf)))
        LNSELECT = GFMODALGEN('TRM00412B42003', 'DIALOG', RTRIM(LCUOM_B) + '|' + RTRIM(LCUOM_V) + '|' + ALLTRIM(STR(LNCONF,9,3)))
        *! B609740,1 MMT 11/23/2011 Material conversion factor does not allow a value between 1.000 and 2.000[End]
      ENDIF
    ELSE
      *--More than one Relation Found Between <UOM_B> and <UOM_V>
      *--  "Add,Browse,Reenter"
      LNSELECT = GFMODALGEN('TRM00413B42003', 'DIALOG', RTRIM(LCUOM_B) + '|' + RTRIM(LCUOM_V))
    ENDIF

    DO CASE
      *--Case Select Add
    CASE LNSELECT = 1
      LLADDREL = .T.
      *--Case Select Browse
    CASE LNSELECT = 2
      LCWHERECOND = "CUOM_B = '" + LCUOM_B + "'" +;
        " AND CUOM_V = '" + LCUOM_V + "'"

      IF EMPTY(LNCONF) OR GFOPNSQLFL("UOM", LCUOMCURS, LCWHERECOND, '', "UOM")
        LLBROWSEREL = .T.
      ENDIF
    ENDCASE
  ELSE
    IF EOF(LCUOMCURS)
      IF EMPTY(LNCONF)
        *--Nothing to ADD or Browse for.
        *IF !EMPTY(lcUOMCode)
        IF EMPTY(LCUOMCODE)
          *=gfModalGen('TRM00415B42001', 'DIALOG')
          *ELSE
          =GFMODALGEN('TRM00411B42001', 'DIALOG', RTRIM(LCUOM_B) + '|' + RTRIM(LCUOM_V))
        ENDIF
      ELSE
        LLADDREL = .T.
      ENDIF
    ELSE
      IF EMPTY(LCUOMCODE) AND EMPTY(LNCONF)
        LLBROWSEREL = .T.
      ENDIF
    ENDIF
  ENDIF
ENDIF

*--Relation not found and not added then browse.
IF LLBROWSEREL
  IF !EOF()
    PRIVATE LCBRFIELDS

    *--Browse UOMs
    *lcBrFields = "CUOMCODE:H='"+LANG_ARIA_UOM_REL+"':22,lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_B+"':22,"+;
    "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_V+"':22,NCONF:H='"+LANG_ARIA_UOM_CONF+"':22"

    LCBRFIELDS = "lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_UOM_B,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_UOM_B",LCARIAHFILE))+"':22,"+;
      "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+;
      IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_UOM_V,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_UOM_V",LCARIAHFILE))+"':22,"+;
      "NCONF:H='"+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_UOM_CONF,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_UOM_CONF",LCARIAHFILE))+"':25"

    SELECT (LCUOMCURS)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *llWasSel = ARIABROW('', LANG_ARIA_UOM, .F., .F., .F., .F., '', '', 'CUOMCODE,CUOM_B,CUOM_V,NCONF', 'laUOMData')
    LLWASSEL = ARIABROW('', IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ARIA_UOM,OARIAAPPLICATION.GETHEADERTEXT("LANG_ARIA_UOM",LCARIAHFILE)),;
      .F., .F., .F., .F., '', '', 'CUOMCODE,CUOM_B,CUOM_V,NCONF', 'laUOMData')
    *N000682,1 11/20/2012 MMT Globlization changes[End]


    *--Fill UOM Variables if Relation Code Selected
    IF LLWASSEL
      LCRELCODE = LAUOMDATA[1]
      LCUOMFROM = LAUOMDATA[2]
      LCUOMTO   = LAUOMDATA[3]
      LNCONV    = LAUOMDATA[4]
      LNRETVAL  = 2
    ENDIF
  ELSE
    *--Nothing to ADD or Browse for.
    =GFMODALGEN('TRM00415B42001', 'DIALOG')
  ENDIF
ENDIF

IF LLADDREL
  LOCAL LCTRANCODE

  *--Add Record to UOM Table if we have a conversion factor.
  IF !EMPTY(LNCONF)
    LOCAL LCSTATEMENT, LCTEMPCURS, LCTOPREL
    LCTEMPCURS = GFTEMPNAME()

    *--Get Last Relation Code Value to Set New One.
    LCSTATEMENT = "SELECT TOP 1 CUOMCODE FROM UOM (INDEX=UOMCODE) ORDER BY CUOMCODE DESC"
    LNHANDLAR = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSTATEMENT, LCTEMPCURS, "UOM",;
      OARIAAPPLICATION.ACTIVECOMPANYCONSTR, 3, 'SAVE', SET("Datasession"))

    IF LNHANDLAR = 1
      *--Set the Highest Relation Code in UOM file
      LCTOPREL = STRTRAN(STR(VAL(EVALUATE(LCTEMPCURS + '.CUOMCODE')) + 1, 6, 0), ' ', '0')

      *--Add record in UOM table for new Relation.
      INSERT INTO (LCUOMCURS) (CUOMCODE, CUOM_B, CUOM_V, NCONF) VALUES (LCTOPREL, LCUOM_B, LCUOM_V, LNCONF)
      =GFADD_INFO(LCUOMCURS)

      IF GFUPDSQLFL(LCUOMCURS, 'CUOMCODE', 'UOM')
        LCRELCODE = LCTOPREL
        LCUOMFROM = LCUOM_B
        LCUOMTO   = LCUOM_V
        LNCONV    = LNCONF
        LNRETVAL  = 2
      ENDIF
    ELSE
      =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("SQLRUN", LNHANDLAR, .T.)
    ENDIF

    USE IN (LCTEMPCURS)
  ELSE
    LNRETVAL = 1
  ENDIF
ENDIF

USE IN (LCUOMCURS)
IF LLCODESOPENED
  USE IN CODES
ENDIF

SELECT (LCOLDALIAS)
RETURN LNRETVAL


*!*************************************************************
*! Name      	: lfGetSysColors
*! Developer 	: Mahmoud Said (MAH)
*! Date      	: 08/23/2004
*! Purpose   	: Return the nuber of colors used by the system.
*! Tracking #   : E038428,1 MAH 08/23/2004 The color background is not clear.
*!*************************************************************
*! Returns 	: Number of color used by the system
*!*************************************************************
FUNCTION LFGETSYSCOLORS
*-- Get handle to desktop device context
DECLARE INTEGER GetDC IN WIN32API INTEGER
LOCAL LNDCHANDLE
LNDCHANDLE = GETDC(0)

*-- Get device info
DECLARE INTEGER GetDeviceCaps IN WIN32API INTEGER, INTEGER
LOCAL LNPLANES, LNBITSPIXEL
LNPLANES    = GETDEVICECAPS(LNDCHANDLE, 14)
LNBITSPIXEL = GETDEVICECAPS(LNDCHANDLE, 12)

*-- Returns the number of colors used by the driver
LOCAL LNNUMCOLORS
LNNUMCOLORS = 2 ^ (LNPLANES * LNBITSPIXEL)

*-- The following code releases the GetDC handle
DECLARE INTEGER ReleaseDC IN Win32API INTEGER, INTEGER
=RELEASEDC(0, LNDCHANDLE)

RETURN LNNUMCOLORS



*!*****************************************************************************************
*! Name       : gfConvertDataType
*! Developer  : SMM - Saeed Mohammed Mostafa
*! Date       : 09/05/2004
*! Purpose    : Convert data type from Fox Format to SQL Format
*! Entry no.  : N038470
*! Parameters : lcFoxStruct : Comma separated string or array
*!*****************************************************************************************
FUNCTION GFCONVERTDATATYPE
LPARAMETERS LCFOXSTRUCT


LOCAL LCSQLSTRUCT,LCCOMMAPOS,LCSPACEPOS,LCFIELD,LCSTRUCT, LNCOUNT,LCSTR
LOCAL LCOPENBRACKETPOS, LCCLOSEBRACKETPOS

LCFILETYPE = IIF(TYPE("lcFoxStruct[1]") ='C', 'A', 'C')
IF LCFILETYPE = 'C' && Structure passed as comma separated string
  LOCAL LCRETSTRUCT
  LCSTR = ALLTRIM(LCFOXSTRUCT) + ","
  LCRETSTRUCT = ""
  DO WHILE LEN(LCSTR) > 1
    LCSPACEPOS = AT(SPACE(1),LCSTR)
    LCOPENBRACKETPOS  = AT('(',LCSTR)
    LCCLOSEBRACKETPOS = AT(')',LCSTR)
    LCCOMMAPOS = IIF(LCOPENBRACKETPOS < AT(',',LCSTR) AND LCCLOSEBRACKETPOS > ;
      AT(',',LCSTR),AT(',',LCSTR,2),AT(',',LCSTR))
    LCFIELD  = SUBSTR(LCSTR,1,LCSPACEPOS-1)
    LCSTRUCT = ALLTRIM(SUBSTR(LCSTR,LCSPACEPOS,LCCOMMAPOS-LCSPACEPOS))
    DO CASE && Check For the connection Driver
    CASE OARIAAPPLICATION.CONNECTIONDRIVER = 'SQL'
      DO CASE
      CASE LEFT(LCSTRUCT,1) = 'C'
        LCSQLSTRUCT = "Char" + SUBSTR(LCSTRUCT,2,LEN(LCSTRUCT)-1)
      CASE LEFT(LCSTRUCT,1) = 'N'
        LCSQLSTRUCT = "Numeric"+ SUBSTR(LCSTRUCT,2,LEN(LCSTRUCT)-1)
      CASE LCSTRUCT = 'L'
        LCSQLSTRUCT = "bit"
      CASE LCSTRUCT = 'D'
        LCSQLSTRUCT = "datetime"
      CASE LCSTRUCT = 'T'
        LCSQLSTRUCT = "datetime"
      CASE LCSTRUCT = 'MEMO'
        LCSQLSTRUCT = "Text"
      CASE LCSTRUCT = 'M'
        LCSQLSTRUCT = "Text"
      OTHERWISE
        LCSQLSTRUCT = ""
      ENDCASE
    ENDCASE
    IF EMPTY(LCRETSTRUCT)
      LCRETSTRUCT = LCFIELD + SPACE(1) + LCSQLSTRUCT
    ELSE
      LCRETSTRUCT = LCRETSTRUCT + "," + LCFIELD + SPACE(1) + LCSQLSTRUCT
    ENDIF
    LCSTR = ALLTRIM(SUBSTR(LCSTR,LCCOMMAPOS+1,LEN(LCSTR)-LCCOMMAPOS))
  ENDDO
  RETURN LCRETSTRUCT

ELSE && AFIELDS
  FOR LNCOUNT = 1 TO ALEN(LCFOXSTRUCT,1)
    * C = Character, D = Date, L = Logical, M = Memo, N = Numeric, F = Float, I = Integer
    DO CASE && Check For the connection Driver
    CASE OARIAAPPLICATION.CONNECTIONDRIVER = 'SQL'
      DO CASE
      CASE LCFOXSTRUCT[lnCount,2] = 'C'
        LCSQLSTRUCT = "Char"
      CASE LCFOXSTRUCT[lnCount,2] = 'N'
        LCSQLSTRUCT = "Numeric"
      CASE LCFOXSTRUCT[lnCount,2] = 'L'
        LCSQLSTRUCT = "bit"
      CASE LCFOXSTRUCT[lnCount,2]	= 'D'
        LCSQLSTRUCT = "datetime"
      CASE LCFOXSTRUCT[lnCount,2]	= 'T'
        LCSQLSTRUCT = "datetime"
      CASE LCFOXSTRUCT[lnCount,2]	= 'M'
        LCSQLSTRUCT = "Text"
      CASE LCFOXSTRUCT[lnCount,2]	= 'MEMO'
        LCSQLSTRUCT = "Text"
      CASE LCFOXSTRUCT[lnCount,2]	= 'F'
        LCSQLSTRUCT = "Float"
      CASE LCFOXSTRUCT[lnCount,2]	= 'I'
        LCSQLSTRUCT = "Integer"
      OTHERWISE
        LCSQLSTRUCT = ""
      ENDCASE
    ENDCASE
    LCFOXSTRUCT[lnCount,2] = LCSQLSTRUCT
  ENDFOR
ENDIF





*!***********************************************************************************************************************
*! Name      	: lfGetExpTableName
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Get the table name, which is used in any expression.
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	:
*!  lcExpression : Expression
*!***********************************************************************************************************************
*! Returns 	: Table name
*!***********************************************************************************************************************
FUNCTION LFGETEXPTABLENAME
LPARAMETERS LCEXPRESSION

LOCAL LCTABLENAME
LCTABLENAME = ''

IF AT('(', LCEXPRESSION) = 0
  LCTABLENAME = ALLTRIM(LEFT(LCEXPRESSION, AT('.', LCEXPRESSION)))
ELSE
  *-- Get table name
  IF AT('.', LCEXPRESSION) = 0
    FOR LNINDEX = AT('.', LCEXPRESSION) - 1 TO 1 STEP -1
      IF LNINDEX = 1
        LCTABLENAME = ALLTRIM(LEFT(LCEXPRESSION, AT('.', LCEXPRESSION)))
        EXIT
      ENDIF

      IF !ISALPHA(SUBSTR(LCEXPRESSION, LNINDEX, 1))
        LCTABLENAME = ALLTRIM(SUBSTR(LCEXPRESSION, LNINDEX + 1, AT('.', LCEXPRESSION) - LNINDEX - 1))
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF

RETURN LCTABLENAME


*!***********************************************************************************************************************
*! Name      	: lfGetConditionLeftSide
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Get the left side name, which is used in any expression.
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	:
*!  lcExpression : Expression
*!  lcDBEngine   : DB engine ID
*!***********************************************************************************************************************
*! Returns 	: Table name
*!***********************************************************************************************************************
FUNCTION LFGETCONDITIONLEFTSIDE
LPARAMETERS LCEXPRESSION, LCDBENGINE

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)

LOCAL LCTORETEXPRESSION
LCTORETEXPRESSION = ''

DO CASE
CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
  RETURN LCEXPRESSION

CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
  IF AT('(', LCEXPRESSION) = 0
    LCTORETEXPRESSION = LCEXPRESSION
  ELSE
    *-- Convert SUBSTR   , PADR, UPPER, DTOS   , STR, ALLTRIM, LEFT, LOWER to the new list
    *-- Convert SUBSTRING,     ,      , CONVERT, STR, LTRIM  , LEFT,
    LOCAL LNINDEX
    FOR LNINDEX = 1 TO LEN(LCEXPRESSION)
      IF SUBSTR(LCEXPRESSION, LNINDEX, 1) == "("

        *-- Get Command
        LOCAL LNINTERNALINDEX
        FOR LNINTERNALINDEX = LNINDEX - 1 TO 1 STEP -1
          IF LNINTERNALINDEX = 1
            EXIT
          ENDIF

          IF !ISALPHA(SUBSTR(LCEXPRESSION, LNINTERNALINDEX, 1))
            EXIT
          ENDIF
        ENDFOR

        LOCAL LCCOMMAND, LCREPLACE
        LCREPLACE = ''

        *-- Replace command
        LCCOMMAND = UPPER(ALLTRIM(SUBSTR(LCEXPRESSION, LNINTERNALINDEX + 1, LNINDEX - LNINTERNALINDEX - 1)))
        DO CASE
        CASE LCCOMMAND == 'SUBSTR'
          LCREPLACE = 'SUBSTRING'

        CASE LCCOMMAND == 'PADR'
          LCREPLACE = ''

        CASE LCCOMMAND == 'UPPER'
          LCREPLACE = ''

        CASE LCCOMMAND == 'LOWER'
          LCREPLACE = ''

        CASE LCCOMMAND == 'ALLTRIM'
          LCREPLACE = 'LTRIM'

        CASE LCCOMMAND == 'DTOS'
          LCREPLACE = 'CONVERT(CHAR(8), '
        ENDCASE

        LCTORETEXPRESSION = LEFT(LCTORETEXPRESSION, LEN(LCTORETEXPRESSION) - LEN(LCCOMMAND))
        LCTORETEXPRESSION = LCTORETEXPRESSION + LCREPLACE

        *-- Handle DTOS
        IF LCCOMMAND == 'DTOS'
          LOCAL LNBRACKETS
          LNBRACKETS = 1

          FOR LNINTERNALINDEX = LNINDEX + 1 TO LEN(LCEXPRESSION)
            LOCAL LCCURRENTCHAR
            LCCURRENTCHAR = SUBSTR(LCEXPRESSION, LNINTERNALINDEX, 1)
            LNBRACKETS = IIF(LCCURRENTCHAR = ')', -1, IIF(LCCURRENTCHAR = '(', 1, 0))

            IF LNBRACKETS = 0
              LCEXPRESSION = LEFT(LCEXPRESSION, LNINTERNALINDEX - 1) + ;
                '|' + ;
                SUBSTR(LCEXPRESSION, LNINTERNALINDEX + 1)
            ENDIF
          ENDFOR
        ENDIF
      ELSE
        IF SUBSTR(LCEXPRESSION, LNINDEX, 1) = "|"
          LCTORETEXPRESSION = LCTORETEXPRESSION + ', 112)'
        ELSE
          LCTORETEXPRESSION = LCTORETEXPRESSION + SUBSTR(LCEXPRESSION, LNINDEX, 1)
        ENDIF
      ENDIF
    ENDFOR
  ENDIF

  *-- Replace fields names with [FieldName]
  LOCAL LNDOTNO, LNDOTPOS
  LNDOTNO = 1

  LNDOTPOS = AT('.', LCTORETEXPRESSION, LNDOTNO)
  DO WHILE LNDOTPOS > 0
    LOCAL LNINDEX
    FOR LNINDEX = LNDOTPOS + 1 TO LEN(LCTORETEXPRESSION)
      IF LNINDEX = LEN(LCTORETEXPRESSION)
        LCTORETEXPRESSION = SUBSTR(LCTORETEXPRESSION, 1, LNDOTPOS) + ;
          '[' + ;
          SUBSTR(LCTORETEXPRESSION, LNDOTPOS + 1, LNINDEX - LNDOTPOS) + ;
          ']'
      ELSE
        * MAH T20080331.0008 20 May 2008 Check Also for underscor
        *-- IF !ISALPHA(SUBSTR(lcToRetExpression, lnIndex, 1))
        IF !ISALPHA(SUBSTR(LCTORETEXPRESSION, LNINDEX, 1)) .AND. !(SUBSTR(LCTORETEXPRESSION, LNINDEX, 1) = "_")
          * MAH T20080331.0008 20 May 2008 Check Also for underscor
          LCTORETEXPRESSION = SUBSTR(LCTORETEXPRESSION, 1, LNDOTPOS) + ;
            '[' + ;
            SUBSTR(LCTORETEXPRESSION, LNDOTPOS + 1, LNINDEX - LNDOTPOS - 1) + ;
            ']' + ;
            SUBSTR(LCTORETEXPRESSION, LNINDEX)
          EXIT
        ENDIF
      ENDIF
    ENDFOR

    LNDOTNO  = LNDOTNO + 1
    LNDOTPOS = AT('.', LCTORETEXPRESSION, LNDOTNO)
  ENDDO

  RETURN LCTORETEXPRESSION
ENDCASE

*!***********************************************************************************************************************
*! Name      	: gfGetTableInfo
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 09/27/2004
*! Purpose   	: Get information about any table
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	:
*! lcTableName             : (R) Table name
*! lcDBEngineType          : (W) Table database engine
*! lcUniqueIndexName       : (W) Unique index name in case of oAriaApplication.cSQLDBID
*! lcUniqueIndexExpression : (W) Unique index expression in case of oAriaApplication.cSQLDBID
*! llUniqueIndexOrder      : (W) Unique index order in case of oAriaApplication.cSQLDBID
*! llServerError           : (W) Error while connecting to server
*!***********************************************************************************************************************
*! Returns 	:
*!***********************************************************************************************************************
FUNCTION GFGETTABLEINFO

PARAMETERS LCTABLENAME, LCDBENGINETYPE, LCUNIQUEINDEXNAME, LCUNIQUEINDEXEXPRESSION, LLUNIQUEINDEXORDER, LLSERVERERROR

LOCAL LNSELECT
LNSELECT = SELECT()

LLSERVERERROR = .F.

LOCAL LCTEMPNAME
LCTEMPNAME = GFTEMPNAME()

LOCAL LNFLDRESULT
*E303030,1 BEGIN
*!*	lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
*!*	                                        "SELECT cFile_Nam FROM SydFiles WHERE cFile_Nam = '" + ;
*!*	                                        PADR(lcTableName, 8) + "'", ;
*!*	                                        '', ;
*!*	                                        lcTempName, ;
*!*	                                        '', ;
*!*	                                        oAriaApplication.cAria4Sysfiles, ;
*!*	                                        3, ;
*!*	                                        '', ;
*!*	                                        SET("Datasession"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
*!*	  "SELECT cFile_Nam FROM SydFiles WHERE cFile_Nam = '" + ;
*!*	  PADR(lcTableName, oAriaApplication.FileW) + "'", ;
*!*	  '', ;
*!*	  lcTempName, ;
*!*	  '', ;
*!*	  oAriaApplication.cAria4Sysfiles, ;
*!*	  3, ;
*!*	  '', ;
*!*	  SET("Datasession"))
LNFLDRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE( ;
  "SELECT cFile_Nam FROM SydFiles WHERE cFile_Nam = '" + ;
  PADR(LCTABLENAME, OARIAAPPLICATION.FILEW) + "' AND CVER = 'A40'", ;
  '', ;
  LCTEMPNAME, ;
  '', ;
  OARIAAPPLICATION.CARIA4SYSFILES, ;
  3, ;
  '', ;
  SET("Datasession"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]
*E303030,1 END

IF LNFLDRESULT = 1
  *-- If RECCOUNT(lcTempName) = 1 means this table from type SQL
  *-- If RECCOUNT(lcTempName) = 0  means this table from type FOX
  IF RECCOUNT(LCTEMPNAME) > 0
    LCDBENGINETYPE = OARIAAPPLICATION.CSQLDBID

    LOCAL LNFLDRESULT
    *E303030,1 BEGIN
    *!*	    lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
    *!*	                                           "SELECT cfile_tag, cindx_exp, lascend FROM sydindex WHERE cFile_nam = '" + ;
    *!*	                                           PADR(lcTableName, 8) + "'" + ;
    *!*	                                           ' AND lunique', ;
    *!*	                                           '', ;
    *!*	                                           lcTempName, ;
    *!*	                                           '', ;
    *!*	                                           oAriaApplication.cAria4Sysfiles, ;
    *!*	                                           3, ;
    *!*	                                           '', ;
    *!*	                                           SET("Datasession"))

    LNFLDRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE( ;
      "SELECT cfile_tag, cindx_exp, lascend FROM sydindex WHERE cFile_nam = '" + ;
      PADR(LCTABLENAME, OARIAAPPLICATION.FILEW) + "'" + ;
      ' AND lunique', ;
      '', ;
      LCTEMPNAME, ;
      '', ;
      OARIAAPPLICATION.CARIA4SYSFILES, ;
      3, ;
      '', ;
      SET("Datasession"))
    *E303030,1 END
    IF LNFLDRESULT = 1
      LCUNIQUEINDEXNAME       = ALLTRIM(&LCTEMPNAME..CFILE_TAG)
      LCUNIQUEINDEXEXPRESSION = ALLTRIM(&LCTEMPNAME..CINDX_EXP)
      LLUNIQUEINDEXORDER      = &LCTEMPNAME..LASCEND
    ELSE
      OARIAAPPLICATION.REMOTESYSTEMDATA.CHECKRETRESULT('Execute', LNFLDRESULT, .T.)
      LLSERVERERROR = .T.
    ENDIF
  ELSE
    LCDBENGINETYPE = OARIAAPPLICATION.CFOXDBID
  ENDIF

  USE IN (LCTEMPNAME)
ELSE
  OARIAAPPLICATION.REMOTESYSTEMDATA.CHECKRETRESULT('Execute', LNFLDRESULT, .T.)
  LLSERVERERROR = .T.
ENDIF

SELECT(LNSELECT)
*PADR

*!*************************************************************
*! Name      : gfValdPath
*! Developer : Mariam Mazhar
*! Date      : 17/08/2004
*! Purpose   : To validate any path if right or wrong
*!*************************************************************
*! Calls     :
*!      Called by: Called from custom style summary report
*!*************************************************************
*! Passed Parameters  : Path to be validated
*!*************************************************************
*! Returns            : Flag to tell valid or not
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION GFVALDPATH
PARAMETERS LCPATH

LLRETVAL = .T.
LCDEFAULT=FULLPATH(SET('DEFAULT'))
LCONERROR=ON('ERROR')

ON ERROR LLERROR = .T.
LLERROR          = .F.


SET DEFAULT TO (LCPATH)

IF LLERROR
  LLRETVAL = .F.
ENDIF

ON ERROR &LCONERROR

SET DEFAULT TO (LCDEFAULT)

RETURN LLRETVAL



*!*************************************************************
*! Name      : gfVldUsrFld
*! Developer : Saeed Mohammed Mostafa
*! Date      : 09/30/2004
*! Purpose   : Function to Validate User define fields Values
*! Tracking  : N037249
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : laPrUsrFields Array
*!*************************************************************
*! Returns            : Logical
*!*************************************************************
*! Called Form        : gfOpGrid , Control panel save
*!*************************************************************
*! Example            : = gfVldUsrFld(@laUsrFields)
*!*************************************************************
*! Due To C101459,1
*!*************************************************************
FUNCTION GFVLDUSRFLD
PARAMETERS LAPRUSRFIELDS

*N037773,1 WSH 05/17/2005 Declare variables as local as they may cause an infinite loop. [Start]
*PRIVATE lnCount,llValid,lnAlias
LOCAL LNCOUNT,LLVALID,LNALIAS,LCCURRCNTRL
*N037773,1 WSH 05/17/2005 [End]

LNALIAS = SELECT()
SELECT 0
LLVALID = .T.
FOR LNCOUNT = 1 TO ALEN(LAPRUSRFIELDS,1)
  IF !EMPTY(LAPRUSRFIELDS[lnCount,8])

    *N037773,1 WSH 05/17/2005 Assign the Focus Control of the Option Grid to be the validated field, to use in valid functions. [Start]
    LCCURRCNTRL = LOOGSCROLL.GETOBJECTREF('laOGFxFlt[' + ALLTRIM(STR(LNCOUNT)) + ',1]')
    LCCURRCNTRL.PARENT.PARENT.FOCUSCONTROL = LCCURRCNTRL.PARENT.NAME
    LCCURRCNTRL.PARENT.FOCUSCONTROL        = LCCURRCNTRL.NAME
    *N037773,1 WSH 05/17/2005 [End]

    LCFLDNAME = LAPRUSRFIELDS[lnCount,1]
    PRIVATE &LCFLDNAME
    &LCFLDNAME = LAPRUSRFIELDS[lnCount,6]
    LNWHENPOS = AT('WHEN',LAPRUSRFIELDS[lnCount,8])
    LCVALID = SUBSTR(LAPRUSRFIELDS[lnCount,8],1,IIF(LNWHENPOS=0,LEN(LAPRUSRFIELDS[lnCount,8]),LNWHENPOS-1))

    *! SMM to cancel error [START]
    LOCAL LCOLDERR
    LCOLDERR = ON("Error")
    ON ERROR LLVALID = .T.
    *! SMM to cancel error [END]

    LLVALID = EVAL(ALLT(STRTRAN(LCVALID,ALLT(LAPRUSRFIELDS[lnCount,1]),'m.'+ALLT(LAPRUSRFIELDS[lnCount,1]))))
    IF TYPE("llValid") # "L"
      LLVALID  = .T.
    ENDIF
    *! SMM to cancel error [START]
    ON ERROR &LCOLDERR
    *! SMM to cancel error [END]

    IF !LLVALID
      *N037773,1 WSH 10/11/2005 Check the existance of the array element before using it. [Start]
      IF TYPE("laPrUsrFields[lnCount,9]") # 'U' AND !EMPTY(LAPRUSRFIELDS[lnCount,9])
        *N037773,1 WSH 10/11/2005 [End]

        LCERRMESSAGE = "User defined field ("+ALLT(LAPRUSRFIELDS[lnCount,9])+") is not valid."+CHR(13)+CHR(10)+;
          "Value must be "+ALLT(STRTRAN(LAPRUSRFIELDS[lnCount,8],LAPRUSRFIELDS[lnCount,1],ALLT(LAPRUSRFIELDS[lnCount,9])))
        =GFMODALGEN(.F.,.F.,.F.,.F.,LCERRMESSAGE)

        *N037773,1 WSH 10/11/2005 Check the existance of the array element before using it. [Start]
      ENDIF
      *N037773,1 WSH 10/11/2005 [End]

      EXIT
    ENDIF
  ENDIF
ENDFOR
SELECT (LNALIAS)
RETURN LLVALID


*!***********************************************************************************************************************
*! Name        : gfExecute
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 10/10/2004
*! Purpose     : Executes a command to overcome the problem of calling forms include in the EXE
*!***********************************************************************************************************************
*! Parameters  : The string to execute
*!***********************************************************************************************************************
*! Returns     : None
*!***********************************************************************************************************************
FUNCTION GFEXECUTE
LPARAMETERS LCSTR
&LCSTR
RETURN

*!*************************************************************
*! Name      : gfUsrVldFn
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 01/31/2001
*! Purpose   : Function to run any function withen its program
*!*************************************************************
*! Called from : Global all over the system
*!*************************************************************
*! Calls       : passed function as a parameter
*!*************************************************************
*! Passed Parameters : lcFncNam,lcFncLoc,lcParam
*!                     lcFncNam  : Variable hold the name function
*!                     lcFncLoc  : Variable hold the function path
*!                     lcParam   : Variable hold the parameters
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = gfUsrVldFn()
*!*************************************************************
FUNCTION GFUSRVLDFN
PARAMETERS LCFNCNAM,LCFNCLOC,LCPARAM
*-- lcRetrn variable to hold the return value
PRIVATE LCRETRN

IF !EMPTY(LCFNCNAM)
  LCRETRN  = SPACE(0)
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][Start]
  LLCLIENTFILE = .F.
  IF OARIAAPPLICATION.MULTIINST
    LCROOTDR = STRTRAN(UPPER(OARIAAPPLICATION.CLIENTAPPLICATIONHOME),'PRGS\','')
    IF !EMPTY(LCFNCLOC)
      IF  FILE(LCROOTDR+LCFNCLOC+'.FXP')
        LCFNCLOC = LCROOTDR+LCFNCLOC
        LLCLIENTFILE = .T.
      ELSE
        LCFNCLOC= IIF(FILE(OARIAAPPLICATION.CLIENTAPPLICATIONHOME+LCFNCLOC+'.FXP'),;
          OARIAAPPLICATION.CLIENTAPPLICATIONHOME+LCFNCLOC,;
          IIF(FILE(OARIAAPPLICATION.CLIENTAPPLICATIONHOME+;
          OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCFNCLOC+'.FXP'),;
          OARIAAPPLICATION.CLIENTAPPLICATIONHOME+;
          OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCFNCLOC,;
          OARIAAPPLICATION.CLIENTAPPLICATIONHOME+LEFT(LCFNCLOC,2)+'\'+LCFNCLOC))
        IF FILE(LCFNCLOC+'.FXP')
          LLCLIENTFILE = .T.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF !LLCLIENTFILE
    *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][End]
    LCROOTDR = STRTRAN(UPPER(OARIAAPPLICATION.APPLICATIONHOME),'PRGS\','')
    * If lcFncLoc parm. is not empty that measns the lcFncNam function will be called from
    * main program lcFncLoc
    IF !EMPTY(LCFNCLOC)
      IF  FILE(LCROOTDR+LCFNCLOC+'.FXP')
        LCFNCLOC = LCROOTDR+LCFNCLOC
      ELSE
        LCFNCLOC= IIF(FILE(OARIAAPPLICATION.APPLICATIONHOME+LCFNCLOC+'.FXP'),;
          OARIAAPPLICATION.APPLICATIONHOME+LCFNCLOC,;
          IIF(FILE(OARIAAPPLICATION.APPLICATIONHOME+;
          OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCFNCLOC+'.FXP'),;
          OARIAAPPLICATION.APPLICATIONHOME+;
          OARIAAPPLICATION.ACTIVEMODULEID+'\'+LCFNCLOC,;
          OARIAAPPLICATION.APPLICATIONHOME+LEFT(LCFNCLOC,2)+'\'+LCFNCLOC))
      ENDIF
    ELSE
      * If lcFncLoc is empty that means the lcFncNam function is not embaeded into prg bit its a
      * separate program.
    ENDIF
    *! SMM to cancel error [START]
    *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][Start]
  ENDIF
  *! B609526,1 MMT 02/17/2011 Check the client main file on X in case of SAAS[T20101109.0012][End]
  LOCAL LCOLDERR
  LCOLDERR = ON("Error")
  ON ERROR RETURN .T.
  *! SMM to cancel error [END]
  IF EMPTY(LCPARAM)
    DO (LCFNCNAM) IN (LCFNCLOC) WITH LCRETRN
  ELSE
    DO (LCFNCNAM) IN (LCFNCLOC) WITH LCRETRN,&LCPARAM
  ENDIF
  *! SMM to cancel error [START]
  ON ERROR &LCOLDERR
  *! SMM to cancel error [END]
  RETURN LCRETRN
ENDIF
*!*************************************************************
*! Name       : gfPDFViewer
*! Developer  : Saeed Mostafa (SMM)
*! Date       : 10/28/2004.
*! Purpose    : To run PDF viewer
*! Entry      : N038424,1
*!*************************************************************
*! PARAMETERS :
*! lcCaption  : Title
*! lcFilePath : The Path of the PDF File
*!*************************************************************
FUNCTION GFPDFVIEWER
PARAMETERS  LCCAPTION, LCFILEPATH
DO FORM PDFVIEWER WITH LCCAPTION, LCFILEPATH
RETURN .T.

****************************************************************************
*! Name      : GetScale()
*! Developer : Heba Fathi (HFK)
*! Date      : 10/20/2004
*! Purpose   : Function that will return a string with the size scale
****************************************************************************
*! Parameters: 1 =>  Size scale to be displayed
*!             2 =>  Spacing between each size
****************************************************************************
PROCEDURE GETSCALE
PARAMETERS XSC,XSPACE
PRIVATE XOLDALIAS,X,XSTRING,XSC,XSPACE,Z
XOLDALIAS= ALIAS()
=GFOPENFILE(GCDATADIR+'SCALE',GCDATADIR+'SCALE','SH')
SELE SCALE
SEEK 'S'+XSC
X       = 1
XSTRING = ''
DO WHILE FOUND() .AND. X<=CNT
  Z = STR(X,1)
  XSTRING = XSTRING + SZ&Z + IIF(X=CNT,'',XSPACE)
  X= X + 1
ENDDO
IF .NOT. FOUND()
  XSTRING ='* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(XOLDALIAS)) > 0
  SELE &XOLDALIAS
ENDIF

RETURN(XSTRING)

*!*************************************************************
*! Name      	: gfGetTopLevelFrom
*! Developer 	: Mahmoud Said (MAH)
*! Date      	: 12/03/2004
*! Purpose   	: Return refrence to top level form
*! Tracking #   : E037885,2 MAH 12/03/2004 Separate screen in different session.
****************************************************************************
*! Parameters :
*!  loScreen : Handle to screen we need to get parent for
****************************************************************************
*! Returns 	: Return refrence to top level form or .NULL.
*:************************************************************************
*: Modifications :
*! B039071,1 MAH 02/24/2005 Close Group Problem.
*:************************************************************************

FUNCTION GFGETTOPLEVELFROM
LPARAMETERS LOSCREEN

DECLARE INTEGER GetParent IN WIN32API INTEGER

LOCAL LOFORM

IF TYPE('loScreen') = 'O' .AND. !ISNULL(LOSCREEN)
  LOFORM = LOSCREEN
ELSE
  IF TYPE('_SCREEN.ActiveForm') # 'O' .OR. ISNULL(_SCREEN.ACTIVEFORM)
    RETURN .NULL.
  ELSE
    LOFORM = _SCREEN.ACTIVEFORM
  ENDIF
ENDIF


IF TYPE('loForm') # 'O' .OR. ISNULL(LOFORM)
  RETURN .NULL.
ELSE
  DO WHILE !(UPPER(LOFORM.BASECLASS) == 'FORM') .AND. TYPE('loForm.Parent') = 'O' .AND. !ISNULL(LOFORM.PARENT)


    * T20080429.0009 MAH 18 / 6 / 2008 BEGIN
    IF TYPE('loForm.Parent') != 'O' .OR. ISNULL(LOFORM.PARENT)
      RETURN .NULL.
    ENDIF
    * T20080429.0009 MAH 18 / 6 / 2008 END

    LOFORM = LOFORM.PARENT

  ENDDO

  LOCAL LNTOPLEVELHWND
  LNTOPLEVELHWND = LOFORM.HWND

  DO WHILE LNTOPLEVELHWND # 0
    IF LNTOPLEVELHWND = 0
      RETURN .NULL.
    ELSE
      IF _SCREEN.HWND = LNTOPLEVELHWND
        RETURN _SCREEN
      ENDIF

      LOCAL LNINDEX
      FOR LNINDEX = 1 TO _SCREEN.FORMCOUNT
        *! B039071,1 MAH 02/24/2005 [BEGIN]
        IF TYPE('_SCREEN.Forms(lnIndex).Name') = 'O' .AND. !ISNULL(_SCREEN.FORMS(LNINDEX).NAME)
          *! B039071,1 MAH 02/24/2005 [END]

          IF !EMPTY(_SCREEN.FORMS(LNINDEX).NAME) .AND. TYPE('_SCREEN.Forms(lnIndex).HWnd') = 'N' .AND. ;
              _SCREEN.FORMS(LNINDEX).HWND = LNTOPLEVELHWND .AND. _SCREEN.FORMS(LNINDEX).SHOWWINDOW = 2
            RETURN _SCREEN.FORMS(LNINDEX)
          ENDIF

          *! B039071,1 MAH 02/24/2005 [BEGIN]
        ENDIF
        *! B039071,1 MAH 02/24/2005 [END]
      ENDFOR
    ENDIF

    LNTOPLEVELHWND = GETPARENT(LNTOPLEVELHWND)
  ENDDO

  RETURN .NULL.
ENDIF

*!*************************************************************
*! Name        : gfFormIsActive
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 12/07/2004
*! Purpose     : To Check if the Form is the active form of the FoxPro
*! Parameters  : The cHostFormName property of the FormSet
*!*************************************************************
*! Returns     : True / False
*!*************************************************************
FUNCTION GFFORMISACTIVE
LPARAMETERS LCHFN
LOCAL LLRETVAL
LLRETVAL = TYPE("_screen.ActiveForm.Parent") = "O" AND ;
  TYPE("_screen.ActiveForm.Parent.cHostFormName") = "C" AND ;
  _SCREEN.ACTIVEFORM.PARENT.CHOSTFORMNAME == LCHFN
RETURN LLRETVAL

*!*************************************************************
*! Name      	: gfFireValidEvent
*! Developer 	: Mahmoud Said (MAH)
*! Date      	: 05/17/2005
*! Purpose   	: Prevent fire valid when we switch from form to another
*! Tracking #   : E037885,4 MAH 02/17/2005 Remove the host form.
****************************************************************************
*! Parameters :
*!  loThisForm : Handle to current Form
****************************************************************************
*! Returns 	: .T. if we need to fire the event otherwise .F.
*!*************************************************************
FUNCTION GFFIREVALIDEVENT

LPARAMETERS LOTHISFORM

LOCAL LOSCREEN
LOSCREEN = _SCREEN.ACTIVEFORM

IF TYPE('loScreen') = 'O' .AND. !ISNULL(LOSCREEN) .AND. LOSCREEN.HWND = LOTHISFORM.HWND
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      	: gfGetIndexColumn
*! Developer 	: Mahmoud Said (MAH)
*! Date      	: 04/17/2005
*! Purpose   	: Return string to use by FOX index (Asc, Desc) in order to create the same SQL collating order.
*! Tracking #   : E037241,2 MAH 04/17/2005 Browse User Defined Sort.
****************************************************************************
*! Parameters :
*!  lcValue     : String
*!  llDirection : .T. for Asc and .F. for Desc
****************************************************************************
*! Returns 	: String
*:************************************************************************
*: Modifications :
*:************************************************************************

FUNCTION GFGETINDEXCOLUMN
LPARAMETERS LCVALUE, LLDIRECTION

LOCAL LCRETURN
PUBLIC LAASCII[256]
IF EMPTY(LAASCII)
  LAASCII[001] = 001
  LAASCII[002] = 002
  LAASCII[003] = 003
  LAASCII[004] = 004
  LAASCII[005] = 005
  LAASCII[006] = 006
  LAASCII[007] = 007
  LAASCII[008] = 008
  LAASCII[009] = 009
  LAASCII[010] = 010
  LAASCII[011] = 011
  LAASCII[012] = 012
  LAASCII[013] = 013
  LAASCII[014] = 014
  LAASCII[015] = 015
  LAASCII[016] = 016
  LAASCII[017] = 017
  LAASCII[018] = 018
  LAASCII[019] = 019
  LAASCII[020] = 020
  LAASCII[021] = 021
  LAASCII[022] = 022
  LAASCII[023] = 023
  LAASCII[024] = 024
  LAASCII[025] = 025
  LAASCII[026] = 026
  LAASCII[027] = 027
  LAASCII[028] = 028
  LAASCII[029] = 028
  LAASCII[030] = 030
  LAASCII[031] = 031
  LAASCII[032] = 032
  LAASCII[033] = 033
  LAASCII[034] = 034
  LAASCII[035] = 035
  LAASCII[036] = 036
  LAASCII[037] = 037
  LAASCII[038] = 038
  LAASCII[039] = 039
  LAASCII[040] = 040
  LAASCII[041] = 041
  LAASCII[042] = 042
  LAASCII[043] = 043
  LAASCII[044] = 044
  LAASCII[045] = 045
  LAASCII[046] = 046
  LAASCII[047] = 047
  LAASCII[048] = 048
  LAASCII[049] = 133
  LAASCII[050] = 134
  LAASCII[051] = 135
  LAASCII[052] = 136
  LAASCII[053] = 137
  LAASCII[054] = 138
  LAASCII[055] = 139
  LAASCII[056] = 140
  LAASCII[057] = 141
  LAASCII[058] = 142
  LAASCII[059] = 049
  LAASCII[060] = 050
  LAASCII[061] = 051
  LAASCII[062] = 052
  LAASCII[063] = 053
  LAASCII[064] = 054
  LAASCII[065] = 055
  LAASCII[066] = 143
  LAASCII[067] = 159
  LAASCII[068] = 162
  LAASCII[069] = 166
  LAASCII[070] = 167
  LAASCII[071] = 178
  LAASCII[072] = 179
  LAASCII[073] = 182
  LAASCII[074] = 183
  LAASCII[075] = 194
  LAASCII[076] = 195
  LAASCII[077] = 198
  LAASCII[078] = 199
  LAASCII[079] = 202
  LAASCII[080] = 206
  LAASCII[081] = 220
  LAASCII[082] = 221
  LAASCII[083] = 224
  LAASCII[084] = 225
  LAASCII[085] = 229
  LAASCII[086] = 230
  LAASCII[087] = 241
  LAASCII[088] = 242
  LAASCII[089] = 245
  LAASCII[090] = 246
  LAASCII[091] = 252
  LAASCII[092] = 056
  LAASCII[093] = 057
  LAASCII[094] = 058
  LAASCII[095] = 059
  LAASCII[096] = 060
  LAASCII[097] = 061
  LAASCII[098] = 144
  LAASCII[099] = 160
  LAASCII[100] = 161

  LAASCII[101] = 165
  LAASCII[102] = 168
  LAASCII[103] = 177
  LAASCII[104] = 180
  LAASCII[105] = 181
  LAASCII[106] = 184
  LAASCII[107] = 193
  LAASCII[108] = 196
  LAASCII[109] = 197
  LAASCII[110] = 200
  LAASCII[111] = 201
  LAASCII[112] = 205
  LAASCII[113] = 219
  LAASCII[114] = 222
  LAASCII[115] = 223
  LAASCII[116] = 226
  LAASCII[117] = 228
  LAASCII[118] = 231
  LAASCII[119] = 240
  LAASCII[120] = 243
  LAASCII[121] = 244
  LAASCII[122] = 247
  LAASCII[123] = 251
  LAASCII[124] = 062
  LAASCII[125] = 063
  LAASCII[126] = 064
  LAASCII[127] = 065
  LAASCII[128] = 066
  LAASCII[129] = 067
  LAASCII[130] = 068
  LAASCII[131] = 069
  LAASCII[132] = 070
  LAASCII[133] = 071
  LAASCII[134] = 072
  LAASCII[135] = 073
  LAASCII[136] = 074
  LAASCII[137] = 075
  LAASCII[138] = 076
  LAASCII[139] = 077
  LAASCII[140] = 078
  LAASCII[141] = 079
  LAASCII[142] = 080
  LAASCII[143] = 081
  LAASCII[144] = 082
  LAASCII[145] = 083
  LAASCII[146] = 084
  LAASCII[147] = 085
  LAASCII[148] = 086
  LAASCII[149] = 087
  LAASCII[150] = 088
  LAASCII[151] = 089
  LAASCII[152] = 090
  LAASCII[153] = 091
  LAASCII[154] = 092
  LAASCII[155] = 093
  LAASCII[156] = 094
  LAASCII[157] = 095
  LAASCII[158] = 096
  LAASCII[159] = 097
  LAASCII[160] = 098
  LAASCII[161] = 099
  LAASCII[162] = 100
  LAASCII[163] = 101
  LAASCII[164] = 102
  LAASCII[165] = 103
  LAASCII[166] = 104
  LAASCII[167] = 105
  LAASCII[168] = 106
  LAASCII[169] = 107
  LAASCII[170] = 108
  LAASCII[171] = 109
  LAASCII[172] = 110
  LAASCII[173] = 111
  LAASCII[174] = 112
  LAASCII[175] = 113
  LAASCII[176] = 114
  LAASCII[177] = 115
  LAASCII[178] = 116
  LAASCII[179] = 117
  LAASCII[180] = 118
  LAASCII[181] = 119
  LAASCII[182] = 120
  LAASCII[183] = 121
  LAASCII[184] = 122
  LAASCII[185] = 123
  LAASCII[186] = 124
  LAASCII[187] = 125
  LAASCII[188] = 126
  LAASCII[189] = 127
  LAASCII[190] = 128
  LAASCII[191] = 129
  LAASCII[192] = 130
  LAASCII[193] = 146
  LAASCII[194] = 147
  LAASCII[195] = 150
  LAASCII[196] = 151
  LAASCII[197] = 154
  LAASCII[198] = 155
  LAASCII[199] = 158
  LAASCII[200] = 163

  LAASCII[201] = 170
  LAASCII[202] = 171
  LAASCII[203] = 174
  LAASCII[204] = 175
  LAASCII[205] = 186
  LAASCII[206] = 187
  LAASCII[207] = 190
  LAASCII[208] = 191
  LAASCII[209] = 253
  LAASCII[210] = 203
  LAASCII[211] = 208
  LAASCII[212] = 209
  LAASCII[213] = 212
  LAASCII[214] = 213
  LAASCII[215] = 216
  LAASCII[216] = 131
  LAASCII[217] = 217
  LAASCII[218] = 232
  LAASCII[219] = 235
  LAASCII[220] = 236
  LAASCII[221] = 239
  LAASCII[222] = 248
  LAASCII[223] = 255
  LAASCII[224] = 227
  LAASCII[225] = 145
  LAASCII[226] = 148
  LAASCII[227] = 149
  LAASCII[228] = 152
  LAASCII[229] = 153
  LAASCII[230] = 156
  LAASCII[231] = 157
  LAASCII[232] = 164
  LAASCII[233] = 169
  LAASCII[234] = 172
  LAASCII[235] = 173
  LAASCII[236] = 176
  LAASCII[237] = 185
  LAASCII[238] = 188
  LAASCII[239] = 189
  LAASCII[240] = 192
  LAASCII[241] = 254
  LAASCII[242] = 204
  LAASCII[243] = 207
  LAASCII[244] = 210
  LAASCII[245] = 211
  LAASCII[246] = 214
  LAASCII[247] = 215
  LAASCII[248] = 132
  LAASCII[249] = 218
  LAASCII[250] = 233
  LAASCII[251] = 234
  LAASCII[252] = 237
  LAASCII[253] = 238
  LAASCII[254] = 249
  LAASCII[255] = 256
  LAASCII[256] = 250
ENDIF

LOCAL LNCHAR

LCRETURN = ''
FOR LNCHAR = 1 TO LEN(LCVALUE)
  IF LLDIRECTION
    LCRETURN = LCRETURN + CHR(LAASCII[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] - 1)
  ELSE
    *! E037241,2 MAH 07/17/2005 [BEGIN]
    *-- lcReturn = lcReturn + CHR(laAscii[256 - laAscii[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] + 1] - 1)
    LCRETURN = LCRETURN + CHR(255 - LAASCII[ASC(SUBSTR(lcValue, lnChar, 1)) + 1] - 1)
    *! E037241,2 MAH 07/17/2005 [END]
  ENDIF
ENDFOR

RETURN LCRETURN

*!*************************************************************
*! Name      	: gfStrToExp
*! Developer 	: Mahmoud Said (MAH)
*! Date      	: 04/17/2005
*! Purpose   	:
*! Tracking #   : B039435,1 MAH Fix bug of there is a quote in the field value.
****************************************************************************
*! Parameters :
*!  lcValue        : String
*!  lcDBEngine     : Database engine type
*!  llEnglishQuery : Show expression in englis query
****************************************************************************
*! Returns 	: Expression
*:************************************************************************
*: Modifications :
*:************************************************************************
FUNCTION GFSTRTOEXP
LPARAMETERS LCVALUE, LCDBENGINE, LLENGLISHQUERY

IF LLENGLISHQUERY
  LCVALUE =  '"' + LCVALUE + '"'
ELSE
  LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)

  *-- Note: not supported case: there is ', " and ]

  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    IF !("'" $ LCVALUE)
      LCVALUE =  "'" + LCVALUE + "'"
    ELSE
      IF !('"' $ LCVALUE)
        LCVALUE =  '"' + LCVALUE + '"'
      ELSE
        LCVALUE =  "[" + LCVALUE + "]"
      ENDIF
    ENDIF

  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCVALUE = "'" + STRTRAN(LCVALUE, "'", "''") + "'"
  ENDCASE
ENDIF

RETURN LCVALUE

*!*************************************************************
*! Name        : gfOpenTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Create a Remote Table Object if Trying to open Fox or Sql Table Defined in the Sysem Files
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : File Name, Index Tag, Open Mode ("EX" or "SH"), Alias Name, Force Open
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
FUNCTION GFOPENTABLE
LPARAMETERS NFILE,LCINDEX,MODE,LCALIASNAM,LLFORCEOP,LCCOMPID,LLFASTTABLE
LOCAL LNTABLE, LNL

LCALIASNAM = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(LCALIASNAM),JUSTSTEM(NFILE),LCALIASNAM)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIASNAM)

IF LNTABLE=0 && No Remote Table Object was Found

  LNL = ALEN(OARIAAPPLICATION.LAREMOTETABLE)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')='O'
    LNL = LNL + 1
    DIMENSION OARIAAPPLICATION.LAREMOTETABLE[lnL]
  ENDIF
  *WAIT WINDOW NFILE+', '+lcIndex+', '+PADR(SET("Datasession"),10) TIMEOUT 0.5
  LCINDEX = IIF(EMPTY(LCINDEX),'',JUSTSTEM(LCINDEX))
  OARIAAPPLICATION.LAREMOTETABLE[lnL] = CREATEOBJECT("RemoteTable",JUSTSTEM(NFILE),LCINDEX,LCALIASNAM,SET("Datasession"),LCCOMPID,LLFASTTABLE)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')<>'O'
    LNL = MAX(LNL - 1,1)
    DIMENSION OARIAAPPLICATION.LAREMOTETABLE[lnL]
    RETURN GFOPENFILE(NFILE,LCINDEX,MODE,LCALIASNAM,LLFORCEOP)
  ENDIF

ELSE && a Remote Table Object Already Exist

  IF !EMPTY(LCINDEX)
    *WAIT WINDOW lcIndex TIMEOUT 0.5
    OARIAAPPLICATION.LAREMOTETABLE[lnTable].SETORDER(JUSTSTEM(LCINDEX))
  ENDIF

ENDIF

RETURN
*--end of gfOpenTable

*!*************************************************************
*! Name        : gfGetRemoteTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Searches the AriaApplication laRemoteTable Array for an Existing Object
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : DataSession, Alias Name
*!*************************************************************
*! Returns            :  0  ----> If Remote Table Object Not Found in the laRemoteTable Array
*!                       n  ----> Element No. of laRemoteTable in which the Remote Table Exists
*!*************************************************************
FUNCTION GFGETREMOTETABLE
LPARAMETERS LNDATASESSIONID, LCALIAS
LOCAL LNC, LNL, LNV

LNV=0
LNL = ALEN(OARIAAPPLICATION.LAREMOTETABLE)
FOR LNC=1 TO LNL
  IF TYPE('oAriaApplication.laRemoteTable[lnC]')='O' AND ;
      OARIAAPPLICATION.LAREMOTETABLE[lnC].LNDATASESSION == LNDATASESSIONID AND ;
      UPPER(OARIAAPPLICATION.LAREMOTETABLE[lnC].LCCURSORVIEW)==UPPER(LCALIAS)
    LNV = LNC
    EXIT
  ENDIF
NEXT

RETURN LNV
*--end of gfGetRemoteTable

*!*************************************************************
*! Name        : gfGetRemoteProp
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Searches the AriaApplication laRemoteTable Array for an Existing Object
*!               And if found Returns the value of a Remote Table Property
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Property Name, Alias Name
*!*************************************************************
*! Returns            :  0  ----> If Remote Table Object Not Found in the laRemoteTable Array
*!                       n  ----> Element No. of laRemoteTable in which the Remote Table Exists
*!*************************************************************
FUNCTION GFGETREMOTEPROP
LPARAMETERS LCPROP, LCALIAS
LOCAL LCMACRO, LRETVALUE

LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  LCMACRO = 'oAriaApplication.laRemoteTable[lnTable].'+LCPROP
  LRETVALUE = &LCMACRO
ELSE
  LRETVALUE = .F.
ENDIF

RETURN LRETVALUE
*--end of gfGetRemoteProp


*!*************************************************************
*! Name        : gfGetAlias
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Extracts Alias Name from a Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statment
*!*************************************************************
*! Returns     :  Alias Name
*!*************************************************************
FUNCTION GFGETALIAS
LPARAMETERS LCSTAT
LOCAL LCALIAS
DO CASE
CASE TYPE('lcStat')='C' AND (' IN ' $ UPPER(LCSTAT) OR UPPER(LCSTAT)='IN ')
  *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [Start]
  *lcAlias = ALLTRIM(SUBSTR(lcStat,AT(' IN ',UPPER(lcStat))+4))
  LCALIAS = ALLTRIM(SUBSTR(LCSTAT,RAT(' IN ',UPPER(LCSTAT))+4))
  *lcStat = LEFT(lcStat,AT(' IN ',UPPER(lcStat))-1)
  IF "'" $ LCALIAS OR '"' $ LCALIAS OR "]" $ LCALIAS
    LCALIAS = ALIAS()
  ELSE
    LCSTAT = LEFT(LCSTAT,RAT(' IN ',UPPER(LCSTAT))-1)
  ENDIF
  *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [End]
OTHERWISE
  LCALIAS = ALIAS()
ENDCASE

RETURN LCALIAS
*--end of gfGetAlias

*!*************************************************************
*! Name        : gfSetOrder
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Set Order To Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Index Tag
*!*************************************************************
*! Returns     :  None
*!*************************************************************
FUNCTION GFSETORDER
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS

LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  OARIAAPPLICATION.LAREMOTETABLE[lnTable].SETORDER(LCSTAT)
ELSE
  SELECT (LCALIAS)
  IF TYPE('lcStat')='C' AND !EMPTY(LCSTAT)
    SET ORDER TO &LCSTAT
  ELSE
    SET ORDER TO
  ENDIF
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN
*--end of gfSetOrder

*!*************************************************************
*! Name        : gfSeek
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Seek Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Expression, Alias, Tag Name, Fast Seek
*!*************************************************************
*! Returns     :  Seek Result
*!*************************************************************
FUNCTION GFSEEK
LPARAMETERS LCEXP, LCALIAS, LCTAGNAME, LLFASTSEEK
LOCAL LNTABLE

LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  RETURN OARIAAPPLICATION.LAREMOTETABLE[lnTable].SEEK(LCEXP,LCTAGNAME,LLFASTSEEK)
ELSE
  LOCAL LCPARAM
  LCPARAM = 'lcExp' + IIF(!EMPTY(LCALIAS), ',lcAlias' + IIF(!EMPTY(LCTAGNAME), ',lcTagName', ''), '')

  RETURN SEEK(&LCPARAM.)
ENDIF

RETURN
*--end of gfSeek


*!*************************************************************
*! Name        : gfRelation
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Within the Set Relation To command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Expression, Alias, Additive
*!*************************************************************
*! Returns     :  The Expression Paramater Value
*!*************************************************************
FUNCTION GFRELATION
LPARAMETERS LCEXP, LCALIAS, LLADDITIVE
LOCAL LNTABLE

LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)
IF LNTABLE<>0 && Remote Table Object was Found
  OARIAAPPLICATION.LAREMOTETABLE[lnTable].SEEK(EVALUATE(LCEXP))
  IF LLADDITIVE
    SET RELATION TO &LCEXP INTO &LCALIAS ADDITIVE
  ELSE
    SET RELATION TO &LCEXP INTO &LCALIAS
  ENDIF
ENDIF

RETURN EVALUATE(LCEXP)
*--end of gfRelation


*!*************************************************************
*! Name        : gfSqlRun
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Locate Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : SQL Select Statement, Cursor Receiving Results, Ignore Creating Index
*!*************************************************************
*! Returns     :  False if Falied to Create the Cursor
*!*************************************************************
FUNCTION GFSQLRUN
LPARAMETERS LCSQLSTATEMENT, LCALIAS, LLNOINDEX, LCTEMPALIAS
LOCAL LNTABLE

LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
LCTEMPALIAS = IIF(TYPE('lcTempAlias')='C',LCTEMPALIAS,LCALIAS)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  RETURN OARIAAPPLICATION.LAREMOTETABLE[lnTable].SQLRUN(LCSQLSTATEMENT, LCTEMPALIAS, LLNOINDEX)
ELSE
  RETURN .F.
ENDIF

RETURN
*--end of SqlRun

*!*************************************************************
*! Name        : gfRecNo
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Recno Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Alias
*!*************************************************************
*! Returns     :  RecNo Result
*!*************************************************************
FUNCTION GFRECNO
LPARAMETERS LCALIAS
LOCAL LNTABLE

LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  RETURN OARIAAPPLICATION.LAREMOTETABLE[lnTable].RECNO()
ELSE
  RETURN RECNO(LCALIAS)
ENDIF

RETURN
*--end of gfRecNo


*!*************************************************************
*! Name        : gfGoRec
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go / Goto Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION GFGOREC
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS, LNRECNUM

LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  *khm1
  *oAriaApplication.laRemoteTable[lnTable].GoRec(&lcStat)
  OARIAAPPLICATION.LAREMOTETABLE[lnTable].GOREC(LCSTAT)
  *khm1
ELSE
  SELECT (LCALIAS)
  LNRECNUM = IIF(TYPE('lcStat')='N',LCSTAT,VAL(LCSTAT))
  GO LNRECNUM
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN
*--end of gfGoRec


*!*************************************************************
*! Name        : gfGoTop
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go Top Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION GFGOTOP
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS, LLRETVAL

LLRETVAL = .T.
LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  LLRETVAL = OARIAAPPLICATION.LAREMOTETABLE[lnTable].GOTOP()
ELSE
  SELECT (LCALIAS)
  GO TOP
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN LLRETVAL
*--end of gfGoTop


*!*************************************************************
*! Name        : gfGoBottom
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Go Bottom Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION GFGOBOTTOM
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS

LLRETVAL = .T.
LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  LLRETVAL = OARIAAPPLICATION.LAREMOTETABLE[lnTable].GOBOTTOM()
ELSE
  SELECT (LCALIAS)
  GO BOTTOM
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN LLRETVAL
*--end of gfGoBottom


*!*************************************************************
*! Name        : gfGoNext
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of Skip Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION GFGONEXT
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS, LLRETVAL

LLRETVAL = .T.
LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  LLRETVAL = OARIAAPPLICATION.LAREMOTETABLE[lnTable].GONEXT()
ELSE
  SELECT (LCALIAS)
  SKIP
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN LLRETVAL
*--end of gfGoNext


*!*************************************************************
*! Name        : gfGoPrevious
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of Skip -1 Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Statemnet
*!*************************************************************
*! Returns     : None (or True/False if Remote Table)
*!*************************************************************
FUNCTION GFGOPREVIOUS
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS, LLRETVAL

LLRETVAL = .T.
LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  LLRETVAL = OARIAAPPLICATION.LAREMOTETABLE[lnTable].GOPREVIOUS()
ELSE
  SELECT (LCALIAS)
  SKIP -1
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN LLRETVAL
*--end of gfGoPrevious


*!*************************************************************
*! Name        : gfAppend
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Append Blank Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION GFAPPEND

*N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
*LPARAMETERS lcStat
LPARAMETERS LCSTAT, LLFORMMEMVAR
*N039550,1 WSH 09/18/2005 [End]

LOCAL LNTABLE, LCALIAS, LCOLDALIAS

LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 AND !OARIAAPPLICATION.LAREMOTETABLE[lnTable].LLNATIVE && Remote Table Object was Found and the Table is SQL
  SELECT (OARIAAPPLICATION.LAREMOTETABLE[lnTable].LCCURSORUPDATE)
  APPEND BLANK

  *N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
  IF LLFORMMEMVAR
    GATHER MEMVAR MEMO
  ENDIF
  *N039550,1 WSH 09/18/2005 [End]

ENDIF
SELECT (LCALIAS)
APPEND BLANK

*N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
IF LLFORMMEMVAR
  GATHER MEMVAR MEMO
ENDIF
*N039550,1 WSH 09/18/2005 [End]

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN
*--end of gfAppend


*!*************************************************************
*! Name        : gfReplace
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Replace Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Replace Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION GFREPLACE
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS

LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 AND !OARIAAPPLICATION.LAREMOTETABLE[lnTable].LLNATIVE && Remote Table Object was Found and the Table is SQL
  IF TYPE('lcStat')='C' AND !EMPTY(LCSTAT)
    OARIAAPPLICATION.LAREMOTETABLE[lnTable].REPLACE(LCSTAT)
  ELSE
    OARIAAPPLICATION.LAREMOTETABLE[lnTable].REPLACE()
  ENDIF
ENDIF
SELECT (LCALIAS)

IF TYPE('lcStat')='C' AND !EMPTY(LCSTAT)
  REPLACE &LCSTAT
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN
*--end of gfReplace


*!*************************************************************
*! Name        : gfDelete
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Replace Statement
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Delete Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION GFDELETE
LPARAMETERS LCSTAT
LOCAL LNTABLE, LCALIAS, LCOLDALIAS

LCOLDALIAS = ALIAS()
LCALIAS = GFGETALIAS(@LCSTAT)
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

SELECT (LCALIAS)
IF LNTABLE<>0 AND !OARIAAPPLICATION.LAREMOTETABLE[lnTable].LLNATIVE && Remote Table Object was Found and the Table is SQL
  IF TYPE('lcStat')='C' AND !EMPTY(LCSTAT)
    SCAN &LCSTAT
      OARIAAPPLICATION.LAREMOTETABLE[lnTable].DELETE()
    ENDSCAN
  ELSE
    OARIAAPPLICATION.LAREMOTETABLE[lnTable].DELETE()
  ENDIF
ENDIF

IF TYPE('lcStat')='C' AND !EMPTY(LCSTAT)
  DELETE &LCSTAT
ELSE
  DELETE
ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN
*--end of gfDelete


*!*************************************************************
*! Name        : gfTableUpdate
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the TableUpdate Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Paremeters of the TableUpdate Function
*!*************************************************************
*! Returns     : TableUpdate Function Result
*!*************************************************************
FUNCTION GFTABLEUPDATE
LPARAMETERS LLFORCE, LCALIAS, LCERROR
LOCAL LNTABLE, LCOLDALIAS, LCTRANCODE, LLRETVALUE

LCOLDALIAS = ALIAS()
LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

LLRETVALUE = .T.
IF LNTABLE<>0 && Remote Table Object was Found
  IF TYPE('llForce')<>'C'
    LCTRANCODE = OARIAAPPLICATION.REMOTECOMPANYDATA.BEGINTRAN(OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'')
    IF TYPE('lcTranCode') = 'N'
      LLRETVALUE = .F.
    ENDIF
  ELSE
    LCTRANCODE = LLFORCE
  ENDIF
  LLRETVALUE = LLRETVALUE AND OARIAAPPLICATION.LAREMOTETABLE[lnTable].TABLEUPDATE(LCTRANCODE)
  IF TYPE('llForce')<>'C' AND TYPE('lcTranCode')='C'
    IF LLRETVALUE
      LLRETVALUE = (OARIAAPPLICATION.REMOTECOMPANYDATA.COMMITTRAN(LCTRANCODE)=1)  AND LLRETVALUE
    ELSE
      LLRETVALUE = (OARIAAPPLICATION.REMOTECOMPANYDATA.ROLLBACKTRAN(LCTRANCODE)=1)
    ENDIF
  ENDIF
ELSE
  LOCAL LCPARAM
  *! B609140,1 MMT 02/10/2010 Fix bug of error while cancelling Sales order [Start]
  *lcParam = IIF(TYPE('llForce') = 'L', PADR(llForce,3), '.T.') + ', '+;
  IIF(!EMPTY(lcAlias), lcAlias ,' .F.')+ ', '+;
  IIF(!EMPTY(lcError), lcError,' .F.')
  *llRetValue = TableUpdate(&lcParam.)
  IF CURSORGETPROP("Buffering",LCALIAS) <> 1
  *B611925,1 ES 10/14/2019 Random issue in Manual packing list screen that Packing list is saved without carton detail [Start]
    *LCPARAM = '.F.,'
     LCPARAM = '.T.,'
*B611925,1 ES 10/14/2019 Random issue in Manual packing list screen that Packing list is saved without carton detail [End]

    *B610451,1 HIA 07/28/13 T20130723.0006 - Error in editing company information [Begin]
    *lcParam = lcParam +  IIF(TYPE('llForce') = 'L', IIF(llForce,'.T.','.F.'), '.T.')+ ', '+;
    *  IIF(!EMPTY(lcAlias), "'"+lcAlias+"'" ,' .F.')+ ', '+;
    *  IIF(!EMPTY(lcError), lcError,' .F.')

    LCPARAM = LCPARAM +  IIF(TYPE('llForce') = 'L', IIF(LLFORCE,'.T.','.F.'), '.T.')+ ', '+;
      IIF(!EMPTY(LCALIAS), "'"+LCALIAS+"'" ,' .F.')+ ;
      IIF(!EMPTY(LCERROR), ', '+LCERROR,'')

    *B610451,1 HIA 07/28/13 T20130723.0006 - Error in editing company information [End]
    LLRETVALUE = TABLEUPDATE(&LCPARAM.)
  ENDIF
  *! B609140,1 MMT 02/10/2010 Fix bug of error while cancelling Sales order [End]


ENDIF

IF !EMPTY(LCOLDALIAS)
  SELECT (LCOLDALIAS)
ENDIF

RETURN LLRETVALUE
*--end of gfTableUpdate


*!*************************************************************
*! Name        : gfCloseTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Use / Use in statemnet
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Alias
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION GFCLOSETABLE
LPARAMETERS LCALIAS
LOCAL LNTABLE, LNLEN

IF TYPE('lcAlias')='C' AND 'IN ' $ LCALIAS
  LCALIAS = GFGETALIAS(@LCALIAS)
ELSE
  LCALIAS = IIF(TYPE('lcAlias')='C',LCALIAS,ALIAS())
ENDIF
LNTABLE = GFGETREMOTETABLE(SET("Datasession"),LCALIAS)

IF LNTABLE<>0 && Remote Table Object was Found
  OARIAAPPLICATION.LAREMOTETABLE[lnTable] = NULL
  ADEL(OARIAAPPLICATION.LAREMOTETABLE,LNTABLE)
  LNLEN = MAX(ALEN(OARIAAPPLICATION.LAREMOTETABLE)-1,1)
  DIMENSION OARIAAPPLICATION.LAREMOTETABLE[lnLen]
ENDIF

RETURN
*--end of gfCloseTable


FUNCTION GFGETTASKLISTID
LOCAL LNSELECTED
LNSELECTED = SELECT()

LOCAL LCRETURN
LCRETURN = ''

DO CASE
CASE SYSCHDUL.CCONTTYPE = 'O' .OR. SYSCHDUL.CCONTTYPE = 'T'
  LOCAL LCKEY
  LCKEY = ALLTRIM(SYSCHDUL.CCONTTYPE) + ALLTRIM(SYSCHDUL.CSEQNUMBER)
  GFOPENFILE(OARIAAPPLICATION.DATADIR+'ORDHDR','ORDHDR',"SH")
  = SEEK(LCKEY, 'ORDHDR', 'ORDHDR')
  LCRETURN = ORDHDR.ACCOUNT

CASE SYSCHDUL.CCONTTYPE = 'P'
  LOCAL LCWHERE, LCSQL, LCTEMPCURSORNAME
  LCWHERE = "WHERE [cbusdocu] = '" + ALLTRIM(SYSCHDUL.CCONTTYPE) + "' and [cstytype] = 'P' and [po] = '" + ALLTRIM(SYSCHDUL.CSEQNUMBER) + "'"
  LCSQL   = "SELECT vendor FROM poshdr WITH (INDEX = poshdr) " + LCWHERE

  LCTEMPCURSORNAME = GFTEMPNAME()
  LOCAL LNSQLRUNRESULT
  LNSQLRUNRESULT = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSQL, LCTEMPCURSORNAME, ;
    '', OARIAAPPLICATION.ACTIVECOMPANYCONSTR, 3, 'SAVE', SET("Datasession"))

  IF LNSQLRUNRESULT # 1
    *-- No need due to the error apears while refreshing forever
    *-- oAriaApplication.RemoteCompanyData.CheckRetResult('SQLRun', lnSQLRunResult, .T.)
  ELSE
    LCRETURN = &LCTEMPCURSORNAME..VENDOR
  ENDIF

OTHERWISE
  LCRETURN = SYSCHDUL.CCONT_ID
ENDCASE

SELECT(LNSELECTED)
RETURN LCRETURN


FUNCTION GFGETTASKLISTNAME
LOCAL LNSELECTED
LNSELECTED = SELECT()

LOCAL LCRETURN
LCRETURN = ''

DO CASE
CASE SYSCHDUL.CCONTTYPE = 'O' .OR. SYSCHDUL.CCONTTYPE = 'T'
  LOCAL LCKEY
  LCKEY = ALLTRIM(SYSCHDUL.CCONTTYPE) + ALLTRIM(SYSCHDUL.CSEQNUMBER)
  GFOPENFILE(OARIAAPPLICATION.DATADIR+'ORDHDR','ORDHDR',"SH")
  = SEEK(LCKEY, 'ORDHDR', 'ORDHDR')
  LCRETURN = ORDHDR.ACCOUNT
  LCRETURN = LOOKUP(CUSTOMER.BTNAME,"M"+ALLTRIM(LCRETURN),CUSTOMER.ACCOUNT,"CUSTOMER")

CASE SYSCHDUL.CCONTTYPE = 'P'
  LOCAL LCWHERE, LCSQL, LCTEMPCURSORNAME
  LCWHERE = "WHERE [cbusdocu] = '" + ALLTRIM(SYSCHDUL.CCONTTYPE) + "' and [cstytype] = 'P' and [po] = '" + ALLTRIM(SYSCHDUL.CSEQNUMBER) + "'"
  LCSQL   = "SELECT vendor FROM poshdr WITH (INDEX = poshdr) " + LCWHERE

  LCTEMPCURSORNAME = GFTEMPNAME()
  LOCAL LNSQLRUNRESULT
  LNSQLRUNRESULT = OARIAAPPLICATION.REMOTECOMPANYDATA.SQLRUN(LCSQL, LCTEMPCURSORNAME, ;
    '', OARIAAPPLICATION.ACTIVECOMPANYCONSTR, 3, 'SAVE', SET("Datasession"))

  IF LNSQLRUNRESULT # 1
    *-- No need due to the error apears while refreshing forever
    *-- oAriaApplication.RemoteCompanyData.CheckRetResult('SQLRun', lnSQLRunResult, .T.)
  ELSE
    LCRETURN = &LCTEMPCURSORNAME..VENDOR
    GFOPENFILE(OARIAAPPLICATION.DATADIR+'APVENDOR','VENCODE',"SH")
    LCRETURN = LOOKUP(APVENDOR.CVENCOMP,ALLTRIM(LCRETURN),APVENDOR.CVENDCODE,"VENCODE")
  ENDIF

OTHERWISE
  LCRETURN = LOOKUP(CUSTOMER.BTNAME,"M"+SYSCHDUL.CCONT_ID,CUSTOMER.ACCOUNT,"CUSTOMER")
ENDCASE

SELECT(LNSELECTED)
RETURN LCRETURN

*!*************************************************************
*! Name        : gfToolTipAssign
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Checks the
****************************************************************************
*! Parameters  : The Control, The Value
*!*************************************************************
*! Returns     : None
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION GFTOOLTIPASSIGN
LPARAMETERS LOCTRL, m.VNEWVAL

LOCTRL.XTOOLTIP = m.VNEWVAL
LOCTRL.TOOLTIPTEXT = ''

RETURN
*--end of gfToolTipAssign

*!*************************************************************
*! Name        : gfToolTipShift
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Checks the Docking Effect of the ToolBar on the TollTips
****************************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : Numeric
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION GFTOOLTIPSHIFT
LOCAL LNRETVALUE

LNRETVALUE = IIF(TYPE('oAriaApplication.oToolBar.oWindParent.AriaForm1.Name')='C' AND ;
  _SCREEN.ACTIVEFORM.NAME==OARIAAPPLICATION.OTOOLBAR.OWINDPARENT.ARIAFORM1.NAME, ;
  OARIAAPPLICATION.OTOOLBAR.DOCKPOSITION,2)
RETURN LNRETVALUE
*--end of gfToolTipShift

*!*************************************************************
*! Name        : gfMouseEnter
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : Solve the Problem of ToolTipText When the ToolBar is Docked Top or Left. Called From Mouse Enter Event of the Control
****************************************************************************
*! Parameters  : The Control
*!*************************************************************
*! Returns     : None
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION GFMOUSEENTER
LPARAMETERS LOCTRL
LOCAL LNDOCKPOS, LOFORM, LNABSTOP, LNABSLEFT

* T20080429.0009 MAH 18 / 6 / 2008 BEGIN
IF TYPE('loForm') != 'O' .OR. ISNULL(LOFORM)
  RETURN
ENDIF
* T20080429.0009 MAH 18 / 6 / 2008 END

LOFORM = _SCREEN.ACTIVEFORM
IF TYPE('loForm.lblToolTip')<>'O'
  LOFORM.ADDOBJECT('lblToolTip','ariatooltip')
ENDIF

LOFORM.LBLTOOLTIP.VISIBLE = .F.
LNDOCKPOS = GFTOOLTIPSHIFT()
LNABSTOP = GFABSOLUTEPOS('Top',LOCTRL)
LNABSLEFT = GFABSOLUTEPOS('Left',LOCTRL)
DO CASE
CASE LNDOCKPOS = 0 && Top Docking
  LOFORM.LBLTOOLTIP.TOP = LNABSTOP - OARIAAPPLICATION.OTOOLBAR.HEIGHT
  LOFORM.LBLTOOLTIP.LEFT = LNABSLEFT
CASE LNDOCKPOS = 1 && Left Docking
  LOFORM.LBLTOOLTIP.TOP = LNABSTOP
  LOFORM.LBLTOOLTIP.LEFT = LNABSLEFT - OARIAAPPLICATION.OTOOLBAR.WIDTH
OTHERWISE
  LOFORM.LBLTOOLTIP.TOP = LNABSTOP
  LOFORM.LBLTOOLTIP.LEFT = LNABSLEFT
ENDCASE
LOFORM.LBLTOOLTIP.HEIGHT = LOCTRL.HEIGHT
LOFORM.LBLTOOLTIP.WIDTH = LOCTRL.WIDTH

IF LOCTRL.MOUSEPOINTER<>0
  LOFORM.LBLTOOLTIP.MOUSEPOINTER = LOCTRL.MOUSEPOINTER
ELSE
  DO CASE
  CASE LOWER(LOCTRL.BASECLASS) = 'commandbutton' OR !LOCTRL.ENABLED
    LOFORM.LBLTOOLTIP.MOUSEPOINTER = 1
  CASE LOWER(LOCTRL.BASECLASS) = 'combobox' AND LOCTRL.ENABLED AND LOCTRL.STYLE=2
    LOFORM.LBLTOOLTIP.MOUSEPOINTER = 1
  CASE LOCTRL.ENABLED
    LOFORM.LBLTOOLTIP.MOUSEPOINTER = 3
  ENDCASE
ENDIF
LOFORM.LBLTOOLTIP.TOOLTIPTEXT = IIF(TYPE('loCtrl.xToolTip')='C',LOCTRL.XTOOLTIP,LOFORM.LBLTOOLTIP.TOOLTIPTEXT)
LOFORM.LBLTOOLTIP.LOCONTROL = LOCTRL
LOFORM.LBLTOOLTIP.VISIBLE = .T.

RETURN
*--end of gfMouseEnter

*!*************************************************************
*! Name        : gfAbsolutePos
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 05/01/2006
*! Purpose     : To Calculate the Absolute Top or Left of a control in a Container
****************************************************************************
*! Parameters  : The Property, The Control
*!*************************************************************
*! Returns     : Numeric
*!*************************************************************
*040206 ASM,1 Fix the Problem of ToolTip text shifting
FUNCTION GFABSOLUTEPOS
LPARAMETERS LCPROP, LOCTRL
LOCAL LNVALUE
LNVALUE = IIF(UPPER(LCPROP)="TOP" AND LOWER(LOCTRL.PARENT.BASECLASS)<>'form',25,0) + EVALUATE('loCtrl.'+LCPROP)

LOCTRL = LOCTRL.PARENT
DO WHILE LOWER(LOCTRL.BASECLASS)<>'form'
  IF TYPE('loCtrl.'+LCPROP)='N'
    LNVALUE = LNVALUE +  EVALUATE('loCtrl.'+LCPROP)
  ENDIF
  LOCTRL = LOCTRL.PARENT
ENDDO

RETURN LNVALUE

*:**************************************************************************
*:* Name        : lfGtCallFn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/19/2007
*:* Purpose     : Get calling function name
*:***************************************************************************
*B608223,1 TMI
FUNCTION LFGTCALLFN
LOCAL LNI,LCRET
LNI = 0
DO WHILE !EMPTY(PROGRAM(LNI))
  LNI = LNI + 1
ENDDO
LCRET = PROGRAM(LNI-3)
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[Start]
RELEASE LNI
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[End]
RETURN LCRET
*-- end of lfGtCallFn.

*E302567,1 MMT 01/12/2009 Change paths for SAAS [sTART]
*!*************************************************************
*! Name      : gfCallForm
*! Developer : MARIAM MAZHAR [MMT]
*! Date      : 01/12/2009
*! Purpose   : Function to call forms instead of DO FORM
*!*************************************************************
*! Parameters:
*! lcFormName : The form to be called Name
*! lcModuleID : Module that form existing in
*! lcWithParam: WITH Paramters
*! lcToParam  : TO Paramters
*!*************************************************************
FUNCTION GFCALLFORM
PARAMETERS LCFORMNAME,LCMODULEID,LCWITHPARAM,LCTOPARAM



IF TYPE('lcModuleID') = 'C'
  IF OARIAAPPLICATION.MULTIINST AND FILE(OARIAAPPLICATION.CLIENTSCREENHOME+LCMODULEID+'\'+LCFORMNAME+'.SCX')
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCMODULEID+'\'+LCFORMNAME+'.SCX' WITH &LCWITHPARAM TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCMODULEID+'\'+LCFORMNAME+'.SCX' WITH &LCWITHPARAM
      ENDIF
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCMODULEID+'\'+LCFORMNAME+'.SCX' TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCMODULEID+'\'+LCFORMNAME+'.SCX'
      ENDIF
    ENDIF
  ELSE
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.SCREENHOME+LCMODULEID+'\'+LCFORMNAME+'.SCX' WITH &LCWITHPARAM TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.SCREENHOME+LCMODULEID+'\'+LCFORMNAME+'.SCX' WITH &LCWITHPARAM
      ENDIF
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.SCREENHOME+LCMODULEID+'\'+LCFORMNAME+'.SCX' TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.SCREENHOME+LCMODULEID+'\'+LCFORMNAME+'.SCX'
      ENDIF
    ENDIF
  ENDIF
ELSE
  IF OARIAAPPLICATION.MULTIINST AND FILE(OARIAAPPLICATION.CLIENTSCREENHOME+LCFORMNAME+'.SCX')
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCFORMNAME+'.SCX' WITH &LCWITHPARAM TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCFORMNAME+'.SCX' WITH &LCWITHPARAM
      ENDIF
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCFORMNAME+'.SCX' TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.CLIENTSCREENHOME +LCFORMNAME+'.SCX'
      ENDIF
    ENDIF
  ELSE
    IF TYPE('lcWithParam') = 'C'
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.SCREENHOME+LCFORMNAME+'.SCX' WITH &LCWITHPARAM TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.SCREENHOME+LCFORMNAME+'.SCX' WITH &LCWITHPARAM
      ENDIF
    ELSE
      IF  TYPE('lcToParam') = 'C'
        DO FORM OARIAAPPLICATION.SCREENHOME+LCFORMNAME+'.SCX' TO &LCTOPARAM
      ELSE
        DO FORM OARIAAPPLICATION.SCREENHOME+LCFORMNAME+'.SCX'
      ENDIF
    ENDIF
  ENDIF
ENDIF
*E302567,1 MMT 01/12/2009 Change paths for SAAS [eND]
*:******************************************************************
*! Function  : gfDispSpack
*! Developer : Wael Ali Mohamed
*! Date      : 01/01/2011
*! DESC      : function to display All installed tracking entries that was
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =gfDispSpack()
*!*************************************************************
*! B609487,1 WAM 01/23/2011 Display System updates [T20110109.0001]
*!*************************************************************
FUNCTION GFDISPSPAC

LCSQLDICPATH =  OARIAAPPLICATION.CARIA4SYSPATH
SELECT PADR(IIF(SYDATTACH.CENTRYTYPE='C',"Custom","System"),10) AS CTYPE, SYDATTACH.CRELEASE ,SYDATTACH.CSRVPACK,SYDATTACH.CBUILD,SYDATTACH.CENTRYTYPE,SYDATTACH.CENTRYID,SYDEXES.TICKET,SYDEXES.CTRKSDSC,;
  SYDEXES.DGENDATE, SYDEXES.DINSTDATE,SYDATTACH.CNAME,SYDATTACH.CKEY,SYDATTACH.CTAG ;
  FROM (LCSQLDICPATH +"SYDATTACH") INNER JOIN (LCSQLDICPATH+"SYDEXES") ;
  ON SYDATTACH.CRELEASE+SYDATTACH.CSRVPACK+SYDATTACH.CBUILD+SYDATTACH.CENTRYTYPE+SYDATTACH.CENTRYID =  SYDEXES.CRELEASE+SYDEXES.CSRVPACK+SYDEXES.CBUILD+SYDEXES.CENTRYTYPE+SYDEXES.CTRACKNO ;
  UNION ;
  (SELECT "Temporary " AS CTYPE, CLIENTS_APPS.CRELEASE ,CLIENTS_APPS.CSRVPACK,SPACE(3) AS CBUILD,CLIENTS_APPS.CENTRYTYPE,CLIENTS_APPS.CENTRYID,CLIENTS_APPS.TICKET,CLIENTS_APPS.CTRKSDSC,;
  TTOD(DGENDATE) AS DGENDATE,TTOD(DINSTDATE) AS DINSTDATE,CLIENTS_APPS.CNAME,CLIENTS_APPS.CKEY,CLIENTS_APPS.CTAG ;
  FROM (LCSQLDICPATH+"CLIENTS_APPS") ) ;
  ORDER BY  CLIENTS_APPS.CRELEASE,CLIENTS_APPS.CSRVPACK,CBUILD,CLIENTS_APPS.CENTRYTYPE,CLIENTS_APPS.CENTRYID INTO CURSOR SYSUPDATES

SELECT SYSUPDATES

LCBRFIELDS =[crelease:H="Release#",csrvpack:H="Service Pack#",cbuild:H="Build#",centrytype:H="Type",centryid:H="Entry ID",ticket:H="Ticket#",ctrksdsc:H="Description",]
LCBRFIELDS =LCBRFIELDS +[dgendate:H="Generate Date", dinstdate:H="Install Date",cname:H="File name",ckey:H="Key",ctag:H="Tag Name",cType:H="Update Type"]

=ARIABROW(.F.,"System Updates")

USE IN 'SysUpdates'
USE IN 'SYDATTACH'
USE IN 'SYDEXES'
USE IN 'CLIENTS_APPS'


* T20101118.0003 - Attach Objects by Style /Colour MAH [BEGIN]
FUNCTION GFGETSTYLEIMAGEINFO
LPARAMETERS LCRETURNTYPE, LCSTYLEKEY, LLNEWSESSION
IF LLNEWSESSION
  LOCAL LNOLDSESSION
  LNOLDSESSION = SET("Datasession")

  LOCAL LONEWSESSION
  LONEWSESSION = CREATEOBJECT('session')
  SET DATASESSION TO LONEWSESSION.DATASESSIONID
  SET MULTILOCKS ON
ENDIF
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
LCDELETESET =  SET("Deleted")
SET DELETED ON
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][End]

LOCAL LNSELECT
LNSELECT = SELECT(0)

DIMENSION LASEGINFO[1,1]
LOCAL LOGETITEMMASK
LOGETITEMMASK = CREATEOBJECT('GetItemMask')
LOGETITEMMASK.DO(@LASEGINFO, '', "0001")
*! B610014,1 HIA 07/18/2012 Error Popsup when open Aria Application [T20120711.0030][Begin]
*IF ALEN(laSegInfo, 2) > 1
IF ALEN(LASEGINFO, 1) > 1
  *! B610014,1 HIA 07/18/2012 Error Popsup when open Aria Application [T20120711.0030][End]
  IF EMPTY(SUBSTR(LCSTYLEKEY, LASEGINFO[2, 4], LEN(LASEGINFO[2, 3])))
    LCSTYLEKEY = SUBSTR(LCSTYLEKEY, LASEGINFO[1, 4], LEN(LASEGINFO[1, 3]))
  ELSE
    LCSTYLEKEY = SUBSTR(LCSTYLEKEY, LASEGINFO[1, 4], LEN(LASEGINFO[1, 3])) + LASEGINFO[1, 6] + ;
      SUBSTR(LCSTYLEKEY, LASEGINFO[2, 4], LEN(LASEGINFO[2, 3]))
  ENDIF
ENDIF

LOCAL LNMAJLEN, LCRETVALUE

LCSTYLEKEY = PADR(LCSTYLEKEY, 19)

IF !USED('OBJLINKTMP')
  =GFOPENTABLE('OBJLINK','OBJLNKTY', 'SH', 'OBJLINKTMP')
ENDIF

IF !USED('OBJECTSTMP')
  =GFOPENTABLE('OBJECTS','OBJECTID', 'SH', 'OBJECTSTMP')
ENDIF

IF GFSEEK('D' + 'S' + LCSTYLEKEY, 'OBJLINKTMP', 'OBJDEFA') .OR. GFSEEK('S' + LCSTYLEKEY, 'OBJLINKTMP', 'OBJLNKTY')
  IF ALLTRIM(LCRETURNTYPE) == "K"
    LCRETVALUE = LCSTYLEKEY
  ELSE
    IF GFSEEK(OBJLINKTMP.COBJECT_ID, 'OBJECTSTMP', 'OBJECTID')
  	  *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][Start]      
  	  *      LCRETVALUE  = OBJECTSTMP.MIMGPATH
      LCRETVALUE  =IIF(!EMPTY(OBJECTSTMP.MIMGPATH) AND FILE(ALLTRIM(OBJECTSTMP.MIMGPATH)),OBJECTSTMP.MIMGPATH,"")
  	  *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][End]            
    ELSE
      LCRETVALUE  = ""
    ENDIF
  ENDIF
ELSE
  LNMAJLEN = LEN(GFITEMMASK('PM'))

  IF ALLTRIM(LCRETURNTYPE) == "K"
    LCRETVALUE = SUBSTR(LCSTYLEKEY, 1, LNMAJLEN)
  ELSE
    * B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
    *lcRetValue = SUBSTR(lcStyleKey, 1, lnMajLen)
    LCRETVALUE = PADR(SUBSTR(LCSTYLEKEY, 1, LNMAJLEN),19)
    * B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][End]
    IF GFSEEK('D' + 'S' + LCRETVALUE, 'OBJLINKTMP', 'OBJDEFA') .OR. GFSEEK('S' + LCRETVALUE, 'OBJLINKTMP', 'OBJLNKTY')
      IF GFSEEK(OBJLINKTMP.COBJECT_ID, 'OBJECTSTMP', 'OBJECTID')
    	*B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][Start]      
   	    *LCRETVALUE  = OBJECTSTMP.MIMGPATH
        LCRETVALUE  =IIF(!EMPTY(OBJECTSTMP.MIMGPATH) AND FILE(ALLTRIM(OBJECTSTMP.MIMGPATH)),OBJECTSTMP.MIMGPATH,"")
       *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][End]            
       
      ELSE
        LCRETVALUE  = ""
      ENDIF
    ELSE
      LCRETVALUE = ""
    ENDIF
  ENDIF
ENDIF

=GFCLOSETABLE('OBJLINKTMP')
=GFCLOSETABLE('OBJECTSTMP')

SELECT(LNSELECT)
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
SET DELETED &LCDELETESET.
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][End]
IF LLNEWSESSION
  LONEWSESSION = NULL
  SET DATASESSION TO LNOLDSESSION
ENDIF

IF ALLTRIM(LCRETURNTYPE) == "K"
  RETURN PADR(LCRETVALUE, 19)
ELSE
  RETURN LCRETVALUE
ENDIF

RETURN ""

FUNCTION GFGETITEMIMAGEINFO
LPARAMETERS LCRETURNTYPE, LCSTYLEKEY, LLNEWSESSION

IF LLNEWSESSION
  LOCAL LNOLDSESSION
  LNOLDSESSION = SET("Datasession")

  LOCAL LONEWSESSION
  LONEWSESSION = CREATEOBJECT('session')
  SET DATASESSION TO LONEWSESSION.DATASESSIONID
  SET MULTILOCKS ON
ENDIF
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
LCDELETESET =  SET("Deleted")
SET DELETED ON
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][END]
LNSELECT = SELECT(0)

DIMENSION LASEGINFO[1,1]
LOCAL LOGETITEMMASK
LOGETITEMMASK = CREATEOBJECT('GetItemMask')
LOGETITEMMASK.DO(@LASEGINFO, '', "0002")


*! B610014,1 HIA 07/18/2012 Error Popsup when open Aria Application [T20120711.0030][Begin]
*IF ALEN(laSegInfo, 2) > 1
IF ALEN(LASEGINFO, 1) > 1
  *! B610014,1 HIA 07/18/2012 Error Popsup when open Aria Application [T20120711.0030][End]
  IF EMPTY(SUBSTR(LCSTYLEKEY, LASEGINFO[2, 4], LEN(LASEGINFO[2, 3])))
    LCSTYLEKEY = SUBSTR(LCSTYLEKEY, LASEGINFO[1, 4], LEN(LASEGINFO[1, 3]))
  ELSE
    LCSTYLEKEY = SUBSTR(LCSTYLEKEY, LASEGINFO[1, 4], LEN(LASEGINFO[1, 3])) + LASEGINFO[1, 6] + ;
      SUBSTR(LCSTYLEKEY, LASEGINFO[2, 4], LEN(LASEGINFO[2, 3]))
  ENDIF
ENDIF

LOCAL LNMAJLEN, LCRETVALUE

LCSTYLEKEY  = PADR(LCSTYLEKEY, 19)

IF !USED('OBJLINKTMP')
  =GFOPENTABLE('OBJLINK','OBJLNKTY', 'SH', 'OBJLINKTMP')
ENDIF

IF !USED('OBJECTSTMP')
  =GFOPENTABLE('OBJECTS','OBJECTID', 'SH', 'OBJECTSTMP')
ENDIF

IF GFSEEK('D' + 'M' + LCSTYLEKEY, 'OBJLINKTMP', 'OBJDEFA') .OR. GFSEEK('M' + LCSTYLEKEY, 'OBJLINKTMP', 'OBJLNKTY')
  IF ALLTRIM(LCRETURNTYPE) == "K"
    LCRETVALUE = LCSTYLEKEY
  ELSE
    IF GFSEEK(OBJLINKTMP.COBJECT_ID, 'OBJECTSTMP', 'OBJECTID')
     * LCRETVALUE  = OBJECTSTMP.MIMGPATH
     *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][Start]      
  	  *      LCRETVALUE  = OBJECTSTMP.MIMGPATH
      LCRETVALUE  =IIF(!EMPTY(OBJECTSTMP.MIMGPATH) AND FILE(ALLTRIM(OBJECTSTMP.MIMGPATH)),OBJECTSTMP.MIMGPATH,"")
  	  *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][End]            
    ELSE
      LCRETVALUE  = ""
    ENDIF
  ENDIF
ELSE
  LNMAJLEN    = LEN(GFITEMMASK('PM', '', '0002'))
  IF ALLTRIM(LCRETURNTYPE) == "K"
    LCRETVALUE  = SUBSTR(LCSTYLEKEY, 1, LNMAJLEN)
  ELSE
    * B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
    *lcRetValue  = SUBSTR(lcStyleKey, 1, lnMajLen)
    LCRETVALUE  = PADR(SUBSTR(LCSTYLEKEY, 1, LNMAJLEN),19)
    * B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][End]
    IF GFSEEK('D' + 'M' + LCRETVALUE, 'OBJLINKTMP', 'OBJDEFA') .OR. GFSEEK('M' + LCRETVALUE, 'OBJLINKTMP', 'OBJLNKTY')
      IF GFSEEK(OBJLINKTMP.COBJECT_ID, 'OBJECTSTMP', 'OBJECTID')
        *LCRETVALUE  = OBJECTSTMP.MIMGPATH
        *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][Start]      
  	    *      LCRETVALUE  = OBJECTSTMP.MIMGPATH
        LCRETVALUE  =IIF(!EMPTY(OBJECTSTMP.MIMGPATH) AND FILE(ALLTRIM(OBJECTSTMP.MIMGPATH)),OBJECTSTMP.MIMGPATH,"")
  	    *B612694,1 MMT 09/21/2023 Error in Cutting ticket form if the printed cutting ticket includes style has assigned image but it is related image file is not found[T-ERP-20230920.0002][End]            
      ELSE
        LCRETVALUE  = ""
      ENDIF
    ELSE
      LCRETVALUE  = ""
    ENDIF
  ENDIF
ENDIF

=GFCLOSETABLE('OBJLINKTMP')
=GFCLOSETABLE('OBJECTSTMP')

SELECT(LNSELECT)
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][Start]
SET DELETED &LCDELETESET.
* B610134,1 MMT 10/29/2012 PO form deos not print style-color picture[T20121023.0008][End]
IF LLNEWSESSION
  LONEWSESSION = NULL
  SET DATASESSION TO LNOLDSESSION
ENDIF


IF ALLTRIM(LCRETURNTYPE) == "K"
  RETURN PADR(LCRETVALUE, 19)
ELSE
  RETURN LCRETVALUE
ENDIF

RETURN ""
* T20101118.0003 - Attach Objects by Style /Colour MAH [END]

*:******************************************************************
*! Function  : gfGetdueD
*! Developer : Hassan Ibrahim Ali
*! Date      : 04/22/2012
*! DESC      : function to Return the Due Date
*:******************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : Invoice_Date, Payment_Term_Code
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =gfGetdueD(Invoice_Date,Payment_Term_Code)
*!*************************************************************
*! E303111,1 HIA 04/22/2012 Add Next month day related field, to trems code [T20120301.0038][Begin]
FUNCTION GFGETDUED
LPARAMETERS LDINVDATE, LCTERMCODE

LOCAL LDDUEDATE
LDMYDUEDATE = {}
*-- Get Trade discount related fields


DECLARE LATRLTFLD[7,2]
LATRLTFLD[1,1] = 'NTERDISCR'
LATRLTFLD[1,2] = 'lnTrde_Disc' && Trade Discount
LATRLTFLD[2,1] = 'EOM '
LATRLTFLD[2,2] = 'lcTEOM'      && End of Month Y/N
LATRLTFLD[3,1] = 'NTERDUED'
LATRLTFLD[3,2] = 'lnTDaysDue'  && Net Due Days
LATRLTFLD[4,1] = 'CODYN'
LATRLTFLD[4,2] = 'lcTCod'      && Cash on delivery Y/N
LATRLTFLD[5,1] = 'LINSTALLM'
LATRLTFLD[5,2] = 'llInstTerm'  && Use Instalment
LATRLTFLD[6,1] = 'EOMDAY'
LATRLTFLD[6,2] = 'lnEomDay'    && End Of Month Day
LATRLTFLD[7,1] = 'NXTMNTHDY'
LATRLTFLD[7,2] = 'lNXTMNTHD'    && Next Month Day
STORE 0   TO LNTRDE_DISC, LNTDAYSDUE, LNEOMDAY, LNXTMNTHD
STORE 'N' TO LCTEOM, LCTCOD
STORE .F. TO LLINSTTERM
=GFRLTFLD(LCTERMCODE,@LATRLTFLD,'CTERMCODE')
LCTEOM   = ALLTRIM(LCTEOM)
*lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
LCTCOD   = ALLTRIM(LCTCOD)

*-- Compute invoice due date
INVOICE_DATE   = LDINVDATE
NEXT_MONTH_DAY = LNXTMNTHD
NET_DUE_DAYS   = LNTDAYSDUE

IF LCTEOM= "Y"
  FIRST_DAY_OF_CURRENT_MONTH = DATE( YEAR(INVOICE_DATE) , MONTH(INVOICE_DATE) , 1 )

  IF DAY(INVOICE_DATE) > LNEOMDAY
    FIRST_DAY_OF_TWO_MONTHS_AFTER = GOMONTH(FIRST_DAY_OF_CURRENT_MONTH,2)

    LAST_DAY_OF_NEXT_MONTH = FIRST_DAY_OF_TWO_MONTHS_AFTER - 1
    DUE_DATE = LAST_DAY_OF_NEXT_MONTH + NEXT_MONTH_DAY
  ELSE
    FIRST_DAY_OF_NEXT_MONTH = GOMONTH(FIRST_DAY_OF_CURRENT_MONTH,1)
    LAST_DAY_OF_CURRENT_MONTH = FIRST_DAY_OF_NEXT_MONTH - 1
    DUE_DATE = LAST_DAY_OF_CURRENT_MONTH + NEXT_MONTH_DAY
  ENDIF
  DUE_DATE = DUE_DATE + NET_DUE_DAYS
ELSE
  DUE_DATE = INVOICE_DATE + NET_DUE_DAYS
ENDIF

RETURN  DUE_DATE
*! E303111,1 HIA 04/22/2012 Add Next month day related field, to trems code [T20120301.0038][End]

*!*************************************************************
*! Name      : GetRpField
*! Developer : Tarek mohammed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Get Selected Fields
*!*************************************************************
**E303256,1 TMI 09/25/2012 [Start]
FUNCTION GETRPFIELD
PRIVATE LCOGSELECTED,LCOGFILES,LNOGCOUNT
LCOGSELECTED=SELECT()
SELECT SYDREPRT
LOCATE FOR CREP_ID= LCOGPRGNAME
IF FOUND()
  IF !EMPTY(MREPFIELD)
    RESTORE FROM MEMO MREPFIELD ADDITIVE
    LCOGFILES=''
    FOR LNOGCOUNT=1 TO ALEN(LASELFIELD,1)
      LCOGFILES=LCOGFILES+IIF(LNOGCOUNT>1,',','')+;
        LASELFIELD[lnOGCount,1]
      IF TYPE('laselfield[lnOGCount,2]')='C'
        LCOGFILES=LCOGFILES+IIF(!EMPTY(LASELFIELD[lnOGCount,2]),;
          ' AS "'+ALLTRIM(LASELFIELD[lnOGCount,2])+'"','')
      ENDIF
    ENDFOR
  ENDIF
ELSE
  LCOGFILES=''
ENDIF
SELECT(LCOGSELECTED)
RETURN LCOGFILES


*!********************************************************************
*!
*!*************************************************************
*! Name      : lfGetOrder
*! Developer : Tarek mohammed Ibrahim
*! Date      : 10/08/2012
*! Purpose   : used in the GL Reports OG
*! Entry     : E303256,1
*!*************************************************************
FUNCTION LFGETORDER
PARAMETERS LCOGSORTID

LCOGSELECTED=SELECT()
IF !USED('SYREPSRT')
  SELECT 0
  USE (OARIAAPPLICATION.SYSPATH+'SYREPSRT')
ELSE
  SELECT SYREPSRT
ENDIF
SET ORDER TO TAG CREP_ID
LCOGSORT=''
IF !EMPTY(LCOGSORTID)
  IF SEEK(LCOGREPID+LCOGSORTID)
    LCOGSORT=MORDERSTR
  ENDIF
ENDIF
SELECT(LCOGSELECTED)
RETURN LCOGSORT
* E303855,1 MMT 07/31/2017 Applying Aria5 new Framework - Decorator[T20170706.0015][Start]
*!*	* E303736,1 MMT 12/29/2016 Aria5 changes - Facelift [Aria5 ERP - Main System - Iteration 2016-12][Start]
*!*	*!*************************************************************
*!*	*! Name      : lfRemoveFromFavorites
*!*	*! Developer : Mariam Mazhar[MMT]
*!*	*! Date      : 12/29/2016
*!*	*! Purpose   : Remove item from Favorites
*!*	*! Entry     : E303736,1
*!*	*!*************************************************************
*!*	FUNCTION lfRemoveFromFavorites
*!*	LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcUser_ID
*!*	lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
*!*	SELECT(lcFavoritesCursor)
*!*	LOCATE FOR cExpKey = lcModule+ALLTRIM(SUBSTR(lcprogramname,4))+lcprogorwin
*!*	IF FOUND()
*!*	  DELETE
*!*	ENDIF
*!*	IF USED(lcFavoritesCursor)
*!*	  LOCAL lnOldSession, lnOldSelect
*!*	  lnOldSession = SET("Datasession")
*!*	  lnOldSelect  = SELECT()
*!*	  SET DATASESSION TO 1
*!*	  SELECT(lcFavoritesCursor)
*!*	  LOCAL loXMLParase, lcXML
*!*	  loXMLParase = NEWOBJECT('wwxml')
*!*	  lcXML = loXMLParase.CursorToXML()
*!*	  LOCAL lcPreferenceDir
*!*	  lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
*!*	  LOCAL lcPreferenceName
*!*	  lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
*!*	  SET SAFETY OFF
*!*	  STRTOFILE(lcXML, lcPreferenceName ) 
*!*	  SELECT(lnOldSelect)
*!*	  SET DATASESSION TO lnOldSession
*!*	ENDIF 
*!*	oAriaApplication.oMainForm.DisplayFavorites(.T.)
*!*	DEACTIVATE POPUP  popTileOptions

*!*	*!*************************************************************
*!*	*! Name      : lfAddToFavorites
*!*	*! Developer : Mariam Mazhar[MMT]
*!*	*! Date      : 12/29/2016
*!*	*! Purpose   : Add item to Favorites
*!*	*! Entry     : E303736,1
*!*	*!*************************************************************
*!*	FUNCTION lfAddToFavorites
*!*	LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcParam,lcName

*!*	lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
*!*	SELECT(lcFavoritesCursor)

*!*	IF USED(lcFavoritesCursor)
*!*	  LOCAL lnOldSession, lnOldSelect
*!*	  lnOldSession = SET("Datasession")
*!*	  lnOldSelect  = SELECT()
*!*	  SET DATASESSION TO 1
*!*	  SELECT(lcFavoritesCursor)
*!*	  *-- Get Last no
*!*	  LOCAL lcSec
*!*	  CALCULATE MAX(VAL(cSec)) TO lcSec
*!*	  lcSec = lcSec + 1
*!*	  lcSec = PADR(STR(lcSec, 10), 10)
*!*	  
*!*	  *-- Append new record
*!*	  APPEND BLANK 
*!*	  REPLACE cType WITH 'L', ;
*!*	        cSec WITH lcSec, ;
*!*	        cName WITH lcName, ;
*!*	        cParent WITH 'FAVORITES', ;
*!*	        cEXPKey WITH UPPER(ALLTRIM(lcModule)) + UPPER(ALLTRIM(lcprogramname)) + UPPER(ALLTRIM(lcprogorwin)) + UPPER(ALLTRIM(lcParam))
*!*	  LOCAL loXMLParase, lcXML
*!*	  loXMLParase = NEWOBJECT('wwxml')
*!*	  lcXML = loXMLParase.CursorToXML()
*!*	  LOCAL lcPreferenceDir
*!*	  lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
*!*	  LOCAL lcPreferenceName
*!*	  lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
*!*	  SET SAFETY OFF
*!*	  STRTOFILE(lcXML, lcPreferenceName ) 
*!*	  SELECT(lnOldSelect)
*!*	  SET DATASESSION TO lnOldSession
*!*	ENDIF 
*!*	oAriaApplication.oMainForm.DisplayFavorites(.T.)
*!*	* E303736,1 MMT 12/29/2016 Aria5 changes - Facelift [Aria5 ERP - Main System - Iteration 2016-12][End]
* E303855,1 MMT 07/31/2017 Applying Aria5 new Framework - Decorator[T20170706.0015][End]

* E304037,1 MMT 08/15/2018 Create new function to read the Identifier code structure [P20171120.0011][Start]
*!*************************************************************
*! Name      : gfGetIdentifierStructure
*! Developer : Mariam Mazhar
*! Date      : 08/15/2018
*! Purpose   : Read the Identifier code structure
*! Entry     : E304037,1
*!*************************************************************
FUNCTION gfGetIdentifierStructure
LPARAMETERS lcIdentifierStructure
lcReturnVal = ''
lnConnHandSeg  = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
IF lnConnHandSeg > 0
LNREMRESULT = SQLEXEC(lnConnHandSeg ,"Select * from IdentifierStructure Where IdentifierStructureId = '"+lcIdentifierStructure+"'","IdenStru")
IF LNREMRESULT > 0
  SELECT "IdenStru"
  LOCATE 
  IF !EOF()
    LNREMRESULT = SQLEXEC(lnConnHandSeg ,"Select * from IdentifierSegment Where IdentifierStructure = '"+IdenStru.Oid+"' Order by POSITION","IdenStruSeg")
    IF LNREMRESULT > 0
      SELECT "IdenStruSeg"
      LOCATE 
      SCAN 
        IF ValueType ='I' 
  	  	  LNREMRESULT = SQLEXEC(lnConnHandSeg ,"Select * from IdentifierSegmentFormat Where IdentifierSegment = '"+IdenStruSeg.Oid+"'","IdenStruSegFormat")
          if LNREMRESULT > 0
            SELECT "IdenStruSegFormat"
            LOCATE 
            IF !EOF()
              lcReturnVal =  PADL(ALLTRIM(STR(IdenStruSegFormat.NextIdentity)) ,IdenStruSeg.Length,"0")+ ALLTRIM(IdenStruSeg.SeparatorAfter)
		      LNREMRESULT = SQLEXEC(lnConnHandSeg ,"UPDATE IdentifierSegmentFormat SET NextIdentity = NextIdentity+ IdentityIncrement  Where IdentifierSegment = '"+IdenStruSeg.Oid+"'")      
            ENDIF  
          ENDIF   
        ELSE
         *lcReturnVal = 
        ENDIF
      ENDSCAN	
    ENDIF
  ENDIF 
ENDIF
ENDIF
RETURN lcReturnVal
* E304037,1 MMT 08/15/2018 Create new function to read the Identifier code structure [P20171120.0011][End]
* E611820,1 MMT 11/13/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
*!*************************************************************
*! Name      : gfCreateGLEntries
*! Developer : Mariam Mazhar
*! Date      : 11/13/2019
*! Purpose   : Global function to update GLTRNHD,GLTRNDT
*! Entry     : E611820,1 
*!*************************************************************
FUNCTION gfCreateGLEntries
PARAMETERS lcGLEntriesTmp, lcContact

LOCAL lnOldAlias  
lnOldAlias  = SELECT(0)
IF !USED('GENERALLEDGERBATCH')
  =gfOpenTable('GENERALLEDGERBATCH','GLBATCHNO')
ENDIF
IF !USED('GLTRNHD')
  =gfOpenTable('GLTRNHD','GLTRNHD')
ENDIF
IF !USED('GLTRNDT')
  =gfOpenTable('GLTRNDT','GLTRNDT')
ENDIF
IF !USED('FSPRD_A')
  =gfOpenTable('FSPRD','COMFYRPRDI','SH','FSPRD_A')
ENDIF
IF !USED('GLSETUP_A')
  =gfOpenTable('GLSETUP','GLSETUP','SH','GLSETUP_A')
ENDIF
llPostAuto = GLSETUP_A.lglbatchst
=gfCloseTable('GLSETUP_A')
IF llPostAuto
  IF !USED('GLACBALS_A')
    =gfOpenTable('GLACBALS','ACCYRPRD','SH','GLACBALS_A')   && CACCTCODE+CFISFYEAR+CFSPPRDID
  ENDIF
ENDIF
lcTmpGl = gfTempName()
SELECT (lcGLEntriesTmp)
SELECT Tran_no,Tran_type,GLFYear,Tran_date,GLPERIOD,glsession,TRAN_DESC  FROM (lcGLEntriesTmp);
	 WHERE !DELETED() GROUP BY Tran_no,Tran_type,GLFYear,Tran_date,GLPERIOD,glsession,TRAN_DESC  INTO CURSOR (lcTmpGl)
	 
*B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
CREATE CURSOR 'USEDGLBATCH'	 (GLBATCH C(6),DStartDATE D(8),DENDDATE D(8),GLBATOID C(30),NTOTDB N(12,2),NTOTCR N(12,2),NTOTAL N(12,2))
SELECT 'USEDGLBATCH'
INDEX on GLBATCH  TAG 'USEDBTCH'
SELECT DISTINC TRAN_DATE FROM (lcGLEntriesTmp) INTO CURSOR 'DatesDis'

SELECT 'DatesDis'
LOCATE
SCAN
  m.Tran_date  = DatesDis.Tran_date
  
  lnQueryBatchRslt = oAriaApplication.RemoteCompanyData.SqlRun("SELECT CGLBATCHNO , OID,DBATPBEG,DBATPEND FROM GENERALLEDGERBATCH Where CBATSTAT = 'U' AND"+;
       " ?m.Tran_date Between dbatpbeg AND dbatpend ",'TMPGLBatch','GENERALLEDGERBATCH',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
  IF lnQueryBatchRslt > 0
    SELECT 'TMPGLBatch'
    LOCATE
    IF !EOF()
      IF !SEEK(TMPGLBatch.CGLBATCHNO ,'USEDGLBATCH','USEDBTCH')
        INSERT INTO 'USEDGLBATCH'(GLBATCH ,DStartDATE ,DENDDATE ,GLBATOID) VALUES (TMPGLBatch.CGLBATCHNO ,TMPGLBatch.DBATPBEG,TMPGLBatch.DBATPEND,TMPGLBatch.OID)
      ENDIF
    ENDIF
  ENDIF
ENDSCAN 
*B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]
	 
	 
SELECT (lcTmpGl)
SCAN 
  m.cGLTranNo = gfSequence('CGLTRANNO')
  m.CGLBATCHNO = ''
  m.BatchOID = ''
  m.TRANOID = ''
  SCATTER MEMO MEMVAR
  ** Check if There is an open batch
  *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
*!*	  lnQueryBatchRslt = oAriaApplication.RemoteCompanyData.SqlRun("SELECT CGLBATCHNO , OID FROM GENERALLEDGERBATCH Where CBATSTAT = 'U' AND"+;
*!*	  " ?m.Tran_date Between dbatpbeg AND dbatpend ",'TMPGLBatch','GENERALLEDGERBATCH',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
*!*	  IF lnQueryBatchRslt > 0
*!*	    SELECT 'TMPGLBatch'
*!*	    LOCATE 
*!*	    IF !EOF()
*!*	      m.CGLBATCHNO  = TMPGLBatch.CGLBATCHNO
*!*	      m.BatchOID = TMPGLBatch.OID
*!*	      m.CPOSTSESS = ''
*!*	      USE IN 'TMPGLBatch'
*!*	    ENDIF
*!*	  ENDIF
  SELECT 'USEDGLBATCH'
  LOCATE FOR BETWEEN(m.Tran_date,DStartDATE ,DENDDATE)
  IF FOUND()
    m.CGLBATCHNO  = USEDGLBATCH.GLBATCH 
    m.BatchOID = USEDGLBATCH.GLBATOID
    m.CPOSTSESS = ''
  ENDIF
  *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]
  
  IF EMPTY(m.CGLBATCHNO)
    m.dbatpbeg   = m.Tran_date
    m.dbatpend   = m.Tran_date
    SELECT 'FSPRD_A'
    LOCATE FOR BETWEEN(m.Tran_date,dfsppbgdt,dfsppendt)
    IF FOUND()
      m.dbatpbeg   = FSPRD_A.dfsppbgdt
      m.dbatpend   = FSPRD_A.dfsppendt
    ENDIF
    SELECT GENERALLEDGERBATCH
    APPEND BLANK  
    m.CGLBATCHNO = gfSequence('CGLBATCHNO')
    IF llPostAuto 
      m.CPOSTSESS = gfSequence('CPOSTSESS')
    ENDIF  
    REPLACE CGLBATCHNO WITH m.CGLBATCHNO,;
            cbatstat   WITH IIF(llPostAuto ,'P','U'),;
            lbatind    WITH .F.,;
            cbattype   WITH 'L',;
            cbatpyr    WITH m.GLFYear,;
            dbatpbeg   WITH m.dbatpbeg  ,;
            dbatpend   WITH m.dbatpend   ,;
            cBatrefer  WITH 'On '+ DTOC(oAriaApplication.Systemdate),;
            cbatdesc   WITH 'Created by '+oAriaApplication.User_ID,;
            nbatcntot  WITH  0 ,;
            nbatotdr   WITH  0 ,;
            nbatotcr   WITH  0 ,;
            cbatelusr  WITH  '',;
            dbateldat  WITH  {},;
            cbateltim  WITH  '',;
            cbatarusr  WITH  '',;
            dbatatdat  WITH  {},;
            cbatattim  WITH  '',;
            csrcmodul  WITH  oAriaApplication.ActiveModuleID,;
            ccomp_id   WITH  oAriaApplication.ActiveCompanyID,;
            cpostsess  WITH IIF(llPostAuto AND !EMPTY(m.CPOSTSESS) ,m.cpostsess ,''),;
            cpostprog  WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS) ,'A',''),;
            cpostuser  WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),oAriaApplication.User_ID,''),;
            dpostdate  WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),oAriaApplication.Systemdate,{}),;
            cposttime  WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS), TIME(),'')            
    =gfadd_info('GENERALLEDGERBATCH')        
    =gfReplace('')        
    =gfTableUpdate()
  ENDIF
  IF gfSeek(m.CGLBATCHNO,'GENERALLEDGERBATCH','GLBATCHNO')
    m.BatchOID = GENERALLEDGERBATCH.OID
    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
    IF !SEEK(m.CGLBATCHNO ,'USEDGLBATCH','USEDBTCH')
      INSERT INTO 'USEDGLBATCH'(GLBATCH ,DStartDATE ,DENDDATE ,GLBATOID) VALUES (m.CGLBATCHNO ,GENERALLEDGERBATCH.DBATPBEG,GENERALLEDGERBATCH.DBATPEND,GENERALLEDGERBATCH.OID)
    ENDIF
    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]
  ENDIF  
  SELECT GLTRNHD
  APPEND BLANK 
  REPLACE ntrnindic WITH  0,;
		  CGLBATCHNO WITH m.CGLBATCHNO,;
		  cGLTranNo  WITH m.cGLTranNo,;
		  BatchOID  WITH  m.BatchOID,;
		  ctrndesc  WITH 'Created by '+ oAriaApplication.User_ID,;
		  ctrnrefer WITH 'On '+ DTOC(oAriaApplication.Systemdate),;
		  dtrnpdate WITH m.Tran_date,;
		  ctrnpyr   WITH m.GLFYear,;
		  ctrnpprd  WITH m.GLPERIOD ,;
		  ctrnstat  WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),'P','U'),;
		  ctrntype  WITH 'N',;
		  ctrnrever with  'N',; 
		  dtrnrevdt With {},;
		  ctrnrevyr With "",;
		  ctrnrevpr With "",;
		  ntrntotdr with 0,;
		  ntrntotcr with 0,;
		  csrcmodul with oAriaApplication.ActiveModuleID,;
		  cstandard With 'Y',;
		  csrcjrnl  WITH 'GL',;
		  ccomp_id  With  oAriaApplication.ActiveCompanyID,;
		  cauttype  with '',;
		  cautcode  with '',;
		  cpostsess with IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),m.cpostsess ,''),;
		  cpostprog With IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),'A',''),;
		  cpostuser WITH IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),oAriaApplication.User_ID,''),;
		  dpostdate with  IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS),oAriaApplication.Systemdate,{}),;
		  cposttime with IIF(llPostAuto  AND !EMPTY(m.CPOSTSESS), TIME(),'') ,; 
		  glsession with m.glsession,;
		  Tran_Type with m.Tran_Type ,;
		  TRAN_DESC with m.TRAN_DESC ,;
		  CCONT_ID  with lcContact,;
		  cstatus   with '',;
		  ctran_no  WITH m.Tran_no
		  
    =gfadd_info('GLTRNHD') 
    =gfReplace('')        
    =gfTableUpdate()
    IF gfSeek(m.CGLBATCHNO+m.CGLTRANNO,'GLTRNHD','GLTRNHD')
      m.TRANOID = GLTRNHD.OID
    ENDIF 
     lnLineNo=1   
        SELECT (lcGLEntriesTmp)    		  
  SCAN FOR Tran_no =&lcTmpGl..Tran_no AND Tran_type = &lcTmpGl..Tran_type AND ;
          GLFYear = &lcTmpGl..GLFYear  AND  Tran_date = &lcTmpGl..Tran_date ;
          AND GLPERIOD  = &lcTmpGl..GLPERIOD   and  glsession = &lcTmpGl..glsession  AND TRAN_DESC = &lcTmpGl..TRAN_DESC 
     SCATTER MEMO MEMVAR      
     SELECT GLTRNDT
     APPEND BLANK
     GATHER MEMO MEMVAR       
     REPLACE TRANOID WITH m.TRANOID ,;
             CGLBATCHNO  WITH m.CGLBATCHNO,;
    		 CGLTRANNO WITH m.CGLTRANNO ,; 
    		 CACCTCODE WITH m.GLAccount,;
    		 NAMOUNT WITH ABS(m.nEqvAmnt),;
		     CTRDTEXP WITH "Transaction#:"+m.Tran_No,;
    		 CDRORCR  WITH IIF(m.nEqvAmnt < 0,'C','D'),;
     	    DTRNPDATE WITH oAriaApplication.Systemdate,;
   		  CTRNPYR  WITH m.GLFYear  ,;
   		  CTRNPPRD  WITH m.GLPERIOD  ,;
   		  NENTRYNO WITH lnLineNo,;
   		  LineNo  WITH 0,;
   		  ctran_no  WITH m.Tran_no
		  
		  IF GLTRNDT.CDRORCR ='C'
  		  SELECT GLTRNHD
		    =gfReplace('ntrntotcr with ntrntotcr+GLTRNDT.NAMOUNT')
		    =gfTableUpdate()
		    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
*!*			    SELECT GENERALLEDGERBATCH
*!*			    =gfReplace('nbatotcr with nbatotcr + GLTRNDT.NAMOUNT')
*!*			    =gfReplace('nbatcntot  with nbatcntot  + GLTRNDT.NAMOUNT')
*!*			    =gfTableUpdate()
		    REPLACE NTOTDB WITH NTOTDB+ GLTRNDT.NAMOUNT,;
		            NTOTAL WITH NTOTAL+ GLTRNDT.NAMOUNT IN 'USEDGLBATCH'
		    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]
		  ELSE
  		  SELECT GLTRNHD
		    =gfReplace('ntrntotdr with ntrntotdr + GLTRNDT.NAMOUNT')
		    =gfTableUpdate()
		    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
*!*			    SELECT GENERALLEDGERBATCH
*!*			    =gfReplace('nbatotdr with nbatotdr   + GLTRNDT.NAMOUNT')
*!*			    *=gfReplace('nbatcntot  with nbatcntot  + GLTRNDT.NAMOUNT')
*!*			    =gfTableUpdate()
            REPLACE NTOTCR WITH NTOTCR + GLTRNDT.NAMOUNT IN 'USEDGLBATCH'
		    *B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]
		  ENDIF
		  
		  IF llPostAuto
		    IF gfSeek(m.GLAccount+ m.GLFYear+ m.GLPERIOD ,'GLACBALS_A','ACCYRPRD')
  		    SELECT 'GLACBALS_A'
		      REPLACE nAcBPtdDr  WITH  nAcBPtdDr + IIF(GLTRNDT.CDRORCR ='C',0,GLTRNDT.NAMOUNT),;
                      nAcBYtdDr  WITH  nAcBYtdDr + IIF(GLTRNDT.CDRORCR ='C',0,GLTRNDT.NAMOUNT),;
                      nAcBPtdCr  WITH  nAcBPtdCr + IIF(GLTRNDT.CDRORCR ='D',0,GLTRNDT.NAMOUNT),;
		              nAcBYtdCr  WITH  nAcBYtdCr + IIF(GLTRNDT.CDRORCR ='D',0,GLTRNDT.NAMOUNT),;
                      nAcBClBal  WITH  nAcBClBal + IIF(GLTRNDT.CDRORCR ='D',GLTRNDT.NAMOUNT,-1*GLTRNDT.NAMOUNT)
              = gfReplace('')        
              SKIP
	          SCAN REST WHILE CACCTCODE+CFISFYEAR+CFSPPRDID = m.GLAccount+ m.GLFYear 
                 REPLACE nAcBYtdDr  WITH nAcBYtdDr + IIF(GLTRNDT.CDRORCR ='C',0,GLTRNDT.NAMOUNT),;
                 		nAcBYtdCr  WITH nAcBYtdCr + IIF(GLTRNDT.CDRORCR ='D',0,GLTRNDT.NAMOUNT),;
                		 nAcBOpBal  WITH nAcBOpBal + IIF(GLTRNDT.CDRORCR ='D',GLTRNDT.NAMOUNT,-1*GLTRNDT.NAMOUNT),;
            			 nAcBClBal  WITH nAcBClBal + IIF(GLTRNDT.CDRORCR ='D',GLTRNDT.NAMOUNT,-1*GLTRNDT.NAMOUNT)
		         = gfReplace('')      
 			     ENDSCAN        			 
		      =gfTableUpdate()       			 
            ENDIF     
		  ENDIF
		  
	IF !EMPTY(m.cEntryId)
	  *IF INLIST(m.catg_key,'006','007','008')
  	lnQueryStyInvJlRslt = oAriaApplication.RemoteCompanyData.SqlRun("SELECT rec_no From STYINVJL Where CEntryID = ?m.CEntryID ",'TMPSTYINV','STYINVJL',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
  	IF lnQueryStyInvJlRslt > 0
  	  SELECT TMPSTYINV
  	  LOCATE
  	  IF EOF()
    	  lnQueryStyInvJlRslt = oAriaApplication.RemoteCompanyData.SqlRun("SELECT rec_no From ITEMJRNL Where CEntryID = ?m.CEntryID ",'TMPSTYINV','ITEMJRNL',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))  	
    	  IF lnQueryStyInvJlRslt > 0
     	   SELECT TMPSTYINV
      	  LOCATE 
      	  IF !EOF()
      	    REPLACE ENTRYOID with TMPSTYINV.rec_no IN GLTRNDT
    	    ENDIF  
    	  ENDIF  
    	ELSE
          REPLACE ENTRYOID with TMPSTYINV.rec_no IN GLTRNDT
        ENDIF  
        IF USED('TMPSTYINV')
          USE IN 'TMPSTYINV'
        ENDIF  
      ENDIF
	ENDIF
	REPLACE Oid WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4) IN GLTRNDT
	SELECT GLTRNDT
    =gfadd_info('GLTRNDT') 
    =gfReplace('')
  ENDSCAN
ENDSCAN

*B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][Start]
SELECT 'USEDGLBATCH'
LOCATE
SCAN
  SELECT 'GENERALLEDGERBATCH'
  IF gfSeek(USEDGLBATCH.GLBATCH)
    =gfReplace('nbatotcr   with nbatotcr   + USEDGLBATCH.NTOTCR')
    =gfReplace('nbatotdr   with nbatotdr   + USEDGLBATCH.NTOTDB')
    =gfReplace('nbatcntot  with nbatcntot  + USEDGLBATCH.NTOTAL')
  ENDIF  
ENDSCAN 
SELECT 'GENERALLEDGERBATCH'
=gfTableUpdate()  
*B612549,1 MMT 05/08/2022 User got an error while saving huge Key Off Transactions[T20220215.0003][End]

SELECT GLTRNDT        
=gfTableUpdate()  
=gfCloseTable('GLTRNDT')
=gfCloseTable('FSPRD_A')
=gfCloseTable('GLTRNHD')
=gfCloseTable('GENERALLEDGERBATCH')
SELECT (lnOldAlias)
* E611820,1 MMT 11/13/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][END]
* B612010,1 MMT 02/03/2020 Add new Global function to check if same windows user is running aria4xp or not[T20180807.0005][Start]
*!*************************************************************
*! Name      : lfUserIsRunningAriaExe
*! Developer : Mariam Mazhar
*! Date      : 02/03/2020
*! Purpose   : Global function to check if there is another instance of Aria.exe is running
*! Entry     : B612010,1 
*!*************************************************************
FUNCTION lfUserIsRunningAriaExe
LOCAL liReturn as Integer, lcComputer as String, loCIMV2 as Object, loProcCols as Object, lcUsername as String 
SET STEP ON 
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][START]
DECLARE INTEGER Sleep IN WIN32API INTEGER
sleep(1000)
LLRETError= .F.
lcOldErrVal = ON('Error')
ON ERROR LLRETError = .T.
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][END]
lcLogInUserName = SYS(0)
lcLogInUserName = ALLTRIM(SUBSTR(lcLogInUserName , AT("#",lcLogInUserName) + 1))
liReturn	= 0
lcComputer	= [.]
loCIMV2		= GETOBJECT([winmgmts:{impersonationLevel=impersonate}!\\] + lcComputer + [\root\cimv2])
loProcCols	= loCIMV2.ExecQuery([select * from Win32_Process where name='aria.exe'])
lnExeisRunning = 0
FOR EACH objProcess in loProcCols
	lcUsername	= SPACE(256)
	liReturn	= objProcess.GetOwner(lcUsername)
	m.ProgName	= objProcess.Name
	m.ProgPath	= objProcess.ExecutablePath
	* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][Start]
	strNameOfUser = ''
	strUserDomain = ''
	colProperties = objProcess.GetOwner(@strNameOfUser,@strUserDomain)
	* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][End]     
	IF EMPTY(m.ProgPath) OR ISNULL(m.ProgPath)
 	  LOOP 
 	ELSE
  	  * B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][Start]
  	  *IF FILE(m.ProgPath)
      IF FILE(m.ProgPath) AND ALLTRIM(UPPER(lcLogInUserName)) == ALLTRIM(UPPER(strNameOfUser))
      * B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][End]
        lnExeisRunning = lnExeisRunning + 1 
      ENDIF  
	ENDIF
ENDFOR 
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][START]
ON ERROR &lcOldErrVal.
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][End]
IF lnExeisRunning > 1
  RETURN .F.
ELSE
  RETURN .T.  
ENDIF
* B612010,1 MMT 02/03/2020 Add new Global function to check if same windows user is running aria4xp or not[T20180807.0005][End]
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][START]
IF LLRETError
  RETURN .t.
ENDIF 
* B612218,1 MMT 09/21/2020 Windows update on SAAS does not allow running more than one aria.exe + random error while running ARIA4XP[T20200910.0004][END]
* E612596,1 MMT 07/26/2022 Add memo field to the profiles list and profiles screens [T20220407.0001][Start]
*!*************************************************************
*! Name      : lfGetTextProfiles
*! Developer : Mariam Mazhar
*! Date      : 07/27/2022
*! Purpose   : Global function to Read memo field of the profiles
*! Entry     : E612596,1 
*!*************************************************************
FUNCTION lfGetTextProfiles
LPARAMETERS lcType,lcKey

lcReturnText = ''
IF !USED('ProfValu_T')
  =gfOpenTable('ProfValu','PROFILE','SH','ProfValu_T')
ENDIF
lnSelAlias = SELECT()
IF gfSeek(PADR(lcType,2)+PADR(lcKey,130),'ProfValu_T')
  SELECT ProfValu_T
  SCAN REST WHILE  CPRO_TYPE+CKEY+CPRO_CODE=PADR(lcType,2)+PADR(lcKey,130) FOR !EMPTY(mnotes)
    lcReturnText = lcReturnText + ALLTRIM( ProfValu_T.cpro_value)+":" +CHR(13)+CHR(10)+ProfValu_T.mnotes+CHR(13)+CHR(10)+CHR(13)+CHR(10)
  ENDSCAN
ENDIF  
=gfCloseTable('ProfValu_T')
SELECT(lnSelAlias)
RETURN lcReturnText 
* E612596,1 MMT 07/26/2022 Add memo field to the profiles list and profiles screens [T20220407.0001][End]