*!*****************************************************************************************
*! Name      : ARRVKEY
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/24/2006
*! Purpose   : Reverse Key Off screen
*! Entry no. : N040245 - Reverse Key Off screen
*!*****************************************************************************************
*:Modifications :
*:B608241,1 WAM 08/27/2007 Fix error while saving
*:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has
*:                          related keyoff lines in ARHIST file not reversed yet.
*:B609241,1 HES 05/09/2010 Aria4xp - AR- Reverse keyoff problem [T20100329.0010]
*:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[T20110627.0031]
*:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*:B610709,1 TMI 04/09/2014 deal with accounts having single quote in their names [T20140325.0040 ] 
*!B611755,1 HMS 31/03/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005]
*!E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004]
*!*****************************************************************************************

#INCLUDE r:\ARIA4XP\SCREENS\AR\ARRVKEY.H
*:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"\AR\ARRVKEY.Scx")
=gfCallForm('ARRVKEY','AR')
*:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
RETURN

DEFINE CLASS Rev_key_off AS Custom
loForm      = ""
loFormSet   = ""
llGLLink    = ""
llMltCur    = ""
llRepCBk    = ""
llApLink    = ""
lcTmpArhist = ""
lcArhist    = ""
lcCredit    = ""
llArhist    = .F.
lcTmpGLDIS  = ""
lcGlFYear   = SPACE(4)          && Fiscal Year
lcGlPeriod  = SPACE(2)          && Period

llSelectIvertEna = .F.
llSelectEna  = .F.
llSelectAllEna = .F.
llSelectnOneEna = .F.
lcSelectCap = ""

llSelectIvertEna2 = .F.
llSelectEna2  = .F.
llSelectAllEna2 = .F.
llSelectnOneEna2 = .F.
lcSelectCap2 = ""
*:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
llvaliddat = .F.
ldReverseDate = {}
*:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
*!*****************************************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Init fuction
*!*****************************************************************************************
FUNCTION lfInit

LPARAMETERS loFrm
SET MULTILOCKS ON
This.loForm    = loFrm
This.loFormSet = loFrm.Parent
THis.lcTmpArhist = gfTempName()
THIS.lcArhist  = gfTempName()
THIS.lcCredit  = gfTempName()
this.lcTmpGLDIS = gfTempName()
This.lfcrtTemp()
This.llGLLink   = (UPPER(ALLTRIM(gfGetMemVar("M_LINK_GL"  ))) = "Y")    && GL Link status.
This.llMltCur   = gfGetMemVar("llMulCurr")                              && Multi currency status.
This.llRepCBk   = (OCCURS("SR", oAriaApplication.CompanyInstalledModules )#0) AND ;
                  (UPPER(ALLTRIM(gfGetMemVar("M_REPCB")))="Y")          && Charge back sales rep setup.
This.llApLink  = "AP" $ oAriaApplication.CompanyInstalledModules

This.lfOpenFile('Credit','Credit')
This.lfOpenFile('Customer','Customer')
This.lfOpenFile('ARHIST','ARHISTHT')
This.lfOpenFile('DEBIT','DEBIT')
*!*****************************************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : fuction to validate account
*!*****************************************************************************************
FUNCTION lfvAccount

LOCAL lcSelAccount
   lcSelAccount =PADR(ALLTRIM(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value), FSIZE('Account', 'Customer'))
   * IIF(!EMPTY(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value),ALLTRIM(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value),This.loFormSet.AriaForm1.kbAccount.keytextbox.Value)

   IF This.loFormSet.AriaForm1.kbAccount.selectedfrombrowse  OR (!EMPTY(lcSelAccount) AND !This.Customer.SEEK('M'+lcSelAccount))
     DO CUSBROWM WITH lcSelAccount
     This.loFormSet.AriaForm1.kbAccount.selectedfrombrowse = .F.
     This.loFormSet.ariaform1.kbaccount.keytextbox.Value = lcSelAccount
   ELSE
     IF (!EMPTY(lcSelAccount) AND This.Customer.SEEK('M'+lcSelAccount))
       This.loFormSet.ariaform1.kbaccount.keytextbox.Value = lcSelAccount
	 ENDIF
   ENDIF

   IF This.Customer.SEEK('M' +lcSelAccount)
	 This.loFormSet.ariaform1.txtAccName.Value     = Customer.BtName
	 This.loFormSet.ariaform1.txtAccCity.Value     = Customer.cAddress32
	
	 lcAcc = ALLT(lcSelAccount)
     lnPayCnt = 0
     lnHistCnt = 0
     This.CREDIT.Seek(lcAcc)
     SELECT('CREDIT')
     COUNT TO lnPayCnt while Account = lcAcc FOR TranType = "4"
     This.lfTmpHist(lcAcc)
   ELSE
     This.loFormSet.ariaform1.txtAccName.Value     = SPACE(30)
	 This.loFormSet.ariaform1.txtAccCity.Value     = SPACE(30)
   ENDIF

*!*****************************************************************************************
*! Name      : lfOpenFile
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile, lcTag
LOCAL lcProp
  lcFile = JUSTSTEM(lcFile)
  lcTag = JUSTSTEM(lcTag)
  lcProp = lcFile
  IF !PEMSTATUS(This,lcProp,5)
    This.addproperty(lcProp)
  ENDIF
  lcProp = 'This.'+lcProp

  IF TYPE(lcProp)<>'O'
    &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,This.loFormSet.DataSessionID)
  ELSE
    &lcProp..SetOrder(lcTag)
  ENDIF
ENDFUNC
*!*************************************************************
*! Name      : lfTmpHist
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to create temporary file contain key off sessions.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfTmpHist()
*!*************************************************************
FUNCTION lfTmpHist
PARAMETERS LcAcc

This.loFormSet.AriaForm1.LockScreen = .T.

*B610709,1 TMI 04/09/2014 17:28 [Start] deal with accounts having single quote in their names [T20140325.0040 ] 
*THIS.ARHIST.Sqlrun("SELECT DISTINCT Account,History , Histdate , Totdb , Totcr ,Openamt ;
					FROM ARHIST ;
					WHERE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO ='"+ LcAcc +"' AND (!EMPTY(Totdb) OR !EMPTY(Totcr))",'Arhist_Temp')
THIS.ARHIST.Sqlrun("SELECT DISTINCT Account,History , Histdate , Totdb , Totcr ,Openamt ;
					FROM ARHIST ;
					WHERE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO ='"+ STRTRAN(LcAcc,"'","''") +"' AND (!EMPTY(Totdb) OR !EMPTY(Totcr))",'Arhist_Temp')
*B610709,1 TMI 04/09/2014 17:28 [End  ] 

IF USED(THIS.lcTmpArhist) AND RECCOUNT(THIS.lcTmpArhist) > 0
  SELECT(THIS.lcTmpArhist)
  ZAP
ENDIF

SELECT('Arhist_Temp')
SCAN
  SCATTER MEMO MEMVAR
  INSERT INTO (THIS.lcTmpArhist) FROM MEMVAR
ENDSCAN
SELECT(THIS.lcTmpArhist)
LOCATE
IF !EOF(THIS.lcTmpArhist)
  REPLACE ALL  llSelect WITH .F.
  This.llSelectEna2  =.T.
  This.llSelectnOneEna2 =.F.
  This.llSelectAllEna2 =.T.
  This.llSelectIvertEna2 =.T.
ELSE
  This.llSelectEna2  =.F.
  This.llSelectnOneEna2 =.F.
  This.llSelectAllEna2 =.F.
  This.llSelectIvertEna2 =.F.
ENDIF
LOCATE
THIS.ARHIST.Seek(LCACC)
IF USED(THIS.lcArhist) AND RECCOUNT(THIS.lcArhist) > 0
  SELECT(THIS.lcArhist)
  ZAP
ENDIF
SELECT('ARHIST')
SCAN REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO =LcAcc &&FOR (!EMPTY(Totdb) OR !EMPTY(Totcr))
  SCATTER MEMO MEMVAR
  INSERT INTO (THIS.lcArhist) FROM MEMVAR
ENDSCAN
This.llArhist = IIF(RECCOUNT(THIS.lcArhist) > 0 ,.T.,.F.)
SELECT(THIS.lcTmpArhist)
SET RELATION TO ACCOUNT+HISTORY INTO (THIS.lcArhist)


THIS.Credit.Seek(LCACC)
IF USED(THIS.lcCredit) AND RECCOUNT(THIS.lcCredit) > 0
  SELECT(THIS.lcCredit)
  ZAP
ENDIF

SELECT('Credit')
SCAN REST WHILE Account = LcAcc FOR  !EMPTY(Account) AND Trantype ='4'
  SCATTER MEMO MEMVAR
  INSERT INTO (THIS.lcCredit) FROM MEMVAR
ENDSCAN

SELECT(THIS.lcCredit)
LOCATE
IF !EOF(THIS.lcCredit)
  REPLACE ALL lSelect WITH .F.
  This.llSelectEna  =.T.
  This.llSelectnOneEna =.F.
  This.llSelectAllEna =.T.
  This.llSelectIvertEna =.T.
ELSE
  This.llSelectEna  =.F.
  This.llSelectnOneEna =.F.
  This.llSelectAllEna =.F.
  This.llSelectIvertEna =.F.
ENDIF
LOCATE
This.loFormSet.changemode('E')
This.loFormSet.AriaForm1.LockScreen = .F.
*!*****************************************************************************************
*! Name      : lfcrtTemp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : fuction to create temp files
*!*****************************************************************************************
FUNCTION lfcrtTemp

DIMENSION laLines[7,4]

laLines[1,1]='llSelect'
laLines[1,2]='L'
laLines[1,3]= 1
laLines[1,4]= 0

laLines[2,1]='History'
laLines[2,2]='C'
laLines[2,3]= 6
laLines[2,4]= 0

laLines[3,1]='histdate'
laLines[3,2]='D'
laLines[3,3]=8
laLines[3,4]=0

laLines[4,1]='Account'
laLines[4,2]='C'
laLines[4,3]= 5
laLines[4,4]= 0

laLines[5,1]='Totdb'
laLines[5,2]='N'
laLines[5,3]=11
laLines[5,4]=2

laLines[6,1]='Totcr'
laLines[6,2]='N'
laLines[6,3]=11
laLines[6,4]=2

laLines[7,1]='Openamt'
laLines[7,2]='N'
laLines[7,3]=14
laLines[7,4]=2

gfCrtTmp(THIS.lcTmpArhist,@laLines,"Account+History+DTOS(Histdate)",THIS.lcTmpArhist,.F.)

DIMENSION laLine[9,4]

laLine[1,1]='Account'
laLine[1,2]='C'
laLine[1,3]= 5
laLine[1,4]= 0

laLine[2,1]='History'
laLine[2,2]='C'
laLine[2,3]= 6
laLine[2,4]= 0


laLine[3,1]='Tran'
laLine[3,2]='C'
laLine[3,3]= 6
laLine[3,4]= 0

laLine[4,1]='Store'
laLine[4,2]='C'
laLine[4,3]= 8
laLine[4,4]= 0

laLine[5,1]='Trandate'
laLine[5,2]='D'
laLine[5,3]=8
laLine[5,4]=0

laLine[6,1]='Duedate'
laLine[6,2]='D'
laLine[6,3]= 8
laLine[6,4]= 0

laLine[7,1]='Batch'
laLine[7,2]='C'
laLine[7,3]=6
laLine[7,4]=0

laLine[8,1]='Amount'
laLine[8,2]='N'
laLine[8,3]=14
laLine[8,4]=2

laLine[9,1]='Desc'
laLine[9,2]='C'
laLine[9,3]=20
laLine[9,4]=0

gfCrtTmp(THIS.lcArhist,@laLine,"Account+History",THIS.lcArhist,.F.)

DIMENSION laCredit[7,4]

laCredit[1,1]='Tran'
laCredit[1,2]='C'
laCredit[1,3]= 6
laCredit[1,4]= 0

laCredit[2,1]='Trandate'
laCredit[2,2]='D'
laCredit[2,3]= 8
laCredit[2,4]= 0

laCredit[3,1]='Desc'
laCredit[3,2]='C'
laCredit[3,3]= 20
laCredit[3,4]= 0

laCredit[4,1]='Amount'
laCredit[4,2]='N'
laCredit[4,3]= 14
laCredit[4,4]= 2

laCredit[5,1]='Store'
laCredit[5,2]='C'
laCredit[5,3]= 8
laCredit[5,4]= 0

laCredit[6,1]='Batch'
laCredit[6,2]='C'
laCredit[6,3]=6
laCredit[6,4]=0

laCredit[7,1]='lSelect'
laCredit[7,2]='L'
laCredit[7,3]= 1
laCredit[7,4]= 0

gfCrtTmp(THIS.lcCredit,@laCredit,"Tran",THIS.lcCredit,.F.)
*!*****************************************************************************************
*! Name      : lfvSearch
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : fuction to search for store #
*!*****************************************************************************************
FUNCTION lfvSearch
lcCheck = SPACE(8)
*:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"AR\arrvksrc.SCX")
=gfCallForm('arrvksrc','AR')
*:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF !EMPTY(lcCheck)
  This.lfvFind(lcCheck)
ENDIF
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : function to make selection
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType
PRIVATE llSelAll
*N000682,1 12/18/12 TMI Globlization changes[Start] 
loFormSet = This.loFormSet
*N000682,1 12/18/12 TMI Globlization changes[End  ] 

llSelAll  = .F.
lnRecNo = RECNO()
DO CASE
  CASE lcType = 'S'

    SELECT (THIS.lcCredit)

    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
    llVld = .T.
    LCAccount = This.loFormSet.AriaForm1.kbAccount.keytextbox.Value
    SELECT ARHIST
    lcOrder = ORDER()
    this.ARHIST.SetOrder('ARHISTT')

    IF This.ARHIST.SEEK(LCAccount +EVALUATE(THIS.lcCredit+'.tran'))
      LOCATE REST WHILE ACCOUNT+TRAN+CINSTALNO = ACCOUNT + EVALUATE(THIS.lcCredit+'.tran') for trantype = Credit.TRANTYPE
      IF FOUND()
         REPLACE lSelect WITH  .F. IN (This.lcCredit)
         llVld = .F.
      ENDIF
    ENDIF

    SELECT ARHIST
    this.ARHIST.SetOrder(lcOrder)
    SELECT (THIS.lcCredit)
    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
    *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
    *IF EVALUATE(THIS.lcCredit+'.TranDate') > oariaapplication.systemdate
    IF EVALUATE(THIS.lcCredit+'.TranDate') > THIS.ldReverseDate
    *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_date)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_date,loFormSet.GetHeaderText("LANG_Tran_date",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap = LANG_Select
This.lcSelectCap = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      REPLACE lSelect WITH .F.
      RETURN
    ELSE

      * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
      IF !llVld
        REPLACE lSelect WITH .F.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Revrs_Fail)
=gfModalGen('INM00000B00000','ALERT','','',LANG_Revrs_Fail)
*N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN
      ENDIF
      * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

      REPLACE lSelect WITH !lSelect
    ENDIF

  CASE lcType = 'A'

     llSelAll  = .F.
     llFound = .F.

     * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
     llVld = .T.
     * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

	 SELECT (THIS.lcCredit)
	 lnRec = RECNO()
	 SCAN
	     *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
       *IF EVALUATE(THIS.lcCredit+'.TranDate') <= oariaapplication.systemdate
       IF EVALUATE(THIS.lcCredit+'.TranDate') <= THIS.ldReverseDate
       *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
         REPLACE lSelect WITH .T.
       ELSE
        llFound = .T.
       ENDIF

       * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
       LCAccount = This.loFormSet.AriaForm1.kbAccount.keytextbox.Value
       SELECT ARHIST
       lcOrder = ORDER()
       this.ARHIST.SetOrder('ARHISTT')

       IF this.ARHIST.SEEK(LCAccount + tran,'ARHISTT')
         LOCATE REST WHILE ACCOUNT+TRAN+CINSTALNO = ACCOUNT + EVALUATE(THIS.lcCredit+'.tran') for trantype = Credit.TRANTYPE
         IF FOUND()
           REPLACE lSelect WITH  .F. IN (This.lcCredit)
           llVld = .F.
         ENDIF
       ENDIF

       SELECT ARHIST
       this.ARHIST.SetOrder(lcOrder)
       SELECT (THIS.lcCredit)
       * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

     ENDSCAN

     * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
     IF !llVld
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_RevrsA_Fail)
=gfModalGen('INM00000B00000','ALERT','','',LANG_RevrsA_Fail)
*N000682,1 11/20/2012 MMT Globlization changes[End]

     ENDIF
     * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

     IF llFound
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_Fail)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_Fail,loFormSet.GetHeaderText("LANG_Tran_Fail",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

     ENDIF

     *REPLACE  ALL lSelect WITH .T.
     GO lnRec
     THIS.llSelectAllEna =.F.
  CASE lcType = 'N'
     llSelAll  = .T.
	 SELECT (THIS.lcCredit)
     REPLACE  ALL lSelect WITH .F.
     THIS.llSelectnOneEna =.F.

  CASE lcType = 'V'

    llFound = .F.
    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
    llVld = .T.
    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
    SELECT (THIS.lcCredit)

    SCAN
      IF !lSelect
        *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
        *IF  EVALUATE(THIS.lcCredit+'.TranDate') <= oariaapplication.systemdate
        IF  EVALUATE(THIS.lcCredit+'.TranDate') <= THIS.ldReverseDate
        *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
          REPLACE lSelect WITH .T.
        ELSE
          llFound = .T.
        ENDIF

        * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
        LCAccount = This.loFormSet.AriaForm1.kbAccount.keytextbox.Value
        SELECT ARHIST

        lcOrder = ORDER()
        this.ARHIST.SetOrder('ARHISTT')

        IF this.ARHIST.SEEK(LCAccount + tran,'ARHISTT')
          LOCATE REST WHILE ACCOUNT+TRAN+CINSTALNO = ACCOUNT + EVALUATE(THIS.lcCredit+'.tran') for trantype = Credit.TRANTYPE
          IF FOUND()
            REPLACE lSelect WITH  .F. IN (This.lcCredit)
            llVld = .F.
          ENDIF
        ENDIF

        SELECT ARHIST
        this.ARHIST.SetOrder(lcOrder)
        SELECT (THIS.lcCredit)
        * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

      ELSE
        REPLACE lSelect WITH !lSelect
      ENDIF
    ENDSCAN

    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.
    IF !llVld
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_RevrsA_Fail)
=gfModalGen('INM00000B00000','ALERT','','',LANG_RevrsA_Fail)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF
    * *:B608835,1 HES 02/04/2009 Prevent selecting an open balance line to reverse if it has related keyoff lines in ARHIST file not reversed yet.

    IF llFound
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_Fail)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_Fail,loFormSet.GetHeaderText("LANG_Tran_Fail",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF

    llSelAll  = !llSelAll
 ENDCASE

 This.llSelectAllEna =llSelAll

 SELECT (THIS.lcCredit)


 LOCATE FOR lSelect

 IF FOUND()
   THIS.llSelectnOneEna =.T.
 ELSE
   THIS.llSelectnOneEna =.F.
 ENDIF

LOCATE FOR !lSelect

IF FOUND()
  This.llSelectAllEna = .T.
ELSE
  This.llSelectAllEna = .F.
ENDIF

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo
ENDIF

 THIS.llSelectIvertEna =.T.

 This.lfvpbSel()

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel

*N000682,1 12/18/12 TMI Globlization changes[Start] 
loFormSet = This.loFormSet
*N000682,1 12/18/12 TMI Globlization changes[End  ] 
SELECT (THIS.lcCredit)
IF lSelect
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap = LANG_unSelect
This.lcSelectCap = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap = LANG_Select
This.lcSelectCap = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF


*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : function to make selection
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvSelect2
PARAMETERS lcType

*N000682,1 12/18/12 TMI Globlization changes[Start] 
loFormSet = This.loFormSet
*N000682,1 12/18/12 TMI Globlization changes[End  ] 
PRIVATE llSelAll
llSelAll  = .F.
lnRecNo = RECNO()
DO CASE
  CASE lcType = 'S'
    SELECT (THIS.lcTmpArhist)
    *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
    *IF EVALUATE(THIS.lcTmpArhist + '.Histdate') > oariaapplication.systemdate
    IF EVALUATE(THIS.lcTmpArhist + '.Histdate') > THIS.ldReverseDate
    *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_date)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_date,loFormSet.GetHeaderText("LANG_Tran_date",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap2 = LANG_Select
This.lcSelectCap2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN
    ENDIF
    IF !This.lfSelAcc()
      =gfModalGen('INM40174B00000','ALERT',EVALUATE(THIS.lcTmpArhist + '.HISTORY'))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap2 = LANG_Select
This.lcSelectCap2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN
    ENDIF
    REPLACE llSelect WITH !llSelect
  CASE lcType = 'A'
     llFound = .F.
     lnRec = RECNO()
     SELECT (THIS.lcTmpArhist)
     SCAN
       IF !This.lfSelAcc()
         =gfModalGen('INM40174B00000','ALERT',EVALUATE(THIS.lcTmpArhist + '.HISTORY'))
         LOOP
       ENDIF
       *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
       *IF EVALUATE(THIS.lcTmpArhist + '.Histdate') <= oariaapplication.systemdate
       IF EVALUATE(THIS.lcTmpArhist + '.Histdate') <= THIS.ldReverseDate
       *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
   	     REPLACE llSelect WITH .T.
       ELSE
        llFound = .T.
       ENDIF
     ENDSCAN
     IF llFound
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_Fail)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_Fail,loFormSet.GetHeaderText("LANG_Tran_Fail",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

     ENDIF
     GO lnRec
     llSelAll  = .F.
     THIS.llSelectAllEna2 =.F.
  CASE lcType = 'N'
     llSelAll  = .T.
	 SELECT (THIS.lcTmpArhist)
     REPLACE  ALL llSelect WITH .F.
     THIS.llSelectnOneEna2 =.F.

  CASE lcType = 'V'
    llSelAll  = !llSelAll
    llFound = .F.
    SELECT (THIS.lcTmpArhist)
    SCAN
      IF !llSelect
        *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
        *IF  EVALUATE(THIS.lcTmpArhist+'.Histdate') <= oariaapplication.systemdate
        IF  EVALUATE(THIS.lcTmpArhist+'.Histdate') <= THIS.ldReverseDate
        *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
          REPLACE llSelect WITH .T.
        ELSE
          llFound = .T.
        ENDIF
      ELSE
        REPLACE llSelect WITH !llSelect
      ENDIF
    ENDSCAN
    IF llFound
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_Tran_Fail)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_Fail,loFormSet.GetHeaderText("LANG_Tran_Fail",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF

 ENDCASE

 This.llSelectAllEna2 =llSelAll

 SELECT (THIS.lcTmpArhist)


 LOCATE FOR llSelect

 IF FOUND()
   THIS.llSelectnOneEna2 =.T.
 ELSE
   THIS.llSelectnOneEna2 =.F.
 ENDIF

 LOCATE FOR !llSelect

 IF FOUND()
   This.llSelectAllEna2 = .T.
 ELSE
   This.llSelectAllEna2 = .F.
 ENDIF

 IF BETWEEN(lnRecNo,1,RECCOUNT())
   GO RECORD lnRecNo
 ENDIF

 THIS.llSelectIvertEna =.T.

 This.lfvpbSel2()

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel2
SELECT (THIS.lcTmpArhist)
*N000682,1 12/18/12 TMI Globlization changes[Start] define the loformset
loFormSet = This.loFormSet 
*N000682,1 12/18/12 TMI Globlization changes[End  ] 
IF llSelect
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap2 = LANG_unSelect
This.lcSelectCap2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcSelectCap2 = LANG_Select
This.lcSelectCap2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : The validation function for proceed button that
*!             reverse selected key offs or payments.
*!*************************************************************
*! Calls     : lfAccRev,lfUpdSale,lfUpdateD,lfUpdateC,
*!             lfDelhc,lfwBrow.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lpSavScr()
*!*************************************************************
FUNCTION lpSavScr


lcAccount = This.loForm.kbaccount.KeyTextbox.value

*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]
IF This.loForm.cboType.Value = 'N'
  lcAccount = SPACE(5)
ENDIF
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][End]

PRIVATE lnRecCnt,llSelCre,llSelHist
llSelCre = .F.
llSelHist = .F.
lnRecCnt = 0

SELECT(THIS.lcCredit)
LOCATE FOR lSelect
IF FOUND()
  llSelCre = .T.
ENDIF

SELECT(THIS.lcTmpArhist)
LOCATE FOR llSelect
IF FOUND()
  llSelHist = .T.
ENDIF

*N000682,1 12/18/12 TMI Globlization changes[Start] 
loFormSet = This.loFormSet
*N000682,1 12/18/12 TMI Globlization changes[End  ]
IF !llSelHist  AND !llSelCre
   *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000','ALERT','','',LANG_No_Trans)
=gfModalGen('INM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Trans,loFormSet.GetHeaderText("LANG_No_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  llcSave = .F.
  RETURN .F.
ENDIF

IF This.llRepCBk
  This.lfOpenFile('REPCOMM','REPCOMM')
  This.lfOpenFile('SalesRep','SalesRep')
ENDIF


IF This.llGLLink
  This.lfOpenFile('GLDIST','GLDISTNO')
  IF !USED(this.lcTmpGLDIS)
    SELECT GLDIST
    =AFIELDS(laFileStru)
    CREATE TABLE (oAriaApplication.WorkDir +THIS.lcTmpGLDIS) FROM ARRAY laFileStru
*    gfCrtTmp(THIS.lcTmpGLDIS ,@laFileStru,"TRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY",THIS.lcTmpGLDIS,.F.)
  ENDIF
ENDIF

IF This.llApLink
  This.lfOpenFile('ApPayMnt','TypMethDoc')
ENDIF

llUpdate = .F.  && So far no update has been done

SELECT(THIS.lcCredit)
LOCATE
SCAN FOR lSelect
  lnRecno = RECNO()
  *E000000,1 Message : 40169
  *E000000,1 Are you sure you want to reverse payment # 99999
  *E000000,1 Button : 40003
  *E000000,1 Proceed Cancel
  IF THIS.CREDIT.SEEK(lcAccount + tran,"CREDIT")  AND ;
   gfModalGen('TRM40169B40003','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reserve,loFormSet.GetHeaderText("LANG_Reserve",loFormSet.HeaderAlias)) + CREDIT.Tran ) = 1

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Reserve + CREDIT.Tran WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reserve,loFormSet.GetHeaderText("LANG_Reserve",loFormSet.HeaderAlias)) + CREDIT.Tran WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    IF This.llGlLink
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Upd_Gl  WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Upd_Gl,loFormSet.GetHeaderText("LANG_Upd_Gl",loFormSet.HeaderAlias))  WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *-- 1) TOTAL GROSS MERCHANDISE     <DEBIT>   "CASH RECEIPT"
      *--    CATEGORY KEY FOR "Cash Receipts"..........=> '002'
      *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
*!*        DO GLDIST WITH Credit.Link_Code,'002',CREDIT.Amount,'CR',;
*!*                       Credit.Tran,oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,this.lcTmpGLDIS ,Credit.cAdjAcct,;
*!*                       Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate

*!*        *-- 2) MERCHANDISE DISCOUNT        <CREDIT>  "ACCOUNT RECEIVABLE"
*!*        *--    CATEGORY KEY FOR "Accounts Receivable"....=> '001'
*!*        DO GLDIST WITH Credit.Link_Code,'001',-(CREDIT.Amount),'CR',;
*!*                       Credit.Tran,oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,Credit.cArGlAcc,;
*!*                       Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate
      DO GLDIST WITH Credit.Link_Code,'002',CREDIT.Amount,'CR',;
                     Credit.Tran,THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,this.lcTmpGLDIS ,Credit.cAdjAcct,;
                     Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate

      *-- 2) MERCHANDISE DISCOUNT        <CREDIT>  "ACCOUNT RECEIVABLE"
      *--    CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH Credit.Link_Code,'001',-(CREDIT.Amount),'CR',;
                     Credit.Tran,THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,Credit.cArGlAcc,;
                     Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate
     *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]

    ENDIF


    IF THIS.llApLink
      This.ApPayMnt.SEEK('A'+IIF(Credit.lNonAr,'N','A')+Credit.Store)
      SELECT ApPayMnt
      LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = ;
                        'A'+IIF(Credit.lNonAr,'N','A')+Credit.Store ;
                  FOR   dPayDate = Credit.TranDate .AND. cPayClNo= lcAccount ;
                 .AND.  nPayAmnt = Credit.Amount
      IF FOUND()
        This.ApPayMnt.DELETE ()
      ENDIF
    ENDIF

   *!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]
   IF This.loForm.cboType.Value = 'A'
   *!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][End]
    SELECT CUSTOMER
    This.CUSTOMER.SEEK("M" + lcAccount)
    =RLOCK()

    This.CUSTOMER.REPLACE("OPENCR WITH OPENCR - CREDIT.AMOUNT,;
            				NETBAL WITH NETBAL - CREDIT.AMOUNT")
    UNLOCK
   *!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]
   ENDIF
   *!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][End]
    SELECT CREDIT
    this.credit.DELETE()
    llupdate =.T.   &&To indicate that there is update has been done
  ENDIF
  GO lnRecno IN (THIS.lcCredit)
ENDSCAN

IF this.llGlLink AND llupdate
  SELECT (this.lcTmpGLDIS)
  lcGlSess = gfSequence('GLSESSION')
  REPLACE ALL Glsession WITH lcGlSess
  *E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  =gfCreateGLEntries(this.lcTmpGLDIS,lcAccount)
  *E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End] 
  USE
  *--Update GLDIST File
  SELECT GLDIST
  This.GLDIST.APPENDFromFile(oAriaApplication.WorkDir+this.lcTmpGLDIS)
  =gfOpenFile(oAriaApplication.WorkDir + this.lcTmpGLDIS,'','EX')
  ZAP
ENDIF

SELECT(THIS.lcTmpArhist)
LOCATE
SCAN FOR llSelect
  lnRecordno = RECNO()
  *E000000,1 Message : 40169
  *E000000,1 Are you sure you want to reverse key off session# 99999
  *E000000,1 Button : 40003
  *E000000,1 Proceed Cancel
  IF this.ARHIST.SEEK(lcAccount +History ,"ARHIST")  AND ;
gfModalGen('TRM40169B40003','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Rev_Session,loFormSet.GetHeaderText("LANG_Rev_Session",loFormSet.HeaderAlias)) +ARHIST.History) = 1

     SELECT ARHIST
     GO lnRecordno IN (THIS.lcTmpArhist)
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Rev_Key  + History WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Rev_Key,loFormSet.GetHeaderText("LANG_Rev_Key",loFormSet.HeaderAlias))  + History WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *WAIT WINDOW EVALUATE(THIS.lcTmpArhist+'.History')
    IF !This.lfAccRev(lcAccount)
      =gfModalGen('INM00000B00000','ALERT','','',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CB,loFormSet.GetHeaderText("LANG_CB",loFormSet.HeaderAlias)) + history + ;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_already_key,loFormSet.GetHeaderText("LANG_already_key",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      LOOP
    ENDIF
    GO lnRecordno IN (THIS.lcTmpArhist)


    STORE 0 TO lnExchDif
    lcUnitSign = "/"
    SELECT ARHIST
    =SEEK(lcAccount  + EVALUATE(THIS.lcTmpArhist+'.History'))
    lcLnk = Link_Code


    SCAN REST WHILE ACCOUNT +History +Trantype+Tran = lcAccount  + EVALUATE(THIS.lcTmpArhist+'.History')
      lcAdjAcc  = cAdjAcct
      lcArGlAcc = cArGlAcc

      IF This.llGlLink AND This.llMltCur
        =This.lfUExchDif()
      ENDIF


      SELECT ARHIST

      IF Trantype ="7" OR (Trantype ="2" AND Deb_adj = "Y")
        IF This.llGlLink
          *-- Credit Adjustment

          IF Trantype ="7"
            *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
*!*              DO GLDIST WITH ArHist.Link_Code,'009',ArHist.Amount,'CA',ArHist.Tran, ;
*!*                             oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,this.lcTmpGLDIS,lcAdjAcc,;
*!*                             ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

*!*              DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'CA', ;
*!*                             ArHist.Tran,oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,;
*!*                             lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
            DO GLDIST WITH ArHist.Link_Code,'009',ArHist.Amount,'CA',ArHist.Tran, ;
                           THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,this.lcTmpGLDIS,lcAdjAcc,;
                           ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

            DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'CA', ;
                           ArHist.Tran,THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,;
                           lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
          *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
          ELSE

          *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
*!*               DO GLDIST WITH ArHist.Link_Code,'010',ArHist.Amount,'DA',ArHist.Tran, ;
*!*                              oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,lcAdjAcc,;
*!*                              ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

*!*               DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'DA', ;
*!*                              ArHist.Tran,oariaapplication.systemdate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,;
*!*                              lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
             DO GLDIST WITH ArHist.Link_Code,'010',ArHist.Amount,'DA',ArHist.Tran, ;
                            THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,lcAdjAcc,;
                            ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

             DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'DA', ;
                            ArHist.Tran,THIS.ldReverseDate,THIS.lcGlFYear,THIS.lcGlPeriod,This.lcTmpGLDIS,;
                            lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
            *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
          ENDIF
        ENDIF


        SELECT ARHIST
        IF This.llApLink
          SELECT ApPayMnt
          This.ApPayMnt.SEEK('AA')

          *B608241,1 WAM 08/27/2007 Fix error while saving
          *LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = 'AA' ;
           FOR ((CPAYDOCNO = ArHist.History) .OR. EMPTY(CPAYDOCNO)) ;
               .AND.  dPayDate = ArHist.TranDate .AND. cPayClNo=lcAcc ;
               .AND.  ABS(nPayAmnt) = ABS(ArHist.Amount)
          LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = 'AA' ;
           FOR ((CPAYDOCNO = ArHist.History) .OR. EMPTY(CPAYDOCNO)) ;
               .AND.  dPayDate = ArHist.TranDate .AND. cPayClNo=lcAccount ;
               .AND.  ABS(nPayAmnt) = ABS(ArHist.Amount)
          *B608241,1 WAM 08/27/2007 (End)

          IF FOUND()
            This.ApPayMnt.DELETE()
          ENDIF
        ENDIF


        SELECT ARHIST
        IF This.llRepCBk .AND. Trantype ="7"
          =This.lfUpdSale(TranDate,Tran)
        ENDIF

      ELSE
        SELECT ARHIST
        DO CASE

          CASE INLIST(Trantype,"1","2","3","M")
            =This.lfUpdateD()
          CASE INLIST(Trantype,"0","4","5","6")
            =This.lfUpdateC()
          OTHERWISE
            =This.lfDelhc(lcAccount)
        ENDCASE

        SELECT ARHIST
      ENDIF

      *-- Register a reverse GL transaction..

      IF This.llGlLink
        IF Trantype  $ "89"
          *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
*!*            DO GLDIST WITH ArHist.Link_Code  , "001"   , -(ARHIST.Amount) , "KO",;
*!*                           ArHist.Tran , oariaapplication.systemdate, THIS.lcGlFYear , THIS.lcGlPeriod  ,;
*!*                           this.lcTmpGLDIS, lcArGlAcc, ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
*!*          ELSE
*!*            DO GLDIST WITH ArHist.Link_Code  , "001"  , ARHIST.Amount , "KO",;
*!*                           ArHist.Tran , oariaapplication.systemdate, THIS.lcGlFYear , THIS.lcGlPeriod   ,;
*!*                           this.lcTmpGLDIS, lcArGlAcc , ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
          DO GLDIST WITH ArHist.Link_Code  , "001"   , -(ARHIST.Amount) , "KO",;
                         ArHist.Tran , THIS.ldReverseDate, THIS.lcGlFYear , THIS.lcGlPeriod  ,;
                         this.lcTmpGLDIS, lcArGlAcc, ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
        ELSE
          DO GLDIST WITH ArHist.Link_Code  , "001"  , ARHIST.Amount , "KO",;
                         ArHist.Tran , THIS.ldReverseDate, THIS.lcGlFYear , THIS.lcGlPeriod   ,;
                         this.lcTmpGLDIS, lcArGlAcc , ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
          *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
        ENDIF
      ENDIF
    ENDSCAN



    SELECT ARHIST
    =IIF(lnExchDif <> 0, This.lfUpdDiff(), .T.)



    =IIF(This.llGlLink,This.lfUpdGL(),.T.)



    SELECT ARHIST
    =This.ARHIST.SEEK(lcAccount +  EVALUATE(THIS.lcTmpArhist+'.History'))
    SCAN REST WHILE ACCOUNT+History+Trantype+Tran = LcAccount +  EVALUATE(THIS.lcTmpArhist+'.History')
      This.ARHIST.DELETE()
    ENDSCAN



    llupdate =.T.   &&To indicate that there is update has been done
  ENDIF
  SELECT(THIS.lcTmpArhist)
  GO lnRecordno IN (THIS.lcTmpArhist)
ENDSCAN

SELECT (This.lcTmpArhist)
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]    
*IF llupdate
IF llupdate AND This.loForm.cboType.Value = 'A'   && Open the screen only if there are some lines will be reversed
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][End]
*- Save alias orders to restore them after calling ARAGEAR.PRG program
  PRIVATE laEnvrnmt,lnLn
  lnLn = 0
  FOR lnI = 1 TO 225
    SELECT (lnI)
    IF !EMPTY(ALIAS())
      lnLn = lnLn + 1
      DIMENSION laEnvrnmt[lnLn,2]
      laEnvrnmt[lnLn,1] = ALIAS()
      laEnvrnmt[lnLn,2] = ORDER()
    ENDIF
  ENDFOR
  SAVE TO (oAriaApplication.WorkDir+'TMPMEMVR.MEM') ALL LIKE l*

  *-- Age the customer
  SELECT ArHist
  *:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO (oAriaApplication.ApplicationHome+"ARAGEAR.PRG") WITH lcAccount
  IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+"ARAGEAR.FXP")
    DO (oAriaApplication.CLIENTAPPLICATIONHOME+"ARAGEAR.FXP") WITH lcAccount
  ELSE
    DO (oAriaApplication.ApplicationHome+"ARAGEAR.FXP") WITH lcAccount
  ENDIF
  *:E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  RESTORE FROM (oAriaApplication.WorkDir+'TMPMEMVR.MEM') ADDITIVE
  FOR lnI = 1 TO ALEN(laEnvrnmt,1)
    SET ORDER TO (laEnvrnmt[lnI,2]) IN (laEnvrnmt[lnI,1])
  ENDFOR
ENDIF

SELECT (This.lcTmpArhist)
IF !llupdate
  llcSave = .F.
  RETURN .F.
ENDIF

DIMENSION laTableUpdate[4]
laTableUpdate[1] = This.ARHIST
laTableUpdate[2] = This.Credit
laTableUpdate[3] = This.Customer
laTableUpdate[4] = This.Debit

IF This.llRepCBk
  lnArrLen = ALEN(laTableUpdate)
  DIMENSION laTableUpdate[lnArrLen+2]
  laTableUpdate[lnArrLen+1] = This.RepComm
  laTableUpdate[lnArrLen+2] = This.SalesRep
ENDIF

IF This.llApLink
  lnArrLen = ALEN(laTableUpdate)
  DIMENSION laTableUpdate[lnArrLen+1]
  laTableUpdate[lnArrLen+1] = This.ApPayMnt
ENDIF
IF This.llGLLink
  lnArrLen = ALEN(laTableUpdate)
  DIMENSION laTableUpdate[lnArrLen+1]
  laTableUpdate[lnArrLen+1] = This.GLDIST
ENDIF
=This.lfTableUpdate()
WAIT CLEAR
RETURN .T.
*!*************************************************************
*! Name      : lfUpdSale
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Updating SalesRep file
*!*************************************************************
*! Passed Parameters  : ldDate  = Transaction date
*!                      lcTrano = Tran number in ArHist file
*!*************************************************************
FUNCTION lfUpdSale
PARAMETERS ldDate,lcTrano
PRIVATE lnAlias

lnAlias = SELECT()

*B608241,1 WAM 08/27/2007 Fix error while saving
*this.CUSTOMER.SEEK('M'+lcAcc,"CUSTOMER")
this.CUSTOMER.SEEK('M'+lcAccount,"CUSTOMER")
*B608241,1 WAM 08/27/2007 (End)

IF This.RepComm.SEEK(Customer.Salesrep+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  IF This.SalesRep.SEEK(Customer.Salesrep,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    This.SalesRep.REPLACE("Current WITH Current - RepComm.Amount,;
            Balance WITH Balance - RepComm.Amount")
    UNLOCK
    SELECT REPCOMM
    This.RepComm.DELETE()
  ENDIF
ENDIF
IF This.RepComm.SEEK(Customer.Rep2+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  IF This.SalesRep.SEEK(Customer.Rep2,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    This.SalesRep.REPLACE("Current WITH Current - RepComm.Amount,;
            Balance WITH Balance - RepComm.Amount")
    UNLOCK
    SELECT REPCOMM
    This.RepComm.DELETE()
  ENDIF
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfAccRev
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : function to determine if there are charge back or
*!             credit on account already keyeed off found in the
*!             key off selected to revese.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfAccRev()
*!*************************************************************
FUNCTION lfAccRev
PARAMETERS lcAcc
SCAN REST WHILE ACCOUNT +History +Trantype+Tran = lcAcc + EVALUATE(THIS.lcTmpArhist+'.HISTORY')
  IF (Trantype = "8" AND !This.Debit.SEEK(LcAcc +Tran,"DEBIT")) OR;
     (Trantype = "9" AND !This.Credit.SEEK(LcAcc +Tran,"CREDIT"))
    RETURN .F.
  ENDIF
ENDSCAN
*!*************************************************************
*! Name      : lfUpdateD
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function that udpate debit file.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfUpdateD()
*!*************************************************************
FUNCTION lfUpdateD

IF !This.debit.SEEK(ArHist.Account+ArHist.Tran,'Debit')

  INSERT INTO debit (Account,Store,Trantype,Trancode,Tran,Trandate,Chgbk_date,Desc,Reference,;
    Amount,Batch,Duedate,cFacCode,Dsc_amt,dadd_date,cadd_time,cadd_Ver,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate,cAdjAcct,CarglAcc) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Chgbk_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.Duedate,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,oariaapplication.systemdate,TIME(),'A40',oariaapplication.user_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct,;
           ArHist.CarglAcc)

   This.debit.REPLACE()

ELSE
  SELECT DEBIT
  =RLOCK()
  This.debit.REPLACE ("Amount WITH Amount + ARHIST.Amount")
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfUpdateC
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function that update credit file.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfUpdateC()
*!*************************************************************
FUNCTION lfUpdateC
PRIVATE lcAlias
lcAlias = ALIAS()
IF !This.CREDIT.SEEK(ArHist.Account+ArHist.Tran,'Credit')

  SELECT CREDIT
  INSERT INTO CREDIT (Account,Store,Trantype,cCreditCod,Tran,Trandate,Credt_date,Desc,Reference,;
    Amount,Batch,History,cFacCode,Dsc_amt,dadd_date,cadd_time,cadd_Ver,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate,cAdjAcct,CarglAcc,CBnkCode,CChkAcct,Carptype) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Credt_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.History,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,oariaapplication.systemdate,TIME(),'A40',oariaapplication.user_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct,;
           ArHist.CarglAcc,ArHist.CBnkCode,ArHist.CChkAcct,ArHist.Carptype)

    This.CREDIT.REPLACE()

ELSE
  SELECT Credit
  =RLOCK()
  This.CREDIT.REPLACE(" Amount WITH Amount + ARHIST.Amount")
  UNLOCK
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfDelhc
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function that delete credit on account or charge back.
*!             when reversing the key off.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfDelhc
PARAMETERS lcAcc

IF Trantype = "8"
  IF This.DEBIT.SEEK(LcAcc+Tran,"DEBIT")
    SELECT DEBIT
    This.DEBIT.DELETE()
  ENDIF
ELSE
  IF This.CREDIT.SEEK(LcAcc+Tran,"CREDIT")
    SELECT CREDIT
    This.CREDIT.DELETE()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfUExchDif
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Calculate the diff. in the exchange rate.
*:*************************************************************
*! Example     : =lfUExchDif()
*!*************************************************************
FUNCTION lfUExchDif
lcRateSign = gfGetExSin(@lcUnitSign,cCurrCode)
lnNewVal   = (Amount * IIF(Trantype  $ "89", 1, -1) &lcRateSign nExRate &lcUnitSign nCurrUnit)
lnExchDif  = lnExchDif + lnNewVal

*!*************************************************************
*! Name      : lfUpdDiff
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Create GL entries due to difference in ex. rate
*:*************************************************************
*! Example     : =lfUpdDiff()
*!*************************************************************
FUNCTION lfUpdDiff
PRIVATE lnAlias

lnAlias  = SELECT(0)
lcUseAcc = SPACE(0)

*:B609241,1 HES 05/09/2010 Aria4xp - AR- Reverse keyoff problem [Start]
*!*	This.lfOpenFile('GLSETUP','')
gfOpenFile(oAriaApplication.DATADIR+'GLSETUP','','SH')
*:B609241,1 HES 05/09/2010 Aria4xp - AR- Reverse keyoff problem [End  ]

This.lfOpenFile('GLACCHAR','')
lcUseAcc = ALLTRIM(GLSETUP.cSetExMj)

This.lfOpenFile('ACCOD','Accsegno')

*:B609241,1 HES 05/09/2010 Aria4xp - AR- Reverse keyoff problem [Start]
SELECT ACCOD
*:B609241,1 HES 05/09/2010 Aria4xp - AR- Reverse keyoff problem [Start]

GO TOP
lnAcsSegSz = nAcsSegSz
lnAcsNoSeg = nAcsNoSeg
lcAcsMask  = "X" + SUBSTR(cAcsMask,2)
lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
*-- Prepare account code according to the account code
*-- mask (lcAcsMask) as well as the field content
lcUseAcc = lcUseAcc +;
STRTRAN(SUBSTR(ALLTRIM(lcAcsMask),LEN(lcUseAcc)+1),'9','0') +;
REPLICATE(' ',FSIZE('cAcctCode','GLACCHAR')-lnAcsSegSz)
IF This.llGlLink
  *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[Start]
*!*    DO GLDIST WITH lcLnk, "009", lnExchDif, "EX", "" ,;
*!*                   oariaapplication.systemdate, THIS.lcGlFYear, THIS.lcGlPeriod, This.lcTmpGLDIS,lcUseAcc
  DO GLDIST WITH lcLnk, "009", lnExchDif, "EX", "" ,;
                 THIS.ldReverseDate, THIS.lcGlFYear, THIS.lcGlPeriod, This.lcTmpGLDIS,lcUseAcc
  *:E303005,1 MMT 12/01/2011 reverse\key off screen  has no Reverse date[End]
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpdGL
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Update GL distribution file.
*:*************************************************************
*! Example     : =lfUpdGL()
*!*************************************************************
FUNCTION lfUpdGL
PRIVATE lnAlias

lnAlias  = SELECT(0)

USE (oAriaApplication.WorkDir+This.lcTmpGLDIS) IN 0 SHARED AGAIN ALIAS UseToSum
DIMENSION laAccounts[1,2]
laAccounts = SPACE(0)
SELECT (This.lcTmpGLDIS)
lcGlSess = gfSequence('GLSESSION')
SCAN
  lcGLAcc = GLAccount + Tran_Type
  IF Tran_Type # 'KO'
    lnLen = ALEN(laAccounts, 1)
    lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
    DIMENSION laAccounts[lnLen,2]
    laAccounts[lnLen, 1] = lcGLAcc
    laAccounts[lnLen, 2] = .T.
  ELSE
    IF ASCAN(laAccounts, lcGLAcc) = 0
      SELECT UseToSum
      SUM ALL nEqvAmnt FOR GLAccount+Tran_Type = lcGLAcc  TO lnGLAmount
      lnLen = ALEN(laAccounts, 1)
      lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
      DIMENSION laAccounts[lnLen,2]
      laAccounts[lnLen, 1] = lcGLAcc
      laAccounts[lnLen, 2] = lnGLAmount # 0
    ENDIF
  ENDIF
  SELECT (This.lcTmpGLDIS)
  REPLACE GLSESSION WITH lcGlSess
ENDSCAN

USE IN UseToSum

SELECT (This.lcTmpGLDIS)
SCAN FOR laAccounts[ASUBSCRIPT(laAccounts,ASCAN(laAccounts,GLAccount+Tran_Type),1),2]
  SCATTER MEMVAR MEMO
  this.GLDIST.INSERT ("FROM MEMVAR")
ENDSCAN

*!E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
 *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][Start]
*!*	IF !USED('FSPRD_B')
*!*	  =gfOpenTable('FSPRD','COMFYRPRDI','SH','FSPRD_B')
*!*	ENDIF
 *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][End]
SELECT ARHIST
lcOrdHistval =ORDER()
lcTmpGLD = This.lcTmpGLDIS
SELECT (lcTmpGLD)
LOCATE 
SCAN FOR tran_type ='KO' AND catg_key ='001'
  lnRecNum = RECNO()
  SCATTER MEMVAR MEMO
  
  
  SELECT(THIS.lcArhist)
  llFound = .F.
  SCAN FOR Tran =m.Tran_NO 
   IF SEEK(EVALUATE(THIS.lcArhist+'.Account')+EVALUATE(THIS.lcArhist+'.History'),THIS.lcTmpArhist,THIS.lcTmpArhist)
     SELECT(THIS.lcTmpArhist)
     LOCATE REST WHILE Account+History+DTOS(Histdate)= EVALUATE(THIS.lcArhist+'.Account')+EVALUATE(THIS.lcArhist+'.History') FOR  llSelect
     IF FOUND()
       llFound = .T.  
       EXIT      
     ENDIF  
   ENDIF          
  ENDSCAN
  IF llFound
	SELECT ARHIST
    this.ARHIST.SetOrder('ARHISTHT')  && ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO
    this.ARHIST.SEEK(lcAccount  + EVALUATE(THIS.lcArhist+'.History'))
    LOCATE REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO =lcAccount  + EVALUATE(THIS.lcArhist+'.History') FOR TRAN=m.Tran_NO 
    IF FOUND()
      lcTrnTyp =ARHIST.trantype
      m.tran_type = IIF(lcTrnTyp ='0','RM',IIF(lcTrnTyp ='4',;
	                   'CR',IIF(lcTrnTyp $'56','CA',IIF(lcTrnTyp ='I','VI',IIF(lcTrnTyp ='1','IN',IIF(lcTrnTyp $'23','DA',IIF(lcTrnTyp='R','VR','KO')))))))
      m.cGlRef = EVALUATE(This.lcArhist+'.History')
      *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][Start]
      *m.Tran_date = EVALUATE(This.lcArhist+'.trandate')
      *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][End]
	  m.Tran_desc = This.lfGetTranDesc(m.tran_type)
	  *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][Start]
*!*		  SELECT 'FSPRD_B'
*!*	 	  LOCATE FOR BETWEEN(m.Tran_date,dfsppbgdt,dfsppendt)
*!*		  IF FOUND()
*!*		    m.glperiod =FSPRD_B.cfspprdid
*!*		    m.glfyear =FSPRD_B.cfisfyear
*!*		  ENDIF
	  *!B612238,1 MMT 11/11/2020 Reverse Key off updates the Keyed off Transaction date incorrectly[T20200930.0004][End]
 	  SELECT (lcTmpGLD)
	  GATHER MEMVAR MEMO  
    ENDIF  
  ENDIF
  IF betweEn(lnRecNum,1, RECCOUNT(lcTmpGLD))
     GO RECORD lnRecNum IN (lcTmpGLD)
  ENDIF
ENDSCAN  
IF !EMPTY(lcOrdHistval)
  SELECT ARHIST
  this.ARHIST.SetOrder(lcOrdHistval)
ENDIF
IF USED('FSPRD_B') 
  =gfCloseTable('FSPRD_B')
ENDIF
=gfCreateGLEntries(this.lcTmpGLDIS,lcAccount)
*!E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]

SELECT (This.lcTmpGLDIS)
ZAP
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.
*!*************************************************************
*! Name      : lfvFind
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to search for check # of payments
*:*************************************************************
*! Example     : =lfvFind()
*!*************************************************************
FUNCTION lfvFind
PARAMETERS lcCheck
lcAcc = This.loFormSet.AriaForm1.kbAccount.keytextbox.Value
llFind =.F.
SELECT (THIS.lcTmpArhist)
lcCurrKey = lcAcc+History
=This.ARHIST.SEEK(lcAcc)
SELECT ARHIST
SCAN REST WHILE ACCOUNT+HISTORY = lcAcc FOR Trantype ="4"
  IF ALLTRIM(STORE) = ALLTRIM(lcCheck)
    lcCurrKey = lcAcc+ History
    llFind = .T.
    EXIT
  ENDIF
ENDSCAN
This.ARHIST.SetOrder('ARHISTHT')
SELECT (THIS.lcTmpArhist)
=SEEK(lcCurrKey)
IF !llFind
  *N000682,1 12/18/12 TMI Globlization changes[Start] 
  loFormSet = This.loFormSet
  *N000682,1 12/18/12 TMI Globlization changes[End  ] 
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Session,loFormSet.GetHeaderText("LANG_No_Session",loFormSet.HeaderAlias))  + ALLTRIM(LcCheck))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
*!*************************************************************
*! Name      : lfSelAcc
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to prevent void invoice selection.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .F. if void invoice otherwise return .T.
*!*************************************************************
*! Example            : lfSelAcc()
*!**********************************************************
FUNCTION lfSelAcc
PRIVATE lnArea,lnRecNo,llRetVal
lnArea=SELECT()
lnRecNo=RECNO(This.lcTmpArhist)
llRetVal = .T.
IF !USED("INVHDR")
  This.lfOpenFile('INVHDR','INVHDR')
ENDIF
IF This.ARHIST.SEEK(EVALUATE(this.lcTmpArhist+'.ACCOUNT')+EVALUATE(this.lcTmpArhist+'.HISTORY'))
  SELECT( "ARHIST" )
  SCAN WHILE ACCOUNT+HISTORY = EVALUATE(this.lcTmpArhist+'.ACCOUNT')+EVALUATE(this.lcTmpArhist+'.HISTORY')  FOR  TRANTYPE='1'
    IF This.INVHDR.SEEK(TRAN,"INVHDR")
      IF INVHDR.STATUS="V"
        llRetVal = .F.
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnArea)
GOTO lnRecNo
RETURN llRetVal
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : function to control source
*!*************************************************************
FUNCTION lfAddControlSource
*N000682,1 12/18/12 TMI Globlization changes[Start] define loFormSet
LOCAL loFormSet
loFormSet = This.loFormSet
*N000682,1 12/18/12 TMI Globlization changes[End  ] 

WITH This.loFormSet.ariaForm1.ariapageframe1.pgfHist.grdHist
  .RecordSource = ''
  .RecordSource = This.loFormSet.lorevkeyoff.lcTmpArhist
  .READONLY = .T.
  .colUMN1.CurrentControl =  'Ariagridcheckbox1'
  .colUMN1.ControlSource = 'ThisFormSet.lorevkeyoff.lfgetValueLogic2()'
  *.colUMN1.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.llSelect'
  .colUMN1.READONLY = .F.
  .colUMN2.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.History'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.colUMN2.header1.Caption = LANG_Hist
.colUMN2.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Hist,loFormSet.GetHeaderText("LANG_Hist",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN3.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.Histdate'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.colUMN3.header1.Caption = LANG_Key_date
.colUMN3.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Key_date,loFormSet.GetHeaderText("LANG_Key_date",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN4.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.Totdb'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.colUMN4.header1.Caption = LANG_Tot_Deb
.colUMN4.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tot_Deb,loFormSet.GetHeaderText("LANG_Tot_Deb",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN5.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.Totcr'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.colUMN5.header1.Caption = LANG_Tot_Cred
.colUMN5.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tot_Cred,loFormSet.GetHeaderText("LANG_Tot_Cred",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN6.ControlSource = This.loFormSet.lorevkeyoff.lcTmpArhist + '.Openamt'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.colUMN6.header1.Caption = LANG_Opn_Amt
.colUMN6.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Opn_Amt,loFormSet.GetHeaderText("LANG_Opn_Amt",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDWITH


WITH This.loFormSet.ariaForm1.ariapageframe1.pgfHist.grdHistDet
  .RecordSource = ''
  .RecordSource = This.loFormSet.lorevkeyoff.lcArhist
  .READONLY = .T.
  .colUMN1.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Tran'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN1.header1.Caption = LANG_Tran_no
  .colUMN1.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_no,loFormSet.GetHeaderText("LANG_Tran_no",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN2.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Store'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN2.header1.Caption = LANG_Check
  .colUMN2.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Check,loFormSet.GetHeaderText("LANG_Check",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN3.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.TranDate'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN3.header1.Caption = LANG_Trn_Date
  .colUMN3.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Trn_Date,loFormSet.GetHeaderText("LANG_Trn_Date",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN4.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Duedate'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN4.header1.Caption = LANG_Due_Date
  .colUMN4.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Due_Date,loFormSet.GetHeaderText("LANG_Due_Date",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN5.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Batch'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN5.header1.Caption = LANG_Batch
  .colUMN5.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Batch,loFormSet.GetHeaderText("LANG_Batch",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN6.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Amount'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN6.header1.Caption = LANG_Amt
  .colUMN6.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Amt,loFormSet.GetHeaderText("LANG_Amt",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .colUMN7.ControlSource = This.loFormSet.lorevkeyoff.lcArhist + '.Desc'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.colUMN7.header1.Caption = LANG_Descr
  .colUMN7.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Descr,loFormSet.GetHeaderText("LANG_Descr",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDWITH


WITH This.loFormSet.ariaForm1.ariapageframe1.pgfOpen.msgOpen.grdMultiSelectionGrid
  .RecordSource = ''
  .RecordSource = This.loFormSet.lorevkeyoff.lcCredit
  .READONLY = .T.
  .colUMN1.ControlSource = 'ThisFormSet.lorevkeyoff.lfgetValueLogic()'
  .coLUMN1.header1.Caption = ""
  .coLUMN1.CurrentControl = 'Ariacheckbox1'
  .colUMN1.READONLY = .F.
  .coLUMN2.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Tran'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN2.header1.Caption = LANG_Tran_no
  .coLUMN2.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tran_no,loFormSet.GetHeaderText("LANG_Tran_no",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .coLUMN3.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Trandate'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN3.header1.Caption = LANG_Date
  .coLUMN3.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Date,loFormSet.GetHeaderText("LANG_Date",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .coLUMN4.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Desc'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN4.header1.Caption = LANG_Descr
  .coLUMN4.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Descr,loFormSet.GetHeaderText("LANG_Descr",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .coLUMN5.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Amount'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN5.header1.Caption = LANG_Amt
  .coLUMN5.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Amt,loFormSet.GetHeaderText("LANG_Amt",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .coLUMN6.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Store'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN6.header1.Caption = LANG_Check
  .coLUMN6.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Check,loFormSet.GetHeaderText("LANG_Check",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .coLUMN7.ControlSource = This.loFormSet.lorevkeyoff.lcCredit + '.Batch'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.coLUMN7.header1.Caption = LANG_Batch
  .coLUMN7.header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Batch,loFormSet.GetHeaderText("LANG_Batch",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDWITH

*!*************************************************************
*! Name      : lfgetValueLogic
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to evaluate the value of the logic field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*!
FUNCTION lfgetValueLogic
  PRIVATE lnRetVal
  lnRetVal = EVAL(This.lcCredit + '.lSelect')
  RETURN lnRetVal
ENDFUNC
*!*************************************************************
*! Name      : lfgetValueLogic
*! Developer : Mariam Mazhar [MMT]
*! Date      : 05/24/2006
*! Purpose   : Function to evaluate the value of the logic field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*!
FUNCTION lfgetValueLogic2
  PRIVATE lnRetVal
  lnRetVal = EVAL(This.lcTmpArhist + '.llSelect')
  RETURN lnRetVal
ENDFUNC

*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]
*!*************************************************************
*! Name      : lfGetNonARCash
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/01/2019
*! Purpose   : Function to get Non-AR Cash record from Credit table
*!*************************************************************
FUNCTION lfGetNonARCash
This.loFormSet.AriaForm1.LockScreen = .T.
IF USED(THIS.lcTmpArhist) AND RECCOUNT(THIS.lcTmpArhist) > 0
  SELECT(THIS.lcTmpArhist)
  ZAP
ENDIF

IF USED(THIS.lcCredit) AND RECCOUNT(THIS.lcCredit) > 0
  SELECT(THIS.lcCredit)
  ZAP
ENDIF

SELECT('Credit')
SCAN FOR  EMPTY(Account) AND Trantype ='4'
  SCATTER MEMO MEMVAR
  INSERT INTO (THIS.lcCredit) FROM MEMVAR
ENDSCAN

SELECT(THIS.lcCredit)
LOCATE
IF !EOF(THIS.lcCredit)
  REPLACE ALL lSelect WITH .F.
  This.llSelectEna  =.T.
  This.llSelectnOneEna =.F.
  This.llSelectAllEna =.T.
  This.llSelectIvertEna =.T.
ELSE
  This.llSelectEna  =.F.
  This.llSelectnOneEna =.F.
  This.llSelectAllEna =.F.
  This.llSelectIvertEna =.F.
ENDIF
LOCATE
This.loFormSet.changemode('E')
This.loFormSet.AriaForm1.LockScreen = .F.
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][End]
*E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
*!*************************************************************
*! Name      : lfGetTranDesc
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/25/2019
*! Purpose   : Function to get Transaction Desc.
*!*************************************************************
FUNCTION lfGetTranDesc
lPARAMETERS _TRANTYP
XTRANDESC = ''
loFormSet = This.loFormSet
LCARIAHFILE = loFormSet.CHeaderAlias
DO CASE
CASE _TRANTYP = 'IN'
  XTRANDESC = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLDIST_INVOICE,loFormSet.GetHeaderText("LANG_GLDIST_INVOICE",LCARIAHFILE)) 
CASE _TRANTYP = 'VI'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_VOIDINVOICE,loFormSet.GETHEADERTEXT("LANG_GLDIST_VOIDINVOICE",LCARIAHFILE))
CASE _TRANTYP = 'CR'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CASHR,loFormSet.GETHEADERTEXT("LANG_GLDIST_CASHR",LCARIAHFILE))
CASE _TRANTYP = 'CA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CREDITADJ,loFormSet.GETHEADERTEXT("LANG_GLDIST_CREDITADJ",LCARIAHFILE))
CASE _TRANTYP = 'DA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_DEBITADJ,loFormSet.GETHEADERTEXT("LANG_GLDIST_DEBITADJ",LCARIAHFILE))
CASE _TRANTYP = 'RM'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_RM,loFormSet.GETHEADERTEXT("LANG_GLDIST_RM",LCARIAHFILE))
CASE _TRANTYP = 'VR'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_VOIDRM,loFormSet.GETHEADERTEXT("LANG_GLDIST_VOIDRM",LCARIAHFILE))
CASE _TRANTYP = 'IP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVPHY,loFormSet.GETHEADERTEXT("LANG_GLDIST_INVPHY",LCARIAHFILE))
CASE _TRANTYP = 'IA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVADJ,loFormSet.GETHEADERTEXT("LANG_GLDIST_INVADJ",LCARIAHFILE))
CASE _TRANTYP = 'MP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATINVPHY,loFormSet.GETHEADERTEXT("LANG_GLDIST_MATINVPHY",LCARIAHFILE))
CASE _TRANTYP = 'MA'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATINVDJ,loFormSet.GETHEADERTEXT("LANG_GLDIST_MATINVDJ",LCARIAHFILE))
CASE _TRANTYP = 'PO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_POREC,loFormSet.GETHEADERTEXT("LANG_GLDIST_POREC",LCARIAHFILE))
CASE _TRANTYP = 'MO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MAPOREC,loFormSet.GETHEADERTEXT("LANG_GLDIST_MAPOREC",LCARIAHFILE))
CASE _TRANTYP = 'CT'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CTREC,loFormSet.GETHEADERTEXT("LANG_GLDIST_CTREC",LCARIAHFILE))
CASE _TRANTYP = 'ZE'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_ZEROOUT,loFormSet.GETHEADERTEXT("LANG_GLDIST_ZEROOUT",LCARIAHFILE))
CASE _TRANTYP = 'NL'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_NONMATLIB,loFormSet.GETHEADERTEXT("LANG_GLDIST_NONMATLIB",LCARIAHFILE))
CASE _TRANTYP = 'JC'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_POJOBCLOSE,loFormSet.GETHEADERTEXT("LANG_GLDIST_POJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'RO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MATOPREC,loFormSet.GETHEADERTEXT("LANG_GLDIST_MATOPREC",LCARIAHFILE))
CASE _TRANTYP = 'RS'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_STYOPREC,loFormSet.GETHEADERTEXT("LANG_GLDIST_STYOPREC",LCARIAHFILE))
CASE _TRANTYP = 'MM'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_RECMFGORDER,loFormSet.GETHEADERTEXT("LANG_GLDIST_RECMFGORDER",LCARIAHFILE))
CASE _TRANTYP = 'EX'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_EXRATEDIFF,loFormSet.GETHEADERTEXT("LANG_GLDIST_EXRATEDIFF",LCARIAHFILE))
CASE _TRANTYP = 'KO'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_KEYOFF,loFormSet.GETHEADERTEXT("LANG_GLDIST_KEYOFF",LCARIAHFILE))
CASE _TRANTYP = 'MC'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_MAJOBCLOSE,loFormSet.GETHEADERTEXT("LANG_GLDIST_MAJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'JP'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_CTJOBCLOSE,loFormSet.GETHEADERTEXT("LANG_GLDIST_CTJOBCLOSE",LCARIAHFILE))
CASE _TRANTYP = 'LK'
  XTRANDESC = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_GLDIST_INVLOCK,loFormSet.GETHEADERTEXT("LANG_GLDIST_INVLOCK",LCARIAHFILE))
ENDCASE
RETURN XTRANDESC 
*E611824,1 MMT 12/25/2019 call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
ENDDEFINE