*:***************************************************************************
*: Program file  : SMREBAL
*: Program desc. : Rebalance Databases
*: For screen    : 
*:        System : Aria 4 XP
*:        Module : System Manager (SM)
*:     Developer : Wael M. Abo-Shawareb (WSH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : lfvCompany,lfvSelAll,lfWOldVal,lfvCheck,lfShowChk,lfRebalanc,
*:                : lfFilesStr,lfOpnFiles,gfModalGen,lfStartReb,lfUpdSty,
*:                : gfTraceKey,lfUpStyDye,lpRepStyDy,lfUpdtLoop,lpGathFlds,
*:                : lfUpdtOper,lpUpDtWork,lpCalGrpDt,lpStyDyeUp
*:***************************************************************************
*: Passed Parameters  : 1- Short cut string for databases to rebalance
*:                    : 2- Company Code (Current if nothing) 
*:***************************************************************************
*: Example : DO SmRebal
*:***************************************************************************
*: Modifications :
*: B127969,1 WSH 05/25/2005 Add GMA Custom trigger to calculate Shipment 
*:                          Amount and Balance to Pay fields.
*: B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance...
*: E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File.
*: B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance.
*: B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse 
*: B130225,1 WSH 10/26/2005 Fix the Bug of variable cOrdType not found in case of rebalancing selected SO No.
*: B130510,1 WSH 12/05/2005 Style browse is slow.
*: B131722,1 WSH 05/02/2006 Fix bug in Style Ordered Quantities.
*: B607832,1 MMT 11/15/2006 bug of not considering all lines in itemjrnl file    T20060824.0019
*: B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit T20061211.0008
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage            T20070117.0001 
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module        T20070213.0006 
*: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program                          T20071019.0005
*: B608487,1 WAM 03/19/2008 Enhance performance of rebalance                     T20080229.0009
*: B608487,1 WAM 03/19/2008 Enhance performance of rebalance                     T20080307.0002
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace 					[T20071129.0005]
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record    [T20080530.0008]
*: B608600,1 MMT 07/01/2008 Fix bug of Reblanacing cancelled Bulk Orders		[T20080305.0010]
*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts 					[T20090106.0043]
*! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[T20090414.0016]
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[T20090224.0001]
*! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[T20090224.0001]
*! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[T20090224.0001]
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[T20090224.0001]
*! E302821,1 TMI 12/27/2010 Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>] [T20101124.0009]
*! B609548,1 MMT 03/07/2011 Rebalance update stock fields of non inventory styles incorrectly[T20110301.0006]
*! B609578,1 WAM 05/04/2011 Update vbendor balance while rebalance AP module when the option AP Balance is selected [T20110124.0004]
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{MEDIA}
*! B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [T20110622.0022]
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*! B609996,1 MMT 07/11/2012 Rebalance program use ncost * ntotstk instead of nstkval  from styinvjl table[T20120316.0004]
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035]
*! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008]
*! B610207,2 MMT 01/29/2013 AP- Vendor Balances incorrect [T20130102.0008]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001]
*! N000682,1 TMI 04/20/2013 fix a problem that the period is returned empty in the rebalance program
*! E303410,1 TMI 08/28/2013 call the rebalance program from the style screen with a stymajor as a parameter , rebalance and refresh [Start] *:***************************************************************************
*! B610535,1 MMT 09/29/2013 Rabalance program gives error becuase of its progress bar[T20130819.0010]
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004]
*! B610750,1 TMI 06/18/2014 1. add trace log to the Rebalance in case of RB,  [T20140530.0001] 
*! B610750,1                2. don't call the ReBalLog.txt in case of RB 
*! B610750,3 TMI 06/23/2014 if the output file handle is invalid then skip the closing loop using fclose 
*! B610814,1 TMI 08/20/2014 add styles with high cost to error log [T20140715.0001 ] 
*! B610821,1 TMI 08/25/2014 Modify function lfUpdItem2 to replace the loop that blank and delete vendor record in apvenhst with command BLANK FOR cVendCode + cFisFYear = APVENDOR.cVendCode [T20140703.0002]
*! B610973,1 MMT 03/26/2015 Fix issues in the Rebalance program[T20150318.0075]
*:***************************************************************************

*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
*PARAMETERS lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal
*E303410,1 TMI 08/29/2013 [Start] add a new parameter for style major before request builder parameters
*PARAMETERS lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal,lcRequestID, lcXMLFileName, CLIENTID
PARAMETERS lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal,lcStyMaj,lcRequestID, lcXMLFileName, CLIENTID
*E303410,1 TMI 08/29/2013 [End  ] 
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

#INCLUDE R:\ARIA4XP\PRGS\SM\SMREBAL.H

*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') = 'C'

*B610750,1  test code TMI 06/18/2014 14:58 [Start] 
DECLARE INTEGER GetCurrentProcessId IN kernel32
lnProcID = GetCurrentProcessId()
lcProcID = ALLTRIM(STR(lnProcID))
*B610750,1  test code TMI 06/18/2014 14:58 [End  ] 

  PRIVATE loAgent
	PRIVATE loProgress
	loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")   
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
	LOCAL loEnvironment
	loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
	loEnvironment.ClientID = ClientID
	loEnvironment.ConnectionsRefresh()
	LOCAL lcCurrentProcedure
	lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
	lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1) 
	DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID),ClientID

*B610750,1  test code TMI 06/18/2014 14:58 [End  ] 	
=lfSTRTOFILE("starting the rebalance program")
*B610750,1  test code TMI 06/18/2014 14:58 [End  ] 
	
  IF !USED('SYUUSER')
    =gfOpenFile(gcsyshome + "SYCCOMP" , "CCOMP_ID" , 'SH')
  ENDIF	
  oAriaApplication.syspath =oAriaApplication.systemfilespath
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

*-- Rebalance log file mus reflect any error occuring while process 
*-- any different between master files and temporary files.
*-- Verify Variables
laRebMsg  = ''     && Array holds the verify rebalance log message
lcWinTitl = LANG_SMREBAL_REBLOG

*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
IF TYPE('lcXMLFileName') = 'C'
  loRequestObj = loAgent.GetRequest(lcRequestID, ClientID)
  lcOutPutFile = loRequestObj.CompleteNotification.ConvertAttachmentToString()
  *B610750,1  test code TMI 06/18/2014 14:46 [Start] 
  lcMsg = 'lcRequestID:'+lcRequestID+' ,ClientID:'+ClientID
  =lfSTRTOFILE(lcMsg)
  *B610750,1  test code TMI 06/18/2014 14:46 [End ] 
ENDIF
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]

*-- Modules to convert in Aria4.

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO'

*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO,MF'

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO,MF,RM'
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO,MF,RM,AL'
lcAria4Modules = 'AR,IC,MA,PO,SO,MF,RM,AL,AP'
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[End]
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

IF !llAutoBal
  llOpenRep  = .F.
  lcFilHandl = ''
ELSE
  =FPUTS(lcFilHandl, LANG_SMREBAL_REB)
  =FPUTS(lcFilHandl, LANG_SMREBAL_PROCESS + DTOC(DATE()) + SPACE(5) + TIME())
ENDIF
llSaveAut  = llOgForAut

laTempName = ""    && Array holds the temporar yfile name
lcInfoHdr  = ""    && Variable holds header temporary file name
lcInfoDtl  = ""    && Variable holds detail temporary file name

*-- AP variables
lnInvAmt   = 0                  && Varible to hold the invoice amount
lnDiscTakn = 0                  && Varible to hold the total discount taken per invoice
lnDiscOffr = 0                  && Varible to hold the discount offeramount per invoice
lnAdjtAmt  = 0                  && Varible to hold the total adjustment amount per invoice
lnAmtPaid  = 0                  && Varible to hold the total paid amount per invoice
lnPurchAmt = 0                  && Varible to hold the purchase amount per invoice
lcInvNo    = ""                 && Varible to hold the A/P invoice no.

*-- GL variables
STORE '' TO lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt     
llBalnBfor = .F.         && Flag to know if the Rebalance process was done befor in another session without Updating the master file
lnPostTran = 0           && Varible to hold the number of transactions that was Reposted in the Rebalance process
lcRpFiscYr = ""          && Variable holds the fiscal year if GL module was selected
lcFiscYear = ""          && Variable holds the fiscal year if GL module was selected
lcCurr_yer = ""
lnCurr_yer = 0
lcAcsMask  = ""
lnAcsSegSz = 0 
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  lcRpCmpExp = ''
  lcRpModExp = ''
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]


lcCurrComp_ID = ''

lcFilePath  = oAriaApplication.DataDir
lcKeyVal    = ''            && Variable to hold the key that we seek with in the detail file while rebalance header&detail (ex. Invoice#)
laOldVal    = ''            && Varible to hold the old value in the option grid.
lcKeyType   = ''            && Variable to hold the order type.
llAdoDye    = .F.
llViewLog   = .F.

*B127969,1 WSH 05/25/2005 Add variable to not check Shipment Amount if the GMA Trigger exests. [Start]
llChkInvAmt = .T.
*B127969,1 WSH 05/25/2005 [End]

DIMENSION laFChck [2,6]  && Array to hold the fields that we will check while rebalancing header&detail.

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON
STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

IF !llAutoBal
  DIMENSION laFXFLT [1,1]  && Array to hold the fix filter comming from the option grid.
ENDIF

DECLARE laRpCmpCod[1,3]  && Array to hold the companies information that will be rebalance.
DECLARE laRpCodMod[1,3]

*-- Rebalance Options and Functions Tables...
lcRebalHdr = gfTempName()
lcRebalDtl = gfTempName()

*-- Global variables to hold Remote Tables Object References and Cursors...
loVendor   = .F.
loWareHous = .F.
loStyle_X  = .F.
loCustomer = .F.
lcVendor   = gfTempName()
lcWareHous = gfTempName()
lcStyle_X  = gfTempName()
lcCustomer = gfTempName()
lcSYCCOMP  = gfTempName()

*-- Global Cursors hold Selected Filter Values
lcSelInvNo   = gfTempName()
lcSelOrdNo   = gfTempName()
lcSelPONo    = gfTempName()
lcSelAccount = gfTempName()
lcSelStyle   = gfTempName()

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
lcSelFabric  = gfTempName()
*B608487,1 WAM 03/19/2008 (End)
lcSelYear    = gfTempName()

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
lcSelCTNo    = gfTempName()
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

*-- TempCursors used for verifying data in Header files...
lcSPO    = gfTempName()
lcSoOrd  = gfTempName()
lcArInv  = gfTempName()
lcMfCt   = gfTempName()

*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[Start]
lcAPRBALV =  gfTempName()
lcAPRBALVH=  gfTempName()
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[End]

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
lcPackSelect   = gfTempName()
lcALPack       = gfTempName()
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*-- Open Company System File...
lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM SYCCOMP", '', lcSYCCOMP, "", oAriaApplication.SystemConnectionString, 3, "", 1)
IF lnRemResult <> 1
  RETURN .F.
ENDIF

*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *-- Initialize the progress bar.
  
  *E303410,1 TMI 09/01/2013 [Start] 
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 
  
  oProgress = NEWOBJECT('AriaProgressBar', oAriaApplication.classdir + 'Utility.VCX')
    
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF 
  *E303410,1 TMI 09/01/2013 [End  ] 
  
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
STORE '' TO lcComps, lcModls

lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET FULLPATH ON

*--Get Data Session number for the program as OG deeals with its own Data Session
lnPrgSession = SET("Datasession")

*-- Build tempoaray files and gathering all info. into this temp.
=lfInfoHdr()


lcAutoBal = ''
llExpr1   = .F.
llUpdt    = .F.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
PRIVATE lcSQLConStr, lnCompConnHand
*B128464,2 WSH 08/21/2005 [End]

IF llAutoBal
  lcAutoBal = lcToReb
  lcToReb   = ''
ENDIF

IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
  lcCurrComp_ID = lcCompCode
  = lpMainReb(lcToReb, '', 'U')
ELSE
  llNComp = .F.
  
  *--If we want only to specify databases to rebalance from Auto balance then only show OG.
  llExpr1 = '.F.'
  IF llOgForAut
    llClrReadN = .T.
    llExpr1 = gfOpGrid('SMREBAL' , .T.)  && Run selection grid.
    RETURN
  ENDIF
  
  IF !llAutoBal
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') <> 'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      
      *E303410,1 TMI 08/29/2013 [Start] if style major sent then create your my own criteria
      IF TYPE('lcStyMaj')='C' AND !EMPTY(lcStyMaj)
        =lfCrtStyCriteria(lcStyMaj)
      ELSE 
        *E303410,1 TMI 08/29/2013 [Start]      
             
      llExpr1 = gfOpGrid('SMREBAL' , .T.)  && Run selection grid.
      
        *E303410,1 TMI 08/29/2013 [Start] 
      ENDIF 
      *E303410,1 TMI 08/29/2013 [End  ] 
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      
  ELSE
    llExpr1 = lfUpBalance()
  ENDIF  
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF  TYPE('lcXMLFileName') = 'C'
		oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

    lcRpCmpExp = lcRpCmpEx1
    lcRpModExp = lcRpModEx1


		FOR lnI = 1 TO ALEN(laogfxflt,1)
		  IF !EMPTY(laogfxflt[lnI ,6]) AND USED(laogfxflt[lnI ,6])
    		DO CASE 
		      CASE ALLTRIM(UPPER(laogfxflt[lnI ,1])) = UPPER('lcInvN')
    		    =lfCreateCursor(1, 'lcInvN', @lcSelInvNo, 'Invoice C(6)', 'Invoice', 'Invoice')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcOrdN")
      		  =lfCreateCursor(1, 'lcOrdN', @lcSelOrdNo, 'Order C(6)', 'Order', 'Order')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER('lcPON')
      		  =lfCreatePOCursor(1, 'lcPON', @lcSelPONo)
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcAccount")
      		  =lfCreateCursor(1, 'lcAccount', @lcSelAccount, 'Account C(5)', 'Account', 'Account')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcFisYear")
        		=lfCreateCursor(1, 'lcFisYear', @lcSelYear, 'cFisfYear C(4)', 'cFisfYear', 'cFisfYear')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcAllStyle")
        		=lfCreateCursor(1, 'lcAllStyle', @lcSelStyle, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcAllFabric")
      		  =lfCreateCursor(1, 'lcAllFabric', @lcSelFabric, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcMPON")
        		=lfCreatePOCursor(1, 'lcMPON', @lcSelPONo)
          CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcCTN")
  	  		  =lfCreatePOCursor(1, 'lcCTN', @lcSelCTNo)
  	  	 CASE UPPER(laogfxflt[lnI ,1]) = UPPER("lcPackNo")  
	   	  	 =lfCreateCursor(1, 'lcPackNo', @lcSelOrdNo, 'PACK_NO C(6)', 'PACK_NO', 'PACK_NO')
	    ENDCASE 
  	ENDIF   
	ENDFOR 
  SELECT (lcRebalHdr)
  SET ORDER TO ITEMHDR   && CITEMNAME 
  SEEK("LCRPITEM")
  SCAN
	  IF TYPE(&lcRebalHdr..cItemName)<> 'U'
	    LCFLDNAME = "m."+ &lcRebalHdr..cItemName
	    REPLACE &lcRebalHdr..cUpdVryIgn WITH EVALUATE(LCFLDNAME) 
	  ENDIF 
  ENDSCAN
  DECLARE laRpCmpCod1[1] 
  =gfSubStr(lcRpCmpExp,@laRpCmpCod1, ',')
  LOCAL lnSelected 
  lnSelected = SELECT()
  SELECT(lcSYCCOMP)
  DECLARE laRpCmpCod [ALEN(laRpCmpCod1, 1) ,3]
  LOCAL lnIndex
  FOR lnIndex = 1 TO ALEN(laRpCmpCod1, 1)
    LOCATE FOR &lcSYCCOMP..CCOMP_ID = laRpCmpCod1[lnIndex] 
    laRpCmpCod [lnIndex,1]= &lcSYCCOMP..ccomp_id + " - "+ &lcSYCCOMP..cCom_Name          
    laRpCmpCod [lnIndex,2]= &lcSYCCOMP..cCom_dDir
    laRpCmpCod [lnIndex,3]= &lcSYCCOMP..mModlSet
  ENDFOR
  SELECT(lnSelected)
	=gfSubStr(lcRpModExp,@laRpCodMod, ',')
ENDIF 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  *IF !EMPTY(llExpr1) AND IIF(TYPE('llExpr1') = 'C', llExpr1 # '.F.', .T.)
  IF IIF(TYPE('lcXMLFileName') = 'C',.T.,!EMPTY(llExpr1) AND IIF(TYPE('llExpr1') = 'C', llExpr1 # '.F.', .T.))
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    LOCAL lcObjName, lcCurOption
    lcStyOptions = ',5,6,7,8,9,10,11,12,13,'
    
    *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
    *lcMatOptions = ',3,4,'
    lcMatOptions = ',3,4,21,22,'
    *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
    
    *B127969,1 WSH 05/25/2005 Check if the custom that calculate Shipment Amount. [Start]
    LOCAL lcSycTrigg
    lcSycTrigg = gfTempName()
    
    lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT cAPObjNam FROM SYCTRIGG WHERE cAPObjNam = 'SMREBAL ' AND cEvent_ID = 'INVREBAL '", '', lcSycTrigg, "", oAriaApplication.SystemConnectionString, 3, '', SET("DATASESSION"))
    IF lnRemResult >= 1
      SELECT (lcSycTrigg)
      llChkInvAmt = EOF()
      USE IN (lcSycTrigg)
      
    ENDIF
    *B127969,1 WSH 05/25/2005 [End]
    
    *-- Select Header file
    SELECT (lcRebalHdr)
    llViewLog = SEEK('LCLOGFILE') AND cLogFile = 'Y'
    LOCATE
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') = 'C'
      lcOldCompany = oAriaEnvironment.ActiveCompanyID
    ENDIF  
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
    FOR lnAllComp = 1 TO ALEN(laRpCmpCod,1)
      DIMENSION laFileName [1,2]      
      laFileName    = ''
      lcCurrComp_ID = PADR(laRpCmpCod[lnAllComp,1],2)
      
      IF lcCurrComp_ID $ lcRpCmpExp
        lcFilePath = ALLTRIM(LOWER(laRpCmpCod[lnAllComp,2]))
        lcCompName = ALLTRIM(SUBSTR(laRpCmpCod[lnAllComp,1],6))
        lcCompMods = laRpCmpCod[lnAllComp,3]
        
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
        IF TYPE('lcXMLFileName') = 'C'
          oAriaEnvironment.ActiveCompanyID = lcCurrComp_ID
          oAriaEnvironment.GetCompanyInformation(lcCurrComp_ID)
          SQLSETPROP(0,"DispLogin",3)
          lcSQLConStr = oAriaApplication.readconnectionstring(lcCurrComp_ID) 
          lnCompConnHand = SQLSTRINGCONNECT(lcSQLConStr)
          IF lnCompConnHand < 1
            LOOP
           ENDIF
          
        ELSE  
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          lcSQLConStr = oAriaApplication.mReadConStr(lcCurrComp_ID)
          lnCompConnHand = SQLSTRINGCONNECT(lcSQLConStr)
          IF lnCompConnHand < 1
            LOOP
          ENDIF
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
        ENDIF
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
        *B128464,2 WSH 08/21/2005 [End]
        *B610750,1  test code TMI 06/18/2014 14:32 [Start] 
        =lfSTRTOFILE("=lfAllStyOptions()")
        *B610750,1  test code TMI 06/18/2014 14:32 [End  ] 
        =lfAllStyOptions()
        *B610750,1  test code TMI 06/18/2014 14:32 [Start] 
        =lfSTRTOFILE(MESSAGE())
        *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
        =lfAllMatOptions()
        
        SELECT (lcRebalHdr)
        LOCATE
        *! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
        *SCAN FOR cUpdVryIgn $ 'UV'
        SCAN FOR cUpdVryIgn $ 'UV' AND (cModule $ lcCompMods OR cModule2 $ lcCompMods)
        *! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
          *-- Get item name
          lcItemId    = UPPER(&lcRebalHdr..cItemName)
          lcCurOption = ',' + ALLTRIM(STRTRAN(lcItemId, 'LCRPITEM')) + ','
          lcInvType   = EVALUATE(lcRebalHdr + '.cInvType')
          
          IF lcCurOption $ lcStyOptions OR lcCurOption $ lcMatOptions
            LOOP
          ENDIF
          
          *-- Define Table Objects as Global to use them
          SELECT (lcRebalDtl)
          =SEEK(UPPER(lcItemId))
          SCAN REST WHILE cItemName = UPPER(lcItemId)
            lcObjName  = cFileObj
            &lcObjName = ''
          ENDSCAN
          SELECT (lcRebalHdr)
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          DIMENSION laFileStr[1,9]
          laFileStr = .F.
          *B128464,2 WSH 08/21/2005 [End]
          
          *--If there is no error occured while opening file, continue 
          IF lfOpnFiles(lcItemId) 
          
            *--Get current record no.
            lnCurNo = RECNO(lcRebalHdr) 
            
            *--Check mandatory data.
            IF !EMPTY(ALLTRIM(&lcRebalHdr..cValidFun))
              lcChkMntry = .T.
              lcDoProg = ALLTRIM(&lcRebalHdr..cValidFun)
              IF &lcDoProg()
              
                 *--Call any additional setup program.
                IF !EMPTY(ALLTRIM(&lcRebalHdr..cAddSetup))
                  lcDoProg = ALLTRIM(&lcRebalHdr..cAddSetup)
                  DO &lcDoProg
                ENDIF      
                
                *--Call Verify or Update program.
                lcUpVrProg = ALLTRIM(IIF(cUpdVryIgn='U', &lcRebalHdr..cUpdFunNam ,&lcRebalHdr..cVryFunNam))
                IF !EMPTY(lcDoProg)
                  DO &lcUpVrProg 
                ENDIF
              ENDIF   && End of   IF &lcDoProg() 
            ENDIF     && !EMPTY(ALLTRIM(lcRebalHdr.cValidFun)) 
            
            *--Close all files that were open to complete process of current item.
            =lfClosFile(lcItemId)
            
            IF BETWEEN( lnCurNo,1,RECCOUNT(lcRebalHdr))
              SELECT (lcRebalHdr)
              GO lnCurNo 
            ELSE
              EXIT  
            ENDIF
          ENDIF 
        ENDSCAN
        
        *B127969,1 WSH 05/25/2005 Add the trigger to calculate Invoice Shipment and Balance to pay fields for GMA. [Start]
        =gfDoTriger('SMREBAL',PADR('INVREBAL',10))
        *B127969,1 WSH 05/25/2005 [End]
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
	    IF TYPE('lcXMLFileName') <> 'C'
     	*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
          =SQLDISCONNECT(lnCompConnHand)
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
        ENDIF
        *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
      ENDIF
    ENDFOR
    
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') = 'C'
      oAriaEnvironment.ActiveCompanyID = lcOldCompany
      oAriaEnvironment.GetCompanyInformation(lcOldCompany)
    ENDIF  
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    
    *--Call rebalance Log screen.
    IF !llAutoBal
     =lfViewRLog()
    ENDIF
    
    *--Close info. temp. files.
    =lfErasTmp()  
  ENDIF
ENDIF

IF TYPE('loWareHous') = 'O'
  loWareHous = .NULL.
ENDIF

IF TYPE('loVendor') = 'O'
  loVendor = .NULL.
ENDIF

IF TYPE('loStyle_X') = 'O'
  loStyle_X = .NULL.
ENDIF

IF TYPE('loCustomer') = 'O'
  loCustomer = .NULL.
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <>'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  oProgress = .NULL.
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

*-- Reset the FULLPATH setting.
SET FULLPATH &lcPathStat
RETURN

*!*************************************************************
*! Name      : lfAllStyOptions
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Update all style options...
*!*************************************************************
*! Example   : = lfAllStyOptions()
*!*************************************************************
FUNCTION lfAllStyOptions

LOCAL lcCurOption
lcInvType = '0001'

STORE 0 TO lnOnOrder, lnWip, lnWo, lnIntrans, lnOrd, lnAlo, lnShp, lnRet, lnRa, lnStock, lnOnHand
STORE 'I' TO lcVrUpStk,  lcVrUpTrn, lcVrUpWo, lcVrUpWip, lcVrUpShp, lcVrUpAlo, lcVrUpOrd, lcVrUpRa, lcVrUpRet, lcVrUpMO, lcVrUpMH


*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
STORE 0 TO lnMIntrans , lnMatUsg
STORE 'I' TO lcVrUpTrnM,lcVrUpUsage
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
STORE 0 TO lnPackReb
STORE 'I' TO lcVrUpPack
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]



SELECT (lcRebalHdr)
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
*SCAN FOR cUpdVryIgn $ 'UV'
SCAN FOR cUpdVryIgn $ 'UV' AND (cModule $ lcCompMods OR cModule2 $ lcCompMods)
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
  *-- Get item name
  lcItemId  = UPPER(&lcRebalHdr..cItemName)
  lcRebItem = ALLTRIM(STRTRAN(lcItemId, 'LCRPITEM'))
  
  IF !((',' + lcRebItem + ',') $ lcStyOptions)
    LOOP
  ENDIF

  DO CASE 
    CASE lcRebItem = '5'
      lnStock   = 1
      lcVrUpStk = cUpdVryIgn
    CASE lcRebItem = '6'
      lnIntrans = 1
      lcVrUpTrn = cUpdVryIgn
    CASE lcRebItem = '7'
      lnWo     = 1
      lcVrUpWo = cUpdVryIgn
    CASE lcRebItem = '8'
      lnWip     = 1
      lcVrUpWip = cUpdVryIgn
    CASE lcRebItem = '9'
      lnShp     = 1
      lcVrUpShp = cUpdVryIgn
    CASE lcRebItem = '10'
      lnAlo     = 1
      lcVrUpAlo = cUpdVryIgn
    CASE lcRebItem = '11'
      lnOrd     = 1
      lcVrUpOrd = cUpdVryIgn
    CASE lcRebItem = '12'
      lnRa      = 1
      lcVrUpRa = cUpdVryIgn
    CASE lcRebItem = '13'
      lnRet    = 1
      lcVrUpRet = cUpdVryIgn
      
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
    CASE lcRebItem = '23'
      lnPackReb = 1
	  lcVrUpPack = cUpdVryIgn
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End] 
    
  ENDCASE
ENDSCAN

*-- Call the old program with some modifications to mainipulate the verify option befroe updating.

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*IF lnWip + lnWo + lnIntrans + lnOrd + lnAlo + lnShp + lnRet + lnRa + lnStock > 0
IF lnWip + lnWo + lnIntrans + lnOrd + lnAlo + lnShp + lnRet + lnRa + lnStock + lnPackReb > 0
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]  

    *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
    =lfSTRTOFILE("DO lpMainReb WITH lcToReb, .F., .F., lcFilePath, lcCompName, lcCompMods")
    *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
  DO lpMainReb WITH lcToReb, .F., .F., lcFilePath, lcCompName, lcCompMods
ENDIF

*!*************************************************************
*! Name      : lfAllMatOptions
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Update all style options...
*!*************************************************************
*! Example   : = lfAllMatOptions()
*!*************************************************************
FUNCTION lfAllMatOptions

LOCAL lcCurOption
lcInvType = '0002'

STORE 0 TO lnOnOrder, lnWip, lnWo, lnIntrans, lnOrd, lnAlo, lnShp, lnRet, lnRa, lnStock, lnOnHand
STORE 'I' TO lcVrUpStk, lcVrUpTrn, lcVrUpWo, lcVrUpWip, lcVrUpShp, lcVrUpAlo, lcVrUpOrd, lcVrUpRa, lcVrUpRet, lcVrUpMO, lcVrUpMH

*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
STORE 0 TO lnMIntrans , lnMatUsg
STORE 'I' TO lcVrUpTrnM,lcVrUpUsage
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

SELECT (lcRebalHdr)
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
*SCAN FOR cUpdVryIgn $ 'UV'
SCAN FOR cUpdVryIgn $ 'UV' AND (cModule $ lcCompMods OR cModule2 $ lcCompMods)
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
  *-- Get item name
  lcItemId  = UPPER(&lcRebalHdr..cItemName)
  lcRebItem = ALLTRIM(STRTRAN(lcItemId, 'LCRPITEM'))
  
  IF !((',' + lcRebItem + ',') $ lcMatOptions)
    LOOP
  ENDIF
  
  DO CASE 
    CASE lcRebItem = '3'
      lnOnHand = 1
      lcVrUpMH = cUpdVryIgn
    CASE lcRebItem = '4'
      lnOnOrder = 1
      lcVrUpMO  = cUpdVryIgn
     
    *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
    CASE lcRebItem = '21'
      lnMatUsg = 1
      lcVrUpUsage = cUpdVryIgn
    
    CASE lcRebItem = '22'
      lnMIntrans = 1    
      lcVrUpTrnM = cUpdVryIgn
    *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
     
  ENDCASE
ENDSCAN

*-- Call the old program with some modifications to mainipulate the verify option befroe updating.

*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
*IF lnOnHand + lnOnOrder > 0
IF lnOnHand + lnOnOrder + lnMatUsg + lnMIntrans > 0
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

  DO lpMainReb WITH lcToReb, lcCurOption, .F., lcFilePath, lcCompName, lcCompMods
ENDIF

*!*************************************************************
*! Name      : lfClosFile
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Close all open files 
*!*************************************************************
*! Example   : = lfClosFile()
*!*************************************************************
FUNCTION lfClosFile
PARAMETER lcItemId

LOCAL lcFileName, lcFileObj, lnAlias
lnAlias = SELECT(0)

*-- Convert it to upper case
lcItemId = UPPER(lcItemId)

*-- First close all master files belong to item (lcItemId). 
SELECT (lcRebalDtl)
=SEEK(lcItemId)
SCAN REST WHILE cItemName = lcItemId  
  *-- Assign file name , Order name to variables
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *lcFileName = ALLTRIM(&lcRebalDtl..cFileName)
  *lcFileObj  = &lcRebalDtl..cFileObj
  *&lcFileObj = .NULL.
  lcFileName = ALLTRIM(&lcRebalDtl..cAliasName)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <>'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    USE IN (lcFileName)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    =gfCloseTable(lcFileName)
	ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *B128464,2 WSH 08/21/2005 [End]
  
ENDSCAN

*-- Second close & delete all temporary files belong to item (lcItemId).
IF !EMPTY(laTempName)
  lnTempName = ALEN(laTempName,1)
  FOR lnCount = 1  TO lnTempName 
    IF !EMPTY(laTempName[lnCount,1]) AND ;
      laTempName[lnCount,1] <> lcInfoHdr AND ;
      laTempName[lnCount,1] <> lcInfoDtl  

      IF FILE(oAriaApplication.WorkDir + laTempName[lnCount,1] + '.DBF')
        IF USED(laTempName[lnCount,1])
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
          IF TYPE('lcXMLFileName') <>'C'
	      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
            USE IN (laTempName[lnCount,1])
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
          ELSE
            =gfCloseTable(laTempName[lnCount,1])
	      ENDIF
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
        ENDIF
        ERASE (oAriaApplication.WorkDir + laTempName[lnCount,1] + '.DBF')
      ENDIF

      IF FILE(oAriaApplication.WorkDir + laTempName[lnCount,1] + '.CDX')
        IF USED(laTempName[lnCount,1])
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
	      IF TYPE('lcXMLFileName') <>'C'
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
            USE IN (laTempName[lnCount,1])
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
          ELSE
            =gfCloseTable(laTempName[lnCount,1])
	      ENDIF
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
        ENDIF
        ERASE (oAriaApplication.WorkDir + laTempName[lnCount,1] + '.CDX')
      ENDIF
    ENDIF
  ENDFOR
ENDIF  

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : When function of Option Grid
*!*************************************************************
*! Example   : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

=lfGetVarVl()
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfFillRepVars
*! Developer : Wael M. Abo-Shawareb
*! Date      : 03/30/2005
*! Purpose   : Fill Variables and Create Temp cursors needed in the Program
*!*************************************************************
*! Example   : = lfFillRepVars()
*!*************************************************************
FUNCTION lfFillRepVars

IF EMPTY(laRpCmpCod[1,1]) OR EMPTY(laRpSorCmp[1,1])
  LOCAL lnDataSess
  lnDataSess = SET("Datasession")
  SET DATASESSION TO (lnPrgSession)
  
  *-- Define companies array that be used in company mover
  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp
  
  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM (lcSYCCOMP)                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR
  
  =lfCmpExpr()
  =lfvDfinMod()
  =lfvUVIAll()
  
  SET DATASESSION TO (lnDataSess)
ENDIF

*-- Open Remote Tables needed by the Option Grid...
=lfOpenRemote()

*!*************************************************************
*! Name      : lfvCompany
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Call Companies mover function 
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
FUNCTION lfvCompany

= gfMover(@laRpSorCmp, @laRpTarCmp, 'Select Company', .T., '')  && call mover function.
= lfCmpExpr()
loOgScroll.RefreshScroll()
*-- end of lfvCompany.

*!***************************************************************************
*! Name      : lfCmpExpr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
FUNCTION lfCmpExpr

PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF

= ASORT(laTarget)

lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
lcRpCmpEx1 = lcRpCmpExp
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
*B127969,1 WSH 05/25/2005 Hide Fitlers if the selected company is not the Active Company or no Active company. [Start]
*!*  IF LEN(lcRpCmpExp) > 2
*!*    llNComp = .T.
*!*  ELSE
*!*    llNComp = .F.
*!*  ENDIF
llNComp = LEN(lcRpCmpExp) > 2 OR EMPTY(oAriaApplication.ActiveCompanyID) OR !(oAriaApplication.ActiveCompanyID $ lcRpCmpExp)
*B127969,1 WSH 05/25/2005 [End]

*-- end of lfCmpExpr.

*!***************************************************************************
*! Name      : lfModExpr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Evaluate Category expression.
*!***************************************************************************
*! Example   : = lfModExpr()
*!***************************************************************************
FUNCTION lfModExpr

PRIVATE laTarget

IF EMPTY(laRpTarMod)
  =ACOPY(laRpSorMod,laTarget)
ELSE
  =ACOPY(laRpTarMod,laTarget)
ENDIF

=ASORT(laTarget)
lcRpModExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('llCallFromRequestHandler') ="L" AND llCallFromRequestHandler
    POS = ASCAN(laRpCodMod,laTarget[lnI])
    POS = POS - 1
    lcRpModExp = IIF(EMPTY(lcRpModExp),laRpCodMod[POS],lcRpModExp + ','+laRpCodMod[POS])   
  ELSE 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    lcRpModExp = IIF(EMPTY(lcRpModExp),PADR(laTarget[lnI],2),;
                    lcRpModExp + ','+PADR(laTarget[lnI],2))
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
ENDFOR
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
lcRpModex1 = lcRpModExp
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
*-- end of lfModExpr.

*!*************************************************************
*! Name      : lfvModule
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/23/1999
*! Purpose   : - Call Categories mover function 
*!*************************************************************
*! Example   : = lfvModule()
*!*************************************************************
FUNCTION lfvModule

= gfMover(@laRpSorMod,@laRpTarMod,'Select Module',.T.,'')  && call mover function.

=lfModExpr()
loOgScroll.RefreshScroll()

*-- end of lfvModule.

*!*************************************************************
*! Name      : lfvDclTarg
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Declare target array to avoid error message
*!             'lcRpTarCat' not an array' 
*!*************************************************************
*! Example   : = lfvDclTarg()
*!*************************************************************
FUNCTION lfvDclTarg

DECLARE laRpTarMod[1] 
*-- end of lfvDclTarg

*!*************************************************************
*! Name      : lfvGetModl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 03/31/2005
*! Purpose   : Check and assign the active module to module array
*!*************************************************************
*! Example   : =lfvGetModl()
*!*************************************************************
FUNCTION lfvGetModl

LOCAL lnDataSess, lnRemResult, lcSYDAPPL, lcModId, lnModlNo, lcAreaNo
lnDataSess = SET("Datasession")
lcAreaNo   = SELECT(0)

SET DATASESSION TO (lnPrgSession)

lcSYDAPPL  = gfTempName()

DECLARE laRpCodMod[1,2]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
*lnRemResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYDAPPL ORDER BY CAPP_ID", '', lcSYDAPPL, "", oAriaApplication.SystemConnectionString, 3, "", 1)
lnRemResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYDAPPL ORDER BY CAPP_ID", '', lcSYDAPPL, "", oAriaApplication.SystemConnectionString, 3, "", SET("Datasession"))
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
IF lnRemResult <> 1
  SET DATASESSION TO (lnDataSess)
  RETURN .F.
ENDIF  

SELECT (lcSYCCOMP)
LOCATE

SCAN WHILE !EOF()
  *-- Get & assign all modules to string varibale
  lcComp_mdl = ALLTRIM(&lcSYCCOMP..mComp_mdl)
  
  *-- If no module has been defined in this record skip to next record
  IF EMPTY(ALLTRIM(lcComp_mdl))
    SKIP
    LOOP
  ENDIF
  
  *-- Variable used as a counter to redclare the module array.
  lnModlNo = 0
  
  DO WHILE !EMPTY(lcComp_mdl)

    *-- Get position of '|' character
    lnPosition = ATC('|', lcComp_mdl)

    *-- Accumulate no. of modules that found in SYCCOMP
    lnModlNo = lnModlNo + 1
    IF lnModlNo  = 1 
      *-- Split module id from whole module string
      lcModId = SUBSTR(lcComp_mdl, 1, 2)

      *-- Remove module ID from modules string       
      lcComp_mdl = SUBSTR(lcComp_mdl, 3)
    ELSE
      *-- Split module id from whole module string     
      lcModId = SUBSTR(lcComp_mdl, 2, 2)

      *-- Remove module ID from modules string
      lcComp_mdl = SUBSTR(lcComp_mdl, 4)
    ENDIF

    *--Check if Module exisit in lcAria4Modules
    IF !lcModId $ lcAria4Modules
      LOOP
    ENDIF
    
    *-- Check duplicated module id , If this module id was inserted before in 
    *-- module array, skip it.
    IF ASCAN(laRpCodMod, lcModId) = 0 AND !(lcModId $ 'SUEBPSRGSP')
      *-- Get module name , then svae module id & module name.  
      SELECT (lcSYDAPPL)
      LOCATE FOR cApp_Id = lcModId

      IF FOUND()
        *-- Declare module code array
        IF EMPTY(laRpCodMod)
           DECLARE laRpCodMod[1,2]
        ELSE
           DECLARE laRpCodMod[ALEN(laRpCodMod,1)+1,2]
        ENDIF

        laRpCodMod[ALEN(laRpCodMod,1),1] = lcModId
        laRpCodMod[ALEN(laRpCodMod,1),2] = ALLTRIM(&lcSYDAPPL..cApp_Name)
      ENDIF
    ENDIF
  ENDDO
ENDSCAN

IF !EMPTY(laRpCodMod)
  =ASORT(laRpCodMod,2)
ENDIF

USE IN (lcSYDAPPL)

SET DATASESSION TO (lnDataSess)
SELECT (lcAreaNo)  
*-- end of lfvGetModl

*!*************************************************************
*! Name      : lfvChkModul
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Appear or disappear each item in OG
*!*************************************************************
*! Example   : =lfvChkModul()
*!*************************************************************
FUNCTION lfvChkModul
LPARAMETER lcChkModul, lcFldName


IF EMPTY(lcChkModul)
  RETURN .T.
ENDIF

*B127969,1 WSH 05/25/2005 Reassign the llNComp variable. [Start]
llNComp = LEN(lcRpCmpExp) > 2 OR EMPTY(oAriaApplication.ActiveCompanyID) OR !(oAriaApplication.ActiveCompanyID $ lcRpCmpExp)
*B127969,1 WSH 05/25/2005 [End]

LOCAL lnDataSess, lnAlias
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")

SET DATASESSION TO (lnPrgSession)
SELECT (lcRebalHdr)

*-- If module array not defined , build it first
=lfvDfinMod()

*-- Variable to hold module array length
PRIVATE lnModLen 
lnModLen = ALEN(laRpCodMod,1)

*-- Variable to hold Module name 
PRIVATE lcModlName 
lcModlName = ''

*-- Get module name from module array to compare it with module target array
FOR lnI = 1  TO  lnModLen
  IF lcChkModul = laRpCodMod[lnI, 1]
    lcModlName  = laRpCodMod[lnI, 2]
    EXIT
  ENDIF
ENDFOR

*-- If module name is empty disable option grid 
IF EMPTY (lcModlName)
  IF SEEK(UPPER(lcFldName), lcRebalHdr)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH 'I'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('llCallFromRequestHandler') = "L" AND llCallFromRequestHandler
		  lcVarToUpd = UPPER(lcFldName)
      &lcVarToUpd = 'I'      
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    IF llSaveAut
      lcItemV    = UPPER(lcFldName)
      lcOldValue = EVALUATE(lcItemV)
      &lcItemV   = 'I'
    ENDIF
  ENDIF
  
  SET DATASESSION TO (lnDataSess)
  SELECT (lnAlias)
  RETURN .T.
ENDIF

*-- If passed module id (lcChkModul) is not defined in module array, disable the 
*--  variable from option grid.
IF ASCAN(laRpTarMod, lcModlName) = 0
  IF SEEK(UPPER(lcFldName), lcRebalHdr)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH 'I'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    lcVarToUpd = UPPER(lcFldName)
    &lcVarToUpd = 'I'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    
    IF llSaveAut
      lcItemV  = UPPER(lcFldName)
      &lcItemV = 'I'
    ENDIF  
  ENDIF
  SET DATASESSION TO (lnDataSess)
  SELECT (lnAlias)
  RETURN .T.
ELSE
  IF SEEK(UPPER(lcFldName), lcRebalHdr)
    
    *B127969,1 WSH 05/25/2005 Get the value from the memory variable. [Start]
    *REPLACE &lcRebalHdr..cUpdVryIgn WITH IIF(&lcRebalHdr..cUpdVryIgn = 'I', 'V', &lcRebalHdr..cUpdVryIgn)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH IIF(TYPE(lcFldName) = 'C', EVALUATE(lcFldName), &lcRebalHdr..cUpdVryIgn)
    *B127969,1 WSH 05/25/2005 [End]
    
    IF llSaveAut
      lcItemV  = UPPER(lcFldName)
      &lcItemV = lcOldValue
    ENDIF
  ENDIF
ENDIF

SET DATASESSION TO (lnDataSess)
SELECT (lnAlias)
RETURN .F.

*!*************************************************************
*! Name      : lfvDfinMod
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Build source & target module array from main module array.
*!*************************************************************
*! Example   : =lfvDfinMod()
*!*************************************************************
FUNCTION lfvDfinMod

IF EMPTY(laRpCodMod[1,1]) OR EMPTY(laRpSorMod[1,1])
  *-- Declare and define modules.
  =lfvGetModl()

  * Declare soruce & Target of module array
  DECLARE laRpSorMod[1] , laRpTarMod[1]

  * Copy module array to source module array.
  lnModLen = ALEN(laRpCodMod,1)
  FOR lnI = 1  TO  lnModLen 
    DECLARE laRpSorMod[lnI] , laRpTarMod[lnI]  
    laRpSorMod[lnI] = laRpCodMod[lnI , 2] 
    laRpTarMod[lnI] = laRpCodMod[lnI , 2] 
  ENDFOR
  =lfModExpr()
ENDIF

*!*************************************************************
*! Name      : lfvUpdLog
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 03/30/2005
*! Purpose   : Replace Use Log File Field...
*!*************************************************************
*! Example   : = lfvUpdLog()
*!*************************************************************
FUNCTION lfvUpdLog

LOCAL lnDataSess, lnAlias
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")

SET DATASESSION TO (lnPrgSession)

*-- Now change the laInfoTemp.cUpdVryIgn  with the value of (Update/verify/Ignore)
SELECT (lcRebalHdr)
IF SEEK('LCLOGFILE')
  REPLACE cLogFile WITH IIF(llLogFile, 'Y', 'N')
ENDIF

SET DATASESSION TO (lnDataSess)

SELECT (lnAlias)
RETURN 

*!*************************************************************
*! Name      : lfvUVIAll
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Change the stauts (Update / Verify /Ignore) of all 
*!             rows of option grid. 
*!*************************************************************
*! Example   : = lfvUVIAll()
*!*************************************************************
FUNCTION lfvUVIAll

*B127969,1 WSH 05/25/2005 Refresh the OG controls after updating the Table. [Start]
*-- Get Length of Visiable rows in Option Grid
*!*  PRIVATE lnOGTypLen 
*!*  lnOGTypLen = ALEN(laOGObjType,1)

*!*  *-- Note:
*!*  *-- I know the next FOR ... ENDFOR loop it's not good to start from hard code 
*!*  *-- number 5. but I will try to find out a way to get first item by generic function.
*!*  *-- Get the first position item that can be used as a begining of first item that 
*!*  *-- going to be rebalanced.

*!*  FOR lnI = 1  TO lnOGTypLen
*!*    IF 'LCRPITEM' $ UPPER(laOGObjType[lnI ,1])
*!*      &laOGObjType[lnI,1] = lcUpdVry
*!*      =lfOGShowGet("&laOGObjType[lnI ,1]")
*!*    ENDIF
*!*  ENDFOR

*!*  LOCAL lnDataSess
*!*  lnDataSess = SET("Datasession")

*!*  SET DATASESSION TO (lnPrgSession)

*!*  *-- Now change the laInfoTemp.cUpdVryIgn  with the value of (Update all / vrify all / Ignore all)
*!*  SELECT (lcRebalHdr)
*!*  IF SEEK('LCLOGFILE')
*!*    REPLACE cLogFile WITH 'Y'
*!*  ENDIF

*!*  LOCATE
*!*  SCAN WHILE !EOF() FOR  'LCRPITEM' $ UPPER(&lcRebalHdr..cItemName)
*!*    REPLACE &lcRebalHdr..cUpdVryIgn WITH lcUpdVry
*!*  ENDSCAN

*!*  SET DATASESSION TO (lnDataSess)

LOCAL lnAlias, lnDataSess, lnOGTypLen
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")

*-- Get Length of Visiable rows in Option Grid
lnOGTypLen = ALEN(laOGObjType,1)

SET DATASESSION TO (lnPrgSession)

*-- Now change the laInfoTemp.cUpdVryIgn  with the value of (Update all / vrify all / Ignore all)
SELECT (lcRebalHdr)
IF SEEK('LCLOGFILE')
  REPLACE cLogFile WITH 'Y'
ENDIF

LOCATE
SCAN WHILE !EOF() FOR  'LCRPITEM' $ UPPER(&lcRebalHdr..cItemName)
  REPLACE &lcRebalHdr..cUpdVryIgn WITH lcUpdVry
ENDSCAN

SET DATASESSION TO (lnDataSess)

FOR lnI = 1  TO lnOGTypLen
  IF 'LCRPITEM' $ UPPER(laOGObjType[lnI ,1])
    &laOGObjType[lnI,1] = lcUpdVry
    =lfOGShowGet("&laOGObjType[lnI ,1]")
  ENDIF
ENDFOR

SELECT (lnAlias)
RETURN
*B127969,1 WSH 05/25/2005 [End]

*-- end of lfvUVIAllf.

*!*************************************************************
*! Name      : lfvUpVrIg
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Replace the stauts flag (Update / Verify /Ignore) in temp.
*!             file with the value of Option Grid.
*!*************************************************************
*! Example   : = lfvUpVrIg()
*!*************************************************************
FUNCTION lfvUpVrIg
PARAMETER  lcCurRow

LOCAL lnDataSess, lcGetValue
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

* Now change the laInfoTemp.cUpdVryIgn  with the value of (Update/verify/Ignore)
IF SEEK( UPPER(lcCurRow) , lcRebalHdr)
  lcGetValue = &lcCurRow
  REPLACE &lcRebalHdr..cUpdVryIgn WITH lcGetValue 
ENDIF

SET DATASESSION TO (lnDataSess)
RETURN 
*--- End of lfvUpVrIg()

*!*************************************************************
*! Name      : lfGetVendor
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Get Vendor Name...
*!*************************************************************
*! Example     : = lfGetVendor()
*!*************************************************************
FUNCTION lfGetVendor
LPARAMETERS lcVenCode, lcStyType

LOCAL lcRetVal
lcRetVal = ''

IF lcStyType = 'N'
  IF loWareHous.SEEK(RTRIM(lcVenCode))
    lcRetVal = EVALUATE(lcWareHous + '.cDesc')
  ENDIF
ELSE
  IF loVendor.SEEK(RTRIM(lcVenCode))
    lcRetVal = EVALUATE(lcVendor + '.cVenComp')
  ENDIF
ENDIF

RETURN lcRetVal

*!*************************************************************
*! Name      : lfStySum
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/13/2004
*! Purpose   : Sum Quantities from Style file
*!*************************************************************
*! Example     : = lfStySum()
*!*************************************************************
FUNCTION lfStySum
LPARAMETERS lcSty, lccomp, lnAddToVar

LOCAL lnTotcomp, lnAlias
lnAlias   = SELECT(0)
lnTotcomp = 0

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*IF loStyle_X.SEEK(ALLTRIM(lcSty))
IF loStyle_X.SEEK(lcSty)
*B130510,1 WSH 12/05/2005 [End]

  SELECT (lcStyle_X)
  
  *B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
  *SUM REST &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SUM REST &lcCOMP TO lnTotcomp WHILE Style = lcSty
  *B130510,1 WSH 12/05/2005 [End]
  
ENDIF 

DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

SELECT (lnAlias)

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*RETURN INT(lnTotcomp)
RETURN lnTotcomp
*B130510,1 WSH 12/05/2005 [End]

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Example   : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet
LPARAMETERS lcItmInvType

*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
*RETURN gfItemMask('HM', '', lcItmInvType)
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  RETURN loItemMask.Do('HM', '', lcItmInvType)  
ELSE
  RETURN gfItemMask('HM', '', lcItmInvType)
ENDIF 
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]

*!*************************************************************
*! Name      : lfGetCust
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : To get the Customer name
*!*************************************************************
*! Example   : = lfGetCust()
*!*************************************************************
FUNCTION lfGetCust
LPARAMETERS lcAccount

IF loCustomer.SEEK('M' + lcAccount)
  RETURN EVALUATE(lcCustomer + '.btName')
ENDIF

*!*************************************************************
*! Name      : lfInfoHdr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Create one temporary file
*!             Fill it with header info. that will need it while process
*!*************************************************************
*! Example   : = lfInfoHdr()
*!*************************************************************
FUNCTION lfInfoHdr

LOCAL lnDataSess
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

* lcUpVrIg is a varibale represents the default value of Update all / Verify all/ Ignore all.
PRIVATE  lcUpVrIg 
lcUpVrIg = 'V'  

IF USED(lcRebalHdr)
  SELECT (lcRebalHdr)
  USE
ENDIF
IF USED(lcRebalDtl)
  SELECT (lcRebalDtl)
  USE
ENDIF

* Get temporary file header and index name.
lcInfoHdr = gfTempName()  

*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
*CREATE TABLE (oAriaApplication.WorkDir + lcInfoHdr ) ;
  (cItemName  C(10)  ,;
   cUpdFunNam C(10)  ,;
   cVryFunNam C(10)  ,;
   cIgnFunNam C(10)  ,;    
   cItemDesc  C(30)  ,;
   cAddSetup  C(10)  ,;
   cValidFun  C(10)  ,;
   cUpdVryIgn C(1)   ,;
   cLogFile   C(1)   ,;
   cTempName  C(8)   ,;
   cFileStr   C(180) ,;
   cMFiles    C(80)  ,;
   cInvType   C(4))
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}   
*CREATE TABLE (oAriaApplication.WorkDir + lcInfoHdr ) ;
  (cItemName  C(10)  ,;
   cUpdFunNam C(10)  ,;
   cVryFunNam C(10)  ,;
   cIgnFunNam C(10)  ,;    
   cItemDesc  C(30)  ,;
   cAddSetup  C(10)  ,;
   cValidFun  C(10)  ,;
   cUpdVryIgn C(1)   ,;
   cLogFile   C(1)   ,;
   cTempName  C(8)   ,;
   cFileStr   M(10) ,;
   cMFiles    C(80)  ,;
   cInvType   C(4))
CREATE TABLE (oAriaApplication.WorkDir + lcInfoHdr ) ;
  (cItemName  C(10)  ,;
   cUpdFunNam C(10)  ,;
   cVryFunNam C(10)  ,;
   cIgnFunNam C(10)  ,;    
   cItemDesc  C(30)  ,;
   cAddSetup  C(10)  ,;
   cValidFun  C(10)  ,;
   cUpdVryIgn C(1)   ,;
   cLogFile   C(1)   ,;
   cTempName  C(8)   ,;
   cFileStr   M(10) ,;
   cMFiles    C(80)  ,;
   cInvType   C(4)   ,;
   cModule    C(2),cModule2 C(2))
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}   
*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

* Create index on temporary file    
INDEX ON  cItemName TAG ItemHdr  OF (oAriaApplication.WorkDir + lcInfoHdr + '.CDX') 
USE     && Colse the temp. file and open it with alias name 
USE (oAriaApplication.WorkDir + lcInfoHdr ) ALIAS (lcRebalHdr) IN 0 
SET ORDER TO TAG ItemHdr

* Get temporary file detail and index name.
lcInfoDtl  = gfTempName()  

*Build the information file structure.
CREATE TABLE (oAriaApplication.WorkDir + lcInfoDtl ) ;
   (cItemName  C(10) ,;
    cFileName  C(10) ,;
    cNdxName   C(10) ,;
    cAliasName C(10) ,;    
    lOpenBefor L(1)  ,;
    cFileObj   C(10) ,;
    lSQLFile   L(1))

* Fields description
*Field name         Description
*----------     -----------------------------------------------------------
*cItemName     Represents SyRepUvr.mfld_name field
*cFileName     File name 
*cNdxName      index name
*cAliasName    Alias name
*lOpenBefor    '.T.' file was open before , '.F.' file was not open before
*----------     -----------------------------------------------------------
* Create index on temporary file

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*INDEX ON cItemName TAG ItemDtl
*INDEX ON cFileName TAG cFileName
INDEX ON cItemName+cFileName TAG ItemDtl
*B128464,2 WSH 08/21/2005 [End]

USE     && Colse the temp. file and open it with alias name 
USE (oAriaApplication.WorkDir + lcInfoDtl) ALIAS (lcRebalDtl) IN 0
SET ORDER TO ItemDtl

* Insert generate rebalance log information (LCLOGFILE). 
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCLOGFILE' ,;
        cUpdFunNam WITH 'UpdName'   ,;
        cVryFunNam WITH 'VryName'   ,; 
        cIgnFunNam WITH 'IgnName'   ,;     
        cLogFile   WITH  'Y'

*----------  Not in  this Release
*!*  * Header info. about GL relabance information (lcRpItem1).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1'  ,;
*!*          cUpdFunNam WITH 'lfUpdItem1' ,;
*!*          cVryFunNam WITH 'lfVryItem1' ,; 
*!*          cIgnFunNam WITH 'IgnName'    ,;     
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStupItm1' ,;
*!*          cValidFun  WITH 'lfValdItm1'

*!*  * Detail info. about GL rebalance master files 'lcRpItem1'.
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLBATCH'   ,;
*!*           cNdxName   WITH 'BATSTAT'   ,;
*!*          cAliasName WITH 'GLBATCH'   ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLPTRNHD'  ,;
*!*           cNdxName   WITH ''          ,;
*!*          cAliasName WITH 'GLTRNSHD'  ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLPTRNDT'  ,;
*!*           cNdxName   WITH 'BATCHTRN'  ,;
*!*          cAliasName WITH 'GLTRNSDT'  ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLACBALS'  ,;
*!*           cNdxName   WITH 'ACCYRPRD'  ,;
*!*          cAliasName WITH 'GLTMPBAL'  ,;    
*!*          lOpenBefor WITH .F.


*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLACCHAR' ,;
*!*           cNdxName   WITH 'ACCTCODE'  ,;
*!*          cAliasName WITH 'GLACCHAR'  ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'FSPRD'     ,;
*!*           cNdxName   WITH 'COMFYRPRDI',;
*!*          cAliasName WITH 'FSPRD'     ,;    
*!*          lOpenBefor WITH .F.  

*-- End of Setup Header and detail files of GL (lcRpItem1) --*

*!*  * Header info. about AP relabance information (lcRpItem2).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2'  ,;
*!*          cUpdFunNam WITH 'lfUpdItem2' ,;
*!*          cVryFunNam WITH 'lfVryItem2' ,; 
*!*          cIgnFunNam WITH 'IgnName'    ,;     
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStupItm2' ,;
*!*          cValidFun  WITH 'lfValdItm2' 

*!*  * Detail info. about GL rebalance master files (lcRpItem2).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APVENDOR'  ,;
*!*           cNdxName   WITH 'VENCODE'   ,;
*!*          cAliasName WITH 'APVENDOR'  ,;    
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'FISHD'     ,;
*!*           cNdxName   WITH 'COMPFYEAR' ,;
*!*          cAliasName WITH 'FISHD'     ,;    
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*        cFileName  WITH 'APDIST'    ,;
*!*           cNdxName   WITH 'INVVEND'   ,;
*!*          cAliasName WITH 'APDIST'    ,;    
*!*        lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APINVHDR'  ,;
*!*           cNdxName   WITH 'VENDINV'   ,;
*!*          cAliasName WITH 'APINVHDR'  ,;    
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APVENHST'  ,;
*!*           cNdxName   WITH 'VENDYEAR' ,;
*!*          cAliasName WITH 'APVENHST'  ,;    
*!*          lOpenBefor WITH .F.  
*!*  *-- End of Setup Header and detail files of AP (lcRpItem2) --*

* Insert Material on-hand information (lcRpItem3).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM3' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MA"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
* Insert Material on-order information (lcRpItem4).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM4' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MA"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
    
* Insert Style on-hand information (lcRpItem5).
* ----------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM5' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg


*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "IC"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
    
* Insert Style in transit information (lcRpItem6).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM6' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg


*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "PO"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Insert Style work orders information (lcRpItem7).
* -------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM7' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "PO",;
        cModule2 WITH "MF"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Insert Style work in process information (lcRpItem8).
* -----------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM8' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "PO",;
        cModule2 WITH "MF"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Insert Style shipped quantity information (lcRpItem9).
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM9' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg


*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AR"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM10' ,;
	      cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,; 
        cIgnFunNam WITH 'lfIgn'      ,;     
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AL"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

*----------  Not in  this Release
*!*  * Insert Style allocated quantity information (lcRpItem10).
*!*  * ------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM10' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,; 
*!*          cIgnFunNam WITH 'lfIgn'      ,;     
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg

* Insert Style ordered quantity information (lcRpItem11).
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM11' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "SO"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
*----------  Not in  this Release
*!*  * Insert Return Authorization (lcRpItem12).
*!*  * -----------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM12' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,;
*!*          cIgnFunNam WITH 'lfIgn'      ,;
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg

*!*  * Insert Return Credit Memo (lcRpItem13).
*!*  * ---------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM13' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,; 
*!*          cIgnFunNam WITH 'lfIgn'      ,;     
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg


*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
* Insert Return Authorization (lcRpItem12).
* -----------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM12' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "RM"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Insert Return Credit Memo (lcRpItem13).
* ---------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM13' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,; 
        cIgnFunNam WITH 'lfIgn'      ,;     
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "RM"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[Start]
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2'  ,;
    cUpdFunNam WITH 'lfUpdItem2' ,;
    cVryFunNam WITH 'lfVryItem2' ,; 
    cIgnFunNam WITH 'IgnName'    ,;     
    cUpdVryIgn WITH  lcUpVrIg     ,;
    cAddSetup  WITH 'lfStupItm2' ,;
    cValidFun  WITH 'lfValdItm2' 


*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AP"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}


SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
    cFileName  WITH 'APVENDOR'  ,;
     cNdxName   WITH 'VENCODE'   ,;
    cAliasName WITH 'APVENDOR'  ,;    
    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
    cFileName  WITH 'FISHD'     ,;
     cNdxName   WITH 'COMPFYEAR' ,;
    cAliasName WITH 'FISHD'     ,;    
    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
  cFileName  WITH 'APDIST'    ,;
     cNdxName   WITH 'INVVEND'   ,;
    cAliasName WITH 'APDIST'    ,;    
  lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
    cFileName  WITH 'APINVHDR'  ,;
     cNdxName   WITH 'VENDINV'   ,;
    cAliasName WITH 'APINVHDR'  ,;    
    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
    cFileName  WITH 'APVENHST'  ,;
     cNdxName   WITH 'VENDYEAR' ,;
    cAliasName WITH 'APVENHST'  ,;    
    lOpenBefor WITH .F.  
    
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM24'  ,;
    cUpdFunNam WITH 'lfUpdItm24' ,;
    cVryFunNam WITH 'lfVryItm24' ,; 
    cIgnFunNam WITH 'IgnName'    ,;     
    cUpdVryIgn WITH  lcUpVrIg     ,;
    cAddSetup  WITH 'lfStpItm24' ,;
    cValidFun  WITH 'lfVldItm24' 
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AP"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM24' ,;
  cFileName  WITH 'APDIST'    ,;
     cNdxName   WITH 'INVVEND'   ,;
    cAliasName WITH 'APDIST'    ,;    
  lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM24' ,;
    cFileName  WITH 'APINVHDR'  ,;
     cNdxName   WITH 'VENDINV'   ,;
    cAliasName WITH 'APINVHDR'  ,;    
    lOpenBefor WITH .F.  
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[End]

* Header info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
lcFName  = '(Invoice C(6)|Ship N(7)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'InvHdr,InvLine'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,; 
        cIgnFunNam WITH 'IgnName'    ,;     
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcArInv      ,;
        cInvType   WITH '0001'       ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AR"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Detail info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cFileName  WITH 'INVHDR'     ,;
        cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvHdr'   ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cFileName  WITH 'INVLINE'    ,;
        cNdxName   WITH 'INVLINE'    ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvLine'  ,;
        lOpenBefor WITH .F.  

* Header info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|Cancel N(8)|CancelAmt N(13,2)|Book N(8)|BookAmt N(11,2)|Ship N(8)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'OrdHdr,OrdLine'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSoOrd      ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0001'      ,;
        cMFiles    WITH lcMFiles 
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "SO"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Detail info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15',;
        cFileName  WITH 'ORDHDR'    ,;
        cNdxName   WITH 'ORDHDR'    ,;
        cAliasName WITH gfTempName(),;
        cFileObj   WITH 'loOrdHdr'  ,;
        lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'ORDLINE'    ,;
        cNdxName   WITH 'ORDLINE'    ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loOrdLine'  ,;
        lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'INVHDR'     ,;
        cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvHdr'   ,;
        lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'INVLINE'    ,;
        cNdxName   WITH 'INVLINEO'   ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvLine'  ,;
        lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'ORDCANLN'   ,;
        cNdxName   WITH 'ORDCANLN'   ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loOrdCanLn' ,;
        lOpenBefor WITH .F.  

*----------  Not in  this Release
*!*  * Header info. of CT header (lcRpItem16).
*!*  * ---------------------------------------
*!*  lcFName  = '(CutTkt C(6)|Pcs_Bud N(7)|Pcs_Rec N(7)|cAdd_time C(11)|dAdd_Date D)'
*!*  lcMFiles = 'CutTktH,CutTktL'

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]

*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
*lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)|cAdd_time C(11)|dAdd_Date D)'
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)'
FOR lnCstCnt = 1 TO 7
  lcCstCnt = STR(lnCstCnt,1)
  lcFName  = lcFName + "|"+"niCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nfCost"+ lcCstCnt +" N(13,3)" 
  lcFName  = lcFName + "|"+"nflanCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nlan_Cost"+ lcCstCnt +" N(13,3)"
ENDFOR 
lcFName   = lcFName + "|cAdd_time C(11)|dAdd_Date D)"
**: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

lcMFiles = 'POSHDR,POSLN'
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]



*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,; 
        cIgnFunNam WITH 'IgnName'    ,;     
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcMfCt     ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles   ,;
        cInvType   WITH '0001'      
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MF"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

*!*  * Detail info. of CT header (lcRpItem16).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16',;
        cFileName  WITH 'POSHDR'   ,;
        cNdxName   WITH 'POSHDR'   ,;
        cAliasName WITH gfTempName()   ,;    
        lOpenBefor WITH .F.  ,;
        cFileObj   WITH 'loCUTTKTH'  ,;
        lSQLFile   WITH .T.


APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
        cFileName  WITH 'POSLN'    ,;
        cNdxName   WITH 'POSLN'    ,;
        cAliasName WITH gfTempName()    ,;    
        lOpenBefor WITH .F.           ,; 
        cFileObj   WITH 'loCUTTKTL'  ,;
        lSQLFile   WITH .T.

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

* Header info. of Style PO header (lcRpItem17).
* ---------------------------------------

*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
*lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)|cAdd_time C(11)|dAdd_Date D)'
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)'
FOR lnCstCnt = 1 TO 7
  lcCstCnt = STR(lnCstCnt,1)
  lcFName  = lcFName + "|"+"niCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nfCost"+ lcCstCnt +" N(13,3)" 
  lcFName  = lcFName + "|"+"nflanCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nlan_Cost"+ lcCstCnt +" N(13,3)"
ENDFOR 
lcFName   = lcFName + "|cAdd_time C(11)|dAdd_Date D)"
*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

lcMFiles = 'POSHDR,POSLN'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,; 
        cIgnFunNam WITH 'IgnName'    ,;     
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSPO        ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0001'      ,;
        cMFiles    WITH lcMFiles 
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "PO"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

* Detail info. of CT header (lcRpItem17).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
        cFileName  WITH 'POSHDR'    ,;
        cNdxName   WITH 'POSHDR'    ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
        cFileName  WITH 'POSLN'     ,;
        cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSLN'   ,;
        lSQLFile   WITH .T.

* Header info. of Material PO header (lcRpItem18).
* ---------------------------------------

*B128464,2 WSH 08/21/2005 Add decimals for the Material PO Quantities. [Start]
*: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
*lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(11,3)|Receive N(11,3)|Open N(11,3)|Cancel N(11,3)|Damage N(11,3)|cAdd_time C(11)|dAdd_Date D)'
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(13,3)|Receive N(13,3)|Open N(13,3)|Cancel N(13,3)|Damage N(13,3)'
FOR lnCstCnt = 1 TO 7
  lcCstCnt = STR(lnCstCnt,1)
  lcFName  = lcFName + "|"+"niCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nfCost"+ lcCstCnt +" N(13,3)" 
  lcFName  = lcFName + "|"+"nflanCost"+ lcCstCnt+" N(13,3)" 
  lcFName  = lcFName + "|"+"nlan_Cost"+ lcCstCnt +" N(13,3)"
ENDFOR 
lcFName   = lcFName + "|cAdd_time C(11)|dAdd_Date D)"
**: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

lcMFiles = 'POSHDR,POSLN'
*B128464,2 WSH 08/21/2005 [End]

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,; 
        cIgnFunNam WITH 'IgnName'    ,;     
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSPO        ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0002'       ,;
        cMFiles    WITH lcMFiles
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MA"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
* Detail info. of CT header (lcRpItem17).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18',;
        cFileName  WITH 'POSHDR'    ,;
        cNdxName   WITH 'POSHDR'    ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18',;
        cFileName  WITH 'POSLN'     ,;
        cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSLN'   ,;
        lSQLFile   WITH .T.

*----------  Not in  this Release
*!*  *-- This item is related to the AR Module.
*!*  SELECT (lcRebalHdr)

*!*  lcMFiles = 'InvHdr,InvLine,OrdHdr,RetHdr,Debit,Credit,ArHist,ArCusHst'

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cUpdFunNam WITH 'lfUpdCsHst' ,;
*!*          cVryFunNam WITH 'lfVryCsHst' ,; 
*!*          cIgnFunNam WITH 'IgnName'    ,;     
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStpCsHst' ,;
*!*          cValidFun  WITH 'lfVldHdDtl' ,;
*!*          cTempName  WITH 'lcArCsHst'  ,;
*!*          cFileStr   WITH ''           ,;
*!*          cMFiles    WITH lcMFiles 
*!*      
*!*  * Detail info. of AR Customer History (lcRpItem18).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'INVHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'INVHDR'     ,;    
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'INVLINE'    ,;
*!*           cNdxName   WITH 'INVLINE'    ,;
*!*          cAliasName WITH 'INVLINE'    ,;
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ORDHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ORDHDR'     ,;
*!*          lOpenBefor WITH .F.  

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'RETHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'RETHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'DEBIT'      ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'DEBIT'      ,;    
*!*          lOpenBefor WITH .F.        

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'CREDIT'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'CREDIT'     ,;    
*!*          lOpenBefor WITH .F.        

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ARHIST'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ARHIST'     ,;    
*!*          lOpenBefor WITH .F.        

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ARCUSHST'   ,;
*!*           cNdxName   WITH 'ACTHST'     ,;
*!*          cAliasName WITH 'ARCUSHST'   ,;    
*!*          lOpenBefor WITH .F.        

*!*  *-- This item is related to the IC Module.
*!*  SELECT (lcRebalHdr)
*!*  lcMFiles = 'InvHdr,InvLine,OrdHdr,OrdLine,RetHdr,RetLine,ICStyHst'

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cUpdFunNam WITH 'lfUpdStHst' ,;
*!*          cVryFunNam WITH 'lfVryStHst' ,; 
*!*          cIgnFunNam WITH 'IgnName'    ,;     
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStpStHst' ,;
*!*          cValidFun  WITH 'lfVldHdDtl' ,;
*!*          cTempName  WITH 'lcIcStHst'   ,;
*!*          cFileStr   WITH ''           ,;
*!*          cMFiles    WITH lcMFiles 

*!*  * Detail info. of IC Style History (lcRpItem19).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'INVHDR'     ,;
*!*           cNdxName   WITH 'INVHDR'     ,;
*!*          cAliasName WITH 'INVHDR'     ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'INVLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'INVLINE'    ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ORDHDR'     ,;
*!*           cNdxName   WITH 'ORDHDR'     ,;
*!*          cAliasName WITH 'ORDHDR'     ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ORDLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ORDLINE'    ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'RETHDR'     ,;
*!*           cNdxName   WITH 'RETHDR'     ,;
*!*          cAliasName WITH 'RETHDR'     ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'RETLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'RETLINE'    ,;    
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ICSTYHST'   ,;
*!*           cNdxName   WITH 'STYHST'     ,;
*!*          cAliasName WITH 'ICSTYHST'   ,;    
*!*          lOpenBefor WITH .F.        

*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
* Insert Material Usage information (lcRpItem21).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MA"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
		
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
	    cFileName  WITH 'BOMCOST'     ,;
   	    cNdxName   WITH 'BOMCOST'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loBOMCOST'  ,;
        lSQLFile   WITH .T.


APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
	    cFileName  WITH 'POSHDR'     ,;
   	    cNdxName   WITH 'POSHDR'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.


*: B607982,1 MMT 12/18/2006 Fix bug of Wrong update of material usage[Start]
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
        cFileName  WITH 'BOMLINE'     ,;
        cNdxName   WITH 'BOMLINEU'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loBOMLINE'  ,;
        lSQLFile   WITH .T.
*: B607982,1 MMT 12/18/2006 Fix bug of Wrong update of material usage[End]


* Insert Material In Transit information (lcRpItem22).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM22' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,; 
        cIgnFunNam WITH 'lfIgn'     ,;     
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "MA"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
		
SELECT (lcRebalDtl)
APPEND BLANK   
REPLACE cItemName  WITH 'LCRPITEM22' ,;
   	    cFileName  WITH 'POSLN'     ,;
   	    cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'MATSHPDT'  ,;
        lSQLFile   WITH .T.
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
lcFName  = '(PACK_NO C(6)|TOT_CART N(8)|TOT_PCS N(8)|TOT_WGHT N(13,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'PACK_HDR,PACK_LIN'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM23' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,; 
        cIgnFunNam WITH 'IgnName'    ,;     
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcALPack ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles   ,;
        cInvType   WITH '0001' 
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
REPLACE cModule WITH "AL"
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
    
SELECT (lcRebalDtl)
APPEND BLANK   
REPLACE cItemName  WITH 'LCRPITEM23' ,;
         cFileName  WITH 'PACK_LIN'     ,;
         cNdxName   WITH 'PACK_LIN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPACK_LIN'  ,;
        lSQLFile   WITH .F.
        
SELECT (lcRebalDtl)
APPEND BLANK   
REPLACE cItemName  WITH 'LCRPITEM23' ,;
         cFileName  WITH 'PACK_HDR'     ,;
         cNdxName   WITH 'PACK_HDR'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPACK_HDR'  ,;
        lSQLFile   WITH .F.
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
    
SET DATASESSION TO (lnDataSess)
*-- End of lfInfoHdr()

*!*************************************************************
*! Name      : lfErasTmp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : - Erase temp. files 
*!               Erase temp. index. 
*!*************************************************************
*! Example   : = lfErasTmp()
*!*************************************************************
FUNCTION lfErasTmp

IF FILE(oAriaApplication.WorkDir + lcInfoHdr + '.DBF')
  IF USED(lcRebalHdr)
    SELECT (lcRebalHdr)
    SCAN
      IF !EMPTY(cTempName) AND FILE(oAriaApplication.DataDir + cTempName + '.DBF')
        ERASE(oAriaApplication.DataDir + cTempName + '.DBF')
        IF FILE(oAriaApplication.DataDir + cTempName +'.CDX')
          ERASE(oAriaApplication.DataDir + cTempName +'.CDX')
        ENDIF
      ENDIF  
    ENDSCAN
    USE    && Close info. header file 
  ENDIF
  ERASE (oAriaApplication.WorkDir + lcInfoHdr + '.DBF' )
  ERASE (oAriaApplication.WorkDir + lcInfoHdr + '.CDX' )
ENDIF  

IF FILE(oAriaApplication.WorkDir + lcInfoDtl  + '.DBF' )
  IF USED(lcRebalDtl)
    SELECT (lcRebalDtl)  
    USE    && Close info. detail file 
  ENDIF
  ERASE (oAriaApplication.WorkDir + lcInfoDtl + '.DBF' )
  ERASE (oAriaApplication.WorkDir + lcInfoDtl + '.CDX' )
ENDIF  

RETURN 
*-- End of lfErasTmp

*!*************************************************************
*! Name      : lfvFisYer
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/23/1999
*! Purpose   : - Browse fiscal years 
*!*************************************************************
FUNCTION lfvFisYer
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcFisYear', @lcSelYear, 'cFisfYear C(4)', 'cFisfYear', 'cFisfYear')

*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Open needed files.
*!*************************************************************
*! Example   : = lfOpnFiles()
*!*************************************************************
FUNCTION lfOpnFiles
LPARAMETER lcItemId

LOCAL lcCurrArea, lcFileName, lcNdxName, lcAliasNam, lcFileObj, loOpenRDAC, llOpnError
lcCurrArea = SELECT(0)
llOpnError = .F.

SELECT (lcRebalDtl)
=SEEK(UPPER(lcItemId))
SCAN REST WHILE cItemName = UPPER(lcItemId)
  *-- Assign file name , Order name to variables
  lcFileName = ALLTRIM(&lcRebalDtl..cFileName)
  lcNdxName  = ALLTRIM(&lcRebalDtl..cNdxName)
  lcAliasNam = ALLTRIM(&lcRebalDtl..cAliasName)
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loOpenRDAC = CREATEOBJECT('RemoteTable', lcFileName, lcNdxName, lcAliasNam, SET("Datasession"), lcCurrComp_ID)
  *IF TYPE("lOOpenRDAC") # 'O'
  *
  *  *-- Message : XXX Not found in data file.
  *  *--                   <Ok>
  *  DECLARE laRebMsg[3]
  *  laRebMsg[1] = " "
  *  laRebMsg[2] = LANG_SMREBAL_TABLE + ' "' + lcFileName + '" ' + LANG_SMREBAL_NOTFND
  *  laRebMsg[3] = " "
  *  IF llAutoBal
  *    laRebMsg[3] = DTOC(DATE()) + SPACE(5) + TIME()
  *  ENDIF
  *  
  *  =lfVryRport()
  *  llOpnError = .T.
  *  
  *  *-- If there is error don't continue opening file and quit
  *  EXIT
  *ELSE
  *  lcFileObj  = &lcRebalDtl..cFileObj
  *  &lcFileObj = loOpenRDAC
  *ENDIF
  = lfFilesStr(lcFileName, lcNdxName, lcAliasNam, '', '', .T.)
  *B128464,2 WSH 08/21/2005 [End]
  
ENDSCAN

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
= lfOpenFls()
*B128464,2 WSH 08/21/2005 [End]

SELECT (lcCurrArea)
RETURN !llOpnError
*-- end if lfOpnFiles.

*------------------ Not in this Release ----------------------*
*                                                             *
*                                                             *
*!*  *!*************************************************************
*!*  *! Name      : lfValdItm1
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Check whether all mandatory factors that be used in
*!*  *!             GL are defined or not. 
*!*  *!*************************************************************
*!*  *! Example   : =lfValdItm1()
*!*  *!*************************************************************
*!*  FUNCTION lfValdItm1

*!*  PRIVATE lcCurrArea
*!*  lcCurrArea = SELECT(0)

*!*  PRIVATE llValdMand
*!*  llValdMand =  .T.

*!*  * Check fiscal year variable
*!*  IF EMPTY(lcRpFiscYr)
*!*    llValdMand =.F.
*!*    DECLARE laRebMsg[3]
*!*    laRebMsg[1] = " "
*!*    laRebMsg[2] = LANG_SMREBAL_BADFY
*!*    laRebMsg[3] = " "
*!*    IF llAutoBal
*!*      laRebMsg[3] = LANG_SMREBAL_ENDPROC + DTOC(DATE()) + SPACE(5) + TIME()
*!*    ENDIF
*!*    =lfVryRport()
*!*  ENDIF

*!*  SELECT (lcCurrArea)
*!*  RETURN llValdMand 
*!*  *--- End of lfValdItm1() 

*!*  *!*************************************************************
*!*  *! Name      : lfStupItm1
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Additional GL setup that needs to process GL rebalance
*!*  *!*************************************************************
*!*  *! Example   : = lfStupItm1()
*!*  *!*************************************************************
*!*  FUNCTION  lfStupItm1

*!*  PRIVATE lcCurrArea
*!*  lcCurrArea = SELECT(0)

*!*  *-- Call this function here to get the current year form SycComp file
*!*  =lfGetCurYr()

*!*  lcTmpBBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Beginning batches to be Reposted
*!*  lcTmpJBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Normal batches to be Reposted
*!*  lcTmpCBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Closing batches to be Reposted
*!*  lcTmpTran   = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Single transactions to be Reposted
*!*  lcTmpGLTHd  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLPTRNHD file
*!*  lcTmpGLTDt  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLTRNSDT file

*!*  DECLARE laTempName[6] 
*!*  laTempName[1] =  lcTmpBBtch  
*!*  laTempName[2] =  lcTmpJBtch
*!*  laTempName[3] =  lcTmpCBtch 
*!*  laTempName[4] =  lcTmpTran 
*!*  laTempName[5] =  lcTmpGLTHd 
*!*  laTempName[6] =  lcTmpGLTDt

*!*  SELECT GLTRNSHD
*!*  COPY STRUCTURE TO (oAriaApplication.WorkDir + lcTmpGLTHd) WITH CDX
*!*  SELECT GLTRNSDT
*!*  COPY STRUCTURE TO (oAriaApplication.WorkDir + lcTmpGLTDt) WITH CDX

*!*  *IF There is any file that is opened with the Alias GLPTRNHD
*!*  IF USED('GLPTRNHD')
*!*    llUsdTrnHd = .T.
*!*    SELECT GLPTRNHD
*!*    lcUsdTrnHd = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNHD
*!*    lnUsdTrnHd = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNHD
*!*    USE
*!*  ENDIF    && End of IF

*!*  *IF There is any file that is opened with the Alias GLPTRNDT
*!*  IF USED('GLPTRNDT')
*!*    llUsdTrnDt = .T.
*!*    SELECT GLPTRNDT
*!*    lcUsdTrnDt = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNDT
*!*    lnUsdTrnDt = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNDT
*!*    USE
*!*  ENDIF    && End of IF

*!*  USE (oAriaApplication.WorkDir + lcTmpGLTHd) ALIAS GLPTRNHD IN 0
*!*  USE (oAriaApplication.WorkDir + lcTmpGLTDt) ALIAS GLPTRNDT IN 0

*!*  *IF There is any file that is opened with the Alias GLACBALS
*!*  IF USED('GLACBALS')
*!*    llUsdAcBal = .T.
*!*    SELECT GLACBALS
*!*    lnUsdAcBal = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLACBALS
*!*    lcUsdAcBal = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLACBALS
*!*    USE
*!*  ENDIF    && End of IF

*!*  IF &lcRebalHdr..cUpdVryIgn='U' AND !FILE(lcFilePath + 'GLTMPBAL.DBF')
*!*    =lfVryItem1()
*!*  ENDIF

*!*  *IF The file GLTMPBAL exist
*!*  IF FILE(lcFilePath + 'GLTMPBAL.DBF')
*!*    llBalnBfor = .T.
*!*    lcUpDateSt = 'ENABLE'
*!*    USE (lcFilePath + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0 EXCLUSIVE
*!*  ENDIF    && End of IF

*!*  *IF The memory variable file exist
*!*  IF FILE(lcFilePath + 'GLRBAL.MEM')
*!*    RESTORE FROM (lcFilePath + 'GLRBAL.MEM') ADDITIVE
*!*  ENDIF    && End of IF
*!*  lnPostTran = IIF(TYPE('mvPostTran') <> 'N' , 0 , mvPostTran)    && Varible to hold the number of transactions that was Reposted in the Rebalance process
*!*  RELEASE ALL LIKE mv*

*!*  SELECT (lcCurrArea)
*!*  RETURN 

*!*  *!*************************************************************
*!*  *! Name      : lfVryItem1
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Valid function for the Verify of (lcRpItem1)
*!*  *!*************************************************************
*!*  *! Called from : SMREBAL.PRG
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  FUNCTION lfVryItem1

*!*  lcRBalYear = lcFiscYear
*!*  llBalnBfor = .F.

*!*  *IF The Starting year is the Previous year
*!*  IF VAL(lcFiscYear) < VAL(lcCurr_yer)
*!*    * Meesage : "You have selected to rebalance starting previous year (�). "
*!*    *           "The system allows purging files of the previous year.      "
*!*    *           "Rebalancing starting from previous year wan't be accurate  "
*!*    *           "if you purged the files through this year.                 "
*!*    * Buttons :           <Proceed> <Cacel>

*!*    IF !llAutoBal
*!*      IF gfModalGen("TRM02250B00012" , "DIALOG" , lcFiscYear) = 2
*!*        RETURN
*!*      ENDIF    
*!*    ELSE
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "You have selected to rebalance starting previous year."
*!*      laRebMsg[3] = "The system allows purging files of the previous year."
*!*      laRebMsg[4] = "Rebalancing starting from previous year won't be accurate"
*!*      laRebMsg[5] = "if you purged the files through this year.                "
*!*      laRebMsg[6] = " "
*!*      =lfVryRport()
*!*    ENDIF
*!*  ENDIF    
*!*  =lfFixBatch()

*!*  *IF There is any Batches or Transactions to be Reposted
*!*  IF lfSelData()
*!*    *IF There is Single transactions to be Reposted and not all of them was  
*!*    *Reposted succesfully
*!*    IF RECCOUNT(lcTmpTran) <> 0    
*!*      lnTBPosted = 0
*!*      llFrReb = .T.
*!*      DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Transactions',lcTmpTran ,'CLOSING','GLACBALS',lcCurrComp_ID,lcFilePath,llFrReb",'',"T"     
*!*      IF RECCOUNT(lcTmpTran) <> lnTBPosted
*!*      * Meesage : "Some of the � were not succesfully reposted to the "
*!*      *           "balances file , Unable to proceed with rebalancing."
*!*      * Buttons :                           <Ok>
*!*      
*!*        DECLARE laRebMsg[5]
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = PADC("GL Rebalance" , 68) 
*!*        laRebMsg[3] = "Some of the transactions were not succesfully reposted to the " 
*!*        laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
*!*        laRebMsg[5] = " "

*!*        IF llAutoBal
*!*          laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*        ENDIF
*!*        =lfVryRport()
*!*        =lfRmTmpBal()
*!*        RETURN
*!*      ENDIF    && End of IF
*!*    ENDIF
*!*    *IF There is Beginning batches to be Reposted and not all of them was  
*!*    *Reposted succesfully
*!*    IF RECCOUNT(lcTmpBBtch) <> 0      
*!*      lnTBPosted = 0
*!*      llFrReb = .T.
*!*      DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Batch',lcTmpBBtch ,'Beginning','GLACBALS',lcCurrComp_ID,lcFilePath,llFrReb",'',"T"          
*!*      IF RECCOUNT(lcTmpBBtch) <> lnTBPosted
*!*      * Meesage : "Some of the � were not succesfully reposted to the "
*!*      *           "balances file , Unable to proceed with rebalancing."
*!*      * Buttons :           <Ok>
*!*      
*!*        DECLARE laRebMsg[5]
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = PADC("Beginning Batches" , 68) 
*!*        laRebMsg[3] = "Some of the Batches were not succesfully reposted to the " 
*!*        laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
*!*        laRebMsg[5] = " "
*!*        IF llAutoBal
*!*          laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*        ENDIF
*!*        =lfVryRport()
*!*        =lfRmTmpBal()
*!*        RETURN
*!*      ENDIF    && End of IF
*!*    ENDIF
*!*    *IF There is Normal batches to be Reposted and not all of them was  
*!*    *Reposted succesfully
*!*    IF RECCOUNT(lcTmpJBtch) <> 0 
*!*      lnTBPosted = 0 
*!*      llFrReb = .T.
*!*      DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Batch',lcTmpJBtch ,'Nonbeginning','GLACBALS',lcCurrComp_ID,lcFilePath,llFrReb",'',"T"          
*!*      IF RECCOUNT(lcTmpJBtch) <> lnTBPosted
*!*        *E301337,1 ASH 10/18/1999 (End)
*!*        * Message : "Some of the � were not succesfully reposted to the "
*!*        *           "balances file , Unable to proceed with rebalancing."
*!*        * Buttons :           <Ok>
*!*        DECLARE laRebMsg[5]
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = PADC("Batches" , 68) 
*!*        laRebMsg[3] = "Some of the Batches were not succesfully reposted to the " 
*!*        laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
*!*        laRebMsg[5] = " "
*!*        IF llAutoBal
*!*          laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*        ENDIF
*!*        =lfVryRport()
*!*        =lfRmTmpBal()
*!*        RETURN
*!*      ENDIF    && End of IF
*!*    ENDIF
*!*    mvYear = lcFiscYear
*!*    mvPostTran = lnPostTran
*!*    SAVE TO (lcFilePath + 'GLRBAL.MEM') ALL LIKE mv*
*!*    SHOW GET pbReport ENABLE
*!*    SHOW GET pbUpDate ENABLE
*!*  ELSE    && Else
*!*    * Meesage : "No batche's or transaction's were reposted."
*!*    * Buttons :                <Ok>
*!*    DECLARE laRebMsg[3]
*!*    laRebMsg[1] = " "
*!*    laRebMsg[2] = "No batche's or transaction's were reposted."
*!*    laRebMsg[3] = " "
*!*    IF llAutoBal
*!*      laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*    ENDIF
*!*    =lfVryRport()
*!*    
*!*    mvYear = lcFiscYear
*!*    mvPostTran = lnPostTran
*!*    SAVE TO (lcFilePath + 'GLRBAL.MEM') ALL LIKE mv*
*!*    SHOW GET pbReport ENABLE
*!*    SHOW GET pbUpDate ENABLE
*!*  ENDIF    && End of IF
*!*  IF USED('GLTMPBAL')
*!*    USE IN GLTMPBAL
*!*  ENDIF
*!*  IF USED('GLACBALS')
*!*    USE IN GLACBALS
*!*  ENDIF
*!*  *-- End of lfVryItem1()

*!*  *!*************************************************************
*!*  *! Name      : lfFixBatch
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to Fix the GLBATCH file
*!*  *!*************************************************************
*!*  *! Called from : lfVryItem1()
*!*  *!*************************************************************
*!*  *! Calls       : lfPreData()
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  FUNCTION lfFixBatch
*!*  PRIVATE lcSetDelSt , llFixSom

*!*  llFixSom = .F.                  && Flag to know if we have fixed any records
*!*  lcSetDelSt = SET('DELETED')     && Varible to save the SET DELETED status
*!*  SET DELETED OFF

*!*  lcBatType = ''          && Varible to hold the Batch type
*!*  ldBatPBeg = {}          && Varible to hold the Batch beging poste date
*!*  ldBatPEnd = {}          && Varible to hold the Batch ending poste date
*!*  lnBatCnTot = 0          && Varible to hold the Batch Total Dibet

*!*  SET ORDER TO TAG BATCHNO  IN GLBATCH
*!*  SELECT GLTRNSHD
*!*  SET ORDER TO TAG BATCHTRN
*!*  SET RELATION TO cBatchNo INTO GLBATCH
*!*  GO TOP

*!*  lnCurRec = 0                && Varible to use for the thermometer
*!*  lnTotRec = RECCOUNT()       && Varible to use for the thermometer

*!*  =lfInitThermo(lnTotRec, 'Fixing Batches file')

*!*  *-- SCAN Loop to scan the Posted transaction header for the records that
*!*  *-- has Batch number and this Batch number dose not exist in the GLBATCH
*!*  *-- file or was deleted
*!*  SCAN FOR cBatchNo + cTranNo <> '000000' ;
*!*      .AND. (EOF('GLBATCH') .OR. DELETED('GLBATCH'))

*!*    llFixSom = .T.
*!*    
*!*    *-- IF The Batch number was deleted
*!*    IF DELETED('GLBATCH')
*!*      lnCurRec = lnCurRec + 1
*!*      SELECT GLBATCH
*!*      RECALL
*!*      SELECT GLTRNSHD
*!*    ELSE    && Else
*!*      =lfPreData()
*!*      SKIP -1
*!*      INSERT INTO GLBATCH (cBatchNo , cBatStat , lBatInd , cBatType ,;
*!*                           cBatPyr , dBatPBeg , dBatPEnd , nBatCnTot ,;
*!*                           nBaTotDR , nBaTotCR , cSrcModul , cComp_ID ,;
*!*                           cPostSess , cPostProg , cPostUser , dPostDate ,;
*!*                           cPostTime , cAdd_User , dAdd_Date , cAdd_Time);
*!*           VALUES (GLTRNSHD.cBatchNo , GLTRNSHD.cTrnStat , .F. , lcBatType ,;
*!*                   GLTRNSHD.cTrnPYr , ldBatPBeg , ldBatPEnd , lnBatCnTot ,;
*!*                   lnBatCnTot , lnBatCnTot , GLTRNSHD.cSrcModul ,;
*!*                   GLTRNSHD.cComp_ID , GLTRNSHD.cPostSess , GLTRNSHD.cPostProg ,;
*!*                   GLTRNSHD.cPostUser , GLTRNSHD.dPostDate , GLTRNSHD.cPostTime ,;
*!*                   GLTRNSHD.cAdd_User , GLTRNSHD.dAdd_Date , GLTRNSHD.cAdd_Time)
*!*    ENDIF    && End of IF
*!*    oProgress.CurrentProgress(lnCurRec)
*!*  ENDSCAN    && End of Scan Loop

*!*  *-- IF we had Fix any records
*!*  IF llFixSom
*!*    *-- FOR Loop to complete the thermometer
*!*    FOR lnRst = lnCurRec TO lnTotRec
*!*      oProgress.CurrentProgress(lnRst)
*!*    ENDFOR    && End of FOR Loop
*!*  ENDIF    && End of IF

*!*  oProgress.Visible = .F.

*!*  SET RELATION TO
*!*  SET ORDER TO
*!*  SET ORDER TO TAG BATSTAT IN GLBATCH
*!*  SET DELETED &lcSetDelSt
*!*  *-- End of lfFixBatch()

*!*  *!*************************************************************
*!*  *! Name      : lfSelData
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to get the Single transactions ,
*!*  *!             Beginning batches , Normal batches and Closing batches
*!*  *!             to be Reposted and repare the Temp. Balance file for
*!*  *!             Reposting
*!*  *!*************************************************************
*!*  *! Called from : lfVryItem1()
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : .T. if ther is any Batches or Transactions to be
*!*  *!               Reposted , .F. Otherwise
*!*  *!*************************************************************
*!*  FUNCTION lfSelData

*!*  PRIVATE lnPBatTran , lnTotal , lnCurrent , lcAccount , llBalShtAc

*!*  lnPBatTran = 0        && Varible to hold the number of Transactions in the Batches to be Reposted
*!*  lnPostTran = 0

*!*  CREATE TABLE (oAriaApplication.WorkDir + lcTmpBBtch) (cBatchNo C(6) , cBatStat C(1))
*!*  INDEX ON cBatchNo TAG CBATCHNO 
*!*  SET ORDER TO CBATCHNO      

*!*  CREATE TABLE (oAriaApplication.WorkDir + lcTmpJBtch) (cBatchNo C(6) , cBatStat C(1))
*!*  INDEX ON cBatchNo TAG CBATCHNO 
*!*  SET ORDER TO CBATCHNO      

*!*  CREATE TABLE (oAriaApplication.WorkDir + lcTmpCBtch) (cBatchNo C(6) , cBatStat C(1))
*!*  INDEX ON cBatchNo TAG CBATCHNO 
*!*  SET ORDER TO CBATCHNO      

*!*  CREATE TABLE (oAriaApplication.WorkDir + lcTmpTran) (cTranNo C(8) , cTrnStat C(1))
*!*  INDEX ON cTranNo TAG CTRANNO 

*!*  SELECT GLBATCH

*!*  *IF There is a posted Batches
*!*  IF SEEK('P')
*!*    *SCAN Loop to scan the GLBATCH file [The Batches file] FOR the posted
*!*    SCAN REST WHILE cBatStat = 'P'  FOR cBatPYr >= lcFiscYear
*!*      *DO CASE Statment
*!*      DO CASE
*!*        *Case of Closing Batches
*!*        CASE cBatType = 'C'
*!*          INSERT INTO &lcTmpCBtch (cBatchNo , cBatStat) VALUES;
*!*                                  (GLBATCH.cBatchNo , GLBATCH.cBatStat)
*!*        
*!*        *Case of Beginning Batches
*!*        CASE cBatType = 'B'
*!*          INSERT INTO &lcTmpBBtch (cBatchNo , cBatStat) VALUES;
*!*                                  (GLBATCH.cBatchNo , GLBATCH.cBatStat)
*!*        
*!*        OTHERWISE
*!*          INSERT INTO &lcTmpJBtch (cBatchNo , cBatStat) VALUES;
*!*                                  (GLBATCH.cBatchNo , GLBATCH.cBatStat)
*!*      
*!*      ENDCASE    && End of DO CASE Statment
*!*      
*!*    ENDSCAN    && End of SCAN Loop
*!*  ENDIF    && End of IF

*!*  SELECT GLTRNSHD

*!*  *SCAN Loop to scan the GLTRNSHD file [The Posted Transactios header file
*!*  *opened with the Alias GLTRNSHD] FOR Batch # = 0 and cTrnPYr > or = the
*!*  *Starting year
*!*  SCAN FOR cBatchNo + cTranNo = '000000' .AND. cTrnPYr >= lcFiscYear
*!*    lnPostTran = lnPostTran + 1
*!*    INSERT INTO &lcTmpTran (cTranNo , cTrnStat) VALUES;
*!*                           (GLTRNSHD.cTranNo , GLTRNSHD.cTrnStat)
*!*  ENDSCAN    && End of SCAN Loop

*!*  COUNT FOR cBatchNo + cTranNo <> '000000';
*!*       .AND. cTrnPYr >= lcFiscYear;
*!*         TO lnPBatTran
*!*  lnPostTran = lnPostTran + lnPBatTran

*!*  *IF The Temp. Balance file [Opened with the Alias GLACBALS] exist
*!*  IF USED('GLACBALS')
*!*    USE IN GLACBALS

*!*    *IF The file GLTMPBAL [Temp. Balance file] exist
*!*    IF FILE(lcFilePath + 'GLTMPBAL.DBF')
*!*      ERASE (lcFilePath + 'GLTMPBAL.DBF')
*!*    ENDIF    && End of IF

*!*    *IF The CDX file of GLTMPBAL [Temp. Balance file] exist
*!*    IF FILE(lcFilePath + 'GLTMPBAL.CDX')
*!*      ERASE (lcFilePath + 'GLTMPBAL.CDX')
*!*    ENDIF    && End of IF
*!*  ENDIF    && End of IF

*!*  SELECT GLTMPBAL
*!*  COPY STRUCTURE TO (lcFilePath + 'GLTMPBAL.DBF') WITH CDX
*!*  USE (lcFilePath + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0

*!*  SELECT GLTMPBAL
*!*  lnTotal   = RECCOUNT()        && Varible to hold the number of Account Balances file records [Opened with the Alias GLTMPBAL]
*!*  lnCurrent = 0               && Varible to hold the number of records that was finished

*!*  =lfInitThermo(lnTotal, 'Fixing Account balance file')

*!*  *-- SCAN Loop to Scan the Account Balances file 
*!*  LOCATE
*!*  SCAN
*!*    lcAccount = cAcctCode        && Varible to hold the current account code
*!*    =SEEK(cAcctCode , 'GLACCHAR')
*!*    llBalShtAc = IIF(LEFT(GLACCHAR.cTypeCode , 1) $ 'ALQY' , .T. , .F.)    && Flag to know if the account is one of the Balance shet accounts
*!*    
*!*    *-- SCAN Loop to Scan the Account Balances file for the current account
*!*    *-- and Fiscal year < the Starting from year
*!*    SCAN REST WHILE cAcctCode = lcAccount .AND. cFisFYear < lcFiscYear
*!*      lnCurrent = lnCurrent + 1
*!*      SCATTER MEMVAR MEMO
*!*      INSERT INTO GLACBALS FROM MEMVAR
*!*      oProgress.CurrentProgress(lnCurrent)
*!*    ENDSCAN    && End of SCAN Loop
*!*    
*!*    SKIP -1
*!*    IF lcAccount <> cAcctCode
*!*      lnClosBal = 0
*!*    ELSE  
*!*      lnClosBal = nAcbClBal
*!*    ENDIF
*!*    SCATTER FIELDS LIKE N* MEMVAR BLANK
*!*    
*!*    *-- IF We are at the Begin of the Account Balances file
*!*    IF BOF()
*!*      GO RECNO()
*!*    ELSE    && Else
*!*      SKIP 1
*!*    ENDIF    && End of IF
*!*    
*!*    *-- SCAN Loop to Scan the Account Balances file for the current account
*!*    *-- and Fiscal year > or = the Starting from year
*!*    SCAN REST WHILE cAcctCode = lcAccount
*!*      lnCurrent = lnCurrent + 1
*!*      SCATTER FIELDS EXCEPT N* MEMVAR MEMO
*!*      INSERT INTO GLACBALS FROM MEMVAR

*!*      *-- IF the account is one of the Balance shet accounts
*!*      IF llBalShtAc
*!*        REPLACE GLACBALS.nAcbOpBal WITH lnClosBal;
*!*                GLACBALS.nAcbClBal WITH lnClosBal
*!*      ENDIF    && End of IF
*!*      oProgress.CurrentProgress(lnCurrent)
*!*    ENDSCAN    && End of SCAN Loop
*!*    SKIP -1  
*!*  ENDSCAN    && End of SCAN Loop

*!*  oProgress.Visible = .F.

*!*  RETURN lnPostTran <> 0
*!*  *-- End of lfSelData()

*!*  *!*************************************************************
*!*  *! Name      : lfRmTmpBal
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to delete the GLTMPBAL file and the
*!*  *!             GLRBAL.MEM memory variable file
*!*  *!*************************************************************
*!*  *! Called from : lfVryItem1()
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  FUNCTION lfRmTmpBal

*!*  lcRBalYear = lcFiscYear
*!*  lnPostTran = 0

*!*  *IF The Alias GLACBALS exist [That was used for the Temp. balance file]
*!*  IF USED('GLACBALS')
*!*    USE IN GLACBALS
*!*  ENDIF    && End of IF

*!*  *IF The file GLTMPBAL exist [The Temp. balance file]
*!*  IF FILE(lcFilePath + 'GLTMPBAL.DBF')
*!*    ERASE (lcFilePath + 'GLTMPBAL.DBF')
*!*  ENDIF    && End of IF

*!*  *IF The CDX file of GLTMPBAL [Temp. Balance file] exist
*!*  IF FILE(lcFilePath + 'GLTMPBAL.CDX')
*!*    ERASE (lcFilePath + 'GLTMPBAL.CDX')
*!*  ENDIF    && End of IF

*!*  *IF The memory variable file exist
*!*  IF FILE(lcFilePath + 'GLRBAL.MEM')
*!*    ERASE (lcFilePath + 'GLRBAL.MEM')
*!*  ENDIF    && End of IF
*!*  *-- End of lfRmTmpBal()

*!*  *!*************************************************************
*!*  *! Name      : lfPreData
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to get the values for cBatType , nBatCnTot
*!*  *!             dBatPBeg , dBatPEnd
*!*  *!*************************************************************
*!*  *! Called from : lfFixBatch()
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  FUNCTION lfPreData

*!*  PRIVATE lcFisYear , lcMinPrd , lcMaxPrd , lcBatchNo

*!*  *DO CASE Statment to get the Batch type
*!*  DO CASE
*!*    *Case The transaction type is 'Y'
*!*    CASE GLTRNSHD.cTrnType = 'Y'
*!*      lcBatType = 'C'
*!*    
*!*    *Case The transaction type is 'B'
*!*    CASE GLTRNSHD.cTrnType = 'B'
*!*      lcBatType = 'B'
*!*    
*!*    *Otherwise
*!*    OTHERWISE
*!*      *IF the accounts in the Transactions lines for this Batch is
*!*      *Statystical account
*!*      IF SEEK(GLTRNSHD.cBatchNo , 'GLTRNSDT') .AND.;
*!*         SEEK(GLTRNSDT.cAcctCode , 'GLACCHAR') .AND.;
*!*         LEFT(GLACCHAR.cTypeCode , 1) = 'Y'
*!*        lcBatType = 'S'
*!*      ELSE    && Else
*!*        lcBatType = 'N'
*!*      ENDIF    && End of IF  
*!*  ENDCASE    && End of CASE Statment

*!*  lcBatchNo = cBatchNo      && Varible to hold the Batch number
*!*  lcFisYear = cTrnPYr       && Varible to hold the Fiscal year
*!*  lcMinPrd = cTrnPPrd       && Varible to hold the Min. Prioud
*!*  lcMaxPrd = cTrnPPrd       && Varible to hold the Max. Prioud
*!*  lnBatCnTot = 0

*!*  *SCAN Loop to scan the Posted transaction header for the current Batch
*!*  SCAN REST  WHILE cBatchNo = lcBatchNo
*!*    lnCurRec = lnCurRec + 1
*!*    lcMinPrd = MIN(lcMinPrd , cTrnPPrd)
*!*    lcMaxPrd = MAX(lcMaxPrd , cTrnPPrd)
*!*    lnBatCnTot = lnBatCnTot + nTrnTotDR
*!*  ENDSCAN    && End of SCAN Loop

*!*  ldBatPBeg = IIF(SEEK(lcFisYear + lcMinPrd , 'FSPRD') ,;
*!*                  FSPRD.dFspPBgDt , {})

*!*  ldBatPEnd = IIF(SEEK(lcFisYear + lcMaxPrd , 'FSPRD') ,;
*!*                  FSPRD.dFspPEnDt , {})

*!*  *-- End of lfPreData()

*!*  *!*************************************************************
*!*  *! Name      : lfUpdItem1
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Update master file of (lcRpItem1)
*!*  *!*************************************************************
*!*  *! Called from : Screen GLRBAL
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  FUNCTION lfUpdItem1

*!*  PRIVATE lnCurPost
*!*  lnCurPost = 0          && Varible to hold the number of Transactions posted with cTrnPYr > or = Starting year

*!*  *IF the Rebalance process was done befor in another session without Updating
*!*  *the master file
*!*  IF llBalnBfor
*!*    SELECT GLTRNSHD
*!*    COUNT FOR cTrnPYr >= lcRpFiscYr TO lnCurPost
*!*    
*!*    *IF the number of Transactions posted with cTrnPYr > or = Starting year
*!*    *dose not equal the the number of transactions that was Reposted in the
*!*    *Rebalance process
*!*    IF lnCurPost <> lnPostTran
*!*      
*!*      * Meesage : "One or more transaction were posted since the last "
*!*      *           "recaculation of balances. You need to run the      "
*!*      *           "recalculation process again.                       " 
*!*      * Buttons :                     <Ok>
*!*      DECLARE laRebMsg[5]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "One or more transaction were posted since the last "
*!*      laRebMsg[3] = "recaculation of balances. You need to run the "
*!*      laRebMsg[4] =  "recalculation process again.                       " 
*!*      laRebMsg[5] = " "

*!*      IF llAutoBal
*!*        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF

*!*      =lfVryRport()
*!*      =lfRmTmpBal()
*!*      RETURN
*!*    ENDIF    && End of IF
*!*    
*!*    SELECT GLACBALS
*!*    SET ORDER TO TAG ACCYRPRD
*!*    SET RELATION TO cAcctCode INTO GLACCHAR
*!*    LOCATE FOR EOF('GLACCHAR')
*!*    
*!*    *IF There is an account that exist in the Temp. balance file and dose
*!*    *not exist in the GLACCHAR file [Chart of accounts file]
*!*    IF FOUND()
*!*      SET RELATION TO
*!*      * Meesage : "Found mismatch between the current chart of account's, and"
*!*      *           "the recalculated balances file. You need to run the       "
*!*      *           "recalculation process again.                              "
*!*      * Buttons :                         <Ok>

*!*      DECLARE laRebMsg[5]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "Found mismatch between the current chart of account's, and"
*!*      laRebMsg[3] = "the recalculated balances file. You need to run the       "
*!*      laRebMsg[4] = "recalculation process again. " 
*!*      laRebMsg[5] = " "

*!*      IF llAutoBal
*!*        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF

*!*      =lfVryRport()
*!*      =lfRmTmpBal()
*!*      RETURN
*!*    ENDIF    && End of IF
*!*    
*!*    SET RELATION TO
*!*    SELECT GLACCHAR
*!*    SET RELATION TO cAcctCode INTO GLACBALS
*!*    LOCATE FOR EOF('GLACBALS')
*!*    
*!*    *IF There is an account that exist in the GLACCHAR file [Chart of accounts
*!*    *file] and dose not exist in the Temp. balance file
*!*    IF FOUND()
*!*      SET RELATION TO
*!*      
*!*      * Meesage : "Found mismatch between the current chart of account's, and"
*!*      *           "the recalculated balances file. You need to run the       "
*!*      *           "recalculation process again.                              "
*!*      * Buttons :                     <Ok>
*!*      DECLARE laRebMsg[5]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "Found mismatch between the current chart of account's, and"
*!*      laRebMsg[3] = "the recalculated balances file. You need to run the"
*!*      laRebMsg[4] = "recalculation process again.       "
*!*      laRebMsg[5] = " "

*!*      IF llAutoBal
*!*        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF

*!*      =lfVryRport()
*!*      =lfRmTmpBal()
*!*      RETURN
*!*    ENDIF    && End of IF
*!*    SET RELATION TO
*!*  ENDIF    && End of IF

*!*  IF !FILE(lcFilePath + 'GLTMPBAL.DBF') 
*!*    RETURN
*!*  ENDIF

*!*  *IF The user selected to cancel
*!*  IF !llAutoBal
*!*    * Meesage : "We strongly recommend that a backup is done for company � data"
*!*    *           "files immediately prior to the � process.  Do you wish to     "
*!*    *           "continue?                                                     "
*!*    * Buttons :                     <Proceed>    <Cancel>
*!*    IF gfModalGen("TRM02255B00012" , "ALERT" , lcCurrComp_ID + '|Update') = 2
*!*      RETURN
*!*    ENDIF    && End of IF
*!*  ENDIF  

*!*  IF USED('GLACBALS')
*!*    USE IN GLACBALS
*!*  ENDIF
*!*  IF USED('GLTMPBAL')
*!*    USE IN GLTMPBAL
*!*  ENDIF
*!*  RENAME (lcFilePath + 'GLACBALS.DBF') TO (lcFilePath + 'TMPNAME.DBF')
*!*  RENAME (lcFilePath + 'GLACBALS.CDX') TO (lcFilePath + 'TMPNAME.CDX')
*!*  RENAME (lcFilePath + 'GLTMPBAL.DBF') TO (lcFilePath + 'GLACBALS.DBF')
*!*  RENAME (lcFilePath + 'GLTMPBAL.CDX') TO (lcFilePath + 'GLACBALS.CDX')

*!*  ERASE (lcFilePath + 'TMPNAME.DBF')
*!*  ERASE (lcFilePath + 'TMPNAME.CDX')

*!*  *IF The memory variable file exist
*!*  IF FILE(lcFilePath + 'GLRBAL.MEM')
*!*    ERASE (lcFilePath + 'GLRBAL.MEM')
*!*  ENDIF    && End of IF
*!*  USE (lcFilePath + 'GLACBALS.DBF') ALIAS GLTMPBAL ORDER TAG ACCYRPRD IN 0
*!*  *--- End of lfUpdItem1()

*!*  *!*************************************************************
*!*  *! Name      : lfGetCurYr
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Get the cuurent year. This varuable was defined before in   
*!*  *!             GL module.   
*!*  *!*************************************************************
*!*  FUNCTION lfGetCurYr

*!*  =gfSysOpen(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'CCOMP_ID','SH')
*!*  lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),sycComp.cCurr_Yer,'')
*!*  lnCurr_yer = INT(VAL(lcCurr_yer))
*!*  =gfSysClose("SYCCOMP")

*!*  =gfOpenFile(lcFilePath+'ACCOD', '' ,'SH')
*!*  SELECT ACCOD
*!*  GO TOP
*!*  lcAcsMask  = ACCOD.cAcsMask
*!*  lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
*!*  lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
*!*  lnAcsSegSz = ACCOD.nAcsSegSz 
*!*  =gfCloseFile("ACCOD")

*!*************************************************************
*! Function  : lfVryRport
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : - Create a low level text file (ReBalLog.txt). 
*!             - Append rebalance notes to rebalance log file. 
*!*************************************************************
FUNCTION lfVryRport

IF !llOpenRep  
  llOpenRep = .T.
  *! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
  *lcFilHandl = FCREAT(oAriaApplication.WorkDir + 'ReBalLog.txt')  
  IF TYPE('lcXMLFileName') = 'C'
    lcFilHandl = FCREAT(lcOutPutFile)
  ELSE
    lcFilHandl = FCREAT(oAriaApplication.WorkDir + 'ReBalLog.txt')
  ENDIF
  *! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]
ENDIF
lnRebMsg = ALEN(laRebMsg,1) 

*B610750,3 TMI 06/23/2014 10:35 [Start] if lcFilHandl = -1 , go out
IF lcFilHandl < 0
  RETURN 
ENDIF   
*B610750,3 TMI 06/23/2014 10:35 [End  ] 

FOR lnCount = 1 TO lnRebMsg  
  =FPUTS(lcFilHandl,laRebMsg[lnCount])
ENDFOR  
*-- End of lfVryRport()

*!*************************************************************
*! Function  : lfViewRLog
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Preview rebalance log file (ReBalLog.txt).
*!*************************************************************
FUNCTION lfViewRLog

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("lfViewRLog ")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 

LOCAL lnAlias
lnAlias = SELECT(0)

*-- If there is a report created dispaly it to the user
IF llViewLog OR llOpenRep
  IF !llOpenRep
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = LANG_SMREBAL_NOERR
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF
  =FFLUSH(lcFilHandl)

  *-- Close the text file
  *B610750,3 TMI 06/23/2014 10:36 [Start] check that handle is value
  IF lcFilHandl>0
    *B610750,3 TMI 06/23/2014 10:36 [End  ] 
  DO WHILE !FCLOSE(lcFilHandl)
  ENDDO
    *B610750,3 TMI 06/23/2014 10:36 [Start] 
  ENDIF 
  *B610750,3 TMI 06/23/2014 10:36 [End  ] 

  **B610750,1 * TMI 06/18/2014 16:33 [Start] we used the lcOutPutFile for RB, no need for the following in RB 
  IF TYPE('lcXMLFileName') <> 'C'
    **B610750,1 * TMI 06/18/2014 17:42 [End  ] 
  *-- Close the text file

  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (oAriaApplication.WorkDir + 'ReBalLog.txt') OVERWRITE
  SCATTER MEMVAR MEMO
  APPEND BLANK
  GATHER MEMVAR MEMO 
  LOCATE
  
  REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
                       LANG_SMREBAL_REBVERI + CHR(13) +;
                       REPLICATE('*',68) + CHR(13) + ' ' + CHR(13) + ' ' +;
                       mStrRep
    **B610750,1 * TMI 06/18/2014 16:37 [Start] 
  ENDIF 
  **B610750,1 * TMI 06/18/2014 16:37 [End  ] 
  
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]                     
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    DO FORM (oAriaApplication.ScreenHome + 'SM\SMSTRREP')
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
  *B610750,3 TMI 06/23/2014 10:38 [Start] 
  IF TYPE('lcXMLFileName') <> 'C'
    *B610750,3 TMI 06/23/2014 10:38 [End  ] 
  USE IN TMPSTR
    *B610750,3 TMI 06/23/2014 10:38 [Start] 
  ENDIF 
  *B610750,3 TMI 06/23/2014 10:38 [End  ] 
ELSE
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]                     
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if not called from style screen then show this message 'Finished Rebalance', else don't show
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 
  
    =gfModalGen("TRM00366B00000" , "DIALOG")   
      
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF 
    *E303410,1 TMI 09/01/2013 [End  ] 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]     
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Function  : lfwShowRep
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Show report
*!*************************************************************
FUNCTION lfwShowRep

LOCAL lnDataSess
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

lcRpFiscYr = IIF(SEEK(oAriaApplication.ActiveCompanyID, lcSYCCOMP), &lcSYCCOMP..cCurr_Yer, '')
lcFiscYear = lcRpFiscYr

SET DATASESSION TO (lnDataSess)
RETURN 

*----------------- Not in this Release -------------------*
*                                                         *
*                                                         *
*!*  *!*************************************************************
*!*  *! Name      : lfVryItem2
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Verify function for AP vendor  (lcRpItem2)
*!*  *!*************************************************************
*!*  FUNCTION lfVryItem2
*!*  PRIVATE laStructur , lnCount

*!*  *-- Set the Order Tags for all needed files
*!*  SET ORDER TO TAG VENCODE  IN APRBALV
*!*  SET ORDER TO TAG VENDYEAR IN APRBALVH
*!*  SET ORDER TO TAG VENCODE  IN APVENDOR
*!*  SET ORDER TO TAG VENDYEAR IN APVENHST
*!*  SET ORDER TO TAG VENDINV  IN APINVHDR
*!*  SET ORDER TO TAG INVVEND  IN APDIST

*!*  SELECT APVENDOR

*!*  *-- If We are going to recalculate the balances for all Vendors
*!*  lnTotal = RECCOUNT()              && These variable is for the thermometer
*!*  lnCurrent = 0                     && These variable is for the thermometer

*!*  =lfInitThermo(lnTotal, 'Recalculating balances...')

*!*  *-- SCAN Loop to scan the Vendor file
*!*  SCAN
*!*    =lfRbalVend()
*!*    lnCurrent = lnCurrent  + 1
*!*    oProgress.lblSecondLabel.Caption = 'Vendor : ' + ALLTRIM(APVENDOR.cVendCode)
*!*    oProgress.CurrentProgress(lnCurrent)
*!*    SELECT APVENDOR
*!*  ENDSCAN    && End of SCAN Loop to scan the Vendor file
*!*    
*!*  *-- If The thermometer is not finished
*!*  IF lnCurrent < lnTotal
*!*    *-- FOR Loop to finish the thermometer
*!*    FOR lnCount = lnCurrent TO lnTotal
*!*      oProgress.CurrentProgress(lnCount)
*!*    ENDFOR    && End of FOR Loop to finish the thermometer
*!*  ENDIF

*!*  oProgress.Visible = .F.
*!*  *-- End of lfVryItem2()

*!*  *!*************************************************************
*!*  *! Name      : lfRbalVend
*!*  *! Developer : Amin Khodary Amin
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to recalculate the Vendor balances
*!*  *!*************************************************************
*!*  *! Called from : lfVryItem2()
*!*  *!*************************************************************
*!*  FUNCTION lfRbalVend

*!*  PRIVATE lcPeriod , lcField

*!*  *-- Initialize OpenDebit variables
*!*  lnInvAmnt = 0       
*!*  lnInvPaid = 0 
*!*  lnInvDisTk= 0 
*!*  lnInvAdj  = 0 

*!*  SCATTER MEMVAR MEMO
*!*  M.NVENBAL = 0
*!*  M.NVENCPUR = 0
*!*  M.NVENCPAY = 0
*!*  M.NVENOPNDR = 0

*!*  SELECT APRBALV

*!*  *-- If The Vendor code dose not exist in the Vendor rebalance file
*!*  IF !SEEK(M.cVendCode)
*!*    APPEND BLANK
*!*  ENDIF    && End of IF The Vendor code dose not exist in the Vendor rebalance file
*!*  GATHER MEMVAR MEMO

*!*  SELECT APVENHST
*!*  SCATTER MEMVAR MEMO BLANK
*!*  M.cVendCode = APVENDOR.cVendCode

*!*  SELECT FISHD
*!*  GO TOP

*!*  *-- SCAN Loop to scan the Fiscal Calendar file for the current company
*!*  SCAN 
*!*    M.cFisFYear = cFisFYear
*!*    SELECT APRBALVH
*!*    *If The Vendor code and Fiscal year dose not exist in the Vendor History
*!*    *rebalance file insert it. 
*!*    IF !SEEK(M.cVendCode + M.cFisFYear)
*!*      APPEND BLANK
*!*    ENDIF    && End of IF The Vendor code and Fiscal year dose not exist
*!*    GATHER MEMVAR MEMO
*!*    SELECT FISHD
*!*  ENDSCAN    && End of SCAN Loop to scan the Fiscal Calendar file for the current company

*!*  SELECT APINVHDR

*!*  *-- If Statment to replace the record pointer in the first record for this
*!*  *-- Vendor in the Invoice Header file
*!*  IF SEEK(M.cVendCode)
*!*    *-- SCAN Loop To scan the Vendor invoices in the Invoice Header file
*!*    *-- with status <> Void
*!*    SCAN REST;
*!*        WHILE cVendCode = M.cVendCode;
*!*          FOR cInvStat <> 'V'
*!*      lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt)  
*!*      
*!*      *--If Not af advance payment, update cumulative purchases
*!*      IF APINVHDR.cInvStat <> 'A'
*!*        SELECT APRBALV
*!*        REPLACE nVenCPur WITH nVenCPur + lnInvAmt

*!*        *-- Update Vendor History file for APINVHDR.cFisfYear + APINVHDR.cFspPrdId
*!*        SELECT APRBALVH
*!*        
*!*        *-- If Statment to replace the record pointer in the correct record for
*!*        *-- this Vendor and Fiscal Year in the Vendor History file
*!*        IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*          lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
*!*          REPLACE nVnhPurch        WITH nVnhPurch + lnPurchAmt ,;
*!*                  nVnhPur&lcPeriod WITH nVnhPur&lcPeriod + lnPurchAmt 
*!*        ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
*!*      ELSE       && APINVHDR.cInvStat <> 'A'
*!*        *-- Calculate Open debit 
*!*        lnInvAmnt = 0       
*!*        lnInvPaid = 0 
*!*        lnInvDisTk= 0 
*!*        lnInvAdj  = 0 
*!*        lcExSin2   = ''
*!*        lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
*!*        lnInvAmnt = lnInvAmnt + ROUND(APINVHDR.nInvAmnt   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
*!*        lnInvPaid = lnInvPaid + ROUND(APINVHDR.nInvPaid   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
*!*        lnInvDisTk= lnInvDisTk+ ROUND(APINVHDR.nInvDisTk  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)      
*!*        lnInvAdj  = lnInvAdj  + ROUND(APINVHDR.nInvAdj    &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)            
*!*      ENDIF    && End of IF Not an advance payment, update cumulative purchases
*!*    
*!*      *Update vendor balances if the invoice is not a credit card payment
*!*      *If Not credit card payment
*!*      IF APINVHDR.cVenPMeth <> 'C'       
*!*        *Update Vendor file
*!*        SELECT APRBALV
*!*        REPLACE nVenBal WITH nVenBal + lnInvAmt - lnAmtPaid;
*!*                                     - lnDiscTakn - lnAdjtAmt 

*!*        *Update Vendor History file
*!*        SELECT APRBALVH
*!*        *If Statment to replace the record pointer in the correct record for
*!*        *this Vendor and Fiscal Year in the Vendor History file
*!*        IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*          REPLACE nVnhDisOf  WITH nVnhDisOf +  lnDiscOffr ,;
*!*                  nVnhDisTkn WITH nVnhDisTkn + lnDiscTakn ,;
*!*                  nVnhAdj    WITH nVnhAdj + lnAdjtAmt
*!*        ENDIF    && SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*        
*!*        *Update payments from APDIST (including advance payments)
*!*        SELECT APDIST
*!*        =SEEK(APINVHDR.cInvNo + M.cVendCode)
*!*        SCAN REST;
*!*            WHILE cInvNo + cVendCode = APINVHDR.cInvNo + M.cVendCode ;
*!*              FOR cAPdStat<> 'V'
*!*          *If the record is a payment record (cApdActId = 'C') 
*!*          IF cApdActId = 'C'
*!*            *Update Vendor file
*!*            SELECT APRBALV
*!*            REPLACE nVenCPay WITH nVenCPay -  APDIST.nEqvAmnt 
*!*            *Update Vendor History file for APDIST.cFisfYear + APDIST.cFspPrdId
*!*            SELECT APRBALVH
*!*            *If Statment to replace the record pointer in the correct record
*!*            *for this Vendor and Fiscal Year in the Vendor History file
*!*            IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*              lcPeriod = ALLTRIM(STR(VAL(APDIST.cFspPrdId)))
*!*              lcField = IIF(APDIST.cApdTrTyp = 'P' , 'nVnhPChkP' ,;
*!*                        IIF(APDIST.cApdTrTyp = 'M' , 'nVnhMChkP' ,;
*!*                        IIF(APDIST.cApdTrTyp = 'N' , 'nVnhNChkP' ,;
*!*                            'nVnhCashP' )))
*!*              REPLACE nVnhTotPa        WITH nVnhTotPa - APDIST.nEqvAmnt  ,;
*!*                      &lcField         WITH &lcField  - APDIST.nEqvAmnt ,;
*!*                      nVnhPay&lcPeriod WITH nVnhPay&lcPeriod -  APDIST.nEqvAmnt 
*!*                      
*!*            ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
*!*          ENDIF    && End IF cApdActId = 'C'
*!*          
*!*          *If the record is an application record (applied bdebit memo)
*!*          IF APDIST.cApdTrTyp = 'A' .AND. cApdActID = 'A' .AND. nApdAmnt < 0
*!*            
*!*            *Update Vendor History file for APDIST.cFisfYear
*!*            SELECT APRBALVH
*!*            *If Statment to replace the record pointer in the correct record
*!*            *for this Vendor and Fiscal Year in the Vendor History file
*!*            IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*              REPLACE nVnhDMApP WITH nVnhDMApP + - APDIST.nEqvAmnt ,;
*!*                      nVnHAdj   WITH nVnHAdj   +  lnAdjtAmt 
*!*            ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
*!*          ENDIF    && End IF APDIST.cApdTrTyp = 'A' ,,,
*!*          
*!*          SELECT APDIST
*!*        ENDSCAN    && End of SCAN Loop To scan the Vendor invoices lines in the Dist. file with status <> Void
*!*      
*!*      ELSE      && IF Credit card payment, update payments
*!*      
*!*        *Update Vendor file
*!*        SELECT APRBALV
*!*        REPLACE nVenCPay WITH nVenCPay + lnInvAmt 
*!*    
*!*        
*!*        *Update Vendor History file
*!*        SELECT APRBALVH
*!*        
*!*        *If Statment to replace the record pointer in the correct record
*!*        *for this Vendor and Fiscal Year in the Vendor History file
*!*        IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
*!*          lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
*!*          REPLACE nVnhTotPa        WITH nVnhTotPa + lnInvAmt ,;
*!*                  nVnhCCPay        WITH nVnhCCPay + lnInvAmt ,;
*!*                  nVnhPay&lcPeriod WITH nVnhPay&lcPeriod + lnInvAmt
*!*        ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
*!*      ENDIF    && End of IF Not credit card payment
*!*      
*!*      SELECT APINVHDR
*!*    ENDSCAN    && End of SCAN Loop To scan the Vendor invoices in the Invoice Header file
*!*  ENDIF    && End of IF Statment to replace the record pointer in the first record for this Vendor in the Invoice Header file

*!*  ** Update OpenDebit value in Vendor Master
*!*  SELECT APRBALV      
*!*  IF SEEK(M.cVendCode)
*!*     REPLACE nVenOpnDR WITH -lnInvAmnt  + lnInvPaid + ;
*!*                            lnInvDisTk + lnInvAdj  
*!*    IF ApVendor.nVenOpnDr <> nVenOpnDr AND !llUpdt
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "Company" + lcCurrComp_ID +": Vendor ("+ ALLTRIM(M.cVendCode) + ") has wrong open debit amount ."
*!*      laRebMsg[3] = " "

*!*      IF llAutoBal
*!*        laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF

*!*      =lfVryRport()
*!*    ENDIF

*!*    IF ApVendor.nVenBal <> nVenBal AND !llUpdt
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "Company" + lcCurrComp_ID +": Vendor ("+ ALLTRIM(M.cVendCode) + ") has wrong current balance."
*!*      laRebMsg[3] = " "

*!*      IF llAutoBal
*!*        laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF

*!*      =lfVryRport()
*!*    ENDIF

*!*  ENDIF  

*!*  * Initialize OpenDebit variables
*!*  lnInvAmnt = 0       
*!*  lnInvPaid = 0 
*!*  lnInvDisTk= 0 
*!*  lnInvAdj  = 0 

*!*  SELECT APINVHDR
*!*  *--- End of lfRbalVend()

*!*  *!*************************************************************
*!*  *! Name      : lfStupItm2
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Additional AP setup that needs to process AP rebalance
*!*  *! Purpose   : of item  (lcRpItem2)
*!*  *!*************************************************************
*!*  *! Example   : = lfStupItm2()
*!*  *!*************************************************************
*!*  FUNCTION  lfStupItm2

*!*  PRIVATE lcCurrArea
*!*  lcCurrArea = SELECT(0)

*!*  llVerify = .T.
*!*  DECLARE laStructur[1,4]

*!*  *If The Vendor rebalance file is not opened
*!*  IF !USED('APRBALV')
*!*    SELECT APVENDOR
*!*    IF !FILE(lcFilePath + 'APRBALV.DBF' )
*!*      =AFIELDS(laStructur)
*!*      CREATE TABLE (lcFilePath + 'APRBALV' ) FROM ARRAY laStructur
*!*      INDEX ON cVendCode TAG VENCODE
*!*      USE 
*!*      llVerify = .F.
*!*    ENDIF
*!*    SELECT 0
*!*    USE (lcFilePath + 'APRBALV' ) 
*!*  ENDIF    && End of IF The Vendor rebalance file is not opened

*!*  *If The Vendor History rebalance file is not opened
*!*  IF !USED('APRBALVH')
*!*    SELECT APVENHST
*!*    IF !FILE(lcFilePath + 'APRBALVH.DBF' ) 
*!*      =AFIELDS(laStructur)
*!*      CREATE TABLE (lcFilePath + 'APRBALVH' ) FROM ARRAY laStructur
*!*      INDEX ON cVendCode + cFisFYear TAG VENDYEAR
*!*      USE
*!*      llVerify = .F.
*!*    ENDIF
*!*    SELECT 0 
*!*    USE (lcFilePath + 'APRBALVH')      && ApVenHst
*!*  ENDIF    && End of IF The Vendor History rebalance file is not opened
*!*  IF &lcRebalHdr..cUpdVryIgn='U' AND !llVerify 
*!*    llUpdt = .T.
*!*    =lfVryItem2()
*!*  ENDIF
*!*  SELECT (lcCurrArea)
*!*  *-- End of lfStupItm2()

*!*  *!*************************************************************
*!*  *! Name      : lfValdItm2
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Check whether all mandatory factors that be used in
*!*  *!             AP are defined or not. 
*!*  *!*************************************************************
*!*  *! Example   : =lfValdItm2()
*!*  *!*************************************************************
*!*  FUNCTION  lfValdItm2
*!*  RETURN .T.
*!*  *--- End of lfValdItm2() 

*!*  *!*************************************************************
*!*  *! Name      : lfUpdItem2
*!*  *! Developer : Amin Khodary Amin (AKA)
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Update Fuction for A/P.
*!*  *!*************************************************************
*!*  *! Example   : =lfUpdItem2()
*!*  *!*************************************************************

*!*  FUNCTION  lfUpdItem2
*!*  PRIVATE llNotUpDat , lcVendor , lnCount

*!*  *Set the Order Tags for all needed files
*!*  SET ORDER TO TAG VENCODE IN APRBALV
*!*  SET ORDER TO TAG VENDYEAR IN APRBALVH
*!*  SET ORDER TO TAG VENCODE IN APVENDOR
*!*  SET ORDER TO TAG VENDYEAR IN APVENHST
*!*  SET ORDER TO TAG VENDINV IN APINVHDR
*!*  SET ORDER TO TAG INVVEND IN APDIST

*!*  *** To get the number of records in calender year table withoud deleted records
*!*  SELECT FISHD
*!*  COUNT TO lnFisYerNo FOR !DELETED()  &&  Get no. of calender years

*!*  llNotUpDat = .F.                             && Flag to know if one or more Vendors was not updated

*!*  SELECT APRBALV
*!*  lnTotal = RECCOUNT() * lnFisYerNo            && These variable is for the Thermometer
*!*  lnCurrent = 0                                && These variable is for the Thermometer
*!*  =lfInitThermo(lnTotal, 'Updating balances...')

*!*  *-- SCAN Loop to scan the Vendor rebalance file
*!*  SCAN
*!*    *-- If The Vendor time stamp is not changed
*!*    IF SEEK(cVendCode , 'APVENDOR') .AND. ;
*!*       (dAdd_Date = APVENDOR.dAdd_Date .AND. cAdd_Time = APVENDOR.cAdd_Time)

*!*      SELECT APVENHST

*!*      *-- SCAN Loop to scan the Vendor records in the Vendor History file
*!*      SCAN FOR cVendCode + cFisFYear = APVENDOR.cVendCode
*!*      
*!*        BLANK
*!*        DELETE
*!*      ENDSCAN    && End of SCAN Loop to scan the Vendor records in the Vendor History file

*!*      SET DELETED OFF

*!*      SELECT FISHD 
*!*      GO TOP

*!*      SCAN FOR !DELETED()

*!*        SELECT APRBALVH
*!*        *-- If There is a record for this Fiscal Year and Vendor in the
*!*        *-- Vendor History rebalance file
*!*        IF SEEK(APVENDOR.cVendCode + FISHD.cFisFYear)
*!*          SCATTER MEMVAR MEMO
*!*        ELSE    && Else [IF There is no records for this Fiscal Year and Vendor]
*!*          SCATTER MEMVAR MEMO BLANK
*!*          M.cVendCode = APVENDOR.cVendCode
*!*          M.cFisFYear = FISHD.cFisFYear
*!*        ENDIF    && End of IF There is a record for this Fiscal Year and Vendor
*!*        
*!*        SELECT APVENHST
*!*        
*!*        *-- If There is a deleted record in the Vendor History file
*!*        IF SEEK(SPACE(12))
*!*          RECALL
*!*        ELSE    && Else [IF There is no deleted record in the Vendor History file]
*!*          APPEND BLANK
*!*        ENDIF    && End of IF There is a deleted record in the Vendor History file
*!*        
*!*        GATHER MEMVAR MEMO
*!*        =gfAdd_Info('APVENHST')
*!*        
*!*        lnCurrent = lnCurrent  + 1
*!*        oProgress.lblSecondLabel.Caption = 'Vendor : ' + ALLTRIM(APRBALV.cVendCode)
*!*        oProgress.CurrentProgress(lnCurrent)

*!*        SELECT FISHD
*!*        
*!*      ENDSCAN    && End of SCAN Loop to scan the Fiscal calendar for this company
*!*      
*!*      SET DELETED ON
*!*      
*!*      SELECT APRBALV
*!*      SCATTER MEMVAR MEMO
*!*      SELECT APVENDOR
*!*      GATHER MEMVAR MEMO
*!*      =gfAdd_Info('APVENDOR')
*!*      SELECT APRBALV
*!*      DELETE
*!*    ELSE    && Else [IF The Vendor time stamp was changed]
*!*      llNotUpDat = .T.
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = "Company" + lcCurrComp_ID +": "+ ALLTRIM(cVendCode) + " information has changed since rebalancing. Balances are not updated."
*!*      laRebMsg[3] = " "
*!*      IF llAutoBal
*!*        laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
*!*      ENDIF
*!*      =lfVryRport()

*!*      lcVendor = ALLTRIM(APVENDOR.cVendCode)          && Variable to hold the current Vendor code
*!*    ENDIF    && End of IF The Vendor time stamp is not changed
*!*    SELECT APRBALV
*!*  ENDSCAN    && End of SCAN Loop to scan the Vendor rebalance file

*!*  *-- If The thermometer is not finished
*!*  IF lnCurrent < lnTotal
*!*    *-- FOR Loop to finish the thermometer
*!*    FOR lnCount = lnCurrent TO lnTotal
*!*      oProgress.CurrentProgress(lnCount)
*!*    ENDFOR    && End of FOR Loop to finish the thermometer
*!*  ENDIF    && End of IF The thermometer is not finished

*!*  oProgress.Visible = .F.

*!*  =lfErsVBalF()

*!*  *!*************************************************************
*!*  *! Name      : lfErsVBalF
*!*  *! Developer : Haytham El_Sheltawi
*!*  *! Date      : 02/11/1998
*!*  *! Purpose   : Function to Erases the Vendor rebalance file
*!*  *!             and the Vendor History rebalance file
*!*  *!*************************************************************
*!*  *! Called from : lfvRBalnce()
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  *
*!*  FUNCTION lfErsVBalF


*!*  *If The Vendor rebalance file is open
*!*  IF USED('APRBALV')
*!*    USE IN APRBALV
*!*  ENDIF    && End of IF The Vendor rebalance file is open

*!*  *If The APRBALV.DBF file exist
*!*  IF FILE(lcFilePath + 'APRBALV.DBF')
*!*    ERASE (lcFilePath + 'APRBALV.DBF')
*!*  ENDIF    && End of IF The APRBALV.DBF file exist

*!*  *If The APRBALV.FPT file exist
*!*  IF FILE(lcFilePath + 'APRBALV.FPT')
*!*    ERASE (lcFilePath + 'APRBALV.FPT')
*!*  ENDIF    && End of IF The APRBALV.FPT file exist

*!*  *If The APRBALV.CDX file exist
*!*  IF FILE(lcFilePath + 'APRBALV.CDX')
*!*    ERASE (lcFilePath + 'APRBALV.CDX')
*!*  ENDIF    && End of IF The APRBALV.CDX file exist


*!*  *If The Vendor History rebalance file is open
*!*  IF USED('APRBALVH')
*!*    USE IN APRBALVH
*!*  ENDIF    && End of IF The Vendor History rebalance file is open

*!*  *If The APRBALVH.DBF file exist
*!*  IF FILE(lcFilePath + 'APRBALVH.DBF')
*!*    ERASE (lcFilePath + 'APRBALVH.DBF')
*!*  ENDIF    && End of IF The APRBALVH.DBF file exist

*!*  *If The APRBALVH.FPT file exist
*!*  IF FILE(lcFilePath + 'APRBALVH.FPT')
*!*    ERASE (lcFilePath + 'APRBALVH.FPT')
*!*  ENDIF    && End of IF The APRBALVH.FPT file exist

*!*  *If The APRBALVH.CDX file exist
*!*  IF FILE(lcFilePath + 'APRBALVH.CDX')
*!*    ERASE (lcFilePath + 'APRBALVH.CDX')
*!*  ENDIF    && End of IF The APRBALVH.CDX file exist

*!*  *!*************************************************************
*!*  *! Name      : lfInvInfo
*!*  *! Developer : Amin Khodary 
*!*  *! Date      : 08/23/1999
*!*  *! Purpose   : Function to calculate and return the values of A/P invoice
*!*  *!             amount , total discount taken , total adj. amount , discount offer
*!*  *!             as well as the amount paid from invoice distribution file.
*!*  *!*************************************************************
*!*  *! Called from : lfRbalVend()
*!*  *!*************************************************************
*!*  *! Calls       : None
*!*  *!*************************************************************
*!*  *! Passed Parameters : 1 ) lcInvNo 
*!*  *!                     2 ) lnInvAmt   
*!*  *!             3 ) lnDiscTakn 
*!*  *!             4 ) lnDiscOffr 
*!*  *!             5 ) lnAdjtAmt  
*!*  *!                     6 ) lnAmtPaid
*!*  *!*************************************************************
*!*  *! Return      : 1 ) lnInvAmt holds invoice amount. 
*!*  *!               2 ) lnDiscTakn holds total discount taken.
*!*  *!               3 ) lnDiscOffr holds discount offer amount.
*!*  *!               4 ) lnAdjtAmt holds total adj. amount.  
*!*  *!               5 ) lnAmtPaid holds total paid amount. 
*!*  *!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*  *!*************************************************************
*!*  FUNCTION lfInvInfo
*!*  PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt 

*!*  *Save the current area
*!*  lcCurArea  = SELECT()          && Varible to save the number of the current work area

*!*  * Select the A/P invoice distribution
*!*  SELECT APDIST

*!*  * Define private variable to save the current record no. of invoice distribution
*!*  PRIVATE  lnSaveRec
*!*  lnSaveRec = RECNO('APDIST')

*!*  * Initialize variables 
*!*  lnInvAmt   = 0                  
*!*  lnDiscTakn = 0                  
*!*  lnDiscOffr = 0                  
*!*  lnAdjtAmt  = 0                  
*!*  lnAmtPaid  = 0 
*!*  lnPurchAmt  = 0 
*!*  *If condition not found set the record pointer to the saved record no. then retrun
*!*  IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST') 
*!*    IF BETWEEN( lnSaveRec  , 1, RECCOUNT('APDIST')  ) 
*!*      GOTO (lnSaveRec) IN APDIST
*!*    ENDIF
*!*    SELECT (lcCurArea)
*!*    RETURN lnDiscOffr 
*!*  ENDIF 
*!*   
*!*  * Loop the required records and calcluate the values.
*!*  SCAN REST WHILE APDIST.cVendCode + APDIST.cInvNo = M.cVendCode + lcInvNo ;
*!*            FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' ) 
*!*    
*!*    * Get equivelent value of A/P invoice amount or debit memo amount. 
*!*    * Note: It must only one record per invoice.  
*!*    IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'  
*!*      lnInvAmt   = - APDIST.nEqvAmnt 
*!*      lnPurchAmt = - APDIST.nEqvAmnt 
*!*    ENDIF 

*!*    * Notes
*!*    * APDIST.cApDtrTyp contains 'A' means debit memo
*!*    * APDIST.cApDtrTyp contains 'H' means cash payments 
*!*    * APDIST.cApDtrTyp contains 'M' means manuals checks payments 
*!*    * APDIST.cApDtrTyp contains 'N' means non checks payments 
*!*    * APDIST.cApDtrTyp contains 'P' means printed checks
*!*    
*!*    * Get and sum the equivelent value of paid amount 
*!*    IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'C'
*!*      lnAmtPaid = lnAmtPaid -  APDIST.nEqvAmnt 
*!*    ENDIF 

*!*    * Get and sum the equivelent value of discount taken 
*!*    IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
*!*      lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt 
*!*    ENDIF 

*!*    
*!*    * Get and sum the equivelent value of adj. amount.
*!*    IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'
*!*      lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
*!*    ENDIF
*!*    
*!*  ENDSCAN  && End of Loop the required records and calcluate the values.       

*!*  IF APINVHDR.nInvAmnt > 0 && Payable Invoice
*!*    SUM IIF(cApdActId ="A" and nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) to lnp,lndis;
*!*    FOR cApdTrTyp+cBnkCode+cChkAcct+cApdRef+cInvNo+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)
*!*    lnAmtPaid  = lnAmtPaid   + lnp
*!*    lnDiscTakn = lnDiscTakn  + lndis
*!*  ENDIF  

*!*  * Get and calculate the value of discount offer from invoice header file.
*!*  lcExSin2   = ''
*!*  lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
*!*  lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)

*!*  * Back to the initial record
*!*  SELECT APDIST
*!*  IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() ) 
*!*    GOTO (lnSaveRec) IN APDIST
*!*  ENDIF  

*!*  * Back to the initial file and record no.
*!*  SELECT (lcCurArea)
*!*  RETURN lnDiscOffr 
*!*  *--- End of lfInvInfo()

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the invoice number.
*!*************************************************************
*! Example   : = lfvInvNo()
*!*************************************************************
FUNCTION lfvInvNo
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcInvN', @lcSelInvNo, 'Invoice C(6)', 'Invoice', 'Invoice')

*!*************************************************************
*! Name      : lfvOrdNo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the order number.
*!*************************************************************
*! Example   : = lfvOrdNo()
*!*************************************************************
FUNCTION lfvOrdNo
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcOrdN', @lcSelOrdNo, 'Order C(6)', 'Order', 'Order')

*------------------  Not in this Release  ----------------*
*                                                         *
*                                                         *
*!*  *!*************************************************************
*!*  *! Name      : lfvCtNo
*!*  *! Developer : Ashraf Sherif Mohammad (ASH)
*!*  *! Date      : 10/18/1999
*!*  *! Purpose   : Valid function for the C/T number.
*!*  *!*************************************************************
*!*  *! Example   : = lfvCtNo()
*!*  *!*************************************************************

*!*  FUNCTION lfvCtNo
*!*  PRIVATE lnAlias, lcObjVal, lcObjName  

*!*  lnAlias   = SELECT(0)
*!*  lcObjName = SYS(18)
*!*  lcObjVal  = EVALUATE(SYS(18))      
*!*  IF !USED('CutTktH')
*!*    =gfOpenFile(lcFilePath+'CutTktH', 'CutTktH' ,'SH')
*!*  ENDIF

*!*  SELECT CutTktH
*!*  IF '?' $ lcObjVal  OR (!EMPTY(lcObjVal) AND !SEEK(lcObjVal,"CutTktH"))
*!*    xCutTkt   = lcObjVal
*!*    llNoThing = CutBrow(xCutTkt)
*!*    IF EMPTY(xCutTkt)
*!*      &lcObjName = laOldVal
*!*      _CUROBJ    = _CUROBJ
*!*      RETURN
*!*    ELSE
*!*      &lcObjName = xCutTkt
*!*    ENDIF
*!*  ENDIF
*!*  SELECT(lnAlias)

*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Validation for Account code.
*!***************************************************************************
*! Example   : = lfvAccount()
*!***************************************************************************
FUNCTION lfvAccount
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcAccount', @lcSelAccount, 'Account C(5)', 'Account', 'Account')

*!*************************************************************
*! Name      : lfvPONO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the Style P/O number.
*!*************************************************************
*! Example   : = lfvPONO()
*!*************************************************************
FUNCTION lfvPONO
LPARAMETERS lcMode

*B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*=lfCreateCursor(1, 'lcPON', @lcSelPONo, 'PO C(6)', 'PO', 'PO')
=lfCreatePOCursor(1, 'lcPON', @lcSelPONo)
*B129821,1 WSH 09/29/2005 [End]

*!*************************************************************
*! Name      : lfvMPONO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the Material P/O number.
*!*************************************************************
*! Example   : = lfvMPONO()
*!*************************************************************
FUNCTION lfvMPONO
LPARAMETERS lcMode

*B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*=lfCreateCursor(1, 'lcMPON', @lcSelPONo, 'PO C(6)', 'PO', 'PO')
=lfCreatePOCursor(1, 'lcMPON', @lcSelPONo)
*B129821,1 WSH 09/29/2005 [End]

*!***************************************************************************
*! Name      : lfvStyle
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Validation for Style code.
*!***************************************************************************
*! Example   : = lfvStyle()
*!***************************************************************************
FUNCTION lfvStyle
LPARAMETERS lcMode
=lfCreateCursor(1, 'lcAllStyle', @lcSelStyle, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
*-- End of lfvStyle.

*!***************************************************************************
*! Name      : lfvFabric
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Validation for Style code.
*!***************************************************************************
*! Example   : = lfvStyle()
*!***************************************************************************
FUNCTION lfvFabric
LPARAMETERS lcMode

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
*=lfCreateCursor(1, 'lcAllFabric', @lcSelStyle, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
=lfCreateCursor(1, 'lcAllFabric', @lcSelFabric, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
*B608487,1 WAM 03/19/2008 (End)
*-- End of lfvStyle.

*!*************************************************************
*! Name      : lfStpHdDtl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Setup function for the Header/Detail rebalance.
*!*************************************************************
*! Example   : = lfStpHdDtl()
*!*************************************************************
FUNCTION lfStpHdDtl

LOCAL lcCurrArea
laFChck = ''

DO CASE
  
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
  CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
    lcKeyType    = ''
    lcKeyVal     = 'PACK_NO'
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

  CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCARINV
    lcKeyType    = ''
    lcKeyVal     = 'Invoice'
    laFChck[1,1] = 'Ship'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'ShipAmt'
    laFChck[2,2] = 'Price'
  CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
    lcKeyType    = 'cOrdType'
    lcKeyVal     = 'Order'
    laFChck[1,1] = 'Open'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'OpenAmt'
    laFChck[2,2] = 'Price'
    laFChck[1,3] = 'Book'
    laFChck[1,4] = 'TotBook'
    laFChck[2,3] = 'BookAmt'
    laFChck[2,4] = 'Price'
    laFChck[1,5] = 'Cancel'
    laFChck[1,6] = 'TotQty'
    laFChck[2,5] = 'CancelAmt'
    laFChck[2,6] = 'Price'    
*------------------  Not in this Release ------------------*
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
  CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
*!*      lcKeyType    = 'cBusDocu+cStyType'
*!*      lcKeyVal     = 'PO'
*!*      laFChck[1,1] = 'nStyOrder'
*!*      laFChck[1,2] = 'TotQty'
*!*      laFChck[2,1] = 'Receive'
*!*      laFChck[2,2] = 'TotQty'
    lcKeyType    = 'cBusDocu+cStyType'
    lcKeyVal     = 'PO'
    laFChck[1,1] = 'nStyOrder'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'Receive'
    laFChck[2,2] = 'TotQty'
    laFChck[1,5] = 'Cancel'
    laFChck[1,3] = 'Damage'
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

  CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO
    lcKeyType    = 'cBusDocu+cStyType'
    lcKeyVal     = 'PO'
    laFChck[1,1] = 'nStyOrder'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'Receive'
    laFChck[2,2] = 'TotQty'
    laFChck[1,5] = 'Cancel'
    laFChck[1,3] = 'Damage'
ENDCASE

IF &lcRebalHdr..cUpdVryIgn = 'U' AND !llUpdt
  RETURN
ENDIF

lcCurrArea = SELECT(0)

IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.DBF') 
  USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0 EXCL
  SELECT ALLTRIM(&lcRebalHdr..cTempName)
  SET ORDER TO TAG (&lcRebalHdr..cTempName)
ELSE
  lcTag  = &lcRebalHdr..cTempName
  lcFlds = STRTRAN(&lcRebalHdr..cFileStr,'|',',')
  CREATE TABLE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) ;
               &lcFlds
ENDIF

IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.CDX')               
  SET ORDER TO TAG ALLTRIM(&lcRebalHdr..cTempName)
ELSE
  * Create index on temporary file    
  INDEX ON  &lcKeyType+&lcKeyVal TAG &lctag OF (oAriaApplication.WorkDir +ALLTRIM(&lcRebalHdr..cTempName) + '.CDX')
ENDIF

SELECT (lcCurrArea)

*!*************************************************************
*! Name      : lfVldHdDtl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Valid function for the Header/Detail rebalance.
*!*************************************************************
*! Example   : = lfVldHdDtl()
*!*************************************************************
FUNCTION lfVldHdDtl

RETURN .T.

*!*************************************************************
*! Name      : lfVryHdDtl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Verify function for the Header/Detail rebalance.
*!*************************************************************
*! Example   : = lfVryHdDtl()
*!*************************************************************
FUNCTION lfVryHdDtl

LOCAL lcSetDelet, lnCanQty, lnCanAmt, lcHdrLnRel, llSQL, lnCurrent, lnAlias, llFound, lnUpThermo
lnAlias    = SELECT(0)
lnSep      = AT(',',&lcRebalHdr..cMFiles)
lcFilter   = '.T.'
lcHdrLnRel = ''
lnCurrent  = 0

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*=SEEK(UPPER(SUBSTR(&lcRebalHdr..cMFiles,1,lnSep-1)), lcRebalDtl, 'cFileName')
*loHdrFile = &lcRebalDtl..cFileObj
*loHdrFile = &loHdrFile.
*lcHdrFile = loHdrFile.lcCursorView
*
*=SEEK(UPPER(SUBSTR(ALLTRIM(&lcRebalHdr..cMFiles),lnSep+1)), lcRebalDtl, 'cFileName')
*loDetFile = &lcRebalDtl..cFileObj
*loDetFile = &loDetFile.
*lcDetFile = loDetFile.lcCursorView
=SEEK(&lcRebalHdr..cItemName+UPPER(SUBSTR(&lcRebalHdr..cMFiles,1,lnSep-1)), lcRebalDtl)
lcHdrFile = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))

=SEEK(&lcRebalHdr..cItemName+UPPER(SUBSTR(ALLTRIM(&lcRebalHdr..cMFiles),lnSep+1)), lcRebalDtl)
lcDetFile = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))

LOCAL lcStat, llCustNat, llConsHNat, llConsLNat

IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
  PRIVATE lcInvLine, lcInvHdr, lcORDCANLN
  
  =SEEK(&lcRebalHdr..cItemName+'INVLINE', lcRebalDtl)
  lcInvLine = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))
  
  =SEEK(&lcRebalHdr..cItemName+'INVHDR', lcRebalDtl)
  lcInvHdr = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))
  
  =SEEK(&lcRebalHdr..cItemName+'ORDCANLN', lcRebalDtl)
  lcORDCANLN = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))
ENDIF
*B128464,2 WSH 08/21/2005 [End]

SELECT ALLTRIM(&lcRebalHdr..cTempName)
ZAP

IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  *lcFilter   = IIF(lcInvType = '0001', "cStyType $ 'PN'", "cStyType $ 'ML'")
  lcFilter   = IIF(lcInvType = '0001', "cStyType = 'P' OR cStyType = 'N'", "cStyType = 'M' OR cStyType = 'L'")
  *B608487,1 WAM 03/19/2008 (End)
  lcFiltCurs = lcSelPONo
ENDIF

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = lcMfCt
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  *lcFilter   = "cStyType $ 'U'"
  lcFilter   = "cStyType = 'U'"
  *B608487,1 WAM 03/19/2008 (End)
  lcFiltCurs = lcSelCTNo
ENDIF
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

*B608487,1 WAM 03/19/2008 Enhance performance of rebalance (Commented out)
*LOCAL lcCustomer
*lcCustomer = gfTempName()
*B608487,1 WAM 03/19/2008 (End)


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = lcALPack
  lcFilter   = ".T."
  lcFiltCurs = lcPackSelect   
ENDIF   
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]



IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  LOCAL lcCustomer
  lcCustomer = gfTempName()
  *B608487,1 WAM 03/19/2008 (End)
  
  lcFilter   = "cOrdType = 'O'"
  lcFiltCurs = lcSelOrdNo
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loCustomer = CREATEOBJECT('RemoteTable', 'CUSTOMER', 'CUSTOMER', lcCustomer, SET("Datasession"), lcCurrComp_ID, .T.)
  llCustNat = lfCREATEOBJECT('CUSTOMER', 'CUSTOMER', lcCustomer)
  *B128464,2 WSH 08/21/2005 [End]

ENDIF

IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = lcArInv
  lcFiltCurs = lcSelInvNo
ENDIF


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
  lcFiltCurs = lcPackSelect
ENDIF
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]



IF FILE(oAriaApplication.WorkDir + lcFiltCurs + '.DBF') AND !USED(lcFiltCurs)
  USE (oAriaApplication.WorkDir + lcFiltCurs + '.DBF') IN 0 ALIAS (lcFiltCurs)
ENDIF
*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
*B608487,1 WAM 03/19/2008 (End)

LOCAL loConsInvH, loConsInvL, lcConsInvH, lcConsInvL
lcConsInvH = gfTempName()
lcConsInvL = gfTempName()
llConsH    = .F.
llConsL    = .F.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*loConsInvH = CREATEOBJECT('RemoteTable', 'ConsInvH', 'CONSINVH', lcConsInvH, SET("Datasession"), lcCurrComp_ID, .T.)
*llConsH = TYPE('loConsInvH') = 'O'

*loConsInvL = CREATEOBJECT('RemoteTable', 'ConsInvL', 'CINVLINE', lcConsInvL, SET("Datasession"), lcCurrComp_ID, .T.)
*llConsL = TYPE('loConsInvL') = 'O'
llConsHNat = lfCREATEOBJECT('ConsInvH', 'CONSINVH', lcConsInvH)
llConsLNat = lfCREATEOBJECT('ConsInvL', 'CINVLINE', lcConsInvL)
llConsH = USED(lcConsInvH)
llConsL = USED(lcConsInvL)
*B128464,2 WSH 08/21/2005 [End]
*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
ENDIF
*B608487,1 WAM 03/19/2008 (End)
SELECT &lcHdrFile 
lcCrit = IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) <> LCSOORD, "!(Status $ 'SX')" , ".T.")

LOCAL llExitLoop, llScanInFilt
llScanInFilt = .F.
llExitLoop   = .F.

IF USED(lcFiltCurs) AND RECCOUNT(lcFiltCurs) > 0
  llScanInFilt = .T.
ENDIF

IF llScanInFilt
  SELECT (lcFiltCurs)
  lnUpThermo = RECCOUNT()
  LOCATE
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  lcInExpression = ''
  SCAN
    m.KeyType  = IIF(EMPTY(lcKeyType),'',IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD,'O',EVALUATE(lcKeyType)))
    lcInExpression=lcInExpression+IIF(EMPTY(lcInExpression),'',',')+"'"+m.KeyType+ALLTRIM(EVALUATE(lcKeyVal))+"'"
  ENDSCAN
  lcSeekCond = lcKeyType+IIF(EMPTY(lcKeyType),'','+')+lcKeyVal+" IN ("+lcInExpression +")"
  llFound= lfSEEK(lcHdrFile, lcSeekCond, '', .T.)
  *B608487,1 WAM 03/19/2008 (End)

ELSE
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance (commented out)
*!*	  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	  *lnUpThermo = lfRecCount(loHdrFile)
*!*	  *llExitLoop = !loHdrFile.GOTOP()
*!*	  *lcHdrFile  = loHdrFile.lcCursorView
*!*	  lnUpThermo = lfRecCount(lcHdrFile)
*!*	  llExitLoop = !lfGOTOP(lcHdrFile)
*!*	  *B128464,2 WSH 08/21/2005 [End]
*!*	  
*!*	  SELECT (lcHdrFile)
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
    llFound = lfSEEK(lcHdrFile, lcFilter, '', .T.)
  ELSE
    llFound = lfSEEK(lcHdrFile, lcFilter, IIF(EMPTY(lcKeyType), '', 'O'), .T.)
  ENDIF
  SELECT (lcHdrFile)
  lnUpThermo = RECCOUNT()
  *B608487,1 WAM 03/19/2008 (End)
ENDIF
=lfInitThermo(lnUpThermo, LANG_SMREBAL_RECALC)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
PRIVATE m.KeyVal, lcSeekCond
*B128464,2 WSH 08/21/2005 [End]

*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
*DO WHILE !llExitLoop

lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcHdrFile)), 1)
llNative  = laFileStr[lnFilePos,6]
IF llNative AND llScanInFilt
  SELECT (lcFiltCurs)
ELSE
  SELECT (lcHdrFile)
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
lnUpThermo = RECCOUNT()
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
SCAN 
  IF llNative AND llScanInFilt
    SELECT (lcFiltCurs)
    m.KeyType = IIF(EMPTY(lcKeyType),'',IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD,'O',EVALUATE(lcKeyType)))
    =SEEK(m.KeyType+ALLTRIM(EVALUATE(lcKeyVal)),lcHdrFile)
    SELECT (lcHdrFile)
  ENDIF
  IF NOT &lcCrit
    LOOP
  ENDIF
*B608487,1 WAM 03/19/2008 (End)

  lnCurrent = lnCurrent + 1
  
  *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
  *oProgress.lblSecondLabel.Caption = lcKeyVal + ' : ' + ALLTRIM(&lcKeyVal)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <>'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 

    oProgress.lblSecondLabel.Caption = IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT,LANG_SMREBAL_CTNO,lcKeyVal)+ ' : ' + ALLTRIM(&lcKeyVal)
    *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
  
    oProgress.CurrentProgress(lnCurrent)
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF 
    *E303410,1 TMI 09/01/2013 [End  ] 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    
    IF MOD(lnCurrent ,CEILING(lnUpThermo / 10)) = 0
      loProgress.Percent     = lnCurrent / lnUpThermo
      loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT,LANG_SMREBAL_CTNO,lcKeyVal)+ ' : ' + ALLTRIM(&lcKeyVal)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
    ENDIF    
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance (Commented out)
 
*!*	  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	  m.KeyVal = EVALUATE(lcKeyVal)
*!*	  *B128464,2 WSH 08/21/2005 [End]
*!*	  
*!*	  *B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*!*	  
*!*	  *B130225,1 WSH 10/26/2005 No Key Type in case of SO Rebal and scan in temp cursor. [Start]
*!*	  *m.KeyType = EVALUATE(lcKeyType)
*!*	  *B130225,1 WSH 10/26/2005 [End]
*!*	  
*!*	  *B129821,1 WSH 09/29/2005 [End]
*!*	  
*!*	  llFound = .T.
*!*	  IF llScanInFilt
*!*	    *-- Seek for current PO or order number in the master header file...
*!*	    
*!*	    SELECT (lcFiltCurs)
*!*	    
*!*	    *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*!*	    *IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO
*!*	    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
*!*	    *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
*!*	    
*!*	      *B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*!*	      *IF lcInvType = '0001'
*!*	      *  
*!*	      *  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *  *llFound = loHdrFile.SEEK('PP' + &lcKeyVal)
*!*	      *  *llFound = llFound OR loHdrFile.SEEK('NN' + &lcKeyVal)
*!*	      *  *llFound = llFound OR loHdrFile.SEEK('RP' + &lcKeyVal)
*!*	      *  lcSeekCond = "cBusDocu IN ('P', 'N', 'R') AND cStyType IN ('P', 'N') AND PO = ?m.KeyVal"
*!*	      *  llFound    = lfSEEK(lcHdrFile, lcSeekCond, '', .T.)
*!*	      *  *B128464,2 WSH 08/21/2005 [End]
*!*	      *  
*!*	      *ELSE
*!*	      *  
*!*	      *  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *  *llFound = loHdrFile.SEEK('PM' + &lcKeyVal)
*!*	      *  *llFound = llFound OR loHdrFile.SEEK('RP' + &lcKeyVal)
*!*	      *  lcSeekCond = "cBusDocu IN ('P', 'R') AND cStyType = 'M' AND PO = ?m.KeyVal"
*!*	      *  llFound    = lfSEEK(lcHdrFile, lcSeekCond, '', .T.)
*!*	      *  *B128464,2 WSH 08/21/2005 [End]
*!*	      *
*!*	      *ENDIF
*!*	      
*!*	      *B130225,1 WSH 10/26/2005 Put the Key Type variable in case of PO only. [Start]
*!*	      m.KeyType  = EVALUATE(lcKeyType)
*!*	      *B130225,1 WSH 10/26/2005 [End]
*!*	      
*!*	      m.cBusDocu = SUBSTR(m.KeyType, 1, 1)
*!*	      m.cStyType = SUBSTR(m.KeyType, 2, 1)
*!*	      lcSeekCond = "cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.KeyVal"
*!*	      
*!*	      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*!*	      llFound    = lfSEEK(lcHdrFile, lcSeekCond, m.cBusDocu+m.cStyType+m.KeyVal, .T.)
*!*	      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
*!*	      
*!*	      *B129821,1 WSH 09/29/2005 [End]
*!*	      
*!*	    ELSE
*!*	      
*!*	      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *llFound = loHdrFile.SEEK(IIF(EMPTY(lcKeyType), '', 'O') + &lcKeyVal)
*!*	      lcSeekCond = IIF(EMPTY(lcKeyType), "", "CORDTYPE = 'O' AND ") + lcKeyVal + " = ?m.KeyVal"
*!*	      llFound    = lfSEEK(lcHdrFile, lcSeekCond, IIF(EMPTY(lcKeyType), '', 'O') + &lcKeyVal, .T.)
*!*	      *B128464,2 WSH 08/21/2005 [End]
*!*	      
*!*	    ENDIF
*!*	  ENDIF
*!*	  SELECT (lcHdrFile)
*!*	  IF !llFound OR !(&lcFilter) OR !(&lcCrit)
*!*	    IF llScanInFilt
*!*	      SELECT (lcFiltCurs)
*!*	      SKIP
*!*	      llExitLoop = EOF()
*!*	    ELSE
*!*	      SELECT (lcHdrFile)
*!*	      
*!*	      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *llExitLoop = !loHdrFile.GONEXT()
*!*	      llExitLoop = !lfGONEXT(lcHdrFile)
*!*	      *B128464,2 WSH 08/21/2005 [End]
*!*	      
*!*	    ENDIF
*!*	    LOOP
*!*	  ENDIF
  *B608487,1 WAM 03/19/2008 (End)
  
  SELECT (lcHdrFile)
  
  IF EMPTY(lcKeyType)
    lcExpr = &lcKeyVal
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    m.KeyVal   = lcExpr
    lcSeekCond = lcKeyVal + " = ?m.KeyVal"
    *B128464,2 WSH 08/21/2005 [End]
    
  ELSE
    lcExpr = &lcKeyType + &lcKeyVal
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    IF UPPER(lcKeyType) = 'CORDTYPE'
      m.Order    = EVALUATE(lcKeyVal)
      m.cOrdType = EVALUATE(lcKeyType)
      lcSeekCond = "CORDTYPE = ?m.cOrdType AND " + lcKeyVal + " = ?m.Order"
    ELSE
      m.KeyVal   = EVALUATE(lcKeyVal)
      m.cBusDocu = SUBSTR(EVALUATE(lcKeyType), 1, 1)
      m.cStyType = SUBSTR(EVALUATE(lcKeyType), 2, 1)
      lcSeekCond = "cBusDocu  = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.KeyVal"
    ENDIF
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  
  
  *: B608600,1 MMT 07/01/2008 Fix bug of Reblanacing cancelled Bulk Orders[Start]
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
    IF &lcHdrFile..CORDTYPE = 'O'  AND &lcHdrFile..Status = 'X' AND &lcHdrFile..BULK = 'Y'
      LOOP 
    ENDIF 
  ENDIF 
  *: B608600,1 MMT 07/01/2008 Fix bug of Reblanacing cancelled Bulk Orders[End]
  
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loDetFile.SEEK(lcExpr)
  =lfseek(lcDetFile, lcSeekCond, lcExpr)
  *B128464,2 WSH 08/21/2005 [End]
  
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loCustomer.SEEK('M'+ &lcHdrFile..Account)
    *E302821,1 TMI 12/27/2010 [Start] no need here to change the INDEX=<index name> as the programmer use the table in its native mode, I just changed it for future use
    *lcStat = "SELECT * FROM Customer (INDEX = CUSTOMER) WHERE Type = 'M' AND Account = ?" + lcHdrFile + ".Account"
    lcStat = "SELECT * FROM Customer WITH(INDEX(CUSTOMER)) WHERE Type = 'M' AND Account = ?" + lcHdrFile + ".Account"
    *E302821,1 TMI 12/27/2010 [End  ] 
    
    =lfSQLRun(lcStat, lcCustomer, llCustNat, 'M'+ &lcHdrFile..Account)
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  
  lnQty = 0
  lnAmt = 0
  
  DIMENSION laOpenQty[8], laCanQty[8], laDmgQty[8]
  STORE 0 TO lnOpenQty, lnCanQty, lnDmgQty
  
  *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
  STORE 0 TO lnICost1,lnICost2,lnICost3,lnICost4,lnICost5,lnICost6,lnICost7,;
  			 lnfCost1,lnfCost2,lnfCost3,lnfCost4,lnfCost5,lnfCost6,lnfCost7
  			 
  STORE 0 TO lnflanCost1,lnflanCost2,lnflanCost3,lnflanCost4,lnflanCost5,lnflanCost6,lnflanCost7,;
  			 lnlan_Cost1,lnlan_Cost2,lnlan_Cost3,lnlan_Cost4,lnlan_Cost5,lnlan_Cost6,lnlan_Cost7
  *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]
  
  IF !EMPTY(laFChck[1,3])
    lnQty1 = 0
    lnAmt1 = 0
  ENDIF
  
  IF !EMPTY(laFChck[1,5])
    STORE 0 TO lnCanQty, lnCanAmt
  ENDIF
  
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
    lnTotPcs = 0
    lnTotCart = 0
    lnTotWght = 0
    DIMENSION laCartNO[1]
    laCartNO = ''
  ENDIF 
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

  
  
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
    lnShipQty = 0
    lnShipAmt = 0
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF loInvLine.SEEK(&lcHdrFile..Order)
    *  SELECT (loInvLine.lcCursorView)
    *E302821,1 TMI 12/27/2010 [Start] no need here to change the INDEX=<index name> as the programmer use the table in its native mode, I just changed it for future use
    *lcStat = "SELECT * FROM InvLine (INDEX = INVLINEO) WHERE Order = ?" + lcHdrFile + ".Order"
    lcStat = "SELECT * FROM InvLine WITH(INDEX(INVLINEO)) WHERE Order = ?" + lcHdrFile + ".Order"
    *E302821,1 TMI 12/27/2010 [End  ] 
    IF lfSeek(lcInvLine, lcStat, &lcHdrFile..Order)
      SELECT (lcInvLine)
    *B128464,2 WSH 08/21/2005 [End]
    
      SCAN REST WHILE order+STR(lineno,6)+invoice = &lcHdrFile..Order
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *IF !loInvHdr.SEEK(Invoice) OR EVALUATE(loInvHdr.lcCursorView + '.Status') = 'V' OR EVALUATE(loInvHdr.lcCursorView + '.CONSOL') = 'Y'
        m.Invoice = Invoice
        IF !lfSeek(lcInvHdr, "Invoice = ?m.Invoice", Invoice, .T.) OR EVALUATE(lcInvHdr + '.Status') = 'V' OR EVALUATE(lcInvHdr + '.CONSOL') = 'Y'
        *B128464,2 WSH 08/21/2005 [End]
        
          LOOP
        ENDIF
        
        lnShipQty = lnShipQty + TotQty
        lnShipAmt = lnShipAmt + TotQty * Price
      ENDSCAN
    ENDIF
    
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') <>'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      WAIT WINDOW LANG_SMREBAL_SHPCOLL1 + Order + LANG_SMREBAL_COLL NOWAIT
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ENDIF 
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF loConsInvL.SEEK(&lcHdrFile..Order)
    m.Order = EVALUATE(lcHdrFile + '.Order')
    IF lfSQLRun("SELECT * FROM ConsInvL (INDEEX = CINVLINE) WHERE Order = ?m.Order", lcConsInvL, llConsLNat, m.Order)
    *B128464,2 WSH 08/21/2005 [End]
    
      SELECT (lcConsInvL)
      SCAN REST WHILE order+STR(lineno,6)+invoice = &lcHdrFile..Order
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *IF !loConsInvH.SEEK(Invoice) OR &lcConsInvH..Status = 'V'
        m.Invoice = Invoice
        *E302821,1 TMI 12/27/2010 [Start] no need here to change the INDEX=<index name> as the programmer use the table in its native mode, I just changed it for future use
        *IF !lfSQLRun("SELECT * FROM ConsInvH (INDEX = CONSINVH) WHERE Invoice = ?m.Invoice", lcConsInvH, llConsHNat, Invoice) OR EVALUATE(lcConsInvH + '.Status') = 'V'
        IF !lfSQLRun("SELECT * FROM ConsInvH WITH(INDEX(CONSINVH)) WHERE Invoice = ?m.Invoice", lcConsInvH, llConsHNat, Invoice) OR EVALUATE(lcConsInvH + '.Status') = 'V'
        *E302821,1 TMI 12/27/2010 [End  ] 
        *B128464,2 WSH 08/21/2005 [End]
        
          LOOP
        ENDIF
        
        SELECT (lcConsInvL)
        lnShipQty = lnShipQty + TotQty
        lnShipAmt = lnShipAmt + TotQty*Price
      ENDSCAN
    ENDIF
  ENDIF
  
  SELECT (lcDetFile)
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT;
     OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO

    lcWhileExp = "&lcKeyType+&lcKeyVal+cInvType+Style+STR(Lineno,6) = lcExpr+lcTransSty+lcTransLin"

    DO WHILE &lcKeyType+&lcKeyVal = lcExpr
      lcTransSty = cInvType + Style
      lcTransLin = STR(LineNo,6)
      STORE 0 TO laOpenQty, laCanQty, laDmgQty
      SCAN REST WHILE &lcWhileExp
        DO CASE
          CASE Trancd = '1'
            lnQty = lnQty + &laFChck[1,2]

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = &lcDetFile..Qty&lcCntr
              
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
              FOR lnCstCntr = 1 TO 7
                lcCstCntr = STR(lnCstCntr ,1)
                lnICost&lcCstCntr. = lnICost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * niCost&lcCstCntr.,3)
                lnFCost&lcCstCntr. = lnFCost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * nfCost&lcCstCntr.,3)                
    		  ENDFOR 		
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

            ENDFOR

          CASE Trancd = '2'
            lnAmt = lnAmt  + &laFChck[2,2]

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
              
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
              FOR lnCstCntr = 1 TO 7
                lcCstCntr = STR(lnCstCntr ,1)
                lnflanCost&lcCstCntr. = lnflanCost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * nflanCost&lcCstCntr.,3)
                lnlan_Cost&lcCstCntr. = lnlan_Cost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * nlan_Cost&lcCstCntr.,3)                
    		  ENDFOR 		
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]
              
            ENDFOR

          CASE Trancd = '4'

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
              
              *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
              *laDmgQty[lnCntr]  = &lcDetFile..Qty&lcCntr
              laDmgQty[lnCntr]  = laDmgQty[lnCntr] + &lcDetFile..Qty&lcCntr
              *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
              
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
              FOR lnCstCntr = 1 TO 7
                lcCstCntr = STR(lnCstCntr ,1)
                lnflanCost&lcCstCntr. = lnflanCost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * nflanCost&lcCstCntr.,3)
                lnlan_Cost&lcCstCntr. = lnlan_Cost&lcCstCntr. +  ROUND(&lcDetFile..Qty&lcCntr * nlan_Cost&lcCstCntr.,3)                
    		  ENDFOR 
              *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]
            ENDFOR

          CASE Trancd = '5'
             FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
              
              *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
              *laCanQty[lnCntr]  = &lcDetFile..Qty&lcCntr
              laCanQty[lnCntr]  = laCanQty[lnCntr] + &lcDetFile..Qty&lcCntr
              *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
              
            ENDFOR

        ENDCASE
      ENDSCAN 
      
      FOR lnCntr = 1 TO 8
        lnOpenQty = lnOpenQty + laOpenQty[lnCntr]
        lnCanQty  = lnCanQty + laCanQty[lnCntr]
        lnDmgQty  = lnDmgQty + laDmgQty[lnCntr]
      ENDFOR
      lnQty1 = lnDmgQty
    ENDDO
  ELSE
    SCAN WHILE &lcKeyType+&lcKeyVal = lcExpr 
    
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
        lnTotPcs = lnTotPcs + TOTQTY
        IF ASCAN(laCartNO,No_Cart,1) = 0
            lnTotCart = lnTotCart + 1
            IF EMPTY(laCartNO[1])
              laCartNO[1] = No_Cart
            ELSE
              DIMENSION laCartNO[ALEN(laCartNO)+1]
              laCartNO[ALEN(laCartNO)] = No_Cart
            ENDIF 
          ENDIF   
        lnTotWght = lnTotWght + Weight
        LOOP 
      ENDIF 
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
    
      IF &lcHdrFile..STATUS = 'X' AND !EMPTY(laFChck[1,5])
        lnCanQty = lnCanQty + &laFChck[1,2] 
        lnCanAmt = lnCanAmt + &laFChck[1,2] * &laFChck[2,2]
      ELSE
        lnQty   = lnQty  + &laFChck[1,2] 
        lnAmt   = lnAmt  + &laFChck[1,2] * &laFChck[2,2] 
      ENDIF
      
      IF !EMPTY(laFChck[1,3])
        lnQty1   = lnQty1  + &laFChck[1,4] 
        lnAmt1   = lnAmt1  + &laFChck[1,4] * &laFChck[2,4] 
      ENDIF
    ENDSCAN  
  ENDIF
  
  IF !EMPTY(laFChck[1,3])
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF loOrdCanLn.SEEK(lcExpr)
      *  SELECT (loOrdCanLn.lcCursorView)
      IF lfseek(lcORDCANLN, lcSeekCond, lcExpr)
        SELECT (lcORDCANLN)
      *B128464,2 WSH 08/21/2005 [End]
      
        SCAN REST WHILE &lcKeyType+&lcKeyVal = lcExpr
          lnCanQty = lnCanQty + TotQty
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *IF loDetFile.SEEK(cOrdType+Order+STR(LineNo,6))
          m.cOrdType = cOrdType
          m.Order    = Order
          m.LineNo   = LineNo
          IF lfSeek(lcDetFile, "cOrdType = ?m.cOrdType AND Order = ?m.Order AND LineNo = ?m.LineNo", cOrdType+Order+STR(LineNo,6))
          *B128464,2 WSH 08/21/2005 [End]
          
            lnCanAmt = lnCanAmt + (TotQty * &lcDetFile..Price)
          ELSE
            lnQty1 = lnQty1  + TotQty
            
            *-- if price iz zero check for price in ordline file
            IF Price = 0
              
              *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
              *IF loDetFile.SEEK(cOrdType+Order+STR(LineNo,6))
              *  lnAmt1   = lnAmt1  + (TotQty * &lcDetFile..Price)
              *  lnCanAmt = lnCanAmt + (TotQty * &lcDetFile..Price)
              *ENDIF
              *B128464,2 WSH 08/21/2005 [End]
              
            ELSE
              lnAmt1   = lnAmt1  + (TotQty * Price)
              lnCanAmt = lnCanAmt + (TotQty * Price)
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    
    lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]"+;
              " OR lnAmt <> &lcHdrFile..&laFChck[2,1]"+;
              " OR lnQty1 <> &lcHdrFile..&laFChck[1,3]"+;
              IIF(EMPTY(laFChck[2,3]), "", " OR  lnAmt1 <> &lcHdrFile..&laFChck[2,3]")+;
              " OR lnCanQty <> &lcHdrFile..&laFChck[1,5]"+;
              IIF(EMPTY(laFChck[2,5]), "", " OR lnCanAmt <> &lcHdrFile..&laFChck[2,5]")
  ELSE
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCARINV AND &lcHdrFile..Status = 'V'
      
      *B127969,1 WSH 05/25/2005 Don't check for shipment amount if the custom trigger exists. [Start]
      *lcIfExp = "lnQty <> &lcHdrFile..vShip  OR lnAmt <> &lcHdrFile..vShipAmt"
      IF !llChkInvAmt
        lcIfExp = "lnQty <> &lcHdrFile..vShip"
      ELSE
        lcIfExp = "lnQty <> &lcHdrFile..vShip OR lnAmt <> &lcHdrFile..vShipAmt"
      ENDIF
      *B127969,1 WSH 05/25/2005 [End]
      
    ELSE
      
      *B127969,1 WSH 05/25/2005 Don't check for shipment amount if the custom trigger exists. [Start]
      *lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1] OR lnAmt <> &lcHdrFile..&laFChck[2,1]"
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = lcArInv AND !llChkInvAmt
        lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]"
      ELSE
        lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1] OR lnAmt <> &lcHdrFile..&laFChck[2,1]"
      ENDIF
      *B127969,1 WSH 05/25/2005 [End]
      
      *B039383,1 WSH 06/05/2005 Move The check for open quantity for POs outside This Condition. [Start]
      *IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO
        *lcIfExp = lcIfExp + " OR lnOpenQty <> &lcHdrFile..Open"
      *ENDIF
      *B039383,1 WSH 06/05/2005 [End]
      
    ENDIF
    
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
      lcIfExp = "lnTotPcs <> &lcHdrFile..TOT_PCS OR lnTotCart  <> &lcHdrFile..TOT_CART or lnTotWght <> &lcHdrFile..tot_wght"
    ENDIF 
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

    
  ENDIF
  *WSH 06/05/2005 Check here for open quantity for POs. [Start]
  
  *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
  *IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO 
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO  OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
  **: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
  
    lcIfExp = lcIfExp + " OR lnOpenQty <> &lcHdrFile..Open"
    
    *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
    FOR lnCstCntr = 1 TO 7
      lcCstCntr = STR(lnCstCntr ,1)
      lcIfExp = lcIfExp + " OR " + "&lcHdrFile..niCost"+lcCstCntr +" <> lnICost"+lcCstCntr
      lcIfExp = lcIfExp + " OR " + "&lcHdrFile..nfCost"+lcCstCntr +" <> lnfCost"+lcCstCntr
      lcIfExp = lcIfExp + " OR " + "&lcHdrFile..nflanCost"+lcCstCntr +" <> lnflanCost"+lcCstCntr
      lcIfExp = lcIfExp + " OR " + "&lcHdrFile..nlan_Cost"+lcCstCntr +" <> lnlan_Cost"+lcCstCntr
    ENDFOR   
    *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]

    
  ENDIF
  *WSH 06/05/2005 [End]
  
  IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
    lcIfExp = lcIfExp + " OR lnShipQty <> &lcHdrFile..Ship OR lnShipAmt <> &lcHdrFile..ShipAmt"
  ENDIF
  
  
  IF &lcIfExp
    SELECT ALLTRIM(&lcRebalHdr..cTempName)
    APPEND BLANK
    
    FOR mCount = 1 TO FCOUNT()
      lcFld = FIELD(mCount)
      REPLACE &lcFld WITH &lcHdrFile..&lcFld
    ENDFOR
    
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) <> LCALPACK
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

       REPLACE &laFChck[1,1] WITH lnQty
 
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
    ELSE
      REPLACE Tot_cart WITH lnTotCart  ,;
            tot_pcs  WITH lnTotPcs  ,;
            tot_wght with lnTotWght 
    ENDIF 
    *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
   
    
    *B127969,1 WSH 05/25/2005 Don't check for shipment amount if the custom trigger exists. [Start]
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) # lcArInv OR llChkInvAmt
    *B127969,1 WSH 05/25/2005 [End]
    
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      IF (UPPER(ALLTRIM(&lcRebalHdr..cTempName)) <> LCALPACK)
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

    
        REPLACE &laFChck[2,1] WITH lnAmt
              
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      ENDIF 
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
    
    *B127969,1 WSH 05/25/2005 Don't check for shipment amount if the custom trigger exists. [Start]
    ENDIF
    *B127969,1 WSH 05/25/2005 [End]

    *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
    *IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO 
    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO;
     OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
    *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
    
      REPLACE Open   WITH lnOpenQty,;
              Damage WITH lnDmgQty,;
              Cancel WITH lnCanQty
              
              
      *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
      FOR lnCstCntr = 1 TO 7
        lcCstCntr = STR(lnCstCntr ,1)
        REPLACE  niCost&lcCstCntr. WITH lnICost&lcCstCntr,;
		         nfCost&lcCstCntr. WITH lnfCost&lcCstCntr,;
          		 nlan_Cost&lcCstCntr. WITH lnlan_Cost&lcCstCntr,;
		         nflanCost&lcCstCntr. WITH lnflanCost&lcCstCntr
		         
      ENDFOR   
      *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]
              
              
    ELSE
      IF !EMPTY(laFChck[1,3])
        REPLACE &laFChck[1,3] WITH lnQty1
        REPLACE &laFChck[2,3] WITH lnAmt1
      ENDIF
    ENDIF

    IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD 
      REPLACE Ship    WITH lnShipQty ,;
              ShipAmt WITH lnShipAmt 

      REPLACE Cancel WITH lnCanQty , CancelAmt WITH lnCanAmt
    ENDIF

    IF llViewLog
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      
      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
      *laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + lcKeyVal + ' # ' + &lcHdrFile..&lcKeyVal + LANG_SMREBAL_DIFFAMT
      
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      *laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT,LANG_SMREBAL_CTNO,lcKeyVal) + ' # ' + &lcHdrFile..&lcKeyVal + LANG_SMREBAL_DIFFAMT
      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + ;
                    IIF(UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT,LANG_SMREBAL_CTNO,;
                    IIF((UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK),LANG_SMREBAL_PACKNO,lcKeyVal)) +;
                    IIF((UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK),"",' # ') + &lcHdrFile..&lcKeyVal + IIF((UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK),LANG_SMREBAL_PACKDIFF,LANG_SMREBAL_DIFFAMT)
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
      
      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
      
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance (commented out)
*!*	  IF llScanInFilt
*!*	    SELECT (lcFiltCurs)
*!*	    SKIP
*!*	    llExitLoop = EOF()
*!*	  ELSE
*!*	    SELECT (lcHdrFile)
*!*	    
*!*	    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	    *llExitLoop = !loHdrFile.GONEXT()
*!*	    llExitLoop = !lfGONEXT(lcHdrFile)
*!*	    *B128464,2 WSH 08/21/2005 [End]
*!*	    
*!*	  ENDIF
*!*	ENDDO
ENDSCAN
*B608487,1 WAM 03/19/2008 (End)
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <>'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

  *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 

  oProgress.FinishProgress()
    
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF 
  *E303410,1 TMI 09/01/2013 [End  ] 
  
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*IF TYPE('loConsInvH') = 'O'
*  loConsInvH = .NULL.
*ENDIF
*IF TYPE('loConsInvL') = 'O'
*  loConsInvL = .NULL.
*ENDIF
*IF TYPE('loCustomer') = 'O'
*  loCustomer = .NULL.
*ENDIF

*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
*B608487,1 WAM 03/19/2008 (End)

IF USED(lcConsInvH)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    USE IN (lcConsInvH)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    =gfCloseTable(lcConsInvH)
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
ENDIF
IF USED(lcConsInvL)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    USE IN (lcConsInvL)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    =gfCloseTable(lcConsInvL)
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
ENDIF
IF USED(lcCustomer)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    USE IN (lcCustomer)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    =gfCloseTable(lcCustomer)
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
ENDIF
*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
ENDIF
*B608487,1 WAM 03/19/2008 (End)

*B128464,2 WSH 08/21/2005 [End]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  WAIT CLEAR
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
SELECT ALLTRIM(&lcRebalHdr..cTempName)
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  USE
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ELSE
  =gfCloseTable(ALLTRIM(&lcRebalHdr..cTempName))
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfUpdHdDtl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Update function for the Header/Detail rebalance.
*!*************************************************************
*! Example   : = lfUpdHdDtl()
*!*************************************************************
FUNCTION lfUpdHdDtl

LOCAL lnCurrent, lnUpThermo, lnAlias
lnAlias   = SELECT(0)
lnCurrent = 0
lnSep     = AT(',', &lcRebalHdr..cMFiles)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*=SEEK(UPPER(SUBSTR(&lcRebalHdr..cMFiles,1,lnSep-1)), lcRebalDtl, 'cFileName')
*loHdrFile = &lcRebalDtl..cFileObj
*loHdrFile = &loHdrFile.
*lcHdrFile = loHdrFile.lcCursorView
*
*loDetFile = &lcRebalDtl..cFileObj
*loDetFile = &loDetFile.
*lcDetFile = loDetFile.lcCursorView
=SEEK(&lcRebalHdr..cItemName+UPPER(SUBSTR(&lcRebalHdr..cMFiles,1,lnSep-1)), lcRebalDtl)
lcHdrFile = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))

=SEEK(&lcRebalHdr..cItemName+ALLTRIM(UPPER(SUBSTR(ALLTRIM(&lcRebalHdr..cMFiles), lnSep + 1))), lcRebalDtl)
lcDetFile = ALLTRIM(EVALUATE(lcRebalDtl + '.cAliasName'))
*B128464,2 WSH 08/21/2005 [End]

IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.DBF')
  IF !USED(ALLTRIM(&lcRebalHdr..cTempName))
    USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0
  ENDIF
  SELECT ALLTRIM(&lcRebalHdr..cTempName)
  SET ORDER TO TAG (&lcRebalHdr..cTempName)
  
  lnUpThermo = RECCOUNT()
  =lfInitThermo(lnUpThermo, LANG_SMREBAL_UPDBAL)
  
  LOCATE
  SCAN
    lnCurrent = lnCurrent  + 1
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') <>'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
      IF EMPTY(lcStyMaj)
        *E303410,1 TMI 09/01/2013 [End  ] 
      oProgress.lblSecondLabel.Caption = lcKeyVal + ' : ' + ALLTRIM(&lcKeyVal)
      oProgress.CurrentProgress(lnCurrent)
      
        *E303410,1 TMI 09/01/2013 [Start] 
      ENDIF 
      *E303410,1 TMI 09/01/2013 [End  ] 
      
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ELSE
      IF MOD(lnCurrent ,CEILING(lnUpThermo / 10)) = 0
        loProgress.Percent     = lnCurrent / lnUpThermo
        loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+lcKeyVal + ' : ' + ALLTRIM(&lcKeyVal)
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)      
      ENDIF    
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loHdrFile.SEEK(&lcKeyType+&lcKeyVal)
    SCATTER MEMVAR
    IF EMPTY(lcKeyType)
      lcSeekCond = "Invoice = ?m.Invoice"
      
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
        lcSeekCond = "PACK_NO = ?m.PACK_NO"
      ENDIF
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

      
    ELSE
      IF UPPER(lcKeyType) = 'CORDTYPE'
        lcSeekCond = "CORDTYPE = ?m.cOrdType AND Order = ?m.Order"
      ELSE
        lcSeekCond = "cBusDocu  = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO"
      ENDIF
    ENDIF
    =lfSEEK(lcHdrFile, lcSeekCond, &lcKeyType+&lcKeyVal, .T.)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF DTOC(DADD_DATE)+CADD_TIME <> DTOC(&lcHdrFile..DADD_DATE)+&lcHdrFile..CADD_TIME
      DECLARE laRebMsg[3]
      DO CASE
        CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCARINV
          lcExpr = LANG_SMREBAL_INVNO
        CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
          lcExpr = LANG_SMREBAL_ORDNO
          
        *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
        CASE UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
          lcExpr = LANG_SMREBAL_PACKNO
        *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
        
  
*----------------- Not in This Release --------------------*
*!*          CASE UPPER(lcHdrFile) = 'CUTTKTH'
*!*            lcExpr = 'CutTkt #'
        OTHERWISE
          lcExpr = LANG_SMREBAL_PONO
      ENDCASE
      
      laRebMsg[1] = " "
      
      *B127969,1 WSH 05/26/2005 Write the correct expression. [Start]
      *laRebMsg[2] = LANG_SMREBAL_NOTUPD+lcExpr+&lcHdrFile..&lcKeyVal+ LANG_SMREBAL_DATACHG
      laRebMsg[2] = LANG_SMREBAL_NOTUPD + lcExpr + &lcKeyVal + LANG_SMREBAL_DATACHG
      *B127969,1 WSH 05/26/2005 [End]
      
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *loHdrFile.REPLACE(IIF(loHdrFile.llNative, "", "Rec_No WITH Rec_No"))
      *SELECT (loHdrFile.lcCursorUpdate)
      SELECT (lcHdrFile)
      *B128464,2 WSH 08/21/2005 [End]
      
      lcTFile = ALLTRIM(&lcRebalHdr..cTempName)
      
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCALPACK
      
        REPLACE TOT_pcs WITH &lcTFile..TOT_pcs,;
            tot_cart  WITH &lcTFile..tot_cart,;
            tot_wght WITH &lcTFile..tot_wght
        
      ELSE
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
  
        REPLACE &laFChck[1,1] WITH &lcTFile..&laFChck[1,1]
        REPLACE &laFChck[2,1] WITH &lcTFile..&laFChck[2,1]

      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
      ENDIF 
      *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
      *IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSPO;
           OR UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCMFCT
      *: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
      
        REPLACE Open   WITH &lcTFile..Open,;
                Damage WITH &lcTFile..Damage,;
                Cancel WITH &lcTFile..Cancel
                
        
        *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[Start]
        FOR lnCstCntr = 1 TO 7
          lcCstCntr = STR(lnCstCntr ,1)
          REPLACE  niCost&lcCstCntr. WITH &lcTFile..nICost&lcCstCntr,;
	 	          nfCost&lcCstCntr. WITH &lcTFile..nfCost&lcCstCntr,;
	 	          nlan_Cost&lcCstCntr. WITH &lcTFile..nlan_Cost&lcCstCntr,;
	 	          nflanCost&lcCstCntr. WITH &lcTFile..nflanCost&lcCstCntr
  
        ENDFOR   
        REPLACE POTotal  WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
        *: B608913,1 MMT 07/01/2009 Fix bug of not updating PO amounts[End]
                        
                
      ELSE
        IF !EMPTY(laFChck[1,3])
          REPLACE &laFChck[1,3] WITH &lcTFile..&laFChck[1,3]
          REPLACE &laFChck[2,3] WITH &lcTFile..&laFChck[2,3]
        ENDIF
      ENDIF
      
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCSOORD
        REPLACE Ship    WITH &lcTFile..Ship ,;
                ShipAmt WITH &lcTFile..ShipAmt
        
        REPLACE Cancel    WITH &lcTFile..Cancel ,;
                CancelAmt WITH &lcTFile..CancelAmt        
        
      ENDIF
      
      IF UPPER(ALLTRIM(&lcRebalHdr..cTempName)) = LCARINV AND &lcHdrFile..Status = 'V'
        REPLACE Ship     WITH 0 ,;
                ShipAmt  WITH 0 ,;
                VShip    WITH &lcTFile..&laFChck[1,1] ,;
                VShipAmt WITH &lcTFile..&laFChck[2,1]        
      ENDIF
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *=lfMasterUpd('loHdrFile', .T.)
    =lfMasterUpd('lcHdrFile', .T.)
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDSCAN
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <>'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 

    oProgress.FinishProgress()
      
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF 
    *E303410,1 TMI 09/01/2013 [End  ] 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]  
  
  SELECT ALLTRIM(&lcRebalHdr..cTempName)
  USE
  ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.DBF')
  ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.CDX')
ELSE
  llUpdt = .T.
  =lfStpHdDtl()
  =lfVryHdDtl()
  =lfUpdHdDtl()
  llUpdt = .F.
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lpMainReb
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : This is the old code of rebalance program done by Badran with some modifications
*! Parameters: lcToReb      && which items want to be rebalanced if called from another program .
*!             lcRebal      && item to be rebalanced.
*!             lcVrUpd      && verify or update.
*!             lcFilePath   && path to open the file from.
*!             lcCompName   && company name
*!             lcCompMods   && modules installed in each company.
*!*************************************************************
*! Example   : DO lpMainReb
*!*************************************************************
PROCEDURE lpMainReb
PARAMETERS lcToReb, lcRebal, lcVrUpd, lcFilePath, lcCompName, lcCompMods

llConvert  = .F.  && if it's .T. means that called from another program
llFinish   = .F.  && .T. if finish rebalnce selected company.
lnLocOnHnd = 0
lnLocOnHVl = 0

llOldRel   = .F.  && Variable to indicate if this ver. of 2.7 contain Ntokal fileds.
lnNPRVSQTY = 0    && Hold Old Qty.
lnNPRVSVAL = 0    && Hold Old Val.
llSysComp  = .F.

DECLARE laSetups[3,2]
laSetups[1,1] = 'M_Dyelot'
laSetups[2,1] = 'M_WareHouse'
laSetups[3,1] = 'M_MATDYE'

*--------------------  Not in this Release ------------------*
*!*  *-- if called from another program.
*!*  IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
*!*    *-- llSysComp : .T. if this program open company file.
*!*    llSysComp = gfOpenFile(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'Ccomp_id','SH')
*!*    
*!*    IF TYPE('lcCurrComp_ID') $ 'UL' OR EMPTY(lcCurrComp_ID)
*!*      lcCurrComp_ID = oAriaApplication.ActiveCompanyID
*!*    ENDIF

*!*    SET ORDER TO TAG Ccomp_id IN SYCCOMP
*!*    *-- if you find passed company in company files.
*!*    IF SEEK(lcCurrComp_ID,'SYCCOMP')
*!*      lcCmPath  = gfGetDataDir(ALLTRIM(LOWER(SYCCOMP.cCom_dDir)))
*!*      lcCmName  = ALLTRIM(SYCCOMP.cCom_Name)  
*!*      lcInstMod = SYCCOMP.mModlSet
*!*      llConvert = .T. && avoiding run screen.  
*!*    ELSE
*!*      RETURN
*!*    ENDIF
*!*  ENDIF

*-- llOpnError : Flag to detect if you can lock file.
*-- llDyelot   : Flag to detect that current company use dyelot.
*-- llMultWare : Flag to detect that current company use multi warehouse or not.
STORE .F. TO llOpnError,llDyelot,llMultWare,llFabDye

*-- if come from another program.
IF llConvert

*--------------------  Not in this Release ------------------*
*!*    STORE 0 TO lnWip,lnIntrans,lnWo,lnOrd,lnAlo,lnShp,;
*!*               lnRet,lnRa,lnOnOrder,lnOnHand,lnStock
*!*    
*!*    lcInsModul = ''  && have installed modules.

*!*    *-- Style Work Orders.
*!*    IF ('WO' $ lcToReb) OR ('WI' $ lcToReb)
*!*      lcInsModul = IIF('PO' $ lcInstMod,'PO','')
*!*      lcInsModul = IIF('MF' $ lcInstMod, IIF(EMPTY(lcInsModul),'MF',lcInsModul+',MF'),lcInsModul)

*!*      IF ('WI' $ lcToReb)
*!*        lnWip = 1
*!*      ENDIF

*!*      IF ('WO' $ lcToReb) AND (('PO' $ lcInsModul) OR ('MF' $ lcInsModul))
*!*        lnWo = 1
*!*      ENDIF

*!*    ENDIF  

*!*    *-- Style InTransit.
*!*    IF 'IN' $ lcToReb AND ('PO' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'PO',;
*!*                   IIF('PO' $ lcInsModul,lcInsModul,lcInsModul+',PO'))
*!*      lnIntrans = 1
*!*    ENDIF  
*!*    
*!*    *-- Return auth.
*!*    IF 'RA' $ lcToReb AND ('RM' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'RM',lcInsModul+',RM')
*!*      lnRa = 1
*!*    ENDIF  

*!*    IF 'ONO' $ lcToReb AND ('MA' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
*!*      lnOnOrder = 1
*!*    ENDIF
*!*    
*!*    *-- Material OnHand.
*!*    *--[Coment until check with Omar] IF 'MH' $ lcToReb AND ('MA' $ lcInstMod)
*!*    *--[Coment until check with Omar]   lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
*!*    *--[Coment until check with Omar]   lnOnHand = 1
*!*    *--[Coment until check with Omar] ENDIF  
*!*    IF 'MH' $ lcToReb AND ('MA' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
*!*      lnOnHand = 1
*!*    ENDIF

*!*    *-- Invoice details.
*!*    IF 'SH' $ lcToReb AND ('AR' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'AR',lcInsModul+',AR')
*!*      lnShp = 1
*!*    ENDIF
*!*    
*!*    *-- Sales Order or Picking ticktes.
*!*    IF (('OR' $ lcToReb) .OR. ('AL' $ lcToReb)) AND (('SO' $ lcInstMod) .OR. ('AL' $ lcInstMod))
*!*      lcInsModul = IIF('SO' $ lcInstMod,IIF(EMPTY(lcInsModul),'SO',lcInsModul+',SO'),lcInsModul)
*!*      lcInsModul = IIF('AL' $ lcInstMod,IIF(EMPTY(lcInsModul),'AL',lcInsModul+',AL'),lcInsModul)
*!*      
*!*      IF ('OR' $ lcToReb)
*!*        lnOrd = 1
*!*      ENDIF
*!*      
*!*      IF ('AL' $ lcToReb)
*!*        lnAlo = 1
*!*      ENDIF
*!*    ENDIF  
*!*    
*!*    *-- Return Details.
*!*    IF 'RE' $ lcToReb AND ('RM' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'RM',lcInsModul+',RM')
*!*      lnRet = 1
*!*    ENDIF
*!*    
*!*    *-- Inventory Control.
*!*    IF 'ST' $ lcToReb AND ('IC' $ lcInstMod)
*!*      lcInsModul = IIF(EMPTY(lcInsModul),'IC',lcInsModul+',IC')
*!*      lnStock = 1
*!*    ENDIF

*!*    IF !EMPTY(lcInsModul)
*!*      lcFilePath = lcCmPath
*!*      =lfRebalanc(lcCurrComp_ID,lcCmName,lcCmPath,lcInsModul)  && Rebalance Passed company.
*!*    ENDIF  
*!*    
ELSE
  STORE 0 TO lnSelRebal,lnOldVal,lnMaxToSel
    *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
    =lfSTRTOFILE("= lfRebalanc(lcCompName, lcFilePath, lcCompMods)")
    *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
  = lfRebalanc(lcCompName, lcFilePath, lcCompMods)  && Rebalnce this company
ENDIF

*-- Functions and procedures.
*--------------------------------------------------------------

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Get current object old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfWOldVal()
*!****pm*********************************************************
*!
FUNCTION lfWOldVal

laOldVal = EVALUATE(SYS(18))
*-- end of lfWOldVal.

*!*************************************************************
*! Name      : lfRebalanc
*! Developer : Wael M. abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Rebalnce Code
*!*************************************************************
*! Example   : = lfRebalanc()
*!*************************************************************
FUNCTION lfRebalanc
PARAMETERS lcCurrName, lcCurrPath, lcCurModul

*-- if pass from another program and no passed codes.
IF TYPE('lcCurrPath') $ 'UL' OR EMPTY(lcCurrPath) OR ;
   TYPE('lcCurrName') $ 'UL' OR EMPTY(lcCurrName) OR ;
   TYPE('lcCurModul') $ 'UL' OR EMPTY(lcCurModul)
  lcCurrPath = oAriaApplication.DataDir
  lcCurrName = oAriaApplication.ActiveCompanyName
  lcCurModul = oAriaApplication.CompanyInstalledModules
ENDIF

llOpnError = .F.  && initially there is No errors in open files.

DIMENSION laFileStr[1,9]

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*laFileStr  = ''  && array hold files to open.
laFileStr  = .F.  && array hold files to open.
*B128464,2 WSH 08/21/2005 [End]

lcHdrLnRel = ''  && Variable to hold Relation String between Header and Detail files

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
lcHdrLnCnd = ''  && Variable to hold Relation Condition between Header and Detail files
*B128464,2 WSH 08/21/2005 [End]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') = 'C'
  FOR lnT=1 TO ALEN(laSetUps,1)
     laSetUps[lnT,2]= gfgetmemvar(laSetUps[lnT,1], lcCurrComp_ID)
  ENDFOR
ELSE
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *-- Setup flags for installed modules.
  =gfGetMemVar(@laSetups,lcCurrComp_ID)
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
llDyelot    = IIF(lcInvType = '0001', (UPPER(ALLTRIM(laSetups[1,2])) = 'Y'), (UPPER(ALLTRIM(laSetups[3,2])) = 'Y'))
llMultWare  = (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
llWip_nWo   = ('MF' $ lcCurModul OR 'PO' $ lcCurModul) AND ;
              ((lnWip=1) OR (lnWo=1))
              
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
*llInTrans   = ('PO' $ lcCurModul) AND (lnIntrans=1)
llInTrans   = ('PO' $ lcCurModul) AND (lnIntrans=1 OR lnMIntrans = 1)
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record    [Start]
*llPackRebal = ('AL' $ lcCurModul) and  (lnPackReb = 1)
llPackRebal = ('AL' $ lcCurModul) and TYPE('lnPackReb')<> 'U' AND (lnPackReb = 1)
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record    [End]
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]



llSoOrder   = ('SO' $ lcCurModul) AND ((lnOrd=1) OR (lnAlo=1))
llInvoice   = ('AR' $ lcCurModul) AND (lnShp=1)
llRetMemo   = ('RM' $ lcCurModul) AND (lnRet=1)
llRauth     = ('RM' $ lcCurModul) AND (lnRa=1)


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*llStyFile   = (lnStock = 1) OR llWip_nWo OR llInTrans  OR llSoOrder OR ;
              llInvoice OR llRetMemo OR llRauth
llStyFile   = (lnStock = 1) OR llWip_nWo OR llInTrans  OR llSoOrder OR ;
              llInvoice OR llRetMemo OR llRauth OR llPackRebal  
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]              

llWipAdj    = llWip_nWo AND (lnWip=1) AND 'MF' $ lcCurModul
llStyDyeFl  = llStyFile 

*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
*llMaterial  = ('MA' $ lcCurModul) AND ((lnOnOrder=1) OR (lnOnHand=1))
llMaterial  = ('MA' $ lcCurModul) AND ((lnOnOrder=1) OR (lnOnHand=1) OR (lnMatUsg=1) OR (lnMIntrans=1))
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

llFabDyeFl  = llMaterial

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llMaterial")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF llMaterial
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loFabric, loFabDye, loPOSHDR, loPOSLN, loMatInvJL
  *B128464,2 WSH 08/21/2005 [End]
  
  lcFabAls    = gfTempName()  && FABRIC alias
  lcFDyAls    = gfTempName()  && FABDYE alias
  lcPoLAls    = gfTempName()  && POSLN  alias
  lcPoHAls    = gfTempName()  && POFHDR alias
  lcPoLnAls   = gfTempName()  && POFLN alias 
  lcTmpMDyeFl = gftempName()
  lcMatJlAls  = gfTempName()  && MATINVJL alias 
  
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  lcBOMCost   = gfTempName()  
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
 
  *: B607982,1 MMT 02/02/2007 Fix bug of Wrong update of material usage[Start]
  lcBOMLine   = gfTempName()  
  *: B607982,1 MMT 02/02/2007 Fix bug of Wrong update of material usage[End]

  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
  lcPack_lin = gfTempName()  
  lcPack_hdr = gfTempName()  
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('ITEM', 'STYLE', 'lcFabAls', 'Material module', 'loFabric', .T.)
  *= lfFilesStr('ITEMLOC', 'STYDYE', 'lcFDyAls', 'Material module', 'loFabDye', .T.)
  *= lfFilesStr('POSHDR', 'POSHDR', 'lcPoHAls', 'Material module', 'loPOSHDR', .T.)
  *= lfFilesStr('POSLN', 'POSLNS', 'lcPoLAls', 'Material module', 'loPOSLN', .T.)
  = lfFilesStr('ITEM', 'STYLE', lcFabAls, 'Material module', 'loFabric', .T.)
  = lfFilesStr('ITEMLOC', 'STYDYE', lcFDyAls, 'Material module', 'loFabDye', .T.)
  = lfFilesStr('POSHDR', 'POSHDR', lcPoHAls, 'Material module', 'loPOSHDR')
  = lfFilesStr('POSLN', 'POSLNS', lcPoLAls, 'Material module', 'loPOSLN')
  *B128464,2 WSH 08/21/2005 [End]

  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  = lfFilesStr('BOMCost', 'BOMCost', lcBOMCost, 'Material module', 'loBOMCOST')
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

  *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
  = lfFilesStr('BOMLINE', 'BOMLINEU', lcBOMLine, 'Material module', 'loBOMLINE')
  *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]

  
ELSE
  lnOnOrder = 0
  lnOnHand  = 0
  
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  lnMatUsg  = 0
  lnMIntrans = 0
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
  
ENDIF

*-- Add Style and StyDye Files to string array.
  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llStyFile")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF llStyFile
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loStyle, loStyDye
  *B128464,2 WSH 08/21/2005 [End]
  
  lcStyAls = gfTempName()  && STYLE alias 
  lcSDyAls = gfTempName()  && STYDYE alias 
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('STYLE', 'STYLE', 'lcStyAls', .F., 'loStyle')
  *= lfFilesStr('STYDYE', 'STYDYE', 'lcSDyAls', .F., 'loStyDye')
  = lfFilesStr('STYLE', 'STYLE', lcStyAls, .F., 'loStyle', .T.)
  = lfFilesStr('STYDYE', 'STYDYE', lcSDyAls, .F., 'loStyDye', .T.)
  *B128464,2 WSH 08/21/2005 [End]
  
ELSE
  lnStock = 0
ENDIF

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF lnStock = 1")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF lnStock = 1
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loStyInvJL
  *B128464,2 WSH 08/21/2005 [End]
  
  lcStyJlAls = gfTempName()  && STYINVJL alias 
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('STYINVJL', 'STYDATE', 'lcStyJlAls', .F., 'loStyInvJL')
  = lfFilesStr('STYINVJL', 'STYDATE', lcStyJlAls, .F., 'loStyInvJL')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF lnOnHand = 1")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF lnOnHand = 1
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('ITEMJRNL', 'STYDATE', 'lcMatJlAls', .F., 'loMatInvJL', .T.)
  = lfFilesStr('ITEMJRNL', 'STYDATE', lcMatJlAls, .F., 'loMatInvJL')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

*------------ Not in this Release ---------------------*
*!*  *-- Add C/T Files to string array.

*!*  IF llWip_nWo AND ('MF' $ lcCurModul)
*!*    = lfFilesStr('POSHDR','POSHDR',lcCutHdAls,'Manufacture module') AND lfFilesStr('	','CUTTKTLS',lcCutLnAls,'Manufacture module')
*!*  ENDIF
*-- Add P/O Files to string array.

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*IF llInTrans OR (llWip_nWo AND ('PO' $ lcCurModul))
IF llInTrans OR (llWip_nWo AND ('PO' $ lcCurModul OR 'MF' $ lcCurModul))
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loPOSHDR, loPOSLN
  *B128464,2 WSH 08/21/2005 [End]
  
  lcPoHAls   = gfTempName()  && POSHDR alias 
  lcPoLAls   = gfTempName()  && POSLN  alias 
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('POSHDR', 'POSHDR', 'lcPoHAls', 'Purchase Order module', 'loPOSHDR', .T.)
  *= lfFilesStr('POSLN', 'POSLNS', 'lcPoLAls', 'Purchase Order module', 'loPOSLN', .T.)
  = lfFilesStr('POSHDR', 'POSHDR', lcPoHAls, 'Purchase Order module', 'loPOSHDR')
  = lfFilesStr('POSLN', 'POSLNS', lcPoLAls, 'Purchase Order module', 'loPOSLN')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

*-- Add Wip adjustment File to string array.
IF llWipAdj
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loWIPAdj
  *B128464,2 WSH 08/21/2005 [End]
  
  lcWipJAls  = gfTempName()  && WIPADJ alias         
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('WIPADJ', 'WIPADJ', 'lcWipJAls', .F., 'loWIPAdj')
  = lfFilesStr('WIPADJ', 'WIPADJ', lcWipJAls, .F., 'loWIPAdj')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

*-- Add Sales order Files to string array.
  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llSoOrder")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF llSoOrder
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loOrdHdr, loOrdLine
  *B128464,2 WSH 08/21/2005 [End]
  
  lcOrdHAls  = gfTempName()  && ORDHDR alias 
  lcOrdLAls  = gfTempName()  && ORDLINE alias 
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('ORDHDR', 'ORDHDR', 'lcOrdHAls', 'Sales Order module', 'loOrdHdr')
  *= lfFilesStr('ORDLINE', 'ORDLINES', 'lcOrdLAls', 'Sales Order module', 'loOrdLine')
  = lfFilesStr('ORDHDR', 'ORDHDR', lcOrdHAls, 'Sales Order module', 'loOrdHdr')
  = lfFilesStr('ORDLINE', 'ORDLINES', lcOrdLAls, 'Sales Order module', 'loOrdLine')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record    [Start]
*IF llPackRebal
IF TYPE('llPackRebal') <> 'U' AND llPackRebal
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record    [End]
  lcPackHAls  = gfTempName()  && PACK_HDR alias 
  lcPackLAls  = gfTempName()  && PACK_LINE alias 
  = lfFilesStr('PACK_HDR', 'PACK_HDR', lcPackHAls  , 'Sales Order Allocation module', 'loPack_Hdr')
  = lfFilesStr('PACK_LIN', 'PACK_LIN', lcPackLAls  , 'Sales Order Allocation module', 'loPack_Lin')
ENDIF 
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*-- Add A/R Files to string array.
IF llInvoice
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE '' TO loInvHdr, loInvLine
  *B128464,2 WSH 08/21/2005 [End]
  
  lcInvHAls = gfTempName()  && INVHDR alias 
  lcInvLAls = gfTempName()  && INVLINE alias 
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= lfFilesStr('INVHDR', 'INVHDR', 'lcInvHAls', 'Account Receivable module', 'loInvHdr')
  *= lfFilesStr('INVLINE', 'INVLINES', 'lcInvLAls', 'Account Receivable module', 'loInvLine')
  = lfFilesStr('INVHDR', 'INVHDR', lcInvHAls, 'Account Receivable module', 'loInvHdr')
  = lfFilesStr('INVLINE', 'INVLINES', lcInvLAls, 'Account Receivable module', 'loInvLine')
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

*------------ Not in this Release ---------------------*
*!*  *-- Add RM credit memo Files to string array.
*!*  IF llRetMemo
*!*    = lfFilesStr('RETHDR','RETHDR',lcRetHdAls,'Ret. Merch. Credit memo files') AND lfFilesStr('RETLINE','RETLINES',lcRetLnAls,'Ret. Merch. Credit memo files')
*!*  ENDIF

*!*  *-- Add RM return authorization Files to string array.
*!*  IF llRauth
*!*    = lfFilesStr('RETAUTH','RETAUTH',lcRaHdAls,'Ret. Merch. Authorization files') AND lfFilesStr('RALINE','RALINES',lcRaLnAls,'Ret. Merch. Authorization files')
*!*  ENDIF


*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
IF llRetMemo
  lcRetHdAls = gfTempName() 
  lcRetLnAls = gfTempName() 
  = lfFilesStr('RETHDR','RETHDR',lcRetHdAls,'Ret. Merch. Credit memo files') AND lfFilesStr('RETLINE','RETLINES',lcRetLnAls,'Ret. Merch. Credit memo files')
ENDIF
IF llRauth
  lcRaHdAls = gfTempName() 
  lcRaLnAls = gfTempName() 
  = lfFilesStr('RETAUTH','RETAUTH',lcRaHdAls,'Ret. Merch. Authorization files') AND lfFilesStr('RALINE','RALINES',lcRaLnAls,'Ret. Merch. Authorization files')
ENDIF
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]

IF !EMPTY(laFileStr[1,1])
  = lfOpenFls()  && Open files in string array.
ENDIF  

*-- if there is no errors in open files start rebalancing.
IF !EMPTY(laFileStr[1,1]) AND !llOpnError
    *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
    =lfSTRTOFILE("= lfStartReb()")
    *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
  = lfStartReb()  && Start rebalance.
ENDIF

*-- Close company opend files.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*STORE .NULL. TO loStyle, loStyDye, loFabric, loFabDye, loStyInvJL, loMatInvJL
*STORE .NULL. TO loPOSHDR, loPOSLN, loOrdHdr, loOrdLine, loWipAdj, loInvHdr, loInvLine
=lfCloseFls()
*B128464,2 WSH 08/21/2005 [End]

*-- end of lfRebalanc.

*!*************************************************************
*! Name      : lfFilesStr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Fill file string array.
*!*************************************************************
*! Example   : = lfFilesStr()
*!*************************************************************
FUNCTION lfFilesStr

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS lcFileName, lcFileTag, lcAliasNam, lcErrMesg, lcObjName, llSQL
LPARAMETERS lcFileName, lcFileTag, lcAliasNam, lcErrMesg, lcObjName, llUpdatable
*B128464,2 WSH 08/21/2005 [End]

IF TYPE('lcErrMesg') $ 'UL' OR EMPTY(lcErrMesg)
  lcErrMesg = PROPER(lcFileName) + ' File'
ENDIF

IF !EMPTY(laFileStr[1,1])
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *DIMENSION laFileStr[ALEN(laFileStr,1)+1,6]
  DIMENSION laFileStr[ALEN(laFileStr,1)+1,9]
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

laFileStr[ALEN(laFileStr,1),1] = lcFileName  && File Name.
laFileStr[ALEN(laFileStr,1),2] = lcFileTag   && Active index.
laFileStr[ALEN(laFileStr,1),3] = lcErrMesg   && Error message.
laFileStr[ALEN(laFileStr,1),4] = lcAliasNam  && Alias name.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*laFileStr[ALEN(laFileStr,1),5] = lcObjName   && RDAC file objeect name.
*laFileStr[ALEN(laFileStr,1),6] = llSQL       && If File if SQL or Native.
laFileStr[ALEN(laFileStr,1),8] = llUpdatable  && Will Hold the SQL Update Statement
*B128464,2 WSH 08/21/2005 [End]

RETURN .T.
*-- end of lfFilesStr.

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Open files in file string array.
*!*************************************************************
*! Example   : = lfOpenFls()
*!*************************************************************
FUNCTION lfOpenFls
LOCAL lnI, lnBreak, lcObjName

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
LOCAL llNative, lcIndexFlds, lcIndexExp, llOpnError
llOpnError = .F.
*B128464,2 WSH 08/21/2005 [End]

lnBreak = 0

*-- loop around file string array to open files and lock it.
FOR lnI = 1 TO ALEN(laFileStr,1)
  lcObjName = laFileStr[lnI,5]
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *&laFileStr[lnI,5] = CREATEOBJECT('RemoteTable', laFileStr[lnI,1], laFileStr[lnI,2], &laFileStr[lnI,4], SET("Datasession"), lcCurrComp_ID)
  
  *IF TYPE(laFileStr[lnI,5]) # 'O'
  llNative = lfCreateObject(laFileStr[lnI,1], laFileStr[lnI,2], laFileStr[lnI,4], @lcIndexFlds, @lcIndexExp)
  
  IF !USED(laFileStr[lnI,4])
  *B128464,2 WSH 08/21/2005 [End]
  
    *-- Message : XXX Not found in data file.
    *--                   <Ok>
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]                     
    IF TYPE('lcXMLFileName') <> 'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      = gfModalGen('TRM00001B00000', 'Alert', laFileStr[lnI,1])
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    llOpnError = .T.
  ENDIF  
  
  *--WRITE CODE FOR LOCKING THE TABLE HERE
  
  *-- if file open error or this file is used by another and you press Cancel.
  IF llOpnError
    lnBreak = lnI
    EXIT    && exit for loop.
  ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  laFileStr[lnI,5] = lcIndexFlds
  laFileStr[lnI,6] = llNative
  laFileStr[lnI,7] = IIF(llNative, '', lfCrtGoNext(laFileStr[lnI,1], laFileStr[lnI,2], laFileStr[lnI,4], laFileStr[lnI,5], laFileStr[lnI,6]))
  laFileStr[lnI,8] = IIF(!llNative AND laFileStr[lnI,8], lfCrtUpdStat(laFileStr[lnI,1], lcIndexFlds, laFileStr[lnI,2]), '')
  laFileStr[lnI,9] = lcIndexExp
  *B128464,2 WSH 08/21/2005 [End]
  
ENDFOR

*-- end if lfOpenFls.

*!*************************************************************
*! Name      : lfStartReb
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Start rebalance if records found greater than 0.
*!*************************************************************
*! Example   : = lfStartReb()
*!*************************************************************
FUNCTION lfStartReb

IF !llFinish
  llFinish  = .T.

  IF !llConvert
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') <>'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      WAIT WINDOW LANG_SMREBAL_STARTREB + lcCurrName NOWAIT
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  ENDIF  
ENDIF

*-- If Material Module.
IF llMaterial
  lnWip      = lnOnOrder
  llWip_nWo  = (lnOnOrder = 1)
  lnStock    = lnOnHand
  lcVrUpWip  = lcVrUpMO
  lcVrUpStk  = lcVrUpMH
  llStyDyeFl = llFabDyeFl
  
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  lcVrUpTrn =  lcVrUpTrnM
  lnIntrans = lnMIntrans
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loStyle    = loFabric
  *loStyDye   = loFabDye
  *B128464,2 WSH 08/21/2005 [End]
  
  lcStyAls   = lcFabAls
  lcSDyAls   = lcFDyAls
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loStyInvJL = loMatInvJL
  *B128464,2 WSH 08/21/2005 [End]
  
  lcStyJlAls = lcMatJlAls
ENDIF      && end if material module.

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("= lfUpdSty()")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
= lfUpdSty()  && Update Style/Item file

IF llDyelot AND TYPE("lcTmpDyeFl") $ "C" AND USED(lcTmpDyeFl)
  USE IN (lcTmpDyeFl)
ENDIF

IF llFinish AND !llConvert
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <>'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] don't issue this when called from STYLE screen
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 
    WAIT WINDOW LANG_SMREBAL_FINISH + lcCurrName TIMEOUT 1
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF
    *E303410,1 TMI 09/01/2013 [End  ] 
    WAIT CLEAR
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End] 
ENDIF

llFinish = .F.
*-- End of lfStartReb.

*!*************************************************************
*! Name      : lfRelatFls
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Make relation between Open files.
*!*************************************************************
*! Example   : = lfRelatFls()
*!*************************************************************
FUNCTION lfRelatFls
LPARAMETERS lcOptType

*-- Make relation between files.
IF llMaterial
  lcHdrLnRel = 'cBusDocu + cStyType + PO'
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  lcHdrLnCnd = 'cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO'
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF
  
IF llStyFile
  *-- P/O Module.
  IF INLIST(lcOptType, 'WIP', 'WO', 'TRN')
    lcHdrLnRel = 'cBusDocu + cStyType + PO'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    lcHdrLnCnd = 'cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO'
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF

  *-- Sales order module.
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
  *IF lcOptType = 'ORD'
  IF lcOptType = 'ORD' OR lcOptType = 'ALO'
  *: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]
  
    lcHdrLnRel = '"O" + Order'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    lcHdrLnCnd = 'cOrdType = ?m.cOrdType AND Order = ?m.Order'
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF

  *-- A/R module.
  IF lcOptType = 'SHP'
    lcHdrLnRel = 'Invoice'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    lcHdrLnCnd = 'Invoice = ?m.Invoice'
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF

*------------------- Not in this Release ------------------*
*!*    *-- Credit memo files.
*!*    IF llRetMemo
*!*      IF lcVrUpd = 'V'
*!*        SELECT lcStyAls
*!*      ELSE
*!*        SELECT (lcStyAls)
*!*      ENDIF
*!*      SET RELATION TO STYLE INTO (lcRetLnAls) ADDITIVE

*!*      SELECT (lcRetLnAls)
*!*      SET RELATION TO CrMemo INTO (lcRetHdAls)
*!*    ENDIF

*!*    *-- Return authorization files.
*!*    IF llRauth
*!*      IF lcVrUpd = 'V'
*!*        SELECT lcStyAls
*!*      ELSE
*!*        SELECT (lcStyAls)
*!*      ENDIF
*!*      SET RELATION TO STYLE INTO (lcRaLnAls) ADDITIVE

*!*      SELECT (lcRaLnAls)
*!*      SET RELATION TO RaNo INTO (lcRaHdAls)
*!*    ENDIF
*!*    


  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
  *-- Credit memo files.
  IF lcOptType = 'RET'
    lcHdrLnRel = 'CrMemo'
    lcHdrLnCnd = 'CrMemo = ?m.CrMemo '
  ENDIF

  *-- Return authorization files.
  IF lcOptType = 'RA'
    lcHdrLnRel = 'RaNo'
    lcHdrLnCnd = 'RaNo = ?m.RaNo'
  ENDIF
  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]



ENDIF
*-- end of lfRelatFls.

*!*************************************************************
*! Name      : lfUpdSty
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Rebalance Style file.
*!*************************************************************
*! Example   : = lfUpdSty()
*!*************************************************************
FUNCTION lfUpdSty

DIMENSION laUpdatArr[2,4]        && array used to update stydye file.

*-- Style On-Hand case.
IF lnStock = 1
  *-- laStock    : Array hold stock per style.
  *-- laLocStock : Array hold stock per style,location.
  *-- 1 -> 8 hold Stk1 -> Stk8
  *-- 9  hold Totstk
  *-- 10 hold nStkVal
  DIMENSION laStock[10] , laLocStock[10] , laStkArr[1,4]
  STORE 0 TO laStock,laLocStock

  *-- laStkArr : Array hold Stock arrays information.
  laStkArr[1,1] = 'lnStock=1'
  laStkArr[1,2] = 'laLocStock'
  laStkArr[1,3] = 'STK'
  laStkArr[1,4] = 'NSTK'

  IF llDyelot
    lcTmpDyeFl = gfTempName()
    =lfCrtTmpDy()
  ENDIF  
ENDIF

*-- WIP or nWo cases
IF llWip_nWo
  IF lnWip = 1
    DIMENSION laWip[9],laTWip[9],laSWip[9]
    STORE 0 TO laWip,laTWip,laSWip
    
    DIMENSION laWipArr[1,4]
    laWipArr[1,1] = 'lnWip = 1'
    laWipArr[1,2] = 'laWIP'
    laWipArr[1,3] = 'WIP'
    laWipArr[1,4] = 'QTY'
  ENDIF
  
  IF lnWo = 1
    DIMENSION laWo[9],laTempWo[9],laTWo1[9],laSWo1[9]
    STORE 0 TO laWo,laTempWo,laTWo1,laSWo1
    
    DIMENSION laWoArr[1,4]
    laWoArr[1,1] = 'lnWo = 1'
    laWoArr[1,2] = 'laWo'
    laWoArr[1,3] = 'NWO'
    laWoArr[1,4] = 'QTY'
  ENDIF
ENDIF

*-- Intransit case
IF llInTrans
  IF lnIntrans = 1
    DIMENSION laIntrans[9]
    laIntrans = 0
    
    DIMENSION laTrnArr[1,4]
    laTrnArr[1,1] = 'lnIntrans = 1'
    laTrnArr[1,2] = 'laIntrans'
    laTrnArr[1,3] = 'INTRANS'
    laTrnArr[1,4] = 'QTY'
  ENDIF
ENDIF  

*-- Sales Order module ( Ord 1->8 and Alo 1->8 )
IF llSoOrder
  IF lnOrd = 1
    DIMENSION laOrder[9]
    laOrder = 0
    
    DIMENSION laOrdArr[1,4]
    laOrdArr[1,1] = 'lnOrd = 1'
    laOrdArr[1,2] = 'laOrder'
    laOrdArr[1,3] = 'ORD'
    laOrdArr[1,4] = 'QTY'
  ENDIF  
  
  IF lnAlo = 1
    DIMENSION laAlo[9]
    laAlo = 0
    
    IF TYPE('laAloArr') $ 'UL'
      DIMENSION laAloArr[1,4]
    ELSE
      DIMENSION laAloArr[ALEN(laAloArr,1) + 1,4]
    ENDIF
    
    laAloArr[ALEN(laAloArr,1),1] = 'lnAlo = 1'
    laAloArr[ALEN(laAloArr,1),2] = 'laAlo'
    laAloArr[ALEN(laAloArr,1),3] = 'ALO'
    laAloArr[ALEN(laAloArr,1),4] = 'PIK'
  ENDIF  
ENDIF  

IF llInvoice
  *-- Note: Because llInvoice Flag contains (lnShp = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnShp = 1
    DIMENSION laShp[9]
    laShp = 0
    DIMENSION laInvArr[1,4]
    laInvArr[1,1] = 'lnShp = 1'
    laInvArr[1,2] = 'laShp'
    laInvArr[1,3] = 'SHP'
    laInvArr[1,4] = 'QTY'
  ENDIF
ENDIF

*-------------------- Not in this Release ------------------*
*                                                           *
*                                                           *
*!*  IF llRauth
*!*    *-- Note: Because llRauth Flag contains (lnRa = 1) Condition alone
*!*    *--       I can use this condition only for Defination and code 
*!*    *--       but for get more flexiability to code to increase another 
*!*    *--       conditions in the future, I make separate block code
*!*    *--       under its unique condition .
*!*    IF lnRa = 1
*!*      DIMENSION laRa[9]
*!*      laRa = 0
*!*      DIMENSION laAuthArr[1,4]
*!*      laAuthArr[1,1] = 'lnRa = 1'
*!*      laAuthArr[1,2] = 'laRa'
*!*      laAuthArr[1,3] = 'RA'
*!*      laAuthArr[1,4] = 'NOPNQTY'
*!*    ENDIF
*!*  ENDIF

*!*  IF llRetMemo
*!*    *-- Note: Because llRetMemo Flag contains (lnRet = 1) Condition alone
*!*    *--       I can use this condition only for Defination and code 
*!*    *--       but for get more flexiability to code to increase another 
*!*    *--       conditions in the future, I make separate block code
*!*    *--       under its unique condition .
*!*    IF lnRet = 1
*!*      DIMENSION laRet[9]
*!*      laRet = 0
*!*      DIMENSION laMemoArr[1,4]
*!*      laMemoArr[1,1] = 'lnRet = 1'
*!*      laMemoArr[1,2] = 'laRet'
*!*      laMemoArr[1,3] = 'RET'
*!*      laMemoArr[1,4] = 'QTY'
*!*    ENDIF  
*!*  ENDIF

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llRauth")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
IF llRauth
  *-- Note: Because llRauth Flag contains (lnRa = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnRa = 1
    DIMENSION laRa[9]
    laRa = 0
    DIMENSION laAuthArr[1,4]
    laAuthArr[1,1] = 'lnRa = 1'
    laAuthArr[1,2] = 'laRa'
    laAuthArr[1,3] = 'RA'
    laAuthArr[1,4] = 'NOPNQTY'
  ENDIF
ENDIF

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llRetMemo")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF llRetMemo
  *-- Note: Because llRetMemo Flag contains (lnRet = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnRet = 1
    DIMENSION laRet[9]
    laRet = 0
    DIMENSION laMemoArr[1,4]
    laMemoArr[1,1] = 'lnRet = 1'
    laMemoArr[1,2] = 'laRet'
    laMemoArr[1,3] = 'RET'
    laMemoArr[1,4] = 'QTY'
  ENDIF  
ENDIF
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]

PRIVATE lcDyeWhile
lcDyeWhile = IIF(lcInvType = '0002', 'cInvType+', '') + 'STYLE+CWARECODE+DYELOT=' + IIF(lcInvType = '0002', 'lcInvType+', '') + '&lcStyAls..STYLE'

LOCAL llExitLoop, llFirstRec, lnMajLen, lcTermoMsg, lnUpThermo, lnThermNo, llScanInFilt
llExitLoop   = .F.
llFirstRec   = .T.
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
*lnMajLen     = LEN(gfItemMask('PM', '', lcInvType))
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  lnMajLen   = LEN(loItemMask.Do('PM', '', lcInvType))
ELSE
  lnMajLen     = LEN(gfItemMask('PM', '', lcInvType))
ENDIF 
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]
lnThermNo    = 0
llScanInFilt = .F.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
LOCAL lcWhereCond
lcWhereCond = ''
*B128464,2 WSH 08/21/2005 [End]

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
IF lcInvType = '0002' AND FILE(oAriaApplication.WorkDir + lcSelFabric+ '.DBF') AND !USED(lcSelFabric)
  USE (oAriaApplication.WorkDir + lcSelFabric + '.DBF') IN 0 ALIAS (lcSelFabric)
ENDIF
*B608487,1 WAM 03/19/2008 (End)

IF FILE(oAriaApplication.WorkDir + lcSelStyle + '.DBF') AND !USED(lcSelStyle)
  USE (oAriaApplication.WorkDir + lcSelStyle + '.DBF') IN 0 ALIAS (lcSelStyle)
ENDIF

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
*!*	IF USED(lcSelStyle) AND RECCOUNT(lcSelStyle) > 0
*!*	  llScanInFilt = .T.
*!*	ENDIF
IF lcInvType = '0002' AND USED(lcSelFabric) AND RECCOUNT(lcSelFabric) > 0
  llScanInFilt = .T.
ENDIF
IF lcInvType = '0001' AND USED(lcSelStyle) AND RECCOUNT(lcSelStyle) > 0
  llScanInFilt = .T.
ENDIF
*B608487,1 WAM 03/19/2008 (End)

  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("IF llScanInFilt")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
IF llScanInFilt
  *B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
  *SELECT (lcSelStyle)
  SELECT (IIF(lcInvType='0001',lcSelStyle,lcSelFabric))
  *B608487,1 WAM 03/19/2008 (End)

  lnUpThermo = RECCOUNT()
  LOCATE

  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  *llExitLoop = EOF()
  lcInExpression = ''
  SCAN
    lcInExpression=lcInExpression+IIF(EMPTY(lcInExpression),'',',')+"'"+cStyMajor+"'"
  ENDSCAN
  GO TOP
  lcInExpression = '('+lcInExpression+')'
  =lfSEEK(lcStyAls, "cStyMajor IN "+lcInExpression , '', .T.)
  *B608487,1 WAM 03/19/2008 (End)
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *= loStyle.GOBOTTOM()
  *B128464,2 WSH 08/21/2005 [End]
  
ELSE
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
*!*	  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	  *lnUpThermo = lfRecCount(loStyle)
*!*	  *llExitLoop = !loStyle.GOTOP()
*!*	  lnUpThermo = lfRecCount(lcStyAls)
*!*	  llExitLoop = !lfGOTOP(lcStyAls)
*!*	  *B128464,2 WSH 08/21/2005 [End]
  
  =lfSEEK(lcStyAls, "CINVTYPE = '"+lcInvType+"'", '', .T.)
  SELECT (lcStyAls)
  lnUpThermo = RECCOUNT()
  *B608487,1 WAM 03/19/2008 (End)
ENDIF
  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("lcTermoMsg = LANG_SMREBAL_REBCOMP")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
  *-SAB 09-26-2013 [Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]
  lcTermoMsg = LANG_SMREBAL_REBCOMP + lcCurrComp_ID + " " + ALLTRIM(gfItemMask('HM', lcFilePath, lcInvType)) + ":  "
  =lfInitThermo(lnUpThermo, lcTermoMsg)
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
ENDIF 
*! E303349,2 SAB 10/10/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]

*B608487,1 WAM 03/19/2008 Enhance performance of rebalance
*!*	*-- Scan Style file.
*!*	DO WHILE !llExitLoop
*!*	  
*!*	  IF llScanInFilt
*!*	    *-- I use these conditions as the Filter Cursor have only Style Majors but
*!*	    *-- we need to scan all Style Colors in Style file...!
*!*	    SELECT (lcSelStyle)
*!*	    IF llFirstRec
*!*	      lnThermNo = lnThermNo + 1
*!*	      oProgress.lblSecondLabel.Caption = cStyMajor
*!*	      oProgress.CurrentProgress(lnThermNo)
*!*	      
*!*	      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *IF !loStyle.SEEK(IIF(lcInvType = '0001', '', lcInvType) + PADR(cStyMajor, lnMajLen), .F., .T.)
*!*	      m.cInvType  = lcInvType
*!*	      m.Style     = PADR(cStyMajor, lnMajLen) + '%'
*!*	      lcWhereCond = IIF(lcInvType = '0002', "cInvType = ?m.cInvType AND ", "") + "Style Like ?m.Style"
*!*	      IF !lfSEEK(lcStyAls, lcWhereCond, IIF(lcInvType = '0001', '', m.cInvType) + PADR(cStyMajor, lnMajLen))
*!*	      *B128464,2 WSH 08/21/2005 [End]
*!*	      
*!*	        SKIP
*!*	        llExitLoop = EOF()
*!*	        LOOP
*!*	      ELSE
*!*	        llFirstRec = .F.
*!*	      ENDIF
*!*	    ELSE
*!*	      
*!*	      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	      *IF !(loStyle.GONEXT() AND EVALUATE(lcStyAls + '.Style') = PADR(cStyMajor, lnMajLen))
*!*	      IF !(lfGONEXT(lcStyAls) AND EVALUATE(lcStyAls + '.Style') = PADR(cStyMajor, lnMajLen))
*!*	      *B128464,2 WSH 08/21/2005 [End]
*!*	      
*!*	        llFirstRec = .T.
*!*	        SKIP
*!*	        llExitLoop = EOF()
*!*	        LOOP
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*	  SELECT (lcStyAls)
  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("GO TOP IN (lcStyAls)")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
GO TOP IN (lcStyAls)
lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcStyAls)), 1)
  *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
  =lfSTRTOFILE("llNative  = laFileStr[lnFilePos,6]")
  *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
llNative  = laFileStr[lnFilePos,6]
DO WHILE .T.
  lcWhereCond = '.T.'
  IF llNative AND llScanInFilt
    SELECT (IIF(lcInvType='0001',lcSelStyle,lcSelFabric))
    lcWhereCond = "Style='"+ALLTRIM(cStyMajor)+"'"
    =SEEK(ALLTRIM(cStyMajor),lcStyAls)
  ENDIF
SELECT (lcStyAls)
SCAN REST WHILE &lcWhereCond
*B608487,1 WAM 03/19/2008 (End)
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  m.cInvType = lcInvType
  m.Style    = Style
  =lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
  *B128464,2 WSH 08/21/2005 [End]
  
  IF llConvert
*-------------------- Not in this Release ------------------*
*!*      IF llPurge
*!*        lnThermNo = lnThermNo + 1
*!*        oProgress.lblFirstLabel.Caption = Style
*!*        oProgress.CurrentProgress(lnThermNo)
*!*      ELSE
*!*        lcMFleDesc = 'Rebalance Styles...'
*!*        SHOW GET lcMFleDesc
*!*        =lfThrmo(lnCurNm1,lnUpThermo,'123')
*!*      ENDIF  
  ELSE
    IF !llScanInFilt
      lnThermNo = lnThermNo + 1
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
      IF TYPE('lcXMLFileName') <>'C'
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
         *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
         IF EMPTY(lcStyMaj)
           *E303410,1 TMI 09/01/2013 [End  ] 
        oProgress.lblSecondLabel.Caption = Style
        oProgress.CurrentProgress(lnThermNo)
            
            *E303410,1 TMI 09/01/2013 [Start] 
          ENDIF 
          *E303410,1 TMI 09/01/2013 [End  ] 
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
      ELSE
        IF MOD(lnThermNo ,CEILING(lnUpThermo / 10)) = 0
          loProgress.Percent     = lnThermNo / lnUpThermo
          loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +IIF(lcInvType = '0001',' Style:',' Fabric:')+Style
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
	    ENDIF
      ENDIF
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    ENDIF
  ENDIF
  
  
  IF lnStock = 1
    *-- We can make it easy like all transaction (Ord,Alo), by calling lfUpdtLoop function
    *-- after copy of its array, but I found that Style inventory journal file is one of the 
    *-- huge files in the system and its index start with Style + cWareCode 
    *-- and this case help us in special optimizing technique for this file.
    = lfStyStock()  && Calculate Stocks
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpStk = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpStk = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  
  lcWorkFile = ''  && used in update Wip/nWo/InTrans
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loWorkFile = ''
  *loWorkHdr  = ''
  *
  **B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
  *IF llWip_nWo OR llInTrans
  *  *=loPOSLN.SEEK(lcInvType + &lcStyAls..Style)
  *ENDIF
  **B128464,1 WSH 06/12/2005 [End]
  
  lcWorkHdr  = ''
  *B128464,2 WSH 08/21/2005 [End]
  
      *B610750,1  test code TMI 06/18/2014 14:33 [Start] 
      =lfSTRTOFILE("IF llWip_nWo")
      *B610750,1  test code TMI 06/18/2014 14:33 [End  ] 
  *-- Update Wip(s) and nWo(s)
  IF llWip_nWo
    lcWorkFile = lcPoLAls
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loWorkFile = loPOSLN
    *loWorkHdr  = loPOSHDR
    lcWorkHdr   = lcPoHAls
    *B128464,2 WSH 08/21/2005 [End]
    
    *-- Set scan and while expressions for all transactions.
    lcWhilExpr = 'cInvType+Style+cBusDocu+cStyType+PO+STR(lineno,6)+Trancd = lcInvType + &lcStyAls..Style'
    lcForExpr  = IIF(lnWo = 1, [TOTQTY > 0 AND CSTYTYPE <> 'C' AND !(&lcPoHAls..STATUS $ "XB")],;
                               [TOTQTY > 0 AND CSTYTYPE <> 'C' AND !(&lcPoHAls..STATUS $ "SXB")])
    lcBatChng  = 'cInvType+Style+cBusDocu+cStyType+PO+STR(lineno,6)+PADR(cWareCode,6)'
  ENDIF
  SELECT (lcStyAls)
  
  *-- Update Work Order.
  IF lnWo = 1
    =ACOPY(laWoArr, laUpdatArr)  && copy order array to laUpdatArr to call generic code.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    **B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
    *SELECT (lcWorkFile)
    *LOCATE
    **B128464,1 WSH 06/12/2005 [End]
    *B128464,2 WSH 08/21/2005 [End]
    
    *-- Update StyDye Records with Zeros
    = lfZeroDye()
    = lfRelatFls('WO')  && Set Relation Strings
    = lfUpdtOper('WO')
    
    IF llViewLog
      llMisMatch = .F.
      IF lcInvType = '0001'
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
        *B128464,2 WSH 08/21/2005 [End]
        
        lnAlias = SELECT(0)
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *SELECT (loStyle.lcCursorUpdate)
        SELECT (lcStyAls)
        *B128464,2 WSH 08/21/2005 [End]
        
        llMisMatch = OLDVAL('nTotWo') <> laWo[9] OR;
                     OLDVAL('nWo1') <> laWo[1] OR;
                     OLDVAL('nWo2') <> laWo[2] OR;
                     OLDVAL('nWo3') <> laWo[3] OR;
                     OLDVAL('nWo4') <> laWo[4] OR;
                     OLDVAL('nWo5') <> laWo[5] OR;
                     OLDVAL('nWo6') <> laWo[6] OR;
                     OLDVAL('nWo7') <> laWo[7] OR;
                     OLDVAL('nWo8') <> laWo[8]
        IF llMisMatch
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGWO
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
        SELECT (lnAlias)
      
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      ELSE
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
        *B128464,2 WSH 08/21/2005 [End]
        
        lnAlias = SELECT(0)
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *SELECT (loStyle.lcCursorUpdate)
        SELECT (lcStyAls)
        *B128464,2 WSH 08/21/2005 [End]
        
        llMisMatch = OLDVAL('nTotWo') <> laWo[9]
        IF llMisMatch
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGWO
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
        SELECT (lnAlias)
      *E039550,1 WSH 08/07/2005 [End]
      
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType) + &lcStyAls..Style)
      m.cInvType = lcInvType
      m.Style    = EVALUATE(lcStyAls + '.Style')
      IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('nTotWo') <> nTotWo OR;
                       OLDVAL('nWo1') <> nWo1 OR;
                       OLDVAL('nWo2') <> nWo2 OR;
                       OLDVAL('nWo3') <> nWo3 OR;
                       OLDVAL('nWo4') <> nWo4 OR;
                       OLDVAL('nWo5') <> nWo5 OR;
                       OLDVAL('nWo6') <> nWo6 OR;
                       OLDVAL('nWo7') <> nWo7 OR;
                       OLDVAL('nWo8') <> nWo8
          IF llMisMatch
            IF llMisMatch
              DECLARE laRebMsg[3]
              laRebMsg[1] = " "
              laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGWO
              laRebMsg[3] = " "
              =lfVryRport()
              EXIT
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpWo = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpWo = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  SELECT (lcStyAls)
  
  *-- Update Work process.  
  IF lnWip = 1
    =ACOPY(laWipArr, laUpdatArr)  && copy order array to laUpdatArr to call generic code.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    **B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
    *SELECT (lcWorkFile)
    *LOCATE
    **B128464,1 WSH 06/12/2005 [End]
    *B128464,2 WSH 08/21/2005 [End]
    
    *-- Update StyDye Records with Zeros
    = lfZeroDye()
    = lfRelatFls('WIP')  && Set Relation Strings
    = lfUpdtOper('WIP')
    
    IF llViewLog
      llMisMatch = .F.
      IF lcInvType = '0001'
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
        *SELECT (loStyle.lcCursorUpdate)
        SELECT (lcStyAls)
        *B128464,2 WSH 08/21/2005 [End]
        
        llMisMatch = OLDVAL('TotWip') <> laWip[9] OR;
                     OLDVAL('Wip1') <> laWip[1] OR;
                     OLDVAL('Wip2') <> laWip[2] OR;
                     OLDVAL('Wip3') <> laWip[3] OR;
                     OLDVAL('Wip4') <> laWip[4] OR;
                     OLDVAL('Wip5') <> laWip[5] OR;
                     OLDVAL('Wip6') <> laWip[6] OR;
                     OLDVAL('Wip7') <> laWip[7] OR;
                     OLDVAL('Wip8') <> laWip[8]
        IF llMisMatch
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGWIP
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      ELSE
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
        *SELECT (loStyle.lcCursorUpdate)
        SELECT (lcStyAls)
        *B128464,2 WSH 08/21/2005 [End]
        
        llMisMatch = OLDVAL('TotWip') <> laWip[9]
        IF llMisMatch
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGWIP
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      *E039550,1 WSH 08/07/2005 [End]
      
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType) + &lcStyAls..Style)
      m.cInvType = lcInvType
      m.Style    = EVALUATE(lcStyAls + '.Style')
      IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotWip') <> TotWip OR;
                       OLDVAL('Wip1') <> Wip1 OR;
                       OLDVAL('Wip2') <> Wip2 OR;
                       OLDVAL('Wip3') <> Wip3 OR;
                       OLDVAL('Wip4') <> Wip4 OR;
                       OLDVAL('Wip5') <> Wip5 OR;
                       OLDVAL('Wip6') <> Wip6 OR;
                       OLDVAL('Wip7') <> Wip7 OR;
                       OLDVAL('Wip8') <> Wip8
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGWIP
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpWip = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpWip = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  SELECT (lcStyAls)

  laWo  = 0  
  laWip = 0
  
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  *Material Usage
  IF lnMatUsg = 1
    lcWorkFile = lcBOMCost
    lcWhilExpr = 'cinvtype+item+cwarecode+cdyelot+crsession+cisession = lcInvType + &lcStyAls..Style'

    LOCAL lcWhereCondition
    m.cInvTypeFab  = lcInvType
    m.StyleSelect     = EVALUATE(lcStyAls + '.STYLE')
    lcWhereCondition = "cInvType = ?m.cInvTypeFab  AND " + "Item = ?m.StyleSelect"
    STORE 0 TO lnUSGHist,lnUSGCur
    *Test
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*!*      IF lfSEEK(lcWorkFile , lcWhereCondition , m.cInvTypeFab  + m.StyleSelect)
*!*        *update stydye file with Zero values 
*!*        IF llStyDyeFl
*!*          IF lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab  AND " + "Style = ?m.StyleSelect", m.cInvTypeFab  + m.StyleSelect)
*!*            SELECT (lcSDyAls)
*!*            SCAN REST WHILE &lcDyeWhile
*!*              REPLACE NCUSAGE1   WITH 0 ,;
*!*                      NCUSAGE2   WITH 0,;
*!*                      NCUSAGE3   WITH 0,;
*!*                      NCUSAGE4   WITH 0,;
*!*                      NCUSAGE5   WITH 0,;
*!*                      NCUSAGE6   WITH 0,;
*!*                      NCUSAGE7   WITH 0,;
*!*                      NCUSAGE8   WITH 0,;
*!*                      NTOTCUSA   WITH 0 ,;
*!*                      NHUSAGE1   WITH 0 ,;
*!*                      NHUSAGE2   WITH 0;
*!*                      NHUSAGE3   WITH 0;
*!*                      NHUSAGE4   WITH 0;
*!*                      NHUSAGE5   WITH 0;
*!*                      NHUSAGE6   WITH 0;
*!*                      NHUSAGE7   WITH 0;
*!*                      NHUSAGE8   WITH 0;
*!*                      NTOTHUSAGE WITH 0 
*!*            ENDSCAN
*!*          ENDIF
*!*        ENDIF
*!*        *update stydye file with Zero values 
*!*        SELECT(lcWorkFile)
*!*        SCAN REST WHILE cinvtype+item+cwarecode+cdyelot+crsession+cisession = m.cInvTypeFab  + m.StyleSelect
*!*          *Poshdr effect
*!*          DO CASE
*!*            CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'M'
*!*              m.cBusDocu = 'P' 
*!*              m.cStyType = 'U'
*!*            CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'I'
*!*              m.cBusDocu = 'P' 
*!*              m.cStyType = 'P'
*!*            CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'T'
*!*              m.cBusDocu = 'P' 
*!*              m.cStyType = 'F'
*!*            CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'F'
*!*              m.cBusDocu = 'P' 
*!*              m.cStyType = 'M'
*!*          ENDCASE
*!*          m.PO = EVALUATE(lcWorkFile+'.ctktno')
*!*          IF lfSEEK(lcPoHAls, "cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO", m.cBusDocu  + m.cStyType +m.PO) 
*!*            IF EVALUATE(lcPoHAls+'.Status') $ 'OAC'
*!*              lnUSGCur  = lnUSGCur + EVALUATE(lcWorkFile+'.ntotqty')
*!*            ELSE
*!*              IF EVALUATE(lcPoHAls+'.Status') $ 'S'
*!*                lnUSGHist = lnUSGHist +  EVALUATE(lcWorkFile+'.ntotqty')
*!*              ENDIF   
*!*            ENDIF 
*!*          ENDIF 
*!*          m.cInvTypeFab   = lcInvType
*!*          m.StyleSelect     = EVALUATE(lcStyAls + '.STYLE')
*!*          m.Cwarecode = EVALUATE(lcWorkFile+'.cwarecode')
*!*          
*!*          llExprStyDye = lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab  AND " + "Style = ?m.StyleSelect And Cwarecode = ?m.cwarecode", m.cInvTypeFab  + m.StyleSelect+m.Cwarecode )
*!*          
*!*          IF llExprStyDye
*!*            SELECT (lcSDyAls)
*!*    	      REPLACE NCUSAGE1   WITH NCUSAGE1   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'COA',EVALUATE(lcWorkFile+'.ntotqty'),0) ,;
*!*                    NCUSAGE2   WITH 0,;
*!*                    NCUSAGE3   WITH 0,;
*!*                    NCUSAGE4   WITH 0,;
*!*                    NCUSAGE5   WITH 0,;
*!*                    NCUSAGE6   WITH 0,;
*!*                    NCUSAGE7   WITH 0,;
*!*                    NCUSAGE8   WITH 0,;
*!*  	                NTOTCUSA   WITH NTOTCUSA   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'COA',EVALUATE(lcWorkFile+'.ntotqty'),0)  ,;
*!*  	                NHUSAGE1   WITH NHUSAGE1   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'S',EVALUATE(lcWorkFile+'.ntotqty'),0) ,;
*!*                    NHUSAGE2   WITH 0;
*!*                    NHUSAGE3   WITH 0;
*!*                    NHUSAGE4   WITH 0;
*!*                    NHUSAGE5   WITH 0;
*!*                    NHUSAGE6   WITH 0;
*!*                    NHUSAGE7   WITH 0;
*!*                    NHUSAGE8   WITH 0;
*!*  	                NTOTHUSAGE WITH NTOTHUSAGE + IIF(EVALUATE(lcPoHAls+'.Status') $ 'S',EVALUATE(lcWorkFile+'.ntotqty'),0)  

*!*          ENDIF 
*!*          SELECT(lcWorkFile)
*!*        ENDSCAN 
*!*        SELECT (lcStyAls)
*!*        REPLACE NTOTCUSA WITH lnUSGCur ,;
*!*                NTOTHUSAGE WITH lnUSGHist  
*!*        
*!*                
*!*      ENDIF 
    IF lfSEEK(lcWorkFile , lcWhereCondition , m.cInvTypeFab + m.StyleSelect)
      *update stydye file with Zero values 
      IF llStyDyeFl
        IF lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab AND " + "Style = ?m.StyleSelect", m.cInvTypeFab + m.StyleSelect)
          SELECT (lcSDyAls)
          SCAN REST WHILE &lcDyeWhile
            REPLACE NCUSAGE1   WITH 0 ,;
                    NCUSAGE2   WITH 0,;
                    NCUSAGE3   WITH 0,;
                    NCUSAGE4   WITH 0,;
                    NCUSAGE5   WITH 0,;
                    NCUSAGE6   WITH 0,;
                    NCUSAGE7   WITH 0,;
                    NCUSAGE8   WITH 0,;
                    NTOTCUSA   WITH 0 ,;
                    NHUSAGE1   WITH 0 ,;
                    NHUSAGE2   WITH 0;
                    NHUSAGE3   WITH 0;
                    NHUSAGE4   WITH 0;
                    NHUSAGE5   WITH 0;
                    NHUSAGE6   WITH 0;
                    NHUSAGE7   WITH 0;
                    NHUSAGE8   WITH 0;
                    NTOTHUSAGE WITH 0 
          ENDSCAN
        ENDIF
      ENDIF
      *update stydye file with Zero values 
      SELECT(lcWorkFile)
      
      LOCATE FOR cInvType+Item = m.cInvTypeFab+m.StyleSelect
      IF FOUND()   
        DO WHILE !EOF()
          m.cbomtype = cbomtype
          m.cimtyp   = cimtyp
          m.ctktno   = ctktno
          m.cInvTypeFab = cinvtype
          m.item = item
         
          SCAN REST WHILE  cbomtype+ cimtyp+ ctktno+ cinvtype+ item+ mfgcode+ cwarecode+ cdyelot+ crsession+ cisession=m.cbomtype +;
             m.cimtyp+m.ctktno  +m.cInvTypeFab +m.item
            *Poshdr effect
             DO CASE
               CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'M'
                  m.cBusDocu = 'P' 
                  m.cStyType = 'U'
               CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'I'
                 m.cBusDocu = 'P' 
                 m.cStyType = 'P'
               CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'T'
                 m.cBusDocu = 'P' 
                 m.cStyType = 'F'
               CASE EVALUATE(lcWorkFile + '.cIMTyp') = 'F'
                 m.cBusDocu = 'P' 
                 m.cStyType = 'M'
             ENDCASE
             *lcBOMLine
             m.PO = EVALUATE(lcWorkFile+'.ctktno')
        
             IF lfSEEK(lcPoHAls, "cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO", m.cBusDocu  + m.cStyType +m.PO) 
               IF EVALUATE(lcPoHAls+'.Status') $ 'OACS'
                lnUSGHist = lnUSGHist +  EVALUATE(lcWorkFile+'.ntotqty')
              ENDIF   
               IF EVALUATE(lcPoHAls+'.Status') $ 'OA'
                lnUSGCur  = lnUSGCur + EVALUATE(lcWorkFile+'.ntotqty')
              ENDIF 
            ENDIF 
            m.cInvTypeFab  = lcInvType
            m.StyleSelect     = EVALUATE(lcStyAls + '.STYLE')
            m.CwarecodeSelect = EVALUATE(lcWorkFile+'.cwarecode')
        
            llExprStyDye = lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab AND " + "Style = ?m.StyleSelect And Cwarecode = ?m.CwarecodeSelect", m.cInvTypeFab + m.StyleSelect+ m.CwarecodeSelect)
        
            IF llExprStyDye
              SELECT (lcSDyAls)
              REPLACE NCUSAGE1   WITH NCUSAGE1   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'OA',EVALUATE(lcWorkFile+'.ntotqty'),0) ,;
                      NCUSAGE2   WITH 0,;
                      NCUSAGE3   WITH 0,;
                      NCUSAGE4   WITH 0,;
                      NCUSAGE5   WITH 0,;
                      NCUSAGE6   WITH 0,;
                      NCUSAGE7   WITH 0,;
                      NCUSAGE8   WITH 0,;
                      NTOTCUSA   WITH NTOTCUSA   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'OA',EVALUATE(lcWorkFile+'.ntotqty'),0)  ,;
                      NHUSAGE1   WITH NHUSAGE1   + IIF(EVALUATE(lcPoHAls+'.Status') $ 'OACS',EVALUATE(lcWorkFile+'.ntotqty'),0) ,;
                      NHUSAGE2   WITH 0;
                      NHUSAGE3   WITH 0;
                      NHUSAGE4   WITH 0;
                      NHUSAGE5   WITH 0;
                      NHUSAGE6   WITH 0;
                      NHUSAGE7   WITH 0;
                      NHUSAGE8   WITH 0;
                      NTOTHUSAGE WITH NTOTHUSAGE + IIF(EVALUATE(lcPoHAls+'.Status') $ 'OACS',EVALUATE(lcWorkFile+'.ntotqty'),0)  

            ENDIF 
            SELECT(lcWorkFile)
          ENDSCAN 
          SELECT(lcWorkFile)
          DO CASE
            CASE m.cimtyp = 'M'
              m.cBusDocu = 'P' 
              m.cStyType = 'U'
            CASE m.cimtyp = 'I'
              m.cBusDocu = 'P' 
              m.cStyType = 'P'
            CASE m.cimtyp= 'T'
              m.cBusDocu = 'P' 
              m.cStyType = 'F'
            CASE m.cimtyp= 'F'
              m.cBusDocu = 'P' 
              m.cStyType = 'M'
          ENDCASE
           m.PO = m.Ctktno
           
         IF lfSEEK(lcPoHAls, "cBusDocu = ?m.cBusDocu AND cStyType = ?m.cStyType AND PO = ?m.PO", m.cBusDocu  + m.cStyType +m.PO) 
          IF EVALUATE(lcPoHAls+'.Status') $ 'OA'
            m.cIMTyp = m.cimtyp
            m.CtktNo = m.PO
            IF lfSeek(lcBOMLine,"cimtyp = ?m.cIMTyp AND ctype = '2' AND ctktno = ?m.CtktNo",m.cIMTyp+'2'+m.CtktNo)
              SELECT (lcBOMLine)
              SCAN REST WHILE cimtyp+ ctype+ ctktno+ shipno+ STR(lineno,6)+ cbomtyp+ cinvtype+;
                 style+cinvtypc+ item+ mfgcode+ crsession+cstygrade+ STR(nlineno,6) =m.cIMTyp+'2'+m.CtktNo;
                 FOR !EMPTY(crsession) AND Item = EVALUATE(lcStyAls + '.STYLE') 
                 lnUSGCur  = lnUSGCur - itemQty
                 llExStyDye = lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab AND " + "Style = ?m.StyleSelect And Cwarecode = ?m.CwarecodeSelect", m.cInvTypeFab + m.StyleSelect+m.CwarecodeSelect )  
                 IF llExStyDye
                  SELECT (lcSDyAls)
                  REPLACE NCUSAGE1   WITH NCUSAGE1   - EVALUATE(lcBOMLine+'.itemQty')  ,;
                          NCUSAGE2   WITH 0,;
                          NCUSAGE3   WITH 0,;
                          NCUSAGE4   WITH 0,;
                          NCUSAGE5   WITH 0,;
                          NCUSAGE6   WITH 0,;
                          NCUSAGE7   WITH 0,;
                          NCUSAGE8   WITH 0,;
                          NTOTCUSA   WITH NTOTCUSA  - EVALUATE(lcBOMLine+'.itemQty')
                 ENDIF          
              ENDSCAN 
            ENDIF
            ENDIF 
        ENDIF   
        SELECT (lcStyAls)
        REPLACE NTOTCUSA WITH lnUSGCur ,;
                NTOTHUSAGE WITH lnUSGHist  

        SELECT(lcWorkFile)
      ENDDO 
    ELSE
      IF llStyDyeFl
        IF lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab AND " + "Style = ?m.StyleSelect", m.cInvTypeFab + m.StyleSelect)
          SELECT (lcSDyAls)
          SCAN REST WHILE &lcDyeWhile
            REPLACE NCUSAGE1   WITH 0 ,;
                    NCUSAGE2   WITH 0,;
                    NCUSAGE3   WITH 0,;
                    NCUSAGE4   WITH 0,;
                    NCUSAGE5   WITH 0,;
                    NCUSAGE6   WITH 0,;
                    NCUSAGE7   WITH 0,;
                    NCUSAGE8   WITH 0,;
                    NTOTCUSA   WITH 0 ,;
                    NHUSAGE1   WITH 0 ,;
                    NHUSAGE2   WITH 0;
                    NHUSAGE3   WITH 0;
                    NHUSAGE4   WITH 0;
                    NHUSAGE5   WITH 0;
                    NHUSAGE6   WITH 0;
                    NHUSAGE7   WITH 0;
                    NHUSAGE8   WITH 0;
                    NTOTHUSAGE WITH 0 
          ENDSCAN
        ENDIF
      ENDIF
      
      SELECT (lcStyAls)
      REPLACE NTOTCUSA WITH 0 ,;
              NTOTHUSAGE WITH 0 
    
   ENDIF       
    ELSE
      IF llStyDyeFl
        IF lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab AND " + "Style = ?m.StyleSelect", m.cInvTypeFab + m.StyleSelect)
          SELECT (lcSDyAls)
          SCAN REST WHILE &lcDyeWhile
            REPLACE NCUSAGE1   WITH 0 ,;
                    NCUSAGE2   WITH 0,;
                    NCUSAGE3   WITH 0,;
                    NCUSAGE4   WITH 0,;
                    NCUSAGE5   WITH 0,;
                    NCUSAGE6   WITH 0,;
                    NCUSAGE7   WITH 0,;
                    NCUSAGE8   WITH 0,;
                    NTOTCUSA   WITH 0 ,;
                    NHUSAGE1   WITH 0 ,;
                    NHUSAGE2   WITH 0;
                    NHUSAGE3   WITH 0;
                    NHUSAGE4   WITH 0;
                    NHUSAGE5   WITH 0;
                    NHUSAGE6   WITH 0;
                    NHUSAGE7   WITH 0;
                    NHUSAGE8   WITH 0;
                    NTOTHUSAGE WITH 0 
          ENDSCAN
        ENDIF
      ENDIF
      
      SELECT (lcStyAls)
      REPLACE NTOTCUSA WITH 0 ,;
              NTOTHUSAGE WITH 0 
    
    ENDIF 
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]

    *Log file 
    llMisMatch = .F.
    IF llViewLog
      SELECT (lcStyAls)
      llMisMatch = (OLDVAL('NTOTHUSAGE') <> NTOTHUSAGE) or ((OLDVAL('NTOTCUSA') <> NTOTCUSA))
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGUSE
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      
       IF !llMisMatch AND lfSEEK(lcSDyAls, "cInvType = ?m.cInvTypeFab  AND " + "Style = ?m.StyleSelect", m.cInvTypeFab  + m.StyleSelect)
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
    		  llMisMatch = OLDVAL('NTOTCUSA') <> NTOTCUSA OR;
                       OLDVAL('NCUSAGE1') <> NCUSAGE1 OR;
                       OLDVAL('NHUSAGE1') <> NHUSAGE1 OR;
                       OLDVAL('NTOTHUSAGE') <> NTOTHUSAGE 
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGUSE
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF

       
    ENDIF 
    *Log file 
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpUsage = 'U')
  ENDIF 
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]


  *-- if in transit case only.
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
  *  IF llInTrans AND EMPTY(lcWorkFile)
  IF llInTrans &&AND EMPTY(lcWorkFile)
  *B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]
  
    lcWorkFile = lcPoLAls
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loWorkFile = loPOSLN
    *loWorkHdr  = loPOSHDR
    lcWorkHdr = lcPoHAls
    *B128464,2 WSH 08/21/2005 [End]
    
    *-- Set scan and while expressions for all transactions.
    lcWhilExpr = 'cInvType+Style+cBusDocu+cStyType+PO+STR(lineno,6)+Trancd = lcInvType + &lcStyAls..Style'
    lcForExpr  = [TOTQTY > 0 AND CSTYTYPE <> 'C' AND !(&lcPoHAls..STATUS $ "CSX")]
    lcBatChng  = 'cInvType+Style+cBusDocu+cStyType+PO+STR(lineno,6)+PADR(cWareCode,6)'
    
    =ACOPY(laTrnArr, laUpdatArr)  && copy order array to laUpdatArr to call generic code.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    **B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
    *SELECT (lcWorkFile)
    *LOCATE
    **B128464,1 WSH 06/12/2005 [End]
    *B128464,2 WSH 08/21/2005 [End]
    
    *-- Update StyDye Records with Zeros
    = lfZeroDye()
    = lfRelatFls('TRN')  && Set Relation Strings
    = lfUpdtOper('TRN')
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpTrn = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpTrn = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  SELECT (lcStyAls)
  
  *-- Update Wip from WIPAADJ
  IF llWipAdj AND !MAKE
    lcWorkFile = lcWipJAls
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loWorkFile = loWipAdj
    *
    **B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
    *=loWorkFile.SEEK(&lcStyAls..Style)
    **B128464,1 WSH 06/12/2005 [End]
    *B128464,2 WSH 08/21/2005 [End]
    
    = lfRelatFls('ADJ')  && Set Relation Strings
    = lfUpdtOper('ADJ')
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpWip = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpWip = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
  SELECT (lcStyAls)
  
  *-- Update Ord(s) and Alo(s)
  IF lnOrd = 1
    =ACOPY(laOrdArr, laUpdatArr)  && copy order array to laUpdatArr to call generic code.
    
    *-- Update StyDye Records with Zeros
    = lfZeroDye()

    *-- By pass parameters used in rebalance.
    lcExpr = "Style+DTOS(complete)+cordtype+order+Store+STR(lineno,6)=&lcStyAls..Style"

    = lfRelatFls('ORD')  && Set Relation Strings
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfUpdtLoop(loOrdLine,loOrdHdr,lcExpr,;
                "(&lcOrdHAls..STATUS $ 'OH') AND (cordtype = 'O')",;
                "cWareCode", 'ORD')
    = lfUpdtLoop(lcOrdLAls,lcOrdHAls,lcExpr,;
                "(&lcOrdHAls..STATUS $ 'OH') AND (cordtype = 'O')",;
                "cWareCode", 'ORD')
    *B128464,2 WSH 08/21/2005 [End]
    
    laUpdatArr = .F.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpOrd = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpOrd = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF  && end Update Ord(s) and Alo(s).
  SELECT (lcStyAls)

  *-- Update Ord(s) and Alo(s)
  IF lnAlo = 1
    =ACOPY(laAloArr, laUpdatArr)  && copy order array to laUpdatArr to call generic code.

    *-- By pass parameters used in rebalance.
    lcExpr = "Style+DTOS(complete)+cordtype+order+Store+STR(lineno,6)=&lcStyAls..Style"

    *-- Update StyDye Records with Zeros
    = lfZeroDye()

    = lfRelatFls('ALO')  && Set Relation Strings
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfUpdtLoop(loOrdLine,loOrdHdr,lcExpr,;
                "(&lcOrdHAls..STATUS $ 'OH') AND (cordtype = 'O')",;
                "cWareCode", 'ALO')
    = lfUpdtLoop(lcOrdLAls,lcOrdHAls,lcExpr,;
                "(&lcOrdHAls..STATUS $ 'OH') AND (cordtype = 'O')",;
                "cWareCode", 'ALO')
    *B128464,2 WSH 08/21/2005 [End]
    
    laUpdatArr = .F.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpAlo = 'U')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpAlo = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF  && end Update Ord(s) and Alo(s).
  SELECT (lcStyAls)
  
  *-- Update Shp Quantity.
  IF llInvoice
    =ACOPY(laInvArr, laUpdatArr)  && copy invoice array to alUpdatArr to call generic code.

    *-- Update StyDye Records with Zeros
    =lfZeroDye()

    *-- By pass parameters used in rebalance.
    lcExpr = 'Style+invoice+STR(lineno,6) =&lcStyAls..Style'
    
    = lfRelatFls('SHP')  && Set Relation Strings
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfUpdtLoop(loInvLine,loInvHdr,lcExpr,;
                "(&lcInvHAls..STATUS = 'C')","&lcInvHAls..cWareCode",'SHP')
    *= lfMasterUpd('loStyle,loStyDye', lcVrUpShp = 'U')
    = lfUpdtLoop(lcInvLAls,lcInvHAls,lcExpr,;
                "(&lcInvHAls..STATUS = 'C')","&lcInvHAls..cWareCode",'SHP')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpShp = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF  && end of update invoice ship quantity.
  SELECT (lcStyAls)

*-------------------- Not in this Release ------------------*
*!*    IF llRauth
*!*      =ACOPY(laAuthArr,laUpdatArr)  && copy Ra array to alUpdatArr to call generic code.
*!*      IF lcVrUpd = 'V'
*!*        lcExpr = "Style+rano+cra_linno = lcStyAls.Style"
*!*      ELSE
*!*        lcExpr = "Style+rano+cra_linno = &lcStyAls..Style"
*!*      ENDIF
*!*      =lfUpdtLoop(lcRaLnAls,lcExpr,;
*!*                  "(&lcRaHdAls..STATUS = 'O')","&lcRaHdAls..cWareCode")  
*!*    ENDIF

*!*    *-- Ret(s) fields.
*!*    IF llRetMemo
*!*      =ACOPY(laMemoArr,laUpdatArr)  && copy credit memo array to alUpdatArr to call generic code.
*!*      IF lcVrUpd = 'V'
*!*        lcExpr = "Style+CrMemo = lcStyAls.Style"
*!*      ELSE
*!*        lcExpr = "Style+CrMemo = &lcStyAls..Style"
*!*      ENDIF
*!*      =lfUpdtLoop(lcRetLnAls,lcExpr,;
*!*                  "(&lcRetHdAls..STATUS <> 'V')","&lcRetHdAls..cWareCode")  
*!*    ENDIF
  
  *-- Another case if new feature added in the future.
  
  
  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
  IF llRauth
    =ACOPY(laAuthArr,laUpdatArr)  && copy Ra array to alUpdatArr to call generic code.
   
    =lfZeroDye()

    *-- By pass parameters used in rebalance.
     lcExpr = "Style+rano+cra_linno = &lcStyAls..Style"
    
    = lfRelatFls('RA')  && Set Relation Strings
    
    = lfUpdtLoop(lcRaLnAls,lcRaHdAls,lcExpr,;
                "(&lcRaHdAls..STATUS = 'O')","&lcRaHdAls..cWareCode",'RA')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpRa = 'U')
                
  ENDIF

  *-- Ret(s) fields.
  IF llRetMemo
    =ACOPY(laMemoArr,laUpdatArr)  && copy credit memo array to alUpdatArr to call generic code.
                
    =lfZeroDye()

    *-- By pass parameters used in rebalance.
     lcExpr = "Style+CrMemo = &lcStyAls..Style"
    
    = lfRelatFls('RET')  && Set Relation Strings
    
    = lfUpdtLoop(lcRetLnAls,lcRetHdAls,lcExpr,;
                "(&lcRetHdAls..STATUS <> 'V')","&lcRetHdAls..cWareCode",'RET')
    = lfMasterUpd('lcStyAls,lcSDyAls', lcVrUpRet = 'U')
            
                
  ENDIF
  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]
  
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
*!*	  *-- Step to the Next Style Record
*!*	  IF !llScanInFilt
*!*	    
*!*	    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*	    *llExitLoop = !loStyle.GONEXT()
*!*	    llExitLoop = !lfGONEXT(lcStyAls)
*!*	    *B128464,2 WSH 08/21/2005 [End]
*!*	    
*!*	  ENDIF
*!*	ENDDO  && end Scan Style file.
ENDSCAN
  IF llNative AND llScanInFilt
    SKIP IN (IIF(lcInvType='0001',lcSelStyle,lcSelFabric))
    IF EOF(IIF(lcInvType='0001',lcSelStyle,lcSelFabric))
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*B608487,1 WAM 03/19/2008 (End)

IF !llConvert
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <>'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 
  
    oProgress.FinishProgress()
      
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF 
    *E303410,1 TMI 09/01/2013 [End  ] 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
ENDIF  
*-- end of lfUpdSty.

*!*************************************************************
*! Name      : lfUpStyDye
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update stydye record for all rebalance cases except
*!           : work process cases.
*!*************************************************************
*! Example   : = lfUpStyDye()
*!*************************************************************
FUNCTION lfUpStyDye
LPARAMETERS lcUpSty, lcUpWare, lcUpDye, lcFieldNam, lcTotNam, lcRepFrNam

*-- if stydye file is found .
IF llStyDyeFl AND (TYPE('lcUpSty')  = 'C') AND !EMPTY(lcUpSty)  AND ;
                  (TYPE('lcUpWare') = 'C') AND !EMPTY(lcUpWare)
  lcUpSty  = PADR(lcUpSty,19)
  lcUpWare = PADR(lcUpWare,6)
  
  IF TYPE('lcUpDye') $ 'UL'
    lcUpDye = SPACE(10)
  ENDIF
  lcUpDye = PADR(lcUpDye,10)
  
  *-- Update Location Record if found.
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+lcUpSty+lcUpWare+SPACE(10))
  LOCAL lcWhereCond
  m.cInvType  = lcInvType
  m.Style     = lcUpSty
  m.cWareCode = lcUpWare
  lcWhereCond = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style AND cWareCode = ?m.cWareCode AND Dyelot = ''"
  
  llExpr = lfSEEK(lcSDyAls, lcWhereCond, IIF(lcInvType = '0001', '', m.cInvType)+m.Style+m.cWareCode+SPACE(10))
  *B128464,2 WSH 08/21/2005 [End]
  
  IF !llExpr AND !EMPTY(lcUpWare)
    =lfAddRec(lcUpSty,lcUpWare,0)
    llExpr = .T.
  ENDIF
  
  IF llExpr
    DO lpRepStyDy WITH lcFieldNam,lcTotNam,lcRepFrNam  && Replace current record with passed values.
    
    *-- Update dyelot record if found.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF llDyelot AND !EMPTY(lcUpDye) AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+lcUpSty+lcUpWare+lcUpDye)
    m.cInvType  = lcInvType
    m.Style     = lcUpSty
    m.cWareCode = lcUpWare
    m.Dyelot    = lcUpDye
    lcWhereCond = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style AND cWareCode = ?m.cWareCode AND Dyelot = ?m.Dyelot"
    IF llDyelot AND !EMPTY(lcUpDye) AND lfSEEK(lcSDyAls, lcWhereCond, IIF(lcInvType = '0001', '', lcInvType)+lcUpSty+lcUpWare+lcUpDye)
    *B128464,2 WSH 08/21/2005 [End]
    
      DO lpRepStyDy WITH lcFieldNam, lcTotNam, lcRepFrNam
    ENDIF
  ENDIF
ENDIF
*-- end of Update Stydye file.

*!*************************************************************
*! Name      : lpRepStyDy
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2004
*! Purpose   : Update stydye record with passed values
*!*************************************************************
*! Example   : = lpRepStyDy()
*!*************************************************************
PROCEDURE lpRepStyDy
LPARAMETERS lcFieldNam, lcTotNam, lcRepFrNam

LOCAL lcReplStat, lnTotQty
lnTotQty = EVALUATE(lcRepFrNam+'1') + EVALUATE(lcRepFrNam+'2')  +;
           EVALUATE(lcRepFrNam+'3') + EVALUATE(lcRepFrNam+'4')  +;
           EVALUATE(lcRepFrNam+'5') + EVALUATE(lcRepFrNam+'6')  +;
           EVALUATE(lcRepFrNam+'7') + EVALUATE(lcRepFrNam+'8')

lcReplStat = lcFieldNam + '1 WITH ' + lcFieldNam + '1 + ' + STR(EVALUATE(lcRepFrNam + '1')) + ',' +;
             lcFieldNam + '2 WITH ' + lcFieldNam + '2 + ' + STR(EVALUATE(lcRepFrNam + '2')) + ',' +;
             lcFieldNam + '3 WITH ' + lcFieldNam + '3 + ' + STR(EVALUATE(lcRepFrNam + '3')) + ',' +;
             lcFieldNam + '4 WITH ' + lcFieldNam + '4 + ' + STR(EVALUATE(lcRepFrNam + '4')) + ',' +;
             lcFieldNam + '5 WITH ' + lcFieldNam + '5 + ' + STR(EVALUATE(lcRepFrNam + '5')) + ',' +;
             lcFieldNam + '6 WITH ' + lcFieldNam + '6 + ' + STR(EVALUATE(lcRepFrNam + '6')) + ',' +;
             lcFieldNam + '7 WITH ' + lcFieldNam + '7 + ' + STR(EVALUATE(lcRepFrNam + '7')) + ',' +;
             lcFieldNam + '8 WITH ' + lcFieldNam + '8 + ' + STR(EVALUATE(lcRepFrNam + '8')) + ',' +;
             lcTotNam   + '  WITH ' + lcTotNam   + ' + ' + STR(lnTotQty)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*loStyDye.Replace(lcReplStat)
REPLACE &lcReplStat. IN (lcSDyAls)
*B128464,2 WSH 08/21/2005 [End]

*-- end of lpRepStyDy.        

*!*************************************************************
*! Name      : lfUpdtLoop
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update all cases except work process ones.
*!           : note that it can update multiple cases in the same file.
*!*************************************************************
*! Example   : = lfUpdtLoop()
*!*************************************************************
FUNCTION lfUpdtLoop

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS loScanFile, loScanHdr, lcWhilCond, lcForCond, lcUptWare, lcOption
LPARAMETERS lcScanFile, lcScanHdr, lcWhilCond, lcForCond, lcUptWare, lcOption
*B128464,2 WSH 08/21/2005 [End]

LOCAL lnAlias
lnAlias = SELECT(0)

*-- If no transactions found.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*IF !loScanFile.SEEK(&lcStyAls..Style)
m.cInvType = lcInvType
m.Style    = EVALUATE(lcStyAls + '.Style')

IF !lfSEEK(lcScanFile, "Style = ?m.Style", &lcStyAls..Style)
*B128464,2 WSH 08/21/2005 [End]

  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *IF lcInvType = '0001'
  *E039550,1 WSH 08/07/2005 [End]
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start] lcSDyAls
    *DO lpGathFlds WITH loStyle && Update style record with zeros.
    DO lpGathFlds WITH lcStyAls && Update style record with zeros.
    *B128464,2 WSH 08/21/2005 [End]
    
  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *ENDIF
  *E039550,1 WSH 08/07/2005 [End]
  
  *-- Update stydye record if system does not have multiware and dyelots.
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
  llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
  *B128464,2 WSH 08/21/2005 [End]
  
  IF !llStyDyeFl AND llExpr
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyDye && Update stydye record with zeros.
    DO lpGathFlds WITH lcSDyAls && Update stydye record with zeros.
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF

ELSE
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *SELECT (loScanFile.lcCursorView)
  SELECT (lcScanFile)
  *B128464,2 WSH 08/21/2005 [End]
  
  SCAN REST WHILE &lcWhilCond
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF !loScanHdr.SEEK(&lcHdrLnRel) OR !&lcForCond
    SCATTER MEMVAR
    
    *B131722,1 WSH 05/02/2006 Add brackets. [Start]
    *IF !lfSEEK(lcScanHdr, lcHdrLnCnd, &lcHdrLnRel) OR !&lcForCond
    IF !lfSEEK(lcScanHdr, lcHdrLnCnd, &lcHdrLnRel) OR !(&lcForCond.)
    *B131722,1 WSH 05/02/2006 [End]
    
    *B128464,2 WSH 08/21/2005 [End]
    
      LOOP
    ENDIF

    *-- loop all databases to rebalance it.
    FOR lnAllTrans = 1 TO ALEN(laUpdatArr,1)

      IF EMPTY(laUpdatArr[lnAllTrans,1])
        EXIT
      ENDIF

      *-- if current database condition is true.
      IF EVALUATE(laUpdatArr[lnAllTrans,1])
        *-- accomulate transactions in database array.
        FOR lnJ = 1 To 8
          lcJ = STR(lnJ,1)
          &laUpdatArr[lnAllTrans,2].[lnJ] = &laUpdatArr[lnAllTrans,2].[lnJ] + &laUpdatArr[lnAllTrans,4].&lcJ
        ENDFOR
        &laUpdatArr[lnAllTrans,2].[9] = &laUpdatArr[lnAllTrans,2].[1] + &laUpdatArr[lnAllTrans,2].[2] +;
                                        &laUpdatArr[lnAllTrans,2].[3] + &laUpdatArr[lnAllTrans,2].[4] +;
                                        &laUpdatArr[lnAllTrans,2].[5] + &laUpdatArr[lnAllTrans,2].[6] +;
                                        &laUpdatArr[lnAllTrans,2].[7] + &laUpdatArr[lnAllTrans,2].[8]
        
        *-- update stydye record.
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *=lfUpStyDye(Style,&lcUptWare,IIF(TYPE('Dyelot') $ "UL",SPACE(10),Dyelot),;
                   laUpdatArr[lnAllTrans,3],;
                   'TOT'+laUpdatArr[lnAllTrans,3],loScanFile.lcCursorView+'.'+ laUpdatArr[lnAllTrans,4])
        =lfUpStyDye(Style,&lcUptWare,IIF(TYPE('Dyelot') $ "UL",SPACE(10),Dyelot),;
                   laUpdatArr[lnAllTrans,3],;
                   'TOT'+laUpdatArr[lnAllTrans,3],lcScanFile+'.'+ laUpdatArr[lnAllTrans,4])
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
    ENDFOR
  ENDSCAN
  
  IF llViewLog
    IF lcOption = 'SHP'
      *-- Add the wrong style to the log file.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
      *SELECT (loStyle.lcCursorUpdate)
      SELECT (lcStyAls)
      *B128464,2 WSH 08/21/2005 [End]
      
      llMisMatch = OLDVAL('TotShp') <> laShp[9] OR;
                   OLDVAL('Shp1') <> laShp[1] OR;
                   OLDVAL('Shp2') <> laShp[2] OR;
                   OLDVAL('Shp3') <> laShp[3] OR;
                   OLDVAL('Shp4') <> laShp[4] OR;
                   OLDVAL('Shp5') <> laShp[5] OR;
                   OLDVAL('Shp6') <> laShp[6] OR;
                   OLDVAL('Shp7') <> laShp[7] OR;
                   OLDVAL('Shp8') <> laShp[8]
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGSHP
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..Style)
      IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotShp') <> TotShp OR;
                       OLDVAL('Shp1') <> Shp1 OR;
                       OLDVAL('Shp2') <> Shp2 OR;
                       OLDVAL('Shp3') <> Shp3 OR;
                       OLDVAL('Shp4') <> Shp4 OR;
                       OLDVAL('Shp5') <> Shp5 OR;
                       OLDVAL('Shp6') <> Shp6 OR;
                       OLDVAL('Shp7') <> Shp7 OR;
                       OLDVAL('Shp8') <> Shp8
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGSHP
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF

    IF lcOption = 'ALO'
      *-- Add the wrong style to the log file.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *SELECT (loStyle.lcCursorUpdate)
      SELECT (lcStyAls)
      *B128464,2 WSH 08/21/2005 [End]
      
      llMisMatch = OLDVAL('TotAlo') <> laAlo[9] OR;
                   OLDVAL('Alo1') <> laAlo[1] OR;
                   OLDVAL('Alo2') <> laAlo[2] OR;
                   OLDVAL('Alo3') <> laAlo[3] OR;
                   OLDVAL('Alo4') <> laAlo[4] OR;
                   OLDVAL('Alo5') <> laAlo[5] OR;
                   OLDVAL('Alo6') <> laAlo[6] OR;
                   OLDVAL('Alo7') <> laAlo[7] OR;
                   OLDVAL('Alo8') <> laAlo[8]
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGALO
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..Style)
      IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotAlo') <> TotAlo OR;
                       OLDVAL('Alo1') <> Alo1 OR;
                       OLDVAL('Alo2') <> Alo2 OR;
                       OLDVAL('Alo3') <> Alo3 OR;
                       OLDVAL('Alo4') <> Alo4 OR;
                       OLDVAL('Alo5') <> Alo5 OR;
                       OLDVAL('Alo6') <> Alo6 OR;
                       OLDVAL('Alo7') <> Alo7 OR;
                       OLDVAL('Alo8') <> Alo8
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGALO
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF  
    
    IF lcOption = 'ORD'
      *-- Add the wrong style to the log file.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *SELECT (loStyle.lcCursorUpdate)
      SELECT (lcStyAls)
      *B128464,2 WSH 08/21/2005 [End]
      
      llMisMatch = OLDVAL('TotOrd') <> laOrder[9] OR;
                   OLDVAL('Ord1') <> laOrder[1] OR;
                   OLDVAL('Ord2') <> laOrder[2] OR;
                   OLDVAL('Ord3') <> laOrder[3] OR;
                   OLDVAL('Ord4') <> laOrder[4] OR;
                   OLDVAL('Ord5') <> laOrder[5] OR;
                   OLDVAL('Ord6') <> laOrder[6] OR;
                   OLDVAL('Ord7') <> laOrder[7] OR;
                   OLDVAL('Ord8') <> laOrder[8]
      
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGORD
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..Style)
      IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotOrd') <> TotOrd OR;
                       OLDVAL('Ord1') <> Ord1 OR;
                       OLDVAL('Ord2') <> Ord2 OR;
                       OLDVAL('Ord3') <> Ord3 OR;
                       OLDVAL('Ord4') <> Ord4 OR;
                       OLDVAL('Ord5') <> Ord5 OR;
                       OLDVAL('Ord6') <> Ord6 OR;
                       OLDVAL('Ord7') <> Ord7 OR;
                       OLDVAL('Ord8') <> Ord8
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGORD
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
*----------------- Not in this Release -----------------------*
*!*    IF lcVrUpRa = 'V' AND lcOption = 'RA' AND (TotRa <> laRa[9] OR lcSDyAls.TotRa <> StyDye.TotRa )
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET
*!*      laRebMsg[3] = " "
*!*      =lfVryRport()
*!*    ENDIF  
*!*    IF lcVrUpRet = 'V' AND lcOption = 'RET' AND (TotRet <> laRet[9] OR lcSDyAls.TotRet <> StyDye.TotRet )
*!*      DECLARE laRebMsg[3]
*!*      laRebMsg[1] = " "
*!*      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET
*!*      laRebMsg[3] = " "
*!*      =lfVryRport()
*!*    ENDIF  
  
  
  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
  IF lcOption = 'RA'
    SELECT (lcStyAls)
    llMisMatch = OLDVAL('TotRA') <> laRa[9] OR;
                 OLDVAL('RA1') <> laRa[1] OR;
                 OLDVAL('RA2') <> laRa[2] OR;
                 OLDVAL('RA3') <> laRa[3] OR;
                 OLDVAL('RA4') <> laRa[4] OR;
                 OLDVAL('RA5') <> laRa[5] OR;
                 OLDVAL('RA6') <> laRa[6] OR;
                 OLDVAL('RA7') <> laRa[7] OR;
                 OLDVAL('RA8') <> laRa[8]
    
    IF llMisMatch
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET 
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF            
    IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      SELECT (lcSDyAls)
      SCAN REST WHILE &lcDyeWhile
      llMisMatch = OLDVAL('TotRA') <> TotRA OR;
                       OLDVAL('RA1') <> RA1 OR;
                       OLDVAL('RA2') <> RA2 OR;
                       OLDVAL('RA3') <> RA3 OR;
                       OLDVAL('RA4') <> RA4 OR;
                       OLDVAL('RA5') <> RA5 OR;
                       OLDVAL('RA6') <> RA6 OR;
                       OLDVAL('RA7') <> RA7 OR;
                       OLDVAL('RA8') <> RA8
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET 
        laRebMsg[3] = " "
        =lfVryRport()
        EXIT
      ENDIF
        ENDSCAN
    ENDIF
  ENDIF 
  
  IF lcOption = 'RET'
    SELECT (lcStyAls)
    llMisMatch = OLDVAL('TotRet') <> laRet[9] OR;
                 OLDVAL('RET1') <> laRet[1] OR;
                 OLDVAL('RET2') <> laRet[2] OR;
                 OLDVAL('RET3') <> laRet[3] OR;
                 OLDVAL('RET4') <> laRet[4] OR;
                 OLDVAL('RET5') <> laRet[5] OR;
                 OLDVAL('RET6') <> laRet[6] OR;
                 OLDVAL('RET7') <> laRet[7] OR;
                 OLDVAL('RET8') <> laRet[8]
    
    IF llMisMatch
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET 
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF            
    IF !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      SELECT (lcSDyAls)
      SCAN REST WHILE &lcDyeWhile
        llMisMatch = OLDVAL('TotRET') <> TotRET OR;
                       OLDVAL('RET1') <> RET1 OR;
                       OLDVAL('RET2') <> RET2 OR;
                       OLDVAL('RET3') <> RET3 OR;
                       OLDVAL('RET4') <> RET4 OR;
                       OLDVAL('RET5') <> RET5 OR;
                       OLDVAL('RET6') <> RET6 OR;
                       OLDVAL('RET7') <> RET7 OR;
                       OLDVAL('RET8') <> RET8
        IF llMisMatch
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGRET 
          laRebMsg[3] = " "
          =lfVryRport()
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF 
  *: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]
  
  
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *DO lpGathFlds WITH loStyle && Update style record.
  DO lpGathFlds WITH lcStyAls && Update style record.
  *B128464,2 WSH 08/21/2005 [End]

  *-- update stydye record if system does not have multiware and dyelots.
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
  llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
  *B128464,2 WSH 08/21/2005 [End]
  
  IF !llStyDyeFl AND llExpr
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyDye && Update stydye record with zeros.
    DO lpGathFlds WITH lcSDyAls && Update stydye record with zeros.
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF
ENDIF

SELECT (lnAlias)
*-- end of lfUpdtLoop.

*!*************************************************************
*! Name      : lpGathFlds
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update Style record.
*!*************************************************************
*! Example   : = lpGathFlds()
*!*************************************************************
PROCEDURE lpGathFlds

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS loUpdTable, lcNewReplc
LPARAMETERS lcUpdCurs, lcNewReplc
*B128464,2 WSH 08/21/2005 [End]

LOCAL lnAlias
lnAlias = SELECT(0)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*loUpdTable.REPLACE(IIF(loUpdTable.llNative, "", "Rec_No WITH Rec_No"))
*SELECT (loUpdTable.lcCursorUpdate)
SELECT (lcUpdCurs)
*B128464,2 WSH 08/21/2005 [End]
FOR lnAllTrans = 1 TO ALEN(laUpdatArr,1)
  IF EMPTY(laUpdatArr[lnAllTrans,1])
    EXIT
  ENDIF
  
  IF EVALUATE(laUpdatArr[lnAllTrans,1]) &&AND !EOF(lcUpdCurs)
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF lcInvType = '0001'
    IF TYPE(lcUpdCurs + '.' + laUpdatArr[lnAllTrans,3] + '1') # 'U'
    *B128464,2 WSH 08/21/2005 [End]
    
    *E039550,1 WSH 08/07/2005 [End]
    
    GATHER FROM (laUpdatArr[lnAllTrans,2]) FIELDS ;
                (laUpdatArr[lnAllTrans,3]+'1'),;
                (laUpdatArr[lnAllTrans,3]+'2'),;
                (laUpdatArr[lnAllTrans,3]+'3'),;
                (laUpdatArr[lnAllTrans,3]+'4'),;
                (laUpdatArr[lnAllTrans,3]+'5'),;
                (laUpdatArr[lnAllTrans,3]+'6'),;
                (laUpdatArr[lnAllTrans,3]+'7'),;
                (laUpdatArr[lnAllTrans,3]+'8'),;
                (IIF(laUpdatArr[lnAllTrans,3] = 'NWO', 'nTotWo', IIF(laUpdatArr[lnAllTrans,3] = 'INTRANS', 'TOTINTRN', 'TOT'+laUpdatArr[lnAllTrans,3])))
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    ELSE
      REPLACE (IIF(laUpdatArr[lnAllTrans,3] = 'NWO', 'nTotWo', IIF(laUpdatArr[lnAllTrans,3] = 'INTRANS', 'TOTINTRN', 'TOT'+laUpdatArr[lnAllTrans,3]))) WITH EVALUATE(laUpdatArr[lnAllTrans,2] + '[9]')
    ENDIF
    *E039550,1 WSH 08/07/2005 [End]
    
    *-- if you pass replace expression, do it
    *B610814,1 TMI 08/20/2014 11:32 [Start]     
    *IF TYPE('lcNewReplc') = "C"
    IF TYPE('lcNewReplc') = "C" AND lfReplaceIsValid(lcNewReplc,'NSTKVAL')
      *B610814,1 TMI 08/20/2014 11:32 [End  ] 
      REPLACE &lcNewReplc
    ENDIF

    &laUpdatArr[lnAllTrans,2] = 0  && restore initial value for another accomulate.
  ENDIF
ENDFOR

SELECT (lnAlias)
*--- end of lpGathFlds.

*!*************************************************************
*! Name      : lfReplaceIsValid
*! Developer : TMI Tarek Mohamed Ibrahim
*! Date      : 08/18/2014
*! Purpose   : This function is to overcome the numeric overflow happening in the updating of the STYDYE.NSTKVAL where the sum comes from STYINVJL
**             Currently it handles only this field, if this problem happened to other fields code could be updated
*!*************************************************************  
**tmi**t20140715.0001
*B610814,1 TMI 08/20/2014 11:32 [Start] 
FUNCTION lfReplaceIsValid
LPARAMETERS lcReplace,lcFld
lcReplace = UPPER(lcReplace)
lcFld = UPPER(lcFld)
LOCAL llRet,lcArr
llRet = .T.

lcArr = IIF('LALOCSTOCK'$lcReplace,'LALOCSTOCK','LASTOCK')
DO CASE 
CASE lcFld = 'NSTKVAL'
  IF LEN(ALLTRIM(STR(&lcArr.[10],30))) > lfGetLen(lcFld)
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    lcCst = IIF(&lcArr.[9]=0,&lcArr.[10],&lcArr.[10]/&lcArr.[9])
    lcCst = ', Cost:'+ALLTRIM(STR(lcCst,30))+', '
    laRebMsg[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMREBAL_COMP,loFormSet.GetHeaderText("LANG_SMREBAL_COMP",loFormSet.HeaderAlias)) + ;
    lcCurrComp_ID +": "+ STYLE + lcCst + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMREBAL_EXTRFIELDSIZE,loFormSet.GetHeaderText("LANG_SMREBAL_EXTRFIELDSIZE",loFormSet.HeaderAlias))
    laRebMsg[3] = " "
    =lfVryRport()
    llRet = .F.
  ENDIF 
ENDCASE

RETURN llRet
*- end of lfReplaceIsValid

*!*************************************************************
*! Name      : lfUpdtOper
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update Work process cases.
*!*************************************************************
*! Example   : = lfUpdtOper()
*!*************************************************************
FUNCTION lfUpdtOper
LPARAMETERS lcOption

LOCAL lnAlias
lnAlias  = SELECT(0)
lcOption = UPPER(lcOption)

*-- if there no records in transaction file.

*B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
*IF !loWorkFile.SEEK(IIF(lcInvType = '0001' AND llWipAdj AND lcWorkFile = lcWipJAls, '', lcInvType) + &lcStyAls..Style)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*IF EOF(lcWorkFile)
LOCAL lcWhereCond
m.cInvType  = lcInvType
m.Style     = EVALUATE(lcStyAls + '.STYLE')
lcWhereCond = IIF(lcInvType = '0001' AND llWipAdj AND lcWorkFile = lcWipJAls, '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style"

IF !lfSEEK(lcWorkFile, lcWhereCond, IIF(lcInvType = '0001' AND llWipAdj AND lcWorkFile = lcWipJAls, '', m.cInvType) + m.Style)
*B128464,2 WSH 08/21/2005 [End]

*B128464,1 WSH 06/12/2005 [End]

  IF lcOption = 'WIP' AND !llAdoDye
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *IF lcInvType = '0001'
    *E039550,1 WSH 08/07/2005 [End]
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyle, laWip, 'WIP', 'TOTWIP'            && Update WIP
      DO lpUpDtWork WITH lcStyAls, laWip, 'WIP', 'TOTWIP'            && Update WIP
      *B128464,2 WSH 08/21/2005 [End]
      
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *ENDIF
    *E039550,1 WSH 08/07/2005 [End]
    
    *-- update stydye record if system does not have multiware and dyelots.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
    llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF !llStyDyeFl AND llExpr
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyDye, laWip, 'WIP', 'TOTWIP'            && Update WIP
      DO lpUpDtWork WITH lcSDyAls, laWip, 'WIP', 'TOTWIP'            && Update WIP
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDIF
  ENDIF
  
  IF lcOption = 'WO' AND !llAdoDye
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *IF lcInvType = '0001'
    *E039550,1 WSH 08/07/2005 [End]
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyle, laWo, 'NWO', 'NTOTWO'             && Update nwo
      DO lpUpDtWork WITH lcStyAls, laWo, 'NWO', 'NTOTWO'             && Update nwo
      *B128464,2 WSH 08/21/2005 [End]
      
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *ENDIF
    *E039550,1 WSH 08/07/2005 [End]
    
    *-- Update stydye record if system does not have multiware and dyelots.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
    llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF !llStyDyeFl AND llExpr
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyDye, laWo, 'NWO', 'NTOTWO'             && Update nwo
      DO lpUpDtWork WITH lcSDyAls, laWo, 'NWO', 'NTOTWO'             && Update nwo
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDIF
  ENDIF
  
  IF lcOption = 'TRN'
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *IF lcInvType = '0001'
    *E039550,1 WSH 08/07/2005 [End]
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyle, laIntrans, 'INTRANS', 'TOTINTRN'  && Update Intrans
      DO lpUpDtWork WITH lcStyAls, laIntrans, 'INTRANS', 'TOTINTRN'  && Update Intrans
      *B128464,2 WSH 08/21/2005 [End]
      
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *ENDIF
    *E039550,1 WSH 08/07/2005 [End]
    
    *-- update stydye record if system does not have multiware and dyelots.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
    llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF !llStyDyeFl AND llExpr
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpUpDtWork WITH loStyDye, laIntrans, 'INTRANS', 'TOTINTRN'  && Update Intrans
      DO lpUpDtWork WITH lcSDyAls, laIntrans, 'INTRANS', 'TOTINTRN'  && Update Intrans
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDIF
  ENDIF

ELSE  && Find transactions in current file.

  SELECT (lcWorkFile)
  
  *-- If Wip Only from W.I.P. adjustment file.
  IF lcOption = 'ADJ'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF loWipAdj.SEEK(&lcStyAls..Style)
    IF lfSEEK(lcWorkFile, "Style = ?m.Style", m.Style)
    *B128464,2 WSH 08/21/2005 [End]
    
      *-- Case of Base module only or Manufacturing module for imported
      *-- styles, Scan the WIP adjustment file
      SCAN REST WHILE Style+cWareCode = &lcStyAls..Style
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          laWip[lnCount] = laWip[lnCount] + Adj&lcCount
        ENDFOR
      ENDSCAN
      
      *-- Update stydye file if there is warehouse records.
      =lfUpStyDye(Style,cWareCode,SPACE(10),'WIP','TOTWIP','&lcWipJAls..ADJ')
    ENDIF
  ELSE  && Find Transaction file.
    PRIVATE lcStyle
    LOCAL lcLastType, lcLastBD, lcSrcWar, lcGrpWare
    
    *-- Save current record values to update it.
    lcStyle    = Style
    lcLastType = cStyType
    lcLastBD   = cBusDocu
    lcSrcWar   = Vendor
    lcGrpWare  = cWareCode
    
    IF lcOption = 'WIP' AND '+PADR(cWareCode,6)' $ lcBatChng
      lcBatChng = STRTRAN(lcBatChng, '+PADR(cWareCode,6)')
    ENDIF
    lcKeyVal = &lcBatChng
    
    *-- Scan Master Transaction file (Cuttktl OR Posln) which ordered by Style.
    SCAN REST WHILE &lcWhilExpr
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *loWorkHdr.SEEK(&lcHdrLnRel)
      SCATTER MEMVAR
      =lfSEEK(lcWorkHdr, lcHdrLnCnd, &lcHdrLnRel)
      *B128464,2 WSH 08/21/2005 [End]
      
      llExVal = EVALUATE(lcForExpr)
      IF !llExVal
        LOOP 
      ENDIF
      
      *-- if current group was changed.
      IF lcKeyVal <> &lcBatChng

        *-- adjust current used arrays and update stydye record for this group.
        DO lpCalGrpDt WITH lcOption, lcLastBD, lcGrpWare, IIF(lcOption = 'WIP', 'laTWip', ''), IIF(lcOption = 'WO', 'laTWo1', '')

        *-- if inter-location case OR Adornment Order.
        IF (lcWorkFile = lcPoLAls) AND lcLastType $ 'AN'
          DO lpCalGrpDt WITH lcOption, lcLastBD, lcSrcWar, IIF(lcOption = 'WIP', 'laSWip', ''), IIF(lcOption = 'WO', 'laSWo1', '')
        ENDIF  
      ENDIF
      
      IF lcInvType = '0002'
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *= gfGetUOMData(EVALUATE(lcWorkFile + '.cUOMCode'), '', '', @lnConv)
        m.cUOMCode = EVALUATE(lcWorkFile + '.cUOMCode')
        *E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
        *llResult   = lfSQLRun("SELECT nConF FROM UOM (INDEX = UOMCODE) WHERE cUOMCode = ?m.cUOMCode", "TmpUOM")
        llResult   = lfSQLRun("SELECT nConF FROM UOM WITH(INDEX(UOMCODE)) WHERE cUOMCode = ?m.cUOMCode", "TmpUOM")
        *E302821,1 TMI 12/27/2010 [End  ] 
        lnConv     = IIF(llResult, TmpUOM.nConF, 1)
        IF USED("TmpUOM")
          USE IN TmpUOM
        ENDIF
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
      
      lnTranSign = IIF(cBusDocu = 'R', -1, 1)  && Transcation sign [Reverse sign in Return Case]
      
      *-- Transaction code cases.
      DO CASE
      
        CASE Trancd = '1'
        
          lcStyle    = Style
          lcLastType = cStyType
          lcLastBD   = cBusDocu
          lcSrcWar   = Vendor
          lcGrpWare  = cWareCode

          FOR lnW = 1 TO 8
            lcW = STR(lnW,1)

            *-- if want to update wips and this P/O not completed
            IF (lcOption = 'WIP') AND &lcPoHAls..STATUS <> 'C'
              IF lcInvType = '0001'
                laTWip[lnW] = laTWip[lnW] + (lnTranSign * Qty&lcW)  && accomulate target wip array
              ELSE
                laTWip[lnW] = laTWip[lnW] + ROUND(lnTranSign * MAX(Qty&lcW, 0) * lnConv, 1)
              ENDIF

              *-- if Inter-Location lines 
              IF cStyType = 'N' 
                laSWip[lnW] = laSWip[lnW] - Qty&lcW   && accomulate source wip array
              ENDIF  
            ENDIF
             
            *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
            *IF (lcOption = 'WO') AND &lcPoHAls..STATUS <> 'C'
            IF (lcOption = 'WO') 
            *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
            
              laTWo1[lnW] = laTWo1[lnW] + (lnTranSign * Qty&lcW)  && accomulate target nWo array

              *-- if Inter-Location lines.
              IF cStyType = 'N'
                laSWo1[lnW] = laSWo1[lnW] - Qty&lcW  && accomulate source wip array
              ENDIF
            ENDIF
          ENDFOR

        CASE Trancd $ '245'  && Receive, Damage, Or Canceled

          FOR lnW = 1 TO 8
            lcW = STR(lnW,1)

            IF (lcOption = 'WIP') AND &lcPoHAls..STATUS <> 'C'
              IF lcInvType = '0001'
                laTWip[lnW] = laTWip[lnW] - (lnTranSign * Qty&lcW)
              ELSE
                laTWip[lnW] = laTWip[lnW] - ROUND(lnTranSign * MAX(Qty&lcW, 0) * lnConv, 1)
              ENDIF

              *-- if Inter-Location line OR Adornment Order.
              IF cStyType = 'N' 
                laSWip[lnW] = laSWip[lnW] + Qty&lcW
              ENDIF  
            ENDIF

            *-- if Inter-Location line.
            IF (lcOption = 'WO')  AND (cStyType = 'N') AND &lcPoHAls..STATUS <> 'C'
              laSWo1[lnW] = laSWo1[lnW] + Qty&lcW
            ENDIF

            *-- if this P/O was completed.
            *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
*!*	            IF (lcOption = 'WO') AND &lcPoHAls..STATUS = 'C' 
*!*	              laTWo1[lnW] = laTWo1[lnW] + (lnTranSign * Qty&lcW)  && accomulate target nWo array from rest transaction codes.
*!*	              IF (cStyType = 'N')
*!*	                laSWo1[lnW] = laSWo1[lnW] - Qty&lcW
*!*	              ENDIF
*!*	            ENDIF
            *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
            
          ENDFOR
          
        CASE Trancd = '3'    && P/O Shipment case.

          *-- if user want to rebalance in transit.
          IF lcOption = 'TRN'
            FOR lnW = 1 TO 8
              lcW = STR(lnW,1)
              laIntrans[lnW] = laIntrans[lnW] + Qty&lcW
            ENDFOR  
            =lfUpStyDye(Style,cWareCode,SPACE(10),'INTRANS','TOTINTRN','&lcPoLAls..QTY')
          ENDIF
        
        CASE Trancd = '6'    && Inter-Location line.

          FOR lnW = 1 TO 8
            lcW = STR(lnW,1)

            IF (lcOption = 'WIP') AND &lcPoHAls..STATUS <> 'C' AND &lcPoHAls..cStyType <> 'A' 
              IF lcInvType = '0001'
                laSWip[lnW] = laSWip[lnW] + Qty&lcW
              ELSE
                laSWip[lnW] = laSWip[lnW] + ROUND(MAX(Qty&lcW, 0) * lnConv, 1)
              ENDIF
            ENDIF

            IF lcOption = 'WO'
              laSWo1[lnW] = laSWo1[lnW] + Qty&lcW
            ENDIF

            IF lcOption = 'TRN'
              laIntrans[lnW] = laIntrans[lnW] + Qty&lcW
            ENDIF  
          ENDFOR
          
          *-- Update intransit fields.
          IF lcOption = 'TRN'
            =lfUpStyDye(Style,cWareCode,SPACE(10),'INTRANS','TOTINTRN','&lcPoLAls..QTY')
          ENDIF  
      ENDCASE

      lcKeyVal = &lcBatChng
    ENDSCAN

    *-- Update last group.
    DO lpCalGrpDt WITH lcOption, lcLastBD, lcGrpWare, IIF(lcOption = 'WIP', 'laTWip', ''), IIF(lcOption = 'WO', 'laTWo1', '')
    
    IF (lcWorkFile = lcPoLAls) AND lcLastType $ 'AN'
      DO lpCalGrpDt WITH lcOption, lcLastBD, lcSrcWar, IIF(lcOption = 'WIP', 'laSWip', ''), IIF(lcOption = 'WO', 'laSWo1', '')
    ENDIF

    *-- Update style record.
    SELECT (lcStyAls)

    IF lcOption = 'WIP'
      
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      *IF lcInvType = '0001'
      *E039550,1 WSH 08/07/2005 [End]
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyle, laWip, 'WIP', 'TOTWIP'
        DO lpUpDtWork WITH lcStyAls, laWip, 'WIP', 'TOTWIP'
        *B128464,2 WSH 08/21/2005 [End]
        
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      *ENDIF
      *E039550,1 WSH 08/07/2005 [End]
      
      *-- update stydye record if system does not have multiware and dyelots.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
      llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
      IF !llStyDyeFl AND llExpr
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyDye, laWip, 'WIP', 'TOTWIP'
        DO lpUpDtWork WITH lcSDyAls, laWip, 'WIP', 'TOTWIP'
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
    ENDIF

    IF lcOption = 'WO'
      
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      *IF lcInvType = '0001'
      *E039550,1 WSH 08/07/2005 [End]
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyle, laWo, 'NWO', 'NTOTWO'
        DO lpUpDtWork WITH lcStyAls, laWo, 'NWO', 'NTOTWO'
        *B128464,2 WSH 08/21/2005 [End]
        
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      *ENDIF
      *E039550,1 WSH 08/07/2005 [End]
      
      *-- update stydye record if system does not have multiware and dyelots.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
      llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
      IF !llStyDyeFl AND llExpr
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyDye, laWo,'NWO','NTOTWO'
        DO lpUpDtWork WITH lcSDyAls, laWo,'NWO','NTOTWO'
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
    ENDIF

    IF lcOption = 'TRN'
      llMisMatch = .F.
      IF lcInvType = '0001'
        laIntrans[9] = laIntrans[1]+laIntrans[2]+laIntrans[3]+laIntrans[4]+;
                       laIntrans[5]+laIntrans[6]+laIntrans[7]+laIntrans[8]

        IF llViewLog
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyle.lcCursorUpdate)
          SELECT (lcStyAls)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotInTrn') <> laIntrans[9] OR;
                       OLDVAL('InTrans1') <> laIntrans[1] OR;
                       OLDVAL('InTrans2') <> laIntrans[2] OR;
                       OLDVAL('InTrans3') <> laIntrans[3] OR;
                       OLDVAL('InTrans4') <> laIntrans[4] OR;
                       OLDVAL('InTrans5') <> laIntrans[5] OR;
                       OLDVAL('InTrans6') <> laIntrans[6] OR;
                       OLDVAL('InTrans7') <> laIntrans[7] OR;
                       OLDVAL('InTrans8') <> laIntrans[8]
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGTRN
            laRebMsg[3] = " "
            =lfVryRport()
          ENDIF
        ENDIF
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyle, laIntrans, 'INTRANS', 'TOTINTRN'
        DO lpUpDtWork WITH lcStyAls, laIntrans, 'INTRANS', 'TOTINTRN'
        *B128464,2 WSH 08/21/2005 [End]
        
      *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
      ELSE
        laIntrans[9] = laIntrans[1]+laIntrans[2]+laIntrans[3]+laIntrans[4]+;
                       laIntrans[5]+laIntrans[6]+laIntrans[7]+laIntrans[8]

        IF llViewLog
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyle.lcCursorUpdate)
          SELECT (lcStyAls)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotInTrn') <> laIntrans[9]
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGTRN
            laRebMsg[3] = " "
            =lfVryRport()
          ENDIF
        ENDIF
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyle, laIntrans, 'INTRANS', 'TOTINTRN'
        DO lpUpDtWork WITH lcStyAls, laIntrans, 'INTRANS', 'TOTINTRN'
        *B128464,2 WSH 08/21/2005 [End]
        
      *E039550,1 WSH 08/07/2005 [End]
      
      ENDIF

      *-- update stydye record if system does not have multiware and dyelots.
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
      llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
      IF !llStyDyeFl AND llExpr
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *DO lpUpDtWork WITH loStyDye, laIntrans,'INTRANS','TOTINTRN'
        DO lpUpDtWork WITH lcSDyAls, laIntrans,'INTRANS','TOTINTRN'
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF llViewLog AND !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..Style)
      IF llViewLog AND !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcSDyAls)
        SCAN REST WHILE &lcDyeWhile
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
          *SELECT (loStyDye.lcCursorUpdate)
          *B128464,2 WSH 08/21/2005 [End]
          
          llMisMatch = OLDVAL('TotInTrn') <> TotInTrn OR;
                       OLDVAL('InTrans1') <> InTrans1 OR;
                       OLDVAL('InTrans2') <> InTrans2 OR;
                       OLDVAL('InTrans3') <> InTrans3 OR;
                       OLDVAL('InTrans4') <> InTrans4 OR;
                       OLDVAL('InTrans5') <> InTrans5 OR;
                       OLDVAL('InTrans6') <> InTrans6 OR;
                       OLDVAL('InTrans7') <> InTrans7 OR;
                       OLDVAL('InTrans8') <> InTrans8
          IF llMisMatch
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +": "+ Style + LANG_SMREBAL_WRONGTRN
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
      laIntrans = 0
    ENDIF  
  ENDIF  && end If Wip Only from W.I.P. adjustment file.
ENDIF

SELECT (lnAlias)
RETURN
*-- end of lfUpdtOper.

*!*************************************************************
*! Name      : lpUpDtWork
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update Style file record for work process cases.
*!*************************************************************
*! Example   : = lpUpDtWork()
*!*************************************************************
PROCEDURE lpUpDtWork

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS loUpdTable, laUpdArr, lcUpdFld, lcTotFld
LPARAMETERS lcUpdCurs, laUpdArr, lcUpdFld, lcTotFld
*B128464,2 WSH 08/21/2005 [End]

LOCAL lnAlias
lnAlias = SELECT(0)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*loUpdTable.REPLACE(IIF(loUpdTable.llNative, "", "Rec_No WITH Rec_No"))
*SELECT (loUpdTable.lcCursorUpdate)
SELECT (lcUpdCurs)
*B128464,2 WSH 08/21/2005 [End]

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
IF lcInvType = '0001'
*E039550,1 WSH 08/07/2005 [End]

GATHER FROM laUpdArr FIELDS ;
            (lcUpdFld+'1'),;
            (lcUpdFld+'2'),;
            (lcUpdFld+'3'),;
            (lcUpdFld+'4'),;
            (lcUpdFld+'5'),;
            (lcUpdFld+'6'),;
            (lcUpdFld+'7'),;
            (lcUpdFld+'8'),;
            (lcTotFld)

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
ELSE
  REPLACE (lcTotFld) WITH laUpdArr[9]
ENDIF
*E039550,1 WSH 08/07/2005 [End]

SELECT (lnAlias)
*-- end of lpUpDtWork.

*!*************************************************************
*! Name      : lpCalGrpDt
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Adjust passed group data then update stydye file .
*!*************************************************************
*! Example   : = lpCalGrpDt()
*!*************************************************************
PROCEDURE lpCalGrpDt
LPARAMETERS lcOption, lcBusType, lcMastWare, laTempArr1, laTempArr2

LOCAL lnAlias
lnAlias = SELECT(0)

IF lcOption = 'WIP'
  FOR lnAdj = 1 TO 8
    *-- Detect (C/T or P/O) and Return P/O Over Receive Quantities.
    IF (lcBusType = 'P' AND &laTempArr1[lnAdj] < 0) OR ;
       (lcBusType = 'R' AND &laTempArr1[lnAdj] > 0)
      &laTempArr1[lnAdj] = 0
    ENDIF
    &laTempArr1[9] = &laTempArr1[9] + &laTempArr1[lnAdj]
  ENDFOR

  IF &laTempArr1[9] <> 0
    FOR lnAdd = 1 TO 9
      laWip[lnAdd] = laWip[lnAdd] + &laTempArr1[lnAdd]
    ENDFOR
    DO lpStyDyeUp WITH lcVrUpWip, lcStyle, lcMastWare, &laTempArr1, 'WIP', 'TOTWIP'
  ENDIF
ENDIF  

IF lcOption = 'WO'

  &laTempArr2[9] = &laTempArr2[1]+&laTempArr2[2]+&laTempArr2[3]+&laTempArr2[4]+;
                   &laTempArr2[5]+&laTempArr2[6]+&laTempArr2[7]+&laTempArr2[8]

  IF &laTempArr2[9] <> 0
    FOR lnAdd = 1 TO 9
      laWo[lnAdd] = laWo[lnAdd] + &laTempArr2[lnAdd]
    ENDFOR
    DO lpStyDyeUp WITH lcVrUpWo, lcStyle,lcMastWare,&laTempArr2,'NWO','NTOTWO'    
    laTempWo = 0
  ENDIF  
            
ENDIF

SELECT (lnAlias)
*-- end of lpCalGrpDt.

*!*************************************************************
*! Name      : lpStyDyeUp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Update stydye record for current group.
*!*************************************************************
*! Example   : = lpStyDyeUp()
*!*************************************************************
PROCEDURE lpStyDyeUp
LPARAMETERS lcUpdVryF, lcGrpStyle, lcGrpWare, laUpdArr, lcUpdFld, lcTotFld

LOCAL lnAlias
lnAlias    = SELECT(0)
lcGrpStyle = PADR(lcGrpStyle,19)
lcGrpWare  = PADR(lcGrpWare,6)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+lcGrpStyle+lcGrpWare+SPACE(10))
LOCAL lcWhereCond
m.cInvType  = lcInvType
m.Style     = lcGrpStyle
m.cWareCode = lcGrpWare
lcWhereCond = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style AND cWareCode = ?m.cWareCode AND Dyelot = ''"

llExpr = lfSEEK(lcSDyAls, lcWhereCond, IIF(lcInvType = '0001', '', m.cInvType)+m.Style+m.cWareCode+SPACE(10))
*B128464,2 WSH 08/21/2005 [End]

IF lcUpdVryF = 'U' AND !llExpr
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loStyDye.APPEND()
  *SELECT (loStyDye.lcCursorUpdate)
  SELECT (lcSDyAls)
  APPEND BLANK
  *B128464,2 WSH 08/21/2005 [End]
  
  REPLACE STYLE     WITH lcGrpStyle,;
          cWareCode WITH lcGrpWare
  
  *B128464,1 WSH 06/20/2005 Replace the Inventory Type field in case of itemloc SQL file. [Start]
  IF lcInvType # '0001'
    REPLACE cInvType WITH lcInvType
    =lfSQLAppend()
  ENDIF
  
  llExpr = .T.
  *B128464,1 WSH 06/20/2005 [Emd]
  
ENDIF

IF llStyDyeFl AND llExpr
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
  *SELECT (loStyDye.lcCursorUpdate)
  SELECT (lcSDyAls)
  *B128464,2 WSH 08/21/2005 [End]
  
  REPLACE (lcUpdFld+'1') WITH EVALUATE(lcUpdFld+'1') + laUpdArr[1] ,;
          (lcUpdFld+'2') WITH EVALUATE(lcUpdFld+'2') + laUpdArr[2] ,;
          (lcUpdFld+'3') WITH EVALUATE(lcUpdFld+'3') + laUpdArr[3] ,;
          (lcUpdFld+'4') WITH EVALUATE(lcUpdFld+'4') + laUpdArr[4] ,;
          (lcUpdFld+'5') WITH EVALUATE(lcUpdFld+'5') + laUpdArr[5] ,;
          (lcUpdFld+'6') WITH EVALUATE(lcUpdFld+'6') + laUpdArr[6] ,;
          (lcUpdFld+'7') WITH EVALUATE(lcUpdFld+'7') + laUpdArr[7] ,;
          (lcUpdFld+'8') WITH EVALUATE(lcUpdFld+'8') + laUpdArr[8] ,;
          (lcTotFld)     WITH EVALUATE(lcTotFld)     + laUpdArr[9]
ENDIF
laUpdArr = 0  && Clear Temp. Arrays for another use.
SELECT (lnAlias)
*-- end of Update Stydye file.

*!*************************************************************
*! Name      : lfCloseFls
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2005
*! Purpose   : Close all open files (another alias name)
*!*************************************************************
*! Example   : = lfCloseFls()
*!*************************************************************
FUNCTION lfCloseFls

LOCAL lnFileCnt

FOR lnFileCnt = 1 TO ALEN(laFileStr, 1)
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *&laFileStr[lnFileCnt, 5] = .NULL.
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    USE IN (laFileStr[lnFileCnt, 4])
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    =gfCloseTable(laFileStr[lnFileCnt, 4])
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *B128464,2 WSH 08/21/2005 [End]
  
ENDFOR
*-- end of lfCloseFls.

*!*************************************************************
*! Name      : lfStyStock
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Calculate Style and Style/Location Stock.
*!*************************************************************
*! Example   : = lfStyStock()
*!*************************************************************
FUNCTION lfStyStock
LOCAL lnAlias
lnAlias = SELECT(0)

PRIVATE lnAliasNow , lcStyWare , lnI

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
PRIVATE lcStyCond
*B128464,2 WSH 08/21/2005 [End]

=ACOPY(laStkArr, laUpdatArr)  && copy stock array to laUpdatArr to call generic code.

*-- Update StyDye Records with Zeros
=lfZeroDye()

LOCAL llExitLoop
lcStyExp   = IIF(lcInvType = '0001', '', lcInvType) + &lcStyAls..Style

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*llExitLoop = !loStyInvJL.SEEK(lcStyExp, .F., .T.)
LOCAL lcStyCnd
m.cInvType = lcInvType
m.Style    = EVALUATE(lcStyAls + '.Style')
lcStyCnd   = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style"


*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
IF m.cInvType = '0002'
  lnConv = 1
  IF !EMPTY(&lcStyAls..cConvBuy)
    =gfGetUOMData(&lcStyAls..cConvBuy, '', '', @lnConv)
  ENDIF   
ENDIF   

llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
IF !llExpr AND lcVrUpStk = 'U' AND !EMPTY(&lcStyAls..cDefWare)
  =lfAddRec(&lcStyAls..Style,&lcStyAls..cDefWare,&lcStyAls..Ave_cost)
ENDIF
SELECT (lcSDyAls)
SCAN REST WHILE IIF(lcInvType = '0001', Style = m.Style, cInvType+Style = m.cInvType+m.Style) 
  m.cWarecode = cWarecode
  lcStyWareCond   = IIF(lcInvType = '0001', '', 'cInvType = ?m.cInvType AND ') + 'Style = ?m.Style AND cWareCode = ?m.cWarecode'
  IF !lfSEEK(lcStyJlAls, lcStyWareCond, IIF(lcInvType = '0001', '', m.cInvType) + m.Style+m.cWarecode, .F.)
    IF m.cInvType = '0002'
	  lnCstBuy = &lcStyAls..TotCost * lnConv 
    ENDIF   
    lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH " + lcStyAls + ".TotCost"+IIF(m.cInvType = '0001','',",navecstbuy with lnCstBuy")
    DO lpGathFlds WITH lcSDyAls, lcExpr && Stydye record.
  ENDIF
ENDSCAN
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]



*B607944,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file[Start]
*llExitLoop = !lfSEEK(lcStyJlAls, lcStyCnd, lcStyExp, .T.)
llExitLoop = !lfSEEK(lcStyJlAls, lcStyCnd, lcStyExp, .F.)
*B607944,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file[End]
*B128464,2 WSH 08/21/2005 [End]

*-- if no transactions found for this style in style inventory journal.
IF llExitLoop

  *-- Update Style record with zero values if single location.
  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *IF lcInvType = '0001'
  *E039550,1 WSH 08/07/2005 [End]
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyle, "NSTKVAL WITH 0.00 , AVE_COST WITH TotCost"
    
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
    *DO lpGathFlds WITH lcStyAls, "NSTKVAL WITH 0.00 , AVE_COST WITH TotCost"
    DO lpGathFlds WITH lcStyAls, "NSTKVAL WITH 0.00 , AVE_COST WITH TotCost" + ;
    				   IIF(m.cInvType = '0001','',",navecstbuy WITH TotCost * lnConv ")
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
    
    *B128464,2 WSH 08/21/2005 [End]
    
  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *ENDIF
  *E039550,1 WSH 08/07/2005 [End]
  
  *-- Update stydye record if system does not have multiware and dyelots.
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *llExpr = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
  llExpr = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
  *B128464,2 WSH 08/21/2005 [End]
  
  IF !llMultWare AND llExpr 
    
    *WSH [Start]
    *lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH lcStyAls.TotCost"  
    
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
    *lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH " + lcStyAls + ".TotCost"
    lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH " + lcStyAls + ".TotCost" + ;
    		 IIF(m.cInvType = '0001','',",navecstbuy WITH "+ lcStyAls +".TotCost * lnConv ")
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
    
    *WSH [End]
    
    *-- Update StyDye record with zero values.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyDye, lcExpr && Stydye record.
    DO lpGathFlds WITH lcSDyAls, lcExpr && Stydye record.
    *B128464,2 WSH 08/21/2005 [End]
    
  ELSE
    IF !llExpr AND lcVrUpStk = 'U' AND !EMPTY(&lcStyAls..cDefWare)
      =lfAddRec(&lcStyAls..Style,&lcStyAls..cDefWare,&lcStyAls..Ave_cost)
    ENDIF
  ENDIF
  
ELSE  && Else there are some transactions in style inventory journal file.
  SELECT (lcStyJlAls)
  IF llMultWare
    lcStyWare = lcStyExp + cWareCode
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    m.cStyJWare = cWareCode
    lcStyCond   = IIF(lcInvType = '0001', '', 'cInvType = ?m.cInvType AND ') + 'Style = ?m.Style AND cWareCode = ?m.cStyJWare'
    *B128464,2 WSH 08/21/2005 [End]
    
  ENDIF 
  IF lcInvType = '0001'
    lcWhileLoop = "Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode = lcStyExp"
*: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance
    lcWhileLoop1 = lcStyExp
*: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance
  ELSE
    *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
    *lcWhileLoop = "cInvType+Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode = lcStyExp"
    lcWhileLoop = "CINVTYPE+STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE+CTRCODE+STR(LINENO,6)+CTRTYPE+STR(NLINENO,4)= lcStyExp"    
    *B608487,1 WAM 03/19/2008 (End)
    
*: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance
    lcWhileLoop1 = lcStyExp
*: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance
  ENDIF

  *B607832,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file [Start]

  *: B608214,1 SSH 08/08/2007 commentd out to be seek
  *LOCATE FOR &lcWhileLoop.
  =SEEK(lcWhileLoop1)
  *: B608214,1 SSH 08/08/2007 Inventory Control rebalance performance

  *B607832,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file [End]
  
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  *DO WHILE !llExitLoop AND &lcWhileLoop.
  SCAN REST WHILE &lcWhileLoop.
  *B608487,1 WAM 03/19/2008 (End)

    SELECT (lcStyJlAls)
    llFound = .T.
    
    *-- if you go to another Style/Location group
    lcGroupChng = IIF(lcInvType = "0001", "", "cInvType + ") + "Style + cWareCode <>  lcStyWare"
    
    IF llMultWare AND &lcGroupChng.
      IF lcVrUpStk = 'V'
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *= loStyDye.SEEK(lcStyWare)
        = lfSEEK(lcSDyAls, lcStyCond, lcStyWare)
        *B128464,2 WSH 08/21/2005 [End]
        
      ELSE
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *llFound = loStyDye.SEEK(lcStyWare)
        llFound = lfSEEK(lcSDyAls, lcStyCond, lcStyWare)
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF

      IF !llFound AND lcVrUpStk = 'U'
        IF lcInvType = '0001'
          
          *B128464,1 WSH 06/20/2005 Get the correct to get the Fabric Code. [Start]
          *=lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,5),&lcStyAls..Ave_Cost)
          =lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,6),&lcStyAls..Ave_Cost)
          *B128464,1 WSH 06/20/2005 [End]
          
        ELSE
          
          *B128464,1 WSH 06/20/2005 Get the correct to get the Fabric Code. [Start]
          *=lfAddRec(SUBSTR(lcStyWare,7,19),SUBSTR(lcStyWare,26,5),&lcStyAls..Ave_Cost)
          =lfAddRec(SUBSTR(lcStyWare,5,19),SUBSTR(lcStyWare,24,6),&lcStyAls..Ave_Cost)
          *B128464,1 WSH 06/20/2005 [End]
          
        ENDIF
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpGathFlds WITH loStyDye, "nStkVal WITH laLocStock[10] ," +;
                         "Ave_Cost WITH IIF(TotStk = 0, Ave_Cost, ABS(nStkVal/TotStk))"
      
      *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
      *DO lpGathFlds WITH lcSDyAls, "nStkVal WITH laLocStock[10] ," +;
                         "Ave_Cost WITH IIF(TotStk = 0, Ave_Cost, ABS(nStkVal/TotStk))"
      DO lpGathFlds WITH lcSDyAls, "nStkVal WITH laLocStock[10] ," +;
                         "Ave_Cost WITH IIF(TotStk = 0, Ave_Cost, ABS(nStkVal/TotStk))" + ;
                         IIF(m.cInvType = '0001','',",navecstbuy WITH IIF(TotStk = 0, Ave_Cost, ABS(laLocStock[10]/TotStk)) * lnConv")
      *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
      
      *B128464,2 WSH 08/21/2005 [End]
      
      SELECT (lcStyJlAls)
    ENDIF
    
    *-- Accomulate transactions in database array.
    FOR lnJ = 1 To 8
      lcJ = STR(lnJ,1)
      *! B609548,1 MMT 03/07/2011 Rebalance update stock fields of non inventory styles incorrectly[Start]
      *laStock[lnJ]    = laStock[lnJ] + nStk&lcJ
      laStock[lnJ]    = laStock[lnJ] + IIF(&lcStyAls..linvsty ,nStk&lcJ,0)
      *! B609548,1 MMT 03/07/2011 Rebalance update stock fields of non inventory styles incorrectly[End]
      IF llMultWare
        *! B609548,1 MMT 03/07/2011 Rebalance update stock fields of non inventory styles incorrectly[Start]
        *laLocStock[lnJ] = laLocStock[lnJ] + nStk&lcJ
        laLocStock[lnJ] = laLocStock[lnJ] + IIF(&lcStyAls..linvsty ,nStk&lcJ,0)
        *! B609548,1 MMT 03/07/2011 Rebalance update stock fields of non inventory styles incorrectly[END]
      ENDIF
    ENDFOR
    
    laStock[9]  = laStock[1] + laStock[2] + laStock[3] + laStock[4] +;
                  laStock[5] + laStock[6] + laStock[7] + laStock[8]
    *B609996,1 MMT 07/11/2012 Rebalance program use ncost * ntotstk instead of nstkval  from styinvjl table[T20120316.0004][Start]
    *laStock[10] = laStock[10] + nStkVal    
    laStock[10] = laStock[10] + ncost * ntotStk
    *B609996,1 MMT 07/11/2012 Rebalance program use ncost * ntotstk instead of nstkval  from styinvjl table[T20120316.0004][END]    
    IF llMultWare
      laLocStock[9]  = laLocStock[1] + laLocStock[2] + laLocStock[3] + laLocStock[4] +;
                       laLocStock[5] + laLocStock[6] + laLocStock[7] + laLocStock[8]
      *B609996,1 MMT 07/11/2012 Rebalance program use ncost * ntotstk instead of nstkval  from styinvjl table[T20120316.0004][Start]                       
      *laLocStock[10] = laLocStock[10] + nStkVal
      laLocStock[10] = laLocStock[10] + ncost * ntotStk
      *B609996,1 MMT 07/11/2012 Rebalance program use ncost * ntotstk instead of nstkval  from styinvjl table[T20120316.0004][END]      
      lcStyWare = IIF(lcInvType = '0001', '', cInvType) + PADR(Style,19) + PADR(cWareCode,6)
    ENDIF  
    
    llExpr = &lcStyAls..cDye_Flg = 'Y'
    
    IF llDyelot AND llExpr AND !EMPTY(cDyelot)
      IF SEEK(Style + cWareCode + cDyelot, lcTmpDyeFl)
        lnAliasNow = SELECT(0)
        SELECT (lcTmpDyeFl)
        REPLACE Stk1 WITH Stk1   + &lcStyJlAls..nStk1 ,;
                Stk2 WITH Stk2   + &lcStyJlAls..nStk2 ,;
                Stk3 WITH Stk3   + &lcStyJlAls..nStk3 ,;
                Stk4 WITH Stk4   + &lcStyJlAls..nStk4 ,;
                Stk5 WITH Stk5   + &lcStyJlAls..nStk5 ,;
                Stk6 WITH Stk6   + &lcStyJlAls..nStk6 ,;
                Stk7 WITH Stk7   + &lcStyJlAls..nStk7 ,;
                Stk8 WITH Stk8   + &lcStyJlAls..nStk8 ,;
              TotStk WITH TotStk + &lcStyJlAls..nTotStk
        SELECT (lnAliasNow)
      ELSE
        STORE 0 TO lnI,m.TotStk
        m.Style     = Style
        m.cWareCode = cWareCode
        m.Dyelot    = cDyelot
        FOR lnI = 1 TO 8
          lcStkDye = "m.Stk"+STR(lnI,1)
          lcStkJl  = "nStk" +STR(lnI,1)
          &lcStkDye = &lcStkJl
          m.TotStk = m.TotStk + &lcStkDye
        ENDFOR
        INSERT INTO (lcTmpDyeFl) FROM MEMVAR
      ENDIF
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *SELECT (loStyInvJL.lcCursorView)
    *llExitLoop = !loStyInvJL.GONEXT()

    *B608487,1 WAM 03/19/2008 Enhance performance of rebalance (commented out)
*!*	    SELECT (lcStyJlAls)
*!*	    llExitLoop = !lfGONEXT(lcStyJlAls)
*!*	    SELECT (lcStyJlAls)
*!*	    *B128464,2 WSH 08/21/2005 [End]
*!*	  ENDDO
  ENDSCAN
  *B608487,1 WAM 03/19/2008 (End)
  
  *-- Update last Style/Location group if system have multiple locations.
  LOCAL llFoundRec
  llFoundRec = .F.
  
  IF llMultWare
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llFoundRec = loStyDye.SEEK(lcStyWare)
    *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
    llFoundRec = lfSEEK(lcSDyAls, lcStyCond, lcStyWare)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF lcVrUpStk = 'U' AND !llFoundRec
      IF lcInvType = '0001'
        
        *B128464,1 WSH 06/20/2005 Get the correct to get the Fabric Code. [Start]
        *=lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,5),&lcStyAls..Ave_Cost)
        =lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,6),&lcStyAls..Ave_Cost)
        *B128464,1 WSH 06/20/2005 [End]
        
      ELSE
        
        *B128464,1 WSH 06/20/2005 Get the correct to get the Fabric Code. [Start]
        *=lfAddRec(SUBSTR(lcStyWare,7,19),SUBSTR(lcStyWare,26,5),&lcStyAls..Ave_Cost)
        =lfAddRec(SUBSTR(lcStyWare,5,19),SUBSTR(lcStyWare,24,6),&lcStyAls..Ave_Cost)
        *B128464,1 WSH 06/20/2005 [End]
        
      ENDIF
    ENDIF
  ELSE  && Else update stydye record if system does not have multiple locations.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llFoundRec = loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
    llFoundRec = lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', 'cInvType = ?m.cInvType AND ') + 'Style = ?m.Style', IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
    *B128464,2 WSH 08/21/2005 [End]
    
    IF lcVrUpStk = 'U' AND !llFoundRec AND !EMPTY(&lcStyAls..cDefWare)
      =lfAddRec(&lcStyAls..STYLE,&lcStyAls..cDefWare,&lcStyAls..Ave_Cost)
    ENDIF
    =ACOPY(laStock,laLocStock)  && copy Stock array to Stock location array avoid zero stock array.
  ENDIF

  SELECT (lcSDyAls)
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *DO lpGathFlds WITH loStyDye, "nStkVal WITH laLocStock[10]," +;
                     "Ave_Cost WITH IIF(TotStk=0,Ave_Cost,ABS(nStkVal/TotStk))"
  
  *loStyle.REPLACE(IIF(loStyle.llNative, "", "Rec_No WITH Rec_No"))
  *SELECT (loStyle.lcCursorUpdate)
  
  *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
  *DO lpGathFlds WITH lcSDyAls, "nStkVal WITH laLocStock[10]," +;
                     "Ave_Cost WITH IIF(TotStk=0,Ave_Cost,ABS(nStkVal/TotStk))"
  DO lpGathFlds WITH lcSDyAls, "nStkVal WITH laLocStock[10]," +;
                     "Ave_Cost WITH IIF(TotStk=0,Ave_Cost,ABS(nStkVal/TotStk))" +;
                     IIF(m.cInvType = '0001','',",navecstbuy WITH IIF(TotStk = 0, Ave_Cost, ABS(laLocStock[10]/TotStk)) * lnConv")
  *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
  
  SELECT (lcStyAls)
  *B128464,2 WSH 08/21/2005 [End]
  
  llMisMatch = .F.
  IF lcInvType = '0001'
    *-- Update Style Record.
    
    IF llViewLog
      llMisMatch = OLDVAL('TotStk') <> laStock[9] OR;
                   OLDVAL('Stk1') <> laStock[1] OR;
                   OLDVAL('Stk2') <> laStock[2] OR;
                   OLDVAL('Stk3') <> laStock[3] OR;
                   OLDVAL('Stk4') <> laStock[4] OR;
                   OLDVAL('Stk5') <> laStock[5] OR;
                   OLDVAL('Stk6') <> laStock[6] OR;
                   OLDVAL('Stk7') <> laStock[7] OR;
                   OLDVAL('Stk8') <> laStock[8]
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGHND
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
    ENDIF

    laUpdatArr[1,2] = 'laStock'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyle, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"
    
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
    *DO lpGathFlds WITH lcStyAls, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"
    IF m.cInvType = '0002'                   
      lnCstBuy = IIF(laStock[9] = 0, &lcStyAls..Ave_Cost, ABS(laStock[10]/laStock[9])) * lnConv                     
    ENDIF   
    DO lpGathFlds WITH lcStyAls, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"+;
                       IIF(m.cInvType = '0001','',",navecstbuy with lnCstBuy")
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
          
    *B128464,2 WSH 08/21/2005 [End]
    
  ELSE
    
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *loStyle.REPLACE("nStkVal WITH laStock[10], Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))")
    IF llViewLog
      llMisMatch = OLDVAL('TotStk') <> laStock[9]
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGHND
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
    ENDIF
    
    laUpdatArr[1,2] = 'laStock'
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *DO lpGathFlds WITH loStyle, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"
    
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
    *DO lpGathFlds WITH lcStyAls, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"
    IF m.cInvType = '0002'                                      
      lnCstBuy = IIF(laStock[9] = 0, &lcStyAls..Ave_Cost, ABS(laStock[10]/laStock[9])) * lnConv                     
    ENDIF   
    DO lpGathFlds WITH lcStyAls, "nStkVal WITH laStock[10] ," +;
                       "Ave_Cost WITH IIF(laStock[9] = 0, Ave_Cost, ABS(nStkVal / laStock[9]))"+;
                       IIF(m.cInvType = '0001','',",navecstbuy with lnCstBuy")
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
                       
    *B128464,2 WSH 08/21/2005 [End]
    
    *E039550,1 WSH 08/07/2005 [End]
    
  ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *IF llViewLog AND !llMisMatch AND OLDVAL('AVE_COST') <> AVE_COST OR OLDVAL('NSTKVAL') <> NSTKVAL
  SELECT (lcStyAls)
  IF !llMisMatch
    llMisMatch = OLDVAL('AVE_COST') <> AVE_COST OR OLDVAL('NSTKVAL') <> NSTKVAL
    IF llViewLog AND llMisMatch
  *B128464,2 WSH 08/21/2005 [End]
  
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      
      *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[Start]
      *laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGHND
      laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_STKVAL_MISMTCH
      *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[End]
      
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  ENDIF
  *B128464,2 WSH 08/21/2005 [End]
  
  IF llDyelot AND TYPE('lcTmpDyeFl') = "C" AND USED(lcTmpDyeFl) AND RECCOUNT(lcTmpDyeFl) > 0
    SELECT (lcTmpDyeFl)
    SCAN
      SCATTER MEMVAR
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+Style+cWareCode+Dyelot)
      *  loStyDye.Append()
      SELECT (lcSDyAls)
      lcWhereCond = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style AND cWareCode = ?m.cWareCode AND Dyelot = ?m.Dyelot"
      IF !lfSEEK(lcSDyAls, lcWhereCond, IIF(lcInvType = '0001', '', m.cInvType)+m.Style+m.cWareCode+m.Dyelot)
        APPEND BLANK
        IF lcInvType # '0001'
          =lfSQLAppend()
        ENDIF
      ELSE
        GATHER MEMVAR
      *B128464,2 WSH 08/21/2005 [End]
      
      ENDIF
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
      *SELECT (loStyDye.lcCursorUpdate)
      *
      *GATHER MEMVAR
      *
      **B128464,1 WSH 06/20/2005 Insert the Record in the SQL database. [Start]
      *IF lcInvType # '0001'
      *  =lfSQLAppend()
      *ENDIF
      **B128464,1 WSH 06/20/2005 [Emd]
      *B128464,2 WSH 08/21/2005 [End]
      
      SELECT (lcTmpDyeFl)
      lcSTYDYE = Style+cWareCode
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcTmpDyeFl..Style+&lcTmpDyeFl..cWareCode+SPACE(10))
      m.cInvType  = lcInvType
      m.Style     = EVALUATE(lcTmpDyeFl + '.Style')
      m.cWareCode = EVALUATE(lcTmpDyeFl + '.cWareCode')
      lcWhereCond = IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style AND cWareCode = ?m.cWareCode AND Dyelot = ''"
      IF !lfSEEK(lcSDyAls, lcWhereCond, IIF(lcInvType = '0001', '', lcInvType)+m.Style+m.cWareCode+SPACE(10))
      *B128464,2 WSH 08/21/2005 [End]
      
        SELECT (lcTmpDyeFl)
        lnRec = RECNO()
        
        SUM STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK ;
         TO lnTSTK1,lnTSTK2,lnTSTK3,lnTSTK4,lnTSTK5,lnTSTK6,lnTSTK7,lnTSTK8,lnTStk ;
         FOR STYLE+CWARECODE=lcSTYDYE
        GOTO lnRec
        
        SELECT (lcAlias)
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *loStyDye.APPEND()
        *SELECT (loStyDye.lcCursorUpdate)
        SELECT (lcSDyAls)
        APPEND BLANK
        *B128464,2 WSH 08/21/2005 [End]
        
        REPLACE Style WITH &lcTmpDyeFl..Style ,;
                cWareCode WITH &lcTmpDyeFl..cWareCode ,;
                STK1 WITH lnTStk1 ,;
                STK2 WITH lnTStk2 ,;
                STK3 WITH lnTStk3 ,;
                STK4 WITH lnTStk4 ,;
                STK5 WITH lnTStk5 ,;
                STK6 WITH lnTStk6 ,;
                STK7 WITH lnTStk7 ,;
                STK8 WITH lnTStk8 ,;
                TOTSTK WITH lnTStk
        
        *B128464,1 WSH 06/20/2005 Replace the Inventory Type field in case of itemloc SQL file. [Start]
        IF lcInvType # '0001'
          REPLACE cInvType WITH lcInvType
          =lfSQLAppend()
        ENDIF
        *B128464,1 WSH 06/20/2005 [End]
        
      ENDIF
    ENDSCAN

    SELECT (lcTmpDyeFl)
    ZAP
  ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *IF llViewLog AND !llMisMatch AND loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType) + &lcStyAls..Style)
  m.cInvType = lcInvType
  m.Style    = EVALUATE(lcStyAls + '.Style')
  IF llViewLog AND !llMisMatch AND lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', 'cInvType = ?m.cInvType AND ') + 'Style = ?m.Style', IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
  *B128464,2 WSH 08/21/2005 [End]
    SELECT (lcSDyAls)
    SCAN REST WHILE &lcDyeWhile
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *loStyDye.REPLACE(IIF(loStyDye.llNative, "", "Rec_No WITH Rec_No"))
      *SELECT (loStyDye.lcCursorUpdate)
      *B128464,2 WSH 08/21/2005 [End]
      
      llMisMatch = OLDVAL('Stk1') <> Stk1 OR;
                   OLDVAL('Stk2') <> Stk2 OR;
                   OLDVAL('Stk3') <> Stk3 OR;
                   OLDVAL('Stk4') <> Stk4 OR;
                   OLDVAL('Stk5') <> Stk5 OR;
                   OLDVAL('Stk6') <> Stk6 OR;
                   OLDVAL('Stk7') <> Stk7 OR;
                   OLDVAL('Stk8') <> Stk8 OR;
                   OLDVAL('TotStk')   <> TotStk  OR;
                   OLDVAL('nStkVal')  <> nStkVal OR;
                   OLDVAL('Ave_Cost') <> Ave_Cost
      IF llMisMatch
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        
        *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[Start]
        IF OLDVAL('Stk1') <> Stk1 OR;
                   OLDVAL('Stk2') <> Stk2 OR;
                   OLDVAL('Stk3') <> Stk3 OR;
                   OLDVAL('Stk4') <> Stk4 OR;
                   OLDVAL('Stk5') <> Stk5 OR;
                   OLDVAL('Stk6') <> Stk6 OR;
                   OLDVAL('Stk7') <> Stk7 OR;
                   OLDVAL('Stk8') <> Stk8 OR;
                   OLDVAL('TotStk')   <> TotStk 
        *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[End]
        
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_WRONGHND
        
        *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[Start]
        ELSE
          laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID + ": " + Style + LANG_SMREBAL_STKVAL_MISMTCH
        ENDIF 
        *! B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[End]
        
        laRebMsg[3] = " "
        =lfVryRport()
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN
*-- end of lfStyStock.

*!*************************************************************
*! Name      : lfCrtTmpDy
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Create temporary file used to update dyelot records.
*!*************************************************************
*! Example   : = lfCrtTmpDy()
*!*************************************************************
FUNCTION lfCrtTmpDy

PRIVATE lcAliasNow
lcAliasNow = SELECT(0)

IF USED(lcTmpDyeFl)
  USE IN (lcTmpDyeFl)
ENDIF

CREATE CURSOR (lcTmpDyeFl) (Style C(19) , cWareCode C(6) , Dyelot C(10) ,;
                            Stk1 N(7,0),Stk2 N(7,0),Stk3 N(7,0),Stk4 N(7,0),;
                            Stk5 N(7,0),Stk6 N(7,0),Stk7 N(7,0),Stk8 N(7,0),TotStk N(9,0))
ZAP
INDEX ON Style+cWareCode+Dyelot TAG (lcTmpDyeFl) OF (oAriaApplication.WorkDir+lcTmpDyeFl+'.CDX') 

SELECT (lcAliasNow)
*-- end of lfCrtTmpDy.

*!*************************************************************
*! Name      : lfPrnt
*! Developer : Ramy Mabrouk
*! Date      : 
*! Purpose   : Print the rebalance log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = oAriaApplication.WorkDir + gfTempName() + '.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*-------------------- Not in this Release ------------------------*
*                                                                 *
*                                                                 *
*!*  *!*************************************************************
*!*  *! Name      : lfUpBalance
*!*  *! Developer : Adel Mohammed El Gazzar (ADEL)
*!*  *! Date      : 29/01/2001
*!*  *! Purpose   : Update temp header file with selected elemets
*!*  *!           : to be rebalanced.
*!*  *!*************************************************************
*!*  *! Passed Parameters      : None.
*!*  *!*************************************************************
*!*  *! Returns                : ....
*!*  *!*************************************************************
*!*  *! Example   : = lfUpBalance()
*!*  *!*************************************************************
*!*  FUNCTION lfUpBalance

*!*  PRIVATE lnSelect
*!*  lnSelect = SELECT()
*!*  SELECT (lcRebalHdr)

*!*  *--Initialize all option with 'I'gnore.
*!*  REPLACE ALL cUpdVryIgn WITH 'I'

*!*  FOR lnCnt = 1 TO 99
*!*    lnNoOfIt = IIF(lnCnt<10,1,2)
*!*    IF TYPE('LCRPITEM'+STR(lnCnt,lnNoOfIt)) <> 'C'
*!*      EXIT
*!*    ENDIF
*!*    IF EVAL('LCRPITEM'+STR(lnCnt,lnNoOfIt)) <> 'I' AND ;
*!*       SEEK('LCRPITEM'+STR(lnCnt,lnNoOfIt))
*!*      REPLACE cUpdVryIgn WITH EVAL('LCRPITEM'+STR(lnCnt,lnNoOfIt))
*!*    ENDIF  
*!*  ENDFOR
*!*  lcRpCmpExp = lcCurrComp_ID 
*!*  SELECT (lnSelect)
*!*  RETURN '.T.'

*!*  *!***************************************************************************
*!*  *! Name      : lfvHisYear
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Validation for Fiscal year ArCusHst case.
*!*  *!***************************************************************************
*!*  *! Example   : = lfvHisYear()
*!*  *!***************************************************************************
*!*  FUNCTION lfvHisYear

*!*  DECLARE laRpRetFld(1)
*!*  lcOldBrFld    = lcBrFields
*!*  lcBrFields    = 'cFisFYear:H="Year",cFisYStat:H="Status",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
*!*  laRpRetFld[1] = ''
*!*  lcFicsYr = EVALUATE(SYS(18))
*!*  lcRpCurFld = VARREAD()
*!*  lcOldAlias = SELECT()    && Save the current alias
*!*  llUesdBefo = .F.        && Check if used before or this the first time

*!*  IF !USED("FISHD") 
*!*    SELECT 0
*!*    USE (lcFilePath+'FISHD') ORDER TAG compfyear
*!*    llUesdBefo = .T.
*!*  ENDIF
*!*  SELECT FISHD
*!*   
*!*  IF EMPTY(lcFicsYr) OR '?' $ &lcRpCurFld. OR !SEEK(lcFicsYr)  
*!*    =gfBrows( " FOR .T. ",'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
*!*    &lcRpCurFld. = laRpRetFld[1]
*!*    lcFicsYr   = laRpRetFld[1]
*!*    SHOW GET (lcRpCurFld)
*!*  ENDIF

*!*  lcFiscYear = lcFicsYr   

*!*  IF llUesdBefo       && Close file if it was not used before.
*!*    USE IN FISHD
*!*  ENDIF

*!*  SELECT (lcOldAlias)
*!*  RETURN 
*!*  *-- End of lfvHisYear.

*!*  *!***************************************************************************
*!*  *! Name      : lfStpCsHst
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Setup function for ArCusHst.
*!*  *!***************************************************************************
*!*  *! Example   : = lfStpCsHst()
*!*  *!***************************************************************************
*!*  FUNCTION lfStpCsHst

*!*  PRIVATE lcCurrArea , laFields

*!*  IF &lcRebalHdr..cUpdVryIgn='U' AND !llUpdt
*!*    RETURN
*!*  ENDIF

*!*  lcCurrArea = SELECT(0)

*!*  IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.DBF') 
*!*    USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0 EXCL
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    SET ORDER TO TAG (&lcRebalHdr..cTempName)
*!*  ELSE
*!*    lcTag = &lcRebalHdr..cTempName
*!*    SELECT ArCusHst 
*!*    =AFIELDS(laFields)
*!*    CREATE TABLE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) FROM ARRAY laFields  
*!*  ENDIF

*!*  IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')               
*!*    SET ORDER TO TAG ALLTRIM(&lcRebalHdr..cTempName)
*!*  ELSE
*!*    INDEX ON Account+cFisFYear TAG &lcTag OF (oAriaApplication.WorkDir +ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')
*!*  ENDIF

*!*  SELECT (lcCurrArea)
*!*  *-- End of lfStpCsHst.

*!*  *!***************************************************************************
*!*  *! Name      : lfVryCsHst
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Verify function for ArCusHst.
*!*  *!***************************************************************************
*!*  *! Example   : = lfVryCsHst()
*!*  *!***************************************************************************
*!*  FUNCTION lfVryCsHst

*!*  PRIVATE lcUntSin,lcExRSin,lcGlYear,lcGlPeriod,lnCOGSAmt,lnShipAmnt,lnDiscAmnt,;
*!*          lcFilter,lcDateExpr,lcExpr
*!*  STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
*!*  STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
*!*  DECLARE laRebMsg[3]

*!*  *-- Zap Temp ArCusHst file first.
*!*  SELECT ALLTRIM(&lcRebalHdr..cTempName)

*!*  DO CASE 
*!*    CASE EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND EMPTY(SUBSTR(laFXFLT[5,6],7,5))
*!*      lcFilter = '.T.'
*!*    CASE EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND !EMPTY(SUBSTR(laFXFLT[5,6],7,5))
*!*      lcFilter = 'Account <= SUBSTR(laFXFLT[5,6],7,5)'
*!*    CASE !EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND EMPTY(SUBSTR(laFXFLT[5,6],7,5))
*!*      lcFilter = 'Account >= SUBSTR(laFXFLT[5,6],1,5)'
*!*    CASE !EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND !EMPTY(SUBSTR(laFXFLT[5,6],7,5))
*!*      lcFilter = 'Account >= SUBSTR(laFXFLT[5,6],1,5) AND Account <= SUBSTR(laFXFLT[5,6],7,5)'
*!*  ENDCASE

*!*  *-- 1) check InvHdr and InvLine files.
*!*  lcLow = SUBSTR(laFXFLT[6,6],1,4)
*!*  lcHigh = SUBSTR(laFXFLT[6,6],6,4)

*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(dPostDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(dPostDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(dPostDate),4) >= lcLow AND STR(YEAR(dPostDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + Invoice')

*!*  SELECT InvHdr
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND Status <> 'V'
*!*      WAIT WINDOW 'Fix Customer History File from Invoice # ' + Invoice NOWAIT
*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
*!*      STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
*!*      
*!*      =CHECKPRD(dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Invoice # " + InvHdr.Invoice + " has Transaction date " + DTOC(InvHdr.dPostDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        lnShipAmnt = ShipAmt &lcExRSin nExRate &lcUntSin nCurrUnit
*!*        lnDiscAmnt = ABS(Discount) &lcExRSin nExRate &lcUntSin nCurrUnit
*!*      
*!*        IF SEEK(Invoice,'InvLine')
*!*          SELECT InvLine
*!*          SCAN REST WHILE Invoice + STR(LineNo,6) = InvHdr.Invoice
*!*            lnCOGSAmt = lnCOGSAmt + (TotQty * Cost)
*!*          ENDSCAN
*!*        ENDIF
*!*        
*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(InvHdr.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH InvHdr.Account , cFisFYear WITH lcGlYear
*!*        ENDIF        
*!*        REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + InvHdr.Ship ,;
*!*                nSlsQty             WITH nSlsQty             + InvHdr.Ship ,;
*!*                nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
*!*                nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
*!*                nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
*!*                nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
*!*                nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + lnCOGSAmt ,;
*!*                nCOGSAmt            WITH nCOGSAmt            + lnCOGSAmt
*!*      ENDIF          
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 2) check OrdHdr file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(Entered),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(Entered),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(Entered),4) >= lcLow AND STR(YEAR(Entered),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + cOrdType + Order')

*!*  SELECT OrdHdr
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND !(Status $ 'BX') AND !Direct_Inv
*!*      WAIT WINDOW 'Fix Customer History File from Order # ' + Order NOWAIT

*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(Entered,'lcGlYear','lcGlPeriod','',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order # " + OrdHdr.Order + " has Transaction date " + DTOC(OrdHdr.Entered) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(OrdHdr.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH OrdHdr.Account , cFisFYear WITH lcGlYear
*!*        ENDIF
*!*        REPLACE  nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + OrdHdr.Book , ;
*!*                 nOrdQty WITH nOrdQty + OrdHdr.Book , ;
*!*                 nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + OrdHdr.BookAmt , ;
*!*                 nOrdAmt WITH nOrdAmt + OrdHdr.BookAmt
*!*      ENDIF           
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 3) check RetHdr file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(CrDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(CrDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(CrDate),4) >= lcLow AND STR(YEAR(CrDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + CrMemo')

*!*  SELECT RetHdr
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr
*!*      WAIT WINDOW 'Fix Customer History File from Return # ' + CrMemo NOWAIT

*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(CrDate,'lcGlYear','lcGlPeriod','RM',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Return # " + RetHdr.CrMemo + " has Transaction date " + DTOC(RetHdr.CrDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(RetHdr.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH RetHdr.Account , cFisFYear WITH lcGlYear
*!*        ENDIF        
*!*        REPLACE nRetAmt            WITH nRetAmt + ROUND(RetHdr.Gross_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetQty            WITH nRetQty + RetHdr.Pieces ,;
*!*                nDisAmt            WITH nDisAmt - ROUND(RetHdr.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetAmt&lcGlPeriod WITH nRetAmt&lcGlPeriod + ROUND(RetHdr.Gross_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetQty&lcGlPeriod WITH nRetQty&lcGlPeriod + RetHdr.Pieces ,;
*!*                nDisAmt&lcGlPeriod WITH nDisAmt&lcGlPeriod - ROUND(RetHdr.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2)
*!*      ENDIF          
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 4) check Debit file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + cInstalNo + DTOS(TranDate)')

*!*  SELECT Debit
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND TranType <> '1'
*!*      WAIT WINDOW 'Fix Customer History File from Debit Transaction # ' + Tran NOWAIT
*!*    
*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','',.T.)

*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Debit Tran # " + Debit.Tran + " has Transaction date " + DTOC(Debit.TranDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)

*!*        IF Debit.TranType = '3' AND !EMPTY(Debit.Chgbk_Date)
*!*          LOOP
*!*        ENDIF

*!*        IF !SEEK(Debit.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH Debit.Account , cFisFYear WITH lcGlYear
*!*        ENDIF
*!*        REPLACE nDrAdj            WITH nDrAdj + ROUND(Debit.Amount &lcExRSin Debit.nExRate &lcUntSin Debit.nCurrUnit,2) ,;
*!*                nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(Debit.Amount &lcExRSin Debit.nExRate &lcUntSin Debit.nCurrUnit,2)
*!*      ENDIF          
*!*    ENDIF
*!*      
*!*  ENDSCAN

*!*  *-- 5) check Credit file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + DTOS(TranDate)')

*!*  SELECT Credit
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND TranType <> '0'
*!*      WAIT WINDOW 'Fix Customer History File from Credit Transaction # ' + Tran NOWAIT
*!*    
*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','CR',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Credit Tran # " + Credit.Tran + " has Transaction date " + DTOC(Credit.TranDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF Credit.TranType = '6' AND !EMPTY(Credit.Credt_Date)
*!*          LOOP
*!*        ENDIF

*!*        IF !SEEK(Credit.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH Credit.Account , cFisFYear WITH lcGlYear
*!*        ENDIF

*!*        IF Credit.TranType = '4'    
*!*          REPLACE nPayment            WITH nPayment + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2)) ,;
*!*                  nPayment&lcGLPeriod WITH nPayment&lcGLPeriod + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2))
*!*        ELSE
*!*          REPLACE nCrAdj            WITH nCrAdj + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2)) ,;
*!*                  nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2))
*!*        ENDIF
*!*      ENDIF  
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 6) check ArHist file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + cInstalNo + DTOS(TranDate)')

*!*  SELECT ArHist
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND !(TranType $ '89')
*!*      WAIT WINDOW 'Fix Customer History File from History Transaction # ' + Tran NOWAIT

*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": History Tran # " + ArHist.Tran + " has Transaction date " + DTOC(Arhist.TranDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF (ArHist.TranType = '3' AND !EMPTY(ArHist.ChgBk_Date)) OR (ArHist.TranType = '6' AND !EMPTY(ArHist.Credt_Date))
*!*          LOOP
*!*        ENDIF
*!*        IF !SEEK(ArHist.Account+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Account WITH ArHist.Account , cFisFYear WITH lcGlYear
*!*        ENDIF
*!*      
*!*        DO CASE
*!*          CASE ArHist.TranType $ '23'
*!*            REPLACE nDrAdj            WITH nDrAdj + ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2) ,;
*!*                    nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)
*!*          CASE ArHist.TranType = '4'
*!*            REPLACE nPayment            WITH nPayment + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
*!*                    nPayment&lcGLPeriod WITH nPayment&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
*!*          CASE ArHist.TranType $ '56'
*!*            REPLACE nCrAdj            WITH nCrAdj + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
*!*                    nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
*!*          CASE ArHist.TranType = '7'
*!*            REPLACE nAllow            WITH nAllow + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
*!*                    nAllow&lcGLPeriod WITH nAllow&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
*!*        ENDCASE
*!*      ENDIF  
*!*    ENDIF
*!*  ENDSCAN
*!*  WAIT CLEAR

*!*  IF !llUpdt
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    PRIVATE lnLoop , lcLoop
*!*    DECLARE laRebMsg[3]
*!*    SCAN
*!*      WAIT WINDOW 'Reporting differences in Customer History file for Account : ' + Account +", Year : " + cFisFYear NOWAIT
*!*      IF SEEK(Account+cFisFYear,'ArCusHst')
*!*        FOR lnLoop = 1 TO 13
*!*          lcLoop = PADL(ALLTRIM(STR(lnLoop,2)),2,'0')        
*!*          laRebMsg[1] = " "
*!*        
*!*          *-- Checking Order Amount.
*!*          IF nOrdAmt&lcLoop <> ArCusHst.nOrdAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF
*!*        
*!*          *-- Checking Order Qty.
*!*          IF nOrdQty&lcLoop <> ArCusHst.nOrdQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Sales Amount.
*!*          IF nSlsAmt&lcLoop <> ArCusHst.nSlsAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Sales Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Sales Qty.
*!*          IF nSlsQty&lcLoop <> ArCusHst.nSlsQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Sales Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Discount Amount.
*!*          IF nDisAmt&lcLoop <> ArCusHst.nDisAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Discount Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Cost of goods sold Amount.
*!*          IF nCogsAmt&lcLoop <> ArCusHst.nCogsAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Cost of goods sold Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Return Amount.
*!*          IF nRetAmt&lcLoop <> ArCusHst.nRetAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Returned Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Return Amount.
*!*          IF nRetQty&lcLoop <> ArCusHst.nRetQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Returned Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Allowance.
*!*          IF nAllow&lcLoop <> ArCusHst.nAllow&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Allowance Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Payment.
*!*          IF nPayment&lcLoop <> ArCusHst.nPayment&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Payment Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Debit Adjustments.
*!*          IF nDrAdj&lcLoop <> ArCusHst.nDrAdj&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Debit Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Credit Adjustments.
*!*          IF nCrAdj&lcLoop <> ArCusHst.nCrAdj&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Credit Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF
*!*        ENDFOR
*!*      ELSE
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": has no record for Account code " + Account + ", Fiscal year " + cFisFYear + " in Customer History file."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()        
*!*      ENDIF
*!*    ENDSCAN
*!*  ENDIF

*!*  SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*  USE
*!*  *-- End of lfVryCsHst.

*!*  *!***************************************************************************
*!*  *! Name      : lfUpdCsHst
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Update function for ArCusHst.
*!*  *!***************************************************************************
*!*  *! Example   : = lfUpdCsHst()
*!*  *!***************************************************************************
*!*  FUNCTION lfUpdCsHst
*!*  PRIVATE lcFile

*!*  IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.DBF')
*!*    IF !USED(ALLTRIM(&lcRebalHdr..cTempName))
*!*      USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0 
*!*    ENDIF
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    lcFile = ALLTRIM(&lcRebalHdr..cTempName)
*!*    SCAN
*!*      SCATTER MEMVAR MEMO
*!*      SELECT ArCusHst
*!*      IF !SEEK(&lcFile..Account+&lcFile..cFisFYear,'ArCusHst')
*!*        APPEND BLANK
*!*      ENDIF
*!*      m.cAdd_User = ArCusHst.cAdd_User
*!*      m.dAdd_Date = ArCusHst.dAdd_Date
*!*      m.cAdd_Time = ArCusHst.cAdd_Time
*!*      m.cEdit_User = oAriaApplication.User_ID
*!*      m.dEdit_Date = DATE()
*!*      m.cEdit_Time = gfGetTime()
*!*      =RLOCK()
*!*      GATHER MEMVAR MEMO
*!*      UNLOCK      
*!*    ENDSCAN
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    USE
*!*    ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.DBF')
*!*    ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')  
*!*  ELSE
*!*    llUpdt = .T.
*!*    =lfStpCsHst()
*!*    =lfVryCsHst()
*!*    =lfUpdCsHst()
*!*    llUpdt = .F.
*!*  ENDIF
*!*  *-- End of lfUpdCsHst.

*---------------- Not in this Release ----------------------*
*!*  *!***************************************************************************
*!*  *! Name      : lfStpStHst
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Setup function for IcStyHst.
*!*  *!***************************************************************************
*!*  *! Example   : = lfStpStHst()
*!*  *!***************************************************************************
*!*  FUNCTION lfStpStHst

*!*  PRIVATE lcCurrArea , laFields

*!*  IF &lcRebalHdr..cUpdVryIgn='U' AND !llUpdt
*!*    RETURN
*!*  ENDIF
*!*  lcCurrArea = SELECT(0)
*!*  IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.DBF') 
*!*    USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0 EXCL
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    SET ORDER TO TAG (&lcRebalHdr..cTempName)
*!*  ELSE
*!*    lcTag = &lcRebalHdr..cTempName
*!*    SELECT IcStyHst
*!*    =AFIELDS(laFields)
*!*    CREATE TABLE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) FROM ARRAY laFields  
*!*  ENDIF

*!*  IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')               
*!*    SET ORDER TO TAG ALLTRIM(&lcRebalHdr..cTempName)
*!*  ELSE
*!*    INDEX ON Style+cFisFYear TAG &lcTag OF (oAriaApplication.WorkDir +ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')
*!*  ENDIF
*!*  SELECT (lcCurrArea)
*!*  *-- End of lfStpStHst.

*!*  *!***************************************************************************
*!*  *! Name      : lfVryStHst
*!*  *! Developer : Sameh Saiid Ezzat (SSE)
*!*  *! Date      : 03/31/2002
*!*  *! Purpose   : Verify function for IcStyHst.
*!*  *!***************************************************************************
*!*  *! Example   : = lfVryStHst()
*!*  *!***************************************************************************
*!*  FUNCTION lfVryStHst

*!*  PRIVATE lcUntSin,lcExRSin,lcGlYear,lcGlPeriod,lnCOGSAmt,lnShipAmnt,lnDiscAmnt,;
*!*          lcFilter,lcDateExpr,lcExpr,lcStyTitle
*!*  STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
*!*  STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
*!*  lcStyTitle = gfItemMask('HI')
*!*  DECLARE laRebMsg[3]

*!*  *-- Zap Temp IcStyHst file first.
*!*  SELECT ALLTRIM(&lcRebalHdr..cTempName)

*!*  DO CASE 
*!*    CASE EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND EMPTY(SUBSTR(laFXFLT[7,6],21,19))
*!*      lcFilter = '.T.'
*!*    CASE EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND !EMPTY(SUBSTR(laFXFLT[7,6],21,19))
*!*      lcFilter = 'Style <= SUBSTR(laFXFLT[7,6],21,19)'
*!*    CASE !EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND EMPTY(SUBSTR(laFXFLT[7,6],21,19))
*!*      lcFilter = 'Style >= SUBSTR(laFXFLT[7,6],1,19)'
*!*    CASE !EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND !EMPTY(SUBSTR(laFXFLT[7,6],21,19))
*!*      lcFilter = 'Style >= SUBSTR(laFXFLT[7,6],1,19) AND Style <= SUBSTR(laFXFLT[7,6],21,19)'
*!*  ENDCASE

*!*  *-- 1) check InvHdr and InvLine files.
*!*  lcLow = SUBSTR(laFXFLT[6,6],1,4)
*!*  lcHigh = SUBSTR(laFXFLT[6,6],6,4)

*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) >= lcLow AND STR(YEAR(InvHdr.dPostDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Style','Style + Invoice + STR(LineNo,6)')

*!*  SELECT InvLine
*!*  SET RELATION TO Invoice INTO InvHdr ADDITIVE
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND InvHdr.Status <> 'V'
*!*      WAIT WINDOW 'Fix Style History File from Invoice # ' + Invoice NOWAIT
*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
*!*      STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
*!*      
*!*      =CHECKPRD(InvHdr.dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Invoice # " + Invoice + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(InvHdr.dPostDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,InvHdr.cCurrCode)

*!*        lnShipAmnt = TotQty * Price &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
*!*        lnDiscAmnt = TotQty * Price * InvHdr.DiscPcnt/100 &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
*!*              
*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(InvLine.Style+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Style WITH InvLine.Style , cFisFYear WITH lcGlYear
*!*        ENDIF        
*!*          REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + InvLine.TotQty   ,;
*!*                  nSlsQty             WITH nSlsQty             + InvLine.TotQty   ,;
*!*                  nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
*!*                  nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
*!*                  nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
*!*                  nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
*!*                  nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + InvLine.TotQty*InvLine.Cost  ,;
*!*                  nCOGSAmt            WITH nCOGSAmt            + InvLine.TotQty*InvLine.Cost  
*!*      ENDIF          
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 2) check OrdHdr file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) >= lcLow AND STR(YEAR(OrdHdr.Entered),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Style','Style + DTOS(Complete) + cOrdType + Order + Store + STR(Lineno,6)')

*!*  SELECT OrdLine
*!*  SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr AND !(OrdHdr.Status $ 'BX') AND !OrdHdr.Direct_Inv
*!*      WAIT WINDOW 'Fix Style History File from Order # ' + Order NOWAIT

*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod','',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order # " + Order + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(OrdHdr.Entered) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,OrdHdr.cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(OrdLine.Style+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Style WITH OrdLine.Style , cFisFYear WITH lcGlYear
*!*        ENDIF
*!*        REPLACE  nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + OrdLine.TotBook , ;
*!*                 nOrdQty WITH nOrdQty + OrdLine.TotBook , ;
*!*                 nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + OrdLine.TotBook*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit, ;
*!*                 nOrdAmt WITH nOrdAmt + OrdLine.TotBook*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
*!*      ENDIF           
*!*    ENDIF
*!*  ENDSCAN

*!*  *-- 3) check RetHdr file.
*!*  DO CASE 
*!*    CASE EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = '.T.'
*!*    CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) <= lcHigh'
*!*    CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) >= lcLow'
*!*    CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
*!*      lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) >= lcLow AND STR(YEAR(RetHdr.CrDate),4) <= lcHigh'
*!*  ENDCASE

*!*  *-- To make rushmore expression.
*!*  lcExpr = STRTRAN(lcFilter,'Style','Style + CrMemo')

*!*  SELECT RetLine
*!*  SET RELATION TO CrMemo INTO RetHdr ADDITIVE
*!*  SCAN FOR &lcExpr
*!*    IF &lcDateExpr
*!*      WAIT WINDOW 'Fix Style History File from Return # ' + CrMemo NOWAIT

*!*      STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
*!*      =CHECKPRD(RetHdr.CrDate,'lcGlYear','lcGlPeriod','RM',.T.)
*!*      IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
*!*        laRebMsg[1] = " "
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": Return # " + CrMemo + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(RetHdr.CrDate) + " which does not fall within any period."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()
*!*      ELSE
*!*        lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
*!*        lcUntSin = ''  
*!*        lcExRSin = gfGetExSin(@lcUntSin,RetHdr.cCurrCode)

*!*        SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*        IF !SEEK(RetLine.Style+lcGlYear)
*!*          APPEND BLANK
*!*          REPLACE Style WITH RetLine.Style , cFisFYear WITH lcGlYear
*!*        ENDIF
*!*        REPLACE nRetAmt            WITH nRetAmt + ROUND(RetLine.Amount &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetQty            WITH nRetQty + RetLine.TotQty ,;
*!*                nDisAmt            WITH nDisAmt - ROUND(RetLine.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetAmt&lcGlPeriod WITH nRetAmt&lcGlPeriod + ROUND(RetLine.Amount &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
*!*                nRetQty&lcGlPeriod WITH nRetQty&lcGlPeriod + RetLine.TotQty ,;
*!*                nDisAmt&lcGlPeriod WITH nDisAmt&lcGlPeriod - ROUND(RetLine.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2)
*!*      ENDIF          
*!*    ENDIF
*!*  ENDSCAN

*!*  IF !llUpdt
*!*    SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*    PRIVATE lnLoop , lcLoop
*!*    DECLARE laRebMsg[3]
*!*    SCAN
*!*      WAIT WINDOW 'Reporting differences in Style History file for ' + lcStyTitle + " : " + Style +", Year : " + cFisFYear NOWAIT
*!*      IF SEEK(Style+cFisFYear,'IcStyHst')
*!*        FOR lnLoop = 1 TO 13
*!*          lcLoop = PADL(ALLTRIM(STR(lnLoop,2)),2,'0')        
*!*          laRebMsg[1] = " "
*!*        
*!*          *-- Checking Order Amount.
*!*          IF nOrdAmt&lcLoop <> IcStyHst.nOrdAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF
*!*        
*!*          *-- Checking Order Qty.
*!*          IF nOrdQty&lcLoop <> IcStyHst.nOrdQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Order Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Sales Amount.
*!*          IF nSlsAmt&lcLoop <> IcStyHst.nSlsAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Sales Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Sales Qty.
*!*          IF nSlsQty&lcLoop <> IcStyHst.nSlsQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Sales Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Discount Amount.
*!*          IF nDisAmt&lcLoop <> IcStyHst.nDisAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Discount Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Cost of goods sold Amount.
*!*          IF nCogsAmt&lcLoop <> IcStyHst.nCogsAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Cost of goods sold Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Return Amount.
*!*          IF nRetAmt&lcLoop <> IcStyHst.nRetAmt&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Returned Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF

*!*          *-- Checking Return Amount.
*!*          IF nRetQty&lcLoop <> IcStyHst.nRetQty&lcLoop
*!*            laRebMsg[2] = "Company" + lcCurrComp_ID + ": Returned Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
*!*            laRebMsg[3] = " "
*!*            =lfVryRport()
*!*          ENDIF
*!*        ENDFOR
*!*      ELSE
*!*        laRebMsg[2] = "Company" + lcCurrComp_ID + ": has no record for " + lcStyTitle + " : " + Style + ", Fiscal year " + cFisFYear + " in Customer History file."
*!*        laRebMsg[3] = " "
*!*        =lfVryRport()        
*!*      ENDIF
*!*    ENDSCAN
*!*  ENDIF

*!*  SELECT ALLTRIM(&lcRebalHdr..cTempName)
*!*  USE
*!*  *-- End of lfVryStHst.

*!***************************************************************************
*! Name      : lfUpdStHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Update function for IcStyHst.
*!***************************************************************************
*! Example   : = lfUpdStHst()
*!***************************************************************************
FUNCTION lfUpdStHst
PRIVATE lcFile

IF FILE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName) + '.DBF')
  IF !USED(ALLTRIM(&lcRebalHdr..cTempName))
    USE (oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)) IN 0 
  ENDIF
  SELECT ALLTRIM(&lcRebalHdr..cTempName)
  lcFile = ALLTRIM(&lcRebalHdr..cTempName)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT IcStyHst
    *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]    
    *IF !SEEK(&lcFile..Style+&lcFile..cFisFYear,'IcStyHst')
    IF !gfSEEK(&lcFile..Style+&lcFile..cFisFYear)
    *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
      APPEND BLANK
    ENDIF
    m.cAdd_User = IcStyHst.cAdd_User
    m.dAdd_Date = IcStyHst.dAdd_Date
    m.cAdd_Time = IcStyHst.cAdd_Time
    m.cEdit_User = oAriaApplication.User_ID
    m.dEdit_Date = DATE()
    m.cEdit_Time = gfGetTime()
    =RLOCK()
    GATHER MEMVAR MEMO
    UNLOCK      
    *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    =gfReplace('')
    *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
  ENDSCAN
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  SELECT icstyhst 
  =gfTableUpdate()
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
  
  SELECT ALLTRIM(&lcRebalHdr..cTempName)
  USE
  ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.DBF')
  ERASE(oAriaApplication.WorkDir + ALLTRIM(&lcRebalHdr..cTempName)+'.CDX')  
ELSE
  llUpdt = .T.
  =lfStpStHst()
  =lfVryStHst()
  =lfUpdStHst()
  llUpdt = .F.
ENDIF
*-- End of lfUpdStHst.


*!*************************************************************
*! Name      : lfAddRec
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Add missed records to the StyDye file.
*!*************************************************************
*! Example   : = lfAddRec()
*!*************************************************************
FUNCTION lfAddRec
LPARAMETER lcSty, lcWare, lnCst

LOCAL lcCurAlias, lcReplStat
lcCurAlias = SELECT(0)

*B128464,1 WSH 06/20/2005 Add cInvType Filed to the insert statement. [Start]
*lcReplStat = "REPLACE STYLE     WITH '" + lcSty  + "'," +;
                     "cWareCode WITH '" + lcWare + "'," +;
                     "AVE_COST  WITH  " + STR(lnCst)
lcReplStat = "REPLACE " + IIF(lcInvType # '0001', "cInvType WITH '" + lcInvType + "',", "") +;
                     "STYLE     WITH '" + lcSty  + "'," +;
                     "cWareCode WITH '" + lcWare + "'," +;
                     "AVE_COST  WITH  " + STR(lnCst)
*B128464,1 WSH 06/20/2005 [End]

*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
IF lcInvType # '0001'
  lcReplStat = lcReplStat + ",navecstbuy with "+STR(lnCst*lnConv)
ENDIF 
*: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]



*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*loStyDye.Append(lcReplStat)
SELECT (lcSDyAls)
APPEND BLANK
&lcReplStat.
*B128464,2 WSH 08/21/2005 [End]

*B128464,1 WSH 06/20/2005 Insert the Record in the SQL database. [Start]
IF lcInvType # '0001'
  =lfSQLAppend()
ENDIF
*B128464,1 WSH 06/20/2005 [Emd]

SELECT (lcCurAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfGetVarVl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Get the default values of OG variables.
*!*************************************************************
*! Example   : = lfGetVarVl()
*!*************************************************************
FUNCTION lfGetVarVl

LOCAL lnDataSess, lnI, lcGetValue
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

FOR lnI = 1 TO ALEN(laOGObjType,1)
  IF UPPER(laOGObjType[lnI,1]) = 'LCRPITEM' .AND. SEEK(UPPER(laOGObjType[lnI,1]), lcRebalHdr)
    lcGetValue = EVALUATE(laOGObjType[lnI,1])
    REPLACE &lcRebalHdr..cUpdVryIgn WITH lcGetValue
  ENDIF

  IF UPPER(laOGObjType[lnI,1]) = 'LCLOGFILE' .AND. SEEK(UPPER(laOGObjType[lnI,1]), lcRebalHdr)
    lcGetValue = EVALUATE(laOGObjType[lnI,1])
    REPLACE &lcRebalHdr..cLogFile WITH lcGetValue
  ENDIF
ENDFOR

SET DATASESSION TO (lnDataSess)

*!*************************************************************
*! Name      : lfOpenRemote
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Open Remote Cursors needed by the Option Grid
*!*************************************************************
FUNCTION lfOpenRemote

LOCAL lnAlias
lnAlias = SELECT(0)

STORE .NULL. TO loVendor, loWareHous, loStyle_X, loCustomer

*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
IF 'AP' $ oAriaApplication.CompanyInstalledModules OR 'PO' $ oAriaApplication.CompanyInstalledModules OR ;
   'MA' $ oAriaApplication.CompanyInstalledModules OR 'MF' $ oAriaApplication.CompanyInstalledModules
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}
*--Create Vendor table Object...
  loVendor = CREATEOBJECT('RemoteTable', 'APVENDOR', 'VENCODE', lcVendor, SET("Datasession"))
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{Start}
ENDIF  
*! B609592,1 MMT 05/31/2011 Error while opening reblanace if AP module is not in company installed modules{End}

*--Create WareHous table Object...
loWareHous = CREATEOBJECT('RemoteTable', 'WAREHOUS', 'WAREHOUS', lcWareHous, SET("Datasession"))

*--Create Style table Object...

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*loStyle_X = CREATEOBJECT('RemoteTable', 'STYLE', 'STYLE', lcStyle_X, SET("Datasession"))
loStyle_X = CREATEOBJECT('RemoteTable', 'STYLE', 'CSTYLE', lcStyle_X, SET("Datasession"))
*B130510,1 WSH 12/05/2005 [End]

*--Create Customer table Object...
loCustomer = CREATEOBJECT('RemoteTable', 'CUSTOMER', 'CUSTOMER', lcCustomer, SET("Datasession"))

IF FILE(oAriaApplication.WorkDir + lcSelInvNo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelInvNo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
IF FILE(oAriaApplication.WorkDir + lcPackSelect + '.DBF')
  USE (oAriaApplication.WorkDir + lcPackSelect + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]


IF FILE(oAriaApplication.WorkDir + lcSelOrdNo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelOrdNo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelPONo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelPONo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelAccount + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelAccount + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelStyle + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelStyle + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelYear + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelYear + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCreateCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/02/2005
*! Purpose   : Build SQL Cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfCreateCursor
LPARAMETERS lnFiltType, lcFiltExp, lcCursName, lcStruct, lcRetFld, lcIndex

LOCAL lcTmpCurs, lcSQLRet, llFound, lnRet
llFound = .F.
lnRet   = 0  && Filter not found

lcTmpCurs = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcTmpCurs)
  SELECT (lcTmpCurs)
  llFound = (RECCOUNT() > 0)
  IF llFound
    IF !FILE(oAriaApplication.WorkDir + lcCursName + '.DBF')
      CREATE TABLE (oAriaApplication.WorkDir + lcCursName + '.DBF') ( &lcStruct. )
      INDEX ON &lcIndex TAG (lcCursName)
      USE IN (lcCursName)
    ENDIF

    USE (oAriaApplication.WorkDir + lcCursName) EXCLUSIVE IN 0 ALIAS (lcCursName)
    SELECT (lcCursName)
    ZAP

    SELECT (lcTmpCurs)
    SCAN
      SCATTER MEMVAR MEMO
      SELECT (lcCursName)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN

    USE IN (lcCursName)
  ENDIF
ENDIF

RETURN lnRet

*!*************************************************************
*! Name      : lfCreatePOCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/29/2005
*! Purpose   : Build SQL Cursor for PO
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! B129821,1 
*!*************************************************************
FUNCTION lfCreatePOCursor
LPARAMETERS lnFiltType, lcFiltExp, lcCursName

LOCAL lcTmpCurs, lcSQLRet, llFound, lnRet
llFound = .F.
lnRet   = 0  && Filter not found

lcTmpCurs = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcTmpCurs)
  SELECT (lcTmpCurs)
  llFound = (RECCOUNT() > 0)
  IF llFound
    IF !FILE(oAriaApplication.WorkDir + lcCursName + '.DBF')
      CREATE TABLE (oAriaApplication.WorkDir + lcCursName + '.DBF') (cBusDocu C(1), cStyType C(1), PO C(6))
      INDEX ON cBusDocu+cStyType+PO TAG (lcCursName)
      USE IN (lcCursName)
    ENDIF

    USE (oAriaApplication.WorkDir + lcCursName) EXCLUSIVE IN 0 ALIAS (lcCursName)
    SELECT (lcCursName)
    ZAP

    SELECT (lcTmpCurs)
    SCAN
      SCATTER MEMO TO laTmp
      SELECT (lcCursName)
      APPEND BLANK
      REPLACE cBusDocu WITH SUBSTR(laTmp[1], 1, 1),;
              cStyType WITH SUBSTR(laTmp[1], 2, 1),;
              PO       WITH SUBSTR(laTmp[1], 3)
    ENDSCAN
    USE IN (lcCursName)
  ENDIF
ENDIF

RETURN lnRet

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS

DO CASE
  CASE lnArrayType = 1              && Fixed Filter
    lnPOS = ASCAN(loOgScroll.laOGFxFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2             && Hidden Filter
    lnPOS = ASCAN(loOgScroll.laOGHDFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3           && Variable Filter
    lnPOS = ASCAN(loOgScroll.laOGvrFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE :
    lcReturn = ""
ENDCASE

RETURN lcReturn

*************************************************************
*! Name      : lfInitThermo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Initialize Progress Bar...
*!*************************************************************
FUNCTION lfInitThermo
LPARAMETERS lnTotalThermo, lcFirstCaption

*E303410,1 TMI 09/01/2013 [Start] if called from style screen do not run thermometer
*! B610535,1 MMT 09/29/2013 Rabalance program gives error becuase of its progress bar[T20130819.0010][Start]
*IF EMPTY(lcStyMaj)
IF !EMPTY(lcStyMaj)
*! B610535,1 MMT 09/29/2013 Rabalance program gives error becuase of its progress bar[T20130819.0010][End]
  RETURN 
ENDIF 
*E303410,1 TMI 09/01/2013 [End  ] 

IF lnTotalThermo = 0
  RETURN
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 

  oProgress.TotalProgress = lnTotalThermo
  oProgress.lblFirstLabel.Caption  = lcFirstCaption
  oProgress.lblSecondLabel.Caption = ''
  oProgress.CurrentProgress(0)
    oProgress.SHOW()
    
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF
  *E303410,1 TMI 09/01/2013 [End  ] 
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ELSE
  loProgress.Percent     = 0
  loProgress.Description = lcFirstCaption
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

*************************************************************
*! Name      : lfRecCount
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Get Current Company Connection Strings...
*!*************************************************************
FUNCTION lfRecCount

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS loObject
*
**B128464,1 WSH 06/12/2005 Enhance Style Rebalance Performance. [Start]
*IF loObject.llNative
*  RETURN RECCOUNT(loObject.lcCursorView)
*ENDIF
**B128464,1 WSH 06/12/2005 [End]
*
*LOCAL lnRetVal, lcTempCurs, lcTable, lcConStr, lcStat, lnResult
*lcConStr   = loObject.lcConnStr
*lcTable    = ALLTRIM(loObject.lcTablename)
*lcStat     = "SELECT COUNT(*) AS nCount FROM " + lcTable
*lnRetVal   = 1
*lcTempCurs = gfTempName()

*lnResult   = oAriaApplication.RemoteSystemData.Execute(lcStat, "", lcTempCurs, "", lcConStr, 3 , "", SET("Datasession"))
*IF lnResult >= 1
LPARAMETERS lcCursor

LOCAL lnAlias, lnRetVal, lnFilePos, llNative, lcTableName, llResult, lcSqlStat, lcTempCurs
lnAlias    = SELECT(0)
lnFilePos  = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcCursor)), 1)
llNative   = laFileStr[lnFilePos,6]
lnRetVal   = 1
lcTempCurs = gfTempName()

IF llNative
  RETURN RECCOUNT(lcCursor)
ELSE
  lcTableName = ALLTRIM(laFileStr[lnFilePos,1])
  lcSqlStat   = "SELECT COUNT(*) AS nCount FROM " + lcTableName
  llResult    = lfSQLRun(lcSqlStat, lcTempCurs)
ENDIF

IF llResult
*B128464,2 WSH 08/21/2005 [End]

  SELECT (lcTempCurs)
  LOCATE

  lnRetVal = nCount
  USE IN (lcTempCurs)
ENDIF

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
SELECT (lnAlias)
*B128464,2 WSH 08/21/2005 [End]

RETURN lnRetVal

*************************************************************
*! Name      : lfMasterUpd
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Update or Revert a List of Table Objects...
*!*************************************************************
FUNCTION lfMasterUpd

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*LPARAMETERS lcObjList, llUpdate
LPARAMETERS lcCursList, llUpdate

LOCAL lnAlias, llNative, lcTableName, lcTagName, lcTagFlds
lnAlias = SELECT(0)
*B128464,2 WSH 08/21/2005 [End]

LOCAL lnI, laOjects[1], lcTranCode, lnConnHandler, llExitLopp

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*IF llUpdate
*  *-- Begin Updating Transaction
*  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')
*  
*  *-- Check Resule for Begin Transaction
*  IF TYPE('lcTranCode') = 'N'
*    RETURN .F.
*  ENDIF
*ENDIF
*
*-- Update All passed Objects.
*= gfSubStr(lcObjList, @laOjects, ',')
= gfSubStr(lcCursList, @laOjects, ',')
*B128464,2 WSH 08/21/2005 [End]

FOR lnI = 1 TO ALEN(laOjects, 1)
  llExitLopp = .F.
  DO WHILE !llExitLopp
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    lnFilePos   = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, EVALUATE(ALLTRIM(laOjects[lnI]))), 1)
    lcTableName = ALLTRIM(laFileStr[lnFilePos,1])

    SELECT (EVALUATE(laOjects[lnI]))
    *B128464,2 WSH 08/21/2005 [End]
    
    IF llUpdate
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExitLopp = &laOjects[lnI]..TABLEUPDATE(lcTranCode)
      lcTagName = ALLTRIM(laFileStr[lnFilePos,2])
      llNative  = laFileStr[lnFilePos,6]
      
      IF llNative
        llExitLopp = TABLEUPDATE(.T.)
      ELSE
        SCATTER MEMVAR MEMO
        
        llExitLopp = lfSqlUpdate(EVALUATE(laOjects[lnI]), laFileStr[lnFilePos,8])
        llExitLopp = llExitLopp AND TABLEUPDATE(.T.)
      ENDIF
      *B128464,2 WSH 08/21/2005 [End]
      
    ELSE
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExitLopp = &laOjects[lnI]..TABLEREVERT()
      = TABLEREVERT(.T.)
      llExitLopp = .T.
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExitLopp = llExitLopp OR MESSAGEBOX(LANG_SMREBAL_UPDERR + '"' + &laOjects[lnI]..lcTableName + '"', 5 + 32, _Screen.Caption) = 2
    llExitLopp = llExitLopp OR MESSAGEBOX(LANG_SMREBAL_UPDERR + '"' + lcTableName + '"', 5 + 32, _Screen.Caption) = 2
    *B128464,2 WSH 08/21/2005 [End]
  ENDDO
ENDFOR

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*IF llUpdate
*  *-- Commit Changes and Check Result
*  lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
*  IF lnConnHandler <> 1
*    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*    RETURN .F.
*  ENDIF
*ENDIF
SELECT (lnAlias)
*B128464,2 WSH 08/21/2005 [End]

RETURN .T.

*************************************************************
*! Name      : lfZeroDye
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Update Stydye records with zeros
*!*************************************************************
FUNCTION lfZeroDye

LOCAL lnAlias
lnAlias = SELECT(0)

*-- Update stydye record if system does not have multiware and dyelots.
IF llStyDyeFl
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *IF loStyDye.SEEK(IIF(lcInvType = '0001', '', lcInvType)+&lcStyAls..STYLE)
  *  SELECT (loStyDye.lcCursorView)
  m.cInvType = lcInvType
  m.Style    = EVALUATE(lcStyAls + '.STYLE')
  IF lfSEEK(lcSDyAls, IIF(lcInvType = '0001', '', "cInvType = ?m.cInvType AND ") + "Style = ?m.Style", IIF(lcInvType = '0001', '', m.cInvType) + m.Style)
    SELECT (lcSDyAls)
  *B128464,2 WSH 08/21/2005 [End]
  
    SCAN REST WHILE &lcDyeWhile
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *DO lpGathFlds WITH loStyDye && Update stydye record with zeros.
      DO lpGathFlds WITH lcSDyAls && Update stydye record with zeros.
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDSCAN
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN

*B128464,1 WSH 06/20/2005 Insert the Record in the SQL database. [Start]
*!*************************************************************
*! Name      : lfSQLAppend
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Add missed records to the ItemLoc file in SQL Database.
*!*************************************************************
*! Example   : = lfSQLAppend()
*!*************************************************************
FUNCTION lfSQLAppend

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
****   Note: Don't need to Use Native SQL Commands for inserting records in ItemLoc
****         as this function is called only in special cases not frequently ....
*B128464,2 WSH 08/21/2005 [End]

LOCAL lcStat, lnResult, lcTmpCurs, lcTranCode, lnConnHandler, lcCurAlias
*E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
*lcStat     = "SELECT TOP 0 * FROM ITEMLOC (INDEX = STYDYE)"
lcStat     = "SELECT TOP 0 * FROM ITEMLOC WITH(INDEX(STYDYE))"
*E302821,1 TMI 12/27/2010 [End  ] 
lcTmpCurs  = gfTempName()
lcCurAlias = SELECT(0)

lnResult = oAriaApplication.RemoteCompanyData.SqlRun(lcStat, lcTmpCurs, "ITEMLOC", oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
IF lnResult < 1
  oAriaApplication.RemoteCompanyData.CheckRetResult("Execute", lnResult, .T.)
  RETURN .F.
ENDIF

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*SELECT (loStyDye.lcCursorUpdate)
SELECT (lcSDyAls)
*B128464,2 WSH 08/21/2005 [End]

SCATTER MEMVAR MEMO
=TABLEUPDATE(.F.)

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*SELECT (loStyDye.lcCursorView)
*APPEND BLANK
*GATHER MEMVAR MEMO
*=TABLEUPDATE(.F.)
*B128464,2 WSH 08/21/2005 [End]

SELECT (lcTmpCurs)
APPEND BLANK
GATHER MEMVAR MEMO

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  SELECT (lcCurAlias)
  RETURN .F.
ENDIF

lnConnHandler = oAriaApplication.RemoteCompanyData.SQLUpdate(lcTmpCurs, lcTranCode, SET("Datasession"), "cInvType,Style,cWareCode,Dyelot", "ITEMLOC", "STYDYE")

IF lnConnHandler <> 1 AND lnConnHandler <> 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUpdate", lnConnHandler, .T.)
  lnConnHandler = oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT (lcCurAlias)
  RETURN .F.
ENDIF

lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  SELECT (lcCurAlias)
  Return .F.
ENDIF

SELECT (lcCurAlias)
RETURN .T.
*B128464,1 WSH 06/20/2005 [End]

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*!*************************************************************
*! Name      : lfCreateObject
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Open a remote cursor for the passed table alias
*!*************************************************************
*! Example   : = lfCreateObject()
*!*************************************************************
FUNCTION lfCreateObject
LPARAMETERS lcTableName, lcTagName, lcCursorName, lcIndexFields, lcIndexExp

LOCAL llNative, lnResult, lcAlias, lnRetRecs, lcStatement, lcTmpCurs, lnI
lcAlias      = ALIAS()
llNative     = .F.

*E303030,1 BEGIN
*lcTableName  = PADR(ALLTRIM(UPPER(lcTablename)), 8)
*lcTagName    = IIF(TYPE('lcTagName') = 'C', PADR(ALLTRIM(UPPER(lcTagName)), 10), '')
lcTableName  = PADR(ALLTRIM(UPPER(lcTablename)), oAriaApplication.FileW)
lcTagName    = IIF(TYPE('lcTagName') = 'C', PADR(ALLTRIM(UPPER(lcTagName)), oAriaApplication.IndexW), '')
*E303030,1 END

lnResult = oAriaApplication.RemoteSystemData.Execute("SELECT cFile_Tag, cIndx_Exp FROM SydIndex WHERE CFILE_NAM + CFILE_TAG = '" + lcTableName + lcTagName + "'",;
           "", "SysTmpCursor", "", oAriaApplication.cAria4SysFiles, 3 , "", SET("Datasession"))
IF lnResult < 1
  oAriaApplication.RemoteSystemData.CheckRetResult("Execute", lnResult, .T.)
  SELECT (lcAlias)
  RETURN
ENDIF

SELECT SysTmpCursor
LOCATE
IF !EOF()
  *E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
  *lcStatement = "SELECT TOP 0 * FROM " + ALLTRIM(lcTableName) +;
                IIF(!EMPTY(lcTagName), "(INDEX = " + ALLTRIM(lcTagName) + ")", "")
  lcStatement = "SELECT TOP 0 * FROM " + ALLTRIM(lcTableName) +;
                IIF(!EMPTY(lcTagName), " WITH(INDEX(" + ALLTRIM(lcTagName) + "))", "")
  *E302821,1 TMI 12/27/2010 [End  ]               
  
  lnResult = SQLEXEC(lnCompConnHand, lcStatement, lcCursorName)
  
  IF lnResult < 1
    USE IN SysTmpCursor
    SELECT (lcAlias)
    RETURN
  ENDIF
  
  lcIndexExp = UPPER(SysTmpCursor.cIndx_Exp)
  *lcIndexExp = STRTRAN(lcIndexExp, 'DTOS(', 'TTOC(')
  
  LOCAL ARRAY laFields(1)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') = 'C'
    =oAriaApplication.RemoteCompanyData.IndexFields(lcIndexExp, @laFields)
  ELSE
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    =oAriaApplication.RemoteCompanyData.mIndexFields(lcIndexExp, @laFields)
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  lcIndexFields = ''
  FOR lnI = 1 TO ALEN(laFields, 1)
    lcIndexFields = lcIndexFields + laFields[lnI] + ','
  ENDFOR
  lcIndexFields = LEFT(lcIndexFields, LEN(lcIndexFields) - 1)
  
  SELECT (lcCursorName)
  CURSORSETPROP("Buffering", 3, lcCursorName)
  INDEX ON &lcIndexExp. TAG &lcTagName.
  CURSORSETPROP("Buffering", 5, lcCursorName)
ELSE
  LOCAL llRetVal
  llRetVal = gfOpenFile(lcFilePath + lcTableName, lcTagName, 'SH', lcCursorName, .T.)
  IF llRetVal
    CURSORSETPROP("Buffering", 5, lcCursorName)
  ELSE
    USE IN SysTmpCursor
    SELECT (lcAlias)
    RETURN
  ENDIF
  
  llNative = .T.
ENDIF

USE IN SysTmpCursor
SELECT (lcAlias)

RETURN llNative
*PADR

*!*************************************************************
*! Name      : lfCrtGoNext
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Create SQL Statements for the Go Next record...
*!*************************************************************
*! Example   : = lfCrtGoNext()
*!*************************************************************
FUNCTION lfCrtGoNext
LPARAMETERS lcTableName, lcTagName, lcCursor, lcIndexFields, llNative

LOCAL lnI, lnJ, lnFields, lcSqlStat
LOCAL ARRAY laFields[1]

lcIndexFields = lcIndexFields + ',Rec_No'
= gfSubStr(lcIndexFields, @laFields, ',')

lnFields  = ALEN(laFields, 1)
*E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
*lcSqlStat = 'SELECT TOP 1 * FROM ' + lcTableName + ' (INDEX = ' + lcTagName + ') WHERE '
lcSqlStat = 'SELECT TOP 1 * FROM ' + lcTableName + ' WITH(INDEX(' + lcTagName + ')) WHERE '
*E302821,1 TMI 12/27/2010 [End  ] 

*-- Create the GoNext Statement
FOR lnI = 1 TO lnFields
  lcSqlStat = lcSqlStat + '('
  FOR lnJ = 1 TO lnI - 1
    lcSqlStat = lcSqlStat + '[' + laFields[lnJ] + ']' + ' = ?m.' + laFields[lnJ] + ' AND '
  ENDFOR
  lcSqlStat = lcSqlStat + '[' + laFields[lnI] + ']' + ' > ?m.' + laFields[lnI] + ')' + IIF(lnI = lnFields, '', ' OR ')
ENDFOR

RETURN lcSqlStat

*:*************************************************************
*: Name      : lfCrtUpdStat
*: Developer : Wael M. Abo-Shawareb (WSH)
*: Date      : 08/22/2005
*: Purpose   : Create the Update Sql statment for each table.
*:*************************************************************
FUNCTION lfCrtUpdStat
LPARAMETERS lcFile_Name, lcIndx_Flds, lcTagName

LOCAL lnAlias, lnConnectionHandler, lcSqlUpdate
lnAlias = SELECT(0)

= SQLEXEC(lnCompConnHand, "DROP FUNCTION " + ALLTRIM(LOWER(lcFile_Name)) + "fun")

lnConnectionHandler = SQLEXEC(lnCompConnHand,"select syscolumns.name,syscolumns.usertype,syscolumns.prec,syscolumns.scale"+;
                                         " from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+;
                                         "' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)

IF lnConnectionHandler < 0
  lnConnectionHandlar = SQLDISCONNECT(lnCompConnHand)
  lnCompConnHand      = SQLSTRINGCONNECT(lcSQLConStr)
  lnConnectionHandler = SQLEXEC(lnCompConnHand,"select syscolumns.name,syscolumns.usertype,syscolumns.prec,syscolumns.scale"+;
                                           " from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+;
                                           "' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)
  IF lnConnectionHandlar < 0
    RETURN ''
  ENDIF
ENDIF
SELECT ("COL" + lcFile_Name)

LOCAL lcSqlCommand,lcWhereCond,llFirstField,lnI
LOCAL ARRAY laFields[1]

STORE "" TO lcSqlCommand,lcWhereCond,lcParameters,lcVarType,lcParaVal

=gfSubStr(lcIndx_Flds, @laFields, ',')

lcSqlCommand = "CREATE FUNCTION " + ALLTRIM(LOWER(lcFile_Name)) + "fun ("
llFirstField = .T.
FOR lnI = 1 TO ALEN(laFields)
  LOCATE FOR Name = PADR(LOWER(laFields[lnI]),128)
  IF FOUND()
    DO CASE
      CASE usertype = 1
        lcVarType = "char(" + ALLTRIM(STR(prec)) + ")"
      CASE usertype = 10
        lcVarType = "numeric(" + ALLTRIM(STR(prec)) + "," + ALLTRIM(STR(scale)) + ")"
      CASE usertype = 12
        lcVarType = "datetime"
      CASE usertype = 16
        lcVarType = "bit"
      CASE usertype = 19
        lcVarType = "text"
    ENDCASE
    lcParameters = lcParameters+IIF(llFirstField,"",",")+"@"+LOWER(laFields[lnI])+"var " + lcVarType
    lcParaVal    = lcParaVal+IIF(llFirstField,"",",")+"?m."+LOWER(laFields[lnI])
    lcWhereCond  = lcWhereCond+IIF(llFirstField,""," and ")+"["+LOWER(laFields[lnI])+"] = @"+LOWER(laFields[lnI])+"var"
    llFirstField = .F.
  ENDIF
ENDFOR
*E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
*lcSqlCommand = lcSqlCommand + lcParameters + ") RETURNS TABLE AS RETURN (SELECT * FROM " + " ["+LOWER(lcFile_Name) +;
                                             "] (index=" + lcTagName + ") WHERE " + lcWhereCond + ")"
lcSqlCommand = lcSqlCommand + lcParameters + ") RETURNS TABLE AS RETURN (SELECT * FROM " + " ["+LOWER(lcFile_Name) +;
                                             "] WITH(index(" + lcTagName + ")) WHERE " + lcWhereCond + ")"
*E302821,1 TMI 12/27/2010 [End  ] 
lnConnectionHandler = SQLEXEC(lnCompConnHand, lcSqlCommand)

IF lnConnectionHandler < 0
  lnConnectionHandlar = SQLDISCONNECT(lnCompConnHand)
  lnCompConnHand      = SQLSTRINGCONNECT(lcSQLConStr)
  lnConnectionHandler = SQLEXEC(lnCompConnHand, lcSqlCommand)
  IF lnConnectionHandlar < 0
    RETURN ''
  ENDIF
ENDIF

lcSqlUpdate = "update "+ALLTRIM(LOWER(lcFile_Name))+"fun("+lcParaVal+") set "
LOCAL lcFields
STORE "" TO lcFields
SELECT ("COL"+lcFile_Name)
llFirstField = .T.
SCAN
  IF UPPER(ALLTRIM(name)) == 'REC_NO'
    LOOP
  ENDIF
  
  lcFields = lcFields + IIF(llFirstField,"",",") + "[" + ALLTRIM(name) + "]=?m." + ALLTRIM(name)
  llFirstField = .F.
ENDSCAN
lcSqlUpdate = lcSqlUpdate + lcFields

USE IN ("COL" + lcFile_Name)

SELECT (lnAlias)
RETURN lcSqlUpdate

*!*************************************************************
*! Name      : lfGoTop
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Navigate to the first record in a SQL table
*!*************************************************************
*! Example   : = lfGoTop()
*!*************************************************************
FUNCTION lfGoTop
LPARAMETERS lcCursor

LOCAL lnAlias, lnFilePos, llNative, lcTableName, lcTagName, lcSqlStat, lcIdxExp
lnAlias   = SELECT(0)
lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcCursor)), 1)
llNative  = laFileStr[lnFilePos,6]

IF llNative
  SELECT (lcCursor)
  LOCATE
  SELECT (lnAlias)
  RETURN !EOF(lcCursor)
ELSE
  lcTableName = ALLTRIM(laFileStr[lnFilePos,1])
  lcTagName   = ALLTRIM(laFileStr[lnFilePos,2])
  lcIdxExp    = ALLTRIM(laFileStr[lnFilePos,9])
  *E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
  *lcSqlStat   = 'SELECT TOP 1 * FROM ' + lcTableName + ' (INDEX = ' + lcTagName + ')'
  lcSqlStat   = 'SELECT TOP 1 * FROM ' + lcTableName + ' WITH(INDEX(' + lcTagName + '))'
  *E302821,1 TMI 12/27/2010 [End  ] 
  
  IF !lfSQLRun(lcSqlStat, lcCursor)
    RETURN .F.
  ENDIF
  
  RETURN !EOF(lcCursor)
ENDIF

*!*************************************************************
*! Name      : lfGoNext
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Navigate to the next record in a SQL table
*!*************************************************************
*! Example   : = lfGoNext()
*!*************************************************************
FUNCTION lfGoNext
LPARAMETERS lcCursor

LOCAL lnAlias, lnFilePos, llNative, lcSqlStat
lnAlias   = SELECT(0)
lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcCursor)), 1)
llNative  = laFileStr[lnFilePos,6]

IF llNative
  SKIP IN (lcCursor)
  RETURN !EOF(lcCursor)
ELSE
  IF EOF(lcCursor)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  SELECT (lcCursor)
  SKIP
  IF EOF()
    SKIP -1
    SCATTER MEMVAR
    
    lcSqlStat = laFileStr[lnFilePos,7]
    IF !lfSQLRun(lcSqlStat, lcCursor)
      SELECT (lnAlias)
      RETURN .F.
	*B607944,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file[Start]
  ELSE
  	lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcCursor)), 1)
	  lcFileNam= laFileStr[lnFilePos,1]
  	lcTagNam = laFileStr[lnFilePos,2]

	*E303030,1 BEGIN
	*lcFileNam = PADR(ALLTRIM(UPPER(lcFileNam)), 8)
  	*lcTagNam     = IIF(TYPE('lcTagNam ') = 'C', PADR(ALLTRIM(UPPER(lcTagNam)), 10), '')
	lcFileNam = PADR(ALLTRIM(UPPER(lcFileNam)), oAriaApplication.FileW)
  	lcTagNam     = IIF(TYPE('lcTagNam ') = 'C', PADR(ALLTRIM(UPPER(lcTagNam)), oAriaApplication.IndexW), '')
	*E303030,1 END

	  lnResult = oAriaApplication.RemoteSystemData.Execute("SELECT cFile_Tag, cIndx_Exp FROM SydIndex WHERE CFILE_NAM + CFILE_TAG = '" + lcFileNam + lcTagNam  + "'",;
	           "", "SysTmpCursor", "", oAriaApplication.cAria4SysFiles, 3 , "", SET("Datasession"))
  	IF lnResult < 1
	    oAriaApplication.RemoteSystemData.CheckRetResult("Execute", lnResult, .T.)
	    SELECT (lcAlias)
  	  RETURN
	  ENDIF

  	SELECT SysTmpCursor
	  LOCATE
  	IF !EOF()
	   
	    lcIndexExp = UPPER(SysTmpCursor.cIndx_Exp)
	  
  	  LOCAL ARRAY laFields(1)
  	  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
	  IF TYPE('lcXMLFileName') = 'C'
            =oAriaApplication.RemoteCompanyData.IndexFields(lcIndexExp, @laFields)
	  ELSE
	  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
	    =oAriaApplication.RemoteCompanyData.mIndexFields(lcIndexExp, @laFields)
          *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
      ENDIF
	  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
	    lcIndexFields = ''
  	  FOR lnI = 1 TO ALEN(laFields, 1)
	      lcIndexFields = lcIndexFields + laFields[lnI] + ','
	    ENDFOR
  	  lcIndexFields = LEFT(lcIndexFields, LEN(lcIndexFields) - 1)

	    SELECT (lcCursor)
	    CURSORSETPROP("Buffering", 3, lcCursor)
  	  INDEX ON &lcIndexExp. TAG &lcTagNam.
	    =CURSORSETPROP("Buffering", 5)
  	ENDIF 
	*B607944,1 11/15/2006 MMT bug of not considering all lines in itemjrnl file[End]
      
    ENDIF
  ENDIF
  
  SELECT (lnAlias)
  RETURN !EOF(lcCursor)
ENDIF
*PADR

*!*************************************************************
*! Name      : lfSeek
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Seek in Fox or SQL Table
*!*************************************************************
*! Example   : = lfSeek()
*!*************************************************************
FUNCTION lfSeek
LPARAMETERS lcCursor, lcWhereCond, lcValue, llNoCheckExist

LOCAL lnAlias, lnFilePos, llNative, lcTableName, lcTagName, lcSqlStat, lcIndexFlds
lnFilePos = ASUBSCRIPT(laFileStr, ASCAN(laFileStr, ALLTRIM(lcCursor)), 1)
llNative  = laFileStr[lnFilePos,6]

IF llNative
  RETURN SEEK(lcValue, lcCursor)
ELSE
  IF !llNoCheckExist AND SEEK(lcValue, lcCursor)
    RETURN .T.
  ENDIF
  
  lnAlias     = SELECT(0)
  lcTableName = ALLTRIM(laFileStr[lnFilePos,1])
  lcTagName   = ALLTRIM(laFileStr[lnFilePos,2])
  *B608487,1 WAM 03/19/2008 Enhance performance of rebalance
  *lcSqlStat   = 'SELECT * FROM ' + lcTableName + ' (INDEX = ' + lcTagName + ') WHERE ' + lcWhereCond
  *E302821,1 TMI 12/27/2010 [Start] Change the syntax to meet sqlserver more than 2000 by put WITH(INDEX(<index name>)) instead of [INDEX=<index name>]
  *lcSqlStat   = 'SELECT * FROM ' + lcTableName + ' (INDEX = ' + lcTagName + ') '+IIF(EMPTY(lcWhereCond),'',' WHERE ' + lcWhereCond)
  lcSqlStat   = 'SELECT * FROM ' + lcTableName + ' WITH(INDEX(' + lcTagName + ')) '+IIF(EMPTY(lcWhereCond),'',' WHERE ' + lcWhereCond)
  *E302821,1 TMI 12/27/2010 [End  ] 
  *B608487,1 WAM 03/19/2008 (End)
  
  IF !lfSQLRun(lcSqlStat, "TmpCurs")
    RETURN .F.
  ENDIF
  
  SELECT TmpCurs
  SCAN
    SCATTER MEMVAR
    SELECT (lcCursor)
    APPEND BLANK
    GATHER MEMVAR
    =TABLEUPDATE(.F.)
  ENDSCAN
  
  SELECT (lcCursor)
  IF !llNoCheckExist AND !SEEK(lcValue, lcCursor)
    
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[Start]
    *LOCATE
    *: B608581,1 MMT 06/11/2008 Fix bug of Wrong update of First Material record[End]
    
  ENDIF
  USE IN TmpCurs
  
  SELECT (lnAlias)
  RETURN !EOF(lcCursor)
ENDIF

*!*************************************************************
*! Name      : lfSQLRun
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Runs a SQL Statement
*!*************************************************************
*! Example   : = lfSQLRun()
*!*************************************************************
FUNCTION lfSQLRun
LPARAMETERS lcStat, lcCursor, llNative, lcValue

IF llNative
  RETURN SEEK(lcValue, lcCursor)
ENDIF

LOCAL lnAlias, lnConnHnd
lnAlias   = SELECT(0)
lnConnHnd = SQLEXEC(lnCompConnHand, lcStat, lcCursor)

IF lnConnHnd < 0
  lnConnHnd      = SQLDISCONNECT(lnCompConnHand)
  lnCompConnHand = SQLSTRINGCONNECT(lcSQLConStr)
  lnConnHnd      = SQLEXEC(lnCompConnHand, lcStat, lcCursor)
  
  IF lnConnHnd < 0
    =MESSAGEBOX(LANG_SMREBAL_CONNERR, 16, _Screen.Caption)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

SELECT (lcCursor)
=CURSORSETPROP("Buffering", 5)
LOCATE

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfSQLUpdate
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/18/2005
*! Purpose   : Runs an Update Statement
*!*************************************************************
*! Example   : = lfSQLUpdate()
*!*************************************************************
FUNCTION lfSQLUpdate
LPARAMETERS lcCursor, lcUpdStatement

LOCAL lnAlias, lnCurrRec, lnUpdRec, lnConnHnd
lnAlias   = SELECT(0)
lnCurrRec = IIF(EOF(lcCursor), 0, RECNO(lcCursor))

SELECT (lcCursor)
lnUpdRec = GETNEXTMODIFIED(0)

IF lnUpdRec > 0

  *B607883,1 MMT 12/20/2006 Fix bug of not updating material usage and intransit[Start]
  GOTO (lnUpdRec)
  *B607883,1 MMT 12/20/2006 Fix bug of not updating material usage and intransit[End]

  SCATTER MEMVAR MEMO
  lnConnHnd = SQLEXEC(lnCompConnHand, lcUpdStatement)

  IF lnConnHnd < 0
    lnConnHnd      = SQLDISCONNECT(lnCompConnHand)
    lnCompConnHand = SQLSTRINGCONNECT(lcSQLConStr)
    lnConnHnd      = SQLEXEC(lnCompConnHand, lcUpdStatement)
    
    IF lnConnHnd < 0
      =MESSAGEBOX('Connection level error...', 16, _Screen.Caption)
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

DO WHILE lnUpdRec > 0
  lnUpdRec = GETNEXTMODIFIED(lnUpdRec)
  
  IF lnUpdRec > 0

    *B607883,1 MMT 12/20/2006 Fix bug of not updating material usage and intransit[Start]
    GOTO (lnUpdRec)
    *B607883,1 MMT 12/20/2006 Fix bug of not updating material usage and intransit[End]
    
    SCATTER MEMVAR MEMO
    lnConnHnd = SQLEXEC(lnCompConnHand, lcUpdStatement)

    IF lnConnHnd < 0
      lnConnHnd      = SQLDISCONNECT(lnCompConnHand)
      lnCompConnHand = SQLSTRINGCONNECT(lcSQLConStr)
      lnConnHnd      = SQLEXEC(lnCompConnHand, lcUpdStatement)
      
      IF lnConnHnd < 0
        =MESSAGEBOX(LANG_SMREBAL_CONNERR, 16, _Screen.Caption)
        SELECT (lnAlias)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ENDDO

SELECT (lcCursor)
IF lnCurrRec <> 0
  GOTO (lnCurrRec)
ENDIF

SELECT (lnAlias)
RETURN .T.
*B128464,2 WSH 08/21/2005 [End]

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*!*************************************************************
*! Name      : lfvCtNo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/27/2007
*! Purpose   : Valid function for the C/T number.
*!*************************************************************
*! Example   : = lfvCtNo()
*!*************************************************************

FUNCTION lfvCtNo
LPARAMETERS lcMode

=lfCreatePOCursor(1, 'lcCTN', @lcSelCTNo)
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]


*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*!*************************************************************
*! Name      : lfvPackNo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/17/2008
*! Purpose   : Valid function for the Pack number.
*!*************************************************************
*! Example   : = lfvPackNo()
*!*************************************************************
FUNCTION lfvPackNo
PARAMETERS lcParam

lcTmpCurs = lfCheckFilter(1, "lcPackNo")
IF !EMPTY(lcTmpCurs)
  SELECT (lcTmpCurs)
  llFound = (RECCOUNT() > 0)
  IF llFound
    IF !FILE(oAriaApplication.WorkDir + lcPackSelect + '.DBF')
      CREATE TABLE (oAriaApplication.WorkDir + lcPackSelect + '.DBF') (PACK_NO C(6))
      INDEX ON PACK_NO  TAG (lcPackSelect)
      USE IN (lcPackSelect )
    ENDIF

    USE (oAriaApplication.WorkDir + lcPackSelect) EXCLUSIVE IN 0 ALIAS (lcPackSelect)
    SELECT (lcPackSelect)
    ZAP

    SELECT (lcTmpCurs)
    SCAN
      SCATTER MEMO TO laTmp
      SELECT (lcPackSelect)
      APPEND BLANK
      REPLACE PACK_NO WITH laTmp[1]
    ENDSCAN
    USE IN (lcPackSelect)
  ENDIF
ENDIF
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[Start]
*!*************************************************************
*! Name      : lfVryItem2
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Verify function for AP vendor  (lcRpItem2)
*!*************************************************************
FUNCTION lfVryItem2
PRIVATE laStructur , lnCount
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]
lcInvByDB=gfTempName()
DIMENSION laApdistStru[1,18]
SELECT APDIST
lnApdistCnt =AFIELDS(laApdistStru)
DIMENSION laApdistStru[lnApdistCnt +1,18]
laApdistStru[lnApdistCnt +1,1] = 'cDMno'
laApdistStru[lnApdistCnt +1,2] = 'C'
laApdistStru[lnApdistCnt +1,3] = 12
laApdistStru[lnApdistCnt +1,4] = 0
STORE ' ' TO  laApdistStru[lnApdistCnt +1,7],laApdistStru[lnApdistCnt +1,8],;
  laApdistStru[lnApdistCnt+1,9],laApdistStru[lnApdistCnt +1,10],;
  laApdistStru[lnApdistCnt+1,11],laApdistStru[lnApdistCnt+1,12],;
  laApdistStru[lnApdistCnt+1,13],laApdistStru[lnApdistCnt+1,14],;
  laApdistStru[lnApdistCnt+1,15],laApdistStru[lnApdistCnt+1,16]
STORE 0 TO    laApdistStru[lnApdistCnt +1,17] ,laApdistStru[lnApdistCnt +1,18]
=gfCrtTmp(lcInvByDB,@laApdistStru," cinvno+cvendcode+cDMno+capsessno",lcInvByDB,.T.)
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[End]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
*-- Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE  IN (lcAPRBALV)
SET ORDER TO TAG VENDYEAR IN (lcAPRBALVH)
SET ORDER TO TAG VENCODE  IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV  IN APINVHDR
SET ORDER TO TAG INVVEND  IN APDIST

SELECT APVENDOR

*-- If We are going to recalculate the balances for all Vendors
lnTotal = RECCOUNT()              && These variable is for the thermometer
lnCurrent = 0                     && These variable is for the thermometer

=lfInitThermo(lnTotal, LANG_SMREBAL_RECALC)
*-- SCAN Loop to scan the Vendor file
SCAN
  =lfRbalVend()
  lnCurrent = lnCurrent  + 1
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 
    oProgress.lblSecondLabel.Caption = LANG_SMREBAL_VENDOR  + ALLTRIM(APVENDOR.cVendCode)
    oProgress.CurrentProgress(lnCurrent)
      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF
    *E303410,1 TMI 09/01/2013 [End  ] 
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    IF MOD(lnCurrent ,CEILING(lnTotal/ 10)) = 0
      loProgress.Percent     = lnCurrent / lnTotal
      loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+LANG_SMREBAL_VENDOR  + ALLTRIM(APVENDOR.cVendCode)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
    ENDIF   
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  SELECT APVENDOR
ENDSCAN    && End of SCAN Loop to scan the Vendor file
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 

  *-- If The thermometer is not finished
  IF lnCurrent < lnTotal
    *-- FOR Loop to finish the thermometer
    FOR lnCount = lnCurrent TO lnTotal
      oProgress.CurrentProgress(lnCount)
    ENDFOR    && End of FOR Loop to finish the thermometer
  ENDIF
  oProgress.Visible = .F.
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF 
  *E303410,1 TMI 09/01/2013 [End  ] 
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
*-- End of lfVryItem2()

*!*************************************************************
*! Name      : lfRbalVend
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Function to recalculate the Vendor balances
*!*************************************************************
*! Called from : lfVryItem2()
*!*************************************************************
FUNCTION lfRbalVend

PRIVATE lcPeriod , lcField
*-- Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 

SCATTER MEMVAR MEMO
M.NVENBAL = 0
M.NVENCPUR = 0
M.NVENCPAY = 0
M.NVENOPNDR = 0

SELECT (lcAPRBALV)

*-- If The Vendor code dose not exist in the Vendor rebalance file
IF !SEEK(M.cVendCode)
  APPEND BLANK
ENDIF    && End of IF The Vendor code dose not exist in the Vendor rebalance file
GATHER MEMVAR MEMO

SELECT APVENHST
SCATTER MEMVAR MEMO BLANK
M.cVendCode = APVENDOR.cVendCode

SELECT FISHD
GO TOP

*-- SCAN Loop to scan the Fiscal Calendar file for the current company
SCAN 
  M.cFisFYear = cFisFYear
  SELECT (lcAPRBALVH)
  *If The Vendor code and Fiscal year dose not exist in the Vendor History
  *rebalance file insert it. 
  IF !SEEK(M.cVendCode + M.cFisFYear)
    APPEND BLANK
  ENDIF    && End of IF The Vendor code and Fiscal year dose not exist
  GATHER MEMVAR MEMO
  SELECT FISHD
ENDSCAN    && End of SCAN Loop to scan the Fiscal Calendar file for the current company
*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [Start]
IF "PO" $ oAriaApplication.CompanyInstalledModules OR  "MA" $ oAriaApplication.CompanyInstalledModules
  m.nVenOpnPo = 0
  m.nVenOpnPo = lfVryVenPOOpnAmt(M.cVendCode)
  SELECT (lcAPRBALV)
  IF SEEK(M.cVendCode)
    REPLACE nVenOpnPo WITH m.nVenOpnPo
     IF ApVendor.nVenOpnPo <> m.nVenOpnPo &&AND !llUpdt
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +LANG_SMREBAL_BARACT + ALLTRIM(M.cVendCode) + LANG_SMREBAL_WRONGOPNPOS
        laRebMsg[3] = " "
        IF llAutoBal
          laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
        ENDIF
        =lfVryRport()
      ENDIF    
  ENDIF
ENDIF
*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [END]
SELECT APINVHDR

*-- If Statment to replace the record pointer in the first record for this
*-- Vendor in the Invoice Header file
IF SEEK(M.cVendCode)
  *-- SCAN Loop To scan the Vendor invoices in the Invoice Header file
  *-- with status <> Void
  SCAN REST;
      WHILE cVendCode = M.cVendCode;
        FOR cInvStat <> 'V'
     *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
     *lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt)  
     lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt, .T.)  
     *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][End]
    *--If Not af advance payment, update cumulative purchases
    IF APINVHDR.cInvStat <> 'A'
      SELECT (lcAPRBALV)
      REPLACE nVenCPur WITH nVenCPur + lnInvAmt

      *-- Update Vendor History file for APINVHDR.cFisfYear + APINVHDR.cFspPrdId
      SELECT (lcAPRBALVH)
      
      *-- If Statment to replace the record pointer in the correct record for
      *-- this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        *N000682,1 TMI 22/04/2013 [Start] get the period ID if the field is empty
        *lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        lcPeriod = lfGetPrdID()
        *N000682,1 TMI 22/04/2013 [End  ] 
        REPLACE nVnhPurch        WITH nVnhPurch + lnPurchAmt ,;
                nVnhPur&lcPeriod WITH nVnhPur&lcPeriod + lnPurchAmt 
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
    ELSE       && APINVHDR.cInvStat <> 'A'
      *-- Calculate Open debit 
      *B609578,1 WAM 05/04/2011 Commented out
*!*	      lnInvAmnt = 0
*!*	      lnInvPaid = 0 
*!*	      lnInvDisTk= 0 
*!*	      lnInvAdj  = 0 
      *B609578,1 WAM 05/04/2011 (End)
      lcExSin2   = ''
      lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
      lnInvAmnt = lnInvAmnt + ROUND(APINVHDR.nInvAmnt   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvPaid = lnInvPaid + ROUND(APINVHDR.nInvPaid   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvDisTk= lnInvDisTk+ ROUND(APINVHDR.nInvDisTk  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)      
      lnInvAdj  = lnInvAdj  + ROUND(APINVHDR.nInvAdj    &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)            

      *B609578,1 WAM 05/04/2011 Get advanced payment amount from invoice header
      lnInvAmt = ROUND(APINVHDR.nInvAmnt   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnAmtPaid  = ROUND(APINVHDR.nInvPaid   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnDiscTakn = ROUND(APINVHDR.nInvDisTk  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)      
      lnAdjtAmt  = ROUND(APINVHDR.nInvAdj    &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)            
      *B609578,1 WAM 05/04/2011 (End)

    ENDIF    && End of IF Not an advance payment, update cumulative purchases

    *Update vendor balances if the invoice is not a credit card payment
    *If Not credit card payment

    IF APINVHDR.cVenPMeth <> 'C'       
      *Update Vendor file
      SELECT (lcAPRBALV)
      REPLACE nVenBal WITH nVenBal + lnInvAmt - lnAmtPaid;
                                   - lnDiscTakn - lnAdjtAmt 

      *Update Vendor History file
      SELECT (lcAPRBALVH)
      *If Statment to replace the record pointer in the correct record for
      *this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        REPLACE nVnhDisOf  WITH nVnhDisOf +  lnDiscOffr ,;
                nVnhDisTkn WITH nVnhDisTkn + lnDiscTakn ,;
                nVnhAdj    WITH nVnhAdj + lnAdjtAmt
      ENDIF    && SEEK(M.cVendCode + APINVHDR.cFisfYear)
      
      *Update payments from APDIST (including advance payments)
      SELECT APDIST
      =SEEK(APINVHDR.cInvNo + M.cVendCode)
      SCAN REST;
          WHILE cInvNo + cVendCode = APINVHDR.cInvNo + M.cVendCode ;
            FOR cAPdStat<> 'V'
        *If the record is a payment record (cApdActId = 'C') 
        IF cApdActId = 'C'
          *Update Vendor file
          SELECT (lcAPRBALV)
          REPLACE nVenCPay WITH nVenCPay -  APDIST.nEqvAmnt 
          *Update Vendor History file for APDIST.cFisfYear + APDIST.cFspPrdId
          SELECT (lcAPRBALVH)
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APDIST.cFisfYear)
            lcPeriod = ALLTRIM(STR(VAL(APDIST.cFspPrdId)))
            lcField = IIF(APDIST.cApdTrTyp = 'P' , 'nVnhPChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'M' , 'nVnhMChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'N' , 'nVnhNChkP' ,;
                          'nVnhCashP' )))
            REPLACE nVnhTotPa        WITH nVnhTotPa - APDIST.nEqvAmnt  ,;
                    &lcField         WITH &lcField  - APDIST.nEqvAmnt ,;
                    nVnhPay&lcPeriod WITH nVnhPay&lcPeriod -  APDIST.nEqvAmnt 
                    
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF cApdActId = 'C'
        
        *If the record is an application record (applied bdebit memo)
        IF APDIST.cApdTrTyp = 'A' .AND. cApdActID = 'A' .AND. nApdAmnt < 0
          
          *Update Vendor History file for APDIST.cFisfYear
          SELECT (lcAPRBALVH)
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APDIST.cFisfYear)
            REPLACE nVnhDMApP WITH nVnhDMApP + - APDIST.nEqvAmnt ,;
                    nVnHAdj   WITH nVnHAdj   +  lnAdjtAmt 
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF APDIST.cApdTrTyp = 'A' ,,,
        
        SELECT APDIST
      ENDSCAN    && End of SCAN Loop To scan the Vendor invoices lines in the Dist. file with status <> Void
    ELSE      && IF Credit card payment, update payments
      *Update Vendor file
      SELECT (lcAPRBALV)
      REPLACE nVenCPay WITH nVenCPay + lnInvAmt 
      *Update Vendor History file
      SELECT (lcAPRBALVH)
  
      *If Statment to replace the record pointer in the correct record
      *for this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        *N000682,1 TMI 22/04/2013 [Start] get the period ID if the field is empty
        *lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        lcPeriod = lfGetPrdID()
        *N000682,1 TMI 22/04/2013 [End  ] 
        REPLACE nVnhTotPa        WITH nVnhTotPa + lnInvAmt ,;
                nVnhCCPay        WITH nVnhCCPay + lnInvAmt ,;
                nVnhPay&lcPeriod WITH nVnhPay&lcPeriod + lnInvAmt
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
    ENDIF    && End of IF Not credit card payment
    SELECT APINVHDR
  ENDSCAN    && End of SCAN Loop To scan the Vendor invoices in the Invoice Header file
ENDIF    && End of IF Statment to replace the record pointer in the first record for this Vendor in the Invoice Header file

** Update OpenDebit value in Vendor Master
SELECT (lcAPRBALV)
IF SEEK(M.cVendCode)
  REPLACE nVenOpnDR WITH -lnInvAmnt  + lnInvPaid + ;
                          lnInvDisTk + lnInvAdj  

  IF ApVendor.nVenOpnDr <> nVenOpnDr &&AND !llUpdt
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = LANG_SMREBAL_COMP + lcCurrComp_ID +LANG_SMREBAL_BARACT + ALLTRIM(M.cVendCode) + LANG_SMREBAL_WRONGDEBIT
    laRebMsg[3] = " "

    IF llAutoBal
      laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
    ENDIF

    =lfVryRport()
  ENDIF

  IF ApVendor.nVenBal <> nVenBal &&AND !llUpdt
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = LANG_SMREBAL_COMP  + lcCurrComp_ID +LANG_SMREBAL_BARACT + ALLTRIM(M.cVendCode) + LANG_SMREBAL_CURRENTBAL
    laRebMsg[3] = " "

    IF llAutoBal
      laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
    ENDIF

    =lfVryRport()
  ENDIF
ENDIF  

* Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 

SELECT APINVHDR
*--- End of lfRbalVend()
 *!*************************************************************
*! Name      : lfStupItm2
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Additional AP setup that needs to process AP rebalance
*! Purpose   : of item  (lcRpItem2)
*!*************************************************************
*! Example   : = lfStupItm2()
*!*************************************************************
FUNCTION  lfStupItm2
 
PRIVATE lcCurrArea
lcCurrArea = SELECT(0)

llVerify = .T.
DIMENSION laStructur[1,18]
STORE '' TO laStructur
*If The Vendor rebalance file is not opened
IF USED(lcAPRBALV)
  USE IN (lcAPRBALV)
ENDIF 
 SELECT APVENDOR
=AFIELDS(laStructur)
gfCrtTmp(lcAPRBALV,@laStructur,"cVendCode","VENCODE",.T.)  

DIMENSION laStructur[1,18]
STORE '' TO laStructur

*If The Vendor History rebalance file is not opened
IF USED(lcAPRBALVH)
  USE IN (lcAPRBALVH)
ENDIF 

 SELECT APVENHST
=AFIELDS(laStructur)
gfCrtTmp(lcAPRBALVH,@laStructur,"cVendCode + cFisFYear",'VENDYEAR',.T.)  

IF &lcRebalHdr..cUpdVryIgn='U' 
  llUpdt = .T.
  =lfVryItem2()
ENDIF
SELECT (lcCurrArea)
*-- End of lfStupItm2()

*!*************************************************************
*! Name      : lfValdItm2
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Check whether all mandatory factors that be used in
*!             AP are defined or not. 
*!*************************************************************
*! Example   : =lfValdItm2()
*!*************************************************************
FUNCTION  lfValdItm2
 
RETURN .T.
*--- End of lfValdItm2() 

*!*************************************************************
*! Name      : lfUpdItem2
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Update Fuction for A/P.
*!*************************************************************
*! Example   : =lfUpdItem2()
*!*************************************************************

FUNCTION  lfUpdItem2
PRIVATE llNotUpDat , lcVendor , lnCount
 
*Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE IN (lcAPRBALV)
SET ORDER TO TAG VENDYEAR IN (lcAPRBALVH)
SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV IN APINVHDR
SET ORDER TO TAG INVVEND IN APDIST

*** To get the number of records in calender year table withoud deleted records
SELECT FISHD
COUNT TO lnFisYerNo FOR !DELETED()  &&  Get no. of calender years

llNotUpDat = .F.                             && Flag to know if one or more Vendors was not updated

SELECT (lcAPRBALV)
lnTotal = RECCOUNT() * lnFisYerNo            && These variable is for the Thermometer
lnCurrent = 0                                && These variable is for the Thermometer
=lfInitThermo(lnTotal, LANG_SMREBAL_UPDBAL)

*-- SCAN Loop to scan the Vendor rebalance file
SCAN
*-- If The Vendor time stamp is not changed
  IF SEEK(cVendCode , 'APVENDOR') .AND. ;
    (dAdd_Date = APVENDOR.dAdd_Date .AND. cAdd_Time = APVENDOR.cAdd_Time)

    SELECT APVENHST
    
    *-- SCAN Loop to scan the Vendor records in the Vendor History file
    *! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[Start]
    *SCAN FOR cVendCode + cFisFYear = APVENDOR.cVendCode
    =SEEK(APVENDOR.cVendCode)
    *B610821,1 TMI 08/25/2014 16:36 [Start] replace the loop with BLANK FOR command
    *SCAN REST WHILE cVendCode + cFisFYear = APVENDOR.cVendCode
    **! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[End]
    *  BLANK
    *  DELETE
    *ENDSCAN    && End of SCAN Loop to scan the Vendor records in the Vendor History file
    BLANK FOR cVendCode + cFisFYear = APVENDOR.cVendCode
    *B610821,1 TMI 08/25/2014 16:36 [End  ] 

    SET DELETED OFF

    SELECT FISHD 
    GO TOP
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
lnUpThermo = RECCOUNT()
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    SCAN FOR !DELETED()

      SELECT (lcAPRBALVH)
      *-- If There is a record for this Fiscal Year and Vendor in the
      *-- Vendor History rebalance file
      IF SEEK(APVENDOR.cVendCode + FISHD.cFisFYear)
        SCATTER MEMVAR MEMO
      ELSE    && Else [IF There is no records for this Fiscal Year and Vendor]
        SCATTER MEMVAR MEMO BLANK
        M.cVendCode = APVENDOR.cVendCode
        M.cFisFYear = FISHD.cFisFYear
      ENDIF    && End of IF There is a record for this Fiscal Year and Vendor
      
      SELECT APVENHST
      
      *-- If There is a deleted record in the Vendor History file
      IF SEEK(SPACE(12))
        RECALL
      ELSE    && Else [IF There is no deleted record in the Vendor History file]
        APPEND BLANK
      ENDIF    && End of IF There is a deleted record in the Vendor History file
      
      GATHER MEMVAR MEMO
      =gfAdd_Info('APVENHST')
      
      lnCurrent = lnCurrent  + 1
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
      IF TYPE('lcXMLFileName') <> 'C'
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
        *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
        IF EMPTY(lcStyMaj)
          *E303410,1 TMI 09/01/2013 [End  ] 
        oProgress.lblSecondLabel.Caption = LANG_SMREBAL_VENDOR + ALLTRIM(&lcAPRBALV..cVendCode)
        oProgress.CurrentProgress(lnCurrent)
          
          *E303410,1 TMI 09/01/2013 [Start] 
        ENDIF
        *E303410,1 TMI 09/01/2013 [End  ] 
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
      ELSE
        IF MOD(lnCurrent ,CEILING(lnUpThermo / 10)) = 0
	  loProgress.Percent     = lnCurrent / lnUpThermo
          loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+ LANG_SMREBAL_VENDOR + ALLTRIM(&lcAPRBALV..cVendCode)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
        ENDIF       
      ENDIF
      *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      SELECT FISHD
    ENDSCAN    && End of SCAN Loop to scan the Fiscal calendar for this company
    SET DELETED ON
    SELECT (lcAPRBALV)
    SCATTER MEMVAR MEMO
    SELECT APVENDOR
    GATHER MEMVAR MEMO
    =gfAdd_Info('APVENDOR')
    SELECT (lcAPRBALV)
    DELETE
  ELSE    && Else [IF The Vendor time stamp was changed]
    llNotUpDat = .T.
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = LANG_SMREBAL_COMP    + lcCurrComp_ID +": "+ ALLTRIM(cVendCode) + LANG_SMREBAL_INFORMATION
    laRebMsg[3] = " "
    IF llAutoBal
      laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    =lfVryRport()
    lcVendor = ALLTRIM(APVENDOR.cVendCode)          && Variable to hold the current Vendor code
  ENDIF    && End of IF The Vendor time stamp is not changed
  SELECT (lcAPRBALV)
ENDSCAN    && End of SCAN Loop to scan the Vendor rebalance file

= lfMasterUpd("'APVENHST','APVENDOR'", &lcRebalHdr..cUpdVryIgn= 'U')
*-- If The thermometer is not finished
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 
  IF lnCurrent < lnTotal
  *-- FOR Loop to finish the thermometer
    FOR lnCount = lnCurrent TO lnTotal
      oProgress.CurrentProgress(lnCount)
    ENDFOR    && End of FOR Loop to finish the thermometer
  ENDIF    && End of IF The thermometer is not finished

    oProgress.VISIBLE = .F.
    
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF
  *E303410,1 TMI 09/01/2013 [End  ] 
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
=lfErsVBalF()

*!*************************************************************
*! Name      : lfErsVBalF
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Function to Erases the Vendor rebalance file
*!             and the Vendor History rebalance file
*!*************************************************************
*! Called from : lfvRBalnce()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfErsVBalF

 
*If The Vendor rebalance file is open
IF USED(lcAPRBALV)
  USE IN (lcAPRBALV)
ENDIF    && End of IF The Vendor rebalance file is open

*If The Vendor History rebalance file is open
IF USED(lcAPRBALVH)
  USE IN (lcAPRBALVH)
ENDIF    && End of IF The Vendor History rebalance file is open

*!*************************************************************
*! Name      : lfInvInfo
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Function to calculate and return the values of A/P invoice
*!             amount , total discount taken , total adj. amount , discount offer
*!             as well as the amount paid from invoice distribution file.
*!*************************************************************
*! Called from : lfRbalVend()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lcInvNo 
*!                     2 ) lnInvAmt   
*!             3 ) lnDiscTakn 
*!             4 ) lnDiscOffr 
*!             5 ) lnAdjtAmt  
*!                     6 ) lnAmtPaid
*!*************************************************************
*! Return      : 1 ) lnInvAmt holds invoice amount. 
*!               2 ) lnDiscTakn holds total discount taken.
*!               3 ) lnDiscOffr holds discount offer amount.
*!               4 ) lnAdjtAmt holds total adj. amount.  
*!               5 ) lnAmtPaid holds total paid amount. 
*!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*************************************************************
*! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[Start]
*!*  FUNCTION lfInvInfo
FUNCTION XlfInvInfo
*! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[End]
PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt 
*Save the current area
lcCurArea  = SELECT()          && Varible to save the number of the current work area
 
* Select the A/P invoice distribution
SELECT APDIST

* Define private variable to save the current record no. of invoice distribution
PRIVATE  lnSaveRec
lnSaveRec = RECNO('APDIST')

* Initialize variables 
lnInvAmt   = 0                  
lnDiscTakn = 0                  
lnDiscOffr = 0                  
lnAdjtAmt  = 0                  
lnAmtPaid  = 0 
lnPurchAmt  = 0 
*If condition not found set the record pointer to the saved record no. then retrun
IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST') 
  IF BETWEEN( lnSaveRec  , 1, RECCOUNT('APDIST')  ) 
    GOTO (lnSaveRec) IN APDIST
  ENDIF
  SELECT (lcCurArea)
  RETURN lnDiscOffr 
ENDIF 
 
* Loop the required records and calcluate the values.
SCAN REST WHILE APDIST.cVendCode + APDIST.cInvNo = M.cVendCode + lcInvNo ;
      FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' ) 

  * Get equivelent value of A/P invoice amount or debit memo amount. 
  * Note: It must only one record per invoice.  
  IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'  
    lnInvAmt   = - APDIST.nEqvAmnt 
    lnPurchAmt = - APDIST.nEqvAmnt 
  ENDIF 

  * Notes
  * APDIST.cApDtrTyp contains 'A' means debit memo
  * APDIST.cApDtrTyp contains 'H' means cash payments 
  * APDIST.cApDtrTyp contains 'M' means manuals checks payments 
  * APDIST.cApDtrTyp contains 'N' means non checks payments 
  * APDIST.cApDtrTyp contains 'P' means printed checks

  * Get and sum the equivelent value of paid amount 
  IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId $ 'ASJ'
    lnAmtPaid = lnAmtPaid +  APDIST.NAPDAMNT
  ENDIF 


  * Get and sum the equivelent value of discount taken 
  IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
    lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt 
  ENDIF 

  * Get and sum the equivelent value of adj. amount.
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'
    lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
  ENDIF
ENDSCAN  && End of Loop the required records and calcluate the values.       

IF APINVHDR.nInvAmnt > 0 && Payable Invoice
  SUM IIF(cApdActId ="A" and nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) to lnp,lndis;
  FOR cApdTrTyp+cBnkCode+cChkAcct+cApdRef+cInvNo+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)
  lnAmtPaid  = lnAmtPaid   + lnp
  lnDiscTakn = lnDiscTakn  + lndis
ENDIF  

* Get and calculate the value of discount offer from invoice header file.
lcExSin2   = ''
lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)

* Back to the initial record
SELECT APDIST
IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() ) 
  GOTO (lnSaveRec) IN APDIST
ENDIF  

* Back to the initial file and record no.
SELECT (lcCurArea)
RETURN lnDiscOffr 
*--- End of lfInvInfo()
*!*************************************************************
*! Name      : lfUpdItm24
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Update Item24
*!*************************************************************
FUNCTION lfUpdItm24
= lfMasterUpd("'APINVHDR'", &lcRebalHdr..cUpdVryIgn= 'U')

*!*************************************************************
*! Name      : lfVryItm24
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Verify Item24
*!*************************************************************
FUNCTION lfVryItm24

*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]
lcInvByDB=gfTempName()
DIMENSION laApdistStru[1,18]
SELECT APDIST
lnApdistCnt =AFIELDS(laApdistStru)
DIMENSION laApdistStru[lnApdistCnt +1,18]
laApdistStru[lnApdistCnt +1,1] = 'cDMno'
laApdistStru[lnApdistCnt +1,2] = 'C'
laApdistStru[lnApdistCnt +1,3] = 12
laApdistStru[lnApdistCnt +1,4] = 0
STORE ' ' TO  laApdistStru[lnApdistCnt +1,7],laApdistStru[lnApdistCnt +1,8],;
  laApdistStru[lnApdistCnt+1,9],laApdistStru[lnApdistCnt +1,10],;
  laApdistStru[lnApdistCnt+1,11],laApdistStru[lnApdistCnt+1,12],;
  laApdistStru[lnApdistCnt+1,13],laApdistStru[lnApdistCnt+1,14],;
  laApdistStru[lnApdistCnt+1,15],laApdistStru[lnApdistCnt+1,16]
STORE 0 TO    laApdistStru[lnApdistCnt +1,17] ,laApdistStru[lnApdistCnt +1,18]
=gfCrtTmp(lcInvByDB,@laApdistStru," cinvno+cvendcode+cDMno+capsessno",lcInvByDB,.T.)
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[End]
SELECT APINVHDR
lnCurrent = 0
=lfInitThermo(RECCOUNT(), 'Rebalance AP Invoices...')
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
lnUpThermo = RECCOUNT()
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
SCAN FOR cInvStat <> 'V'
  lnInvAmt  = 0
  lnDiscTakn  = 0
  lnDiscOffr  = 0
  lnAdjtAmt   = 0
  lnAmtPaid  = 0
  lnPurchAmt = 0
  M.cVendCode  = APINVHDR.cVendCode
  lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt)  
  *! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[Start]
  *IF APINVHDR.NINVPAID <> lnAmtPaid 
  IF IIF(APINVHDR.NINVPAID <> lnAmtPaid ,Apinvhdr.ninvadj+APINVHDR.NINVPAID <>lnAmtPaid,.F.)
  *! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[End]
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = LANG_SMREBAL_COMP  + lcCurrComp_ID +LANG_SMREBAL_BARACT+ ALLTRIM(APINVHDR.cVendCode) +;
                                             LANG_SMREBAL_INVOICE+ ALLTRIM(APINVHDR.cInvNo) +;
                                            LANG_SMREBAL_WRONGPAY
    laRebMsg[3] = " "

    IF llAutoBal
      laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    =lfVryRport()
  ENDIF 
  REPLACE APINVHDR.NINVPAID WITH  lnAmtPaid 
  lnCurrent = lnCurrent  + 1
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
    *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
    IF EMPTY(lcStyMaj)
      *E303410,1 TMI 09/01/2013 [End  ] 
    oProgress.lblSecondLabel.Caption = LANG_SMREBAL_VENDOR + ALLTRIM(APINVHDR.cVendCode)+LANG_SMREBAL_INVOICE2+ ALLTRIM(APINVHDR.cInvNo)
    oProgress.CurrentProgress(lnCurrent)

      *E303410,1 TMI 09/01/2013 [Start] 
    ENDIF
    *E303410,1 TMI 09/01/2013 [End  ]    
    
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  ELSE
    IF MOD(lnCurrent ,CEILING(lnUpThermo / 10)) = 0
      loProgress.Percent     = lnCurrent / lnUpThermo
      loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+  LANG_SMREBAL_VENDOR + ALLTRIM(APINVHDR.cVendCode)+LANG_SMREBAL_INVOICE2+ ALLTRIM(APINVHDR.cInvNo)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
    ENDIF    
  ENDIF
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  SELECT APINVHDR
ENDSCAN 

*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]
SELECT (lcInvByDB)
SELECT DISTINCT cinvno,cvendcode  FROM (lcInvByDB) INTO CURSOR 'InvVend' ORDER BY cinvno,cvendcode 
SELECT 'InvVend'
LOCATE
IF !EOF()
  lnCurrent = 0
  =lfInitThermo(RECCOUNT(), 'Rebalance AP Invoices...')
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
  lnUpThermo= RECCOUNT()
  *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  SCAN 
    =SEEK(InvVend.cinvno+InvVend.cvendcode,'APINVHDR','INVVEND')
    lnAdjtAmt   = 0
    M.cVendCode  = InvVend.cVendCode
    
    lnDiscOffr = lfInvAdj(InvVend.cinvno,@lnAdjtAmt)
    IF Apinvhdr.ninvadj <>lnAdjtAmt
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = LANG_SMREBAL_COMP  + lcCurrComp_ID +LANG_SMREBAL_BARACT+ ALLTRIM(APINVHDR.cVendCode) +;
                                             LANG_SMREBAL_INVOICE+ ALLTRIM(APINVHDR.cInvNo) +;
                                            LANG_SMREBAL_WRONGPAY
      laRebMsg[3] = " "

      IF llAutoBal
        laRebMsg[3] = LANG_SMREBAL_ENDPROC +DTOC(DATE())+SPACE(5)+TIME()
      ENDIF
      =lfVryRport()
    ENDIF 
    REPLACE APINVHDR.ninvadj WITH  lnAdjtAmt
    lnCurrent = lnCurrent  + 1
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    IF TYPE('lcXMLFileName') <> 'C'
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
      *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
      IF EMPTY(lcStyMaj)
        *E303410,1 TMI 09/01/2013 [End  ] 
      oProgress.lblSecondLabel.Caption = LANG_SMREBAL_VENDOR + ALLTRIM(APINVHDR.cVendCode)+LANG_SMREBAL_INVOICE2+ ALLTRIM(APINVHDR.cInvNo)
      oProgress.CurrentProgress(lnCurrent)
        
        *E303410,1 TMI 09/01/2013 [Start] 
      ENDIF
      *E303410,1 TMI 09/01/2013 [End  ] 
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
    ELSE
      IF MOD(lnCurrent ,CEILING(lnUpThermo / 10)) = 0
        loProgress.Percent     = lnCurrent / lnUpThermo
	loProgress.Description = LANG_SMREBAL_COMP+ lcCurrComp_ID +' '+ LANG_SMREBAL_VENDOR + ALLTRIM(APINVHDR.cVendCode)+LANG_SMREBAL_INVOICE2+ ALLTRIM(APINVHDR.cInvNo)
	loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)    
      ENDIF      
    ENDIF
    *! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  ENDSCAN 
ENDIF
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[End]
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
  *E303410,1 TMI 09/01/2013 [Start] if called from style screen do not show progress bar
  IF EMPTY(lcStyMaj)
    *E303410,1 TMI 09/01/2013 [End  ] 
    oProgress.VISIBLE = .F.
    *E303410,1 TMI 09/01/2013 [Start] 
  ENDIF 
  *E303410,1 TMI 09/01/2013 [End  ] 
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
ENDIF
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]
*!*************************************************************
*! Name      : lfVldItm24
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Valid Item24
*!*************************************************************
FUNCTION lfVldItm24
RETURN .T. 
*!*************************************************************
*! Name      : lfStpItm24
*! Developer : Mariam Mazhar
*! Date      : 03/24/2010
*! Purpose   : Setup Item24
*!*************************************************************
FUNCTION lfStpItm24
lcAlisOld = SELECT()
IF &lcRebalHdr..cUpdVryIgn='U' 
  lfVryItm24()
ENDIF 
SELECT(lcAlisOld)
RETURN .T.
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[End]

*! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[Start]
*!*************************************************************
*! Name      : lfInvInfo
*! Developer : Mariam Mazhar
*! Date      : 07/08/2010
*! Purpose   : Function to calculate and return the values of A/P invoice
*!             amount , total discount taken , total adj. amount , discount offer
*!             as well as the amount paid from invoice distribution file.
*!*************************************************************
*! Called from : lfRbalVend()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lcInvNo 
*!                     2 ) lnInvAmt   
*!             3 ) lnDiscTakn 
*!             4 ) lnDiscOffr 
*!             5 ) lnAdjtAmt  
*!                     6 ) lnAmtPaid
*!*************************************************************
*! Return      : 1 ) lnInvAmt holds invoice amount. 
*!               2 ) lnDiscTakn holds total discount taken.
*!               3 ) lnDiscOffr holds discount offer amount.
*!               4 ) lnAdjtAmt holds total adj. amount.  
*!               5 ) lnAmtPaid holds total paid amount. 
*!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*************************************************************
*! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[Start]
*FUNCTION lfInvInfo
FUNCTION _lfInvInfo
*! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[End]
PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt 

*Save the current area
lcCurArea  = SELECT()          && Varible to save the number of the current work area
* Select the A/P invoice distribution
SELECT APDIST

* Define private variable to save the current record no. of invoice distribution
PRIVATE  lnSaveRec
lnSaveRec = RECNO('APDIST')

* Initialize variables 
lnInvAmt   = 0                  
lnDiscTakn = 0                  
lnDiscOffr = 0                  
lnAdjtAmt  = 0                  
lnAmtPaid  = 0 
lnPurchAmt  = 0 
lnAdjDiff = 0
llDebitMemo = .F.
IF !USED('APDIST_A')
  =gfOpenTable('APDIST','PAYMNTS','SH','APDIST_A')
ENDIF 


SELECT APDIST
*If condition not found set the record pointer to the saved record no. then retrun
IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST') 
  IF BETWEEN( lnSaveRec  , 1, RECCOUNT('APDIST')  ) 
    GOTO (lnSaveRec) IN APDIST
  ENDIF
  SELECT (lcCurArea)
  RETURN lnDiscOffr 
ENDIF 
 
* Loop the required records and calcluate the values.
*!*  llGetCashRec = .F.
IF APINVHDR.nInvAmnt  < 0
  llDebitMemo = .T.
ENDIF 
SCAN REST WHILE APDIST.cVendCode + APDIST.cInvNo = M.cVendCode + lcInvNo ;
      FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' ) 
  * Get equivelent value of A/P invoice amount or debit memo amount. 
  * Note: It must only one record per invoice.  
  IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'  
    lnInvAmt   = - APDIST.nEqvAmnt 
    lnPurchAmt = - APDIST.nEqvAmnt 
  ENDIF 

  * Notes
  * APDIST.cApDtrTyp contains 'A' means debit memo
  * APDIST.cApDtrTyp contains 'H' means cash payments 
  * APDIST.cApDtrTyp contains 'M' means manuals checks payments 
  * APDIST.cApDtrTyp contains 'N' means non checks payments 
  * APDIST.cApDtrTyp contains 'P' means printed checks
  
  * Get and sum the equivelent value of paid amount 
  *IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId $ 'ASJ'
  IF IIF(llDebitMemo , APDIST.cApdActId = 'A' AND apdist.napdamnt < 0,APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'C')
    lnOrgPay = apdist.napdamnt 
    lnSign = IIF(apdist.napdamnt < 0,-1,1)
    IF apdist.ccurrcode <> oAriaApplication.BaseCurrency 
      lcKeyVal = CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
      SELECT APDIST_A
      IF SEEK(lcKeyVal)
        LOCATE REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID = lcKeyVal FOR;
                        cVendCode = m.cVendCode AND capSessNo= apdist.capSessNo AND  APDIST_A.cCurrCode = oAriaApplication.BaseCurrency 
        IF FOUND() 
          lcExSin2   = ''
          lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.cCurrCode ) 
          lcExSin1   = IIF(lcExSin1   = '*','/','*')
          lnOrgPay = ABS(APDIST_A.napdamnt  &lcExSin1 APDIST_A.nExRate  &lcExSin2 APDIST_A.nCurrUnit) * lnSign 
        ENDIF 
      ENDIF 
    ELSE 
      IF APINVHDR.cCurrCode <> oAriaApplication.BaseCurrency 
        lcKeyVal = CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
        SELECT APDIST_A
        IF SEEK(lcKeyVal)
          LOCATE REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID = lcKeyVal FOR;
                         cVendCode = m.cVendCode AND capSessNo= apdist.capSessNo AND  APDIST_A.cCurrCode = APINVHDR.cCurrCode 
          IF FOUND() 
            lcExSin2   = ''
            lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.cCurrCode ) 
*            lcExSin1   = IIF(lcExSin1   = '*','/','*')
            lnOrgPay = ABS(APDIST_A.napdamnt  &lcExSin1 APDIST_A.nExRate  &lcExSin2 APDIST_A.nCurrUnit) * lnSign 
          ENDIF 
        ENDIF 
      ENDIF   
    ENDIF 
    SELECT APDIST
    *IF apdist.ccurrcode <> oAriaApplication.BaseCurrency 
      lcExApSin2   = ''
      lcExAPSin1   = gfGetExSin(@lcExApSin2 , apdist.ccurrcode) 
      lnFPaidAmt = lnOrgPay  &lcExApSin1   apdist.nexrate &lcExAPSin2 apdist.ncurrunit
*!*	    ELSE
*!*	      lnFPaidAmt = lnOrgPay  
*!*	    ENDIF  
    
    IF llDebitMemo 
      lnAmtPaid = lnAmtPaid +  ABS(lnFPaidAmt)
    ELSE 
      lnAmtPaid = lnAmtPaid -  lnFPaidAmt 
    ENDIF 
    IF apdist.ccurrcode = APINVHDR.cCurrCode &&AND APINVHDR.cCurrCode <> oAriaApplication.BaseCurrency 
      lnRFPaidAmt =lnOrgPay  &lcExApSin1  APINVHDR.nExRate &lcExAPSin2 apdist.ncurrunit
      lnAdjDiff = lnAdjDiff + (lnRFPaidAmt - lnFPaidAmt)
    ENDIF   
  ENDIF 

  * Get and sum the equivelent value of discount taken 
  IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
    lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt 
  ENDIF 

  * Get and sum the equivelent value of adj. amount.
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'
    lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
  ENDIF
ENDSCAN  && End of Loop the required records and calcluate the values.       

IF APINVHDR.nInvAmnt > 0 && Payable Invoice
  SUM IIF(cApdActId ="A" and nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) to lnp,lndis;
  FOR cApdTrTyp+cBnkCode+cChkAcct+cApdRef+cInvNo+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)
  lnAmtPaid  = lnAmtPaid   + lnp
  lnDiscTakn = lnDiscTakn  + lndis
ENDIF  

* Get and calculate the value of discount offer from invoice header file.
lcExSin2   = ''
lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)

IF llDebitMemo 
  lnAmtPaid = lnAmtPaid + lnAdjDiff 
ELSE
  lnAmtPaid = lnAmtPaid - lnAdjDiff 
ENDIF   
lcExSin1   = IIF(lcExSin1   = '*','/','*')
lnAmtPaid = ROUND(lnAmtPaid  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
IF llDebitMemo 
  lnAmtPaid = -1 * lnAmtPaid 
ENDIF 

* Back to the initial record
SELECT APDIST
IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() ) 
  GOTO (lnSaveRec) IN APDIST
ENDIF  

* Back to the initial file and record no.
SELECT (lcCurArea)
RETURN lnDiscOffr 
*--- End of lfInvInfo()
*! B609188,2 MMT 07/08/2010 Reblance Vendor takes long time and Reblance AP Inv. get wrong results[End]
  *! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[Start]
*!*************************************************************
*! Name      : lfInvInfo
*! Developer : Mariam Mazhar
*! Date      : 07/08/2010
*! Purpose   : Function to calculate and return the values of A/P invoice
*!             amount , total discount taken , total adj. amount , discount offer
*!             as well as the amount paid from invoice distribution file.
*!*************************************************************
*! Called from : lfRbalVend()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lcInvNo 
*!                     2 ) lnInvAmt   
*!             3 ) lnDiscTakn 
*!             4 ) lnDiscOffr 
*!             5 ) lnAdjtAmt  
*!                     6 ) lnAmtPaid
*!*************************************************************
*! Return      : 1 ) lnInvAmt holds invoice amount. 
*!               2 ) lnDiscTakn holds total discount taken.
*!               3 ) lnDiscOffr holds discount offer amount.
*!               4 ) lnAdjtAmt holds total adj. amount.  
*!               5 ) lnAmtPaid holds total paid amount. 
*!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*************************************************************
FUNCTION lfInvInfo
*! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
  *PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt
  PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt,llBasedAmt   
  *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][End]


  *Save the current area
  lcCurArea  = SELECT()          && Varible to save the number of the current work area
  * Select the A/P invoice distribution
  SELECT APDIST

  * Define private variable to save the current record no. of invoice distribution
  PRIVATE  lnSaveRec
  lnSaveRec = RECNO('APDIST')

  * Initialize variables
  lnInvAmt   = 0
  lnDiscTakn = 0
  lnDiscOffr = 0
  lnAdjtAmt  = 0
  lnAmtPaid  = 0
  lnPurchAmt  = 0
  lnAdjDiff = 0
  llDebitMemo = .F.
  
  *! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]
  IF !USED('APINVHDR_W')
    =gfOpenTable('APINVHDR','INVVEND','SH','APINVHDR_W')
  ENDIF
  *! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[End]
  
  IF !USED('APDIST_A')
    =gfOpenTable('APDIST','PAYMNTS','SH','APDIST_A')
  ENDIF
  SELECT APDIST
  *If condition not found set the record pointer to the saved record no. then retrun
  IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST')
    IF BETWEEN( lnSaveRec  , 1, RECCOUNT('APDIST')  )
      GOTO (lnSaveRec) IN APDIST
    ENDIF
    SELECT (lcCurArea)
    RETURN lnDiscOffr
  ENDIF

  * Loop the required records and calcluate the values.
  *!*  llGetCashRec = .F.
  IF APINVHDR.nInvAmnt  < 0
    llDebitMemo = .T.
  ENDIF

  SCAN REST WHILE APDIST.cVendCode + APDIST.CINVNO = M.cVendCode + lcInvNo ;
      FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' )
    * Get equivelent value of A/P invoice amount or debit memo amount.
    * Note: It must only one record per invoice.
    IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'
      lnInvAmt   = - APDIST.nEqvAmnt
      lnPurchAmt = - APDIST.nEqvAmnt
    ENDIF

    * Notes
    * APDIST.cApDtrTyp contains 'A' means debit memo
    * APDIST.cApDtrTyp contains 'H' means cash payments
    * APDIST.cApDtrTyp contains 'M' means manuals checks payments
    * APDIST.cApDtrTyp contains 'N' means non checks payments
    * APDIST.cApDtrTyp contains 'P' means printed checks

    * Get and sum the equivelent value of paid amount
     
    IF  IIF(llDebitMemo , APDIST.cApdActId = 'A' AND APDIST.napdamnt < 0,APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'C')
      llConvToBase = .F.
      lnOrgPay = APDIST.napdamnt
      lnSign = IIF(APDIST.napdamnt < 0,-1,1)
      IF APDIST.ccurrcode <> oAriaApplication.BaseCurrency
        lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
        SELECT APDIST_A
        IF SEEK(lcKeyVal)
          LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
            cVendCode = m.cVendCode AND capSessNo= APDIST.capSessNo AND  APDIST_A.ccurrcode = oAriaApplication.BaseCurrency
          IF FOUND()
            *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
            llConvToBase = .T.
            *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][End]
            lcExSin2   = ''
            lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
            lcExSin1   = IIF(lcExSin1   = '*','/','*')
            lcExSin2 = IIF(lcExSin2 = '*','/','*')
            lnOrgPay = ABS(APDIST_A.napdamnt  &lcExSin1 APDIST_A.nExRate  &lcExSin2 APDIST_A.nCurrUnit) * lnSign
          ELSE
            IF !llDebitMemo  and APINVHDR.ccurrcode <> oAriaApplication.BaseCurrency
              SELECT APDIST
              lcFrstKey = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO
              lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
              SELECT APDIST_A
              IF SEEK(lcKeyVal)
                LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
                  cVendCode = m.cVendCode AND capSessNo= APDIST.capSessNo AND  APDIST_A.ccurrcode = APINVHDR.ccurrcode
                IF FOUND()
                  lcExSin2   = ''
                  lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
                  lnNetAmt  = APDIST_A.napdamnt  
                  lnNetRate = APDIST_A.nExRate
                  lnNetUnit = APDIST_A.nCurrUnit
                  lcCurcCode = APDIST_A.ccurrcode 
                  =SEEK(lcFrstKey)
                  SUM ABS(napdamnt)  REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcFrstKey;
                   FOR !(cApdActId $ 'AC') AND cVendCode = m.cVendCode AND capSessNo= APDIST.capSessNo AND  APDIST_A.ccurrcode = lcCurcCode AND APDIST_A.nExRate =lnNetRate  TO lnExdNet
                   lnNetAmt  = lnNetAmt - lnExdNet
                  lnOrgPay = ABS(lnNetAmt   &lcExSin1 lnNetRate   &lcExSin2 lnNetUnit) * lnSign
                  llConvToBase = .T.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        IF APINVHDR.ccurrcode <> oAriaApplication.BaseCurrency
          lcFrstKey = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO
          lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
          SELECT APDIST_A
          IF SEEK(lcKeyVal)
            LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
              cVendCode = m.cVendCode AND capSessNo= APDIST.capSessNo AND  APDIST_A.ccurrcode = APINVHDR.ccurrcode
            IF FOUND()
              lcExSin2   = ''
              lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
              lnNetAmt  = APDIST_A.napdamnt  
              lnNetRate = APDIST_A.nExRate
              lnNetUnit = APDIST_A.nCurrUnit
              lcCurcCode = APDIST_A.ccurrcode 
              =SEEK(lcFrstKey)
              SUM ABS(napdamnt)  REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcFrstKey;
               FOR !(cApdActId $ 'AC') AND cVendCode = m.cVendCode AND capSessNo= APDIST.capSessNo AND  APDIST_A.ccurrcode = lcCurcCode AND APDIST_A.nExRate =lnNetRate  TO lnExdNet
               lnNetAmt  = lnNetAmt - lnExdNet
               lnOrgPay = ABS(lnNetAmt   &lcExSin1 lnNetRate   &lcExSin2 lnNetUnit) * lnSign
               *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
               llConvToBase = .T.
               *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][End]
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      SELECT APDIST
      *! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]
      IF llDebitMemo AND TYPE('lcInvByDB')='C' AND USED(lcInvByDB)
        *B610973,1 MMT 03/26/2015 Fix issues in the Rebalance program[T20150318.0075][Start]
*!*	        IF !EMPTY(apdist.capdref) AND gfSeek(PADR(ALLTRIM(apdist.capdref),12)+apdist.cvendcode,'APINVHDR_W') ;
*!*	           AND APINVHDR_W.ninvamnt > 0
*!*	           IF !SEEK(PADR(ALLTRIM(apdist.capdref),12)+apdist.cvendcode+apdist.cinvno+apdist.capsessno ,lcInvByDB)
        IF !EMPTY(apdist.capdref) AND gfSeek(PADR(apdist.capdref,12)+apdist.cvendcode,'APINVHDR_W') ;
           AND APINVHDR_W.ninvamnt > 0
           IF !SEEK(PADR(apdist.capdref,12)+apdist.cvendcode+apdist.cinvno+apdist.capsessno ,lcInvByDB)
        *B610973,1 MMT 03/26/2015 Fix issues in the Rebalance program[T20150318.0075][End]   
             SELECT APDIST 
             SCATTER MEMO MEMVAR 
             m.cDMno = apdist.cinvno 
             *B610973,1 MMT 03/26/2015 Fix issues in the Rebalance program[T20150318.0075][Start]
             *m.cinvno  = PADR(ALLTRIM(apdist.capdref),12)             
             m.cinvno  = PADR(apdist.capdref,12)
	         *B610973,1 MMT 03/26/2015 Fix issues in the Rebalance program[T20150318.0075][End]             
             INSERT INTO (lcInvByDB) FROM MEMVAR 
           ENDIF
        ENDIF
      ENDIF
      *! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[End]      
      lcExApSin2   = ''
      lcExAPSin1   = gfGetExSin(@lcExApSin2 , APDIST.ccurrcode)

      IF !llConvToBase
        lnFPaidAmt = lnOrgPay  &lcExAPSin1   APDIST.nExRate &lcExApSin2 APDIST.nCurrUnit
      ELSE
        lnFPaidAmt = lnOrgPay
      ENDIF
      IF llDebitMemo 
        lnAmtPaid = lnAmtPaid +  ABS(lnFPaidAmt)
      ELSE
        lnAmtPaid = lnAmtPaid -  lnFPaidAmt
      ENDIF
      IF !llConvToBase
        IF APDIST.ccurrcode = APINVHDR.ccurrcode &&AND APINVHDR.cCurrCode <> oAriaApplication.BaseCurrency
          lnRFPaidAmt =lnOrgPay  &lcExAPSin1  APINVHDR.nExRate &lcExApSin2 APDIST.nCurrUnit
          lnAdjDiff = lnAdjDiff + (lnRFPaidAmt - lnFPaidAmt)
        ENDIF
      ENDIF
    ENDIF

    * Get and sum the equivelent value of discount taken
    IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
      lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt
    ENDIF

    * Get and sum the equivelent value of adj. amount.
    *! B610207,2 MMT 01/29/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
    *IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'    
    IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'J'
    *! B610207,2 MMT 01/29/2013 AP- Vendor Balances incorrect [T20130102.0008][End]
      lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
    ENDIF
  ENDSCAN  && End of Loop the required records and calcluate the values.

  *B609578,1 WAM 05/04/2011 Update vbendor balance while rebalance AP module when the option AP Balance is selected
*!*	  IF APINVHDR.nInvAmnt > 0 && Payable Invoice
*!*	    SUM IIF(cApdActId ="A" AND nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) TO lnp,lndis;
*!*	      FOR cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)

  IF APINVHDR.nInvAmnt > 0 AND TYPE('lcInvByDB')='U'
    *! B610207,2 MMT 01/29/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
*!*	    SUM IIF(cApdActId ="A" AND nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) TO lnp,lndis;
*!*	      FOR cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = "A"+SPACE(20)+PADR(lcInvNo,12) AND CVENDCODE = m.cVendCode
    SUM IIF(cApdActId ="A" ,IIF(nEqvAmnt<0,nEqvAmnt,0),IIF (cApdActId ="J", nEqvAmnt,0)), IIF ( cApdActId ="S",-nEqvAmnt,0) TO lnp,lndis;
      FOR cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = "A"+SPACE(20)+PADR(lcInvNo,12) AND CVENDCODE = m.cVendCode
    lnp=abs(lnp)  
    *! B610207,2 MMT 01/29/2013 AP- Vendor Balances incorrect [T20130102.0008][END]      
    *B609578,1 WAM 05/04/2011 (End)

    lnAmtPaid  = lnAmtPaid   + lnp
    lnDiscTakn = lnDiscTakn  + lndis
  ENDIF

  * Get and calculate the value of discount offer from invoice header file.
  lcExSin2   = ''
  lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.ccurrcode )
  lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)

  IF llDebitMemo
    lnAmtPaid = lnAmtPaid + lnAdjDiff
  ELSE
    lnAmtPaid = lnAmtPaid - lnAdjDiff
  ENDIF
  lcExSin1   = IIF(lcExSin1   = '*','/','*')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  **! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][Start]
  *lnAmtPaid = ROUND(lnAmtPaid  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
  IF llBasedAmt = .F.
    lnAmtPaid = ROUND(lnAmtPaid  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
  ELSE
    lnAmtPaid = lnAmtPaid - lnAdjtamt
  ENDIF 
  *! B610207,1 HIA 01/21/2013 AP- Vendor Balances incorrect [T20130102.0008][End]
  IF llDebitMemo
    lnAmtPaid = -1 * lnAmtPaid
  ENDIF

  * Back to the initial record
  SELECT APDIST
  IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() )
    GOTO (lnSaveRec) IN APDIST
  ENDIF

  * Back to the initial file and record no.
  SELECT (lcCurArea)
  RETURN lnDiscOffr
  *--- End of lfInvInfo()
  *! B609188,3 MMT 12/01/2010 Reblance Give wrong paid amount if ncurrunit <> 1[End]
  
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[Start]  
*!*************************************************************
*! Name      : lfInvAdj
*! Developer : Mariam Mazhar
*! Date      : 12/15/2010
*! Purpose   : Function to calculate and return the values of A/P invoice Adjustment amount
*!*************************************************************
  FUNCTION lfInvAdj
  LPARAMETERS  lcInvNo, lnAdjtAmt  

  *Save the current area
  llDebitMemo = .F.
  lcCurArea  = SELECT()          && Varible to save the number of the current work area
  * Select the A/P invoice distribution
  lnSaveRec = RECNO(lcInvByDB)
  lnAdjtAmt  = 0
  lnAdjDiff = 0
  lnAmtPaid  = 0
  IF !USED('APDIST_A')
    =gfOpenTable('APDIST','PAYMNTS','SH','APDIST_A')
  ENDIF
  
  SELECT (lcInvByDB)
  *If condition not found set the record pointer to the saved record no. then retrun
  IF !SEEK( lcInvNo + M.cVendCode  , lcInvByDB)
    IF BETWEEN( lnSaveRec  , 1, RECCOUNT(lcInvByDB))
      GOTO (lnSaveRec) IN (lcInvByDB)
    ENDIF
    SELECT (lcCurArea)
    RETURN 0
  ENDIF

  SELECT (lcInvByDB)
  SCAN REST WHILE cVendCode + CINVNO = M.cVendCode + lcInvNo ;
      FOR (!DELETED() AND  cApdStat <> 'V' )

     
    IF .T.
      llConvToBase = .F.
      lnOrgPay = &lcInvByDB..napdamnt
      lnSign = IIF(&lcInvByDB..napdamnt < 0,-1,1)
      IF &lcInvByDB..ccurrcode <> oAriaApplication.BaseCurrency
        lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
        SELECT APDIST_A
        IF SEEK(lcKeyVal)
          LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
            cVendCode = m.cVendCode AND capSessNo= &lcInvByDB..capSessNo AND  APDIST_A.ccurrcode = oAriaApplication.BaseCurrency
          IF FOUND()
            lcExSin2   = ''
            lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
            lcExSin1   = IIF(lcExSin1   = '*','/','*')
            lcExSin2 = IIF(lcExSin2 = '*','/','*')
            lnOrgPay = ABS(APDIST_A.napdamnt  &lcExSin1 APDIST_A.nExRate  &lcExSin2 APDIST_A.nCurrUnit) * lnSign
          ELSE
            IF !llDebitMemo  and APINVHDR.ccurrcode <> oAriaApplication.BaseCurrency
              SELECT (lcInvByDB)
              lcFrstKey = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO
              lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
              SELECT APDIST_A
              IF SEEK(lcKeyVal)
                LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
                  cVendCode = m.cVendCode AND capSessNo= &lcInvByDB..capSessNo AND  APDIST_A.ccurrcode = APINVHDR.ccurrcode
                IF FOUND()
                  lcExSin2   = ''
                  lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
                  lnNetAmt  = APDIST_A.napdamnt  
                  lnNetRate = APDIST_A.nExRate
                  lnNetUnit = APDIST_A.nCurrUnit
                  lcCurcCode = APDIST_A.ccurrcode 
                  =SEEK(lcFrstKey)
                  SUM ABS(napdamnt)  REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcFrstKey;
                   FOR !(cApdActId $ 'AC') AND cVendCode = m.cVendCode AND capSessNo= &lcInvByDB..capSessNo AND  APDIST_A.ccurrcode = lcCurcCode AND APDIST_A.nExRate =lnNetRate  TO lnExdNet
                   lnNetAmt  = lnNetAmt - lnExdNet
                  lnOrgPay = ABS(lnNetAmt   &lcExSin1 lnNetRate   &lcExSin2 lnNetUnit) * lnSign
                  llConvToBase = .T.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        IF APINVHDR.ccurrcode <> oAriaApplication.BaseCurrency
          lcFrstKey = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO
          lcKeyVal = cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+'A'
          SELECT APDIST_A
          IF SEEK(lcKeyVal)
            LOCATE REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcKeyVal FOR;
              cVendCode = m.cVendCode AND capSessNo= &lcInvByDB..capSessNo AND  APDIST_A.ccurrcode = APINVHDR.ccurrcode
            IF FOUND()
              lcExSin2   = ''
              lcExSin1   = gfGetExSin(@lcExSin2, APDIST_A.ccurrcode )
              lnNetAmt  = APDIST_A.napdamnt  
              lnNetRate = APDIST_A.nExRate
              lnNetUnit = APDIST_A.nCurrUnit
              lcCurcCode = APDIST_A.ccurrcode 
              =SEEK(lcFrstKey)
              SUM ABS(napdamnt)  REST WHILE cApDtrTyp+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+cApdActId = lcFrstKey;
               FOR !(cApdActId $ 'AC') AND cVendCode = m.cVendCode AND capSessNo= &lcInvByDB..capSessNo AND  APDIST_A.ccurrcode = lcCurcCode AND APDIST_A.nExRate =lnNetRate  TO lnExdNet
               lnNetAmt  = lnNetAmt - lnExdNet
               lnOrgPay = ABS(lnNetAmt   &lcExSin1 lnNetRate   &lcExSin2 lnNetUnit) * lnSign
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      
      SELECT (lcInvByDB)

      lcExApSin2   = ''
      lcExAPSin1   = gfGetExSin(@lcExApSin2 , &lcInvByDB..ccurrcode)

      IF !llConvToBase
        lnFPaidAmt = lnOrgPay  &lcExAPSin1   &lcInvByDB..nExRate &lcExApSin2 &lcInvByDB..nCurrUnit
      ELSE
        lnFPaidAmt = lnOrgPay
      ENDIF
      lnAmtPaid = lnAmtPaid -  lnFPaidAmt
      IF !llConvToBase
        IF &lcInvByDB..ccurrcode = APINVHDR.ccurrcode &&AND APINVHDR.cCurrCode <> oAriaApplication.BaseCurrency
          lnRFPaidAmt =lnOrgPay  &lcExAPSin1  APINVHDR.nExRate &lcExApSin2 &lcInvByDB..nCurrUnit
          lnAdjDiff = lnAdjDiff + (lnRFPaidAmt - lnFPaidAmt)
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN  && End of Loop the required records and calcluate the values.

  * Get and calculate the value of discount offer from invoice header file.
  lcExSin2   = ''
  lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.ccurrcode )
  lnAmtPaid = lnAmtPaid - lnAdjDiff
  
  lcExSin1   = IIF(lcExSin1   = '*','/','*')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  lnAmtPaid = ROUND(lnAmtPaid  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
  
  
  * Back to the initial record
  SELECT (lcInvByDB)
  IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() )
    GOTO (lnSaveRec) IN (lcInvByDB)
  ENDIF

  * Back to the initial file and record no.
  SELECT (lcCurArea)
  lnAdjtAmt = lnAmtPaid
  RETURN lnAdjtAmt
*! B609188,4 MMT 12/01/2010 Reblance Does not reblanace paid amount for invoices paid by applying debit memo[ENd]  
*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [Start]
*!*************************************************************
*! Name      : lfVryVenPOOpnAmt
*! Developer : Mariam Mazhar
*! Date      : 10/13/2011
*! Purpose   : Function to calculate the Vendor Open POs
*!*************************************************************
FUNCTION lfVryVenPOOpnAmt
LPARAMETERS lcVendCode
IF !USED('POSLN_VEN')
  =gfOpenTable('POSLN','POSLN','SH','POSLN_VEN')
ENDIF
lnOpenPoAmt = 0
SELECT POSLN_VEN
IF gfSqlRun("SELECT POSLN.* FROM POSLN INNER JOIN POSHDR ON POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.PO = POSHDR.PO WHERE "+;  
          " POSLN.TRANCD IN ('1','2','4','5','6') and POSHDR.STATUS in ('A','O','H') and POSHDR.cstytype in ('P','M') AND POSHDR.VENDOR = '"+STRTRAN(lcVendCode,"'","''")+"' AND POSHDR.CBUSDOCU NOT IN ('C','N') and POSHDR.CSTYTYPE NOT IN ('N','C')",'POSLN_VEN',.F.,'POSLN_VEN')
  SELECT POSLN_VEN
  DO WHILE !EOF()
    STORE 0 TO lnOpenQty1,lnOpenQty2,lnOpenQty3,lnOpenQty4,lnOpenQty5,lnOpenQty6,lnOpenQty7,lnOpenQty8
    lcKeyExpr = CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)
    lnnFCost = IIF(CBUSDOCU+CSTYTYPE = "RP",-1,1) * nFCost1                                                          
    SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = lcKeyExpr
      FOR lnCntQty = 1 TO 8
        lcCntQty = STR(lnCntQty,1)
        lnOpenQty&lcCntQty = lnOpenQty&lcCntQty + IIF(TranCd = '1', Qty&lcCntQty , -Qty&lcCntQty)
      ENDFOR
    ENDSCAN  
    lnOpenPoAmt = lnOpenPoAmt + ((MAX(lnOpenQty1,0) + MAX(lnOpenQty2,0) + MAX(lnOpenQty3,0) + MAX(lnOpenQty4,0) + MAX(lnOpenQty5,0) +;
                                 MAX(lnOpenQty6,0) + MAX(lnOpenQty7,0) + MAX(lnOpenQty8,0))*lnnFCost)
  ENDDO
ENDIF          
RETURN lnOpenPoAmt 
*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [END]

*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][Start]
FUNCTION gfGetOptionGridVars

IF TYPE('llCallFromRequestHandler') <> 'L' OR !llCallFromRequestHandler
  RETURN ''
ENDIF

RETURN "lcAria4Modules, lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal ,LANG_SMREBAL_REBLOG ,laRebMsg ,lcAria4Modules ,lcWinTitl,"+;
		"llOpenRep ,lcFilHandl ,llSaveAut ,laTempName ,lcInfoHdr ,lcInfoDtl ,"+;
		"lnInvAmt   ,lnDiscTakn ,lnDiscOffr,lnAdjtAmt ,lnAmtPaid,lnPurchAmt ,lcInvNo,"+;
		"lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt,"+;
		"llBalnBfor , lnPostTran ,lcRpFiscYr ,lcFiscYear,lcCurr_yer ,lnCurr_yer ,lcAcsMask ,lnAcsSegSz ,lcRpCmpExp ,lcRpModExp,"+;
		"lcCurrComp_ID,lcFilePath ,lcKeyVal ,laOldVal ,lcKeyType ,llAdoDye , llViewLog ,llChkInvAmt ,lcRebalHdr ,lcRebalDtl,"+;
		"lcSQLConStr, lnCompConnHand ,lcCurrComp_ID ,lcCompCode ,lcToReb ,llAutoBal, llExpr1 ,llClrReadN, "+;
		"laFChck,lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo,laFXFLT,llNComp ,"+;
		"loVendor , loWareHous ,loStyle_X ,lcCustomer,lcVendor ,lcWareHous ,lcStyle_X ,lcCustomer , lcSYCCOMP,"+;
		"lcSelInvNo,lcSelOrdNo,lcSelPONo ,lcSelAccount ,lcSelStyle ,lcSelFabric ,lcSelYear,lcSelCTNo,lcSPO,lcSoOrd,lcArInv,lcMfCt,"+;
		"lcPackSelect , lcALPack ,lnRemResult ,oProgress ,lnPrgSession ,lcComps ,lcModls ,lcPathStat ,lcAutoBal ,llExpr1,llUpdt,laRpSorCmp,"+;
		"laRpCmpCod,laRpCodMod"

FUNCTION gfInitOptionGridVars
IF TYPE('llCallFromRequestHandler') <> 'L' OR !llCallFromRequestHandler
  RETURN
ENDIF
laRebMsg  = '' && Array holds the verify rebalance log message
lcToReb= .F.
lcCompCode= .F.
llPurge= .F.
llOgForAut= .F.
llAutoBal = .F.
*-- Modules to convert in Aria4.

lcAria4Modules ="AR,IC,MA,PO,SO,MF,RM,AL,AP"
llSaveAut  = llOgForAut 
laTempName = ""  
lcInfoHdr  = "" 
lcInfoDtl  = "" 

lnInvAmt   = 0                  && Varible to hold the invoice amount
lnDiscTakn = 0                   && Varible to hold the total discount taken per invoice
lnDiscOffr = 0
lnAdjtAmt  = 0                  && Varible to hold the total adjustment amount per invoice
lnAmtPaid  = 0
lnPurchAmt = 0                   && Varible to hold the purchase amount per invoice
lcInvNo    = ""                  && Varible to hold the A/P invoice no.

*-- GL variables
STORE '' TO lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt     
llBalnBfor = .F. 
lnPostTran = 0           && Varible to hold the number of transactions that was Reposted in the Rebalance process
lcRpFiscYr = ""          && Variable holds the fiscal year if GL module was selected
lcFiscYear = ""          && Variable holds the fiscal year if GL module was selected
lcCurr_yer = ""
lnCurr_yer = 0
lcAcsMask  = ""
lnAcsSegSz = 0 
lcRpCmpExp = ''
lcRpModExp = ''
lcCurrComp_ID = ''
lcFilePath  = oAriaApplication.DataDir
lcKeyVal    = ''            && Variable to hold the key that we seek with in the detail file while rebalance header&detail (ex. Invoice#)
laOldVal    = ''            && Varible to hold the old value in the option grid.
lcKeyType   = ''            && Variable to hold the order type.
llAdoDye    = .F.
llViewLog   = .F.
llChkInvAmt = .T.
DIMENSION laFChck [2,6]  && Array to hold the fields that we will check while rebalancing header&detail.
STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo
IF !llAutoBal
  DIMENSION laFXFLT [1,1]  && Array to hold the fix filter comming from the option grid.
ENDIF

DECLARE laRpCmpCod[1,3]  && Array to hold the companies information that will be rebalance.
DECLARE laRpCodMod[1,3]
DECLARE laRpSorCmp[1,3]
*-- Rebalance Options and Functions Tables...
lcRebalHdr = gfTempName()
lcRebalDtl = gfTempName()

*-- Global variables to hold Remote Tables Object References and Cursors...
loVendor   = .F.
loWareHous = .F.
loStyle_X  = .F.
loCustomer = .F.
lcVendor   = gfTempName()
lcWareHous = gfTempName()
lcStyle_X  = gfTempName()
lcCustomer = gfTempName()
lcSYCCOMP  = gfTempName()

*-- Global Cursors hold Selected Filter Values
lcSelInvNo   = gfTempName()
lcSelOrdNo   = gfTempName()
lcSelPONo    = gfTempName()
lcSelAccount = gfTempName()
lcSelStyle   = gfTempName()
lcSelFabric  = gfTempName()
lcSelYear    = gfTempName()
lcSelCTNo    = gfTempName()
*-- TempCursors used for verifying data in Header files...
lcSPO    = gfTempName()
lcSoOrd  = gfTempName()
lcArInv  = gfTempName()
lcMfCt   = gfTempName()
lcPackSelect   = gfTempName()
lcALPack       = gfTempName()
lnPrgSession = SET("Datasession")
lcAPRBALV =  gfTempName()
lcAPRBALVH=  gfTempName()

*-- Open Company System File...
lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM SYCCOMP", '', lcSYCCOMP, "", oAriaApplication.SystemConnectionString, 3, "", lnPrgSession)
IF lnRemResult <> 1
  RETURN .F.
ENDIF
STORE '' TO lcComps, lcModls
lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET FULLPATH ON

*-- Build tempoaray files and gathering all info. into this temp.
=lfInfoHdr()

lcAutoBal = ''
llExpr1   = .F.
llUpdt    = .F.

IF llAutoBal
   lcAutoBal = lcToReb
   lcToReb   = ''
ENDIF

IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
  lcCurrComp_ID = lcCompCode
  = lpMainReb(lcToReb, '', 'U')
ELSE
  llNComp = .F.
ENDIF 
llExpr1 = '.F.'
	
*! E303349,1 SAB 02/25/2013 Convert Rebalnce to run from Request Builder [T20100422.0001][End]

**********************************************************************************************
* get the period ID if the field is empty
* TMI 
*N000682,1 TMI 22/04/2013 [Start] 
**********************************************************************************************
function lfGetPrdID
local lcRet,lnSlct
lnSlct = sele(0)
if !Empty(APINVHDR.cFspPrdId)
  lcRet = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
else
  if !used('FSPRD')
    gfOpenFile(oAriaApplication.DataDir+'FSPRD','COMFYRPRDI','SH')
  ENDIF 
  SELE FSPRD
  LOCATE FOR BETWEEN(APINVHDR.DINVDATE,FSPRD.DFSPPBGDT,FSPRD.DFSPPENDT)
  lcRet = ALLTRIM(STR(VAL(FSPRD.CFSPPRDID)))
endif 

sele (lnSlct)
retur lcRet  
*- End of lfGetPrdID.


************************************************************
*! Name      : lfCrtStyCriteria
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/29/2013
*! Purpose   : Create a special criteria for a style major passed
*! E303410,1 
************************************************************
FUNCTION lfCrtStyCriteria
PARAMETERS lcStyMaj
LOCAL lnSlct
lnSlct = SELECT(0)

*- Create the temp table lcSelStyle with field CSTYMAJOR and fill it with the value came from the style parameter
CREATE TABLE (oAriaApplication.WorkDir+lcSelStyle) (cStyMajor C(19))
SELECT (lcSelStyle)
INDEX ON cStyMajor TAG &lcSelStyle
ZAP
APPEND BLANK
REPLACE cStyMajor WITH lcStyMaj
USE IN (lcSelStyle)

*- Need to adjust the temp file lcRebalHdr to do the intended rebalance actions
*- Include the following types '5', '6', '7', '8', '9', '10', '11', '12', '13', '23'
SELECT (lcRebalHdr)
LOCATE
SCAN 
  lcItemId  = UPPER(&lcRebalHdr..cItemName)
  lcRebItem = ALLTRIM(STRTRAN(lcItemId, 'LCRPITEM'))
  IF INLIST(lcRebItem,'5', '6', '7', '8', '9', '10', '11', '12', '13', '23')
    REPLACE cUpdVryIgn WITH 'U' 
  ELSE
    REPLACE cUpdVryIgn WITH 'I'
  ENDIF 
ENDSCAN 

*- Set cLogFile = 'N'
=SEEK('LCLOGFILE') 
REPLACE cLogFile WITH  'N'

*- Need to update the variable lcRpCmpExp with the current company ID
lcRpCmpExp = oAriaApplication.ActiveCompanyID

*- Update the variable lcCurModul with the following modules  AS,EB,RM,AL,AR,IC,MA,MF,PO,SO,SR,UP,AP,GL
lcCurModul = 'AS,EB,RM,AL,AR,IC,MA,MF,PO,SO,SR,UP,AP,GL'

STORE .T. TO llRauth, llRetMemo
STORE 1 TO lnRa, lnRet

*- set llExpr1 to '.T.' to proceed in rebalance
llExpr1 = '.T.'

*- Update the array laRpCmpCod with only the current company
SET MULTILOCKS ON
llOpen = .F.
IF !USED(lcSYCCOMP)
  llOpen = gfOpenTable('SYCCOMP','CCOMP_ID','SH',lcSYCCOMP)
ELSE
  SELECT (lcSYCCOMP)
ENDIF 
*E303410,3 TMI 09/22/2013 [Start] be sure that lcSYCCOMP is the current alias
SELECT (lcSYCCOMP)
*E303410,3 TMI 09/22/2013 [End  ] 
gfSEEK(oAriaApplication.ActiveCompanyID)
laRpCmpCod [1,1]= &lcSYCCOMP..ccomp_id + " - "+ &lcSYCCOMP..cCom_Name
laRpCmpCod [1,2]= &lcSYCCOMP..cCom_dDir
laRpCmpCod [1,3]= &lcSYCCOMP..mModlSet
IF llOpen
  =gfCloseTable(lcSYCCOMP)
ENDIF

SELECT (lnSlct)
*- End of lfCrtStyCriteria.
************************************************************
*! Name      : lfSTRTOFILE
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/18/2014
*! Purpose   : testing function , logging the request builder behaviour to know where the program stucks
************************************************************
*B610750,1  test code TMI 06/18/2014 14:46 [Start] 
FUNCTION lfSTRTOFILE
PARAMETERS lcMsg
LOCAL lcFl

return  && the problem that this log introduced for it has been resolved, no more need for this log data for now

IF TYPE('lcXMLFileName') = 'C'
  SET STEP ON 
  lcLn = CHR(13)+CHR(10)+TTOC(DATETIME()) + ':'
  lcMsg = lcLn + lcMsg  
  lcFl = oariaapplication.outputhome + 'SMREBAL_RB_For_Process_&lcProcID..LOG'
  STRTOFILE(lcMsg , lcFl , 1 )
ENDIF 
*- End of lfSTRTOFILE.

******************************************************************************************
*
*    FUNCTION lfGetLen
*    t20140715.0001
*B610814,1 TMI 08/20/2014 11:32 
******************************************************************************************
FUNCTION lfGetLen
PARAMETERS lcFld
LOCAL lnSlct,lnLen,llOpen
lnSlct = SELECT(0)
*B610814,1 TMI 08/20/2014 11:32 [Start] comment this and check againest the physical field size as per Mariam's note 
*llOpen = .F.
*IF !USED('SYDFIELD')
*  USE (oAriaApplication.Syspath+'SYDFIELD') IN 0 AGAIN ALIAS SYDFIELD ORDER CFLD_NAME
*  llOpen = .T.
*ENDIF 
*=SEEK(lcFld,'SYDFIELD','CFLD_NAME')
*lnLen = SYDFIELD.NFLD_WDTH
*IF llOpen
*  USE IN SYDFIELD
*ENDIF 
*SELECT (lnSlct )
LOCAL lcAlias
lnLen = 1000  && set value to high to not get into error if the field is not defined
lcAlias = ALIAS()
IF TYPE('&lcAlias..&lcFld') # 'U'
  lnLen = FSIZE(lcFld)
ENDIF 
*B610814,1 TMI 08/20/2014 11:33 [End  ] 
RETURN lnLen