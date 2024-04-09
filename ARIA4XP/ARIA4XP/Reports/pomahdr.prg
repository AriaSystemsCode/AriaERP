*:**********************************************************************
*: Program file       : POSTYHRE.PRG
*: Program description: P/O Header report.
*: Module             : STYLE PURCHASE ORDER (PO)
*: Developer          :  NFZ neveen	farouk
*: Tracking Job Number:  037718
*: Date                :06/24/2004
*:**********************************************************************
*: Calls: 
*:         Programs        : lfCollectData 
*:                         : lfCrtTable 
*:                         : lfCalcCost    
*:         Screens         : 
*:         Global Function :
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*: Example: DO POSTYHRE
*: B608436,1 SSH Remove season filter and activate purchase group filter
*: B608777,1 HES Make a relation between POSHDR and APVENDOR in the Set function and remove it in the Reset function
*:*********************************************************
LOCAL loRDA ,lnResult

*HMA [Begin]
*-- Define variables hold the label of Cost
STORE ' ' TO lcISlbl1 ,lcISlbl2 ,lcISlbl3 ,lcISlbl4  ,;
               lcISlbl5 ,lcISlbl6 ,lcISlbl7 
IF LCRPLNGSHR='L'  && Long Layout Report
  lcISlbl1  = laCost[1,2]
  lcISlbl2  = laCost[2,2]
  lcISlbl3  = laCost[3,2]
  lcISlbl4  = laCost[4,2]
  lcISlbl5  = laCost[5,2]
  lcISlbl6  = laCost[6,2]
  lcISlbl7  = laCost[7,2]
ENDIF
*HMA [End]
IF loOgScroll.llOGFltCh 

  DO lfCollectData  && Collect Data From Poshdr (sql Table ) & from Apvendor
  SELECT lcPosHdr
  IF RECCOUNT()=0  
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF
  *--- Create Temp Table 
  DO lfCrtTable 
  *-------- && Replace Date From Cursor and calc. Cost 
  DO lfCalcCost  
  *--B127304,MMT,04/11/2005,add print decimals option [Start]
ELSE
  SELECT lcPosHdr
  IF RECCOUNT()=0  
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF
  IF !USED(lcWorkFile)
    USE oAriaApplication.WorkDir +  lcWorkFile + ".DBF" IN 0  
  ENDIF
ENDIF

DECLARE loOgScroll.lacrparams[14,2]   && array For Param
*--DECLARE loOgScroll.lacrparams[13,2]   && array For Param
*--B127304,MMT,04/11/2005,add print decimals option [End]
loOgScroll.lacrparams[1,1] ='RP_TITL'
loOgScroll.lacrparams[1,2] = M.LCRPTITLE
loOgScroll.lacrparams[2,1] ='RP_NOTPAD'
loOgScroll.lacrparams[2,2] = M.LCRPNOTE
loOgScroll.lacrparams[3,1] ='RP_SORT'
loOgScroll.lacrparams[3,2] = M.lcRPSortBy
loOgScroll.lacrparams[4,1] ='RP_Cost1'
loOgScroll.lacrparams[4,2] = lcISlbl1
loOgScroll.lacrparams[5,1] ='RP_Cost2'
loOgScroll.lacrparams[5,2] = lcISlbl2
loOgScroll.lacrparams[6,1] ='RP_Cost3'
loOgScroll.lacrparams[6,2] = lcISlbl3
loOgScroll.lacrparams[7,1] ='RP_Cost4'
loOgScroll.lacrparams[7,2] = lcISlbl4
loOgScroll.lacrparams[8,1] ='RP_Cost5'
loOgScroll.lacrparams[8,2] = lcISlbl5
loOgScroll.lacrparams[9,1] ='RP_Cost6'
loOgScroll.lacrparams[9,2] = lcISlbl6
loOgScroll.lacrparams[10,1]='RP_Cost7'
loOgScroll.lacrparams[10,2]= lcISlbl7
loOgScroll.lacrparams[11,1]='RP_Name'
*-- Amin
loOgScroll.lacrparams[11,2]= IIF(lccInvType = '0001', 'Style Purchase Order Header Report', 'Material Purchase Order Header Report')
*-- Amin
loOgScroll.lacrparams[12,1]='AllowCost'
loOgScroll.lacrparams[12,2]= IIF(gfUserPriv('IC', 'ICSTYLE', 'COSTING'),1,0)
**---- Do Case For Param. Sort By (Description)
DO CASE 
	CASE M.lcRPSortBy='P' 
	  loOgScroll.lacrparams[3,2]='P'
	CASE M.lcRPSortBy='V'
	  loOgScroll.lacrparams[3,2]='V'
	CASE M.lcRPSortBy='D'
	  loOgScroll.lacrparams[3,2]='D'
ENDCASE

loOgScroll.lacrparams[13,1]='MA'
loOgScroll.lacrparams[13,2]= IIF(oAriaApplication.ActiveModuleID='MA',1,0)
*--B127304,MMT,04/11/2005,add print decimals option [Start]
loOgScroll.lacrparams[14,1]='DisplayDecimals'
loOgScroll.lacrparams[14,2]= IIF(llRpDec,1,0)
*--B127304,MMT,04/11/2005,add print decimals option [End]
SELECT (lcWorkFile)
DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcWorkFile+'.DBF'


USE IN (lcWorkFile)
*HMA [Begin]
*RELEASE loRDA ,lnResult,lcISlbl1 ,lcISlbl2 ,lcISlbl3  ,;
              lcISlbl4 ,lcISlbl5 ,lcISlbl6 ,lcISlbl7 ,;
              lcCostField,lcLanCostField,lcActCostField,;
              lcFCostField,lcFLanCostField,lcFActCostField
RELEASE loRDA ,lnResult,lcCostField,lcLanCostField,lcActCostField,;
              lcFCostField,lcFLanCostField,lcFActCostField
*HMA [End]
=gfDispRe()

************************************************************
*! Name      : lfSetRes
*! Developer : Hesham Elmasry (HES)
*! Date      : 12/24/2008
*! Purpose   : Make set and reset function
************************************************************
*! Passed Parameters : 'S' for Set, 'R' for Reset, 'V' for Valid
************************************************************
*! Return      : None
************************************************************
*! B608777 *****
FUNCTION lfSetRes
*-----------------
LPARAMETERS LCPARAM
DO CASE
  CASE LCPARAM='S'
  	SELECT APVENDOR
    SET ORDER TO VENCODE
 	 
  	SELECT POSHDR
    lcSavRel = SET('Relation')
 	 SET RELATION TO VENDOR INTO APVENDOR ADDITIVE 
  CASE LCPARAM='R'
    SELECT POSHDR
	  SET RELATION TO &lcSavRel
ENDCASE
RETURN
*! B608777 *****
*!*************************************************************
*! Name      : lfCalcCost
*! Developer : Neveen Farouk Zaher (NFZ)
*! Date      : 07/13/2004
*! Purpose   : Replace Data From Cursor to temp. table & Calc Cost 
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*---------------
PROCEDURE lfCalcCost
*--------------

DIMENSION laNormCost[8],laLandAmnt[8] , laActuAmnt[8]
STORE 0 TO laNormCost,laLandAmnt,laActuAmnt

*-- Amin Array for save the description
DIMENSION laVendAdr[6], laCodes[3,3]
STORE '' TO laVendAdr, laCodes
lnExRate = 1
SELECT lcPosHdr
SCAN
  SCATTER MEMVAR MEMO
  **---- Search in apvendor file to replace the vendor data
  SELECT ApVendor 
  IF SEEK(lcPosHdr.Vendor)
*-- Amin This to bueatify the code not for the perforamnce  [Start]
*--- Save the address for the vendor
	laVendAdr= ''
    gfGetAdr('APVENDOR','','','', @laVendAdr)
    m.cVENCOMP  = apvendor.CVENCOMP
    =lfAdrShift('laVendAdr')  && Shift Vendor address if there is empty line.
    m.cADDRESS1 = laVendAdr[1]
    m.cADDRESS2 = laVendAdr[2]
    m.cADDRESS3 = laVendAdr[3]
    m.cADDRESS4 = laVendAdr[4]
    m.cADDRESS5 = laVendAdr[5]
    m.cADDRESS6 = laVendAdr[6]
*-- Amin This to beautify the code not for the perforamnce  [End]
  ENDIF 
  *--- 'Y' For Print NotePade
  IF M.LCRPNOTE='Y'   
    SELECT NotePad
    *-- Amin This shoudl be based on option in Option Grid. Please confirm it 
    IF SEEK(lccStyType+lcPosHdr.PO)
      m.p_notes = notepad.MNOTES
    ELSE
      m.p_notes = ""
    ENDIF 
  ENDIF  
  SELECT (lcWorkFile)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *-- Amin  We do not need to refill the array each time  [Start]
  laCodes[1,2] = "CTERMCODE"
  laCodes[2,2] = "SHIPVIA"
  laCodes[3,2] = "CDIVISION"
  laCodes[1,1] = CTERMCODE
  laCodes[2,1] = SHIPVIA
  laCodes[3,1] = cDivision
  *-- Amin  We do not need to refill the array each time  [End]
  = gfCodDes(@laCodes)
  **------ REPLACE THE DESC. OF CODES TO THE WORK FILE  

  REPLACE CTERMDESC WITH  LaCodes[1,3] ,;
          SHIPDESC  WITH  LaCodes[2,3] ,;
          CDIVDESC  WITH  LaCodes[3,3] ,;
          empNots   WITH IIF(EMPTY(P_NOTES),'0','1'),;
          LinkDesc  WITH IIF(SEEK(lcPosHdr.link_code+'013','gl_link'),gl_link.LINK_CODE + '-'+gl_link.glacnt,'')
  IF llMultCurr
    REPLACE CCURRDESC WITH lfCurrDesc()
  ENDIF  
  IF LCRPLNGSHR='L'  && IN CASE THE LONG Layout REPORT
    DO CASE
    CASE lCrpcurr = 'F'  &&  Forgin Curr.
      lnExRate = 1
      lnMyRate=1
      =lfFillCost(@laNormCost,"nFCost",.F.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
    CASE lcrpcurr='O'
      lnExRate = nPriceRat
      lnMyRate= nPriceRat
      =lfFillCost(@laNormCost , "nICost",.F.,@laLandAmnt,"nLan_Cost",@laActuAmnt,"nAct_Cost")
    CASE lcrpcurr='D' OR lcrpcurr='N'  && curr. rate by  date or by enter cursor 
      STORE 1 TO lnMyRate,lnExRate
      =lfFillCost(@laNormCost,"nFCost",.T.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
      lnExRate = lnMyRate
    ENDCASE
  ENDIF
  SELECT lcPosHdr
ENDSCAN
SELECT (lcWorkFile)
** LABEL OF COSTING TYPE
IF LCRPLNGSHR='L'  && Long Layout Report
  SELECT (lcWorkFile)
  lcISlbl1  = laCost[1,2]
  lcISlbl2  = laCost[2,2]
  lcISlbl3  = laCost[3,2]
  lcISlbl4  = laCost[4,2]
  lcISlbl5  = laCost[5,2]
  lcISlbl6  = laCost[6,2]
  lcISlbl7  = laCost[7,2]
ENDIF
RETURN
*!*************************************************************
*! Name      : lfCrtTable
*! Developer : Neveen Farouk Zaher (NFZ)
*! Date      : 07/13/2004
*! Purpose   : Create Temp. Table as a cursor struct  
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*-----------
PROCEDURE lfCrtTable
*------------
SELECT lcPosHdr
=AFIELD(laCurName)   && Copy cursor struct for to array
lnFileStru = ALEN(laCurName,1)
** add 14 field to temp file for description for currency , terms , ship ,division
DIMENSION laCurName[lnFileStru+15,18]
laCurName[lnFileStru+1,1]  = 'CcurrDesc'
laCurName[lnFileStru+2,1]  = 'CTERMDESC'
laCurName[lnFileStru+3,1]  = 'SHIPDESC'
laCurName[lnFileStru+4,1]  = 'CDIVDESC'
laCurName[lnFileStru+5,1]  = 'LinkDesc'
laCurName[lnFileStru+6,1]  = 'EmpNots'
laCurName[lnFileStru+7,1]  = 'CVENCOMP'
laCurName[lnFileStru+8,1]  = 'CADDRESS1'
laCurName[lnFileStru+9,1]  = 'CADDRESS2'
laCurName[lnFileStru+10,1] = 'CADDRESS3'
laCurName[lnFileStru+11,1] = 'CADDRESS4'
laCurName[lnFileStru+12,1] = 'CADDRESS5'
laCurName[lnFileStru+13,1] = 'CADDRESS6'
laCurName[lnFileStru+14,1] = 'P_NOTES'
laCurName[lnFileStru+15,1] = 'cCompDate'
FOR lnLoop = 1  TO 15  && CREATE THE ARRAY COLUMN FROM 2 TO 18
  IF lnLoop=14
    laCurName[lnFileStru+lnLoop,2] = 'M'
    laCurName[lnFileStru+lnLoop,3] = 0
  ELSE
    laCurName[lnFileStru+lnLoop,2] = 'C'
    laCurName[lnFileStru+lnLoop,3] = 30
  ENDIF 
  STORE 0 TO laCurName[lnFileStru+lnLoop,4],laCurName[lnFileStru+lnLoop,17],;
             laCurName[lnFileStru+lnLoop,18]
 
  STORE ' ' TO laCurName[lnFileStru+lnLoop,7],laCurName[lnFileStru+lnLoop,8],;
               laCurName[lnFileStru+lnLoop,9],laCurName[lnFileStru+lnLoop,10],;
               laCurName[lnFileStru+lnLoop,11],laCurName[lnFileStru+lnLoop,12],;
               laCurName[lnFileStru+lnLoop,13],laCurName[lnFileStru+lnLoop,14],;
               laCurName[lnFileStru+lnLoop,15],laCurName[lnFileStru+lnLoop,16],;
               laCurName[lnFileStru+lnLoop,17]
            
               laCurName[lnFileStru+lnLoop,5]=.T.
               laCurName[lnFileStru+lnLoop,6]=.f.

ENDFOR 

CREATE TABLE  oAriaApplication.WorkDir + (lcWorkFile) + ".dbf" FROM ARRAY laCurName
*! 125723,1 SMM Solve bug of wrong sorting by date [START]
SELECT (lcWorkFile)
Replace ALL cCompDate WITH DTOC(Complete)
*! 125723,1 SMM Solve bug of wrong sorting by date [START]

return
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Neveen Farouk Zaher (NFZ)
*! Date      : 07/13/2004
*! Purpose   : Collect Data From Poshdr (sql Table) & apvendor (fox Table )  
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************

*-----------------
PROCEDURE lfCollectData
*-----------------
*!*	IF TYPE("loRDA") <> 'O' 
*!*	  loRDA = CREATEOBJECT("RemoteDataAccess")
*!*	ENDIF 
*-- The following fields are used as a default in all report cases
 lcRepField="POSHDR.PO       ,poshdr.available ,POSHDR.status ,POSHDR.nPriceRat,POSHDR.nStyORDER,"+;
           "POSHDR.ENTERED  ,POSHDR.COMPLETE  ,POSHDR.VENDOR ,POSHDR.SEASON   , "+;   
           "POSHDR.CDIVISION,POSHDR.order_comp,POSHDR.receive,POSHDR.damage   , "+;
           "POSHDR.cancel   ,POSHDR.[open]    ,POSHDR.cpricecur, "+;
           "POSHDR.CTERMCODE,POSHDR.SHIPVIA,POSHDR.LINK_CODE "


*-- The following fields are used in long layout and cost layout		
IF LCRPLNGSHR='L'
  lcRepField=lcRepField+;
             ",poshdr.CWARECODE,poshdr.CDUTYCUR,poshdr.lmultiware, "+;
             "POSHDR.NICOST1   ,POSHDR.NICOST2   ,POSHDR.NICOST3   ,POSHDR.NICOST4,"   +;
             "POSHDR.NICOST5   ,POSHDR.NICOST6   ,POSHDR.NICOST7   , "                 +;
             "POSHDR.NACT_COST1,POSHDR.NACT_COST2,POSHDR.NACT_COST3,POSHDR.NACT_COST4,"+;
             "POSHDR.NACT_COST5,POSHDR.NACT_COST6,POSHDR.NACT_COST7, "                 +;
             "POSHDR.NLAN_COST1,POSHDR.NLAN_COST2,POSHDR.NLAN_COST3,POSHDR.NLAN_COST4,"+;
             "POSHDR.NLAN_COST5,POSHDR.NLAN_COST6,POSHDR.NLAN_COST7, "                 +;
             "POSHDR.NFCOST1   ,POSHDR.NFCOST2   ,POSHDR.NFCOST3   ,POSHDR.NFCOST4, "  +;
             "POSHDR.NFCOST5   ,POSHDR.NFCOST6   ,POSHDR.NFCOST7   , "                 +;
             "POSHDR.nFLanCost1,POSHDR.NfLANCOST2,POSHDR.NfLANCOST3,POSHDR.NfLANCOST4,"+;
             "POSHDR.NfLANCOST5,POSHDR.NfLANCOST6,POSHDR.NfLANCOST7, "                 +;
             "POSHDR.NFACTCOST1,POSHDR.NfACTCOST2,POSHDR.NfACTCOST3,POSHDR.NfACTCOST4,"+;
             "POSHDR.NfACTCOST5,POSHDR.NfACTCOST6,POSHDR.NfACTCOST7,  "                +;   
             "POSHDR.SHPNAME   ,POSHDR.INSURANCE ,POSHDR.CFOB      ,POSHDR.QUOTACAT,"  +;
             "POSHDR.CONTACT   ,POSHDR.PHONE     ,POSHDR.ORIGIN    ,POSHDR.LCEXPIRE,"  +;
             "POSHDR.CLCNO "
ENDIF
lcJoin = ''
*!*	lcWhereCon = " POSHDR.cBusDocu = '" + cBusDocu + "' and POSHDR.cStyType='" + lccstytype +"' "
lcWhereCon = " POSHDR.cStyType='" + lccstytype +"' "
* Check if there is a filter on PO Number

lcCurName = lfCheckFilter(3, 'POSHDR.PO')  	
IF !EMPTY(lcCurName)
  SELECT &lcCurName  	
  IF RECCOUNT() > 0
    lcSQLPO = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO') && SQL Temp File
    IF EMPTY(lcSQLPO)
      *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
	lcJoin = lcJoin + " inner join " + lcSQLPO + " TmpPo on POSHDR.PO = TmpPO.PO  "
  ENDIF
ENDIF

* Check if there is a filter on Vendor
lcCurName = lfCheckFilter(3, 'POSHDR.VENDOR')  	
IF !EMPTY(lcCurName)
  SELECT &lcCurName  	
  IF RECCOUNT() > 0
    lcSQLVendor = loOgScroll.gfSQLTempName('','Vendor C(8)',lcCurName,'cVendCode') && SQL Temp File
    IF EMPTY(lcSQLVendor)
      *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
    lcJoin = lcJoin +" inner join "+ lcSQLVendor +" TmpVend on TmpVend.Vendor = POSHDR.Vendor "
  ENDIF
ENDIF

* Get Price filter
lcCurName = lfCheckFilter(1, 'POSHDR.CPRICECUR')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","','")
  lcWhereCon = lcWhereCon + " AND POSHDR.CPRICECUR IN ('" + lcCon + "')"
ENDIF

* Get Status Filter
lcCurName = lfCheckFilter(3, 'POSHDR.STATUS')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName 
  lcCon = STRTRAN(lcCon,"|","','")
  lcWhereCon = lcWhereCon + " AND POSHDR.STATUS IN ('" + lcCon + "')"
ENDIF

* Get cDivision Filter
lcCurName = lfCheckFilter(3, 'POSHDR.CDIVISION')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","','")
  lcWhereCon = lcWhereCon + " AND POSHDR.CDIVISION IN ('" + lcCon + "')"
ENDIF

 * Get Entered Filter
lcCurName = lfCheckFilter(3, 'POSHDR.ENTERED')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","' and '")
  lcWhereCon = lcWhereCon + " AND POSHDR.ENTERED Between '" + lcCon + "'"
ENDIF

* Get Completed Filter
lcCurName = lfCheckFilter(3, 'POSHDR.COMPLETE')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","' and '")
  lcWhereCon = lcWhereCon + " AND POSHDR.COMPLETE Between '" + lcCon + "'"
ENDIF


*: B608436,1 SSH Remove season filter and activate purchase group filter
* Get Purchase Group Filter
lcCurName = lfCheckFilter(3, 'POSHDR.CPURCODE')
IF !EMPTY(lcCurName) 
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","','")
  lcWhereCon = lcWhereCon + " AND POSHDR.CPURCODE IN ('" + lcCon + "')"
ENDIF
*: B608436,1 SSH[END]

*!*	lnResult  = loRDA.sqlrun("SELECT "+lcRepField+;
*!*	                         " FROM POSHDR POSHDR (INDEX = POSHDR) ";
*!*	                         " WHERE "+ loOgScroll.lcrpsqlexp ,;
*!*	  	       			     "lcPosHdr",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)                                                

lnResult  = loOGScroll.oRDA.sqlrun("SELECT " + lcRepField+;
                         " FROM POSHDR POSHDR (INDEX = POSHDR) "+ lcJoin +;
                         " WHERE "+ lcWhereCon ,;
  	       			     "lcPosHdr",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)

SELECT lcPosHdr

RETURN 

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS 	
  DO CASE
	CASE lnArrayType = 1 
	  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 2  
	  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 3  
	  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	OTHERWISE :
		lcReturn = ""
  ENDCASE	
  RETURN lcReturn

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************

FUNCTION lfvCurDisp
*----------------
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.
return
******
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Passed Parameters : None
*!CALLED FUNCTION  lfItmPos()
*!*************************************************************
*! Return      : None
*!*************************************************************

 FUNCTION lfwRepWhen
*----------------
lcRpCurr = "F"   && Forgin Currency
*-- Amin
lcRpName='POMAHDRL'  && Long Layout Report
*-- Amin

LNRPFORMAT = ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPFORMAT'),1)
LAOGOBJCNT[LNRPFORMAT] = gfUserPriv('IC', 'ICSTYLE', 'COSTING')
= lfOGShowGet('LCRPFORMAT')
*-- validate Report format option
= lfvRepForm()
*-- end of lfwRepWhen.
return
************************************************************
*! Name      : lfFillVars
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Fill most of report memory variables.
************************************************************
*! Passed Parameters : None
************************************************************
*! Return      : None
************************************************************
FUNCTION lfFillVars
*-------------------
DIMENSION laCost [7,2] , laSetUps[15,2]
laCost = SPACE(9)
IF !USED('SYCCOMP')
  *-USE &oAriaApplication.SysPath.SYCCOMP ORDER TAG cComp_ID IN 0
  lcSelectCommand=[SELECT * FROM SYCCOMP ]
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))   
ENDIF
*** array to hold the 7 type of cost and labels
laSetUps[1,1]   = 'M_CITYPE1'
laSetUps[2,1]   = 'M_CITYPE2'
laSetUps[3,1]   = 'M_CITYPE3'
laSetUps[4,1]   = 'M_CITYPE4'
laSetUps[5,1]   = 'M_CITYPE5'
laSetUps[6,1]   = 'M_CITYPE6'
laSetUps[7,1]   = 'M_CITYPE7'

laSetUps[8,1]   = 'M_CISLBL1'
laSetUps[9,1]   = 'M_CISLBL2'
laSetUps[10,1]  = 'M_CISLBL3'
laSetUps[11,1]  = 'M_CISLBL4'
laSetUps[12,1]  = 'M_CISLBL5'
laSetUps[13,1]  = 'M_CISLBL6'
laSetUps[14,1]  = 'M_CISLBL7'
laSetUps[15,1]  = 'llMulCurr'
= gfGetMemVar(@laSetups)

laCost[1,1]  = ALLTRIM(laSetUps[1,2])
laCost[2,1]  = ALLTRIM(laSetUps[2,2])
laCost[3,1]  = ALLTRIM(laSetUps[3,2])
laCost[4,1]  = ALLTRIM(laSetUps[4,2])
laCost[5,1]  = ALLTRIM(laSetUps[5,2])
laCost[6,1]  = ALLTRIM(laSetUps[6,2])
laCost[7,1]  = ALLTRIM(laSetUps[7,2])

laCost[1,2]  = LEFT(laSetUps[8,2],9)
laCost[2,2]  = LEFT(laSetUps[9,2],9)
laCost[3,2]  = LEFT(laSetUps[10,2],9)
laCost[4,2]  = LEFT(laSetUps[11,2],9)
laCost[5,2]  = LEFT(laSetUps[12,2],9)
laCost[6,2]  = LEFT(laSetUps[13,2],9)
laCost[7,2]  = LEFT(laSetUps[14,2],9)
llMultCurr   = laSetUps[15,2]

IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    *- USE (oAriaApplication.SysPath+"SYCINT.DBF") IN 0
    lcSelectCommand=[SELECT * FROM SYCINT ]
    lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))       
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (oAriaApplication.SysPath+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF
  SELECT SYCCURR
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF
return
*-- end of lfFillVars.
************************************************************
*! Name      : lvfRepName
*! Developer : Neveen Farouk (NFZ)
*! Date      : 06/30/1999
*! Purpose   : Change the report name 
************************************************************
*! Passed Parameters : None
************************************************************
*! Return      : None
************************************************************
*---------------
FUNCTION lvfRepName
*-----------------
DO CASE
   CASE LCRPLNGSHR='S'
  	 lcRpName='POMAHDRS'
   CASE LCRPLNGSHR='L'
	   IF LCRPFORMAT='Y'
	     lcRpName='POMAHDRC'
	   ELSE
	     lcRpName='POMAHDRL'
	   ENDIF
ENDCASE
loOgScroll.lcOGLastForm = lcRpName 
RETURN

*!*************************************************************
*! Name      : lfFillCost
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Fill any cost array.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfFillCost
*----------------
PARAMETERS lcArray1,lcField1,llCallCurr,lcArray2,lcField2,lcArray3,lcField3
     
PRIVATE lnCstItem , lnCurntAls
lnCurntAls = SELECT(0)
STORE 0 TO lcArray1,lnCstItem
FOR lnCstItem = 1 TO 7   && 7 for 7 types of Cost
  lcCstItem = STR(lnCstItem,1)
  lcArray1[lnCstItem] = EVALUATE(lcField1+lcCstItem)

  IF TYPE("lcField2") = "C" 
    lcArray2[lnCstItem] = EVALUATE(lcField2+lcCstItem)
  ENDIF

  IF TYPE("lcField3") = "C"
    lcArray3[lnCstItem] = EVALUATE(lcField3+lcCstItem)
  ENDIF

  IF llCallCurr AND (lcArray1[lnCstItem] <> 0 OR ;
                     (TYPE("lcField2") = "C" AND lcArray2[lnCstItem] <> 0) OR;
                     (TYPE("lcField3") = "C" AND lcArray3[lnCstItem] <> 0))

    *-- IF P.Price. and currency not equal base currency.
    IF laCost[lnCstItem,1] = "P" AND (cPriceCur <> oAriaApplication.BaseCurrency)
      m.cCurrCode = cPriceCur
      =lfAmntDisp()  && Function for display Amount Based the Currency
      lnMyRate = lnExRate
    ENDIF

    *-- IF Duty or Manufacturing (Misc.) Operations and currency not equal base currency.
    IF laCost[lnCstItem,1] $ "MD" AND (cDutyCur <> oAriaApplication.BaseCurrency)
      m.cCurrCode = cDutyCur
      =lfAmntDisp()
    ENDIF
  ENDIF
  SELECT (lcWorkFile)
  lcCostfield="nICost"+lcCstItem
  lcLanCostfield="nLan_Cost"+lcCstItem
  lcActCostfield="nAct_Cost"+lcCstItem

  lcFCostfield="nFCost"+lcCstItem
  lcFLanCostfield="nFLanCost"+lcCstItem
  lcFActCostfield="nFActCost"+lcCstItem
  
	IF  lcrpcurr='D' OR lcrpcurr='N'
    REPLACE &LcCostField    WITH lcArray1[lnCstItem],;
            &LcLanCostField WITH lcArray2[lnCstItem],;
            &LcActCostField WITH lcArray3[lnCstItem] ,;
            nPriceRat       WITH  lnMyRate

	ENDIF
  ** COPY THE TOTAL BASIC CURRENCY TO FORGHIN BECOUSE 
  ** THE CRYSTAL REPORT TAKE DATA FROM NF FIELD
  ** THIS TYPE USED THE NORMAL COST AND I COPY IT TO FORIGN COST FIELD 
  REPLACE &lcFCostfield    WITH &LcCostField  ,;
          &lcFLanCostfield WITH &LcLanCostField ,;
          &lcFActCostfield WITH &LcActCostField  ,;
          nPriceRat        WITH lnMyRate
            
ENDFOR
SELECT (lnCurntAls)
return
*-- end of lfFillCost.
*!*************************************************************
*! Name      : lfAmntDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Calculate equavelent amount based on currency of transaction
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfAmntDisp
*------------
lcArray1[lnCstItem] = gfAmntDisp(lcArray1[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)
IF TYPE("lcField2") = "C"
  lcArray2[lnCstItem] = gfAmntDisp(lcArray2[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
IF TYPE("lcField3") = "C"
  lcArray3[lnCstItem] = gfAmntDisp(lcArray3[lnCstItem],lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
SELECT (lnCurntAls)
return
*-- end of lfAmntDisp.
*!*************************************************************
*! Name      : lfvRepForm
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate Report format option
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvRepForm
*-------------------
LNPRNNOTEPO = ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPNOTE'),1)
LAOGOBJCNT[LNPRNNOTEPO] = (LCRPFORMAT = 'N')
IF lcRPFormat = 'Y'
  lcRPNote = 'N'
ENDIF
=lfOGShowGet('lCRPNOTE')

LNLNGSHRPO = ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPLNGSHR'),1)
LAOGOBJCNT[LNLNGSHRPO] = (LCRPFORMAT = 'N')
= lfOGShowGet('LCRPLNGSHR')
IF lcRPFormat = 'Y'  && 'Y' For Costing Format
  lcRPLngShr = 'L'   && Long Layout Report 
ENDIF
=lvfREPNAME()
RETURN
*!*	*!*************************************************************
*!*	*! Name      : lfCurrDesc
*!*	*! Developer : Mohamed Badran (MAB)
*!*	*! Date      : 06/30/1999
*!*	*! Purpose   : Currency description if sort by currency.
*!*	*!*************************************************************
*!*	*! Passed Parameters  : None
*!*	*!*************************************************************
*!*	*! Returns            : Currency description.
*!*	*!*************************************************************
FUNCTION lfCurrDesc
*---------------------
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cPriceCur)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF
RETURN PADR(ALLTRIM(lcCurDesc),18)
*-- end of lfCurrDesc.


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1999
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
*MAB 07/01/1999 Speed Report performance.
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
LOCAL lnCount 
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam[lnCount]) 
     =ADEL(&lcArrayNam ,lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
   *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C" .AND. ; 
     EMPTY(&lcArrayNam[lnCount])
    &lcArrayNam[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
