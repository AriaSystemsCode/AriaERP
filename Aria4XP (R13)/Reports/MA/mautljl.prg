*:***************************************************************************
*: Program file  : MAUTLJL
*: Program desc. : Material Utilization Report
*: For Report    : MAUTLJL.RPT
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Issue#		 : N039369
*: Developer     : Heba Mohamed Amin	(HMA)
*:***************************************************************************
*: Calls :
*:    Functions  : lfBldSqlCur(),lfCrtIndex(),lfGetColor(),lfSlctFox(),lfSRVSty(),
*:                 lfStySum(),lfSumFab1(),lfSumFab2(),lfwRepWhen()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAUTLJL
*:***************************************************************************
*: Modifications:*--E102585 ,1 HMA  01/03/2005 location filter will Filter
*:                             on Transaction location not for Fabric Location. 
*: E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve 
*:       the performance,Don't recollect Data if nothing change in Option Grid 
*:B127748 ,1 HMA  05/04/2005 use letter Paper Size in America.
*:B127747 ,1 HMA  06/13/2005 Improve the performance of this report
*:B608734,1 MMT 11/02/2008 Fix bug of wrong path of .H file{T20081023.0010}
*:***************************************************************************
* OG filter
* Activity Date Range
* Item Type . Domistic/Imported
* Fabric Type
* Fabric Range
* Location
* Fabric Color
* Season
* Division
* Group
* Status
* Print consolidated Invoice

*:***************************************************************************
*B608734,1 MMT 11/02/2008 Fix bug of wrong path of .H file{Start}
*#INCLUDE V:\Aria4xp\reports\MA\mautljl.H
#INCLUDE R:\Aria4xp\reports\MA\mautljl.H
*B608734,1 MMT 11/02/2008 Fix bug of wrong path of .H file{End}

*!*	_screen.Visible =.T.
*!*	ACTIVATE WINDOW trace

*!*	SUSPEND 

IF loOGScroll.llOGFltCh   &&If Filter Changed
  *HMA  06/22/2005 Display Message of time & no of Records Colllected [Begin]
  lcStTime   = TIME()                     && Variable to hold the start Time.
  *HMA  06/22/2005 Display Message of time & no of Records Colllected [End]
  lcSelFab = ""
  llFabric = .F.
  llSeqRng = .F.
  lcSeqRngSql = ""
  lcSeqRngFox = ""
  lcDateRng =""
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
  llLocRng = .F.
  lcLocRng = ""
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 

  *--Open MINVHDR Fox File (Retrieve Cursor which have only the required fields)
  *B127747 ,1 HMA  06/13/2005 open MINVHDR file remotely [Begin]
  *lcInvFlds = "MINVHDR.CMATINV , MINVHDR.ACCOUNT "
  *lcInvTable = "MINVHDR "
  *=lfSlctFox(lcInvFlds,lcInvTable,lcMInvHdr,"") 
  loMInvHdr = CREATEOBJECT("RemoteTable","MINVHDR","MINVHDR",lcMInvHdr,SET("Datasession"))
  *B127747 ,1 HMA  06/13/2005 open MINVHDR file remotely [END]
  
  *--Open MFGOPRHD Sql File
  lcSelFlds= "SELECT MFGOPRHD.CIMTYP,MFGOPRHD.CTKTNO,MFGOPRHD.COPERSEQ ,MFGOPRHD.CCONTCODE,MFGOPRHD.LINHOUSE;
	           FROM MFGOPRHD INNER JOIN  POSHDR  ON  MFGOPRHD.CTKTNO = POSHDR.PO "
  lnConnection = loOGScroll.orda.SqlRun (lcSelFlds,lcMfgoprhd,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnConnection >=1
    lnBuffering = CURSORGETPROP("Buffering",lcMfgoprhd)
	=CURSORSETPROP("Buffering",3,lcMfgoprhd)
	SELECT (lcMfgoprhd)
	INDEX ON CTKTNO+COPERSEQ TAG lcMfgoprhd
  ENDIF 

  *--Open POSHDR Sql File (to retrieve data from POSHDR & CUTTKTH & MMFGORDH Fox Files)
  lcSelFlds= "SELECT CBUSDOCU,CSTYTYPE,PO,STYLE,VENDOR,STATUS FROM POSHDR "
  lnConnection = loOGScroll.orda.SqlRun (lcSelFlds,lcPosHdr,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnConnection >=1
	lnBuffering = CURSORGETPROP("Buffering",lcPosHdr)
	=CURSORSETPROP("Buffering",3,lcPosHdr)
	SELECT (lcPosHdr)
	INDEX ON CBUSDOCU+CSTYTYPE+PO TAG lcPosHdr      
  ENDIF 

  *--Open POSLN Sql File (to retrieve data from POFLN & MMFGORDD Fox Files)
  lcSelFlds= "SELECT CBUSDOCU,CSTYTYPE,CRSESSION,PO,STYLE,TRANCD,VENDOR,CRSESSION,REFERENCE,DYELOT;
	            FROM POSLN WHERE POSLN.CINVTYPE= '" +lcInvType+"'"
  lnConnection = loOGScroll.orda.SqlRun (lcSelFlds,lcPosLin,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnConnection >=1
    lnBuffering = CURSORGETPROP("Buffering",lcPosLin)
    =CURSORSETPROP("Buffering",3,lcPosLin)
    SELECT (lcPosLin)
    INDEX ON STYLE+CBUSDOCU+CSTYTYPE+PO+TRANCD  TAG lcPofln  
    INDEX ON CBUSDOCU+CSTYTYPE+CRSESSION+PO+STYLE+TRANCD  TAG lcMmfgordd  
  ENDIF 

  *--Create variable have the selected item seasons
  lcSeasonVal =""
  IF !EMPTY(loOgScroll.laOgfxflt[5,6])
    lcSeasonVal="'"+STRTRAN(loOgScroll.laOgFxflt[5,6],"|","','")+"'"
  ENDIF 

  *--Create variable have the selected item Divisions
  lcDivisionVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[6,6])
    lcDivisionVal="'"+STRTRAN(loOgScroll.laOgFxflt[6,6],"|","','")+"'"
  ENDIF 

  *--Create variable have the selected item Groups
  lcGroupVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[4,6])
    lcGroupVal="'"+STRTRAN(loOgScroll.laOgFxflt[4,6],"|","','")+"'"
  ENDIF 

  *--Create variable have the selected item types
  lcFabTypeVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[7,6])
    lcFabTypeVal ="'"+STRTRAN(loOgScroll.laOgFxflt[7,6],"|","','")+"'"
  ENDIF 

  *--Create variable have the selected item  status
  lcStatusVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[3,6])
    lcStatusVal ="'"+STRTRAN(loOgScroll.laOgFxflt[3,6],"|","','")+"'"
  ENDIF 

  *-- Append the selected Fabrics to the where condition Of Sql Statement.
  lcSelFab=lfBldSqlCur('ITEM.CSTYMAJOR','llFabric','lcSelFab','Fabric C(19)','CSTYMAJOR') &&IT'S A CURSOR HOLD THE SELECTED FABRBRICS
 
  *--Create item cursor.
  lcFabric=loogscroll.gftempname() 
  lcSelFld3= "SELECT ITEM.STYLE,ITEM.CSTYMAJOR AS FABRIC,ITEM.[DESC],ITEM.LOCATION AS LOC,ITEM.VENDOR,ITEM.SCALE,;
	            ITEM.PATTERN,ITEM.CITEMFLD1 AS WIDTH ,ITEM.SEASON ,ITEM.CDIVISION ,ITEM.CSTYGROUP FROM ITEM "
	    
  *--Add the filter of selected Fabrics .
  IF llFabric  && Fabrics selected in the in list browse
    lcSelFld3 = lcSelFld3 + " INNER JOIN " + lcSelFab + " TmpFabric ON TmpFabric.Fabric = ITEM.CSTYMAJOR "            
  ENDIF 

  *--Add the Filter of Inventory Type (Fabric OR Style).
  lcSelFld3= lcSelFld3 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"

  *-- Add filter (Material Type) = 'B' -> Both, 'D'-> Domestic, 'I'->Imported
  IF lcRpDomImp <>'B'
	IF lcRpDomImp = 'D'
	  lcFlt1 = " ITEM.MAKE = 1 "
	ELSE
	  lcFlt1 = " ITEM.MAKE = 0 "
	ENDIF 
	*-- add Material Type Filter to where condition
	lcSelFld3= lcSelFld3 +" AND" +lcFlt1 
  ENDIF 


  *--Add the Item Season Filter.
  IF !EMPTY(lcSeasonVal) 
	lcSelFld3= lcSelFld3 + " AND ITEM.SEASON  IN (" + lcSeasonVal+ ")"
  ENDIF 

  *--Add the Item Division Filter.
  IF !EMPTY(lcDivisionVal) 
    lcSelFld3= lcSelFld3 + " AND ITEM.CDIVISION  IN (" + lcDivisionVal + ")"
  ENDIF 

  *--Add the Item Group Filter.
  IF !EMPTY(lcGroupVal) 
	lcSelFld3= lcSelFld3 + " AND ITEM.CSTYGROUP  IN (" + lcGroupVal+ ")"
  ENDIF 

  *--Add the Item Status Filter.
  IF !EMPTY(lcStatusVal) 
	lcSelFld3= lcSelFld3 + " AND ITEM.STATUS  IN (" + lcStatusVal + ")"
  ENDIF 

  *--Add the fabric type Filter.
  IF !EMPTY(lcFabTypeVal) 
	lcSelFld3= lcSelFld3 + " AND ITEM.ITEM_TYPE IN (" + lcFabTypeVal + ")"
  ENDIF 

  *--Retrieve the item file after filtered by the previous filters.
  lnResult3 = loOGScroll.oRDA.SqlRun(lcSelFld3,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 

  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
  *!*	*--Add Filter of Selected Locations.
  *!*	lcWareHous=loOgScroll.laOgFxflt[9,6]
  *!*	*--Create another File(FABDYE1) to hold FABDYE Fields with filters needed to be made on it 

  *!*	lcFabDye1=loogscroll.gftempname() 
  *!*	IF !EMPTY(lcWareHous)
  *!*	  SELECT &lcFabDye..* FROM &lcFabDye INNER JOIN &lcWareHous ON &lcWareHous..CWARECODE = &lcFabDye..CWARECODE INTO CURSOR &lcFabDye1  
  *!*	ELSE 
  *!*	  SELECT * FROM &lcFabDye INTO CURSOR &lcFabDye1 
  *!*	ENDIF 


  *!*	SELECT(lcFabDye1)
  *!*	INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye1
  *!*	SET ORDER TO TAG lcFabDye1

  *!*	*--Display Message of No Records to Display
  *!*	SELECT (lcFabDye1)
  *!*	LOCATE 
  *!*	IF EOF()
  *!*	  =gfModalGen('TRM00052B40011','ALERT')
  *!*	  RETURN
  *!*	ENDIF 

  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 

  *--Set relation between item&itemloc to retrieve warehous value from itemloc.
  IF lnResult3 >=1
    SELECT (lcFabric)
	lnBuffering = CURSORGETPROP("Buffering",lcFabric)
	=CURSORSETPROP("Buffering",3,lcFabric)
	INDEX ON STYLE TAG lcFabric
	SET ORDER TO TAG lcFabric
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
  *!*		  SET RELATION TO STYLE  INTO &lcFabDye1 ADDITIVE
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 
  ENDIF 

  *--Display Message of No Records to Display
  SELECT (lcFabric)
  LOCATE 
  IF EOF()
    =gfModalGen('TRM00052B40011','ALERT')
	RETURN
  ENDIF 


  *--Create variable have the selected item colors
  lcColorVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[1,6])
	lcColorVal="'"+STRTRAN(loOgScroll.laOgFxflt[1,6],"|","','")+"'"
  ENDIF 
	 

  *--Create variable have the selected TARNSCATION DATE RANGE
  lcDateRng=""
  lnDateRng=AT('ITEMJRNL.DTRDATE',loOgScroll.lcRpSqlExp)
  IF lnDateRng > 0
	lcleftExp = SUBSTR(loOgScroll.lcRpSqlExp,lnDateRng)
	lnStrOfRng=AT('ITEMJRNL.DTRDATE',lcleftExp)
	lnEndOfRng=AT(')',lcLeftExp)
	lcDateRng=SUBSTR(lcleftexp,lnStrOfRng,lnEndOfRng-1)
  ENDIF   

  *--Create two variables to hold the high & low value of range.
  ldlDate={//}
  ldhDate={//}
  IF !EMPTY(loOgScroll.laOgFxflt[8,6])
    lcRange=loOgScroll.laOgFxflt[8,6]
	lnDate=AT('|',lcRange)
	ldlDate=CTOD(SUBSTR(lcRange,1,lnDate-1))
	ldhDate=CTOD(SUBSTR(lcRange,lnDate+1))
  ENDIF   

  *--Create variable have the selected TARNSCATION SEQUENCE RANGE
  lcSeqRngSql =""
  lcSeqRngSql =lfBldSqlCur('ITEMJRNL.CSESSION','llSeqRng','lcSeqRngSql','CSESSION C(6)','CSESSION') &&IT'S A CURSOR HOLD THE SELECTED sequence range
  lcSeqRngFox = loOgScroll.laOgfxflt[10,6]

  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
  lcLocRng =""
  lcLocRng =lfBldSqlCur('ITEMJRNL.CWARECODE','llLocRng','lcLocRng','CWARECODE C(6)','CWARECODE') &&IT'S A CURSOR HOLD THE SELECTED Locations range
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 



  *--Create ItemJrnl cursor.
  lcMatJrnl=loogscroll.gftempname() 
  lcSelFld4= "SELECT ITEMJRNL.STYLE,ITEMJRNL.CTRCODE ,ITEMJRNL.CIRTYPE ,ITEMJRNL.DTRDATE,ITEMJRNL.CSESSION,;
	            ITEMJRNL.REFERENCE , ITEMJRNL.CBUSDOCU , ITEMJRNL.CSTYTYPE ,ITEMJRNL.NCOST,ITEMJRNL.CTRTYPE,ITEMJRNL.CWARECODE,"    
  lcSelFld4=lcSelFld4+"ITEMJRNL.NSTK1,ITEMJRNL.NSTK2,ITEMJRNL.NSTK3,ITEMJRNL.NSTK4,ITEMJRNL.NSTK5,;
	            ITEMJRNL.NSTK6,ITEMJRNL.NSTK7,ITEMJRNL.NSTK8,ITEMJRNL.NTOTSTK,ITEMJRNL.CDYELOT,ITEMJRNL.CRSESSION,;
	            ITEMJRNL.CISESSION FROM ITEMJRNL  "
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
  *--Add the TARNSCATION LOCATION RANGE Filter.
  IF llLocRng  && Locations selected in the in list browse
	lcSelFld4 = lcSelFld4 + " INNER JOIN " + lcLocRng  + " TmpLocRng ON TmpLocRng.cWarecode = ITEMJRNL.CWARECODE "            
  ENDIF 
  *E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 

  *--Add the TARNSCATION SEQUENCE RANGE Filter.
  IF llSeqRng  && Sequences selected in the in list browse
	lcSelFld4 = lcSelFld4 + " INNER JOIN " + lcSeqRngSql  + " TmpSeqRng ON TmpSeqRng.cSession = ITEMJRNL.CSESSION "            
  ENDIF 

  *--Add the Filter of Inventory Type (Fabric OR Style).
  lcSelFld4= lcSelFld4 + "  WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"

  *--Add the color Filter.
  IF !EMPTY(lcColorVal) 
    lcSelFld4= lcSelFld4 + " AND RIGHT(ITEMJRNL.STYLE," +ALLTRIM(STR(lnColorLen)) +" ) IN (" + lcColorVal +")"
  ENDIF 

  *--Add the TARNSCATION DATE RANGE Filter.
  IF !EMPTY(lcDateRng) 
    lcSelFld4= lcSelFld4 + " AND "+lcDateRng
  ENDIF 


  *--Retrieve the Inventory journal file after filtered by the previous filters.

  lnResult4 = loOGScroll.oRDA.SqlRun(lcSelFld4,lcMatJrnl,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnResult4 >=1
	SELECT (lcMatJrnl)
	lnBuffering = CURSORGETPROP("Buffering",lcMatJrnl)
	=CURSORSETPROP("Buffering",3,lcMatJrnl)
	IF lcrpSortby='D' &&Sort By Date
	  INDEX ON STYLE+DTOS(DTRDATE)+CSESSION TAG lcMatJrnl
	ELSE   &&Sort By Sequence Range
	  INDEX ON STYLE+CSESSION TAG lcMatJrnl
	ENDIF 
    SET ORDER TO TAG lcMatJrnl
  ENDIF  


  *--Display Message of No Records to Display
  SELECT (lcMatJrnl)
  LOCATE 
  IF EOF()
	=gfModalGen('TRM00052B40011','ALERT')
	RETURN
  ENDIF 

*  SELECT (lcFabric)
*  SET RELATION TO STYLE INTO &lcMatJrnl ADDITIVE 

  SELECT (lcMatJrnl)
  SET RELATION TO CBUSDOCU+CSTYTYPE+CTRCODE INTO &lcPosHdr ADDITIVE

  *-- Start Collecting Data
  DO lpCollData

ELSE 
*E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve the performance,Don't recollect Data if nothing change in Option Grid [Begin]
  IF !USED(lcTempHdr)
    USE oAriaApplication.WorkDir +  lcTempHdr + ".DBF" IN 0  
  ENDIF

  SELECT(lcTempHdr)
  IF RECCOUNT()=0 
 	*-- There is no record to display
    =gfModalGen('TRM00052B40011','ALERT')
    USE IN &lcTempHdr
    RETURN 
  ELSE 
    IF RECCOUNT(lcFabric)=0  OR  RECCOUNT(lcMatJrnl)=0
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN 
    ELSE 
      *-- RETURN .T.	
      gfDispRe()
    ENDIF 
  ENDIF

ENDIF &&End Of Filter Change 
*E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve the performance,Don't recollect Data if nothing change in Option Grid [End]
*!*************************************************************
*! Name      : lpCollData
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 11/01/2004
*! Purpose   : Collecting data
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpCollData
*!*************************************************************

PROCEDURE lpCollData 
PRIVATE lnSelect
*ssh
*return
*--Create Temp. File to Collect Header Data for material.
lcTempHdr = loOgScroll.gfTempName()  

*--Create Temp. File to Collect Detail Data about operation done on materials.
lcTempLin = loOgScroll.gfTempName()  

SELECT (lcFabric)
AFIELDS(laTempHdr)
lnArrLen = ALEN(laTempHdr,1) 
DIMENSION laTempHdr[lnArrLen+25,18] 

laTempHdr[lnArrLen+1,1]="Privious_Reciepts"
laTempHdr[lnArrLen+1,2]="N"
laTempHdr[lnArrLen+1,3]=12
laTempHdr[lnArrLen+1,4]=3

laTempHdr[lnArrLen+2,1]="Privious_Usage"
laTempHdr[lnArrLen+2,2]="N"
laTempHdr[lnArrLen+2,3]=12
laTempHdr[lnArrLen+2,4]=3

laTempHdr[lnArrLen+3,1]="Start_Inv_Bal"
laTempHdr[lnArrLen+3,2]="N"
laTempHdr[lnArrLen+3,3]=12
laTempHdr[lnArrLen+3,4]=3

laTempHdr[lnArrLen+4,1]="Size1"
laTempHdr[lnArrLen+4,2]="C"
laTempHdr[lnArrLen+4,3]=10
laTempHdr[lnArrLen+4,4]=0

laTempHdr[lnArrLen+5,1]="Size2"
laTempHdr[lnArrLen+5,2]="C"
laTempHdr[lnArrLen+5,3]=10
laTempHdr[lnArrLen+5,4]=0

laTempHdr[lnArrLen+6,1]="Size3"
laTempHdr[lnArrLen+6,2]="C"
laTempHdr[lnArrLen+6,3]=10
laTempHdr[lnArrLen+6,4]=0

laTempHdr[lnArrLen+7,1]="Size4"
laTempHdr[lnArrLen+7,2]="C"
laTempHdr[lnArrLen+7,3]=10
laTempHdr[lnArrLen+7,4]=0

laTempHdr[lnArrLen+8,1]="Size5"
laTempHdr[lnArrLen+8,2]="C"
laTempHdr[lnArrLen+8,3]=10
laTempHdr[lnArrLen+8,4]=0

laTempHdr[lnArrLen+9,1]="Size6"
laTempHdr[lnArrLen+9,2]="C"
laTempHdr[lnArrLen+9,3]=10
laTempHdr[lnArrLen+9,4]=0

laTempHdr[lnArrLen+10,1]="Size7"
laTempHdr[lnArrLen+10,2]="C"
laTempHdr[lnArrLen+10,3]=10
laTempHdr[lnArrLen+10,4]=0

laTempHdr[lnArrLen+11,1]="Size8"
laTempHdr[lnArrLen+11,2]="C"
laTempHdr[lnArrLen+11,3]=10
laTempHdr[lnArrLen+11,4]=0

laTempHdr[lnArrLen+12,1]="Qty1"
laTempHdr[lnArrLen+12,2]="N"
laTempHdr[lnArrLen+12,3]=12
laTempHdr[lnArrLen+12,4]=3

laTempHdr[lnArrLen+13,1]="Qty2"
laTempHdr[lnArrLen+13,2]="N"
laTempHdr[lnArrLen+13,3]=12
laTempHdr[lnArrLen+13,4]=3

laTempHdr[lnArrLen+14,1]="Qty3"
laTempHdr[lnArrLen+14,2]="N"
laTempHdr[lnArrLen+14,3]=12
laTempHdr[lnArrLen+14,4]=3

laTempHdr[lnArrLen+15,1]="Qty4"
laTempHdr[lnArrLen+15,2]="N"
laTempHdr[lnArrLen+15,3]=12
laTempHdr[lnArrLen+15,4]=3

laTempHdr[lnArrLen+16,1]="Qty5"
laTempHdr[lnArrLen+16,2]="N"
laTempHdr[lnArrLen+16,3]=12
laTempHdr[lnArrLen+16,4]=3

laTempHdr[lnArrLen+17,1]="Qty6"
laTempHdr[lnArrLen+17,2]="N"
laTempHdr[lnArrLen+17,3]=12
laTempHdr[lnArrLen+17,4]=3

laTempHdr[lnArrLen+18,1]="Qty7"
laTempHdr[lnArrLen+18,2]="N"
laTempHdr[lnArrLen+18,3]=12
laTempHdr[lnArrLen+18,4]=3

laTempHdr[lnArrLen+19,1]="Qty8"
laTempHdr[lnArrLen+19,2]="N"
laTempHdr[lnArrLen+19,3]=12
laTempHdr[lnArrLen+19,4]=3

laTempHdr[lnArrLen+20,1]="TotQty"
laTempHdr[lnArrLen+20,2]="N"
laTempHdr[lnArrLen+20,3]=13
laTempHdr[lnArrLen+20,4]=3

laTempHdr[lnArrLen+21,1]="Po_Reciept"
laTempHdr[lnArrLen+21,2]="N"
laTempHdr[lnArrLen+21,3]=12
laTempHdr[lnArrLen+21,4]=3

laTempHdr[lnArrLen+22,1]="Adjustment"
laTempHdr[lnArrLen+22,2]="N"
laTempHdr[lnArrLen+22,3]=12
laTempHdr[lnArrLen+22,4]=3

laTempHdr[lnArrLen+23,1]="Usage"
laTempHdr[lnArrLen+23,2]="N"
laTempHdr[lnArrLen+23,3]=12
laTempHdr[lnArrLen+23,4]=3

laTempHdr[lnArrLen+24,1]="SHIPPED"
laTempHdr[lnArrLen+24,2]="N"
laTempHdr[lnArrLen+24,3]=12
laTempHdr[lnArrLen+24,4]=3

laTempHdr[lnArrLen+25,1]="Location"
laTempHdr[lnArrLen+25,2]="C"
laTempHdr[lnArrLen+25,3]=10
laTempHdr[lnArrLen+25,4]=0

FOR lncnt=7 TO 16
  FOR lnInc=1 TO 25
    STORE SPACE(0) TO laTempHdr[lnArrLen+lnInc,lnCnt]
  ENDFOR 
ENDFOR  
FOR lnInc=1 TO 25
  STORE 0 TO laTempHdr[lnArrLen+lnInc,17],laTempHdr[lnArrLen+lnInc,18]
ENDFOR   

=gfCrtTmp(lcTempHdr,@laTempHdr,'STYLE',lcTempHdr,.T.)

LnFabLength=LEN(&lcFabric..STYLE) 
DIMENSION laTempLin[19,18] 

laTempLin[1,1]="Style"
laTempLin[1,2]="C"
laTempLin[1,3]=LnFabLength
laTempLin[1,4]=0

laTempLin[2,1]="Date"
laTempLin[2,2]="D"
laTempLin[2,3]=8
laTempLin[2,4]=0

laTempLin[3,1]="Type"
laTempLin[3,2]="C"
laTempLin[3,3]=1
laTempLin[3,4]=0

laTempLin[4,1]="PO_CT"
laTempLin[4,2]="C"
laTempLin[4,3]=6
laTempLin[4,4]=0

laTempLin[5,1]="Vendor1"
laTempLin[5,2]="C"
laTempLin[5,3]=8
laTempLin[5,4]=0

laTempLin[6,1]="Vendor2"
laTempLin[6,2]="C"
laTempLin[6,3]=8
laTempLin[6,4]=0

laTempLin[7,1]="Vendor3"
laTempLin[7,2]="C"
laTempLin[7,3]=8
laTempLin[7,4]=0

laTempLin[8,1]="Location"
laTempLin[8,2]="C"
laTempLin[8,3]=6
laTempLin[8,4]=0

laTempLin[9,1]="Reference"
laTempLin[9,2]="C"
laTempLin[9,3]=30
laTempLin[9,4]=0

laTempLin[10,1]="Dyelot"
laTempLin[10,2]="C"
laTempLin[10,3]=10
laTempLin[10,4]=0

laTempLin[11,1]="Unit_Cost"
laTempLin[11,2]="N"
laTempLin[11,3]=12
laTempLin[11,4]=3

laTempLin[12,1]="Item_Cost"
laTempLin[12,2]="N"
laTempLin[12,3]=12
laTempLin[12,4]=3

laTempLin[13,1]="Balance"
laTempLin[13,2]="N"
laTempLin[13,3]=12
laTempLin[13,4]=3

laTempLin[14,1]="NewBal"
laTempLin[14,2]="N"
laTempLin[14,3]=12
laTempLin[14,4]=3

laTempLin[15,1]="RcvAdjUsed"
laTempLin[15,2]="N"
laTempLin[15,3]=12
laTempLin[15,4]=3

laTempLin[16,1]="TranType"
laTempLin[16,2]="C"
laTempLin[16,3]=25
laTempLin[16,4]=0

laTempLin[17,1]="Seq_No"
laTempLin[17,2]="C"
laTempLin[17,3]=6
laTempLin[17,4]=0

laTempLin[18,1]="Status"
laTempLin[18,2]="C"
laTempLin[18,3]=10
laTempLin[18,4]=0

laTempLin[19,1]="CutTktStyle"
laTempLin[19,2]="C"
laTempLin[19,3]=20
laTempLin[19,4]=0


FOR lncnt=7 TO 16
  FOR lnInc=1 TO 19
    STORE SPACE(0) TO laTempLin[lnInc,lnCnt]
  ENDFOR 
ENDFOR  
FOR lnInc=1 TO 19
  STORE 0 TO laTempLin[lnInc,17],laTempLin[lnInc,18]
ENDFOR   

  
=gfCrtTmp(lcTempLin,@laTempLin,'STYLE',lcTempLin,.T.)

SELECT (lcTempLin)
INDEX ON STYLE TAG lcTempLin1
INDEX ON STYLE+TYPE TAG lcTempLin
SET ORDER TO lcTempLin
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
IF lcrpSortby='D' AND !EMPTY(ldlDate)   && if sort by Date
  *--Create Cursor from Item Journal File  without filter of date
  lcMat1=loogscroll.gftempname() 
  lcSelFld5= "SELECT ITEMJRNL.STYLE,ITEMJRNL.CTRTYPE,ITEMJRNL.CIRTYPE ,ITEMJRNL.DTRDATE,ITEMJRNL.CSESSION,ITEMJRNL.NTOTSTK;
               FROM  ITEMJRNL WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"
  lnResult5 = loOGScroll.oRDA.SqlRun(lcSelFld5,lcMat1,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnResult5 >=1
    SELECT (lcMat1)
    lnBuffering = CURSORGETPROP("Buffering",lcMat1)
    =CURSORSETPROP("Buffering",3,lcMat1)
    INDEX ON STYLE+DTOS(DTRDATE)+CSESSION TAG lcMat1
    SET ORDER TO TAG lcMat1
  ENDIF
ENDIF 
IF lcrpSortby='S' AND !EMPTY(lcSeqRngFox)   && if sort by Sequence
  *--Create Cursor from Item Journal File  without filter of Sequence Range.
  lcMat1=loogscroll.gftempname() 
  	  lcSelFld5= "SELECT ITEMJRNL.STYLE,ITEMJRNL.CTRTYPE,ITEMJRNL.CIRTYPE ,ITEMJRNL.DTRDATE,ITEMJRNL.CSESSION,ITEMJRNL.NTOTSTK;
                   FROM  ITEMJRNL WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"
  lnResult5 = loOGScroll.oRDA.SqlRun(lcSelFld5,lcMat1,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnResult5 >=1
    SELECT (lcMat1)
    lnBuffering = CURSORGETPROP("Buffering",lcMat1)
    =CURSORSETPROP("Buffering",3,lcMat1)
    INDEX ON STYLE+CSESSION TAG lcMat1
    SET ORDER TO TAG lcMat1
  ENDIF  
ENDIF 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End] 
*--Scan for Journal Headers 

*HMA  06/19/2005 Open the Scale File Remotly [Begin]
*SELECT(lcFabric)
IF llRpPrtSz
  loScale =CreateObject("RemoteTable","Scale","Scale","ScaleR",SET("Datasession"))
ENDIF 
*SET RELATION TO 'S'+Scale INTO Scale ADDITIVE 
*HMA  06/19/2005 Open the Scale File Remotly [End]
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
lcMatJrnl1=loogscroll.gftempname()
SELECT DISTINCT &lcMatJrnl..Style FROM &lcMatJrnl INTO CURSOR &lcMatJrnl1
SELECT(lcMatJrnl)
LOCATE 
SELECT(lcMatJrnl1)
SET RELATION TO STYLE INTO &lcMatJrnl ADDITIVE 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
SCAN 

*E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[BEGIN] 
*!*	  IF !SEEK(&lcFabric..style ,lcFabDye1)
*!*	    LOOP
*!*	  ENDIF 
*E102585 ,1 HMA  01/03/2005 location filter will Filter on Transaction location instead Fabric Location.[END] 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
*  IF !SEEK(&lcFabric..style ,lcMatJrnl) 
*    LOOP
*  ENDIF 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
  SELECT(lcMatJrnl)
  DIMENSION laTOTCLR(4)
  STORE 0.00 TO laTOTCLR
  lnPRV_REC=0
  lnPRV_USG =0
  lnBAL=0
  lnNEWBAL=0
  lnTotCost =0
  *-- laTotClr(1) : is an array element used to accumulate the PO-RECEIPTS . 
  *-- in case of lcMatJrnl.CTRTYPE='5' (PO-RECEIPT) or '6'  (PO-RETURN) 
  *-- then we have laTOTCLR(1)= laTOTCLR(1)+IIF(cTrType='5',nTotStk,-nTotStk)
  *-- laTOTCLR(2) : is an array element used to accumulate the ADJUSTMENTS .
  *-- in case of lcMatJrnl.CTRTYPE='1' (ADJUSTMENTS) 
  *-- then we have laTOTCLR(2)=laTOTCLR(2)+IIF(cirType='R',nTotStk,IIF(cIrType='I',nTotStk,-nTotStk)) 
  *-- laTOTCLR(3) : is an array element used to accumulate the USAGE .
  *-- in case of lcMatJrnl.CTRTYPE='9' (USAGE) 
  *-- then we have laTOTCLR(3)=laTOTCLR(3)+nTotStk
  *-- laTOTCLR(4) : is an array element used to accumulate the Shipped Amount .
  *-- in case of lcMatJrnl.CTRTYPE='3' (SHIPPED) 
  *-- then we have laTOTCLR(4)=laTOTCLR(4)+nTotStk
  
  *--NRECIEVED field in MATINVJL file will be replaced by NTOTSTK when CIRTYPE="R" in ITEMJRNL file
  *--NISSUED field in MATINVJL file will be replaced by -NTOTSTK when CIRTYPE="I" in ITEMJRNL file
  *--NRECEIVED-NISSUED in MATINVJL file will be replaced by NTOTSTK in ITEMJRNL file

 
  IF lcrpSortby='D'   && if sort by Date
    *-- if there is  no date range is entered then we fill Header Data & quantities & totals of po-recipts or adjustments or 
    *-- usage for each item-color
    *-- in this case there is no values for previous reciepts,usage,or starting inventorey balance.
    IF EMPTY(ldlDate) 
      lcStyle = &lcMatJrnl1..Style
      IF !SEEK(lcStyle,lcTempHdr)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
        IF SEEK(lcStyle,lcFabric)
		    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
        *HMA  06/19/2005 Open the Scale File Remotly [Begin]
        *INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
        *                        size3,size4,size5,size6,size7,size8) VALUES(&lcFabric..Style,;
        *                        &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
        *                        &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric,;
        *                        SCALE.Sz1,SCALE.Sz2,SCALE.Sz3,SCALE.Sz4,SCALE.Sz5,SCALE.Sz6,SCALE.Sz7,SCALE.Sz8)
          IF llRpPrtSz && if print sizes breakDown
            SELECT ScaleR
            loScale.Seek('S'+&lcFabric..Scale)
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
                                size3,size4,size5,size6,size7,size8) VALUES(&lcFabric..Style,;
                                &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
                                &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric,;
                                SCALER.Sz1,SCALER.Sz2,SCALER.Sz3,SCALER.Sz4,SCALER.Sz5,SCALER.Sz6,SCALER.Sz7,SCALER.Sz8)
  		  SELECT(lcMatJrnl)
		  ELSE 
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric) VALUES(&lcFabric..Style,;
                                &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
                                &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric)


          ENDIF 
        *HMA  06/19/2005 Open the Scale File Remotly [End]			
        ENDIF 
      ENDIF      
      IF SEEK(lcStyle,lcTempHdr)
        SCAN REST WHILE STYLE+DTOS(DTRDATE)+CSESSION = lcStyle  && To Collect Stock Qtys  per each Item-Color.
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
          SELECT(lcTempHdr)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
          IF llRpPrtSz && if print sizes breakDown
            FOR lncnt=1 TO 8
              lcCnt = STR(lnCnt,1)
              REPLACE Qty&lcCnt WITH Qty&lcCnt+&lcMatJrnl..nStk&lcCnt
            ENDFOR 
            REPLACE TotQty   WITH  TotQty+&lcMatJrnl..nTotStk
          ENDIF 
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
          SELECT(lcMatJrnl)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          DO CASE
            CASE cTrType $ '56'   && po-receipt \po-Return
              laTOTCLR(1)= laTOTCLR(1)+IIF(cTrType='5',nTotStk,-nTotStk)
              lnNEWBAL   = lnNEWBAL + IIF(cTrType='5',nTotStk,-nTotStk) 
              lnTotCost  = IIF(cTrType='5',nTotStk,-nTotStk) * nCost 
            CASE cTrType ='1'   && adjustment
              laTOTCLR(2)=laTOTCLR(2)+nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
            CASE cTrType $ '28'   && Physical\Locking
              lnNEWBAL   = lnNEWBAL + nTotStk
            CASE cTrType = '9'   && (usage)issue/return fabric in cuttkt
              laTOTCLR(3)=laTOTCLR(3)-nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
              lnTotCost  = - nTotStk * nCost               
            CASE cTrType = '3'  AND  !llRpPCnInv && Shipped
              laTOTCLR(4)=laTOTCLR(4)+nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
            CASE cTrType = '3'  AND  llRpPCnInv && Total Shipped   ,Don't calculate the balance amount because it will be consolidated 
              laTOTCLR(4)=laTOTCLR(4)+nTotStk
          ENDCASE && end case transaction type  
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
          *REPLACE &lcTempHdr..Po_Reciept   WITH  laTOTCLR(1) ,;
          *        &lcTempHdr..Adjustment   WITH  laTOTCLR(2) ,;
          *        &lcTempHdr..Usage        WITH  laTOTCLR(3) ,;
          *        &lcTempHdr..Shipped      WITH  laTOTCLR(4)
          lnSelect=SELECT(0)
          SELECT(lcTempHdr)
          REPLACE Po_Reciept   WITH  laTOTCLR(1) ,;
                  Adjustment   WITH  laTOTCLR(2) ,;
                  Usage        WITH  laTOTCLR(3) ,;
                  Shipped      WITH  laTOTCLR(4)
          SELECT(lnSelect)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]         
          *--call Function to Collect Line Data 
          =lfCollLinData()
          lnBal=lnNewBal         
        ENDSCAN 
        IF llRpPCnInv &&if consolidated by invoice 
          =lfConsInv()
        ENDIF 
      ENDIF 
      
    ELSE     && if there is date range selected
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
*!*	      *--Create Cursor from Item Journal File  without filter of date
*!*	      lcMat1=loogscroll.gftempname() 
*!*	      lcSelFld5= "SELECT ITEMJRNL.STYLE,ITEMJRNL.CTRTYPE,ITEMJRNL.CIRTYPE ,ITEMJRNL.DTRDATE,ITEMJRNL.CSESSION,ITEMJRNL.NTOTSTK;
*!*	                   FROM  ITEMJRNL WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"
*!*	      lnResult5 = loOGScroll.oRDA.SqlRun(lcSelFld5,lcMat1,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
*!*	      IF lnResult5 >=1
*!*		    SELECT (lcMat1)
*!*	  	    lnBuffering = CURSORGETPROP("Buffering",lcMat1)
*!*		    =CURSORSETPROP("Buffering",3,lcMat1)
*!*		    INDEX ON STYLE+DTOS(DTRDATE)+CSESSION TAG lcMat1
*!*		    SET ORDER TO TAG lcMat1
*!*		  ENDIF  
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
      *-- scan lcMatJrnl for the same fabric and same color and collect the data as follows:
      *-- Header Data such as desc,pattern ,Season,....etc., will be entered in lctemphdr file for each item when transaction Date included in Date Range 
      *-- previous Reciepts,Usage,Starting Inv Balance will be entered for in lctemphdr file for each item when Transction Date < Low entered Date Range
      *-- Qtys &Po_Reciept ,Adjustment,Usage will be entered for in lctemphdr file for each item within date range           

      SELECT(lcMat1)
      lcStyle = &lcMatJrnl..Style
      =SEEK(lcStyle,lcMat1)
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
      SCAN REST WHILE STYLE+DTOS(DTRDATE)+CSESSION = lcStyle   
      *SCAN WHILE STYLE+DTOS(DTRDATE)+CSESSION = lcStyle   
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        IF !SEEK(lcStyle,lcTempHdr) AND TTOD(&lcMat1..dTrDate) < ldlDate 
          *-- case transaction type  
          DO CASE
            CASE cTrType $ '56'   && po-receipt \po-Return
              lnBAL = lnBAL +IIF(cTrType='5',nTotStk,-nTotStk)
              lnPRV_REC  = lnPRV_REC + IIF(cTrType='5',nTotStk,-nTotStk)
            CASE cTrType ='1'   && adjustment
              lnBAL = lnBAL + nTotStk
            CASE cTrType $ '28'   && physical and Locking
              lnBAL = lnBAL +nTotStk
            CASE cTrType ='9'   && issue/return fabric in cuttkt
              lnBAL = lnBAL  + nTotStk
              lnPRV_USG = lnPRV_USG - nTotStk
          ENDCASE && end case transaction type  
        ENDIF 
      ENDSCAN  && end scan lcMatJrnl 
      lnNEWBAL   = lnBAL
      SELECT (lcMatJrnl)        
      IF !SEEK(lcStyle,lcTempHdr)  
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
        IF SEEK(lcStyle,lcFabric)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
        *HMA  06/19/2005 Open the Scale File Remotly [Begin]
        *INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
        *                        size3,size4,size5,size6,size7,size8,Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
        *                         VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
        *                         &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
        *                         &lcFabric..cStyGroup,&lcFabric..Fabric,SCALE.Sz1,SCALE.Sz2,SCALE.Sz3,SCALE.Sz4,;
        *                         SCALE.Sz5,SCALE.Sz6,SCALE.Sz7,SCALE.Sz8,lnPRV_REC,lnPRV_USG,lnBAL)


          IF llRpPrtSz && if print sizes breakDown
            SELECT ScaleR
            loScale.Seek('S'+&lcFabric..Scale)
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
                                  size3,size4,size5,size6,size7,size8,Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
                                   VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
                                   &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
                                   &lcFabric..cStyGroup,&lcFabric..Fabric,SCALER.Sz1,SCALER.Sz2,SCALER.Sz3,SCALER.Sz4,;
                                   SCALER.Sz5,SCALER.Sz6,SCALER.Sz7,SCALER.Sz8,lnPRV_REC,lnPRV_USG,lnBAL)
      
  		  SELECT(lcMatJrnl)
		  ELSE 
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,;
                                   Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
                                   VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
                                   &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
                                   &lcFabric..cStyGroup,&lcFabric..Fabric,lnPRV_REC,lnPRV_USG,lnBAL)
  
          ENDIF 
        ENDIF 
        *HMA  06/19/2005 Open the Scale File Remotly [End]			
      ENDIF 

      SELECT (lcMatJrnl)
      SCAN REST WHILE STYLE+DTOS(DTRDATE)+CSESSION = lcStyle  
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
		SELECT (lcTempHdr)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
        IF llRpPrtSz && if print sizes breakDown        
          FOR lncnt=1 TO 8
            lcCnt = STR(lnCnt,1)
            REPLACE Qty&lcCnt WITH Qty&lcCnt+&lcMatJrnl..nStk&lcCnt
          ENDFOR 
          REPLACE TotQty   WITH  TotQty+&lcMatJrnl..nTotStk
        ENDIF 
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
        SELECT (lcMatJrnl)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        DO CASE
          CASE cTrType $ '56'   && po-receipt \po-Return
            laTOTCLR(1)= laTOTCLR(1)+IIF(cTrType='5',nTotStk,-nTotStk)
            lnNEWBAL   = lnNEWBAL + IIF(cTrType='5',nTotStk,-nTotStk) 
            lnTotCost  = IIF(cTrType='5',nTotStk,-nTotStk) * nCost 
          CASE cTrType ='1'   && adjustment
            laTOTCLR(2)=laTOTCLR(2)+nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
          CASE cTrType $ '28'   && Physical\Locking
            lnNEWBAL   = lnNEWBAL + nTotStk
          CASE cTrType = '9'   && (usage)issue/return fabric in cuttkt
            laTOTCLR(3)=laTOTCLR(3)-nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
            lnTotCost  = - nTotStk * nCost 
          CASE cTrType = '3'  AND  !llRpPCnInv && Shipped
            laTOTCLR(4)=laTOTCLR(4)+nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
            lnTotCost  = - nTotStk * nCost               
          CASE cTrType = '3'  AND  llRpPCnInv && Total Shipped   ,Don't calculate the balance amount because it will be consolidated 
            laTOTCLR(4)=laTOTCLR(4)+nTotStk
        ENDCASE && end case transaction type  
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
		SELECT (lcTempHdr)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        REPLACE Po_Reciept   WITH  laTOTCLR(1) ,;
                Adjustment   WITH  laTOTCLR(2) ,;
                Usage        WITH  laTOTCLR(3) ,;
                Shipped      WITH  laTOTCLR(4)
 
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
        SELECT (lcMatJrnl)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        *--call Function to Collect Line Data 
        IF TTOD(&lcMatJrnl..dTrDate) >= ldlDate
          =lfCollLinData()
          lnBAL   = lnNEWBAL
        ENDIF 
      ENDSCAN 
      IF llRpPCnInv &&if consolidated by invoice
        =lfConsInv()
      ENDIF 

    ENDIF  &&END OF EMPTY DATE RANGE OR NOT


  ELSE  &&sort by Sequence 

    *-- if there is  no Sequence range is entered then we fill Header Data & quantities & totals of po-recipts or adjustments or 
    *-- usage for each item-color
    *-- in this case there is no values for previous reciepts,usage,or starting inventorey balance.
    IF EMPTY(lcSeqRngFox) 
      lcStyle = &lcMatJrnl..Style
      IF !SEEK(lcStyle,lcTempHdr)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
        IF SEEK(lcStyle,lcFabric)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
        *HMA  06/19/2005 Open the Scale File Remotly [Begin]
        *INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
        *                        size3,size4,size5,size6,size7,size8) VALUES(&lcFabric..Style,;
        *                        &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
        *                        &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric,;
        *                        SCALE.Sz1,SCALE.Sz2,SCALE.Sz3,SCALE.Sz4,SCALE.Sz5,SCALE.Sz6,SCALE.Sz7,SCALE.Sz8)
         IF llRpPrtSz && if print sizes breakDown
            SELECT ScaleR
            loScale.Seek('S'+&lcFabric..Scale)
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
                                size3,size4,size5,size6,size7,size8) VALUES(&lcFabric..Style,;
                                &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
                                &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric,;
                                SCALER.Sz1,SCALER.Sz2,SCALER.Sz3,SCALER.Sz4,SCALER.Sz5,SCALER.Sz6,SCALER.Sz7,SCALER.Sz8)
	  	  SELECT(lcMatJrnl)
  		ELSE 
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric) VALUES(&lcFabric..Style,;
                                &lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,&lcFabric..Loc,&lcFabric..Width,;
                                &lcFabric..Season,&lcFabric..Cdivision,&lcFabric..cStyGroup,&lcFabric..Fabric)


          ENDIF 
        *HMA  06/19/2005 Open the Scale File Remotly [End]			
        ENDIF 
      ENDIF      
      IF SEEK(lcStyle,lcTempHdr)
        SCAN REST WHILE STYLE+CSESSION = lcStyle  && To Collect Stock Qtys  per each Item-Color.
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
	  	  SELECT (lcTempHdr)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          IF llRpPrtSz && if print sizes breakDown
            FOR lncnt=1 TO 8
              lcCnt = STR(lnCnt,1)
              REPLACE Qty&lcCnt WITH Qty&lcCnt+&lcMatJrnl..nStk&lcCnt
            ENDFOR 
            REPLACE TotQty   WITH  TotQty+&lcMatJrnl..nTotStk
          ENDIF 
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
  	  	  SELECT (lcMatJrnl)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          DO CASE
            CASE cTrType $ '56'   && po-receipt \po-Return
              laTOTCLR(1)= laTOTCLR(1)+IIF(cTrType='5',nTotStk,-nTotStk)
              lnNEWBAL   = lnNEWBAL + IIF(cTrType='5',nTotStk,-nTotStk) 
              lnTotCost  = IIF(cTrType='5',nTotStk,-nTotStk) * nCost 
            CASE cTrType ='1'   && adjustment
              laTOTCLR(2)=laTOTCLR(2)+nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
            CASE cTrType $ '28'   && Physical\Locking
              lnNEWBAL   = lnNEWBAL + nTotStk
            CASE cTrType = '9'   && (usage)issue/return fabric in cuttkt
              laTOTCLR(3)=laTOTCLR(3)-nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
              lnTotCost  = - nTotStk * nCost 
            CASE cTrType = '3'  AND  !llRpPCnInv && Shipped
              laTOTCLR(4)=laTOTCLR(4)+nTotStk
              lnNEWBAL   = lnNEWBAL + nTotStk
              lnTotCost  = - nTotStk * nCost               
            CASE cTrType = '3'  AND  llRpPCnInv && Total Shipped   ,Don't calculate the balance amount because it will be consolidated 
              laTOTCLR(4)=laTOTCLR(4)+nTotStk
          ENDCASE && end case transaction type  
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
	  	  SELECT (lcTempHdr)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          REPLACE Po_Reciept   WITH  laTOTCLR(1) ,;
                  Adjustment   WITH  laTOTCLR(2) ,;
                  Usage        WITH  laTOTCLR(3) ,;
                  Shipped      WITH  laTOTCLR(4)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
  	  	  SELECT (lcMatJrnl)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          *--call Function to Collect Line Data 
          =lfCollLinData()
          lnBAL   = lnNEWBAL
        ENDSCAN 
        IF llRpPCnInv  &&if consolidated by invoice
          =lfConsInv()
        ENDIF 
      ENDIF 
      
    ELSE     &&there is sequence range selected
	*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]    
*!*	      *--Create Cursor from Item Journal File  without filter of Sequence Range.
*!*		  lcMat1=loogscroll.gftempname() 
*!*	  	  lcSelFld5= "SELECT ITEMJRNL.STYLE,ITEMJRNL.CTRTYPE,ITEMJRNL.CIRTYPE ,ITEMJRNL.DTRDATE,ITEMJRNL.CSESSION,ITEMJRNL.NTOTSTK;
*!*		                   FROM  ITEMJRNL WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"
*!*		  lnResult5 = loOGScroll.oRDA.SqlRun(lcSelFld5,lcMat1,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
*!*		  IF lnResult5 >=1
*!*		    SELECT (lcMat1)
*!*		    lnBuffering = CURSORGETPROP("Buffering",lcMat1)
*!*		    =CURSORSETPROP("Buffering",3,lcMat1)
*!*	        INDEX ON STYLE+CSESSION TAG lcMat1
*!*		    SET ORDER TO TAG lcMat1
*!*		  ENDIF  
	*B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]    
      *-- scan lcMatJrnl for the same fabric and same color and collect the data as follows:
      *-- Header Data such as desc,pattern ,Season,....etc., will be entered in lctemphdr file for each item when transaction Date included in Sequence Range 
      *-- previous Reciepts,Usage,Starting Inv Balance will be entered for in lctemphdr file for each item when Transction Sequence < Low entered Sequence Range
      *-- Qtys &Po_Reciept ,Adjustment,Usage will be entered for in lctemphdr file for each item within Sequence range           
      SELECT(lcMat1)
      lcStyle = &lcMatJrnl..Style
      lcSeqNo = &lcMatJrnl..cSession
      =SEEK(lcStyle,lcMat1)
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
      *SCAN WHILE STYLE+CSESSION = lcStyle   
      SCAN REST WHILE STYLE+CSESSION = lcStyle   
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
        IF !SEEK(lcStyle,lcTempHdr) AND &lcMat1..cSession < lcSeqNo
          *-- case transaction type  
          DO CASE
            CASE cTrType $ '56'   && po-receipt \po-Return
              lnBAL = lnBAL + IIF(cTrType='5',nTotStk,-nTotStk) 
              lnPRV_REC = lnPRV_REC + IIF(cTrType='5',nTotStk,-nTotStk) 
            CASE cTrType ='1'   && adjustment
              lnBAL = lnBAL + nTotStk
            CASE cTrType $ '28'   && physical and Locking
              lnBAL = lnBAL +nTotStk
            CASE cTrType ='9'   && issue/return fabric in cuttkt
              lnBAL = lnBAL  + nTotStk
              lnPRV_USG = lnPRV_USG - nTotStk
          ENDCASE && end case transaction type  
        ENDIF 
      ENDSCAN  && end scan lcMatJrnl 
      lnNEWBAL   = lnBAL
      SELECT (lcMatJrnl)        
      IF !SEEK(lcStyle,lcTempHdr)  
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin] 
        IF SEEK(lcStyle,lcFabric)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End] 
        *HMA  06/19/2005 Open the Scale File Remotly [Begin]
        *INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
        *                        size3,size4,size5,size6,size7,size8,Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
        *                         VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
        *                         &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
        *                         &lcFabric..cStyGroup,&lcFabric..Fabric,SCALE.Sz1,SCALE.Sz2,SCALE.Sz3,SCALE.Sz4,;
        *                         SCALE.Sz5,SCALE.Sz6,SCALE.Sz7,SCALE.Sz8,lnPRV_REC,lnPRV_USG,lnBAL)

          IF llRpPrtSz && if print sizes breakDown
            SELECT ScaleR
            loScale.Seek('S'+&lcFabric..Scale)
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,size1,size2,;
                                size3,size4,size5,size6,size7,size8,Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
                                 VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
                                 &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
                                 &lcFabric..cStyGroup,&lcFabric..Fabric,SCALER.Sz1,SCALER.Sz2,SCALER.Sz3,SCALER.Sz4,;
                                 SCALER.Sz5,SCALER.Sz6,SCALER.Sz7,SCALER.Sz8,lnPRV_REC,lnPRV_USG,lnBAL)

		    SELECT(lcMatJrnl)
  		ELSE 
            INSERT INTO &lcTempHdr(Style,Desc,Pattern,Vendor,Loc,Width,Season,Cdivision,cStyGroup,Fabric,;
                                 Privious_Reciepts,Privious_Usage,Start_Inv_Bal);
                                 VALUES(&lcFabric..Style,&lcFabric..Desc,&lcFabric..Pattern,&lcFabric..Vendor,;
                                 &lcFabric..Loc,&lcFabric..Width,&lcFabric..Season,&lcFabric..Cdivision,;
                                 &lcFabric..cStyGroup,&lcFabric..Fabric,lnPRV_REC,lnPRV_USG,lnBAL)

          ENDIF 
        *HMA  06/19/2005 Open the Scale File Remotly [End]	      
        ENDIF 
      ENDIF 
      SELECT (lcMatJrnl)
      SCAN REST WHILE STYLE+CSESSION = lcStyle 
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
    	SELECT (lcTempHdr)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        IF llRpPrtSz && if print sizes breakDown
          FOR lncnt=1 TO 8
            lcCnt = STR(lnCnt,1)
            REPLACE Qty&lcCnt WITH Qty&lcCnt+&lcMatJrnl..nStk&lcCnt
          ENDFOR 
          REPLACE TotQty   WITH  TotQty+&lcMatJrnl..nTotStk
        ENDIF 
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
    	SELECT (lcMatJrnl)
        *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        DO CASE
          CASE cTrType $ '56'   && po-receipt \po-Return
            laTOTCLR(1)= laTOTCLR(1)+IIF(cTrType='5',nTotStk,-nTotStk)
            lnNEWBAL   = lnNEWBAL + IIF(cTrType='5',nTotStk,-nTotStk) 
            lnTotCost  = IIF(cTrType='5',nTotStk,-nTotStk) * nCost 
          CASE cTrType ='1'   && adjustment
            laTOTCLR(2)=laTOTCLR(2)+nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
          CASE cTrType $ '28'   && Physical\Locking
            lnNEWBAL   = lnNEWBAL + nTotStk
          CASE cTrType = '9'   && (usage)issue/return fabric in cuttkt
            laTOTCLR(3)=laTOTCLR(3)-nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
            lnTotCost  = - nTotStk * nCost 
          CASE cTrType = '3'  AND  !llRpPCnInv && Shipped
            laTOTCLR(4)=laTOTCLR(4)+nTotStk
            lnNEWBAL   = lnNEWBAL + nTotStk
            lnTotCost  = - nTotStk * nCost               
          CASE cTrType = '3'  AND  llRpPCnInv && Total Shipped   ,Don't calculate the balance amount because it will be consolidated 
            laTOTCLR(4)=laTOTCLR(4)+nTotStk
          ENDCASE && end case transaction type  
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
	      SELECT (lcTempHdr)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
          REPLACE Po_Reciept   WITH  laTOTCLR(1) ,;
                  Adjustment   WITH  laTOTCLR(2) ,;
                  Usage        WITH  laTOTCLR(3) ,;
                  Shipped      WITH  laTOTCLR(4)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[Begin]
  	  	  SELECT (lcMatJrnl)
          *B127747 ,1 HMA  06/13/2005 Improve the performance of this report[End]
        *--call Function to Collect Line Data 
        IF &lcMatJrnl..cSession >= lcSeqNo
          =lfCollLinData()
          lnBAL   = lnNEWBAL
        ENDIF 
      ENDSCAN 
      IF llRpPCnInv && if consolidated by invoice
        =lfConsInv()
      ENDIF 

    ENDIF  &&END OF EMPTY Sequence RANGE OR NOT

  ENDIF  && end of select by date or Sequence range 
ENDSCAN && scan of lcMatJrnl1

*--Create Dbfs to link them to crystal Reports
SELECT(lcTempHdr)
COPY TO oAriaApplication.WorkDir+lcTempHdr+".dbf" WITH CDX
SELECT (lcTempLin)
SET ORDER TO lcTempLin1
COPY TO oAriaApplication.WorkDir+lcTempLin+".dbf" WITH CDX

*--If file is empty,then there is no records to display
SELECT (lcTempHdr)
LOCATE 
IF EOF()
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF 
*HMA  06/22/2005 Display Message of time & no of Records Colllected [Begin]
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW LANG_MAUTLJL_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(lcTempLin))) + LANG_MAUTLJL_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_MAUTLJL_SecondMsg  NOWAIT
*HMA  06/22/2005 Display Message of time & no of Records Colllected [End]

*--Create Tables into crystal.
DIMENSION loOGScroll.laCRTables[2]
loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempHdr  + ".DBF"
loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcTempLin  + ".DBF"



*--Passing Parameters to Crystal Report.
DIMENSION loOGScroll.laCRParams[5,2]

loOGScroll.laCRParams[1,1] = 'SortBy'
loOGScroll.laCRParams[1,2] = IIF(lcRPSortBy='D',LANG_MAUTLJL_Date,LANG_MAUTLJL_Sequence)
loOGScroll.laCRParams[2,1] = 'ReportName'
loOGScroll.laCRParams[2,2] = LANG_MAUTLJL_ReportTitle
loOGScroll.laCRParams[3,1] = 'lcRPSortBy'
loOGScroll.laCRParams[3,2] = lcRPSortBy
loOGScroll.laCRParams[4,1] = 'LLRPPRNDYE'
loOGScroll.laCRParams[4,2] = LLRPPRNDYE
loOGScroll.laCRParams[5,1] = 'llRpPrtSz'
loOGScroll.laCRParams[5,2] = llRpPrtSz
 


*--To Display report in PDF in landscape layout
loOgScroll.cCROrientation = 'L'
*B127748 ,1 HMA  05/04/2005 use letter Paper Size in America [Begin]
*loOgScroll.cCRPaperSize ='A4'
*B127748 ,1 HMA  05/04/2005 use letter Paper Size in America [End]
USE IN &lcTempHdr
USE IN &lcTempLin
*--Display the report.
DO gfDispRe

*!*************************************************************
*! Name      : lfCollLinData
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 12/06/2004
*! Purpose   : to collect the line data &fill the lcTempLin file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCollLinData()
*!*************************************************************

FUNCTION lfCollLinData
PRIVATE lnSelect,lnSelected

DO CASE 
    
  CASE cTrType $ '56'   && po-receipt \po-Return
    INSERT INTO &lcTempLin(Style,date,Location,Unit_cost,Item_Cost,Balance,NewBal,RcvAdjUsed,type) VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,&lcmatjrnl..nCost,lnTotCost,lnBal,lnNewBal,IIF(&lcmatjrnl..cTrType='5',&lcmatjrnl..nTotStk,-&lcmatjrnl..nTotStk),&lcMatJrnl..cTrType)
    SET ORDER TO lcMmfgordd IN &lcPosLin
    llMFO = (SEEK('P'+'F'+CRSESSION+cTrCode+STYLE+'2','&lcPosLin','lcMmfgordd'))
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
    *REPLACE &lcTempLin..TranType WITH IIF(!llMFO,IIF(EMPTY(CiSession),LANG_MAUTLJL_PORECEIPT,LANG_MAUTLJL_PORETURN),LANG_MAUTLJL_MFRECEIPT)
    lnSelect=SELECT(0)
    SELECT(lcTempLin)
    REPLACE TranType WITH IIF(!llMFO,IIF(EMPTY(&lcMatJrnl..CiSession),LANG_MAUTLJL_PORECEIPT,LANG_MAUTLJL_PORETURN),LANG_MAUTLJL_MFRECEIPT)
    SELECT(lnSelect)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
    *-- get some informations from purchase order fabric line for the fabric/color  
    *-- if found record for that fabric/color in POFLN
    SET ORDER TO lcPofLn IN &lcPosLin
    IF SEEK(STYLE+IIF(EMPTY(CiSession),'P','R')+'M'+cTrCode+'2','&lcPosLin','lcPofLn')
      lnSelected=SELECT(0) 
      SELECT (lcPosLin)   
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
      *LOCATE REST FOR &lcMatJrnl..cSession=cRsession;
      *       WHILE &lcMatJrnl..Style = Style
      *       REPLACE &lcTempLin..PO_CT   WITH &lcPosLin..PO ,;
      *             &lcTempLin..VENDOR1   WITH &lcPosLin..VENDOR ,;
      *             &lcTempLin..Seq_No    WITH IIF (lcrpSortby='S',&lcMatJrnl..CSESSION,'') ,;
      *             &lcTempLin..Reference WITH SUBSTR(&lcPosLin..REFERENCE,1,IIF(LLRPPRNDYE,11,22)) ,;      
      *             &lcTempLin..Dyelot    WITH IIF (LLRPPRNDYE,&lcPosLin..Dyelot,'') 
      SCAN REST WHILE &lcMatJrnl..Style = Style  FOR  &lcMatJrnl..cSession=cRsession
        lnSelect=SELECT(0)
		SELECT(lcTempLin)
        REPLACE PO_CT      WITH  &lcPosLin..PO ,;
                VENDOR1    WITH  &lcPosLin..VENDOR ,;
                Seq_No     WITH  IIF (lcrpSortby='S',&lcMatJrnl..CSESSION,'') ,;
                Reference  WITH  SUBSTR(&lcPosLin..REFERENCE,1,IIF(LLRPPRNDYE,11,22)) ,;      
                Dyelot     WITH  IIF (LLRPPRNDYE,&lcPosLin..Dyelot,'') 
        SELECT(lnSelect)
        SELECT(lnSelected)
      ENDSCAN 
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
    ELSE
      SET ORDER TO lcMmfgordd IN &lcPosLin
      IF SEEK('P'+'F'+crsession+cTrCode+STYLE+'2','&lcPosLin','lcMmfgordd')
	    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
        *REPLACE &lcTempLin..PO_CT     WITH &lcPosLin..PO ,;
        *        &lcTempLin..Seq_No    WITH IIF (lcrpSortby='S',&lcMatJrnl..CSESSION,'') ,;
        *        &lcTempLin..Reference WITH SUBSTR(&lcPosLin..REFERENCE,1,IIF(LLRPPRNDYE,11,22)) ,;      
        *        &lcTempLin..Dyelot    WITH IIF (llRpPrnDye,&lcPosLin..Dyelot,'') 
        lnSelect=SELECT(0)
		    SELECT(lcTempLin)
        REPLACE PO_CT     WITH &lcPosLin..PO ,;
                Seq_No    WITH IIF (lcrpSortby='S',&lcMatJrnl..CSESSION,'') ,;
                Reference WITH SUBSTR(&lcPosLin..REFERENCE,1,IIF(LLRPPRNDYE,11,22)) ,;      
                Dyelot    WITH IIF (llRpPrnDye,&lcPosLin..Dyelot,'') 	    
		    SELECT(lnSelect)
	    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]               
      ENDIF
    ENDIF 

  CASE cTrType='1'  && Adjustment
    INSERT INTO &lcTempLin(Style,date,Location,TranType,Reference,Seq_No,Dyelot,Balance,NewBal,RcvAdjUsed,type);
           VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,LANG_MAUTLJL_ADJUSTMENT,&lcmatjrnl..Reference,;
                   &lcmatjrnl..cSession,&lcmatjrnl..cDyelot,lnBal,;
                   lnNewBal,&lcmatjrnl..nTotStk,'1')

  CASE cTrType $ '28'  && Physical\Locking
    INSERT INTO &lcTempLin(Style,date,Location,TranType,Reference,Seq_No,Dyelot,Balance,NewBal,RcvAdjUsed,type);
           VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,IIF(&lcmatjrnl..cTrType='2',LANG_MAUTLJL_PHYSICAL,LANG_MAUTLJL_LOCKING),;
                   &lcmatjrnl..Reference,&lcmatjrnl..cSession,&lcmatjrnl..cDyelot,;
                   lnBal,lnNewBal,&lcmatjrnl..nTotStk,IIF(&lcmatjrnl..cTrType='2','2','8'))
       
  CASE cTrType='9'  && Usage (issue\return fabric)   
    INSERT INTO &lcTempLin(Style,date,Location,TranType,Seq_No,PO_CT,Unit_cost,Item_Cost,Balance,NewBal,RcvAdjUsed,type);
           VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,LANG_MAUTLJL_USAGE,&lcmatjrnl..cSession,&lcmatjrnl..ctrCode,;
                   &lcmatjrnl..nCost,lnTotCost,lnBal,lnNewBal,&lcmatjrnl..nTotStk,'9')
    lnCurAlias = SELECT(0)
    SELECT (lcMfgoprhd)
    *-- laContr : stores the contractors' values
    DIMENSION laContr[3]
    STORE SPACE(08) TO laContr
    lnInd  = 1
    *-- to get the required record from MFGOPRHD (ticket operation header);
    *-- we will join itemjrnl & poshdr files by  BUSDOCU+CSTYTYPE & itemjrnl.ctrcode=poshdr.po ,then join poshdr & mfgoprhd on poshdr.po = mfgoprhd.ctktno
    =SEEK(&lcPosHdr..PO)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]                   
    *SCAN WHILE CTKTNO+COPERSEQ = &lcPosHdr..PO  FOR !(&lcMfgoprhd..lInHouse)
    SCAN REST WHILE CTKTNO+COPERSEQ = &lcPosHdr..PO  FOR !(&lcMfgoprhd..lInHouse)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]               
      laContr[lnInd] = &lcMfgoprhd..CCONTCODE
      lnInd = lnInd+1 
      IF lnInd > 3     && to limit printing 3 contractors only
        EXIT
      ENDIF  
    ENDSCAN
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]                   
    *SELECT (lnCurAlias)
    *REPLACE &lcTempLin..Vendor1 WITH  laContr[1],;
    *        &lcTempLin..Vendor2 WITH  laContr[2],;
    *  		 &lcTempLin..Vendor3 WITH  laContr[3],; 
	*        &lcTempLin..Status  WITH  &lcPosHdr..Status + "/"
    SELECT(lcTempLin)
    REPLACE Vendor1 WITH  laContr[1],;
            Vendor2 WITH  laContr[2],;
   		 Vendor3 WITH  laContr[3],; 
		    Status  WITH  &lcPosHdr..Status + "/"
    SELECT (lnCurAlias)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]               
    *-- if there's cuttkt, print cuttktkh.style( now it's become&lcposHdr..Style) 
    IF &lcPosHdr..CBUSDOCU ='P' AND &lcPosHdr..CSTYTYPE='U'
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]                   
      *  REPLACE &lcTempLin..CutTktStyle WITH &lcPosHdr..STYLE ,;
      *  		   &lcTempLin..Dyelot      WITH &lcMatJrnl..cDyelot
      lnSelect=SELECT(0)
	  SELECT(lcTempLin)
      REPLACE CutTktStyle WITH &lcPosHdr..STYLE ,;
      	    Dyelot      WITH &lcMatJrnl..cDyelot
	  SELECT(lnSelect)
	  *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]               
    ENDIF  && end if there's cuttkt


  CASE cTrType='3' 
    IF !llRpPCnInv  && Shipped,not consolidated by invoice
      INSERT INTO &lcTempLin(Style,date,Location,TranType,Reference,Seq_No,Dyelot,Balance,NewBal,RcvAdjUsed,type);
             VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,LANG_MAUTLJL_SHIPPED,&lcmatjrnl..Reference,;
                     &lcmatjrnl..cSession,&lcmatjrnl..cDyelot,lnBal,;
                     lnNewBal,&lcmatjrnl..nTotStk,'3')
	  *B127747 ,1 HMA  06/13/2005 seek in  MINVHDR file remotely [Begin]
      *=SEEK(&lcMatJrnl..cTrCode,lcMInvHdr)
      lnSelect=SELECT(0)
      SELECT (lcMInvHdr)
      loMInvHdr.SEEK(&lcMatJrnl..cTrCode)
      *B127747 ,1 HMA  06/13/2005 seek in  MINVHDR file remotely [END]
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]    
	  *REPLACE  &lcTempLin..VENDOR1   WITH &lcMInvHdr..Account  
      SELECT(lcTempLin)
      REPLACE  VENDOR1   WITH &lcMInvHdr..Account  
      *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
      SELECT(lnSelect)
    ENDIF 
ENDCASE       


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 11/01/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Create a cursor from ItemLoc Sql File to calculate TotStk & WIP Quantities for each Item
IF oAriaApplication.ActiveModuleID = 'MA' .AND. !llFrstTime
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC " 
  lcSelFld1= lcSelFld1 +"  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 

  IF lnResult1 >=1
    lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
    =CURSORSETPROP("Buffering",3,lcFabDye)
    SELECT (lcFabDye)
    INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
  ENDIF 

  *--Create a cursor from ItemJrnl Sql File to browse activity sequence range .
  lcSelFld2= "SELECT ITEMJRNL.STYLE ,ITEMJRNL.CWARECODE,ITEMJRNL.CDYELOT ,ITEMJRNL.CSESSION  FROM ITEMJRNL " 
  lcSelFld2= lcSelFld2 +"  WHERE ITEMJRNL.CINVTYPE= '" +lcInvType+"'"
  lnResult2 = loOGScroll.orda.SqlRun (lcSelFld2,lcJrnl,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 

  IF lnResult2 >=1
    lnBuffering = CURSORGETPROP("Buffering",lcJrnl)
    =CURSORSETPROP("Buffering",3,lcJrnl)
    SELECT (lcJrnl)
    INDEX ON CSESSION+STYLE+CWARECODE+CDYELOT TAG lcJrnl
  ENDIF 
  llFrstTime = .T.
ENDIF 

*--Check dyelot option in MA Module Setup
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
LNPRNDYEP = ASUBSCRIPT(LOOGSCROLL.LAOGOBJTYPE,ASCAN(LOOGSCROLL.LAOGOBJTYPE,'LLRPPRNDYE'),1)
LOOGSCROLL.LAOGOBJCNT[LNPRNDYEP] = LLDYELOT                             
= LFOGSHOWGET('LLRPPRNDYE')

IF !llDyelot  
  llRpPrnDye = .F.
ENDIF

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()



*!*************************************************************
*! Name      : lfGetColor
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 11/01/2004
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************

FUNCTION lfGetColor
DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN


*!*************************************************************
*! Name      : lfSumFab1
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : sum a specific field for the current fabric 
*!                  in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,Item browse calculated fields.
*!*************************************************************
*! Passed Parameters  : (Item.CSTYMAJOR,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************

FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT(0)

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE 
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : IHB
*! Date      : 12/06/1998
*! Purpose   : To validate the sort by option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
ClearRead()


*!*************************************************************
*! Name      : lfSRVSeq
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSeq
PARAMETERS lcParm
IF llFrstTime
  SELECT ITEMJRNL
  INDEX ON CSESSION TAG SEQUENCE
  llFrstTime = !llFrstTime
ENDIF 
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT ITEMJRNL
    LOCATE 
    SET ORDER TO SEQUENCE
  CASE lcParm = 'R'  && Reset code
    SET ORDER TO 
ENDCASE
*-- end of lfsrvSty.



*!*************************************************************
*! Name      : lfBldSqlCur
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : Build Sql Cursors Needed For Getting Data
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : = lfBldSqlCur()
*!*************************************************************

FUNCTION lfBldSqlCur
PARAMETERS lcFilter,lcFound,lcCursor,lcFldName,lcSntFld
LOCAL   lnPosition,lnFltPos,lnRow,lcExpression
STORE 0 TO lnPosition,lnFltPos,lnRow
STORE '' TO lcExpression
lnFltPos = ASCAN(loOGScroll.laOGFxFlt,lcFilter)
IF lnFltPos > 0  
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnFltPos,1)
  lcTmpCur = loOGScroll.laOGFxFlt[lnRow,6]
  IF !EMPTY(lcTmpCur)  &&user selected some styles.
    SELECT &lcTmpCur
    &lcFound = (RECCOUNT() > 0) 
    IF &lcFound
      &lcCursor = loOgScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
      IF EMPTY(&lcCursor)
        *-- SQL connection Error. Can't open The Report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF 
ENDIF 
RETURN &lcCursor
*!*************************************************************
*! Name      : lfSlctFox
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 12/12/2004
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfSlctFox()
*!*************************************************************

FUNCTION lfSlctFox

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]
 
IF TYPE("loRDA1") <> 'O' 
  loRDA1 = CREATEOBJECT("RemoteDataAccess")
ENDIF 
lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM  " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
lnConnectionHandlar = loRDA1.sqlrun(lcSqlStatment,lcCursor,,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF

RETURN
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 12/12/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfCrtindex()
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE
   
  CASE UPPER(lcTable) = lcMInvHdr  
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CMATINV'
    laIndex[1,2] = 'LCMINVHDR'

  
ENDCASE

*!*************************************************************
*! Name      : lfConsInv
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 12/12/2004
*! Purpose   : function to calculate all shipped transactions 
*!             per each invoice
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfConsInv()
*!*************************************************************

FUNCTION lfConsInv
PRIVATE lnSelect,lnRec

lnSelect=SELECT(0)
lnRec=RECNO()
=SEEK(lcStyle,lcMatJrnl) &&to calculate total amount per one invoice 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
*SCAN WHILE STYLE+DTOS(DTRDATE)+CSESSION =lcStyle FOR ctrtype='3'
SCAN REST WHILE STYLE+DTOS(DTRDATE)+CSESSION =lcStyle FOR ctrtype='3'
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
  IF !SEEK(lcStyle+'3',lcTempLin)
    INSERT INTO &lcTempLin(Style,date,Location,TranType,Reference,Seq_No,Dyelot,RcvAdjUsed,Balance,type);
           VALUES (&lcmatjrnl..Style,TTOD(&lcmatjrnl..DTRDATE),&lcmatjrnl..cWareCode,LANG_MAUTLJL_TOTAL_SHIPPED,&lcmatjrnl..Reference,;
                   &lcmatjrnl..cSession,&lcmatjrnl..cDyelot;
                  ,&lcmatjrnl..nTotStk,lnBal,'3')
    *B127747 ,1 HMA  06/13/2005 seek in  MINVHDR file remotely [Begin]
	*=SEEK(&lcMatJrnl..cTrCode,lcMInvHdr)
	SELECT (lcMInvHdr)
	loMInvHdr.SEEK(&lcMatJrnl..cTrCode)
	*B127747 ,1 HMA  06/13/2005 seek in  MINVHDR file remotely [END]
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
    SELECT(lcTempLin)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
    REPLACE  &lcTempLin..VENDOR1   WITH &lcMInvHdr..Account  
  ELSE 
	*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
    SELECT(lcTempLin)
    *B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
    REPLACE &lcTempLin..RcvAdjUsed WITH &lcTempLin..RcvAdjUsed + &lcMatJrnl..nTotStk
  ENDIF 
  SELECT(lnSelect)
ENDSCAN 
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [Begin]
SELECT(lcTempLin)
REPLACE  &lcTempLin..NewBal WITH &lcTempLin..Balance + &lcTempLin..RcvAdjUsed
SELECT(lnSelect)
*B127747 ,1 HMA  06/13/2005 Improve the performance of this report [End]
lnBal=lnNewBal               
IF lnRec <> RECNO() AND !EOF()
  GOTO lnRec 
ENDIF 


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/22/2005
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
