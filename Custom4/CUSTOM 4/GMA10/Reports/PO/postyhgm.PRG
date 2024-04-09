*:***************************************************************************
*: Program file    : POSTYHGM
*: Program desc.   : P/O Header report for GMA. (C038207)
*: For Report      : 
*: System          : Aria Advantage Series.
*: Module          : STYLE PURCHASE ORDER (PO)
*: Developer       : NEVEEN FAROUK (NFZ)
*:***************************************************************************
*
LOCAL loRDA ,lnResult

IF loOgScroll.llOGFltCh 
  *---- Collect Data From PosHdr and PosLn & Creat Cursor LcPosHdr
  =lfColIData()           
  SELECT lcPosHdr
  IF RECCOUNT()=0  
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF
  *--- Create Temp Table & Replace Data From Cursor To Table
  DO lfCrtTable  
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

DECLARE loOgScroll.lacrparams[5,2]   && array For Param
loOgScroll.lacrparams[1,1]='OPTITLE'
loOgScroll.lacrparams[1,2]=M.LCRPTITLE
loOgScroll.lacrparams[2,1]='RP_NOTPAD'
loOgScroll.lacrparams[2,2]=M.LCRPNOTE
loOgScroll.lacrparams[3,1]='RP_SORT'
loOgScroll.lacrparams[3,2]=M.lcRPSortBy
loOgScroll.lacrparams[4,1]='SORTBY'
**---- Do Case For Param. Sort By (Description)
DO CASE 
	CASE M.lcRPSortBy='P' 
	  loOgScroll.lacrparams[4,2]='P/O #'
	CASE M.lcRPSortBy='V'
	  loOgScroll.lacrparams[4,2]='Vendor'
	CASE M.lcRPSortBy='W'
	  loOgScroll.lacrparams[4,2]='Location'
	CASE M.lcRPSortBy='D'
	  loOgScroll.lacrparams[4,2]='Date Completed'
ENDCASE
loOgScroll.lacrparams[5,1]='ReportName'
loOgScroll.lacrparams[5,2]='STYLE PURCHASE ORDER HEADER REOPRT '

SELECT (lcWorkFile)
DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcWorkFile+'.DBF'
SELECT (lcWorkFile)
*COPY TO R:\ARIA4XP\WORK\PO_test
USE IN (lcWorkFile)
RELEASE loRDA ,lnResult       
=gfDispRe()
**************************
*-- end of code.
*!*************************************************************
*! Name      : lfColIData
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 03/03/2002
*! Purpose   : Function to collect Data
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColIData()
*!*************************************************************

FUNCTION lfColIData
**********
IF TYPE("loRDA") <> 'O' 
  loRDA = CREATEOBJECT("RemoteDataAccess")
ENDIF 
*-- The following fields are used as a default 
lnResult  = loRDA.sqlrun("SELECT  posln.po,poshdr.available,posln.CWARECODE,POSLN.TotQty ,"+;
                                 "POSHDR.status,POSLN.TRANCD ,POSHDR.ENTERED,POSHDR.COMPLETE,"+;
                                 "POSHDR.VENDOR,POSHDR.SEASON, POSHDR.CDIVISION "+;
                                 "FROM POSHDR POSHDR (INDEX = POSHDR) Inner join POSLN POSLN (INDEX = POSLNS)"+;
                                 " ON POSHDR.cbusdocu =POSLN.cbusdocu and  POSHDR.CSTYTYPE = POSLN.CSTYTYPE and POSHDR.PO = POSLN.PO  " + ;
                                 " WHERE "+ loOgScroll.lcrpsqlexp +"order by posln.po",+;
                    			 "lcPosHdr",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",) 

SELECT lcPosHdr
*brow
RETURN 
*!*************************************************************
*! Name      : lfCrtTable
*: Developer : Neveen Farouk (nfz)
*! Date      : 21/07/2004
*! Purpose   : Function to create temp. Table For Crystal Report 
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTable()
*!*************************************************************
*---------------------
FUNCTION lfCrtTable
*-------------
SELECT lcPosHdr
=AFIELD(laCurName)   && Copy cursor struct for to array
lnFileStru = ALEN(laCurName,1)
** add 9 field to temp file 
DIMENSION laCurName[lnFileStru+9,18]
LaCurName[lnFileStru+1,1]  = 'Receive'
laCurName[lnFileStru+2,1]  = 'Open'
laCurName[lnFileStru+3,1]  = 'Damage'
laCurName[lnFileStru+4,1]  = 'Cancel'
laCurName[lnFileStru+5,1]  = 'nIn_Trans'
laCurName[lnFileStru+6,1]  = 'nStyOrder'
laCurName[lnFileStru+7,1]  = 'EmpNots'
laCurName[lnFileStru+8,1]  = 'CVENCOMP'
laCurName[lnFileStru+9,1]  = 'P_NOTES'


FOR lnLoop = 1  TO 9 && CREATE THE ARRAY COLUMN FROM 2 TO 18
  DO CASE 
  CASE lnLoop<8
     laCurName[lnFileStru+lnLoop,2] = 'N'
     laCurName[lnFileStru+lnLoop,3] = 13
     laCurName[lnFileStru+lnLoop,4] = 3
     
  CASE lnLoop=8
    laCurName[lnFileStru+lnLoop,2] = 'C'
    laCurName[lnFileStru+lnLoop,3] = 30
    laCurName[lnFileStru+lnLoop,4] = 0
  CASE  lnLoop=9
    laCurName[lnFileStru+lnLoop,2] = 'M'
    laCurName[lnFileStru+lnLoop,3] = 0
    laCurName[lnFileStru+lnLoop,4] = 0
  ENDCASE
    
  STORE 0 TO laCurName[lnFileStru+lnLoop,17],;
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
SELECT (lcWorkFile) 
m.empNots=0  && Flag in Temp Table Indicate to the memo empty or not for Crystal Report
SELECT lcPosHdr
SCAN
  SELECT lcPosHdr
  DO WHILE &lcWorkFile..po =lcPosHdr.po AND IIF(M.lcRPSortBy='W',(&lcWorkFile..cWareCode =lcPosHdr.cWareCode),.T.)
    SELECT lcPosHdr
    DO LpCalcData
    SELECT lcPosHdr
	IF !EOF() AND &lcWorkFile..PO =lcPosHdr.PO
	  SKIP
	ELSE
	  EXIT
	ENDIF
  ENDDO  
  SCATTER MEMVAR MEMO
  **---- Search in apvendor file to replace the vendor data
*!*	  SELECT ApVendor 
*!*	  IF SEEK(lcPosHdr.Vendor)
*!*	    *---- Save the address for the vendor
*!*	    m.cVENCOMP= apvendor.CVENCOMP
*!*	  ENDIF 
  *--- 'Y' For Print NotePade
  IF M.LCRPNOTE='Y'   
    SELECT NotePad
    IF SEEK('P'+lcPosHdr.PO)
      m.p_notes=notepad.MNOTES
      m.empNots=IIF(EMPTY(notepad.MNOTES),'0','1')
    ELSE
      m.p_notes=""
      m.empNots='0'
    ENDIF 
  ENDIF
  SELECT lcPosHdr  
  IF !EOF()
    SELECT (lcWorkFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
    SELECT lcPosHdr
    DO LpCalcData
  ENDIF
ENDSCAN
SELECT (lcWorkFile)
RETURN

*!*************************************************************
*! Name      : LpCalcData
*: Developer : Neveen Farouk (nfz)
*! Date      : 21/07/2004
*! Purpose   : Function to Calc. the total of open ,Cancel,damege 
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = LpCalcData()
*!*************************************************************

PROCEDURE LpCalcData
*-----------------
SELECT (lcWorkFile)
DO CASE
	CASE lcPosHdr.TRANCD = '1'    && Order
	  REPLACE &lcWorkFile..nStyOrder  WITH &lcWorkFile..nStyOrder+lcPosHdr.TotQty,;
	          &lcWorkFile..Open       WITH &lcWorkFile..Open+lcPosHdr.TotQty

	CASE lcPosHdr.TRANCD= '2'     && Receive
	  REPLACE &lcWorkFile..Receive  WITH &lcWorkFile..Receive+lcPosHdr.TotQty,;
	          &lcWorkFile..Open     WITH &lcWorkFile..Open - lcPosHdr.TotQty

	CASE lcPosHdr.TRANCD= '3'     && trans
	  REPLACE &lcWorkFile..nIn_Trans  WITH &lcWorkFile..nIn_Trans +lcPosHdr.TotQty

	CASE lcPosHdr.TRANCD= '4'    && Damage
	  REPLACE &lcWorkFile..Damage  WITH &lcWorkFile..Damage +lcPosHdr.TotQty,;
	          &lcWorkFile..Open    WITH &lcWorkFile..Open - lcPosHdr.TotQty

	CASE lcPosHdr.TRANCD= '5'     && Cancel
	  REPLACE &lcWorkFile..Cancel WITH &lcWorkFile..Cancel+lcPosHdr.TotQty,;
	          &lcWorkFile..Open   WITH &lcWorkFile..Open - lcPosHdr.TotQty

ENDCASE        
RETURN
