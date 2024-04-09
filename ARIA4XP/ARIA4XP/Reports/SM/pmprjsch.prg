*!*****************************************************************************************
*! Name      : PMPRJSCH.prg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/10/2009 
*! Purpose   : Scheduling From Request builder
*! Entry no. : N037574 
*!*****************************************************************************************
FUNCTION LFWHENFN
DIMENSION laPrjDes[2],laPrjVal[2]
STORE '' TO laPrjDes,laPrjVal
laPrjDes[1] = 'Style'
laPrjVal[1] = 'S'
laPrjDes[2] = 'Other'
laPrjVal[2] = 'H'
lnValCnt = 3 

IF 'MF' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+3],laPrjVal[ALEN(laPrjVal)+3]
  laPrjDes[lnValCnt] = 'Cutting Ticket'
  laPrjVal[lnValCnt] = 'C'
  lnValCnt = lnValCnt + 1
  laPrjDes[lnValCnt] = 'Adornment Order'
  laPrjVal[lnValCnt] = 'A'
  lnValCnt = lnValCnt + 1
  laPrjDes[lnValCnt] = 'Dye Order'
  laPrjVal[lnValCnt] = 'D'
  lnValCnt = lnValCnt + 1
ENDIF  

IF 'MA' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+1],laPrjVal[ALEN(laPrjVal)+1]
  laPrjDes[lnValCnt] = 'Material'
  laPrjVal[lnValCnt] = 'M'
  lnValCnt = lnValCnt + 1
  =gfOpenTable("ITEM",'Cstyle')
ENDIF 

IF 'PO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+3],laPrjVal[ALEN(laPrjVal)+3]
  laPrjDes[lnValCnt] = 'Purchase Order'
  laPrjVal[lnValCnt] = 'P'
  lnValCnt = lnValCnt + 1
  laPrjDes[lnValCnt] = 'Inter-Location P/O'
  laPrjVal[lnValCnt] = 'N'
  lnValCnt = lnValCnt + 1
  laPrjDes[lnValCnt] = 'Return P/O'
  laPrjVal[lnValCnt] = 'R'
  lnValCnt = lnValCnt + 1
ENDIF  

IF 'SO' $ oAriaApplication.CompanyInstalledModules
  DIMENSION laPrjDes[ALEN(laPrjDes)+2],laPrjVal[ALEN(laPrjVal)+2]
  laPrjDes[lnValCnt] =  'Sales Order'
  laPrjVal[lnValCnt] = 'O'
  lnValCnt = lnValCnt + 1
  laPrjDes[lnValCnt] = 'EDI temporary Order '
  laPrjVal[lnValCnt] = 'T'
  lnValCnt = lnValCnt + 1
ENDIF  

IF (TYPE('llSchedule') <> 'U' AND  llSchedule) AND (TYPE('lcPrj_Typ') <> 'U') AND (TYPE('lcXmlFlName') <> 'U')
  lcRpPrjTyp = lcPrj_Typ
  ldRpSchDt = dSch_Date
  lcTempPrj = loogscroll.gfTempName()
  CREATE CURSOR (lcTempPrj ) (KeyExp C(20))
  INSERT INTO (lcTempPrj ) VALUES (PADR(lcPrj_ID,6)+'-'+lcStyle)
  lnPrjId= ASCAN(loOgScroll.laOgFXFlt,"PMPRJHD.CPRJ_ID")
  IF lnPrjId > 0 
    lnPrjId = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPrjId,1)
    loOgScroll.laOgFxFlt[lnPrjId,6] = lcTempPrj 
  ENDIF 
  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+lcXmlFlName+'.xml')
ENDIF 

*!*************************************************************
*! Name      : lfVPrjType
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Validate Project type
*!*************************************************************
FUNCTION lfVPrjType

lnPrjId= ASCAN(loOgScroll.laOgFXFlt,"PMPRJHD.CPRJ_ID")


IF lnPrjId > 0 
  lnPrjId = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPrjId,1)
  lcCursorPrj= loOgScroll.laOgFxFlt[lnPrjId,6]
  SELECT(lcCursorPrj) 
  ZAP 
  clearread()
ENDIF       
