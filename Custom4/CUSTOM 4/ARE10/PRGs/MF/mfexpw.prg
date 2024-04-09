*:****************************************************************
*: Program file  : MFEXPWIP.PRG
*: Program desc. : Custom WIP Export Report for ARE10
*: System        : Aria Apparel System - Version 4XP
*: Module        : MF
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 04/12/2017 (C202002){P20170404.0001}
*:****************************************************************
=gfCallForm('MFEXPWIP','MF')
*!*************************************************************
*! Name      : lfvgetFile
*: Developer : Mariam Mazhar- [MMT]
*: Date      : 04/12/2017
*! Purpose   : Function to get the Excel file Path
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvgetFile()
*!*************************************************************
FUNCTION lfvgetFile
PRIVATE lcOld_Path

lcOld_Path = FULLPATH('')

lcPathName  = GETFILE('XLSX', 'Excel sheet Path : ','Create') 

SET DEFAULT TO &lcOld_Path

IF EMPTY(lcPathName)
  RETURN ''  	
ELSE
  RETURN lcPathName 
ENDIF
*!*************************************************************
*! Name      : lfvProceed
*: Developer : Mariam Mazhar- [MMT]
*: Date      : 04/12/2017
*! Purpose   : Function to Export data and collect data
*!*************************************************************
FUNCTION lfvProceed
PARAMETERS lcOutputFile,loFormSet
IF !USED("POSHDR")
  =gfOpenTable("POSHDR","POSHDR",'SH')
ENDIF  

IF !USED("POSHDR_A")
  =gfOpenTable("POSHDR","POSHDR",'SH','POSHDR_A')
ENDIF  

IF !USED("POSLN")
  =gfOpenTable("POSLN","POSLN",'SH')
ENDIF  

IF !USED("POSLN_A")
  =gfOpenTable("POSLN","POSLN",'SH','POSLN_A')
ENDIF

IF !Used("NOTEPAD")
  =gfOpenTable("NOTEPAD","NOTEPAD",'SH')
ENDIF
IF !Used("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR",'SH')
ENDIF
IF !Used("CUTPICK")
  =gfOpenTable("CUTPICK","CUTPICK",'SH')
ENDIF
IF !Used("MFGOPRHD")
  =gfOpenTable("MFGOPRHD","MFGOPRHD",'SH')
ENDIF

IF !Used("MFGOPRDT")
  =gfOpenTable("MFGOPRDT","MFGOPRDT",'SH')
ENDIF



IF !Used("BOM")
  =gfOpenTable("BOM","MULTIBOM",'SH')
ENDIF

IF !Used("ITEM")
  =gfOpenTable("ITEM","STYLE",'SH')
ENDIF

lnMjrWid   = LEN(gfItemMask('PM',"",'0001'))
lnMAMjrWid   = LEN(gfItemMask('PM',"",'0002'))
STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos 
  
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
    CASE laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lcClrSpr = ALLT(laItemSeg[lnCount,6])    
    ENDCASE  
ENDFOR

lcTmpCursor = gfTempName()
CREATE CURSOR (lcTmpCursor) (TOP C(10),contrator C(8),CXL_Date D,Customer C(5),Lot_NO C(6),WO C(10),Cust_PO C(15),Style C(19),Color C(6),Orig_Qty N(12),;
                       Cut_Qty N(12),Cut_Date D,Qty_Received N(12),Date_Received D,Open_Wip_Qty N(12),Fabric_PO C(6),;
                        Fabric_Due D,Fabric_Received D, Fabric_Vendor C(8),Pattern C(10),Trims C(60),Notes C(254))
                       

SELECT POSHDR
=gfSeek("PU")
SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO ='PU' FOR !Status $ "CSX"    
  WAIT WINDOW  "Collecting data for Cutting ticket# "+POSHDR.PO NOWAIT     
  =gfSeek('I'+POSHDR.PO,'NotePad','NotePad')
  ldRecMADate = {}
  IF !EMPTY(POSHDR.MAPO)and !ISNULL(POSHDR.MAPO)
    IF gfSeek('PM'+POSHDR.MAPO,'POSHDR_A')
      
      IF gfSeek(POSHDR_A.CBUSDOCU+POSHDR_A.CSTYTYPE+POSHDR_A.PO+"0002",'POSLN_A','POSLN')
         ldRecMADate =  CTOD("01/01/1900")
         SELECT POSLN_A
         SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=;
                                      POSHDR_A.CBUSDOCU+POSHDR_A.CSTYTYPE+POSHDR_A.PO+"0002" FOR TRANCD ='2'
           IF ldRecMADate < POSLN_A.Date
             ldRecMADate = POSLN_A.Date 
           ENDIF                             
         ENDSCAN 
       ENDIF 
     ENDIF   
  ENDIF  
  IF ldRecMADate ==  CTOD("01/01/1900")
    ldRecMADate = {}
  ENDIF
  SELECT POSLN
  IF gfSeek('PU'+POSHDR.PO)
    SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = 'PU'+POSHDR.PO FOR TRANCD ='1'
      SELECT (lcTmpCursor)
      APPEND BLANK 
      REPLACE TOP           WITH IIF(ISNULL(POSHDR.TOP),'',POSHDR.TOP) ,;
              Pattern       WITH POSHDR.Pattern ,;
              CXL_Date      WITH POSHDR.Complete,;
              Lot_NO        WITH POSHDR.PO,;
              WO            WITH IIF(ISNULL(POSHDR.WONUM),'',POSHDR.WONUM),;
              Notes         WITH NotePad.mNotes,;
              Fabric_PO     WITH IIF(ISNULL(POSHDR.MAPO),'',POSHDR.MAPO),;
              Fabric_Due    WITH POSHDR_A.Complete,;
              Fabric_Vendor WITH POSHDR_A.Vendor,;
              Fabric_Received WITH ldRecMADate ,;
              Cut_Date WITH POSHDR.Act_Date
              
     lcContractor = ''         
     IF gfSeek('M'+POSHDR.PO+'SEW01 ','MFGOPRHD','MFGOPRHD')         
       lcContractor = MFGOPRHD.cContCode 
     ENDIF
     
     REPLACE Style     WITH SUBSTR(POSLN.Style,1,lnMjrWid),;
             Color     WITH SUBSTR(POSLN.STYLE,lnClrPos,lnClrLen),;
             Orig_Qty  WITH POSLN.TotQty ,;
             contrator WITH lcContractor  IN (lcTmpCursor)
             
     IF gfSeek('1'+POSHDR.PO+STR(POSLN.LineNo,6),'CUTPICK','CUTPKORD')
       =gfSeek('O'+CUTPICK.ORDER,'ORDHDR','ORDHDR')
       REPLACE Cust_PO WITH ORDHDR.CUSTPO,;
               Customer WITH ORDHDR.Account  IN (lcTmpCursor)
     ENDIF
     IF gfSeek(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'2','POSLN_A','POSLN')
       SELECT POSLN_A
       SUM TotQty TO lnQty REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=;
                                      POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'2'
       REPLACE Qty_Received WITH lnQty IN (lcTmpCursor)
       ldRecDate = CTOD("01/01/1900")
       SELECT POSLN_A
       =Seek(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'2','POSLN_A','POSLN')
       SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=;
                                      POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'2'
         IF ldRecDate < POSLN_A.Date
           ldRecDate = POSLN_A.Date 
         ENDIF                             
       ENDSCAN 
       IF ldRecDate ==  CTOD("01/01/1900")
         ldRecDate = {}
       ENDIF
       REPLACE Date_Received WITH ldRecDate IN (lcTmpCursor)
     ENDIF         
     lnDamaged = 0
     IF gfSeek(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+"4",'POSLN_A','POSLN')
       SELECT POSLN_A
       SUM TotQty TO lnDamaged REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=;
                                      POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'4'
     ENDIF
     lnCancelled = 0
     IF gfSeek(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+"5",'POSLN_A','POSLN')
       SELECT POSLN_A
       SUM TotQty TO lnCancelled REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=;
                                      POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO+POSLN.CINVTYPE+POSLN.STYLE+STR(POSLN.LINENO,6)+'5'
     ENDIF
     REPLACE Open_Wip_Qty WITH Orig_Qty - lnCancelled - lnDamaged - Qty_Received  IN (lcTmpCursor)         
     lcTrims = ''
     IF gfSeek('0001'+PADR(SUBSTR(POSLN.Style,1,lnMjrWid),19)+'M'+POSLN.ccstsht_id,'BOM')        
       SELECT BOM
       SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
				       '0001'+PADR(SUBSTR(POSLN.Style,1,lnMjrWid),19)+'M'+POSLN.ccstsht_id   FOR ccatgtyp ='T' AND ;
				       IIF(SUBSTR(BOM.cItmMask,lnClrPos,lnClrLen)= REPLICATE('*',lnClrLen) ,.T.,SUBSTR(BOM.cItmMask,lnClrPos,lnClrLen)=SUBSTR(POSLN.Style,lnClrPos,lnClrLen))
		IF gfSeek('0002'+SUBSTR(BOM.ITEM,1,lnMAMjrWid),'ITEM')		       
    	  lcTrims = lcTrims + ","+ALLTRIM(ITEM.Desc)
		ENDIF		       
      ENDSCAN				       
     ENDIF
     IF !EMPTY(lcTrims)
       REPLACE Trims WITH SUBSTR(lcTrims,2) IN (lcTmpCursor)
     ENDIF
     IF gfSeek('M'+POSHDR.PO+'SEW01 ','MFGOPRDT','MFGOPRDT')
       SELECT MFGOPRDT
       SUM nLotTotQty TO lnIssued REST WHILE CIMTYP+CTKTNO+COPRCODE+CLOTNO+TRANCD = 'M'+POSHDR.PO+'SEW01 ' FOR trancd ='1' AND Item = POSLN.STYLE
       REPLACE Cut_Qty WITH lnIssued  IN (lcTmpCursor)
     ENDIF
     SELECT POSLN
    ENDSCAN
  ENDIF 
  SELECT POSHDR
ENDSCAN

WAIT CLEAR 
SELECT  (lcTmpCursor)
LOCATE 
IF !EOF()

  ***
  lcTmpFile = oAriaApplication.workDir+gfTempName()+".XLS" 
  COPY TO &lcTmpFile TYPE XL5
  lcXTmpFile = FORCEEXT(lcTmpFile ,".XLSX")
  oExcelSheet = CREATEOBJECT('Excel.Application') 
  loFileExcel =oExcelSheet.Workbooks.Open (lcTmpFile)
  loSheet = loFileExcel.Sheets [1]
  loSheet.Range ("A1")= "TOP"
  loSheet.Range ("B1")= "CONTRACTOR"
  loSheet.Range ("C1")= "CXL DATE"
  loSheet.Range ("D1")= "CUSTOMER"
  loSheet.Range ("E1")= "LOT#"
  loSheet.Range ("F1")= "WO"
  loSheet.Range ("G1")= "CUST PO"
  loSheet.Range ("H1")= "STYLE"
  loSheet.Range ("I1")= "COLOR"
  loSheet.Range ("J1")= "ORIG QUANTITY"
  loSheet.Range ("K1")= "CUT QUATITY"
  loSheet.Range ("L1")= "CUT DATE "
  loSheet.Range ("M1")= "QTY RECEIVED "
  loSheet.Range ("N1")= "DATE RECEIVED"
  loSheet.Range ("O1")= "Open WIP Qty"
  loSheet.Range ("O1").Interior.ColorIndex = 4
  loSheet.Range ("P1")= "FABRIC PO "
  loSheet.Range ("Q1")= "FABRIC DUE"              
  loSheet.Range ("R1")= "FABRIC RECEIVED "              
  loSheet.Range ("S1")= "FABRIC VENDOR"              
  loSheet.Range ("T1")= "PATTERN"              
  loSheet.Range ("U1")= "TRIMS"                      
  loSheet.Range ("V1")= "NOTES"    
  loFileExcel.SaveAs(lcXTmpFile,51)
  loFileExcel.Close (.t.)
  oExcelSheet = NULL
  loFileExcel = NULL
  lcTmpFile  = lcXTmpFile
  IF FILE(lcTmpFile)
     COPY FILE (lcTmpFile) TO (lcOutputFile) 
     =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+lcOutputFile+" has been created successfully")
     loFormSet.Release()
  ENDIF
ELSE
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No records to export")
  loFormSet.Release()
ENDIF  