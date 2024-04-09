*:***************************************************************************
*: Program file  : ALMSIMS
*: Program desc. : Download from M&S(Offshore Procurement Services)
*: System        : Aria4XP
*: Module        : AL
*: Developer     : Mariam MAzhar(MMT)
*: TRACKING      : C201037(Aria4) C201036(Aria27) [T20080327.0001]
*: Date          : 07/31/2008
*:***************************************************************************
lcMsg = "Please ensure that all outstanding Pick Tickets for M&S have been invoiced before running this program "
IF gfModalGen('QRM00000B00008',.F.,.F.,.F.,lcMsg ) <> 1
  RETURN 
ENDIF 

lcExpr = gfOpGrid('ALMSIMS' , .T.)
RETURN  

*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam MAzhar(MMT)
*! Date      : 07/31/2008
*! Purpose   : When Func. of Option grid
*!*************************************************************
FUNCTION lfwRepWhen
IF EMPTY(lcRpSpPath)
  lcRpSpPath = gfGetMemVar('M_ALMSSPDR')
endif 
IF EMPTY(lcRpHSPath )
  lcRpHSPath = gfGetMemVar('M_ALMSHSDR')
ENDIF  



*************************************************************
*! Name      : lfvPath
*! Developer : Mariam MAzhar(MMT)
*! Date      : 07/31/2008
*! Purpose   : Validate Path of file
*!*************************************************************
FUNCTION lfvPath

IF '?' $ lcRpSpPath
  lcSetPath = gfGetMemVar('M_ALMSSPDR')
  IF !EMPTY(lcSetPath) AND DIRECTORY(lcSetPath)
    lcDefaPath = SET("Default")
    SET DEFAULT TO (lcSetPath)
    lcRpSpPath = GETFILE('XLS')
    SET DEFAULT TO (lcDefaPath)
  ENDIF 
ENDIF    
*************************************************************
*! Name      : lfvHSPath
*! Developer : Mariam MAzhar(MMT)
*! Date      : 07/31/2008
*! Purpose   : Validate History Path of file
*!*************************************************************
FUNCTION lfvHSPath
IF '?' $ lcRpHSPath
  lcOldHSPath= gfGetMemVar('M_ALMSHSDR')
  IF !EMPTY(lcOldHSPath) AND DIRECTORY(lcOldHSPath)
    lcDefaPath = SET("Default")
    SET DEFAULT TO (lcOldHSPath)
    lcRpHSPath = GETDIR()
    SET DEFAULT TO (lcDefaPath)
  ENDIF 
ENDIF 
*************************************************************
*! Name      : lfGetCrt
*! Developer : Mariam MAzhar(MMT)
*! Date      : 07/31/2008
*! Purpose   : Get Option critria and process file
*!*************************************************************
FUNCTION lfGetCrt
IF EMPTY(lcRpSpPath) OR (!EMPTY(lcRpSpPath) AND !FILE(lcRpSpPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File to be Imported")
  RETURN .F.
ENDIF 

IF EMPTY(lcRpHSPath) OR (!EMPTY(lcRpHSPath) AND !DIRECTORY(lcRpHSPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid History Directory")
  RETURN .F.
ENDIF 

gfOpenTable('ORDHDR','ORDHDR')
gfOpenTable('ORDLINE','ORDLINES')
gfOpenTable('SPCK_LIN','SPCK_LIN')
gfOpenTable('STYLE','STYLE')
gfOpenTable('STYDYE','STYDYE')

DIMENSION laErrFlLog[3,4]

laErrFlLog[1,1] = 'UPC'
laErrFlLog[1,2] = 'C'
laErrFlLog[1,3] = 10
laErrFlLog[1,4] = 0

laErrFlLog[2,1] = 'cReason'
laErrFlLog[2,2] = 'C'
laErrFlLog[2,3] = 200
laErrFlLog[2,4] = 0

laErrFlLog[3,1] = 'LINENO'
laErrFlLog[3,2] = 'N'
laErrFlLog[3,3] = 5
laErrFlLog[3,4] = 0

lcErrorFile = loogscroll.gfTempName()
=gfCrtTmp(lcErrorFile ,@laErrFlLog,"Str(LINENO,5)+UPC",lcErrorFile ,.F.)

DIMENSION laXLSFile[8,4]

laXLSFile[1,1] = 'A'
laXLSFile[1,2] = 'C'
laXLSFile[1,3] = '30'
laXLSFile[1,4] = 0

laXLSFile[2,1] = 'B'
laXLSFile[2,2] = 'C'
laXLSFile[2,3] = '30'
laXLSFile[2,4] = 0

laXLSFile[3,1] = 'C'
laXLSFile[3,2] = 'C'
laXLSFile[3,3] = '30'
laXLSFile[3,4] = 0

laXLSFile[4,1] = 'D'
laXLSFile[4,2] = 'C'
laXLSFile[4,3] = '30'
laXLSFile[4,4] = 0

laXLSFile[5,1] = 'E'
laXLSFile[5,2] = 'C'
laXLSFile[5,3] = '30'
laXLSFile[5,4] = 0

laXLSFile[6,1] = 'F'
laXLSFile[6,2] = 'N'
laXLSFile[6,3] = '10'
laXLSFile[6,4] = 0

laXLSFile[7,1] = 'G'
laXLSFile[7,2] = 'N'
laXLSFile[7,3] = '10'
laXLSFile[7,4] = 0

laXLSFile[8,1] = 'H'
laXLSFile[8,2] = 'N'
laXLSFile[8,3] = '10'
laXLSFile[8,4] = 0

lcXLSFile = loogscroll.gfTempName()
=gfCrtTmp(lcXLSFile ,@laXLSFile,"A",lcXLSFile ,.F.)


DIMENSION laImFile[15,4]

laImFile[1,1] = 'Style'
laImFile[1,2] = 'C'
laImFile[1,3] = 19
laImFile[1,4] = 0

laImFile[2,1] = 'UPC'
laImFile[2,2] = 'C'
laImFile[2,3] = 10
laImFile[2,4] = 0

laImFile[3,1] = 'PIKDATE'
laImFile[3,2] = 'D'
laImFile[3,3] = 8
laImFile[3,4] = 0

laImFile[4,1] = 'CCALLOF'
laImFile[4,2] = 'C'
laImFile[4,3] = 6
laImFile[4,4] = 0

laImFile[5,1] = 'Stroke'
laImFile[5,2] = 'C'
laImFile[5,3] = 10
laImFile[5,4] = 0


laImFile[6,1] = 'QTY1'
laImFile[6,2] = 'N'
laImFile[6,3] = 6
laImFile[6,4] = 0

laImFile[7,1] = 'QTY2'
laImFile[7,2] = 'N'
laImFile[7,3] = 6
laImFile[7,4] = 0

laImFile[8,1] = 'QTY3'
laImFile[8,2] = 'N'
laImFile[8,3] = 6
laImFile[8,4] = 0

laImFile[9,1] = 'QTY4'
laImFile[9,2] = 'N'
laImFile[9,3] = 6
laImFile[9,4] = 0

laImFile[10,1] = 'QTY5'
laImFile[10,2] = 'N'
laImFile[10,3] = 6
laImFile[10,4] = 0

laImFile[11,1] = 'QTY6'
laImFile[11,2] = 'N'
laImFile[11,3] = 6
laImFile[11,4] = 0

laImFile[12,1] = 'QTY7'
laImFile[12,2] = 'N'
laImFile[12,3] = 6
laImFile[12,4] = 0

laImFile[13,1] = 'QTY8'
laImFile[13,2] = 'N'
laImFile[13,3] = 6
laImFile[13,4] = 0

laImFile[14,1] = 'TotQTY'
laImFile[14,2] = 'N'
laImFile[14,3] = 7
laImFile[14,4] = 0

laImFile[15,1] = 'LINENO'
laImFile[15,2] = 'N'
laImFile[15,3] = 5
laImFile[15,4] = 0

lcImpFile = loogscroll.gfTempName()
=gfCrtTmp(lcImpFile ,@laImFile,"Style+CCALLOF",lcImpFile ,.T.)


oAlObj =CREATEOBJECT("AL")
lopross = CREATEOBJECT('ariaprogressbar') 
SELECT (lcXLSFile)  
APPEND FROM (lcRpSpPath) XLS  
*IMPORT from (lcRpSpPath) XLS
LOCATE 
lcSpShName = ALIAS()
loPross.lblFirstLabel.Caption = 'Error checking'
loPross.TotalProgress = RECCOUNT(lcSpShName)
loPross.AutoCenter = .T.
loPross.Show()
lnPrepRec = 0  
lnCountErr = 1
SCAN FOR !EMPTY(D) AND RECNO() > 6
  lnPrepRec = lnPrepRec + 1
  loPross.CurrentProgress(lnPrepRec)
  llFound = .F.
  SELECT SPCK_LIN 
  gfSeek('S')
  SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'S' FOR ALLTRIM(&lcSpShName..D) $ RIGHT(ALLTRIM(PACK_ID),8)
    llFound = .T.
    lcSz =''
    FOR lnI = 1 TO 8
      lcI = STR(lnI ,1)
      IF SPCK_LIN.Qty&lcI. = 1
        lcSz = lcI
        EXIT  
      ENDIF 
    ENDFOR 
    
    IF !SEEK(SPCK_LIN.Style+SUBSTR(ALLTRIM(&lcSpShName..A),8,6),lcImpFile)
      INSERT INTO (lcImpFile) (STYLE,UPC,PIKDATE,CCALLOF,Stroke,LINENO)VALUES (SPCK_LIN.Style,&lcSpShName..D,CTOD(&lcSpShName..B),SUBSTR(ALLTRIM(&lcSpShName..A),8,6),&lcSpShName..C,RECNO(lcSpShName))
      REPLACE qty&lcSz. WITH &lcSpShName..H ,;
              totqty    WITH totqty + &lcSpShName..H IN (lcImpFile) 
    ELSE
      REPLACE qty&lcSz. WITH qty&lcSz. + &lcSpShName..H,;
              totqty    WITH totqty + &lcSpShName..H IN (lcImpFile)
    ENDIF 
      
    IF !gfSeek(SPCK_LIN.Style,'Ordline','ORDLINES')
	    INSERT INTO (lcErrorFile) VALUES (&lcSpShName..D,'Stroke '+&lcSpShName..C+' '+&lcSpShName..D+'- has no sales order in Aria - please correct and reimport',lnCountErr )      
      lnCountErr = lnCountErr +  1
  	ELSE
	    SELECT ordline 
	    LOCATE REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =SPCK_LIN.Style FOR CORDTYPE = 'O'
	    IF !FOUND()
		   INSERT INTO (lcErrorFile) VALUES (&lcSpShName..D,'Stroke '+&lcSpShName..C+' '+&lcSpShName..D+'- has no sales order in Aria - please correct and reimport',lnCountErr )      	    
       lnCountErr = lnCountErr +  1
	    ENDIF 
    ENDIF 
    EXIT 
  ENDSCAN
  IF !llFound 
    INSERT INTO (lcErrorFile) VALUES (&lcSpShName..D,'UPC-'+&lcSpShName..D+' is an invalid upc number for stroke '+&lcSpShName..C+'- please correct and reimport ',lnCountErr )
    lnCountErr = lnCountErr +  1
  ENDIF  
ENDSCAN 

loPross.Hide()


SELECT (lcErrorFile)
LOCATE 
IF !EOF()
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,"Some lines in the Spreadsheet are rejected. Do you want to print error log?") = 1
    *--Array hold Parameters will be sent to Crystal report 
    lcRepFile  = loogscroll.gftempname()
    loOGScroll.cCRPaperSize = 'A4'
    DIMENSION loOGScroll.laCRParams[2,2]
    loOGScroll.laCRParams[1,1] = 'lcFileName'
    loOGScroll.laCRParams[1,2] = lcRpSpPath
    
    loOGScroll.laCRParams[2,1] = 'lcUSerName'
    loOGScroll.laCRParams[2,2] = oAriaApplication.User_Name                        
    loogscroll.lcLogoPath = ''
    DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
    SELECT (lcErrorFile)
    COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF'
    loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF' 
    loOgScroll.lcOGLastForm = "ALMSIMS"
    
    loogScroll.cCROrientation = 'L'
    LoOGScroll.llCrystal = .T.
    loOGScroll.llPrintPDF = .F.
    oAriaApplication.gcDevice = 'FILE'
    loOGScroll.cTextRepType = 'PDF'
    oAriaApplication.gcOutFile = oAriaApplication.WorkDir+lcRepFile+'.PDF'
    =gfDispRe(oAriaApplication.ReportHome+"\AL\ALMSIMS")
    
    IF FILE( oAriaApplication.gcOutFile)
      loRun = CreateObject("WScript.Shell")
      loRun.Run( oAriaApplication.gcOutFile, 3)
      loRun = NULL
    ENDIF
    
    USE IN (lcSpShName)
    RETURN .F.
  ELSE
    USE IN (lcSpShName)
    RETURN .F.
  ENDIF 
ENDIF 
SELECT (lcImpFile)
SCAN FOR totqty > 0
  IF gfSeek(&lcImpFile..Style,'Ordline','ORDLINES')
    lcOrder = ''
    ldStartDate = {}
    llOpenFound = .F.
    SELECT ORDLINE
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =&lcImpFile..Style FOR ;
    	 			CORDTYPE = 'O' AND gfSeek('O'+Ordline.order,'ORDHDR') AND OrdHdr.Status = 'O'
    	 			
      IF EMPTY(lcOrder)
        lcOrder    = Ordline.Order
  	    ldStartDate = Ordhdr.Start
      ENDIF 
      
      IF lcOrder  <> Ordline.Order AND 	Ordhdr.Start < ldStartDate 			
        lcOrder    = Ordline.Order
  	    ldStartDate = Ordhdr.Start
  	  ENDIF   
  	  llOpenFound = .T.
    ENDSCAN 
    
    IF !llOpenFound
      ldStartDate = {}
      = gfSeek(&lcImpFile..Style,'Ordline','ORDLINES')
      SELECT ORDLINE
      SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =&lcImpFile..Style FOR ;
    	 			CORDTYPE = 'O' AND gfSeek('O'+Ordline.order,'ORDHDR') AND OrdHdr.Status = 'C'
	    	IF EMPTY(lcOrder)
          lcOrder    = Ordline.Order
          ldStartDate = Ordhdr.Start
        ENDIF     	 			
  	    IF lcOrder  <> Ordline.Order AND 	Ordhdr.Complete > ldStartDate 			
      	  lcOrder    = Ordline.Order
    	    ldStartDate = Ordhdr.Complete 
  	    ENDIF   
 	      llOpenFound = .T.
      ENDSCAN 
    ENDIF
     
    IF llOpenFound 
      SELECT Ordhdr 
      =gfSeek('O'+lcOrder)
      SELECT Ordline
      gfSetOrder('Ordline')
      =gfSeek('O'+lcOrder)
      LOCATE REST WHILE  CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder FOR Style = &lcImpFile..Style
      IF FOUND()
        IF EMPTY(Ordline.Piktkt)
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            lnQty = &lcImpFile..QTY&lcI.
            IF Ordline.Qty&lcI. < &lcImpFile..QTY&lcI.
              lnDiff = &lcImpFile..QTY&lcI. - Ordline.Qty&lcI. 
              SELECT Ordline 
              gfReplace('QTY&lcI. WITH  '+STR(lnQty,5))
              gfReplace('Book&lcI. WITH  '+STR(Book&lcI. + lnDiff ,5))
              gfReplace('TOTQTY   WITH  '+STR(TOTQTY + lnDiff ,5))
              gfReplace('totbook  WITH  '+STR(totbook + lnDiff ,5))
              SELECT ORDHDR
              gfReplace('book WITH  '+STR(book + lnDiff ,5))
              gfReplace('Open  WITH  '+STR(Open + lnDiff ,5))
            ENDIF 
            SELECT Ordline 
            gfReplace('PIK&lcI. WITH  '+STR(PIK&lcI.+lnQty,5))
            gfReplace('totPIK WITH  '+STR(totPIK+lnQty,5))
            gfReplace('picked WITH  .T.')
  		      gfReplace('pikdate WITH  CTOD("'+DTOC(&lcImpFile..PIKDATE)+'")')
  		      IF gfSeek(&lcImpFile..Style,'Style')
    		      SELECT Style 
   		        gfReplace('ALO&lcI. WITH '+STR(ALO&lcI.+lnQty,7))
   		        gfReplace('totalo WITH '+STR(totalo+lnQty,7))
   		        IF gfSeek(&lcImpFile..Style+Ordline.cWareCode,'STYDYE')
   		          SELECT STYDYE
                gfReplace('ALO&lcI. WITH '+STR(ALO&lcI.+lnQty,7))
	   		        gfReplace('totalo WITH '+STR(totalo+lnQty,7))
   		        ENDIF 
  		      ENDIF   
          ENDFOR  
          SELECT Ordline
          lcKeyLine = EVALUATE(KEY())
          SELECT OrdHDR
          lcKeyHDr =  EVALUATE(KEY()) 
          lcWareHouse = Ordline.cWareCode
          lcStoreName = Ordline.Store
          lcDivision  = Ordhdr .cDivision 
          lcOrderNo   = Ordhdr.Order
          SELECT Ordline
          gfTableUpdate()
          gfSeek(lcKeyLine)
          
          SELECT ORDHDR
          IF Ordhdr.Status = 'C'
            gfReplace("Status  WITH  'O'" )
          ENDIF 
          gfTableUpdate()
          gfSeek(lcKeyHDr)
          
          =gfOpenTable('PIKTKT','PIKTKT')
    		  lcPikTkt = oAlObj.lfGetPkTkt( lcOrderNo, lcDivision , lcStoreName  ,lcWareHouse,1)                
          m.PIKTKT = lcPikTkt
          m.Account = ORDHDR.Account
          m.store  = lcStoreName 
          m.Date = &lcImpFile..PIKDATE
          m.ccaloffno = &lcImpFile..CCALLOF
          m.order = lcOrderNo   
          m.cwarecode = lcWareHouse 
          m.custPO   = ORDHDR.custPO  
          m.status = 'O'
     		  SELECT PIKTKT 
          IF !gfSeek(lcPikTkt,'PIKTKT')
            gfAPPEND(' IN PIKTKT ',.T.) 
      		  gfTableUpdate()
          ENDIF   
    		  SELECT Ordline
    		  gfReplace("PIKTKT WITH '"+lcPikTkt+"'")
    		  gfTableUpdate()
          SELECT STYDYE
          gfTableUpdate()
          SELECT STYLE
    		  gfTableUpdate()
          
        ENDIF 
      ENDIF   
      SELECT Ordline
      gfSetOrder('ORDLINES')
    ENDIF 
  ENDIF 
ENDSCAN 
lcFileName = JUSTSTEM(lcRpSpPath )
lcExtEn = JUSTEXT(lcRpSpPath )
COPY FILE (lcRpSpPath) TO  (ADDBS(ALLTRIM(lcRpHSPath))+lcFileName+"."+lcExtEn ) 
ERASE (lcRpSpPath) 
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'File '+ALLTRIM(lcRpSpPath) +' is processed successfully')
  

