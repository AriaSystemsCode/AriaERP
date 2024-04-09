*:**********************************************************************
*: Program file       : POTRKSHP.PRG
*: Program description: Tracking Shipment Report
*: Module             : Style Purchase Order (PO)
*: Developer          : Omnia MOhammed Mostafa  (OMM)
*: Tracking Job Number: N#C127107
*: Date               : 10/10/2005
*:**********************************************************************
*: Calls: 
*:         Programs       : 
*:         Screens        : 
*:         Global Function:
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*:  OG Filter:
*:    POSHDR.PO   
*:    POSHDR.VENDOR  
*:    POSHDR.SHIPVIA   
*:    POSHDR.STATUS    
*:    POSHDR.CDIVISION 
*:    POSHDR.COMPLETE
*:    POSHDR.AVAILABLE    
*:    POSHDR.CLCNO   
*:    POSLN.STYLE 
*:    POSHDR.SHIPVIA
*:    POSHDR.COLOR
*:    POSHDR.CTERMCODE
*:    POSHDR.CPURCODE 
*:    lcRpSort  
*:    lcRPPrintFor
*:    lcRPStatus
*****************************************************************************
*Modifications:
*C127107,MMT 27/03/2006 Convert program to FOX 
*****************************************************************************
ON KEY LABEL SPACEBAR

PRIVATE lcRpPath 

lcRpPath = getfile("XLS","Excel File Name","Create") && get the file name will be created

IF loOgScroll.llOGFltCh
  STORE ""  TO lcFreeClr,lcNonMajPi,lcNonMajT , lcRpWrDir,lcRpSysDir,lcRpRpDir ,lcRpDefPth,lcRpDtDir 
  STORE 0   TO lnClrPo,lnNonMajPo
  STORE ""  TO lcPOGRUP,lcPOTerm,lcVAl,lcVAl,lcAVLDate ,lcCmpDate ,lcCompDate ,lcCmpDateFrom, lcCmpDateTO ,lcShpVia ,lcTempColor,  lcTempDiv ,lcStyle , lcVendor ,  lcLcNum , LcPO
  STORE ""  TO lcAVLDateFrom ,  lcAVLDateTO , lcCmpDateFrom ,lcCmpDateTo

  lcMajPic  = gfItemMask("PM")
  lnMajPic  = LEN(lcMajPic)

  llNodata = .F. && flag to indicate if there is data to print or not

  *STORE "" TO lcNOLCFile,lcLCNOShipFile,lcShipFile
  STORE .F. TO llPOGRUP ,llPOTerm ,llCmpDate ,llAVLDate ,llCmpDate ,llShpVia ,llTempColor,  llTempDiv ,llStyle , llVendor ,  llLcNum , LlPO

  *Create temp. files
   =lfCrtTables()

  *Check selected filters
  =lfGetFilters()

  lcPoshdrFields = "PO,Complete,CLCNO,POTotal,NStyOrder,CDivision,CPurCode,CTermCode,ShipVia,Available,Vendor" 

  lcPoshdrCond   = "Poshdr.Cbusdocu = 'P' AND  Poshdr.CStyType = 'P'" + IIF(lcRPStatus <> 'L'," AND  POSHDR.Status= '"+lcRPStatus+"'","")


  lcPoslnForm    = "POSLN(Index = POSLN) INNER JOIN POSHDR ON ;
                  Posln.Cbusdocu = Poshdr.Cbusdocu AND ;
                  Posln.Cbusdocu = Poshdr.CStyType ;
                  AND POSLN.PO = POSHDR.PO " 

  lcPoslnCond = " POSLN.Cbusdocu = 'P' AND  POSLN.CStyType = 'P' AND POSLN.Trancd in ('1','3')"

  lcPoslnFld  = "SELECT POSLN.Po,POSLN.Style,POSLN.Trancd,POSLN.ShipNo "
  DO CASE
    CASE  llPO && User selected a PO number
      SELECT (lcPO)
      SCAN 
        lcTempLPO = &lcPO..PO
        loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.PO='" + ;
                                      lcTempLPO + "'" ;
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD
         SCAN FOR  IIF(llPOGRUP,SEEK(CPURCODE,'POGRUP'),.T.) ;
                   AND IIF(llPOTerm,SEEK(CTERMCODE,'POTerm'),.T.) AND ;
                   IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                   AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                   IIF(llShpVia,SEEK(SHIPVIA,'ShpVia'),.T.) AND ;
                   IIF(llTempDiv,SEEK(CDIVISION,'Div'),.T.) AND IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                   IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO) ,.T.);
                   AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
           
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ lcTempLPO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR IIF(llStyle,SEEK(SUBSTR(TPOSLINE.STYLE,1,lnMajPic),lcStyle),.T.) AND ;
               IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
               AND TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED)
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM)
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND() && we checked if the po has shipment or not
               SCAN FOR IIF(llStyle,SEEK(SUBSTR(TPOSLINE.STYLE,1,lnMajPic),lcStyle),.T.) AND ;
                        IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llStyle,SEEK(SUBSTR(TPOSLINE.STYLE,1,lnMajPic),lcStyle),.T.) AND ;
                        IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
      ENDSCAN 
      
    CASE llStyle && user selected style
      SELECT (lcStyle)
      SCAN 

        loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND SUBSTRING(POSLN.Style,1,"+ALLTRIM(STR(lnMajPic))+") = '"+ &lcStyle..Cstymajor+ "'","TPOSLINE",.T.)                  
        SELECT TPOSLINE 
        SELECT DISTINCT PO FROM TPOSLINE INTO CURSOR SelectedPO READWRIT 
        SELECT SelectedPO 
        SCAN 
         lcTempLPO = SelectedPO.PO
         loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.PO='" + ;
                                      lcTempLPO + "'" ;
                                      ,"TPOSHD",.T.)
          SELECT TPOSHD                            
          SCAN FOR  IIF(llPOGRUP,SEEK(CPURCODE,'POGRUP'),.T.) ;
                     AND IIF(llPOTerm,SEEK(CTERMCODE,'POTerm'),.T.) AND ;
                     IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                     AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                     IIF(llShpVia,SEEK(SHIPVIA,'ShpVia'),.T.) AND ;
                     IIF(llTempDiv,SEEK(CDIVISION,'Div'),.T.) AND IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                     IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                     AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
            SELECT  TPOSLINE
            IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
              SCAN FOR Po = lcTempLPO AND ;
                       IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                       AND TPOSLINE.TRANCD = '3'

                IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = Shpmthdr.CVESSEL         
                  m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                  m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                  m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                  m.CTNCNT         = Shpmthdr.CARTONS
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  SELECT (lcShipFile)
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
                ENDIF   
              ENDSCAN   
            ENDIF    
            IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
              LOCATE FOR Po = lcTempLPO AND  TPOSLINE.TRANCD = '3'
              IF !FOUND()           
                SCAN FOR Po = lcTempLPO AND ;
                         IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
                ENDSCAN 
              ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR Po = lcTempLPO  AND TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR Po = lcTempLPO AND ;
                        IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF        
         ENDSCAN 
       ENDSCAN   
     ENDSCAN 
     
   CASE llTempDiv && used selected a division 
     SELECT Div  
     SCAN 
       lcTempDiv = CDiv
       loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.CDIVISION = '" + ;
                                      lcTempDiv + "'" ;
                                      ,"TPOSHD",.T.)
       SELECT TPOSHD                                    
       SCAN FOR  IIF(llPOGRUP,SEEK(CPURCODE,'POGRUP'),.T.) ;
                 AND IIF(llPOTerm,SEEK(CTERMCODE,'POTerm'),.T.) AND ;
                 IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                 AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                 IIF(llShpVia,SEEK(SHIPVIA,'ShpVia'),.T.) AND ;
                 IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                 IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                 AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
                 loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
               AND TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
       ENDSCAN 

     CASE  llShpVia && user selected a shipvia
     
       SELECT ShpVia 
       **Get selected ShipVia from header and apply header where condition and join statment   
       SCAN 
         lcTempShpVia = cShpVia 
         loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.SHIPVIA  = '" + ;
                                      lcTempShpVia + "'" ;
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD                                    
         SCAN FOR  IIF(llPOGRUP,SEEK(CPURCODE,'POGRUP'),.T.) ;
              AND IIF(llPOTerm,SEEK(CTERMCODE,'POTerm'),.T.) AND ;
              IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
              AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
              IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
              IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
              AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
               AND TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
       ENDSCAN 
       
     CASE llPOTerm && user selected po terms
       SELECT POTerm
       SCAN 
         lcTempPOTerm = CPOTerm 
         loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.CTERMCODE   = '" + ;
                                      lcTempPOTerm + "'" ;
                                      ,"TPOSHD",.T.)
         
         SELECT TPOSHD
         SCAN FOR  IIF(llPOGRUP,SEEK(CPURCODE,'POGRUP'),.T.) AND ;
                 IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                 AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                 IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                 IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                 AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
                 loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
               AND TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    

           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR  TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
       ENDSCAN 
       
    CASE llPOGRUP && user selected a Po Group
      SELECT POGRUP
      SCAN
        lcTempPOGrup = CPOGRUP
        loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.CPURCODE = '" + ;
                                      lcTempPOGrup + "'" ;
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD
         SCAN FOR IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                 AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                 IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                 IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                 AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
               AND TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND EMPTY(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
      ENDSCAN 
      
    CASE  llTempColor  && user selected a style color 
      SELECT COLOR
      SCAN
        lcTemColor = COLOR.CCOLOR    
        loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND SUBSTRING(POSLN.STYLE," + ;
                             ALLTRIM(STR(lnClrPo))+ "," +;
                             ALLTRIM(STR(LEN(lcNonMajPi)))+") = '" + lcTemColor +"'","TPOSLINE",.T.)                  
        SELECT TPOSLINE 
        SELECT DISTINCT PO FROM TPOSLINE INTO CURSOR SelectedPO READWRIT 
        SELECT SelectedPO 
        SCAN 
          lcTempLPO = SelectedPO.PO
          loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.PO='" + ;
                                      lcTempLPO + "'" ;
                                      ,"TPOSHD",.T.)
          SELECT TPOSHD
          
          SCAN FOR   IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                     AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                     IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                     IIF(llVendor,SEEK(VENDOR,lcVendor),.T.) AND IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                     AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
            
            SELECT  TPOSLINE
            
            IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
              SCAN FOR Po = lcTempLPO AND ;
                       TPOSLINE.TRANCD = '3'

                IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = Shpmthdr.CVESSEL         
                  m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                  m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                  m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                  m.CTNCNT         = Shpmthdr.CARTONS
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  SELECT (lcShipFile)
                  APPEND BLANK
                  GATHER MEMO MEMVAR
                ENDIF   
              ENDSCAN
            ENDIF
            IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
              LOCATE FOR Po = lcTempLPO AND TPOSLINE.TRANCD = '3'
              IF !FOUND()
                SCAN FOR Po = lcTempLPO AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE)
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)
                  APPEND BLANK
                  GATHER MEMO MEMVAR
                ENDSCAN
              ENDIF
           ENDIF
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR Po = lcTempLPO AND TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR Po = lcTempLPO AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF        
          ENDSCAN 
        ENDSCAN 
      ENDSCAN 
      
   CASE llVendor && user selected a vendor code
     SELECT(lcVendor)     
     SCAN 
       lcTempVend = Cvendcode
       loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                      lcPoshdrCond +" AND POSHDR.Vendor = '" + ;
                                      lcTempVend + "'" ;
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD
         SCAN FOR IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                 AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                 IIF(llLcNum,SEEK(CLCNO,lcLcNum),.T.) AND ;
                 IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                 AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.); 
                         AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR IIF(llTempColor,SEEK(SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)),'COLOR'),.T.);
                        AND TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
      ENDSCAN 
      
    CASE llLcNum && user selected a LC number
    SELECT(lcLcNum)
    SCAN 
      lcLC = ClcNO
      loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                     lcPoshdrCond +" AND POSHDR.CLCNO = '" + ;
                                      lcLC + "'" ;
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD
         SCAN FOR IIF(llCmpDate,BETWEEN(TPOSHD.COMPLETE,CTOD(lcCmpDateFrom) ,CTOD(lcCmpDateTo)),.T.);
                 AND IIF(llAVLDate,BETWEEN(TPOSHD.AVAILABLE,CTOD(lcAvlDateFrom) ,CTOD(lcAvlDateTo)),.T.) AND ;
                 IIF("NLC" $ lcRPPrintFor,CLCNO = '',.T.);
                 AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = DTOC({})
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
      ENDSCAN 
      
    OTHERWISE &&otherwise date 
      lcDateValue = ""
      IF llCmpDate
        lcDateValue = "POSHDR.COMPLETE BETWEEN '"+ lcCmpDateFrom + "' AND '"+ lcCmpDateTo +"'"
      ENDIF 
      
      IF llAVLDate
        lcDateValue  = lcDateValue  + IIF(!EMPTY(lcDateValue),' AND ',"") + ;
                        " POSHDR.AVAILABLE BETWEEN '" + lcAvlDateFrom + "' AND '"+lcAvlDateTo+"'"
      ENDIF 
      loDBFPOSHDR.SqlRun("Select "+ lcPosHdrFields + " FROM POSHDR(INDEX = POSHDR) WHERE " + ;
                                     lcPoshdrCond + IIF(!EMPTY(lcDateValue) , " AND " + lcDateValue  ,"");
                                      ,"TPOSHD",.T.)
         SELECT TPOSHD
         SCAN FOR  IIF("NLC" $ lcRPPrintFor,empty(TPOSHD.CLCNO),.T.);
                   AND IIF("LCSH" $ lcRPPrintFor OR "NSH" $ lcRPPrintFor ,!empty(TPOSHD.CLCNO),.T.)
          
           loDBFPOSLN.SqlRun(lcPoslnFld  + " FROM " + lcPoslnForm + " WHERE " + ;
                             lcPoslnCond  + " AND POSLN.PO = '"+ TPOSHD.PO + "'","TPOSLINE",.T.)                  
                             
           SELECT  TPOSLINE
           IF ("LCSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             SCAN FOR TPOSLINE.TRANCD = '3'

               IF loDBFShpmthdr.Seek('P'+'P'+TPOSLINE.ShipNo,'Shpmthdr') &&ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
                 SCATTER MEMO MEMVAR 
                 m.Supplier       = TPOSHD.VENDOR
                 m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                 m.CLCNO          = TPOSHD.CLCNO
                 m.PO_AMT         = TPOSHD.POTOTAL 
                 m.TotalPCs       = TPOSHD.NSTYORDER
                 m.Vessel_Carrier = Shpmthdr.CVESSEL         
                 m.Depart_Date    = DTOC(Shpmthdr.ENTERED) 
                 m.ETA_KC         = DTOC(Shpmthdr.DCUSTOM) 
                 m.ETA_CD         = DTOC(Shpmthdr.DWAREHOUS) 
                 m.CTNCNT         = Shpmthdr.CARTONS
                 m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                 m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                 SELECT (lcShipFile)
                 APPEND BLANK 
                 GATHER MEMO MEMVAR  
               ENDIF   
             ENDSCAN   
           ENDIF    
           IF ("NSH" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND !empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = IIF(SEEK('S'+ m.CLCNO ,'LC'),LC.FRN_AMT,0)
                  m.LC_OPDATE      = IIF(SEEK('S'+ m.CLCNO ,'LC'),DTOC(LC.CREATED),'')
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcLCNoShipFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
           IF ("NLC" $ lcRPPrintFor OR 'A' $ lcRPPrintFor) AND empty(TPOSHD.CLCNO)
             LOCATE FOR TPOSLINE.TRANCD = '3'
             IF !FOUND()           
               SCAN FOR TPOSLINE.TRANCD = '1'
                  SCATTER MEMO MEMVAR 
                  m.Supplier       = TPOSHD.VENDOR
                  m.SHIP_Date      = DTOC(TPOSHD.COMPLETE) 
                  m.CLCNO          = TPOSHD.CLCNO
                  m.LC_OPAMT       = 0
                  m.LC_OPDATE      = ''
                  m.PO_AMT         = TPOSHD.POTOTAL 
                  m.TotalPCs       = TPOSHD.NSTYORDER
                  m.Vessel_Carrier = gfCodDes(TPOSHD.SHIPVIA, 'SHIPVIA')
                  SELECT(lcNOLCFile)      
                  APPEND BLANK 
                  GATHER MEMO MEMVAR  
               ENDSCAN 
             ENDIF    
           ENDIF 
         ENDSCAN 
    ENDCASE   
ENDIF 

lcOrder = IIF(lcRPSortBy ='LC','LCINDEX' , IIF(lcRPSortBy ='V', 'VENINDX ', 'POINDEX'))
DO CASE 
  CASE "A" $ lcRPPrintFor && All
    SELECT (lcShipFile) 
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 
    SELECT(lcLCNOShipFile) 
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 
    
    SELECT(lcNOLCFile)
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 

    
  CASE "LCSH" $ lcRPPrintFor && LC and Shipment 
    SELECT(lcShipFile) 
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 

       
  CASE "NLC" $ lcRPPrintFor && NO LC
    SELECT(lcNOLCFile) 
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 

    
  CASE "NSH" $ lcRPPrintFor  && LC but no Shipment
    SELECT(lcLCNOShipFile)
    SET ORDER TO &lcOrder 
    LOCATE 
    IF !EOF()
      llNodata = .T.
    ENDIF 
ENDCASE 

IF llNodata  
  =lfPrint()
ELSE
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  Return
ENDIF 

*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Omnia Mohammed Mostafa 
*! Date      : 13/10/2005
*! Purpose   : Check Filters values
*!*************************************************************
*! Passed Parameters  : lnArrayType= Type of filter (Fixed OR Variable Or Hedden)
*!                      lcFilter= Filter Name 
*!                      
*!*************************************************************
*! Returns            : lcReturn=The Name of Cursor that have the records that has been choosen from filter
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS   
lcReturn = ""     
DO CASE
CASE lnArrayType = 1 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  IF lnPos > 0
   lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
   lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  ENDIF
CASE lnArrayType = 2  
  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  IF lnPos > 0
   lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
   lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
  ENDIF
CASE lnArrayType = 3  
  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  IF lnPos > 0
   lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
   lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
  ENDIF
ENDCASE  
RETURN lcReturn

*!*************************************************************
*! Name      : lfGetJoin  
*! Developer : Omnia Mohammed Mostafa 
*! Date      : 13/10/2005
*! Purpose   : Jet Join Statment
*!*************************************************************
*! Passed Parameters  :lcfile=tabel name that will be joined with the final 
*!                      
*!*************************************************************
*! Returns            : lcJoin = join line code
*!*************************************************************

FUNCTION lfGetJoin  
IF lcfile='POSHDR'
  * Check if there is a filter on PO
  LcPO = lfCheckFilter(1, 'POSHDR.PO')
  llPO = !EMPTY(LcPO ) AND USED(LcPO ) AND (RECCOUNT(LcPO )> 0)
 
    * Check if there is a filter on DIVISION
    lcTempDiv = lfCheckFilter(1, 'POSHDR.CDIVISION')
    llTempDiv  = !EMPTY(lcTempDiv) AND lfStr2Curs(lcTempDiv,"Div","CDIV")
    IF llTempDiv  
        lcJoin = lcJoin +" Inner join  Div  ON TPOSHD.CDIVISION = Div.CDIV"   
    ENDIF
    
    * Check if there is a filter on SHIPVIA
    lcShpVia = lfCheckFilter(1, 'POSHDR.SHIPVIA')
    llShpVia  = !EMPTY(lcShpVia) AND lfStr2Curs(lcShpVia,"ShpVia","CShpVia")
    IF llShpVia  
        lcJoin = lcJoin +" Inner join  ShpVia ON TPOSHD.SHIPVIA = ShpVia.CShpVia"   &&miro
    ENDIF

    * Check if there is a filter on TERMCODE
    lcPOTerm = lfCheckFilter(1, 'POSHDR.CTERMCODE')
    llPOTerm  = !EMPTY(lcPOTerm) AND lfStr2Curs(lcPOTerm,"POTerm","CPOTerm")
    IF llPOTerm  
        lcJoin = lcJoin +" Inner join POTerm  ON TPOSHD.CTERMCODE= POTerm.CPOTerm"  &&miro
    ENDIF
    
    * Check if there is a filter on PURCODE
    lcPOGRUP = lfCheckFilter(1, 'POSHDR.CPURCODE')
    llPOGRUP  = !EMPTY(lcPOGRUP) AND lfStr2Curs(lcPOGRUP,"POGRUP","CPOGRUP")
    IF llPOGRUP  
        lcJoin = lcJoin +" Inner join  POGRUP  ON TPOSHD.CPURCODE= POGRUP.CPOGRUP" &&miro 
    ENDIF
    
    * Check if there is a filter on COmplete date
    lcCmpDate = lfCheckFilter(1, 'POSHDR.COMPLETE')
    llCmpDate = !EMPTY(lcCmpDate)
    lcCmpDateFrom = SUBSTR(lcCmpDate ,1,AT('|',lcCmpDate)-1)
    lcCmpDateTO = SUBSTR(lcCmpDate ,AT('|',lcCmpDate)+1)

    * Check if there is a filter on Available Date
    lcAVLDate = lfCheckFilter(1, 'POSHDR.AVAILABLE')
    llAVLDate  = !EMPTY(lcAVLDate)
    lcAVLDateFrom = SUBSTR(lcAVLDate,1,AT('|',lcAVLDate)-1)
    lcAVLDateTO = SUBSTR(lcAVLDate,AT('|',lcAVLDate)+1)
 ENDIF

 IF lcfile='NOTPOSHDR'
 
    * Check if there is a filter on Color
    lcTempColor = lfCheckFilter(1, 'POSHDR.COLOR')
    llTempColor = !EMPTY(lcTempColor) AND lfStr2Curs(lcTempColor ,"COLOR","CCOLOR")
    lfNonMaj ()
    IF llTempColor
      lcJoin = lcJoin +" INNER JOIN  COLOR ON SUBSTR(TPOSLINE.STYLE,lnClrPo,LEN(lcNonMajPi)) = COLOR.CCOLOR"
    ENDIF

    * Check if there is a filter on STYLE
    lcStyle = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
    llStyle  =!EMPTY(lcStyle) AND USED(lcStyle ) AND (RECCOUNT(lcStyle )>0)
    IF llStyle
    
        lcJoin = lcJoin +" INNER JOIN " + lcStyle + " ON TPOSLINE.STYLE = " + lcStyle + ".STYLE"
    ENDI

    * Check if there is a filter on LC 
    *-lcLcNum = lfCheckFilter(1, 'LCGOURNL.CLCNO')
    lcLcNum = lfCheckFilter(1, 'LC.CLCNO')
    llLcNum = !EMPTY(lcLcNum ) AND USED(lcLcNum ) AND (RECCOUNT(lcLcNum ) > 0)
    IF llLcNum 
      *lcJoin = lcJoin +" INNER JOIN  "+ lcLcNum + " ON TPOSLINE.CLCNO = "+&lcLcNum. +".CLCNO"   
      lcJoin = lcJoin +" INNER JOIN  "+ lcLcNum + " ON TPOSLINE.CLCNO = " + lcLcNum + ".CLCNO"
    ENDIF

    * Check if there is a filter on VENDOR
    lcVendor = lfCheckFilter(1, 'APVENDOR.CVENDCODE')
    llVendor  =!EMPTY(lcVendor ) AND USED(lcVendor ) AND (RECCOUNT(lcVendor )> 0)
    IF llVendor  
      *lcJoin = lcJoin +" Inner join  "+lcVendor +"  ON TPOSLINE.VENDOR = "+ &lcVendor. + ".VENDOR"  
      lcJoin = lcJoin +" Inner join  "+lcVendor +"  ON TPOSLINE.VENDOR = "+ lcVendor + ".CVENDCODE"  
    ENDIF
 ENDIF
   
*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Omnia Mohammed Mostafa 
*! Date      : 13/10/2005
*! Purpose   : Put string Into cursor
*!*************************************************************
*! Passed Parameters  : lnArrayType= Type of filter (Fixed OR Variable Or Hedden)
*!                      lcFilter= Filter Name 
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName
CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
INDEX on &lcFieldsName. TAG &lcFieldsName
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

*!*************************************************************
*! Name      : lfwRepWhen 
*! Developer : Omnia Mohammed Mostafa 
*! Date      : 13/10/2005
*! Purpose   : When function of the option grid.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwRepWhen

loDBFAPV      = CreateObject("RemoteTable","APVENDOR",,"APVENDOR",SET("DATASESSION"),,.T.)
loDBFPOSLN    = CreateObject("RemoteTable","POSLN",,"POSLN",SET("DATASESSION"),,.T.)
loDBFPOSHDR   = CreateObject("RemoteTable","POSHDR",,"POSHDR",SET("DATASESSION"),,.T.)
loDBFShpmthdr = CreateObject("RemoteTable","SHPMTHDR",,"SHPMTHDR",SET("DATASESSION"),,.T.)

*!*************************************************************
*! Name      : lfNonMaj 
*! Developer :  
*! Date      : 17/10/2005
*! Purpose   : Compute Free/Color Items in Style Structure
*!*************************************************************
*! Passed Parameters  : none
*!                      
*!*************************************************************
*! Returns            : none
*!*************************************************************
****Function TO get Color Length
FUNCTION lfNonMaj 
LOCAL llStopConc
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg = gfItemMask('SM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg, '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))
llStopConc = .F.
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(EMPTY(lnNonMajPo), laMajSeg[lnI,4], lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only these ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''

*!*************************************************************
*! Name      : lfFormName
*! Developer : Omnia Mohammed Mostafa
*! Date      : 09/18/2001
*! Purpose   : 13/10/2005
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : Name of  FRX report 
*!*************************************************************
*!

FUNCTION lfFormName

RETURN 'POTRKSHP'

****************************************************************************
*! PROG      : lfPrint.PRG   
*! DESC      : Function to Print To EXCEl Sheet
*! Developer : Mariam Mazhar (MMT)
*! Date      : 17/08/2004
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrint

lcRpDtDir  = oAriaApplication.DataDir      && Save The Data Directory.
lcRpWrDir  = oAriaApplication.WorkDir       && Save The Work Directory.
lcRpSysDir = oAriaApplication.SysPath      && Save The system  Directory.
lcRpRpDir  = gcRepHome             && Save The report Directory.
lcRpComp   = oAriaApplication.ActiveCompanyID&& Save The Active Company.
lcRpDefPth = oAriaApplication.defaultpath && Save The DefPath. oAriaApplication.defaultpath

*--copy file POSTYSGM to our new file 
lcRpPath = ALLTRIM(lcRpPath)
*--to be sure that it is EXCEL Name
lcRpForm = ALLTRIM(lcRpForm)
IF RIGHT(lcRpForm,4)<>".XLS"
  lcRpForm= lcRpForm+".XLS"
ENDIF

lcRpDefPth = ALLTRIM(lcRpDefPth)

lcSorCop = "'"+ALLTRIM(lcRpDefPth) +'POTRKSHP.XLS'+"'"
lcTarCop = "'"+lcRpPath + "'"


*-- When user press preview twice with no time interval between the two presses 
*-- an error occures while creating the target Excel file,this loop to resolve this situation
PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  COPY FILE &lcSorCop TO  &lcTarCop  
  *-- if error occurs while copying , ask to continue or exit loop
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr


=lfWriteExl()

RETURN
****************************************************************************
*! PROG      : lfWriteExl.PRG  
*! DESC      : Function to fill the EXCEl Sheet with data 
*! Developer : Mariam Mazhar (MMT)
*! Date      : 17/08/2004
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
***********************************************************************
FUNCTION lfWriteExl

ON ERROR DO lfError
LOCAL xlsheet,XLApp,tmpsheet
*-- Opren Temp File.

STORE 0 TO lnTotAmt

tmpsheet = GetObject(ALLTRIM(lcRpPath)+lcRpForm,'excel.sheet')
XLApp = tmpsheet.application

XLApp.WorkBooks.Add(ALLTRIM(lcRpPath))


IF "NLC" $ lcRPPrintFor OR "A" $ lcRPPrintFor 
  XLApp.Sheets("NO LC").Select
  *-- Change the Company Name if Not 
  SELECT (lcNOLCFile)
  LOCATE
  lnRow = 4
  SCAN
    XLApp.Sheets("NO LC").Cells(lnRow,1).Value = PO
    XLApp.Sheets("NO LC").Cells(lnRow,2).Value = Vessel_Carrier
    XLApp.Sheets("NO LC").Cells(lnRow,3).Value = STYLE
    XLApp.Sheets("NO LC").Cells(lnRow,4).Value = SUPPLIER
    XLApp.Sheets("NO LC").Cells(lnRow,5).Value = TotalPCs
    XLApp.Sheets("NO LC").Cells(lnRow,6).Value = CTNCNT 
    XLApp.Sheets("NO LC").Cells(lnRow,7).Value = SHIP_Date
    XLApp.Sheets("NO LC").Cells(lnRow,8).Value = Depart_Date
    XLApp.Sheets("NO LC").Cells(lnRow,9).Value = ETA_KC
    XLApp.Sheets("NO LC").Cells(lnRow,10).Value = ETA_CD
*!*      *-------
    lcThisPO = PO
    IF RECNO() <> 1
      SKIP -1 
      IF PO != lcThisPO 
        SKIP 1
        XLApp.Sheets("NO LC").Cells(lnRow,11).Value = PO_AMT
        lnTotAmt = lnTotAmt + PO_AMT
      ELSE
        SKIP 1
        XLApp.Sheets("NO LC").Cells(lnRow,11).Value = ''
      ENDIF 
    ELSE
      XLApp.Sheets("NO LC").Cells(lnRow,11).Value = PO_AMT 
      lnTotAmt = lnTotAmt + PO_AMT
    ENDIF 
*!*      *-------
   * XLApp.Sheets("NO LC").Cells(lnRow,11).Value = PO_AMT
    XLApp.Sheets("NO LC").Cells(lnRow,12).Value = CLCNO
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,13).Value = LC_OPDATE
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,14).Value = LC_OPAMT

    lnRow = lnRow + 1
  ENDSCAN
*!*    SCAN 
*!*      lnTotAmt = lnTotAmt + PO_AMT
*!*    ENDSCAN
  lnRow = lnRow + 2
  XLApp.Sheets("NO LC").Cells(lnRow,11).Font.FontStyle ="Bold"
  XLApp.Sheets("NO LC").Cells(lnRow,11).Value = lnTotAmt
  XLApp.Sheets("NO LC").Cells(lnRow,5).Value ="Total Orders Amount"
  XLApp.Sheets("NO LC").Cells(lnRow,5).Font.FontStyle ="Bold"
ENDIF

IF "NSH" $ lcRPPrintFor OR "A" $ lcRPPrintFor 
  XLApp.Sheets("NO SHIPPNG").Select

  SELECT(lcLCNOShipFile)
  LOCATE
  lnRow = 4
  SCAN

    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,1).Value = PO
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,2).Value = Vessel_Carrier
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,3).Value = STYLE
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,4).Value = SUPPLIER
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,5).Value = TotalPCs
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,6).Value = CTNCNT 
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,7).Value = SHIP_Date
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,8).Value = Depart_Date
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,9).Value = ETA_KC
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,10).Value = ETA_CD
    *-------
    lcThisPO = PO
    IF RECNO() <> 1
      SKIP -1 
      IF PO != lcThisPO 
        SKIP 1
        XLApp.Sheets("NO SHIPPNG").Cells(lnRow,11).Value = PO_AMT  
      ELSE
        SKIP 1
        XLApp.Sheets("NO SHIPPNG").Cells(lnRow,11).Value = ''
      ENDIF 
    ELSE
      XLApp.Sheets("NO SHIPPNG").Cells(lnRow,11).Value = PO_AMT  
    ENDIF 
    *-------
*    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,11).Value = PO_AMT  
    XLApp.Sheets("NO SHIPPNG").Cells(lnRow,12).Value = CLCNO
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,13).Value = LC_OPDATE
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,14).Value = LC_OPAMT

    lnRow = lnRow + 1
  ENDSCAN
ENDIF

IF "A" $ lcRPPrintFor OR "LCSH" $ lcRPPrintFor
  XLApp.Sheets("TRACKING SHIPMENT").Select

  *-- Change the Company Name if Not 
  SELECT(lcShipFile)
  LOCATE
  lnRow = 4
  SCAN
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,1).Value = PO
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,2).Value = Vessel_Carrier
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,3).Value = STYLE
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,4).Value = SUPPLIER
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,5).Value = TotalPCs
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,6).Value = CTNCNT 
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,7).Value = SHIP_Date
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,8).Value = Depart_Date
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,9).Value = ETA_KC
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,10).Value = ETA_CD
*!*          *-------
    lcThisPO = PO
    IF RECNO() <> 1
      SKIP -1 
      IF PO != lcThisPO 
        SKIP 1
        XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,11).Value = PO_AMT  
      ELSE
        SKIP 1
        XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,11).Value = ''
      ENDIF 
    ELSE
      XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,11).Value = PO_AMT  
    ENDIF 
*!*      *-------
*    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,11).Value = PO_AMT  
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,12).Value = CLCNO
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,13).Value = LC_OPDATE
    XLApp.Sheets("TRACKING SHIPMENT").Cells(lnRow,14).Value = LC_OPAMT
    lnRow = lnRow + 1
  ENDSCAN
ENDIF

XLApp.Visible = .T.
****************************************************************************
*! PROG      : lfError.PRG   
*! DESC      : Function to Print message in case of error
*! Developer : Mariam Mazhar (MMT)
*! Date      : 17/08/2004
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
***********************************************************************
FUNCTION lfError
  =gfModalGen("TRM000000B00000","DIALOG",'','','Error while creating the Excel file , please retry.')
READ EVENTS
****************************************************************************
*! PROG      : lfCrtTables
*! DESC      : Function to create tables
*! Developer : Mariam Mazhar (MMT)
*! Date      : 23/03/2006
****************************************************************************
FUNCTION lfCrtTables

DIMENSION laTempacstru[14,4]
laTempacstru[1,1]='PO'
laTempacstru[1,2]='C'
laTempacstru[1,3]= 6
laTempacstru[1,4]= 0

laTempacstru[2,1]='Vessel_Carrier'
laTempacstru[2,2]='C'
laTempacstru[2,3]= 30
laTempacstru[2,4]= 0
    
laTempacstru[3,1]='STYLE'
laTempacstru[3,2]='C'
laTempacstru[3,3]= 19
laTempacstru[3,4]= 0

laTempacstru[4,1]='Supplier'
laTempacstru[4,2]='C'
laTempacstru[4,3]= 8
laTempacstru[4,4]= 0

laTempacstru[5,1]='TotalPCs'
laTempacstru[5,2]='N'
laTempacstru[5,3]= 10
laTempacstru[5,4]= 2


laTempacstru[6,1]='CTNCNT'
laTempacstru[6,2]='N'
laTempacstru[6,3]= 5
laTempacstru[6,4]= 0

laTempacstru[7,1]='SHIP_Date'
laTempacstru[7,2]='C'
laTempacstru[7,3]= 8
laTempacstru[7,4]= 0


laTempacstru[8,1]='Depart_Date'
laTempacstru[8,2]='C'
laTempacstru[8,3]= 8
laTempacstru[8,4]= 0

laTempacstru[9,1]='ETA_KC'
laTempacstru[9,2]='C'
laTempacstru[9,3]= 8
laTempacstru[9,4]= 0

laTempacstru[10,1]='ETA_CD'
laTempacstru[10,2]='C'
laTempacstru[10,3]= 8
laTempacstru[10,4]= 0

laTempacstru[11,1]='PO_AMT'
laTempacstru[11,2]='N'
laTempacstru[11,3]= 9
laTempacstru[11,4]= 3

laTempacstru[12,1]='CLCNO'
laTempacstru[12,2]='C'
laTempacstru[12,3]= 6
laTempacstru[12,4]= 0

laTempacstru[13,1]='LC_OPDATE'
laTempacstru[13,2]='C'
laTempacstru[13,3]= 8
laTempacstru[13,4]= 0

laTempacstru[14,1]='LC_OPAMT'
laTempacstru[14,2]='N'
laTempacstru[14,3]= 9
laTempacstru[14,4]= 3

DO CASE 
  CASE "A" $ lcRPPrintFor && All
    lcShipFile = loogScroll.gfTempName() 
    lcLCNOShipFile = loogScroll.gfTempName() 
    lcNOLCFile = loogScroll.gfTempName() 
    
    gfCrtTmp(lcShipFile ,@laTempacstru,"PO",'POINDEX',.T.)    
    =CURSORSETPROP("Buffering",3,lcShipFile)
    SELECT (lcShipFile)
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX

    gfCrtTmp(lcLCNOShipFile ,@laTempacstru,"PO",'POINDEX',.T.)    
    =CURSORSETPROP("Buffering",3,lcLCNOShipFile)
    SELECT(lcLCNOShipFile )
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX

    gfCrtTmp(lcNOLCFile ,@laTempacstru,"PO",'POINDEX' ,.T.)    
    =CURSORSETPROP("Buffering",3,lcNOLCFile)
    SELECT(lcNOLCFile )
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX

    
  CASE "LCSH" $ lcRPPrintFor && LC and Shipment 
    lcShipFile = loogScroll.gfTempName() 
    gfCrtTmp(lcShipFile ,@laTempacstru,"PO",'POINDEX',.T.) 
    =CURSORSETPROP("Buffering",3,lcShipFile )
    SELECT (lcShipFile)
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX

       
  CASE "NLC" $ lcRPPrintFor && NO LC
    lcNOLCFile = loogScroll.gfTempName() 
    gfCrtTmp(lcNOLCFile ,@laTempacstru,"PO",'POINDEX' ,.T.)    
    =CURSORSETPROP("Buffering",3,lcNOLCFile)
    SELECT(lcNOLCFile )
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX

    
  CASE "NSH" $ lcRPPrintFor  && LC but no Shipment
    lcLCNOShipFile = loogScroll.gfTempName() 
    gfCrtTmp(lcLCNOShipFile ,@laTempacstru,"PO",'POINDEX',.T.)    
    =CURSORSETPROP("Buffering",3,lcLCNOShipFile)
    SELECT(lcLCNOShipFile )
    INDEX ON Supplier TAG VENINDX
    INDEX ON CLCNO TAG LCINDEX
ENDCASE 

****************************************************************************
*! PROG      : lfGetFilters
*! DESC      : Function to create tables
*! Developer : Mariam Mazhar (MMT)
*! Date      : 23/03/2006
****************************************************************************
FUNCTION lfGetFilters
lcPO = lfCheckFilter(1, 'POSHDR.PO')
llPO = !EMPTY(LcPO ) AND USED(LcPO ) AND (RECCOUNT(lcPO )> 0)

lcTempDiv = lfCheckFilter(1, 'POSHDR.CDIVISION')
llTempDiv  = !EMPTY(lcTempDiv) AND lfStr2Curs(lcTempDiv,"Div","CDIV")

* Check if there is a filter on SHIPVIA
lcShpVia = lfCheckFilter(1, 'POSHDR.SHIPVIA')
llShpVia  = !EMPTY(lcShpVia) AND lfStr2Curs(lcShpVia,"ShpVia","CShpVia")

* Check if there is a filter on TERMCODE
lcPOTerm = lfCheckFilter(1, 'POSHDR.CTERMCODE')
llPOTerm  = !EMPTY(lcPOTerm) AND lfStr2Curs(lcPOTerm,"POTerm","CPOTerm")

* Check if there is a filter on PURCODE
lcPOGRUP = lfCheckFilter(1, 'POSHDR.CPURCODE')
llPOGRUP  = !EMPTY(lcPOGRUP) AND lfStr2Curs(lcPOGRUP,"POGRUP","CPOGRUP")

   
* Check if there is a filter on COmplete date
lcCmpDate = lfCheckFilter(1, 'POSHDR.COMPLETE')
llCmpDate = !EMPTY(lcCmpDate)
lcCmpDateFrom = SUBSTR(lcCmpDate ,1,AT('|',lcCmpDate)-1)
lcCmpDateTO = SUBSTR(lcCmpDate ,AT('|',lcCmpDate)+1)

* Check if there is a filter on Available Date
lcAVLDate = lfCheckFilter(1, 'POSHDR.AVAILABLE')
llAVLDate  = !EMPTY(lcAVLDate)
lcAVLDateFrom = SUBSTR(lcAVLDate,1,AT('|',lcAVLDate)-1)
lcAVLDateTO = SUBSTR(lcAVLDate,AT('|',lcAVLDate)+1)


* Check if there is a filter on Color
lcTempColor = lfCheckFilter(1, 'POSHDR.COLOR')
llTempColor = !EMPTY(lcTempColor) AND lfStr2Curs(lcTempColor ,"COLOR","CCOLOR")

=lfNonMaj ()

* Check if there is a filter on STYLE
lcStyle = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llStyle  =!EMPTY(lcStyle) AND USED(lcStyle ) AND (RECCOUNT(lcStyle )>0)

* Check if there is a filter on LC 
lcLcNum = lfCheckFilter(1, 'LC.CLCNO')
llLcNum = !EMPTY(lcLcNum ) AND USED(lcLcNum ) AND (RECCOUNT(lcLcNum ) > 0)

* Check if there is a filter on VENDOR
lcVendor = lfCheckFilter(1, 'APVENDOR.CVENDCODE')
llVendor  =!EMPTY(lcVendor ) AND USED(lcVendor ) AND (RECCOUNT(lcVendor )> 0)

