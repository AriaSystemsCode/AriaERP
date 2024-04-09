****************************************************************************
*: Program file  : ALPCKLMA.PRG
*: Program desc. : Custom Packing List for JASCO(MACYS)
*: System        : Aria Apparel System (Aria4XP).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Mariam Mazhar - (MMT) Due to issue #C201485[T20120424.0015]
*: Date          : 05/21/2012
*:**************************************************************************
*: Calls : FUNCTIONS  : lfWOgWhen  ,lfWorkFile ,lfSRInv    ,lfAdrShift
*:                    : lfCollect  ,lfCollTime ,lfHeadVar  ,lfGrpSetes  
*:                    : lfChngScle ,lfStyMasks ,lfEvlMajNn ,lfNonMjDes
*:                    : lfSpckln   ,lfEndGroup ,lfScalHead ,lfClearRep.
*:                    : gfDispRe    ..........
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*! Modification:
*! C201485,2 MMT 07/03/2012 use the retail price instead of gros price in calcuations[T20120424.0015]
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[T20120424.0015]
*:**************************************************************************
lcStTime   = TIME()
*--we add record in SYREPUVR for lcFormName 
lcPrgName  = lcFormName
*llIsAparel = lfIsApparl(@lcPrgName)
lcCompFax = TRANSFORM(&lcCompInfo..cCom_Fax , '@R '+lcPhonPict)  && && Variable hold the Company Fax.
llALPakLst = .T.
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[Start]
lcBrnd = ''
lcWebSite = ''
lcAvlTime= ''
lcPhoneNumber= ''
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[End]
*-- Define variables that defined in OG for this report [Begin]
DIMENSION laSpckTemp[8] , laTemp[8] 

STORE ''TO lcColor, lcScale, lcNotes, lcTitle,;
           laSpckTemp, laTemp , lcPackNo,lcLinFile,lcStyleExp

lcLinFile = lcPakLnTmp

llEndGroup = .F.
lnLastRec  = 0


STORE SPACE(16) TO lcPackId 
IF loPack_Hdr.llnative
  SELECT(lcTempPack_Hdr)
  LOCATE 
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
     =gfModalGen('TRM00052B00000','DIALOG')
   RETURN
  ENDIF
ENDIF 
*-- if user change filter criteria then you must collect data again 
IF llOGFltCh
  DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2] , laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  laCompAdd = ''           && Array to hold the Company address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  *--selecting company information from syccomp remotely[Begin]
  lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
  lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult = 1
    SELECT &lcCompInfo
    lcCompName      = cCom_Name
    lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    DIMENSION laCompAdd[6,1]
    *laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 

  *-- NotePad File  
  IF lcNotePad   = lcTempNotePad
    lcNotePad = loogScroll.gfTempName()
    SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
    =lfMakeIndex(lcNotePad)
  ELSE 
    IF USED(lcNotePad) 
      USE IN (lcNotePad)
      SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
      =lfMakeIndex(lcNotePad)
    ENDIF  
  ENDIF 
  *--customer file
  IF lcCustomer  = lcTempCustomer
    lcCustomer = loogScroll.gfTempName()
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
  ELSE
    IF USED(lcCustomer)
      USE IN (lcCustomer)
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
    ENDIF 
  ENDIF 
  *--OrdHdr File
  IF lcOrdhdr    = lcTempOrdhdr
    lcOrdhdr = loogScroll.gfTempName()
    SELECT &lcTempOrdHdr..*,&lcTempOBJECTS..gobject as UPCBAR,&lcTempOBJECTS..gobject as VUPCBAR FROM &lcTempOrdHdr,&lcTempOBJECTS WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
    =lfMakeIndex(lcOrdhdr)
  ELSE
    IF USED(lcOrdhdr)
      USE IN (lcOrdhdr)
      SELECT &lcTempOrdHdr..*,&lcTempOBJECTS..gobject as UPCBAR,&lcTempOBJECTS..gobject as VUPCBAR  FROM &lcTempOrdHdr,&lcTempOBJECTS WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
      =lfMakeIndex(lcOrdhdr)
    ENDIF 
  ENDIF 
  *--Ordline File
  IF lcOrdLnTmp  = lcTempOrdline
    lcOrdLnTmp = loogScroll.gfTempName()
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
    =lfMakeIndex(lcOrdLnTmp)
  ELSE
    IF USED(lcOrdLnTmp)
      USE IN (lcOrdLnTmp)
      SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
      =lfMakeIndex(lcOrdLnTmp)
    ENDIF 
  ENDIF 
  *--Invoice line File
*!*	  IF   lcInvLnTmp  = lcTempInvLine
*!*	    lcInvLnTmp = loogScroll.gfTempName()
*!*	    SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
*!*	    =lfMakeIndex(lcInvLnTmp)
*!*	  ELSE
*!*	    IF USED(lcInvLnTmp)
*!*	      USE IN (lcInvLnTmp)
*!*	      SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
*!*	      =lfMakeIndex(lcInvLnTmp)
*!*	    ENDIF 
*!*	  ENDIF 
  *--style file
  IF lcStyleFile = lcTempStyle
    lcStyleFile = loogScroll.gfTempName()
    SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
    =lfMakeIndex(lcStyleFile)
  ELSE 
    IF USED(lcStyleFile)
      USE IN (lcStyleFile)
      SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
      =lfMakeIndex(lcStyleFile)
    ENDIF 
  ENDIF 
  *--scale file
  IF lcScaleFile = lcTempScale
    lcScaleFile = loogScroll.gfTempName()
    SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
    =lfMakeIndex(lcScaleFile)
  ELSE 
    IF USED(lcScaleFile)
      USE IN (lcScaleFile)
      SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
      =lfMakeIndex(lcScaleFile)
    ENDIF 
  ENDIF 
  *--WareHous File
  IF lcWareHous  = lcTempWareHous
    lcWareHous = loogScroll.gfTempName()
    SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
    =lfMakeIndex(lcWareHous)
  ELSE 
    IF USED(lcWareHous)
      USE IN (lcWareHous)
      SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
      =lfMakeIndex(lcWareHous)
    ENDIF 
  ENDIF 
  *-- Collecting Code...
  IF !USED(lcPackTmp) OR (RECCOUNT(lcPackTmp) > 0)
    =lfWorkFile()
  ENDIF

  =lfCollect()               && Collect Aging data.
  SELECT (lcPackTmp)
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO &lcCustomer ,;
                                      "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
                                      "B" + ORDER             INTO &lcNotePad  ,;  
                                      PACK_NO                 INTO (lcPakLnTmp) ,;           
                                      "O" + ORDER             INTO &lcOrdHdr  ADDITIVE
ENDIF

*-- Asking if no records (Display message) otherwise print report 
SELECT (lcPackTmp)
IF RECCOUNT(lcPackTmp) = 0
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF

SELECT(lcSTYLEfile)
SET RELATION TO  "S" + scale  INTO (lcScaleFile)

SELECT (lcPackTmp)
GOTO TOP

lnMajorPic = LEN(lcMajorPic)  
*-- llUseExtSc : This variable was defined at OG level, to keep track of using 
*--            : extended size scale.
IF llUseExtSc
  lcItemTitl = PADR(lcMajorTlt,lnMajorPic)+'-'+;
               SUBSTR(lcNnMajTlt,1,(lnMajorPic-lnExtScLen))
  lnNMajorPc = SUBSTR(lcNnMajTlt,1,(lnMajorPic-lnExtScLen))
ELSE
  lcItemTitl = PADR(lcMajorTlt,LEN(lcMajorPic)) +;
               PADR(lcNnMajTlt,LEN(lcNnMajPic))
  lnNMajorPc = PADR(lcNnMajTlt,LEN(lcNnMajPic))
ENDIF  


*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.

IF llALPakLst
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2
ENDIF
*-- Sort by Carton No. Index
SET ORDER TO TAG (lcPakLnTmp) IN (lcPakLnTmp)

IF llOGFltCh
  SELECT (lcPackTmp)
  LOCATE 
  SCAN 
    =SEEK(&lcPackTmp..PACK_NO,lcPakLnTmp)
    SELECT (lcPakLnTmp)
    SCAN REST WHILE PACK_NO+STR(nOrdLineNO,6)+STYLE+SIZE = &lcPackTmp..PACK_NO
      REPLACE MLines WITH MLines+PADR(EVAL(lcPakLnTmp+'.UPC'),15)+SPACE(8)+ALLTRIM(EVAL(lcPakLnTmp+'.DESC1'))+CHR(13)+CHR(10) IN (lcPackTmp)
    ENDSCAN 
    SELECT (lcPackTmp)
  ENDSCAN
  SELECT (lcPackTmp)
  LOCATE 
ENDIF  
SELECT (lcPackTmp)
SET SKIP TO (lcPakLnTmp) 
LOCATE 
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcFormName')

*!**************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Name      : lfWOgWhen()    
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To Set When running the Option Grid.
*!**************************************************************************
*! Called from : "When" Option Grid.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfWOgWhen()    
*!**************************************************************************
*
FUNCTION lfWOgWhen

*--Pack_Hdr file
IF TYPE('loPack_Hdr') <> 'O'
  loPack_Hdr = CreateObject("RemoteTable","Pack_Hdr","Pack_Hdr",lcTempPack_Hdr,SET("DATASESSION")) 
ENDIF 

IF TYPE('loStyleUpc') <> 'O'
  loStyleUpc = CreateObject("RemoteTable","StyleUpc","STYLEUPC",lcTempStyleUpc,SET("DATASESSION")) 
ENDIF 


*--Pack_lin file
IF TYPE('loPack_Lin') <> 'O'
  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 
ENDIF 

*--Piktkt file
IF TYPE('loPikTkt') <> 'O'
   loPikTkt   = CreateObject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
ENDIF

*--Customer file
IF TYPE('loCustomer') <> 'O'
  loCustomer = CreateObject("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))  
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ENDIF   

*--Notepad file
IF TYPE('loNotePad') <> 'O'
  loNotePad  = CreateObject("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION")) 
  SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
  =lfMakeIndex(lcNotePad)
ENDIF  

*--Style file
IF TYPE('loStyle') <> 'O' 
  loStyle    = CreateObject("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION")) 
  SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
  =lfMakeIndex(lcStyleFile)
ENDIF

*--Scale file
IF TYPE('loScale') <> 'O'
  loScale    = CreateObject("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION")) 
  SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
  =lfMakeIndex(lcScaleFile)
ENDIF 
  
*--Ordhdr file  
IF TYPE('loOrdHdr') <> 'O'
  loOrdHdr   = CreateObject("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
  =lfMakeIndex(lcOrdhdr)
ENDIF

*--Ordline file 
IF TYPE('loOrdLine') <> 'O'  
  loOrdLine  = CreateObject("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
  =lfMakeIndex(lcOrdLnTmp)
ENDIF   

*--Invline File
*!*	IF TYPE('loInvLine') <> 'O'  
*!*	  loInvLine  = CreateObject("RemoteTable","InvLine","InvLine",lcTempInvLine,SET("DATASESSION")) 
*!*	  SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
*!*	  =lfMakeIndex(lcInvLnTmp)
*!*	ENDIF   

*--Invhdr file
IF TYPE('loInvHdr') <> 'O'  
  loInvHdr  = CreateObject("RemoteTable","InvHdr","InvHdr",lcTempInvHdr,SET("DATASESSION")) 
ENDIF   
 
*--WareHous  file
IF TYPE('loWareHous') <> 'O'  
  loWareHous  = CreateObject("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION")) 
  SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
  =lfMakeIndex(lcWareHous)
ENDIF   

*--ICSEGVAL file
IF TYPE('loICSEGVAL') <> 'O'  
  loICSEGVAL = CreateObject("RemoteTable","ICSEGVAL","SEGVAL",lcTempICSEGVAL,SET("DATASESSION")) 
ENDIF   

*--Spck_lin file
IF TYPE('loSpck_Lin') <> 'O'  
  loSpck_Lin = CreateObject("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION")) 
ENDIF   

 
IF TYPE('lcBuild') = 'N'
  lcBuild = 'OK'
  SELECT(lcTempPack_Hdr)
  DIMENSION laTempStru[1,18]
  lnFldLen = AFIELDS(laTempStru)
  DIMENSION laTempStru[lnFldLen + 3 , 18]

  laTempStru[lnFldLen + 1 , 1] = 'nRprtTyp'
  laTempStru[lnFldLen + 1 , 2] = 'N'
  laTempStru[lnFldLen + 1 , 3] = 1
  laTempStru[lnFldLen + 1 , 4] = 0

  laTempStru[lnFldLen + 2 , 1] = 'Invoice'
  laTempStru[lnFldLen + 2 , 2] = 'C'
  laTempStru[lnFldLen + 2 , 3] = 6
  laTempStru[lnFldLen + 2 , 4] = 0

  laTempStru[lnFldLen + 3 , 1] = 'MLines'
  laTempStru[lnFldLen + 3 , 2] = 'M'
  laTempStru[lnFldLen + 3 , 3] = 10
  laTempStru[lnFldLen + 3 , 4] = 0


 FOR lnLoop = 1 to  3
  STORE ' ' TO  laTempStru[lnFldLen +lnLoop,7],laTempStru[lnFldLen +lnLoop,8],;
                laTempStru[lnFldLen +lnLoop,9],laTempStru[lnFldLen +lnLoop,10],;
                laTempStru[lnFldLen +lnLoop,11],laTempStru[lnFldLen +lnLoop,12],;
                laTempStru[lnFldLen +lnLoop,13],laTempStru[lnFldLen +lnLoop,14],;
                laTempStru[lnFldLen +lnLoop,15],laTempStru[lnFldLen +lnLoop,16]
  STORE 0 TO    laTempStru[lnFldLen +lnLoop,17] ,laTempStru[lnFldLen +lnLoop,18]

 ENDFOR
 
 
  SELECT(lcTempPack_Lin)
  DIMENSION laPckLinSt[1,18]
  =AFIELDS(laPckLinSt)

  lnFldLnPck = AFIELDS(laPckLinSt)
  DIMENSION laPckLinSt[lnFldLnPck + 6 , 18]

  *-- Field for the printing lines.
  laPckLinSt[lnFldLnPck + 1 , 1] = 'UPC'
  laPckLinSt[lnFldLnPck + 1 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 1 , 3] = 15
  laPckLinSt[lnFldLnPck + 1 , 4] = 0

  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 2 , 1] = 'Size'
  laPckLinSt[lnFldLnPck + 2 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 2 , 3] = 1
  laPckLinSt[lnFldLnPck + 2 , 4] = 0
 
 
  laPckLinSt[lnFldLnPck + 3 , 1] = 'OrdQty'
  laPckLinSt[lnFldLnPck + 3 , 2] = 'N'
  laPckLinSt[lnFldLnPck + 3 , 3] = 6
  laPckLinSt[lnFldLnPck + 3 , 4] = 0

  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 4 , 1] = 'PQty'
  laPckLinSt[lnFldLnPck + 4 , 2] = 'N'
  laPckLinSt[lnFldLnPck + 4 , 3] = 6
  laPckLinSt[lnFldLnPck + 4 , 4] = 0

  laPckLinSt[lnFldLnPck + 5 , 1] = 'Price'
  laPckLinSt[lnFldLnPck + 5 , 2] = 'N'
  laPckLinSt[lnFldLnPck + 5, 3] = 12
  laPckLinSt[lnFldLnPck + 5 , 4] = 2
 
 
  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 6 , 1] = 'desc1 '
  laPckLinSt[lnFldLnPck + 6 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 6 , 3] = 60
  laPckLinSt[lnFldLnPck + 6 , 4] = 0
 
 
 
 
 
 FOR lnLoop = 1 to  6
   STORE ' ' TO  laPckLinSt[lnFldLnPck +lnLoop,7],laPckLinSt[lnFldLnPck +lnLoop,8],;
                 laPckLinSt[lnFldLnPck +lnLoop,9],laPckLinSt[lnFldLnPck +lnLoop,10],;
                 laPckLinSt[lnFldLnPck +lnLoop,11],laPckLinSt[lnFldLnPck +lnLoop,12],;
                 laPckLinSt[lnFldLnPck +lnLoop,13],laPckLinSt[lnFldLnPck +lnLoop,14],;
                 laPckLinSt[lnFldLnPck +lnLoop,15],laPckLinSt[lnFldLnPck +lnLoop,16]
   STORE 0 TO    laPckLinSt[lnFldLnPck +lnLoop,17] ,laPckLinSt[lnFldLnPck +lnLoop,18]

  ENDFOR

   =lfWorkFile()  && Fill arrays then create files.

  *-- llHaveLogo : If Company Have a Logo. 
  IF TYPE('loObjects') <> 'O'  
    loObjects = CreateObject("RemoteTable","OBJECTS","OBJECTID",lcTempOBJECTS,SET("DATASESSION")) 
  ENDIF 
  
  IF TYPE('loObjLink') <> 'O'  
    loObjLink = CreateObject("RemoteTable","OBJLINK","OBJLNKTY",lcTempObjLink,SET("DATASESSION")) 
  ENDIF   
  llHaveLogo = loObjLink.SEEK('*' + 'LOGO') AND loObjects.SEEK(&lcTempObjLink..cObject_ID)
  *--
   DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2] , laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  laCompAdd = ''           && Array to hold the Company address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  *--selecting company information from syccomp remotely[Begin]
  lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
  lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
 
 
  IF lnResult = 1
    SELECT &lcCompInfo
    lcCompName      = cCom_Name
    lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    DIMENSION laCompAdd[6,1]
    *laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
ENDIF


*-- End Of lfWOgWhen.
*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfWorkFile()
*!**************************************************************************
*
FUNCTION lfWorkFile
*-- First Close all temporary files if it was opened before 
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
ENDIF
*-- First Close all temporary files if it was opened before
=gfCrtTmp(lcPackTmp,@laTempStru,"PACK_NO",lcPackTmp,.T.)
SELECT (lcPackTmp)
*-- You Must Make A Zap To decoument The File After Cursor on The Hard Disk
ZAP
INDEX ON PACK_NO TAG (lcPackTmp)

*-- First Close all temporary files if it was opened before 
IF USED(lcPakLnTmp)
  USE IN (lcPakLnTmp)
ENDIF


=gfCrtTmp(lcPakLnTmp,@laPckLinSt,"PACK_NO+STR(nOrdLineNO,6)+STYLE+SIZE",lcPakLnTmp,.T.)

SELECT (lcPakLnTmp)
ZAP
*!*	INDEX ON PACK_NO+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp) 

INDEX ON pack_no+STR(no_cart,4)+style TAG 'PACKCRTN' ADDITIVE
*-- End Of lfWorkFile.

*!**************************************************************************
*! Name      : lfSRInv   
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRInv (S-> , R->)
*!**************************************************************************
*
FUNCTION lfSRInv
PARAMETERS lcParm

SET ORDER TO TAG Customer IN Customer
SET ORDER TO TAG Invhdr   IN Invhdr
IF lcParm = "S"
  SELECT INVHDR
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  GO TOP
ELSE
  SELECT INVHDR
  SET RELATION TO
ENDIF
*-- End Of lfSRInv.

*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : used to shift the lashipto array
*!**************************************************************************
*! Passed Parameters  : lcArrayNam : Array hold The address.
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : For Collecting Data In The First Time.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : = lfCollect()
*!**************************************************************************
FUNCTION lfCollect

lnPosPackList = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")
loPack_Hdr.setorder("Pack_Hdr")
IF lnPosPackList > 0 
  lnPosPackList = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPackList,1)
  lcPackLstSel =IIF(!EMPTY(laOgFxFlt[lnPosPackList,6]),laOgFxFlt[lnPosPackList,6],'')
  IF !EMPTY(lcPackLstSel) AND USED(lcPackLstSel)
    SELECT(lcPackLstSel)
    LOCATE
    IF !EOF()
      SCAN 
        WAIT WINDOW "Collect data for PACK_NO #" + &lcPackLstSel..PACK_NO  + ' ...' NOWAIT
        loPack_hdr.Seek(&lcPackLstSel..PACK_NO)
        SELECT(lcTempPack_Hdr)
        SCATTER MEMVAR MEMO
        IF EMPTY(M.CWARECODE)
          loOrdHdr.Seek('O' + m.Order)
          M.CWARECODE = &lctempOrdHdr..CWARECODE
        ENDIF 
        INSERT INTO (lcPackTmp) FROM MEMVAR
      ENDSCAN 
      WAIT CLEAR
      *--function to collect the lines 
      =lfGetLines()
      =lfGetData()
    ELSE
      =lfNoPackSel()
    ENDIF  
  ELSE
    =lfNoPackSel()
  ENDIF 
ENDIF    
*-- END OF lfCollect.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.

*!**************************************************************************
*! Name      : lfHeadVar
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 1 - Hold Address In Shipto Array
*!           : 2 - Get the division long name.
*!**************************************************************************
*! Called from : Report Group Header band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfHeadVar()
*!**************************************************************************
FUNCTION lfHeadVar

DIMENSION laShipTo[5,1]
laShipTo = ''

DIMENSION laSoldTo[6,1]
laSoldTo = ''
loNotepad.seek("B" + &lcPackTmp..ORDER)              
lcSolTName = ''
lnNotesLine = MEMLINES(eval(lcTempNotePad+'.mNotes'))
lnStLnAdd = 0
FOR lnF = 1  TO lnNotesLine 
  IF 'Billing Address:' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnF)
    lnStLnAdd = lnF+1
    EXIT 
  ENDIF 
ENDFOR  
IF lnStLnAdd > 0
  lcSolTName = MLINE(eval(lcTempNotePad+'.mNotes'),lnStLnAdd)
  lnAddCnt = 1
  FOR lnCnt = lnStLnAdd TO lnNotesLine 
    IF !'Billing Address' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt)
      EXIT 
    ENDIF
    laSoldTo[lnAddCnt] = ALLTRIM(SUBSTR(MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt ),AT('Billing Address',MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt))+LEN('Billing Address')+1))  
*    MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt)
    lnAddCnt = lnAddCnt + 1
    IF lnAddCnt > 6
        EXIT 
    ENDIF
  ENDFOR 
ENDIF 
=lfAdrShift('laSoldTo')

DIMENSION laShipTo[6,1]
laShipTo = ''
laShipTo[1] = &lcOrdHdr..STName
laShipTo[2] = &lcOrdHdr..cAddress1
laShipTo[3] = &lcOrdHdr..cAddress2
laShipTo[4] = &lcOrdHdr..cAddress3
laShipTo[5] = &lcOrdHdr..cAddress4
laShipTo[6] = &lcOrdHdr..cAddress5
=lfAdrShift('laShipTo')




*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [BEGIN]
*!*	DIMENSION laCompAdd[6,1]
*!*	laCompAdd = ''
*!*	STORE '' TO lcCompName,lcCompPhon
*!*	PRIVATE lnSlct
*!*	lnSlct = SELECT(0)
*!*	SELECT(lcWareHous)
*!*	SEEK &lcPackTmp..CWARECODE
*!*	IF llPrntComp   && If no need to print the warehous data
PRIVATE lnSlct
lnSlct = SELECT(0)
*IF llPrntComp AND lcRpAddres = 'W'   && If no need to print the warehous data
  DIMENSION laCompAdd[6,1]
  laCompAdd = ''
  STORE '' TO lcCompName,lcCompPhon
  SELECT(lcWareHous)
  SEEK &lcPackTmp..CWARECODE
*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [END]

  DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
  =gfGetAdr(lcWareHous , '' , '' , '' , @laCompAdd)

  && Get the warehous name ane its phone in the variables   lcCompName , lcCompPhon
  lcCompName = &lcWareHous..CDESC  && WAREHOUS Name.
  lcCompPhon = &lcWareHous..CPHONE && WAREHOUS Phone.

  DIMENSION laCompAdd[6,1]
  *laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , '@R '+lcPhonPict)
  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
*ENDIF
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[Start]
lcBrnd =lfGetSlBnd()
DO CASE 
CASE  lcBrnd = '21'
  lcWebSite = 'bloomingdales.com/contactus'
  lcAvlTime= 'Mon - Fri 9am to 9pm, Sat 9am - 7pm, Sun 11am - 7pm'
  lcPhoneNumber= '1-800-777-0000'
CASE  lcBrnd = '11'
  lcWebSite = 'www.macys.com/contactus'
  lcAvlTime= 'Mon - Fri 9am-9pm, Sat 9am - 7pm, Sun 11am - 7pm'
  lcPhoneNumber= '1-800-289-6229'
OTHERWISE 
  lcWebSite = ''
  lcAvlTime= ''
  lcPhoneNumber= ''
ENDCASE 
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[End]
SELECT (lnSlct)


STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfHeadVar

*!**************************************************************************
*! Name      : lfGrpSetes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfGrpSetes()
*!**************************************************************************
FUNCTION lfGrpSetes
PARAMETERS llDosMode
lnGrdTotWg = 0

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)

lcSkipExpr = ''
SET SKIP TO

SELECT (lcInvLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT (lcPakLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT(lcOrdLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT (lnCurAlias)

PRIVATE lcLocExpr

DO CASE 
  CASE nRprtTyp = 1
    SELECT(lcInvLnTmp)
    lcLinFile = lcInvLnTmp

    lcPackNo = INVOICE
    lcLocExpr = "INVOICE = '" + lcPackNo + "' AND ORDER = '" + &lcORDHDR..ORDER + "'"

  CASE nRprtTyp = 2
    SELECT (lcPakLnTmp)
    lcLinFile = lcPakLnTmp

    lcPackNo = pack_no
    lcLocExpr = ""
     
  CASE nRprtTyp = 3
    SELECT(lcOrdLnTmp)
    lcLinFile = lcOrdLnTmp

    lcPackNo = "O"+ORDER
    lcLocExpr = "cORDTYPE+ORDER = '" + lcPackNo + "' AND PIKTKT = '" + &lcOrdLnTmp..PIKTKT + "'"
    
ENDCASE  
 
  lcSkipTag = ORDER()
  lcCurRec = EVALUATE(KEY())
  SET ORDER TO (lcSkipTag) DESC
  IF SEEK(lcPackNo)

    IF !EMPTY(lcLocExpr)    
      LOCATE REST WHILE &lcLocExpr
    ENDIF
    lnLastRec = RECNO()
  ENDIF
  SET ORDER TO (lcSkipTag) ASCE
  =SEEK(lcCurRec)
  SET RELATION TO  STYLE INTO &lcStyleFile ADDITIVE
SELECT (lnCurAlias)
lcSkipExpr = IIF(nRprtTyp=1,lcInvLnTmp,IIF(nRprtTyp=2,lcPakLnTmp,lcOrdLnTmp))
SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ""
*-- END OF  lfGrpSetes

*!**************************************************************************
*! Name      : lfChngScle
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To Print New Scale If Changed
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfChngScle()
*!**************************************************************************
*
FUNCTION lfChngScle
lcScale = &lcStyleFile..Scale
RETURN ''
*-- End of lfChngScle 

*!**************************************************************************
*! Name      : lfStyMasks
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfStyMasks()
*!**************************************************************************
*
FUNCTION lfStyMasks
PRIVATE lnI , lnSegLen
STORE 0 TO lnI , lnClrStart , lnSegLen
lnMajSeg = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
STORE '' TO lcMajorTlt , lcMajorPic , lcNnMajTlt , lcNnMajPic

*-- lcMajorTlt : Variable Hold Major Title    
*-- lcMajorPic : Variable Hold Major Picture      
*-- lcNnMajTlt : Variable Hold Non   Major Title    
*-- lcNnMajPic : Variable Hold Non   Major Picture  
*-- lnClrStart : Variable Hold Color Start Position
*-- lnClrLen   : Variable Hold Color Length

FOR lnI = 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScSep = IIF(lnExtScSep=0,LEN(laMajSegs[lnI,6]),lnExtScLen)    
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen) + lnExtScSep
  ENDIF
  
  *-- Evaluate Major title and picture
  lnSegLen = LEN(laMajSegs[lnI,3])
  IF lnI <= lnMajSeg
    = lfEvlMajNn(@lcMajorTlt , @lcMajorPic)
  ELSE  && else Evaluate Non Major title and picture
    = lfEvlMajNn(@lcNnMajTlt , @lcNnMajPic)
    IF laMajSegs[lnI,1] = "C"
      lnClrStart = laMajSegs[lnI,4]
      lnClrLen   = lnSegLen
    ENDIF
  ENDIF
ENDFOR
*-- End Of lfStyMasks.

*!**************************************************************************
*! Name      : lfEvlMajNn
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Passed Parameters : 1 - (lcTltValue) 
*!                     2 - (lcPicValue)
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfEvlMajNn()
*!**************************************************************************
*
FUNCTION lfEvlMajNn
PARAMETERS lcTltValue , lcPicValue
IF !EMPTY(lcTltValue)
  lcTltValue = lcTltValue + laMajSegs[lnI-1,6]
  lcPicValue = lcPicValue + laMajSegs[lnI-1,6]
ENDIF
lcTltValue = lcTltValue + PADR(laMajSegs[lnI,2],lnSegLen)
lcPicValue = lcPicValue + laMajSegs[lnI,3]
*-- End Of lfEvlMajNn.

*!**************************************************************************
*! Name      : lfNonMjDes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Non Major Code and Description
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfNonMjDes()
*!**************************************************************************
*
FUNCTION lfNonMjDes

PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes,lnAlias
STORE '' TO lcTemp , lcNonMjDes , lnAlias
lnAlias = SELECT()
SELECT(lcPackTmp)

lcStyle = IIF(&lcPackTmp..nRprtTyp=1,&lcInvLnTmp..STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,&lcOrdLnTmp..STYLE))
lcDyelot = IIF(&lcPackTmp..nRprtTyp=1,&lcInvLnTmp..dyelot,IIF(&lcPackTmp..nRprtTyp=3,&lcOrdLnTmp..Dyelot,&lcPakLnTmp..Dyelot))
lnI = 0

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  lcTemp = ''

  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSegs[lnI,1] $ "FOTQ"
      IF loICSEGVAL.SEEK(STR(lnI,1)+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])))
        lcTemp = ALLTRIM(&lcTempICSEGVAL..cISgValSd)
      ENDIF
    *-- Season, Color, Division, or lcStyle group Segment.
    CASE laMajSegs[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSegs[lnI,1] = "Z"
          lcCodeExpr = "SEASON"    
        CASE laMajSegs[lnI,1] = "C"
          lcCodeExpr = "COLOR"    
        CASE laMajSegs[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"    
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"    
      ENDCASE
      
     lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.F.))
     *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcScaleFile)
        lcTemp = ALLTRIM(&lcScaleFile..cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
 ENDFOR    && end Loop Around Non Major elements.

lcStyle    = IIF(lnExtScPos = 0,lcStyle,LEFT(lcStyle,LEN(lcStyle)-lnExtScLen))
IF llUse_config
  lcStyleExp = lcStyle
ELSE
  lcStyleExp = lcStyle+' '+lcNonMjDes
ENDIF   
SELECT(lnAlias)
RETURN ''

*-- End Of lfNonMjDes.

*!**************************************************************************
*! Name      : lfSpckln
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   :  Fill The Array With Spck_lin & Scale
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfSpckln()
*!**************************************************************************
*
FUNCTION lfSpckln
PRIVATE lcStyls , lnCount , lcCount ,lcAlias
STORE '' TO  lcStyls , lnCount , lcCount
DIMENSION laTemp[8] 

STORE "" TO laSpckTemp , laTemp
lcAlias = SELECT (0)

IF lcRpForm = "ALPKLSB" AND llPrnPack   
  IF llUse_config 
    =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style,&lcLinFile..DYELOT)
  ELSE 
    =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style,'')
  ENDIF   
ENDIF

PRIVATE lcAlasPck , lcOrdrLin , lnRcNoOrd , lcOrdrSek , lcLnNoFld
lcAlasPck = SELECT(0)
SELECT(lcOrdLnTmp)
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
IF lcOrdLnTmp <> lcTempOrdline
  SET ORDER TO TAG &lcOrdLnTmp
ELSE
  SET ORDER TO tag ordline 
ENDIF 
*SET ORDER TO TAG &lcOrdLnIndTmp
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

DO CASE
  CASE lcRpSelcBy $ "TI"
    lcLnNoFld = STR(EVAl(lcLinFile+'.LINENO'),6)
  CASE lcRpSelcBy = "P"
    lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)

ENDCASE

IF SEEK("O" + lcOrdrSek + lcLnNoFld) AND !EMPTY(&lcOrdLnTmp..PACK_ID)
    STORE SPACE(0) TO laSpckTemp[1]
    laSpckTemp[1] = "Pack ID : " + ALLTRIM(&lcOrdLnTmp..PACK_ID)
ELSE
  *--
  IF llUse_config 
    IF loSpck_lin.Seek('S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style + &lcLinFile..Dyelot)
      IF &lcTempSpck_Lin..TotQty = 0
        laSpckTemp[1]='SKU #:' + &lcTempSpck_Lin..Pack_Id
      ELSE
        lnCount = 1 
        SELECT(lcTempSpck_Lin) 
        lnSavRec = RECNO()
        FOR lnCount = 1 To 8
          GOTO lnSavRec
          lcCount = STR(lnCount, 1 )
          IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)) 
            SCAN REST WHILE TYPE+ACCOUNT+STYLE+DYELOT+PACK_ID = 'S' + &lcPackTmp..Account+&lcLinFile..Style + &lcLinFile..Dyelot;
              FOR lnCount <= 8
                IF !EMPTY(&lcTempSpck_Lin..Qty&lcCount)
                  laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',&lcScaleFile..Sz&lcCount + ':' + &lcTempSpck_Lin..Pack_Id)
                ENDIF
            ENDSCAN
          ENDIF 
        ENDFOR
      ENDIF
    ENDIF
  ELSE 
    IF loSpck_lin.SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style )
      IF &lcTempSpck_Lin..TotQty = 0
        laSpckTemp[1]='SKU #:' + &lcTempSpck_Lin..Pack_Id
      ELSE
        lnCount = 1  
        SELECT(lcTempSpck_Lin)
        lnSavRec = RECNO()
        FOR lnCount = 1 To 8
          GOTO lnSavRec
          lcCount = STR(lnCount, 1 )
          IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)) 
            SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+&lcLinFile..Style;
              FOR lnCount <= 8
                IF !EMPTY(&lcTempSpck_Lin..Qty&lcCount)
                  laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',&lcScaleFile..Sz&lcCount + ':' + &lcTempSpck_Lin..Pack_Id)
                ENDIF
            ENDSCAN
          ENDIF 
        ENDFOR
      ENDIF
    ENDIF
  ENDIF 
  SELECT(lcOrdLnTmp)
  IF BETWEEN(lnRcNoOrd,1,RECCOUNT(lcOrdLnTmp))
    GOTO lnRcNoOrd IN &lcOrdLnTmp
  ENDIF
  SET ORDER TO TAG &lcOrdrLin
  SELECT(lcAlasPck)
ENDIF

lnNtpty = 1
FOR I   = 1 TO 8
  IF !EMPTY(laTemp[I])
    laSpckTemp[lnNtpty] = laTemp[I]
    lnNtpty = lnNtpty + 1    
  ENDIF
ENDFOR 

*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[Start]
SELECT(lcOrdLnTmp)
SET ORDER TO TAG &lcOrdrLin
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[End]

SELECT (lcalias)
RETURN ''
*-- End Of lfSpckln

*!**************************************************************************
*! Name      : lfEndGroup
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Rise End Group Flag Which Control Page Footer Data.
*!**************************************************************************
*! Called from : Group fotter band.
*!**************************************************************************
*! Passed Parameters : None.
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfEndGroup()
*!**************************************************************************
FUNCTION lfEndGroup
*-- Set this variable .T. to don't print the word "CONTINUE NEXT PAGE"
*-- and then print Totals.
llEndGroup = .T.
lnLastRec = 0
RETURN ''
*-- End Of lfEndGroup.

*!**************************************************************************
*! Name      : lfScalHead
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Return The New Scales If The Scale Changed
*!**************************************************************************
*! Called from : Pomat Details Band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfScalHead()
*!**************************************************************************
*
FUNCTION lfScalHead
PRIVATE lcScalHead
lcScalHead = ""

IF lcScale != &lcStyleFile..Scale
  lcScale = &lcStyleFile..Scale
  lcScalHead = lcScale+" " + PADL(ALLTRIM(&lcScaleFile..SZ1),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ2),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ3),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ4),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ5),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ6),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ7),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ8),5)
ENDIF
RETURN lcScalHead
*-- End Of lfScalHead.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Close any opened files if user press OG <Close> Button
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .F.  

*-- Close Temporary Cursors [Begin]
IF USED (lcPackTmp )
 USE IN (lcPackTmp )
ENDIF
*-- Close Temporary Cursors [End]
*-- End Of lfClearRep.
*!**************************************************************************
*! Name      : lfSRPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To set the relations or To release the relations when browse
*!         packing list No.   
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
FUNCTION lfSRPack
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PACK_HDR
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  GO TOP
ELSE
  SELECT PACK_HDR
  SET RELATION TO
ENDIF

*!**************************************************************************
*! Name      : lfVSelcBy
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : to redisplay the element of option grad again after change the select
*!             the select by from (invoice -> pack list )or(Pack list -> invoice)
*!*************************************************************
*! Passed Parameters :
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfVSelcBy()
*!**************************************************************************
FUNCTION lfVSelcBy
llClearInv = (lcRpSelcBy # 'I')    && to clear lcRpExp 
llClearPak = (lcRpSelcBy # 'P')     && to clear lcRpExp 
llClearPik = (lcRpSelcBy # 'T')     && to clear lcRpExp 

CLEARREAD()              && to redisplay (reread)

*!**************************************************************************
*! Name      : lfSRPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To set the relations or To release the relations when browse
*!         packing list No.   
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
FUNCTION lfSRPick
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PikTkt
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  SET RELATION TO Piktkt.Order INTO ordline  ADDITIVE
  GO TOP
ELSE
  SELECT PikTkt
  SET RELATION TO
ENDIF


*!**************************************************************************
*! Name      : lfGrdTot
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Calculate Grand Total for Weight
*!**************************************************************************
FUNCTION lfGrdTot
lnGrdTotWg = LNGRDWHGT
RETURN ""

*!**************************************************************************
*! Name      : lfGetLines
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : to collect the detaile line from pack_line Into Temp File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetLines()
*!**************************************************************************
FUNCTION lfGetLines
*  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 
loPack_Lin.SETORDER('PACKSTYLE')
*SET ORDER TO PackStyle IN Pack_lin
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)
SELECT (lcPackTmp)
   SCAN 
    lnStyles = 0
    loPack_Lin.SEEK(&lcPackTmp..Pack_no)
    loPack_hdr.SEEK(&lcPackTmp..Pack_no)
    SELECT(lcTempPack_Lin)
    SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no
      *lnCartonNO = no_cart
      
      *SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no+STR(lnCartonNO,4)
        SCATTER MEMVAR MEMO
        =loOrdline.Seek('O'+&lcTempPack_hdr..Order+STR(&lcTempPack_Lin..nordlineno,6),'ORDLINE')
        =loStyle.Seek(&lcTempPack_Lin..Style)
        =loScale.Seek('S'+&lcTempStyle..Scale)
        FOR lnCntPk = 1 TO &lcTempScale..Cnt
          lcCntPk = STR(lnCntPk,1)
          IF &lcTempPack_Lin..Qty&lcCntPk. <> 0 &&OR &lcTempOrdLine..book&lcCntPk.<> 0
            loStyleupc.Seek(m.Style+lcCntPk)
            *! C201485,2 MMT 07/03/2012 use the retail price instead of gros price in calcuations[Start]
            *m.Price = &lcTempOrdLine..PRICE            
            m.Price = &lcTempOrdLine..nsugretpri
            *! C201485,2 MMT 07/03/2012 use the retail price instead of gros price in calcuations[END]
            M.desc1 = &lcTempOrdLine..desc1 
            m.OrdQty  = &lcTempOrdLine..book&lcCntPk.
            m.pQty = &lcTempPack_Lin..Qty&lcCntPk.
            m.UPC = &lcTempStyleUpc..cupcnum1+&lcTempStyleUpc..cupcnum2+&lcTempStyleUpc..cupcnum3
            m.Size =lcCntPk 
            INSERT INTO (lcPakLnTmp) FROM MEMVAR            
          ENDIF
        ENDFOR
        
        
*!*	        IF llSameCarton
*!*	          SELECT (lcPakLnTmp)
*!*	          =SEEK(m.Pack_No+STR(lnCartonNO-1,4)+m.style)
*!*	          REPLACE To_Crt  WITH lnCartonNO  ,;
*!*	                  No_Cart WITH lnCartonNO ,;
*!*	                  Qty1    WITH Qty1 + m.Qty1 ,;
*!*	                  Qty2    WITH Qty2 + m.Qty2 ,;
*!*	                  Qty3    WITH Qty3 + m.Qty3 ,;
*!*	                  Qty4    WITH Qty4 + m.Qty4 ,;
*!*	                  Qty5    WITH Qty5 + m.Qty5 ,;
*!*	                  Qty6    WITH Qty6 + m.Qty6 ,;
*!*	                  Qty7    WITH Qty7 + m.Qty7 ,;
*!*	                  Qty8    WITH Qty8 + m.Qty8 ,;
*!*	                  TotQty  WITH TotQty + m.TotQty ,;
*!*	                  Weight  WITH Weight + m.Weight
*!*	        ELSE  
*!*	          STORE m.No_Cart TO m.From_Crt,To_Crt,lnFromCrtn
*!*	          INSERT INTO (lcPakLnTmp) FROM MEMVAR
*!*	          lnStyles = lnStyles + 1
*!*	        ENDIF
      *ENDSCAN
    ENDSCAN
  ENDSCAN
*!**************************************************************************
*! Name      : lfSameCartn
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Check if the previous carton has the same information
*!**************************************************************************
*! Passed Parameters : lcPackNo    : Packinf list# 
*!                     lnCartnNo   : Carton #
*!                     lnSameLines : Previous carton total lines number
*!**************************************************************************
*! Return      : .T. If the previous carton has the same information
*!**************************************************************************
*! Example     : =lfSameCartn('000001',3,20)
*!**************************************************************************
FUNCTION lfSameCartn
PARAMETERS lcPackNo,lnCartnNo,lnSameLines
PRIVATE llSameCrtn,lnCrtnLines

llSameCrtn  = .T.
lnCrtnLines = 0

SELECT(lcTempPack_Lin)
SCAN REST WHILE pack_no+STR(no_cart,4)+style = lcPackNo+STR(lnCartnNo,4)
  =SEEK(lcPackNo+STR(lnCartnNo-1,4)+style,lcPakLnTmp)
  lnCrtnRange = &lcPakLnTmp..To_Crt - &lcPakLnTmp..From_Crt+1
  IF EOF(lcPakLnTmp) OR ;
     Qty1*lnCrtnRange  <> &lcPakLnTmp..Qty1 OR Qty2*lnCrtnRange <>  &lcPakLnTmp..Qty2 OR ;
     Qty3*lnCrtnRange  <> &lcPakLnTmp..Qty3 OR Qty4*lnCrtnRange <>  &lcPakLnTmp..Qty4 OR ;
     Qty5*lnCrtnRange  <> &lcPakLnTmp..Qty5 OR Qty6*lnCrtnRange <>  &lcPakLnTmp..Qty6 OR ;
     Qty7*lnCrtnRange  <> &lcPakLnTmp..Qty7 OR Qty8*lnCrtnRange <>  &lcPakLnTmp..Qty8 OR ;
     Weight*lnCrtnRange <> &lcPakLnTmp..Weight
    llSameCrtn = .F.
    EXIT
  ENDIF
  lnCrtnLines = lnCrtnLines + 1
ENDSCAN
llSameCrtn = llSameCrtn AND (lnCrtnLines=lnSameLines)
loPack_Lin.SEEK(lcPackNo+STR(lnCartnNo,4))
RETURN(llSameCrtn)
*!**************************************************************************
*! Name      : lfChkModls
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Cheak if moduls AL & AS instaled in the company.
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfChkModls()
*!**************************************************************************
FUNCTION lfChkModls
llModInst = .F.
llModInst = ('AL' $ oAriaApplication.CompanyInstalledModules  .OR. 'AS' $ oAriaApplication.CompanyInstalledModules  )
*-- llogtrmnat:- Variable Terminate The Option Grid.
IF !llModInst 
  =gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
  llOgTrmnat = .T.
  CLEARREAD()
  RETURN
ENDIF
*-- End Of lfChkModls.

*!**************************************************************************
*! Name      : lfGetPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Retrieve Pack ID
*!**************************************************************************
*! Passed Parameters  : lcOrder,lcLineNo
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfGetPack()
*!**************************************************************************
FUNCTION lfGetPack
PARAMETERS lcOrder,lcStore,lcStyle,lcStyDyelot

PRIVATE lcAlias,lcCurTag

STORE "" TO lcPackId

lcAlias = ALIAS()

SELECT(lcOrdLnTmp)
lcCurTag = TAG()
*B132411,1 MMT 11/13/2006 Error when you press preview [Start]
*SET ORDER TO TAG &lcOrdLnIndTmp
IF lcOrdLnTmp  = lcTempOrdline
  SET ORDER TO ORDLINST
ELSE 
  SET ORDER TO TAG &lcOrdLnIndTmp
ENDIF 
*B132411,1 MMT 11/13/2006 Error when you press preview [End]

IF llUse_config 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id) AND DYELOT = lcStyDyelot
    lcPackId = Pack_Id
  ENDIF
ELSE 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id)
    lcPackId = Pack_Id
  ENDIF
ENDIF 
SET ORDER TO &lcCurTag
SELECT &lcAlias
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
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

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) =  lcOrdLnTmp
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)'
      laIndex[1,2] = lcOrdLnIndTmp
      laIndex[2,1] = 'CORDTYPE+ORDER+STR(LINENO,6)'
      laIndex[2,2] = lcOrdLnTmp
      
   *-- temp. piktkt file lcPickTkTmp
   CASE UPPER(lcTable) =lcInvLnTmp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'INVOICE+STR(LINENO,6)'
      laIndex[1,2] = lcInvLnTmp
 
   *--temp. pikline file
   CASE UPPER(lcTable) =  lcNotePad
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+key'
      laIndex[1,2] = lcNotePad

   CASE UPPER(lcTable) =  lcStyleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

   CASE UPPER(lcTable) =  lcScalefile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+scale+prepak'
      laIndex[1,2] = lcScalefile

   CASE UPPER(lcTable) =  lcWareHous
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cWareCode'
      laIndex[1,2] = lcWareHous
   *--lcPikTemp
   CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcPikTemp
   *--lcPckHdTemp
   CASE UPPER(lcTable) =  lcPckHdTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'pack_no'
      laIndex[1,2] = lcPckHdTemp

   CASE UPPER(lcTable) =  lcInvhdTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'invoice'
      laIndex[1,2] = lcInvHdTemp

ENDCASE
*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to get the data from ordhdr and customer files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetData
SELECT (lcPackTmp)
LOCATE 

*! B127377,1 MMT 04/13/2005 , FIX BUG OF NOT PRINTING SHIP TO ADDRESS[Start]
SCAN 
  *--IF EMPTY(Store)
  =loCustomer.Seek('M'+ &lcPackTmp..Account)
  *--ELSE
   *-- loCustomer.Seek('S'+ Account + Store)
*--  ENDIF 
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'M'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
  =loCustomer.Seek('S'+ &lcPackTmp..Account)
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'S'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
ENDSCAN 
*! B127377,1 MMT 04/13/2005 , FIX BUG OF NOT PRINTING SHIP TO ADDRESS[End]
SELECT (lcPackTmp)
LOCATE 
SCAN 
  loNotepad.seek("B" + ORDER)
  SELECT(lcTempNotepad)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcNotePad) FROM MEMVAR 
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  looRDHDR.SEEK("O" + ORDER)
  SELECT(lcTempordhdr)
  SCATTER MEMO MEMVAR 
  *Get Barcode of ccontref
  INSERT INTO (lcordhdr) FROM MEMVAR 
  SELECT(lcordhdr) 
  IF !EMPTY(m.ccontref)
    m.ccontref = '*'+ALLTRIM(m.ccontref)+'*'
    lfGetConBC(m.ccontref,0,'UPCBAR')
    lfGetConBC(m.ccontref,90,'VUPCBAR')    
  endif  
  SELECT (lcPackTmp)
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loOrdLine.Seek('O'+ &lcPackTmp..order + &lcPackTmp..Store)
  SELECT(lcTempOrdLine)
  *B608381,1 WAM 12/13/2007 Get only pick ticket lines
  *SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store
   SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store ;
             FOR   piktkt = &lcPackTmp..piktkt
  *B608381,1 WAM 12/13/2007 (End)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLnTmp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 
    SELECT(lcPakLnTmp)
    SCAN 
      loStyle.Seek(Style)
      SELECT(lcStyleFile)
      IF !SEEK(&lcTempStyle..Style,lcStyleFile)
        SELECT(lcTempStyle)
        SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
      ENDIF
    ENDSCAN 

SELECT(lcStyleFile)
SCAN 
  loScale.Seek('S'+ Scale)
  IF !SEEK('S'+ &lcTempScale..Scale,lcScalefile)
    SELECT(lcTempScale)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcScaleFile) FROM MEMVAR 
  ENDIF   
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loWareHous.Seek(&lcPackTmp..cWareCode)
  IF !SEEK(&lcTempWareHous..cWareCode,lcWareHous)
    SELECT(lcTempWareHous)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcWareHous) FROM MEMVAR 
  ENDIF   
ENDSCAN 

*!*************************************************************
*! Name      : lfNoInvSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfNoInvSel

IF loInvhdr.llnative
  SELECT(lcTempInvHdr)
  SCAN FOR Status <> 'V'
    WAIT WINDOW "Collect data for Invoice #" + Invoice  + ' ...' NOWAIT
    *-- If Find this Piktik in Pack_Hdr file.
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[Start]
*!*	    IF !EMPTY(&lcTempInvHdr..PikTkt) .AND. loPack_Hdr.SEEK(&lcTempInvHdr..PikTkt)
*!*	      SELECT (lcTempPack_Hdr)
*!*	      SCATTER MEMVAR MEMO
*!*	      m.nRprtTyp  = 2
*!*	      m.Invoice   = &lcTempInvHdr..Invoice
*!*	      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempInvHdr..CWARECODE)
*!*	      m.shipvia   = &lcTempInvHdr..shipvia
   loPack_Hdr.setorder("ORDERPCK")   && ORDER+STORE+PACK_NO 
   IF loPack_Hdr.SEEK(&lcTempInvHdr..order+&lcTempInvHdr..STORE)
     SELECT (lcTempPack_Hdr)
     SCATTER MEMVAR MEMO
     m.nRprtTyp  = 1
     m.Invoice   = &lcTempInvHdr..Invoice
     M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempInvHdr..CWARECODE)
     m.shipvia   = &lcTempInvHdr..shipvia
    loPack_Hdr.setorder("Pack_Hdr")
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[End]

    ELSE  && pick ticket not found in pack header file. 
      SELECT(lcTempInvHdr)
      m.Pack_No    = Invoice
      m.Account    = Account
      m.Order      = Order        
      m.Store      = Store
      m.Note       = Note2
      m.Tot_Wght   = Weight
      m.Tot_Cart   = Cartons
      m.Tot_Pcs    = Ship
      m.CWARECODE  = CWARECODE
      m.nRprtTyp   = 1
      m.Invoice    = Invoice
      m.shipvia    = &lcTempInvHdr..shipvia
    ENDIF
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  WAIT CLEAR
  *--function to collect the lines 
*  =lfGetLines()
*  lcInvLnTmp  = lcTempInvLine
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous
ELSE 
  lcSelFile =  "Invhdr"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile  ,lcInvHdTemp,lcSelCond)  
  SELECT(lcInvHdTemp)
  SCAN
    IF !EMPTY(&lcInvHdTemp..PikTkt) .AND. loPack_Hdr.SEEK(&lcInvHdTemp..PikTkt)
      SELECT (lcTempPack_Hdr)
      SCATTER MEMVAR MEMO
      m.nRprtTyp  = 2
      m.Invoice   = &lcInvHdTemp..Invoice
      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcInvHdTemp..CWARECODE)
      m.shipvia   = &lcInvHdTemp..shipvia
    ELSE  && pick ticket not found in pack header file. 
      SELECT(lcInvHdTemp)
      m.Pack_No    = Invoice
      m.Account    = Account
      m.Order      = Order        
      m.Store      = Store
      m.Note       = Note2
      m.Tot_Wght   = Weight
      m.Tot_Cart   = Cartons
      m.Tot_Pcs    = Ship
      m.CWARECODE  = CWARECODE
      m.nRprtTyp   = 1
      m.Invoice    = Invoice
      m.shipvia    = shipvia
    ENDIF
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
   =lfGetLines()
   =lfGetData()
ENDIF 
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
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
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfNoPackSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfNoPackSel
IF loPack_hdr.llnative
*--  loPack_Hdr = CreateObject("RemoteTable","Pack_Hdr","Pack_Hdr",lcTempPack_Hdr,SET("DATASESSION")) 
  SELECT(lcTempPack_Hdr)
  SCAN FOR Status <> 'V'
    WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    SCATTER MEMVAR MEMO
    m.nRprtTyp = 2
    m.Invoice = ''
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
   * M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempORDHDR..CWARECODE)
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN
  WAIT CLEAR
  =lfGetLines()
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous

ELSE
  lcSelFile =  "Pack_hdr"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile ,lcPckHdTemp,lcSelCond)  
  SELECT(lcPckHdTemp)
  SCAN
    WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    SCATTER MEMVAR MEMO
    m.nRprtTyp = 2
    m.Invoice = ''
    IF EMPTY(M.CWARECODE)
       loOrdHdr.Seek('O' + m.Order)
       M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
   * M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,ORDHDR.CWARECODE)
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
  =lfGetLines()
  =lfGetData()

ENDIF 
*!*************************************************************
*! Name      : lfNoPickSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfNoPickSel
IF loPikTkt.llNative
  SELECT(lcTempPikTkt)
  SCAN FOR  STATUS $ "HOP" .AND. PIKTKT # '******' 
    m.Pack_No    = PikTkt
    m.Account    = Account
    m.Order      = Order        
    m.Store      = Store
    m.nRprtTyp   = 3
    m.CWARECODE =  CWARECODE
    *--Update cwarecode if empty
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN  
  WAIT CLEAR
 * =lfGetLines()
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous
ELSE
  lcSelFile =  "PikTkt"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile ,lcPikTemp,lcSelCond)  
  SELECT(lcPikTemp)
  SCAN
    m.Pack_No    = PikTkt
    m.Account    = Account
    m.Order      = Order        
    m.Store      = Store
    m.nRprtTyp   = 3
    m.CWARECODE =  CWARECODE
    *--Update cwarecode if empty
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lcTempOrdHdr..CWARECODE
    ENDIF 
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
  =lfGetLines()
  =lfGetData()

ENDIF   

*!*************************************************************
*! Name      : lfGetAddr
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 12/27/04
*! Purpose   : to CHANGE COMPANY ADDRESS TO WAREHOUS ADDRESS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetAddr
lcAlas= SELECT(0)
SELECT WAREHOUS
SEEK &lcPackTmp..CWARECODE

IF llPrntComp   && If no need to print the warehous data
  IF lcRpAddres = 'W'
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    =gfGetAdr('WAREHOUS' , '' , '' , '' , @laCompAdd)
    lcCompName = WAREHOUS.CDESC  && WAREHOUS Name.
    lcCompPhon = WAREHOUS.CPHONE && WAREHOUS Phone.
  ELSE
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    = gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)

    SELECT SYCCOMP
    SEEK gcAct_Comp
    lcCompName = cCom_Name             && Company Name.
    lcCompPhon = cCom_Phon             && Company Phone.
  ENDIF
  DIMENSION laCompAdd[6,1]
* laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)

  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
SELECT (lnSlct)

SELECT(lcAlas)

RETURN ""
*!*************************************************************
*! Name      : lfGetConBC
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/21/2012
*! Purpose   : Get Barcode
*!*************************************************************
FUNCTION lfGetConBC
lPARAMETERS  tcCUPC,lnOrient,lcFldName
lcF_name = UPPER(lcFldName)
WITH loogscroll && your form name
  IF TYPE('loogscroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name
  .&lcF_name..WIDTH         = 180
  .&lcF_name..HEIGHT        = 200
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  .&lcF_name..OBJECT.DataToEncode = tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  .&lcF_name..OBJECT.Barheight    = 0.3125*2.54
  .&lcF_name..OBJECT.SymbologyID  = 13
  .&lcF_name..OBJECT.showtext     = 1
  .&lcF_name..OBJECT.NarrowBarWidth = 0.03
  .&lcF_name..OBJECT.Wide2NarrowRatio = 3
  .&lcF_name..AddCheckDigitToText  = 0
  
  *adding Orientation to the barcode
  * Values are 0,90,180
  .&lcF_name..OBJECT.Orientation = lnOrient
  .&lcF_name..OBJECT.TopMarginCm = 0
  .&lcF_name..OBJECT.AddCheckDigit = 0
  .&lcF_name..OBJECT.LeftMarginCm = 0.2&&0.11
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
ENDWITH

*!*************************************************************
*! Name      : lfGetGiftMsg
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/21/2012
*! Purpose   : Get Gift Message
*!*************************************************************
FUNCTION lfGetGiftMsg
lcGiftMsg = ''
*lnNotesLine = MEMLINES(eval(lcTempOrdLine+'.Note_mem'))
loNotepad.seek("B" + &lcPackTmp..ORDER)              
lnNotesLine = MEMLINES(eval(lcTempNotePad+'.mNotes'))
FOR lnF = 1  TO lnNotesLine 
  IF 'Gift Wrap'  $ MLINE(eval(lcTempNotePad+'.mNotes'),lnF)
    lcGiftMsg = lcGiftMsg +  ALLTRIM(SUBSTR(MLINE(eval(lcTempNotePad+'.mNotes'),lnF),AT('Gift Wrap',MLINE(eval(lcTempNotePad+'.mNotes'),lnF))+LEN('Gift Wrap')+1))+CHR(13)+CHR(10)  
  ENDIF 
ENDFOR  
RETURN lcGiftMsg
*!*************************************************************
*! Name      : lfGetChrg
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/21/2012
*! Purpose   : Get Charges
*!*************************************************************
FUNCTION lfGetChrg
LPARAMETERS lcChrgType
lnCurAlias = SELECT()
lnChrgAmt = 0
IF !USED('ORDERCHG')
  =gfOpenTable('ORDERCHG','ORDERCHG')
ENDIF
IF gfSeek('O'+&lcPackTmp..ORDER,'ORDERCHG')
  SELECT ORDERCHG
  DO CASE 
    CASE  lcChrgType = 'S'
      SUM ORDERCHG.nchrgamnt TO lnChrgAmt REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE = 'O'+&lcPackTmp..ORDER FOR CCHRGCODE = '000184'
    CASE  lcChrgType = 'T'
      SUM ORDERCHG.nchrgamnt TO lnChrgAmt REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE = 'O'+&lcPackTmp..ORDER FOR CCHRGCODE = '000206'
    CASE  lcChrgType = 'G'      
      SUM ORDERCHG.nchrgamnt TO lnChrgAmt REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE = 'O'+&lcPackTmp..ORDER FOR CCHRGCODE = '000186'
  ENDCASE
ENDIF
SELECT(lnCurAlias)
RETURN lnChrgAmt

*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[Start]
*!*************************************************************
*! Name      : lfGetSlBnd
*: Developer : Mariam Mazhar[MMT]
*: Date      : 07/25/2012
*! Purpose   : Get Selling Brand
*!*************************************************************
FUNCTION lfGetSlBnd
lcBandcd = ''
IF !EMPTY(&lcOrdHdr..note2) AND 'Selling Brand: ' $ &lcOrdHdr..note2
  lcBandcd  = ALLTRIM(SUBSTR(&lcOrdHdr..note2,AT('Selling Brand: ',&lcOrdHdr..note2)+LEN('Selling Brand: ')))
ENDIF
RETURN lcBandcd
*! B610021,1 MMT 07/25/2012 change custom Packing list for Macy's logo and contact us[End]