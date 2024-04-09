****************************************************************************
*: Program file  : ALPKLS.PRG --- ALPKLSA.FRX
*: Program desc. : Print Packing List
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to E#301275,1
*: Date          : 06/23/1999
*:**************************************************************************
*: Calls : FUNCTIONS  : lfWOgWhen  ,lfWorkFile ,lfSRInv    ,lfAdrShift
*:					  : lfCollect  ,lfCollTime ,lfHeadVar  ,lfGrpSetes  
*:                    : lfChngScle ,lfStyMasks ,lfEvlMajNn ,lfNonMjDes
*:                    : lfSpckln   ,lfEndGroup ,lfScalHead ,lfClearRep.
*:                    : gfDispRe    ..........
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:E301275,1           : New Program code.
*:**************************************************************************
*:Modifications :
*:C200094,1	WAB 10/10/1999 Call optional function lfOptProg() - new Form
*:E301331,1 WAB 10/12/1999 We need to have the option to select packing list 
*:E301331,1 WAB            numbers to print. by Add an option : Select by 
*:E301331,1 WAB            Packing List/Invoice/Pick ticket .
*:B603278,1 WAB 11/14/1999 all lines having the same style/color should be 
*:B603278,1 WAB            printed in the same line
*:B803065,1 ABD 03/09/2000 Fix Some Bugs in the printout using extended size scale.
*:B803161,1 WAM, MAB 03/29/2000 
*:B803161,1                1- Report Prints one line for each unique Carton# and sizes
*:B803161,1                2- Grand Total for Total Weight
*:B803161,1                3- Print Order Notepad under OG Condition
*:B803161,1                For this entry we update the following
*:B803161,1                a- This Program
*:B803161,1                b- ALPKLSA.FRX
*:B803161,1                c- Add new Record to Syrepuvr for Notepad
*:B603541,1 WAM 04/02/2000 Group Cartons only if all cartons information are identical.
*:B803217,1 ABD 04/26/2000 Fix bug That no Sku's printed in the packing list
*:B803217,1 ABD            For style have different Qty.
*:B803183,1 ABD 05/02/2000 Print packing list  From So Moduls without AL modules.
*:E301412,1 SHA 05/15/2000 Added a new form.
*:B803815,1 HBG 11/13/2000 CustPo field not printed
*:C102061,1 MHM 01/30/2001 Modify to work form
*:B804094,1 ABD 05/23/2001 Fix Bug that the Company name does not print on Packing List
*:B804094,1 ABD            Settings were made on form setup to print co name 'Yes'
*:B604570,1 SSH 09/02/2001 Use OrdHdr instead of POFHDR.
*:E500436,1 TMI 09/09/2001 Show the warehouse data instead of the company data.
*:C102506,1 BWA 12/18/2001 1) Initial variable to check if print the report 
*                             from the main program or from the optional program.
*                          2) Add 2 custom fields used in custom paking list for "VITAL" 
*                             and we can use them in other customs.
*:B606322,1 BWA 08/07/2002 Fix the bug of not printing the DC.
*:B606538,1 BWA 10/15/2002 Fix the bug of printing the SKU all the time.
*:C200432,1 BWA 12/19/2002 Add variable hold the fax and variables hold the soldto address.
*:B119051,1 SSE 08/24/2003 Fix bug of not printing more than 1000 cartons, it prints ****.
*:B119051,1                All modifications is in FRX only (Program for Documentation.)
*:C123849,1 BWA 02/28/2005 Close any external relations and internal relations after finish.
*:B130118,1 HBG 11/10/2005 Print Warehous address or company address depending on option in option grid
*:B131393,1 MMR 04/03/2006 Fix bug of duplicating the PACK ID in form B.
*:**************************************************************************
*-- llRpPrtDoz : Print by dozen 'Special Instrutions Code For Dozen'
*-- lcStTime   : Variable to hold the start Time
lcStTime   = TIME()
*--C102061,1 MHM 01/30/2001 Modify to work form [Start]
*--we add record in SYREPUVR for lcFormName 
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)

*C123849,1 BWA 02/28/2005 Close any external relations and internal relations after finish.[START]
PRIVATE lcPosAlis
lcPosAlis = SELECT(0)
IF USED('POSLN')
  SELECT POSLN
  SET RELATION TO
ENDIF
SELECT(lcPosAlis)
*C123849,1 BWA 02/28/2005.[END]

*:C102061,1 MHM 01/30/2001 Modify to work form [End]

*C200432,1 BWA 12/19/2002 Add variable hold the fax number.[START]
lcCompFax = TRANSFORM(SYCCOMP.cCom_Fax , lcPhonPict)  && && Variable hold the Company Fax.
*C200432,1 BWA 12/19/2002.[END]

*C102506,1 BWA 09/20/2000 initial variable to check if print the report 
*                         from the main program or from the optional prorgram.[START]
*                         "llALPakLst" it will be a commen variable for all forms.
llALPakLst = .T.
*C102506,1 [END]

*-- Define variables that defined in OG for this report [Begin]
*B803065,1 ABD Define New Variable lcStyleExp and new array . [ Begin ]
*STORE ''TO lcDivLName, lcCompName, lcColor, lcScale, lcNotes, lcTitle,;
*           laSpckTemp, lcPackNo,lcLinFile
DIMENSION laSpckTemp[8] , laTemp[8] 

*B804094,1 ABD Fix Bug that the Company name does not print on Packing List
*B804094,1 ABD Settings were made on form setup to print co name 'Yes'
*STORE ''TO lcDivLName, lcCompName, lcColor, lcScale, lcNotes, lcTitle,;
*           laSpckTemp, laTemp , lcPackNo,lcLinFile,lcStyleExp
STORE ''TO lcColor, lcScale, lcNotes, lcTitle,;
           laSpckTemp, laTemp , lcPackNo,lcLinFile,lcStyleExp
*B804094,1 ABD [End]

*:B803161,1 MAB Intialize lcLinFile to detect last Lines record when printing Notepad [Begin]
lnGrdTotWg = 0
DO CASE 
  CASE lcRpSelcBy = "I"
    lcLinFile = "INVLINE"
  CASE lcRpSelcBy = "P"
    lcLinFile = lcPakLnTmp
  CASE lcRpSelcBy = "T"
    lcLinFile = "ORDLINE"
ENDCASE  
*:B803161,1 MAB Intialize lcLinFile to detect last Lines record when printing Notepad [End  ]

*B803065,1 ABD [ END ]
llEndGroup = .F.
lnLastRec  = 0
*:E301412,1 SHA(Begin)Added to initialize the var. that holds the pack ID  
STORE SPACE(16) TO lcPackId 
*:E301412,1 SHA(End)


SELECT PACK_HDR
GOTO TOP
IF EOF()
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF
*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
 *-- Collecting Code...
 IF !USED(lcPackTmp) OR (RECCOUNT(lcPackTmp) > 0)
   =lfWorkFile()
 ENDIF
 lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
 *C200094,1 WAB - add "Pack_no" into lcrpexp in case of select pack no
 *C200094,1 WAB - START
 *lcRpExp = IIF(lcRpExp=".T.","Invoice= '' ",lcRpExp) + [ AND (Status <> "V")]
 lcRpExp = IIF(lcRpExp=".T.",IIF(lcRpSelcBy='I',"Invoice= '' ",IIF(lcRpSelcBy='P',"PACK_NO = '' ","PIKTKT = '' ")),lcRpExp) +;
           [ AND (Status <> "V")]
 *C200094,1 WAB - END
 =lfCollect()               && Collect Aging data.
ENDIF
*-- if user change filter criteria then you must collect data again [End  ]

*-- Asking if no records (Display message) otherwise print report [Begin.]
SELECT (lcPackTmp)
IF RECCOUNT(lcPackTmp) = 0
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF

*-- Asking if no records (Display message) otherwise print report [End.]

SELECT INVHDR 
SET ORDER TO TAG INVHDR

SELECT (lcPackTmp)

*C200094,1 WAB - add relation between temp file and piktkt
*C200094,1 WAB - START
*SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                PACK_NO               INTO PACK_HDR   ,; 
                INVOICE               INTO INVHDR     ,; 
                "O" + ORDER           INTO ORDHDR     ,;
                "B" + ORDER           INTO NOTEPAD    
*B803815,1 HBG 11/13/2000 Set relation between temp Pack_Hdr file and OrdLine 
*B803815,1                to print CustPo field from OrdLine in case of MultiPo[Begin]
*SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                PACK_NO               INTO PACK_HDR   ,; 
                INVOICE               INTO INVHDR     ,; 
                PikTkt				  INTO PIKTKT	  ,;
                "O" + ORDER           INTO ORDHDR     ,;
                "B" + ORDER           INTO NOTEPAD          
SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                PACK_NO               INTO PACK_HDR   ,; 
                INVOICE               INTO INVHDR     ,; 
                PikTkt				  INTO PIKTKT	  ,;
                "O" + ORDER           INTO ORDHDR     ,;
                "O" + ORDER + STORE   INTO ORDLINE    ,;
                "B" + ORDER           INTO NOTEPAD               
*:B803815,1 [End]                
*C200094,1 WAB - END

SELECT INVHDR 
SET RELATION TO INVOICE INTO INVLINE

SELECT PACK_HDR
*B603278,1 WAB - we used a temp file instead of pack_lin
*B603278,1 WAB - START
*SET RELATION TO PACK_NO INTO PACK_LIN
SET RELATION TO PACK_NO INTO (lcPakLnTmp) 
*B603278,1 WAB - END

SELECT STYLE
SET RELATION TO  "S" + scale  INTO Scale 

*C200094,1 WAB - add relation Betwwen ordrer header and order line
*C200094,1 WAB - START
*B803815,1 HBG 11/13/2000 No need to a relation between OrdHdr & OrdLine 
*803815,1                because both have arelations with temp Pack_Hdr file [Begin]
*SELECT ORDHDR
*SET RELATION TO  "O" + ORDER  INTO ORDLINE
*:B803815,1 [End]
*C200094,1 WAB - END
SELECT (lcPackTmp)
GOTO TOP
*--- Call the FRX.

*B803065,1 ABD-Remark the next lines and get the title without sacle 
*B803065,1 ABD-If company extended size scale. [ Begin ]
*lcItemTitl = PADR(lcMajorTlt,LEN(lcMajorPic)) +;
*             PADR(lcNnMajTlt,LEN(lcNnMajPic))

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
*B803065,1 ABD  [ END ]
*C200094,1 WAB - Call optional function  to add relation to temp file
*C200094,1 WAB - START
=lfOptProg()
*C200094,1 WAB - END

*-- [ BEGIN] Printing code.
*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.

*C102506,1 BWA 12/18/2001 Check if print the report from the main program or the optional program.[START]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2
IF llALPakLst
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2
ENDIF
*C102506 BWA 12/18/2001.[END]

*B803161,1 WAM Sort by Carton No. Index [Begin]
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)
*B803161,1 WAM Sort by Carton No. Index [End  ]

*--C102061,1 MHM 01/30/2001 Modify to work form [Start]
*--We add llNoRec in SYREPUVR.DBF and get initiale value .F. 
IF llIsAparel
  lnOldpLen = _pLength 
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT    
  ENDIF
  _pLength  = lnOldpLen 
ELSE
  *C102506,1 BWA 12/18/2001 Check if print the report from the main program or the optional prorgram.[START]
  *DO gfDispRe WITH EVAL('lcFormName')
  IF llALPakLst
    DO gfDispRe WITH EVAL('lcFormName')
  ENDIF
  *C102506 BWA 12/18/2001.[END]
ENDIF  
*DO gfDispRe WITH EVAL('lcRpForm')
*--C102061,1 MHM 01/30/2001 Modify to work form [end]

*C123849,1 BWA 02/28/2005 Close any external relations and internal relations after finish.[START]
IF USED('POSLN')
  SELECT POSLN
  SET RELATION TO
ENDIF
SELECT STYLE
SET RELATION TO
SELECT INVHDR
SET RELATION TO

SELECT (lcPackTmp)
IF USED(lcPackTmp)
  SELECT (lcPackTmp)
  SET RELATION TO
ENDIF
*C123849,1 BWA 02/28/2005.[END]

*-- [ END ] Of Report Code.

*!**************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Name      : lfWOgWhen()    
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 06/23/1999
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
*-- 
IF TYPE('lcBuild') = 'N'
  lcBuild = 'OK'
  SELECT PACK_HDR
  DIMENSION laTempStru[1,4]
  lnFldLen = AFIELDS(laTempStru)
  DIMENSION laTempStru[lnFldLen + 2 , 4]

  *C200094,1 WAB - change the type and the name of the flag field 
  *C200094,1 WAB - nRprtTyp = 1  -> from invoice
  *C200094,1 WAB - nRprtTyp = 2  -> from Packing list 
  *C200094,1 WAB - nRprtTyp = 3  -> from Pick Ticket
  *C200094,1 WAB - START
  *laTempStru[lnFldLen + 1 , 1] = 'lIsInvoice'
  *laTempStru[lnFldLen + 1 , 2] = 'L'
  laTempStru[lnFldLen + 1 , 1] = 'nRprtTyp'
  laTempStru[lnFldLen + 1 , 2] = 'N'
  *C200094,1 WAB - END
  laTempStru[lnFldLen + 1 , 3] = 1
  laTempStru[lnFldLen + 1 , 4] = 0

  laTempStru[lnFldLen + 2 , 1] = 'Invoice'
  laTempStru[lnFldLen + 2 , 2] = 'C'
  laTempStru[lnFldLen + 2 , 3] = 6
  laTempStru[lnFldLen + 2 , 4] = 0

  *B603278,1 WAB - get the structured of the pack_lin file 
  *B603278,1 WAB - start
  SELECT PACK_LIN
  DIMENSION laPckLinSt[1,4]
  =AFIELDS(laPckLinSt)


  *C102506,1 BWA 12/18/2001 Add 2 custom fields used in custom paking list for "VITAL" 
  *                         and we can use them in other customs.[START]
  lnFldLnPck = AFIELDS(laPckLinSt)
  DIMENSION laPckLinSt[lnFldLnPck + 2 , 4]

  *-- Field for the printing lines.
  laPckLinSt[lnFldLnPck + 1 , 1] = 'llPrint'
  laPckLinSt[lnFldLnPck + 1 , 2] = 'L'
  laPckLinSt[lnFldLnPck + 1 , 3] = 1
  laPckLinSt[lnFldLnPck + 1 , 4] = 0

  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 2 , 1] = 'lcType'
  laPckLinSt[lnFldLnPck + 2 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 2 , 3] = 1
  laPckLinSt[lnFldLnPck + 2 , 4] = 0
  *C102506,1 BWA 12/18/2001.[END]

  *B603278,1 WAB - END
  =lfWorkFile()  && Fill arrays then create files.

  *-- llHaveLogo : If Company Have a Logo. 
  llHaveLogo = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')

  *C200432,1 BWA 12/19/2002 Add variables hold the soldto address.[START]
  *DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2]
  DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2] , laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  *C200432,1 BWA 12/19/2002.[END]

  laCompAdd = ''           && Array to hold the Company address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  = gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
  
  DIMENSION laCompAdd[6,1]
  laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [End].
ENDIF
*-- End Of lfWOgWhen.

*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 06/23/1999
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
*-- First Close all temporary files if it was opened before [Begin]
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
ENDIF
*-- First Close all temporary files if it was opened before [ End  ]
CREATE CURSOR (lcPackTmp) FROM ARRAY laTempStru
SELECT (lcPackTmp)
*-- You Must Make A Zap To decoument The File After Cursor on The Hard Disk
ZAP
INDEX ON PACK_NO TAG (lcPackTmp) OF (gcWorkDir+lcPackTmp+'.CDX')


*B603278,1 WAB - create a cursor with the same structure of the pack_lin File
*B603278,1 WAB - START
*-- First Close all temporary files if it was opened before [Begin]
IF USED(lcPakLnTmp)
  USE IN (lcPakLnTmp)
ENDIF

CREATE CURSOR (lcPakLnTmp) FROM ARRAY laPckLinSt
SELECT (lcPakLnTmp)
ZAP
INDEX ON PACK_NO+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp) OF (gcWorkDir+lcPakLnTmp+'.CDX')

*B603541,1 Group Cartons only if all cartons information are identical.
*B803161,1 WAM Add new index by Carton# [Begin]
*INDEX ON pack_no+STR(from_crt,4)+STR(to_crt,4)+style TAG 'PACKCRTN' OF (gcWorkDir+lcPakLnTmp+'.CDX') ADDITIVE
INDEX ON pack_no+STR(no_cart,4)+style TAG 'PACKCRTN' OF (gcWorkDir+lcPakLnTmp+'.CDX') ADDITIVE
*B803161,1 WAM Add new index by Carton# [End  ]
*B603541,1 (End)

*-- End Of lfWorkFile.

*!**************************************************************************
*! Name      : lfSRInv   
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 06/23/1999
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
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 06/23/1999
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

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift

*!**************************************************************************
*! Name      : lfCollect
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
*! Purpose   : For Collecting Data In The First Time.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : = lfCollect()
*!**************************************************************************
*
FUNCTION lfCollect

*C200094,1 WAB -  we have 3 choice insted of 2 choice so we used do case insted of 
*C200094,1 WAB -  if statment

  *E301331,1 WAB - ADD IF statment to colect data from invoice file in Case 
  *E301331,1 WAB - of select by invoice  
  *E301331,1 WAB - START
*IF lcRpSelcBy = 'I'			&& select by invoice		
**E301331,1 WAB - END
DO CASE 
  CASE lcRpSelcBy = 'I'			&& select by invoice		
*C200094,1 WAB - END
    SELECT INVHDR
    SET ORDER TO
    SCAN FOR &lcRpExp
      WAIT WINDOW "Collect data for Invoice #" + Invoice  + ' ...' NOWAIT
      SELECT PACK_Hdr
      *-- If Find this Piktik in Pack_Hdr file.
      IF !EMPTY(INVHDR.PikTkt) .AND. SEEK(INVHDR.PikTkt)
        SCATTER MEMVAR MEMO
        *C200094,1 WAB - the flag field is m.nRprtTyp 
        *C200094,1 WAB - start
        *m.lIsInvoice = .F.
        m.nRprtTyp = 2
        *C200094,1 WAB - end
        m.Invoice = InvHdr.Invoice
        *E500436,1 TMI [Start] Update cwarecode if empty
        M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,INVHDR.CWARECODE)
        *E500436,1 TMI [End  ]
        
      ELSE  && pick ticket not found in pack header file. 
        SELECT INVHDR
        m.Pack_No    = Invoice
        m.Account    = Account
        m.Order      = Order        
        m.Store      = Store
        m.Note       = Note2
        m.Tot_Wght   = Weight
        m.Tot_Cart   = Cartons
        m.Tot_Pcs    = Ship
        *E500436,1 TMI [Start] assign the variable m.CWARECODE the warehous code so the field is filled in the temp work file
        m.CWARECODE =  CWARECODE
        *E500436,1 TMI [End  ]        

        *C200094,1 WAB - the flag field is nRprtTyp
        *C200094,1 WAB - start
        *m.lIsInvoice = .T.
        m.nRprtTyp = 1
        *C200094,1 WAB - End
        m.Invoice = Invoice
      ENDIF
      INSERT INTO (lcPackTmp) FROM MEMVAR
    ENDSCAN
    WAIT CLEAR
  *E301331,1 WAB - colect data from Pack_hdr file only in Case of select by 
  *E301331,1 WAB - Packing List
  *E301331,1 WAB - START

  *C200094,1 WAB - use do case insted of if statment cuse there are 3 choice
  *ELSE
  CASE lcRpSelcBy = 'P'			&& select by Pack List		
  *C200094,1 WAB - END
    SELECT PACK_Hdr
    SCAN FOR &lcRpExp
      WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
      SCATTER MEMVAR MEMO
      *C200094,1 WAB - the flag field is nRprtTyp
      *C200094,1 WAB - END
      *m.lIsInvoice = .F.
      m.nRprtTyp = 2
      *C200094,1 WAB - END
      m.Invoice = ''
      *E500436,1 TMI [Start] Update cwarecode if empty
      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,ORDHDR.CWARECODE)
      *E500436,1 TMI [End  ]
      INSERT INTO (lcPackTmp) FROM MEMVAR
    ENDSCAN
    WAIT CLEAR
  *C200094,1 WAB - colect data from PICKTKT
  CASE lcRpSelcBy = 'T'			&& select by pickticket
    SELECT PIkTkt
    SCAN FOR &lcRpExp
      m.Pack_No    = PikTkt
      m.Account    = Account
      m.Order      = Order        
      m.Store      = Store
      m.nRprtTyp   = 3
      *E500436,1 TMI [Start] assign the variable m.CWARECODE the warehous code so the field is filled in the temp work file
      m.CWARECODE =  CWARECODE
      *--Update cwarecode if empty
      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,ORDHDR.CWARECODE)
      *E500436,1 TMI [End  ]        
      INSERT INTO (lcPackTmp) FROM MEMVAR
    ENDSCAN
ENDCASE
*ENDIF
*C200094,1 WAB - END
*B603278,1 WAB - call function to collect the line into temp file in case of paking list
*B603278,1 WAB - START
=lfGetLines()
*B603278,1 WAB - END

*-- END OF lfCollect.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
*
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
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
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
*
FUNCTION lfHeadVar
*B604570,1 SSH 09/02/2001 (Begin) Use OrdHdr instead of POFHDR.
*= gfRltFld(POFHDR.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
= gfRltFld(OrdHdr.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
*B604570,1 SSH 09/02/2001 (End)
DIMENSION laShipTo[5,1]
laShipTo = ''

*C200432,1 BWA 12/19/2002 Add variables hold the soldto address.[START]
DIMENSION laSoldTo[5,1]
laSoldTo = ''

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfAdrShift('laSoldTo')
*C200432,1 BWA 12/19/2002.[END]

*B606322,1 BWA 08/07/2002 Fix the bug of not printing the DC.[START]
PRIVATE lcDistCntr , lcAlasCust
lcAlasCust = SELECT(0)
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  DIMENSION laShipTo[6,1]
  laShipTo = ''
  laShipTo[1] = ORDHDR.STName
  laShipTo[2] = ORDHDR.cAddress1
  laShipTo[3] = ORDHDR.cAddress2
  laShipTo[4] = ORDHDR.cAddress3
  laShipTo[5] = ORDHDR.cAddress4
  laShipTo[6] = ORDHDR.cAddress5
  =lfAdrShift('laShipTo')
ELSE
  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF

  *B606322,1 BWA 08/07/2002.[END]

  =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)
  =lfAdrShift('laShipTo')

  DIMENSION laShipTo[6,1]
  =AINS(laShipTo,1)

  *C200094,1 WAB - [Ship to] is field Btname not Stname
  *C200094,1 WAB - START
  *laShipTo[1,1] = Customer.BtName
  laShipTo[1,1] = Customer.StName
  =lfAdrShift('laShipTo')
  *C200094,1 WAB - END

  *B606322,1 BWA 08/07/2002 Fix the bug of not printing the DC.[START]
  IF BETWEEN(lnCusRec , 1 , RECCOUNT('CUSTOMER'))
    GOTO lnCusRec IN CUSTOMER
  ENDIF
ENDIF
SELECT(lcAlasCust)
*B606322,1 BWA 08/07/2002.[END]

*E500436,1 TMI [Start] Fill the laCompAdd array with the warehous data
DIMENSION laCompAdd[6,1]
laCompAdd = ''
STORE '' TO lcCompName,lcCompPhon

PRIVATE lnSlct
lnSlct = SELECT(0)
SELECT WAREHOUS
SEEK &lcPackTmp..CWARECODE

IF llPrntComp   && If no need to print the warehous data
  *B130118,1 HBG 11/10/2005 Print Warehous address or company address depending on option in option grid[Begin]
  IF lcRpAddres = 'W'
  *B130118,1 [End]
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    =gfGetAdr('WAREHOUS' , '' , '' , '' , @laCompAdd)

    && Get the warehous name ane its phone in the variables   lcCompName , lcCompPhon
    lcCompName = WAREHOUS.CDESC  && WAREHOUS Name.
    lcCompPhon = WAREHOUS.CPHONE && WAREHOUS Phone.
  *B130118,1 HBG 11/10/2005 Print Warehous address or company address depending on option in option grid[Begin]
  ELSE
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    = gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)

    SELECT SYCCOMP
    SEEK gcAct_Comp
    lcCompName = cCom_Name             && Company Name.
    lcCompPhon = cCom_Phon             && Company Phone.
  ENDIF
  *B130118,1 [End]
  DIMENSION laCompAdd[6,1]
  laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)

  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
SELECT (lnSlct)
*E500436,1 TMI [End  ]

STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfHeadVar

*!**************************************************************************
*! Name      : lfGrpSetes
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfGrpSetes()
*!**************************************************************************
*
FUNCTION lfGrpSetes
PARAMETERS llDosMode

*!B803161,1 MAB Add New Variable to print Grand Total [Begin]
lnGrdTotWg = 0
*!B803161,1 MAB Add New Variable to print Grand Total [End  ]

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)

lcSkipExpr = ''
SET SKIP TO

SELECT INVLINE
SET RELATION OFF INTO STYLE

*B603278,1 WAB - we used a temp file instead of pack_lin
*B603278,1 WAB - START
*SELECT PACK_LIN
*SET RELATION OFF INTO STYLE
SELECT (lcPakLnTmp)
SET RELATION OFF INTO STYLE
*B603278,1 WAB - END

*C200094,1 WAB - off the relation between style and ordline
SELECT ORDLINE
SET RELATION OFF INTO STYLE
*C200094,1 WAB -end 
SELECT (lnCurAlias)
*C200094,1 WAB - commit this line and use do case cause we have 3 choice
*C200094,1 WAB - START
*IF lIsInvoice
*  SELECT INVLINE
*  lcLinFile = "INVLINE"
*ELSE
*  SELECT PACK_LIN
*  lcLinFile = "PACK_LIN"
*ENDIF

*:B803161,1 MAB Define variable for locate expression used in cases of  [Begin]
*:B803161,1 MAB By Invoice or By Pick ticket
PRIVATE lcLocExpr
*:B803161,1 MAB Define variable for locate expression used in cases of  [End  ]

DO CASE 
  CASE nRprtTyp = 1
    SELECT INVLINE
    lcLinFile = "INVLINE"

    *:B803161,1 MAB Detect last record in all cases  [Begin]
    lcPackNo = INVOICE
    lcLocExpr = "INVOICE = '" + lcPackNo + "' AND ORDER = '" + ORDHDR.ORDER + "'"
    *:B803161,1 MAB Detect last record in all cases  [End  ]

  CASE nRprtTyp = 2
    *B603278,1 WAB - we used a temp file instead of pack_lin
    *B603278,1 WAB - START
    *SELECT PACK_LIN
    *lcLinFile = "PACK_LIN"
    SELECT (lcPakLnTmp)
    lcLinFile = lcPakLnTmp

    *:B803161,1 MAB Detect last record in all cases  [Begin]
    lcPackNo = pack_no
    lcLocExpr = ""
    *:B803161,1 MAB Detect last record in all cases  [End  ]
     
    *B603278,1 WAB - END
  CASE nRprtTyp = 3
    SELECT ORDLINE
    lcLinFile = "ORDLINE"

    *:B803161,1 MAB Detect last record in all cases  [Begin]
    lcPackNo = "O"+ORDER
    lcLocExpr = "cORDTYPE+ORDER = '" + lcPackNo + "' AND PIKTKT = '" + ORDLINE.PIKTKT + "'"
    *:B803161,1 MAB Detect last record in all cases  [End  ]
    
ENDCASE  
*C200094,1 WAB - END

*:B803161,1 MAB Comment IF statement, to Detect last record in all Windows and DOS modes  [Begin]
* IF llDosMode
*  lcPackNo = &lcPackTmp..pack_no
  lcSkipTag = ORDER()
  lcCurRec = EVALUATE(KEY())
  SET ORDER TO (lcSkipTag) DESC
  IF SEEK(lcPackNo)

    *:B803161,1 MAB if not packing list locate correct line in master table [Begin]
    IF !EMPTY(lcLocExpr)    
      LOCATE REST WHILE &lcLocExpr
    ENDIF
    *:B803161,1 MAB if not packing list locate correct line in master table [End  ]

    lnLastRec = RECNO()
  ENDIF
  SET ORDER TO (lcSkipTag) ASCE
  =SEEK(lcCurRec)
* ENDIF
*:B803161,1 MAB Comment IF statement, to Detect last record in all Windows and DOS modes  [End  ]

SET RELATION TO  STYLE INTO STYLE ADDITIVE

SELECT (lnCurAlias)
*C200094,1 WAB - add the file name in case 'from piktkt
*C200094,1 WAB - START
*lcSkipExpr = IIF(lIsInvoice,"INVLINE","PACK_LIN")
*B603278,1 WAB - we used a temp file instead of pack_lin
*B603278,1 WAB - START
*lcSkipExpr = IIF(nRprtTyp=1,"INVLINE",IIF(nRprtTyp=2,"PACK_LIN","ORDLINE"))
lcSkipExpr = IIF(nRprtTyp=1,"INVLINE",IIF(nRprtTyp=2,lcPakLnTmp,"ORDLINE"))
*B603278,1 WAB - END
*C200094,1 WAB - END
SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ""
*-- END OF  lfGrpSetes

*!**************************************************************************
*! Name      : lfChngScle
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 06/23/1999
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
lcScale = Style.Scale
RETURN ''
*-- End of lfChngScle 

*!**************************************************************************
*! Name      : lfStyMasks
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/06/1999
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
*B803065,1 ABD Get the scale Pos. And Scale Length if company extended size scale. [ Begin ]
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScSep = IIF(lnExtScSep=0,LEN(laMajSegs[lnI,6]),lnExtScLen)    
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen) + lnExtScSep
  ENDIF
*B803065,1 ABD [ End ]
  
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
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/06/1999
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
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/06/1999
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

*B803065,1 ABD Define New Variable lcStyleExp. [ Begin ]
*PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes
*STORE '' TO lcTemp , lcNonMjDes
PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes,lnAlias
STORE '' TO lcTemp , lcNonMjDes , lnAlias
lnAlias = SELECT()
SELECT(lcPackTmp)
*B803065,1 ABD [ END ]

*C200094,1 WAB - START
*lcStyle = IIF(&lcPackTmp..lIsInvoice,INVLINE.STYLE,PACK_LIN.STYLE)
*B603278,1 WAB - START
*lcStyle = IIF(&lcPackTmp..nRprtTyp=1,INVLINE.STYLE,IIF(&lcPackTmp..nRprtTyp=2,PACK_LIN.STYLE,ORDLINE.STYLE))
lcStyle = IIF(&lcPackTmp..nRprtTyp=1,INVLINE.STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,ORDLINE.STYLE))
*B603278,1 WAB - END
*C200094,1 WAB - end
lnI = 0

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  lcTemp = ''

  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSegs[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"ICSEGVAL")
        lcTemp = ALLTRIM(ICSEGVAL.cISgValSd)
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
      
      *B803065,1 ABD Remark the next lines and call the function with .f. 
      *B803065,1 ABD to get the color desc only. [ Begin  ]
      *lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.T.))
      lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.F.))
      *B803065,1 ABD  [ End ]
    *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"SCALE")
        lcTemp = ALLTRIM(SCALE.cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
  *B803065,1 ABD  Remark the next lines .[ Begin ]
  *lcNonMjDes =  SUBSTR(lcNonMjDes,8,15)
  *B803065,1 ABD [ END ]
ENDFOR    && end Loop Around Non Major elements.

*B803065,1 ABD Get the Style,Color and Color Desc. [ Begin  ]
*RETURN lcNonMjDes
lcStyle    = IIF(lnExtScPos = 0,lcStyle,LEFT(lcStyle,LEN(lcStyle)-lnExtScLen))
lcStyleExp = lcStyle+' '+lcNonMjDes
SELECT(lnAlias)
RETURN ''
*B803065,1 ABD [ END ]

*-- End Of lfNonMjDes.

*!**************************************************************************
*! Name      : lfSpckln
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/12/1999
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
*C200094,1 WAB - START
*lcStyls = IIF (&lcPackTmp..lIsInvoice,INVLINE.STYLE,PACK_LIN.STYLE)
*B603278,1 WAB - we used a temp file instead of pack_lin
*B603278,1 WAB - START
*lcStyle = IIF(&lcPackTmp..nRprtTyp=1,INVLINE.STYLE,IIF(&lcPackTmp..nRprtTyp=2,PACK_LIN.STYLE,ORDLINE.STYLE))

*B803065,1 ABD-Remark the next lines and get the lcStyls Not lcStyle becouse it seek with lcStyls. [ Begin ]
*lcStyle = IIF(&lcPackTmp..nRprtTyp=1,INVLINE.STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,ORDLINE.STYLE))
*B803217,1 ABD Remark the next lines & seek with correct style.  [Begin]
*lcStyls  = IIF(&lcPackTmp..nRprtTyp=1,INVLINE.STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,ORDLINE.STYLE))
*B803217,1 ABD [End]
*B803065,1 ABD [ End ] 

*B603278,1 WAB - END
*C200094,1 WAB - end

*:E301412,1 SHA(Begin)Added to get the pack ID
IF lcRpForm = "ALPKLSB" AND llPrnPack   
  =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style)
ENDIF
*:E301412,1 SHA(End)

*B606538,1 BWA 10/15/2002 Fix the bug of printing the SKU all the time.[START]
PRIVATE lcAlasPck , lcOrdrLin , lnRcNoOrd , lcOrdrSek , lcLnNoFld
lcAlasPck = SELECT(0)
SELECT ORDLINE
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
SET ORDER TO TAG ORDLINE
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

DO CASE
  CASE lcRpSelcBy $ "TI"
    lcLnNoFld = STR(EVAl(lcLinFile+'.LINENO'),6)
  CASE lcRpSelcBy = "P"
    *ashraf on 11/21/2002
    *lcLnNoFld = STR(EVAl(lcLinFile+'.LINE_NO'),6)
    lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)

ENDCASE

IF SEEK("O" + lcOrdrSek + lcLnNoFld) AND !EMPTY(ORDLINE.PACK_ID)
  *B131393,1 MMR 04/03/2006 Fix bug of duplicating the PACK ID in form B.[Start]
  IF lcRpForm <> "ALPKLSB"
  *B131393,1 MMR.[End]
    STORE SPACE(0) TO laSpckTemp[1]
    laSpckTemp[1] = "Pack ID : " + ALLTRIM(ORDLINE.PACK_ID)
  *B131393,1 MMR 04/03/2006 Fix bug of duplicating the PACK ID in form B.[Start]
  ENDIF
  *B131393,1 MMR.[End]
ELSE
*B606538,1 BWA 10/15/2002.[END]

  *B803217,1 ABD Remark the next lines & seek with correct style.  [Begin]
  *IF SEEK( 'S' + &lcPackTmp..ACCOUNT + lcStyls , 'Spck_Lin' )
  IF SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style, 'Spck_Lin' )
  *B803217,1 ABD [End]

    IF Spck_Lin.TotQty = 0
      laSpckTemp[1]='SKU #:' + Spck_Lin.Pack_Id
    ELSE
      *B803217,1 ABD Fix bug That no Sku's printed in the packing list
      *B803217,1 ABD For style have different Qty.  [Begin]
      *lnCount = 1  
      *SELECT Spck_Lin
      *B603278,1 WAB - we used a temp file instead of pack_lin
      *B603278,1 WAB - START
      *SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+ Pack_lin.Style;
      *FOR lnCount <= 8
      *SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+ &lcPakLnTmp..Style;
      *FOR lnCount <= 8
      *B603278,1 WAB - END
      *lcCount = STR(lnCount, 1 )
      *B803065,1 ABD Remark the next lines and get sku# if Not empty Qty in temp array.[ Begin ]
      *laSpckTemp[lnCount] = Scale.Sz&lcCount + ':' + Spck_Lin.Pack_Id
      *laTemp[lnCount] =IIF(EMPTY(EVAL(lcPakLnTmp+'.Qty'+lcCount)),'',Scale.Sz&lcCount + ':' + Spck_Lin.Pack_Id)
      *B803065,1 ABD [ End ]
      *lnCount = lnCount + 1
      *ENDSCAN
      lnCount = 1  
      SELECT Spck_Lin
      lnSavRec = RECNO()
      FOR lnCount = 1 To 8
        GOTO lnSavRec
        lcCount = STR(lnCount, 1 )
        IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)) 
          SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+&lcLinFile..Style;
            FOR lnCount <= 8
              IF !EMPTY(Spck_Lin.Qty&lcCount)
                laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',Scale.Sz&lcCount + ':' + Spck_Lin.Pack_Id)
              ENDIF
          ENDSCAN
        ENDIF 
      ENDFOR
      *B803217,1 ABD [End]
    ENDIF
  ENDIF

*B606538,1 BWA 10/15/2002 Fix the bug of printing the SKU all the time.[START]
SELECT ORDLINE
IF BETWEEN(lnRcNoOrd,1,RECCOUNT('ORDLINE'))
  GOTO lnRcNoOrd IN ORDLINE
ENDIF
SET ORDER TO TAG &lcOrdrLin
SELECT(lcAlasPck)
ENDIF
*B606538,1 BWA 10/15/2002.[END]

*B803065,1 ABD fill the Stander array with value not empty.[ Begin ]
lnNtpty = 1
FOR I   = 1 TO 8
  IF !EMPTY(laTemp[I])
    laSpckTemp[lnNtpty] = laTemp[I]
    lnNtpty = lnNtpty + 1    
  ENDIF
ENDFOR 
*B803065,1 ABD [ End ]

SELECT (lcalias)
RETURN ''
*-- End Of lfSpckln

*!**************************************************************************
*! Name      : lfEndGroup
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
*! Purpose   : Rise End Group Flag Which Control Page Footer Data.
*!**************************************************************************
*! Called from : POMAT Group fotter band.
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
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/01/1999
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
IF lcScale != Style.Scale
  lcScale = Style.Scale
  lcScalHead = lcScale+" " + PADL(ALLTRIM(Scale.SZ1),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ2),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ3),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ4),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ5),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ6),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ7),5)+" " +;
                             PADL(ALLTRIM(Scale.SZ8),5)
ENDIF
RETURN lcScalHead
*-- End Of lfScalHead.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/1999
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

*!**************************************************************************
*! Name      : lfSRPack
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/11/1999
*! Purpose   : To set the relations or To release the relations when browse
*!			   packing list No. 	
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
*E301331,1 WAB
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
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/11/1999
*! Purpose   : to redisplay the element of option grad again after change the select
*! 			   the select by from (invoice -> pack list )or(Pack list -> invoice)
*!*************************************************************
*! Passed Parameters :
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfVSelcBy()
*!**************************************************************************
*E301331,1 WAB
*!**************************************************************************
FUNCTION lfVSelcBy
llClearInv = (lcRpSelcBy # 'I')		&& to clear lcRpExp 
llClearPak = (lcRpSelcBy # 'P')     && to clear lcRpExp 
*C200094,1 WAB - case from picktkt
*C200094,1 WAB - START
llClearPik = (lcRpSelcBy # 'T')     && to clear lcRpExp 
*C200094,1 WAB - end
CLEAR READ							&& to redisplay (reread)

*!**************************************************************************
*! Name      : lfSRPack
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/11/1999
*! Purpose   : To set the relations or To release the relations when browse
*!			   packing list No. 	
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
*C200094,1 WAB
*!**************************************************************************
FUNCTION lfSRPick
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PikTkt
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  SET RELATION TO Piktkt.Order INTO ORDLINE ADDITIVE
  GO TOP
ELSE
  SELECT PikTkt
  SET RELATION TO
ENDIF


*!**************************************************************************
*! Name      : lfGetLines
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/14/1999
*! Purpose   : to collect the detaile line from pack_line Into Temp File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetLines()
*!**************************************************************************
*B603278,1 WAB
*!**************************************************************************
*B603541,1 Group Cartons only if all cartons information are identical.
*FUNCTION lfGetLines
FUNCTION OLdlfGetLines
*B603541,1 (End)

SELECT Pack_lin
SET ORDER TO PackStyle
*B803161,1 WAM Set order to Temp. Order [Begin]
SET ORDER TO TAG (lcPakLnTmp) IN (lcPakLnTmp)
*B803161,1 WAM Set order to Temp. Order [End  ]

SELECT (lcPackTmp)
GO TOP
*--filter the data to used only record with nRprtTyp = 2 (Select by packing list  )
SCAN FOR nRprtTyp  = 2
  SELECT Pack_lin
  SCAN FOR Pack_no = &lcPackTmp..Pack_no
    SCATTER MEMVAR MEMO
    SELECT (lcPakLnTmp)
    llFound = .F.			&& logic variable to check the record is already exist
    *--check if the same style and the same ordline is already exist in the 
    *--temp file

    *B803161,1 WAM Unique Quantities in Locate expression instead of direct seek only [Begin]
    *IF SEEK(m.PACk_no+m.Style+STR(m.nOrdLineNo,6))  
    =SEEK(m.PACk_no+m.Style+STR(m.nOrdLineNo,6))      
    LOCATE REST WHILE PACK_NO+STYLE+STR(nOrdLineNO,6) = m.PACk_no+m.Style+STR(m.nOrdLineNo,6)  ;
                  FOR Qty1=m.Qty1 AND Qty2=m.Qty2 AND Qty3=m.Qty3 AND Qty4=m.Qty4 AND ;
                      Qty5=m.Qty5 AND Qty6=m.Qty6 AND Qty7=m.Qty7 AND Qty8=m.Qty8 AND Weight = m.Weight
    IF FOUND()
    *B803161,1 WAM Unique Quantities in Locate expression instead of direct seek only [End  ]
    
      SCAN WHILE nOrdLineNo= m.nOrdLineNo 
        *-- check if the carton no is in the sequential number in other 
        *-- way to check if there is no gap between the cartons no by 
        *-- found an record with the 'to_cart' is less than the 'carton no'
        *-- of the current record it is mean that there is no gap between cartons no
        IF m.No_Cart - To_Crt = 1
          *B803065,1 ABD- Calculate the Weight for more than one 
          *B803065,1 ABD- Carton if have more than one carton in the same line. [ Begin ]
          *REPLACE To_Crt  WITH m.No_Cart          ,;
                  Qty1    WITH Qty1    + m.Qty1   ,;
                  Qty2    WITH Qty2    + m.Qty2   ,;
                  Qty3    WITH Qty3    + m.Qty3   ,;
                  Qty4    WITH Qty4    + m.Qty4   ,;
                  Qty5    WITH Qty5    + m.Qty5   ,;
                  Qty6    WITH Qty6    + m.Qty6   ,;
                  Qty7    WITH Qty7    + m.Qty7   ,;
                  Qty8    WITH Qty8    + m.Qty8   ,;
                  TotQty  WITH TotQty  + m.TotQty
                  
          REPLACE To_Crt  WITH m.No_Cart          ,;
                  Qty1    WITH Qty1    + m.Qty1   ,;
                  Qty2    WITH Qty2    + m.Qty2   ,;
                  Qty3    WITH Qty3    + m.Qty3   ,;
                  Qty4    WITH Qty4    + m.Qty4   ,;
                  Qty5    WITH Qty5    + m.Qty5   ,;
                  Qty6    WITH Qty6    + m.Qty6   ,;
                  Qty7    WITH Qty7    + m.Qty7   ,;
                  Qty8    WITH Qty8    + m.Qty8   ,;
                  TotQty  WITH TotQty  + m.TotQty ,;  
                  weight  WITH weight  +m.weight
          *B803065,1 ABD-[ End ] 
          llFound = .T.
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    IF !llFOUND				&& the record is not exist ( new record )
      m.From_Crt  = m.No_Cart			
      m.To_Crt    = m.No_Cart      
      INSERT INTO (lcPakLnTmp) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDSCAN

*!**************************************************************************
*! Name      : lfGrdTot
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/30/2000
*! Purpose   : Calculate Grand Total for Weight
*!**************************************************************************
*!B803161,1 MAB
*!**************************************************************************
*!
FUNCTION lfGrdTot
lnGrdTotWg = LNGRDWHGT
RETURN ""

*!**************************************************************************
*! Name      : lfGetLines
*! Developer : WAM - Wael Ali Mohamed
*! Date      : 04/02/2000
*! Purpose   : to collect the detaile line from pack_line Into Temp File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetLines()
*!**************************************************************************
*B603541,1 Group Cartons only if all cartons information are identical.
*!**************************************************************************
FUNCTION lfGetLines

SET ORDER TO PackStyle IN Pack_lin
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)
SELECT (lcPackTmp)
*--filter the data to used only record with nRprtTyp = 2 (Select by packing list  )
SCAN FOR nRprtTyp  = 2
  SELECT Pack_lin
  lnStyles = 0
  =SEEK(&lcPackTmp..Pack_no)
  DO WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no
    lnCartonNO = no_cart
    llSameCarton = .F.
    *:E301412,1 SHA(Begin)Changed in order to print a separate line for
    *:E301412,1 each carton if the selected form is form B
    *IF lfSameCartn(&lcPackTmp..Pack_no,lnCartonNO,lnStyles)
    IF lcRpForm <> "ALPKLSB" .AND. lfSameCartn(&lcPackTmp..Pack_no,lnCartonNO,lnStyles)
      llSameCarton  = .T.
    ELSE  
      lnStyles = 0
    ENDIF
    *:E301412,1 SHA(End)
    SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no+STR(lnCartonNO,4)
      SCATTER MEMVAR MEMO
      IF llSameCarton
        SELECT (lcPakLnTmp)
        =SEEK(m.Pack_No+STR(lnCartonNO-1,4)+m.style)
        REPLACE To_Crt  WITH lnCartonNO  ,;
                No_Cart WITH lnCartonNO ,;
                Qty1    WITH Qty1 + m.Qty1 ,;
                Qty2    WITH Qty2 + m.Qty2 ,;
                Qty3    WITH Qty3 + m.Qty3 ,;
                Qty4    WITH Qty4 + m.Qty4 ,;
                Qty5    WITH Qty5 + m.Qty5 ,;
                Qty6    WITH Qty6 + m.Qty6 ,;
                Qty7    WITH Qty7 + m.Qty7 ,;
                Qty8    WITH Qty8 + m.Qty8 ,;
                TotQty  WITH TotQty + m.TotQty ,;
                Weight  WITH Weight + m.Weight
      ELSE  
        STORE m.No_Cart TO m.From_Crt,To_Crt,lnFromCrtn
        INSERT INTO (lcPakLnTmp) FROM MEMVAR
        lnStyles = lnStyles + 1
      ENDIF
    ENDSCAN
  ENDDO
ENDSCAN

*!**************************************************************************
*! Name      : lfSameCartn
*! Developer : WAM - Wael Ali Mohamed
*! Date      : 04/02/2000
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
*B603541,1 Group Cartons only if all cartons information are identical.
*!**************************************************************************
FUNCTION lfSameCartn
PARAMETERS lcPackNo,lnCartnNo,lnSameLines
PRIVATE llSameCrtn,lnCrtnLines

llSameCrtn  = .T.
lnCrtnLines = 0
SELECT Pack_lin
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
=SEEK(lcPackNo+STR(lnCartnNo,4))
RETURN(llSameCrtn)
*!**************************************************************************
*! Name      : lfChkModls (B803183)
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/02/2000
*! Purpose   : Cheak if moduls AL & AS instaled in the company.
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfChkModls()
*!**************************************************************************
*B803183,1 
FUNCTION lfChkModls
llModInst = .F.
llModInst = ('AL' $ gcCmpModules  .OR. 'AS' $ gcCmpModules)
*-- llogtrmnat:- Variable Terminate The Option Grid.
IF !llModInst 
  *B803183,1 Message No.    : 42083
  *B803183,1 Message Text   : XX module is not installed. Cannot proceed.    
  *B803183,1 Button No.     :  00000   
  *B803183,1 Message Button : OK
  =gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
  llOgTrmnat = .T.
  CLEAR READ
  RETURN
ENDIF
*-- End Of lfChkModls.

*!**************************************************************************
*! Name      : lfGetPack
*! Developer : Sherif Ishak
*! Date      : 05/15/2000
*! Purpose   : Retrieve Pack ID
*!**************************************************************************
*! Passed Parameters  : lcOrder,lcLineNo
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfGetPack()
*!**************************************************************************
*:E301412,1 SHA
*!**************************************************************************

FUNCTION lfGetPack
PARAMETERS lcOrder,lcStore,lcStyle

PRIVATE lcAlias,lcCurTag

STORE "" TO lcPackId

lcAlias = ALIAS()

SELECT ORDLINE
lcCurTag = TAG()

SET ORDER TO TAG ORDLINST
IF SEEK("O"+ lcOrder + lcStore +lcStyle,"ORDLINE") AND !EMPTY(Pack_Id)
  lcPackId = Pack_Id
ENDIF

SET ORDER TO &lcCurTag
SELECT &lcAlias




*!**************************************************************************
