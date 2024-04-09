*:****************************************************************
*: Program file  : ICFLATFL.PRG
*: Program desc. : Generate Flat file with UPC info.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Inventory Control (IC)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 06/22/2003
*: Tracking Job Number : C#200576,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  :  lfProceed , lfUpdate  , lfvFpath
*:               :            :  lfOpenFls , lfClosFls . 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200576,1.
*:****************************************************************
*:Modifications  :
*C120735,1  TMI 01/05/2004 Add selection browse for style depends on sent/not sent styles
*:****************************************************************
*:
DIMENSION laOpenFile[4,3]
STORE 0 TO lnFilHandl
llUPInstld = (OCCURS('UP',gcComp_Mdl)<>0)

*- Message Text   :- Universal Product Code is not Installed for company ' +gcAct_Comp+'. You have to install it 
*- Message Text   :- 'through the Company Information program.' 
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.

lcMessage = 'Universal Product Code is not Installed for company ' +gcAct_Comp+'. You have to install it through the Company Information program.' 

IF !llUPInstld .AND. gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage) = 1
  RETURN
ENDIF


IF !gfSetup()
  RETURN
ENDIF

STORE '' TO lcRpfName , lcRpFPath
*C120735,1  TMI [Start] Initialize sent status variable
lcRpSntSt = ' '
*C120735,1  TMI [End  ] 
lcExpr = gfOpGrid('ICFLATFL',.T.)

IF TYPE('lcExpr') = "L" .AND. !lcExpr
  RETURN
ENDIF

*-- Check on the File Path.
DO CASE 
  CASE EMPTY(lcRpFPath)
   = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a file directory, Can not proceed.')
   RETURN
   *-- Check on the File Name.
  CASE EMPTY(lcRpfName)
    = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a file name, Can not proceed.')
    RETURN
ENDCASE
*-- Funtion to collect the data into text file.

*-- Open needed files
= lfOpenFls ()

*C120735,1  TMI [Start] Check if styles selected
llSlctdSty = .F.
*C120735,1  TMI [End  ] 


= lfProceed ()


*-- Close opened files 
= lfClosFls ()

*-- this loop to close the file, the program should close this file under any condation.

DO WHILE !FCLOSE(lnFilHandl)

ENDDO

*C120735,1  TMI [Start] Show successfull message if there is styles sent
IF llSlctdSty
*C120735,1  TMI [End  ] 
  *- Message Text   :- Process complete successfully.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','Process complete successfully.')
  *C120735,1  TMI [Start] If there is no styles selected
ELSE
  = gfModalGen('INM00000B00000','F','ALERT',' ','No Styles selected.')  
ENDIF  
*C120735,1  TMI [End  ] 

RETURN
*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 06/22/2003
*: Purpose   : Funtion to collect the data into text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
*:
FUNCTION lfProceed
PRIVATE lcUptext

lcFile = lcRpFPath+ALLTRIM(lcRpfName)+'.Txt'
lnFilHandl = FCREAT(lcFile)

IF lnFilHandl < 0
  *- Message Text   :- File can not be create. can not proceed.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','File can not be create. can not proceed.')
  RETURN
ENDIF

lcMjrPct   = gfItemMask('PM')
lcNMjrPt   = gfItemMask('PN')
lnstylewid = LEN(lcMjrPct)
lncolorwid = LEN(lcNMjrPt)


SELECT STYLE
*C120735,1  TMI [Start] Check the field "LSENTFLAG"
*SCAN FOR &lcExpr
SCAN FOR &lcExpr .AND. ;
         IIF(lcRpSntSt='N',NOT STYLE.LSENTFLAG,IIF(lcRpSntSt='S',STYLE.LSENTFLAG,.T.)) 
*C120735,1  TMI [End  ] 
  WAIT WINDOW 'Style # :-' + Style NOWAIT
  lcStyleUpc = ''
  llStyleUpc = .F.
  *- Check if the style has a UPC
  IF SEEK(Style,'STYLEUPC')
    = SEEK('S'+Scale,'SCALE')
    SELECT STYLEUPC
    *-- Scan for the style upc.
    SCAN REST WHILE style+size = Style.style
      *-- send the upc Number.
      lcStyleUpc = cUpcnum1 + cUpcnum2 + cUpcnum3
      llStyleUpc = .T.
      SELECT STYLE
      = lfUpdate ()
      SELECT STYLEUPC
    ENDSCAN
  ELSE
    IF SEEK('S'+Scale,'SCALE')
      FOR I = 1 TO Scale.CNT
        = lfUpdate ()
      ENDFOR
    ENDIF
  ENDIF
  *C120735,1  TMI [Start] Update 'LSENTFLAG' field and "llSlctdSty" variable
  SELECT STYLE  
  REPLACE LSENTFLAG WITH .T.
  llSlctdSty = .T.
  *C120735,1  TMI [End  ] 
ENDSCAN

*-- End of lfProceed
*:*************************************************************
*: Name      : lfUpdate 
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 06/22/2003
*: Purpose   : Funtion to update the txt file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdate()
*:*************************************************************
*:
FUNCTION lfUpdate 
PRIVATE lnalias

lnalias = SELECT (0)
lcMajor  = SUBSTR(STYLE.Style,1,lnStyleWid)
lcNonMjr = SUBSTR(STYLE,lnstylewid+2,lncolorwid)

*-- Style UPC.
lcUptext = lcStyleUpc                     +','

*-- Group Code.
lcUptext = lcUptext + ALLTRIM(cStyGroup)  + ','
*-- Short Description.
lcUptext = lcUptext + ALLTRIM(Desc)       + ','
*-- Long Description.
lcUptext = lcUptext +  ALLTRIM(Desc1)      + ','
*-- Style Number.
lcUptext = lcUptext +  ALLTRIM(lcMajor)    + ','
*-- Color Number.
lcUptext = lcUptext +  ALLTRIM(lcNonMjr)   + ','
*-- Size.
lcUptext = lcUptext +  ALLTRIM(Eval('Scale.sz'+ALLTRIM(IIF(llStyleUpc,StyleUpc.Size,STR(I)))))   + ','
*-- Division Code
lcUptext = lcUptext +  ALLTRIM(cDivision)  + ','
*-- Season Code
lcUptext = lcUptext +  ALLTRIM(Season)     + ','
*-- Group Description
lcUptext = lcUptext +  ALLTRIM(gfCodDes(lcNonMjr,'COLOR')) +','
  
*-- Color Description
lcUptext = lcUptext +  ALLTRIM(gfCodDes(cStyGroup,'CSTYGROUP')) +','
  

IF EMPTY(FABRIC) .OR. !SEEK(Fabric,'FABRIC')
  lcUptext = lcUptext + ',,'
ELSE
  *-- Fabric Code.  
  lcUptext = lcUptext + ALLTRIM(Fabric.Fabric) + ','
  *-- Fabric Description.
  lcUptext = lcUptext + ALLTRIM(Fabric.Desc) + ','
ENDIF  

*-- Style Cost
lcUptext = lcUptext + ALLTRIM(STR(TotCost,10,3)) + ','

*-- Retail Price.
lcUptext = lcUptext + ALLTRIM(STR(nsugretpri,12,2))     + ',' 

* Nad Added stle group Desc2 
DECLARE laVRltFld[1,2]
laVRltFld[1,1] = 'CGRPDESC2'
laVRltFld[1,2] = 'lcCarCod'
lcCarCod = ''
=gfRltFld(cStyGroup,@laVRltFld,'CSTYGROUP')
lcUptext = lcUptext + ALLTRIM(lcCarCod)

=FPUTS(lnFilHandl,lcUptext)

SELECT (lnalias)
*-- End of lfUpdate 
*:*************************************************************
*: Name      : lfvFpath
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 06/22/2003
*: Purpose   : Funtion to get the fil path.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFpath()
*:*************************************************************
*:
FUNCTION lfvFpath
PRIVATE lcoldPath

lcoldPath = FULLPATH('')

lcRpFPath = GETDIR()

SET DEFA TO &lcoldPath
*-- End of lfvFpath
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 06/22/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .

laOpenFile[1,1] = 'STYLE'
laOpenFile[1,2] = 'STYLE'

laOpenFile[2,1] = 'SCALE'
laOpenFile[2,2] = 'SCALE'


laOpenFile[3,1] = 'STYLEUPC'
laOpenFile[3,2] = 'STYLEUPC'

laOpenFile[4,1] = 'FABRIC'
laOpenFile[4,2] = 'FABRIC'



FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

*-- End of lfOpenFls.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : Abdou Elgendy
*: Date      : 06/22/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

FOR I = 1 To ALEN(laOpenFile,1)
  IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
    = gfCloseFile(laOpenFile[I,1])
  ENDIF
ENDFOR

*-- End of lfClosFls
*:*************************************************************


*:**************************************************************************
*:* Name        : lfGetSesns
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/05/2004
*:* Purpose     : Get selected seasons
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : Selected seasons
*:***************************************************************************
*:* Example     :  = lfGetSesns()
*:***************************************************************************
*C120735,1  TMI 
FUNCTION lfsrvSty 
PARAMETERS lcParam
PRIVATE lnSesPos,lcRetSesn
IF lcParam = 'S'
  lcRetSesn = ''
  lnSesPos = ASCAN(LAOGFXFLT,"STYLE.SEASON    ")
  IF lnSesPos > 0  
    lcSeasons = LAOGFXFLT[ ASUBSCRIPT(LAOGFXFLT,lnSesPos,1) , 6 ]
  ENDIF
ENDIF
*-- end of lfGetSesns.

