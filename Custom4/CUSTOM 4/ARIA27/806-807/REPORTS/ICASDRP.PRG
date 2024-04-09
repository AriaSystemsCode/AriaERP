******************************************************************
* PROG : ROB4250.PRG FOR (Robyn Meredith)
* DATE : 08/23/98
* DESC : Print Assebler's declaration forms.
* AUTH : Adel Mohammed El Gazzar (ADEL)
* Refer to (101260)
****************************************************************************


*----Initilize variables
*--lcFrstSty       && Holds the first style in the range.
*--lcSndSty        && Holds the second style in the range.
*--lcFrstInv       && Holds the first invoice in the range.
*--lcSndInv        && Holds the second invoice in the range.
*--ldFrstDat       && Holds the first date in the range.
*--ldSndDat        && Holds the second date in the range.
*--llInvFltr       && Flag to see if the user left the Invoice range empty.
*--R_WIDTH         && Variable needed for printing
*--lcFrxStr1,2,3  && Variables to hold the FRX's strings.

STORE SPACE(12) TO lcFrstSty,lcSndSty
STORE SPACE(15) TO lcFrstInv,lcSndInv
STORE DATE() TO ldFrstDat,ldSndDat
STORE .F. TO llInvFltr
R_WIDTH = 'N'
lcFrxStr1 =  '         I,Robyn Meredith , Inc. Declare that to the best of my knowledge and belief the'+CHR(13)+;
             '                                 were assembled in whole or part of fabricated components'+CHR(13)+;
             'listed and described below which are products of the United States.'
lcFrxStr2 = 'In accordance with Headnote 3,part 1B,Schedule 8 Tariff Schedule of the United States (19)'+CHR(13)+;
             'U.S.C. (1202).'+CHR(13)+;
             'Description of the operations performed abroad on the exported components (insufficient)'+CHR(13)+;
             'detail to enable Customs officers to determine whether the operations performed are within the'+CHR(13)+;
             'purview of Item 807.00 Tariff Schedule of the United States (19  U.S.C.  1202) (attached '+CHR(13)+;
             'supplemental sheet if more space required)'+CHR(13)
lcFrxStr3 = CHR(13)+;
             'I declare to the best of my knowledge and belief the (above) attached declaration and any'+CHR(13)+;
             'other information submitted here with ,or otherwise supplied or reffered to,is correct in every'+CHR(13)+;
             'respect and they has been compliance with all partinent headnotes of the Tarrif Schedule of the'+CHR(13)+;
             'United States (10  U.S.C.  1202)'+CHR(13)

           
SELECT Style
*--Set a relation between the STYLE (parent) file and ASSMDECH (child) file
*--to browse styles having assembler's declaration formse (!EOF(ASSMDECH))
SET RELATION TO cStyMajor INTO ASSMDECH

*--The assembler's declarations temp. file.
lcDecTemp= gfTempName()
SELECT ASSMDECH
*---Copy structure to temp file.
COPY STRUCTURE TO &QWD.&lcDecTemp

*--Change the file index to be on  cinvoice+style
SET ORDER TO ASSMDEC1 
*--If no ranges were entered
*laOGFxFlt

IF !EMPTY(lcRpExp)
  *--Seek the first invoice if any.
  *IF !EMPTY(lcFrstInv)
  *  =SEEK(lcFrstInv)
  *ELSE
    GO TOP
  *ENDIF  
  *--Copy the corresponding records to the filter expression.
  lcRpExp = STRTRAN(lcRpExp,'STYLE.','')
  COPY REST TO &gcWorkDir.&lcDecTemp FOR &lcRpExp 
  *--Open the temp file.
  =gfOpenFile('&gcWorkDir.&lcDecTemp','','EX')
  INDEX ON cInvoice+Style TAG (lcDecTemp)
  IF RECCOUNT() = 0
    =gfModalGen('QRM42188B42001','DIALOG','records selected')
    RETURN
  ENDIF
  SET RELATION TO STYLE+CINVOICE INTO ASSMDECL
ELSE
  SET ORDER TO ASSMDEC1
  SET RELATION TO STYLE+CINVOICE INTO ASSMDECL  
ENDIF
SELECT &lcDecTemp
SET SKIP TO ASSMDECL
GO TOP

DO gfDispRe WITH EVALUATE('lcFormName') 

 

****  
FUNCTION lfVStyle
  *--See whether the entered style(if any) exists in ASSMDECH file or not.If not show the browse.
  SELECT ASSMDECH
  *--Set order to style+Cinvoice
  SET ORDER TO ASSMDECH
  SELECT Style
  SET RELATION TO cStyMajor INTO ASSMDECH
  lcStyle = VARREAD()
  
  IF !EMPTY(&lcStyle) AND !SEEK(&lcStyle,'ASSMDECH')) 
    &lcStyle = lfBrowStyl('STYLE')
    *--If ESC was pressed
    IF EMPTY(lcStyle)
      &lcStyle = ''
    ENDIF  
  ENDIF
  

FUNCTION lfvInvoice

  *-------------------------------------------------------
  * INVOICE RANGE
  *-------------------------------------------------------
  lcInv = VARREAD()
  *--Change the file index to be cInvoice+style
  SELECT ASSMDECH
  SET ORDER TO ASSMDEC1 

  *--See whether the entered invoice(if any) exists in the file or not.If not show the browse.
  IF !EMPTY(lcInv) AND !SEEK(lcInv,'ASSMDECH') 
    &lcInv=lfBrowStyl('ASSMDECH')  
    *--If ESC wae pressed
    IF EMPTY(lcInv)
      &lcInv = ''
    ELSE
      
    ENDIF  
    
  ENDIF



*!*************************************************************
*! Name      : lfBrowStyl
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To browse the styles having declarations of values.
*!*************************************************************
*! PARAMETERS:
*!            lcAlias   : Holds the alias from which we will browse fields.
*!****************************************************************
FUNCTION lfBrowStyl
PARAMETERS lcAlias

PRIVATE lcOldAlias,lcBrowTtl,lcBrowCon,lcBrFields,lcRetVal
lcOldAlias = ALIAS()
*---Vriables needed for ARIABROW()
*--lcBrowCon   && The 'FOR condition' VAR. for the BROWSE statement.
*--lcBrowTtl   && The browse title VAR. .
*--lcBrFields  && The VAR. holds fields nsmes.
IF UPPER(lcAlias)="STYLE"
  lcBrowCon  = "FOR !EOF('ASSMDECH')"
  lcBrowTtl  = "Styles having assembler's declaration forms"
  lcBrFields = "cStyMajor  :H= 'Style'      ,"+;
                "desc      :H= 'Style desc.',"+;
                "Pattern   :H= 'Pattern'    ,"+;
                "cStyGroup :H= 'Group' ,"+;
                "Scale     :H= 'Scale' ,  "+;
                "cDivision :H= 'Division',"+;
                "Season    :H= 'Season'      "
ELSE
  lcBrowCon   = ""
  lcBrowTtl   = "Invoices for styles having assembler's declaration forms"   && Varible to hold the browse title
  lcBrFields  = "Style     :H= 'Style#' , "+;
                "desc      :H= 'Style desc.',"+;    
                "cInvoice  :H= 'Invoice#' , "+;
                "nUnitVal  :H= 'Unit price' , "+;
                "Cartons   :H= 'Cartons' , "+;
                "nTotQty   :H= 'Quantity' , "+;
                "InvDate   :H= 'Invoice date'"
ENDIF
DECLARE laReturn[1] 
STORE '' TO laReturn
lcRetVal = IIF(UPPER(lcAlias)='STYLE','cStyMajor','cInvoice')
SELECT (lcAlias)
*--To speed the browse

IF UPPER(lcAlias) = "STYLE" AND !EOF()
  GO RECNO()
ENDIF 

*=AriaBrow(lcBrowCon, lcBrowTtl, gnBrFSRow1, gnBrFSCol1,;
          gnBrFSRow2, gnBrFSCol2,'','',lcRetVal,'laReturn')
=gfBrows(lcBrowCon,lcRetVal,'laReturn')           
SELECT (lcOldAlias)
*--Return the proper space no. if it is empty.
IF EMPTY(laReturn[1])
  laReturn[1]=IIF(UPPER(lcAlias)="STYLE",SPACE(19),SPACE(15))
ENDIF  
  
RETURN (laReturn[1])

FUNCTION lfwRepWhen
