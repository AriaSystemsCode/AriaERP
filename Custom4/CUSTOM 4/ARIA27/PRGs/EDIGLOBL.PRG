***************************************************************
*Modifications:
*B605568,1 HIA 2/15/2002 Adjust the lfread function to read in
*B605568,1 HIA           segment other than the picked one.
*N127567,1 AAH           Read the subfield at segment when EDI type is EDI-FACT.
*E040124,1 HIA Enhance The Used Method To Write Carton Sequence From Fixed Method At The Code;
*              To Method Depend On 'UCC # Structure' Setup Which Added To Al By E040123 By HBG.
*E302435,1 WLD Increase the  MEMOWIDTH to be 85  08/29/2007
***************************************************************

***************************************************************
**
** FUNCTION TO CHANGE THE DATE FROM YYMMDD TO MM/DD/YY FORMAT
**                                  YYMMDD TO DD/MM/YY FORMAT
**
****************************************************************
FUNCTION EDATE
  PARAMETER MDATE

  IF ALLTRIM(oAriaApplication.DefaultCountry) = 'CANADA' OR ;
      ALLTRIM(oAriaApplication.DefaultCountry) = 'USA'
    RETURN(CTOD(SUBSTR(MDATE,3,2)+'/'+SUBSTR(MDATE,5,2)+'/'+;
      IIF(VAL(SUBSTR(MDATE,1,2))<50,'20','19')+SUBSTR(MDATE,1,2)))
  ELSE
    RETURN(CTOD(SUBSTR(MDATE,5,2)+'/'+SUBSTR(MDATE,3,2)+'/'+PADL(YEAR(DATE()),2)+SUBSTR(MDATE,1,2)))
  ENDIF

  ***************************************************************
  **
  ** FUNCTION TO CHANGE THE DATE FROM YYYYMMDD TO MM/DD/YYYY FORMAT
  **                                  YYYYMMDD TO DD/MM/YYYY FORMAT
  *C101100,1 Add new function to Read Date with century
  ****************************************************************
FUNCTION CENDATE
  PARAMETER MDATE

  IF ALLTRIM(oAriaApplication.DefaultCountry) = 'CANADA' OR ;
      ALLTRIM(oAriaApplication.DefaultCountry) = 'USA'
    RETURN(CTOD(SUBSTR(MDATE,5,2)+'/'+SUBSTR(MDATE,7,2)+'/'+SUBSTR(MDATE,1,4)))
  ELSE
    RETURN(CTOD(SUBSTR(MDATE,7,2)+'/'+SUBSTR(MDATE,5,2)+'/'+SUBSTR(MDATE,1,4)))
  ENDIF

  *!*************************************************************************
  *! Proc : lfSkuDesc
  *! Auth : Wael Aly Mohamed
  *! Date : 01/13/98
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  *! Synopsis : Restore PO SKU information
  *!C101190,1 WAM 06/07/98 Initialize SKU information.
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION lfSkuDesc
  PARAMETERS lcOrder,lcLineNo,lcSKU,lcType
  PRIVATE lcNotes,lcLine
  *N129597,1 WLD increase the  MEMOWIDTH to be 65 (not use the default 50) [Begin]
  *E302435,1 WLD Increase the  MEMOWIDTH to be 85  08/29/2007 [Begin]
  *Set MEMOWIDTH To 65
  Set MEMOWIDTH To 85
  *E302435,1 WLD Increase the  MEMOWIDTH to be 85  08/29/2007 [End]
  *N129597,1 WLD increase the  MEMOWIDTH to be 65 (not use the default 50) [End]
     
  =SEEK('O'+lcOrder+STR(lcLineNo,6),'ORDLINE','ORDLINE')
  lcNotes = ORDLINE.NOTE_MEM
  lcReturn = ''
  IF ATCLINE(ALLTRIM(lcSKU),lcNotes) > 0
    lcLine=MLINE(lcNotes,ATCLINE(ALLTRIM(lcSKU),lcNotes))
    IF AT(lcType+'|',lcLine) > 0
      lcLine=SUBSTR(lcLine,AT(lcType+'|',lcLine)+LEN(lcType)+1)
      lcReturn = SUBSTR(lcLine,1,AT('|',lcLine)-1)
    ENDIF
  ENDIF
  RETURN(lcReturn)

  ************************************************
  **
  ** FUNCTION GETCENT Return Date Century
  **
  *C101100,1 WAM 03/08/98 Add new function to get date century
  ************************************************
FUNCTION GETCENT
  PARAMETERS MDATE

  RETURN LEFT(STR(YEAR(MDATE),4),2)

  ************************************************
  **
  ** FUNCTION XDATE GIVE THE DATE IN YYMMDD FORMAT
  **
  *C101100,1 WAM 03/09/98 Use Date functions in EDI100
  ************************************************
FUNCTION XDATE
  PARAMETERS MDATE

  RETURN(RIGHT(STR(YEAR(MDATE),4),2)+PADL(MONTH(MDATE),2,'0')+PADL(DAY(MDATE),2,'0'))

  ************************************************
  **
  ** FUNCTION CENTDATE GIVE THE DATE IN CCYYMMDD FORMAT
  **
  ************************************************
FUNCTION CENTDATE
  PARAMETERS MDATE
  RETURN(STR(YEAR(MDATE),4)+PADL(MONTH(MDATE),2,'0')+PADL(DAY(MDATE),2,'0'))

  **********************************************
  **
  ** FUNCTION XTIME GIVE THE TIME IN HHMM FORMAT
  **
  **********************************************
FUNCTION XTIME
  RETURN(SUBSTR(TIME(),1,2)+SUBSTR(TIME(),4,2))

  *!*************************************************************************
  *! Proc : lfPOLine
  *! Auth : Wael Aly Mohamed
  *! Date : 01/13/98
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  *! Synopsis : Restore PO line information
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION lfPOLine
  PARAMETERS lcType
  PRIVATE lcLine

  lcLine=SUBSTR(ORDLINE.Desc1,AT(lcType+'|',ORDLINE.Desc1)+3)
  RETURN(SUBSTR(lcLine,1,AT('|',lcLine)-1))

  *!*************************************************************************
  *! Proc : lfInvoice
  *! Auth : Ahmed Mohamed Ibrahim
  *! Date : 10/31/2000
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  *! Synopsis : Get invoice number
  *! Ref      : B803788
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

FUNCTION lfInvoice
  PARAMETERS lcAccount,lcOrderNo,lcStoreNo

  lnAlias=SELECT()
  SELECT INVHDR
  SET ORDER TO TAG INVHDRA DESC
  =SEEK(lcAccount)
  LOCATE REST WHILE ACCOUNT+INVOICE = lcAccount ;
    FOR  ORDER=lcOrderNo .AND. ;
    IIF(MCONSOL='Y',SEEK(INVOICE+lcStoreNo,'CONSINVH'),STORE=lcStoreNo)
  SELECT (lnAlias)
  RETURN(INVHDR.INVOICE)

  *!*************************************************************************
  *! Proc : GetChkDgt
  *! Auth : Wael Ali Mohamed
  *! Date : 08/06/2001
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  *! Synopsis : Calculate Check digit
  *! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION GetChkDgt
  LPARAMETER lcUccNo
  PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount,lnLength
  STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit

  lnLength = LEN(lcUccNo)
  FOR lnCount = 1 TO INT(lnLength/2)
    lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
    lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
  ENDFOR
  IF MOD(lnLength,2) = 1
    lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,lnLength,1))
    lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
  ELSE
    lnChkDigit = MOD(lnSumOdd+lnSumEven*3,10)
  ENDIF
  RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

  *!**************************************************************************
  *! Name      : lfRead
  *! Developer : Hassan Ali
  *! Date      : 06/08/2001
  *! Purpose   : Read EDI file seg. or Specified Field value From "edisv" Table
  *!             AND RETURN THE FIELD VALUE OR THE SEG VALUES ALL
  *!**************************************************************************
  *! Call From : The Mapping tables
  *!**************************************************************************
  *! Param(s)  :  nFld_no    :The field number(N) or (.F.) for Whole SEG.
  *!              cKey_Value :The key value to detect the value
  *!     		   	LEVEL      :THE lcPROCESSLEVEL
  *!     	       cTran_type :THE TRANSACTION (WE NEED TO GET FROM ) TYPE
  *!**************************************************************************
  *! Returns   : The field value (OR THE FIRST FIELD VALUE IN CASE OF ALL SEG)
  *!**************************************************************************
  *! Example   : = lfRead(nFld_no,cKey_Value,PROCESSLEVEL,cTran_type)
  *!**************************************************************************
  *:->
FUNCTION lfREAD
  *B605568,1 HIA 2/15/2002 Read in segment other than the picked one [Begin].
  *!*	LPARAM nFld_no     ,;
  *!*	       cKey_Value     ,;
  *!*	       lcPROCESSLEVEL ,;
  *!*	       cTran_type
  LPARAMETERS nFld_No        ,;
    cKey_Value     ,;
    lcProcessLevel ,;
    cTran_Type     ,;
    lcSegId

  lcSegId = IIF(TYPE('lcSegId')="C",lcSegId,mSegId)
  *B605568,1 HIA 2/15/2002 Read in segment other than the picked one [End].
  *!**********Save Work Area**************************************************
  LOCAL lnArea ,;
    lnInc  ,;
    cRet

  cRet   = ""
  lnArea = SELECT()
  *!*	*!**********Main***********************************************************
  cField_Sep = IIF(EOF('EdiPh'),'',EVAL(ALLTRIM(EdiPh.cFieldSep)))
  cLine_Sep  = IIF(EOF('EdiPh'),'',EVAL(ALLTRIM(EdiPh.cLineSep)))
  SELECT EdiSv
  DO CASE
      *!*	***************CASE Type(nFld_no) = "L"************************************
    CASE TYPE('nFld_No') = "L"
      IF SEEK( EdiAcPrt.cPartCode           +;
          EdiPd.cVersion               +;
          cTran_Type                   +;
          lcSegId                      +;
          PADR(lcProcessLevel, 50, " ")+;
          PADR(cKey_Value    , 60, " "))

        cRet  = ALLT(cValue)

        *!*    		For lnInc = 1 to mFl_Cnt - 1
        *!*    			xField(lnInc)  = SubStr( cRet                               ,;
        *!*    			                         At(cField_Sep,cRet,lnInc     ) +1  ,;
        *!*    			                         At(cField_Sep,cRet,lnInc + 1 ) - At(cField_Sep,cRet,lnInc) -1)

        *!*    		EndFor
        *!*    		xField(lnInc)  = SubStr( cRet ,At(cFIELD_SEP,cRet,lnINc    ) +1)
        *!*    		cRet           = xField(1)
      ELSE
      ENDIF
      ****************CASE Type(nFld_no) = "N"************************************
    CASE TYPE('nFld_No') = "N"

      *N128528, AHH HAS PROBLEM WITH SGEID OF TWO CHAR LENGTH,06/23/2005	[BEGIN]
      *!*		If Seek( EdiAcPrt.cPartCode            +;
      *!*		         EdiPd.cVersion                +;
      *!*		         cTran_Type                    +;
      *!*		         lcSegId			           +;
      *!*		         Padr(lcProcessLevel, 50, " ") +;
      *!*		         Padr(cKey_Value    , 60, " ") )
      *!*	        *B119185,1 WAM Get Fields Separator
      *!*			cField_Sep = SUBSTR(cValue,LEN(lcSegId)+1,1)
      *!*	        *B119185,1 WAM (End)

      IF SEEK( EdiAcPrt.cPartCode            +;
          EdiPd.cVersion                +;
          cTran_Type                    +;
          PADR(lcSegId,3,SPACE(1))            +;
          PADR(lcProcessLevel, 50, " ") +;
          PADR(cKey_Value    , 60, " ") )
        cField_Sep = SUBSTR(cValue,LEN(ALLTRIM(lcSegId))+1,1)

        *N128528, AHH HAS PROBLEM WITH SGEID OF TWO CHAR LENGTH,06/23/2005	[END]
        cRet  = ALLT(cValue)
        nOcur = OCCUR(cField_Sep, cRet)
        nLOC1 = AT(cField_Sep, cRet, nFld_No  )
        nLOC2 = AT(cField_Sep, cRet, nFld_No+1)
        DO CASE
          CASE nLOC1 <> 0 AND nLOC2 <> 0
            cRet = SUBSTR(cRet, nLOC1 + 1, nLOC2 - nLOC1 -1 )
          CASE nLOC1 <> 0 AND nLOC2 = 0
            cRet = SUBSTR(cRet, nLOC1+1)
            *B119185,1 WAM Get Fields Separator
          OTHERWISE
            cRet = ''
            *B119185,1 WAM (End)
        ENDCASE
      ELSE
      ENDIF
  ENDCASE
  **********Restore Work Area*************************************************
  SELECT (lnArea)
  RETURN cRet
  *!**************************************************************************
ENDFUN
*!****************END PROCEDURE*********************************************
*!*	***************************************************************************
*E039066 AAH  save edi raw files into aria data base 02/28/2005	[Begin]
*!*	This function will retrive data of stored raw files from aria data base
*!*	aria read edi rawfile and store it into two files one for header segment and
*!*	second for detail loop segement
*!*	IF we need header segment data we pass line no parameter with 0 ELSE if we
*!*	need detail segment we pass line no and size or pack which we need
*!*	***************************************************************************
FUNCTION ReadFld
  PARAMETERS ;
    lcAccount,;
    lcOrder,;
    lnOrdline,;
    lcSeg_id,;
    lnFieldno,;
    lcQualifier,;
    lcsz_pack

  LOCAL lcRetAlias

  lcFldValue = ""
  lcRetAlias = ALIAS()

  IF lnOrdline = 0	&&Read HEADER segment
    IF !USED("EDISOHD")
      SELECT 0
      USE EDISOHD
      SET ORDER TO ACC_ORD
    ELSE
      SELECT EDISOHD
    ENDIF
    IF SEEK(lcAccount+lcOrder)
      lcFieldSep=SUBSTR(Edi_lines,LEN(ALLTRIM(Seg_id))+1,1)
      lcQualifier=lcFieldSep+lcQualifier+lcFieldSep
      SCAN WHILE ACCOUNT+ORDER = lcAccount+lcOrder
        IF Seg_id = lcSeg_id  AND  lcQualifier$Edi_lines
          *------------------------
          mFielsNo = OCCURS(lcFieldSep,Edi_lines)
          *-- Put the data elements into array
          IF mFielsNo > 0
            FOR X = 1 TO  mFielsNo
              Strt_Pos = AT(lcFieldSep,Edi_lines , X) + 1
              End_Pos  = IIF(X < mFielsNo , AT(lcFieldSep , Edi_lines , X + 1),LEN(Edi_lines) + 1)
              IF End_Pos => Strt_Pos AND X = lnFieldno
                lcFldValue = SUBSTR(Edi_lines  , Strt_Pos , End_Pos - Strt_Pos)
                lcFldValue = TRIM(lcFldValue)
              ENDIF
            ENDFOR
          ENDIF
          *------------------------
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  ELSE 				&&Read DETAIL Segment
    IF !USED("EDISODT")
      SELECT 0
      USE EDISODT
      SET ORDER TO ACC_ORD
    ELSE
      SELECT EDISODT
    ENDIF

    IF SEEK(lcAccount+lcOrder+STR(lnOrdline,6))
      lcFieldSep=SUBSTR(Edi_lines,LEN(ALLTRIM(Seg_id))+1,1)
      lcQualifier=lcFieldSep+lcQualifier+lcFieldSep
      SCAN WHILE ACCOUNT+ORDER+STR(line_no,6) = lcAccount+lcOrder+STR(lnOrdline,6)
        IF Seg_id = lcSeg_id  AND  lcQualifier$Edi_lines AND ALLTRIM(csz_pack)==ALLTRIM(lcsz_pack)
          *------------------------
          mFielsNo = OCCURS(lcFieldSep,Edi_lines)
          *-- Put the data elements into array
          IF mFielsNo > 0
            FOR X = 1 TO  mFielsNo
              Strt_Pos = AT(lcFieldSep,Edi_lines , X) + 1
              End_Pos  = IIF(X < mFielsNo , AT(lcFieldSep , Edi_lines , X + 1),LEN(Edi_lines) + 1)
              IF End_Pos => Strt_Pos AND X = lnFieldno
                lcFldValue = SUBSTR(Edi_lines  , Strt_Pos , End_Pos - Strt_Pos)
                lcFldValue = TRIM(lcFldValue)
              ENDIF
            ENDFOR
          ENDIF
          *------------------------
          EXIT
        ENDIF
      ENDSCAN
    ENDIF

  ENDIF
  RETURN lcFldValue
*!****************END PROCEDURE************************************
*******************************************************************
**
** FUNCTION lfCOMPFLD used to get subfiled from segment at EDI-FACT
**
**
******************************************************************
*N127567,1 AAH           Read the subfield at segment when EDI type is EDI-FACT. [BEGIN]  
FUNCTION lfCOMPFLD
  LPARAMETERS lcData,lcSeprator ,lnFieldno
  IF EMPTY(lcSeprator)
    lcSeprator =":"
  ENDIF

  DO CASE
      *handel case of no subfield and need to return fld #1
    CASE OCCURS(lcSeprator,lcData) = 0 AND lnFieldno = 1
      RETURN(ALLTRIM(lcData))
      *handel case of no subfield
    CASE OCCURS(lcSeprator,lcData) = 0
      RETURN('')
      *handel case of requested subfld not exist at the segment
    CASE OCCURS(lcSeprator,lcData) +1 < lnFieldno
      RETURN('')

  ENDCASE

  *!*	IF OCCURS(lcSeprator,lcData) +1 < lnFieldNo
  *!*	  *=MESSAGEBOX("Invalid Field # ")
  *!*	  RETURN('')
  *!*	ENDIF

  DO CASE
    CASE lnFieldno  = 1
      lnStartpos 	= 1
      lnStrlen	 	= AT(lcSeprator,lcData,lnFieldno)-1
    CASE lnFieldno  = OCCURS(lcSeprator,lcData) +1
      lnStartpos 	= AT(lcSeprator,lcData,lnFieldno-1)+1
      lnStrlen	 	= LEN(lcData)- lnStartpos +1
    OTHER
      lnStartpos 	= AT(lcSeprator,lcData,lnFieldno-1)+1
      lnStrlen	 	= AT(lcSeprator,lcData,lnFieldno)-AT(lcSeprator,lcData,lnFieldno-1)-1

  ENDCASE
RETURN(SUBSTR(lcData,lnStartpos,lnStrlen))
*N127567,1 AAH           Read the subfield at segment when EDI type is EDI-FACT. [END] 
*:****************************************************************************************
*! Name      : GetUccNO
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 03/19/2006
*! Purpose   : Eet the UCC9 # from the new file EDICRTSQ
*:****************************************************************************************
*! Called from : EDI.SENDUCCLABELS CLASS, EDISH.SEND856 CLASS 
*! Called Like : lfGetUccNO (lnCRTSEQ_SETUP, lcPACKNO, lcBOX_SER)
*:****************************************************************************************
*E040124,1 HIA Enhance The Used Method To Write Carton Sequence From Fixed Method At The Code;
*              To Method Depend On 'UCC # Structure' Setup Which Added To Al By E040123 By HBG [BEGIN]
Function lfGetUccNO

Lparameters lnCRTSEQ_SETUP,; && THE 'UCC # Structure ' SETUP FROM AL MODULE
            lcPACKNO,;       && Packing List #
            lcBOX_SER        && Carton # Within The Packing List    
              
Local oGetMemVar_SETUP,;
      lcUCC9

  * Verified The 'UCC # Structure ' Setup From AL Module Passed Or Not [START]
  If Type('lnCRTSEQ_SETUP') <> 'C'

    lnCRTSEQ_SETUP   = '0'
    oGetMemVar_SETUP = Createobject('GetMemVar')
    lnCRTSEQ_SETUP   = Alltrim(oGetMemVar_SETUP.Do('M_UCCBSDON',oAriaApplication.ActiveCompanyId))
    Release oGetMemVar_SETUP
    If lnCRTSEQ_SETUP   = '0'
      *=Messagebox('UCC # Structure Setup [AL Module Setup] Has Not Accepted Value [Accept 5,6,9], Carton Sequence Can Not Be Generated !!',0+16,_Screen.Caption)
      = Messagebox('Please setup the UCC # structure code from the Allocation Module setup screen in order to using the Bill Of Lading screen.',0+16,_Screen.Caption)
    Endif
  ENDIF
  * Verified The 'UCC # Structure ' Setup From AL Module Passed Or Not [END]

   Do Case
    Case lnCRTSEQ_SETUP = '5'
      * UCC9 Format: Right 5 Digits of Packing list + 4 Digits From Carton Sequence Pad Left With '0' 
      lcUCC9 = Right(Padl(Alltrim(lcPACKNO),6,'0'),5)+Padl(lcBOX_SER,4,'0')

    Case lnCRTSEQ_SETUP = '6'
      * UCC9 Format: Right 6 Digits of Packing list + 3 Digits From Carton Sequence Pad Left With '0' 
      lcUCC9 = Right(Padl(Alltrim(lcPACKNO),6,'0'),6)+Padl(lcBOX_SER,3,'0')

    Case lnCRTSEQ_SETUP = '9'
      * UCC9 Format: No format, It Is Picked From EDICRTSQ.UCC9 Which Filled By A27 Packing Screens;
      *              Which Catched From EDIACPRT.UCC9 Sequence In Case Of The Account Is EDI Trading Partner;
      *              OTHERWISE Catched From Sequence At Allocation Module For EDI Carton Sequence.
                
      lcAlias = Alias()
      If !Used('EDICRTSQ')
        Use (oAriaApplication.DataDir+'EDICRTSQ') Order Tag PCKCRTSQ In 0
      Endif
      If Seek(lcPACKNO+Str(lcBOX_SER,6),'EDICRTSQ')
        Select EDICRTSQ
        lcUCC9 = PADL(ALLTRIM(EDICRTSQ.UCC9),9,'0')
      Endif
      Select (lcAlias)
      
    Otherwise
      lcUCC9 = ''
      *=MESSAGEBOX('UCC # Structure Setup [AL Module Setup] Has Not Accepted Value [Accept 5,6,9], Carton Sequence Can Not Be Generated !!',0+16,_SCREEN.CAPTION)
      = Messagebox('Please setup the UCC # structure code from the Allocation Module setup screen in order to using the Bill Of Lading screen.',0+16,_Screen.Caption)
  Endcase
  Return(lcUCC9)
Endfunc
*:****************************************************************************************