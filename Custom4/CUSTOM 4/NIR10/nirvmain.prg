*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Rasha (RAS)
*: Date      : 05/22/2011
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C201344,1 RAS 05/22/2011 show UPC price ticket for Packs if the checkbox for that purpose is checked[T20110223.0011]
*C201344,2 Ras 08/22/2011 get Pack Data if it belong to generic Account[T20110223.0011]
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfIsNirv
*! Developer : Rasha (RAS)
*! Date      : 05/22/2011
*! Purpose   : return true if customer is nirvanna
*:****************************************************************************************
*! Called from : screen ebtktprc.print
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : logic
*:****************************************************************************************
FUNCTION lfIsNirv
  return .T.
ENDFUNC

*:****************************************************************************************
*! Name      : lfGetData
*! Developer : Rasha (RAS)
*! Date      : 05/22/2011
*! Purpose   : get data which will be shown on print UPC3 for packID checkbox 
*:****************************************************************************************
*! Called from : screen ebtktprc.print
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : logic
*:****************************************************************************************
FUNCTION lfGetData
 
 IF oFormSet.ariaform1.Ariapageframe1.page1.chk_ppUPC.Value=1
	  lcSizeDesc =  SCALE.SZ&lcCount            && Size description
	  lcUpcCode = ''
	  lcUpcStyle = EVAL(oFormSet.tmpDetails+'.Style')
	  IF llCatDanial
	    lcTmpStyle = ALLTRIM(SUBSTR(lcUpcStyle,1,lnMajorLen))
	    lcTmpStyle = PADR(SUBSTR(lcTmpStyle,1,LEN(lcTmpStyle)-2),lnMajorLen)
	    lcTmpStyle = lcTmpStyle + SUBSTR(lcUpcStyle,lnMajorLen+1)
	    lcUpcStyle = IIF(SEEK(lcTmpStyle,'STYLE'),lcTmpStyle,lcUpcStyle)
	  ENDIF
	  STORE '' TO lcPack_Id , lcSku

    IF SEEK(TYPE + Ticket , oFormSet.tmpCuttkt)
	    lcAccount = EVAL(oFormSet.tmpCuttkt + '.Account')
	    IF EMPTY(lcAccount) AND TYPE = 'C' AND SEEK('1'+Ticket,'CUTPICK') AND SEEK('O'+CUTPICK.ORDER,'ORDHDR')
	      lcAccount = ORDHdr.Account
	    ENDIF
	    lcPack_Id = ""
	    lnpackQuant=0
	    select Spck_lin 
	    *C201344,2 Ras 08/22/2011 get Pack Data if it belong to generic Account[begin]
*!*		    SCAN for type='P' and Account=lcAccount and style= lcUpcStyle and QTY&lcCount>0
	    SCAN for type='P' and (Account=lcAccount OR Account='*****' ) and style= lcUpcStyle and QTY&lcCount>0 
	    *C201344,2 Ras 08/22/2011 get Pack Data if it belong to generic Account[begin]    
	     lcPack_Id = Spck_lin.PACK_ID
	     targetQuant="QTY"+lcCount
	     lnpackQuant= Spck_lin.&targetQuant.
	    ENDSCAN
	  ENDIF
	  select StyleUpc
	  set order to PACKUPC
	  *C201344,2 Ras 08/22/2011 get Pack Data if it belong to generic Account[begin]
*!*		  IF SEEK(lcAccount+lcPack_Id+'  ') and lupcpack 
	  IF (SEEK(lcAccount+lcPack_Id+'  ')OR SEEK('*****'+lcPack_Id+'  ') ) and lupcpack 
	  *C201344,2 Ras 08/22/2011 get Pack Data if it belong to generic Account[begin]
	    lcUpcCode = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
		ENDIF
    SELECT (oFormSet.tmpDetails) 
    
 IF Qty&lcCount <= 0
     lnQuantity=0
 ELSE
     lnQuantity = INT(ROUND((( oFormSet.Ariaform1.Ariapageframe1.Page1.txtPercent.VALUE/100) * Qty&lcCount),0))
     lnQuantity = IIF(lnQuantity = 0, 1, lnQuantity)   && prints at least 1
 ENDIF   
   
 IF EMPTY(lcUpcCode)
  IF EMPTY(lcPack_Id)
    oFormSet.Ariaform1.Ariapageframe1.Page1.EDERROR.VALUE=oFormSet.Ariaform1.Ariapageframe1.Page1.EDERROR.VALUE+'  '+TRIM(STYLE)+'/'+lcSizeDesc+' have no pack id. This size will be ignored.'+CHR(13)+CHR(10)
  ELSE
    oFormSet.Ariaform1.Ariapageframe1.Page1.EDERROR.VALUE=oFormSet.Ariaform1.Ariapageframe1.Page1.EDERROR.VALUE+'U.P.C. # for : '+TRIM(lcPack_Id)+'/'+' not found. This Pack will be ignored.'+CHR(13)+CHR(10)
  ENDIF
  lnQuantity= 0 
 ENDIF     
  lnQuantity= lnQuantity/MAX(lnpackQuant,1)
 return .T.
ELSE
 return .F.
ENDIF

ENDFUNC