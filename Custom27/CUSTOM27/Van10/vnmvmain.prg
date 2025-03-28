*:****************************************************************************************
*: Program desc. : Main Program for EDI module for VAN10
*: Developer     : TMI - Tarek mohamed Ibrahim
*: Module        : EB2
*: Application   : Aria27 - EDI (VFP)
*: Date			    : 11/27/2003
*:****************************************************************************************
*C102834,4  : lfDEFSPINS
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfDEFSPINS
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/27/2003
*! Purpose   : Default the field BOL_HDR.MSPC_INST to the value comes from the fields mcomm1,.
*!           ; mcomm2,... , mcomm5 from customer file 
*! ENTRY     :*C102834,4  
*:****************************************************************************************
*! Called from : EBBOL.SCX 
*:****************************************************************************************
FUNCTION lfDEFSPINS
LOCAL MACCNT,mSpc_Inst2

MACCNT = PADR(ALLT(oObject.Parent.Parent.KBACCOUNT.Keytextbox.value),5)
MSTOR  = PADR(ALLT(oObject.Parent.Page1.kbShipTo.Keytextbox.value),8)
IF SEEK(IIF(EMPTY(MSTOR),'M','S')+MACCNT+MSTOR,'CUSTOMER','CUSTOMER')
  mSpc_Inst2 = IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM1)), '' ,ALLTRIM(CUSTOMER.CCOMM1) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM2)), '' ,ALLTRIM(CUSTOMER.CCOMM2) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM3)), '' ,ALLTRIM(CUSTOMER.CCOMM3) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM4)), '' ,ALLTRIM(CUSTOMER.CCOMM4) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM5)), '' ,ALLTRIM(CUSTOMER.CCOMM5))
    REPLACE mspc_inst WITH mSpc_Inst2
    oObject.edtspc_inst.Refresh
ENDIF

*!*************************************************************
*! Name      : lfUCC9NOVNM 
*! Developer : SSE SAMEH TODARY
*! Date      : 06/10/2004
*! Purpose   : For UCC9 Increase the width of PackNo to be 6 digits instead of 5 digits
*!             and decrease the Carton No to be 3 digits instead of 4 digits 
*! ENTRY     :*C123205,1
*!*************************************************************
*! Calls From     : ClassLib EDI for class SendUccLabels metod Do()
*!                        ClassLib EDISH for class Send856 metod Do()   
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lIsCorrect =lfvldDate_To(ldDate_To,laDate_From)
*!*************************************************************
FUNCTION lfUCC9NOVNM
lcUCC9 = RIGHT(PADL(ALLTRIM(MPACKNO),6,'0'),6)+PADL(MBOX_SER,3,'0')
