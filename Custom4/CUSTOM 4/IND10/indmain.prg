*!**************************************************************************
*! Name      : INDMAIN.PRG
*! Developer : AWD (AHMED AWAAD)
*! Date      : 06/01/2008
*! Purpose   : IND10 Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
*!C201011,2 MMT 08/25/2008 Fix bug of wrong shipment No.[T20080403.0008]
*:***************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

FUNCTION  lfGETSHPNO


LOCAL oldalias
 oldalias = SELECT(0)
 lcstykey = PADR(loformset.lcstylekey, IIF(loformset.llallcolors, loformset.lnstylewid, IIF(loformset.llallscales, 16, 19)))+'%'
 lcwherecond = " POSLN.trancd=2 and POSLN.Style like '"+lcstykey+"'"
 lcwherecond = "SELECT POSLN.* FROM POSLN (INDEX = POSLN)"+" WHERE "+lcwherecond
 loformset.shipcrs = gftempname()
     loformset.mOpenSQLStatment('POSLN',loformset.shipcrs,;
                              'cbusdocu+cstytype+po+cinvtype+style+lineno+trancd',;
                              'POSLN',;
                              lcWhereCond,;
                              "POSLN")
    

    SELECT (loformset.shipcrs)
    =CURSORSETPROP("Buffering",3,loformset.shipcrs)
    
    *!C201011,2 MMT 08/25/2008 Fix bug of wrong shipment No.[Start]
    *INDEX on po+STR(lineno,6) TAG postyinv
    INDEX on po+STR(lineno,6)+crsession TAG postyinv
    *!C201011,2 MMT 08/25/2008 Fix bug of wrong shipment No.[End]
    
    SET ORDER TO  postyinv
    SELECT styinvjl
    
    *!C201011,2 MMT 08/25/2008 Fix bug of wrong shipment No.[Start]
    *SET RELATION TO ctrcode+STR(lineno,6) INTO (loformset.shipcursor)
    SET RELATION TO ctrcode+STR(lineno,6)+crsession  INTO (loformset.shipcrs)
    *!C201011,2 MMT 08/25/2008 Fix bug of wrong shipment No.[End]
    
    SELECT(oldAlias)   

*:**************************************************************************
*:* Name        : lfADDCOL
*:* Developer   : AWD - Ahmed Awaad
*:* Date        : 06/01/2008
*:* Purpose     : Adding a clomn to view ship # on style screen> cut&sold > total inv
*:***************************************************************************
FUNCTION lfADDCOL
 shipnocol = loformset.shipcrs+".shipno :15 :H='Shipment #',"
 RETURN  shipnocol

*:**************************************************************************
*:* Name        : lfADDPROP
*:* Developer   : AWD - Ahmed Awaad
*:* Date        : 06/01/2008
*:* Purpose     : Adding prop. to FormSet
*:***************************************************************************
FUNCTION  lfADDPROP
IF TYPE('loformset.shipcrs') = 'U'
 loformset.addproperty('shipcrs')
ENDIF  
