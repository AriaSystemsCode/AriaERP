//var laRadio = new Array();

function Is()
{
    var agent = navigator.userAgent.toLowerCase();
    this.major = parseInt(navigator.appVersion);
    this.minor = parseFloat(navigator.appVersion);
    this.ns  = ((agent.indexOf('mozilla')!=-1) && ((agent.indexOf('spoofer')==-1) && (agent.indexOf('compatible') == -1)));
    this.ns2 = (this.ns && (this.major == 2));
    this.ns3 = (this.ns && (this.major == 3));
    this.ns4b = (this.ns && (this.minor < 4.04));
    this.ns4 = (this.ns && (this.major >= 4));
    this.ns6 = (this.ns && (this.major >= 5));
    this.ie   = (agent.indexOf("msie") != -1);
    this.ie3  = (this.ie && (this.major == 2));
    this.ie4  = (this.ie && (this.major >= 4));
    this.op3 = (agent.indexOf("opera") != -1);
    this.win   = (agent.indexOf("win")!=-1);
    this.mac   = (agent.indexOf("mac")!=-1);
    this.unix  = (agent.indexOf("x11")!=-1);
}

function lfChng(src,srcVal)
{
	var newIs = new Is()

	if (newIs.ie4)
	{
		for (lnLoop = 0 ; lnLoop < laRadio.length; lnLoop++)
		{
			lcDiv = eval(laRadio[lnLoop]);
			lcDiv.style.display = 'none';
		}
		lcToShow = eval(src.value)
		lcToShow.style.display = 'block'
	}
	if (newIs.ns6)
	{


		for (lnLoop = 0 ; lnLoop < laRadio.length; lnLoop++)
		{
			lcToHideDiv = eval('srcVal'+'.' + laRadio[lnLoop]);
			lcToHideDiv.style.display = 'none'
		}
		lc = src.value;
		lcToShow = eval('srcVal'+'.'+lc);
		lcToShow.style.display = 'block';
	}
}

function CheckForm(objForm)
{
	//var intFormLoop
	var intFormLen = objForm.length - 1
	for (intFormLoop = 0 ; intFormLoop <  intFormLen ; intFormLoop++)
	{
		if (objForm.elements(intFormLoop).getAttribute("Req",false) == "Y")
		{
			if (objForm.elements(intFormLoop).value == "")
			{
				alert(objForm.elements(intFormLoop).getAttribute("ERRORTXT",false))
				return false
			}
		}
	}
	
	return true
}
