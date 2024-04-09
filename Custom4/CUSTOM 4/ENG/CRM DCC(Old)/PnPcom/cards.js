

<!-- Begin
var Cards = new makeArray(8);
Cards[0] = new CardType("MasterCard", "51,52,53,54,55", "16");
var MasterCard = Cards[0];
Cards[1] = new CardType("VisaCard", "4", "13,16");
var VisaCard = Cards[1];
Cards[2] = new CardType("AmExCard", "34,37", "15");
var AmExCard = Cards[2];
Cards[3] = new CardType("DinersClubCard", "30,36,38", "14");
var DinersClubCard = Cards[3];
Cards[4] = new CardType("DiscoverCard", "6011", "16");
var DiscoverCard = Cards[4];
Cards[5] = new CardType("enRouteCard", "2014,2149", "15");
var enRouteCard = Cards[5];
Cards[6] = new CardType("JCBCard", "3088,3096,3112,3158,3337,3528", "16");
var JCBCard = Cards[6];
var LuhnCheckSum = Cards[7] = new CardType();






/*************************************************************************\
CheckCardNumber(form)
function called when users click the "check" button.
\*************************************************************************/
function CheckCardNumber(form) {
var tmpyear;

if (form.Name.value == "" ) {
    alert("You must indicate your Name");
    form.Name.focus();
    form.Name.select();
    return false;
						}

if (form.Company.value == "" ) {
    alert("You must indicate your Company Name");
    form.Company.focus();
    form.Company.select();
    return false;
						}

if (form.Street.value == "" ) {
    alert("You must indicate your Street Address");
    form.Street.focus();
    form.Street.select();
    return false;
						}

if (form.City.value == "" ) {
    alert("You must indicate your City");
    form.City.focus();
    form.City.select();
    return false;
						}

if (form.State.value == "" ) {
    alert("You must indicate your State");
    form.State.focus();
    form.State.select();
    return false;
						}
if (form.Zip.value == "" ) {
    alert("You must indicate your ZipCode");
    form.Zip.focus();
    form.Zip.select();
    return false;
						}
if (form.Country.value == "" ) {
    alert("You must indicate your Country");
    form.Country.focus();
    form.Country.select();
    return false;
						}


// Email

if (form.EMail.value == "" ) {
    alert("You must indicate your E-Mail Address");
    form.EMail.focus();
    form.EMail.select();
    return false;
						}

	 if (!isEmail(form.EMail.value) )  {
		alert("You must indicate Correct E-Mail Address");
    form.EMail.focus();
    form.EMail.select();
    return false;
						}


  if (form.UserName.value == "" ) {
    alert("You must indicate your User Name");
    form.UserName.focus();
    form.UserName.select();
    return false;
						}

  if (form.UserName.value != "" ) {

	if (!doesStringHaveOnlyCharset(form.UserName.value.toLowerCase(),"qwertyuiopasdfghjklzxcvbnm1234567890"))
		{
		alert("your username must be alpha-numeric only");
		form.UserName.focus();
	    form.UserName.select();
		return false;
		}
								}

  if (form.password.value == "" ) {
    alert("You must indicate your password");
    form.password.focus();
    form.password.select();
    return false;
						}

  if (form.password.value.length < 8 ) {
    alert("Your password must be 8 Letters at least");
    form.password.focus();
    form.password.select();
    return false;
						}

// DomainPrefix
if (form.DomainPrefix.value == "" ) {
    alert("You must indicate your primary domain");
    form.DomainPrefix.focus();
    form.DomainPrefix.select();
    return false;
						}

	 if (!isStringDomainName(form.DomainPrefix.value) )  {
		alert("your primary domain must be alpha-numeric or have '-' only");
    form.DomainPrefix.focus();
    form.DomainPrefix.select();
    return false;
						}


//other card type
if (form.EZPASS.value == 0) {

	if (form.CardNumber.value.length == 0) {
	alert("Please enter a Card Number.");
	form.CardNumber.focus();
	form.CardNumber.select();
	return false;
	}

	//cvv2
	if (form.CVV2.value.length != 3) {
	alert("CVV2 must be three digits");
	form.CVV2.focus();
	form.CVV2.select();
	return false;
	}

	tmpyear = "20" + form.CardEXPy.options[form.CardEXPy.selectedIndex].value;
	tmpmonth = form.CardEXPm.options[form.CardEXPm.selectedIndex].value;

	if (!(new CardType()).isExpiryDate(tmpyear, tmpmonth)) {
	alert("This card has already expired.");
	return false;
	}

	card = form.CardType.options[form.CardType.selectedIndex].value;
	var retval = eval(card + ".checkCardNumber(\"" + form.CardNumber.value +
	"\", " + tmpyear + ", " + tmpmonth + ");");
	cardname = "";


}
else
	{
	retval = true;
	} // end ezpass





if (retval)
	{
	// comment this out if used on an order form
	//cardname = "";
	form.verify.value = "Verify Order ... Please Wait ";
	form.verify.disabled = true;
	form.submit();
	}


//return false;

else {
	// The cardnumber has the valid luhn checksum, but we want to know which
	// cardtype it belongs to.
	alert("Please Check the card number or Card Type again.");
	form.CardNumber.select();
	return false;
   }





}

function CardType() {
var n;
var argv = CardType.arguments;
var argc = CardType.arguments.length;

this.objname = "object CardType";

var tmpcardtype = (argc > 0) ? argv[0] : "CardObject";
var tmprules = (argc > 1) ? argv[1] : "0,1,2,3,4,5,6,7,8,9";
var tmplen = (argc > 2) ? argv[2] : "13,14,15,16,19";

this.setCardNumber = setCardNumber;  // set CardNumber method.
this.setCardType = setCardType;  // setCardType method.
this.setLen = setLen;  // setLen method.
this.setRules = setRules;  // setRules method.
this.setExpiryDate = setExpiryDate;  // setExpiryDate method.

this.setCardType(tmpcardtype);
this.setLen(tmplen);
this.setRules(tmprules);
if (argc > 4)
this.setExpiryDate(argv[3], argv[4]);

this.checkCardNumber = checkCardNumber;  // checkCardNumber method.
this.getExpiryDate = getExpiryDate;  // getExpiryDate method.
this.getCardType = getCardType;  // getCardType method.
this.isCardNumber = isCardNumber;  // isCardNumber method.
this.isExpiryDate = isExpiryDate;  // isExpiryDate method.
this.luhnCheck = luhnCheck;// luhnCheck method.
return this;
}

/*************************************************************************\
boolean checkCardNumber([String cardnumber, int year, int month])
return true if cardnumber pass the luhncheck and the expiry date is
valid, else return false.
\*************************************************************************/
function checkCardNumber() {
var argv = checkCardNumber.arguments;
var argc = checkCardNumber.arguments.length;
var cardnumber = (argc > 0) ? argv[0] : this.cardnumber;
var year = (argc > 1) ? argv[1] : this.year;
var month = (argc > 2) ? argv[2] : this.month;

this.setCardNumber(cardnumber);
this.setExpiryDate(year, month);

if (!this.isCardNumber())
return false;
if (!this.isExpiryDate())
return false;

return true;
}
/*************************************************************************\
String getCardType()
return the cardtype.
\*************************************************************************/
function getCardType() {
return this.cardtype;
}
/*************************************************************************\
String getExpiryDate()
return the expiry date.
\*************************************************************************/
function getExpiryDate() {
return this.month + "/" + this.year;
}
/*************************************************************************\
boolean isCardNumber([String cardnumber])
return true if cardnumber pass the luhncheck and the rules, else return
false.
\*************************************************************************/
function isCardNumber() {
var argv = isCardNumber.arguments;
var argc = isCardNumber.arguments.length;
var cardnumber = (argc > 0) ? argv[0] : this.cardnumber;
if (!this.luhnCheck())
return false;

for (var n = 0; n < this.len.size; n++)
if (cardnumber.toString().length == this.len[n]) {
for (var m = 0; m < this.rules.size; m++) {
var headdigit = cardnumber.substring(0, this.rules[m].toString().length);
if (headdigit == this.rules[m])
return true;
}
return false;
}
return false;
}

/*************************************************************************\
boolean isExpiryDate([int year, int month])
return true if the date is a valid expiry date,
else return false.
\*************************************************************************/
function isExpiryDate() {
var argv = isExpiryDate.arguments;
var argc = isExpiryDate.arguments.length;

year = argc > 0 ? argv[0] : this.year;
month = argc > 1 ? argv[1] : this.month;

if (!isNum(year+""))
return false;
if (!isNum(month+""))
return false;
today = new Date();
expiry = new Date(year, month);
if (today.getTime() > expiry.getTime())
return false;
else
return true;
}

/*************************************************************************\
boolean isNum(String argvalue)
return true if argvalue contains only numeric characters,
else return false.
\*************************************************************************/
function isNum(argvalue) {
argvalue = argvalue.toString();

if (argvalue.length == 0)
return false;

for (var n = 0; n < argvalue.length; n++)
if (argvalue.substring(n, n+1) < "0" || argvalue.substring(n, n+1) > "9")
return false;

return true;
}

function isEmail(str) {
  // are regular expressions supported?
  var supported = 0;
  if (window.RegExp) {
    var tempStr = "a";
    var tempReg = new RegExp(tempStr);
    if (tempReg.test(tempStr)) supported = 1;
  }
  if (!supported)
    return (str.indexOf(".") > 2) && (str.indexOf("@") > 0);
  var r1 = new RegExp("(@.*@)|(\\.\\.)|(@\\.)|(^\\.)");
  var r2 = new RegExp("^.+\\@(\\[?)[a-zA-Z0-9\\-\\.]+\\.([a-zA-Z]{2,3}|[0-9]{1,3})(\\]?)$");
  return (!r1.test(str) && r2.test(str));
}
function doesStringHaveOnlyCharset( s, charset ) {
    var i;
    for( i=0; i<s.length; i++ ) {
        if( charset.indexOf(s.charAt(i))==-1 ) {
            return false;
        }
    }
    return true;
}


function isStringDomainName( s ) {
    var lowerS = s.toLowerCase();
    return doesStringHaveOnlyCharset(lowerS,"qwertyuiopasdfghjklzxcvbnm1234567890-");
}



/*************************************************************************\
boolean luhnCheck([String CardNumber])
return true if CardNumber pass the luhn check else return false.
Reference: http://www.ling.nwu.edu/~sburke/pub/luhn_lib.pl
\*************************************************************************/
function luhnCheck() {
var argv = luhnCheck.arguments;
var argc = luhnCheck.arguments.length;

var CardNumber = argc > 0 ? argv[0] : this.cardnumber;

if (! isNum(CardNumber)) {
return false;
  }

var no_digit = CardNumber.length;
var oddoeven = no_digit & 1;
var sum = 0;

for (var count = 0; count < no_digit; count++) {
var digit = parseInt(CardNumber.charAt(count));
if (!((count & 1) ^ oddoeven)) {
digit *= 2;
if (digit > 9)
digit -= 9;
}
sum += digit;
}
if (sum % 10 == 0)
return true;
else
return false;
}

/*************************************************************************\
ArrayObject makeArray(int size)
return the array object in the size specified.
\*************************************************************************/
function makeArray(size) {
this.size = size;
return this;
}

/*************************************************************************\
CardType setCardNumber(cardnumber)
return the CardType object.
\*************************************************************************/
function setCardNumber(cardnumber) {
this.cardnumber = cardnumber;
return this;
}

/*************************************************************************\
CardType setCardType(cardtype)
return the CardType object.
\*************************************************************************/
function setCardType(cardtype) {
this.cardtype = cardtype;
return this;
}

/*************************************************************************\
CardType setExpiryDate(year, month)
return the CardType object.
\*************************************************************************/
function setExpiryDate(year, month) {
this.year = year;
this.month = month;
return this;
}

/*************************************************************************\
CardType setLen(len)
return the CardType object.
\*************************************************************************/
function setLen(len) {
// Create the len array.
if (len.length == 0 || len == null)
len = "13,14,15,16,19";

var tmplen = len;
n = 1;
while (tmplen.indexOf(",") != -1) {
tmplen = tmplen.substring(tmplen.indexOf(",") + 1, tmplen.length);
n++;
}
this.len = new makeArray(n);
n = 0;
while (len.indexOf(",") != -1) {
var tmpstr = len.substring(0, len.indexOf(","));
this.len[n] = tmpstr;
len = len.substring(len.indexOf(",") + 1, len.length);
n++;
}
this.len[n] = len;
return this;
}

/*************************************************************************\
CardType setRules()
return the CardType object.
\*************************************************************************/
function setRules(rules) {
// Create the rules array.
if (rules.length == 0 || rules == null)
rules = "0,1,2,3,4,5,6,7,8,9";

var tmprules = rules;
n = 1;
while (tmprules.indexOf(",") != -1) {
tmprules = tmprules.substring(tmprules.indexOf(",") + 1, tmprules.length);
n++;
}
this.rules = new makeArray(n);
n = 0;
while (rules.indexOf(",") != -1) {
var tmpstr = rules.substring(0, rules.indexOf(","));
this.rules[n] = tmpstr;
rules = rules.substring(rules.indexOf(",") + 1, rules.length);
n++;
}
this.rules[n] = rules;
return this;
}
//  End -->


