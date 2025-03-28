/*HM_Loader.js
* by Peter Belesis. v4.0.12 010612
* Copyright (c) 2001 Peter Belesis. All Rights Reserved.
*/
   HM_DOM = (document.getElementById) ? true : false;
   HM_NS4 = (document.layers) ? true : false;
    HM_IE = (document.all) ? true : false;
   HM_IE4 = HM_IE && !HM_DOM;
   HM_Mac = (navigator.appVersion.indexOf("Mac") != -1);
  HM_IE4M = HM_IE4 && HM_Mac;
HM_IsMenu = (HM_DOM || HM_NS4 || (HM_IE4 && !HM_IE4M));

HM_BrowserString = HM_NS4 ? "NS4" : HM_DOM ? "DOM" : "IE4";

if(window.event + "" == "undefined") event = null;
function HM_f_PopUp(){return false};
function HM_f_PopDown(){return false};
popUp = HM_f_PopUp;
popDown = HM_f_PopDown;

//Start Comment out in hm_loader.txt

//End Comment out in hm_loader.txt

// the following function is included to illustrate the improved JS expression handling of
// the left_position and top_position parameters
// you may delete if you have no use for it

function HM_f_CenterMenu(topmenuid) {
	var MinimumPixelLeft = 0;
	var TheMenu = HM_DOM ? document.getElementById(topmenuid) : HM_IE4 ? document.all(topmenuid) : eval("window." + topmenuid);
	var TheMenuWidth = HM_DOM ? parseInt(TheMenu.style.width) : HM_IE4 ? TheMenu.style.pixelWidth : TheMenu.clip.width;
	var TheWindowWidth = HM_IE ? document.body.clientWidth : window.innerWidth;
	return Math.max(parseInt((TheWindowWidth-TheMenuWidth) / 2),MinimumPixelLeft);
	
}

if(HM_IsMenu) {
	strPath = window.location.pathname
	strPath = strPath.split("/")
	
	/* WMA  User setup CRM at his wwwRoot then we avoid crm folder or any other folder name */
	//document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/" + strPath[1] + "/menu/HM_Arrays_Cust.js' TYPE='text/javascript'></SCR" + "IPT>");
	//document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/" + strPath[1] + "/HM_Script"+ HM_BrowserString +".js' TYPE='text/javascript'></SCR" + "IPT>");
	strFolder = strPath[1].split("."); //"RepCust.asp"
	if(strFolder[1] == "asp" || strPath[1] == "catalog" || strPath[1] == "common" || strPath[1] == "customer" || strPath[1] == "celpdesk" || strPath[1] == "invoice" || strPath[1] == "order" || strPath[1] == "ots" || strPath[1] == "ra"){
		//wwwRoot
		document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/"  + "menu/HM_Arrays_Cust.js' TYPE='text/javascript'></SCR" + "IPT>");
		document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/"  + "HM_Script"+ HM_BrowserString +".js' TYPE='text/javascript'></SCR" + "IPT>");
	}
	else{
		//CRM Folder or any other folder
		document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/" + strPath[1] + "/menu/HM_Arrays_Cust.js' TYPE='text/javascript'></SCR" + "IPT>");
		document.write("<SCR" + "IPT LANGUAGE='JavaScript1.2' SRC='/" + strPath[1] + "/HM_Script"+ HM_BrowserString +".js' TYPE='text/javascript'></SCR" + "IPT>");
	}


}


//end