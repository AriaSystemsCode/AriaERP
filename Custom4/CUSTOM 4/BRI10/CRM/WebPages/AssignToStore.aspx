<%@ Page Language="C#" AutoEventWireup="true" CodeFile="AssignToStore.aspx.cs" Inherits="WebPages_AssignToStore" EnableEventValidation = "false"  AsyncTimeout = "3600" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <link visible="false" />
    <base target="_self" />    
    <title>Assign to Stores</title>
    <script language = "jscript" type = "text/jscript">
    var lastValue = "";

    function setDefault()
    {
          document.getElementById("HiddenField1").value = window.dialogArguments;
    }
             
    function SaveLastValue(value)
    {
      lastValue = value;
    }

    function IsNumber(fld, length)
    {
      
      var RegExPattern = /^[0-9]+$/;

      if ((fld.value.match(RegExPattern) && fld.value.length <= length) || fld.value == '')
      {
      }
      else
      {
        try
        {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        }
        catch(e)
        {
        }
      }
    }
    </script>




<script language="javascript" type="text/javascript">
<!--

function check(e)
{
if(e.keyCode == 13)
{
  btnSearch_onclick();
}
return true;
}

function btnSearch_onclick() {
  var search = window.document.getElementById("Text1").value;
  var controls = window.document.getElementById("HiddenField2").value;
  var stores = window.document.getElementById("HiddenField3").value;
  
  var col_array = controls.split(", ");
  var store_array = stores.split(", ");
  
  for(i = 0; i < store_array.length; i++)
  {
    if(search.toString().replace(/^\s*/, "").replace(/\s*$/, "").toUpperCase() == store_array[i].toString().replace(/^\s*/, "").replace(/\s*$/, "").toUpperCase())
    {
      if(i + 17 < col_array.length)
      {
        window.document.getElementById(col_array[i + 17].toString()).focus();
      }
      else
      {
        window.document.getElementById(col_array[col_array.length - 1].toString()).focus();
      }
        window.document.getElementById(col_array[i].toString()).focus();
      
     return;
    }
  }
  
  alert("No matched store found!");
}

function btnSelectAll_onclick() {
  var controls = window.document.getElementById("HiddenField4").value;
  var col_array = controls.split(", ");
  var obj1 = window.document.getElementById(col_array[0].toString()).parentElement.parentElement.parentElement;

  for(i = 1; i < col_array.length + 1; i++)
  {
    if(!obj1.children[i].children[0].children[0].checked)
    {
      obj1.children[i].children[0].children[0].checked  = true;
      obj1.children[i].children[3].children[0].value  = '1';
    }
  }
}

function btnSelectNone_onclick() {
  var controls = window.document.getElementById("HiddenField4").value;
  var col_array = controls.split(", ");
  var obj1 = window.document.getElementById(col_array[0].toString()).parentElement.parentElement.parentElement;

  for(i = 1; i < col_array.length + 1; i++)
  {
    obj1.children[i].children[0].children[0].checked  = false;
    obj1.children[i].children[3].children[0].value  = '';
  }
}


function OkButton_OnClientClick()
{
  alert('This process may take few minutes, please wait!') ;
  window.document.getElementById("btnSelectAll").disabled = true;
  window.document.getElementById("btnSelectNone").disabled = true;
  window.document.getElementById("CancelButton").disabled = true;
  window.document.getElementById("Text2").disabled = false;
  window.document.getElementById("Text2").value = " Please wait...";
  window.document.getElementById("Text2").style.backgroundColor = "HotPink";
  
}

// -->
</script>
</head>
<body onload = "setDefault();">
    <form id="form1" runat="server">
        <div>       
            <table width="100%">
                <tr>
                    <td>
                      Find Store:
                      <input id="Text1" style="width: 62px" type="text" maxlength="8" onkeypress = "return check(event)" />
                      <input id="btnSearch" type="button" value=" Go " language="javascript" onclick="return btnSearch_onclick()" /><br />
                      <hr />
                      <br />
                        <asp:Table ID="Table1" runat="server" Height="200px" EnableViewState="true">
                            <asp:TableRow>
                                <asp:TableCell></asp:TableCell>
                                <asp:TableCell>
                                    <asp:Label runat="server" ID="storeLabel" Text="Store"></asp:Label></asp:TableCell>
                                <asp:TableCell>
                                    <asp:Label runat="server" ID="poLabel" Text="PO#"></asp:Label></asp:TableCell>
                                <asp:TableCell>
                                    <asp:Label runat="server" ID="qtyLabel" Text="Qty"></asp:Label></asp:TableCell>
                            </asp:TableRow>
                        </asp:Table>
                    </td>
                </tr>
                <tr>
                    <td>
                        <div align="center" style="text-align: left">
                          <input id="btnSelectAll" type="button" value="Select All" language="javascript" onclick="return btnSelectAll_onclick()"/>
                          <input id="btnSelectNone" type="button" value="Select None" language="javascript" style="width: 92px" onclick="return btnSelectNone_onclick()"/>
                          &nbsp;<input id="Text2"  disabled ="disabled" style="border-right: white thin solid;
                            border-top: white thin solid; border-left: white thin solid; border-bottom: white thin solid; width: 132px; color: white;"
                            type="text" />
                            <asp:Button ID="OkButton" runat="server" Text="OK" OnClick="OkButton_Click" UseSubmitBehavior ="false" OnClientClick = "OkButton_OnClientClick();" />
                            <asp:Button ID="CancelButton" runat="server" OnClientClick = "window.close();" Text="Cancel" UseSubmitBehavior ="false" />
                        </div>
                    </td>
                </tr>
            </table>
        </div>
        <asp:HiddenField ID="HiddenField1" runat="server" />
      <asp:HiddenField ID="HiddenField2" runat="server" /><asp:HiddenField ID="HiddenField3" runat="server" /><asp:HiddenField ID="HiddenField4" runat="server" />
      &nbsp; &nbsp;&nbsp;
    </form>
</body>
</html>
