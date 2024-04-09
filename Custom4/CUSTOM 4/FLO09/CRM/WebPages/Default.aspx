<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Default.aspx.cs" Inherits="WebPages_Default" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Untitled Page</title>
<script language="javascript" type="text/javascript">

   var myReq = new XMLHttpRequest();
    function callWSMethod2()

    {
        if (window.XMLHttpRequest)
            {
                var url = "http://localhost:3778/Aria.CRM/WebServices/AutoComplete/Profile/Profile.asmx?op=GetProfile?ProfileNo=1";
                myReq.onreadystatechange = checkStatus2;
                myReq.open("GET", url, true);
                alert("aa");
            }
    }

    function checkStatus2()
    {
    alert("aa1");
        if (myReq.readyState == 4)
        {
            alert(myReq.responseText);
        }
    }  



function Button2_onclick() {
var args = window; 
var modal = window.showModalDialog("configure.aspx", args, "dialogWidth=800px;dialogHeight=600px;scrollbars=yes;resizable=no;status=no;help=no"); 

}

function Button3_onclick() {
//callWSMethod2();
SendRequest();
}



    function FillProfiles()
    {
        Profile.GetProfile(1, SucceededCallback);
    }


    function SucceededCallback(result, eventArgs)
    {
        // Page element to display feedback.
        var RsltElem = document.getElementById("TextBox1");
        RsltElem.value = result;
    }

// -->
</script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
      &nbsp;
      <asp:ScriptManager ID="ScriptManager1" runat="server">
      <Scripts>
          <asp:ScriptReference Path = "Profile.js" />
      </Scripts>
      <Services>
          <asp:ServiceReference  Path="Profile.asmx" />
      </Services>
      </asp:ScriptManager>
      
      <asp:TextBox ID="TextBox2" runat="server"></asp:TextBox>
      <asp:TextBox ID="TextBox1" runat="server"></asp:TextBox>
      <asp:Button ID="Button1" runat="server" OnClick="Button1_Click1" Text="Button" />
      <asp:TextBox ID="TextBox3" runat="server"></asp:TextBox><br />
      <br />
      <asp:GridView ID="GridView1" runat="server">
      </asp:GridView>
      <br />
      <asp:Button ID="Button2" runat="server" OnClick="Button2_Click" Text="Button" />
      <input id="Button3" type="button" value="button3" language="javascript" onclick="FillProfiles()" /></div>
    </form>
</body>
</html>
