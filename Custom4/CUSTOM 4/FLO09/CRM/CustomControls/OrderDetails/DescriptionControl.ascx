<%@ Control Language="C#" AutoEventWireup="true" CodeFile="DescriptionControl.ascx.cs" Inherits="DescriptionControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<script language="javascript" type="text/javascript">
// <!CDATA[

function jsCalculateTotal() {
    var txtBox = new Array();
    txtBox[0] = document.getElementById("<%= OrderQty1.ClientID %>");
    txtBox[1] = document.getElementById("<%= OrderQty2.ClientID %>");
    txtBox[2] = document.getElementById("<%= OrderQty3.ClientID %>");
    txtBox[3] = document.getElementById("<%= OrderQty4.ClientID %>");
    txtBox[4] = document.getElementById("<%= OrderQty5.ClientID %>");
    txtBox[5] = document.getElementById("<%= OrderQty6.ClientID %>");
    txtBox[6] = document.getElementById("<%= OrderQty7.ClientID %>");
    txtBox[7] = document.getElementById("<%= OrderQty8.ClientID %>");
    
    
    var values = new Array();
    var totalValue = 0;
    for(i = 0; i < 8; i=i+1){
        if(txtBox[i]!= null){
            if(parseInt(txtBox[i].value).toString() == "NaN"){
                values[i] = 0;  
            }
            else
            {
                values[i] = parseInt(txtBox[i].value);
            }                
        }
        else
        {
            values[i] = 0;
        }        
        totalValue = totalValue+values[i];       
    }
    
    var totalTxt = document.getElementById("<%= Total.ClientID %>");      
    
    totalTxt.innerText = totalValue;    
}

// ]]>
</script>

<table style=" ;background-color: #cbd9fe;" id="TABLE1">
    <tr >
        <td style=" ; background-color:#eff3ff; background-color:#eff3ff; height: 26px; width: 76px;">
          &nbsp;<asp:LinkButton ID="StyleBtn" runat="server" Font-Names="Tahoma" Font-Size="small"
            OnClick="StyleBtn_Click" Width="50px">Description</asp:LinkButton></td>
        <td style=" ; background-color:#eff3ff; height: 26px;" colspan="9">
            <asp:TextBox ID="StyleDescription" runat="server" Width="575px" OnTextChanged="StyleDescription_TextChanged" AutoPostBack="True" MaxLength="60" ></asp:TextBox>
        </td>
        <td  style=" ; background-color:#eff3ff; padding:3px 3px 3px 3px; height: 26px; width: 46px;">
          &nbsp;<asp:Label ID="Label13" runat="server" Text="Group"></asp:Label></td>
        <td style=" ; background-color:#eff3ff; height: 26px; width: 31px;" dir="ltr">
            <asp:TextBox  id="Group" runat="server" Enabled = "false" MaxLength = "1" Width="47px"/></td>
    </tr>
    <tr>
        <td style="height: 21px;  ; background-color:#eff3ff; width: 76px;">
            &nbsp;</td>
        <td style="height: 21px;  ; background-color:#eff3ff; width: 59px;">
            &nbsp;<asp:Label ID="scale1" runat="server" style=" "></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; width:60px ">
            &nbsp;<asp:Label ID="scale2" runat="server" style=" "></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; width:60px ">
            &nbsp;<asp:Label ID="scale3" runat="server" style=" "></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; width:60px ">
            &nbsp;<asp:Label ID="scale4" runat="server" style=" "></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; width:60px ">
            &nbsp;<asp:Label ID="scale5" runat="server" style=" "></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff;width:60px  ">
            &nbsp;<asp:Label ID="scale6" runat="server" style=""></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; width:60px  ">
            &nbsp;<asp:Label ID="scale7" runat="server" style=""></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff;width:60px  ">
            &nbsp;<asp:Label ID="scale8" runat="server"></asp:Label>
        </td>
        <td style="height: 21px; ; background-color:#eff3ff; ">
            &nbsp;
          <asp:Label ID="Label2" runat="server" Text="Price"></asp:Label></td>
        <td style="height: 21px; ; background-color:#eff3ff;  width: 46px;">
          &nbsp;<asp:Label ID="Label12" runat="server" Text="Disc%"></asp:Label></td>
        <td style="height: 21px; ; background-color:#eff3ff;  width: 31px;">
            &nbsp;
          <asp:Label ID="Label1" runat="server" Text="Net"></asp:Label></td>
    </tr>
    <tr>
        <td style="width: 76px; ; background-color:#eff3ff; height: 26px;">
            <asp:Label ID="Label37" runat="server" Text="Order" Width="69px"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff; width:59px; height: 26px;">
            <asp:TextBox id="OrderQty1" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty2" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty3" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty4" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty5" runat="server" Width="52px" style="text-align: center" Enabled="False"/></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty6" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty7" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  height: 26px;">
            <asp:TextBox id="OrderQty8" runat="server" Width="52px" style="text-align: center" Enabled="False" /></td>
        <td style=" ; background-color:#eff3ff;  width: 55px; height: 26px;">
          <asp:TextBox ID="GrossPrice" runat="server" Width="53px" Enabled="False">0</asp:TextBox></td>
        <td style=" ; background-color:#eff3ff;  width: 46px; height: 26px;">
          <asp:TextBox  id="Disc" runat="server" Enabled = "false" Width="46px" >0</asp:TextBox></td>
        <td style=" ; background-color:#eff3ff;  width: 31px; height: 26px;">
          <asp:TextBox ID="NetPrice" runat="server" Width="48px" Enabled="False">0</asp:TextBox></td>
    </tr>
    <tr>
        <td style=" ; background-color:#eff3ff; width: 76px;">
            <asp:Label ID="Label38" runat="server" Text="OTS" Width="68px"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff; width: 59px;">
            <asp:Label ID="OTS1" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff">
            <asp:Label ID="OTS2" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff;">
            <asp:Label ID="OTS3" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff;">
            <asp:Label ID="OTS4" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff">
            <asp:Label ID="OTS5" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff">
            <asp:Label ID="OTS6" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff">
            <asp:Label ID="OTS7" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff">
            <asp:Label ID="OTS8" runat="server" Width="60px" style=" text-align:center"></asp:Label>
        </td>
        <td style=" ; background-color:#eff3ff; width: 55px;">
        &nbsp;
            </td>
        <td style=" ; background-color:#eff3ff; width: 46px;">
        &nbsp;
            <asp:Label ID="Label14" runat="server" Text="Total" Width="46px"></asp:Label></td>
        <td style=" ; background-color:#eff3ff; width: 31px;">
        &nbsp;
            <asp:Label ID="Total" Text="0" style="text-align:center" runat="server" Width="40px"></asp:Label></td>
    </tr>
</table>
<cc1:autocompleteextender id="AutoCompleteExtender1" runat="server" ServicePath="~/WebServices/AutoComplete/OrderDetails/StyleDescriptionAutoComplete.asmx" ServiceMethod="GetCompletionList"
    TargetControlID="StyleDescription" UseContextKey="True" MinimumPrefixLength="1" CompletionListElementID="DescriptionAutoComplete" EnableCaching="false" CompletionListCssClass ="StyleDescription" Enabled="True" ></cc1:autocompleteextender>
&nbsp;&nbsp;

   