<%@ Control Language="C#" AutoEventWireup="true" CodeFile="StylesControl.ascx.cs" Inherits="StylesControl" %>
<%@ Register Src="DescriptionControl.ascx" TagName="DescriptionControl" TagPrefix="uc111" %>
<%@ Register Src="ProfilesControl.ascx" TagName="ProfilesControl" TagPrefix="uc1112" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>


<script language ="JScript">
var oPopup = window.createPopup();
function jsLineUpdated()
{    
    var oPopBody = oPopup.document.body;
    oPopBody.style.backgroundColor = "lightyellow";
    oPopBody.style.border = "solid black 2px";
    oPopBody.innerHTML = "<b><center>Line Updated</center></b>";
    oPopup.show(350, 50, 120, 25, document.body);
}
</script>
        <table style="border: solid 2px #cbd9fe; background-color: #eff3ff; height: 44px" id="TABLE1" language="javascript" >
            <tr>
                <td style="height: 0px; width: 40px;">
                </td>
            </tr>
            <tr>
                <td style="width: 40px; height: 29px;">
                  &nbsp;<asp:LinkButton ID="StyleBtn" runat="server" Font-Size=small Font-Names="Tahoma" Width="38px" OnClick="StyleBtn_Click">Style</asp:LinkButton></td>
                <td style="height: 29px">
                    <asp:TextBox ID="StyleCode" runat="server" Width="78px" AutoPostBack="True" OnTextChanged="StyleCode_Selected" MaxLength="12"></asp:TextBox></td>
                <td style="height: 29px">
                    <asp:Label ID="Label2" runat="server" Text="Color:"></asp:Label></td>
                <td style="height: 29px">
                    <asp:DropDownList ID="ColorCode" runat="server" Width="86px" AutoPostBack="True" OnTextChanged="ColorCode_TextChanged" Height="22px">
                    </asp:DropDownList>
                </td>
                <td style="height: 29px">
                    <asp:Label ID="Label11" runat="server" Text="Size:"></asp:Label></td>
                <td style="width: 10px; height: 29px;" dir="ltr">
                  <asp:DropDownList ID="ScaleCode" runat="server" Width="48px" AutoPostBack="True" Height="22px" OnSelectedIndexChanged="ScaleCode_SelectedIndexChanged">
                  </asp:DropDownList>
                </td>
                <td style="height: 29px; width: 3px;">
                  <asp:LinkButton ID="Label3" runat="server" Font-Names="Tahoma" Font-Size="small"
                    OnClick="Label3_Click" Width="38px">Store:</asp:LinkButton></td>
                <td style="height: 29px; width: 5px;">
                  <asp:TextBox ID="StoreCode" runat="server" AutoPostBack="True" OnTextChanged="StoreCode_TextChanged" Width="92px"></asp:TextBox></td>
                <td style="width: 10px; height: 29px;">
                </td>
                <td style="height: 29px; width: 35px;">
                    <asp:Label ID="Label4" runat="server" Text="PO#:" Enabled="False" EnableTheming="True"></asp:Label></td>
                <td style="width: 82px; height: 29px;">
                    <asp:TextBox ID="POCode" runat="server" Width="79px" Enabled="False" Height="21px" MaxLength="15"></asp:TextBox></td>
                <td style="width: 10px; height: 29px;">
                </td>
                <td style="height: 29px">
                    <asp:Button ID="UpdateLineBtn" runat="server" Text="Update Line" Width="75px" OnClick="UpdateLineBtn_Click" Height="24px"/></td>
                <td style="width: 10px; height: 29px;">
                </td>
                <td style="height: 29px">
                    <asp:Button ID="ResetBtn" runat="server" Text="Reset" Width="60px" OnClick="ResetBtn_Click" /></td>
            </tr>
        </table>
        <cc1:AutoCompleteExtender ID="AutoCompleteExtender1" runat="server" ServicePath="~/WebServices/AutoComplete/OrderDetails/StyleCodeAutoComplete.asmx"
            ServiceMethod="GetCompletionList" TargetControlID="StyleCode" UseContextKey="True"
            MinimumPrefixLength="1" CompletionListCssClass ="Style" CompletionInterval="10"
            EnableCaching="false">
        </cc1:AutoCompleteExtender><cc1:AutoCompleteExtender ID="AutoCompleteExtender2" runat="server" ServicePath="~/WebServices/AutoComplete/OrderDetails/StoreAutoComplete.asmx"
            ServiceMethod="GetCompletionList" TargetControlID="StoreCode" UseContextKey="True"
            MinimumPrefixLength="1" CompletionListCssClass ="AutoCompleteExtenderStore" CompletionInterval="10"
            EnableCaching="false">
        </cc1:AutoCompleteExtender>
        <div class="style_dropdown" id="StyleAutoComplete" runat="server">
        </div>

    
