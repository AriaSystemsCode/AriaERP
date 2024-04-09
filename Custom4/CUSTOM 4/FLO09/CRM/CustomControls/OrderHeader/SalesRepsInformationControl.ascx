<link rel="stylesheet" type="text/css" href="../StyleSheet.css" />
<%@ Control Language="C#" AutoEventWireup="true" CodeFile="SalesRepsInformationControl.ascx.cs"
    Inherits="SalesRepsInformationControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
        <asp:Panel ID="SalesRepsPanel" runat="server" Height="33px" Width="780px">
            <asp:Label ID="Label1" runat="server" Style="position: relative; left: 10px; top: 10px;"
                Text="Sales Rep 1:" Font-Underline="True"></asp:Label>
            <asp:TextBox ID="SalesRep1" runat="server" AutoPostBack="true" OnTextChanged="SalesRep1_TextChanged"
                Style="position: relative; left: 8px; top: 10px;" Width="50px" Enabled="False"></asp:TextBox>
            <asp:TextBox ID="SalesRep1Name" runat="server" Style="position: relative; left: 8px;
                top: 10px;" Width="156px" Enabled="False"></asp:TextBox>
          <asp:TextBox ID="SalesrepCommission1" runat="server" Enabled="False" Style="left: 7px;
            position: relative; top: 10px" Width="56px" ReadOnly="True"></asp:TextBox>
          <asp:TextBox ID="TextBox1" runat="server" Enabled="False" ReadOnly="True" Style="left: 7px;
            position: relative; top: 10px" Visible="False" Width="56px"></asp:TextBox>&nbsp;
            <asp:LinkButton ID="btnSalesRep2" runat="server" Text="Sales Rep 2:" Style="position: relative;
                left: 9px; top: 10px;" Font-Underline="True" OnClick="btnSalesRep2_Click"></asp:LinkButton>
            <asp:TextBox ID="SalesRep2" runat="server" AutoPostBack="true" OnTextChanged="SalesRep2_TextChanged"
                Style="position: relative; left: 10px; top: 10px;" Width="50px"></asp:TextBox>
            <asp:TextBox ID="SalesRep2Name" runat="server" Style="position: relative; left: 11px;
                top: 10px;" Width="169px" Enabled="False"></asp:TextBox>
          <asp:TextBox ID="SalesrepCommission2" runat="server" Enabled="False" 
            Style="left: 12px; position: relative; top: 10px" Width="64px" ReadOnly="True"></asp:TextBox>
          <asp:TextBox ID="TextBox2" runat="server" Enabled="False" ReadOnly="True" Style="left: 12px;
            position: relative; top: 10px" Visible="False" Width="64px"></asp:TextBox>
          </asp:Panel>
        <cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" BorderColor="LightGray"
            TargetControlID="SalesRepsPanel">
        </cc1:RoundedCornersExtender>
        <asp:Panel ID="Panel1" runat="server" Height="2px" Style="left: 9px; position: relative;
            top: -56px; background-color: white; z-index: 20" Width="141px">
            <asp:Label ID="Label3" runat="server" BackColor="White" Text="Sales Reps Information"
                Width="100%"></asp:Label></asp:Panel>
        <cc1:AutoCompleteExtender ID="AutoCompleteExtender1" runat="server" ServicePath="~/WebServices/AutoComplete/OrderHeader/SalesRepAutoComplete.asmx"
            ServiceMethod="GetCompletionList" TargetControlID="SalesRep1" UseContextKey="True"
            MinimumPrefixLength="1" CompletionInterval="10" EnableCaching="false" CompletionListCssClass="SalesRep">
        </cc1:AutoCompleteExtender>
        <cc1:AutoCompleteExtender ID="AutoCompleteExtender2" runat="server" ServicePath="~/WebServices/AutoComplete/OrderHeader/SalesRepAutoComplete.asmx"
            ServiceMethod="GetCompletionList" TargetControlID="SalesRep2" UseContextKey="True"
            MinimumPrefixLength="1" CompletionInterval="10" EnableCaching="false" CompletionListCssClass="SalesRep">
        </cc1:AutoCompleteExtender>
