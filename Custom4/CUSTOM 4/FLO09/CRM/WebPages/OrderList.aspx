<%@ Page Language="C#" AutoEventWireup="true" CodeFile="OrderList.aspx.cs" Inherits="OrderList" EnableEventValidation = "false"  %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Untitled Page</title>
</head>
<body>
    <form id="OrderListForm" runat="server">
    <div>
      <table>
        <tr>
          <td style="font-weight: bold; font-size: 1.5em; color: white; background-color: navy">
            Modify / Copy Order
          </td>
        </tr>
        <tr>
          <td style="text-align: right">
            <asp:GridView ID="Orders" runat="server" AutoGenerateColumns="False" AllowPaging="True" CellPadding="4" ForeColor="#333333" GridLines="None" OnPageIndexChanging="Orders_PageIndexChanging" OnRowCommand="Orders_RowCommand" Width="711px">
              <Columns>
                <asp:BoundField DataField="Order" HeaderText="Order" />
                <asp:BoundField DataField="Customer" HeaderText="Customer" ItemStyle-Font-Size = "Medium" ItemStyle-HorizontalAlign = "Left" />
                <asp:BoundField DataField="EnterDate" HeaderText="Enter" DataFormatString="{0:MM/dd/yyyy}" HtmlEncode="False" />
                <asp:BoundField DataField="TotBooked" HeaderText="Booked Qty." />
                <asp:BoundField DataField="TotShipped" HeaderText="Shipped Qty." />
                <asp:BoundField DataField="Amount" HeaderText="Amount" DataFormatString = "{0:c2}" />
                <asp:BoundField DataField="Store" HeaderText="Store"  ItemStyle-Font-Size = "Medium" ItemStyle-HorizontalAlign = "Left"/>
                <asp:TemplateField HeaderText="Modify">
                  <ItemTemplate>
                    <asp:LinkButton ID="btnModify" runat="server">Modify</asp:LinkButton>
                  </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Copy">
                  <ItemTemplate>
                    <asp:LinkButton ID="btnCopy" runat="server">Copy</asp:LinkButton>
                  </ItemTemplate>
                </asp:TemplateField>
              </Columns>
              <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
              <RowStyle BackColor="#EFF3FB" />
              <EditRowStyle BackColor="#2461BF" />
              <SelectedRowStyle BackColor="#D1DDF1" Font-Bold="True" ForeColor="#333333" />
              <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
              <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
              <AlternatingRowStyle BackColor="White" />
              <EmptyDataTemplate>
                <strong>No orders match your criteria.</strong>
              </EmptyDataTemplate>
            </asp:GridView>    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp;<asp:Button ID="btnback" runat="server" Text="Back" OnClick="btnback_Click" /></td>
        </tr>
      </table>
    </div>
    </form>
</body>
</html>
