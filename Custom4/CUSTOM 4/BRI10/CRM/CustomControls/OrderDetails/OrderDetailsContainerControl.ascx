<%@ Control Language="C#" AutoEventWireup="true" CodeFile="OrderDetailsContainerControl.ascx.cs" Inherits="CustomControls_OrderDetails_OrderDetailsContainerControl" %>
<%@ Register Src="~/CustomControls/OrderDetails/StylesControl.ascx" TagName="StylesControl" TagPrefix="uc7" %>
<%@ Register Src="~/CustomControls/OrderDetails/ButtonsControl.ascx" TagName="ButtonsControl" TagPrefix="uc10" %>
<%@ Register Src="~/CustomControls/OrderDetails/ProfilesControl.ascx" TagName="ProfilesControl" TagPrefix="uc9" %>
<%@ Register Src="~/CustomControls/OrderDetails/DescriptionControl.ascx" TagName="DescriptionControl" TagPrefix="uc8" %>
<table style="width: 789px">
                        <tr>
                            <td>
                                <uc7:StylesControl ID="Styles1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <uc8:DescriptionControl ID="Description1" runat="server" />
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <table style="border: solid 1px gray">
                                    <tr>
                                        <td>
                                            <div class="profile" id="ProDiv" runat="server">
                                                <uc9:ProfilesControl ID="Profiles1" runat="server" />
                                            </div>
                                        </td>
                                    </tr>
                                </table>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <table>
                                    <tr>
                                        <td>
                                            <uc10:ButtonsControl ID="Buttons1" runat="server" />
                                        </td>
                                    </tr>
                                </table>
                            </td>
                        </tr>
                    </table>