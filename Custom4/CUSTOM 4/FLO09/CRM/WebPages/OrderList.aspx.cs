using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Aria.Data;
using Aria.Data.BusinessObject;
using Aria.Environment;
using Aria.DataTypes;

public partial class OrderList : System.Web.UI.Page
{
    private string ConvertDateToFox(DateTime date)
    {
        return "{^" +
                date.Year.ToString() + "-" +
                date.Month.ToString() + "-" +
                date.Day.ToString() +
                "}";
    }

    private string GetFilter()
    {
        string filter = " ordhdr.Status = 'B' AND (ordhdr.cordtype + ordhdr.order = 'T' OR ordhdr.cordtype + ordhdr.order = 'O')";

        if (Session["OrderSearchAccountNo"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.account+ordhdr.cordtype+ordhdr.order = '" + Session["OrderSearchAccountNo"].ToString().Trim() + "'";
        }

        if (Session["OrderSearchStoreType"].ToString() == "Multi")
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.multi = 'Y'";
        }

        if (Session["OrderSearchStoreType"].ToString() == "Single")
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.store = '" + Session["OrderSearchStoreID"].ToString().Trim() + "'";
        }

        if (Session["OrderSearchOrderFrom"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "(ordhdr.cordtype + ordhdr.order >= 'O" + Session["OrderSearchOrderFrom"].ToString().Trim() + "') OR " +
                      "(ordhdr.cordtype + ordhdr.order >= 'T" + Session["OrderSearchOrderFrom"].ToString().Trim() + "')";
        }

        if (Session["OrderSearchOrderTo"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "(ordhdr.cordtype + ordhdr.order <= 'O" + Session["OrderSearchOrderTo"].ToString().Trim() + "%') OR " +
                      "(ordhdr.cordtype + ordhdr.order <= 'T" + Session["OrderSearchOrderTo"].ToString().Trim() + "%')";
        }

        if (Session["OrderSearchEnterDateFrom"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.entered >= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchEnterDateFrom"].ToString()));
        }


        if (Session["OrderSearchEnterDateTo"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.entered <= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchEnterDateTo"].ToString()));
        }

        if (Session["OrderSearchStartShipDateFrom"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.start >= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchStartShipDateFrom"].ToString()));
        }

        if (Session["OrderSearchStartShipDateTo"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.start <= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchStartShipDateTo"].ToString()));
        }

        if (Session["OrderSearchExpectedShipDateFrom"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.complete >= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchExpectedShipDateFrom"].ToString()));
        }

        if (Session["OrderSearchExpectedShipDateTo"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "ordhdr.complete <= " + ConvertDateToFox(DateTime.Parse(Session["OrderSearchExpectedShipDateTo"].ToString()));
        }

        if (Session["OrderSearchSeason"].ToString().Trim().Length > 0)
        {
            if (filter.Trim().Length > 0) filter += " AND ";
            filter += "season = '" + Session["OrderSearchSeason"].ToString() + "'";
        }

        return filter;
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["OrderSearchAccountNo"] == null)
        {
            this.Response.Redirect("OrderSearch.aspx");
        }
        else
        {
            if (Page.Session["OrderList"] == null || Page.Session["OrderList"] == "")
            {
                AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data);
                command.CommandText = "SELECT ORDHDR.order, CUSTOMER.btname as customer, IIF(EMPTY(CUSTOMER.Store), IIF(ORDHDR.multi = 'Y', 'Multi -', 'Main - ') + CUSTOMER.Stname, 'Store - ' + CUSTOMER.Stname) as Store, ORDHDR.entered as EnterDate, ORDHDR.Book as TotBooked, ORDHDR.Ship as TotShipped, ORDHDR.Bookamt as Amount FROM ORDHDR, CUSTOMER WHERE ORDHDR.Account + ORDHDR.Store == CUSTOMER.Account + CUSTOMER.Store ";

                string filter = GetFilter();
                if (filter.Trim().Length > 0)
                {
                    command.CommandText += " AND " + filter + " ORDER BY Order";
                }

                AriaDataProvider provider = new AriaDataProvider();

                Page.Session["OrderList"] = provider.GetDataTable(command);
                Orders.DataSource = Page.Session["OrderList"];

                Orders.PageSize = 10;

                Orders.DataBind();
            }
        }
    }

    protected void Orders_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        Orders.DataSource = Page.Session["OrderList"];
        Orders.PageSize = 10;
        Orders.PageIndex = e.NewPageIndex;
        Orders.DataBind();
    }

    protected void Orders_RowCommand(object sender, GridViewCommandEventArgs e)
    {
        if (e.CommandSource is LinkButton)
        {
            int rowNo = Convert.ToInt32(((LinkButton)e.CommandSource).ClientID.Replace("Orders_ctl", "").Replace("_btnCopy", "").Replace("_btnModify", "")) - 2 + Orders.PageSize * Orders.PageIndex ;

            Session.Add("ActiveCustomerGetAddressInfo", "Yes");
            Session.Add("ActiveCustomerGetContactInfo", "Yes");
            Session.Add("ActiveCustomerGetGeneralInfo", "Yes");
            Session.Add("ActiveCustomerGetNoteInfo", "Yes");
            Session.Add("ActiveCustomerGetOrderInfo", "Yes");
            Session.Add("ActiveCustomerGetSalesRepresentativeInfo", "Yes");

            Session["TemplateState"] = null;
            Session["TemplateLastLineNumber"] = 0;
            Session["LineState"] = "New";
            Session["sequence"] = null;

            Session["Saving"] = null;
            Session["AddingLine"] = null;

            if (((LinkButton)e.CommandSource).Text == "Copy")
            {
                Session["PageMode"] = "Copy";
            }
            else
            {
                Session["PageMode"] = "Modify";
            }

            Session["Order"] = new OrderDataTableController();
            ((OrderDataTableController)Session["Order"]).Load(((DataTable)Session["OrderList"]).Rows[rowNo]["Order"].ToString());

            OrderDataTableController order = ((OrderDataTableController)Session["Order"]);

            Session["AccountID"] = order.header.Rows[0]["Customercode"];

            AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
            AriaDataObjectPointer pointer = new AriaDataObjectPointer();
            pointer.AddKeyField("Account", order.header.Rows[0]["Customercode"].ToString().PadRight(5));
            DataTable result = null;

            if (order.header.Rows[0]["Multi"].ToString() == "Y")
            {
                result = provider.GetBusinessObject("", "", "99", "CRM.Customer", pointer);
                Session["ActiveCustomer"] = result.Rows[0];

                Session.Add("StoreID", "Multi");
                Session.Add("StoreName", "Multi");
            }
            else if (order.header.Rows[0]["StoreNumber"].ToString().Trim().Length == 0)
            {
                result = provider.GetBusinessObject("", "", "99", "CRM.Customer", pointer);
                Session["ActiveCustomer"] = result.Rows[0];

                Session.Add("StoreID", "Main");
                Session.Add("StoreName", "Main");
            }
            else
            {
                pointer.AddKeyField("Store", order.header.Rows[0]["StoreNumber"].ToString().PadRight(8));
                result = provider.GetBusinessObject("", "", "99", "CRM.Store", pointer);
                Session["ActiveCustomer"] = result.Rows[0];

                Session.Add("StoreID", order.header.Rows[0]["StoreNumber"].ToString().Trim());
                Session.Add("StoreName", ((DataRow)Session["ActiveCustomer"])["BillToName"]);
            }

            this.Page.Response.Redirect("Order.aspx");
        }
    }
    protected void btnback_Click(object sender, EventArgs e)
    {
        this.Page.Response.Redirect("OrderSearch.aspx");
         
    }
}
