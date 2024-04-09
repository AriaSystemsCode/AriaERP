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
using Aria.Data.BusinessObject;
using Aria.DataTypes;
using Aria.Data;

public partial class Contact_Infromation : System.Web.UI.UserControl
{
    DataTable contactTable;

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        ContactName.Attributes.Add("onchange", "Loading(' Loading...')");
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["ActiveCustomerGetContactInfo"].ToString() == "Yes")
        {
            if (Session["PageMode"].ToString().Equals("Add"))
            {
                if (((DataRow)Session["ActiveCustomer"])["Buyer"].ToString().Trim().Length != 0)
                {
                    ContactName.Text = ((DataRow)Session["ActiveCustomer"])["Buyer"].ToString();
                    ContactName_TextChanged(null, null);
                }
            }

            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetContactInfo"] = "No";
        }
    }

    private void LoadFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        if (datatable.Rows[0]["buyer"].ToString().Trim().Length != 0)
        {
            this.ContactName.Text = datatable.Rows[0]["buyer"].ToString().TrimEnd();
            ContactName_TextChanged(null, null);
        }
        this.Phone.Text = datatable.Rows[0]["phone"].ToString();
    }

    public void SaveFields()
    {
        if (ContactTitle.Enabled && ContactName.Text.TrimEnd().Length > 0)
        {
            string storeID = "";
            if (Session["StoreID"].ToString().TrimEnd() != "Main" && Session["StoreID"].ToString().TrimEnd() != "Multi")
            {
                storeID = Session["StoreID"].ToString();
            }


            AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("", "99"), Aria.Environment.AriaDatabaseTypes.Aria27Data);
            command.CommandText = "INSERT INTO CONTACT (cContType, cCont_Id, Store, Contact, cContTtl, Phone) VALUES " +
                                  "('C', '" + Session["AccountID"].ToString().Replace("'", "' + chr(39) + '") + "', '" +
                                              storeID.Replace("'", "' + chr(39) + '") +
                                              "', '" + 
                                              ContactName.Text.Replace("'", "' + chr(39) + '") + "', '" + 
                                              ContactTitle.Text.Replace("'", "' + chr(39) + '") + "', '" +
                                              Phone.Text.Replace("'", "' + chr(39) + '") + "')";
            command.ExecuteNonQuery();
        }

        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];
        r["buyer"] = this.ContactName.Text;
        r["phone"] = this.Phone.Text;
    }
    
    protected void ContactName_TextChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();", true);

        ContactController contacts = new ContactController();
        contactTable = contacts.GetContacts();

        if (!ContactTitle.Enabled)
        {
            ContactTitle.Text = "";
            Phone.Text = "";
        }

        ContactTitle.Enabled = false;

        for (int index = 0; index < contactTable.Rows.Count; index++)
        {
            if (contactTable.Rows[index]["Contact"].ToString().ToUpper().TrimEnd() == ContactName.Text.ToUpper().TrimEnd())
            {
                ContactTitle.Text = contactTable.Rows[index]["Title"].ToString().TrimEnd();
                Phone.Text = contactTable.Rows[index]["Phone"].ToString().TrimEnd();

                ScriptManager.GetCurrent(this.Page).SetFocus(ContactName.ClientID);
                return;
            }
        }

        ContactTitle.Enabled = true;

        ScriptManager.GetCurrent(this.Page).SetFocus(ContactName.ClientID);
    }
    
    protected void Label1_Click(object sender, EventArgs e)
    {
        ContactName.Text = "*";
        ScriptManager.GetCurrent(this.Page).SetFocus(ContactName.ClientID);
    }
}
