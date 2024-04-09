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

public partial class NoteControl : System.Web.UI.UserControl
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["ActiveCustomerGetNoteInfo"].ToString() == "Yes")
        {
            if (Session["PageMode"].ToString().Equals("Add"))
            {
                Notes1.Text = ((DataRow)Session["ActiveCustomer"])["Note"].ToString().TrimEnd();
            }

            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetNoteInfo"] = "No";
        }
    }

    public Hashtable getValues()
    {
        Hashtable hashtable = new Hashtable();
        hashtable.Add("Notes1", Notes1.Text);
        hashtable.Add("Notes2", Notes2.Text);
        return hashtable;
    }

    private void LoadFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        this.Notes1.Text = datatable.Rows[0]["Notes1"].ToString().TrimEnd();
        this.Notes2.Text = datatable.Rows[0]["Notes2"].ToString().TrimEnd();
    }

    public void SaveFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];
        r["Notes1"] = this.Notes1.Text;
        r["Notes2"] = this.Notes2.Text;
    }
}
