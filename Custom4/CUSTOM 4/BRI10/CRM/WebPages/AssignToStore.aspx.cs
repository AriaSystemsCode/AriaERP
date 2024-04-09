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

public partial class WebPages_AssignToStore : System.Web.UI.Page
{
    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        Response.Cache.SetNoServerCaching();
        Response.Cache.SetCacheability(System.Web.HttpCacheability.NoCache);
        Response.Cache.SetNoStore();
        Response.Cache.SetExpires(new DateTime(1900, 01, 01, 00, 00, 00, 00));

        InitAssignToStorePanel();
    }

    private void InitAssignToStorePanel()
    {
        string clientIDs = "";
        string stores = "";
        string checkBoxs = "";


        Table table = Table1;
        DataTable datatable = this.GetStoresDataTable();
        storeRows = this.GenerateStoreRows(datatable);
        for (int i = 0; i < storeRows.Length; i++)
        {
            table.Rows.Add(storeRows[i].GetRow());
            storeRows[i].RegisterJavaScript();

            if (i > 0)
            {
                clientIDs += ", ";
                stores += ", ";
                checkBoxs += ", ";
            }

            clientIDs += storeRows[i].TxtStoreId.ClientID;

            stores += storeRows[i].TxtStoreId.Text;

            checkBoxs += storeRows[i].checkBox.ClientID;
        }

        HiddenField2.Value = clientIDs;
        HiddenField3.Value = stores;
        HiddenField4.Value = checkBoxs;
    }


    private StoreRow[] storeRows;
    private StoreRow[] GenerateStoreRows(DataTable table)
    {
        int count = table.Rows.Count;
        storeRows = new StoreRow[count];
        for (int i = 0; i < count; i++)
        {
            storeRows[i] = new StoreRow();
            storeRows[i].SetStoreValue(table.Rows[i]["Store"].ToString());
        }
        return storeRows;
    }

    private DataTable GetStoresDataTable()
    {
        AriaBusinessObjectAdapter object1 = new AriaBusinessObjectAdapter();

        Aria.DataTypes.AriaConditionList pointer = new Aria.DataTypes.AriaConditionList();
        Aria.DataTypes.AriaCondition condition = new Aria.DataTypes.AriaCondition();
        condition.LeftHandSide = new Aria.DataTypes.AriaStandardDataType();
        condition.LeftHandSide.AddPropertyDataPathEntry("Value", "Account");
        condition.RightHandSide = new Aria.DataTypes.AriaStandardDataType();
        ((Aria.DataTypes.AriaStandardDataType)condition.RightHandSide).Value = "";
        pointer.Items.Add(condition);

        Aria.DataTypes.AriaCondition condition1 = new Aria.DataTypes.AriaCondition();
        condition1.LeftHandSide = new Aria.DataTypes.AriaStandardDataType();
        condition1.LeftHandSide.AddPropertyDataPathEntry("Value", "Account");
        condition1.RightHandSide = new Aria.DataTypes.AriaStandardDataType();
        ((Aria.DataTypes.AriaStandardDataType)condition1.RightHandSide).Value = Session["AccountID"].ToString();
        pointer.Items.Add(condition1);

        DataTable storesDT = object1.GetBusinessObjects("Aria", "", "99", "CRM.Customer.Store", pointer);

        return storesDT;
    }

    protected void OkButton_Click(object sender, EventArgs e)
    {
        try
        {

            if (Session["AssignToStore"] != null)
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Please wait!')", true);
                return;
            }

            ArrayList selectedLines = GetSelectedLinesFromTemplate(HiddenField1.Value);
            if (selectedLines != null)
            {
                TemplateDataTableController templateController = ((OrderDataTableController)Session["Order"]).templateLines;
                ProfilesTemplateController profilesTemplateController = ((OrderDataTableController)Session["Order"]).templateProfiles;
                DesignDataTableController templateDesignDataTableController = ((OrderDataTableController)Session["Order"]).tempDesigns;

                for (int i = 0; i < storeRows.Length; i++)
                {
                    LinesDataTableController linesController = ((OrderDataTableController)Session["Order"]).lines;
                    int lastLine = linesController.select().Rows.Count;


                    if (!storeRows[i].IsDisabled())
                    {
                        string StoreID = storeRows[i].GetStoreValue();
                        string PO = storeRows[i].GetPOValue();
                        int Qty = Int32.Parse(storeRows[i].GetQtyValue());
                        templateController.AssignToStores(selectedLines, StoreID, PO, Qty);
                    }
                }
            }

            ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "window.close();", true);
        }
        catch (Exception ex)
        {
            Session["AssignToStore"] = null;
            string errorMessage = "Error: " + ex.Message;
            if (ex.InnerException != null)
            {
                errorMessage += "\n\r Error Detail:" + ex.InnerException.Message;
            }
            errorMessage = errorMessage.Replace("'", "");

            ScriptManager.RegisterClientScriptBlock(this, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);
        }
    }


    private ArrayList GetSelectedLinesFromTemplate(string value)
    {
        ArrayList arrayList = null;
        if (value.Length > 0)
        {
            arrayList = new ArrayList();
            string[] splited = value.Split(",".ToCharArray());
            for (int i = 0; i < splited.Length; i++)
            {
                arrayList.Add(Int32.Parse(splited[i]));
            }
        }
        return arrayList;
    }
}
