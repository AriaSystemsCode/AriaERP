using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for Stub
/// </summary>
public class Stub
{
    public static DataTable GetTestTable()
    {
        DataTable table = new DataTable();
        table.Columns.Add("StoreID", typeof(string));
        table.Columns.Add("StoreName", typeof(string));
        DataRow r = table.NewRow();
        r["StoreID"] = 10;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 9;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 8;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 7;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 6;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 5;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 4;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 3;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 2;
        table.Rows.Add(r);

        r = table.NewRow();
        r["StoreID"] = 1;
        table.Rows.Add(r);

        return table;
    }
}
