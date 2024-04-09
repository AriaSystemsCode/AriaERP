using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Aria.Data.BusinessObject;
using System.Web.UI.MobileControls;
using System.Collections.Generic;

public class ProfilesSummaryController
{
    private DataTable datatable;
    public DataTable profilesNamesDataTable;

    public List<string> codes = new List<string>();
    public ProfilesSummaryController()
    {    
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

        string pageMode = System.Web.HttpContext.Current.Session["PageMode"].ToString();

        profilesNamesDataTable = adapter.GetProfileCodes("", "", "99", "SO", "ST"); //
        for (int i = 0; i < profilesNamesDataTable.Rows.Count; i++)
        {
            codes.Add(profilesNamesDataTable.Rows[i]["Code"].ToString());
        }
    }

    public ProfilesSummaryController(DataTable table) : this()
    {
        datatable = table;
    }

    public int getNumberOfProfiles()
    {
        return codes.Count;
    }

    public DataTable select()
    {
        return datatable;
    }

    public void insert(int lineNumber, string[] values)
    {
        for(int i = 0; i < getNumberOfProfiles(); i++)
        {
            DataRow row = datatable.NewRow();
            row["Type"] = "SO";            
            row["code"] = codes[i];
            row["value"] = values[i];
            row["ObjectKey"] = lineNumber.ToString().PadLeft(7 + 6);
            datatable.Rows.Add(row);
        }
    }

    public void delete(int lineNumber)
    {
        DataRow[] rows = datatable.Select("SUBSTRING(ObjectKey, 8, 6) = '" + lineNumber.ToString().PadLeft(6) + "'");
        for(int i = 0; i < rows.Length; i++)
        {
            rows[i].Delete();
        }
    }

    public void update(int lineNumber, string[] values)
    {
        for (int i = 0; i < getNumberOfProfiles(); i++)
        {
            DataRow[] rows = datatable.Select("SUBSTRING(ObjectKey, 8, 6) = '" + lineNumber.ToString().PadLeft(6) + "' AND code = '" + codes[i] + "'");
            if (rows.Length > 0)
            {
                rows[0]["value"] = values[i];
            }
            else
            {
                DataRow row = datatable.NewRow();
                row["Type"] = "SO";
                row["code"] = codes[i];
                row["value"] = values[i];
                row["ObjectKey"] = lineNumber.ToString().PadLeft(7 + 6);
                datatable.Rows.Add(row);
            }
        }
    }

    public string[] GetValues(int lineNumber)
    {
        string[] result = new string[getNumberOfProfiles()];

        for (int i = 0; i < getNumberOfProfiles(); i++)
        {
            DataRow[] rows = datatable.Select("SUBSTRING(ObjectKey, 8, 6) = '" + lineNumber.ToString().PadLeft(6) + "' AND code = '" + codes[i] + "'");
            if (rows.Length > 0)
            {
                result[i] = rows[0]["value"].ToString();
            }
            else
            {
                result[i] = "";
            }
        }

        return result;
    }

    public string[] GetDefaultValues(string style)
    {
        string[] result = new string[codes.Count];
        for (int i = 0; i < codes.Count; i++)
        {
            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable profiles = businessObject.GetProfileValues("", "", "99", "ST", style.PadRight(130), codes[i].Trim());

            if (profiles.Rows.Count > 0)
            {
                result[i] = profiles.Rows[0]["value"].ToString();
            }
            else
            {
                result[i] = "";
            }
        }

        return result;
    }
}
