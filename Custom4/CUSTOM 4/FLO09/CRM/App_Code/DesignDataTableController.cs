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
using System.Collections;

/// <summary>
/// Summary description for DesignDataTableController
/// </summary>
public class DesignDataTableController
{
    public DataTable designsTable;
    public bool descriptionLoaded = false;


    public DesignDataTableController()
    {
        AriaBusinessObjectAdapter adapter1 = new AriaBusinessObjectAdapter();
        designsTable = adapter1.GetNewBusinessObject("", "", "99", "CRM.OrderDesign");
        designsTable.Columns.Add("ArtworDesignCategoryDescription", typeof(string));
        designsTable.Columns.Add("ArtworkDesignTypeDescription", typeof(string));
        designsTable.Columns.Add("StylePositionDescription", typeof(string));
    }

    public DesignDataTableController(DataTable table)
    {
        designsTable = table;
        designsTable.Columns.Add("ArtworDesignCategoryDescription", typeof(string));
        designsTable.Columns.Add("ArtworkDesignTypeDescription", typeof(string));
        designsTable.Columns.Add("StylePositionDescription", typeof(string));
    }

    public void LoadDescription()
    {
        descriptionLoaded = true;

        AriaBusinessObjectAdapter adapter1 = new AriaBusinessObjectAdapter();

        for (int index = 0; index < designsTable.Rows.Count; index++)
        {
            if (designsTable.Rows[index].RowState != DataRowState.Deleted)
            {
                designsTable.Rows[index]["ArtworDesignCategoryDescription"] = adapter1.GetCodeDescription("", "", "99", "CDSGNCTGRY", designsTable.Rows[index]["ArtworDesignCategory"].ToString());
                designsTable.Rows[index]["ArtworkDesignTypeDescription"] = adapter1.GetCodeDescription("", "", "99", "CDSGNTYPE", designsTable.Rows[index]["ArtworkDesignType"].ToString()); ;
                designsTable.Rows[index]["StylePositionDescription"] = adapter1.GetCodeDescription("", "", "99", "CSTYLEPOS", designsTable.Rows[index]["StylePosition"].ToString()); ;
            }
        }
    }

    public void Insert(DataRow designRow, int lineNumber)
    {
        int designIndex = designsTable.Rows.Count;

        DataRow row = designsTable.NewRow();

        for (int j = 0; j < designsTable.Columns.Count; j++)
        {
            if (designRow.RowState == DataRowState.Deleted)
            {
                row[j] = designRow[j, DataRowVersion.Original];
            }
            else
            {
                row[j] = designRow[j];
            }
        }

        row["SalesOrderLineNo"] = lineNumber.ToString().PadLeft(6);
        designsTable.Rows.Add(row);
    }

    public void Insert(DataRow[] rows, int lineNumber)
    {
        for (int i = 0; i < rows.Length; i++)
        {
            DataRow row = designsTable.NewRow();

            for (int j = 0; j < rows[i].Table.Columns.Count; j++)
            {
                if (rows[i].RowState == DataRowState.Deleted)
                {
                    row[j] = rows[i][j, DataRowVersion.Original];
                }
                else
                {
                    row[j] = rows[i][j];
                }
            }

            Insert(row, lineNumber);
        }
    }

    public void Update(DataRow[] rows, int lineNumber)
    {
        DataRow[] currentRows = designsTable.Select("SalesOrderLineNo = '" + lineNumber.ToString().PadLeft(6) + "'");

        for (int i = 0; i < rows.Length; i++)
        {

            if (i >= currentRows.Length)
            {
                Insert(rows[i], lineNumber);
            }
            else
            {
                DataRow row = currentRows[i];
                for (int j = 0; j < rows[i].Table.Columns.Count; j++)
                {
                    if (rows[i].RowState == DataRowState.Deleted)
                    {
                        row[j] = rows[i][j, DataRowVersion.Original];
                    }
                    else
                    {
                        row[j] = rows[i][j];
                    }
                }
            }
        }
    }

    public void RemoveDesignsForLine(int lineNumber)
    {
        DataRow[] rows = designsTable.Select("SalesOrderLineNo = '" + lineNumber.ToString().PadLeft(6) + "'");
        for (int i = 0; i < rows.Length; i++)
        {
            rows[i].Delete();
        }
    }



    public DataRow[] GetDesignsForLine(int lineNumber)
    {
        return designsTable.Select("SalesOrderLineNo = '" + lineNumber.ToString().PadLeft(6) + "'");
    }
}
