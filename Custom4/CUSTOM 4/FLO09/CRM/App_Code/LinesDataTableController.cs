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
using Aria.DataTypes;
using System.Collections.Generic;

/// <summary>
/// Summary description for LinesDataTableController
/// </summary>
public class LinesDataTableController
{
    private DataTable datatable;
    private bool _useExtenedScale;

    public LinesDataTableController()
    {
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();
        datatable = adapter.GetNewBusinessObject("", "", "99", "CRM.OrderLine");
        datatable.Columns.Add("Check", typeof(bool));
        datatable.Columns.Add("amount", typeof(double));
        datatable.Columns.Add("PiecesPerCase", typeof(int));
        datatable.Columns.Add("NumberOfCases", typeof(int));
        datatable.Columns["Check"].ReadOnly = false;

        _useExtenedScale = (bool)HttpContext.Current.Session["UseExtenedScale"];
    }

    public LinesDataTableController(DataTable table)
    {
        datatable = table;
        table.Columns.Add("Check", typeof(bool));
        table.Columns.Add("amount", typeof(double));
        datatable.Columns.Add("PiecesPerCase", typeof(int));
        datatable.Columns.Add("NumberOfCases", typeof(int));
        table.Columns["Check"].ReadOnly = false;

        for (int i = 0; i < table.Rows.Count; i++)
        {
            DataRow r = table.Rows[i];
            r["Amount"] = Convert.ToInt32(r["TotalBookedquantity"]) * Convert.ToDouble(r["GrossPrice"]) * (1 - Convert.ToDouble(r["DiscountPercent"]) / 100);
            if ((bool)HttpContext.Current.Session["UseExtenedScale"])
            {
                r["ColorDescription"] = r["Style"].ToString().Substring(9, 6).Trim();
            }
            else
            {
                r["ColorDescription"] = r["Style"].ToString().Substring(13).Trim();
            }
            CalculatePackingInfo(Convert.ToInt32(r["LineNumber"]));
        }
    }

    public DataTable select()
    {
        return datatable;
    }

    public DataRow getLineDataRow(int index)
    {
        return datatable.Rows[index];
    }

    public void delete(int index)
    {
        ((OrderDataTableController)HttpContext.Current.Session["Order"]).linesProfiles.delete(Convert.ToInt32(datatable.Rows[index]["LineNumber"]));
        ((OrderDataTableController)HttpContext.Current.Session["Order"]).linesDesigns.RemoveDesignsForLine(Convert.ToInt32(datatable.Rows[index]["LineNumber"]));
        datatable.Rows[index].Delete();
    }

    private Dictionary<string, DataTable> styleCache = new Dictionary<string, DataTable>();

    public void insert(string style, int qty1, int qty2, int qty3, int qty4, int qty5, int qty6, int qty7, int qty8,
                        int openQty1, int openQty2, int openQty3, int openQty4, int openQty5, int openQty6, int openQty7, int openQty8,
                            double grossPrice, double discount, DateTime shipDate, string group, string storeID, string customerPO, string description)
    {
        style = style.Trim();

        DataRow r = datatable.NewRow();

        r["LineNumber"] = Convert.ToInt32(HttpContext.Current.Session["LastLineNumber"]) + 1;
        HttpContext.Current.Session["LastLineNumber"] = r["LineNumber"];

        r["Style"] = style;

        AriaDataObjectPointer pointer = new AriaDataObjectPointer();
        pointer.AddKeyField("Style", style);

        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

        DataTable styleTable;

        if (styleCache.ContainsKey(style.TrimEnd()))
        {
            styleTable = styleCache[style.TrimEnd()];
        }
        else
        {
            styleTable = adapter.GetBusinessObject("", "", "99", "CRM.Style", pointer);
            styleCache.Add(style.TrimEnd(), styleTable);
        }

        r["StyleDescription"] = styleTable.Rows[0]["Description"];
        r["StyleGroups"] = styleTable.Rows[0]["StyleGroups"];

        string color = "";
        if (_useExtenedScale)
        {
            color = style.Substring(9, 6).Trim();
        }
        else
        {
            color = style.Substring(13).Trim();
        }

        r["ColorDescription"] = color;

        r["Description1"] = description;

        r["BookedQuantity1"] = qty1;
        r["BookedQuantity2"] = qty2;
        r["BookedQuantity3"] = qty3;
        r["BookedQuantity4"] = qty4;
        r["BookedQuantity5"] = qty5;
        r["BookedQuantity6"] = qty6;
        r["BookedQuantity7"] = qty7;
        r["BookedQuantity8"] = qty8;

        r["TotalBookedquantity"] = qty1 + qty2 + qty3 + qty4 + qty5 + qty6 + qty7 + qty8;

        r["QuantityforSize1"] = openQty1;
        r["QuantityforSize2"] = openQty2;
        r["QuantityforSize3"] = openQty3;
        r["QuantityforSize4"] = openQty4;
        r["QuantityforSize5"] = openQty5;
        r["QuantityforSize6"] = openQty6;
        r["QuantityforSize7"] = openQty7;
        r["QuantityforSize8"] = openQty8;

        r["TotalQuantity"] = openQty1 + openQty2 + openQty3 + openQty4 + openQty5 + openQty6 + openQty7 + openQty8;

        r["GrossPrice"] = grossPrice;
        r["price"] = grossPrice * (1 - discount / 100);
        r["Group"] = group;
        r["DiscountPercent"] = discount;
        r["StartDate"] = shipDate.Date;

        r["Amount"] = Convert.ToInt32(r["TotalBookedquantity"]) * grossPrice * (1-discount/100);

        r["CustomerPO"] = customerPO;
        r["StoreNumber"] = storeID;

        datatable.Rows.Add(r);
    }

    public void Update(int lineNo, string style, int qty1, int qty2, int qty3, int qty4, int qty5, int qty6, int qty7, int qty8,
                       int openQty1, int openQty2, int openQty3, int openQty4, int openQty5, int openQty6, int openQty7, int openQty8,
                           double grossPrice, double discount, DateTime shipDate, string group, string storeID, string customerPO, string description)
    {
        style = style.Trim();

        DataRow r = datatable.Select("LineNumber = " + lineNo.ToString())[0];

        r["Style"] = style;

        AriaDataObjectPointer pointer = new AriaDataObjectPointer();
        pointer.AddKeyField("Style", style);

        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

        DataTable styleTable = adapter.GetBusinessObject("", "", "99", "CRM.Style", pointer);
        r["StyleDescription"] = styleTable.Rows[0]["Description"];
        r["StyleGroups"] = styleTable.Rows[0]["StyleGroups"];

        string color = "";
        if (_useExtenedScale)
        {
            color = style.Substring(9, 6).Trim();
        }
        else
        {
            color = style.Substring(13).Trim();
        }

        r["ColorDescription"] = color;

        r["Description1"] = description;

        r["BookedQuantity1"] = qty1;
        r["BookedQuantity2"] = qty2;
        r["BookedQuantity3"] = qty3;
        r["BookedQuantity4"] = qty4;
        r["BookedQuantity5"] = qty5;
        r["BookedQuantity6"] = qty6;
        r["BookedQuantity7"] = qty7;
        r["BookedQuantity8"] = qty8;

        r["TotalBookedquantity"] = qty1 + qty2 + qty3 + qty4 + qty5 + qty6 + qty7 + qty8;

        r["QuantityforSize1"] = openQty1;
        r["QuantityforSize2"] = openQty2;
        r["QuantityforSize3"] = openQty3;
        r["QuantityforSize4"] = openQty4;
        r["QuantityforSize5"] = openQty5;
        r["QuantityforSize6"] = openQty6;
        r["QuantityforSize7"] = openQty7;
        r["QuantityforSize8"] = openQty8;

        r["TotalQuantity"] = openQty1 + openQty2 + openQty3 + openQty4 + openQty5 + openQty6 + openQty7 + openQty8;

        r["GrossPrice"] = grossPrice;
        r["price"] = grossPrice * (1 - discount / 100);
        r["Group"] = group;
        
        r["DiscountPercent"] = discount;
        r["StartDate"] = shipDate.Date;

        

        r["Amount"] = Convert.ToInt32(r["TotalBookedquantity"]) * grossPrice * (1 - discount / 100);
        r["CustomerPO"] = customerPO;
        r["StoreNumber"] = storeID;
    }

    public void UpdatePO(string po)
    {
        for (int i = 0; i < datatable.Rows.Count; i++)
        {
            if (datatable.Rows[i].RowState == DataRowState.Deleted)
            {
                datatable.Rows.InsertAt(datatable.NewRow(), i + 1);
                for (int j = 0; j < datatable.Columns.Count; j++)
                {
                    datatable.Rows[i + 1][j] = datatable.Rows[i][j, DataRowVersion.Original];
                }

                datatable.Rows[i + 1].AcceptChanges();

                datatable.Rows.RemoveAt(i);

                datatable.Rows[i]["CustomerPO"] = po;
                datatable.Rows[i].Delete();

            }
            else
            {
                datatable.Rows[i]["CustomerPO"] = po;
            }
        }
    }

    public void UpdateShipStartDate(DateTime dateTime)
    {
        for (int i = 0; i < datatable.Rows.Count; i++)
        {
            if (datatable.Rows[i].RowState == DataRowState.Deleted)
            {
                datatable.Rows.InsertAt(datatable.NewRow(), i + 1);
                for (int j = 0; j < datatable.Columns.Count; j++)
                {
                    datatable.Rows[i + 1][j] = datatable.Rows[i][j, DataRowVersion.Original];
                }

                datatable.Rows[i + 1].AcceptChanges();

                datatable.Rows.RemoveAt(i);

                datatable.Rows[i]["StartDate"] = dateTime.Date;
                datatable.Rows[i].Delete();
            }
            else
            {
                datatable.Rows[i]["StartDate"] = dateTime.Date;
            }
        }
    }

    public void CalculatePackingInfo(int lineNo)
    {
        DataRow r = datatable.Select("LineNumber = " + lineNo.ToString())[0];
        r["PiecesPerCase"] = 0;
        r["NumberOfCases"] = 0;

        int qty = 0;
        for (int index = 1; index <= 8; index++)
        {
            qty += Convert.ToInt32(r["BookedQuantity" + index.ToString().Trim()]);
        }

        AriaDataObjectPointer pointer = new AriaDataObjectPointer();
        pointer.AddKeyField("Style", r["Style"].ToString());

        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

        DataTable styleTable;

        if (styleCache.ContainsKey(r["Style"].ToString().TrimEnd()))
        {
            styleTable = styleCache[r["Style"].ToString().TrimEnd()];
        }
        else
        {
            styleTable = adapter.GetBusinessObject("", "", "99", "CRM.Style", pointer);
            styleCache.Add(r["Style"].ToString().TrimEnd(), styleTable);
        }


        if (styleTable.Rows.Count > 0 && Convert.ToInt32(styleTable.Rows[0]["QuantityperCarton"]) > 0)
        {
            r["PiecesPerCase"] = Convert.ToInt32(styleTable.Rows[0]["QuantityperCarton"]);
            r["NumberOfCases"] = qty / Convert.ToInt32(r["PiecesPerCase"]);
            if ((qty % Convert.ToInt32(r["PiecesPerCase"])) > 0)
            {
                r["NumberOfCases"] = Convert.ToInt32(r["NumberOfCases"]) + 1;
            }
        }
        else
        {
            DataTable profileCodes = ((OrderDataTableController)HttpContext.Current.Session["Order"]).linesProfiles.profilesNamesDataTable;
            ProfilesSummaryController profile = ((OrderDataTableController)HttpContext.Current.Session["Order"]).linesProfiles;

            for (int index = 0; index < profileCodes.Rows.Count; index++)
            {
                if (profileCodes.Rows[index]["Description"].ToString().Trim() == "Case Pack")
                {
                    string piecesPerCase = profile.GetValues(lineNo)[index];
                    int result = 0;
                    if (piecesPerCase.Length > 3 && int.TryParse(piecesPerCase.Substring(0, 3), out result) && result > 0)
                    {
                        r["PiecesPerCase"] = result;
                        r["NumberOfCases"] = qty / Convert.ToInt32(r["PiecesPerCase"]);

                        if ((qty % Convert.ToInt32(r["PiecesPerCase"])) > 0)
                        {
                            r["NumberOfCases"] = Convert.ToInt32(r["NumberOfCases"]) + 1;
                        }
                    }
                }
            }
        }

        if (Convert.ToInt32(r["PiecesPerCase"]) == 0)
        {
            r["PiecesPerCase"] = DBNull.Value;
            r["NumberOfCases"] = DBNull.Value;
        }
    }

    public int GetNotDeletedRowNo(int rowIndex)
    {
        int notDeleteCount = -1;
        for (int index = 0; index < datatable.Rows.Count; index++)
        {
            if(datatable.Rows[index].RowState != DataRowState.Deleted)
            {
                notDeleteCount += 1;
            }

            if (rowIndex == notDeleteCount)
            {
                return index;
            }
        }

       return 0;
    }
}
