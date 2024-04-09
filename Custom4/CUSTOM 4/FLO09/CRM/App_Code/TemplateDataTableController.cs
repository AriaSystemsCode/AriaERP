using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Aria.DataTypes;
using Aria.Data.BusinessObject;
using System.Collections;

/// <summary>
/// Summary description for TemplateDataTableController
/// </summary>
public class TemplateDataTableController
{
    private DataTable datatable;

    public TemplateDataTableController()
    {
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();
        datatable = adapter.GetNewBusinessObject("", "", "99", "CRM.OrderLine");
        datatable.Columns.Add("Check", typeof(bool));
        datatable.Columns.Add("amount", typeof(double));
        datatable.Columns.Add("PiecesPerCase", typeof(int));
        datatable.Columns.Add("NumberOfCases", typeof(int));
        datatable.Columns["Check"].ReadOnly = false;
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
        ((OrderDataTableController)HttpContext.Current.Session["Order"]).templateProfiles.delete(Convert.ToInt32(datatable.Rows[index]["LineNumber"]));
        ((OrderDataTableController)HttpContext.Current.Session["Order"]).templateDesigns.RemoveDesignsForLine(Convert.ToInt32(datatable.Rows[index]["LineNumber"]));
        datatable.Rows[index].Delete();
    }

    public void insert(string style, string colorDescription, string styleDescription,  int qty1, int qty2, int qty3, int qty4, int qty5, int qty6, int qty7, int qty8,
                        int openQty1, int openQty2, int openQty3, int openQty4, int openQty5, int openQty6, int openQty7, int openQty8,
                            double grossPrice, double discount, DateTime shipDate, string group, string description)
    {
        style = style.Trim();

        DataRow r = datatable.NewRow();

        r["Style"] = style;

        r["LineNumber"] = Convert.ToInt32(HttpContext.Current.Session["TemplateLastLineNumber"]) + 1;
        HttpContext.Current.Session["TemplateLastLineNumber"] = r["LineNumber"];

        r["StyleDescription"] = styleDescription;
        r["ColorDescription"] = colorDescription;

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

        datatable.Rows.Add(r);
    }
    public void Update(int lineNo, string style, string colorDescription,string styleDescription , int qty1, int qty2, int qty3, int qty4, int qty5, int qty6, int qty7, int qty8,
                       int openQty1, int openQty2, int openQty3, int openQty4, int openQty5, int openQty6, int openQty7, int openQty8,
                           double grossPrice, double discount, DateTime shipDate, string group, string description)
    {
        style = style.Trim();

        DataRow r = datatable.Select("LineNumber = " + lineNo.ToString())[0];

        r["Style"] = style;

        r["StyleDescription"] = styleDescription;
        r["ColorDescription"] = colorDescription;

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
    }

    public void AssignToStores(ArrayList lineIndeces, string store, string PO, int qty)
    {
        LinesDataTableController linesController = ((OrderDataTableController)System.Web.HttpContext.Current.Session["Order"]).lines;
        
        for (int i = 0; i < lineIndeces.Count; i++)
        {
            DataRow r = datatable.Rows[(int)lineIndeces[i]];
            if (r.RowState != DataRowState.Deleted)
            {
                string style = r["Style"].ToString();
                string styleDescription = r["StyleDescription"].ToString();
                string description = r["Description1"].ToString();
                string colorDescription = r["ColorDescription"].ToString();
                int qty1 = qty * ConvertToInt(r["BookedQuantity1"].ToString());
                int qty2 = qty * ConvertToInt(r["BookedQuantity2"].ToString());
                int qty3 = qty * ConvertToInt(r["BookedQuantity3"].ToString());
                int qty4 = qty * ConvertToInt(r["BookedQuantity4"].ToString());
                int qty5 = qty * ConvertToInt(r["BookedQuantity5"].ToString());
                int qty6 = qty * ConvertToInt(r["BookedQuantity6"].ToString());
                int qty7 = qty * ConvertToInt(r["BookedQuantity7"].ToString());
                int qty8 = qty * ConvertToInt(r["BookedQuantity8"].ToString());
                int openQty1 = qty * ConvertToInt(r["QuantityforSize1"].ToString());
                int openQty2 = qty * ConvertToInt(r["QuantityforSize2"].ToString());
                int openQty3 = qty * ConvertToInt(r["QuantityforSize3"].ToString());
                int openQty4 = qty * ConvertToInt(r["QuantityforSize4"].ToString());
                int openQty5 = qty * ConvertToInt(r["QuantityforSize5"].ToString());
                int openQty6 = qty * ConvertToInt(r["QuantityforSize6"].ToString());
                int openQty7 = qty * ConvertToInt(r["QuantityforSize7"].ToString());
                int openQty8 = qty * ConvertToInt(r["QuantityforSize8"].ToString());
                double grossPrice = ConvertToDouble(r["GrossPrice"].ToString());
                double discount = ConvertToDouble(r["DiscountPercent"].ToString());
                DateTime shipDate = (DateTime)r["StartDate"];
                string group = r["Group"].ToString();
                string storeID = store;
                string customerPO = PO;
                linesController.insert(style, qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, openQty1, openQty2, openQty3, openQty4, openQty5, openQty6, openQty7, openQty8, grossPrice, discount, shipDate, group, storeID, customerPO, description);

                ProfilesTemplateController profilesTemplateController = ((OrderDataTableController)System.Web.HttpContext.Current.Session["Order"]).templateProfiles;
                ProfilesSummaryController profilesSummaryController = ((OrderDataTableController)System.Web.HttpContext.Current.Session["Order"]).linesProfiles;
                profilesSummaryController.insert(Convert.ToInt32(HttpContext.Current.Session["LastLineNumber"]), profilesTemplateController.GetValues(Convert.ToInt32(r["LineNumber"])));

                linesController.CalculatePackingInfo(Convert.ToInt32(HttpContext.Current.Session["LastLineNumber"]));

                DesignDataTableController designTemplateController = ((OrderDataTableController)System.Web.HttpContext.Current.Session["Order"]).templateDesigns;
                DesignDataTableController designSummaryController = ((OrderDataTableController)System.Web.HttpContext.Current.Session["Order"]).linesDesigns;
                designSummaryController.Insert(designTemplateController.GetDesignsForLine(Convert.ToInt32(r["LineNumber"])), Convert.ToInt32(HttpContext.Current.Session["LastLineNumber"]));
            }
        }
    }

    private int ConvertToInt(string value)
    {
        try
        {
            return Int32.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
        }
    }

    private double ConvertToDouble(string value)
    {
        try
        {
            return double.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
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

    public int GetNotDeletedRowNo(int rowIndex)
    {
        int notDeleteCount = -1;
        for (int index = 0; index < datatable.Rows.Count; index++)
        {
            if (datatable.Rows[index].RowState != DataRowState.Deleted)
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
