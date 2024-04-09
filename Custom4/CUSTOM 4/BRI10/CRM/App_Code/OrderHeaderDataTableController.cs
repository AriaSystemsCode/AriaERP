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
using Aria.Data;
using Aria.Environment;

/// <summary>
/// Summary description for OrderHeaderDataTableController
/// </summary>
public class OrderDataTableController
{
    public DataTable header;

    public LinesDataTableController lines ;
    public ProfilesSummaryController linesProfiles ;
    public DesignDataTableController linesDesigns;
    public TemplateDataTableController templateLines;
    public ProfilesTemplateController templateProfiles;
    public DesignDataTableController templateDesigns;
    public DesignDataTableController tempDesigns;

    private bool _useExtenedScale;

    public bool UseExtenedScale
    {
        get { return _useExtenedScale; }
        set { _useExtenedScale = value; }
    }

    public OrderDataTableController()
    {

        AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data);
        command.CommandText = "SELECT cwarecode FROM warehous WHERE ldefware";

        AriaDataProvider provider = new AriaDataProvider();
        DataTable wareHouses = provider.GetDataTable(command);
        if (wareHouses.Rows.Count == 0)
        {
            command.CommandText = "SELECT cwarecode FROM warehous";
            wareHouses = provider.GetDataTable(command);

            if (wareHouses.Rows.Count == 0)
            {
                HttpContext.Current.Session["LocationCode"] = null;
            }
            else
            {
                HttpContext.Current.Session["LocationCode"] = wareHouses.Rows[0]["cwarecode"].ToString();
            }
        }
        else
        {
            HttpContext.Current.Session["LocationCode"] = wareHouses.Rows[0]["cwarecode"].ToString();
        }
        
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();
        HttpContext.Current.Session["UseExtenedScale"] = adapter.GetSetup("", "", "99", "IC", "M_USEEXSSC").Rows[0]["DefaultData"].ToString().TrimEnd() == ".T.";
    }

    public DataTable GetOrderHeaderData()
    {
        return header;
    }

    public void Add()
    {
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();
        header = adapter.GetBusinessObjects("Aria", "", "99", "CRM.OrderHeader", ".F.", "");
        header.Rows.Add(header.NewRow());

        HttpContext.Current.Session["LastLineNumber"] = 0;

        lines = new LinesDataTableController();
        linesProfiles = new ProfilesSummaryController(adapter.GetProfileNewValues("", "", "99"));
        linesDesigns = new DesignDataTableController();
        templateLines = new TemplateDataTableController();
        templateProfiles = new ProfilesTemplateController();
        templateDesigns = new DesignDataTableController();

        tempDesigns = new DesignDataTableController();
    }

    public void Load(string orderNo)
    {
        AriaBusinessObjectAdapter adaper = new AriaBusinessObjectAdapter();
        header = adaper.GetBusinessObjects("Aria", "", "99", "CRM.OrderHeader", "cordtype + order = 'O" + orderNo + "'", "");

        HttpContext.Current.Session["LastLineNumber"] = header.Rows[0]["LastLineNumber"];

        linesProfiles = new ProfilesSummaryController(adaper.GetProfileValues("Aria", "", "99", "CPRO_TYPE+CKEY+CPRO_CODE = '" + "SOT" + orderNo + "'"));
        lines = new LinesDataTableController(adaper.GetBusinessObjects("Aria", "", "99", "CRM.OrderLine", "CORDTYPE+ORDER+STR(LINENO,6) = 'T" + orderNo + "' OR CORDTYPE+ORDER+STR(LINENO,6) = 'O" + orderNo + "'", "lineNo"));
        linesDesigns = new DesignDataTableController(adaper.GetBusinessObjects("Aria", "", "99", "CRM.OrderDesign", "ORDER+CORDLINE+STR(LINENO,6) = '" + orderNo + "'", "cordline, lineNo"));

        templateLines = new TemplateDataTableController();
        templateProfiles = new ProfilesTemplateController();
        templateDesigns = new DesignDataTableController();

        tempDesigns = new DesignDataTableController();
    }
}
