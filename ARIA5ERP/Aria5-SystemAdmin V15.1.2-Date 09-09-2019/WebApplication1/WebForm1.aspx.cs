using Microsoft.TeamFoundation.Client;
using Microsoft.TeamFoundation.VersionControl.Client;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace WebApplication1
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            createfixproject.createnewfix newins = new createfixproject.createnewfix();
            var Buildnumber = newins.createthebuild("B", "A40", "13", true, "0", "182", "D:\\Tracking\\Fixes\\", "D:\\Tracking\\Fixes\\Entries\\Entries.dbf");

        }

        protected void newid_Click(object sender, EventArgs e)
        {
            
        }
    }
}