﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.Web;
using AriaDevExpress.Module.BusinessObjects.SysFiles;

namespace AriaDevExpress.Web.UserControls.SysFiles
{
    public partial class SortReportVariable : System.Web.UI.UserControl
    {
        protected override void OnInit(System.EventArgs e)
        {
            lbAvailable.DataSource = ParentReport.variables.OrderBy(variable => variable.Position);
            lbAvailable.DataBind();
            base.OnInit(e);
        }
        protected void Page_Load(object sender, EventArgs e)
        {
            if (IsPostBack && Request["__CALLBACKPARAM"].Contains("DialogOK"))
                Save();
        }

        public Report ParentReport { get; set; }

        //protected void lbAvailable_DataBound(object sender, EventArgs e)
        //{
        //    if (ParentFile != null && !string.IsNullOrWhiteSpace(ParentFile.UserBrowseFileds))
        //    {
        //        string script = "";
        //        string scriptTemplate = lbAvailable.ClientInstanceName + ".SelectValues(['{0}']);AddSelectedItems();";
        //        string[] browseFields = ParentFile.UserBrowseFileds.Split(new string[] { "|" }, StringSplitOptions.RemoveEmptyEntries);
        //        foreach (string field in browseFields)
        //        {
        //            if (lbAvailable.Items.FindByValue(field) != null)
        //            {
        //                lbAvailable.Items.FindByValue(field).Selected = true;
        //                // script += "'" + field + "',";
        //                script += string.Format(scriptTemplate, field);
        //            }
        //        }
        //        if (!string.IsNullOrWhiteSpace(script))
        //        {
        //            Page.ClientScript.RegisterStartupScript(typeof(Page), "MoveItems", script, true);
        //        }
        //    }
        //}

        protected void Save()
        {
            var items = lbAvailable.Items;
            for (int i = 0; i < items.Count; i++)
            {
                ParentReport.variables.Where(var => var.Name == items[i].Value.ToString()).First().Position = i;
            }
        }
    }
}