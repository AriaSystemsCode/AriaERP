using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.Xpo;
using DevExpress.Persistent.AuditTrail;
using DevExpress.Persistent.Validation;
using DevExpress.Xpo.Metadata;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using DevExpress.ExpressApp.Web.SystemModule;

namespace AriaDevExpress.Module.Controllers.SysFiles
{
    public partial class AuditController : ViewController
    {
        public AuditController()
        {
            InitializeComponent();
            RegisterActions(components);

        }

        AriaAudit audit;
        public void ShowAriaAuditAction_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace ExpBuilderObjectSpace = Application.CreateObjectSpace();
            audit = ExpBuilderObjectSpace.CreateObject<AriaAudit>();
            if (!string.IsNullOrWhiteSpace(Ticket))
                audit.TicketNumber = Ticket;
            if (!string.IsNullOrWhiteSpace(Comments))
                audit.Comments = Comments;
            if (!string.IsNullOrWhiteSpace(TrackingEntry))
                audit.TrackingEntry = TrackingEntry;
            DetailView detailView = Application.CreateDetailView(ExpBuilderObjectSpace, audit);
            detailView.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            e.View = detailView;
        }

        private void ShowAriaAuditAction_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            // To Be Check By Mahmoud
            if (Validator.RuleSet.ValidateTarget(null, audit, ContextIdentifier.Save).State == ValidationState.Valid)
            {
                Ticket = audit.TicketNumber;
                Comments = audit.Comments;
                TrackingEntry = audit.TrackingEntry;
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;

                foreach (ReflectionPropertyInfo associatedProperty in currentobj.ClassInfo.AssociationListProperties)
                {
                    if (associatedProperty.IsAssociation || associatedProperty.IsAssociationList || associatedProperty.IsCollection)
                    {
                        object x = currentobj.GetMemberValue(associatedProperty.Name);
                        if (x is XPBaseCollection)
                        {
                            XPBaseCollection coll = (XPBaseCollection)x;
                            if (coll.Count > 0 && coll.BaseIndexer(0) is IAuditable)
                            {
                                for (int i = 0; i < coll.Count; i++)
                                {
                                    XPBaseObject currentChildobj = coll.BaseIndexer(i) as XPBaseObject;
                                    if (currentChildobj.ClassInfo.FindMember("Ticket") != null)
                                        currentChildobj.SetMemberValue("Ticket", audit.TicketNumber);
                                    if (currentChildobj.ClassInfo.FindMember("Comments") != null)
                                        currentChildobj.SetMemberValue("Comments", audit.Comments);
                                    if (currentChildobj.ClassInfo.FindMember("TrackingEntry") != null)
                                        currentChildobj.SetMemberValue("TrackingEntry", audit.TrackingEntry);
                                }
                            }
                        }
                    }
                }
            }
        }

        public string Ticket
        {
            get
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("Ticket") != null)
                    return (string)(currentobj.GetMemberValue("Ticket"));
                return "";
            }
            set
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("Ticket") != null)
                    currentobj.SetMemberValue("Ticket", value);
            }
        }

        public string Comments
        {
            get
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("Comments") != null)
                    return (string)(currentobj.GetMemberValue("Comments"));
                return "";
            }
            set
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("Comments") != null)
                    currentobj.SetMemberValue("Comments", value);
            }
        }

        public string TrackingEntry
        {
            get
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("TrackingEntry") != null)
                    return (string)(currentobj.GetMemberValue("TrackingEntry"));
                return "";
            }
            set
            {
                XPBaseObject currentobj = View.CurrentObject as XPBaseObject;
                if (currentobj.ClassInfo.FindMember("TrackingEntry") != null)
                    currentobj.SetMemberValue("TrackingEntry", value);
            }
        }
    }
}
