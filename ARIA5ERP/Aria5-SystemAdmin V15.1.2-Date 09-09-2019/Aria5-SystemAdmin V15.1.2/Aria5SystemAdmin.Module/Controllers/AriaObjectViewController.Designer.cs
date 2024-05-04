namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectViewController
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.AssignNewRevision = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateTrackingEntry = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateChildTrackingEntry = new DevExpress.ExpressApp.Actions.ParametrizedAction(this.components);
            // 
            // AssignNewRevision
            // 
            this.AssignNewRevision.Caption = "Assign New Revision";
            this.AssignNewRevision.ConfirmationMessage = null;
            this.AssignNewRevision.Id = "AssignNewRevision";
            this.AssignNewRevision.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.AssignNewRevision.ToolTip = null;
            this.AssignNewRevision.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.AssignNewRevision.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.AssignNewRevision_Execute);
            // 
            // CreateTrackingEntry
            // 
            this.CreateTrackingEntry.Caption = "Create Tracking Entry";
            this.CreateTrackingEntry.ConfirmationMessage = null;
            this.CreateTrackingEntry.Id = "CreateTrackingEntry";
            this.CreateTrackingEntry.TargetObjectsCriteria = "ObjectType.ObjectTypeID != \'Entity\'";
            this.CreateTrackingEntry.TargetObjectsCriteriaMode = DevExpress.ExpressApp.Actions.TargetObjectsCriteriaMode.TrueForAll;
            this.CreateTrackingEntry.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObject);
            this.CreateTrackingEntry.TargetViewNesting = DevExpress.ExpressApp.Nesting.Nested;
            this.CreateTrackingEntry.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.CreateTrackingEntry.ToolTip = null;
            this.CreateTrackingEntry.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.CreateTrackingEntry.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateTrackingEntry_Execute);
            // 
            // CreateChildTrackingEntry
            // 
            this.CreateChildTrackingEntry.Caption = "Create Child Tracking Entry";
            this.CreateChildTrackingEntry.ConfirmationMessage = null;
            this.CreateChildTrackingEntry.Id = "CreateChildTrackingEntry";
            this.CreateChildTrackingEntry.NullValuePrompt = null;
            this.CreateChildTrackingEntry.ShortCaption = null;
            this.CreateChildTrackingEntry.TargetObjectsCriteria = "ObjectType.ObjectTypeID != \'Entity\'";
            this.CreateChildTrackingEntry.TargetObjectsCriteriaMode = DevExpress.ExpressApp.Actions.TargetObjectsCriteriaMode.TrueForAll;
            this.CreateChildTrackingEntry.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObject);
            this.CreateChildTrackingEntry.TargetViewNesting = DevExpress.ExpressApp.Nesting.Nested;
            this.CreateChildTrackingEntry.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.CreateChildTrackingEntry.ToolTip = null;
            this.CreateChildTrackingEntry.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.CreateChildTrackingEntry.ValueType = typeof(int);
            this.CreateChildTrackingEntry.Execute += new DevExpress.ExpressApp.Actions.ParametrizedActionExecuteEventHandler(this.CreateChildTrackingEntry_Execute);
            // 
            // AriaObjectViewController
            // 
            this.Actions.Add(this.AssignNewRevision);
            this.Actions.Add(this.CreateTrackingEntry);
            this.Actions.Add(this.CreateChildTrackingEntry);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObject);
            this.TypeOfView = typeof(DevExpress.ExpressApp.View);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction AssignNewRevision;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateTrackingEntry;
        private DevExpress.ExpressApp.Actions.ParametrizedAction CreateChildTrackingEntry;


    }
}
