namespace Aria.Utilities.ObjectDictionary
{
    partial class ObjectDictionary
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.tvwTreeView = new System.Windows.Forms.TreeView();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.pgdPropertyGrid = new System.Windows.Forms.PropertyGrid();
            this.mnuMenuStrip = new System.Windows.Forms.MenuStrip();
            this.fielToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.newToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.objectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.revisionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.propertyToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.methodToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.methodParameterToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.eventToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.eventParameterToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.deleteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.importTableFieldsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.importTableRelatedFieldsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.importOptionGridToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuMenuStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // tvwTreeView
            // 
            this.tvwTreeView.Dock = System.Windows.Forms.DockStyle.Left;
            this.tvwTreeView.Location = new System.Drawing.Point(0, 24);
            this.tvwTreeView.Name = "tvwTreeView";
            this.tvwTreeView.Size = new System.Drawing.Size(198, 476);
            this.tvwTreeView.TabIndex = 0;
            this.tvwTreeView.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.tvwTreeView_AfterSelect);
            // 
            // splitter1
            // 
            this.splitter1.Location = new System.Drawing.Point(198, 24);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(4, 476);
            this.splitter1.TabIndex = 1;
            this.splitter1.TabStop = false;
            // 
            // pgdPropertyGrid
            // 
            this.pgdPropertyGrid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pgdPropertyGrid.Location = new System.Drawing.Point(202, 24);
            this.pgdPropertyGrid.Name = "pgdPropertyGrid";
            this.pgdPropertyGrid.Size = new System.Drawing.Size(670, 476);
            this.pgdPropertyGrid.TabIndex = 2;
            // 
            // mnuMenuStrip
            // 
            this.mnuMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fielToolStripMenuItem,
            this.toolsToolStripMenuItem});
            this.mnuMenuStrip.Location = new System.Drawing.Point(0, 0);
            this.mnuMenuStrip.Name = "mnuMenuStrip";
            this.mnuMenuStrip.Size = new System.Drawing.Size(872, 24);
            this.mnuMenuStrip.TabIndex = 3;
            this.mnuMenuStrip.Text = "menuStrip1";
            // 
            // fielToolStripMenuItem
            // 
            this.fielToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newToolStripMenuItem,
            this.openToolStripMenuItem,
            this.saveToolStripMenuItem,
            this.deleteToolStripMenuItem});
            this.fielToolStripMenuItem.Name = "fielToolStripMenuItem";
            this.fielToolStripMenuItem.Size = new System.Drawing.Size(41, 20);
            this.fielToolStripMenuItem.Text = "Item";
            // 
            // newToolStripMenuItem
            // 
            this.newToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.objectToolStripMenuItem,
            this.revisionToolStripMenuItem,
            this.propertyToolStripMenuItem,
            this.methodToolStripMenuItem,
            this.methodParameterToolStripMenuItem,
            this.eventToolStripMenuItem,
            this.eventParameterToolStripMenuItem});
            this.newToolStripMenuItem.Name = "newToolStripMenuItem";
            this.newToolStripMenuItem.Size = new System.Drawing.Size(140, 22);
            this.newToolStripMenuItem.Text = "New";
            // 
            // objectToolStripMenuItem
            // 
            this.objectToolStripMenuItem.Name = "objectToolStripMenuItem";
            this.objectToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.objectToolStripMenuItem.Text = "Object";
            this.objectToolStripMenuItem.Click += new System.EventHandler(this.objectToolStripMenuItem_Click);
            // 
            // revisionToolStripMenuItem
            // 
            this.revisionToolStripMenuItem.Name = "revisionToolStripMenuItem";
            this.revisionToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.revisionToolStripMenuItem.Text = "Revision";
            this.revisionToolStripMenuItem.Click += new System.EventHandler(this.revisionToolStripMenuItem_Click);
            // 
            // propertyToolStripMenuItem
            // 
            this.propertyToolStripMenuItem.Name = "propertyToolStripMenuItem";
            this.propertyToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.propertyToolStripMenuItem.Text = "Property";
            this.propertyToolStripMenuItem.Click += new System.EventHandler(this.propertyToolStripMenuItem_Click);
            // 
            // methodToolStripMenuItem
            // 
            this.methodToolStripMenuItem.Name = "methodToolStripMenuItem";
            this.methodToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.methodToolStripMenuItem.Text = "Method";
            this.methodToolStripMenuItem.Click += new System.EventHandler(this.methodToolStripMenuItem_Click);
            // 
            // methodParameterToolStripMenuItem
            // 
            this.methodParameterToolStripMenuItem.Name = "methodParameterToolStripMenuItem";
            this.methodParameterToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.methodParameterToolStripMenuItem.Text = "Method Parameter";
            this.methodParameterToolStripMenuItem.Click += new System.EventHandler(this.methodParameterToolStripMenuItem_Click);
            // 
            // eventToolStripMenuItem
            // 
            this.eventToolStripMenuItem.Name = "eventToolStripMenuItem";
            this.eventToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.eventToolStripMenuItem.Text = "Event";
            this.eventToolStripMenuItem.Click += new System.EventHandler(this.eventToolStripMenuItem_Click);
            // 
            // eventParameterToolStripMenuItem
            // 
            this.eventParameterToolStripMenuItem.Name = "eventParameterToolStripMenuItem";
            this.eventParameterToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.eventParameterToolStripMenuItem.Text = "Event Parameter";
            this.eventParameterToolStripMenuItem.Click += new System.EventHandler(this.eventParameterToolStripMenuItem_Click);
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.openToolStripMenuItem.Size = new System.Drawing.Size(140, 22);
            this.openToolStripMenuItem.Text = "Open";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.S)));
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(140, 22);
            this.saveToolStripMenuItem.Text = "Save";
            this.saveToolStripMenuItem.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
            // 
            // deleteToolStripMenuItem
            // 
            this.deleteToolStripMenuItem.Name = "deleteToolStripMenuItem";
            this.deleteToolStripMenuItem.Size = new System.Drawing.Size(140, 22);
            this.deleteToolStripMenuItem.Text = "Delete";
            this.deleteToolStripMenuItem.Click += new System.EventHandler(this.deleteToolStripMenuItem_Click);
            // 
            // toolsToolStripMenuItem
            // 
            this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.importTableFieldsToolStripMenuItem,
            this.importTableRelatedFieldsToolStripMenuItem,
            this.importOptionGridToolStripMenuItem,
            this.toolStripMenuItem1});
            this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
            this.toolsToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.toolsToolStripMenuItem.Text = "Tools";
            // 
            // importTableFieldsToolStripMenuItem
            // 
            this.importTableFieldsToolStripMenuItem.Name = "importTableFieldsToolStripMenuItem";
            this.importTableFieldsToolStripMenuItem.Size = new System.Drawing.Size(205, 22);
            this.importTableFieldsToolStripMenuItem.Text = "Import Table Fields";
            this.importTableFieldsToolStripMenuItem.Click += new System.EventHandler(this.importTableFieldsToolStripMenuItem_Click);
            // 
            // importTableRelatedFieldsToolStripMenuItem
            // 
            this.importTableRelatedFieldsToolStripMenuItem.Name = "importTableRelatedFieldsToolStripMenuItem";
            this.importTableRelatedFieldsToolStripMenuItem.Size = new System.Drawing.Size(205, 22);
            this.importTableRelatedFieldsToolStripMenuItem.Text = "Import Table Related Fields";
            this.importTableRelatedFieldsToolStripMenuItem.Click += new System.EventHandler(this.importTableRelatedFieldsToolStripMenuItem_Click);
            // 
            // importOptionGridToolStripMenuItem
            // 
            this.importOptionGridToolStripMenuItem.Name = "importOptionGridToolStripMenuItem";
            this.importOptionGridToolStripMenuItem.Size = new System.Drawing.Size(205, 22);
            this.importOptionGridToolStripMenuItem.Text = "Import Report";
            this.importOptionGridToolStripMenuItem.Click += new System.EventHandler(this.importOptionGridToolStripMenuItem_Click);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(205, 22);
            this.toolStripMenuItem1.Text = "Import Program";
            this.toolStripMenuItem1.Click += new System.EventHandler(this.toolStripMenuItem1_Click);
            // 
            // ObjectDictionary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(872, 500);
            this.Controls.Add(this.pgdPropertyGrid);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.tvwTreeView);
            this.Controls.Add(this.mnuMenuStrip);
            this.MainMenuStrip = this.mnuMenuStrip;
            this.Name = "ObjectDictionary";
            this.Text = "Object Dictionary";
            this.mnuMenuStrip.ResumeLayout(false);
            this.mnuMenuStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TreeView tvwTreeView;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.PropertyGrid pgdPropertyGrid;
        private System.Windows.Forms.MenuStrip mnuMenuStrip;
        private System.Windows.Forms.ToolStripMenuItem fielToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem saveToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem newToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem deleteToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem objectToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem revisionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem propertyToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem toolsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem importTableFieldsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem importTableRelatedFieldsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem methodParameterToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem eventParameterToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem methodToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem eventToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem importOptionGridToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem1;

    }
}

