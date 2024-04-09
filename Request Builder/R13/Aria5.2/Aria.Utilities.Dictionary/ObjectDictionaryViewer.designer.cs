namespace Aria.Utilities.Dictionary
{
    partial class ObjectDictionaryViewer
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ObjectDictionaryViewer));
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.propertyGrid1 = new System.Windows.Forms.PropertyGrid();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fixToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.customerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.systemToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addStandardDataToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addStandardFieldToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addRelatedDataToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addStandardReportToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addStandardProgramToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.addCustomReportToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.generateFixToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 24);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.treeView1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.propertyGrid1);
            this.splitContainer1.Size = new System.Drawing.Size(788, 505);
            this.splitContainer1.SplitterDistance = 307;
            this.splitContainer1.TabIndex = 0;
            // 
            // treeView1
            // 
            this.treeView1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.treeView1.CheckBoxes = true;
            this.treeView1.Location = new System.Drawing.Point(0, 0);
            this.treeView1.Name = "treeView1";
            this.treeView1.Size = new System.Drawing.Size(304, 505);
            this.treeView1.TabIndex = 0;
            this.treeView1.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.treeView1_AfterSelect);
            // 
            // propertyGrid1
            // 
            this.propertyGrid1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.propertyGrid1.Location = new System.Drawing.Point(0, 0);
            this.propertyGrid1.Name = "propertyGrid1";
            this.propertyGrid1.Size = new System.Drawing.Size(477, 505);
            this.propertyGrid1.TabIndex = 0;
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "CloseFolder.bmp");
            this.imageList1.Images.SetKeyName(1, "DataObject.bmp");
            this.imageList1.Images.SetKeyName(2, "DataPointer.bmp");
            this.imageList1.Images.SetKeyName(3, "Event.bmp");
            this.imageList1.Images.SetKeyName(4, "Method.bmp");
            this.imageList1.Images.SetKeyName(5, "Option.bmp");
            this.imageList1.Images.SetKeyName(6, "PackageObject.bmp");
            this.imageList1.Images.SetKeyName(7, "RelatedDataObject.bmp");
            this.imageList1.Images.SetKeyName(8, "RelatedField.bmp");
            this.imageList1.Images.SetKeyName(9, "Report.bmp");
            this.imageList1.Images.SetKeyName(10, "soord.ico");
            this.imageList1.Images.SetKeyName(11, "SOEDORD ttt.ico");
            this.imageList1.Images.SetKeyName(12, "abview.ico");
            this.imageList1.Images.SetKeyName(13, "arrvkey.ico");
            this.imageList1.Images.SetKeyName(14, "Untitled.bmp");
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fixToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(788, 24);
            this.menuStrip1.TabIndex = 1;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fixToolStripMenuItem
            // 
            this.fixToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.customerToolStripMenuItem,
            this.addStandardDataToolStripMenuItem,
            this.addStandardFieldToolStripMenuItem,
            this.addRelatedDataToolStripMenuItem,
            this.addStandardReportToolStripMenuItem,
            this.addStandardProgramToolStripMenuItem,
            this.toolStripSeparator2,
            this.addCustomReportToolStripMenuItem,
            this.toolStripSeparator3,
            this.generateFixToolStripMenuItem});
            this.fixToolStripMenuItem.Name = "fixToolStripMenuItem";
            this.fixToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fixToolStripMenuItem.Text = "File";
            // 
            // customerToolStripMenuItem
            // 
            this.customerToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.systemToolStripMenuItem});
            this.customerToolStripMenuItem.Name = "customerToolStripMenuItem";
            this.customerToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.customerToolStripMenuItem.Text = "Customer";
            // 
            // systemToolStripMenuItem
            // 
            this.systemToolStripMenuItem.Name = "systemToolStripMenuItem";
            this.systemToolStripMenuItem.Size = new System.Drawing.Size(112, 22);
            this.systemToolStripMenuItem.Text = "System";
            this.systemToolStripMenuItem.Click += new System.EventHandler(this.systemToolStripMenuItem_Click);
            // 
            // addStandardDataToolStripMenuItem
            // 
            this.addStandardDataToolStripMenuItem.Name = "addStandardDataToolStripMenuItem";
            this.addStandardDataToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addStandardDataToolStripMenuItem.Text = "Add Standard Data";
            this.addStandardDataToolStripMenuItem.Click += new System.EventHandler(this.addStandardDataToolStripMenuItem_Click);
            // 
            // addStandardFieldToolStripMenuItem
            // 
            this.addStandardFieldToolStripMenuItem.Name = "addStandardFieldToolStripMenuItem";
            this.addStandardFieldToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addStandardFieldToolStripMenuItem.Text = "Add Standard Field";
            this.addStandardFieldToolStripMenuItem.Click += new System.EventHandler(this.addStandardFieldToolStripMenuItem_Click);
            // 
            // addRelatedDataToolStripMenuItem
            // 
            this.addRelatedDataToolStripMenuItem.Name = "addRelatedDataToolStripMenuItem";
            this.addRelatedDataToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addRelatedDataToolStripMenuItem.Text = "Add Related Data";
            this.addRelatedDataToolStripMenuItem.Click += new System.EventHandler(this.addRelatedDataToolStripMenuItem_Click);
            // 
            // addStandardReportToolStripMenuItem
            // 
            this.addStandardReportToolStripMenuItem.Name = "addStandardReportToolStripMenuItem";
            this.addStandardReportToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addStandardReportToolStripMenuItem.Text = "Add Standard Report";
            this.addStandardReportToolStripMenuItem.Click += new System.EventHandler(this.addStandardReportToolStripMenuItem_Click);
            // 
            // addStandardProgramToolStripMenuItem
            // 
            this.addStandardProgramToolStripMenuItem.Name = "addStandardProgramToolStripMenuItem";
            this.addStandardProgramToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addStandardProgramToolStripMenuItem.Text = "Add Standard Program";
            this.addStandardProgramToolStripMenuItem.Click += new System.EventHandler(this.addStandardProgramToolStripMenuItem_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(192, 6);
            // 
            // addCustomReportToolStripMenuItem
            // 
            this.addCustomReportToolStripMenuItem.Name = "addCustomReportToolStripMenuItem";
            this.addCustomReportToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.addCustomReportToolStripMenuItem.Text = "Add Custom Report";
            this.addCustomReportToolStripMenuItem.Click += new System.EventHandler(this.addCustomReportToolStripMenuItem_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(192, 6);
            // 
            // generateFixToolStripMenuItem
            // 
            this.generateFixToolStripMenuItem.Name = "generateFixToolStripMenuItem";
            this.generateFixToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.generateFixToolStripMenuItem.Text = "Generate Fix";
            this.generateFixToolStripMenuItem.Click += new System.EventHandler(this.generateFixToolStripMenuItem_Click);
            // 
            // ObjectDictionaryViewer
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(788, 529);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.menuStrip1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "ObjectDictionaryViewer";
            this.Text = "Aria 5.0 Object Dictionary Viewer";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.Load += new System.EventHandler(this.ObjectDictionaryViewer_Load);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.TreeView treeView1;
        private System.Windows.Forms.PropertyGrid propertyGrid1;
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fixToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem generateFixToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addStandardDataToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addStandardFieldToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addRelatedDataToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addStandardReportToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripMenuItem addCustomReportToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripMenuItem customerToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem systemToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addStandardProgramToolStripMenuItem;
    }
}