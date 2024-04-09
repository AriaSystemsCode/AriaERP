namespace Mapping
{
    partial class Form1
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
            this.btnOpen = new System.Windows.Forms.Button();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.txtSchemaPath = new System.Windows.Forms.TextBox();
            this.btnServerLoad = new System.Windows.Forms.Button();
            this.treeDist = new System.Windows.Forms.TreeView();
            this.btnConfigure = new System.Windows.Forms.Button();
            this.treesource = new System.Windows.Forms.TreeView();
            this.btnSave = new System.Windows.Forms.Button();
            this.saveFileDialog1 = new System.Windows.Forms.SaveFileDialog();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.addConditionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.fixedValueToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.txtValue = new System.Windows.Forms.TextBox();
            this.lblTitle = new System.Windows.Forms.Label();
            this.btnAdd = new System.Windows.Forms.Button();
            this.txtCondition = new System.Windows.Forms.TextBox();
            this.lblCondition = new System.Windows.Forms.Label();
            this.btnAddCondition = new System.Windows.Forms.Button();
            this.contextMenuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnOpen
            // 
            this.btnOpen.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
            this.btnOpen.Location = new System.Drawing.Point(289, 12);
            this.btnOpen.Name = "btnOpen";
            this.btnOpen.Size = new System.Drawing.Size(39, 21);
            this.btnOpen.TabIndex = 0;
            this.btnOpen.Text = "...";
            this.btnOpen.UseVisualStyleBackColor = true;
            this.btnOpen.Click += new System.EventHandler(this.btnOpen_Click);
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "openFileDialog1";
            this.openFileDialog1.Filter = "Schema files|*.xsd";
            // 
            // txtSchemaPath
            // 
            this.txtSchemaPath.Enabled = false;
            this.txtSchemaPath.Location = new System.Drawing.Point(12, 12);
            this.txtSchemaPath.Name = "txtSchemaPath";
            this.txtSchemaPath.Size = new System.Drawing.Size(277, 20);
            this.txtSchemaPath.TabIndex = 1;
            // 
            // btnServerLoad
            // 
            this.btnServerLoad.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
            this.btnServerLoad.Location = new System.Drawing.Point(548, 9);
            this.btnServerLoad.Name = "btnServerLoad";
            this.btnServerLoad.Size = new System.Drawing.Size(170, 23);
            this.btnServerLoad.TabIndex = 6;
            this.btnServerLoad.Text = "Load";
            this.btnServerLoad.UseVisualStyleBackColor = true;
            this.btnServerLoad.Click += new System.EventHandler(this.btnServerLoad_Click);
            // 
            // treeDist
            // 
            this.treeDist.AllowDrop = true;
            this.treeDist.HideSelection = false;
            this.treeDist.HotTracking = true;
            this.treeDist.Location = new System.Drawing.Point(425, 39);
            this.treeDist.Name = "treeDist";
            this.treeDist.Size = new System.Drawing.Size(318, 436);
            this.treeDist.TabIndex = 1;
            this.treeDist.NodeMouseDoubleClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.treeServerTableColumns_NodeMouseDoubleClick);
            this.treeDist.DragDrop += new System.Windows.Forms.DragEventHandler(this.treeSchemaTableColumns_DragDrop);
            this.treeDist.NodeMouseClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.treeDist_NodeMouseClick);
            this.treeDist.DragOver += new System.Windows.Forms.DragEventHandler(this.treeSchemaTableColumns_DragOver);
            // 
            // btnConfigure
            // 
            this.btnConfigure.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
            this.btnConfigure.Location = new System.Drawing.Point(434, 9);
            this.btnConfigure.Name = "btnConfigure";
            this.btnConfigure.Size = new System.Drawing.Size(93, 23);
            this.btnConfigure.TabIndex = 7;
            this.btnConfigure.Text = "Configure SQL";
            this.btnConfigure.UseVisualStyleBackColor = true;
            this.btnConfigure.Click += new System.EventHandler(this.btnConfigure_Click);
            // 
            // treesource
            // 
            this.treesource.HideSelection = false;
            this.treesource.HotTracking = true;
            this.treesource.Location = new System.Drawing.Point(12, 39);
            this.treesource.Name = "treesource";
            this.treesource.Size = new System.Drawing.Size(318, 436);
            this.treesource.TabIndex = 1;
            this.treesource.NodeMouseDoubleClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.treeSchemaTableColumns_NodeMouseDoubleClick);
            this.treesource.DragDrop += new System.Windows.Forms.DragEventHandler(this.treeSchemaTableColumns_DragDrop);
            this.treesource.MouseDown += new System.Windows.Forms.MouseEventHandler(this.treeSchemaTableColumns_MouseDown);
            this.treesource.DragOver += new System.Windows.Forms.DragEventHandler(this.treeSchemaTableColumns_DragOver);
            // 
            // btnSave
            // 
            this.btnSave.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
            this.btnSave.Location = new System.Drawing.Point(280, 537);
            this.btnSave.Name = "btnSave";
            this.btnSave.Size = new System.Drawing.Size(185, 23);
            this.btnSave.TabIndex = 8;
            this.btnSave.Text = "Save";
            this.btnSave.UseVisualStyleBackColor = true;
            this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
            // 
            // saveFileDialog1
            // 
            this.saveFileDialog1.Filter = "XML Files|*.xml";
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.addConditionToolStripMenuItem,
            this.fixedValueToolStripMenuItem});
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(146, 48);
            // 
            // addConditionToolStripMenuItem
            // 
            this.addConditionToolStripMenuItem.Name = "addConditionToolStripMenuItem";
            this.addConditionToolStripMenuItem.Size = new System.Drawing.Size(145, 22);
            this.addConditionToolStripMenuItem.Text = "AddCondition";
            this.addConditionToolStripMenuItem.Click += new System.EventHandler(this.addConditionToolStripMenuItem_Click);
            // 
            // fixedValueToolStripMenuItem
            // 
            this.fixedValueToolStripMenuItem.Name = "fixedValueToolStripMenuItem";
            this.fixedValueToolStripMenuItem.Size = new System.Drawing.Size(145, 22);
            this.fixedValueToolStripMenuItem.Text = "AddFixedValue";
            this.fixedValueToolStripMenuItem.Click += new System.EventHandler(this.fixedValueToolStripMenuItem_Click);
            // 
            // txtValue
            // 
            this.txtValue.Location = new System.Drawing.Point(491, 479);
            this.txtValue.Name = "txtValue";
            this.txtValue.Size = new System.Drawing.Size(227, 20);
            this.txtValue.TabIndex = 10;
            this.txtValue.Visible = false;
            // 
            // lblTitle
            // 
            this.lblTitle.AutoSize = true;
            this.lblTitle.Location = new System.Drawing.Point(422, 482);
            this.lblTitle.Name = "lblTitle";
            this.lblTitle.Size = new System.Drawing.Size(69, 13);
            this.lblTitle.TabIndex = 11;
            this.lblTitle.Text = "Fixed Value :";
            this.lblTitle.Visible = false;
            // 
            // btnAdd
            // 
            this.btnAdd.Location = new System.Drawing.Point(720, 476);
            this.btnAdd.Name = "btnAdd";
            this.btnAdd.Size = new System.Drawing.Size(28, 24);
            this.btnAdd.TabIndex = 12;
            this.btnAdd.Text = "ok";
            this.btnAdd.UseVisualStyleBackColor = true;
            this.btnAdd.Visible = false;
            this.btnAdd.Click += new System.EventHandler(this.btnAdd_Click);
            // 
            // txtCondition
            // 
            this.txtCondition.AllowDrop = true;
            this.txtCondition.Location = new System.Drawing.Point(491, 505);
            this.txtCondition.Name = "txtCondition";
            this.txtCondition.Size = new System.Drawing.Size(227, 20);
            this.txtCondition.TabIndex = 10;
            this.txtCondition.Visible = false;
            this.txtCondition.DragDrop += new System.Windows.Forms.DragEventHandler(this.txtCondition_DragDrop);
            this.txtCondition.DragOver += new System.Windows.Forms.DragEventHandler(this.txtCondition_DragOver);
            // 
            // lblCondition
            // 
            this.lblCondition.AutoSize = true;
            this.lblCondition.Location = new System.Drawing.Point(422, 508);
            this.lblCondition.Name = "lblCondition";
            this.lblCondition.Size = new System.Drawing.Size(58, 13);
            this.lblCondition.TabIndex = 11;
            this.lblCondition.Text = "Conditioin:";
            this.lblCondition.Visible = false;
            // 
            // btnAddCondition
            // 
            this.btnAddCondition.Location = new System.Drawing.Point(720, 502);
            this.btnAddCondition.Name = "btnAddCondition";
            this.btnAddCondition.Size = new System.Drawing.Size(28, 24);
            this.btnAddCondition.TabIndex = 12;
            this.btnAddCondition.Text = "ok";
            this.btnAddCondition.UseVisualStyleBackColor = true;
            this.btnAddCondition.Visible = false;
            this.btnAddCondition.Click += new System.EventHandler(this.btnAddCondition_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(769, 562);
            this.Controls.Add(this.treesource);
            this.Controls.Add(this.btnSave);
            this.Controls.Add(this.btnAdd);
            this.Controls.Add(this.btnAddCondition);
            this.Controls.Add(this.lblTitle);
            this.Controls.Add(this.lblCondition);
            this.Controls.Add(this.txtValue);
            this.Controls.Add(this.txtCondition);
            this.Controls.Add(this.btnConfigure);
            this.Controls.Add(this.treeDist);
            this.Controls.Add(this.btnServerLoad);
            this.Controls.Add(this.txtSchemaPath);
            this.Controls.Add(this.btnOpen);
            this.Name = "Form1";
            this.Text = "Form1";
            this.contextMenuStrip1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button btnOpen;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.TextBox txtSchemaPath;
        private System.Windows.Forms.Button btnServerLoad;
        private System.Windows.Forms.TreeView treeDist;
        private System.Windows.Forms.Button btnConfigure;
        private System.Windows.Forms.TreeView treesource;
        private System.Windows.Forms.Button btnSave;
        private System.Windows.Forms.SaveFileDialog saveFileDialog1;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.ToolStripMenuItem addConditionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem fixedValueToolStripMenuItem;
        private System.Windows.Forms.TextBox txtValue;
        private System.Windows.Forms.Label lblTitle;
        private System.Windows.Forms.Button btnAdd;
        private System.Windows.Forms.TextBox txtCondition;
        private System.Windows.Forms.Label lblCondition;
        private System.Windows.Forms.Button btnAddCondition;
    }
}

