namespace Aria.Utilities.ObjectDictionary
{
    partial class ImportOptionGridOptions
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
            this.dlgFolderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
            this.lblTableName = new System.Windows.Forms.Label();
            this.cboOptionGrids = new System.Windows.Forms.ComboBox();
            this.cmdCancel = new System.Windows.Forms.Button();
            this.cmdOK = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // lblTableName
            // 
            this.lblTableName.AutoSize = true;
            this.lblTableName.Location = new System.Drawing.Point(12, 12);
            this.lblTableName.Name = "lblTableName";
            this.lblTableName.Size = new System.Drawing.Size(98, 13);
            this.lblTableName.TabIndex = 3;
            this.lblTableName.Text = "Option Grid Name :";
            // 
            // cboOptionGrids
            // 
            this.cboOptionGrids.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboOptionGrids.FormattingEnabled = true;
            this.cboOptionGrids.Location = new System.Drawing.Point(116, 12);
            this.cboOptionGrids.Name = "cboOptionGrids";
            this.cboOptionGrids.Size = new System.Drawing.Size(390, 21);
            this.cboOptionGrids.TabIndex = 4;
            // 
            // cmdCancel
            // 
            this.cmdCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cmdCancel.Location = new System.Drawing.Point(429, 46);
            this.cmdCancel.Name = "cmdCancel";
            this.cmdCancel.Size = new System.Drawing.Size(77, 24);
            this.cmdCancel.TabIndex = 13;
            this.cmdCancel.Text = "Cancel";
            this.cmdCancel.UseVisualStyleBackColor = true;
            // 
            // cmdOK
            // 
            this.cmdOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.cmdOK.Location = new System.Drawing.Point(347, 46);
            this.cmdOK.Name = "cmdOK";
            this.cmdOK.Size = new System.Drawing.Size(77, 24);
            this.cmdOK.TabIndex = 12;
            this.cmdOK.Text = "OK";
            this.cmdOK.UseVisualStyleBackColor = true;
            // 
            // ImportOptionGridOptions
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(518, 82);
            this.Controls.Add(this.cmdCancel);
            this.Controls.Add(this.cmdOK);
            this.Controls.Add(this.cboOptionGrids);
            this.Controls.Add(this.lblTableName);
            this.Name = "ImportOptionGridOptions";
            this.Text = "Import Table Fields";
            this.Load += new System.EventHandler(this.ImportTableFields_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.FolderBrowserDialog dlgFolderBrowserDialog;
        private System.Windows.Forms.Label lblTableName;
        private System.Windows.Forms.Button cmdCancel;
        private System.Windows.Forms.Button cmdOK;
        public System.Windows.Forms.ComboBox cboOptionGrids;
    }
}