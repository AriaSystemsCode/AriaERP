namespace Aria.Utilities.Dictionary
{
    partial class FormAddRelatedFileToDictionary
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormAddRelatedFileToDictionary));
            this.labelObjectAria5 = new System.Windows.Forms.Label();
            this.comboBoxFiles = new System.Windows.Forms.ComboBox();
            this.comboBoxCustomer = new System.Windows.Forms.ComboBox();
            this.labelCustomer = new System.Windows.Forms.Label();
            this.labelFileNameAria5 = new System.Windows.Forms.Label();
            this.textBoxAria4XP = new System.Windows.Forms.TextBox();
            this.textBoxRelatedFileName = new System.Windows.Forms.TextBox();
            this.buttonAddToAria4 = new System.Windows.Forms.Button();
            this.buttonClose = new System.Windows.Forms.Button();
            this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
            this.textBoxFileFilter = new System.Windows.Forms.TextBox();
            this.labelRelatedFileFilterAria5 = new System.Windows.Forms.Label();
            this.comboBoxRelatedFiles = new System.Windows.Forms.ComboBox();
            this.labelRelatedFileAria4XP = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
            this.SuspendLayout();
            // 
            // labelObjectAria5
            // 
            this.labelObjectAria5.AutoSize = true;
            this.labelObjectAria5.Location = new System.Drawing.Point(20, 48);
            this.labelObjectAria5.Name = "labelObjectAria5";
            this.labelObjectAria5.Size = new System.Drawing.Size(112, 13);
            this.labelObjectAria5.TabIndex = 2;
            this.labelObjectAria5.Text = "Object Name in Aria5:";
            // 
            // comboBoxFiles
            // 
            this.comboBoxFiles.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxFiles.FormattingEnabled = true;
            this.comboBoxFiles.Location = new System.Drawing.Point(201, 45);
            this.comboBoxFiles.Name = "comboBoxFiles";
            this.comboBoxFiles.Size = new System.Drawing.Size(282, 21);
            this.comboBoxFiles.TabIndex = 3;
            this.comboBoxFiles.SelectedIndexChanged += new System.EventHandler(this.comboBoxFiles_SelectedIndexChanged);
            // 
            // comboBoxCustomer
            // 
            this.comboBoxCustomer.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxCustomer.FormattingEnabled = true;
            this.comboBoxCustomer.Location = new System.Drawing.Point(201, 18);
            this.comboBoxCustomer.Name = "comboBoxCustomer";
            this.comboBoxCustomer.Size = new System.Drawing.Size(282, 21);
            this.comboBoxCustomer.TabIndex = 1;
            this.comboBoxCustomer.SelectedIndexChanged += new System.EventHandler(this.comboBoxCustomer_SelectedIndexChanged);
            // 
            // labelCustomer
            // 
            this.labelCustomer.AutoSize = true;
            this.labelCustomer.Location = new System.Drawing.Point(20, 21);
            this.labelCustomer.Name = "labelCustomer";
            this.labelCustomer.Size = new System.Drawing.Size(57, 13);
            this.labelCustomer.TabIndex = 0;
            this.labelCustomer.Text = "Customer:";
            // 
            // labelFileNameAria5
            // 
            this.labelFileNameAria5.AutoSize = true;
            this.labelFileNameAria5.Location = new System.Drawing.Point(20, 105);
            this.labelFileNameAria5.Name = "labelFileNameAria5";
            this.labelFileNameAria5.Size = new System.Drawing.Size(96, 13);
            this.labelFileNameAria5.TabIndex = 6;
            this.labelFileNameAria5.Text = "File Name in Aria5:";
            // 
            // textBoxAria4XP
            // 
            this.textBoxAria4XP.Location = new System.Drawing.Point(201, 102);
            this.textBoxAria4XP.Name = "textBoxAria4XP";
            this.textBoxAria4XP.ReadOnly = true;
            this.textBoxAria4XP.Size = new System.Drawing.Size(121, 20);
            this.textBoxAria4XP.TabIndex = 7;
            this.textBoxAria4XP.Text = "Aria4XP.";
            // 
            // textBoxRelatedFileName
            // 
            this.textBoxRelatedFileName.Location = new System.Drawing.Point(328, 102);
            this.textBoxRelatedFileName.MaxLength = 92;
            this.textBoxRelatedFileName.Name = "textBoxRelatedFileName";
            this.textBoxRelatedFileName.Size = new System.Drawing.Size(155, 20);
            this.textBoxRelatedFileName.TabIndex = 8;
            this.textBoxRelatedFileName.TextChanged += new System.EventHandler(this.textBoxReportName_TextChanged);
            // 
            // buttonAddToAria4
            // 
            this.buttonAddToAria4.Location = new System.Drawing.Point(328, 162);
            this.buttonAddToAria4.Name = "buttonAddToAria4";
            this.buttonAddToAria4.Size = new System.Drawing.Size(82, 24);
            this.buttonAddToAria4.TabIndex = 11;
            this.buttonAddToAria4.Text = "Convert";
            this.buttonAddToAria4.UseVisualStyleBackColor = true;
            this.buttonAddToAria4.Click += new System.EventHandler(this.buttonAddToAria4_Click);
            // 
            // buttonClose
            // 
            this.buttonClose.Location = new System.Drawing.Point(416, 162);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(67, 24);
            this.buttonClose.TabIndex = 12;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);
            // 
            // errorProvider
            // 
            this.errorProvider.ContainerControl = this;
            // 
            // textBoxFileFilter
            // 
            this.textBoxFileFilter.Location = new System.Drawing.Point(201, 128);
            this.textBoxFileFilter.Name = "textBoxFileFilter";
            this.textBoxFileFilter.Size = new System.Drawing.Size(282, 20);
            this.textBoxFileFilter.TabIndex = 10;
            // 
            // labelRelatedFileFilterAria5
            // 
            this.labelRelatedFileFilterAria5.AutoSize = true;
            this.labelRelatedFileFilterAria5.Location = new System.Drawing.Point(20, 131);
            this.labelRelatedFileFilterAria5.Name = "labelRelatedFileFilterAria5";
            this.labelRelatedFileFilterAria5.Size = new System.Drawing.Size(133, 13);
            this.labelRelatedFileFilterAria5.TabIndex = 9;
            this.labelRelatedFileFilterAria5.Text = "Related File Filter in Aria5:";
            // 
            // comboBoxRelatedFiles
            // 
            this.comboBoxRelatedFiles.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxRelatedFiles.FormattingEnabled = true;
            this.comboBoxRelatedFiles.Location = new System.Drawing.Point(201, 72);
            this.comboBoxRelatedFiles.Name = "comboBoxRelatedFiles";
            this.comboBoxRelatedFiles.Size = new System.Drawing.Size(282, 21);
            this.comboBoxRelatedFiles.TabIndex = 5;
            this.comboBoxRelatedFiles.SelectedIndexChanged += new System.EventHandler(this.comboBoxRelatedFiles_SelectedIndexChanged);
            // 
            // labelRelatedFileAria4XP
            // 
            this.labelRelatedFileAria4XP.AutoSize = true;
            this.labelRelatedFileAria4XP.Location = new System.Drawing.Point(20, 75);
            this.labelRelatedFileAria4XP.Name = "labelRelatedFileAria4XP";
            this.labelRelatedFileAria4XP.Size = new System.Drawing.Size(152, 13);
            this.labelRelatedFileAria4XP.TabIndex = 4;
            this.labelRelatedFileAria4XP.Text = "Related Object Name in Aria5:";
            // 
            // FormAddRelatedFileToDictionary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(495, 195);
            this.Controls.Add(this.comboBoxRelatedFiles);
            this.Controls.Add(this.labelRelatedFileAria4XP);
            this.Controls.Add(this.textBoxFileFilter);
            this.Controls.Add(this.labelRelatedFileFilterAria5);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.buttonAddToAria4);
            this.Controls.Add(this.textBoxRelatedFileName);
            this.Controls.Add(this.textBoxAria4XP);
            this.Controls.Add(this.labelFileNameAria5);
            this.Controls.Add(this.comboBoxCustomer);
            this.Controls.Add(this.labelCustomer);
            this.Controls.Add(this.comboBoxFiles);
            this.Controls.Add(this.labelObjectAria5);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormAddRelatedFileToDictionary";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Add Related File to Aria5 Dictionary";
            this.Load += new System.EventHandler(this.FormConvertReport_Load);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelObjectAria5;
        private System.Windows.Forms.ComboBox comboBoxFiles;
        private System.Windows.Forms.ComboBox comboBoxCustomer;
        private System.Windows.Forms.Label labelCustomer;
        private System.Windows.Forms.Label labelFileNameAria5;
        private System.Windows.Forms.TextBox textBoxAria4XP;
        private System.Windows.Forms.TextBox textBoxRelatedFileName;
        private System.Windows.Forms.Button buttonAddToAria4;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.ErrorProvider errorProvider;
        private System.Windows.Forms.TextBox textBoxFileFilter;
        private System.Windows.Forms.Label labelRelatedFileFilterAria5;
        private System.Windows.Forms.ComboBox comboBoxRelatedFiles;
        private System.Windows.Forms.Label labelRelatedFileAria4XP;
    }
}

