namespace Aria.Utilities.Dictionary
{
    partial class FormAddFileToDictionary
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormAddFileToDictionary));
            this.labelFileAria4XP = new System.Windows.Forms.Label();
            this.comboBoxFiles = new System.Windows.Forms.ComboBox();
            this.comboBoxCustomer = new System.Windows.Forms.ComboBox();
            this.labelCustomer = new System.Windows.Forms.Label();
            this.labelFileNameAria5 = new System.Windows.Forms.Label();
            this.textBoxAria4XP = new System.Windows.Forms.TextBox();
            this.textBoxFileName = new System.Windows.Forms.TextBox();
            this.buttonAddToAria4 = new System.Windows.Forms.Button();
            this.buttonClose = new System.Windows.Forms.Button();
            this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
            this.textBoxFileDescription = new System.Windows.Forms.TextBox();
            this.labelFileDescriptionAria5 = new System.Windows.Forms.Label();
            this.comboBoxApplication = new System.Windows.Forms.ComboBox();
            this.textBoxFileFilter = new System.Windows.Forms.TextBox();
            this.labelFileFilterAria5 = new System.Windows.Forms.Label();
            this.textBoxFileFixedFilterAria5 = new System.Windows.Forms.TextBox();
            this.labelFileFixedFilterAria5 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
            this.SuspendLayout();
            // 
            // labelFileAria4XP
            // 
            this.labelFileAria4XP.AutoSize = true;
            this.labelFileAria4XP.Location = new System.Drawing.Point(20, 48);
            this.labelFileAria4XP.Name = "labelFileAria4XP";
            this.labelFileAria4XP.Size = new System.Drawing.Size(57, 13);
            this.labelFileAria4XP.TabIndex = 2;
            this.labelFileAria4XP.Text = "File Name:";
            // 
            // comboBoxFiles
            // 
            this.comboBoxFiles.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxFiles.FormattingEnabled = true;
            this.comboBoxFiles.Location = new System.Drawing.Point(225, 45);
            this.comboBoxFiles.Name = "comboBoxFiles";
            this.comboBoxFiles.Size = new System.Drawing.Size(212, 21);
            this.comboBoxFiles.TabIndex = 4;
            this.comboBoxFiles.SelectedIndexChanged += new System.EventHandler(this.comboBoxFiles_SelectedIndexChanged);
            // 
            // comboBoxCustomer
            // 
            this.comboBoxCustomer.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxCustomer.FormattingEnabled = true;
            this.comboBoxCustomer.Location = new System.Drawing.Point(155, 18);
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
            this.labelFileNameAria5.Location = new System.Drawing.Point(20, 74);
            this.labelFileNameAria5.Name = "labelFileNameAria5";
            this.labelFileNameAria5.Size = new System.Drawing.Size(96, 13);
            this.labelFileNameAria5.TabIndex = 5;
            this.labelFileNameAria5.Text = "File Name in Aria5:";
            // 
            // textBoxAria4XP
            // 
            this.textBoxAria4XP.Location = new System.Drawing.Point(155, 71);
            this.textBoxAria4XP.Name = "textBoxAria4XP";
            this.textBoxAria4XP.ReadOnly = true;
            this.textBoxAria4XP.Size = new System.Drawing.Size(70, 20);
            this.textBoxAria4XP.TabIndex = 6;
            this.textBoxAria4XP.Text = "Aria4XP.";
            // 
            // textBoxFileName
            // 
            this.textBoxFileName.Location = new System.Drawing.Point(225, 71);
            this.textBoxFileName.MaxLength = 92;
            this.textBoxFileName.Name = "textBoxFileName";
            this.textBoxFileName.Size = new System.Drawing.Size(212, 20);
            this.textBoxFileName.TabIndex = 7;
            this.textBoxFileName.TextChanged += new System.EventHandler(this.textBoxReportName_TextChanged);
            // 
            // buttonAddToAria4
            // 
            this.buttonAddToAria4.Location = new System.Drawing.Point(281, 182);
            this.buttonAddToAria4.Name = "buttonAddToAria4";
            this.buttonAddToAria4.Size = new System.Drawing.Size(82, 24);
            this.buttonAddToAria4.TabIndex = 14;
            this.buttonAddToAria4.Text = "Convert";
            this.buttonAddToAria4.UseVisualStyleBackColor = true;
            this.buttonAddToAria4.Click += new System.EventHandler(this.buttonAddToAria4_Click);
            // 
            // buttonClose
            // 
            this.buttonClose.Location = new System.Drawing.Point(369, 182);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(67, 24);
            this.buttonClose.TabIndex = 15;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);
            // 
            // errorProvider
            // 
            this.errorProvider.ContainerControl = this;
            // 
            // textBoxFileDescription
            // 
            this.textBoxFileDescription.Location = new System.Drawing.Point(155, 97);
            this.textBoxFileDescription.MaxLength = 92;
            this.textBoxFileDescription.Name = "textBoxFileDescription";
            this.textBoxFileDescription.Size = new System.Drawing.Size(282, 20);
            this.textBoxFileDescription.TabIndex = 9;
            // 
            // labelFileDescriptionAria5
            // 
            this.labelFileDescriptionAria5.AutoSize = true;
            this.labelFileDescriptionAria5.Location = new System.Drawing.Point(20, 100);
            this.labelFileDescriptionAria5.Name = "labelFileDescriptionAria5";
            this.labelFileDescriptionAria5.Size = new System.Drawing.Size(122, 13);
            this.labelFileDescriptionAria5.TabIndex = 8;
            this.labelFileDescriptionAria5.Text = "File Description in Aria5:";
            // 
            // comboBoxApplication
            // 
            this.comboBoxApplication.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxApplication.FormattingEnabled = true;
            this.comboBoxApplication.Items.AddRange(new object[] {
            "Aria27",
            "Aria4XP"});
            this.comboBoxApplication.Location = new System.Drawing.Point(155, 44);
            this.comboBoxApplication.Name = "comboBoxApplication";
            this.comboBoxApplication.Size = new System.Drawing.Size(64, 21);
            this.comboBoxApplication.TabIndex = 3;
            this.comboBoxApplication.SelectedIndexChanged += new System.EventHandler(this.comboBoxApplication_SelectedIndexChanged);
            // 
            // textBoxFileFilter
            // 
            this.textBoxFileFilter.Location = new System.Drawing.Point(155, 123);
            this.textBoxFileFilter.Name = "textBoxFileFilter";
            this.textBoxFileFilter.Size = new System.Drawing.Size(282, 20);
            this.textBoxFileFilter.TabIndex = 11;
            // 
            // labelFileFilterAria5
            // 
            this.labelFileFilterAria5.AutoSize = true;
            this.labelFileFilterAria5.Location = new System.Drawing.Point(20, 126);
            this.labelFileFilterAria5.Name = "labelFileFilterAria5";
            this.labelFileFilterAria5.Size = new System.Drawing.Size(93, 13);
            this.labelFileFilterAria5.TabIndex = 10;
            this.labelFileFilterAria5.Text = "File Filter in Aria5:";
            // 
            // textBoxFileFixedFilterAria5
            // 
            this.textBoxFileFixedFilterAria5.Location = new System.Drawing.Point(155, 149);
            this.textBoxFileFixedFilterAria5.Name = "textBoxFileFixedFilterAria5";
            this.textBoxFileFixedFilterAria5.Size = new System.Drawing.Size(282, 20);
            this.textBoxFileFixedFilterAria5.TabIndex = 13;
            // 
            // labelFileFixedFilterAria5
            // 
            this.labelFileFixedFilterAria5.AutoSize = true;
            this.labelFileFixedFilterAria5.Location = new System.Drawing.Point(20, 152);
            this.labelFileFixedFilterAria5.Name = "labelFileFixedFilterAria5";
            this.labelFileFixedFilterAria5.Size = new System.Drawing.Size(122, 13);
            this.labelFileFixedFilterAria5.TabIndex = 12;
            this.labelFileFixedFilterAria5.Text = "File Fixed Filter in Aria5:";
            // 
            // FormAddFileToDictionary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(457, 218);
            this.Controls.Add(this.textBoxFileFixedFilterAria5);
            this.Controls.Add(this.labelFileFixedFilterAria5);
            this.Controls.Add(this.textBoxFileFilter);
            this.Controls.Add(this.labelFileFilterAria5);
            this.Controls.Add(this.comboBoxApplication);
            this.Controls.Add(this.textBoxFileDescription);
            this.Controls.Add(this.labelFileDescriptionAria5);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.buttonAddToAria4);
            this.Controls.Add(this.textBoxFileName);
            this.Controls.Add(this.textBoxAria4XP);
            this.Controls.Add(this.labelFileNameAria5);
            this.Controls.Add(this.comboBoxCustomer);
            this.Controls.Add(this.labelCustomer);
            this.Controls.Add(this.comboBoxFiles);
            this.Controls.Add(this.labelFileAria4XP);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormAddFileToDictionary";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Add File to Aria5 Dictionary";
            this.Load += new System.EventHandler(this.FormConvertReport_Load);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelFileAria4XP;
        private System.Windows.Forms.ComboBox comboBoxFiles;
        private System.Windows.Forms.ComboBox comboBoxCustomer;
        private System.Windows.Forms.Label labelCustomer;
        private System.Windows.Forms.Label labelFileNameAria5;
        private System.Windows.Forms.TextBox textBoxAria4XP;
        private System.Windows.Forms.TextBox textBoxFileName;
        private System.Windows.Forms.Button buttonAddToAria4;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.ErrorProvider errorProvider;
        private System.Windows.Forms.TextBox textBoxFileDescription;
        private System.Windows.Forms.Label labelFileDescriptionAria5;
        private System.Windows.Forms.ComboBox comboBoxApplication;
        private System.Windows.Forms.TextBox textBoxFileFilter;
        private System.Windows.Forms.Label labelFileFilterAria5;
        private System.Windows.Forms.TextBox textBoxFileFixedFilterAria5;
        private System.Windows.Forms.Label labelFileFixedFilterAria5;
    }
}

