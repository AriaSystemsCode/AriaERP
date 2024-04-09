namespace Aria.Utilities.Dictionary
{
    partial class FormAddCustomReportToDictionary
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormAddCustomReportToDictionary));
            this.labelReportAria4XP = new System.Windows.Forms.Label();
            this.comboBoxReports = new System.Windows.Forms.ComboBox();
            this.comboBoxCustomer = new System.Windows.Forms.ComboBox();
            this.labelCustomer = new System.Windows.Forms.Label();
            this.labelOptionGrid = new System.Windows.Forms.Label();
            this.listBoxOptionGrid = new System.Windows.Forms.ListBox();
            this.labelReportAria5 = new System.Windows.Forms.Label();
            this.textBoxAria4XP = new System.Windows.Forms.TextBox();
            this.textBoxReportName = new System.Windows.Forms.TextBox();
            this.buttonAddToAria4 = new System.Windows.Forms.Button();
            this.buttonClose = new System.Windows.Forms.Button();
            this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
            this.SuspendLayout();
            // 
            // labelReportAria4XP
            // 
            this.labelReportAria4XP.AutoSize = true;
            this.labelReportAria4XP.Location = new System.Drawing.Point(20, 48);
            this.labelReportAria4XP.Name = "labelReportAria4XP";
            this.labelReportAria4XP.Size = new System.Drawing.Size(125, 13);
            this.labelReportAria4XP.TabIndex = 2;
            this.labelReportAria4XP.Text = "Report Name in Aria4XP:";
            // 
            // comboBoxReports
            // 
            this.comboBoxReports.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxReports.FormattingEnabled = true;
            this.comboBoxReports.Location = new System.Drawing.Point(155, 45);
            this.comboBoxReports.Name = "comboBoxReports";
            this.comboBoxReports.Size = new System.Drawing.Size(282, 21);
            this.comboBoxReports.TabIndex = 3;
            this.comboBoxReports.SelectedIndexChanged += new System.EventHandler(this.comboBoxReports_SelectedIndexChanged);
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
            // labelOptionGrid
            // 
            this.labelOptionGrid.AutoSize = true;
            this.labelOptionGrid.Location = new System.Drawing.Point(20, 104);
            this.labelOptionGrid.Name = "labelOptionGrid";
            this.labelOptionGrid.Size = new System.Drawing.Size(65, 13);
            this.labelOptionGrid.TabIndex = 7;
            this.labelOptionGrid.Text = "Option Grid:";
            // 
            // listBoxOptionGrid
            // 
            this.listBoxOptionGrid.FormattingEnabled = true;
            this.listBoxOptionGrid.Location = new System.Drawing.Point(160, 104);
            this.listBoxOptionGrid.Name = "listBoxOptionGrid";
            this.listBoxOptionGrid.Size = new System.Drawing.Size(277, 173);
            this.listBoxOptionGrid.TabIndex = 8;
            // 
            // labelReportAria5
            // 
            this.labelReportAria5.AutoSize = true;
            this.labelReportAria5.Location = new System.Drawing.Point(20, 74);
            this.labelReportAria5.Name = "labelReportAria5";
            this.labelReportAria5.Size = new System.Drawing.Size(113, 13);
            this.labelReportAria5.TabIndex = 4;
            this.labelReportAria5.Text = "Report Name in Aria5:";
            // 
            // textBoxAria4XP
            // 
            this.textBoxAria4XP.Location = new System.Drawing.Point(155, 71);
            this.textBoxAria4XP.Name = "textBoxAria4XP";
            this.textBoxAria4XP.ReadOnly = true;
            this.textBoxAria4XP.Size = new System.Drawing.Size(70, 20);
            this.textBoxAria4XP.TabIndex = 5;
            this.textBoxAria4XP.Text = "Aria4XP.";
            // 
            // textBoxReportName
            // 
            this.textBoxReportName.Location = new System.Drawing.Point(225, 71);
            this.textBoxReportName.MaxLength = 92;
            this.textBoxReportName.Name = "textBoxReportName";
            this.textBoxReportName.Size = new System.Drawing.Size(212, 20);
            this.textBoxReportName.TabIndex = 6;
            this.textBoxReportName.TextChanged += new System.EventHandler(this.textBoxReportName_TextChanged);
            // 
            // buttonAddToAria4
            // 
            this.buttonAddToAria4.Location = new System.Drawing.Point(282, 290);
            this.buttonAddToAria4.Name = "buttonAddToAria4";
            this.buttonAddToAria4.Size = new System.Drawing.Size(82, 24);
            this.buttonAddToAria4.TabIndex = 9;
            this.buttonAddToAria4.Text = "Convert";
            this.buttonAddToAria4.UseVisualStyleBackColor = true;
            this.buttonAddToAria4.Click += new System.EventHandler(this.buttonAddToAria4_Click);
            // 
            // buttonClose
            // 
            this.buttonClose.Location = new System.Drawing.Point(370, 290);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(67, 24);
            this.buttonClose.TabIndex = 11;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);
            // 
            // errorProvider
            // 
            this.errorProvider.ContainerControl = this;
            // 
            // FormAddCustomReportToDictionary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(457, 326);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.buttonAddToAria4);
            this.Controls.Add(this.textBoxReportName);
            this.Controls.Add(this.textBoxAria4XP);
            this.Controls.Add(this.labelReportAria5);
            this.Controls.Add(this.listBoxOptionGrid);
            this.Controls.Add(this.labelOptionGrid);
            this.Controls.Add(this.comboBoxCustomer);
            this.Controls.Add(this.labelCustomer);
            this.Controls.Add(this.comboBoxReports);
            this.Controls.Add(this.labelReportAria4XP);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormAddCustomReportToDictionary";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Add Custom Report to Aria5 Dictionary";
            this.Load += new System.EventHandler(this.FormConvertReport_Load);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelReportAria4XP;
        private System.Windows.Forms.ComboBox comboBoxReports;
        private System.Windows.Forms.ComboBox comboBoxCustomer;
        private System.Windows.Forms.Label labelCustomer;
        private System.Windows.Forms.Label labelOptionGrid;
        private System.Windows.Forms.ListBox listBoxOptionGrid;
        private System.Windows.Forms.Label labelReportAria5;
        private System.Windows.Forms.TextBox textBoxAria4XP;
        private System.Windows.Forms.TextBox textBoxReportName;
        private System.Windows.Forms.Button buttonAddToAria4;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.ErrorProvider errorProvider;
    }
}

