namespace Aria.Utilities.Dictionary
{
    partial class FormAddFieldToDictionary
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormAddFieldToDictionary));
            this.labelObjectName = new System.Windows.Forms.Label();
            this.comboBoxObjectName = new System.Windows.Forms.ComboBox();
            this.comboBoxCustomer = new System.Windows.Forms.ComboBox();
            this.labelCustomer = new System.Windows.Forms.Label();
            this.labelFieldNameAria5 = new System.Windows.Forms.Label();
            this.textBoxPropertyName = new System.Windows.Forms.TextBox();
            this.buttonAddToAria4 = new System.Windows.Forms.Button();
            this.buttonClose = new System.Windows.Forms.Button();
            this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
            this.comboBoxFields = new System.Windows.Forms.ComboBox();
            this.labelFieldName = new System.Windows.Forms.Label();
            this.textBoxValidEntryAria5 = new System.Windows.Forms.TextBox();
            this.labelFieldValidEntryAria5 = new System.Windows.Forms.Label();
            this.labelFieldIsEmailAria5 = new System.Windows.Forms.Label();
            this.checkBoxIsEmailAria5 = new System.Windows.Forms.CheckBox();
            this.checkBoxIsInternalEmailAria5 = new System.Windows.Forms.CheckBox();
            this.labelIsInternalEmailAria5 = new System.Windows.Forms.Label();
            this.textBoxInternalEmailConnectionTypeAria5 = new System.Windows.Forms.TextBox();
            this.LabelInternalEmailConnectionType = new System.Windows.Forms.Label();
            this.textBoxInternalEmailParameterAria5 = new System.Windows.Forms.TextBox();
            this.labelInternalEmailParameterAria5 = new System.Windows.Forms.Label();
            this.textBoxInternalEmailSelectAria5 = new System.Windows.Forms.TextBox();
            this.labelInternalEmailSelectAria5 = new System.Windows.Forms.Label();
            this.textBoxPropertyDescription = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.checkBoxIsPrimaryKeyAria5 = new System.Windows.Forms.CheckBox();
            this.labelPropertyIsKeyAria5 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
            this.SuspendLayout();
            // 
            // labelObjectName
            // 
            this.labelObjectName.AutoSize = true;
            this.labelObjectName.Location = new System.Drawing.Point(20, 48);
            this.labelObjectName.Name = "labelObjectName";
            this.labelObjectName.Size = new System.Drawing.Size(73, 13);
            this.labelObjectName.TabIndex = 2;
            this.labelObjectName.Text = "Object Name:";
            // 
            // comboBoxObjectName
            // 
            this.comboBoxObjectName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxObjectName.FormattingEnabled = true;
            this.comboBoxObjectName.Location = new System.Drawing.Point(282, 45);
            this.comboBoxObjectName.Name = "comboBoxObjectName";
            this.comboBoxObjectName.Size = new System.Drawing.Size(245, 21);
            this.comboBoxObjectName.TabIndex = 4;
            this.comboBoxObjectName.SelectedIndexChanged += new System.EventHandler(this.comboBoxObjectNames_SelectedIndexChanged);
            // 
            // comboBoxCustomer
            // 
            this.comboBoxCustomer.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxCustomer.FormattingEnabled = true;
            this.comboBoxCustomer.Location = new System.Drawing.Point(282, 18);
            this.comboBoxCustomer.Name = "comboBoxCustomer";
            this.comboBoxCustomer.Size = new System.Drawing.Size(245, 21);
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
            // labelFieldNameAria5
            // 
            this.labelFieldNameAria5.AutoSize = true;
            this.labelFieldNameAria5.Location = new System.Drawing.Point(20, 122);
            this.labelFieldNameAria5.Name = "labelFieldNameAria5";
            this.labelFieldNameAria5.Size = new System.Drawing.Size(122, 13);
            this.labelFieldNameAria5.TabIndex = 9;
            this.labelFieldNameAria5.Text = "Property Name in Aria5:";
            // 
            // textBoxPropertyName
            // 
            this.textBoxPropertyName.Location = new System.Drawing.Point(282, 119);
            this.textBoxPropertyName.MaxLength = 92;
            this.textBoxPropertyName.Name = "textBoxPropertyName";
            this.textBoxPropertyName.Size = new System.Drawing.Size(245, 20);
            this.textBoxPropertyName.TabIndex = 10;
            this.textBoxPropertyName.TextChanged += new System.EventHandler(this.textBoxPropertyName_TextChanged);
            // 
            // buttonAddToAria4
            // 
            this.buttonAddToAria4.Location = new System.Drawing.Point(372, 333);
            this.buttonAddToAria4.Name = "buttonAddToAria4";
            this.buttonAddToAria4.Size = new System.Drawing.Size(82, 24);
            this.buttonAddToAria4.TabIndex = 25;
            this.buttonAddToAria4.Text = "Convert";
            this.buttonAddToAria4.UseVisualStyleBackColor = true;
            this.buttonAddToAria4.Click += new System.EventHandler(this.buttonAddToAria4_Click);
            // 
            // buttonClose
            // 
            this.buttonClose.Location = new System.Drawing.Point(460, 333);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(67, 24);
            this.buttonClose.TabIndex = 26;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);
            // 
            // errorProvider
            // 
            this.errorProvider.ContainerControl = this;
            // 
            // comboBoxFields
            // 
            this.comboBoxFields.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxFields.FormattingEnabled = true;
            this.comboBoxFields.Location = new System.Drawing.Point(282, 72);
            this.comboBoxFields.Name = "comboBoxFields";
            this.comboBoxFields.Size = new System.Drawing.Size(245, 21);
            this.comboBoxFields.TabIndex = 6;
            this.comboBoxFields.SelectedIndexChanged += new System.EventHandler(this.comboBoxFields_SelectedIndexChanged);
            // 
            // labelFieldName
            // 
            this.labelFieldName.AutoSize = true;
            this.labelFieldName.Location = new System.Drawing.Point(20, 75);
            this.labelFieldName.Name = "labelFieldName";
            this.labelFieldName.Size = new System.Drawing.Size(74, 13);
            this.labelFieldName.TabIndex = 5;
            this.labelFieldName.Text = "Field Name in:";
            // 
            // textBoxValidEntryAria5
            // 
            this.textBoxValidEntryAria5.Location = new System.Drawing.Point(282, 171);
            this.textBoxValidEntryAria5.Name = "textBoxValidEntryAria5";
            this.textBoxValidEntryAria5.Size = new System.Drawing.Size(245, 20);
            this.textBoxValidEntryAria5.TabIndex = 14;
            // 
            // labelFieldValidEntryAria5
            // 
            this.labelFieldValidEntryAria5.AutoSize = true;
            this.labelFieldValidEntryAria5.Location = new System.Drawing.Point(20, 174);
            this.labelFieldValidEntryAria5.Name = "labelFieldValidEntryAria5";
            this.labelFieldValidEntryAria5.Size = new System.Drawing.Size(146, 13);
            this.labelFieldValidEntryAria5.TabIndex = 13;
            this.labelFieldValidEntryAria5.Text = "Property Valid Entry in Aria5:";
            // 
            // labelFieldIsEmailAria5
            // 
            this.labelFieldIsEmailAria5.AutoSize = true;
            this.labelFieldIsEmailAria5.Location = new System.Drawing.Point(20, 202);
            this.labelFieldIsEmailAria5.Name = "labelFieldIsEmailAria5";
            this.labelFieldIsEmailAria5.Size = new System.Drawing.Size(131, 13);
            this.labelFieldIsEmailAria5.TabIndex = 15;
            this.labelFieldIsEmailAria5.Text = "Property Is Email in Aria5:";
            // 
            // checkBoxIsEmailAria5
            // 
            this.checkBoxIsEmailAria5.AutoSize = true;
            this.checkBoxIsEmailAria5.Location = new System.Drawing.Point(282, 202);
            this.checkBoxIsEmailAria5.Name = "checkBoxIsEmailAria5";
            this.checkBoxIsEmailAria5.Size = new System.Drawing.Size(15, 14);
            this.checkBoxIsEmailAria5.TabIndex = 16;
            this.checkBoxIsEmailAria5.UseVisualStyleBackColor = true;
            // 
            // checkBoxIsInternalEmailAria5
            // 
            this.checkBoxIsInternalEmailAria5.AutoSize = true;
            this.checkBoxIsInternalEmailAria5.Location = new System.Drawing.Point(282, 227);
            this.checkBoxIsInternalEmailAria5.Name = "checkBoxIsInternalEmailAria5";
            this.checkBoxIsInternalEmailAria5.Size = new System.Drawing.Size(15, 14);
            this.checkBoxIsInternalEmailAria5.TabIndex = 18;
            this.checkBoxIsInternalEmailAria5.UseVisualStyleBackColor = true;
            // 
            // labelIsInternalEmailAria5
            // 
            this.labelIsInternalEmailAria5.AutoSize = true;
            this.labelIsInternalEmailAria5.Location = new System.Drawing.Point(20, 227);
            this.labelIsInternalEmailAria5.Name = "labelIsInternalEmailAria5";
            this.labelIsInternalEmailAria5.Size = new System.Drawing.Size(172, 13);
            this.labelIsInternalEmailAria5.TabIndex = 17;
            this.labelIsInternalEmailAria5.Text = "Property Is Internal Email in Aria5:";
            // 
            // textBoxInternalEmailConnectionTypeAria5
            // 
            this.textBoxInternalEmailConnectionTypeAria5.Location = new System.Drawing.Point(282, 247);
            this.textBoxInternalEmailConnectionTypeAria5.Name = "textBoxInternalEmailConnectionTypeAria5";
            this.textBoxInternalEmailConnectionTypeAria5.Size = new System.Drawing.Size(245, 20);
            this.textBoxInternalEmailConnectionTypeAria5.TabIndex = 20;
            // 
            // LabelInternalEmailConnectionType
            // 
            this.LabelInternalEmailConnectionType.AutoSize = true;
            this.LabelInternalEmailConnectionType.Location = new System.Drawing.Point(20, 250);
            this.LabelInternalEmailConnectionType.Name = "LabelInternalEmailConnectionType";
            this.LabelInternalEmailConnectionType.Size = new System.Drawing.Size(241, 13);
            this.LabelInternalEmailConnectionType.TabIndex = 19;
            this.LabelInternalEmailConnectionType.Text = "Property InternalEmail Connection Type in Aria5:";
            // 
            // textBoxInternalEmailParameterAria5
            // 
            this.textBoxInternalEmailParameterAria5.Location = new System.Drawing.Point(282, 273);
            this.textBoxInternalEmailParameterAria5.Name = "textBoxInternalEmailParameterAria5";
            this.textBoxInternalEmailParameterAria5.Size = new System.Drawing.Size(245, 20);
            this.textBoxInternalEmailParameterAria5.TabIndex = 22;
            // 
            // labelInternalEmailParameterAria5
            // 
            this.labelInternalEmailParameterAria5.AutoSize = true;
            this.labelInternalEmailParameterAria5.Location = new System.Drawing.Point(20, 276);
            this.labelInternalEmailParameterAria5.Name = "labelInternalEmailParameterAria5";
            this.labelInternalEmailParameterAria5.Size = new System.Drawing.Size(202, 13);
            this.labelInternalEmailParameterAria5.TabIndex = 21;
            this.labelInternalEmailParameterAria5.Text = "Property Internal Email Parameter Aria5:";
            // 
            // textBoxInternalEmailSelectAria5
            // 
            this.textBoxInternalEmailSelectAria5.Location = new System.Drawing.Point(282, 299);
            this.textBoxInternalEmailSelectAria5.Name = "textBoxInternalEmailSelectAria5";
            this.textBoxInternalEmailSelectAria5.Size = new System.Drawing.Size(245, 20);
            this.textBoxInternalEmailSelectAria5.TabIndex = 24;
            // 
            // labelInternalEmailSelectAria5
            // 
            this.labelInternalEmailSelectAria5.AutoSize = true;
            this.labelInternalEmailSelectAria5.Location = new System.Drawing.Point(20, 302);
            this.labelInternalEmailSelectAria5.Name = "labelInternalEmailSelectAria5";
            this.labelInternalEmailSelectAria5.Size = new System.Drawing.Size(181, 13);
            this.labelInternalEmailSelectAria5.TabIndex = 23;
            this.labelInternalEmailSelectAria5.Text = "Property Internal Email Select Aria5:";
            // 
            // textBoxPropertyDescription
            // 
            this.textBoxPropertyDescription.Location = new System.Drawing.Point(282, 145);
            this.textBoxPropertyDescription.MaxLength = 92;
            this.textBoxPropertyDescription.Name = "textBoxPropertyDescription";
            this.textBoxPropertyDescription.Size = new System.Drawing.Size(245, 20);
            this.textBoxPropertyDescription.TabIndex = 12;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 148);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(148, 13);
            this.label1.TabIndex = 11;
            this.label1.Text = "Property Description in Aria5:";
            // 
            // checkBoxIsPrimaryKeyAria5
            // 
            this.checkBoxIsPrimaryKeyAria5.AutoSize = true;
            this.checkBoxIsPrimaryKeyAria5.Location = new System.Drawing.Point(282, 100);
            this.checkBoxIsPrimaryKeyAria5.Name = "checkBoxIsPrimaryKeyAria5";
            this.checkBoxIsPrimaryKeyAria5.Size = new System.Drawing.Size(15, 14);
            this.checkBoxIsPrimaryKeyAria5.TabIndex = 8;
            this.checkBoxIsPrimaryKeyAria5.UseVisualStyleBackColor = true;
            // 
            // labelPropertyIsKeyAria5
            // 
            this.labelPropertyIsKeyAria5.AutoSize = true;
            this.labelPropertyIsKeyAria5.Location = new System.Drawing.Point(20, 100);
            this.labelPropertyIsKeyAria5.Name = "labelPropertyIsKeyAria5";
            this.labelPropertyIsKeyAria5.Size = new System.Drawing.Size(164, 13);
            this.labelPropertyIsKeyAria5.TabIndex = 7;
            this.labelPropertyIsKeyAria5.Text = "Property Is Primary Key in Aria5:";
            // 
            // FormAddFieldToDictionary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(547, 366);
            this.Controls.Add(this.checkBoxIsPrimaryKeyAria5);
            this.Controls.Add(this.labelPropertyIsKeyAria5);
            this.Controls.Add(this.textBoxPropertyDescription);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBoxInternalEmailSelectAria5);
            this.Controls.Add(this.labelInternalEmailSelectAria5);
            this.Controls.Add(this.textBoxInternalEmailParameterAria5);
            this.Controls.Add(this.labelInternalEmailParameterAria5);
            this.Controls.Add(this.textBoxInternalEmailConnectionTypeAria5);
            this.Controls.Add(this.LabelInternalEmailConnectionType);
            this.Controls.Add(this.checkBoxIsInternalEmailAria5);
            this.Controls.Add(this.labelIsInternalEmailAria5);
            this.Controls.Add(this.checkBoxIsEmailAria5);
            this.Controls.Add(this.labelFieldIsEmailAria5);
            this.Controls.Add(this.textBoxValidEntryAria5);
            this.Controls.Add(this.labelFieldValidEntryAria5);
            this.Controls.Add(this.comboBoxFields);
            this.Controls.Add(this.labelFieldName);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.buttonAddToAria4);
            this.Controls.Add(this.textBoxPropertyName);
            this.Controls.Add(this.labelFieldNameAria5);
            this.Controls.Add(this.comboBoxCustomer);
            this.Controls.Add(this.labelCustomer);
            this.Controls.Add(this.comboBoxObjectName);
            this.Controls.Add(this.labelObjectName);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormAddFieldToDictionary";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Add Field to Aria5 Dictionary";
            this.Load += new System.EventHandler(this.FormConvertProperty_Load);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelObjectName;
        private System.Windows.Forms.ComboBox comboBoxObjectName;
        private System.Windows.Forms.ComboBox comboBoxCustomer;
        private System.Windows.Forms.Label labelCustomer;
        private System.Windows.Forms.Label labelFieldNameAria5;
        private System.Windows.Forms.TextBox textBoxPropertyName;
        private System.Windows.Forms.Button buttonAddToAria4;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.ErrorProvider errorProvider;
        private System.Windows.Forms.ComboBox comboBoxFields;
        private System.Windows.Forms.Label labelFieldName;
        private System.Windows.Forms.TextBox textBoxValidEntryAria5;
        private System.Windows.Forms.Label labelFieldValidEntryAria5;
        private System.Windows.Forms.Label labelFieldIsEmailAria5;
        private System.Windows.Forms.CheckBox checkBoxIsEmailAria5;
        private System.Windows.Forms.CheckBox checkBoxIsInternalEmailAria5;
        private System.Windows.Forms.Label labelIsInternalEmailAria5;
        private System.Windows.Forms.TextBox textBoxInternalEmailSelectAria5;
        private System.Windows.Forms.Label labelInternalEmailSelectAria5;
        private System.Windows.Forms.TextBox textBoxInternalEmailParameterAria5;
        private System.Windows.Forms.Label labelInternalEmailParameterAria5;
        private System.Windows.Forms.TextBox textBoxInternalEmailConnectionTypeAria5;
        private System.Windows.Forms.Label LabelInternalEmailConnectionType;
        private System.Windows.Forms.TextBox textBoxPropertyDescription;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox checkBoxIsPrimaryKeyAria5;
        private System.Windows.Forms.Label labelPropertyIsKeyAria5;
    }
}

