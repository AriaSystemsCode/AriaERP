namespace Aria.Utilities.Aria40Converter
{
    partial class FormAriaObjectProperty
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
            this.button1 = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.textBox2 = new System.Windows.Forms.TextBox();
            this.dataGridView1 = new System.Windows.Forms.DataGridView();
            this.Select = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.ObjectName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.OrgObjectPropertyName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.NewObjectPropertyName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.FieldName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.NotAsScreen = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.WhyNotAsScreen = new System.Windows.Forms.DataGridViewComboBoxColumn();
            this.MissingRelation = new System.Windows.Forms.DataGridViewCheckBoxColumn();
            this.Comment = new System.Windows.Forms.DataGridViewTextBoxColumn();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
            this.SuspendLayout();
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(764, 328);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 0;
            this.button1.Text = "Save";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(860, 328);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(75, 23);
            this.button2.TabIndex = 1;
            this.button2.Text = "Cancel";
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(13, 13);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(73, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Object Name:";
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(92, 10);
            this.textBox1.Name = "textBox1";
            this.textBox1.Size = new System.Drawing.Size(247, 20);
            this.textBox1.TabIndex = 4;
            this.textBox1.TextChanged += new System.EventHandler(this.textBox1_TextChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(360, 13);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(63, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "Field Name:";
            // 
            // textBox2
            // 
            this.textBox2.Location = new System.Drawing.Point(449, 10);
            this.textBox2.Name = "textBox2";
            this.textBox2.Size = new System.Drawing.Size(97, 20);
            this.textBox2.TabIndex = 6;
            this.textBox2.TextChanged += new System.EventHandler(this.textBox2_TextChanged);
            // 
            // dataGridView1
            // 
            this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Select,
            this.ObjectName,
            this.OrgObjectPropertyName,
            this.NewObjectPropertyName,
            this.FieldName,
            this.NotAsScreen,
            this.WhyNotAsScreen,
            this.MissingRelation,
            this.Comment});
            this.dataGridView1.Location = new System.Drawing.Point(23, 47);
            this.dataGridView1.Name = "dataGridView1";
            this.dataGridView1.Size = new System.Drawing.Size(901, 268);
            this.dataGridView1.TabIndex = 7;
            // 
            // Select
            // 
            this.Select.DataPropertyName = "Select";
            this.Select.HeaderText = "Select";
            this.Select.Name = "Select";
            this.Select.Width = 42;
            // 
            // ObjectName
            // 
            this.ObjectName.DataPropertyName = "ObjectName";
            this.ObjectName.HeaderText = "ObjectName";
            this.ObjectName.Name = "ObjectName";
            this.ObjectName.Width = 91;
            // 
            // OrgObjectPropertyName
            // 
            this.OrgObjectPropertyName.DataPropertyName = "OrgObjectPropertyName";
            this.OrgObjectPropertyName.HeaderText = "OrgObjectPropertyName";
            this.OrgObjectPropertyName.Name = "OrgObjectPropertyName";
            this.OrgObjectPropertyName.Width = 151;
            // 
            // NewObjectPropertyName
            // 
            this.NewObjectPropertyName.DataPropertyName = "NewObjectPropertyName";
            this.NewObjectPropertyName.HeaderText = "NewObjectPropertyName";
            this.NewObjectPropertyName.Name = "NewObjectPropertyName";
            this.NewObjectPropertyName.Width = 154;
            // 
            // FieldName
            // 
            this.FieldName.DataPropertyName = "FieldName";
            this.FieldName.HeaderText = "FieldName";
            this.FieldName.Name = "FieldName";
            this.FieldName.Width = 81;
            // 
            // NotAsScreen
            // 
            this.NotAsScreen.DataPropertyName = "NotAsScreen";
            this.NotAsScreen.HeaderText = "NotAsScreen";
            this.NotAsScreen.Name = "NotAsScreen";
            this.NotAsScreen.Width = 75;
            // 
            // WhyNotAsScreen
            // 
            this.WhyNotAsScreen.DataPropertyName = "WhyNotAsScreen";
            this.WhyNotAsScreen.HeaderText = "WhyNotAsScreen";
            this.WhyNotAsScreen.Items.AddRange(new object[] {
            "",
            "Not Exist",
            "Doublicated",
            "Serial",
            "Follow",
            "Correct"});
            this.WhyNotAsScreen.Name = "WhyNotAsScreen";
            this.WhyNotAsScreen.Width = 97;
            // 
            // MissingRelation
            // 
            this.MissingRelation.DataPropertyName = "MissingRelation";
            this.MissingRelation.HeaderText = "MissingRelation";
            this.MissingRelation.Name = "MissingRelation";
            this.MissingRelation.Width = 86;
            // 
            // Comment
            // 
            this.Comment.DataPropertyName = "Comment";
            this.Comment.HeaderText = "Comment";
            this.Comment.Name = "Comment";
            this.Comment.Width = 77;
            // 
            // FormAriaObjectProperty
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(947, 363);
            this.Controls.Add(this.dataGridView1);
            this.Controls.Add(this.textBox2);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.textBox1);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.button1);
            this.Name = "FormAriaObjectProperty";
            this.Text = "FormAriaObjectProperty";
            this.Load += new System.EventHandler(this.FormAriaObjectProperty_Load);
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBox1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox textBox2;
        private System.Windows.Forms.DataGridView dataGridView1;
        private System.Windows.Forms.DataGridViewCheckBoxColumn Select;
        private System.Windows.Forms.DataGridViewTextBoxColumn ObjectName;
        private System.Windows.Forms.DataGridViewTextBoxColumn OrgObjectPropertyName;
        private System.Windows.Forms.DataGridViewTextBoxColumn NewObjectPropertyName;
        private System.Windows.Forms.DataGridViewTextBoxColumn FieldName;
        private System.Windows.Forms.DataGridViewCheckBoxColumn NotAsScreen;
        private System.Windows.Forms.DataGridViewComboBoxColumn WhyNotAsScreen;
        private System.Windows.Forms.DataGridViewCheckBoxColumn MissingRelation;
        private System.Windows.Forms.DataGridViewTextBoxColumn Comment;
    }
}