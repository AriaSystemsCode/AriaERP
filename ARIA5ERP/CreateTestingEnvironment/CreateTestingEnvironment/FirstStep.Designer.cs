namespace CreateTestingEnvironment
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
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.lblSelect = new System.Windows.Forms.Label();
            this.clientList = new System.Windows.Forms.DataGridView();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOK = new System.Windows.Forms.Button();
            this.btnFinish = new System.Windows.Forms.Button();
            this.btnSetting = new System.Windows.Forms.Button();
            this.prgbcopying = new System.Windows.Forms.ProgressBar();
            this.lblcopyfile = new System.Windows.Forms.Label();
            this.lblNote = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.clientList)).BeginInit();
            this.SuspendLayout();
            // 
            // lblSelect
            // 
            this.lblSelect.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.lblSelect.AutoSize = true;
            this.lblSelect.Font = new System.Drawing.Font("Arial", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblSelect.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblSelect.Location = new System.Drawing.Point(13, 11);
            this.lblSelect.Name = "lblSelect";
            this.lblSelect.Size = new System.Drawing.Size(410, 18);
            this.lblSelect.TabIndex = 0;
            this.lblSelect.Text = "Select the client(s) to create the testing environment for:";
            // 
            // clientList
            // 
            this.clientList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.clientList.BackgroundColor = System.Drawing.SystemColors.Info;
            this.clientList.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            dataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle1.SelectionBackColor = System.Drawing.Color.RoyalBlue;
            dataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.clientList.DefaultCellStyle = dataGridViewCellStyle1;
            this.clientList.Location = new System.Drawing.Point(15, 40);
            this.clientList.Name = "clientList";
            this.clientList.Size = new System.Drawing.Size(587, 364);
            this.clientList.TabIndex = 1;
            // 
            // btnCancel
            // 
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.Location = new System.Drawing.Point(354, 448);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(75, 23);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "&Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            // 
            // btnOK
            // 
            this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnOK.Location = new System.Drawing.Point(433, 448);
            this.btnOK.Name = "btnOK";
            this.btnOK.Size = new System.Drawing.Size(75, 23);
            this.btnOK.TabIndex = 3;
            this.btnOK.Text = "&Next";
            this.btnOK.UseVisualStyleBackColor = true;
            this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
            // 
            // btnFinish
            // 
            this.btnFinish.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnFinish.Enabled = false;
            this.btnFinish.Location = new System.Drawing.Point(512, 448);
            this.btnFinish.Name = "btnFinish";
            this.btnFinish.Size = new System.Drawing.Size(75, 23);
            this.btnFinish.TabIndex = 4;
            this.btnFinish.Text = "&Finish";
            this.btnFinish.UseVisualStyleBackColor = true;
            this.btnFinish.Click += new System.EventHandler(this.btnFinish_Click);
            // 
            // btnSetting
            // 
            this.btnSetting.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnSetting.Location = new System.Drawing.Point(527, 9);
            this.btnSetting.Name = "btnSetting";
            this.btnSetting.Size = new System.Drawing.Size(75, 23);
            this.btnSetting.TabIndex = 5;
            this.btnSetting.Text = "Settings";
            this.btnSetting.UseVisualStyleBackColor = true;
            this.btnSetting.Click += new System.EventHandler(this.btnSetting_Click);
            // 
            // prgbcopying
            // 
            this.prgbcopying.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.prgbcopying.Location = new System.Drawing.Point(16, 448);
            this.prgbcopying.Name = "prgbcopying";
            this.prgbcopying.Size = new System.Drawing.Size(332, 23);
            this.prgbcopying.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            this.prgbcopying.TabIndex = 6;
            // 
            // lblcopyfile
            // 
            this.lblcopyfile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.lblcopyfile.ForeColor = System.Drawing.Color.RoyalBlue;
            this.lblcopyfile.Location = new System.Drawing.Point(13, 432);
            this.lblcopyfile.Name = "lblcopyfile";
            this.lblcopyfile.Size = new System.Drawing.Size(335, 13);
            this.lblcopyfile.TabIndex = 7;
            this.lblcopyfile.Text = "Preparing....";
            // 
            // lblNote
            // 
            this.lblNote.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.lblNote.AutoSize = true;
            this.lblNote.ForeColor = System.Drawing.Color.ForestGreen;
            this.lblNote.Location = new System.Drawing.Point(18, 411);
            this.lblNote.Name = "lblNote";
            this.lblNote.Size = new System.Drawing.Size(247, 13);
            this.lblNote.TabIndex = 8;
            this.lblNote.Text = "* Green clients already have a testing environment ";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Info;
            this.ClientSize = new System.Drawing.Size(614, 483);
            this.Controls.Add(this.lblNote);
            this.Controls.Add(this.lblcopyfile);
            this.Controls.Add(this.prgbcopying);
            this.Controls.Add(this.btnSetting);
            this.Controls.Add(this.btnFinish);
            this.Controls.Add(this.btnOK);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.clientList);
            this.Controls.Add(this.lblSelect);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Form1";
            this.Text = "Create Testing Environment";
            this.Load += new System.EventHandler(this.Form1_Load);
            ((System.ComponentModel.ISupportInitialize)(this.clientList)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label lblSelect;
        private System.Windows.Forms.DataGridView clientList;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnOK;
        private System.Windows.Forms.Button btnFinish;
        private System.Windows.Forms.Button btnSetting;
        private System.Windows.Forms.ProgressBar prgbcopying;
        private System.Windows.Forms.Label lblcopyfile;
        private System.Windows.Forms.Label lblNote;
    }
}

