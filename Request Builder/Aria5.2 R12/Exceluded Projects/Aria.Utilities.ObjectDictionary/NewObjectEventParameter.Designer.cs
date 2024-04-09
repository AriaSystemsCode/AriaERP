namespace Aria.Utilities.ObjectDictionary
{
    partial class NewObjectEventParameter
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
            this.cmdCancel = new System.Windows.Forms.Button();
            this.cmdOK = new System.Windows.Forms.Button();
            this.txtEventParameter = new System.Windows.Forms.TextBox();
            this.lblEventParameter = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // cmdCancel
            // 
            this.cmdCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cmdCancel.Location = new System.Drawing.Point(276, 51);
            this.cmdCancel.Name = "cmdCancel";
            this.cmdCancel.Size = new System.Drawing.Size(77, 24);
            this.cmdCancel.TabIndex = 11;
            this.cmdCancel.Text = "Cancel";
            this.cmdCancel.UseVisualStyleBackColor = true;
            // 
            // cmdOK
            // 
            this.cmdOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.cmdOK.Location = new System.Drawing.Point(194, 51);
            this.cmdOK.Name = "cmdOK";
            this.cmdOK.Size = new System.Drawing.Size(77, 24);
            this.cmdOK.TabIndex = 10;
            this.cmdOK.Text = "OK";
            this.cmdOK.UseVisualStyleBackColor = true;
            this.cmdOK.Click += new System.EventHandler(this.cmdOK_Click);
            // 
            // txtEventParameter
            // 
            this.txtEventParameter.Location = new System.Drawing.Point(120, 15);
            this.txtEventParameter.Name = "txtEventParameter";
            this.txtEventParameter.Size = new System.Drawing.Size(233, 20);
            this.txtEventParameter.TabIndex = 7;
            this.txtEventParameter.Text = "Pointer";
            // 
            // lblEventParameter
            // 
            this.lblEventParameter.AutoSize = true;
            this.lblEventParameter.Location = new System.Drawing.Point(2, 18);
            this.lblEventParameter.Name = "lblEventParameter";
            this.lblEventParameter.Size = new System.Drawing.Size(95, 13);
            this.lblEventParameter.TabIndex = 6;
            this.lblEventParameter.Text = "Event Parameter :";
            // 
            // NewObjectEventParameter
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(365, 91);
            this.Controls.Add(this.cmdCancel);
            this.Controls.Add(this.cmdOK);
            this.Controls.Add(this.txtEventParameter);
            this.Controls.Add(this.lblEventParameter);
            this.Name = "NewObjectEventParameter";
            this.Text = "New Object Event Parameter";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button cmdCancel;
        private System.Windows.Forms.Button cmdOK;
        public System.Windows.Forms.TextBox txtEventParameter;
        private System.Windows.Forms.Label lblEventParameter;

    }
}