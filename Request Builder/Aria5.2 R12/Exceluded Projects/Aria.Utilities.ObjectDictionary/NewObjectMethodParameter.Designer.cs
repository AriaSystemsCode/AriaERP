namespace Aria.Utilities.ObjectDictionary
{
    partial class NewObjectMethodParameter
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
            this.txtMethodParameter = new System.Windows.Forms.TextBox();
            this.lblMethodParameter = new System.Windows.Forms.Label();
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
            // txtMethodParameter
            // 
            this.txtMethodParameter.Location = new System.Drawing.Point(120, 15);
            this.txtMethodParameter.Name = "txtMethodParameter";
            this.txtMethodParameter.Size = new System.Drawing.Size(233, 20);
            this.txtMethodParameter.TabIndex = 7;
            this.txtMethodParameter.Text = "OptionGrid";
            // 
            // lblMethodParameter
            // 
            this.lblMethodParameter.AutoSize = true;
            this.lblMethodParameter.Location = new System.Drawing.Point(2, 18);
            this.lblMethodParameter.Name = "lblMethodParameter";
            this.lblMethodParameter.Size = new System.Drawing.Size(103, 13);
            this.lblMethodParameter.TabIndex = 6;
            this.lblMethodParameter.Text = "Method Parameter :";
            // 
            // NewObjectMethodParameter
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(365, 91);
            this.Controls.Add(this.cmdCancel);
            this.Controls.Add(this.cmdOK);
            this.Controls.Add(this.txtMethodParameter);
            this.Controls.Add(this.lblMethodParameter);
            this.Name = "NewObjectMethodParameter";
            this.Text = "New Object Method Parameter";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button cmdCancel;
        private System.Windows.Forms.Button cmdOK;
        public System.Windows.Forms.TextBox txtMethodParameter;
        private System.Windows.Forms.Label lblMethodParameter;

    }
}