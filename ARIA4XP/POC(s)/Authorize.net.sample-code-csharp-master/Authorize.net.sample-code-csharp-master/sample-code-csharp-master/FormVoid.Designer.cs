namespace net.authorize.sample
{
    partial class FormVoid
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
            this.textBoxTransactionId = new System.Windows.Forms.TextBox();
            this.label13 = new System.Windows.Forms.Label();
            this.buttonViod = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // textBoxTransactionId
            // 
            this.textBoxTransactionId.Location = new System.Drawing.Point(132, 12);
            this.textBoxTransactionId.Name = "textBoxTransactionId";
            this.textBoxTransactionId.Size = new System.Drawing.Size(204, 20);
            this.textBoxTransactionId.TabIndex = 21;
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(19, 13);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(80, 13);
            this.label13.TabIndex = 20;
            this.label13.Text = "Transaction Id:";
            // 
            // buttonViod
            // 
            this.buttonViod.Location = new System.Drawing.Point(261, 38);
            this.buttonViod.Name = "buttonViod";
            this.buttonViod.Size = new System.Drawing.Size(75, 23);
            this.buttonViod.TabIndex = 22;
            this.buttonViod.Text = "Void";
            this.buttonViod.UseVisualStyleBackColor = true;
            this.buttonViod.Click += new System.EventHandler(this.buttonCapture_Click);
            // 
            // FormVoid
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(356, 76);
            this.Controls.Add(this.textBoxTransactionId);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.buttonViod);
            this.Name = "FormVoid";
            this.Text = "Capture Transaction";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox textBoxTransactionId;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Button buttonViod;
    }
}