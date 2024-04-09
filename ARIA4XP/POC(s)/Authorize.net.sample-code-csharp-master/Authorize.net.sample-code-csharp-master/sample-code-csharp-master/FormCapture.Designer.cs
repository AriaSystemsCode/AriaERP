namespace net.authorize.sample
{
    partial class FormCapture
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
            this.buttonCapture = new System.Windows.Forms.Button();
            this.textBoxAmount = new System.Windows.Forms.TextBox();
            this.label12 = new System.Windows.Forms.Label();
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
            // buttonCapture
            // 
            this.buttonCapture.Location = new System.Drawing.Point(261, 38);
            this.buttonCapture.Name = "buttonCapture";
            this.buttonCapture.Size = new System.Drawing.Size(75, 23);
            this.buttonCapture.TabIndex = 22;
            this.buttonCapture.Text = "Capture";
            this.buttonCapture.UseVisualStyleBackColor = true;
            this.buttonCapture.Click += new System.EventHandler(this.buttonCapture_Click);
            // 
            // textBoxAmount
            // 
            this.textBoxAmount.Location = new System.Drawing.Point(132, 40);
            this.textBoxAmount.Name = "textBoxAmount";
            this.textBoxAmount.Size = new System.Drawing.Size(60, 20);
            this.textBoxAmount.TabIndex = 27;
            this.textBoxAmount.Text = "30.00";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(19, 43);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(48, 13);
            this.label12.TabIndex = 26;
            this.label12.Text = "Amount:";
            // 
            // FormCapture
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(356, 76);
            this.Controls.Add(this.textBoxAmount);
            this.Controls.Add(this.label12);
            this.Controls.Add(this.textBoxTransactionId);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.buttonCapture);
            this.Name = "FormCapture";
            this.Text = "Capture Transaction";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox textBoxTransactionId;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Button buttonCapture;
        private System.Windows.Forms.TextBox textBoxAmount;
        private System.Windows.Forms.Label label12;
    }
}