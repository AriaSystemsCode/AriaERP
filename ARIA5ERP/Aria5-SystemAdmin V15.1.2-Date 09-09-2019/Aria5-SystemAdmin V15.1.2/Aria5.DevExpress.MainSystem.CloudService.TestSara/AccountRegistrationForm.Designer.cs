namespace Aria5.DevExpress.MainSystem.CloudService.TestSara
{
    partial class AccountRegistrationForm
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
            this.buttonGetDemoCategories = new System.Windows.Forms.Button();
            this.buttonGetDemoGuide = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // buttonGetDemoCategories
            // 
            this.buttonGetDemoCategories.Location = new System.Drawing.Point(69, 67);
            this.buttonGetDemoCategories.Name = "buttonGetDemoCategories";
            this.buttonGetDemoCategories.Size = new System.Drawing.Size(137, 23);
            this.buttonGetDemoCategories.TabIndex = 0;
            this.buttonGetDemoCategories.Text = "Get Demo Categories";
            this.buttonGetDemoCategories.UseVisualStyleBackColor = true;
            this.buttonGetDemoCategories.Click += new System.EventHandler(this.buttonGetDemoCategories_Click);
            // 
            // buttonGetDemoGuide
            // 
            this.buttonGetDemoGuide.Location = new System.Drawing.Point(69, 165);
            this.buttonGetDemoGuide.Name = "buttonGetDemoGuide";
            this.buttonGetDemoGuide.Size = new System.Drawing.Size(137, 23);
            this.buttonGetDemoGuide.TabIndex = 1;
            this.buttonGetDemoGuide.Text = "Get Demo Guides";
            this.buttonGetDemoGuide.UseVisualStyleBackColor = true;
            this.buttonGetDemoGuide.Click += new System.EventHandler(this.buttonGetDemoGuide_Click);
            // 
            // AccountRegistrationForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(284, 261);
            this.Controls.Add(this.buttonGetDemoGuide);
            this.Controls.Add(this.buttonGetDemoCategories);
            this.Name = "AccountRegistrationForm";
            this.Text = "AccountRegistrationForm";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button buttonGetDemoCategories;
        private System.Windows.Forms.Button buttonGetDemoGuide;
    }
}