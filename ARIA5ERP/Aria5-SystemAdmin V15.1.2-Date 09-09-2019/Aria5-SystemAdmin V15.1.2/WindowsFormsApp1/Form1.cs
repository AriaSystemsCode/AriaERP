using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApp1
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
          
            string BNUM = "20";
            string ProjectName = "a30";
            string sourcepath = "D:\\Tracking";
            string destPath = "D:\\Tracking\\BUILDS\\BD" + BNUM + ProjectName + " ";
            string BuildNUM = "BD" + BNUM + ProjectName;
           
            List<string> TNUMBERS = new List<string>();
            TNUMBERS.Add("E6423");
            TNUMBERS.Add("B6420");

            foreach (var TNUM in TNUMBERS)
            {
                foreach (string dirPath in Directory.GetDirectories(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", "*",
                   SearchOption.AllDirectories))
                    Directory.CreateDirectory(dirPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", destPath + "\\" + "Attachments\\"));

                //Copy all the files & Replaces any files with the same name
                foreach (string newPath in Directory.GetFiles(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", "*.*",
                    SearchOption.AllDirectories))
                    File.Copy(newPath, newPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", destPath + "\\" + "Attachments\\"), true);
            }

        }

        private void CopyFolder(string sourceFolder, string destFolder,string TNUM,string BuildNUM)
        {
            if (!Directory.Exists(destFolder))
                Directory.CreateDirectory(destFolder);
            string[] files = Directory.GetFiles(sourceFolder);
         
            
            foreach (string file in files)
            {
                string name = Path.GetFileName(file);
                string dest = Path.Combine(destFolder, name);
                File.Copy(file, dest,true);  
            }
            string[] SourceSubfolders = Directory.GetDirectories(sourceFolder);
            string[] DestSubfolders = Directory.GetDirectories(sourceFolder);
            for (int i = 0; i < SourceSubfolders.Length; i++)
            {
                DestSubfolders[i] = SourceSubfolders[i].Replace("Fixes", "Builds");
                DestSubfolders[i] = SourceSubfolders[i].Replace(TNUM, BuildNUM);
            }
            foreach (string item in DestSubfolders)
            {
                    if (!Directory.Exists(item))
                    Directory.CreateDirectory(item);
            }
            foreach (string folder in SourceSubfolders)
            {
                string name = Path.GetFileName(folder);
                string dest = Path.Combine(destFolder, name);
                CopyFolder(folder, dest, TNUM, BuildNUM);
            }
        }
    }
    }

