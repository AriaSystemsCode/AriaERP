using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using AriaObjectBrowser.DataTypes;
using System.Runtime.InteropServices;
using System.IO;
using TaskPaneControl;

namespace AriaObjectBrowser.UserControls
{
    public partial class AriaThumbnailControl : UserControl
    {
        #region Win32 declarations
        private const uint SHGFI_ICON = 0x100;
        private const uint SHGFI_LARGEICON = 0x0;
        private const uint SHGFI_SMALLICON = 0x1;

        [StructLayout(LayoutKind.Sequential)]
        public struct SHFILEINFO
        {
            public IntPtr hIcon;
            public IntPtr iIcon;
            public uint dwAttributes;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 260)]
            public string szDisplayName;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 80)]
            public string szTypeName;
        };

        [DllImport("shell32.dll")]
        public static extern IntPtr SHGetFileInfo(string pszPath, uint dwFileAttributes, ref SHFILEINFO psfi, uint cbSizeFileInfo, uint uFlags);
        #endregion

        private AriaObjectList _sources;
        private int _index;
        private AriaObject _source;

        public AriaThumbnailControl()
        {
            InitializeComponent();

            fileaxGdViewer.SetLicenseNumber("4690766445560616550501148");
            // MOH T20100331.0004   Objects not deleting from objects directory start
            fileaxGdViewer.ImageForceTemporaryMode = true;
            // MOH T20100331.0004   Objects not deleting from objects directory start
            Resize += new EventHandler(AriaThumbnailControl_Resize);
        }

        private AriaSettings _settings;
        public AriaSettings Settings
        {
            get { return _settings; }
            set 
            {
                _settings = value;
                if (_settings == null) return;

                _settings.Changed += new AriaSettingChangedEventHandler(AriaSettings_Changed);
                _settings.RefreshAll += new AriaSettingRefreshAllHandler(AriaSettings_RefreshAll);
            }
        }

        private object _ariaApplication;
        public object AriaApplication
        {
            get { return _ariaApplication; }
            set { _ariaApplication = value; }
        }

        public AriaThumbnailControl(object ariaApplication, AriaSettings settings, AriaObjectList sources, int index)
            : this()
        {

            _ariaApplication = ariaApplication;
            _sources = sources;
            _index = index;
            _source = sources[index];

            Settings = settings;

            _sources.ObjectDeleted += new AriaDeleteObjectChangedEventHandler(_sources_ObjectDeleted);
            _sources.DefaultObjectChanged += new AriaDefaultObjectChangedEventHandler(_sources_DefaultObjectChanged);
            _sources.ActiveObjectChanged += new AriaActiveObjectChangedEventHandler(_sources_ActiveObjectChanged);
            _source.Changed += new AriaObjectChangedEventHandler(_source_Changed);

            _source_Changed();
        }

        void AriaSettings_Changed()
        {
            AriaThumbnailControl_Resize(null, null);
        }

        void AriaSettings_RefreshAll()
        {
            _source_Changed();
        }

        void _sources_ObjectDeleted(int index)
        {
            if(index <= _index) _index--;
        }

        void _sources_DefaultObjectChanged()
        {
            AriaThumbnailControl_Resize(null, null);
        }

        void _sources_ActiveObjectChanged()
        {
            AriaThumbnailControl_Resize(null, null);
        }

        void _source_Changed()
        {
            filelabel.Text = _source.Name;

            if (File.Exists(_source.File.FullName))
            {
                fileaxGdViewer.DisplayFromFile(_source.File.FullName);
            }
            else
            {
                InterOper interOper = new InterOper();
                fileaxGdViewer.DisplayFromFile(interOper.GetBMPPath(_ariaApplication) + "emptypic.bmp");
            }

            iconPictureBox.Visible = false;
            fileaxGdViewer.Visible = false;
            innerPanel.Visible = false;

            if (fileaxGdViewer.ImageWidth == 0)
            {
                if (_source.IsPicture) _source.IsPicture = false;

                iconPictureBox.Visible = true;
                innerPanel.Visible = true;

                SHFILEINFO shinfo = new SHFILEINFO();
                Icon smallIcon = null;

                SHGetFileInfo(_source.File.FullName, 0, ref shinfo, (uint)Marshal.SizeOf(shinfo), SHGFI_ICON | SHGFI_LARGEICON);

                try
                {
                    smallIcon = Icon.FromHandle(shinfo.hIcon);
                }
                catch (Exception)
                {
                }

                Bitmap bitmap = smallIcon.ToBitmap();
                iconPictureBox.Image = bitmap.GetThumbnailImage(64, 64, null, IntPtr.Zero);

                _source.SftFormatFileName = System.IO.Path.GetTempFileName().Replace(".tmp", ".bmp");
                bitmap.Save(_source.SftFormatFileName, System.Drawing.Imaging.ImageFormat.Bmp);
            }
            else
            {
                if (!_source.IsPicture) _source.IsPicture = true;

                fileaxGdViewer.Visible = true;

                if (fileaxGdViewer.ImageWidth > fileaxGdViewer.Width ||
                   fileaxGdViewer.ImageHeight > fileaxGdViewer.Height)
                {
                    fileaxGdViewer.ZoomMode = GdViewer4S.ViewerZoomMode.ZoomFitToControl;
                }

                _source.SftFormatFileName = System.IO.Path.GetTempFileName().Replace(".tmp", ".bmp");
                Bitmap bitmap = Bitmap.FromHbitmap((IntPtr)fileaxGdViewer.GetHBitmap());
                bitmap.Save(_source.SftFormatFileName, System.Drawing.Imaging.ImageFormat.Bmp);
            }

            AriaThumbnailControl_Resize(null, null);
        }

        void AriaThumbnailControl_Resize(object sender, EventArgs e)
        {
            SuspendLayout();

            if (_source == null) return;

            filelabel.Text = _source.Name;
            if (_source.IsDefault)
            {
                filelabel.Font = new Font(filelabel.Font, FontStyle.Bold);
            }
            else
            {
                filelabel.Font = new Font(filelabel.Font, FontStyle.Regular);
            }

            if (_settings.ShowFileNames)
            {
                filelabel.Visible = true;
            }
            else
            {
                filelabel.Visible = false;
            }

            if (_source.IsActive)
            {
                filePanel.BorderStyle = BorderStyle.FixedSingle;
                filePanel.BackColor = System.Drawing.Color.FromArgb(255, 192, 128);
                
                filelabel.BorderStyle = BorderStyle.FixedSingle;
                filelabel.BackColor = System.Drawing.Color.FromArgb(255, 192, 128);
            }
            else
            {
                filePanel.BorderStyle = BorderStyle.None;
                filePanel.BackColor = System.Drawing.Color.White;

                filelabel.BorderStyle = BorderStyle.None;
                filelabel.BackColor = System.Drawing.Color.White;
            }

            iconPictureBox.Location = new Point((Width - iconPictureBox.Width) / 2, (Height - iconPictureBox.Height) / 2);

            ResumeLayout();
        }

        private void AriaThumbnailControl_Click(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex == _index) return;

            _sources.ActiveObjectIndex = _index;

            Select();
        }

        private void filePanel_Click(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex == _index) return;

            _sources.ActiveObjectIndex = _index;

            Select();
        }

        private void filelabel_Click(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex == _index) return; 
            
            _sources.ActiveObjectIndex = _index;

            Select();
        }

        private void iconPictureBox_Click(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex == _index) return; 
            
            _sources.ActiveObjectIndex = _index;

            Select();
        }

        private void innerPanel_Click(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex == _index) return; 
            
            _sources.ActiveObjectIndex = _index;

            Select();
        }

        public void Print()
        {
            fileaxGdViewer.PrintImageDialog();
        }

        private void fileaxGdViewer_DblClickControl(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex > -1)
            {
                System.Diagnostics.Process.Start(_sources[_sources.ActiveObjectIndex].File.FullName);
            }
        }

        private void iconPictureBox_DoubleClick(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex > -1)
            {
                System.Diagnostics.Process.Start(_sources[_sources.ActiveObjectIndex].File.FullName);
            }
        }

        private void innerPanel_DoubleClick(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex > -1)
            {
                System.Diagnostics.Process.Start(_sources[_sources.ActiveObjectIndex].File.FullName);
            }
        }

        private void AriaThumbnailControl_DoubleClick(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex > -1)
            {
                System.Diagnostics.Process.Start(_sources[_sources.ActiveObjectIndex].File.FullName);
            }
        }

        private void filePanel_DoubleClick(object sender, EventArgs e)
        {
            if (_sources.ActiveObjectIndex > -1)
            {
                System.Diagnostics.Process.Start(_sources[_sources.ActiveObjectIndex].File.FullName);
            }
        }

        private void AriaThumbnailControl_PreviewKeyDown(object sender, PreviewKeyDownEventArgs e)
        {
            //MessageBox.Show(e.KeyCode.ToString());
        }
    }
}