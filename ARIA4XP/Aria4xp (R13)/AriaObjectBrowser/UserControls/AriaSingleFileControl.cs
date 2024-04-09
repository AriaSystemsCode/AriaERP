using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using AriaObjectBrowser.DataTypes;
using System.Runtime.InteropServices;
using TaskPaneControl;
using System.IO;


namespace AriaObjectBrowser.UserControls
{
    public partial class AriaSingleFileControl : UserControl
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

        public AriaSingleFileControl()
        {
            InitializeComponent();

            innerPanel.Dock = DockStyle.Fill;
            fileaxGdViewer.Dock = DockStyle.Fill;
            
            fileaxGdViewer.SetLicenseNumber("4690766445560616550501148");
            // MOH T20100331.0004   Objects not deleting from objects directory start
            fileaxGdViewer.ImageForceTemporaryMode = true;
            // MOH T20100331.0004   Objects not deleting from objects directory start

        }

        public void SetSource(AriaObjectList sources, int index)
        {
            _sources = sources;
            _index = index;

            if (_sources.ActiveObjectIndex == -1) return;

            _source = sources[index];

            _sources.ActiveObjectChanged += new AriaActiveObjectChangedEventHandler(_sources_ActiveObjectChanged);
            _source.Changed += new AriaObjectChangedEventHandler(_source_Changed);

            _source_Changed();

            Visible = true;

        }

        void AriaSettings_Changed()
        {
            if (_source == null) return;

            SuspendLayout();

            if (_source.IsPicture)
            {
                fileaxGdViewer.Zoom = _settings.Zoom / 100.00;
            }
            else
            {
                iconPictureBox.Location = new Point((innerPanel.Width - iconPictureBox.Width) / 2, (innerPanel.Height - iconPictureBox.Height) / 2);
            }

            ResumeLayout();
        }

        void AriaSettings_RefreshAll()
        {

            try
            {
                fileaxGdViewer.DisplayFromFile(_source.File.FullName);
            
            }
            catch (Exception)
            {
                }
        }

        void _sources_ActiveObjectChanged()
        {
            if (_sources.ActiveObjectIndex == -1)
            {
                Visible = false;
            
                fileaxGdViewer.Clear();
                iconPictureBox.Image = null;
                _index = -1;
                _source = null;
            }
            else
            {
                if (_index != _sources.ActiveObjectIndex)
                {
                    _source_Changed();
                }
            }
        }

        void _source_Changed()
        {
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

                SHGetFileInfo(_source.File.FullName, 0, ref shinfo, (uint)Marshal.SizeOf(shinfo), SHGFI_ICON | 6);

                try
                {
                    smallIcon = Icon.FromHandle(shinfo.hIcon);
                }
                catch (Exception)
                {
                }

                Bitmap bitmap = smallIcon.ToBitmap();
                iconPictureBox.Image = bitmap.GetThumbnailImage(46, 46, null, IntPtr.Zero);
            }
            else
            {
                if (!_source.IsPicture) _source.IsPicture = true;

                fileaxGdViewer.Visible = true;
            }
            
            AriaSettings_Changed();
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
   }
}