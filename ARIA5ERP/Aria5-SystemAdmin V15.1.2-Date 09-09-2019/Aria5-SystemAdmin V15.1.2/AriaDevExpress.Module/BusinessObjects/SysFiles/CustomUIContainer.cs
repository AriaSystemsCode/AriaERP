using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "Browse Fields")]
    public class BrowseFieldsEmptyClass
    {
        //  public BrowseFieldsEmptyClass(Session session) : base(session) { }
        //  [DevExpress.Xpo.Association("File-BrowseFields"), DevExpress.Xpo.NoForeignKey()]
        private File ParentFile { get; set; }
        public File GetFile()
        {
            return ParentFile;
        }
        public void SetFile(File file)
        {
            ParentFile = file;
        }
    }

    [DevExpress.Xpo.NonPersistent]
    [DevExpress.Xpo.Custom("Caption", "Report Variables Postions")]
    public class ReportVariablesSorterEmptyClass
    {
        //  public BrowseFieldsEmptyClass(Session session) : base(session) { }
        //  [DevExpress.Xpo.Association("File-BrowseFields"), DevExpress.Xpo.NoForeignKey()]
        private Report ParentReport { get; set; }
        public Report GetReport()
        {
            return ParentReport;
        }
        public void SetReport(Report report)
        {
            ParentReport = report;
        }
    }

    [DevExpress.Xpo.NonPersistent()]
    [DevExpress.Xpo.Custom("Caption", "Export")]
    public class ExportEmptyClass
    {

        private Session _session;
        public Session GetSession()
        {
            return _session;
        }

        public void SetSession(Session session)
        {
            _session = session;
        }
    }
}
