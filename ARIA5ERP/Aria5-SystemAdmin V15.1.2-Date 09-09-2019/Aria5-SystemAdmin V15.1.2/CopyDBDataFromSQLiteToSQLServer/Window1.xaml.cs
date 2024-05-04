using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace CopyDBDataFromSQLiteToSQLServer
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        public Window1()
        {
            InitializeComponent();
            DataContext = this;
        }

        public Window1(ObservableCollection<string> matchedTableNames, ObservableCollection<string> missMatchedCapTableNames, Dictionary<string, List<Column>> missMatchedCapColumnNames, Dictionary<string, List<Column>> missingColumnNames)
        {
            InitializeComponent();
            MatchedTableNames = matchedTableNames;
            MissMatchedCapTableNames = missMatchedCapTableNames;
            MissMatchedCapColumnNames = missMatchedCapColumnNames;
            MissingColumnNames = missingColumnNames;
            //MissMatchedCapColumnNames.Keys
            DataContext = this;


        }

        private ObservableCollection<string> _matchedTableNames = new ObservableCollection<string>();
        public ObservableCollection<string> MatchedTableNames
        {
            set
            {
                _matchedTableNames = value;
            }
            get
            {
                return _matchedTableNames;
            }
        }

        private ObservableCollection<string> _missMatchedCapTableNames = new ObservableCollection<string>();
        public ObservableCollection<string> MissMatchedCapTableNames
        {
            set
            {
                _missMatchedCapTableNames = value;
            }
            get
            {
                return _missMatchedCapTableNames;
            }
        }

        private Dictionary<string, List<Column>> _missMatchedCapColumnNames = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> MissMatchedCapColumnNames
        {
            set
            {
                _missMatchedCapColumnNames = value;
            }
            get
            {
                return _missMatchedCapColumnNames;
            }
        }

        private Dictionary<string, List<Column>> _missingColumnNames = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> MissingColumnNames
        {
            set
            {
                _missingColumnNames = value;
            }
            get
            {
                return _missingColumnNames;
            }
        }
    }
}
