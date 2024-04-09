using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.ServiceProcess;
using System.Text;

namespace Aria.Services.RetryFailedRequests
{
    public partial class Service1 : ServiceBase
    {
        public Service1()
        {
            InitializeComponent();
        }

        protected override void OnStart(string[] args)
        {
        }

        protected override void OnStop()
        {
        }

        public void retryFailedRequests()
        {
            List<int> requestList = new List<int>(); 
        }

        private void Timer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
        {
            // connnect to db 

            // get all reqquests with fail for last 30 minutes

            // loop on each request and update to to onhold

        }
    }
}
