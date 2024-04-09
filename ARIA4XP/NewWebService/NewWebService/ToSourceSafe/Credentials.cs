using System;
using System.Runtime.CompilerServices;

namespace Fedex
{
	internal class Credentials
	{
		public string AccountNumber
		{
			get;
			set;
		}

		public string Key
		{
			get;
			set;
		}

		public string MeterNumber
		{
			get;
			set;
		}

		public string Password
		{
			get;
			set;
		}

		public Credentials()
		{
		}
	}
}