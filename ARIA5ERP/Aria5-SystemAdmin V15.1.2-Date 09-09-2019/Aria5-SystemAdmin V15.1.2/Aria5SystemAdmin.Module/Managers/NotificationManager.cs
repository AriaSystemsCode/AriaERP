using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Aria5SystemAdmin.Module.BusinessObjects;

namespace Aria5SystemAdmin.Module.Managers
{
    class NotificationManager
    {

        public void ProjectTemplateNotification(ProjectTemplate Project)
        {
            if (Project.Type == ProjectTemplate.ProjectType.Key)
            {
                bool missanything = false;
                if (DateTime.Today >= CalcEndate.CalcEnddate(Project.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)) && DateTime.Today < CalcEndate.CalcEnddate(Project.StartDate, 2))
                {
                    Project.NotificationMessage = string.Format("We need to check the blew missing items for this project '{0}' before this date '{1}'", Project.Name, CalcEndate.CalcEnddate(Project.StartDate, 2));
                    if (Project.Requirements.Count == 0)
                    {
                       Project.NotificationMessage += " Requirement is missing ";
                        missanything = true;
                    }
                    else
                    {
                        foreach (Requirement requirement in Project.Requirements)
                        {
                            if (requirement.ApproveStatus != Requirement.ApproveStatu.Ready)
                            {
                                Project.NotificationMessage += string.Format("This Requirement '{0}' is not Approved ", requirement.Title);
                                missanything = true;

                            }
                        }
                    }
                    if (Project.UseCases.Count == 0)
                    {
                        Project.NotificationMessage += " Use cases is missing ";
                        missanything = true;

                    }
                    else
                    {
                        foreach (QAUseCase usecase in Project.UseCases)
                        {
                            if (usecase.ApproveStatus != QAUseCase.ApproveStatu.Ready)
                            {
                                Project.NotificationMessage += string.Format("This use case '{0}' is not Approved ", usecase.Name);
                                missanything = true;

                            }
                        }
                    }
                    if (Project.ProjectEntities.Count == 0)
                    {
                        Project.NotificationMessage += " Project Entities is missing ";
                        missanything = true;

                    }
                    else
                    {
                        foreach (QAProjectEntity projectentity in Project.ProjectEntities)
                        {
                            if (projectentity.ApproveStatus != QAProjectEntity.ApproveStatu.Ready)
                            {
                                Project.NotificationMessage += string.Format("This project entity '{0}' is not Approved", projectentity.Name);
                                missanything = true;

                            }
                        }

                    }
                    if (Project.UseCasePoints == null)
                    {
                        Project.NotificationMessage += " UseCasePoints missing ";
                        missanything = true;

                    }
                    if (missanything == true && Project.IsPostponed == false)
                    {
                        Project.AlarmTime = DateTime.Today;
                    }
                    else if (missanything == true && Project.IsPostponed == true && DateTime.Today > Project.AlarmTime)
                    {
                        Project.AlarmTime = DateTime.Today;
                        Project.IsPostponed = false;
                    }
                    else if (missanything == true && Project.IsPostponed == true && DateTime.Today < Project.AlarmTime)
                    {
                    }
                    else
                    {
                        Project.AlarmTime = null;
                    }
                }
                else if (DateTime.Today >= CalcEndate.CalcEnddate(Project.StartDate, 3).Subtract(new TimeSpan(10, 0, 0, 0)) && DateTime.Today < CalcEndate.CalcEnddate(Project.StartDate, 3))
                {
                    Project.NotificationMessage = string.Format("We need to check the blew missing items for this project '{0}' before this date '{1}'", Project.Name, CalcEndate.CalcEnddate(Project.StartDate, 3));
                    if (Project.TestCases.Count == 0)
                    {
                        Project.NotificationMessage += " Test Cases ";
                        missanything = true;
                    }
                    else
                    {
                        foreach (TestCase testcase in Project.TestCases)
                        {
                            if (testcase.ApproveStatus != TestCase.ApproveStatu.Ready)
                            {
                                Project.NotificationMessage += string.Format("This test case {0} is not Approved ", testcase.Title);
                                missanything = true;
                            }
                        }
                    }
                    if (missanything == true && Project.IsPostponed == false)
                    {
                        Project.AlarmTime = DateTime.Today;
                    }
                    else if (missanything == true && Project.IsPostponed == true && DateTime.Today > Project.AlarmTime)
                    {
                        Project.AlarmTime = DateTime.Today;
                        Project.IsPostponed = false;
                    }
                    else if (missanything == true && Project.IsPostponed == true && DateTime.Today < Project.AlarmTime)
                    {
                    }
                    else
                    {
                        Project.AlarmTime= null;
                    }
                }
            }

        }
    }
}
