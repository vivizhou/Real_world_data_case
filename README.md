# Real world data case
This project contains the solutions for a real world problem regarding dosing strategy for medications in children with organ transplant.

File: main.md

Data have been extracted from EHR regarding the use of tacrolimus in pediatric solid organ transplant recipients. One of the known side effects of tacrolimus is the decline of renal function over time.


### Problem 1

We have a data set of 500 patients. However, all the data are stored in the old excel file type (.xls). Please write a program to convert these files into (.xlsx) in a batch and execute it as well.

File screenshot:

   [1] "1-10/Admission Tac & meds 1-10.xls"                          
   [2] "1-10/Admissions 1,2,3,4,6,7,10.xls"                          
   [3] "1-10/Admissions 5,8,9.xls"                                   
   [4] "1-10/AdmitDate 1-10.xls"                                     
   [5] "1-10/Basic patient 5,8,9.xls"                                
   [6] "1-10/Basic transplant 5,8,9.xls"                             
   [7] "1-10/BasicPatient 1,2,3,4,6,7,10.xls"                        
   [8] "1-10/BasicTransplant 1,2,3,6,7,10.xls"                       
   [9] "1-10/CIMS misc 5,8,9.xls"                                    
  [10] "1-10/CIMSOther 1,2,3,4,6,7,10.xls"                           
  [11] "1-10/Dialysis 1-10.xls"                                      
  [12] "1-10/Glucose 1,2,3,4,6,7,10.xls"                             
  [13] "1-10/Glucose 5,8,9.xls"                                      
  [14] "1-10/Labs avg 5,8,9.xls"                                     
  [15] "1-10/Labs raw 5,8,9.xls"                                     
  [16] "1-10/LabsAvg 1,2,3,4,6,7,10.xls"                             
  [17] "1-10/LabsRaw 1,2,3,4,6,7,10.xls"                             
  [18] "1-10/Meds 1,2,3,4,6,7,10.xls"                                
  [19] "1-10/Meds 5,8,9.xls"                                         
  [20] "1-10/mGFR & MRI 1,3-10.xls"                                  
  [21] "1-10/mGFR & MRI 2.xls"                                       
  [22] "1-10/mGFR & MRI 5,8,9.xls"                                   
  [23] "1-10/MMF levels 1-10.xls"                                    
  [24] "1-10/Serology (2) 1-10.xls"                                  
  [25] "1-10/Serology 1,2,3,4,6,7,10.xls"                            
  [26] "1-10/Serology 5,8,9.xls"                                     
  [27] "1-10/Tac levels 5,8,9.xls"                                   
  [28] "1-10/Tacro 1,2,3,4,6,7,10.xls"                               
  [29] "1-10/Vitals 1-10.xls"                                        
  [30] "1-10/Vitals 2.xls"                                           
  [31] "100-109/Admissions 100,101,102,103,104,106,108.xls"          
  [32] "100-109/Admissions 109.xls"                                  
  [33] "100-109/ADMIT & MEDS & TAC 100-109.xls"                      
  [34] "100-109/AdmitDate 100-109 excl 105,107.xls"                  
  [35] "100-109/Basic patient 100,101,102,103,104,106,108.xls"       
  [36] "100-109/Basic patient 109.xls"                               
  [37] "100-109/Basic transplant 100,101,102,103,104,106,108.xls"    
  [38] "100-109/Basic transplant 109.xls"                            
  [39] "100-109/CIMS misc 100,101,102,103,104,106,108.xls"           
  [40] "100-109/CIMS misc 109.xls"                                   
  [41] "100-109/Dialysis 100-109 excl 105,107.xls"                   
  [42] "100-109/Glucose 100,101,102,103,104,106,108.xls"             
  [43] "100-109/Glucose 109.xls"                                     
  [44] "100-109/Labs avg 100,101,102,103,104,106,108.xls"            
  [45] "100-109/Labs avg 109.xls"                                    
  [46] "100-109/Labs raw 100,101,102,103,104,106,108.xls"            
  [47] "100-109/Labs raw 109.xls"                                    
  [48] "100-109/Meds 100,101,102,103,104,106,108.xls"                
  [49] "100-109/Meds 109.xls"                                        
  [50] "100-109/mGFR & MRI 109.xls"                                  
  [51] "100-109/mGFR&MRI 100,101,102,103,104,106,108.xls"            
  [52] "100-109/MMF levels 100-101 excl 105,107.xls"                 
  [53] "100-109/Serology (2) 100-109 excl 105, 107.xls"              
  [54] "100-109/Serology 100,101,102,103,104,106,108.xls"            
  [55] "100-109/Serology 109.xls"                                    
  [56] "100-109/Tac levels 100,101,102,103,104,106,108.xls"          
  [57] "100-109/Tac levels 109.xls"                                  
  [58] "100-109/Vitals 100-109 excl 105,107.xls"                     
  [59] "11-20/11,16,17,18 admissions.xls"                            
  [60] "11-20/11,16,17,18 admissions1.xls"                           
  [61] "11-20/11,16,17,18 basic patient.xls"                         
  [62] "11-20/11,16,17,18 basic patient1.xls"                        
  [63] "11-20/11,16,17,18 basic transplant.xls"                      
  [64] "11-20/11,16,17,18 basic transplant1.xls"                     
  [65] "11-20/11,16,17,18 CIMS misc.xls"                             
  [66] "11-20/11,16,17,18 CIMS misc1.xls"                            
  [67] "11-20/11,16,17,18 gfr and mri.xls"                           
  [68] "11-20/11,16,17,18 gfr and mri1.xls"                          
  [69] "11-20/11,16,17,18 glucose.xls"                               
  [70] "11-20/11,16,17,18 glucose1.xls"                              
  [71] "11-20/11,16,17,18 lab avg.xls"                               
  [72] "11-20/11,16,17,18 lab avg1.xls"                              
  [73] "11-20/11,16,17,18 meds.xls"                                  
  [74] "11-20/11,16,17,18 meds1.xls"                                 
  [75] "11-20/11,16,17,18 raw lab.xls"                               
  [76] "11-20/11,16,17,18 raw lab1.xls"                              
  [77] "11-20/11,16,17,18 serology.xls"                              
  [78] "11-20/11,16,17,18 serology1.xls"                             
  [79] "11-20/11,16,17,18 tac levels.xls"                            
  [80] "11-20/11,16,17,18 tac levels1.xls"                           
  [81] "11-20/Admissions 12,13,14,19,20.xls"                         
  [82] "11-20/Admissions 12,13,14,19,201.xls"                        
  [83] "11-20/Admit date & Meds 11-20.xls"                           
  [84] "11-20/Admit date & Meds 11-201.xls"                          
  [85] "11-20/AdmitDate 11-20 excl 15.xls"                           
  [86] "11-20/AdmitDate 11-20 excl 151.xls"                          
  [87] "11-20/Basic patient 12,13,14,19,20.xls"                      
  [88] "11-20/Basic patient 12,13,14,19,201.xls"                     
  [89] "11-20/Basic transplant 12,13,14,19,20.xls"          

### Problem 2

Data have been extracted from EHR regarding the use of tacrolimus in pediatric solid organ transplant recipients. One of the known side effects of tacrolimus is the decline of renal function over time.

To have a better understanding of this problem, we would like you to show us how many patients develop chronic kidney disease.
Design and conduct an analysis of the data to determine possible factors contributing to this decline in renal function over time.
Is there a dose-response relationship between tacrolimus and renal function? Please design and conduct the analysis.
