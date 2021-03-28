# WXYZ-Model
 A 4-state Markov model built in R


##################################################################################
#                                                                                #
#  TITLE: The 'WXYZ' model - probabilistic                                       #
#                                                                                #
#  AUTHOR: Ian Andrew Cromwell, PhD                                              #
#                                                                                #
#  NOTE: This code is a modified verion of a program written by the              #
#        DARTH Working Group. Full attribution follows.                          #
#                                                                                #
#  PURPOSE: This code is a deterministic version of the Markov model that is     #
#            described on the 'Healthy Uncertainty' R Guide:                     #
#                 https://healthyuncertainty.github.io/RGuide/Chapter5/          #
#            It has been updated to reflect the DARTH method.                    #
#                                                                                #
#  MORE INFORMATION: http://healthyuncertainty.github.io/WXYZ_Model              #
#                                                                                #
##################################################################################

Developed by the Decision Analysis in R for Technologies in Health (DARTH) workgroup:

Fernando Alarid-Escudero, PhD (1) 

Eva A. Enns, MS, PhD (2)	

M.G. Myriam Hunink, MD, PhD (3,4)

Hawre J. Jalal, MD, PhD (5) 

Eline M. Krijkamp, MSc (3)	

Petros Pechlivanoglou, PhD (6,7)

Alan Yang, MSc (7)

In collaboration of: 		

1. Drug Policy Program, Center for Research and Teaching in Economics (CIDE) - CONACyT, 
   Aguascalientes, Mexico
2. University of Minnesota School of Public Health, Minneapolis, MN, USA
3. Erasmus MC, Rotterdam, The Netherlands
4. Harvard T.H. Chan School of Public Health, Boston, USA
5. University of Pittsburgh Graduate School of Public Health, Pittsburgh, PA, USA
6. University of Toronto, Toronto ON, Canada
7. The Hospital for Sick Children, Toronto ON, Canada

Please cite our publications when using this code:
 
- Jalal H, Pechlivanoglou P, Krijkamp E, Alarid-Escudero F, Enns E, Hunink MG. 
An Overview of R in Health Decision Sciences. Med Decis Making. 2017; 37(3): 735-746. 
https://journals.sagepub.com/doi/abs/10.1177/0272989X16686559
 
- Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
Microsimulation modeling for health decision sciences using R: A tutorial. 
Med Decis Making. 2018;38(3):400–22. 
https://journals.sagepub.com/doi/abs/10.1177/0272989X18754513
 
- Krijkamp EM, Alarid-Escudero F, Enns E, Pechlivanoglou P, Hunink MM, Jalal H. 
A Multidimensional Array Representation of State-Transition Model Dynamics. 
Med Decis Making. Online First https://doi.org/10.1177/0272989X19893973

Copyright 2017, THE HOSPITAL FOR SICK CHILDREN AND THE COLLABORATING INSTITUTIONS. 
All rights reserved in Canada, the United States and worldwide. Copyright, 
trademarks, trade names and any and all associated intellectual property are 
exclusively owned by THE HOSPITAL FOR Sick CHILDREN and the collaborating 
institutions. These materials may be used, reproduced, modified, distributed 
and adapted with proper attribution.