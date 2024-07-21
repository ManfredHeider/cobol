000010*---------------------------------------------------------------* 
000020*    Standard für alle Objekte unter PVCS                         
000030*            Datum   :  Januar 2001                               
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
000050*---------------------------------------------------------------* 
000060*                                                                 
000070*---------------------------------------------------------------* 
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH                    
000090*---------------------------------------------------------------* 
000100*    $Workfile:   drvb0589.cpy  $                                 
000110*    $Revision:   2.3  $                                          
000120*                                                                 
000130*    $Log:   L:/mf/cpy/DRVB0589.CPv  $                            
000140*
000150*   Rev 2.3   Mar 26 2003 12:04:36   t13ex57
000160*AVB00666 - Einführung neue Stufen 
000170*
000180*   Rev 2.2   Nov 05 2002 16:28:42   t13ex57
000190*AVB00636 - Erweiterung SST
000200*                                                                 
000210*   Rev 2.1   Aug 09 2002 11:06:52   t13ex57                      
000220*AVB00587-SST Satzbeschreibung fuer URL                           
000230*                                                                 
000240*   Rev 2.0   Feb 25 2002 17:33:04   p13sp20                      
000250*Archiverstellung                                                 
000260*                                                                 
000270*---------------------------------------------------------------* 
000280***************************************************************** 
000290*    COPY-MEMBER: SATZBESCHREIBUNG SCHNITTSTELLENDATEI FÜR      * 
000300*                 URL                                           * 
000310*    --------------------------------------------------------   * 
000320*    ERSTELLT: UTE BOLL                                         * 
000330***************************************************************** 
000340*    VERSION   01   25.02.1994                                  * 
000350***************************************************************** 
000360* AENDERUNGEN:                                                  * 
000370* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      * 
000380* --------------------------------------------------------------* 
000390* AVB00587!09.07.02! 01 ! NEU                       ! U.BOLL    * 
000400* AVB00636!17.10.02! 02 ! Erweiterung der SST       ! U. BOLL   *
000410* AVB00666!05.03.03! 03 ! Anpassung wg neuer Stufen ! U. Boll   *
000420*         !        !    ! VBAS/ALP und Sonderregel  !           *
000430*         !        !    ! (pos. Beschränkung)       !           *
000440*         !        !    !                           !           * 
000450*         !        !    !                           !           * 
000460*         !        !    !                           !           * 
000470*                                                               * 
000480* ENDE-LOGBUCH                                                  * 
000490****************************************************************  
000500*                                                                 
000510*    URL-SST                                                      
000520*                                                                 
000530  01  DRVB0589.                                                   
000540     05 DRVB0589-VBPORT-ST-NR          PIC  9(007).               
000550     05 DRVB0589-VBPORT-URL            PIC  X(070).               
000560     05 DRVB0589-VBPORTSTUFE           PIC  9(003).               
000570     05 DRVB0589-VBPORTPRAXIS          PIC  9(001).               
000580     05 DRVB0589-VBPORT-GES-TYP        PIC  9(001).               
000590     05 DRVB0589-VBPORT-REGION         PIC  X(004).               
000600     05 DRVB0589-VBPORT-DIR            PIC  9(004).               
000610     05 DRVB0589-ADM-CLUB              PIC  X(001).               
000620     05 DRVB0589-AS-CLUB               PIC  X(001).               
000630     05 DRVB0589-BDV                   PIC  X(001).               
000640     05 DRVB0589-VB-DAT                PIC  X(010).               
000650     05 DRVB0589-DL-DAT                PIC  X(010).               
000660     05 DRVB0589-ALD                   PIC  X(001).
000670     05 DRVB0589-SONDERREGEL           PIC  X(001).
000680*    05 FILLER                         PIC  X(014)  VALUE SPACE.
000690     05 FILLER                         PIC  X(013)  VALUE SPACE.
000700                                                                  
000710*                                                                 
000720******** ENDE COPY-MEMBER (DRVB0589) ************                 
000730*                                                                 
000740                                                                  
000750                                                                  
000760                                                                  
000770                                                                  
000780                                                                  
