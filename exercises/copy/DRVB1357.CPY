      *****************************************************************         
      *    COPY-MEMBER: DRVB1357                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0238                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    12.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !12.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB1357 - ERMITTELN SATZ AUS TABELLE BZLE        *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1357.                                                          
          05 PI-DRVB1357-DATEN.                                                 
             10 PI-DRVB1357-BZLE-LFDNR        PIC S9(004) BINARY.               
             10 PI-DRVB1357-KNR-GESELL-TYP    PIC S9(004) BINARY.               
             10 PI-DRVB1357-MA-ST-NR          PIC S9(009) BINARY.               
                                                                                
       01 PO-DRVB1357.                                                          
          05 PO-DRVB1357-RC                   PIC  X(003).                      
             88 PO-DRVB1357-RC-OK                  VALUE 'OK '.                 
             88 PO-DRVB1357-RC-NOT-FOUND           VALUE 'NFD'.                 
          05 PO-DRVB1357-DATEN.                                                 
             10 PO-DRVB1357-NI-VIVB0238.                                        
                15 NI-DRVB1357-BZLE-LFDNR          PIC S9(004) BINARY.          
                15 NI-DRVB1357-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1357-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ART            PIC S9(004) BINARY.          
                15 NI-DRVB1357-MA-ST-NR            PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ERM-KZ         PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-MS-NR          PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-VBP-NR         PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-VBP-NR-G       PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ZV-NR          PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ZV-DAT-BIS     PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ZV-DAT-VBG     PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-EG-NEU      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-EG-NEU-A    PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ERF-KZ-EG      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-JEQF-EG        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-EG-GES      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-EG-KORR     PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-GR-GES      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-GES         PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-EH-GES-A       PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ERF-KZ-GS      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-JEQF-GES       PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-PROV-VERD      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-VBG-ABSICH     PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-CEN-VNR        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-CKV-NEU        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ASCLUB-DAT     PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-ASCLUB-BL      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-BS-VORJ        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-INFO-KZ        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-VERTRAG        PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-DAT-VOLLM      PIC S9(004) BINARY.          
                15 NI-DRVB1357-BZLE-LAST-UPDATE    PIC S9(004) BINARY.          
             10 PO-DRVB1357-DCL-VIVB0238.                                       
                15 PO-DRVB1357-BZLE-LFDNR          PIC S9(004) BINARY.          
                15 PO-DRVB1357-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1357-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1357-BZLE-ART            PIC  X(001).                 
                15 PO-DRVB1357-MA-ST-NR            PIC S9(009) BINARY.          
                15 PO-DRVB1357-BZLE-ERM-KZ         PIC  X(002).                 
                15 PO-DRVB1357-BZLE-MS-NR          PIC S9(004) BINARY.          
                15 PO-DRVB1357-BZLE-VBP-NR         PIC S9(004) BINARY.          
                15 PO-DRVB1357-BZLE-VBP-NR-G       PIC S9(004) BINARY.          
                15 PO-DRVB1357-BZLE-ZV-NR          PIC  X(001).                 
                15 PO-DRVB1357-BZLE-ZV-DAT-BIS     PIC  X(010).                 
                15 PO-DRVB1357-BZLE-ZV-DAT-VBG     PIC  X(010).                 
                15 PO-DRVB1357-BZLE-EH-EG-NEU                                   
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-EG-NEU-A                                 
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-ERF-KZ-EG      PIC  X(001).                 
                15 PO-DRVB1357-BZLE-JEQF-EG PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-EG-GES                                   
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-EG-KORR                                  
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-GR-GES                                   
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-GES  PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-EH-GES-A                                    
                                            PIC S9(009)V9(001) COMP-3.          
                15 PO-DRVB1357-BZLE-ERF-KZ-GS      PIC  X(001).                 
                15 PO-DRVB1357-BZLE-JEQF-GES                                    
                                            PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1357-BZLE-PROV-VERD                                   
                                            PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1357-BZLE-VBG-ABSICH                                  
                                            PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1357-BZLE-CEN-VNR        PIC  X(015).                 
                15 PO-DRVB1357-BZLE-CKV-NEU        PIC  X(001).                 
                15 PO-DRVB1357-BZLE-ASCLUB-DAT     PIC  X(010).                 
                15 PO-DRVB1357-BZLE-ASCLUB-BL      PIC  X(001).                 
                15 PO-DRVB1357-BZLE-BS-VORJ        PIC  X(001).                 
                15 PO-DRVB1357-BZLE-INFO-KZ        PIC  X(002).                 
                15 PO-DRVB1357-BZLE-VERTRAG        PIC  X(001).                 
                15 PO-DRVB1357-BZLE-DAT-VOLLM      PIC  X(010).                 
                15 PO-DRVB1357-BZLE-LAST-UPDATE    PIC  X(026).                 
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1357   --------------------------*         
      *---------------------------------------------------------------*         
