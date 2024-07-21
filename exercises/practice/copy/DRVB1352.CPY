      *****************************************************************         
      *    COPY-MEMBER: DRVB1352                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0237                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    02.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !02.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB1352 - ERMITTELN SATZ AUS TABELLE BZLP (LFDNR)*         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1352.                                                          
          05 PI-DRVB1352-DATEN.                                                 
             10 PI-DRVB1352-BZLP-LFDNR        PIC S9(004) BINARY.               
             10 PI-DRVB1352-KNR-GESELL-TYP    PIC S9(004) BINARY.               
                                                                                
       01 PO-DRVB1352.                                                          
          05 PO-DRVB1352-RC                   PIC  X(003).                      
             88 PO-DRVB1352-RC-OK                  VALUE 'OK '.                 
             88 PO-DRVB1352-RC-NOT-FOUND           VALUE 'NFD'.                 
          05 PO-DRVB1352-DATEN.                                                 
             10 PO-DRVB1352-NI-VIVB0237.                                        
                15 NI-DRVB1352-BZLP-LFDNR          PIC S9(004) BINARY.          
                15 NI-DRVB1352-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1352-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-BIS-STAT       PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-ART            PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-EQF-1          PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-EQF-2          PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-EH-PROZ        PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-VB-VERTR       PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-BS             PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-BS-PROZ        PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-MS-NR          PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-VBP-NR         PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-STATUS         PIC S9(004) BINARY.          
                15 NI-DRVB1352-BZLP-LAST-UPDATE    PIC S9(004) BINARY.          
             10 PO-DRVB1352-DCL-VIVB0237.                                       
                15 PO-DRVB1352-BZLP-LFDNR          PIC S9(004) BINARY.          
                15 PO-DRVB1352-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1352-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1352-BZLP-BIS-STAT       PIC  X(010).                 
                15 PO-DRVB1352-BZLP-ART            PIC  X(001).                 
                15 PO-DRVB1352-BZLP-EQF-1   PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1352-BZLP-EQF-2   PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1352-BZLP-EH-PROZ PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1352-BZLP-VB-VERTR       PIC S9(004) BINARY.          
                15 PO-DRVB1352-BZLP-BS             PIC  X(001).                 
                15 PO-DRVB1352-BZLP-BS-PROZ PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1352-BZLP-MS-NR          PIC S9(004) BINARY.          
                15 PO-DRVB1352-BZLP-VBP-NR         PIC S9(004) BINARY.          
                15 PO-DRVB1352-BZLP-STATUS         PIC  X(001).                 
                15 PO-DRVB1352-BZLP-LAST-UPDATE    PIC  X(026).                 
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1352   --------------------------*         
      *---------------------------------------------------------------*         
