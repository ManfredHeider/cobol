      *****************************************************************         
      *    COPY-MEMBER: DRVB1348                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0237                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    01.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !01.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB1348 - ERMITTELN SATZ AUS TABELLE BZLP (OFFEN)*         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1348.                                                          
          05 PI-DRVB1348-DATEN.                                                 
             10 PI-DRVB1348-DUMMY             PIC  X(001).                      
                                                                                
       01 PO-DRVB1348.                                                          
          05 PO-DRVB1348-RC                   PIC  X(003).                      
             88 PO-DRVB1348-RC-OK                  VALUE 'OK '.                 
             88 PO-DRVB1348-RC-NOT-FOUND           VALUE 'NFD'.                 
          05 PO-DRVB1348-DATEN.                                                 
             10 PO-DRVB1348-NI-VIVB0237.                                        
                15 NI-DRVB1348-BZLP-LFDNR          PIC S9(004) BINARY.          
                15 NI-DRVB1348-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1348-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-BIS-STAT       PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-ART            PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-EQF-1          PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-EQF-2          PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-EH-PROZ        PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-VB-VERTR       PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-BS             PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-BS-PROZ        PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-MS-NR          PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-VBP-NR         PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-STATUS         PIC S9(004) BINARY.          
                15 NI-DRVB1348-BZLP-LAST-UPDATE    PIC S9(004) BINARY.          
             10 PO-DRVB1348-DCL-VIVB0237.                                       
                15 PO-DRVB1348-BZLP-LFDNR          PIC S9(004) BINARY.          
                15 PO-DRVB1348-KTY-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1348-KNR-GESELL-TYP      PIC S9(004) BINARY.          
                15 PO-DRVB1348-BZLP-BIS-STAT       PIC  X(010).                 
                15 PO-DRVB1348-BZLP-ART            PIC  X(001).                 
                15 PO-DRVB1348-BZLP-EQF-1   PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1348-BZLP-EQF-2   PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1348-BZLP-EH-PROZ PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1348-BZLP-VB-VERTR       PIC S9(004) BINARY.          
                15 PO-DRVB1348-BZLP-BS             PIC  X(001).                 
                15 PO-DRVB1348-BZLP-BS-PROZ PIC S9(003)V9(002) COMP-3.          
                15 PO-DRVB1348-BZLP-MS-NR          PIC S9(004) BINARY.          
                15 PO-DRVB1348-BZLP-VBP-NR         PIC S9(004) BINARY.          
                15 PO-DRVB1348-BZLP-STATUS         PIC  X(001).                 
                15 PO-DRVB1348-BZLP-LAST-UPDATE    PIC  X(026).                 
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1348   --------------------------*         
      *---------------------------------------------------------------*         
