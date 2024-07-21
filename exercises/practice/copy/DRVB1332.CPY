      *****************************************************************         
      *    COPY-MEMBER: DRVB1332                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0514                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: G.BRUDER                                         *         
      *    DATUM:    22.02.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4138 !22.02.22! NEU                           ! G.BRUDER  *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB1332 - ERMITTELN SATZ AUS TABELLE CABB        *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB1332.                                                          
          05 PI-DRVB1332-DATEN.                                                 
             10 PI-DRVB1332-ST-NR              PIC S9(09)       BINARY.         
             10 PI-DRVB1332-CABB-ERM-VOM       PIC  X(10).                      
             10 PI-DRVB1332-KNR-ERM-ART        PIC S9(04)       BINARY.         
                                                                                
       01 PO-DRVB1332.                                                          
         03 PO-DRVB1332-RC                     PIC  X(03).                      
             88 PO-DRVB1332-RC-OK                   VALUE 'OK '.                
             88 PO-DRVB1332-RC-NOT-FOUND            VALUE 'NFD'.                
         03 PO-DRVB1332-DATEN.                                                  
          05 PO-DRVB1332-NI-VIVB0514.                                           
             10 NI-DRVB1332-ST-NR              PIC S9(04)       BINARY.         
             10 NI-DRVB1332-CABB-ERM-VOM       PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KNR-ERM-ART        PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KTY-ERM-ART        PIC S9(04)       BINARY.         
             10 NI-DRVB1332-CABB-MS-NR         PIC S9(04)       BINARY.         
             10 NI-DRVB1332-CABB-LAST-UPDATE   PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KTY-WAEHRUNG       PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KNR-WAEHRUNG       PIC S9(04)       BINARY.         
             10 NI-DRVB1332-CABB-NETTO-BETR    PIC S9(04)       BINARY.         
             10 NI-DRVB1332-CABB-BRUTTO-BETR   PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KTY-BERECH-STAT    PIC S9(04)       BINARY.         
             10 NI-DRVB1332-KNR-BERECH-STAT    PIC S9(04)       BINARY.         
          05 PO-DRVB1332-DCL-VIVB0514.                                          
             10 PO-DRVB1332-ST-NR              PIC S9(09)       BINARY.         
             10 PO-DRVB1332-CABB-ERM-VOM       PIC  X(10).                      
             10 PO-DRVB1332-KNR-ERM-ART        PIC S9(04)       BINARY.         
             10 PO-DRVB1332-KTY-ERM-ART        PIC S9(04)       BINARY.         
             10 PO-DRVB1332-CABB-MS-NR         PIC S9(04)       BINARY.         
             10 PO-DRVB1332-CABB-LAST-UPDATE   PIC  X(26).                      
             10 PO-DRVB1332-KTY-WAEHRUNG       PIC S9(04)       BINARY.         
             10 PO-DRVB1332-KNR-WAEHRUNG       PIC S9(04)       BINARY.         
             10 PO-DRVB1332-CABB-NETTO-BETR    PIC S9(09)V9(02) COMP-3.         
             10 PO-DRVB1332-CABB-BRUTTO-BETR   PIC S9(09)V9(02) COMP-3.         
             10 PO-DRVB1332-KTY-BERECH-STAT    PIC S9(04)       BINARY.         
             10 PO-DRVB1332-KNR-BERECH-STAT    PIC S9(04)       BINARY.         
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB1332   --------------------------*         
      *---------------------------------------------------------------*         
