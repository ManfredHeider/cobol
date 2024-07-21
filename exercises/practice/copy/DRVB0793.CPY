      *****************************************************************         
      *    COPY-MEMBER: DRVB0793                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0096                   *         
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: T13AE30                                          *         
      *    DATUM:    05.03.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *         
      * --------+--------+----+---------------------------+-----------*         
      * VLA-1995!05.03.21!2.1 ! NEU                       ! T13AE30   *         
      *         !        !    !                           !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                               *         
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *         
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *         
      *                                                               *         
      *    FUNKTION DRVB0793 - ERMITTELN SATZ AUS TABELLE EBB         *         
      *                                                               *         
      *****************************************************************         
                                                                                
       01 PI-DRVB0793.                                                          
          05 PI-DRVB0793-DATEN.                                                 
             10 PI-DRVB0793-MA-ST-NR           PIC S9(09)       BINARY.         
             10 PI-DRVB0793-KNR-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 PI-DRVB0793-LAUF-DAT           PIC  X(10).                      
                                                                                
       01 PO-DRVB0793.                                                          
         03 PO-DRVB0793-RC                     PIC  X(03).                      
             88 PO-DRVB0793-RC-OK                   VALUE 'OK '.                
             88 PO-DRVB0793-RC-NOT-FOUND            VALUE 'NFD'.                
         03 PO-DRVB0793-DATEN.                                                  
          05 PO-DRVB0793-NI-VIVB0096.                                           
             10 NI-DRVB0793-MA-ST-NR           PIC S9(04)       BINARY.         
             10 NI-DRVB0793-KTY-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 NI-DRVB0793-KNR-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 NI-DRVB0793-LAUF-DAT           PIC S9(04)       BINARY.         
             10 NI-DRVB0793-STF-AKT            PIC S9(04)       BINARY.         
             10 NI-DRVB0793-STF-SEIT           PIC S9(04)       BINARY.         
             10 NI-DRVB0793-ZPKT-VON           PIC S9(04)       BINARY.         
             10 NI-DRVB0793-ZPKT-BIS           PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EH-EIG             PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EH-EIG-KZ          PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EQF-EIG            PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EQF-EIG-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EH-GES             PIC S9(04)       BINARY.         
             10 NI-DRVB0793-KORR-EH-GES        PIC S9(04)       BINARY.         
             10 NI-DRVB0793-KORR-EH-GES-KZ     PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EQF-GES            PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EQF-GES-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EH-NEU-PART        PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EH-PART-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0793-PROZ-KLAUS         PIC S9(04)       BINARY.         
             10 NI-DRVB0793-EIGEN-MAX          PIC S9(04)       BINARY.         
             10 NI-DRVB0793-GESAMT-KZ          PIC S9(04)       BINARY.         
             10 NI-DRVB0793-LAST-UPDATE        PIC S9(04)       BINARY.         
          05 PO-DRVB0793-DCL-VIVB0096.                                          
             10 PO-DRVB0793-MA-ST-NR           PIC S9(09)       BINARY.         
             10 PO-DRVB0793-KTY-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 PO-DRVB0793-KNR-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 PO-DRVB0793-LAUF-DAT           PIC  X(10).                      
             10 PO-DRVB0793-STF-AKT            PIC S9(04)       BINARY.         
             10 PO-DRVB0793-STF-SEIT           PIC  X(10).                      
             10 PO-DRVB0793-ZPKT-VON           PIC  X(10).                      
             10 PO-DRVB0793-ZPKT-BIS           PIC  X(10).                      
             10 PO-DRVB0793-EH-EIG             PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-EH-EIG-KZ          PIC  X(01).                      
             10 PO-DRVB0793-EQF-EIG            PIC S9(03)V9(02) COMP-3.         
             10 PO-DRVB0793-EQF-EIG-KZ         PIC  X(01).                      
             10 PO-DRVB0793-EH-GES             PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-KORR-EH-GES        PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-KORR-EH-GES-KZ     PIC  X(01).                      
             10 PO-DRVB0793-EQF-GES            PIC S9(03)V9(02) COMP-3.         
             10 PO-DRVB0793-EQF-GES-KZ         PIC  X(01).                      
             10 PO-DRVB0793-EH-NEU-PART        PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-EH-PART-KZ         PIC  X(01).                      
             10 PO-DRVB0793-PROZ-KLAUS         PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-EIGEN-MAX          PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0793-GESAMT-KZ          PIC  X(01).                      
             10 PO-DRVB0793-LAST-UPDATE        PIC  X(26).                      
      *---------------------------------------------------------------*         
      *----   ENDE  COPY-MEMBER  DRVB0793   --------------------------*         
      *---------------------------------------------------------------*         
