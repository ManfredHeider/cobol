      *****************************************************************
      *    COPY-MEMBER: DRVB1225                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0234                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.07.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !31.07.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
151121* JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
151121* ------------------------------------------------------------- *
151121* VLA-3688 !15.11.21! EQF-2 HINZU --> A-VIEW,       !           *
151121*          !        ! PVCS-KOPF ENTFERNT            ! KLEEMEIER *
      *          !        !                               !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1225 - ERMITTELN SATZ AUS TABELLE FAPP (OFFEN)*
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1225.
          05 PI-DRVB1225-DATEN.
             10 PI-DRVB1225-DUMMY              PIC  X(01).
 
       01 PO-DRVB1225.
         03 PO-DRVB1225-RC                     PIC  X(03).
             88 PO-DRVB1225-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1225-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1225-DATEN.
151121****05 PO-DRVB1225-NI-VIVB0234.
151121    05 PO-DRVB1225-NI-VIVBA234.
             10 NI-DRVB1225-FAPP-LFDNR         PIC S9(04)       BINARY.
             10 NI-DRVB1225-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1225-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-BIS-STAT      PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-ART           PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-EQF           PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-EH-PROZ       PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-VB-VERTR      PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-BS            PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-MIN-PROZ      PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-STATUS        PIC S9(04)       BINARY.
             10 NI-DRVB1225-FAPP-LAST-UPDATE   PIC S9(04)       BINARY.
151121       10 NI-DRVB1225-FAPP-EQF-2         PIC S9(04)       BINARY.
151121****05 PO-DRVB1225-DCL-VIVB0234.
151121    05 PO-DRVB1225-DCL-VIVBA234.
             10 PO-DRVB1225-FAPP-LFDNR         PIC S9(04)       BINARY.
             10 PO-DRVB1225-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1225-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1225-FAPP-BIS-STAT      PIC  X(10).
             10 PO-DRVB1225-FAPP-ART           PIC  X(01).
             10 PO-DRVB1225-FAPP-EQF           PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1225-FAPP-EH-PROZ       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1225-FAPP-VB-VERTR      PIC S9(04)       BINARY.
             10 PO-DRVB1225-FAPP-BS            PIC  X(01).
             10 PO-DRVB1225-FAPP-MIN-PROZ      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1225-FAPP-STATUS        PIC  X(01).
             10 PO-DRVB1225-FAPP-LAST-UPDATE   PIC  X(26).
151121       10 PO-DRVB1225-FAPP-EQF-2         PIC S9(03)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1225   --------------------------*
      *---------------------------------------------------------------*
