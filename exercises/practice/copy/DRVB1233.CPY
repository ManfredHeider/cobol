      *****************************************************************
      *    COPY-MEMBER: DRVB1233                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0234                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    09.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !09.09.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
210122* JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
210122* ------------------------------------------------------------- *
210122* VLA-3688 !21.01.22! EQF-2 HINZU --> A-VIEW,       !           *
210122*          !        ! PVCS-KOPF ENTFERNT            ! KLEEMEIER *
      *          !        !                               !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1233 - ERMITTELN SATZ AUS TABELLE FAPP (LFDNR)*
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1233.
          05 PI-DRVB1233-DATEN.
             10 PI-DRVB1233-FAPP-LFDNR         PIC S9(04)       BINARY.
             10 PI-DRVB1233-KNR-GESELL-TYP     PIC S9(04)       BINARY.
 
       01 PO-DRVB1233.
         03 PO-DRVB1233-RC                     PIC  X(03).
             88 PO-DRVB1233-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1233-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1233-DATEN.
210122****   05 PO-DRVB1233-NI-VIVB0234.
210122    05 PO-DRVB1233-NI-VIVBA234.
             10 NI-DRVB1233-FAPP-LFDNR         PIC S9(04)       BINARY.
             10 NI-DRVB1233-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1233-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-BIS-STAT      PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-ART           PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-EQF           PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-EH-PROZ       PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-VB-VERTR      PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-BS            PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-MIN-PROZ      PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-STATUS        PIC S9(04)       BINARY.
             10 NI-DRVB1233-FAPP-LAST-UPDATE   PIC S9(04)       BINARY.
210122       10 NI-DRVB1233-FAPP-EQF-2         PIC S9(04)       BINARY.
210122****05 PO-DRVB1233-DCL-VIVB0234.
210122    05 PO-DRVB1233-DCL-VIVBA234.
             10 PO-DRVB1233-FAPP-LFDNR         PIC S9(04)       BINARY.
             10 PO-DRVB1233-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1233-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1233-FAPP-BIS-STAT      PIC  X(10).
             10 PO-DRVB1233-FAPP-ART           PIC  X(01).
             10 PO-DRVB1233-FAPP-EQF           PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1233-FAPP-EH-PROZ       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1233-FAPP-VB-VERTR      PIC S9(04)       BINARY.
             10 PO-DRVB1233-FAPP-BS            PIC  X(01).
             10 PO-DRVB1233-FAPP-MIN-PROZ      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1233-FAPP-STATUS        PIC  X(01).
             10 PO-DRVB1233-FAPP-LAST-UPDATE   PIC  X(26).
210122       10 PO-DRVB1233-FAPP-EQF-2         PIC S9(03)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1233   --------------------------*
      *---------------------------------------------------------------*
