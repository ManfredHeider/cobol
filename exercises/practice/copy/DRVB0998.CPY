      *****************************************************************
      *    COPY-MEMBER: DRVB0998                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0205                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    28.09.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01103!28.09.10!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0998 - ERMITTELN SATZ AUS TABELLE QEDD        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0998.
          05 PI-DRVB0998-DATEN.
             10 PI-DRVB0998-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0998-ED-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0998-QEDD-DATUM         PIC  X(10).

       01 PO-DRVB0998.
         03 PO-DRVB0998-RC                     PIC  X(03).
             88 PO-DRVB0998-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0998-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0998-DATEN.
          05 PO-DRVB0998-NI-VIVB0205.
             10 NI-DRVB0998-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0998-ED-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-ED-ZUSATZ     PIC S9(04)       BINARY.
             10 NI-DRVB0998-ED-GENERATION      PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-DATUM         PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-ED-EINH       PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-EINH-MAX      PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-EINH-GEW      PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-ANSPR-BETRAG  PIC S9(04)       BINARY.
             10 NI-DRVB0998-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0998-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0998-DL-NET-GES-EH      PIC S9(04)       BINARY.
             10 NI-DRVB0998-DL-EH-MAX-PROZ     PIC S9(04)       BINARY.
             10 NI-DRVB0998-ED-EINH-FAKTOR     PIC S9(04)       BINARY.
             10 NI-DRVB0998-QUARTAL            PIC S9(04)       BINARY.
             10 NI-DRVB0998-QEDD-LAST-UPDATE   PIC S9(04)       BINARY.
          05 PO-DRVB0998-DCL-VIVB0205.
             10 PO-DRVB0998-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0998-ED-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0998-QEDD-ED-ZUSATZ     PIC S9(04)       BINARY.
             10 PO-DRVB0998-ED-GENERATION      PIC S9(04)       BINARY.
             10 PO-DRVB0998-QEDD-DATUM         PIC  X(10).
             10 PO-DRVB0998-QEDD-ED-EINH       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0998-QEDD-EINH-MAX      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0998-QEDD-EINH-GEW      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0998-QEDD-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0998-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0998-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0998-DL-NET-GES-EH      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0998-DL-EH-MAX-PROZ     PIC S9(03)V9(03) COMP-3.
             10 PO-DRVB0998-ED-EINH-FAKTOR     PIC S9(03)V9(03) COMP-3.
             10 PO-DRVB0998-QUARTAL            PIC  X(17).
             10 PO-DRVB0998-QEDD-LAST-UPDATE   PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0998   --------------------------*
      *---------------------------------------------------------------*
