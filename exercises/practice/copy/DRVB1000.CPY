      *****************************************************************
      *    COPY-MEMBER: DRVB1000                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0206                    *
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
      *    FUNKTION DRVB1000 - ERMITTELN SATZ AUS TABELLE QEDA        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1000.
          05 PI-DRVB1000-DATEN.
             10 PI-DRVB1000-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1000-ED-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1000-QEDA-ANSPR-VOM     PIC  X(10).

       01 PO-DRVB1000.
         03 PO-DRVB1000-RC                     PIC  X(03).
             88 PO-DRVB1000-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1000-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1000-DATEN.
          05 PO-DRVB1000-NI-VIVB0206.
             10 NI-DRVB1000-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1000-ED-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1000-QEDA-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1000-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1000-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1000-QEDA-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1000-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1000-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1000-QEDA-ANSPR-BETRAG  PIC S9(04)       BINARY.
             10 NI-DRVB1000-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1000-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1000-DCL-VIVB0206.
             10 PO-DRVB1000-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1000-ED-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1000-QEDA-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1000-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1000-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1000-QEDA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1000-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1000-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1000-QEDA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1000-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1000-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1000   --------------------------*
      *---------------------------------------------------------------*
