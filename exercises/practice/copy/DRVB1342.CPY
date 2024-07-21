      *****************************************************************
      *    COPY-MEMBER: DRVB1342                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVBC046                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    26.04.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * -        !26.04.22! NEU                           ! -         *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1342 - ERMITTELN SATZ AUS TABELLE KLZA        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1342.
          05 PI-DRVB1342-DATEN.
             10 PI-DRVB1342-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1342-KLZA-ANSPR-VOM     PIC  X(10).
             10 PI-DRVB1342-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.

       01 PO-DRVB1342.
         03 PO-DRVB1342-RC                     PIC  X(03).
             88 PO-DRVB1342-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1342-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1342-DATEN.
          05 PO-DRVB1342-NI-VIVBC046.
             10 NI-DRVB1342-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1342-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-BEITRAG       PIC S9(04)       BINARY.
             10 NI-DRVB1342-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1342-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-ANSPR-BETRAG  PIC S9(04)       BINARY.
             10 NI-DRVB1342-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-CENTRAL-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-EH-NNE        PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-JEQF          PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-ERF-INDX      PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-AS-CLUB-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-BES-345-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-STRUKTUR      PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-PRAXIS        PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-AUSTRITT-KZ   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-BES-229-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-BV-FEHLT-KZ   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-PFAEND-KZ     PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-SPERRE-KZ     PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-SPERRE-TEXT   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-EH-NNE-SOLL   PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-BQF           PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-SACH-EG-BASIS PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-SACH-EG-VERGL PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-SACH-EG-STEIG PIC S9(04)       BINARY.
             10 NI-DRVB1342-KLZA-ZUGANG        PIC S9(04)       BINARY.
          05 PO-DRVB1342-DCL-VIVBC046.
             10 PO-DRVB1342-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1342-KLZA-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1342-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1342-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1342-KLZA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1342-KLZA-BEITRAG       PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1342-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1342-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1342-KLZA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1342-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1342-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1342-KLZA-CENTRAL-KZ    PIC  X(01).
             10 PO-DRVB1342-KLZA-EH-NNE        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1342-KLZA-JEQF          PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1342-KLZA-ERF-INDX      PIC  X(01).
             10 PO-DRVB1342-KLZA-AS-CLUB-KZ    PIC  X(01).
             10 PO-DRVB1342-KLZA-BES-345-KZ    PIC  X(01).
             10 PO-DRVB1342-KLZA-STRUKTUR      PIC S9(04)       BINARY.
             10 PO-DRVB1342-KLZA-PRAXIS        PIC S9(04)       BINARY.
             10 PO-DRVB1342-KLZA-AUSTRITT-KZ   PIC  X(01).
             10 PO-DRVB1342-KLZA-BES-229-KZ    PIC  X(01).
             10 PO-DRVB1342-KLZA-BV-FEHLT-KZ   PIC  X(01).
             10 PO-DRVB1342-KLZA-PFAEND-KZ     PIC  X(01).
             10 PO-DRVB1342-KLZA-SPERRE-KZ     PIC  X(01).
             10 PO-DRVB1342-KLZA-SPERRE-TEXT   PIC  X(40).
             10 PO-DRVB1342-KLZA-EH-NNE-SOLL   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1342-KLZA-BQF           PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1342-KLZA-SACH-EG-BASIS PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1342-KLZA-SACH-EG-VERGL PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1342-KLZA-SACH-EG-STEIG PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1342-KLZA-ZUGANG        PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1342   --------------------------*
      *---------------------------------------------------------------*
