      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0989.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0989.CPv  $
      *
      *   Rev 2.1   Sep 03 2010 13:52:20   t13ex13
      *ASL00352: RGT: Günstigerprüfung
      *
      *   Rev 2.0   Aug 13 2010 10:27:00   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0989                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0202                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    13.08.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00352!13.08.10!2.1 ! NEU                       ! GUTFLEISCH*
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0989 - ERMITTELN SATZ AUS TABELLE ROWS.1.1    *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0989.
          05 PI-DRVB0989-DATEN.
             10 PI-DRVB0989-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB0989-STICHTAG           PIC  X(10).
             10 PI-DRVB0989-ANSPR-MONAT        PIC  X(10).

       01 PO-DRVB0989.
         03 PO-DRVB0989-RC                     PIC  X(03).
             88 PO-DRVB0989-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0989-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0989-DATEN.
          05 PO-DRVB0989-NI-VIVB0202.
             10 NI-DRVB0989-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB0989-STICHTAG           PIC S9(04)       BINARY.
             10 NI-DRVB0989-ANSPR-MONAT        PIC S9(04)       BINARY.
             10 NI-DRVB0989-ANSPR-VOM          PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZBALT-LAST-UPD   PIC S9(04)       BINARY.
             10 NI-DRVB0989-MS-NR              PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-12MON-UMS      PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-12MON-EQF      PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-03MON-UMS      PIC S9(04)       BINARY.
             10 NI-DRVB0989-EH-PRO-ABZUG       PIC S9(04)       BINARY.
             10 NI-DRVB0989-EH-NACH-PRO-ABZUG  PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-ZW-BETRAG      PIC S9(04)       BINARY.
             10 NI-DRVB0989-EQF-ABZUG          PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-BETRAG         PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-FAKTOR         PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-ANZ-MONATE     PIC S9(04)       BINARY.
             10 NI-DRVB0989-UMS-BED-ERF        PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-ALT-AUSZAHLUNG PIC S9(04)       BINARY.
             10 NI-DRVB0989-BOZ-NACH-DIR       PIC S9(04)       BINARY.
          05 PO-DRVB0989-DCL-VIVB0202.
             10 PO-DRVB0989-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB0989-STICHTAG           PIC  X(10).
             10 PO-DRVB0989-ANSPR-MONAT        PIC  X(10).
             10 PO-DRVB0989-ANSPR-VOM          PIC  X(10).
             10 PO-DRVB0989-BOZBALT-LAST-UPD   PIC  X(26).
             10 PO-DRVB0989-MS-NR              PIC S9(04)       BINARY.
             10 PO-DRVB0989-BOZ-12MON-UMS      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0989-BOZ-12MON-EQF      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB0989-BOZ-03MON-UMS      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0989-EH-PRO-ABZUG       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0989-EH-NACH-PRO-ABZUG  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0989-BOZ-ZW-BETRAG      PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0989-EQF-ABZUG          PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0989-BOZ-BETRAG         PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0989-BOZ-FAKTOR         PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB0989-BOZ-ANZ-MONATE     PIC S9(04)       BINARY.
             10 PO-DRVB0989-UMS-BED-ERF        PIC  X(01).
             10 PO-DRVB0989-BOZ-ALT-AUSZAHLUNG PIC  X(01).
             10 PO-DRVB0989-BOZ-NACH-DIR       PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0989   --------------------------*
      *---------------------------------------------------------------*
