      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0991.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0991.CPv  $
      *
      *   Rev 2.1   Sep 03 2010 13:52:58   t13ex13
      *ASL00352: RGT: Günstigerprüfung
      *
      *   Rev 2.0   Aug 13 2010 10:59:12   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0991                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0203                    *
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
      *    FUNKTION DRVB0991 - ERMITTELN SATZ AUS TABELLE BOZBNEU     *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0991.
          05 PI-DRVB0991-DATEN.
             10 PI-DRVB0991-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB0991-STICHTAG           PIC  X(10).
             10 PI-DRVB0991-ANSPR-MONAT        PIC  X(10).

       01 PO-DRVB0991.
         03 PO-DRVB0991-RC                     PIC  X(03).
             88 PO-DRVB0991-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0991-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0991-DATEN.
          05 PO-DRVB0991-NI-VIVB0203.
             10 NI-DRVB0991-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB0991-STICHTAG           PIC S9(04)       BINARY.
             10 NI-DRVB0991-ANSPR-MONAT        PIC S9(04)       BINARY.
             10 NI-DRVB0991-ANSPR-VOM          PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZBNEU-LAST-UPD   PIC S9(04)       BINARY.
             10 NI-DRVB0991-UMS-12-EIG         PIC S9(04)       BINARY.
             10 NI-DRVB0991-UMS-12-GRU         PIC S9(04)       BINARY.
             10 NI-DRVB0991-UMS-12-GES         PIC S9(04)       BINARY.
             10 NI-DRVB0991-EQF-12-GRU         PIC S9(04)       BINARY.
             10 NI-DRVB0991-UMS-03-GRU         PIC S9(04)       BINARY.
             10 NI-DRVB0991-EH-PRO-ABZUG       PIC S9(04)       BINARY.
             10 NI-DRVB0991-EH-NACH-PRO-ABZUG  PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-BASIS-BETRAG   PIC S9(04)       BINARY.
             10 NI-DRVB0991-UMS-03-PART        PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-AUFBAU-BETRAG  PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-ZW-SUMME       PIC S9(04)       BINARY.
             10 NI-DRVB0991-EQF-ABZUG          PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-BETRAG         PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-FAKTOR-BASIS   PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-FAKTOR-AUFBAU  PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-ANZ-MONATE     PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-NEU-AUSZAHLUNG PIC S9(04)       BINARY.
             10 NI-DRVB0991-BOZ-NACH-DIR       PIC S9(04)       BINARY.
          05 PO-DRVB0991-DCL-VIVB0203.
             10 PO-DRVB0991-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB0991-STICHTAG           PIC  X(10).
             10 PO-DRVB0991-ANSPR-MONAT        PIC  X(10).
             10 PO-DRVB0991-ANSPR-VOM          PIC  X(10).
             10 PO-DRVB0991-BOZBNEU-LAST-UPD   PIC  X(26).
             10 PO-DRVB0991-UMS-12-EIG         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-UMS-12-GRU         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-UMS-12-GES         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-EQF-12-GRU         PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB0991-UMS-03-GRU         PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-EH-PRO-ABZUG       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-EH-NACH-PRO-ABZUG  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-BOZ-BASIS-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0991-UMS-03-PART        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0991-BOZ-AUFBAU-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0991-BOZ-ZW-SUMME       PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0991-EQF-ABZUG          PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0991-BOZ-BETRAG         PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0991-BOZ-FAKTOR-BASIS   PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB0991-BOZ-FAKTOR-AUFBAU  PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB0991-BOZ-ANZ-MONATE     PIC S9(04)       BINARY.
             10 PO-DRVB0991-BOZ-NEU-AUSZAHLUNG PIC  X(01).
             10 PO-DRVB0991-BOZ-NACH-DIR       PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0991   --------------------------*
      *---------------------------------------------------------------*
