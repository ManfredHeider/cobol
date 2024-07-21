      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0956.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0956.CPv  $
      *
      *   Rev 2.2   Mar 10 2014 08:52:48   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.1   03 Nov 2006 14:21:24   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:41:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:18:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0956 FUNKTION list PROVISIONSDETAIL      *
      *                                   PROV.-Bestand  BILD1 + 2   *
      *                          FüR BESTANDSMODUL UPPA0096          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Foltz                                           *
      *    DATUM:    08.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.05.98! 01  !NEU                       !FOLTZ     *
      *         !07.06.98!01.02!HISTORIENUMMER NEU        !BOHLINGER *
      * APA00452!03.11.06! 2.1 !ERW. WG. 3-STELL. PRODUKTE!WANG      *
      * APA01526!07.03.14! 2.2 !FOLGENUMMER DREISTELLIG   !STORCH    *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0956                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0956.
          05 PI-DRPA0956-UFKT                  PIC  9(02).
             88 PI-DRPA0956-GET-FIRST               VALUE 02.
             88 PI-DRPA0956-GET-NEXT                VALUE 03.
          05 PI-DRPA0956-NAME-ZW-SPEICHER      PIC  X(08).
          05 PI-DRPA0956-LOG-SATZ-NR           PIC  9(05).

          05 PI-DRPA0956-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0956-PDPT-ID               PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0956-S-PDPT-ID             PIC  X(001).

          05 PI-DRPA0956-AUSWAHL-KRITERIEN.
             10 PI-DRPA0956-PGEB-GES           PIC 9(002).
             10 PI-DRPA0956-VNR                PIC X(015).
             10 PI-DRPA0956-FAELLIG-VON        PIC X(010).
             10 PI-DRPA0956-FAELLIG-BIS        PIC X(010).
070314*      10 PI-DRPA0956-FOLGE-NR           PIC 9(002).
070314       10 PI-DRPA0956-FOLGE-NR           PIC 9(003).
             10 PI-DRPA0956-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA0956-ORDN-NR            PIC 9(002).
             10 PI-DRPA0956-HIST-NR            PIC S9(004) BINARY.

      *
       01 PO-DRPA0956.
          05 PO-DRPA0956-RC                    PIC  X(003).
             88 PO-DRPA0956-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0956-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA0956-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA0956-MELDUNG.
             10 PO-DRPA0956-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA0956-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA0956-ANZAHL-ERG-ZEILEN     PIC S9(004) BINARY.
          05 PO-DRPA0956-ERGEBNISZEILE.

             COPY DRPA0903.

      *
      ******** Ende Copy-Member **************************************
