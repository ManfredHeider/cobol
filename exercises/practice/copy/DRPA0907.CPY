      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0907.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0907.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 08:52:56   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.2   Nov 09 2006 12:08:06   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Nov 03 2006 15:37:32   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:29:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0907 FUNKTION LIST PROVISIONSDETAIL      *
      *                                   PROV.-BESTAND  BILD1 + 2   *
      *                          FÜR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                           *
      *    DATUM:    08.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.05.98! 01 !NEU                        ! FOLTZ    *
      *         !        !    !NAMENSANPASSUNGEN          ! BOHLINGER*
      * APA00452!01.11.06! 2.1! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA01526!07.03.14! 2.2! FOLGENUMMER DREISTELLIG   ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0907                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0907.
          05 PI-DRPA0907-UFKT                  PIC  9(02).
          05 PI-DRPA0907-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0907-AUSWAHL-KRITERIEN.
             10 PI-DRPA0907-PGEB-GES           PIC 9(002).
             10 PI-DRPA0907-VNR                PIC X(015).
             10 PI-DRPA0907-FAELLIG-VON        PIC X(010).
             10 PI-DRPA0907-FAELLIG-BIS        PIC X(010).
070314*      10 PI-DRPA0907-FOLGE-NR           PIC 9(002).
07031        10 PI-DRPA0907-FOLGE-NR           PIC 9(003).
             10 PI-DRPA0907-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA0907-ORDN-NR            PIC 9(002).
             10 PI-DRPA0907-HIST-NR            PIC S9(004) BINARY.

      *
       01 PO-DRPA0907.
          05 PO-DRPA0907-RC                    PIC  X(003).
             88 PO-DRPA0907-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0907-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA0907-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA0907-ERGEBNISZEILE.
             COPY DRPA0903.
      *
      ******** Ende Copy-Member **************************************
