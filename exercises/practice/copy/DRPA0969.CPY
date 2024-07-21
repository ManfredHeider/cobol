      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0969.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0969.CPv  $
      *
      *   Rev 2.1   03 Nov 2006 09:57:08   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 16:25:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0969 FUNKTION LIST VERTRAGSPRODUKTTEILE  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.06.98! 01 !NEU                        ! STORCH   *
      * APA00452!02.11.06!2.2 !ERW. WG. 3-STELL. PRODUKTE ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0969                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0969.
          05 PI-DRPA0969-UFKT                  PIC  9(02).
             88 PI-DRPA0969-GET-FIRST               VALUE 02.
             88 PI-DRPA0969-GET-NEXT                VALUE 03.
          05 PI-DRPA0969-NAME-ZW-SPEICHER      PIC  X(08).
          05 PI-DRPA0969-LOG-SATZ-NR           PIC  9(05).

          05 PI-DRPA0969-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0969-AUSWAHL-KRITERIEN.
             10 PI-DRPA0969-PGEB-GES           PIC 9(002).
             10 PI-DRPA0969-VNR                PIC X(015).
             10 PI-DRPA0969-S-PROD-SCHL        PIC X(001).
021106*      10 PI-DRPA0969-PROD-SCHL          PIC 9(002).
021106       10 PI-DRPA0969-PROD-PROD-SCHL     PIC S9(004) BINARY.
      *
       01 PO-DRPA0969.
          05 PO-DRPA0969-RC                    PIC  X(003).
             88 PO-DRPA0969-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0969-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA0969-RC-LEER                 VALUE 'LER'.

          05  PO-DRPA0969-ANZAHL-ERG-ZEILEN    PIC  9(05).
          05  PO-DRPA0969-LOG-SATZ-NR-1        PIC  9(05).
          05  PO-DRPA0969-KZ-WEITERE-PROD      PIC  X(01).

          05 PO-DRPA0969-ERGEBNISZEILE.
             COPY DRPA0970.
      *
      ******** Ende Copy-Member **************************************
