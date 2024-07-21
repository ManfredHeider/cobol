      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0876.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0876.CPv  $
      *
      *   Rev 2.2   23 Nov 2006 14:12:10   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   03 Nov 2006 09:20:00   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 13:23:36   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:48:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0876 FUNKTION LIST VERTRAGSPRODUKTTEILE  *
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
      *         !08.02.99!1.02!AVERM-KD-Betr entfällt     ! bohlinger*
      * APA00452!02.11.06!2.1 !ERW. WG. 3-STELL. PRODUKTE ! WANG     *
      *         !23.11.06!2.2 !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0876                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0876.
          05 PI-DRPA0876-UFKT                  PIC  9(02).
          05 PI-DRPA0876-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA0876-AUSWAHL-KRITERIEN.
             10 PI-DRPA0876-PGEB-GES           PIC  X(002).
             10 PI-DRPA0876-VNR                PIC  X(015).
             10 PI-DRPA0876-S-PROD-SCHL        PIC  X(001).
021106*      10 PI-DRPA0876-PROD-SCHL          PIC  X(002).
231106*      10 PI-DRPA0876-PROD-PROD-SCHL     PIC  X(003).
231106       10 PI-DRPA0876-PROD-PROD-SCHL     PIC S9(004) BINARY.
      *
       01 PO-DRPA0876.
          05 PO-DRPA0876-RC                    PIC  X(003).
             88 PO-DRPA0876-RC-OK                   VALUE 'OK '.
             88 PO-DRPA0876-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA0876-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA0876-ERGEBNISZEILE.
             COPY DRPA0970.
      *
