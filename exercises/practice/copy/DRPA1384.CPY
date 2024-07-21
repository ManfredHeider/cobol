      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1384.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1384.CPv  $
      *
      *   Rev 2.2   25 May 2000 17:37:42   T13EX44
      *Korrektur Laenge PIN
      *
      *   Rev 2.1   22 May 2000 11:29:20   T13EX44
      *Neu
      *
      *   Rev 2.0   Apr 11 2000 16:58:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1384 FUNKTION VERTRAGSDATEN UND -STRUKTUR*
      *                                   ERMITTELN                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    12.05.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!12.05.00!2.1 !NEU                        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1384                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1384.
             10 PI-DRPA1384-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1384-VNR             PIC  X(015).
             10 PI-DRPA1384-PIN             PIC S9(009) BINARY.

       01 PO-DRPA1384.
          05 PO-DRPA1384-RC                 PIC  X(003).
             88 PO-DRPA1384-RC-OK                VALUE 'OK '.
             88 PO-DRPA1384-RC-NOK-MAX-EIN-PIN   VALUE 'NMP'.
             88 PO-DRPA1384-RC-NOK-VNR-PIN-DIF   VALUE 'NVP'.
             88 PO-DRPA1384-RC-NOK-AND-VNR-DA    VALUE 'NAV'.
          05 PO-DRPA1384-DATEN.
             10 PO-DRPA1384-KNR-VERTRAGSSTAND
                                            PIC S9(004)  BINARY.
             10 PO-DRPA1384-PIN-VNR-AKZ     PIC S9(004)  BINARY.
             10 PO-DRPA1384-PVER.
                COPY DRPA0408.
             10 PO-DRPA1384-PVPT.
                COPY DRPA0420.
      ******** ENDE COPY-MEMBER **************************************
