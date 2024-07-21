      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1270.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1270.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:29:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 12:35:18   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Jun 24 1999 16:55:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1270                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN PVER MIT PRINTE>>                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    24.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.06.99! 01 ! NEU                       ! STORCH   *
      * APA00622!11.09.07!2.02! NEU: PO-DRPA1270-KNR-VGES ! STORCH   *
      * APA01101!23.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1270                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1270.
          05 PI-DRPA1270-DATEN.
             10 PI-DRPA1270-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1270.
          05 PO-DRPA1270-RC                         PIC  X(003).
             88 PO-DRPA1270-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1270-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1270-DATEN.
             10 PO-DRPA1270-PVER-VNR                PIC  X(015).
             10 PO-DRPA1270-PVER-PG-VBNR            PIC S9(009) BINARY.
             10 PO-DRPA1270-PVER-AKT-PIN            PIC S9(009) BINARY.
             10 PO-DRPA1270-PGEB-GES                PIC S9(004) BINARY.
             10 PO-DRPA1270-PVER-KUNDE-NAME         PIC  X(025).
             10 PO-DRPA1270-KNR-MANDANT             PIC S9(004)  BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
