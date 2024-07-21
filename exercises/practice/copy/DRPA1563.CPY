      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1563.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1563.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:31:04   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:56:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Jan 13 2009 11:30:38   t13ex21
      *FPA01117: Prüfung auf doppelte DTA's mit Mandant
      *
      *   Rev 2.1   Sep 08 2003 13:51:26   t13ex21
      *APA00248 - Neuerstellung
      *
      *   Rev 2.0   Jul 04 2002 15:30:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1563                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PRÜFEN, OB DTA MIT ERSTELLDATUM,         *
      *                   KONTROLLWERT UND SATZANZAHL VORHANDEN IST>> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    05.09.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00248!05.09.03! 01 ! NEUERSTELLUNG             ! STORCH   *
      * FPA01117!13.01.09!2.02! ERWEITERUNG UM MANDANT    ! STORCH   *
      * APA01101!23.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1563                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1563.
          05 PI-DRPA1563-DATEN.
             10 PI-DRPA1563-PGEB-GES       PIC S9(004) BINARY.
             10 PI-DRPA1563-ERSTELL-DAT    PIC  X(010).
             10 PI-DRPA1563-ANZ-SATZ       PIC S9(009) BINARY.
             10 PI-DRPA1563-KONTR-WERT     PIC S9(011)V99 COMP-3.
             10 PI-DRPA1563-KNR-MANDANT    PIC S9(004) BINARY.
      *
       01 PO-DRPA1563.
          05 PO-DRPA1563-RC                  PIC  X(003).
             88 PO-DRPA1563-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1563-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
