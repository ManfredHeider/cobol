      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1365.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1365.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:31:18   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Apr 14 2000 14:19:28   T13EX21
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1365                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BADENIA PM FORMAL PRÜFEN UND NORMIEREN>> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    12.04.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!12.04.00!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1365                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1365.
          05 PI-DRPA1365-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1365-DATEN.
             10 PI-DRPA1365-BADENIA.
                COPY DRPA1330.
080408    05 PI-DRPA1365-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1365.
          05 PO-DRPA1365-RC                         PIC  X(003).
             88 PO-DRPA1365-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1365-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************
