      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1398.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1398.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:31:28   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   12 Jul 2000 13:17:02   T13EX22
      *APA00098: Anpassungen PEG III
      *
      *   Rev 2.0   Apr 11 2000 16:58:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1398
      *    --------------------------------------------------------
      *    FUNKTIONSAUFRUF <<III PM FORMAL PRÜFEN UND NORMIEREN>>
      *    --------------------------------------------------------
      *    ERSTELLT: MUND
      *    DATUM:    08.08.2000
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00098!08.08.00! 02 ! NEUERSTELLUNG             ! MUND     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1398                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1398.
          05 PI-DRPA1398-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1398-DATEN.
             10 PI-DRPA1398-III.
                COPY DRPA1393.
080408    05 PI-DRPA1398-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1398.
          05 PO-DRPA1398-RC                         PIC  X(003).
             88 PO-DRPA1398-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1398-RC-VSTAT-GEFUELLT            VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER ***************************************
