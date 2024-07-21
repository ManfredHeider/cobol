      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1555.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1555.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:35:52   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Aug 21 2002 11:02:18   t13ex22
      *APA00206: Funktion DRPA1555
      *
      *   Rev 2.0   Jul 04 2002 15:29:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *            COPY-MEMBER: DRPA1555 FUNKTION UWPA0049           *
      *        WIENER ALLIANZ-LEBEN FORMAL PRUEFEN UND NORMIEREN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    19.08.02                                        *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!19.08.02!2.1 ! NEU                       ! MUND     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1555                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1555.
             10 PI-DRPA1555-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1555-PEG-DATEN-WI-ALL-L.
                COPY DRPA1554.
             10 PI-DRPA1555-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1555.
          05 PO-DRPA1555-RC                 PIC  X(003).
             88 PO-DRPA1555-RC-OK                VALUE 'OK '.
             88 PO-DRPA1555-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
