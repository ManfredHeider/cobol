      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1571.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1571.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:36:26   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Mar 16 2004 11:52:30   t13ex44
      *APA00270 - PEG für Ges. 39 und 69
      *
      *   Rev 2.0   Jul 04 2002 15:30:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1571 FUNKTION UWPA0047                   *
      *                 GEN.-BANK FORMAL PRUEFEN UND NORMIEREN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    11.03.2004                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00270!11.03.04!2.1 ! NEU                       ! SCHEIDEL *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1571                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1571.
             10 PI-DRPA1571-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1571-PEG-DATEN-GEN-B.
                COPY DRPA1573.
             10 PI-DRPA1571-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1571.
          05 PO-DRPA1571-RC                 PIC  X(003).
             88 PO-DRPA1571-RC-OK                VALUE 'OK '.
             88 PO-DRPA1571-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
