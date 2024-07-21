      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1545.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1545.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:35:18   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Aug 21 2002 09:07:30   t13ae46
      *APA00206: Neue Funktion  
      *
      *   Rev 2.0   04 Jul 2001 11:11:44   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *            COPY-MEMBER: DRPA1545 FUNKTION UWPA0049           *
      *        GENERALI LEBEN-PM FORMAL PRUEFEN UND NORMIEREN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    25.06.2002                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!25.06.02!2.1 ! NEU                       ! WANG     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1545                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1545.
             10 PI-DRPA1545-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1545-PEG-DATEN-GENE-L.
                COPY DRPA1546.
080408       10 PI-DRPA1545-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1545.
          05 PO-DRPA1545-RC                 PIC  X(003).
             88 PO-DRPA1545-RC-OK                VALUE 'OK '.
             88 PO-DRPA1545-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
