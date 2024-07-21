      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1550.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1550.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:35:42   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Aug 21 2002 09:15:52   t13ae46
      *APA00206: Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:52   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *            COPY-MEMBER: DRPA1550 FUNKTION UWPA0015           *
      *        GENERALI SACH-PM FORMAL PRUEFEN UND NORMIEREN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    04.07.2002                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!04.07.02!2.1 ! NEU                       ! WANG     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1550                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1550.
             10 PI-DRPA1550-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1550-PEG-DATEN-GENE-S.
                COPY DRPA1551.
080408       10 PI-DRPA1550-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1550.
          05 PO-DRPA1550-RC                 PIC  X(003).
             88 PO-DRPA1550-RC-OK                VALUE 'OK '.
             88 PO-DRPA1550-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
