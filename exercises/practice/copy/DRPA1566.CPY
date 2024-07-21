      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1566.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1566.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:36:04   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Dec 22 2003 14:04:00   t13ae46
      *APA00262:Neu
      *
      *   Rev 2.0   Jul 04 2002 15:30:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1566 FUNKTION UWPA0047                   *
      *                 DSL-PM FORMAL PRUEFEN UND NORMIEREN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    05.12.2003                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00262!05.12.03!2.1 ! NEU                       ! WANG     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1566                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1566.
             10 PI-DRPA1566-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1566-PEG-DATEN-DSL-BANK.
                COPY DRPA1567.
             10 PI-DRPA1566-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1566.
          05 PO-DRPA1566-RC                 PIC  X(003).
             88 PO-DRPA1566-RC-OK                VALUE 'OK '.
             88 PO-DRPA1566-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
