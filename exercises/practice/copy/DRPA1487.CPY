      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1487.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1487.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:32:56   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   30 Mar 2001 14:42:48   T13EX43
      *APA00131 : PEG DGI-PM formal prüfen und normieren
      *
      *   Rev 2.0   09 Jan 2001 10:22:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1487 FUNKTION UWPA0046                   *
      *                 DGI-PM FORMAL PRUEFEN UND NORMIEREN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    29.03.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!29.03.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1487                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1487.
             10 PI-DRPA1487-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1487-PEG-DATEN-DGI.
                COPY DRPA1486.
080408       10 PI-DRPA1487-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1487.
          05 PO-DRPA1487-RC                 PIC  X(003).
             88 PO-DRPA1487-RC-OK                VALUE 'OK '.
             88 PO-DRPA1487-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
