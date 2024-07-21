      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1500.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1500.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:33:28   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   11 Jul 2001 10:53:00   t13ex43
      *APA00133 : PEG Advocard
      *
      *   Rev 2.0   10 Apr 2001 08:42:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1500 FUNKTION UWPA0015                   *
      *                 ADVOCARD-PM FORMAL PRUEFEN UND NORMIEREN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    03.07.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!03.07.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1500                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1500.
             10 PI-DRPA1500-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1500-PEG-DATEN-ADVOCARD.
                COPY DRPA1499.
080408       10 PI-DRPA1500-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1500.
          05 PO-DRPA1500-RC                 PIC  X(003).
             88 PO-DRPA1500-RC-OK                VALUE 'OK '.
             88 PO-DRPA1500-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
