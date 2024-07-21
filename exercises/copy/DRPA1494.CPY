      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1494.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1494.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:33:18   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   20 Jul 2001 09:41:00   t13ex21
      *APA00133 - Neuerstellung
      *
      *   Rev 2.0   10 Apr 2001 08:42:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1494 FUNKTION UWPA0047                   *
      *         DRESDNER BANK-PM FORMAL PRUEFEN UND NORMIEREN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    21.06.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!21.06.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1494                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1494.
             10 PI-DRPA1494-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1494-PEG-DATEN-DREBA.
                COPY DRPA1497.
080408       10 PI-DRPA1494-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1494.
          05 PO-DRPA1494-RC                 PIC  X(003).
             88 PO-DRPA1494-RC-OK                VALUE 'OK '.
             88 PO-DRPA1494-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
