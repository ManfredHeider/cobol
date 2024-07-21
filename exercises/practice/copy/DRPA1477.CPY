      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1477.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1477.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:32:34   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   09 Mar 2001 12:09:44   T13EX21
      *APA00131 - Neuerstellung
      *
      *   Rev 2.0   09 Jan 2001 10:22:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1477 FUNKTION UWPA0045                   *
      *         AML-DARLEHEN-PM FORMAL PRUEFEN UND NORMIEREN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    28.02.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!28.02.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1477                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1477.
             10 PI-DRPA1477-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1477-PEG-DATEN-AML-DARL.
                COPY DRPA1478.
080408       10 PI-DRPA1477-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1477.
          05 PO-DRPA1477-RC                 PIC  X(003).
             88 PO-DRPA1477-RC-OK                VALUE 'OK '.
             88 PO-DRPA1477-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
