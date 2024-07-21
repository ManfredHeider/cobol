      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1548.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1548.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:35:30   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Aug 21 2002 08:56:30   t13ex21
      *APA00206 - Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:50   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1548 FUNKTION UWPA0046                   *
      *                 DWS-A-PM FORMAL PRUEFEN UND NORMIEREN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    03.07.2002                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!03.07.02!2.1 ! NEU                       ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1548                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1548.
             10 PI-DRPA1548-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1548-PEG-DATEN-DWS-A.
                COPY DRPA1549.
080408       10 PI-DRPA1548-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1548.
          05 PO-DRPA1548-RC                 PIC  X(003).
             88 PO-DRPA1548-RC-OK                VALUE 'OK '.
             88 PO-DRPA1548-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
