      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1517.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1517.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:34:36   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Feb 06 2002 12:36:28   T13ex25
      *APA00172; PEG DWS
      *
      *   Rev 2.0   04 Jul 2001 11:11:02   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1517 FUNKTION UWPA0046                   *
      *                 DWS-PM FORMAL PRUEFEN UND NORMIEREN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger                                           *
      *    DATUM:    11.01.2002                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00172!11.03.02!2.1 ! NEU                       ! JÄGER    *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1517                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1517.
             10 PI-DRPA1517-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1517-PEG-DATEN-DWS.
                COPY DRPA1518.
080408       10 PI-DRPA1517-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1517.
          05 PO-DRPA1517-RC                 PIC  X(003).
             88 PO-DRPA1517-RC-OK                VALUE 'OK '.
             88 PO-DRPA1517-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
