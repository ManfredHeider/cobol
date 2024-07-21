      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1542.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1542.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:34:58   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Jul 18 2002 10:55:14   t13ex43
      *APA00206 : PEG S-Bausparkasse
      *
      *   Rev 2.0   04 Jul 2001 11:11:38   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1542 FUNKTION UWPA0045                   *
      *                 S-BAUSPARKASSE-PM FORMAL PRUEFEN + NORMIEREN *
      *    --------------------------------------------------------- *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    19.06.2002                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!19.06.02!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1542                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1542.
             10 PI-DRPA1542-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1542-PEG-DATEN-S-BSPK.
                COPY DRPA1536.
080408       10 PI-DRPA1542-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1542.
          05 PO-DRPA1542-RC                 PIC  X(003).
             88 PO-DRPA1542-RC-OK                VALUE 'OK '.
             88 PO-DRPA1542-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
