      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1512.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1512.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:34:16   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Dec 05 2001 11:13:04   t13ex43
      *APA00167 : PEG Bank Austria (Sonstige Banken Österreich)
      *
      *   Rev 2.0   04 Jul 2001 11:10:54   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1512 FUNKTION UWPA0047                   *
      *                 BANK AUSTRIA-PM FORMAL PRUEFEN UND NORMIEREN *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    22.11.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!22.11.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1512                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1512.
             10 PI-DRPA1512-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1512-PEG-DATEN-BA-AUST.
                COPY DRPA1513.
080408       10 PI-DRPA1512-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1512.
          05 PO-DRPA1512-RC                 PIC  X(003).
             88 PO-DRPA1512-RC-OK                VALUE 'OK '.
             88 PO-DRPA1512-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
