      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1509.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1509.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:34:04   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Nov 28 2001 16:30:54   t13ex22
      *APA00167: Neuerstellung für DIT-Österreich (44)
      *
      *   Rev 2.0   04 Jul 2001 11:10:50   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1509 FUNKTION DIT-AUSTRIA FORMAL PRUEFEN *
      *                 UND NORMIEREN                                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    21.11.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!21.11.01!2.1 ! NEU                       ! MUND     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1509                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1509.
             10 PI-DRPA1509-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1509-PEG-DATEN-DIT-OEST.
                COPY DRPA1510.
080408       10 PI-DRPA1509-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1509.
          05 PO-DRPA1509-RC                 PIC  X(003).
             88 PO-DRPA1509-RC-OK                VALUE 'OK '.
             88 PO-DRPA1509-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************

