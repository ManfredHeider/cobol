      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1479.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1479.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:32:46   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   30 Mar 2001 07:13:02   T13EX22
      *APA00131: PEG-BfG-Bank
      *
      *   Rev 2.0   09 Jan 2001 10:22:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1479 FUNKTION UWPA0045                   *
      *         BFG-BANK PM FORMAL PRUEFEN UND NORMIEREN             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    09.03.2001                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!09.03.01!2.1 ! NEUERSTELLUNG             ! MUND     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1479                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1479.
             10 PI-DRPA1479-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1479-PEG-DATEN-BFG-BANK.
                COPY DRPA1481.
080408       10 PI-DRPA1479-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1479.
          05 PO-DRPA1479-RC                 PIC  X(003).
             88 PO-DRPA1479-RC-OK                VALUE 'OK '.
             88 PO-DRPA1479-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
