      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1464.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1464.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:32:02   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   05 Feb 2001 10:22:02   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   09 Jan 2001 10:12:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1464 FUNKTION BFG-PM FORMAL PRUEFEN      *
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!23.01.01!2.1 ! NEU                       ! MUND     *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1464                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1464.
             10 PI-DRPA1464-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1464-PEG-DATEN-BFG.
                COPY DRPA1438.
080408       10 PI-DRPA1464-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1464.
          05 PO-DRPA1464-RC                 PIC  X(003).
             88 PO-DRPA1464-RC-OK                VALUE 'OK '.
             88 PO-DRPA1464-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
