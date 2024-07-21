      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1787.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1787.CPv  $
      *
      *   Rev 2.1   Jan 03 2014 10:19:30   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Sonstige Anpassungen (MSS)
      *
      *   Rev 2.0   Dec 17 2013 06:43:18   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1787  STRUKTURBEREINIGUNG OHNE VBAS      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    17.12.2013                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!17.12.13!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1787                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1787.
          05 PI-DRPA1787-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA1787-PROV-TERMIN            PIC  X(010).

       01 PO-DRPA1787.
          05 PO-DRPA1787-RC                     PIC  X(003).
             88 PO-DRPA1787-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1787-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA1787-DATEN.
             10 PO-DRPA1787-STRK-KZ             PIC  X(001).
      *
      ******** ENDE COPY-MEMBER DRPA1787 *****************************
