      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1777.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1777.CPv  $
      *
      *   Rev 2.2   Jun 05 2019 11:41:04   u130076
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.1   Jan 13 2014 09:48:00   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.0   Dec 04 2013 10:38:40   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1777  FUNKTION UPDATE PMBE-RFOLGE-NR     *
      *                                       FÜR STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    04.12.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!04.12.13!2.01! NEUERSTELLUNG             ! STORCH   *
      * APA02796!03.04.19!2.02! NEUES UPDATE-FELD IN PMBE ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1777                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  PGFAELLIG-BIS      <<< TABELLE PMBE TBPA0113 *
      *    >>>  UPDATE  PMBE-PG-DATEN-TEXT <<< TABELLE PMBE TBPA0113 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1777.
          05 PI-DRPA1777-PMBE-LAST-UPDATE   PIC  X(26).
          05 PI-DRPA1777-PMBE-KEY.
             10 PI-DRPA1777-PMBE-ID         PIC S9(15)V PACKED-DECIMAL.
             10 PI-DRPA1777-PMBE-VERSION    PIC S9(04)  BINARY.
          05 PI-DRPA1777-PMBE-DATEN.
             10 PI-DRPA1777-PMBE-RFOLGE-NR  PIC S9(04)  BINARY.
030419       10 PI-DRPA1777-PMBE-PG-DATEN.
030419          49 PI-DRPA1777-PMBE-PG-DATEN-LEN  PIC S9(4) USAGE COMP.
030419          49 PI-DRPA1777-PMBE-PG-DATEN-TEXT PIC X(300).
      *
       01 PO-DRPA1777.
          05 PO-DRPA1777-RC                 PIC  X(03).
             88 PO-DRPA1777-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1777-RC-CHANGED                  VALUE 'CHA'.
             88 PO-DRPA1777-RC-NOT-FOUND                VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

