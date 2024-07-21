      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1778.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1778.CPv  $
      *
      *   Rev 2.1   Jan 13 2014 09:48:08   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.0   Dec 04 2013 11:26:56   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1778 FUNKTION SET R-FOLGE-NR FÜR         *
      *                          FÜR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    04.12.2013                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!04.12.13!2.01 !NEU                       ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1778                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1778.
             10 PI-DRPA1778-PRINTE             PIC S9(15)
                                               PACKED-DECIMAL.
             10 PI-DRPA1778-PMBE-ID            PIC S9(15)
                                               PACKED-DECIMAL.
             10 PI-DRPA1778-RFOLGE-NR          PIC S9(04) COMP.

       01 PO-DRPA1778.
          05 PO-DRPA1778-RC                    PIC  X(03).
             88 PO-DRPA1778-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1778-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1778-MELDUNG.
             10 PO-DRPA1778-MELDUNG-TYP        PIC  X(01).
             10 PO-DRPA1778-MELDUNG-NR         PIC S9(04) BINARY.
      *
          05 PO-DRPA1778-MAP-STEUERUNG.
             10 PO-DRPA1778-MAP-ST-RFOLGE-NR.
                20 FELD-INTENSIV               PIC X(01).
                20 FELD-OFFEN                  PIC X(01).
                20 FELD-CURSOR-KZ              PIC X(01).
      ******** ENDE COPY-MEMBER **************************************
