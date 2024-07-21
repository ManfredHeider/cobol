      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1748.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1748.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:50:36   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jan 31 2014 14:22:04   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Migration
      *
      *   Rev 2.1   Aug 21 2013 14:22:12   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Migration
      *
      *   Rev 2.0   Apr 10 2013 14:43:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!21.08.13! 2.1! NEUERSTELLUNG             ! RAHNER   *
      * APA01526!31.01.14! 2.2! ZÄHLER VERARBEITETE SÄTZE ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE RESTART-INFORMATIONEN             *
      *    FÜR DAS PROGRAMM PMPA0098                                 *
      *                                                              *
      ****************************************************************
      *
        01 DRPA1748-RESTART.
           05 DRPA1748-ID              PIC  X(008)
                                       VALUE 'DRPA1748'.
      *
           05 DRPA1748-PGEB-GES           PIC S9(004)       BINARY.
           05 DRPA1748-COMMIT-VERTRAGSNR  PIC 9(008).
310114     05 DRPA1748-ZAEHLER.
310114        10 DRPA1748-ANZ-VERARBEITET PIC 9(010).
310114*    05 FILLER                      PIC X(3985).
310114     05 FILLER                      PIC X(3972).

