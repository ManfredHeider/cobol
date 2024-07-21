      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0421.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0421.CPv  $
      *
      *   Rev 2.3   Apr 13 2011 17:09:32   t13ae34
      *AAN01000: Zahlweise "0" bei Lebensversicherung installieren.
      *
      *   Rev 2.2   09 Nov 2006 15:08:22   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.1   Sep 26 2006 11:42:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 20 2006 12:04:00   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    KONSTANTEN FÜR     KTY = 222                              *
      *    ANTRAG ZAHLWEISE                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!20.09.06! 2.1 ! NEUERSTELLUNG            ! HEIDER   *
      * AAN01000!31.03.11! 2.2 ! ZAHLWEISE = KEINE        ! HEIDER   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AN-ZW-JAHR           PIC S9(004) BINARY VALUE 1.
           05 K-KNR-AN-ZW-HALBJAHR       PIC S9(004) BINARY VALUE 2.
           05 K-KNR-AN-ZW-QUARTAL        PIC S9(004) BINARY VALUE 4.
           05 K-KNR-AN-ZW-MONAT          PIC S9(004) BINARY VALUE 6.
           05 K-KNR-AN-ZW-EINMAL         PIC S9(004) BINARY VALUE 8.
           05 K-KNR-AN-ZW-DEPOT          PIC S9(004) BINARY VALUE 9.
310311     05 K-KNR-AN-ZW-KEINE          PIC S9(004) BINARY VALUE 7.
