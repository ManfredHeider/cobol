      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0585.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0585.CPv  $
      *
      *   Rev 2.3   May 29 2019 09:27:08   t13ae37
      *APA02841: Interprätation neues Tarifkennzeichen bei den AML-
      *Produktschlüsseln 20/236 und 20/237
      *
      *   Rev 2.2   May 30 2016 11:20:36   t13ae46
      *APA02249: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.1   Aug 20 2013 11:30:00   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG
      *
      *   Rev 2.0   Feb 20 2013 11:32:28   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0585  KONSTANTEN FÜR                     *
      *                           TARIF-KZ                 KTY = 386 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    20.02.2013                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!20.02.13!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA02249!20.05.16!2.2 ! K-KNR-TARIF-KZ-SKH HINZU  ! WANG     *
      * APA02841!02.04.19!2.3 ! K-KNR-TARIF-KZ-BOZV       ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-TARIF-KZ-HT          PIC S9(004) BINARY VALUE 1.
           05 K-KNR-TARIF-KZ-BEF         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-TARIF-KZ-BAR         PIC S9(004) BINARY VALUE 3.
           05 K-KNR-TARIF-KZ-HZV         PIC S9(004) BINARY VALUE 4.
           05 K-KNR-TARIF-KZ-TZV         PIC S9(004) BINARY VALUE 5.
           05 K-KNR-TARIF-KZ-KIZV        PIC S9(004) BINARY VALUE 6.
           05 K-KNR-TARIF-KZ-PZV-POZ     PIC S9(004) BINARY VALUE 7.
           05 K-KNR-TARIF-KZ-EUB         PIC S9(004) BINARY VALUE 8.
           05 K-KNR-TARIF-KZ-EUR         PIC S9(004) BINARY VALUE 9.
190516     05 K-KNR-TARIF-KZ-SKH         PIC S9(004) BINARY VALUE 10.
020419     05 K-KNR-TARIF-KZ-BOZV        PIC S9(004) BINARY VALUE 88.


