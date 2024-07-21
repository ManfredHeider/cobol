      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0204.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0204.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:51:14   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:03:26   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.2   Nov 24 2008 14:20:48   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:48   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 29 2008 11:18:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0204 FUNKTION                             *
      *                 ZUSATZDATEN ELEKTRONISCHER ANTRAG GENERALI-L  *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 ! NEU                       ! RAHNER    *
      * AAN01033!14.12.11!2.2 ! DATEN GELDWÄSCHE ECT.     ! RAHNER    *
      * AAN01073!25.07.13!2.3 ! SEPA                      ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
       20 ZUSATZ-GENL-VORSCHL-NR          PIC X(9).
       20 ZUSATZ-GENL-PIN                 PIC S9(009)     COMP.
       20 ZUSATZ-GENL-AVERM               PIC S9(009)     COMP.
       20 ZUSATZ-GENL-AVERM-PZ            PIC X(001).
       20 ZUSATZ-GENL-VMNR                PIC S9(009)     COMP.
       20 ZUSATZ-GENL-VMPZ                PIC X(001).
       20 ZUSATZ-GENL-VM-VORNA            PIC X(026).
       20 ZUSATZ-GENL-VM-ZUNA             PIC X(026).
       20 ZUSATZ-GENL-VB-GW               PIC S9(009)     COMP.
       20 ZUSATZ-GENL-VB-GW-PZ            PIC X(001).
       20 ZUSATZ-GENL-VN-AKD-T            PIC X(026).
       20 ZUSATZ-GENL-VN-VORNA            PIC X(026).
       20 ZUSATZ-GENL-VN-ZUNA             PIC X(026).
       20 ZUSATZ-GENL-VN-SVNR             PIC S9(010)V    COMP-3.
       20 ZUSATZ-GENL-VN-STR              PIC X(026).
       20 ZUSATZ-GENL-VN-PLZ              PIC X(005).
       20 ZUSATZ-GENL-VN-ORT              PIC X(026).
       20 ZUSATZ-GENL-VN-LKZ              PIC X(003).
       20 ZUSATZ-GENL-VN-KNR              PIC S9(15)V     COMP-3.
       20 ZUSATZ-GENL-VN-BLZ              PIC S9(15)V     COMP-3.
       20 ZUSATZ-GENL-BEGINN              PIC X(10).
       20 ZUSATZ-GENL-PZ-DAUER            PIC S9(02).
       20 ZUSATZ-GENL-BEITRAG             PIC S9(11)V9(2) COMP-3.
       20 ZUSATZ-GENL-SUMME               PIC S9(009)     COMP.
       20 ZUSATZ-GENL-JAHR-NTP            PIC S9(11)V9(2) COMP-3.
       20 ZUSATZ-GENL-PRSUM               PIC S9(11)V     COMP-3.
       20 ZUSATZ-GENL-ZAHLW               PIC 9(002).
       20 ZUSATZ-GENL-VD-EINH             PIC S9(6)V9(1)  COMP-3.
       20 ZUSATZ-GENL-AN-ART              PIC X(001).
       20 ZUSATZ-GENL-AN-DAT              PIC X(010).
141211 20 ZUSATZ-GENL-VN-STAA             PIC X(003).
141211 20 ZUSATZ-GENL-VN-BGRP             PIC X(040).
141211 20 ZUSATZ-GENL-VN-BRAN             PIC X(040).
141211 20 ZUSATZ-GENL-VN-PEP              PIC X(001).
250713 20 ZUSATZ-GENL-VN-IBAN             PIC X(034).
141211*20 FILLER                          PIC X(707).
250713*20 FILLER                          PIC X(623).
250713 20 FILLER                          PIC X(589).

