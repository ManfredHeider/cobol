      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0221.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0221.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:52:28   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:28:28   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:50:56   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:03:00   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 29 2008 15:52:42   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0221 FUNKTION                             *
      *                 ZUSATZDATEN ELEKTRONISCHER ANTRAG GENERALI-S  *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 ! NEU                       ! RAHNER    *
      * AAN01033!14.12.11!2.2 ! DATEN GELDWÄSCHE ECT.     ! RAHNER    *
      * AAN01073!25.07.13!2.4 ! SEPA                      ! RAHNER    *
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
       20 ZUSATZ-GENS-VORSCHL-NR          PIC X(9).
       20 ZUSATZ-GENS-PIN                 PIC S9(009)     COMP.
       20 ZUSATZ-GENS-AVERM               PIC S9(009)     COMP.
       20 ZUSATZ-GENS-AVERM-PZ            PIC X(001).
       20 ZUSATZ-GENS-VMNR                PIC S9(009)     COMP.
       20 ZUSATZ-GENS-VMPZ                PIC X(001).
       20 ZUSATZ-GENS-VM-VORNA            PIC X(026).
       20 ZUSATZ-GENS-VM-ZUNA             PIC X(026).
       20 ZUSATZ-GENS-VB-GW               PIC S9(009)     COMP.
       20 ZUSATZ-GENS-VB-GW-PZ            PIC X(001).
       20 ZUSATZ-GENS-VN-AKD-T            PIC X(026).
       20 ZUSATZ-GENS-VN-VORNA            PIC X(026).
       20 ZUSATZ-GENS-VN-ZUNA             PIC X(026).
       20 ZUSATZ-GENS-VN-SVNR             PIC S9(010)V    COMP-3.
       20 ZUSATZ-GENS-VN-STR              PIC X(026).
       20 ZUSATZ-GENS-VN-PLZ              PIC X(005).
       20 ZUSATZ-GENS-VN-ORT              PIC X(026).
       20 ZUSATZ-GENS-VN-LKZ              PIC X(003).
       20 ZUSATZ-GENS-VN-KFZ-KZ           PIC X(010).
       20 ZUSATZ-GENS-VN-FGST-NR          PIC X(026).
       20 ZUSATZ-GENS-VN-KFZ-B-NR         PIC X(020).
       20 ZUSATZ-GENS-VN-KNR              PIC S9(15)V     COMP-3.
       20 ZUSATZ-GENS-VN-BLZ              PIC S9(15)V     COMP-3.
250713 20 ZUSATZ-GENS-VN-IBAN             PIC X(034).
141211 20 ZUSATZ-GENS-VN-STAAANG          PIC X(003).
141211 20 ZUSATZ-GENS-VN-GEBDAT           PIC X(010).
       20 ZUSATZ-GENS-BEGINN              PIC X(10).
       20 ZUSATZ-GENS-ENDE                PIC X(10).
       20 ZUSATZ-GENS-ZAHLW               PIC 9(002).
       20 ZUSATZ-GENS-AN-ART              PIC X(001).
       20 ZUSATZ-GENS-AN-DAT              PIC X(010).
       20 ZUSATZ-GENS-PROD-TAB     OCCURS 8
                              INDEXED BY I-ZUSATZ-GENS-PROD,
                                         I-ZUSATZ-GENS-PROD-2.
          25 ZUSATZ-GENS-PRODKEY          PIC X(2).
          25 ZUSATZ-GENS-BTP-PRODKEY      PIC S9(6)V9(2) COMP-3.
          25 ZUSATZ-GENS-BTP-PRODKEY-E    PIC S9(6)V9(2) COMP-3.
          25 ZUSATZ-GENS-JNTP-PRODKEY     PIC S9(6)V9(2) COMP-3.
          25 ZUSATZ-GENS-EH-PRODKEY       PIC S9(9)V9(2) COMP-3.

141211*20 FILLER                          PIC X(487).
250713*20 FILLER                          PIC X(474).
250713 20 FILLER                          PIC X(440).
