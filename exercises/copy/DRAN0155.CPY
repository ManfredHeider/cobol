      *----------.-----------------------------------------------------*
      *    Standar. fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0155.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0155.CPv  $
      *
      *   Rev 2.8   Dec 18 2015 08:58:44   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.7   Oct 21 2015 14:40:36   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.6   Oct 28 2013 17:27:26   t13ae23
      *AAN01073: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.5   Oct 24 2013 11:01:48   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.4   Feb 13 2007 19:33:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 18:32:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 17:26:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   24 Oct 2006 13:56:50   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Oct 2006 10:38:22   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0155 FUNKTION                             *
      *                 ZUSATZDATEN ELEKTRONISCHER ANTRAG ADVOCARD    *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00655!12.10.06!2.1 ! NEU                       ! RAHNER    *
      * AAN00655!09.02.07!2.2 ! EINHEITEN                 ! HEIDER    *
      * AAN01073!24.10.13!2.5 ! SEPA                      ! RAHNER    *
      * AAN01073!28.10.13!2.6 ! KORR. LÄNGE FILLER        ! RAHNER    *
      * AAN01223!08.12.15!2.8 ! BEGINN/ABLAUF ÄNDERBAR    ! RAHNER    *
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
       20 ADVC-VORSCHL-NR          PIC X(9).
       20 ADVC-PIN                 PIC S9(009) COMP.
       20 ADVC-VB-NR               PIC S9(009) COMP.
       20 ADVC-VB-NR-PZ            PIC X(001).
       20 ADVC-VM-NR               PIC S9(009) COMP.
       20 ADVC-VM-NR-PZ            PIC X(001).
       20 ADVC-HAND                PIC X(001).
       20 ADVC-BEITRAG-BRUTTO      PIC S9(5)V99 COMP-3.
       20 ADVC-BEITRAG-NETTO       PIC S9(5)V99 COMP-3.
       20 ADVC-KONTONR             PIC S9(012)V COMP-3.
       20 ADVC-BLZ                 PIC S9(009)  COMP.
       20 ADVC-AMTLKZ              PIC X(012).
241013*20 ADVC-V-PLUS              PIC X(001).
       20 ADVC-VDABLEIN            PIC S9(6)V9(1) COMP-3.
241013 20 ADVC-IBAN                PIC X(034).
241013 20 ADVC-BIC                 PIC X(011).
081215 20 ADVC-BEGINN              PIC X(010).
081215 20 ADVC-ABLAUF              PIC X(010).
241013*20 FILLER                   PIC X(940).
081215*20 FILLER                   PIC X(896).
081215 20 FILLER                   PIC X(876).



