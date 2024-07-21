      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0274.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0274.CPv  $
      *
      *   Rev 2.7   Feb 25 2016 10:10:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:21:54   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:51:18   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:55:48   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:54:04   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:04:54   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:20:02   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Feb 23 2015 13:03:44   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0274 FUNKTION                             *
      *                 ZUSATZDATEN ELEKTRONISCHER ANTRAG DVM         *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 ! NEU                       ! RAHNER    *
      * AAN01198!07.07.15!2.4 ! NEU FELD EINR.GEBÜHR      ! RAHNER    *
      * AAN01198!11.08.15!2.5 ! VERGRÖßERN FELDER         ! RAHNER    *
      * AAN01240!22.02.16!2.6 ! NEU FIRMA-NAME-LANG       ! RAHNER    *
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
       20 ZUSATZ-DVM-VORSCHL-NR          PIC X(009).
       20 ZUSATZ-DVM-VNR                 PIC S9(015)V    COMP-3.
       20 ZUSATZ-DVM-AVERM               PIC S9(009)     COMP.
       20 ZUSATZ-DVM-AVERM-PZ            PIC X(001).
       20 ZUSATZ-DVM-VMNR                PIC S9(009)     COMP.
       20 ZUSATZ-DVM-VMPZ                PIC X(001).
220216*20 ZUSATZ-DVM-VM-VORNA            PIC X(030).
220216*20 ZUSATZ-DVM-VM-ZUNA             PIC X(030).
       20 ZUSATZ-DVM-FA-RF               PIC 9(003).
       20 ZUSATZ-DVM-FA-N1               PIC X(030).
       20 ZUSATZ-DVM-FA-N2               PIC X(030).
220216 20 ZUSATZ-DVM-FA-N-LANG           PIC X(070).
       20 ZUSATZ-DVM-FA-STR              PIC X(045).
       20 ZUSATZ-DVM-FA-PLZ              PIC X(015).
       20 ZUSATZ-DVM-FA-ORT              PIC X(050).
       20 ZUSATZ-DVM-FA-POSTF            PIC X(010).
       20 ZUSATZ-DVM-FA-PLZ-POSTF        PIC X(005).
       20 ZUSATZ-DVM-FA-ORT-POSTF        PIC X(040).
110815*20 ZUSATZ-DVM-FA-TELE             PIC X(015).
110815 20 ZUSATZ-DVM-FA-TELE             PIC X(030).
110815*20 ZUSATZ-DVM-FA-FAX              PIC X(015).
110815 20 ZUSATZ-DVM-FA-FAX              PIC X(030).
110815*20 ZUSATZ-DVM-FA-MOBIL            PIC X(015).
110815 20 ZUSATZ-DVM-FA-MOBIL            PIC X(030).
110815*20 ZUSATZ-DVM-FA-EMAIL            PIC X(040).
110815 20 ZUSATZ-DVM-FA-EMAIL            PIC X(050).
       20 ZUSATZ-DVM-FA-KZ-VST           PIC X(001).
       20 ZUSATZ-DVM-FA-ST-NR            PIC X(015).
       20 ZUSATZ-DVM-FA-HREG-AMTG        PIC X(050).
       20 ZUSATZ-DVM-FA-HREG-NR          PIC X(030).
       20 ZUSATZ-DVM-FA-INH              PIC X(100).
       20 ZUSATZ-DVM-KZ-FAC-OPT          PIC X(001).
       20 ZUSATZ-DVM-UMSATZ-JAHR         PIC 9(009) COMP.
       20 ZUSATZ-DVM-KZ-GARANT           PIC X(001).
       20 ZUSATZ-DVM-KZ-GW-GES           PIC X(001).
       20 ZUSATZ-DVM-KZ-N-ERKL           PIC X(001).
       20 ZUSATZ-DVM-VBEGINN             PIC S9(009) COMP.
070715 20 ZUSATZ-DVM-EINRGEBUEHR         PIC X(001).
       20 ZUSATZ-DVM-FA-KIH-VN           PIC X(030).
       20 ZUSATZ-DVM-FA-KIH-ZN           PIC X(030).
       20 ZUSATZ-DVM-FA-KIH-ABW-STR      PIC X(045).
       20 ZUSATZ-DVM-FA-KIH-ABW-PLZ      PIC X(015).
       20 ZUSATZ-DVM-FA-KIH-ABW-ORT      PIC X(050).
       20 ZUSATZ-DVM-GELDINST            PIC X(058).
       20 ZUSATZ-DVM-BIC                 PIC X(011).
       20 ZUSATZ-DVM-IBAN                PIC X(034).
       20 ZUSATZ-DVM-PROD-PROD-SCH1      PIC S9(004) COMP.
       20 ZUSATZ-DVM-ABEITRAG-1          PIC S9(006)V9(002) COMP-3.
       20 ZUSATZ-DVM-ABEINH-1            PIC S9(006)V9(002) COMP-3.
       20 ZUSATZ-DVM-PROD-PROD-SCH2      PIC S9(004) COMP.
       20 ZUSATZ-DVM-ABEITRAG-2          PIC S9(006)V9(002) COMP-3.
       20 ZUSATZ-DVM-ABEINH-2            PIC S9(006)V9(002) COMP-3.
       20 ZUSATZ-DVM-AN-DAT              PIC S9(009) COMP.
       20 ZUSATZ-DVM-HAND                PIC X(001).
       20 ZUSATZ-DVM-SEPA-M-BEG          PIC S9(09) COMP.
070715*20 FILLER                         PIC X(87).
110815*20 FILLER                         PIC X(86).
220216*20 FILLER                         PIC X(30).
220216 20 FILLER                         PIC X(20).




