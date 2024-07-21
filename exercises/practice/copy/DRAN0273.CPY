      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0273.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0273.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:15:26   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:09:58   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:21:44   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:51:08   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:55:42   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:53:54   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:04:44   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:19:52   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 19 2015 13:30:54   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      ****************************************************************
      *    COPY-MEMBER: DRAN0273                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 !NEU                        !RAHNER    *
      * AAN01198!07.07.15!2.4 !NEUES FELD EINRGEBÜHR      !RAHNER    *
      * AAN01198!12.08.15!2.5 !VERGRÖßERN FELDER          !RAHNER    *
      * AAN01240!25.02.16!2.6 !NEU FELD FIRMA-NAME-LANG   !RAHNER    *
      * AAN01370!22.11.18!2.8 !NEUE FELDER F. NEUE PROD.  !RAHNER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
           05  DVM-PC-DATEN.
      *
               10  DVM-PC-PGEB-GES       PIC S9(04) COMP.
               10  DVM-PC-HVANTNUM       PIC X(09).
               10  DVM-PC-VNR            PIC S9(15) COMP-3.
      *
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
               10  DVM-PC-ABVERM         PIC 9(07) COMP-3.
      *            PRÜFZIFFER AVERM
               10  DVM-PC-ABVERM-PZ      PIC X(01).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  DVM-PC-ABVERM-E       PIC 9(07) COMP-3.
      *            PRÜFZIFFER AVERM
               10  DVM-PC-ABVERM-PZ-E    PIC X(01).
      *            VERTRIEBSGESELLSCHAFT VB
               10  DVM-PC-VB-GES         PIC 9(02).
      *
      *            VM
               10  DVM-PC-VMNR           PIC 9(07) COMP-3.
      *            PRÜFZIFFER VM
               10  DVM-PC-VMNR-PZ        PIC X(01).
      *            VOR- UND ZUNAME VM
               10  DVM-PC-VM-VORNA       PIC X(30).
               10  DVM-PC-VM-ZUNA        PIC X(30).
      *            VM-ERFASSUNG
               10  DVM-PC-VMNR-E         PIC 9(07) COMP-3.
      *            PRÜFZIFFER VM
               10  DVM-PC-VMPZ-E         PIC X(01).
      *            VOR- UND ZUNAME VM
               10  DVM-PC-VM-VORNA-E     PIC X(30).
               10  DVM-PC-VM-ZUNA-E      PIC X(30).
      *
      *            DVAG-DIREKTION
               10  DVM-PC-DIR            PIC 9(03).
      *
      *            KMU/VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  DVM-PC-FA-RF             PIC S9(04) COMP.
               10  DVM-PC-FA-RF-E           PIC S9(04) COMP.
               10  DVM-PC-ANR-FA            PIC S9(04) COMP.
               10  DVM-PC-FA-N1             PIC X(30).
               10  DVM-PC-FA-N1-E           PIC X(30).
               10  DVM-PC-FA-N2             PIC X(30).
               10  DVM-PC-FA-N2-E           PIC X(30).
220216         10  DVM-PC-FA-N-LANG         PIC X(70).
220216         10  DVM-PC-FA-N-LANG-E       PIC X(70).
               10  DVM-PC-FA-STR            PIC X(45).
               10  DVM-PC-FA-STR-E          PIC X(45).
               10  DVM-PC-FA-PLZ            PIC X(15).
               10  DVM-PC-FA-PLZ-E          PIC X(15).
               10  DVM-PC-FA-ORT            PIC X(50).
               10  DVM-PC-FA-ORT-E          PIC X(50).
               10  DVM-PC-FA-POSTF          PIC X(10).
               10  DVM-PC-FA-POSTF-E        PIC X(10).
               10  DVM-PC-FA-PLZ-POSTF      PIC X(05).
               10  DVM-PC-FA-PLZ-POSTF-E    PIC X(05).
               10  DVM-PC-FA-ORT-POSTF      PIC X(40).
               10  DVM-PC-FA-ORT-POSTF-E    PIC X(40).
      *
      *            KMU (FIRMA)/ANTRAGSTELLER ERREICHBARKEIT
110815*        10  DVM-PC-FA-TELE           PIC X(15).
110815*        10  DVM-PC-FA-TELE-E         PIC X(15).
110815         10  DVM-PC-FA-TELE           PIC X(30).
110815         10  DVM-PC-FA-TELE-E         PIC X(30).
110815*        10  DVM-PC-FA-FAX            PIC X(15).
110815*        10  DVM-PC-FA-FAX-E          PIC X(15).
110815         10  DVM-PC-FA-FAX            PIC X(30).
110815         10  DVM-PC-FA-FAX-E          PIC X(30).
110815*        10  DVM-PC-FA-MOBIL          PIC X(15).
110815*        10  DVM-PC-FA-MOBIL-E        PIC X(15).
110815         10  DVM-PC-FA-MOBIL          PIC X(30).
110815         10  DVM-PC-FA-MOBIL-E        PIC X(30).
110815*        10  DVM-PC-FA-EMAIL          PIC X(40).
110815*        10  DVM-PC-FA-EMAIL-E        PIC X(40).
110815         10  DVM-PC-FA-EMAIL          PIC X(50).
110815         10  DVM-PC-FA-EMAIL-E        PIC X(50).
      *
      *            KMU ANSPRECHPARTNER
               10  DVM-PC-FA-ANSP-TITEL     PIC S9(04) COMP.
               10  DVM-PC-FA-ANSP-ANR       PIC S9(04) COMP.
               10  DVM-PC-FA-ANSP-VN        PIC X(30).
               10  DVM-PC-FA-ANSP-ZN        PIC X(30).
               10  DVM-PC-FA-ANSP-GEB       PIC S9(09) COMP.
               10  DVM-PC-FA-ANSP-FUNK      PIC X(50).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER RECHT
               10  DVM-PC-FA-KZ-VST         PIC X(01).
               10  DVM-PC-FA-KZ-VST-E       PIC X(01).
               10  DVM-PC-FA-ST-NR          PIC X(15).
               10  DVM-PC-FA-ST-NR-E        PIC X(15).
               10  DVM-PC-FA-HREG-AMTG      PIC X(50).
               10  DVM-PC-FA-HREG-AMTG-E    PIC X(50).
               10  DVM-PC-FA-HREG-NR        PIC X(30).
               10  DVM-PC-FA-HREG-NR-E      PIC X(30).
               10  DVM-PC-FA-INH            PIC X(100).
               10  DVM-PC-FA-INH-E          PIC X(100).
               10  DVM-PC-FA-BR             PIC 9(04) COMP.
               10  DVM-PC-FA-GR-DAT         PIC 9(08) COMP.
      *
      *        KMU SONSTIGES
               10  DVM-PC-KZ-FAC-OPT        PIC X(01).
               10  DVM-PC-KZ-FAC-OPT-E      PIC X(01).
               10  DVM-PC-UMSATZ-JAHR       PIC 9(09) COMP.
               10  DVM-PC-UMSATZ-JAHR-E     PIC 9(09) COMP.
               10  DVM-PC-KZ-GARANT         PIC X(01).
               10  DVM-PC-KZ-GARANT-E       PIC X(01).
               10  DVM-PC-KZ-GW-GES         PIC X(01).
               10  DVM-PC-KZ-GW-GES-E       PIC X(01).
               10  DVM-PC-KZ-N-ERKL         PIC X(01).
               10  DVM-PC-KZ-N-ERKL-E       PIC X(01).
               10  DVM-PC-Z-ZIEL-TAGE       PIC 9(03).
               10  DVM-PC-PS-GR-KUNDE       PIC 9(03).
               10  DVM-PC-PS-PRIVAT-K       PIC 9(03).
               10  DVM-PC-FAC-ANKH          PIC S9(09)V9(02) COMP-3.
      *
               10  DVM-PC-VBEGINN           PIC S9(09) COMP.
               10  DVM-PC-VBEGINN-E         PIC S9(09) COMP.
      *
070715         10  DVM-PC-EINRGEBUEHR       PIC X(01).
070715         10  DVM-PC-EINRGEBUEHR-E     PIC X(01).
      *
      *        KMU(FIRMA) KONTO INFORMATIONEN
               10  DVM-PC-KIH-VN            PIC X(30).
               10  DVM-PC-KIH-VN-E          PIC X(30).
               10  DVM-PC-KIH-ZN            PIC X(30).
               10  DVM-PC-KIH-ZN-E          PIC X(30).
               10  DVM-PC-KIH-ABW-STR       PIC X(45).
               10  DVM-PC-KIH-ABW-STR-E     PIC X(45).
               10  DVM-PC-KIH-ABW-PLZ       PIC X(15).
               10  DVM-PC-KIH-ABW-PLZ-E     PIC X(15).
               10  DVM-PC-KIH-ABW-ORT       PIC X(50).
               10  DVM-PC-KIH-ABW-ORT-E     PIC X(50).
               10  DVM-PC-FA-GELDINST       PIC X(58).
               10  DVM-PC-FA-GELDINST-E     PIC X(58).
               10  DVM-PC-BIC               PIC X(11).
               10  DVM-PC-BIC-E             PIC X(11).
               10  DVM-PC-IBAN              PIC X(34).
               10  DVM-PC-IBAN-E            PIC X(34).
               10  DVM-PC-SEPA-M-BEG        PIC S9(09) COMP.
               10  DVM-PC-SEPA-M-BEG-E      PIC S9(09) COMP.
      *
      *        DVAG PRODUKTE MIT JEW. BETRÄGEN
               10  DVM-PC-PROD-PROD-SCHL1   PIC S9(04)       COMP.
               10  DVM-PC-ABEITRAG-1        PIC S9(06)V9(02) COMP-3.
               10  DVM-PC-ABEINH-1          PIC S9(06)V9(02) COMP-3.
               10  DVM-PC-PROD-PROD-SCHL2   PIC S9(04)       COMP.
               10  DVM-PC-ABEITRAG-2        PIC S9(06)V9(02) COMP-3.
               10  DVM-PC-ABEINH-2          PIC S9(06)V9(02) COMP-3.
 
      *           VERSIONSDATEN
               10  DVM-PC-VERSION           PIC X(04).
      *
      *           SONSTIGE DATEN
               10  DVM-PC-LAST-UPDATE       PIC X(26).
               10  DVM-PC-KENNUNG-A         PIC X(08).
               10  DVM-PC-ERSTELLUNG        PIC X(26).
               10  DVM-PC-KENNUNG-ERST      PIC X(08).
               10  DVM-PC-HAND              PIC X(01).
               10  DVM-PC-AN-DAT            PIC S9(09)  COMP.
               10  DVM-PC-KD-BEST-ID        PIC S9(18)V COMP-3.
221118         10  DVM-PC-BUERGEL-ARCHIV-ID PIC S9(16)V COMP-3.
221118         10  DVM-PC-BUERGEL-INDEX     PIC S9(04) COMP.
221118         10  DVM-PC-DDM-KEY           PIC S9(10)V COMP-3.
221118         10  DVM-PC-WZ-CODE           PIC X(10).

