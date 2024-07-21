      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0264.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAN0264.CPv  $
      *
      *   Rev 2.9   Nov 22 2018 16:14:10   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.8   Feb 25 2016 10:08:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.7   Feb 22 2016 14:20:32   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.6   Aug 12 2015 14:49:52   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.5   Jul 07 2015 15:55:02   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.4   Jun 22 2015 15:44:54   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 17:39:00   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:48:10   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:24:06   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:16   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRAN0264  FUNKTION INSERT STANDARD-OZP
      *    FÜR DVM-TBAN0020
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01198!07.07.15!2.5 ! NEU FELD EINR.GEBÜHR      ! RAHNER   *
      * AAN01198!11.08.15!2.6 ! VERGRÖßERN FELDER         ! RAHNER   *
      * AAN01240!22.02.16!2.7 ! NEUES FELD:FIRMA-NAME-LANG! RAHNER   *
      * AAN01370!22.11.18!2.9 ! NEUE FELDER F. NEUE PROD. ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0264                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE DVM  TBAN0020              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0264.
           05 PI-DRAN0264-DVM-KEY.
              10 PI-DRAN0264-PGEB-GES              PIC S9(004)    COMP.
              10 PI-DRAN0264-DVM-HVANTNUM          PIC X(009).
 
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
               10  PI-DRAN0264-DVM-ABVERM          PIC 9(007)   COMP-3.
      *            PRÜFZIFFER AVERM
               10  PI-DRAN0264-DVM-ABVERMPZ        PIC X(001).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  PI-DRAN0264-DVM-ABVERM-E        PIC 9(007)   COMP-3.
      *            PRÜFZIFFER AVERM - ERFASSUNG
               10  PI-DRAN0264-DVM-ABVERMPZ-E      PIC X(001).
      *            VERTRIEBSGESELLSCHAFT VB
               10  PI-DRAN0264-DVM-VB-GES          PIC 9(002).
      *
      *            VM
               10  PI-DRAN0264-DVM-VMNR            PIC 9(007)   COMP-3.
      *            PRÜFZIFFER VM
               10  PI-DRAN0264-DVM-VMNRPZ          PIC X(001).
      *            VOR- UND ZUNAME VM
               10  PI-DRAN0264-DVM-VMVORNA         PIC X(030).
               10  PI-DRAN0264-DVM-VMZUNA          PIC X(030).
      *            VM-ERFASSUNG
               10  PI-DRAN0264-DVM-VMNR-E          PIC 9(007)   COMP-3.
      *            PRÜFZIFFER VM ERFASSUNG
               10  PI-DRAN0264-DVM-VMPZ-E          PIC X(001).
      *            VOR- UND ZUNAME VM - ERFASSUNG
               10  PI-DRAN0264-DVM-VMVORNA-E       PIC X(030).
               10  PI-DRAN0264-DVM-VMZUNA-E        PIC X(030).
      *
      *            DVAG-DIREKTION
               10  PI-DRAN0264-DVM-DIR             PIC 9(003).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER
               10  PI-DRAN0264-DVM-FA-RF           PIC 9(003).
               10  PI-DRAN0264-DVM-FA-RF-E         PIC 9(003).
               10  PI-DRAN0264-DVM-ANR-FA          PIC 9(003).
               10  PI-DRAN0264-DVM-FA-N1           PIC X(030).
               10  PI-DRAN0264-DVM-FA-N1-E         PIC X(030).
               10  PI-DRAN0264-DVM-FA-N2           PIC X(030).
               10  PI-DRAN0264-DVM-FA-N2-E         PIC X(030).
220216         10  PI-DRAN0264-DVM-FA-N-LANG       PIC X(070).
220216         10  PI-DRAN0264-DVM-FA-N-LANG-E     PIC X(070).
               10  PI-DRAN0264-DVM-FA-STR          PIC X(045).
               10  PI-DRAN0264-DVM-FA-STR-E        PIC X(045).
               10  PI-DRAN0264-DVM-FA-PLZ          PIC X(015).
               10  PI-DRAN0264-DVM-FA-PLZ-E        PIC X(015).
               10  PI-DRAN0264-DVM-FA-ORT          PIC X(050).
               10  PI-DRAN0264-DVM-FA-ORT-E        PIC X(050).
               10  PI-DRAN0264-DVM-FA-POSTF        PIC X(010).
               10  PI-DRAN0264-DVM-FA-POSTF-E      PIC X(010).
               10  PI-DRAN0264-DVM-FA-PLZ-POSTF    PIC X(005).
               10  PI-DRAN0264-DVM-FA-PLZ-POSTF-E  PIC X(005).
               10  PI-DRAN0264-DVM-FA-ORT-POSTF    PIC X(040).
               10  PI-DRAN0264-DVM-FA-ORT-POSTF-E  PIC X(040).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER ERREICHBARKEIT
110815*        10  PI-DRAN0264-DVM-FA-TELE         PIC X(015).
110815*        10  PI-DRAN0264-DVM-FA-TELE-E       PIC X(015).
110815         10  PI-DRAN0264-DVM-FA-TELE         PIC X(030).
110815         10  PI-DRAN0264-DVM-FA-TELE-E       PIC X(030).
110815*        10  PI-DRAN0264-DVM-FA-FAX          PIC X(015).
110815*        10  PI-DRAN0264-DVM-FA-FAX-E        PIC X(015).
110815         10  PI-DRAN0264-DVM-FA-FAX          PIC X(030).
110815         10  PI-DRAN0264-DVM-FA-FAX-E        PIC X(030).
110815*        10  PI-DRAN0264-DVM-FA-MOBIL        PIC X(015).
110815*        10  PI-DRAN0264-DVM-FA-MOBIL-E      PIC X(015).
110815         10  PI-DRAN0264-DVM-FA-MOBIL        PIC X(030).
110815         10  PI-DRAN0264-DVM-FA-MOBIL-E      PIC X(030).
110815*        10  PI-DRAN0264-DVM-FA-EMAIL        PIC X(040).
110815*        10  PI-DRAN0264-DVM-FA-EMAIL-E      PIC X(040).
110815         10  PI-DRAN0264-DVM-FA-EMAIL        PIC X(050).
110815         10  PI-DRAN0264-DVM-FA-EMAIL-E      PIC X(050).
      *
      *        KMU ANSPRECHPARTNER
               10  PI-DRAN0264-DVM-FA-ANSP-TITEL   PIC 9(003).
               10  PI-DRAN0264-DVM-FA-ANSP-ANR     PIC 9(003).
               10  PI-DRAN0264-DVM-FA-ANSP-VN      PIC X(030).
               10  PI-DRAN0264-DVM-FA-ANSP-ZN      PIC X(030).
               10  PI-DRAN0264-DVM-FA-ANSP-GEB     PIC 9(008) COMP-3.
               10  PI-DRAN0264-DVM-FA-ANSP-FUNK    PIC X(050).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER RECHT
               10  PI-DRAN0264-DVM-FA-KZ-VST       PIC X(001).
               10  PI-DRAN0264-DVM-FA-KZ-VST-E     PIC X(001).
               10  PI-DRAN0264-DVM-FA-ST-NR        PIC X(015).
               10  PI-DRAN0264-DVM-FA-ST-NR-E      PIC X(015).
               10  PI-DRAN0264-DVM-FA-HREG-AMTG    PIC X(050).
               10  PI-DRAN0264-DVM-FA-HREG-AMTG-E  PIC X(050).
               10  PI-DRAN0264-DVM-FA-HREG-NR      PIC X(030).
               10  PI-DRAN0264-DVM-FA-HREG-NR-E    PIC X(030).
               10  PI-DRAN0264-DVM-FA-INH          PIC X(100).
               10  PI-DRAN0264-DVM-FA-INH-E        PIC X(100).
               10  PI-DRAN0264-DVM-FA-BR           PIC X(003).
               10  PI-DRAN0264-DVM-FA-GR-DAT       PIC 9(008) COMP-3.
      *
      *        KMU SONSTIGES
               10  PI-DRAN0264-DVM-KZ-FAC-OPT    PIC X(001).
               10  PI-DRAN0264-DVM-KZ-FAC-OPT-E  PIC X(001).
               10  PI-DRAN0264-DVM-UMSATZ-JAHR   PIC 9(009) COMP-3.
               10  PI-DRAN0264-DVM-UMSATZ-JAHR-E PIC 9(009) COMP-3.
               10  PI-DRAN0264-DVM-KZ-GARANT     PIC X(001).
               10  PI-DRAN0264-DVM-KZ-GARANT-E   PIC X(001).
               10  PI-DRAN0264-DVM-KZ-GW-GES     PIC X(001).
               10  PI-DRAN0264-DVM-KZ-GW-GES-E   PIC X(001).
               10  PI-DRAN0264-DVM-KZ-N-ERKL     PIC X(001).
               10  PI-DRAN0264-DVM-KZ-N-ERKL-E   PIC X(001).
               10  PI-DRAN0264-DVM-Z-ZIEL-TAGE   PIC 9(003).
               10  PI-DRAN0264-DVM-PS-GR-KUNDE   PIC 9(003).
               10  PI-DRAN0264-DVM-PS-PRIVAT-K   PIC 9(003).
               10  PI-DRAN0264-DVM-FAC-ANKH    PIC S9(009)V9(02) COMP-3.
070715         10  PI-DRAN0264-DVM-EINRGEBUEHR   PIC X(001).
070715         10  PI-DRAN0264-DVM-EINRGEBUEHR-E PIC X(001).
      *
               10  PI-DRAN0264-DVM-VBEGINN     PIC 9(008) COMP-3.
               10  PI-DRAN0264-DVM-VBEGINN-E   PIC 9(008) COMP-3.
      *
      *        DVAG PRODUKTE MIT JEW. BETRÄGEN
               10 PI-DRAN0264-PROD-PROD-SCHL1  PIC S9(004)       COMP-3.
               10 PI-DRAN0264-DVM-ABEINH-1     PIC S9(007)V9(02) COMP-3.
               10 PI-DRAN0264-DVM-ABEITRAG-1   PIC S9(007)V9(02) COMP-3.
               10 PI-DRAN0264-PROD-PROD-SCHL2  PIC S9(004)       COMP-3.
               10 PI-DRAN0264-DVM-ABEINH-2     PIC S9(007)V9(02) COMP-3.
               10 PI-DRAN0264-DVM-ABEITRAG-2   PIC S9(007)V9(02) COMP-3.
 
      *        KMU(FIRMA) KONTO INFORMATIONEN
               10  PI-DRAN0264-DVM-KIH-VN          PIC X(030).
               10  PI-DRAN0264-DVM-KIH-VN-E        PIC X(030).
               10  PI-DRAN0264-DVM-KIH-ZN          PIC X(030).
               10  PI-DRAN0264-DVM-KIH-ZN-E        PIC X(030).
               10  PI-DRAN0264-DVM-KIH-ABW-STR     PIC X(045).
               10  PI-DRAN0264-DVM-KIH-ABW-STR-E   PIC X(045).
               10  PI-DRAN0264-DVM-KIH-ABW-PLZ     PIC X(015).
               10  PI-DRAN0264-DVM-KIH-ABW-PLZ-E   PIC X(015).
               10  PI-DRAN0264-DVM-KIH-ABW-ORT     PIC X(050).
               10  PI-DRAN0264-DVM-KIH-ABW-ORT-E   PIC X(050).
               10  PI-DRAN0264-DVM-GELDINST        PIC X(058).
               10  PI-DRAN0264-DVM-GELDINST-E      PIC X(058).
               10  PI-DRAN0264-DVM-BIC             PIC X(011).
               10  PI-DRAN0264-DVM-BIC-E           PIC X(011).
               10  PI-DRAN0264-DVM-IBAN            PIC X(034).
               10  PI-DRAN0264-DVM-IBAN-E          PIC X(034).
               10  PI-DRAN0264-DVM-SEPA-M-BEG      PIC 9(008) COMP-3.
               10  PI-DRAN0264-DVM-SEPA-M-BEG-E    PIC 9(008) COMP-3.
      *
      *            VERSIONSDATEN
               10  PI-DRAN0264-DVM-VERSION         PIC 9(004).
      *
      *            SONSTIGE DATEN
               10  PI-DRAN0264-DVM-AN-DAT          PIC 9(008)   COMP-3.
               10  PI-DRAN0264-DVM-HAND            PIC X(001).
               10  PI-DRAN0264-DVM-LAST-UPDATE     PIC X(026).
               10  PI-DRAN0264-DVM-KENNUNG-AEND    PIC X(008).
               10  PI-DRAN0264-DVM-KD-BEST-ID      PIC S9(018)V COMP-3.
221118         10  PI-DRAN0264-DVM-BGEL-ARCH-ID    PIC S9(016)V COMP-3.
221118         10  PI-DRAN0264-DVM-BGEL-INDEX      PIC S9(004)  COMP.
221118         10  PI-DRAN0264-DVM-DDM-KEY         PIC S9(010)V COMP-3.
221118         10  PI-DRAN0264-DVM-WZ-CODE         PIC X(010).
      *
      *            ELEKTR. SIGNATUR
               10  PI-DRAN0264-DVM-E-SIGN          PIC X(001).
               10  PI-DRAN0264-DVM-XGDV.
                   15  PI-DRAN0264-DVM-XGDV-LEN    PIC S9(004).
                   15  PI-DRAN0264-DVM-XGDV-DATEN  PIC X(20000).
      *
       01  PO-DRAN0264.
           05 PO-DRAN0264-RC                       PIC  X(003).
             88 PO-DRAN0264-RC-OK                       VALUE 'OK '.
             88 PO-DRAN0264-RC-DUPLICATE-VALUE          VALUE 'DVA'.

