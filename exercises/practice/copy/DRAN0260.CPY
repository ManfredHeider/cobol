      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0260.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0260.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:13:46   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:08:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:20:06   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:49:24   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:54:42   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:53:30   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:04:14   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:08:22   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:45:48   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0260                                     *
      *    FUNKTION   : ERM. DVM-BESTAND ÜBER GES, VORSCHLAGSNUMMER  *
      *                 -> AUFBEREITET FÜR DVMI           (BAN0049)  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 !NEU                         !RAHNER   *
      * AAN01198!07.07.15!2.4 !NEU FELD EINR.GEBÜHR        !RAHNER   *
      * AAN01198!11.08.15!2.5 !VERGRÖßERN FELDER           !RAHNER   *
      * AAN01240!22.02.16!2.6 !NEU FELD FIRMA-NAME-LANG    !RAHNER   *
      * AAN01370!22.11.18!2.8 !C+L WG. DRAN0273            !RAHNER   *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0260                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0260.
           05 PI-DRAN0260-DVM-KEY.
               10 PI-DRAN0260-PGEB-GES       PIC  X(2).
               10 PI-DRAN0260-DVM-HVANTNUM   PIC  X(9).
      *           OPTIONAL
               10 PI-DRAN0260-DVM-AVERM      PIC  X(7).
               10 PI-DRAN0260-DVM-AVERM-PZ   PIC  X(1).
      *
       01 PO-DRAN0260.
          05 PO-DRAN0260-RC                  PIC  X(003).
             88 PO-DRAN0260-RC-OK                 VALUE 'OK '.
             88 PO-DRAN0260-RC-NOK                VALUE 'NOK'.
             88 PO-DRAN0260-RC-KEY-NOT-UNIQUE     VALUE 'KNU'.
      *
          05 PO-DRAN0260-DATEN.
             10 PO-DRAN0260-BAN0049.
                15 PO-DRAN0260-VBNR1O         PIC X(7).
                15 PO-DRAN0260-VBNR-PZ1O      PIC X(1).
      *
                15 PO-DRAN0260-FA-RECHTSF1O   PIC X(003).
                15 PO-DRAN0260-FA-F11O        PIC X(030).
                15 PO-DRAN0260-FA-F21O        PIC X(030).
220216          15 PO-DRAN0260-FA-N-LANGO     PIC X(070).
                15 PO-DRAN0260-FA-STRASSE1O   PIC X(045).
                15 PO-DRAN0260-FA-PLZ1O       PIC X(015).
                15 PO-DRAN0260-FA-ORT1O       PIC X(050).
                15 PO-DRAN0260-FA-POSTF1O     PIC X(010).
                15 PO-DRAN0260-FA-PLZ-POSTF1O PIC X(005).
                15 PO-DRAN0260-FA-ORT-POSTF1O PIC X(040).
      *
110815*         15 PO-DRAN0260-FA-TELE1O      PIC X(015).
110815          15 PO-DRAN0260-FA-TELE1O      PIC X(030).
110815*         15 PO-DRAN0260-FA-FAX1O       PIC X(015).
110815          15 PO-DRAN0260-FA-FAX1O       PIC X(030).
110815*         15 PO-DRAN0260-FA-MOBIL1O     PIC X(015).
110815          15 PO-DRAN0260-FA-MOBIL1O     PIC X(030).
110815*         15 PO-DRAN0260-FA-EMAIL1O     PIC X(040).
110815          15 PO-DRAN0260-FA-EMAIL1O     PIC X(050).
      *
                15 PO-DRAN0260-FA-KZ-VST1O    PIC X(001).
                15 PO-DRAN0260-FA-ST-NR1O     PIC X(015).
                15 PO-DRAN0260-FA-HREG-AMTG1O PIC X(050).
                15 PO-DRAN0260-FA-HREG-NR1O   PIC X(030).
                15 PO-DRAN0260-FA-INH1O       PIC X(100).
      *
                15 PO-DRAN0260-KZ-FAC-OPT1O   PIC X(001).
                15 PO-DRAN0260-UMSATZ-JAHR1O  PIC X(009).
                15 PO-DRAN0260-KZ-GARANT1O    PIC X(001).
                15 PO-DRAN0260-KZ-GW-GES1O    PIC X(001).
                15 PO-DRAN0260-KZ-N-ERKL1O    PIC X(001).
      *
                15 PO-DRAN0260-BEGINN1O       PIC X(006).
                15 PO-DRAN0260-PRODKEY1O      PIC X(003).
                15 PO-DRAN0260-ABEINH1O       PIC X(007).
                15 PO-DRAN0260-ABEITRAG1O     PIC X(008).
                15 PO-DRAN0260-PRODKEY2O      PIC X(003).
                15 PO-DRAN0260-ABEINH2O       PIC X(007).
                15 PO-DRAN0260-ABEITRAG2O     PIC X(008).
      *
070715          15 PO-DRAN0260-EINRGEBUEHR1O  PIC X(001).
                15 PO-DRAN0260-KIH-VN1O       PIC X(030).
                15 PO-DRAN0260-KIH-ZN1O       PIC X(030).
                15 PO-DRAN0260-KIH-ABW-STR1O  PIC X(045).
                15 PO-DRAN0260-KIH-ABW-PLZ1O  PIC X(015).
                15 PO-DRAN0260-KIH-ABW-ORT1O  PIC X(050).
                15 PO-DRAN0260-GELDINST1O     PIC X(058).
                15 PO-DRAN0260-IBAN1O         PIC X(034).
                15 PO-DRAN0260-BIC1O          PIC X(011).
                15 PO-DRAN0260-SEPA-M-BEG1O   PIC X(006).
                15 PO-DRAN0260-HAND1O         PIC X(001).
      *
                15 PO-DRAN0260-AN-DAT1O       PIC X(006).
                15 PO-DRAN0260-AN-ART1O       PIC X(001).
 
             10 PO-DRAN0260-SONSTIGE.
                15 PO-DRAN0260-DVM-VNR        PIC X(015).
 
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0273.
      *
      ******** ENDE COPY-MEMBER **************************************





