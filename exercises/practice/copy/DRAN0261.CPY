      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0261.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0261.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:14:00   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:08:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:20:14   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:49:34   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:54:48   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:53:38   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:04:24   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:08:32   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:45:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0261                                     *
      *    FUNKTION   : ERM. DVM-BESTAND ÜBER GES, PIN               *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 !NEU                         !RAHNER   *
      * AAN01198!07.07.15!2.4 !NEU FELD EINR.GEBÜHR        !RAHNER   *
      * AAN01198!11.08.15!2.5 !VERGRÖßERN FELDER           !RAHNER   *
      * AAN01240!22.02.16!2.6 !NEU FLED FIRMA-NAME-LANG    !RAHNER   *
      * AAN01370!22.11.18!2.8 !C+L WG. DRAN0273            !RAHNER   *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0261                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0261.
           05 PI-DRAN0261-DVM-KEY.
               10 PI-DRAN0261-PGEB-GES      PIC  X(2).
               10 PI-DRAN0261-DVM-VNR       PIC  S9(15) COMP-3.
               10 PI-DRAN0261-DVM-AVERM     PIC  S9(9) COMP.
      *
       01 PO-DRAN0261.
          05 PO-DRAN0261-RC                 PIC  X(003).
             88 PO-DRAN0261-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0261-RC-NOK                 VALUE 'NOK'.
 
          05 PO-DRAN0261-DATEN.
             10 PO-DRAN0261-BAN0049.
                15 PO-DRAN0261-VBNR1O         PIC X(7).
                15 PO-DRAN0261-VBNR-PZ1O      PIC X(1).
      *
                15 PO-DRAN0261-FA-RF1O        PIC X(003).
                15 PO-DRAN0261-FA-F1O         PIC X(030).
                15 PO-DRAN0261-FA-F21O        PIC X(030).
220216          15 PO-DRAN0261-FA-N-LANGO     PIC X(070).
                15 PO-DRAN0261-FA-STR1O       PIC X(045).
                15 PO-DRAN0261-FA-PLZ1O       PIC X(015).
                15 PO-DRAN0261-FA-ORT1O       PIC X(050).
                15 PO-DRAN0261-FA-POSTF1O     PIC X(010).
                15 PO-DRAN0261-FA-PLZ-POSTF1O PIC X(005).
                15 PO-DRAN0261-FA-ORT-POSTF1O PIC X(040).
      *
110815*         15 PO-DRAN0261-FA-TELE1O      PIC X(015).
110815          15 PO-DRAN0261-FA-TELE1O      PIC X(030).
110815*         15 PO-DRAN0261-FA-FAX1O       PIC X(015).
110815          15 PO-DRAN0261-FA-FAX1O       PIC X(030).
110815*         15 PO-DRAN0261-FA-MOBIL1O     PIC X(015).
110815          15 PO-DRAN0261-FA-MOBIL1O     PIC X(030).
110815*         15 PO-DRAN0261-FA-EMAIL1O     PIC X(040).
110815          15 PO-DRAN0261-FA-EMAIL1O     PIC X(050).
      *
                15 PO-DRAN0261-FA-KZ-VST1O    PIC X(001).
                15 PO-DRAN0261-FA-ST-NR1O     PIC X(015).
                15 PO-DRAN0261-FA-HREG-AMTG1O PIC X(050).
                15 PO-DRAN0261-FA-HREG-NR1O   PIC X(030).
                15 PO-DRAN0261-FA-INH1O       PIC X(100).
      *
                15 PO-DRAN0261-KZ-FAC-OPT1O   PIC X(001).
                15 PO-DRAN0261-UMSATZ-JAHR1O  PIC X(009).
                15 PO-DRAN0261-KZ-GARANT1O    PIC X(001).
                15 PO-DRAN0261-KZ-GW-GES1O    PIC X(001).
                15 PO-DRAN0261-KZ-N-ERKL1O    PIC X(001).
      *
                15 PO-DRAN0261-BEGINN1O       PIC X(006).
                15 PO-DRAN0261-PRODKEY1O      PIC X(001).
                15 PO-DRAN0261-ABEINH1O       PIC X(009).
                15 PO-DRAN0261-ABEITRAG1O     PIC X(008).
                15 PO-DRAN0261-PRODKEY2O      PIC X(001).
                15 PO-DRAN0261-ABEINH2O       PIC X(009).
                15 PO-DRAN0261-ABEITRAG2O     PIC X(008).
      *
070715          15 PO-DRAN0261-EINRGEBUEHR1O  PIC X(001).
      *
                15 PO-DRAN0261-KIH-VN1O       PIC X(030).
                15 PO-DRAN0261-KIH-ZN1O       PIC X(030).
                15 PO-DRAN0261-KIH-ABW-STR1O  PIC X(045).
                15 PO-DRAN0261-KIH-ABW-PLZ1O  PIC X(015).
                15 PO-DRAN0261-KIH-ABW-ORT1O  PIC X(050).
                15 PO-DRAN0261-GELDINST1O     PIC X(058).
                15 PO-DRAN0261-IBAN1O         PIC X(034).
                15 PO-DRAN0261-BIC1O          PIC X(011).
                15 PO-DRAN0261-SEPA-M-BEG1O   PIC X(006).
                15 PO-DRAN0261-HAND1O         PIC X(001).
      *
                15 PO-DRAN0261-AN-DAT1O       PIC X(006).
                15 PO-DRAN0261-AN-ART1O       PIC X(001).
 
             10 PO-DRAN0261-SONSTIGE.
                15 PO-DRAN0261-DVM-VNR        PIC X(015).
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0273.
      *
      ******** ENDE COPY-MEMBER **************************************
      *




