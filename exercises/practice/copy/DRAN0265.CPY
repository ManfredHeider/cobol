      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0265.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0265.CPv  $
      *
      *   Rev 2.8   Feb 25 2016 10:08:48   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.7   Feb 22 2016 14:20:42   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.6   Aug 12 2015 14:50:02   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.5   Jul 07 2015 15:55:08   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.4   Jun 22 2015 15:44:58   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 17:39:04   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:48:18   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:24:18   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:24   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0265                                     *
      *    FUNKTION   : UPDATE VNR IN DVM-BESTAND                    *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 !NEU                        ! RAHNER   *
      * AAN01198!07.07.15!2.5 !NEU FELD EINR.GEBUEHR      ! RAHNER   *
      * AAN01198!11.08.15!2.6 !VERGRÖßERN FELDER          ! RAHNER   *
      * AAN01240!22.02.16!2.7 !NEU FELD FIRMA-NAME-LANG   ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0265                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0265.
           05 PI-DRAN0265-DVM-KEY.
               10 PI-DRAN0265-PGEB-GES            PIC X(002).
               10 PI-DRAN0265-DVM-HVANTNUM        PIC X(009).
               10 PI-DRAN0265-DVM-AVERM-E         PIC S9(009) COMP.
               10 PI-DRAN0265-DVM-AVERM-PZ-E      PIC X(001).
          05 PI-DRAN0265-DATEN.
               10 PI-DRAN0265-DVM-VNR             PIC S9(015)V COMP-3.
               10 PI-DRAN0265-DVM-VMNR-E          PIC S9(009) COMP.
               10 PI-DRAN0265-DVM-VMPZ-E          PIC X(001).
               10 PI-DRAN0265-DVM-VM-VORNA-E      PIC X(030).
               10 PI-DRAN0265-DVM-VM-ZUNA-E       PIC X(030).
      *
               10 PI-DRAN0265-DVM-FA-RF-E         PIC S9(004) COMP.
               10 PI-DRAN0265-DVM-FA-N1-E         PIC X(030).
               10 PI-DRAN0265-DVM-FA-N2-E         PIC X(030).
220216         10 PI-DRAN0265-DVM-FA-N-LANG-E     PIC X(070).
               10 PI-DRAN0265-DVM-FA-STR-E        PIC X(045).
               10 PI-DRAN0265-DVM-FA-PLZ-E        PIC X(015).
               10 PI-DRAN0265-DVM-FA-ORT-E        PIC X(050).
               10 PI-DRAN0265-DVM-FA-POSTF-E      PIC X(010).
               10 PI-DRAN0265-DVM-FA-PLZ-POSTF-E  PIC X(005).
               10 PI-DRAN0265-DVM-FA-ORT-POSTF-E  PIC X(040).
      *
110815*        10 PI-DRAN0265-DVM-FA-TELE-E       PIC X(015).
110815         10 PI-DRAN0265-DVM-FA-TELE-E       PIC X(030).
110815*        10 PI-DRAN0265-DVM-FA-FAX-E        PIC X(015).
110815         10 PI-DRAN0265-DVM-FA-FAX-E        PIC X(030).
110815*        10 PI-DRAN0265-DVM-FA-MOBIL-E      PIC X(015).
110815         10 PI-DRAN0265-DVM-FA-MOBIL-E      PIC X(030).
110815*        10 PI-DRAN0265-DVM-FA-EMAIL-E      PIC X(040).
110815         10 PI-DRAN0265-DVM-FA-EMAIL-E      PIC X(050).
      *
               10 PI-DRAN0265-DVM-FA-KZ-VST-E     PIC X(001).
               10 PI-DRAN0265-DVM-FA-ST-NR-E      PIC X(015).
               10 PI-DRAN0265-DVM-FA-HREG-AMTG-E  PIC X(015).
               10 PI-DRAN0265-DVM-FA-HREG-NR-E    PIC X(030).
               10 PI-DRAN0265-DVM-FA-INH-E        PIC X(100).
      *
               10 PI-DRAN0265-DVM-KZ-FAC-OPT-E    PIC X(001).
               10 PI-DRAN0265-DVM-UMSATZ-JAHR-E   PIC S9(009) COMP-3.
               10 PI-DRAN0265-DVM-KZ-GARANT-E     PIC X(001).
               10 PI-DRAN0265-DVM-KZ-GW-GES-E     PIC X(001).
               10 PI-DRAN0265-DVM-KZ-N-ERKL-E     PIC X(001).
 
               10 PI-DRAN0265-DVM-VBEGINN-E       PIC S9(009) COMP-3.
      *
070715         10 PI-DRAN0265-DVM-EINRGEBUEHR-E   PIC X(001).
               10 PI-DRAN0265-DVM-KIH-VN-E        PIC X(030).
               10 PI-DRAN0265-DVM-KIH-ZN-E        PIC X(030).
               10 PI-DRAN0265-DVM-KIH-ABW-STR-E   PIC X(045).
               10 PI-DRAN0265-DVM-KIH-ABW-PLZ-E   PIC X(015).
               10 PI-DRAN0265-DVM-KIH-ABW-ORT-E   PIC X(050).
               10 PI-DRAN0265-DVM-GELDINST-E      PIC X(058).
               10 PI-DRAN0265-DVM-BIC-E           PIC X(011).
               10 PI-DRAN0265-DVM-IBAN-E          PIC X(034).
               10 PI-DRAN0265-DVM-SEPA-M-BEG-E    PIC S9(009) COMP-3.
      *
               10 PI-DRAN0265-DVM-HAND            PIC X(001).
      *
       01 PO-DRAN0265.
          05 PO-DRAN0265-RC                   PIC X(003).
             88 PO-DRAN0265-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0265-RC-NOK                 VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************




