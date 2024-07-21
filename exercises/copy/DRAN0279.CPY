      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0279.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0279.CPv  $
      *
      *   Rev 2.7   Feb 25 2016 10:10:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:22:20   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:51:46   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:56:08   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:54:34   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:05:32   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:09:16   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Mar 16 2015 17:07:22   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0279                                     *
      *    FUNKTION   : ERM. DVM-BESTAND ÜBER GES, VNR               *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 !NEU                         !RAHNER   *
      * AAN01198!07.07.15!2.2 !NEU FELD EINR.GEBÜHR        !RAHNER   *
      * AAN01198!11.08.15!2.3 !VERGRÖßERN FELDER           !RAHNER   *
      * AAN01240!22.02.16!2.6 !NEU FELD FIRMA-NAME-LANG    !RAHNER   *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0279                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0279.
           05 PI-DRAN0279-DVM-KEY.
               10 PI-DRAN0279-PGEB-GES      PIC  S9(04) COMP.
               10 PI-DRAN0279-DVMH-VNR      PIC  S9(15) COMP-3.
      *
       01 PO-DRAN0279.
          05 PO-DRAN0279-RC                 PIC  X(003).
             88 PO-DRAN0279-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0279-RC-NOK                 VALUE 'NOK'.
 
          05 PO-DRAN0279-DATEN.
      *
             10 PO-DRAN0279-DVMH-FA-RF          PIC S9(004) COMP.
             10 PO-DRAN0279-DVMH-FA-N1          PIC X(030).
             10 PO-DRAN0279-DVMH-FA-N2          PIC X(030).
220216       10 PO-DRAN0279-DVMH-FA-N-LANG      PIC X(070).
             10 PO-DRAN0279-DVMH-FA-STR         PIC X(045).
             10 PO-DRAN0279-DVMH-FA-PLZ         PIC X(015).
             10 PO-DRAN0279-DVMH-FA-ORT         PIC X(050).
             10 PO-DRAN0279-DVMH-FA-POSTF       PIC X(010).
             10 PO-DRAN0279-DVMH-FA-PLZ-POSTF   PIC X(005).
             10 PO-DRAN0279-DVMH-FA-ORT-POSTF   PIC X(040).
      *
110815*      10 PO-DRAN0279-DVMH-FA-TELE        PIC X(015).
110815       10 PO-DRAN0279-DVMH-FA-TELE        PIC X(030).
110815*      10 PO-DRAN0279-DVMH-FA-FAX         PIC X(015).
110815       10 PO-DRAN0279-DVMH-FA-FAX         PIC X(030).
110815*      10 PO-DRAN0279-DVMH-FA-MOBIL       PIC X(015).
110815       10 PO-DRAN0279-DVMH-FA-MOBIL       PIC X(030).
110815*      10 PO-DRAN0279-DVMH-FA-EMAIL       PIC X(040).
110815       10 PO-DRAN0279-DVMH-FA-EMAIL       PIC X(050).
      *
             10 PO-DRAN0279-DVMH-FA-KZ-VST      PIC X(001).
             10 PO-DRAN0279-DVMH-FA-ST-NR       PIC X(015).
             10 PO-DRAN0279-DVMH-FA-HREG-AMTG   PIC X(050).
             10 PO-DRAN0279-DVMH-FA-HREG-NR     PIC X(030).
             10 PO-DRAN0279-DVMH-FA-INH         PIC X(100).
      *
             10 PO-DRAN0279-DVMH-KZ-FAC-OPT     PIC X(001).
             10 PO-DRAN0279-DVMH-UMSATZ-JAHR    PIC S9(009)V COMP-3.
             10 PO-DRAN0279-DVMH-KZ-GARANT      PIC X(001).
             10 PO-DRAN0279-DVMH-KZ-GW-GES      PIC X(001).
             10 PO-DRAN0279-DVMH-KZ-N-ERKL      PIC X(001).
070715       10 PO-DRAN0279-DVMH-EINRGEBUEHR    PIC X(001).
      *
             10 PO-DRAN0279-DVMH-KIH-VN         PIC X(030).
             10 PO-DRAN0279-DVMH-KIH-ZN         PIC X(030).
             10 PO-DRAN0279-DVMH-KIH-ABW-STR    PIC X(045).
             10 PO-DRAN0279-DVMH-KIH-ABW-PLZ    PIC X(015).
             10 PO-DRAN0279-DVMH-KIH-ABW-ORT    PIC X(050).
             10 PO-DRAN0279-DVMH-GELDINST       PIC X(058).
             10 PO-DRAN0279-DVMH-IBAN           PIC X(034).
             10 PO-DRAN0279-DVMH-BIC            PIC X(011).
             10 PO-DRAN0279-DVMH-SEPA-M-BEG     PIC S9(009) COMP-3.
      *


