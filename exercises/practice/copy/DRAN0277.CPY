      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0277.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0277.CPv  $
      *
      *   Rev 2.7   Feb 25 2016 10:10:28   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:22:12   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:51:36   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:56:02   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   May 28 2015 09:54:18   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.2   May 06 2015 11:05:12   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.1   May 05 2015 16:08:58   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Mar 16 2015 17:07:00   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRAN0277  FUNKTION INSERT STANDARD-OZP
      *    FÜR DVM-TBAN0024
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01198!07.07.15!2.4 ! NEU FELD EINR.GEBÜHR      ! RAHNER   *
      * AAN01198!11.08.15!2.5 ! VERGRÖßERN FELDER         ! RAHNER   *
      * AAN01240!22.02.16!2.6 ! NEU FELD FIRMA-NAME-LANG  ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0277                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE DVM  TBAN0024              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0277.
           05 PI-DRAN0277-DVM-KEY.
              10 PI-DRAN0277-PGEB-GES              PIC S9(004) COMP.
              10 PI-DRAN0277-DVMH-VNR              PIC S9(015) COMP-3.
      *
      *        KMU (FIRMA)/ANTRAGSTELLER
               10  PI-DRAN0277-DVMH-FA-RF           PIC 9(003).
               10  PI-DRAN0277-DVMH-FA-N1           PIC X(030).
               10  PI-DRAN0277-DVMH-FA-N2           PIC X(030).
220216         10  PI-DRAN0277-DVMH-FA-N-LANG       PIC X(070).
               10  PI-DRAN0277-DVMH-FA-STR          PIC X(045).
               10  PI-DRAN0277-DVMH-FA-PLZ          PIC X(015).
               10  PI-DRAN0277-DVMH-FA-ORT          PIC X(050).
               10  PI-DRAN0277-DVMH-FA-POSTF        PIC X(010).
               10  PI-DRAN0277-DVMH-FA-PLZ-POSTF    PIC X(005).
               10  PI-DRAN0277-DVMH-FA-ORT-POSTF    PIC X(040).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER ERREICHBARKEIT
110815*        10  PI-DRAN0277-DVMH-FA-TELE         PIC X(015).
110815         10  PI-DRAN0277-DVMH-FA-TELE         PIC X(030).
110815*        10  PI-DRAN0277-DVMH-FA-FAX          PIC X(015).
110815         10  PI-DRAN0277-DVMH-FA-FAX          PIC X(030).
110815*        10  PI-DRAN0277-DVMH-FA-MOBIL        PIC X(015).
110815         10  PI-DRAN0277-DVMH-FA-MOBIL        PIC X(030).
110815*        10  PI-DRAN0277-DVMH-FA-EMAIL        PIC X(040).
110815         10  PI-DRAN0277-DVMH-FA-EMAIL        PIC X(050).
      *
      *        KMU (FIRMA)/ANTRAGSTELLER RECHT
               10  PI-DRAN0277-DVMH-FA-KZ-VST       PIC X(001).
               10  PI-DRAN0277-DVMH-FA-ST-NR        PIC X(015).
               10  PI-DRAN0277-DVMH-FA-HREG-AMTG    PIC X(050).
               10  PI-DRAN0277-DVMH-FA-HREG-NR      PIC X(030).
               10  PI-DRAN0277-DVMH-FA-INH          PIC X(100).
      *
      *        KMU SONSTIGES
               10  PI-DRAN0277-DVMH-KZ-FAC-OPT      PIC X(001).
               10  PI-DRAN0277-DVMH-UMSATZ-JAHR     PIC 9(006) COMP.
               10  PI-DRAN0277-DVMH-KZ-GARANT       PIC X(001).
               10  PI-DRAN0277-DVMH-KZ-GW-GES       PIC X(001).
               10  PI-DRAN0277-DVMH-KZ-N-ERKL       PIC X(001).
070715         10  PI-DRAN0277-DVMH-EINRGEBUEHR     PIC X(001).
      *
      *        KMU(FIRMA) KONTO INFORMATIONEN
               10  PI-DRAN0277-DVMH-KIH-VN          PIC X(030).
               10  PI-DRAN0277-DVMH-KIH-ZN          PIC X(030).
               10  PI-DRAN0277-DVMH-KIH-ABW-STR     PIC X(045).
               10  PI-DRAN0277-DVMH-KIH-ABW-PLZ     PIC X(015).
               10  PI-DRAN0277-DVMH-KIH-ABW-ORT     PIC X(050).
               10  PI-DRAN0277-DVMH-GELDINST        PIC X(058).
               10  PI-DRAN0277-DVMH-BIC             PIC X(011).
               10  PI-DRAN0277-DVMH-IBAN            PIC X(034).
               10  PI-DRAN0277-DVMH-SEPA-M-BEG      PIC 9(008) COMP-3.
      *
       01  PO-DRAN0277.
           05 PO-DRAN0277-RC                        PIC  X(003).
             88 PO-DRAN0277-RC-OK                       VALUE 'OK '.
             88 PO-DRAN0277-RC-DUPLICATE-VALUE          VALUE 'DVA'.




