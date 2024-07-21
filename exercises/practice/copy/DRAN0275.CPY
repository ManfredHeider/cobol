      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0275.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0275.CPv  $
      *
      *   Rev 2.5   Feb 25 2016 10:10:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.4   Feb 22 2016 14:22:02   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.3   Aug 12 2015 14:51:28   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.2   Jul 07 2015 15:55:54   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   May 05 2015 16:20:10   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Feb 24 2015 10:21:02   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!05.05.15!2.1  ! NEU                      ! RAHNER   *
      * AAN01198!07.07.15!2.2  ! NEU FELD EINR.GEBÜHR     ! RAHNER   *
      * AAN01198!11.08.15!2.3  ! VERGRÖßERN FELDER        ! RAHNER   *
      * AAN01240!22.02.16!2.4  ! NEU FELD FIRMA-NAME-LANG ! RAHNER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
           20  DRAN0275-MASKENFELDER-DVM.
               30 DVM-VNR-PZ           PIC  X(002).
               30 DVM-FA-RF            PIC  X(003).
               30 DVM-FA-NAME1         PIC  X(030).
               30 DVM-FA-NAME2         PIC  X(030).
220216         30 DVM-FA-NAME-LANG     PIC  X(070).
               30 DVM-FA-STRASSE       PIC  X(045).
               30 DVM-FA-PLZ           PIC  X(005).
               30 DVM-FA-ORT           PIC  X(050).
               30 DVM-FA-POSTF         PIC  X(010).
               30 DVM-FA-PLZ-POSTF     PIC  X(005).
               30 DVM-FA-ORT-POSTF     PIC  X(040).
110815*        30 DVM-FA-TELE          PIC  X(015).
110815         30 DVM-FA-TELE          PIC  X(030).
110815*        30 DVM-FA-FAX           PIC  X(015).
110815         30 DVM-FA-FAX           PIC  X(030).
110815*        30 DVM-FA-MOBIL         PIC  X(015).
110815         30 DVM-FA-MOBIL         PIC  X(030).
110815*        30 DVM-FA-EMAIL         PIC  X(040).
110815         30 DVM-FA-EMAIL         PIC  X(050).
               30 DVM-FA-KZ-VST        PIC  X(001).
               30 DVM-FA-ST-NR         PIC  X(015).
               30 DVM-FA-HREG-AMTG     PIC  X(050).
               30 DVM-FA-HREG-NR       PIC  X(030).
               30 DVM-FA-INH           PIC  X(100).
070715         30 DVM-FA-EINRGEBUEHR   PIC  X(001).
               30 DVM-FA-KIH-VN        PIC  X(030).
               30 DVM-FA-KIH-ZN        PIC  X(030).
               30 DVM-FA-KIH-ABW-STR   PIC  X(045).
               30 DVM-FA-KIH-ABW-PLZ   PIC  X(015).
               30 DVM-FA-KIH-ABW-ORT   PIC  X(050).
               30 DVM-FA-GELDINST      PIC  X(058).
               30 DVM-KZ-FAC-OPT       PIC  X(001).
               30 DVM-UMSATZ-JAHR      PIC  X(009).
               30 DVM-KZ-GW-GES        PIC  X(001).
               30 DVM-KZ-N-ERKL        PIC  X(001).
               30 DVM-KZ-U-VOLLM       PIC  X(001).
               30 DVM-SEPA-M-BEG       PIC  X(006).


