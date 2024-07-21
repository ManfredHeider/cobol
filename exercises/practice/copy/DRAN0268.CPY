      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0268.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0268.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:14:32   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:09:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:21:00   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:50:20   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:55:22   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 22 2015 15:45:10   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 17:39:18   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:25:00   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:46   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0268  FUNKTION SELECT ÜBER GES, VN, AVERM*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01198!07.07.15!2.4 ! NEU FELD EINR.GEBÜHR      ! RAHNER   *
      * AAN01198!11.08.15!2.5 ! VERGRÖßERN FELDER         ! RAHNER   *
      * AAN01240!22.02.16!2.6 ! NEU FLED FIRMA-NAME-LANG  ! RAHNER   *
      * AAN01370!22.11.18!2.8 ! C+L DRAN0273              ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0268                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE DVM  TBAN0020              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0268.
           05 PI-DRAN0268-DVM-KEY.
               10 PI-DRAN0268-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0268-DVM-HVANTNUM    PIC X(9).
               10 PI-DRAN0268-DVM-AVERM       PIC S9(9) COMP.
      *
       01  PO-DRAN0268.
           05 PO-DRAN0268-RC                  PIC  X(003).
             88 PO-DRAN0268-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0268-RC-NOT-FOUND           VALUE 'NFD'.
             88 PO-DRAN0268-RC-KEY-NOT-UNIQUE      VALUE 'KNU'.
      *
           05 PO-DRAN0268-DVMB-DATEN.
               10 PO-DRAN0268-DVM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0268-DVM-KENNUNG-A   PIC X(8).
 
      *    DVM-DATEN AUS PC-DATEN
           COPY DRAN0273.
      ****** ENDE COPY-MEMBER ****************************************





