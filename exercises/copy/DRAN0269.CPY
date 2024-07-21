      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0269.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0269.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:14:42   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:09:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:21:10   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:50:28   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:55:28   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 22 2015 15:45:16   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 17:39:22   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:25:12   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0269  FUNKTION SELECT ÜBER GES, VNR      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01198!07.07.15!2.4 ! NEU FELD EINR.GEBÜHR      ! RAHNER   *
      * AAN01198!11.08.15!2.5 ! VERGRÖßERN FELDER         ! RAHNER   *
      * AAN01240!22.02.16!2.6 ! NEU FELD FIRMA-NAME-LANG  ! RAHNER   *
      * AAN01370!22.11.18!2.8 ! C+L DRAN0273              ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0269                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE DVM TBAN0020              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0269.
           05 PI-DRAN0269-DVM-KEY.
               10 PI-DRAN0269-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0269-DVM-VNR         PIC S9(15) COMP-3.
      *
       01  PO-DRAN0269.
           05 PO-DRAN0269-RC                  PIC  X(003).
             88 PO-DRAN0269-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0269-RC-NOT-FOUND           VALUE 'NFD'.
             88 PO-DRAN0269-RC-KEY-NOT-UNIQUE      VALUE 'KNU'.
      *
           05 PO-DRAN0269-DVM-DATEN.
               10 PO-DRAN0269-DVM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0269-DVM-KENNUNG-A   PIC X(8).
 
      *    DVM-DATEN AUS PC-DATEN
           COPY DRAN0273.
 
      ****** ENDE COPY-MEMBER ****************************************





