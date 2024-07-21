      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0266.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0266.CPv  $
      *
      *   Rev 2.4   Jun 22 2015 15:45:02   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 17:39:08   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:48:26   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:24:30   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:30   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0266  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0266                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE DVM TBAN0020               *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0266.
           05 PI-DRAN0266-DVM-KEY.
               10 PI-DRAN0266-PGEB-GES       PIC S9(04) COMP.
               10 PI-DRAN0266-DVM-VNR        PIC S9(15) COMP-3.
      *
       01  PO-DRAN0266.
           05 PO-DRAN0266-RC                 PIC  X(003).
             88 PO-DRAN0266-RC-OK                 VALUE 'OK '.
             88 PO-DRAN0266-RC-CHANGED            VALUE 'CHA'.
             88 PO-DRAN0266-RC-NOT-FOUND          VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 
 

