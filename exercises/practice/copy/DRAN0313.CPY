      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0313.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0313.CPv  $
      *
      *   Rev 2.3   Apr 12 2018 09:26:36   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrere "Inhaber" in der Antr
      *agserf.
      *
      *   Rev 2.2   Mar 26 2018 11:16:08   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.1   Mar 20 2018 10:39:44   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.0   Jan 04 2018 10:49:42   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0313  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!04.01.18!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0313                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE DVM TBAN0027               *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0313.
           05 PI-DRAN0313-INHA-KEY.
               10 PI-DRAN0313-PGEB-GES       PIC S9(04) COMP.
               10 PI-DRAN0313-INHA-VNR       PIC S9(15) COMP-3.
      *
       01  PO-DRAN0313.
           05 PO-DRAN0313-RC                 PIC  X(003).
             88 PO-DRAN0313-RC-OK                 VALUE 'OK '.
             88 PO-DRAN0313-RC-CHANGED            VALUE 'CHA'.
             88 PO-DRAN0313-RC-NOT-FOUND          VALUE 'NFD'.
      *



