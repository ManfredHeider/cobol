      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1846.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1846.CPv  $
      *
      *   Rev 2.1   Jul 04 2016 10:06:28   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.0   Jul 01 2016 16:52:08   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1846  FUNKTION SELECT NEUESTER EINTRAG   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDERn                                         *
      *    DATUM:    01.07.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN1269 !01.07.16!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1846                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-NEUESTER EINTRAG <<<         PGTA TBPA0009    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1846.
          05 PI-DRPA1846-PGTA-KEY.
             10 PI-DRPA1846-PGEB-GES        PIC S9(004)  BINARY.
             10 PI-DRPA1846-PGTA-TARIF      PIC  X(015).
      *
       01 PO-DRPA1846.
          05 PO-DRPA1846-RC                 PIC  X(003).
             88 PO-DRPA1846-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1846-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1846-PGTA-DATEN.
             10 PO-DRPA1846-PGTA-GUELT-BIS  PIC  X(010).
             10 PO-DRPA1846-PROD-PROD-SCHL  PIC S9(004)  BINARY.
          05 PO-DRPA1846-PGTA-LAST-UPDATE   PIC  X(026).
          05 PO-DRPA1846-PGTA-KENNUNG       PIC  X(008).
          05 PO-DRPA1846-KNR-MANDANT        PIC S9(004)  BINARY.
      *
      ****** Ende Copy-Member ****************************************

