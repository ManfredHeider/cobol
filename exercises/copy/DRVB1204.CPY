      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1204.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1204.CPv  $
      *
      *   Rev 2.1   Oct 10 2018 17:21:54   t13ae30
      *AVB01519: F&A-SST - Neue Kriterien Qualitätsmanagm. bzw. Ums
      *atzst. des VB
      *
      *   Rev 2.0   Sep 12 2018 16:14:38   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1204                                     *
      *    FUNKTION ERM. STATUS ERFÜLLUNG KRITERIUM QUD UND USV      *
      *    FÜR SEMINARIS                                             *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    12.09.2018                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01519!12.09.18! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1204                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1204.
          05 PI-DRVB1204-MA-ST-NR           PIC  9(007).
      *
       01 PO-DRVB1204.
          05 PO-DRVB1204-RC                 PIC  X(003).
             88 PO-DRVB1204-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1204-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1204-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1204-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1204-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1204-DATEN.
             10 PO-DRVB1204-QUD-STATUS    PIC X(001).
             10 PO-DRVB1204-USV-STATUS    PIC X(001).
      *
      ******** Ende Copy-Member DRVB1204 *****************************

