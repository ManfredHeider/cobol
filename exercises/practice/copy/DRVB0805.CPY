      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0805.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0805.CPv  $
      *
      *   Rev 2.2   Jun 24 2008 12:58:22   t13ae30
      *ASL00302: Tankgutscheine 2008
      *
      *   Rev 2.1   Nov 29 2005 17:28:34   t13ae30
      *AVB00827: Neuauflage Riester-WB mit Neukundenausprägung
      *
      *   Rev 2.0   Nov 08 2005 17:56:38   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0805  (ERSTELLT F. RIESTERWETTBEWERB)    *
      *    FUNKTION ERMITTELT FÜR EINEN VB ALLE VETRÄGE FÜR DIE      *
      *    NOCH KEINE LEISTUNG2 GEZAHLT WURDE UND DEREN NEUKUNDEN-KZ *
      *    MIT 'NULL' GEFÜLLT IST.                                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    09.11.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00827!09.11.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0805                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0805.
          05 PI-DRVB0805-PROGRAMM                PIC  X(008).
          05 PI-DRVB0805-UFKT-CODE               PIC  9(002).
          05 PI-DRVB0805-MA-ST-NR                PIC  S9(009)    COMP.
      *
       01 PO-DRVB0805.
          05 PO-DRVB0805-RC                      PIC  X(003).
             88 PO-DRVB0805-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0805-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0805-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0805-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0805-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0805-DATEN.
             10 PO-DRVB0805-RINFO-VBNR           PIC S9(9) COMP.
             10 PO-DRVB0805-RINFO-VERSNR         PIC X(10).
             10 PO-DRVB0805-RINFO-MONAT          PIC X(10).
             10 PO-DRVB0805-RINFO-KZ             PIC X(02).
             10 PO-DRVB0805-RINFO-LAST-UPDATE    PIC X(26).
             10 PO-DRVB0805-RINFO-LEISTG1        PIC X(10).
             10 PO-DRVB0805-PF-KZ-LEISTG1        PIC X(01).
             10 PO-DRVB0805-RINFO-LEISTG2        PIC X(10).
             10 PO-DRVB0805-RINFO-NEUKDN-KZ      PIC X(01).
             10 PO-DRVB0805-PF-KZ-LEISTG2        PIC X(01).
             10 PO-DRVB0805-RINFO-LEISTG3        PIC X(10).
             10 PO-DRVB0805-PF-KZ-LEISTG3        PIC X(01).
             10 PO-DRVB0805-RINFO-LEISTG4        PIC X(10).
             10 PO-DRVB0805-PF-KZ-LEISTG4        PIC X(01).
             10 PO-DRVB0805-RINFO-BEMERKUNG      PIC X(20).
      *
      ******** Ende Copy-Member DRVB0805 *****************************
