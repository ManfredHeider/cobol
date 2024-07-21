      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0807.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0807.CPv  $
      *
      *   Rev 2.2   Jun 24 2008 12:58:20   t13ae30
      *ASL00302: Tankgutscheine 2008
      *
      *   Rev 2.1   Nov 29 2005 17:28:58   t13ae30
      *AVB00827: Neuauflage Riester-WB mit Neukundenausprägung
      *
      *   Rev 2.0   Nov 15 2005 15:57:04   t13ae30
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0807  (ERSTELLT F. RIESTERWETTBEWERB)    *
      *    FUNKTION ERMITTELT FÜR EINEN VB ALLE VETRÄGE FÜR DIE      *
      *    NOCH KEINE LEISTUNG2 GEZAHLT WURDE UND DEREN NEUKUNDEN-KZ *
      *    MIT 'NULL' GEFÜLLT IST.                                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    15.11.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00827!15.11.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * ASL00302!27.05.08! 02 ! ERW.VERSNR. 10 --> 15BYTE ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0807                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0807.
          05 PI-DRVB0807-PROGRAMM                PIC  X(008).
          05 PI-DRVB0807-UFKT-CODE               PIC  9(002).
      *
       01 PO-DRVB0807.
          05 PO-DRVB0807-RC                      PIC  X(003).
             88 PO-DRVB0807-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0807-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0807-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0807-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0807-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0807-DATEN.
             10 NI-DRVB0807-TAB.
                15  NI-DRVB0807-ELE PIC S9(04)  COMP   OCCURS  15 TIMES.
             10 NI-DRVB0807            REDEFINES   NI-DRVB0807-TAB.
                15 NI-RINFO-VBNR                    PIC S9(04)  COMP.
                15 NI-RINFO-VERSNR                  PIC S9(04)  COMP.
                15 NI-RINFO-MONAT                   PIC S9(04)  COMP.
                15 NI-RINFO-KZ                      PIC S9(04)  COMP.
                15 NI-RINFO-LAST-UPDATE             PIC S9(04)  COMP.
                15 NI-RINFO-LEISTG1                 PIC S9(04)  COMP.
                15 NI-PF-KZ-LEISTG1                 PIC S9(04)  COMP.
                15 NI-RINFO-LEISTG2                 PIC S9(04)  COMP.
                15 NI-RINFO-NEUKDN-KZ               PIC S9(04)  COMP.
                15 NI-PF-KZ-LEISTG2                 PIC S9(04)  COMP.
                15 NI-RINFO-LEISTG3                 PIC S9(04)  COMP.
                15 NI-PF-KZ-LEISTG3                 PIC S9(04)  COMP.
                15 NI-RINFO-LEISTG4                 PIC S9(04)  COMP.
                15 NI-PF-KZ-LEISTG4                 PIC S9(04)  COMP.
                15 NI-RINFO-BEMERKUNG               PIC S9(04)  COMP.
             10 PO-DRVB0807-ATTR.
                15 PO-DRVB0807-RINFO-VBNR           PIC S9(9)   COMP.
270508*         15 PO-DRVB0807-RINFO-VERSNR         PIC X(10).
270508          15 PO-DRVB0807-RINFO-VERSNR         PIC X(15).
                15 PO-DRVB0807-RINFO-MONAT          PIC X(10).
                15 PO-DRVB0807-RINFO-KZ             PIC X(02).
                15 PO-DRVB0807-RINFO-LAST-UPDATE    PIC X(26).
                15 PO-DRVB0807-RINFO-LEISTG1        PIC X(10).
                15 PO-DRVB0807-PF-KZ-LEISTG1        PIC X(01).
                15 PO-DRVB0807-RINFO-LEISTG2        PIC X(10).
                15 PO-DRVB0807-RINFO-NEUKDN-KZ      PIC X(01).
                15 PO-DRVB0807-PF-KZ-LEISTG2        PIC X(01).
                15 PO-DRVB0807-RINFO-LEISTG3        PIC X(10).
                15 PO-DRVB0807-PF-KZ-LEISTG3        PIC X(01).
                15 PO-DRVB0807-RINFO-LEISTG4        PIC X(10).
                15 PO-DRVB0807-PF-KZ-LEISTG4        PIC X(01).
                15 PO-DRVB0807-RINFO-BEMERKUNG      PIC X(20).
      *
      ******** Ende Copy-Member DRVB0807 *****************************
