      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1150.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1150.CPv  $
      *
      *   Rev 2.2   Feb 10 2017 15:01:02   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.1   Jan 16 2017 11:02:02   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.0   Sep 15 2016 13:39:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1150                                     *
      *    FUNKTION ERM. AKTUELLE MITGLIEDER IM ADMIRALS-CLUB, WIEN  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    15.09.2016                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01186!15.09.16! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1150 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1150.
          05 PI-DRVB1150-PROGRAMM             PIC  X(008).
          05 PI-DRVB1150-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1150.
          05 PO-DRVB1150-RC                   PIC  X(003).
             88 PO-DRVB1150-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1150-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1150-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1150-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1150-DATEN.
             10 PO-DRVB1150-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB1150-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB1150-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1150-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1150-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB1150-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB1150 *****************************


