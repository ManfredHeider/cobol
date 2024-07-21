      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1250.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1250.CPv  $
      *
      *   Rev 2.1   Oct 30 2019 12:49:52   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 02 2019 17:54:42   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1250                                     *
      *    FUNKTION ERM. AKT. MITGLIEDER IM BES.LEIST.AS-CLUB, WIEN  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    11.09.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VBL-964 !11.09.19! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1250 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1250.
          05 PI-DRVB1250-PROGRAMM             PIC  X(008).
          05 PI-DRVB1250-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1250.
          05 PO-DRVB1250-RC                   PIC  X(003).
             88 PO-DRVB1250-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1250-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1250-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1250-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1250-DATEN.
             10 PO-DRVB1250-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB1250-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB1250-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1250-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1250-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB1250-MAST-DAT-BIS      PIC  X(010).
      *
      ******** ENDE COPY-MEMBER DRVB1250 *****************************

