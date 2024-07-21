      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1251.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1251.CPv  $
      *
      *   Rev 2.1   Oct 30 2019 12:50:36   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 02 2019 17:54:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1251                                     *
      *    FUNKTION ERM. AKT. MITGLIEDER IM AS-CLUB, WIEN, DIE NICHT *
      *    IN DEN BES.LEIST.IM AS-CLUB, WIEN SIND.                   *
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
      *    FÜR DIE FUNKTION DRVB1251 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1251.
          05 PI-DRVB1251-PROGRAMM             PIC  X(008).
          05 PI-DRVB1251-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1251.
          05 PO-DRVB1251-RC                   PIC  X(003).
             88 PO-DRVB1251-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1251-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1251-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1251-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1251-DATEN.
             10 PO-DRVB1251-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB1251-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB1251-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1251-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1251-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB1251-MAST-DAT-BIS      PIC  X(010).
      *
      ******** ENDE COPY-MEMBER DRVB1251 *****************************

