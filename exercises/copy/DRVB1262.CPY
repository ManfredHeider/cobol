      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1262.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1262.CPv  $
      *
      *   Rev 2.1   Oct 30 2019 12:59:10   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 02 2019 17:56:58   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1262                                     *
      *    FUNKTION ERM. AKTUELLE MITGLIEDER IM AS-CLUB, ALLFINANZ2 *
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
      *    FÜR DIE FUNKTION DRVB1262 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1262.
          05 PI-DRVB1262-PROGRAMM             PIC  X(008).
          05 PI-DRVB1262-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1262.
          05 PO-DRVB1262-RC                   PIC  X(003).
             88 PO-DRVB1262-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1262-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1262-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1262-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1262-DATEN.
             10 PO-DRVB1262-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB1262-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB1262-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1262-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB1262-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB1262-MAST-DAT-BIS      PIC  X(010).
      *
      ******** ENDE COPY-MEMBER DRVB1262 *****************************

