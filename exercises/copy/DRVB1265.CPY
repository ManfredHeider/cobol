      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1265.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1265.CPv  $
      *
      *   Rev 2.1   Oct 30 2019 13:01:32   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 07 2019 17:53:24   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1265                                     *
      *    FUNKTION ERM. AKT. MITGL. IM AS-CLUB, ALLF2 - MIT         *
      *    BESCHRÄNKUNG KEINE MASCH. ERM. IM AS-CLUB, ALLF1          *
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
      *    FÜR DIE FUNKTION DRVB1265 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1265.
          05 PI-DRVB1265-PROGRAMM             PIC  X(008).
          05 PI-DRVB1265-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1265.
          05 PO-DRVB1265-RC                   PIC  X(003).
             88 PO-DRVB1265-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1265-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1265-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1265-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1265-DATEN.
             10 PO-DRVB1265-MA-ST-NR          PIC S9(009)  COMP.
      *
      ******** ENDE COPY-MEMBER DRVB1265 *****************************

