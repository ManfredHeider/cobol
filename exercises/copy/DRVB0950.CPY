      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0950.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0950.CPv  $
      *
      *   Rev 2.4   Oct 05 2009 16:18:06   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.3   Oct 05 2009 16:10:22   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.2   Oct 05 2009 15:50:42   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.1   Oct 05 2009 15:18:14   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.0   Aug 14 2009 14:30:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0950                                     *
      *    FUNKTION KAPITÄNS-CLUB DVAG(Wien), KANDIDATEN ERMITTELN   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.08.2009                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00847!14.08.09! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0950                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0950.
          05 PI-DRVB0950-PROGRAMM             PIC  X(008).
          05 PI-DRVB0950-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0950.
          05 PO-DRVB0950-RC                   PIC  X(003).
             88 PO-DRVB0950-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0950-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0950-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0950-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0950-DATEN.
             10 PO-DRVB0950-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0950-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0950-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0950-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0950-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0950-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0950 *****************************
