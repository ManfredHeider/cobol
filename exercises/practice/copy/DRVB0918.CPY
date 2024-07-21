      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0918.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0918.CPv  $
      *
      *   Rev 2.2   Feb 20 2009 15:52:40   t13ae30
      *FVA00002: Abbruch Champions-Club
      *
      *   Rev 2.1   Jan 09 2009 17:04:32   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 01 2008 15:50:16   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0918                                     *
      *    FUNKTION CHAMPIONS-CLUB, MITGLIEDER ERMITTELN             *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    01.09.2008                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00779!01.09.08! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * FVA00002!20.02.09! 02 ! KORREKTUR                 ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0918                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0918.
          05 PI-DRVB0918-PROGRAMM           PIC  X(008).
          05 PI-DRVB0918-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0918.
          05 PO-DRVB0918-RC                 PIC  X(003).
             88 PO-DRVB0918-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0918-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0918-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0918-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0918-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0918-DATEN.
200209*      10 PO-DRVB0918-MA-ST-NR             PIC  9(007) COMP-3.
200209       10 PO-DRVB0918-MA-ST-NR             PIC S9(009)  COMP.
             10 PO-DRVB0918-MAST-DAT-VON         PIC  X(026).
             10 PO-DRVB0918-KTY-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0918-KNR-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0918-MAST-LAST-UPDATE     PIC  X(026).
             10 PO-DRVB0918-MAST-DAT-BIS         PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0918 *****************************
