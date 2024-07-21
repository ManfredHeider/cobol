      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0758.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0758.CPv  $
      *
      *   Rev 2.1   Jul 07 2005 11:55:08   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Mar 08 2005 17:47:48   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0666                                     *
      *    FUNKTION ERM. AKTUELLE MITGLIEDER IM ADMIRALS-CLUB        *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.03.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!08.03.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0758 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0758.
          05 PI-DRVB0758-PROGRAMM             PIC  X(008).
          05 PI-DRVB0758-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0758.
          05 PO-DRVB0758-RC                   PIC  X(003).
             88 PO-DRVB0758-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0758-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0758-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0758-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0758-DATEN.
             10 PO-DRVB0758-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0758-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0758-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0758-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0758-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0758-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0758 *****************************
