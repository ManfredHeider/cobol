      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0666.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0666.CPv  $
      *
      *   Rev 2.1   Aug 14 2003 14:10:38   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.0   Feb 17 2003 10:58:40   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0666                                     *
      *    FUNKTION ERM. AKTUELLE MITGLIEDER IM AS-CLUB              *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    17.07.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00681!17.07.03! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0666 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0666.
          05 PI-DRVB0666-PROGRAMM             PIC  X(008).
          05 PI-DRVB0666-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0666.
          05 PO-DRVB0666-RC                   PIC  X(003).
             88 PO-DRVB0666-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0666-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0666-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0666-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0666-DATEN.
             10 PO-DRVB0666-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0666-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0666-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0666-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0666-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0666-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0666 *****************************
