      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0887.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0887.CPv  $
      *
      *   Rev 2.3   Nov 19 2009 10:34:22   t13ae27
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.2   Mar 13 2008 11:03:08   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.1   04 Mar 2008 13:38:18   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.0   18 Jan 2008 12:11:36   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0887                                     *
      *    FUNKTION AS-CLUB-MITGLIEDER ERMITTELN, DEREN MITGLIED-    *
      *    SCHAFT IM AS-CLUB ERNEUT ÜBERPRÜFT WERDEN MUß.            *
      *    (NACH ABLAUF DER MINDESTMITGLIEDSDAUER VON 3-MONATEN)     *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00158!14.10.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0887                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0887.
          05 PI-DRVB0887-PROGRAMM                PIC  X(008).
          05 PI-DRVB0887-UFKT-CODE               PIC  9(002).
          05 PI-DRVB0887-KNR-ELITECLUB           PIC S9(004) COMP.
      *
       01 PO-DRVB0887.
          05 PO-DRVB0887-RC                      PIC  X(003).
             88 PO-DRVB0887-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0887-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0887-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0887-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0887-DATEN.
             10 PO-DRVB0887-MA-ST-NR             PIC  S9(009)  COMP.
             10 PO-DRVB0887-MAST-DAT-VON         PIC  X(026).
             10 PO-DRVB0887-KTY-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0887-KNR-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0887-MAST-LAST-UPDATE     PIC  X(026).
             10 PO-DRVB0887-MAST-DAT-BIS         PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0887 *****************************
