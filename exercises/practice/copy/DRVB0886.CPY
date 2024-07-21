      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0886.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0886.CPv  $
      *
      *   Rev 2.3   Dec 03 2009 14:15:38   t13ae27
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.2   Feb 10 2009 15:06:48   t13ae27
      *AAS00796: Eliteclub Unternehmensaufbau 2009
      *
      *   Rev 2.1   Mar 14 2008 16:33:06   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.0   18 Jan 2008 10:41:10   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0886                                     *
      *    FUNKTION Elite-CLUB KANDIDATEN ERMITTELN                  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: P. Duerr                                        *
      *    DATUM:    18.01.2008                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00733!18.01.08! 01 ! NEUERSTELLUNG             ! P. Duerr *
      * AAS00796!06.01.09! 02 ! Gewinner-KZ in Übergabeber! P. Duerr *
      * AAS00847!30.09.09! 03 ! MAST-TYP in PI-bereich    ! P. Duerr *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0886                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0886.
          05 PI-DRVB0886-PROGRAMM           PIC  X(008).
          05 PI-DRVB0886-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0886-WB-TYP             PIC S9(004) COMP.
          05 PI-DRVB0886-WERTUNGS-TYP       PIC S9(004) COMP.
          05 PI-DRVB0886-GESELL             PIC S9(004) COMP.
          05 PI-DRVB0886-DAT-BIS            PIC  X(010).
          05 PI-DRVB0886-LIST-TYP           PIC S9(004) COMP.
300909    05 PI-DRVB0886-MAST-TYP           PIC S9(004) COMP.
      *
       01 PO-DRVB0886.
          05 PO-DRVB0886-RC                 PIC  X(003).
             88 PO-DRVB0886-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0886-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0886-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0886-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0886-DATEN.
             10 PO-DRVB0886-MA-ST-NR        PIC  9(007)  COMP-3.
060109       10 PO-DRVB0886-GEWINNER-KZ     PIC  X(001).

      *
      ******** Ende Copy-Member DRVB0886 *****************************
