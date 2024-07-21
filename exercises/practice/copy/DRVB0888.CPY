      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0888.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0888.CPv  $
      *
      *   Rev 2.1   25 Feb 2008 11:29:36   t13ex43
      *ASL00292: PGM - Auszahlung Direktions-Aufbau-Plan
      *
      *   Rev 2.0   31 Jan 2008 11:04:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0888                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    31.01.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00292!31.01.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0888 - ERMITTELN OFFENE ANSPRÜCHE AUS DAPA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0888.
           05  PI-DRVB0888-DATEN.
               10  PI-DRVB0888-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0888-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0888-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0888-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0888-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0888.
           05  PO-DRVB0888-RC                    PIC  X(003).
               88  PO-DRVB0888-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0888-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0888-DATEN.
               10  PO-DRVB0888-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0888-DAPA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0888-DAPA-PART-VBNR    PIC S9(009) COMP.
               10  PO-DRVB0888-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0888-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0888-DAPA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0888-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0888-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0888-DAPA-VBNR-TYP     PIC  X(001).
               10  PO-DRVB0888-DAPA-STUFE        PIC  X(004).
               10  PO-DRVB0888-DAPA-BEF-DAT      PIC  X(010).
               10  PO-DRVB0888-DAPA-EH-GESAMT    PIC S9(009)V9  COMP-3.
               10  PO-DRVB0888-DAPA-EH-EIGEN     PIC S9(009)V9  COMP-3.
               10  PO-DRVB0888-DAPA-EQF-GESAMT   PIC S9(003)V99 COMP-3.
               10  PO-DRVB0888-DAPA-RATE         PIC  X(001).
               10  PO-DRVB0888-DAPA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0888-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0888-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0888 ****************************************
      ****
