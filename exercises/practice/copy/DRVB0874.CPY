      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0874.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0874.CPv  $
      *
      *   Rev 2.1   14 Jan 2008 13:00:18   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   30 Nov 2007 13:38:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0874                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.12.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!07.12.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0874 - ERMITTELN OFFENE ANSPRÜCHE AUS ABBA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0874.
           05  PI-DRVB0874-DATEN.
               10  PI-DRVB0874-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0874-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0874-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0874-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0874-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0874.
           05  PO-DRVB0874-RC                    PIC  X(003).
               88  PO-DRVB0874-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0874-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0874-DATEN.
               10  PO-DRVB0874-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0874-ABBA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0874-ABBA-PART-VBNR    PIC S9(009) COMP.
               10  PO-DRVB0874-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0874-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0874-ABBA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0874-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0874-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0874-ABBA-PART-BETRAG  PIC S9(009)V99 COMP-3.
               10  PO-DRVB0874-ABBA-PART-EH      PIC S9(009)V9  COMP-3.
               10  PO-DRVB0874-ABBA-PART-SEIT    PIC  X(010).
               10  PO-DRVB0874-ABBA-BETREUT-SEIT PIC  X(010).
               10  PO-DRVB0874-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0874-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0874 ****************************************
      ****
