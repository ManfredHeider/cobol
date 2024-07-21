      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0889.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0889.CPv  $
      *
      *   Rev 2.1   25 Feb 2008 11:29:44   t13ex43
      *ASL00292: PGM - Auszahlung Direktions-Aufbau-Plan
      *
      *   Rev 2.0   31 Jan 2008 11:05:06   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0889                                      *
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
      *    FUNKTION DRVB0889 - ERMITTELN OFFENE ANSPRÜCHE AUS MAPA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0889.
           05  PI-DRVB0889-DATEN.
               10  PI-DRVB0889-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0889-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0889-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0889-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0889-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0889.
           05  PO-DRVB0889-RC                    PIC  X(003).
               88  PO-DRVB0889-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0889-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0889-DATEN.
               10  PO-DRVB0889-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0889-MAP-ANSPR-VOM     PIC  X(010).
               10  PO-DRVB0889-MAP-STUFE         PIC  X(010).
               10  PO-DRVB0889-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0889-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0889-MAP-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB0889-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0889-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0889-MAP-ANSPR-BETRAG  PIC S9(009)V99 COMP-3.
               10  PO-DRVB0889-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0889-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0889-MAPA-UNTST        PIC S9(009) COMP.
      ****
      **** ENDE COPY DRVB0889 ****************************************
      ****
