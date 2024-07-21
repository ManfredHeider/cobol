      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0875.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0875.CPv  $
      *
      *   Rev 2.1   14 Jan 2008 13:00:30   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   30 Nov 2007 13:38:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0875                                      *
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
      *    FUNKTION DRVB0875 - ERMITTELN OFFENE ANSPRÜCHE AUS BOZA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0875.
           05  PI-DRVB0875-DATEN.
               10  PI-DRVB0875-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0875-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0875-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0875-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0875-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0875.
           05  PO-DRVB0875-RC                    PIC  X(003).
               88  PO-DRVB0875-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0875-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0875-DATEN.
               10  PO-DRVB0875-BOZA-ST-NR        PIC S9(009) COMP.
               10  PO-DRVB0875-BOZA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0875-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0875-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0875-BOZA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0875-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0875-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0875-BOZA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0875-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0875-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0875 ****************************************
      ****
