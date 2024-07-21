      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0877.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0877.CPv  $
      *
      *   Rev 2.1   14 Jan 2008 13:00:54   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   30 Nov 2007 13:39:14   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0877                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    30.11.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!30.11.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0877 - ERMITTELN OFFENE ANSPRÜCHE AUS LZUA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0877.
           05  PI-DRVB0877-DATEN.
               10  PI-DRVB0877-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0877-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0877-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0877-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0877-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0877.
           05  PO-DRVB0877-RC                    PIC  X(003).
               88  PO-DRVB0877-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0877-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0877-DATEN.
               10  PO-DRVB0877-LZUA-ST-NR        PIC S9(009) COMP.
      **** ACHTUNG: REIHENFOLGE KTY + KNR VERTAUSCHT, ANALOG WB-VIVB0093
               10  PO-DRVB0877-KNR-OLZ-TYP       PIC S9(004) COMP.
               10  PO-DRVB0877-KTY-OLZ-TYP       PIC S9(004) COMP.
               10  PO-DRVB0877-LZUA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0877-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0877-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0877-LZUA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0877-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0877-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0877-LZUA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0877-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0877-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0877 ****************************************
      ****
