      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0114.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0114.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:13:04   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0114  FUNKTION LIST-OZP FÜR TABELLE MS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    22.10.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !22.10.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0114                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0114.
          05 PI-DRVB0114-PROGRAMM           PIC  X(008).
          05 PI-DRVB0114-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0114.
          05 PO-DRVB0114-RC                 PIC  X(003).
             88 PO-DRVB0114-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0114-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0114-RC-EOF                       VALUE 'EOF'.
          05 PO-DRVB0114-DATEN.
             10 PO-DRVB0114-MS-NR           PIC S9(004)  BINARY.
             10 PO-DRVB0114-MS-BEZ-KURZ     PIC  X(004).
             10 PO-DRVB0114-MS-PROV-STF     PIC S9(003)V99
                                                         PACKED-DECIMAL.
             10 PO-DRVB0114-MS-GUELTIG-BIS  PIC  X(010).
      *
      ***** Ende-Copy-Member *****************************************
