      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0116.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0116.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:13:16   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0116  FUNKTION LIST-OZP FÜR TABELLE VBP  *
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
      *    FÜR DIE FUNKTION DRVB0116                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0116.
          05 PI-DRVB0116-PROGRAMM           PIC  X(008).
          05 PI-DRVB0116-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0116.
          05 PO-DRVB0116-RC                 PIC  X(003).
             88 PO-DRVB0116-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0116-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0116-RC-EOF                       VALUE 'EOF'.
          05 PO-DRVB0116-DATEN.
             10 PO-DRVB0116-VBP-NR          PIC S9(004)  BINARY.
             10 PO-DRVB0116-VBP-PROV-STF    PIC S9(003)V99
                                                         PACKED-DECIMAL.
             10 PO-DRVB0116-VBP-GUELTIG-BIS PIC  X(010).
      *
      ***** Ende-Copy-Member *****************************************
