      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:42   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0016  FUNKTION LIST-OZP FÜR TABELLE ZVP  *
      *    --------------------------------------------------------  *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    02.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !02.09.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0016                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0016.
          05 PI-DRPA0016-PROGRAMM           PIC  X(008).
          05 PI-DRPA0016-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRPA0016.
          05 PO-DRPA0016-RC                 PIC  X(003).
             88 PO-DRPA0016-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0016-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0016-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0016-DATEN.
             10 PO-DRPA0016-PZVP-VERFAHREN  PIC  X(020).
             10 PO-DRPA0016-PGEB-GES        PIC S9(004)  BINARY.
             10 PO-DRPA0016-PROD-PROD-SCHL  PIC S9(004)  BINARY.
             10 PO-DRPA0016-PZVP-GUELT-AB   PIC  X(010).
             10 PO-DRPA0016-PZVP-GUELT-BIS  PIC  X(010).
             10 PO-DRPA0016-PZVP-PROGRAMM   PIC  X(008).
             10 PO-DRPA0016-PZVP-FUNKTION   PIC  X(008).
             10 PO-DRPA0016-PZVP-IO-DATEN   PIC  X(008).
             10 PO-DRPA0016-PZVP-VERSION    PIC S9(004)  BINARY.
      *
      ***** Ende-Copy-Member *****************************************
