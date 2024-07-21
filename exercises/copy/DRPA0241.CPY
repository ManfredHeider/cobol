      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0241.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0241.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Jun 1997 08:06:08   T13AE31
      *AFPA0017 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:13:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0241  FUNKTION LIST-OZP FÜR TABELLE PEWP *
      *                 FUNKTION: PRODUKTSCHLÜSSEL EINER PEWA-ID     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: CECCONI                                         *
      *    DATUM:    17.03.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0017!17.03.97! 01 ! NEUERSTELLUNG             ! A.CECCONI*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0241                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0241.
          05 PI-DRPA0241-PROGRAMM           PIC  X(008).
          05 PI-DRPA0241-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0241-AUSWAHL-KRITERIEN.
             10 PI-DRPA0241-PEWA-ID         PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0241-PGEB-GES        PIC S9(004) BINARY.
      *
       01 PO-DRPA0241.
          05 PO-DRPA0241-RC                 PIC  X(003).
             88 PO-DRPA0241-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0241-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0241-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0241-DATEN.
             10 PO-DRPA0241-PROD-PROD-SCHL   PIC S9(04) BINARY.
          05 PO-DRPA0241-PEWP-LAST-UPDATE    PIC  X(26).
      *
      ***** Ende-Copy-Member *****************************************

