      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0206.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0206.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:51:46   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:09:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0206  FUNKTION LIST-OZP FÜR TABELLE PEWP *
      *                           FUNKTION : PEW-EXPORT              *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPAnnnn!30.01.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0206                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0206.
          05 PI-DRPA0206-PROGRAMM           PIC  X(008).
          05 PI-DRPA0206-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0206-AUSWAHL-KRITERIEN.
             10 PI-DRPA0206-PEWA-ID         PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0206-PGEB-GES        PIC S9(004) BINARY.
      *
       01 PO-DRPA0206.
          05 PO-DRPA0206-RC                 PIC  X(003).
             88 PO-DRPA0206-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0206-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0206-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0206-DATEN.
             10 PO-DRPA0206-PROD-PROD-SCHL   PIC S9(04) BINARY.
      *
      ***** Ende-Copy-Member *****************************************
