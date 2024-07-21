      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0286.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0286.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Jun 1997 09:46:58   T13AE31
      *AFPA0017 - Neuerstellung
      *
      *   Rev 1.0   Apr 24 1997 14:30:56   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0206  FUNKTION LIST-OZP FÜR TABELLE PEWP *
      *                           FUNKTION LÖSCHEN DER EINTRÄGE      *
      *                           MIT STATUS ABGERECHNET             *
      *    --------------------------------------------------------  *
      *    ERSTELLT: CECCONI                                         *
      *    DATUM:    15.05.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPAnnnn!15.05.97! 01 ! NEUERSTELLUNG             ! CECCONI  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0286                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0286.
          05 PI-DRPA0286-PROGRAMM           PIC  X(008).
          05 PI-DRPA0286-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0286-AUSWAHL-KRITERIEN.
             10 PI-DRPA0286-PEWA-ID         PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0286-PGEB-GES        PIC S9(004) BINARY.
      *
       01 PO-DRPA0286.
          05 PO-DRPA0286-RC                 PIC  X(003).
             88 PO-DRPA0286-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0286-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0286-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0286-DATEN.
             10 PO-DRPA0286-PROD-PROD-SCHL   PIC S9(04) BINARY.
             10 PO-DRPA0286-PEWP-VSTAT       PIC  X(04).
             10 PO-DRPA0286-LAST-UPDATE      PIC  X(26).
      *
      ***** Ende-Copy-Member *****************************************
