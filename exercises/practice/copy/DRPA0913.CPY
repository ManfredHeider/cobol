      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0913.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0913.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:46:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   10 Mar 1998 15:35:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0913                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LAUFZEIT PRÜFEN     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.03.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0913                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0913.
          05 PI-DRPA0913-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0913-DATEN.
             10 PI-DRPA0913-PMPP-PGV                PIC S9(004) BINARY.
             10 PI-DRPA0913-PMPE-VERTR-BEGINN       PIC  X(010).
             10 PI-DRPA0913-PMPE-VERTR-ABLAUF       PIC  X(010).
      *
       01 PO-DRPA0913.
          05 PO-DRPA0913-RC                         PIC  X(003).
             88 PO-DRPA0913-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0913-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA0913-DATEN.
             10 PO-DRPA0913-LAUFZEIT                PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
