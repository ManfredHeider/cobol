      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0015.CPv  $
      *
      *   Rev 2.1   Apr 14 2000 11:16:52   T13EX23
      *APA00067 - Neue 88-Stufe definieren
      *
      *
      *   Rev 2.0   Feb 27 1999 14:27:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:36   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0015  FUNKTION ZVP Funktion ermitteln    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    02.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !02.09.96! 01 ! NEUERSTELLUNG             ! WOLFF    *
      * APA00067!13.04.00! 2.1! NEUE 88-STUFE HINZUFÜGEN  ! GRAY     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0015                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0015.
          05 PI-DRPA0015-PZVP-VERFAHREN     PIC  X(020).
          05 PI-DRPA0015-STICHTAG           PIC  X(010).
          05 PI-DRPA0015-PGEB-GES           PIC S9(004) BINARY.
          05 PI-DRPA0015-PROD-PROD-SCHL     PIC S9(004) BINARY.
      *
       01 PO-DRPA0015.
          05 PO-DRPA0015-RC                 PIC  X(003).
             88 PO-DRPA0015-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0015-RC-OK-VERF-UNDEF            VALUE 'OKU'.
             88 PO-DRPA0015-RC-NOT-FOUND                VALUE 'NFD'.
          05 PO-DRPA0015-DATEN.
             10 PO-DRPA0015-PZVP-PROGRAMM   PIC  X(008).
             10 PO-DRPA0015-PZVP-FUNKTION   PIC  X(008).
             10 PO-DRPA0015-PZVP-IO-DATEN   PIC  X(008).
             10 PO-DRPA0015-PZVP-VERSION    PIC S9(004) BINARY.
             10 PO-DRPA0015-GUELT-AB-JMT    PIC S9(008) BINARY.
             10 PO-DRPA0015-GUELT-BIS-JMT   PIC S9(008) BINARY.
      *
      ******** Ende Copy-Member **************************************
