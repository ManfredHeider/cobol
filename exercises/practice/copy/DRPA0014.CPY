      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0014.CPv  $
      *
      *   Rev 2.1   Apr 17 2000 10:51:44   T13EX23
      *APA00067 - Neue 88-Stufe RC-OK-VERF-UNDEF
      *
      *   Rev 2.0   Feb 27 1999 14:27:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:32   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0014  FUNKTION ZVP Funktion ermitteln    *
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
      *    FÜR DIE FUNKTION DRPA0014                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0014.
          05 PI-DRPA0014-PZVP-VERFAHREN     PIC  X(020).
          05 PI-DRPA0014-STICHTAG           PIC  X(010).
          05 PI-DRPA0014-PGEB-GES           PIC S9(004) BINARY.
      *
       01 PO-DRPA0014.
          05 PO-DRPA0014-RC                 PIC  X(003).
             88 PO-DRPA0014-RC-OK                       VALUE 'OK '.
130400       88 PO-DRPA0014-RC-OK-VERF-UNDEF            VALUE 'OKU'.
             88 PO-DRPA0014-RC-NOT-FOUND                VALUE 'NFD'.
          05 PO-DRPA0014-DATEN.
             10 PO-DRPA0014-PZVP-PROGRAMM   PIC  X(008).
             10 PO-DRPA0014-PZVP-FUNKTION   PIC  X(008).
             10 PO-DRPA0014-PZVP-IO-DATEN   PIC  X(008).
             10 PO-DRPA0014-PZVP-VERSION    PIC S9(004) BINARY.
             10 PO-DRPA0014-GUELT-AB-JMT    PIC S9(008) BINARY.
             10 PO-DRPA0014-GUELT-BIS-JMT   PIC S9(008) BINARY.
      *
      ******** Ende Copy-Member **************************************
