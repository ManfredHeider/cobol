      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0260.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0260.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Jul 1998 09:28:04   T13AE34
      *AAT00002 - Euro
      *
      *   Rev 1.0   01 Apr 1998 08:38:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0260                                     *
      *    FUNKTION   : EURO - ERMITTELN BETRAG FüR PRäSENTATIONS-   *
      *                 SCHICHT AUS GESPEICHERTEM BETRAG             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00002!12.06.98! 01 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0260                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0260.
          05 PI-DRAT0260-DATEN.
             10 PI-DRAT0260-PGEB-GES             PIC S9(004)
                                                        BINARY.
             10 PI-DRAT0260-WT-SPEICHERUNG       PIC S9(004)
                                                        BINARY.
             10 PI-DRAT0260-WT-EINGABE           PIC S9(004)
                                                        BINARY.
             10 PI-DRAT0260-BETRAG               PIC S9(009)V9(002)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRAT0260.
          05 PO-DRAT0260-RC                      PIC  X(003).
             88 PO-DRAT0260-RC-OK                     VALUE 'OK '.
          05 PO-DRAT0260-DATEN.
             10 PO-DRAT0260-BETRAG               PIC S9(009)V9(002)
                                                        PACKED-DECIMAL.
             10 PO-DRAT0260-WKZ                  PIC  X(001).
             10 PO-DRAT0260-WKZ-TEXT             PIC  X(003).
      *
      ******** Ende Copy-Member **************************************
