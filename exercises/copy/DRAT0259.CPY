      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0259.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0259.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   11 Feb 1999 14:15:50   T13AE34
      *FAN00201  ANTRAG-EURO
      *
      *   Rev 1.2   07 Aug 1998 11:28:22   T13AE34
      *AAN00167 - FAT00002 Euro-Modul mit Y2K
      *
      *   Rev 1.1   06 Jul 1998 09:27:38   T13AE34
      *AAT00002 - Euro
      *
      *   Rev 1.0   01 Apr 1998 08:37:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0259                                     *
      *    FUNKTION   : EURO - PRüFUNG FACHLICHES DATUM IN VERBINDUNG*
      *                 MIT DEM WäRHRUNGSKENNZEICHEN                 *
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
      * FAN00201!01.02.99!1.3 !WäHRUNGSTYP-EINGABE        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0259                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0259.
          05 PI-DRAT0259-DATEN.
             10 PI-DRAT0259-WKZ                  PIC  X(001).
             10 PI-DRAT0259-PGEB-GES             PIC S9(004)  BINARY.
      *         DATUM IN FORMAT EURO (TT.MM.JJJJ)
             10 PI-DRAT0259-DATUM-EURO           PIC  X(010).
      *         DATUM IN FORMAT JJMMTT
             10 PI-DRAT0259-DATUM-JJMMTT         PIC  X(006).
             10 PI-DRAT0259-PROD                 PIC S9(004)  BINARY.
      *
       01 PO-DRAT0259.
          05 PO-DRAT0259-RC                      PIC  X(003).
             88 PO-DRAT0259-RC-OK                     VALUE 'OK '.
             88 PO-DRAT0259-RC-NOK                    VALUE 'NOK'.
             88 PO-DRAT0259-RC-DAT                    VALUE 'DAT'.
             88 PO-DRAT0259-RC-PRO                    VALUE 'PRO'.
          05 PO-DRAT0259-DATEN.
             10 PO-DRAT0259-WKZ-TEXT             PIC  X(003).
010299       10 PO-DRAT0259-WT-EINGABE           PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
