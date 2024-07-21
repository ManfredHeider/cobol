      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0028.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTAT0028.CPv  $
      *
      *   Rev 2.3   Aug 07 2003 14:50:26   t13ex43
      *AAT00024 : Neuer Tabellenaufbau
      *
      *   Rev 2.2   Nov 29 1999 17:39:46   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.1   Nov 29 1999 11:01:42   T13AE34
      *AAN00236-PLZ
      *
      *
      *   Rev 2.0   Mar 18 1999 10:44:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0028                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0028 - PLZ  POSTLEITZAHLEN                  *
      *                                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    07.10.1999                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00236!07.10.99!2.1 ! NEU                       ! HEIDER   *
060803* AAT00024!06.08.03!2.3 ! KOMPLETT NEUER AUFBAU     ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0028 TABLE
060803**** ( PLZ_PLZ                       CHAR (5)        NOT NULL,
060803****   PLZ_ORT                       CHAR (35)       NOT NULL
CCC004*    ( PLZ_PLZ                       CHAR (09)       NOT NULL,
CCC004*      PLZ_ORT                       CHAR (40)       NOT NULL,
CCC004*      KTY_LAND                      SMALLINT        NOT NULL,
CCC004*      KNR_LAND                      SMALLINT        NOT NULL,
CCC004*      KTY_BUNDESLAND                SMALLINT        NOT NULL,
CCC004*      KNR_BUNDESLAND                SMALLINT        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0028                           *
      ******************************************************************
       01  TBAT0028-GESAMT.
           05  DCL-TBAT0028.
060803****     10 PLZ-PLZ                  PIC  X(5).
060803****     10 PLZ-ORT                  PIC  X(35).
060803         10 PLZ-PLZ                  PIC  X(09).
060803         10 PLZ-ORT                  PIC  X(40).
060803         10 KTY-LAND                 PIC S9(04) USAGE COMP.
060803         10 KNR-LAND                 PIC S9(04) USAGE COMP.
060803         10 KTY-BUNDESLAND           PIC S9(04) USAGE COMP.
060803         10 KNR-BUNDESLAND           PIC S9(04) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
