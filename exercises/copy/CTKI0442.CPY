      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0442.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0442.CPv  $
      *
      *   Rev 2.1   Mar 11 2010 10:00:30   t13ex14
      *AKI00935: Std.-OZP für Tabelle LEBENPERSONEIGEN 
      *
      *   Rev 2.0   Feb 25 2010 10:45:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00935 !25.02.10!    !-Neu                       !W. Misgeld*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE LEBENPERSONEIGEN                                  *
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENPERSONEIGEN TABLE
CCC004*    ( LPEVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LPEPERSROLLEKZ                 CHAR(1) NOT NULL,
CCC004*      LPENACHNAME                    CHAR(30),
CCC004*      LPEVORNAME                     CHAR(30),
CCC004*      LPEGEBDATUM                    DATE,
CCC004*      LPEADRESSEID                   CHAR(13),
CCC004*      LPEANREDE_KTY                  SMALLINT NOT NULL,
CCC004*      LPEANREDE_KNR                  SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TCKI0442                    *
      ******************************************************************
       01  LEBENPERSONEIGEN-GESAMT.
        05  NI-LEBENPERSONEIGEN-TAB.
           10 NI-LEBENPERSONEIGEN-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05  NI-LEBENPERSONEIGEN  REDEFINES  NI-LEBENPERSONEIGEN-TAB.
           10 NI-LPEVERTRAGSID        PIC S9(4) COMP.
           10 NI-LPEPERSROLLEKZ       PIC S9(4) COMP.
           10 NI-LPENACHNAME          PIC S9(4) COMP.
           10 NI-LPEVORNAME           PIC S9(4) COMP.
           10 NI-LPEGEBDATUM          PIC S9(4) COMP.
           10 NI-LPEADRESSEID         PIC S9(4) COMP.
           10 NI-LPEANREDE-KTY        PIC S9(4) COMP.
           10 NI-LPEANREDE-KNR        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
        05  DCL-LEBENPERSONEIGEN.
           10 LPEVERTRAGSID        PIC X(13).
           10 LPEPERSROLLEKZ       PIC X(1).
           10 LPENACHNAME          PIC X(30).
           10 LPEVORNAME           PIC X(30).
           10 LPEGEBDATUM          PIC X(10).
           10 LPEADRESSEID         PIC X(13).
           10 LPEANREDE-KTY        PIC S9(4) USAGE COMP.
           10 LPEANREDE-KNR        PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
