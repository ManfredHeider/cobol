      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0032.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0032.CPv  $
      *
      *   Rev 2.2   Feb 01 2005 16:12:08   t13ex49
      *FKI00908: Fehlerverfahren
      *
      *   Rev 2.1   02 May 2001 09:26:20   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Mar 18 1999 10:44:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!19.03.01!    ! Neuerstellung             !A.Blaess  *
010205* FKI00908!01.02.05!    ! Korrektur                 !P.Pohl    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *****************************************************************

CCC005*    EXEC SQL DECLARE TBAT0032 TABLE
CCC004*    (FTXT_SYSCODE            CHAR (2)  NOT NULL,
CCC004*     FTXT_FEHLERNUMMER       SMALLINT  NOT NULL,
CCC004*     FTXT_TEXT               CHAR (60) NOT NULL,
CCC004*     LETZTEAENDERUNG         TIMESTAMP NOT NULL WITH DEFAULT,
CCC004*     FTXT_USERID             CHAR (7)  NOT NULL
CCC003*    ) END-EXEC.

       01  TBAT0032-GESAMT.
           05  DCL-TBAT0032.
              10 FTXT-SYSCODE                   PIC X(2).
              10 FTXT-FEHLERNUMMER              PIC S9(4) COMP.
              10 FTXT-TEXT                      PIC X(60).
              10 LETZTEAENDERUNG                PIC X(26).
              10 FTXT-USERID                    PIC X(7).

