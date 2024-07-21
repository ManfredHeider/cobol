      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3596.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTMA3596.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 16:02:56   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 11:19:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00129!24.06.15! 2.2 ! Änderungen wg. Lucy       ! D.WILHELM
      * --------!--------!-----!---------------------------!----------
      *         !        !     !                           !
CCC005*    EXEC SQL DECLARE T13MAS.OVREF_SALD_BUCH00 TABLE
CCC004*    (VERSION               CHAR(1)              NOT NULL
CCC004*    ,VG                    SMALLINT             NOT NULL
CCC004*    ,FACH_KENNUNG          CHAR(10)             NOT NULL
CCC004*    ,DV_KENNUNG            CHAR(10)             NOT NULL
CCC004*    ,LFDNR                 DECIMAL(9, 0)        NOT NULL
CCC004*    ,BUCHTEXT              CHAR(92)             NOT NULL
CCC004*    ,BUCHTEXT_FIBU         CHAR(40)             NOT NULL
CCC004*    ,KTO_KZN_1             DECIMAL(1, 0)        NOT NULL
CCC004*    ,KTO_KZN_2             CHAR(1)              NOT NULL
CCC004*    ,FIBU_KONTO            DECIMAL(6, 0)        NOT NULL
CCC004*    ,AENDERUNGSSYSZEIT     TIMESTAMP            NOT NULL
CCC004*    ,AENDERUNGSKENNUNG     CHAR(8)              NOT NULL)
CCC003*   END-EXEC.
      *
       01  OVREF-SALD-BUCH00-SATZ.
        10 VERSION                             PIC X(1).
        10 VG                                  PIC S9(4) COMP.
        10 FACH-KENNUNG                        PIC X(10).
        10 DV-KENNUNG                          PIC X(10).
        10 LFDNR                               PIC S9(9) COMP-3.
        10 BUCHTEXT                            PIC X(92).
        10 BUCHTEXT-FIBU                       PIC X(40).
        10 KTO-KZN-1                           PIC S9(1) COMP-3.
        10 KTO-KZN-2                           PIC X(1).
        10 FIBU-KONTO                          PIC S9(6) COMP-3.
        10 AENDERUNGSSYSZEIT                   PIC X(26).
        10 AENDERUNGSKENNUNG                   PIC X(8).

