      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3577.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTMA3577.CPv  $
      *
      *   Rev 2.3   Jun 08 2020 14:16:48   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - DCL für T
      *
      *
      *   Rev 2.2   Dec 09 2015 08:37:42   t13ac20
      *AMA00024: SEPA-Nacharbeiten - Copies/Programme Datenmodell
      *
      *   Rev 2.1   Jun 28 2013 11:18:58   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00476!05.05.20!2.3  ! Erweiterung BKTO_INH und  ! D.WILHELM
      *         !        !     ! KREDITINST auf 40 Stellen !
      *         !        !     !                           !
      *         !        !     !                           !
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3577
CCC005*    EXEC SQL DECLARE OVBANKVERBINDUNG00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VBNR                 INTEGER              NOT NULL
CCC004*     ,HIST_KZN             CHAR(1)              NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,BKTO_INH             CHAR(40)             NOT NULL
CCC004*     ,KREDITINST           CHAR(40)             NOT NULL
CCC004*     ,IBAN                 CHAR(34)             NOT NULL
CCC004*     ,BIC                  CHAR(11)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3577
        01 OVBANKVERBINDUNG00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VBNR                              PIC S9(9) COMP.
         10 HIST-KZN                          PIC X(1).
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 BKTO-INH                          PIC X(40).
         10 KREDITINST                        PIC X(40).
         10 IBAN                              PIC X(34).
         10 BIC                               PIC X(11).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

