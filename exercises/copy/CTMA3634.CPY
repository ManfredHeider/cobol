      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3634.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTMA3634.CPv  $
      *
      *   Rev 2.3   Oct 20 2017 08:01:28   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - DCL für Tabellen
      *
      *   Rev 2.2   Jul 28 2015 15:45:48   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.1   Jun 28 2013 11:22:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!01.06.15!2.2  ! Korrektur PRODART_MAX:    ! D.WILHELM
      *         !        !     ! CHAR(1) statt (80)        !
      *         !        !     ! neue Attribute: VERSION,  !
      *         !        !     ! VG, etc.                  !
      * AMA00360!11.10.17!2.3  ! PRODART_MIN und PRODART_MAX D.WILHELM
      *         !        !     ! von CHAR(1) nach CHAR(2)  !
      *         !        !     ! geändert                  !
      *         !        !     !                           !
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OVPRODUKT00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,PG                   SMALLINT             NOT NULL
CCC004*     ,PRODSCHL             SMALLINT             NOT NULL
CCC004*     ,GUELTIG_AB           DATE                 NOT NULL
CCC004*     ,GUELTIG_BIS          DATE                 NOT NULL
CCC004*     ,PROD_KURZ            CHAR(10)             NOT NULL
CCC004*     ,PROD_LANG            CHAR(40)             NOT NULL
CCC004*     ,PRODSPARTE           SMALLINT             NOT NULL
CCC004*     ,KNR_ART_PROV_TYP     SMALLINT             NOT NULL
CCC004*     ,IND_JW               SMALLINT             NOT NULL
CCC004*     ,PRODART_MIN          CHAR(2)              NOT NULL
CCC004*     ,PRODART_MAX          CHAR(2)              NOT NULL
CCC004*     ,TARIFBEZ             CHAR(8)              NOT NULL
CCC004*     ,UST_SATZ             DECIMAL(5, 2)        NOT NULL
CCC004*     ,SL_KZN               SMALLINT             NOT NULL
CCC004*     ,ZUSATZTAR-KZN        CHAR(1)              NOT NULL
CCC004*     ,ZUSATZTAR-BEZEICH    CHAR(16)             NOT NULL
CCC004*     ,ERWERB-FOLGE-KZN     CHAR(1)              NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      *
        01 OVPRODUKT00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 PG                                PIC S9(4) COMP.
         10 PRODSCHL                          PIC S9(4) COMP.
         10 GUELTIG-AB                        PIC X(10).
         10 GUELTIG-BIS                       PIC X(10).
         10 PROD-KURZ                         PIC X(10).
         10 PROD-LANG                         PIC X(40).
         10 PRODSPARTE                        PIC S9(4) COMP.
         10 KNR-ART-PROV-TYP                  PIC S9(4) COMP.
         10 IND-JW                            PIC S9(4) COMP.
         10 PRODART-MIN                       PIC X(2).
         10 PRODART-MAX                       PIC X(2).
         10 TARIFBEZ                          PIC X(8).
         10 UST-SATZ                          PIC S9(3)V9(2) COMP-3.
         10 SL-KZN                            PIC S9(4) COMP.
         10 ZUSATZTAR-KZN                     PIC X(1).
         10 ZUSATZTAR-BEZEICH                 PIC X(16).
         10 ERWERB-FOLGE-KZN                  PIC X(1).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
 

