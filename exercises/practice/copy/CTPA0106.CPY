      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0106.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/CTPA0106.CPv  $
      *
      *   Rev 2.7   Mar 28 2017 08:21:54   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs 
      *
      *   Rev 2.6   Jul 16 2015 15:10:34   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.5   Mar 10 2014 16:52:30   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.4   Jul 26 2013 11:44:02   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Jun 19 2012 09:32:28   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:57:26   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 14:32:00   t13ex21
      *APA00622: Einführung Allfinanz (MST)
      *
      *   Rev 2.0   Feb 27 1999 14:26:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:59:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Aug 1997 13:11:54   P13SP99
      *Initial revision.
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !27.02.99!2.0 ! NEUERSTELLUNG             !          *
      * APA00622!10.12.07!2.1 ! EINFÜHRUNG ALLFINANZ      !          *
      * APA01101!21.05.12!2.2 ! RESTRUKTURIERUNG          ! STORCH   *
260713* APA01539!26.07.13!2.4 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02353!17.02.17!2.8 ! Teleson                   ! Buch     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0106)                                         *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0106 TABLE
CCC004*    ( PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_VERSION                   SMALLINT NOT NULL,
CCC004*      PVER_F_GUELTIG_AB              DATE NOT NULL,
CCC004*      PVER_F_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PVER_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PVER_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PVER_V_KENNUNG                 CHAR(8) NOT NULL,
CCC004*      KNR_PVER_ART                   SMALLINT NOT NULL,
CCC004*      PVER_VORVERTRAG                CHAR(1) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PVER_VNR                       CHAR(15) NOT NULL,
CCC004*      PVER_KUNDE_NAME                CHAR(25) NOT NULL,
CCC004*      PVER_KUNDE_VNAME               CHAR(20) NOT NULL,
CCC004*      PVER_VERTR_BEGINN              DATE NOT NULL,
CCC004*      PVER_VERTR_ABLAUF              DATE NOT NULL,
CCC004*      PVER_PG_VBNR                   INTEGER NOT NULL,
CCC004*      KNR_PVER_STAND                 SMALLINT NOT NULL,
CCC004*      PVER_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PVER_AKT_PIN                   INTEGER NOT NULL,
CCC004*      PVER_AVERM                     INTEGER NOT NULL,
CCC004*      PVER_VM                        INTEGER NOT NULL,
CCC004*      PVER_PNAP_EINTRAG              CHAR(1) NOT NULL,
CCC004*      PVER_AKT_DGV                   SMALLINT NOT NULL,
CCC004*      PVER_AKT_PMBE_ID               DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_AKT_FAELLIG_V             DATE NOT NULL,
CCC004*      PVER_AKT_FAELLIG_B             DATE NOT NULL,
CCC004*      PVER_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PVER_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PVER_BZ_NAME                   CHAR(25) NOT NULL,
CCC004*      PVER_BZ_VNAME                  CHAR(20) NOT NULL,
CCC004*      PVER_VP_NAME                   CHAR(25) NOT NULL,
CCC004*      PVER_VP_VNAME                  CHAR(20) NOT NULL,
CCC004*      KNR_FEIV_KZ                    SMALLINT NOT NULL,
CCC004*      KNR_VERM_GES                   SMALLINT NOT NULL,
CCC004*      PVER_UMW_NR                    DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_URSPR_POL_DAT             DATE NOT NULL,
CCC004*      PVER_VN_STRASSE                CHAR(35) NOT NULL,
CCC004*      PVER_VN_PLZ                    CHAR(5) NOT NULL,
CCC004*      PVER_VN_ORT                    CHAR(35) NOT NULL,
CCC004*      PVER_PG_UST_SATZ               DECIMAL(6, 3) NOT NULL,
CCC004*      KNR_TLSN_PARTNER               SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0106                           *
      ******************************************************************
       01  TBPA0106-GESAMT.
         05  DCL-TBPA0106.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PVER-F-GUELTIG-AB    PIC X(10).
           10 PVER-F-GUELTIG-BIS   PIC X(10).
           10 PVER-V-GUELTIG-AB    PIC X(26).
           10 PVER-V-GUELTIG-BIS   PIC X(10).
           10 PVER-V-KENNUNG       PIC X(8).
           10 KNR-PVER-ART         PIC S9(4) USAGE COMP.
           10 PVER-VORVERTRAG      PIC X(1).
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PVER-VNR             PIC X(15).
           10 PVER-KUNDE-NAME      PIC X(25).
           10 PVER-KUNDE-VNAME     PIC X(20).
           10 PVER-VERTR-BEGINN    PIC X(10).
           10 PVER-VERTR-ABLAUF    PIC X(10).
           10 PVER-PG-VBNR         PIC S9(9) USAGE COMP.
           10 KNR-PVER-STAND       PIC S9(4) USAGE COMP.
           10 PVER-VSTAT           PIC X(4).
           10 PVER-AKT-PIN         PIC S9(9) USAGE COMP.
           10 PVER-AVERM           PIC S9(9) USAGE COMP.
           10 PVER-VM              PIC S9(9) USAGE COMP.
           10 PVER-PNAP-EINTRAG    PIC X(1).
           10 PVER-AKT-DGV         PIC S9(4) USAGE COMP.
           10 PVER-AKT-PMBE-ID     PIC S9(15)V USAGE COMP-3.
           10 PVER-AKT-FAELLIG-V   PIC X(10).
           10 PVER-AKT-FAELLIG-B   PIC X(10).
           10 PVER-LAST-UPDATE     PIC X(26).
           10 PVER-KENNUNG         PIC X(8).
           10 KNR-MANDANT          PIC S9(4) USAGE COMP.
           10 PVER-BZ-NAME         PIC X(25).
           10 PVER-BZ-VNAME        PIC X(20).
           10 PVER-VP-NAME         PIC X(25).
           10 PVER-VP-VNAME        PIC X(20).
           10 KNR-FEIV-KZ          PIC S9(4) USAGE COMP.
           10 KNR-VERM-GES         PIC S9(4) USAGE COMP.
           10 PVER-UMW-NR          PIC S9(15)V USAGE COMP-3.
           10 PVER-URSPR-POL-DAT   PIC X(10).
           10 PVER-VN-STRASSE      PIC X(35).
           10 PVER-VN-PLZ          PIC X(5).
           10 PVER-VN-ORT          PIC X(35).
           10 PVER-PG-UST-SATZ     PIC S9(3)V9(3) USAGE COMP-3.
           10 KNR-TLSN-PARTNER     PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 40      *
      ******************************************************************
 

