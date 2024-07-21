      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0005.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/CTAN0005.CPv  $
      *
      *   Rev 2.14   Dec 18 2015 08:58:54   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.13   Oct 21 2015 14:40:46   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.12   May 28 2015 09:55:34   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.11   Apr 23 2015 13:25:58   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Oct 24 2013 11:02:18   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.9   23 Nov 2007 11:02:24   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.8   01 Sep 2006 12:02:44   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.7   01 Sep 2006 09:49:56   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.6   31 Aug 2006 09:44:16   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   Feb 01 2005 15:34:22   t13ae34
      *AAN00544: DB2+Batch: PoA Optimierung Advocard
      *
      *   Rev 2.4   Dec 30 2003 08:57:16   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.3   Dec 12 2003 12:05:34   t13ae34
      *AAN00422 - Vergrößern Blackbox + Feld Version
      *
      *   Rev 2.2   Oct 31 2003 10:53:38   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:03:20   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Feb 27 1999 11:05:12   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   09 Sep 1997 09:13:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0005                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0005 ADVC-ADVOCARD    *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1  !NEU                        !RAHNER   *
      * AAN00422!31.10.03!2.2  !VERGRÖßERN BLACKBOX        !RAHNER   *
      * AAN00422!08.12.03!2.3  !MIT FELD VERSION           !HEIDER   *
      * AAN00422!30.12.03!2.4  !AKTIVIEREN ÄNDERUNGEN VOM  !         *
      *         !        !     !31.10.2003                 !RAHNER   *
      * AAN00544!19.01.05!2.5  !OPTIMIERUNG QUOTE          !HEIDER   *
      * AAN00684!31.08.06!2.6  !KUNDENBESTANDS-ID FÜR AVS  !RAHNER   *
      * AAN00730!30.11.07!2.7  !ALLFINANZ                  !RAHNER   *
      * AAN01073!24.10.13!2.8  !SEPA DEUTSCHLAND           !RAHNER   *
      * AAN01211!09.03.15!2.11 !ESIGN                      !HEIDER   *
      * AAN01223!08.12.15!2.13 !BEGINN/ABLAUF ÄNDERBAR     !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0005 TABLE
 
CCC004*    ( ADVC_HVANTNUM                  CHAR(9) NOT NULL,
CCC004*      ADVC_PIN                       INTEGER NOT NULL,
CCC004*      ADVC_VDABVERM                  INTEGER NOT NULL,
CCC004*      ADVC_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      ADVC_KENNUNG_AEND              CHAR(8) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      ADVC_VDABVERMPZ                CHAR(1) NOT NULL,
CCC004*      ADVC_VDVMNR                    INTEGER NOT NULL,
CCC004*      ADVC_VDVMNRPZ                  CHAR(1) NOT NULL,
CCC004*      ADVC_VDVMZUNA                  CHAR(26) NOT NULL,
CCC004*      ADVC_VDVMVORNA                 CHAR(26) NOT NULL,
CCC004*      ADVC_VDDIREKT                  SMALLINT NOT NULL,
CCC004*      ADVC_PDVNZUNA                  CHAR(26) NOT NULL,
CCC004*      ADVC_PDVNVORNA                 CHAR(26) NOT NULL,
CCC004*      ADVC_PDVNPLZ                   CHAR(6) NOT NULL,
CCC004*      ADVC_PDVNORT                   CHAR(30) NOT NULL,
CCC004*      ADVC_PDVNLKZ                   CHAR(3) NOT NULL,
CCC004*      ADVC_VBEGINN                   DATE NOT NULL,
CCC004*      ADVC_VABLAUF                   DATE NOT NULL,
CCC004*      ADVC_KNR_ZAHLW                 SMALLINT NOT NULL,
CCC004*      ADVC_KTY_ZAHLW                 SMALLINT NOT NULL,
CCC004*      ADVC_VDPRODKEY                 SMALLINT NOT NULL,
CCC004*      ADVC_VDABLEIN                  DECIMAL(7, 1) NOT NULL,
CCC004*      ADVC_BEITRAG_BR                DECIMAL(11, 2) NOT NULL,
CCC004*      ADVC_BEITRAG_NE                DECIMAL(11, 2) NOT NULL,
CCC004*      ADVC_HAND                      CHAR(1) NOT NULL,
CCC004*      ADVC_ANTRAG_DATUM              DATE NOT NULL,
CCC004*      ADVC_ERSTELLUNG                TIMESTAMP NOT NULL,
CCC004*      ADVC_KENNUNG_ERST              CHAR(8) NOT NULL,
CCC004*      ADVC_VERSION                   SMALLINT NOT NULL,
CCC004*      ADCV_KONTONR                   DECIMAL(12, 0) NOT NULL,
CCC004*      ADCV_KONTONR_E                 DECIMAL(12, 0) NOT NULL,
CCC004*      ADVC_BLZ                       INTEGER NOT NULL,
CCC004*      ADVC_BLZ_E                     INTEGER NOT NULL,
CCC004*      ADVC_AMTLKZ                    CHAR(12) NOT NULL,
CCC004*      ADVC_AMTLKZ_E                  CHAR(12) NOT NULL,
CCC004*      ADVC_KNR_ZAHLW_E               SMALLINT NOT NULL,
CCC004*      ADVC_KTY_ZAHLW_E               SMALLINT NOT NULL,
CCC004*      ADVC_V_PLUS                    CHAR(1) NOT NULL,
CCC004*      ADVC_V_PLUS_E                  CHAR(1) NOT NULL,
CCC004*      ADVC_XGDV                      VARCHAR(20000) NOT NULL,
CCC004*      ADVC_KD_BEST_ID                DECIMAL(18, 0) NOT NULL,
CCC004*      ADVC_VB_GES                    SMALLINT NOT NULL,
CCC004*      ADVC_IBAN                      CHAR(34) NOT NULL,
CCC004*      ADVC_IBAN_E                    CHAR(34) NOT NULL,
CCC004*      ADVC_BIC                       CHAR(11) NOT NULL,
CCC004*      ADVC_BIC_E                     CHAR(11) NOT NULL,
CCC004*      ADVC_ESIGN                     CHAR(1) NOT NULL,
CCC004*      ADVC_VBEGINN_E                 DATE NOT NULL,
CCC004*      ADVC_VABLAUF_E                 DATE NOT NULL
CCC004*    )
CCC003*    END-EXEC.
 
 
       01  DCLTBAN0005.
 
           10 ADVC-HVANTNUM        PIC X(9).
           10 ADVC-PIN             PIC S9(9) USAGE COMP.
           10 ADVC-VDABVERM        PIC S9(9) USAGE COMP.
           10 ADVC-LAST-UPDATE     PIC X(26).
           10 ADVC-KENNUNG-AEND    PIC X(8).
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 ADVC-VDABVERMPZ      PIC X(1).
           10 ADVC-VDVMNR          PIC S9(9) USAGE COMP.
           10 ADVC-VDVMNRPZ        PIC X(1).
           10 ADVC-VDVMZUNA        PIC X(26).
           10 ADVC-VDVMVORNA       PIC X(26).
           10 ADVC-VDDIREKT        PIC S9(4) USAGE COMP.
           10 ADVC-PDVNZUNA        PIC X(26).
           10 ADVC-PDVNVORNA       PIC X(26).
           10 ADVC-PDVNPLZ         PIC X(6).
           10 ADVC-PDVNORT         PIC X(30).
           10 ADVC-PDVNLKZ         PIC X(3).
           10 ADVC-VBEGINN         PIC X(10).
           10 ADVC-VABLAUF         PIC X(10).
           10 ADVC-KNR-ZAHLW       PIC S9(4) USAGE COMP.
           10 ADVC-KTY-ZAHLW       PIC S9(4) USAGE COMP.
           10 ADVC-VDPRODKEY       PIC S9(4) USAGE COMP.
           10 ADVC-VDABLEIN        PIC S9(6)V9(1) USAGE COMP-3.
           10 ADVC-BEITRAG-BR      PIC S9(9)V9(2) USAGE COMP-3.
           10 ADVC-BEITRAG-NE      PIC S9(9)V9(2) USAGE COMP-3.
           10 ADVC-HAND            PIC X(1).
           10 ADVC-ANTRAG-DATUM    PIC X(10).
           10 ADVC-ERSTELLUNG      PIC X(26).
           10 ADVC-KENNUNG-ERST    PIC X(8).
           10 ADVC-VERSION         PIC S9(4) USAGE COMP.
           10 ADCV-KONTONR         PIC S9(12)V USAGE COMP-3.
           10 ADCV-KONTONR-E       PIC S9(12)V USAGE COMP-3.
           10 ADVC-BLZ             PIC S9(9) USAGE COMP.
           10 ADVC-BLZ-E           PIC S9(9) USAGE COMP.
           10 ADVC-AMTLKZ          PIC X(12).
           10 ADVC-AMTLKZ-E        PIC X(12).
           10 ADVC-KNR-ZAHLW-E     PIC S9(4) USAGE COMP.
           10 ADVC-KTY-ZAHLW-E     PIC S9(4) USAGE COMP.
           10 ADVC-V-PLUS          PIC X(1).
           10 ADVC-V-PLUS-E        PIC X(1).
           10 ADVC-XGDV.
              49 ADVC-XGDV-LEN     PIC S9(4) USAGE COMP.
              49 ADVC-XGDV-TEXT    PIC X(20000).
           10 ADVC-KD-BEST-ID      PIC S9(18)V USAGE COMP-3.
           10 ADVC-VB-GES          PIC S9(4) USAGE COMP.
           10 ADVC-IBAN            PIC X(34).
           10 ADVC-IBAN-E          PIC X(34).
           10 ADVC-BIC             PIC X(11).
           10 ADVC-BIC-E           PIC X(11).
           10 ADVC-ESIGN           PIC X(1).
           10 ADVC-VBEGINN-E       PIC X(10).
           10 ADVC-VABLAUF-E       PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 50      *
      ******************************************************************


