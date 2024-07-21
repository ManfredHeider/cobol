      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0083.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRAN0083.CPv  $
      *
      *   Rev 2.13   Dec 18 2015 08:58:08   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.12   Oct 21 2015 14:40:08   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.11   May 28 2015 09:52:20   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Apr 23 2015 13:24:22   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.9   Oct 24 2013 11:01:24   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.8   23 Nov 2007 11:02:06   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.7   01 Sep 2006 09:47:56   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.6   31 Aug 2006 09:43:26   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   Feb 01 2005 15:24:26   t13ae34
      *AAN00544: DB2+Batch: PoA Optimierung Advocard
      *
      *   Rev 2.4   Dec 30 2003 08:55:32   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.3   Dec 12 2003 12:05:22   t13ae34
      *AAN00422 - Vergrößern Blackbox + Feld Version
      *
      *   Rev 2.2   Oct 31 2003 10:52:44   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:06:06   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:16:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      *    ZUM ADVOCARD-HOST
      ****************************************************************
      *    COPY-MEMBER: DRAN0083                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1 !NEU                        ! RAHNER   *
      * AAN00422!31.10.03!2.2 !VERGRÖßERN BLACKBOX        !          *
      * AAN00422!09.12.03!2.3 !FELD VERSION               ! HEIDER   *
      * AAN00422!30.12.03!2.3 !AKTIVIEREN ÄNDERUNGEN VOM  !          *
      *         !        !    !31.10.2003                 ! RAHNER   *
      * AAN00544!19.01.05!2.5 !OPTIMIERUNG QUOTE          ! HEIDER   *
      * AAN00684!31.08.06!2.6 !KUNDENBESTANDS-ID FÜR AVS  ! RAHNER   *
      * AAN00730!30.11.07!2.7 !ALLFINANZ                  ! RAHNER   *
      * AAN01073!24.10.13!2.9 !SEPA                       ! RAHNER   *
      * AAN01211!09.03.15!2.10!ESIGN                      ! HEIDER   *
      * AAN01223!08.12.15!2.13!BEGINN/ABLAUF ÄNDERBAR     ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  ADVC-PC-DATEN.
 
      *        GLEICHHEITSPRÜFUNG
                10 ADVC-PC-PGEB-GES          PIC S9(4) COMP.
                10 ADVC-PC-PIN               PIC S9(9) COMP.
                10 ADVC-PC-HVANTNUM          PIC X(9).
                10 ADVC-PC-VDABVERM          PIC S9(9) COMP.
                10 ADVC-PC-VDABVERMPZ        PIC X.
                10 ADVC-PC-VDVMNR            PIC S9(9) COMP.
                10 ADVC-PC-VDVMNRPZ          PIC X.
                10 ADVC-PC-VDVMZUNA          PIC X(26).
                10 ADVC-PC-VDVMVORNA         PIC X(26).
                10 ADVC-PC-VDDIREKT          PIC S9(4) COMP.
                10 ADVC-PC-PDVNZUNA          PIC X(26).
                10 ADVC-PC-PDVNVORNA         PIC X(26).
                10 ADVC-PC-PDVNPLZ           PIC X(6).
                10 ADVC-PC-PDVNORT           PIC X(30).
                10 ADVC-PC-PDVNLKZ           PIC X(3).
                10 ADVC-PC-BEGIN             PIC X(10).
                10 ADVC-PC-ABLAUF            PIC X(10).
                10 ADVC-PC-PRODKEY           PIC S9(4) COMP.
                10 ADVC-PC-KNR-ZAHLW         PIC S9(4) COMP.
                10 ADVC-PC-KTY-ZAHLW         PIC S9(4) COMP.
                10 ADVC-PC-VDABLEIN          PIC S9(6)V9(1) COMP-3.
                10 ADVC-PC-BEITRAG-BRUTTO    PIC S9(5)V99 COMP-3.
                10 ADVC-PC-BEITRAG-NETTO     PIC S9(5)V99 COMP-3.
                10 ADVC-PC-HAND              PIC X.
                10 ADVC-PC-ANTRAGS-DATUM     PIC X(10).
                10 ADVC-PC-ERSTELLUNG        PIC X(26).
                10 ADVC-PC-KENNUNG-ERST      PIC X(8).
                10 ADVC-PC-KENNUNG-AEND      PIC X(8).
                10 ADVC-PC-LAST-UPDATE       PIC X(26).
                10 ADVC-PC-VERSION           PIC S9(4) COMP.
      *>>>>>
                10 ADVC-PC-KONTONR           PIC S9(12)V COMP-3.
                10 ADVC-PC-KONTONR-E         PIC S9(12)V COMP-3.
                10 ADVC-PC-BLZ               PIC S9(9) COMP.
                10 ADVC-PC-BLZ-E             PIC S9(9) COMP.
                10 ADVC-PC-AMTLKZ            PIC X(12).
                10 ADVC-PC-AMTLKZ-E          PIC X(12).
                10 ADVC-PC-KNR-ZAHLW-E       PIC S9(4) COMP.
                10 ADVC-PC-KTY-ZAHLW-E       PIC S9(4) COMP.
      *         10 ADVC-PC-V-PLUS            PIC X.
      *         10 ADVC-PC-V-PLUS-E          PIC X.
      *<<<<<
                10 ADVC-PC-XGDV.
                   15 ADVC-PC-XGDV-LEN       PIC S9(5) COMP.
      *            15 ADVC-PC-XGDV-TEXT      PIC X(9999).
      *            15 ADVC-PC-XGDV-TEXT      PIC X(99999).
                   15 ADVC-PC-XGDV-TEXT      PIC X(20000).
                10 ADVC-PC-KD-BEST-ID        PIC S9(18)V COMP-3.
                10 ADVC-PC-VB-GES            PIC S9(4) COMP.
                10 ADVC-PC-IBAN              PIC X(34).
                10 ADVC-PC-IBAN-E            PIC X(34).
                10 ADVC-PC-BIC               PIC X(11).
                10 ADVC-PC-BIC-E             PIC X(11).
                10 ADVC-PC-ESIGN             PIC X(1).
                10 ADVC-PC-BEGIN-E           PIC X(10).
                10 ADVC-PC-ABLAUF-E          PIC X(10).
      *
      ****** ENDE COPY-MEMBER ****************************************


