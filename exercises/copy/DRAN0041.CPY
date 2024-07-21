      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0041.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRAN0041.CPv  $
      *
      *   Rev 2.14   May 28 2015 09:51:34   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.13   Apr 23 2015 13:23:34   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.12   Oct 24 2013 10:59:34   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.11   Jun 19 2009 09:15:46   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.10   Mar 24 2009 10:55:54   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.9   Mar 11 2009 16:11:48   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.8   May 28 2008 08:49:48   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.7   23 Nov 2007 10:17:32   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.6   01 Sep 2006 09:43:22   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   31 Aug 2006 09:42:18   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.4   Nov 26 2004 15:29:12   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.3   Sep 30 2002 14:55:22   t13ae34
      *AAN00397-POA Antragsdatum
      *
      *   Rev 2.2   Jun 03 2002 18:13:00   t13ae34
      *AAN00357 - POA-AMV
      *
      *   Rev 2.1   Apr 23 2002 16:59:48   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0041  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00357!03.06.02!2.2 ! 10 STATT 3 VORVERTRÄGE    ! HEIDER   *
      * AAN00397!30.09.02!2.3 ! POA_ANTRAG_DATUM          ! HEIDER   *
      * AAN00529!26.11.04!2.4 ! OPTIMIERUNG POA-QUOTE     ! RAHNER   *
      * AAN00684!31.08.06!2.5 ! NEU: KUNDENBESTANDS-ID F. ! RAHNER   *
      *         !        !    ! AVS                       !          *
      * AAN00730!30.11.07!2.7 ! ALLFINANZ                 ! RAHNER   *
      * AAN00801!26.05.08!2.8 ! KENNZEICHEN WIDERSPRUCH   ! HEIDER   *
      * AAN00843!11.03.09!2.9 ! VERGRÖßERN XGDV-FELD POA  ! RAHNER   *
      * AAN00884!17.06.09!2.9 !- KZ FÜR TARIFÄNDERUNG     !          *
      *         !        !    !- FELDER FÜR DIREKT POA    ! RAHNER   *
      * AAN01073!24.10.13!2.10! SEPA                      ! RAHNER   *
      * AAN01211!04.03.15!2.13! ESIGN                     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0041                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE POAA TBAN0003              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0041.
           05 PI-DRAN0041-POAA-KEY.
               10 PI-DRAN0041-PGEB-GES           PIC S9(4) COMP.
               10 PI-DRAN0041-POA-HVANTNUM       PIC X(9).
               10 PI-DRAN0041-POA-HVANTNUM-LFD   PIC X(1).
           05 PI-DRAN0041-POAA-DATEN.
               10 PI-DRAN0041-POA-PIN            PIC S9(9) COMP.
               10 PI-DRAN0041-POA-VDABVERM       PIC S9(9) COMP.
               10 PI-DRAN0041-POA-VDABVERMPZ     PIC X.
               10 PI-DRAN0041-POA-VDVMNR         PIC S9(9) COMP.
               10 PI-DRAN0041-POA-VDVMNRPZ       PIC X.
               10 PI-DRAN0041-POA-VDVMZUNA       PIC X(26).
               10 PI-DRAN0041-POA-VDVMVORNA      PIC X(26).
               10 PI-DRAN0041-POA-VDDIREKT       PIC S9(4) COMP.
               10 PI-DRAN0041-POA-PDVNZUNA       PIC X(26).
               10 PI-DRAN0041-POA-PDVNVORNA      PIC X(26).
               10 PI-DRAN0041-POA-PDVNPLZ        PIC X(06).
               10 PI-DRAN0041-POA-PDVNORT        PIC X(30).
               10 PI-DRAN0041-POA-PDVNLKZ        PIC X(3).
               10 PI-DRAN0041-POA-VBEGINN        PIC X(10).
               10 PI-DRAN0041-POA-VABLAUF        PIC X(10).
               10 PI-DRAN0041-POA-KNR-ZAHLW      PIC S9(4) COMP.
               10 PI-DRAN0041-POA-KTY-ZAHLW      PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDPRODKEY1     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDABLEIN1      PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGBR1     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGNE1     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-VDPRODKEY2     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDABLEIN2      PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGBR2     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGNE2     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-VDPRODKEY3     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDABLEIN3      PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGBR3     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGNE3     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-VDPRODKEY4     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDABLEIN4      PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGBR4     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGNE4     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-VDPRODKEY5     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VDABLEIN5      PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGBR5     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-BEITRAGNE5     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0041-POA-KTY-ZWEIG      PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR1       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG1     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR2       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG2     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR3       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG3     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR4       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG4     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR5       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG5     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR6       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG6     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR7       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG7     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR8       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG8     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR9       PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG9     PIC S9(4) COMP.
               10 PI-DRAN0041-POA-VOR-VNR10      PIC X(14).
               10 PI-DRAN0041-POA-KNR-ZWEIG10    PIC S9(4) COMP.
               10 PI-DRAN0041-POA-HAND           PIC X.
110309*        10 PI-DRAN0041-POA-XGDV           PIC X(7793).
110309         10 PI-DRAN0041-POA-XGDV           PIC X(25000).
300902         10 PI-DRAN0041-POA-ANTRAG-DATUM   PIC X(10).
261104         10 PI-DRAN0041-POA-VBEGINN-E      PIC X(10).
261104         10 PI-DRAN0041-POA-VABLAUF-E      PIC X(10).
261104         10 PI-DRAN0041-POA-KNR-ZAHLW-E    PIC S9(4) COMP.
261104         10 PI-DRAN0041-POA-KTY-ZAHLW-E    PIC S9(4) COMP.
261104         10 PI-DRAN0041-POA-PDVNBLZ        PIC S9(8) COMP.
261104         10 PI-DRAN0041-POA-PDVNBLZ-E      PIC S9(8) COMP.
261104         10 PI-DRAN0041-POA-PDVNKONTONR    PIC S9(10) COMP.
261104         10 PI-DRAN0041-POA-PDVNKONTONR-E  PIC S9(10) COMP.
261104         10 PI-DRAN0041-POA-PDVNBEGINKM    PIC S9(6) COMP.
261104         10 PI-DRAN0041-POA-PDVNBEGINKM-E  PIC S9(6) COMP.
261104         10 PI-DRAN0041-POA-PDVNFGESTNR    PIC X(17).
261104         10 PI-DRAN0041-POA-PDVNFGESTNR-E  PIC X(17).
261104         10 PI-DRAN0041-POA-PDVNAMTLKZ     PIC X(11).
261104         10 PI-DRAN0041-POA-PDVNAMTLKZ-E   PIC X(11).
261104         10 PI-DRAN0041-POA-PDVNJAHRKM     PIC S9(6) COMP.
261104         10 PI-DRAN0041-POA-PDVNJAHRKM-E   PIC S9(6) COMP.
310806         10 PI-DRAN0041-POA-KD-BEST-ID     PIC S9(18)V COMP-3.
301107         10 PI-DRAN0041-POA-VB-GES         PIC S9(4) COMP.
260508         10 PI-DRAN0041-POA-WIDERSPR       PIC X(1).
260508         10 PI-DRAN0041-POA-WIDERSPR-E     PIC X(1).
170609         10 PI-DRAN0041-POA-PIN-PZ         PIC X(2).
170609         10 PI-DRAN0041-POA-BARCODE        PIC X(10).
170609         10 PI-DRAN0041-POA-SCHNELLANTR    PIC X(1).
170609         10 PI-DRAN0041-POA-GESCHAEFT-VORG PIC X(2).
241013         10 PI-DRAN0041-POA-IBAN           PIC X(34).
241013         10 PI-DRAN0041-POA-IBAN-E         PIC X(34).
241013         10 PI-DRAN0041-POA-BIC            PIC X(11).
241013         10 PI-DRAN0041-POA-BIC-E          PIC X(11).
040315         10 PI-DRAN0041-POA-ESIGN          PIC X(01).
      *
       01  PO-DRAN0041.
           05 PO-DRAN0041-RC                   PIC  X(003).
             88 PO-DRAN0041-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0041-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 

