      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0056.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRAN0056.CPv  $
      *
      *   Rev 2.14   May 28 2015 09:51:50   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.13   Apr 23 2015 13:23:50   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.12   Oct 24 2013 11:00:26   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.11   Jun 19 2009 09:16:12   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.10   Mar 24 2009 10:56:40   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.9   Mar 11 2009 16:12:30   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.8   May 28 2008 08:50:10   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.7   23 Nov 2007 10:17:50   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.6   01 Sep 2006 09:44:46   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   31 Aug 2006 09:42:36   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.4   Nov 26 2004 15:29:36   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.3   Sep 30 2002 14:56:14   t13ae34
      *AAN00397-POA Antragsdatum
      *
      *   Rev 2.2   Jun 03 2002 18:13:24   t13ae34
      *AAN00357 - POA-AMV
      *
      *   Rev 2.1   Apr 23 2002 17:00:04   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      *    ZUM AMV-HOST
      ****************************************************************
      *    COPY-MEMBER: DRAN0056                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1 !NEU                        !HEIDER    *
      * AAN00357!27.05.02!2.2 !10 STATT 3 VORVERTRAEGE    !HEIDER    *
      * AAN00397!30.09.02!2.3 !POA_ANTRAG_DATUM           !HEIDER    *
      * AAN00529!26.11.04!2.4 !OPTIMIERUNG POA-QUOTE      !RAHNER    *
      * AAN00684!31.08.06!2.5 !NEU: POA_KD_BEST_ID FÜR AVS!RAHNER    *
      * AAN00730!23.11.07!2.6 !ALLFINANZ NEU POA_VB_GES   !RAHNER    *
      * AAN00730!30.11.07!2.7 !ALLFINANZ                  !RAHNER    *
      * AAN00801!26.05.08!2.8 !KENNZEICHEN WIDERSPRUCH    !HEIDER    *
      * AAN00843!11.03.09!2.9 !VERGRÖßERN XGDV-FELD       !RAHNER    *
      * AAN00884!17.06.09!2.11!- KZ FÜR TARIFÄNDERUNG     !          *
      *         !        !    !- FELDER FÜR DIREKT POA    !RAHNER    *
      * AAN01073!        !2.12!SEPA                       !RAHNER    *
      * AAN01211!04.03.15!2.13!ESIGN                      !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  POA-PC-DATEN.
 
      *        GLEICHHEITSPRÜFUNG
                10 POA-PC-PGEB-GES     PIC S9(4) COMP.
                10 POA-PC-PIN          PIC S9(9) COMP.
                10 POA-PC-HVANTNUM     PIC X(9).
                10 POA-PC-HVANTNUM-LFD PIC X.
                10 POA-PC-VDABVERM     PIC S9(9) COMP.
                10 POA-PC-VDABVERMPZ   PIC X.
                10 POA-PC-VDVMNR       PIC S9(9) COMP.
                10 POA-PC-VDVMNRPZ     PIC X.
                10 POA-PC-VDVMZUNA     PIC X(26).
                10 POA-PC-VDVMVORNA    PIC X(26).
                10 POA-PC-VDDIREKT     PIC S9(4) COMP.
                10 POA-PC-PDVNZUNA     PIC X(26).
                10 POA-PC-PDVNVORNA    PIC X(26).
                10 POA-PC-PDVNPLZ      PIC X(6).
                10 POA-PC-PDVNORT      PIC X(30).
                10 POA-PC-PDVNLKZ      PIC X(3).
                10 POA-PC-VBEGINN      PIC X(10).
                10 POA-PC-VABLAUF      PIC X(10).
                10 POA-PC-KNR-ZAHLW    PIC S9(4) COMP.
                10 POA-PC-KTY-ZAHLW    PIC S9(4) COMP.
                10 POA-PC-VDPRODKEY1   PIC S9(4) COMP.
                10 POA-PC-VDABLEIN1    PIC S9(6)V9(1) COMP-3.
                10 POA-PC-BEITRAGBR1   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-BEITRAGNE1   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-VDPRODKEY2   PIC S9(4) COMP.
                10 POA-PC-VDABLEIN2    PIC S9(6)V9(1) COMP-3.
                10 POA-PC-BEITRAGBR2   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-BEITRAGNE2   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-VDPRODKEY3   PIC S9(4) COMP.
                10 POA-PC-VDABLEIN3    PIC S9(6)V9(1) COMP-3.
                10 POA-PC-BEITRAGBR3   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-BEITRAGNE3   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-VDPRODKEY4   PIC S9(4) COMP.
                10 POA-PC-VDABLEIN4    PIC S9(6)V9(1) COMP-3.
                10 POA-PC-BEITRAGBR4   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-BEITRAGNE4   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-VDPRODKEY5   PIC S9(4) COMP.
                10 POA-PC-VDABLEIN5    PIC S9(6)V9(1) COMP-3.
                10 POA-PC-BEITRAGBR5   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-BEITRAGNE5   PIC S9(9)V9(2) COMP-3.
                10 POA-PC-KTY-ZWEIG    PIC S9(4) COMP.
                10 POA-PC-VOR-VNR1     PIC X(14).
                10 POA-PC-KNR-ZWEIG1   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR2     PIC X(14).
                10 POA-PC-KNR-ZWEIG2   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR3     PIC X(14).
                10 POA-PC-KNR-ZWEIG3   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR4     PIC X(14).
                10 POA-PC-KNR-ZWEIG4   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR5     PIC X(14).
                10 POA-PC-KNR-ZWEIG5   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR6     PIC X(14).
                10 POA-PC-KNR-ZWEIG6   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR7     PIC X(14).
                10 POA-PC-KNR-ZWEIG7   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR8     PIC X(14).
                10 POA-PC-KNR-ZWEIG8   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR9     PIC X(14).
                10 POA-PC-KNR-ZWEIG9   PIC S9(4) COMP.
                10 POA-PC-VOR-VNR10    PIC X(14).
                10 POA-PC-KNR-ZWEIG10  PIC S9(4) COMP.
                10 POA-PC-HAND         PIC X.
                10 POA-PC-ERSTELLUNG   PIC X(26).
                10 POA-PC-KENNUNG-ERST PIC X(8).
                10 POA-PC-LAST-UPDATE  PIC X(26).
                10 POA-PC-KENNUNG-AEND PIC X(8).
                10 POA-PC-XGDV.
                   15 POA-PC-XGDV-LEN    PIC S9(4) COMP.
                   15 POA-PC-XGDV-TEXT   PIC X(25000).
                10 POA-PC-ANTRAG-DATUM  PIC X(10).
                10 POA-PC-VBEGINN-E     PIC X(10).
                10 POA-PC-VABLAUF-E     PIC X(10).
                10 POA-PC-KNR-ZAHLW-E   PIC S9(4) COMP.
                10 POA-PC-KTY-ZAHLW-E   PIC S9(4) COMP.
                10 POA-PC-PDVNBLZ       PIC S9(8) COMP.
                10 POA-PC-PDVNBLZ-E     PIC S9(8) COMP.
                10 POA-PC-PDVNKONTONR   PIC S9(10) COMP.
                10 POA-PC-PDVNKONTONR-E PIC S9(10) COMP.
                10 POA-PC-PDVNBEGINKM   PIC S9(6) COMP.
                10 POA-PC-PDVNBEGINKM-E PIC S9(6) COMP.
                10 POA-PC-PDVNFGESTNR   PIC X(17).
                10 POA-PC-PDVNFGESTNR-E PIC X(17).
                10 POA-PC-PDVNAMTLKZ    PIC X(11).
                10 POA-PC-PDVNAMTLKZ-E  PIC X(11).
                10 POA-PC-PDVNJAHRKM    PIC S9(6) COMP.
                10 POA-PC-PDVNJAHRKM-E  PIC S9(6) COMP.
                10 POA-PC-KD-BEST-ID    PIC S9(18)V COMP-3.
                10 POA-PC-VB-GES        PIC S9(4) COMP.
                10 POA-PC-WIDERSPR      PIC X(1).
                10 POA-PC-WIDERSPR-E    PIC X(1).
                10 POA-PC-PIN-PZ        PIC X(2).
                10 POA-PC-BARCODE       PIC X(10).
                10 POA-PC-SCHNELLANTR   PIC X(1).
                10 POA-PC-GESCHAEFT-VORG PIC X(2).
                10 POA-PC-IBAN          PIC X(34).
                10 POA-PC-IBAN-E        PIC X(34).
                10 POA-PC-BIC           PIC X(11).
                10 POA-PC-BIC-E         PIC X(11).
                10 POA-PC-ESIGN         PIC X(01).
      *
      ****** ENDE COPY-MEMBER ****************************************
 

