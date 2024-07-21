      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0073.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DROK0073.CPv  $
      *
      *   Rev 2.9   Oct 24 2013 11:02:00   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.8   19 Jun 2009 14:45:28   t13ex32
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.7   15 Jan 2009 15:59:54   t13ex32
      *AAN00843: Horst Bößhar OKI
      *
      *   Rev 2.6   19 Jun 2008 08:26:50   T13EX32
      *AAN00801: Hr. Bößhar 
      *
      *   Rev 2.5   11 Jun 2008 16:24:24   T13ex32
      *AAN00801: Hr. Bößhar 
      *
      *   Rev 2.4   11 Jun 2008 15:49:54   T13ex32
      *AAN00801: Hr. Bößhar 
      *
      *   Rev 2.3   22 Apr 2008 13:08:30   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.2   07 Apr 2008 14:48:06   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:06:30   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   23 Feb 2007 09:03:20   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0073                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE AMV (KFZ), GESELLSCHAFT 10           *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0003   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.02.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!23.02.07!2.1 ! NEU                       ! KLEEMEIER *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!02.04.08!2.2 ! Mandantenmerkmal eigefügt ! Bößhar    *
      *         !        !    ! POA-VB-GES                !           *
      * --------+--------+----+---------------------------+---------- *
      * AAL00801!02.06.08!2.3 ! Widerspruchsfelfd eigefügt! Bößhar    *
      * --------+--------+----+---------------------------+---------- *
020609* AAN00884!02.06.09!2.5 ! Neue Felder               ! Wehlitz   *
020609*         !        !    ! - PIN + PIN-PZ            !           *
020609*         !        !    ! - Barcode                 !           *
020609*         !        !    ! - KZ-Schnellantrag        !           *
020609*         !        !    ! - Geschaeftsvorgang       !           *
      * --------+--------+----+---------------------------+---------- *
      * AAN01073!24.10.13!2.9 ! SEPA                      ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-AMV.

           03  POA-DATEN.
               05  POA-KENNUNG-AEND         PIC X(04).
               05  POA-HVANTNUM             PIC X(09).
               05  POA-HVANTNUM-LFD         PIC X(01).
      *        Neu POA-VB-GES
               05  POA-VB-GES               PIC 9(02).
020609         05  POA-PIN                  PIC X(09).
020609         05  POA-PIN-PZ               PIC X(02).
020609         05  POA-BARCODE              PIC X(10).
020609         05  POA-SCHNELLANTR          PIC X(01).
020609         05  POA-GESCHAEFT-VORG       PIC X(02).
               05  POA-VDABVERM             PIC 9(07).
               05  POA-VDABVERMPZ           PIC X(01).
               05  POA-VDVMNR               PIC 9(07).
               05  POA-VDVMNRPZ             PIC X(01).
               05  POA-VDVMZUNA             PIC X(26).
               05  POA-VDVMVORNA            PIC X(26).
               05  POA-VDDIREKT             PIC 9(03).
               05  POA-PDVNZUNA             PIC X(26).
               05  POA-PDVNVORNA            PIC X(26).
               05  POA-PDVNPLZ              PIC X(06).
               05  POA-PDVNORT              PIC X(21).
               05  POA-PDVNLKZ              PIC X(03).
               05  POA-ANTRAG-DATUM         PIC X(10).

               05  POA-VBEGINN              PIC X(10).
      ***      05  POA-VBEGINN-E            PIC X(10).
               05  POA-VABLAUF              PIC X(10).
      ***      05  POA-VABLAUF-E            PIC X(10).
               05  POA-KNR-ZAHLW            PIC 9(02).
      ***      05  POA-KNR-ZAHLW-E          PIC 9(02).
               05  POA-PDVN-BLZ             PIC 9(08).
      ***      05  POA-PDVN-BLZ-E           PIC 9(08).
               05  POA-PDVN-KONTONR         PIC 9(10).
      ***      05  POA-PDVN-KONTONR-E       PIC 9(10).
241013         05  POA-PDVN-IBAN            PIC X(34).
241013         05  POA-PDVN-BIC             PIC X(11).
               05  POA-PDVN-BEGINKM         PIC 9(06).
      ***      05  POA-PDVN-BEGINKM-E       PIC 9(06).
               05  POA-PDVN-FGESTNR         PIC X(17).
      ***      05  POA-PDVN-FGESTNR-E       PIC X(17).
               05  POA-PDVN-AMTLKZ          PIC X(11).
      ***      05  POA-PDVN-AMTLKZ-E        PIC X(11).
               05  POA-PDVN-JAHRKM          PIC 9(06).
      ***      05  POA-PDVN-JAHRKM-E        PIC 9(06).
020608         05  POA-WIDERSPRUCH          PIC X.

               05  POA-VDPRODKEY1           PIC 9(04).
               05  POA-VDABLEIN1            PIC 9(06)V9(01).
               05  POA-BEITRAGBR1           PIC 9(05)V9(02).
               05  POA-BEITRAGNE1           PIC 9(05)V9(02).

               05  POA-VDPRODKEY2           PIC 9(04).
               05  POA-VDABLEIN2            PIC 9(06)V9(01).
               05  POA-BEITRAGBR2           PIC 9(05)V9(02).
               05  POA-BEITRAGNE2           PIC 9(05)V9(02).

               05  POA-VDPRODKEY3           PIC 9(04).
               05  POA-VDABLEIN3            PIC 9(06)V9(01).
               05  POA-BEITRAGBR3           PIC 9(05)V9(02).
               05  POA-BEITRAGNE3           PIC 9(05)V9(02).

               05  POA-VDPRODKEY4           PIC 9(04).
               05  POA-VDABLEIN4            PIC 9(06)V9(01).
               05  POA-BEITRAGBR4           PIC 9(05)V9(02).
               05  POA-BEITRAGNE4           PIC 9(05)V9(02).

               05  POA-VDPRODKEY5           PIC 9(04).
               05  POA-VDABLEIN5            PIC 9(06)V9(01).
               05  POA-BEITRAGBR5           PIC 9(05)V9(02).
               05  POA-BEITRAGNE5           PIC 9(05)V9(02).

               05  POA-KNR-ZWEIG1           PIC 9(04).
               05  POA-VOR-VNR1             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG2           PIC 9(04).
               05  POA-VOR-VNR2             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG3           PIC 9(04).
               05  POA-VOR-VNR3             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG4           PIC 9(04).
               05  POA-VOR-VNR4             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG5           PIC 9(04).
               05  POA-VOR-VNR5             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG6           PIC 9(04).
               05  POA-VOR-VNR6             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG7           PIC 9(04).
               05  POA-VOR-VNR7             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG8           PIC 9(04).
               05  POA-VOR-VNR8             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG9           PIC 9(04).
               05  POA-VOR-VNR9             PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KNR-ZWEIG10          PIC 9(04).
               05  POA-VOR-VNR10            PIC X(14).
               05  FILLER                   PIC X(07).

               05  POA-KD-BEST-ID           PIC 9(18).

               05  POA-XGDV                 PIC X(25000).

      **** DIE BLACK-BOX HAT 20.000 BYTES
           03  FILLER                       PIC X(11554).
      ****
      **** ENDE COPY DROK0073 *****************************************
      ****
