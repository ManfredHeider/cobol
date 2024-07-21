      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0075.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DROK0075.CPv  $
      *
      *   Rev 2.5   Oct 24 2013 11:02:06   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.4   22 Apr 2008 13:11:02   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.3   15 Apr 2008 13:34:08   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.2   07 Apr 2008 14:48:24   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:06:56   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   23 Feb 2007 09:03:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0075                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE ADVOCARD, GESELLSCHAFT 81            *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0005   *
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
      *         !        !    ! ADVC-VB-GES               !           *
      * AAN01073!24.1013 !2.3 ! SEPA                      ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-ADVOCARD.

           03  ADVC-DATEN.
               05  ADVC-VERSION             PIC 9(04).
               05  ADVC-HVANTNUM            PIC X(09).
      *        neu   ADVC-VB-GES
               05  ADVC-VB-GES              PIC 9(02).
               05  ADVC-VDABVERM            PIC 9(07).
               05  ADVC-VDABVERMPZ          PIC X(01).
               05  ADVC-VDVMNR              PIC 9(07).
               05  ADVC-VDVMNRPZ            PIC X(01).
               05  ADVC-VDVMZUNA            PIC X(26).
               05  ADVC-VDVMVORNA           PIC X(26).
               05  ADVC-VDDIREKT            PIC 9(03).
               05  ADVC-PDVNZUNA            PIC X(26).
               05  ADVC-PDVNVORNA           PIC X(26).
               05  ADVC-PDVNPLZ             PIC X(06).
               05  ADVC-PDVNORT             PIC X(21).
               05  ADVC-PDVNLKZ             PIC X(03).
               05  ADVC-VDPRODKEY           PIC 9(04).
               05  ADVC-VBEGINN             PIC X(10).
               05  ADVC-VABLAUF             PIC X(10).
               05  ADVC-KNR-ZAHLW           PIC 9(02).
160408*        05  ADVC-KNR-ZAHLW-E         PIC 9(02).
               05  ADVC-BEITRAG-BR          PIC 9(5)V99.
               05  ADVC-BEITRAG-NE          PIC 9(5)V99.
               05  ADVC-VDABLEIN            PIC 9(6)V9.
               05  FILLER                   PIC X(03).
               05  ADVC-ANTRAG-DATUM        PIC X(10).
               05  ADVC-KONTONR             PIC 9(12).
160408*        05  ADVC-KONTONR-E           PIC 9(12).
               05  ADVC-BLZ                 PIC 9(08).
241013         05  ADVC-IBAN                PIC X(34).
241013         05  ADVC-BIC                 PIC X(11).
160408*        05  ADVC-BLZ-E               PIC 9(08).
               05  ADVC-AMTLKZ              PIC X(12).
160408*        05  ADVC-AMTLKZ-E            PIC X(12).
               05  ADVC-V-PLUS              PIC X(01).
160408*        05  ADVC-V-PLUS-E            PIC X(01).
               05  ADVC-KD-BEST-ID          PIC 9(18).
      *
160408*        05  ADVC-XGDV                PIC X(19688).
160408         05  ADVC-XGDV                PIC X(19686).

      **** ADVOCARD FÜLLT DIE BLACK-BOX KOMPLETT AUS (XGDV)

      ****
      **** ENDE COPY DROK0075 *****************************************
      ****
