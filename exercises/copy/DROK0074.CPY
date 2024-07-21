      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0074.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DROK0074.CPv  $
      *
      *   Rev 2.5   Nov 19 2015 08:04:48   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.4   Nov 04 2015 13:32:10   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.3   22 Apr 2008 13:09:02   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.2   07 Apr 2008 14:48:16   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:06:42   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   23 Feb 2007 09:03:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0074                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE BADENIA, GESELLSCHAFT 70             *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0004   *
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
      * AKI00556!02.04.08!2.2 ! Mandantenmerkmal eingefügt! Bößhar    *
      *         !        !    ! VIA-VB-GES                !           *
      * AAN01211!21.10.15!2.3 ! XGDV AUF 30000            ! HEIDER    *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-BADENIA.
 
           03  VIA-DATEN.
               05  VIA-HVANTNUM             PIC 9(17).
      *        Neu VIA-VB-GES
               05  VIA-VB-GES               PIC 9(02).
               05  VIA-VDABVERM             PIC 9(07).
               05  VIA-VDABVERMPZ           PIC X(01).
 
               05  VIA-KENNUNG-BSV          PIC X(01).
               05  VIA-KENNUNG-TARIF        PIC X(01).
               05  VIA-KENNUNG-FRST         PIC X(01).
               05  VIA-KENNUNG-LEG          PIC X(01).
               05  VIA-KENNUNG-VL           PIC X(01).
               05  VIA-KENNUNG-VL2          PIC X(01).
               05  VIA-KENNUNG-ERH          PIC X(01).
 
               05  VIA-VDVMNR               PIC 9(07).
               05  VIA-VDVMNRPZ             PIC X(01).
               05  VIA-VDVMZUNA             PIC X(26).
               05  VIA-VDVMVORNA            PIC X(26).
               05  VIA-VDDIREKT             PIC 9(03).
               05  VIA-PDVNZUNA             PIC X(26).
               05  VIA-PDVNVORNA            PIC X(26).
               05  VIA-PDVNPLZ              PIC X(06).
               05  VIA-PDVNORT              PIC X(21).
               05  VIA-PDVNLKZ              PIC X(03).
 
               05  FILLER                   PIC X(03).
               05  VIA-VDPRODKEY            PIC 9(04).
               05  VIA-DVAG-PRODKEY         PIC 9(04).
               05  VIA-SUMME                PIC 9(09)V9(02).
               05  VIA-VDABLEIN             PIC 9(06)V9(01).
               05  VIA-ANTRAG-DATUM         PIC X(10).
               05  VIA-VERSION              PIC 9(04).
               05  VIA-KD-BEST-ID           PIC 9(18).
 
               05  VIA-XGDV                 PIC X(30000).
 
      **** DIE BLACK-BOX HAT 20.000 BYTES
      *    03  FILLER                       PIC X(11967).
211015*?   03  FILLER                       PIC X(11965).
      ****
      **** ENDE COPY DROK0074 *****************************************
      ****
 

