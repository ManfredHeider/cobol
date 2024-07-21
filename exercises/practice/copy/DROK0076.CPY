      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0076.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DROK0076.CPv  $
      *
      *   Rev 2.5   May 28 2015 09:54:56   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   Apr 23 2015 13:25:18   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.3   22 Apr 2008 13:09:14   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.2   07 Apr 2008 14:48:32   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:07:10   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   23 Feb 2007 09:04:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0076                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE DWS, GESELLSCHAFT 48                 *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0006   *
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
      * AKI00556!07.04.08!2.2 ! Mandantenmerkmal eingefügt! Bößhar    *
      *         !        !    ! DWS-VB-GES                !           *
      * AAN01211!17.04.15!2.4 ! XGDV, ZAHLWEISE, LAUFZEIT !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-DWS.
 
           03  DWS-DATEN.
               05  DWS-HVANTNUM             PIC X(09).
               05  DWS-VB-GES               PIC 9(02).
               05  DWS-VDABVERM             PIC 9(07).
               05  DWS-VDABVERMPZ           PIC X(01).
               05  DWS-VDVMNR               PIC 9(07).
               05  DWS-VDVMNRPZ             PIC X(01).
               05  DWS-VDVMZUNA             PIC X(26).
               05  DWS-VDVMVORNA            PIC X(26).
               05  DWS-VDDIREKT             PIC 9(03).
               05  DWS-PDVNZUNA             PIC X(26).
               05  DWS-PDVNVORNA            PIC X(26).
               05  DWS-PDVNPLZ              PIC X(06).
               05  DWS-PDVNORT              PIC X(21).
               05  DWS-PDVNLKZ              PIC X(03).
 
               05  DWS-PROD1                PIC 9(03).
               05  DWS-SUMME-PROD1          PIC 9(07).
               05  DWS-EINH-PROD1           PIC 9(06)V9.
               05  DWS-ZAHLWEISE1           PIC 9(02).
               05  DWS-LAUFZEIT1            PIC 9(04).
 
               05  DWS-PROD2                PIC 9(03).
               05  DWS-SUMME-PROD2          PIC 9(07).
               05  DWS-EINH-PROD2           PIC 9(06)V9.
               05  DWS-ZAHLWEISE2           PIC 9(02).
               05  DWS-LAUFZEIT2            PIC 9(04).
 
               05  DWS-PROD3                PIC 9(03).
               05  DWS-SUMME-PROD3          PIC 9(07).
               05  DWS-EINH-PROD3           PIC 9(06)V9.
               05  DWS-ZAHLWEISE3           PIC 9(02).
               05  DWS-LAUFZEIT3            PIC 9(04).
 
               05  DWS-PROD4                PIC 9(03).
               05  DWS-SUMME-PROD4          PIC 9(07).
               05  DWS-EINH-PROD4           PIC 9(06)V9.
               05  DWS-ZAHLWEISE4           PIC 9(02).
               05  DWS-LAUFZEIT4            PIC 9(04).
 
               05  DWS-PROD5                PIC 9(03).
               05  DWS-SUMME-PROD5          PIC 9(07).
               05  DWS-EINH-PROD5           PIC 9(06)V9.
               05  DWS-ZAHLWEISE5           PIC 9(02).
               05  DWS-LAUFZEIT5            PIC 9(04).
 
               05  DWS-PROD6                PIC 9(03).
               05  DWS-SUMME-PROD6          PIC 9(07).
               05  DWS-EINH-PROD6           PIC 9(06)V9.
               05  DWS-ZAHLWEISE6           PIC 9(02).
               05  DWS-LAUFZEIT6            PIC 9(04).
 
               05  DWS-PROD7                PIC 9(03).
               05  DWS-SUMME-PROD7          PIC 9(07).
               05  DWS-EINH-PROD7           PIC 9(06)V9.
               05  DWS-ZAHLWEISE7           PIC 9(02).
               05  DWS-LAUFZEIT7            PIC 9(04).
 
               05  DWS-PROD8                PIC 9(03).
               05  DWS-SUMME-PROD8          PIC 9(07).
               05  DWS-EINH-PROD8           PIC 9(06)V9.
               05  DWS-ZAHLWEISE8           PIC 9(02).
               05  DWS-LAUFZEIT8            PIC 9(04).
 
               05  DWS-PROD9                PIC 9(03).
               05  DWS-SUMME-PROD9          PIC 9(07).
               05  DWS-EINH-PROD9           PIC 9(06)V9.
               05  DWS-ZAHLWEISE9           PIC 9(02).
               05  DWS-LAUFZEIT9            PIC 9(04).
 
               05  DWS-VDABLEIN             PIC 9(06)V9.
               05  DWS-AFO-34C-ZEICHN       PIC 9(07).
               05  FILLER                   PIC X(03).
               05  DWS-ANTRAG-DATUM         PIC X(10).
               05  DWS-VERSION              PIC 9(04).
 
      **** DIE BLACK-BOX HAT 20.000 BYTES
      *    03  FILLER                       PIC X(19654).
170415         05  DWS-XGDV                 PIC X(20000).
170415*    03  FILLER                       PIC X(19652).
      ****
      **** ENDE COPY DROK0076 *****************************************
      ****
 

