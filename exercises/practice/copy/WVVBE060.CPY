      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBE060.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBE060.CPv  $
      *
      *   Rev 2.1   Feb 17 2020 14:40:02   t13ae30
      *AVB01605: Nacharbeiten alle Clubs (VB-System)
      *
      *   Rev 2.0   Feb 06 2020 17:03:22   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBE060                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBE060 (CLPRUEF )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T13AE30                                          *
      *    DATUM:    06.02.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-1975!06.02.20!2.1 ! NEU                       ! T13AE30   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBE060                          *
      *****************************************************************
 
        05 WB-NI-VIVBE060-TAB.
           10 WB-NI-VIVBE060-ELE   PIC S9(04)  BINARY OCCURS  74.
        05 WB-NI-VIVBE060          REDEFINES   WB-NI-VIVBE060-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-KTY-CLUB-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-CLUB-TYP          PIC S9(04)       BINARY.
           10 WB-NI-CLUB-KRITERIUM        PIC S9(04)       BINARY.
           10 WB-NI-CLUB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-CL-MS-NR-ERF          PIC S9(04)       BINARY.
           10 WB-NI-CLUB-VBP-NR           PIC S9(04)       BINARY.
           10 WB-NI-CL-VBP-NR-ERF         PIC S9(04)       BINARY.
           10 WB-NI-CLUB-ANZ-KDN          PIC S9(04)       BINARY.
           10 WB-NI-CL-ANZ-KDN-ERF        PIC S9(04)       BINARY.
           10 WB-NI-ANZ-KDN-2-PLUS        PIC S9(04)       BINARY.
           10 WB-NI-CL-ANZ-KDN2-ERF       PIC S9(04)       BINARY.
           10 WB-NI-NET-NEU-EIG-12MON     PIC S9(04)       BINARY.
           10 WB-NI-CL-NN-EIG12MO-ERF     PIC S9(04)       BINARY.
           10 WB-NI-MAX-EQF-EIG           PIC S9(04)       BINARY.
           10 WB-NI-CL-MAX-EQFEIG-ERF     PIC S9(04)       BINARY.
           10 WB-NI-BEITRAG-VOL           PIC S9(04)       BINARY.
           10 WB-NI-CL-BEITRAG-ERF        PIC S9(04)       BINARY.
           10 WB-NI-VB-SEIT-MIN-MON       PIC S9(04)       BINARY.
           10 WB-NI-CL-VB-SEIT-ERF        PIC S9(04)       BINARY.
           10 WB-NI-AS-SEIT-MIN-MON       PIC S9(04)       BINARY.
           10 WB-NI-CL-AS-SEIT-ERF        PIC S9(04)       BINARY.
           10 WB-NI-NET-EIG-GES-36MO      PIC S9(04)       BINARY.
           10 WB-NI-CL-NEGES36MO-ERF      PIC S9(04)       BINARY.
           10 WB-NI-NET-GES-12MO          PIC S9(04)       BINARY.
           10 WB-NI-CL-NGES12MO-ERF       PIC S9(04)       BINARY.
           10 WB-NI-KTY-TYP-BEN-CLUB      PIC S9(04)       BINARY.
           10 WB-NI-KNR-TYP-BEN-CLUB      PIC S9(04)       BINARY.
           10 WB-NI-CL-BEN-CLUB-ERF       PIC S9(04)       BINARY.
           10 WB-NI-KTY-TYP-BEEND-CLUB    PIC S9(04)       BINARY.
           10 WB-NI-KNR-TYP-BEEND-CLUB    PIC S9(04)       BINARY.
           10 WB-NI-CL-BEEND-CLUB-ERF     PIC S9(04)       BINARY.
           10 WB-NI-ED-ANZ-SOLL           PIC S9(04)       BINARY.
           10 WB-NI-CL-EDANZ-ERF          PIC S9(04)       BINARY.
           10 WB-NI-ANZ-PART-KAP          PIC S9(04)       BINARY.
           10 WB-NI-CL-ANZPART-ERF        PIC S9(04)       BINARY.
           10 WB-NI-ANZ-AKT-STRUK-ADM     PIC S9(04)       BINARY.
           10 WB-NI-CL-AKTSTRUK-ERF       PIC S9(04)       BINARY.
           10 WB-NI-EIG-GES-12MON         PIC S9(04)       BINARY.
           10 WB-NI-CL-GES-EIG12MO-ERF    PIC S9(04)       BINARY.
           10 WB-NI-CLBED-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-MAX-EQF-GES           PIC S9(04)       BINARY.
           10 WB-NI-CL-MX-EQFGES-ERF      PIC S9(04)       BINARY.
           10 WB-NI-ANZ-HAUPTBEPART       PIC S9(04)       BINARY.
           10 WB-NI-CL-HAUPTPART-ERF      PIC S9(04)       BINARY.
           10 WB-NI-ANZ-NEU-DIR-PART      PIC S9(04)       BINARY.
           10 WB-NI-CL-NEU-DPART-ERF      PIC S9(04)       BINARY.
           10 WB-NI-NET-NEU-GES-12MO      PIC S9(04)       BINARY.
           10 WB-NI-CL-NNGES12MO-ERF      PIC S9(04)       BINARY.
           10 WB-NI-CL-ANZ-STERNE         PIC S9(04)       BINARY.
           10 WB-NI-CL-ANZ-STERNE-ERF     PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-1              PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-1-ERF          PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-2              PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-2-ERF          PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-GES            PIC S9(04)       BINARY.
           10 WB-NI-FAKTOR-GES-ERF        PIC S9(04)       BINARY.
           10 WB-NI-BQF                   PIC S9(04)       BINARY.
           10 WB-NI-BQF-ERF               PIC S9(04)       BINARY.
           10 WB-NI-CL-GESICHERT-BIS      PIC S9(04)       BINARY.
           10 WB-NI-BED-ERF-MIN-MON       PIC S9(04)       BINARY.
           10 WB-NI-BED-ERF-MM-ERF        PIC S9(04)       BINARY.
           10 WB-NI-KAP-SEIT-MIN-MON      PIC S9(04)       BINARY.
           10 WB-NI-KAP-SEIT-MM-ERF       PIC S9(04)       BINARY.
           10 WB-NI-ED-MITGL-BIS          PIC S9(04)       BINARY.
           10 WB-NI-FAK-1-ISTWERT         PIC S9(04)       BINARY.
           10 WB-NI-FAK-2-ISTWERT         PIC S9(04)       BINARY.
           10 WB-NI-GES-EH-EIG-MAX        PIC S9(04)       BINARY.
           10 WB-NI-EH-ZUSTRUK            PIC S9(04)       BINARY.
           10 WB-NI-EH-NEUE-ED            PIC S9(04)       BINARY.
           10 WB-NI-FAK2-200-KZ           PIC S9(04)       BINARY.
           10 WB-NI-KRIT-ERF-KZ           PIC S9(04)       BINARY.
           10 WB-NI-PRUEF-GES-KZ          PIC S9(04)       BINARY.
           10 WB-NI-ED-MITGL-ERF          PIC S9(04)       BINARY.
        05 WB-VIVBE060.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-KTY-CLUB-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-CLUB-TYP             PIC S9(04)       BINARY.
           10 WB-CLUB-KRITERIUM           PIC S9(04)       BINARY.
           10 WB-CLUB-MS-NR               PIC S9(04)       BINARY.
           10 WB-CL-MS-NR-ERF             PIC  X(01).
           10 WB-CLUB-VBP-NR              PIC S9(04)       BINARY.
           10 WB-CL-VBP-NR-ERF            PIC  X(01).
           10 WB-CLUB-ANZ-KDN             PIC S9(04)       BINARY.
           10 WB-CL-ANZ-KDN-ERF           PIC  X(01).
           10 WB-ANZ-KDN-2-PLUS           PIC S9(04)       BINARY.
           10 WB-CL-ANZ-KDN2-ERF          PIC  X(01).
           10 WB-NET-NEU-EIG-12MON        PIC S9(09)V9(01) COMP-3.
           10 WB-CL-NN-EIG12MO-ERF        PIC  X(01).
           10 WB-MAX-EQF-EIG              PIC S9(03)V9(02) COMP-3.
           10 WB-CL-MAX-EQFEIG-ERF        PIC  X(01).
           10 WB-BEITRAG-VOL              PIC S9(09)V9(02) COMP-3.
           10 WB-CL-BEITRAG-ERF           PIC  X(01).
           10 WB-VB-SEIT-MIN-MON          PIC S9(04)       BINARY.
           10 WB-CL-VB-SEIT-ERF           PIC  X(01).
           10 WB-AS-SEIT-MIN-MON          PIC S9(04)       BINARY.
           10 WB-CL-AS-SEIT-ERF           PIC  X(01).
           10 WB-NET-EIG-GES-36MO         PIC S9(09)V9(01) COMP-3.
           10 WB-CL-NEGES36MO-ERF         PIC  X(01).
           10 WB-NET-GES-12MO             PIC S9(09)V9(01) COMP-3.
           10 WB-CL-NGES12MO-ERF          PIC  X(01).
           10 WB-KTY-TYP-BEN-CLUB         PIC S9(04)       BINARY.
           10 WB-KNR-TYP-BEN-CLUB         PIC S9(04)       BINARY.
           10 WB-CL-BEN-CLUB-ERF          PIC  X(01).
           10 WB-KTY-TYP-BEEND-CLUB       PIC S9(04)       BINARY.
           10 WB-KNR-TYP-BEEND-CLUB       PIC S9(04)       BINARY.
           10 WB-CL-BEEND-CLUB-ERF        PIC  X(01).
           10 WB-ED-ANZ-SOLL              PIC S9(04)       BINARY.
           10 WB-CL-EDANZ-ERF             PIC  X(01).
           10 WB-ANZ-PART-KAP             PIC S9(04)       BINARY.
           10 WB-CL-ANZPART-ERF           PIC  X(01).
           10 WB-ANZ-AKT-STRUK-ADM        PIC S9(04)       BINARY.
           10 WB-CL-AKTSTRUK-ERF          PIC  X(01).
           10 WB-EIG-GES-12MON            PIC S9(09)V9(01) COMP-3.
           10 WB-CL-GES-EIG12MO-ERF       PIC  X(01).
           10 WB-CLBED-DAT-VON            PIC  X(10).
           10 WB-MAX-EQF-GES              PIC S9(03)V9(02) COMP-3.
           10 WB-CL-MX-EQFGES-ERF         PIC  X(01).
           10 WB-ANZ-HAUPTBEPART          PIC S9(04)       BINARY.
           10 WB-CL-HAUPTPART-ERF         PIC  X(01).
           10 WB-ANZ-NEU-DIR-PART         PIC S9(04)       BINARY.
           10 WB-CL-NEU-DPART-ERF         PIC  X(01).
           10 WB-NET-NEU-GES-12MO         PIC S9(09)V9(01) COMP-3.
           10 WB-CL-NNGES12MO-ERF         PIC  X(01).
           10 WB-CL-ANZ-STERNE            PIC S9(04)       BINARY.
           10 WB-CL-ANZ-STERNE-ERF        PIC  X(01).
           10 WB-FAKTOR-1                 PIC S9(03)V9(02) COMP-3.
           10 WB-FAKTOR-1-ERF             PIC  X(01).
           10 WB-FAKTOR-2                 PIC S9(03)V9(02) COMP-3.
           10 WB-FAKTOR-2-ERF             PIC  X(01).
           10 WB-FAKTOR-GES               PIC S9(03)V9(02) COMP-3.
           10 WB-FAKTOR-GES-ERF           PIC  X(01).
           10 WB-BQF                      PIC S9(03)V9(02) COMP-3.
           10 WB-BQF-ERF                  PIC  X(01).
           10 WB-CL-GESICHERT-BIS         PIC  X(10).
           10 WB-BED-ERF-MIN-MON          PIC S9(04)       BINARY.
           10 WB-BED-ERF-MM-ERF           PIC  X(01).
           10 WB-KAP-SEIT-MIN-MON         PIC S9(04)       BINARY.
           10 WB-KAP-SEIT-MM-ERF          PIC  X(01).
           10 WB-ED-MITGL-BIS             PIC  X(10).
           10 WB-FAK-1-ISTWERT            PIC S9(09)V9(01) COMP-3.
           10 WB-FAK-2-ISTWERT            PIC S9(09)V9(01) COMP-3.
           10 WB-GES-EH-EIG-MAX           PIC S9(09)V9(01) COMP-3.
           10 WB-EH-ZUSTRUK               PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NEUE-ED               PIC S9(09)V9(01) COMP-3.
           10 WB-FAK2-200-KZ              PIC  X(01).
           10 WB-KRIT-ERF-KZ              PIC  X(01).
           10 WB-PRUEF-GES-KZ             PIC  X(01).
           10 WB-ED-MITGL-ERF             PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  74                    *
      *****************************************************************

