      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB060.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVBB060.CPv  $
      *
      *   Rev 2.2   Jan 12 2009 15:44:16   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.1   Jan 09 2009 17:05:32   t13ae30
      *AAS00779: Allfinanz "Champions-Club" 
      *
      *   Rev 2.0   Sep 15 2008 12:09:16   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB060                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB060 (CLPRUEF)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    08.01.2007                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00779!01.09.08!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  WB-NI-VIVBB060-TAB.
              10 WB-NI-VIVBB060-ELE  PIC S9(04) BINARY OCCURS 49 TIMES.
           05  WB-NI-VIVBB060        REDEFINES  WB-NI-VIVBB060-TAB.
              10 WB-NI-MA-ST-NR                PIC S9(04)       BINARY.
              10 WB-NI-KTY-CL-TYP              PIC S9(04)       BINARY.
              10 WB-NI-KNR-CL-TYP              PIC S9(04)       BINARY.
              10 WB-NI-CLUB-KRITERIUM          PIC S9(04)       BINARY.
              10 WB-NI-CLUB-MS-NR              PIC S9(04)       BINARY.
              10 WB-NI-CL-MS-NR-ERF            PIC S9(04)       BINARY.
              10 WB-NI-CLUB-VBP-NR             PIC S9(04)       BINARY.
              10 WB-NI-CL-VBP-NR-ERF           PIC S9(04)       BINARY.
              10 WB-NI-CLUB-ANZ-KDN            PIC S9(04)       BINARY.
              10 WB-NI-CL-ANZ-KDN-ERF          PIC S9(04)       BINARY.
              10 WB-NI-ANZ-KDN-2-PLUS          PIC S9(04)       BINARY.
              10 WB-NI-CL-ANZ-KDN2-ERF         PIC S9(04)       BINARY.
              10 WB-NI-NET-NEU-EIG-12MON       PIC S9(04)       BINARY.
              10 WB-NI-CL-NN-EIG12MO-ERF       PIC S9(04)       BINARY.
              10 WB-NI-MAX-EQF-EIG             PIC S9(04)       BINARY.
              10 WB-NI-CL-MAX-EQFEIG-ERF       PIC S9(04)       BINARY.
              10 WB-NI-BEITRAG-VOL             PIC S9(04)       BINARY.
              10 WB-NI-CL-BEITRAG-ERF          PIC S9(04)       BINARY.
              10 WB-NI-VB-SEIT-MIN-MON         PIC S9(04)       BINARY.
              10 WB-NI-CL-VB-SEIT-ERF          PIC S9(04)       BINARY.
              10 WB-NI-AS-SEIT-MIN-MON         PIC S9(04)       BINARY.
              10 WB-NI-CL-AS-SEIT-ERF          PIC S9(04)       BINARY.
              10 WB-NI-NET-EIG-GES-36MO        PIC S9(04)       BINARY.
              10 WB-NI-CL-NEGES36MO-ERF        PIC S9(04)       BINARY.
              10 WB-NI-NET-GES-12MO            PIC S9(04)       BINARY.
              10 WB-NI-CL-NGES12MO-ERF         PIC S9(04)       BINARY.
              10 WB-NI-KTY-TYP-BEN-CLUB        PIC S9(04)       BINARY.
              10 WB-NI-KNR-TYP-BEN-CLUB        PIC S9(04)       BINARY.
              10 WB-NI-CL-BEN-CLUB-ERF         PIC S9(04)       BINARY.
              10 WB-NI-KTY-TYP-BEEND-CLUB      PIC S9(04)       BINARY.
              10 WB-NI-KNR-TYP-BEEND-CLUB      PIC S9(04)       BINARY.
              10 WB-NI-CL-BEEND-CLUB-ERF       PIC S9(04)       BINARY.
              10 WB-NI-ED-ANZ-SOLL             PIC S9(04)       BINARY.
              10 WB-NI-CL-EDANZ-ERF            PIC S9(04)       BINARY.
              10 WB-NI-ANZ-PART-KAP            PIC S9(04)       BINARY.
              10 WB-NI-CL-ANZPART-ERF          PIC S9(04)       BINARY.
              10 WB-NI-ANZ-AKT-STRUK-ADM       PIC S9(04)       BINARY.
              10 WB-NI-CL-AKTSTRUK-ERF         PIC S9(04)       BINARY.
              10 WB-NI-EIG-GES-12MO            PIC S9(04)       BINARY.
              10 WB-NI-CL-GES-EIG12MO-ERF      PIC S9(04)       BINARY.
              10 WB-NI-CLBED-DAT-VON           PIC S9(04)       BINARY.
              10 WB-NI-MAX-EQF-GES             PIC S9(04)       BINARY.
              10 WB-NI-CL-MAX-EQFGES-ERF       PIC S9(04)       BINARY.
              10 WB-NI-ANZ-HAUPTBER-PART       PIC S9(04)       BINARY.
              10 WB-NI-CL-HAUPTBER-PART-ERF    PIC S9(04)       BINARY.
              10 WB-NI-ANZ-NEU-DIR-PART        PIC S9(04)       BINARY.
              10 WB-NI-CL-ANZNEU-DIRPART-ER    PIC S9(04)       BINARY.
              10 WB-NI-NET-NEU-GES-12MO        PIC S9(04)       BINARY.
              10 WB-NI-CL-NNGES12MO-ERF        PIC S9(04)       BINARY.

           05  WB-VIVBB060.
              10 WB-MA-ST-NR                       PIC S9(9)    COMP.
              10 WB-KTY-CLUB-TYP                   PIC S9(4)    COMP.
              10 WB-KNR-CLUB-TYP                   PIC S9(4)    COMP.
              10 WB-CLUB-KRITERIUM                 PIC S9(4)    COMP.
              10 WB-CLUB-MS-NR                     PIC S9(4)    COMP.
              10 WB-CL-MS-NR-ERF                   PIC X.
              10 WB-CLUB-VBP-NR                    PIC S9(4)    COMP.
              10 WB-CL-VBP-NR-ERF                  PIC X.
              10 WB-CLUB-ANZ-KDN                   PIC S9(4)    COMP.
              10 WB-CL-ANZ-KDN-ERF                 PIC X.
              10 WB-ANZ-KDN-2-PLUS                 PIC S9(4)    COMP.
              10 WB-CL-ANZ-KDN2-ERF                PIC X.
              10 WB-NET-NEU-EIG-12MON              PIC S9(9)V9  COMP-3.
              10 WB-CL-NN-EIG12MO-ERF              PIC X.
              10 WB-MAX-EQF-EIG                    PIC S9(3)V9  COMP-3.
              10 WB-CL-MAX-EQFEIG-ERF              PIC X.
              10 WB-BEITRAG-VOL                    PIC S9(9)V9  COMP-3.
              10 WB-CL-BEITRAG-ERF                 PIC X.
              10 WB-VB-SEIT-MIN-MON                PIC S9(4)    COMP.
              10 WB-CL-VB-SEIT-ERF                 PIC X.
              10 WB-AS-SEIT-MIN-MON                PIC S9(4)    COMP.
              10 WB-CL-AS-SEIT-ERF                 PIC X.
              10 WB-NET-EIG-GES-36MO               PIC S9(9)V9  COMP-3.
              10 WB-CL-NEGES36MO-ERF               PIC X.
              10 WB-NET-GES-12MO                   PIC S9(9)V9  COMP-3.
              10 WB-CL-NGES12MO-ERF                PIC X.
              10 WB-KTY-TYP-BEN-CLUB               PIC S9(4)    COMP.
              10 WB-KNR-TYP-BEN-CLUB               PIC S9(4)    COMP.
              10 WB-CL-BEN-CLUB-ERF                PIC X.
              10 WB-KTY-TYP-BEEND-CLUB             PIC S9(4)    COMP.
              10 WB-KNR-TYP-BEEND-CLUB             PIC S9(4)    COMP.
              10 WB-CL-BEEND-CLUB-ERF              PIC X.
              10 WB-ED-ANZ-SOLL                    PIC S9(4)    COMP.
              10 WB-CL-EDANZ-ERF                   PIC X.
              10 WB-ANZ-PART-KAP                   PIC S9(4)    COMP.
              10 WB-CL-ANZPART-ERF                 PIC X.
              10 WB-ANZ-AKT-STRUK-ADM              PIC S9(4)    COMP.
              10 WB-CL-AKTSTRUK-ERF                PIC X.
              10 WB-EIG-GES-12MO                   PIC S9(9)V9  COMP-3.
              10 WB-CL-GES-EIG12MO-ERF             PIC X.
              10 WB-CLBED-DAT-VON                  PIC X(10).
              10 WB-MAX-EQF-GES                    PIC S9(3)V9  COMP-3.
              10 WB-CL-MAX-EQFGES-ERF              PIC X.
              10 WB-ANZ-HAUPTBER-PART              PIC S9(4)    COMP.
              10 WB-CL-HAUPTBER-PART-ERF           PIC X.
              10 WB-ANZ-NEU-DIR-PART               PIC S9(4)    COMP.
              10 WB-CL-ANZNEU-DIRPART-ER           PIC X.
              10 WB-NET-NEU-GES-12MO               PIC S9(9)V9  COMP-3.
              10 WB-CL-NNGES12MO-ERF               PIC X.
