      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0060.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0060.CPv  $
      *
      *   Rev 2.1   Aug 17 2005 09:13:16   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Jul 22 2005 10:44:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0060                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0060 (CLPRUEF)                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    22.07.2005                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!22.07.05!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  WB-NI-VIVB0060-TAB.
              10 WB-NI-VIVB0060-ELE  PIC S9(04) BINARY OCCURS 39 TIMES.
           05  WB-NI-VIVB0060        REDEFINES  WB-NI-VIVB0060-TAB.
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
              10 WB-NI-CLBED-DAT-VON           PIC S9(04)       BINARY.

           05  WB-VIVB0060.
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
              10 WB-CLBED-DAT-VON                  PIC X(10).
