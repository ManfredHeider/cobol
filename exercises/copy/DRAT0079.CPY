      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0079.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0079.CPv  $
      *
      *   Rev 2.0   Mar 01 1999 11:08:08   T13AE26
      *Wettbewerbe 99
      *
      *   Rev 1.0   Nov 04 1995 08:49:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0079   KNR-KONST. FUER PRODUKT-SPARTEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    28.03.1994                                       *
      *****************************************************************
      *    VERSION   1    29.10.94 10:10:45
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB00XX!28.03.94! 01 ! NEU                       ! SCHMIDT  *
      * AFVB0142!12.10.94! 02 ! ANPASSEN AN NEUE - BZW.   !          *
      *         !        !    ! GEÄNDERTE STATISTIK-TABLES! SCHMIDT  *
      * AAS00088!12.01.99! 03 ! KONSTANTE FÜR             !          *
      *         !        !    ! ...-DYN.-LEBEN-SONST      !          *
      *         !        !    ! VERKüRZT UND SPALTEN >72  !          *
      *         !        !    ! gelöscht                  ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE FÜR PRODUKT-SPARTEN
      *
       01  FILLER.
121094*
121094*    05  K-KNR-LF-SACH-FOLGE            PIC S9(004) COMP VALUE 01.
121094*    05  K-KNR-LF-FOND-FOLGE            PIC S9(004) COMP VALUE 02.
121094*    05  K-KNR-LF-LEBEN                 PIC S9(004) COMP VALUE 03.
121094*    05  K-KNR-LF-KLV                   PIC S9(004) COMP VALUE 04.
121094*    05  K-KNR-LF-FLV                   PIC S9(004) COMP VALUE 05.
121094*    05  K-KNR-LF-RLV                   PIC S9(004) COMP VALUE 06.
121094*    05  K-KNR-LF-VLV                   PIC S9(004) COMP VALUE 07.
121094*    05  K-KNR-LF-SONSTIGE-LEBEN        PIC S9(004) COMP VALUE 08.
121094*    05  K-KNR-LF-UV                    PIC S9(004) COMP VALUE 09.
121094*    05  K-KNR-LF-KFZ                   PIC S9(004) COMP VALUE 10.
121094*    05  K-KNR-LF-H-H-G                 PIC S9(004) COMP VALUE 11.
121094*    05  K-KNR-LF-SV                    PIC S9(004) COMP VALUE 12.
121094*    05  K-KNR-LF-RS                    PIC S9(004) COMP VALUE 13.
121094*    05  K-KNR-LF-BS                    PIC S9(004) COMP VALUE 14.
121094*    05  K-KNR-LF-KV                    PIC S9(004) COMP VALUE 15.
121094*    05  K-KNR-LF-INVEST                PIC S9(004) COMP VALUE 16.
121094*    05  K-KNR-LF-IMMO                  PIC S9(004) COMP VALUE 17.
121094*    05  K-KNR-LF-BANKEN                PIC S9(004) COMP VALUE 18.
121094*    05  K-KNR-LF-SONSTIGE              PIC S9(004) COMP VALUE 19.
121094*    05  K-KNR-LF-DYNAMIK               PIC S9(004) COMP VALUE 20.
121094*    05  K-KNR-LF-FOLGEPROVISION        PIC S9(004) COMP VALUE 21.
121094*    05  K-KNR-LF-BUZ                   PIC S9(004) COMP VALUE 22.
121094
121094*    KEY ENTFÄLLT                        PIC S9(4) COMP VALUE 001.
121094     05 K-KNR-PR-SP-DG-FONDS-SONST       PIC S9(4) COMP VALUE 002.
121094*    KEY ENTFÄLLT                        PIC S9(4) COMP VALUE 003.
121094     05 K-KNR-PR-SP-AML-KLV              PIC S9(4) COMP VALUE 004.
121094     05 K-KNR-PR-SP-AML-FLV              PIC S9(4) COMP VALUE 005.
121094     05 K-KNR-PR-SP-AML-RLV              PIC S9(4) COMP VALUE 006.
121094     05 K-KNR-PR-SP-AML-VLV              PIC S9(4) COMP VALUE 007.
121094     05 K-KNR-PR-SP-AML-SONST            PIC S9(4) COMP VALUE 008.
121094     05 K-KNR-PR-SP-AMV-UV               PIC S9(4) COMP VALUE 009.
121094     05 K-KNR-PR-SP-AMV-KFZ              PIC S9(4) COMP VALUE 010.
121094     05 K-KNR-PR-SP-AMV-HHG              PIC S9(4) COMP VALUE 011.
121094     05 K-KNR-PR-SP-AMV-SONST            PIC S9(4) COMP VALUE 012.
121094     05 K-KNR-PR-SP-DG-RS                PIC S9(4) COMP VALUE 013.
121094     05 K-KNR-PR-SP-DG-BS                PIC S9(4) COMP VALUE 014.
121094     05 K-KNR-PR-SP-DG-KV                PIC S9(4) COMP VALUE 015.
121094     05 K-KNR-PR-SP-DG-INVEST            PIC S9(4) COMP VALUE 016.
121094     05 K-KNR-PR-SP-DG-IMMO              PIC S9(4) COMP VALUE 017.
121094     05 K-KNR-PR-SP-DG-BANKEN            PIC S9(4) COMP VALUE 018.
121094     05 K-KNR-PR-SP-DG-SONST             PIC S9(4) COMP VALUE 019.
121094     05 K-KNR-PR-SP-DYNAMIK              PIC S9(4) COMP VALUE 020.
121094*    KEY ENTFÄLLT                        PIC S9(4) COMP VALUE 021.
121094     05 K-KNR-PR-SP-AML-BUZ              PIC S9(4) COMP VALUE 022.
121094     05 K-KNR-PR-SP-DG-LEBEN-SONST       PIC S9(4) COMP VALUE 023.
120199* 121094     05 K-KNR-PR-SP-DG-LEBEN-DYNAMIK-SONST
120199     05 K-KNR-PR-SP-DG-LEBEN-DYN-SONST
121094                                         PIC S9(4) COMP VALUE 024.
121094*    KEYS VON 25 BIS 101 SIND NICHT VERGEBEN.
121094     05 K-KNR-PR-SP-WAL-DYNAMIK          PIC S9(4) COMP VALUE 102.
121094     05 K-KNR-PR-SP-WAV-UV               PIC S9(4) COMP VALUE 103.
121094     05 K-KNR-PR-SP-WAV-HHG              PIC S9(4) COMP VALUE 104.
121094     05 K-KNR-PR-SP-WAV-RS               PIC S9(4) COMP VALUE 105.
121094     05 K-KNR-PR-SP-WAV-VS               PIC S9(4) COMP VALUE 106.
121094     05 K-KNR-PR-SP-WAV-KV               PIC S9(4) COMP VALUE 107.
121094*    KEY ENTFÄLLT                        PIC S9(4) COMP VALUE 108.
121094     05 K-KNR-PR-SP-OEG-BS               PIC S9(4) COMP VALUE 109.
121094     05 K-KNR-PR-SP-OEG-INVEST           PIC S9(4) COMP VALUE 110.
121094     05 K-KNR-PR-SP-OEG-IMMO             PIC S9(4) COMP VALUE 111.
121094     05 K-KNR-PR-SP-OEG-BANKEN           PIC S9(4) COMP VALUE 112.
121094     05 K-KNR-PR-SP-WAL-KLV              PIC S9(4) COMP VALUE 113.
121094     05 K-KNR-PR-SP-WAL-RLV              PIC S9(4) COMP VALUE 114.
121094     05 K-KNR-PR-SP-WAL-BUZ              PIC S9(4) COMP VALUE 115.
121094     05 K-KNR-PR-SP-WAL-UZV              PIC S9(4) COMP VALUE 116.
121094     05 K-KNR-PR-SP-WAV-SONST            PIC S9(4) COMP VALUE 117.
121094*    KEY ENTFÄLLT                        PIC S9(4) COMP VALUE 118.
121094     05 K-KNR-PR-SP-WAL-SONST            PIC S9(4) COMP VALUE 119.
121094     05 K-KNR-PR-SP-WAV-KFZ              PIC S9(4) COMP VALUE 120.
      *
      ***** ENDE COPY-MEMBER DRAT0079  KNR-KONST. PRODUKT-SPARTEN ******
