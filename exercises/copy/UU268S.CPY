      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU268S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\UU268S.CPv  $
      *
      *   Rev 2.1   26 Mar 2001 14:36:48   T13AE28
      *ASL00058: Anpassungen VSW2000
      *
      *   Rev 2.0   Feb 27 1999 15:15:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 13 1997 13:32:32   T13AE28
      *AFVB0345 Anpassungen ZV-Verlängerung VSW96
      *
      *   Rev 1.0   Nov 04 1995 09:58:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    01.02.94 15:57:58
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : ZV-VERLAENGERUNG              *
      *  DATEINUMMER       : UU0268                        *
      *  COPYMEMBER        : UU268S                        *
      *  SATZLAENGE        : 158 BYTES                     *
      *  DAVON FILLER      :  00 BYTES                     *
      *  SCHLUESSEL        : UU268S-VBNR                   *
      *  SCHLUESSELLAENGE  :   7 BYTES                     *
      *  ERSTELLDATUM      : 26.11.1990                    *
      *  AENDERUNGSDATUM   : 27.01.1997                    *
      *                                                    *
      *  HINZUFUEGEN VON:                                  *
      *                                                    *
      *          UU268S-GES-EQF-V                          *
      *          UU268S-EIG-EQF-V                          *
      *  12.12.91  MUSALL                                  *
      *          UU268S-ZV-VERL-KZ-ALT  PIC X(1). GEAEND.  *
      *          (VORHER: UU268S-KUEND-KZ-ALT)             *
      *          UU268S-ZV-VERL-KZ-NEU  PIC X(1). GEAEND.  *
      *          (VORHER: UU268S-KUEND-KZ-NEU)             *
      *          UU268S-ZV-KUEDAT-ALT   PIC 9(6). NEU!     *
      *          UU268S-ZV-KUEDAT-NEU   PIC 9(6). NEU!     *
      *  XX.01.94  GSELLHOFER                              *
      *          NEUE UMSATZFELDER EINGEFÜGT               *
      *          VORHER FILLER (PIC X(01) )                *
      *                                                    *
      *  27.01.97  D.EHRMANN                               *
      *          NEUE UMSATZFELDER EINGEFÜGT               *
      *          UU268S-NET-NEU-E                          *
      *                                                    *
      ******************************************************

       01  UU268S-REC.
           05  UU268S-DIR              PIC 9(03).
           05  UU268S-VBNR             PIC 9(07).
           05  UU268S-NACHNAME         PIC X(20).
           05  UU268S-VORNAME          PIC X(20).
           05  UU268S-STRUK-STUFE      PIC X(04).
120301*    05  UU268S-ZV-STAND         PIC 9(02).
120301     05  UU268S-ZV-STAND         PIC X(02).
           05  UU268S-ZV-BEGINN        PIC 9(06).
           05  UU268S-ZV-ABLAUF        PIC 9(02).
           05  UU268S-ZV-VERL-KZ-ALT   PIC X(01).
           05  UU268S-ZV-VERL-KZ-NEU   PIC X(01).
           05  UU268S-KUM-EIGEN        PIC S9(07)     COMP-3.
           05  UU268S-GES-UMSATZ-2     PIC S9(07)     COMP-3.
           05  UU268S-EIG-UMSATZ-2     PIC S9(07)     COMP-3.
           05  UU268S-GRU-UMSATZ-2     PIC S9(07)     COMP-3.
           05  UU268S-GES-UMSATZ-V     PIC S9(07)     COMP-3.
           05  UU268S-EIG-UMSATZ-V     PIC S9(07)     COMP-3.
270197     05  UU268S-NET-NEU-E        PIC S9(07)     COMP-3.
           05  UU268S-GRU-UMSATZ-V     PIC S9(07)     COMP-3.
           05  UU268S-GES-UMSATZ-D     PIC S9(07)     COMP-3.
           05  UU268S-EIG-UMSATZ-D     PIC S9(07)     COMP-3.
270197     05  UU268S-NET-NEU-E-D      PIC S9(07)     COMP-3.
           05  UU268S-GRU-UMSATZ-D     PIC S9(07)     COMP-3.
           05  UU268S-GES-EQF          PIC 9(03)V9    COMP-3.
           05  UU268S-GES-EQF-V        PIC 9(03)V9    COMP-3.
           05  UU268S-EIG-EQF          PIC 9(03)V9    COMP-3.
           05  UU268S-EIG-EQF-V        PIC 9(03)V9    COMP-3.
           05  UU268S-GRU-EQF          PIC 9(03)V9    COMP-3.
           05  UU268S-GRU-EQF-V        PIC 9(03)V9    COMP-3.
           05  UU268S-D7-STUFE         PIC 9(01).
           05  UU268S-STAND-LFD-JAHR   PIC X(02).
           05  UU268S-PRAXIS-STUFE     PIC 9(01).
           05  UU268S-BDV-MITGL        PIC X(01).
           05  UU268S-ABLAUF-NEU       PIC 9(02).
120301*    05  UU268S-BEMERKUNG        PIC X(06).
120301     05  UU268S-BEMERKUNG        PIC X(05).
120301     05  UU268S-VERTRAG          PIC X(01).
           05  UU268S-ZV-KUEDAT-ALT    PIC 9(06).
           05  UU268S-ZV-KUEDAT-NEU    PIC 9(06).
           05  UU268S-PRZ              PIC X(01).
      *    05  UU268S-FILLER           PIC X(06).
