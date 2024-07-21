      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU530S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU530S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:01:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ****************************************************************
      *                                                              *
      *          OP-SATZ AUS AACHEN                                  *
      *          VARIABELE LAENGE, MINIMAL 107, MAXIMAL 395 BYTES    *
      *          SEQUENTIELLE DATEI                                  *
      *          SA: DTA-KENNZEICHEN, HIER KONSTANT = 'C'            *
      *                                                              *
      *          AUFTRAG UU89030 VB-WETTBEWERB 1990                  *
      *          EINFUEHRUNG UU530S-ZAHLWEISE                        *
      *          28.11.89  GEIGER                                    *
      *                                                              *
      *          AUFTRAG UU90003-014/056                             *
      *          EINFUEHRUNG DDR-KENNZEICHEN                         *
      *          30.08.90  GEIGER                                    *
      *                                                              *
      *          AUFTRAG UU90003-014/056                             *
      *          EINFUEHRUNG STORNOKENNZEICHEN FUER PS 85 UND 86     *
      *          03.09.90  GEIGER                                    *
      *                                                              *
      *          EINFUEHRUNG UNFALL-KENNZEICHEN UU530S-UNVKZ         *
      *          05.02.91  HECKER                                    *
      *                                                              *
      *          LOESCHEN UNFALL-KENNZEICHEN UU530S-UNVKZ            *
      *          02.07.91  HECKER                                    *
      ****************************************************************
       01  UU530S-SATZ-1.
           03 UU530S-SA                 PIC X.
           03 UU530S-KEY.
              05 UU530S-VSNR            PIC 9(11).
              05 UU530S-FAELLIG         PIC 9(6).
              05 UU530S-ERSTELL         PIC 9(13).
           03 UU530S-VBNR               PIC 9(6).
           03 UU530S-GES                PIC 99.
           03 UU530S-NAME               PIC X(20).
           03 UU530S-ABRECHDAT          PIC 9(6).
           03 UU530S-BUCHDATPART        PIC 9(6).
           03 UU530S-BVRD               PIC XX.
           03 UU530S-INTVSNR            PIC 9(15).
           03 UU530S-RESERVE            PIC X(14).
           03 UU530S-AMVERS             REDEFINES UU530S-RESERVE.
              05 UU530S-AMVERS-RESERVE  PIC X(14).
           03 UU530S-ROLAND             REDEFINES UU530S-RESERVE.
              05 UU530S-WERTNR          PIC X.
              05 UU530S-SPARTE          PIC 9.
              05 UU530S-LAUFZEIT.
                 07 UU530S-LAUFZEIT1    PIC 9.
                 07 UU530S-LAUFZEIT2    PIC 9.
              05 UU530S-EINLOESBETR     PIC 9(7)V99.
              05 UU530S-EINLOESKZ       PIC 9.
           03 UU530S-DDR-KZ             PIC X.
           03 UU530S-VERTRAGS-STORNO    PIC X.
           03 UU530S-ZAHLWEISE          PIC 9.
           03 UU530S-TABIND             PIC 99.
           03 UU530S-PRODTAB  OCCURS 0 TO 12 DEPENDING ON UU530S-TABIND.
              05 UU530S-PRODART         PIC 9.
              05 UU530S-PRODSCHL        PIC 99.
              05 UU530S-PROVPFLSU       PIC 9(7)V99.
              05 UU530S-PROVSATZ        PIC 99V9.
              05 UU530S-PRORATASU       PIC 9(7)V99.

       01  UU530S-SATZ.
           03 UU530S-D-SA               PIC X.
           03 UU530S-D-KEY              PIC X(17).
           03 UU530S-D-SATZANZ          PIC 9(7)        COMP-3.
           03 UU530S-D-GESPROVGUT       PIC 9(15)V99    COMP-3.
           03 UU530S-D-GESPROVBEL       PIC 9(15)V99    COMP-3.
           03 UU530S-D-RESTANZ          PIC 9(7)        COMP-3.
           03 UU530S-D-RESTPROVGUT      PIC 9(15)V99    COMP-3.
           03 UU530S-D-RESTPROVBEL      PIC 9(15)V99    COMP-3.
           03 UU530S-D-REST             PIC X(55).

