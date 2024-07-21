      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU260S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\UU260S.CPv  $
      *
      *   Rev 2.1   23 Nov 2000 15:25:56   T13AE28
      *ASL00048: Anpassungen Neue ZV2-Neuaufnahme
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   03 Nov 1997 11:06:34   T13AE28
      *AFVB0378 - Anpassung an Bed. VSW97
      *
      *   Rev 1.1   Oct 30 1996 11:49:20   T13AE28
      *AFVB0316 Anpassungen/Korrekturen im Rahmen des VSW96
      *         D.Ehrmann
      *
      *   Rev 1.0   Nov 04 1995 09:57:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    23.11.93 15:57:10
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : ZV II AUFNAHME DATEI          *
      *  DATEINUMMER       : UU0260                        *
      *  COPYMEMBER        : UU260S                        *
      *  SATZLAENGE        : 120 BYTES                     *
      *  DAVON FILLER      :  05 BYTES                     *
      *  SCHLUESSEL        : UU260S-VBNR                   *
      *  SCHLUESSELLAENGE  :   7 BYTES                     *
      *  ERSTELLDATUM      : 01.10.1990                    *
      *                                                    *
      *  AENDERUNG:                                        *
      *  NEUES FELD;   MUSALL;   12.12.91                  *
      *                UU260S-ZV-KUEDAT      PIC 9(6) NEU! *
      *                UU260S-FILLER         PIC X(5) VERK.*
      *  NEUE FELDER   GSELLHOFER 23.11.93                 *
      *                UU260S-KUM-EIGEN                    *
      *                UU260S-UMSATZ-E                     *
      *                UU260S-EQF-E                        *
      *                UU260S-UMSATZ-B                     *
      *                UU260S-EQF-B                        *
      *                UU260S-UMSATZ-G                     *
      *                UU260S-EQF-G                        *
      *                UU260S-PRZ                          *
      ******************************************************
      *****************************************************************
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00048!06.11.00! 01 ! KZ NEUER VB-VERTRAG       !D.EHRMANN *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  UU260S-REC.
           05  UU260S-DIR              PIC 9(03).
           05  UU260S-VBNR             PIC 9(07).
           05  UU260S-NACHNAME         PIC X(20).
           05  UU260S-VORNAME          PIC X(20).
           05  UU260S-ZV-STAND         PIC X(02).
           05  UU260S-ZV-BEGINN        PIC 9(06).
171097*    05  UU260S-ZV-ABLAUF        PIC 9(02).
171097     05  UU260S-ZV-ABLAUF        PIC X(02).
           05  UU260S-ZV-VERL-KZ       PIC X(01).
           05  UU260S-KUM-EIGEN        PIC S9(09)      COMP-3.
071096     05  UU260S-NET-NEU-E        PIC S9(09)      COMP-3.
           05  UU260S-UMSATZ-E         PIC S9(09)      COMP-3.
           05  UU260S-EQF-E            PIC 9(03)V9     COMP-3.
           05  UU260S-UMSATZ-B         PIC S9(09)      COMP-3.
           05  UU260S-EQF-B            PIC 9(03)V9     COMP-3.
           05  UU260S-UMSATZ-G         PIC S9(09)      COMP-3.
           05  UU260S-EQF-G            PIC 9(03)V9     COMP-3.
           05  UU260S-D7-STUFE         PIC 9(01).
           05  UU260S-STRUK-STUFE      PIC X(04).
           05  UU260S-STAND-LFD-JAHR   PIC X(02).
           05  UU260S-PRAXIS-STUFE     PIC 9(01).
           05  UU260S-VERTR-VOM        PIC 9(06).
           05  UU260S-BDV-MITGL        PIC X(01).
           05  UU260S-ZV-KUEDAT        PIC 9(06).
           05  UU260S-PRZ              PIC X(01).
           05  UU260S-VERTRAG          PIC X(01).
071096*    05  UU260S-FILLER           PIC X(08).
061100*    05  UU260S-FILLER           PIC X(03).
061100     05  UU260S-FILLER           PIC X(02).

