      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU264S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/UU264S.CPv  $
      *
      *   Rev 2.2   Jan 30 2006 16:04:24   t13ae28
      *ASL00216: GV und AV
      *
      *   Rev 2.1   21 Feb 2001 15:11:58   T13AE28
      *ASL00058: Anpassungen VSW2000
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Dec 02 1996 11:02:20   T13AE28
      *AFVB0323 Anpassungen im Zuge vSW96
      *         D.Ehrmann
      *
      *   Rev 1.1   Dec 06 1995 09:02:22   T13AE25
      *AFVB0244-4: Neue Bedingungen VSW '95
      *            Copy um ED-Anzahl und prozentuale Abweichung ergänzt
      *
      *   Rev 1.0   Nov 04 1995 09:58:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    16.12.93 17:19:49
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00048!06.11.00! 01 ! FELD PROZABW ERWEITERT    !D.EHRMANN *
      *         !        !    !                           !          *
      * ASL00048!22.11.05! 05 ! Anp.VSW2005               !D.EHRMANN *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : AUFBAUVERSORGUNG VSW          *
      *  DATEINUMMER       : UU0264                        *
      *  COPYMEMBER        : UU264S                        *
      *  SATZLAENGE        : 155 BYTES                     *
      *  DAVON FILLER      :   7 BYTES                     *
      *  SCHLUESSEL        : UU264S-VBNR                   *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *  ERSTELLDATUM      : 26.11.1990                    *
      *                                                    *
      ******************************************************


       01  UU264S-REC.
           05  UU264S-DIR              PIC 9(03).
           05  UU264S-VBNR             PIC 9(07).
           05  UU264S-NACHNAME         PIC X(20).
           05  UU264S-VORNAME          PIC X(20).
           05  UU264S-STRUK-STUFE      PIC X(04).
XX1293*    05  UU264S-GES-UMSATZ       PIC 9(09).
XX1293*    05  UU264S-EQF              PIC 9(03)V9.
XX1293     05  UU264S-KUM-EIGEN        PIC S9(09)      COMP-3.
211096     05  UU264S-NET-NEU-E        PIC S9(09)      COMP-3.
XX1293     05  UU264S-UMSATZ-E         PIC S9(09)      COMP-3.
XX1293     05  UU264S-EQF-E            PIC 9(03)V9     COMP-3.
XX1293     05  UU264S-UMSATZ-B         PIC S9(09)      COMP-3.
XX1293     05  UU264S-EQF-B            PIC 9(03)V9     COMP-3.
XX1293     05  UU264S-UMSATZ-G         PIC S9(09)      COMP-3.
XX1293     05  UU264S-EQF-G            PIC 9(03)V9     COMP-3.
           05  UU264S-D7-STUFE         PIC 9(01).
           05  UU264S-D7-HOLOG         PIC X(01).
           05  UU264S-STAND-VORJAHR    PIC X(02).
           05  UU264S-NEU              PIC X(01).
           05  UU264S-STAND-LFD-JAHR   PIC X(02).
           05  UU264S-IG-VSW           PIC X(01).
           05  UU264S-AG-96-ZUS        PIC X(01).
           05  UU264S-ZV-STAND.
               10  UU264S-ZV-STAND-1   PIC X(01).
               10  UU264S-ZV-STAND-2   PIC X(01).
           05  UU264S-UEBERWSG-AE      PIC 9(06).
           05  UU264S-UEBERWSG-D7      PIC 9(06).
           05  UU264S-UEBERWSG-HG      PIC 9(06).
           05  UU264S-UEBERWSG-AG      PIC 9(06).
           05  UU264S-UEBERWSG-SU      PIC 9(06).
           05  UU264S-AUSDAT           PIC 9(06).
           05  UU264S-PRAXIS-STUFE     PIC 9(01).
XX1293     05  UU264S-STATUS-AUN       PIC X(01).
XX1293     05  UU264S-PRZ              PIC X(01).
           05  UU264S-Umsatz-G-V       PIC S9(09)      COMP-3.
061100*    05  UU264S-ProzAbw          PIC s9(03)V9    COMP-3.
081100     05  UU264S-ProzAbw          PIC s9(05)V9    COMP-3.
           05  UU264S-ED-Anz           PIC 9(02).
211096     05  UU264S-DBI-KTO-NR       PIC X(09).
081100*    05  UU264S-FILLER           PIC X(07).
081100     05  UU264S-VERTRAG          PIC X(01).
221105     05  UU264S-EIGEN-GES-E      PIC S9(09)      COMP-3.
221105     05  UU264S-GRUPPE-GES-E     PIC S9(09)      COMP-3.
221105     05  UU264S-EQF-GRUPPE       PIC 9(03)V9     COMP-3.
221105     05  UU264S-KORR-E           PIC S9(09)      COMP-3.
081100     05  UU264S-FILLER           PIC X(20).

