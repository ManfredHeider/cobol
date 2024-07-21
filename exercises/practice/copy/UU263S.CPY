      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU263S.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/UU263S.CPv  $
      *
      *   Rev 2.3   May 15 2008 07:56:56   t13ae28
      *ASL00284: VSW2007 / GV und Zusatzvers.
      *
      *   Rev 2.2   Jan 20 2006 08:30:14   t13ae28
      *ASL00216: Versorgungswerk 2005
      *
      *   Rev 2.1   05 Feb 2001 16:07:46   T13AE28
      *ASL00058: Anpassungen VSW2000
      *
      *   Rev 2.0   Sep 10 1999 10:31:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Nov 18 1996 13:04:00   T13AE28
      *AFVB0321 Anpassungen an die Bedingungen des VSW96
      *         D.Ehrmann
      *
      *   Rev 1.0   Nov 04 1995 09:57:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   4    18.11.96 16:57:13
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : GRUNDVERSORGUNG VSW U. ZV     *
      *  DATEINUMMER       : UU0263                        *
      *  COPYMEMBER        : UU263S                        *
      *  SATZLAENGE        : 155 BYTES                     *
      *  DAVON FILLER      :   0 BYTES                     *
      *  SCHLUESSEL        : UU263S-VBNR                   *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *  ERSTELLDATUM      : 01.10.1990                    *
      *                                                    *
      *  NEUE FELDER   GSELLHOFER XX.XX.93                 *
      *                UU263S-KUM-EIGEN                    *
      *                UU263S-UMSATZ-E                     *
      *                UU263S-EQF-E                        *
      *                UU263S-UMSATZ-B                     *
      *                UU263S-EQF-B                        *
      *                UU263S-UMSATZ-G                     *
      *                UU263S-EQF-G                        *
      *  NEUE FELDER   D.EHRMANN 21.10.1996                *
      *                UU263S-NET-NEU-E                    *
      ******************************************************
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00058!08.11.00! 01 ! KZ NEUER VB-VERTRAG       !D.EHRMANN *
      *         !        !    !                           !          *
      * ASL00216!14.10.05! 05 ! LV-Beitrag für GV und ZV  !D.EHRMANN *
      *         !        !    ! eingefügt                 !          *
      * ASL00284!09.10.07!3.4 ! ANPASSUNGEN VSW2007       ! D.Ehrmann *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************


       01  UU263S-REC.
           05  UU263S-DIR              PIC 9(03).
           05  UU263S-VBNR             PIC 9(07).
           05  UU263S-NACHNAME         PIC X(20).
           05  UU263S-VORNAME          PIC X(20).
           05  UU263S-G-VSNR           PIC 9(10).
           05  UU263S-G-ABLAUFLSTG     PIC 9(07)V9(02).
           05  UU263S-GEBURTSTAG       PIC 9(06).
           05  UU263S-ALTERSGRENZE     PIC X(01).
           05  UU263S-AG-96-ZUS        PIC X(01).
           05  UU263S-STRUK-STUFE      PIC X(04).
    93*    05  UU263S-GES-UMSATZ       PIC 9(09).
    93*    05  UU263S-EQF              PIC 9(03)V9.
           05  UU263S-KUM-EIGEN        PIC S9(09)      COMP-3.
211096     05  UU263S-NET-NEU-E        PIC S9(09)      COMP-3.
           05  UU263S-UMSATZ-E         PIC S9(09)      COMP-3.
           05  UU263S-EQF-E            PIC 9(03)V9     COMP-3.
           05  UU263S-UMSATZ-B         PIC S9(09)      COMP-3.
           05  UU263S-EQF-B            PIC 9(03)V9     COMP-3.
           05  UU263S-UMSATZ-G         PIC S9(09)      COMP-3.
           05  UU263S-EQF-G            PIC 9(03)V9     COMP-3.
081100*    05  UU263S-D7-STUFE         PIC 9(01).
081100     05  UU263S-VERTRAG          PIC X(01).
           05  UU263S-STAND-VORJAHR    PIC X(02).
           05  UU263S-NEU-WIK          PIC X(01).
           05  UU263S-STAND-LFD-JAHR   PIC X(02).
           05  UU263S-ZV-STAND.
               10  UU263S-ZV-STAND-1   PIC X(01).
               10  UU263S-ZV-STAND-2   PIC X(01).
           05  UU263S-ZVKZ             PIC 9(04).
           05  FILLER                  REDEFINES UU263S-ZVKZ.
               10  UU263S-ZVKZ-NR      PIC 9(02).
               10  UU263S-ZVKZ-JJ      PIC 9(02).
           05  UU263S-ZV-II-KZ         PIC X(01).
           05  UU263S-ZV-VSNR          PIC 9(10).
           05  UU263S-ZV-ABLAUFLSTG    PIC 9(07)V9(02).
           05  UU263S-PRAXIS-STUFE     PIC 9(01).
           05  UU263S-AUSDAT           PIC 9(06).
           05  UU263S-PRZ              PIC X(01).
141005     05  UU263S-LV-Beitrag-GV    PIC 9(07)V9(02).
141005     05  UU263S-LV-Beitrag-ZV    PIC 9(07)V9(02).
091007     05  UU263S-G-RV-VSNR        PIC 9(10).
091007     05  UU263S-Z-RV-VSNR        PIC 9(10).
091007     05  UU263S-RV-Beitrag-GV    PIC 9(07)V9(02).
091007     05  UU263S-RV-Beitrag-ZV    PIC 9(07)V9(02).
091007     05  UU263S-UMSATZ-EIG       PIC S9(09)      COMP-3.
091007     05  UU263S-UMSATZ-GES       PIC S9(09)      COMP-3.
091007     05  FILLER                  PIC 9(09).

