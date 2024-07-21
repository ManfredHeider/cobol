      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU297S.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/UU297S.CPv  $
      *
      *   Rev 2.3   Apr 17 2015 12:59:28   t13ae35
      *ASL00517: Abwicklung FAP 3. und 4.Quartal 2014 - UU800
      *
      *   Rev 2.2   20 Jun 2007 16:04:50   t13ae35
      *ASL00272: Besondere Zusatzleistungen
      *
      *   Rev 2.1   28 Mar 2001 13:47:10   T13EX57
      *ASL00085 - ANPASSUNG FUER AVBG
      *
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   05 May 1998 09:46:22   T13AE36
      *asl00016 - Zusaetziliches Feld eingefuehrt wegen neuen fap-n
      *
      *   Rev 1.1   May 20 1996 15:27:50   T13AE24
      *AFVB0274, neue Bedingungen FAP 1996
      *
      *   Rev 1.0   Nov 04 1995 09:59:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    26.08.94 16:36:33
      ******************************************************************
      *****                                                        *****
      *****        COPY-MEMBER FAP-BESTANDSDATEI                   *****
      *****        DATEINUMMER: UU0297                             *****
      *****        COPYMEMBER:  UU297S                             *****
      *****        SATZLAENGE:  100    BYTES                       *****
      *****                                                        *****
      *****        GEAENDERT AM 12.07.93 WEGEN NEUE BEDINGUNGEN,   *****
      *****        LAENGE BLEIBT, FILLER REDUZIERT VON 43 AUF 17.  *****
      *****                                                        *****
      *****        GEAENDERT AM 26.08.94 WIECHERS AFRZ0013         *****
      *****        KUS-SYSTEM / RACF                               *****
      *****                                                        *****
      *****        GEAENDERT AM 26.04.96 SCHMIDT  AFVB0294         *****
      *****        EIGEN-UMSATZ (NETTO OHNE FOLGE) NEU             *****
      *****                                                        *****
      *****        GEAENDERT AM 09.09.14 J.Hering ASL00517         *****
      *****        DVAG-STUFE-ALT als zusätzliches Attribut        *****
      *****                                                        *****
      ******************************************************************
       01  UU297S.
           05  FAP-SATZ.
               10  FAP-DIR             PIC  9(03).
               10  FAP-VBNR            PIC  9(07).
               10  FAP-GEBDAT          PIC  9(06).
               10  FAP-AUSDAT          PIC  9(06).
               10  FAP-VONDAT          PIC  9(06).
               10  FAP-MSTUFE          PIC  9(03).
               10  FAP-PRDAT           PIC  9(06).
               10  FAP-PRSTUFE         PIC  9(01).
               10  FAP-EIG-UMS-A       PIC S9(07)        COMP-3.
               10  FAP-EIG-UMS         PIC S9(07)        COMP-3.
               10  FAP-EIG-EQF         PIC S9(02)V9(01)  COMP-3.
               10  FAP-GR-UMS          PIC S9(07)        COMP-3.
               10  FAP-GR-EQF          PIC S9(02)V9(01)  COMP-3.
               10  FAP-GES-UMS         PIC S9(07)        COMP-3.
               10  FAP-GES-EQF         PIC S9(02)V9(01)  COMP-3.
               10  FAP-STUFE-ALT       PIC  9(01).
               10  FAP-STUFE-NEU       PIC  9(01).
               10  FAP-STUFE-MAN       PIC  X(01).
               10  FAP-AEN-KZ          PIC  9(01).
               10  FAP-ALTERSGR        PIC  X(01).
               10  FAP-KORR-KZ         PIC  X(01).
               10  FAP-DAT             PIC  9(07)        COMP-3.
               10  FAP-UHR             PIC  9(09)        COMP-3.
               10  FAP-TERM            PIC  X(04).
260894         10  FAP-USERID          PIC  X(07).
260496         10  FAP-EIG-UMS-NET     PIC S9(07)        COMP-3.
200498         10  FAP-EIG-UMS-NEU-A   PIC S9(07)        COMP-3.
250101         10  FAP-GES-BEZ         PIC  X(04).
190607         10  FAP-VB-VERTR-KZ     PIC  X(01).
090914         10  DVAG-STUFE-ALT      PIC  X(01).
190607*        10  FILLER              PIC  X(01).
           05  FAP-DUMMY   REDEFINES   FAP-SATZ.
               10  FAPD-DIR            PIC  9(03).
               10  FAPD-VBNR           PIC  9(07).
               10  FAPD-ANZS           PIC S9(05)       COMP-3.
               10  FAPD-STICHTAG       PIC  9(06)       COMP-3.
               10  FAPD-EDAT           PIC  9(06)       COMP-3.
               10  FAPD-UDAT           PIC  9(06)       COMP-3.
               10  FAPD-UZEIT          PIC  9(09)       COMP-3.
               10  FAPD-VERARB         PIC  X(01).
               10  FILLER              PIC  X(07).
               10  FAPD-AEN-KZ         PIC  9(01).
               10  FILLER              PIC  X(61).
 

