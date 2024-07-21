      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU265S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/UU265S.CPv  $
      *
      *   Rev 2.2   Apr 18 2002 11:35:46   t13ae28
      *FSL00092: LV-Betrag bei Bestand GV und ZV nicht korrekt
      *
      *   Rev 2.1   Mar 28 2002 15:00:50   t13ae28
      *FSL00092: Zahlungssumme GV für Bestand nicht korrekt
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Dec 06 1996 09:39:54   T13AE28
      *AFVB0325 Anpassung Satzbett an neue GV- und AV-Felder
      *         D.Ehrmann
      *
      *   Rev 1.1   Jan 03 1996 10:16:36   T13AE25
      *AFVB0244  Anpassungen für neue Bedingungen
      *          im Lauf VSW'95
      *
      *   Rev 1.0   Nov 04 1995 09:58:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    22.12.93 16:22:46
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : UEBERSICHT VERSORGUNGSWERK    *
      *                      GRUND- UND AUFBAUVERSORGUNG   *
      *  DATEINUMMER       : UU0265                        *
      *  COPYMEMBER        : UU265S                        *
      *  SATZLAENGE        : 160 BYTES                     *
      *  DAVON FILLER      :   5 BYTES                     *
      *  SCHLUESSEL        : UU265S-VBNR                   *
      *  SCHLUESSELLAENGE  : 7 BYTES                       *
      *  ERSTELLDATUM      : 28.11.1990                    *
      *                                                    *
      ******************************************************
      ****************************************************************  00090200
      * AENDERUNGEN:                                                 *  00090300
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00090400
      * -------------------------------------------------------------*  00090500
      *         !22.12.93! 01 ! NEU                       !          *  00090600
      * AFVB0244!22.12.95! 02 ! Neue Felder für Wachtums- !          *  00091500
      *         !        !    ! bonus und ED-Förderung    ! GERLACH  *  00091600
      *         !        !    !                           !          *  00091700
      *         !21.10.96! 03 ! Neue Felder für Netto-    !          *  00091500
      *         !        !    ! Eigen-Geschäft und        ! D.EHRMANN*  00091600
      *         !        !    ! DBI-KTO-NR                !          *  00091700
      *         !        !    !                           !          *  00091800
      * FSL00092!25.03.02! 04 ! UU265S-UEBERWSG-GV-VSW    !          *  00091500
      *         !        !    ! MIT NK-STELLEN WG EURO    ! D.EHRMANN*  00091600
      *         !        !    !                           !          *  00091700
      *         !        !    !                           !          *  00091800
      *                                                              *  00091900
      * ENDE-LOGBUCH                                                 *  00092000
      ****************************************************************  00092100


       01  UU265S-REC.
           05  UU265S-DIR               PIC 9(03).
           05  UU265S-VBNR              PIC 9(07).
           05  UU265S-NACHNAME          PIC X(20).
           05  UU265S-VORNAME           PIC X(20).
           05  UU265S-STRUK-STUFE       PIC X(04).
           05  UU265S-ZV-STAND          PIC X(02).
           05  UU265S-PRAXIS-STUFE      PIC 9(01).
XX1293*    05  UU265S-GES-UMSATZ        PIC 9(09).
XX1293*    05  UU265S-EQF               PIC 9(03)V9.
XX1293     05  UU265S-KUM-EIGEN        PIC S9(09)      COMP-3.
XX1293     05  UU265S-UMSATZ-E         PIC S9(09)      COMP-3.
XX1293     05  UU265S-EQF-E            PIC 9(03)V9     COMP-3.
XX1293     05  UU265S-UMSATZ-B         PIC S9(09)      COMP-3.
XX1293     05  UU265S-EQF-B            PIC 9(03)V9     COMP-3.
XX1293     05  UU265S-UMSATZ-G         PIC S9(09)      COMP-3.
XX1293     05  UU265S-EQF-G            PIC 9(03)V9     COMP-3.
           05  UU265S-D7-STUFE          PIC 9(01).
           05  UU265S-D7-HOLOG          PIC X(01).
           05  UU265S-STAND-GV          PIC X(02).
           05  UU265S-ZV-II-KZ          PIC X(01).
           05  UU265S-STAND-AV          PIC X(02).
           05  UU265S-UEBERWSG-AE       PIC 9(06).
           05  UU265S-UEBERWSG-D7       PIC 9(06).
           05  UU265S-UEBERWSG-HG       PIC 9(06).
           05  UU265S-UEBERWSG-AG       PIC 9(06).
250302*    05  UU265S-UEBERWSG-GV-VSW   PIC 9(06).
250302     05  UU265S-UEBERWSG-GV-VSW   PIC 9(06)V99.
250302*    05  UU265S-UEBERWSG-GV-ZV    PIC 9(06).
250302     05  UU265S-UEBERWSG-GV-ZV    PIC 9(06)V99.
XX1293     05  UU265S-STATUS-AUN       PIC X(01).
XX1293     05  UU265S-PRZ              PIC X(01).
221295     05  UU265S-Umsatz-G-V       PIC S9(09)      COMP-3.
221295     05  UU265S-ProzAbw          PIC s9(03)V9    COMP-3.
221295     05  UU265S-ED-Anz           PIC 9(02).
221295     05  UU265S-NET-NEU-E        PIC S9(09) COMP-3.
221295     05  UU265S-DBI-KTO-NR       PIC X(09).
250302*    05  UU265S-FILLER           PIC X(05).
250302     05  UU265S-FILLER           PIC X(01).

