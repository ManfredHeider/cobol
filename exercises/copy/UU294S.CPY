      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   uu294s.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\uu294s.cpv  $
      *
      *   Rev 2.1   01 Dec 2000 10:10:42   T13AE30
      *ASL00081 - Bürozuschuss für AVBG
      *
      *   Rev 2.0   Feb 27 1999 15:15:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:59:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : BUERO-DATEI                   *
      *  DATEINUMMER       : UU0245                        *
      *  COPYMEMBER        : UU294S                        *
      *  SATZLAENGE        : 170 BYTES                     *
      *  SCHLUESSEL        : UU294S-KEY                    *
      *  SCHLUESSELLAENGE  :  11 BYTES                     *
      *  ERSTELLDATUM      : 26.06.1991                    *
      *                                                    *
      ******************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM ! VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00081!01.11.00! 2.1! ANPASSUNG BÜRO-ZUSCHUSS   ! WILLIG   *
      *         !        !    ! AUCH FÜR AVBG             !          *
      *         !        !    ! ERWEITERUNG DER SST UM    !          *
      *         !        !    ! GESELL-TYP UND WÄHRUNGS-KZ!          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  UU294S.
           05  BER-SATZ.
             10  UU294S-KEY.
                 15  UU294S-DIR            PIC 9(03).
                 15  UU294S-VBNR           PIC 9(07).
                 15  UU294S-VBNR-P         PIC X.
             10  UU294S-STUFE              PIC X(04).
             10  UU294S-NAME               PIC X(20).
             10  UU294S-NETGRUP            PIC S9(07)V9.
             10  UU294S-NETGRUP-KORR1      PIC S9(07)V9.
             10  UU294S-EINH-STUFE         PIC S9(07)V9.
             10  UU294S-BUERO-ZUSCHUSS-TAB OCCURS 3.
                 15 UU294S-DATUM           PIC 9(04).
                 15 UU294S-BUERO-ZUSCHUSS  PIC S9(07).
             10  UU294S-ZUSCHUSS-ALT       PIC S9(06).
             10  UU294S-ZUSCHUSS-NEU       PIC S9(06).
             10  UU294S-ABZUG-SUMME        PIC S9(06).
             10  UU294S-EHEP-VBNR          PIC 9(07).
             10  UU294S-EHEP-UMSATZ        PIC S9(07)V9.
             10  UU294S-25-KZ              PIC 9.
             10  UU294S-ANPASS-AUS         PIC X(05).
             10  UU294S-ANPASS-ZUM         PIC X(05).
011100       10  UU294S-KTY-GESELL-TYP     PIC S9(04) COMP.
011100       10  UU294S-KNR-GESELL-TYP     PIC S9(04) COMP.
011100       10  UU294S-KTY-WAEHRUNGS-KZ   PIC S9(04) COMP.
011100       10  UU294S-KNR-WAEHRUNGS-KZ   PIC S9(04) COMP.
011100       10  FILLER                    PIC X(26).
011100*      10  FILLER                    PIC X(34).
