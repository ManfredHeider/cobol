      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0109.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0109.CPv  $
      *
      *   Rev 2.1   Jan 26 2000 16:28:42   T13EX21
      *APA00080 - Erweiterung Status-Bereich
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:08   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Mar 07 1997 09:17:30   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0109  Standard-Status-Bereich             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       *
      *    DATUM:    16.01.1996                                       *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    ! NEU                       ! MUELLER  *
      * AFPA0002!03.02.97! 02 ! TERMINAL-ID VON 8 AUF 4   ! AUERBACH *
      *         !        !    ! GEKÜRZT                   !          *
      * APA00012!23.07.98! 03 ! PTR FÜR CICS-ZUGRIFF      ! AUERBACH *
      *         !        !    !                           !          *
      * APA00081!21.01.00! 04 ! ERWEITERUNG UM PGEB-GES   ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *   - FUNKTION: STANDARDÜBERGABEBEREICH ERSTMALS IM DAISY
      *               PROJEKT VERWENDET
      *               DIESER BEREICH WIRD NEBEN INPUT-, OUTPUT- UND
      *               ERRORBEREICH üBERGEBEN.
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  STANDARD-STATUS-BEREICH.
           05  STAT-TERMINAL-ID               PIC X(004).
      *        * Kennung: Dialog=Userid; Batch=Hpro-Name
           05  STAT-KENNUNG                   PIC X(008).
           05  STAT-DIALOG-ID                 PIC X(004).
           05  STAT-S-SYSTEM                  PIC 9(001).
               88 STAT-SYSTEM-PROD                VALUE 1.
               88 STAT-SYSTEM-TEST                VALUE 2.
           05  STAT-S-SESSION                 PIC 9(001).
               88 STAT-DIALOG                     VALUE 1.
               88 STAT-BATCH                      VALUE 2.
           05  STAT-S-TRCE                    PIC 9(001).
               88 STAT-TRCE-EIN                   VALUE 1.
               88 STAT-TRCE-AUS                   VALUE 2.
           05  STAT-USITZG-ANZ                PIC 9(001).

      ***  DIE FOLGENDEN POINTER SIND NUR IM DIALOG (CICS) GEFÜLLT.
      ***  SIE DIENEN DAZU CICS-PGM UNTERHALB VON COBOL-PGM AUSZU-
      ***  FÜHREN OHNE EIBLK UND COMMAREA IMMER MITZUFÜHREN. DIES
      ***  IST INSBESONDERE FÜR FACHFUNKTIONEN NOTWENDIG, DIE IN
      ***  BATCH UND DIALOG GEMEINSAM BENUTZT WERDEN.
      ***  ANWENDUNGFALL: VSAM-OZP (ZAssnnnn RUFT ZDssnnnn AUF)
230798     05  STAT-PTR-DFHEIBLK              POINTER.
230798     05  STAT-PTR-DFHCOMMAREA           POINTER.
      *
210100     05  STAT-PGEB-GES                  PIC 9(004) USAGE COMP.
      *
      *
      ******** ENDE COPY-MEMBER (DRAT0109) *******************
