      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Nov 1998 16:53:34   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 20 1997 17:00:10   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:38:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: AUFRUFPARAMETER UPAT0006  (DRAT0027)         *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                 *
      *    DATUM:    05.03.1993                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !05.03.93!    ! NEU                       ! SCHÖLLH. *
      * AFPA0002!20.01.97! 02 ! ÄNDERUNG SCHNITTST. WG.   ! SCHÖLLH. *
      *         !        !    !  'DUD' U. A.              !          *
      *         !        !    ! NEUER TYP 'USER-DATEN'    !          *
      * APA00012!......98! 03 ! ÄNDERUNG SCHNITTST. WG.   ! BOHLINGER*
      *         !        !    ! NEUER TYP 'DIALOG-MODUL'  !          *
      *         !        !    !                           !          *
      * APA00012!27.08.98! 04 ! ÄNDERUNG SCHNITTST. WG.   ! BOHLINGER*
      *         !        !    ! NEUER TYP 'buffer-ozp  '  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    AUFRUF-PARAMETER FUER UPRO UPAT0006 (ERMITTELN TSQ-NAME)
      *
      *    TSQ-TYPEN:
      *    - CA:        SICHERN DER CA BEI DIALOGUNTERBR. (IM DRIVER)
      *    - MAP:       SICHERN DER MAPS DES AKTUELLEN DIALOGS
      *    - BLAETTERN: SICHERN DER ANFANGS-KEYS BEI TABELLEN
      *    - DD-KOMM:   SICH. VON INFOS FUER DIALOG-DIALOG-KOMMUNIKATION
      *    - LINK-ERR:  SICH. VON ERROR-BEREICH FUER AUFRUF UEBER LINK
      *    - ...        (MAX. 8 TYPEN)
      *
      *    IMMER ZU SETZEN: TSQ-TYP
      *
      *    ABHAENGIG VON PI-UPAT0006-TSQ-TYP SIND FOLGENDE PARAMETER
      *    ZU FÜLLEN:
      *                         !EIBTRMID!PGM-NAME!USITZG-ANZ!  UANW  !
      *    ---------------------+--------+--------+----------+--------+
      *    TSQ-TYP-CA           !   X    !   X    !    X     !        !
      *    TSQ-TYP-MAP          !   X    !   X    !    X     !   X    !
      *    TSQ-TYP-BLAETTERN    !   X    !   X    !    X     !   X    !
      *    TSQ-TYP-DD-KOMM      !   X    !   X    !    X     !   X    !
      *    TSQ-TYP-LINK-ERR     !   X    !   X    !          !        !
      *    ---------------------+--------+--------+----------+--------+
      *
       01  PI-UPAT0006-EINGABE.
           05  PI-UPAT0006-TSQ-TYP             PIC  9(001).
               88  TSQ-TYP-CA                              VALUE ZERO.
               88  TSQ-TYP-MAP                             VALUE 1.
               88  TSQ-TYP-BLAETTERN                       VALUE 2.
               88  TSQ-TYP-DD-KOMM                         VALUE 3.
               88  TSQ-TYP-LINK-ERR                        VALUE 4.
200197         88  TSQ-TYP-USER-DATEN                      VALUE 5.
bb             88  TSQ-TYP-DIALOG-MODUL                    VALUE 6.
270898         88  TSQ-TYP-buffer-ozp                      VALUE 7.

           05  PI-UPAT0006-EIBTRMID            PIC  X(004).
           05  PI-UPAT0006-PGM-NAME            PIC  X(008).
           05  PI-UPAT0006-USITZG-ANZ          PIC  9(001).
           05  PI-UPAT0006-UANW                PIC  9(001).
200197     05  PI-UPAT0006-DIALOG-FOLGE-NR     PIC  9(002).
200197     05  PI-UPAT0006-PAGE                PIC  9(001).

200197     05  FILLER                          PIC  X(014).
200197*    05  FILLER                          PIC  X(017).

       01  PO-UPAT0006-AUSGABE.
           05  PO-UPAT0006-TSQ-NAME            PIC  X(008).
      *
      *
      ******** ENDE COPY-MEMBER AUFRUF UPAT0006    (DRAT0027) *********
