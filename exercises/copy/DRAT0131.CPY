      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0131.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0131.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 12 1997 10:57:14   T13EX11
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:20:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0073                                      *
      *    --------------------------------------------------------   *
      *    SATZBESCHREIBUNG: DIALOG-PROTOKOLL-FUNKTION MIT ST-NR      *
      *    (ERSTELLT IM UPAT0013)                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *    DATUM:    16.04.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0339!16.04.97! 01 ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   HINWEIS:
      *
tst   *   DRAT0131-SATZ IST DIE STRUKTUR FÜR EINE  E S D S - DATEI.
tst   *   DER  R B A -VORSATZ IST NUR FÜR TESTZWECKE AUF DER WORKBENCH
tst   *   ERFORDERLICH, DAMIT MIT 'DRAT0131-EXPANDET' IM FILE-EDITOR
tst   *   EINE STRUKTUR ZUR VERFÜGUNG STEHT
      *
       01  DRAT0131-EXPANDED.
tst   *    05  DRAT0131-RBA                      PIC S9(009) BINARY.
           05  DRAT0131-SATZ.
      *      TRANS: CICS-TRANSAKTION
               10  DRAT0131-TRANS                PIC  X(004).
      *      USER-ID: RACF-ID
               10  DRAT0131-USERID               PIC  X(007).
      *      ST-NR: MEISTENS DIE VB-NR
               10  DRAT0131-ST-NR                PIC S9(009) BINARY.
      *      TIMESTAMP: ZEITP. (DER PROTOKOLLIERUNG) DER DIALOGAUSKUNFT
               10  DRAT0131-TIMESTAMP            PIC  X(026).
      *
      ******** ENDE COPY-MEMBER **************************************

