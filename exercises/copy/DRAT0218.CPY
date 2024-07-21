      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0218.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0218.CPv  $
      *
      *   Rev 2.0   May 27 1999 13:14:08   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 16:03:24   T13EX26
      *apa00033 - Neuerstellung
      *
      *   Rev 1.0   03 Dec 1997 15:47:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0218                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FšR DIE TRANSAKTION XA15 'PFLEGE NOTIZ                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    05.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00020!21.02.98! 01 ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DIALOGEN
      *    - INPUT (VON AUFRUFENDEM DIALOG)
      *      ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *      !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *      !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *      !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *      !!!      WERDEN.
      *      !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *      !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
      *
      ****************************************************************

       01  PI-DRAT0218.
           05  PI-DRAT0218-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0218-CADD-I-KOMM-VAR.


               10  PI-DRAT0218-FUNKTION               PIC  X(008).
                   88 PI-DRAT0218-CADD-XA14-XA15      VALUE 'XA14XA15'.

      ***      TS-QUEUE NAME F?R DATEN?BERGABE ZWISCHEN XA14 UND XA15
               10  PI-DRAT0218-CADD-TSQDD-NAME        PIC  X(008).

      ***      NOTIZ-ID DER GEW„HLTEN NOTIZ
               10  PI-DRAT0218-CADD-KEY.
                   15 PI-DRAT0218-CADD-NTZ-ID         PIC S9(015)
                                                      PACKED-DECIMAL.

      ***      AUSWAHL-KRITERIEN DER NOTIZ, WERDEN BEIM ERSTELLEN
      ***      EINER NEUEN NOTIZ BEN”TIGT
      ***      SCHL?SSELFELDER MIT ZUGEH”RIGEN POINTER
               10  PI-DRAT0218-CADD-AUSWAHL-KRIT.
                   15 PI-DRAT0218-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
                   15 PI-DRAT0218-NTZ-PTR-ID         PIC S9(015)
                                                     PACKED-DECIMAL.
                   15 PI-DRAT0218-NTZ-PTR-ST-NR      PIC S9(009) BINARY.
                   15 PI-DRAT0218-NTZ-PTR-PIN        PIC S9(009) BINARY.
                   15 PI-DRAT0218-NTZ-PTR-VNR        PIC  X(015).

      ***      STEUERINFOS (FC-CODE DER GEW„HLTEN ZEILE)
               10  PI-DRAT0218-CADD-STEUERFELD        PIC  X(001).
                   88 PI-DRAT0218-FC-DETAIL           VALUE 'D'.
                   88 PI-DRAT0218-FC-NEU              VALUE 'N'.
                   88 PI-DRAT0218-FC-LOE              VALUE 'L'.

       01  PO-DRAT0218.
           05  PO-DRAT0218-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0218-CADD-O-KOMM-VAR.
      *
      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG ?BERGEBEN
               10  PO-DRAT0218-CADD-ZST               PIC  X(001).
                   88 PO-DRAT0218-CADD-UNVERAENDERT   VALUE '1'.
                   88 PO-DRAT0218-CADD-VERAEND-OK     VALUE '2'.
                   88 PO-DRAT0218-CADD-UPDATE-OK      VALUE '3'.
                   88 PO-DRAT0218-CADD-LOESCH-OK      VALUE '4'.
                   88 PO-DRAT0218-CADD-INSERT-OK      VALUE '5'.
                   88 PO-DRAT0218-CADD-ABBR-PF3       VALUE '6'.
                   88 PO-DRAT0218-CADD-ABBR-CHG       VALUE '7'.

      ******* ENDE COPY-MEMBER DRAT0218  ****************************
