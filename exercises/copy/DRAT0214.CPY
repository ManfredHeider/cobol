      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0214.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0214.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:24   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:47:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0214                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE TRANSAKTION XA14 'ÜBERSICHT NOTIZEN'               *
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
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
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

       01  PI-DRAT0214.
           05  PI-DRAT0214-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0214-CADD-I-KOMM-VAR.


               10  PI-DRAT0214-CADD-COPY-ID           PIC  X(008).
                   88 PI-DRAT0214-CADD-COPY-ID-XA14   VALUE 'DRAT0214'.

      ***      TS-QUEUE NAME FüR DATENüBERGABE DER MAP-KEY-TEXTE
               10  PI-DRAT0214-CADD-TSQDD-NAME        PIC  X(008).

      ***      SCHLüSSELFELD MIT ZUGEHöRIGEN POINTER
               10  PI-DRAT0214-CADD-AUSWAHL-KRIT.
                   15 PI-DRAT0214-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
                   15 PI-DRAT0214-NTZ-PTR-ID         PIC S9(015)
                                                     PACKED-DECIMAL.
                   15 PI-DRAT0214-NTZ-PTR-ST-NR      PIC S9(009) BINARY.
                   15 PI-DRAT0214-NTZ-PTR-PIN        PIC S9(009) BINARY.
                   15 PI-DRAT0214-NTZ-PTR-VNR        PIC  X(015).

       01  PO-DRAT0214.
           05  PO-DRAT0214-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0214-CADD-O-KOMM-VAR.

      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG üBERGEBEN
               10  PO-DRAT0214-CADD-ZST               PIC  X(001).
                   88 PO-DRAT0214-CADD-UNVERAENDERT   VALUE '1'.
                   88 PO-DRAT0214-CADD-VERAEND-OK     VALUE '2'.
                   88 PO-DRAT0214-CADD-LOESCH-OK      VALUE '4'.
                   88 PO-DRAT0214-CADD-ABBR-PF3       VALUE '6'.

      ******* ENDE COPY-MEMBER DRAT0214  ****************************
