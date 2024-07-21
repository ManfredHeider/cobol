      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0140.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0140.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   04 Jan 1999 17:04:20   T13AE34
      *APA00035 - Dialoge fuer die Programmverwaltung
      *
      *   Rev 1.0   Feb 19 1997 12:21:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0140                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE äNDERUNGSTRANSAKTION ZA12                          *
      *    SONDERDIALOG PFLEGE COPIES                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    06.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00035!06.06.97! 01 ! NEU                       ! BOHLINGER*
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

       01  PI-DRAT0140.
           05  PI-DRAT0140-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0140-CADD-I-KOMM-VAR.


               10  PI-DRAT0140-FUNKTION               PIC  X(008).
                   88 PI-DRAT0140-CADD-PV12-ZA12      VALUE 'PV12ZA12'.
                   88 PI-DRAT0140-CADD-ZA11-ZA12      VALUE 'ZA11ZA12'.

      ***      STEUERINFOS
               10  PI-DRAT0140-CADD-FC                PIC  X(001).
                 88 PI-DRAT0140-CADD-FC-DETAIL      VALUE 'D'.
                 88 PI-DRAT0140-CADD-FC-INS         VALUE 'E'.
                 88 PI-DRAT0140-CADD-FC-LOE         VALUE 'L'.

             10  PI-DRAT0140-CADD-KEY.
               15  PI-DRAT0140-CADD-COPY-ID         PIC  X(008).

             10  PI-DRAT0140-CADD-DATEN.
               15  PI-DRAT0140-CADD-PM-ID           PIC  X(008).

       01  PO-DRAT0140.
           05  PO-DRAT0140-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0140-CADD-O-KOMM-VAR.

      *
      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG üBERGEBEN
               10  PO-DRAT0140-CADD-ZST              PIC  X(001).
                   88 PO-DRAT0140-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0140-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0140-CADD-UPDATE-OK        VALUE '3'.
                   88 PO-DRAT0140-CADD-LOESCH-OK        VALUE '4'.
                   88 PO-DRAT0140-CADD-INSERT-OK        VALUE '5'.
                   88 PO-DRAT0140-CADD-ABBR-PF3         VALUE '6'.
                   88 PO-DRAT0140-CADD-ABBR-CHG         VALUE '7'.



      ******* ENDE COPY-MEMBER DRAT0140  ****************************
