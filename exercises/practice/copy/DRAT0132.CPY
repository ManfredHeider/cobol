      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0132.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0132.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Dec 1998 13:14:48   T13AE34
      *APA00035 PROGRAMMVERWALTUNG
      *
      *   Rev 1.0   Feb 19 1997 12:20:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0132                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE äNDERUNGSTRANSAKTION ZA11                          *
      *    SONDERDIALOG PFLEGE PROGRAMME                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    08.09.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00035!08.09.97! 01 ! NEU                       ! BOHLINGER*
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

       01  PI-DRAT0132.
           05  PI-DRAT0132-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0132-CADD-I-KOMM-VAR.


               10  PI-DRAT0132-FUNKTION               PIC  X(008).
                   88 PI-DRAT0132-CADD-PV11-ZA11      VALUE 'PV11ZA11'.

      ***      STEUERINFOS
               10  PI-DRAT0132-CADD-FC                PIC  X(001).
                 88 PI-DRAT0132-CADD-FC-DETAIL      VALUE 'D'.
                 88 PI-DRAT0132-CADD-FC-INS         VALUE 'E'.
                 88 PI-DRAT0132-CADD-FC-LOE         VALUE 'L'.

             10  PI-DRAT0132-CADD-KEY.
               15  PI-DRAT0132-CADD-PM-ID           PIC  X(008).

       01  PO-DRAT0132.
           05  PO-DRAT0132-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0132-CADD-O-KOMM-VAR.

      *
      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG üBERGEBEN
               10  PO-DRAT0132-CADD-ZST              PIC  X(001).
                   88 PO-DRAT0132-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0132-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0132-CADD-UPDATE-OK        VALUE '3'.
                   88 PO-DRAT0132-CADD-LOESCH-OK        VALUE '4'.
                   88 PO-DRAT0132-CADD-INSERT-OK        VALUE '5'.
                   88 PO-DRAT0132-CADD-ABBR-PF3         VALUE '6'.
                   88 PO-DRAT0132-CADD-ABBR-CHG         VALUE '7'.



      ******* ENDE COPY-MEMBER DRAT0132  ****************************
