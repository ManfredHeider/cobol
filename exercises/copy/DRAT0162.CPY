      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0162.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAT0162.CPv  $
      *
      *   Rev 2.4   03 Nov 2006 09:56:22   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Mar 27 2003 10:57:50   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.2   Feb 22 2000 09:04:04   T13AE18
      *apa00080
      *
      *   Rev 2.1   Jan 04 2000 13:05:32   T13AE18
      *APA00068
      *
      *   Rev 2.0   Jun 29 1999 13:52:32   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:21:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0162                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE ÄNDERUNGSTRANSAKTION TTNN                          *
      *    PFLEGE PRODUKT                                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97! 01 ! NEU                       ! BOHLINGER*
      * APA00068!21.10.99! 01 ! PI/PO-BEREICH ERWEITERT   ! BOHLINGER*
      * APA000NN!03.02.00!2.02! PI/PO-BEREICH ERWEITERT   ! BOHLINGER*
      * APA00225!21.03.03!2.03! ERWEITERUNG PG-TARIF      ! STORCH   *
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

       01  PI-DRAT0162.
           05  PI-DRAT0162-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0162-CADD-I-KOMM-VAR.

      *
             10  PI-DRAT0162-COPY-SF                PIC  X(008).
                   88 PI-DRAT0162-COPY-ID-SF        VALUE 'DRAT0162'.
      *
      ***    STEUERINFOS FÜR SONDERFÄLLE BEIM DIALOGAUFRUF
             10  PI-DRAT0162-CADD-STEUER-INFO       PIC  X(008).
211099***           WIRD GESETZT BEI AUFRUF SF41 VON SF42
                 88 PI-DRAT0162-CADD-SF41-SF42      VALUE 'SF41SF42'.
211099***           WIRD GESETZT BEI AUFRUF SF43 VON SF41
                 88 PI-DRAT0162-CADD-SF41-SF43      VALUE 'SF41SF43'.
221199           88 PI-DRAT0162-CADD-SF41-SF44      VALUE 'SF41SF44'.

211099       10  PI-DRAT0162-CADD-MODUS             PIC 9(001).
      ***        SETZEN FÜR BEARBEITUNGSMODUS
                 88 PI-DRAT0162-CADD-MODUS-AENDERN  VALUE 1.
      ***        SETZEN FÜR SCHLUESSELHILFEMODUS
                 88 PI-DRAT0162-CADD-MODUS-SCHL-HI  VALUE 2.
221199***           WIRD GESETZT BEI AUFRUF SF44 VON SF41

             10  PI-DRAT0162-CADD-PGEB-GES        PIC  S9(004).
             10  PI-DRAT0162-CADD-S-PGEB-GES      PIC   X(001).

021106*      10  PI-DRAT0162-CADD-PROD-SCHL       PIC  S9(004).
021106       10  PI-DRAT0162-CADD-PROD-SCHL       PIC  S9(004) BINARY.
             10  PI-DRAT0162-CADD-S-PROD-SCHL     PIC   X(001).

             10  PI-DRAT0162-CADD-PROD-GRP        PIC  S9(004).
             10  PI-DRAT0162-CADD-S-PROD-GRP      PIC   X(001).
      *
             10  PI-DRAT0162-CADD-STICHTAG        PIC  X(010).
             10  PI-DRAT0162-CADD-S-STICHTAG      PIC  X(001).
      *
211099*      10  PI-DRAT0162-CADD-PGTA-TARIF      PIC  X(012).
210303       10  PI-DRAT0162-CADD-PGTA-TARIF      PIC  X(015).
211099       10  PI-DRAT0162-CADD-S-PGTA-TARIF    PIC  X(001).

221199       10  PI-DRAT0162-CADD-DAISY-PGV       PIC  S9(004).
221199       10  PI-DRAT0162-CADD-S-DAISY-PGV     PIC   X(001).
221199
221199       10  PI-DRAT0162-CADD-PGV             PIC   X(008).
221199       10  PI-DRAT0162-CADD-S-PGV           PIC   X(001).

       01  PO-DRAT0162.
           05  PO-DRAT0162-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0162-CADD-O-KOMM-VAR.
      *
      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG ÜBERGEBEN
               10  PO-DRAT0162-CADD-ZST                PIC  X(001).
                   88 PO-DRAT0162-CADD-UNVERAENDERT    VALUE '1'.
                   88 PO-DRAT0162-CADD-VERAEND-OK      VALUE '2'.
                   88 PO-DRAT0162-CADD-VERAENDERT-OK   VALUE '3'.
                   88 PO-DRAT0162-CADD-ABBR-PF3        VALUE '6'.
                   88 PO-DRAT0162-CADD-ABBR-CHG        VALUE '7'.

      ***      HIER WIRD IM SCHLÜSSELHILFEMODUS FÜR DEN SF43 DER
      ***      PRODUKTGEBERTARIF ÜBERGEBEN.
211099         10 PO-DRAT0162-CADD-DATEN.
      ***         PRODUKTSCHLÜSSEL
                  15 PO-DRAT0162-CADD-S-PROD-SCHL      PIC  X(001).
021106*           15 PO-DRAT0162-CADD-PROD-SCHL        PIC  9(002).
021106            15 PO-DRAT0162-CADD-PROD-SCHL      PIC S9(004) BINARY.
                  15 PO-DRAT0162-CADD-PROD-BEZ         PIC  X(040).
      ***         PRODUKTGEBERTARIF
                  15 PO-DRAT0162-CADD-S-PGTA-TARIF     PIC  X(001).
210303*           15 PO-DRAT0162-CADD-PGTA-TARIF       PIC  X(012).
210303            15 PO-DRAT0162-CADD-PGTA-TARIF       PIC  X(015).
      ***         DAISY-PGV
221199            15 PO-DRAT0162-CADD-S-DAISY-PGV      PIC  X(001).
221199            15 PO-DRAT0162-CADD-DAISY-PGV        PIC  9(004).
221199            15 PO-DRAT0162-CADD-DPGV-BEZ         PIC  X(040).
      ***         PRODUKTGEBER-PGV
030200            15 PO-DRAT0162-CADD-S-PGV            PIC  X(001).
030200            15 PO-DRAT0162-CADD-PGV              PIC  X(008).

      ******* ENDE COPY-MEMBER DRAT0162  ****************************
