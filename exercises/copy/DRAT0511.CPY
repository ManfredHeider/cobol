      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0511.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0511.CPv  $
      *
      *   Rev 2.3   Apr 12 2018 09:27:18   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrere "Inhaber" in der Antr
      *agserf.
      *
      *   Rev 2.2   Mar 26 2018 11:16:48   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.1   Mar 20 2018 10:40:30   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.0   Feb 12 2013 13:22:40   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0511                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN ANXX (ANTRAG) UND AN25 (WEITERE INHABER BEARB.)   *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!20.03.18!2.1 ! NEU                       ! RAHNER   *
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

       01  PI-DRAT0511.
           05  PI-DRAT0511-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0511-CADD-I-KOMM-VAR.

               10  PI-DRAT0511-FUNKTION               PIC  X(008).
                   88 PI-DRAT0511-CADD-ANXX-AN25      VALUE 'DRAT0511'.

      ***      STEUERINFOS
               10  PI-DRAT0511-CADD-STEUER-INFO       PIC  X(008).
                   88 PI-DRAT0511-CADD-ERSTAUFRUF     VALUE 'FIRST'.
                   88 PI-DRAT0511-CADD-FOLGEAUFRUF    VALUE 'NEXT'.

      ***      KZ, OB ÄNDERUNGEN DURCHGEFÜHRT WERDEN DÜRFEN;
      ***      WENN Z.B. ANTRAG STORNIERT SIND ÄND. VERBOTEN
               10  PI-DRAT0511-CADD-AENDERUNGS-KZ     PIC  X(001).
                   88 PI-DRAT0511-CADD-AEND-ERLAUBT     VALUE 'J'.
                   88 PI-DRAT0511-CADD-AEND-VERBOTEN    VALUE 'N'.

      ***      HIER WIRD DER ZUSTAND DES ANTRAGES ÜBERGEBEN
               10  PI-DRAT0511-CADD-ZST-ANTRAG        PIC  X(001).
                   88 PI-DRAT0511-CADD-UNVERAENDERT     VALUE '1'.
                   88 PI-DRAT0511-CADD-VERAEND-OK       VALUE '2'.

      ***      TS-QUEUE NAME FÜR DATENÜBERGABE ZWISCHEN
               10  PI-DRAT0511-CADD-TSQDD-NAME        PIC  X(008).

      ***      ANZUZEIGENDE DATEN
               10  PI-DRAT0511-CADD-VB-NR             PIC  X(007).
               10  PI-DRAT0511-CADD-VM-NR             PIC  X(007).
               10  PI-DRAT0511-CADD-VNR               PIC  X(015).

           05  PO-DRAT0511-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0511-CADD-O-KOMM-VAR.

      ***      HIER WIRD DER ZUSTAND DER INHABER ÜBERGEBEN
               10  PO-DRAT0511-CADD-ZST-INHABER      PIC  X(001).
                   88 PO-DRAT0511-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0511-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0511-CADD-UPDATE-JA        VALUE '3'.

               10  PO-DRAT0511-FUNKTION               PIC  X(008).
                   88 PO-DRAT0511-CADD-ANXX-AN25      VALUE 'DRAT0511'.

      ******* ENDE COPY-MEMBER DRAT0511  ****************************




