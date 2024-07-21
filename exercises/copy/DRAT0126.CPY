      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0126.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0126.CPv  $
      *
      *   Rev 2.3   Oct 07 2015 16:55:06   t13ae34
      *AAN01231: Ges. 16/26: Prüfung Gewerbeberechtigung im Provisi
      *onsdialog
      *
      *   Rev 2.2   Oct 07 2015 16:07:34   t13ae34
      *AAN01231: Ges. 16/26: Prüfung Gewerbeberechtigung im Provisi
      *onsdialog
      *
      *   Rev 2.1   May 07 2008 09:25:48   t13ae34
      *FAN00825: PA11 -> Fehler: Anzeige PIN 7-stellig
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:46:38   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:19:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0126                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN PA12 (ANTRAGSINFOS) UND PA11 (PE-STRUKTUR BEARB.) *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    31.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!31.01.97! 01 ! NEU                       ! SCHÄFER  *
      * FAN00825!06.05.08!2.1 ! PIN 9-STELLIG             ! HEIDER   *
      * AAN01231!05.10.15!2.2 ! VB-ZEICHNER               ! HEIDER   *
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
 
       01  PI-DRAT0126.
           05  PI-DRAT0126-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0126-CADD-I-KOMM-VAR.
 
               10  PI-DRAT0126-FUNKTION               PIC  X(008).
                   88 PI-DRAT0126-CADD-PA12-PA11      VALUE 'DRAT0126'.
 
      ***      STEUERINFOS
               10  PI-DRAT0126-CADD-STEUER-INFO       PIC  X(008).
                   88 PI-DRAT0126-CADD-ERSTAUFRUF     VALUE 'FIRST'.
                   88 PI-DRAT0126-CADD-FOLGEAUFRUF    VALUE 'NEXT'.
 
      ***      TS-QUEUE NAME FüR DATENüBERGABE ZWISCHEN PA11 UND PA12
               10  PI-DRAT0126-CADD-TSQDD-NAME        PIC  X(008).
 
      ***      VSTATUS DES ANTRAGS (AUS WF-ALT)
               10  PI-DRAT0126-CADD-PEWA-VSTAT        PIC  X(004).
 
      ***      DRUCKAUFB. SCHLüSSELFELDER FüR DIE ANZEIGE IN PA11
               10  PI-DRAT0126-CADD-GESELL            PIC  9(002).
               10  PI-DRAT0126-CADD-GESELL-BEZ        PIC  X(010).
               10  PI-DRAT0126-CADD-VNR               PIC  X(015).
060508*        10  PI-DRAT0126-CADD-PIN               PIC  X(007).
060508         10  PI-DRAT0126-CADD-PIN               PIC  X(009).
               10  PI-DRAT0126-CADD-STRUK-DAT         PIC  X(010).
 
      ***      KZ, OB ÄNDERUNGEN DURCHGEFüHRT WERDEN DüRFEN;
      ***      WENN Z.B. ANTRAG STORNIERT SIND ÄND. VERBOTEN
               10  PI-DRAT0126-CADD-AENDERUNGS-KZ     PIC  X(001).
                   88 PI-DRAT0126-CADD-AEND-ERLAUBT     VALUE 'J'.
                   88 PI-DRAT0126-CADD-AEND-VERBOTEN    VALUE 'N'.
 
      ***      HIER WIRD DER ZUSTAND DES ANTRAGES üBERGEBEN
               10  PI-DRAT0126-CADD-ZST-ANTRAG        PIC  X(001).
                   88 PI-DRAT0126-CADD-UNVERAENDERT     VALUE '1'.
                   88 PI-DRAT0126-CADD-VERAEND-OK       VALUE '2'.
      *
051015         10  PI-DRAT0126-CADD-VB-ZEICHNER       PIC  X(007).
051015         10  PI-DRAT0126-CADD-VB-ZEICHNER-P     PIC  X(001).
 
           05  PO-DRAT0126-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0126-CADD-O-KOMM-VAR.
 
      ***      HIER WIRD DER ZUSTAND DER PE-STRUKTUR üBERGEBEN
               10  PO-DRAT0126-CADD-ZST-PE-S          PIC  X(001).
                   88 PO-DRAT0126-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0126-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0126-CADD-UPDATE-JA        VALUE '3'.
 
               10  PO-DRAT0126-CADD-AVERM             PIC  9(007).
               10  PO-DRAT0126-CADD-S-AVERM           PIC  X(001).
               10  PO-DRAT0126-CADD-VM                PIC  9(007).
               10  PO-DRAT0126-CADD-S-VM              PIC  X(001).
051015         10  PO-DRAT0126-CADD-VB-ZEICHNER       PIC  X(007).
051015         10  PO-DRAT0126-CADD-VB-ZEICHNER-P     PIC  X(001).
      ******* ENDE COPY-MEMBER DRAT0126  ****************************
 

