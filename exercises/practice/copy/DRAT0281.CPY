      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0281.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0281.CPv  $
      *
      *   Rev 2.5   Jun 19 2012 09:22:06   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:47:06   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Dec 10 2007 08:12:32   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   23 Aug 2007 09:52:02   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.1   May 25 1999 14:38:26   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:03:36   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   13 Jul 1998 09:28:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0281                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN PO15 UND PO17                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    22.12.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.12.98! 01 ! NEU                       ! BOHLINGER*
      * APA00639!21.06.07!2.02! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * APA00622!14.06.07!2.03! ERWEITERUNG UM KNR-VGES   ! STORCH   *
      * APA01101!05.03.12!2.04! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DEN DIALOGEN P023 UND PO26
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
      *
       01  PI-DRAT0281.
           05  PI-DRAT0281-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0281-CADD-I-KOMM-VAR.

               10  PI-DRAT0281-FUNKTION             PIC  X(008).
                   88 PI-DRAT0281-CADD-PO15-PO17      VALUE 'DRAT0281'.

      ***      STEUERINFOS
               10  PI-DRAT0281-CADD-STEUER-INFO     PIC  X(003).
      ***      VERSTRAGSSTRUKTUR-STRUKTUR ÄNDERN BZW. ANZEIGEN
      ***      FüR NORMALE VERTRäGE IST NUR DER ANZEIGE-MODUS MöGLICH
                   88 PI-DRAT0281-CADD-MODUS-AEND   VALUE 'AND'.
                   88 PI-DRAT0281-CADD-MODUS-ANZ    VALUE 'ANZ'.

      ***      DRUCKAUFB. SCHLÜSSELFELDER FÜR DIE ANZEIGE IN PO17
               10  PI-DRAT0281-CADD-GES             PIC  9(002).
               10  PI-DRAT0281-CADD-GES-BM          PIC  X(010).
               10  PI-DRAT0281-CADD-VNR             PIC  X(015).
               10  PI-DRAT0281-CADD-PIN             PIC  X(009).
      *
      ***      HIER WERDEN DIE ID'S ÜBERGEBEN
               10  PI-DRAT0281-CADD-PRINTE          PIC  S9(015)
                                                    PACKED-DECIMAL.
               10  PI-DRAT0281-CADD-KNR-MANDANT     PIC S9(04) BINARY.

       01  PO-DRAT0281.
           05  PO-DRAT0281-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0281-CADD-O-KOMM-VAR.

      ***      HIER WIRD DER RETURNCODE ÜBERGEBEN
               10  PO-DRAT0281-CADD-ZST-PE-S        PIC X(001).
                   88 PO-DRAT0281-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0281-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0281-CADD-VERAEND-NOK      VALUE '3'.
                   88 PO-DRAT0281-CADD-ABBR-PF12        VALUE '4'.
                   88 PO-DRAT0281-CADD-ABBR-PF3         VALUE '5'.
      ******* ENDE COPY-MEMBER DRAT0281  ****************************
