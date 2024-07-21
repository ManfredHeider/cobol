      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0128.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0128.CPv  $
      *
      *   Rev 2.1   Jun 25 1999 09:56:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 12:19:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0128                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE ÄNDERUNGSTRANSAKTION XANN                          *
      *    PFLEGE PRODUKTGEBER GESELLSCHAFTEN                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    28.02.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0014!28.02.97! 01 ! NEU                       ! BOHLINGER*
      *         !18.08.97!    ! KORREKTUREN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENäNDERUNGEN        !          *
      *         !05.01.99!    ! KORREKTUR                 ! STORCH   *
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

       01  PI-DRAT0128.
180897*    05  PI-DRAT0128-CADD-I-KOMM-VAR          PIC  X(128).
180897*    05  PI-DRAT0128-CADD-I-KOMM-VAR          PIC  X(139).
050199     05  PI-DRAT0128-CADD-I-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0128-CADD-I-KOMM-VAR.


               10  PI-DRAT0128-FUNKTION               PIC  X(008).
                   88 PI-DRAT0128-CADD-SF21-XA13      VALUE 'DRAT0128'.

      ***      STEUERINFOS
               10  PI-DRAT0126-CADD-STEUER-INFO       PIC  X(003).
                 88 PI-DRAT0128-PGEB-GES-UPD        VALUE 'UPD'.
                 88 PI-DRAT0128-PGEB-GES-INS        VALUE 'INS'.
                 88 PI-DRAT0128-PGEB-GES-LOE        VALUE 'LOE'.
                 88 PI-DRAT0128-PGEB-GES-ANZ        VALUE 'ANZ'.

             10  PI-DRAT0128-CADD-KEY.
               15  PI-DRAT0128-CADD-PGEB-GES        PIC S9(004) BINARY.
180897*        15  PI-DRAT0128-CADD-GES-GUELT-AB    PIC  X(010).
180897         15  PI-DRAT0128-CADD-GES-GUELT-AB    PIC  X(026).
             10  PI-DRAT0128-CADD-DATEN.
               15  PI-DRAT0128-CADD-KTY-GES         PIC  9(002).
               15  PI-DRAT0128-CADD-KNR-LAST-UPD    PIC  X(026).
               15  PI-DRAT0128-CADD-GES-L-BEZ       PIC  X(040).
               15  PI-DRAT0128-CADD-GES-K-BEZ       PIC  X(010).
               15  PI-DRAT0128-CADD-GES-K-BEZ4      PIC  X(004).
050199*        15  PI-DRAT0128-CADD-AUSL            PIC  9(002).
050199*        15  PI-DRAT0128-CADD-AUSL-BEZ        PIC  X(004).
050199*        15  PI-DRAT0128-CADD-BEW-ART         PIC  9(002).
050199*        15  PI-DRAT0128-CADD-BEW-ART-BEZ     PIC  X(010).

       01  PO-DRAT0128.
           05  PO-DRAT0128-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0128-CADD-O-KOMM-VAR.

      *
      ***      HIER WIRD DER ZUSTAND DER VERARBEITUNG üBERGEBEN
               10  PO-DRAT0128-CADD-ZST              PIC  X(001).
                   88 PO-DRAT0128-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0128-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0128-CADD-UPDATE-OK        VALUE '3'.
                   88 PO-DRAT0128-CADD-LOESCH-OK        VALUE '4'.
                   88 PO-DRAT0128-CADD-INSERT-OK        VALUE '5'.
                   88 PO-DRAT0128-CADD-ABBR-PF3         VALUE '6'.



      ******* ENDE COPY-MEMBER DRAT0128  ****************************
