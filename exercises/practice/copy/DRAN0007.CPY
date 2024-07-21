      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0007.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0007.CPv  $
      *
      *   Rev 2.2   Jul 18 2016 15:40:34   t13ae23
      *AAN01095: Testauftrag VT-Umgebung
      *
      *   Rev 2.1   Jun 23 1999 10:02:58   T13AE23
      *FAN00215 -Korrektur PVCS-Kopf-
      *
      *   Rev 2.0   Feb 27 1999 11:05:30   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   29 Apr 1998 09:18:44   T13AE34
      *AAN00165 -   Statistikdatum
      *
      *   Rev 1.0   29 Jul 1997 08:58:02   T13AE34
      *AFAN0143 - initial put
      *
      *   Rev 1.0   Feb 19 1997 12:21:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0007                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    VON PGM PGAN0030 (bearbeiten VORVERTRAGSNUMMER)            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    SEPTEMBER 1997                                   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!        ! 01 ! NEU                       ! HEIDER   *
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

       01  PI-DRAN0007.
           05  PI-DRAN0007-CADD-I-KOMM-VAR            PIC  X(128).

           05  FILLER REDEFINES PI-DRAN0007-CADD-I-KOMM-VAR.

               10  PI-DRAN0007-KENNUNG                PIC  X(008).
                   88 PI-DRAN0007-VVNR                VALUE 'DRAN0007'.

               10  PI-DRAN0007-FUNKTION               PIC  X(001).
      ***      BEARBEITEN
                   88 PI-DRAN0007-BEARBEITEN          VALUE '1'.
      ***      ANZEIGEN
                   88 PI-DRAN0007-ANZEIGEN            VALUE '2'.

      ***      PRODUKTGEBER
               10  PI-DRAN0007-GESELL                 PIC  9(002).

      ***      PIN
               10  PI-DRAN0007-PIN                    PIC  9(007).

      ***      FILLER
               10  FILLER                             PIC  X(110).


       01  PO-DRAN0007.
           05  PO-DRAN0007-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAN0007-CADD-O-KOMM-VAR.

               10  PO-DRAN0007-KENNUNG                PIC  X(008).
                   88 PO-DRAN0007-VVNR                VALUE 'DRAN0007'.

      ***      WURDEN VVNR ANGELEGT/GEäNDERT/GELöSCHT ?
               10  PO-DRAN0007-UPDATE                 PIC  X(001).
                   88 PO-DRAN0007-UPDATE-NEIN         VALUE SPACE.
                   88 PO-DRAN0007-UPDATE-JA           VALUE '1'.

      ***      FILLER
               10  FILLER                             PIC  X(119).

      ******* ENDE COPY-MEMBER DRAN0007  ****************************

