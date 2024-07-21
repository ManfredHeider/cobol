      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1042.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1042.CPv  $
      *
      *   Rev 2.1   Feb 10 2012 11:25:00   t13ae13
      *AVB01132: VB53 - Rangfrei bearbeiten
      *
      *   Rev 2.0   Jan 25 2012 09:37:34   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1042 - SCHNITTSTELLE FÜR UPVB0178        *
      *    --------------------------------------------------------  *
      *    ERSTELLT: S. Walb                                         *
      *    DATUM:    25.01.2012                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01132!25.01.12!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  EINGABE:                                                    *
      *    FKT      -->  KUENDIGUNG RFT / KL                         *
      *                                                              *
      *  AUSGABE:                                                    *
      *    RC-OK    -->  ALLES OK                                    *
      *    BETR-ED  -->  NEUER BETREUER WÄRE EIN ED                  *
      *    RC-NFND  -->  SATZ NICHT GEFUNDEN                         *
      *    RC-DB2S  -->  FEHLER BEIM SCHREIBMODUL                    *
      *                  - PR-PARAMTER WIRD GEFÜLLT MIT              *
      *                    PARAMETER-RETURN AUS SCHREIBMODUL         *
      ****************************************************************
      *
       01  PI-UPVB0178-EINGABE.
           05  PI-UPVB0178-FKT            PIC  X(002).
               88 PI-UPVB0178-FKT-KUEND-RFT                 VALUE 'RF'.
               88 PI-UPVB0178-FKT-KUEND-KL                  VALUE 'KL'.
           05  PI-UPVB0178-ST-NR          PIC S9(009)    COMP.
           05  PI-UPVB0178-DAT-UPDATE     PIC  X(010).
           05  PI-UPVB0178-PROGRAMM-ID    PIC  X(008).
           05  PI-UPVB0178-PROGRAMM-KZ    PIC  X(001).
           05  PI-UPVB0178-USER-ID        PIC  X(008).
           05  PI-UPVB0178-TERMINAL-ID    PIC  X(008).
       01  PO-UPVB0178-AUSGABE.
           05  PO-UPVB0178-RETURN-CODE    PIC  9.
               88  PO-UPVB0178-RC-OK                        VALUE ZERO.
               88  PO-UPVB0178-RC-BETR-ED                   VALUE  1.
               88  PO-UPVB0178-RC-NFND                      VALUE  2.
               88  PO-UPVB0178-RC-DB2S                      VALUE  3.
           05  PO-UPVB0178-PR-PARAMETER   PIC  X(082).
      *
      ******** ENDE COPY-MEMBER (DRVB1042) *******************
