      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.10.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2432.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2432.CPv  $
      *
      *   Rev 2.3   Nov 11 2010 19:05:36   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.2   Nov 11 2010 18:56:38   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.1   Nov 11 2010 18:44:20   t13ex52
      *AKI00968: BADENIA Überarbeitung aller Buchungstextschlüssel
      *
      *   Rev 2.0   Oct 08 2010 10:54:12   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER fuer Aufbereitungsprogramm BADENIA Vertrag und
      *    Umsatz.
      *   - MAPPING BADENIA Buchungstextschluessel ==> DVAG Buchungs-
      *     textschluessel.
      *   - Einlesen ueber VORLAUFKARTE (80 BYTES)
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                       ! NAME      !VERS.*
      *AUFTRAG !                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *01.09.10! Neuerstellung                       ! Schlenther!     *
      *AKI00968!                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      ******************************************************************
         01 DRKI2432-REC.
           05 DRKI2432.
      *       * DVAG Buchungstextschluessel
              10  MAP-DVAG-BUCHUNGS-TXTKEY     PIC 9(03).
      *       * Leerzeichen
              10  MAP-LEERZEICHEN              PIC X(02).
      *       * BADENIA Buchungstextschluessel
              10  MAP-BADENIA-BUCHUNGS-TXTKEY  PIC 9(03).
      *
           05 FILLER                           PIC X(72).

      ******************************************************************
      *      E N D E  Copy-Buch   DRKI2342  Buchungstextschluesselmap  *
      ******************************************************************
