      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0482.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0482.CPv  $
      *
      *   Rev 2.1   Feb 03 2016 15:00:20   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.0   May 26 1999 17:04:20   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.1   May 26 1999 16:57:56   T13EX25
      *APA00042, Erfassung V2.0
      *
      *
      *   Rev 1.0   23 Jul 1997 12:42:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0482  Tabelle entsprechend PDPT          *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    26.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !26.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      * Apa00012!01.08.98! 1.1! Buchungsgrund & KZ-Einheit! Parent   *
      *         !        !    ! Buchungsreihenfolge hinzu !          *
      * APA02208!03.02.16! 2.1! STEUERJAHR ERGÄNZT        ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       20  PDPT.
           25 PDPT-ID            PIC S9(015) PACKED-DECIMAL.
           25 PMPT-ID            PIC S9(015) PACKED-DECIMAL.
           25 PMPT-VERSION       PIC S9(004) BINARY.
           25 PEST-ID            PIC S9(015) PACKED-DECIMAL.
010898     25 PDPT-BUCH-RFOLGE   PIC S9(004) BINARY.
           25 MA-ABR-DATUM       PIC  X(010).
           25 KNR-BUCHUNGS-STAND PIC S9(004) BINARY.
           25 KNR-BUCHUNGS-ART   PIC S9(004) BINARY.
010898     25 KNR-BUCHUNGS-GRUND PIC S9(004) BINARY.
010898     25 KNR-KZ-EH-MAX      PIC S9(004) BINARY.
           25 KNR-BERECH-WAEHRG  PIC S9(004) BINARY.
           25 DAISY-GEVO         PIC S9(004) BINARY.
           25 PDPT-PRODART       PIC S9(004) BINARY.
           25 FIRST-INSERT       PIC  X(026).
           25 FIRST-KENNUNG      PIC  X(008).
           25 LAST-UPDATE        PIC  X(026).
           25 KENNUNG            PIC  X(008).
           COPY DRPA0483.
030216 20  PDPT-UST.
030216     25 PDPT-UST-JAHR      PIC S9(004) BINARY.

