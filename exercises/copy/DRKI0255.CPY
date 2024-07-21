      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0255.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0255.CPv  $
      *
      *   Rev 2.2   15 Sep 2007 10:17:14   t13ae31
      *AAN00753: Erweiterung der PIN auf 9 Stellen KI
      *
      *   Rev 2.1   Sep 30 1999 09:02:20   T13EX41
      *AKI00049
      *-VERARBEITUNG WÄHRUNGSKENN-
      * ZEICHEN ZWECKS EURO-FÄHIG-
      * KEIT                      
      *
      *
      *   Rev 2.0   Mar 02 1999 13:34:12   I13PC48
      *Version 2.0 wegen Umstellung auf NT
      *
      *   Rev 1.0   07 Jul 1998 11:47:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 2 5 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  AUSGABE AUS AUFBEREITUNGSPROGRAMMEN AM-VERS,                  *
      *  COPYSTRECKE FUER ALLGEMEINE VERTRAGSDATEN FUER ALLE ZWEIGEN.  *
      *                                                                *
990913*    SATZLÄNGE   108                                             *
200807*    SATZLÄNGE   110                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.06.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *29.07.98!>Zahlungsart<: Feldtyp geändert, damit !KH Müller! 02  *
      *        !Feldnamen aus Copy-Buch DSKI0368 ver-  !         !     *
      *        !wendet werden können.                  !         !     *
      * 1.09.98!Bei >Zahlungsweise + Vertragszustand<  !KH Müller! 02  *
      *        !Feldtyp auf 'S9..' geändert, damit bei !         !     *
      *        !unbekanntem Wert der Value '-2' einge- !         !     *
      *        !stellt werden kann.                    !         !     *
      *13.09.99!EINFÜHRUNG WÄHRUNGSKENNZEICHEN         !C.HEINECK!     *
      *AKI00049!                                       !         !     *
      *        !                                       !         !     *
200807*AAN00753! PIN-Nr. Erweit. auf 9 Stellen         !Hübner   !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************

       01  DRKI0255.

           05  VS-GES                     PIC  9(002).
           05  VS-VNR                     PIC  9(015).

    ****    05  VS-INT-VNR                 PIC  9(007).
200807     05  VS-INT-VNR                 PIC  9(009).
           05  VS-INT-VNR-AENDKZ          PIC  X(001).
      *        'J' wenn interne Vertragsnummer geändert

           05  VS-INT-VNR-PRUEF           PIC  9(002).
           05  VS-INT-VNR-PRUEF-AENDKZ    PIC  X(001).
      *        'J' wenn Prüfziffer für interne Vertragsnummer geändert

           05  VS-VBNR                    PIC  9(007).
           05  VS-VBNR-AENDKZ             PIC  X(001).
      *        'J' wenn Abschluss-Vermittler (Vermögensberater-Nr.)
      *            geändert

           05  VS-ZAHLWEISE               PIC S9(002).
           05  VS-ZAHLWEISE-AENDKZ        PIC  X(001).
      *        'J' wenn Zahlweise geändert

           05  VS-VERTRAGSZUSTAND         PIC S9(002).
           05  VS-VERTRAGSZUSTAND-AENDKZ  PIC  X(001).
      *        'J' wenn Vertragszustand geändert

           05  VS-AENDDATUM               PIC  X(010).
           05  VS-AENDDATUM-AENDKZ        PIC  X(001).
      *        'J' wenn Vertragszustand geändert

           05  VS-BEITRAG-GESAMT          PIC  9(011)V99.
           05  VS-BEITRAG-GESAMT-AENDKZ   PIC  X(001).
      *        'J' wenn Beitrag Gesamt geändert

990913     05  VS-WAEHRUNG-KZ             PIC  X(001).
990913     05  VS-WAEHRUNG-KZ-AENDKZ      PIC  X(001).
990913*        'J' wenn Währungskennzeichen geändert

           05  VS-VERTRAGSBEGINN          PIC  X(010).
           05  VS-VERTRAGSBEGINN-AENDKZ   PIC  X(001).
      *        'J' wenn Vertragsbeginn geändert

           05  VS-VERTRAGSABLAUF          PIC  X(010).
           05  VS-VERTRAGSABLAUF-AENDKZ   PIC  X(001).
      *        'J' wenn Vertragsablauf geändert

           05  VS-ZAHLUNGSART             PIC S9(004) BINARY.
           05  VS-ZAHLUNGSART-AENDKZ      PIC  X(001).
      *        'J' wenn Zahlungsart geändert

           05  VS-STORNO-DATUM            PIC  X(010).
           05  VS-STORNO-DATUM-AENDKZ     PIC  X(001).
      *        'J' wenn Stornodatum geändert

           05  VS-GEB-VERTR               PIC  X(001).
           05  VS-GEB-VERTR-AENDKZ        PIC  X(001).
      *        'J' wenn Gebündelt-Kz geändert

           05  VS-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

      ******************************************************************
      *          E n d e    Copy-Buch   DRKI0255                       *
      ******************************************************************
