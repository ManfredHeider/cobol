      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0171.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI0171.CPv  $
      *
      *   Rev 2.10   Nov 24 2020 09:17:16   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.9   Aug 15 2005 12:50:18   t13ex39
      *AKI00501: Anzeige von VL-Investmentkonten bei dit und SEB In
      *vest
      *
      *   Rev 2.8   Jan 27 2003 10:53:02   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.7   Jul 30 2002 16:47:10   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.6   Mar 07 2002 17:43:24   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.5   31 Oct 2000 16:47:34   T13EX12
      *AKI00145 Risikoklasse
      *
      *   Rev 2.4   Dec 01 1999 16:30:04   T13EX39
      *AKI00051 WKZ
      *
      *   Rev 2.3   Nov 25 1999 15:58:04   T13EX39
      *AKI00051    WKZ
      *
      *   Rev 2.2   Nov 25 1999 14:12:20   T13EX39
      *AKI00051    WKZ
      *
      *   Rev 2.1   Nov 23 1999 14:57:34   T13EX39
      *AKI00051    WKZ
      *
      *   Rev 2.0   Feb 27 1999 12:53:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 11:05:06   T13EX12
      *AKI00014 - Aufbereitete Bank-Vertragsdaten
      *
      *   Rev 1.0   29 Sep 1997 11:54:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: ALLGEMEINE VERTRAGSDATEN FUER ALLE BANKEN.   *
      *                                                                *
      *    SATZLÄNGE   116                                             *
      *    SATZLÄNGE   von 116 in 138 geändert (24.11.99)
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *29.05.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *11.09.98!Vorzeichen für Zahlweise/Vertragszust. !V.D.GOLTZ! 01  *
      *KI-NEU  !wg. '-1' '-2' Ausprägungen.            !         !     *
      *        !                                       !         !     *
      *22.09.99! WKZ aufgenommen                       !WHO      ! 01  *
      *AKI00051!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *06.09.00!Risikoklasse und Änd.-KZ für Prod.-ID  !v.d.Goltz!     *
      *AKI00145!aufgenommen.                           !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.11.01!Fax-, Tel- und Online-Revers           !v.d.Goltz!     *
      *AKI00162!aufgenommen.                           !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00274! In DB24-Verträgen kann VB-Nr. geändert!v.d.Goltz!     *
      *        ! werden. Änd.-KZ am Ende der Copystr.  !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *14.11.02! Anlageziel mit aufgenommen.           ! WHO     !
      *AKI00162!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *08.07.05! DEPOT-ART für GHP aufgenommen         ! WHO     !
      *AKI00501!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.11.20! NEUES ATTRIBUT ESAFE                  ! ATAR    !     *
      *AKI01805!                                       !         !
      ******************************************************************
       01  DRKI0171.

           05  VB-GES                     PIC  9(02).
           05  VB-VNR                     PIC  9(15).

           05  VB-INT-VNR                 PIC  9(07).
           05  VB-INT-VNR-PRUEF           PIC  9(02).
           05  VB-VBNR                    PIC  9(07).

           05  VB-ZAHLWEISE               PIC S9(02).
           05  VB-AEND-ZAHLWEISE-KZ       PIC  X(01).
      *        J WENN ZAHLWEISE GEAENDERT

           05  VB-VERTRAGSZUSTAND         PIC S9(02).
           05  VB-VERTRAGSZUSTAND-DATUM   PIC  X(10).
           05  VB-AEND-VERTRAGSZUSTAND-KZ PIC  X(01).
      *        J WENN VERTRAGSZUSTAND GEAENDERT

           05  VB-VAEND-DAT               PIC  X(10).
           05  VB-BEITRAG-GESAMT          PIC  9(11)V99.
           05  VB-AEND-BEITRAG-GESAMT-KZ  PIC  X(01).
      *        J WENN BEITRAG GESAMT GEAENDERT

           05  VB-NACHLASS-KZ             PIC  X(01).
           05  VB-AEND-NACHLASS-KZ        PIC  X(01).
      *        J WENN NACHLASS-KZ GEAENDERT

           05  VB-VERTRAGSBEGINN          PIC  X(10).
           05  VB-AEND-VERTRAGSBEGINN-KZ  PIC  X(01).
      *        J WENN VERTRAGSBEGINN GEAENDERT

           05  VB-VERTRAGSABLAUF          PIC  X(10).
           05  VB-AEND-VERTRAGSABLAUF-KZ  PIC  X(01).
      *        J WENN VERTRAGSABLAUF GEAENDERT

           05  VB-PRODUKT-ID              PIC  9(02).
           05  VB-VL-KZ                   PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG

           05  VB-VERFUEGUNGS-KZ          PIC  X(02).
           05  VB-AEND-VERFUEGUNGS-KZ     PIC  X(01).
      *        J WENN VERFUEGUNGS-KZ GEAENDERT

           05  VB-GUTHABEN                PIC S9(09)V99.
           05  VB-AEND-GUTHABEN-KZ        PIC  X(01).
      *        J WENN GUTHABEN GEAENDERT

241199     05  VB-WKZ                     PIC  X(01).
241199     05  VB-AEND-WKZ-KZ             PIC  X(01).
      *        J WENN WÄHRUNGSKENNZEICHEN GEAENDERT

060900     05  VB-RISIKOKLASSE            PIC  X(01).
...   *        Risikoklasse 0 - 9
...        05  VB-AEND-RISIKO-KZ          PIC  X(01).
...   *        J WENN Risikoklasse GEAENDERT

...        05  VB-AEND-PRODUKT-ID-KZ      PIC  X(01).
...   *        J wenn Produkt-ID GEAENDERT

151101     05  VB-FAXREVERS               PIC X(001).
...   *        Faxrevers = 'J' oder 'N'
...        05  VB-TELREVERS               PIC X(001).
...   *        Telefonrevers = 'J' oder 'N'
...        05  VB-ONLINEREVERS            PIC X(001).
...   *        Onlinerevers = 'J' oder 'N'
...        05  VB-AEND-REVERS-KZ          PIC X(001).
151101*        J Wenn einer der 3 Revers-Felder geändert

160402     05  VB-AEND-VBNR-KZ            PIC X(001).

141102     05  VB-ANLAGE-ZIEL             PIC X(001).

080705     05  VB-DEPOT-ART               PIC  X(01).

           05  VB-ESAFE                   PIC  X(01).
 
           05  FILLER                     PIC  X(09).
080705*    05  FILLER                     PIC  X(11).
080705*    05  FILLER                     PIC  X(10).

           05  VB-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      ******************************************************************

