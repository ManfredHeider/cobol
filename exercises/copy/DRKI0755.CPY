      * T13EX49   Stand: 15/12/98    12:50:44
      * T13EX12   Stand: 13/09/98    13:52:53
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0755.CPY  $
      *    $Revision:   2.29  $
      *
      *    $Log:   L:/mf/CPY/DRKI0755.CPv  $
      *
      *   Rev 2.29   Mar 30 2020 08:21:42   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.28   Dec 09 2019 14:01:54   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050
      *
      *   Rev 2.27   Feb 24 2017 14:22:26   t13ex78
      *AKI01497: G81 -Antrags-PIN weitergeben
      *
      *
      *   Rev 2.11   Jan 11 2016 15:00:56   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Ä
      *nderungen an PMKI's
      *
      *   Rev 2.10   Sep 24 2013 13:33:38   t13ex19
      *AKI01204: SEPA - DB-Online - AdvoCard
      *
      *   Rev 2.9   Nov 11 2011 14:37:16   t13ex14
      *AKI01079: Anpassung Aufbereitung u. Aktualisierung
      *
      *   Rev 2.8   Jul 21 2009 13:59:04   t13ex14
      *AKI00817: Ausweis von Zusatzinfos: Ermittlung, Aufbereitung,
      * Aktualisierung
      *
      *   Rev 2.7   Feb 26 2009 09:18:30   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.6   Feb 23 2009 08:07:34   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.5   Feb 23 2009 07:46:08   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.4   May 12 2005 08:54:52   t13ae31
      *AKI00465: Vertragsdaten Advocard: Berücksichtigung Selbstbet
      *eiligung.
      *
      *   Rev 2.3   16 Oct 2001 14:09:44   t13ex49
      *FKI00503 - Risikotabelle
      *
      *   Rev 2.2   Dec 27 1999 15:43:50   T13EX49
      *AKI00061 - Währungskennzeichen und
      *8-stelliges Datum
      *
      *   Rev 2.1   May 26 1999 11:01:14   T13EX49
      *AKI00025 - Neuerstellung
      *
      *   Rev 2.0   May 25 1999 17:15:58   T13EX39
      *AKI00025 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:54:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 7 5 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER ADVOCARD          *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: ALLGEMEINE VERTRAGSDATEN                     *
      *                                                                *
      *    SATZLÄNGE   116                                             *
270801*    SATZLÄNGE   468
280205*    SATZLÄNGE   480
190109*    SATZLÄNGE   730
150409*    SATZLÄNGE   880
161215*    SATZLÄNGE   911
      *    SATZLÄNGE  1000                                             *
      *    SATZLÄNGE  1100                                             *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      * 9.12.98!NEUERSTELLUNG                          !WHO      ! 01  *
      *KI-NEU  !                                       !         !     *
151199*AKI00061!Währungskennzeichen wird von AdvoCard  !PPO      ! 02  *
151199*        !geliefert                              !         !     *
      *        !                                       !         !     *
270801*FKI00503!Anpassung RISIKO-Tabellen an DTA       !P.Pohl   !     *
      *        !                                       !         !     *
280205*AKI00465!Selbstbeteilung (Tarif)+ Wagnisüberarb. !P.Hübner !    *
      *        !                                       !         !     *
190109*AKI00642!RISTXT1 ersetzt durch RISTXT und ver-  !W.Misgeld!     *
      *         größert auf 50 Stellen zur Speich. von !         !     *
      *        !RISTXT1 und RISTXT2                    !         !     *
150409* ---------------------------------------------------------------*
150409*AKI00817!15.04.09!    ! neue Felder f. Nachlässe,!W. Misgeld    *
150409*        !        !    ! Bonus u. Schadenfreiheit !              *
      * ---------------------------------------------------------------*
181011*AKI01079!18.10.11!    ! VB-WAG Format geändert   !W. Misgeld    *
      *        !        !    !                          !              *
      * ---------------------------------------------------------------*
080713*AKI01204!08.07.13!    ! Feld f. Sofort-Rabatt    !B. Stoldt     *
      * ---------------------------------------------------------------*
161215*AKI01404!16.12.15!    ! KombiRabatt (aus Kunden- !tfl           *
161215*        !        !    ! bonus abgeleitet bis die !              *
161215*        !        !    ! AdvoCard den DTA erw.    !              *
      * ---------------------------------------------------------------*
210217*AKI01497!21.02.17!    ! Antrags-PIN weitergeben  !tfl           *
      *        !        !    !                          !              *
      *        !        !    !                          !              *
      * ---------------------------------------------------------------*
      *AKI01734!08.11.19!    ! Risikobruttobeitrag      !P. Hübner     *
      *        !        !    ! aktivieren               !              *
      *--------!----------------------------------------!--------------*
      *AKI01766!18.02.20!    !neue Felder Mitarb Rabatt !P. Hübner     *
      *        !        !    !Mitarb ID Nettobeiträge   !              *
      *--------!----------------------------------------!--------------*
      *DBO-8432!20.04.21!    !neue Felder               !P. Hübner     *
      ******************************************************************
       01  DRKI0755.

           05  VB-GES                     PIC  9(02).
           05  VB-VNR                     PIC  9(15).

           05  VB-VBNR                    PIC  9(07).
           05  VB-AEND-VBNR-KZ            PIC  X(01).
      *        J WENN VBNR GEAENDERT

           05  VB-VERTRAGSZUSTAND-DATUM   PIC  X(10).
           05  VB-AEND-VERTRAGSZUSTAND-KZ PIC  X(01).
      *        J WENN VERTRAGSZUSTAND GEAENDERT

           05  VB-AEND                   PIC  X(10).
           05  VB-AEND-AEND-KZ           PIC  X(01).
      *        J WENN VBEG GESAMT GEAENDERT

           05  VB-VBEG                    PIC  X(10).
           05  VB-AEND-VBEG-KZ            PIC  X(01).
      *        J WENN VABL GEAENDERT

           05  VB-VABL                    PIC  X(10).
           05  VB-AEND-VABL-KZ            PIC  X(01).
      *        J WENN VABL GEAENDERT

           05  VB-STORNODAT               PIC  X(10).
           05  VB-AEND-STORNODAT-KZ       PIC  X(01).
      *        J WENN STORNODAT GEAENDERT

           05  VB-ZAHLW                   PIC S9(02).
           05  VB-AEND-ZAHLW-KZ           PIC  X(01).
      *        J WENN ZAHLWEISE GEAENDERT

           05  VB-ZAHLART                 PIC S9(02).
           05  VB-AEND-ZAHLART-KZ         PIC  X(01).
      *        J WENN ZAHLART GEAENDERT

           05  VB-VSU                     PIC  9(09)V99.
           05  VB-AEND-VSU-KZ             PIC  X(01).
      *        J WENN VSU GEAENDERT

           05  VB-PRODUKT-ID              PIC  9(02).

           05  VB-PRAEMIE                 PIC  9(09)V99.
           05  VB-AEND-PRAEMIE-KZ         PIC  X(01).
      *        J WENN PRVBMIE GEAENDERT

           05  VB-FAELM                   PIC  9(02).
           05  VB-AEND-FAELM-KZ           PIC  X(01).
      *        J WENN PRVBMIE GEAENDERT

150409* Vertragsbedingung
150409     05  VB-AVB                       PIC 9(02).
150409     05  VB-AEND-AVB-KZ               PIC X(01).

151199     05  VB-WKZ                     PIC  X(01).
151199     05  VB-AEND-WKZ-KZ             PIC  X(01).
151199*        J WENN PRVBMIE GEAENDERT
151199
150409* Stornogrund
>>         05  VB-STORNOGR                PIC  X(02).
           05  VB-AEND-STORNOGR-KZ        PIC  X(01).
      *        J WENN STORNOGRUND GEAENDERT

      * Bonus u. Nachlässe
           05  VB-KDBONUS                 PIC X(01).
           05  VB-AEND-KDBONUS-KZ         PIC X(01).
      *        J WENN KUNDENBONUS GEAENDERT
           05  VB-DAUERNL                 PIC X(01).
           05  VB-AEND-DAUERNL-KZ         PIC X(01).
      *        J WENN DAUERNACHLASS GEAENDERT
           05  VB-VSPNACH                 PIC X(01).
           05  VB-AEND-VSPNACH-KZ         PIC X(01).
      *        J WENN VSP-NACHLASS GEAENDERT
           05  VB-OEDIENL                 PIC X(01).
    <<     05  VB-AEND-OEDIENL-KZ         PIC X(01).
150409*        J WENN ÖFF. DIENST NACHLASS GEAENDERT
080713     05  VB-SOFORTRAB               PIC X(02).
080713     05  VB-AEND-SOFORTRAB-KZ       PIC X(01).
      *        J wenn Sofort-Rabatt geändert

270801*    05  VB-VARIABEL                OCCURS 5.
270801     05  VB-VARIABEL                OCCURS 10.
181011*        15  VB-WAG                 PIC  X(04).
181011         15  VB-WAG                 PIC  9(05).
               15  VB-AEND-WAG-KZ         PIC  X(01).
      *        J WENN WAG GEAENDERT

190109*        15  VB-RISTXT1             PIC  X(25).
               15  VB-RISBTR              PIC  S9(07)V99 COMP-3.
               15  VB-AEND-RIS-KZ         PIC  X(01).
      *        J WENN Risikobetrag GEAENDERT
190109         15  VB-RISTXT              PIC  X(50).
               15  VB-AEND-RISTXT1-KZ     PIC  X(01).
      *        J WENN RISKTEXT GEAENDERT

150409         15 VB-SELBSTBETEIL         PIC S9(7).
>>             15 VB-AEND-SELBSTBETEIL-KZ PIC  X(1).
      *        J WENN SELBSTBETEIL GEAENDERT
               15 VB-SFRSTUFE             PIC  X(1).
               15 VB-AEND-SFRSTUFE-KZ     PIC  X(1).
      *        J WENN SFRSTUFE GEAENDERT
               15 VB-SFRVARIANTE          PIC  X(1).
    <<         15 VB-AEND-SFRVARIANTE-KZ  PIC  X(1).
150409*        J WENN SFRVARIANTE GEAENDERT
               15  VB-RISBTR-NETTO        PIC  S9(07)V99 COMP-3.
               15  VB-AEND-RISNET-KZ      PIC  X(01).

270801     05  VB-VARIABEL-D              OCCURS 10.
181011*        15  VB-WAG-D               PIC  X(04).
181011         15  VB-WAG-D               PIC  9(05).

280205     05  VB-TARIF                   PIC XXX.
280205     05  VB-AEND-TARIF-KZ                PIC X.

161215     05  VB-KOMBIRABATT             PIC S9(04) COMP.
161215     05  VB-AEND-KOMBIRABATT-KZ     PIC  X(01).
210217     05  VB-PNR                     PIC  9(09) COMP.
210217     05  VB-AEND-PNR-KZ             PIC  X(01).
           05  VB-PRAEMIE-NETTO           PIC  9(09)V99.
           05  VB-AEND-PRAEMIE-NET-KZ     PIC  X(01).
      *        J WENN PRAEMIE GEAENDERT
           05  VB-MITARB-RABATT           PIC  99.
           05  VB-AEND-MITARB-RAB-KZ      PIC  X.
      *        J wenn Rabatt geändert
           05  VB-MITARB-ID               PIC X(12).
           05  VB-AEND-MITARB-ID-KZ       PIC X.
      *        J wenn ID geändert
           05 VB-VERSAND-WEG              PIC 9.    
           05 VB-AEND-VERSAND-WEG-KZ      PIC X.    
      *        J wenn Versandweg geändert           
           05 VB-ANNAHME-DAT              PIC X(10).
           05 VB-AEND-ANNAHME-DAT-KZ      PIC X.    
      *        J wenn Annahme Datum geändert        
           05 VB-UMST-FAEHIGKEIT          PIC X.    
           05 VB-AEND-UMST-FAEHIGKEIT-KZ  PIC X.    
      *        J wenn UMST-FAEHIGKEIT geändert      
           05 FILLER                      PIC X(50).
           
           05  VB-VERARB-KZ               PIC  X(01).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      ******************************************************************



















