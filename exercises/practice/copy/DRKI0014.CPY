      * T13AE38   Stand: 07/10/98    11:48:28
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0014.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI0014.CPv  $
      *
      *   Rev 2.12   Feb 17 2021 13:58:48   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Copies
      *
      *   Rev 2.11   Mar 12 2019 07:37:54   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Sourcen
      *
      *   Rev 2.10   Sep 03 2018 11:28:34   t13ac23
      *AKI01618: Erweiterung des EVWS Bestands-DTAs der AMLeben um
      *ein neues Attribut (Timestamp für WebService) - DTA-Übernahm
      *I-Mapping  - Sourcen
      *
      *   Rev 2.9   Aug 22 2018 10:45:50   t13ac23
      *AKI01618: Erweiterung des EVWS Bestands-DTAs der AMLeben um
      *ein neues Attribut (Timestamp für WebService) - DTA-Übernahm
      *I-Mapping  - Sourcen
      *
      *   Rev 2.8   Aug 22 2018 09:36:32   t13ac23
      *AKI01618: Erweiterung des EVWS Bestands-DTAs der AMLeben um
      *ein neues Attribut (Timestamp für WebService) - DTA-Übernahm
      *I-Mapping  - Sourcen
      *
      *   Rev 2.7   Aug 16 2018 09:33:06   t13ac23
      *AKI01618: Erweiterung des EVWS Bestands-DTAs der AMLeben um
      *ein neues Attribut (Timestamp für WebService) - DTA-Übernahm
      *I-Mapping  - Sourcen
      *
      *   Rev 2.6   Aug 15 2018 12:59:10   t13ac23
      *AKI01618: Erweiterung des EVWS Bestands-DTAs der AMLeben um
      *ein neues Attribut (Timestamp für WebService) - DTA-Übernahm
      *I-Mapping  - Sourcen
      *
      *   Rev 2.5   Jun 23 2016 10:40:56   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.4   Sep 12 2011 10:03:52   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.3   24 Jul 2001 08:23:44   t13ex27
      *AKI00212 AML-Riester-Produkte
      *
      *   Rev 2.2   23 Jul 2001 14:56:58   t13ex27
      *AKI00212 AML-Riester-Produkte
      *
      *   Rev 2.1   Mar 25 1999 09:19:16   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.1   14 May 1998 10:33:16   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:40:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI0124              *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN TEILTYP AML -    *
      *                                                                *
      *    SATZLÄNGE 62                                                *
      *                                                                *
      *----------------------------------------------------------------*
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      *---------!--------!----!---------------------------!------------*
      *KI-NEU   !04.08.97!    !NEUERSTELLUNG              !A.CECCONI   *
      *KI-NEU   !03.10.98!    !Änderung Vorsatztext in    !S.Jost      *
      *         !        !    !Text Änderungsbestand und  !            *
      *         !        !    !Text Komplettbestand!      !            *
      *AKI00212 !20.06.01!2.3 !Feld AML-DYN-PROZKZN       !I.Frenzel   *
      *         !        !    !eingefügt                  !            *
      * AKI01065!10.08.11!2.4 ! Erweiterung um Rechnungs- !I. Frenzel  *
      *         !        !    ! zins                      !            *
020516* AKI01434!02.05.16!2.5 ! NEUE PRODUKTE TR 06/16    !I. FRENZEL  *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
180731* AKI01618!31.07.18!2.6 ! NEUES FELD: STAND-VERTR   !DETMAR      *
      *         !        !2.10! UND ERWEITERUNG SATZLÄNGE !            *
110219* AKI01664!11.02.19!2.11! NEUES FELD: LSTERHPROZ    !Walbert     *
      * AKI01825!04.02.21!2.12! NEUES FELD: BERUFGRP      !T13AC20     *
      * -----------!--------!------------------------------!----------
      * DBO-9994!03.05.22   ! Ausbau Feld STAND-VERT       ! U131381  
      *            !        !                              !          
      * -----------!--------!------------------------------!----------
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
CCC009*    SKIP1
020516*01  DRKI0014.
 
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'P'-SATZ.
      *
               20  'P'-SA                        PIC  X(001).
      *            SATZART        = 'C'
               20  'P'-VNR                       PIC  9(010).
      *            VERTRAGSNR. (ERSTE 5 STELLEN = 0)
               20  'P'-TEILTYP                   PIC  9(002).
      *            LV-TEILTYP-NUMMER
      ***DIE DATENGRUPPE MUSS IN DER FORM BESTEHEN WEGEN 'P'-KZ # !!
               20  'P'-TEILDATEN.
                   30  'P'-DYN-PROZ              PIC  9(002)V9.
      *                ERHÖHUNGSPROZENTSATZ DER DYNAMIKERHÖHUNG
                   30  'P'-DYN-KZ                PIC  9(002).
      *                KENNZEICHEN DYNAMIKSPERRE   99 = J   0,10 = N
200601         20  'P'-DYN-PROZKZN               PIC  9(002).
200601*                ERHÖHUNGSPROZENTSATZ-KENNZEICHEN
020516         20  'P'-DYNWAHL                   PIC  9(002).
>>    *            Dynamikwahl
               20  'P'-ABSICHART                 PIC  9(002).
      *            Absicherungsart
    <<         20  'P'-ABSICHVARIANTE            PIC  9(002).
020516*            Absicherungsvariante
               20  'P'-AKTUAL-KZ                 PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN
100811         20  'P'-RZINS                     PIC  9(003)V99.
100811*            Rechnungszins
110219         20  'P'-LSTERHPROZ                PIC  9(002)V9.
110219*            Leistungserhöhungsprozentsatz
               20  'P'-BERUFGRP                  PIC  9(002).
      *            Kennzeichen Berufsgruppe
               20  'P'-RESERVE                   PIC  X(025).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'P'-VORSATZ     REDEFINES      'P'-SATZ.
      *
               20  'P'-V-SA                      PIC  X(001).
      *            SATZART = 'A'
               20  'P'-V-ABSENDER                PIC  X(003).
      *            ABSENDER = 'AML'
               20  'P'-V-DATUM                   PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  'P'-V-EMPFAENGER              PIC  X(004).
      *            EMPFAENGER = 'DVAG'
031098****     20  'P'-V-TEXT                    PIC  X(012).
031098         20  'P'-V-TEXT                    PIC  X(020).
      *            ÄNDERUNGSBESTAND = 'VERTRAG TEIL'
      *            KOMPLETTBESTAND  = 'VERTRAG TEIL    INIT'
031098***      20  FILLER                        PIC  X(008).
220503         20  FILLER                        PIC  X(026).
      *
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'P'-NACHSATZ    REDEFINES     'P'-SATZ.
      *
               20  'P'-N-SA                      PIC  X(001).
      *            SATZART             'E'
               20  'P'-N-ANZ                     PIC  9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
020516*        20  FILLER                        PIC  X(026).
      *            RESERVE

