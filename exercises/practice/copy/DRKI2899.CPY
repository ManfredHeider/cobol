      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2899.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2899.CPv  $
      *
      *   Rev 2.2   Mar 27 2017 09:30:52   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.1   Jun 24 2014 11:10:56   t13ex77
      *AKI01305: Deuba - Erweiterung OIF
      *
      *   Rev 2.0   May 12 2014 11:53:56   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
 
      *---------------------------------------------------------------*
      *                                                               *
      *    COPY-MEMBER FÜR DEUTSCHE BANK DTA OIF-KUEMDIGUNG           *
      *    --------------------------------------------------------   *
      *                                                               *
      *    Länge = 100 Bytes                                          *
      *                                                               *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01305!??.06.14! 01 ! Neu                       ! abn       *
      * --------------------------------------------------------------*
      * FKI01544!24.03.17!    ! FVD entfernt              !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *01  'XX'-SATZ.
      *
           10  'XX'-VORSATZ.
      *
               20  'XX'-V-SA                        PIC X(001).
                   88 'XX'-V-SA-OK                    VALUE '1'.
      *            SATZART = '1'
               20  'XX'-V-ABSENDER                  PIC X(008).
                   88 'XX'-V-ABSENDER-OK              VALUE 'DB AG   '.
      *            ABSENDER = 'DB AG'
               20  'XX'-V-DATUM                     PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER                PIC X(004).
                   88 'XX'-V-EMPF-DVAG                VALUE 'DVAG'.
                   88 'XX'-V-EMPF-ALLF                VALUE 'ALLF'.
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT                      PIC X(008).
                   88 'XX'-LIEFER-T                   VALUE 'OIFKUENT'.
      *            LIEFERUNG       TÄGLICH    OIFKUENT
                   88 'XX'-LIEFER-K                   VALUE 'OIFKUENK'.
      *            LIEFERUNG       KOMPLETT   OIFKUENK
               20  'XX'-V-WKZ                       PIC X(003).
                   88 'XX'-V-WKZ-OK                   VALUE 'EUR'.
      *            Währungs-Kennz. =  'EUR'
               20  FILLER                           PIC X(068).
      *            RESERVE
 
 
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                          PIC X(002).
                   88 'XX'-SA-OK                      VALUE '22'.
      *            SATZART        = '22'
               20  'XX'-GES                         PIC 9(002).
                   88 'XX'-GES-OK                     VALUE 48.
      *            GESELLSCHAFT   = '48'
               20  'XX'-KEY.
                 22  'XX'-VNR.
                   25  'XX'-FILIAL-NR               PIC 9(003).
                   25  'XX'-KDNR                    PIC 9(007).
                   25  'XX'-UNTER-DEP-NR            PIC 9(002).
                   25  'XX'-WKZ                     PIC 9(003).
                 22  FILLER   REDEFINES 'XX'-VNR.
                   25  'XX'-VNR-1                   PIC 9(010).
                   25  FILLER                       PIC X(005).
      *            VERTRAGSNUMMER
                 22  'XX'-ISIN                      PIC X(012).
      *            ISIN
                 22  'XX'-DAT-VOR-RUECKZAHL         PIC 9(008).
      *            DATUM VORAUSSICHTLICHER RÜCKZAHLUNGSTERMIN
               20  'XX'-ANTEILE                     PIC 9(013)V9999.
      *            ANZAHL ANTEILE
               20  'XX'-ANTEILE-X REDEFINES 'XX'-ANTEILE.
                 22  'XX'-ANTEILE-X-1               PIC X(002).
                 22  'XX'-ANTEILE-X-2               PIC X(015).
      *            ANZAHL ANTEILE ALS STRING
               20  'XX'-WP-KENN-NR                  PIC X(006).
      *            WERTPAPIERKENNNUMMER
               20  'XX'-DEL-KZ                      PIC X(001).
                   88 'XX'-DEL-LOESCH                   VALUE 'D'.
      *            KENNZEICHEN LÖSCHUNG
                   88 'XX'-DS-SPERRE                    VALUE 'S'.
      *            KENNZEICHEN INFOSPERRE
               20  FILLER                           PIC X(037).
      *            RESERVE
 
 
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA                        PIC X(001).
                   88 'XX'-N-SA-OK                    VALUE '9'.
      *            SATZART             '9'
               20  'XX'-N-ANZ                       PIC 9(007).
      *            ANZAHL SAETZE INKL. VOR- UND NACHSATZ
               20  FILLER                           PIC X(092).
      *            RESERVE

