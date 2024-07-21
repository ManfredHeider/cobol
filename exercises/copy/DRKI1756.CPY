      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1756.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1756.CPv  $
      *
      *   Rev 2.4   Sep 04 2020 11:56:02   t13ac20
      *AKI01799: Central: Aufnahme des EBE Beitrags in die Bestands
      *daten - Source
      *
      *   Rev 2.3   Mar 27 2017 09:28:06   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   Apr 28 2009 08:36:26   t13ae31
      *AKI00779: Masch. DTA-Kontrolle CKV
      *
      *   Rev 2.1   Jul 27 2005 13:02:48   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.0   Mar 14 2005 11:05:12   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV                          *
      *    -----------------------------------                         *
      *    SATZLAENGE 344                                              *
      *                                                                *
      *    ANGELEGT 28.02.2005     / HUEBNER                           *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.02.05! Neuerstellung wegen neuer Schnittstelle! Hübner ! 01  *
      *AKI00469!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.04.09  Anpassung neue masch. DTA Kontrolle   ! Hübner  !     *
      *AKI00779!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *FKI01544! FVD entfernt                          ! HHU     !     *
      *--------!---------------------------------------!---------!-----*
      *AKI01799! neues Felder STVTBTG                  ! T13AC20 ! 2.4 *
      *        !              BE-REDUK-BTG             !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
CCC009*     SKIP1
       01  DRKI1756.
      * DTA VORSATZ
           05  CKV-SATZ0.
           20  CKV0.
               30  CKV-SAA             PIC X.
210409***      30  FILLER              PIC X(87).
210409         30  CKV-ABSENDER        PIC X(27).
210409***          'CENTRAL KRANKENVERS KOELN'
210409         30  CKV-ERSTELLDATUM    PIC X(08).
210409***               TTMMJJJJ
210409         30  CKV-DTASTAND        PIC X(08).
210409***               TTMMJJJJ
210409         30  CKV-EMPFAENGER1     PIC X(04).
210409***          'DVAG'
210409***          'ALLF'
210409         30  CKV-EMPFAENGER2     PIC X(23).
210409***          'FFM-RECHENZENTRUM-5.OG'
210409         30  FILLER              PIC X(17).
               30  CKV-KOMPLETTBESTAND PIC X(01).
210409***                 ' 1 = Komplettbestand '
               30  FILLER              PIC X(02).
               30  CKV-WHR-KZ          PIC X(03).
               30  CKV-RESERVE0        PIC X(250).
 
            05  CKV-SATZE REDEFINES CKV-SATZ0.
            20  CKV-SAE                 PIC X(1).
            20  CKVE.
                30 CKV-GESAMT-ZEILEN    PIC 9(7) COMP-3.
                30 CKV-RESERVEE         PIC X(339).
            05  CKV-SATZ1 REDEFINES CKV-SATZ0.
            10  CKV-KEY.
              15  CKV-SA1                 PIC X.
              15  CKV-SA                  PIC XXX.
              15  CKV-VNR                 PIC 9(9).
              15  CKV-PNR                 PIC 9(4).
              15  CKV-PNRLFDNR            PIC 9(4).
              15  CKV-TTARNAM1            PIC X(8).
            10  CKV-REST.
              15  CKV-FU                  PIC X.
              15  CKV-GS                  PIC XXX.
              15  CKV-TVT                 PIC 9(9).
              15  CKV1.
               20  CKV-SONDBETR        PIC X(11).
               20  CKV-PARTINF         PIC X(15).
               20  CKV-VBEG            PIC X(10).
               20  CKV-VABL            PIC X(10).
               20  CKV-VAEND           PIC X(10).
               20  CKV-ANREDE          PIC XX.
               20  CKV-NAME            PIC X(40).
               20  CKV-TITEL           PIC X(25).
               20  CKV-ADELSTITEL      PIC X(20).
               20  CKV-VORNAMEVN       PIC X(25).
               20  CKV-AUSL            PIC XXX.
               20  CKV-PLZ             PIC X(6).
               20  CKV-ORT             PIC X(34).
               20  CKV-ORTZUKZ         PIC X.
               20  CKV-STRASSE         PIC X(40).
               20  CKV-BERUF           PIC X(4).
               20  FILLER              PIC 9(9).
               20  CKV-LAST            PIC X.
               20  CKV-ZAHLWEISE       PIC X.
               20  CKV-PNRVN           PIC 9(4).
               20  CKV-WHR             PIC X(3).
               20  CKV-KUNDENNR        PIC X(10).
               20  CKV-ABVERM          PIC 9(9).
               20  FILLER              PIC X(9).
            05  CKV-SATZ2 REDEFINES CKV-SATZ0.
            10  FILLER                  PIC X(42).
            10  CKV2.
               20  CKV-VORNAME         PIC X(25).
               20  CKV-GEBDAT          PIC X(10).
               20  CKV-GESCHL          PIC X.
               20  CKV-BERUF3          PIC X(4).
               20  FILLER              PIC 9(9).
               20  CKV-PERSBEG         PIC X(10).
               20  CKV-PERSABL         PIC X(10).
               20  CKV-PERSAEND        PIC X(10).
               20  CKV-DSS             PIC 9(4).
               20  CKV-RESERVE2        PIC X(219).
            05  CKV-SATZ3 REDEFINES CKV-SATZ0.
            10  FILLER                  PIC X(42).
            10  CKV3.
               20  CKV-ZUVST           PIC X(10).
               20  CKV-ZUERSTELL       PIC X(10).
               20  CKV-ABVST           PIC X(10).
               20  CKV-TARNAM          PIC X(8).
               20  CKV-TARFORM         PIC X(10).
               20  CKV-VLEIST          PIC S9(9)V99.
               20  CKV-SBTEIL          PIC S9(9)V99.
               20  CKV-BEITRAG         PIC S9(9)V99.
               20  CKV-STVTBTG         PIC X(02).
               20  CKV-BE-REDUK-BTG    PIC S9(9)V99.
               20  CKV-RESERVE3        PIC X(208).


