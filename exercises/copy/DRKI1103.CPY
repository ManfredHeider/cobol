      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1103.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1103.CPv  $
      *
      *   Rev 2.1   13 Jul 2000 10:03:04   T13EX30
      *AKI00122
      *Neuerstellung: Key-Anpassung CKVKOMPL
      *wg. Änderung CKV KRTARIFARTBEZ
      *
      *   Rev 2.0   07 Jun 2000 17:08:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV  MIT NEUEM FUEHRENDEM KEY*
      *    -----------------------------------                         *
      *    VARIABLE SATZLAENGE                                         *
      *    MIN = 100 BYTES                                             *
      *    MAX = 220 BYTES                                             *
      *    DAVON KEY = 030 BYTES                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.06.00!NEUERSTELLUNG wg. Key-Verlängerung     !Spooren  ! 2.0 *
      *AKI00122!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
CCC009*    SKIP1
       01  CKV-K-SATZ1.
           10  CKV-K-KEY-FELDER.
              15  CKV-K-KEY-VNR         PIC X(14).
              15  CKV-K-KEY-UNR         PIC X(04).
              15  CKV-K-KEY-LFDNR       PIC 9(04).
              15  CKV-K-KEY-TARNAM      PIC X(8).
           10  CKV-K-KOPFA.
              15  CKV-K-SA1             PIC X.
              15  CKV-K-SA              PIC XXX.
              15  CKV-K-FU              PIC X.
           10  CKV-K-KOPF.
              15  CKV-K-GS              PIC 999.
              15  CKV-K-SPARTE          PIC X.
              15  CKV-K-VNR             PIC X(14).
              15  CKV-K-UNR             PIC XXXX.
           10  CKV1.
              20  CKV-K-SONDBETR        PIC X(8).
              20  CKV-K-PARTINF         PIC X(15).
              20  CKV-K-VBEG            PIC S9(7)       COMP-3.
              20  CKV-K-VABL            PIC S9(7)       COMP-3.
              20  CKV-K-VAEND           PIC S9(7)       COMP-3.
              20  CKV-K-ANREDE          PIC X.
              20  CKV-K-NAME.
              25  CKV-K-NNAME           PIC X(20).
              25  CKV-K-VNAME           PIC X(10).
              20  CKV-K-AUSL            PIC XXX.
              20  CKV-K-PLZ             PIC X(6).
              20  CKV-K-ORT             PIC X(25).
              20  CKV-K-ORTZUKZ         PIC X.
              20  CKV-K-STRASSE         PIC X(25).
              20  CKV-K-BERUF           PIC XXXX.
              20  CKV-K-LAST            PIC X.
              20  CKV-K-ZAHLWEISE       PIC X.
              20  CKV-K-PNRVN           PIC 99.
              20  CKV-K-WHR             PIC X(03).
              20  CKV-K-RESERVE1        PIC X(02).
       01  CKV-K-SATZ2.
           10  FILLER                   PIC X(57).
           10  CKV2.
               20  CKV-K-VORNAME        PIC X(10).
               20  CKV-K-GEBDAT         PIC S9(7)          COMP-3.
               20  CKV-K-GESCHL         PIC X.
               20  CKV-K-BERUF2         PIC X(4).
               20  CKV-K-PERSBEG        PIC S9(5)          COMP-3.
               20  CKV-K-PERSABL        PIC S9(5)          COMP-3.
               20  CKV-K-PERSAEND       PIC S9(7)          COMP-3.
               20  CKV-K-DSS            PIC XX.
               20  CKV-K-RESERVE2       PIC X(108).
       01  CKV-K-SATZ3.
           10  FILLER                   PIC X(22).
           10  CKV-K3-tARNAM            PIC X(8).
           10  FILLER                   PIC X(27).
           10  CKV3.
               20  CKV-K-ZUVST          PIC S9(5)          COMP-3.
               20  CKV-K-ZUERSTELL      PIC S9(7)          COMP-3.
               20  CKV-K-ABVST          PIC S9(5)          COMP-3.
               20  CKV-K-ABERSTELL      PIC S9(7)          COMP-3.
               20  CKV-K-TARNAM         PIC X(8).
               20  CKV-K-TARFORM        PIC X.
               20  CKV-K-FILLER         PIC XXXX.
               20  CKV-K-VLEIST         PIC S9(5)V99       COMP-3.
               20  CKV-K-SBTEIL         PIC S9(5)V99       COMP-3.
               20  CKV-K-BEITRAG        PIC S9(5)V99       COMP-3.
               20  CKV-K-RESERVE3       PIC X(0100).
       01  CKV-K-SATZ.
           10 CKV-K-KEY1                PIC X(022).
           10 CKV-K-KEY2                PIC X(008).
           10 CKV-K-REST                PIC X(166).
      *  ----------------- COPY ENDE -------------
