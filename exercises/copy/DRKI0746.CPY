      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0746.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0746.CPv  $
      *
      *   Rev 2.1   Jun 21 1999 11:15:04   T13EX50
      *Anpassen an die neue Satzstruktur 
      *
      *   Rev 2.0   Apr 15 1999 11:42:54   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:32:50   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   06 Oct 1998 15:35:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *
       01  DRKI0746.
           20  FILLER                  PIC X(188).
      * DTA VORSATZ
       01  CKV-SATZ0.
           20  FILLER                  PIC X(22).
           20  CKV0.
               30  FILLER              PIC X(90).
               30  CKV-KOMPLETTBESTAND PIC X(01).
               30  CKV-WHR-KZ          PIC X(03).
               30  CKV-RESERVE0        PIC X(72).

       01  CKV-SATZE.
           20  FILLER                  PIC X(22).
           20  CKV-SAE                 PIC X(1).
           20  CKVE.
               30 CKV-GESAMT-ZEILEN    PIC 9(7) COMP-3.
               30 CKV-RESERVEE         PIC X(161).

       01  CKV-SATZ1.
           20  CKV-KEY-FELDER.
               25 CKV-KEY-VNR          PIC X(14).
               25 CKV-KEY-UNR          PIC X(04).
               25 CKV-KEY-LFDNR        PIC 9(04).
           20  CKV-KOPFA.
               25  CKV-SA1             PIC X.
               25  CKV-SA              PIC XXX.
               25  CKV-FU              PIC X.
           20  CKV-KOPF.
               25  CKV-GS              PIC 999.
               25  CKV-SPARTE          PIC X.
               25  CKV-VNR             PIC X(14).
               25  CKV-UNR             PIC XXXX.
           20  CKV1.
               30  CKV-SONDBETR        PIC X(8).
               30  CKV-PARTINF         PIC X(15).
               30  CKV-VBEG            PIC S9(7) COMP-3.
               30  CKV-VABL            PIC S9(7) COMP-3.
               30  CKV-VAEND           PIC S9(7) COMP-3.
               30  CKV-ANREDE          PIC X.
               30  CKV-NAME.
               35  CKV-NNAME           PIC X(20).
               35  CKV-VNAME           PIC X(10).
               30  CKV-AUSL            PIC XXX.
               30  CKV-PLZ             PIC X(6).
               30  CKV-ORT             PIC X(25).
               30  CKV-ORTZUKZ         PIC X.
               30  CKV-STRASSE         PIC X(25).
               30  CKV-BERUF           PIC XXXX.
               30  CKV-LAST            PIC X.
               30  CKV-ZAHLWEISE       PIC X.
               30  CKV-PNRVN           PIC 99.
               30  CKV-WHR             PIC X(03).
               30  CKV-RESERVE1        PIC X(02).
       01  CKV-SATZ2.
           20  FILLER                  PIC X(49).
           20  CKV2.
               30  CKV-VORNAME         PIC X(10).
               30  CKV-GEBDAT          PIC S9(7)  COMP-3.
               30  CKV-GESCHL          PIC X.
               30  CKV-BERUF2          PIC X(4).
               30  CKV-PERSBEG         PIC S9(5)  COMP-3.
               30  CKV-PERSABL         PIC S9(5)  COMP-3.
               30  CKV-PERSAEND        PIC S9(7)  COMP-3.
               30  CKV-DSS             PIC XX.
               30  CKV-RESERVE2        PIC X(108).
       01  CKV-SATZ3.
           20  FILLER                  PIC X(49).
           20  CKV3.
               30  CKV-ZUVST           PIC S9(5)  COMP-3.
               30  CKV-ZUERSTELL       PIC S9(7)  COMP-3.
               30  CKV-ABVST           PIC S9(5)  COMP-3.
               30  CKV-ABERSTELL       PIC S9(7)  COMP-3.
               30  CKV-TARNAM          PIC X(8).
               30  CKV-TARFORM         PIC X.
               30  CKV-FILLER          PIC XXXX.
               30  CKV-VLEIST          PIC S9(5)V99 COMP-3.
               30  CKV-SBTEIL          PIC S9(5)V99 COMP-3.
               30  CKV-BEITRAG         PIC S9(5)V99 COMP-3.
               30  CKV-RESERVE3        PIC X(100).
      *  ----------------- COPY ENDE -------------
