      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU064S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU064S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Jan 03 1997 14:14:18   T13AE22
      *FEKD0144   Vorzeichen-S bei gepackten Feldern ergänzt
      *
      *   Rev 1.0   Nov 04 1995 09:51:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    28.07.93 10:00:42
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE ROLAND                       *
      *    -------------------------------------                       *
      *                                                                *
      *    LAENGE = 426 BYTES        ERSTELLT AM 15.06.82.             *
      *                              HUEBNER (GEDO)                    *
      *                                                                *
      *    -----------------------------------------------             *
      *    GEAENDERT AM 22.01.86/BIEHL: VARIABLE SATZLAENGE            *
      *    MIN =  430 BYTE                                             *
      *    MAX = 1600 BYTE                                             *
      *                                                                *
      *    -----------------------------------------------             *
      *    GEAENDERT AM 21.10.91/KUHN : PLZ GEPACKTES FORMAT           *
      *                                                                *
      *----------------------------------------------------------------
      *LOGBUCH
      *DATUM   !AUFTRAG ! BESCHREIBUNG               !NAME     !VERSION
      *--------!--------!----------------------------!---------!-------
      *03.01.97!FEKD0144! VORZEICHEN-'S' ERGÄNZT     !SCHÄFER  !  03
      *--------!--------!----------------------------!---------!-------
      *        !        !                            !         !
      *ENDE-LOGBUCH
      *----------------------------------------------------------------
CCC009*    SKIP1
       01  UU064S.
           10  ROL-SATZ.
030197         20  ROL-VNR             PIC S9(11)         COMP-3.
               20  ROL-PNR             PIC X(15).
               20  ROL-ORGANR1         PIC S9(9)          COMP-3.
               20  ROL-ORGANR2         PIC X(8).
               20  ROL-AB              PIC XX.
               20  ROL-BAKKZ           PIC X.
030197         20  ROL-NNR             PIC S9(3)          COMP-3.
               20  ROL-NNRGR           PIC XX.
030197         20  ROL-VBEG            PIC S9(7)          COMP-3.
030197         20  ROL-VABL            PIC S9(7)          COMP-3.
030197         20  ROL-AEND            PIC S9(7)          COMP-3.
030197         20  ROL-RUHVON          PIC S9(7)          COMP-3.
030197         20  ROL-RUHBIS          PIC S9(7)          COMP-3.
               20  ROL-RUHGR           PIC X.
               20  ROL-RUHVKZ          PIC X.
030197         20  ROL-STORNODAT       PIC S9(7)          COMP-3.
               20  ROL-STORNOGR        PIC XX.
               20  ROL-ZAHLW           PIC X.
               20  ROL-FAEL            PIC XX.
               20  ROL-LAST            PIC X.
               20  ROL-AVB             PIC XX.
               20  ROL-TARIF           PIC XX.
               20  ROL-BVTR            PIC X.
               20  ROL-VSU             PIC S9(7)          COMP-3.
               20  ROL-KLAUS           PIC XX            OCCURS 6.
               20  ROL-ADRESSKZ        PIC X.
               20  ROL-ANREDE          PIC X.
               20  ROL-TITEL           PIC X.
               20  ROL-NAME            PIC X(30).
               20  ROL-AUSL            PIC X(3).
               20  ROL-PLZ             PIC S9(7)       COMP-3.
               20  ROL-ORT             PIC X(25).
               20  ROL-STR             PIC X(30).
               20  ROL-ZUPOST          PIC X(30).
030197         20  ROL-GEBD            PIC S9(7)       COMP-3.
               20  ROL-PRAEMIE         PIC S9(8)V9     COMP-3.
               20  ROL-SCHADENTAB      OCCURS 3.
                   30 ROL-SANZ         PIC S9(5)       COMP-3.
                   30 ROL-SDM          PIC S9(9)       COMP-3.
               20  ROL-AKTKZ           PIC X(6).
               20  FILLER              PIC X(36).
030197         20  ROL-ERSTELLDAT      PIC S9(7)       COMP-3.
               20  ROL-RISZAEHL        PIC S9(3)       COMP-3.
               20  ROL-VARIABEL OCCURS 10 DEPENDING ON ROL-RISZAEHL.
030197             30 ROL-LFDNR        PIC S9(3)       COMP-3.
                   30 ROL-LBAPRO       PIC S9(5)       COMP-3.
                   30 ROL-LBATAR       PIC X(2).
030197             30 ROL-LBADAT       PIC S9(7)       COMP-3.
030197             30 ROL-RISBEG       PIC S9(7)       COMP-3.
030197             30 ROL-RISAEND      PIC S9(7)       COMP-3.
                   30 ROL-WAG          PIC X(4).
                   30 ROL-TARIFGR      PIC XX.
                   30 ROL-VTEXT        PIC X(25).
                   30 ROL-BEITRBER     PIC S9(5)       COMP-3.
                   30 ROL-JBTR         PIC S9(6)V9     COMP-3.
                   30 ROL-RISIKO       PIC X(45).
                   30 ROL-ZUABKZ       PIC X.
                   30 ROL-ZUABART      PIC X.
                   30 ROL-ZUABDM       PIC S9(6)V9     COMP-3.
                   30 ROL-VART         PIC X(3).
                   30 FILLER           PIC X(19).

