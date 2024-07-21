      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UR090S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UR090S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:40:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************************
      *                                                                *
      *        C O P Y M E M B E R    V S A M  -  F E H L E R          *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 1. DATEI --- *
      ******************************************************************
       01  VSAM-FEHLER.
           02 STAT1                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S1-OK                           VALUE '00'.
              88  VSAM-S1-EOF                          VALUE '10'.
              88  VSAM-S1-NOT-FOUND                    VALUE '23'.
              88  VSAM-S1-DUP-KEY                      VALUE '22'.
              88  VSAM-S1-DATA-CHK                     VALUE '30'.
              88  VSAM-S1-BOUND-VIO                    VALUE '34'.
              88  VSAM-S1-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 2. DATEI --- *
      ******************************************************************
           02 STAT2                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S2-OK                           VALUE '00'.
              88  VSAM-S2-EOF                          VALUE '10'.
              88  VSAM-S2-NOT-FOUND                    VALUE '23'.
              88  VSAM-S2-DUP-KEY                      VALUE '22'.
              88  VSAM-S2-DATA-CHK                     VALUE '30'.
              88  VSAM-S2-BOUND-VIO                    VALUE '34'.
              88  VSAM-S2-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 3. DATEI --- *
      ******************************************************************
           02 STAT3                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S3-OK                           VALUE '00'.
              88  VSAM-S3-EOF                          VALUE '10'.
              88  VSAM-S3-NOT-FOUND                    VALUE '23'.
              88  VSAM-S3-DUP-KEY                      VALUE '22'.
              88  VSAM-S3-DATA-CHK                     VALUE '30'.
              88  VSAM-S3-BOUND-VIO                    VALUE '34'.
              88  VSAM-S3-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 4. DATEI --- *
      ******************************************************************
           02 STAT4                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S4-OK                           VALUE '00'.
              88  VSAM-S4-EOF                          VALUE '10'.
              88  VSAM-S4-NOT-FOUND                    VALUE '23'.
              88  VSAM-S4-DUP-KEY                      VALUE '22'.
              88  VSAM-S4-DATA-CHK                     VALUE '30'.
              88  VSAM-S4-BOUND-VIO                    VALUE '34'.
              88  VSAM-S4-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 5. DATEI --- *
      ******************************************************************
           02 STAT5                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S5-OK                           VALUE '00'.
              88  VSAM-S5-EOF                          VALUE '10'.
              88  VSAM-S5-NOT-FOUND                    VALUE '23'.
              88  VSAM-S5-DUP-KEY                      VALUE '22'.
              88  VSAM-S5-DATA-CHK                     VALUE '30'.
              88  VSAM-S5-BOUND-VIO                    VALUE '34'.
              88  VSAM-S5-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 6. DATEI --- *
      ******************************************************************
           02 STAT6                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S6-OK                           VALUE '00'.
              88  VSAM-S6-EOF                          VALUE '10'.
              88  VSAM-S6-NOT-FOUND                    VALUE '23'.
              88  VSAM-S6-DUP-KEY                      VALUE '22'.
              88  VSAM-S6-DATA-CHK                     VALUE '30'.
              88  VSAM-S6-BOUND-VIO                    VALUE '34'.
              88  VSAM-S6-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 7. DATEI --- *
      ******************************************************************
           02 STAT7                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S7-OK                           VALUE '00'.
              88  VSAM-S7-EOF                          VALUE '10'.
              88  VSAM-S7-NOT-FOUND                    VALUE '23'.
              88  VSAM-S7-DUP-KEY                      VALUE '22'.
              88  VSAM-S7-DATA-CHK                     VALUE '30'.
              88  VSAM-S7-BOUND-VIO                    VALUE '34'.
              88  VSAM-S7-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 8. DATEI --- *
      ******************************************************************
           02 STAT8                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S8-OK                           VALUE '00'.
              88  VSAM-S8-EOF                          VALUE '10'.
              88  VSAM-S8-NOT-FOUND                    VALUE '23'.
              88  VSAM-S8-DUP-KEY                      VALUE '22'.
              88  VSAM-S8-DATA-CHK                     VALUE '30'.
              88  VSAM-S8-BOUND-VIO                    VALUE '34'.
              88  VSAM-S8-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL --- 9. DATEI --- *
      ******************************************************************
           02 STAT9                          PIC XX    VALUE 'ZZ'.
              88  VSAM-S9-OK                           VALUE '00'.
              88  VSAM-S9-EOF                          VALUE '10'.
              88  VSAM-S9-NOT-FOUND                    VALUE '23'.
              88  VSAM-S9-DUP-KEY                      VALUE '22'.
              88  VSAM-S9-DATA-CHK                     VALUE '30'.
              88  VSAM-S9-BOUND-VIO                    VALUE '34'.
              88  VSAM-S9-BOUND-VIO1                   VALUE '24'.
      ******************************************************************
      *  STATUS-BYTES FUER VSAM-VERARBEITUNG IN COBOL -- 10. DATEI --- *
      ******************************************************************
           02 STAT10                         PIC XX    VALUE 'ZZ'.
              88  VSAM-S10-OK                          VALUE '00'.
              88  VSAM-S10-EOF                         VALUE '10'.
              88  VSAM-S10-NOT-FOUND                   VALUE '23'.
              88  VSAM-S10-DUP-KEY                     VALUE '22'.
              88  VSAM-S10-DATA-CHK                    VALUE '30'.
              88  VSAM-S10-BOUND-VIO                   VALUE '34'.
              88  VSAM-S10-BOUND-VIO1                  VALUE '24'.
      ******************************************************************
      *                                                                *
      *               E N D E   COPYMEMBER VSAM-FEHLER                 *
      *                                                                *
      ******************************************************************

