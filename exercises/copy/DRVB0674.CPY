      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0674.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0674.CPv  $
      *
      *   Rev 2.2   Nov 26 2003 11:31:08   t13ae27
      *Kuendigungsbestaetigung in Bild 2
      *
      *   Rev 2.1   Sep 04 2003 13:06:46   t13ex22
      *AVB00701:Dialog VB44 - Pflege Insolvenzkonten; 
      *Anzeige BV-Schutz in UU11 und VB21
      *
      *   Rev 2.0   Feb 17 2003 10:59:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFER25)
      *   MapSet Name   UU956
      *   Date Created  09/02/2003
      *   Time Created  15:11:20

      *  Input Data For Map UU956
         01 UU956I.
            03 FILLER                         PIC X(12).
            03 PNR2L                          PIC S9(4) COMP.
            03 PNR2F                          PIC X.
            03 FILLER REDEFINES PNR2F.
               05 PNR2A                          PIC X.
            03 PNR2I                          PIC X(7).
            03 DAT2L                          PIC S9(4) COMP.
            03 DAT2F                          PIC X.
            03 FILLER REDEFINES DAT2F.
               05 DAT2A                          PIC X.
            03 DAT2I                          PIC X(8).
            03 UHR2L                          PIC S9(4) COMP.
            03 UHR2F                          PIC X.
            03 FILLER REDEFINES UHR2F.
               05 UHR2A                          PIC X.
            03 UHR2I                          PIC X(5).
            03 VBNR2L                         PIC S9(4) COMP.
            03 VBNR2F                         PIC X.
            03 FILLER REDEFINES VBNR2F.
               05 VBNR2A                         PIC X.
            03 VBNR2I                         PIC X(7).
            03 PRZ2L                          PIC S9(4) COMP.
            03 PRZ2F                          PIC X.
            03 FILLER REDEFINES PRZ2F.
               05 PRZ2A                          PIC X.
            03 PRZ2I                          PIC X(1).
            03 DIR2L                          PIC S9(4) COMP.
            03 DIR2F                          PIC X.
            03 FILLER REDEFINES DIR2F.
               05 DIR2A                          PIC X.
            03 DIR2I                          PIC X(3).
            03 ZUNAM2L                        PIC S9(4) COMP.
            03 ZUNAM2F                        PIC X.
            03 FILLER REDEFINES ZUNAM2F.
               05 ZUNAM2A                        PIC X.
            03 ZUNAM2I                        PIC X(20).
            03 VORNAM2L                       PIC S9(4) COMP.
            03 VORNAM2F                       PIC X.
            03 FILLER REDEFINES VORNAM2F.
               05 VORNAM2A                       PIC X.
            03 VORNAM2I                       PIC X(10).
            03 STR2TL                         PIC S9(4) COMP.
            03 STR2TF                         PIC X.
            03 FILLER REDEFINES STR2TF.
               05 STR2TA                         PIC X.
            03 STR2TI                         PIC X(7).
            03 AUSL2TL                        PIC S9(4) COMP.
            03 AUSL2TF                        PIC X.
            03 FILLER REDEFINES AUSL2TF.
               05 AUSL2TA                        PIC X.
            03 AUSL2TI                        PIC X(4).
            03 STR2L                          PIC S9(4) COMP.
            03 STR2F                          PIC X.
            03 FILLER REDEFINES STR2F.
               05 STR2A                          PIC X.
            03 STR2I                          PIC X(20).
            03 AUSL2L                         PIC S9(4) COMP.
            03 AUSL2F                         PIC X.
            03 FILLER REDEFINES AUSL2F.
               05 AUSL2A                         PIC X.
            03 AUSL2I                         PIC X(3).
            03 PLZ2TL                         PIC S9(4) COMP.
            03 PLZ2TF                         PIC X.
            03 FILLER REDEFINES PLZ2TF.
               05 PLZ2TA                         PIC X.
            03 PLZ2TI                         PIC X(17).
            03 PLZ2L                          PIC S9(4) COMP.
            03 PLZ2F                          PIC X.
            03 FILLER REDEFINES PLZ2F.
               05 PLZ2A                          PIC X.
            03 PLZ2I                          PIC X(6).
            03 ORT2L                          PIC S9(4) COMP.
            03 ORT2F                          PIC X.
            03 FILLER REDEFINES ORT2F.
               05 ORT2A                          PIC X.
            03 ORT2I                          PIC X(16).
            03 PB2L                           PIC S9(4) COMP.
            03 PB2F                           PIC X.
            03 FILLER REDEFINES PB2F.
               05 PB2A                           PIC X.
            03 PB2I                           PIC X(3).
            03 TEL2TL                         PIC S9(4) COMP.
            03 TEL2TF                         PIC X.
            03 FILLER REDEFINES TEL2TF.
               05 TEL2TA                         PIC X.
            03 TEL2TI                         PIC X(6).
            03 TEL2L                          PIC S9(4) COMP.
            03 TEL2F                          PIC X.
            03 FILLER REDEFINES TEL2F.
               05 TEL2A                          PIC X.
            03 TEL2I                          PIC X(14).
            03 AFAXNR2L                       PIC S9(4) COMP.
            03 AFAXNR2F                       PIC X.
            03 FILLER REDEFINES AFAXNR2F.
               05 AFAXNR2A                       PIC X.
            03 AFAXNR2I                       PIC X(20).
            03 INSTTL                         PIC S9(4) COMP.
            03 INSTTF                         PIC X.
            03 FILLER REDEFINES INSTTF.
               05 INSTTA                         PIC X.
            03 INSTTI                         PIC X(12).
            03 INSTL                          PIC S9(4) COMP.
            03 INSTF                          PIC X.
            03 FILLER REDEFINES INSTF.
               05 INSTA                          PIC X.
            03 INSTI                          PIC X(22).
            03 BLZTL                          PIC S9(4) COMP.
            03 BLZTF                          PIC X.
            03 FILLER REDEFINES BLZTF.
               05 BLZTA                          PIC X.
            03 BLZTI                          PIC X(14).
            03 BLZL                           PIC S9(4) COMP.
            03 BLZF                           PIC X.
            03 FILLER REDEFINES BLZF.
               05 BLZA                           PIC X.
            03 BLZI                           PIC X(8).
            03 KTONRL                         PIC S9(4) COMP.
            03 KTONRF                         PIC X.
            03 FILLER REDEFINES KTONRF.
               05 KTONRA                         PIC X.
            03 KTONRI                         PIC X(11).
            03 ABVSCHUL                       PIC S9(4) COMP.
            03 ABVSCHUF                       PIC X.
            03 FILLER REDEFINES ABVSCHUF.
               05 ABVSCHUA                       PIC X.
            03 ABVSCHUI                       PIC X(14).
            03 KTOINHTL                       PIC S9(4) COMP.
            03 KTOINHTF                       PIC X.
            03 FILLER REDEFINES KTOINHTF.
               05 KTOINHTA                       PIC X.
            03 KTOINHTI                       PIC X(13).
            03 KTOINHL                        PIC S9(4) COMP.
            03 KTOINHF                        PIC X.
            03 FILLER REDEFINES KTOINHF.
               05 KTOINHA                        PIC X.
            03 KTOINHI                        PIC X(25).
            03 PCNRT1L                        PIC S9(4) COMP.
            03 PCNRT1F                        PIC X.
            03 FILLER REDEFINES PCNRT1F.
               05 PCNRT1A                        PIC X.
            03 PCNRT1I                        PIC X(7).
            03 PCNRT2L                        PIC S9(4) COMP.
            03 PCNRT2F                        PIC X.
            03 FILLER REDEFINES PCNRT2F.
               05 PCNRT2A                        PIC X.
            03 PCNRT2I                        PIC X(7).
            03 PCNRT3L                        PIC S9(4) COMP.
            03 PCNRT3F                        PIC X.
            03 FILLER REDEFINES PCNRT3F.
               05 PCNRT3A                        PIC X.
            03 PCNRT3I                        PIC X(7).
            03 PCNR1L                         PIC S9(4) COMP.
            03 PCNR1F                         PIC X.
            03 FILLER REDEFINES PCNR1F.
               05 PCNR1A                         PIC X.
            03 PCNR1I                         PIC X(5).
            03 PCNR2L                         PIC S9(4) COMP.
            03 PCNR2F                         PIC X.
            03 FILLER REDEFINES PCNR2F.
               05 PCNR2A                         PIC X.
            03 PCNR2I                         PIC X(5).
            03 PCNR3L                         PIC S9(4) COMP.
            03 PCNR3F                         PIC X.
            03 FILLER REDEFINES PCNR3F.
               05 PCNR3A                         PIC X.
            03 PCNR3I                         PIC X(7).
            03 PCNRT4L                        PIC S9(4) COMP.
            03 PCNRT4F                        PIC X.
            03 FILLER REDEFINES PCNRT4F.
               05 PCNRT4A                        PIC X.
            03 PCNRT4I                        PIC X(7).
            03 PCNR4L                         PIC S9(4) COMP.
            03 PCNR4F                         PIC X.
            03 FILLER REDEFINES PCNR4F.
               05 PCNR4A                         PIC X.
            03 PCNR4I                         PIC X(7).
            03 VBSEITTL                       PIC S9(4) COMP.
            03 VBSEITTF                       PIC X.
            03 FILLER REDEFINES VBSEITTF.
               05 VBSEITTA                       PIC X.
            03 VBSEITTI                       PIC X(7).
            03 VBSEITL                        PIC S9(4) COMP.
            03 VBSEITF                        PIC X.
            03 FILLER REDEFINES VBSEITF.
               05 VBSEITA                        PIC X.
            03 VBSEITI                        PIC X(8).
            03 ANLAGB1L                       PIC S9(4) COMP.
            03 ANLAGB1F                       PIC X.
            03 FILLER REDEFINES ANLAGB1F.
               05 ANLAGB1A                       PIC X.
            03 ANLAGB1I                       PIC X(8).
            03 ANLAGB2L                       PIC S9(4) COMP.
            03 ANLAGB2F                       PIC X.
            03 FILLER REDEFINES ANLAGB2F.
               05 ANLAGB2A                       PIC X.
            03 ANLAGB2I                       PIC X(7).
            03 ANLAGB3L                       PIC S9(4) COMP.
            03 ANLAGB3F                       PIC X.
            03 FILLER REDEFINES ANLAGB3F.
               05 ANLAGB3A                       PIC X.
            03 ANLAGB3I                       PIC X(3).
            03 ANLAGB4L                       PIC S9(4) COMP.
            03 ANLAGB4F                       PIC X.
            03 FILLER REDEFINES ANLAGB4F.
               05 ANLAGB4A                       PIC X.
            03 ANLAGB4I                       PIC X(7).
            03 VERTRTL                        PIC S9(4) COMP.
            03 VERTRTF                        PIC X.
            03 FILLER REDEFINES VERTRTF.
               05 VERTRTA                        PIC X.
            03 VERTRTI                        PIC X(14).
            03 VERT1L                         PIC S9(4) COMP.
            03 VERT1F                         PIC X.
            03 FILLER REDEFINES VERT1F.
               05 VERT1A                         PIC X.
            03 VERT1I.
               05 VERTR1I                        PIC X(8).
               05 VERTZ1I                        PIC X(3).
            03 VERTNEUL                       PIC S9(4) COMP.
            03 VERTNEUF                       PIC X.
            03 FILLER REDEFINES VERTNEUF.
               05 VERTNEUA                       PIC X.
            03 VERTNEUI                       PIC X(3).
            03 VERT2L                         PIC S9(4) COMP.
            03 VERT2F                         PIC X.
            03 FILLER REDEFINES VERT2F.
               05 VERT2A                         PIC X.
            03 VERT2I.
               05 VERTR2I                        PIC X(8).
               05 VERTZ2I                        PIC X(3).
            03 BESCHRL                        PIC S9(4) COMP.
            03 BESCHRF                        PIC X.
            03 FILLER REDEFINES BESCHRF.
               05 BESCHRA                        PIC X.
            03 BESCHRI                        PIC X(15).
            03 BESCHR1L                       PIC S9(4) COMP.
            03 BESCHR1F                       PIC X.
            03 FILLER REDEFINES BESCHR1F.
               05 BESCHR1A                       PIC X.
            03 BESCHR1I                       PIC X(20).
            03 ZUVERTL                        PIC S9(4) COMP.
            03 ZUVERTF                        PIC X.
            03 FILLER REDEFINES ZUVERTF.
               05 ZUVERTA                        PIC X.
            03 ZUVERTI                        PIC X(9).
            03 ZUVER1L                        PIC S9(4) COMP.
            03 ZUVER1F                        PIC X.
            03 FILLER REDEFINES ZUVER1F.
               05 ZUVER1A                        PIC X.
            03 ZUVER1I                        PIC X(2).
            03 ZUVER2L                        PIC S9(4) COMP.
            03 ZUVER2F                        PIC X.
            03 FILLER REDEFINES ZUVER2F.
               05 ZUVER2A                        PIC X.
            03 ZUVER2I                        PIC X(8).
            03 ZUVERBL                        PIC S9(4) COMP.
            03 ZUVERBF                        PIC X.
            03 FILLER REDEFINES ZUVERBF.
               05 ZUVERBA                        PIC X.
            03 ZUVERBI                        PIC X(1).
            03 ZUVER3L                        PIC S9(4) COMP.
            03 ZUVER3F                        PIC X.
            03 FILLER REDEFINES ZUVER3F.
               05 ZUVER3A                        PIC X.
            03 ZUVER3I                        PIC X(2).
            03 ZUVERVL                        PIC S9(4) COMP.
            03 ZUVERVF                        PIC X.
            03 FILLER REDEFINES ZUVERVF.
               05 ZUVERVA                        PIC X.
            03 ZUVERVI                        PIC X(1).
            03 ZUVKTXL                        PIC S9(4) COMP.
            03 ZUVKTXF                        PIC X.
            03 FILLER REDEFINES ZUVKTXF.
               05 ZUVKTXA                        PIC X.
            03 ZUVKTXI                        PIC X(2).
            03 ZUVERKL                        PIC S9(4) COMP.
            03 ZUVERKF                        PIC X.
            03 FILLER REDEFINES ZUVERKF.
               05 ZUVERKA                        PIC X.
            03 ZUVERKI                        PIC X(2).
            03 KUENDBTL                       PIC S9(4) COMP.
            03 KUENDBTF                       PIC X.
            03 FILLER REDEFINES KUENDBTF.
               05 KUENDBTA                       PIC X.
            03 KUENDBTI                       PIC X(18).
            03 KUENDBL                        PIC S9(4) COMP.
            03 KUENDBF                        PIC X.
            03 FILLER REDEFINES KUENDBF.
               05 KUENDBA                        PIC X.
            03 KUENDBI                        PIC X(1).
            03 SCHUFTL                        PIC S9(4) COMP.
            03 SCHUFTF                        PIC X.
            03 FILLER REDEFINES SCHUFTF.
               05 SCHUFTA                        PIC X.
            03 SCHUFTI                        PIC X(11).
            03 SCHUF1L                        PIC S9(4) COMP.
            03 SCHUF1F                        PIC X.
            03 FILLER REDEFINES SCHUF1F.
               05 SCHUF1A                        PIC X.
            03 SCHUF1I                        PIC X(8).
            03 FUEHZGL                        PIC S9(4) COMP.
            03 FUEHZGF                        PIC X.
            03 FILLER REDEFINES FUEHZGF.
               05 FUEHZGA                        PIC X.
            03 FUEHZGI                        PIC X(8).
            03 GEWSCHL                        PIC S9(4) COMP.
            03 GEWSCHF                        PIC X.
            03 FILLER REDEFINES GEWSCHF.
               05 GEWSCHA                        PIC X.
            03 GEWSCHI                        PIC X(79).
            03 NVBNR2L                        PIC S9(4) COMP.
            03 NVBNR2F                        PIC X.
            03 FILLER REDEFINES NVBNR2F.
               05 NVBNR2A                        PIC X.
            03 NVBNR2I                        PIC X(7).
            03 NMCODE2L                       PIC S9(4) COMP.
            03 NMCODE2F                       PIC X.
            03 FILLER REDEFINES NMCODE2F.
               05 NMCODE2A                       PIC X.
            03 NMCODE2I                       PIC X(18).
            03 NTRANS2L                       PIC S9(4) COMP.
            03 NTRANS2F                       PIC X.
            03 FILLER REDEFINES NTRANS2F.
               05 NTRANS2A                       PIC X.
            03 NTRANS2I                       PIC X(4).
            03 INFO2L                         PIC S9(4) COMP.
            03 INFO2F                         PIC X.
            03 FILLER REDEFINES INFO2F.
               05 INFO2A                         PIC X.
            03 INFO2I                         PIC X(75).

      *  Output Data For Map UU956
         01 UU956O REDEFINES UU956I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 PNR2O                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 DAT2O                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 UHR2O                          PIC X(5).
            03 FILLER                         PIC X(3).
            03 VBNR2O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 PRZ2O                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 DIR2O                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 ZUNAM2O                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 VORNAM2O                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 STR2TO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AUSL2TO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 STR2O                          PIC X(20).
            03 FILLER                         PIC X(3).
            03 AUSL2O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 PLZ2TO                         PIC X(17).
            03 FILLER                         PIC X(3).
            03 PLZ2O                          PIC X(6).
            03 FILLER                         PIC X(3).
            03 ORT2O                          PIC X(16).
            03 FILLER                         PIC X(3).
            03 PB2O                           PIC X(3).
            03 FILLER                         PIC X(3).
            03 TEL2TO                         PIC X(6).
            03 FILLER                         PIC X(3).
            03 TEL2O                          PIC X(14).
            03 FILLER                         PIC X(3).
            03 AFAXNR2O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 INSTTO                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 INSTO                          PIC X(22).
            03 FILLER                         PIC X(3).
            03 BLZTO                          PIC X(14).
            03 FILLER                         PIC X(3).
            03 BLZO                           PIC Z(8).
            03 FILLER                         PIC X(3).
            03 KTONRO                         PIC Z(11).
            03 FILLER                         PIC X(3).
            03 ABVSCHUO                       PIC X(14).
            03 FILLER                         PIC X(3).
            03 KTOINHTO                       PIC X(13).
            03 FILLER                         PIC X(3).
            03 KTOINHO                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 PCNRT1O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 PCNRT2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 PCNRT3O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 PCNR1O                         PIC Z(5).
            03 FILLER                         PIC X(3).
            03 PCNR2O                         PIC Z(5).
            03 FILLER                         PIC X(3).
            03 PCNR3O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 PCNRT4O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 PCNR4O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 VBSEITTO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 VBSEITO                        PIC 99.99.99.
            03 FILLER                         PIC X(3).
            03 ANLAGB1O                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 ANLAGB2O                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 ANLAGB3O                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ANLAGB4O                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 VERTRTO                        PIC X(14).
            03 FILLER                         PIC X(3).
            03 VERT1O.
               05 VERTR1O                        PIC 99.99.99.
               05 VERTZ1O                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 VERTNEUO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 VERT2O.
               05 VERTR2O                        PIC X(8).
               05 VERTZ2O                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 BESCHRO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 BESCHR1O                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 ZUVERTO                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 ZUVER1O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ZUVER2O                        PIC 99.99.99.
            03 FILLER                         PIC X(3).
            03 ZUVERBO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ZUVER3O                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ZUVERVO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ZUVKTXO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ZUVERKO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 KUENDBTO                       PIC X(18).
            03 FILLER                         PIC X(3).
            03 KUENDBO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 SCHUFTO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 SCHUF1O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 FUEHZGO                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 GEWSCHO                        PIC X(79).
            03 FILLER                         PIC X(3).
            03 NVBNR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 NMCODE2O                       PIC X(18).
            03 FILLER                         PIC X(3).
            03 NTRANS2O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 INFO2O                         PIC X(75).

