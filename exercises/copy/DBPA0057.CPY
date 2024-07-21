      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0057
      *   Date Created  03/15/2016
      *   Time Created  08:01:49

      *  Input Data For Map BPA0057
         01 BPA0057I.
            03 FILLER                         PIC X(12).
            03 LBL-GESL                       PIC S9(4) COMP.
            03 LBL-GESF                       PIC X.
            03 FILLER REDEFINES LBL-GESF.
               05 LBL-GESA                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-GESI                       PIC X(13).
            03 EIN-GESL                       PIC S9(4) COMP.
            03 EIN-GESF                       PIC X.
            03 FILLER REDEFINES EIN-GESF.
               05 EIN-GESA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EIN-GESI                       PIC X(2).
            03 LBL-PRDL                       PIC S9(4) COMP.
            03 LBL-PRDF                       PIC X.
            03 FILLER REDEFINES LBL-PRDF.
               05 LBL-PRDA                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-PRDI                       PIC X(17).
            03 EIN-PRDL                       PIC S9(4) COMP.
            03 EIN-PRDF                       PIC X.
            03 FILLER REDEFINES EIN-PRDF.
               05 EIN-PRDA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EIN-PRDI                       PIC X(3).
            03 LBL-EG1L                       PIC S9(4) COMP.
            03 LBL-EG1F                       PIC X.
            03 FILLER REDEFINES LBL-EG1F.
               05 LBL-EG1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-EG1I                       PIC X(50).
            03 LBL-EG2L                       PIC S9(4) COMP.
            03 LBL-EG2F                       PIC X.
            03 FILLER REDEFINES LBL-EG2F.
               05 LBL-EG2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-EG2I                       PIC X(51).
            03 A-DATEGL                       PIC S9(4) COMP.
            03 A-DATEGF                       PIC X.
            03 FILLER REDEFINES A-DATEGF.
               05 A-DATEGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-DATEGI                       PIC X(10).
            03 A-PRVEGL                       PIC S9(4) COMP.
            03 A-PRVEGF                       PIC X.
            03 FILLER REDEFINES A-PRVEGF.
               05 A-PRVEGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-PRVEGI                       PIC X(9).
            03 F000000L                       PIC S9(4) COMP.
            03 F000000F                       PIC X.
            03 FILLER REDEFINES F000000F.
               05 F000000A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000000I                       PIC X(1).
            03 A-VWFEGL                       PIC S9(4) COMP.
            03 A-VWFEGF                       PIC X.
            03 FILLER REDEFINES A-VWFEGF.
               05 A-VWFEGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-VWFEGI                       PIC X(9).
            03 F000001L                       PIC S9(4) COMP.
            03 F000001F                       PIC X.
            03 FILLER REDEFINES F000001F.
               05 F000001A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000001I                       PIC X(1).
            03 A-VTFEGL                       PIC S9(4) COMP.
            03 A-VTFEGF                       PIC X.
            03 FILLER REDEFINES A-VTFEGF.
               05 A-VTFEGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-VTFEGI                       PIC X(9).
            03 F000002L                       PIC S9(4) COMP.
            03 F000002F                       PIC X.
            03 FILLER REDEFINES F000002F.
               05 F000002A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000002I                       PIC X(1).
            03 LBL-AG1L                       PIC S9(4) COMP.
            03 LBL-AG1F                       PIC X.
            03 FILLER REDEFINES LBL-AG1F.
               05 LBL-AG1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-AG1I                       PIC X(50).
            03 LBL-AG2L                       PIC S9(4) COMP.
            03 LBL-AG2F                       PIC X.
            03 FILLER REDEFINES LBL-AG2F.
               05 LBL-AG2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-AG2I                       PIC X(51).
            03 A-DATAGL                       PIC S9(4) COMP.
            03 A-DATAGF                       PIC X.
            03 FILLER REDEFINES A-DATAGF.
               05 A-DATAGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-DATAGI                       PIC X(10).
            03 A-PRVAGL                       PIC S9(4) COMP.
            03 A-PRVAGF                       PIC X.
            03 FILLER REDEFINES A-PRVAGF.
               05 A-PRVAGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-PRVAGI                       PIC X(9).
            03 F000003L                       PIC S9(4) COMP.
            03 F000003F                       PIC X.
            03 FILLER REDEFINES F000003F.
               05 F000003A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000003I                       PIC X(1).
            03 A-VWFAGL                       PIC S9(4) COMP.
            03 A-VWFAGF                       PIC X.
            03 FILLER REDEFINES A-VWFAGF.
               05 A-VWFAGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-VWFAGI                       PIC X(9).
            03 F000004L                       PIC S9(4) COMP.
            03 F000004F                       PIC X.
            03 FILLER REDEFINES F000004F.
               05 F000004A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000004I                       PIC X(1).
            03 A-VTFAGL                       PIC S9(4) COMP.
            03 A-VTFAGF                       PIC X.
            03 FILLER REDEFINES A-VTFAGF.
               05 A-VTFAGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-VTFAGI                       PIC X(9).
            03 F000005L                       PIC S9(4) COMP.
            03 F000005F                       PIC X.
            03 FILLER REDEFINES F000005F.
               05 F000005A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000005I                       PIC X(1).
            03 LBL-GS1L                       PIC S9(4) COMP.
            03 LBL-GS1F                       PIC X.
            03 FILLER REDEFINES LBL-GS1F.
               05 LBL-GS1A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-GS1I                       PIC X(51).
            03 LBL-GS2L                       PIC S9(4) COMP.
            03 LBL-GS2F                       PIC X.
            03 FILLER REDEFINES LBL-GS2F.
               05 LBL-GS2A                       PIC X.
            03 FILLER                         PIC X(1).
            03 LBL-GS2I                       PIC X(26).
            03 A-DATGSL                       PIC S9(4) COMP.
            03 A-DATGSF                       PIC X.
            03 FILLER REDEFINES A-DATGSF.
               05 A-DATGSA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-DATGSI                       PIC X(10).
            03 A-PRVGSL                       PIC S9(4) COMP.
            03 A-PRVGSF                       PIC X.
            03 FILLER REDEFINES A-PRVGSF.
               05 A-PRVGSA                       PIC X.
            03 FILLER                         PIC X(1).
            03 A-PRVGSI                       PIC X(9).
            03 F000006L                       PIC S9(4) COMP.
            03 F000006F                       PIC X.
            03 FILLER REDEFINES F000006F.
               05 F000006A                       PIC X.
            03 FILLER                         PIC X(1).
            03 F000006I                       PIC X(1).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER REDEFINES AFCANZF.
               05 AFCANZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0057
         01 BPA0057O REDEFINES BPA0057I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 LBL-GESH                       PIC X.
            03 LBL-GESO                       PIC X(13).
            03 FILLER                         PIC X(3).
            03 EIN-GESH                       PIC X.
            03 EIN-GESO                       PIC X(2).
            03 FILLER                         PIC X(3).
            03 LBL-PRDH                       PIC X.
            03 LBL-PRDO                       PIC X(17).
            03 FILLER                         PIC X(3).
            03 EIN-PRDH                       PIC X.
            03 EIN-PRDO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 LBL-EG1H                       PIC X.
            03 LBL-EG1O                       PIC X(50).
            03 FILLER                         PIC X(3).
            03 LBL-EG2H                       PIC X.
            03 LBL-EG2O                       PIC X(51).
            03 FILLER                         PIC X(3).
            03 A-DATEGH                       PIC X.
            03 A-DATEGO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 A-PRVEGH                       PIC X.
            03 A-PRVEGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000000H                       PIC X.
            03 F000000O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A-VWFEGH                       PIC X.
            03 A-VWFEGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000001H                       PIC X.
            03 F000001O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A-VTFEGH                       PIC X.
            03 A-VTFEGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000002H                       PIC X.
            03 F000002O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 LBL-AG1H                       PIC X.
            03 LBL-AG1O                       PIC X(50).
            03 FILLER                         PIC X(3).
            03 LBL-AG2H                       PIC X.
            03 LBL-AG2O                       PIC X(51).
            03 FILLER                         PIC X(3).
            03 A-DATAGH                       PIC X.
            03 A-DATAGO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 A-PRVAGH                       PIC X.
            03 A-PRVAGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000003H                       PIC X.
            03 F000003O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A-VWFAGH                       PIC X.
            03 A-VWFAGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000004H                       PIC X.
            03 F000004O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 A-VTFAGH                       PIC X.
            03 A-VTFAGO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000005H                       PIC X.
            03 F000005O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 LBL-GS1H                       PIC X.
            03 LBL-GS1O                       PIC X(51).
            03 FILLER                         PIC X(3).
            03 LBL-GS2H                       PIC X.
            03 LBL-GS2O                       PIC X(26).
            03 FILLER                         PIC X(3).
            03 A-DATGSH                       PIC X.
            03 A-DATGSO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 A-PRVGSH                       PIC X.
            03 A-PRVGSO                       PIC X(9).
            03 FILLER                         PIC X(3).
            03 F000006H                       PIC X.
            03 F000006O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).


