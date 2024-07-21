      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0681.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0681.CPv  $
      *
      *   Rev 2.1   Nov 26 2003 11:31:30   t13ae27
      *Kuendigungsbestaetigung in Bild 2
      *
      *   Rev 2.0   Feb 17 2003 11:00:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  Input Data For Map UU957
         01 UU957I.
            03 FILLER                         PIC X(12).
            03 PNR3L                          PIC S9(4) COMP.
            03 PNR3F                          PIC X.
            03 FILLER REDEFINES PNR3F.
               05 PNR3A                          PIC X.
            03 PNR3I                          PIC X(7).
            03 DAT3L                          PIC S9(4) COMP.
            03 DAT3F                          PIC X.
            03 FILLER REDEFINES DAT3F.
               05 DAT3A                          PIC X.
            03 DAT3I                          PIC X(8).
            03 UHR3L                          PIC S9(4) COMP.
            03 UHR3F                          PIC X.
            03 FILLER REDEFINES UHR3F.
               05 UHR3A                          PIC X.
            03 UHR3I                          PIC X(5).
            03 VBNR3L                         PIC S9(4) COMP.
            03 VBNR3F                         PIC X.
            03 FILLER REDEFINES VBNR3F.
               05 VBNR3A                         PIC X.
            03 VBNR3I                         PIC X(7).
            03 PRZ3L                          PIC S9(4) COMP.
            03 PRZ3F                          PIC X.
            03 FILLER REDEFINES PRZ3F.
               05 PRZ3A                          PIC X.
            03 PRZ3I                          PIC X(1).
            03 DIR3L                          PIC S9(4) COMP.
            03 DIR3F                          PIC X.
            03 FILLER REDEFINES DIR3F.
               05 DIR3A                          PIC X.
            03 DIR3I                          PIC X(3).
            03 ZUNAM3L                        PIC S9(4) COMP.
            03 ZUNAM3F                        PIC X.
            03 FILLER REDEFINES ZUNAM3F.
               05 ZUNAM3A                        PIC X.
            03 ZUNAM3I                        PIC X(20).
            03 VNAM3L                         PIC S9(4) COMP.
            03 VNAM3F                         PIC X.
            03 FILLER REDEFINES VNAM3F.
               05 VNAM3A                         PIC X.
            03 VNAM3I                         PIC X(10).
            03 VBSEML                         PIC S9(4) COMP.
            03 VBSEMF                         PIC X.
            03 FILLER REDEFINES VBSEMF.
               05 VBSEMA                         PIC X.
            03 VBSEMI                         PIC X(5).
            03 IHKZERL                        PIC S9(4) COMP.
            03 IHKZERF                        PIC X.
            03 FILLER REDEFINES IHKZERF.
               05 IHKZERA                        PIC X.
            03 IHKZERI                        PIC X(5).
            03 KOFFER1L                       PIC S9(4) COMP.
            03 KOFFER1F                       PIC X.
            03 FILLER REDEFINES KOFFER1F.
               05 KOFFER1A                       PIC X.
            03 KOFFER1I                       PIC X(5).
            03 VBPRUL                         PIC S9(4) COMP.
            03 VBPRUF                         PIC X.
            03 FILLER REDEFINES VBPRUF.
               05 VBPRUA                         PIC X.
            03 VBPRUI                         PIC X(5).
            03 DAVZERL                        PIC S9(4) COMP.
            03 DAVZERF                        PIC X.
            03 FILLER REDEFINES DAVZERF.
               05 DAVZERA                        PIC X.
            03 DAVZERI                        PIC X(5).
            03 KOFFER2L                       PIC S9(4) COMP.
            03 KOFFER2F                       PIC X.
            03 FILLER REDEFINES KOFFER2F.
               05 KOFFER2A                       PIC X.
            03 KOFFER2I                       PIC X(5).
            03 DBBVBL                         PIC S9(4) COMP.
            03 DBBVBF                         PIC X.
            03 FILLER REDEFINES DBBVBF.
               05 DBBVBA                         PIC X.
            03 DBBVBI                         PIC X(5).
            03 DBBVAL                         PIC S9(4) COMP.
            03 DBBVAF                         PIC X.
            03 FILLER REDEFINES DBBVAF.
               05 DBBVAA                         PIC X.
            03 DBBVAI                         PIC X(5).
            03 MITGLL                         PIC S9(4) COMP.
            03 MITGLF                         PIC X.
            03 FILLER REDEFINES MITGLF.
               05 MITGLA                         PIC X.
            03 MITGLI                         PIC X(1).
            03 ALSEMDL                        PIC S9(4) COMP.
            03 ALSEMDF                        PIC X.
            03 FILLER REDEFINES ALSEMDF.
               05 ALSEMDA                        PIC X.
            03 ALSEMDI                        PIC X(5).
            03 ALSEML                         PIC S9(4) COMP.
            03 ALSEMF                         PIC X.
            03 FILLER REDEFINES ALSEMF.
               05 ALSEMA                         PIC X.
            03 ALSEMI                         PIC X(1).
            03 VBHANDBL                       PIC S9(4) COMP.
            03 VBHANDBF                       PIC X.
            03 FILLER REDEFINES VBHANDBF.
               05 VBHANDBA                       PIC X.
            03 VBHANDBI                       PIC X(5).
            03 BDVPRUL                        PIC S9(4) COMP.
            03 BDVPRUF                        PIC X.
            03 FILLER REDEFINES BDVPRUF.
               05 BDVPRUA                        PIC X.
            03 BDVPRUI                        PIC X(5).
            03 KARSEMDL                       PIC S9(4) COMP.
            03 KARSEMDF                       PIC X.
            03 FILLER REDEFINES KARSEMDF.
               05 KARSEMDA                       PIC X.
            03 KARSEMDI                       PIC X(5).
            03 KARSEML                        PIC S9(4) COMP.
            03 KARSEMF                        PIC X.
            03 FILLER REDEFINES KARSEMF.
               05 KARSEMA                        PIC X.
            03 KARSEMI                        PIC X(1).
            03 HALSTL                         PIC S9(4) COMP.
            03 HALSTF                         PIC X.
            03 FILLER REDEFINES HALSTF.
               05 HALSTA                         PIC X.
            03 HALSTI                         PIC X(5).
            03 RGSEMDL                        PIC S9(4) COMP.
            03 RGSEMDF                        PIC X.
            03 FILLER REDEFINES RGSEMDF.
               05 RGSEMDA                        PIC X.
            03 RGSEMDI                        PIC X(5).
            03 RGSEML                         PIC S9(4) COMP.
            03 RGSEMF                         PIC X.
            03 FILLER REDEFINES RGSEMF.
               05 RGSEMA                         PIC X.
            03 RGSEMI                         PIC X(1).
            03 RAUT1L                         PIC S9(4) COMP.
            03 RAUT1F                         PIC X.
            03 FILLER REDEFINES RAUT1F.
               05 RAUT1A                         PIC X.
            03 RAUT1I                         PIC X(5).
            03 AUFB1TL                        PIC S9(4) COMP.
            03 AUFB1TF                        PIC X.
            03 FILLER REDEFINES AUFB1TF.
               05 AUFB1TA                        PIC X.
            03 AUFB1TI                        PIC X(15).
            03 AUFB1L                         PIC S9(4) COMP.
            03 AUFB1F                         PIC X.
            03 FILLER REDEFINES AUFB1F.
               05 AUFB1A                         PIC X.
            03 AUFB1I                         PIC X(5).
            03 MESSINL                        PIC S9(4) COMP.
            03 MESSINF                        PIC X.
            03 FILLER REDEFINES MESSINF.
               05 MESSINA                        PIC X.
            03 MESSINI                        PIC X(5).
            03 RAUT2L                         PIC S9(4) COMP.
            03 RAUT2F                         PIC X.
            03 FILLER REDEFINES RAUT2F.
               05 RAUT2A                         PIC X.
            03 RAUT2I                         PIC X(5).
            03 PRUEF1TL                       PIC S9(4) COMP.
            03 PRUEF1TF                       PIC X.
            03 FILLER REDEFINES PRUEF1TF.
               05 PRUEF1TA                       PIC X.
            03 PRUEF1TI                       PIC X(15).
            03 PRUEF1L                        PIC S9(4) COMP.
            03 PRUEF1F                        PIC X.
            03 FILLER REDEFINES PRUEF1F.
               05 PRUEF1A                        PIC X.
            03 PRUEF1I                        PIC X(5).
            03 BMAPPEL                        PIC S9(4) COMP.
            03 BMAPPEF                        PIC X.
            03 FILLER REDEFINES BMAPPEF.
               05 BMAPPEA                        PIC X.
            03 BMAPPEI                        PIC X(5).
            03 RAUT3L                         PIC S9(4) COMP.
            03 RAUT3F                         PIC X.
            03 FILLER REDEFINES RAUT3F.
               05 RAUT3A                         PIC X.
            03 RAUT3I                         PIC X(5).
            03 RAUT4L                         PIC S9(4) COMP.
            03 RAUT4F                         PIC X.
            03 FILLER REDEFINES RAUT4F.
               05 RAUT4A                         PIC X.
            03 RAUT4I                         PIC X(5).
            03 AUFB2TL                        PIC S9(4) COMP.
            03 AUFB2TF                        PIC X.
            03 FILLER REDEFINES AUFB2TF.
               05 AUFB2TA                        PIC X.
            03 AUFB2TI                        PIC X(15).
            03 AUFB2L                         PIC S9(4) COMP.
            03 AUFB2F                         PIC X.
            03 FILLER REDEFINES AUFB2F.
               05 AUFB2A                         PIC X.
            03 AUFB2I                         PIC X(5).
            03 RAUT5L                         PIC S9(4) COMP.
            03 RAUT5F                         PIC X.
            03 FILLER REDEFINES RAUT5F.
               05 RAUT5A                         PIC X.
            03 RAUT5I                         PIC X(5).
            03 PRUEF2TL                       PIC S9(4) COMP.
            03 PRUEF2TF                       PIC X.
            03 FILLER REDEFINES PRUEF2TF.
               05 PRUEF2TA                       PIC X.
            03 PRUEF2TI                       PIC X(15).
            03 PRUEF2L                        PIC S9(4) COMP.
            03 PRUEF2F                        PIC X.
            03 FILLER REDEFINES PRUEF2F.
               05 PRUEF2A                        PIC X.
            03 PRUEF2I                        PIC X(5).
            03 GABSTUL                        PIC S9(4) COMP.
            03 GABSTUF                        PIC X.
            03 FILLER REDEFINES GABSTUF.
               05 GABSTUA                        PIC X.
            03 GABSTUI                        PIC X(5).
            03 NADELTL                        PIC S9(4) COMP.
            03 NADELTF                        PIC X.
            03 FILLER REDEFINES NADELTF.
               05 NADELTA                        PIC X.
            03 NADELTI                        PIC X(19).
            03 NADELL                         PIC S9(4) COMP.
            03 NADELF                         PIC X.
            03 FILLER REDEFINES NADELF.
               05 NADELA                         PIC X.
            03 NADELI                         PIC X(3).
            03 GABPRUL                        PIC S9(4) COMP.
            03 GABPRUF                        PIC X.
            03 FILLER REDEFINES GABPRUF.
               05 GABPRUA                        PIC X.
            03 GABPRUI                        PIC X(5).
            03 GUHRL                          PIC S9(4) COMP.
            03 GUHRF                          PIC X.
            03 FILLER REDEFINES GUHRF.
               05 GUHRA                          PIC X.
            03 GUHRI                          PIC X(19).
            03 GUHR1L                         PIC S9(4) COMP.
            03 GUHR1F                         PIC X.
            03 FILLER REDEFINES GUHR1F.
               05 GUHR1A                         PIC X.
            03 GUHR1I                         PIC X(5).
            03 GPENL                          PIC S9(4) COMP.
            03 GPENF                          PIC X.
            03 FILLER REDEFINES GPENF.
               05 GPENA                          PIC X.
            03 GPENI                          PIC X(19).
            03 GPENNRL                        PIC S9(4) COMP.
            03 GPENNRF                        PIC X.
            03 FILLER REDEFINES GPENNRF.
               05 GPENNRA                        PIC X.
            03 GPENNRI                        PIC X(2).
            03 GPENDATL                       PIC S9(4) COMP.
            03 GPENDATF                       PIC X.
            03 FILLER REDEFINES GPENDATF.
               05 GPENDATA                       PIC X.
            03 GPENDATI                       PIC X(5).
            03 CORRL                          PIC S9(4) COMP.
            03 CORRF                          PIC X.
            03 FILLER REDEFINES CORRF.
               05 CORRA                          PIC X.
            03 CORRI                          PIC X(5).
            03 NVBNR3L                        PIC S9(4) COMP.
            03 NVBNR3F                        PIC X.
            03 FILLER REDEFINES NVBNR3F.
               05 NVBNR3A                        PIC X.
            03 NVBNR3I                        PIC X(7).
            03 NMCODE3L                       PIC S9(4) COMP.
            03 NMCODE3F                       PIC X.
            03 FILLER REDEFINES NMCODE3F.
               05 NMCODE3A                       PIC X.
            03 NMCODE3I                       PIC X(18).
            03 NTRANS3L                       PIC S9(4) COMP.
            03 NTRANS3F                       PIC X.
            03 FILLER REDEFINES NTRANS3F.
               05 NTRANS3A                       PIC X.
            03 NTRANS3I                       PIC X(4).
            03 INFO3L                         PIC S9(4) COMP.
            03 INFO3F                         PIC X.
            03 FILLER REDEFINES INFO3F.
               05 INFO3A                         PIC X.
            03 INFO3I                         PIC X(75).

      *  Output Data For Map UU957
         01 UU957O REDEFINES UU957I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 PNR3O                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 DAT3O                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 UHR3O                          PIC X(5).
            03 FILLER                         PIC X(3).
            03 VBNR3O                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 PRZ3O                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 DIR3O                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 ZUNAM3O                        PIC X(20).
            03 FILLER                         PIC X(3).
            03 VNAM3O                         PIC X(10).
            03 FILLER                         PIC X(3).
            03 VBSEMO                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 IHKZERO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 KOFFER1O                       PIC 99.99.
            03 FILLER                         PIC X(3).
            03 VBPRUO                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 DAVZERO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 KOFFER2O                       PIC 99.99.
            03 FILLER                         PIC X(3).
            03 DBBVBO                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 DBBVAO                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 MITGLO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ALSEMDO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 ALSEMO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 VBHANDBO                       PIC 99.99.
            03 FILLER                         PIC X(3).
            03 BDVPRUO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 KARSEMDO                       PIC 99.99.
            03 FILLER                         PIC X(3).
            03 KARSEMO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 HALSTO                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RGSEMDO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RGSEMO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 RAUT1O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 AUFB1TO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 AUFB1O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 MESSINO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RAUT2O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 PRUEF1TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 PRUEF1O                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 BMAPPEO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RAUT3O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RAUT4O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 AUFB2TO                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 AUFB2O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 RAUT5O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 PRUEF2TO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 PRUEF2O                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 GABSTUO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 NADELTO                        PIC X(19).
            03 FILLER                         PIC X(3).
            03 NADELO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 GABPRUO                        PIC 99.99.
            03 FILLER                         PIC X(3).
            03 GUHRO                          PIC X(19).
            03 FILLER                         PIC X(3).
            03 GUHR1O                         PIC 99.99.
            03 FILLER                         PIC X(3).
            03 GPENO                          PIC X(19).
            03 FILLER                         PIC X(3).
            03 GPENNRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 GPENDATO                       PIC 99.99.
            03 FILLER                         PIC X(3).
            03 CORRO                          PIC 99.99.
            03 FILLER                         PIC X(3).
            03 NVBNR3O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 NMCODE3O                       PIC X(18).
            03 FILLER                         PIC X(3).
            03 NTRANS3O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 INFO3O                         PIC X(75).

