      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0065.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DBPA0065.CPv  $
      *
      *   Rev 2.7   Aug 11 2015 11:32:28   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.6   Aug 11 2015 09:39:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.5   Jul 22 2015 08:44:44   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.4   Jul 20 2015 16:10:52   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.3   Jul 16 2015 11:18:14   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.1   Jul 16 2015 08:27:14   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.0   Jul 14 2015 11:59:04   t13ex24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0065
      *   Date Created  07/16/2015
      *   Time Created  07:57:43
      *---------------------------------------------------------------*
      *  Input Data For Map BPA0065
         01 BPA0065I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESEBML                       PIC S9(4) COMP.
            03 AGESEBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AGESEBMI                       PIC X(10).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 ADTAARTL                       PIC S9(4) COMP.
            03 ADTAARTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADTAARTI                       PIC X(3).
            03 ADARTNLL                       PIC S9(4) COMP.
            03 ADARTNLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADARTNLI                       PIC X(40).
            03 AUEBDATL                       PIC S9(4) COMP.
            03 AUEBDATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUEBDATI                       PIC X(10).
            03 AFCSFBL                        PIC S9(4) COMP.
            03 AFCSFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSFBI                        PIC X(5).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(50).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 APWAEUKL                       PIC S9(4) COMP.
            03 APWAEUKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEUKI                       PIC X(3).
            03 APWAEU2L                       PIC S9(4) COMP.
            03 APWAEU2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEU2I                       PIC X(3).
            03 DFHG001I                       OCCURS 8 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 APGGEVOL                       PIC S9(4) COMP.
               05 APGGEVOF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APGGEVOI                       PIC X(4).
               05 AANZVERL                       PIC S9(4) COMP.
               05 AANZVERF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANZVERI                       PIC X(6).
               05 APRONVEL                       PIC S9(4) COMP.
               05 APRONVEF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRONVEI                       PIC X(12).
               05 APROUVEL                       PIC S9(4) COMP.
               05 APROUVEF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APROUVEI                       PIC X(12).
               05 AANZNVEL                       PIC S9(4) COMP.
               05 AANZNVEF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANZNVEI                       PIC X(6).
               05 APRONNVL                       PIC S9(4) COMP.
               05 APRONNVF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRONNVI                       PIC X(12).
               05 APROUNVL                       PIC S9(4) COMP.
               05 APROUNVF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APROUNVI                       PIC X(12).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).
 
      *  Output Data For Map BPA0065
         01 BPA0065O REDEFINES BPA0065I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGESA                          PIC X.
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESEBMA                       PIC X.
            03 AGESEBMH                       PIC X.
            03 AGESEBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ADTAARTA                       PIC X.
            03 ADTAARTH                       PIC X.
            03 ADTAARTO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ADARTNLA                       PIC X.
            03 ADARTNLH                       PIC X.
            03 ADARTNLO                       PIC X(40).
            03 FILLER                         PIC X(2).
            03 AUEBDATA                       PIC X.
            03 AUEBDATH                       PIC X.
            03 AUEBDATO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCSFBA                        PIC X.
            03 AFCSFBH                        PIC X.
            03 AFCSFBO                        PIC X(5).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(50).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 APWAEUKA                       PIC X.
            03 APWAEUKH                       PIC X.
            03 APWAEUKO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 APWAEU2A                       PIC X.
            03 APWAEU2H                       PIC X.
            03 APWAEU2O                       PIC X(3).
            03 DFHG001O OCCURS 8.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 APGGEVOA                       PIC X.
               05 APGGEVOH                       PIC X.
               05 APGGEVOO                       PIC X(4).
               05 FILLER                         PIC X(2).
               05 AANZVERA                       PIC X.
               05 AANZVERH                       PIC X.
               05 AANZVERO                       PIC X(6).
               05 FILLER                         PIC X(2).
               05 APRONVEA                       PIC X.
               05 APRONVEH                       PIC X.
               05 APRONVEO                       PIC X(12).
               05 FILLER                         PIC X(2).
               05 APROUVEA                       PIC X.
               05 APROUVEH                       PIC X.
               05 APROUVEO                       PIC X(12).
               05 FILLER                         PIC X(2).
               05 AANZNVEA                       PIC X.
               05 AANZNVEH                       PIC X.
               05 AANZNVEO                       PIC X(6).
               05 FILLER                         PIC X(2).
               05 APRONNVA                       PIC X.
               05 APRONNVH                       PIC X.
               05 APRONNVO                       PIC X(12).
               05 FILLER                         PIC X(2).
               05 APROUNVA                       PIC X.
               05 APROUNVH                       PIC X.
               05 APROUNVO                       PIC X(12).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

