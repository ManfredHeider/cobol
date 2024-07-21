      *   Micro Focus BMS Screen Painter (ver MFBMW 1.1.14)
      *   MapSet Name   BPA0008
      *   Date Created  03/08/1999
      *   Time Created  10:55:46
 
      *  Input Data For Map BPA0008
         01 BPA0008I.
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
            03 APWAEBKL                       PIC S9(4) COMP.
            03 APWAEBKF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEBKI                       PIC X(4).
            03 APWAEB2L                       PIC S9(4) COMP.
            03 APWAEB2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 APWAEB2I                       PIC X(4).
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
               05 AANZVERI                       PIC X(7).
               05 APROBVEL                       PIC S9(4) COMP.
               05 APROBVEF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APROBVEI                       PIC X(16).
               05 AANZNVEL                       PIC S9(4) COMP.
               05 AANZNVEF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AANZNVEI                       PIC X(7).
               05 APROBNVL                       PIC S9(4) COMP.
               05 APROBNVF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APROBNVI                       PIC X(16).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).
 
      *  Output Data For Map BPA0008
         01 BPA0008O REDEFINES BPA0008I.
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
            03 APWAEBKA                       PIC X.
            03 APWAEBKH                       PIC X.
            03 APWAEBKO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 APWAEB2A                       PIC X.
            03 APWAEB2H                       PIC X.
            03 APWAEB2O                       PIC X(4).
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
               05 AANZVERO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 APROBVEA                       PIC X.
               05 APROBVEH                       PIC X.
               05 APROBVEO                       PIC X(16).
               05 FILLER                         PIC X(2).
               05 AANZNVEA                       PIC X.
               05 AANZNVEH                       PIC X.
               05 AANZNVEO                       PIC X(7).
               05 FILLER                         PIC X(2).
               05 APROBNVA                       PIC X.
               05 APROBNVH                       PIC X.
               05 APROBNVO                       PIC X(16).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).
 
 
 

