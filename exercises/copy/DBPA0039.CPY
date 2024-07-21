      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BPA0039
      *   Date Created  08/09/2018
      *   Time Created  15:06:42

      *  Input Data For Map BPA0039
         01 BPA0039I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(9).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 EUNRL                          PIC S9(4) COMP.
            03 EUNRF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EUNRI                          PIC X(3).
            03 ESCHICHL                       PIC S9(4) COMP.
            03 ESCHICHF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESCHICHI                       PIC X(3).
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
            03 EPRODSLL                       PIC S9(4) COMP.
            03 EPRODSLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPRODSLI                       PIC X(3).
            03 APRODBML                       PIC S9(4) COMP.
            03 APRODBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 APRODBMI                       PIC X(10).
            03 EFAEVTTL                       PIC S9(4) COMP.
            03 EFAEVTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEVTTI                       PIC X(2).
            03 EFAEVMML                       PIC S9(4) COMP.
            03 EFAEVMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEVMMI                       PIC X(2).
            03 EFAEVJJL                       PIC S9(4) COMP.
            03 EFAEVJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEVJJI                       PIC X(4).
            03 EFAEBTTL                       PIC S9(4) COMP.
            03 EFAEBTTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEBTTI                       PIC X(2).
            03 EFAEBMML                       PIC S9(4) COMP.
            03 EFAEBMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEBMMI                       PIC X(2).
            03 EFAEBJJL                       PIC S9(4) COMP.
            03 EFAEBJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFAEBJJI                       PIC X(4).
            03 EFOLGNRL                       PIC S9(4) COMP.
            03 EFOLGNRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFOLGNRI                       PIC X(3).
            03 EABRVMML                       PIC S9(4) COMP.
            03 EABRVMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRVMMI                       PIC X(2).
            03 EABRVJJL                       PIC S9(4) COMP.
            03 EABRVJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRVJJI                       PIC X(4).
            03 EABRBMML                       PIC S9(4) COMP.
            03 EABRBMMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRBMMI                       PIC X(2).
            03 EABRBJJL                       PIC S9(4) COMP.
            03 EABRBJJF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EABRBJJI                       PIC X(4).
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
            03 AFCSANZI                       PIC X(17).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(25).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 9 TIMES.
               05 EFCL                           PIC S9(4) COMP.
               05 EFCF                           PIC X.
               05 FILLER                         PIC X(1).
               05 EFCI                           PIC X(1).
               05 AZINFOL                        PIC S9(4) COMP.
               05 AZINFOF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AZINFOI                        PIC X(2).
               05 APRODSLL                       PIC S9(4) COMP.
               05 APRODSLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODSLI                       PIC X(3).
               05 ANSTKZL                        PIC S9(4) COMP.
               05 ANSTKZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ANSTKZI                        PIC X(2).
               05 AFAEVONL                       PIC S9(4) COMP.
               05 AFAEVONF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVONI                       PIC X(10).
               05 APRSCHLL                       PIC S9(4) COMP.
               05 APRSCHLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRSCHLI                       PIC X(5).
               05 ABWHKFTL                       PIC S9(4) COMP.
               05 ABWHKFTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABWHKFTI                       PIC X(1).
               05 ABWSCHLL                       PIC S9(4) COMP.
               05 ABWSCHLF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABWSCHLI                       PIC X(6).
               05 ATARIKZL                       PIC S9(4) COMP.
               05 ATARIKZF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ATARIKZI                       PIC X(2).
               05 AKVBTXTL                       PIC S9(4) COMP.
               05 AKVBTXTF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AKVBTXTI                       PIC X(29).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0039
         01 BPA0039O REDEFINES BPA0039I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 EGESA                          PIC X.
            03 EGESH                          PIC X.
            03 EGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(9).
            03 FILLER                         PIC X(2).
            03 EVNRA                          PIC X.
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(2).
            03 EUNRA                          PIC X.
            03 EUNRH                          PIC X.
            03 EUNRO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 ESCHICHA                       PIC X.
            03 ESCHICHH                       PIC X.
            03 ESCHICHO                       PIC X(3).
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
            03 EPRODSLA                       PIC X.
            03 EPRODSLH                       PIC X.
            03 EPRODSLO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 APRODBMA                       PIC X.
            03 APRODBMH                       PIC X.
            03 APRODBMO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 EFAEVTTA                       PIC X.
            03 EFAEVTTH                       PIC X.
            03 EFAEVTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EFAEVMMA                       PIC X.
            03 EFAEVMMH                       PIC X.
            03 EFAEVMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EFAEVJJA                       PIC X.
            03 EFAEVJJH                       PIC X.
            03 EFAEVJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EFAEBTTA                       PIC X.
            03 EFAEBTTH                       PIC X.
            03 EFAEBTTO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EFAEBMMA                       PIC X.
            03 EFAEBMMH                       PIC X.
            03 EFAEBMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EFAEBJJA                       PIC X.
            03 EFAEBJJH                       PIC X.
            03 EFAEBJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EFOLGNRA                       PIC X.
            03 EFOLGNRH                       PIC X.
            03 EFOLGNRO                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 EABRVMMA                       PIC X.
            03 EABRVMMH                       PIC X.
            03 EABRVMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRVJJA                       PIC X.
            03 EABRVJJH                       PIC X.
            03 EABRVJJO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EABRBMMA                       PIC X.
            03 EABRBMMH                       PIC X.
            03 EABRBMMO                       PIC X(2).
            03 FILLER                         PIC X(2).
            03 EABRBJJA                       PIC X.
            03 EABRBJJH                       PIC X.
            03 EABRBJJO                       PIC X(4).
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
            03 AFCSANZO                       PIC X(17).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(25).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 9.
               05 FILLER                         PIC X(2).
               05 EFCA                           PIC X.
               05 EFCH                           PIC X.
               05 EFCO                           PIC X(1).
               05 FILLER                         PIC X(2).
               05 AZINFOA                        PIC X.
               05 AZINFOH                        PIC X.
               05 AZINFOO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 APRODSLA                       PIC X.
               05 APRODSLH                       PIC X.
               05 APRODSLO                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 ANSTKZA                        PIC X.
               05 ANSTKZH                        PIC X.
               05 ANSTKZO                        PIC X(2).
               05 FILLER                         PIC X(2).
               05 AFAEVONA                       PIC X.
               05 AFAEVONH                       PIC X.
               05 AFAEVONO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 APRSCHLA                       PIC X.
               05 APRSCHLH                       PIC X.
               05 APRSCHLO                       PIC X(5).
               05 FILLER                         PIC X(2).
               05 ABWHKFTA                       PIC X.
               05 ABWHKFTH                       PIC X.
               05 ABWHKFTO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ABWSCHLA                       PIC X.
               05 ABWSCHLH                       PIC X.
               05 ABWSCHLO                       PIC X(6).
               05 FILLER                         PIC X(2).
               05 ATARIKZA                       PIC X.
               05 ATARIKZH                       PIC X.
               05 ATARIKZO                       PIC X(2).
               05 FILLER                         PIC X(2).
               05 AKVBTXTA                       PIC X.
               05 AKVBTXTH                       PIC X.
               05 AKVBTXTO                       PIC X(29).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).


