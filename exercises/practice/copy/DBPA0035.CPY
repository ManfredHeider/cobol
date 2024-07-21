      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0035
      *   Date Created  12/10/2013
      *   Time Created  07:26:04

      *  Input Data For Map BPA0035
         01 BPA0035I.
            03 FILLER                         PIC X(12).
            03 AGESL                          PIC S9(4) COMP.
            03 AGESF                          PIC X.
            03 FILLER                         PIC X(1).
            03 AGESI                          PIC X(2).
            03 AGESBML                        PIC S9(4) COMP.
            03 AGESBMF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AGESBMI                        PIC X(10).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(4).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(4).
            03 AABRVDAL                       PIC S9(4) COMP.
            03 AABRVDAF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRVDAI                       PIC X(10).
            03 AABRBDAL                       PIC S9(4) COMP.
            03 AABRBDAF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRBDAI                       PIC X(10).
            03 AFCSFBL                        PIC S9(4) COMP.
            03 AFCSFBF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSFBI                        PIC X(4).
            03 AFCSANZL                       PIC S9(4) COMP.
            03 AFCSANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSANZI                       PIC X(14).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 AFBPRO1L                       PIC S9(4) COMP.
            03 AFBPRO1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFBPRO1I                       PIC X(7).
            03 AFBPRO2L                       PIC S9(4) COMP.
            03 AFBPRO2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFBPRO2I                       PIC X(7).
            03 AWAEBELL                       PIC S9(4) COMP.
            03 AWAEBELF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEBELI                       PIC X(4).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 ANTZL                          PIC S9(4) COMP.
               05 ANTZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 ANTZI                          PIC X(1).
               05 AVNRL                          PIC S9(4) COMP.
               05 AVNRF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AVNRI                          PIC X(15).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 APRODS1L                       PIC S9(4) COMP.
               05 APRODS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS1I                       PIC X(3).
               05 APRODS2L                       PIC S9(4) COMP.
               05 APRODS2F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS2I                       PIC X(3).
               05 APRODS3L                       PIC S9(4) COMP.
               05 APRODS3F                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRODS3I                       PIC X(3).
               05 APLUSL                         PIC S9(4) COMP.
               05 APLUSF                         PIC X.
               05 FILLER                         PIC X(1).
               05 APLUSI                         PIC X(1).
               05 APROVBL                        PIC S9(4) COMP.
               05 APROVBF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APROVBI                        PIC X(15).
               05 AFAEVDAL                       PIC S9(4) COMP.
               05 AFAEVDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AFAEVDAI                       PIC X(10).
               05 AABREDAL                       PIC S9(4) COMP.
               05 AABREDAF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABREDAI                       PIC X(10).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0035
         01 BPA0035O REDEFINES BPA0035I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AGESA                          PIC X.
            03 AGESH                          PIC X.
            03 AGESO                          PIC X(2).
            03 FILLER                         PIC X(2).
            03 AGESBMA                        PIC X.
            03 AGESBMH                        PIC X.
            03 AGESBMO                        PIC X(10).
            03 FILLER                         PIC X(2).
            03 ASEIFBA                        PIC X.
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 ESEIAKTA                       PIC X.
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 ASEISTRA                       PIC X.
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASEIGESA                       PIC X.
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AABRVDAA                       PIC X.
            03 AABRVDAH                       PIC X.
            03 AABRVDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AABRBDAA                       PIC X.
            03 AABRBDAH                       PIC X.
            03 AABRBDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCSFBA                        PIC X.
            03 AFCSFBH                        PIC X.
            03 AFCSFBO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 AFCSANZA                       PIC X.
            03 AFCSANZH                       PIC X.
            03 AFCSANZO                       PIC X(14).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AFBPRO1A                       PIC X.
            03 AFBPRO1H                       PIC X.
            03 AFBPRO1O                       PIC X(7).
            03 FILLER                         PIC X(2).
            03 AFBPRO2A                       PIC X.
            03 AFBPRO2H                       PIC X.
            03 AFBPRO2O                       PIC X(7).
            03 FILLER                         PIC X(2).
            03 AWAEBELA                       PIC X.
            03 AWAEBELH                       PIC X.
            03 AWAEBELO                       PIC X(4).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 ANTZA                          PIC X.
               05 ANTZH                          PIC X.
               05 ANTZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVNRA                          PIC X.
               05 AVNRH                          PIC X.
               05 AVNRO                          PIC X(15).
               05 FILLER                         PIC X(2).
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 APRODS1A                       PIC X.
               05 APRODS1H                       PIC X.
               05 APRODS1O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APRODS2A                       PIC X.
               05 APRODS2H                       PIC X.
               05 APRODS2O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APRODS3A                       PIC X.
               05 APRODS3H                       PIC X.
               05 APRODS3O                       PIC X(3).
               05 FILLER                         PIC X(2).
               05 APLUSA                         PIC X.
               05 APLUSH                         PIC X.
               05 APLUSO                         PIC X(1).
               05 FILLER                         PIC X(2).
               05 APROVBA                        PIC X.
               05 APROVBH                        PIC X.
               05 APROVBO                        PIC X(15).
               05 FILLER                         PIC X(2).
               05 AFAEVDAA                       PIC X.
               05 AFAEVDAH                       PIC X.
               05 AFAEVDAO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AABREDAA                       PIC X.
               05 AABREDAH                       PIC X.
               05 AABREDAO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

