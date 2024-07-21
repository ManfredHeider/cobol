      *   Mainframe Express 3.1  6.013.0041
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-57)
      *   MapSet Name   BPA0007
      *   Date Created  07/16/2015
      *   Time Created  15:57:22

      *  Input Data For Map BPA0007
         01 BPA0007I.
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
            03 ADARTBML                       PIC S9(4) COMP.
            03 ADARTBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADARTBMI                       PIC X(10).
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
            03 AFCSANZI                       PIC X(51).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 AVSBEZL                        PIC S9(4) COMP.
            03 AVSBEZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVSBEZI                        PIC X(3).
            03 AWAEGUTL                       PIC S9(4) COMP.
            03 AWAEGUTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEGUTI                       PIC X(4).
            03 AWAEBELL                       PIC S9(4) COMP.
            03 AWAEBELF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEBELI                       PIC X(4).
            03 AWAEPOZL                       PIC S9(4) COMP.
            03 AWAEPOZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEPOZI                       PIC X(4).
            03 AWAEDIFL                       PIC S9(4) COMP.
            03 AWAEDIFF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AWAEDIFI                       PIC X(4).
            03 DFHG001I                       OCCURS 9 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AVSTATL                        PIC S9(4) COMP.
               05 AVSTATF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AVSTATI                        PIC X(4).
               05 ASANZBWL                       PIC S9(4) COMP.
               05 ASANZBWF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASANZBWI                       PIC X(9).
               05 ASGUTL                         PIC S9(4) COMP.
               05 ASGUTF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ASGUTI                         PIC X(13).
               05 ASBELASL                       PIC S9(4) COMP.
               05 ASBELASF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASBELASI                       PIC X(13).
               05 ASPBOZL                        PIC S9(4) COMP.
               05 ASPBOZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 ASPBOZI                        PIC X(13).
               05 ADIFFL                         PIC S9(4) COMP.
               05 ADIFFF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ADIFFI                         PIC X(13).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  Output Data For Map BPA0007
         01 BPA0007O REDEFINES BPA0007I.
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
            03 ADARTBMA                       PIC X.
            03 ADARTBMH                       PIC X.
            03 ADARTBMO                       PIC X(10).
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
            03 AFCSANZO                       PIC X(51).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 FILLER                         PIC X(2).
            03 AVSBEZA                        PIC X.
            03 AVSBEZH                        PIC X.
            03 AVSBEZO                        PIC X(3).
            03 FILLER                         PIC X(2).
            03 AWAEGUTA                       PIC X.
            03 AWAEGUTH                       PIC X.
            03 AWAEGUTO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AWAEBELA                       PIC X.
            03 AWAEBELH                       PIC X.
            03 AWAEBELO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AWAEPOZA                       PIC X.
            03 AWAEPOZH                       PIC X.
            03 AWAEPOZO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 AWAEDIFA                       PIC X.
            03 AWAEDIFH                       PIC X.
            03 AWAEDIFO                       PIC X(4).
            03 DFHG001O OCCURS 9.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AVSTATA                        PIC X.
               05 AVSTATH                        PIC X.
               05 AVSTATO                        PIC X(4).
               05 FILLER                         PIC X(2).
               05 ASANZBWA                       PIC X.
               05 ASANZBWH                       PIC X.
               05 ASANZBWO                       PIC X(9).
               05 FILLER                         PIC X(2).
               05 ASGUTA                         PIC X.
               05 ASGUTH                         PIC X.
               05 ASGUTO                         PIC X(13).
               05 FILLER                         PIC X(2).
               05 ASBELASA                       PIC X.
               05 ASBELASH                       PIC X.
               05 ASBELASO                       PIC X(13).
               05 FILLER                         PIC X(2).
               05 ASPBOZA                        PIC X.
               05 ASPBOZH                        PIC X.
               05 ASPBOZO                        PIC X(13).
               05 FILLER                         PIC X(2).
               05 ADIFFA                         PIC X.
               05 ADIFFH                         PIC X.
               05 ADIFFO                         PIC X(13).
            03 FILLER                         PIC X(2).
            03 AFCZANZA                       PIC X.
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


