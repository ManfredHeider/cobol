      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BPA0051
      *   Date Created  03/07/2018
      *   Time Created  14:25:57

      *  Input Data For Map BPA0051
         01 BPA0051I.
            03 FILLER                         PIC X(12).
            03 ASTAPEGL                       PIC S9(4) COMP.
            03 ASTAPEGF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASTAPEGI                       PIC X(1).
            03 ASTAPVWL                       PIC S9(4) COMP.
            03 ASTAPVWF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASTAPVWI                       PIC X(1).
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
            03 EVGESL                         PIC S9(4) COMP.
            03 EVGESF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVGESI                         PIC X(1).
            03 AVGESBML                       PIC S9(4) COMP.
            03 AVGESBMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVGESBMI                       PIC X(4).
            03 EFCSL                          PIC S9(4) COMP.
            03 EFCSF                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFCSI                          PIC X(3).
            03 AFCSTXTL                       PIC S9(4) COMP.
            03 AFCSTXTF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCSTXTI                       PIC X(17).
            03 AMAABRDL                       PIC S9(4) COMP.
            03 AMAABRDF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AMAABRDI                       PIC X(10).
            03 AABRDATL                       PIC S9(4) COMP.
            03 AABRDATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AABRDATI                       PIC X(10).
            03 AFCFBL                         PIC S9(4) COMP.
            03 AFCFBF                         PIC X.
            03 FILLER                         PIC X(1).
            03 AFCFBI                         PIC X(2).
            03 DFHG001I                       OCCURS 10 TIMES.
               05 EFCZL                          PIC S9(4) COMP.
               05 EFCZF                          PIC X.
               05 FILLER                         PIC X(1).
               05 EFCZI                          PIC X(1).
               05 AZINFOL                        PIC S9(4) COMP.
               05 AZINFOF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AZINFOI                        PIC X(1).
               05 AGESL                          PIC S9(4) COMP.
               05 AGESF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AGESI                          PIC X(2).
               05 AGESBEZL                       PIC S9(4) COMP.
               05 AGESBEZF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AGESBEZI                       PIC X(10).
               05 AZSTL                          PIC S9(4) COMP.
               05 AZSTF                          PIC X.
               05 FILLER                         PIC X(1).
               05 AZSTI                          PIC X(10).
               05 APGANZL                        PIC S9(4) COMP.
               05 APGANZF                        PIC X.
               05 FILLER                         PIC X(1).
               05 APGANZI                        PIC X(5).
               05 AMANANZL                       PIC S9(4) COMP.
               05 AMANANZF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AMANANZI                       PIC X(5).
               05 APRBETRL                       PIC S9(4) COMP.
               05 APRBETRF                       PIC X.
               05 FILLER                         PIC X(1).
               05 APRBETRI                       PIC X(15).
               05 AANZUBL                        PIC S9(4) COMP.
               05 AANZUBF                        PIC X.
               05 FILLER                         PIC X(1).
               05 AANZUBI                        PIC X(7).
               05 ESTAPEGL                       PIC S9(4) COMP.
               05 ESTAPEGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ESTAPEGI                       PIC X(1).
               05 ESTAPVWL                       PIC S9(4) COMP.
               05 ESTAPVWF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ESTAPVWI                       PIC X(1).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  Output Data For Map BPA0051
         01 BPA0051O REDEFINES BPA0051I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 ASTAPEGA                       PIC X.
            03 ASTAPEGH                       PIC X.
            03 ASTAPEGO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 ASTAPVWA                       PIC X.
            03 ASTAPVWH                       PIC X.
            03 ASTAPVWO                       PIC X(1).
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
            03 EVGESA                         PIC X.
            03 EVGESH                         PIC X.
            03 EVGESO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 AVGESBMA                       PIC X.
            03 AVGESBMH                       PIC X.
            03 AVGESBMO                       PIC X(4).
            03 FILLER                         PIC X(2).
            03 EFCSA                          PIC X.
            03 EFCSH                          PIC X.
            03 EFCSO                          PIC X(3).
            03 FILLER                         PIC X(2).
            03 AFCSTXTA                       PIC X.
            03 AFCSTXTH                       PIC X.
            03 AFCSTXTO                       PIC X(17).
            03 FILLER                         PIC X(2).
            03 AMAABRDA                       PIC X.
            03 AMAABRDH                       PIC X.
            03 AMAABRDO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AABRDATA                       PIC X.
            03 AABRDATH                       PIC X.
            03 AABRDATO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AFCFBA                         PIC X.
            03 AFCFBH                         PIC X.
            03 AFCFBO                         PIC X(2).
            03 DFHG001O OCCURS 10.
               05 FILLER                         PIC X(2).
               05 EFCZA                          PIC X.
               05 EFCZH                          PIC X.
               05 EFCZO                          PIC X(1).
               05 FILLER                         PIC X(2).
               05 AZINFOA                        PIC X.
               05 AZINFOH                        PIC X.
               05 AZINFOO                        PIC X(1).
               05 FILLER                         PIC X(2).
               05 AGESA                          PIC X.
               05 AGESH                          PIC X.
               05 AGESO                          PIC X(2).
               05 FILLER                         PIC X(2).
               05 AGESBEZA                       PIC X.
               05 AGESBEZH                       PIC X.
               05 AGESBEZO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 AZSTA                          PIC X.
               05 AZSTH                          PIC X.
               05 AZSTO                          PIC X(10).
               05 FILLER                         PIC X(2).
               05 APGANZA                        PIC X.
               05 APGANZH                        PIC X.
               05 APGANZO                        PIC X(5).
               05 FILLER                         PIC X(2).
               05 AMANANZA                       PIC X.
               05 AMANANZH                       PIC X.
               05 AMANANZO                       PIC X(5).
               05 FILLER                         PIC X(2).
               05 APRBETRA                       PIC X.
               05 APRBETRH                       PIC X.
               05 APRBETRO                       PIC X(15).
               05 FILLER                         PIC X(2).
               05 AANZUBA                        PIC X.
               05 AANZUBH                        PIC X.
               05 AANZUBO                        PIC X(7).
               05 FILLER                         PIC X(2).
               05 ESTAPEGA                       PIC X.
               05 ESTAPEGH                       PIC X.
               05 ESTAPEGO                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ESTAPVWA                       PIC X.
               05 ESTAPVWH                       PIC X.
               05 ESTAPVWO                       PIC X(1).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).


